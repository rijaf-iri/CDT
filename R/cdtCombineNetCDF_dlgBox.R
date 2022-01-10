
combine.netcdf_getParams <- function(){
    if(WindowsOS()){
        largeur0 <- 23
        largeur1 <- 46
    }else{
        largeur0 <- 23
        largeur1 <- 45
    }

    xml.dlg <- file.path(.cdtDir$dirLocal, "languages", "cdtCombineNetCDF_dlgBox.xml")
    lang.dlg <- cdtLanguageParse(xml.dlg, .cdtData$Config$lang.iso)

    ###################

    tt <- tktoplevel()
    tkgrab.set(tt)
    tkfocus(tt)

    frMRG0 <- tkframe(tt, relief = 'raised', borderwidth = 2)
    frMRG1 <- tkframe(tt)

    ############################################

    frTstep <- tkframe(frMRG0, relief = "sunken", borderwidth = 2)

    timeStep <- tclVar()
    CbperiodVAL <- .cdtEnv$tcl$lang$global[['combobox']][['1']][1:6]
    periodVAL <- c('minute', 'hourly', 'daily', 'pentad', 'dekadal', 'monthly')
    tclvalue(timeStep) <- CbperiodVAL[periodVAL %in% .cdtData$GalParams$tstep]

    retminhr <- set.hour.minute(.cdtData$GalParams$tstep, .cdtData$GalParams$minhour)
    minhour.tclVar <- tclVar(retminhr$val)

    txt.tstep <- tklabel(frTstep, text = lang.dlg[['label']][['1']], anchor = 'w', justify = 'left')
    cb.tstep <- ttkcombobox(frTstep, values = CbperiodVAL, textvariable = timeStep, justify = 'center', width = largeur0)
    cb.minhour <- ttkcombobox(frTstep, values = retminhr$cb, textvariable = minhour.tclVar, state = retminhr$state, width = 2)

    tkgrid(txt.tstep, row = 0, column = 0, sticky = 'we', rowspan = 1, columnspan = 2, padx = 1, pady = 1, ipadx = 1, ipady = 1)
    tkgrid(cb.tstep, row = 0, column = 2, sticky = 'we', rowspan = 1, columnspan = 6, padx = 1, pady = 1, ipadx = 1, ipady = 1)
    tkgrid(cb.minhour, row = 0, column = 8, sticky = 'we', rowspan = 1, columnspan = 2, padx = 0, pady = 1, ipadx = 1, ipady = 1)

    ############

    tkbind(cb.tstep, "<<ComboboxSelected>>", function(){
        intstep <- periodVAL[CbperiodVAL %in% str_trim(tclvalue(timeStep))]
        minhour <- as.numeric(str_trim(tclvalue(minhour.tclVar)))
        retminhr <- set.hour.minute(intstep, minhour)
        tkconfigure(cb.minhour, values = retminhr$cb, state = retminhr$state)
        tclvalue(minhour.tclVar) <- retminhr$val
    })

    ############################################

    frNCDF <- tkframe(frMRG0, relief = "sunken", borderwidth = 2)

    dir.InNCDF <- tclVar(.cdtData$GalParams$ncdf$dir)

    txt.InNCDF <- tklabel(frNCDF, text = lang.dlg[['label']][['2']], anchor = 'w', justify = 'left')
    set.InNCDF <- ttkbutton(frNCDF, text = .cdtEnv$tcl$lang$global[['button']][['5']])
    en.InNCDF <- tkentry(frNCDF, textvariable = dir.InNCDF, width = largeur1)
    bt.InNCDF <- tkbutton(frNCDF, text = "...")

    tkgrid(txt.InNCDF, row = 0, column = 0, sticky = 'we', rowspan = 1, columnspan = 3, padx = 1, pady = 0, ipadx = 1, ipady = 1)
    tkgrid(set.InNCDF, row = 0, column = 3, sticky = 'we', rowspan = 1, columnspan = 2, padx = 1, pady = 0, ipadx = 1, ipady = 1)
    tkgrid(en.InNCDF, row = 1, column = 0, sticky = 'we', rowspan = 1, columnspan = 4, padx = 0, pady = 0, ipadx = 1, ipady = 1)
    tkgrid(bt.InNCDF, row = 1, column = 4, sticky = 'we', rowspan = 1, columnspan = 1, padx = 0, pady = 0, ipadx = 1, ipady = 1)

    ######

    settingSNC <- .cdtData$GalParams$settingSNC
    tkconfigure(set.InNCDF, command = function(){
        tcl('wm', 'attributes', tt, topmost = FALSE)
        .cdtData$GalParams[["ncdf"]] <- getInfoNetCDFData(tt, .cdtData$GalParams[["ncdf"]],
                                                          str_trim(tclvalue(dir.InNCDF)))
        tcl('wm', 'attributes', tt, topmost = TRUE)
        settingSNC <<- 1
    })

    tkconfigure(bt.InNCDF, command = function(){
        tcl('wm', 'attributes', tt, topmost = FALSE)
        dirnc <- tk_choose.dir(getwd(), "")
        tcl('wm', 'attributes', tt, topmost = TRUE)
        tclvalue(dir.InNCDF) <- if(!is.na(dirnc)) dirnc else ""
    })

    ############################################

    frSave <- tkframe(frMRG0, relief = "sunken", borderwidth = 2)

    file.save <- tclVar(.cdtData$GalParams$file2save)

    txt.file.save <- tklabel(frSave, text = lang.dlg[['label']][['3']], anchor = 'w', justify = 'left')
    en.file.save <- tkentry(frSave, textvariable = file.save, width = largeur1)
    bt.file.save <- tkbutton(frSave, text = "...")

    tkgrid(txt.file.save, row = 0, column = 0, sticky = 'we', rowspan = 1, columnspan = 5, padx = 1, pady = 0, ipadx = 1, ipady = 1)
    tkgrid(en.file.save, row = 1, column = 0, sticky = 'we', rowspan = 1, columnspan = 4, padx = 0, pady = 0, ipadx = 1, ipady = 1)
    tkgrid(bt.file.save, row = 1, column = 4, sticky = 'we', rowspan = 1, columnspan = 1, padx = 0, pady = 0, ipadx = 1, ipady = 1)

    #########

    tkconfigure(bt.file.save, command = function(){
        tcl('wm', 'attributes', tt, topmost = FALSE)
        # fileORdir2Save(file.save, filetypes = .cdtEnv$tcl$data$filetypes3)
        file2save <- tk_get_SaveFile(filetypes = .cdtEnv$tcl$data$filetypes3)
        tclvalue(file.save) <- if(is.na(file2save)) "" else file2save
        tcl('wm', 'attributes', tt, topmost = TRUE)
    })

    ############################################

    tkgrid(frTstep, row = 0, column = 0, sticky = '', padx = 1, pady = 5, ipadx = 1, ipady = 5)
    tkgrid(frNCDF, row = 1, column = 0, sticky = 'we', padx = 1, pady = 3, ipadx = 1, ipady = 1)
    tkgrid(frSave, row = 2, column = 0, sticky = 'we', padx = 1, pady = 3, ipadx = 1, ipady = 1)

    ############################################

    bt.prm.OK <- ttkbutton(frMRG1, text = .cdtEnv$tcl$lang$global[['button']][['1']])
    bt.prm.CA <- ttkbutton(frMRG1, text = .cdtEnv$tcl$lang$global[['button']][['2']])

    ####
    tkconfigure(bt.prm.OK, command = function(){
        .cdtData$GalParams$tstep <- periodVAL[CbperiodVAL %in% str_trim(tclvalue(timeStep))]
        .cdtData$GalParams$minhour <- as.numeric(str_trim(tclvalue(minhour.tclVar)))

        .cdtData$GalParams$ncdf$dir <- str_trim(tclvalue(dir.InNCDF))
        .cdtData$GalParams$file2save <- str_trim(tclvalue(file.save))

        .cdtData$GalParams$message <- lang.dlg[['message']]

        tkgrab.release(tt)
        tkdestroy(tt)
        tkfocus(.cdtEnv$tcl$main$win)
    })

    tkconfigure(bt.prm.CA, command = function(){
        tkgrab.release(tt)
        tkdestroy(tt)
        tkfocus(.cdtEnv$tcl$main$win)
    })

    ####
    tkgrid(bt.prm.OK, row = 0, column = 0, sticky = 'w', padx = 5, pady = 1, ipadx = 1, ipady = 1)
    tkgrid(bt.prm.CA, row = 0, column = 1, sticky = 'e', padx = 5, pady = 1, ipadx = 1, ipady = 1)

    ############################################
    
    tkgrid(frMRG0, row = 0, column = 0, sticky = 'nswe', rowspan = 1, columnspan = 2, padx = 1, pady = 1, ipadx = 1, ipady = 1)
    tkgrid(frMRG1, row = 1, column = 1, sticky = 'se', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)

    ###########################
    tkwm.withdraw(tt)
    tcl('update')
    tt.w <- as.integer(tkwinfo("reqwidth", tt))
    tt.h <- as.integer(tkwinfo("reqheight", tt))
    tt.x <- as.integer(.cdtEnv$tcl$data$width.scr*0.5 - tt.w*0.5)
    tt.y <- as.integer(.cdtEnv$tcl$data$height.scr*0.5 - tt.h*0.5)
    tkwm.geometry(tt, paste0('+', tt.x, '+', tt.y))
    tkwm.transient(tt)
    tkwm.title(tt, lang.dlg[['title']])
    tkwm.deiconify(tt)
    tcl('wm', 'attributes', tt, topmost = TRUE)

    tkfocus(tt)
    tkbind(tt, "<Destroy>", function() {
        tkgrab.release(tt)
        tkfocus(.cdtEnv$tcl$main$win)
    })
    tkwait.window(tt)
}
