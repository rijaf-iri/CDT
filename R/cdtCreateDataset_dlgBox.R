
cdtDataset_getParams <- function(){
    if(WindowsOS()){
        largeur1 <- 26
        largeur2 <- 60
        largeur3 <- 50
    }else{
        largeur1 <- 26
        largeur2 <- 56
        largeur3 <- 49
    }

    xml.dlg <- file.path(.cdtDir$dirLocal, "languages", "cdtCreateDataset_dlgBox.xml")
    lang.dlg <- cdtLanguageParse(xml.dlg, .cdtData$Config$lang.iso)

    ############################################

    tt <- tktoplevel()
    tkgrab.set(tt)
    tkfocus(tt)

    frMRG0 <- tkframe(tt, relief = 'raised', borderwidth = 2)
    frMRG1 <- tkframe(tt)
    frLeft <- tkframe(frMRG0, relief = "groove", borderwidth = 2)

    ############################################

    frtimestep <- tkframe(frLeft, relief = 'sunken', borderwidth = 2)

    timeStep <- tclVar()
    CbperiodVAL <- .cdtEnv$tcl$lang$global[['combobox']][['1']][1:6]
    periodVAL <- c('minute', 'hourly', 'daily', 'pentad', 'dekadal', 'monthly')
    tclvalue(timeStep) <- CbperiodVAL[periodVAL %in% .cdtData$GalParams$tstep]

    retminhr <- set.hour.minute(.cdtData$GalParams$tstep, .cdtData$GalParams$minhour)
    minhour.tclVar <- tclVar(retminhr$val)

    cb.tstep <- ttkcombobox(frtimestep, values = CbperiodVAL, textvariable = timeStep, justify = 'center', width = largeur1)
    cb.minhour <- ttkcombobox(frtimestep, values = retminhr$cb, textvariable = minhour.tclVar, state = retminhr$state, width = 3)
    bt.period <- ttkbutton(frtimestep, text = lang.dlg[['button']][['1']], width = largeur1)

    tkgrid(cb.tstep, row = 0, column = 0, sticky = 'we', padx = 1, pady = 3, ipadx = 1, ipady = 1)
    tkgrid(cb.minhour, row = 0, column = 1, sticky = 'we', padx = 1, pady = 3, ipadx = 1, ipady = 1)
    tkgrid(bt.period, row = 0, column = 2, sticky = 'we', padx = 1, pady = 3, ipadx = 1, ipady = 1)

    helpWidget(cb.tstep, lang.dlg[['tooltip']][['1']], lang.dlg[['status']][['1']])
    helpWidget(bt.period, lang.dlg[['tooltip']][['5']], lang.dlg[['status']][['5']])

    tkconfigure(bt.period, command = function(){
        intstep <- periodVAL[CbperiodVAL %in% trimws(tclvalue(timeStep))]
        .cdtData$GalParams[["date.range"]] <- getInfoDateRange(tt, .cdtData$GalParams[["date.range"]], intstep)
    })

    tkbind(cb.tstep, "<<ComboboxSelected>>", function(){
        intstep <- periodVAL[CbperiodVAL %in% trimws(tclvalue(timeStep))]
        minhour <- as.numeric(trimws(tclvalue(minhour.tclVar)))
        retminhr <- set.hour.minute(intstep, minhour)
        tkconfigure(cb.minhour, values = retminhr$cb, state = retminhr$state)
        tclvalue(minhour.tclVar) <- retminhr$val
    })

    ############################################

    frameNCDF <- tkframe(frLeft, relief = 'sunken', borderwidth = 2)

    dir.NCDF <- tclVar(.cdtData$GalParams$NCDF$dir)

    txt.NCDF <- tklabel(frameNCDF, text = lang.dlg[['label']][['1']], anchor = 'w', justify = 'left')
    set.NCDF <- ttkbutton(frameNCDF, text = .cdtEnv$tcl$lang$global[['button']][['5']])
    en.NCDF <- tkentry(frameNCDF, textvariable = dir.NCDF, width = largeur2)
    bt.NCDF <- tkbutton(frameNCDF, text = "...")

    ######
    tkconfigure(set.NCDF, command = function(){
        tcl('wm', 'attributes', tt, topmost = FALSE)
        .cdtData$GalParams[["NCDF"]] <- getInfoNetCDFData(tt, .cdtData$GalParams[["NCDF"]],
                                                          trimws(tclvalue(dir.NCDF)))
        tcl('wm', 'attributes', tt, topmost = TRUE)
    })

    tkconfigure(bt.NCDF, command = function(){
        tcl('wm', 'attributes', tt, topmost = FALSE)
        dirnc <- tk_choose.dir(getwd(), "")
        tcl('wm', 'attributes', tt, topmost = TRUE)
        tclvalue(dir.NCDF) <- if(!is.na(dirnc)) dirnc else ""
    })

    ######
    tkgrid(txt.NCDF, row = 0, column = 0, sticky = 'we', rowspan = 1, columnspan = 3, padx = 1, pady = 0, ipadx = 1, ipady = 1)
    tkgrid(set.NCDF, row = 0, column = 3, sticky = 'we', rowspan = 1, columnspan = 2, padx = 1, pady = 0, ipadx = 1, ipady = 1)
    tkgrid(en.NCDF, row = 1, column = 0, sticky = 'we', rowspan = 1, columnspan = 4, padx = 0, pady = 0, ipadx = 1, ipady = 1)
    tkgrid(bt.NCDF, row = 1, column = 4, sticky = 'we', rowspan = 1, columnspan = 1, padx = 0, pady = 0, ipadx = 1, ipady = 1)

    helpWidget(en.NCDF, lang.dlg[['tooltip']][['2']], lang.dlg[['status']][['2']])
    helpWidget(bt.NCDF, lang.dlg[['tooltip']][['3']], lang.dlg[['status']][['3']])
    helpWidget(set.NCDF, lang.dlg[['tooltip']][['4']], lang.dlg[['status']][['4']])

    ############################################

    frUpdate <- tkframe(frLeft, relief = 'sunken', borderwidth = 2)

    update.data <- tclVar(.cdtData$GalParams$Update)
    file.dataRDS <- tclVar(.cdtData$GalParams$cdtDataSet)

    stateUp <- if(tclvalue(update.data) == "1") "normal" else "disabled"

    chk.update <- tkcheckbutton(frUpdate, variable = update.data, text = lang.dlg[['label']][['2']], anchor = 'w', justify = 'left')
    en.update <- tkentry(frUpdate, textvariable = file.dataRDS, width = largeur2, state = stateUp)
    bt.update <- tkbutton(frUpdate, text = "...", state = stateUp)

    tkconfigure(bt.update, command = function(){
        tcl('wm', 'attributes', tt, topmost = FALSE)
        path.update <- tclvalue(tkgetOpenFile(initialdir = getwd(), filetypes = .cdtEnv$tcl$data$filetypes6))
        tcl('wm', 'attributes', tt, topmost = TRUE)
        if(path.update == "") return(NULL)
        tclvalue(file.dataRDS) <- path.update

        if(file.exists(tclvalue(file.dataRDS))){
            tclvalue(dir2save) <- dirname(dirname(tclvalue(file.dataRDS)))
            tclvalue(nom.data) <- basename(dirname(tclvalue(file.dataRDS)))
        }
    })

    tkgrid(chk.update, row = 0, column = 0, sticky = 'we', rowspan = 1, columnspan = 5, padx = 1, pady = 1, ipadx = 1, ipady = 1)
    tkgrid(en.update, row = 1, column = 0, sticky = 'we', rowspan = 1, columnspan = 4, padx = 1, pady = 1, ipadx = 1, ipady = 1)
    tkgrid(bt.update, row = 1, column = 4, sticky = 'we', rowspan = 1, columnspan = 1, padx = 0, pady = 1, ipadx = 1, ipady = 1)

    helpWidget(chk.update, lang.dlg[['tooltip']][['6']], lang.dlg[['status']][['6']])
    helpWidget(en.update, lang.dlg[['tooltip']][['7']], lang.dlg[['status']][['7']])
    helpWidget(bt.update, lang.dlg[['tooltip']][['3']], lang.dlg[['status']][['3']])

    ###############

    tkbind(chk.update, "<Button-1>", function(){
        stateUp <- if(tclvalue(update.data) == '1') 'disabled' else 'normal'
        tkconfigure(en.update, state = stateUp)
        tkconfigure(bt.update, state = stateUp)
        stateOUT <- if(tclvalue(update.data) == '1') 'normal' else 'disabled'
        tkconfigure(en.dir2save, state = stateOUT)
        tkconfigure(bt.dir2save, state = stateOUT)
        tkconfigure(en.nomdata, state = stateOUT)
    })

    ############################################

    frOutput <- tkframe(frLeft, relief = 'sunken', borderwidth = 2)

    dir2save <- tclVar(.cdtData$GalParams$output$dir)
    nom.data <- tclVar(.cdtData$GalParams$output$data.name)

    stateOUT <- if(tclvalue(update.data) == "0") "normal" else "disabled"

    txt.dir2save <- tklabel(frOutput, text = lang.dlg[['label']][['3']], anchor = 'w', justify = 'left')
    en.dir2save <- tkentry(frOutput, textvariable = dir2save, width = largeur2, state = stateOUT)
    bt.dir2save <- tkbutton(frOutput, text = "...", state = stateOUT)
    txt.nomdata <- tklabel(frOutput, text = lang.dlg[['label']][['4']], anchor = 'e', justify = 'right')
    en.nomdata <- tkentry(frOutput, textvariable = nom.data, width = largeur3, state = stateOUT)

    #####

    tkconfigure(bt.dir2save, command = function(){
        tcl('wm', 'attributes', tt, topmost = FALSE)
        dir2savepth <- tk_choose.dir(.cdtData$GalParams$output$dir, "")
        tcl('wm', 'attributes', tt, topmost = TRUE)
        if(is.na(dir2savepth)) tclvalue(dir2save) <- .cdtData$GalParams$output$dir
        else{
            dir.create(dir2savepth, showWarnings = FALSE, recursive = TRUE)
            tclvalue(dir2save) <- dir2savepth
        }
    })

    #####

    tkgrid(txt.dir2save, row = 0, column = 0, sticky = 'we', rowspan = 1, columnspan = 7, padx = 1, pady = 1, ipadx = 1, ipady = 1)
    tkgrid(en.dir2save, row = 1, column = 0, sticky = 'we', rowspan = 1, columnspan = 6, padx = 0, pady = 1, ipadx = 1, ipady = 1)
    tkgrid(bt.dir2save, row = 1, column = 6, sticky = 'we', rowspan = 1, columnspan = 1, padx = 0, pady = 1, ipadx = 1, ipady = 1)
    tkgrid(txt.nomdata, row = 2, column = 0, sticky = 'e', rowspan = 1, columnspan = 3, padx = 1, pady = 1, ipadx = 1, ipady = 1)
    tkgrid(en.nomdata, row = 2, column = 3, sticky = 'we', rowspan = 1, columnspan = 4, padx = 1, pady = 1, ipadx = 1, ipady = 1)

    helpWidget(en.dir2save, lang.dlg[['tooltip']][['8']], lang.dlg[['status']][['8']])
    helpWidget(bt.dir2save, lang.dlg[['tooltip']][['3']], lang.dlg[['status']][['3']])
    helpWidget(en.nomdata, lang.dlg[['tooltip']][['9']], lang.dlg[['status']][['9']])

    ############################################
    tkgrid(frtimestep, row = 0, column = 0, sticky = '', padx = 1, pady = 1, ipadx = 1, ipady = 1)
    tkgrid(frameNCDF, row = 1, column = 0, sticky = 'we', padx = 1, pady = 3, ipadx = 1, ipady = 1)
    tkgrid(frUpdate, row = 2, column = 0, sticky = 'we', padx = 1, pady = 3, ipadx = 1, ipady = 1)
    tkgrid(frOutput, row = 3, column = 0, sticky = 'we', padx = 1, pady = 3, ipadx = 1, ipady = 1)

    ############################################

    tkgrid(frLeft, row = 0, column = 0, sticky = 'news', padx = 5, pady = 1, ipadx = 1, ipady = 1)

    ############################################

    bt.prm.OK <- ttkbutton(frMRG1, text = .cdtEnv$tcl$lang$global[['button']][['1']])
    bt.prm.CA <- ttkbutton(frMRG1, text = .cdtEnv$tcl$lang$global[['button']][['2']])

    tkconfigure(bt.prm.OK, command = function(){
        if(trimws(tclvalue(dir.NCDF)) %in% c("", "NA")){
            cdt.tkmessageBox(tt, message = lang.dlg[['message']][['1']], icon = "warning", type = "ok")
            tkwait.window(tt)
        }else if((tclvalue(update.data) == "1") & trimws(tclvalue(file.dataRDS)) %in% c("", "NA")){
            cdt.tkmessageBox(tt, message = lang.dlg[['message']][['2']], icon = "warning", type = "ok")
            tkwait.window(tt)
        }else if((tclvalue(update.data) == "0") & trimws(tclvalue(dir2save)) %in% c("", "NA")){
            cdt.tkmessageBox(tt, message = lang.dlg[['message']][['3']], icon = "warning", type = "ok")
            tkwait.window(tt)
        }else{
            .cdtData$GalParams$tstep <- periodVAL[CbperiodVAL %in% trimws(tclvalue(timeStep))]
            .cdtData$GalParams$minhour <- as.numeric(trimws(tclvalue(minhour.tclVar)))
            .cdtData$GalParams$NCDF$dir <- trimws(tclvalue(dir.NCDF))

            .cdtData$GalParams$Update <- switch(tclvalue(update.data), '0' = FALSE, '1' = TRUE)
            .cdtData$GalParams$cdtDataSet <- trimws(tclvalue(file.dataRDS))
            .cdtData$GalParams$output$dir <- trimws(tclvalue(dir2save))
            .cdtData$GalParams$output$data.name <- trimws(tclvalue(nom.data))

            .cdtData$GalParams$message <- lang.dlg[['message']]

            tkgrab.release(tt)
            tkdestroy(tt)
            tkfocus(.cdtEnv$tcl$main$win)
        }
    })

    tkconfigure(bt.prm.CA, command = function(){
        tkgrab.release(tt)
        tkdestroy(tt)
        tkfocus(.cdtEnv$tcl$main$win)
    })

    tkgrid(bt.prm.OK, row = 0, column = 0, sticky = 'w', padx = 5, pady = 1, ipadx = 1, ipady = 1)
    tkgrid(bt.prm.CA, row = 0, column = 1, sticky = 'e', padx = 5, pady = 1, ipadx = 1, ipady = 1)

    ############################################

    tkgrid(frMRG0, row = 0, column = 0, sticky = 'nswe', rowspan = 1, columnspan = 2, padx = 1, pady = 1, ipadx = 1, ipady = 1)
    tkgrid(frMRG1, row = 1, column = 1, sticky = 'se', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)

    ############################3
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
    tkbind(tt, "<Destroy>", function(){
        tkgrab.release(tt)
        tkfocus(.cdtEnv$tcl$main$win)
    })
    tkwait.window(tt)
}
