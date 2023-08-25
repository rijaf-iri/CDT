
fill_Miss_DekTemp <- function(){
    listOpenFiles <- openFile_ttkcomboList()
    if(WindowsOS()){
        largeur0 <- 56
        largeur1 <- 53
        largeur2 <- 20
    }else{
        largeur0 <- 54
        largeur1 <- 53
        largeur2 <- 20
    }

    xml.dlg <- file.path(.cdtDir$dirLocal, "languages", "cdtFillCDTdekTemp_dlgBox.xml")
    lang.dlg <- cdtLanguageParse(xml.dlg, .cdtData$Config$lang.iso)

    ############################################

    tt <- tktoplevel()
    tkgrab.set(tt)
    tkfocus(tt)

    frMRG0 <- tkframe(tt, relief = 'raised', borderwidth = 2)
    frMRG1 <- tkframe(tt)
    frLeft <- tkframe(frMRG0, relief = "groove", borderwidth = 2)

    ############################################

    frSTN <- tkframe(frLeft, relief = 'sunken', borderwidth = 2)

    file.stnfl <- tclVar(.cdtData$GalParams$STN.file)

    txt.stnfl <- tklabel(frSTN, text = lang.dlg[['label']][['1']], anchor = 'w', justify = 'left')
    cb.stnfl <- ttkcombobox(frSTN, values = unlist(listOpenFiles), textvariable = file.stnfl, width = largeur1)
    bt.stnfl <- tkbutton(frSTN, text = "...")

    ######
    tkconfigure(bt.stnfl, command = function(){
        tcl('wm', 'attributes', tt, topmost = FALSE)
        dat.opfiles <- getOpenFiles(tt)
        tcl('wm', 'attributes', tt, topmost = TRUE)
        if(!is.null(dat.opfiles)){
            update.OpenFiles('ascii', dat.opfiles)
            listOpenFiles[[length(listOpenFiles)+1]] <<- dat.opfiles[[1]]
            tclvalue(file.stnfl) <- dat.opfiles[[1]]
            tkconfigure(cb.stnfl, values = unlist(listOpenFiles))
        }
    })

    ######
    tkgrid(txt.stnfl, row = 0, column = 0, sticky = 'we', rowspan = 1, columnspan = 2, padx = 1, pady = 0, ipadx = 1, ipady = 1)
    tkgrid(cb.stnfl, row = 1, column = 0, sticky = 'we', rowspan = 1, columnspan = 1, padx = 0, pady = 0, ipadx = 1, ipady = 1)
    tkgrid(bt.stnfl, row = 1, column = 1, sticky = 'we', rowspan = 1, columnspan = 1, padx = 0, pady = 0, ipadx = 1, ipady = 1)

    helpWidget(bt.stnfl, lang.dlg[['tooltip']][['1']], lang.dlg[['status']][['1']])
    helpWidget(bt.stnfl, lang.dlg[['tooltip']][['2']], lang.dlg[['status']][['2']])

    ############################################

    frRFE <- tkframe(frLeft, relief = 'sunken', borderwidth = 2)

    dir.rfe <- tclVar(.cdtData$GalParams$NCDF$dir)

    txt.dir.rfe <- tklabel(frRFE, text = lang.dlg[['label']][['2']], anchor = 'w', justify = 'left')
    set.dir.rfe <- ttkbutton(frRFE, text = .cdtEnv$tcl$lang$global[['button']][['5']])
    en.dir.rfe <- tkentry(frRFE, textvariable = dir.rfe, width = largeur0)
    bt.dir.rfe <- tkbutton(frRFE, text = "...")

    ######
    tkconfigure(bt.dir.rfe, command = function(){
        tcl('wm', 'attributes', tt, topmost = FALSE)
        dir4rfe <- tk_choose.dir(.cdtData$GalParams$NCDF$dir, "")
        tcl('wm', 'attributes', tt, topmost = TRUE)
        tclvalue(dir.rfe) <- if(!is.na(dir4rfe)) dir4rfe else .cdtData$GalParams$NCDF$dir
    })

    tkconfigure(set.dir.rfe, command = function(){
        tcl('wm', 'attributes', tt, topmost = FALSE)
        .cdtData$GalParams[["NCDF"]] <- getInfoNetCDFData(tt, .cdtData$GalParams[["NCDF"]],
                                                          trimws(tclvalue(dir.rfe)))
        tcl('wm', 'attributes', tt, topmost = TRUE)
    })

    ######

    tkgrid(txt.dir.rfe, row = 0, column = 0, sticky = 'we', rowspan = 1, columnspan = 3, padx = 1, pady = 0, ipadx = 1, ipady = 1)
    tkgrid(set.dir.rfe, row = 0, column = 3, sticky = 'we', rowspan = 1, columnspan = 2, padx = 1, pady = 0, ipadx = 1, ipady = 1)
    tkgrid(en.dir.rfe, row = 1, column = 0, sticky = 'we', rowspan = 1, columnspan = 4, padx = 0, pady = 0, ipadx = 1, ipady = 1)
    tkgrid(bt.dir.rfe, row = 1, column = 4, sticky = 'we', rowspan = 1, columnspan = 1, padx = 0, pady = 0, ipadx = 1, ipady = 1)

    helpWidget(en.dir.rfe, lang.dlg[['tooltip']][['3']], lang.dlg[['status']][['3']])
    helpWidget(bt.dir.rfe, lang.dlg[['tooltip']][['4']], lang.dlg[['status']][['4']])
    helpWidget(set.dir.rfe, lang.dlg[['tooltip']][['5']], lang.dlg[['status']][['5']])

    ############################################

    frPars <- tkframe(frLeft, relief = 'sunken', borderwidth = 2)

    min.len <- tclVar(.cdtData$GalParams$Fill.Params$min.length)

    txt.min.len <- tklabel(frPars, text = lang.dlg[['label']][['3']], anchor = 'e', justify = 'right')
    en.min.len <- tkentry(frPars, textvariable = min.len, width = 4)
    bt.date.range <- ttkbutton(frPars, text = lang.dlg[['button']][['1']], width = largeur2)

    tkconfigure(bt.date.range, command = function(){
        .cdtData$GalParams[["Fill.Date.Range"]] <- getInfoDateRange(tt, 
                                                .cdtData$GalParams[["Fill.Date.Range"]],
                                                .cdtData$GalParams$tstep)
    })

    tkgrid(txt.min.len, row = 0, column = 0, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
    tkgrid(en.min.len, row = 0, column = 1, sticky = 'w', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
    tkgrid(bt.date.range, row = 0, column = 2, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)

    helpWidget(bt.date.range, lang.dlg[['tooltip']][['8']], lang.dlg[['status']][['8']])
    helpWidget(en.min.len, lang.dlg[['tooltip']][['9']], lang.dlg[['status']][['9']])

    ############################################

    frSave <- tkframe(frLeft, relief = 'sunken', borderwidth = 2)

    file.save1 <- tclVar(.cdtData$GalParams$out.file)

    txt.file.save <- tklabel(frSave, text = lang.dlg[['label']][['4']], anchor = 'w', justify = 'left')
    en.file.save <- tkentry(frSave, textvariable = file.save1, width = largeur0)
    bt.file.save <- tkbutton(frSave, text = "...")

    ######
    tkconfigure(bt.file.save, command = function(){
        tcl('wm', 'attributes', tt, topmost = FALSE)
        fileORdir2Save(file.save1, isFile = TRUE)
        tcl('wm', 'attributes', tt, topmost = TRUE)
    })

    ######
    tkgrid(txt.file.save, row = 0, column = 0, sticky = 'we', rowspan = 1, columnspan = 2, padx = 1, pady = 0, ipadx = 1, ipady = 1)
    tkgrid(en.file.save, row = 1, column = 0, sticky = 'we', rowspan = 1, columnspan = 1, padx = 0, pady = 0, ipadx = 1, ipady = 1)
    tkgrid(bt.file.save, row = 1, column = 1, sticky = 'w', rowspan = 1, columnspan = 1, padx = 0, pady = 0, ipadx = 1, ipady = 1)

    status.bar.display(en.file.save, lang.dlg[['status']][['6']])
    infobulle(en.file.save, lang.dlg[['tooltip']][['6']])
    status.bar.display(bt.file.save, lang.dlg[['status']][['7']])
    infobulle(bt.file.save, lang.dlg[['tooltip']][['7']])

    ############################################
    tkgrid(frSTN, row = 0, column = 0, sticky = 'we', padx = 1, pady = 1, ipadx = 1, ipady = 1)
    tkgrid(frRFE, row = 1, column = 0, sticky = 'we', padx = 1, pady = 1, ipadx = 1, ipady = 1)
    tkgrid(frPars, row = 2, column = 0, sticky = 'we', padx = 1, pady = 1, ipadx = 1, ipady = 1)
    tkgrid(frSave, row = 3, column = 0, sticky = 'we', padx = 1, pady = 1, ipadx = 1, ipady = 1)

    ############################################
    
    tkgrid(frLeft, row = 0, column = 0, sticky = 'news', padx = 5, pady = 1, ipadx = 1, ipady = 1)

    ############################################

    bt.prm.OK <- ttkbutton(frMRG1, text = .cdtEnv$tcl$lang$global[['button']][['1']])
    bt.prm.CA <- ttkbutton(frMRG1, text = .cdtEnv$tcl$lang$global[['button']][['2']])

    tkconfigure(bt.prm.OK, command = function(){
        if(trimws(tclvalue(file.stnfl)) == ""){
            cdt.tkmessageBox(tt, message = lang.dlg[['message']][['1']], icon = "warning", type = "ok")
        }else if(trimws(tclvalue(dir.rfe)) %in% c("", "NA")){
            cdt.tkmessageBox(tt, message = lang.dlg[['message']][['2']], icon = "warning", type = "ok")
            tkwait.window(tt)
        }else if(trimws(tclvalue(file.save1)) %in% c("", "NA")){
            cdt.tkmessageBox(tt, message = lang.dlg[['message']][['3']], icon = "warning", type = "ok")
            tkwait.window(tt)
        }else{
            .cdtData$GalParams$STN.file <- trimws(tclvalue(file.stnfl))
            .cdtData$GalParams$NCDF$dir <- trimws(tclvalue(dir.rfe))
            .cdtData$GalParams$out.file <- trimws(tclvalue(file.save1))

            .cdtData$GalParams$Fill.Params$min.length <- as.numeric(trimws(tclvalue(min.len)))
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

    ############################
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
