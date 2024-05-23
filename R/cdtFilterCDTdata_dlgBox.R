filterCDTData_getParams <- function(){
    listOpenFiles <- openFile_ttkcomboList()
    if(WindowsOS()){
        largeur1 <- 48
        largeur2 <- 45
        largeur3 <- 20
    }else{
        largeur1 <- 42
        largeur2 <- 40
        largeur3 <- 21
    }

    ############################################

    tt <- tktoplevel()
    tkgrab.set(tt)
    tkfocus(tt)

    frMRG0 <- tkframe(tt, relief = 'raised', borderwidth = 2)
    frMRG1 <- tkframe(tt)

    ############################################

    xml.dlg <- file.path(.cdtDir$dirLocal, "languages", "cdtFilterCDTdata_dlgBox.xml")
    lang.dlg <- cdtLanguageParse(xml.dlg, .cdtData$Config$lang.iso)

    #############

    frInput <- tkframe(frMRG0, relief = "groove", borderwidth = 2)

    file.stnfl1 <- tclVar(.cdtData$GalParams$filein)

    txtStnfl1 <- tklabel(frInput, text = lang.dlg[['label']][['1']], anchor = 'w', justify = 'left')
    cbStnfl1 <- ttkcombobox(frInput, values = unlist(listOpenFiles), textvariable = file.stnfl1, width = largeur2)
    btStnfl1 <- tkbutton(frInput, text = "...")

    tkgrid(txtStnfl1, row = 0, column = 0, sticky = 'we', rowspan = 1, columnspan = 10, padx = 1, pady = 0, ipadx = 1, ipady = 1)
    tkgrid(cbStnfl1, row = 1, column = 0, sticky = 'we', rowspan = 1, columnspan = 9, padx = 0, pady = 0, ipadx = 1, ipady = 1)
    tkgrid(btStnfl1, row = 1, column = 9, sticky = 'we', rowspan = 1, columnspan = 1, padx = 0, pady = 0, ipadx = 1, ipady = 1)

    #############

    tkconfigure(btStnfl1, command = function(){
        tcl('wm', 'attributes', tt, topmost = FALSE)
        dat.opfiles <- getOpenFiles(tt)
        tcl('wm', 'attributes', tt, topmost = TRUE)
        if(!is.null(dat.opfiles)){
            update.OpenFiles('ascii', dat.opfiles)
            listOpenFiles[[length(listOpenFiles) + 1]] <<- dat.opfiles[[1]]
            tclvalue(file.stnfl1) <- dat.opfiles[[1]]
            tkconfigure(cbStnfl1, values = unlist(listOpenFiles), textvariable = file.stnfl1)
        }
    })

    ############################################

    frPeriod <- tkframe(frMRG0, relief = "groove", borderwidth = 2)

    timeSteps <- tclVar()
    CbperiodVAL <- .cdtEnv$tcl$lang$global[['combobox']][['1']][1:6]
    periodVAL <- c('minute', 'hourly', 'daily', 'pentad', 'dekadal', 'monthly')
    tclvalue(timeSteps) <- CbperiodVAL[periodVAL %in% .cdtData$GalParams$tstep]

    all.period <- tclVar(.cdtData$GalParams$all.period)

    retminhr <- set.hour.minute(.cdtData$GalParams$tstep, .cdtData$GalParams$minhour)
    minhour.tclVar <- tclVar(retminhr$val)

    statePeriod0 <- if(tclvalue(all.period) == "1") "disabled" else "normal"
    statePeriod1 <- if(tclvalue(all.period) == "1") "disabled" else retminhr$state

    chk.Period <- tkcheckbutton(frPeriod, variable = all.period, text = lang.dlg[['label']][['4']], anchor = 'w', justify = 'left')
    txt.Tstep <- tklabel(frPeriod, text = lang.dlg[['label']][['5']], anchor = 'e', justify = 'right')
    cb.Tstep <- ttkcombobox(frPeriod, values = CbperiodVAL, textvariable = timeSteps, state = statePeriod0, width = largeur3)
    cb.minhour <- ttkcombobox(frPeriod, values = retminhr$cb, textvariable = minhour.tclVar, state = statePeriod1, width = 2)
    bt.Period <- ttkbutton(frPeriod, text = lang.dlg[['button']][['1']], state = statePeriod0)
    txt.Period1 <- tklabel(frPeriod, text = "", anchor = 'e', justify = 'right', width = 10)

    tkgrid(chk.Period, row = 0, column = 0, sticky = 'we', rowspan = 1, columnspan = 10, padx = 1, pady = 1, ipadx = 1, ipady = 1)
    tkgrid(txt.Period1, row = 1, column = 0, sticky = 'e', rowspan = 1, columnspan = 2, padx = 1, pady = 1, ipadx = 1, ipady = 1)
    tkgrid(txt.Tstep, row = 1, column = 2, sticky = 'e', rowspan = 1, columnspan = 3, padx = 1, pady = 1, ipadx = 1, ipady = 1)
    tkgrid(cb.Tstep, row = 1, column = 5, sticky = 'we', rowspan = 1, columnspan = 4, padx = 1, pady = 1, ipadx = 1, ipady = 1)
    tkgrid(cb.minhour, row = 1, column = 9, sticky = 'w', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
    tkgrid(bt.Period, row = 2, column = 0, sticky = 'we', rowspan = 1, columnspan = 10, padx = 1, pady = 2, ipadx = 1, ipady = 1)

    #############

    tkconfigure(bt.Period, command = function(){
        intstep <- periodVAL[CbperiodVAL %in% trimws(tclvalue(timeSteps))]
        .cdtData$GalParams[["date.range"]] <- getInfoDateRange(tt, .cdtData$GalParams[["date.range"]], intstep)
    })

    tkbind(chk.Period, "<Button-1>", function(){
        statePeriod0 <- if(tclvalue(all.period) == '1') 'normal' else 'disabled'
        tkconfigure(cb.Tstep, state = statePeriod0)
        tkconfigure(bt.Period, state = statePeriod0)
        statePeriod1 <- if(tclvalue(all.period) == "1") retminhr$state else "disabled"
        tkconfigure(cb.minhour, state = statePeriod1)
    })

    tkbind(cb.Tstep, "<<ComboboxSelected>>", function(){
        intstep <- periodVAL[CbperiodVAL %in% trimws(tclvalue(timeSteps))]

        minhour <- as.numeric(trimws(tclvalue(minhour.tclVar)))
        retminhr <<- set.hour.minute(intstep, minhour)
        tkconfigure(cb.minhour, values = retminhr$cb, state = retminhr$state)
        tclvalue(minhour.tclVar) <- retminhr$val
    })

    ############################################

    frPercent <- tkframe(frMRG0, relief = "groove", borderwidth = 2)

    filter.crt <- tclVar(.cdtData$GalParams$opfilter)
    filter.val <- tclVar(.cdtData$GalParams$valfilter)

    txtFilter1 <- tklabel(frPercent, text = lang.dlg[['label']][['2']], anchor = 'e', justify = 'right')
    cbFilter <- ttkcombobox(frPercent, values = c(">=", ">", "<=", "<"), textvariable = filter.crt, width = 4)
    enFilter <- tkentry(frPercent, textvariable = filter.val, width = 4)
    txtFilter2 <- tklabel(frPercent, text = '%', anchor = 'w', justify = 'left')

    tkgrid(txtFilter1, row = 0, column = 0, sticky = 'e', rowspan = 1, columnspan = 1, padx = 1, pady = 0, ipadx = 1, ipady = 1)
    tkgrid(cbFilter, row = 0, column = 1, sticky = 'w', rowspan = 1, columnspan = 1, padx = 0, pady = 0, ipadx = 1, ipady = 1)
    tkgrid(enFilter, row = 0, column = 2, sticky = 'e', rowspan = 1, columnspan = 1, padx = 0, pady = 0, ipadx = 1, ipady = 1)
    tkgrid(txtFilter2, row = 0, column = 3, sticky = 'w', rowspan = 1, columnspan = 1, padx = 0, pady = 0, ipadx = 1, ipady = 1)

    ############################################

    frOutput <- tkframe(frMRG0, relief = "groove", borderwidth = 2)

    file.save1 <- tclVar(.cdtData$GalParams$file2save)

    txtFileSave <- tklabel(frOutput, text = lang.dlg[['label']][['3']], anchor = 'w', justify = 'left')
    enFileSave <- tkentry(frOutput, textvariable = file.save1, width = largeur1)
    btFileSave <- tkbutton(frOutput, text = "...")

    tkgrid(txtFileSave, row = 0, column = 0, sticky = 'we', rowspan = 1, columnspan = 10, padx = 1, pady = 0, ipadx = 1, ipady = 1)
    tkgrid(enFileSave, row = 1, column = 0, sticky = 'we', rowspan = 1, columnspan = 9, padx = 0, pady = 0, ipadx = 1, ipady = 1)
    tkgrid(btFileSave, row = 1, column = 9, sticky = 'we', rowspan = 1, columnspan = 1, padx = 0, pady = 0, ipadx = 1, ipady = 1)

    #############

    tkconfigure(btFileSave, command = function(){
        tcl('wm', 'attributes', tt, topmost = FALSE)
        file2save1 <- tk_get_SaveFile(filetypes = .cdtEnv$tcl$data$filetypesA)
        tclvalue(file.save1) <- if(is.na(file2save1)) "" else file2save1
        tcl('wm', 'attributes', tt, topmost = TRUE)
    })

    ############################################

    tkgrid(frInput, row = 0, column = 0, sticky = 'we', padx = 1, pady = 1, ipadx = 1, ipady = 1)
    tkgrid(frPeriod, row = 1, column = 0, sticky = 'we', padx = 1, pady = 1, ipadx = 1, ipady = 1)
    tkgrid(frPercent, row = 2, column = 0, sticky = 'e', padx = 1, pady = 1, ipadx = 1, ipady = 1)
    tkgrid(frOutput, row = 3, column = 0, sticky = 'we', padx = 1, pady = 1, ipadx = 1, ipady = 1)

    ############################################

    bt.prm.OK <- ttkbutton(frMRG1, text = .cdtEnv$tcl$lang$global[['button']][['1']])
    bt.prm.CA <- ttkbutton(frMRG1, text = .cdtEnv$tcl$lang$global[['button']][['2']])

    tkconfigure(bt.prm.OK, command = function(){
        if(trimws(tclvalue(file.stnfl1)) %in% c("", "NA")){
            cdt.tkmessageBox(tt, message = lang.dlg[['message']][['6']], icon = "warning", type = "ok")
            tkwait.window(tt)
        }else if(trimws(tclvalue(file.save1)) %in% c("", "NA")){
            cdt.tkmessageBox(tt, message = lang.dlg[['message']][['7']], icon = "warning", type = "ok")
            tkwait.window(tt)
        }else{
            .cdtData$GalParams$filein <- trimws(tclvalue(file.stnfl1))
            .cdtData$GalParams$all.period <- switch(tclvalue(all.period), '0' = FALSE, '1' = TRUE)
            .cdtData$GalParams$tstep <- periodVAL[CbperiodVAL %in% trimws(tclvalue(timeSteps))]
            .cdtData$GalParams$minhour <- as.numeric(trimws(tclvalue(minhour.tclVar)))
            .cdtData$GalParams$opfilter <- trimws(tclvalue(filter.crt))
            .cdtData$GalParams$valfilter <- as.numeric(trimws(tclvalue(filter.val)))
            .cdtData$GalParams$file2save <- trimws(tclvalue(file.save1))
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
