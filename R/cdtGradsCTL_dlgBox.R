
grads_create.ctl_getParams <- function(){
    if(WindowsOS()){
        largeur0 <- 33
        largeur1 <- 70
    }else{
        largeur0 <- 23
        largeur1 <- 45
    }

    xml.dlg <- file.path(.cdtDir$dirLocal, "languages", "cdtGradsCTL_dlgBox.xml")
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

    file.period <- tclVar()

    cb.periodVAL <- .cdtEnv$tcl$lang$global[['combobox']][['1']][c(3, 6, 7)]
    periodVAL <- c('daily', 'monthly', 'annual')
    tclvalue(file.period) <- cb.periodVAL[periodVAL %in% .cdtData$GalParams$tstep]

    cb.period <- ttkcombobox(frtimestep, values = cb.periodVAL, textvariable = file.period, width = largeur0)
    bt.period <- ttkbutton(frtimestep, text = lang.dlg[['button']][['1']], width = largeur0)

    tkconfigure(bt.period, command = function(){
        tstep <- periodVAL[cb.periodVAL %in% str_trim(tclvalue(file.period))]
        .cdtData$GalParams[["date"]] <- getInfoDateRange(tt, .cdtData$GalParams[["date"]], tstep)
    })

    tkgrid(cb.period, row = 0, column = 0, sticky = 'we', padx = 1, pady = 3, ipadx = 1, ipady = 1)
    tkgrid(bt.period, row = 0, column = 1, sticky = 'we', padx = 1, pady = 3, ipadx = 1, ipady = 1)

    helpWidget(cb.period, lang.dlg[['tooltip']][['1']], lang.dlg[['status']][['1']])
    helpWidget(bt.period, lang.dlg[['tooltip']][['5']], lang.dlg[['status']][['5']])

    ############################################

    frameInData <- tkframe(frLeft, relief = 'sunken', borderwidth = 2)

    input.DataF <- tclVar(.cdtData$GalParams$nc$dir)

    txt.indata <- tklabel(frameInData, text = lang.dlg[['label']][['1']], anchor = 'w', justify = 'left')
    set.indata <- ttkbutton(frameInData, text = .cdtEnv$tcl$lang$global[['button']][['5']])
    cb.en.indata <- tkentry(frameInData, textvariable = input.DataF, width = largeur1)
    bt.indata <- tkbutton(frameInData, text = "...")

    settingNCF <- .cdtData$GalParams$settingNCF
    tkconfigure(set.indata, command = function(){
        tcl('wm', 'attributes', tt, topmost = FALSE)
        .cdtData$GalParams[["nc"]] <- grads.getInfoNetcdfData(tt, .cdtData$GalParams[["nc"]],
                                                              str_trim(tclvalue(input.DataF)))
        tcl('wm', 'attributes', tt, topmost = TRUE)
        settingNCF <<- 1
    })

    tkconfigure(bt.indata, command = function(){
        tcl('wm', 'attributes', tt, topmost = FALSE)
        dirnc <- tk_choose.dir(getwd(), "")
        tcl('wm', 'attributes', tt, topmost = TRUE)
        tclvalue(input.DataF) <- if(dirnc %in% c("", "NA") | is.na(dirnc)) "" else dirnc
    })

    ############ 
    tkgrid(txt.indata, row = 0, column = 0, sticky = 'we', rowspan = 1, columnspan = 3, padx = 1, pady = 0, ipadx = 1, ipady = 1)
    tkgrid(set.indata, row = 0, column = 3, sticky = 'we', rowspan = 1, columnspan = 2, padx = 1, pady = 0, ipadx = 1, ipady = 1)
    tkgrid(cb.en.indata, row = 1, column = 0, sticky = 'we', rowspan = 1, columnspan = 4, padx = 0, pady = 0, ipadx = 1, ipady = 1)
    tkgrid(bt.indata, row = 1, column = 4, sticky = 'we', rowspan = 1, columnspan = 1, padx = 0, pady = 0, ipadx = 1, ipady = 1)

    ############ 

    helpWidget(cb.en.indata, lang.dlg[['tooltip']][['2']], lang.dlg[['status']][['2']])
    helpWidget(bt.indata, lang.dlg[['tooltip']][['3']], lang.dlg[['status']][['3']])

    ############################################

    frSave <- tkframe(frLeft, relief = 'sunken', borderwidth = 2)

    file.save <- tclVar(.cdtData$GalParams$out.ctl)

    txt.file.save <- tklabel(frSave, text = lang.dlg[['label']][['2']], anchor = 'w', justify = 'left')
    en.file.save <- tkentry(frSave, textvariable = file.save, width = largeur1)
    bt.file.save <- tkbutton(frSave, text = "...")

    #########
    tkconfigure(bt.file.save, command = function(){
        filetypes <- "{{Control File} {.ctl .CTL}} {{Text Files} {.txt .TXT}} {{All files} *}"
        tcl('wm', 'attributes', tt, topmost = FALSE)
        file2save <- tk_get_SaveFile(filetypes = filetypes)
        tcl('wm', 'attributes', tt, topmost = TRUE)
        tclvalue(file.save) <- if(!is.na(file2save)) file2save else .cdtData$GalParams$out.ctl
    })

    #########
    tkgrid(txt.file.save, row = 0, column = 0, sticky = 'we', rowspan = 1, columnspan = 5, padx = 1, pady = 0, ipadx = 1, ipady = 1)
    tkgrid(en.file.save, row = 1, column = 0, sticky = 'we', rowspan = 1, columnspan = 4, padx = 0, pady = 0, ipadx = 1, ipady = 1)
    tkgrid(bt.file.save, row = 1, column = 4, sticky = 'we', rowspan = 1, columnspan = 1, padx = 0, pady = 0, ipadx = 1, ipady = 1)

    #########

    helpWidget(en.file.save, lang.dlg[['tooltip']][['4']], lang.dlg[['status']][['4']])
    helpWidget(bt.file.save, lang.dlg[['tooltip']][['3']], lang.dlg[['status']][['3']])

    ############################################
    tkgrid(frtimestep, row = 0, column = 0, sticky = '', padx = 1, pady = 3, ipadx = 1, ipady = 1)
    tkgrid(frameInData, row = 1, column = 0, sticky = 'we', padx = 1, pady = 3, ipadx = 1, ipady = 1)
    tkgrid(frSave, row = 2, column = 0, sticky = 'we', padx = 1, pady = 3, ipadx = 1, ipady = 1)

    ############################################
    
    tkgrid(frLeft, row = 0, column = 0, sticky = 'news', padx = 5, pady = 1, ipadx = 1, ipady = 1)

    ############################################

    bt.prm.OK <- ttkbutton(frMRG1, text = .cdtEnv$tcl$lang$global[['button']][['1']])
    bt.prm.CA <- ttkbutton(frMRG1, text = .cdtEnv$tcl$lang$global[['button']][['2']])

    tkconfigure(bt.prm.OK, command = function(){
        if(str_trim(tclvalue(input.DataF)) %in% c("", "NA")){
            cdt.tkmessageBox(tt, message = lang.dlg[['message']][['1']], icon = "warning", type = "ok")
            tkwait.window(tt)
        }else if(tclvalue(file.save) %in% c("", "NA")){
            cdt.tkmessageBox(tt, message = lang.dlg[['message']][['2']], icon = "warning", type = "ok")
            tkwait.window(tt)
        }else if(is.null(settingNCF)){
            cdt.tkmessageBox(tt, message = lang.dlg[['message']][['3']], icon = "warning", type = "ok")
            tkwait.window(tt)
        }else{
            .cdtData$GalParams$tstep <- periodVAL[cb.periodVAL %in% str_trim(tclvalue(file.period))]

            .cdtData$GalParams$nc$dir <- str_trim(tclvalue(input.DataF))
            .cdtData$GalParams$out.ctl <- str_trim(tclvalue(file.save))

            .cdtData$GalParams$settingNCF <- settingNCF
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
