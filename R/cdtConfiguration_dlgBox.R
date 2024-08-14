
cdtConfiguration <- function(parent.win){
    largeur0 <- 19
    largeur1 <- 50
    largeur2 <- 15

    xml.dlg <- file.path(.cdtDir$dirLocal, "languages", "cdtConfiguration_dlgBox.xml")
    lang.dlg <- cdtLanguageParse(xml.dlg, .cdtData$Config$lang.iso)

    cdt.file.conf <- file.path(.cdtDir$dirLocal, "config", "cdt_config.json")
    cdtConfig <- jsonlite::fromJSON(cdt.file.conf)

    tcl.file.conf <- file.path(.cdtDir$dirLocal, "config", "Tcl_config.json")
    tclConfig <- jsonlite::fromJSON(tcl.file.conf)

    ####################################

    tt <- tktoplevel()
    tkgrab.set(tt)
    tkfocus(tt)

    frMRG0 <- tkframe(tt, relief = 'raised', borderwidth = 2)
    frMRG1 <- tkframe(tt)

    ####################################

    bwnote <- bwNoteBook(frMRG0)
    conf.tab1 <- bwAddTab(bwnote, text = lang.dlg[['tab_title']][['1']])
    conf.tab2 <- bwAddTab(bwnote, text = lang.dlg[['tab_title']][['2']])
    conf.tab3 <- bwAddTab(bwnote, text = lang.dlg[['tab_title']][['3']])
    conf.tab4 <- bwAddTab(bwnote, text = lang.dlg[['tab_title']][['4']])
    conf.tab5 <- bwAddTab(bwnote, text = lang.dlg[['tab_title']][['5']])

    bwRaiseTab(bwnote, conf.tab1)
    tkgrid.columnconfigure(conf.tab1, 0, weight = 1)
    tkgrid.columnconfigure(conf.tab2, 0, weight = 1)
    tkgrid.columnconfigure(conf.tab3, 0, weight = 1)
    tkgrid.columnconfigure(conf.tab4, 0, weight = 1)
    tkgrid.columnconfigure(conf.tab5, 0, weight = 1)

    ####################################

    frTab1 <- tkframe(conf.tab1)

        ####################################

        lang.iso <- cdtConfig$lang.iso
        lang.iso.list <- cdtConfig$lang.iso.list
        lang.name.list <- cdtConfig$lang.name.list
        lang.select <- tclVar(lang.name.list[lang.iso.list %in% lang.iso])
        miss.value <- tclVar(cdtConfig$missval)
        miss.value.anom <- tclVar(cdtConfig$missval.anom)
        work.dir <- tclVar(.cdtData$Config$wd)
        file.exts <- tclVar()
        CbfileExtVAL <- lang.dlg[['combobox']][['1']]
        fileExtVAL <- c('csv', 'txt')
        tclvalue(file.exts) <- CbfileExtVAL[fileExtVAL %in% cdtConfig$ascii.file.ext]

        txt.lang <- tklabel(frTab1, text = lang.dlg[['label']][['1']], anchor = 'e', justify = 'right')
        cb.lang <- ttkcombobox(frTab1, values = lang.name.list, textvariable = lang.select, width = largeur0, justify = 'center')
        txt.miss <- tklabel(frTab1, text = lang.dlg[['label']][['2']], anchor = 'e', justify = 'right')
        en.miss <- tkentry(frTab1, textvariable = miss.value, width = 8)
        txt.amiss <- tklabel(frTab1, text = lang.dlg[['label']][['3']], anchor = 'e', justify = 'right')
        en.amiss <- tkentry(frTab1, textvariable = miss.value.anom, width = 8)
        txt.exts <- tklabel(frTab1, text = lang.dlg[['label']][['17']], anchor = 'e', justify = 'right')
        cb.exts <- ttkcombobox(frTab1, values = CbfileExtVAL, textvariable = file.exts, width = largeur0, justify = 'center')
 
        ######
        frWd <- tkframe(frTab1, relief = 'groove', borderwidth = 2)

        txt.wd <- tklabel(frWd, text = lang.dlg[['label']][['4']], anchor = 'w', justify = 'left')
        en.wd <- tkentry(frWd, textvariable = work.dir, width = largeur1)
        bt.wd <- tkbutton(frWd, text = "...")

        tkgrid(txt.wd, row = 0, column = 0, sticky = 'we', rowspan = 1, columnspan = 5, padx = 1, pady = 1, ipadx = 1, ipady = 1)
        tkgrid(en.wd, row = 1, column = 0, sticky = 'we', rowspan = 1, columnspan = 4, padx = 0, pady = 1, ipadx = 1, ipady = 1)
        tkgrid(bt.wd, row = 1, column = 4, sticky = 'w', rowspan = 1, columnspan = 1, padx = 0, pady = 1, ipadx = 1, ipady = 1)

        tkconfigure(bt.wd, command = function(){
            tcl('wm', 'attributes', tt, topmost = FALSE)
            dir.in <- tk_choose.dir(.cdtData$Config$wd, "")
            tcl('wm', 'attributes', tt, topmost = TRUE)
            tclvalue(work.dir) <- if(dir.in %in% c("", "NA") | is.na(dir.in)) "" else dir.in
        })

        ######
        tkgrid(txt.lang, row = 0, column = 0, sticky = 'we', rowspan = 1, columnspan = 3, padx = 1, pady = 1, ipadx = 1, ipady = 1)
        tkgrid(cb.lang, row = 0, column = 3, sticky = 'w', rowspan = 1, columnspan = 2, padx = 1, pady = 1, ipadx = 1, ipady = 1)
        tkgrid(txt.miss, row = 1, column = 0, sticky = 'we', rowspan = 1, columnspan = 3, padx = 1, pady = 1, ipadx = 1, ipady = 1)
        tkgrid(en.miss, row = 1, column = 3, sticky = 'w', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
        tkgrid(txt.amiss, row = 2, column = 0, sticky = 'we', rowspan = 1, columnspan = 3, padx = 1, pady = 1, ipadx = 1, ipady = 1)
        tkgrid(en.amiss, row = 2, column = 3, sticky = 'w', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
        tkgrid(txt.exts, row = 3, column = 0, sticky = 'we', rowspan = 1, columnspan = 3, padx = 1, pady = 1, ipadx = 1, ipady = 1)
        tkgrid(cb.exts, row = 3, column = 3, sticky = 'w', rowspan = 1, columnspan = 2, padx = 1, pady = 1, ipadx = 1, ipady = 1)
        tkgrid(frWd, row = 4, column = 0, sticky = 'we', rowspan = 1, columnspan = 5, padx = 1, pady = 1, ipadx = 1, ipady = 1)

        helpWidget(en.miss, lang.dlg[['tooltip']][['1']], lang.dlg[['status']][['1']])
        helpWidget(en.amiss, lang.dlg[['tooltip']][['2']], lang.dlg[['status']][['2']])

        ####################################

        tkgrid(frTab1, padx = 0, pady = 1, ipadx = 1, ipady = 1)

    ####################################

    frTab2 <- tkframe(conf.tab2)

        ####################################

        minLon <- tclVar(cdtConfig$region$minlon)
        maxLon <- tclVar(cdtConfig$region$maxlon)
        minLat <- tclVar(cdtConfig$region$minlat)
        maxLat <- tclVar(cdtConfig$region$maxlat)

        fr_grd <- ttklabelframe(frTab2, text = lang.dlg[['label']][['8']], relief = "groove", borderwidth = 2)

        grd_llon <- tklabel(fr_grd, text = lang.dlg[['label']][['9']], anchor = 'e', justify = 'right', width = largeur2)
        grd_llat <- tklabel(fr_grd, text = lang.dlg[['label']][['10']], anchor = 'e', justify = 'right')
        grd_lb1 <- tklabel(fr_grd, text = lang.dlg[['label']][['11']])
        grd_lb2 <- tklabel(fr_grd, text = lang.dlg[['label']][['12']])
        grd_vlon1 <- tkentry(fr_grd, width = 8, justify = "right", textvariable = minLon)
        grd_vlon2 <- tkentry(fr_grd, width = 8, justify = "right", textvariable = maxLon)
        grd_vlat1 <- tkentry(fr_grd, width = 8, justify = "right", textvariable = minLat)
        grd_vlat2 <- tkentry(fr_grd, width = 8, justify = "right", textvariable = maxLat)

        tkgrid(grd_lb1, row = 0, column = 1, sticky = "ew")
        tkgrid(grd_lb2, row = 0, column = 2, sticky = "ew")
        tkgrid(grd_llon, row = 1, column = 0, sticky = "ew")
        tkgrid(grd_vlon1, row = 1, column = 1, sticky = "ew")
        tkgrid(grd_vlon2, row = 1, column = 2, sticky = "ew")
        tkgrid(grd_llat, row = 2, column = 0, sticky = "ew")
        tkgrid(grd_vlat1, row = 2, column = 1, sticky = "ew")
        tkgrid(grd_vlat2, row = 2, column = 2, sticky = "ew")

        tkgrid(fr_grd, row = 0, column = 0, sticky = "ew", padx = 5, pady = 5)

        helpWidget(grd_vlon1, lang.dlg[['tooltip']][['9']], lang.dlg[['status']][['9']])
        helpWidget(grd_vlon2, lang.dlg[['tooltip']][['10']], lang.dlg[['status']][['10']])
        helpWidget(grd_vlat1, lang.dlg[['tooltip']][['11']], lang.dlg[['status']][['11']])
        helpWidget(grd_vlat2, lang.dlg[['tooltip']][['12']], lang.dlg[['status']][['12']])

        ####################################

        tkgrid(frTab2, padx = 0, pady = 1, ipadx = 1, ipady = 1)

    ####################################

    frTab3 <- tkframe(conf.tab3)

        ####################################

        if(WindowsOS()) ostype <- "Windows"
        if(MacOSXP()) ostype <- "MacOS"
        if(LinuxOS()) ostype <- "Linux"
        tclConf <- tclConfig[[ostype]]

        Tktable.auto <- tclVar(tclConf$Tktable.auto)
        Tktable.path <- tclVar(tclConf$Tktable.path)

        Bwidget.auto <- tclVar(tclConf$Bwidget.auto)
        Bwidget.path <- tclVar(tclConf$Bwidget.path)

        stateTkTb <- if(tclConf$Tktable.auto) "disabled" else "normal"
        stateBw <- if(tclConf$Bwidget.auto) "disabled" else "normal"

        if(WindowsOS()){
            useOTcl <- tclVar(tclConf$UseOtherTclTk)
            Tclbin <- tclVar(tclConf$Tclbin)
            statebin <- if(tclConf$UseOtherTclTk) "normal" else "disabled"

            fr.win <- tkframe(frTab3, relief = 'groove', borderwidth = 2)
            chk.win <- tkcheckbutton(fr.win, variable = useOTcl, text = lang.dlg[['checkbutton']][['1']], anchor = 'w', justify = 'left')
            txt.bin <- tklabel(fr.win, text = lang.dlg[['label']][['5']], anchor = 'w', justify = 'left')
            en.bin <- tkentry(fr.win, textvariable = Tclbin, width = largeur1, state = statebin)
            bt.bin <- tkbutton(fr.win, text = "...", state = statebin)

            tkconfigure(bt.bin, command = function(){
                tcl('wm', 'attributes', tt, topmost = FALSE)
                dir.in <- tk_choose.dir(getwd(), "")
                tcl('wm', 'attributes', tt, topmost = TRUE)
                tclvalue(Tclbin) <- if(dir.in %in% c("", "NA") | is.na(dir.in)) "" else dir.in
            })

            tkgrid(chk.win, row = 0, column = 0, sticky = 'we', rowspan = 1, columnspan = 5, padx = 1, pady = 1, ipadx = 1)
            tkgrid(txt.bin, row = 1, column = 0, sticky = 'we', rowspan = 1, columnspan = 5, padx = 1, pady = 1, ipadx = 1)
            tkgrid(en.bin, row = 2, column = 0, sticky = 'we', rowspan = 1, columnspan = 4, padx = 0, pady = 1, ipadx = 1)
            tkgrid(bt.bin, row = 2, column = 4, sticky = 'w', rowspan = 1, columnspan = 1, padx = 0, pady = 1, ipadx = 1)

            helpWidget(chk.win, lang.dlg[['tooltip']][['3']], lang.dlg[['status']][['3']])
            helpWidget(en.bin, lang.dlg[['tooltip']][['4']], lang.dlg[['status']][['4']])
        }

        fr.TkTb <- tkframe(frTab3, relief = 'groove', borderwidth = 2)
        chk.TkTb <- tkcheckbutton(fr.TkTb, variable = Tktable.auto, text = lang.dlg[['checkbutton']][['2']], anchor = 'w', justify = 'left')
        txt.TkTb <- tklabel(fr.TkTb, text = lang.dlg[['label']][['6']], anchor = 'w', justify = 'left')
        en.TkTb <- tkentry(fr.TkTb, textvariable = Tktable.path, width = largeur1, state = stateTkTb)
        bt.TkTb <- tkbutton(fr.TkTb, text = "...", state = stateTkTb)

        fr.Bw <- tkframe(frTab3, relief = 'groove', borderwidth = 2)
        chk.Bw <- tkcheckbutton(fr.Bw, variable = Bwidget.auto, text = lang.dlg[['checkbutton']][['3']], anchor = 'w', justify = 'left')
        txt.Bw <- tklabel(fr.Bw, text = lang.dlg[['label']][['7']], anchor = 'w', justify = 'left')
        en.Bw <- tkentry(fr.Bw, textvariable = Bwidget.path, width = largeur1, state = stateBw)
        bt.Bw <- tkbutton(fr.Bw, text = "...", state = stateBw)

        tkconfigure(bt.TkTb, command = function(){
            tcl('wm', 'attributes', tt, topmost = FALSE)
            dir.in <- tk_choose.dir(getwd(), "")
            tcl('wm', 'attributes', tt, topmost = TRUE)
            tclvalue(Tktable.path) <- if(dir.in %in% c("", "NA") | is.na(dir.in)) "" else dir.in
        })

        tkconfigure(bt.Bw, command = function(){
            tcl('wm', 'attributes', tt, topmost = FALSE)
            dir.in <- tk_choose.dir(getwd(), "")
            tcl('wm', 'attributes', tt, topmost = TRUE)
            tclvalue(Bwidget.path) <- if(dir.in %in% c("", "NA") | is.na(dir.in)) "" else dir.in
        })

        tkgrid(chk.TkTb, row = 0, column = 0, sticky = 'we', rowspan = 1, columnspan = 5, padx = 1, pady = 1, ipadx = 1)
        tkgrid(txt.TkTb, row = 1, column = 0, sticky = 'we', rowspan = 1, columnspan = 5, padx = 1, pady = 1, ipadx = 1)
        tkgrid(en.TkTb, row = 2, column = 0, sticky = 'we', rowspan = 1, columnspan = 4, padx = 0, pady = 1, ipadx = 1)
        tkgrid(bt.TkTb, row = 2, column = 4, sticky = 'w', rowspan = 1, columnspan = 1, padx = 0, pady = 1, ipadx = 1)

        tkgrid(chk.Bw, row = 0, column = 0, sticky = 'we', rowspan = 1, columnspan = 5, padx = 1, pady = 1, ipadx = 1)
        tkgrid(txt.Bw, row = 1, column = 0, sticky = 'we', rowspan = 1, columnspan = 5, padx = 1, pady = 1, ipadx = 1)
        tkgrid(en.Bw, row = 2, column = 0, sticky = 'we', rowspan = 1, columnspan = 4, padx = 0, pady = 1, ipadx = 1)
        tkgrid(bt.Bw, row = 2, column = 4, sticky = 'w', rowspan = 1, columnspan = 1, padx = 0, pady = 1, ipadx = 1)

        helpWidget(chk.TkTb, lang.dlg[['tooltip']][['5']], lang.dlg[['status']][['5']])
        helpWidget(en.TkTb, lang.dlg[['tooltip']][['6']], lang.dlg[['status']][['6']])
        helpWidget(chk.Bw, lang.dlg[['tooltip']][['7']], lang.dlg[['status']][['7']])
        helpWidget(en.Bw, lang.dlg[['tooltip']][['8']], lang.dlg[['status']][['8']])

        #############

        if(WindowsOS()){
            tkbind(chk.win, "<Button-1>", function(){
                statebin <- if(tclvalue(useOTcl) == '1') 'disabled' else 'normal'
                tkconfigure(en.bin, state = statebin)
                tkconfigure(bt.bin, state = statebin)
            })
        }

        tkbind(chk.TkTb, "<Button-1>", function(){
            stateTkTb <- if(tclvalue(Tktable.auto) == '1') 'normal' else 'disabled'
            tkconfigure(en.TkTb, state = stateTkTb)
            tkconfigure(bt.TkTb, state = stateTkTb)
        })

        tkbind(chk.Bw, "<Button-1>", function(){
            stateBw <- if(tclvalue(Bwidget.auto) == '1') 'normal' else 'disabled'
            tkconfigure(en.Bw, state = stateBw)
            tkconfigure(bt.Bw, state = stateBw)
        })

        #############

        if(WindowsOS()) tkgrid(fr.win, row = 0, sticky = 'we', padx = 1, pady = 1)
        tkgrid(fr.TkTb, row = 1, sticky = 'we', padx = 1, pady = 1)
        tkgrid(fr.Bw, row = 2, sticky = 'we', padx = 1, pady = 1)

        ####################################

        tkgrid(frTab3, padx = 0, pady = 1, ipadx = 1, ipady = 1)

    ####################################

    frTab4 <- tkframe(conf.tab4)

        ####################################

        chunkSize <- tclVar(cdtConfig$cdtDataset.chunk$chunksize)
        chunkFac <- tclVar(cdtConfig$cdtDataset.chunk$chunkfac)
        chunkFun <- tclVar(cdtConfig$cdtDataset.chunk$chunkfun)

        txt.cdtD <- tklabel(frTab4, text = lang.dlg[['label']][['15']], anchor = 'w', justify = 'left')
        txt.chkS <- tklabel(frTab4, text = lang.dlg[['label']][['13']], anchor = 'e', justify = 'right')
        en.chkS <- tkentry(frTab4, textvariable = chunkSize, width = 6)
        txt.chkF <- tklabel(frTab4, text = lang.dlg[['label']][['14']], anchor = 'e', justify = 'right')
        en.chkF <- tkentry(frTab4, textvariable = chunkFac, width = 3)
        txt.chkFo <- tklabel(frTab4, text = lang.dlg[['label']][['14-a']], anchor = 'e', justify = 'right')
        cb.chkFo <- ttkcombobox(frTab4, values = 1:2, textvariable = chunkFun, width = 3, justify = 'center')

        helpWidget(en.chkS, lang.dlg[['tooltip']][['13']], lang.dlg[['status']][['13']])
        helpWidget(en.chkF, lang.dlg[['tooltip']][['14']], lang.dlg[['status']][['14']])
        helpWidget(cb.chkFo, lang.dlg[['tooltip']][['14-a']], lang.dlg[['status']][['14-a']])

        tkgrid(txt.cdtD, row = 0, column = 0, sticky = 'we', rowspan = 1, columnspan = 2, padx = 1, pady = 3, ipadx = 1, ipady = 3)
        tkgrid(txt.chkS, row = 1, column = 0, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
        tkgrid(en.chkS, row = 1, column = 1, sticky = 'w', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
        tkgrid(txt.chkF, row = 2, column = 0, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
        tkgrid(en.chkF, row = 2, column = 1, sticky = 'w', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)

        tkgrid(txt.chkFo, row = 3, column = 0, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
        tkgrid(cb.chkFo, row = 3, column = 1, sticky = 'w', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)

        ####################################

        tkgrid(frTab4, padx = 0, pady = 1, ipadx = 1, ipady = 1)

    ####################################

    frTab5 <- tkframe(conf.tab5)

        ####################################

        doparVar <- tclVar(cdtConfig$parallel$dopar)
        coresVar <- tclVar(cdtConfig$parallel$detect.cores)
        nbCores <- tclVar(cdtConfig$parallel$nb.cores)

        stateCores <- if(cdtConfig$parallel$detect.cores) "disabled" else "normal"

        chk.dopar <- tkcheckbutton(frTab5, variable = doparVar, text = lang.dlg[['checkbutton']][['4']], anchor = 'w', justify = 'left')
        chk.cores <- tkcheckbutton(frTab5, variable = coresVar, text = lang.dlg[['checkbutton']][['5']], anchor = 'w', justify = 'left')
        txt.cores <- tklabel(frTab5, text = lang.dlg[['label']][['16']], anchor = 'e', justify = 'right')
        en.cores <- tkentry(frTab5, textvariable = nbCores, width = 3, state = stateCores)

        tkgrid(chk.dopar, row = 0, column = 0, sticky = 'we', rowspan = 1, columnspan = 2, padx = 1, pady = 3, ipadx = 1, ipady = 1)
        tkgrid(chk.cores, row = 1, column = 0, sticky = 'we', rowspan = 1, columnspan = 2, padx = 1, pady = 1, ipadx = 1, ipady = 1)
        tkgrid(txt.cores, row = 2, column = 0, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 3, ipadx = 1, ipady = 1)
        tkgrid(en.cores, row = 2, column = 1, sticky = 'w', rowspan = 1, columnspan = 1, padx = 1, pady = 3, ipadx = 1, ipady = 1)

        tkbind(chk.cores, "<Button-1>", function(){
            stateCores <- if(tclvalue(coresVar) == '1') 'normal' else 'disabled'
            tkconfigure(en.cores, state = stateCores)
        })

        ####################################

        tkgrid(frTab5, padx = 0, pady = 1, ipadx = 1, ipady = 1)

    ####################################

    bt.prm.OK <- ttkbutton(frMRG1, text = lang.dlg[['button']][['1']])
    bt.prm.CA <- ttkbutton(frMRG1, text = lang.dlg[['button']][['2']])

    tkconfigure(bt.prm.OK, command = function(){
        if(WindowsOS()){
            if(tclvalue(useOTcl) == "1" &
                trimws(tclvalue(Tclbin)) %in% c("", "NA"))
            {
                cdt.tkmessageBox(tt, message = lang.dlg[['message']][['2']], icon = "warning", type = "ok")
                tkwait.window(tt)
            }
            if(tclvalue(useOTcl) == "1" &
                !dir.exists(trimws(tclvalue(Tclbin))))
            {
                cdt.tkmessageBox(tt, message = paste(tclvalue(Tclbin), lang.dlg[['message']][['3']]), icon = "warning", type = "ok")
                tkwait.window(tt)
            }
        }
        if(tclvalue(Tktable.auto) == "0" &
            trimws(tclvalue(Tktable.path)) %in% c("", "NA"))
        {
            cdt.tkmessageBox(tt, message = lang.dlg[['message']][['4']], icon = "warning", type = "ok")
            tkwait.window(tt)
        }else if(tclvalue(Tktable.auto) == "0" &
            !dir.exists(trimws(tclvalue(Tktable.path))))
        {
            cdt.tkmessageBox(tt, message = paste(tclvalue(Tktable.path), lang.dlg[['message']][['3']]), icon = "warning", type = "ok")
            tkwait.window(tt)
        }else if(tclvalue(Bwidget.auto) == "0" &
            trimws(tclvalue(Bwidget.path)) %in% c("", "NA"))
        {
            cdt.tkmessageBox(tt, message = lang.dlg[['message']][['5']], icon = "warning", type = "ok")
            tkwait.window(tt)
        }else if(tclvalue(Bwidget.auto) == "0" &
            !dir.exists(trimws(tclvalue(Bwidget.path))))
        {
            cdt.tkmessageBox(tt, message = paste(tclvalue(Bwidget.path), lang.dlg[['message']][['3']]), icon = "warning", type = "ok")
            tkwait.window(tt)
        }else{
            if(WindowsOS()){
                tclConfig[[ostype]]$UseOtherTclTk <- switch(tclvalue(useOTcl), '0' = FALSE, '1' = TRUE)
                tclConfig[[ostype]]$Tclbin <- trimws(tclvalue(Tclbin))
            }
            tclConfig[[ostype]]$Tktable.auto <- switch(tclvalue(Tktable.auto), '0' = FALSE, '1' = TRUE)
            tclConfig[[ostype]]$Tktable.path <- trimws(tclvalue(Tktable.path))
            tclConfig[[ostype]]$Bwidget.auto <- switch(tclvalue(Bwidget.auto), '0' = FALSE, '1' = TRUE)
            tclConfig[[ostype]]$Bwidget.path <- trimws(tclvalue(Bwidget.path))

            jsonlite::write_json(tclConfig, path = tcl.file.conf, auto_unbox = TRUE, pretty = TRUE)

            cdtConfig$lang.iso <- lang.iso.list[lang.name.list %in% trimws(tclvalue(lang.select))]
            cdtConfig$missval <- trimws(tclvalue(miss.value))
            cdtConfig$missval.anom <- trimws(tclvalue(miss.value.anom))
            cdtConfig$wd <- trimws(tclvalue(work.dir))
            cdtConfig$ascii.file.ext <- fileExtVAL[CbfileExtVAL %in% trimws(tclvalue(file.exts))]
            cdtConfig$region$minlon <- as.numeric(trimws(tclvalue(minLon)))
            cdtConfig$region$maxlon <- as.numeric(trimws(tclvalue(maxLon)))
            cdtConfig$region$minlat <- as.numeric(trimws(tclvalue(minLat)))
            cdtConfig$region$maxlat <- as.numeric(trimws(tclvalue(maxLat)))

            cdtConfig$cdtDataset.chunk$chunksize <- as.integer(trimws(tclvalue(chunkSize)))
            cdtConfig$cdtDataset.chunk$chunkfac <- as.integer(trimws(tclvalue(chunkFac)))
            cdtConfig$cdtDataset.chunk$chunkfun <- as.integer(trimws(tclvalue(chunkFun)))

            cdtConfig$parallel$dopar <- switch(tclvalue(doparVar), '0' = FALSE, '1' = TRUE)
            cdtConfig$parallel$detect.cores <- switch(tclvalue(coresVar), '0' = FALSE, '1' = TRUE)
            cdtConfig$parallel$nb.cores <- as.integer(trimws(tclvalue(nbCores)))

            jsonlite::write_json(cdtConfig, path = cdt.file.conf, auto_unbox = TRUE, pretty = TRUE)

            .cdtData$Config$wd <- cdtConfig$wd
            .cdtData$Config$missval <- cdtConfig$missval
            .cdtData$Config$missval.anom <- cdtConfig$missval.anom
            .cdtData$Config$lang.iso <- cdtConfig$lang.iso
            .cdtData$Config$ascii.file.ext <- cdtConfig$ascii.file.ext
            .cdtData$Config$region <- cdtConfig$region
            .cdtData$Config$cdtDataset.chunk <- cdtConfig$cdtDataset.chunk
            .cdtData$Config$parallel <- cdtConfig$parallel

            setwd(cdtConfig$wd)
            Insert.Messages.Out(lang.dlg[['message']][['1']], TRUE, "i")

            tkgrab.release(tt)
            tkdestroy(tt)
            tkfocus(parent.win)
        }
    })

    tkconfigure(bt.prm.CA, command = function(){
        tkgrab.release(tt)
        tkdestroy(tt)
        tkfocus(parent.win)
    })

    tkgrid(bt.prm.CA, row = 0, column = 0, sticky = 'w', padx = 5, pady = 1, ipadx = 1, ipady = 1)
    tkgrid(bt.prm.OK, row = 0, column = 1, sticky = 'e', padx = 5, pady = 1, ipadx = 1, ipady = 1)

    ####################################

    tkgrid(frMRG0, row = 0, column = 0, sticky = 'nswe', rowspan = 1, columnspan = 2, padx = 1, pady = 1, ipadx = 1, ipady = 1)
    tkgrid(frMRG1, row = 1, column = 1, sticky = 'se', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)

    tcl('update')
    tkgrid(bwnote, sticky = 'nwes')
    tkgrid.columnconfigure(bwnote, 0, weight = 1)

    ####################################

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
        tkfocus(parent.win)
    })
    tkwait.window(tt)
    invisible()
}
