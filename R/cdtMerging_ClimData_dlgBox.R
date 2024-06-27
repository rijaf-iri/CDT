
mergeGetInfoClimData <- function(){
    if(WindowsOS()){
        largeur0 <- 21
        largeur1 <- 47
        largeur2 <- 49
        largeur3 <- 32
        largeur4 <- 18
    }else{
        largeur0 <- 21
        largeur1 <- 43
        largeur2 <- 45
        largeur3 <- 32
        largeur4 <- 17
    }

    ####################################

    xml.dlg <- file.path(.cdtDir$dirLocal, "languages", "cdtMerging_ClimData_dlgBox.xml")
    lang.dlg <- cdtLanguageParse(xml.dlg, .cdtData$Config$lang.iso)

    ####################################

    tt <- tktoplevel()
    tkgrab.set(tt)
    tkfocus(tt)

    frMRG0 <- tkframe(tt, relief = 'raised', borderwidth = 2, padx = 2, pady = 2)
    frMRG1 <- tkframe(tt)

    ####################################

    bwnote <- bwNoteBook(frMRG0)
    conf.tab1 <- bwAddTab(bwnote, text = lang.dlg[['tab_title']][['1']])
    conf.tab2 <- bwAddTab(bwnote, text = lang.dlg[['tab_title']][['2']])

    bwRaiseTab(bwnote, conf.tab1)
    tkgrid.columnconfigure(conf.tab1, 0, weight = 1)
    tkgrid.columnconfigure(conf.tab2, 0, weight = 1)

    ############################################

    frTab1 <- tkframe(conf.tab1)

        ####################################

        frtimestep <- tkframe(frTab1, relief = 'sunken', borderwidth = 2)

        CbperiodVAL <- .cdtEnv$tcl$lang$global[['combobox']][['1']][1:6]
        periodVAL <- c('minute', 'hourly', 'daily', 'pentad', 'dekadal', 'monthly')
        file.period <- tclVar()
        tclvalue(file.period) <- CbperiodVAL[periodVAL %in% .cdtData$GalParams$period]

        retminhr <- set.hour.minute(.cdtData$GalParams$period, .cdtData$GalParams$minhour)
        minhour.tclVar <- tclVar(retminhr$val)

        cb.period <- ttkcombobox(frtimestep, values = CbperiodVAL, textvariable = file.period, justify = 'center', width = largeur0)
        cb.minhour <- ttkcombobox(frtimestep, values = retminhr$cb, textvariable = minhour.tclVar, state = retminhr$state, width = 2)
        bt.DateRange <- ttkbutton(frtimestep, text = lang.dlg[['button']][['1']], width = largeur0)

        #######

        tkgrid(cb.period, row = 0, column = 0, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
        tkgrid(cb.minhour, row = 0, column = 1, sticky = 'we', rowspan = 1, columnspan = 1, padx = 0, pady = 1, ipadx = 1, ipady = 1)
        tkgrid(bt.DateRange, row = 0, column = 2, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)

        helpWidget(cb.period, lang.dlg[['tooltip']][['1']], lang.dlg[['status']][['1']])
        helpWidget(bt.DateRange, lang.dlg[['tooltip']][['2']], lang.dlg[['status']][['2']])

        ###########

        tkconfigure(bt.DateRange, command = function(){
            tstep <- periodVAL[CbperiodVAL %in% trimws(tclvalue(file.period))]
            tcl('wm', 'attributes', tt, topmost = FALSE)
            .cdtData$GalParams[["date.range"]] <- getInfoDateRange(tt, .cdtData$GalParams[["date.range"]], tstep, TRUE)
            tcl('wm', 'attributes', tt, topmost = TRUE)
        })

        tkbind(cb.period, "<<ComboboxSelected>>", function(){
            tstep <- periodVAL[CbperiodVAL %in% trimws(tclvalue(file.period))]
            minhour <- as.numeric(trimws(tclvalue(minhour.tclVar)))
            retminhr <- set.hour.minute(tstep, minhour)
            tkconfigure(cb.minhour, values = retminhr$cb, state = retminhr$state)
            tclvalue(minhour.tclVar) <- retminhr$val
        })

        ############################################

        frInputData <- tkframe(frTab1, relief = 'sunken', borderwidth = 2)

        file.stnfl <- tclVar(.cdtData$GalParams$STN.file)
        dir.InNCDF <- tclVar(.cdtData$GalParams$INPUT$dir)

        txt.stnfl <- tklabel(frInputData, text = lang.dlg[['label']][['1']], anchor = 'w', justify = 'left')
        cb.stnfl <- ttkcombobox(frInputData, values = unlist(openFile_ttkcomboList()), textvariable = file.stnfl, width = largeur1)
        addTo_all_Combobox_List(cb.stnfl)
        bt.stnfl <- tkbutton(frInputData, text = "...")
        txt.InNCDF <- tklabel(frInputData, text = lang.dlg[['label']][['2']], anchor = 'w', justify = 'left')
        set.InNCDF <- ttkbutton(frInputData, text = .cdtEnv$tcl$lang$global[['button']][['5']])
        en.InNCDF <- tkentry(frInputData, textvariable = dir.InNCDF, width = largeur2)
        bt.InNCDF <- tkbutton(frInputData, text = "...")

        ######
        tkgrid(txt.stnfl, row = 0, column = 0, sticky = 'we', rowspan = 1, columnspan = 5, padx = 1, pady = 0, ipadx = 1, ipady = 1)
        tkgrid(cb.stnfl, row = 1, column = 0, sticky = 'we', rowspan = 1, columnspan = 4, padx = 0, pady = 0, ipadx = 1, ipady = 1)
        tkgrid(bt.stnfl, row = 1, column = 4, sticky = 'we', rowspan = 1, columnspan = 1, padx = 0, pady = 0, ipadx = 1, ipady = 1)

        tkgrid(txt.InNCDF, row = 2, column = 0, sticky = 'we', rowspan = 1, columnspan = 3, padx = 1, pady = 0, ipadx = 1, ipady = 1)
        tkgrid(set.InNCDF, row = 2, column = 3, sticky = 'we', rowspan = 1, columnspan = 2, padx = 1, pady = 0, ipadx = 1, ipady = 1)
        tkgrid(en.InNCDF, row = 3, column = 0, sticky = 'we', rowspan = 1, columnspan = 4, padx = 0, pady = 0, ipadx = 1, ipady = 1)
        tkgrid(bt.InNCDF, row = 3, column = 4, sticky = 'we', rowspan = 1, columnspan = 1, padx = 0, pady = 0, ipadx = 1, ipady = 1)

        helpWidget(cb.stnfl, lang.dlg[['tooltip']][['3']], lang.dlg[['status']][['3']])
        helpWidget(bt.stnfl, lang.dlg[['tooltip']][['4']], lang.dlg[['status']][['4']])
        helpWidget(en.InNCDF, lang.dlg[['tooltip']][['5']], lang.dlg[['status']][['5']])
        helpWidget(bt.InNCDF, lang.dlg[['tooltip']][['6']], lang.dlg[['status']][['6']])
        helpWidget(set.InNCDF, lang.dlg[['tooltip']][['7']], lang.dlg[['status']][['7']])

        ######
        tkconfigure(bt.stnfl, command = function(){
            tcl('wm', 'attributes', tt, topmost = FALSE)
            dat.opfiles <- getOpenFiles(tt)
            tcl('wm', 'attributes', tt, topmost = TRUE)
            if(!is.null(dat.opfiles)){
                update.OpenFiles('ascii', dat.opfiles)
                tclvalue(file.stnfl) <- dat.opfiles[[1]]
            }
        })

        settingSNC <- .cdtData$GalParams$settingSNC
        tkconfigure(set.InNCDF, command = function(){
            tcl('wm', 'attributes', tt, topmost = FALSE)
            .cdtData$GalParams[["INPUT"]] <- getInfoNetCDFData(tt, .cdtData$GalParams[["INPUT"]],
                                                               trimws(tclvalue(dir.InNCDF)))
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

        frSave <- tkframe(frTab1, relief = 'sunken', borderwidth = 2)

        dir2save <- tclVar(.cdtData$GalParams$output$dir)
        outmrgff <- tclVar(.cdtData$GalParams$output$format)

        txt.dir2save <- tklabel(frSave, text = lang.dlg[['label']][['3']], anchor = 'w', justify = 'left')
        en.dir2save <- tkentry(frSave, textvariable = dir2save, width = largeur2)
        bt.dir2save <- tkbutton(frSave, text = "...")
        txt.outmrgff <- tklabel(frSave, text = lang.dlg[['label']][['4']], anchor = 'w', justify = 'left')
        en.outmrgff <- tkentry(frSave, textvariable = outmrgff, width = largeur2)

        #####

        tkgrid(txt.dir2save, row = 0, column = 0, sticky = 'we', rowspan = 1, columnspan = 2, padx = 1, pady = 2, ipadx = 1, ipady = 1)
        tkgrid(en.dir2save, row = 1, column = 0, sticky = 'we', rowspan = 1, columnspan = 1, padx = 0, pady = 1, ipadx = 1, ipady = 1)
        tkgrid(bt.dir2save, row = 1, column = 1, sticky = 'we', rowspan = 1, columnspan = 1, padx = 0, pady = 1, ipadx = 1, ipady = 1)
        tkgrid(txt.outmrgff, row = 2, column = 0, sticky = 'we', rowspan = 1, columnspan = 2, padx = 1, pady = 1, ipadx = 1, ipady = 1)
        tkgrid(en.outmrgff, row = 3, column = 0, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)

        helpWidget(en.dir2save, lang.dlg[['tooltip']][['8']], lang.dlg[['status']][['8']])
        helpWidget(bt.dir2save, lang.dlg[['tooltip']][['6']], lang.dlg[['status']][['6']])
        helpWidget(en.outmrgff, lang.dlg[['tooltip']][['9']], lang.dlg[['status']][['9']])

        #####

        tkconfigure(bt.dir2save, command = function(){
            tcl('wm', 'attributes', tt, topmost = FALSE)
            dir2savepth <- tk_choose.dir(.cdtData$GalParams$output$dir, "")
            tcl('wm', 'attributes', tt, topmost = TRUE)
            if(is.na(dir2savepth))
                tclvalue(dir2save) <- .cdtData$GalParams$output$dir
            else{
                dir.create(dir2savepth, showWarnings = FALSE, recursive = TRUE)
                tclvalue(dir2save) <- dir2savepth
            }
        })

        ############################################

        tkgrid(frtimestep, row = 0, column = 0, sticky = '', padx = 1, pady = 1, ipadx = 1, ipady = 1)
        tkgrid(frInputData, row = 1, column = 0, sticky = 'we', padx = 1, pady = 1, ipadx = 1, ipady = 1)
        tkgrid(frSave, row = 2, column = 0, sticky = 'we', padx = 1, pady = 1, ipadx = 1, ipady = 1)

        ####################################

        tkgrid(frTab1, padx = 0, pady = 1, ipadx = 1, ipady = 1)

    ############################################

    frTab2 <- tkframe(conf.tab2)

        ####################################

        auxiliary.variables <- function(mrgmethod){
            tkdestroy(frauxvar)

            frauxvar <<- ttklabelframe(frMerge, text = lang.dlg[['label']][['5']], relief = 'groove', borderwidth = 2)

            if(mrgmethod == "RK"){
                frAUX <- tkframe(frauxvar)

                dem.chk.auxvar <- tkcheckbutton(frAUX, variable = dem.auxvar, text = lang.dlg[['checkbutton']][['1']], anchor = 'w', justify = 'left')
                slope.chk.auxvar <- tkcheckbutton(frAUX, variable = slope.auxvar, text = lang.dlg[['checkbutton']][['2']], anchor = 'w', justify = 'left')
                aspect.chk.auxvar <- tkcheckbutton(frAUX, variable = aspect.auxvar, text = lang.dlg[['checkbutton']][['3']], anchor = 'w', justify = 'left')
                lon.chk.auxvar <- tkcheckbutton(frAUX, variable = lon.auxvar, text = lang.dlg[['checkbutton']][['4']], anchor = 'w', justify = 'left')
                lat.chk.auxvar <- tkcheckbutton(frAUX, variable = lat.auxvar, text = lang.dlg[['checkbutton']][['5']], anchor = 'w', justify = 'left')

                tkgrid(dem.chk.auxvar, row = 0, column = 0, sticky = 'we', rowspan = 1, columnspan = 1, ipady = 1)
                tkgrid(slope.chk.auxvar, row = 0, column = 1, sticky = 'we', rowspan = 1, columnspan = 1, ipady = 1)
                tkgrid(aspect.chk.auxvar, row = 0, column = 2, sticky = 'we', rowspan = 1, columnspan = 1, ipady = 1)
                tkgrid(lon.chk.auxvar, row = 0, column = 3, sticky = 'we', rowspan = 1, columnspan = 1, ipady = 1)
                tkgrid(lat.chk.auxvar, row = 0, column = 4, sticky = 'we', rowspan = 1, columnspan = 1, ipady = 1)

                helpWidget(dem.chk.auxvar, lang.dlg[['tooltip']][['10']], lang.dlg[['status']][['10']])
                helpWidget(slope.chk.auxvar, lang.dlg[['tooltip']][['11']], lang.dlg[['status']][['11']])
                helpWidget(aspect.chk.auxvar, lang.dlg[['tooltip']][['12']], lang.dlg[['status']][['12']])
                helpWidget(lon.chk.auxvar, lang.dlg[['tooltip']][['13']], lang.dlg[['status']][['13']])
                helpWidget(lat.chk.auxvar, lang.dlg[['tooltip']][['14']], lang.dlg[['status']][['14']])

                ###########

                frDEM <- tkframe(frauxvar)

                statedem <- if(tclvalue(dem.auxvar) == "1" |
                               tclvalue(slope.auxvar) == "1" |
                               tclvalue(aspect.auxvar) == "1") "normal" else "disabled"
 
                txt.grddem <- tklabel(frDEM, text = lang.dlg[['label']][['6']], anchor = 'w', justify = 'left')
                cb.grddem <- ttkcombobox(frDEM, values = unlist(openFile_ttkcomboList()), textvariable = demfile.var, width = largeur1, state = statedem)
                addTo_all_Combobox_List(cb.grddem)
                bt.grddem <- tkbutton(frDEM, text = "...", state = statedem)

                tkgrid(txt.grddem, row = 0, column = 0, sticky = 'we', rowspan = 1, columnspan = 2, ipady = 1)
                tkgrid(cb.grddem, row = 1, column = 0, sticky = 'we', rowspan = 1, columnspan = 1, ipady = 1)
                tkgrid(bt.grddem, row = 1, column = 1, sticky = 'we', rowspan = 1, columnspan = 1, ipady = 1)

                helpWidget(cb.grddem, lang.dlg[['tooltip']][['15']], lang.dlg[['status']][['15']])
                helpWidget(bt.grddem, lang.dlg[['tooltip']][['4']], lang.dlg[['status']][['4']])

                ###########

                tkgrid(frAUX, row = 0, column = 0, sticky = 'we', rowspan = 1, columnspan = 1, ipady = 1)
                tkgrid(frDEM, row = 1, column = 0, sticky = 'we', rowspan = 1, columnspan = 1, ipady = 1)

                ###########

                tkconfigure(bt.grddem, command = function(){
                    tcl('wm', 'attributes', tt, topmost = FALSE)
                    nc.opfiles <- getOpenNetcdf(tt, initialdir = getwd())
                    tcl('wm', 'attributes', tt, topmost = TRUE)
                    if(!is.null(nc.opfiles)){
                        update.OpenFiles('netcdf', nc.opfiles)
                        tclvalue(demfile.var) <- nc.opfiles[[1]]
                    }
                })

                tkbind(dem.chk.auxvar, "<Button-1>", function(){
                    statedem <- if(tclvalue(dem.auxvar) == "0" |
                                  (tclvalue(slope.auxvar) == "1" |
                                   tclvalue(aspect.auxvar) == "1")) 'normal' else 'disabled'
                    tkconfigure(cb.grddem, state = statedem)
                    tkconfigure(bt.grddem, state = statedem)
                })

                tkbind(slope.chk.auxvar, "<Button-1>", function(){
                    statedem <- if(tclvalue(slope.auxvar) == "0" |
                                  (tclvalue(dem.auxvar) == "1" |
                                   tclvalue(aspect.auxvar) == "1")) 'normal' else 'disabled'
                    tkconfigure(cb.grddem, state = statedem)
                    tkconfigure(bt.grddem, state = statedem)
                })

                tkbind(aspect.chk.auxvar, "<Button-1>", function(){
                    statedem <- if(tclvalue(aspect.auxvar) == "0" |
                                  (tclvalue(slope.auxvar) == "1" |
                                   tclvalue(dem.auxvar) == "1")) 'normal' else 'disabled'
                    tkconfigure(cb.grddem, state = statedem)
                    tkconfigure(bt.grddem, state = statedem)
                })

                ######
                tkgrid(frauxvar, row = 2, column = 0, sticky = 'we', rowspan = 1, columnspan = 2, padx = 1, pady = 2, ipadx = 1, ipady = 1)
            }
        }

        ####################################

        frMerge <- tkframe(frTab2, relief = 'sunken', borderwidth = 2, pady = 3)

        cb.mrgMthd <- lang.dlg[['combobox']][['1']]
        val.mrgMthd <- c("CSc", "BSc", "SBA", "RK")

        merge.method <- tclVar()
        tclvalue(merge.method) <- cb.mrgMthd[val.mrgMthd %in% .cdtData$GalParams$MRG$method]

        nb.run <- tclVar(.cdtData$GalParams$MRG$nrun)
        pass.ratio <- tclVar(paste0(.cdtData$GalParams$MRG$pass, collapse = ", "))

        dem.auxvar <- tclVar(.cdtData$GalParams$auxvar$dem)
        slope.auxvar <- tclVar(.cdtData$GalParams$auxvar$slope)
        aspect.auxvar <- tclVar(.cdtData$GalParams$auxvar$aspect)
        lon.auxvar <- tclVar(.cdtData$GalParams$auxvar$lon)
        lat.auxvar <- tclVar(.cdtData$GalParams$auxvar$lat)
        demfile.var <- tclVar(.cdtData$GalParams$auxvar$demfile)

        txt.mrg <- tklabel(frMerge, text = lang.dlg[['label']][['7']], anchor = 'e', justify = 'right')
        cb.mrg <- ttkcombobox(frMerge, values = cb.mrgMthd, textvariable = merge.method, width = largeur3)
        frMrgP <- tkframe(frMerge)
        frauxvar <- ttklabelframe(frMerge, text = lang.dlg[['label']][['8']], relief = 'groove', borderwidth = 2)

        txt.nrun <- tklabel(frMrgP, text = lang.dlg[['label']][['9']], anchor = 'w', justify = 'left')
        en.nrun <- tkentry(frMrgP, textvariable = nb.run, width = 3)
        txt.pass <- tklabel(frMrgP, text = lang.dlg[['label']][['10']], anchor = 'w', justify = 'left')
        en.pass <- tkentry(frMrgP, textvariable = pass.ratio, width = largeur4)

        tkgrid(txt.nrun, row = 0, column = 0, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
        tkgrid(en.nrun, row = 0, column = 1, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
        tkgrid(txt.pass, row = 0, column = 2, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
        tkgrid(en.pass, row = 0, column = 3, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)

        tkgrid(txt.mrg, row = 0, column = 0, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
        tkgrid(cb.mrg, row = 0, column = 1, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
        tkgrid(frMrgP, row = 1, column = 0, sticky = 'we', rowspan = 1, columnspan = 2, padx = 1, pady = 2, ipadx = 1, ipady = 1)

        helpWidget(cb.mrg, lang.dlg[['tooltip']][['16']], lang.dlg[['status']][['16']])
        helpWidget(en.nrun, lang.dlg[['tooltip']][['17']], lang.dlg[['status']][['17']])
        helpWidget(en.pass, lang.dlg[['tooltip']][['18']], lang.dlg[['status']][['18']])

        ########

        tkbind(cb.mrg, "<<ComboboxSelected>>", function(){
            mrgmethod <- val.mrgMthd[cb.mrgMthd %in% trimws(tclvalue(merge.method))]
            auxiliary.variables(mrgmethod)
        })

        ############################################

        bt.mrg.interp <- ttkbutton(frTab2, text = lang.dlg[['button']][['2']])

        helpWidget(bt.mrg.interp, lang.dlg[['tooltip']][['19']], lang.dlg[['status']][['19']])

        tkconfigure(bt.mrg.interp, command = function(){
            mrgmethod <- val.mrgMthd[cb.mrgMthd %in% trimws(tclvalue(merge.method))]
            stateMethod <- if(mrgmethod %in% c("CSc", "BSc")) "disabled" else "normal"

            tcl('wm', 'attributes', tt, topmost = FALSE)
            .cdtData$GalParams[["interp"]] <- getInterpolationPars1(tt, .cdtData$GalParams[["interp"]], stateMethod)
            tcl('wm', 'attributes', tt, topmost = TRUE)
        })

        ############################################

        bt.grid.interp <- ttkbutton(frTab2, text = lang.dlg[['button']][['3']])

        helpWidget(bt.grid.interp, lang.dlg[['tooltip']][['20']], lang.dlg[['status']][['20']])

        tkconfigure(bt.grid.interp, command = function(){
            tcl('wm', 'attributes', tt, topmost = FALSE)
            .cdtData$GalParams[["grid"]] <- createGridInterpolation(tt, .cdtData$GalParams[["grid"]])
            tcl('wm', 'attributes', tt, topmost = TRUE)
        })

        ############################################

        frBlank <- tkframe(frTab2, relief = 'sunken', borderwidth = 2)

        blank.data <- tclVar(.cdtData$GalParams$blank$data)
        blank.shpf <- tclVar(.cdtData$GalParams$blank$shpf)

        stateSHP <- if(.cdtData$GalParams$blank$data) "normal" else "disabled"

        chk.blank <- tkcheckbutton(frBlank, variable = blank.data, text = lang.dlg[['checkbutton']][['6']], anchor = 'w', justify = 'left')
        bt.blankOpt <- ttkbutton(frBlank, text = .cdtEnv$tcl$lang$global[['button']][['4']], state = stateSHP)
        cb.blank <- ttkcombobox(frBlank, values = unlist(openFile_ttkcomboList()), textvariable = blank.shpf, width = largeur1, state = stateSHP)
        addTo_all_Combobox_List(cb.blank)
        bt.blank <- tkbutton(frBlank, text = "...", state = stateSHP)

        tkgrid(chk.blank, row = 0, column = 0, sticky = 'w', rowspan = 1, columnspan = 5, padx = 1, pady = 1, ipadx = 1, ipady = 1)
        tkgrid(bt.blankOpt, row = 0, column = 5, sticky = 'we', rowspan = 1, columnspan = 3, padx = 1, pady = 1, ipadx = 1, ipady = 1)
        tkgrid(cb.blank, row = 1, column = 0, sticky = 'we', rowspan = 1, columnspan = 7, padx = 0, pady = 1, ipadx = 1, ipady = 1)
        tkgrid(bt.blank, row = 1, column = 7, sticky = 'we', rowspan = 1, columnspan = 1, padx = 0, pady = 1, ipadx = 1, ipady = 1)

        helpWidget(cb.blank, lang.dlg[['tooltip']][['21']], lang.dlg[['status']][['21']])
        helpWidget(bt.blank, lang.dlg[['tooltip']][['4']], lang.dlg[['status']][['4']])

        ########

        tkconfigure(bt.blank, command = function(){
            tcl('wm', 'attributes', tt, topmost = FALSE)
            shp.opfiles <- getOpenShp(tt)
            tcl('wm', 'attributes', tt, topmost = TRUE)
            if(!is.null(shp.opfiles)){
                update.OpenFiles('shp', shp.opfiles)
                tclvalue(blank.shpf) <- shp.opfiles[[1]]
            }
        })

        tkbind(chk.blank, "<Button-1>", function(){
            stateSHP <- if(tclvalue(blank.data) == "1") "disabled" else "normal"
            tkconfigure(cb.blank, state = stateSHP)
            tkconfigure(bt.blank, state = stateSHP)
            tkconfigure(bt.blankOpt, state = stateSHP)
        })

        tkconfigure(bt.blankOpt, command = function(){
            blankNcdf_Options(tt)
        })

        ############################################

        if(.cdtData$GalParams$action == "merge.rain"){
            frRnoR <- tkframe(frTab2, relief = 'sunken', borderwidth = 2)

            rnor.mask <- tclVar(.cdtData$GalParams$RnoR$use)
            rnor.wet <- tclVar(.cdtData$GalParams$RnoR$wet)
            rnor.smooth <- tclVar(.cdtData$GalParams$RnoR$smooth)

            stateRnoR <- if(.cdtData$GalParams$RnoR$use) 'normal' else 'disabled'

            chk.rnor.mask <- tkcheckbutton(frRnoR, variable = rnor.mask, text = lang.dlg[['checkbutton']][['7']], anchor = 'w', justify = 'left')
            txt.rnor.wet <- tklabel(frRnoR, text = lang.dlg[['label']][['11']], anchor = 'e', justify = 'right')
            en.rnor.wet <- tkentry(frRnoR, textvariable = rnor.wet, width = 4, state = stateRnoR)
            chk.rnor.smooth <- tkcheckbutton(frRnoR, variable = rnor.smooth, text = lang.dlg[['checkbutton']][['8']], anchor = 'w', justify = 'left', state = stateRnoR)

            tkgrid(chk.rnor.mask, row = 0, column = 0, sticky = 'we', rowspan = 1, columnspan = 3, padx = 1, pady = 1, ipadx = 1, ipady = 1)
            tkgrid(txt.rnor.wet, row = 0, column = 3, sticky = 'e', rowspan = 1, columnspan = 2, padx = 1, pady = 1, ipadx = 1, ipady = 1)
            tkgrid(en.rnor.wet, row = 0, column = 5, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
            tkgrid(chk.rnor.smooth, row = 1, column = 0, sticky = 'we', rowspan = 1, columnspan = 3, padx = 1, pady = 1, ipadx = 1, ipady = 1)

            tkbind(chk.rnor.mask, "<Button-1>", function(){
                stateRnoR <- if(tclvalue(rnor.mask) == '0') 'normal' else 'disabled'
                tkconfigure(en.rnor.wet, state = stateRnoR)
                tkconfigure(chk.rnor.smooth, state = stateRnoR)
            })
        }

        if(.cdtData$GalParams$action == "merge.pres"){
            frPRMSL <- tkframe(frTab2, relief = 'sunken', borderwidth = 2)

            prmsl <- tclVar(.cdtData$GalParams$prmsl)

            chk.prmsl <- tkcheckbutton(frPRMSL, variable = prmsl, text = lang.dlg[['checkbutton']][['9']], anchor = 'w', justify = 'left')

            tkgrid(chk.prmsl, row = 0, column = 0, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
        }

        ############################################
        tkgrid(frMerge, row = 0, column = 0, sticky = 'we', padx = 1, pady = 1, ipadx = 1, ipady = 1)
        tkgrid(bt.mrg.interp, row = 1, column = 0, sticky = 'we', padx = 1, pady = 1, ipadx = 1, ipady = 1)
        tkgrid(bt.grid.interp, row = 2, column = 0, sticky = 'we', padx = 1, pady = 1, ipadx = 1, ipady = 1)
        tkgrid(frBlank, row = 3, column = 0, sticky = 'we', padx = 1, pady = 1, ipadx = 1, ipady = 1)
        if(.cdtData$GalParams$action == "merge.rain")
            tkgrid(frRnoR, row = 4, column = 0, sticky = 'we', padx = 1, pady = 1, ipadx = 1, ipady = 1)
        if(.cdtData$GalParams$action == "merge.pres")
            tkgrid(frPRMSL, row = 4, column = 0, sticky = 'we', padx = 1, pady = 1, ipadx = 1, ipady = 1)

        ####################################

        tkgrid(frTab2, padx = 0, pady = 1, ipadx = 1, ipady = 1)

    ############################################

    bt.prm.Opt <- ttkbutton(frMRG1, text = .cdtEnv$tcl$lang$global[['button']][['4']])
    bt.prm.OK <- ttkbutton(frMRG1, text = .cdtEnv$tcl$lang$global[['button']][['1']])
    bt.prm.CA <- ttkbutton(frMRG1, text = .cdtEnv$tcl$lang$global[['button']][['2']])

    #######

    tkconfigure(bt.prm.Opt, command = function(){
        variable <- gsub("merge\\.", "", .cdtData$GalParams$action)
        if(variable == "pres"){
            if(tclvalue(prmsl) == '1') variable <- "prmsl"
        }
        mergingData_Options(tt, 'merge', variable)
    })

    tkconfigure(bt.prm.OK, command = function(){
        if(trimws(tclvalue(file.stnfl)) == ""){
            cdt.tkmessageBox(tt, message = lang.dlg[['message']][['1']], icon = "warning", type = "ok")
            tkwait.window(tt)
        }else if(trimws(tclvalue(dir.InNCDF)) %in% c("", "NA")){
            cdt.tkmessageBox(tt, message = lang.dlg[['message']][['2']], icon = "warning", type = "ok")
            tkwait.window(tt)
        }else if(trimws(tclvalue(dir2save)) %in% c("", "NA")){
            cdt.tkmessageBox(tt, message = lang.dlg[['message']][['3']], icon = "warning", type = "ok")
            tkwait.window(tt)
        }else if(is.null(settingSNC)){
            cdt.tkmessageBox(tt, message = lang.dlg[['message']][['4']], icon = "warning", type = "ok")
            tkwait.window(tt)
        }else if(tclvalue(blank.data) == "1" & trimws(tclvalue(blank.shpf)) == ""){
            cdt.tkmessageBox(tt, message = lang.dlg[['message']][['5']], icon = "warning", type = "ok")
            tkwait.window(tt)
        }else{
            .cdtData$GalParams$period <- periodVAL[CbperiodVAL %in% trimws(tclvalue(file.period))]
            .cdtData$GalParams$minhour <- as.numeric(trimws(tclvalue(minhour.tclVar)))

            .cdtData$GalParams$STN.file <- trimws(tclvalue(file.stnfl))
            .cdtData$GalParams$INPUT$dir <- trimws(tclvalue(dir.InNCDF))
            .cdtData$GalParams$output$dir <- trimws(tclvalue(dir2save))

            len_format <- switch(.cdtData$GalParams$period,
                                 'minute' = 5, 'hourly' = 4,
                                 'daily' = 3, 'pentad' = 3,
                                 'dekadal' = 3, 'monthly' = 2)

            out_format <- gregexpr('%', trimws(tclvalue(outmrgff)))[[1]]
            if(length(out_format) < 2 || out_format[1] == -1){
                cdt.tkmessageBox(tt, message = lang.dlg[['message']][['20']], icon = "warning", type = "ok")
                tkwait.window(tt)
            }
            if(length(out_format) != len_format){
                msg <- paste0(lang.dlg[['message']][['20']], '.',
                              lang.dlg[['message']][['21']], ': ',
                              len_format)
                cdt.tkmessageBox(tt, message = msg, icon = "warning", type = "ok")
                tkwait.window(tt)
            }
            .cdtData$GalParams$output$format <- trimws(tclvalue(outmrgff))

            .cdtData$GalParams$MRG$method  <- val.mrgMthd[cb.mrgMthd %in% trimws(tclvalue(merge.method))]
            .cdtData$GalParams$MRG$nrun <- as.numeric(trimws(tclvalue(nb.run)))
             pass <- trimws(strsplit(tclvalue(pass.ratio), ",")[[1]])
            .cdtData$GalParams$MRG$pass <- as.numeric(pass[pass != ""])
            if(.cdtData$GalParams$MRG$nrun != length(.cdtData$GalParams$MRG$pass)){
                cdt.tkmessageBox(tt, message = lang.dlg[['message']][['6']], icon = "warning", type = "ok")
                tkwait.window(tt)
            }

            .cdtData$GalParams$auxvar$dem <- switch(tclvalue(dem.auxvar), '0' = FALSE, '1' = TRUE)
            .cdtData$GalParams$auxvar$slope <- switch(tclvalue(slope.auxvar), '0' = FALSE, '1' = TRUE)
            .cdtData$GalParams$auxvar$aspect <- switch(tclvalue(aspect.auxvar), '0' = FALSE, '1' = TRUE)
            .cdtData$GalParams$auxvar$lon <- switch(tclvalue(lon.auxvar), '0' = FALSE, '1' = TRUE)
            .cdtData$GalParams$auxvar$lat <- switch(tclvalue(lat.auxvar), '0' = FALSE, '1' = TRUE)
            .cdtData$GalParams$auxvar$demfile <- trimws(tclvalue(demfile.var))

            if(.cdtData$GalParams$MRG$method == "RK" &
               (.cdtData$GalParams$auxvar$dem |
                .cdtData$GalParams$auxvar$slope |
                .cdtData$GalParams$auxvar$aspect) &
               .cdtData$GalParams$auxvar$demfile == ""
              )
            {
                cdt.tkmessageBox(tt, message = lang.dlg[['message']][['7']], icon = "warning", type = "ok")
                tkwait.window(tt)
            }

            .cdtData$GalParams$blank$data <- switch(tclvalue(blank.data), '0' = FALSE, '1' = TRUE)
            .cdtData$GalParams$blank$shpf <- trimws(tclvalue(blank.shpf))

            if(.cdtData$GalParams$action == "merge.rain"){
                .cdtData$GalParams$RnoR$use <- switch(tclvalue(rnor.mask), '0' = FALSE, '1' = TRUE)
                .cdtData$GalParams$RnoR$smooth <- switch(tclvalue(rnor.smooth), '0' = FALSE, '1' = TRUE)
                .cdtData$GalParams$RnoR$wet <- as.numeric(trimws(tclvalue(rnor.wet)))
            }

            if(.cdtData$GalParams$action == "merge.pres"){
                .cdtData$GalParams$prmsl <- switch(tclvalue(prmsl), '0' = FALSE, '1' = TRUE)
            }

            .cdtData$GalParams$settingSNC <- settingSNC
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

    tkgrid(bt.prm.Opt, row = 0, column = 0, sticky = 'w', padx = 5, pady = 1, ipadx = 1, ipady = 1)
    tkgrid(bt.prm.OK, row = 0, column = 1, sticky = '', padx = 5, pady = 1, ipadx = 1, ipady = 1)
    tkgrid(bt.prm.CA, row = 0, column = 2, sticky = 'e', padx = 5, pady = 1, ipadx = 1, ipady = 1)

    ############################################
    
    tkgrid(frMRG0, row = 0, column = 0, sticky = 'nswe', rowspan = 1, columnspan = 2, padx = 1, pady = 1, ipadx = 1, ipady = 1)
    tkgrid(frMRG1, row = 1, column = 1, sticky = 'se', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)

    ############################################

    ## set maximum height of tab
    auxiliary.variables("RK")

    #####
    tcl('update')
    tkgrid(bwnote, sticky = 'nwes')
    tkgrid.columnconfigure(bwnote, 0, weight = 1)
    tcl("update", "idletasks")

    ## update the form
    auxiliary.variables(.cdtData$GalParams$MRG$method)

    ############################################

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
    invisible()
}
