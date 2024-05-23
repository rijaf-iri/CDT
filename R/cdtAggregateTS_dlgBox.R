
AggregateTS_GetInfo <- function(){
    listOpenFiles <- openFile_ttkcomboList()
    if(WindowsOS()){
        largeur0 <- 56
        largeur1 <- 53
        largeur2 <- 42
        wtkcombo <- 23
    }else{
        largeur0 <- 48
        largeur1 <- 47
        largeur2 <- 40
        wtkcombo <- 22
    }

    xml.dlg <- file.path(.cdtDir$dirLocal, "languages", "cdtAggregateTS_dlgBox.xml")
    lang.dlg <- cdtLanguageParse(xml.dlg, .cdtData$Config$lang.iso)

    ###################

    tt <- tktoplevel()
    tkgrab.set(tt)
    tkfocus(tt)

    frMRG0 <- tkframe(tt, relief = 'raised', borderwidth = 2)
    frMRG1 <- tkframe(tt)
    frAGGRTS <- tkframe(frMRG0, relief = "sunken", borderwidth = 2)

    ############################################

    saveFun <- function(data.type){
        tkdestroy(frSaveIn)
        frSaveIn <<- tkframe(frSave)

        #########
        if(data.type == 'cdtstation'){
            txtSaveDir <- lang.dlg[['label']][['9']]
            isFile <- TRUE
        }else{
            txtSaveDir <- lang.dlg[['label']][['10']]
            isFile <- FALSE
        }
        fileORdir <- tclVar(txtSaveDir)

        txt.file.save <- tklabel(frSaveIn, text = tclvalue(fileORdir), textvariable = fileORdir, anchor = 'w', justify = 'left')
        en.file.save <- tkentry(frSaveIn, textvariable = file.save, width = largeur0)
        bt.file.save <- tkbutton(frSaveIn, text = "...")

        ihlpSav <- if(data.type == 'cdtstation') '10' else '11'
        helpWidget(en.file.save, lang.dlg[['tooltip']][[ihlpSav]], lang.dlg[['status']][[ihlpSav]])
        helpWidget(bt.file.save, lang.dlg[['tooltip']][['9']], lang.dlg[['status']][['9']])

        #########
        tkgrid(txt.file.save, row = 0, column = 0, sticky = 'we', rowspan = 1, columnspan = 5, padx = 1, pady = 0, ipadx = 1, ipady = 1)
        tkgrid(en.file.save, row = 1, column = 0, sticky = 'we', rowspan = 1, columnspan = 4, padx = 0, pady = 0, ipadx = 1, ipady = 1)
        tkgrid(bt.file.save, row = 1, column = 4, sticky = 'w', rowspan = 1, columnspan = 1, padx = 0, pady = 0, ipadx = 1, ipady = 1)

        #########

        tkconfigure(bt.file.save, command = function(){
            tcl('wm', 'attributes', tt, topmost = FALSE)
            fileORdir2Save(file.save, isFile = isFile)
            tcl('wm', 'attributes', tt, topmost = TRUE)
        })

        #########

        if(data.type == 'cdtnetcdf'){
            txt.ncout.frmt <- tklabel(frSaveIn, text = lang.dlg[['label']][['16']], anchor = 'w', justify = 'left')
            en.ncout.frmt <- tkentry(frSaveIn, textvariable = ncout.format, width = largeur0)

            tkgrid(txt.ncout.frmt, row = 2, column = 0, sticky = 'we', rowspan = 1, columnspan = 5, padx = 1, pady = 0, ipadx = 1, ipady = 1)
            tkgrid(en.ncout.frmt, row = 3, column = 0, sticky = 'we', rowspan = 1, columnspan = 4, padx = 0, pady = 0, ipadx = 1, ipady = 1)
        }

        if(data.type == 'cdtdataset'){
            txt.dset.name <- tklabel(frSaveIn, text = lang.dlg[['label']][['17']], anchor = 'w', justify = 'left')
            en.dset.name <- tkentry(frSaveIn, textvariable = dataset.name, width = largeur0)

            tkgrid(txt.dset.name, row = 2, column = 0, sticky = 'we', rowspan = 1, columnspan = 5, padx = 1, pady = 0, ipadx = 1, ipady = 1)
            tkgrid(en.dset.name, row = 3, column = 0, sticky = 'we', rowspan = 1, columnspan = 4, padx = 0, pady = 0, ipadx = 1, ipady = 1)
        }

        #########

        if(data.type %in% c('cdtnetcdf', 'cdtdataset')){
            bt.Varinfo <- tkbutton(frSaveIn, text = lang.dlg[['button']][['2']])

            tkgrid(bt.Varinfo, row = 4, column = 0, sticky = 'we', rowspan = 1, columnspan = 5, padx = 1, pady = 3, ipadx = 1, ipady = 1)

            tkconfigure(bt.Varinfo, command = function(){
                initVar <- .cdtData$GalParams$varinfo
                varInfo <- NULL

                if(data.type == "cdtnetcdf"){
                    inputFile <- .cdtData$GalParams$cdtnetcdf$sample
                    if(inputFile != ""){
                        varInfo <- getNCDFSampleData(inputFile)
                        varInfo <- varInfo$varinfo
                    }
                }

                if(data.type == "cdtdataset"){
                    inputFile <- trimws(tclvalue(file.stnfl))
                    if(inputFile != "" & file.exists(inputFile)){
                        varInfo <- readRDS(inputFile)
                        varInfo <- varInfo$varInfo
                    }
                }

                if(!is.null(varInfo)){
                    if(initVar$name != varInfo$name && initVar$name == "") initVar$name <- varInfo$name
                    if(initVar$units != varInfo$units && initVar$units == "") initVar$units <- varInfo$units
                    if(initVar$prec != varInfo$prec && initVar$prec == "float") initVar$prec <- varInfo$prec
                    if(initVar$longname != varInfo$longname && initVar$longname == "") initVar$longname <- varInfo$longname
                    if(initVar$missval != varInfo$missval && initVar$missval == -9999) initVar$missval <- varInfo$missval
                }

                tcl('wm', 'attributes', tt, topmost = FALSE)
                .cdtData$GalParams$varinfo <- getNetCDFvarInfo(tt, initVar)
                tcl('wm', 'attributes', tt, topmost = TRUE)
            })
        }

        #########

        tkgrid(frSaveIn)
    }

    ############################################

    seasonDispFun <- function(){
        mois <- format(ISOdate(2014, 1:12, 1), "%b")
        mon <- which(MOIS %in% trimws(tclvalue(start.mon)))
        len <- as.numeric(trimws(tclvalue(length.mon)))
        mon1 <- (mon + len - 1) %% 12
        mon1[mon1 == 0] <- 12
        paste(mois[mon], "->", mois[mon1])
    }

    setSeasonalFun <- function(out.tstep){
        tkdestroy(frameSeas)
        frameSeas <<- tkframe(frConvTS, relief = "sunken", borderwidth = 2)

        if(out.tstep %in% c('seasonal', 'roll.seas')){
            seasdef <- if(out.tstep == 'seasonal') seasonDispFun() else ""
            season.def <- tclVar(seasdef)

            #######
            state.cbSeasS <- if(out.tstep == 'seasonal') "normal" else "disabled"

            txt.seasS <- tklabel(frameSeas, text = lang.dlg[['label']][['3']], anchor = 'e', justify = 'right')
            cb.seasS <- ttkcombobox(frameSeas, values = MOIS, textvariable = start.mon, width = 10, state = state.cbSeasS, justify = 'center')
            txt.seasL <- tklabel(frameSeas, text = lang.dlg[['label']][['4']])
            cb.seasL <- ttkcombobox(frameSeas, values = 2:12, textvariable = length.mon, width = 3)
            txt.SeasD <- tklabel(frameSeas, text = tclvalue(season.def), textvariable = season.def)

            tkgrid(txt.seasS, row = 0, column = 0, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1)
            tkgrid(cb.seasS, row = 0, column = 1, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1)
            tkgrid(txt.seasL, row = 0, column = 2, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1)
            tkgrid(cb.seasL, row = 0, column = 3, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1)
            tkgrid(txt.SeasD, row = 0, column = 4, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1)

            helpWidget(cb.seasS, lang.dlg[['tooltip']][['3']], lang.dlg[['status']][['3']])
            helpWidget(cb.seasL, lang.dlg[['tooltip']][['4']], lang.dlg[['status']][['4']])

            #######

            tkgrid(frameSeas, row = 1, column = 0, sticky = '', rowspan = 1, columnspan = 2, padx = 1, pady = 2, ipady = 3)

            #######

            tkbind(cb.seasS, "<<ComboboxSelected>>", function(){
                out.tstep <- periodVAL[CbperiodVAL %in% trimws(tclvalue(ConvertData))]
                seasdef <- if(out.tstep == 'seasonal') seasonDispFun() else ""
                tclvalue(season.def) <- seasdef
            })

            ######

            tkbind(cb.seasL, "<<ComboboxSelected>>", function(){
                out.tstep <- periodVAL[CbperiodVAL %in% trimws(tclvalue(ConvertData))]
                seasdef <- if(out.tstep == 'seasonal') seasonDispFun() else ""
                tclvalue(season.def) <- seasdef
            })
        }

        tcl('update')
    }

    ############################################

    setMinHourFun <- function(dataTstepIn, dataTstepOut){
        tkdestroy(frameMinHourDay)
        frameMinHourDay <<- tkframe(frConvTS)

        ####################

        if(dataTstepIn %in% c('minute', 'hourly')){
            frameMH <- tkframe(frameMinHourDay, relief = "sunken", borderwidth = 2)

            CbminhourVAL0 <- ""
            CbminhourVAL1 <- ""

            minhour0.txtVar <- tclVar(lang.dlg[['label']][['12']])
            minhour1.txtVar <- tclVar(lang.dlg[['label']][['11']])

            inMinHr <- as.numeric(trimws(tclvalue(minhour.in)))
            outMinHr <- as.numeric(trimws(tclvalue(minhour.out)))

            ###########
            state.mhO <- if(dataTstepOut %in% c('minute', 'hourly')) "normal" else "disabled"

            ## min
            if(dataTstepIn == 'minute'){
                Cbperiod1VAL <- .cdtEnv$tcl$lang$global[['combobox']][['1']][1:3]
                period1VAL <- c('minute', 'hourly', 'daily')
                tkconfigure(cb.outstep, values = Cbperiod1VAL)

                outTsVal <- c('pentad', 'dekadal', 'monthly', 'annual', 'seasonal', 'roll.seas')
                if(dataTstepOut %in% outTsVal) tclvalue(ConvertData) <- Cbperiod1VAL[1]
                dataTstepOut <- periodVAL[CbperiodVAL %in% trimws(tclvalue(ConvertData))]

                #######
                CbminhourVAL0 <- c(5, 10, 15, 30)
                tclvalue(minhour0.txtVar) <- lang.dlg[['label']][['12']]

                if(!inMinHr %in% CbminhourVAL0){
                    tclvalue(minhour.in) <- CbminhourVAL0[1]
                    inMinHr <- CbminhourVAL0[1]
                }

                ## min
                if(dataTstepOut == 'minute'){
                    if(inMinHr == 5){
                        CbminhourVAL1 <- c(10, 15, 30)
                        tclvalue(minhour1.txtVar) <- lang.dlg[['label']][['12']]
                    }
                    if(inMinHr == 10){
                        CbminhourVAL1 <- 30
                        tclvalue(minhour1.txtVar) <- lang.dlg[['label']][['12']]
                    }
                    if(inMinHr == 15){
                        CbminhourVAL1 <- 30
                        tclvalue(minhour1.txtVar) <- lang.dlg[['label']][['12']]
                    }
                    if(inMinHr == 30){
                        tclvalue(ConvertData) <- Cbperiod0VAL[2]
                        dataTstepOut <- 'hourly'
                        CbminhourVAL1 <- c(1, 3, 6, 12)
                        tclvalue(minhour1.txtVar) <- lang.dlg[['label']][['11']]
                    }
                }

                ## hour
                if(dataTstepOut == 'hourly'){
                    CbminhourVAL1 <- c(1, 3, 6, 12)
                    tclvalue(minhour1.txtVar) <- lang.dlg[['label']][['11']]
                }
            }

            ## hour
            if(dataTstepIn == 'hourly'){
                Cbperiod1VAL <- .cdtEnv$tcl$lang$global[['combobox']][['1']][2:3]
                period1VAL <- c('hourly', 'daily')
                tkconfigure(cb.outstep, values = Cbperiod1VAL)

                outTsVal <- c('minute', 'pentad', 'dekadal', 'monthly', 'annual', 'seasonal', 'roll.seas')
                if(dataTstepOut %in% outTsVal) tclvalue(ConvertData) <- Cbperiod1VAL[1]
                dataTstepOut <- periodVAL[CbperiodVAL %in% trimws(tclvalue(ConvertData))]

                #######
                CbminhourVAL0 <- c(1, 3, 6, 12)
                tclvalue(minhour0.txtVar) <- lang.dlg[['label']][['11']]
                tclvalue(minhour1.txtVar) <- lang.dlg[['label']][['11']]

                if(!inMinHr %in% CbminhourVAL0){
                    tclvalue(minhour.in) <- CbminhourVAL0[1]
                    inMinHr <- CbminhourVAL0[1]
                }

                ## hour
                if(dataTstepOut == 'hourly'){
                    if(inMinHr == 1) CbminhourVAL1 <- c(3, 6, 12)
                    if(inMinHr == 3) CbminhourVAL1 <- c(6, 12)
                    if(inMinHr == 6) CbminhourVAL1 <- 12
                    if(inMinHr == 12){
                        tclvalue(ConvertData) <- Cbperiod0VAL[3]
                        dataTstepOut <- 'daily'
                        state.mhO <- "disabled"
                    }
                }
            }

            if(!outMinHr %in% CbminhourVAL1) tclvalue(minhour.out) <- CbminhourVAL1[1]

            ###########

            cb.mhI <- ttkcombobox(frameMH, values = CbminhourVAL0, textvariable = minhour.in, width = 3)
            txt.mhI <- tklabel(frameMH, text = tclvalue(minhour0.txtVar), textvariable = minhour0.txtVar, anchor = 'w', justify = 'left')
            txt.mht <- tklabel(frameMH, text = paste("-", lang.dlg[['label']][['2']], "-"))
            cb.mhO <- ttkcombobox(frameMH, values = CbminhourVAL1, textvariable = minhour.out, width = 3, state = state.mhO)
            txt.mhO <- tklabel(frameMH, text = tclvalue(minhour1.txtVar), textvariable = minhour1.txtVar, anchor = 'w', justify = 'left')

            tkgrid(cb.mhI, row = 0, column = 0, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1)
            tkgrid(txt.mhI, row = 0, column = 1, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1)
            tkgrid(txt.mht, row = 0, column = 2, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1)
            tkgrid(cb.mhO, row = 0, column = 3, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1)
            tkgrid(txt.mhO, row = 0, column = 4, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1)

            ##########

            frameOH <- tkframe(frameMinHourDay, relief = "sunken", borderwidth = 2)

            state.shour <- "disabled"
            if(dataTstepOut == 'daily'){
                if(dataTstepIn == 'minute') state.shour <- "normal"
                if(dataTstepIn == 'hourly') state.shour <- if(inMinHr %in% c(6:12)) "disabled" else "normal"
            }

            txt.shour <- tklabel(frameOH, text = lang.dlg[['label']][['13']], anchor = 'e', justify = 'right')
            en.shour <- tkentry(frameOH, textvariable = obs.hour, width = 2, state = state.shour)

            tkgrid(txt.shour, row = 0, column = 0, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1)
            tkgrid(en.shour, row = 0, column = 1, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1)

            helpWidget(en.shour, lang.dlg[['tooltip']][['12']], lang.dlg[['status']][['12']])

            ##########

            frameAggrP <- tkframe(frameMinHourDay, relief = "sunken", borderwidth = 2)

            txt.AggP <- tklabel(frameAggrP, text = lang.dlg[['label']][['14']], anchor = 'e', justify = 'right')
            cb.AggP <- ttkcombobox(frameAggrP, values = CbAggPeriodVAL, textvariable = aggr.period, width = 7, justify = 'center')

            tkgrid(txt.AggP, row = 0, column = 0, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1)
            tkgrid(cb.AggP, row = 0, column = 1, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1)

            helpWidget(cb.AggP, lang.dlg[['tooltip']][['13']], lang.dlg[['status']][['13']])

            ##########

            frameAggrD <- tkframe(frameMinHourDay, relief = "sunken", borderwidth = 2)

            txt.AggD <- tklabel(frameAggrD, text = lang.dlg[['label']][['15']], anchor = 'e', justify = 'right')
            cb.AggD <- ttkcombobox(frameAggrD, values = CbaggDateVAL, textvariable = aggr.date, width = 7, justify = 'center')

            tkgrid(txt.AggD, row = 0, column = 0, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1)
            tkgrid(cb.AggD, row = 0, column = 1, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1)

            helpWidget(cb.AggD, lang.dlg[['tooltip']][['14']], lang.dlg[['status']][['14']])

            ##########

            tkgrid(frameMH, row = 0, column = 0, sticky = '', rowspan = 1, columnspan = 1, padx = 1, pady = 4, ipady = 3)
            tkgrid(frameOH, row = 0, column = 1, sticky = 'e', rowspan = 1, columnspan = 1, padx = 1, pady = 4, ipady = 3)
            tkgrid(frameAggrP, row = 1, column = 0, sticky = '', rowspan = 1, columnspan = 1, padx = 1, pady = 2, ipady = 3)
            tkgrid(frameAggrD, row = 1, column = 1, sticky = '', rowspan = 1, columnspan = 1, padx = 1, pady = 2, ipady = 3)

            ##########

            tkgrid(frameMinHourDay, row = 1, column = 0, sticky = '', rowspan = 1, columnspan = 2, padx = 1, pady = 2, ipady = 3)

            ##########

            tkbind(cb.outstep, "<<ComboboxSelected>>", function(){
                dataTstepIn <- period0VAL[Cbperiod0VAL %in% trimws(tclvalue(OriginData))]
                dataTstepOut <- periodVAL[CbperiodVAL %in% trimws(tclvalue(ConvertData))]
                inMinHr <- as.numeric(trimws(tclvalue(minhour.in)))
                outMinHr <- as.numeric(trimws(tclvalue(minhour.out)))

                if(dataTstepOut == 'minute'){
                    state.mhI <- "normal"
                    state.mhO <- "normal"
                    state.shour <- "disabled"
                    tclvalue(minhour1.txtVar) <- lang.dlg[['label']][['12']]
                }else if(dataTstepOut == 'hourly'){
                    state.mhI <- "normal"
                    state.mhO <- "normal"
                    state.shour <- "disabled"
                    tclvalue(minhour1.txtVar) <- lang.dlg[['label']][['11']]
                }else if(dataTstepOut == 'daily'){
                    state.mhI <- "normal"
                    state.mhO <- "disabled"
                    if(dataTstepIn == 'minute') state.shour <- "normal"
                    if(dataTstepIn == 'hourly')
                        state.shour <- if(inMinHr %in% c(6:12)) "disabled" else "normal"
                }else{
                    state.mhI <- "disabled"
                    state.mhO <- "disabled"
                    state.shour <- "disabled"
                }

                #######

                if(dataTstepIn == 'minute'){
                    if(dataTstepOut == 'minute'){
                        if(inMinHr == 5) CbminhourVAL1 <- c(10, 15, 30)
                        if(inMinHr == 10) CbminhourVAL1 <- 30
                        if(inMinHr == 15) CbminhourVAL1 <- 30
                        if(inMinHr == 30){
                            tclvalue(ConvertData) <- Cbperiod0VAL[2]
                            dataTstepOut <- 'hourly'
                            CbminhourVAL1 <- c(1, 3, 6, 12)
                            tclvalue(minhour1.txtVar) <- lang.dlg[['label']][['11']]
                        }
                    }

                    if(dataTstepOut == 'hourly'){
                        CbminhourVAL1 <- c(1, 3, 6, 12)
                        tclvalue(minhour1.txtVar) <- lang.dlg[['label']][['11']]
                    }

                    if(!outMinHr %in% CbminhourVAL1) tclvalue(minhour.out) <- CbminhourVAL1[1]
                }

                if(dataTstepIn == 'hourly'){
                    if(dataTstepOut == 'hourly'){
                        if(inMinHr == 1) CbminhourVAL1 <- c(3, 6, 12)
                        if(inMinHr == 3) CbminhourVAL1 <- c(6, 12)
                        if(inMinHr == 6) CbminhourVAL1 <- 12
                        if(inMinHr == 12){
                            tclvalue(ConvertData) <- Cbperiod0VAL[3]
                            dataTstepOut <- 'daily'
                            state.mhO <- "disabled"
                        }
                    }

                    if(!outMinHr %in% CbminhourVAL1) tclvalue(minhour.out) <- CbminhourVAL1[1]
                }

                tkconfigure(cb.mhI, state = state.mhI)
                tkconfigure(cb.mhO, values = CbminhourVAL1, state = state.mhO)
                tkconfigure(en.shour, state = state.shour)
            })

            ##########

            tkbind(cb.mhI, "<<ComboboxSelected>>", function(){
                dataTstepIn <- period0VAL[Cbperiod0VAL %in% trimws(tclvalue(OriginData))]
                dataTstepOut <- periodVAL[CbperiodVAL %in% trimws(tclvalue(ConvertData))]
                inMinHr <- as.numeric(trimws(tclvalue(minhour.in)))
                outMinHr <- as.numeric(trimws(tclvalue(minhour.out)))

                if(dataTstepIn == 'minute'){
                    ## min
                    if(dataTstepOut == 'minute'){
                        if(inMinHr == 5){
                            CbminhourVAL1 <- c(10, 15, 30)
                            tclvalue(minhour1.txtVar) <- lang.dlg[['label']][['12']]
                        }
                        if(inMinHr == 10){
                            CbminhourVAL1 <- 30
                            tclvalue(minhour1.txtVar) <- lang.dlg[['label']][['12']]
                        }
                        if(inMinHr == 15){
                            CbminhourVAL1 <- 30
                            tclvalue(minhour1.txtVar) <- lang.dlg[['label']][['12']]
                        }
                        if(inMinHr == 30){
                            tclvalue(ConvertData) <- Cbperiod0VAL[2]
                            CbminhourVAL1 <- c(1, 3, 6, 12)
                            tclvalue(minhour1.txtVar) <- lang.dlg[['label']][['11']]
                        }

                        state.shour <- "disabled"
                        state.mhO <- "normal"
                    }

                    ## hour
                    if(dataTstepOut == 'hourly'){
                        CbminhourVAL1 <- c(1, 3, 6, 12)
                        tclvalue(minhour1.txtVar) <- lang.dlg[['label']][['11']]
                        state.shour <- "disabled"
                        state.mhO <- "normal"
                    }

                    if(dataTstepOut == 'daily'){
                        state.shour <- "normal"
                        state.mhO <- "disabled"
                    }
                }

                if(dataTstepIn == 'hourly'){
                    if(dataTstepOut == 'hourly'){
                        state.mhO <- "normal"

                        if(inMinHr == 1) CbminhourVAL1 <- c(3, 6, 12)
                        if(inMinHr == 3) CbminhourVAL1 <- c(6, 12)
                        if(inMinHr == 6) CbminhourVAL1 <- 12
                        if(inMinHr == 12){
                            tclvalue(ConvertData) <- Cbperiod0VAL[3]
                            state.mhO <- "disabled"
                        }

                        state.shour <- "disabled"
                    }

                    if(dataTstepOut == 'daily'){
                        state.shour <- if(inMinHr %in% c(6:12)) "disabled" else "normal"
                        state.mhO <- "disabled"
                    }
                }

                tkconfigure(cb.mhO, values = CbminhourVAL1, state = state.mhO)
                tkconfigure(en.shour, state = state.shour)

                if(!outMinHr %in% CbminhourVAL1) tclvalue(minhour.out) <- CbminhourVAL1[1]
            })
        }else{
            tkbind(cb.outstep, "<<ComboboxSelected>>", function(){
                dataTstepOut <- periodVAL[CbperiodVAL %in% trimws(tclvalue(ConvertData))]

                setSeasonalFun(dataTstepOut)
            })
        }

        tcl('update')
    }

    ############################################

    frConvTS <- ttklabelframe(frAGGRTS, text = lang.dlg[['label']][['1']], labelanchor = "nw", relief = "groove", borderwidth = 2)

    ####################

    frameTS <- tkframe(frConvTS)

    OriginData <- tclVar()
    Cbperiod0VAL <- .cdtEnv$tcl$lang$global[['combobox']][['1']][1:6]
    period0VAL <- c('minute', 'hourly', 'daily', 'pentad', 'dekadal', 'monthly')
    tclvalue(OriginData) <- Cbperiod0VAL[period0VAL %in% .cdtData$GalParams$in.tstep]

    CbperiodVAL <- .cdtEnv$tcl$lang$global[['combobox']][['1']][1:9]
    periodVAL <- c('minute', 'hourly', 'daily', 'pentad', 'dekadal', 'monthly', 'annual', 'seasonal', 'roll.seas')

    if(.cdtData$GalParams$in.tstep == 'minute'){
        Cbperiod1VAL <- CbperiodVAL[1:3]
        period1VAL <- periodVAL[1:3]
    }
    if(.cdtData$GalParams$in.tstep == 'hourly'){
        Cbperiod1VAL <- CbperiodVAL[2:3]
        period1VAL <- periodVAL[2:3]
    }
    if(.cdtData$GalParams$in.tstep == 'daily'){
        Cbperiod1VAL <- CbperiodVAL[4:9]
        period1VAL <- periodVAL[4:9]
    }
    if(.cdtData$GalParams$in.tstep == 'pentad'){
        Cbperiod1VAL <- CbperiodVAL[5:9]
        period1VAL <- periodVAL[5:9]
    }
    if(.cdtData$GalParams$in.tstep == 'dekadal'){
        Cbperiod1VAL <- CbperiodVAL[6:9]
        period1VAL <- periodVAL[6:9]
    }
    if(.cdtData$GalParams$in.tstep == 'monthly'){
        Cbperiod1VAL <- CbperiodVAL[7:9]
        period1VAL <- periodVAL[7:9]
    }

    ConvertData <- tclVar()
    tclvalue(ConvertData) <- Cbperiod1VAL[period1VAL %in% .cdtData$GalParams$out.tstep]

    cb.intstep <- ttkcombobox(frameTS, values = Cbperiod0VAL, textvariable = OriginData, width = wtkcombo, justify = 'center')
    cb.outstep <- ttkcombobox(frameTS, values = Cbperiod1VAL, textvariable = ConvertData, width = wtkcombo, justify = 'center')
    txt.convTs <- tklabel(frameTS, text = paste("-", lang.dlg[['label']][['2']], "-"))

    tkgrid(cb.intstep, row = 0, column = 0, sticky = 'we', rowspan = 1, columnspan = 8, padx = 1, pady = 1)
    tkgrid(txt.convTs, row = 0, column = 8, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1)
    tkgrid(cb.outstep, row = 0, column = 9, sticky = 'we', rowspan = 1, columnspan = 8, padx = 1, pady = 1)

    helpWidget(cb.intstep, lang.dlg[['tooltip']][['1']], lang.dlg[['status']][['1']])
    helpWidget(cb.outstep, lang.dlg[['tooltip']][['2']], lang.dlg[['status']][['2']])

    ####################

    frameMinHourDay <- tkframe(frConvTS)

    dataTstepIn <- period0VAL[Cbperiod0VAL %in% trimws(tclvalue(OriginData))]
    dataTstepOut <- periodVAL[CbperiodVAL %in% trimws(tclvalue(ConvertData))]

    ######

    mnhr1 <- .cdtData$GalParams$HourMin$int
    if(is.na(mnhr1)) mnhr1 <- ""
    mnhr2 <- .cdtData$GalParams$HourMin$out
    if(is.na(mnhr2)) mnhr2 <- ""

    minhour.in <- tclVar(mnhr1)
    minhour.out <- tclVar(mnhr2)

    obs.hour <- tclVar(.cdtData$GalParams$HourMin$obs.hour)

    ######

    aggr.period <- tclVar()
    CbAggPeriodVAL <- lang.dlg[['combobox']][['1']]
    aggPeriodVAL <- c('start', 'end')
    tclvalue(aggr.period) <- CbAggPeriodVAL[aggPeriodVAL %in% .cdtData$GalParams$aggr.period]

    ######

    aggr.date <- tclVar()
    CbaggDateVAL <- lang.dlg[['combobox']][['1']]
    aggDateVAL <- c('start', 'end')
    tclvalue(aggr.date) <- CbaggDateVAL[aggDateVAL %in% .cdtData$GalParams$aggr.date]

    #####

    if(dataTstepIn %in% c('minute', 'hourly')){
        setMinHourFun(dataTstepIn, dataTstepOut)
    }

    ####################

    frameSeas <- tkframe(frConvTS, relief = "sunken", borderwidth = 2)

    seasonalVAL <- .cdtEnv$tcl$lang$global[['combobox']][['1']][8:9]

    MOIS <- format(ISOdate(2014, 1:12, 1), "%B")
    mon_start <- as.numeric(trimws(.cdtData$GalParams$Seasonal$start.mon))
    mon_len <- as.numeric(trimws(.cdtData$GalParams$Seasonal$length.mon))

    start.mon <- tclVar(MOIS[mon_start])
    length.mon <- tclVar(mon_len)

    if(dataTstepOut %in% c('seasonal', 'roll.seas')){
        setSeasonalFun(dataTstepOut)
    }

    #####################

    tkgrid(frameTS, row = 0, column = 0, sticky = '', rowspan = 1, columnspan = 2, padx = 1, pady = 2)

    #####################

    tkbind(cb.intstep, "<<ComboboxSelected>>", function(){
        dataTstepIn <- period0VAL[Cbperiod0VAL %in% trimws(tclvalue(OriginData))]
        dataTstepOut <- periodVAL[CbperiodVAL %in% trimws(tclvalue(ConvertData))]

        ## daily
        if(dataTstepIn == 'daily'){
            Cbperiod1VAL <- .cdtEnv$tcl$lang$global[['combobox']][['1']][4:9]
            period1VAL <- c('pentad', 'dekadal', 'monthly', 'annual', 'seasonal', 'roll.seas')
            tkconfigure(cb.outstep, values = Cbperiod1VAL)

            outTsVal <- c('minute', 'hourly', 'daily')
            if(dataTstepOut %in% outTsVal) tclvalue(ConvertData) <- Cbperiod1VAL[1]
            dataTstepOut <- periodVAL[CbperiodVAL %in% trimws(tclvalue(ConvertData))]
        }

        ## pentad
        if(dataTstepIn == 'pentad'){
            Cbperiod1VAL <- .cdtEnv$tcl$lang$global[['combobox']][['1']][5:9]
            period1VAL <- c('dekadal', 'monthly', 'annual', 'seasonal', 'roll.seas')
            tkconfigure(cb.outstep, values = Cbperiod1VAL)

            outTsVal <- c('minute', 'hourly', 'daily', 'pentad')
            if(dataTstepOut %in% outTsVal) tclvalue(ConvertData) <- Cbperiod1VAL[1]
            dataTstepOut <- periodVAL[CbperiodVAL %in% trimws(tclvalue(ConvertData))]
        }

        ## dekadal
        if(dataTstepIn == 'dekadal'){
            Cbperiod1VAL <- .cdtEnv$tcl$lang$global[['combobox']][['1']][6:9]
            period1VAL <- c('monthly', 'annual', 'seasonal', 'roll.seas')
            tkconfigure(cb.outstep, values = Cbperiod1VAL)

            outTsVal <- c('minute', 'hourly', 'daily', 'pentad', 'dekadal')
            if(dataTstepOut %in% outTsVal) tclvalue(ConvertData) <- Cbperiod1VAL[1]
            dataTstepOut <- periodVAL[CbperiodVAL %in% trimws(tclvalue(ConvertData))]
        }

        ## monthly
        if(dataTstepIn == 'monthly'){
            Cbperiod1VAL <- .cdtEnv$tcl$lang$global[['combobox']][['1']][7:9]
            period1VAL <- c('annual', 'seasonal', 'roll.seas')
            tkconfigure(cb.outstep, values = Cbperiod1VAL)

            outTsVal <- c('minute', 'hourly', 'daily', 'pentad', 'dekadal', 'monthly')
            if(dataTstepOut %in% outTsVal) tclvalue(ConvertData) <- Cbperiod1VAL[1]
            dataTstepOut <- periodVAL[CbperiodVAL %in% trimws(tclvalue(ConvertData))]
        }

        if(!dataTstepOut %in% c('seasonal', 'roll.seas')){
            tkdestroy(frameSeas)
        }

        ## minute, hourly
        setMinHourFun(dataTstepIn, dataTstepOut)
    })

    ##############

    tkbind(cb.outstep, "<<ComboboxSelected>>", function(){
        dataTstepOut <- periodVAL[CbperiodVAL %in% trimws(tclvalue(ConvertData))]

        setSeasonalFun(dataTstepOut)
    })

    ###########################################

    frDataType <- ttklabelframe(frAGGRTS, text = lang.dlg[['label']][['5']], labelanchor = "nw", relief = "groove", borderwidth = 2)

    DataType <- tclVar()
    CbdatatypeVAL <- .cdtEnv$tcl$lang$global[['combobox']][['2']][1:3]
    datatypeVAL <- c('cdtstation', 'cdtdataset', 'cdtnetcdf')
    tclvalue(DataType) <- CbdatatypeVAL[datatypeVAL %in% .cdtData$GalParams$data.type]

    if(.cdtData$GalParams$data.type == 'cdtstation'){
        file.stnfl <- tclVar(.cdtData$GalParams$cdtstation)
        txtFileDir <- lang.dlg[['label']][['6']]
        stateSetData <- "disabled"
    }else if(.cdtData$GalParams$data.type == 'cdtdataset'){
        file.stnfl <- tclVar(.cdtData$GalParams$cdtdataset)
        txtFileDir <- lang.dlg[['label']][['7']]
        stateSetData <- "disabled"
    }else{
        file.stnfl <- tclVar(.cdtData$GalParams$cdtnetcdf$dir)
        txtFileDir <- lang.dlg[['label']][['8']]
        stateSetData <- "normal"
    }
    fileINdir <- tclVar(txtFileDir)

    ##############
    cb.datatype <- ttkcombobox(frDataType, values = CbdatatypeVAL, textvariable = DataType, width = largeur2, justify = 'center')
    set.datatype <- ttkbutton(frDataType, text = .cdtEnv$tcl$lang$global[['button']][['5']], state = stateSetData)

    txt.stnfl <- tklabel(frDataType, text = tclvalue(fileINdir), textvariable = fileINdir, anchor = 'w', justify = 'left')
    if(.cdtData$GalParams$data.type == 'cdtstation'){
        cb.stnfl <- ttkcombobox(frDataType, values = unlist(listOpenFiles), textvariable = file.stnfl, width = largeur1)
    }else{
        cb.stnfl <- tkentry(frDataType, textvariable = file.stnfl, width = largeur0)
    }
    bt.stnfl <- tkbutton(frDataType, text = "...")

    ##############

    tkgrid(cb.datatype, row = 0, column = 0, sticky = 'we', rowspan = 1, columnspan = 3, padx = 1, pady = 1, ipadx = 1, ipady = 1)
    tkgrid(set.datatype, row = 0, column = 3, sticky = 'we', rowspan = 1, columnspan = 2, padx = 1, pady = 1, ipadx = 1, ipady = 1)
    tkgrid(txt.stnfl, row = 1, column = 0, sticky = 'we', rowspan = 1, columnspan = 5, padx = 1, pady = 1, ipadx = 1, ipady = 1)
    tkgrid(cb.stnfl, row = 2, column = 0, sticky = 'we', rowspan = 1, columnspan = 4, padx = 0, pady = 1, ipadx = 1, ipady = 1)
    tkgrid(bt.stnfl, row = 2, column = 4, sticky = 'we', rowspan = 1, columnspan = 1, padx = 0, pady = 1, ipadx = 1, ipady = 1)

    if(.cdtData$GalParams$data.type == 'cdtstation'){
        helpWidget(cb.stnfl, lang.dlg[['tooltip']][['5']], lang.dlg[['status']][['5']])
        helpWidget(bt.stnfl, lang.dlg[['tooltip']][['8']], lang.dlg[['status']][['8']])
    }else if(.cdtData$GalParams$data.type == 'cdtdataset'){
        helpWidget(cb.stnfl, lang.dlg[['tooltip']][['6']], lang.dlg[['status']][['6']])
        helpWidget(bt.stnfl, lang.dlg[['tooltip']][['9']], lang.dlg[['status']][['9']])
    }else{
        helpWidget(cb.stnfl, lang.dlg[['tooltip']][['7']], lang.dlg[['status']][['7']])
        helpWidget(bt.stnfl, lang.dlg[['tooltip']][['9']], lang.dlg[['status']][['9']])
    }

    ##############

    settingdone <- .cdtData$GalParams$settingdone

    tkconfigure(set.datatype, command = function(){
        tcl('wm', 'attributes', tt, topmost = FALSE)
        AggregateTS_ncdfData(tt, trimws(tclvalue(file.stnfl)), tclvalue(OriginData))
        tcl('wm', 'attributes', tt, topmost = TRUE)
        settingdone <<- 1
    })

    tkconfigure(bt.stnfl, command = function(){
        if(.cdtData$GalParams$data.type == 'cdtstation'){
            tcl('wm', 'attributes', tt, topmost = FALSE)
            dat.opfiles <- getOpenFiles(tt)
            tcl('wm', 'attributes', tt, topmost = TRUE)
            if(!is.null(dat.opfiles)){
                update.OpenFiles('ascii', dat.opfiles)
                listOpenFiles[[length(listOpenFiles) + 1]] <<- dat.opfiles[[1]]
                tclvalue(file.stnfl) <- dat.opfiles[[1]]
                tkconfigure(cb.stnfl, values = unlist(listOpenFiles))
            }
        }else if(.cdtData$GalParams$data.type == 'cdtdataset'){
            tcl('wm', 'attributes', tt, topmost = FALSE)
            path.rds <- tclvalue(tkgetOpenFile(filetypes = .cdtEnv$tcl$data$filetypes6))
            tcl('wm', 'attributes', tt, topmost = TRUE)
            tclvalue(file.stnfl) <- if(path.rds %in% c("", "NA") | is.na(path.rds)) "" else path.rds
        }else{
            tcl('wm', 'attributes', tt, topmost = FALSE)
            dirnc <- tk_choose.dir(getwd(), "")
            tcl('wm', 'attributes', tt, topmost = TRUE)
            tclvalue(file.stnfl) <- if(dirnc %in% c("", "NA") | is.na(dirnc)) "" else dirnc
        }
    })

    ##############

    tkbind(cb.datatype, "<<ComboboxSelected>>", function(){
        tkdestroy(cb.stnfl)
        tclvalue(file.stnfl) <- ''

        ####
        dataType <- datatypeVAL[CbdatatypeVAL %in% trimws(tclvalue(DataType))]
 
        if(dataType == 'cdtstation'){
            tclvalue(fileINdir) <- lang.dlg[['label']][['6']]

            cb.stnfl <- ttkcombobox(frDataType, values = unlist(listOpenFiles), textvariable = file.stnfl, width = largeur1)

            #######
            tkconfigure(bt.stnfl, command = function(){
                tcl('wm', 'attributes', tt, topmost = FALSE)
                dat.opfiles <- getOpenFiles(tt)
                tcl('wm', 'attributes', tt, topmost = TRUE)
                if(!is.null(dat.opfiles)){
                    update.OpenFiles('ascii', dat.opfiles)
                    listOpenFiles[[length(listOpenFiles) + 1]] <<- dat.opfiles[[1]]
                    tclvalue(file.stnfl) <- dat.opfiles[[1]]
                    tkconfigure(cb.stnfl, values = unlist(listOpenFiles))
                }
            })

            tkconfigure(set.datatype, state = 'disabled')

            #######
            helpWidget(cb.stnfl, lang.dlg[['tooltip']][['5']], lang.dlg[['status']][['5']])
            helpWidget(bt.stnfl, lang.dlg[['tooltip']][['8']], lang.dlg[['status']][['8']])
        }

        ####
        if(dataType == 'cdtdataset'){
            tclvalue(fileINdir) <- lang.dlg[['label']][['7']]

            cb.stnfl <- tkentry(frDataType, textvariable = file.stnfl, width = largeur0)

            #######
            tkconfigure(bt.stnfl, command = function(){
                tcl('wm', 'attributes', tt, topmost = FALSE)
                path.rds <- tclvalue(tkgetOpenFile(filetypes = .cdtEnv$tcl$data$filetypes6))
                tcl('wm', 'attributes', tt, topmost = TRUE)
                tclvalue(file.stnfl) <- if(path.rds %in% c("", "NA") | is.na(path.rds)) "" else path.rds
            })

            tkconfigure(set.datatype, state = 'disabled')

            #######
            helpWidget(cb.stnfl, lang.dlg[['tooltip']][['6']], lang.dlg[['status']][['6']])
            helpWidget(bt.stnfl, lang.dlg[['tooltip']][['9']], lang.dlg[['status']][['9']])
        }

        ####
        if(dataType == 'cdtnetcdf'){
            tclvalue(fileINdir) <- lang.dlg[['label']][['8']]

            cb.stnfl <- tkentry(frDataType, textvariable = file.stnfl, width = largeur0)

            #######
            tkconfigure(bt.stnfl, command = function(){
                tcl('wm', 'attributes', tt, topmost = FALSE)
                dirnc <- tk_choose.dir(getwd(), "")
                tcl('wm', 'attributes', tt, topmost = TRUE)
                tclvalue(file.stnfl) <- if(dirnc %in% c("", "NA") | is.na(dirnc)) "" else dirnc
            })

            tkconfigure(set.datatype, command = function(){
                tcl('wm', 'attributes', tt, topmost = FALSE)
                AggregateTS_ncdfData(tt, trimws(tclvalue(file.stnfl)), tclvalue(OriginData))
                tcl('wm', 'attributes', tt, topmost = TRUE)
                settingdone <<- 1
            })

            tkconfigure(set.datatype, state = 'normal')

            #######
            helpWidget(cb.stnfl, lang.dlg[['tooltip']][['7']], lang.dlg[['status']][['7']])
            helpWidget(bt.stnfl, lang.dlg[['tooltip']][['9']], lang.dlg[['status']][['9']])
        }

        #######
        tkgrid(cb.stnfl, row = 2, column = 0, sticky = 'we', rowspan = 1, columnspan = 4, padx = 0, pady = 1, ipadx = 1, ipady = 1)

        #######

        saveFun(dataType)
        tkfocus(tt)
    })

    ############################################

    bt.aggrPars <- ttkbutton(frAGGRTS, text = lang.dlg[['button']][['1']])

    tkconfigure(bt.aggrPars, command = function(){
        tcl('wm', 'attributes', tt, topmost = FALSE)
        .cdtData$GalParams[['aggr.series']] <- getInfo_AggregateFun(tt, .cdtData$GalParams[['aggr.series']])
        tcl('wm', 'attributes', tt, topmost = TRUE)
    })

    ############################################

    frSave <- tkframe(frAGGRTS, relief = 'groove', borderwidth = 2)

    frSaveIn <- tkframe(frSave)

    file.save <- tclVar(.cdtData$GalParams$output)
    ncout.format <- tclVar(.cdtData$GalParams$ncout.format)
    dataset.name <- tclVar(.cdtData$GalParams$dataset.name)

    saveFun(.cdtData$GalParams$data.type)

    ############################################
    tkgrid(frConvTS, row = 0, column = 0, sticky = 'we', padx = 1, pady = 1, ipadx = 1, ipady = 1)
    tkgrid(frDataType, row = 1, column = 0, sticky = 'we', padx = 1, pady = 1, ipadx = 1, ipady = 1)
    tkgrid(bt.aggrPars, row = 3, column = 0, sticky = 'we', padx = 1, pady = 3, ipadx = 1, ipady = 1)
    tkgrid(frSave, row = 4, column = 0, sticky = 'we', padx = 1, pady = 1, ipadx = 1, ipady = 1)

    ############################################

    tkgrid(frAGGRTS, row = 0, column = 0, sticky = 'news', padx = 5, pady = 1, ipadx = 1, ipady = 1)

    ############################################

    bt.prm.OK <- ttkbutton(frMRG1, text = .cdtEnv$tcl$lang$global[['button']][['1']])
    bt.prm.CA <- ttkbutton(frMRG1, text = .cdtEnv$tcl$lang$global[['button']][['2']])

    ####
    tkconfigure(bt.prm.OK, command = function(){
        if(trimws(tclvalue(file.stnfl)) == ""){
            cdt.tkmessageBox(tt, message = lang.dlg[['message']][['1']], icon = "warning", type = "ok")
            tkwait.window(tt)
        }else if(trimws(tclvalue(file.save)) %in% c("", "NA")){
            cdt.tkmessageBox(tt, message = lang.dlg[['message']][['2']], icon = "warning", type = "ok")
            tkwait.window(tt)
        }else if(trimws(tclvalue(DataType)) == CbdatatypeVAL[3] & is.null(settingdone)){
            cdt.tkmessageBox(tt, message = lang.dlg[['message']][['3']], icon = "warning", type = "ok")
            tkwait.window(tt)
        }else{
            .cdtData$GalParams$in.tstep <- period0VAL[Cbperiod0VAL %in% trimws(tclvalue(OriginData))]
            .cdtData$GalParams$out.tstep <- periodVAL[CbperiodVAL %in% trimws(tclvalue(ConvertData))]

            .cdtData$GalParams$HourMin$int <- as.numeric(trimws(tclvalue(minhour.in)))
            .cdtData$GalParams$HourMin$out <- as.numeric(trimws(tclvalue(minhour.out)))
            .cdtData$GalParams$HourMin$obs.hour <- as.numeric(trimws(tclvalue(obs.hour)))

            .cdtData$GalParams$aggr.date <- aggDateVAL[CbaggDateVAL %in% trimws(tclvalue(aggr.date))]
            .cdtData$GalParams$aggr.period <- aggPeriodVAL[CbAggPeriodVAL %in% trimws(tclvalue(aggr.period))]

            .cdtData$GalParams$Seasonal$start.mon <- which(MOIS %in% trimws(tclvalue(start.mon)))
            .cdtData$GalParams$Seasonal$length.mon <- as.numeric(trimws(tclvalue(length.mon)))

            .cdtData$GalParams$data.type <- datatypeVAL[CbdatatypeVAL %in% trimws(tclvalue(DataType))]

            if(.cdtData$GalParams$data.type == 'cdtstation')
                .cdtData$GalParams$cdtstation <- trimws(tclvalue(file.stnfl))
            if(.cdtData$GalParams$data.type == 'cdtdataset'){
                .cdtData$GalParams$cdtdataset <- trimws(tclvalue(file.stnfl))
                .cdtData$GalParams$dataset.name <- trimws(tclvalue(dataset.name))
                if(.cdtData$GalParams$dataset.name == ""){
                    cdt.tkmessageBox(tt, message = lang.dlg[['message']][['15']], icon = "warning", type = "ok")
                    tkwait.window(tt)
                }
            }
            if(.cdtData$GalParams$data.type == 'cdtnetcdf'){
                .cdtData$GalParams$cdtnetcdf$dir <- trimws(tclvalue(file.stnfl))
                .cdtData$GalParams$ncout.format <- trimws(tclvalue(ncout.format))
            }

            .cdtData$GalParams$output <- trimws(tclvalue(file.save))

            .cdtData$GalParams$settingdone <- settingdone
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
