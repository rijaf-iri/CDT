
climatologiesCalcPanelCmd <- function(){
    listOpenFiles <- openFile_ttkcomboList()
    if(WindowsOS()){
        largeur0 <- .cdtEnv$tcl$fun$w.widgets(22)
        largeur1 <-.cdtEnv$tcl$fun$w.widgets(31)
        largeur2 <- .cdtEnv$tcl$fun$w.widgets(33)
        largeur3 <- 32
        largeur4 <- 24
        largeur5 <- 25
        largeur6 <- 11
        largeur7 <- 10
        largeur8 <- 18
    }else{
        largeur0 <- .cdtEnv$tcl$fun$w.widgets(22)
        largeur1 <- .cdtEnv$tcl$fun$w.widgets(23)
        largeur2 <- .cdtEnv$tcl$fun$w.widgets(24)
        largeur3 <- 24
        largeur4 <- 16
        largeur5 <- 17
        largeur6 <- 11
        largeur7 <- 10
        largeur8 <- 18
    }

    GeneralParameters <- list(intstep = "dekadal", outstep = "dekadal", data.type = "cdtstation", 
                              seasonal = list(start.mon = 1, length.mon = 3),
                              cdtstation = list(file = ""),
                              cdtdataset = list(index = ""),
                              cdtnetcdf = list(dir = "", sample = "", format = "rfe_%s%s%s.nc"),
                              aggr.series = list(aggr.fun = "sum", opr.fun = ">=", opr.thres = 1,
                                                 min.frac = list(unique = TRUE, all = 0.95, month = rep(0.95, 12))),
                              climato = list(all.years = TRUE, start.year = 1981, end.year = 2010,
                                             min.year = 20, window = 0),
                              out.dir = "")

    .cdtData$EnvData$tab$pointSize <- NULL
    .cdtData$EnvData$climMapOp <- list(presetCol = list(color = 'tim.colors', reverse = FALSE),
                                       userCol = list(custom = FALSE, color = NULL),
                                       userLvl = list(custom = FALSE, levels = NULL, equidist = FALSE),
                                       title = list(user = FALSE, title = ''),
                                       colkeyLab = list(user = FALSE, label = ''),
                                       scalebar = list(add = FALSE, pos = 'bottomleft'),
                                       pointSize = .cdtData$EnvData$tab$pointSize)

    .cdtData$EnvData$TSGraphOp <- list(
                                        bar = list(
                                                xlim = list(is.min = FALSE, min = "1-1", is.max = FALSE, max = "12-3"),
                                                ylim = list(is.min = FALSE, min = 0, is.max = FALSE, max = 100),
                                                axislabs = list(is.xlab = FALSE, xlab = '', is.ylab = FALSE, ylab = ''),
                                                title = list(is.title = FALSE, title = '', position = 'top'),
                                                colors = list(col = "darkblue")
                                               ),
                                        line = list(
                                                xlim = list(is.min = FALSE, min = "1-1", is.max = FALSE, max = "12-3"),
                                                ylim = list(is.min = FALSE, min = 0, is.max = FALSE, max = 100),
                                                axislabs = list(is.xlab = FALSE, xlab = '', is.ylab = FALSE, ylab = ''),
                                                title = list(is.title = FALSE, title = '', position = 'top'),
                                                plot = list(type = 'both',
                                                            col = list(line = "red", points = "blue"),
                                                            lwd = 2, cex = 1.4),
                                                legend = NULL)
                                        )

    .cdtData$EnvData$SHPOp <- list(col = "black", lwd = 1.5)

    ###################

    xml.dlg <- file.path(.cdtDir$dirLocal, "languages", "cdtCompute_Climatologies_leftCmd.xml")
    lang.dlg <- cdtLanguageParse(xml.dlg, .cdtData$Config$lang.iso)
    .cdtData$EnvData$message <- lang.dlg[['message']]

    ###################

    .cdtEnv$tcl$main$cmd.frame <- tkframe(.cdtEnv$tcl$main$panel.left)

    tknote.cmd <- bwNoteBook(.cdtEnv$tcl$main$cmd.frame)
    cmd.tab1 <- bwAddTab(tknote.cmd, text = lang.dlg[['tab_title']][['1']])
    cmd.tab2 <- bwAddTab(tknote.cmd, text = lang.dlg[['tab_title']][['2']])
    cmd.tab3 <- bwAddTab(tknote.cmd, text = lang.dlg[['tab_title']][['3']])
    cmd.tab4 <- bwAddTab(tknote.cmd, text = lang.dlg[['tab_title']][['4']])

    bwRaiseTab(tknote.cmd, cmd.tab1)

    tkgrid.columnconfigure(cmd.tab1, 0, weight = 1)
    tkgrid.columnconfigure(cmd.tab2, 0, weight = 1)
    tkgrid.columnconfigure(cmd.tab3, 0, weight = 1)
    tkgrid.columnconfigure(cmd.tab4, 0, weight = 1)

    tkgrid.rowconfigure(cmd.tab1, 0, weight = 1)
    tkgrid.rowconfigure(cmd.tab2, 0, weight = 1)
    tkgrid.rowconfigure(cmd.tab3, 0, weight = 1)
    tkgrid.rowconfigure(cmd.tab4, 0, weight = 1)

    #######################################################################################################

    #Tab1
    subfr1 <- bwTabScrollableFrame(cmd.tab1)

        #######################

        frameTimeS <- ttklabelframe(subfr1, text = lang.dlg[['label']][['1']], relief = 'groove')

        timeSteps <- tclVar()
        CbperiodVAL <- .cdtEnv$tcl$lang$global[['combobox']][['1']][3:6]
        periodVAL <- c('daily', 'pentad', 'dekadal', 'monthly')
        tclvalue(timeSteps) <- CbperiodVAL[periodVAL %in% GeneralParameters$intstep]

        cb.fperiod <- ttkcombobox(frameTimeS, values = CbperiodVAL, textvariable = timeSteps, width = largeur5)

        tkgrid(cb.fperiod, row = 0, column = 0, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)

        helpWidget(cb.fperiod, lang.dlg[['tooltip']][['1']], lang.dlg[['status']][['1']])

        ############

        tkbind(cb.fperiod, "<<ComboboxSelected>>", function(){
            instep <- periodVAL[CbperiodVAL %in% str_trim(tclvalue(timeSteps))]

            if(instep == 'daily'){
                CbperiodVAL1 <<- CbOutVAL
                periodVAL1 <<- OutVAL
            }
            if(instep == 'pentad'){
                CbperiodVAL1 <<- CbOutVAL[2:6]
                periodVAL1 <<- OutVAL[2:6]
            }
            if(instep == 'dekadal'){
                CbperiodVAL1 <<- CbOutVAL[3:6]
                periodVAL1 <<- OutVAL[3:6]
            }
            if(instep == 'monthly'){
                CbperiodVAL1 <<- CbOutVAL[4:6]
                periodVAL1 <<- OutVAL[4:6]
            }

            tkconfigure(cb.outclim, values = CbperiodVAL1)
            if(!str_trim(tclvalue(outSteps)) %in% CbperiodVAL1)
                tclvalue(outSteps) <- CbperiodVAL1[1]

            stateAggr <- if(str_trim(tclvalue(timeSteps)) == str_trim(tclvalue(outSteps))) "disabled" else "normal"
            tkconfigure(bt.AggrFun, state = stateAggr)

            statedayW <- if(str_trim(tclvalue(outSteps)) == CbOutVAL[1]) "normal" else "disabled"
            tkconfigure(en.daywin, state = statedayW)
        })

        #############################

        frameOutS <- ttklabelframe(subfr1, text = lang.dlg[['label']][['1a']], relief = 'groove')

        CbOutVAL <- .cdtEnv$tcl$lang$global[['combobox']][['1']][3:8]
        OutVAL <- c('daily', 'pentad', 'dekadal', 'monthly', 'annual', 'seasonal')

        if(GeneralParameters$intstep == 'daily'){
            CbperiodVAL1 <- CbOutVAL
            periodVAL1 <- OutVAL
        }
        if(GeneralParameters$intstep == 'pentad'){
            CbperiodVAL1 <- CbOutVAL[2:6]
            periodVAL1 <- OutVAL[2:6]
        }
        if(GeneralParameters$intstep == 'dekadal'){
            CbperiodVAL1 <- CbOutVAL[3:6]
            periodVAL1 <- OutVAL[3:6]
        }
        if(GeneralParameters$intstep == 'monthly'){
            CbperiodVAL1 <- CbOutVAL[4:6]
            periodVAL1 <- OutVAL[4:6]
        }

        outSteps <- tclVar()
        tclvalue(outSteps) <- CbperiodVAL1[periodVAL1 %in% GeneralParameters$outstep]

        cb.outclim <- ttkcombobox(frameOutS, values = CbperiodVAL1, textvariable = outSteps, width = largeur5)

        tkgrid(cb.outclim, row = 0, column = 0, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)

        helpWidget(cb.outclim, lang.dlg[['tooltip']][['1a']], lang.dlg[['status']][['1a']])

        ############

        tkbind(cb.outclim, "<<ComboboxSelected>>", function(){
            statedayW <- if(str_trim(tclvalue(outSteps)) == CbOutVAL[1]) "normal" else "disabled"
            tkconfigure(en.daywin, state = statedayW)

            stateSeas <- if(str_trim(tclvalue(outSteps)) == CbOutVAL[6]) "normal" else "disabled"
            tkconfigure(cb.seasS, state = stateSeas)
            tkconfigure(cb.seasL, state = stateSeas)

            stateAggr <- if(str_trim(tclvalue(timeSteps)) == str_trim(tclvalue(outSteps))) "disabled" else "normal"
            tkconfigure(bt.AggrFun, state = stateAggr)

            stateMaps <- if(str_trim(tclvalue(outSteps)) %in% CbOutVAL[5:6]) 'disabled' else 'normal'
            tkconfigure(cb.clim.Date, state = stateMaps)
            tkconfigure(bt.clim.Date.prev, state = stateMaps)
            tkconfigure(bt.clim.Date.next, state = stateMaps)

            stateGraphs <- if(str_trim(tclvalue(outSteps)) %in% CbOutVAL[5:6]) 'disabled' else 'normal'
            tkconfigure(cb.typeTSp, state = stateGraphs)
            tkconfigure(bt.TsGraph.plot, state = stateGraphs)
            tkconfigure(bt.TSGraphOpt, state = stateGraphs)
        })

        #############################

        frameSeas <- tkframe(subfr1, relief = 'groove', borderwidth = 2)

        MOIS <- format(ISOdate(2014, 1:12, 1), "%B")
        mois <- format(ISOdate(2014, 1:12, 1), "%b")
        mon <- as.numeric(str_trim(GeneralParameters$seasonal$start.mon))
        len <- as.numeric(str_trim(GeneralParameters$seasonal$length.mon))
        mon1 <- (mon + len - 1) %% 12
        mon1[mon1 == 0] <- 12
        seasdef <- paste(mois[mon], "->", mois[mon1])

        start.mon <- tclVar(MOIS[mon])
        length.mon <- tclVar(len)
        season.def <- tclVar(seasdef)

        stateSeas <- if(GeneralParameters$outstep == 'seasonal') "normal" else "disabled"

        txt.seasS <- tklabel(frameSeas, text = lang.dlg[['label']][['23']], anchor = 'e', justify = 'right')
        cb.seasS <- ttkcombobox(frameSeas, values = MOIS, textvariable = start.mon, width = 9, state = stateSeas)
        txt.seasL <- tklabel(frameSeas, text = lang.dlg[['label']][['24']])
        cb.seasL <- ttkcombobox(frameSeas, values = 2:12, textvariable = length.mon, width = 2, state = stateSeas)
        txt.SeasD <- tklabel(frameSeas, text = tclvalue(season.def), textvariable = season.def)

        tkgrid(txt.seasS, row = 0, column = 0, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1)
        tkgrid(cb.seasS, row = 0, column = 1, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1)
        tkgrid(txt.seasL, row = 0, column = 2, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1)
        tkgrid(cb.seasL, row = 0, column = 3, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1)
        tkgrid(txt.SeasD, row = 0, column = 4, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1)

        helpWidget(cb.seasS, lang.dlg[['tooltip']][['13']], lang.dlg[['status']][['13']])
        helpWidget(cb.seasL, lang.dlg[['tooltip']][['14']], lang.dlg[['status']][['14']])

        ##############

        tkbind(cb.seasS, "<<ComboboxSelected>>", function(){
            seasdef <- ""
            if(str_trim(tclvalue(outSteps)) == CbOutVAL[6]){
                mon <-  which(MOIS %in% str_trim(tclvalue(start.mon)))
                len <- as.numeric(str_trim(tclvalue(length.mon)))
                mon1 <- (mon + len - 1) %% 12
                mon1[mon1 == 0] <- 12
                seasdef <- paste(mois[mon], "->", mois[mon1])
            }
            tclvalue(season.def) <- seasdef
        })

        ##############

        tkbind(cb.seasL, "<<ComboboxSelected>>", function(){
            seasdef <- ""
            if(str_trim(tclvalue(outSteps)) == CbOutVAL[6]){
                mon <-  which(MOIS %in% str_trim(tclvalue(start.mon)))
                len <- as.numeric(str_trim(tclvalue(length.mon)))
                mon1 <- (mon + len - 1) %% 12
                mon1[mon1 == 0] <- 12
                seasdef <- paste(mois[mon], "->", mois[mon1])
            }
            tclvalue(season.def) <- seasdef
        })

        #######################

        frameInData <- ttklabelframe(subfr1, text = lang.dlg[['label']][['2']], relief = 'groove')

        DataType <- tclVar()
        CbdatatypeVAL <- .cdtEnv$tcl$lang$global[['combobox']][['2']][1:3]
        datatypeVAL <- c('cdtstation', 'cdtdataset', 'cdtnetcdf')
        tclvalue(DataType) <- CbdatatypeVAL[datatypeVAL %in% GeneralParameters$data.type]

        if(GeneralParameters$data.type == 'cdtstation'){
            input.file <- tclVar(GeneralParameters$cdtstation$file)
            txt.INData <- lang.dlg[['label']][['3']]
            stateSetNC <- "disabled"
        }else if(GeneralParameters$data.type == 'cdtdataset'){
            input.file <- tclVar(GeneralParameters$cdtdataset$index)
            txt.INData <- lang.dlg[['label']][['4']]
            stateSetNC <- "disabled"
        }else{
            input.file <- tclVar(GeneralParameters$cdtnetcdf$dir)
            txt.INData <- lang.dlg[['label']][['5']]
            stateSetNC <- "normal"
        }
        txt.INData.var <- tclVar(txt.INData)

        txt.datatype <- tklabel(frameInData, text = lang.dlg[['label']][['6']], anchor = 'w', justify = 'left')
        cb.datatype <- ttkcombobox(frameInData, values = CbdatatypeVAL, textvariable = DataType, width = largeur0)
        txt.infile <- tklabel(frameInData, text = tclvalue(txt.INData.var), textvariable = txt.INData.var, anchor = 'w', justify = 'left')
        set.infile <- tkbutton(frameInData, text = .cdtEnv$tcl$lang$global[['button']][['5']], width = 8, state = stateSetNC)
        if(GeneralParameters$data.type == 'cdtstation'){
            cb.en.infile <- ttkcombobox(frameInData, values = unlist(listOpenFiles), textvariable = input.file, width = largeur1)
        }else{
            cb.en.infile <- tkentry(frameInData, textvariable = input.file, width = largeur2)
        }
        bt.infile <- tkbutton(frameInData, text = "...")

        tkgrid(txt.datatype, row = 0, column = 0, sticky = 'we', rowspan = 1, columnspan = 2, padx = 1, pady = 1, ipadx = 1, ipady = 1)
        tkgrid(cb.datatype, row = 0, column = 2, sticky = 'we', rowspan = 1, columnspan = 8, padx = 1, pady = 1, ipadx = 1, ipady = 1)
        tkgrid(txt.infile, row = 1, column = 0, sticky = 'we', rowspan = 1, columnspan = 8, padx = 1, pady = 1, ipadx = 1, ipady = 1)
        tkgrid(set.infile, row = 1, column = 8, sticky = 'we', rowspan = 1, columnspan = 2, padx = 1, pady = 1, ipadx = 1, ipady = 1)
        tkgrid(cb.en.infile, row = 2, column = 0, sticky = 'we', rowspan = 1, columnspan = 9, padx = 0, pady = 1, ipadx = 1, ipady = 1)
        tkgrid(bt.infile, row = 2, column = 9, sticky = 'w', rowspan = 1, columnspan = 1, padx = 0, pady = 1, ipadx = 1, ipady = 1)

        ############
        helpWidget(cb.datatype, lang.dlg[['tooltip']][['2']], lang.dlg[['status']][['2']])

        if(GeneralParameters$data.type == 'cdtstation'){
            helpWidget(cb.en.infile, lang.dlg[['tooltip']][['3']], lang.dlg[['status']][['3']])
            helpWidget(bt.infile, lang.dlg[['tooltip']][['6']], lang.dlg[['status']][['6']])
        }else if(GeneralParameters$data.type == 'cdtdataset'){
            helpWidget(cb.en.infile, lang.dlg[['tooltip']][['4']], lang.dlg[['status']][['4']])
            helpWidget(bt.infile, lang.dlg[['tooltip']][['7']], lang.dlg[['status']][['7']])
        }else{
            helpWidget(cb.en.infile, lang.dlg[['tooltip']][['5']], lang.dlg[['status']][['5']])
            helpWidget(bt.infile, lang.dlg[['tooltip']][['7']], lang.dlg[['status']][['7']])
        }

        ############
        settingINData <- NULL
        tkconfigure(set.infile, command = function(){
            GeneralParameters$cdtnetcdf <<- getInfoNetcdfData(.cdtEnv$tcl$main$win, GeneralParameters$cdtnetcdf,
                                                            str_trim(tclvalue(input.file)), tclvalue(timeSteps))
            settingINData <<- 1
        })

        tkconfigure(bt.infile, command = function(){
            if(GeneralParameters$data.type == 'cdtstation'){
                dat.opfiles <- getOpenFiles(.cdtEnv$tcl$main$win)
                if(!is.null(dat.opfiles)){
                    update.OpenFiles('ascii', dat.opfiles)
                    listOpenFiles[[length(listOpenFiles) + 1]] <<- dat.opfiles[[1]]
                    tclvalue(input.file) <- dat.opfiles[[1]]
                    tkconfigure(cb.en.infile, values = unlist(listOpenFiles))
                }
            }else if(GeneralParameters$data.type == 'cdtdataset'){
                path.rds <- tclvalue(tkgetOpenFile(initialdir = getwd(), filetypes = .cdtEnv$tcl$data$filetypes6))
                tclvalue(input.file) <- if(path.rds %in% c("", "NA") | is.na(path.rds)) "" else path.rds
            }else{
                dirnc <- tk_choose.dir(getwd(), "")
                tclvalue(input.file) <- if(dirnc %in% c("", "NA") | is.na(dirnc)) "" else dirnc
            }
        })

        ############

        tkbind(cb.datatype, "<<ComboboxSelected>>", function(){
            tkdestroy(cb.en.infile)
            tclvalue(input.file) <- ''

            ###
            stateSetNC <- if(str_trim(tclvalue(DataType)) == CbdatatypeVAL[3]) "normal" else "disabled"
            tkconfigure(set.infile, state = stateSetNC)

            ###
            if(str_trim(tclvalue(DataType)) == CbdatatypeVAL[1]){
                tclvalue(txt.INData.var) <- lang.dlg[['label']][['3']]

                cb.en.infile <<- ttkcombobox(frameInData, values = unlist(listOpenFiles), textvariable = input.file, width = largeur1)

                tkconfigure(bt.infile, command = function(){
                    dat.opfiles <- getOpenFiles(.cdtEnv$tcl$main$win)
                    if(!is.null(dat.opfiles)){
                        update.OpenFiles('ascii', dat.opfiles)
                        listOpenFiles[[length(listOpenFiles) + 1]] <<- dat.opfiles[[1]]
                        tclvalue(input.file) <- dat.opfiles[[1]]
                        tkconfigure(cb.en.infile, values = unlist(listOpenFiles))
                    }
                })

                helpWidget(cb.en.infile, lang.dlg[['tooltip']][['3']], lang.dlg[['status']][['3']])
                helpWidget(bt.infile, lang.dlg[['tooltip']][['6']], lang.dlg[['status']][['6']])
            }

            ###
            if(str_trim(tclvalue(DataType)) == CbdatatypeVAL[2]){
                tclvalue(txt.INData.var) <- lang.dlg[['label']][['4']]

                cb.en.infile <<- tkentry(frameInData, textvariable = input.file, width = largeur2)

                tkconfigure(bt.infile, command = function(){
                    path.rds <- tclvalue(tkgetOpenFile(initialdir = getwd(), filetypes = .cdtEnv$tcl$data$filetypes6))
                    tclvalue(input.file) <- if(path.rds %in% c("", "NA") | is.na(path.rds)) "" else path.rds
                })

                helpWidget(cb.en.infile, lang.dlg[['tooltip']][['4']], lang.dlg[['status']][['4']])
                helpWidget(bt.infile, lang.dlg[['tooltip']][['7']], lang.dlg[['status']][['7']])
            }

            ###
            if(str_trim(tclvalue(DataType)) == CbdatatypeVAL[3]){
                tclvalue(txt.INData.var) <- lang.dlg[['label']][['5']]

                cb.en.infile <<- tkentry(frameInData, textvariable = input.file, width = largeur2)

                tkconfigure(set.infile, command = function(){
                    GeneralParameters$cdtnetcdf <<- getInfoNetcdfData(.cdtEnv$tcl$main$win, GeneralParameters$cdtnetcdf,
                                                                    str_trim(tclvalue(input.file)), tclvalue(timeSteps))
                    settingINData <<- 1
                })

                tkconfigure(bt.infile, command = function(){
                    dirnc <- tk_choose.dir(getwd(), "")
                    tclvalue(input.file) <- if(dirnc %in% c("", "NA") | is.na(dirnc)) "" else dirnc
                })

                helpWidget(cb.en.infile, lang.dlg[['tooltip']][['5']], lang.dlg[['status']][['5']])
                helpWidget(bt.infile, lang.dlg[['tooltip']][['7']], lang.dlg[['status']][['7']])
            }

            tkgrid(cb.en.infile, row = 2, column = 0, sticky = 'we', rowspan = 1, columnspan = 9, padx = 0, pady = 1, ipadx = 1, ipady = 1)
        })

        #######################

        stateAggr <- if(GeneralParameters$intstep == GeneralParameters$outstep) "disabled" else "normal"

        bt.AggrFun <- ttkbutton(subfr1, text = lang.dlg[['button']][['3']], state = stateAggr)

        tkconfigure(bt.AggrFun, command = function(){
            AGGRFUN <- c("sum", "mean", "median", "max", "min", "count")
            GeneralParameters$aggr.series <<- getInfo_AggregateFun(.cdtEnv$tcl$main$win,
                                                                   GeneralParameters$aggr.series,
                                                                   AGGRFUN
                                                                  )
        })

        #######################

        bt.BasePeriod <- ttkbutton(subfr1, text = lang.dlg[['button']][['1']])

        tkconfigure(bt.BasePeriod, command = function(){
            GeneralParameters$climato <<- getInfoBasePeriod(.cdtEnv$tcl$main$win,
                                                            GeneralParameters$climato)
        })

        helpWidget(bt.BasePeriod, lang.dlg[['tooltip']][['8']], lang.dlg[['status']][['8']])

        #######################

        frameBaseP <- tkframe(subfr1, relief = 'groove', borderwidth = 2)

        dayWin <- tclVar(GeneralParameters$climato$window)

        statedayW <- if(GeneralParameters$outstep == "daily") "normal" else "disabled"

        txt.daywin1 <- tklabel(frameBaseP, text = lang.dlg[['label']][['11']], anchor = 'e', justify = 'right')
        en.daywin <- tkentry(frameBaseP, textvariable = dayWin, width = 3, state = statedayW)
        txt.daywin2 <- tklabel(frameBaseP, text = lang.dlg[['label']][['12']], anchor = 'w', justify = 'left')

        tkgrid(txt.daywin1, row = 0, column = 0, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
        tkgrid(en.daywin, row = 0, column = 1, sticky = 'w', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
        tkgrid(txt.daywin2, row = 0, column = 2, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)

        helpWidget(en.daywin, lang.dlg[['tooltip']][['11']], lang.dlg[['status']][['11']])

        #############################

        frameDirSav <- tkframe(subfr1, relief = 'groove', borderwidth = 2)

        dir.save <- tclVar(GeneralParameters$out.dir)

        txt.dir.save <- tklabel(frameDirSav, text = lang.dlg[['label']][['13']], anchor = 'w', justify = 'left')
        en.dir.save <- tkentry(frameDirSav, textvariable = dir.save, width = largeur2)
        bt.dir.save <- tkbutton(frameDirSav, text = "...")

        tkgrid(txt.dir.save, row = 0, column = 0, sticky = 'we', rowspan = 1, columnspan = 5, padx = 1, pady = 0, ipadx = 1, ipady = 1)
        tkgrid(en.dir.save, row = 1, column = 0, sticky = 'we', rowspan = 1, columnspan = 4, padx = 0, pady = 0, ipadx = 1, ipady = 1)
        tkgrid(bt.dir.save, row = 1, column = 4, sticky = 'w', rowspan = 1, columnspan = 1, padx = 0, pady = 0, ipadx = 1, ipady = 1)

        helpWidget(en.dir.save, lang.dlg[['tooltip']][['12']], lang.dlg[['status']][['12']])
        helpWidget(bt.dir.save, lang.dlg[['tooltip']][['7']], lang.dlg[['status']][['7']])

        ######
        tkconfigure(bt.dir.save, command = function() fileORdir2Save(dir.save, isFile = FALSE))

        #############################

        calculateBut <- ttkbutton(subfr1, text = lang.dlg[['button']][['2']])

        #################

        tkconfigure(calculateBut, command = function(){
            GeneralParameters$intstep <- periodVAL[CbperiodVAL %in% str_trim(tclvalue(timeSteps))]
            GeneralParameters$outstep <- periodVAL1[CbperiodVAL1 %in% str_trim(tclvalue(outSteps))]
            GeneralParameters$seasonal$start.mon <- which(MOIS %in% str_trim(tclvalue(start.mon)))
            GeneralParameters$seasonal$length.mon <- as.numeric(str_trim(tclvalue(length.mon)))

            GeneralParameters$data.type <- datatypeVAL[CbdatatypeVAL %in% str_trim(tclvalue(DataType))]

            if(str_trim(tclvalue(DataType)) == CbdatatypeVAL[1])
                GeneralParameters$cdtstation$file <- str_trim(tclvalue(input.file))
            if(str_trim(tclvalue(DataType)) == CbdatatypeVAL[2])
                GeneralParameters$cdtdataset$index <- str_trim(tclvalue(input.file))
            if(str_trim(tclvalue(DataType)) == CbdatatypeVAL[3])
                GeneralParameters$cdtnetcdf$dir <- str_trim(tclvalue(input.file))

            GeneralParameters$out.dir <- str_trim(tclvalue(dir.save))
            GeneralParameters$climato$window <- if(GeneralParameters$intstep == 'daily') as.numeric(str_trim(tclvalue(dayWin))) else 0

            if(str_trim(tclvalue(DataType)) == CbdatatypeVAL[3] & is.null(settingINData)){
                Insert.Messages.Out(lang.dlg[['message']][['1']], format = TRUE)
                return(NULL)
            }

            # assign('GeneralParameters', GeneralParameters, envir = .GlobalEnv)

            tkconfigure(.cdtEnv$tcl$main$win, cursor = 'watch')
            tcl('update')
            ret <- tryCatch(
                {
                    climatologiesCalcProcs(GeneralParameters)
                },
                warning = function(w) warningFun(w),
                error = function(e) errorFun(e),
                finally = {
                    tkconfigure(.cdtEnv$tcl$main$win, cursor = '')
                    tcl('update')
                }
            )

            if(!is.null(ret)){
                if(ret == 0){
                    Insert.Messages.Out(lang.dlg[['message']][['3']], TRUE, "s")

                    .cdtData$EnvData$plot.maps$data.type <- .cdtData$EnvData$output$params$data.type
                    .cdtData$EnvData$plot.maps[c('lon', 'lat', 'id')] <- .cdtData$EnvData$output$data[c('lon', 'lat', 'id')]
                    ###################
                    set.climato.index()
                    if(!str_trim(tclvalue(outSteps)) %in% CbOutVAL[5:6]) widgets.Station.Pixel()
                    set.plot.type()
                    res <- try(read.Climatology.Map(), silent = TRUE)
                    if(inherits(res, "try-error") | is.null(res)) return(NULL)
                }else Insert.Messages.Out(lang.dlg[['message']][['4']], TRUE, 'e')
            }else Insert.Messages.Out(lang.dlg[['message']][['4']], TRUE, 'e')
        })

        ############################################

        tkgrid(frameTimeS, row = 0, column = 0, sticky = 'we', padx = 1, pady = 1, ipadx = 1, ipady = 1)
        tkgrid(frameOutS, row = 0, column = 1, sticky = 'we', padx = 1, pady = 1, ipadx = 1, ipady = 1)
        tkgrid(frameSeas, row = 1, column = 0, sticky = '', columnspan = 2, padx = 1, pady = 3, ipadx = 1, ipady = 1)
        tkgrid(frameInData, row = 2, column = 0, sticky = 'we', columnspan = 2, padx = 1, pady = 1, ipadx = 1, ipady = 1)

        tkgrid(bt.AggrFun, row = 3, column = 0, sticky = 'we', columnspan = 2, padx = 1, pady = 3, ipadx = 1, ipady = 1)
        tkgrid(bt.BasePeriod, row = 4, column = 0, sticky = 'we', columnspan = 2, padx = 1, pady = 1, ipadx = 1, ipady = 1)

        tkgrid(frameBaseP, row = 5, column = 0, sticky = '', columnspan = 2, padx = 1, pady = 3, ipadx = 1, ipady = 1)
        tkgrid(frameDirSav, row = 6, column = 0, sticky = '', columnspan = 2, padx = 1, pady = 1, ipadx = 1, ipady = 1)
        tkgrid(calculateBut, row = 7, column = 0, sticky = 'we', columnspan = 2, padx = 1, pady = 3, ipadx = 1, ipady = 1)

    #######################################################################################################

    #Tab2
    subfr2 <- bwTabScrollableFrame(cmd.tab2)

        ##############################################

        frameClimatoDat <- ttklabelframe(subfr2, text = lang.dlg[['label']][['14']], relief = 'groove')

        climDataExist <- tclVar(0)
        file.ClimIndex <- tclVar()

        stateClimatoDat <- if(tclvalue(climDataExist) == "1") "normal" else "disabled"

        chk.climIdx <- tkcheckbutton(frameClimatoDat, variable = climDataExist, text = lang.dlg[['checkbutton']][['1']], anchor = 'w', justify = 'left')
        en.climIdx <- tkentry(frameClimatoDat, textvariable = file.ClimIndex, width = largeur2, state = stateClimatoDat)
        bt.climIdx <- ttkbutton(frameClimatoDat, text = .cdtEnv$tcl$lang$global[['button']][['6']], state = stateClimatoDat)

        tkgrid(chk.climIdx, row = 0, column = 0, sticky = 'we', rowspan = 1, columnspan = 4, padx = 1, pady = 1, ipadx = 1, ipady = 1)
        tkgrid(bt.climIdx, row = 0, column = 4, sticky = 'e', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
        tkgrid(en.climIdx, row = 1, column = 0, sticky = 'we', rowspan = 1, columnspan = 5, padx = 1, pady = 1, ipadx = 1, ipady = 1)

        ##############

        tkconfigure(bt.climIdx, command = function(){
            path.Clim <- tclvalue(tkgetOpenFile(initialdir = getwd(), filetypes = .cdtEnv$tcl$data$filetypes6))
            if(path.Clim %in% c("", "NA") | is.na(path.Clim)) return(NULL)
            tclvalue(file.ClimIndex) <- path.Clim

            if(file.exists(str_trim(tclvalue(file.ClimIndex)))){
                OutClimdata <- try(readRDS(str_trim(tclvalue(file.ClimIndex))), silent = TRUE)
                if(inherits(OutClimdata, "try-error")){
                    Insert.Messages.Out(lang.dlg[['message']][['5']], TRUE, 'e')
                    Insert.Messages.Out(gsub('[\r\n]', '', OutClimdata[1]), TRUE, 'e')
                    tkconfigure(cb.clim.Date, values = "")
                    tclvalue(.cdtData$EnvData$climDate) <- ""
                    return(NULL)
                }

                .cdtData$EnvData$output <- OutClimdata
                .cdtData$EnvData$PathClim <- dirname(str_trim(tclvalue(file.ClimIndex)))
                .cdtData$EnvData$plot.maps$data.type <- .cdtData$EnvData$output$params$data.type
                .cdtData$EnvData$plot.maps[c('lon', 'lat', 'id')] <- .cdtData$EnvData$output$data[c('lon', 'lat', 'id')]
                ###################

                set.climato.index()
                if(!OutClimdata$params$outstep %in% c('annual', 'seasonal')) widgets.Station.Pixel()
                set.plot.type()
                ret <- try(read.Climatology.Map(), silent = TRUE)
                if(inherits(ret, "try-error") | is.null(ret)) return(NULL)

                ###################
                stateMaps <- if(OutClimdata$params$outstep %in% c('annual', 'seasonal')) 'disabled' else 'normal'
                tkconfigure(cb.clim.Date, state = stateMaps)
                tkconfigure(bt.clim.Date.prev, state = stateMaps)
                tkconfigure(bt.clim.Date.next, state = stateMaps)

                stateGraphs <- if(OutClimdata$params$outstep %in% c('annual', 'seasonal')) 'disabled' else 'normal'
                tkconfigure(cb.typeTSp, state = stateGraphs)
                tkconfigure(bt.TsGraph.plot, state = stateGraphs)
                tkconfigure(bt.TSGraphOpt, state = stateGraphs)
            }
        })

        ###############
        tkbind(chk.climIdx, "<Button-1>", function(){
            stateClimatoDat <- if(tclvalue(climDataExist) == '1') 'disabled' else 'normal'
            tkconfigure(en.climIdx, state = stateClimatoDat)
            tkconfigure(bt.climIdx, state = stateClimatoDat)

            stateCaclBut <- if(tclvalue(climDataExist) == '1') 'normal' else 'disabled'
            tkconfigure(calculateBut, state = stateCaclBut)
            tkconfigure(cb.fperiod, state = stateCaclBut)
            tkconfigure(cb.outclim, state = stateCaclBut)
            tkconfigure(cb.datatype, state = stateCaclBut)
            tkconfigure(cb.en.infile, state = stateCaclBut)
            tkconfigure(bt.infile, state = stateCaclBut)
            tkconfigure(bt.BasePeriod, state = stateCaclBut)
            tkconfigure(en.dir.save, state = stateCaclBut)
            tkconfigure(bt.dir.save, state = stateCaclBut)

            stateSeas <- 'disabled'
            if(tclvalue(climDataExist) == '1')
                stateSeas <- if(str_trim(tclvalue(outSteps)) == CbOutVAL[6]) "normal" else "disabled"
            tkconfigure(cb.seasS, state = stateSeas)
            tkconfigure(cb.seasL, state = stateSeas)

            stateAggr <- 'disabled'
            if(tclvalue(climDataExist) == '1')
                stateAggr <- if(str_trim(tclvalue(timeSteps)) == str_trim(tclvalue(outSteps))) "disabled" else "normal"
            tkconfigure(bt.AggrFun, state = stateAggr)

            stateSetNC <- 'disabled'
            statedayW <- 'disabled'
            if(tclvalue(climDataExist) == '1'){
                stateSetNC <- if(str_trim(tclvalue(DataType)) == CbdatatypeVAL[3]) "normal" else "disabled"
                statedayW <- if(str_trim(tclvalue(timeSteps)) == CbperiodVAL[1]) "normal" else "disabled"
            }
            tkconfigure(set.infile, state = stateSetNC)
            tkconfigure(en.daywin, state = statedayW)
        })

        ##############################################

        frameClimatoMap <- ttklabelframe(subfr2, text = lang.dlg[['label']][['15']], relief = 'groove')

        .cdtData$EnvData$CbClimSTAT <- lang.dlg[['combobox']][['2']]
        .cdtData$EnvData$climVar <- tclVar(.cdtData$EnvData$CbClimSTAT[1])
        .cdtData$EnvData$climDate <- tclVar()

        stateMaps <- if(GeneralParameters$outstep %in% c('annual', 'seasonal')) 'disabled' else 'normal'

        cb.clim.Var <- ttkcombobox(frameClimatoMap, values = .cdtData$EnvData$CbClimSTAT, textvariable = .cdtData$EnvData$climVar, width = largeur3)
        bt.clim.maps <- ttkbutton(frameClimatoMap, text = .cdtEnv$tcl$lang$global[['button']][['3']], width = largeur6)
        cb.clim.Date <- ttkcombobox(frameClimatoMap, values = "", textvariable = .cdtData$EnvData$climDate, justify = 'center', width = largeur7, state = stateMaps)
        bt.clim.Date.prev <- ttkbutton(frameClimatoMap, text = "<<", width = 4, state = stateMaps)
        bt.clim.Date.next <- ttkbutton(frameClimatoMap, text = ">>", width = 4, state = stateMaps)
        bt.clim.MapOpt <- ttkbutton(frameClimatoMap, text = .cdtEnv$tcl$lang$global[['button']][['4']], width = largeur6)

        tkgrid(cb.clim.Var, row = 0, column = 0, sticky = 'we', rowspan = 1, columnspan = 4, padx = 1, pady = 1, ipadx = 1, ipady = 1)
        tkgrid(bt.clim.maps, row = 0, column = 4, sticky = 'we', rowspan = 1, columnspan = 2, padx = 1, pady = 1, ipadx = 1, ipady = 1)
        tkgrid(bt.clim.Date.prev, row = 1, column = 0, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
        tkgrid(cb.clim.Date, row = 1, column = 1, sticky = 'we', rowspan = 1, columnspan = 2, padx = 1, pady = 1, ipadx = 1, ipady = 1)
        tkgrid(bt.clim.Date.next, row = 1, column = 3, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
        tkgrid(bt.clim.MapOpt, row = 1, column = 4, sticky = 'we', rowspan = 1, columnspan = 2, padx = 1, pady = 1, ipadx = 1, ipady = 1)

        ###############

        tkconfigure(bt.clim.MapOpt, command = function(){
            if(!is.null(.cdtData$EnvData$climdata$map)){
                atlevel <- pretty(.cdtData$EnvData$climdata$map$z, n = 10, min.n = 7)
                if(is.null(.cdtData$EnvData$climMapOp$userLvl$levels)){
                    .cdtData$EnvData$climMapOp$userLvl$levels <- atlevel
                }else{
                    if(!.cdtData$EnvData$climMapOp$userLvl$custom)
                        .cdtData$EnvData$climMapOp$userLvl$levels <- atlevel
                }
            }
            .cdtData$EnvData$climMapOp <- MapGraph.MapOptions(.cdtData$EnvData$climMapOp)

            if(str_trim(tclvalue(.cdtData$EnvData$plot.maps$plot.type)) == "Points")
                .cdtData$EnvData$tab$pointSize <- .cdtData$EnvData$climMapOp$pointSize
        })

        #########
        .cdtData$EnvData$tab$ClimMap <- NULL

        tkconfigure(bt.clim.maps, command = function(){
            if(str_trim(tclvalue(.cdtData$EnvData$climVar)) != "" &
                str_trim(tclvalue(.cdtData$EnvData$climDate)) != "" &
                !is.null(.cdtData$EnvData$climdata))
                    climatologiesCalc.Display.Maps()
        })

        tkconfigure(bt.clim.Date.prev, command = function(){
            if(str_trim(tclvalue(.cdtData$EnvData$climDate)) != ""){
                idaty <- which(.cdtData$EnvData$output$index == as.numeric(str_trim(tclvalue(.cdtData$EnvData$climDate))))
                idaty <- idaty - 1
                if(idaty < 1) idaty <- length(.cdtData$EnvData$output$index)
                tclvalue(.cdtData$EnvData$climDate) <- .cdtData$EnvData$output$index[idaty]

                ret <- try(read.Climatology.Map(), silent = TRUE)
                if(inherits(ret, "try-error") | is.null(ret)) return(NULL)

                climatologiesCalc.Display.Maps()
            }
        })

        tkconfigure(bt.clim.Date.next, command = function(){
            if(str_trim(tclvalue(.cdtData$EnvData$climDate)) != ""){
                idaty <- which(.cdtData$EnvData$output$index == as.numeric(str_trim(tclvalue(.cdtData$EnvData$climDate))))
                idaty <- idaty + 1
                if(idaty > length(.cdtData$EnvData$output$index)) idaty <- 1
                tclvalue(.cdtData$EnvData$climDate) <- .cdtData$EnvData$output$index[idaty]

                ret <- try(read.Climatology.Map(), silent = TRUE)
                if(inherits(ret, "try-error") | is.null(ret)) return(NULL)

                climatologiesCalc.Display.Maps()
            }
        })

        ###############

        tkbind(cb.clim.Var, "<<ComboboxSelected>>", function(){
            if(!is.null(.cdtData$EnvData$output)){
                ret <- try(read.Climatology.Map(), silent = TRUE)
                if(inherits(ret, "try-error") | is.null(ret)) return(NULL)
            }
        })

        tkbind(cb.clim.Date, "<<ComboboxSelected>>", function(){
            if(!is.null(.cdtData$EnvData$climdata)){
                ret <- try(read.Climatology.Map(), silent = TRUE)
                if(inherits(ret, "try-error") | is.null(ret)) return(NULL)
            }
        })

        ##############################################

        framePlotType <- tkframe(subfr2)

        .cdtData$EnvData$plot.maps$plot.type <- tclVar("Pixels")

        txt.plotType <- tklabel(framePlotType, text = lang.dlg[['label']][['25']], anchor = 'e', justify = 'right')
        cb.plotType <- ttkcombobox(framePlotType, values = "Pixels", textvariable = .cdtData$EnvData$plot.maps$plot.type, width = largeur3)

        tkgrid(txt.plotType, row = 0, column = 0, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
        tkgrid(cb.plotType, row = 0, column = 1, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)

        ###############

        tkbind(cb.plotType, "<<ComboboxSelected>>", function(){
            if(!is.null(.cdtData$EnvData$climdata)){
                ret <- try(read.Climatology.Map(), silent = TRUE)
                if(inherits(ret, "try-error") | is.null(ret)) return(NULL)
            }
        })

        ##############################################

        tkgrid(frameClimatoDat, row = 0, column = 0, sticky = 'we', padx = 1, pady = 1, ipadx = 1, ipady = 1)
        tkgrid(frameClimatoMap, row = 1, column = 0, sticky = 'we', padx = 1, pady = 3, ipadx = 1, ipady = 1)
        tkgrid(framePlotType, row = 2, column = 0, sticky = '', padx = 1, pady = 3, ipadx = 1, ipady = 1)

    #######################################################################################################

    #Tab3
    subfr3 <- bwTabScrollableFrame(cmd.tab3)

        ##############################################

        frameClimatoTS <- ttklabelframe(subfr3, text = lang.dlg[['label']][['16']], relief = 'groove')

        typeTSPLOT <- c("Line", "Barplot")
        .cdtData$EnvData$plot.maps$typeTSp <- tclVar("Line")

        stateGraphs <- if(GeneralParameters$outstep %in% c('annual', 'seasonal')) 'disabled' else 'normal'

        cb.typeTSp <- ttkcombobox(frameClimatoTS, values = typeTSPLOT, textvariable = .cdtData$EnvData$plot.maps$typeTSp, width = largeur4, state = stateGraphs)
        bt.TsGraph.plot <- ttkbutton(frameClimatoTS, text = .cdtEnv$tcl$lang$global[['button']][['3']], width = largeur8, state = stateGraphs)
        bt.TSGraphOpt <- ttkbutton(frameClimatoTS, text = .cdtEnv$tcl$lang$global[['button']][['4']], width = largeur8, state = stateGraphs)

        tkgrid(cb.typeTSp, row = 0, column = 3, sticky = 'we', rowspan = 1, columnspan = 4, padx = 1, pady = 5, ipadx = 1, ipady = 1)
        tkgrid(bt.TSGraphOpt, row = 1, column = 0, sticky = 'we', rowspan = 1, columnspan = 5, padx = 1, pady = 1, ipadx = 1, ipady = 1)
        tkgrid(bt.TsGraph.plot, row = 1, column = 5, sticky = 'we', rowspan = 1, columnspan = 5, padx = 1, pady = 1, ipadx = 1, ipady = 1)

        #################

        tkconfigure(bt.TSGraphOpt, command = function(){
            typeTSp <- str_trim(tclvalue(.cdtData$EnvData$plot.maps$typeTSp))
            suffix.fun <- switch(typeTSp, "Barplot" = "Bar", "Line" = "Line")
            plot.fun <- get(paste0("MapGraph.GraphOptions.", suffix.fun), mode = "function")
            .cdtData$EnvData$TSGraphOp <- plot.fun(.cdtData$EnvData$TSGraphOp)
        })

        #########
        .cdtData$EnvData$tab$ClimGraph <- NULL

        tkconfigure(bt.TsGraph.plot, command = function(){
            if(!is.null(.cdtData$EnvData$climdata)){
                imgContainer <- CDT.Display.Graph(climatologiesCalc.plotClimGraph, .cdtData$EnvData$tab$ClimGraph, 'Climatology-Graph')
                .cdtData$EnvData$tab$ClimGraph <- imageNotebookTab_unik(imgContainer, .cdtData$EnvData$tab$ClimGraph)
            }
        })

        ##############################################

        frameSTNCrds <- ttklabelframe(subfr3, text = lang.dlg[['label']][['17']], relief = 'groove')

        frTS2 <- tkframe(frameSTNCrds)
        .cdtData$EnvData$plot.maps$lonLOC <- tclVar()
        .cdtData$EnvData$plot.maps$latLOC <- tclVar()
        .cdtData$EnvData$plot.maps$stnIDTSp <- tclVar()

        tkgrid(frTS2, row = 0, column = 0, sticky = 'e', pady = 1)

        ##############################################

        tkgrid(frameClimatoTS, row = 0, column = 0, sticky = 'we', pady = 1)
        tkgrid(frameSTNCrds, row = 1, column = 0, sticky = '', pady = 3)

    #######################################################################################################

    #Tab4
    subfr4 <- bwTabScrollableFrame(cmd.tab4)

        ##############################################

        frameSHP <- ttklabelframe(subfr4, text = lang.dlg[['label']][['18']], relief = 'groove')

        .cdtData$EnvData$shp$add.shp <- tclVar(FALSE)
        file.plotShp <- tclVar()
        stateSHP <- "disabled"

        chk.addshp <- tkcheckbutton(frameSHP, variable = .cdtData$EnvData$shp$add.shp, text = lang.dlg[['checkbutton']][['2']], anchor = 'w', justify = 'left')
        bt.addshpOpt <- ttkbutton(frameSHP, text = .cdtEnv$tcl$lang$global[['button']][['4']], state = stateSHP)
        cb.addshp <- ttkcombobox(frameSHP, values = unlist(listOpenFiles), textvariable = file.plotShp, width = largeur1, state = stateSHP)
        bt.addshp <- tkbutton(frameSHP, text = "...", state = stateSHP)

        tkgrid(chk.addshp, row = 0, column = 0, sticky = 'we', rowspan = 1, columnspan = 6, padx = 1, pady = 1)
        tkgrid(bt.addshpOpt, row = 0, column = 6, sticky = 'we', rowspan = 1, columnspan = 2, padx = 1, pady = 1)
        tkgrid(cb.addshp, row = 1, column = 0, sticky = 'we', rowspan = 1, columnspan = 7, padx = 1, pady = 1)
        tkgrid(bt.addshp, row = 1, column = 7, sticky = 'w', rowspan = 1, columnspan = 1, padx = 0, pady = 1)

        ########
        tkconfigure(bt.addshp, command = function(){
            shp.opfiles <- getOpenShp(.cdtEnv$tcl$main$win)
            if(!is.null(shp.opfiles)){
                update.OpenFiles('shp', shp.opfiles)
                tclvalue(file.plotShp) <- shp.opfiles[[1]]
                listOpenFiles[[length(listOpenFiles) + 1]] <<- shp.opfiles[[1]]
                tkconfigure(cb.addshp, values = unlist(listOpenFiles))

                shpofile <- getShpOpenData(file.plotShp)
                if(is.null(shpofile))
                    .cdtData$EnvData$shp$ocrds <- NULL
                else
                    .cdtData$EnvData$shp$ocrds <- getBoundaries(shpofile[[2]])
            }
        })

        ########

        tkconfigure(bt.addshpOpt, command = function(){
            .cdtData$EnvData$SHPOp <- MapGraph.GraphOptions.LineSHP(.cdtData$EnvData$SHPOp)
        })

        #################
        tkbind(cb.addshp, "<<ComboboxSelected>>", function(){
            shpofile <- getShpOpenData(file.plotShp)
            if(is.null(shpofile))
                .cdtData$EnvData$shp$ocrds <- NULL
            else
                .cdtData$EnvData$shp$ocrds <- getBoundaries(shpofile[[2]])
        })

        tkbind(chk.addshp, "<Button-1>", function(){
            stateSHP <- if(tclvalue(.cdtData$EnvData$shp$add.shp) == "1") "disabled" else "normal"
            tkconfigure(cb.addshp, state = stateSHP)
            tkconfigure(bt.addshp, state = stateSHP)
            tkconfigure(bt.addshpOpt, state = stateSHP)
        })

        ##############################################

        tkgrid(frameSHP, row = 0, column = 0, sticky = 'we', pady = 1)

    #######################################################################################################

    widgets.Station.Pixel <- function(){
        tkdestroy(frTS2)
        frTS2 <<- tkframe(frameSTNCrds)

        if(.cdtData$EnvData$plot.maps$data.type == "cdtstation"){
            stnIDTSPLOT <- .cdtData$EnvData$output$data$id
            txt.stnSel <- tklabel(frTS2, text = lang.dlg[['label']][['19']])
            bt.stnID.prev <- ttkbutton(frTS2, text = "<<", width = 6)
            bt.stnID.next <- ttkbutton(frTS2, text = ">>", width = 6)
            cb.stnID <- ttkcombobox(frTS2, values = stnIDTSPLOT, textvariable = .cdtData$EnvData$plot.maps$stnIDTSp, width = largeur4)
            tclvalue(.cdtData$EnvData$plot.maps$stnIDTSp) <- stnIDTSPLOT[1]

            tkgrid(txt.stnSel, row = 0, column = 0, sticky = '', rowspan = 1, columnspan = 3, padx = 1, pady = 1, ipadx = 1, ipady = 1)
            tkgrid(bt.stnID.prev, row = 1, column = 0, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
            tkgrid(cb.stnID, row = 1, column = 1, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
            tkgrid(bt.stnID.next, row = 1, column = 2, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)

            tkconfigure(bt.stnID.prev, command = function(){
                if(!is.null(.cdtData$EnvData$climdata)){
                    istn <- which(stnIDTSPLOT == str_trim(tclvalue(.cdtData$EnvData$plot.maps$stnIDTSp)))
                    istn <- istn - 1
                    if(istn < 1) istn <- length(stnIDTSPLOT)
                    tclvalue(.cdtData$EnvData$plot.maps$stnIDTSp) <- stnIDTSPLOT[istn]

                    imgContainer <- CDT.Display.Graph(climatologiesCalc.plotClimGraph, .cdtData$EnvData$tab$ClimGraph, 'Climatology-Graph')
                    .cdtData$EnvData$tab$ClimGraph <- imageNotebookTab_unik(imgContainer, .cdtData$EnvData$tab$ClimGraph)
                }
            })

            tkconfigure(bt.stnID.next, command = function(){
                if(!is.null(.cdtData$EnvData$climdata)){
                    istn <- which(stnIDTSPLOT == str_trim(tclvalue(.cdtData$EnvData$plot.maps$stnIDTSp)))
                    istn <- istn + 1
                    if(istn > length(stnIDTSPLOT)) istn <- 1
                    tclvalue(.cdtData$EnvData$plot.maps$stnIDTSp) <- stnIDTSPLOT[istn]

                    imgContainer <- CDT.Display.Graph(climatologiesCalc.plotClimGraph, .cdtData$EnvData$tab$ClimGraph, 'Climatology-Graph')
                    .cdtData$EnvData$tab$ClimGraph <- imageNotebookTab_unik(imgContainer, .cdtData$EnvData$tab$ClimGraph)
                }
            })
        }else{
            txt.crdSel <- tklabel(frTS2, text = lang.dlg[['label']][['20']], anchor = 'w', justify = 'left')
            txt.lonLoc <- tklabel(frTS2, text = lang.dlg[['label']][['21']], anchor = 'e', justify = 'right')
            en.lonLoc <- tkentry(frTS2, textvariable = .cdtData$EnvData$plot.maps$lonLOC, width = 8)
            txt.latLoc <- tklabel(frTS2, text = lang.dlg[['label']][['22']], anchor = 'e', justify = 'right')
            en.latLoc <- tkentry(frTS2, textvariable = .cdtData$EnvData$plot.maps$latLOC, width = 8)
            stnIDTSPLOT <- ""
            tclvalue(.cdtData$EnvData$plot.maps$stnIDTSp) <- ""

            tkgrid(txt.crdSel, row = 0, column = 0, sticky = 'we', rowspan = 1, columnspan = 4, padx = 1, pady = 1, ipadx = 1, ipady = 1)
            tkgrid(txt.lonLoc, row = 1, column = 0, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
            tkgrid(en.lonLoc, row = 1, column = 1, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
            tkgrid(txt.latLoc, row = 1, column = 2, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
            tkgrid(en.latLoc, row = 1, column = 3, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
        }

        tkgrid(frTS2, row = 0, column = 0, sticky = 'e', pady = 1)
        return(0)
    }

    #################

    set.plot.type <- function(){
        if(.cdtData$EnvData$plot.maps$data.type == "cdtstation")
        {
            plot.type <- c("Pixels", "Points")
            .cdtData$EnvData$plot.maps$.data.type <- "Points"

            .cdtData$EnvData$climMapOp$pointSize <- 1.0
        }else{
            plot.type <- c("Pixels", "FilledContour")
            .cdtData$EnvData$plot.maps$.data.type <- "Grid"
        }
        tkconfigure(cb.plotType, values = plot.type)
    }

    #################

    set.climato.index <- function(){
        tkconfigure(cb.clim.Date, values = .cdtData$EnvData$output$index)
        tclvalue(.cdtData$EnvData$climDate) <- .cdtData$EnvData$output$index[1]
        return(0)
    }

    #######################################################################################################

    read.Climatology.Map <- function(){
        tkconfigure(.cdtEnv$tcl$main$win, cursor = 'watch')
        tcl('update')
        on.exit({
            tkconfigure(.cdtEnv$tcl$main$win, cursor = '')
            tcl('update')
        })

        cilmdata.Var <- if(str_trim(tclvalue(.cdtData$EnvData$climVar)) == .cdtData$EnvData$CbClimSTAT[1]) "CDTMEAN" else "CDTSTD"

        if(.cdtData$EnvData$plot.maps$data.type == "cdtstation"){
            fileClimdata <- file.path(.cdtData$EnvData$PathClim, cilmdata.Var, paste0(cilmdata.Var, ".rds"))
            if(!file.exists(fileClimdata)){
                Insert.Messages.Out(paste(fileClimdata, lang.dlg[['message']][['6']]), TRUE, 'e')
                return(NULL)
            }

            change.plot <- str_trim(tclvalue(.cdtData$EnvData$plot.maps$plot.type))

            ########
            readClimData <- TRUE
            if(!is.null(.cdtData$EnvData$climdata))
                if(!is.null(.cdtData$EnvData$fileClimdata))
                    if(.cdtData$EnvData$fileClimdata == fileClimdata) readClimData <- FALSE

            if(readClimData){
                .cdtData$EnvData$climdata$data <- readRDS(fileClimdata)
                .cdtData$EnvData$fileClimdata <- fileClimdata
            }

            ########
            rasterClimData <- TRUE
            if(!rasterClimData)
                if(!is.null(.cdtData$EnvData$climdata$rasterIdx))
                    if(.cdtData$EnvData$fileClimdata == fileClimdata)
                        if(.cdtData$EnvData$climdata$rasterIdx == str_trim(tclvalue(.cdtData$EnvData$climDate))) rasterClimData <- FALSE

            if(!rasterClimData)
                if(.cdtData$EnvData$change.plot != change.plot) rasterClimData <- TRUE

            if(rasterClimData){
                idt <- which(.cdtData$EnvData$output$index == as.numeric(str_trim(tclvalue(.cdtData$EnvData$climDate))))

                X0 <- .cdtData$EnvData$output$data$lon
                Y0 <- .cdtData$EnvData$output$data$lat
                VAR0 <- as.numeric(.cdtData$EnvData$climdata$data[idt, ])

                if(change.plot == "Pixels"){
                    nx <- nx_ny_as.image(diff(range(X0)))
                    ny <- nx_ny_as.image(diff(range(Y0)))
                    tmp <- cdt.as.image(VAR0, nx = nx, ny = ny, pts.xy = cbind(X0, Y0))
                    .cdtData$EnvData$climdata$map$x <- tmp$x
                    .cdtData$EnvData$climdata$map$y <- tmp$y
                    .cdtData$EnvData$climdata$map$z <- tmp$z
                    rm(tmp)
                }

                if(change.plot == "Points"){
                    .cdtData$EnvData$climdata$map$x <- X0
                    .cdtData$EnvData$climdata$map$y <- Y0
                    .cdtData$EnvData$climdata$map$z <- VAR0
                }

                .cdtData$EnvData$climdata$rasterIdx <- str_trim(tclvalue(.cdtData$EnvData$climDate))
                .cdtData$EnvData$climdata$Var <- cilmdata.Var
                .cdtData$EnvData$change.plot <- change.plot
            }
        }else{
            fileClimdata <- file.path(.cdtData$EnvData$PathClim, "DATA_NetCDF", cilmdata.Var,
                            paste0("clim_", as.numeric(str_trim(tclvalue(.cdtData$EnvData$climDate))), ".nc"))
            if(!file.exists(fileClimdata)){
                Insert.Messages.Out(paste(fileClimdata, lang.dlg[['message']][['6']]), TRUE, 'e')
                return(NULL)
            }

            readClimData <- TRUE
            if(!is.null(.cdtData$EnvData$climdata))
                if(!is.null(.cdtData$EnvData$fileClimdata))
                    if(.cdtData$EnvData$fileClimdata == fileClimdata) readClimData <- FALSE

            if(readClimData){
                nc <- nc_open(fileClimdata)
                .cdtData$EnvData$climdata$map$x <- nc$dim[[1]]$vals
                .cdtData$EnvData$climdata$map$y <- nc$dim[[2]]$vals
                .cdtData$EnvData$climdata$map$z <- ncvar_get(nc, varid = nc$var[[1]]$name)
                nc_close(nc)
                .cdtData$EnvData$fileClimdata <- fileClimdata
                .cdtData$EnvData$climdata$Var <- cilmdata.Var
            }

            ###################

            fileClimIdx <- file.path(.cdtData$EnvData$PathClim, cilmdata.Var, paste0(cilmdata.Var, ".rds"))

            readClimIdx <- TRUE
            if(!is.null(.cdtData$EnvData$cdtdataset))
                if(!is.null(.cdtData$EnvData$fileClimIdx))
                    if(.cdtData$EnvData$fileClimIdx == fileClimIdx) rreadClimIdx <- FALSE
            if(readClimIdx){
                .cdtData$EnvData$cdtdataset <- readRDS(fileClimIdx)
                .cdtData$EnvData$cdtdataset$fileInfo <- fileClimIdx
                .cdtData$EnvData$fileClimIdx <- fileClimIdx
            }
        }

        return(0)
    }

    #######################################################################################################

    tkgrid(tknote.cmd, sticky = 'nwes')
    tkgrid.columnconfigure(tknote.cmd, 0, weight = 1)
    tkgrid.rowconfigure(tknote.cmd, 0, weight = 1)

    tcl('update')
    tkgrid(.cdtEnv$tcl$main$cmd.frame, sticky = 'nwes', pady = 1)
    tkgrid.columnconfigure(.cdtEnv$tcl$main$cmd.frame, 0, weight = 1)
    tkgrid.rowconfigure(.cdtEnv$tcl$main$cmd.frame, 0, weight = 1)

    invisible()
}
