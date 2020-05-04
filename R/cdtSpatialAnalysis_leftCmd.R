
spatialAnalysisPanelCmd <- function(){
    listOpenFiles <- openFile_ttkcomboList()
    if(WindowsOS()){
        largeur0 <- .cdtEnv$tcl$fun$w.widgets(22)
        largeur1 <- .cdtEnv$tcl$fun$w.widgets(31)
        largeur2 <- .cdtEnv$tcl$fun$w.widgets(33)
        largeur3 <- 13
        largeur4 <- .cdtEnv$tcl$fun$w.widgets(32)
        largeur5 <- 30
        largeur6 <- 23
    }else{
        largeur0 <- .cdtEnv$tcl$fun$w.widgets(20)
        largeur1 <- .cdtEnv$tcl$fun$w.widgets(21)
        largeur2 <- .cdtEnv$tcl$fun$w.widgets(23)
        largeur3 <- 10
        largeur4 <- .cdtEnv$tcl$fun$w.widgets(22)
        largeur5 <- 22
        largeur6 <- 14
    }

    ###################

    GeneralParameters <- list(data.type = "cdtstation", in.tstep = "daily", in.file = "", out.dir = "",
                              analysis.method = list(mth.fun = "mean", mth.perc = 90, low.thres = 0,
                                                     up.thres = 200, perc.anom = FALSE, startYr.anom = 1981,
                                                     endYr.anom = 2010, trend.unit = 1, trend.min.year = 10),
                              aggr.series = list(aggr.fun = "sum", min.frac = 0.95, opr.fun = ">=", opr.thres = 0),
                              time.series = list(out.series = "monthly", all.years = TRUE, start.year = 1981,
                                                 end.year = 2010, nseq.years = FALSE, custom.years = NA,
                                                 start.month = 1, len.seas = 3, end.month = 12, nseq.months = FALSE,
                                                 custom.months = 1:12)
                            )

    MOIS <- format(ISOdate(2014, 1:12, 1), "%B")

    .cdtData$EnvData$tab$pointSize.climMap <- NULL
    .cdtData$EnvData$climMapOp <- list(presetCol = list(color = 'tim.colors', reverse = FALSE),
                                        userCol = list(custom = FALSE, color = NULL),
                                        userLvl = list(custom = FALSE, levels = NULL, equidist = FALSE),
                                        title = list(user = FALSE, title = ''),
                                        colkeyLab = list(user = FALSE, label = ''),
                                        scalebar = list(add = FALSE, pos = 'bottomleft'),
                                        pointSize = .cdtData$EnvData$tab$pointSize.climMap)

    .cdtData$EnvData$tab$pointSize.TSMap <- NULL
    .cdtData$EnvData$TSMapOp <- list(presetCol = list(color = 'tim.colors', reverse = FALSE),
                                    userCol = list(custom = FALSE, color = NULL),
                                    userLvl = list(custom = FALSE, levels = NULL, equidist = FALSE),
                                    title = list(user = FALSE, title = ''),
                                    colkeyLab = list(user = FALSE, label = ''),
                                    scalebar = list(add = FALSE, pos = 'bottomleft'),
                                    pointSize = .cdtData$EnvData$tab$pointSize.TSMap)

    .cdtData$EnvData$TSGraphOp <- list(
                                    anomaly = list(
                                            anom = list(perc.anom = FALSE, basePeriod = FALSE, startYr.anom = 1981, endYr.anom = 2010),
                                            xlim = list(is.min = FALSE, min = 1981, is.max = FALSE, max = 2017),
                                            ylim = list(is.min = FALSE, min = -100, is.max = FALSE, max = 100),
                                            axislabs = list(is.xlab = FALSE, xlab = '', is.ylab = FALSE, ylab = ''),
                                            title = list(is.title = FALSE, title = '', position = 'top'),
                                            colors = list(negative = "blue", positive = "red")
                                            ),
                                    bar = list(
                                        xlim = list(is.min = FALSE, min = 1981, is.max = FALSE, max = 2017),
                                        ylim = list(is.min = FALSE, min = 0, is.max = FALSE, max = 100),
                                        axislabs = list(is.xlab = FALSE, xlab = '', is.ylab = FALSE, ylab = ''),
                                        title = list(is.title = FALSE, title = '', position = 'top'),
                                        colors = list(col = "darkblue")
                                        ),
                                    line = list(
                                        xlim = list(is.min = FALSE, min = 1981, is.max = FALSE, max = 2017),
                                        ylim = list(is.min = FALSE, min = 0, is.max = FALSE, max = 100),
                                        axislabs = list(is.xlab = FALSE, xlab = '', is.ylab = FALSE, ylab = ''),
                                        title = list(is.title = FALSE, title = '', position = 'top'),
                                        plot = list(type = 'both',
                                            col = list(line = "red", points = "blue"),
                                            lwd = 2, cex = 1.4),
                                        legend = list(
                                            is = list(mean = FALSE, tercile = FALSE, linear = FALSE),
                                            add = list(mean = FALSE, tercile = FALSE, linear = FALSE),
                                            col = list(mean = "black", tercile1 = "green", tercile2 = "blue", linear = "purple3"),
                                            text = list(mean = "Average", tercile1 = "Tercile 0.33333", tercile2 = "Tercile 0.66666", linear = "Trend line"),
                                            lwd = list(mean = 2, tercile = 2, linear = 2))
                                        ),
                                    proba = list(
                                        xlim = list(is.min = FALSE, min = 0, is.max = FALSE, max = 100),
                                        ylim = list(is.min = FALSE, min = 0, is.max = FALSE, max = 100),
                                        axislabs = list(is.xlab = FALSE, xlab = '', is.ylab = FALSE, ylab = ''),
                                        title = list(is.title = FALSE, title = '', position = 'top'),
                                        plot = list(type = 'both',
                                            col = list(line = "red", points = "blue"),
                                            lwd = 2, cex = 0.8),
                                        proba = list(theoretical = TRUE, col = 'black', lwd = 2)
                                        ),
                                    line.enso = list(
                                        xlim = list(is.min = FALSE, min = 1981, is.max = FALSE, max = 2017),
                                        ylim = list(is.min = FALSE, min = 0, is.max = FALSE, max = 100),
                                        axislabs = list(is.xlab = FALSE, xlab = '', is.ylab = FALSE, ylab = ''),
                                        title = list(is.title = FALSE, title = '', position = 'top'),
                                        plot = list(lwd = 2, cex = 2, col = list(line = "black",
                                                    points = c("blue", "gray", "red"))),
                                        legend = list(
                                            is = list(mean = FALSE, tercile = FALSE, linear = FALSE),
                                            add = list(mean = FALSE, tercile = FALSE, linear = FALSE),
                                            col = list(mean = "darkblue", tercile1 = "chartreuse4", tercile2 = "darkgoldenrod4", linear = "purple3"),
                                            text = list(mean = "Average", tercile1 = "Tercile 0.33333", tercile2 = "Tercile 0.66666", linear = "Trend line"),
                                            lwd = list(mean = 2, tercile = 2, linear = 2))
                                        ),
                                    bar.enso = list(
                                        xlim = list(is.min = FALSE, min = 1981, is.max = FALSE, max = 2017),
                                        ylim = list(is.min = FALSE, min = 0, is.max = FALSE, max = 100),
                                        axislabs = list(is.xlab = FALSE, xlab = '', is.ylab = FALSE, ylab = ''),
                                        title = list(is.title = FALSE, title = '', position = 'top'),
                                        colors = list(col = c("blue", "gray", "red"))
                                        ),
                                    proba.enso = list(
                                        xlim = list(is.min = FALSE, min = 0, is.max = FALSE, max = 100),
                                        ylim = list(is.min = FALSE, min = 0, is.max = FALSE, max = 100),
                                        axislabs = list(is.xlab = FALSE, xlab = '', is.ylab = FALSE, ylab = ''),
                                        title = list(is.title = FALSE, title = '', position = 'top'),
                                        plot = list(type = 'both', lwd = 2, cex = 1.4,
                                            all = list(line = "black", points = "lightgray"),
                                            nina = list(line = "blue", points = "lightblue"),
                                            neutre = list(line = "gray", points = "lightgray"),
                                            nino = list(line = "red", points = "lightpink"))
                                        )
                                )

    ###################

    # xml.dlg <- file.path(.cdtDir$dirLocal, "languages", "cdtSpatialAnalysis_leftCmd.xml")
    # lang.dlg <- cdtLanguageParse(xml.dlg, .cdtData$Config$lang.iso)
    # .cdtData$EnvData$message <- lang.dlg[['message']]
    lang.dlg <- NULL

    ###################

    .cdtEnv$tcl$main$cmd.frame <- tkframe(.cdtEnv$tcl$main$panel.left)

    tknote.cmd <- bwNoteBook(.cdtEnv$tcl$main$cmd.frame)
    cmd.tab1 <- bwAddTab(tknote.cmd, text = "Input")
    cmd.tab2 <- bwAddTab(tknote.cmd, text = "Analysis")
    cmd.tab3 <- bwAddTab(tknote.cmd, text = "Maps")
    cmd.tab4 <- bwAddTab(tknote.cmd, text = "Graphs")
    cmd.tab5 <- bwAddTab(tknote.cmd, text = "Boundaries")

    bwRaiseTab(tknote.cmd, cmd.tab1)

    tkgrid.columnconfigure(cmd.tab1, 0, weight = 1)
    tkgrid.columnconfigure(cmd.tab2, 0, weight = 1)
    tkgrid.columnconfigure(cmd.tab3, 0, weight = 1)
    tkgrid.columnconfigure(cmd.tab4, 0, weight = 1)
    tkgrid.columnconfigure(cmd.tab5, 0, weight = 1)

    tkgrid.rowconfigure(cmd.tab1, 0, weight = 1)
    tkgrid.rowconfigure(cmd.tab2, 0, weight = 1)
    tkgrid.rowconfigure(cmd.tab3, 0, weight = 1)
    tkgrid.rowconfigure(cmd.tab4, 0, weight = 1)
    tkgrid.rowconfigure(cmd.tab5, 0, weight = 1)

    #######################################################################################################

    #Tab1
    subfr1 <- bwTabScrollableFrame(cmd.tab1)

        #######################

        frameTimeS <- ttklabelframe(subfr1, text = "Time step of input data", relief = 'groove')

        timeSteps <- tclVar()
        CbperiodVAL <- .cdtEnv$tcl$lang$global[['combobox']][['1']][3:6]
        periodVAL <- c('daily', 'pentad', 'dekadal', 'monthly')
        tclvalue(timeSteps) <- CbperiodVAL[periodVAL %in% GeneralParameters$in.tstep]

        cb.fperiod <- ttkcombobox(frameTimeS, values = CbperiodVAL, textvariable = timeSteps, width = largeur1)

        tkgrid(cb.fperiod, row = 0, column = 0, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)

        helpWidget(cb.fperiod, 'Select the time step of the data', 'Select the time step of the data')

        #######################

        frameData <- ttklabelframe(subfr1, text = "Input Data", relief = 'groove')

        DataType <- tclVar()
        CbdatatypeVAL <- .cdtEnv$tcl$lang$global[['combobox']][['2']][1:2]
        datatypeVAL <- c('cdtstation', 'cdtdataset')
        tclvalue(DataType) <- CbdatatypeVAL[datatypeVAL %in% GeneralParameters$data.type]

        file.stnfl <- tclVar(GeneralParameters$in.file)

        if(GeneralParameters$data.type == 'cdtstation')
            txt.INData <- 'File containing stations input data'
        if(GeneralParameters$data.type == 'cdtdataset')
            txt.INData <- 'Index file (*.rds) of the dataset'
        fileINdir <- tclVar(txt.INData)


        txt.datatype <- tklabel(frameData, text = "Format", anchor = 'e', justify = 'right')
        cb.datatype <- ttkcombobox(frameData, values = CbdatatypeVAL, textvariable = DataType, width = largeur0)
        txt.stnfl <- tklabel(frameData, text = tclvalue(fileINdir), textvariable = fileINdir, anchor = 'w', justify = 'left')
        if(GeneralParameters$data.type == 'cdtstation'){
            cb.stnfl <- ttkcombobox(frameData, values = unlist(listOpenFiles), textvariable = file.stnfl, width = largeur1)
        }else{
            cb.stnfl <- tkentry(frameData, textvariable = file.stnfl, width = largeur2)
        }
        bt.stnfl <- tkbutton(frameData, text = "...")

        ###############
        tkgrid(txt.datatype, row = 0, column = 0, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
        tkgrid(cb.datatype, row = 0, column = 1, sticky = 'we', rowspan = 1, columnspan = 6, padx = 1, pady = 1, ipadx = 1, ipady = 1)
        tkgrid(txt.stnfl, row = 1, column = 0, sticky = 'we', rowspan = 1, columnspan = 7, padx = 1, pady = 0, ipadx = 1, ipady = 1)
        tkgrid(cb.stnfl, row = 2, column = 0, sticky = 'we', rowspan = 1, columnspan = 6, padx = 0, pady = 0, ipadx = 1, ipady = 1)
        tkgrid(bt.stnfl, row = 2, column = 6, sticky = 'w', rowspan = 1, columnspan = 1, padx = 0, pady = 0, ipadx = 1, ipady = 1)

        helpWidget(cb.datatype, 'Select the type of input data', 'Select the type of input data')
        helpWidget(cb.stnfl, 'Select the file in the list', 'Select the file containing the input data')
        helpWidget(bt.stnfl, 'Browse file if not listed', 'Browse file if not listed')

        ###############
        tkconfigure(bt.stnfl, command = function(){
            if(GeneralParameters$data.type == 'cdtstation'){
                dat.opfiles <- getOpenFiles(.cdtEnv$tcl$main$win)
                if(!is.null(dat.opfiles)){
                    update.OpenFiles('ascii', dat.opfiles)
                    listOpenFiles[[length(listOpenFiles) + 1]] <<- dat.opfiles[[1]]
                    tclvalue(file.stnfl) <- dat.opfiles[[1]]
                    tkconfigure(cb.stnfl, values = unlist(listOpenFiles))
                }
            }
            if(GeneralParameters$data.type == 'cdtdataset'){
                path.dataset <- tclvalue(tkgetOpenFile(initialdir = getwd(), filetypes = .cdtEnv$tcl$data$filetypes6))
                tclvalue(file.stnfl) <- if(path.dataset %in% c("", "NA") | is.na(path.dataset)) "" else path.dataset
            }

        })

        ###############

        tkbind(cb.datatype, "<<ComboboxSelected>>", function(){
            tkdestroy(cb.stnfl)
            tclvalue(file.stnfl) <- ''

            if(str_trim(tclvalue(DataType)) == CbdatatypeVAL[1]){
                tclvalue(fileINdir) <- 'File containing stations input data'

                cb.stnfl <<- ttkcombobox(frameData, values = unlist(listOpenFiles), textvariable = file.stnfl, width = largeur1)

                #######
                tkconfigure(bt.stnfl, command = function(){
                    dat.opfiles <- getOpenFiles(.cdtEnv$tcl$main$win)
                    if(!is.null(dat.opfiles)){
                        update.OpenFiles('ascii', dat.opfiles)
                        listOpenFiles[[length(listOpenFiles) + 1]] <<- dat.opfiles[[1]]
                        tclvalue(file.stnfl) <- dat.opfiles[[1]]
                        tkconfigure(cb.stnfl, values = unlist(listOpenFiles))
                    }
                })

                helpWidget(cb.stnfl, 'Select the file in the list', 'Select the file containing the input data')
            }

            if(str_trim(tclvalue(DataType)) == CbdatatypeVAL[2]){
                tclvalue(fileINdir) <- 'Index file (*.rds) of the dataset'

                cb.stnfl <<- tkentry(frameData, textvariable = file.stnfl, width = largeur2)

                #######
                tkconfigure(bt.stnfl, command = function(){
                    path.dataset <- tclvalue(tkgetOpenFile(initialdir = getwd(), filetypes = .cdtEnv$tcl$data$filetypes6))
                    tclvalue(file.stnfl) <- if(path.dataset %in% c("", "NA") | is.na(path.dataset)) "" else path.dataset
                })

                helpWidget(cb.stnfl, 'Enter the full path to the index file <dataset name>.rds', 'Enter the full path to the index file <dataset name>.rds')
            }
            
            #######
            tkgrid(cb.stnfl, row = 2, column = 0, sticky = 'we', rowspan = 1, columnspan = 6, padx = 0, pady = 0, ipadx = 1, ipady = 1)
        })

        #############################

        frameAggr <- ttklabelframe(subfr1, text = "Time series aggregation", relief = 'groove')

        AGGRFUN <- c("mean", "sum", "count")
        OPRFUN <- c(">=", ">", "<=", "<")

        aggr.fun <- tclVar(GeneralParameters$aggr.series$aggr.fun)
        min.frac <- tclVar(GeneralParameters$aggr.series$min.frac)
        opr.fun <- tclVar(GeneralParameters$aggr.series$opr.fun)
        opr.thres <- tclVar(GeneralParameters$aggr.series$opr.thres)

        stateo1 <- if(str_trim(GeneralParameters$aggr.series$aggr.fun) == "count") 'readonly' else 'disabled'
        stateo2 <- if(str_trim(GeneralParameters$aggr.series$aggr.fun) == "count") 'normal' else 'disabled'

        txt.aggfun <- tklabel(frameAggr, text = 'Function', anchor = 'w', justify = 'left')
        cb.aggfun <- ttkcombobox(frameAggr, values = AGGRFUN, textvariable = aggr.fun, width = 6, state = 'readonly')
        txt.minfrac <- tklabel(frameAggr, text = 'Min.Frac', anchor = 'w', justify = 'left')
        en.minfrac <- tkentry(frameAggr, textvariable = min.frac, width = 6)
        txt.opfun <- tklabel(frameAggr, text = 'Operator', anchor = 'w', justify = 'left')
        cb.opfun <- ttkcombobox(frameAggr, values = OPRFUN, textvariable = opr.fun, width = 6, state = stateo1)
        txt.opthres <- tklabel(frameAggr, text = 'Threshold', anchor = 'w', justify = 'left')
        en.opthres <- tkentry(frameAggr, textvariable = opr.thres, width = 6, width = 6, state = stateo2)

        tkgrid(txt.aggfun, row = 0, column = 0, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
        tkgrid(cb.aggfun, row = 0, column = 1, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
        tkgrid(txt.minfrac, row = 0, column = 2, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
        tkgrid(en.minfrac, row = 0, column = 3, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
        tkgrid(txt.opfun, row = 1, column = 0, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
        tkgrid(cb.opfun, row = 1, column = 1, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
        tkgrid(txt.opthres, row = 1, column = 2, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
        tkgrid(en.opthres, row = 1, column = 3, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)

        helpWidget(cb.aggfun, 'Function that have to be applied for aggregating from daily/dekadal/monthly into\na higher time step (e.g., for precipitation FUN=sum and for temperature FUN=mean)', 'Function that have to be applied for aggregating from daily/dekadal/monthly into\na higher time step (e.g., for precipitation FUN=sum and for temperature FUN=mean)')
        helpWidget(en.minfrac, 'Minimum fraction of available data that must be present within each output time step', 'Minimum fraction of available data that must be present within each output time step')
        helpWidget(cb.opfun, 'Select the comparison operator to be used to match event', 'Select the comparison operator to be used to match event')
        helpWidget(en.opthres, 'User defined threshold applied to count event', 'User defined threshold applied to count event')

        ##############
        tkbind(cb.aggfun, "<<ComboboxSelected>>", function(){
            stateo1 <- if(tclvalue(aggr.fun) == "count") "readonly" else "disabled"
            stateo2 <- if(tclvalue(aggr.fun) == "count") "normal" else "disabled"
            tkconfigure(cb.opfun, state = stateo1)
            tkconfigure(en.opthres, state = stateo2)
        })

        #############################
        frameDirSav <- tkframe(subfr1, relief = 'groove', borderwidth = 2)

        file.save1 <- tclVar(GeneralParameters$out.dir)

        txt.file.save <- tklabel(frameDirSav, text = "Directory to save results", anchor = 'w', justify = 'left')
        en.file.save <- tkentry(frameDirSav, textvariable = file.save1, width = largeur2)
        bt.file.save <- tkbutton(frameDirSav, text = "...")

        ##############
        tkgrid(txt.file.save, row = 0, column = 0, sticky = 'we', rowspan = 1, columnspan = 5, padx = 1, pady = 0, ipadx = 1, ipady = 1)
        tkgrid(en.file.save, row = 1, column = 0, sticky = 'we', rowspan = 1, columnspan = 4, padx = 0, pady = 0, ipadx = 1, ipady = 1)
        tkgrid(bt.file.save, row = 1, column = 4, sticky = 'w', rowspan = 1, columnspan = 1, padx = 0, pady = 0, ipadx = 1, ipady = 1)

        helpWidget(en.file.save, 'Enter the full path to directory to save outputs', 'Enter the full path to directory to save outputs')
        helpWidget(bt.file.save, 'or browse here', 'or browse here')

        ##############
        tkconfigure(bt.file.save, command = function() fileORdir2Save(file.save1, isFile = FALSE))

        #############################
        tkgrid(frameTimeS, row = 0, column = 0, sticky = '', pady = 1)
        tkgrid(frameData, row = 1, column = 0, sticky = 'we', pady = 3)
        tkgrid(frameAggr, row = 2, column = 0, sticky = 'we', pady = 3)
        tkgrid(frameDirSav, row = 3, column = 0, sticky = 'we', pady = 3)

    #######################################################################################################

    #Tab2
    subfr2 <- bwTabScrollableFrame(cmd.tab2)

        #######################

        frameYear <- ttklabelframe(subfr2, text = "Years to analyze", relief = 'groove')

        allYears <- tclVar(GeneralParameters$time.series$all.years)
        startYear <- tclVar(GeneralParameters$time.series$start.year)
        endYear <- tclVar(GeneralParameters$time.series$end.year)
        nseqYears <- tclVar(GeneralParameters$time.series$nseq.years)

        if(GeneralParameters$time.series$all.years){
            state0 <- 'disabled'
            state1 <- 'disabled'
            state2 <- 'disabled'
        }else{
            state0 <- if(!GeneralParameters$time.series$nseq.years) 'normal' else 'disabled'
            state1 <- 'normal'
            state2 <- if(GeneralParameters$time.series$nseq.years) 'normal' else 'disabled'
        }

        chk.allYears <- tkcheckbutton(frameYear, variable = allYears, text = "Use all years from the input data", anchor = 'w', justify = 'left')
        txt.startYear <- tklabel(frameYear, text = "Start Year", anchor = 'e', justify = 'right')
        en.startYear <- tkentry(frameYear, textvariable = startYear, width = 6, state = state0)
        txt.endYear <- tklabel(frameYear, text = "End Year", anchor = 'e', justify = 'right')
        en.endYear <- tkentry(frameYear, textvariable = endYear, width = 6, state = state0)
        chk.customYear <- tkcheckbutton(frameYear, variable = nseqYears, text = "Customized Years", anchor = 'w', justify = 'left', state = state1)
        bt.customYear <- tkbutton(frameYear, text = "Custom", state = state2)

        tkgrid(chk.allYears, row = 0, column = 0, sticky = 'we', rowspan = 1, columnspan = 7, padx = 1, pady = 1, ipadx = 1, ipady = 1)
        tkgrid(txt.startYear, row = 1, column = 0, sticky = 'e', rowspan = 1, columnspan = 2, padx = 0, pady = 1, ipadx = 1, ipady = 1)
        tkgrid(en.startYear, row = 1, column = 2, sticky = 'w', rowspan = 1, columnspan = 1, padx = 0, pady = 1, ipadx = 1, ipady = 1)
        tkgrid(txt.endYear, row = 1, column = 3, sticky = 'e', rowspan = 1, columnspan = 3, padx = 0, pady = 1, ipadx = 1, ipady = 1)
        tkgrid(en.endYear, row = 1, column = 6, sticky = 'w', rowspan = 1, columnspan = 1, padx = 0, pady = 1, ipadx = 1, ipady = 1)
        tkgrid(chk.customYear, row = 2, column = 0, sticky = 'we', rowspan = 1, columnspan = 5, padx = 1, pady = 1, ipadx = 1, ipady = 1)
        tkgrid(bt.customYear, row = 2, column = 5, sticky = 'w', rowspan = 1, columnspan = 2, padx = 1, pady = 1, ipadx = 1, ipady = 1)

        helpWidget(chk.allYears, "Check this box to use all years from the input data", "Check this box to use all years from the input data")
        helpWidget(en.startYear, "Enter the start year of the period to analyze", "Enter the start year of the period to analyze")
        helpWidget(en.endYear, "Enter the end year of the period to analyze", "Enter the end year of the period to analyze")
        helpWidget(chk.customYear, "Check this box if the years to analyze are not continuous", "Check this box if the years to analyze are not continuous")
        helpWidget(bt.customYear, "Edit the years to analyze", "Edit the years to analyze")

        ############

        tkconfigure(bt.customYear, command = function(){
            titre <- 'Years to analyze'
            help <- "Edit the years to analyze. The years need to be separated by commas. E.g., 1983, 1984, 1991, 1997, 1998, 2002, 2003, ..."
            years <- GeneralParameters$time.series$custom.years
            if(length(years) == 1 & is.na(years)) years <- ''
            years <- paste0(years, collapse = ', ')
            GeneralParameters$time.series$custom.years <<- spatialAnalysisEditYrsMon(.cdtEnv$tcl$main$win, years, titre, help, TRUE, lang.dlg)
        })

        ############
        tkbind(chk.allYears, "<Button-1>", function(){
            if(tclvalue(allYears) == '1'){
                state0 <- if(tclvalue(nseqYears) == '1') 'disabled' else 'normal'
                state1 <- 'normal'
                state2 <- if(tclvalue(nseqYears) == '1') 'normal' else 'disabled'
            }else{
                state0 <- 'disabled'
                state1 <- 'disabled'
                state2 <- if(tclvalue(nseqYears) == '1' & tclvalue(allYears) == '1') 'normal' else 'disabled'
            }

            tkconfigure(en.startYear, state = state0)
            tkconfigure(en.endYear, state = state0)
            tkconfigure(chk.customYear, state = state1)
            tkconfigure(bt.customYear, state = state2)
        })

        tkbind(chk.customYear, "<Button-1>", function(){
            if(tclvalue(allYears) == '0'){
                state0 <- if(tclvalue(nseqYears) == '1') 'normal' else 'disabled'
                state2 <- if(tclvalue(nseqYears) == '1') 'disabled' else 'normal'
                tkconfigure(en.startYear, state = state0)
                tkconfigure(en.endYear, state = state0)
                tkconfigure(bt.customYear, state = state2)
            }
        })

        #######################

        frameOut <- ttklabelframe(subfr2, text = "Output Time series", relief = 'groove')

        CboutSeriesVAL <- c('Monthly', 'Seasonal', 'Annual')
        outSeriesVAL <- c('monthly', 'seasonal', 'annual')
        out.series <- tclVar()
        tclvalue(out.series) <- CboutSeriesVAL[outSeriesVAL %in% GeneralParameters$time.series$out.series]

        mon1 <- as.numeric(str_trim(GeneralParameters$time.series$start.month))
        mon2 <- as.numeric(str_trim(GeneralParameters$time.series$end.month))
        startMonth <- tclVar(MOIS[mon1])
        nseqMonths <- tclVar(GeneralParameters$time.series$nseq.months)

        stateOut <- if(GeneralParameters$time.series$out.series == 'seasonal') "normal" else "disabled"

        if(GeneralParameters$time.series$out.series == 'monthly'){
            stateMon1 <- "normal"
            stateMon2 <- "normal"
            stateMon3 <- "normal"
            stateMon4 <- if(GeneralParameters$time.series$nseq.months) "normal" else "disabled"
        }else{
            stateMon1 <- "normal"
            stateMon2 <- "disabled"
            stateMon3 <- "disabled"
            stateMon4 <- "disabled"
            len <- if(GeneralParameters$time.series$out.series == 'seasonal') GeneralParameters$time.series$len.seas else 12
            mon2 <- (mon1 + len - 1) %% 12
            mon2[mon2 == 0] <- 12
        }
        endMonth <- tclVar(MOIS[mon2])

        txt.outTS <- tklabel(frameOut, text = "Time step", anchor = 'e', justify = 'right')
        cb.outTS <- ttkcombobox(frameOut, values = CboutSeriesVAL, textvariable = out.series, width = largeur3)
        txt.lenSeas <- tklabel(frameOut, text = 'Width')
        spin.lenSeas <- ttkspinbox(frameOut, from = 1, to = 12, increment = 1, justify = 'center', width = 2, state = stateOut)
        tkset(spin.lenSeas, GeneralParameters$time.series$len.seas)

        txt.Month <- tklabel(frameOut, text = "Months to process", anchor = 'w', justify = 'left')
        fr.Month <- tkframe(frameOut)

        txt.startMonth <- tklabel(fr.Month, text = "From", anchor = 'e', justify = 'right')
        cb.startMonth <- ttkcombobox(fr.Month, values = MOIS, textvariable = startMonth, width = largeur3, state = stateMon1)
        txt.endMonth <- tklabel(fr.Month, text = "to")
        cb.endMonth <- ttkcombobox(fr.Month, values = MOIS, textvariable = endMonth, width = largeur3, state = stateMon2)
        chk.customMonth <- tkcheckbutton(fr.Month, variable = nseqMonths, text = "Customized Months", anchor = 'w', justify = 'left', state = stateMon3)
        bt.customMonth <- tkbutton(fr.Month, text = "Custom", state = stateMon4)

        tkgrid(txt.startMonth, row = 0, column = 0, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
        tkgrid(cb.startMonth, row = 0, column = 1, sticky = 'we', rowspan = 1, columnspan = 3, padx = 1, pady = 1, ipadx = 1, ipady = 1)
        tkgrid(txt.endMonth, row = 0, column = 4, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
        tkgrid(cb.endMonth, row = 0, column = 5, sticky = 'we', rowspan = 1, columnspan = 3, padx = 1, pady = 1, ipadx = 1, ipady = 1)
        tkgrid(chk.customMonth, row = 1, column = 0, sticky = 'we', rowspan = 1, columnspan = 6, padx = 1, pady = 1, ipadx = 1, ipady = 1)
        tkgrid(bt.customMonth, row = 1, column = 6, sticky = 'e', rowspan = 1, columnspan = 2, padx = 1, pady = 1, ipadx = 1, ipady = 1)

        tkgrid(txt.outTS, row = 0, column = 0, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
        tkgrid(cb.outTS, row = 0, column = 1, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
        tkgrid(txt.lenSeas, row = 0, column = 2, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
        tkgrid(spin.lenSeas, row = 0, column = 3, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
        tkgrid(txt.Month, row = 1, column = 0, sticky = 'we', rowspan = 1, columnspan = 4, padx = 1, pady = 1, ipadx = 1, ipady = 1)
        tkgrid(fr.Month, row = 2, column = 0, sticky = '', rowspan = 1, columnspan = 4, padx = 1, pady = 1, ipadx = 1, ipady = 1)

        helpWidget(cb.outTS, "Select the time step of the output time series", "Select the time step of the output time series")
        helpWidget(spin.lenSeas, 'Select the width of the season [in month] (e.g., 3 for three-month season)', 'Select the width of the season [in month] (e.g., 3 for three-month season)')
        helpWidget(cb.startMonth, 'Select the start month', 'Select the start month')
        helpWidget(chk.customMonth, "Check this box if the months to process are not consecutive", "Check this box if the months to process are not consecutive")
        helpWidget(bt.customMonth, "Edit the months to process", "Edit the months to process")

        ##############

        tkconfigure(bt.customMonth, command = function(){
            titre <- 'Months to process'
            help <- "Edit the months to be included in the analysis. A month number must be between 1 and 12. The months need to be separated by commas. E.g., 1, 2, 3, 7, 8, 9"
            months <- GeneralParameters$time.series$custom.months
            months <- paste0(months, collapse = ', ')
            GeneralParameters$time.series$custom.months <<- spatialAnalysisEditYrsMon(.cdtEnv$tcl$main$win, months, titre, help, FALSE, lang.dlg)
        })

        ##############

        tkbind(cb.outTS, "<<ComboboxSelected>>", function(){
            stateOut <- if(str_trim(tclvalue(out.series)) == CboutSeriesVAL[2]) "normal" else "disabled"
            tkconfigure(spin.lenSeas, state = stateOut)

            if(str_trim(tclvalue(out.series)) == CboutSeriesVAL[1]){
                stateMon1 <- "normal"
                stateMon2 <- "normal"
                stateMon3 <- "normal"
                stateMon4 <- if(tclvalue(nseqMonths) == '1') "normal" else "disabled"
            }else{
                stateMon1 <- "normal"
                stateMon2 <- "disabled"
                stateMon3 <- "disabled"
                stateMon4 <- "disabled"
            }
            tkconfigure(cb.startMonth, state = stateMon1)
            tkconfigure(cb.endMonth, state = stateMon2)
            tkconfigure(chk.customMonth, state = stateMon3)
            tkconfigure(bt.customMonth, state = stateMon4)

            if(str_trim(tclvalue(out.series)) %in% CboutSeriesVAL[2:3]){
                len <- if(str_trim(tclvalue(out.series)) == CboutSeriesVAL[2]) as.numeric(str_trim(tclvalue(tkget(spin.lenSeas)))) else 12
                mon1 <- which(MOIS %in% str_trim(tclvalue(startMonth)))
                mon2 <- (mon1 + len - 1) %% 12
                mon2[mon2 == 0] <- 12
                tclvalue(endMonth) <- MOIS[mon2]
            }
        })

        tkbind(spin.lenSeas, "<ButtonRelease-1>", function(){
            if(str_trim(tclvalue(out.series)) == CboutSeriesVAL[2]){
                len <- as.numeric(str_trim(tclvalue(tkget(spin.lenSeas))))
                mon1 <- which(MOIS %in% str_trim(tclvalue(startMonth)))
                mon2 <- (mon1 + len - 1) %% 12
                mon2[mon2 == 0] <- 12
                tclvalue(endMonth) <- MOIS[mon2]
            }
        })

        tkbind(spin.lenSeas, "<FocusOut>", function(){
            if(str_trim(tclvalue(out.series)) == CboutSeriesVAL[2]){
                len <- as.numeric(str_trim(tclvalue(tkget(spin.lenSeas))))
                mon1 <- which(MOIS %in% str_trim(tclvalue(startMonth)))
                mon2 <- (mon1 + len - 1) %% 12
                mon2[mon2 == 0] <- 12
                tclvalue(endMonth) <- MOIS[mon2]
            }
        })

        tkbind(chk.customMonth, "<Button-1>", function(){
            if(str_trim(tclvalue(out.series)) == CboutSeriesVAL[1]){
                stateMon <- if(tclvalue(nseqMonths) == '1') 'normal' else 'disabled'
                stateMon4 <- if(tclvalue(nseqMonths) == '1') 'disabled' else 'normal'
                tkconfigure(cb.startMonth, state = stateMon)
                tkconfigure(cb.endMonth, state = stateMon)
                tkconfigure(bt.customMonth, state = stateMon4)
            }
        })

        tkbind(cb.startMonth, "<<ComboboxSelected>>", function(){
            if(str_trim(tclvalue(out.series)) %in% CboutSeriesVAL[2:3]){
                len <- if(tclvalue(out.series) == CboutSeriesVAL[2]) as.numeric(str_trim(tclvalue(tkget(spin.lenSeas)))) else 12
                mon1 <- which(MOIS %in% str_trim(tclvalue(startMonth)))
                mon2 <- (mon1 + len - 1) %% 12
                mon2[mon2 == 0] <- 12
                tclvalue(endMonth) <- MOIS[mon2]
            }
        })

        #######################

        frameAnalysis <- ttklabelframe(subfr2, text = "Analysis Method", relief = 'groove')

        ANALYSIS <- c('Mean', 'Median', 'Standard deviation', 'Coefficient of variation',
                      'Trend', 'Percentiles', 'Frequency', 'Anomaly')
        Analyses <- c('mean', 'median', 'std', 'cv', 'trend', 'percentile', 'frequency', 'anomaly')
        analysis.method <- tclVar()
        tclvalue(analysis.method) <- ANALYSIS[Analyses %in% GeneralParameters$analysis.method$mth.fun]

        cb.anMthd <- ttkcombobox(frameAnalysis, values = ANALYSIS, textvariable = analysis.method, width = largeur4)
        fr.anMthd <- tkframe(frameAnalysis)

        trend.unit <- tclVar(GeneralParameters$analysis.method$trend.unit)
        if(str_trim(tclvalue(analysis.method)) == ANALYSIS[5]){
            rd.trend1 <- tkradiobutton(fr.anMthd, text = "change (trend) / year", variable = trend.unit, value = "1", anchor = 'w', justify = 'left')
            rd.trend2 <- tkradiobutton(fr.anMthd, text = "change (trend) over the period", variable = trend.unit, value = "2", anchor = 'w', justify = 'left')
            rd.trend3 <- tkradiobutton(fr.anMthd, text = "change (trend) / average (in %)", variable = trend.unit, value = "3", anchor = 'w', justify = 'left')

            tkgrid(rd.trend1, sticky = 'we')
            tkgrid(rd.trend2, sticky = 'we')
            tkgrid(rd.trend3, sticky = 'we')
        }

        mth.perc <- tclVar(GeneralParameters$analysis.method$mth.perc)
        if(str_trim(tclvalue(analysis.method)) == ANALYSIS[6]){
            txt.Percent <- tklabel(fr.anMthd, text = "Percentile", anchor = 'e', justify = 'right')
            en.Percent <- tkentry(fr.anMthd, textvariable = mth.perc, width = 4)
            th.Percent <- tklabel(fr.anMthd, text = "th", anchor = 'w', justify = 'left')

            tkgrid(txt.Percent, en.Percent, th.Percent)

            helpWidget(en.Percent, "Enter the nth percentile to be calculated", "Enter the nth percentile to be calculated")
        }

        low.thres <- tclVar(GeneralParameters$analysis.method$low.thres)
        up.thres <- tclVar(GeneralParameters$analysis.method$up.thres)
        if(str_trim(tclvalue(analysis.method)) == ANALYSIS[7]){
            txt.Freq0 <- tklabel(fr.anMthd, text = "Frequency", anchor = 'w', justify = 'left')
            txt.Freq1 <- tklabel(fr.anMthd, text = "Between", anchor = 'e', justify = 'right')
            en.Freq1 <- tkentry(fr.anMthd, textvariable = low.thres, width = 5)
            txt.Freq2 <- tklabel(fr.anMthd, text = "And")
            en.Freq2 <- tkentry(fr.anMthd, textvariable = up.thres, width = 5)

            tkgrid(txt.Freq0, sticky = 'we', columnspan = 4)
            tkgrid(txt.Freq1, en.Freq1, txt.Freq2, en.Freq2)

            helpWidget(en.Freq1, "Enter the lower bound of the interval to count the number of occurrences", "Enter the lower bound of the interval to count the number of occurrences")
            helpWidget(en.Freq2, "Enter the upper bound of the interval to count the number of occurrences", "Enter the upper bound of the interval to count the number of occurrences")
        }

        perc.anom <- tclVar(GeneralParameters$analysis.method$perc.anom)
        startYr.anom <- tclVar(GeneralParameters$analysis.method$startYr.anom)
        endYr.anom <- tclVar(GeneralParameters$analysis.method$endYr.anom)
        if(str_trim(tclvalue(analysis.method)) == ANALYSIS[8]){
            txt.Anom0 <- tklabel(fr.anMthd, text = "Anomaly", anchor = 'w', justify = 'left')
            chk.Anom <- tkcheckbutton(fr.anMthd, variable = perc.anom, text = "Percentage of mean", anchor = 'w', justify = 'left')

            fr.Anom <- ttklabelframe(fr.anMthd, text = "Base period", relief = 'groove')
            txt.Anom1 <- tklabel(fr.Anom, text = "Start Year", anchor = 'e', justify = 'right')
            txt.Anom2 <- tklabel(fr.Anom, text = "End Year", anchor = 'e', justify = 'right')
            en.Anom1 <- tkentry(fr.Anom, textvariable = startYr.anom, width = 5)
            en.Anom2 <- tkentry(fr.Anom, textvariable = endYr.anom, width = 5)

            tkgrid(txt.Anom0, chk.Anom)
            tkgrid(txt.Anom1, en.Anom1, txt.Anom2, en.Anom2)
            tkgrid(fr.Anom, sticky = 'we', columnspan = 2)

            helpWidget(chk.Anom, "Check this box to calculate the anomaly as percentage of mean", "Check this box to calculate the anomaly as percentage of mean")
            helpWidget(fr.Anom, "Enter the start and end year to be used to calculate the climatology", "Enter the start and end year to be used to calculate the climatology")
        }

        tkgrid(cb.anMthd, row = 0, column = 0, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
        tkgrid(fr.anMthd, row = 1, column = 0, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)

        helpWidget(cb.anMthd, "Select the analysis method", "Select the analysis method")

        #################
        tkbind(cb.anMthd, "<<ComboboxSelected>>", function(){
            tkdestroy(fr.anMthd)
            fr.anMthd <<- tkframe(frameAnalysis)

            if(str_trim(tclvalue(analysis.method)) == ANALYSIS[5]){
                rd.trend1 <<- tkradiobutton(fr.anMthd, text = "change (trend) / year", variable = trend.unit, value = "1", anchor = 'w', justify = 'left')
                rd.trend2 <<- tkradiobutton(fr.anMthd, text = "change (trend) over the period", variable = trend.unit, value = "2", anchor = 'w', justify = 'left')
                rd.trend3 <<- tkradiobutton(fr.anMthd, text = "change (trend) / average (in %)", variable = trend.unit, value = "3", anchor = 'w', justify = 'left')

                tkgrid(rd.trend1, sticky = 'we')
                tkgrid(rd.trend2, sticky = 'we')
                tkgrid(rd.trend3, sticky = 'we')
            }

            if(str_trim(tclvalue(analysis.method)) == ANALYSIS[6]){
                txt.Percent <- tklabel(fr.anMthd, text = "Percentile", anchor = 'e', justify = 'right')
                en.Percent <<- tkentry(fr.anMthd, textvariable = mth.perc, width = 4)
                th.Percent <- tklabel(fr.anMthd, text = "th", anchor = 'w', justify = 'left')

                tkgrid(txt.Percent, en.Percent, th.Percent)

                helpWidget(en.Percent, "Enter the nth percentile to be calculated", "Enter the nth percentile to be calculated")
            }
            if(str_trim(tclvalue(analysis.method)) == ANALYSIS[7]){
                txt.Freq0 <- tklabel(fr.anMthd, text = "Frequency", anchor = 'w', justify = 'left')
                txt.Freq1 <- tklabel(fr.anMthd, text = "Between", anchor = 'e', justify = 'right')
                en.Freq1 <<- tkentry(fr.anMthd, textvariable = low.thres, width = 5)
                txt.Freq2 <- tklabel(fr.anMthd, text = "And")
                en.Freq2 <<- tkentry(fr.anMthd, textvariable = up.thres, width = 5)

                tkgrid(txt.Freq0, sticky = 'we', columnspan = 4)
                tkgrid(txt.Freq1, en.Freq1, txt.Freq2, en.Freq2)

                helpWidget(en.Freq1, "Enter the lower bound of the interval to count the number of occurrences", "Enter the lower bound of the interval to count the number of occurrences")
                helpWidget(en.Freq2, "Enter the upper bound of the interval to count the number of occurrences", "Enter the upper bound of the interval to count the number of occurrences")
            }
            if(str_trim(tclvalue(analysis.method)) == ANALYSIS[8]){
                txt.Anom0 <- tklabel(fr.anMthd, text = "Anomaly", anchor = 'w', justify = 'left')
                chk.Anom <<- tkcheckbutton(fr.anMthd, variable = perc.anom, text = "Percentage of mean", anchor = 'w', justify = 'left')

                fr.Anom <- ttklabelframe(fr.anMthd, text = "Base period", relief = 'groove')
                txt.Anom1 <- tklabel(fr.Anom, text = "Start Year", anchor = 'e', justify = 'right')
                txt.Anom2 <- tklabel(fr.Anom, text = "End Year", anchor = 'e', justify = 'right')
                en.Anom1 <<- tkentry(fr.Anom, textvariable = startYr.anom, width = 5)
                en.Anom2 <<- tkentry(fr.Anom, textvariable = endYr.anom, width = 5)

                tkgrid(txt.Anom0, chk.Anom)
                tkgrid(txt.Anom1, en.Anom1, txt.Anom2, en.Anom2)
                tkgrid(fr.Anom, sticky = 'we', columnspan = 2)

                helpWidget(chk.Anom, "Check this box to calculate the anomaly as percentage of mean", "Check this box to calculate the anomaly as percentage of mean")
                helpWidget(fr.Anom, "Enter the start and end year to be used to calculate the climatology", "Enter the start and end year to be used to calculate the climatology")
            }

            tkgrid(fr.anMthd, row = 1, column = 0, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
        })

        #############################

        AnalyzeBut <- ttkbutton(subfr2, text = "Calculate")

        #################
        tkconfigure(AnalyzeBut, command = function(){
            GeneralParameters$data.type <- datatypeVAL[CbdatatypeVAL %in% str_trim(tclvalue(DataType))]
            GeneralParameters$in.tstep <- periodVAL[CbperiodVAL %in% str_trim(tclvalue(timeSteps))]

            GeneralParameters$in.file <- str_trim(tclvalue(file.stnfl))
            GeneralParameters$out.dir <- str_trim(tclvalue(file.save1))

            GeneralParameters$aggr.series$aggr.fun <- str_trim(tclvalue(aggr.fun))
            GeneralParameters$aggr.series$min.frac <- as.numeric(str_trim(tclvalue(min.frac)))
            GeneralParameters$aggr.series$opr.fun <- str_trim(tclvalue(opr.fun))
            GeneralParameters$aggr.series$opr.thres <- as.numeric(str_trim(tclvalue(opr.thres)))

            GeneralParameters$time.series$all.years <- switch(tclvalue(allYears), '0' = FALSE, '1' = TRUE)
            if(!GeneralParameters$time.series$all.years){
                GeneralParameters$time.series$nseq.years <- switch(tclvalue(nseqYears), '0' = FALSE, '1' = TRUE)
                if(GeneralParameters$time.series$nseq.years){
                    if(length(GeneralParameters$time.series$custom.years) < 1){
                        Insert.Messages.Out("No years selected.", format = TRUE)
                        return(NULL)
                    }
                    if(any(is.na(GeneralParameters$time.series$custom.years))){
                        Insert.Messages.Out("The edited years contain missing values.", format = TRUE)
                        return(NULL)
                    }
                }else{
                    GeneralParameters$time.series$start.year <- as.numeric(str_trim(tclvalue(startYear)))
                    GeneralParameters$time.series$end.year <- as.numeric(str_trim(tclvalue(endYear)))
                    if(is.na(GeneralParameters$time.series$start.year) | is.na(GeneralParameters$time.series$end.year)){
                        Insert.Messages.Out("Start or end year are missing.", format = TRUE)
                        return(NULL)
                    }
                }
            }

            GeneralParameters$time.series$out.series <- outSeriesVAL[CboutSeriesVAL %in% str_trim(tclvalue(out.series))]

            GeneralParameters$time.series$len.seas <- as.numeric(str_trim(tclvalue(tkget(spin.lenSeas))))
            GeneralParameters$time.series$start.month <- which(MOIS %in% str_trim(tclvalue(startMonth)))
            GeneralParameters$time.series$end.month <- which(MOIS %in% str_trim(tclvalue(endMonth)))
            GeneralParameters$time.series$nseq.months <- switch(tclvalue(nseqMonths), '0' = FALSE, '1' = TRUE)

            if(GeneralParameters$time.series$out.series == 'monthly'){
                if(GeneralParameters$time.series$nseq.months){
                    if(length(GeneralParameters$time.series$custom.months) < 1){
                        Insert.Messages.Out("No months selected.", format = TRUE)
                        return(NULL)
                    }
                    if(any(is.na(GeneralParameters$time.series$custom.months))){
                        Insert.Messages.Out("The edited months contain missing values.", format = TRUE)
                        return(NULL)
                    }
                }
            }

            GeneralParameters$analysis.method$mth.fun <- Analyses[ANALYSIS %in% str_trim(tclvalue(analysis.method))]

            if(GeneralParameters$analysis.method$mth.fun == 'trend'){
                GeneralParameters$analysis.method$trend.unit <- as.numeric(tclvalue(trend.unit))
            }

            if(GeneralParameters$analysis.method$mth.fun == 'percentile'){
                GeneralParameters$analysis.method$mth.perc <- as.numeric(str_trim(tclvalue(mth.perc)))
                if(is.na(GeneralParameters$analysis.method$mth.perc)){
                    Insert.Messages.Out("Percentile is missing.", format = TRUE)
                    return(NULL)
                }
            }

            if(GeneralParameters$analysis.method$mth.fun == 'frequency'){
                GeneralParameters$analysis.method$low.thres <- as.numeric(str_trim(tclvalue(low.thres)))
                GeneralParameters$analysis.method$up.thres <- as.numeric(str_trim(tclvalue(up.thres)))
                if(is.na(GeneralParameters$analysis.method$low.thres) | is.na(GeneralParameters$analysis.method$up.thres)){
                    Insert.Messages.Out("Lower or upper bounds of the interval for frequency are missing.", format = TRUE)
                    return(NULL)
                }
            }

            if(GeneralParameters$analysis.method$mth.fun == 'anomaly'){
                GeneralParameters$analysis.method$perc.anom <- switch(tclvalue(perc.anom), '0' = FALSE, '1' = TRUE)
                GeneralParameters$analysis.method$startYr.anom <- as.numeric(str_trim(tclvalue(startYr.anom)))
                GeneralParameters$analysis.method$endYr.anom <- as.numeric(str_trim(tclvalue(endYr.anom)))
                if(is.na(GeneralParameters$analysis.method$startYr.anom) | is.na(GeneralParameters$analysis.method$endYr.anom)){
                    Insert.Messages.Out("Base period to compute climatology for anomaly is missing.", format = TRUE)
                    return(NULL)
                }
            }

            # assign("GeneralParameters", GeneralParameters, envir = .GlobalEnv)

            Insert.Messages.Out(paste("Calculating", tclvalue(analysis.method), "......."), TRUE, "i")

            tkconfigure(.cdtEnv$tcl$main$win, cursor = 'watch')
            tcl('update')
            ret <- tryCatch(
                {
                    spatialAnalysisProcs(GeneralParameters)
                },
                warning = function(w) warningFun(w),
                error = function(e) errorFun(e),
                finally = {
                    tkconfigure(.cdtEnv$tcl$main$win, cursor = '')
                    tcl('update')
                }
            )

            msg0 <- paste(tclvalue(analysis.method), "calculation finished successfully")
            msg1 <- paste(tclvalue(analysis.method), "calculation failed")

            if(!is.null(ret)){
                if(ret == 0){
                    Insert.Messages.Out(msg0, TRUE, "s")

                    ###################

                    load.SpatialAnalysis.Data()

                }else Insert.Messages.Out(msg1, format = TRUE)
            }else Insert.Messages.Out(msg1, format = TRUE)
        })

        ##############################################
        tkgrid(frameYear, row = 0, column = 0, sticky = '')
        tkgrid(frameOut, row = 1, column = 0, sticky = 'we', pady = 3)
        tkgrid(frameAnalysis, row = 2, column = 0, sticky = 'we', pady = 3)
        tkgrid(AnalyzeBut, row = 3, column = 0, sticky = 'we', pady = 3)

    #######################################################################################################

    #Tab3
    subfr3 <- bwTabScrollableFrame(cmd.tab3)

        ##############################################

        frameAnDat <- ttklabelframe(subfr3, text = "Analysis data", relief = 'groove')

        .cdtData$EnvData$DirExist <- tclVar(0)
        file.Stat <- tclVar()

        statedirStat <- if(tclvalue(.cdtData$EnvData$DirExist) == "1") "normal" else "disabled"

        chk.dirStat <- tkcheckbutton(frameAnDat, variable = .cdtData$EnvData$DirExist, text = "Analysis already computed", anchor = 'w', justify = 'left')
        en.dirStat <- tkentry(frameAnDat, textvariable = file.Stat, width = largeur2, state = statedirStat)
        bt.dirStat <- tkbutton(frameAnDat, text = "...", state = statedirStat)

        tkgrid(chk.dirStat, row = 0, column = 0, sticky = 'we', rowspan = 1, columnspan = 5, padx = 1, pady = 1, ipadx = 1, ipady = 1)
        tkgrid(en.dirStat, row = 1, column = 0, sticky = 'we', rowspan = 1, columnspan = 4, padx = 1, pady = 1, ipadx = 1, ipady = 1)
        tkgrid(bt.dirStat, row = 1, column = 4, sticky = 'w', rowspan = 1, columnspan = 1, padx = 0, pady = 1, ipadx = 1, ipady = 1)

        ###############

        tkconfigure(bt.dirStat, command = function(){
            path.Stat <- tclvalue(tkgetOpenFile(initialdir = getwd(), filetypes = .cdtEnv$tcl$data$filetypes6))
            if(path.Stat %in% c("", "NA") | is.na(path.Stat)) return(NULL)
            tclvalue(file.Stat) <- path.Stat

            if(file.exists(str_trim(tclvalue(file.Stat)))){
                dirAnalysis <- try(readRDS(str_trim(tclvalue(file.Stat))), silent = TRUE)
                if(inherits(dirAnalysis, "try-error")){
                    Insert.Messages.Out('Unable to load Climate Analysis data', format = TRUE)
                    Insert.Messages.Out(gsub('[\r\n]', '', dirAnalysis[1]), format = TRUE)
                    tkconfigure(cb.climato.maps, values = "")
                    tclvalue(.cdtData$EnvData$climStat) <- ""
                    return(NULL)
                }

                .cdtData$EnvData$DirStat <- dirAnalysis
                .cdtData$EnvData$PathStat <- dirname(str_trim(tclvalue(file.Stat)))

                ###################

                load.SpatialAnalysis.Data()
            }
        })

        ###############
        tkbind(chk.dirStat, "<Button-1>", function(){
            statedirStat <- if(tclvalue(.cdtData$EnvData$DirExist) == '1') 'disabled' else 'normal'
            tkconfigure(en.dirStat, state = statedirStat)
            tkconfigure(bt.dirStat, state = statedirStat)

            stateAnaBut <- if(tclvalue(.cdtData$EnvData$DirExist) == '1') 'normal' else 'disabled'
            tkconfigure(AnalyzeBut, state = stateAnaBut)
            tkconfigure(cb.fperiod, state = stateAnaBut)
            tkconfigure(cb.datatype, state = stateAnaBut)
            tkconfigure(cb.stnfl, state = stateAnaBut)
            tkconfigure(bt.stnfl, state = stateAnaBut)
            tkconfigure(cb.aggfun, state = stateAnaBut)
            tkconfigure(en.minfrac, state = stateAnaBut)
            tkconfigure(en.file.save, state = stateAnaBut)
            tkconfigure(bt.file.save, state = stateAnaBut)
            tkconfigure(chk.allYears, state = stateAnaBut)
            tkconfigure(cb.outTS, state = stateAnaBut)
            tkconfigure(cb.anMthd, state = stateAnaBut)
            if(str_trim(tclvalue(analysis.method)) == ANALYSIS[5]){
                tkconfigure(rd.trend1, state = stateAnaBut)
                tkconfigure(rd.trend2, state = stateAnaBut)
                tkconfigure(rd.trend3, state = stateAnaBut)
            }
            if(str_trim(tclvalue(analysis.method)) == ANALYSIS[6])
                tkconfigure(en.Percent, state = stateAnaBut)

            if(str_trim(tclvalue(analysis.method)) == ANALYSIS[7]){
                tkconfigure(en.Freq1, state = stateAnaBut)
                tkconfigure(en.Freq2, state = stateAnaBut)
            }
            if(str_trim(tclvalue(analysis.method)) == ANALYSIS[8]){
                tkconfigure(chk.Anom, state = stateAnaBut)
                tkconfigure(en.Anom1, state = stateAnaBut)
                tkconfigure(en.Anom2, state = stateAnaBut)
            }

            if(tclvalue(.cdtData$EnvData$DirExist) == '1'){
                stateo1 <- if(tclvalue(aggr.fun) == "count") "readonly" else "disabled"
                stateo2 <- if(tclvalue(aggr.fun) == "count") "normal" else "disabled"

                if(tclvalue(allYears) == '0'){
                    state0 <- if(tclvalue(nseqYears) == '1') 'disabled' else 'normal'
                    state1 <- 'normal'
                    state2 <- if(tclvalue(nseqYears) == '1') 'normal' else 'disabled'
                }else{
                    state0 <- 'disabled'
                    state1 <- 'disabled'
                    state2 <- if(tclvalue(nseqYears) == '1' & tclvalue(allYears) == '1') 'normal' else 'disabled'
                }

                stateOut <- if(str_trim(tclvalue(out.series)) == CboutSeriesVAL[2]) "normal" else "disabled"

                if(str_trim(tclvalue(out.series)) == CboutSeriesVAL[1]){
                    stateMon1 <- "normal"
                    stateMon2 <- "normal"
                    stateMon3 <- "normal"
                    stateMon4 <- if(tclvalue(nseqMonths) == '1') "normal" else "disabled"
                }else{
                    stateMon1 <- "normal"
                    stateMon2 <- "disabled"
                    stateMon3 <- "disabled"
                    stateMon4 <- "disabled"
                }
            }else{
                stateo1 <- "disabled"
                stateo2 <- "disabled"
                state0 <- "disabled"
                state1 <- "disabled"
                state2 <- "disabled"
                stateOut <- "disabled"
                stateMon1 <- "disabled"
                stateMon2 <- "disabled"
                stateMon3 <- "disabled"
                stateMon4 <- "disabled"
            }

            tkconfigure(cb.opfun, state = stateo1)
            tkconfigure(en.opthres, state = stateo2)
            tkconfigure(en.startYear, state = state0)
            tkconfigure(en.endYear, state = state0)
            tkconfigure(chk.customYear, state = state1)
            tkconfigure(bt.customYear, state = state2)
            tkconfigure(spin.lenSeas, state = stateOut)
            tkconfigure(cb.startMonth, state = stateMon1)
            tkconfigure(cb.endMonth, state = stateMon2)
            tkconfigure(chk.customMonth, state = stateMon3)
            tkconfigure(bt.customMonth, state = stateMon4)
        })

        ##############################################

        frameClimatoMap <- ttklabelframe(subfr3, text = "Statistics Maps", relief = 'groove')

        .cdtData$EnvData$climStat <- tclVar()
        .cdtData$EnvData$climDate <- tclVar()

        cb.climato.maps <- ttkcombobox(frameClimatoMap, values = "", textvariable = .cdtData$EnvData$climStat, width = largeur5)
        bt.climato.maps <- ttkbutton(frameClimatoMap, text = .cdtEnv$tcl$lang$global[['button']][['3']])
        cb.climDate <- ttkcombobox(frameClimatoMap, values = "", textvariable = .cdtData$EnvData$climDate, width = largeur6, justify = 'center')
        bt.climMapOpt <- ttkbutton(frameClimatoMap, text = .cdtEnv$tcl$lang$global[['button']][['4']])

        ###################
        tkgrid(cb.climato.maps, row = 0, column = 0, sticky = 'we', rowspan = 1, columnspan = 4, padx = 1, pady = 1, ipadx = 1, ipady = 1)
        tkgrid(bt.climato.maps, row = 0, column = 4, sticky = '', rowspan = 1, columnspan = 2, padx = 1, pady = 1, ipadx = 1, ipady = 1)
        tkgrid(cb.climDate, row = 1, column = 0, sticky = 'we', rowspan = 1, columnspan = 2, padx = 1, pady = 1, ipadx = 1, ipady = 1)

        tkgrid(bt.climMapOpt, row = 1, column = 4, sticky = 'we', rowspan = 1, columnspan = 2, padx = 1, pady = 1, ipadx = 1, ipady = 1)

        ###################

        tkconfigure(bt.climMapOpt, command = function(){
            if(!is.null(.cdtData$EnvData$don)){
                atlevel <- pretty(.cdtData$EnvData$don$z, n = 10, min.n = 7)
                if(is.null(.cdtData$EnvData$climMapOp$userLvl$levels)){
                    .cdtData$EnvData$climMapOp$userLvl$levels <- atlevel
                }else{
                    if(!.cdtData$EnvData$climMapOp$userLvl$custom)
                        .cdtData$EnvData$climMapOp$userLvl$levels <- atlevel
                }
            }
            .cdtData$EnvData$climMapOp <- MapGraph.MapOptions(.cdtData$EnvData$climMapOp)

            if(str_trim(tclvalue(.cdtData$EnvData$plot.maps$plot.type)) == "Points")
                .cdtData$EnvData$tab$pointSize.climMap <- .cdtData$EnvData$climMapOp$pointSize
        })

        .cdtData$EnvData$tab$climMap <- NULL

        tkconfigure(bt.climato.maps, command = function(){
            if(is.null(.cdtData$EnvData$statpars)) return(NULL)

            ret <- read.ClimStat()
            if(is.null(ret)) return(NULL)

            if(!str_trim(tclvalue(.cdtData$EnvData$climStat)) %in% c("", "Anomaly"))
                spatialAnalysis.DisplayStatMaps()
        })

        ###############

        tkbind(cb.climato.maps, "<<ComboboxSelected>>", function(){
            analysis.path <- file.path(.cdtData$EnvData$PathStat, str_trim(tclvalue(.cdtData$EnvData$climStat)))
            params <- try(readRDS(file.path(analysis.path, "params.rds")), silent = TRUE)
            if(inherits(params, "try-error")){
                Insert.Messages.Out('Unable to load Climate Analysis data', format = TRUE)
                Insert.Messages.Out(gsub('[\r\n]', '', params[1]), format = TRUE)
                tkconfigure(cb.climDate, values = "")
                tclvalue(.cdtData$EnvData$climDate) <- ""
                return(NULL)
            }
            tkconfigure(cb.climDate, values = params$stats)
            tclvalue(.cdtData$EnvData$climDate) <- params$stats[1]
            stateclimDate <- if(params$params$time.series$out.series == "monthly") "normal" else "disabled"
            tkconfigure(cb.climDate, state = stateclimDate)

            .cdtData$EnvData$statpars <- params

            ###################
            ret <- read.ClimStat()
            if(is.null(ret)) return(NULL)

            ###################
            tkconfigure(cb.TSDate, values = params$timeseries[[1]][[2]])
            tclvalue(.cdtData$EnvData$TSDate) <- params$timeseries[[1]][[2]][1]

            ###################
            if(!is.null(.cdtData$EnvData$don)){
                atlevel <- pretty(.cdtData$EnvData$don$z, n = 10, min.n = 7)
                .cdtData$EnvData$climMapOp$userLvl$levels <- atlevel
            }

            ###################
            ret1 <- read.ClimTSData()
            if(is.null(ret1)) return(NULL)
        })

        ###############

        tkbind(cb.climDate, "<<ComboboxSelected>>", function(){
            ret <- read.ClimStat()
            if(is.null(ret)) return(NULL)

            ###################
            ipos <- which(.cdtData$EnvData$statpars$stats == tclvalue(.cdtData$EnvData$climDate))
            tkconfigure(cb.TSDate, values = .cdtData$EnvData$statpars$timeseries[[ipos]][[2]])
            tclvalue(.cdtData$EnvData$TSDate) <- .cdtData$EnvData$statpars$timeseries[[ipos]][[2]][1]

            ###################
            ret1 <- read.ClimTSData()
            if(is.null(ret1)) return(NULL)
        })

        ##############################################

        frameTSMaps <- ttklabelframe(subfr3, text = "Monthly/Seasonal/Annual Maps", relief = 'groove')

        .cdtData$EnvData$TSDate <- tclVar()
        .cdtData$EnvData$TSData <- tclVar("Data")

        cb.TSDate <- ttkcombobox(frameTSMaps, values = "", textvariable = .cdtData$EnvData$TSDate, width = largeur6, justify = 'center')
        bt.TSDate.prev <- ttkbutton(frameTSMaps, text = "<<", width = 3)
        bt.TSDate.next <- ttkbutton(frameTSMaps, text = ">>", width = 3)
        bt.TSDate.plot <- ttkbutton(frameTSMaps, text = .cdtEnv$tcl$lang$global[['button']][['3']], width = 7)

        cb.TSData <- ttkcombobox(frameTSMaps, values = "Data", textvariable = .cdtData$EnvData$TSData, width = largeur6)
        bt.TSMapOpt <- ttkbutton(frameTSMaps, text = .cdtEnv$tcl$lang$global[['button']][['4']], width = 7)

        ###################
        tkgrid(bt.TSDate.prev, row = 0, column = 0, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
        tkgrid(cb.TSDate, row = 0, column = 1, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
        tkgrid(bt.TSDate.next, row = 0, column = 2, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
        tkgrid(bt.TSDate.plot, row = 0, column = 3, sticky = '', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)

        tkgrid(cb.TSData, row = 1, column = 1, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
        tkgrid(bt.TSMapOpt, row = 1, column = 3, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)

        ###################

        tkconfigure(bt.TSMapOpt, command = function(){
            if(!is.null(.cdtData$EnvData$tsdata)){
                if(tclvalue(.cdtData$EnvData$TSData) == "Data")
                    atlevel <- pretty(.cdtData$EnvData$tsdata$z, n = 10, min.n = 7)
                if(tclvalue(.cdtData$EnvData$TSData) == "Anomaly")
                    atlevel <- pretty(.cdtData$EnvData$anomData$z, n = 10, min.n = 7)
                if(is.null(.cdtData$EnvData$TSMapOp$userLvl$levels)){
                    .cdtData$EnvData$TSMapOp$userLvl$levels <- atlevel
                }else{
                    if(!.cdtData$EnvData$TSMapOp$userLvl$custom)
                        .cdtData$EnvData$TSMapOp$userLvl$levels <- atlevel
                }
            }
            .cdtData$EnvData$TSMapOp <- MapGraph.MapOptions(.cdtData$EnvData$TSMapOp)

            if(str_trim(tclvalue(.cdtData$EnvData$plot.maps$plot.type)) == "Points")
                .cdtData$EnvData$tab$pointSize.TSMap <- .cdtData$EnvData$TSMapOp$pointSize
        })

        .cdtData$EnvData$tab$TSMap <- NULL

        tkconfigure(bt.TSDate.plot, command = function(){
            if(str_trim(tclvalue(.cdtData$EnvData$TSDate)) != "")
                spatialAnalysis.DisplayTSMaps()
        })

        tkconfigure(bt.TSDate.prev, command = function(){
            if(str_trim(tclvalue(.cdtData$EnvData$TSDate)) != ""){
                ipos <- which(.cdtData$EnvData$statpars$stats == tclvalue(.cdtData$EnvData$climDate))
                idaty <- which(.cdtData$EnvData$statpars$timeseries[[ipos]][[2]] == tclvalue(.cdtData$EnvData$TSDate))
                idaty <- idaty - 1
                if(idaty < 1) idaty <- length(.cdtData$EnvData$statpars$timeseries[[ipos]][[2]])
                tclvalue(.cdtData$EnvData$TSDate) <- .cdtData$EnvData$statpars$timeseries[[ipos]][[2]][idaty]
                ret1 <- read.ClimTSData()
                if(is.null(ret1)) return(NULL)

                spatialAnalysis.DisplayTSMaps()
            }
        })

        tkconfigure(bt.TSDate.next, command = function(){
            if(str_trim(tclvalue(.cdtData$EnvData$TSDate)) != ""){
                ipos <- which(.cdtData$EnvData$statpars$stats == tclvalue(.cdtData$EnvData$climDate))
                idaty <- which(.cdtData$EnvData$statpars$timeseries[[ipos]][[2]] == tclvalue(.cdtData$EnvData$TSDate))
                idaty <- idaty + 1
                if(idaty > length(.cdtData$EnvData$statpars$timeseries[[ipos]][[2]])) idaty <- 1
                tclvalue(.cdtData$EnvData$TSDate) <- .cdtData$EnvData$statpars$timeseries[[ipos]][[2]][idaty]
                ret1 <- read.ClimTSData()
                if(is.null(ret1)) return(NULL)

                spatialAnalysis.DisplayTSMaps()
            }
        })

        ###############

        tkbind(cb.TSDate, "<<ComboboxSelected>>", function(){
            ret1 <- read.ClimTSData()
            if(is.null(ret1)) return(NULL)
        })

        tkbind(cb.TSData, "<<ComboboxSelected>>", function(){
            if(!is.null(.cdtData$EnvData$tsdata)){
                if(str_trim(tclvalue(.cdtData$EnvData$TSData)) == "Data")
                    atlevel <- pretty(.cdtData$EnvData$tsdata$z, n = 10, min.n = 7)
                if(str_trim(tclvalue(.cdtData$EnvData$TSData)) == "Anomaly")
                    atlevel <- pretty(.cdtData$EnvData$anomData$z, n = 10, min.n = 7)
                .cdtData$EnvData$TSMapOp$userLvl$levels <- atlevel
            }
        })

        ##############################################

        framePlotType <- tkframe(subfr3)

        .cdtData$EnvData$plot.maps$plot.type <- tclVar("Pixels")

        txt.plotType <- tklabel(framePlotType, text = "Plot Type", anchor = 'e', justify = 'right')
        cb.plotType <- ttkcombobox(framePlotType, values = "Pixels", textvariable = .cdtData$EnvData$plot.maps$plot.type, width = largeur6)

        tkgrid(txt.plotType, row = 0, column = 0, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
        tkgrid(cb.plotType, row = 0, column = 1, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)

        ###############

        tkbind(cb.plotType, "<<ComboboxSelected>>", function(){
            if(is.null(.cdtData$EnvData$statpars)) return(NULL)

            ret <- read.ClimStat()
            if(is.null(ret)) return(NULL)

            ########
            ret1 <- read.ClimTSData()
            if(is.null(ret1)) return(NULL)
        })

        ##############################################

        tkgrid(frameAnDat, row = 0, column = 0, sticky = 'we')
        tkgrid(frameClimatoMap, row = 1, column = 0, sticky = 'we')
        tkgrid(frameTSMaps, row = 2, column = 0, sticky = 'we', pady = 1)
        tkgrid(framePlotType, row = 3, column = 0, sticky = '', pady = 3)

    #######################################################################################################

    #Tab4
    subfr4 <- bwTabScrollableFrame(cmd.tab4)

        ##############################################

        frameTSPlot <- ttklabelframe(subfr4, text = "Time Series Graph", relief = 'groove')

        typeTSPLOT <- c("Line", "Barplot", "Probability", "ENSO-Line", "ENSO-Barplot", "ENSO-Proba", "Anomaly")
        .cdtData$EnvData$plot.maps$typeTSp <- tclVar("Line")
        .cdtData$EnvData$plot.maps$averageTSp <- tclVar(FALSE)
        .cdtData$EnvData$plot.maps$tercileTSp <- tclVar(FALSE)
        .cdtData$EnvData$plot.maps$trendTSp <- tclVar(FALSE)

        stateType <- if(tclvalue(.cdtData$EnvData$plot.maps$typeTSp) %in% c("Line", "ENSO-Line")) "normal" else "disabled"

        cb.typeTSp <- ttkcombobox(frameTSPlot, values = typeTSPLOT, textvariable = .cdtData$EnvData$plot.maps$typeTSp, width = largeur6)
        bt.TsGraph.plot <- ttkbutton(frameTSPlot, text = .cdtEnv$tcl$lang$global[['button']][['3']], width = 7)
        bt.TSGraphOpt <- ttkbutton(frameTSPlot, text = .cdtEnv$tcl$lang$global[['button']][['4']], width = 8)

        frTS1 <- tkframe(frameTSPlot)
        chk.meanTSp <- tkcheckbutton(frTS1, variable = .cdtData$EnvData$plot.maps$averageTSp, text = "Add Mean", anchor = 'w', justify = 'left', state = stateType)
        chk.tercTSp <- tkcheckbutton(frTS1, variable = .cdtData$EnvData$plot.maps$tercileTSp, text = "Add Terciles", anchor = 'w', justify = 'left', state = stateType)
        chk.trendTSp <- tkcheckbutton(frTS1, variable = .cdtData$EnvData$plot.maps$trendTSp, text = "Add Trend", anchor = 'w', justify = 'left', state = stateType)
        tkgrid(chk.meanTSp, chk.tercTSp, chk.trendTSp)

        #################

        tkconfigure(bt.TSGraphOpt, command = function(){
            suffix.fun <- switch(tclvalue(.cdtData$EnvData$plot.maps$typeTSp),
                                    "Anomaly" = "Anomaly",
                                    "Barplot" = "Bar",
                                    "Line" = "Line",
                                    "Probability" = "Proba",
                                    "ENSO-Line" = "LineENSO",
                                    "ENSO-Barplot" = "BarENSO",
                                    "ENSO-Proba" = "ProbaENSO")
            plot.fun <- get(paste0("MapGraph.GraphOptions.", suffix.fun), mode = "function")
            .cdtData$EnvData$TSGraphOp <- plot.fun(.cdtData$EnvData$TSGraphOp)
        })

        .cdtData$EnvData$tab$TSplot <- NULL
        tkconfigure(bt.TsGraph.plot, command = function(){
            if(!is.null(.cdtData$EnvData$tsdata)){
                imgContainer <- CDT.Display.Graph(spatialAnalysis.plotTSGraph, .cdtData$EnvData$tab$TSplot, 'Time-Series-Plot')
                .cdtData$EnvData$tab$TSplot <- imageNotebookTab_unik(imgContainer, .cdtData$EnvData$tab$TSplot)
            }
        })

        #################

        tkgrid(cb.typeTSp, row = 0, column = 0, sticky = 'we', pady = 3, columnspan = 1)
        tkgrid(bt.TSGraphOpt, row = 0, column = 1, sticky = 'we', padx = 4, pady = 1, columnspan = 1)
        tkgrid(bt.TsGraph.plot, row = 0, column = 2, sticky = 'we', pady = 3, columnspan = 1)
        tkgrid(frTS1, row = 1, column = 0, sticky = 'we', pady = 3, columnspan = 3)

        #################

        tkbind(cb.typeTSp, "<<ComboboxSelected>>", function(){
            stateType <- if(tclvalue(.cdtData$EnvData$plot.maps$typeTSp) %in% c("Line", "ENSO-Line")) "normal" else "disabled"
            tkconfigure(chk.meanTSp, state = stateType)
            tkconfigure(chk.tercTSp, state = stateType)
            tkconfigure(chk.trendTSp, state = stateType)
        })

        tkbind(chk.meanTSp, "<Button-1>", function(){
            .cdtData$EnvData$TSGraphOp$line$legend$add$mean <- 
                        if(tclvalue(.cdtData$EnvData$plot.maps$averageTSp) == '0') TRUE else FALSE
            .cdtData$EnvData$TSGraphOp$line.enso$legend$add$mean <- 
                        if(tclvalue(.cdtData$EnvData$plot.maps$averageTSp) == '0') TRUE else FALSE
        })

        tkbind(chk.tercTSp, "<Button-1>", function(){
            .cdtData$EnvData$TSGraphOp$line$legend$add$tercile <- 
                        if(tclvalue(.cdtData$EnvData$plot.maps$tercileTSp) == '0') TRUE else FALSE
            .cdtData$EnvData$TSGraphOp$line.enso$legend$add$tercile <- 
                        if(tclvalue(.cdtData$EnvData$plot.maps$tercileTSp) == '0') TRUE else FALSE
        })

        tkbind(chk.trendTSp, "<Button-1>", function(){
            .cdtData$EnvData$TSGraphOp$line$legend$add$linear <- 
                        if(tclvalue(.cdtData$EnvData$plot.maps$trendTSp) == '0') TRUE else FALSE
            .cdtData$EnvData$TSGraphOp$line.enso$legend$add$linear <- 
                        if(tclvalue(.cdtData$EnvData$plot.maps$trendTSp) == '0') TRUE else FALSE
        })

        ##############################################

        frameSTNCrds <- ttklabelframe(subfr4, text = "Station/Coordinates", relief = 'groove')

        frTS2 <- tkframe(frameSTNCrds)
        .cdtData$EnvData$plot.maps$lonLOC <- tclVar()
        .cdtData$EnvData$plot.maps$latLOC <- tclVar()
        .cdtData$EnvData$plot.maps$stnIDTSp <- tclVar()

        tkgrid(frTS2, row = 0, column = 0, sticky = 'e', pady = 1)

        ##############################################

        tkgrid(frameTSPlot, row = 0, column = 0, sticky = 'we', pady = 1)
        tkgrid(frameSTNCrds, row = 1, column = 0, sticky = '', pady = 3)

    #######################################################################################################

    #Tab5
    subfr5 <- bwTabScrollableFrame(cmd.tab5)

        ##############################################

        frameSHP <- ttklabelframe(subfr5, text = "Boundaries", relief = 'groove')

        .cdtData$EnvData$shp$add.shp <- tclVar(FALSE)
        file.plotShp <- tclVar()
        stateSHP <- "disabled"

        chk.addshp <- tkcheckbutton(frameSHP, variable = .cdtData$EnvData$shp$add.shp, text = "Add boundaries to Map", anchor = 'w', justify = 'left')
        bt.addshpOpt <- ttkbutton(frameSHP, text = .cdtEnv$tcl$lang$global[['button']][['4']], state = stateSHP)
        cb.addshp <- ttkcombobox(frameSHP, values = unlist(listOpenFiles), textvariable = file.plotShp, width = largeur1, state = stateSHP)
        bt.addshp <- tkbutton(frameSHP, text = "...", state = stateSHP)

        ########
        tkconfigure(bt.addshp, command = function(){
            shp.opfiles <- getOpenShp(.cdtEnv$tcl$main$win)
            if(!is.null(shp.opfiles)){
                update.OpenFiles('shp', shp.opfiles)
                tclvalue(file.plotShp) <- shp.opfiles[[1]]
                listOpenFiles[[length(listOpenFiles) + 1]] <<- shp.opfiles[[1]]
                lapply(list(cb.stnfl, cb.addshp), tkconfigure, values = unlist(listOpenFiles))

                shpofile <- getShpOpenData(file.plotShp)
                if(is.null(shpofile))
                    .cdtData$EnvData$shp$ocrds <- NULL
                else
                    .cdtData$EnvData$shp$ocrds <- getBoundaries(shpofile[[2]])
            }
        })

        ########
        .cdtData$EnvData$SHPOp <- list(col = "black", lwd = 1.5)

        tkconfigure(bt.addshpOpt, command = function(){
            .cdtData$EnvData$SHPOp <- MapGraph.GraphOptions.LineSHP(.cdtData$EnvData$SHPOp)
        })

        ########
        tkgrid(chk.addshp, row = 0, column = 0, sticky = 'we', rowspan = 1, columnspan = 6, padx = 1, pady = 1)
        tkgrid(bt.addshpOpt, row = 0, column = 6, sticky = 'we', rowspan = 1, columnspan = 2, padx = 1, pady = 1)
        tkgrid(cb.addshp, row = 1, column = 0, sticky = 'we', rowspan = 1, columnspan = 7, padx = 1, pady = 1)
        tkgrid(bt.addshp, row = 1, column = 7, sticky = 'w', rowspan = 1, columnspan = 1, padx = 0, pady = 1)

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

        if(.cdtData$EnvData$statpars$params$data.type == "cdtstation"){
            stnIDTSPLOT <- .cdtData$EnvData$tsdata$id
            txt.stnSel <- tklabel(frTS2, text = "Select a station to plot")
            bt.stnID.prev <- ttkbutton(frTS2, text = "<<", width = 6)
            bt.stnID.next <- ttkbutton(frTS2, text = ">>", width = 6)
            cb.stnID <- ttkcombobox(frTS2, values = stnIDTSPLOT, textvariable = .cdtData$EnvData$plot.maps$stnIDTSp, width = largeur6)
            tclvalue(.cdtData$EnvData$plot.maps$stnIDTSp) <- stnIDTSPLOT[1]

            tkconfigure(bt.stnID.prev, command = function(){
                if(!is.null(.cdtData$EnvData$tsdata)){
                    istn <- which(stnIDTSPLOT == str_trim(tclvalue(.cdtData$EnvData$plot.maps$stnIDTSp)))
                    istn <- istn - 1
                    if(istn < 1) istn <- length(stnIDTSPLOT)
                    tclvalue(.cdtData$EnvData$plot.maps$stnIDTSp) <- stnIDTSPLOT[istn]

                    imgContainer <- CDT.Display.Graph(spatialAnalysis.plotTSGraph, .cdtData$EnvData$tab$TSplot, 'Time-Series-Plot')
                    .cdtData$EnvData$tab$TSplot <- imageNotebookTab_unik(imgContainer, .cdtData$EnvData$tab$TSplot)
                }
            })

            tkconfigure(bt.stnID.next, command = function(){
                if(!is.null(.cdtData$EnvData$tsdata)){
                    istn <- which(stnIDTSPLOT == str_trim(tclvalue(.cdtData$EnvData$plot.maps$stnIDTSp)))
                    istn <- istn + 1
                    if(istn > length(stnIDTSPLOT)) istn <- 1
                    tclvalue(.cdtData$EnvData$plot.maps$stnIDTSp) <- stnIDTSPLOT[istn]

                    imgContainer <- CDT.Display.Graph(spatialAnalysis.plotTSGraph, .cdtData$EnvData$tab$TSplot, 'Time-Series-Plot')
                    .cdtData$EnvData$tab$TSplot <- imageNotebookTab_unik(imgContainer, .cdtData$EnvData$tab$TSplot)
                }
            })

            tkgrid(txt.stnSel, row = 0, column = 0, sticky = '', rowspan = 1, columnspan = 3, padx = 1, pady = 1, ipadx = 1, ipady = 1)
            tkgrid(bt.stnID.prev, row = 1, column = 0, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
            tkgrid(cb.stnID, row = 1, column = 1, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
            tkgrid(bt.stnID.next, row = 1, column = 2, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
        }else{
            txt.crdSel <- tklabel(frTS2, text = "Enter longitude and latitude to plot", anchor = 'w', justify = 'left')
            txt.lonLoc <- tklabel(frTS2, text = "Longitude", anchor = 'e', justify = 'right')
            en.lonLoc <- tkentry(frTS2, textvariable = .cdtData$EnvData$plot.maps$lonLOC, width = 8)
            txt.latLoc <- tklabel(frTS2, text = "Latitude", anchor = 'e', justify = 'right')
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

    #####################

    set.plot.type <- function(){
        if(is.null(.cdtData$EnvData$statpars)) return(NULL)

        if(.cdtData$EnvData$statpars$params$data.type == "cdtstation")
        {
            plot.type <- c("Pixels", "Points")
            .cdtData$EnvData$plot.maps$.data.type <- "Points"

            .cdtData$EnvData$climMapOp$pointSize <- 0.7
            .cdtData$EnvData$TSMapOp$pointSize <- 0.7
        }else{
            plot.type <- c("Pixels", "FilledContour")
            .cdtData$EnvData$plot.maps$.data.type <- "Grid"
        }
        tkconfigure(cb.plotType, values = plot.type)
    }

    #######################################################################################################

    load.SpatialAnalysis.Data <- function(){
        if("Anomaly" %in% .cdtData$EnvData$DirStat$Stats){
            climato.maps.Values <- .cdtData$EnvData$DirStat$Stats[!.cdtData$EnvData$DirStat$Stats %in% "Anomaly"]
            if(length(climato.maps.Values) == 0){
                tkconfigure(cb.climato.maps, values = "", state = 'disabled')
                tclvalue(.cdtData$EnvData$climStat) <- "Anomaly"
            }else{
                tkconfigure(cb.climato.maps, values = climato.maps.Values, state = 'normal')
                lastVal <- if(.cdtData$EnvData$DirStat$last == "Anomaly") climato.maps.Values[1] else .cdtData$EnvData$DirStat$last
                tclvalue(.cdtData$EnvData$climStat) <- lastVal
            }
        }else{
            tkconfigure(cb.climato.maps, values = .cdtData$EnvData$DirStat$Stats, state = 'normal')
            tclvalue(.cdtData$EnvData$climStat) <- .cdtData$EnvData$DirStat$last
        }

        ###################
        TSDATA <- if("Anomaly" %in% .cdtData$EnvData$DirStat$Stats) c("Data", "Anomaly") else "Data"
        tkconfigure(cb.TSData, values = TSDATA)
        tclvalue(.cdtData$EnvData$TSData) <- "Data"

        ###################
        analysis.path <- file.path(.cdtData$EnvData$PathStat, tclvalue(.cdtData$EnvData$climStat))
        params <- try(readRDS(file.path(analysis.path, "params.rds")), silent = TRUE)
        if(inherits(params, "try-error")){
            Insert.Messages.Out('Unable to load Climate Analysis data', format = TRUE)
            Insert.Messages.Out(gsub('[\r\n]', '', params[1]), format = TRUE)
            tkconfigure(cb.climDate, values = "")
            tclvalue(.cdtData$EnvData$climDate) <- ""
            return(NULL)
        }
        tkconfigure(cb.climDate, values = params$stats)
        tclvalue(.cdtData$EnvData$climDate) <- params$stats[1]
        stateclimDate <- if(params$params$time.series$out.series == "monthly") "normal" else "disabled"
        tkconfigure(cb.climDate, state = stateclimDate)

        .cdtData$EnvData$statpars <- params

        ###################
        ret <- read.ClimStat()
        if(is.null(ret)) return(NULL)

        ###################
        tkconfigure(cb.TSDate, values = params$timeseries[[1]][[2]])
        tclvalue(.cdtData$EnvData$TSDate) <- params$timeseries[[1]][[2]][1]

        ###################
        ret1 <- read.ClimTSData()
        if(is.null(ret1)) return(NULL)

        ###################
        xlim.years <- substr(.cdtData$EnvData$statpars$stats[1], 1, 9)
        xlim.year1 <- as.numeric(substr(xlim.years, 1, 4))
        xlim.year2 <- as.numeric(substr(xlim.years, 6, 9))
        plotCHOIX <- c("anomaly", "bar", "line", "line.enso", "bar.enso")
        for(pp in plotCHOIX){
            .cdtData$EnvData$TSGraphOp[[pp]]$xlim$min <- xlim.year1
            .cdtData$EnvData$TSGraphOp[[pp]]$xlim$max <- xlim.year2
        }

        ###################
        .cdtData$EnvData$plot.maps$data.type <- .cdtData$EnvData$statpars$params$data.type

        ###################

        widgets.Station.Pixel()
        set.plot.type()
    }

    #######################################################################################################

    read.ClimStat <- function(){
        tkconfigure(.cdtEnv$tcl$main$win, cursor = 'watch')
        tcl('update')
        on.exit({
            tkconfigure(.cdtEnv$tcl$main$win, cursor = '')
            tcl('update')
        })

        if(tclvalue(.cdtData$EnvData$climStat) == "Anomaly") return(0)

        analysis.path <- file.path(.cdtData$EnvData$PathStat, tclvalue(.cdtData$EnvData$climStat))
        extFile <- if(.cdtData$EnvData$statpars$params$data.type == "cdtstation") ".csv" else ".nc"

        filestat <- file.path(analysis.path, paste0(.cdtData$EnvData$statpars$params$analysis.method$mth.fun,
                            "_", tclvalue(.cdtData$EnvData$climDate), extFile))
        if(!file.exists(filestat)){
            Insert.Messages.Out(paste(filestat, 'not found'), format = TRUE)
            return(NULL)
        }

        readClimData <- TRUE
        if(!is.null(.cdtData$EnvData$don))
            if(!is.null(.cdtData$EnvData$filestat))
                if(.cdtData$EnvData$filestat == filestat) readClimData <- FALSE

        if(.cdtData$EnvData$statpars$params$data.type == "cdtstation"){
            change.plot <- str_trim(tclvalue(.cdtData$EnvData$plot.maps$plot.type))

            if(!readClimData)
                if(.cdtData$EnvData$change.plot.ClimData != change.plot) readClimData <- TRUE

            if(readClimData){
                don <- read.table(filestat, header = FALSE, sep = ",",
                                    stringsAsFactors = FALSE, colClasses = "character",
                                    na.strings = .cdtData$Config$missval)

                if(tclvalue(.cdtData$EnvData$climStat) == "Trend"){
                    .cdtData$EnvData$don <- list(
                                                    id = as.character(don[1, -1]), 
                                                    x0 = as.numeric(don[2, -1]),
                                                    y0 = as.numeric(don[3, -1]), 
                                                    var = as.numeric(don[4, -1]),
                                                    p.value = as.numeric(don[6, -1])

                                                    # std.slope = as.numeric(don[5, -1]),
                                                    # r2 = as.numeric(don[7, -1]),
                                                    # na = as.numeric(don[8, -1])
                                                )

                    X0 <- .cdtData$EnvData$don$x0
                    Y0 <- .cdtData$EnvData$don$y0
                    VAR0 <- .cdtData$EnvData$don$var

                    if(change.plot == "Pixels"){
                        nx <- nx_ny_as.image(diff(range(X0)))
                        ny <- nx_ny_as.image(diff(range(Y0)))
                        tmp <- cdt.as.image(VAR0, nx = nx, ny = ny, pts.xy = cbind(X0, Y0))
                        .cdtData$EnvData$don$x <- tmp$x
                        .cdtData$EnvData$don$y <- tmp$y
                        .cdtData$EnvData$don$z <- tmp$z
                        tmp <- cdt.as.image(.cdtData$EnvData$don$p.value, nx = nx, ny = ny, pts.xy = cbind(X0, Y0))
                        .cdtData$EnvData$don$pval <- tmp$z

                        # tmp <- cdt.as.image(.cdtData$EnvData$don$std.slope, nx = nx, ny = ny, pts.xy = cbind(X0, Y0))
                        # .cdtData$EnvData$don$std <- tmp$z
                        # tmp <- cdt.as.image(.cdtData$EnvData$don$r2, nx = nx, ny = ny, pts.xy = cbind(X0, Y0))
                        # .cdtData$EnvData$don$r2 <- tmp$z
                        # tmp <- cdt.as.image(.cdtData$EnvData$don$na, nx = nx, ny = ny, pts.xy = cbind(X0, Y0))
                        # .cdtData$EnvData$don$na <- tmp$z
                        rm(tmp)
                    }

                    if(change.plot == "Points"){
                        .cdtData$EnvData$don$x <- X0
                        .cdtData$EnvData$don$y <- Y0
                        .cdtData$EnvData$don$z <- VAR0
                        .cdtData$EnvData$don$pval <- .cdtData$EnvData$don$p.value

                        # .cdtData$EnvData$don$na <- .cdtData$EnvData$don$na
                    }
                }else{
                    .cdtData$EnvData$don <- list(
                                                    id = as.character(don[1, -1]),
                                                    x0 = as.numeric(don[2, -1]),
                                                    y0 = as.numeric(don[3, -1]),
                                                    var = as.numeric(don[4, -1])

                                                    # na = as.numeric(don[5, -1])
                                                )

                    X0 <- .cdtData$EnvData$don$x0
                    Y0 <- .cdtData$EnvData$don$y0
                    VAR0 <- .cdtData$EnvData$don$var

                    if(change.plot == "Pixels"){
                        nx <- nx_ny_as.image(diff(range(X0)))
                        ny <- nx_ny_as.image(diff(range(Y0)))
                        tmp <- cdt.as.image(VAR0, nx = nx, ny = ny, pts.xy = cbind(X0, Y0))
                        .cdtData$EnvData$don$x <- tmp$x
                        .cdtData$EnvData$don$y <- tmp$y
                        .cdtData$EnvData$don$z <- tmp$z

                        # tmp <- cdt.as.image(.cdtData$EnvData$don$na, nx = nx, ny = ny, pts.xy = cbind(X0, Y0))
                        # .cdtData$EnvData$don$na <- tmp$z
                        rm(tmp)
                    }

                    if(change.plot == "Points"){
                        .cdtData$EnvData$don$x <- X0
                        .cdtData$EnvData$don$y <- Y0
                        .cdtData$EnvData$don$z <- VAR0

                        # .cdtData$EnvData$don$na <- .cdtData$EnvData$don$na
                    }
                }
                .cdtData$EnvData$filestat <- filestat
                .cdtData$EnvData$change.plot.ClimData <- change.plot

                rm(don)
            }
        }else{
            if(readClimData){
                if(tclvalue(.cdtData$EnvData$climStat) == "Trend"){
                    nc <- nc_open(filestat)
                    .cdtData$EnvData$don$x <- nc$dim[[1]]$vals
                    .cdtData$EnvData$don$y <- nc$dim[[2]]$vals
                    .cdtData$EnvData$don$z <- ncvar_get(nc, varid = "trend")
                    .cdtData$EnvData$don$pval <- ncvar_get(nc, varid = "pvalue")

                    # .cdtData$EnvData$don$std <- ncvar_get(nc, varid = "std.slope")
                    # .cdtData$EnvData$don$r2 <- ncvar_get(nc, varid = "r2")
                    # .cdtData$EnvData$don$na <- ncvar_get(nc, varid = "nonNA")
                    nc_close(nc)
                }else{
                    nc <- nc_open(filestat)
                    .cdtData$EnvData$don$x <- nc$dim[[1]]$vals
                    .cdtData$EnvData$don$y <- nc$dim[[2]]$vals
                    .cdtData$EnvData$don$z <- ncvar_get(nc, varid = nc$var[[1]]$name)

                    # .cdtData$EnvData$don$na <- ncvar_get(nc, varid = "nonNA")
                    nc_close(nc)
                }
                .cdtData$EnvData$filestat <- filestat
            }
        }
        return(0)
    }

    ##############################################

    read.ClimTSData <- function(){
        tkconfigure(.cdtEnv$tcl$main$win, cursor = 'watch')
        tcl('update')
        on.exit({
            tkconfigure(.cdtEnv$tcl$main$win, cursor = '')
            tcl('update')
        })

        tsdata.path <- file.path(.cdtData$EnvData$PathStat, "Aggregated_TimeSeries")

        if(.cdtData$EnvData$statpars$params$data.type == "cdtstation"){
            filetsdata <- file.path(tsdata.path, paste0("outTS", "_", tclvalue(.cdtData$EnvData$climDate), ".csv"))
            if(!file.exists(filetsdata)){
                Insert.Messages.Out(paste(filetsdata, 'not found'), format = TRUE)
                return(NULL)
            }

            change.plot <- str_trim(tclvalue(.cdtData$EnvData$plot.maps$plot.type))

            ########
            readTsData <- TRUE
            if(!is.null(.cdtData$EnvData$tsdata))
                if(!is.null(.cdtData$EnvData$filetsdata))
                    if(.cdtData$EnvData$filetsdata == filetsdata) readTsData <- FALSE

            if(readTsData){
                don <- read.table(filetsdata, header = FALSE, sep = ",",
                                        stringsAsFactors = FALSE, colClasses = "character",
                                        na.strings = .cdtData$Config$missval)

                .cdtData$EnvData$tsdata <- list(id = as.character(don[1, -1]), 
                                                x0 = as.numeric(don[2, -1]),
                                                y0 = as.numeric(don[3, -1]), 
                                                date = as.character(don[-(1:3), 1]),
                                                data = local({
                                                        x <- don[-(1:3), -1, drop = FALSE]
                                                        xdim <- dim(x)
                                                        x <- apply(x, 2, as.numeric)
                                                        dim(x) <- xdim
                                                        x
                                                    })
                                                )
                .cdtData$EnvData$filetsdata <- filetsdata
            }

            ########

            rasterTsData <- TRUE
            if(!readTsData)
                if(!is.null(.cdtData$EnvData$rasterTsData))
                    if(.cdtData$EnvData$filetsdata == filetsdata)
                        if(.cdtData$EnvData$rasterTsData == tclvalue(.cdtData$EnvData$TSDate)) rasterTsData <- FALSE

            if(!rasterTsData)
                if(.cdtData$EnvData$change.plot.rasterTsData != change.plot) rasterTsData <- TRUE

            if(rasterTsData){
                idt <- which(.cdtData$EnvData$tsdata$date == tclvalue(.cdtData$EnvData$TSDate))

                X0 <- .cdtData$EnvData$tsdata$x0
                Y0 <- .cdtData$EnvData$tsdata$y0
                VAR0 <- as.numeric(.cdtData$EnvData$tsdata$data[idt, ])

                if(change.plot == "Pixels"){
                    nx <- nx_ny_as.image(diff(range(X0)))
                    ny <- nx_ny_as.image(diff(range(Y0)))
                    tmp <- cdt.as.image(VAR0, nx = nx, ny = ny, pts.xy = cbind(X0, Y0))
                    .cdtData$EnvData$tsdata$x <- tmp$x
                    .cdtData$EnvData$tsdata$y <- tmp$y
                    .cdtData$EnvData$tsdata$z <- tmp$z
                    rm(tmp)
                }

                if(change.plot == "Points"){
                    .cdtData$EnvData$tsdata$x <- X0
                    .cdtData$EnvData$tsdata$y <- Y0
                    .cdtData$EnvData$tsdata$z <- VAR0
                }

                .cdtData$EnvData$rasterTsData <- tclvalue(.cdtData$EnvData$TSDate)
                .cdtData$EnvData$change.plot.rasterTsData <- change.plot
            }

            if("Anomaly" %in% .cdtData$EnvData$DirStat$Stats){
                anom.path <- file.path(.cdtData$EnvData$PathStat, "Anomaly")
                file.anom <- file.path(anom.path, paste0("anomaly", "_", tclvalue(.cdtData$EnvData$climDate), ".csv"))
                if(!file.exists(file.anom)){
                    Insert.Messages.Out(paste(file.anom, 'not found'), format = TRUE)
                    return(NULL)
                }

                readAnomData <- TRUE
                if(!is.null(.cdtData$EnvData$anomData))
                    if(!is.null(.cdtData$EnvData$file.anom))
                        if(.cdtData$EnvData$file.anom == file.anom) readAnomData <- FALSE

                if(readAnomData){
                    don <- read.table(file.anom, header = FALSE, sep = ",",
                                                stringsAsFactors = FALSE, colClasses = "character",
                                                na.strings = .cdtData$Config$missval.anom)

                    .cdtData$EnvData$anomData <- list(id = as.character(don[1, -1]), 
                                                      x0 = as.numeric(don[2, -1]),
                                                      y0 = as.numeric(don[3, -1]), 
                                                      date = as.character(don[-(1:3), 1]),
                                                      data = local({
                                                            x <- don[-(1:3), -1, drop = FALSE]
                                                            xdim <- dim(x)
                                                            x <- apply(x, 2, as.numeric)
                                                            dim(x) <- xdim
                                                            x
                                                        })
                                                    )
                    .cdtData$EnvData$file.anom <- file.anom
                    params <- readRDS(file.path(anom.path, "params.rds"))
                    .cdtData$EnvData$anomData$params <- params$params
                    rm(don)
                }

                ########
                rasterAnomData <- TRUE
                if(!readAnomData)
                    if(!is.null(.cdtData$EnvData$rasterAnomData))
                        if(.cdtData$EnvData$file.anom == file.anom)
                            if(.cdtData$EnvData$rasterAnomData == tclvalue(.cdtData$EnvData$TSDate)) rasterAnomData <- FALSE

                if(!rasterAnomData)
                    if(.cdtData$EnvData$change.plot.rasterAnomData != change.plot) rasterAnomData <- TRUE

                if(rasterAnomData){
                    idt <- which(.cdtData$EnvData$anomData$date == tclvalue(.cdtData$EnvData$TSDate))

                    X0 <- .cdtData$EnvData$anomData$x0
                    Y0 <- .cdtData$EnvData$anomData$y0
                    VAR0 <- as.numeric(.cdtData$EnvData$anomData$data[idt, ])

                    if(change.plot == "Pixels"){
                        nx <- nx_ny_as.image(diff(range(X0)))
                        ny <- nx_ny_as.image(diff(range(Y0)))
                        tmp <- cdt.as.image(VAR0, nx = nx, ny = ny, pts.xy = cbind(X0, Y0))
                        .cdtData$EnvData$anomData$x <- tmp$x
                        .cdtData$EnvData$anomData$y <- tmp$y
                        .cdtData$EnvData$anomData$z <- tmp$z
                        rm(tmp)
                    }

                    if(change.plot == "Points"){
                        .cdtData$EnvData$anomData$x <- X0
                        .cdtData$EnvData$anomData$y <- Y0
                        .cdtData$EnvData$anomData$z <- VAR0
                    }

                    .cdtData$EnvData$rasterAnomData <- tclvalue(.cdtData$EnvData$TSDate)
                    .cdtData$EnvData$change.plot.rasterAnomData <- change.plot
                }
            }
        }else{
            filetsdata <- file.path(tsdata.path, paste0("outTS", "_", tclvalue(.cdtData$EnvData$TSDate), ".nc"))
            if(!file.exists(filetsdata)){
                Insert.Messages.Out(paste(filetsdata, 'not found'), format = TRUE)
                return(NULL)
            }

            readTsData <- TRUE
            if(!is.null(.cdtData$EnvData$tsdata))
                if(!is.null(.cdtData$EnvData$filetsdata))
                    if(.cdtData$EnvData$filetsdata == filetsdata) readTsData <- FALSE

            if(readTsData){
                nc <- nc_open(filetsdata)
                .cdtData$EnvData$tsdata$x <- nc$dim[[1]]$vals
                .cdtData$EnvData$tsdata$y <- nc$dim[[2]]$vals
                .cdtData$EnvData$tsdata$z <- ncvar_get(nc, varid = nc$var[[1]]$name)
                nc_close(nc)
                .cdtData$EnvData$filetsdata <- filetsdata
            }

            if(is.null(.cdtData$EnvData$cdtdataset)){
                fldataset <- basename(.cdtData$EnvData$statpars$params$in.file)
                fldataset <- file.path(.cdtData$EnvData$PathStat, paste0("Aggregated_", tools::file_path_sans_ext(fldataset)), fldataset)
                .cdtData$EnvData$cdtdataset <- readRDS(fldataset)
                .cdtData$EnvData$cdtdataset$fileInfo <- fldataset
            }

            if("Anomaly" %in% .cdtData$EnvData$DirStat$Stats){
                anom.path <- file.path(.cdtData$EnvData$PathStat, "Anomaly")
                file.anom <- file.path(anom.path, paste0("anomaly", "_", tclvalue(.cdtData$EnvData$TSDate), ".nc"))
                if(!file.exists(file.anom)){
                    Insert.Messages.Out(paste(file.anom, 'not found'), format = TRUE)
                    return(NULL)
                }

                readAnom <- TRUE
                if(!is.null(.cdtData$EnvData$anomData))
                    if(!is.null(.cdtData$EnvData$file.anom))
                        if(.cdtData$EnvData$file.anom == file.anom) readAnom <- FALSE

                if(readAnom){
                    nc <- nc_open(file.anom)
                    .cdtData$EnvData$anomData$x <- nc$dim[[1]]$vals
                    .cdtData$EnvData$anomData$y <- nc$dim[[2]]$vals
                    .cdtData$EnvData$anomData$z <- ncvar_get(nc, varid = nc$var[[1]]$name)
                    nc_close(nc)
                    .cdtData$EnvData$file.anom <- file.anom
                    params <- readRDS(file.path(anom.path, "params.rds"))
                    .cdtData$EnvData$anomData$params <- params$params
                }
            }
        }

        if(is.null(.cdtData$EnvData$ONI)){
            ONI <- readRDS(file.path(.cdtDir$Root, 'data', 'ONI_1950-present.rds'))
            .cdtData$EnvData$ONI$date <- format(addMonths(as.Date(paste0(ONI$ts[, 1], "-15")), 1), "%Y%m")
            .cdtData$EnvData$ONI$data <- ONI$ts[, 3]
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

#######################################################################################################

spatialAnalysisEditYrsMon <- function(parent.win, vedit, titre, help, year, lang.dlg){
    if(WindowsOS()){
        largeur1 <- 35
        largeur2 <- 35
    }else{
        largeur1 <- 40
        largeur2 <- 40
    }

    #########
    tt <- tktoplevel()
    tkgrab.set(tt)
    tkfocus(tt)

    frDialog <- tkframe(tt, relief = 'raised', borderwidth = 2)
    frButt <- tkframe(tt)

    ####
    frameEdit <- tkframe(frDialog)
    frameInfo <- tkframe(frDialog)

    #########
    yscr.Edit <- tkscrollbar(frameEdit, repeatinterval = 4,
                            command = function(...) tkyview(text.Edit, ...))
    text.Edit <- tktext(frameEdit, bg = "white", wrap = "word",
                        height = 4, width = largeur1,
                        yscrollcommand = function(...) tkset(yscr.Edit, ...))

    tkgrid(text.Edit, yscr.Edit)
    tkgrid.configure(yscr.Edit, sticky = "ns")
    tkgrid.configure(text.Edit, sticky = 'nswe')

    tkinsert(text.Edit, "end", vedit)

    #########

    yscr.Info <- tkscrollbar(frameInfo, repeatinterval = 4, command = function(...) tkyview(txta.Info, ...))
    txta.Info <- tktext(frameInfo, cursor = "", wrap = "word", height = 4, width = largeur2,
                        yscrollcommand = function(...) tkset(yscr.Info, ...))

    tkgrid(txta.Info, yscr.Info)
    tkgrid.configure(yscr.Info, sticky = "ns")
    tkgrid.configure(txta.Info, sticky = 'nswe')

    tkinsert(txta.Info, "1.0", help)
    tkconfigure(txta.Info, state = "disabled")

    #########
    tkgrid(frameEdit, row = 0, column = 0, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
    tkgrid(frameInfo, row = 1, column = 0, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)

    #########
    vedit <- str_trim(strsplit(vedit, ",")[[1]])
    vedit <- as.numeric(vedit)

    if(year){
        xtm <- "years"
        xlo <- 1800
        xup <- 2100
        if(length(vedit) == 0) vedit <- NA
    }else{
        xtm <- "months"
        xlo <- 1
        xup <- 12
    }

    #########
    bt.opt.OK <- ttkbutton(frButt, text = .cdtEnv$tcl$lang$global[['button']][['1']]) 
    bt.opt.CA <- ttkbutton(frButt, text = .cdtEnv$tcl$lang$global[['button']][['2']]) 

    tkconfigure(bt.opt.OK, command = function(){
        tmp <- tclvalue(tkget(text.Edit, "0.0", "end"))
        tmp <- gsub("[\r\n]", "", tmp)
        tmp <- str_trim(strsplit(tmp, ",")[[1]])
        tmp <- as.numeric(tmp)
        if(length(tmp) == 0){
            tkmessageBox(message = paste("No", xtm, "edited"), icon = "warning", type = "ok")
        }else if(length(tmp) > 0 & any(is.na(tmp))){
            tkmessageBox(message = paste("Check the", xtm, "that you edited\n", paste0(tmp, collapse = ', ')), icon = "warning", type = "ok")
            tkwait.window(tt)
        }else if(any(tmp > xup | tmp < xlo)){
            msg <- if(year) "Ambiguous years. Year must be 4 digits" else "Month number must be between 1 and 12"
            tkmessageBox(message = paste(msg, "\n", paste0(tmp, collapse = ', ')), icon = "warning", type = "ok")
            tkwait.window(tt)
        }else{
            vedit <<- tmp
            tkgrab.release(tt)
            tkdestroy(tt)
            tkfocus(parent.win)
        }
    })

    tkconfigure(bt.opt.CA, command = function(){
        tkgrab.release(tt)
        tkdestroy(tt)
        tkfocus(parent.win)
    })

    tkgrid(bt.opt.OK, row = 0, column = 0, padx = 5, pady = 1, ipadx = 1, sticky = 'w')
    tkgrid(bt.opt.CA, row = 0, column = 1, padx = 5, pady = 1, ipadx = 1, sticky = 'e')

    ############################################################### 

    tkgrid(frDialog, row = 0, column = 0, sticky = 'nswe', rowspan = 1, columnspan = 2, padx = 1, pady = 1, ipadx = 1, ipady = 1)
    tkgrid(frButt, row = 1, column = 1, sticky = 'se', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)

    tkwm.withdraw(tt)
    tcl('update')
    tt.w <- as.integer(tkwinfo("reqwidth", tt))
    tt.h <- as.integer(tkwinfo("reqheight", tt))
    tt.x <- as.integer(.cdtEnv$tcl$data$width.scr*0.5 - tt.w*0.5)
    tt.y <- as.integer(.cdtEnv$tcl$data$height.scr*0.5 - tt.h*0.5)
    tkwm.geometry(tt, paste0('+', tt.x, '+', tt.y))
    tkwm.transient(tt)
    tkwm.title(tt, titre)
    tkwm.deiconify(tt)

    ##################################################################  
    tkfocus(tt)
    tkbind(tt, "<Destroy>", function(){
        tkgrab.release(tt)
        tkfocus(parent.win)
    })
    tkwait.window(tt)
    return(vedit)
}
