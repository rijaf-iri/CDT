
spatialAnalysisPanelCmd <- function(){
    listOpenFiles <- openFile_ttkcomboList()
    if(WindowsOS()){
        largeur0 <- 29
        largeur1 <- 33
        largeur2 <- 36
        largeur3 <- 25
        largeur4a <- 27
        largeur4b <- 34
        largeur5 <- 22
        largeur6 <- 18
        largeur7 <- 7
        largeur8 <- 20
        largeur9 <- 14
        largeur10 <- 30
        largeur11 <- 19
        largeur12 <- 10
        largeur13 <- 2
    }else{
        largeur0 <- 30
        largeur1 <- 32
        largeur2 <- 33
        largeur3 <- 25
        largeur4a <- 27
        largeur4b <- 39
        largeur5 <- 22
        largeur6 <- 18
        largeur7 <- 7
        largeur8 <- 19
        largeur9 <- 14
        largeur10 <- 30
        largeur11 <- 19
        largeur12 <- 10
        largeur13 <- 4
    }

    ###################

    GeneralParameters <- list(data.type = "cdtstation", in.tstep = "daily", in.file = "", out.dir = "",
                              analysis = list(method = "mean",
                                             percentile = 90,
                                             frequency = list(low = 0, up = 200),
                                             anomaly = list(perc = FALSE, all.years = TRUE,
                                                            start.year = 1981, end.year = 2010, min.year = 10),
                                             trend = list(unit = 1, min.year = 10)
                                            ),
                              aggr.series = list(aggr.fun = "sum", opr.fun = ">=", opr.thres = 1,
                                                 min.frac = list(unique = TRUE, all = 0.95, month = rep(0.95, 12))),
                              use.years = list(all.years = TRUE, start.year = 1981, end.year = 2010,
                                               nseq.years = FALSE, custom.years = NA),
                              out.series = list(tstep = 'monthly', start.mon = 1, length.mon = 3),
                              use.month = list(start.month = 1, end.month = 12,
                                               nseq.months = FALSE, custom.months = 1:12)
                            )

    GeneralParameters$plot <- list(TSData = "Data", typeTSp = "line")

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

    .cdtData$EnvData$SHPOp <- list(col = "black", lwd = 1.5)

    ###################

    xml.dlg <- file.path(.cdtDir$dirLocal, "languages", "cdtSpatialAnalysis_leftCmd.xml")
    lang.dlg <- cdtLanguageParse(xml.dlg, .cdtData$Config$lang.iso)
    .cdtData$EnvData$message <- lang.dlg[['message']]

    ###################

    .cdtEnv$tcl$main$cmd.frame <- tkframe(.cdtEnv$tcl$main$panel.left)

    tknote.cmd <- bwNoteBook(.cdtEnv$tcl$main$cmd.frame)
    cmd.tab1 <- bwAddTab(tknote.cmd, text = lang.dlg[['tab_title']][['1']])
    cmd.tab2 <- bwAddTab(tknote.cmd, text = lang.dlg[['tab_title']][['2']])
    cmd.tab3 <- bwAddTab(tknote.cmd, text = lang.dlg[['tab_title']][['3']])
    cmd.tab4 <- bwAddTab(tknote.cmd, text = lang.dlg[['tab_title']][['4']])
    cmd.tab5 <- bwAddTab(tknote.cmd, text = lang.dlg[['tab_title']][['5']])

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

        frameTimeS <- ttklabelframe(subfr1, text = lang.dlg[['label']][['1']], relief = 'groove')

        timeSteps <- tclVar()
        CbperiodVAL <- .cdtEnv$tcl$lang$global[['combobox']][['1']][3:6]
        periodVAL <- c('daily', 'pentad', 'dekadal', 'monthly')
        tclvalue(timeSteps) <- CbperiodVAL[periodVAL %in% GeneralParameters$in.tstep]

        cb.fperiod <- ttkcombobox(frameTimeS, values = CbperiodVAL, textvariable = timeSteps, width = largeur0)

        tkgrid(cb.fperiod, row = 0, column = 0, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)

        helpWidget(cb.fperiod, lang.dlg[['tooltip']][['1']], lang.dlg[['status']][['1']])

        ############

        tkbind(cb.fperiod, "<<ComboboxSelected>>", function(){
            outstep <- outSeriesVAL[CboutSeriesVAL %in% str_trim(tclvalue(out.tstep))]
            instep <- periodVAL[CbperiodVAL %in% str_trim(tclvalue(timeSteps))]

            stateAggr <- if(outstep == 'monthly' & instep == 'monthly') "disabled" else "normal"
            tkconfigure(bt.AggrFun, state = stateAggr)
        })

        #######################

        frameData <- ttklabelframe(subfr1, text = lang.dlg[['label']][['2']], relief = 'groove')

        DataType <- tclVar()
        CbdatatypeVAL <- .cdtEnv$tcl$lang$global[['combobox']][['2']][1:2]
        datatypeVAL <- c('cdtstation', 'cdtdataset')
        tclvalue(DataType) <- CbdatatypeVAL[datatypeVAL %in% GeneralParameters$data.type]

        file.stnfl <- tclVar(GeneralParameters$in.file)

        if(GeneralParameters$data.type == 'cdtstation')
            txt.INData <- lang.dlg[['label']][['3']]
        if(GeneralParameters$data.type == 'cdtdataset')
            txt.INData <- lang.dlg[['label']][['4']]
        fileINdir <- tclVar(txt.INData)

        txt.datatype <- tklabel(frameData, text = lang.dlg[['label']][['5']], anchor = 'e', justify = 'right')
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

        helpWidget(cb.datatype, lang.dlg[['tooltip']][['2']], lang.dlg[['status']][['2']])
        helpWidget(cb.stnfl, lang.dlg[['tooltip']][['3']], lang.dlg[['status']][['3']])
        helpWidget(bt.stnfl, lang.dlg[['tooltip']][['4']], lang.dlg[['status']][['4']])

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
                tclvalue(fileINdir) <- lang.dlg[['label']][['3']]

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

                helpWidget(cb.stnfl, lang.dlg[['tooltip']][['3']], lang.dlg[['status']][['3']])
            }

            if(str_trim(tclvalue(DataType)) == CbdatatypeVAL[2]){
                tclvalue(fileINdir) <- lang.dlg[['label']][['4']]

                cb.stnfl <<- tkentry(frameData, textvariable = file.stnfl, width = largeur2)

                #######
                tkconfigure(bt.stnfl, command = function(){
                    path.dataset <- tclvalue(tkgetOpenFile(initialdir = getwd(), filetypes = .cdtEnv$tcl$data$filetypes6))
                    tclvalue(file.stnfl) <- if(path.dataset %in% c("", "NA") | is.na(path.dataset)) "" else path.dataset
                })

                helpWidget(cb.stnfl, lang.dlg[['tooltip']][['5']], lang.dlg[['status']][['5']])
            }
            
            #######
            tkgrid(cb.stnfl, row = 2, column = 0, sticky = 'we', rowspan = 1, columnspan = 6, padx = 0, pady = 0, ipadx = 1, ipady = 1)
        })

        #############################

        frameDirSav <- tkframe(subfr1, relief = 'groove', borderwidth = 2)

        file.save1 <- tclVar(GeneralParameters$out.dir)

        txt.file.save <- tklabel(frameDirSav, text = lang.dlg[['label']][['6']], anchor = 'w', justify = 'left')
        en.file.save <- tkentry(frameDirSav, textvariable = file.save1, width = largeur2)
        bt.file.save <- tkbutton(frameDirSav, text = "...")

        ##############
        tkgrid(txt.file.save, row = 0, column = 0, sticky = 'we', rowspan = 1, columnspan = 5, padx = 1, pady = 0, ipadx = 1, ipady = 1)
        tkgrid(en.file.save, row = 1, column = 0, sticky = 'we', rowspan = 1, columnspan = 4, padx = 0, pady = 0, ipadx = 1, ipady = 1)
        tkgrid(bt.file.save, row = 1, column = 4, sticky = 'w', rowspan = 1, columnspan = 1, padx = 0, pady = 0, ipadx = 1, ipady = 1)

        helpWidget(en.file.save, lang.dlg[['tooltip']][['6']], lang.dlg[['status']][['6']])
        helpWidget(bt.file.save, lang.dlg[['tooltip']][['7']], lang.dlg[['status']][['7']])

        ##############
        tkconfigure(bt.file.save, command = function() fileORdir2Save(file.save1, isFile = FALSE))

        #############################
        tkgrid(frameTimeS, row = 0, column = 0, sticky = '', pady = 1)
        tkgrid(frameData, row = 1, column = 0, sticky = 'we', pady = 3)
        tkgrid(frameDirSav, row = 2, column = 0, sticky = 'we', pady = 3)

    #######################################################################################################

    #Tab2
    subfr2 <- bwTabScrollableFrame(cmd.tab2)

        #######################

        frameOut <- ttklabelframe(subfr2, text = lang.dlg[['label']][['7']], relief = 'groove')

        ###############

        out.tstep <- tclVar()
        CboutSeriesVAL <- .cdtEnv$tcl$lang$global[['combobox']][['1']][6:8]
        outSeriesVAL <- c('monthly', 'annual', 'seasonal')
        tclvalue(out.tstep) <- CboutSeriesVAL[outSeriesVAL %in% GeneralParameters$out.series$tstep]

        txt.outTS <- tklabel(frameOut, text = "Time step", anchor = 'e', justify = 'right')
        cb.outTS <- ttkcombobox(frameOut, values = CboutSeriesVAL, textvariable = out.tstep, width = largeur3)

        helpWidget(cb.outTS, lang.dlg[['tooltip']][['9']], lang.dlg[['status']][['9']])

        ############

        tkbind(cb.outTS, "<<ComboboxSelected>>", function(){
            outstep <- outSeriesVAL[CboutSeriesVAL %in% str_trim(tclvalue(out.tstep))]
            instep <- periodVAL[CbperiodVAL %in% str_trim(tclvalue(timeSteps))]

            stateSeas <- if(outstep == 'seasonal') "normal" else "disabled"
            tkconfigure(cb.seasS, state = stateSeas)
            tkconfigure(cb.seasL, state = stateSeas)

            seasdef <- ""
            if(outstep == 'seasonal'){
                mon <-  which(MOIS %in% str_trim(tclvalue(start.mon)))
                len <- as.numeric(str_trim(tclvalue(length.mon)))
                mon1 <- (mon + len - 1) %% 12
                mon1[mon1 == 0] <- 12
                seasdef <- paste(MOIS[mon], "->", MOIS[mon1])
            }
            if(outstep == 'annual'){
                tclvalue(start.mon) <- MOIS[1]
                tclvalue(length.mon) <- 12
                seasdef <- paste(MOIS[1], "->", MOIS[12])
            }
            tclvalue(season.def) <- seasdef

            stateMonth <- if(outstep == 'monthly') "normal" else "disabled" 
            tkconfigure(bt.MonAnalyze, state = stateMonth)

            stateAggr <- if(outstep == 'monthly' & instep == 'monthly') "disabled" else "normal"
            tkconfigure(bt.AggrFun, state = stateAggr)
        })

        ###############

        frDefSeas <- tkframe(frameOut)

        mon <- GeneralParameters$out.series$start.mon
        len <- GeneralParameters$out.series$length.mon
        mon1 <- (mon + len - 1) %% 12
        mon1[mon1 == 0] <- 12

        start.mon <- tclVar(MOIS[mon])
        length.mon <- tclVar(len)

        stateSeas <- if(GeneralParameters$out.series$tstep == 'seasonal') "normal" else "disabled"

        txt.seasS <- tklabel(frDefSeas, text = lang.dlg[['label']][['8']], anchor = 'e', justify = 'right')
        cb.seasS <- ttkcombobox(frDefSeas, values = MOIS, textvariable = start.mon, width = 11, state = stateSeas)
        txt.seasL <- tklabel(frDefSeas, text = lang.dlg[['label']][['9']])
        cb.seasL <- ttkcombobox(frDefSeas, values = 2:12, textvariable = length.mon, width = 3, state = stateSeas)

        sepL.seasS <- tklabel(frDefSeas, text = '', width = largeur13)

        tkgrid(txt.seasS, row = 0, column = 0, sticky = 'we')
        tkgrid(cb.seasS, row = 0, column = 1, sticky = 'we')
        tkgrid(sepL.seasS, row = 0, column = 2)
        tkgrid(txt.seasL, row = 0, column = 3, sticky = 'we')
        tkgrid(cb.seasL, row = 0, column = 4, sticky = 'we')

        helpWidget(cb.seasS, lang.dlg[['tooltip']][['10']], lang.dlg[['status']][['10']])
        helpWidget(cb.seasL, lang.dlg[['tooltip']][['11']], lang.dlg[['status']][['11']])

        ##############

        tkbind(cb.seasS, "<<ComboboxSelected>>", function(){
            outstep <- outSeriesVAL[CboutSeriesVAL %in% str_trim(tclvalue(out.tstep))]
            seasdef <- ""
            if(outstep == 'seasonal'){
                mon <-  which(MOIS %in% str_trim(tclvalue(start.mon)))
                len <- as.numeric(str_trim(tclvalue(length.mon)))
                mon1 <- (mon + len - 1) %% 12
                mon1[mon1 == 0] <- 12
                seasdef <- paste(MOIS[mon], "->", MOIS[mon1])
            }
            tclvalue(season.def) <- seasdef
        })

        tkbind(cb.seasL, "<<ComboboxSelected>>", function(){
            outstep <- outSeriesVAL[CboutSeriesVAL %in% str_trim(tclvalue(out.tstep))]
            seasdef <- ""
            if(outstep == 'seasonal'){
                mon <-  which(MOIS %in% str_trim(tclvalue(start.mon)))
                len <- as.numeric(str_trim(tclvalue(length.mon)))
                mon1 <- (mon + len - 1) %% 12
                mon1[mon1 == 0] <- 12
                seasdef <- paste(MOIS[mon], "->", MOIS[mon1])
            }
            tclvalue(season.def) <- seasdef
        })

        ##############

        frDispSeas <- tkframe(frameOut)

        seasdef <- paste(MOIS[mon], "->", MOIS[mon1])
        season.def <- tclVar(seasdef)

        txt.SeasD <- tklabel(frDispSeas, text = tclvalue(season.def), textvariable = season.def)

        tkgrid(txt.SeasD, row = 0, column = 0, sticky = 'we')

        ###############

        tkgrid(txt.outTS, row = 0, column = 0, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
        tkgrid(cb.outTS, row = 0, column = 1, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
        tkgrid(frDefSeas, row = 1, column = 0, sticky = 'we', rowspan = 1, columnspan = 2, padx = 1, pady = 1, ipadx = 1, ipady = 1)
        tkgrid(frDispSeas, row = 2, column = 0, sticky = '', rowspan = 1, columnspan = 2, padx = 1, pady = 1, ipadx = 1, ipady = 1)

        #######################

        stateMonth <- if(GeneralParameters$out.series$tstep == 'monthly') "normal" else "disabled" 

        bt.MonAnalyze <- ttkbutton(subfr2, text = lang.dlg[['button']][['3']], state = stateMonth)

        helpWidget(bt.MonAnalyze, lang.dlg[['tooltip']][['12']], lang.dlg[['status']][['12']])

        ######

        tkconfigure(bt.MonAnalyze, command = function(){
            GeneralParameters$use.month <<- getInfoMonths2Process(.cdtEnv$tcl$main$win,
                                                                  GeneralParameters$use.month)
        })

        #######################

        bt.YearAnalyze <- ttkbutton(subfr2, text = lang.dlg[['button']][['1']])

        helpWidget(bt.YearAnalyze, lang.dlg[['tooltip']][['8']], lang.dlg[['status']][['8']])

        ######

        tkconfigure(bt.YearAnalyze, command = function(){
            GeneralParameters$use.years <<- getInfoYears2Analyze(.cdtEnv$tcl$main$win,
                                                                 GeneralParameters$use.years)
        })

        #######################

        stateAggr <- if(GeneralParameters$out.series$tstep == 'monthly' &
                        GeneralParameters$in.tstep == 'monthly') "disabled" else "normal"

        bt.AggrFun <- ttkbutton(subfr2, text = lang.dlg[['button']][['2']], state = stateAggr)

        ######

        tkconfigure(bt.AggrFun, command = function(){
            AGGRFUN <- c("sum", "mean", "median", "max", "min", "count")
            GeneralParameters$aggr.series <<- getInfo_AggregateFun(.cdtEnv$tcl$main$win,
                                                                   GeneralParameters$aggr.series,
                                                                   AGGRFUN
                                                                  )
        })

        #######################

        frameAnalysis <- ttklabelframe(subfr2, text = lang.dlg[['label']][['27']], relief = 'groove')

        CbAnalysesVAL <- lang.dlg[['combobox']][['1']]
        AnalysesVAL <- c('mean', 'median', 'std', 'cv', 'trend', 'percentile', 'frequency', 'anomaly')
        analysis.method <- tclVar()
        tclvalue(analysis.method) <- CbAnalysesVAL[AnalysesVAL %in% GeneralParameters$analysis$method]

        cb.anMthd <- ttkcombobox(frameAnalysis, values = CbAnalysesVAL, textvariable = analysis.method, justify = 'center', width = largeur4a)
        txt.anMthd <- tklabel(frameAnalysis, text = '', width = largeur4b)

        tkgrid(cb.anMthd, row = 0, column = 0, sticky = '')
        tkgrid(txt.anMthd, row = 1, column = 0, sticky = 'we')

        helpWidget(cb.anMthd, lang.dlg[['tooltip']][['13']], lang.dlg[['status']][['13']])

        #######################

        analysisMethodFun <- function(analysisMthd){
            tkdestroy(fr.anMthd)
            fr.anMthd <<- tkframe(frameAnalysis)

            ###############
            if(analysisMthd == 'trend'){
                rd.trend1 <- tkradiobutton(fr.anMthd, text = lang.dlg[['radiobutton']][['1']][1], variable = trend.unit, value = "1", anchor = 'w', justify = 'left')
                rd.trend2 <- tkradiobutton(fr.anMthd, text = lang.dlg[['radiobutton']][['1']][2], variable = trend.unit, value = "2", anchor = 'w', justify = 'left')
                rd.trend3 <- tkradiobutton(fr.anMthd, text = lang.dlg[['radiobutton']][['1']][3], variable = trend.unit, value = "3", anchor = 'w', justify = 'left')
                txt.trend <- tklabel(fr.anMthd, text = lang.dlg[['label']][['10']], anchor = 'e', justify = 'right')
                en.trend <- tkentry(fr.anMthd, textvariable = trend.minyear, width = 4)

                tkgrid(rd.trend1, sticky = 'we', columnspan = 2)
                tkgrid(rd.trend2, sticky = 'we', columnspan = 2)
                tkgrid(rd.trend3, sticky = 'we', columnspan = 2)
                tkgrid(txt.trend, en.trend)

                helpWidget(en.trend, lang.dlg[['tooltip']][['14']], lang.dlg[['status']][['14']])
            }

            if(analysisMthd == 'percentile'){
                en.Percent <- tkentry(fr.anMthd, textvariable = mth.perc, width = 4, justify = 'center')
                th.Percent <- tklabel(fr.anMthd, text = lang.dlg[['label']][['11']], anchor = 'w', justify = 'left')
                txt.Percent <- tklabel(fr.anMthd, text = lang.dlg[['label']][['12']])

                tkgrid(en.Percent, th.Percent, txt.Percent)

                helpWidget(en.Percent, lang.dlg[['tooltip']][['15']], lang.dlg[['status']][['15']])
            }

            if(analysisMthd == 'frequency'){
                txt.Freq1 <- tklabel(fr.anMthd, text = lang.dlg[['label']][['13']], anchor = 'e', justify = 'right')
                en.Freq1 <- tkentry(fr.anMthd, textvariable = low.thres, width = 5, justify = 'center')
                txt.Freq2 <- tklabel(fr.anMthd, text = lang.dlg[['label']][['14']])
                en.Freq2 <- tkentry(fr.anMthd, textvariable = up.thres, width = 5, justify = 'center')

                tkgrid(txt.Freq1, en.Freq1, txt.Freq2, en.Freq2)

                helpWidget(en.Freq1, lang.dlg[['tooltip']][['16']], lang.dlg[['status']][['16']])
                helpWidget(en.Freq2, lang.dlg[['tooltip']][['17']], lang.dlg[['status']][['17']])
            }

            if(analysisMthd == 'anomaly'){
                chk.Anom <- tkcheckbutton(fr.anMthd, variable = perc.anom, text = lang.dlg[['checkbutton']][['1']], anchor = 'w', justify = 'left')
                bt.AnomBP <- ttkbutton(fr.anMthd, text = lang.dlg[['button']][['4']])

                tkgrid(chk.Anom)
                tkgrid(bt.AnomBP)

                helpWidget(chk.Anom, lang.dlg[['tooltip']][['18']], lang.dlg[['status']][['18']])
                helpWidget(bt.AnomBP, lang.dlg[['tooltip']][['19']], lang.dlg[['status']][['19']])

                tkconfigure(bt.AnomBP, command = function(){
                    GeneralParameters$analysis$anomaly <<- getInfoBasePeriod(.cdtEnv$tcl$main$win,
                                                                             GeneralParameters$analysis$anomaly)
                })
            }

            ###############

            tkgrid(fr.anMthd, row = 2, column = 0, pady = 2, ipady = 2)
        }

        #######################

        tkbind(cb.anMthd, "<<ComboboxSelected>>", function(){
            analysisMthd <- AnalysesVAL[CbAnalysesVAL %in% str_trim(tclvalue(analysis.method))]
            analysisMethodFun(analysisMthd)
        })

        #######################

        fr.anMthd <- tkframe(frameAnalysis)

        trend.unit <- tclVar(GeneralParameters$analysis$trend$unit)
        trend.minyear <- tclVar(GeneralParameters$analysis$trend$min.year)
        mth.perc <- tclVar(GeneralParameters$analysis$percentile)
        low.thres <- tclVar(GeneralParameters$analysis$frequency$low)
        up.thres <- tclVar(GeneralParameters$analysis$frequency$up)
        perc.anom <- tclVar(GeneralParameters$analysis$anomaly$perc)

        analysisMethodFun(GeneralParameters$analysis$method)

        #############################

        AnalyzeBut <- ttkbutton(subfr2, text = lang.dlg[['button']][['5']])

        #################
        tkconfigure(AnalyzeBut, command = function(){
            GeneralParameters$in.tstep <- periodVAL[CbperiodVAL %in% str_trim(tclvalue(timeSteps))]
            GeneralParameters$in.file <- str_trim(tclvalue(file.stnfl))
            GeneralParameters$data.type <- datatypeVAL[CbdatatypeVAL %in% str_trim(tclvalue(DataType))]
            GeneralParameters$out.dir <- str_trim(tclvalue(file.save1))

            GeneralParameters$out.series$tstep <- outSeriesVAL[CboutSeriesVAL %in% str_trim(tclvalue(out.tstep))]
            GeneralParameters$out.series$start.mon <- which(MOIS %in% str_trim(tclvalue(start.mon)))
            GeneralParameters$out.series$length.mon <- as.numeric(str_trim(tclvalue(length.mon)))

            GeneralParameters$analysis$method <- AnalysesVAL[CbAnalysesVAL %in% str_trim(tclvalue(analysis.method))]

            if(GeneralParameters$analysis$method == 'trend'){
                GeneralParameters$analysis$trend$unit <- as.numeric(tclvalue(trend.unit))
                GeneralParameters$analysis$trend$min.year <- as.numeric(str_trim(tclvalue(trend.minyear)))
                if(is.na(GeneralParameters$analysis$trend$min.year)){
                    Insert.Messages.Out(lang.dlg[['message']][['1']], TRUE, 'e')
                    return(NULL)
                }
            }

            if(GeneralParameters$analysis$method == 'percentile'){
                GeneralParameters$analysis$percentile <- as.numeric(str_trim(tclvalue(mth.perc)))
                if(is.na(GeneralParameters$analysis$percentile)){
                    Insert.Messages.Out(lang.dlg[['message']][['2']], TRUE, 'e')
                    return(NULL)
                }
            }

            if(GeneralParameters$analysis$method == 'frequency'){
                GeneralParameters$analysis$frequency$low <- as.numeric(str_trim(tclvalue(low.thres)))
                GeneralParameters$analysis$frequency$up <- as.numeric(str_trim(tclvalue(up.thres)))
                if(is.na(GeneralParameters$analysis$frequency$low) |
                   is.na(GeneralParameters$analysis$frequency$up)){
                    Insert.Messages.Out(lang.dlg[['message']][['3']], TRUE, 'e')
                    return(NULL)
                }
            }

            if(GeneralParameters$analysis$method == 'anomaly'){
                GeneralParameters$analysis$anomaly$perc <- switch(tclvalue(perc.anom), '0' = FALSE, '1' = TRUE)
            }

            ##########
            # assign("GeneralParameters", GeneralParameters, envir = .GlobalEnv)

            Insert.Messages.Out(paste(lang.dlg[['message']][['4']], tclvalue(analysis.method), "......."), TRUE, "i")

            tkconfigure(.cdtEnv$tcl$main$win, cursor = 'watch')
            tcl('update')
            ret <- tryCatch({
                            spatialAnalysisProcs(GeneralParameters)
                        },
                        warning = function(w) warningFun(w),
                        error = function(e) errorFun(e),
                        finally = {
                            tkconfigure(.cdtEnv$tcl$main$win, cursor = '')
                            tcl('update')
                        })

            msg0 <- paste(tclvalue(analysis.method), lang.dlg[['message']][['5']])
            msg1 <- paste(tclvalue(analysis.method), lang.dlg[['message']][['6']])

            if(!is.null(ret)){
                if(ret == 0){
                    Insert.Messages.Out(msg0, TRUE, "s")

                    ###################

                    load.SpatialAnalysis.Data()

                }else Insert.Messages.Out(msg1, TRUE, 'e')
            }else Insert.Messages.Out(msg1, TRUE, 'e')
        })

        ##############################################
        tkgrid(frameOut, row = 0, column = 0, sticky = 'we')
        tkgrid(bt.MonAnalyze, row = 1, column = 0, sticky = 'we', pady = 3)
        tkgrid(bt.YearAnalyze, row = 2, column = 0, sticky = 'we', pady = 1)
        tkgrid(bt.AggrFun, row = 3, column = 0, sticky = 'we', pady = 3)
        tkgrid(frameAnalysis, row = 4, column = 0, sticky = 'we', pady = 1)
        tkgrid(AnalyzeBut, row = 5, column = 0, sticky = 'we', pady = 3)

    #######################################################################################################

    #Tab3
    subfr3 <- bwTabScrollableFrame(cmd.tab3)

        ##############################################

        frameAnDat <- ttklabelframe(subfr3, text = lang.dlg[['label']][['15']], relief = 'groove')

        DirExist <- tclVar(0)
        file.Stat <- tclVar()

        statedirStat <- if(tclvalue(DirExist) == "1") "normal" else "disabled"

        chk.dirStat <- tkcheckbutton(frameAnDat, variable = DirExist, text = lang.dlg[['checkbutton']][['2']], anchor = 'w', justify = 'left')
        en.dirStat <- tkentry(frameAnDat, textvariable = file.Stat, width = largeur2 + 5, state = statedirStat)
        bt.dirStat <- ttkbutton(frameAnDat, text = .cdtEnv$tcl$lang$global[['button']][['6']], state = statedirStat)

        tkgrid(chk.dirStat, row = 0, column = 0, sticky = 'we', rowspan = 1, columnspan = 4, padx = 1, pady = 1, ipadx = 1, ipady = 1)
        tkgrid(bt.dirStat, row = 0, column = 4, sticky = 'e', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
        tkgrid(en.dirStat, row = 1, column = 0, sticky = 'we', rowspan = 1, columnspan = 5, padx = 1, pady = 1, ipadx = 1, ipady = 1)

        ###############

        tkconfigure(bt.dirStat, command = function(){
            path.Stat <- tclvalue(tkgetOpenFile(initialdir = getwd(), filetypes = .cdtEnv$tcl$data$filetypes6))
            if(path.Stat %in% c("", "NA") | is.na(path.Stat)) return(NULL)
            tclvalue(file.Stat) <- path.Stat

            if(file.exists(str_trim(tclvalue(file.Stat)))){
                dirAnalysis <- try(readRDS(str_trim(tclvalue(file.Stat))), silent = TRUE)
                if(inherits(dirAnalysis, "try-error")){
                    Insert.Messages.Out(lang.dlg[['message']][['7']], TRUE, 'e')
                    Insert.Messages.Out(gsub('[\r\n]', '', dirAnalysis[1]), TRUE, 'e')
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
            statedirStat <- if(tclvalue(DirExist) == '1') 'disabled' else 'normal'
            tkconfigure(en.dirStat, state = statedirStat)
            tkconfigure(bt.dirStat, state = statedirStat)

            stateAnaBut <- if(tclvalue(DirExist) == '1') 'normal' else 'disabled'
            tcl(tknote.cmd, 'itemconfigure', cmd.tab1$IDtab, state = stateAnaBut)
            tcl(tknote.cmd, 'itemconfigure', cmd.tab2$IDtab, state = stateAnaBut)
        })

        ##############################################

        frameClimatoMap <- ttklabelframe(subfr3, text = lang.dlg[['label']][['16']], relief = 'groove')

        .cdtData$EnvData$climStat <- tclVar()
        .cdtData$EnvData$climDate <- tclVar()

        cb.climato.maps <- ttkcombobox(frameClimatoMap, values = "", textvariable = .cdtData$EnvData$climStat, justify = 'center', width = largeur5)
        bt.climato.maps <- ttkbutton(frameClimatoMap, text = .cdtEnv$tcl$lang$global[['button']][['3']], width = largeur6)
        bt.climMapOpt <- ttkbutton(frameClimatoMap, text = .cdtEnv$tcl$lang$global[['button']][['4']], width = largeur6)

        cb.climDate <- ttkcombobox(frameClimatoMap, values = "", textvariable = .cdtData$EnvData$climDate, justify = 'center', width = largeur8)
        bt.climDate.prev <- ttkbutton(frameClimatoMap, text = "<<", width = largeur7)
        bt.climDate.next <- ttkbutton(frameClimatoMap, text = ">>", width = largeur7)

        ###################
        tkgrid(cb.climato.maps, row = 0, column = 1, sticky = 'we', rowspan = 1, columnspan = 8, padx = 1, pady = 1, ipadx = 1, ipady = 1)
        tkgrid(bt.climMapOpt, row = 1, column = 0, sticky = 'we', rowspan = 1, columnspan = 5, ipadx = 1, ipady = 1)
        tkgrid(bt.climato.maps, row = 1, column = 5, sticky = 'we', rowspan = 1, columnspan = 5, ipadx = 1, ipady = 1)

        tkgrid(bt.climDate.prev, row = 2, column = 0, sticky = 'we', rowspan = 1, columnspan = 2, padx = 1, pady = 1, ipadx = 1, ipady = 1)
        tkgrid(cb.climDate, row = 2, column = 2, sticky = 'we', rowspan = 1, columnspan = 6, padx = 1, pady = 1, ipadx = 1, ipady = 1)
        tkgrid(bt.climDate.next, row = 2, column = 8, sticky = 'we', rowspan = 1, columnspan = 2, padx = 1, pady = 1, ipadx = 1, ipady = 1)

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

        ###############

        .cdtData$EnvData$tab$climMap <- NULL

        tkconfigure(bt.climato.maps, command = function(){
            if(is.null(.cdtData$EnvData$statpars)) return(NULL)

            ret <- read.ClimStat()
            if(is.null(ret)) return(NULL)

            if(!str_trim(tclvalue(.cdtData$EnvData$climStat)) %in% c("", "Anomaly"))
                spatialAnalysis.DisplayStatMaps()
        })

        tkconfigure(bt.climDate.prev, command = function(){
            if(is.null(.cdtData$EnvData$statpars)) return(NULL)

            ipos <- which(.cdtData$EnvData$statpars$stats == tclvalue(.cdtData$EnvData$climDate))
            ipos <- ipos - 1
            if(ipos < 1) ipos <- length(.cdtData$EnvData$statpars$stats)
            tclvalue(.cdtData$EnvData$climDate) <- .cdtData$EnvData$statpars$stats[ipos]

            ret <- read.ClimStat()
            if(is.null(ret)) return(NULL)

            if(!str_trim(tclvalue(.cdtData$EnvData$climStat)) %in% c("", "Anomaly"))
                spatialAnalysis.DisplayStatMaps()
        })

        tkconfigure(bt.climDate.next, command = function(){
            if(is.null(.cdtData$EnvData$statpars)) return(NULL)

            ipos <- which(.cdtData$EnvData$statpars$stats == tclvalue(.cdtData$EnvData$climDate))
            ipos <- ipos + 1
            if(ipos > length(.cdtData$EnvData$statpars$stats)) ipos <- 1
            tclvalue(.cdtData$EnvData$climDate) <- .cdtData$EnvData$statpars$stats[ipos]

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
                Insert.Messages.Out(lang.dlg[['message']][['7']], TRUE, 'e')
                Insert.Messages.Out(gsub('[\r\n]', '', params[1]), TRUE, 'e')
                tkconfigure(cb.climDate, values = "")
                tclvalue(.cdtData$EnvData$climDate) <- ""
                return(NULL)
            }
            tkconfigure(cb.climDate, values = params$stats)
            tclvalue(.cdtData$EnvData$climDate) <- params$stats[1]
            stateclimDate <- if(params$params$out.series$tstep == "monthly") "normal" else "disabled"
            tkconfigure(cb.climDate, state = stateclimDate)
            tkconfigure(bt.climDate.prev, state = stateclimDate)
            tkconfigure(bt.climDate.next, state = stateclimDate)

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

        frameTSMaps <- ttklabelframe(subfr3, text = lang.dlg[['label']][['17']], relief = 'groove')

        .cdtData$EnvData$TSDate <- tclVar()

        CbsourceTSDataVAL <- lang.dlg[['combobox']][['2']]
        sourceTSDataVAL <- c("Data", "Anomaly")
        .cdtData$EnvData$TSData <- tclVar()
        tclvalue(.cdtData$EnvData$TSData) <- CbsourceTSDataVAL[sourceTSDataVAL %in% GeneralParameters$plot$TSData]

        cb.TSDate <- ttkcombobox(frameTSMaps, values = "", textvariable = .cdtData$EnvData$TSDate, width = largeur8, justify = 'center')
        bt.TSDate.prev <- ttkbutton(frameTSMaps, text = "<<", width = largeur7)
        bt.TSDate.next <- ttkbutton(frameTSMaps, text = ">>", width = largeur7)

        bt.TSDate.plot <- ttkbutton(frameTSMaps, text = .cdtEnv$tcl$lang$global[['button']][['3']], width = largeur6)
        bt.TSMapOpt <- ttkbutton(frameTSMaps, text = .cdtEnv$tcl$lang$global[['button']][['4']], width = largeur6)

        txt.TSData <- tklabel(frameTSMaps, text = lang.dlg[['label']][['18']], anchor = 'e', justify = 'right')
        cb.TSData <- ttkcombobox(frameTSMaps, values = CbsourceTSDataVAL[1], textvariable = .cdtData$EnvData$TSData, width = largeur9, justify = 'center')

        ###################

        tkgrid(bt.TSDate.prev, row = 0, column = 0, sticky = 'we', rowspan = 1, columnspan = 2, padx = 1, pady = 1, ipadx = 1, ipady = 1)
        tkgrid(cb.TSDate, row = 0, column = 2, sticky = 'we', rowspan = 1, columnspan = 6, padx = 1, pady = 1, ipadx = 1, ipady = 1)
        tkgrid(bt.TSDate.next, row = 0, column = 8, sticky = 'we', rowspan = 1, columnspan = 2, padx = 1, pady = 1, ipadx = 1, ipady = 1)

        tkgrid(bt.TSMapOpt, row = 1, column = 0, sticky = 'we', rowspan = 1, columnspan = 5, ipadx = 1, ipady = 1)
        tkgrid(bt.TSDate.plot, row = 1, column = 5, sticky = 'we', rowspan = 1, columnspan = 5, ipadx = 1, ipady = 1)

        tkgrid(txt.TSData, row = 2, column = 0, sticky = 'e', rowspan = 1, columnspan = 3, padx = 1, pady = 1, ipadx = 1, ipady = 1)
        tkgrid(cb.TSData, row = 2, column = 3, sticky = 'we', rowspan = 1, columnspan = 4, padx = 1, pady = 1, ipadx = 1, ipady = 1)

        ###################

        tkconfigure(bt.TSMapOpt, command = function(){
            sourceTSData <- sourceTSDataVAL[CbsourceTSDataVAL %in% str_trim(tclvalue(.cdtData$EnvData$TSData))]
            if(!is.null(.cdtData$EnvData$tsdata)){
                if(sourceTSData == "Data")
                    atlevel <- pretty(.cdtData$EnvData$tsdata$z, n = 10, min.n = 7)
                if(sourceTSData == "Anomaly")
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
            sourceTSData <- sourceTSDataVAL[CbsourceTSDataVAL %in% str_trim(tclvalue(.cdtData$EnvData$TSData))]
            if(!is.null(.cdtData$EnvData$tsdata)){
                if(sourceTSData == "Data")
                    atlevel <- pretty(.cdtData$EnvData$tsdata$z, n = 10, min.n = 7)
                if(sourceTSData == "Anomaly")
                    atlevel <- pretty(.cdtData$EnvData$anomData$z, n = 10, min.n = 7)
                .cdtData$EnvData$TSMapOp$userLvl$levels <- atlevel
            }
        })

        ##############################################

        framePlotType <- tkframe(subfr3)

        .cdtData$EnvData$plot.maps$plot.type <- tclVar("Pixels")

        txt.plotType <- tklabel(framePlotType, text = lang.dlg[['label']][['19']], anchor = 'e', justify = 'right')
        cb.plotType <- ttkcombobox(framePlotType, values = "Pixels", textvariable = .cdtData$EnvData$plot.maps$plot.type, justify = 'center', width = largeur9)

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

        frameTSPlot <- ttklabelframe(subfr4, text = lang.dlg[['label']][['20']], relief = 'groove')
 
        CbtypeTSPLOTVAL <- lang.dlg[['combobox']][['3']]
        typeTSPLOTVAL <- c('line', 'bar', 'proba', 'eline', 'ebar', 'eproba', 'anom')
        typeTSp <- tclVar()
        tclvalue(typeTSp) <- CbtypeTSPLOTVAL[typeTSPLOTVAL %in% GeneralParameters$plot$typeTSp]

        .cdtData$EnvData$plot.maps$typeTSp <- GeneralParameters$plot$typeTSp

        .cdtData$EnvData$plot.maps$averageTSp <- tclVar(FALSE)
        .cdtData$EnvData$plot.maps$tercileTSp <- tclVar(FALSE)
        .cdtData$EnvData$plot.maps$trendTSp <- tclVar(FALSE)

        stateType <- if(GeneralParameters$plot$typeTSp %in% c('line', 'eline')) "normal" else "disabled"

        cb.typeTSp <- ttkcombobox(frameTSPlot, values = CbtypeTSPLOTVAL, textvariable = typeTSp, justify = 'center', width = largeur10)
        bt.TsGraph.plot <- ttkbutton(frameTSPlot, text = .cdtEnv$tcl$lang$global[['button']][['3']], width = largeur6)
        bt.TSGraphOpt <- ttkbutton(frameTSPlot, text = .cdtEnv$tcl$lang$global[['button']][['4']], width = largeur6)

        frTS1 <- tkframe(frameTSPlot)
        chk.meanTSp <- tkcheckbutton(frTS1, variable = .cdtData$EnvData$plot.maps$averageTSp, text = lang.dlg[['checkbutton']][['3']], anchor = 'w', justify = 'left', state = stateType)
        chk.tercTSp <- tkcheckbutton(frTS1, variable = .cdtData$EnvData$plot.maps$tercileTSp, text = lang.dlg[['checkbutton']][['4']], anchor = 'w', justify = 'left', state = stateType)
        chk.trendTSp <- tkcheckbutton(frTS1, variable = .cdtData$EnvData$plot.maps$trendTSp, text = lang.dlg[['checkbutton']][['5']], anchor = 'w', justify = 'left', state = stateType)
        tkgrid(chk.meanTSp, chk.tercTSp, chk.trendTSp)

        #################

        tkgrid(cb.typeTSp, row = 0, column = 1, sticky = 'we', rowspan = 1, columnspan = 8, padx = 1, pady = 1, ipadx = 1, ipady = 1)
        tkgrid(bt.TSGraphOpt, row = 1, column = 0, sticky = 'we', rowspan = 1, columnspan = 5, padx = 1, pady = 1, ipadx = 1, ipady = 1)
        tkgrid(bt.TsGraph.plot, row = 1, column = 5, sticky = 'we', rowspan = 1, columnspan = 5, padx = 1, pady = 1, ipadx = 1, ipady = 1)
        tkgrid(frTS1, row = 2, column = 0, sticky = '', rowspan = 1, columnspan = 10, padx = 1, pady = 1, ipadx = 1, ipady = 1)

        #################

        tkconfigure(bt.TSGraphOpt, command = function(){
            vtypeTSp <- typeTSPLOTVAL[CbtypeTSPLOTVAL %in% str_trim(tclvalue(typeTSp))]
            suffix.fun <- switch(vtypeTSp,
                                 'anom' = "Anomaly",
                                 'bar' = "Bar",
                                 'line' = "Line",
                                 'proba' = "Proba",
                                 'eline' = "LineENSO",
                                 'ebar' = "BarENSO",
                                 'eproba' = "ProbaENSO")
            plot.fun <- get(paste0("MapGraph.GraphOptions.", suffix.fun), mode = "function")
            .cdtData$EnvData$TSGraphOp <- plot.fun(.cdtData$EnvData$TSGraphOp)
        })

        #################

        .cdtData$EnvData$tab$TSplot <- NULL

        tkconfigure(bt.TsGraph.plot, command = function(){
            .cdtData$EnvData$plot.maps$typeTSp <- typeTSPLOTVAL[CbtypeTSPLOTVAL %in% str_trim(tclvalue(typeTSp))]

            if(!is.null(.cdtData$EnvData$tsdata)){
                imgContainer <- CDT.Display.Graph(spatialAnalysis.plotTSGraph, .cdtData$EnvData$tab$TSplot, 'Time-Series-Plot')
                .cdtData$EnvData$tab$TSplot <- imageNotebookTab_unik(imgContainer, .cdtData$EnvData$tab$TSplot)
            }
        })

        #################

        tkbind(cb.typeTSp, "<<ComboboxSelected>>", function(){
            .cdtData$EnvData$plot.maps$typeTSp <- typeTSPLOTVAL[CbtypeTSPLOTVAL %in% str_trim(tclvalue(typeTSp))]

            stateType <- if(.cdtData$EnvData$plot.maps$typeTSp %in% c('line', 'eline')) "normal" else "disabled"
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

        frameSTNCrds <- ttklabelframe(subfr4, text = lang.dlg[['label']][['21']], relief = 'groove')

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

        frameSHP <- ttklabelframe(subfr5, text = lang.dlg[['label']][['22']], relief = 'groove')

        .cdtData$EnvData$shp$add.shp <- tclVar(FALSE)
        file.plotShp <- tclVar()
        stateSHP <- "disabled"

        chk.addshp <- tkcheckbutton(frameSHP, variable = .cdtData$EnvData$shp$add.shp, text = lang.dlg[['checkbutton']][['6']], anchor = 'w', justify = 'left')
        bt.addshpOpt <- ttkbutton(frameSHP, text = .cdtEnv$tcl$lang$global[['button']][['4']], state = stateSHP)
        cb.addshp <- ttkcombobox(frameSHP, values = unlist(listOpenFiles), textvariable = file.plotShp, width = largeur1, state = stateSHP)
        bt.addshp <- tkbutton(frameSHP, text = "...", state = stateSHP)

        ########
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
                lapply(list(cb.stnfl, cb.addshp), tkconfigure, values = unlist(listOpenFiles))

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

        if(.cdtData$EnvData$statpars$params$data.type == "cdtstation"){
            stnIDTSPLOT <- .cdtData$EnvData$tsdata$id
            txt.stnSel <- tklabel(frTS2, text = lang.dlg[['label']][['23']])
            bt.stnID.prev <- ttkbutton(frTS2, text = "<<", width = largeur7)
            bt.stnID.next <- ttkbutton(frTS2, text = ">>", width = largeur7)
            cb.stnID <- ttkcombobox(frTS2, values = stnIDTSPLOT, textvariable = .cdtData$EnvData$plot.maps$stnIDTSp, justify = 'center', width = largeur11)
            tclvalue(.cdtData$EnvData$plot.maps$stnIDTSp) <- stnIDTSPLOT[1]

            tkgrid(txt.stnSel, row = 0, column = 0, sticky = '', rowspan = 1, columnspan = 3, padx = 1, pady = 1, ipadx = 1, ipady = 1)
            tkgrid(bt.stnID.prev, row = 1, column = 0, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
            tkgrid(cb.stnID, row = 1, column = 1, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
            tkgrid(bt.stnID.next, row = 1, column = 2, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)

            ############
            tkconfigure(bt.stnID.prev, command = function(){
                .cdtData$EnvData$plot.maps$typeTSp <- typeTSPLOTVAL[CbtypeTSPLOTVAL %in% str_trim(tclvalue(typeTSp))]

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
                .cdtData$EnvData$plot.maps$typeTSp <- typeTSPLOTVAL[CbtypeTSPLOTVAL %in% str_trim(tclvalue(typeTSp))]

                if(!is.null(.cdtData$EnvData$tsdata)){
                    istn <- which(stnIDTSPLOT == str_trim(tclvalue(.cdtData$EnvData$plot.maps$stnIDTSp)))
                    istn <- istn + 1
                    if(istn > length(stnIDTSPLOT)) istn <- 1
                    tclvalue(.cdtData$EnvData$plot.maps$stnIDTSp) <- stnIDTSPLOT[istn]

                    imgContainer <- CDT.Display.Graph(spatialAnalysis.plotTSGraph, .cdtData$EnvData$tab$TSplot, 'Time-Series-Plot')
                    .cdtData$EnvData$tab$TSplot <- imageNotebookTab_unik(imgContainer, .cdtData$EnvData$tab$TSplot)
                }
            })
        }else{
            txt.crdSel <- tklabel(frTS2, text = lang.dlg[['label']][['24']], anchor = 'w', justify = 'left')
            txt.lonLoc <- tklabel(frTS2, text = lang.dlg[['label']][['25']], anchor = 'e', justify = 'right')
            en.lonLoc <- tkentry(frTS2, textvariable = .cdtData$EnvData$plot.maps$lonLOC, width = largeur12)
            txt.latLoc <- tklabel(frTS2, text = lang.dlg[['label']][['26']], anchor = 'e', justify = 'right')
            en.latLoc <- tkentry(frTS2, textvariable = .cdtData$EnvData$plot.maps$latLOC, width = largeur12)
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

            .cdtData$EnvData$climMapOp$pointSize <- 1.0
            .cdtData$EnvData$TSMapOp$pointSize <- 1.0
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

        TSDATA <- if("Anomaly" %in% .cdtData$EnvData$DirStat$Stats) CbsourceTSDataVAL else CbsourceTSDataVAL[1]
        tkconfigure(cb.TSData, values = TSDATA)
        tclvalue(.cdtData$EnvData$TSData) <- CbsourceTSDataVAL[1]

        ###################
        analysis.path <- file.path(.cdtData$EnvData$PathStat, tclvalue(.cdtData$EnvData$climStat))
        params <- try(readRDS(file.path(analysis.path, "params.rds")), silent = TRUE)
        if(inherits(params, "try-error")){
            Insert.Messages.Out(lang.dlg[['message']][['7']], TRUE, 'e')
            Insert.Messages.Out(gsub('[\r\n]', '', params[1]), TRUE, 'e')
            tkconfigure(cb.climDate, values = "")
            tclvalue(.cdtData$EnvData$climDate) <- ""
            return(NULL)
        }
        tkconfigure(cb.climDate, values = params$stats)
        tclvalue(.cdtData$EnvData$climDate) <- params$stats[1]
        stateclimDate <- if(params$params$out.series$tstep == "monthly") "normal" else "disabled"
        tkconfigure(cb.climDate, state = stateclimDate)
        tkconfigure(bt.climDate.prev, state = stateclimDate)
        tkconfigure(bt.climDate.next, state = stateclimDate)

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

        filestat <- file.path(analysis.path, paste0(.cdtData$EnvData$statpars$params$analysis$method,
                              "_", tclvalue(.cdtData$EnvData$climDate), extFile))
        if(!file.exists(filestat)){
            Insert.Messages.Out(paste(filestat, lang.dlg[['message']][['8']]), TRUE, 'e')
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
                Insert.Messages.Out(paste(filetsdata, lang.dlg[['message']][['8']]), TRUE, 'e')
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
                    Insert.Messages.Out(paste(file.anom, lang.dlg[['message']][['8']]), TRUE, 'e')
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
                Insert.Messages.Out(paste(filetsdata, lang.dlg[['message']][['8']]), TRUE, 'e')
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
