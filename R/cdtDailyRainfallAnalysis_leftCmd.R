
dailyRainAnalysisPanelCmd <- function(){
    if(WindowsOS()){
        largeur0 <- 29
        largeur1 <- 33
        largeur2 <- 36
        largeur3 <- 24
        largeur4 <- 28
        largeur5 <- 14
        largeur6 <- 9
        largeur7 <- 7
        largeur8 <- 19
        largeur9 <- 20
        largeur10 <- 30
        largeur11 <- 10
    }else{
        largeur0 <- 30
        largeur1 <- 32
        largeur2 <- 33
        largeur3 <- 24
        largeur4 <- 26
        largeur5 <- 14
        largeur6 <- 11
        largeur7 <- 7
        largeur8 <- 18
        largeur9 <- 19
        largeur10 <- 30
        largeur11 <- 10
    }

    MOIS <- format(ISOdate(2014, 1:12, 1), "%b")
    varsname <- list(name = c("TOTALRAIN", "RAININT", "WETDAY", "DRYDAY", "WETSPELL", "DRYSPELL"),
                     longname = c('Total Rainfall', 'Rainfall Intensity', 'Number of Wet Days',
                                  'Number of Dry Days', 'Number of Wet Spells', 'Number of Dry Spells'))
    statsname <- list(name = c('mean', 'stdev', 'coefvar', 'proba'),
                      longname = c('Mean', 'Standard deviation', 'Coefficient of variation',
                                   'Probability of exceeding'))

    GeneralParameters <- list(data.type = "cdtstation", cdtstation = "", cdtdataset = "",
                              seas = list(all.years = TRUE,
                                          startYear = 1981, startMon = 9, startDay = 1,
                                          endYear = 2021, endMon = 12, endDay = 31,
                                          min.frac = 0.95),
                              stats = list(daily = 'tot.rain', yearly = 'mean'),
                              def = list(drywet.day = 0.85, drywet.spell = 7, proba.thres = 400),
                              output = "")

    GeneralParameters$plot <- list(typeTSp = "line")

    .cdtData$EnvData$varstatMapOp <- list(presetCol = list(color = 'tim.colors', reverse = FALSE),
                                          userCol = list(custom = FALSE, color = NULL),
                                          userLvl = list(custom = FALSE, levels = NULL, equidist = FALSE),
                                          title = list(user = FALSE, title = ''),
                                          colkeyLab = list(user = FALSE, label = ''),
                                          scalebar = list(add = FALSE, pos = 'bottomleft'),
                                          plotType = list(values = c("Pixels", "Points"), var = "Pixels"),
                                          pointSize = 1.0, bbox = .cdtData$Config$region)

    .cdtData$EnvData$dataMapOp <- list(presetCol = list(color = 'tim.colors', reverse = FALSE),
                                       userCol = list(custom = FALSE, color = NULL),
                                       userLvl = list(custom = FALSE, levels = NULL, equidist = FALSE),
                                       title = list(user = FALSE, title = ''),
                                       colkeyLab = list(user = FALSE, label = ''),
                                       scalebar = list(add = FALSE, pos = 'bottomleft'),
                                       plotType = list(values = c("Pixels", "Points"), var = "Pixels"),
                                       pointSize = 1.0, bbox = .cdtData$Config$region)

    .cdtData$EnvData$TSGraphOp <- list(
                        anomaly = list(
                                anom = list(perc.anom = FALSE, basePeriod = FALSE, startYr.anom = 1991, endYr.anom = 2020),
                                xlim = list(is.min = FALSE, min = 1981, is.max = FALSE, max = 2021),
                                ylim = list(is.min = FALSE, min = -100, is.max = FALSE, max = 100),
                                axislabs = list(is.xlab = FALSE, xlab = '', is.ylab = FALSE, ylab = ''),
                                title = list(is.title = FALSE, title = '', position = 'top'),
                                colors = list(negative = "blue", positive = "red")
                            ),
                        bar = list(
                            xlim = list(is.min = FALSE, min = 1981, is.max = FALSE, max = 2021),
                            ylim = list(is.min = FALSE, min = 0, is.max = FALSE, max = 100),
                            axislabs = list(is.xlab = FALSE, xlab = '', is.ylab = FALSE, ylab = ''),
                            title = list(is.title = FALSE, title = '', position = 'top'),
                            colors = list(col = "darkblue")
                        ),
                        line = list(
                            xlim = list(is.min = FALSE, min = 1981, is.max = FALSE, max = 2021),
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
                            proba = list(theoretical = FALSE, col = 'black', lwd = 2)
                        )
                    )

    ###################

    xml.dlg <- file.path(.cdtDir$dirLocal, "languages", "cdtDailyRainfallAnalysis_leftCmd.xml")
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

        ############################################

        frameInData <- ttklabelframe(subfr1, text = lang.dlg[['label']][['1']], relief = 'groove')

        DataType <- tclVar()
        CbdatatypeVAL <- .cdtEnv$tcl$lang$global[['combobox']][['2']][1:2]
        datatypeVAL <- c('cdtstation', 'cdtdataset')
        tclvalue(DataType) <- CbdatatypeVAL[datatypeVAL %in% GeneralParameters$data.type]

        if(GeneralParameters$data.type == 'cdtstation'){
            input.Prec <- tclVar(GeneralParameters$cdtstation)
            txt.INPrec <- lang.dlg[['label']][['2']]
        }else{
            input.Prec <- tclVar(GeneralParameters$cdtdataset)
            txt.INPrec <- lang.dlg[['label']][['3']]
        }
        txt.INPrec.var <- tclVar(txt.INPrec)

        txt.datatype <- tklabel(frameInData, text = lang.dlg[['label']][['4']], anchor = 'w', justify = 'left')
        cb.datatype <- ttkcombobox(frameInData, values = CbdatatypeVAL, textvariable = DataType, width = largeur0, justify = 'center')

        txt.INPrec <- tklabel(frameInData, text = tclvalue(txt.INPrec.var), textvariable = txt.INPrec.var, anchor = 'w', justify = 'left')
        if(GeneralParameters$data.type == 'cdtstation'){
            cb.en.INPrec <- ttkcombobox(frameInData, values = unlist(openFile_ttkcomboList()), textvariable = input.Prec, width = largeur1)
            addTo_all_Combobox_List(cb.en.INPrec)
        }else{
            cb.en.INPrec <- tkentry(frameInData, textvariable = input.Prec, width = largeur2)
        }
        bt.INPrec <- tkbutton(frameInData, text = "...")

        ############
        tkgrid(txt.datatype, row = 0, column = 0, sticky = 'we', rowspan = 1, columnspan = 2, padx = 1, pady = 1, ipadx = 1, ipady = 1)
        tkgrid(cb.datatype, row = 0, column = 2, sticky = 'we', rowspan = 1, columnspan = 8, padx = 1, pady = 1, ipadx = 1, ipady = 1)
        tkgrid(txt.INPrec, row = 1, column = 0, sticky = 'we', rowspan = 1, columnspan = 10, padx = 1, pady = 1, ipadx = 1, ipady = 1)
        tkgrid(cb.en.INPrec, row = 2, column = 0, sticky = 'we', rowspan = 1, columnspan = 9, padx = 0, pady = 1, ipadx = 1, ipady = 1)
        tkgrid(bt.INPrec, row = 2, column = 9, sticky = 'w', rowspan = 1, columnspan = 1, padx = 0, pady = 1, ipadx = 1, ipady = 1)

        ############

        helpWidget(cb.datatype, lang.dlg[['tooltip']][['1']], lang.dlg[['status']][['1']])

        if(GeneralParameters$data.type == 'cdtstation'){
            helpWidget(cb.en.INPrec, lang.dlg[['tooltip']][['2']], lang.dlg[['status']][['2']])
            helpWidget(bt.INPrec, lang.dlg[['tooltip']][['4']], lang.dlg[['status']][['4']])
        }else{
            helpWidget(cb.en.INPrec, lang.dlg[['tooltip']][['3']], lang.dlg[['status']][['3']])
            helpWidget(bt.INPrec, lang.dlg[['tooltip']][['5']], lang.dlg[['status']][['5']])
        }

        ############

        tkconfigure(bt.INPrec, command = function(){
            data_type <- datatypeVAL[CbdatatypeVAL %in% trimws(tclvalue(DataType))]
            if(data_type == 'cdtstation'){
                dat.opfiles <- getOpenFiles(.cdtEnv$tcl$main$win)
                if(!is.null(dat.opfiles)){
                    update.OpenFiles('ascii', dat.opfiles)
                    tclvalue(input.Prec) <- dat.opfiles[[1]]
                }
            }else{
                path.rds <- tclvalue(tkgetOpenFile(initialdir = getwd(), filetypes = .cdtEnv$tcl$data$filetypes6))
                tclvalue(input.Prec) <- if(path.rds %in% c("", "NA") | is.na(path.rds)) "" else path.rds
            }
        })

        ############

        tkbind(cb.datatype, "<<ComboboxSelected>>", function(){
            tkdestroy(cb.en.INPrec)
            tclvalue(input.Prec) <- ''

            data_type <- datatypeVAL[CbdatatypeVAL %in% trimws(tclvalue(DataType))]
            set.plot.type(data_type)

            ###
            if(data_type == 'cdtstation'){
                tclvalue(txt.INPrec.var) <- lang.dlg[['label']][['2']]

                cb.en.INPrec <<- ttkcombobox(frameInData, values = unlist(openFile_ttkcomboList()), textvariable = input.Prec, width = largeur1)
                addTo_all_Combobox_List(cb.en.INPrec)

                ######
                tkconfigure(bt.INPrec, command = function(){
                    dat.opfiles <- getOpenFiles(.cdtEnv$tcl$main$win)
                    if(!is.null(dat.opfiles)){
                        update.OpenFiles('ascii', dat.opfiles)
                        tclvalue(input.Prec) <- dat.opfiles[[1]]
                    }
                })

                ######
                helpWidget(cb.en.INPrec, lang.dlg[['tooltip']][['2']], lang.dlg[['status']][['2']])
                helpWidget(bt.INPrec, lang.dlg[['tooltip']][['4']], lang.dlg[['status']][['4']])
            }

            ###
            if(data_type == 'cdtdataset'){
                tclvalue(txt.INPrec.var) <- lang.dlg[['label']][['3']]

                cb.en.INPrec <<- tkentry(frameInData, textvariable = input.Prec, width = largeur2)

                ######
                tkconfigure(bt.INPrec, command = function(){
                    path.rds <- tclvalue(tkgetOpenFile(initialdir = getwd(), filetypes = .cdtEnv$tcl$data$filetypes6))
                    tclvalue(input.Prec) <- if(path.rds %in% c("", "NA")) "" else path.rds
                })

                ######
                helpWidget(cb.en.INPrec, lang.dlg[['tooltip']][['3']], lang.dlg[['status']][['3']])
                helpWidget(bt.INPrec, lang.dlg[['tooltip']][['5']], lang.dlg[['status']][['5']])
            }

            #######
            tkgrid(cb.en.INPrec, row = 2, column = 0, sticky = 'we', rowspan = 1, columnspan = 9, padx = 0, pady = 1, ipadx = 1, ipady = 1)
        })

        ############################################

        bt.defineSeas <- ttkbutton(subfr1, text = lang.dlg[['button']][['1']])

        tkconfigure(bt.defineSeas, command = function(){
            GeneralParameters$seas <<- getInfoDaily2Season(.cdtEnv$tcl$main$win,
                                                           GeneralParameters$seas)
        })

        ############################################

        frameDirSav <- tkframe(subfr1, relief = 'groove', borderwidth = 2)

        dir.save <- tclVar(GeneralParameters$output)

        txt.dir.save <- tklabel(frameDirSav, text = lang.dlg[['label']][['5']], anchor = 'w', justify = 'left')
        en.dir.save <- tkentry(frameDirSav, textvariable = dir.save, width = largeur2)
        bt.dir.save <- tkbutton(frameDirSav, text = "...")

        ######
        tkgrid(txt.dir.save, row = 0, column = 0, sticky = 'we', rowspan = 1, columnspan = 5, padx = 1, pady = 0, ipadx = 1, ipady = 1)
        tkgrid(en.dir.save, row = 1, column = 0, sticky = 'we', rowspan = 1, columnspan = 4, padx = 0, pady = 0, ipadx = 1, ipady = 1)
        tkgrid(bt.dir.save, row = 1, column = 4, sticky = 'w', rowspan = 1, columnspan = 1, padx = 0, pady = 0, ipadx = 1, ipady = 1)

        helpWidget(en.dir.save, lang.dlg[['tooltip']][['6']], lang.dlg[['status']][['6']])
        helpWidget(bt.dir.save, lang.dlg[['tooltip']][['5']], lang.dlg[['status']][['5']])

        ######
        tkconfigure(bt.dir.save, command = function() fileORdir2Save(dir.save, isFile = FALSE))

        ############################################

        tkgrid(frameInData, row = 0, column = 0, sticky = 'we', padx = 1, pady = 3, ipadx = 1, ipady = 1)
        tkgrid(bt.defineSeas, row = 1, column = 0, sticky = 'we', padx = 1, pady = 3, ipadx = 1, ipady = 1)
        tkgrid(frameDirSav, row = 2, column = 0, sticky = 'we', padx = 1, pady = 3, ipadx = 1, ipady = 1)

    #######################################################################################################

    #Tab2
    subfr2 <- bwTabScrollableFrame(cmd.tab2)

        ##############################################

        frameStats <- ttklabelframe(subfr2, text = lang.dlg[['label']][['6']], relief = 'groove')

        daily.Stats <- tclVar()
        CbDailyStatsVAL <- lang.dlg[['combobox']][['1']]
        DailyStatsVAL <- c('tot.rain', 'rain.int', 'nb.wet.day', 'nb.dry.day', 'nb.wet.spell', 'nb.dry.spell')
        tclvalue(daily.Stats) <- CbDailyStatsVAL[DailyStatsVAL %in% GeneralParameters$stats$daily]

        yearly.Stats <- tclVar()
        CbYearlyStatsVAL <- lang.dlg[['combobox']][['2']]
        YearlyStatsVAL <- c('mean', 'stdev', 'coefvar', 'proba')
        tclvalue(yearly.Stats) <- CbYearlyStatsVAL[YearlyStatsVAL %in% GeneralParameters$stats$yearly]

        txt.StatDay <- tklabel(frameStats, text = lang.dlg[['label']][['7']], anchor = 'w', justify = 'left')
        cb.StatDay <- ttkcombobox(frameStats, values = CbDailyStatsVAL, textvariable = daily.Stats, justify = 'center', width = largeur3)

        txt.StatYear <- tklabel(frameStats, text = lang.dlg[['label']][['8']], anchor = 'w', justify = 'left')
        cb.StatYear <- ttkcombobox(frameStats, values = CbYearlyStatsVAL, textvariable = yearly.Stats, justify = 'center', width = largeur3)

        ########

        sepLabStat <- tklabel(frameStats, width = largeur6)

        tkgrid(txt.StatDay, row = 0, column = 0, sticky = 'we', rowspan = 1, columnspan = 8, padx = 1, pady = 1, ipadx = 1, ipady = 1)
        tkgrid(cb.StatDay, row = 1, column = 3, sticky = 'e', rowspan = 1, columnspan = 5, padx = 1, pady = 1, ipadx = 1, ipady = 1)
        tkgrid(txt.StatYear, row = 2, column = 0, sticky = 'we', rowspan = 1, columnspan = 8, padx = 1, pady = 1, ipadx = 1, ipady = 1)
        tkgrid(sepLabStat, row = 3, column = 0, sticky = 'we', rowspan = 1, columnspan = 3, padx = 1, pady = 1, ipadx = 1, ipady = 1)
        tkgrid(cb.StatYear, row = 3, column = 3, sticky = 'e', rowspan = 1, columnspan = 5, padx = 1, pady = 1, ipadx = 1, ipady = 1)

        ##############################################

        frameDryDay <- ttklabelframe(subfr2, text = lang.dlg[['label']][['9']], relief = 'groove')

        drywet.day <- tclVar(GeneralParameters$def$drywet.day)

        txt.DryDay1 <- tklabel(frameDryDay, text = lang.dlg[['label']][['10']], anchor = 'e', justify = 'right')
        en.DryDay <- tkentry(frameDryDay, textvariable = drywet.day, width = 5, justify = 'center')
        txt.DryDay2 <- tklabel(frameDryDay, text = lang.dlg[['label']][['11']], anchor = 'w', justify = 'left')

        tkgrid(txt.DryDay1, en.DryDay, txt.DryDay2)

        ##############################################

        frameDrySpell <- ttklabelframe(subfr2, text = lang.dlg[['label']][['12']], relief = 'groove')

        drywet.spell <- tclVar(GeneralParameters$def$drywet.spell)

        txt.DrySpell1 <- tklabel(frameDrySpell, text = lang.dlg[['label']][['13']], anchor = 'e', justify = 'right')
        en.DrySpell <- tkentry(frameDrySpell, textvariable = drywet.spell, width = 3, justify = 'center')
        txt.DrySpell2 <- tklabel(frameDrySpell, text = lang.dlg[['label']][['14']], anchor = 'w', justify = 'left')

        tkgrid(txt.DrySpell1, en.DrySpell, txt.DrySpell2)

        ##############################################

        frameProba <- tkframe(subfr2)

        INITIAL.VAL <- c(400, 10, 30, 30, 5, 5)
        UNIT.TXT <- lang.dlg[['combobox']][['3']]

        txt.units.thres <- UNIT.TXT[DailyStatsVAL %in% GeneralParameters$stats$daily]

        proba.thres <- tclVar(GeneralParameters$def$proba.thres)
        units.thres <- tclVar(txt.units.thres)
        stateProba <- if(GeneralParameters$stats$yearly == 'proba') 'normal' else 'disabled'

        txt.Proba1 <- tklabel(frameProba, text = lang.dlg[['label']][['15']], anchor = 'e', justify = 'right')
        en.Proba <- tkentry(frameProba, textvariable = proba.thres, width = 5, state = stateProba, justify = 'center')
        txt.Proba2 <- tklabel(frameProba, text = tclvalue(units.thres), textvariable = units.thres, anchor = 'w', justify = 'left')

        tkgrid(txt.Proba1, en.Proba, txt.Proba2)

        ###################

        tkbind(cb.StatYear, "<<ComboboxSelected>>", function(){
            stateProba <- if(trimws(tclvalue(yearly.Stats)) == CbYearlyStatsVAL[4]) 'normal' else 'disabled'
            tkconfigure(en.Proba, state = stateProba)

            if(trimws(tclvalue(yearly.Stats)) == CbYearlyStatsVAL[4]){
                tclvalue(units.thres) <- UNIT.TXT[CbDailyStatsVAL %in% trimws(tclvalue(daily.Stats))]
                tclvalue(proba.thres) <- INITIAL.VAL[CbDailyStatsVAL %in% trimws(tclvalue(daily.Stats))]
            }
        })

        tkbind(cb.StatDay, "<<ComboboxSelected>>", function(){
            if(trimws(tclvalue(yearly.Stats)) == CbYearlyStatsVAL[4]){
                tclvalue(units.thres) <- UNIT.TXT[CbDailyStatsVAL %in% trimws(tclvalue(daily.Stats))]
                tclvalue(proba.thres) <- INITIAL.VAL[CbDailyStatsVAL %in% trimws(tclvalue(daily.Stats))]
            }
        })

        ##############################################

        bt.CalcDaily <- ttkbutton(subfr2, text = lang.dlg[['button']][['2']])

        tkconfigure(bt.CalcDaily, command = function(){
            GeneralParameters$data.type <- datatypeVAL[CbdatatypeVAL %in% trimws(tclvalue(DataType))]

            if(GeneralParameters$data.type == 'cdtstation')
                GeneralParameters$cdtstation <- trimws(tclvalue(input.Prec))

            if(GeneralParameters$data.type == 'cdtdataset')
                GeneralParameters$cdtdataset <- trimws(tclvalue(input.Prec))

            GeneralParameters$output <- trimws(tclvalue(dir.save))

            GeneralParameters$stats$daily <- DailyStatsVAL[CbDailyStatsVAL %in% trimws(tclvalue(daily.Stats))]
            GeneralParameters$stats$yearly <- YearlyStatsVAL[CbYearlyStatsVAL %in% trimws(tclvalue(yearly.Stats))]

            GeneralParameters$def$drywet.day <- as.numeric(trimws(tclvalue(drywet.day)))
            GeneralParameters$def$drywet.spell <- as.numeric(trimws(tclvalue(drywet.spell)))
            GeneralParameters$def$proba.thres <- as.numeric(trimws(tclvalue(proba.thres)))

            ##########
            # assign('GeneralParameters', GeneralParameters, envir = .GlobalEnv)

            analysis.method <- paste(trimws(tclvalue(daily.Stats)), ":", trimws(tclvalue(yearly.Stats)))
            Insert.Messages.Out(paste(lang.dlg[['message']][['1']], analysis.method, "......."), TRUE, "i")

            tkconfigure(.cdtEnv$tcl$main$win, cursor = 'watch')
            tcl('update')
            ret <- tryCatch2(
                {
                    dailyRainAnalysisCalcProcs(GeneralParameters)
                },
                error = function(e) errorFun(e),
                finally = {
                    tkconfigure(.cdtEnv$tcl$main$win, cursor = '')
                    tcl('update')
                }
            )

            msg0 <- paste(analysis.method, lang.dlg[['message']][['2']])
            msg1 <- paste(analysis.method, lang.dlg[['message']][['3']])

            if(!is.null(ret)){
                if(ret == 0){
                    Insert.Messages.Out(msg0, TRUE, "s")

                    .cdtData$EnvData$plot.maps[c('lon', 'lat', 'id')] <- .cdtData$EnvData$output$data[c('lon', 'lat', 'id')]
                    set.plot.type(.cdtData$EnvData$output$params$data.type)
                    ###################

                    set.Data.VarStat.Dates_1st()
                    widgets.Station.Pixel()
                    res1 <- try(read.Data.MapVarStat(), silent = TRUE)
                    if(inherits(res1, "try-error") | is.null(res1)) return(NULL)
                    res2 <- try(read.Data.MapVarTS(), silent = TRUE)
                    if(inherits(res2, "try-error") | is.null(res2)) return(NULL)
                }else Insert.Messages.Out(msg1, TRUE, 'e')
            }else Insert.Messages.Out(msg1, TRUE, 'e')
        })

        ############################################

        tkgrid(frameStats, row = 0, column = 0, sticky = 'we', padx = 1, pady = 1, ipadx = 1, ipady = 1)
        tkgrid(frameDryDay, row = 1, column = 0, sticky = 'we', padx = 1, pady = 3, ipadx = 1, ipady = 2)
        tkgrid(frameDrySpell, row = 2, column = 0, sticky = 'we', padx = 1, pady = 3, ipadx = 1, ipady = 2)
        tkgrid(frameProba, row = 3, column = 0, sticky = 'e', padx = 1, pady = 3, ipadx = 1, ipady = 2)
        tkgrid(bt.CalcDaily, row = 4, column = 0, sticky = 'we', padx = 1, pady = 3, ipadx = 1, ipady = 1)

    #######################################################################################################

    #Tab3
    subfr3 <- bwTabScrollableFrame(cmd.tab3)

        ##############################################

        frameDataExist <- ttklabelframe(subfr3, text = lang.dlg[['label']][['16']], relief = 'groove')

        DirExist <- tclVar(0)
        file.dataIndex <- tclVar()

        stateExistData <- if(tclvalue(DirExist) == "1") "normal" else "disabled"

        chk.dataIdx <- tkcheckbutton(frameDataExist, variable = DirExist, text = lang.dlg[['checkbutton']][['1']], anchor = 'w', justify = 'left')
        en.dataIdx <- tkentry(frameDataExist, textvariable = file.dataIndex, width = largeur2 + 5, state = stateExistData)
        bt.dataIdx <- ttkbutton(frameDataExist, text = .cdtEnv$tcl$lang$global[['button']][['6']], state = stateExistData)

        tkgrid(chk.dataIdx, row = 0, column = 0, sticky = 'we', rowspan = 1, columnspan = 4, padx = 1, pady = 1, ipadx = 1, ipady = 1)
        tkgrid(bt.dataIdx, row = 0, column = 4, sticky = 'e', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
        tkgrid(en.dataIdx, row = 1, column = 0, sticky = 'we', rowspan = 1, columnspan = 5, padx = 1, pady = 1, ipadx = 1, ipady = 1)

        ###############

        tkconfigure(bt.dataIdx, command = function(){
            path.dataIdx <- tclvalue(tkgetOpenFile(initialdir = getwd(), filetypes = .cdtEnv$tcl$data$filetypes6))
            if(path.dataIdx %in% c("", "NA") | is.na(path.dataIdx)) return(NULL)
            tclvalue(file.dataIndex) <- path.dataIdx

            if(file.exists(trimws(tclvalue(file.dataIndex)))){
                OutIndexdata <- try(readRDS(trimws(tclvalue(file.dataIndex))), silent = TRUE)
                if(inherits(OutIndexdata, "try-error")){
                    Insert.Messages.Out(lang.dlg[['message']][['4']], TRUE, 'e')
                    Insert.Messages.Out(gsub('[\r\n]', '', OutIndexdata[1]), TRUE, 'e')

                    tkconfigure(cb.varstat.var, values = "")
                    tclvalue(.cdtData$EnvData$anaVars) <- ""
                    tkconfigure(cb.varstat.stat, values = "")
                    tclvalue(.cdtData$EnvData$anaStat) <- ""
                    tkconfigure(cb.data.Index, values = "")
                    tclvalue(.cdtData$EnvData$donDate) <- ""
                    return(NULL)
                }

                .cdtData$EnvData$output <- OutIndexdata
                .cdtData$EnvData$PathData <- dirname(trimws(tclvalue(file.dataIndex)))
                .cdtData$EnvData$plot.maps[c('lon', 'lat', 'id')] <- .cdtData$EnvData$output$data[c('lon', 'lat', 'id')]
                set.plot.type(.cdtData$EnvData$output$params$data.type)
                ###################

                set.Data.VarStat.Dates_1st()
                widgets.Station.Pixel()
                ret1 <- try(read.Data.MapVarStat(), silent = TRUE)
                if(inherits(ret1, "try-error") | is.null(ret1)) return(NULL)
                ret2 <- try(read.Data.MapVarTS(), silent = TRUE)
                if(inherits(ret2, "try-error") | is.null(ret2)) return(NULL)
            }
        })

        ###############

        tkbind(chk.dataIdx, "<Button-1>", function(){
            stateExistData <- if(tclvalue(DirExist) == '1') 'disabled' else 'normal'
            tkconfigure(en.dataIdx, state = stateExistData)
            tkconfigure(bt.dataIdx, state = stateExistData)

            stateCaclBut <- if(tclvalue(DirExist) == '1') 'normal' else 'disabled'
            tcl(tknote.cmd, 'itemconfigure', cmd.tab1$IDtab, state = stateCaclBut)
            tcl(tknote.cmd, 'itemconfigure', cmd.tab2$IDtab, state = stateCaclBut)
        })

        ##############################################

        frameDataStatMap <- ttklabelframe(subfr3, text = lang.dlg[['label']][['17']], relief = 'groove')

        .cdtData$EnvData$anaVars <- tclVar()
        .cdtData$EnvData$anaStat <- tclVar()

        cb.varstat.var <- ttkcombobox(frameDataStatMap, values = "", textvariable = .cdtData$EnvData$anaVars, justify = 'center', width = largeur4)
        bt.varstat.maps <- ttkbutton(frameDataStatMap, text = .cdtEnv$tcl$lang$global[['button']][['3']], width = largeur8)
        bt.varstat.MapOpt <- ttkbutton(frameDataStatMap, text = .cdtEnv$tcl$lang$global[['button']][['4']], width = largeur8)
        cb.varstat.stat <- ttkcombobox(frameDataStatMap, values = "", textvariable = .cdtData$EnvData$anaStat, justify = 'center', width = largeur4)

        tkgrid(cb.varstat.var, row = 0, column = 1, sticky = 'we', rowspan = 1, columnspan = 8, padx = 1, pady = 1, ipadx = 1, ipady = 1)
        tkgrid(bt.varstat.MapOpt, row = 1, column = 0, sticky = 'we', rowspan = 1, columnspan = 5, ipadx = 1, ipady = 1)
        tkgrid(bt.varstat.maps, row = 1, column = 5, sticky = 'we', rowspan = 1, columnspan = 5, ipadx = 1, ipady = 1)
        tkgrid(cb.varstat.stat, row = 2, column = 1, sticky = 'we', rowspan = 1, columnspan = 8, padx = 1, pady = 1, ipadx = 1, ipady = 1)

        ###################

        tkconfigure(bt.varstat.MapOpt, command = function(){
            if(!is.null(.cdtData$EnvData$varData$map)){
                atlevel <- pretty(.cdtData$EnvData$varData$map$z, n = 10, min.n = 7)
                if(is.null(.cdtData$EnvData$varstatMapOp$userLvl$levels)){
                    .cdtData$EnvData$varstatMapOp$userLvl$levels <- atlevel
                }else{
                    if(!.cdtData$EnvData$varstatMapOp$userLvl$custom)
                        .cdtData$EnvData$varstatMapOp$userLvl$levels <- atlevel
                }
            }
            .cdtData$EnvData$varstatMapOp <- MapGraph.MapOptions(.cdtData$EnvData$varstatMapOp)
        })

        ###################

        .cdtData$EnvData$tab$dataMapStat <- NULL

        tkconfigure(bt.varstat.maps, command = function(){
            if(trimws(tclvalue(.cdtData$EnvData$anaVars)) != "" &
                trimws(tclvalue(.cdtData$EnvData$anaStat)) != "")
            {
                ret <- try(read.Data.MapVarStat(), silent = TRUE)
                if(inherits(ret, "try-error") | is.null(ret)) return(NULL)
                dailyRainAnalysis.Display.MapsVarStats()
            }
        })

        ###################

        tkbind(cb.varstat.var, "<<ComboboxSelected>>", function(){
            vars <- trimws(tclvalue(.cdtData$EnvData$anaVars))
            if(vars == "") return(NULL)
            varstats <- .cdtData$EnvData$output$exist.vars.dates
            statsval <- varstats[[varsname$name[which(varsname$longname == vars)]]]

            STATSVAL <- statsname$longname[statsname$name %in% names(statsval)]
            if(length(STATSVAL) == 1) STATSVAL <- c(STATSVAL, "")
            tkconfigure(cb.varstat.stat, values = STATSVAL)
            tclvalue(.cdtData$EnvData$anaStat) <- STATSVAL[1]

            tkconfigure(cb.data.Index, values = statsval$date)
            tclvalue(.cdtData$EnvData$donDate) <- statsval$date[length(statsval$date)]

            ##############
            vars <- trimws(tclvalue(.cdtData$EnvData$anaVars))
            this.vars <- varsname$name[which(varsname$longname == vars)]
            .cdtData$EnvData$now$this.vars <- this.vars

            stats <- trimws(tclvalue(.cdtData$EnvData$anaStat))
            this.stats <- statsname$name[which(statsname$longname == stats)]
            .cdtData$EnvData$now$this.stats <- this.stats

            return(0)
        })

        tkbind(cb.varstat.stat, "<<ComboboxSelected>>", function(){
            vars <- trimws(tclvalue(.cdtData$EnvData$anaVars))
            this.vars <- varsname$name[which(varsname$longname == vars)]
            .cdtData$EnvData$now$this.vars <- this.vars

            stats <- trimws(tclvalue(.cdtData$EnvData$anaStat))
            this.stats <- statsname$name[which(statsname$longname == stats)]
            .cdtData$EnvData$now$this.stats <- this.stats
        })

        ##############################################

        frameDataMap <- ttklabelframe(subfr3, text = lang.dlg[['label']][['18']], relief = 'groove')

        .cdtData$EnvData$donDate <- tclVar()

        cb.data.Index <- ttkcombobox(frameDataMap, values = "", textvariable = .cdtData$EnvData$donDate, justify = 'center', width = largeur9)
        bt.data.Index.prev <- ttkbutton(frameDataMap, text = "<<", width = largeur7)
        bt.data.Index.next <- ttkbutton(frameDataMap, text = ">>", width = largeur7)
        bt.data.maps <- ttkbutton(frameDataMap, text = .cdtEnv$tcl$lang$global[['button']][['3']], width = largeur8)
        bt.data.MapOpt <- ttkbutton(frameDataMap, text = .cdtEnv$tcl$lang$global[['button']][['4']], width = largeur8)

        tkgrid(bt.data.Index.prev, row = 0, column = 0, sticky = 'we', rowspan = 1, columnspan = 2, padx = 1, pady = 1, ipadx = 1, ipady = 1)
        tkgrid(cb.data.Index, row = 0, column = 2, sticky = 'we', rowspan = 1, columnspan = 6, padx = 1, pady = 1, ipadx = 1, ipady = 1)
        tkgrid(bt.data.Index.next, row = 0, column = 8, sticky = 'we', rowspan = 1, columnspan = 2, padx = 1, pady = 1, ipadx = 1, ipady = 1)
        tkgrid(bt.data.MapOpt, row = 1, column = 0, sticky = 'we', rowspan = 1, columnspan = 5, ipadx = 1, ipady = 1)
        tkgrid(bt.data.maps, row = 1, column = 5, sticky = 'we', rowspan = 1, columnspan = 5, ipadx = 1, ipady = 1)

        ###############

        tkconfigure(bt.data.MapOpt, command = function(){
            if(!is.null(.cdtData$EnvData$varData$map)){
                atlevel <- pretty(.cdtData$EnvData$varData$map$z, n = 10, min.n = 7)
                if(is.null(.cdtData$EnvData$dataMapOp$userLvl$levels)){
                    .cdtData$EnvData$dataMapOp$userLvl$levels <- atlevel
                }else{
                    if(!.cdtData$EnvData$dataMapOp$userLvl$custom)
                        .cdtData$EnvData$dataMapOp$userLvl$levels <- atlevel
                }
            }
            .cdtData$EnvData$dataMapOp <- MapGraph.MapOptions(.cdtData$EnvData$dataMapOp)
        })

        ###############

        .cdtData$EnvData$tab$dataMapTS <- NULL

        tkconfigure(bt.data.maps, command = function(){
            if(trimws(tclvalue(.cdtData$EnvData$donDate)) != ""){
                ret <- try(read.Data.MapVarTS(), silent = TRUE)
                if(inherits(ret, "try-error") | is.null(ret)) return(NULL)

                dailyRainAnalysis.Display.MapVarTS()
            }
        })

        tkconfigure(bt.data.Index.prev, command = function(){
            if(trimws(tclvalue(.cdtData$EnvData$donDate)) != ""){
                vars <- trimws(tclvalue(.cdtData$EnvData$anaVars))
                this.vars <- varsname$name[which(varsname$longname == vars)]
                donDates <- .cdtData$EnvData$output$exist.vars.dates[[this.vars]]$date
                idaty <- which(donDates == trimws(tclvalue(.cdtData$EnvData$donDate)))
                idaty <- idaty - 1
                if(idaty < 1) idaty <- length(donDates)
                tclvalue(.cdtData$EnvData$donDate) <- donDates[idaty]

                ret <- try(read.Data.MapVarTS(), silent = TRUE)
                if(inherits(ret, "try-error") | is.null(ret)) return(NULL)

                dailyRainAnalysis.Display.MapVarTS()
            }
        })

        tkconfigure(bt.data.Index.next, command = function(){
            if(trimws(tclvalue(.cdtData$EnvData$donDate)) != ""){
                vars <- trimws(tclvalue(.cdtData$EnvData$anaVars))
                this.vars <- varsname$name[which(varsname$longname == vars)]
                donDates <- .cdtData$EnvData$output$exist.vars.dates[[this.vars]]$date
                idaty <- which(donDates == trimws(tclvalue(.cdtData$EnvData$donDate)))
                idaty <- idaty + 1
                if(idaty > length(donDates)) idaty <- 1
                tclvalue(.cdtData$EnvData$donDate) <- donDates[idaty]

                ret <- try(read.Data.MapVarTS(), silent = TRUE)
                if(inherits(ret, "try-error") | is.null(ret)) return(NULL)

                dailyRainAnalysis.Display.MapVarTS()
            }
        })

        ###############

        tkbind(cb.data.Index, "<<ComboboxSelected>>", function(){
            if(!is.null(.cdtData$EnvData$tsData)){
                ret <- try(read.Data.MapVarTS(), silent = TRUE)
                if(inherits(ret, "try-error") | is.null(ret)) return(NULL)
            }
        })

        ##############################################

        tkgrid(frameDataExist, row = 0, column = 0, sticky = 'we', padx = 1, pady = 1, ipadx = 1, ipady = 1)
        tkgrid(frameDataStatMap, row = 1, column = 0, sticky = 'we', padx = 1, pady = 3, ipadx = 1, ipady = 1)
        tkgrid(frameDataMap, row = 2, column = 0, sticky = 'we', padx = 1, pady = 1, ipadx = 1, ipady = 1)

    #######################################################################################################

    #Tab4
    subfr4 <- bwTabScrollableFrame(cmd.tab4)

        ##############################################

        frameDataTS <- ttklabelframe(subfr4, text = lang.dlg[['label']][['20']], relief = 'groove')

        CbtypeTSPLOTVAL <- lang.dlg[['combobox']][['4']]
        typeTSPLOTVAL <- c('line', 'bar', 'proba', 'anom')
        typeTSp <- tclVar()
        tclvalue(typeTSp) <- CbtypeTSPLOTVAL[typeTSPLOTVAL %in% GeneralParameters$plot$typeTSp]

        .cdtData$EnvData$plot.maps$typeTSp <- GeneralParameters$plot$typeTSp

        .cdtData$EnvData$plot.maps$averageTSp <- tclVar(FALSE)
        .cdtData$EnvData$plot.maps$tercileTSp <- tclVar(FALSE)
        .cdtData$EnvData$plot.maps$trendTSp <- tclVar(FALSE)

        stateType <- if(GeneralParameters$plot$typeTSp == 'line') "normal" else "disabled"

        cb.typeTSp <- ttkcombobox(frameDataTS, values = CbtypeTSPLOTVAL, textvariable = typeTSp, justify = 'center', width = largeur10)
        bt.TsGraph.plot <- ttkbutton(frameDataTS, text = .cdtEnv$tcl$lang$global[['button']][['3']], width = largeur8)
        bt.TSGraphOpt <- ttkbutton(frameDataTS, text = .cdtEnv$tcl$lang$global[['button']][['4']], width = largeur8)

        frTS1 <- tkframe(frameDataTS)
        chk.meanTSp <- tkcheckbutton(frTS1, variable = .cdtData$EnvData$plot.maps$averageTSp, text = lang.dlg[['checkbutton']][['2']], anchor = 'w', justify = 'left', state = stateType)
        chk.tercTSp <- tkcheckbutton(frTS1, variable = .cdtData$EnvData$plot.maps$tercileTSp, text = lang.dlg[['checkbutton']][['3']], anchor = 'w', justify = 'left', state = stateType)
        chk.trendTSp <- tkcheckbutton(frTS1, variable = .cdtData$EnvData$plot.maps$trendTSp, text = lang.dlg[['checkbutton']][['4']], anchor = 'w', justify = 'left', state = stateType)
        tkgrid(chk.meanTSp, chk.tercTSp, chk.trendTSp)

        #################

        tkgrid(cb.typeTSp, row = 0, column = 1, sticky = 'we', rowspan = 1, columnspan = 8, padx = 1, pady = 1, ipadx = 1, ipady = 1)
        tkgrid(bt.TSGraphOpt, row = 1, column = 0, sticky = 'we', rowspan = 1, columnspan = 5, padx = 1, pady = 1, ipadx = 1, ipady = 1)
        tkgrid(bt.TsGraph.plot, row = 1, column = 5, sticky = 'we', rowspan = 1, columnspan = 5, padx = 1, pady = 1, ipadx = 1, ipady = 1)
        tkgrid(frTS1, row = 2, column = 0, sticky = '', rowspan = 1, columnspan = 10, padx = 1, pady = 1, ipadx = 1, ipady = 1)

        #################

        tkconfigure(bt.TSGraphOpt, command = function(){
            vtypeTSp <- typeTSPLOTVAL[CbtypeTSPLOTVAL %in% trimws(tclvalue(typeTSp))]
            suffix.fun <- switch(vtypeTSp,
                                 "anom" = "Anomaly",
                                 "bar" = "Bar",
                                 "line" = "Line",
                                 "proba" = "Proba")
            plot.fun <- get(paste0("MapGraph.GraphOptions.", suffix.fun), mode = "function")
            .cdtData$EnvData$TSGraphOp <- plot.fun(.cdtData$EnvData$TSGraphOp)
        })

        #################

        .cdtData$EnvData$tab$dataGraph <- NULL

        tkconfigure(bt.TsGraph.plot, command = function(){
            .cdtData$EnvData$plot.maps$typeTSp <- typeTSPLOTVAL[CbtypeTSPLOTVAL %in% trimws(tclvalue(typeTSp))]

            if(!is.null(.cdtData$EnvData$tsData)){
                imgContainer <- CDT.Display.Graph(dailyRainAnalysis.plotVarGraph, .cdtData$EnvData$tab$dataGraph, 'Analysis-Graph')
                .cdtData$EnvData$tab$dataGraph <- imageNotebookTab_unik(imgContainer, .cdtData$EnvData$tab$dataGraph)
            }
        })

        #################

        tkbind(cb.typeTSp, "<<ComboboxSelected>>", function(){
            .cdtData$EnvData$plot.maps$typeTSp <- typeTSPLOTVAL[CbtypeTSPLOTVAL %in% trimws(tclvalue(typeTSp))]

            stateType <- if(.cdtData$EnvData$plot.maps$typeTSp == "line") "normal" else "disabled"
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

        tkgrid(frameDataTS, row = 0, column = 0, sticky = 'we', pady = 1)
        tkgrid(frameSTNCrds, row = 1, column = 0, sticky = '', pady = 3)

    #######################################################################################################

    #Tab5
    subfr5 <- bwTabScrollableFrame(cmd.tab5)

        ##############################################

        frameSHP <- create_shpLayer_frame(subfr5)
        tkgrid(frameSHP, row = 0, column = 0, sticky = 'we', pady = 1)

    #######################################################################################################

    widgets.Station.Pixel <- function(){
        tkdestroy(frTS2)
        frTS2 <<- tkframe(frameSTNCrds)

        if(.cdtData$EnvData$output$params$data.type == "cdtstation"){
            stnIDTSPLOT <- .cdtData$EnvData$output$data$id
            txt.stnSel <- tklabel(frTS2, text = lang.dlg[['label']][['23']])
            bt.stnID.prev <- ttkbutton(frTS2, text = "<<", width = largeur7)
            bt.stnID.next <- ttkbutton(frTS2, text = ">>", width = largeur7)
            cb.stnID <- ttkcombobox(frTS2, values = stnIDTSPLOT, textvariable = .cdtData$EnvData$plot.maps$stnIDTSp, justify = 'center', width = largeur9)
            tclvalue(.cdtData$EnvData$plot.maps$stnIDTSp) <- stnIDTSPLOT[1]

            tkgrid(txt.stnSel, row = 0, column = 0, sticky = '', rowspan = 1, columnspan = 3, padx = 1, pady = 1, ipadx = 1, ipady = 1)
            tkgrid(bt.stnID.prev, row = 1, column = 0, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
            tkgrid(cb.stnID, row = 1, column = 1, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
            tkgrid(bt.stnID.next, row = 1, column = 2, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)

            tkconfigure(bt.stnID.prev, command = function(){
                .cdtData$EnvData$plot.maps$typeTSp <- typeTSPLOTVAL[CbtypeTSPLOTVAL %in% trimws(tclvalue(typeTSp))]

                if(!is.null(.cdtData$EnvData$tsData)){
                    istn <- which(stnIDTSPLOT == trimws(tclvalue(.cdtData$EnvData$plot.maps$stnIDTSp)))
                    istn <- istn - 1
                    if(istn < 1) istn <- length(stnIDTSPLOT)
                    tclvalue(.cdtData$EnvData$plot.maps$stnIDTSp) <- stnIDTSPLOT[istn]

                    imgContainer <- CDT.Display.Graph(dailyRainAnalysis.plotVarGraph, .cdtData$EnvData$tab$dataGraph, 'Analysis-Graph')
                    .cdtData$EnvData$tab$dataGraph <- imageNotebookTab_unik(imgContainer, .cdtData$EnvData$tab$dataGraph)
                }
            })

            tkconfigure(bt.stnID.next, command = function(){
                .cdtData$EnvData$plot.maps$typeTSp <- typeTSPLOTVAL[CbtypeTSPLOTVAL %in% trimws(tclvalue(typeTSp))]

                if(!is.null(.cdtData$EnvData$tsData)){
                    istn <- which(stnIDTSPLOT == trimws(tclvalue(.cdtData$EnvData$plot.maps$stnIDTSp)))
                    istn <- istn + 1
                    if(istn > length(stnIDTSPLOT)) istn <- 1
                    tclvalue(.cdtData$EnvData$plot.maps$stnIDTSp) <- stnIDTSPLOT[istn]

                    imgContainer <- CDT.Display.Graph(dailyRainAnalysis.plotVarGraph, .cdtData$EnvData$tab$dataGraph, 'Analysis-Graph')
                    .cdtData$EnvData$tab$dataGraph <- imageNotebookTab_unik(imgContainer, .cdtData$EnvData$tab$dataGraph)
                }
            })
        }else{
            txt.crdSel <- tklabel(frTS2, text = lang.dlg[['label']][['24']], anchor = 'w', justify = 'left')
            txt.lonLoc <- tklabel(frTS2, text = lang.dlg[['label']][['25']], anchor = 'e', justify = 'right')
            en.lonLoc <- tkentry(frTS2, textvariable = .cdtData$EnvData$plot.maps$lonLOC, width = largeur11)
            txt.latLoc <- tklabel(frTS2, text = lang.dlg[['label']][['26']], anchor = 'e', justify = 'right')
            en.latLoc <- tkentry(frTS2, textvariable = .cdtData$EnvData$plot.maps$latLOC, width = largeur11)
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

    ###################

    set.plot.type <- function(data_type){
        if(data_type == 'cdtstation'){
            .data.type <- "Points"
            plot_type <- list(values = c("Pixels", "Points"), var = "Pixels")
        }

        if(data_type == 'cdtdataset'){
            .data.type <- "Grid"
            plot_type <- list(values = c("Pixels", "FilledContour"), var = "Pixels")
        }

        .cdtData$EnvData$varstatMapOp$plotType <- plot_type
        .cdtData$EnvData$dataMapOp$plotType <- plot_type
        .cdtData$EnvData$plot.maps$.data.type <- .data.type
        .cdtData$EnvData$plot.maps$data.type <- data_type
    }

    ###################

    set.Data.VarStat.Dates_1st <- function(){
        varstats <- .cdtData$EnvData$output$exist.vars.dates
        if(length(names(varstats)) == 0) return(NULL)

        VARSVAL <- varsname$longname[varsname$name %in% names(varstats)]
        if(length(VARSVAL) == 1) VARSVAL <- c(VARSVAL, "")
        tkconfigure(cb.varstat.var, values = VARSVAL)
        last.vars <- varsname$longname[which(varsname$name == .cdtData$EnvData$output$last[1])]
        tclvalue(.cdtData$EnvData$anaVars) <- last.vars

        statsval <- varstats[[.cdtData$EnvData$output$last[1]]]
        STATSVAL <- statsname$longname[statsname$name %in% names(statsval)]
        if(length(STATSVAL) == 1) STATSVAL <- c(STATSVAL, "")
        tkconfigure(cb.varstat.stat, values = STATSVAL)
        last.stats <- statsname$longname[which(statsname$name == .cdtData$EnvData$output$last[2])]
        tclvalue(.cdtData$EnvData$anaStat) <- last.stats

        tkconfigure(cb.data.Index, values = statsval$date)
        tclvalue(.cdtData$EnvData$donDate) <- statsval$date[length(statsval$date)]

        return(0)
    }

    #######################################################################################################

    read.Data.MapVarStat <- function(){
        tkconfigure(.cdtEnv$tcl$main$win, cursor = 'watch')
        tcl('update')
        on.exit({
            tkconfigure(.cdtEnv$tcl$main$win, cursor = '')
            tcl('update')
        })

        vars <- trimws(tclvalue(.cdtData$EnvData$anaVars))
        stats <- trimws(tclvalue(.cdtData$EnvData$anaStat))
        this.vars <- varsname$name[which(varsname$longname == vars)]
        this.stats <- statsname$name[which(statsname$longname == stats)]
        
        if(vars == "" | stats == "") return(NULL)

        if(.cdtData$EnvData$output$params$data.type == "cdtstation"){
            filePathData <- file.path(.cdtData$EnvData$PathData, "CDTDATASET", paste0(this.vars, "_", this.stats, ".rds"))
            if(!file.exists(filePathData)){
                Insert.Messages.Out(paste(filePathData, lang.dlg[['message']][['5']]), TRUE, 'e')
                return(NULL)
            }

            change.plot <- .cdtData$EnvData$varstatMapOp$plotType$var

            readVarData <- TRUE
            if(!is.null(.cdtData$EnvData$statData))
                if(!is.null(.cdtData$EnvData$statData$filePathData))
                    if(.cdtData$EnvData$statData$filePathData == filePathData) readVarData <- FALSE

            if(!readVarData)
                if(.cdtData$EnvData$change.plot.VarData != change.plot) readVarData <- TRUE

            if(readVarData){
                .cdtData$EnvData$statData$data <- readRDS(filePathData)

                X0 <- .cdtData$EnvData$output$data$lon
                Y0 <- .cdtData$EnvData$output$data$lat
                VAR0 <- .cdtData$EnvData$statData$data
                if(change.plot == "Pixels"){
                    nx <- nx_ny_as.image(diff(range(X0)))
                    ny <- nx_ny_as.image(diff(range(Y0)))
                    tmp <- cdt.as.image(VAR0, nx = nx, ny = ny, pts.xy = cbind(X0, Y0))
                    .cdtData$EnvData$statData$map$x <- tmp$x
                    .cdtData$EnvData$statData$map$y <- tmp$y
                    .cdtData$EnvData$statData$map$z <- tmp$z
                    rm(tmp)
                }

                if(change.plot == "Points"){
                    .cdtData$EnvData$statData$map$x <- X0
                    .cdtData$EnvData$statData$map$y <- Y0
                    .cdtData$EnvData$statData$map$z <- VAR0
                }

                .cdtData$EnvData$statData$filePathData <- filePathData
                .cdtData$EnvData$change.plot.VarData <- change.plot
            }
        }else{
            filePathData <- file.path(.cdtData$EnvData$PathData, "DATA_NetCDF_STATS", paste0(this.vars, "_", this.stats, ".nc"))
            if(!file.exists(filePathData)){
                Insert.Messages.Out(paste(filePathData, lang.dlg[['message']][['5']]), TRUE, 'e')
                return(NULL)
            }

            readVarData <- TRUE
            if(!is.null(.cdtData$EnvData$statData))
                if(!is.null(.cdtData$EnvData$statData$filePathData))
                    if(.cdtData$EnvData$statData$filePathData == filePathData) readVarData <- FALSE

            if(readVarData){
                nc <- ncdf4::nc_open(filePathData)
                .cdtData$EnvData$statData$map$x <- nc$dim[[1]]$vals
                .cdtData$EnvData$statData$map$y <- nc$dim[[2]]$vals
                .cdtData$EnvData$statData$map$z <- ncdf4::ncvar_get(nc, varid = nc$var[[1]]$name)
                ncdf4::nc_close(nc)
                .cdtData$EnvData$statData$filePathData <- filePathData
            }
        }

        .cdtData$EnvData$now$this.vars <- this.vars
        .cdtData$EnvData$now$this.stats <- this.stats

        return(0)
    }

    ###################

    read.Data.MapVarTS <- function(){
        tkconfigure(.cdtEnv$tcl$main$win, cursor = 'watch')
        tcl('update')
        on.exit({
            tkconfigure(.cdtEnv$tcl$main$win, cursor = '')
            tcl('update')
        })

        vars <- trimws(tclvalue(.cdtData$EnvData$anaVars))
        this.vars <- varsname$name[which(varsname$longname == vars)]
        this.daty <- trimws(tclvalue(.cdtData$EnvData$donDate))

        if(vars == "" | this.daty == "") return(NULL)

        if(.cdtData$EnvData$output$params$data.type == "cdtstation"){
            filePathData <- file.path(.cdtData$EnvData$PathData, this.vars, paste0(this.vars, ".rds"))
            if(!file.exists(filePathData)){
                Insert.Messages.Out(paste(filePathData, lang.dlg[['message']][['5']]), TRUE, 'e')
                return(NULL)
            }

            change.plot <- .cdtData$EnvData$dataMapOp$plotType$var

            ########
            readVarData <- TRUE
            if(!is.null(.cdtData$EnvData$tsData))
                if(!is.null(.cdtData$EnvData$tsData$filePathData))
                    if(.cdtData$EnvData$tsData$filePathData == filePathData) readVarData <- FALSE

            if(readVarData){
                .cdtData$EnvData$tsData$data <- readRDS(filePathData)
                .cdtData$EnvData$tsData$filePathData <- filePathData
            }

            ########
            rasterVarData <- TRUE
            if(!rasterVarData)
                if(!is.null(.cdtData$EnvData$tsData$rasterDate))
                    if(.cdtData$EnvData$tsData$filePathData == filePathData)
                        if(.cdtData$EnvData$tsData$rasterDate == this.daty) rasterVarData <- FALSE

            if(!rasterVarData)
                if(.cdtData$EnvData$change.plot.rasterVarData != change.plot) rasterVarData <- TRUE

            if(rasterVarData){
                idt <- which(.cdtData$EnvData$output$exist.vars.dates[[this.vars]]$date == this.daty)

                X0 <- .cdtData$EnvData$output$data$lon
                Y0 <- .cdtData$EnvData$output$data$lat
                VAR0 <- as.numeric(.cdtData$EnvData$tsData$data[idt, ])
                if(change.plot == "Pixels"){
                    nx <- nx_ny_as.image(diff(range(X0)))
                    ny <- nx_ny_as.image(diff(range(Y0)))
                    tmp <- cdt.as.image(VAR0, nx = nx, ny = ny, pts.xy = cbind(X0, Y0))
                    .cdtData$EnvData$tsData$map$x <- tmp$x
                    .cdtData$EnvData$tsData$map$y <- tmp$y
                    .cdtData$EnvData$tsData$map$z <- tmp$z
                    rm(tmp)
                }

                if(change.plot == "Points"){
                    .cdtData$EnvData$tsData$map$x <- X0
                    .cdtData$EnvData$tsData$map$y <- Y0
                    .cdtData$EnvData$tsData$map$z <- VAR0
                }

                .cdtData$EnvData$tsData$rasterDate <- this.daty
                .cdtData$EnvData$change.plot.rasterVarData <- change.plot
            }
        }else{
            filePathData <- file.path(.cdtData$EnvData$PathData, this.vars, "DATA_NetCDF", paste0("Seas_", this.daty, ".nc"))
            if(!file.exists(filePathData)){
                Insert.Messages.Out(paste(filePathData, lang.dlg[['message']][['5']]), TRUE, 'e')
                return(NULL)
            }

            readVarData <- TRUE
            if(!is.null(.cdtData$EnvData$tsData))
                if(!is.null(.cdtData$EnvData$tsData$filePathData))
                    if(.cdtData$EnvData$tsData$filePathData == filePathData) readVarData <- FALSE

            if(readVarData){
                nc <- ncdf4::nc_open(filePathData)
                .cdtData$EnvData$tsData$map$x <- nc$dim[[1]]$vals
                .cdtData$EnvData$tsData$map$y <- nc$dim[[2]]$vals
                .cdtData$EnvData$tsData$map$z <- ncdf4::ncvar_get(nc, varid = nc$var[[1]]$name)
                ncdf4::nc_close(nc)
                .cdtData$EnvData$tsData$filePathData <- filePathData
            }

            ###################

            file.CDT.Idx <- file.path(.cdtData$EnvData$PathData, this.vars, paste0(this.vars, ".rds"))

            read.cdt.dataIdx<- TRUE
            if(!is.null(.cdtData$EnvData$cdtdataset))
                if(!is.null(.cdtData$EnvData$file.CDT.Idx))
                    if(.cdtData$EnvData$file.CDT.Idx == file.CDT.Idx) read.cdt.dataIdx <- FALSE

            if(read.cdt.dataIdx){
                .cdtData$EnvData$cdtdataset <- readRDS(file.CDT.Idx)
                .cdtData$EnvData$cdtdataset$fileInfo <- file.CDT.Idx
                .cdtData$EnvData$file.CDT.Idx <- file.CDT.Idx
            }
        }

        stats <- trimws(tclvalue(.cdtData$EnvData$anaStat))
        this.stats <- statsname$name[which(statsname$longname == stats)]

        .cdtData$EnvData$now$this.vars <- this.vars
        .cdtData$EnvData$now$this.stats <- this.stats

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
