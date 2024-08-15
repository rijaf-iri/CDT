
SeasonAnalysisPanelCmd <- function(){
    if(WindowsOS()){
        largeur0 <- 36
        largeur1 <- 27
        largeur2 <- 33
        largeur3 <- 14
        largeur4 <- 36
        largeur5 <- 19
        largeur6 <- 20
        largeur7 <- 7
        largeur8 <- 27
        largeur9 <- 22
        largeur11 <- 30
        largeur12 <- 10
    }else{
        largeur0 <- 33
        largeur1 <- 28
        largeur2 <- 32
        largeur3 <- 14
        largeur4 <- 36
        largeur5 <- 18
        largeur6 <- 19
        largeur7 <- 7
        largeur8 <- 27
        largeur9 <- 22
        largeur11 <- 30
        largeur12 <- 10
    }

    GeneralParameters <- list(onset = "", cessation = "", output = "",
                              seastot = list(useTotal = FALSE, Tstep = "dekadal",
                                             data.type = "cdtstation",
                                             cdtstation = list(prec = ""),
                                             cdtdataset = list(prec = "")),
                              dryday = 0.85, min.frac = 0.95)

    GeneralParameters$plotVar <- list(varPICSA = 'totrainSeas', dryspell = 5, yearseas = '')
    GeneralParameters$analysis <- list(method = 'mean', trend = 'trendEY', mth.perc = 95,
                                       low.thres = "0", up.thres = "200")

    GeneralParameters$graph <- list(varTSp = 'onset', typeTSp = 'line')

    .cdtData$EnvData$TSMapOp <- list(presetCol = list(color = 'tim.colors', reverse = FALSE),
                                     userCol = list(custom = FALSE, color = NULL),
                                     userLvl = list(custom = FALSE, levels = NULL, equidist = FALSE),
                                     title = list(user = FALSE, title = ''),
                                     colkeyLab = list(user = FALSE, label = ''),
                                     scalebar = list(add = FALSE, pos = 'bottomleft'),
                                     plotType = list(values = c("Pixels", "Points"), var = "Pixels"),
                                     pointSize = 1.0, bbox = .cdtData$Config$region)

    .cdtData$EnvData$climMapOp <- list(presetCol = list(color = 'tim.colors', reverse = FALSE),
                                       userCol = list(custom = FALSE, color = NULL),
                                       userLvl = list(custom = FALSE, levels = NULL, equidist = FALSE),
                                       title = list(user = FALSE, title = ''),
                                       colkeyLab = list(user = FALSE, label = ''),
                                       scalebar = list(add = FALSE, pos = 'bottomleft'),
                                       plotType = list(values = c("Pixels", "Points"), var = "Pixels"),
                                       pointSize = 1.0, bbox = .cdtData$Config$region)

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

    xml.dlg <- file.path(.cdtDir$dirLocal, "languages", "cdtSeasonAnalysis_leftCmd.xml")
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

        input.Onset <- tclVar(GeneralParameters$onset)
        input.Cessation <- tclVar(GeneralParameters$cessation)

        txt.Ons <- tklabel(frameInData, text = lang.dlg[['label']][['2']], anchor = 'w', justify = 'left')
        en.Ons <- tkentry(frameInData, textvariable = input.Onset, width = largeur0)
        bt.Ons <- tkbutton(frameInData, text = "...")

        txt.Ces <- tklabel(frameInData, text = lang.dlg[['label']][['3']], anchor = 'w', justify = 'left')
        en.Ces <- tkentry(frameInData, textvariable = input.Cessation, width = largeur0)
        bt.Ces <- tkbutton(frameInData, text = "...")

        tkconfigure(bt.Ons, command = function(){
            path.rds <- tclvalue(tkgetOpenFile(initialdir = getwd(), filetypes = .cdtEnv$tcl$data$filetypes6))
            tclvalue(input.Onset) <- if(path.rds %in% c("", "NA") | is.na(path.rds)) "" else path.rds
        })

        tkconfigure(bt.Ces, command = function(){
            path.rds <- tclvalue(tkgetOpenFile(initialdir = getwd(), filetypes = .cdtEnv$tcl$data$filetypes6))
            tclvalue(input.Cessation) <- if(path.rds %in% c("", "NA") | is.na(path.rds)) "" else path.rds
        })

        tkgrid(txt.Ons, row = 0, column = 0, sticky = 'we', rowspan = 1, columnspan = 10, padx = 1, pady = 1, ipadx = 1, ipady = 1)
        tkgrid(en.Ons, row = 1, column = 0, sticky = 'we', rowspan = 1, columnspan = 9, padx = 0, pady = 1, ipadx = 1, ipady = 1)
        tkgrid(bt.Ons, row = 1, column = 9, sticky = 'w', rowspan = 1, columnspan = 1, padx = 0, pady = 1, ipadx = 1, ipady = 1)
        tkgrid(txt.Ces, row = 2, column = 0, sticky = 'we', rowspan = 1, columnspan = 10, padx = 1, pady = 1, ipadx = 1, ipady = 1)
        tkgrid(en.Ces, row = 3, column = 0, sticky = 'we', rowspan = 1, columnspan = 9, padx = 0, pady = 1, ipadx = 1, ipady = 1)
        tkgrid(bt.Ces, row = 3, column = 9, sticky = 'w', rowspan = 1, columnspan = 1, padx = 0, pady = 1, ipadx = 1, ipady = 1)

        ############

        helpWidget(en.Ons, lang.dlg[['tooltip']][['1']], lang.dlg[['status']][['1']])
        helpWidget(en.Ces, lang.dlg[['tooltip']][['2']], lang.dlg[['status']][['2']])
        helpWidget(bt.Ons, lang.dlg[['tooltip']][['3']], lang.dlg[['status']][['3']])
        helpWidget(bt.Ces, lang.dlg[['tooltip']][['3']], lang.dlg[['status']][['3']])

        ############################################

        frameSeasTot <- ttklabelframe(subfr1, text = lang.dlg[['label']][['4']], relief = 'groove')

        useTotal <- tclVar(GeneralParameters$seastot$useTotal)

        timeSteps <- tclVar()
        CbperiodVAL <- .cdtEnv$tcl$lang$global[['combobox']][['1']][3:5]
        periodVAL <- c('pentad', 'dekadal', 'monthly')
        tclvalue(timeSteps) <- CbperiodVAL[periodVAL %in% GeneralParameters$seastot$Tstep]

        DataType <- tclVar()
        CbdatatypeVAL <- .cdtEnv$tcl$lang$global[['combobox']][['2']][1:2]
        datatypeVAL <- c('cdtstation', 'cdtdataset')
        tclvalue(DataType) <- CbdatatypeVAL[datatypeVAL %in% GeneralParameters$seastot$data.type]

        if(GeneralParameters$seastot$data.type == 'cdtstation'){
            input.Prec <- tclVar(GeneralParameters$seastot$cdtstation$prec)
            txt.INPrec <- lang.dlg[['label']][['5']]
        }else{
            input.Prec <- tclVar(GeneralParameters$seastot$cdtdataset$prec)
            txt.INPrec <- lang.dlg[['label']][['6']]
        }
        txt.INPrec.var <- tclVar(txt.INPrec)

        stateSEAS <- if(GeneralParameters$seastot$useTotal) 'normal' else 'disabled'

        chk.seastot <- tkcheckbutton(frameSeasTot, variable = useTotal, text = lang.dlg[['checkbutton']][['1']], anchor = 'w', justify = 'left')
        txt.period <- tklabel(frameSeasTot, text = lang.dlg[['label']][['7']], anchor = 'e', justify = 'right')
        cb.period <- ttkcombobox(frameSeasTot, values = CbperiodVAL, textvariable = timeSteps, width = largeur1, state = stateSEAS)
        txt.datatype <- tklabel(frameSeasTot, text = lang.dlg[['label']][['8']], anchor = 'e', justify = 'right')
        cb.datatype <- ttkcombobox(frameSeasTot, values = CbdatatypeVAL, textvariable = DataType, width = largeur1, state = stateSEAS)

        txt.INPrec <- tklabel(frameSeasTot, text = tclvalue(txt.INPrec.var), textvariable = txt.INPrec.var, anchor = 'w', justify = 'left')
        if(GeneralParameters$seastot$data.type == 'cdtstation'){
            cb.en.INPrec <- ttkcombobox(frameSeasTot, values = unlist(openFile_ttkcomboList()), textvariable = input.Prec, width = largeur2, state = stateSEAS)
            addTo_all_Combobox_List(cb.en.INPrec)
        }else{
            cb.en.INPrec <- tkentry(frameSeasTot, textvariable = input.Prec, width = largeur0, state = stateSEAS)
        }
        bt.INPrec <- tkbutton(frameSeasTot, text = "...", state = stateSEAS)

        ############

        tkgrid(chk.seastot, row = 0, column = 0, sticky = 'we', rowspan = 1, columnspan = 10, padx = 1, pady = 1, ipadx = 1, ipady = 1)

        tkgrid(txt.period, row = 1, column = 0, sticky = 'we', rowspan = 1, columnspan = 2, padx = 1, pady = 1, ipadx = 1, ipady = 1)
        tkgrid(cb.period, row = 1, column = 2, sticky = 'we', rowspan = 1, columnspan = 8, padx = 1, pady = 1, ipadx = 1, ipady = 1)

        tkgrid(txt.datatype, row = 2, column = 0, sticky = 'we', rowspan = 1, columnspan = 2, padx = 1, pady = 1, ipadx = 1, ipady = 1)
        tkgrid(cb.datatype, row = 2, column = 2, sticky = 'we', rowspan = 1, columnspan = 8, padx = 1, pady = 1, ipadx = 1, ipady = 1)

        tkgrid(txt.INPrec, row = 3, column = 0, sticky = 'we', rowspan = 1, columnspan = 10, padx = 1, pady = 1, ipadx = 1, ipady = 1)
        tkgrid(cb.en.INPrec, row = 4, column = 0, sticky = 'we', rowspan = 1, columnspan = 9, padx = 0, pady = 1, ipadx = 1, ipady = 1)
        tkgrid(bt.INPrec, row = 4, column = 9, sticky = 'w', rowspan = 1, columnspan = 1, padx = 0, pady = 1, ipadx = 1, ipady = 1)

        ############

        helpWidget(cb.period, lang.dlg[['tooltip']][['4']], lang.dlg[['status']][['4']])
        helpWidget(cb.datatype, lang.dlg[['tooltip']][['5']], lang.dlg[['status']][['5']])

        if(GeneralParameters$seastot$data.type == 'cdtstation'){
            helpWidget(cb.en.INPrec, lang.dlg[['tooltip']][['6']], lang.dlg[['status']][['6']])
            helpWidget(bt.INPrec, lang.dlg[['tooltip']][['8']], lang.dlg[['status']][['8']])
        }else{
            helpWidget(cb.en.INPrec, lang.dlg[['tooltip']][['7']], lang.dlg[['status']][['7']])
            helpWidget(bt.INPrec, lang.dlg[['tooltip']][['3']], lang.dlg[['status']][['3']])
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

        tkbind(chk.seastot, "<Button-1>", function(){
            stateSEAS <- if(tclvalue(useTotal) == '1') 'disabled' else 'normal'
            tkconfigure(cb.period, state = stateSEAS)
            tkconfigure(cb.datatype, state = stateSEAS)
            tkconfigure(cb.en.INPrec, state = stateSEAS)
            tkconfigure(bt.INPrec, state = stateSEAS)
        })

        ############

        tkbind(cb.datatype, "<<ComboboxSelected>>", function(){
            tkdestroy(cb.en.INPrec)
            tclvalue(input.Prec) <- ''

            data_type <- datatypeVAL[CbdatatypeVAL %in% trimws(tclvalue(DataType))]
            set.plot.type(data_type)

            stateSEAS <- if(tclvalue(useTotal) == '1') 'normal' else 'disabled'

            ###
            if(data_type == 'cdtstation'){
                tclvalue(txt.INPrec.var) <- lang.dlg[['label']][['5']]

                cb.en.INPrec <<- ttkcombobox(frameSeasTot, values = unlist(openFile_ttkcomboList()), textvariable = input.Prec, width = largeur2, state = stateSEAS)
                addTo_all_Combobox_List(cb.en.INPrec)

                ######
                helpWidget(cb.en.INPrec, lang.dlg[['tooltip']][['6']], lang.dlg[['status']][['6']])
                helpWidget(bt.INPrec, lang.dlg[['tooltip']][['8']], lang.dlg[['status']][['8']])

                ######
                tkconfigure(bt.INPrec, command = function(){
                    dat.opfiles <- getOpenFiles(.cdtEnv$tcl$main$win)
                    if(!is.null(dat.opfiles)){
                        update.OpenFiles('ascii', dat.opfiles)
                        tclvalue(input.Prec) <- dat.opfiles[[1]]
                    }
                })
            }

            ###
            if(data_type == 'cdtdataset'){
                tclvalue(txt.INPrec.var) <- lang.dlg[['label']][['6']]

                cb.en.INPrec <<- tkentry(frameSeasTot, textvariable = input.Prec, width = largeur0, state = stateSEAS)

                ######
                helpWidget(cb.en.INPrec, lang.dlg[['tooltip']][['7']], lang.dlg[['status']][['7']])
                helpWidget(bt.INPrec, lang.dlg[['tooltip']][['3']], lang.dlg[['status']][['3']])

                ######
                tkconfigure(bt.INPrec, command = function(){
                    path.rds <- tclvalue(tkgetOpenFile(initialdir = getwd(), filetypes = .cdtEnv$tcl$data$filetypes6))
                    tclvalue(input.Prec) <- if(path.rds %in% c("", "NA")) "" else path.rds
                })
            }

            #######
            tkgrid(cb.en.INPrec, row = 4, column = 0, sticky = 'we', rowspan = 1, columnspan = 9, padx = 0, pady = 1, ipadx = 1, ipady = 1)

            #######
            tkbind(chk.seastot, "<Button-1>", function(){
                stateSEAS <- if(tclvalue(useTotal) == '1') 'disabled' else 'normal'
                tkconfigure(cb.period, state = stateSEAS)
                tkconfigure(cb.datatype, state = stateSEAS)
                tkconfigure(cb.en.INPrec, state = stateSEAS)
                tkconfigure(bt.INPrec, state = stateSEAS)
            })
        })

        ############################################

        tkgrid(frameInData, row = 0, column = 0, sticky = 'we', padx = 1, pady = 3, ipadx = 1, ipady = 1)
        tkgrid(frameSeasTot, row = 1, column = 0, sticky = 'we', padx = 1, pady = 3, ipadx = 1, ipady = 1)

    #######################################################################################################

    #Tab2
    subfr2 <- bwTabScrollableFrame(cmd.tab2)

        ##############################################

        frameDryDay <- ttklabelframe(subfr2, text = lang.dlg[['label']][['9']], relief = 'groove')

        drywet.day <- tclVar(GeneralParameters$dryday)

        txt.DryDay1 <- tklabel(frameDryDay, text = lang.dlg[['label']][['10']], anchor = 'w', justify = 'left')
        en.DryDay <- tkentry(frameDryDay, textvariable = drywet.day, width = 5)
        txt.DryDay2 <- tklabel(frameDryDay, text = lang.dlg[['label']][['11']], anchor = 'w', justify = 'left')

        tkgrid(txt.DryDay1, en.DryDay, txt.DryDay2)

        ##############################################

        frameMinFrac <- tkframe(subfr2, relief = 'groove', borderwidth = 2)

        min.frac <- tclVar(GeneralParameters$min.frac)

        txt.MinFrac <- tklabel(frameMinFrac, text = lang.dlg[['label']][['12']], anchor = 'w', justify = 'left')
        en.MinFrac <- tkentry(frameMinFrac, textvariable = min.frac, width = 5)
        tkgrid(txt.MinFrac, en.MinFrac)

        ##############################################

        frameDirSav <- tkframe(subfr2, relief = 'groove', borderwidth = 2)

        dir.save <- tclVar(GeneralParameters$output)

        txt.dir.save <- tklabel(frameDirSav, text = lang.dlg[['label']][['13']], anchor = 'w', justify = 'left')
        en.dir.save <- tkentry(frameDirSav, textvariable = dir.save, width = largeur0)
        bt.dir.save <- tkbutton(frameDirSav, text = "...")

        ######
        tkgrid(txt.dir.save, row = 0, column = 0, sticky = 'we', rowspan = 1, columnspan = 5, padx = 1, pady = 0, ipadx = 1, ipady = 1)
        tkgrid(en.dir.save, row = 1, column = 0, sticky = 'we', rowspan = 1, columnspan = 4, padx = 0, pady = 0, ipadx = 1, ipady = 1)
        tkgrid(bt.dir.save, row = 1, column = 4, sticky = 'w', rowspan = 1, columnspan = 1, padx = 0, pady = 0, ipadx = 1, ipady = 1)

        helpWidget(en.dir.save, lang.dlg[['tooltip']][['9']], lang.dlg[['status']][['9']])
        helpWidget(bt.dir.save, lang.dlg[['tooltip']][['3']], lang.dlg[['status']][['3']])

        ######
        tkconfigure(bt.dir.save, command = function() fileORdir2Save(dir.save, isFile = FALSE))

        ############################################

        bt.CalcPICSA <- ttkbutton(subfr2, text = lang.dlg[['button']][['1']])

        tkconfigure(bt.CalcPICSA, command = function(){
            GeneralParameters$onset <- trimws(tclvalue(input.Onset))
            GeneralParameters$cessation <- trimws(tclvalue(input.Cessation))
            GeneralParameters$output <- trimws(tclvalue(dir.save))

            GeneralParameters$min.frac <- as.numeric(trimws(tclvalue(min.frac)))
            GeneralParameters$dryday <- as.numeric(trimws(tclvalue(drywet.day)))

            GeneralParameters$seastot$useTotal <- switch(tclvalue(useTotal), '0' = FALSE, '1' = TRUE)
            if(tclvalue(useTotal) == "1"){
                GeneralParameters$seastot$Tstep <- periodVAL[CbperiodVAL %in% trimws(tclvalue(timeSteps))]
                GeneralParameters$seastot$data.type <- datatypeVAL[CbdatatypeVAL %in% trimws(tclvalue(DataType))]

                if(GeneralParameters$seastot$data.type == 'cdtstation')
                    GeneralParameters$seastot$cdtstation$prec <- trimws(tclvalue(input.Prec))

                if(GeneralParameters$seastot$data.type == 'cdtdataset')
                    GeneralParameters$seastot$cdtdataset$prec <- trimws(tclvalue(input.Prec))
            }

            # assign('GeneralParameters', GeneralParameters, envir = .GlobalEnv)

            Insert.Messages.Out(lang.dlg[['message']][['1']], TRUE, "i")

            tkconfigure(.cdtEnv$tcl$main$win, cursor = 'watch')
            tcl('update')
            ret <- tryCatch2({
                            compute_RainySeasonData(GeneralParameters)
                        },
                        error = function(e) errorFun(e),
                        finally = {
                            tkconfigure(.cdtEnv$tcl$main$win, cursor = '')
                            tcl('update')
                        })

            if(!is.null(ret)){
                if(ret == 0){
                    Insert.Messages.Out(lang.dlg[['message']][['2']], TRUE, "s")

                    .cdtData$EnvData$plot.maps[c('lon', 'lat', 'id')] <- .cdtData$EnvData$output$data[c('lon', 'lat', 'id')]
                    ###################

                    load.PICSA.Data()

                }else Insert.Messages.Out(lang.dlg[['message']][['3']], TRUE, 'e')
            }else Insert.Messages.Out(lang.dlg[['message']][['3']], TRUE, 'e')
        })

        ############################################

        tkgrid(frameDryDay, row = 0, column = 0, sticky = 'we', padx = 1, pady = 3, ipadx = 1, ipady = 1)
        tkgrid(frameMinFrac, row = 1, column = 0, sticky = 'we', padx = 1, pady = 3, ipadx = 1, ipady = 1)
        tkgrid(frameDirSav, row = 2, column = 0, sticky = 'we', padx = 1, pady = 3, ipadx = 1, ipady = 1)
        tkgrid(bt.CalcPICSA, row = 3, column = 0, sticky = 'we', padx = 1, pady = 3, ipadx = 1, ipady = 1)

    #######################################################################################################

    #Tab3
    subfr3 <- bwTabScrollableFrame(cmd.tab3)

        ##############################################

        framePICSADat <- ttklabelframe(subfr3, text = lang.dlg[['label']][['14']], relief = 'groove')

        DirExist <- tclVar(0)
        file.PICSAIndex <- tclVar()

        statePICSADat <- if(tclvalue(DirExist) == "1") "normal" else "disabled"

        chk.PICSAIdx <- tkcheckbutton(framePICSADat, variable = DirExist, text = lang.dlg[['checkbutton']][['2']], anchor = 'w', justify = 'left')
        en.PICSAIdx <- tkentry(framePICSADat, textvariable = file.PICSAIndex, width = largeur0 + 5, state = statePICSADat)
        bt.PICSAIdx <- ttkbutton(framePICSADat, text = .cdtEnv$tcl$lang$global[['button']][['6']], state = statePICSADat)

        tkgrid(chk.PICSAIdx, row = 0, column = 0, sticky = 'we', rowspan = 1, columnspan = 4, padx = 1, pady = 1, ipadx = 1, ipady = 1)
        tkgrid(bt.PICSAIdx, row = 0, column = 4, sticky = 'e', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
        tkgrid(en.PICSAIdx, row = 1, column = 0, sticky = 'we', rowspan = 1, columnspan = 5, padx = 1, pady = 1, ipadx = 1, ipady = 1)

        helpWidget(chk.PICSAIdx, lang.dlg[['tooltip']][['10']], lang.dlg[['status']][['10']])
        helpWidget(en.PICSAIdx, lang.dlg[['tooltip']][['11']], lang.dlg[['status']][['11']])
        helpWidget(bt.PICSAIdx, lang.dlg[['tooltip']][['3']], lang.dlg[['status']][['3']])

        ###############

        tkconfigure(bt.PICSAIdx, command = function(){
            path.Stat <- tclvalue(tkgetOpenFile(initialdir = getwd(), filetypes = .cdtEnv$tcl$data$filetypes6))
            if(path.Stat %in% c("", "NA") | is.na(path.Stat)) return(NULL)
            tclvalue(file.PICSAIndex) <- path.Stat

            if(file.exists(trimws(tclvalue(file.PICSAIndex)))){
                OutPicsa <- try(readRDS(trimws(tclvalue(file.PICSAIndex))), silent = TRUE)
                if(inherits(OutPicsa, "try-error")){
                    Insert.Messages.Out(lang.dlg[['message']][['4']], TRUE, 'e')
                    Insert.Messages.Out(gsub('[\r\n]', '', OutPicsa[1]), TRUE, 'e')
                    return(NULL)
                }

                .cdtData$EnvData$output <- OutPicsa
                .cdtData$EnvData$PathPicsa <- dirname(trimws(tclvalue(file.PICSAIndex)))
                .cdtData$EnvData$plot.maps[c('lon', 'lat', 'id')] <- .cdtData$EnvData$output$data[c('lon', 'lat', 'id')]
                ###################

                load.PICSA.Data()
            }
        })

        ###############

        tkbind(chk.PICSAIdx, "<Button-1>", function(){
            statePICSADat <- if(tclvalue(DirExist) == '1') 'disabled' else 'normal'
            tkconfigure(en.PICSAIdx, state = statePICSADat)
            tkconfigure(bt.PICSAIdx, state = statePICSADat)

            stateCaclBut <- if(tclvalue(DirExist) == '1') 'normal' else 'disabled'
            tcl(tknote.cmd, 'itemconfigure', cmd.tab1$IDtab, state = stateCaclBut)
            tcl(tknote.cmd, 'itemconfigure', cmd.tab2$IDtab, state = stateCaclBut)
        })

        ##############################################

        framePICSATSMap <- ttklabelframe(subfr3, text = lang.dlg[['label']][['15']], relief = 'groove')

        varPICSA <- tclVar()
        CbvarPICSAVAL <- lang.dlg[['combobox']][['1']]
        varPICSAVAL <- c('onset', 'cessation', 'lengthSeas', 'totrainSeas',
                         'dryspell', 'longdryspell', 'nbrainSeas', 'max24hrain',
                         'totrain95P', 'nbrain95P')
        tclvalue(varPICSA) <- CbvarPICSAVAL[varPICSAVAL %in% GeneralParameters$plotVar$varPICSA]

        .cdtData$EnvData$plotVar$varPICSA <- GeneralParameters$plotVar$varPICSA

        cb.TsMap.picsavar <- ttkcombobox(framePICSATSMap, values = CbvarPICSAVAL, textvariable = varPICSA, justify = 'center', width = largeur4)
        bt.TsMap.plot <- ttkbutton(framePICSATSMap, text = .cdtEnv$tcl$lang$global[['button']][['3']], width = largeur5)
        bt.TsMap.Opt <- ttkbutton(framePICSATSMap, text = .cdtEnv$tcl$lang$global[['button']][['4']], width = largeur5)

        #################

        var.TsMap.year <- tclVar(GeneralParameters$plotVar$yearseas)
        .cdtData$EnvData$plotVar$yearseas <- GeneralParameters$plotVar$yearseas

        cb.TsMap.year <- ttkcombobox(framePICSATSMap, values = "", textvariable = var.TsMap.year, justify = 'center', width = largeur6)
        bt.TsMap.prev <- ttkbutton(framePICSATSMap, text = "<<", width = largeur7)
        bt.TsMap.next <- ttkbutton(framePICSATSMap, text = ">>", width = largeur7)

        #################

        fr.TsMap.dryspell <- tkframe(framePICSATSMap)

        .cdtData$EnvData$plotVar$dryspell <- GeneralParameters$plotVar$dryspell

        stateDrySpl <- 'disabled'

        txt.TsMap.dryspell <- tklabel(fr.TsMap.dryspell, text = lang.dlg[['label']][['16']], anchor = 'w', justify = 'left')
        spin.TsMap.dryspell <- ttkspinbox(fr.TsMap.dryspell, from = 1, to = 50, increment = 1, justify = 'center', width = 3, state = stateDrySpl)
        tkset(spin.TsMap.dryspell, GeneralParameters$plotVar$dryspell)

        tkgrid(txt.TsMap.dryspell, spin.TsMap.dryspell)

        #################

        tkgrid(cb.TsMap.picsavar, row = 0, column = 1, sticky = 'we', rowspan = 1, columnspan = 8, padx = 1, pady = 1, ipadx = 1, ipady = 1)
        tkgrid(bt.TsMap.Opt, row = 1, column = 0, sticky = 'we', rowspan = 1, columnspan = 5, ipadx = 1, ipady = 1)
        tkgrid(bt.TsMap.plot, row = 1, column = 5, sticky = 'we', rowspan = 1, columnspan = 5, ipadx = 1, ipady = 1)

        tkgrid(bt.TsMap.prev, row = 2, column = 0, sticky = 'we', rowspan = 1, columnspan = 2, padx = 1, pady = 1, ipadx = 1, ipady = 1)
        tkgrid(cb.TsMap.year, row = 2, column = 2, sticky = 'we', rowspan = 1, columnspan = 6, padx = 1, pady = 1, ipadx = 1, ipady = 1)
        tkgrid(bt.TsMap.next, row = 2, column = 8, sticky = 'we', rowspan = 1, columnspan = 2, padx = 1, pady = 1, ipadx = 1, ipady = 1)

        tkgrid(fr.TsMap.dryspell, row = 3, column = 0, sticky = '', rowspan = 1, columnspan = 10, padx = 1, pady = 1, ipadx = 1, ipady = 1)

        #################

        tkconfigure(bt.TsMap.Opt, command = function(){
            if(!is.null(.cdtData$EnvData$tsdata)){
                atlevel <- pretty(.cdtData$EnvData$tsdata$z, n = 10, min.n = 7)
                if(is.null(.cdtData$EnvData$TSMapOp$userLvl$levels)){
                    .cdtData$EnvData$TSMapOp$userLvl$levels <- atlevel
                }else{
                    if(!.cdtData$EnvData$TSMapOp$userLvl$custom)
                        .cdtData$EnvData$TSMapOp$userLvl$levels <- atlevel
                }
            }
            .cdtData$EnvData$TSMapOp <- MapGraph.MapOptions(.cdtData$EnvData$TSMapOp)
        })

        #################

        .cdtData$EnvData$tab$TSMap <- NULL
 
        tkconfigure(bt.TsMap.plot, command = function(){
            get.analysis.method()

            if(!is.null(.cdtData$EnvData$tsdata)){
                ret <- read.PicsaTSData()
                if(is.null(ret)) return(NULL)

                SeasonAnalysis.Display.TSMaps()
            }
        })

        tkconfigure(bt.TsMap.prev, command = function(){
            get.analysis.method()

            if(!is.null(.cdtData$EnvData$tsdata)){
                val.TsMap.year <- format(.cdtData$EnvData$output$start.date, '%Y')
                iyear <- which(val.TsMap.year == .cdtData$EnvData$plotVar$yearseas)
                iyear <- iyear - 1
                if(iyear < 1) iyear <- length(val.TsMap.year)
                tclvalue(var.TsMap.year) <- val.TsMap.year[iyear]
                .cdtData$EnvData$plotVar$yearseas <- val.TsMap.year[iyear]

                ret <- read.PicsaTSData()
                if(is.null(ret)) return(NULL)

                SeasonAnalysis.Display.TSMaps()
            }
        })

        tkconfigure(bt.TsMap.next, command = function(){
            get.analysis.method()

            if(!is.null(.cdtData$EnvData$tsdata)){
                val.TsMap.year <- format(.cdtData$EnvData$output$start.date, '%Y')
                iyear <- which(val.TsMap.year == .cdtData$EnvData$plotVar$yearseas)
                iyear <- iyear + 1
                if(iyear > length(val.TsMap.year)) iyear <- 1
                tclvalue(var.TsMap.year) <- val.TsMap.year[iyear]
                .cdtData$EnvData$plotVar$yearseas <- val.TsMap.year[iyear]

                ret <- read.PicsaTSData()
                if(is.null(ret)) return(NULL)

                SeasonAnalysis.Display.TSMaps()
            }
        })

        #################

        tkbind(cb.TsMap.picsavar, "<<ComboboxSelected>>", function(){
            get.analysis.method()

            stateDrySpl <- if(.cdtData$EnvData$plotVar$varPICSA == "dryspell") "normal" else "disabled"
            tkconfigure(spin.TsMap.dryspell, state = stateDrySpl)

            if(!is.null(.cdtData$EnvData$output)){
                ret <- read.PicsaTSData()
                if(is.null(ret)) return(NULL)
            }
        })

        ##############################################

        funClimMapStat <- function(analysis.mthd){
            tkdestroy(fr.ClimMap.Stat)
            fr.ClimMap.Stat <<- tkframe(framePICSACLMMap)

            ############

            if(analysis.mthd == 'perc'){
                en.Percent <- tkentry(fr.ClimMap.Stat, textvariable = percentClimAna, width = 4, justify = 'center')
                th.Percent <- tklabel(fr.ClimMap.Stat, text = lang.dlg[['label']][['17']], anchor = 'w', justify = 'left')
                txt.Percent <- tklabel(fr.ClimMap.Stat, text = lang.dlg[['label']][['18']])

                tkgrid(en.Percent, th.Percent, txt.Percent)

                helpWidget(en.Percent, lang.dlg[['tooltip']][['12']], lang.dlg[['status']][['12']])
            }
            if(analysis.mthd == 'freq'){
                txt.Freq1 <- tklabel(fr.ClimMap.Stat, text = "Between", anchor = 'e', justify = 'right')
                en.Freq1 <- tkentry(fr.ClimMap.Stat, textvariable = freqLowClimAna, width = 7, justify = 'center')
                txt.Freq2 <- tklabel(fr.ClimMap.Stat, text = "And")
                en.Freq2 <- tkentry(fr.ClimMap.Stat, textvariable = freqUpClimAna, width = 7, justify = 'center')

                tkgrid(txt.Freq1, en.Freq1, txt.Freq2, en.Freq2)

                helpWidget(en.Freq1, lang.dlg[['tooltip']][['13']], lang.dlg[['status']][['13']])
                helpWidget(en.Freq2, lang.dlg[['tooltip']][['14']], lang.dlg[['status']][['14']])
            }
            if(analysis.mthd == 'trend'){
                cb.Trend <- ttkcombobox(fr.ClimMap.Stat, values = CbtrendAnalVAL, textvariable = trendClimAna, width = largeur8)

                tkgrid(cb.Trend)
            }

            ############
            tkgrid(fr.ClimMap.Stat, row = 2, column = 0, sticky = '', rowspan = 1, columnspan = 10, padx = 1, pady = 3, ipadx = 1, ipady = 1)
        }

        ##############################################

        framePICSACLMMap <- ttklabelframe(subfr3, text = lang.dlg[['label']][['19']], relief = 'groove')

        analysisMethod <- tclVar()
        CbAnalysisVAL <- lang.dlg[['combobox']][['2']]
        AnalysisVAL <- c('mean', 'med', 'std', 'trend', 'perc', 'freq')
        tclvalue(analysisMethod) <- CbAnalysisVAL[AnalysisVAL %in% GeneralParameters$analysis$method]

        .cdtData$EnvData$analysis$method <- GeneralParameters$analysis$method

        cb.ClimMap.mth <- ttkcombobox(framePICSACLMMap, values = CbAnalysisVAL, textvariable = analysisMethod, justify = 'center', width = largeur9)
        bt.ClimMap.plot <- ttkbutton(framePICSACLMMap, text = .cdtEnv$tcl$lang$global[['button']][['3']], width = largeur5)
        bt.ClimMap.Opt <- ttkbutton(framePICSACLMMap, text = .cdtEnv$tcl$lang$global[['button']][['4']], width = largeur5)

        #####################

        fr.ClimMap.Stat <- tkframe(framePICSACLMMap)

        trendClimAna <- tclVar()
        CbtrendAnalVAL <- lang.dlg[['combobox']][['3']]
        trendAnalVAL <- c('trendEY', 'trendOP', 'trendAP')
        tclvalue(trendClimAna) <- CbtrendAnalVAL[trendAnalVAL %in% GeneralParameters$analysis$trend]

        percentClimAna <- tclVar(GeneralParameters$analysis$mth.perc)
        freqLowClimAna <- tclVar(GeneralParameters$analysis$low.thres)
        freqUpClimAna <- tclVar(GeneralParameters$analysis$up.thres)

        .cdtData$EnvData$analysis$trend <- GeneralParameters$analysis$trend
        .cdtData$EnvData$analysis$mth.perc <- GeneralParameters$analysis$mth.perc
        .cdtData$EnvData$analysis$low.thres <- GeneralParameters$analysis$low.thres
        .cdtData$EnvData$analysis$up.thres <- GeneralParameters$analysis$up.thres

        ######

        funClimMapStat(GeneralParameters$analysis$method)

        #####################

        tkgrid(cb.ClimMap.mth, row = 0, column = 2, sticky = 'we', rowspan = 1, columnspan = 6, padx = 1, pady = 1, ipadx = 1, ipady = 1)
        tkgrid(bt.ClimMap.Opt, row = 1, column = 0, sticky = 'we', rowspan = 1, columnspan = 5, ipadx = 1, ipady = 1)
        tkgrid(bt.ClimMap.plot, row = 1, column = 5, sticky = 'we', rowspan = 1, columnspan = 5, ipadx = 1, ipady = 1)

        #################

        tkconfigure(bt.ClimMap.Opt, command = function(){
            if(!is.null(.cdtData$EnvData$climdata)){
                atlevel <- pretty(.cdtData$EnvData$climdata$z, n = 10, min.n = 7)
                if(is.null(.cdtData$EnvData$climMapOp$userLvl$levels)){
                    .cdtData$EnvData$climMapOp$userLvl$levels <- atlevel
                }else{
                    if(!.cdtData$EnvData$climMapOp$userLvl$custom)
                        .cdtData$EnvData$climMapOp$userLvl$levels <- atlevel
                }
            }
            .cdtData$EnvData$climMapOp <- MapGraph.MapOptions(.cdtData$EnvData$climMapOp)
        })

        #################

        .cdtData$EnvData$tab$ClimMap <- NULL

        tkconfigure(bt.ClimMap.plot, command = function(){
            get.analysis.method()

            ret <- calculate.ClimStat()
            if(is.null(ret)) return(NULL)

            if(!is.null(.cdtData$EnvData$climdata)) SeasonAnalysis.Display.ClimMap()
        })

        #################

        tkbind(cb.ClimMap.mth, "<<ComboboxSelected>>", function(){
            analysis.method <- AnalysisVAL[CbAnalysisVAL %in% trimws(tclvalue(analysisMethod))]
            funClimMapStat(analysis.method)
        })

        ##############################################

        tkgrid(framePICSADat, row = 0, column = 0, sticky = 'we', padx = 1, pady = 1, ipadx = 1, ipady = 1)
        tkgrid(framePICSATSMap, row = 1, column = 0, sticky = 'we', padx = 1, pady = 1, ipadx = 1, ipady = 1)
        tkgrid(framePICSACLMMap, row = 2, column = 0, sticky = 'we', padx = 1, pady = 1, ipadx = 1, ipady = 1)

    #######################################################################################################

    #Tab4
    subfr4 <- bwTabScrollableFrame(cmd.tab4)

        ##############################################

        framePICSATSGRAPH <- ttklabelframe(subfr4, text = lang.dlg[['label']][['21']], relief = 'groove')

        CbvarTSPLOTVAL <- lang.dlg[['combobox']][['4']]
        varTSPLOTVAL <- c('onset', 'cessation', 'lengthSeas', 'totrainSeas',
                          'dryspell', 'longdryspell', 'nbrainSeas', 'max24hrain',
                          'totrain95P', 'nbrain95P', 'raints')

        varTSp <- tclVar()
        tclvalue(varTSp) <- CbvarTSPLOTVAL[varTSPLOTVAL %in% GeneralParameters$graph$varTSp]

        .cdtData$EnvData$plot.maps$varTSp <- GeneralParameters$graph$varTSp

        CbtypeTSPLOTVAL <- lang.dlg[['combobox']][['5']]
        typeTSPLOTVAL <- c('line', 'bar', 'proba', 'eline', 'ebar', 'eproba', 'anom')
        typeTSp <- tclVar()
        tclvalue(typeTSp) <- CbtypeTSPLOTVAL[typeTSPLOTVAL %in% GeneralParameters$graph$typeTSp]

        .cdtData$EnvData$plot.maps$typeTSp <- GeneralParameters$graph$typeTSp
        .cdtData$EnvData$plot.maps$averageTSp <- tclVar(FALSE)
        .cdtData$EnvData$plot.maps$tercileTSp <- tclVar(FALSE)
        .cdtData$EnvData$plot.maps$trendTSp <- tclVar(FALSE)

        stateTsp <- if(GeneralParameters$graph$varTSp != "raints") "normal" else "disabled"
        stateType <- if(GeneralParameters$graph$typeTSp %in% c("line", "eline") &&
                        GeneralParameters$graph$varTSp != "raints") "normal" else "disabled"

        cb.varTSp <- ttkcombobox(framePICSATSGRAPH, values = CbvarTSPLOTVAL, textvariable = varTSp, justify = 'center', width = largeur4)
        cb.typeTSp <- ttkcombobox(framePICSATSGRAPH, values = CbtypeTSPLOTVAL, textvariable = typeTSp, justify = 'center', width = largeur11, state = stateTsp)

        bt.TsGraph.plot <- ttkbutton(framePICSATSGRAPH, text = .cdtEnv$tcl$lang$global[['button']][['3']], width = largeur5)
        bt.TsGraph.Opt <- ttkbutton(framePICSATSGRAPH, text = .cdtEnv$tcl$lang$global[['button']][['4']], width = largeur5, state = stateTsp)

        frTS1 <- tkframe(framePICSATSGRAPH)
        chk.meanTSp <- tkcheckbutton(frTS1, variable = .cdtData$EnvData$plot.maps$averageTSp, text = lang.dlg[['checkbutton']][['3']], anchor = 'w', justify = 'left', state = stateType)
        chk.tercTSp <- tkcheckbutton(frTS1, variable = .cdtData$EnvData$plot.maps$tercileTSp, text = lang.dlg[['checkbutton']][['4']], anchor = 'w', justify = 'left', state = stateType)
        chk.trendTSp <- tkcheckbutton(frTS1, variable = .cdtData$EnvData$plot.maps$trendTSp, text = lang.dlg[['checkbutton']][['5']], anchor = 'w', justify = 'left', state = stateType)
        tkgrid(chk.meanTSp, chk.tercTSp, chk.trendTSp)

        #################

        tkgrid(cb.varTSp, row = 0, column = 1, sticky = '', rowspan = 1, columnspan = 8, padx = 1, pady = 1, ipadx = 1, ipady = 1)
        tkgrid(cb.typeTSp, row = 1, column = 1, sticky = '', rowspan = 1, columnspan = 8, padx = 1, pady = 10, ipadx = 1, ipady = 1)
        tkgrid(bt.TsGraph.Opt, row = 2, column = 0, sticky = 'we', rowspan = 1, columnspan = 5, padx = 1, pady = 1, ipadx = 1, ipady = 1)
        tkgrid(bt.TsGraph.plot, row = 2, column = 5, sticky = 'we', rowspan = 1, columnspan = 5, padx = 1, pady = 1, ipadx = 1, ipady = 1)
        tkgrid(frTS1, row = 3, column = 0, sticky = '', rowspan = 1, columnspan = 10, padx = 1, pady = 1, ipadx = 1, ipady = 1)

        #################

        tkconfigure(bt.TsGraph.Opt, command = function(){
            vtypeTSp <- typeTSPLOTVAL[CbtypeTSPLOTVAL %in% trimws(tclvalue(typeTSp))]
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

        .cdtData$EnvData$tab$Tsplot <- NULL

        tkconfigure(bt.TsGraph.plot, command = function(){
            .cdtData$EnvData$plot.maps$typeTSp <- typeTSPLOTVAL[CbtypeTSPLOTVAL %in% trimws(tclvalue(typeTSp))]
            .cdtData$EnvData$plot.maps$varTSp <- varTSPLOTVAL[CbvarTSPLOTVAL %in% trimws(tclvalue(varTSp))]

            if(!is.null(.cdtData$EnvData$output)){
                imgContainer <- CDT.Display.Graph(SeasonAnalysis.plot.TSGraph, .cdtData$EnvData$tab$Tsplot, 'Time-Series-Plot')
                .cdtData$EnvData$tab$Tsplot <- imageNotebookTab_unik(imgContainer, .cdtData$EnvData$tab$Tsplot)
            }
        })

        #################

        tkbind(cb.varTSp, "<<ComboboxSelected>>", function(){
            .cdtData$EnvData$plot.maps$typeTSp <- typeTSPLOTVAL[CbtypeTSPLOTVAL %in% trimws(tclvalue(typeTSp))]
            .cdtData$EnvData$plot.maps$varTSp <- varTSPLOTVAL[CbvarTSPLOTVAL %in% trimws(tclvalue(varTSp))]

            stateTsp <- if(.cdtData$EnvData$plot.maps$varTSp != "raints") "normal" else "disabled"
            tkconfigure(cb.typeTSp, state = stateTsp)
            tkconfigure(bt.TsGraph.Opt, state = stateTsp)

            stateType <- if(.cdtData$EnvData$plot.maps$typeTSp %in% c('line', 'eline') &&
                            .cdtData$EnvData$plot.maps$varTSp != "raints") "normal" else "disabled"
            tkconfigure(chk.meanTSp, state = stateType)
            tkconfigure(chk.tercTSp, state = stateType)
            tkconfigure(chk.trendTSp, state = stateType)
        })

        tkbind(cb.typeTSp, "<<ComboboxSelected>>", function(){
            .cdtData$EnvData$plot.maps$typeTSp <- typeTSPLOTVAL[CbtypeTSPLOTVAL %in% trimws(tclvalue(typeTSp))]
            .cdtData$EnvData$plot.maps$varTSp <- varTSPLOTVAL[CbvarTSPLOTVAL %in% trimws(tclvalue(varTSp))]

            stateType <- if(.cdtData$EnvData$plot.maps$typeTSp %in% c('line', 'eline') &&
                            .cdtData$EnvData$plot.maps$varTSp != "raints") "normal" else "disabled"
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

        frameSTNCrds <- ttklabelframe(subfr4, text = lang.dlg[['label']][['22']], relief = 'groove')

        frTS2 <- tkframe(frameSTNCrds)
        .cdtData$EnvData$plot.maps$lonLOC <- tclVar()
        .cdtData$EnvData$plot.maps$latLOC <- tclVar()
        .cdtData$EnvData$plot.maps$stnIDTSp <- tclVar()

        tkgrid(frTS2, row = 0, column = 0, sticky = 'e', pady = 1)

        ##############################################

        tkgrid(framePICSATSGRAPH, row = 0, column = 0, sticky = 'we', padx = 1, pady = 1, ipadx = 1, ipady = 1)
        tkgrid(frameSTNCrds, row = 1, column = 0, sticky = '', padx = 1, pady = 3, ipadx = 1, ipady = 1)

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

        if(.cdtData$EnvData$output$data.type == "cdtstation"){
            stnIDTSPLOT <- .cdtData$EnvData$output$data$id
            txt.stnSel <- tklabel(frTS2, text = lang.dlg[['label']][['24']], anchor = 'w', justify = 'left')
            bt.stnID.prev <- ttkbutton(frTS2, text = "<<", width = largeur7)
            bt.stnID.next <- ttkbutton(frTS2, text = ">>", width = largeur7)
            cb.stnID <- ttkcombobox(frTS2, values = stnIDTSPLOT, textvariable = .cdtData$EnvData$plot.maps$stnIDTSp, width = largeur6, justify = 'center')
            tclvalue(.cdtData$EnvData$plot.maps$stnIDTSp) <- stnIDTSPLOT[1]

            tkgrid(txt.stnSel, row = 0, column = 0, sticky = '', rowspan = 1, columnspan = 3, padx = 1, pady = 1, ipadx = 1, ipady = 1)
            tkgrid(bt.stnID.prev, row = 1, column = 0, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
            tkgrid(cb.stnID, row = 1, column = 1, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
            tkgrid(bt.stnID.next, row = 1, column = 2, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)

            #######
            tkconfigure(bt.stnID.prev, command = function(){
                .cdtData$EnvData$plot.maps$typeTSp <- typeTSPLOTVAL[CbtypeTSPLOTVAL %in% trimws(tclvalue(typeTSp))]
                .cdtData$EnvData$plot.maps$varTSp <- varTSPLOTVAL[CbvarTSPLOTVAL %in% trimws(tclvalue(varTSp))]

                if(!is.null(.cdtData$EnvData$output)){
                    istn <- which(stnIDTSPLOT == trimws(tclvalue(.cdtData$EnvData$plot.maps$stnIDTSp)))
                    istn <- istn - 1
                    if(istn < 1) istn <- length(stnIDTSPLOT)
                    tclvalue(.cdtData$EnvData$plot.maps$stnIDTSp) <- stnIDTSPLOT[istn]

                    imgContainer <- CDT.Display.Graph(SeasonAnalysis.plot.TSGraph, .cdtData$EnvData$tab$Tsplot, 'Time-Series-Plot')
                    .cdtData$EnvData$tab$Tsplot <- imageNotebookTab_unik(imgContainer, .cdtData$EnvData$tab$Tsplot)
                }
            })

            tkconfigure(bt.stnID.next, command = function(){
                .cdtData$EnvData$plot.maps$typeTSp <- typeTSPLOTVAL[CbtypeTSPLOTVAL %in% trimws(tclvalue(typeTSp))]
                .cdtData$EnvData$plot.maps$varTSp <- varTSPLOTVAL[CbvarTSPLOTVAL %in% trimws(tclvalue(varTSp))]

                if(!is.null(.cdtData$EnvData$output)){
                    istn <- which(stnIDTSPLOT == trimws(tclvalue(.cdtData$EnvData$plot.maps$stnIDTSp)))
                    istn <- istn + 1
                    if(istn > length(stnIDTSPLOT)) istn <- 1
                    tclvalue(.cdtData$EnvData$plot.maps$stnIDTSp) <- stnIDTSPLOT[istn]

                    imgContainer <- CDT.Display.Graph(SeasonAnalysis.plot.TSGraph, .cdtData$EnvData$tab$Tsplot, 'Time-Series-Plot')
                    .cdtData$EnvData$tab$Tsplot <- imageNotebookTab_unik(imgContainer, .cdtData$EnvData$tab$Tsplot)
                }
            })
        }else{
            txt.crdSel <- tklabel(frTS2, text = lang.dlg[['label']][['25']], anchor = 'w', justify = 'left')
            txt.lonLoc <- tklabel(frTS2, text = lang.dlg[['label']][['26']], anchor = 'e', justify = 'right')
            en.lonLoc <- tkentry(frTS2, textvariable = .cdtData$EnvData$plot.maps$lonLOC, width = largeur12)
            txt.latLoc <- tklabel(frTS2, text = lang.dlg[['label']][['27']], anchor = 'e', justify = 'right')
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

    set.plot.type <- function(data_type){
        if(data_type == 'cdtstation'){
            .data.type <- "Points"
            plot_type <- list(values = c("Pixels", "Points"), var = "Pixels")
        }

        if(data_type == 'cdtdataset'){
            .data.type <- "Grid"
            plot_type <- list(values = c("Pixels", "FilledContour"), var = "Pixels")
        }

        .cdtData$EnvData$climMapOp$plotType <- plot_type
        .cdtData$EnvData$TSMapOp$plotType <- plot_type
        .cdtData$EnvData$plot.maps$.data.type <- .data.type
        .cdtData$EnvData$plot.maps$data.type <- data_type
    }

    #####################

    get.analysis.method <- function(){
        .cdtData$EnvData$plotVar$varPICSA <- varPICSAVAL[CbvarPICSAVAL %in% trimws(tclvalue(varPICSA))]
        .cdtData$EnvData$plotVar$dryspell <- as.numeric(trimws(tclvalue(tkget(spin.TsMap.dryspell))))
        .cdtData$EnvData$plotVar$yearseas <- trimws(tclvalue(var.TsMap.year))

        .cdtData$EnvData$analysis$method <- AnalysisVAL[CbAnalysisVAL %in% trimws(tclvalue(analysisMethod))]
        .cdtData$EnvData$analysis$trend <- trendAnalVAL[CbtrendAnalVAL %in% trimws(tclvalue(trendClimAna))]
        .cdtData$EnvData$analysis$mth.perc <- as.numeric(trimws(tclvalue(percentClimAna)))

        xlow <- trimws(tclvalue(freqLowClimAna))
        xup <- trimws(tclvalue(freqUpClimAna))

        # if(.cdtData$EnvData$plotVar$varPICSA %in% c("onset", "cessation")){
        #     dlo <- try(as.POSIXlt(as.Date(paste(2015, xlow, sep = '-'))), silent = TRUE)
        #     dup <- try(as.POSIXlt(as.Date(paste(2015, xup, sep = '-'))), silent = TRUE)
        #     if(inherits(dlo, "try-error") | inherits(dup, "try-error"))
        #         Insert.Messages.Out(lang.dlg[['message']][['6']], TRUE, 'e')
        # }else{
        #     dlow <- as.numeric(xlow)
        #     dup <- as.numeric(xup)
        #     if(is.na(xlow) | is.na(xup))
        #         Insert.Messages.Out(lang.dlg[['message']][['6']], TRUE, 'e')
        # }

        .cdtData$EnvData$analysis$low.thres <- xlow
        .cdtData$EnvData$analysis$up.thres <- xup
    }

    #######################################################################################################

    load.PICSA.Data <- function(){
        val.TsMap.year <- format(.cdtData$EnvData$output$start.date, '%Y')
        tkconfigure(cb.TsMap.year, values = val.TsMap.year)
        tclvalue(var.TsMap.year) <- val.TsMap.year[length(val.TsMap.year)]
        .cdtData$EnvData$plotVar$yearseas <- tclvalue(var.TsMap.year)

        ###################
        ret <- read.PicsaTSData()
        if(is.null(ret)) return(NULL)

        ###################
        plotCHOIX <- c("anomaly", "bar", "line", "line.enso", "bar.enso")
        range.TsMap.year <- range(as.numeric(val.TsMap.year))
        for(pp in plotCHOIX){
            .cdtData$EnvData$TSGraphOp[[pp]]$xlim$min <- range.TsMap.year[1]
            .cdtData$EnvData$TSGraphOp[[pp]]$xlim$max <- range.TsMap.year[2]
        }

        ###################
        # widgets.Station.Pixel
        widgets.Station.Pixel()
        set.plot.type(.cdtData$EnvData$output$data.type)

        ###################
        # load daily precip
        if(.cdtData$EnvData$output$data.type == "cdtstation"){
            file.daily.rr <- file.path(.cdtData$EnvData$PathPicsa, "CDTDATASET", "Daily_precip.rds") 
        }else file.daily.rr <- .cdtData$EnvData$output$daily.precip

        if(!file.exists(file.daily.rr)){
            Insert.Messages.Out(paste(file.daily.rr, lang.dlg[['message']][['5']]), TRUE, 'e')
            return(NULL)
        }

        .cdtData$EnvData$daily.precip <- readRDS(file.daily.rr)
    }

    #######################################################################################################

    read.PicsaTSData <- function(){
        tkconfigure(.cdtEnv$tcl$main$win, cursor = 'watch')
        tcl('update')
        on.exit({
            tkconfigure(.cdtEnv$tcl$main$win, cursor = '')
            tcl('update')
        })

        tsdata.dir <- switch(.cdtData$EnvData$plotVar$varPICSA,
                             'onset' = "Onset_days",
                             'cessation' = "Cessation_days",
                             'lengthSeas' = "Season_length",
                             'totrainSeas' = "Seasonal_rain_amount",
                             'nbrainSeas' = "Number_rainy_day",
                             'max24hrain' = "Maximum_rain_daily",
                             'totrain95P' = "Total_rain_above_Perc95th",
                             'nbrain95P' = "Number_day_above_Perc95th",
                             'longdryspell' = "Longest_dry_spell",
                             'dryspell' = "Dry_Spells")

        start.date <- format(.cdtData$EnvData$output$start.date, '%Y%m%d')
        start.dateYear <- format(.cdtData$EnvData$output$start.date, '%Y')
        idaty <- start.date[start.dateYear == .cdtData$EnvData$plotVar$yearseas]
        dryspl <- .cdtData$EnvData$plotVar$dryspell

        if(.cdtData$EnvData$output$data.type == "cdtstation"){
            tsdata.path <- file.path(.cdtData$EnvData$PathPicsa, "CDTDATASET")
            filetsdata <- file.path(tsdata.path, paste0(tsdata.dir, ".rds"))

            if(!file.exists(filetsdata)){
                Insert.Messages.Out(paste(filetsdata, lang.dlg[['message']][['5']]), TRUE, 'e')
                return(NULL)
            }

            change.plot <- .cdtData$EnvData$TSMapOp$plotType$var

            ########
            readTsData <- TRUE
            if(!is.null(.cdtData$EnvData$tsdata))
                if(!is.null(.cdtData$EnvData$filetsdata))
                    if(.cdtData$EnvData$filetsdata == filetsdata) readTsData <- FALSE

            if(readTsData){
                .cdtData$EnvData$tsdata <- list(date = start.date, data = readRDS(filetsdata))
                .cdtData$EnvData$filetsdata <- filetsdata
            }

            ########
            rasterTsData <- TRUE
            if(!readTsData)
                if(!is.null(.cdtData$EnvData$rasterTsData))
                    if(.cdtData$EnvData$filetsdata == filetsdata)
                        if(.cdtData$EnvData$rasterTsData == idaty) rasterTsData <- FALSE

            if(.cdtData$EnvData$plotVar$varPICSA == "dryspell")
                if(!is.null(.cdtData$EnvData$oldDryspell))
                    if(.cdtData$EnvData$oldDryspell != dryspl & !rasterTsData) rasterTsData <- TRUE

            if(!rasterTsData)
                if(.cdtData$EnvData$change.plot.rasterTsData != change.plot) rasterTsData <- TRUE

            if(rasterTsData){
                X0 <- .cdtData$EnvData$output$data$lon
                Y0 <- .cdtData$EnvData$output$data$lat
                idt <- which(.cdtData$EnvData$tsdata$date == idaty)
                if(.cdtData$EnvData$plotVar$varPICSA == "dryspell"){
                    tmp <- .cdtData$EnvData$tsdata$data[idt, ]
                    nval <- sapply(tmp, function(x) (length(x) == 1) & is.na(x[1]))
                    tmp <- sapply(tmp, function(x) sum(!is.na(x) & x >= dryspl))
                    tmp[nval] <- NA
                    rm(nval)
                    .cdtData$EnvData$oldDryspell <- dryspl
                }else tmp <- as.numeric(.cdtData$EnvData$tsdata$data[idt, ])

                if(change.plot == "Pixels"){
                    nx <- nx_ny_as.image(diff(range(X0)))
                    ny <- nx_ny_as.image(diff(range(Y0)))
                    tmp <- cdt.as.image(tmp, nx = nx, ny = ny, pts.xy = cbind(X0, Y0))
                    .cdtData$EnvData$tsdata$x <- tmp$x
                    .cdtData$EnvData$tsdata$y <- tmp$y
                    .cdtData$EnvData$tsdata$z <- tmp$z
                }

                if(change.plot == "Points"){
                    .cdtData$EnvData$tsdata$x <- X0
                    .cdtData$EnvData$tsdata$y <- Y0
                    .cdtData$EnvData$tsdata$z <- tmp
                }

                .cdtData$EnvData$rasterTsData <- idaty
                .cdtData$EnvData$change.plot.rasterTsData <- change.plot
                rm(tmp)
            }
        }else{
            if(.cdtData$EnvData$plotVar$varPICSA == "dryspell"){
                tsdata.path <- file.path(.cdtData$EnvData$PathPicsa, "CDTDATASET", tsdata.dir)
                tsdata.index <- file.path(.cdtData$EnvData$PathPicsa, "CDTDATASET", "CDTDATASET.rds")
                filetsdata <- tsdata.path
            }else{
                tsdata.path <- file.path(.cdtData$EnvData$PathPicsa, "DATA_NetCDF")
                filetsdata <- file.path(tsdata.path, tsdata.dir, paste0("data_", idaty, ".nc"))
            }

            if(!file.exists(filetsdata)){
                Insert.Messages.Out(paste(filetsdata, lang.dlg[['message']][['5']]), TRUE, 'e')
                return(NULL)
            }

            readTsData <- TRUE
            if(!is.null(.cdtData$EnvData$tsdata))
                if(!is.null(.cdtData$EnvData$filetsdata))
                    if(.cdtData$EnvData$filetsdata == filetsdata) readTsData <- FALSE

            if(.cdtData$EnvData$plotVar$varPICSA == "dryspell")
                if(!is.null(.cdtData$EnvData$oldDryspell))
                    if(.cdtData$EnvData$oldDryspell != dryspl & !readTsData) readTsData <- TRUE

            if(readTsData){
                if(.cdtData$EnvData$plotVar$varPICSA == "dryspell"){
                    cdtParallelCond <- .cdtData$Config$parallel
                    .cdtData$EnvData$tsdata <- readCdtDatasetChunk.sepdir.dates.order(tsdata.index, tsdata.path, idaty,
                                                                                      cdtParallelCond, onedate = TRUE)
                    zdim <- dim(.cdtData$EnvData$tsdata$z)
                    nval <- sapply(.cdtData$EnvData$tsdata$z, function(x) (length(x) == 1) & is.na(x[1]))
                    zval <- sapply(.cdtData$EnvData$tsdata$z, function(x) sum(!is.na(x) & x >= dryspl))
                    zval[nval] <- NA
                    dim(zval) <- zdim
                    .cdtData$EnvData$tsdata$z <- zval
                    rm(nval, zval)
                    .cdtData$EnvData$oldDryspell <- dryspl
                }else{
                    nc <- ncdf4::nc_open(filetsdata)
                    .cdtData$EnvData$tsdata$x <- nc$dim[[1]]$vals
                    .cdtData$EnvData$tsdata$y <- nc$dim[[2]]$vals
                    .cdtData$EnvData$tsdata$z <- ncdf4::ncvar_get(nc, varid = nc$var[[1]]$name)
                    ncdf4::nc_close(nc)
                }
                .cdtData$EnvData$filetsdata <- filetsdata
            }

            if(is.null(.cdtData$EnvData$cdtdataset)){
                tsdata.index <- file.path(.cdtData$EnvData$PathPicsa, "CDTDATASET", "CDTDATASET.rds")
                .cdtData$EnvData$cdtdataset <- readRDS(tsdata.index)
                .cdtData$EnvData$cdtdataset$fileInfo <- tsdata.index
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

    calculate.ClimStat <- function(){
        tkconfigure(.cdtEnv$tcl$main$win, cursor = 'watch')
        tcl('update')
        on.exit({
            tkconfigure(.cdtEnv$tcl$main$win, cursor = '')
            tcl('update')
        })

        tsdata.dir <- switch(.cdtData$EnvData$plotVar$varPICSA,
                             'onset' = "Onset_days",
                             'cessation' = "Cessation_days",
                             'lengthSeas' = "Season_length",
                             'totrainSeas' = "Seasonal_rain_amount",
                             'nbrainSeas' = "Number_rainy_day",
                             'max24hrain' = "Maximum_rain_daily",
                             'totrain95P' = "Total_rain_above_Perc95th",
                             'nbrain95P' = "Number_day_above_Perc95th",
                             'longdryspell' = "Dry_Spells",
                             'dryspell' = "Dry_Spells")

        dryspl <- .cdtData$EnvData$plotVar$dryspell

        if(.cdtData$EnvData$output$data.type == "cdtstation"){
            tsdata.path <- file.path(.cdtData$EnvData$PathPicsa, "CDTDATASET")
            filetsdata <- file.path(tsdata.path, paste0(tsdata.dir, ".rds"))

            if(!file.exists(filetsdata)){
                Insert.Messages.Out(paste(filetsdata, lang.dlg[['message']][['5']]), TRUE, 'e')
                return(NULL)
            }

            change.plot <- .cdtData$EnvData$climMapOp$plotType$var

            ########
            calcClim <- TRUE
            if(!is.null(.cdtData$EnvData$climdata))
                if(!is.null(.cdtData$EnvData$filetsdata1))
                    if(.cdtData$EnvData$filetsdata1 == filetsdata)
                        if(.cdtData$EnvData$StatCalc == .cdtData$EnvData$analysis$method) calcClim <- FALSE

            trendUnit <- .cdtData$EnvData$analysis$trend
            if(.cdtData$EnvData$analysis$method == "trend")
                if(!is.null(.cdtData$EnvData$trendUnit))
                    if(.cdtData$EnvData$trendUnit != trendUnit & !calcClim) calcClim <- TRUE

            if(.cdtData$EnvData$plotVar$varPICSA == 'dryspell')
                if(!is.null(.cdtData$EnvData$oldDryspell1))
                    if(.cdtData$EnvData$oldDryspell1 != dryspl & !calcClim) calcClim <- TRUE

            if(.cdtData$EnvData$plotVar$varPICSA == 'longdryspell')
                if(!is.null(.cdtData$EnvData$oldDryspell2))
                    if(.cdtData$EnvData$oldDryspell2 != dryspl & !calcClim) calcClim <- TRUE

            if(!calcClim)
                if(.cdtData$EnvData$change.plot.calcClim != change.plot) calcClim <- TRUE

            if(calcClim){
                don <- readRDS(filetsdata)
                if(.cdtData$EnvData$plotVar$varPICSA == 'dryspell'){
                    ndim <- dim(don)
                    nval <- sapply(don, function(x) (length(x) == 1) & is.na(x[1]))
                    don <- sapply(don, function(x) sum(!is.na(x) & x >= dryspl))
                    don[nval] <- NA
                    dim(don) <- ndim
                    rm(nval)
                    .cdtData$EnvData$oldDryspell1 <- dryspl
                }
                if(.cdtData$EnvData$plotVar$varPICSA == 'longdryspell'){
                    ndim <- dim(don)
                    don <- sapply(don, max, na.rm = TRUE)
                    don[is.infinite(don)] <- NA
                    dim(don) <- ndim
                    .cdtData$EnvData$oldDryspell2 <- dryspl
                }

                X0 <- .cdtData$EnvData$output$data$lon
                Y0 <- .cdtData$EnvData$output$data$lat
                ## na min.frac
                don <- statisticFunction(don)

                if(change.plot == "Pixels"){
                    nx <- nx_ny_as.image(diff(range(X0)))
                    ny <- nx_ny_as.image(diff(range(Y0)))
                    don <- cdt.as.image(don, nx = nx, ny = ny, pts.xy = cbind(X0, Y0))
                    .cdtData$EnvData$climdata$x <- don$x
                    .cdtData$EnvData$climdata$y <- don$y
                    .cdtData$EnvData$climdata$z <- don$z
                }

                if(change.plot == "Points"){
                    .cdtData$EnvData$climdata$x <- X0
                    .cdtData$EnvData$climdata$y <- Y0
                    .cdtData$EnvData$climdata$z <- don
                }

                .cdtData$EnvData$filetsdata1 <- filetsdata
                .cdtData$EnvData$StatCalc <- .cdtData$EnvData$analysis$method
                if(.cdtData$EnvData$analysis$method == "trend") .cdtData$EnvData$trendUnit <- trendUnit
                .cdtData$EnvData$change.plot.calcClim <- change.plot
                rm(don)
            }
        }else{
            tsdata.path <- file.path(.cdtData$EnvData$PathPicsa, "CDTDATASET", tsdata.dir)
            tsdata.index <- file.path(.cdtData$EnvData$PathPicsa, "CDTDATASET", "CDTDATASET.rds")
            filetsdata <- tsdata.path

            if(!file.exists(filetsdata)){
                Insert.Messages.Out(paste(filetsdata, lang.dlg[['message']][['5']]), TRUE, 'e')
                return(NULL)
            }

            calcClim <- TRUE
            if(!is.null(.cdtData$EnvData$climdata))
                if(!is.null(.cdtData$EnvData$filetsdata1))
                    if(.cdtData$EnvData$filetsdata1 == filetsdata)
                        if(.cdtData$EnvData$StatCalc == .cdtData$EnvData$analysis$method) calcClim <- FALSE

            trendUnit <- trimws(tclvalue(.cdtData$EnvData$trend))
            if(.cdtData$EnvData$analysis$method == "trend")
                if(!is.null(.cdtData$EnvData$trendUnit))
                    if(.cdtData$EnvData$trendUnit != trendUnit & !calcClim) calcClim <- TRUE

            if(.cdtData$EnvData$plotVar$varPICSA == 'dryspell')
                if(!is.null(.cdtData$EnvData$oldDryspell1))
                    if(.cdtData$EnvData$oldDryspell1 != dryspl & !calcClim) calcClim <- TRUE

            if(.cdtData$EnvData$plotVar$varPICSA == 'longdryspell')
                if(!is.null(.cdtData$EnvData$oldDryspell2))
                    if(.cdtData$EnvData$oldDryspell2 != dryspl & !calcClim) calcClim <- TRUE

            if(calcClim){
                index <- readRDS(tsdata.index)

                chunkfile <- sort(unique(index$colInfo$index))
                chunkcalc <- split(chunkfile, ceiling(chunkfile / index$chunkfac))

                EnvData <- .cdtData$EnvData 

                parsL <- doparallel.cond(length(chunkcalc) > 10)
                don <- cdt.foreach(seq_along(chunkcalc), parsL, GUI = TRUE,
                                   progress = TRUE, FUN = function(jj)
                {
                    don <- lapply(chunkcalc[[jj]], function(j){
                        file.rds <- file.path(tsdata.path, paste0(j, ".rds"))
                        readRDS(file.rds)
                    })
                    don <- do.call(cbind, don)

                    if(EnvData$plotVar$varPICSA == 'dryspell'){
                        ndim <- dim(don)
                        nval <- sapply(don, function(x) (length(x) == 1) & is.na(x[1]))
                        don <- sapply(don, function(x) sum(!is.na(x) & x >= dryspl))
                        don[nval] <- NA
                        dim(don) <- ndim
                        rm(nval)
                    }
                    if(EnvData$plotVar$varPICSA == 'longdryspell'){
                        ndim <- dim(don)
                        don <- sapply(don, max, na.rm = TRUE)
                        don[is.infinite(don)] <- NA
                        dim(don) <- ndim
                    }

                    statisticFunction(don, EnvData)
                })

                don <- do.call(c, don)
                don <- don[index$colInfo$order]
                dim(don) <- sapply(index$coords$mat, length)

                .cdtData$EnvData$climdata$x <- index$coords$mat$x
                .cdtData$EnvData$climdata$y <- index$coords$mat$y
                .cdtData$EnvData$climdata$z <- don
                
                rm(don, index)
                if(.cdtData$EnvData$plotVar$varPICSA == 'dryspell') .cdtData$EnvData$oldDryspell1 <- dryspl
                if(.cdtData$EnvData$plotVar$varPICSA == 'longdryspell') .cdtData$EnvData$oldDryspell2 <- dryspl
                if(.cdtData$EnvData$analysis$method == "trend") .cdtData$EnvData$trendUnit <- trendUnit
                .cdtData$EnvData$filetsdata1 <- filetsdata
                .cdtData$EnvData$StatCalc <- .cdtData$EnvData$analysis$method
            }
        }

        return(0)
    }

    #######################################################################################################

    TrendFunction <- function(Y, X){
        ncolY <- ncol(Y)
        nrowY <- nrow(Y)
        X <- if(is.matrix(X)) X else matrix(X, nrow = nrowY, ncol = ncolY)
        ina <- is.na(X) | is.na(Y)
        X[ina] <- NA
        Y[ina] <- NA
        nbY <- colSums(!is.na(Y))
        nbY[nbY < 3] <- NA

        mX <- colMeans(X, na.rm = TRUE)
        mY <- colMeans(Y, na.rm = TRUE)
        vX <- matrixStats::colVars(X, na.rm = TRUE)
        # vY <- matrixStats::colVars(Y, na.rm = TRUE)

        X1 <- X - matrix(mX, nrowY, ncolY, byrow = TRUE)
        Y1 <- Y - matrix(mY, nrowY, ncolY, byrow = TRUE)
        COV <- colSums(X1 * Y1, na.rm = TRUE) / (nbY - 1)
        alpha <- COV / vX
        return(alpha)
    }

    ###################################

    statisticFunction <- function(don, EnvData = .cdtData$EnvData){
        start.dateYear <- as.numeric(format(EnvData$output$start.date, '%Y'))

        if(EnvData$analysis$method == 'mean'){
            don <- colMeans(don, na.rm = TRUE)
        }

        if(EnvData$analysis$method == 'med'){
            don <- matrixStats::colMedians(don, na.rm = TRUE)
        }

        if(EnvData$analysis$method == 'std'){
            don <- matrixStats::colSds(don, na.rm = TRUE)
        }

        if(EnvData$analysis$method == 'trend'){
            tmp <- TrendFunction(don, start.dateYear)
            if(EnvData$analysis$trend == 'trendEY') don <- tmp
            if(EnvData$analysis$trend == 'trendOP'){
                rgYr <- diff(range(start.dateYear, na.rm = TRUE)) + 1
                don <- tmp * rgYr
            }
            if(EnvData$analysis$trend == 'trendAP'){
                rgYr <- diff(range(start.dateYear, na.rm = TRUE)) + 1
                moy <- colMeans(don, na.rm = TRUE)
                don <- 100 * tmp * rgYr / moy
            }
            rm(tmp)
        }

        if(EnvData$analysis$method == 'perc'){
            Q <- EnvData$analysis$mth.perc / 100
            don <- apply(don, 2, quantile8, probs = Q)
        }

        if(EnvData$analysis$method == 'freq'){
            xlow <- EnvData$analysis$low.thres
            xup <- EnvData$analysis$up.thres
            if(EnvData$plotVar$varPICSA %in% c("onset", "cessation")){
                dlo <- try(as.POSIXlt(as.Date(paste(start.dateYear, xlow, sep = '-'))), silent = TRUE)
                dup <- try(as.POSIXlt(as.Date(paste(start.dateYear, xup, sep = '-'))), silent = TRUE)
                if(inherits(dlo, "try-error") | inherits(dup, "try-error")) return(rep(NA, ncol(don)))
                if(is.na(dlo) | is.na(dup)) return(rep(NA, ncol(don)))

                ix <- dlo > dup
                dup$year[ix] <- dup$year[ix] + 1
                xlow <- as.numeric(as.Date(dlo) - EnvData$output$start.date)
                xup <- as.numeric(as.Date(dup) - EnvData$output$start.date)
            }else{
                xlow <- as.numeric(xlow)
                xup <- as.numeric(xup)
                if(is.na(xlow) | is.na(xup)) return(rep(NA, ncol(don)))
            }

            don <- colSums(don >= xlow & don <= xup, na.rm = TRUE)
        }

        return(don)
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
