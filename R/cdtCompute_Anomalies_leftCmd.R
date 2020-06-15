
anomaliesCalcPanelCmd <- function(){
    listOpenFiles <- openFile_ttkcomboList()
    if(WindowsOS()){
        largeur0 <- 29
        largeur1 <- 33
        largeur2 <- 35
        largeur3 <- 25
        largeur4 <- 20
        largeur5 <- 15
        largeur6 <- 18
        largeur7 <- 7
        largeur8 <- 14
        largeur9 <- 10
        largeur10 <- 1
        largeur11 <- 20
    }else{
        largeur0 <- 30
        largeur1 <- 32
        largeur2 <- 33
        largeur3 <- 25
        largeur4 <- 19
        largeur5 <- 16
        largeur6 <- 18
        largeur7 <- 7
        largeur8 <- 14
        largeur9 <- 10
        largeur10 <- 4
        largeur11 <- 20
    }

    date.range <- list(start.year = 1981, start.mon = 1, start.dek = 1,
                       start.pen = 1, start.day = 1,
                       start.hour = 0, start.min = 0,
                       end.year = 2018, end.mon = 12, end.dek = 3,
                       end.pen = 6, end.day = 31,
                       end.hour = 23, end.min = 55)

    GeneralParameters <- list(intstep = "dekadal", outstep = "dekadal", data.type = "cdtstation", 
                              seasonal = list(start.mon = 1, length.mon = 3),
                              cdtstation = list(file = ""),
                              cdtdataset = list(index = ""),
                              date.range = date.range,
                              aggr.series = list(aggr.fun = "sum", opr.fun = ">=", opr.thres = 1,
                                                 min.frac = list(unique = TRUE, all = 0.95, month = rep(0.95, 12))),
                              climato = list(clim.exist = FALSE, clim.file = "",
                                             all.years = TRUE, start.year = 1981, end.year = 2010,
                                             min.year = 20, window = 0),
                              anomaly = "Difference", outdir = list(update = FALSE, dir = ""))

    .cdtData$EnvData$tab$pointSize <- NULL
    .cdtData$EnvData$anomMapOp <- list(presetCol = list(color = 'tim.colors', reverse = FALSE),
                                       userCol = list(custom = FALSE, color = NULL),
                                       userLvl = list(custom = FALSE, levels = NULL, equidist = FALSE),
                                       title = list(user = FALSE, title = ''),
                                       colkeyLab = list(user = FALSE, label = ''),
                                       scalebar = list(add = FALSE, pos = 'bottomleft'),
                                       pointSize = .cdtData$EnvData$tab$pointSize)

    .cdtData$EnvData$TSGraphOp <- list(
                                        anomaly = list(
                                                    anom = NULL,
                                                    xlim = list(is.min = FALSE, min = "1981-1-1", is.max = FALSE, max = "2017-12-31"),
                                                    ylim = list(is.min = FALSE, min = -100, is.max = FALSE, max = 100),
                                                    axislabs = list(is.xlab = FALSE, xlab = '', is.ylab = FALSE, ylab = ''),
                                                    title = list(is.title = FALSE, title = '', position = 'top'),
                                                    colors = list(negative = "blue", positive = "red")
                                                  ),
                                        line = list(
                                                xlim = list(is.min = FALSE, min = "1981-1-1", is.max = FALSE, max = "2017-12-31"),
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

    xml.dlg <- file.path(.cdtDir$dirLocal, "languages", "cdtCompute_Anomalies_leftCmd.xml")
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
        tclvalue(timeSteps) <- CbperiodVAL[periodVAL %in% GeneralParameters$intstep]

        cb.fperiod <- ttkcombobox(frameTimeS, values = CbperiodVAL, textvariable = timeSteps, width = largeur5)

        tkgrid(cb.fperiod, row = 0, column = 0, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)

        helpWidget(cb.fperiod, lang.dlg[['tooltip']][['1']], lang.dlg[['status']][['1']])

        #######################

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
        })

       #############################

        frameSeas <- tkframe(subfr1, relief = 'groove', borderwidth = 2)

        MOIS <- format(ISOdate(2014, 1:12, 1), "%B")
        mois <- format(ISOdate(2014, 1:12, 1), "%b")
        mon <- as.numeric(str_trim(GeneralParameters$seasonal$start.mon))
        len <- as.numeric(str_trim(GeneralParameters$seasonal$length.mon))
        mon1 <- (mon + len - 1) %% 12
        mon1[mon1 == 0] <- 12

        ##############

        frDefSeas <- tkframe(frameSeas)

        start.mon <- tclVar(MOIS[mon])
        length.mon <- tclVar(len)

        stateSeas <- if(GeneralParameters$outstep == 'seasonal') "normal" else "disabled"

        txt.seasS <- tklabel(frDefSeas, text = lang.dlg[['label']][['1b']], anchor = 'e', justify = 'right')
        cb.seasS <- ttkcombobox(frDefSeas, values = MOIS, textvariable = start.mon, width = 11, state = stateSeas)
        txt.seasL <- tklabel(frDefSeas, text = lang.dlg[['label']][['1c']])
        cb.seasL <- ttkcombobox(frDefSeas, values = 2:12, textvariable = length.mon, width = 3, state = stateSeas)

        sepL.seasS <- tklabel(frDefSeas, text = '', width = largeur10)

        tkgrid(txt.seasS, row = 0, column = 0, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1)
        tkgrid(cb.seasS, row = 0, column = 1, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1)
        tkgrid(sepL.seasS, row = 0, column = 2)
        tkgrid(txt.seasL, row = 0, column = 3, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1)
        tkgrid(cb.seasL, row = 0, column = 4, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1)

        helpWidget(cb.seasS, lang.dlg[['tooltip']][['1b']], lang.dlg[['status']][['1b']])
        helpWidget(cb.seasL, lang.dlg[['tooltip']][['1c']], lang.dlg[['status']][['1c']])

        ##############

        tkbind(cb.seasS, "<<ComboboxSelected>>", function(){
            seasdef <- ""
            if(str_trim(tclvalue(outSteps)) == CbOutVAL[6]){
                mon <-  which(MOIS %in% str_trim(tclvalue(start.mon)))
                len <- as.numeric(str_trim(tclvalue(length.mon)))
                mon1 <- (mon + len - 1) %% 12
                mon1[mon1 == 0] <- 12
                seasdef <- paste(MOIS[mon], "->", MOIS[mon1])
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
                seasdef <- paste(MOIS[mon], "->", MOIS[mon1])
            }
            tclvalue(season.def) <- seasdef
        })

        ##############

        frDispSeas <- tkframe(frameSeas)

        seasdef <- paste(MOIS[mon], "->", MOIS[mon1])
        season.def <- tclVar(seasdef)

        txt.SeasD <- tklabel(frDispSeas, text = tclvalue(season.def), textvariable = season.def)

        tkgrid(txt.SeasD, row = 0, column = 0, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1)

        ##############

        tkgrid(frDefSeas, row = 0, column = 0, sticky = 'we')
        tkgrid(frDispSeas, row = 1, column = 0)

        #######################

        frameInData <- ttklabelframe(subfr1, text = lang.dlg[['label']][['2']], relief = 'groove')

        DataType <- tclVar()
        CbdatatypeVAL <- .cdtEnv$tcl$lang$global[['combobox']][['2']][1:2]
        datatypeVAL <- c('cdtstation', 'cdtdataset')
        tclvalue(DataType) <- CbdatatypeVAL[datatypeVAL %in% GeneralParameters$data.type]

        if(GeneralParameters$data.type == 'cdtstation'){
            input.file <- tclVar(GeneralParameters$cdtstation$file)
            txt.INData <- lang.dlg[['label']][['3']]
        }else{
            input.file <- tclVar(GeneralParameters$cdtdataset$index)
            txt.INData <- lang.dlg[['label']][['4']]
        }
        txt.INData.var <- tclVar(txt.INData)

        txt.datatype <- tklabel(frameInData, text = lang.dlg[['label']][['5']], anchor = 'w', justify = 'left')
        cb.datatype <- ttkcombobox(frameInData, values = CbdatatypeVAL, textvariable = DataType, width = largeur0)
        txt.infile <- tklabel(frameInData, text = tclvalue(txt.INData.var), textvariable = txt.INData.var, anchor = 'w', justify = 'left')
        if(GeneralParameters$data.type == 'cdtstation'){
            cb.en.infile <- ttkcombobox(frameInData, values = unlist(listOpenFiles), textvariable = input.file, width = largeur1)
        }else{
            cb.en.infile <- tkentry(frameInData, textvariable = input.file, width = largeur2)
        }
        bt.infile <- tkbutton(frameInData, text = "...")

        tkgrid(txt.datatype, row = 0, column = 0, sticky = 'we', rowspan = 1, columnspan = 2, padx = 1, pady = 1, ipadx = 1, ipady = 1)
        tkgrid(cb.datatype, row = 0, column = 2, sticky = 'we', rowspan = 1, columnspan = 8, padx = 1, pady = 1, ipadx = 1, ipady = 1)
        tkgrid(txt.infile, row = 1, column = 0, sticky = 'we', rowspan = 1, columnspan = 10, padx = 1, pady = 1, ipadx = 1, ipady = 1)
        tkgrid(cb.en.infile, row = 2, column = 0, sticky = 'we', rowspan = 1, columnspan = 9, padx = 0, pady = 1, ipadx = 1, ipady = 1)
        tkgrid(bt.infile, row = 2, column = 9, sticky = 'w', rowspan = 1, columnspan = 1, padx = 0, pady = 1, ipadx = 1, ipady = 1)

        ############
        helpWidget(cb.datatype, lang.dlg[['tooltip']][['2']], lang.dlg[['status']][['2']])

        if(GeneralParameters$data.type == 'cdtstation'){
            helpWidget(cb.en.infile, lang.dlg[['tooltip']][['3']], lang.dlg[['status']][['3']])
            helpWidget(bt.infile, lang.dlg[['tooltip']][['5']], lang.dlg[['status']][['5']])
        }else{
            helpWidget(cb.en.infile, lang.dlg[['tooltip']][['4']], lang.dlg[['status']][['4']])
            helpWidget(bt.infile, lang.dlg[['tooltip']][['6']], lang.dlg[['status']][['6']])
        }
        ############

        tkconfigure(bt.infile, command = function(){
            if(GeneralParameters$data.type == 'cdtstation'){
                dat.opfiles <- getOpenFiles(.cdtEnv$tcl$main$win)
                if(!is.null(dat.opfiles)){
                    update.OpenFiles('ascii', dat.opfiles)
                    listOpenFiles[[length(listOpenFiles) + 1]] <<- dat.opfiles[[1]]
                    tclvalue(input.file) <- dat.opfiles[[1]]
                    tkconfigure(cb.en.infile, values = unlist(listOpenFiles))
                }
            }else{
                path.rds <- tclvalue(tkgetOpenFile(initialdir = getwd(), filetypes = .cdtEnv$tcl$data$filetypes6))
                tclvalue(input.file) <- if(path.rds %in% c("", "NA") | is.na(path.rds)) "" else path.rds
            }
        })

        ############

        tkbind(cb.datatype, "<<ComboboxSelected>>", function(){
            tkdestroy(cb.en.infile)
            tclvalue(input.file) <- ''

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
                helpWidget(bt.infile, lang.dlg[['tooltip']][['5']], lang.dlg[['status']][['5']])
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
                helpWidget(bt.infile, lang.dlg[['tooltip']][['6']], lang.dlg[['status']][['6']])
            }

            tkgrid(cb.en.infile, row = 2, column = 0, sticky = 'we', rowspan = 1, columnspan = 9, padx = 0, pady = 1, ipadx = 1, ipady = 1)
        })

        #######################

        stateAggr <- if(GeneralParameters$intstep == GeneralParameters$outstep) "disabled" else "normal"

        bt.AggrFun <- ttkbutton(subfr1, text = lang.dlg[['button']][['1']], state = stateAggr)

        tkconfigure(bt.AggrFun, command = function(){
            AGGRFUN <- c("sum", "mean", "median", "max", "min", "count")
            GeneralParameters$aggr.series <<- getInfo_AggregateFun(.cdtEnv$tcl$main$win,
                                                                   GeneralParameters$aggr.series,
                                                                   AGGRFUN
                                                                  )
        })

        #######################

        frameDirSav <- tkframe(subfr1, relief = 'groove', borderwidth = 2)

        updateAnom <- tclVar(GeneralParameters$outdir$update)
        outAnom <- tclVar(GeneralParameters$outdir$dir)

        iupAnom <- if(GeneralParameters$outdir$update) '7' else '6'
        txt.upAnom.var <- tclVar(lang.dlg[['label']][[iupAnom]])

        chk.outAnom <- tkcheckbutton(frameDirSav, variable = updateAnom, text = lang.dlg[['checkbutton']][['1']], anchor = 'w', justify = 'left')
        txt.outAnom <- tklabel(frameDirSav, text = tclvalue(txt.upAnom.var), textvariable = txt.upAnom.var, anchor = 'w', justify = 'left')
        en.outAnom <- tkentry(frameDirSav, textvariable = outAnom, width = largeur2)
        bt.outAnom <- tkbutton(frameDirSav, text = "...")

        tkgrid(chk.outAnom, row = 0, column = 0, sticky = 'we', rowspan = 1, columnspan = 5, padx = 1, pady = 0, ipadx = 1, ipady = 1)
        tkgrid(txt.outAnom, row = 1, column = 0, sticky = 'we', rowspan = 1, columnspan = 5, padx = 1, pady = 0, ipadx = 1, ipady = 1)
        tkgrid(en.outAnom, row = 2, column = 0, sticky = 'we', rowspan = 1, columnspan = 4, padx = 0, pady = 0, ipadx = 1, ipady = 1)
        tkgrid(bt.outAnom, row = 2, column = 4, sticky = 'w', rowspan = 1, columnspan = 1, padx = 0, pady = 0, ipadx = 1, ipady = 1)

        helpWidget(chk.outAnom, lang.dlg[['tooltip']][['7']], lang.dlg[['status']][['7']])
        helpWidget(bt.outAnom, lang.dlg[['tooltip']][['6']], lang.dlg[['status']][['6']])
        ihlpUp <- if(GeneralParameters$outdir$update) '8' else '9'
        helpWidget(en.outAnom, lang.dlg[['tooltip']][[ihlpUp]], lang.dlg[['status']][[ihlpUp]])

        ############

        tkconfigure(bt.outAnom, command = function(){
            if(GeneralParameters$outdir$update){
                path.anomIdx <- tclvalue(tkgetOpenFile(initialdir = getwd(), filetypes = .cdtEnv$tcl$data$filetypes6))
                tclvalue(outAnom) <- if(path.anomIdx %in% c("", "NA") | is.na(path.anomIdx)) "" else path.anomIdx
            }else{
                dirAnom <- tk_choose.dir(getwd(), "")
                tclvalue(outAnom) <- if(dirAnom %in% c("", "NA") | is.na(dirAnom)) "" else dirAnom
            }
        })

        tkbind(chk.outAnom, "<Button-1>", function(){
            if(tclvalue(updateAnom) == '0'){
                if(tclvalue(anomDataExist) == '0')
                    tclvalue(txt.upAnom.var) <- lang.dlg[['label']][['7']]

                tkconfigure(bt.outAnom, command = function(){
                    path.anomIdx <- tclvalue(tkgetOpenFile(initialdir = getwd(), initialfile = "", filetypes = .cdtEnv$tcl$data$filetypes6))
                    tclvalue(outAnom) <- if(path.anomIdx %in% c("", "NA") | is.na(path.anomIdx)) "" else path.anomIdx
                })

                helpWidget(en.outAnom, lang.dlg[['tooltip']][['8']], lang.dlg[['status']][['8']])
            }else{
                if(tclvalue(anomDataExist) == '0')
                    tclvalue(txt.upAnom.var) <- lang.dlg[['label']][['6']]

                tkconfigure(bt.outAnom, command = function(){
                    dirAnom <- tk_choose.dir(getwd(), "")
                    tclvalue(outAnom) <- if(dirAnom %in% c("", "NA") | is.na(dirAnom)) "" else dirAnom
                })

                helpWidget(en.outAnom, lang.dlg[['tooltip']][['9']], lang.dlg[['status']][['9']])
            }

            stateClim.Ex <- 'disabled'
            stateClim <- 'disabled'
            stateBaseP <- 'disabled'
            statedayW <- 'disabled'
            stateAnomC <- 'disabled'
            if(tclvalue(updateAnom) == '1'){
                stateClim.Ex <- 'normal'
                if(tclvalue(anomDataExist) == '0')
                    stateClim <- if(tclvalue(climDataExist) == '1') 'normal' else 'disabled'
                stateBaseP <- if(tclvalue(climDataExist) == '1') 'disabled' else 'normal'
                statedayW <- if(str_trim(tclvalue(outSteps)) == CbOutVAL[1] & 
                                tclvalue(climDataExist) == '0') "normal" else "disabled"
                stateAnomC <- 'normal'
            }

            tkconfigure(chk.climIdx, state = stateClim.Ex)
            tkconfigure(en.climIdx, state = stateClim)
            tkconfigure(bt.climIdx, state = stateClim)
            tkconfigure(bt.BasePeriod, state = stateBaseP)
            tkconfigure(en.daywin, state = statedayW)
            tkconfigure(cb.anomaly, state = stateAnomC)
        })

        ############################################

        tkgrid(frameTimeS, row = 0, column = 0, sticky = 'we', padx = 1, pady = 1, ipadx = 1, ipady = 1)
        tkgrid(frameOutS, row = 0, column = 1, sticky = 'we', padx = 1, pady = 1, ipadx = 1, ipady = 1)
        tkgrid(frameSeas, row = 1, column = 0, sticky = '', columnspan = 2, padx = 1, pady = 3, ipadx = 1, ipady = 1)
        tkgrid(frameInData, row = 2, column = 0, sticky = 'we', columnspan = 2, padx = 1, pady = 1, ipadx = 1, ipady = 1)

        tkgrid(bt.AggrFun, row = 3, column = 0, sticky = 'we', columnspan = 2, padx = 1, pady = 3, ipadx = 1, ipady = 1)
        tkgrid(frameDirSav, row = 4, column = 0, sticky = 'we', columnspan = 2, padx = 1, pady = 1, ipadx = 1, ipady = 1)

    #######################################################################################################

    #Tab2
    subfr2 <- bwTabScrollableFrame(cmd.tab2)

        ##############################################

        frameClimato <- ttklabelframe(subfr2, text = lang.dlg[['label']][['8']], relief = 'groove')

        #############################

        frameClim <- tkframe(frameClimato)

        climDataExist <- tclVar(GeneralParameters$climato$clim.exist)
        file.ClimIndex <- tclVar(GeneralParameters$climato$clim.file)

        stateClim.Ex <- 'disabled'
        stateClim <- 'disabled'
       if(!GeneralParameters$outdir$update){
            stateClim.Ex <- 'normal'
            stateClim <- if(GeneralParameters$climato$clim.exist) 'normal' else 'disabled'
        }

        chk.climIdx <- tkcheckbutton(frameClim, variable = climDataExist, text = lang.dlg[['checkbutton']][['2']], anchor = 'w', justify = 'left', state = stateClim.Ex)
        txt.climIdx <- tklabel(frameClim, text = lang.dlg[['label']][['9']], anchor = 'w', justify = 'left')
        en.climIdx <- tkentry(frameClim, textvariable = file.ClimIndex, width = largeur2 + 5, state = stateClim)
        bt.climIdx <- ttkbutton(frameClim, text = .cdtEnv$tcl$lang$global[['button']][['6']], state = stateClim)

        tkgrid(chk.climIdx, row = 0, column = 0, sticky = 'we', rowspan = 1, columnspan = 5, padx = 1, pady = 1, ipadx = 1, ipady = 1)
        tkgrid(txt.climIdx, row = 1, column = 0, sticky = 'we', rowspan = 1, columnspan = 4, padx = 1, pady = 1, ipadx = 1, ipady = 1)
        tkgrid(bt.climIdx, row = 1, column = 4, sticky = 'e', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
        tkgrid(en.climIdx, row = 2, column = 0, sticky = 'we', rowspan = 1, columnspan = 5, padx = 1, pady = 1, ipadx = 1, ipady = 1)

        helpWidget(en.climIdx, lang.dlg[['tooltip']][['11']], lang.dlg[['status']][['11']])
        helpWidget(bt.climIdx, lang.dlg[['tooltip']][['6']], lang.dlg[['status']][['6']])

        ######

        tkconfigure(bt.climIdx, command = function(){
            path.climIdx <- tclvalue(tkgetOpenFile(initialdir = getwd(), filetypes = .cdtEnv$tcl$data$filetypes6))
            tclvalue(file.ClimIndex) <- if(path.climIdx %in% c("", "NA") | is.na(path.climIdx)) "" else path.climIdx
        })

        tkbind(chk.climIdx, "<Button-1>", function(){
            stateClim <- 'disabled'
            stateBaseP <- 'disabled'
            statedayW <- 'disabled'
            if(tclvalue(updateAnom) == '0'){
                if(tclvalue(anomDataExist) == '0')
                    stateClim <- if(tclvalue(climDataExist) == '1') 'disabled' else 'normal'
                stateBaseP <- if(tclvalue(climDataExist) == '1') 'normal' else 'disabled'
                statedayW <- if(str_trim(tclvalue(outSteps)) == CbOutVAL[1] &
                                tclvalue(climDataExist) == '1') 'normal' else 'disabled'
            }

            tkconfigure(en.climIdx, state = stateClim)
            tkconfigure(bt.climIdx, state = stateClim)
            tkconfigure(bt.BasePeriod, state = stateBaseP)
            tkconfigure(en.daywin, state = statedayW)
        })

        #############################

        if(!GeneralParameters$outdir$update){
            stateBaseP <- if(GeneralParameters$climato$clim.exist) 'disabled' else 'normal'
        }else stateBaseP <- 'disabled'

        bt.BasePeriod <- ttkbutton(frameClimato, text = lang.dlg[['button']][['3']], state = stateBaseP)

        helpWidget(bt.BasePeriod, lang.dlg[['tooltip']][['12']], lang.dlg[['status']][['12']])

        ######

        tkconfigure(bt.BasePeriod, command = function(){
            GeneralParameters$climato <<- getInfoBasePeriod(.cdtEnv$tcl$main$win,
                                                            GeneralParameters$climato)
        })

        #############################

        framedayWin <- tkframe(frameClimato)

        dayWin <- tclVar(GeneralParameters$climato$window)

        statedayW <- 'disabled'
        if(!GeneralParameters$outdir$update){
            statedayW <- if(GeneralParameters$outstep == "daily" & 
                            !GeneralParameters$climato$clim.exist) "normal" else "disabled"
        }

        txt.daywin1 <- tklabel(framedayWin, text = lang.dlg[['label']][['10']], anchor = 'e', justify = 'right')
        en.daywin <- tkentry(framedayWin, textvariable = dayWin, width = 3, state = statedayW)
        txt.daywin2 <- tklabel(framedayWin, text = lang.dlg[['label']][['11']], anchor = 'w', justify = 'left')

        tkgrid(txt.daywin1, row = 0, column = 0, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
        tkgrid(en.daywin, row = 0, column = 1, sticky = 'w', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
        tkgrid(txt.daywin2, row = 0, column = 2, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)

        helpWidget(en.daywin, lang.dlg[['tooltip']][['13']], lang.dlg[['status']][['13']])

        #############################

        tkgrid(frameClim, row = 0, column = 0, sticky = 'we', padx = 1, pady = 1, ipadx = 1, ipady = 1)
        tkgrid(bt.BasePeriod, row = 1, column = 0, sticky = 'we', padx = 1, pady = 1, ipadx = 1, ipady = 1)
        tkgrid(framedayWin, row = 2, column = 0, sticky = '', padx = 1, pady = 1, ipadx = 1, ipady = 1)

        #############################

        frameAnom <- tkframe(subfr2, relief = 'groove', borderwidth = 2)

        anomaly <- tclVar()
        CbAnomType <- lang.dlg[['combobox']][['1']]
        AnomType <- c("Difference", "Percentage", "Standardized")
        tclvalue(anomaly) <- CbAnomType[AnomType %in% GeneralParameters$anomaly]

        stateAnomC <- if(!GeneralParameters$outdir$update) 'normal' else 'disabled'

        txt.anomaly <- tklabel(frameAnom, text = lang.dlg[['label']][['12']], anchor = 'e', justify = 'right')
        cb.anomaly <- ttkcombobox(frameAnom, values = CbAnomType, textvariable = anomaly, width = largeur3, state = stateAnomC, justify = 'center')

        tkgrid(txt.anomaly, row = 0, column = 0, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
        tkgrid(cb.anomaly, row = 0, column = 1, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)

        helpWidget(cb.anomaly, lang.dlg[['tooltip']][['14']], lang.dlg[['status']][['14']])

        #############################

        btDateRange <- ttkbutton(subfr2, text = lang.dlg[['button']][['2']])

        tkconfigure(btDateRange, command = function(){
            intstep <- periodVAL[CbperiodVAL %in% str_trim(tclvalue(timeSteps))]
            GeneralParameters[["date.range"]] <<- getInfoDateRange(.cdtEnv$tcl$main$win,
                                                                   GeneralParameters[["date.range"]],
                                                                   intstep)
        })

        helpWidget(btDateRange, lang.dlg[['tooltip']][['10']], lang.dlg[['status']][['10']])

        #############################

        calculateBut <- ttkbutton(subfr2, text = lang.dlg[['button']][['4']])

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

            GeneralParameters$outdir$update <- switch(tclvalue(updateAnom), '0' = FALSE, '1' = TRUE)
            GeneralParameters$outdir$dir <- str_trim(tclvalue(outAnom))

            GeneralParameters$climato$clim.exist <- switch(tclvalue(climDataExist), '0' = FALSE, '1' = TRUE)
            GeneralParameters$climato$clim.file <- str_trim(tclvalue(file.ClimIndex))
            GeneralParameters$climato$window <- if(GeneralParameters$outstep == 'daily') as.numeric(str_trim(tclvalue(dayWin))) else 0

            GeneralParameters$anomaly <- AnomType[CbAnomType %in% str_trim(tclvalue(anomaly))]

            # assign('GeneralParameters', GeneralParameters, envir = .GlobalEnv)

            tkconfigure(.cdtEnv$tcl$main$win, cursor = 'watch')
            tcl('update')
            ret <- tryCatch(
                {
                    anomaliesCalcProcs(GeneralParameters)
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

                    set.anomaly.dates()
                    widgets.Station.Pixel()
                    set.plot.type()
                    res <- try(read.Anomaly.Map(), silent = TRUE)
                    if(inherits(res, "try-error") | is.null(res)) return(NULL)
                }else Insert.Messages.Out(lang.dlg[['message']][['4']], TRUE, 'e')
            }else Insert.Messages.Out(lang.dlg[['message']][['4']], TRUE, 'e')
        })

        ############################################

        tkgrid(frameClimato, row = 0, column = 0, sticky = 'we', padx = 1, pady = 1, ipadx = 1, ipady = 1)
        tkgrid(frameAnom, row = 1, column = 0, sticky = '', padx = 1, pady = 3, ipadx = 1, ipady = 1)
        tkgrid(btDateRange, row = 2, column = 0, sticky = 'we', padx = 1, pady = 3, ipadx = 1, ipady = 1)
        tkgrid(calculateBut, row = 3, column = 0, sticky = 'we', padx = 1, pady = 3, ipadx = 1, ipady = 1)

    #######################################################################################################

    #Tab3
    subfr3 <- bwTabScrollableFrame(cmd.tab3)

        ##############################################

        frameAnomalyDat <- ttklabelframe(subfr3, text = lang.dlg[['label']][['13']], relief = 'groove')

        anomDataExist <- tclVar(0)
        file.AnomIndex <- tclVar()

        stateAnomDat <- if(tclvalue(anomDataExist) == "1") "normal" else "disabled"

        chk.anomIdx <- tkcheckbutton(frameAnomalyDat, variable = anomDataExist, text = lang.dlg[['checkbutton']][['3']], anchor = 'w', justify = 'left')
        en.anomIdx <- tkentry(frameAnomalyDat, textvariable = file.AnomIndex, width = largeur2 + 5, state = stateAnomDat)
        bt.anomIdx <- ttkbutton(frameAnomalyDat, text = .cdtEnv$tcl$lang$global[['button']][['6']], state = stateAnomDat)

        tkgrid(chk.anomIdx, row = 0, column = 0, sticky = 'we', rowspan = 1, columnspan = 4, padx = 1, pady = 1, ipadx = 1, ipady = 1)
        tkgrid(bt.anomIdx, row = 0, column = 4, sticky = 'e', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
        tkgrid(en.anomIdx, row = 1, column = 0, sticky = 'we', rowspan = 1, columnspan = 5, padx = 1, pady = 1, ipadx = 1, ipady = 1)

        ##############

        tkconfigure(bt.anomIdx, command = function(){
            path.Anom <- tclvalue(tkgetOpenFile(initialdir = getwd(), filetypes = .cdtEnv$tcl$data$filetypes6))
            if(path.Anom %in% c("", "NA") | is.na(path.Anom)) return(NULL)
            tclvalue(file.AnomIndex) <- path.Anom

            if(file.exists(str_trim(tclvalue(file.AnomIndex)))){
                OutAnomdata <- try(readRDS(str_trim(tclvalue(file.AnomIndex))), silent = TRUE)
                if(inherits(OutAnomdata, "try-error")){
                    Insert.Messages.Out(lang.dlg[['message']][['5']], TRUE, 'e')
                    Insert.Messages.Out(gsub('[\r\n]', '', OutAnomdata[1]), TRUE, 'e')
                    tkconfigure(cb.anom.Date, values = "")
                    tclvalue(.cdtData$EnvData$anomDate) <- ""
                    return(NULL)
                }

                .cdtData$EnvData$output <- OutAnomdata
                .cdtData$EnvData$PathAnom <- dirname(str_trim(tclvalue(file.AnomIndex)))
                .cdtData$EnvData$plot.maps$data.type <- .cdtData$EnvData$output$params$data.type
                .cdtData$EnvData$plot.maps[c('lon', 'lat', 'id')] <- .cdtData$EnvData$output$data[c('lon', 'lat', 'id')]
                ###################
                set.anomaly.dates()
                widgets.Station.Pixel()
                set.plot.type()
                ret <- try(read.Anomaly.Map(), silent = TRUE)
                if(inherits(ret, "try-error") | is.null(ret)) return(NULL)
            }
        })

        ##############

        tkbind(chk.anomIdx, "<Button-1>", function(){
            stateAnomDat <- if(tclvalue(anomDataExist) == '1') 'disabled' else 'normal'
            tkconfigure(en.anomIdx, state = stateAnomDat)
            tkconfigure(bt.anomIdx, state = stateAnomDat)

            stateCaclBut <- if(tclvalue(anomDataExist) == '1') 'normal' else 'disabled'
            tcl(tknote.cmd, 'itemconfigure', cmd.tab1$IDtab, state = stateCaclBut)
            tcl(tknote.cmd, 'itemconfigure', cmd.tab2$IDtab, state = stateCaclBut)
        })

        ##############################################

        frameAnomalyMap <- ttklabelframe(subfr3, text = lang.dlg[['label']][['14']], relief = 'groove')

        .cdtData$EnvData$anomDate <- tclVar()

        frameNav <- tkframe(frameAnomalyMap)
        cb.anom.Date <- ttkcombobox(frameNav, values = "", textvariable = .cdtData$EnvData$anomDate, width = largeur4, justify = 'center')
        bt.anom.Date.prev <- ttkbutton(frameNav, text = "<<", width = largeur7)
        bt.anom.Date.next <- ttkbutton(frameNav, text = ">>", width = largeur7)

        bt.anom.maps <- ttkbutton(frameAnomalyMap, text = .cdtEnv$tcl$lang$global[['button']][['3']], width = largeur6)
        bt.anom.MapOpt <- ttkbutton(frameAnomalyMap, text = .cdtEnv$tcl$lang$global[['button']][['4']], width = largeur6)

        tkgrid(bt.anom.Date.prev, row = 0, column = 0, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
        tkgrid(cb.anom.Date, row = 0, column = 1, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
        tkgrid(bt.anom.Date.next, row = 0, column = 2, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)

        tkgrid(frameNav, row = 0, column = 0, sticky = '', rowspan = 1, columnspan = 2, pady = 1, ipady = 1)
        tkgrid(bt.anom.MapOpt, row = 1, column = 0, sticky = 'we', rowspan = 1, columnspan = 1, pady = 1, ipady = 1)
        tkgrid(bt.anom.maps, row = 1, column = 1, sticky = 'we', rowspan = 1, columnspan = 1, pady = 1, ipady = 1)

        ##############

        tkconfigure(bt.anom.MapOpt, command = function(){
            if(!is.null(.cdtData$EnvData$anomdata$map)){
                atlevel <- pretty(.cdtData$EnvData$anomdata$map$z, n = 10, min.n = 7)
                if(is.null(.cdtData$EnvData$anomMapOp$userLvl$levels)){
                    .cdtData$EnvData$anomMapOp$userLvl$levels <- atlevel
                }else{
                    if(!.cdtData$EnvData$anomMapOp$userLvl$custom)
                        .cdtData$EnvData$anomMapOp$userLvl$levels <- atlevel
                }
            }
            .cdtData$EnvData$anomMapOp <- MapGraph.MapOptions(.cdtData$EnvData$anomMapOp)

            if(str_trim(tclvalue(.cdtData$EnvData$plot.maps$plot.type)) == "Points")
                .cdtData$EnvData$tab$pointSize <- .cdtData$EnvData$anomMapOp$pointSize
        })

        #########
        .cdtData$EnvData$tab$AnomMap <- NULL

        tkconfigure(bt.anom.maps, command = function(){
            if(str_trim(tclvalue(.cdtData$EnvData$anomDate)) != "" &
                !is.null(.cdtData$EnvData$anomdata))
                    anomaliesCalc.Display.Maps()
        })

        tkconfigure(bt.anom.Date.prev, command = function(){
            if(str_trim(tclvalue(.cdtData$EnvData$anomDate)) != ""){
                if(.cdtData$EnvData$output$params$data.type == "cdtstation")
                    anomDates <- .cdtData$EnvData$output$data$dates
                else anomDates <- .cdtData$EnvData$output$dates

                idaty <- which(anomDates == str_trim(tclvalue(.cdtData$EnvData$anomDate)))
                idaty <- idaty - 1
                if(idaty < 1) idaty <- length(anomDates)
                tclvalue(.cdtData$EnvData$anomDate) <- anomDates[idaty]

                ret <- try(read.Anomaly.Map(), silent = TRUE)
                if(inherits(ret, "try-error") | is.null(ret)) return(NULL)

                anomaliesCalc.Display.Maps()
            }
        })

        tkconfigure(bt.anom.Date.next, command = function(){
            if(str_trim(tclvalue(.cdtData$EnvData$anomDate)) != ""){
                if(.cdtData$EnvData$output$params$data.type == "cdtstation")
                    anomDates <- .cdtData$EnvData$output$data$dates
                else anomDates <- .cdtData$EnvData$output$dates

                idaty <- which(anomDates == str_trim(tclvalue(.cdtData$EnvData$anomDate)))
                idaty <- idaty + 1
                if(idaty > length(anomDates)) idaty <- 1
                tclvalue(.cdtData$EnvData$anomDate) <- anomDates[idaty]

                ret <- try(read.Anomaly.Map(), silent = TRUE)
                if(inherits(ret, "try-error") | is.null(ret)) return(NULL)

                anomaliesCalc.Display.Maps()
            }
        })

        #########
        tkbind(cb.anom.Date, "<<ComboboxSelected>>", function(){
            if(!is.null(.cdtData$EnvData$anomdata)){
                ret <- try(read.Anomaly.Map(), silent = TRUE)
                if(inherits(ret, "try-error") | is.null(ret)) return(NULL)
            }
        })

        ##############################################

        framePlotType <- tkframe(subfr3)

        .cdtData$EnvData$plot.maps$plot.type <- tclVar("Pixels")

        txt.plotType <- tklabel(framePlotType, text = lang.dlg[['label']][['15']], anchor = 'e', justify = 'right')
        cb.plotType <- ttkcombobox(framePlotType, values = "Pixels", textvariable = .cdtData$EnvData$plot.maps$plot.type, justify = 'center', width = largeur8)

        tkgrid(txt.plotType, row = 0, column = 0, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
        tkgrid(cb.plotType, row = 0, column = 1, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)

        ###############

        tkbind(cb.plotType, "<<ComboboxSelected>>", function(){
            if(!is.null(.cdtData$EnvData$anomdata)){
                ret <- try(read.Anomaly.Map(), silent = TRUE)
                if(inherits(ret, "try-error") | is.null(ret)) return(NULL)
            }
        })

        ##############################################

        tkgrid(frameAnomalyDat, row = 0, column = 0, sticky = 'we', padx = 1, pady = 1, ipadx = 1, ipady = 1)
        tkgrid(frameAnomalyMap, row = 1, column = 0, sticky = 'we', padx = 1, pady = 3, ipadx = 1, ipady = 1)
        tkgrid(framePlotType, row = 2, column = 0, sticky = '', padx = 1, pady = 3, ipadx = 1, ipady = 1)

    #######################################################################################################

    #Tab4
    subfr4 <- bwTabScrollableFrame(cmd.tab4)

        ##############################################

        frameAnomalyTS <- ttklabelframe(subfr4, text = lang.dlg[['label']][['16']], relief = 'groove')

        typeTSPLOT <- c("Bar", "Line")
        .cdtData$EnvData$plot.maps$typeTSp <- tclVar("Bar")

        cb.typeTSp <- ttkcombobox(frameAnomalyTS, values = typeTSPLOT, textvariable = .cdtData$EnvData$plot.maps$typeTSp, justify = 'center', width = largeur11)
        bt.TsGraph.plot <- ttkbutton(frameAnomalyTS, text = .cdtEnv$tcl$lang$global[['button']][['3']], width = largeur6)
        bt.TSGraphOpt <- ttkbutton(frameAnomalyTS, text = .cdtEnv$tcl$lang$global[['button']][['4']], width = largeur6)

        tkgrid(cb.typeTSp, row = 0, column = 2, sticky = 'we', rowspan = 1, columnspan = 6, padx = 1, pady = 5, ipadx = 1, ipady = 1)
        tkgrid(bt.TSGraphOpt, row = 1, column = 0, sticky = 'we', rowspan = 1, columnspan = 5, padx = 1, pady = 1, ipadx = 1, ipady = 1)
        tkgrid(bt.TsGraph.plot, row = 1, column = 5, sticky = 'we', rowspan = 1, columnspan = 5, padx = 1, pady = 1, ipadx = 1, ipady = 1)

        #################

        tkconfigure(bt.TSGraphOpt, command = function(){
            typeTSp <- str_trim(tclvalue(.cdtData$EnvData$plot.maps$typeTSp))
            suffix.fun <- switch(typeTSp, "Bar" = "Anomaly", "Line" = "Line")
            plot.fun <- get(paste0("MapGraph.GraphOptions.", suffix.fun), mode = "function")
            .cdtData$EnvData$TSGraphOp <- plot.fun(.cdtData$EnvData$TSGraphOp)
        })

        #########
        .cdtData$EnvData$tab$AnomGraph <- NULL

        tkconfigure(bt.TsGraph.plot, command = function(){
            if(!is.null(.cdtData$EnvData$anomdata)){
                imgContainer <- CDT.Display.Graph(anomaliesCalc.plotAnomGraph, .cdtData$EnvData$tab$AnomGraph, 'Anomaly-Graph')
                .cdtData$EnvData$tab$AnomGraph <- imageNotebookTab_unik(imgContainer, .cdtData$EnvData$tab$AnomGraph)
            }
        })

        ##############################################

        frameSTNCrds <- ttklabelframe(subfr4, text = lang.dlg[['label']][['17']], relief = 'groove')

        frTS2 <- tkframe(frameSTNCrds)
        .cdtData$EnvData$plot.maps$lonLOC <- tclVar()
        .cdtData$EnvData$plot.maps$latLOC <- tclVar()
        .cdtData$EnvData$plot.maps$stnIDTSp <- tclVar()

        tkgrid(frTS2, row = 0, column = 0, sticky = 'e', pady = 1)

        ##############################################

        tkgrid(frameAnomalyTS, row = 0, column = 0, sticky = 'we', pady = 1)
        tkgrid(frameSTNCrds, row = 1, column = 0, sticky = '', pady = 3)

    #######################################################################################################

    #Tab5
    subfr5 <- bwTabScrollableFrame(cmd.tab5)

        ##############################################

        frameSHP <- ttklabelframe(subfr5, text = lang.dlg[['label']][['18']], relief = 'groove')

        .cdtData$EnvData$shp$add.shp <- tclVar(FALSE)
        file.plotShp <- tclVar()
        stateSHP <- "disabled"

        chk.addshp <- tkcheckbutton(frameSHP, variable = .cdtData$EnvData$shp$add.shp, text = lang.dlg[['checkbutton']][['4']], anchor = 'w', justify = 'left')
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

        if(.cdtData$EnvData$output$params$data.type == "cdtstation"){
            stnIDTSPLOT <- .cdtData$EnvData$output$data$id
            txt.stnSel <- tklabel(frTS2, text = lang.dlg[['label']][['19']])
            bt.stnID.prev <- ttkbutton(frTS2, text = "<<", width = largeur7)
            bt.stnID.next <- ttkbutton(frTS2, text = ">>", width = largeur7)
            cb.stnID <- ttkcombobox(frTS2, values = stnIDTSPLOT, textvariable = .cdtData$EnvData$plot.maps$stnIDTSp, justify = 'center', width = largeur4)
            tclvalue(.cdtData$EnvData$plot.maps$stnIDTSp) <- stnIDTSPLOT[1]

            tkgrid(txt.stnSel, row = 0, column = 0, sticky = '', rowspan = 1, columnspan = 3, padx = 1, pady = 1, ipadx = 1, ipady = 1)
            tkgrid(bt.stnID.prev, row = 1, column = 0, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
            tkgrid(cb.stnID, row = 1, column = 1, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
            tkgrid(bt.stnID.next, row = 1, column = 2, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
            tkgrid(cb.stnID, row = 1, column = 1, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)

            ########
            tkconfigure(bt.stnID.prev, command = function(){
                if(!is.null(.cdtData$EnvData$anomdata)){
                    istn <- which(stnIDTSPLOT == str_trim(tclvalue(.cdtData$EnvData$plot.maps$stnIDTSp)))
                    istn <- istn-1
                    if(istn < 1) istn <- length(stnIDTSPLOT)
                    tclvalue(.cdtData$EnvData$plot.maps$stnIDTSp) <- stnIDTSPLOT[istn]

                    imgContainer <- CDT.Display.Graph(anomaliesCalc.plotAnomGraph, .cdtData$EnvData$tab$AnomGraph, 'Anomaly-Graph')
                    .cdtData$EnvData$tab$AnomGraph <- imageNotebookTab_unik(imgContainer, .cdtData$EnvData$tab$AnomGraph)
                }
            })

            tkconfigure(bt.stnID.next, command = function(){
                if(!is.null(.cdtData$EnvData$anomdata)){
                    istn <- which(stnIDTSPLOT == str_trim(tclvalue(.cdtData$EnvData$plot.maps$stnIDTSp)))
                    istn <- istn+1
                    if(istn > length(stnIDTSPLOT)) istn <- 1
                    tclvalue(.cdtData$EnvData$plot.maps$stnIDTSp) <- stnIDTSPLOT[istn]

                    imgContainer <- CDT.Display.Graph(anomaliesCalc.plotAnomGraph, .cdtData$EnvData$tab$AnomGraph, 'Anomaly-Graph')
                    .cdtData$EnvData$tab$AnomGraph <- imageNotebookTab_unik(imgContainer, .cdtData$EnvData$tab$AnomGraph)
                }
            })
        }else{
            txt.crdSel <- tklabel(frTS2, text = lang.dlg[['label']][['20']], anchor = 'w', justify = 'left')
            txt.lonLoc <- tklabel(frTS2, text = lang.dlg[['label']][['21']], anchor = 'e', justify = 'right')
            en.lonLoc <- tkentry(frTS2, textvariable = .cdtData$EnvData$plot.maps$lonLOC, width = largeur9)
            txt.latLoc <- tklabel(frTS2, text = lang.dlg[['label']][['22']], anchor = 'e', justify = 'right')
            en.latLoc <- tkentry(frTS2, textvariable = .cdtData$EnvData$plot.maps$latLOC, width = largeur9)
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
        if(.cdtData$EnvData$output$params$data.type == "cdtstation")
        {
            plot.type <- c("Pixels", "Points")
            .cdtData$EnvData$plot.maps$.data.type <- "Points"

            .cdtData$EnvData$anomMapOp$pointSize <- 1.0
        }else{
            plot.type <- c("Pixels", "FilledContour")
            .cdtData$EnvData$plot.maps$.data.type <- "Grid"
        }
        tkconfigure(cb.plotType, values = plot.type)
    }

    #################

    set.anomaly.dates <- function(){
        if(.cdtData$EnvData$output$params$data.type == "cdtstation")
            anomDates <- .cdtData$EnvData$output$data$dates
        else anomDates <- .cdtData$EnvData$output$dates
        tkconfigure(cb.anom.Date, values = anomDates)
        tclvalue(.cdtData$EnvData$anomDate) <- anomDates[1]
        return(0)
    }

    #######################################################################################################

    read.Anomaly.Map <- function(){
        tkconfigure(.cdtEnv$tcl$main$win, cursor = 'watch')
        tcl('update')
        on.exit({
            tkconfigure(.cdtEnv$tcl$main$win, cursor = '')
            tcl('update')
        })

        if(.cdtData$EnvData$output$params$data.type == "cdtstation"){
            fileAnomdata <- file.path(.cdtData$EnvData$PathAnom, "CDTANOM/CDTANOM.rds")
            if(!file.exists(fileAnomdata)){
                Insert.Messages.Out(paste(fileAnomdata, lang.dlg[['message']][['6']]), TRUE, 'e')
                return(NULL)
            }

            change.plot <- str_trim(tclvalue(.cdtData$EnvData$plot.maps$plot.type))

            ########
            readAnomData <- TRUE
            if(!is.null(.cdtData$EnvData$anomdata))
                if(!is.null(.cdtData$EnvData$fileAnomdata))
                    if(.cdtData$EnvData$fileAnomdata == fileAnomdata) readAnomData <- FALSE

            if(readAnomData){
                .cdtData$EnvData$anomdata$data <- readRDS(fileAnomdata)
                .cdtData$EnvData$fileAnomdata <- fileAnomdata
            }

            ########
            rasterAnomData <- TRUE
            if(!rasterAnomData)
                if(!is.null(.cdtData$EnvData$anomdata$rasterDate))
                    if(.cdtData$EnvData$fileAnomdata == fileAnomdata)
                        if(.cdtData$EnvData$anomdata$rasterDate == str_trim(tclvalue(.cdtData$EnvData$anomDate))) rasterAnomData <- FALSE

            if(!rasterAnomData)
                if(.cdtData$EnvData$change.plot != change.plot) rasterAnomData <- TRUE

            if(rasterAnomData){
                idt <- which(.cdtData$EnvData$output$data$dates == str_trim(tclvalue(.cdtData$EnvData$anomDate)))

                X0 <- .cdtData$EnvData$output$data$lon
                Y0 <- .cdtData$EnvData$output$data$lat
                VAR0 <- as.numeric(.cdtData$EnvData$anomdata$data[idt, ])

                if(change.plot == "Pixels"){
                    nx <- nx_ny_as.image(diff(range(X0)))
                    ny <- nx_ny_as.image(diff(range(Y0)))
                    tmp <- cdt.as.image(VAR0, nx = nx, ny = ny, pts.xy = cbind(X0, Y0))
                    .cdtData$EnvData$anomdata$map$x <- tmp$x
                    .cdtData$EnvData$anomdata$map$y <- tmp$y
                    .cdtData$EnvData$anomdata$map$z <- tmp$z
                    rm(tmp)
                }

                if(change.plot == "Points"){
                    .cdtData$EnvData$anomdata$map$x <- X0
                    .cdtData$EnvData$anomdata$map$y <- Y0
                    .cdtData$EnvData$anomdata$map$z <- VAR0
                }

                .cdtData$EnvData$anomdata$rasterDate <- str_trim(tclvalue(.cdtData$EnvData$anomDate))
                .cdtData$EnvData$change.plot <- change.plot
            }
        }else{
            fileAnomdata <- file.path(.cdtData$EnvData$PathAnom, "DATA_NetCDF/CDTANOM",
                            paste0("anomaly_", str_trim(tclvalue(.cdtData$EnvData$anomDate)), ".nc"))
            if(!file.exists(fileAnomdata)){
                Insert.Messages.Out(paste(fileAnomdata, lang.dlg[['message']][['6']]), TRUE, 'e')
                return(NULL)
            }

            readAnomData <- TRUE
            if(!is.null(.cdtData$EnvData$anomdata))
                if(!is.null(.cdtData$EnvData$fileAnomdata))
                    if(.cdtData$EnvData$fileAnomdata == fileAnomdata) readAnomData <- FALSE

            if(readAnomData){
                nc <- nc_open(fileAnomdata)
                .cdtData$EnvData$anomdata$map$x <- nc$dim[[1]]$vals
                .cdtData$EnvData$anomdata$map$y <- nc$dim[[2]]$vals
                .cdtData$EnvData$anomdata$map$z <- ncvar_get(nc, varid = nc$var[[1]]$name)
                nc_close(nc)
                .cdtData$EnvData$fileAnomdata <- fileAnomdata
            }

            ###################

            fileAnomIdx <- file.path(.cdtData$EnvData$PathAnom, "CDTANOM/CDTANOM.rds")

            readAnomIdx <- TRUE
            if(!is.null(.cdtData$EnvData$cdtdataset))
                if(!is.null(.cdtData$EnvData$fileAnomIdx))
                    if(.cdtData$EnvData$fileAnomIdx == fileAnomIdx) readAnomIdx <- FALSE
            if(readAnomIdx){
                .cdtData$EnvData$cdtdataset <- readRDS(fileAnomIdx)
                .cdtData$EnvData$cdtdataset$fileInfo <- fileAnomIdx
                .cdtData$EnvData$fileAnomIdx <- fileAnomIdx
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

