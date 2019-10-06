
summariesDataPanelCmd <- function(){
    listOpenFiles <- openFile_ttkcomboList()
    if(WindowsOS()){
        largeur0 <- .cdtEnv$tcl$fun$w.widgets(22)
        largeur1 <- .cdtEnv$tcl$fun$w.widgets(31)
        largeur2 <- .cdtEnv$tcl$fun$w.widgets(33)
        largeur3 <- 20
        largeur4 <- 29
        largeur5 <- 22
        largeur6 <- 22
    }else{
        largeur0 <- .cdtEnv$tcl$fun$w.widgets(18)
        largeur1 <- .cdtEnv$tcl$fun$w.widgets(22)
        largeur2 <- .cdtEnv$tcl$fun$w.widgets(23)
        largeur3 <- 15
        largeur4 <- 22
        largeur5 <- 14
        largeur6 <- 14
    }

    GeneralParameters <- list(intstep = "dekadal", data.type = "cdtstation", 
                            cdtstation = list(file = ""),
                            cdtdataset = list(index = ""),
                            outdir = "")

    # xml.dlg <- file.path(.cdtDir$dirLocal, "languages", "cdtCompute_Summary_leftCmd.xml")
    # lang.dlg <- cdtLanguageParse(xml.dlg, .cdtData$Config$lang.iso)
    # .cdtData$EnvData$message <- lang.dlg[['message']]

    ###################

    .cdtEnv$tcl$main$cmd.frame <- tkframe(.cdtEnv$tcl$main$panel.left)

    tknote.cmd <- bwNoteBook(.cdtEnv$tcl$main$cmd.frame)
    cmd.tab1 <- bwAddTab(tknote.cmd, text = "Input")
    cmd.tab2 <- bwAddTab(tknote.cmd, text = "Table & Graph")

    bwRaiseTab(tknote.cmd, cmd.tab1)
    tkgrid.columnconfigure(cmd.tab1, 0, weight = 1)
    tkgrid.columnconfigure(cmd.tab2, 0, weight = 1)

    tkgrid.rowconfigure(cmd.tab1, 0, weight = 1)
    tkgrid.rowconfigure(cmd.tab2, 0, weight = 1)

    #######################################################################################################

    #Tab1
    subfr1 <- bwTabScrollableFrame(cmd.tab1)

        #######################

        frameTimeS <- ttklabelframe(subfr1, text = "Time step of input data", relief = 'groove')

        timeSteps <- tclVar()
        CbperiodVAL <- .cdtEnv$tcl$lang$global[['combobox']][['1']][3:6]
        periodVAL <- c('daily', 'pentad', 'dekadal', 'monthly')
        tclvalue(timeSteps) <- CbperiodVAL[periodVAL %in% GeneralParameters$intstep]

        cb.fperiod <- ttkcombobox(frameTimeS, values = CbperiodVAL, textvariable = timeSteps, width = largeur1)

        tkgrid(cb.fperiod, row = 0, column = 0, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)

        infobulle(cb.fperiod, 'Select the time step of the data')
        status.bar.display(cb.fperiod, 'Select the time step of the data')

        #######################

        frameInData <- ttklabelframe(subfr1, text = "Input Data", relief = 'groove')

        DataType <- tclVar()
        CbdatatypeVAL <- .cdtEnv$tcl$lang$global[['combobox']][['2']][1:2]
        datatypeVAL <- c('cdtstation', 'cdtdataset')
        tclvalue(DataType) <- CbdatatypeVAL[datatypeVAL %in% GeneralParameters$data.type]

        if(GeneralParameters$data.type == 'cdtstation'){
            input.file <- tclVar(GeneralParameters$cdtstation$file)
            txt.INData <- 'File containing stations data'
            stateSetNC <- "disabled"
        }else{
            input.file <- tclVar(GeneralParameters$cdtdataset$index)
            txt.INData <- 'Index file (*.rds) of the dataset'
            stateSetNC <- "disabled"
        }
        txt.INData.var <- tclVar(txt.INData)

        txt.datatype <- tklabel(frameInData, text = "Format", anchor = 'w', justify = 'left')
        cb.datatype <- ttkcombobox(frameInData, values = CbdatatypeVAL, textvariable = DataType, width = largeur0)

        txt.infile <- tklabel(frameInData, text = tclvalue(txt.INData.var), textvariable = txt.INData.var, anchor = 'w', justify = 'left')

        if(GeneralParameters$data.type == 'cdtstation'){
            cb.en.infile <- ttkcombobox(frameInData, values = unlist(listOpenFiles), textvariable = input.file, width = largeur1)
        }else{
            cb.en.infile <- tkentry(frameInData, textvariable = input.file, width = largeur2)
        }
        bt.infile <- tkbutton(frameInData, text = "...")

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

        tkgrid(txt.datatype, row = 0, column = 0, sticky = 'we', rowspan = 1, columnspan = 2, padx = 1, pady = 1, ipadx = 1, ipady = 1)
        tkgrid(cb.datatype, row = 0, column = 2, sticky = 'we', rowspan = 1, columnspan = 8, padx = 1, pady = 1, ipadx = 1, ipady = 1)
        tkgrid(txt.infile, row = 1, column = 0, sticky = 'we', rowspan = 1, columnspan = 8, padx = 1, pady = 1, ipadx = 1, ipady = 1)
        tkgrid(cb.en.infile, row = 2, column = 0, sticky = 'we', rowspan = 1, columnspan = 9, padx = 0, pady = 1, ipadx = 1, ipady = 1)
        tkgrid(bt.infile, row = 2, column = 9, sticky = 'w', rowspan = 1, columnspan = 1, padx = 0, pady = 1, ipadx = 1, ipady = 1)

        ############
        infobulle(cb.datatype, 'Select the format of the input data')
        status.bar.display(cb.datatype, 'Select the format of the input data')

        if(GeneralParameters$data.type == 'cdtstation'){
            infobulle(cb.en.infile, 'Select the file containing the input data')
            status.bar.display(cb.en.infile, 'Select the file containing the input data')
            infobulle(bt.infile, 'Browse file if not listed')
            status.bar.display(bt.infile, 'Browse file if not listed')
        }else{
            infobulle(cb.en.infile, 'Enter the full path to the file <dataset name>.rds')
            status.bar.display(cb.en.infile, 'Enter the full path to the file <dataset name>.rds')
            infobulle(bt.infile, 'or browse here')
            status.bar.display(bt.infile, 'or browse here')
        }

        ############

        tkbind(cb.datatype, "<<ComboboxSelected>>", function(){
            tkdestroy(cb.en.infile)
            tclvalue(input.file) <- ''

            ###
            if(str_trim(tclvalue(DataType)) == CbdatatypeVAL[1]){
                tclvalue(txt.INData.var) <- 'File containing stations data'

                cb.en.infile <- ttkcombobox(frameInData, values = unlist(listOpenFiles), textvariable = input.file, width = largeur1)

                tkconfigure(bt.infile, command = function(){
                    dat.opfiles <- getOpenFiles(.cdtEnv$tcl$main$win)
                    if(!is.null(dat.opfiles)){
                        update.OpenFiles('ascii', dat.opfiles)
                        listOpenFiles[[length(listOpenFiles) + 1]] <<- dat.opfiles[[1]]
                        tclvalue(input.file) <- dat.opfiles[[1]]
                        tkconfigure(cb.en.infile, values = unlist(listOpenFiles))
                    }
                })

                infobulle(cb.en.infile, 'Select the file containing the input data')
                status.bar.display(cb.en.infile, 'Select the file containing the input data')
                infobulle(bt.infile, 'Browse file if not listed')
                status.bar.display(bt.infile, 'Browse file if not listed')
            }

            ###
            if(str_trim(tclvalue(DataType)) == CbdatatypeVAL[2]){
                tclvalue(txt.INData.var) <- 'Index file (*.rds) of the dataset'

                cb.en.infile <- tkentry(frameInData, textvariable = input.file, width = largeur2)

                tkconfigure(bt.infile, command = function(){
                    path.rds <- tclvalue(tkgetOpenFile(initialdir = getwd(), filetypes = .cdtEnv$tcl$data$filetypes6))
                    tclvalue(input.file) <- if(path.rds %in% c("", "NA") | is.na(path.rds)) "" else path.rds
                })

                infobulle(cb.en.infile, 'Enter the full path to the file <dataset name>.rds')
                status.bar.display(cb.en.infile, 'Enter the full path to the file <dataset name>.rds')
                infobulle(bt.infile, 'or browse here')
                status.bar.display(bt.infile, 'or browse here')
            }

            tkgrid(cb.en.infile, row = 2, column = 0, sticky = 'we', rowspan = 1, columnspan = 9, padx = 0, pady = 1, ipadx = 1, ipady = 1)
        })

        #############################

        frameDirSav <- tkframe(subfr1, relief = 'groove', borderwidth = 2)

        outAnom <- tclVar(GeneralParameters$outdir)

        txt.outAnom <- tklabel(frameDirSav, text = "Directory to save the outputs", anchor = 'w', justify = 'left')
        en.outAnom <- tkentry(frameDirSav, textvariable = outAnom, width = largeur2)
        bt.outAnom <- tkbutton(frameDirSav, text = "...")

        ######

        tkconfigure(bt.outAnom, command = function(){
            dirAnom <- tk_choose.dir(getwd(), "")
            tclvalue(outAnom) <- if(dirAnom %in% c("", "NA") | is.na(dirAnom)) "" else dirAnom
        })

        ######
        tkgrid(txt.outAnom, row = 0, column = 0, sticky = 'we', rowspan = 1, columnspan = 5, padx = 1, pady = 0, ipadx = 1, ipady = 1)
        tkgrid(en.outAnom, row = 1, column = 0, sticky = 'we', rowspan = 1, columnspan = 4, padx = 0, pady = 0, ipadx = 1, ipady = 1)
        tkgrid(bt.outAnom, row = 1, column = 4, sticky = 'w', rowspan = 1, columnspan = 1, padx = 0, pady = 0, ipadx = 1, ipady = 1)

        infobulle(en.outAnom, 'Enter the full path to directory to save outputs')
        status.bar.display(en.outAnom, 'Enter the full path to directory to save outputs')
        infobulle(bt.outAnom, 'or browse here')
        status.bar.display(bt.outAnom, 'or browse here')

        ############################################

        if(!is.null(.cdtData$EnvData$DirExist)){
            stateSumBut <- if(tclvalue(.cdtData$EnvData$DirExist) == "1") "normal" else "disabled"
        }else stateSumBut <- "normal"

        summaryBut <- ttkbutton(subfr1, text = "Summarize", state = stateSumBut)

        #################

        tkconfigure(summaryBut, command = function(){
            GeneralParameters$data.type <- datatypeVAL[CbdatatypeVAL %in% str_trim(tclvalue(DataType))]
            GeneralParameters$intstep <- periodVAL[CbperiodVAL %in% str_trim(tclvalue(timeSteps))]

            if(str_trim(tclvalue(DataType)) == CbdatatypeVAL[1])
                GeneralParameters$cdtstation$file <- str_trim(tclvalue(input.file))
            if(str_trim(tclvalue(DataType)) == CbdatatypeVAL[2])
                GeneralParameters$cdtdataset$index <- str_trim(tclvalue(input.file))

            GeneralParameters$outdir <- str_trim(tclvalue(outAnom))

            # assign("GeneralParameters", GeneralParameters, envir = .GlobalEnv)

            Insert.Messages.Out("Summarizing data ......", TRUE, "i")

            tkconfigure(.cdtEnv$tcl$main$win, cursor = 'watch')
            tcl('update')
            ret <- tryCatch(
                {
                    summarizeDataProcs(GeneralParameters)
                },
                warning = function(w) warningFun(w),
                error = function(e) errorFun(e),
                finally = {
                    tkconfigure(.cdtEnv$tcl$main$win, cursor = '')
                    tcl('update')
                }
            )

            msg0 <- "Summarizing finished successfully"
            msg1 <- "Summarizing failed"

            if(!is.null(ret)){
                if(ret == 0){
                    Insert.Messages.Out(msg0, TRUE, "s")

                    .cdtData$EnvData$plot.maps$data.type <- .cdtData$EnvData$output$params$data.type
                    .cdtData$EnvData$plot.maps[c('lon', 'lat', 'id')] <- .cdtData$EnvData$output$data[c('lon', 'lat', 'id')]
                    ###################

                    widgets.Station.Pixel()
                }else Insert.Messages.Out(msg1, format = TRUE)
            }else Insert.Messages.Out(msg1, format = TRUE)
        })

        ############################################

        tkgrid(frameTimeS, row = 0, column = 0, sticky = '', padx = 1, pady = 1, ipadx = 1, ipady = 1)
        tkgrid(frameInData, row = 1, column = 0, sticky = 'we', padx = 1, pady = 3, ipadx = 1, ipady = 1)
        tkgrid(frameDirSav, row = 2, column = 0, sticky = 'we', padx = 1, pady = 3, ipadx = 1, ipady = 1)
        tkgrid(summaryBut, row = 3, column = 0, sticky = 'we', padx = 1, pady = 3, ipadx = 1, ipady = 1)

    #######################################################################################################

    #Tab2
    subfr2 <- bwTabScrollableFrame(cmd.tab2)

        ##############################################

        frameSumData <- ttklabelframe(subfr2, text = "Summaries data", relief = 'groove')

        .cdtData$EnvData$DirExist <- tclVar(0)
        file.Stat <- tclVar()

        statedirStat <- if(tclvalue(.cdtData$EnvData$DirExist) == "1") "normal" else "disabled"

        chk.dirStat <- tkcheckbutton(frameSumData, variable = .cdtData$EnvData$DirExist, text = "Data already summarized", anchor = 'w', justify = 'left')
        en.dirStat <- tkentry(frameSumData, textvariable = file.Stat, width = largeur2, state = statedirStat)
        bt.dirStat <- tkbutton(frameSumData, text = "...", state = statedirStat)
        bt.plotMap <- ttkbutton(frameSumData, text = "Plot map to select a pixel or station")

        #######################

        frameSTNPX <- tkframe(frameSumData)
        .cdtData$EnvData$plot.maps$lonLOC <- tclVar()
        .cdtData$EnvData$plot.maps$latLOC <- tclVar()
        .cdtData$EnvData$plot.maps$stnIDTSp <- tclVar()

        #######################

        tkconfigure(bt.dirStat, command = function(){
            path.Stat <- tclvalue(tkgetOpenFile(initialdir = getwd(), filetypes = .cdtEnv$tcl$data$filetypes6))
            if(path.Stat %in% c("", "NA") | is.na(path.Stat)) return(NULL)
            tclvalue(file.Stat) <- path.Stat

            if(file.exists(str_trim(tclvalue(file.Stat)))){
                OutSummary <- try(readRDS(str_trim(tclvalue(file.Stat))), silent = TRUE)
                if(inherits(OutSummary, "try-error")){
                    Insert.Messages.Out('Unable to load Summary data', format = TRUE)
                    Insert.Messages.Out(gsub('[\r\n]', '', OutSummary[1]), format = TRUE)
                    return(NULL)
                }

                .cdtData$EnvData$output <- OutSummary
                .cdtData$EnvData$PathSum <- dirname(str_trim(tclvalue(file.Stat)))
                .cdtData$EnvData$plot.maps$data.type <- .cdtData$EnvData$output$params$data.type
                .cdtData$EnvData$plot.maps[c('lon', 'lat', 'id')] <- .cdtData$EnvData$output$data[c('lon', 'lat', 'id')]
                ###################

                widgets.Station.Pixel()
            }
        })

        .cdtData$EnvData$tab$pMap <- NULL
        tkconfigure(bt.plotMap, command = function(){
            if(!is.null(.cdtData$EnvData$output))
                SummaryData.Display.Map()
        })

        ###############

        tkgrid(chk.dirStat, row = 0, column = 0, sticky = 'we', rowspan = 1, columnspan = 5, padx = 1, pady = 1, ipadx = 1, ipady = 1)
        tkgrid(en.dirStat, row = 1, column = 0, sticky = 'we', rowspan = 1, columnspan = 4, padx = 1, pady = 1, ipadx = 1, ipady = 1)
        tkgrid(bt.dirStat, row = 1, column = 4, sticky = 'w', rowspan = 1, columnspan = 1, padx = 0, pady = 1, ipadx = 1, ipady = 1)
        tkgrid(bt.plotMap, row = 2, column = 0, sticky = '', rowspan = 1, columnspan = 5, padx = 1, pady = 1, ipadx = 1, ipady = 1)
        tkgrid(frameSTNPX, row = 3, column = 0, sticky = '', rowspan = 1, columnspan = 5, padx = 1, pady = 1, ipadx = 1, ipady = 1)

        ###############
        tkbind(chk.dirStat, "<Button-1>", function(){
            statedirStat <- if(tclvalue(.cdtData$EnvData$DirExist) == '1') 'disabled' else 'normal'
            tkconfigure(en.dirStat, state = statedirStat)
            tkconfigure(bt.dirStat, state = statedirStat)
            stateSumBut <- if(tclvalue(.cdtData$EnvData$DirExist) == '1') 'normal' else 'disabled'
            tkconfigure(summaryBut, state = stateSumBut)
            tkconfigure(cb.fperiod, state = stateSumBut)
            tkconfigure(cb.datatype, state = stateSumBut)
            tkconfigure(cb.en.infile, state = stateSumBut)
            tkconfigure(bt.infile, state = stateSumBut)
            tkconfigure(en.outAnom, state = stateSumBut)
            tkconfigure(bt.outAnom, state = stateSumBut)
        })

        ##############################################

        frameSumGraph <- ttklabelframe(subfr2, text = "Summary Graph", relief = 'groove')

        mois <- c(format(ISOdate(2014, 1:12, 1), "%b"), "ALL")
        PLOT_TYPE <- c("Boxplot", "Histogram")
        stateMois <- "disabled"

        .cdtData$EnvData$plot.maps$plotType <- tclVar(PLOT_TYPE[1])
        .cdtData$EnvData$plot.maps$plotMois <- tclVar(mois[1])

        cb.SumGraph.Type <- ttkcombobox(frameSumGraph, values = PLOT_TYPE, textvariable = .cdtData$EnvData$plot.maps$plotType, width = largeur4)
        bt.SumGraph.Plot <- ttkbutton(frameSumGraph, text = .cdtEnv$tcl$lang$global[['button']][['3']])
        cb.SumGraph.Mois <- ttkcombobox(frameSumGraph, values = mois, textvariable = .cdtData$EnvData$plot.maps$plotMois, width = largeur5, state = stateMois)
        bt.SumGraph.Opt <- ttkbutton(frameSumGraph, text = .cdtEnv$tcl$lang$global[['button']][['4']], state = 'disabled')

        .cdtData$EnvData$tab$TGraph <- NULL

        tkconfigure(bt.SumGraph.Plot, command = function(){
            if(!is.null(.cdtData$EnvData$output)){
                imgContainer <- CDT.Display.Graph(SummaryData.Plot.Graph, .cdtData$EnvData$tab$TGraph, 'Summary - Plot')
                .cdtData$EnvData$tab$TGraph <- imageNotebookTab_unik(imgContainer, .cdtData$EnvData$tab$TGraph)
            }
        })

        ###################
        tkgrid(cb.SumGraph.Type, row = 0, column = 0, sticky = 'we', rowspan = 1, columnspan = 4, padx = 1, pady = 1, ipadx = 1, ipady = 1)
        tkgrid(bt.SumGraph.Plot, row = 0, column = 4, sticky = '', rowspan = 1, columnspan = 2, padx = 1, pady = 1, ipadx = 1, ipady = 1)
        tkgrid(cb.SumGraph.Mois, row = 1, column = 1, sticky = 'we', rowspan = 1, columnspan = 2, padx = 1, pady = 1, ipadx = 1, ipady = 1)
        tkgrid(bt.SumGraph.Opt, row = 1, column = 4, sticky = 'we', rowspan = 1, columnspan = 2, padx = 1, pady = 1, ipadx = 1, ipady = 1)

        ###################

        tkbind(cb.SumGraph.Type, "<<ComboboxSelected>>", function(){
            plotType <- str_trim(tclvalue(.cdtData$EnvData$plot.maps$plotType))
            stateMois <- if(plotType == PLOT_TYPE[1]) "disabled" else "normal"
            tkconfigure(cb.SumGraph.Mois, state = stateMois)
        })

        ##############################################

        bt.SumTable <- ttkbutton(subfr2, text = "Display Summary Table")

        .cdtData$EnvData$tab$Table <- NULL

        tkconfigure(bt.SumTable, command = function(){
            if(!is.null(.cdtData$EnvData$output)){
                summary.df <- SummaryData.Get.Table()
                summary.df[is.na(summary.df)] <- ""
                .cdtData$EnvData$tab$Table <- tableNotebookTab_unik(summary.df, .cdtData$EnvData$tab$Table, "Summary Table", 10)
            }
        })

        ##############################################

        frameSHP <- ttklabelframe(subfr2, text = "Boundaries", relief = 'groove')

        .cdtData$EnvData$shp$add.shp <- tclVar(0)
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
                tkconfigure(cb.addshp, values = unlist(listOpenFiles))

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

        tkgrid(frameSumData, row = 0, column = 0, sticky = 'we', padx = 1, pady = 1, ipadx = 1, ipady = 1)
        tkgrid(frameSumGraph, row = 1, column = 0, sticky = 'we', padx = 1, pady = 3, ipadx = 1, ipady = 1)
        tkgrid(bt.SumTable, row = 2, column = 0, sticky = 'we', padx = 1, pady = 3, ipadx = 1, ipady = 1)
        tkgrid(frameSHP, row = 3, column = 0, sticky = 'we', padx = 1, pady = 1, ipadx = 1, ipady = 1)

    #######################################################################################################

    widgets.Station.Pixel <- function(){
        tkdestroy(frameSTNPX)
        frameSTNPX <<- tkframe(frameSumData)

        if(.cdtData$EnvData$output$params$data.type == "cdtstation"){
            stnIDTSPLOT <- .cdtData$EnvData$output$data$id
            txt.stnSel <- tklabel(frameSTNPX, text = "Select a station to plot")
            bt.stnID.prev <- ttkbutton(frameSTNPX, text = "<<", width = 6)
            bt.stnID.next <- ttkbutton(frameSTNPX, text = ">>", width = 6)
            cb.stnID <- ttkcombobox(frameSTNPX, values = stnIDTSPLOT, textvariable = .cdtData$EnvData$plot.maps$stnIDTSp, width = largeur6)
            tclvalue(.cdtData$EnvData$plot.maps$stnIDTSp) <- stnIDTSPLOT[1]

            tkconfigure(bt.stnID.prev, command = function(){
                if(!is.null(.cdtData$EnvData$output)){
                    istn <- which(stnIDTSPLOT == str_trim(tclvalue(.cdtData$EnvData$plot.maps$stnIDTSp)))
                    istn <- istn - 1
                    if(istn < 1) istn <- length(stnIDTSPLOT)
                    tclvalue(.cdtData$EnvData$plot.maps$stnIDTSp) <- stnIDTSPLOT[istn]

                    imgContainer <- CDT.Display.Graph(SummaryData.Plot.Graph, .cdtData$EnvData$tab$TGraph, 'Summary - Plot')
                    .cdtData$EnvData$tab$TGraph <- imageNotebookTab_unik(imgContainer, .cdtData$EnvData$tab$TGraph)
                }
            })

            tkconfigure(bt.stnID.next, command = function(){
                if(!is.null(.cdtData$EnvData$output)){
                    istn <- which(stnIDTSPLOT == str_trim(tclvalue(.cdtData$EnvData$plot.maps$stnIDTSp)))
                    istn <- istn + 1
                    if(istn > length(stnIDTSPLOT)) istn <- 1
                    tclvalue(.cdtData$EnvData$plot.maps$stnIDTSp) <- stnIDTSPLOT[istn]

                    imgContainer <- CDT.Display.Graph(SummaryData.Plot.Graph, .cdtData$EnvData$tab$TGraph, 'Summary - Plot')
                    .cdtData$EnvData$tab$TGraph <- imageNotebookTab_unik(imgContainer, .cdtData$EnvData$tab$TGraph)
                }
            })

            tkgrid(txt.stnSel, row = 0, column = 0, sticky = '', rowspan = 1, columnspan = 3, padx = 1, pady = 1, ipadx = 1, ipady = 1)
            tkgrid(bt.stnID.prev, row = 1, column = 0, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
            tkgrid(cb.stnID, row = 1, column = 1, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
            tkgrid(bt.stnID.next, row = 1, column = 2, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
        }else{
            txt.crdSel <- tklabel(frameSTNPX, text = "Enter longitude and latitude to plot", anchor = 'w', justify = 'left')
            txt.lonLoc <- tklabel(frameSTNPX, text = "Longitude", anchor = 'e', justify = 'right')
            en.lonLoc <- tkentry(frameSTNPX, textvariable = .cdtData$EnvData$plot.maps$lonLOC, width = 8)
            txt.latLoc <- tklabel(frameSTNPX, text = "Latitude", anchor = 'e', justify = 'right')
            en.latLoc <- tkentry(frameSTNPX, textvariable = .cdtData$EnvData$plot.maps$latLOC, width = 8)
            stnIDTSPLOT <- ""
            tclvalue(.cdtData$EnvData$plot.maps$stnIDTSp) <- ""

            tkgrid(txt.crdSel, row = 0, column = 0, sticky = 'we', rowspan = 1, columnspan = 4, padx = 1, pady = 1, ipadx = 1, ipady = 1)
            tkgrid(txt.lonLoc, row = 1, column = 0, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
            tkgrid(en.lonLoc, row = 1, column = 1, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
            tkgrid(txt.latLoc, row = 1, column = 2, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
            tkgrid(en.latLoc, row = 1, column = 3, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
        }
        tkgrid(frameSTNPX, row = 3, column = 0, sticky = 'e', rowspan = 1, columnspan = 5, padx = 1, pady = 1, ipadx = 1, ipady = 1)
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
