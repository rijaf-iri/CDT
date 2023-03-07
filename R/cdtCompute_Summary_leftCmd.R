
summariesDataPanelCmd <- function(){
    listOpenFiles <- openFile_ttkcomboList()
    if(WindowsOS()){
        largeur0 <- 26
        largeur1 <- 32
        largeur2 <- 34
        largeur3 <- 20
        largeur4 <- 14
        largeur5 <- 15
        largeur6 <- 19
        largeur7 <- 7
        largeur8 <- 10
    }else{
        largeur0 <- 26
        largeur1 <- 32
        largeur2 <- 33
        largeur3 <- 20
        largeur4 <- 14
        largeur5 <- 15
        largeur6 <- 19
        largeur7 <- 7
        largeur8 <- 10
    }

    ###################

    GeneralParameters <- list(intstep = "dekadal", data.type = "cdtstation", 
                              cdtstation = list(file = ""),
                              cdtdataset = list(index = ""),
                              outdir = "")

    .cdtData$EnvData$GraphOp <- list(
                            boxplot = list(
                                    axislabs = list(is.xlab = FALSE, xlab = '', is.ylab = FALSE, ylab = ''),
                                    title = list(is.title = FALSE, title = ''),
                                    col = list(diff = FALSE, col = 'lightblue', outbg = 'lightblue',
                                               medcol = 'red', whiskcol = 'blue', staplecol = 'blue',
                                               boxcol = 'blue', outcol = 'blue')
                                ),
                            histogram = list(
                                    axislabs = list(is.xlab = FALSE, xlab = '', is.ylab = FALSE, ylab = ''),
                                    title = list(is.title = FALSE, title = ''),
                                    hist = list(user.break = FALSE, breaks = NULL,
                                                col = "lightblue", border = "blue"),
                                    bw = list(add = FALSE, bw = 2.5, col = "red", lwd = 1.5)
                                )
                            )

    .cdtData$EnvData$SHPOp <- list(col = "black", lwd = 1.5)


    ###################

    xml.dlg <- file.path(.cdtDir$dirLocal, "languages", "cdtCompute_Summary_leftCmd.xml")
    lang.dlg <- cdtLanguageParse(xml.dlg, .cdtData$Config$lang.iso)
    .cdtData$EnvData$message <- lang.dlg[['message']]

    ###################

    .cdtEnv$tcl$main$cmd.frame <- tkframe(.cdtEnv$tcl$main$panel.left)

    tknote.cmd <- bwNoteBook(.cdtEnv$tcl$main$cmd.frame)
    cmd.tab1 <- bwAddTab(tknote.cmd, text = lang.dlg[['tab_title']][['1']])
    cmd.tab2 <- bwAddTab(tknote.cmd, text = lang.dlg[['tab_title']][['2']])
    cmd.tab3 <- bwAddTab(tknote.cmd, text = lang.dlg[['tab_title']][['3']])

    bwRaiseTab(tknote.cmd, cmd.tab1)

    tkgrid.columnconfigure(cmd.tab1, 0, weight = 1)
    tkgrid.columnconfigure(cmd.tab2, 0, weight = 1)
    tkgrid.columnconfigure(cmd.tab3, 0, weight = 1)

    tkgrid.rowconfigure(cmd.tab1, 0, weight = 1)
    tkgrid.rowconfigure(cmd.tab2, 0, weight = 1)
    tkgrid.rowconfigure(cmd.tab3, 0, weight = 1)

    #######################################################################################################

    #Tab1
    subfr1 <- bwTabScrollableFrame(cmd.tab1)

        #######################

        frameTimeS <- ttklabelframe(subfr1, text = lang.dlg[['label']][['1']], relief = 'groove')

        timeSteps <- tclVar()
        CbperiodVAL <- .cdtEnv$tcl$lang$global[['combobox']][['1']][3:6]
        periodVAL <- c('daily', 'pentad', 'dekadal', 'monthly')
        tclvalue(timeSteps) <- CbperiodVAL[periodVAL %in% GeneralParameters$intstep]

        cb.fperiod <- ttkcombobox(frameTimeS, values = CbperiodVAL, textvariable = timeSteps, width = largeur0)

        tkgrid(cb.fperiod, row = 0, column = 0, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)

        helpWidget(cb.fperiod, lang.dlg[['tooltip']][['1']], lang.dlg[['status']][['1']])

        #######################

        frameInData <- ttklabelframe(subfr1, text = lang.dlg[['label']][['2']], relief = 'groove')

        DataType <- tclVar()
        CbdatatypeVAL <- .cdtEnv$tcl$lang$global[['combobox']][['2']][1:2]
        datatypeVAL <- c('cdtstation', 'cdtdataset')
        tclvalue(DataType) <- CbdatatypeVAL[datatypeVAL %in% GeneralParameters$data.type]

        if(GeneralParameters$data.type == 'cdtstation'){
            input.file <- tclVar(GeneralParameters$cdtstation$file)
            txt.INData <- lang.dlg[['label']][['4']]
            stateSetNC <- "disabled"
        }else{
            input.file <- tclVar(GeneralParameters$cdtdataset$index)
            txt.INData <- lang.dlg[['label']][['5']]
            stateSetNC <- "disabled"
        }
        txt.INData.var <- tclVar(txt.INData)

        txt.datatype <- tklabel(frameInData, text = lang.dlg[['label']][['3']], anchor = 'w', justify = 'left')
        cb.datatype <- ttkcombobox(frameInData, values = CbdatatypeVAL, textvariable = DataType, width = largeur0)

        txt.infile <- tklabel(frameInData, text = tclvalue(txt.INData.var), textvariable = txt.INData.var, anchor = 'w', justify = 'left')

        if(GeneralParameters$data.type == 'cdtstation'){
            cb.en.infile <- ttkcombobox(frameInData, values = unlist(listOpenFiles), textvariable = input.file, width = largeur1)
        }else{
            cb.en.infile <- tkentry(frameInData, textvariable = input.file, width = largeur2)
        }
        bt.infile <- tkbutton(frameInData, text = "...")

        ############

        tkgrid(txt.datatype, row = 0, column = 0, sticky = 'we', rowspan = 1, columnspan = 2, padx = 1, pady = 1, ipadx = 1, ipady = 1)
        tkgrid(cb.datatype, row = 0, column = 2, sticky = 'we', rowspan = 1, columnspan = 8, padx = 1, pady = 1, ipadx = 1, ipady = 1)
        tkgrid(txt.infile, row = 1, column = 0, sticky = 'we', rowspan = 1, columnspan = 8, padx = 1, pady = 1, ipadx = 1, ipady = 1)
        tkgrid(cb.en.infile, row = 2, column = 0, sticky = 'we', rowspan = 1, columnspan = 9, padx = 0, pady = 1, ipadx = 1, ipady = 1)
        tkgrid(bt.infile, row = 2, column = 9, sticky = 'w', rowspan = 1, columnspan = 1, padx = 0, pady = 1, ipadx = 1, ipady = 1)

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
                    lapply(list(cb.en.infile, cb.addshp), tkconfigure, values = unlist(listOpenFiles))
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

            data.type <- datatypeVAL[CbdatatypeVAL %in% str_trim(tclvalue(DataType))]
            ###
            if(data.type == 'cdtstation'){
                tclvalue(txt.INData.var) <- lang.dlg[['label']][['4']]

                cb.en.infile <- ttkcombobox(frameInData, values = unlist(listOpenFiles), textvariable = input.file, width = largeur1)

                tkconfigure(bt.infile, command = function(){
                    dat.opfiles <- getOpenFiles(.cdtEnv$tcl$main$win)
                    if(!is.null(dat.opfiles)){
                        update.OpenFiles('ascii', dat.opfiles)
                        listOpenFiles[[length(listOpenFiles) + 1]] <<- dat.opfiles[[1]]
                        tclvalue(input.file) <- dat.opfiles[[1]]
                        lapply(list(cb.en.infile, cb.addshp), tkconfigure, values = unlist(listOpenFiles))
                    }
                })

                helpWidget(cb.en.infile, lang.dlg[['tooltip']][['3']], lang.dlg[['status']][['3']])
                helpWidget(bt.infile, lang.dlg[['tooltip']][['5']], lang.dlg[['status']][['5']])
            }

            ###
            if(data.type == 'cdtdataset'){
                tclvalue(txt.INData.var) <- lang.dlg[['label']][['5']]

                cb.en.infile <- tkentry(frameInData, textvariable = input.file, width = largeur2)

                tkconfigure(bt.infile, command = function(){
                    path.rds <- tclvalue(tkgetOpenFile(initialdir = getwd(), filetypes = .cdtEnv$tcl$data$filetypes6))
                    tclvalue(input.file) <- if(path.rds %in% c("", "NA") | is.na(path.rds)) "" else path.rds
                })

                helpWidget(cb.en.infile, lang.dlg[['tooltip']][['4']], lang.dlg[['status']][['4']])
                helpWidget(bt.infile, lang.dlg[['tooltip']][['6']], lang.dlg[['status']][['6']])
            }

            tkgrid(cb.en.infile, row = 2, column = 0, sticky = 'we', rowspan = 1, columnspan = 9, padx = 0, pady = 1, ipadx = 1, ipady = 1)
        })

        #############################

        frameDirSav <- tkframe(subfr1, relief = 'groove', borderwidth = 2)

        outAnom <- tclVar(GeneralParameters$outdir)

        txt.outAnom <- tklabel(frameDirSav, text = lang.dlg[['label']][['6']], anchor = 'w', justify = 'left')
        en.outAnom <- tkentry(frameDirSav, textvariable = outAnom, width = largeur2)
        bt.outAnom <- tkbutton(frameDirSav, text = "...")

        tkgrid(txt.outAnom, row = 0, column = 0, sticky = 'we', rowspan = 1, columnspan = 5, padx = 1, pady = 0, ipadx = 1, ipady = 1)
        tkgrid(en.outAnom, row = 1, column = 0, sticky = 'we', rowspan = 1, columnspan = 4, padx = 0, pady = 0, ipadx = 1, ipady = 1)
        tkgrid(bt.outAnom, row = 1, column = 4, sticky = 'w', rowspan = 1, columnspan = 1, padx = 0, pady = 0, ipadx = 1, ipady = 1)

        helpWidget(en.outAnom, lang.dlg[['tooltip']][['7']], lang.dlg[['status']][['7']])
        helpWidget(bt.outAnom, lang.dlg[['tooltip']][['6']], lang.dlg[['status']][['6']])

        ######

        tkconfigure(bt.outAnom, command = function(){
            dirAnom <- tk_choose.dir(getwd(), "")
            tclvalue(outAnom) <- if(dirAnom %in% c("", "NA") | is.na(dirAnom)) "" else dirAnom
        })

        ############################################

        summaryBut <- ttkbutton(subfr1, text = lang.dlg[['button']][['1']])

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

            Insert.Messages.Out(lang.dlg[['message']][['1']], TRUE, "i")

            tkconfigure(.cdtEnv$tcl$main$win, cursor = 'watch')
            tcl('update')
            ret <- tryCatch(
                {
                    summarizeDataProcs(GeneralParameters)
                },
                warning = function(w){
                    warningFun(w)
                    return(0)
                },
                error = function(e) errorFun(e),
                finally = {
                    tkconfigure(.cdtEnv$tcl$main$win, cursor = '')
                    tcl('update')
                }
            )

            if(!is.null(ret)){
                if(ret == 0){
                    Insert.Messages.Out(lang.dlg[['message']][['2']], TRUE, "s")

                    .cdtData$EnvData$plot.maps$data.type <- .cdtData$EnvData$output$params$data.type
                    .cdtData$EnvData$plot.maps[c('lon', 'lat', 'id')] <- .cdtData$EnvData$output$data[c('lon', 'lat', 'id')]
                    ###################

                    widgets.Station.Pixel()
                }else Insert.Messages.Out(lang.dlg[['message']][['3']], TRUE, 'e')
            }else Insert.Messages.Out(lang.dlg[['message']][['3']], TRUE, 'e')
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

        frameSumData <- ttklabelframe(subfr2, text = lang.dlg[['label']][['7']], relief = 'groove')

        DirExist <- tclVar(0)
        file.Stat <- tclVar()

        .cdtData$EnvData$plot.maps$lonLOC <- tclVar()
        .cdtData$EnvData$plot.maps$latLOC <- tclVar()
        .cdtData$EnvData$plot.maps$stnIDTSp <- tclVar()

        statedirStat <- if(tclvalue(DirExist) == "1") "normal" else "disabled"

        chk.dirStat <- tkcheckbutton(frameSumData, variable = DirExist, text = lang.dlg[['checkbutton']][['1']], anchor = 'w', justify = 'left')
        en.dirStat <- tkentry(frameSumData, textvariable = file.Stat, width = largeur2 + 5, state = statedirStat)
        bt.dirStat <- ttkbutton(frameSumData, text = .cdtEnv$tcl$lang$global[['button']][['6']], state = statedirStat)

        bt.plotMap <- ttkbutton(frameSumData, text = lang.dlg[['button']][['2']])
        frameSTNPX <- tkframe(frameSumData)

        tkgrid(chk.dirStat, row = 0, column = 0, sticky = 'we', rowspan = 1, columnspan = 4, padx = 1, pady = 1, ipadx = 1, ipady = 1)
        tkgrid(bt.dirStat, row = 0, column = 4, sticky = 'e', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
        tkgrid(en.dirStat, row = 1, column = 0, sticky = 'we', rowspan = 1, columnspan = 5, padx = 1, pady = 1, ipadx = 1, ipady = 1)

        tkgrid(bt.plotMap, row = 2, column = 0, sticky = 'we', rowspan = 1, columnspan = 5, padx = 0, pady = 1, ipadx = 1, ipady = 1)
        tkgrid(frameSTNPX, row = 3, column = 0, sticky = 'we', rowspan = 1, columnspan = 5, padx = 0, pady = 1, ipadx = 1, ipady = 1)

        #######################

        tkconfigure(bt.dirStat, command = function(){
            path.Stat <- tclvalue(tkgetOpenFile(initialdir = getwd(), filetypes = .cdtEnv$tcl$data$filetypes6))
            if(path.Stat %in% c("", "NA") | is.na(path.Stat)) return(NULL)
            tclvalue(file.Stat) <- path.Stat

            if(file.exists(str_trim(tclvalue(file.Stat)))){
                OutSummary <- try(readRDS(str_trim(tclvalue(file.Stat))), silent = TRUE)
                if(inherits(OutSummary, "try-error")){
                    Insert.Messages.Out(lang.dlg[['message']][['4']], TRUE, 'e')
                    Insert.Messages.Out(gsub('[\r\n]', '', OutSummary[1]), TRUE, 'e')
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

        ###############
        tkbind(chk.dirStat, "<Button-1>", function(){
            statedirStat <- if(tclvalue(DirExist) == '1') 'disabled' else 'normal'
            tkconfigure(en.dirStat, state = statedirStat)
            tkconfigure(bt.dirStat, state = statedirStat)

            stateSumBut <- if(tclvalue(DirExist) == '1') 'normal' else 'disabled'
            tcl(tknote.cmd, 'itemconfigure', cmd.tab1$IDtab, state = stateSumBut)
        })

        ###############

        .cdtData$EnvData$tab$pMap <- NULL

        tkconfigure(bt.plotMap, command = function(){
            .cdtData$EnvData$plot.maps$plotType <- PLOT_TYPE[cbPLOT_TYPE %in% str_trim(tclvalue(plot.type))]
            if(!is.null(.cdtData$EnvData$output)) SummaryData.Display.Map()
        })

        ##############################################

        frameSumGraph <- ttklabelframe(subfr2, text = lang.dlg[['label']][['8']], relief = 'groove')

        cbPLOT_TYPE <- lang.dlg[['combobox']][['1']]
        PLOT_TYPE <- c('boxplot', 'histogram')
        plot.type <- tclVar(cbPLOT_TYPE[1])

        cbMOIS <- c(format(ISOdate(2014, 1:12, 1), "%B"), lang.dlg[['label']][['9']])
        mois <- c(format(ISOdate(2014, 1:12, 1), "%b"), "all")
        plotMois <- tclVar(cbMOIS[13])

        cb.SumGraph.Type <- ttkcombobox(frameSumGraph, values = cbPLOT_TYPE, textvariable = plot.type, justify = 'center', width = largeur3)
        bt.SumGraph.Plot <- ttkbutton(frameSumGraph, text = .cdtEnv$tcl$lang$global[['button']][['3']], width = largeur5)
        cb.SumGraph.Mois <- ttkcombobox(frameSumGraph, values = cbMOIS, textvariable = plotMois, justify = 'center', width = largeur4, state = "disabled")
        bt.SumGraph.Opt <- ttkbutton(frameSumGraph, text = .cdtEnv$tcl$lang$global[['button']][['4']], width = largeur5)

        tkgrid(cb.SumGraph.Type, row = 0, column = 0, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
        tkgrid(bt.SumGraph.Plot, row = 0, column = 1, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
        tkgrid(cb.SumGraph.Mois, row = 1, column = 0, sticky = 'e', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
        tkgrid(bt.SumGraph.Opt, row = 1, column = 1, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)

        ###################

        .cdtData$EnvData$tab$TGraph <- NULL

        tkconfigure(bt.SumGraph.Plot, command = function(){
            .cdtData$EnvData$plot.maps$plotType <- PLOT_TYPE[cbPLOT_TYPE %in% str_trim(tclvalue(plot.type))]
            .cdtData$EnvData$plot.maps$plotMois <- mois[cbMOIS %in% str_trim(tclvalue(plotMois))]

            if(!is.null(.cdtData$EnvData$output)){
                imgContainer <- CDT.Display.Graph(SummaryData.Plot.Graph, .cdtData$EnvData$tab$TGraph, 'Summary - Plot')
                .cdtData$EnvData$tab$TGraph <- imageNotebookTab_unik(imgContainer, .cdtData$EnvData$tab$TGraph)
            }
        })

        tkconfigure(bt.SumGraph.Opt, command = function(){
            plotType <- PLOT_TYPE[cbPLOT_TYPE %in% str_trim(tclvalue(plot.type))]
            plot.fun <- get(paste0("Summary.GraphOptions.", str_to_title(plotType)), mode = "function")
            .cdtData$EnvData$GraphOp <- plot.fun(.cdtData$EnvData$GraphOp)
        })

        ###################

        tkbind(cb.SumGraph.Type, "<<ComboboxSelected>>", function(){
            .cdtData$EnvData$plot.maps$plotType <- PLOT_TYPE[cbPLOT_TYPE %in% str_trim(tclvalue(plot.type))]

            stateMois <- if(.cdtData$EnvData$plot.maps$plotType == 'boxplot') "disabled" else "normal"
            tkconfigure(cb.SumGraph.Mois, state = stateMois)
        })

        tkbind(cb.SumGraph.Mois, "<<ComboboxSelected>>", function(){
            .cdtData$EnvData$plot.maps$plotMois <- mois[cbMOIS %in% str_trim(tclvalue(plotMois))]
        })

        ##############################################

        bt.SumTable <- ttkbutton(subfr2, text = lang.dlg[['button']][['3']])

        .cdtData$EnvData$tab$Table <- NULL

        tkconfigure(bt.SumTable, command = function(){
            if(!is.null(.cdtData$EnvData$output)){
                summary.df <- SummaryData.Get.Table()
                summary.df[is.na(summary.df)] <- ""
                .cdtData$EnvData$tab$Table <- tableNotebookTab_unik(summary.df, .cdtData$EnvData$tab$Table, "Summary Table", 10)
            }
        })

        ##############################################

        tkgrid(frameSumData, row = 0, column = 0, sticky = 'we', padx = 1, pady = 1, ipadx = 1, ipady = 1)
        tkgrid(frameSumGraph, row = 1, column = 0, sticky = 'we', padx = 1, pady = 3, ipadx = 1, ipady = 1)
        tkgrid(bt.SumTable, row = 2, column = 0, sticky = 'we', padx = 1, pady = 3, ipadx = 0, ipady = 1)

    #######################################################################################################

    #Tab3
    subfr3 <- bwTabScrollableFrame(cmd.tab3)

        ##############################################

        frameSHP <- ttklabelframe(subfr3, text = lang.dlg[['label']][['10']], relief = 'groove')

        .cdtData$EnvData$shp$add.shp <- tclVar(0)
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
                lapply(list(cb.en.infile, cb.addshp), tkconfigure, values = unlist(listOpenFiles))

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
        tkdestroy(frameSTNPX)
        frameSTNPX <<- tkframe(frameSumData)

        if(.cdtData$EnvData$output$params$data.type == "cdtstation"){
            stnIDTSPLOT <- .cdtData$EnvData$output$data$id
            txt.stnSel <- tklabel(frameSTNPX, text = lang.dlg[['label']][['11']])
            bt.stnID.prev <- ttkbutton(frameSTNPX, text = "<<", width = largeur7)
            bt.stnID.next <- ttkbutton(frameSTNPX, text = ">>", width = largeur7)
            cb.stnID <- ttkcombobox(frameSTNPX, values = stnIDTSPLOT, textvariable = .cdtData$EnvData$plot.maps$stnIDTSp, justify = 'center', width = largeur6)
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
            txt.crdSel <- tklabel(frameSTNPX, text = lang.dlg[['label']][['12']], anchor = 'w', justify = 'left')
            txt.lonLoc <- tklabel(frameSTNPX, text = lang.dlg[['label']][['13']], anchor = 'e', justify = 'right')
            en.lonLoc <- tkentry(frameSTNPX, textvariable = .cdtData$EnvData$plot.maps$lonLOC, width = largeur8)
            txt.latLoc <- tklabel(frameSTNPX, text = lang.dlg[['label']][['14']], anchor = 'e', justify = 'right')
            en.latLoc <- tkentry(frameSTNPX, textvariable = .cdtData$EnvData$plot.maps$latLOC, width = largeur8)
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
