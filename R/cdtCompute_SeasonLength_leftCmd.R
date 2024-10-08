
SeasonLengthCalcPanelCmd <- function(){
    if(WindowsOS()){
        largeur0 <- 36
        largeur1 <- 33
        largeur2 <- 14
        largeur3 <- 20
        largeur4 <- 7
        largeur5 <- 19
        largeur6 <- 20
        largeur7 <- 10
    }else{
        largeur0 <- 33
        largeur1 <- 32
        largeur2 <- 14
        largeur3 <- 19
        largeur4 <- 7
        largeur5 <- 18
        largeur6 <- 20
        largeur7 <- 10
    }

    GeneralParameters <- list(onset = "", cessation = "", output = "")

    .cdtData$EnvData$dataMapOp <- list(presetCol = list(color = 'tim.colors', reverse = FALSE),
                                       userCol = list(custom = FALSE, color = NULL),
                                       userLvl = list(custom = FALSE, levels = NULL, equidist = FALSE),
                                       title = list(user = FALSE, title = ''),
                                       colkeyLab = list(user = FALSE, label = ''),
                                       scalebar = list(add = FALSE, pos = 'bottomleft'),
                                       plotType = list(values = c("Pixels", "Points"), var = "Pixels"),
                                       pointSize = 1.0, bbox = .cdtData$Config$region)

    .cdtData$EnvData$TSGraphOp <- list(
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
                                            legend = NULL)
                                    )

    ###################

    xml.dlg <- file.path(.cdtDir$dirLocal, "languages", "cdtCompute_SeasonLength_leftCmd.xml")
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

        tkgrid(txt.Ons, row = 0, column = 0, sticky = 'we', rowspan = 1, columnspan = 10, padx = 1, pady = 1, ipadx = 1, ipady = 1)
        tkgrid(en.Ons, row = 1, column = 0, sticky = 'we', rowspan = 1, columnspan = 9, padx = 0, pady = 1, ipadx = 1, ipady = 1)
        tkgrid(bt.Ons, row = 1, column = 9, sticky = 'w', rowspan = 1, columnspan = 1, padx = 0, pady = 1, ipadx = 1, ipady = 1)
        tkgrid(txt.Ces, row = 2, column = 0, sticky = 'we', rowspan = 1, columnspan = 10, padx = 1, pady = 1, ipadx = 1, ipady = 1)
        tkgrid(en.Ces, row = 3, column = 0, sticky = 'we', rowspan = 1, columnspan = 9, padx = 0, pady = 1, ipadx = 1, ipady = 1)
        tkgrid(bt.Ces, row = 3, column = 9, sticky = 'w', rowspan = 1, columnspan = 1, padx = 0, pady = 1, ipadx = 1, ipady = 1)

        helpWidget(en.Ons, lang.dlg[['tooltip']][['1']], lang.dlg[['status']][['1']])
        helpWidget(en.Ces, lang.dlg[['tooltip']][['2']], lang.dlg[['status']][['2']])
        helpWidget(bt.Ons, lang.dlg[['tooltip']][['3']], lang.dlg[['status']][['3']])
        helpWidget(bt.Ces, lang.dlg[['tooltip']][['3']], lang.dlg[['status']][['3']])

        ############

        tkconfigure(bt.Ons, command = function(){
            path.rds <- tclvalue(tkgetOpenFile(initialdir = getwd(), filetypes = .cdtEnv$tcl$data$filetypes6))
            tclvalue(input.Onset) <- if(path.rds %in% c("", "NA") | is.na(path.rds)) "" else path.rds
        })

        tkconfigure(bt.Ces, command = function(){
            path.rds <- tclvalue(tkgetOpenFile(initialdir = getwd(), filetypes = .cdtEnv$tcl$data$filetypes6))
            tclvalue(input.Cessation) <- if(path.rds %in% c("", "NA") | is.na(path.rds)) "" else path.rds
        })

        ############################################

        frameDirSav <- tkframe(subfr1, relief = 'groove', borderwidth = 2)

        dir.save <- tclVar(GeneralParameters$output)

        txt.dir.save <- tklabel(frameDirSav, text = lang.dlg[['label']][['4']], anchor = 'w', justify = 'left')
        en.dir.save <- tkentry(frameDirSav, textvariable = dir.save, width = largeur0)
        bt.dir.save <- tkbutton(frameDirSav, text = "...")

        ######
        tkgrid(txt.dir.save, row = 0, column = 0, sticky = 'we', rowspan = 1, columnspan = 5, padx = 1, pady = 0, ipadx = 1, ipady = 1)
        tkgrid(en.dir.save, row = 1, column = 0, sticky = 'we', rowspan = 1, columnspan = 4, padx = 0, pady = 0, ipadx = 1, ipady = 1)
        tkgrid(bt.dir.save, row = 1, column = 4, sticky = 'w', rowspan = 1, columnspan = 1, padx = 0, pady = 0, ipadx = 1, ipady = 1)

        helpWidget(en.dir.save, lang.dlg[['tooltip']][['4']], lang.dlg[['status']][['4']])
        helpWidget(bt.dir.save, lang.dlg[['tooltip']][['3']], lang.dlg[['status']][['3']])

        ######
        tkconfigure(bt.dir.save, command = function() fileORdir2Save(dir.save, isFile = FALSE))

        ############################################

        bt.CalcOnset <- ttkbutton(subfr1, text = lang.dlg[['button']][['1']])

        tkconfigure(bt.CalcOnset, command = function(){
            GeneralParameters$onset <- trimws(tclvalue(input.Onset))
            GeneralParameters$cessation <- trimws(tclvalue(input.Cessation))
            GeneralParameters$output <- trimws(tclvalue(dir.save))

            # assign('GeneralParameters', GeneralParameters, envir = .GlobalEnv)

            Insert.Messages.Out(lang.dlg[['message']][['1']], TRUE, "i")

            tkconfigure(.cdtEnv$tcl$main$win, cursor = 'watch')
            tcl('update')
            ret <- tryCatch2({
                                compute_SeasonLength_Procs(GeneralParameters)
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
                    set.plot.type(.cdtData$EnvData$output$params$data.type)
                    ###################

                    set.Data.Dates()
                    widgets.Station.Pixel()
                    res <- try(read.Data.Map(), silent = TRUE)
                    if(inherits(res, "try-error") | is.null(res)) return(NULL)
                }else Insert.Messages.Out(lang.dlg[['message']][['3']], TRUE, 'e')
            }else Insert.Messages.Out(lang.dlg[['message']][['3']], TRUE, 'e')
        })

        ############################################

        tkgrid(frameInData, row = 0, column = 0, sticky = 'we', padx = 1, pady = 3, ipadx = 1, ipady = 1)
        tkgrid(frameDirSav, row = 1, column = 0, sticky = 'we', padx = 1, pady = 3, ipadx = 1, ipady = 1)
        tkgrid(bt.CalcOnset, row = 2, column = 0, sticky = 'we', padx = 1, pady = 3, ipadx = 1, ipady = 1)

    #######################################################################################################

    #Tab2
    subfr2 <- bwTabScrollableFrame(cmd.tab2)

        ##############################################

        frameDataExist <- ttklabelframe(subfr2, text = lang.dlg[['label']][['5']], relief = 'groove')

        DirExist <- tclVar(0)
        file.dataIndex <- tclVar()

        stateExistData <- if(tclvalue(DirExist) == "1") "normal" else "disabled"

        chk.dataIdx <- tkcheckbutton(frameDataExist, variable = DirExist, text = lang.dlg[['checkbutton']][['1']], anchor = 'w', justify = 'left')
        en.dataIdx <- tkentry(frameDataExist, textvariable = file.dataIndex, width = largeur0 + 5, state = stateExistData)
        bt.dataIdx <- ttkbutton(frameDataExist, text = .cdtEnv$tcl$lang$global[['button']][['6']], state = stateExistData)

        tkgrid(chk.dataIdx, row = 0, column = 0, sticky = 'we', rowspan = 1, columnspan = 4, padx = 1, pady = 1, ipadx = 1, ipady = 1)
        tkgrid(bt.dataIdx, row = 0, column = 4, sticky = 'e', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
        tkgrid(en.dataIdx, row = 1, column = 0, sticky = 'we', rowspan = 1, columnspan = 5, padx = 1, pady = 1, ipadx = 1, ipady = 1)

        #############

        tkconfigure(bt.dataIdx, command = function(){
            path.dataIdx <- tclvalue(tkgetOpenFile(initialdir = getwd(), filetypes = .cdtEnv$tcl$data$filetypes6))
            if(path.dataIdx %in% c("", "NA") | is.na(path.dataIdx)) return(NULL)
            tclvalue(file.dataIndex) <- path.dataIdx

            if(file.exists(trimws(tclvalue(file.dataIndex)))){
                OutIndexdata <- try(readRDS(trimws(tclvalue(file.dataIndex))), silent = TRUE)
                if(inherits(OutIndexdata, "try-error")){
                    Insert.Messages.Out(lang.dlg[['message']][['4']], TRUE, 'e')
                    Insert.Messages.Out(gsub('[\r\n]', '', OutIndexdata[1]), TRUE, 'e')
                    tkconfigure(cb.data.Index, values = "")
                    tclvalue(.cdtData$EnvData$donDate) <- ""
                    return(NULL)
                }

                .cdtData$EnvData$output <- OutIndexdata
                .cdtData$EnvData$PathData <- dirname(trimws(tclvalue(file.dataIndex)))
                .cdtData$EnvData$plot.maps[c('lon', 'lat', 'id')] <- .cdtData$EnvData$output$data[c('lon', 'lat', 'id')]
                set.plot.type(.cdtData$EnvData$output$params$data.type)
                ###################

                set.Data.Dates()
                widgets.Station.Pixel()
                ret <- try(read.Data.Map(), silent = TRUE)
                if(inherits(ret, "try-error") | is.null(ret)) return(NULL)
            }
        })

        ###############
        tkbind(chk.dataIdx, "<Button-1>", function(){
            stateExistData <- if(tclvalue(DirExist) == '1') 'disabled' else 'normal'
            tkconfigure(en.dataIdx, state = stateExistData)
            tkconfigure(bt.dataIdx, state = stateExistData)

            stateCaclBut <- if(tclvalue(DirExist) == '1') 'normal' else 'disabled'
            tcl(tknote.cmd, 'itemconfigure', cmd.tab1$IDtab, state = stateCaclBut)
        })

        ##############################################

        frameDataMap <- ttklabelframe(subfr2, text = lang.dlg[['label']][['6']], relief = 'groove')

        .cdtData$EnvData$donDate <- tclVar()

        cb.data.Index <- ttkcombobox(frameDataMap, values = "", textvariable = .cdtData$EnvData$donDate, width = largeur3, justify = 'center')
        bt.data.Index.prev <- ttkbutton(frameDataMap, text = "<<", width = largeur4)
        bt.data.Index.next <- ttkbutton(frameDataMap, text = ">>", width = largeur4)
        bt.data.maps <- ttkbutton(frameDataMap, text = .cdtEnv$tcl$lang$global[['button']][['3']], width = largeur5)
        bt.data.MapOpt <- ttkbutton(frameDataMap, text = .cdtEnv$tcl$lang$global[['button']][['4']], width = largeur5)

        ###############

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

        #########
        .cdtData$EnvData$tab$dataMap <- NULL

        tkconfigure(bt.data.maps, command = function(){
            if(trimws(tclvalue(.cdtData$EnvData$donDate)) != ""){
                ret <- try(read.Data.Map(), silent = TRUE)
                if(inherits(ret, "try-error") | is.null(ret)) return(NULL)

                SeasonLengthCalc.Display.Maps()
            }
        })

        tkconfigure(bt.data.Index.prev, command = function(){
            if(trimws(tclvalue(.cdtData$EnvData$donDate)) != ""){
                donDates <- format(.cdtData$EnvData$output$start.date, "%Y")
                idaty <- which(donDates == trimws(tclvalue(.cdtData$EnvData$donDate)))
                idaty <- idaty - 1
                if(idaty < 1) idaty <- length(donDates)
                tclvalue(.cdtData$EnvData$donDate) <- donDates[idaty]

                ret <- try(read.Data.Map(), silent = TRUE)
                if(inherits(ret, "try-error") | is.null(ret)) return(NULL)

                SeasonLengthCalc.Display.Maps()
            }
        })

        tkconfigure(bt.data.Index.next, command = function(){
            if(trimws(tclvalue(.cdtData$EnvData$donDate)) != ""){
                donDates <- format(.cdtData$EnvData$output$start.date, "%Y")
                idaty <- which(donDates == trimws(tclvalue(.cdtData$EnvData$donDate)))
                idaty <- idaty + 1
                if(idaty > length(donDates)) idaty <- 1
                tclvalue(.cdtData$EnvData$donDate) <- donDates[idaty]

                ret <- try(read.Data.Map(), silent = TRUE)
                if(inherits(ret, "try-error") | is.null(ret)) return(NULL)

                SeasonLengthCalc.Display.Maps()
            }
        })

        ###############

        tkbind(cb.data.Index, "<<ComboboxSelected>>", function(){
            if(!is.null(.cdtData$EnvData$varData)){
                ret <- try(read.Data.Map(), silent = TRUE)
                if(inherits(ret, "try-error") | is.null(ret)) return(NULL)
            }
        })

        ##############################################

        tkgrid(frameDataExist, row = 0, column = 0, sticky = 'we', padx = 1, pady = 1, ipadx = 1, ipady = 1)
        tkgrid(frameDataMap, row = 1, column = 0, sticky = 'we', padx = 1, pady = 3, ipadx = 1, ipady = 1)

    #######################################################################################################

    #Tab3
    subfr3 <- bwTabScrollableFrame(cmd.tab3)

        ##############################################

        frameDataTS <- ttklabelframe(subfr3, text = lang.dlg[['label']][['8']], relief = 'groove')

        typeTSPLOT <- c("Line", "Barplot")
        .cdtData$EnvData$plot.maps$typeTSp <- tclVar("Line")

        cb.typeTSp <- ttkcombobox(frameDataTS, values = typeTSPLOT, textvariable = .cdtData$EnvData$plot.maps$typeTSp, justify = 'center', width = largeur6)
        bt.TsGraph.plot <- ttkbutton(frameDataTS, text = .cdtEnv$tcl$lang$global[['button']][['3']], width = largeur5)
        bt.TSGraphOpt <- ttkbutton(frameDataTS, text = .cdtEnv$tcl$lang$global[['button']][['4']], width = largeur5)

        #################

        tkgrid(cb.typeTSp, row = 0, column = 2, sticky = 'we', rowspan = 1, columnspan = 6, padx = 1, pady = 1, ipadx = 1, ipady = 1)
        tkgrid(bt.TSGraphOpt, row = 1, column = 0, sticky = 'we', rowspan = 1, columnspan = 5, padx = 1, pady = 1, ipadx = 1, ipady = 1)
        tkgrid(bt.TsGraph.plot, row = 1, column = 5, sticky = 'we', rowspan = 1, columnspan = 5, padx = 1, pady = 1, ipadx = 1, ipady = 1)

        #################

        tkconfigure(bt.TSGraphOpt, command = function(){
            suffix.fun <- switch(trimws(tclvalue(.cdtData$EnvData$plot.maps$typeTSp)),
                                    "Barplot" = "Bar",
                                    "Line" = "Line")
            plot.fun <- get(paste0("MapGraph.GraphOptions.", suffix.fun), mode = "function")
            .cdtData$EnvData$TSGraphOp <- plot.fun(.cdtData$EnvData$TSGraphOp)
        })

        #########
        .cdtData$EnvData$tab$dataGraph <- NULL

        tkconfigure(bt.TsGraph.plot, command = function(){
            if(!is.null(.cdtData$EnvData$varData)){
                imgContainer <- CDT.Display.Graph(SeasonLengthCalc.plotSeasLenGraph, .cdtData$EnvData$tab$dataGraph, 'Season Length-Graph')
                .cdtData$EnvData$tab$dataGraph <- imageNotebookTab_unik(imgContainer, .cdtData$EnvData$tab$dataGraph)
            }
        })

        ##############################################

        frameSTNCrds <- ttklabelframe(subfr3, text = lang.dlg[['label']][['9']], relief = 'groove')

        frTS2 <- tkframe(frameSTNCrds)
        .cdtData$EnvData$plot.maps$lonLOC <- tclVar()
        .cdtData$EnvData$plot.maps$latLOC <- tclVar()
        .cdtData$EnvData$plot.maps$stnIDTSp <- tclVar()

        tkgrid(frTS2, row = 0, column = 0, sticky = 'e', pady = 1)

        ##############################################

        tkgrid(frameDataTS, row = 0, column = 0, sticky = 'we', pady = 1)
        tkgrid(frameSTNCrds, row = 1, column = 0, sticky = '', pady = 3)

    #######################################################################################################

    #Tab4
    subfr4 <- bwTabScrollableFrame(cmd.tab4)

        ##############################################

        frameSHP <- create_shpLayer_frame(subfr4)
        tkgrid(frameSHP, row = 0, column = 0, sticky = 'we', pady = 1)

    #######################################################################################################

    widgets.Station.Pixel <- function(){
        tkdestroy(frTS2)
        frTS2 <<- tkframe(frameSTNCrds)

        if(.cdtData$EnvData$output$params$data.type == "cdtstation"){
            stnIDTSPLOT <- .cdtData$EnvData$output$data$id
            txt.stnSel <- tklabel(frTS2, text = lang.dlg[['label']][['11']])
            bt.stnID.prev <- ttkbutton(frTS2, text = "<<", width = largeur4)
            bt.stnID.next <- ttkbutton(frTS2, text = ">>", width = largeur4)
            cb.stnID <- ttkcombobox(frTS2, values = stnIDTSPLOT, textvariable = .cdtData$EnvData$plot.maps$stnIDTSp, justify = 'center', width = largeur3)
            tclvalue(.cdtData$EnvData$plot.maps$stnIDTSp) <- stnIDTSPLOT[1]

            tkgrid(txt.stnSel, row = 0, column = 0, sticky = '', rowspan = 1, columnspan = 3, padx = 1, pady = 1, ipadx = 1, ipady = 1)
            tkgrid(bt.stnID.prev, row = 1, column = 0, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
            tkgrid(cb.stnID, row = 1, column = 1, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
            tkgrid(bt.stnID.next, row = 1, column = 2, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)

            ######
            tkconfigure(bt.stnID.prev, command = function(){
                if(!is.null(.cdtData$EnvData$varData)){
                    istn <- which(stnIDTSPLOT == trimws(tclvalue(.cdtData$EnvData$plot.maps$stnIDTSp)))
                    istn <- istn - 1
                    if(istn < 1) istn <- length(stnIDTSPLOT)
                    tclvalue(.cdtData$EnvData$plot.maps$stnIDTSp) <- stnIDTSPLOT[istn]

                    imgContainer <- CDT.Display.Graph(SeasonLengthCalc.plotSeasLenGraph, .cdtData$EnvData$tab$dataGraph, 'Season Length-Graph')
                    .cdtData$EnvData$tab$dataGraph <- imageNotebookTab_unik(imgContainer, .cdtData$EnvData$tab$dataGraph)
                }
            })

            tkconfigure(bt.stnID.next, command = function(){
                if(!is.null(.cdtData$EnvData$varData)){
                    istn <- which(stnIDTSPLOT == trimws(tclvalue(.cdtData$EnvData$plot.maps$stnIDTSp)))
                    istn <- istn + 1
                    if(istn > length(stnIDTSPLOT)) istn <- 1
                    tclvalue(.cdtData$EnvData$plot.maps$stnIDTSp) <- stnIDTSPLOT[istn]

                    imgContainer <- CDT.Display.Graph(SeasonLengthCalc.plotSeasLenGraph, .cdtData$EnvData$tab$dataGraph, 'Season Length-Graph')
                    .cdtData$EnvData$tab$dataGraph <- imageNotebookTab_unik(imgContainer, .cdtData$EnvData$tab$dataGraph)
                }
            })
        }else{
            txt.crdSel <- tklabel(frTS2, text = lang.dlg[['label']][['12']], anchor = 'w', justify = 'left')
            txt.lonLoc <- tklabel(frTS2, text = lang.dlg[['label']][['13']], anchor = 'e', justify = 'right')
            en.lonLoc <- tkentry(frTS2, textvariable = .cdtData$EnvData$plot.maps$lonLOC, width = largeur7)
            txt.latLoc <- tklabel(frTS2, text = lang.dlg[['label']][['14']], anchor = 'e', justify = 'right')
            en.latLoc <- tkentry(frTS2, textvariable = .cdtData$EnvData$plot.maps$latLOC, width = largeur7)
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

    set.plot.type <- function(data_type){
        if(data_type == 'cdtstation'){
            .data.type <- "Points"
            plot_type <- list(values = c("Pixels", "Points"), var = "Pixels")
        }

        if(data_type == 'cdtdataset'){
            .data.type <- "Grid"
            plot_type <- list(values = c("Pixels", "FilledContour"), var = "Pixels")
        }

        .cdtData$EnvData$dataMapOp$plotType <- plot_type
        .cdtData$EnvData$plot.maps$.data.type <- .data.type
        .cdtData$EnvData$plot.maps$data.type <- data_type
    }

    #################

    set.Data.Dates <- function(){
        donDates <- format(.cdtData$EnvData$output$start.date, "%Y")
        tkconfigure(cb.data.Index, values = donDates)
        tclvalue(.cdtData$EnvData$donDate) <- donDates[length(donDates)]
        return(0)
    }

    #######################################################################################################

    read.Data.Map <- function(){
        tkconfigure(.cdtEnv$tcl$main$win, cursor = 'watch')
        tcl('update')
        on.exit({
            tkconfigure(.cdtEnv$tcl$main$win, cursor = '')
            tcl('update')
        })

        this.daty <- trimws(tclvalue(.cdtData$EnvData$donDate))
        idt <- which(format(.cdtData$EnvData$output$start.date, "%Y") == this.daty)

        if(.cdtData$EnvData$output$params$data.type == "cdtstation"){
            filePathData <- file.path(.cdtData$EnvData$PathData, "CDTDATASET/SEASONLENGTH.rds")
            if(!file.exists(filePathData)){
                Insert.Messages.Out(paste(filePathData, lang.dlg[['message']][['5']]), TRUE, 'e')
                return(NULL)
            }

            change.plot <- .cdtData$EnvData$dataMapOp$plotType$var

            ########
            readVarData <- TRUE
            if(!is.null(.cdtData$EnvData$varData))
                if(!is.null(.cdtData$EnvData$filePathData))
                    if(.cdtData$EnvData$filePathData == filePathData) readVarData <- FALSE

            if(readVarData){
                .cdtData$EnvData$varData$data <- readRDS(filePathData)
                .cdtData$EnvData$filePathData <- filePathData
            }

            ########
            rasterVarData <- TRUE
            if(!rasterVarData)
                if(!is.null(.cdtData$EnvData$varData$rasterDate))
                    if(.cdtData$EnvData$filePathData == filePathData)
                        if(.cdtData$EnvData$varData$rasterDate == this.daty) rasterVarData <- FALSE

            if(!rasterVarData)
                if(.cdtData$EnvData$change.plot != change.plot) rasterVarData <- TRUE

            if(rasterVarData){
                X0 <- .cdtData$EnvData$output$data$lon
                Y0 <- .cdtData$EnvData$output$data$lat
                VAR0 <- as.numeric(.cdtData$EnvData$varData$data[idt, ])

                if(change.plot == "Pixels"){
                    nx <- nx_ny_as.image(diff(range(X0)))
                    ny <- nx_ny_as.image(diff(range(Y0)))
                    tmp <- cdt.as.image(VAR0, nx = nx, ny = ny, pts.xy = cbind(X0, Y0))
                    .cdtData$EnvData$varData$map$x <- tmp$x
                    .cdtData$EnvData$varData$map$y <- tmp$y
                    .cdtData$EnvData$varData$map$z <- tmp$z
                    rm(tmp)
                }

                if(change.plot == "Points"){
                    .cdtData$EnvData$varData$map$x <- X0
                    .cdtData$EnvData$varData$map$y <- Y0
                    .cdtData$EnvData$varData$map$z <- VAR0
                }

                .cdtData$EnvData$varData$rasterDate <- this.daty
                .cdtData$EnvData$change.plot <- change.plot
            }
        }else{
            filePathData <- file.path(.cdtData$EnvData$PathData, "DATA_NetCDF",
                            paste0("seasLen_", format(.cdtData$EnvData$output$start.date[idt], "%Y%m%d"), ".nc"))
            if(!file.exists(filePathData)){
                Insert.Messages.Out(paste(filePathData, lang.dlg[['message']][['5']]), TRUE, 'e')
                return(NULL)
            }

            readVarData <- TRUE
            if(!is.null(.cdtData$EnvData$varData))
                if(!is.null(.cdtData$EnvData$filePathData))
                    if(.cdtData$EnvData$filePathData == filePathData) readVarData <- FALSE

            if(readVarData){
                nc <- ncdf4::nc_open(filePathData)
                .cdtData$EnvData$varData$map$x <- nc$dim[[1]]$vals
                .cdtData$EnvData$varData$map$y <- nc$dim[[2]]$vals
                .cdtData$EnvData$varData$map$z <- ncdf4::ncvar_get(nc, varid = nc$var[[1]]$name)
                ncdf4::nc_close(nc)
                .cdtData$EnvData$filePathData <- filePathData
            }

            ###################

            file.CDT.Idx <- file.path(.cdtData$EnvData$PathData, "CDTDATASET/CDTDATASET.rds")

            read.cdt.dataIdx <- TRUE
            if(!is.null(.cdtData$EnvData$cdtdataset))
                if(!is.null(.cdtData$EnvData$file.CDT.Idx))
                    if(.cdtData$EnvData$file.CDT.Idx == file.CDT.Idx) read.cdt.dataIdx <- FALSE
            if(read.cdt.dataIdx){
                .cdtData$EnvData$cdtdataset <- readRDS(file.CDT.Idx)
                .cdtData$EnvData$cdtdataset$fileInfo <- file.CDT.Idx
                .cdtData$EnvData$file.CDT.Idx <- file.CDT.Idx
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
