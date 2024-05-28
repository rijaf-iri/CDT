
AssessDataPanelCmd <- function(){
    if(WindowsOS()){
        largeur0 <- 25
        largeur1 <- 34
        largeur2 <- 37
        largeur3 <- 16
        largeur4 <- 14
        largeur5 <- 12
        largeur6 <- 22
    }else{
        largeur0 <- 23
        largeur1 <- 32
        largeur2 <- 33
        largeur3 <- 14
        largeur4 <- 14
        largeur5 <- 11
        largeur6 <- 22
    }

    ###################

    GeneralParameters <- list(intstep = "daily", minhour = 1, infile = "", outdir = "")

    .cdtData$EnvData$availPeriod <- list(start.year = 1981, start.mon = 1,
                                         start.dek = 1, start.pen = 1, start.day = 1,
                                         start.hour = 0, start.min = 0,
                                         end.year = 2022, end.mon = 12,
                                         end.dek = 3, end.pen = 6, end.day = 31,
                                         end.hour = 23, end.min = 55)

    map_Options <- list(presetCol = list(color = 'tim.colors', reverse = FALSE),
                        userCol = list(custom = FALSE, color = NULL),
                        userLvl = list(custom = FALSE, levels = NULL, equidist = FALSE),
                        title = list(user = FALSE, title = ''),
                        colkeyLab = list(user = FALSE, label = ''),
                        scalebar = list(add = FALSE, pos = 'bottomleft'),
                        plotType = list(values = c("Pixels", "Points"), var = "Pixels"),
                        pointSize = 1.0, bbox = .cdtData$Config$region)
    .cdtData$EnvData$avaiMapOp <- map_Options
    .cdtData$EnvData$grpMapOp <- map_Options

    .cdtData$EnvData$TSGraphOp <- list(
                                        axislabs = list(is.xlab = FALSE, xlab = '', is.ylab = FALSE, ylab = ''),
                                        title = list(is.title = FALSE, title = ''),
                                        colors = list(col = "cyan4")
                                    )

    .cdtData$EnvData$plot.maps$data.type <- "cdtstation"

    ###################

    xml.dlg <- file.path(.cdtDir$dirLocal, "languages", "cdtDataAvailab_leftCmd.xml")
    lang.dlg <- cdtLanguageParse(xml.dlg, .cdtData$Config$lang.iso)
    .cdtData$EnvData$message <- lang.dlg[['message']]
    .cdtData$EnvData$plottext <- lang.dlg[['plottext']]

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

        CbperiodVAL <- .cdtEnv$tcl$lang$global[['combobox']][['1']][1:6]
        periodVAL <- c('minute', 'hourly', 'daily', 'pentad', 'dekadal', 'monthly')
        timeSteps <- tclVar()
        tclvalue(timeSteps) <- CbperiodVAL[periodVAL %in% GeneralParameters$intstep]

        retminhr <- set.hour.minute(GeneralParameters$intstep, GeneralParameters$minhour)
        minhour.tclVar <- tclVar(retminhr$val)

        cb.fperiod <- ttkcombobox(frameTimeS, values = CbperiodVAL, textvariable = timeSteps, justify = 'center', width = largeur0)
        cb.minhour <- ttkcombobox(frameTimeS, values = retminhr$cb, textvariable = minhour.tclVar, state = retminhr$state, width = 2)

        tkgrid(cb.fperiod, row = 0, column = 0, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
        tkgrid(cb.minhour, row = 0, column = 1, sticky = 'we', rowspan = 1, columnspan = 1, padx = 0, pady = 1, ipadx = 1, ipady = 1)

        helpWidget(cb.fperiod, lang.dlg[['tooltip']][['1']], lang.dlg[['status']][['1']])

        tkbind(cb.fperiod, "<<ComboboxSelected>>", function(){
            intstep <- periodVAL[CbperiodVAL %in% trimws(tclvalue(timeSteps))]
            minhour <- as.numeric(trimws(tclvalue(minhour.tclVar)))
            retminhr <- set.hour.minute(intstep, minhour)
            tkconfigure(cb.minhour, values = retminhr$cb, state = retminhr$state)
            tclvalue(minhour.tclVar) <- retminhr$val
        
            ###
            i2 <- if(intstep %in% c('minute', 'hourly')) 2 else 1
            tkconfigure(bt.YearMonAvg, text = lang.dlg[['button']][[paste0('3-', i2)]])
            tkconfigure(frameGroupMap, text = lang.dlg[['label']][[paste0('11-', i2)]])
        })

        #######################

        frameInData <- ttklabelframe(subfr1, text = lang.dlg[['label']][['2']], relief = 'groove')

        input.file <- tclVar(GeneralParameters$infile)

        txt.infile <- tklabel(frameInData, text = lang.dlg[['label']][['3']], anchor = 'w', justify = 'left')
        cb.infile <- ttkcombobox(frameInData, values = unlist(openFile_ttkcomboList()), textvariable = input.file, width = largeur1)
        addTo_all_Combobox_List(cb.infile)
        bt.infile <- tkbutton(frameInData, text = "...")

        tkconfigure(bt.infile, command = function(){
            dat.opfiles <- getOpenFiles(.cdtEnv$tcl$main$win)
            if(!is.null(dat.opfiles)){
                update.OpenFiles('ascii', dat.opfiles)
                tclvalue(input.file) <- dat.opfiles[[1]]
            }
        })

        tkgrid(txt.infile, row = 0, column = 0, sticky = 'we', rowspan = 1, columnspan = 5, padx = 1, pady = 1, ipadx = 1, ipady = 1)
        tkgrid(cb.infile, row = 1, column = 0, sticky = 'we', rowspan = 1, columnspan = 4, padx = 0, pady = 1, ipadx = 1, ipady = 1)
        tkgrid(bt.infile, row = 1, column = 4, sticky = 'we', rowspan = 1, columnspan = 1, padx = 0, pady = 1, ipadx = 1, ipady = 1)

        helpWidget(cb.infile, lang.dlg[['tooltip']][['2']], lang.dlg[['status']][['2']])
        helpWidget(bt.infile, lang.dlg[['tooltip']][['3']], lang.dlg[['status']][['3']])

        #######################

        frameDirSav <- tkframe(subfr1, relief = 'groove', borderwidth = 2)

        dir.save <- tclVar(GeneralParameters$outdir)

        txt.dir.save <- tklabel(frameDirSav, text = lang.dlg[['label']][['4']], anchor = 'w', justify = 'left')
        en.dir.save <- tkentry(frameDirSav, textvariable = dir.save, width = largeur2)
        bt.dir.save <- tkbutton(frameDirSav, text = "...")

        ######
        tkconfigure(bt.dir.save, command = function(){
            fileORdir2Save(dir.save, isFile = FALSE)
        })

        ######
        tkgrid(txt.dir.save, row = 0, column = 0, sticky = 'we', rowspan = 1, columnspan = 5, padx = 1, pady = 0, ipadx = 1, ipady = 1)
        tkgrid(en.dir.save, row = 1, column = 0, sticky = 'we', rowspan = 1, columnspan = 4, padx = 0, pady = 0, ipadx = 1, ipady = 1)
        tkgrid(bt.dir.save, row = 1, column = 4, sticky = 'we', rowspan = 1, columnspan = 1, padx = 0, pady = 0, ipadx = 1, ipady = 1)

        helpWidget(en.dir.save, lang.dlg[['tooltip']][['4']], lang.dlg[['status']][['4']])
        helpWidget(bt.dir.save, lang.dlg[['tooltip']][['5']], lang.dlg[['status']][['5']])

        #############################

        assesDataBut <- ttkbutton(subfr1, text = lang.dlg[['button']][['1']])

        tkconfigure(assesDataBut, command = function(){
            GeneralParameters$intstep <- periodVAL[CbperiodVAL %in% trimws(tclvalue(timeSteps))]
            GeneralParameters$minhour <- as.numeric(trimws(tclvalue(minhour.tclVar)))
            GeneralParameters$infile <- trimws(tclvalue(input.file))
            GeneralParameters$outdir <- trimws(tclvalue(dir.save))

            assign('GeneralParameters', GeneralParameters, envir = .GlobalEnv)

            Insert.Messages.Out(lang.dlg[['message']][['1']], TRUE, "i")

            tkconfigure(.cdtEnv$tcl$main$win, cursor = 'watch')
            tcl('update')

            output <- tryCatch2(
                        {
                            AssessDataAvailProcs(GeneralParameters)
                        },
                        error = function(e) errorFun(e),
                        finally = {
                            tkconfigure(.cdtEnv$tcl$main$win, cursor = '')
                            tcl('update')
                        }
                    )

            if(!is.null(output)){
                if(output$status == "ok"){
                    Insert.Messages.Out(lang.dlg[['message']][['2']], TRUE, "s")

                    .cdtData$EnvData$output <- output
                    .cdtData$EnvData$plot.maps[c('lon', 'lat', 'id')] <- output$data[c('lon', 'lat', 'id')]
                    .cdtData$EnvData$availPeriod[names(output$availPeriod)] <- output$availPeriod
                    set.Data.Group()
                    set.Stations.IDs()
                }else Insert.Messages.Out(lang.dlg[['message']][['3']], TRUE, "e")
            }else Insert.Messages.Out(lang.dlg[['message']][['3']], TRUE, "e")
        })

        ##############################################

        tkgrid(frameTimeS, row = 0, column = 0, sticky = '', padx = 1, pady = 1, ipadx = 1, ipady = 1)
        tkgrid(frameInData, row = 1, column = 0, sticky = 'we', padx = 1, pady = 3, ipadx = 1, ipady = 1)
        tkgrid(frameDirSav, row = 2, column = 0, sticky = 'we', padx = 1, pady = 1, ipadx = 1, ipady = 1)
        tkgrid(assesDataBut, row = 3, column = 0, sticky = 'we', padx = 1, pady = 3, ipadx = 1, ipady = 1)

    #######################################################################################################

    #Tab2
    subfr2 <- bwTabScrollableFrame(cmd.tab2)

        #######################

        frameAssesDat <- ttklabelframe(subfr2, text = lang.dlg[['label']][['5']], relief = 'groove')

        availExist <- tclVar(0)
        file.Index <- tclVar()

        stateAssesDat <- if(tclvalue(availExist) == "1") "normal" else "disabled"

        chk.dataIdx <- tkcheckbutton(frameAssesDat, variable = availExist, text = lang.dlg[['checkbutton']][['1']], anchor = 'w', justify = 'left')
        en.dataIdx <- tkentry(frameAssesDat, textvariable = file.Index, width = largeur2 + 5, state = stateAssesDat)
        bt.dataIdx <- ttkbutton(frameAssesDat, text = .cdtEnv$tcl$lang$global[['button']][['6']], state = stateAssesDat)

        ###############

        tkgrid(chk.dataIdx, row = 0, column = 0, sticky = 'we', rowspan = 1, columnspan = 4, padx = 1, pady = 1, ipadx = 1, ipady = 1)
        tkgrid(bt.dataIdx, row = 0, column = 4, sticky = 'e', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
        tkgrid(en.dataIdx, row = 1, column = 0, sticky = 'we', rowspan = 1, columnspan = 5, padx = 1, pady = 1, ipadx = 1, ipady = 1)

        ###############

        tkconfigure(bt.dataIdx, command = function(){
            path.Stat <- tclvalue(tkgetOpenFile(initialdir = getwd(), filetypes = .cdtEnv$tcl$data$filetypes6))
            if(path.Stat %in% c("", "NA") | is.na(path.Stat)) return(NULL)
            tclvalue(file.Index) <- path.Stat

            if(file.exists(trimws(tclvalue(file.Index)))){
                Outdata <- try(readRDS(trimws(tclvalue(file.Index))), silent = TRUE)
                if(inherits(Outdata, "try-error")){
                    Insert.Messages.Out(gsub('[\r\n]', '', Outdata[1]), TRUE, "e")
                    Insert.Messages.Out(lang.dlg[['message']][['3']], TRUE, "e")

                    .cdtData$EnvData$output$data_grp <- NULL
                    .cdtData$EnvData$output$data_ts <- NULL

                    tkconfigure(cb.group.Date, values = "")
                    tclvalue(.cdtData$EnvData$availDate) <- ""
                    tkconfigure(cb.stn.ID, values = "")
                    tclvalue(.cdtData$EnvData$plot.maps$stnIDTSp) <- ""
                    return(NULL)
                }

                .cdtData$EnvData$output <- Outdata
                .cdtData$EnvData$plot.maps[c('lon', 'lat', 'id')] <- Outdata$data[c('lon', 'lat', 'id')]
                .cdtData$EnvData$availPeriod[names(Outdata$availPeriod)] <- Outdata$availPeriod
                .cdtData$EnvData$output$PathData <- dirname(trimws(tclvalue(file.Index)))
                set.Data.Group()
                set.Stations.IDs()

                ####
                intstep <- .cdtData$EnvData$output$params$intstep
                i2 <- if(intstep %in% c('minute', 'hourly')) 2 else 1
                tkconfigure(bt.YearMonAvg, text = lang.dlg[['button']][[paste0('3-', i2)]])
                tkconfigure(frameGroupMap, text = lang.dlg[['label']][[paste0('11-', i2)]])
            }
        })

        ###############

        tkbind(chk.dataIdx, "<Button-1>", function(){
            stateAssesDat <- if(tclvalue(availExist) == '1') 'disabled' else 'normal'
            tkconfigure(en.dataIdx, state = stateAssesDat)
            tkconfigure(bt.dataIdx, state = stateAssesDat)

            stateDataIn <- if(tclvalue(availExist) == '1') 'normal' else 'disabled'
            tcl(tknote.cmd, 'itemconfigure', cmd.tab1$IDtab, state = stateDataIn)
        })

        #######################

        frameAvailab <- ttklabelframe(subfr2, text = lang.dlg[['label']][['6']], relief = 'groove')

        bt.avai.Map <- ttkbutton(frameAvailab, text = .cdtEnv$tcl$lang$global[['button']][['3']])
        bt.avai.MapOpt <- ttkbutton(frameAvailab, text = .cdtEnv$tcl$lang$global[['button']][['4']])
        bt.avai.Period <- ttkbutton(frameAvailab, text = lang.dlg[['button']][['6']])

        ###############

        tkgrid(bt.avai.Map, row = 0, column = 0, sticky = 'we', rowspan = 1, columnspan = 1, padx = 10, pady = 1, ipadx = 10, ipady = 1)
        tkgrid(bt.avai.MapOpt, row = 0, column = 1, sticky = 'we', rowspan = 1, columnspan = 1, padx = 10, pady = 1, ipadx = 10, ipady = 1)
        tkgrid(bt.avai.Period, row = 1, column = 0, sticky = 'we', rowspan = 1, columnspan = 2, padx = 1, pady = 5, ipadx = 10, ipady = 1)

        ###############

        tkconfigure(bt.avai.MapOpt, command = function(){
            if(!is.null(.cdtData$EnvData$DataAvail$map)){
                atlevel <- pretty(.cdtData$EnvData$DataAvail$map$z, n = 10, min.n = 7)
                if(is.null(.cdtData$EnvData$avaiMapOp$userLvl$levels)){
                    .cdtData$EnvData$avaiMapOp$userLvl$levels <- atlevel
                }else{
                    if(!.cdtData$EnvData$avaiMapOp$userLvl$custom)
                        .cdtData$EnvData$avaiMapOp$userLvl$levels <- atlevel
                }
            }
            .cdtData$EnvData$avaiMapOp <- MapGraph.MapOptions(.cdtData$EnvData$avaiMapOp)
        })

        tkconfigure(bt.avai.Period, command = function(){
            intstep <- get.data.timestep()
            .cdtData$EnvData$availPeriod <- getInfoDateRange(.cdtEnv$tcl$main$win,
                                                             .cdtData$EnvData$availPeriod,
                                                             intstep)
        })

        ###############

        .cdtData$EnvData$tab$availMap <- NULL

        tkconfigure(bt.avai.Map, command = function(){
            if(is.null(.cdtData$EnvData$output)) return(NULL)
            get.DataAvailable.Map()

            intstep <- get.data.timestep()
            typeTSp <- trimws(tclvalue(.cdtData$EnvData$plot.maps$typeTSp))
            i1 <- if(typeTSp == "Chart") 7 else 8
            i2 <- if(intstep %in% c('minute', 'hourly')) 2 else 1
            titre.graph <- lang.dlg[['label']][[paste0(i1, '-', i2)]]
            assessData_displayAvailability(lang.dlg[['label']][['9']], titre.graph)
        })

        #######################

        frameGroupMap <- ttklabelframe(subfr2, text = lang.dlg[['label']][['11-1']], relief = 'groove')

        .cdtData$EnvData$availDate <- tclVar()

        frMAPTS <- tkframe(frameGroupMap)
        cb.group.Date <- ttkcombobox(frMAPTS, values = "", textvariable = .cdtData$EnvData$availDate, width = largeur3, justify = 'center')
        bt.group.MapOpt <- ttkbutton(frMAPTS, text = .cdtEnv$tcl$lang$global[['button']][['4']], width = largeur5)

        bt.group.Date.prev <- ttkbutton(frameGroupMap, text = "<<", width = largeur5)
        bt.group.maps <- ttkbutton(frameGroupMap, text = .cdtEnv$tcl$lang$global[['button']][['3']], width = largeur5)
        bt.group.Date.next <- ttkbutton(frameGroupMap, text = ">>", width = largeur5)

        ###############

        tkgrid(cb.group.Date, row = 0, column = 0, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
        tkgrid(bt.group.MapOpt, row = 0, column = 1, sticky = 'we', rowspan = 1, columnspan = 1, padx = 0, pady = 1, ipadx = 1, ipady = 1)

        ###############

        tkgrid(frMAPTS, row = 0, column = 0, sticky = 'e', rowspan = 1, columnspan = 3, padx = 1, pady = 1, ipadx = 1, ipady = 1)
        tkgrid(bt.group.Date.prev, row = 1, column = 0, sticky = 'we', rowspan = 1, columnspan = 1, padx = 2, pady = 1, ipadx = 1, ipady = 1)
        tkgrid(bt.group.maps, row = 1, column = 1, sticky = 'we', rowspan = 1, columnspan = 1, padx = 2, pady = 1, ipadx = 1, ipady = 1)
        tkgrid(bt.group.Date.next, row = 1, column = 2, sticky = 'we', rowspan = 1, columnspan = 1, padx = 2, pady = 1, ipadx = 1, ipady = 1)

        ###############

        tkconfigure(bt.group.MapOpt, command = function(){
            if(!is.null(.cdtData$EnvData$DataGroup$map)){
                atlevel <- pretty(.cdtData$EnvData$DataGroup$map$z, n = 10, min.n = 7)
                if(is.null(.cdtData$EnvData$grpMapOp$userLvl$levels)){
                    .cdtData$EnvData$grpMapOp$userLvl$levels <- atlevel
                }else{
                    if(!.cdtData$EnvData$grpMapOp$userLvl$custom)
                        .cdtData$EnvData$grpMapOp$userLvl$levels <- atlevel
                }
            }
            .cdtData$EnvData$grpMapOp <- MapGraph.MapOptions(.cdtData$EnvData$grpMapOp)
        })

        ###############

        .cdtData$EnvData$tab$grpMap <- NULL

        tkconfigure(bt.group.maps, command = function(){
            if(trimws(tclvalue(.cdtData$EnvData$availDate)) != "" &
               !is.null(.cdtData$EnvData$output$data_grp))
            {
                get.DataGroup.Map()

                intstep <- get.data.timestep()
                typeTSp <- trimws(tclvalue(.cdtData$EnvData$plot.maps$typeTSp))
                i1 <- if(typeTSp == "Chart") 7 else 8
                i2 <- if(intstep %in% c('minute', 'hourly')) 2 else 1
                titre.graph <- lang.dlg[['label']][[paste0(i1, '-', i2)]]
                titre.map <- lang.dlg[['label']][[paste0('10-', i2)]]
                assessData_displayDataGroupMap(titre.map, titre.graph)
            }
        })

        tkconfigure(bt.group.Date.prev, command = function(){
            if(trimws(tclvalue(.cdtData$EnvData$availDate)) != ""){
                donDates <- as.character(.cdtData$EnvData$output$data_grp$grp)
                idaty <- which(donDates == trimws(tclvalue(.cdtData$EnvData$availDate)))
                idaty <- idaty - 1
                if(idaty < 1) idaty <- length(donDates)
                tclvalue(.cdtData$EnvData$availDate) <- donDates[idaty]
                get.DataGroup.Map()

                intstep <- get.data.timestep()
                typeTSp <- trimws(tclvalue(.cdtData$EnvData$plot.maps$typeTSp))
                i1 <- if(typeTSp == "Chart") 7 else 8
                i2 <- if(intstep %in% c('minute', 'hourly')) 2 else 1
                titre.graph <- lang.dlg[['label']][[paste0(i1, '-', i2)]]
                titre.map <- lang.dlg[['label']][[paste0('10-', i2)]]
                assessData_displayDataGroupMap(titre.map, titre.graph)
            }
        })

        tkconfigure(bt.group.Date.next, command = function(){
            if(trimws(tclvalue(.cdtData$EnvData$availDate)) != ""){
                donDates <- as.character(.cdtData$EnvData$output$data_grp$grp)
                idaty <- which(donDates == trimws(tclvalue(.cdtData$EnvData$availDate)))
                idaty <- idaty + 1
                if(idaty > length(donDates)) idaty <- 1
                tclvalue(.cdtData$EnvData$availDate) <- donDates[idaty]
                get.DataGroup.Map()

                intstep <- get.data.timestep()
                typeTSp <- trimws(tclvalue(.cdtData$EnvData$plot.maps$typeTSp))
                i1 <- if(typeTSp == "Chart") 7 else 8
                i2 <- if(intstep %in% c('minute', 'hourly')) 2 else 1
                titre.graph <- lang.dlg[['label']][[paste0(i1, '-', i2)]]
                titre.map <- lang.dlg[['label']][[paste0('10-', i2)]]
                assessData_displayDataGroupMap(titre.map, titre.graph)
            }
        })

        ##############################################

        tkgrid(frameAssesDat, row = 0, column = 0, sticky = 'we', padx = 1, pady = 1, ipadx = 1, ipady = 1)
        tkgrid(frameAvailab, row = 1, column = 0, sticky = '', padx = 1, pady = 3, ipadx = 1, ipady = 1)
        tkgrid(frameGroupMap, row = 2, column = 0, sticky = 'we', padx = 1, pady = 3, ipadx = 1, ipady = 1)

    #######################################################################################################

    #Tab3
    subfr3 <- bwTabScrollableFrame(cmd.tab3)

        #######################

        frameStation <- ttklabelframe(subfr3, text = lang.dlg[['label']][['13']], relief = 'groove')

        .cdtData$EnvData$plot.maps$stnIDTSp <- tclVar()
        plotTYPE <- c("Chart", "Table")
        .cdtData$EnvData$plot.maps$typeTSp <- tclVar('Chart')

        frSTNIDs <- tkframe(frameStation)
        txt.stn.ID <- tklabel(frSTNIDs, text = lang.dlg[['label']][['16']], anchor = 'w', justify = 'left')
        cb.stn.ID <- ttkcombobox(frSTNIDs, values = "", textvariable = .cdtData$EnvData$plot.maps$stnIDTSp, width = largeur6, justify = 'center')

        bt.stn.prev <- ttkbutton(frameStation, text = "<<", width = largeur5)
        bt.stn.plot <- ttkbutton(frameStation, text = .cdtEnv$tcl$lang$global[['button']][['3']], width = largeur5)
        bt.stn.next <- ttkbutton(frameStation, text = ">>", width = largeur5)

        frplotTYPE <- tkframe(frameStation)
        cb.stn.table <- ttkcombobox(frplotTYPE, values = plotTYPE, textvariable = .cdtData$EnvData$plot.maps$typeTSp, justify = 'center', width = largeur4)
        bt.stn.opt <- ttkbutton(frplotTYPE, text = .cdtEnv$tcl$lang$global[['button']][['4']], width = largeur5)

        ###############

        tkgrid(txt.stn.ID, row = 0, column = 0, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
        tkgrid(cb.stn.ID, row = 0, column = 1, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)

        ###############

        tkgrid(cb.stn.table, row = 0, column = 0, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
        tkgrid(bt.stn.opt, row = 0, column = 1, sticky = 'we', rowspan = 1, columnspan = 1, padx = 0, pady = 1, ipadx = 1, ipady = 1)

        ###############

        tkgrid(frSTNIDs, row = 0, column = 0, sticky = '', rowspan = 1, columnspan = 3, padx = 1, pady = 1, ipadx = 1, ipady = 1)
        tkgrid(bt.stn.prev, row = 1, column = 0, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
        tkgrid(bt.stn.plot, row = 1, column = 1, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
        tkgrid(bt.stn.next, row = 1, column = 2, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
        tkgrid(frplotTYPE, row = 2, column = 0, sticky = 'e', rowspan = 1, columnspan = 3, padx = 1, pady = 1, ipadx = 1, ipady = 1)

        #################

        tkconfigure(bt.stn.opt, command = function(){
            .cdtData$EnvData$TSGraphOp <- MapGraph.GraphOptions.Assess(.cdtData$EnvData$TSGraphOp)
        })

        ###############

        .cdtData$EnvData$tab$Chart <- NULL
        .cdtData$EnvData$tab$Table <- NULL

        tkconfigure(bt.stn.plot, command = function(){
            intstep <- get.data.timestep()
            if(trimws(tclvalue(.cdtData$EnvData$plot.maps$typeTSp)) == "Chart"){
                if(is.null(.cdtData$EnvData$output$data_grp)) return(NULL)
                i2 <- if(intstep %in% c('minute', 'hourly')) 2 else 1
                titre <- lang.dlg[['label']][[paste0('7-', i2)]]
                imgContainer1 <- assessData_displayDataGroupTS(.cdtData$EnvData$tab$Chart, titre)
                .cdtData$EnvData$tab$Chart <- imageNotebookTab_unik(imgContainer1, .cdtData$EnvData$tab$Chart)
            }else{
                if(is.null(.cdtData$EnvData$output$data_ts)) return(NULL)
                table.ts <- assessData_dataTableTS()
                if(is.null(table.ts)) return(NULL)
                i2 <- if(intstep %in% c('minute', 'hourly')) 2 else 1
                titre <- lang.dlg[['label']][[paste0('8-', i2)]]
                .cdtData$EnvData$tab$Table <- tableNotebookTab_unik(table.ts, .cdtData$EnvData$tab$Table, titre)
            }
        })

        tkconfigure(bt.stn.prev, command = function(){
            stnIDTSPLOT <- .cdtData$EnvData$output$data$id
            istn <- which(stnIDTSPLOT == trimws(tclvalue(.cdtData$EnvData$plot.maps$stnIDTSp)))
            istn <- istn - 1
            if(istn < 1) istn <- length(stnIDTSPLOT)
            tclvalue(.cdtData$EnvData$plot.maps$stnIDTSp) <- stnIDTSPLOT[istn]

            intstep <- get.data.timestep()
            if(trimws(tclvalue(.cdtData$EnvData$plot.maps$typeTSp)) == "Chart"){
                if(is.null(.cdtData$EnvData$output$data_grp)) return(NULL)
                i2 <- if(intstep %in% c('minute', 'hourly')) 2 else 1
                titre <- lang.dlg[['label']][[paste0('7-', i2)]]
                imgContainer1 <- assessData_displayDataGroupTS(.cdtData$EnvData$tab$Chart, titre)
                .cdtData$EnvData$tab$Chart <- imageNotebookTab_unik(imgContainer1, .cdtData$EnvData$tab$Chart)
            }else{
                if(is.null(.cdtData$EnvData$output$data_ts)) return(NULL)
                table.ts <- assessData_dataTableTS()
                if(is.null(table.ts)) return(NULL)
                i2 <- if(intstep %in% c('minute', 'hourly')) 2 else 1
                titre <- lang.dlg[['label']][[paste0('8-', i2)]]
                .cdtData$EnvData$tab$Table <- tableNotebookTab_unik(table.ts, .cdtData$EnvData$tab$Table, titre)
            }
        })

        tkconfigure(bt.stn.next, command = function(){
            stnIDTSPLOT <- .cdtData$EnvData$output$data$id
            istn <- which(stnIDTSPLOT == trimws(tclvalue(.cdtData$EnvData$plot.maps$stnIDTSp)))
            istn <- istn + 1
            if(istn > length(stnIDTSPLOT)) istn <- 1
            tclvalue(.cdtData$EnvData$plot.maps$stnIDTSp) <- stnIDTSPLOT[istn]

            intstep <- get.data.timestep()
            if(trimws(tclvalue(.cdtData$EnvData$plot.maps$typeTSp)) == "Chart"){
                if(is.null(.cdtData$EnvData$output$data_grp)) return(NULL)
                i2 <- if(intstep %in% c('minute', 'hourly')) 2 else 1
                titre <- lang.dlg[['label']][[paste0('7-', i2)]]
                imgContainer1 <- assessData_displayDataGroupTS(.cdtData$EnvData$tab$Chart, titre)
                .cdtData$EnvData$tab$Chart <- imageNotebookTab_unik(imgContainer1, .cdtData$EnvData$tab$Chart)
            }else{
                if(is.null(.cdtData$EnvData$output$data_ts)) return(NULL)
                table.ts <- assessData_dataTableTS()
                if(is.null(table.ts)) return(NULL)
                i2 <- if(intstep %in% c('minute', 'hourly')) 2 else 1
                titre <- lang.dlg[['label']][[paste0('8-', i2)]]
                .cdtData$EnvData$tab$Table <- tableNotebookTab_unik(table.ts, .cdtData$EnvData$tab$Table, titre)
            }
        })

        ###############

        tkbind(cb.stn.table, "<<ComboboxSelected>>", function(){
            if(trimws(tclvalue(.cdtData$EnvData$plot.maps$typeTSp)) == "Chart"){
                tkconfigure(bt.stn.plot, text = .cdtEnv$tcl$lang$global[['button']][['3']])
                tkconfigure(bt.stn.opt, state = "normal")
            }else{
                tkconfigure(bt.stn.plot, text = lang.dlg[['button']][['2']])
                tkconfigure(bt.stn.opt, state = "disabled")
            }
        })

        #######################

        bt.YearMonAvg <- ttkbutton(subfr3, text = lang.dlg[['button']][['3-1']])

        .cdtData$EnvData$tab$stnAvrg <- NULL
        tkconfigure(bt.YearMonAvg, command = function(){
            if(is.null(.cdtData$EnvData$output$PathData)) return(NULL)
            intstep <- get.data.timestep()
            i2 <- if(intstep %in% c('minute', 'hourly')) 2 else 1
            titre <- lang.dlg[['label']][[paste0('14-', i2)]]
            imgContainer <- CDT.Display.Graph(assessData_plotYearMonAvg, .cdtData$EnvData$tab$stnAvrg, titre)
            .cdtData$EnvData$tab$stnAvrg <- imageNotebookTab_unik(imgContainer, .cdtData$EnvData$tab$stnAvrg)
        })

        #######################

        bt.AllnonNA <- ttkbutton(subfr3, text = lang.dlg[['button']][['4']])

        .cdtData$EnvData$tab$stnActive <- NULL
        tkconfigure(bt.AllnonNA, command = function(){
            if(is.null(.cdtData$EnvData$output$PathData)) return(NULL)
            if(is.null(.cdtData$EnvData$output)) return(NULL)
            titre <- lang.dlg[['label']][['17']]
            imgContainer <- assessData_displayActivities(.cdtData$EnvData$tab$stnActive, titre)
            .cdtData$EnvData$tab$stnActive <- imageNotebookTab_unik(imgContainer, .cdtData$EnvData$tab$stnActive)
        })

        #######################

        bt.DistCor <- ttkbutton(subfr3, text = lang.dlg[['button']][['5']])

        .cdtData$EnvData$tab$distCor <- NULL
        tkconfigure(bt.DistCor, command = function(){
            if(is.null(.cdtData$EnvData$output$PathData)) return(NULL)
            imgContainer <- CDT.Display.Graph(assessData_plotDistCor, .cdtData$EnvData$tab$distCor, lang.dlg[['label']][['15']])
            .cdtData$EnvData$tab$distCor <- imageNotebookTab_unik(imgContainer, .cdtData$EnvData$tab$distCor)
        })

        ##############################################

        tkgrid(frameStation, row = 0, column = 0, sticky = 'we', padx = 1, pady = 1, ipadx = 1, ipady = 1)
        tkgrid(bt.YearMonAvg, row = 1, column = 0, sticky = 'we', padx = 1, pady = 3, ipadx = 1, ipady = 1)
        tkgrid(bt.AllnonNA, row = 2, column = 0, sticky = 'we', padx = 1, pady = 3, ipadx = 1, ipady = 1)
        tkgrid(bt.DistCor, row = 3, column = 0, sticky = 'we', padx = 1, pady = 3, ipadx = 1, ipady = 1)

    #######################################################################################################

    #Tab4
    subfr4 <- bwTabScrollableFrame(cmd.tab4)

        #######################

        frameSHP <- create_shpLayer_frame(subfr4)
        tkgrid(frameSHP, row = 0, column = 0, sticky = 'we', pady = 1)

    #######################################################################################################

    set.Data.Group <- function(){
        grp <- .cdtData$EnvData$output$data_grp$grp
        tkconfigure(cb.group.Date, values = grp)
        tclvalue(.cdtData$EnvData$availDate) <- grp[length(grp)]
    }

    set.Stations.IDs <- function(){
        stnIDTSPLOT <- .cdtData$EnvData$output$data$id
        tkconfigure(cb.stn.ID, values = stnIDTSPLOT)
        tclvalue(.cdtData$EnvData$plot.maps$stnIDTSp) <- stnIDTSPLOT[1]
    }

    get.data.timestep <- function(){
        if(tclvalue(availExist) == "0"){
            intstep <- periodVAL[CbperiodVAL %in% trimws(tclvalue(timeSteps))]
        }else{
            intstep <- .cdtData$EnvData$output$params$intstep
        }
        return(intstep)
    }

    get.data.minhour <- function(){
        if(tclvalue(availExist) == "0"){
            minhour <- as.numeric(trimws(tclvalue(minhour.tclVar)))
        }else{
            minhour <- .cdtData$EnvData$output$params$minhour
        }
        return(minhour)
    }

    ######################

    get.DataAvailable.Map <- function(){
        tkconfigure(.cdtEnv$tcl$main$win, cursor = 'watch')
        tcl('update')
        on.exit({
            tkconfigure(.cdtEnv$tcl$main$win, cursor = '')
            tcl('update')
        })

        intstep <- get.data.timestep()
        minhour <- get.data.minhour()
        cdtstn_dir <- file.path(.cdtData$EnvData$output$PathData, 'CDTSTATIONS')

        change.plot <- .cdtData$EnvData$avaiMapOp$plotType$var
        this.period <- paste0(unlist(.cdtData$EnvData$availPeriod), collapse = '-')

        formatData <- TRUE
        if(!is.null(.cdtData$EnvData$DataAvail))
            if(!is.null(.cdtData$EnvData$DataAvail$this.period))
                if(.cdtData$EnvData$DataAvail$this.period == this.period) formatData <- FALSE

        if(!formatData)
            if(.cdtData$EnvData$DataAvail$change.plotA != change.plot) formatData <- TRUE

        if(formatData){
            tmp <- data_Avail_percentage(cdtstn_dir, .cdtData$EnvData$output$data,
                                         .cdtData$EnvData$availPeriod, intstep, minhour)
            if(change.plot == "Pixels"){
                nx <- nx_ny_as.image(diff(range(tmp$x)))
                ny <- nx_ny_as.image(diff(range(tmp$y)))
                tmp <- cdt.as.image(tmp$z, nx = nx, ny = ny, pts.xy = cbind(tmp$x, tmp$y))
            }

            .cdtData$EnvData$DataAvail$map$x <- tmp$x
            .cdtData$EnvData$DataAvail$map$y <- tmp$y
            .cdtData$EnvData$DataAvail$map$z <- tmp$z

            .cdtData$EnvData$DataAvail$change.plotA <- change.plot
            .cdtData$EnvData$DataAvail$this.period <- this.period
        }
    }

    get.DataGroup.Map <- function(){
        tkconfigure(.cdtEnv$tcl$main$win, cursor = 'watch')
        tcl('update')
        on.exit({
            tkconfigure(.cdtEnv$tcl$main$win, cursor = '')
            tcl('update')
        })

        change.plot <- .cdtData$EnvData$grpMapOp$plotType$var
        this.daty <- trimws(tclvalue(.cdtData$EnvData$availDate))

        formatData <- TRUE
        if(!is.null(.cdtData$EnvData$DataGroup))
            if(!is.null(.cdtData$EnvData$DataGroup$this.daty))
                if(.cdtData$EnvData$DataGroup$this.daty == this.daty) formatData <- FALSE

        if(!formatData)
            if(.cdtData$EnvData$DataGroup$change.plotG != change.plot) formatData <- TRUE

        if(formatData){
            donDates <- as.character(.cdtData$EnvData$output$data_grp$grp)
            idt <- which(donDates == this.daty)
            x <- .cdtData$EnvData$output$data$lon
            y <- .cdtData$EnvData$output$data$lat
            tmp <- as.numeric(.cdtData$EnvData$output$data_grp$data[idt, ])

            if(change.plot == "Pixels"){
                nx <- nx_ny_as.image(diff(range(x)))
                ny <- nx_ny_as.image(diff(range(y)))
                tmp <- cdt.as.image(tmp, nx = nx, ny = ny, pts.xy = cbind(x, y))
                .cdtData$EnvData$DataGroup$map$x <- tmp$x
                .cdtData$EnvData$DataGroup$map$y <- tmp$y
                .cdtData$EnvData$DataGroup$map$z <- tmp$z
            }

            if(change.plot == "Points"){
                .cdtData$EnvData$DataGroup$map$x <- x
                .cdtData$EnvData$DataGroup$map$y <- y
                .cdtData$EnvData$DataGroup$map$z <- tmp
            }

            .cdtData$EnvData$DataGroup$change.plotG <- change.plot
            .cdtData$EnvData$DataGroup$this.daty <- this.daty
        }
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
