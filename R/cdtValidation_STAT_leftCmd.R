
Validation.STAT.PanelCmd <- function(clim.var){
    listOpenFiles <- openFile_ttkcomboList()
    if(WindowsOS()){
        largeur0 <- 32
        largeur1 <- 34
        largeur2 <- 37
        largeur3 <- 20
        largeur4 <- 15
        largeur5 <- 2
        largeur6 <- 32
        largeur7 <- 7
        largeur8 <- 8
        largeur9 <- 18
    }else{
        largeur0 <- 32
        largeur1 <- 33
        largeur2 <- 36
        largeur3 <- 18
        largeur4 <- 16
        largeur5 <- 3
        largeur6 <- 36
        largeur7 <- 7
        largeur8 <- 8
        largeur9 <- 17
    }

    ###################

    aggFun <- switch(clim.var, "RR" = "sum", "TT" = "mean")
    trhesVal <- switch(clim.var, "RR" = 1, "TT" = 20)
    graphMin <- switch(clim.var, "RR" = 0, "TT" = 5)
    graphMax <- switch(clim.var, "RR" = 80, "TT" = 35)

    GeneralParameters <- list(Tstep = "dekadal", STN.file1 = "", STN.file2 = "",
                              date.range = list(start.year = 1981, start.month = 1, end.year = 2018, end.month = 12),
                              aggr.series = list(aggr.data = FALSE, aggr.fun = aggFun, opr.fun = ">=", opr.thres = 0,
                                                 min.frac = list(unique = TRUE, all = 0.95,
                                                                 month = rep(0.95, 12))),
                              stat.data = "all",
                              dicho.fcst = list(fun = ">=", thres = trhesVal),
                              volume.stat = list(user = TRUE, one.thres = TRUE,
                                                 user.val = 80, user.file = '', from = 'obs', perc = 75,
                                                 period = list(all.years = TRUE, start.year = 1981,
                                                               end.year = 2010, min.year = 5)
                                                ),
                              add.to.plot = list(add.shp = FALSE, shp.file = "", add.dem = FALSE, dem.file = ""),
                              outdir = "", clim.var = clim.var, statsVar = 'CORR', type.graph = "Scatter"
                            )

    pointSizeI <- 1.0
    .cdtData$EnvData$statMapOp <- list(presetCol = list(color = 'tim.colors', reverse = FALSE),
                                       userCol = list(custom = FALSE, color = NULL),
                                       userLvl = list(custom = FALSE, levels = NULL, equidist = FALSE),
                                       title = list(user = FALSE, title = ''),
                                       colkeyLab = list(user = FALSE, label = ''),
                                       scalebar = list(add = FALSE, pos = 'bottomleft'),
                                       pointSize = pointSizeI
                                      )

    .cdtData$EnvData$GraphOp <- list(
                            scatter = list(
                                    xlim = list(is.min = FALSE, min = graphMin, is.max = FALSE, max = graphMax),
                                    ylim = list(is.min = FALSE, min = graphMin, is.max = FALSE, max = graphMax),
                                    axislabs = list(is.xlab = FALSE, xlab = '', is.ylab = FALSE, ylab = ''),
                                    title = list(is.title = FALSE, title = '', position = 'top'),
                                    point = list(pch = 20, cex = 0.9, col = 'grey10'),
                                    line = list(draw = TRUE, lwd = 2, col = 'red')
                                ),
                            cdf = list(
                                    xlim = list(is.min = FALSE, min = graphMin, is.max = FALSE, max = graphMax),
                                    ylim = list(is.min = FALSE, min = 0.05, is.max = FALSE, max = 1),
                                    axislabs = list(is.xlab = FALSE, xlab = '', is.ylab = FALSE, ylab = ''),
                                    legend = list(add = TRUE, obs = 'Station', est = 'Estimate'),
                                    title = list(is.title = FALSE, title = '', position = 'top'),
                                    plot = list(obs = list(type = 'line', line = "blue", points = "cyan", lwd = 2, pch = 21, cex = 1),
                                                est = list(type = 'line', line = "red", points = "pink", lwd = 2, pch = 21, cex = 1))
                                ),
                            line = list(
                                    xlim = list(is.min = FALSE, min = "1981-01-01", is.max = FALSE, max = "2017-12-31"),
                                    ylim = list(is.min = FALSE, min = graphMin, is.max = FALSE, max = graphMax),
                                    axislabs = list(is.xlab = FALSE, xlab = '', is.ylab = FALSE, ylab = ''),
                                    legend = list(add = TRUE, obs = 'Station', est = 'Estimate'),
                                    title = list(is.title = FALSE, title = '', position = 'top'),
                                    plot = list(obs = list(type = 'line', line = "blue", points = "cyan", lwd = 2, pch = 21, cex = 1),
                                                est = list(type = 'line', line = "red", points = "pink", lwd = 2, pch = 21, cex = 1))
                                )
                            )

    .cdtData$EnvData$SHPOp <- list(col = "black", lwd = 1.5)

    .cdtData$EnvData$dem$Opt <- list(
                                    user.colors = list(custom = FALSE, color = NULL),
                                    user.levels = list(custom = FALSE, levels = NULL, equidist = FALSE),
                                    preset.colors = list(color = 'gray.colors', reverse = FALSE),
                                    add.hill = FALSE 
                                )

    MOIS <- format(ISOdate(2014, 1:12, 1), "%b")

    ###################

    xml.dlg <- file.path(.cdtDir$dirLocal, "languages", "cdtValidation_STAT_leftCmd.xml")
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

    ##############################################

        frInputData <- ttklabelframe(subfr1, text = lang.dlg[['label']][['1']], relief = 'groove')

        file.period <- tclVar()
        CbperiodVAL <- .cdtEnv$tcl$lang$global[['combobox']][['1']][3:6]
        periodVAL <- c('daily', 'pentad', 'dekadal', 'monthly')
        tclvalue(file.period) <- CbperiodVAL[periodVAL %in% GeneralParameters$Tstep]

        file.stnfl1 <- tclVar(GeneralParameters$STN.file1)
        file.stnfl2 <- tclVar(GeneralParameters$STN.file2)

        txt.tstep <- tklabel(frInputData, text = lang.dlg[['label']][['2']], anchor = 'e', justify = 'right')
        cb.tstep <- ttkcombobox(frInputData, values = CbperiodVAL, textvariable = file.period)

        txt.stnfl <- tklabel(frInputData, text = lang.dlg[['label']][['3']], anchor = 'w', justify = 'left')
        cb.stnfl <- ttkcombobox(frInputData, values = unlist(listOpenFiles), textvariable = file.stnfl1, width = largeur0)
        bt.stnfl <- tkbutton(frInputData, text = "...")

        txt.valid <- tklabel(frInputData, text = lang.dlg[['label']][['4']], anchor = 'w', justify = 'left')
        cb.valid <- ttkcombobox(frInputData, values = unlist(listOpenFiles), textvariable = file.stnfl2, width = largeur0)
        bt.valid <- tkbutton(frInputData, text = "...")

        #######################

        tkgrid(txt.tstep, row = 0, column = 0, sticky = 'we', rowspan = 1, columnspan = 2, padx = 1, pady = 2, ipadx = 1, ipady = 1)
        tkgrid(cb.tstep, row = 0, column = 2, sticky = 'we', rowspan = 1, columnspan = 3, padx = 1, pady = 2, ipadx = 1, ipady = 1)

        tkgrid(txt.stnfl, row = 2, column = 0, sticky = 'we', rowspan = 1, columnspan = 5, padx = 1, pady = 1, ipadx = 1, ipady = 1)
        tkgrid(cb.stnfl, row = 3, column = 0, sticky = 'we', rowspan = 1, columnspan = 4, padx = 0, pady = 1, ipadx = 1, ipady = 1)
        tkgrid(bt.stnfl, row = 3, column = 4, sticky = 'w', rowspan = 1, columnspan = 1, padx = 0, pady = 1, ipadx = 1, ipady = 1)

        tkgrid(txt.valid, row = 4, column = 0, sticky = 'we', rowspan = 1, columnspan = 5, padx = 1, pady = 1, ipadx = 1, ipady = 1)
        tkgrid(cb.valid, row = 5, column = 0, sticky = 'we', rowspan = 1, columnspan = 4, padx = 0, pady = 1, ipadx = 1, ipady = 1)
        tkgrid(bt.valid, row = 5, column = 4, sticky = 'w', rowspan = 1, columnspan = 1, padx = 0, pady = 1, ipadx = 1, ipady = 1)

        helpWidget(cb.tstep, lang.dlg[['tooltip']][['1']], lang.dlg[['status']][['1']])
        helpWidget(cb.stnfl, lang.dlg[['tooltip']][['2']], lang.dlg[['status']][['2']])
        helpWidget(bt.stnfl, lang.dlg[['tooltip']][['3']], lang.dlg[['status']][['3']])
        helpWidget(cb.valid, lang.dlg[['tooltip']][['4']], lang.dlg[['status']][['4']])
        helpWidget(bt.valid, lang.dlg[['tooltip']][['3']], lang.dlg[['status']][['3']])

        #######################

        tkconfigure(bt.stnfl, command = function(){
            dat.opfiles <- getOpenFiles(.cdtEnv$tcl$main$win)
            if(!is.null(dat.opfiles)){
                update.OpenFiles('ascii', dat.opfiles)
                listOpenFiles[[length(listOpenFiles) + 1]] <<- dat.opfiles[[1]]
                tclvalue(file.stnfl1) <- dat.opfiles[[1]]
                listOpenFiles <- openFile_ttkcomboList()
                lapply(list(cb.stnfl, cb.valid, cb.adddem, cb.addshp), tkconfigure, values = unlist(listOpenFiles))
            }
        })

        tkconfigure(bt.valid, command = function(){
            dat.opfiles <- getOpenFiles(.cdtEnv$tcl$main$win)
            if(!is.null(dat.opfiles)){
                update.OpenFiles('ascii', dat.opfiles)
                listOpenFiles[[length(listOpenFiles) + 1]] <<- dat.opfiles[[1]]
                tclvalue(file.stnfl2) <- dat.opfiles[[1]]
                listOpenFiles <- openFile_ttkcomboList()
                lapply(list(cb.stnfl, cb.valid, cb.adddem, cb.addshp), tkconfigure, values = unlist(listOpenFiles))
            }
        })

        ##############################################

        frameDirSav <- ttklabelframe(subfr1, text = lang.dlg[['label']][['5']], relief = 'groove')

        file.save1 <- tclVar(GeneralParameters$outdir)

        en.dir.save <- tkentry(frameDirSav, textvariable = file.save1, width = largeur1)
        bt.dir.save <- tkbutton(frameDirSav, text = "...")

        tkgrid(en.dir.save, row = 0, column = 0, sticky = 'we', rowspan = 1, columnspan = 5, padx = 1, pady = 1, ipadx = 1, ipady = 1)
        tkgrid(bt.dir.save, row = 0, column = 5, sticky = 'e', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)

        helpWidget(en.dir.save, lang.dlg[['tooltip']][['5']], lang.dlg[['status']][['5']])
        helpWidget(bt.dir.save, lang.dlg[['tooltip']][['6']], lang.dlg[['status']][['6']])

        #############################

        tkconfigure(bt.dir.save, command = function() fileORdir2Save(file.save1, isFile = FALSE))

        #############################
        tkgrid(frInputData, row = 0, column = 0, sticky = 'we', padx = 1, pady = 1, ipadx = 1, ipady = 1)
        tkgrid(frameDirSav, row = 1, column = 0, sticky = 'we', padx = 1, pady = 1, ipadx = 1, ipady = 1)

    #######################################################################################################

    #Tab2
    subfr2 <- bwTabScrollableFrame(cmd.tab2)

    ##############################################

        frameHOV <- ttklabelframe(subfr2, text = lang.dlg[['label']][['6']], relief = 'groove')

        validExist <- tclVar(0)
        file.hovd <- tclVar()

        stateHOVd <- if(tclvalue(validExist) == "1") "normal" else "disabled"

        chk.hovd <- tkcheckbutton(frameHOV, variable = validExist, text = lang.dlg[['checkbutton']][['1']], anchor = 'w', justify = 'left')
        en.hovd <- tkentry(frameHOV, textvariable = file.hovd, width = largeur1 + 5, state = stateHOVd)
        bt.hovd <- ttkbutton(frameHOV, text = .cdtEnv$tcl$lang$global[['button']][['6']], state = stateHOVd)

        tkgrid(chk.hovd, row = 0, column = 0, sticky = 'we', rowspan = 1, columnspan = 4, padx = 1, pady = 1, ipadx = 1, ipady = 1)
        tkgrid(bt.hovd, row = 0, column = 4, sticky = 'e', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
        tkgrid(en.hovd, row = 1, column = 0, sticky = 'we', rowspan = 1, columnspan = 5, padx = 1, pady = 1, ipadx = 1, ipady = 1)

        ###############

        valdataExist <- NULL

        tkconfigure(bt.hovd, command = function(){
            path.hovd <- tclvalue(tkgetOpenFile(initialdir = getwd(), filetypes = .cdtEnv$tcl$data$filetypes6))
            if(path.hovd == "") return(NULL)
            tclvalue(file.hovd) <- path.hovd

            if(file.exists(str_trim(tclvalue(file.hovd)))){
                hovd.data <- try(readRDS(str_trim(tclvalue(file.hovd))), silent = TRUE)
                if(inherits(hovd.data, "try-error")){
                    Insert.Messages.Out(lang.dlg[['message']][['4']], TRUE, 'e')
                    Insert.Messages.Out(gsub('[\r\n]', '', hovd.data[1]), TRUE, 'e')
                    return(NULL)
                }

                .cdtData$EnvData$file.hovd <- str_trim(tclvalue(file.hovd))
                .cdtData$EnvData$GeneralParameters <- hovd.data$GeneralParameters
                .cdtData$EnvData$cdtData <- hovd.data$cdtData

                if(!is.null(hovd.data$opDATA)){
                    .cdtData$EnvData$opDATA <- hovd.data$opDATA
                    .cdtData$EnvData$Statistics <- hovd.data$Statistics
                }

                ###
                tclvalue(file.period) <- CbperiodVAL[periodVAL %in% hovd.data$GeneralParameters$Tstep]

                if(!is.null(.cdtData$EnvData$opDATA$id)){
                    statsdata <- StatDataT[STATDATATYPE %in% str_trim(tclvalue(stat.data))]

                    stateDispSTN <- if(statsdata == 'stn') 'normal' else 'disabled'
                    tkconfigure(cb.stat.sel, values = .cdtData$EnvData$opDATA$id, state = stateDispSTN)
                    tclvalue(stn.stat.tab) <- .cdtData$EnvData$opDATA$id[1]
                    tkconfigure(bt.stat.prev, state = stateDispSTN)
                    tkconfigure(bt.stat.next, state = stateDispSTN)

                    stateMaps <- if(statsdata == 'stn') 'normal' else 'disabled'
                    tkconfigure(cb.stats.maps, state = stateMaps)
                    tkconfigure(bt.stats.maps, state = stateMaps)
                    tkconfigure(cb.plot.type, state = stateMaps)
                    tkconfigure(bt.stats.Opt, state = stateMaps)

                    stateStnID <- if(statsdata == 'stn') 'normal' else 'disabled'
                    tkconfigure(cb.stn.graph, values = .cdtData$EnvData$opDATA$id, state = stateStnID)
                    tclvalue(.cdtData$EnvData$stnIDGraph) <- .cdtData$EnvData$opDATA$id[1]
                    tkconfigure(bt.stn.graph.prev, state = stateStnID)
                    tkconfigure(bt.stn.graph.next, state = stateStnID)

                    itype <- if(statsdata == 'all') 1:2 else 1:3
                    CbTypeGRAPH <- typeGraphCombo[itype]

                    if(statsdata == 'all'){
                        if(str_trim(tclvalue(type.graph)) == typeGraphCombo[3])
                            tclvalue(type.graph) <- typeGraphCombo[1]
                    }
                    tkconfigure(cb.stats.graph, values = CbTypeGRAPH)
                }

                valdataExist <<- 1
            }
        })

        tkbind(chk.hovd, "<Button-1>", function(){
            stateHOVd <- if(tclvalue(validExist) == '1') 'disabled' else 'normal'
            tkconfigure(en.hovd, state = stateHOVd)
            tkconfigure(bt.hovd, state = stateHOVd)
            valdataExist <<- if(stateHOVd == 'normal' & !is.null(.cdtData$EnvData$cdtData)) 1 else NULL

            stateValid <- if(tclvalue(validExist) == '1') 'normal' else 'disabled'
            tcl(tknote.cmd, 'itemconfigure', cmd.tab1$IDtab, state = stateValid)
        })

        ##############################################

        frameSeason <- ttklabelframe(subfr2, text = lang.dlg[['label']][['7']], relief = 'groove')

        ##############
        fr.year <- ttklabelframe(frameSeason, text = lang.dlg[['label']][['9']], relief = 'sunken', labelanchor = "n", borderwidth = 2)

        start.year <- tclVar(GeneralParameters$date.range$start.year)
        end.year <- tclVar(GeneralParameters$date.range$end.year)

        txt.to1 <- tklabel(fr.year, text = paste0('-', lang.dlg[['label']][['10']], '-'))
        en.years1 <- tkentry(fr.year, width = 5, textvariable = start.year, justify = 'right')
        en.years2 <- tkentry(fr.year, width = 5, textvariable = end.year, justify = 'right')

        tkgrid(en.years1, row = 0, column = 0, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1)
        tkgrid(txt.to1, row = 0, column = 1, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1)
        tkgrid(en.years2, row = 0, column = 2, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1)

        helpWidget(en.years1, lang.dlg[['tooltip']][['7']], lang.dlg[['status']][['7']])
        helpWidget(en.years2, lang.dlg[['tooltip']][['8']], lang.dlg[['status']][['8']])

        ##############
        fr.seas <- ttklabelframe(frameSeason, text = lang.dlg[['label']][['8']], relief = 'sunken', labelanchor = "n", borderwidth = 2)

        mon1 <- as.numeric(str_trim(GeneralParameters$date.range$start.month))
        mon2 <- as.numeric(str_trim(GeneralParameters$date.range$end.month))
        start.mois <- tclVar(MOIS[mon1])
        end.mois <- tclVar(MOIS[mon2])

        txt.to2 <- tklabel(fr.seas, text = paste0('-', lang.dlg[['label']][['10']], '-'))
        cb.month1 <- ttkcombobox(fr.seas, values = MOIS, textvariable = start.mois, width = 5)
        cb.month2 <- ttkcombobox(fr.seas, values = MOIS, textvariable = end.mois, width = 5)

        tkgrid(cb.month1, row = 0, column = 0, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1)
        tkgrid(txt.to2, row = 0, column = 1, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1)
        tkgrid(cb.month2, row = 0, column = 2, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1)

        helpWidget(cb.month1, lang.dlg[['tooltip']][['9']], lang.dlg[['status']][['9']])
        helpWidget(cb.month2, lang.dlg[['tooltip']][['10']], lang.dlg[['status']][['10']])

        ##############

        sepSeason <- tklabel(frameSeason, text = "", width = largeur5)

        tkgrid(fr.seas, row = 0, column = 0, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1)
        tkgrid(sepSeason, row = 0, column = 1, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1)
        tkgrid(fr.year, row = 0, column = 2, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1)

        ##############################################

        frameAggr <- ttklabelframe(subfr2, text = lang.dlg[['label']][['11']], relief = 'groove')

        aggr.data <- tclVar(GeneralParameters$aggr.series$aggr.data)

        stateAggr <- if(GeneralParameters$aggr.series$aggr.data) "normal" else "disabled"

        chk.aggrdata <- tkcheckbutton(frameAggr, variable = aggr.data, text = lang.dlg[['checkbutton']][['2']], anchor = 'w', justify = 'left', width = largeur6)
        bt.aggrPars <- ttkbutton(frameAggr, text = lang.dlg[['button']][['1']], state = stateAggr)

        tkgrid(chk.aggrdata, row = 0, column = 0, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
        tkgrid(bt.aggrPars, row = 1, column = 0, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)

        ########
        tkconfigure(bt.aggrPars, command = function(){
            GeneralParameters[['aggr.series']] <<- getInfo_AggregateFun(.cdtEnv$tcl$main$win,
                                                                        GeneralParameters[['aggr.series']])
        })

        tkbind(chk.aggrdata, "<Button-1>", function(){
            stateAggr <- if(tclvalue(aggr.data) == '1') 'disabled' else 'normal'
            tkconfigure(bt.aggrPars, state = stateAggr)
        })

        ##############################################

        frameStatData <- tkframe(subfr2, relief = 'groove', borderwidth = 2)

        STATDATATYPE <- lang.dlg[['combobox']][['1']]
        StatDataT <- c('all', 'avg', 'stn')
        stat.data <- tclVar()
        tclvalue(stat.data) <- STATDATATYPE[StatDataT %in% GeneralParameters$stat.data]

        txt.stat.data <- tklabel(frameStatData, text = lang.dlg[['label']][['12']], anchor = 'e', justify = 'right')
        cb.stat.data <- ttkcombobox(frameStatData, values = STATDATATYPE, textvariable = stat.data, justify = 'center', width = largeur4)

        tkgrid(txt.stat.data, row = 0, column = 0, sticky = 'e', padx = 1, pady = 1, ipadx = 1, ipady = 1)
        tkgrid(cb.stat.data, row = 0, column = 1, sticky = 'we', padx = 1, pady = 1, ipadx = 1, ipady = 1)

        helpWidget(cb.stat.data, lang.dlg[['tooltip']][['11']], lang.dlg[['status']][['11']])

        #################
        tkbind(cb.stat.data, "<<ComboboxSelected>>", function(){
            statsdata <- StatDataT[STATDATATYPE %in% str_trim(tclvalue(stat.data))]

            stateDispSTN <- if(statsdata == 'stn') 'normal' else 'disabled'
            tkconfigure(bt.stat.prev, state = stateDispSTN)
            tkconfigure(cb.stat.sel, state = stateDispSTN)
            tkconfigure(bt.stat.next, state = stateDispSTN)

            stateMaps <- if(statsdata == 'stn') 'normal' else 'disabled'
            tkconfigure(cb.stats.maps, state = stateMaps)
            tkconfigure(bt.stats.maps, state = stateMaps)
            tkconfigure(cb.plot.type, state = stateMaps)
            tkconfigure(bt.stats.Opt, state = stateMaps)

            stateStnID <- if(statsdata == 'stn') 'normal' else 'disabled'
            tkconfigure(cb.stn.graph, state = stateStnID)
            tkconfigure(bt.stn.graph.prev, state = stateStnID)
            tkconfigure(bt.stn.graph.next, state = stateStnID)


            itype <- if(statsdata == 'all') 1:2 else 1:3
            CbTypeGRAPH <- typeGraphCombo[itype]

            if(statsdata == 'all'){
                if(str_trim(tclvalue(type.graph)) == typeGraphCombo[3])
                    tclvalue(type.graph) <- typeGraphCombo[1]
            }
            tkconfigure(cb.stats.graph, values = CbTypeGRAPH)
        })

        ##############################################

        bt.categStats <- ttkbutton(subfr2, text = lang.dlg[['button']][['2']])

        tkconfigure(bt.categStats, command = function(){
            GeneralParameters[['dicho.fcst']] <<- getInfo_categoricalValid(.cdtEnv$tcl$main$win,
                                                                           GeneralParameters[['dicho.fcst']])
        })

        ##############################################

        bt.volumeStats <- ttkbutton(subfr2, text = lang.dlg[['button']][['3']])

        tkconfigure(bt.volumeStats, command = function(){
            statsdata <- StatDataT[STATDATATYPE %in% str_trim(tclvalue(stat.data))]
            GeneralParameters[['volume.stat']] <<- getInfo_volumetricValid(.cdtEnv$tcl$main$win, statsdata,
                                                                           GeneralParameters[['volume.stat']])
        })

        ##############################################

        bt.calc.stat <- ttkbutton(subfr2, text = lang.dlg[['button']][['4']])

        tkconfigure(bt.calc.stat, command = function(){
            Insert.Messages.Out(lang.dlg[['message']][['1']], TRUE, "i")

            parsInput <- getInputInfos()
            readInFiles <- parsInput$STN.file1 != "" & parsInput$STN.file2 != ""
            getInputDATA <- FALSE
            if(is.null(valdataExist) & readInFiles) getInputDATA <- TRUE

            if(getInputDATA){
                tkconfigure(.cdtEnv$tcl$main$win, cursor = 'watch')
                tcl('update')
                ret <- tryCatch(
                    {
                        STAT_ValidationDataExec(parsInput)
                    },
                    warning = function(w) warningFun(w),
                    error = function(e) errorFun(e),
                    finally = {
                        tkconfigure(.cdtEnv$tcl$main$win, cursor = '')
                        tcl('update')
                    }
                )

                retNULL <- TRUE
                if(!is.null(ret))
                    if(ret == 0) retNULL <- FALSE
                if(retNULL){
                    Insert.Messages.Out(lang.dlg[['message']][['3']], TRUE, "e")
                    return(NULL)
                }
            }

            GeneralParameters$date.range$start.month <- which(MOIS %in% str_trim(tclvalue(start.mois)))
            GeneralParameters$date.range$end.month <- which(MOIS %in% str_trim(tclvalue(end.mois)))
            GeneralParameters$date.range$start.year <- as.numeric(str_trim(tclvalue(start.year)))
            GeneralParameters$date.range$end.year <- as.numeric(str_trim(tclvalue(end.year)))

            GeneralParameters$aggr.series$aggr.data <- switch(tclvalue(aggr.data), '0' = FALSE, '1' = TRUE)
            GeneralParameters$stat.data <- StatDataT[STATDATATYPE %in% str_trim(tclvalue(stat.data))]

            #####
            GeneralParameters$outdir <- str_trim(tclvalue(file.save1))
            GeneralParameters$STN.file <- str_trim(tclvalue(file.stnfl2))
            GeneralParameters$validExist <- switch(tclvalue(validExist), '0' = FALSE, '1' = TRUE)

            # assign('GeneralParameters', GeneralParameters, envir = .GlobalEnv)

            tkconfigure(.cdtEnv$tcl$main$win, cursor = 'watch')
            tcl('update')
            ret <- tryCatch(
                {
                    ValidationDataProcs(GeneralParameters)
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
                    Insert.Messages.Out(lang.dlg[['message']][['2']], TRUE, "s")

                    if(GeneralParameters$stat.data == 'stn'){
                        tkconfigure(cb.stat.sel, values = .cdtData$EnvData$opDATA$id)
                        tclvalue(stn.stat.tab) <- .cdtData$EnvData$opDATA$id[1]

                        tkconfigure(cb.stn.graph, values = .cdtData$EnvData$opDATA$id, state = 'normal')
                        tclvalue(.cdtData$EnvData$stnIDGraph) <- .cdtData$EnvData$opDATA$id[1]
                    }
                }else Insert.Messages.Out(lang.dlg[['message']][['3']], TRUE, 'e')
            }else Insert.Messages.Out(lang.dlg[['message']][['3']], TRUE, 'e')
        })

        ##############################################

        tkgrid(frameHOV, row = 0, column = 0, sticky = 'we')
        tkgrid(frameSeason, row = 1, column = 0, sticky = 'we', pady = 1)
        tkgrid(frameAggr, row = 2, column = 0, sticky = 'we', pady = 1)
        tkgrid(frameStatData, row = 3, column = 0, sticky = 'we', pady = 3)
        tkgrid(bt.categStats, row = 4, column = 0, sticky = 'we', pady = 3)
        if(clim.var == 'RR')
            tkgrid(bt.volumeStats, row = 5, column = 0, sticky = 'we', pady = 3)
        tkgrid(bt.calc.stat, row = 6, column = 0, sticky = 'we', pady = 3)

    #######################################################################################################

    #Tab3
    subfr3 <- bwTabScrollableFrame(cmd.tab3)

    ##############################################

        frameStatTab <- ttklabelframe(subfr3, text = lang.dlg[['label']][['13']], relief = 'groove')

        STATIONIDS <- ''
        stn.stat.tab <- tclVar()
        stateDispSTN <- if(GeneralParameters$stat.data == 'stn') 'normal' else 'disabled'

        bt.stat.prev <- ttkbutton(frameStatTab, text = "<<", state = stateDispSTN, width = largeur7)
        bt.stat.next <- ttkbutton(frameStatTab, text = ">>", state = stateDispSTN, width = largeur7)
        cb.stat.sel <- ttkcombobox(frameStatTab, values = STATIONIDS, textvariable = stn.stat.tab, width = largeur3, state = stateDispSTN,  justify = 'center')
        bt.stat.disp <- ttkbutton(frameStatTab, text = lang.dlg[['button']][['5']])

        tkgrid(bt.stat.prev, row = 0, column = 0, sticky = 'e', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
        tkgrid(cb.stat.sel, row = 0, column = 1, sticky = 'we', rowspan = 1, columnspan = 3, padx = 1, pady = 1, ipadx = 1, ipady = 1)
        tkgrid(bt.stat.next, row = 0, column = 4, sticky = 'w', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
        tkgrid(bt.stat.disp, row = 1, column = 0, sticky = 'we', rowspan = 1, columnspan = 5, padx = 1, pady = 1, ipadx = 1, ipady = 1)

        ################
        .cdtData$EnvData$tab$validStat <- NULL

        tkconfigure(bt.stat.disp, command = function(){
            if(!is.null(.cdtData$EnvData$Statistics)){
                statsdata <- StatDataT[STATDATATYPE %in% str_trim(tclvalue(stat.data))]

                if(statsdata == 'all'){
                    don <- .cdtData$EnvData$Statistics$ALL
                    dat2disp <- data.frame(don$statNames, don$statistics, don$description, don$perfect.score)
                    titleTab <- 'All-Data Statistics'
                }
                if(statsdata == 'avg'){
                    don <- .cdtData$EnvData$Statistics$AVG
                    dat2disp <- data.frame(don$statNames, don$statistics, don$description, don$perfect.score)
                    titleTab <- 'Spatial-Average Statistics'
                }
                if(statsdata == 'stn'){
                    don <- .cdtData$EnvData$Statistics$STN
                    istn <- which(.cdtData$EnvData$opDATA$id == str_trim(tclvalue(stn.stat.tab)))
                    dat2disp <- data.frame(don$statNames, don$statistics[, istn], don$description, don$perfect.score)
                    titleTab <- paste(tclvalue(stn.stat.tab), 'Statistics')
                }

                names(dat2disp) <- c('Name', 'Statistics', 'Description', 'Perfect.Score')
                rownames(dat2disp) <- NULL

                .cdtData$EnvData$tab$validStat <- tableNotebookTab_unik(dat2disp, .cdtData$EnvData$tab$validStat, titleTab, 12)
            }
        })

        tkconfigure(bt.stat.prev, command = function(){
            if(!is.null(.cdtData$EnvData$Statistics)){
                don <- .cdtData$EnvData$Statistics$STN
                istn <- which(.cdtData$EnvData$opDATA$id == str_trim(tclvalue(stn.stat.tab)))
                istn <- istn - 1
                if(istn < 1) istn <- length(.cdtData$EnvData$opDATA$id)
                tclvalue(stn.stat.tab) <- .cdtData$EnvData$opDATA$id[istn]

                dat2disp <- data.frame(don$statNames, don$statistics[, istn], don$description, don$perfect.score)
                names(dat2disp) <- c('Name', 'Statistics', 'Description', 'Perfect.Score')
                rownames(dat2disp) <- NULL

                titleTab <- paste(tclvalue(stn.stat.tab), 'Statistics')

                .cdtData$EnvData$tab$validStat <- tableNotebookTab_unik(dat2disp, .cdtData$EnvData$tab$validStat, titleTab, 12)
            }
        })

        tkconfigure(bt.stat.next, command = function(){
            if(!is.null(.cdtData$EnvData$Statistics)){
                don <- .cdtData$EnvData$Statistics$STN
                istn <- which(.cdtData$EnvData$opDATA$id == str_trim(tclvalue(stn.stat.tab)))
                istn <- istn + 1
                if(istn > length(.cdtData$EnvData$opDATA$id)) istn <- 1
                tclvalue(stn.stat.tab) <- .cdtData$EnvData$opDATA$id[istn]

                dat2disp <- data.frame(don$statNames, don$statistics[, istn], don$description, don$perfect.score)
                names(dat2disp) <- c('Name', 'Statistics', 'Description', 'Perfect.Score')
                rownames(dat2disp) <- NULL

                titleTab <- paste(tclvalue(stn.stat.tab), 'Statistics')

                .cdtData$EnvData$tab$validStat <- tableNotebookTab_unik(dat2disp, .cdtData$EnvData$tab$validStat, titleTab, 12)
            }
        })

        ##############################################

        frameMap <- ttklabelframe(subfr3, text = lang.dlg[['label']][['14']], relief = 'groove')

        statsCON <- c('CORR', 'BR2', 'BIAS', 'PBIAS', 'ME', 'MAE', 'RMSE', 'NSE', 'MNSE', 'RNSE', 'IOA', 'MIOA', 'RIOA')
        statsCAT <- c('POD', 'POFD', 'FAR', 'FBS', 'CSI', 'HSS')
        statsVOL <- c('MQB', 'MQE', 'VHI', 'QPOD', 'VFAR', 'QFAR', 'VMI', 'QMISS', 'VCSI', 'QCSI')

        ValStatNAMES0 <- c(statsCON, statsCAT, statsVOL)
        CbStatNAMES0 <- lang.dlg[['combobox']][['2']]
        ivarL <- switch(clim.var, "RR" = 1:29, "TT" = 1:19)

        statsVAR <- tclVar()
        CbStatNAMES <- CbStatNAMES0[ivarL]
        ValStatNAMES <- ValStatNAMES0[ivarL]
        tclvalue(statsVAR) <- CbStatNAMES[ValStatNAMES %in% GeneralParameters$statsVar]

        stateMaps <- if(GeneralParameters$stat.data == 'stn') 'normal' else 'disabled'

        cb.stats.maps <- ttkcombobox(frameMap, values = CbStatNAMES, textvariable = statsVAR, width = largeur2, state = stateMaps)

        ##########
        frMapBt <- tkframe(frameMap)

        bt.stats.maps <- ttkbutton(frMapBt, text = .cdtEnv$tcl$lang$global[['button']][['3']], state = stateMaps, width = largeur9)
        bt.stats.Opt <- ttkbutton(frMapBt, text = .cdtEnv$tcl$lang$global[['button']][['4']], state = stateMaps, width = largeur9)

        tkgrid(bt.stats.Opt, row = 0, column = 0, sticky = 'we', rowspan = 1, columnspan = 1, padx = 2, pady = 1, ipadx = 1, ipady = 1)
        tkgrid(bt.stats.maps, row = 0, column = 1, sticky = 'we', rowspan = 1, columnspan = 1, padx = 2, pady = 1, ipadx = 1, ipady = 1)
        
        ##########
        frPlotT <- tkframe(frameMap)

        typeMapPLOT <- c("Points", "Pixels")
        .cdtData$EnvData$typeMap <- tclVar("Points")

        txt.plot.type <- tklabel(frPlotT, text = lang.dlg[['label']][['15']], anchor = "e", justify = "right")
        cb.plot.type <- ttkcombobox(frPlotT, values = typeMapPLOT, textvariable = .cdtData$EnvData$typeMap, width = largeur8, state = stateMaps)

        tkgrid(txt.plot.type, row = 0, column = 0, sticky = 'e', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
        tkgrid(cb.plot.type, row = 0, column = 1, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)

        ##########
        tkgrid(cb.stats.maps, row = 0, column = 0, sticky = 'we')
        tkgrid(frMapBt, row = 1, column = 0, sticky = 'we')
        tkgrid(frPlotT, row = 2, column = 0, sticky = '')

        ##############

        tkconfigure(bt.stats.Opt, command = function(){
            if(!is.null(.cdtData$EnvData$Statistics)){
                mapstat <- ValStatNAMES[CbStatNAMES %in% str_trim(tclvalue(statsVAR))]
                istat <- which(.cdtData$EnvData$Statistics$STN$statNames == mapstat)

                don <- .cdtData$EnvData$Statistics$STN$statistics[istat, ]
                atlevel <- pretty(don, n = 10, min.n = 7)
                if(is.null(.cdtData$EnvData$statMapOp$userLvl$levels)){
                    .cdtData$EnvData$statMapOp$userLvl$levels <- atlevel
                }else{
                    if(!.cdtData$EnvData$statMapOp$userLvl$custom)
                        .cdtData$EnvData$statMapOp$userLvl$levels <- atlevel
                }
            }
            .cdtData$EnvData$statMapOp <- MapGraph.MapOptions(.cdtData$EnvData$statMapOp)

            if(str_trim(tclvalue(.cdtData$EnvData$typeMap)) == "Points")
                pointSizeI <<- .cdtData$EnvData$statMapOp$pointSize
        })

        ################

        .cdtData$EnvData$tab$Maps <- NULL

        tkconfigure(bt.stats.maps, command = function(){
            if(!is.null(.cdtData$EnvData$Statistics)){
                .cdtData$EnvData$statVAR <- ValStatNAMES[CbStatNAMES %in% str_trim(tclvalue(statsVAR))]
                .cdtData$EnvData$plot.maps$data.type <- "cdtstation"
                .cdtData$EnvData$plot.maps$lon <- .cdtData$EnvData$opDATA$lon
                .cdtData$EnvData$plot.maps$lat <- .cdtData$EnvData$opDATA$lat
                .cdtData$EnvData$plot.maps$id <- .cdtData$EnvData$opDATA$id

                Validation.DisplayStatMaps()
            }
        })

        ##############################################

        frameGraph <- ttklabelframe(subfr3, text = lang.dlg[['label']][['16']], relief = 'groove')

        ############
        frameGrP <- tkframe(frameGraph)

        typeGraphCombo <- lang.dlg[['combobox']][['3']]
        valGraphCombo <- c("Scatter", "CDF", "Lines")
        itype <- if(GeneralParameters$stat.data == 'all') 1:2 else 1:3

        type.graph <- tclVar()
        CbTypeGRAPH <- typeGraphCombo[itype]
        ValTypeGRAPH <- valGraphCombo[itype]
        tclvalue(type.graph) <- CbTypeGRAPH[ValTypeGRAPH %in% GeneralParameters$type.graph]

        cb.stats.graph <- ttkcombobox(frameGrP, values = CbTypeGRAPH, textvariable = type.graph, width = largeur2)
        bt.stats.graph <- ttkbutton(frameGrP, text = .cdtEnv$tcl$lang$global[['button']][['3']], width = largeur9)
        bt.Opt.graph <- ttkbutton(frameGrP, text = .cdtEnv$tcl$lang$global[['button']][['4']], width = largeur9)

        tkgrid(cb.stats.graph, row = 0, column = 0, sticky = 'we', rowspan = 1, columnspan = 6, padx = 1, pady = 1, ipadx = 1, ipady = 1)
        tkgrid(bt.Opt.graph, row = 1, column = 0, sticky = 'we', rowspan = 1, columnspan = 3, padx = 2, pady = 1, ipadx = 1, ipady = 1)
        tkgrid(bt.stats.graph, row = 1, column = 3, sticky = 'we', rowspan = 1, columnspan = 3, padx = 2, pady = 1, ipadx = 1, ipady = 1)

        ############
        frameGrS <- tkframe(frameGraph)

        STNIDGRAPH <- ""
        .cdtData$EnvData$stnIDGraph <- tclVar()
        stateStnID <- "disabled"

        cb.stn.graph <- ttkcombobox(frameGrS, values = STNIDGRAPH, textvariable = .cdtData$EnvData$stnIDGraph, width = largeur3, state = stateStnID, justify = 'center')
        bt.stn.graph.prev <- ttkbutton(frameGrS, text = "<<", state = stateStnID, width = largeur7)
        bt.stn.graph.next <- ttkbutton(frameGrS, text = ">>", state = stateStnID, width = largeur7)

        tkgrid(bt.stn.graph.prev, row = 0, column = 0, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 2, ipadx = 1, ipady = 1)
        tkgrid(cb.stn.graph, row = 0, column = 1, sticky = 'we', rowspan = 1, columnspan = 2, padx = 1, pady = 2, ipadx = 1, ipady = 1)
        tkgrid(bt.stn.graph.next, row = 0, column = 3, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 2, ipadx = 1, ipady = 1)

        ##############
        tkgrid(frameGrP, row = 0, column = 0, sticky = 'we')
        tkgrid(frameGrS, row = 1, column = 0, sticky = 'we')

        ##############
        .cdtData$EnvData$tab$Graph <- NULL

        tkconfigure(bt.stats.graph, command = function(){
            .cdtData$EnvData$type.graph <- valGraphCombo[typeGraphCombo %in% str_trim(tclvalue(type.graph))]
            if(!is.null(.cdtData$EnvData$opDATA$stnStatData)){
                imgContainer <- CDT.Display.Graph(Validation.plotGraph, .cdtData$EnvData$tab$Graph, 'Validation-Plot')
                .cdtData$EnvData$tab$Graph <- imageNotebookTab_unik(imgContainer, .cdtData$EnvData$tab$Graph)
            }
        })

        tkconfigure(bt.stn.graph.prev, command = function(){
            .cdtData$EnvData$type.graph <- valGraphCombo[typeGraphCombo %in% str_trim(tclvalue(type.graph))]
            if(!is.null(.cdtData$EnvData$opDATA$stnStatData)){
                istn <- which(.cdtData$EnvData$opDATA$id == str_trim(tclvalue(.cdtData$EnvData$stnIDGraph)))
                istn <- istn - 1
                if(istn < 1) istn <- length(.cdtData$EnvData$opDATA$id)
                tclvalue(.cdtData$EnvData$stnIDGraph) <- .cdtData$EnvData$opDATA$id[istn]

                imgContainer <- CDT.Display.Graph(Validation.plotGraph, .cdtData$EnvData$tab$Graph, 'Validation-Plot')
                .cdtData$EnvData$tab$Graph <- imageNotebookTab_unik(imgContainer, .cdtData$EnvData$tab$Graph)
            }
        })

        tkconfigure(bt.stn.graph.next, command = function(){
            .cdtData$EnvData$type.graph <- valGraphCombo[typeGraphCombo %in% str_trim(tclvalue(type.graph))]
            if(!is.null(.cdtData$EnvData$opDATA$stnStatData)){
                istn <- which(.cdtData$EnvData$opDATA$id == str_trim(tclvalue(.cdtData$EnvData$stnIDGraph)))
                istn <- istn + 1
                if(istn > length(.cdtData$EnvData$opDATA$id)) istn <- 1
                tclvalue(.cdtData$EnvData$stnIDGraph) <- .cdtData$EnvData$opDATA$id[istn]

                imgContainer <- CDT.Display.Graph(Validation.plotGraph, .cdtData$EnvData$tab$Graph, 'Validation-Plot')
                .cdtData$EnvData$tab$Graph <- imageNotebookTab_unik(imgContainer, .cdtData$EnvData$tab$Graph)
            }
        })

        ##############
        tkconfigure(bt.Opt.graph, command = function(){
            typeGraph <- valGraphCombo[typeGraphCombo %in% str_trim(tclvalue(type.graph))]
            plot.fun <- get(paste0("Validation.GraphOptions.", typeGraph), mode = "function")
            .cdtData$EnvData$GraphOp <- plot.fun(.cdtData$EnvData$GraphOp)
        })

        #############################
        tkgrid(frameStatTab, row = 0, column = 0, sticky = 'we')
        tkgrid(frameMap, row = 1, column = 0, sticky = 'we', pady = 3)
        tkgrid(frameGraph, row = 2, column = 0, sticky = 'we', pady = 1)

    #######################################################################################################

    #Tab4
    subfr4 <- bwTabScrollableFrame(cmd.tab4)

    ##############################################

        frameSHP <- ttklabelframe(subfr4, text = lang.dlg[['label']][['17']], relief = 'groove')

        .cdtData$EnvData$shp$add.shp <- tclVar(GeneralParameters$add.to.plot$add.shp)
        file.plotShp <- tclVar(GeneralParameters$add.to.plot$shp.file)

        stateSHP <- if(GeneralParameters$add.to.plot$add.shp) "normal" else "disabled"

        chk.addshp <- tkcheckbutton(frameSHP, variable = .cdtData$EnvData$shp$add.shp, text = lang.dlg[['checkbutton']][['3']], anchor = 'w', justify = 'left')
        bt.addshpOpt <- ttkbutton(frameSHP, text = .cdtEnv$tcl$lang$global[['button']][['4']], state = stateSHP)
        cb.addshp <- ttkcombobox(frameSHP, values = unlist(listOpenFiles), textvariable = file.plotShp, width = largeur0, state = stateSHP)
        bt.addshp <- tkbutton(frameSHP, text = "...", state = stateSHP)

        tkgrid(chk.addshp, row = 0, column = 0, sticky = 'we', rowspan = 1, columnspan = 6, padx = 1, pady = 1)
        tkgrid(bt.addshpOpt, row = 0, column = 6, sticky = 'we', rowspan = 1, columnspan = 2, padx = 1, pady = 1)
        tkgrid(cb.addshp, row = 1, column = 0, sticky = 'we', rowspan = 1, columnspan = 7, padx = 1, pady = 1)
        tkgrid(bt.addshp, row = 1, column = 7, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1)

        #################
        tkconfigure(bt.addshp, command = function(){
            shp.opfiles <- getOpenShp(.cdtEnv$tcl$main$win)
            if(!is.null(shp.opfiles)){
                update.OpenFiles('shp', shp.opfiles)
                tclvalue(file.plotShp) <- shp.opfiles[[1]]
                listOpenFiles[[length(listOpenFiles) + 1]] <<- shp.opfiles[[1]]
                listOpenFiles <- openFile_ttkcomboList()
                lapply(list(cb.stnfl, cb.valid, cb.adddem, cb.addshp), tkconfigure, values = unlist(listOpenFiles))

                shpofile <- getShpOpenData(file.plotShp)
                if(is.null(shpofile))
                    .cdtData$EnvData$shp$ocrds <- NULL
                else
                    .cdtData$EnvData$shp$ocrds <- getBoundaries(shpofile[[2]])
            }
        })

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

        frameDEM <- ttklabelframe(subfr4, text = lang.dlg[['label']][['18']], relief = 'groove')

        .cdtData$EnvData$dem$add.dem <- tclVar(GeneralParameters$add.to.plot$add.dem)
        file.grddem <- tclVar(GeneralParameters$add.to.plot$dem.file)

        stateDEM <- if(GeneralParameters$add.to.plot$add.dem) "normal" else "disabled"

        chk.adddem <- tkcheckbutton(frameDEM, variable = .cdtData$EnvData$dem$add.dem, text = lang.dlg[['checkbutton']][['4']], anchor = 'w', justify = 'left')
        bt.adddemOpt <- ttkbutton(frameDEM, text = .cdtEnv$tcl$lang$global[['button']][['4']], state = stateDEM)
        cb.adddem <- ttkcombobox(frameDEM, values = unlist(listOpenFiles), textvariable = file.grddem, width = largeur0, state = stateDEM)
        bt.adddem <- tkbutton(frameDEM, text = "...", state = stateDEM)

        tkgrid(chk.adddem, row = 0, column = 0, sticky = 'we', rowspan = 1, columnspan = 6, padx = 1, pady = 1)
        tkgrid(bt.adddemOpt, row = 0, column = 6, sticky = 'we', rowspan = 1, columnspan = 2, padx = 1, pady = 1)
        tkgrid(cb.adddem, row = 1, column = 0, sticky = 'we', rowspan = 1, columnspan = 7, padx = 1, pady = 1)
        tkgrid(bt.adddem, row = 1, column = 7, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1)

        #################

        tkconfigure(bt.adddem, command = function(){
            nc.opfiles <- getOpenNetcdf(.cdtEnv$tcl$main$win, initialdir = getwd())
            if(!is.null(nc.opfiles)){
                update.OpenFiles('netcdf', nc.opfiles)
                listOpenFiles[[length(listOpenFiles) + 1]] <<- nc.opfiles[[1]]
                tclvalue(file.grddem) <- nc.opfiles[[1]]
                listOpenFiles <- openFile_ttkcomboList()
                lapply(list(cb.stnfl, cb.valid, cb.adddem, cb.addshp), tkconfigure, values = unlist(listOpenFiles))

                demData <- getNCDFSampleData(str_trim(tclvalue(file.grddem)))
                if(!is.null(demData)){
                    jfile <- getIndex.AllOpenFiles(str_trim(tclvalue(file.grddem)))
                    demData <- .cdtData$OpenFiles$Data[[jfile]][[2]]
                    .cdtData$EnvData$dem$elv <- demData[c('x', 'y', 'z')]

                    demr <- raster::raster(demData[c('x', 'y', 'z')])
                    slope <- raster::terrain(demr, opt = 'slope')
                    aspect <- raster::terrain(demr, opt = 'aspect')
                    hill <- raster::hillShade(slope, aspect, angle = 40, direction = 270)
                    hill <- matrix(hill@data@values, hill@ncols, hill@nrows)
                    hill <- hill[, rev(seq(ncol(hill)))]
                    .cdtData$EnvData$dem$hill <- list(x = demData$x, y = demData$y, z = hill)

                    rm(demData, demr, slope, aspect, hill)
                }else{
                    Insert.Messages.Out(lang.dlg[['message']][['5']], TRUE, "e")
                    tclvalue(file.grddem) <- ""
                    .cdtData$EnvData$dem <- NULL
                }
            }
        })

        tkconfigure(bt.adddemOpt, command = function(){
            if(!is.null(.cdtData$EnvData$dem$elv)){
                atlevel <- pretty(.cdtData$EnvData$dem$elv$z, n = 10, min.n = 7)
                if(is.null(.cdtData$EnvData$dem$Opt$user.levels$levels)){
                    .cdtData$EnvData$dem$Opt$user.levels$levels <- atlevel
                }else{
                    if(!.cdtData$EnvData$dem$Opt$user.levels$custom)
                        .cdtData$EnvData$dem$Opt$user.levels$levels <- atlevel
                }
            }

            .cdtData$EnvData$dem$Opt <- MapGraph.gridDataLayer(.cdtData$EnvData$dem$Opt)
        })

        #################
        tkbind(cb.adddem, "<<ComboboxSelected>>", function(){
            demData <- getNCDFSampleData(str_trim(tclvalue(file.grddem)))
            if(!is.null(demData)){
                jfile <- getIndex.AllOpenFiles(str_trim(tclvalue(file.grddem)))
                demData <- .cdtData$OpenFiles$Data[[jfile]][[2]]
                .cdtData$EnvData$dem$elv <- demData[c('x', 'y', 'z')]

                demr <- raster::raster(demData[c('x', 'y', 'z')])
                slope <- raster::terrain(demr, opt = 'slope')
                aspect <- raster::terrain(demr, opt = 'aspect')
                hill <- raster::hillShade(slope, aspect, angle = 40, direction = 270)
                hill <- matrix(hill@data@values, hill@ncols, hill@nrows)
                hill <- hill[, rev(seq(ncol(hill)))]
                .cdtData$EnvData$dem$hill <- list(x = demData$x, y = demData$y, z = hill)

                rm(demData, demr, slope, aspect, hill)
            }else{
                Insert.Messages.Out(lang.dlg[['message']][['5']], TRUE, "e")
                tclvalue(file.grddem) <- ""
                .cdtData$EnvData$dem <- NULL
            }
        })

        tkbind(chk.adddem, "<Button-1>", function(){
            stateDEM <- if(tclvalue(.cdtData$EnvData$dem$add.dem) == "1") "disabled" else "normal"
            tkconfigure(cb.adddem, state = stateDEM)
            tkconfigure(bt.adddem, state = stateDEM)
            tkconfigure(bt.adddemOpt, state = stateDEM)
        })

        #############################
        tkgrid(frameSHP, row = 0, column = 0, sticky = 'we', pady = 1)
        tkgrid(frameDEM, row = 1, column = 0, sticky = 'we', pady = 1)

    #######################################################################################################

    getInputInfos <- function(){
        GeneralParameters$Tstep <- periodVAL[CbperiodVAL %in% str_trim(tclvalue(file.period))]

        GeneralParameters$STN.file1 <- str_trim(tclvalue(file.stnfl1))
        GeneralParameters$STN.file2 <- str_trim(tclvalue(file.stnfl2))
        GeneralParameters$outdir <- str_trim(tclvalue(file.save1))

        return(GeneralParameters)
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
