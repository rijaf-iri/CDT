
Validation.MultiData.PanelCmd <- function(clim.var){
    listOpenFiles <- openFile_ttkcomboList()
    if(WindowsOS()){
        largeur0 <- .cdtEnv$tcl$fun$w.widgets(30)
        largeur1 <- .cdtEnv$tcl$fun$w.widgets(32)
        largeur2 <- .cdtEnv$tcl$fun$w.widgets(33)
        largeur3 <- .cdtEnv$tcl$fun$w.widgets(20)
        largeur4 <- 30
        largeur5 <- 30

        data.w <- .cdtEnv$tcl$fun$w.scale(25)
        data.h <- .cdtEnv$tcl$fun$h.scale(27)
    }else{
        largeur0 <- .cdtEnv$tcl$fun$w.widgets(26)
        largeur1 <- .cdtEnv$tcl$fun$w.widgets(22)
        largeur2 <- .cdtEnv$tcl$fun$w.widgets(23)
        largeur3 <- .cdtEnv$tcl$fun$w.widgets(14)
        largeur4 <- 21
        largeur5 <- 22

        data.w <- .cdtEnv$tcl$fun$w.scale(25)
        data.h <- .cdtEnv$tcl$fun$h.scale(27)
    }

    ###################

    if(clim.var == "RR"){
        vmin <- 0
        vmax <- 200
    }else{
        vmin <- 5
        vmax <- 35
    }

    GeneralParameters <- list(intstep = "dekadal", outdir = "", STN.file = "",
                              date.range = list(start.year = 1981, start.month = 1, end.year = 2018, end.month = 12),
                              aggr.series = list(aggr.data = FALSE, aggr.fun = "sum", min.frac = 0.80, opr.fun = ">=", opr.thres = 0),
                              dicho.fcst = list(opr.fun = ">=", opr.thres = 1), stat.data = "all",
                              add.to.plot = list(add.shp = FALSE, shp.file = "")
                            )

    pointSizeI <- 1.0
    .cdtData$EnvData$statMapOp <- list(presetCol = list(color = 'tim.colors', reverse = FALSE),
                                       userCol = list(custom = FALSE, color = NULL),
                                       userLvl = list(custom = FALSE, levels = NULL, equidist = FALSE),
                                       title = list(user = FALSE, title = ''),
                                       colkeyLab = list(user = FALSE, label = ''),
                                       pointSize = pointSizeI)

    .cdtData$EnvData$GraphOp <- list(
                                scatter = list(xylim = list(is.min = FALSE, min = vmin, is.max = FALSE, max = vmax),
                                           axislabs = list(is.xlab = FALSE, xlab = '', is.ylab = FALSE, ylab = ''),
                                           title = list(is.title = FALSE, title = ''),
                                           plot = list(type = "points", # "hexbin"
                                                       col.points = "black", cex.points = 0.9,
                                                       # col.hexbin = c('red', 'ornage', 'yellow', 'green', 'blue'),
                                                       col.hexbin = rev(RColorBrewer::brewer.pal(11, 'Spectral')),
                                                       col.line = "red", wd.line = 2)
                                        ),
                                cdf = list(xlim = list(is.min = FALSE, min = vmin, is.max = FALSE, max = vmax),
                                       ylim = list(is.min = FALSE, min = 0, is.max = FALSE, max = 1),
                                       axislabs = list(is.xlab = FALSE, xlab = '', is.ylab = FALSE, ylab = ''),
                                       title = list(is.title = FALSE, title = ''),
                                       col.obs = "black",
                                       col.est = list(preset = TRUE, fun = "rainbow",
                                                      col = c('red', 'ornage', 'yellow', 'green', 'blue'))
                                    ),
                                lines = list(xlim = list(is.min = FALSE, min = '2017-01-01', is.max = FALSE, max = '2019-12-31'),
                                         ylim = list(is.min = FALSE, min = vmin, is.max = FALSE, max = vmax),
                                         axislabs = list(is.xlab = FALSE, xlab = '', is.ylab = FALSE, ylab = ''),
                                         title = list(is.title = FALSE, title = ''),
                                         col.obs = "black",
                                         col.est = list(preset = TRUE, fun = "rainbow",
                                                        col = c('red', 'ornage', 'yellow', 'green', 'blue'))
                                    )
                            )


    .cdtData$EnvData$SHPOp <- list(col = "black", lwd = 1.5)

    MOIS <- format(ISOdate(2014, 1:12, 1), "%b")

    ###################

    # xml.dlg <- file.path(.cdtDir$dirLocal, "languages", "cdtValidation_MultipleData_leftCmd.xml")
    # lang.dlg <- cdtLanguageParse(xml.dlg, .cdtData$Config$lang.iso)

    ###################

    CHXSTATS0 <- c('Correlation', 'Coefficient of determination (R2) multiplied by the regression slope',
                   'Bias', 'Percent Bias', 'Mean Error', 'Mean Absolute Error', 'Root Mean Square Error',
                   'Nash-Sutcliffe Efficiency', 'Modified Nash-Sutcliffe efficiency', 'Relative Nash-Sutcliffe efficiency',
                   'Index of Agreement', 'Modified index of agreement', 'Relative Index of Agreement')
    CHXSTATS1 <- c('Probability Of Detection', 'Probability Of False Detection',
                   'False Alarm Ratio', 'Frequency Bias (Bias score)',
                   'Critical Success Index', 'Heidke Skill Score')

    CHXSTATS2 <- c("Mean Quantile Bias", "Mean Quantile Error",
                   "Volumetric Hit Index", "Quantile Probability of Detection",
                   "Volumetric False Alarm Ratio", "Quantile False Alarm Ratio",
                   "Volumetric Miss Index", "Quantile Miss Index",
                   "Volumetric Critical Success Index", "Quantile Critical Success Index")

    if(clim.var == "RR") CHXSTATS <- c(CHXSTATS0, CHXSTATS1, CHXSTATS2)
    if(clim.var == "TT") CHXSTATS <- c(CHXSTATS0, CHXSTATS1)

    ###################

    .cdtEnv$tcl$main$cmd.frame <- tkframe(.cdtEnv$tcl$main$panel.left)

    tknote.cmd <- bwNoteBook(.cdtEnv$tcl$main$cmd.frame)
    cmd.tab1 <- bwAddTab(tknote.cmd, text = "Input")
    cmd.tab2 <- bwAddTab(tknote.cmd, text = "Validation")
    cmd.tab3 <- bwAddTab(tknote.cmd, text = "Plot")
    cmd.tab4 <- bwAddTab(tknote.cmd, text = "Add layers")

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

    isaDataSet <- local({
        k <- 0
        function() {
            k <<- k + 1
            return(k)
        }
    })

    add.new.datasets <- function(jj, ids){
        .cdtData$GalParams$DATASETs[[jj]]$tcl$frame <- ttklabelframe(subfr1, text = paste0("Data to be validated #", ids), relief = 'groove')

        .cdtData$GalParams$DATASETs[[jj]]$tcl$input.file <- tclVar(.cdtData$GalParams$DATASETs[[jj]]$pars$file)
        .cdtData$GalParams$DATASETs[[jj]]$tcl$data.name <- tclVar(.cdtData$GalParams$DATASETs[[jj]]$pars$title)

        en.datafile <- ttkcombobox(.cdtData$GalParams$DATASETs[[jj]]$tcl$frame, values = unlist(listOpenFiles),
                                    textvariable = .cdtData$GalParams$DATASETs[[jj]]$tcl$input.file, width = largeur1)

        bt.datafile <- tkbutton(.cdtData$GalParams$DATASETs[[jj]]$tcl$frame, text = "...")

        txt.dataname <- tklabel(.cdtData$GalParams$DATASETs[[jj]]$tcl$frame, text = "NAME")
        en.dataname <- tkentry(.cdtData$GalParams$DATASETs[[jj]]$tcl$frame, width = largeur5,
                                textvariable = .cdtData$GalParams$DATASETs[[jj]]$tcl$data.name)

        bt.Remove <- ttkbutton(.cdtData$GalParams$DATASETs[[jj]]$tcl$frame, text = "Remove")

        tkgrid(en.datafile, row = 0, column = 0, sticky = 'we', rowspan = 1, columnspan = 9, padx = 0, pady = 1, ipadx = 1, ipady = 1)
        tkgrid(bt.datafile, row = 0, column = 9, sticky = 'we', rowspan = 1, columnspan = 1, padx = 0, pady = 1, ipadx = 1, ipady = 1)
        tkgrid(txt.dataname, row = 1, column = 0, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
        tkgrid(en.dataname, row = 1, column = 1, sticky = 'we', rowspan = 1, columnspan = 7, padx = 1, pady = 1, ipadx = 1, ipady = 1)
        tkgrid(bt.Remove, row = 1, column = 8, sticky = 'we', rowspan = 1, columnspan = 2, padx = 1, pady = 1, ipadx = 1, ipady = 1)

        tkgrid(.cdtData$GalParams$DATASETs[[jj]]$tcl$frame)

        helpWidget(en.datafile, 'Select the file containing the station data from the list', 'Select the file containing the data to be validated in CDT format')
        helpWidget(bt.datafile, 'Browse file if not listed', 'Browse file if not listed')

        ####

        tkconfigure(bt.datafile, command = function(){
            dat.opfiles <- getOpenFiles(.cdtEnv$tcl$main$win)
            if(!is.null(dat.opfiles)){
                update.OpenFiles('ascii', dat.opfiles)
                listOpenFiles[[length(listOpenFiles) + 1]] <<- dat.opfiles[[1]]
                tclvalue(.cdtData$GalParams$DATASETs[[jj]]$tcl$input.file) <- dat.opfiles[[1]]
                tkconfigure(en.datafile, values = unlist(openFile_ttkcomboList()))
                .cdtData$GalParams$DATASETs[[jj]]$pars$file <- str_trim(tclvalue(.cdtData$GalParams$DATASETs[[jj]]$tcl$input.file))
            }
        })

        ####

        id.fr <- NULL
        tkbind(bt.Remove, "<Button-1>", function(){
            id.fr <<- tclvalue(tkwinfo("parent", bt.Remove))
        })

        tkconfigure(bt.Remove, command = function(){
            id.frame <- sapply(.cdtData$GalParams$DATASETs, function(x) x$tcl$frame$ID)

            ii <- which(id.frame == id.fr)
            tkdestroy(.cdtData$GalParams$DATASETs[[ii]]$tcl$frame)
            .cdtData$GalParams$DATASETs[[ii]] <- NULL
            tcl("update")
        })
    }

    #######################################################################################################

    #Tab1
    frameInValid <- tkframe(cmd.tab1)
    sep_dataset <- ttkseparator(cmd.tab1)
    bt.AddData <- tkbutton(cmd.tab1, text = "Add Data to validate", bg = 'lightgreen')
    frameDirSav <- ttklabelframe(cmd.tab1, text = "Directory to save result", relief = 'groove')

    ##################

    dir2save <- tclVar(GeneralParameters$outdir)

    en.dir.save <- tkentry(frameDirSav, textvariable = dir2save, width = largeur2)
    bt.dir.save <- tkbutton(frameDirSav, text = "...")

    tkgrid(en.dir.save, row = 0, column = 0, sticky = 'we', rowspan = 1, columnspan = 5, padx = 1, pady = 1, ipadx = 1, ipady = 1)
    tkgrid(bt.dir.save, row = 0, column = 5, sticky = 'e', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)

    helpWidget(en.dir.save, 'Enter the full path to the directory to save result', 'Enter the full path to the directory to save result')
    helpWidget(bt.dir.save, 'Browse here the full path to the directory to save result', 'Browse here the full path to the directory to save result')

    tkconfigure(bt.dir.save, command = function() fileORdir2Save(dir2save, isFile = FALSE))

    ##################
    tkgrid(frameInValid, sticky = '', padx = 1, pady = 1, ipadx = 1, ipady = 1)
    tkgrid(sep_dataset, sticky = 'we', padx = 1, pady = 1, ipadx = 1, ipady = 1)

    subfr1 <- bwTabScrollableFrame(cmd.tab1, hscrlwin = data.h, wscrlwin = data.w)
    tkgrid(bt.AddData, sticky = 'we', padx = 1, pady = 1, ipadx = 1, ipady = 1)
    tkgrid(frameDirSav, sticky = '', padx = 1, pady = 1, ipadx = 1, ipady = 1)

        ##############################################

        frameTS <- tkframe(frameInValid)

        timeSteps <- tclVar()
        CbperiodVAL <- .cdtEnv$tcl$lang$global[['combobox']][['1']][3:6]
        periodVAL <- c('daily', 'pentad', 'dekadal', 'monthly')
        tclvalue(timeSteps) <- CbperiodVAL[periodVAL %in% GeneralParameters$intstep]

        txt.tstep <- tklabel(frameTS, text = "Time step", anchor = 'w', justify = 'left')
        cb.tstep <- ttkcombobox(frameTS, values = CbperiodVAL, textvariable = timeSteps, width = largeur3)

        ########

        tkgrid(txt.tstep, row = 0, column = 0, sticky = 'we', rowspan = 1, columnspan = 2, padx = 1, pady = 1, ipadx = 1, ipady = 1)
        tkgrid(cb.tstep, row = 0, column = 2, sticky = 'we', rowspan = 1, columnspan = 6, padx = 1, pady = 1, ipadx = 1, ipady = 1)

        #######################

        frInputData <- tkframe(frameInValid)

        file.stnfl <- tclVar(GeneralParameters$STN.file)

        txt.stnfl <- tklabel(frInputData, text = 'Station data used to validate', anchor = 'w', justify = 'left')
        cb.stnfl <- ttkcombobox(frInputData, values = unlist(listOpenFiles), textvariable = file.stnfl, width = largeur1)
        bt.stnfl <- tkbutton(frInputData, text = "...")

        tkgrid(txt.stnfl, row = 0, column = 0, sticky = 'we', rowspan = 1, columnspan = 5, padx = 1, pady = 1, ipadx = 1, ipady = 1)
        tkgrid(cb.stnfl, row = 1, column = 0, sticky = 'we', rowspan = 1, columnspan = 4, padx = 0, pady = 1, ipadx = 1, ipady = 1)
        tkgrid(bt.stnfl, row = 1, column = 4, sticky = 'we', rowspan = 1, columnspan = 1, padx = 0, pady = 1, ipadx = 1, ipady = 1)

        helpWidget(cb.stnfl, 'Select the file containing the station data from the list', 'Select the file containing the station data used to validate in CDT format')
        helpWidget(bt.stnfl, 'Browse file if not listed', 'Browse file if not listed')

        tkconfigure(bt.stnfl, command = function(){
            dat.opfiles <- getOpenFiles(.cdtEnv$tcl$main$win)
            if(!is.null(dat.opfiles)){
                update.OpenFiles('ascii', dat.opfiles)
                listOpenFiles[[length(listOpenFiles) + 1]] <<- dat.opfiles[[1]]
                tclvalue(file.stnfl) <- dat.opfiles[[1]]
                listOpenFiles <- openFile_ttkcomboList()
                tkconfigure(cb.stnfl, values = unlist(listOpenFiles))
                # lapply(list(cb.stnfl, cb.valid, cb.adddem, cb.addshp), tkconfigure, values = unlist(listOpenFiles))
            }
        })

        #######################

        tkgrid(frameTS, row = 0, column = 0, sticky = '')
        tkgrid(frInputData, row = 1, column = 0, sticky = 'we')

        ##############################################

        tkconfigure(bt.AddData, command = function(){
            jj <- length(.cdtData$GalParams$DATASETs) + 1

            ids <- isaDataSet()
            .cdtData$GalParams$DATASETs[[jj]] <- list()
            .cdtData$GalParams$DATASETs[[jj]]$pars$title <- paste("Data set", ids)
            .cdtData$GalParams$DATASETs[[jj]]$pars$file <- ""

            add.new.datasets(jj, ids)
        })

    #######################################################################################################

    #Tab2
    subfr2 <- bwTabScrollableFrame(cmd.tab2)

    ##############################################

        frameHOV <- ttklabelframe(subfr2, text = "Validation data", relief = 'groove')

        .cdtData$EnvData$hovd <- tclVar(0)
        file.hovd <- tclVar()

        stateHOVd <- if(tclvalue(.cdtData$EnvData$hovd) == "1") "normal" else "disabled"

        chk.hovd <- tkcheckbutton(frameHOV, variable = .cdtData$EnvData$hovd, text = "Validation already performed", anchor = 'w', justify = 'left')
        en.hovd <- tkentry(frameHOV, textvariable = file.hovd, width = largeur1 + 7, state = stateHOVd)
        bt.hovd <- tkbutton(frameHOV, text = .cdtEnv$tcl$lang$global[['button']][['6']], state = stateHOVd)

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
                    Insert.Messages.Out('Unable to load Validation data', format = TRUE)
                    Insert.Messages.Out(gsub('[\r\n]', '', hovd.data[1]), format = TRUE)
                    return(NULL)
                }

                .cdtData$EnvData$file.hovd <- str_trim(tclvalue(file.hovd))
                .cdtData$EnvData$GeneralParameters <- hovd.data$GeneralParameters
                .cdtData$EnvData$cdtData <- hovd.data$cdtData
                .cdtData$EnvData$VALID.names <- hovd.data$VALID.names

                if(!is.null(hovd.data$opDATA)){
                    .cdtData$EnvData$opDATA <- hovd.data$opDATA
                    .cdtData$EnvData$Statistics <- hovd.data$Statistics
                }

                ###
                tclvalue(timeSteps) <- CbperiodVAL[periodVAL %in% hovd.data$GeneralParameters$intstep]

                ##
                AGGREGFUN <- c("mean", "sum", "count")
                if(tclvalue(aggr.data) == "1"){
                    if(tclvalue(timeSteps) != CbperiodVAL[1]){
                        AGGREGFUN <- AGGREGFUN[-3]
                        tclvalue(aggr.fun) <- if(tclvalue(aggr.fun) == "count") "sum" else tclvalue(aggr.fun)
                    }
                    stateo0a <- "readonly"
                }else stateo0a <- "disabled"
                tkconfigure(cb.aggfun, values = AGGREGFUN, state = stateo0a)

                if(!is.null(.cdtData$EnvData$opDATA$id)){
                    stateDispSTN <- if(str_trim(tclvalue(stat.data)) == STATDATATYPE[3]) 'normal' else 'disabled'
                    tkconfigure(cb.stat.sel, values = .cdtData$EnvData$opDATA$id, state = stateDispSTN)
                    tclvalue(stn.stat.tab) <- .cdtData$EnvData$opDATA$id[1]
                    tkconfigure(bt.stat.prev, state = stateDispSTN)
                    tkconfigure(bt.stat.next, state = stateDispSTN)

                    stateMaps <- if(str_trim(tclvalue(stat.data)) == STATDATATYPE[3]) 'normal' else 'disabled'
                    tkconfigure(cb.stats.maps, state = stateMaps)
                    tkconfigure(bt.stats.maps, state = stateMaps)
                    tkconfigure(cb.plot.type, state = stateMaps)
                    tkconfigure(bt.stats.Opt, state = stateMaps)

                    stateStnID <- if(str_trim(tclvalue(stat.data)) == STATDATATYPE[3]) 'normal' else 'disabled'

                    tkconfigure(cb.stn.graph, values = .cdtData$EnvData$opDATA$id, state = stateStnID)
                    tclvalue(.cdtData$EnvData$stnIDGraph) <- .cdtData$EnvData$opDATA$id[1]
                    tkconfigure(bt.stn.graph.prev, state = stateStnID)
                    tkconfigure(bt.stn.graph.next, state = stateStnID)

                    tkconfigure(cb.rank.sel, values = .cdtData$EnvData$opDATA$id, state = stateStnID)
                    tclvalue(.cdtData$EnvData$stnIDRank) <- .cdtData$EnvData$opDATA$id[1]
                    tkconfigure(bt.rank.prev, state = stateStnID)
                    tkconfigure(bt.rank.next, state = stateStnID)

                    TYPEGRAPH <- c("Scatter", "CDF", "Lines")
                    if(str_trim(tclvalue(stat.data)) == STATDATATYPE[1]){
                        TYPEGRAPH <- c("Scatter", "CDF")
                        if(tclvalue(.cdtData$EnvData$type.graph) == "Lines")
                            tclvalue(.cdtData$EnvData$type.graph) <- "Scatter"
                    }
                    tkconfigure(cb.stats.graph, values = TYPEGRAPH)
                }

                valdataExist <<- 1
            }
        })

        ###############

        tkbind(chk.hovd, "<Button-1>", function(){
            stateHOVd <- if(tclvalue(.cdtData$EnvData$hovd) == '1') 'disabled' else 'normal'
            tkconfigure(en.hovd, state = stateHOVd)
            tkconfigure(bt.hovd, state = stateHOVd)

            stateDataIn <- if(tclvalue(.cdtData$EnvData$hovd) == '1') 'normal' else 'disabled'
            tkconfigure(cb.stnfl, state = stateDataIn)
            tkconfigure(bt.stnfl, state = stateDataIn)
            tkconfigure(en.dir.save, state = stateDataIn)
            tkconfigure(bt.dir.save, state = stateDataIn)
            tkconfigure(cb.tstep, state = stateDataIn)
            tkconfigure(bt.AddData, state = stateDataIn)
        })

        ##############################################

        frameSeason <- ttklabelframe(subfr2, text = "Years & Season", relief = 'groove')

        mon1 <- as.numeric(str_trim(GeneralParameters$date.range$start.month))
        mon2 <- as.numeric(str_trim(GeneralParameters$date.range$end.month))
        start.mois <- tclVar(MOIS[mon1])
        end.mois <- tclVar(MOIS[mon2])
        start.year <- tclVar(GeneralParameters$date.range$start.year)
        end.year <- tclVar(GeneralParameters$date.range$end.year)

        fr.seas <- ttklabelframe(frameSeason, text = 'Season', relief = 'sunken', labelanchor = "n", borderwidth = 2)
        fr.year <- ttklabelframe(frameSeason, text = 'Years', relief = 'sunken', labelanchor = "n", borderwidth = 2)

        txt.to1 <- tklabel(fr.year, text = '-to-')
        en.years1 <- tkentry(fr.year, width = 5, textvariable = start.year, justify = 'right')
        en.years2 <- tkentry(fr.year, width = 5, textvariable = end.year, justify = 'right')

        txt.to2 <- tklabel(fr.seas, text = '-to-')
        cb.month1 <- ttkcombobox(fr.seas, values = MOIS, textvariable = start.mois, width = 4)
        cb.month2 <- ttkcombobox(fr.seas, values = MOIS, textvariable = end.mois, width = 4)

        tkgrid(en.years1, row = 0, column = 0, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1)
        tkgrid(txt.to1, row = 0, column = 1, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1)
        tkgrid(en.years2, row = 0, column = 2, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1)

        tkgrid(cb.month1, row = 0, column = 0, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1)
        tkgrid(txt.to2, row = 0, column = 1, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1)
        tkgrid(cb.month2, row = 0, column = 2, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1)

        tkgrid(fr.seas, row = 0, column = 0, sticky = 'ns', rowspan = 1, columnspan = 1, padx = 3, pady = 1)
        tkgrid(fr.year, row = 0, column = 1, sticky = 'ns', rowspan = 1, columnspan = 1, padx = 3, pady = 1)

        helpWidget(en.years1, 'Start year of the period to calculate the statistics', 'Start year of the period to calculate the statistics')
        helpWidget(en.years2, 'End year of the period to calculate the statistics', 'End year of the period to calculate the statistics')
        helpWidget(cb.month1, 'Start month of the period to calculate the statistics', 'Start month of the period to calculate the statistics')
        helpWidget(cb.month2, 'End month of the season to calculate the statistics', 'End month of the season to calculate the statistics')

        ##############################################

        frameAggr <- ttklabelframe(subfr2, text = "Data aggregation", relief = 'groove')

        aggr.data <- tclVar(GeneralParameters$aggr.series$aggr.data)
        aggr.fun <- tclVar(GeneralParameters$aggr.series$aggr.fun)
        # min.frac <- tclVar(GeneralParameters$aggr.series$min.frac)
        opr.fun <- tclVar(GeneralParameters$aggr.series$opr.fun)
        opr.thres <- tclVar(GeneralParameters$aggr.series$opr.thres)

        AGGREGFUN <- c("mean", "sum", "count")
        if(GeneralParameters$intstep != 'daily' & !GeneralParameters$aggr.series$aggr.data) AGGREGFUN <- AGGREGFUN[-3]
        if(!GeneralParameters$aggr.series$aggr.data){
            stateo0a <- 'disabled'
            stateo0b <- 'disabled'
            stateo1 <- 'disabled'
            stateo2 <- 'disabled'
        }else{
            stateo0a <- 'readonly'
            stateo0b <- 'normal'
            stateo1 <- if(str_trim(GeneralParameters$aggr.series$aggr.fun) == "count") 'readonly' else 'disabled'
            stateo2 <- if(str_trim(GeneralParameters$aggr.series$aggr.fun) == "count") 'normal' else 'disabled'
        }

        chk.aggrdata <- tkcheckbutton(frameAggr, variable = aggr.data, text = "Aggregate data", anchor = 'w', justify = 'left')
        txt.aggfun <- tklabel(frameAggr, text = 'Function', anchor = 'w', justify = 'left')
        cb.aggfun <- ttkcombobox(frameAggr, values = AGGREGFUN, textvariable = aggr.fun, width = 6, state = stateo0a)
        # txt.minfrac <- tklabel(frameAggr, text = 'Min.Frac', anchor = 'w', justify = 'left')
        # en.minfrac <- tkentry(frameAggr, textvariable = min.frac, width = 6, state = stateo0b)
        txt.opfun <- tklabel(frameAggr, text = 'Operator', anchor = 'w', justify = 'left')
        cb.opfun <- ttkcombobox(frameAggr, values = c(">=", ">", "<=", "<"), textvariable = opr.fun, width = 6, state = stateo1)
        txt.opthres <- tklabel(frameAggr, text = 'Threshold', anchor = 'w', justify = 'left')
        en.opthres <- tkentry(frameAggr, textvariable = opr.thres, width = 6, state = stateo2)

        tkgrid(chk.aggrdata, row = 0, column = 0, sticky = 'we', rowspan = 1, columnspan = 2, padx = 1, pady = 1, ipadx = 1, ipady = 1)
        tkgrid(txt.aggfun, row = 0, column = 2, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
        tkgrid(cb.aggfun, row = 0, column = 3, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
        # tkgrid(txt.minfrac, row = 1, column = 2, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
        # tkgrid(en.minfrac, row = 1, column = 3, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
        tkgrid(txt.opfun, row = 1, column = 0, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
        tkgrid(cb.opfun, row = 1, column = 1, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
        tkgrid(txt.opthres, row = 1, column = 2, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
        tkgrid(en.opthres, row = 1, column = 3, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)

        helpWidget(cb.aggfun, 'Function that have to be applied for aggregating from daily/dekadal/monthly into\na higher time step (e.g., for precipitation FUN=sum and for temperature FUN=mean)',
                              'Function that have to be applied for aggregating from daily/dekadal/monthly into\na higher time step (e.g., for precipitation FUN=sum and for temperature FUN=mean)')
        # infobulle(en.minfrac, 'Minimum fraction of available data that must be present within each output time step')
        # status.bar.display(en.minfrac, 'Minimum fraction of available data that must be present within each output time step')
        helpWidget(cb.opfun, 'Select the comparison operator to be used to match event', 'Select the comparison operator to be used to match event')
        helpWidget(en.opthres, 'User defined threshold applied to count event', 'User defined threshold applied to count event')

        #################

        tkbind(cb.aggfun, "<<ComboboxSelected>>", function(){
            stateo1 <- if(tclvalue(aggr.fun) == "count") "readonly" else "disabled"
            stateo2 <- if(tclvalue(aggr.fun) == "count") "normal" else "disabled"
            tkconfigure(cb.opfun, state = stateo1)
            tkconfigure(en.opthres, state = stateo2)
        })

        tkbind(chk.aggrdata, "<Button-1>", function(){
            if(tclvalue(aggr.data) == "1"){
                stateo0a <- 'disabled'
                stateo0b <- 'disabled'
                stateo1 <- 'disabled'
                stateo2 <- 'disabled'
            }else{
                stateo0a <- 'readonly'
                stateo0b <- 'normal'
                stateo1 <- if(tclvalue(aggr.fun) == "count") 'readonly' else 'disabled'
                stateo2 <- if(tclvalue(aggr.fun) == "count") 'normal' else 'disabled'
            }

            tkconfigure(cb.aggfun, state = stateo0a)
            # tkconfigure(en.minfrac, state = stateo0b)
            tkconfigure(cb.opfun, state = stateo1)
            tkconfigure(en.opthres, state = stateo2)
            tkconfigure(cb.stats.maps, values = CHXSTATS)
        })

        #############################

        STATDATATYPE <- c('All Data', 'Spatial Average', 'Per station')
        StatDataT <- c('all', 'avg', 'stn')
        stat.data <- tclVar()
        tclvalue(stat.data) <- STATDATATYPE[StatDataT %in% GeneralParameters$stat.data]

        cb.stat.data <- ttkcombobox(subfr2, values = STATDATATYPE, textvariable = stat.data, width = largeur0)

        helpWidget(cb.stat.data, 'Use all data or a spatial average or station by station to calculate the statistics', 'Use all data or a spatial average or station by station to calculate the statistics')

        #################

        tkbind(cb.stat.data, "<<ComboboxSelected>>", function(){
            stateDispSTN <- if(str_trim(tclvalue(stat.data)) == STATDATATYPE[3]) 'normal' else 'disabled'
            tkconfigure(bt.stat.prev, state = stateDispSTN)
            tkconfigure(cb.stat.sel, state = stateDispSTN)
            tkconfigure(bt.stat.next, state = stateDispSTN)

            stateMaps <- if(str_trim(tclvalue(stat.data)) == STATDATATYPE[3]) 'normal' else 'disabled'
            tkconfigure(cb.stats.maps, state = stateMaps)
            tkconfigure(bt.stats.maps, state = stateMaps)
            tkconfigure(cb.plot.type, state = stateMaps)
            tkconfigure(bt.stats.Opt, state = stateMaps)

            stateStnID <- if(str_trim(tclvalue(stat.data)) == STATDATATYPE[3]) 'normal' else 'disabled'
            tkconfigure(cb.stn.graph, state = stateStnID)
            tkconfigure(bt.stn.graph.prev, state = stateStnID)
            tkconfigure(bt.stn.graph.next, state = stateStnID)

            tkconfigure(cb.rank.sel, state = stateStnID)
            tkconfigure(bt.rank.prev, state = stateStnID)
            tkconfigure(bt.rank.next, state = stateStnID)

            TYPEGRAPH <- c("Scatter", "CDF", "Lines")
            if(str_trim(tclvalue(stat.data)) == STATDATATYPE[1]){
                TYPEGRAPH <- c("Scatter", "CDF")
                if(tclvalue(.cdtData$EnvData$type.graph) == "Lines")
                    tclvalue(.cdtData$EnvData$type.graph) <- "Scatter"
            }
            tkconfigure(cb.stats.graph, values = TYPEGRAPH)
        })

        ##############################################

        frameDicho <- ttklabelframe(subfr2, text = "Dichotomous validation", relief = 'groove')

        if(clim.var == 'RR') trhesVal <- 1
        if(clim.var == 'TT') trhesVal <- 20
        dicho.thres <- tclVar(trhesVal)
        # dicho.thres <- tclVar(GeneralParameters$dicho.fcst$opr.thres)
        dicho.opr <- tclVar(GeneralParameters$dicho.fcst$opr.fun)

        txt.dicho <- tklabel(frameDicho, text = 'Threshold', anchor = 'w', justify = 'left')
        cb.dicho <- ttkcombobox(frameDicho, values = c(">=", ">", "<=", "<"), textvariable = dicho.opr, width = 4, state = 'readonly')
        en.dicho <- tkentry(frameDicho, textvariable = dicho.thres, width = 6)

        tkgrid(txt.dicho, row = 0, column = 0, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
        tkgrid(cb.dicho, row = 0, column = 1, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
        tkgrid(en.dicho, row = 0, column = 2, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)

        helpWidget(en.dicho, 'Threshold to be specified to separate "yes" and "no" events', 'Threshold to be specified to separate "yes" and "no" events')

        ##############################################

        bt.calc.stat <- ttkbutton(subfr2, text = "Calculate Statistics")

        tkconfigure(bt.calc.stat, command = function(){
            Insert.Messages.Out("Validation .................", TRUE, "i")
            msg0 <- "Statistics calculation finished successfully"
            msg1 <- "Validation failed"

            parsInput <- getInputInfos()
            readInFiles <- parsInput$STN.file != "" & all(parsInput$VALID.files != "")
            getInputDATA <- FALSE
            if(is.null(valdataExist) & readInFiles) getInputDATA <- TRUE

            if(getInputDATA){
                tkconfigure(.cdtEnv$tcl$main$win, cursor = 'watch')
                tcl('update')

                # assign('GeneralParameters', parsInput, envir = .GlobalEnv)

                ret <- tryCatch(
                    {
                        read_ValidationMultipleData(parsInput)
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
                    Insert.Messages.Out(msg1, TRUE, "e")
                    return(NULL)
                }
            }

            ######################
            GeneralParameters$intstep <- periodVAL[CbperiodVAL %in% str_trim(tclvalue(timeSteps))]

            GeneralParameters$outdir <- str_trim(tclvalue(dir2save))
            GeneralParameters$STN.file <- str_trim(tclvalue(file.stnfl))

            ######################

            GeneralParameters$date.range$start.month <- which(MOIS %in% str_trim(tclvalue(start.mois)))
            GeneralParameters$date.range$end.month <- which(MOIS %in% str_trim(tclvalue(end.mois)))
            GeneralParameters$date.range$start.year <- as.numeric(str_trim(tclvalue(start.year)))
            GeneralParameters$date.range$end.year <- as.numeric(str_trim(tclvalue(end.year)))

            GeneralParameters$aggr.series$aggr.data <- switch(tclvalue(aggr.data), '0' = FALSE, '1' = TRUE)
            GeneralParameters$aggr.series$aggr.fun <- str_trim(tclvalue(aggr.fun))
            # GeneralParameters$aggr.series$min.frac <- as.numeric(str_trim(tclvalue(min.frac)))
            GeneralParameters$aggr.series$opr.fun <- str_trim(tclvalue(opr.fun))
            GeneralParameters$aggr.series$opr.thres <- as.numeric(str_trim(tclvalue(opr.thres)))

            GeneralParameters$stat.data <- StatDataT[STATDATATYPE %in% str_trim(tclvalue(stat.data))]

            GeneralParameters$dicho.fcst$opr.thres <- as.numeric(str_trim(tclvalue(dicho.thres)))
            GeneralParameters$dicho.fcst$opr.fun <- str_trim(tclvalue(dicho.opr))

            GeneralParameters$clim.var <- clim.var

            #####

            # assign('GeneralParameters', GeneralParameters, envir = .GlobalEnv)

            tkconfigure(.cdtEnv$tcl$main$win, cursor = 'watch')
            tcl('update')
            ret <- tryCatch(
                {
                    ValidationMultipleDataProcs(GeneralParameters)
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
                    Insert.Messages.Out(msg0, TRUE, "s")

                    if(str_trim(tclvalue(stat.data)) == STATDATATYPE[3]){
                        tkconfigure(cb.stat.sel, values = .cdtData$EnvData$opDATA$id)
                        tclvalue(stn.stat.tab) <- .cdtData$EnvData$opDATA$id[1]

                        tkconfigure(cb.stn.graph, values = .cdtData$EnvData$opDATA$id, state = 'normal')
                        tclvalue(.cdtData$EnvData$stnIDGraph) <- .cdtData$EnvData$opDATA$id[1]

                        tkconfigure(cb.rank.sel, values = .cdtData$EnvData$opDATA$id, state = 'normal')
                        tclvalue(.cdtData$EnvData$stnIDRank) <- .cdtData$EnvData$opDATA$id[1]
                    }
                }else Insert.Messages.Out(msg1, format = TRUE)
            }else Insert.Messages.Out(msg1, format = TRUE)
        })

        #############################
        tkgrid(frameHOV, row = 0, column = 0, sticky = 'we')
        tkgrid(frameSeason, row = 1, column = 0, sticky = 'we', pady = 1)
        tkgrid(frameAggr, row = 2, column = 0, sticky = 'we', pady = 1)
        tkgrid(cb.stat.data, row = 3, column = 0, sticky = 'we', pady = 3)
        tkgrid(frameDicho, row = 4, column = 0, sticky = '', pady = 3)
        tkgrid(bt.calc.stat, row = 5, column = 0, sticky = 'we', pady = 3)

    #######################################################################################################

    #Tab3
    subfr3 <- bwTabScrollableFrame(cmd.tab3)

    ##############################################

        frameStatTab <- ttklabelframe(subfr3, text = "Display Statistics Table", relief = 'groove')

        STATIONIDS <- ''
        stn.stat.tab <- tclVar()
        stateDispSTN <- if(GeneralParameters$stat.data == 'stn') 'normal' else 'disabled'

        bt.stat.disp <- ttkbutton(frameStatTab, text = "Display Table")
        bt.stat.prev <- ttkbutton(frameStatTab, text = "<<", state = stateDispSTN, width = 4)
        bt.stat.next <- ttkbutton(frameStatTab, text = ">>", state = stateDispSTN, width = 4)
        cb.stat.sel <- ttkcombobox(frameStatTab, values = STATIONIDS, textvariable = stn.stat.tab, width = largeur4, state = stateDispSTN,  justify = 'center')

        tkgrid(bt.stat.disp, row = 1, column = 0, sticky = 'we', rowspan = 1, columnspan = 5, padx = 1, pady = 1, ipadx = 1, ipady = 1)
        tkgrid(bt.stat.prev, row = 2, column = 0, sticky = 'e', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
        tkgrid(cb.stat.sel, row = 2, column = 1, sticky = 'we', rowspan = 1, columnspan = 3, padx = 1, pady = 1, ipadx = 1, ipady = 1)
        tkgrid(bt.stat.next, row = 2, column = 4, sticky = 'w', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)

        ################
        .cdtData$EnvData$tab$validStat <- NULL

        tkconfigure(bt.stat.disp, command = function(){
            if(!is.null(.cdtData$EnvData$Statistics)){
                volume.stats <- NULL
                descrp3 <- NULL

                if(str_trim(tclvalue(stat.data)) == STATDATATYPE[1]){
                    don <- .cdtData$EnvData$Statistics$ALL
                    cont.stats <- do.call(cbind, lapply(don$cont, "[[", "statistics"))
                    descrp1 <- don$cont[[1]]$description
                    catg.stats <- do.call(cbind, lapply(don$catg, "[[", "statistics"))
                    descrp2 <- don$catg[[1]]$description
                    if(clim.var == "RR"){
                        volume.stats <- do.call(cbind, lapply(don$volume, "[[", "statistics"))
                        descrp3 <- don$volume[[1]]$description
                    }

                    don <- rbind(cont.stats, catg.stats, volume.stats)
                    descrp <- c(descrp1, descrp2, descrp3)
                    dat2disp <- data.frame(Name = rownames(don), Statistics = don, Description = descrp)
                    names(dat2disp) <- c('Name', .cdtData$EnvData$VALID.names, 'Description')
                    rownames(dat2disp) <- NULL

                    titleTab <- 'All-Data Statistics'
                }
                if(str_trim(tclvalue(stat.data)) == STATDATATYPE[2]){
                    don <- .cdtData$EnvData$Statistics$AVG
                    cont.stats <- do.call(cbind, lapply(don$cont, "[[", "statistics"))
                    descrp1 <- don$cont[[1]]$description
                    catg.stats <- do.call(cbind, lapply(don$catg, "[[", "statistics"))
                    descrp2 <- don$catg[[1]]$description
                    if(clim.var == "RR"){
                        volume.stats <- do.call(cbind, lapply(don$volume, "[[", "statistics"))
                        descrp3 <- don$volume[[1]]$description
                    }

                    don <- rbind(cont.stats, catg.stats, volume.stats)
                    descrp <- c(descrp1, descrp2, descrp3)
                    dat2disp <- data.frame(Name = rownames(don), Statistics = don, Description = descrp)
                    names(dat2disp) <- c('Name', .cdtData$EnvData$VALID.names, 'Description')
                    rownames(dat2disp) <- NULL

                    titleTab <- 'Spatial-Average Statistics'
                }
                if(str_trim(tclvalue(stat.data)) == STATDATATYPE[3]){
                    istn <- which(.cdtData$EnvData$opDATA$id == str_trim(tclvalue(stn.stat.tab)))

                    don <- .cdtData$EnvData$Statistics$STN
                    stats <- lapply(seq_along(.cdtData$EnvData$VALID.names), function(j){
                        cont.stats <- don$cont[[j]]$statistics[, istn]
                        catg.stats <- don$catg[[j]]$statistics[, istn]
                        if(clim.var == "RR")
                            volume.stats <- don$volume[[j]]$statistics[, istn]

                        c(cont.stats, catg.stats, volume.stats)
                    })
                    stats <- do.call(cbind, stats)
                    descrp1 <- don$cont[[1]]$description
                    descrp2 <- don$catg[[1]]$description
                    if(clim.var == "RR")
                        descrp3 <- don$volume[[1]]$description
                    descrp <- c(descrp1, descrp2, descrp3)

                    dat2disp <- data.frame(Name = rownames(stats), Statistics = stats, Description = descrp)
                    names(dat2disp) <- c('Name', .cdtData$EnvData$VALID.names, 'Description')
                    rownames(dat2disp) <- NULL

                    titleTab <- paste(tclvalue(stn.stat.tab), 'Statistics')
                }

                .cdtData$EnvData$tab$validStat <- tableNotebookTab_unik(dat2disp, .cdtData$EnvData$tab$validStat, titleTab, 12)
            }
        })

        tkconfigure(bt.stat.prev, command = function(){
            if(!is.null(.cdtData$EnvData$Statistics)){
                istn <- which(.cdtData$EnvData$opDATA$id == str_trim(tclvalue(stn.stat.tab)))
                istn <- istn - 1
                if(istn < 1) istn <- length(.cdtData$EnvData$opDATA$id)
                tclvalue(stn.stat.tab) <- .cdtData$EnvData$opDATA$id[istn]

                don <- .cdtData$EnvData$Statistics$STN
                stats <- lapply(seq_along(.cdtData$EnvData$VALID.names), function(j){
                    cont.stats <- don$cont[[j]]$statistics[, istn]
                    catg.stats <- don$catg[[j]]$statistics[, istn]
                    if(clim.var == "RR")
                        volume.stats <- don$volume[[j]]$statistics[, istn]

                    c(cont.stats, catg.stats, volume.stats)
                })
                stats <- do.call(cbind, stats)
                descrp1 <- don$cont[[1]]$description
                descrp2 <- don$catg[[1]]$description
                if(clim.var == "RR")
                    descrp3 <- don$volume[[1]]$description
                descrp <- c(descrp1, descrp2, descrp3)

                dat2disp <- data.frame(Name = rownames(stats), Statistics = stats, Description = descrp)
                names(dat2disp) <- c('Name', .cdtData$EnvData$VALID.names, 'Description')
                rownames(dat2disp) <- NULL

                titleTab <- paste(tclvalue(stn.stat.tab), 'Statistics')

                .cdtData$EnvData$tab$validStat <- tableNotebookTab_unik(dat2disp, .cdtData$EnvData$tab$validStat, titleTab, 12)
            }
        })

        tkconfigure(bt.stat.next, command = function(){
            if(!is.null(.cdtData$EnvData$Statistics)){
                istn <- which(.cdtData$EnvData$opDATA$id == str_trim(tclvalue(stn.stat.tab)))
                istn <- istn + 1
                if(istn > length(.cdtData$EnvData$opDATA$id)) istn <- 1
                tclvalue(stn.stat.tab) <- .cdtData$EnvData$opDATA$id[istn]

                don <- .cdtData$EnvData$Statistics$STN
                stats <- lapply(seq_along(.cdtData$EnvData$VALID.names), function(j){
                    cont.stats <- don$cont[[j]]$statistics[, istn]
                    catg.stats <- don$catg[[j]]$statistics[, istn]
                    if(clim.var == "RR")
                        volume.stats <- don$volume[[j]]$statistics[, istn]

                    c(cont.stats, catg.stats, volume.stats)
                })
                stats <- do.call(cbind, stats)
                descrp1 <- don$cont[[1]]$description
                descrp2 <- don$catg[[1]]$description
                if(clim.var == "RR")
                    descrp3 <- don$volume[[1]]$description
                descrp <- c(descrp1, descrp2, descrp3)

                dat2disp <- data.frame(Name = rownames(stats), Statistics = stats, Description = descrp)
                names(dat2disp) <- c('Name', .cdtData$EnvData$VALID.names, 'Description')
                rownames(dat2disp) <- NULL

                titleTab <- paste(tclvalue(stn.stat.tab), 'Statistics')

                .cdtData$EnvData$tab$validStat <- tableNotebookTab_unik(dat2disp, .cdtData$EnvData$tab$validStat, titleTab, 12)
            }
        })

        ##############################################

        frameMap <- ttklabelframe(subfr3, text = "Statistics Maps", relief = 'groove')

        .cdtData$EnvData$statistics <- tclVar('Correlation')
        typeMapPLOT <- c("Points", "Pixels")
        .cdtData$EnvData$typeMap <- tclVar("Points")

        stateMaps <- if(GeneralParameters$stat.data == 'stn') 'normal' else 'disabled'

        cb.stats.maps <- ttkcombobox(frameMap, values = CHXSTATS, textvariable = .cdtData$EnvData$statistics, width = largeur5, state = stateMaps)
        bt.stats.maps <- ttkbutton(frameMap, text = .cdtEnv$tcl$lang$global[['button']][['3']], state = stateMaps)
        bt.stats.Opt <- ttkbutton(frameMap, text = .cdtEnv$tcl$lang$global[['button']][['4']], state = stateMaps)
        txt.plot.type <- tklabel(frameMap, text = "Plot Type", anchor = "e", justify = "right")
        cb.plot.type <- ttkcombobox(frameMap, values = typeMapPLOT, textvariable = .cdtData$EnvData$typeMap, width = 5, state = stateMaps)

        tkgrid(cb.stats.maps, row = 0, column = 0, sticky = 'we', rowspan = 1, columnspan = 3, padx = 1, pady = 2, ipadx = 1, ipady = 1)
        tkgrid(bt.stats.maps, row = 0, column = 3, sticky = 'we', rowspan = 1, columnspan = 2, padx = 1, pady = 2, ipadx = 1, ipady = 1)
        tkgrid(txt.plot.type, row = 1, column = 0, sticky = 'e', rowspan = 1, columnspan = 2, padx = 1, pady = 2, ipadx = 1, ipady = 1)
        tkgrid(cb.plot.type, row = 1, column = 2, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 2, ipadx = 1, ipady = 1)
        tkgrid(bt.stats.Opt, row = 1, column = 3, sticky = 'we', rowspan = 1, columnspan = 2, padx = 1, pady = 2, ipadx = 1, ipady = 1)

        ##############

        tkconfigure(bt.stats.Opt, command = function(){
            if(!is.null(.cdtData$EnvData$Statistics)){
                mapstat <- str_trim(tclvalue(.cdtData$EnvData$statistics))
                istat <- sapply(lapply(.cdtData$EnvData$Statistics$STN, '[[', 1), function(x){
                    ll <- which(x$description == mapstat)
                    if(length(ll)) ll else 0
                })
                ix <- which(istat != 0)
                don <- lapply(.cdtData$EnvData$Statistics$STN[[ix]], function(x) x$statistics[istat[ix], ])
                don <- do.call(c, don)
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

        ##############

        .cdtData$EnvData$tab$Maps <- NULL

        tkconfigure(bt.stats.maps, command = function(){
            if(!is.null(.cdtData$EnvData$Statistics)){
                imgContainer <- CDT.Display.Graph(multiValidation.plotStatMaps, .cdtData$EnvData$tab$Maps, 'Statistics-Maps')
                .cdtData$EnvData$tab$Maps <- imageNotebookTab_unik(imgContainer, .cdtData$EnvData$tab$Maps)
            }
        })

        ##############################################

        frameGraph <- ttklabelframe(subfr3, text = "Graphs", relief = 'groove')

        TYPEGRAPH <- c("Scatter", "CDF", 'Lines')
        if(GeneralParameters$stat.data == 'all') TYPEGRAPH <- c("Scatter", "CDF")

        .cdtData$EnvData$type.graph <- tclVar("Scatter")
        STNIDGRAPH <- ""
        .cdtData$EnvData$stnIDGraph <- tclVar()
        stateStnID <- "disabled"

        cb.stats.graph <- ttkcombobox(frameGraph, values = TYPEGRAPH, textvariable = .cdtData$EnvData$type.graph, width = largeur5)
        bt.stats.graph <- ttkbutton(frameGraph, text = .cdtEnv$tcl$lang$global[['button']][['3']])
        cb.stn.graph <- ttkcombobox(frameGraph, values = STNIDGRAPH, textvariable = .cdtData$EnvData$stnIDGraph, width = largeur4, state = stateStnID, justify = 'center')
        bt.stn.graph.prev <- ttkbutton(frameGraph, text = "<<", state = stateStnID, width = 4)
        bt.stn.graph.next <- ttkbutton(frameGraph, text = ">>", state = stateStnID, width = 4)

        tkgrid(cb.stats.graph, row = 0, column = 0, sticky = 'we', rowspan = 1, columnspan = 12, padx = 1, pady = 2, ipadx = 1, ipady = 1)
        tkgrid(bt.stats.graph, row = 0, column = 12, sticky = 'we', rowspan = 1, columnspan = 6, padx = 1, pady = 2, ipadx = 1, ipady = 1)
        tkgrid(bt.stn.graph.prev, row = 1, column = 0, sticky = 'we', rowspan = 1, columnspan = 3, padx = 1, pady = 2, ipadx = 1, ipady = 1)
        tkgrid(cb.stn.graph, row = 1, column = 3, sticky = 'we', rowspan = 1, columnspan = 12, padx = 1, pady = 2, ipadx = 1, ipady = 1)
        tkgrid(bt.stn.graph.next, row = 1, column = 15, sticky = 'we', rowspan = 1, columnspan = 3, padx = 1, pady = 2, ipadx = 1, ipady = 1)

        ##############

        .cdtData$EnvData$tab$Graph <- NULL

        tkconfigure(bt.stats.graph, command = function(){
            if(!is.null(.cdtData$EnvData$opDATA$stnStatData)){
                imgContainer <- CDT.Display.Graph(multiValidation.plotGraph, .cdtData$EnvData$tab$Graph, 'Validation-Plot')
                .cdtData$EnvData$tab$Graph <- imageNotebookTab_unik(imgContainer, .cdtData$EnvData$tab$Graph)
            }
        })

        tkconfigure(bt.stn.graph.prev, command = function(){
            if(!is.null(.cdtData$EnvData$opDATA$stnStatData)){
                istn <- which(.cdtData$EnvData$opDATA$id == str_trim(tclvalue(.cdtData$EnvData$stnIDGraph)))
                istn <- istn - 1
                if(istn < 1) istn <- length(.cdtData$EnvData$opDATA$id)
                tclvalue(.cdtData$EnvData$stnIDGraph) <- .cdtData$EnvData$opDATA$id[istn]

                imgContainer <- CDT.Display.Graph(multiValidation.plotGraph, .cdtData$EnvData$tab$Graph, 'Validation-Plot')
                .cdtData$EnvData$tab$Graph <- imageNotebookTab_unik(imgContainer, .cdtData$EnvData$tab$Graph)
            }
        })

        tkconfigure(bt.stn.graph.next, command = function(){
            if(!is.null(.cdtData$EnvData$opDATA$stnStatData)){
                istn <- which(.cdtData$EnvData$opDATA$id == str_trim(tclvalue(.cdtData$EnvData$stnIDGraph)))
                istn <- istn + 1
                if(istn > length(.cdtData$EnvData$opDATA$id)) istn <- 1
                tclvalue(.cdtData$EnvData$stnIDGraph) <- .cdtData$EnvData$opDATA$id[istn]

                imgContainer <- CDT.Display.Graph(multiValidation.plotGraph, .cdtData$EnvData$tab$Graph, 'Validation-Plot')
                .cdtData$EnvData$tab$Graph <- imageNotebookTab_unik(imgContainer, .cdtData$EnvData$tab$Graph)
            }
        })

        ##############################################

        framePerform <- ttklabelframe(subfr3, text = "Performance Map", relief = 'groove')

        STNIDRANK <- ""
        .cdtData$EnvData$stnIDRank <- tclVar()

        bt.rank.disp <- ttkbutton(framePerform, text = "Display Rank")
        bt.rank.prev <- ttkbutton(framePerform, text = "<<", state = stateDispSTN, width = 4)
        bt.rank.next <- ttkbutton(framePerform, text = ">>", state = stateDispSTN, width = 4)
        cb.rank.sel <- ttkcombobox(framePerform, values = STNIDRANK, textvariable = .cdtData$EnvData$stnIDRank, width = largeur4, state = stateDispSTN,  justify = 'center')

        tkgrid(bt.rank.disp, row = 1, column = 0, sticky = 'we', rowspan = 1, columnspan = 5, padx = 1, pady = 1, ipadx = 1, ipady = 1)
        tkgrid(bt.rank.prev, row = 2, column = 0, sticky = 'e', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
        tkgrid(cb.rank.sel, row = 2, column = 1, sticky = 'we', rowspan = 1, columnspan = 3, padx = 1, pady = 1, ipadx = 1, ipady = 1)
        tkgrid(bt.rank.next, row = 2, column = 4, sticky = 'w', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)

        ##############

        .cdtData$EnvData$tab$Performap <- NULL

        tkconfigure(bt.rank.disp, command = function(){
            if(!is.null(.cdtData$EnvData$Statistics)){
                imgContainer <- CDT.Display.Graph(multiValidation.plotRank, .cdtData$EnvData$tab$Performap, 'Validation-Plot')
                .cdtData$EnvData$tab$Performap <- imageNotebookTab_unik(imgContainer, .cdtData$EnvData$tab$Performap)
            }
        })

        tkconfigure(bt.rank.prev, command = function(){
            if(!is.null(.cdtData$EnvData$Statistics)){
                istn <- which(.cdtData$EnvData$opDATA$id == str_trim(tclvalue(.cdtData$EnvData$stnIDRank)))
                istn <- istn - 1
                if(istn < 1) istn <- length(.cdtData$EnvData$opDATA$id)
                tclvalue(.cdtData$EnvData$stnIDRank) <- .cdtData$EnvData$opDATA$id[istn]

                imgContainer <- CDT.Display.Graph(multiValidation.plotRank, .cdtData$EnvData$tab$Performap, 'Validation-Plot')
                .cdtData$EnvData$tab$Performap <- imageNotebookTab_unik(imgContainer, .cdtData$EnvData$tab$Performap)
            }
        })

        tkconfigure(bt.rank.next, command = function(){
            if(!is.null(.cdtData$EnvData$Statistics)){
                istn <- which(.cdtData$EnvData$opDATA$id == str_trim(tclvalue(.cdtData$EnvData$stnIDRank)))
                istn <- istn + 1
                if(istn > length(.cdtData$EnvData$opDATA$id)) istn <- 1
                tclvalue(.cdtData$EnvData$stnIDRank) <- .cdtData$EnvData$opDATA$id[istn]

                imgContainer <- CDT.Display.Graph(multiValidation.plotRank, .cdtData$EnvData$tab$Performap, 'Validation-Plot')
                .cdtData$EnvData$tab$Performap <- imageNotebookTab_unik(imgContainer, .cdtData$EnvData$tab$Performap)
            }
        })

        #############################
        tkgrid(frameStatTab, row = 0, column = 0, sticky = 'we')
        tkgrid(frameMap, row = 1, column = 0, sticky = 'we', pady = 3)
        tkgrid(frameGraph, row = 2, column = 0, sticky = 'we', pady = 1)
        tkgrid(framePerform, row = 3, column = 0, sticky = 'we', pady = 1)

    #######################################################################################################

    #Tab4
    subfr4 <- bwTabScrollableFrame(cmd.tab4)

    ##############################################

        frameSHP <- ttklabelframe(subfr4, text = "Boundaries", relief = 'groove')

        .cdtData$EnvData$add.shp <- tclVar(GeneralParameters$add.to.plot$add.shp)
        file.plotShp <- tclVar(GeneralParameters$add.to.plot$shp.file)

        stateSHP <- if(GeneralParameters$add.to.plot$add.shp) "normal" else "disabled"

        chk.addshp <- tkcheckbutton(frameSHP, variable = .cdtData$EnvData$add.shp, text = "Add boundaries to Map", anchor = 'w', justify = 'left')
        cb.addshp <- ttkcombobox(frameSHP, values = unlist(listOpenFiles), textvariable = file.plotShp, width = largeur1, state = stateSHP)
        bt.addshp <- tkbutton(frameSHP, text = "...", state = stateSHP)

        tkgrid(chk.addshp, row = 0, column = 0, sticky = 'we', rowspan = 1, columnspan = 5, padx = 1, pady = 1, ipadx = 1, ipady = 1)
        tkgrid(cb.addshp, row = 1, column = 0, sticky = 'we', rowspan = 1, columnspan = 4, padx = 1, pady = 1, ipadx = 1, ipady = 1)
        tkgrid(bt.addshp, row = 1, column = 4, sticky = 'w', rowspan = 1, columnspan = 1, padx = 0, pady = 1, ipadx = 1, ipady = 1)

        ########
        tkconfigure(bt.addshp, command = function(){
            shp.opfiles <- getOpenShp(.cdtEnv$tcl$main$win)
            if(!is.null(shp.opfiles)){
                update.OpenFiles('shp', shp.opfiles)
                tclvalue(file.plotShp) <- shp.opfiles[[1]]
                listOpenFiles[[length(listOpenFiles) + 1]] <<- shp.opfiles[[1]]
                listOpenFiles <- openFile_ttkcomboList()
                # lapply(list(cb.stnfl, cb.valid, cb.adddem, cb.addshp), tkconfigure, values = unlist(listOpenFiles))

                shpofile <- getShpOpenData(file.plotShp)
                if(is.null(shpofile))
                    .cdtData$EnvData$shp <- NULL
                else
                    .cdtData$EnvData$shp <- getBoundaries(shpofile[[2]])
            }
        })

        #################

        tkbind(cb.addshp, "<<ComboboxSelected>>", function(){
            shpofile <- getShpOpenData(file.plotShp)
            if(is.null(shpofile))
                .cdtData$EnvData$shp <- NULL
            else
                .cdtData$EnvData$shp <- getBoundaries(shpofile[[2]])
        })

        tkbind(chk.addshp, "<Button-1>", function(){
            stateSHP <- if(tclvalue(.cdtData$EnvData$add.shp) == "1") "disabled" else "normal"
            tkconfigure(cb.addshp, state = stateSHP)
            tkconfigure(bt.addshp, state = stateSHP)
        })

        #############################

        tkgrid(frameSHP, row = 0, column = 0, sticky = 'we', pady = 1)

    #######################################################################################################

    getInputInfos <- function(){
        GeneralParameters$intstep <- periodVAL[CbperiodVAL %in% str_trim(tclvalue(timeSteps))]
        GeneralParameters$STN.file <- str_trim(tclvalue(file.stnfl))
        GeneralParameters$VALID.files <- sapply(.cdtData$GalParams$DATASETs, function(don) str_trim(tclvalue(don$tcl$input.file)))
        GeneralParameters$VALID.names <- sapply(.cdtData$GalParams$DATASETs, function(don) str_trim(tclvalue(don$tcl$data.name)))
        GeneralParameters$outdir <- str_trim(tclvalue(dir2save))

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

    tkbind(.cdtEnv$tcl$main$cmd.frame, "<Destroy>", function(){
        .cdtData$GalParams$DATASETs <- NULL
    })

    invisible()
}
