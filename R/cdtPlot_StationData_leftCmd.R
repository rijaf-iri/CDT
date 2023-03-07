
PlotCDTStationCmd <- function(){
    listOpenFiles <- openFile_ttkcomboList()
    if(WindowsOS()){
        largeur0 <- 22
        largeur1 <- 32
        largeur3 <- 14
        largeur4 <- 25
        largeur5 <- 12
        largeur6 <- 30
    }else{
        largeur0 <- 23
        largeur1 <- 33
        largeur3 <- 14
        largeur4 <- 25
        largeur5 <- 12
        largeur6 <- 30
    }

    ###################

    GeneralParameters <- list(intstep = "dekadal", minhour = 1,
                              cdtstation = "",
                              date = list(year = 2021, mon = 1, day = 1,
                                          hour = 1, min = 0, other = ""))
    pointSizeI <- 1.0
    .cdtData$EnvData$dataMapOp <- list(presetCol = list(color = 'tim.colors', reverse = FALSE),
                                       userCol = list(custom = FALSE, color = NULL),
                                       userLvl = list(custom = FALSE, levels = NULL, equidist = FALSE),
                                       title = list(user = FALSE, title = ''),
                                       colkeyLab = list(user = FALSE, label = ''),
                                       scalebar = list(add = FALSE, pos = 'bottomleft'),
                                       pointSize = pointSizeI, bbox = .cdtData$Config$region)

    .cdtData$EnvData$TSGraphOp <- list(
                                    bar = list(
                                            xlim = list(is.min = FALSE, min = "1981-01-1", is.max = FALSE, max = "2021-12-3"),
                                            ylim = list(is.min = FALSE, min = 0, is.max = FALSE, max = 200),
                                            axislabs = list(is.xlab = FALSE, xlab = '', is.ylab = FALSE, ylab = ''),
                                            title = list(is.title = FALSE, title = '', position = 'top'),
                                            colors = list(col = "darkblue")
                                        ),
                                    line = list(
                                            xlim = list(is.min = FALSE, min = "1981-01-1", is.max = FALSE, max = "2021-12-3"),
                                            ylim = list(is.min = FALSE, min = 0, is.max = FALSE, max = 200),
                                            axislabs = list(is.xlab = FALSE, xlab = '', is.ylab = FALSE, ylab = ''),
                                            title = list(is.title = FALSE, title = '', position = 'top'),
                                            plot = list(type = 'both',
                                                        col = list(line = "red", points = "blue"),
                                                        lwd = 2, cex = 1.4),
                                            legend = NULL)
                                        )

    .cdtData$EnvData$SHPOp <- list(col = "black", lwd = 1.5)

    .cdtData$EnvData$plot.maps$data.type <- "cdtstation"

    ###################

    xml.dlg <- file.path(.cdtDir$dirLocal, "languages", "cdtPlot_StationData_leftCmd.xml")
    lang.dlg <- cdtLanguageParse(xml.dlg, .cdtData$Config$lang.iso)
    .cdtData$EnvData$message <- lang.dlg[['message']]

    ###################

    .cdtEnv$tcl$main$cmd.frame <- tkframe(.cdtEnv$tcl$main$panel.left)

    tknote.cmd <- bwNoteBook(.cdtEnv$tcl$main$cmd.frame)

    cmd.tab1 <- bwAddTab(tknote.cmd, text = lang.dlg[['tab_title']][['1']])
    cmd.tab2 <- bwAddTab(tknote.cmd, text = lang.dlg[['tab_title']][['2']])

    bwRaiseTab(tknote.cmd, cmd.tab1)

    tkgrid.columnconfigure(cmd.tab1, 0, weight = 1)
    tkgrid.columnconfigure(cmd.tab2, 0, weight = 1)

    tkgrid.rowconfigure(cmd.tab1, 0, weight = 1)
    tkgrid.rowconfigure(cmd.tab2, 0, weight = 1)

    #######################################################################################################

    date.time.selection <- function(intstep, frTS1){
        if(intstep == 'others'){
            txt.other <- tklabel(frTS1, text = lang.dlg[['label']][['5']])
            cb.other <<- ttkcombobox(frTS1, values = "", textvariable = date.other, justify = "center", width = largeur6)

            tkgrid(txt.other, row = 0, column = 0, sticky = 'we', pady = 1, padx = 1)
            tkgrid(cb.other, row = 1, column = 0, sticky = 'we', pady = 1, padx = 1)
        }else{
            txtdek <- switch(intstep,
                             'dekadal' = lang.dlg[['label']][['8']],
                             'pentad'  = lang.dlg[['label']][['9']],
                                         lang.dlg[['label']][['10']])
            day.txtVar <- tclVar(txtdek)

            stateday <- if(intstep == 'monthly') 'disabled' else 'normal'
            statehour <- if(intstep %in% c('minute', 'hourly')) 'normal' else 'disabled'
            statemin <- if(intstep == 'minute') 'normal' else 'disabled'

            txt.yrs <- tklabel(frTS1, text = lang.dlg[['label']][['6']])
            txt.mon <- tklabel(frTS1, text = lang.dlg[['label']][['7']])
            txt.day <- tklabel(frTS1, text = tclvalue(day.txtVar), textvariable = day.txtVar)
            txt.hrs <- tklabel(frTS1, text = lang.dlg[['label']][['11']])
            txt.min <- tklabel(frTS1, text = lang.dlg[['label']][['12']])

            en.yrs <- tkentry(frTS1, width = 5, textvariable = date.year, justify = "center")
            en.mon <- tkentry(frTS1, width = 5, textvariable = date.mon, justify = "center")
            en.day <- tkentry(frTS1, width = 5, textvariable = date.day, justify = "center", state = stateday)
            en.hrs <- tkentry(frTS1, width = 5, textvariable = date.hour, justify = "center", state = statehour)
            en.min <- tkentry(frTS1, width = 5, textvariable = date.min, justify = "center", state = statemin)

            ##############
            tkgrid(txt.yrs, row = 0, column = 0, sticky = 'we', pady = 1, padx = 1)
            tkgrid(txt.mon, row = 0, column = 1, sticky = 'we', pady = 1, padx = 1)
            tkgrid(txt.day, row = 0, column = 2, sticky = 'we', pady = 1, padx = 1)
            tkgrid(txt.hrs, row = 0, column = 3, sticky = 'we', pady = 1, padx = 1)
            tkgrid(txt.min, row = 0, column = 4, sticky = 'we', pady = 1, padx = 1)

            tkgrid(en.yrs, row = 1, column = 0, sticky = 'we', pady = 1, padx = 1)
            tkgrid(en.mon, row = 1, column = 1, sticky = 'we', pady = 1, padx = 1)
            tkgrid(en.day, row = 1, column = 2, sticky = 'we', pady = 1, padx = 1)
            tkgrid(en.hrs, row = 1, column = 3, sticky = 'we', pady = 1, padx = 1)
            tkgrid(en.min, row = 1, column = 4, sticky = 'we', pady = 1, padx = 1)
        }
    }

    format.dates.times <- function(intstep, yrs, mon, dpk, hrs, min){
        switch(intstep,
               "minute" = paste(yrs, mon, dpk, hrs, min, sep = '-'),
               "hourly" = paste(yrs, mon, dpk, hrs, sep = '-'),
               "daily" = paste(yrs, mon, dpk, sep = '-'),
               "pentad" = local({
                                if(is.na(dpk) | dpk < 1 | dpk > 6){
                                    msg <- lang.dlg[['message']][['1']]
                                    Insert.Messages.Out(msg, TRUE, "e")
                                    return(NULL)
                                }
                                paste(yrs, mon, dpk, sep = '-')
                            }),
               "dekadal" = local({
                                if(is.na(dpk) | dpk < 1 | dpk > 3){
                                    msg <- lang.dlg[['message']][['2']]
                                    Insert.Messages.Out(msg, TRUE, "e")
                                    return(NULL)
                                }
                                paste(yrs, mon, dpk, sep = '-')
                            }),
               "monthly" = paste(yrs, mon, 1, sep = '-')
            )
    }

    set.dates.times <- function(incr, intstep){
        yrs <- as.numeric(str_trim(tclvalue(date.year)))
        mon <- as.numeric(str_trim(tclvalue(date.mon)))
        dpk <- as.numeric(str_trim(tclvalue(date.day)))
        hrs <- as.numeric(str_trim(tclvalue(date.hour)))
        min <- as.numeric(str_trim(tclvalue(date.min)))

        todaty <- format.dates.times(intstep, yrs, mon, dpk, hrs, min)
        if(is.null(todaty)) return(NULL)

        daty <- try(switch(intstep,
                    "minute" = as.POSIXct(todaty, format = "%Y-%m-%d-%H-%M"),
                    "hourly" = as.POSIXct(todaty, format = "%Y-%m-%d-%H"),
                               as.Date(todaty)
                    ), silent = TRUE)

        if(inherits(daty, "try-error") | is.na(daty)){
            msg <- paste(lang.dlg[['message']][['5']], todaty)
            Insert.Messages.Out(msg, TRUE, "e")
            return(NULL)
        }

        minhour <- as.numeric(str_trim(tclvalue(minhour.tclVar)))
        daty <- switch(intstep,
                       "minute" = daty + incr * minhour * 60,
                       "hourly" = daty + incr * minhour * 3600,
                       "daily" = daty + incr,
                       "pentad" = addPentads(daty, incr),
                       "dekadal" = addDekads(daty, incr),
                       "monthly" = addMonths(daty, incr)
                    )

        return(daty)
    }

    #######################################################################################################

    #Tab1
    subfr1 <- bwTabScrollableFrame(cmd.tab1)

        #######################

        frameCDTdata <- ttklabelframe(subfr1, text = lang.dlg[['label']][['1']], relief = 'groove')

        timeSteps <- tclVar()
        CbperiodVAL <- .cdtEnv$tcl$lang$global[['combobox']][['1']][c(1:6, 10)]
        periodVAL <- c('minute', 'hourly', 'daily', 'pentad', 'dekadal', 'monthly', 'others')
        tclvalue(timeSteps) <- CbperiodVAL[periodVAL %in% GeneralParameters$intstep]

        input.file <- tclVar(GeneralParameters$cdtstation)
        retminhr <- set.hour.minute(GeneralParameters$intstep, GeneralParameters$minhour)
        minhour.tclVar <- tclVar(retminhr$val)

        ############

        txt.cdtdata1 <- tklabel(frameCDTdata, text = lang.dlg[['label']][['2']], anchor = 'w', justify = 'left')
        cb.cdtdata1 <- ttkcombobox(frameCDTdata, values = CbperiodVAL, textvariable = timeSteps, width = largeur0)
        cb.minhour <- ttkcombobox(frameCDTdata, values = retminhr$cb, textvariable = minhour.tclVar, state = retminhr$state, width = 2, justify = 'center')

        txt.cdtdata2 <- tklabel(frameCDTdata, text = lang.dlg[['label']][['3']], anchor = 'w', justify = 'left')
        cb.cdtdata2 <- ttkcombobox(frameCDTdata, values = unlist(listOpenFiles), textvariable = input.file, width = largeur1)
        bt.cdtdata <- tkbutton(frameCDTdata, text = "...")

        ############

        tkgrid(txt.cdtdata1, row = 0, column = 0, sticky = 'we', rowspan = 1, columnspan = 2, padx = 1, pady = 1, ipadx = 1, ipady = 1)
        tkgrid(cb.cdtdata1, row = 0, column = 2, sticky = 'we', rowspan = 1, columnspan = 6, padx = 1, pady = 1, ipadx = 1, ipady = 1)
        tkgrid(cb.minhour, row = 0, column = 8, sticky = 'we', rowspan = 1, columnspan = 1, padx = 0, pady = 1, ipadx = 1, ipady = 1)

        tkgrid(txt.cdtdata2, row = 1, column = 0, sticky = 'we', rowspan = 1, columnspan = 9, padx = 1, pady = 1, ipadx = 1, ipady = 1)
        tkgrid(cb.cdtdata2, row = 2, column = 0, sticky = 'we', rowspan = 1, columnspan = 8, padx = 0, pady = 1, ipadx = 1, ipady = 1)
        tkgrid(bt.cdtdata, row = 2, column = 8, sticky = 'we', rowspan = 1, columnspan = 1, padx = 0, pady = 1, ipadx = 1, ipady = 1)

        ############

        tkconfigure(bt.cdtdata, command = function(){
            dat.opfiles <- getOpenFiles(.cdtEnv$tcl$main$win)
            if(!is.null(dat.opfiles)){
                update.OpenFiles('ascii', dat.opfiles)
                listOpenFiles[[length(listOpenFiles) + 1]] <<- dat.opfiles[[1]]
                tclvalue(input.file) <- dat.opfiles[[1]]
                tkconfigure(cb.cdtdata2, values = unlist(listOpenFiles))

                ret <- try(splitStnData(), silent = TRUE)
                if(inherits(ret, "try-error") | is.null(ret)){
                    Insert.Messages.Out(gsub('[\r\n]', '', ret[1]), TRUE, "e")
                    tclvalue(input.file) <- ""
                    .cdtData$EnvData$don <- NULL
                    return(NULL)
                }
            }else{
                tclvalue(input.file) <- ""
                .cdtData$EnvData$don <- NULL
                return(NULL)
            }
        })

        ############

        tkbind(cb.cdtdata1, "<<ComboboxSelected>>", function(){
            tkdestroy(frTS1)
            frTS1 <<- tkframe(frTS0)

            intstep <- periodVAL[CbperiodVAL %in% str_trim(tclvalue(timeSteps))]
            date.time.selection(intstep, frTS1)

            tkgrid(frTS1, row = 0, column = 1, sticky = 'we', pady = 1, rowspan = 2, columnspan = 1)

            ##############
            minhour <- as.numeric(str_trim(tclvalue(minhour.tclVar)))
            retminhr <- set.hour.minute(intstep, minhour)
            tkconfigure(cb.minhour, values = retminhr$cb, state = retminhr$state)
            tclvalue(minhour.tclVar) <- retminhr$val
        })

        tkbind(cb.cdtdata2, "<<ComboboxSelected>>", function(){
            ret <- try(splitStnData(), silent = TRUE)
            if(inherits(ret, "try-error") | is.null(ret)){
                Insert.Messages.Out(gsub('[\r\n]', '', ret[1]), TRUE, "e")
                tclvalue(input.file) <- ""
                .cdtData$EnvData$don <- NULL
                return(NULL)
            }
        })

        ##############################################

        frameMap <- ttklabelframe(subfr1, text = lang.dlg[['label']][['4']], relief = 'groove')

        typeMapPLOT <- c("Points", "Pixels")
        .cdtData$EnvData$map$typeMap <- tclVar("Points")

        date.year <- tclVar(GeneralParameters$date$year)
        date.mon <- tclVar(GeneralParameters$date$mon)
        date.day <- tclVar(GeneralParameters$date$day)
        date.hour <- tclVar(GeneralParameters$date$hour)
        date.min <- tclVar(GeneralParameters$date$min)
        date.other <- tclVar(GeneralParameters$date$other)

        ##############

        frTS0 <- tkframe(frameMap)
        frTS1 <- tkframe(frTS0)

        cb.other <- NULL
        date.time.selection(GeneralParameters$intstep, frTS1)

        bt.date.prev <- ttkbutton(frameMap, text = "<<", width = largeur5)
        bt.Map.plot <- ttkbutton(frameMap, text = .cdtEnv$tcl$lang$global[['button']][['3']], width = largeur5)
        bt.date.next <- ttkbutton(frameMap, text = ">>", width = largeur5)

        frOPTS0 <- tkframe(frameMap)
        cb.Map.type <- ttkcombobox(frOPTS0, values = typeMapPLOT, textvariable = .cdtData$EnvData$map$typeMap, justify = 'center', width = largeur3)
        bt.Map.Opt <- ttkbutton(frOPTS0, text = .cdtEnv$tcl$lang$global[['button']][['4']], width = largeur5)

        ##############

        tkgrid(frTS1, row = 0, column = 0, sticky = 'we', padx = 1, pady = 1, rowspan = 1, columnspan = 1)

        ##############

        tkgrid(cb.Map.type, row = 0, column = 0, sticky = 'we', padx = 1, pady = 1, columnspan = 1)
        tkgrid(bt.Map.Opt, row = 0, column = 1, sticky = 'we', padx = 1, pady = 1, columnspan = 1)

        ##############

        tkgrid(frTS0, row = 0, column = 0, sticky = '', padx = 1, pady = 1, columnspan = 3)
        tkgrid(bt.date.prev, row = 1, column = 0, sticky = 'we', padx = 1, pady = 1, columnspan = 1)
        tkgrid(bt.Map.plot, row = 1, column = 1, sticky = 'we', padx = 1, pady = 1, columnspan = 1)
        tkgrid(bt.date.next, row = 1, column = 2, sticky = 'we', padx = 1, pady = 1, columnspan = 1)
        tkgrid(frOPTS0, row = 2, column = 0, sticky = '', padx = 1, pady = 1, columnspan = 3)

        ##############

        tkconfigure(bt.Map.Opt, command = function(){
            if(!is.null(.cdtData$EnvData$stndata$map)){
                atlevel <- pretty(.cdtData$EnvData$stndata$map$z, n = 10, min.n = 7)
                if(is.null(.cdtData$EnvData$dataMapOp$userLvl$levels)){
                    .cdtData$EnvData$dataMapOp$userLvl$levels <- atlevel
                }else{
                    if(!.cdtData$EnvData$dataMapOp$userLvl$custom)
                        .cdtData$EnvData$dataMapOp$userLvl$levels <- atlevel
                }
            }
            .cdtData$EnvData$dataMapOp <- MapGraph.MapOptions(.cdtData$EnvData$dataMapOp)

            if(str_trim(tclvalue(.cdtData$EnvData$map$typeMap)) == "Points")
                pointSizeI <<- .cdtData$EnvData$dataMapOp$pointSize
        })

        ##############

        .cdtData$EnvData$tab$dataMap <- NULL

        tkconfigure(bt.Map.plot, command = function(){
            if(is.null(.cdtData$EnvData$don)) return(NULL)
            ret <- try(getStnMap(), silent = TRUE)
            if(inherits(ret, "try-error") | is.null(ret)){
                Insert.Messages.Out(gsub('[\r\n]', '', ret[1]), TRUE, "e")
                return(NULL)
            }

            ####
            CDTdataStation.Display.Maps()
        })

        tkconfigure(bt.date.prev, command = function(){
            if(is.null(.cdtData$EnvData$don)) return(NULL)
            intstep <- periodVAL[CbperiodVAL %in% str_trim(tclvalue(timeSteps))]

            if(intstep == "others"){
                idaty <- which(.cdtData$EnvData$don$dates == str_trim(tclvalue(date.other)))
                idaty <- idaty - 1
                if(idaty < 1) idaty <- length(.cdtData$EnvData$don$dates)
                tclvalue(date.other) <- .cdtData$EnvData$don$dates[idaty]
            }else{
                daty <- set.dates.times(-1, intstep)
                if(daty < .cdtData$EnvData$first.date) daty <- .cdtData$EnvData$last.date
                tclvalue(date.year) <- as.numeric(format(daty, '%Y'))
                tclvalue(date.mon) <- as.numeric(format(daty, '%m'))
                tclvalue(date.day) <- as.numeric(format(daty, '%d'))
                tclvalue(date.hour) <- as.numeric(format(daty, '%H'))
                tclvalue(date.min) <- as.numeric(format(daty, '%M'))
            }

            ######
            ret <- try(getStnMap(), silent = TRUE)
            if(inherits(ret, "try-error") | is.null(ret)){
                Insert.Messages.Out(gsub('[\r\n]', '', ret[1]), TRUE, "e")
                return(NULL)
            }

            ####
            CDTdataStation.Display.Maps()
        })

        tkconfigure(bt.date.next, command = function(){
            if(is.null(.cdtData$EnvData$don)) return(NULL) 
            intstep <- periodVAL[CbperiodVAL %in% str_trim(tclvalue(timeSteps))]

            if(intstep == "others"){
                idaty <- which(.cdtData$EnvData$don$dates == str_trim(tclvalue(date.other)))
                idaty <- idaty + 1
                if(idaty > length(.cdtData$EnvData$don$dates)) idaty <- 1
                tclvalue(date.other) <- .cdtData$EnvData$don$dates[idaty]
            }else{
                daty <- set.dates.times(1, intstep)
                if(daty > .cdtData$EnvData$last.date) daty <- .cdtData$EnvData$first.date
                tclvalue(date.year) <- as.numeric(format(daty, '%Y'))
                tclvalue(date.mon) <- as.numeric(format(daty, '%m'))
                tclvalue(date.day) <- as.numeric(format(daty, '%d'))
                tclvalue(date.hour) <- as.numeric(format(daty, '%H'))
                tclvalue(date.min) <- as.numeric(format(daty, '%M'))
            }

            ######
            ret <- try(getStnMap(), silent = TRUE)
            if(inherits(ret, "try-error") | is.null(ret)){
                Insert.Messages.Out(gsub('[\r\n]', '', ret[1]), TRUE, "e")
                return(NULL)
            }

            ####
            CDTdataStation.Display.Maps()
        })

        ##############

        tkbind(cb.Map.type, "<<ComboboxSelected>>", function(){
            if(str_trim(tclvalue(.cdtData$EnvData$map$typeMap)) == "Points"){
                .cdtData$EnvData$dataMapOp$pointSize <- pointSizeI
            }else .cdtData$EnvData$dataMapOp$pointSize <- NULL

            if(is.null(.cdtData$EnvData$don)) return(NULL)
            getStnMap()
        })

        ##############################################

        frameGraph <- ttklabelframe(subfr1, text = lang.dlg[['label']][['13']], relief = 'groove')

        CbtypeTSPLOT <- lang.dlg[['combobox']][['1']]
        typeTSPLOT <- c('line', 'bar')
        typeTSp <- tclVar(CbtypeTSPLOT[1])
        .cdtData$EnvData$plot.maps$typeTSp <- 'line'

        .cdtData$EnvData$plot.maps$stnIDTSp <- tclVar()

        ##############

        frTS2 <- tkframe(frameGraph)
        txt.stnID <- tklabel(frTS2, text = lang.dlg[['label']][['14']], anchor = 'w', justify = 'left')
        cb.stnID <- ttkcombobox(frTS2, values = "", textvariable = .cdtData$EnvData$plot.maps$stnIDTSp, width = largeur4, justify = 'center')

        bt.stnID.prev <- ttkbutton(frameGraph, text = "<<", width = largeur5)
        bt.TsGraph.plot <- ttkbutton(frameGraph, text = .cdtEnv$tcl$lang$global[['button']][['3']], width = largeur5)
        bt.stnID.next <- ttkbutton(frameGraph, text = ">>", width = largeur5)

        frOPTS1 <- tkframe(frameGraph)
        cb.typeTSp <- ttkcombobox(frOPTS1, values = CbtypeTSPLOT, textvariable = typeTSp, width = largeur3, justify = 'center')
        bt.TSGraphOpt <- ttkbutton(frOPTS1, text = .cdtEnv$tcl$lang$global[['button']][['4']], width = largeur5)

        ##############

        tkgrid(txt.stnID, row = 0, column = 0, sticky = 'we', padx = 1, pady = 1, columnspan = 1)
        tkgrid(cb.stnID, row = 0, column = 1, sticky = 'we', padx = 1, pady = 1, columnspan = 1)

        ##############

        tkgrid(cb.typeTSp, row = 0, column = 0, sticky = 'we', padx = 1, pady = 1, columnspan = 1)
        tkgrid(bt.TSGraphOpt, row = 0, column = 1, sticky = 'we', padx = 1, pady = 1, columnspan = 1)

        ##############

        tkgrid(frTS2, row = 0, column = 0, sticky = '', padx = 1, pady = 1, columnspan = 3)
        tkgrid(bt.stnID.prev, row = 1, column = 0, sticky = 'we', padx = 1, pady = 1, columnspan = 1)
        tkgrid(bt.TsGraph.plot, row = 1, column = 1, sticky = 'we', padx = 1, pady = 1, columnspan = 1)
        tkgrid(bt.stnID.next, row = 1, column = 2, sticky = 'we', padx = 1, pady = 1, columnspan = 1)
        tkgrid(frOPTS1, row = 2, column = 0, sticky = '', padx = 1, pady = 1, columnspan = 3)

        ##############

        tkconfigure(bt.TSGraphOpt, command = function(){
            ptype <- typeTSPLOT[CbtypeTSPLOT %in% str_trim(tclvalue(typeTSp))]
            suffix.fun <- switch(ptype, "bar" = "Bar", "line" = "Line")
            plot.fun <- get(paste0("MapGraph.GraphOptions.", suffix.fun), mode = "function")
            .cdtData$EnvData$TSGraphOp <- plot.fun(.cdtData$EnvData$TSGraphOp)
        })

        ##############

        .cdtData$EnvData$tab$dataGraph <- NULL

        tkconfigure(bt.TsGraph.plot, command = function(){
            .cdtData$EnvData$plot.maps$typeTSp <- typeTSPLOT[CbtypeTSPLOT %in% str_trim(tclvalue(typeTSp))]

            if(is.null(.cdtData$EnvData$don)) return(NULL)
            getStnTS()

            ####
            titre <- paste('Station -', .cdtData$EnvData$stndata$series$id)
            imgContainer <- CDT.Display.Graph(plotCDTStation.Graph, .cdtData$EnvData$tab$dataGraph, titre)
            .cdtData$EnvData$tab$dataGraph <- imageNotebookTab_unik(imgContainer, .cdtData$EnvData$tab$dataGraph)
        })

        tkconfigure(bt.stnID.prev, command = function(){
            .cdtData$EnvData$plot.maps$typeTSp <- typeTSPLOT[CbtypeTSPLOT %in% str_trim(tclvalue(typeTSp))]

            if(is.null(.cdtData$EnvData$don)) return(NULL)
            istn <- which(.cdtData$EnvData$don$id == str_trim(tclvalue(.cdtData$EnvData$plot.maps$stnIDTSp)))
            istn <- istn - 1
            if(istn < 1) istn <- length(.cdtData$EnvData$don$id)
            tclvalue(.cdtData$EnvData$plot.maps$stnIDTSp) <- .cdtData$EnvData$don$id[istn]

            getStnTS()

            ####
            titre <- paste('Station -', .cdtData$EnvData$stndata$series$id)
            imgContainer <- CDT.Display.Graph(plotCDTStation.Graph, .cdtData$EnvData$tab$dataGraph, titre)
            .cdtData$EnvData$tab$dataGraph <- imageNotebookTab_unik(imgContainer, .cdtData$EnvData$tab$dataGraph)
        })

        tkconfigure(bt.stnID.next, command = function(){
            .cdtData$EnvData$plot.maps$typeTSp <- typeTSPLOT[CbtypeTSPLOT %in% str_trim(tclvalue(typeTSp))]

            if(is.null(.cdtData$EnvData$don)) return(NULL)
            istn <- which(.cdtData$EnvData$don$id == str_trim(tclvalue(.cdtData$EnvData$plot.maps$stnIDTSp)))
            istn <- istn + 1
            if(istn > length(.cdtData$EnvData$don$id)) istn <- 1
            tclvalue(.cdtData$EnvData$plot.maps$stnIDTSp) <- .cdtData$EnvData$don$id[istn]

            getStnTS()

            ####
            titre <- paste('Station -', .cdtData$EnvData$stndata$series$id)
            imgContainer <- CDT.Display.Graph(plotCDTStation.Graph, .cdtData$EnvData$tab$dataGraph, titre)
            .cdtData$EnvData$tab$dataGraph <- imageNotebookTab_unik(imgContainer, .cdtData$EnvData$tab$dataGraph)
        })

        tkbind(cb.typeTSp, "<<ComboboxSelected>>", function(){
            .cdtData$EnvData$plot.maps$typeTSp <- typeTSPLOT[CbtypeTSPLOT %in% str_trim(tclvalue(typeTSp))]
        })

        ############################################

        tkgrid(frameCDTdata, row = 0, column = 0, sticky = 'we', padx = 1, pady = 1, ipadx = 1, ipady = 1)
        tkgrid(frameMap, row = 1, column = 0, sticky = 'we', padx = 1, pady = 1, ipadx = 1, ipady = 1)
        tkgrid(frameGraph, row = 2, column = 0, sticky = 'we', padx = 1, pady = 1, ipadx = 1, ipady = 1)

    #######################################################################################################

    #Tab2
    subfr2 <- bwTabScrollableFrame(cmd.tab2)

        ##############################################

        frameSHP <- ttklabelframe(subfr2, text = lang.dlg[['label']][['15']], relief = 'groove')

        .cdtData$EnvData$shp$add.shp <- tclVar(FALSE)
        file.plotShp <- tclVar()
        stateSHP <- "disabled"

        chk.addshp <- tkcheckbutton(frameSHP, variable = .cdtData$EnvData$shp$add.shp, text = lang.dlg[['checkbutton']][['1']], anchor = 'w', justify = 'left')
        bt.addshpOpt <- ttkbutton(frameSHP, text = .cdtEnv$tcl$lang$global[['button']][['4']], state = stateSHP)
        cb.addshp <- ttkcombobox(frameSHP, values = unlist(listOpenFiles), textvariable = file.plotShp, width = largeur1, state = stateSHP)
        bt.addshp <- tkbutton(frameSHP, text = "...", state = stateSHP)

        ########
        tkgrid(chk.addshp, row = 0, column = 0, sticky = 'we', rowspan = 1, columnspan = 6, padx = 1, pady = 1)
        tkgrid(bt.addshpOpt, row = 0, column = 6, sticky = 'we', rowspan = 1, columnspan = 2, padx = 1, pady = 1)
        tkgrid(cb.addshp, row = 1, column = 0, sticky = 'we', rowspan = 1, columnspan = 7, padx = 1, pady = 1)
        tkgrid(bt.addshp, row = 1, column = 7, sticky = 'we', rowspan = 1, columnspan = 1, padx = 0, pady = 1)

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

    splitStnData <- function(){
        .cdtData$EnvData$stndata <- NULL
        intstep <- periodVAL[CbperiodVAL %in% str_trim(tclvalue(timeSteps))]

        stn.file <- str_trim(tclvalue(input.file))
        don <- getStnOpenData(stn.file)
        if(is.null(don)) return(NULL)

        if(intstep == "others"){
            don <- splitCDTData1(don)
            daty <- don$dates
            .cdtData$EnvData$others.frmt <- 'numeric'
            if(all(!grepl("[^[:digit:]]", daty))){
                # yearly and others sequential data 
                .cdtData$EnvData$tsdates <- as.numeric(daty)
            }else if(all(nchar(daty) == 15)){
                mosep1 <- substr(daty, 5, 5)
                mosep2 <- substr(daty, 13, 13)
                yrsep <- substr(daty, 8, 8)
                mosep1 <- all(mosep1 == "-")
                mosep2 <- all(mosep2 == "-")
                yrsep <- all(yrsep == "_")
                if(mosep1 & mosep2 & yrsep){
                    years <- substr(daty, 1, 4)
                    if(any(duplicated(years))){
                        # rolling season
                        mois <- substr(daty, 6, 7)
                        mois <- paste0(years, '-', mois, '-', 1)
                        .cdtData$EnvData$tsdates <- as.Date(mois)
                        .cdtData$EnvData$others.frmt <- 'date'
                    }else{
                        # seasonal data
                        .cdtData$EnvData$tsdates <- as.numeric(years)
                    }
                }else{
                    # unknown
                    .cdtData$EnvData$tsdates <- seq_along(daty)
                }
            }else{
                # unknown
                .cdtData$EnvData$tsdates <- seq_along(daty)
            }

            ##########
            tkconfigure(cb.other, values = don$dates)
            tclvalue(date.other) <- don$dates[1]
        }else{
            don <- getCDTdataAndDisplayMsg(don, intstep, stn.file)
            if(is.null(don)) return(NULL)

            ##########
            en.daty <- don$dates[length(don$dates)]
            hrs <- 0
            min <- 0

            if(intstep == "minute"){
                .cdtData$EnvData$tsdates <- as.POSIXct(don$dates, format = "%Y%m%d%H%M")
                dpk <- as.numeric(substr(en.daty, 7, 8))
                hrs <- as.numeric(substr(en.daty, 9, 10))
                min <- as.numeric(substr(en.daty, 11, 12))
            }
            if(intstep == "hourly"){
                .cdtData$EnvData$tsdates <- as.POSIXct(don$dates, format = "%Y%m%d%H")
                dpk <- as.numeric(substr(en.daty, 7, 8))
                hrs <- as.numeric(substr(en.daty, 9, 10))
            }
            if(intstep == "daily"){
                .cdtData$EnvData$tsdates <- as.Date(don$dates, "%Y%m%d")
                dpk <- as.numeric(substr(en.daty, 7, 8))
            }
            if(intstep == "pentad"){
                pen <- c(1, 6, 11, 16, 21, 26)[as.numeric(substr(don$dates, 7, 7))]
                .cdtData$EnvData$tsdates <- as.Date(paste0(substr(don$dates, 1, 6), pen), "%Y%m%d")
                dpk <- as.numeric(substr(en.daty, 7, 7))
            }
            if(intstep == "dekadal"){
                dek <- c(1, 11, 21)[as.numeric(substr(don$dates, 7, 7))]
                .cdtData$EnvData$tsdates <- as.Date(paste0(substr(don$dates, 1, 6), dek), "%Y%m%d")
                dpk <- as.numeric(substr(en.daty, 7, 7))
            }
            if(intstep == "monthly"){
                .cdtData$EnvData$tsdates <- as.Date(paste0(don$dates, 1), "%Y%m%d")
                dpk <- 1
            }

            ##########

            first.date <- if(intstep == "monthly") paste0(don$dates[1], 1) else don$dates[1]
            last.date <- if(intstep == "monthly") paste0(don$dates[length(don$dates)], 1) else don$dates[length(don$dates)]

            if(intstep == "minute"){
                .cdtData$EnvData$first.date <- as.POSIXct(first.date, format = "%Y%m%d%H%M")
                .cdtData$EnvData$last.date <- as.POSIXct(last.date, format = "%Y%m%d%H%M")
            }
            else if(intstep == "hourly"){
                .cdtData$EnvData$first.date <- as.POSIXct(first.date, format = "%Y%m%d%H")
                .cdtData$EnvData$last.date <- as.POSIXct(last.date, format = "%Y%m%d%H")
            }
            else{
                .cdtData$EnvData$first.date <- as.Date(first.date, format = "%Y%m%d")
                .cdtData$EnvData$last.date <- as.Date(last.date, format = "%Y%m%d")
            }

            ##########
            tclvalue(date.year) <- as.numeric(substr(en.daty, 1, 4))
            tclvalue(date.mon) <- as.numeric(substr(en.daty, 5, 6))
            tclvalue(date.day) <- dpk
            tclvalue(date.hour) <- hrs
            tclvalue(date.min) <- min
        }

        ##########
        tkconfigure(cb.stnID, values = don$id)
        tclvalue(.cdtData$EnvData$plot.maps$stnIDTSp) <- don$id[1]

        .cdtData$EnvData$tstep <- intstep
        .cdtData$EnvData$don <- don
        .cdtData$EnvData$plot.maps[c('lon', 'lat', 'id')] <- don[c('lon', 'lat', 'id')]

        ##########
        getStnTS()
        getStnMap()
        return(0)
    }

    getStnTS <- function(){
        stationID <- str_trim(tclvalue(.cdtData$EnvData$plot.maps$stnIDTSp))
        istn <- which(.cdtData$EnvData$don$id == stationID)
        if(length(istn) == 0){
            .cdtData$EnvData$stndata$series <- NULL
            msg <- paste(stationID, lang.dlg[['message']][['6']])
            Insert.Messages.Out(msg, TRUE, "e")
        }else{
            .cdtData$EnvData$stndata$series$ts <- .cdtData$EnvData$don$data[, istn]
            .cdtData$EnvData$stndata$series$id <- stationID
        }
    }

    getStnMap <- function(){
        tkconfigure(.cdtEnv$tcl$main$win, cursor = 'watch')
        tcl('update')
        on.exit({
            tkconfigure(.cdtEnv$tcl$main$win, cursor = '')
            tcl('update')
        })

        typemap <- str_trim(tclvalue(.cdtData$EnvData$map$typeMap))

        if(.cdtData$EnvData$tstep != "others"){
            yrs <- as.numeric(str_trim(tclvalue(date.year)))
            mon <- as.numeric(str_trim(tclvalue(date.mon)))
            dpk <- as.numeric(str_trim(tclvalue(date.day)))
            hrs <- as.numeric(str_trim(tclvalue(date.hour)))
            min <- as.numeric(str_trim(tclvalue(date.min)))
            getSpat <- list(yrs, mon, dpk, hrs, min, typemap)
        }else getSpat <- list(str_trim(tclvalue(date.other)), typemap)

        if(!is.null(.cdtData$EnvData$stndata$spatial)){
            formatSpData <- all.equal(.cdtData$EnvData$stndata$spatial, getSpat)
            formatSpData <- if(!isTRUE(formatSpData)) TRUE else FALSE
        }else formatSpData <- TRUE

        if(formatSpData){
            if(.cdtData$EnvData$tstep != "others"){
                if(.cdtData$EnvData$tstep == "minute"){
                    mins <- paste(yrs, mon, dpk, hrs, min, sep = "-")
                    daty <- format(as.POSIXct(mins, format = "%Y-%m-%d-%H-%M"), "%Y%m%d%H%M")
                }
                if(.cdtData$EnvData$tstep == "hourly"){
                    hhrs <- paste(yrs, mon, dpk, hrs, sep = "-")
                    daty <- format(as.POSIXct(hhrs, format = "%Y-%m-%d-%H"), "%Y%m%d%H")
                }
                if(.cdtData$EnvData$tstep == "daily")
                    daty <- format(as.Date(paste(yrs, mon, dpk, sep = "-")), "%Y%m%d")
                if(.cdtData$EnvData$tstep == "pentad"){
                    pen <- as.Date(paste(yrs, mon, dpk, sep = "-"))
                    daty <- paste0(format(pen, "%Y%m"), dpk)
                }
                if(.cdtData$EnvData$tstep == "dekadal"){
                    dek <- as.Date(paste(yrs, mon, dpk, sep = "-"))
                    daty <- paste0(format(dek, "%Y%m"), dpk)
                }
                if(.cdtData$EnvData$tstep == "monthly")
                    daty <- format(as.Date(paste(yrs, mon, dpk, sep = "-")), "%Y%m")
            }else daty <- str_trim(tclvalue(date.other))

            idaty <- which(.cdtData$EnvData$don$dates == daty)

            if(length(idaty) == 0){
                .cdtData$EnvData$stndata$map <- NULL
                Insert.Messages.Out(lang.dlg[['message']][['7']], TRUE, "e")
                return(NULL)
            }else{
                if(typemap == "Points"){
                    .cdtData$EnvData$stndata$map$x <- .cdtData$EnvData$don$lon
                    .cdtData$EnvData$stndata$map$y <- .cdtData$EnvData$don$lat
                    .cdtData$EnvData$stndata$map$z <- as.numeric(.cdtData$EnvData$don$data[idaty, ])
                }

                if(typemap == "Pixels"){
                    nx <- nx_ny_as.image(diff(range(.cdtData$EnvData$don$lon)))
                    ny <- nx_ny_as.image(diff(range(.cdtData$EnvData$don$lat)))
                    tmp <- cdt.as.image(as.numeric(.cdtData$EnvData$don$data[idaty, ]), nx = nx, ny = ny,
                                        pts.xy = cbind(.cdtData$EnvData$don$lon, .cdtData$EnvData$don$lat))
                    .cdtData$EnvData$stndata$map$x <- tmp$x
                    .cdtData$EnvData$stndata$map$y <- tmp$y
                    .cdtData$EnvData$stndata$map$z <- tmp$z
                }

                .cdtData$EnvData$stndata$map$t <- daty
                .cdtData$EnvData$stndata$map$p <- typemap
            }

            .cdtData$EnvData$stndata$spatial <- getSpat
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
