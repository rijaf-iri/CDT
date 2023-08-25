
SpatialInterpPanelCmd <- function(){
    listOpenFiles <- openFile_ttkcomboList()
    if(WindowsOS()){
        largeur0 <- 20
        largeur1 <- 32
        largeur2 <- 34
        largeur3 <- 14
        largeur4 <- 20
        largeur5 <- 11
        largeur6 <- 30
        largeur7 <- 38
    }else{
        largeur0 <- 23
        largeur1 <- 32
        largeur2 <- 33
        largeur3 <- 14
        largeur4 <- 20
        largeur5 <- 11
        largeur6 <- 30
        largeur7 <- 38
    }

    ###################

    date.range <- list(start.year = 2021, start.mon = 1, start.dek = 1,
                       start.pen = 1, start.day = 1,
                       start.hour = 0, start.min = 0,
                       end.year = 2021, end.mon = 1, end.dek = 1,
                       end.pen = 1, end.day = 1,
                       end.hour = 0, end.min = 0)

    GeneralParameters <- list(intstep = "daily", minhour = 1,
                              cdtstation = "",  date.range = date.range, outdir = "",
                              grid = list(from = 'ncdf', ncfile = "",
                                          bbox = c(.cdtData$Config$region, reslon = 0.1, reslat = 0.1)),
                             interp = list(method = "idw", nmin = 8, nmax = 16,
                                           maxdist = 3.5, use.block = TRUE, minstn = 10,
                                           vgm.model = c("Sph", "Exp", "Gau", "Pen"),
                                           demfile = "",
                                           auxvar = list(dem = TRUE, slope = FALSE, aspect = FALSE,
                                                         lon = FALSE, lat = FALSE)
                                           ),
                             negative = list(set = FALSE, value = 0),
                             blank = list(blank = FALSE, shpf = ""),
                             date = list(year = 2021, mon = 1, day = 1,
                                          hour = 1, min = 0, other = ""))

    pointSizeI <- 1.0
    .cdtData$EnvData$dataMapOp <- list(presetCol = list(color = 'tim.colors', reverse = FALSE),
                                       userCol = list(custom = FALSE, color = NULL),
                                       userLvl = list(custom = FALSE, levels = NULL, equidist = FALSE),
                                       title = list(user = FALSE, title = ''),
                                       colkeyLab = list(user = FALSE, label = ''),
                                       pointCol = 'black',
                                       pointSize = pointSizeI)

    .cdtData$EnvData$SHPOp <- list(col = "black", lwd = 1.5)

    ###################

    xml.dlg <- file.path(.cdtDir$dirLocal, "languages", "cdtSpatialInterp_leftCmd.xml")
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

    date.time.selection <- function(intstep, frTS1){
        if(intstep == 'others'){
            txt.other <- tklabel(frTS1, text = lang.dlg[['label']][['1']])
            cb.other <<- ttkcombobox(frTS1, values = "", textvariable = date.other, width = largeur6)

            tkgrid(txt.other, row = 0, column = 0, sticky = 'we', pady = 1, padx = 1)
            tkgrid(cb.other, row = 1, column = 0, sticky = 'we', pady = 1, padx = 1)
        }else{
            txtdek <- switch(intstep,
                             'dekadal' = lang.dlg[['label']][['4']],
                             'pentad'  = lang.dlg[['label']][['5']],
                                         lang.dlg[['label']][['6']])
            day.txtVar <- tclVar(txtdek)

            stateday <- if(intstep == 'monthly') 'disabled' else 'normal'
            statehour <- if(intstep %in% c('minute', 'hourly')) 'normal' else 'disabled'
            statemin <- if(intstep == 'minute') 'normal' else 'disabled'

            txt.yrs <- tklabel(frTS1, text = lang.dlg[['label']][['2']])
            txt.mon <- tklabel(frTS1, text = lang.dlg[['label']][['3']])
            txt.day <- tklabel(frTS1, text = tclvalue(day.txtVar), textvariable = day.txtVar)
            txt.hrs <- tklabel(frTS1, text = lang.dlg[['label']][['7']])
            txt.min <- tklabel(frTS1, text = lang.dlg[['label']][['8']])

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
        yrs <- as.numeric(trimws(tclvalue(date.year)))
        mon <- as.numeric(trimws(tclvalue(date.mon)))
        dpk <- as.numeric(trimws(tclvalue(date.day)))
        hrs <- as.numeric(trimws(tclvalue(date.hour)))
        min <- as.numeric(trimws(tclvalue(date.min)))

        todaty <- format.dates.times(intstep, yrs, mon, dpk, hrs, min)
        if(is.null(todaty)) return(NULL)

        daty <- try(switch(intstep,
                    "minute" = as.POSIXct(todaty, format = "%Y-%m-%d-%H-%M"),
                    "hourly" = as.POSIXct(todaty, format = "%Y-%m-%d-%H"),
                               as.Date(todaty)
                    ), silent = TRUE)

        if(inherits(daty, "try-error") | is.na(daty)){
            msg <- paste(lang.dlg[['message']][['3']], todaty)
            Insert.Messages.Out(msg, TRUE, "e")
            return(NULL)
        }

        minhour <- as.numeric(trimws(tclvalue(minhour.tclVar)))
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

        frameCDTdata <- ttklabelframe(subfr1, text = lang.dlg[['label']][['9']], relief = 'groove')

        timeSteps <- tclVar()
        CbperiodVAL <- .cdtEnv$tcl$lang$global[['combobox']][['1']][c(1:6, 10)]
        periodVAL <- c('minute', 'hourly', 'daily', 'pentad', 'dekadal', 'monthly', 'others')
        tclvalue(timeSteps) <- CbperiodVAL[periodVAL %in% GeneralParameters$intstep]

        input.file <- tclVar(GeneralParameters$cdtstation)
        retminhr <- set.hour.minute(GeneralParameters$intstep, GeneralParameters$minhour)
        minhour.tclVar <- tclVar(retminhr$val)

        txt.cdtdata1 <- tklabel(frameCDTdata, text = lang.dlg[['label']][['10']], anchor = 'w', justify = 'left')
        cb.cdtdata1 <- ttkcombobox(frameCDTdata, values = CbperiodVAL, textvariable = timeSteps, width = largeur0)
        cb.minhour <- ttkcombobox(frameCDTdata, values = retminhr$cb, textvariable = minhour.tclVar, state = retminhr$state, width = 2)

        txt.cdtdata2 <- tklabel(frameCDTdata, text = lang.dlg[['label']][['11']], anchor = 'w', justify = 'left')
        cb.cdtdata2 <- ttkcombobox(frameCDTdata, values = unlist(listOpenFiles), textvariable = input.file, width = largeur1)
        bt.cdtdata <- tkbutton(frameCDTdata, text = "...")

        ############

        tkgrid(txt.cdtdata1, row = 0, column = 0, sticky = 'we', rowspan = 1, columnspan = 2, padx = 1, pady = 1, ipadx = 1, ipady = 1)
        tkgrid(cb.cdtdata1, row = 0, column = 2, sticky = 'we', rowspan = 1, columnspan = 6, padx = 1, pady = 1, ipadx = 1, ipady = 1)
        tkgrid(cb.minhour, row = 0, column = 8, sticky = 'we', rowspan = 1, columnspan = 2, padx = 0, pady = 1, ipadx = 1, ipady = 1)

        tkgrid(txt.cdtdata2, row = 1, column = 0, sticky = 'we', rowspan = 1, columnspan = 10, padx = 1, pady = 1, ipadx = 1, ipady = 1)
        tkgrid(cb.cdtdata2, row = 2, column = 0, sticky = 'we', rowspan = 1, columnspan = 9, padx = 0, pady = 1, ipadx = 1, ipady = 1)
        tkgrid(bt.cdtdata, row = 2, column = 9, sticky = 'we', rowspan = 1, columnspan = 1, padx = 0, pady = 1, ipadx = 1, ipady = 1)

        ############

        tkconfigure(bt.cdtdata, command = function(){
            dat.opfiles <- getOpenFiles(.cdtEnv$tcl$main$win)
            if(!is.null(dat.opfiles)){
                update.OpenFiles('ascii', dat.opfiles)
                listOpenFiles[[length(listOpenFiles) + 1]] <<- dat.opfiles[[1]]
                tclvalue(input.file) <- dat.opfiles[[1]]
                lapply(list(cb.cdtdata2, cb.blankgrid, cb.addshp), tkconfigure, values = unlist(listOpenFiles))
            }
        })

        ############

        tkbind(cb.cdtdata1, "<<ComboboxSelected>>", function(){
            tkdestroy(frTS1)
            frTS1 <<- tkframe(frTS0)

            intstep <- periodVAL[CbperiodVAL %in% trimws(tclvalue(timeSteps))]
            date.time.selection(intstep, frTS1)

            tkgrid(frTS1, row = 0, column = 1, sticky = 'we', pady = 1, rowspan = 2, columnspan = 1)

            ##############
            minhour <- as.numeric(trimws(tclvalue(minhour.tclVar)))
            retminhr <- set.hour.minute(intstep, minhour)
            tkconfigure(cb.minhour, values = retminhr$cb, state = retminhr$state)
            tclvalue(minhour.tclVar) <- retminhr$val

            ##############

            statedate <- if(intstep == "others") "disabled" else "normal"
            tkconfigure(btDateRange, state = statedate)
        })

        ##############################################

        statedate <- if(GeneralParameters$intstep == "others") "disabled" else "normal"

        btDateRange <- ttkbutton(subfr1, text = lang.dlg[['button']][['1']], state = statedate)
        btInterpMthd <- ttkbutton(subfr1, text = lang.dlg[['button']][['2']])
        btGridInterp <- ttkbutton(subfr1, text = lang.dlg[['button']][['3']])

        helpWidget(btDateRange, lang.dlg[['tooltip']][['1']], lang.dlg[['status']][['1']])
        helpWidget(btInterpMthd, lang.dlg[['tooltip']][['2']], lang.dlg[['status']][['2']])
        helpWidget(btGridInterp, lang.dlg[['tooltip']][['3']], lang.dlg[['status']][['3']])

        ############

        tkconfigure(btDateRange, command = function(){
            intstep <- periodVAL[CbperiodVAL %in% trimws(tclvalue(timeSteps))]
            GeneralParameters[["date.range"]] <<- getInfoDateRange(.cdtEnv$tcl$main$win,
                                                                    GeneralParameters[["date.range"]],
                                                                    intstep)
        })

        tkconfigure(btInterpMthd, command = function(){
            GeneralParameters[["interp"]] <<- getInterpolationPars2(.cdtEnv$tcl$main$win,
                                                                    GeneralParameters[["interp"]])
        })

        tkconfigure(btGridInterp, command = function(){
            GeneralParameters[["grid"]] <<- createGridInterpolation(.cdtEnv$tcl$main$win,
                                                                    GeneralParameters[["grid"]],
                                                                    group = 2)
        })

        ##############################################

        frameNegVal <- tkframe(subfr1)

        set.neg.value <- tclVar(GeneralParameters$negative$set)
        val.neg.value <- tclVar(GeneralParameters$negative$value)

        statenegval <- if(GeneralParameters$negative$set) "normal" else "disabled"

        chk.negval <- tkcheckbutton(frameNegVal, variable = set.neg.value, text = lang.dlg[['checkbutton']][['1']], anchor = 'w', justify = 'left')
        en.negval <- tkentry(frameNegVal, textvariable = val.neg.value, width = 4, state = statenegval)

        tkgrid(chk.negval, row = 0, column = 0, sticky = 'we',  pady = 1, ipadx = 1, ipady = 1)
        tkgrid(en.negval, row = 0, column = 1, sticky = 'w', pady = 1, ipadx = 1, ipady = 1)

        tkbind(chk.negval, "<Button-1>", function(){
            if(tclvalue(interpData) == '0'){
                statenegval <- if(tclvalue(set.neg.value) == "0")  'normal' else 'disabled'
            }else statenegval <- 'disabled'
            tkconfigure(en.negval, state = statenegval)
        })

        ##############################################

        frameBlank <- tkframe(subfr1, relief = 'groove', borderwidth = 2)

        blankGrid <- tclVar(GeneralParameters$blank$blank)
        file.blankShp <- tclVar(GeneralParameters$blank$shpf)
        stateSHP <- if(GeneralParameters$blank$blank) "normal" else "disabled"

        chk.blankgrid <- tkcheckbutton(frameBlank, variable = blankGrid, text = lang.dlg[['checkbutton']][['2']], anchor = 'w', justify = 'left')
        cb.blankgrid <- ttkcombobox(frameBlank, values = unlist(listOpenFiles), textvariable = file.blankShp, width = largeur1, state = stateSHP)
        bt.blankgrid <- tkbutton(frameBlank, text = "...", state = stateSHP)

        ########
        tkgrid(chk.blankgrid, row = 0, column = 0, sticky = 'we', rowspan = 1, columnspan = 6, padx = 1, pady = 1)
        tkgrid(cb.blankgrid, row = 1, column = 0, sticky = 'we', rowspan = 1, columnspan = 5, padx = 1, pady = 1)
        tkgrid(bt.blankgrid, row = 1, column = 7, sticky = 'we', rowspan = 1, columnspan = 1, padx = 0, pady = 1)

        ########

        tkconfigure(bt.blankgrid, command = function(){
            shp.opfiles <- getOpenShp(.cdtEnv$tcl$main$win)
            if(!is.null(shp.opfiles)){
                update.OpenFiles('shp', shp.opfiles)
                tclvalue(file.blankShp) <- shp.opfiles[[1]]
                listOpenFiles[[length(listOpenFiles) + 1]] <<- shp.opfiles[[1]]
                lapply(list(cb.cdtdata2, cb.blankgrid, cb.addshp), tkconfigure, values = unlist(listOpenFiles))
            }
        })

        tkbind(chk.blankgrid, "<Button-1>", function(){
            if(tclvalue(interpData) == '0'){
                stateSHP <- if(tclvalue(blankGrid) == "1") "disabled" else "normal"
            }else stateSHP <- "disabled"
            tkconfigure(cb.blankgrid, state = stateSHP)
            tkconfigure(bt.blankgrid, state = stateSHP)
        })

        ##############################################

        frameDirSav <- tkframe(subfr1, relief = 'groove', borderwidth = 2)

        dir.save <- tclVar(GeneralParameters$outdir)

        txt.dir.save <- tklabel(frameDirSav, text = lang.dlg[['label']][['12']], anchor = 'w', justify = 'left')
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

        ##############################################

        btInterpolate <- ttkbutton(subfr1, text = lang.dlg[['button']][['4']])

        tkconfigure(btInterpolate, command = function(){
            GeneralParameters$intstep <- periodVAL[CbperiodVAL %in% trimws(tclvalue(timeSteps))]
            GeneralParameters$negative$set <- switch(tclvalue(set.neg.value), '0' = FALSE, '1' = TRUE)
            GeneralParameters$negative$value <- as.numeric(trimws(tclvalue(val.neg.value)))
            GeneralParameters$cdtstation <- trimws(tclvalue(input.file))
            GeneralParameters$outdir <- trimws(tclvalue(dir.save))
            GeneralParameters$blank$blank <- switch(tclvalue(blankGrid), '0' = FALSE, '1' = TRUE)
            GeneralParameters$blank$shpf <- trimws(tclvalue(file.blankShp))

            # assign('GeneralParameters', GeneralParameters, envir = .GlobalEnv)

            Insert.Messages.Out(lang.dlg[['message']][['4']], TRUE, "i")

            tkconfigure(.cdtEnv$tcl$main$win, cursor = 'watch')
            tcl('update')
            ret <- tryCatch(
                        {
                            interpStationsProcs(GeneralParameters)
                        },
                        warning = function(w) warningFun(w),
                        error = function(e) errorFun(e),
                        finally = {
                            tkconfigure(.cdtEnv$tcl$main$win, cursor = '')
                            tcl('update')
                        }
                    )

            if(!is.null(ret)){
                if(ret %in% c(-1, 0)){
                    Insert.Messages.Out(lang.dlg[['message']][['5']], TRUE, "s")

                    if(GeneralParameters$intstep == "others"){
                        tkconfigure(cb.other, values = .cdtData$EnvData$stnData$dates)
                        tclvalue(date.other) <- .cdtData$EnvData$stnData$dates[1]
                    }else{
                        daty <- .cdtData$EnvData$last.date
                        tclvalue(date.year) <- as.numeric(format(daty, '%Y'))
                        tclvalue(date.mon) <- as.numeric(format(daty, '%m'))
                        tclvalue(date.day) <- as.numeric(format(daty, '%d'))
                        tclvalue(date.hour) <- as.numeric(format(daty, '%H'))
                        tclvalue(date.min) <- as.numeric(format(daty, '%M'))
                    }
                }else if(ret == 1){
                    Insert.Messages.Out(lang.dlg[['message']][['6']], TRUE, "w")
                }else Insert.Messages.Out(lang.dlg[['message']][['7']], TRUE, "e")
            }else Insert.Messages.Out(lang.dlg[['message']][['7']], TRUE, "e")
        })

        ##############################################

        tkgrid(frameCDTdata, row = 0, column = 0, sticky = 'we', padx = 1, pady = 1, ipadx = 1, ipady = 1)
        tkgrid(btDateRange, row = 1, column = 0, sticky = 'we', padx = 1, pady = 2, ipadx = 1, ipady = 1)
        tkgrid(btInterpMthd, row = 2, column = 0, sticky = 'we', padx = 1, pady = 1, ipadx = 1, ipady = 1)
        tkgrid(btGridInterp, row = 3, column = 0, sticky = 'we', padx = 1, pady = 1, ipadx = 1, ipady = 1)
        tkgrid(frameNegVal, row = 4, column = 0, sticky = 'we', padx = 1, pady = 3, ipadx = 1, ipady = 1)
        tkgrid(frameBlank, row = 5, column = 0, sticky = 'we', padx = 1, pady = 3, ipadx = 1, ipady = 1)
        tkgrid(frameDirSav, row = 6, column = 0, sticky = 'we', padx = 1, pady = 1, ipadx = 1, ipady = 1)
        tkgrid(btInterpolate, row = 7, column = 0, sticky = 'we', padx = 1, pady = 10, ipadx = 1, ipady = 1)

    #######################################################################################################

    #Tab2
    subfr2 <- bwTabScrollableFrame(cmd.tab2)

        ##############################################

        frameINTRP <- ttklabelframe(subfr2, text = lang.dlg[['label']][['13']], relief = 'groove')

        interpData <- tclVar(0)
        file.interpData <- tclVar()

        stateIntD <- if(tclvalue(interpData) == "1") "normal" else "disabled"

        chk.IntD <- tkcheckbutton(frameINTRP, variable = interpData, text = lang.dlg[['checkbutton']][['3']], anchor = 'w', justify = 'left')
        en.IntD <- tkentry(frameINTRP, textvariable = file.interpData, width = largeur7, state = stateIntD)
        bt.IntD <- ttkbutton(frameINTRP, text = .cdtEnv$tcl$lang$global[['button']][['6']], state = stateIntD)

        tkgrid(chk.IntD, row = 0, column = 0, sticky = 'we', rowspan = 1, columnspan = 4, padx = 1, pady = 1, ipadx = 1, ipady = 1)
        tkgrid(bt.IntD, row = 0, column = 4, sticky = 'e', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
        tkgrid(en.IntD, row = 1, column = 0, sticky = 'we', rowspan = 1, columnspan = 5, padx = 1, pady = 1, ipadx = 1, ipady = 1)

        ##############

        tkconfigure(bt.IntD, command = function(){
            path.Interp <- tclvalue(tkgetOpenFile(initialdir = getwd(), filetypes = .cdtEnv$tcl$data$filetypes6))
            if(path.Interp == "") return(NULL)
            tclvalue(file.interpData) <- path.Interp

            path.Interp <- trimws(tclvalue(file.interpData))
            if(file.exists(path.Interp)){
                interp.data <- try(readRDS(path.Interp), silent = TRUE)
                if(inherits(interp.data, "try-error")){
                    Insert.Messages.Out(lang.dlg[['message']][['8']], TRUE, 'e')
                    Insert.Messages.Out(gsub('[\r\n]', '', interp.data[1]), TRUE, 'e')
                    return(NULL)
                }

                .cdtData$EnvData$stnData <- interp.data$stn
                .cdtData$EnvData$ncdfOUT <- file.path(dirname(path.Interp), 'DATA_NetCDF')
                .cdtData$EnvData$first.date <- interp.data$first.date
                .cdtData$EnvData$last.date <- interp.data$last.date

                if(interp.data$params$intstep == "others"){
                    tkconfigure(cb.other, values = .cdtData$EnvData$stnData$dates)
                    tclvalue(date.other) <- .cdtData$EnvData$stnData$dates[1]
                }else{
                    daty <- .cdtData$EnvData$last.date
                    tclvalue(date.year) <- as.numeric(format(daty, '%Y'))
                    tclvalue(date.mon) <- as.numeric(format(daty, '%m'))
                    tclvalue(date.day) <- as.numeric(format(daty, '%d'))
                    tclvalue(date.hour) <- as.numeric(format(daty, '%H'))
                    tclvalue(date.min) <- as.numeric(format(daty, '%M'))
                }

                tclvalue(timeSteps) <- CbperiodVAL[periodVAL %in% interp.data$params$intstep]
                retminhr <- set.hour.minute(interp.data$params$intstep, interp.data$params$minhour)
                tclvalue(minhour.tclVar) <- retminhr$val
                tkconfigure(cb.minhour, values = retminhr$cb)
            }
        })

        tkbind(chk.IntD, "<Button-1>", function(){
            stateIntD <- if(tclvalue(interpData) == '1') 'disabled' else 'normal'
            tkconfigure(bt.IntD, state = stateIntD)
            tkconfigure(en.IntD, state = stateIntD)

            stateDataIn <- if(tclvalue(interpData) == '1') 'normal' else 'disabled'
            tcl(tknote.cmd, 'itemconfigure', cmd.tab1$IDtab, state = stateDataIn)
        })

        ##############

        frameMap <- ttklabelframe(subfr2, text = lang.dlg[['label']][['14']], relief = 'groove')

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
        bt.Map.Opt <- ttkbutton(frameMap, text = .cdtEnv$tcl$lang$global[['button']][['4']], width = largeur5)

        ##############

        frPLOTpanel <- tkframe(frameMap)

        .cdtData$EnvData$map$panelMap <- 'two'
        panelMapVar <- tclVar()
        panelMaps <- lang.dlg[['combobox']][['1']]
        panelNumber <- c('one', 'two')
        tclvalue(panelMapVar) <- panelMaps[panelNumber %in% .cdtData$EnvData$map$panelMap]

        txt.Map.panel <- tklabel(frPLOTpanel, text = lang.dlg[['label']][['15']], anchor = 'w', justify = 'left')
        cb.Map.panel <- ttkcombobox(frPLOTpanel, values = panelMaps, textvariable = panelMapVar, width = largeur0)

        tkgrid(txt.Map.panel, row = 0, column = 0, sticky = 'e', padx = 0, pady = 1, columnspan = 1)
        tkgrid(cb.Map.panel, row = 0, column = 1, sticky = 'we', padx = 1, pady = 1, columnspan = 1)

        ##############

        tkbind(cb.Map.panel, "<<ComboboxSelected>>", function(){
            .cdtData$EnvData$map$panelMap <- panelNumber[panelMaps %in% trimws(tclvalue(panelMapVar))]

            statetypeMapPLOT1 <- if(.cdtData$EnvData$map$panelMap == "one") "disabled" else "normal"
            tkconfigure(cb.Map.type1, state = statetypeMapPLOT1)
        })

        ##############

        frOPTS0 <- tkframe(frameMap)

        .cdtData$EnvData$map$typeMap1 <- "Points"
        typeMapPLOT1 <- c("Points", "Pixels")
        typeMap1Var <- tclVar(.cdtData$EnvData$map$typeMap1)

        statetypeMapPLOT1 <- if(.cdtData$EnvData$map$panelMap == "one") "disabled" else "normal"

        .cdtData$EnvData$map$typeMap2 <- "Pixels"
        typeMapPLOT2 <- c("Pixels", "FilledContour")
        typeMap2Var <- tclVar(.cdtData$EnvData$map$typeMap2)

        txt.Map.type1 <- tklabel(frOPTS0, text = lang.dlg[['label']][['16']], anchor = 'w', justify = 'left')
        cb.Map.type1 <- ttkcombobox(frOPTS0, values = typeMapPLOT1, textvariable = typeMap1Var, width = largeur3, state = statetypeMapPLOT1)

        txt.Map.type2 <- tklabel(frOPTS0, text = lang.dlg[['label']][['17']], anchor = 'w', justify = 'left')
        cb.Map.type2 <- ttkcombobox(frOPTS0, values = typeMapPLOT2, textvariable = typeMap2Var, width = largeur3)

        ##############

        tkbind(cb.Map.type1, "<<ComboboxSelected>>", function(){
            .cdtData$EnvData$map$typeMap1 <- trimws(tclvalue(typeMap1Var))
            if(.cdtData$EnvData$map$typeMap1 == "Points"){
                .cdtData$EnvData$dataMapOp$pointSize <- pointSizeI
            }else .cdtData$EnvData$dataMapOp$pointSize <- NULL
        })

        tkbind(cb.Map.type2, "<<ComboboxSelected>>", function(){
            .cdtData$EnvData$map$typeMap2 <- trimws(tclvalue(typeMap2Var))
        })

        ##############

        tkgrid(frTS1, row = 0, column = 0, sticky = 'we', padx = 1, pady = 1, rowspan = 1, columnspan = 1)

        ##############

        tkgrid(txt.Map.type1, row = 0, column = 0, sticky = 'e', padx = 1, pady = 1, columnspan = 1)
        tkgrid(cb.Map.type1, row = 0, column = 1, sticky = 'we', padx = 1, pady = 1, columnspan = 1)

        tkgrid(txt.Map.type2, row = 1, column = 0, sticky = 'e', padx = 1, pady = 1, columnspan = 1)
        tkgrid(cb.Map.type2, row = 1, column = 1, sticky = 'we', padx = 1, pady = 1, columnspan = 1)

        ##############

        tkgrid(frTS0, row = 0, column = 0, sticky = '', padx = 1, pady = 1, columnspan = 3)
        tkgrid(bt.date.prev, row = 1, column = 0, sticky = 'we', padx = 1, pady = 1, columnspan = 1)
        tkgrid(bt.Map.plot, row = 1, column = 1, sticky = 'we', padx = 5, pady = 1, columnspan = 1)
        tkgrid(bt.date.next, row = 1, column = 2, sticky = 'we', padx = 1, pady = 1, columnspan = 1)
        tkgrid(frPLOTpanel, row = 2, column = 0, sticky = '', padx = 1, pady = 5, columnspan = 3)
        tkgrid(frOPTS0, row = 3, column = 0, sticky = 'we', padx = 1, pady = 1, columnspan = 2)
        tkgrid(bt.Map.Opt, row = 3, column = 2, sticky = 'we', padx = 1, pady = 1, rowspan = 2, columnspan = 1)

        ##############

        tkconfigure(bt.Map.Opt, command = function(){
            if(!is.null(.cdtData$EnvData$mapdata$mapstn)){
                atlevel <- pretty(.cdtData$EnvData$mapdata$mapstn$z, n = 10, min.n = 7)
                if(is.null(.cdtData$EnvData$dataMapOp$userLvl$levels)){
                    .cdtData$EnvData$dataMapOp$userLvl$levels <- atlevel
                }else{
                    if(!.cdtData$EnvData$dataMapOp$userLvl$custom)
                        .cdtData$EnvData$dataMapOp$userLvl$levels <- atlevel
                }
            }
            .cdtData$EnvData$dataMapOp <- MapGraph.MapOptions(.cdtData$EnvData$dataMapOp)

            if(.cdtData$EnvData$map$typeMap1 == "Points")
                pointSizeI <<- .cdtData$EnvData$dataMapOp$pointSize
        })

        ##############

        .cdtData$EnvData$tab$dataMap <- NULL

        tkconfigure(bt.Map.plot, command = function(){
            if(is.null(.cdtData$EnvData$stnData)) return(NULL)
            ret <- try(getStnMap(), silent = TRUE)
            if(inherits(ret, "try-error") | is.null(ret)){
                Insert.Messages.Out(gsub('[\r\n]', '', ret[1]), TRUE, "e")
                return(NULL)
            }

            ####
            imgContainer <- CDT.Display.Graph(spatialInterp.plotMap, .cdtData$EnvData$tab$dataMap, 'Spatial-Interpolation')
            .cdtData$EnvData$tab$dataMap <- imageNotebookTab_unik(imgContainer, .cdtData$EnvData$tab$dataMap)
        })

        tkconfigure(bt.date.prev, command = function(){
            if(is.null(.cdtData$EnvData$stnData)) return(NULL) 
            intstep <- periodVAL[CbperiodVAL %in% trimws(tclvalue(timeSteps))]

            if(intstep == "others"){
                idaty <- which(.cdtData$EnvData$stnData$dates == trimws(tclvalue(date.other)))
                idaty <- idaty - 1
                if(idaty < 1) idaty <- length(.cdtData$EnvData$stnData$dates)
                tclvalue(date.other) <- .cdtData$EnvData$stnData$dates[idaty]
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
            imgContainer <- CDT.Display.Graph(spatialInterp.plotMap, .cdtData$EnvData$tab$dataMap, 'Spatial-Interpolation')
            .cdtData$EnvData$tab$dataMap <- imageNotebookTab_unik(imgContainer, .cdtData$EnvData$tab$dataMap)
        })

        tkconfigure(bt.date.next, command = function(){
            if(is.null(.cdtData$EnvData$stnData)) return(NULL) 
            intstep <- periodVAL[CbperiodVAL %in% trimws(tclvalue(timeSteps))]

            if(intstep == "others"){
                idaty <- which(.cdtData$EnvData$stnData$dates == trimws(tclvalue(date.other)))
                idaty <- idaty + 1
                if(idaty > length(.cdtData$EnvData$stnData$dates)) idaty <- 1
                tclvalue(date.other) <- .cdtData$EnvData$stnData$dates[idaty]
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
            imgContainer <- CDT.Display.Graph(spatialInterp.plotMap, .cdtData$EnvData$tab$dataMap, 'Spatial-Interpolation')
            .cdtData$EnvData$tab$dataMap <- imageNotebookTab_unik(imgContainer, .cdtData$EnvData$tab$dataMap)
        })

        ############################################

        tkgrid(frameINTRP, row = 0, column = 0, sticky = 'we', padx = 1, pady = 1, ipadx = 1, ipady = 1)
        tkgrid(frameMap, row = 1, column = 0, sticky = 'we', padx = 1, pady = 1, ipadx = 1, ipady = 1)

    #######################################################################################################

    #Tab3
    subfr3 <- bwTabScrollableFrame(cmd.tab3)

        ##############################################

        frameSHP <- ttklabelframe(subfr3, text = lang.dlg[['label']][['18']], relief = 'groove')

        .cdtData$EnvData$shp$add.shp <- tclVar(FALSE)
        file.plotShp <- tclVar()
        stateSHP <- "disabled"

        chk.addshp <- tkcheckbutton(frameSHP, variable = .cdtData$EnvData$shp$add.shp, text = lang.dlg[['checkbutton']][['4']], anchor = 'w', justify = 'left')
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
                lapply(list(cb.cdtdata2, cb.blankgrid, cb.addshp), tkconfigure, values = unlist(listOpenFiles))

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

    getStnMap <- function(){
        tkconfigure(.cdtEnv$tcl$main$win, cursor = 'watch')
        tcl('update')
        on.exit({
            tkconfigure(.cdtEnv$tcl$main$win, cursor = '')
            tcl('update')
        })

        typemap1 <- .cdtData$EnvData$map$typeMap1
        typemap2 <- .cdtData$EnvData$map$typeMap2
        tstep <- periodVAL[CbperiodVAL %in% trimws(tclvalue(timeSteps))]

        if(tstep != "others"){
            yrs <- as.numeric(trimws(tclvalue(date.year)))
            mon <- as.numeric(trimws(tclvalue(date.mon)))
            dpk <- as.numeric(trimws(tclvalue(date.day)))
            hrs <- as.numeric(trimws(tclvalue(date.hour)))
            min <- as.numeric(trimws(tclvalue(date.min)))
            getSpat <- list(yrs, mon, dpk, hrs, min, typemap1)
        }else getSpat <- list(trimws(tclvalue(date.other)), typemap1)

        if(tstep != "others"){
            if(tstep == "minute"){
                mins <- paste(yrs, mon, dpk, hrs, min, sep = "-")
                daty <- format(as.POSIXct(mins, format = "%Y-%m-%d-%H-%M"), "%Y%m%d%H%M")
            }
            if(tstep == "hourly"){
                hhrs <- paste(yrs, mon, dpk, hrs, sep = "-")
                daty <- format(as.POSIXct(hhrs, format = "%Y-%m-%d-%H"), "%Y%m%d%H")
            }
            if(tstep == "daily")
                daty <- format(as.Date(paste(yrs, mon, dpk, sep = "-")), "%Y%m%d")
            if(tstep == "pentad"){
                pen <- as.Date(paste(yrs, mon, dpk, sep = "-"))
                daty <- paste0(format(pen, "%Y%m"), dpk)
            }
            if(tstep == "dekadal"){
                dek <- as.Date(paste(yrs, mon, dpk, sep = "-"))
                daty <- paste0(format(dek, "%Y%m"), dpk)
            }
            if(tstep == "monthly")
                daty <- format(as.Date(paste(yrs, mon, dpk, sep = "-")), "%Y%m")
        }else daty <- trimws(tclvalue(date.other))

        idaty <- which(.cdtData$EnvData$stnData$dates == daty)

        if(length(idaty) == 0){
            .cdtData$EnvData$mapdata$mapstn <- NULL
            .cdtData$EnvData$mapdata$mapncdf <- NULL
            Insert.Messages.Out(lang.dlg[['message']][['9']], TRUE, "e")
            return(NULL)
        }else{
            formatSpData <- TRUE
            if(!is.null(.cdtData$EnvData$mapdata$spatial)){
                formatSpData <- all.equal(.cdtData$EnvData$mapdata$spatial, getSpat)
                formatSpData <- if(!isTRUE(formatSpData)) TRUE else FALSE
            }

            if(formatSpData){
                if(typemap1 == "Points"){
                    .cdtData$EnvData$mapdata$mapstn$x <- .cdtData$EnvData$stnData$lon
                    .cdtData$EnvData$mapdata$mapstn$y <- .cdtData$EnvData$stnData$lat
                    .cdtData$EnvData$mapdata$mapstn$z <- as.numeric(.cdtData$EnvData$stnData$data[idaty, ])
                }

                if(typemap1 == "Pixels"){
                    nx <- nx_ny_as.image(diff(range(.cdtData$EnvData$stnData$lon)))
                    ny <- nx_ny_as.image(diff(range(.cdtData$EnvData$stnData$lat)))
                    tmp <- cdt.as.image(as.numeric(.cdtData$EnvData$stnData$data[idaty, ]), nx = nx, ny = ny,
                                        pts.xy = cbind(.cdtData$EnvData$stnData$lon, .cdtData$EnvData$stnData$lat))
                    .cdtData$EnvData$mapdata$mapstn$x <- tmp$x
                    .cdtData$EnvData$mapdata$mapstn$y <- tmp$y
                    .cdtData$EnvData$mapdata$mapstn$z <- tmp$z
                }

                .cdtData$EnvData$mapdata$spatial <- getSpat
            }

            ################

            ncfile <- paste0("stn_interp_", daty, ".nc")
            ncpath <- file.path(.cdtData$EnvData$ncdfOUT, ncfile)

            readNCDF <- TRUE
            if(!is.null(.cdtData$EnvData$mapdata$readnc)){
                readNCDF <- all.equal(.cdtData$EnvData$mapdata$readnc, ncpath)
                readNCDF <- if(!isTRUE(readNCDF)) TRUE else FALSE
            }

            if(readNCDF){
                if(file.exists(ncpath)){
                    nc <- ncdf4::nc_open(ncpath)
                    .cdtData$EnvData$mapdata$mapncdf$x <- nc$dim[[1]]$vals
                    .cdtData$EnvData$mapdata$mapncdf$y <- nc$dim[[2]]$vals
                    .cdtData$EnvData$mapdata$mapncdf$z <- ncdf4::ncvar_get(nc, "var")
                    ncdf4::nc_close(nc)
                }else{
                    .cdtData$EnvData$mapdata$mapncdf <- NULL
                    Insert.Messages.Out(paste(ncpath, lang.dlg[['message']][['10']]), TRUE, "e")
                }

                .cdtData$EnvData$mapdata$readnc <- ncpath
            }

            .cdtData$EnvData$mapdata$t <- daty
            .cdtData$EnvData$mapdata$mapstn$p <- typemap1
            .cdtData$EnvData$mapdata$mapstn$mp <- "Points"

            if(!is.null(.cdtData$EnvData$mapdata$mapncdf)){
                .cdtData$EnvData$mapdata$mapncdf$p <- typemap2
                .cdtData$EnvData$mapdata$mapncdf$mp <- "Grid"
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
