
DecilesCalcPanelCmd <- function(){
    listOpenFiles <- openFile_ttkcomboList()
    if(WindowsOS()){
        largeur0 <- .cdtEnv$tcl$fun$w.widgets(22)
        largeur1 <- .cdtEnv$tcl$fun$w.widgets(31)
        largeur2 <- .cdtEnv$tcl$fun$w.widgets(33)
        largeur3 <- 23
        largeur4 <- 33
        largeur5 <- 22
    }else{
        largeur0 <- .cdtEnv$tcl$fun$w.widgets(16)
        largeur1 <- .cdtEnv$tcl$fun$w.widgets(21)
        largeur2 <- .cdtEnv$tcl$fun$w.widgets(22)
        largeur3 <- 15
        largeur4 <- 20
        largeur5 <- 14
    }

    GeneralParameters <- list(intstep = "dekadal", data.type = "cdtstation", 
                            cdtstation = "", cdtdataset = "",
                            outfreq = "month", tscale = 3,
                            base.period = list(all.years = TRUE, start.year = 1981, end.year = 2017, min.year = 15),
                            monitoring = FALSE,
                            dates = list(year1 = 2018, mon1 = 6, dek1 = 1, year2 = 2018, mon2 = 6, dek2 = 3),
                            outdir = "")

    GeneralParameters$date.range <- list(start.year = 2018, start.mon = 6, start.dek = 1,
                                       start.pen = 1, start.day = 1,
                                       start.hour = 0, start.min = 0,
                                       end.year = 2018, end.mon = 6, end.dek = 3,
                                       end.pen = 6, end.day = 30,
                                       end.hour = 23, end.min = 55)

    # xml.dlg <- file.path(.cdtDir$dirLocal, "languages", "cdtCompute_Decile_leftCmd.xml")
    # lang.dlg <- cdtLanguageParse(xml.dlg, .cdtData$Config$lang.iso)
    # .cdtData$EnvData$message <- lang.dlg[['message']]

    ###################

    .cdtEnv$tcl$main$cmd.frame <- tkframe(.cdtEnv$tcl$main$panel.left)

    tknote.cmd <- bwNoteBook(.cdtEnv$tcl$main$cmd.frame)

    cmd.tab1 <- bwAddTab(tknote.cmd, text = "Decile")
    cmd.tab2 <- bwAddTab(tknote.cmd, text = "Maps")
    cmd.tab3 <- bwAddTab(tknote.cmd, text = "Graphs")
    cmd.tab4 <- bwAddTab(tknote.cmd, text = "Boundaries")

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

        frameTimeS <- ttklabelframe(subfr1, text = "Time step of input data", relief = 'groove')

        timeSteps <- tclVar()
        CbperiodVAL <- .cdtEnv$tcl$lang$global[['combobox']][['1']][3:6]
        periodVAL <- c('daily', 'pentad', 'dekadal', 'monthly')
        tclvalue(timeSteps) <- CbperiodVAL[periodVAL %in% GeneralParameters$intstep]

        cb.fperiod <- ttkcombobox(frameTimeS, values = CbperiodVAL, textvariable = timeSteps, width = largeur1)

        tkgrid(cb.fperiod, row = 0, column = 0, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)

        infobulle(cb.fperiod, 'Select the time step of the data')
        status.bar.display(cb.fperiod, 'Select the time step of the data')

        ############

        tkbind(cb.fperiod, "<<ComboboxSelected>>", function(){
            valSPIfreq <- if(str_trim(tclvalue(timeSteps)) == CbperiodVAL[4]) "month" else c("dekad", "month")
            tkconfigure(cb.SPIfreq, values = valSPIfreq)
            if(str_trim(tclvalue(timeSteps)) == CbperiodVAL[4]){
                tclvalue(out.spifreq) <- "month"
                tclvalue(txt.suffix.var) <- '-month'
            }
            stateTscale <- if(str_trim(tclvalue(out.spifreq)) == 'month') "normal" else "disabled"
            tkconfigure(spin.Tscale, state = stateTscale)
        })

        #######################

        frameInData <- ttklabelframe(subfr1, text = "Input Data", relief = 'groove')

        DataType <- tclVar()
        CbdatatypeVAL <- .cdtEnv$tcl$lang$global[['combobox']][['2']][1:2]
        datatypeVAL <- c('cdtstation', 'cdtdataset')
        tclvalue(DataType) <- CbdatatypeVAL[datatypeVAL %in% GeneralParameters$data.type]

        if(GeneralParameters$data.type == 'cdtstation'){
            input.file <- tclVar(GeneralParameters$cdtstation)
            txt.INData <- 'File containing stations Precip data'
        }else{
            input.file <- tclVar(GeneralParameters$cdtdataset)
            txt.INData <- 'Index file (*.rds) for Precip dataset'
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
        tkgrid(txt.infile, row = 1, column = 0, sticky = 'we', rowspan = 1, columnspan = 10, padx = 1, pady = 1, ipadx = 1, ipady = 1)
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
                tclvalue(txt.INData.var) <- 'File containing stations Precip data'

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
                tclvalue(txt.INData.var) <- 'Index file (*.rds) for Precip dataset'

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

        frameMoni <- tkframe(subfr1, relief = 'groove', borderwidth = 2)

        monitoring <- tclVar(GeneralParameters$monitoring)

        if(GeneralParameters$monitoring){
            statedates <- 'normal'
            statedatedek <- if(GeneralParameters$outfreq == 'month') 'disabled' else 'normal'
        }else{
            statedates <- 'disabled'
            statedatedek <- 'disabled'
        }

        chk.Moni <- tkcheckbutton(frameMoni, variable = monitoring, text = "Monitoring: update Decile dataset", anchor = 'w', justify = 'left')
        bt.DateRange <- ttkbutton(frameMoni, text = "Set Date Range", state = statedates)

        tkconfigure(bt.DateRange, command = function(){
            outfreq <- switch(str_trim(tclvalue(out.spifreq)), "dekad" = "dekadal", "month" = "monthly")

            GeneralParameters[["date.range"]] <<- getInfoDateRange(.cdtEnv$tcl$main$win,
                                                                    GeneralParameters[["date.range"]],
                                                                    outfreq)
            GeneralParameters$dates$year1 <<- GeneralParameters$date.range$start.year
            GeneralParameters$dates$mon1 <<- GeneralParameters$date.range$start.mon
            GeneralParameters$dates$dek1 <<- GeneralParameters$date.range$start.dek
            GeneralParameters$dates$year2 <<- GeneralParameters$date.range$end.year
            GeneralParameters$dates$mon2 <<- GeneralParameters$date.range$end.mon
            GeneralParameters$dates$dek2 <<- GeneralParameters$date.range$end.dek
        })

        tkgrid(chk.Moni, row = 0, column = 0, rowspan = 1, columnspan = 1, padx = 1, ipadx = 1)
        tkgrid(bt.DateRange, row = 1, column = 0, sticky = 'e', rowspan = 1, columnspan = 1, padx = 1, ipadx = 10)

        ###############

        tkbind(chk.Moni, "<Button-1>", function(){
            if(tclvalue(monitoring) == "0"){
                if(tclvalue(decileDataExist) == '0'){
                    statedates <- 'normal'
                    statedatedek <<- if(str_trim(tclvalue(out.spifreq)) == 'month') 'disabled' else 'normal'
                    tclvalue(txt.save.var) <- "Index file (Decile.rds) for Deciles data"

                    tkconfigure(bt.outSPI, command = function(){
                        path.rds <- tclvalue(tkgetOpenFile(initialdir = getwd(), filetypes = .cdtEnv$tcl$data$filetypes6))
                        tclvalue(outSPIdir) <- if(path.rds %in% c("", "NA") | is.na(path.rds)) "" else path.rds
                    })
                }else{
                    statedates <- 'disabled'
                    statedatedek <<- 'disabled'
                }
                stateBasePeriod <- 'disabled'
            }else{
                statedates <- 'disabled'
                statedatedek <<- 'disabled'
                if(tclvalue(decileDataExist) == '0'){
                    stateBasePeriod <- 'normal'
                    tclvalue(txt.save.var) <- "Directory to save the outputs"
                    tkconfigure(bt.outSPI, command = function(){
                        dirSPI <- tk_choose.dir(getwd(), "")
                        tclvalue(outSPIdir) <- if(dirSPI %in% c("", "NA") | is.na(dirSPI)) "" else dirSPI
                    })
                }else{
                    stateBasePeriod <- 'disabled'
                }
            }

            tkconfigure(bt.DateRange, state = statedates)
            tkconfigure(bt.BasePeriod, state = stateBasePeriod)
        })

        #############################

        frameParams <- tkframe(subfr1, relief = 'groove', borderwidth = 2)

        out.spifreq <- tclVar(GeneralParameters$outfreq)
        CbOutVAL <- c("dekad", "month")

        if(GeneralParameters$outfreq == 'dekad'){
            txt.suffix <- '-dekad'
            stateTscale <- "disabled"
            up.tscale <- 1
            val.tscale <- 1
        }else{
            txt.suffix <- '-month'
            stateTscale <- "normal"
            up.tscale <- 60
            val.tscale <- GeneralParameters$tscale
        }
        txt.suffix.var <- tclVar(txt.suffix)

        frameTscale <- tkframe(frameParams)
        txt.SPIfreq <- tklabel(frameTscale, text = "Series", anchor = 'e', justify = 'right')
        cb.SPIfreq <- ttkcombobox(frameTscale, values = CbOutVAL, textvariable = out.spifreq, width = 8)
        txt.Tscale1 <- tklabel(frameTscale, text = "Timescale", anchor = 'e', justify = 'right')
        spin.Tscale <- ttkspinbox(frameTscale, from = 1, to = up.tscale, increment = 1, justify = 'center', width = 2, state = stateTscale)
        tkset(spin.Tscale, val.tscale)
        txt.Tscale2 <- tklabel(frameTscale, text = tclvalue(txt.suffix.var), textvariable = txt.suffix.var, anchor = 'w', justify = 'left')

        tkgrid(txt.SPIfreq, cb.SPIfreq, txt.Tscale1, spin.Tscale, txt.Tscale2)

        ########
        tkbind(cb.SPIfreq, "<<ComboboxSelected>>", function(){
            if(str_trim(tclvalue(out.spifreq)) == 'dekad'){
                stateTscale <- "disabled"
                tclvalue(txt.suffix.var) <- '-dekad'
                tkset(spin.Tscale, 1)
                statedatedek <<- if(tclvalue(monitoring) == "1") "normal" else "disabled"
            }
            if(str_trim(tclvalue(out.spifreq)) == 'month'){
                stateTscale <- "normal"
                tclvalue(txt.suffix.var) <- '-month'
                tkconfigure(spin.Tscale, to = 60)
                statedatedek <<- "disabled"
            }
            tkconfigure(spin.Tscale, state = stateTscale)
        })

        ########
        frameBasePeriod <- tkframe(frameParams)

        stateBasePeriod <- if(GeneralParameters$monitoring) 'disabled' else 'normal'

        bt.BasePeriod <- ttkbutton(frameBasePeriod, text = "Set Base Period", state = stateBasePeriod)

        tkconfigure(bt.BasePeriod, command = function(){
            GeneralParameters[["base.period"]] <<- getInfoBasePeriod(.cdtEnv$tcl$main$win,
                                                                    GeneralParameters[["base.period"]]
                                                                )
        })

        tkgrid(bt.BasePeriod, ipadx = 10)

        ########
        tkgrid(frameTscale, row = 0, column = 0, padx = 1, pady = 1, ipadx = 1, ipady = 1)
        tkgrid(frameBasePeriod, row = 1, column = 0, sticky = 'e', padx = 1, pady = 1, ipadx = 1, ipady = 1)

        #############################

        frameDirSav <- tkframe(subfr1, relief = 'groove', borderwidth = 2)

        outSPIdir <- tclVar(GeneralParameters$outdir)

        if(GeneralParameters$monitoring){
            text.save <- "Index file (Decile.rds) for Decile data"
        }else{
            text.save <- "Directory to save the outputs"
        }
        txt.save.var <- tclVar(text.save)

        txt.outSPI <- tklabel(frameDirSav, text = tclvalue(txt.save.var), textvariable = txt.save.var, anchor = 'w', justify = 'left')
        en.outSPI <- tkentry(frameDirSav, textvariable = outSPIdir, width = largeur2)
        bt.outSPI <- tkbutton(frameDirSav, text = "...")

        ######

        tkconfigure(bt.outSPI, command = function(){
            if(GeneralParameters$monitoring){
                path.rds <- tclvalue(tkgetOpenFile(initialdir = getwd(), filetypes = .cdtEnv$tcl$data$filetypes6))
                tclvalue(outSPIdir) <- if(path.rds %in% c("", "NA") | is.na(path.rds)) "" else path.rds
            }else{
                dirSPI <- tk_choose.dir(getwd(), "")
                tclvalue(outSPIdir) <- if(dirSPI %in% c("", "NA") | is.na(dirSPI)) "" else dirSPI
            }
        })

        ######
        tkgrid(txt.outSPI, row = 0, column = 0, sticky = 'we', rowspan = 1, columnspan = 5, padx = 1, pady = 0, ipadx = 1, ipady = 1)
        tkgrid(en.outSPI, row = 1, column = 0, sticky = 'we', rowspan = 1, columnspan = 4, padx = 0, pady = 0, ipadx = 1, ipady = 1)
        tkgrid(bt.outSPI, row = 1, column = 4, sticky = 'w', rowspan = 1, columnspan = 1, padx = 0, pady = 0, ipadx = 1, ipady = 1)

        infobulle(en.outSPI, 'Enter the full path to directory to save outputs')
        status.bar.display(en.outSPI, 'Enter the full path to directory to save outputs')
        infobulle(bt.outSPI, 'or browse here')
        status.bar.display(bt.outSPI, 'or browse here')

        #############################

        calculateBut <- ttkbutton(subfr1, text = "Calculate")

        #################

        tkconfigure(calculateBut, command = function(){
            GeneralParameters$intstep <- periodVAL[CbperiodVAL %in% str_trim(tclvalue(timeSteps))]
            GeneralParameters$data.type <- datatypeVAL[CbdatatypeVAL %in% str_trim(tclvalue(DataType))]

            if(str_trim(tclvalue(DataType)) == CbdatatypeVAL[1])
                GeneralParameters$cdtstation <- str_trim(tclvalue(input.file))
            if(str_trim(tclvalue(DataType)) == CbdatatypeVAL[2])
                GeneralParameters$cdtdataset <- str_trim(tclvalue(input.file))

            GeneralParameters$monitoring <- switch(tclvalue(monitoring), '0' = FALSE, '1' = TRUE)

            GeneralParameters$outfreq <- str_trim(tclvalue(out.spifreq))
            GeneralParameters$tscale <- as.numeric(str_trim(tclvalue(tkget(spin.Tscale))))

            GeneralParameters$outdir <- str_trim(tclvalue(outSPIdir))
            GeneralParameters$Indices <- "Decile"

            # assign('GeneralParameters', GeneralParameters, envir = .GlobalEnv)

            Insert.Messages.Out("Calculate Deciles ......", TRUE, "i")

            tkconfigure(.cdtEnv$tcl$main$win, cursor = 'watch')
            tcl('update')
            ret <- tryCatch(
                {
                    computeDecileProcs(GeneralParameters)
                },
                warning = function(w) warningFun(w),
                error = function(e) errorFun(e),
                finally = {
                    tkconfigure(.cdtEnv$tcl$main$win, cursor = '')
                    tcl('update')
                }
            )

            msg0 <- "Deciles calculation finished successfully"
            msg1 <- "Deciles calculation failed"

            if(!is.null(ret)){
                if(ret == 0){
                    Insert.Messages.Out(msg0, TRUE, "s")

                    .cdtData$EnvData$plot.maps$data.type <- .cdtData$EnvData$output$params$data.type
                    .cdtData$EnvData$plot.maps[c('lon', 'lat', 'id')] <- .cdtData$EnvData$output$data[c('lon', 'lat', 'id')]
                    ###################

                    widgets.Station.Pixel()
                    set.plot.type()
                    ret <- try(set.Data.Scales(), silent = TRUE)
                    if(inherits(ret, "try-error") | is.null(ret)) return(NULL)

                    ret <- try(set.Data.Dates(), silent = TRUE)
                    if(inherits(ret, "try-error") | is.null(ret)) return(NULL)
                }else Insert.Messages.Out(msg1, format = TRUE)
            }else Insert.Messages.Out(msg1, format = TRUE)
        })

        ############################################

        tkgrid(frameTimeS, row = 0, column = 0, sticky = '', padx = 1, pady = 1, ipadx = 1, ipady = 1)
        tkgrid(frameInData, row = 1, column = 0, sticky = 'we', padx = 1, pady = 1, ipadx = 1, ipady = 1)
        tkgrid(frameMoni, row = 2, column = 0, sticky = '', padx = 1, pady = 1, ipadx = 1, ipady = 1)
        tkgrid(frameParams, row = 3, column = 0, sticky = '', padx = 1, pady = 1, ipadx = 1, ipady = 1)
        tkgrid(frameDirSav, row = 4, column = 0, sticky = 'we', padx = 1, pady = 1, ipadx = 1, ipady = 1)
        tkgrid(calculateBut, row = 5, column = 0, sticky = 'we', padx = 1, pady = 3, ipadx = 1, ipady = 1)

    #######################################################################################################

    #Tab2
    subfr2 <- bwTabScrollableFrame(cmd.tab2)

        ##############################################

        frameDataExist <- ttklabelframe(subfr2, text = "Decile data", relief = 'groove')

        decileDataExist <- tclVar(0)
        file.dataIndex <- tclVar()

        stateExistData <- if(tclvalue(decileDataExist) == "1") "normal" else "disabled"

        chk.dataIdx <- tkcheckbutton(frameDataExist, variable = decileDataExist, text = "Decile data already computed", anchor = 'w', justify = 'left')
        en.dataIdx <- tkentry(frameDataExist, textvariable = file.dataIndex, width = largeur2, state = stateExistData)
        bt.dataIdx <- tkbutton(frameDataExist, text = .cdtEnv$tcl$lang$global[['button']][['6']], state = stateExistData)

        tkgrid(chk.dataIdx, row = 0, column = 0, sticky = 'we', rowspan = 1, columnspan = 4, padx = 1, pady = 1, ipadx = 1, ipady = 1)
        tkgrid(bt.dataIdx, row = 0, column = 4, sticky = 'e', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
        tkgrid(en.dataIdx, row = 1, column = 0, sticky = 'we', rowspan = 1, columnspan = 5, padx = 1, pady = 1, ipadx = 1, ipady = 1)

        ###############

        tkconfigure(bt.dataIdx, command = function(){
            path.Stat <- tclvalue(tkgetOpenFile(initialdir = getwd(), filetypes = .cdtEnv$tcl$data$filetypes6))
            if(path.Stat %in% c("", "NA") | is.na(path.Stat)) return(NULL)
            tclvalue(file.dataIndex) <- path.Stat

            if(file.exists(str_trim(tclvalue(file.dataIndex)))){
                OutSPIdata <- try(readRDS(str_trim(tclvalue(file.dataIndex))), silent = TRUE)
                if(inherits(OutSPIdata, "try-error")){
                    Insert.Messages.Out('Unable to load decile data', format = TRUE)
                    Insert.Messages.Out(gsub('[\r\n]', '', OutSPIdata[1]), format = TRUE)
                    tkconfigure(cb.spi.maps, values = "")
                    tclvalue(.cdtData$EnvData$spi.tscale) <- ""
                    tkconfigure(cb.spi.Date, values = "")
                    tclvalue(.cdtData$EnvData$spi.date) <- ""
                    return(NULL)
                }

                .cdtData$EnvData$output <- OutSPIdata
                .cdtData$EnvData$PathData <- dirname(str_trim(tclvalue(file.dataIndex)))
                .cdtData$EnvData$plot.maps$data.type <- .cdtData$EnvData$output$params$data.type
                .cdtData$EnvData$plot.maps[c('lon', 'lat', 'id')] <- .cdtData$EnvData$output$data[c('lon', 'lat', 'id')]
                ###################

                widgets.Station.Pixel()
                set.plot.type()
                ret <- try(set.Data.Scales(), silent = TRUE)
                if(inherits(ret, "try-error") | is.null(ret)) return(NULL)

                ret <- try(set.Data.Dates(), silent = TRUE)
                if(inherits(ret, "try-error") | is.null(ret)) return(NULL)
            }
        })

        ###############

        tkbind(chk.dataIdx, "<Button-1>", function(){
            stateExistData <- if(tclvalue(decileDataExist) == '1') 'disabled' else 'normal'
            tkconfigure(en.dataIdx, state = stateExistData)
            tkconfigure(bt.dataIdx, state = stateExistData)
            stateCaclBut <- if(tclvalue(decileDataExist) == '1') 'normal' else 'disabled'
            tkconfigure(calculateBut, state = stateCaclBut)
            tkconfigure(cb.fperiod, state = stateCaclBut)
            tkconfigure(cb.datatype, state = stateCaclBut)
            tkconfigure(cb.en.infile, state = stateCaclBut)
            tkconfigure(bt.infile, state = stateCaclBut)
            tkconfigure(chk.Moni, state = stateCaclBut)
            tkconfigure(cb.SPIfreq, state = stateCaclBut)
            tkconfigure(en.outSPI, state = stateCaclBut)
            tkconfigure(bt.outSPI, state = stateCaclBut)

            if(tclvalue(decileDataExist) == '1'){
                statedates <- if(tclvalue(monitoring) == "1") "normal" else "disabled"
                statedatedek <<- if(str_trim(tclvalue(out.spifreq)) == 'month') 'normal' else 'disabled'
                stateBasePeriod <- if(tclvalue(monitoring) == "1") 'disabled' else 'normal'
            }else{
                statedates <- 'disabled'
                statedatedek <<- 'disabled'
                stateBasePeriod <- 'disabled'
            }

            tkconfigure(bt.DateRange, state = statedates)
            tkconfigure(spin.Tscale, state = statedatedek)
            tkconfigure(bt.BasePeriod, state = stateBasePeriod)
        })

        ##############################################

        frameSPIMap <- ttklabelframe(subfr2, text = "Decile Map", relief = 'groove')

        .cdtData$EnvData$spi.tscale <- tclVar()
        .cdtData$EnvData$spi.date <- tclVar()

        cb.spi.maps <- ttkcombobox(frameSPIMap, values = "", textvariable = .cdtData$EnvData$spi.tscale, width = largeur4)
        bt.spi.maps <- ttkbutton(frameSPIMap, text = .cdtEnv$tcl$lang$global[['button']][['3']], width = 7)
        cb.spi.Date <- ttkcombobox(frameSPIMap, values = "", textvariable = .cdtData$EnvData$spi.date, width = largeur5, justify = 'center')
        bt.spi.Date.prev <- ttkbutton(frameSPIMap, text = "<<", width = 3)
        bt.spi.Date.next <- ttkbutton(frameSPIMap, text = ">>", width = 3)
        bt.spi.MapOpt <- ttkbutton(frameSPIMap, text = .cdtEnv$tcl$lang$global[['button']][['4']], width = 7)

        ###############

        .cdtData$EnvData$tab$pointSize <- NULL
        .cdtData$EnvData$dataMapOp <- list(presetCol = list(color = 'decile.colors', reverse = FALSE),
                                            userCol = list(custom = FALSE, color = NULL),
                                            userLvl = list(custom = TRUE, levels = c(1, 1.5, 3.5, 5.5, 7.5, 9.5, 10), equidist = TRUE),
                                            title = list(user = FALSE, title = ''),
                                            colkeyLab = list(user = FALSE, label = ''),
                                            scalebar = list(add = FALSE, pos = 'bottomleft'),
                                            pointSize = .cdtData$EnvData$tab$pointSize)

        tkconfigure(bt.spi.MapOpt, command = function(){
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

            if(str_trim(tclvalue(.cdtData$EnvData$plot.maps$plot.type)) == "Points")
                .cdtData$EnvData$tab$pointSize <- .cdtData$EnvData$dataMapOp$pointSize
        })

        ###############

        .cdtData$EnvData$tab$dataMap <- NULL

        tkconfigure(bt.spi.maps, command = function(){
            if(str_trim(tclvalue(.cdtData$EnvData$spi.date)) != "" &
                !is.null(.cdtData$EnvData$varData))
            {
                get.Data.Map()

                SPICalc.Display.Maps('Decile - Map', 'Decile - Time Series')
            }
        })

        tkconfigure(bt.spi.Date.prev, command = function(){
            if(str_trim(tclvalue(.cdtData$EnvData$spi.date)) != ""){
                donDates <- .cdtData$EnvData$varData$ts$dates
                idaty <- which(donDates == str_trim(tclvalue(.cdtData$EnvData$spi.date)))
                idaty <- idaty - 1
                if(idaty < 1) idaty <- length(donDates)
                tclvalue(.cdtData$EnvData$spi.date) <- donDates[idaty]
                get.Data.Map()

                SPICalc.Display.Maps('Decile - Map', 'Decile - Time Series')
            }
        })

        tkconfigure(bt.spi.Date.next, command = function(){
            if(str_trim(tclvalue(.cdtData$EnvData$spi.date)) != ""){
                donDates <- .cdtData$EnvData$varData$ts$dates
                idaty <- which(donDates == str_trim(tclvalue(.cdtData$EnvData$spi.date)))
                idaty <- idaty + 1
                if(idaty > length(donDates)) idaty <- 1
                tclvalue(.cdtData$EnvData$spi.date) <- donDates[idaty]
                get.Data.Map()

                SPICalc.Display.Maps('Decile - Map', 'Decile - Time Series')
            }
        })

        ###############
        tkgrid(cb.spi.maps, row = 0, column = 0, sticky = 'we', rowspan = 1, columnspan = 4, padx = 1, pady = 1, ipadx = 1, ipady = 1)
        tkgrid(bt.spi.maps, row = 0, column = 4, sticky = '', rowspan = 1, columnspan = 2, padx = 1, pady = 1, ipadx = 1, ipady = 1)
        tkgrid(bt.spi.Date.prev, row = 1, column = 0, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
        tkgrid(cb.spi.Date, row = 1, column = 1, sticky = 'we', rowspan = 1, columnspan = 2, padx = 1, pady = 1, ipadx = 1, ipady = 1)
        tkgrid(bt.spi.Date.next, row = 1, column = 3, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
        tkgrid(bt.spi.MapOpt, row = 1, column = 4, sticky = 'we', rowspan = 1, columnspan = 2, padx = 1, pady = 1, ipadx = 1, ipady = 1)

        ###############
        tkbind(cb.spi.maps, "<<ComboboxSelected>>", function(){
            ret <- try(set.Data.Dates(), silent = TRUE)
            if(inherits(ret, "try-error") | is.null(ret)) return(NULL)
        })

        ##############################################

        framePlotType <- tkframe(subfr2)

        .cdtData$EnvData$plot.maps$plot.type <- tclVar("Pixels")

        txt.plotType <- tklabel(framePlotType, text = "Plot Type", anchor = 'e', justify = 'right')
        cb.plotType <- ttkcombobox(framePlotType, values = "Pixels", textvariable = .cdtData$EnvData$plot.maps$plot.type, width = largeur5)

        tkgrid(txt.plotType, row = 0, column = 0, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
        tkgrid(cb.plotType, row = 0, column = 1, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)

        ###############

        tkbind(cb.plotType, "<<ComboboxSelected>>", function(){
            if(str_trim(tclvalue(.cdtData$EnvData$spi.date)) != "")
                get.Data.Map()
        })

        ##############################################

        tkgrid(frameDataExist, row = 0, column = 0, sticky = 'we', padx = 1, pady = 1, ipadx = 1, ipady = 1)
        tkgrid(frameSPIMap, row = 1, column = 0, sticky = 'we', padx = 1, pady = 3, ipadx = 1, ipady = 1)
        tkgrid(framePlotType, row = 2, column = 0, sticky = '', padx = 1, pady = 3, ipadx = 1, ipady = 1)

    #######################################################################################################

    #Tab3
    subfr3 <- bwTabScrollableFrame(cmd.tab3)

        ##############################################

        frameDataTS <- ttklabelframe(subfr3, text = "Decile Graph", relief = 'groove')

        typeTSPLOT <- c("Bar-Line", "Polygon")
        .cdtData$EnvData$plot.maps$typeTSp <- tclVar("Bar-Line")

        cb.typeTSp <- ttkcombobox(frameDataTS, values = typeTSPLOT, textvariable = .cdtData$EnvData$plot.maps$typeTSp, width = largeur5)
        bt.TsGraph.plot <- ttkbutton(frameDataTS, text = .cdtEnv$tcl$lang$global[['button']][['3']], width = 7)
        bt.TSGraphOpt <- ttkbutton(frameDataTS, text = .cdtEnv$tcl$lang$global[['button']][['4']], width = 8)

        #################

        .cdtData$EnvData$TSGraphOp <- list(
                                        bar.line = list(
                                            xlim = list(is.min = FALSE, min = "1981-1-1", is.max = FALSE, max = "2017-12-3"),
                                            ylim = list(is.min = FALSE, min = -10, is.max = FALSE, max = 10),
                                            userYTcks = list(custom = TRUE, ticks = c(1, 1.5, 3.5, 5.5, 7.5, 9.5, 10)),
                                            axislabs = list(is.xlab = FALSE, xlab = '', is.ylab = FALSE, ylab = ''),
                                            title = list(is.title = FALSE, title = '', position = 'top'),
                                            colors = list(y0 = 5.5, negative = 'darkorange2', positive = 'steelblue3'),
                                            line = list(plot = FALSE, col = "black", lwd = 1.5)
                                        )
                                    )

        tkconfigure(bt.TSGraphOpt, command = function(){
            suffix.fun <- switch(str_trim(tclvalue(.cdtData$EnvData$plot.maps$typeTSp)),
                                    "Bar-Line" = "Bar.Line",
                                    "Polygon" = "Bar.Line")
            plot.fun <- get(paste0("MapGraph.GraphOptions.", suffix.fun), mode = "function")
            .cdtData$EnvData$TSGraphOp <- plot.fun(.cdtData$EnvData$TSGraphOp)
        })

        #########
        .cdtData$EnvData$tab$dataGraph <- NULL

        tkconfigure(bt.TsGraph.plot, command = function(){
            if(!is.null(.cdtData$EnvData$varData)){
                imgContainer <- CDT.Display.Graph(SPICalc.Plot.Graph, .cdtData$EnvData$tab$dataGraph, 'Decile - Time Series')
                .cdtData$EnvData$tab$dataGraph <- imageNotebookTab_unik(imgContainer, .cdtData$EnvData$tab$dataGraph)
            }
        })

        #################

        tkgrid(cb.typeTSp, row = 0, column = 0, sticky = 'we', pady = 1, columnspan = 1)
        tkgrid(bt.TSGraphOpt, row = 0, column = 1, sticky = 'we', padx = 4, pady = 1, columnspan = 1)
        tkgrid(bt.TsGraph.plot, row = 0, column = 2, sticky = 'we', pady = 1, columnspan = 1)

        ##############################################

        frameSTNCrds <- ttklabelframe(subfr3, text = "Station/Coordinates", relief = 'groove')

        frTS2 <- tkframe(frameSTNCrds)
        .cdtData$EnvData$plot.maps$lonLOC <- tclVar()
        .cdtData$EnvData$plot.maps$latLOC <- tclVar()
        .cdtData$EnvData$plot.maps$stnIDTSp <- tclVar()

        tkgrid(frTS2, row = 0, column = 0, sticky = 'e', pady = 1)

        ##############################################

        frameVizTS <- tkframe(subfr3, relief = 'groove', borderwidth = 2)

        .cdtData$EnvData$spiViz$max.tscale <- tclVar(12)

        bt.VizTS <- ttkbutton(frameVizTS, text = "Visualizing time-scales")
        bt.VizOpt <- ttkbutton(frameVizTS, text = .cdtEnv$tcl$lang$global[['button']][['4']])
        txt.VizTS <- tklabel(frameVizTS, text = "Maximum time-scale", anchor = 'e', justify = 'right')
        en.VizTS <- tkentry(frameVizTS, textvariable = .cdtData$EnvData$spiViz$max.tscale, width = 3)

        ###############

        .cdtData$EnvData$spiVizOp <- list(presetCol = list(color = 'decile.colors', reverse = FALSE),
                                        userCol = list(custom = FALSE, color = NULL),
                                        userLvl = list(custom = TRUE, levels = c(1, 1.5, 3.5, 5.5, 7.5, 9.5, 10), equidist = TRUE),
                                        title = list(user = FALSE, title = ''),
                                        colkeyLab = list(user = FALSE, label = ''),
                                        axislabs = list(is.xlab = FALSE, xlab = '', is.ylab = TRUE, ylab = 'Time-scale (months)'))

        tkconfigure(bt.VizOpt, command = function(){
            .cdtData$EnvData$spiVizOp <- MapGraph.SpiVizOptions(.cdtData$EnvData$spiVizOp)
        })

        ###############

        .cdtData$EnvData$notebookTab.spiViz <- NULL

        tkconfigure(bt.VizTS, command = function(){
            if(!is.null(.cdtData$EnvData$varData)){
                ret <- try(get.Data.spiViz(), silent = TRUE)
                if(inherits(ret, "try-error") | is.null(ret)) return(NULL)

                imgContainer <- CDT.Display.Graph(SPICalc.Plot.VizTS, .cdtData$EnvData$tab$spiViz, 'Decile - Time Scales')
                .cdtData$EnvData$tab$spiViz <- imageNotebookTab_unik(imgContainer, .cdtData$EnvData$tab$spiViz)
            }
        })

        ###############

        tkgrid(bt.VizTS, row = 0, column = 0, sticky = 'we', padx = 3, ipadx = 1, pady = 1)
        tkgrid(bt.VizOpt, row = 0, column = 1, sticky = 'we', padx = 3, ipadx = 1, pady = 1)
        tkgrid(txt.VizTS, row = 1, column = 0, sticky = 'e', padx = 3, ipadx = 1, pady = 1)
        tkgrid(en.VizTS, row = 1, column = 1, sticky = 'w', padx = 3, ipadx = 1, pady = 1)

        ##############################################

        tkgrid(frameDataTS, row = 0, column = 0, sticky = 'we', pady = 1)
        tkgrid(frameSTNCrds, row = 1, column = 0, sticky = '', pady = 3)
        tkgrid(frameVizTS, row = 2, column = 0, sticky = '', pady = 3)

    #######################################################################################################

    #Tab4
    subfr4 <- bwTabScrollableFrame(cmd.tab4)

        ##############################################

        frameSHP <- ttklabelframe(subfr4, text = "Boundaries", relief = 'groove')

        .cdtData$EnvData$shp$add.shp <- tclVar(FALSE)
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

        tkgrid(frameSHP, row = 0, column = 0, sticky = 'we', pady = 1)

    #######################################################################################################

    widgets.Station.Pixel <- function(){
        tkdestroy(frTS2)
        frTS2 <<- tkframe(frameSTNCrds)

        if(.cdtData$EnvData$output$params$data.type == "cdtstation"){
            stnIDTSPLOT <- .cdtData$EnvData$output$data$id
            txt.stnSel <- tklabel(frTS2, text = "Select a station to plot")
            bt.stnID.prev <- ttkbutton(frTS2, text = "<<", width = 6)
            bt.stnID.next <- ttkbutton(frTS2, text = ">>", width = 6)
            cb.stnID <- ttkcombobox(frTS2, values = stnIDTSPLOT, textvariable = .cdtData$EnvData$plot.maps$stnIDTSp, width = largeur5)
            tclvalue(.cdtData$EnvData$plot.maps$stnIDTSp) <- stnIDTSPLOT[1]

            tkconfigure(bt.stnID.prev, command = function(){
                if(!is.null(.cdtData$EnvData$varData)){
                    istn <- which(stnIDTSPLOT == str_trim(tclvalue(.cdtData$EnvData$plot.maps$stnIDTSp)))
                    istn <- istn - 1
                    if(istn < 1) istn <- length(stnIDTSPLOT)
                    tclvalue(.cdtData$EnvData$plot.maps$stnIDTSp) <- stnIDTSPLOT[istn]

                    imgContainer <- CDT.Display.Graph(SPICalc.Plot.Graph, .cdtData$EnvData$tab$dataGraph, 'Decile - Time Series')
                    .cdtData$EnvData$tab$dataGraph <- imageNotebookTab_unik(imgContainer, .cdtData$EnvData$tab$dataGraph)
                }
            })

            tkconfigure(bt.stnID.next, command = function(){
                if(!is.null(.cdtData$EnvData$varData)){
                    istn <- which(stnIDTSPLOT == str_trim(tclvalue(.cdtData$EnvData$plot.maps$stnIDTSp)))
                    istn <- istn + 1
                    if(istn > length(stnIDTSPLOT)) istn <- 1
                    tclvalue(.cdtData$EnvData$plot.maps$stnIDTSp) <- stnIDTSPLOT[istn]

                    imgContainer <- CDT.Display.Graph(SPICalc.Plot.Graph, .cdtData$EnvData$tab$dataGraph, 'Decile - Time Series')
                    .cdtData$EnvData$tab$dataGraph <- imageNotebookTab_unik(imgContainer, .cdtData$EnvData$tab$dataGraph)
                }
            })

            tkgrid(txt.stnSel, row = 0, column = 0, sticky = '', rowspan = 1, columnspan = 3, padx = 1, pady = 1, ipadx = 1, ipady = 1)
            tkgrid(bt.stnID.prev, row = 1, column = 0, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
            tkgrid(cb.stnID, row = 1, column = 1, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
            tkgrid(bt.stnID.next, row = 1, column = 2, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
        }else{
            txt.crdSel <- tklabel(frTS2, text = "Enter longitude and latitude to plot", anchor = 'w', justify = 'left')
            txt.lonLoc <- tklabel(frTS2, text = "Longitude", anchor = 'e', justify = 'right')
            en.lonLoc <- tkentry(frTS2, textvariable = .cdtData$EnvData$plot.maps$lonLOC, width = 8)
            txt.latLoc <- tklabel(frTS2, text = "Latitude", anchor = 'e', justify = 'right')
            en.latLoc <- tkentry(frTS2, textvariable = .cdtData$EnvData$plot.maps$latLOC, width = 8)
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

            .cdtData$EnvData$dataMapOp$pointSize <- 0.7
        }else{
            plot.type <- c("Pixels", "FilledContour")
            .cdtData$EnvData$plot.maps$.data.type <- "Grid"
        }
        tkconfigure(cb.plotType, values = plot.type)
    }

    #################

    set.Data.Scales <- function(){
        path.data <- file.path(.cdtData$EnvData$PathData, "CDTDATASET")
        spi.tscales <- list.files(path.data, "Decile_.+")
        if(length(spi.tscales) == 0){
            Insert.Messages.Out('No Decile data found', format = TRUE)
            return(NULL)
        }
        if(.cdtData$EnvData$output$params$data.type == "cdtstation")
            spi.tscales <- tools::file_path_sans_ext(spi.tscales)

        nch <- nchar(spi.tscales)
        tsc <- str_pad(substr(spi.tscales, 8, nch - 3), 2, pad = "0")
        scales <- substr(spi.tscales, nch - 2, nch)

        spi.tscalesF <- spi.tscales[order(paste0(scales, tsc))]
        spi.tscales <- paste0("Decile-", as.numeric(tsc), "-", ifelse(scales == "dek", "Dekad", "Month"))
        spi.tscales <- spi.tscales[order(paste0(scales, tsc))]

        .cdtData$EnvData$varData$spi$disp <- spi.tscales
        .cdtData$EnvData$varData$spi$dataF <- spi.tscalesF

        tkconfigure(cb.spi.maps, values = spi.tscales)
        tclvalue(.cdtData$EnvData$spi.tscale) <- spi.tscales[1]
        return(0)
    }

    #################

    set.Data.Dates <- function(){
        path.data <- file.path(.cdtData$EnvData$PathData, "CDTDATASET")
        spi_scale <- str_trim(tclvalue(.cdtData$EnvData$spi.tscale))

        ipos <- which(.cdtData$EnvData$varData$spi$disp %in% spi_scale)
        tscale.data <- .cdtData$EnvData$varData$spi$dataF[ipos]
        file.index <- 
            if(.cdtData$EnvData$output$params$data.type == "cdtstation")
                file.path(path.data, paste0(tscale.data, ".rds"))
            else
                file.path(path.data, tscale.data, paste0(tscale.data, ".rds"))

        if(!file.exists(file.index)){
            Insert.Messages.Out(paste(file.index, 'not found'), format = TRUE)
            return(NULL)
        }

        read.cdt.dataIdx <- TRUE
        if(!is.null(.cdtData$EnvData$cdtdataset))
            if(!is.null(.cdtData$EnvData$file.index))
                if(.cdtData$EnvData$file.index == file.index) read.cdt.dataIdx <- FALSE
        if(read.cdt.dataIdx){
            cdtdataset <- readRDS(file.index)
            daty <- if(.cdtData$EnvData$output$params$data.type == "cdtstation") cdtdataset$date else cdtdataset$dateInfo$date

            tkconfigure(cb.spi.Date, values = daty)
            tclvalue(.cdtData$EnvData$spi.date) <- daty[length(daty)]

            .cdtData$EnvData$varData$ts$step <- strsplit(spi_scale, "-")[[1]][3]
            .cdtData$EnvData$varData$ts$dates <- daty
            .cdtData$EnvData$cdtdataset <- cdtdataset
            .cdtData$EnvData$cdtdataset$fileInfo <- file.index
            .cdtData$EnvData$file.index <- file.index
        }

        return(0)
    }

    #################

    get.Data.Map <- function(){
        tkconfigure(.cdtEnv$tcl$main$win, cursor = 'watch')
        tcl('update')
        on.exit({
            tkconfigure(.cdtEnv$tcl$main$win, cursor = '')
            tcl('update')
        })

        this.daty <- str_trim(tclvalue(.cdtData$EnvData$spi.date))

        readVarData <- TRUE
        if(!is.null(.cdtData$EnvData$varData))
            if(!is.null(.cdtData$EnvData$varData$spi$this.daty))
                if(.cdtData$EnvData$varData$spi$this.daty == this.daty) readVarData <- FALSE

        if(.cdtData$EnvData$output$params$data.type == "cdtstation"){
            change.plot <- str_trim(tclvalue(.cdtData$EnvData$plot.maps$plot.type))

            if(!readVarData)
                if(.cdtData$EnvData$change.plot != change.plot) readVarData <- TRUE

            if(readVarData){
                idt <- which(.cdtData$EnvData$cdtdataset$date == this.daty)

                x <- .cdtData$EnvData$output$data$lon
                y <- .cdtData$EnvData$output$data$lat
                tmp <- as.numeric(.cdtData$EnvData$cdtdataset$decile[idt, ])

                if(change.plot == "Pixels"){
                    nx <- nx_ny_as.image(diff(range(x)))
                    ny <- nx_ny_as.image(diff(range(y)))
                    tmp <- cdt.as.image(tmp, nx = nx, ny = ny, pts.xy = cbind(x, y))
                    .cdtData$EnvData$varData$map$x <- tmp$x
                    .cdtData$EnvData$varData$map$y <- tmp$y
                    .cdtData$EnvData$varData$map$z <- tmp$z
                }

                if(change.plot == "Points"){
                    .cdtData$EnvData$varData$map$x <- x
                    .cdtData$EnvData$varData$map$y <- y
                    .cdtData$EnvData$varData$map$z <- tmp
                }

                .cdtData$EnvData$varData$spi$this.daty <- this.daty
                .cdtData$EnvData$change.plot <- change.plot
            }
        }else{
            if(readVarData){
                ipos <- which(.cdtData$EnvData$varData$spi$disp %in% str_trim(tclvalue(.cdtData$EnvData$spi.tscale)))
                tscale.data <- .cdtData$EnvData$varData$spi$dataF[ipos]

                nc.file <- file.path(.cdtData$EnvData$PathData, "DATA_NetCDF", tscale.data, paste0("decile_", this.daty, ".nc"))
                nc <- nc_open(nc.file)
                .cdtData$EnvData$varData$map$x <- nc$dim[[1]]$vals
                .cdtData$EnvData$varData$map$y <- nc$dim[[2]]$vals
                .cdtData$EnvData$varData$map$z <- ncvar_get(nc, varid = nc$var[[1]]$name)
                nc_close(nc)

                .cdtData$EnvData$varData$spi$this.daty <- this.daty
            }
        }
    }

    #################

    get.Data.spiViz <- function(){
        file.mon <- file.path(.cdtData$EnvData$PathData, "MONTHLY_data")
        file.dek <- file.path(.cdtData$EnvData$PathData, "DEKADAL_data")

        if(file.exists(file.mon)){
            file.index <- file.mon
            viztstep <- "monthly"
        }else{
            if(file.exists(file.dek)){
                file.index <- file.dek
                viztstep <- "dekadal"
            }else{
                Insert.Messages.Out('No dekadal or monthly data found', format = TRUE)
                return(NULL)
            }
        }

        readspiVizData <- TRUE
        if(!is.null(.cdtData$EnvData$spiViz))
            if(!is.null(.cdtData$EnvData$spiViz$tstep))
                if(.cdtData$EnvData$spiViz$tstep == viztstep) readspiVizData <- FALSE

        if(readspiVizData){
            file.index <- file.path(file.index, paste0(basename(file.index), ".rds"))
            .cdtData$EnvData$spiViz$cdtdataset <- readRDS(file.index)
            .cdtData$EnvData$spiViz$cdtdataset$fileInfo <- file.index
            .cdtData$EnvData$spiViz$tstep <- viztstep
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
