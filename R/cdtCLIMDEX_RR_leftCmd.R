
climdexPanelCmd.RR <- function(){
    listOpenFiles <- openFile_ttkcomboList()
    if(WindowsOS()){
        largeur0 <- .cdtEnv$tcl$fun$w.widgets(22)
        largeur1 <- .cdtEnv$tcl$fun$w.widgets(31)
        largeur2 <- .cdtEnv$tcl$fun$w.widgets(33)
        largeur3 <- 30
        largeur4 <- 23
        largeur5 <- 22
    }else{
        largeur0 <- .cdtEnv$tcl$fun$w.widgets(20)
        largeur1 <- .cdtEnv$tcl$fun$w.widgets(22)
        largeur2 <- .cdtEnv$tcl$fun$w.widgets(23)
        largeur3 <- 22
        largeur4 <- 14
        largeur5 <- 14
    }

    GeneralParameters <- list(data.type = "cdtstation", cdtstation = "", cdtdataset = "",
                            baseYear = list(all.years = TRUE, start.year = 1981, end.year = 2010, min.year = 15),
                            Indices = list(Rx1day = TRUE, Rx5day = TRUE, SDII = TRUE, R10mm = TRUE, R20mm = TRUE,
                                            Rnnmm = TRUE, CDD = TRUE, CWD = TRUE, R95pTOT = TRUE, R99pTOT = TRUE,
                                            PRCPTOT = TRUE, thres.Rnnmm = 25),
                            start.july = FALSE, output = "")

    # xml.dlg <- file.path(.cdtDir$dirLocal, "languages", "cdtCLIMDEX_RR_leftCmd.xml")
    # lang.dlg <- cdtLanguageParse(xml.dlg, .cdtData$Config$lang.iso)
    # .cdtData$EnvData$message <- lang.dlg[['message']]

    ###################

    .cdtEnv$tcl$main$cmd.frame <- tkframe(.cdtEnv$tcl$main$panel.left)

    tknote.cmd <- bwNoteBook(.cdtEnv$tcl$main$cmd.frame)

    cmd.tab1 <- bwAddTab(tknote.cmd, text = "Input")
    cmd.tab2 <- bwAddTab(tknote.cmd, text = "Indices")
    cmd.tab3 <- bwAddTab(tknote.cmd, text = "Maps")
    cmd.tab4 <- bwAddTab(tknote.cmd, text = "Graphs")
    cmd.tab5 <- bwAddTab(tknote.cmd, text = "Boundaries")

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

        frameInData <- ttklabelframe(subfr1, text = 'Input Data', relief = 'groove')

        DataType <- tclVar()
        CbdatatypeVAL <- .cdtEnv$tcl$lang$global[['combobox']][['2']][1:2]
        datatypeVAL <- c('cdtstation', 'cdtdataset')
        tclvalue(DataType) <- CbdatatypeVAL[datatypeVAL %in% GeneralParameters$data.type]

        if(GeneralParameters$data.type == 'cdtstation'){
            input.Prec <- tclVar(GeneralParameters$cdtstation)
            txt.INPrec <- 'File containing stations daily Precip data'
        }else{
            input.Prec <- tclVar(GeneralParameters$cdtdataset)
            txt.INPrec <- 'Index file (*.rds) for daily Precip data'
        }
        txt.INPrec.var <- tclVar(txt.INPrec)

        txt.datatype <- tklabel(frameInData, text = 'Format', anchor = 'w', justify = 'left')
        cb.datatype <- ttkcombobox(frameInData, values = CbdatatypeVAL, textvariable = DataType, width = largeur0)

        txt.INPrec <- tklabel(frameInData, text = tclvalue(txt.INPrec.var), textvariable = txt.INPrec.var, anchor = 'w', justify = 'left')
        if(GeneralParameters$data.type == 'cdtstation'){
            cb.en.INPrec <- ttkcombobox(frameInData, values = unlist(listOpenFiles), textvariable = input.Prec, width = largeur1)
        }else{
            cb.en.INPrec <- tkentry(frameInData, textvariable = input.Prec, width = largeur2)
        }
        bt.INPrec <- tkbutton(frameInData, text = "...")

        ############

        tkconfigure(bt.INPrec, command = function(){
            if(GeneralParameters$data.type == 'cdtstation'){
                dat.opfiles <- getOpenFiles(.cdtEnv$tcl$main$win)
                if(!is.null(dat.opfiles)){
                    update.OpenFiles('ascii', dat.opfiles)
                    listOpenFiles[[length(listOpenFiles) + 1]] <<- dat.opfiles[[1]]
                    tclvalue(input.Prec) <- dat.opfiles[[1]]
                    tkconfigure(cb.en.INPrec, values = unlist(listOpenFiles))
                }
            }else{
                path.rds <- tclvalue(tkgetOpenFile(initialdir = getwd(), filetypes = .cdtEnv$tcl$data$filetypes6))
                tclvalue(input.Prec) <- if(path.rds %in% c("", "NA") | is.na(path.rds)) "" else path.rds
            }
        })

        ############
        tkgrid(txt.datatype, row = 0, column = 0, sticky = 'we', rowspan = 1, columnspan = 2, padx = 1, pady = 1, ipadx = 1, ipady = 1)
        tkgrid(cb.datatype, row = 0, column = 2, sticky = 'we', rowspan = 1, columnspan = 8, padx = 1, pady = 1, ipadx = 1, ipady = 1)
        tkgrid(txt.INPrec, row = 1, column = 0, sticky = 'we', rowspan = 1, columnspan = 10, padx = 1, pady = 1, ipadx = 1, ipady = 1)
        tkgrid(cb.en.INPrec, row = 2, column = 0, sticky = 'we', rowspan = 1, columnspan = 9, padx = 0, pady = 1, ipadx = 1, ipady = 1)
        tkgrid(bt.INPrec, row = 2, column = 9, sticky = 'w', rowspan = 1, columnspan = 1, padx = 0, pady = 1, ipadx = 1, ipady = 1)

        ############

        helpWidget(cb.datatype, 'Select the type of input data', 'Select the type of input data')
        if(GeneralParameters$data.type == 'cdtstation'){
            helpWidget(cb.en.INPrec, 'Select the file containing the daily precipitation',
                                    'Select the file containing the daily precipitation')
            helpWidget(bt.INPrec, 'Browse file if not listed', 'Browse file if not listed')
        }else{
            helpWidget(cb.en.INPrec, 'Enter the full path to the file <daily precipitation dataset name>.rds',
                                    'Enter the full path to the file <daily precipitation dataset name>.rds')
            helpWidget(bt.INPrec, 'or browse here', 'or browse here')
        }

        ############

        tkbind(cb.datatype, "<<ComboboxSelected>>", function(){
            tkdestroy(cb.en.INPrec)
            tclvalue(input.Prec) <- ''

            ###
            if(str_trim(tclvalue(DataType)) == CbdatatypeVAL[1]){
                tclvalue(txt.INPrec.var) <- 'File containing stations daily Precip data'

                cb.en.INPrec <- ttkcombobox(frameInData, values = unlist(listOpenFiles), textvariable = input.Prec, width = largeur1)

                ######
                tkconfigure(bt.INPrec, command = function(){
                    dat.opfiles <- getOpenFiles(.cdtEnv$tcl$main$win)
                    if(!is.null(dat.opfiles)){
                        update.OpenFiles('ascii', dat.opfiles)
                        listOpenFiles[[length(listOpenFiles) + 1]] <<- dat.opfiles[[1]]
                        tclvalue(input.Prec) <- dat.opfiles[[1]]
                        tkconfigure(cb.en.INPrec, values = unlist(listOpenFiles))
                    }
                })

                ######
                helpWidget(cb.en.INPrec, 'Select the file containing the daily precipitation',
                                        'Select the file containing the daily precipitation')
                helpWidget(bt.INPrec, 'Browse file if not listed', 'Browse file if not listed')
            }

            ###
            if(str_trim(tclvalue(DataType)) == CbdatatypeVAL[2]){
                tclvalue(txt.INPrec.var) <- 'Index file (*.rds) for daily Precip data'

                cb.en.INPrec <- tkentry(frameInData, textvariable = input.Prec, width = largeur2)

                ######
                tkconfigure(bt.INPrec, command = function(){
                    path.rds <- tclvalue(tkgetOpenFile(initialdir = getwd(), filetypes = .cdtEnv$tcl$data$filetypes6))
                    tclvalue(input.Prec) <- if(path.rds %in% c("", "NA")) "" else path.rds
                })

                ######
                helpWidget(cb.en.INPrec, 'Enter the full path to the file <daily precipitation dataset name>.rds',
                                        'Enter the full path to the file <daily precipitation dataset name>.rds')
                helpWidget(bt.INPrec, 'or browse here', 'or browse here')
            }

            #######
            tkgrid(cb.en.INPrec, row = 2, column = 0, sticky = 'we', rowspan = 1, columnspan = 9, padx = 0, pady = 1, ipadx = 1, ipady = 1)
        })

        ############################################

        frameDirSav <- tkframe(subfr1, relief = 'groove', borderwidth = 2)

        dir.save <- tclVar(GeneralParameters$output)

        txt.dir.save <- tklabel(frameDirSav, text = "Directory to save indices", anchor = 'w', justify = 'left')
        en.dir.save <- tkentry(frameDirSav, textvariable = dir.save, width = largeur2)
        bt.dir.save <- tkbutton(frameDirSav, text = "...")

        ######
        tkconfigure(bt.dir.save, command = function() fileORdir2Save(dir.save, isFile = FALSE))

        ######
        tkgrid(txt.dir.save, row = 0, column = 0, sticky = 'we', rowspan = 1, columnspan = 5, padx = 1, pady = 0, ipadx = 1, ipady = 1)
        tkgrid(en.dir.save, row = 1, column = 0, sticky = 'we', rowspan = 1, columnspan = 4, padx = 0, pady = 0, ipadx = 1, ipady = 1)
        tkgrid(bt.dir.save, row = 1, column = 4, sticky = 'w', rowspan = 1, columnspan = 1, padx = 0, pady = 0, ipadx = 1, ipady = 1)

        helpWidget(en.dir.save, 'Enter the full path to directory to save the calculated indices',
                                'Enter the full path to directory to save the calculated indices')
        helpWidget(bt.dir.save, 'or browse here', 'or browse here')

        ############################################

        bt.Baseyear <- ttkbutton(subfr1, text = "Set Base Period")

        tkconfigure(bt.Baseyear, command = function(){
            GeneralParameters[["baseYear"]] <<- getInfoBasePeriod(.cdtEnv$tcl$main$win,
                                                        GeneralParameters[["baseYear"]])
        })

        ############################################

        tkgrid(frameInData, row = 0, column = 0, sticky = 'we', padx = 1, pady = 3, ipadx = 1, ipady = 1)
        tkgrid(frameDirSav, row = 1, column = 0, sticky = 'we', padx = 1, pady = 3, ipadx = 1, ipady = 1)
        tkgrid(bt.Baseyear, row = 2, column = 0, sticky = 'we', padx = 1, pady = 3, ipadx = 1, ipady = 1)

    #######################################################################################################

    #Tab2
    subfr2 <- bwTabScrollableFrame(cmd.tab2, hscrlwin = .cdtEnv$tcl$fun$h.scale(45))

    climdex.frame1 <- tkframe(cmd.tab2)
    tkgrid(climdex.frame1, sticky = 'we')

    bt.CalcIndices <- ttkbutton(cmd.tab2, text = "Calculate Indices")
    tkgrid(bt.CalcIndices, sticky = 'we', padx = 1, ipadx = 1)

        #######################

        frameIndex <- tkframe(subfr2, relief = 'sunken', borderwidth = 2)

        is.Rx1day <- tclVar(GeneralParameters$Indices$Rx1day)
        is.Rx5day <- tclVar(GeneralParameters$Indices$Rx5day)
        is.SDII <- tclVar(GeneralParameters$Indices$SDII)
        is.R10mm <- tclVar(GeneralParameters$Indices$R10mm)
        is.R20mm <- tclVar(GeneralParameters$Indices$R20mm)
        is.Rnnmm <- tclVar(GeneralParameters$Indices$Rnnmm)
        val.Rnnmm <- tclVar(GeneralParameters$Indices$thres.Rnnmm)

        is.CDD <- tclVar(GeneralParameters$Indices$CDD)
        is.CWD <- tclVar(GeneralParameters$Indices$CWD)
        is.R95pTOT <- tclVar(GeneralParameters$Indices$R95pTOT)
        is.R99pTOT <- tclVar(GeneralParameters$Indices$R99pTOT)
        is.PRCPTOT <- tclVar(GeneralParameters$Indices$PRCPTOT)

        chk.Rx1day <- tkcheckbutton(frameIndex, variable = is.Rx1day, text = 'Rx1day: Monthly maximum 1-day precipitation', anchor = 'w', justify = 'left')
        chk.Rx5day <- tkcheckbutton(frameIndex, variable = is.Rx5day, text = 'Rx5day: Monthly maximum consecutive 5-day precipitation', anchor = 'w', justify = 'left')
        chk.SDII <- tkcheckbutton(frameIndex, variable = is.SDII, text = 'SDII: Simple precipitation intensity index', anchor = 'w', justify = 'left')
        chk.R10mm <- tkcheckbutton(frameIndex, variable = is.R10mm, text = 'R10mm: Annual count of days when PRCP >= 10mm', anchor = 'w', justify = 'left')
        chk.R20mm <- tkcheckbutton(frameIndex, variable = is.R20mm, text = 'R20mm: Annual count of days when PRCP >= 20mm', anchor = 'w', justify = 'left')
        chk.Rnnmm <- tkcheckbutton(frameIndex, variable = is.Rnnmm, text = 'Rnnmm: Annual count of days when PRCP >= nnmm', anchor = 'w', justify = 'left')
        chk.CDD <- tkcheckbutton(frameIndex, variable = is.CDD, text = 'CDD: Maximum length of dry spell', anchor = 'w', justify = 'left')
        chk.CWD <- tkcheckbutton(frameIndex, variable = is.CWD, text = 'CWD: Maximum length of wet spell', anchor = 'w', justify = 'left')
        chk.R95pTOT <- tkcheckbutton(frameIndex, variable = is.R95pTOT, text = 'R95pTOT: Annual total PRCP when RR > 95th percentile', anchor = 'w', justify = 'left')
        chk.R99pTOT <- tkcheckbutton(frameIndex, variable = is.R99pTOT, text = 'R99pTOT: Annual total PRCP when RR > 99th percentile', anchor = 'w', justify = 'left')
        chk.PRCPTOT <- tkcheckbutton(frameIndex, variable = is.PRCPTOT, text = 'PRCPTOT: Annual total precipitation in wet days', anchor = 'w', justify = 'left')

        ################

        frameRnnmm <- tkframe(frameIndex)

        txt.Rnnmm <- tklabel(frameRnnmm, text = 'User defined threshold (mm)', anchor = 'w', justify = 'left')
        en.Rnnmm <- tkentry(frameRnnmm, width = 4, textvariable = val.Rnnmm, justify = "left")
        tkgrid(txt.Rnnmm, en.Rnnmm)

        ################

        tkgrid(chk.Rx1day, row = 0, column = 0, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
        tkgrid(chk.Rx5day, row = 1, column = 0, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
        tkgrid(chk.SDII, row = 2, column = 0, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
        tkgrid(chk.R10mm, row = 3, column = 0, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
        tkgrid(chk.R20mm, row = 4, column = 0, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
        tkgrid(chk.Rnnmm, row = 5, column = 0, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
        tkgrid(frameRnnmm, row = 6, column = 0, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
        tkgrid(chk.CDD, row = 7, column = 0, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
        tkgrid(chk.CWD, row = 8, column = 0, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
        tkgrid(chk.R95pTOT, row = 9, column = 0, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
        tkgrid(chk.R99pTOT, row = 10, column = 0, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
        tkgrid(chk.PRCPTOT, row = 11, column = 0, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)

        helpWidget(frameIndex, "Check desired indices to calculate", "Check desired indices to calculate")

        #######################
        tkgrid(frameIndex, row = 0, column = 0, sticky = 'we', padx = 1, pady = 1, ipadx = 1, ipady = 1)
        tcl('update')

        ##############################################

        start.july <- tclVar(GeneralParameters$start.july)

        chk.StartJul <- tkcheckbutton(climdex.frame1, variable = start.july, text = 'Southern Hemisphere: year starting on July', anchor = 'w', justify = 'left')

        tkgrid(chk.StartJul, row = 0, column = 0, sticky = 'we', padx = 1, pady = 1, ipadx = 1, ipady = 1)

        ##############################################

        tkconfigure(bt.CalcIndices, command = function(){
            GeneralParameters$data.type <- datatypeVAL[CbdatatypeVAL %in% str_trim(tclvalue(DataType))]

            if(str_trim(tclvalue(DataType)) == CbdatatypeVAL[1])
                GeneralParameters$cdtstation <- str_trim(tclvalue(input.Prec))

            if(str_trim(tclvalue(DataType)) == CbdatatypeVAL[2])
                GeneralParameters$cdtdataset <- str_trim(tclvalue(input.Prec))

            GeneralParameters$output <- str_trim(tclvalue(dir.save))

            GeneralParameters$Indices$Rx1day <- switch(tclvalue(is.Rx1day), '0' = FALSE, '1' = TRUE)
            GeneralParameters$Indices$Rx5day <- switch(tclvalue(is.Rx5day), '0' = FALSE, '1' = TRUE)
            GeneralParameters$Indices$SDII <- switch(tclvalue(is.SDII), '0' = FALSE, '1' = TRUE)
            GeneralParameters$Indices$R10mm <- switch(tclvalue(is.R10mm), '0' = FALSE, '1' = TRUE)
            GeneralParameters$Indices$R20mm <- switch(tclvalue(is.R20mm), '0' = FALSE, '1' = TRUE)
            GeneralParameters$Indices$Rnnmm <- switch(tclvalue(is.Rnnmm), '0' = FALSE, '1' = TRUE)
            GeneralParameters$Indices$thres.Rnnmm <- as.numeric(str_trim(tclvalue(val.Rnnmm)))
            GeneralParameters$Indices$CDD <- switch(tclvalue(is.CDD), '0' = FALSE, '1' = TRUE)
            GeneralParameters$Indices$CWD <- switch(tclvalue(is.CWD), '0' = FALSE, '1' = TRUE)
            GeneralParameters$Indices$R95pTOT <- switch(tclvalue(is.R95pTOT), '0' = FALSE, '1' = TRUE)
            GeneralParameters$Indices$R99pTOT <- switch(tclvalue(is.R99pTOT), '0' = FALSE, '1' = TRUE)
            GeneralParameters$Indices$PRCPTOT <- switch(tclvalue(is.PRCPTOT), '0' = FALSE, '1' = TRUE)

            GeneralParameters$start.july <- switch(tclvalue(start.july), '0' = FALSE, '1' = TRUE)

            # assign("GeneralParameters", GeneralParameters, envir = .GlobalEnv)

            Insert.Messages.Out("Calculating Indices (this may take some time) .......", TRUE, "i")

            tkconfigure(.cdtEnv$tcl$main$win, cursor = 'watch')
            tcl('update')
            ret <- tryCatch(
                {
                    climdexCalc.RR(GeneralParameters)
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
                    Insert.Messages.Out("Indices calculation finished successfully", TRUE, "s")
                    ## set
                    .cdtData$EnvData$plot.maps$data.type <- .cdtData$EnvData$output$params$data.type
                    .cdtData$EnvData$plot.maps[c('lon', 'lat', 'id')] <- .cdtData$EnvData$output$data[c('lon', 'lat', 'id')]
                    .cdtData$EnvData$indices.data <- "RR"
                    ###################
                    ret <- try(set.vars.dates(), silent = TRUE)
                    if(inherits(ret, "try-error") | is.null(ret)) return(NULL)

                    widgets.Station.Pixel()
                    set.plot.type()
                    set.trend.vars()

                    ret1 <- try(get.data.Trend(), silent = TRUE)
                    if(inherits(ret1, "try-error") | is.null(ret1)) return(NULL)
                    ret2 <- try(get.data.Year(), silent = TRUE)
                    if(inherits(ret2, "try-error") | is.null(ret2)) return(NULL)
                }else Insert.Messages.Out("Indices calculation failed", format = TRUE)
            }else Insert.Messages.Out("Indices calculation failed", format = TRUE)
        })

    #######################################################################################################

    #Tab3
    subfr3 <- bwTabScrollableFrame(cmd.tab3)

        #######################

        frameDataExist <- ttklabelframe(subfr3, text = "CLIMDEX data", relief = 'groove')

        .cdtData$EnvData$DirExist <- tclVar(0)
        file.dataIndex <- tclVar()

        stateExistData <- if(tclvalue(.cdtData$EnvData$DirExist) == "1") "normal" else "disabled"

        chk.dataIdx <- tkcheckbutton(frameDataExist, variable = .cdtData$EnvData$DirExist, text = "CLIMDEX data already computed", anchor = 'w', justify = 'left')
        en.dataIdx <- tkentry(frameDataExist, textvariable = file.dataIndex, width = largeur2, state = stateExistData)
        bt.dataIdx <- tkbutton(frameDataExist, text = "...", state = stateExistData)

        tkconfigure(bt.dataIdx, command = function(){
            path.dataIdx <- tclvalue(tkgetOpenFile(initialdir = getwd(), filetypes = .cdtEnv$tcl$data$filetypes6))
            if(path.dataIdx %in% c("", "NA") | is.na(path.dataIdx)) return(NULL)
            tclvalue(file.dataIndex) <- path.dataIdx

            if(file.exists(str_trim(tclvalue(file.dataIndex)))){
                OutIndexdata <- try(readRDS(str_trim(tclvalue(file.dataIndex))), silent = TRUE)
                if(inherits(OutIndexdata, "try-error")){
                    Insert.Messages.Out('Unable to load CLIMDEX data', format = TRUE)
                    Insert.Messages.Out(gsub('[\r\n]', '', OutIndexdata[1]), format = TRUE)

                    tkconfigure(cb.varstat.var, values = "")
                    tclvalue(.cdtData$EnvData$anaVars) <- ""
                    tkconfigure(cb.varstat.stat, values = "")
                    tclvalue(.cdtData$EnvData$anaStat) <- ""
                    tkconfigure(cb.data.Index, values = "")
                    tclvalue(.cdtData$EnvData$donDate) <- ""
                    return(NULL)
                }

                .cdtData$EnvData$output <- OutIndexdata
                .cdtData$EnvData$PathData <- dirname(str_trim(tclvalue(file.dataIndex)))
                .cdtData$EnvData$plot.maps$data.type <- .cdtData$EnvData$output$params$data.type
                .cdtData$EnvData$plot.maps[c('lon', 'lat', 'id')] <- .cdtData$EnvData$output$data[c('lon', 'lat', 'id')]
                .cdtData$EnvData$indices.data <- "RR"
                ###################
                ret <- try(set.vars.dates(), silent = TRUE)
                if(inherits(ret, "try-error") | is.null(ret)) return(NULL)

                widgets.Station.Pixel()
                set.plot.type()
                set.trend.vars()

                ret1 <- try(get.data.Trend(), silent = TRUE)
                if(inherits(ret1, "try-error") | is.null(ret1)) return(NULL)
                ret2 <- try(get.data.Year(), silent = TRUE)
                if(inherits(ret2, "try-error") | is.null(ret2)) return(NULL)
            }
        })

        tkgrid(chk.dataIdx, row = 0, column = 0, sticky = 'we', rowspan = 1, columnspan = 5, padx = 1, pady = 1, ipadx = 1, ipady = 1)
        tkgrid(en.dataIdx, row = 1, column = 0, sticky = 'we', rowspan = 1, columnspan = 4, padx = 1, pady = 1, ipadx = 1, ipady = 1)
        tkgrid(bt.dataIdx, row = 1, column = 4, sticky = 'w', rowspan = 1, columnspan = 1, padx = 0, pady = 1, ipadx = 1, ipady = 1)

        ###############
        tkbind(chk.dataIdx, "<Button-1>", function(){
            stateExistData <- if(tclvalue(.cdtData$EnvData$DirExist) == '1') 'disabled' else 'normal'
            tkconfigure(en.dataIdx, state = stateExistData)
            tkconfigure(bt.dataIdx, state = stateExistData)
            stateCaclBut <- if(tclvalue(.cdtData$EnvData$DirExist) == '1') 'normal' else 'disabled'
            tkconfigure(bt.CalcIndices, state = stateCaclBut)
        })

        ##############################################

        frameDataStatMap <- ttklabelframe(subfr3, text = "Statistics Maps", relief = 'groove')

        .cdtData$EnvData$anaVars <- tclVar()
        .cdtData$EnvData$anaStat <- tclVar()

        cb.varstat.var <- ttkcombobox(frameDataStatMap, values = "", textvariable = .cdtData$EnvData$anaVars, width = largeur3)
        bt.varstat.maps <- ttkbutton(frameDataStatMap, text = .cdtEnv$tcl$lang$global[['button']][['3']])
        cb.varstat.stat <- ttkcombobox(frameDataStatMap, values = "", textvariable = .cdtData$EnvData$anaStat, width = largeur3)
        bt.varstat.MapOpt <- ttkbutton(frameDataStatMap, text = .cdtEnv$tcl$lang$global[['button']][['4']])

        ###################

        .cdtData$EnvData$tab$pointSize.MapStat <- NULL
        .cdtData$EnvData$varstatMapOp <- list(presetCol = list(color = 'tim.colors', reverse = FALSE),
                                            userCol = list(custom = FALSE, color = NULL),
                                            userLvl = list(custom = FALSE, levels = NULL, equidist = FALSE),
                                            title = list(user = FALSE, title = ''),
                                            colkeyLab = list(user = FALSE, label = ''),
                                            scalebar = list(add = FALSE, pos = 'bottomleft'),
                                            pointSize = .cdtData$EnvData$tab$pointSize.MapStat)

        tkconfigure(bt.varstat.MapOpt, command = function(){
            if(!is.null(.cdtData$EnvData$varData$map)){
                atlevel <- pretty(.cdtData$EnvData$varData$map$z, n = 10, min.n = 7)
                if(is.null(.cdtData$EnvData$varstatMapOp$userLvl$levels)){
                    .cdtData$EnvData$varstatMapOp$userLvl$levels <- atlevel
                }else{
                    if(!.cdtData$EnvData$varstatMapOp$userLvl$custom)
                        .cdtData$EnvData$varstatMapOp$userLvl$levels <- atlevel
                }
            }
            .cdtData$EnvData$varstatMapOp <- MapGraph.MapOptions(.cdtData$EnvData$varstatMapOp)

            if(str_trim(tclvalue(.cdtData$EnvData$plot.maps$plot.type)) == "Points")
                .cdtData$EnvData$tab$pointSize.MapStat <- .cdtData$EnvData$varstatMapOp$pointSize
        })

        ###################

        .cdtData$EnvData$tab$dataMapStat <- NULL

        tkconfigure(bt.varstat.maps, command = function(){
            if(str_trim(tclvalue(.cdtData$EnvData$anaVars)) != "" &
                str_trim(tclvalue(.cdtData$EnvData$anaStat)) != "")
            {
                ret <- try(get.data.Trend(), silent = TRUE)
                if(inherits(ret, "try-error") | is.null(ret)) return(NULL)
                Climdex.Display.MapsTrend()
            }
        })

        ###################

        tkgrid(cb.varstat.var, row = 0, column = 0, sticky = 'we', rowspan = 1, columnspan = 4, padx = 1, pady = 1, ipadx = 1, ipady = 1)
        tkgrid(bt.varstat.maps, row = 0, column = 4, sticky = '', rowspan = 1, columnspan = 2, padx = 1, pady = 1, ipadx = 1, ipady = 1)
        tkgrid(cb.varstat.stat, row = 1, column = 0, sticky = 'we', rowspan = 1, columnspan = 2, padx = 1, pady = 1, ipadx = 1, ipady = 1)
        tkgrid(bt.varstat.MapOpt, row = 1, column = 4, sticky = 'we', rowspan = 1, columnspan = 2, padx = 1, pady = 1, ipadx = 1, ipady = 1)

        ###################

        tkbind(cb.varstat.var, "<<ComboboxSelected>>", function(){
            if(str_trim(tclvalue(.cdtData$EnvData$anaVars)) != "" &
                str_trim(tclvalue(.cdtData$EnvData$anaStat)) != "")
            {
                ret1 <- try(get.data.Trend(), silent = TRUE)
                if(inherits(ret1, "try-error") | is.null(ret1)) return(NULL)
            }

            ########
            if(!is.null(.cdtData$EnvData$YearData)){
                ret2 <- try(get.data.Year(), silent = TRUE)
                if(inherits(ret2, "try-error") | is.null(ret2)) return(NULL)
            }
        })

        ##############################################

        frameDataMap <- ttklabelframe(subfr3, text = "Yearly Maps", relief = 'groove')

        .cdtData$EnvData$donDate <- tclVar()

        cb.data.Index <- ttkcombobox(frameDataMap, values = "", textvariable = .cdtData$EnvData$donDate, width = largeur4, justify = 'center')
        bt.data.Index.prev <- ttkbutton(frameDataMap, text = "<<", width = 3)
        bt.data.Index.next <- ttkbutton(frameDataMap, text = ">>", width = 3)
        bt.data.maps <- ttkbutton(frameDataMap, text = .cdtEnv$tcl$lang$global[['button']][['3']], width = 7)
        bt.data.MapOpt <- ttkbutton(frameDataMap, text = .cdtEnv$tcl$lang$global[['button']][['4']], width = 7)

        ###############

        .cdtData$EnvData$tab$pointSize.MapTS <- NULL
        .cdtData$EnvData$dataMapOp <- list(presetCol = list(color = 'tim.colors', reverse = FALSE),
                                        userCol = list(custom = FALSE, color = NULL),
                                        userLvl = list(custom = FALSE, levels = NULL, equidist = FALSE),
                                        title = list(user = FALSE, title = ''),
                                        colkeyLab = list(user = FALSE, label = ''),
                                        scalebar = list(add = FALSE, pos = 'bottomleft'),
                                        pointSize = .cdtData$EnvData$tab$pointSize.MapTS)

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

            if(str_trim(tclvalue(.cdtData$EnvData$plot.maps$plot.type)) == "Points")
                .cdtData$EnvData$tab$pointSize.MapTS <- .cdtData$EnvData$dataMapOp$pointSize
        })

        ###############

        .cdtData$EnvData$tab$dataMapTS <- NULL

        tkconfigure(bt.data.maps, command = function(){
            if(str_trim(tclvalue(.cdtData$EnvData$donDate)) != "" &
                !is.null(.cdtData$EnvData$YearData))
                    Climdex.Display.MapYear()
        })

        tkconfigure(bt.data.Index.prev, command = function(){
            if(str_trim(tclvalue(.cdtData$EnvData$donDate)) != ""){
                donDates <- .cdtData$EnvData$output$year
                idaty <- which(donDates == str_trim(tclvalue(.cdtData$EnvData$donDate)))
                idaty <- idaty - 1
                if(idaty < 1) idaty <- length(donDates)
                tclvalue(.cdtData$EnvData$donDate) <- donDates[idaty]

                ret <- try(get.data.Year(), silent = TRUE)
                if(inherits(ret, "try-error") | is.null(ret)) return(NULL)

                Climdex.Display.MapYear()
            }
        })

        tkconfigure(bt.data.Index.next, command = function(){
            if(str_trim(tclvalue(.cdtData$EnvData$donDate)) != ""){
                donDates <- .cdtData$EnvData$output$year
                idaty <- which(donDates == str_trim(tclvalue(.cdtData$EnvData$donDate)))
                idaty <- idaty + 1
                if(idaty > length(donDates)) idaty <- 1
                tclvalue(.cdtData$EnvData$donDate) <- donDates[idaty]

                ret <- try(get.data.Year(), silent = TRUE)
                if(inherits(ret, "try-error") | is.null(ret)) return(NULL)

                Climdex.Display.MapYear()
            }
        })

        ###############

        tkgrid(bt.data.Index.prev, row = 0, column = 0, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
        tkgrid(cb.data.Index, row = 0, column = 1, sticky = 'we', rowspan = 1, columnspan = 2, padx = 1, pady = 1, ipadx = 1, ipady = 1)
        tkgrid(bt.data.Index.next, row = 0, column = 3, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
        tkgrid(bt.data.maps, row = 0, column = 4, sticky = 'we', rowspan = 1, columnspan = 2, padx = 1, pady = 1, ipadx = 1, ipady = 1)
        tkgrid(bt.data.MapOpt, row = 1, column = 4, sticky = 'we', rowspan = 1, columnspan = 2, padx = 1, pady = 1, ipadx = 1, ipady = 1)

        ###############

        tkbind(cb.data.Index, "<<ComboboxSelected>>", function(){
            if(!is.null(.cdtData$EnvData$YearData)){
                ret <- try(get.data.Year(), silent = TRUE)
                if(inherits(ret, "try-error") | is.null(ret)) return(NULL)
            }
        })

        ##############################################

        framePlotType <- tkframe(subfr3)

        .cdtData$EnvData$plot.maps$plot.type <- tclVar("Pixels")

        txt.plotType <- tklabel(framePlotType, text = "Plot Type", anchor = 'e', justify = 'right')
        cb.plotType <- ttkcombobox(framePlotType, values = "Pixels", textvariable = .cdtData$EnvData$plot.maps$plot.type, width = largeur4)

        tkgrid(txt.plotType, row = 0, column = 0, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
        tkgrid(cb.plotType, row = 0, column = 1, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)

        ###############

        tkbind(cb.plotType, "<<ComboboxSelected>>", function(){
            if(str_trim(tclvalue(.cdtData$EnvData$anaVars)) != "" &
                str_trim(tclvalue(.cdtData$EnvData$anaStat)) != "")
            {
                ret1 <- try(get.data.Trend(), silent = TRUE)
                if(inherits(ret1, "try-error") | is.null(ret1)) return(NULL)
            }

            ########
            if(!is.null(.cdtData$EnvData$YearData)){
                ret2 <- try(get.data.Year(), silent = TRUE)
                if(inherits(ret2, "try-error") | is.null(ret2)) return(NULL)
            }
        })

        ##############################################

        tkgrid(frameDataExist, row = 0, column = 0, sticky = 'we', padx = 1, pady = 1, ipadx = 1, ipady = 1)
        tkgrid(frameDataStatMap, row = 1, column = 0, sticky = 'we', padx = 1, pady = 3, ipadx = 1, ipady = 1)
        tkgrid(frameDataMap, row = 2, column = 0, sticky = 'we', padx = 1, pady = 1, ipadx = 1, ipady = 1)
        tkgrid(framePlotType, row = 3, column = 0, sticky = '', padx = 1, pady = 3, ipadx = 1, ipady = 1)

    #######################################################################################################

    #Tab4
    subfr4 <- bwTabScrollableFrame(cmd.tab4)

        #######################

        frameDataTS <- ttklabelframe(subfr4, text = "CLIMDEX Graph", relief = 'groove')

        typeTSPLOT <- c("Line", "Barplot")
        .cdtData$EnvData$plot.maps$typeTSp <- tclVar("Line")

        cb.typeTSp <- ttkcombobox(frameDataTS, values = typeTSPLOT, textvariable = .cdtData$EnvData$plot.maps$typeTSp, width = largeur4)
        bt.TsGraph.plot <- ttkbutton(frameDataTS, text = .cdtEnv$tcl$lang$global[['button']][['3']], width = 7)
        bt.TSGraphOpt <- ttkbutton(frameDataTS, text = .cdtEnv$tcl$lang$global[['button']][['4']], width = 8)

        #################

        .cdtData$EnvData$TSGraphOp <- list(
                            bar = list(
                                xlim = list(is.min = FALSE, min = 1981, is.max = FALSE, max = 2017),
                                ylim = list(is.min = FALSE, min = 0, is.max = FALSE, max = 100),
                                axislabs = list(is.xlab = FALSE, xlab = '', is.ylab = FALSE, ylab = ''),
                                title = list(is.title = FALSE, title = '', position = 'top'),
                                colors = list(col = "darkblue")
                            ),
                            line = list(
                                xlim = list(is.min = FALSE, min = 1981, is.max = FALSE, max = 2017),
                                ylim = list(is.min = FALSE, min = 0, is.max = FALSE, max = 100),
                                axislabs = list(is.xlab = FALSE, xlab = '', is.ylab = FALSE, ylab = ''),
                                title = list(is.title = FALSE, title = '', position = 'top'),
                                plot = list(type = 'both',
                                    col = list(line = "red", points = "blue"),
                                    lwd = 2, cex = 1.4),
                                legend = list(
                                    col = list(lowess = "blue", linear = "black"),
                                    text = list(lowess = "Lowess smoother", linear = "Linear Trend"),
                                    lwd = list(lowess = 2, linear = 2),
                                    lty= list(lowess = 2, linear = 1))
                            )
                        )

        tkconfigure(bt.TSGraphOpt, command = function(){
            suffix.fun <- switch(str_trim(tclvalue(.cdtData$EnvData$plot.maps$typeTSp)),
                                    "Barplot" = "Bar",
                                    "Line" = "LineCLIMDEX")
            plot.fun <- get(paste0("MapGraph.GraphOptions.", suffix.fun), mode = "function")
            .cdtData$EnvData$TSGraphOp <- plot.fun(.cdtData$EnvData$TSGraphOp)
        })

        #################

        .cdtData$EnvData$tab$dataGraph <- NULL

        tkconfigure(bt.TsGraph.plot, command = function(){
            if(!is.null(.cdtData$EnvData$YearData)){
                imgContainer <- CDT.Display.Graph(Climdex.plotVarGraph, .cdtData$EnvData$tab$dataGraph, 'Climdex-Graph')
                .cdtData$EnvData$tab$dataGraph <- imageNotebookTab_unik(imgContainer, .cdtData$EnvData$tab$dataGraph)
            }
        })

        #################

        tkgrid(cb.typeTSp, row = 0, column = 0, sticky = 'we', pady = 1, columnspan = 1)
        tkgrid(bt.TSGraphOpt, row = 0, column = 1, sticky = 'we', padx = 4, pady = 1, columnspan = 1)
        tkgrid(bt.TsGraph.plot, row = 0, column = 2, sticky = 'we', pady = 1, columnspan = 1)

        ##############################################

        frameSTNCrds <- ttklabelframe(subfr4, text = "Station/Coordinates", relief = 'groove')

        frTS2 <- tkframe(frameSTNCrds)
        .cdtData$EnvData$plot.maps$lonLOC <- tclVar()
        .cdtData$EnvData$plot.maps$latLOC <- tclVar()
        .cdtData$EnvData$plot.maps$stnIDTSp <- tclVar()

        tkgrid(frTS2, row = 0, column = 0, sticky = 'e', pady = 1)

        ##############################################

        tkgrid(frameDataTS, row = 0, column = 0, sticky = 'we', pady = 1)
        tkgrid(frameSTNCrds, row = 1, column = 0, sticky = '', pady = 3)


    #######################################################################################################

    #Tab5
    subfr5 <- bwTabScrollableFrame(cmd.tab5)

        #######################

        frameSHP <- ttklabelframe(subfr5, text = "Boundaries", relief = 'groove')

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
                if(!is.null(.cdtData$EnvData$YearData)){
                    istn <- which(stnIDTSPLOT == str_trim(tclvalue(.cdtData$EnvData$plot.maps$stnIDTSp)))
                    istn <- istn - 1
                    if(istn < 1) istn <- length(stnIDTSPLOT)
                    tclvalue(.cdtData$EnvData$plot.maps$stnIDTSp) <- stnIDTSPLOT[istn]

                    imgContainer <- CDT.Display.Graph(Climdex.plotVarGraph, .cdtData$EnvData$tab$dataGraph, 'Climdex-Graph')
                    .cdtData$EnvData$tab$dataGraph <- imageNotebookTab_unik(imgContainer, .cdtData$EnvData$tab$dataGraph)
                }
            })

            tkconfigure(bt.stnID.next, command = function(){
                if(!is.null(.cdtData$EnvData$YearData)){
                    istn <- which(stnIDTSPLOT == str_trim(tclvalue(.cdtData$EnvData$plot.maps$stnIDTSp)))
                    istn <- istn + 1
                    if(istn > length(stnIDTSPLOT)) istn <- 1
                    tclvalue(.cdtData$EnvData$plot.maps$stnIDTSp) <- stnIDTSPLOT[istn]

                    imgContainer <- CDT.Display.Graph(Climdex.plotVarGraph, .cdtData$EnvData$tab$dataGraph, 'Climdex-Graph')
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

    ###################

    set.plot.type <- function(){
        if(.cdtData$EnvData$output$params$data.type == "cdtstation")
        {
            plot.type <- c("Pixels", "Points")
            .cdtData$EnvData$plot.maps$.data.type <- "Points"

            .cdtData$EnvData$varstatMapOp$pointSize <- 0.7
            .cdtData$EnvData$dataMapOp$pointSize <- 0.7
        }else{
            plot.type <- c("Pixels", "FilledContour")
            .cdtData$EnvData$plot.maps$.data.type <- "Grid"
        }
        tkconfigure(cb.plotType, values = plot.type)
    }

    ###################

    set.trend.vars <- function(){
        .cdtData$EnvData$trend$name <- c("slope", "std.slope", "t.value.slope", "p.value.slope",
                                        "intercept", "std.intercept", "t.value.intercept",
                                        "p.value.intercept", "R2", "sigma")
        .cdtData$EnvData$trend$longname <- c(
                                            "Slope - Estimate", "Slope - Standard Error", "Slope t-value", "Slope p-value Pr(>t)",
                                            "Intercept - Estimate", "Intercept - Standard Error", "Intercept t-value", "Intercept p-value Pr(>t)",
                                            "Multiple R-squared", "Residual Standard Error"
                                            )
        tkconfigure(cb.varstat.stat, values = .cdtData$EnvData$trend$longname)
        tclvalue(.cdtData$EnvData$anaStat) <- .cdtData$EnvData$trend$longname[1]
    }

    ###################

    set.vars.dates <- function(){
        indxlst <- c("Rx1day", "Rx5day", "SDII", "R10mm", "R20mm", "Rnnmm",
                        "CDD", "CWD", "R95pTOT", "R99pTOT", "PRCPTOT")
        var.dir.path <- file.path(.cdtData$EnvData$PathData, 'CDTDATASET')
        indxexist <- list.files(var.dir.path)
        indices <- indxexist[indxexist %in% indxlst]
        if(length(indices) == 0){
            Insert.Messages.Out('No Indices data found', format = TRUE)
            return(NULL)
        }

        tkconfigure(cb.varstat.var, values = indices)
        tclvalue(.cdtData$EnvData$anaVars) <- indices[1]

        YEARS <- .cdtData$EnvData$output$year
        tkconfigure(cb.data.Index, values = YEARS)
        tclvalue(.cdtData$EnvData$donDate) <- YEARS[length(YEARS)]
        return(0)
    }

    #######################################################################################################

    get.data.Trend <- function(){
        tkconfigure(.cdtEnv$tcl$main$win, cursor = 'watch')
        tcl('update')
        on.exit({
            tkconfigure(.cdtEnv$tcl$main$win, cursor = '')
            tcl('update')
        })

        this.vars <- str_trim(tclvalue(.cdtData$EnvData$anaVars))
        this.trend <- str_trim(tclvalue(.cdtData$EnvData$anaStat))
        ipos <- which(.cdtData$EnvData$trend$longname %in% this.trend)

        readTrendData <- TRUE
        if(!is.null(.cdtData$EnvData$TrendData))
            if(!is.null(.cdtData$EnvData$TrendData$this.vars))
                if(.cdtData$EnvData$TrendData$this.vars == this.vars) readTrendData <- FALSE

        if(.cdtData$EnvData$output$params$data.type == "cdtstation"){
            if(readTrendData){
                filetrend <- file.path(.cdtData$EnvData$PathData, "CDTDATASET", this.vars, "Trend", paste0(this.vars, '.rds'))
                if(!file.exists(filetrend)){
                    Insert.Messages.Out(paste(filetrend, "not found"), format = TRUE)
                    return(NULL)
                }

                .cdtData$EnvData$TrendData$data <- readRDS(filetrend)
                .cdtData$EnvData$TrendData$this.vars <- this.vars
            }

            #####
            change.plot <- str_trim(tclvalue(.cdtData$EnvData$plot.maps$plot.type))

            getTrend <- TRUE
            if(!is.null(.cdtData$EnvData$TrendData$this.trend))
                if(.cdtData$EnvData$TrendData$this.trend == this.trend) getTrend <- FALSE

            if(!getTrend)
                if(.cdtData$EnvData$change.plot.trend != change.plot) getTrend <- TRUE

            if(getTrend){
                x <- .cdtData$EnvData$output$data$lon
                y <- .cdtData$EnvData$output$data$lat
                tmp <- as.numeric(.cdtData$EnvData$TrendData$data[ipos, ])
                if(change.plot == "Pixels"){
                    nx <- nx_ny_as.image(diff(range(x)))
                    ny <- nx_ny_as.image(diff(range(y)))
                    tmp <- cdt.as.image(tmp, nx = nx, ny = ny, pts.xy = cbind(x, y))
                    .cdtData$EnvData$TrendData$map$x <- tmp$x
                    .cdtData$EnvData$TrendData$map$y <- tmp$y
                    .cdtData$EnvData$TrendData$map$z <- tmp$z
                }

                if(change.plot == "Points"){
                    .cdtData$EnvData$TrendData$map$x <- x
                    .cdtData$EnvData$TrendData$map$y <- y
                    .cdtData$EnvData$TrendData$map$z <- tmp
                }

                .cdtData$EnvData$TrendData$this.trend <- this.trend
                .cdtData$EnvData$change.plot.trend <- change.plot
                rm(tmp, x, y)
            }
        }else{
            if(readTrendData){
                filetrend <- file.path(.cdtData$EnvData$PathData, "DATA_NetCDF", this.vars, "Trend", paste0(this.vars, '.nc'))
                if(!file.exists(filetrend)){
                    Insert.Messages.Out(paste(filetrend, "not found"), format = TRUE)
                    return(NULL)
                }

                nc <- nc_open(filetrend)
                .cdtData$EnvData$TrendData$data <- lapply(.cdtData$EnvData$trend$name, function(varid) ncvar_get(nc, varid = varid))
                nc_close(nc)
                .cdtData$EnvData$TrendData$this.vars <- this.vars
            }

            .cdtData$EnvData$TrendData$map$x <- .cdtData$EnvData$output$data$x
            .cdtData$EnvData$TrendData$map$y <- .cdtData$EnvData$output$data$y
            .cdtData$EnvData$TrendData$map$z <- .cdtData$EnvData$TrendData$data[[ipos]]

            ###################
            file.Index.Trend <- file.path(.cdtData$EnvData$PathData, "CDTDATASET", this.vars, "Trend", paste0(this.vars, '.rds'))

            read.cdt.dataTrend <- TRUE
            if(!is.null(.cdtData$EnvData$cdtdataTrend))
                if(!is.null(.cdtData$EnvData$file.Index.Trend))
                    if(.cdtData$EnvData$file.Index.Trend == file.Index.Trend) read.cdt.dataTrend <- FALSE
            if(read.cdt.dataTrend){
                .cdtData$EnvData$cdtdataTrend <- readRDS(file.Index.Trend)
                .cdtData$EnvData$cdtdataTrend$fileInfo <- file.Index.Trend
                .cdtData$EnvData$file.Index.Trend <- file.Index.Trend
            }
        }

        return(0)
    }

    ###################

    get.data.Year <- function(){
        tkconfigure(.cdtEnv$tcl$main$win, cursor = 'watch')
        tcl('update')
        on.exit({
            tkconfigure(.cdtEnv$tcl$main$win, cursor = '')
            tcl('update')
        })

        this.vars <- str_trim(tclvalue(.cdtData$EnvData$anaVars))
        this.year <- str_trim(tclvalue(.cdtData$EnvData$donDate))

        if(.cdtData$EnvData$output$params$data.type == "cdtstation"){
            fileyear <- file.path(.cdtData$EnvData$PathData, "CDTDATASET", this.vars,
                                    "Yearly", paste0(this.vars, '.rds'))

            readYearData <- TRUE
            if(!is.null(.cdtData$EnvData$YearData))
                if(!is.null(.cdtData$EnvData$YearData$fileyear))
                    if(.cdtData$EnvData$YearData$fileyear == fileyear) readYearData <- FALSE

            if(readYearData){
                if(!file.exists(fileyear)){
                    Insert.Messages.Out(paste(fileyear, "not found"), format = TRUE)
                    return(NULL)
                }

                .cdtData$EnvData$YearData$data <- readRDS(fileyear)
                .cdtData$EnvData$YearData$fileyear <- fileyear
            }

            #####
            change.plot <- str_trim(tclvalue(.cdtData$EnvData$plot.maps$plot.type))

            getYEAR <- TRUE
            if(!is.null(.cdtData$EnvData$YearData$this.year))
                if(.cdtData$EnvData$YearData$this.year == this.year) getYEAR <- FALSE

            if(!getYEAR)
                if(.cdtData$EnvData$change.plot.year != change.plot) getYEAR <- TRUE

            if(getYEAR){
                ipos <- which(.cdtData$EnvData$output$year %in% this.year)
                x <- .cdtData$EnvData$output$data$lon
                y <- .cdtData$EnvData$output$data$lat
                tmp <- as.numeric(.cdtData$EnvData$YearData$data[ipos, ])
                if(change.plot == "Pixels"){
                    nx <- nx_ny_as.image(diff(range(x)))
                    ny <- nx_ny_as.image(diff(range(y)))
                    tmp <- cdt.as.image(tmp, nx = nx, ny = ny, pts.xy = cbind(x, y))
                    .cdtData$EnvData$YearData$map$x <- tmp$x
                    .cdtData$EnvData$YearData$map$y <- tmp$y
                    .cdtData$EnvData$YearData$map$z <- tmp$z
                }

                if(change.plot == "Points"){
                    .cdtData$EnvData$YearData$map$x <- x
                    .cdtData$EnvData$YearData$map$y <- y
                    .cdtData$EnvData$YearData$map$z <- tmp
                }

                .cdtData$EnvData$YearData$this.year <- this.year
                .cdtData$EnvData$change.plot.year <- change.plot
                rm(tmp, x, y)
            }
        }else{
            fileyear <- file.path(.cdtData$EnvData$PathData, "DATA_NetCDF", this.vars,
                                    "Yearly", paste0(this.vars, "_", this.year, '.nc'))

            readYearData <- TRUE
            if(!is.null(.cdtData$EnvData$YearData))
                if(!is.null(.cdtData$EnvData$YearData$fileyear))
                    if(.cdtData$EnvData$YearData$fileyear == fileyear) readYearData <- FALSE

            if(readYearData){
                if(!file.exists(fileyear)){
                    Insert.Messages.Out(paste(fileyear, "not found"), format = TRUE)
                    return(NULL)
                }

                nc <- nc_open(fileyear)
                .cdtData$EnvData$YearData$map$x <- nc$dim[[1]]$vals
                .cdtData$EnvData$YearData$map$y <- nc$dim[[2]]$vals
                .cdtData$EnvData$YearData$map$z <- ncvar_get(nc, varid = nc$var[[1]]$name)
                nc_close(nc)

                .cdtData$EnvData$YearData$fileyear <- fileyear
            }

            ###################
            file.Index.Year <- file.path(.cdtData$EnvData$PathData, "CDTDATASET", this.vars, 'Yearly', paste0(this.vars, '.rds'))

            read.cdt.dataYear <- TRUE
            if(!is.null(.cdtData$EnvData$cdtdataYear))
                if(!is.null(.cdtData$EnvData$file.Index.Year))
                    if(.cdtData$EnvData$file.Index.Year == file.Index.Year) read.cdt.dataYear <- FALSE
            if(read.cdt.dataYear){
                .cdtData$EnvData$cdtdataYear <- readRDS(file.Index.Year)
                .cdtData$EnvData$cdtdataYear$fileInfo <- file.Index.Year
                .cdtData$EnvData$file.Index.Year <- file.Index.Year
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
