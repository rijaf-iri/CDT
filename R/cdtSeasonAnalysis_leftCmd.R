
SeasonAnalysisPanelCmd <- function(){
    listOpenFiles <- openFile_ttkcomboList()
    if(WindowsOS()){
        largeur0 <- .cdtEnv$tcl$fun$w.widgets(33)
        largeur1 <- .cdtEnv$tcl$fun$w.widgets(22)
        largeur2 <- .cdtEnv$tcl$fun$w.widgets(31)
        largeur3 <- 28
        largeur4 <- 15
        largeur5 <- 14
        largeur6 <- 22
    }else{
        largeur0 <- .cdtEnv$tcl$fun$w.widgets(23)
        largeur1 <- .cdtEnv$tcl$fun$w.widgets(20)
        largeur2 <- .cdtEnv$tcl$fun$w.widgets(22)
        largeur3 <- 21
        largeur4 <- 11
        largeur5 <- 10
        largeur6 <- 14
    }

    GeneralParameters <- list(onset = "", cessation = "", output = "",
                            seastot = list(useTotal = FALSE, Tstep = "dekadal",
                                        data.type = "cdtstation",
                                        cdtstation = list(prec = ""),
                                        cdtdataset = list(prec = "")),
                            dryday = 0.85, min.frac = 0.95)

    # xml.dlg <- file.path(.cdtDir$dirLocal, "languages", "cdtSeasonAnalysis_leftCmd.xml")
    # lang.dlg <- cdtLanguageParse(xml.dlg, .cdtData$Config$lang.iso)
    # .cdtData$EnvData$message <- lang.dlg[['message']]

    ###################

    .cdtEnv$tcl$main$cmd.frame <- tkframe(.cdtEnv$tcl$main$panel.left)

    tknote.cmd <- bwNoteBook(.cdtEnv$tcl$main$cmd.frame)

    cmd.tab1 <- bwAddTab(tknote.cmd, text = "Input")
    cmd.tab2 <- bwAddTab(tknote.cmd, text = "Output")
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

        ############################################

        frameInData <- ttklabelframe(subfr1, text = "Onset & Cessation", relief = 'groove')

        input.Onset <- tclVar(GeneralParameters$onset)
        input.Cessation <- tclVar(GeneralParameters$cessation)

        txt.Ons <- tklabel(frameInData, text = 'Path to the onset data <Onset.rds>', anchor = 'w', justify = 'left')
        en.Ons <- tkentry(frameInData, textvariable = input.Onset, width = largeur0)
        bt.Ons <- tkbutton(frameInData, text = "...")

        txt.Ces <- tklabel(frameInData, text = 'Path to the cessation data <Cessation.rds>', anchor = 'w', justify = 'left')
        en.Ces <- tkentry(frameInData, textvariable = input.Cessation, width = largeur0)
        bt.Ces <- tkbutton(frameInData, text = "...")

        tkconfigure(bt.Ons, command = function(){
            path.rds <- tclvalue(tkgetOpenFile(initialdir = getwd(), filetypes = .cdtEnv$tcl$data$filetypes6))
            tclvalue(input.Onset) <- if(path.rds %in% c("", "NA") | is.na(path.rds)) "" else path.rds
        })

        tkconfigure(bt.Ces, command = function(){
            path.rds <- tclvalue(tkgetOpenFile(initialdir = getwd(), filetypes = .cdtEnv$tcl$data$filetypes6))
            tclvalue(input.Cessation) <- if(path.rds %in% c("", "NA") | is.na(path.rds)) "" else path.rds
        })

        tkgrid(txt.Ons, row = 0, column = 0, sticky = 'we', rowspan = 1, columnspan = 10, padx = 1, pady = 1, ipadx = 1, ipady = 1)
        tkgrid(en.Ons, row = 1, column = 0, sticky = 'we', rowspan = 1, columnspan = 9, padx = 0, pady = 1, ipadx = 1, ipady = 1)
        tkgrid(bt.Ons, row = 1, column = 9, sticky = 'w', rowspan = 1, columnspan = 1, padx = 0, pady = 1, ipadx = 1, ipady = 1)
        tkgrid(txt.Ces, row = 2, column = 0, sticky = 'we', rowspan = 1, columnspan = 10, padx = 1, pady = 1, ipadx = 1, ipady = 1)
        tkgrid(en.Ces, row = 3, column = 0, sticky = 'we', rowspan = 1, columnspan = 9, padx = 0, pady = 1, ipadx = 1, ipady = 1)
        tkgrid(bt.Ces, row = 3, column = 9, sticky = 'w', rowspan = 1, columnspan = 1, padx = 0, pady = 1, ipadx = 1, ipady = 1)

        ############
        infobulle(en.Ons, 'Enter the full path to the file <Onset.rds>')
        status.bar.display(en.Ons, 'Enter the full path to the file <Onset.rds>')
        infobulle(en.Ces, 'Enter the full path to the file <Cessation.rds>')
        status.bar.display(en.Ces, 'Enter the full path to the file <Cessation.rds>')

        infobulle(bt.Ons, 'or browse here')
        status.bar.display(bt.Ons, 'or browse here')
        infobulle(bt.Ces, 'or browse here')
        status.bar.display(bt.Ces, 'or browse here')

        ############################################

        frameSeasTot <- ttklabelframe(subfr1, text = "Seasonal rainfall amounts", relief = 'groove')

        useTotal <- tclVar(GeneralParameters$seastot$useTotal)

        timeSteps <- tclVar()
        CbperiodVAL <- .cdtEnv$tcl$lang$global[['combobox']][['1']][3:5]
        periodVAL <- c('pentad', 'dekadal', 'monthly')
        tclvalue(timeSteps) <- CbperiodVAL[periodVAL %in% GeneralParameters$seastot$Tstep]


        DataType <- tclVar()
        CbdatatypeVAL <- .cdtEnv$tcl$lang$global[['combobox']][['2']][1:2]
        datatypeVAL <- c('cdtstation', 'cdtdataset')
        tclvalue(DataType) <- CbdatatypeVAL[datatypeVAL %in% GeneralParameters$seastot$data.type]


        if(GeneralParameters$seastot$data.type == 'cdtstation'){
            input.Prec <- tclVar(GeneralParameters$seastot$cdtstation$prec)
            txt.INPrec <- 'File containing stations Precip data'
        }else{
            input.Prec <- tclVar(GeneralParameters$seastot$cdtdataset$prec)
            txt.INPrec <- 'Index file (*.rds) of Precip data'
        }
        txt.INPrec.var <- tclVar(txt.INPrec)

        stateSEAS <- if(GeneralParameters$seastot$useTotal) 'normal' else 'disabled'

        chk.seastot <- tkcheckbutton(frameSeasTot, variable = useTotal, text = "Use aggregated data for seasonal total", anchor = 'w', justify = 'left')
        txt.period <- tklabel(frameSeasTot, text = 'Time step', anchor = 'e', justify = 'right')
        cb.period <- ttkcombobox(frameSeasTot, values = CbperiodVAL, textvariable = timeSteps, width = largeur1, state = stateSEAS)
        txt.datatype <- tklabel(frameSeasTot, text = 'Format', anchor = 'e', justify = 'right')
        cb.datatype <- ttkcombobox(frameSeasTot, values = CbdatatypeVAL, textvariable = DataType, width = largeur1, state = stateSEAS)

        txt.INPrec <- tklabel(frameSeasTot, text = tclvalue(txt.INPrec.var), textvariable = txt.INPrec.var, anchor = 'w', justify = 'left')
        if(GeneralParameters$seastot$data.type == 'cdtstation'){
            cb.en.INPrec <- ttkcombobox(frameSeasTot, values = unlist(listOpenFiles), textvariable = input.Prec, width = largeur2, state = stateSEAS)
        }else{
            cb.en.INPrec <- tkentry(frameSeasTot, textvariable = input.Prec, width = largeur0, state = stateSEAS)
        }
        bt.INPrec <- tkbutton(frameSeasTot, text = "...", state = stateSEAS)

        ############

        tkconfigure(bt.INPrec, command = function(){
            if(GeneralParameters$seastot$data.type == 'cdtstation'){
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

        tkgrid(chk.seastot, row = 0, column = 0, sticky = 'we', rowspan = 1, columnspan = 10, padx = 1, pady = 1, ipadx = 1, ipady = 1)

        tkgrid(txt.period, row = 1, column = 0, sticky = 'we', rowspan = 1, columnspan = 2, padx = 1, pady = 1, ipadx = 1, ipady = 1)
        tkgrid(cb.period, row = 1, column = 2, sticky = 'we', rowspan = 1, columnspan = 8, padx = 1, pady = 1, ipadx = 1, ipady = 1)

        tkgrid(txt.datatype, row = 2, column = 0, sticky = 'we', rowspan = 1, columnspan = 2, padx = 1, pady = 1, ipadx = 1, ipady = 1)
        tkgrid(cb.datatype, row = 2, column = 2, sticky = 'we', rowspan = 1, columnspan = 8, padx = 1, pady = 1, ipadx = 1, ipady = 1)

        tkgrid(txt.INPrec, row = 3, column = 0, sticky = 'we', rowspan = 1, columnspan = 10, padx = 1, pady = 1, ipadx = 1, ipady = 1)
        tkgrid(cb.en.INPrec, row = 4, column = 0, sticky = 'we', rowspan = 1, columnspan = 9, padx = 0, pady = 1, ipadx = 1, ipady = 1)
        tkgrid(bt.INPrec, row = 4, column = 9, sticky = 'w', rowspan = 1, columnspan = 1, padx = 0, pady = 1, ipadx = 1, ipady = 1)

        ############
        infobulle(cb.period, 'Select the time step of the input data')
        status.bar.display(cb.period, 'Select the time step of the input data')
        infobulle(cb.datatype, 'Select the format of the input data')
        status.bar.display(cb.datatype, 'Select the format of the input data')

        if(GeneralParameters$seastot$data.type == 'cdtstation'){
            infobulle(cb.en.INPrec, 'Select the file containing the precipitation data')
            status.bar.display(cb.en.INPrec, 'Select the file containing the precipitation data')

            infobulle(bt.INPrec, 'Browse file if not listed')
            status.bar.display(bt.INPrec, 'Browse file if not listed')
        }else{
            infobulle(cb.en.INPrec, 'Enter the full path to the file <precipitation dataset name>.rds')
            status.bar.display(cb.en.INPrec, 'Enter the full path to the file <precipitation dataset name>.rds')

            infobulle(bt.INPrec, 'or browse here')
            status.bar.display(bt.INPrec, 'or browse here')
        }

        ############

        tkbind(chk.seastot, "<Button-1>", function(){
            stateSEAS <- if(tclvalue(useTotal) == '1') 'disabled' else 'normal'
            tkconfigure(cb.period, state = stateSEAS)
            tkconfigure(cb.datatype, state = stateSEAS)
            tkconfigure(cb.en.INPrec, state = stateSEAS)
            tkconfigure(bt.INPrec, state = stateSEAS)
        })

        ############

        tkbind(cb.datatype, "<<ComboboxSelected>>", function(){
            tkdestroy(cb.en.INPrec)
            tclvalue(input.Prec) <- ''

            stateSEAS <- if(tclvalue(useTotal) == '1') 'normal' else 'disabled'

            ###
            if(str_trim(tclvalue(DataType)) == CbdatatypeVAL[1]){
                tclvalue(txt.INPrec.var) <- 'File containing stations Precip data'

                cb.en.INPrec <- ttkcombobox(frameSeasTot, values = unlist(listOpenFiles), textvariable = input.Prec, width = largeur2, state = stateSEAS)

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
                infobulle(cb.en.INPrec, 'Select the file containing the precipitation data')
                status.bar.display(cb.en.INPrec, 'Select the file containing the precipitation data')

                infobulle(bt.INPrec, 'Browse file if not listed')
                status.bar.display(bt.INPrec, 'Browse file if not listed')
            }

            ###
            if(str_trim(tclvalue(DataType)) == CbdatatypeVAL[2]){
                tclvalue(txt.INPrec.var) <- 'Index file (*.rds) of Precip data'

                cb.en.INPrec <- tkentry(frameSeasTot, textvariable = input.Prec, width = largeur0, state = stateSEAS)

                ######
                tkconfigure(bt.INPrec, command = function(){
                    path.rds <- tclvalue(tkgetOpenFile(initialdir = getwd(), filetypes = .cdtEnv$tcl$data$filetypes6))
                    tclvalue(input.Prec) <- if(path.rds %in% c("", "NA")) "" else path.rds
                })

                ######
                infobulle(cb.en.INPrec, 'Enter the full path to the file <precipitation dataset name>.rds')
                status.bar.display(cb.en.INPrec, 'Enter the full path to the file <precipitation dataset name>.rds')

                infobulle(bt.INPrec, 'or browse here')
                status.bar.display(bt.INPrec, 'or browse here')
            }

            #######
            tkgrid(cb.en.INPrec, row = 4, column = 0, sticky = 'we', rowspan = 1, columnspan = 9, padx = 0, pady = 1, ipadx = 1, ipady = 1)

            #######
            tkbind(chk.seastot, "<Button-1>", function(){
                stateSEAS <- if(tclvalue(useTotal) == '1') 'disabled' else 'normal'
                tkconfigure(cb.period, state = stateSEAS)
                tkconfigure(cb.datatype, state = stateSEAS)
                tkconfigure(cb.en.INPrec, state = stateSEAS)
                tkconfigure(bt.INPrec, state = stateSEAS)
            })
        })

        ############################################

        tkgrid(frameInData, row = 0, column = 0, sticky = 'we', padx = 1, pady = 3, ipadx = 1, ipady = 1)
        tkgrid(frameSeasTot, row = 1, column = 0, sticky = 'we', padx = 1, pady = 3, ipadx = 1, ipady = 1)

    #######################################################################################################

    #Tab2
    subfr2 <- bwTabScrollableFrame(cmd.tab2)

        ##############################################

        frameDryDay <- ttklabelframe(subfr2, text = "Dry Day definition", relief = 'groove')

        drywet.day <- tclVar(GeneralParameters$dryday)

        txt.DryDay1 <- tklabel(frameDryDay, text = 'Rainfall amount below', anchor = 'w', justify = 'left')
        en.DryDay <- tkentry(frameDryDay, textvariable = drywet.day, width = 5)
        txt.DryDay2 <- tklabel(frameDryDay, text = 'mm/day', anchor = 'w', justify = 'left')

        tkgrid(txt.DryDay1, en.DryDay, txt.DryDay2)

        ##############################################

        frameMinFrac <- tkframe(subfr2, relief = 'groove', borderwidth = 2)

        min.frac <- tclVar(GeneralParameters$min.frac)

        txt.MinFrac <- tklabel(frameMinFrac, text = 'Minimum fraction of non-missing values', anchor = 'w', justify = 'left')
        en.MinFrac <- tkentry(frameMinFrac, textvariable = min.frac, width = 5)
        tkgrid(txt.MinFrac, en.MinFrac)

        ##############################################

        frameDirSav <- tkframe(subfr2, relief = 'groove', borderwidth = 2)

        dir.save <- tclVar(GeneralParameters$output)

        txt.dir.save <- tklabel(frameDirSav, text = "Directory to save results", anchor = 'w', justify = 'left')
        en.dir.save <- tkentry(frameDirSav, textvariable = dir.save, width = largeur0)
        bt.dir.save <- tkbutton(frameDirSav, text = "...")

        ######
        tkconfigure(bt.dir.save, command = function() fileORdir2Save(dir.save, isFile = FALSE))

        ######
        tkgrid(txt.dir.save, row = 0, column = 0, sticky = 'we', rowspan = 1, columnspan = 5, padx = 1, pady = 0, ipadx = 1, ipady = 1)
        tkgrid(en.dir.save, row = 1, column = 0, sticky = 'we', rowspan = 1, columnspan = 4, padx = 0, pady = 0, ipadx = 1, ipady = 1)
        tkgrid(bt.dir.save, row = 1, column = 4, sticky = 'w', rowspan = 1, columnspan = 1, padx = 0, pady = 0, ipadx = 1, ipady = 1)

        infobulle(en.dir.save, 'Enter the full path to directory to save outputs')
        status.bar.display(en.dir.save, 'Enter the full path to directory to save outputs')
        infobulle(bt.dir.save, 'or browse here')
        status.bar.display(bt.dir.save, 'or browse here')

        ############################################

        bt.CalcPICSA <- ttkbutton(subfr2, text = 'Run Analysis')

        tkconfigure(bt.CalcPICSA, command = function(){
            GeneralParameters$onset <- str_trim(tclvalue(input.Onset))
            GeneralParameters$cessation <- str_trim(tclvalue(input.Cessation))
            GeneralParameters$output <- str_trim(tclvalue(dir.save))

            GeneralParameters$min.frac <- as.numeric(str_trim(tclvalue(min.frac)))
            GeneralParameters$dryday <- as.numeric(str_trim(tclvalue(drywet.day)))

            GeneralParameters$seastot$useTotal <- switch(tclvalue(useTotal), '0' = FALSE, '1' = TRUE)
            if(tclvalue(useTotal) == "1"){
                GeneralParameters$seastot$Tstep <- periodVAL[CbperiodVAL %in% str_trim(tclvalue(timeSteps))]
                GeneralParameters$seastot$data.type <- datatypeVAL[CbdatatypeVAL %in% str_trim(tclvalue(DataType))]

                if(str_trim(tclvalue(DataType)) == CbdatatypeVAL[1])
                    GeneralParameters$seastot$cdtstation$prec <- str_trim(tclvalue(input.Prec))

                if(str_trim(tclvalue(DataType)) == CbdatatypeVAL[2])
                    GeneralParameters$seastot$cdtdataset$prec <- str_trim(tclvalue(input.Prec))
            }

            # assign('GeneralParameters', GeneralParameters, envir = .GlobalEnv)

            Insert.Messages.Out("Calculate rainy season data ......", TRUE, "i")

            tkconfigure(.cdtEnv$tcl$main$win, cursor = 'watch')
            tcl('update')
            ret <- tryCatch(
                {
                    compute_RainySeasonData(GeneralParameters)
                },
                warning = function(w) warningFun(w),
                error = function(e) errorFun(e),
                finally = {
                    tkconfigure(.cdtEnv$tcl$main$win, cursor = '')
                    tcl('update')
                }
            )

            msg0 <- "Rainy season data calculation finished successfully"
            msg1 <- "Rainy season data calculation failed"

            if(!is.null(ret)){
                if(ret == 0){
                    Insert.Messages.Out(msg0, TRUE, "s")

                    .cdtData$EnvData$plot.maps$data.type <- .cdtData$EnvData$output$data.type
                    .cdtData$EnvData$plot.maps[c('lon', 'lat', 'id')] <- .cdtData$EnvData$output$data[c('lon', 'lat', 'id')]
                    ###################

                    load.PICSA.Data()

                }else Insert.Messages.Out(msg1, format = TRUE)
            }else Insert.Messages.Out(msg1, format = TRUE)
        })

        ############################################

        tkgrid(frameDryDay, row = 0, column = 0, sticky = 'we', padx = 1, pady = 3, ipadx = 1, ipady = 1)
        tkgrid(frameMinFrac, row = 1, column = 0, sticky = 'we', padx = 1, pady = 3, ipadx = 1, ipady = 1)
        tkgrid(frameDirSav, row = 2, column = 0, sticky = 'we', padx = 1, pady = 3, ipadx = 1, ipady = 1)
        tkgrid(bt.CalcPICSA, row = 3, column = 0, sticky = 'we', padx = 1, pady = 3, ipadx = 1, ipady = 1)

    #######################################################################################################

    #Tab3
    subfr3 <- bwTabScrollableFrame(cmd.tab3)

        ##############################################

        framePICSADat <- ttklabelframe(subfr3, text = "Analysis data", relief = 'groove')

        .cdtData$EnvData$DirExist <- tclVar(0)
        file.PICSAIndex <- tclVar()

        statePICSADat <- if(tclvalue(.cdtData$EnvData$DirExist) == "1") "normal" else "disabled"

        chk.PICSAIdx <- tkcheckbutton(framePICSADat, variable = .cdtData$EnvData$DirExist, text = "Rainy season data are already calculated", anchor = 'w', justify = 'left')
        en.PICSAIdx <- tkentry(framePICSADat, textvariable = file.PICSAIndex, width = largeur0, state = statePICSADat)
        bt.PICSAIdx <- tkbutton(framePICSADat, text = "...", state = statePICSADat)

        tkconfigure(bt.PICSAIdx, command = function(){
            path.Stat <- tclvalue(tkgetOpenFile(initialdir = getwd(), filetypes = .cdtEnv$tcl$data$filetypes6))
            if(path.Stat %in% c("", "NA") | is.na(path.Stat)) return(NULL)
            tclvalue(file.PICSAIndex) <- path.Stat

            if(file.exists(str_trim(tclvalue(file.PICSAIndex)))){
                OutPicsa <- try(readRDS(str_trim(tclvalue(file.PICSAIndex))), silent = TRUE)
                if(inherits(OutPicsa, "try-error")){
                    Insert.Messages.Out('Unable to load Analysis data', format = TRUE)
                    Insert.Messages.Out(gsub('[\r\n]', '', OutPicsa[1]), format = TRUE)
                    return(NULL)
                }

                .cdtData$EnvData$output <- OutPicsa
                .cdtData$EnvData$PathPicsa <- dirname(str_trim(tclvalue(file.PICSAIndex)))
                .cdtData$EnvData$plot.maps$data.type <- .cdtData$EnvData$output$data.type
                .cdtData$EnvData$plot.maps[c('lon', 'lat', 'id')] <- .cdtData$EnvData$output$data[c('lon', 'lat', 'id')]
                ###################

                load.PICSA.Data()
            }
        })

        tkgrid(chk.PICSAIdx, row = 0, column = 0, sticky = 'we', rowspan = 1, columnspan = 5, padx = 1, pady = 1, ipadx = 1, ipady = 1)
        tkgrid(en.PICSAIdx, row = 1, column = 0, sticky = 'we', rowspan = 1, columnspan = 4, padx = 1, pady = 1, ipadx = 1, ipady = 1)
        tkgrid(bt.PICSAIdx, row = 1, column = 4, sticky = 'w', rowspan = 1, columnspan = 1, padx = 0, pady = 1, ipadx = 1, ipady = 1)

        infobulle(chk.PICSAIdx, "Check this box if the Analysis data are already calculated")
        status.bar.display(chk.PICSAIdx, "Check this box if the Analysis data are already calculated")
        infobulle(en.PICSAIdx, "Enter the full path to the file <RainySeasonAnalysis.rds>")
        status.bar.display(en.PICSAIdx, "Enter the full path to the file <RainySeasonAnalysis.rds>")
        infobulle(bt.PICSAIdx, 'or browse here')
        status.bar.display(bt.PICSAIdx, 'or browse here')

        ###############

        tkbind(chk.PICSAIdx, "<Button-1>", function(){
            statePICSADat <- if(tclvalue(.cdtData$EnvData$DirExist) == '1') 'disabled' else 'normal'
            tkconfigure(en.PICSAIdx, state = statePICSADat)
            tkconfigure(bt.PICSAIdx, state = statePICSADat)
            stateCaclBut <- if(tclvalue(.cdtData$EnvData$DirExist) == '1') 'normal' else 'disabled'
            tkconfigure(bt.CalcPICSA, state = stateCaclBut)
        })

        ##############################################

        framePICSATSMap <- ttklabelframe(subfr3, text = "Maps", relief = 'groove')

        .cdtData$EnvData$varPICSA <- tclVar("Onset")
        varPICSA.val <- c("Onset", "Cessation", "Season Length", "Seasonal Rainfall Amounts",
                        "Dry Spells", "Longest Dry Spell", 
                        "Number of rain day", "Maximum daily rain",
                        "Total rain when RR>95thPerc", "Nb of day when RR>95thPerc")

        stateDrySpl <- 'disabled'

        cb.TsMap.picsavar <- ttkcombobox(framePICSATSMap, values = varPICSA.val, textvariable = .cdtData$EnvData$varPICSA, width = largeur3)
        txt.TsMap.dryspell <- tklabel(framePICSATSMap, text = "DrySpell", anchor = 'w', justify = 'left')
        .cdtData$EnvData$spin.TsMap.dryspell <- ttkspinbox(framePICSATSMap, from = 1, to = 50, increment = 1, justify = 'center', width = 2, state = stateDrySpl)
        tkset(.cdtData$EnvData$spin.TsMap.dryspell, 5)

        bt.TsMap.prev <- ttkbutton(framePICSATSMap, text = "<<", width = 4)
        bt.TsMap.next <- ttkbutton(framePICSATSMap, text = ">>", width = 4)
        .cdtData$EnvData$spin.TsMap.year <- ttkspinbox(framePICSATSMap, from = 1800, to = 2200, increment = 1, justify = 'center', width = 4)
        tkset(.cdtData$EnvData$spin.TsMap.year, 2015)
        bt.TsMap.plot <- ttkbutton(framePICSATSMap, text = .cdtEnv$tcl$lang$global[['button']][['3']])
        bt.TsMap.Opt <- ttkbutton(framePICSATSMap, text = .cdtEnv$tcl$lang$global[['button']][['4']])

        #################

        .cdtData$EnvData$tab$pointSize.TSMap <- NULL
        .cdtData$EnvData$TSMapOp <- list(presetCol = list(color = 'tim.colors', reverse = FALSE),
                                            userCol = list(custom = FALSE, color = NULL),
                                            userLvl = list(custom = FALSE, levels = NULL, equidist = FALSE),
                                            title = list(user = FALSE, title = ''),
                                            colkeyLab = list(user = FALSE, label = ''),
                                            scalebar = list(add = FALSE, pos = 'bottomleft'),
                                            pointSize = .cdtData$EnvData$tab$pointSize.TSMap)

        tkconfigure(bt.TsMap.Opt, command = function(){
            if(!is.null(.cdtData$EnvData$tsdata)){
                atlevel <- pretty(.cdtData$EnvData$tsdata$z, n = 10, min.n = 7)
                if(is.null(.cdtData$EnvData$TSMapOp$userLvl$levels)){
                    .cdtData$EnvData$TSMapOp$userLvl$levels <- atlevel
                }else{
                    if(!.cdtData$EnvData$TSMapOp$userLvl$custom)
                        .cdtData$EnvData$TSMapOp$userLvl$levels <- atlevel
                }
            }
            .cdtData$EnvData$TSMapOp <- MapGraph.MapOptions(.cdtData$EnvData$TSMapOp)

            if(str_trim(tclvalue(.cdtData$EnvData$plot.maps$plot.type)) == "Points")
                .cdtData$EnvData$tab$pointSize.TSMap <- .cdtData$EnvData$TSMapOp$pointSize
        })

        .cdtData$EnvData$tab$TSMap <- NULL
        tkconfigure(bt.TsMap.plot, command = function(){
            if(!is.null(.cdtData$EnvData$tsdata)){
                ret <- read.PicsaTSData()
                if(is.null(ret)) return(NULL)

                SeasonAnalysis.Display.TSMaps()
            }
        })

        tkconfigure(bt.TsMap.prev, command = function(){
            if(!is.null(.cdtData$EnvData$tsdata)){
                range.TsMap.year <- range(as.numeric(format(.cdtData$EnvData$output$start.date, '%Y')))
                iyear <- as.numeric(str_trim(tclvalue(tkget(.cdtData$EnvData$spin.TsMap.year))))
                iyear <- iyear - 1
                if(iyear < range.TsMap.year[1]) iyear <- range.TsMap.year[2]
                tkset(.cdtData$EnvData$spin.TsMap.year, iyear)

                ret <- read.PicsaTSData()
                if(is.null(ret)) return(NULL)

                SeasonAnalysis.Display.TSMaps()
            }
        })

        tkconfigure(bt.TsMap.next, command = function(){
            if(!is.null(.cdtData$EnvData$tsdata)){
                range.TsMap.year <- range(as.numeric(format(.cdtData$EnvData$output$start.date, '%Y')))
                iyear <- as.numeric(str_trim(tclvalue(tkget(.cdtData$EnvData$spin.TsMap.year))))
                iyear <- iyear + 1
                if(iyear > range.TsMap.year[2]) iyear <- range.TsMap.year[1]
                tkset(.cdtData$EnvData$spin.TsMap.year, iyear)

                ret <- read.PicsaTSData()
                if(is.null(ret)) return(NULL)

                SeasonAnalysis.Display.TSMaps()
            }
        })

        #################

        tkgrid(cb.TsMap.picsavar, row = 0, column = 0, sticky = 'we', rowspan = 1, columnspan = 3, padx = 1, pady = 1, ipadx = 1, ipady = 1)
        tkgrid(txt.TsMap.dryspell, row = 0, column = 3, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
        tkgrid(.cdtData$EnvData$spin.TsMap.dryspell, row = 0, column = 4, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)

        tkgrid(bt.TsMap.prev, row = 1, column = 0, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
        tkgrid(.cdtData$EnvData$spin.TsMap.year, row = 1, column = 1, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
        tkgrid(bt.TsMap.next, row = 1, column = 2, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
        tkgrid(bt.TsMap.plot, row = 1, column = 3, sticky = 'we', rowspan = 1, columnspan = 2, padx = 1, pady = 1, ipadx = 1, ipady = 1)

        tkgrid(bt.TsMap.Opt, row = 2, column = 3, sticky = 'we', rowspan = 1, columnspan = 2, padx = 1, pady = 1, ipadx = 1, ipady = 1)

        infobulle(cb.TsMap.picsavar, "Select the variable to plot")
        status.bar.display(cb.TsMap.picsavar, "Select the variable to plot")
        infobulle(.cdtData$EnvData$spin.TsMap.dryspell, "Dry spell definition (continuous dry days)")
        status.bar.display(.cdtData$EnvData$spin.TsMap.dryspell, "Dry spell definition (continuous dry days)")
        infobulle(bt.TsMap.prev, "Plot the previous year")
        status.bar.display(bt.TsMap.prev, "Plot the previous year")
        infobulle(bt.TsMap.next, "Plot the next year")
        status.bar.display(bt.TsMap.next, "Plot the next year")
        infobulle(.cdtData$EnvData$spin.TsMap.year, "Select the year to plot")
        status.bar.display(.cdtData$EnvData$spin.TsMap.year, "Select the year to plot")

        #################

        tkbind(cb.TsMap.picsavar, "<<ComboboxSelected>>", function(){
            stateDrySpl <- if(tclvalue(.cdtData$EnvData$varPICSA) == "Dry Spells") "normal" else "disabled"
            tkconfigure(.cdtData$EnvData$spin.TsMap.dryspell, state = stateDrySpl)

            ret <- read.PicsaTSData()
            if(is.null(ret)) return(NULL)
        })

        ##############################################

        framePICSACLMMap <- ttklabelframe(subfr3, text = "Climatological Analysis", relief = 'groove')

        ANALYSIS <- c('Average', 'Median', 'Standard deviation', 'Trend', 'Percentiles', 'Frequency')
        .cdtData$EnvData$analysis.method <- tclVar('Average')
        .cdtData$EnvData$mth.perc <- tclVar(95)
        .cdtData$EnvData$low.thres <- tclVar("09-15")
        .cdtData$EnvData$up.thres <- tclVar("11-30")
        .cdtData$EnvData$trend <- tclVar("Change (trend) / year")
        TRENDOPT <- c("Change (trend) / year", "Change (trend) over the period", "Change (trend) / average (in %)")

        statePrc <- if(tclvalue(.cdtData$EnvData$analysis.method) == 'Percentiles') 'normal' else 'disabled'
        stateFrq <- if(tclvalue(.cdtData$EnvData$analysis.method) == 'Frequency') 'normal' else 'disabled'
        stateTrend <- if(tclvalue(.cdtData$EnvData$analysis.method) == 'Trend') 'normal' else 'disabled'

        cb.anMthd <- ttkcombobox(framePICSACLMMap, values = ANALYSIS, textvariable = .cdtData$EnvData$analysis.method, width = largeur3)
        txt.Percent <- tklabel(framePICSACLMMap, text = "Percentile", anchor = 'w', justify = 'left')
        en.Percent <- tkentry(framePICSACLMMap, textvariable = .cdtData$EnvData$mth.perc, width = 3, state = statePrc)

        txt.Freq1 <- tklabel(framePICSACLMMap, text = "Between", anchor = 'w', justify = 'left')
        en.Freq1 <- tkentry(framePICSACLMMap, textvariable = .cdtData$EnvData$low.thres, width = 5, state = stateFrq)
        txt.Freq2 <- tklabel(framePICSACLMMap, text = "And", anchor = 'w', justify = 'left')
        en.Freq2 <- tkentry(framePICSACLMMap, textvariable = .cdtData$EnvData$up.thres, width = 5, state = stateFrq)
        bt.ClimMap.plot <- ttkbutton(framePICSACLMMap, text = .cdtEnv$tcl$lang$global[['button']][['3']])
        cb.Trend <- ttkcombobox(framePICSACLMMap, values = TRENDOPT, textvariable = .cdtData$EnvData$trend, width = largeur3, state = stateTrend)
        bt.ClimMap.Opt <- ttkbutton(framePICSACLMMap, text = .cdtEnv$tcl$lang$global[['button']][['4']])

        #################

        .cdtData$EnvData$tab$pointSize.climMap <- NULL
        .cdtData$EnvData$climMapOp <- list(presetCol = list(color = 'tim.colors', reverse = FALSE),
                                            userCol = list(custom = FALSE, color = NULL),
                                            userLvl = list(custom = FALSE, levels = NULL, equidist = FALSE),
                                            title = list(user = FALSE, title = ''),
                                            colkeyLab = list(user = FALSE, label = ''),
                                            scalebar = list(add = FALSE, pos = 'bottomleft'),
                                            pointSize = .cdtData$EnvData$tab$pointSize.climMap)

        tkconfigure(bt.ClimMap.Opt, command = function(){
            if(!is.null(.cdtData$EnvData$climdata)){
                atlevel <- pretty(.cdtData$EnvData$climdata$z, n = 10, min.n = 7)
                if(is.null(.cdtData$EnvData$climMapOp$userLvl$levels)){
                    .cdtData$EnvData$climMapOp$userLvl$levels <- atlevel
                }else{
                    if(!.cdtData$EnvData$climMapOp$userLvl$custom)
                        .cdtData$EnvData$climMapOp$userLvl$levels <- atlevel
                }
            }
            .cdtData$EnvData$climMapOp <- MapGraph.MapOptions(.cdtData$EnvData$climMapOp)

            if(str_trim(tclvalue(.cdtData$EnvData$plot.maps$plot.type)) == "Points")
                .cdtData$EnvData$tab$pointSize.climMap <- .cdtData$EnvData$climMapOp$pointSize
        })

        .cdtData$EnvData$tab$ClimMap <- NULL
        tkconfigure(bt.ClimMap.plot, command = function(){
            ret <- calculate.ClimStat()
            if(is.null(ret)) return(NULL)

            if(!is.null(.cdtData$EnvData$climdata)) SeasonAnalysis.Display.ClimMap()
        })

        #################

        tkgrid(cb.anMthd, row = 0, column = 0, sticky = 'we', rowspan = 1, columnspan = 4, padx = 1, pady = 1, ipadx = 1, ipady = 1)
        tkgrid(txt.Percent, row = 0, column = 4, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
        tkgrid(en.Percent, row = 0, column = 5, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)

        tkgrid(txt.Freq1, row = 1, column = 0, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
        tkgrid(en.Freq1, row = 1, column = 1, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
        tkgrid(txt.Freq2, row = 1, column = 2, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
        tkgrid(en.Freq2, row = 1, column = 3, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
        tkgrid(bt.ClimMap.plot, row = 1, column = 4, sticky = 'we', rowspan = 1, columnspan = 2, padx = 1, pady = 1, ipadx = 1, ipady = 1)
        tkgrid(cb.Trend, row = 2, column = 0, sticky = 'we', rowspan = 1, columnspan = 4, padx = 1, pady = 1, ipadx = 1, ipady = 1)
        tkgrid(bt.ClimMap.Opt, row = 2, column = 4, sticky = 'we', rowspan = 1, columnspan = 2, padx = 1, pady = 1, ipadx = 1, ipady = 1)

        infobulle(cb.anMthd, "Select the analysis method")
        status.bar.display(cb.anMthd, "Select the analysis method")
        infobulle(en.Percent, "Enter the nth percentile to be calculated")
        status.bar.display(en.Percent, "Enter the nth percentile to be calculated")
        infobulle(en.Freq1, "Enter the lower bound of the interval to count the number of occurrences.\nIn the case of Onset and Cessation, the limit should be of the form Month-Day,\nnumber otherwise")
        status.bar.display(en.Freq1, "Enter the lower bound of the interval to count the number of occurrences.\nIn the case of Onset and Cessation, the limit should be of the form Month-Day,\nnumber otherwise")
        infobulle(en.Freq2, "Enter the upper bound of the interval to count the number of occurrences.\nIn the case of Onset and Cessation, the limit should be of the form Month-Day,\nnumber otherwise")
        status.bar.display(en.Freq2, "Enter the upper bound of the interval to count the number of occurrences.\nIn the case of Onset and Cessation, the limit should be of the form Month-Day,\nnumber otherwise")

        #################
        tkbind(cb.anMthd, "<<ComboboxSelected>>", function(){
            statePrc <- if(tclvalue(.cdtData$EnvData$analysis.method) == 'Percentiles') 'normal' else 'disabled'
            stateFrq <- if(tclvalue(.cdtData$EnvData$analysis.method) == 'Frequency') 'normal' else 'disabled'
            stateTrend <- if(tclvalue(.cdtData$EnvData$analysis.method) == 'Trend') 'normal' else 'disabled'
            tkconfigure(en.Percent, state = statePrc)
            tkconfigure(en.Freq1, state = stateFrq)
            tkconfigure(en.Freq2, state = stateFrq)
            tkconfigure(cb.Trend, state = stateTrend)
        })

        ##############################################

        framePlotType <- tkframe(subfr3)

        .cdtData$EnvData$plot.maps$plot.type <- tclVar("Pixels")

        txt.plotType <- tklabel(framePlotType, text = "Plot Type", anchor = 'e', justify = 'right')
        cb.plotType <- ttkcombobox(framePlotType, values = "Pixels", textvariable = .cdtData$EnvData$plot.maps$plot.type, width = largeur3)

        tkgrid(txt.plotType, row = 0, column = 0, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
        tkgrid(cb.plotType, row = 0, column = 1, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)

        ###############

        tkbind(cb.plotType, "<<ComboboxSelected>>", function(){
            ret <- read.PicsaTSData()
            if(is.null(ret)) return(NULL)

            ########
            ret <- calculate.ClimStat()
            if(is.null(ret)) return(NULL)
        })

        ##############################################

        tkgrid(framePICSADat, row = 0, column = 0, sticky = 'we', padx = 1, pady = 1, ipadx = 1, ipady = 1)
        tkgrid(framePICSATSMap, row = 1, column = 0, sticky = 'we', padx = 1, pady = 1, ipadx = 1, ipady = 1)
        tkgrid(framePICSACLMMap, row = 2, column = 0, sticky = 'we', padx = 1, pady = 1, ipadx = 1, ipady = 1)
        tkgrid(framePlotType, row = 3, column = 0, sticky = '', padx = 1, pady = 3, ipadx = 1, ipady = 1)

    #######################################################################################################

    #Tab4
    subfr4 <- bwTabScrollableFrame(cmd.tab4)

        ##############################################

        framePICSATSGRAPH <- ttklabelframe(subfr4, text = "Time Series Graph", relief = 'groove')

        varTSPLOT <- c("From Maps", "Daily Rainfall")
        .cdtData$EnvData$plot.maps$varTSp <- tclVar("From Maps")

        typeTSPLOT <- c("Line", "Barplot", "Probability", "ENSO-Line", "ENSO-Barplot", "ENSO-Proba", "Anomaly")
        .cdtData$EnvData$plot.maps$typeTSp <- tclVar("Line")
        .cdtData$EnvData$plot.maps$averageTSp <- tclVar(FALSE)
        .cdtData$EnvData$plot.maps$tercileTSp <- tclVar(FALSE)
        .cdtData$EnvData$plot.maps$trendTSp <- tclVar(FALSE)

        stateTsp <- if(tclvalue(.cdtData$EnvData$plot.maps$varTSp) == "From Maps") "normal" else "disabled"
        stateType <- if(tclvalue(.cdtData$EnvData$plot.maps$typeTSp) %in% c("Line", "ENSO-Line") && tclvalue(.cdtData$EnvData$plot.maps$varTSp) == "From Maps") "normal" else "disabled"

        frTS0 <- tkframe(framePICSATSGRAPH)
        cb.varTSp <- ttkcombobox(frTS0, values = varTSPLOT, textvariable = .cdtData$EnvData$plot.maps$varTSp, width = largeur4)
        cb.typeTSp <- ttkcombobox(frTS0, values = typeTSPLOT, textvariable = .cdtData$EnvData$plot.maps$typeTSp, width = largeur5, state = stateTsp)
        bt.TsGraph.plot <- ttkbutton(frTS0, text = .cdtEnv$tcl$lang$global[['button']][['3']], width = 8)
        tkgrid(cb.varTSp, cb.typeTSp, bt.TsGraph.plot)

        frTS1 <- tkframe(framePICSATSGRAPH)
        chk.meanTSp <- tkcheckbutton(frTS1, variable = .cdtData$EnvData$plot.maps$averageTSp, text = "Add Mean", anchor = 'w', justify = 'left', state = stateType)
        chk.tercTSp <- tkcheckbutton(frTS1, variable = .cdtData$EnvData$plot.maps$tercileTSp, text = "Add Terciles", anchor = 'w', justify = 'left', state = stateType)
        chk.trendTSp <- tkcheckbutton(frTS1, variable = .cdtData$EnvData$plot.maps$trendTSp, text = "Add Trend", anchor = 'w', justify = 'left', state = stateType)
        tkgrid(chk.meanTSp, chk.tercTSp, chk.trendTSp)

        bt.TsGraph.Opt <- ttkbutton(framePICSATSGRAPH, text = .cdtEnv$tcl$lang$global[['button']][['4']], width = 8, state = stateTsp)

        #################
        .cdtData$EnvData$TSGraphOp <- list(
                                anomaly = list(
                                        anom = list(perc.anom = FALSE, basePeriod = FALSE, startYr.anom = 1981, endYr.anom = 2010),
                                        xlim = list(is.min = FALSE, min = 1981, is.max = FALSE, max = 2017),
                                        ylim = list(is.min = FALSE, min = -100, is.max = FALSE, max = 100),
                                        axislabs = list(is.xlab = FALSE, xlab = '', is.ylab = FALSE, ylab = ''),
                                        title = list(is.title = FALSE, title = '', position = 'top'),
                                        colors = list(negative = "blue", positive = "red")
                                        ),
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
                                        is = list(mean = FALSE, tercile = FALSE, linear = FALSE),
                                        add = list(mean = FALSE, tercile = FALSE, linear = FALSE),
                                        col = list(mean = "black", tercile1 = "green", tercile2 = "blue", linear = "purple3"),
                                        text = list(mean = "Average", tercile1 = "Tercile 0.33333", tercile2 = "Tercile 0.66666", linear = "Trend line"),
                                        lwd = list(mean = 2, tercile = 2, linear = 2))
                                    ),
                                proba = list(
                                    xlim = list(is.min = FALSE, min = 0, is.max = FALSE, max = 100),
                                    ylim = list(is.min = FALSE, min = 0, is.max = FALSE, max = 100),
                                    axislabs = list(is.xlab = FALSE, xlab = '', is.ylab = FALSE, ylab = ''),
                                    title = list(is.title = FALSE, title = '', position = 'top'),
                                    plot = list(type = 'both',
                                        col = list(line = "red", points = "blue"),
                                        lwd = 2, cex = 0.8),
                                    proba = list(theoretical = TRUE, col = 'black', lwd = 2)
                                    ),
                                line.enso = list(
                                    xlim = list(is.min = FALSE, min = 1981, is.max = FALSE, max = 2017),
                                    ylim = list(is.min = FALSE, min = 0, is.max = FALSE, max = 100),
                                    axislabs = list(is.xlab = FALSE, xlab = '', is.ylab = FALSE, ylab = ''),
                                    title = list(is.title = FALSE, title = '', position = 'top'),
                                    plot = list(lwd = 2, cex = 2, col = list(line = "black",
                                                points = c("blue", "gray", "red"))),
                                    legend = list(
                                        is = list(mean = FALSE, tercile = FALSE, linear = FALSE),
                                        add = list(mean = FALSE, tercile = FALSE, linear = FALSE),
                                        col = list(mean = "darkblue", tercile1 = "chartreuse4", tercile2 = "darkgoldenrod4", linear = "purple3"),
                                        text = list(mean = "Average", tercile1 = "Tercile 0.33333", tercile2 = "Tercile 0.66666", linear = "Trend line"),
                                        lwd = list(mean = 2, tercile = 2, linear = 2))
                                    ),
                                bar.enso = list(
                                    xlim = list(is.min = FALSE, min = 1981, is.max = FALSE, max = 2017),
                                    ylim = list(is.min = FALSE, min = 0, is.max = FALSE, max = 100),
                                    axislabs = list(is.xlab = FALSE, xlab = '', is.ylab = FALSE, ylab = ''),
                                    title = list(is.title = FALSE, title = '', position = 'top'),
                                    colors = list(col = c("blue", "gray", "red"))
                                    ),
                                proba.enso = list(
                                    xlim = list(is.min = FALSE, min = 0, is.max = FALSE, max = 100),
                                    ylim = list(is.min = FALSE, min = 0, is.max = FALSE, max = 100),
                                    axislabs = list(is.xlab = FALSE, xlab = '', is.ylab = FALSE, ylab = ''),
                                    title = list(is.title = FALSE, title = '', position = 'top'),
                                    plot = list(type = 'both', lwd = 2, cex = 1.4,
                                        all = list(line = "black", points = "lightgray"),
                                        nina = list(line = "blue", points = "lightblue"),
                                        neutre = list(line = "gray", points = "lightgray"),
                                        nino = list(line = "red", points = "lightpink"))
                                    )
                                )

        tkconfigure(bt.TsGraph.Opt, command = function(){
            suffix.fun <- switch(tclvalue(.cdtData$EnvData$plot.maps$typeTSp),
                                    "Anomaly" = "Anomaly",
                                    "Barplot" = "Bar",
                                    "Line" = "Line",
                                    "Probability" = "Proba",
                                    "ENSO-Line" = "LineENSO",
                                    "ENSO-Barplot" = "BarENSO",
                                    "ENSO-Proba" = "ProbaENSO")
            plot.fun <- get(paste0("MapGraph.GraphOptions.", suffix.fun), mode = "function")
            .cdtData$EnvData$TSGraphOp <- plot.fun(.cdtData$EnvData$TSGraphOp)
        })

        .cdtData$EnvData$tab$Tsplot <- NULL
        tkconfigure(bt.TsGraph.plot, command = function(){
            if(!is.null(.cdtData$EnvData$output)){
                imgContainer <- CDT.Display.Graph(SeasonAnalysis.plot.TSGraph, .cdtData$EnvData$tab$Tsplot, 'Time-Series-Plot')
                .cdtData$EnvData$tab$Tsplot <- imageNotebookTab_unik(imgContainer, .cdtData$EnvData$tab$Tsplot)
            }
        })

        tkgrid(frTS0, row = 0, column = 0, sticky = 'we', pady = 1, columnspan = 3)
        tkgrid(frTS1, row = 1, column = 0, sticky = 'we', pady = 1, columnspan = 3)
        tkgrid(bt.TsGraph.Opt, row = 2, column = 2, sticky = 'e', pady = 1, columnspan = 1)

        #################

        tkbind(cb.varTSp, "<<ComboboxSelected>>", function(){
            stateTsp <- if(tclvalue(.cdtData$EnvData$plot.maps$varTSp) == "From Maps") "normal" else "disabled"
            tkconfigure(cb.typeTSp, state = stateTsp)
            tkconfigure(bt.TsGraph.Opt, state = stateTsp)

            stateType <- if(tclvalue(.cdtData$EnvData$plot.maps$typeTSp) %in% c("Line", "ENSO-Line") && tclvalue(.cdtData$EnvData$plot.maps$varTSp) == "From Maps") "normal" else "disabled"
            tkconfigure(chk.meanTSp, state = stateType)
            tkconfigure(chk.tercTSp, state = stateType)
            tkconfigure(chk.trendTSp, state = stateType)
        })

        tkbind(cb.typeTSp, "<<ComboboxSelected>>", function(){
            stateType <- if(tclvalue(.cdtData$EnvData$plot.maps$typeTSp) %in% c("Line", "ENSO-Line") && tclvalue(.cdtData$EnvData$plot.maps$varTSp) == "From Maps") "normal" else "disabled"
            tkconfigure(chk.meanTSp, state = stateType)
            tkconfigure(chk.tercTSp, state = stateType)
            tkconfigure(chk.trendTSp, state = stateType)
        })

        tkbind(chk.meanTSp, "<Button-1>", function(){
            .cdtData$EnvData$TSGraphOp$line$legend$add$mean <- 
                        if(tclvalue(.cdtData$EnvData$plot.maps$averageTSp) == '0') TRUE else FALSE
            .cdtData$EnvData$TSGraphOp$line.enso$legend$add$mean <- 
                        if(tclvalue(.cdtData$EnvData$plot.maps$averageTSp) == '0') TRUE else FALSE
        })

        tkbind(chk.tercTSp, "<Button-1>", function(){
            .cdtData$EnvData$TSGraphOp$line$legend$add$tercile <- 
                        if(tclvalue(.cdtData$EnvData$plot.maps$tercileTSp) == '0') TRUE else FALSE
            .cdtData$EnvData$TSGraphOp$line.enso$legend$add$tercile <- 
                        if(tclvalue(.cdtData$EnvData$plot.maps$tercileTSp) == '0') TRUE else FALSE
        })

        tkbind(chk.trendTSp, "<Button-1>", function(){
            .cdtData$EnvData$TSGraphOp$line$legend$add$linear <- 
                        if(tclvalue(.cdtData$EnvData$plot.maps$trendTSp) == '0') TRUE else FALSE
            .cdtData$EnvData$TSGraphOp$line.enso$legend$add$linear <- 
                        if(tclvalue(.cdtData$EnvData$plot.maps$trendTSp) == '0') TRUE else FALSE
        })

        ##############################################

        frameSTNCrds <- ttklabelframe(subfr4, text = "Station/Coordinates", relief = 'groove')

        frTS2 <- tkframe(frameSTNCrds)
        .cdtData$EnvData$plot.maps$lonLOC <- tclVar()
        .cdtData$EnvData$plot.maps$latLOC <- tclVar()
        .cdtData$EnvData$plot.maps$stnIDTSp <- tclVar()

        tkgrid(frTS2, row = 0, column = 0, sticky = 'e', pady = 1)

        ##############################################

        tkgrid(framePICSATSGRAPH, row = 0, column = 0, sticky = 'we', padx = 1, pady = 1, ipadx = 1, ipady = 1)
        tkgrid(frameSTNCrds, row = 1, column = 0, sticky = '', padx = 1, pady = 3, ipadx = 1, ipady = 1)

    #######################################################################################################

    #Tab5
    subfr5 <- bwTabScrollableFrame(cmd.tab5)

        ##############################################

        frameSHP <- ttklabelframe(subfr5, text = "Boundaries", relief = 'groove')

        .cdtData$EnvData$shp$add.shp <- tclVar(FALSE)
        file.plotShp <- tclVar()
        stateSHP <- "disabled"

        chk.addshp <- tkcheckbutton(frameSHP, variable = .cdtData$EnvData$shp$add.shp, text = "Add boundaries to Map", anchor = 'w', justify = 'left')
        bt.addshpOpt <- ttkbutton(frameSHP, text = "Options", state = stateSHP)
        cb.addshp <- ttkcombobox(frameSHP, values = unlist(listOpenFiles), textvariable = file.plotShp, width = largeur2, state = stateSHP)
        bt.addshp <- tkbutton(frameSHP, text = "...", state = stateSHP)

        ########
        tkconfigure(bt.addshp, command = function(){
            shp.opfiles <- getOpenShp(.cdtEnv$tcl$main$win)
            if(!is.null(shp.opfiles)){
                update.OpenFiles('shp', shp.opfiles)
                tclvalue(file.plotShp) <- shp.opfiles[[1]]
                listOpenFiles[[length(listOpenFiles) + 1]] <<- shp.opfiles[[1]]
                lapply(list(cb.en.INPrec, cb.addshp), tkconfigure, values = unlist(listOpenFiles))

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

        if(.cdtData$EnvData$output$data.type == "cdtstation"){
            stnIDTSPLOT <- .cdtData$EnvData$output$data$id
            txt.stnSel <- tklabel(frTS2, text = "Select a station to plot", anchor = 'w', justify = 'left')
            bt.stnID.prev <- ttkbutton(frTS2, text = "<<", width = 6)
            bt.stnID.next <- ttkbutton(frTS2, text = ">>", width = 6)
            cb.stnID <- ttkcombobox(frTS2, values = stnIDTSPLOT, textvariable = .cdtData$EnvData$plot.maps$stnIDTSp, width = largeur6)
            tclvalue(.cdtData$EnvData$plot.maps$stnIDTSp) <- stnIDTSPLOT[1]

            tkconfigure(bt.stnID.prev, command = function(){
                if(!is.null(.cdtData$EnvData$output)){
                    istn <- which(stnIDTSPLOT == str_trim(tclvalue(.cdtData$EnvData$plot.maps$stnIDTSp)))
                    istn <- istn - 1
                    if(istn < 1) istn <- length(stnIDTSPLOT)
                    tclvalue(.cdtData$EnvData$plot.maps$stnIDTSp) <- stnIDTSPLOT[istn]

                    imgContainer <- CDT.Display.Graph(SeasonAnalysis.plot.TSGraph, .cdtData$EnvData$tab$Tsplot, 'Time-Series-Plot')
                    .cdtData$EnvData$tab$Tsplot <- imageNotebookTab_unik(imgContainer, .cdtData$EnvData$tab$Tsplot)
                }
            })

            tkconfigure(bt.stnID.next, command = function(){
                if(!is.null(.cdtData$EnvData$output)){
                    istn <- which(stnIDTSPLOT == str_trim(tclvalue(.cdtData$EnvData$plot.maps$stnIDTSp)))
                    istn <- istn + 1
                    if(istn > length(stnIDTSPLOT)) istn <- 1
                    tclvalue(.cdtData$EnvData$plot.maps$stnIDTSp) <- stnIDTSPLOT[istn]

                    imgContainer <- CDT.Display.Graph(SeasonAnalysis.plot.TSGraph, .cdtData$EnvData$tab$Tsplot, 'Time-Series-Plot')
                    .cdtData$EnvData$tab$Tsplot <- imageNotebookTab_unik(imgContainer, .cdtData$EnvData$tab$Tsplot)
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

    #####################

    set.plot.type <- function(){
        if(.cdtData$EnvData$output$data.type == "cdtstation")
        {
            plot.type <- c("Pixels", "Points")
            .cdtData$EnvData$plot.maps$.data.type <- "Points"

            .cdtData$EnvData$climMapOp$pointSize <- 0.7
            .cdtData$EnvData$TSMapOp$pointSize <- 0.7
        }else{
            plot.type <- c("Pixels", "FilledContour")
            .cdtData$EnvData$plot.maps$.data.type <- "Grid"
        }
        tkconfigure(cb.plotType, values = plot.type)
    }

    #######################################################################################################

    load.PICSA.Data <- function(){
        range.TsMap.year <- range(as.numeric(format(.cdtData$EnvData$output$start.date, '%Y')))
        tkconfigure(.cdtData$EnvData$spin.TsMap.year, from = range.TsMap.year[1], to = range.TsMap.year[2])
        tkset(.cdtData$EnvData$spin.TsMap.year, range.TsMap.year[2])

        ###################
        ret <- read.PicsaTSData()
        if(is.null(ret)) return(NULL)

        ###################
        plotCHOIX <- c("anomaly", "bar", "line", "line.enso", "bar.enso")
        for(pp in plotCHOIX){
            .cdtData$EnvData$TSGraphOp[[pp]]$xlim$min <- range.TsMap.year[1]
            .cdtData$EnvData$TSGraphOp[[pp]]$xlim$max <- range.TsMap.year[2]
        }

        ###################
        # widgets.Station.Pixel
        widgets.Station.Pixel()
        set.plot.type()

        ###################
        # load daily precip
        if(.cdtData$EnvData$output$data.type == "cdtstation"){
            file.daily.rr <- file.path(.cdtData$EnvData$PathPicsa, "CDTDATASET", "Daily_precip.rds") 
        }else file.daily.rr <- .cdtData$EnvData$output$daily.precip

        if(!file.exists(file.daily.rr)){
            Insert.Messages.Out(paste(file.daily.rr, 'not found'), format = TRUE)
            return(NULL)
        }

        .cdtData$EnvData$daily.precip <- readRDS(file.daily.rr)
    }

    #######################################################################################################

    read.PicsaTSData <- function(){
        tkconfigure(.cdtEnv$tcl$main$win, cursor = 'watch')
        tcl('update')
        on.exit({
            tkconfigure(.cdtEnv$tcl$main$win, cursor = '')
            tcl('update')
        })

        tsdata.dir <- switch(str_trim(tclvalue(.cdtData$EnvData$varPICSA)),
                            "Onset" = "Onset_days",
                            "Cessation" = "Cessation_days",
                            "Season Length" = "Season_length",
                            "Seasonal Rainfall Amounts" = "Seasonal_rain_amount",
                            "Number of rain day" = "Number_rainy_day",
                            "Maximum daily rain" = "Maximum_rain_daily",
                            "Total rain when RR>95thPerc" = "Total_rain_above_Perc95th",
                            "Nb of day when RR>95thPerc" = "Number_day_above_Perc95th",
                            "Longest Dry Spell" = "Longest_dry_spell",
                            "Dry Spells" = "Dry_Spells")

        start.date <- format(.cdtData$EnvData$output$start.date, '%Y%m%d')
        start.dateYear <- format(.cdtData$EnvData$output$start.date, '%Y')
        idaty <- start.date[start.dateYear == str_trim(tclvalue(tkget(.cdtData$EnvData$spin.TsMap.year)))]
        dryspl <- as.numeric(str_trim(tclvalue(tkget(.cdtData$EnvData$spin.TsMap.dryspell))))

        if(.cdtData$EnvData$output$data.type == "cdtstation"){
            tsdata.path <- file.path(.cdtData$EnvData$PathPicsa, "CDTDATASET")
            filetsdata <- file.path(tsdata.path, paste0(tsdata.dir, ".rds"))

            if(!file.exists(filetsdata)){
                Insert.Messages.Out(paste(filetsdata, 'not found'), format = TRUE)
                return(NULL)
            }

            change.plot <- str_trim(tclvalue(.cdtData$EnvData$plot.maps$plot.type))

            ########
            readTsData <- TRUE
            if(!is.null(.cdtData$EnvData$tsdata))
                if(!is.null(.cdtData$EnvData$filetsdata))
                    if(.cdtData$EnvData$filetsdata == filetsdata) readTsData <- FALSE

            if(readTsData){
                .cdtData$EnvData$tsdata <- list(date = start.date,
                                            data = readRDS(filetsdata))
                .cdtData$EnvData$filetsdata <- filetsdata
            }

            ########
            rasterTsData <- TRUE
            if(!readTsData)
                if(!is.null(.cdtData$EnvData$rasterTsData))
                    if(.cdtData$EnvData$filetsdata == filetsdata)
                        if(.cdtData$EnvData$rasterTsData == idaty) rasterTsData <- FALSE

            if(tsdata.dir == "Dry_Spells")
                if(!is.null(.cdtData$EnvData$oldDryspell))
                    if(.cdtData$EnvData$oldDryspell != dryspl & !rasterTsData) rasterTsData <- TRUE

            if(!rasterTsData)
                if(.cdtData$EnvData$change.plot.rasterTsData != change.plot) rasterTsData <- TRUE

            if(rasterTsData){
                X0 <- .cdtData$EnvData$output$data$lon
                Y0 <- .cdtData$EnvData$output$data$lat
                idt <- which(.cdtData$EnvData$tsdata$date == idaty)
                if(tsdata.dir == "Dry_Spells"){
                    tmp <- .cdtData$EnvData$tsdata$data[idt, ]
                    nval <- sapply(tmp, function(x) (length(x) == 1) & is.na(x[1]))
                    tmp <- sapply(tmp, function(x) sum(!is.na(x) & x >= dryspl))
                    tmp[nval] <- NA
                    rm(nval)
                    .cdtData$EnvData$oldDryspell <- dryspl
                }else tmp <- as.numeric(.cdtData$EnvData$tsdata$data[idt, ])

                if(change.plot == "Pixels"){
                    nx <- nx_ny_as.image(diff(range(X0)))
                    ny <- nx_ny_as.image(diff(range(Y0)))
                    tmp <- cdt.as.image(tmp, nx = nx, ny = ny, pts.xy = cbind(X0, Y0))
                    .cdtData$EnvData$tsdata$x <- tmp$x
                    .cdtData$EnvData$tsdata$y <- tmp$y
                    .cdtData$EnvData$tsdata$z <- tmp$z
                }

                if(change.plot == "Points"){
                    .cdtData$EnvData$tsdata$x <- X0
                    .cdtData$EnvData$tsdata$y <- Y0
                    .cdtData$EnvData$tsdata$z <- tmp
                }

                .cdtData$EnvData$rasterTsData <- idaty
                .cdtData$EnvData$change.plot.rasterTsData <- change.plot
                rm(tmp)
            }
        }else{
            if(tsdata.dir == "Dry_Spells"){
                tsdata.path <- file.path(.cdtData$EnvData$PathPicsa, "CDTDATASET", tsdata.dir)
                tsdata.index <- file.path(.cdtData$EnvData$PathPicsa, "CDTDATASET", "CDTDATASET.rds")
                filetsdata <- tsdata.path
            }else{
                tsdata.path <- file.path(.cdtData$EnvData$PathPicsa, "DATA_NetCDF")
                filetsdata <- file.path(tsdata.path, tsdata.dir, paste0("data_", idaty, ".nc"))
            }

            if(!file.exists(filetsdata)){
                Insert.Messages.Out(paste(filetsdata, 'not found'), format = TRUE)
                return(NULL)
            }

            readTsData <- TRUE
            if(!is.null(.cdtData$EnvData$tsdata))
                if(!is.null(.cdtData$EnvData$filetsdata))
                    if(.cdtData$EnvData$filetsdata == filetsdata) readTsData <- FALSE

            if(tsdata.dir == "Dry_Spells")
                if(!is.null(.cdtData$EnvData$oldDryspell))
                    if(.cdtData$EnvData$oldDryspell != dryspl & !rasterTsData) rasterTsData <- TRUE

            if(readTsData){
                if(tsdata.dir == "Dry_Spells"){
                    cdtParallelCond <- .cdtData$Config[c('dopar', 'detect.cores', 'nb.cores')]
                    .cdtData$EnvData$tsdata <- readCdtDatasetChunk.sepdir.dates.order(tsdata.index, tsdata.path, idaty,
                                                                                      cdtParallelCond, onedate = TRUE)
                    zdim <- dim(.cdtData$EnvData$tsdata$z)
                    nval <- sapply(.cdtData$EnvData$tsdata$z, function(x) (length(x) == 1) & is.na(x[1]))
                    zval <- sapply(.cdtData$EnvData$tsdata$z, function(x) sum(!is.na(x) & x >= dryspl))
                    zval[nval] <- NA
                    dim(zval) <- zdim
                    .cdtData$EnvData$tsdata$z <- zval
                    rm(nval, zval)
                    .cdtData$EnvData$oldDryspell <- dryspl
                }else{
                    nc <- nc_open(filetsdata)
                    .cdtData$EnvData$tsdata$x <- nc$dim[[1]]$vals
                    .cdtData$EnvData$tsdata$y <- nc$dim[[2]]$vals
                    .cdtData$EnvData$tsdata$z <- ncvar_get(nc, varid = nc$var[[1]]$name)
                    nc_close(nc)
                }
                .cdtData$EnvData$filetsdata <- filetsdata
            }

            if(is.null(.cdtData$EnvData$cdtdataset)){
                tsdata.index <- file.path(.cdtData$EnvData$PathPicsa, "CDTDATASET", "CDTDATASET.rds")
                .cdtData$EnvData$cdtdataset <- readRDS(tsdata.index)
                .cdtData$EnvData$cdtdataset$fileInfo <- tsdata.index
            }
        }

        if(is.null(.cdtData$EnvData$ONI)){
            ONI <- readRDS(file.path(.cdtDir$Root, 'data', 'ONI_1950-present.rds'))
            .cdtData$EnvData$ONI$date <- format(addMonths(as.Date(paste0(ONI$ts[, 1], "-15")), 1), "%Y%m")
            .cdtData$EnvData$ONI$data <- ONI$ts[, 3]
        }

        return(0)
    }

    #######################################################################################################

    calculate.ClimStat <- function(){
        tkconfigure(.cdtEnv$tcl$main$win, cursor = 'watch')
        tcl('update')
        on.exit({
            tkconfigure(.cdtEnv$tcl$main$win, cursor = '')
            tcl('update')
        })

        varPICSA <- str_trim(tclvalue(.cdtData$EnvData$varPICSA))
        tsdata.dir <- switch(varPICSA,
                            "Onset" = "Onset_days",
                            "Cessation" = "Cessation_days",
                            "Season Length" = "Season_length",
                            "Seasonal Rainfall Amounts" = "Seasonal_rain_amount",
                            "Number of rain day" = "Number_rainy_day",
                            "Maximum daily rain" = "Maximum_rain_daily",
                            "Total rain when RR>95thPerc" = "Total_rain_above_Perc95th",
                            "Nb of day when RR>95thPerc" = "Number_day_above_Perc95th",
                            "Longest Dry Spell" = "Dry_Spells",
                            "Dry Spells" = "Dry_Spells")

        start.dateYear <- as.numeric(format(.cdtData$EnvData$output$start.date, '%Y'))
        dryspl <- as.numeric(str_trim(tclvalue(tkget(.cdtData$EnvData$spin.TsMap.dryspell))))

        if(.cdtData$EnvData$output$data.type == "cdtstation"){
            tsdata.path <- file.path(.cdtData$EnvData$PathPicsa, "CDTDATASET")
            filetsdata <- file.path(tsdata.path, paste0(tsdata.dir, ".rds"))

            if(!file.exists(filetsdata)){
                Insert.Messages.Out(paste(filetsdata, 'not found'), format = TRUE)
                return(NULL)
            }

            StatCalc <- str_trim(tclvalue(.cdtData$EnvData$analysis.method))
            change.plot <- str_trim(tclvalue(.cdtData$EnvData$plot.maps$plot.type))

            ########
            calcClim <- TRUE
            if(!is.null(.cdtData$EnvData$climdata))
                if(!is.null(.cdtData$EnvData$filetsdata1))
                    if(.cdtData$EnvData$filetsdata1 == filetsdata)
                        if(.cdtData$EnvData$StatCalc == StatCalc) calcClim <- FALSE

            trendUnit <- str_trim(tclvalue(.cdtData$EnvData$trend))
            if(StatCalc == "Trend")
                if(!is.null(.cdtData$EnvData$trendUnit))
                    if(.cdtData$EnvData$trendUnit != trendUnit & !calcClim) calcClim <- TRUE

            if(varPICSA == "Dry Spells")
                if(!is.null(.cdtData$EnvData$oldDryspell1))
                    if(.cdtData$EnvData$oldDryspell1 != dryspl & !calcClim) calcClim <- TRUE

            if(varPICSA == "Longest Dry Spell")
                if(!is.null(.cdtData$EnvData$oldDryspell2))
                    if(.cdtData$EnvData$oldDryspell2 != dryspl & !calcClim) calcClim <- TRUE

            if(!calcClim)
                if(.cdtData$EnvData$change.plot.calcClim != change.plot) calcClim <- TRUE

            if(calcClim){
                don <- readRDS(filetsdata)
                if(varPICSA == "Dry Spells"){
                    ndim <- dim(don)
                    nval <- sapply(don, function(x) (length(x) == 1) & is.na(x[1]))
                    don <- sapply(don, function(x) sum(!is.na(x) & x >= dryspl))
                    don[nval] <- NA
                    dim(don) <- ndim
                    rm(nval)
                    .cdtData$EnvData$oldDryspell1 <- dryspl
                }
                if(varPICSA == "Longest Dry Spell"){
                    ndim <- dim(don)
                    don <- sapply(don, max, na.rm = TRUE)
                    don[is.infinite(don)] <- NA
                    dim(don) <- ndim
                    .cdtData$EnvData$oldDryspell2 <- dryspl
                }

                X0 <- .cdtData$EnvData$output$data$lon
                Y0 <- .cdtData$EnvData$output$data$lat
                ## na min.frac
                don <- statisticFunction(don)

                if(change.plot == "Pixels"){
                    nx <- nx_ny_as.image(diff(range(X0)))
                    ny <- nx_ny_as.image(diff(range(Y0)))
                    don <- cdt.as.image(don, nx = nx, ny = ny, pts.xy = cbind(X0, Y0))
                    .cdtData$EnvData$climdata$x <- don$x
                    .cdtData$EnvData$climdata$y <- don$y
                    .cdtData$EnvData$climdata$z <- don$z
                }

                if(change.plot == "Points"){
                    .cdtData$EnvData$climdata$x <- X0
                    .cdtData$EnvData$climdata$y <- Y0
                    .cdtData$EnvData$climdata$z <- don
                }

                .cdtData$EnvData$filetsdata1 <- filetsdata
                .cdtData$EnvData$StatCalc <- StatCalc
                if(StatCalc == "Trend") .cdtData$EnvData$trendUnit <- trendUnit
                .cdtData$EnvData$change.plot.calcClim <- change.plot
                rm(don)
            }
        }else{
            tsdata.path <- file.path(.cdtData$EnvData$PathPicsa, "CDTDATASET", tsdata.dir)
            tsdata.index <- file.path(.cdtData$EnvData$PathPicsa, "CDTDATASET", "CDTDATASET.rds")
            filetsdata <- tsdata.path

            if(!file.exists(filetsdata)){
                Insert.Messages.Out(paste(filetsdata, 'not found'), format = TRUE)
                return(NULL)
            }

            StatCalc <- str_trim(tclvalue(.cdtData$EnvData$analysis.method))

            calcClim <- TRUE
            if(!is.null(.cdtData$EnvData$climdata))
                if(!is.null(.cdtData$EnvData$filetsdata1))
                    if(.cdtData$EnvData$filetsdata1 == filetsdata)
                        if(.cdtData$EnvData$StatCalc == StatCalc) calcClim <- FALSE

            trendUnit <- str_trim(tclvalue(.cdtData$EnvData$trend))
            if(StatCalc == "Trend")
                if(!is.null(.cdtData$EnvData$trendUnit))
                    if(.cdtData$EnvData$trendUnit != trendUnit & !calcClim) calcClim <- TRUE

            if(varPICSA == "Dry Spells")
                if(!is.null(.cdtData$EnvData$oldDryspell1))
                    if(.cdtData$EnvData$oldDryspell1 != dryspl & !calcClim) calcClim <- TRUE

            if(varPICSA == "Longest Dry Spell")
                if(!is.null(.cdtData$EnvData$oldDryspell2))
                    if(.cdtData$EnvData$oldDryspell2 != dryspl & !calcClim) calcClim <- TRUE

            if(calcClim){
                index <- readRDS(tsdata.index)

                chunkfile <- sort(unique(index$colInfo$index))
                chunkcalc <- split(chunkfile, ceiling(chunkfile / index$chunkfac))

                EnvData <- .cdtData$EnvData 
                parsL <- doparallel.cond(length(chunkcalc) > 10)
                don <- cdt.foreach(seq_along(chunkcalc), parsL, GUI = TRUE,
                                   progress = TRUE, FUN = function(jj)
                {
                    don <- lapply(chunkcalc[[jj]], function(j){
                        file.rds <- file.path(tsdata.path, paste0(j, ".rds"))
                        readRDS(file.rds)
                    })
                    don <- do.call(cbind, don)

                    if(varPICSA == "Dry Spells"){
                        ndim <- dim(don)
                        nval <- sapply(don, function(x) (length(x) == 1) & is.na(x[1]))
                        don <- sapply(don, function(x) sum(!is.na(x) & x >= dryspl))
                        don[nval] <- NA
                        dim(don) <- ndim
                        rm(nval)
                    }
                    if(varPICSA == "Longest Dry Spell"){
                        ndim <- dim(don)
                        don <- sapply(don, max, na.rm = TRUE)
                        don[is.infinite(don)] <- NA
                        dim(don) <- ndim
                    }

                    statisticFunction(don, EnvData)
                })

                don <- do.call(c, don)
                don <- don[index$colInfo$order]
                dim(don) <- sapply(index$coords$mat, length)

                .cdtData$EnvData$climdata$x <- index$coords$mat$x
                .cdtData$EnvData$climdata$y <- index$coords$mat$y
                .cdtData$EnvData$climdata$z <- don
                
                rm(don, index)
                if(varPICSA == "Dry Spells") .cdtData$EnvData$oldDryspell1 <- dryspl
                if(varPICSA == "Longest Dry Spell") .cdtData$EnvData$oldDryspell2 <- dryspl
                if(StatCalc == "Trend") .cdtData$EnvData$trendUnit <- trendUnit
                .cdtData$EnvData$filetsdata1 <- filetsdata
                .cdtData$EnvData$StatCalc <- StatCalc
            }
        }

        return(0)
    }

    #######################################################################################################

    TrendFunction <- function(Y, X){
        ncolY <- ncol(Y)
        nrowY <- nrow(Y)
        X <- if(is.matrix(X)) X else matrix(X, nrow = nrowY, ncol = ncolY)
        ina <- is.na(X) | is.na(Y)
        X[ina] <- NA
        Y[ina] <- NA
        nbY <- colSums(!is.na(Y))
        nbY[nbY < 3] <- NA

        mX <- colMeans(X, na.rm = TRUE)
        mY <- colMeans(Y, na.rm = TRUE)
        vX <- matrixStats::colVars(X, na.rm = TRUE)
        # vY <- matrixStats::colVars(Y, na.rm = TRUE)

        X1 <- X - matrix(mX, nrowY, ncolY, byrow = TRUE)
        Y1 <- Y - matrix(mY, nrowY, ncolY, byrow = TRUE)
        COV <- colSums(X1 * Y1, na.rm = TRUE) / (nbY - 1)
        alpha <- COV / vX
        return(alpha)
    }

    ###################################

    statisticFunction <- function(don, EnvData = .cdtData$EnvData){
        start.dateYear <- as.numeric(format(EnvData$output$start.date, '%Y'))

        analysis.method <- stringr::str_trim(tcltk::tclvalue(EnvData$analysis.method))

        if(analysis.method == 'Average'){
            don <- colMeans(don, na.rm = TRUE)
        }

        if(analysis.method == 'Median'){
            don <- matrixStats::colMedians(don, na.rm = TRUE)
        }

        if(analysis.method == 'Standard deviation'){
            don <- matrixStats::colSds(don, na.rm = TRUE)
        }

        if(analysis.method == 'Trend'){
            tmp <- TrendFunction(don, start.dateYear)
            trend <- stringr::str_trim(tcltk::tclvalue(EnvData$trend))
            if(trend == "Change (trend) / year") don <- tmp
            if(trend == "Change (trend) over the period"){
                don <- tmp * (diff(range(start.dateYear, na.rm = TRUE)) + 1)
            }
            if(trend == "Change (trend) / average (in %)"){
                don <- 100 * tmp * (diff(range(start.dateYear, na.rm = TRUE)) + 1) / colMeans(don, na.rm = TRUE)
            }
            rm(tmp)
        }

        if(analysis.method == 'Percentiles'){
            Q <- as.numeric(stringr::str_trim(tcltk::tclvalue(EnvData$mth.perc))) / 100
            don <- apply(don, 2, quantile8, probs = Q)
        }

        if(analysis.method == 'Frequency'){
            xlow <- stringr::str_trim(tcltk::tclvalue(EnvData$low.thres))
            xup <- stringr::str_trim(tcltk::tclvalue(EnvData$up.thres))
            if(stringr::str_trim(tcltk::tclvalue(EnvData$varPICSA)) %in% c("Onset", "Cessation")){
                dlo <- try(as.POSIXlt(as.Date(paste(start.dateYear, xlow, sep = '-'))), silent = TRUE)
                dup <- try(as.POSIXlt(as.Date(paste(start.dateYear, xup, sep = '-'))), silent = TRUE)
                if(inherits(dlo, "try-error") | inherits(dup, "try-error")){
                    if(EnvData$output$data.type == "cdtstation")
                        Insert.Messages.Out("Invalid frequency intervals", format = TRUE)
                    return(rep(NA, ncol(don)))
                }
                ix <- dlo > dup
                dup$year[ix] <- dup$year[ix] + 1
                xlow <- as.numeric(as.Date(dlo) - EnvData$output$start.date)
                xup <- as.numeric(as.Date(dup) - EnvData$output$start.date)
            }else{
                xlow <- as.numeric(xlow)
                xup <- as.numeric(xup)
                if(is.na(xlow) | is.na(xup)){
                    if(EnvData$output$data.type == "cdtstation")
                        Insert.Messages.Out("Invalid frequency intervals", format = TRUE)
                    return(rep(NA, ncol(don)))
                }
            }
            don <- colSums(don >= xlow & don <= xup, na.rm = TRUE)
        }

        return(don)
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
