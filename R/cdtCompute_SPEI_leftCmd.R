
SPEICalcPanelCmd <- function(){
    listOpenFiles <- openFile_ttkcomboList()
    if(WindowsOS()){
        largeur0 <- 29
        largeur1 <- 33
        largeur2 <- 36
        largeur3 <- 31
        largeur4 <- 10
        largeur5 <- 22
        largeur6 <- 20
        largeur7 <- 7
        largeur8 <- 19
        largeur9 <- 14
        largeur10 <- 20
        largeur11 <- 10
        largeur12 <- 24
    }else{
        largeur0 <- 30
        largeur1 <- 32
        largeur2 <- 33
        largeur3 <- 36
        largeur4 <- 10
        largeur5 <- 22
        largeur6 <- 19
        largeur7 <- 7
        largeur8 <- 18
        largeur9 <- 14
        largeur10 <- 20
        largeur11 <- 10
        largeur12 <- 22
    }

    GeneralParameters <- list(intstep = "dekadal", data.type = "cdtstation", 
                              cdtstation = list(prec = "", etp = ""),
                              cdtdataset = list(prec = "", etp = ""),
                              outfreq = "month", tscale = 3, distr = "llogistic",
                              monitoring = FALSE,
                              dates = list(year1 = 2021, mon1 = 6, dek1 = 1, year2 = 2021, mon2 = 6, dek2 = 3),
                              outdir = "")

    GeneralParameters$date.range <- list(start.year = 2021, start.mon = 6, start.dek = 1,
                                         start.pen = 1, start.day = 1,
                                         start.hour = 0, start.min = 0,
                                         end.year = 2021, end.mon = 6, end.dek = 3,
                                         end.pen = 6, end.day = 30,
                                         end.hour = 23, end.min = 55)

    .cdtData$EnvData$tab$pointSize <- NULL
    .cdtData$EnvData$dataMapOp <- list(presetCol = list(color = 'tim.colors', reverse = TRUE),
                                       userCol = list(custom = FALSE, color = NULL),
                                       userLvl = list(custom = TRUE, levels = c(-2, -1.5, -1, 0, 1, 1.5, 2), equidist = TRUE),
                                       title = list(user = FALSE, title = ''),
                                       colkeyLab = list(user = FALSE, label = ''),
                                       scalebar = list(add = FALSE, pos = 'bottomleft'),
                                       pointSize = .cdtData$EnvData$tab$pointSize)

    .cdtData$EnvData$TSGraphOp <- list(
                                    bar.line = list(
                                        xlim = list(is.min = FALSE, min = "1981-01-01", is.max = FALSE, max = "2021-12-31"),
                                        ylim = list(is.min = FALSE, min = -10, is.max = FALSE, max = 10),
                                        userYTcks = list(custom = TRUE, ticks = c(-2, -1.5, -1, 0, 1, 1.5, 2)),
                                        axislabs = list(is.xlab = FALSE, xlab = '', is.ylab = FALSE, ylab = ''),
                                        title = list(is.title = FALSE, title = '', position = 'top'),
                                        colors = list(y0 = 0, negative = "#CF661C", positive = "#157040"),
                                        line = list(plot = FALSE, col = "black", lwd = 1.5)
                                    )
                                )

    .cdtData$EnvData$spiVizOp <- list(presetCol = list(color = 'spi.colors', reverse = FALSE),
                                      userCol = list(custom = FALSE, color = NULL),
                                      userLvl = list(custom = TRUE, levels = c(-2, -1.5, -1, 0, 1, 1.5, 2), equidist = TRUE),
                                      title = list(user = FALSE, title = ''),
                                      colkeyLab = list(user = FALSE, label = ''),
                                      axislabs = list(is.xlab = FALSE, xlab = '', is.ylab = TRUE, ylab = 'Time-scale (months)'))

    .cdtData$EnvData$SHPOp <- list(col = "black", lwd = 1.5)

    ###################

    xml.dlg <- file.path(.cdtDir$dirLocal, "languages", "cdtCompute_SPEI_leftCmd.xml")
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

        #######################

        frameTimeS <- ttklabelframe(subfr1, text = lang.dlg[['label']][['1']], relief = 'groove')

        timeSteps <- tclVar()
        CbperiodVAL <- .cdtEnv$tcl$lang$global[['combobox']][['1']][3:6]
        periodVAL <- c('daily', 'pentad', 'dekadal', 'monthly')
        tclvalue(timeSteps) <- CbperiodVAL[periodVAL %in% GeneralParameters$intstep]

        cb.fperiod <- ttkcombobox(frameTimeS, values = CbperiodVAL, textvariable = timeSteps, width = largeur0)

        tkgrid(cb.fperiod, row = 0, column = 0, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)

        helpWidget(cb.fperiod, lang.dlg[['tooltip']][['1']], lang.dlg[['status']][['1']])

        ############

        tkbind(cb.fperiod, "<<ComboboxSelected>>", function(){
            outval <- OutVAL[CbOutVAL %in% str_trim(tclvalue(out.spifreq))]
            tstep <- periodVAL[CbperiodVAL %in% str_trim(tclvalue(timeSteps))]
            
            valSPIfreq <- if(tstep == 'monthly') CbOutVAL[2] else CbOutVAL
            tkconfigure(cb.SPIfreq, values = valSPIfreq)
            if(tstep == 'monthly'){
                tclvalue(out.spifreq) <- CbOutVAL[2]
                tclvalue(txt.suffix.var) <- paste0('-', CbOutVAL[2])
            }

            stateTscale <- if(outval == 'month') "normal" else "disabled"
            tkconfigure(spin.Tscale, state = stateTscale)
        })

        #######################

        frameInData <- ttklabelframe(subfr1, text = lang.dlg[['label']][['2']], relief = 'groove')

        DataType <- tclVar()
        CbdatatypeVAL <- .cdtEnv$tcl$lang$global[['combobox']][['2']][1:2]
        datatypeVAL <- c('cdtstation', 'cdtdataset')
        tclvalue(DataType) <- CbdatatypeVAL[datatypeVAL %in% GeneralParameters$data.type]

        if(GeneralParameters$data.type == 'cdtstation'){
            input.Etp <- tclVar(GeneralParameters$cdtstation$etp)
            input.Prec <- tclVar(GeneralParameters$cdtstation$prec)
            txt.INEtp <- lang.dlg[['label']][['3a']]
            txt.INPrec <- lang.dlg[['label']][['3b']]
        }else{
            input.Etp <- tclVar(GeneralParameters$cdtdataset$etp)
            input.Prec <- tclVar(GeneralParameters$cdtdataset$prec)
            txt.INEtp <- lang.dlg[['label']][['4a']]
            txt.INPrec <- lang.dlg[['label']][['4b']]
        }
        txt.INEtp.var <- tclVar(txt.INEtp)
        txt.INPrec.var <- tclVar(txt.INPrec)

        txt.datatype <- tklabel(frameInData, text = lang.dlg[['label']][['5']], anchor = 'w', justify = 'left')
        cb.datatype <- ttkcombobox(frameInData, values = CbdatatypeVAL, textvariable = DataType, width = largeur0)

        txt.prec <- tklabel(frameInData, text = tclvalue(txt.INPrec.var), textvariable = txt.INPrec.var, anchor = 'w', justify = 'left')

        if(GeneralParameters$data.type == 'cdtstation'){
            cb.en.prec <- ttkcombobox(frameInData, values = unlist(listOpenFiles), textvariable = input.Prec, width = largeur1)
        }else{
            cb.en.prec <- tkentry(frameInData, textvariable = input.Prec, width = largeur2)
        }
        bt.prec <- tkbutton(frameInData, text = "...")

        ##############
        txt.etp <- tklabel(frameInData, text = tclvalue(txt.INEtp.var), textvariable = txt.INEtp.var, anchor = 'w', justify = 'left')

        if(GeneralParameters$data.type == 'cdtstation'){
            cb.en.etp <- ttkcombobox(frameInData, values = unlist(listOpenFiles), textvariable = input.Etp, width = largeur1)
        }else{
            cb.en.etp <- tkentry(frameInData, textvariable = input.Etp, width = largeur2)
        }
        bt.etp <- tkbutton(frameInData, text = "...")

        ############

        tkgrid(txt.datatype, row = 0, column = 0, sticky = 'we', rowspan = 1, columnspan = 2, padx = 1, ipadx = 1)
        tkgrid(cb.datatype, row = 0, column = 2, sticky = 'we', rowspan = 1, columnspan = 8, padx = 1, ipadx = 1)

        tkgrid(txt.prec, row = 1, column = 0, sticky = 'we', rowspan = 1, columnspan = 10, padx = 1, ipadx = 1)
        tkgrid(cb.en.prec, row = 2, column = 0, sticky = 'we', rowspan = 1, columnspan = 9, padx = 0, ipadx = 1)
        tkgrid(bt.prec, row = 2, column = 9, sticky = 'w', rowspan = 1, columnspan = 1, padx = 0, ipadx = 1)

        tkgrid(txt.etp, row = 3, column = 0, sticky = 'we', rowspan = 1, columnspan = 10, padx = 1, ipadx = 1)
        tkgrid(cb.en.etp, row = 4, column = 0, sticky = 'we', rowspan = 1, columnspan = 9, padx = 0, ipadx = 1)
        tkgrid(bt.etp, row = 4, column = 9, sticky = 'w', rowspan = 1, columnspan = 1, padx = 0, ipadx = 1)

        ############

        helpWidget(cb.datatype, lang.dlg[['tooltip']][['2']], lang.dlg[['status']][['2']])

        if(GeneralParameters$data.type == 'cdtstation'){
            helpWidget(cb.en.etp, lang.dlg[['tooltip']][['3']], lang.dlg[['status']][['3']])
            helpWidget(cb.en.prec, lang.dlg[['tooltip']][['4']], lang.dlg[['status']][['4']])
            helpWidget(bt.etp, lang.dlg[['tooltip']][['7']], lang.dlg[['status']][['7']])
            helpWidget(bt.prec, lang.dlg[['tooltip']][['7']], lang.dlg[['status']][['7']])
        }else{
            helpWidget(cb.en.etp, lang.dlg[['tooltip']][['5']], lang.dlg[['status']][['5']])
            helpWidget(cb.en.prec, lang.dlg[['tooltip']][['6']], lang.dlg[['status']][['6']])
            helpWidget(bt.etp, lang.dlg[['tooltip']][['8']], lang.dlg[['status']][['8']])
            helpWidget(bt.prec, lang.dlg[['tooltip']][['8']], lang.dlg[['status']][['8']])
        }

        ############

        tkconfigure(bt.prec, command = function(){
            if(GeneralParameters$data.type == 'cdtstation'){
                dat.opfiles <- getOpenFiles(.cdtEnv$tcl$main$win)
                if(!is.null(dat.opfiles)){
                    update.OpenFiles('ascii', dat.opfiles)
                    listOpenFiles[[length(listOpenFiles) + 1]] <<- dat.opfiles[[1]]
                    tclvalue(input.Prec) <- dat.opfiles[[1]]
                    lapply(list(cb.en.etp, cb.en.prec), tkconfigure, values = unlist(listOpenFiles))
                }
            }else{
                path.rds <- tclvalue(tkgetOpenFile(initialdir = getwd(), filetypes = .cdtEnv$tcl$data$filetypes6))
                tclvalue(input.Prec) <- if(path.rds %in% c("", "NA")) "" else path.rds
            }
        })

        ############

        tkconfigure(bt.etp, command = function(){
            if(GeneralParameters$data.type == 'cdtstation'){
                dat.opfiles <- getOpenFiles(.cdtEnv$tcl$main$win)
                if(!is.null(dat.opfiles)){
                    update.OpenFiles('ascii', dat.opfiles)
                    listOpenFiles[[length(listOpenFiles) + 1]] <<- dat.opfiles[[1]]
                    tclvalue(input.Etp) <- dat.opfiles[[1]]
                    lapply(list(cb.en.etp, cb.en.prec), tkconfigure, values = unlist(listOpenFiles))
                }
            }else{
                path.rds <- tclvalue(tkgetOpenFile(initialdir = getwd(), filetypes = .cdtEnv$tcl$data$filetypes6))
                tclvalue(input.Etp) <- if(path.rds %in% c("", "NA")) "" else path.rds
            }
        })

        ############

        tkbind(cb.datatype, "<<ComboboxSelected>>", function(){
            tkdestroy(cb.en.etp)
            tclvalue(input.Etp) <- ''

            tkdestroy(cb.en.prec)
            tclvalue(input.Prec) <- ''

            ###
            if(str_trim(tclvalue(DataType)) == CbdatatypeVAL[1]){
                tclvalue(txt.INEtp.var) <- lang.dlg[['label']][['3a']]
                tclvalue(txt.INPrec.var) <- lang.dlg[['label']][['3b']]

                cb.en.etp <- ttkcombobox(frameInData, values = unlist(listOpenFiles), textvariable = input.Etp, width = largeur1)
                cb.en.prec <- ttkcombobox(frameInData, values = unlist(listOpenFiles), textvariable = input.Prec, width = largeur1)

                helpWidget(cb.en.etp, lang.dlg[['tooltip']][['3']], lang.dlg[['status']][['3']])
                helpWidget(cb.en.prec, lang.dlg[['tooltip']][['4']], lang.dlg[['status']][['4']])
                helpWidget(bt.etp, lang.dlg[['tooltip']][['7']], lang.dlg[['status']][['7']])
                helpWidget(bt.prec, lang.dlg[['tooltip']][['7']], lang.dlg[['status']][['7']])

                ######
                tkconfigure(bt.etp, command = function(){
                    dat.opfiles <- getOpenFiles(.cdtEnv$tcl$main$win)
                    if(!is.null(dat.opfiles)){
                        update.OpenFiles('ascii', dat.opfiles)
                        listOpenFiles[[length(listOpenFiles) + 1]] <<- dat.opfiles[[1]]
                        tclvalue(input.Etp) <- dat.opfiles[[1]]
                        lapply(list(cb.en.etp, cb.en.prec), tkconfigure, values = unlist(listOpenFiles))
                    }
                })

                tkconfigure(bt.prec, command = function(){
                    dat.opfiles <- getOpenFiles(.cdtEnv$tcl$main$win)
                    if(!is.null(dat.opfiles)){
                        update.OpenFiles('ascii', dat.opfiles)
                        listOpenFiles[[length(listOpenFiles) + 1]] <<- dat.opfiles[[1]]
                        tclvalue(input.Prec) <- dat.opfiles[[1]]
                        lapply(list(cb.en.etp, cb.en.prec), tkconfigure, values = unlist(listOpenFiles))
                    }
                })
            }

            ###
            if(str_trim(tclvalue(DataType)) == CbdatatypeVAL[2]){
                tclvalue(txt.INEtp.var) <- lang.dlg[['label']][['4a']]
                tclvalue(txt.INPrec.var) <- lang.dlg[['label']][['4b']]

                cb.en.etp <- tkentry(frameInData, textvariable = input.Etp, width = largeur2)
                cb.en.prec <- tkentry(frameInData, textvariable = input.Prec, width = largeur2)

                helpWidget(cb.en.etp, lang.dlg[['tooltip']][['5']], lang.dlg[['status']][['5']])
                helpWidget(cb.en.prec, lang.dlg[['tooltip']][['6']], lang.dlg[['status']][['6']])
                helpWidget(bt.etp, lang.dlg[['tooltip']][['8']], lang.dlg[['status']][['8']])
                helpWidget(bt.prec, lang.dlg[['tooltip']][['8']], lang.dlg[['status']][['8']])

                ######
                tkconfigure(bt.etp, command = function(){
                    path.rds <- tclvalue(tkgetOpenFile(initialdir = getwd(), filetypes = .cdtEnv$tcl$data$filetypes6))
                    tclvalue(input.Etp) <- if(path.rds %in% c("", "NA")) "" else path.rds
                })

                tkconfigure(bt.prec, command = function(){
                    path.rds <- tclvalue(tkgetOpenFile(initialdir = getwd(), filetypes = .cdtEnv$tcl$data$filetypes6))
                    tclvalue(input.Prec) <- if(path.rds %in% c("", "NA")) "" else path.rds
                })
            }

            tkgrid(cb.en.prec, row = 2, column = 0, sticky = 'we', rowspan = 1, columnspan = 9, padx = 0, ipadx = 1)
            tkgrid(cb.en.etp, row = 4, column = 0, sticky = 'we', rowspan = 1, columnspan = 9, padx = 0, ipadx = 1)
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

        chk.Moni <- tkcheckbutton(frameMoni, variable = monitoring, text = lang.dlg[['checkbutton']][['1']], anchor = 'w', justify = 'left', width = largeur3)
        bt.DateRange <- ttkbutton(frameMoni, text = lang.dlg[['button']][['1']], state = statedates)

        tkgrid(chk.Moni, row = 0, column = 0, sticky = 'we', rowspan = 1, columnspan = 2, padx = 1, ipadx = 1)
        tkgrid(bt.DateRange, row = 1, column = 1, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, ipadx = 1)

        helpWidget(chk.Moni, lang.dlg[['tooltip']][['9']], lang.dlg[['status']][['9']])

        ###############

        tkconfigure(bt.DateRange, command = function(){
            outval <- OutVAL[CbOutVAL %in% str_trim(tclvalue(out.spifreq))]
            outfreq <- switch(outval, "dekad" = "dekadal", "month" = "monthly")

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

        ###############

        tkbind(chk.Moni, "<Button-1>", function(){
            outval <- OutVAL[CbOutVAL %in% str_trim(tclvalue(out.spifreq))]

            if(tclvalue(monitoring) == "0"){
                if(tclvalue(speiDataExist) == '0'){
                    statedates <- 'normal'
                    statedatedek <<- if(outval == 'month') 'disabled' else 'normal'
                    tclvalue(txt.save.var) <- lang.dlg[['label']][['8']]

                    tkconfigure(bt.outSPI, command = function(){
                        path.rds <- tclvalue(tkgetOpenFile(initialdir = getwd(), filetypes = .cdtEnv$tcl$data$filetypes6))
                        tclvalue(outSPIdir) <- if(path.rds %in% c("", "NA") | is.na(path.rds)) "" else path.rds
                    })
                }else{
                    statedates <- 'disabled'
                    statedatedek <<- 'disabled'
                }
            }else{
                statedates <- 'disabled'
                statedatedek <<- 'disabled'
                if(tclvalue(speiDataExist) == '0'){
                    tclvalue(txt.save.var) <- lang.dlg[['label']][['9']]
                    tkconfigure(bt.outSPI, command = function(){
                        dirSPI <- tk_choose.dir(getwd(), "")
                        tclvalue(outSPIdir) <- if(dirSPI %in% c("", "NA") | is.na(dirSPI)) "" else dirSPI
                    })
                }
            }

            tkconfigure(bt.DateRange, state = statedates)
        })

        #############################

        frameParams <- tkframe(subfr1, relief = 'groove', borderwidth = 2)

        out.spifreq <- tclVar()
        CbOutVAL <- lang.dlg[['combobox']][['3']]
        OutVAL <- c("dekad", "month")
        tclvalue(out.spifreq) <- CbOutVAL[OutVAL %in% GeneralParameters$outfreq]

        if(GeneralParameters$outfreq == 'dekad'){
            txt.suffix <- paste0('-', CbOutVAL[1])
            stateTscale <- "disabled"
            up.tscale <- 1
            val.tscale <- 1
        }else{
            txt.suffix <- paste0('-', CbOutVAL[2])
            stateTscale <- "normal"
            up.tscale <- 60
            val.tscale <- GeneralParameters$tscale
        }
        txt.suffix.var <- tclVar(txt.suffix)

        frameTscale <- tkframe(frameParams)
        txt.SPIfreq <- tklabel(frameTscale, text = "SPEI", anchor = 'e', justify = 'right')
        cb.SPIfreq <- ttkcombobox(frameTscale, values = CbOutVAL, textvariable = out.spifreq, justify = 'center', width = largeur4)
        txt.Tscale1 <- tklabel(frameTscale, text = lang.dlg[['label']][['6']], anchor = 'e', justify = 'right')
        spin.Tscale <- ttkspinbox(frameTscale, from = 1, to = up.tscale, increment = 1, justify = 'center', width = 2, state = stateTscale)
        tkset(spin.Tscale, val.tscale)
        txt.Tscale2 <- tklabel(frameTscale, text = tclvalue(txt.suffix.var), textvariable = txt.suffix.var, anchor = 'w', justify = 'left')

        sepwd <- if(WindowsOS()) 0 else 1
        lab.SPIsep <- tklabel(frameTscale, text = "", width = sepwd)

        tkgrid(txt.SPIfreq, cb.SPIfreq, lab.SPIsep, txt.Tscale1, spin.Tscale, txt.Tscale2)

        helpWidget(cb.SPIfreq, lang.dlg[['tooltip']][['10']], lang.dlg[['status']][['10']])
        helpWidget(spin.Tscale, lang.dlg[['tooltip']][['11']], lang.dlg[['status']][['11']])

        ########
        tkgrid(frameTscale, row = 0, column = 0, padx = 1, pady = 1, ipadx = 1, ipady = 1)

        ########

        tkbind(cb.SPIfreq, "<<ComboboxSelected>>", function(){
            outval <- OutVAL[CbOutVAL %in% str_trim(tclvalue(out.spifreq))]

            if(outval == 'dekad'){
                stateTscale <- "disabled"
                tclvalue(txt.suffix.var) <- paste0('-', CbOutVAL[1])
                tkset(spin.Tscale, 1)
                statedatedek <<- if(tclvalue(monitoring) == "1") "normal" else "disabled"
            }
            if(outval == 'month'){
                stateTscale <- "normal"
                tclvalue(txt.suffix.var) <- paste0('-', CbOutVAL[2])
                tkconfigure(spin.Tscale, to = 60)
                statedatedek <<- "disabled"
            }

            tkconfigure(spin.Tscale, state = stateTscale)
        })

        #############################

        frameDirSav <- tkframe(subfr1, relief = 'groove', borderwidth = 2)

        outSPIdir <- tclVar(GeneralParameters$outdir)

        if(GeneralParameters$monitoring){
            text.save <- lang.dlg[['label']][['8']]
        }else{
            text.save <- lang.dlg[['label']][['9']]
        }
        txt.save.var <- tclVar(text.save)

        txt.outSPI <- tklabel(frameDirSav, text = tclvalue(txt.save.var), textvariable = txt.save.var, anchor = 'w', justify = 'left')
        en.outSPI <- tkentry(frameDirSav, textvariable = outSPIdir, width = largeur2)
        bt.outSPI <- tkbutton(frameDirSav, text = "...")

        ######

        tkgrid(txt.outSPI, row = 0, column = 0, sticky = 'we', rowspan = 1, columnspan = 5, padx = 1, pady = 0, ipadx = 1, ipady = 1)
        tkgrid(en.outSPI, row = 1, column = 0, sticky = 'we', rowspan = 1, columnspan = 4, padx = 0, pady = 0, ipadx = 1, ipady = 1)
        tkgrid(bt.outSPI, row = 1, column = 4, sticky = 'w', rowspan = 1, columnspan = 1, padx = 0, pady = 0, ipadx = 1, ipady = 1)

        helpWidget(en.outSPI, lang.dlg[['tooltip']][['12']], lang.dlg[['status']][['12']])
        helpWidget(bt.outSPI, lang.dlg[['tooltip']][['8']], lang.dlg[['status']][['8']])

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

        #############################

        calculateBut <- ttkbutton(subfr1, text = lang.dlg[['button']][['2']])

        #################

        tkconfigure(calculateBut, command = function(){
            GeneralParameters$intstep <- periodVAL[CbperiodVAL %in% str_trim(tclvalue(timeSteps))]
            GeneralParameters$data.type <- datatypeVAL[CbdatatypeVAL %in% str_trim(tclvalue(DataType))]

            if(str_trim(tclvalue(DataType)) == CbdatatypeVAL[1]){
                GeneralParameters$cdtstation$etp <- str_trim(tclvalue(input.Etp))
                GeneralParameters$cdtstation$prec <- str_trim(tclvalue(input.Prec))
            }
            if(str_trim(tclvalue(DataType)) == CbdatatypeVAL[2]){
                GeneralParameters$cdtdataset$etp <- str_trim(tclvalue(input.Etp))
                GeneralParameters$cdtdataset$prec <- str_trim(tclvalue(input.Prec))
            }

            GeneralParameters$monitoring <- switch(tclvalue(monitoring), '0' = FALSE, '1' = TRUE)

            GeneralParameters$outfreq <- OutVAL[CbOutVAL %in% str_trim(tclvalue(out.spifreq))]
            GeneralParameters$tscale <- as.numeric(str_trim(tclvalue(tkget(spin.Tscale))))

            GeneralParameters$outdir <- str_trim(tclvalue(outSPIdir))
            GeneralParameters$Indices <- "SPEI"

            # assign('GeneralParameters', GeneralParameters, envir = .GlobalEnv)

            Insert.Messages.Out(lang.dlg[['message']][['1']], TRUE, "i")

            tkconfigure(.cdtEnv$tcl$main$win, cursor = 'watch')
            tcl('update')
            ret <- tryCatch({
                                computeSPEIProcs(GeneralParameters)
                            },
                            warning = function(w) warningFun(w),
                            error = function(e) errorFun(e),
                            finally = {
                                tkconfigure(.cdtEnv$tcl$main$win, cursor = '')
                                tcl('update')
                            })

            if(!is.null(ret)){
                if(ret == 0){
                    Insert.Messages.Out(lang.dlg[['message']][['2']], TRUE, "s")

                    .cdtData$EnvData$plot.maps$data.type <- .cdtData$EnvData$output$params$data.type
                    .cdtData$EnvData$plot.maps[c('lon', 'lat', 'id')] <- .cdtData$EnvData$output$data[c('lon', 'lat', 'id')]
                    ###################

                    widgets.Station.Pixel()
                    set.plot.type()
                    ret <- try(set.Data.Scales(), silent = TRUE)
                    if(inherits(ret, "try-error") | is.null(ret)) return(NULL)

                    ret <- try(set.Data.Dates(), silent = TRUE)
                    if(inherits(ret, "try-error") | is.null(ret)) return(NULL)
                }else Insert.Messages.Out(lang.dlg[['message']][['3']], TRUE, 'e')
            }else Insert.Messages.Out(lang.dlg[['message']][['3']], TRUE, 'e')
        })

        ############################################

        tkgrid(frameTimeS, row = 0, column = 0, sticky = '', padx = 1, pady = 0, ipadx = 1, ipady = 1)
        tkgrid(frameInData, row = 1, column = 0, sticky = 'we', padx = 1, pady = 1, ipadx = 1, ipady = 0)
        tkgrid(frameMoni, row = 2, column = 0, sticky = 'we', padx = 1, pady = 0, ipadx = 1, ipady = 0)
        tkgrid(frameParams, row = 3, column = 0, sticky = 'we', padx = 1, pady = 0, ipadx = 1, ipady = 1)
        tkgrid(frameDirSav, row = 4, column = 0, sticky = 'we', padx = 1, pady = 0, ipadx = 1, ipady = 1)
        tkgrid(calculateBut, row = 5, column = 0, sticky = 'we', padx = 1, pady = 2, ipadx = 1, ipady = 1)

    #######################################################################################################

    #Tab2
    subfr2 <- bwTabScrollableFrame(cmd.tab2)

        ##############################################

        frameDataExist <- ttklabelframe(subfr2, text = lang.dlg[['label']][['10']], relief = 'groove')

        speiDataExist <- tclVar(0)
        file.dataIndex <- tclVar()

        stateExistData <- if(tclvalue(speiDataExist) == "1") "normal" else "disabled"

        chk.dataIdx <- tkcheckbutton(frameDataExist, variable = speiDataExist, text = lang.dlg[['checkbutton']][['2']], anchor = 'w', justify = 'left')
        en.dataIdx <- tkentry(frameDataExist, textvariable = file.dataIndex, width = largeur2 + 5, state = stateExistData)
        bt.dataIdx <- ttkbutton(frameDataExist, text = .cdtEnv$tcl$lang$global[['button']][['6']], state = stateExistData)

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
                    Insert.Messages.Out(lang.dlg[['message']][['4']], TRUE, 'e')
                    Insert.Messages.Out(gsub('[\r\n]', '', OutSPIdata[1]), TRUE, 'e')
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
            stateExistData <- if(tclvalue(speiDataExist) == '1') 'disabled' else 'normal'
            tkconfigure(en.dataIdx, state = stateExistData)
            tkconfigure(bt.dataIdx, state = stateExistData)

            stateCaclBut <- if(tclvalue(speiDataExist) == '1') 'normal' else 'disabled'
            tcl(tknote.cmd, 'itemconfigure', cmd.tab1$IDtab, state = stateCaclBut)
        })

        ##############################################

        frameSPIMap <- ttklabelframe(subfr2, text = lang.dlg[['label']][['11']], relief = 'groove')

        .cdtData$EnvData$spi.tscale <- tclVar()
        .cdtData$EnvData$spi.date <- tclVar()

        cb.spi.maps <- ttkcombobox(frameSPIMap, values = "", textvariable = .cdtData$EnvData$spi.tscale, justify = 'center', width = largeur5)
        bt.spi.maps <- ttkbutton(frameSPIMap, text = .cdtEnv$tcl$lang$global[['button']][['3']], width = largeur8)
        bt.spi.MapOpt <- ttkbutton(frameSPIMap, text = .cdtEnv$tcl$lang$global[['button']][['4']], width = largeur8)
        cb.spi.Date <- ttkcombobox(frameSPIMap, values = "", textvariable = .cdtData$EnvData$spi.date, justify = 'center', width = largeur6)
        bt.spi.Date.prev <- ttkbutton(frameSPIMap, text = "<<", width = largeur7)
        bt.spi.Date.next <- ttkbutton(frameSPIMap, text = ">>", width = largeur7)

        ###################

        tkgrid(cb.spi.maps, row = 0, column = 1, sticky = 'we', rowspan = 1, columnspan = 8, padx = 1, pady = 1, ipadx = 1, ipady = 1)
        tkgrid(bt.spi.MapOpt, row = 1, column = 0, sticky = 'we', rowspan = 1, columnspan = 5, ipadx = 1, ipady = 1)
        tkgrid(bt.spi.maps, row = 1, column = 5, sticky = 'we', rowspan = 1, columnspan = 5, ipadx = 1, ipady = 1)

        tkgrid(bt.spi.Date.prev, row = 2, column = 0, sticky = 'we', rowspan = 1, columnspan = 2, padx = 1, pady = 1, ipadx = 1, ipady = 1)
        tkgrid(cb.spi.Date, row = 2, column = 2, sticky = 'we', rowspan = 1, columnspan = 6, padx = 1, pady = 1, ipadx = 1, ipady = 1)
        tkgrid(bt.spi.Date.next, row = 2, column = 8, sticky = 'we', rowspan = 1, columnspan = 2, padx = 1, pady = 1, ipadx = 1, ipady = 1)

        ###############

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
            .cdtData$EnvData$plot.maps$typeTSp <- typeTSPLOT[CbtypeTSPLOT %in% str_trim(tclvalue(typeTSp))]

            if(str_trim(tclvalue(.cdtData$EnvData$spi.date)) != "" &
                !is.null(.cdtData$EnvData$varData))
            {
                get.Data.Map()

                SPICalc.Display.Maps('SPEI - Map', 'SPEI - Time Series')
            }
        })

        tkconfigure(bt.spi.Date.prev, command = function(){
            .cdtData$EnvData$plot.maps$typeTSp <- typeTSPLOT[CbtypeTSPLOT %in% str_trim(tclvalue(typeTSp))]

            if(str_trim(tclvalue(.cdtData$EnvData$spi.date)) != ""){
                donDates <- .cdtData$EnvData$varData$ts$dates
                idaty <- which(donDates == str_trim(tclvalue(.cdtData$EnvData$spi.date)))
                idaty <- idaty - 1
                if(idaty < 1) idaty <- length(donDates)
                tclvalue(.cdtData$EnvData$spi.date) <- donDates[idaty]
                get.Data.Map()

                SPICalc.Display.Maps('SPEI - Map', 'SPEI - Time Series')
            }
        })

        tkconfigure(bt.spi.Date.next, command = function(){
            .cdtData$EnvData$plot.maps$typeTSp <- typeTSPLOT[CbtypeTSPLOT %in% str_trim(tclvalue(typeTSp))]

            if(str_trim(tclvalue(.cdtData$EnvData$spi.date)) != ""){
                donDates <- .cdtData$EnvData$varData$ts$dates
                idaty <- which(donDates == str_trim(tclvalue(.cdtData$EnvData$spi.date)))
                idaty <- idaty + 1
                if(idaty > length(donDates)) idaty <- 1
                tclvalue(.cdtData$EnvData$spi.date) <- donDates[idaty]
                get.Data.Map()

                SPICalc.Display.Maps('SPEI - Map', 'SPEI - Time Series')
            }
        })

        ###############

        tkbind(cb.spi.maps, "<<ComboboxSelected>>", function(){
            ret <- try(set.Data.Dates(), silent = TRUE)
            if(inherits(ret, "try-error") | is.null(ret)) return(NULL)
        })

        ##############################################

        framePlotType <- tkframe(subfr2)

        .cdtData$EnvData$plot.maps$plot.type <- tclVar("Pixels")

        txt.plotType <- tklabel(framePlotType, text = lang.dlg[['label']][['12']], anchor = 'e', justify = 'right')
        cb.plotType <- ttkcombobox(framePlotType, values = "Pixels", textvariable = .cdtData$EnvData$plot.maps$plot.type, justify = 'center', width = largeur9)

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

        frameDataTS <- ttklabelframe(subfr3, text = lang.dlg[['label']][['13']], relief = 'groove')

        CbtypeTSPLOT <- lang.dlg[['combobox']][['2']]
        typeTSPLOT <- c("bar", "poly")
        typeTSp <- tclVar(CbtypeTSPLOT[1])

        cb.typeTSp <- ttkcombobox(frameDataTS, values = CbtypeTSPLOT, textvariable = typeTSp, justify = 'center', width = largeur10)
        bt.TsGraph.plot <- ttkbutton(frameDataTS, text = .cdtEnv$tcl$lang$global[['button']][['3']], width = largeur8)
        bt.TSGraphOpt <- ttkbutton(frameDataTS, text = .cdtEnv$tcl$lang$global[['button']][['4']], width = largeur8)

        #################

        tkgrid(cb.typeTSp, row = 0, column = 2, sticky = 'we', rowspan = 1, columnspan = 6, padx = 1, pady = 1, ipadx = 1, ipady = 1)
        tkgrid(bt.TSGraphOpt, row = 1, column = 0, sticky = 'we', rowspan = 1, columnspan = 5, padx = 1, pady = 1, ipadx = 1, ipady = 1)
        tkgrid(bt.TsGraph.plot, row = 1, column = 5, sticky = 'we', rowspan = 1, columnspan = 5, padx = 1, pady = 1, ipadx = 1, ipady = 1)

        #################

        tkconfigure(bt.TSGraphOpt, command = function(){
            .cdtData$EnvData$TSGraphOp <- MapGraph.GraphOptions.Bar.Line(.cdtData$EnvData$TSGraphOp)
        })

        #########
        .cdtData$EnvData$tab$dataGraph <- NULL

        tkconfigure(bt.TsGraph.plot, command = function(){
            .cdtData$EnvData$plot.maps$typeTSp <- typeTSPLOT[CbtypeTSPLOT %in% str_trim(tclvalue(typeTSp))]

            if(!is.null(.cdtData$EnvData$varData)){
                imgContainer <- CDT.Display.Graph(SPICalc.Plot.Graph, .cdtData$EnvData$tab$dataGraph, 'SPEI - Time Series')
                .cdtData$EnvData$tab$dataGraph <- imageNotebookTab_unik(imgContainer, .cdtData$EnvData$tab$dataGraph)
            }
        })

        ##############################################

        frameSTNCrds <- ttklabelframe(subfr3, text = lang.dlg[['label']][['14']], relief = 'groove')

        frTS2 <- tkframe(frameSTNCrds)
        .cdtData$EnvData$plot.maps$lonLOC <- tclVar()
        .cdtData$EnvData$plot.maps$latLOC <- tclVar()
        .cdtData$EnvData$plot.maps$stnIDTSp <- tclVar()

        tkgrid(frTS2, row = 0, column = 0, sticky = 'e', pady = 1)

        ##############################################

        frameVizTS <- tkframe(subfr3, relief = 'groove', borderwidth = 2)

        .cdtData$EnvData$spiViz$max.tscale <- tclVar(12)

        bt.VizTS <- ttkbutton(frameVizTS, text = lang.dlg[['button']][['4']], width = largeur12)
        bt.VizOpt <- ttkbutton(frameVizTS, text = .cdtEnv$tcl$lang$global[['button']][['4']], width = largeur9)

        frmxTscale <- tkframe(frameVizTS)
        txt.VizTS <- tklabel(frmxTscale, text = lang.dlg[['label']][['15']], anchor = 'e', justify = 'right')
        en.VizTS <- tkentry(frmxTscale, textvariable = .cdtData$EnvData$spiViz$max.tscale, width = 3)

        tkgrid(txt.VizTS, en.VizTS)

        tkgrid(bt.VizOpt, row = 0, column = 0, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, ipadx = 1, pady = 1, ipady = 1)
        tkgrid(bt.VizTS, row = 0, column = 1, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, ipadx = 1, pady = 1, ipady = 1)
        tkgrid(frmxTscale, row = 1, column = 0, sticky = '', rowspan = 1, columnspan = 2, padx = 1, ipadx = 1, pady = 1, ipady = 1)

        ###############

        tkconfigure(bt.VizOpt, command = function(){
            .cdtData$EnvData$spiVizOp <- MapGraph.SpiVizOptions(.cdtData$EnvData$spiVizOp)
        })

        ###############

        .cdtData$EnvData$notebookTab.spiViz <- NULL

        tkconfigure(bt.VizTS, command = function(){
            if(!is.null(.cdtData$EnvData$varData)){
                ret <- try(get.Data.spiViz(), silent = TRUE)
                if(inherits(ret, "try-error") | is.null(ret)) return(NULL)

                imgContainer <- CDT.Display.Graph(SPICalc.Plot.VizTS, .cdtData$EnvData$tab$spiViz, 'SPEI - Time Scales')
                .cdtData$EnvData$tab$spiViz <- imageNotebookTab_unik(imgContainer, .cdtData$EnvData$tab$spiViz)
            }
        })

        ##############################################

        tkgrid(frameDataTS, row = 0, column = 0, sticky = 'we', pady = 1)
        tkgrid(frameSTNCrds, row = 1, column = 0, sticky = '', pady = 3)
        tkgrid(frameVizTS, row = 2, column = 0, sticky = 'we', pady = 3)

    #######################################################################################################

    #Tab4
    subfr4 <- bwTabScrollableFrame(cmd.tab4)

        ##############################################

        frameSHP <- ttklabelframe(subfr4, text = lang.dlg[['label']][['16']], relief = 'groove')

        .cdtData$EnvData$shp$add.shp <- tclVar(FALSE)
        file.plotShp <- tclVar()
        stateSHP <- "disabled"

        chk.addshp <- tkcheckbutton(frameSHP, variable = .cdtData$EnvData$shp$add.shp, text = lang.dlg[['checkbutton']][['3']], anchor = 'w', justify = 'left')
        bt.addshpOpt <- ttkbutton(frameSHP, text = .cdtEnv$tcl$lang$global[['button']][['4']], state = stateSHP)
        cb.addshp <- ttkcombobox(frameSHP, values = unlist(listOpenFiles), textvariable = file.plotShp, width = largeur1, state = stateSHP)
        bt.addshp <- tkbutton(frameSHP, text = "...", state = stateSHP)

        ########
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

    widgets.Station.Pixel <- function(){
        tkdestroy(frTS2)
        frTS2 <<- tkframe(frameSTNCrds)

        if(.cdtData$EnvData$output$params$data.type == "cdtstation"){
            stnIDTSPLOT <- .cdtData$EnvData$output$data$id
            txt.stnSel <- tklabel(frTS2, text = lang.dlg[['label']][['17']])
            bt.stnID.prev <- ttkbutton(frTS2, text = "<<", width = largeur7)
            bt.stnID.next <- ttkbutton(frTS2, text = ">>", width = largeur7)
            cb.stnID <- ttkcombobox(frTS2, values = stnIDTSPLOT, textvariable = .cdtData$EnvData$plot.maps$stnIDTSp, justify = 'center', width = largeur6)
            tclvalue(.cdtData$EnvData$plot.maps$stnIDTSp) <- stnIDTSPLOT[1]

            tkgrid(txt.stnSel, row = 0, column = 0, sticky = '', rowspan = 1, columnspan = 3, padx = 1, pady = 1, ipadx = 1, ipady = 1)
            tkgrid(bt.stnID.prev, row = 1, column = 0, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
            tkgrid(cb.stnID, row = 1, column = 1, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
            tkgrid(bt.stnID.next, row = 1, column = 2, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)

            ##########
            tkconfigure(bt.stnID.prev, command = function(){
                .cdtData$EnvData$plot.maps$typeTSp <- typeTSPLOT[CbtypeTSPLOT %in% str_trim(tclvalue(typeTSp))]

                if(!is.null(.cdtData$EnvData$varData)){
                    istn <- which(stnIDTSPLOT == str_trim(tclvalue(.cdtData$EnvData$plot.maps$stnIDTSp)))
                    istn <- istn - 1
                    if(istn < 1) istn <- length(stnIDTSPLOT)
                    tclvalue(.cdtData$EnvData$plot.maps$stnIDTSp) <- stnIDTSPLOT[istn]

                    imgContainer <- CDT.Display.Graph(SPICalc.Plot.Graph, .cdtData$EnvData$tab$dataGraph, 'SPEI - Time Series')
                    .cdtData$EnvData$tab$dataGraph <- imageNotebookTab_unik(imgContainer, .cdtData$EnvData$tab$dataGraph)
                }
            })

            tkconfigure(bt.stnID.next, command = function(){
                .cdtData$EnvData$plot.maps$typeTSp <- typeTSPLOT[CbtypeTSPLOT %in% str_trim(tclvalue(typeTSp))]

                if(!is.null(.cdtData$EnvData$varData)){
                    istn <- which(stnIDTSPLOT == str_trim(tclvalue(.cdtData$EnvData$plot.maps$stnIDTSp)))
                    istn <- istn + 1
                    if(istn > length(stnIDTSPLOT)) istn <- 1
                    tclvalue(.cdtData$EnvData$plot.maps$stnIDTSp) <- stnIDTSPLOT[istn]

                    imgContainer <- CDT.Display.Graph(SPICalc.Plot.Graph, .cdtData$EnvData$tab$dataGraph, 'SPEI - Time Series')
                    .cdtData$EnvData$tab$dataGraph <- imageNotebookTab_unik(imgContainer, .cdtData$EnvData$tab$dataGraph)
                }
            })
        }else{
            txt.crdSel <- tklabel(frTS2, text = lang.dlg[['label']][['18']], anchor = 'w', justify = 'left')
            txt.lonLoc <- tklabel(frTS2, text = lang.dlg[['label']][['19']], anchor = 'e', justify = 'right')
            en.lonLoc <- tkentry(frTS2, textvariable = .cdtData$EnvData$plot.maps$lonLOC, width = largeur11)
            txt.latLoc <- tklabel(frTS2, text = lang.dlg[['label']][['20']], anchor = 'e', justify = 'right')
            en.latLoc <- tkentry(frTS2, textvariable = .cdtData$EnvData$plot.maps$latLOC, width = largeur11)
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

            .cdtData$EnvData$dataMapOp$pointSize <- 1.0
        }else{
            plot.type <- c("Pixels", "FilledContour")
            .cdtData$EnvData$plot.maps$.data.type <- "Grid"
        }
        tkconfigure(cb.plotType, values = plot.type)
    }

    #################

    set.Data.Scales <- function(){
        path.data <- file.path(.cdtData$EnvData$PathData, "CDTDATASET")
        spi.tscales <- list.files(path.data, "SPEI_.+")
        if(length(spi.tscales) == 0){
            Insert.Messages.Out(lang.dlg[['message']][['5']], TRUE, 'e')
            return(NULL)
        }
        if(.cdtData$EnvData$output$params$data.type == "cdtstation")
            spi.tscales <- tools::file_path_sans_ext(spi.tscales)

        nch <- nchar(spi.tscales)
        tsc <- str_pad(substr(spi.tscales, 6, nch - 3), 2, pad = "0")
        scales <- substr(spi.tscales, nch - 2, nch)

        spi.tscalesF <- spi.tscales[order(paste0(scales, tsc))]
        spi.tscales <- paste0("SPEI-", as.numeric(tsc), "-", ifelse(scales == "dek", "Dekad", "Month"))
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
            Insert.Messages.Out(paste(file.index, lang.dlg[['message']][['6']]), TRUE, 'e')
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
                tmp <- as.numeric(.cdtData$EnvData$cdtdataset$spi[idt, ])

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

                nc.file <- file.path(.cdtData$EnvData$PathData, "DATA_NetCDF", tscale.data, paste0("spei_", this.daty, ".nc"))
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
        file.mon.prec <- file.path(.cdtData$EnvData$PathData, "MONTHLY_precip")
        file.dek.prec <- file.path(.cdtData$EnvData$PathData, "DEKADAL_precip")
        file.mon.etp <- file.path(.cdtData$EnvData$PathData, "MONTHLY_pet")
        file.dek.etp <- file.path(.cdtData$EnvData$PathData, "DEKADAL_pet")

        if(file.exists(file.mon.prec) & file.exists(file.mon.etp)){
            file.index.prec <- file.mon.prec
            file.index.etp <- file.mon.etp
            viztstep <- "monthly"
        }else{
            if(file.exists(file.dek.prec) & file.exists(file.dek.etp)){
                file.index.prec <- file.dek.etp
                file.index.etp <- file.dek.etp
                viztstep <- "dekadal"
            }else{
                Insert.Messages.Out(lang.dlg[['message']][['7']], TRUE, 'e')
                return(NULL)
            }
        }

        readspiVizData <- TRUE
        if(!is.null(.cdtData$EnvData$spiViz))
            if(!is.null(.cdtData$EnvData$spiViz$tstep))
                if(.cdtData$EnvData$spiViz$tstep == viztstep) readspiVizData <- FALSE

        if(readspiVizData){
            file.index.prec <- file.path(file.index.prec, paste0(basename(file.index.prec), ".rds"))
            file.index.etp <- file.path(file.index.etp, paste0(basename(file.index.etp), ".rds"))
            .cdtData$EnvData$spiViz$cdtdataset$prec <- readRDS(file.index.prec)
            .cdtData$EnvData$spiViz$cdtdataset$etp <- readRDS(file.index.etp)
            .cdtData$EnvData$spiViz$cdtdataset$fileInfo.prec <- file.index.prec
            .cdtData$EnvData$spiViz$cdtdataset$fileInfo.etp <- file.index.etp
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
