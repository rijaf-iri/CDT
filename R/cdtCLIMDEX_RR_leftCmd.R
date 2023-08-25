
climdexPanelCmd.RR <- function(){
    listOpenFiles <- openFile_ttkcomboList()
    if(WindowsOS()){
        hauteur <- 370
        largeur <- 320
        largeur0 <- 29
        largeur1 <- 33
        largeur2 <- 36
        largeur3 <- 38
        largeur4 <- 32
        largeur5 <- 14
        largeur6 <- 19
        largeur7 <- 7
        largeur8 <- 20
        largeur9 <- 20
        largeur10 <- 10
    }else{
        hauteur <- 320
        largeur <- 315
        largeur0 <- 30
        largeur1 <- 32
        largeur2 <- 33
        largeur3 <- 36
        largeur4 <- 32
        largeur5 <- 14
        largeur6 <- 18
        largeur7 <- 7
        largeur8 <- 19
        largeur9 <- 20
        largeur10 <- 10
    }

    GeneralParameters <- list(data.type = "cdtstation", cdtstation = "", cdtdataset = "",
                              baseYear = list(all.years = TRUE, start.year = 1981, end.year = 2010, min.year = 15),
                              Indices = list(Rx1day = TRUE, Rx5day = TRUE, SDII = TRUE, R10mm = TRUE, R20mm = TRUE,
                                             Rnnmm = TRUE, CDD = TRUE, CWD = TRUE, R95pTOT = TRUE, R99pTOT = TRUE,
                                             PRCPTOT = TRUE, thres.Rnnmm = 25),
                              start.july = FALSE, output = "")

    .cdtData$EnvData$tab$pointSize.MapStat <- NULL
    .cdtData$EnvData$varstatMapOp <- list(presetCol = list(color = 'tim.colors', reverse = FALSE),
                                          userCol = list(custom = FALSE, color = NULL),
                                          userLvl = list(custom = FALSE, levels = NULL, equidist = FALSE),
                                          title = list(user = FALSE, title = ''),
                                          colkeyLab = list(user = FALSE, label = ''),
                                          scalebar = list(add = FALSE, pos = 'bottomleft'),
                                          pointSize = .cdtData$EnvData$tab$pointSize.MapStat)

    .cdtData$EnvData$tab$pointSize.MapTS <- NULL
    .cdtData$EnvData$dataMapOp <- list(presetCol = list(color = 'tim.colors', reverse = FALSE),
                                       userCol = list(custom = FALSE, color = NULL),
                                       userLvl = list(custom = FALSE, levels = NULL, equidist = FALSE),
                                       title = list(user = FALSE, title = ''),
                                       colkeyLab = list(user = FALSE, label = ''),
                                       scalebar = list(add = FALSE, pos = 'bottomleft'),
                                       pointSize = .cdtData$EnvData$tab$pointSize.MapTS)

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
                                                            lty= list(lowess = 2, linear = 1)
                                                        )
                                            )
                                    )

    .cdtData$EnvData$SHPOp <- list(col = "black", lwd = 1.5)

    ###################

    xml.dlg <- file.path(.cdtDir$dirLocal, "languages", "cdtCLIMDEX_RR_leftCmd.xml")
    lang.dlg <- cdtLanguageParse(xml.dlg, .cdtData$Config$lang.iso)
    .cdtData$EnvData$message <- lang.dlg[['message']]

    ###################

    .cdtEnv$tcl$main$cmd.frame <- tkframe(.cdtEnv$tcl$main$panel.left)

    tknote.cmd <- bwNoteBook(.cdtEnv$tcl$main$cmd.frame)

    cmd.tab1 <- bwAddTab(tknote.cmd, text = lang.dlg[['tab_title']][['1']])
    cmd.tab2 <- bwAddTab(tknote.cmd, text = lang.dlg[['tab_title']][['2']])
    cmd.tab3 <- bwAddTab(tknote.cmd, text = lang.dlg[['tab_title']][['3']])
    cmd.tab4 <- bwAddTab(tknote.cmd, text = lang.dlg[['tab_title']][['4']])
    cmd.tab5 <- bwAddTab(tknote.cmd, text = lang.dlg[['tab_title']][['5']])

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

        frameInData <- ttklabelframe(subfr1, text = lang.dlg[['label']][['1']], relief = 'groove')

        DataType <- tclVar()
        CbdatatypeVAL <- .cdtEnv$tcl$lang$global[['combobox']][['2']][1:2]
        datatypeVAL <- c('cdtstation', 'cdtdataset')
        tclvalue(DataType) <- CbdatatypeVAL[datatypeVAL %in% GeneralParameters$data.type]

        if(GeneralParameters$data.type == 'cdtstation'){
            input.Prec <- tclVar(GeneralParameters$cdtstation)
            txt.INPrec <- lang.dlg[['label']][['2']]
        }else{
            input.Prec <- tclVar(GeneralParameters$cdtdataset)
            txt.INPrec <- lang.dlg[['label']][['3']]
        }
        txt.INPrec.var <- tclVar(txt.INPrec)

        txt.datatype <- tklabel(frameInData, text = lang.dlg[['label']][['4']], anchor = 'w', justify = 'left')
        cb.datatype <- ttkcombobox(frameInData, values = CbdatatypeVAL, textvariable = DataType, width = largeur0)

        txt.INPrec <- tklabel(frameInData, text = tclvalue(txt.INPrec.var), textvariable = txt.INPrec.var, anchor = 'w', justify = 'left')
        if(GeneralParameters$data.type == 'cdtstation'){
            cb.en.INPrec <- ttkcombobox(frameInData, values = unlist(listOpenFiles), textvariable = input.Prec, width = largeur1)
        }else{
            cb.en.INPrec <- tkentry(frameInData, textvariable = input.Prec, width = largeur2)
        }
        bt.INPrec <- tkbutton(frameInData, text = "...")

        ############

        tkgrid(txt.datatype, row = 0, column = 0, sticky = 'we', rowspan = 1, columnspan = 2, padx = 1, pady = 1, ipadx = 1, ipady = 1)
        tkgrid(cb.datatype, row = 0, column = 2, sticky = 'we', rowspan = 1, columnspan = 8, padx = 1, pady = 1, ipadx = 1, ipady = 1)
        tkgrid(txt.INPrec, row = 1, column = 0, sticky = 'we', rowspan = 1, columnspan = 10, padx = 1, pady = 1, ipadx = 1, ipady = 1)
        tkgrid(cb.en.INPrec, row = 2, column = 0, sticky = 'we', rowspan = 1, columnspan = 9, padx = 0, pady = 1, ipadx = 1, ipady = 1)
        tkgrid(bt.INPrec, row = 2, column = 9, sticky = 'w', rowspan = 1, columnspan = 1, padx = 0, pady = 1, ipadx = 1, ipady = 1)

        ############

        helpWidget(cb.datatype, lang.dlg[['tooltip']][['1']], lang.dlg[['status']][['1']])
        if(GeneralParameters$data.type == 'cdtstation'){
            helpWidget(cb.en.INPrec, lang.dlg[['tooltip']][['2']], lang.dlg[['status']][['2']])
            helpWidget(bt.INPrec, lang.dlg[['tooltip']][['4']], lang.dlg[['status']][['4']])
        }else{
            helpWidget(cb.en.INPrec, lang.dlg[['tooltip']][['3']], lang.dlg[['status']][['3']])
            helpWidget(bt.INPrec, lang.dlg[['tooltip']][['5']], lang.dlg[['status']][['5']])
        }

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

        tkbind(cb.datatype, "<<ComboboxSelected>>", function(){
            tkdestroy(cb.en.INPrec)
            tclvalue(input.Prec) <- ''

            ###
            if(trimws(tclvalue(DataType)) == CbdatatypeVAL[1]){
                tclvalue(txt.INPrec.var) <- lang.dlg[['label']][['2']]

                cb.en.INPrec <- ttkcombobox(frameInData, values = unlist(listOpenFiles), textvariable = input.Prec, width = largeur1)

                ######
                helpWidget(cb.en.INPrec, lang.dlg[['tooltip']][['2']], lang.dlg[['status']][['2']])
                helpWidget(bt.INPrec, lang.dlg[['tooltip']][['4']], lang.dlg[['status']][['4']])

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
            }

            ###
            if(trimws(tclvalue(DataType)) == CbdatatypeVAL[2]){
                tclvalue(txt.INPrec.var) <- lang.dlg[['label']][['3']]

                cb.en.INPrec <- tkentry(frameInData, textvariable = input.Prec, width = largeur2)

                ######
                helpWidget(cb.en.INPrec, lang.dlg[['tooltip']][['3']], lang.dlg[['status']][['3']])
                helpWidget(bt.INPrec, lang.dlg[['tooltip']][['5']], lang.dlg[['status']][['5']])

                ######
                tkconfigure(bt.INPrec, command = function(){
                    path.rds <- tclvalue(tkgetOpenFile(initialdir = getwd(), filetypes = .cdtEnv$tcl$data$filetypes6))
                    tclvalue(input.Prec) <- if(path.rds %in% c("", "NA")) "" else path.rds
                })
            }

            #######
            tkgrid(cb.en.INPrec, row = 2, column = 0, sticky = 'we', rowspan = 1, columnspan = 9, padx = 0, pady = 1, ipadx = 1, ipady = 1)
        })

        ############################################

        frameDirSav <- tkframe(subfr1, relief = 'groove', borderwidth = 2)

        dir.save <- tclVar(GeneralParameters$output)

        txt.dir.save <- tklabel(frameDirSav, text = lang.dlg[['label']][['5']], anchor = 'w', justify = 'left')
        en.dir.save <- tkentry(frameDirSav, textvariable = dir.save, width = largeur2)
        bt.dir.save <- tkbutton(frameDirSav, text = "...")

        ######
        tkgrid(txt.dir.save, row = 0, column = 0, sticky = 'we', rowspan = 1, columnspan = 5, padx = 1, pady = 0, ipadx = 1, ipady = 1)
        tkgrid(en.dir.save, row = 1, column = 0, sticky = 'we', rowspan = 1, columnspan = 4, padx = 0, pady = 0, ipadx = 1, ipady = 1)
        tkgrid(bt.dir.save, row = 1, column = 4, sticky = 'w', rowspan = 1, columnspan = 1, padx = 0, pady = 0, ipadx = 1, ipady = 1)

        helpWidget(en.dir.save, lang.dlg[['tooltip']][['6']], lang.dlg[['status']][['6']])
        helpWidget(bt.dir.save, lang.dlg[['tooltip']][['5']], lang.dlg[['status']][['5']])

        ######

        tkconfigure(bt.dir.save, command = function() fileORdir2Save(dir.save, isFile = FALSE))

        ############################################

        bt.Baseyear <- ttkbutton(subfr1, text = lang.dlg[['button']][['1']])

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
    subfr2 <- bwTabScrollableFrame(cmd.tab2, hscrlwin = hauteur, wscrlwin = largeur)

    climdex.frame1 <- tkframe(cmd.tab2)
    tkgrid(climdex.frame1, sticky = 'we')

    bt.CalcIndices <- ttkbutton(cmd.tab2, text = lang.dlg[['button']][['2']])
    tkgrid(bt.CalcIndices, sticky = 'we', padx = 1, ipadx = 1)

        #######################

        frameIndex <- tkframe(subfr2, relief = 'sunken', borderwidth = 2, bg = 'lightblue')

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

        chk.Rx1day <- tkcheckbutton(frameIndex, variable = is.Rx1day, text = lang.dlg[['combobox']][['1']][1], anchor = 'w', justify = 'left')
        chk.Rx5day <- tkcheckbutton(frameIndex, variable = is.Rx5day, text = lang.dlg[['combobox']][['1']][2], anchor = 'w', justify = 'left')
        chk.SDII <- tkcheckbutton(frameIndex, variable = is.SDII, text = lang.dlg[['combobox']][['1']][3], anchor = 'w', justify = 'left')
        chk.R10mm <- tkcheckbutton(frameIndex, variable = is.R10mm, text = lang.dlg[['combobox']][['1']][4], anchor = 'w', justify = 'left')
        chk.R20mm <- tkcheckbutton(frameIndex, variable = is.R20mm, text = lang.dlg[['combobox']][['1']][5], anchor = 'w', justify = 'left')
        chk.Rnnmm <- tkcheckbutton(frameIndex, variable = is.Rnnmm, text = lang.dlg[['combobox']][['1']][6], anchor = 'w', justify = 'left')
        chk.CDD <- tkcheckbutton(frameIndex, variable = is.CDD, text = lang.dlg[['combobox']][['1']][7], anchor = 'w', justify = 'left')
        chk.CWD <- tkcheckbutton(frameIndex, variable = is.CWD, text = lang.dlg[['combobox']][['1']][8], anchor = 'w', justify = 'left')
        chk.R95pTOT <- tkcheckbutton(frameIndex, variable = is.R95pTOT, text = lang.dlg[['combobox']][['1']][9], anchor = 'w', justify = 'left')
        chk.R99pTOT <- tkcheckbutton(frameIndex, variable = is.R99pTOT, text = lang.dlg[['combobox']][['1']][10], anchor = 'w', justify = 'left')
        chk.PRCPTOT <- tkcheckbutton(frameIndex, variable = is.PRCPTOT, text = lang.dlg[['combobox']][['1']][11], anchor = 'w', justify = 'left')

        ################

        frameRnnmm <- tkframe(frameIndex)

        txt.Rnnmm <- tklabel(frameRnnmm, text = lang.dlg[['label']][['6']], anchor = 'w', justify = 'left')
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

        helpWidget(frameIndex, lang.dlg[['tooltip']][['7']], lang.dlg[['status']][['7']])

        #######################
        tkgrid(frameIndex, row = 0, column = 0, sticky = 'we', padx = 1, pady = 1, ipadx = 1, ipady = 1)
        tcl('update')

        ##############################################

        start.july <- tclVar(GeneralParameters$start.july)

        chk.StartJul <- tkcheckbutton(climdex.frame1, variable = start.july, text = lang.dlg[['checkbutton']][['1']], anchor = 'w', justify = 'left')

        tkgrid(chk.StartJul, row = 0, column = 0, sticky = 'we', padx = 1, pady = 1, ipadx = 1, ipady = 1)

        ##############################################

        tkconfigure(bt.CalcIndices, command = function(){
            GeneralParameters$data.type <- datatypeVAL[CbdatatypeVAL %in% trimws(tclvalue(DataType))]

            if(trimws(tclvalue(DataType)) == CbdatatypeVAL[1])
                GeneralParameters$cdtstation <- trimws(tclvalue(input.Prec))

            if(trimws(tclvalue(DataType)) == CbdatatypeVAL[2])
                GeneralParameters$cdtdataset <- trimws(tclvalue(input.Prec))

            GeneralParameters$output <- trimws(tclvalue(dir.save))

            GeneralParameters$Indices$Rx1day <- switch(tclvalue(is.Rx1day), '0' = FALSE, '1' = TRUE)
            GeneralParameters$Indices$Rx5day <- switch(tclvalue(is.Rx5day), '0' = FALSE, '1' = TRUE)
            GeneralParameters$Indices$SDII <- switch(tclvalue(is.SDII), '0' = FALSE, '1' = TRUE)
            GeneralParameters$Indices$R10mm <- switch(tclvalue(is.R10mm), '0' = FALSE, '1' = TRUE)
            GeneralParameters$Indices$R20mm <- switch(tclvalue(is.R20mm), '0' = FALSE, '1' = TRUE)
            GeneralParameters$Indices$Rnnmm <- switch(tclvalue(is.Rnnmm), '0' = FALSE, '1' = TRUE)
            GeneralParameters$Indices$thres.Rnnmm <- as.numeric(trimws(tclvalue(val.Rnnmm)))
            GeneralParameters$Indices$CDD <- switch(tclvalue(is.CDD), '0' = FALSE, '1' = TRUE)
            GeneralParameters$Indices$CWD <- switch(tclvalue(is.CWD), '0' = FALSE, '1' = TRUE)
            GeneralParameters$Indices$R95pTOT <- switch(tclvalue(is.R95pTOT), '0' = FALSE, '1' = TRUE)
            GeneralParameters$Indices$R99pTOT <- switch(tclvalue(is.R99pTOT), '0' = FALSE, '1' = TRUE)
            GeneralParameters$Indices$PRCPTOT <- switch(tclvalue(is.PRCPTOT), '0' = FALSE, '1' = TRUE)

            GeneralParameters$start.july <- switch(tclvalue(start.july), '0' = FALSE, '1' = TRUE)

            # assign("GeneralParameters", GeneralParameters, envir = .GlobalEnv)

            Insert.Messages.Out(lang.dlg[['message']][['1']], TRUE, "i")

            tkconfigure(.cdtEnv$tcl$main$win, cursor = 'watch')
            tcl('update')
            ret <- tryCatch({
                                climdexCalc.RR(GeneralParameters)
                            },
                            warning = function(w){
                                warningFun(w)
                                return(0)
                            },
                            error = function(e) errorFun(e),
                            finally = {
                                tkconfigure(.cdtEnv$tcl$main$win, cursor = '')
                                tcl('update')
                            }
                        )

            if(!is.null(ret)){
                if(ret == 0){
                    Insert.Messages.Out(lang.dlg[['message']][['2']], TRUE, "s")
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
                }else Insert.Messages.Out(lang.dlg[['message']][['3']], TRUE, 'e')
            }else Insert.Messages.Out(lang.dlg[['message']][['3']], TRUE, 'e')
        })

    #######################################################################################################

    #Tab3
    subfr3 <- bwTabScrollableFrame(cmd.tab3)

        #######################

        frameDataExist <- ttklabelframe(subfr3, text = lang.dlg[['label']][['7']], relief = 'groove')

        DirExist <- tclVar(0)
        file.dataIndex <- tclVar()

        stateExistData <- if(tclvalue(DirExist) == "1") "normal" else "disabled"

        chk.dataIdx <- tkcheckbutton(frameDataExist, variable = DirExist, text = lang.dlg[['checkbutton']][['2']], anchor = 'w', justify = 'left')
        en.dataIdx <- tkentry(frameDataExist, textvariable = file.dataIndex, width = largeur2 + 5, state = stateExistData)
        bt.dataIdx <- ttkbutton(frameDataExist, text = .cdtEnv$tcl$lang$global[['button']][['6']], state = stateExistData)

        tkgrid(chk.dataIdx, row = 0, column = 0, sticky = 'we', rowspan = 1, columnspan = 4, padx = 1, pady = 1, ipadx = 1, ipady = 1)
        tkgrid(bt.dataIdx, row = 0, column = 4, sticky = 'e', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
        tkgrid(en.dataIdx, row = 1, column = 0, sticky = 'we', rowspan = 1, columnspan = 5, padx = 1, pady = 1, ipadx = 1, ipady = 1)

        ###############

        tkconfigure(bt.dataIdx, command = function(){
            path.dataIdx <- tclvalue(tkgetOpenFile(initialdir = getwd(), filetypes = .cdtEnv$tcl$data$filetypes6))
            if(path.dataIdx %in% c("", "NA") | is.na(path.dataIdx)) return(NULL)
            tclvalue(file.dataIndex) <- path.dataIdx

            if(file.exists(trimws(tclvalue(file.dataIndex)))){
                OutIndexdata <- try(readRDS(trimws(tclvalue(file.dataIndex))), silent = TRUE)
                if(inherits(OutIndexdata, "try-error")){
                    Insert.Messages.Out(lang.dlg[['message']][['4']], TRUE, 'e')
                    Insert.Messages.Out(gsub('[\r\n]', '', OutIndexdata[1]), TRUE, 'e')

                    tkconfigure(cb.varstat.var, values = "")
                    tclvalue(anaVars) <- ""
                    tkconfigure(cb.varstat.stat, values = "")
                    tclvalue(anaStat) <- ""
                    tkconfigure(cb.data.Index, values = "")
                    tclvalue(donDateVar) <- ""
                    return(NULL)
                }

                .cdtData$EnvData$output <- OutIndexdata
                .cdtData$EnvData$PathData <- dirname(trimws(tclvalue(file.dataIndex)))
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

        ###############
        tkbind(chk.dataIdx, "<Button-1>", function(){
            stateExistData <- if(tclvalue(DirExist) == '1') 'disabled' else 'normal'
            tkconfigure(en.dataIdx, state = stateExistData)
            tkconfigure(bt.dataIdx, state = stateExistData)

            stateCaclBut <- if(tclvalue(DirExist) == '1') 'normal' else 'disabled'
            tcl(tknote.cmd, 'itemconfigure', cmd.tab1$IDtab, state = stateCaclBut)
            tcl(tknote.cmd, 'itemconfigure', cmd.tab2$IDtab, state = stateCaclBut)
        })

        ##############################################

        frameDataStatMap <- ttklabelframe(subfr3, text = lang.dlg[['label']][['8']], relief = 'groove')

        anaVars <- tclVar()
        CbanaVarsVAL <- lang.dlg[['combobox']][['1']]
        anaVarsVAL <- c("Rx1day", "Rx5day", "SDII", "R10mm", "R20mm", "Rnnmm",
                        "CDD", "CWD", "R95pTOT", "R99pTOT", "PRCPTOT")

        anaStat <- tclVar()
        CbanaStatVAL <- lang.dlg[['combobox']][['2']]
        anaStatVAL <- c("slope", "std.slope", "t.value.slope", "p.value.slope",
                        "intercept", "std.intercept", "t.value.intercept",
                        "p.value.intercept", "R2", "sigma")

        cb.varstat.var <- ttkcombobox(frameDataStatMap, values = "", textvariable = anaVars, justify = 'center', width = largeur3)
        bt.varstat.maps <- ttkbutton(frameDataStatMap, text = .cdtEnv$tcl$lang$global[['button']][['3']], width = largeur6)
        bt.varstat.MapOpt <- ttkbutton(frameDataStatMap, text = .cdtEnv$tcl$lang$global[['button']][['4']], width = largeur6)
        cb.varstat.stat <- ttkcombobox(frameDataStatMap, values = "", textvariable = anaStat, justify = 'center', width = largeur4)

        ###################

        tkgrid(cb.varstat.var, row = 0, column = 0, sticky = 'we', rowspan = 1, columnspan = 10, padx = 1, pady = 1, ipadx = 1, ipady = 1)
        tkgrid(bt.varstat.MapOpt, row = 1, column = 0, sticky = 'we', rowspan = 1, columnspan = 5, ipadx = 1, ipady = 1)
        tkgrid(bt.varstat.maps, row = 1, column = 5, sticky = 'we', rowspan = 1, columnspan = 5, ipadx = 1, ipady = 1)
        tkgrid(cb.varstat.stat, row = 2, column = 1, sticky = 'we', rowspan = 1, columnspan = 8, padx = 1, pady = 1, ipadx = 1, ipady = 1)

        ###################

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

            if(trimws(tclvalue(.cdtData$EnvData$plot.maps$plot.type)) == "Points")
                .cdtData$EnvData$tab$pointSize.MapStat <- .cdtData$EnvData$varstatMapOp$pointSize
        })

        ###################

        .cdtData$EnvData$tab$dataMapStat <- NULL

        tkconfigure(bt.varstat.maps, command = function(){
            .cdtData$EnvData$anaVars <- anaVarsVAL[CbanaVarsVAL %in% trimws(tclvalue(anaVars))]
            .cdtData$EnvData$anaStat <- anaStatVAL[CbanaStatVAL %in% trimws(tclvalue(anaStat))]
            .cdtData$EnvData$plot.maps$typeTSp <- typeTSPLOT[CbtypeTSPLOT %in% trimws(tclvalue(typeTSp))]

            if(length(.cdtData$EnvData$anaVars) > 0 &
               length(.cdtData$EnvData$anaStat) > 0)
            {
                ret <- try(get.data.Trend(), silent = TRUE)
                if(inherits(ret, "try-error") | is.null(ret)) return(NULL)
                Climdex.Display.MapsTrend()
            }
        })

        ###################

        tkbind(cb.varstat.var, "<<ComboboxSelected>>", function(){
            .cdtData$EnvData$anaVars <- anaVarsVAL[CbanaVarsVAL %in% trimws(tclvalue(anaVars))]
            .cdtData$EnvData$anaStat <- anaStatVAL[CbanaStatVAL %in% trimws(tclvalue(anaStat))]

            if(length(.cdtData$EnvData$anaVars) > 0 &
               length(.cdtData$EnvData$anaStat) > 0)
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

        frameDataMap <- ttklabelframe(subfr3, text = lang.dlg[['label']][['9']], relief = 'groove')

        donDateVar <- tclVar()

        cb.data.Index <- ttkcombobox(frameDataMap, values = "", textvariable = donDateVar, width = largeur8, justify = 'center')
        bt.data.Index.prev <- ttkbutton(frameDataMap, text = "<<", width = largeur7)
        bt.data.Index.next <- ttkbutton(frameDataMap, text = ">>", width = largeur7)
        bt.data.maps <- ttkbutton(frameDataMap, text = .cdtEnv$tcl$lang$global[['button']][['3']], width = largeur6)
        bt.data.MapOpt <- ttkbutton(frameDataMap, text = .cdtEnv$tcl$lang$global[['button']][['4']], width = largeur6)

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

            if(trimws(tclvalue(.cdtData$EnvData$plot.maps$plot.type)) == "Points")
                .cdtData$EnvData$tab$pointSize.MapTS <- .cdtData$EnvData$dataMapOp$pointSize
        })

        ###############

        .cdtData$EnvData$tab$dataMapTS <- NULL

        tkconfigure(bt.data.maps, command = function(){
            .cdtData$EnvData$anaVars <- anaVarsVAL[CbanaVarsVAL %in% trimws(tclvalue(anaVars))]
            .cdtData$EnvData$anaStat <- anaStatVAL[CbanaStatVAL %in% trimws(tclvalue(anaStat))]
            .cdtData$EnvData$plot.maps$typeTSp <- typeTSPLOT[CbtypeTSPLOT %in% trimws(tclvalue(typeTSp))]

            if(trimws(tclvalue(donDateVar)) != "" &
               !is.null(.cdtData$EnvData$YearData))
                    Climdex.Display.MapYear()
        })

        tkconfigure(bt.data.Index.prev, command = function(){
            .cdtData$EnvData$anaVars <- anaVarsVAL[CbanaVarsVAL %in% trimws(tclvalue(anaVars))]
            .cdtData$EnvData$anaStat <- anaStatVAL[CbanaStatVAL %in% trimws(tclvalue(anaStat))]
            .cdtData$EnvData$plot.maps$typeTSp <- typeTSPLOT[CbtypeTSPLOT %in% trimws(tclvalue(typeTSp))]

            if(trimws(tclvalue(donDateVar)) != ""){
                donDates <- .cdtData$EnvData$output$year
                idaty <- which(donDates == trimws(tclvalue(donDateVar)))
                idaty <- idaty - 1
                if(idaty < 1) idaty <- length(donDates)
                tclvalue(donDateVar) <- donDates[idaty]

                ret <- try(get.data.Year(), silent = TRUE)
                if(inherits(ret, "try-error") | is.null(ret)) return(NULL)

                Climdex.Display.MapYear()
            }
        })

        tkconfigure(bt.data.Index.next, command = function(){
            .cdtData$EnvData$anaVars <- anaVarsVAL[CbanaVarsVAL %in% trimws(tclvalue(anaVars))]
            .cdtData$EnvData$anaStat <- anaStatVAL[CbanaStatVAL %in% trimws(tclvalue(anaStat))]
            .cdtData$EnvData$plot.maps$typeTSp <- typeTSPLOT[CbtypeTSPLOT %in% trimws(tclvalue(typeTSp))]

            if(trimws(tclvalue(donDateVar)) != ""){
                donDates <- .cdtData$EnvData$output$year
                idaty <- which(donDates == trimws(tclvalue(donDateVar)))
                idaty <- idaty + 1
                if(idaty > length(donDates)) idaty <- 1
                tclvalue(donDateVar) <- donDates[idaty]

                ret <- try(get.data.Year(), silent = TRUE)
                if(inherits(ret, "try-error") | is.null(ret)) return(NULL)

                Climdex.Display.MapYear()
            }
        })

        ###############

        tkbind(cb.data.Index, "<<ComboboxSelected>>", function(){
            .cdtData$EnvData$anaVars <- anaVarsVAL[CbanaVarsVAL %in% trimws(tclvalue(anaVars))]
            .cdtData$EnvData$anaStat <- anaStatVAL[CbanaStatVAL %in% trimws(tclvalue(anaStat))]

            if(!is.null(.cdtData$EnvData$YearData)){
                ret <- try(get.data.Year(), silent = TRUE)
                if(inherits(ret, "try-error") | is.null(ret)) return(NULL)
            }
        })

        ##############################################

        framePlotType <- tkframe(subfr3)

        .cdtData$EnvData$plot.maps$plot.type <- tclVar("Pixels")

        txt.plotType <- tklabel(framePlotType, text = lang.dlg[['label']][['10']], anchor = 'e', justify = 'right')
        cb.plotType <- ttkcombobox(framePlotType, values = "Pixels", textvariable = .cdtData$EnvData$plot.maps$plot.type, justify = 'center', width = largeur5)

        tkgrid(txt.plotType, row = 0, column = 0, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
        tkgrid(cb.plotType, row = 0, column = 1, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)

        ###############

        tkbind(cb.plotType, "<<ComboboxSelected>>", function(){
            .cdtData$EnvData$anaVars <- anaVarsVAL[CbanaVarsVAL %in% trimws(tclvalue(anaVars))]
            .cdtData$EnvData$anaStat <- anaStatVAL[CbanaStatVAL %in% trimws(tclvalue(anaStat))]

            if(length(.cdtData$EnvData$anaVars) > 0 &
               length(.cdtData$EnvData$anaStat) > 0)
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

        frameDataTS <- ttklabelframe(subfr4, text = lang.dlg[['label']][['11']], relief = 'groove')

        CbtypeTSPLOT <- lang.dlg[['combobox']][['3']]
        typeTSPLOT <- c("line", "bar")
        typeTSp <- tclVar(CbtypeTSPLOT[1])

        cb.typeTSp <- ttkcombobox(frameDataTS, values = CbtypeTSPLOT, textvariable = typeTSp, justify = 'center', width = largeur9)
        bt.TsGraph.plot <- ttkbutton(frameDataTS, text = .cdtEnv$tcl$lang$global[['button']][['3']], width = largeur6)
        bt.TSGraphOpt <- ttkbutton(frameDataTS, text = .cdtEnv$tcl$lang$global[['button']][['4']], width = largeur6)

        #################

        tkgrid(cb.typeTSp, row = 0, column = 2, sticky = 'we', rowspan = 1, columnspan = 6, padx = 1, pady = 1, ipadx = 1, ipady = 1)
        tkgrid(bt.TSGraphOpt, row = 1, column = 0, sticky = 'we', rowspan = 1, columnspan = 5, padx = 1, pady = 1, ipadx = 1, ipady = 1)
        tkgrid(bt.TsGraph.plot, row = 1, column = 5, sticky = 'we', rowspan = 1, columnspan = 5, padx = 1, pady = 1, ipadx = 1, ipady = 1)

        #################

        tkconfigure(bt.TSGraphOpt, command = function(){
            vtypeTSp <- typeTSPLOT[CbtypeTSPLOT %in% trimws(tclvalue(typeTSp))]
            suffix.fun <- switch(vtypeTSp, "bar" = "Bar", "line" = "LineCLIMDEX")
            plot.fun <- get(paste0("MapGraph.GraphOptions.", suffix.fun), mode = "function")
            .cdtData$EnvData$TSGraphOp <- plot.fun(.cdtData$EnvData$TSGraphOp)
        })

        #################

        .cdtData$EnvData$tab$dataGraph <- NULL

        tkconfigure(bt.TsGraph.plot, command = function(){
            .cdtData$EnvData$plot.maps$typeTSp <- typeTSPLOT[CbtypeTSPLOT %in% trimws(tclvalue(typeTSp))]

            if(!is.null(.cdtData$EnvData$YearData)){
                imgContainer <- CDT.Display.Graph(Climdex.plotVarGraph, .cdtData$EnvData$tab$dataGraph, 'Climdex-Graph')
                .cdtData$EnvData$tab$dataGraph <- imageNotebookTab_unik(imgContainer, .cdtData$EnvData$tab$dataGraph)
            }
        })

        tkbind(cb.typeTSp, "<<ComboboxSelected>>", function(){
            .cdtData$EnvData$plot.maps$typeTSp <- typeTSPLOT[CbtypeTSPLOT %in% trimws(tclvalue(typeTSp))]
        })

        ##############################################

        frameSTNCrds <- ttklabelframe(subfr4, text = lang.dlg[['label']][['12']], relief = 'groove')

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

        frameSHP <- ttklabelframe(subfr5, text = lang.dlg[['label']][['13']], relief = 'groove')

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
            txt.stnSel <- tklabel(frTS2, text = lang.dlg[['label']][['14']])
            bt.stnID.prev <- ttkbutton(frTS2, text = "<<", width = largeur7)
            bt.stnID.next <- ttkbutton(frTS2, text = ">>", width = largeur7)
            cb.stnID <- ttkcombobox(frTS2, values = stnIDTSPLOT, textvariable = .cdtData$EnvData$plot.maps$stnIDTSp, justify = 'center', width = largeur8)
            tclvalue(.cdtData$EnvData$plot.maps$stnIDTSp) <- stnIDTSPLOT[1]

            tkgrid(txt.stnSel, row = 0, column = 0, sticky = '', rowspan = 1, columnspan = 3, padx = 1, pady = 1, ipadx = 1, ipady = 1)
            tkgrid(bt.stnID.prev, row = 1, column = 0, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
            tkgrid(cb.stnID, row = 1, column = 1, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
            tkgrid(bt.stnID.next, row = 1, column = 2, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)

            ###########
            tkconfigure(bt.stnID.prev, command = function(){
                .cdtData$EnvData$plot.maps$typeTSp <- typeTSPLOT[CbtypeTSPLOT %in% trimws(tclvalue(typeTSp))]

                if(!is.null(.cdtData$EnvData$YearData)){
                    istn <- which(stnIDTSPLOT == trimws(tclvalue(.cdtData$EnvData$plot.maps$stnIDTSp)))
                    istn <- istn - 1
                    if(istn < 1) istn <- length(stnIDTSPLOT)
                    tclvalue(.cdtData$EnvData$plot.maps$stnIDTSp) <- stnIDTSPLOT[istn]

                    imgContainer <- CDT.Display.Graph(Climdex.plotVarGraph, .cdtData$EnvData$tab$dataGraph, 'Climdex-Graph')
                    .cdtData$EnvData$tab$dataGraph <- imageNotebookTab_unik(imgContainer, .cdtData$EnvData$tab$dataGraph)
                }
            })

            tkconfigure(bt.stnID.next, command = function(){
                .cdtData$EnvData$plot.maps$typeTSp <- typeTSPLOT[CbtypeTSPLOT %in% trimws(tclvalue(typeTSp))]

                if(!is.null(.cdtData$EnvData$YearData)){
                    istn <- which(stnIDTSPLOT == trimws(tclvalue(.cdtData$EnvData$plot.maps$stnIDTSp)))
                    istn <- istn + 1
                    if(istn > length(stnIDTSPLOT)) istn <- 1
                    tclvalue(.cdtData$EnvData$plot.maps$stnIDTSp) <- stnIDTSPLOT[istn]

                    imgContainer <- CDT.Display.Graph(Climdex.plotVarGraph, .cdtData$EnvData$tab$dataGraph, 'Climdex-Graph')
                    .cdtData$EnvData$tab$dataGraph <- imageNotebookTab_unik(imgContainer, .cdtData$EnvData$tab$dataGraph)
                }
            })
        }else{
            txt.crdSel <- tklabel(frTS2, text = lang.dlg[['label']][['15']], anchor = 'w', justify = 'left')
            txt.lonLoc <- tklabel(frTS2, text = lang.dlg[['label']][['16']], anchor = 'e', justify = 'right')
            en.lonLoc <- tkentry(frTS2, textvariable = .cdtData$EnvData$plot.maps$lonLOC, width = largeur10)
            txt.latLoc <- tklabel(frTS2, text = lang.dlg[['label']][['17']], anchor = 'e', justify = 'right')
            en.latLoc <- tkentry(frTS2, textvariable = .cdtData$EnvData$plot.maps$latLOC, width = largeur10)
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

            .cdtData$EnvData$varstatMapOp$pointSize <- 1.0
            .cdtData$EnvData$dataMapOp$pointSize <- 1.0
        }else{
            plot.type <- c("Pixels", "FilledContour")
            .cdtData$EnvData$plot.maps$.data.type <- "Grid"
        }
        tkconfigure(cb.plotType, values = plot.type)
    }

    ###################

    set.trend.vars <- function(){
        tkconfigure(cb.varstat.stat, values = CbanaStatVAL)
        tclvalue(anaStat) <- CbanaStatVAL[1]

        .cdtData$EnvData$anaStat <- anaStatVAL[1]
    }

    ###################

    set.vars.dates <- function(){
        var.dir.path <- file.path(.cdtData$EnvData$PathData, 'CDTDATASET')
        indxexist <- list.files(var.dir.path)
        indices <- CbanaVarsVAL[anaVarsVAL %in% indxexist]

        if(length(indices) == 0){
            Insert.Messages.Out(lang.dlg[['message']][['5']], TRUE, 'e')
            return(NULL)
        }

        tkconfigure(cb.varstat.var, values = indices)
        tclvalue(anaVars) <- indices[1]

        .cdtData$EnvData$anaVars <- anaVarsVAL[CbanaVarsVAL %in% indices[1]]

        YEARS <- .cdtData$EnvData$output$year
        tkconfigure(cb.data.Index, values = YEARS)
        tclvalue(donDateVar) <- YEARS[length(YEARS)]
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

        this.vars <- .cdtData$EnvData$anaVars
        this.trend <- .cdtData$EnvData$anaStat
        ipos <- which(anaStatVAL %in% this.trend)

        readTrendData <- TRUE
        if(!is.null(.cdtData$EnvData$TrendData))
            if(!is.null(.cdtData$EnvData$TrendData$this.vars))
                if(.cdtData$EnvData$TrendData$this.vars == this.vars) readTrendData <- FALSE

        if(.cdtData$EnvData$output$params$data.type == "cdtstation"){
            if(readTrendData){
                filetrend <- file.path(.cdtData$EnvData$PathData, "CDTDATASET", this.vars, "Trend", paste0(this.vars, '.rds'))
                if(!file.exists(filetrend)){
                    Insert.Messages.Out(paste(filetrend, lang.dlg[['message']][['6']]), TRUE, 'e')
                    return(NULL)
                }

                .cdtData$EnvData$TrendData$data <- readRDS(filetrend)
                .cdtData$EnvData$TrendData$this.vars <- this.vars
            }

            #####
            change.plot <- trimws(tclvalue(.cdtData$EnvData$plot.maps$plot.type))

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
                    Insert.Messages.Out(paste(filetrend, lang.dlg[['message']][['6']]), TRUE, 'e')
                    return(NULL)
                }

                nc <- ncdf4::nc_open(filetrend)
                .cdtData$EnvData$TrendData$data <- lapply(anaStatVAL, function(varid) ncdf4::ncvar_get(nc, varid = varid))
                ncdf4::nc_close(nc)
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

        this.vars <- .cdtData$EnvData$anaVars
        this.year <- trimws(tclvalue(donDateVar))

        if(.cdtData$EnvData$output$params$data.type == "cdtstation"){
            fileyear <- file.path(.cdtData$EnvData$PathData, "CDTDATASET", this.vars,
                                    "Yearly", paste0(this.vars, '.rds'))

            readYearData <- TRUE
            if(!is.null(.cdtData$EnvData$YearData))
                if(!is.null(.cdtData$EnvData$YearData$fileyear))
                    if(.cdtData$EnvData$YearData$fileyear == fileyear) readYearData <- FALSE

            if(readYearData){
                if(!file.exists(fileyear)){
                    Insert.Messages.Out(paste(fileyear, lang.dlg[['message']][['6']]), TRUE, 'e')
                    return(NULL)
                }

                .cdtData$EnvData$YearData$data <- readRDS(fileyear)
                .cdtData$EnvData$YearData$fileyear <- fileyear
            }

            #####
            change.plot <- trimws(tclvalue(.cdtData$EnvData$plot.maps$plot.type))

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
                    Insert.Messages.Out(paste(fileyear, lang.dlg[['message']][['6']]), TRUE, 'e')
                    return(NULL)
                }

                nc <- ncdf4::nc_open(fileyear)
                .cdtData$EnvData$YearData$map$x <- nc$dim[[1]]$vals
                .cdtData$EnvData$YearData$map$y <- nc$dim[[2]]$vals
                .cdtData$EnvData$YearData$map$z <- ncdf4::ncvar_get(nc, varid = nc$var[[1]]$name)
                ncdf4::nc_close(nc)

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
