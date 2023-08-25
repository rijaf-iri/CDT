
CessationCalcPanelCmd <- function(){
    listOpenFiles <- openFile_ttkcomboList()
    if(WindowsOS()){
        largeur0 <- 29
        largeur1 <- 33
        largeur2 <- 36
        largeur3 <- 14
        largeur4 <- 20
        largeur5 <- 19
        largeur6 <- 20
        largeur7 <- 7
        largeur8 <- 10
        largeur9 <- 35
        largeur10 <- 4
    }else{
        largeur0 <- 30
        largeur1 <- 32
        largeur2 <- 33
        largeur3 <- 14
        largeur4 <- 20
        largeur5 <- 18
        largeur6 <- 19
        largeur7 <- 7
        largeur8 <- 10
        largeur9 <- 38
        largeur10 <- 8
    }

    MOIS <- format(ISOdate(2014, 1:12, 1), "%b")
    GeneralParameters <- list(data.type = "cdtstation", wb.data = TRUE,
                              cdtstation = list(wb = "", prec = "", etp = ""),
                              cdtdataset = list(wb = "", prec = "", etp = ""),
                              wb.pars = list(hdate = list(start.month = 1, start.day = 1, separate.year = FALSE),
                                             wb = list(wb1 = 0, multi = FALSE, file = ""),
                                             swhc = list(cap.max = 100, multi = FALSE, file = "")),
                              onset.def = list(method = 1, min.wb = 5,
                                               total.days = 3, thres.rain.day = 0.85, 
                                               accum.method = 1, accum.day = 10, evapo.frac = 0.5,
                                               earliest = list(month = 12, day = 15),
                                               latest = list(month = 2, day = 15)),
                              min.frac = 0.95,
                              onset.reg = list(region = "One", subdiv = "Latitude",
                                               lat = list(nb = 2, div = list(8)),
                                               shp = list(file = "", attr = "")),
                              output = "")

    GeneralParameters$onset.criteria[[1]]$method <- 1

    .cdtData$EnvData$tab$pointSize <- NULL
    .cdtData$EnvData$dataMapOp <- list(presetCol = list(color = 'tim.colors', reverse = FALSE),
                                       userCol = list(custom = FALSE, color = NULL),
                                       userLvl = list(custom = FALSE, levels = NULL, equidist = FALSE),
                                       title = list(user = FALSE, title = ''),
                                       colkeyLab = list(user = FALSE, label = ''),
                                       scalebar = list(add = FALSE, pos = 'bottomleft'),
                                       pointSize = .cdtData$EnvData$tab$pointSize)


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
                                    legend = NULL)
                                )

    .cdtData$EnvData$SHPOp <- list(col = "black", lwd = 1.5)

    ###################

    xml.dlg <- file.path(.cdtDir$dirLocal, "languages", "cdtCompute_Cessation_leftCmd.xml")
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

        ############################################

        frameInData <- ttklabelframe(subfr1, text = lang.dlg[['label']][['1']], relief = 'groove')

        DataType <- tclVar()
        CbdatatypeVAL <- .cdtEnv$tcl$lang$global[['combobox']][['2']][1:2]
        datatypeVAL <- c('cdtstation', 'cdtdataset')
        tclvalue(DataType) <- CbdatatypeVAL[datatypeVAL %in% GeneralParameters$data.type]

        water.balanceOK <- tclVar(GeneralParameters$wb.data)

        if(GeneralParameters$data.type == 'cdtstation'){
            input.WB <- tclVar(GeneralParameters$cdtstation$wb)
            input.Prec <- tclVar(GeneralParameters$cdtstation$prec)
            input.Etp <- tclVar(GeneralParameters$cdtstation$etp)
            txt.INWB <- lang.dlg[['label']][['3']]
            txt.INPrec <- lang.dlg[['label']][['4']]
            txt.INEtp <- lang.dlg[['label']][['4-a']]
        }else{
            input.WB <- tclVar(GeneralParameters$cdtdataset$wb)
            input.Prec <- tclVar(GeneralParameters$cdtdataset$prec)
            input.Etp <- tclVar(GeneralParameters$cdtdataset$etp)
            txt.INWB <- lang.dlg[['label']][['5']]
            txt.INPrec <- lang.dlg[['label']][['6']]
            txt.INEtp <- lang.dlg[['label']][['6-a']]
        }
        txt.INWB.var <- tclVar(txt.INWB)
        txt.INPrec.var <- tclVar(txt.INPrec)
        txt.INEtp.var <- tclVar(txt.INEtp)

        stateWB <- 'normal'
        statePrec <- 'disabled'
        stateETP <- 'disabled'
        statesetWB <- 'disabled'

        txt.datatype <- tklabel(frameInData, text = lang.dlg[['label']][['2']], anchor = 'w', justify = 'left')
        cb.datatype <- ttkcombobox(frameInData, values = CbdatatypeVAL, textvariable = DataType, width = largeur0)
        chk.WBdata <- tkcheckbutton(frameInData, variable = water.balanceOK, text = lang.dlg[['checkbutton']][['0']], anchor = 'w', justify = 'left')

        bt.setWB <- ttkbutton(frameInData, text = lang.dlg[['button']][['0']], state = statesetWB)

        txt.INWB <- tklabel(frameInData, text = tclvalue(txt.INWB.var), textvariable = txt.INWB.var, anchor = 'w', justify = 'left')
        if(GeneralParameters$data.type == 'cdtstation'){
            cb.en.INWB <- ttkcombobox(frameInData, values = unlist(listOpenFiles), textvariable = input.WB, width = largeur1, state = stateWB)
        }else{
            cb.en.INWB <- tkentry(frameInData, textvariable = input.WB, width = largeur2, state = stateWB)
        }
        bt.INWB <- tkbutton(frameInData, text = "...", state = stateWB)

        txt.INPrec <- tklabel(frameInData, text = tclvalue(txt.INPrec.var), textvariable = txt.INPrec.var, anchor = 'w', justify = 'left')
        if(GeneralParameters$data.type == 'cdtstation'){
            cb.en.INPrec <- ttkcombobox(frameInData, values = unlist(listOpenFiles), textvariable = input.Prec, width = largeur1, state = statePrec)
        }else{
            cb.en.INPrec <- tkentry(frameInData, textvariable = input.Prec, width = largeur2, state = statePrec)
        }
        bt.INPrec <- tkbutton(frameInData, text = "...", state = statePrec)

        txt.INEtp <- tklabel(frameInData, text = tclvalue(txt.INEtp.var), textvariable = txt.INEtp.var, anchor = 'w', justify = 'left')
        if(GeneralParameters$data.type == 'cdtstation'){
            cb.en.INEtp <- ttkcombobox(frameInData, values = unlist(listOpenFiles), textvariable = input.Etp, width = largeur1, state = stateETP)
        }else{
            cb.en.INEtp <- tkentry(frameInData, textvariable = input.Etp, width = largeur2, state = stateETP)
        }
        bt.INEtp <- tkbutton(frameInData, text = "...", state = stateETP)

        ############
        tkgrid(txt.datatype, row = 0, column = 0, sticky = 'we', rowspan = 1, columnspan = 2, padx = 1, pady = 1, ipadx = 1, ipady = 1)
        tkgrid(cb.datatype, row = 0, column = 2, sticky = 'we', rowspan = 1, columnspan = 8, padx = 1, pady = 1, ipadx = 1, ipady = 1)
        tkgrid(chk.WBdata, row = 1, column = 0, sticky = 'we', rowspan = 1, columnspan = 10, padx = 1, pady = 1, ipadx = 1, ipady = 1)
        tkgrid(txt.INWB, row = 2, column = 0, sticky = 'we', rowspan = 1, columnspan = 10, padx = 1, pady = 1, ipadx = 1, ipady = 1)
        tkgrid(cb.en.INWB, row = 3, column = 0, sticky = 'we', rowspan = 1, columnspan = 9, padx = 0, pady = 1, ipadx = 1, ipady = 1)
        tkgrid(bt.INWB, row = 3, column = 9, sticky = 'w', rowspan = 1, columnspan = 1, padx = 0, pady = 1, ipadx = 1, ipady = 1)

        tkgrid(bt.setWB, row = 4, column = 5, sticky = 'we', rowspan = 1, columnspan = 5, padx = 1, pady = 1, ipadx = 1, ipady = 1)

        tkgrid(txt.INPrec, row = 5, column = 0, sticky = 'we', rowspan = 1, columnspan = 10, padx = 1, pady = 1, ipadx = 1, ipady = 1)
        tkgrid(cb.en.INPrec, row = 6, column = 0, sticky = 'we', rowspan = 1, columnspan = 9, padx = 0, pady = 1, ipadx = 1, ipady = 1)
        tkgrid(bt.INPrec, row = 6, column = 9, sticky = 'w', rowspan = 1, columnspan = 1, padx = 0, pady = 1, ipadx = 1, ipady = 1)
        tkgrid(txt.INEtp, row = 7, column = 0, sticky = 'we', rowspan = 1, columnspan = 10, padx = 1, pady = 1, ipadx = 1, ipady = 1)
        tkgrid(cb.en.INEtp, row = 8, column = 0, sticky = 'we', rowspan = 1, columnspan = 9, padx = 0, pady = 1, ipadx = 1, ipady = 1)
        tkgrid(bt.INEtp, row = 8, column = 9, sticky = 'w', rowspan = 1, columnspan = 1, padx = 0, pady = 1, ipadx = 1, ipady = 1)

        ############

        helpWidget(cb.datatype, lang.dlg[['tooltip']][['1']], lang.dlg[['status']][['1']])

        if(GeneralParameters$data.type == 'cdtstation'){
            helpWidget(cb.en.INWB, lang.dlg[['tooltip']][['2']], lang.dlg[['status']][['2']])
            helpWidget(cb.en.INPrec, lang.dlg[['tooltip']][['3']], lang.dlg[['status']][['3']])
            helpWidget(cb.en.INEtp, lang.dlg[['tooltip']][['3-a']], lang.dlg[['status']][['3']])
            helpWidget(bt.INWB, lang.dlg[['tooltip']][['6']], lang.dlg[['status']][['6']])
            helpWidget(bt.INPrec, lang.dlg[['tooltip']][['6']], lang.dlg[['status']][['6']])
            helpWidget(bt.INEtp, lang.dlg[['tooltip']][['6']], lang.dlg[['status']][['6']])
        }else{
            helpWidget(cb.en.INWB, lang.dlg[['tooltip']][['4']], lang.dlg[['status']][['4']])
            helpWidget(cb.en.INPrec, lang.dlg[['tooltip']][['5']], lang.dlg[['status']][['5']])
            helpWidget(cb.en.INEtp, lang.dlg[['tooltip']][['5-a']], lang.dlg[['status']][['5']])
            helpWidget(bt.INWB, lang.dlg[['tooltip']][['7']], lang.dlg[['status']][['7']])
            helpWidget(bt.INPrec, lang.dlg[['tooltip']][['7']], lang.dlg[['status']][['7']])
            helpWidget(bt.INEtp, lang.dlg[['tooltip']][['7']], lang.dlg[['status']][['7']])
        }

        ############

        tkconfigure(bt.setWB, command = function(){
            data.type <- datatypeVAL[CbdatatypeVAL %in% trimws(tclvalue(DataType))]
            GeneralParameters[["wb.pars"]] <<- computeWB_Cessation(GeneralParameters[["wb.pars"]], data.type)
        })

        tkconfigure(bt.INWB, command = function(){
            if(GeneralParameters$data.type == 'cdtstation'){
                dat.opfiles <- getOpenFiles(.cdtEnv$tcl$main$win)
                if(!is.null(dat.opfiles)){
                    update.OpenFiles('ascii', dat.opfiles)
                    listOpenFiles[[length(listOpenFiles) + 1]] <<- dat.opfiles[[1]]
                    tclvalue(input.WB) <- dat.opfiles[[1]]
                    lapply(list(cb.en.INWB, cb.en.INPrec, cb.en.INEtp), tkconfigure, values = unlist(listOpenFiles))
                }
            }else{
                path.rds <- tclvalue(tkgetOpenFile(initialdir = getwd(), filetypes = .cdtEnv$tcl$data$filetypes6))
                tclvalue(input.WB) <- if(path.rds %in% c("", "NA") | is.na(path.rds)) "" else path.rds
            }
        })

        tkconfigure(bt.INPrec, command = function(){
            if(GeneralParameters$data.type == 'cdtstation'){
                dat.opfiles <- getOpenFiles(.cdtEnv$tcl$main$win)
                if(!is.null(dat.opfiles)){
                    update.OpenFiles('ascii', dat.opfiles)
                    listOpenFiles[[length(listOpenFiles) + 1]] <<- dat.opfiles[[1]]
                    tclvalue(input.Prec) <- dat.opfiles[[1]]
                    lapply(list(cb.en.INWB, cb.en.INPrec, cb.en.INEtp), tkconfigure, values = unlist(listOpenFiles))
                }
            }else{
                path.rds <- tclvalue(tkgetOpenFile(initialdir = getwd(), filetypes = .cdtEnv$tcl$data$filetypes6))
                tclvalue(input.Prec) <- if(path.rds %in% c("", "NA") | is.na(path.rds)) "" else path.rds
            }
        })

        tkconfigure(bt.INEtp, command = function(){
            if(GeneralParameters$data.type == 'cdtstation'){
                dat.opfiles <- getOpenFiles(.cdtEnv$tcl$main$win)
                if(!is.null(dat.opfiles)){
                    update.OpenFiles('ascii', dat.opfiles)
                    listOpenFiles[[length(listOpenFiles) + 1]] <<- dat.opfiles[[1]]
                    tclvalue(input.Etp) <- dat.opfiles[[1]]
                    lapply(list(cb.en.INWB, cb.en.INPrec, cb.en.INEtp), tkconfigure, values = unlist(listOpenFiles))
                }
            }else{
                path.rds <- tclvalue(tkgetOpenFile(initialdir = getwd(), filetypes = .cdtEnv$tcl$data$filetypes6))
                tclvalue(input.Etp) <- if(path.rds %in% c("", "NA") | is.na(path.rds)) "" else path.rds
            }
        })

        ############

        tkbind(cb.datatype, "<<ComboboxSelected>>", function(){
            tkdestroy(cb.en.INWB)
            tclvalue(input.WB) <- ''
            tkdestroy(cb.en.INPrec)
            tclvalue(input.Prec) <- ''
            tkdestroy(cb.en.INEtp)
            tclvalue(input.Etp) <- ''

            omethods <- sapply(GeneralParameters$onset.criteria, "[[", "method")
            if(all(omethods == 1)){
                if(tclvalue(water.balanceOK) == "1"){
                    stateWB <- 'normal'
                    statePrec <- 'disabled'
                    stateETP <- 'disabled'
                    statesetWB <- 'disabled'
                }else{
                    stateWB <- 'disabled'
                    statePrec <- 'normal'
                    stateETP <- 'normal'
                    statesetWB <- 'normal'
                }
            }else if(all(omethods != 1)){
                stateWB <- 'disabled'
                statePrec <- 'normal'
                stateETP <- 'normal'
                statesetWB <- 'disabled'
            }else{
                stateWB <- 'normal'
                statePrec <- 'normal'
                stateETP <- 'normal'
                statesetWB <- 'normal'
            }

            tkconfigure(bt.setWB, state = statesetWB)

            ###
            if(trimws(tclvalue(DataType)) == CbdatatypeVAL[1]){
                tclvalue(txt.INWB.var) <- lang.dlg[['label']][['3']]
                tclvalue(txt.INPrec.var) <- lang.dlg[['label']][['4']]
                tclvalue(txt.INEtp.var) <- lang.dlg[['label']][['4-a']]

                cb.en.INWB <<- ttkcombobox(frameInData, values = unlist(listOpenFiles), textvariable = input.WB, width = largeur1, state = stateWB)
                cb.en.INPrec <<- ttkcombobox(frameInData, values = unlist(listOpenFiles), textvariable = input.Prec, width = largeur1, state = statePrec)
                cb.en.INEtp <<- ttkcombobox(frameInData, values = unlist(listOpenFiles), textvariable = input.Etp, width = largeur1, state = stateETP)

                ######
                helpWidget(cb.en.INWB, lang.dlg[['tooltip']][['2']], lang.dlg[['status']][['2']])
                helpWidget(cb.en.INPrec, lang.dlg[['tooltip']][['3']], lang.dlg[['status']][['3']])
                helpWidget(cb.en.INEtp, lang.dlg[['tooltip']][['3-a']], lang.dlg[['status']][['3']])
                helpWidget(bt.INWB, lang.dlg[['tooltip']][['6']], lang.dlg[['status']][['6']])
                helpWidget(bt.INPrec, lang.dlg[['tooltip']][['6']], lang.dlg[['status']][['6']])
                helpWidget(bt.INEtp, lang.dlg[['tooltip']][['6']], lang.dlg[['status']][['6']])

                ######
                tkconfigure(bt.INWB, command = function(){
                    dat.opfiles <- getOpenFiles(.cdtEnv$tcl$main$win)
                    if(!is.null(dat.opfiles)){
                        update.OpenFiles('ascii', dat.opfiles)
                        listOpenFiles[[length(listOpenFiles) + 1]] <<- dat.opfiles[[1]]
                        tclvalue(input.WB) <- dat.opfiles[[1]]
                        lapply(list(cb.en.INWB, cb.en.INPrec, cb.en.INEtp), tkconfigure, values = unlist(listOpenFiles))
                    }
                })

                tkconfigure(bt.INPrec, state = statePrec, command = function(){
                    dat.opfiles <- getOpenFiles(.cdtEnv$tcl$main$win)
                    if(!is.null(dat.opfiles)){
                        update.OpenFiles('ascii', dat.opfiles)
                        listOpenFiles[[length(listOpenFiles) + 1]] <<- dat.opfiles[[1]]
                        tclvalue(input.Prec) <- dat.opfiles[[1]]
                        lapply(list(cb.en.INWB, cb.en.INPrec, cb.en.INEtp), tkconfigure, values = unlist(listOpenFiles))
                    }
                })

                tkconfigure(bt.INEtp, state = stateETP, command = function(){
                    dat.opfiles <- getOpenFiles(.cdtEnv$tcl$main$win)
                    if(!is.null(dat.opfiles)){
                        update.OpenFiles('ascii', dat.opfiles)
                        listOpenFiles[[length(listOpenFiles) + 1]] <<- dat.opfiles[[1]]
                        tclvalue(input.Etp) <- dat.opfiles[[1]]
                        lapply(list(cb.en.INWB, cb.en.INPrec, cb.en.INEtp), tkconfigure, values = unlist(listOpenFiles))
                    }
                })
            }

            ###
            if(trimws(tclvalue(DataType)) == CbdatatypeVAL[2]){
                tclvalue(txt.INWB.var) <- lang.dlg[['label']][['5']]
                tclvalue(txt.INPrec.var) <- lang.dlg[['label']][['6']]
                tclvalue(txt.INEtp.var) <- lang.dlg[['label']][['6-a']]

                cb.en.INWB <<- tkentry(frameInData, textvariable = input.WB, width = largeur2, state = stateWB)
                cb.en.INPrec <<- tkentry(frameInData, textvariable = input.Prec, width = largeur2, state = statePrec)
                cb.en.INEtp <<- tkentry(frameInData, textvariable = input.Etp, width = largeur2, state = stateETP)

                ######
                helpWidget(cb.en.INWB, lang.dlg[['tooltip']][['4']], lang.dlg[['status']][['4']])
                helpWidget(cb.en.INPrec, lang.dlg[['tooltip']][['5']], lang.dlg[['status']][['5']])
                helpWidget(cb.en.INEtp, lang.dlg[['tooltip']][['5-a']], lang.dlg[['status']][['5']])
                helpWidget(bt.INWB, lang.dlg[['tooltip']][['7']], lang.dlg[['status']][['7']])
                helpWidget(bt.INPrec, lang.dlg[['tooltip']][['7']], lang.dlg[['status']][['7']])
                helpWidget(bt.INEtp, lang.dlg[['tooltip']][['7']], lang.dlg[['status']][['7']])

                ######
                tkconfigure(bt.INWB, command = function(){
                    path.rds <- tclvalue(tkgetOpenFile(initialdir = getwd(), filetypes = .cdtEnv$tcl$data$filetypes6))
                    tclvalue(input.WB) <- if(path.rds %in% c("", "NA")) "" else path.rds
                })

                tkconfigure(bt.INPrec, state = statePrec, command = function(){
                    path.rds <- tclvalue(tkgetOpenFile(initialdir = getwd(), filetypes = .cdtEnv$tcl$data$filetypes6))
                    tclvalue(input.Prec) <- if(path.rds %in% c("", "NA")) "" else path.rds
                })

                tkconfigure(bt.INEtp, state = stateETP, command = function(){
                    path.rds <- tclvalue(tkgetOpenFile(initialdir = getwd(), filetypes = .cdtEnv$tcl$data$filetypes6))
                    tclvalue(input.Etp) <- if(path.rds %in% c("", "NA")) "" else path.rds
                })
            }

            #######
            tkgrid(cb.en.INWB, row = 3, column = 0, sticky = 'we', rowspan = 1, columnspan = 9, padx = 0, pady = 1, ipadx = 1, ipady = 1)
            tkgrid(cb.en.INPrec, row = 6, column = 0, sticky = 'we', rowspan = 1, columnspan = 9, padx = 0, pady = 1, ipadx = 1, ipady = 1)
            tkgrid(cb.en.INEtp, row = 8, column = 0, sticky = 'we', rowspan = 1, columnspan = 9, padx = 0, pady = 1, ipadx = 1, ipady = 1)
        })

        ############

        tkbind(chk.WBdata, "<Button-1>", function(){
            omethods <- sapply(GeneralParameters$onset.criteria, "[[", "method")
            if(all(omethods == 1)){
                if(tclvalue(water.balanceOK) == "0"){
                    stateWB <- 'normal'
                    statePrec <- 'disabled'
                    stateETP <- 'disabled'
                    statesetWB <- 'disabled'
                }else{
                    stateWB <- 'disabled'
                    statePrec <- 'normal'
                    stateETP <- 'normal'
                    statesetWB <- 'normal'
                }
            }else if(all(omethods != 1)){
                statePrec <- 'normal'
                stateETP <- 'normal'
                stateWB <- 'disabled'
                statesetWB <- 'disabled'
            }else{
                stateWB <- 'normal'
                statePrec <- 'normal'
                stateETP <- 'normal'
                statesetWB <- 'normal'
            }

            tkconfigure(bt.setWB, state = statesetWB)

            tkconfigure(cb.en.INWB, state = stateWB)
            tkconfigure(bt.INWB, state = stateWB)
            tkconfigure(cb.en.INPrec, state = statePrec)
            tkconfigure(bt.INPrec, state = statePrec)
            tkconfigure(cb.en.INEtp, state = stateETP)
            tkconfigure(bt.INEtp, state = stateETP)
        })

        ############################################

        frameDirSav <- tkframe(subfr1, relief = 'groove', borderwidth = 2)

        dir.save <- tclVar(GeneralParameters$output)

        txt.dir.save <- tklabel(frameDirSav, text = lang.dlg[['label']][['7']], anchor = 'w', justify = 'left')
        en.dir.save <- tkentry(frameDirSav, textvariable = dir.save, width = largeur2)
        bt.dir.save <- tkbutton(frameDirSav, text = "...")

        ######
        tkgrid(txt.dir.save, row = 0, column = 0, sticky = 'we', rowspan = 1, columnspan = 5, padx = 1, pady = 0, ipadx = 1, ipady = 1)
        tkgrid(en.dir.save, row = 1, column = 0, sticky = 'we', rowspan = 1, columnspan = 4, padx = 0, pady = 0, ipadx = 1, ipady = 1)
        tkgrid(bt.dir.save, row = 1, column = 4, sticky = 'w', rowspan = 1, columnspan = 1, padx = 0, pady = 0, ipadx = 1, ipady = 1)

        helpWidget(en.dir.save, lang.dlg[['tooltip']][['8']], lang.dlg[['status']][['8']])
        helpWidget(bt.dir.save, lang.dlg[['tooltip']][['7']], lang.dlg[['status']][['7']])

        ######
        tkconfigure(bt.dir.save, command = function() fileORdir2Save(dir.save, isFile = FALSE))

        ############################################

        tkgrid(frameInData, row = 0, column = 0, sticky = 'we', padx = 1, pady = 3, ipadx = 1, ipady = 1)
        tkgrid(frameDirSav, row = 1, column = 0, sticky = 'we', padx = 1, pady = 3, ipadx = 1, ipady = 1)

    #######################################################################################################

    #Tab2
    subfr2 <- bwTabScrollableFrame(cmd.tab2)

        ##############################################

        OnsetDefinitionInput <- function(){
            ########
            tkdestroy(innerRegDef)
            for(i in seq_along(ONSET.vars)) tkdestroy(ONSET.vars[[i]]$frame)
            tcl('update')
            ONSET.vars <<- NULL

            ########

            innerRegDef <<- tkframe(frameRegDef, relief = 'groove', borderwidth = 2)
            if(trimws(tclvalue(onset.region)) == CbregionDIV[2]){
                if(trimws(tclvalue(onset.subdiv)) == CbregionSUBDIV[1]){
                    txt.lat.sep <- tklabel(innerRegDef, text = '', width = largeur9)

                    ######
                    fr.lat.sub0 <- tkframe(innerRegDef)

                    txt.lat.sub <- tklabel(fr.lat.sub0, text = lang.dlg[['label']][['10']], anchor = 'w', justify = 'left')
                    cb.lat.sub <- ttkcombobox(fr.lat.sub0, values = 2:7, textvariable = lat.nbdiv, width = 3)

                    tkgrid(txt.lat.sub, row = 0, column = 0, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, ipadx = 1)
                    tkgrid(cb.lat.sub, row = 0, column = 1, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, ipadx = 1)

                    ######
                    fr.lat.sub1 <- tkframe(innerRegDef)

                    txt.lat.div <- tklabel(fr.lat.sub1, text = lang.dlg[['label']][['11']], anchor = 'w', justify = 'left')
                    en.lat.div <- NULL
                    for(i in seq_along(lat.subdiv)) en.lat.div[[i]] <- tkentry(fr.lat.sub1, textvariable = lat.subdiv[[i]], width = 6)
                    tkgrid(txt.lat.div, row = 0, column = 0, padx = 1)
                    for(i in seq_along(lat.subdiv)){
                        j <- if(i < 5) 0 else 1
                        k <- if(i < 5) i else i - 4
                        tkgrid(en.lat.div[[i]], row = j, column = k, padx = 1)
                    }

                    ######
                    tkbind(cb.lat.sub, "<<ComboboxSelected>>", function(){
                        for(i in seq_along(lat.subdiv)) tkdestroy(en.lat.div[[i]])
                        en.lat.div <<- NULL
                        lat.subdiv  <<- vector('list', length = as.numeric(trimws(tclvalue(lat.nbdiv))) - 1)
                        for(i in seq_along(lat.subdiv)) lat.subdiv[[i]] <<- tclVar()
                        for(i in seq_along(lat.subdiv)) en.lat.div[[i]] <<- tkentry(fr.lat.sub1, textvariable = lat.subdiv[[i]], width = 6)
                        for(i in seq_along(lat.subdiv)){
                            j <- if(i < 5) 0 else 1
                            k <- if(i < 5) i else i - 4
                            tkgrid(en.lat.div[[i]], row = j, column = k, padx = 1)
                        }
                    })

                    ######
                    tkgrid(txt.lat.sep, row = 0, column = 0, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, ipadx = 1)
                    tkgrid(fr.lat.sub0, row = 1, column = 0, sticky = '', rowspan = 1, columnspan = 1, padx = 1, ipadx = 1)
                    tkgrid(fr.lat.sub1, row = 2, column = 0, sticky = '', rowspan = 1, columnspan = 1, padx = 1, ipadx = 1)
                }else{
                    txt.shp.file <- tklabel(innerRegDef, text = lang.dlg[['label']][['12']], anchor = 'w', justify = 'left')
                    cb.shp.file <- ttkcombobox(innerRegDef, values = unlist(listOpenFiles), textvariable = shp.file, width = largeur1)
                    bt.shp.file <- tkbutton(innerRegDef, text = "...")

                    txt.shp.attr <- tklabel(innerRegDef, text = lang.dlg[['label']][['13']], anchor = 'w', justify = 'left')
                    cb.shp.attr <- ttkcombobox(innerRegDef, values = "", textvariable = shp.attr, width = largeur1)

                    tkgrid(txt.shp.file, row = 0, column = 0, sticky = 'we', rowspan = 1, columnspan = 6, padx = 1, ipadx = 1)
                    tkgrid(cb.shp.file, row = 1, column = 0, sticky = 'we', rowspan = 1, columnspan = 5, padx = 0, ipadx = 1)
                    tkgrid(bt.shp.file, row = 1, column = 5, sticky = 'w', rowspan = 1, columnspan = 1, padx = 0, ipadx = 1)
                    tkgrid(txt.shp.attr, row = 2, column = 0, sticky = 'we', rowspan = 1, columnspan = 6, padx = 1, ipadx = 1)
                    tkgrid(cb.shp.attr, row = 3, column = 0, sticky = 'we', rowspan = 1, columnspan = 5, padx = 0, ipadx = 1)

                    ######
                    SHPDATA <- NULL
                    tkconfigure(bt.shp.file, command = function(){
                        shp.opfiles <- getOpenShp(.cdtEnv$tcl$main$win)
                        if(!is.null(shp.opfiles)){
                            update.OpenFiles('shp', shp.opfiles)
                            tclvalue(shp.file) <- shp.opfiles[[1]]
                            listOpenFiles[[length(listOpenFiles) + 1]] <<- shp.opfiles[[1]]
                            lapply(list(cb.shp.file, cb.en.INWB, cb.en.INPrec), tkconfigure, values = unlist(listOpenFiles))
                            tcl('update')

                            shpf <- getShpOpenData(shp.file)
                            SHPDATA <<- shpf[[2]]@data
                            AttrTable <- names(SHPDATA)
                            tkconfigure(cb.shp.attr, values = AttrTable, textvariable = shp.attr)
                            tclvalue(shp.attr) <- AttrTable[1]
                            tclvalue(shp.AttrTable) <- paste0(as.character(SHPDATA[, AttrTable == tclvalue(shp.attr)]), collapse = '|')
                            tcl('update')
                        }
                    })

                    ######
                    tkbind(cb.shp.file, "<<ComboboxSelected>>", function(){
                        shpf <- getShpOpenData(shp.file)
                        SHPDATA <<- shpf[[2]]@data
                        AttrTable <- names(SHPDATA)
                        tkconfigure(cb.shp.attr, values = AttrTable, textvariable = shp.attr)
                        tclvalue(shp.attr) <- AttrTable[1]
                        tclvalue(shp.AttrTable) <- paste0(as.character(SHPDATA[, AttrTable == tclvalue(shp.attr)]), collapse = '|')
                    })

                    tkbind(cb.shp.attr, "<<ComboboxSelected>>", function(){
                        if(!is.null(SHPDATA)){
                            AttrTable <- names(SHPDATA)
                            tclvalue(shp.AttrTable) <- paste0(as.character(SHPDATA[, AttrTable == tclvalue(shp.attr)]), collapse = '|')
                        }
                    })
                }

                ######

                bt.create.subdv <- ttkbutton(innerRegDef, text = lang.dlg[['button']][['1']])

                tkgrid(bt.create.subdv, row = 4, column = 0, sticky = 'we', rowspan = 1, columnspan = 6, padx = 1, pady = 1, ipadx = 1, ipady = 1)

                ######
                tkconfigure(bt.create.subdv, command = function(){
                    if(trimws(tclvalue(onset.subdiv)) == CbregionSUBDIV[1]){
                        lat <- rep(NA, length(lat.subdiv))
                        for(i in seq_along(lat.subdiv)) lat[i] <- as.numeric(trimws(tclvalue(lat.subdiv[[i]])))
                        lat <- ifelse(lat > 60 | lat < -60, NA, lat)
                        if(any(is.na(lat))){
                            lat0 <- sapply(lat.subdiv, tclvalue)
                            Insert.Messages.Out(paste(lang.dlg[['message']][['1']], paste0(lat0[is.na(lat)], collapse = " ")), TRUE, 'e')
                            subdiv <- NULL
                        }else{
                            subdiv <- vector('character', as.numeric(trimws(tclvalue(lat.nbdiv))))
                            if(length(lat) == 1){
                                subdiv[1] <- paste('Lat <=', lat[[1]])
                                subdiv[2] <- paste('Lat >', lat[[1]])
                            }else{
                                subdiv[1] <- paste('Lat <=', lat[[1]])
                                for(i in 1:(length(lat) - 1)) subdiv[i + 1] <- paste(lat[[i + 1]], '>= Lat >', lat[[i]])
                                subdiv[length(subdiv)] <- paste('Lat >', lat[[length(lat)]])
                            }
                        }
                    }else{
                        subdiv <- strsplit(tclvalue(shp.AttrTable), '\\|')[[1]]
                        subdiv <- substr(subdiv, 1, 20)
                        subdiv <- if(length(subdiv) > 0) subdiv else NULL
                    }
                    if(!is.null(subdiv)){
                        tcl("update", "idletasks")
                        for(i in seq_along(ONSET.vars)) tkdestroy(ONSET.vars[[i]]$frame)
                        ONSET.vars <<- NULL
                        tcl("update", "idletasks")
                        for(i in seq_along(subdiv))
                            ONSET.vars[[i]] <<- OnsetDefinitionCriteria(frameOnsetDEF, GeneralParameters$onset.def, subdiv = subdiv[i])
                    }
                })
            }else{
                tcl("update", "idletasks")
                ONSET.vars[[1]] <<- OnsetDefinitionCriteria(frameOnsetDEF, GeneralParameters$onset.def)
            }

            ######
            tkgrid(innerRegDef)
        }

        OnsetDefinitionCriteria <- function(containerFrame, Parameters, subdiv = NA){
            labelReg <- if(is.na(subdiv)) lang.dlg[['label']][['14']] else paste(lang.dlg[['label']][['14']], "- [", subdiv, "]")

            frameOnset <- ttklabelframe(containerFrame, text = labelReg, relief = 'sunken')

            #########
            onset.method <- tclVar(Parameters$method)

            mon1 <- as.numeric(trimws(Parameters$earliest$month))
            onset.start.mon <- tclVar(MOIS[mon1])
            onset.start.day <- tclVar(Parameters$earliest$day)

            min.wb <- tclVar(Parameters$min.wb)
            total.days <- tclVar(Parameters$total.days)
            thres.rain.day <- tclVar(Parameters$thres.rain.day)

            accum.method <- tclVar(Parameters$accum.method)
            accum.day <- tclVar(Parameters$accum.day)
            evapo.frac <- tclVar(Parameters$evapo.frac)

            mon2 <- as.numeric(trimws(Parameters$latest$month))
            onset.late.mon <- tclVar(MOIS[mon2])
            onset.late.day <- tclVar(Parameters$latest$day)

            ##########

            frMethod <- tkframe(frameOnset)
            txt.method <- tklabel(frMethod, text = lang.dlg[['label']][['15']], anchor = 'w', justify = 'left')
            cb.method <- ttkcombobox(frMethod, values = 1:2, textvariable = onset.method, width = 3, justify = 'center')
            bt.method <- ttkbutton(frMethod, text = lang.dlg[['button']][['2']])

            txt.sepMthd <- tklabel(frMethod, text = "", width = largeur10)

            tkgrid(txt.method, cb.method, txt.sepMthd, bt.method)
            tkgrid.configure(bt.method, sticky = 'e', pady = 3)

            ##########

            tkconfigure(bt.method, command = function(){
                Insert.Messages.Out(lang.dlg[['message']][['2']])
                Insert.Messages.Out(lang.dlg[['message']][['3']])
            })

            frEarliest <- tkframe(frameOnset)
            txt.early1 <- tklabel(frEarliest, text = lang.dlg[['label']][['16']], anchor = 'w', justify = 'left')
            cb.early1 <- ttkcombobox(frEarliest, values = MOIS, textvariable = onset.start.mon, width = 5)
            txt.early2 <- tklabel(frEarliest, text = lang.dlg[['label']][['17']], anchor = 'w', justify = 'left')
            cb.early2 <- ttkcombobox(frEarliest, values = 1:31, textvariable = onset.start.day, width = 4)
            tkgrid(txt.early1, cb.early1, txt.early2, cb.early2)

            # mthd1
            frMinWB <- tkframe(frameOnset)
            txt.minwb1 <- tklabel(frMinWB, text = lang.dlg[['label']][['18']], anchor = 'w', justify = 'left')
            en.minwb <- tkentry(frMinWB, textvariable = min.wb, width = 4)
            txt.minwb2 <- tklabel(frMinWB, text = "mm", anchor = 'w', justify = 'left')
            tkgrid(txt.minwb1, en.minwb, txt.minwb2)

            frWinDay <- tkframe(frameOnset)
            txt.winday1 <- tklabel(frWinDay, text = lang.dlg[['label']][['19']], anchor = 'w', justify = 'left')
            en.winday <- tkentry(frWinDay, textvariable = total.days, width = 4)
            txt.winday2 <- tklabel(frWinDay, text = lang.dlg[['label']][['20']], anchor = 'w', justify = 'left')
            tkgrid(txt.winday1, en.winday, txt.winday2)

            # mthd 2
            frAccMTH <- tkframe(frameOnset)
            txt.AccMTH <- tklabel(frAccMTH, text = lang.dlg[['label']][['22-a']], anchor = 'w', justify = 'left')
            cb.AccMTH <- ttkcombobox(frAccMTH, values = 1:2, textvariable = accum.method, width = 2)
            tkgrid(txt.AccMTH, cb.AccMTH)

            frAccRR <- tkframe(frameOnset)
            txt.AccRR1 <- tklabel(frAccRR, text = lang.dlg[['label']][['22']], anchor = 'w', justify = 'left')
            en.AccRR <- tkentry(frAccRR, textvariable = accum.day, width = 4)
            txt.AccRR2 <- tklabel(frAccRR, text = paste(lang.dlg[['label']][['20']], lang.dlg[['label']][['23']]), anchor = 'w', justify = 'left')
            tkgrid(txt.AccRR1, en.AccRR, txt.AccRR2)

            frEvapo <- tkframe(frameOnset)
            txt.evapo1 <- tklabel(frEvapo, text = lang.dlg[['label']][['24']], anchor = 'w', justify = 'left')
            en.evapo <- tkentry(frEvapo, textvariable = evapo.frac, width = 5)
            txt.evapo2 <- tklabel(frEvapo, text = lang.dlg[['label']][['25']], anchor = 'w', justify = 'left')
            tkgrid(txt.evapo1, en.evapo, txt.evapo2)

            ##
            frLastest <- tkframe(frameOnset)
            txt.late1 <- tklabel(frLastest, text = lang.dlg[['label']][['21']], anchor = 'w', justify = 'left')
            cb.late1 <- ttkcombobox(frLastest, values = MOIS, textvariable = onset.late.mon, width = 5)
            txt.late2 <- tklabel(frLastest, text = lang.dlg[['label']][['17']], anchor = 'w', justify = 'left')
            cb.late2 <- ttkcombobox(frLastest, values = 1:31, textvariable = onset.late.day, width = 4)
            tkgrid(txt.late1, cb.late1, txt.late2, cb.late2)

            ########
            tkgrid(frMethod, row = 0, sticky = 'we')
            tkgrid(frEarliest, row = 1, sticky = 'we')
            tkgrid(frMinWB, row = 2, sticky = 'we')
            tkgrid(frWinDay, row = 3, sticky = 'we')

            tkgrid(frLastest, row = 5, sticky = 'we')

            ########
            tkbind(cb.method, "<<ComboboxSelected>>", function(){
                tkdestroy(frMinWB)
                tkdestroy(frWinDay)
                tkdestroy(frAccMTH)
                tkdestroy(frAccRR)
                tkdestroy(frEvapo)

                if(trimws(tclvalue(onset.method)) == '1'){
                    frMinWB <<- tkframe(frameOnset)
                    txt.minwb1 <- tklabel(frMinWB, text = lang.dlg[['label']][['18']], anchor = 'w', justify = 'left')
                    en.minwb <- tkentry(frMinWB, textvariable = min.wb, width = 4)
                    txt.minwb2 <- tklabel(frMinWB, text = "mm", anchor = 'w', justify = 'left')
                    tkgrid(txt.minwb1, en.minwb, txt.minwb2)

                    frWinDay <<- tkframe(frameOnset)
                    txt.winday1 <- tklabel(frWinDay, text = lang.dlg[['label']][['19']], anchor = 'w', justify = 'left')
                    en.winday <- tkentry(frWinDay, textvariable = total.days, width = 4)
                    txt.winday2 <- tklabel(frWinDay, text = lang.dlg[['label']][['20']], anchor = 'w', justify = 'left')
                    tkgrid(txt.winday1, en.winday, txt.winday2)

                    tkgrid(frMinWB, row = 2, sticky = 'we')
                    tkgrid(frWinDay, row = 3, sticky = 'we')
                }

                if(trimws(tclvalue(onset.method)) == '2'){
                    frAccMTH <<- tkframe(frameOnset)
                    txt.AccMTH <- tklabel(frAccMTH, text = lang.dlg[['label']][['22-a']], anchor = 'w', justify = 'left')
                    cb.AccMTH <- ttkcombobox(frAccMTH, values = 1:2, textvariable = accum.method, width = 2)
                    tkgrid(txt.AccMTH, cb.AccMTH)

                    frAccRR <<- tkframe(frameOnset)
                    txt.AccRR1 <- tklabel(frAccRR, text = lang.dlg[['label']][['22']], anchor = 'w', justify = 'left')
                    en.AccRR <- tkentry(frAccRR, textvariable = accum.day, width = 4)
                    txt.AccRR2 <- tklabel(frAccRR, text = paste(lang.dlg[['label']][['20']], lang.dlg[['label']][['23']]), anchor = 'w', justify = 'left')
                    tkgrid(txt.AccRR1, en.AccRR, txt.AccRR2)

                    frEvapo <<- tkframe(frameOnset)
                    txt.evapo1 <- tklabel(frEvapo, text = lang.dlg[['label']][['24']], anchor = 'w', justify = 'left')
                    en.evapo <- tkentry(frEvapo, textvariable = evapo.frac, width = 5)
                    txt.evapo2 <- tklabel(frEvapo, text = lang.dlg[['label']][['25']], anchor = 'w', justify = 'left')
                    tkgrid(txt.evapo1, en.evapo, txt.evapo2)

                    tkgrid(frAccMTH, row = 2, sticky = 'we')
                    tkgrid(frAccRR, row = 3, sticky = 'we')
                    tkgrid(frEvapo, row = 4, sticky = 'we')
                }
            })

            ########
            tkgrid(frameOnset, pady = 5)
            tcl('update')

            ########
            return(list(frame = frameOnset,
                        onset.def = list(method = onset.method,
                                        min.wb = min.wb, total.days = total.days,
                                        thres.rain.day = thres.rain.day,
                                        accum.method = accum.method, accum.day = accum.day,
                                        evapo.frac = evapo.frac,
                                        earliest = list(month = onset.start.mon, day = onset.start.day),
                                        latest = list(month = onset.late.mon, day = onset.late.day))
                ))
        }

        ##############################################

        frameOnsetDEF <- tkframe(subfr2)

        ONSET.vars <- NULL
        ONSET.vars[[1]]$frame <- tkframe(frameOnsetDEF)
        tkgrid(ONSET.vars[[1]]$frame)

        ##############################################

        frameRegion <- tkframe(subfr2)

        onset.region <- tclVar(GeneralParameters$onset.reg$region)
        onset.subdiv <- tclVar(GeneralParameters$onset.reg$subdiv)

        onset.region <- tclVar()
        CbregionDIV <- lang.dlg[['combobox']][['2']]
        regionDIV <- c("One", "Multiple")
        tclvalue(onset.region) <- CbregionDIV[regionDIV %in% GeneralParameters$onset.reg$region]

        onset.subdiv <- tclVar()
        CbregionSUBDIV <- lang.dlg[['combobox']][['3']]
        regionSUBDIV <- c("Latitude", "Shapefile")
        tclvalue(onset.subdiv) <- CbregionSUBDIV[regionSUBDIV %in% GeneralParameters$onset.reg$subdiv]

        stateSubdv <- if(GeneralParameters$onset.reg$region == "One") "disabled" else "normal"

        txt.reg <- tklabel(frameRegion, text = lang.dlg[['label']][['8']], anchor = 'w', justify = 'left')
        cb.reg <- ttkcombobox(frameRegion, values = CbregionDIV, textvariable = onset.region, width = 6)
        txt.subdv <- tklabel(frameRegion, text = lang.dlg[['label']][['9']], anchor = 'w', justify = 'left')
        cb.subdv <- ttkcombobox(frameRegion, values = CbregionSUBDIV, textvariable = onset.subdiv, width = 8, state = stateSubdv)

        tkgrid(txt.reg, cb.reg, txt.subdv, cb.subdv)

        ######
        tkbind(cb.reg, "<<ComboboxSelected>>", function(){
            stateSubdv <- if(trimws(tclvalue(onset.region)) == CbregionDIV[1]) "disabled" else "normal"
            tkconfigure(cb.subdv, state = stateSubdv)

            OnsetDefinitionInput()
            tcl('update')
        })

        tkbind(cb.subdv, "<<ComboboxSelected>>", function(){
            OnsetDefinitionInput()
            tcl('update')
        })

        ########################

        frameRegDef <- tkframe(subfr2)

        shp.file <- tclVar(GeneralParameters$onset.reg$shp$file)
        shp.attr <- tclVar(GeneralParameters$onset.reg$shp$attr)
        shp.AttrTable <- tclVar()
        lat.nbdiv <- tclVar(GeneralParameters$onset.reg$lat$nb)
        lat.subdiv  <- vector('list', length = GeneralParameters$onset.reg$lat$nb - 1)
        for(i in seq_along(lat.subdiv))
            lat.subdiv[[i]] <- tclVar(GeneralParameters$onset.reg$lat$div[[i]])

        innerRegDef <- tkframe(frameRegDef)
        OnsetDefinitionInput()
        tcl('update')

        ##############################################

        bt.CalcOnset <- ttkbutton(subfr2, text = lang.dlg[['button']][['3']])

        tkconfigure(bt.CalcOnset, command = function(){
            GeneralParameters$data.type <- datatypeVAL[CbdatatypeVAL %in% trimws(tclvalue(DataType))]
            GeneralParameters$wb.data <- switch(tclvalue(water.balanceOK), '0' = FALSE, '1' = TRUE)

            if(trimws(tclvalue(DataType)) == CbdatatypeVAL[1]){
                GeneralParameters$cdtstation$wb <- trimws(tclvalue(input.WB))
                GeneralParameters$cdtstation$prec <- trimws(tclvalue(input.Prec))
                GeneralParameters$cdtstation$etp <- trimws(tclvalue(input.Etp))
            }

            if(trimws(tclvalue(DataType)) == CbdatatypeVAL[2]){
                GeneralParameters$cdtdataset$wb <- trimws(tclvalue(input.WB))
                GeneralParameters$cdtdataset$prec <- trimws(tclvalue(input.Prec))
                GeneralParameters$cdtdataset$etp <- trimws(tclvalue(input.Etp))
            }

            GeneralParameters$output <- trimws(tclvalue(dir.save))

            GeneralParameters$onset.reg$region <- trimws(tclvalue(onset.region))
            GeneralParameters$onset.reg$subdiv <- trimws(tclvalue(onset.subdiv))

            if(trimws(tclvalue(onset.region)) == CbregionDIV[2]){
                if(trimws(tclvalue(onset.subdiv)) == CbregionSUBDIV[1]){
                    GeneralParameters$onset.reg$lat$nb <- as.numeric(trimws(tclvalue(lat.nbdiv)))
                    GeneralParameters$onset.reg$lat$div <- lapply(lat.subdiv, function(x) as.numeric(trimws(tclvalue(x))))
                }

                if(trimws(tclvalue(onset.subdiv)) == CbregionSUBDIV[2]){
                    GeneralParameters$onset.reg$shp$file <- trimws(tclvalue(shp.file))
                    GeneralParameters$onset.reg$shp$attr <- trimws(tclvalue(shp.attr))
                }
            }

            GeneralParameters$onset.criteria <- lapply(ONSET.vars, function(x){
                                                        x <- x$onset.def
                                                        list(
                                                            method = as.numeric(trimws(tclvalue(x$method))),
                                                            min.wb = as.numeric(trimws(tclvalue(x$min.wb))),
                                                            total.days = as.numeric(trimws(tclvalue(x$total.days))),
                                                            thres.rain.day = as.numeric(trimws(tclvalue(x$thres.rain.day))),
                                                            accum.method = as.numeric(trimws(tclvalue(x$accum.method))),
                                                            accum.day = as.numeric(trimws(tclvalue(x$accum.day))),
                                                            evapo.frac = as.numeric(trimws(tclvalue(x$evapo.frac))),
                                                            earliest = list(month = which(MOIS %in% trimws(tclvalue(x$earliest$month))),
                                                                            day = as.numeric(trimws(tclvalue(x$earliest$day)))),
                                                            latest = list(month = which(MOIS %in% trimws(tclvalue(x$latest$month))),
                                                                            day = as.numeric(trimws(tclvalue(x$latest$day))))
                                                        )
                                                    })

            nbcriteria <- length(unlist(GeneralParameters$onset.def)) * length(GeneralParameters$onset.criteria)
            criteria <- unlist(GeneralParameters$onset.criteria)
            if(any(is.na(criteria)) | length(criteria) != nbcriteria){
                Insert.Messages.Out(lang.dlg[['message']][['7']], TRUE, 'e')
                return(NULL)
            }
            GeneralParameters$onset.criteria <<- GeneralParameters$onset.criteria

            prec.data <- FALSE
            omethods <- sapply(GeneralParameters$onset.criteria, "[[", "method")
            if(any(omethods != 1) | (any(omethods == 1) & !GeneralParameters$wb.data)){
                if(GeneralParameters$data.type == 'cdtstation'){
                    if(GeneralParameters$cdtstation$prec == "" |
                        GeneralParameters$cdtstation$etp == "") prec.data <- TRUE
                }else{
                    if(GeneralParameters$cdtdataset$prec == "" |
                        GeneralParameters$cdtdataset$etp == "") prec.data <- TRUE
                }
            }

            if(prec.data){
                tkmessageBox(message = paste0(lang.dlg[['message']][['15']], '\n', lang.dlg[['message']][['15-a']]), icon = "warning", type = "ok")
                stateWB <- if(any(omethods == 1) & GeneralParameters$wb.data) 'normal' else 'disabled'
                tkconfigure(cb.en.INWB, state = stateWB)
                tkconfigure(bt.INWB, state = stateWB)
                statesetWB <- if(any(omethods == 1) & !GeneralParameters$wb.data) 'normal' else 'disabled'
                tkconfigure(bt.setWB, state = statesetWB)

                tkconfigure(cb.en.INPrec, state = 'normal')
                tkconfigure(bt.INPrec, state = 'normal')
                tkconfigure(cb.en.INEtp, state = 'normal')
                tkconfigure(bt.INEtp, state = 'normal')
                return(NULL)
            }else{
                if(all(omethods == 1)){
                    if(GeneralParameters$wb.data){
                        stateWB <- 'normal'
                        statePrec <- 'disabled'
                        stateETP <- 'disabled'
                        statesetWB <- 'disabled'
                    }else{
                        stateWB <- 'disabled'
                        statePrec <- 'normal'
                        stateETP <- 'normal'
                        statesetWB <- 'normal'
                    }
                }else if(all(omethods != 1)){
                    stateWB <- 'disabled'
                    statePrec <- 'normal'
                    stateETP <- 'normal'
                    statesetWB <- 'disabled'
                }else{
                    stateWB <- 'normal'
                    statePrec <- 'normal'
                    stateETP <- 'normal'
                    statesetWB <- 'normal'
                }

                tkconfigure(bt.setWB, state = statesetWB)

                tkconfigure(cb.en.INWB, state = stateWB)
                tkconfigure(bt.INWB, state = stateWB)
                tkconfigure(cb.en.INPrec, state = statePrec)
                tkconfigure(bt.INPrec, state = statePrec)
                tkconfigure(cb.en.INEtp, state = stateETP)
                tkconfigure(bt.INEtp, state = stateETP)
            }

            # assign('GeneralParameters', GeneralParameters, envir = .GlobalEnv)

            Insert.Messages.Out(lang.dlg[['message']][['8']], TRUE, 'i')

            tkconfigure(.cdtEnv$tcl$main$win, cursor = 'watch')
            tcl('update')
            ret <- tryCatch({
                                compute_SeasonCessation_Procs(GeneralParameters)
                            },
                            warning = function(w){
                                warningFun(w)
                                return(0)
                            },
                            error = function(e) errorFun(e),
                            finally = {
                                tkconfigure(.cdtEnv$tcl$main$win, cursor = '')
                                tcl('update')
                            })

            if(!is.null(ret)){
                if(ret == 0){
                    Insert.Messages.Out(lang.dlg[['message']][['9']], TRUE, 's')

                    .cdtData$EnvData$plot.maps$data.type <- .cdtData$EnvData$output$params$data.type
                    .cdtData$EnvData$plot.maps[c('lon', 'lat', 'id')] <- .cdtData$EnvData$output$data[c('lon', 'lat', 'id')]
                    ###################
                    set.Data.Dates()
                    widgets.Station.Pixel()
                    set.plot.type()
                    res <- try(read.Data.Map(), silent = TRUE)
                    if(inherits(res, "try-error") | is.null(res)) return(NULL)
                }else Insert.Messages.Out(lang.dlg[['message']][['10']], TRUE, 'e')
            }else Insert.Messages.Out(lang.dlg[['message']][['10']], TRUE, 'e')
        })

        ##############################################

        tkgrid(frameRegion, row = 0, column = 0, sticky = 'we')
        tkgrid(frameRegDef, row = 1, column = 0, sticky = 'we')
        tkgrid(frameOnsetDEF, row = 2, column = 0, sticky = 'we')
        tkgrid(bt.CalcOnset, row = 3, column = 0, sticky = 'we')

    #######################################################################################################

    #Tab3
    subfr3 <- bwTabScrollableFrame(cmd.tab3)

        ##############################################

        frameDataExist <- ttklabelframe(subfr3, text = lang.dlg[['label']][['29']], relief = 'groove')

        DirExist <- tclVar(0)
        file.dataIndex <- tclVar()

        stateExistData <- if(tclvalue(DirExist) == "1") "normal" else "disabled"

        chk.dataIdx <- tkcheckbutton(frameDataExist, variable = DirExist, text = lang.dlg[['checkbutton']][['1']], anchor = 'w', justify = 'left')
        en.dataIdx <- tkentry(frameDataExist, textvariable = file.dataIndex, width = largeur2 + 5, state = stateExistData)
        bt.dataIdx <- ttkbutton(frameDataExist, text = .cdtEnv$tcl$lang$global[['button']][['6']], state = stateExistData)

        tkgrid(chk.dataIdx, row = 0, column = 0, sticky = 'we', rowspan = 1, columnspan = 4, padx = 1, pady = 1, ipadx = 1, ipady = 1)
        tkgrid(bt.dataIdx, row = 0, column = 4, sticky = 'e', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
        tkgrid(en.dataIdx, row = 1, column = 0, sticky = 'we', rowspan = 1, columnspan = 5, padx = 1, pady = 1, ipadx = 1, ipady = 1)

        #############

        tkconfigure(bt.dataIdx, command = function(){
            path.dataIdx <- tclvalue(tkgetOpenFile(initialdir = getwd(), filetypes = .cdtEnv$tcl$data$filetypes6))
            if(path.dataIdx %in% c("", "NA") | is.na(path.dataIdx)) return(NULL)
            tclvalue(file.dataIndex) <- path.dataIdx

            if(file.exists(trimws(tclvalue(file.dataIndex)))){
                OutIndexdata <- try(readRDS(trimws(tclvalue(file.dataIndex))), silent = TRUE)
                if(inherits(OutIndexdata, "try-error")){
                    Insert.Messages.Out(lang.dlg[['message']][['11']], TRUE, 'e')
                    Insert.Messages.Out(gsub('[\r\n]', '', OutIndexdata[1]), TRUE, 'e')
                    tkconfigure(cb.data.Index, values = "")
                    tclvalue(.cdtData$EnvData$donDate) <- ""
                    return(NULL)
                }

                .cdtData$EnvData$output <- OutIndexdata
                .cdtData$EnvData$PathData <- dirname(trimws(tclvalue(file.dataIndex)))
                .cdtData$EnvData$plot.maps$data.type <- .cdtData$EnvData$output$params$data.type
                .cdtData$EnvData$plot.maps[c('lon', 'lat', 'id')] <- .cdtData$EnvData$output$data[c('lon', 'lat', 'id')]
                ###################
                set.Data.Dates()
                widgets.Station.Pixel()
                set.plot.type()
                ret <- try(read.Data.Map(), silent = TRUE)
                if(inherits(ret, "try-error") | is.null(ret)) return(NULL)
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

        frameDataMap <- ttklabelframe(subfr3, text = lang.dlg[['label']][['30']], relief = 'groove')

        .cdtData$EnvData$donDate <- tclVar()

        cb.data.Index <- ttkcombobox(frameDataMap, values = "", textvariable = .cdtData$EnvData$donDate, justify = 'center', width = largeur6)
        bt.data.Index.prev <- ttkbutton(frameDataMap, text = "<<", width = largeur7)
        bt.data.Index.next <- ttkbutton(frameDataMap, text = ">>", width = largeur7)
        bt.data.maps <- ttkbutton(frameDataMap, text = .cdtEnv$tcl$lang$global[['button']][['3']], width = largeur5)
        bt.data.MapOpt <- ttkbutton(frameDataMap, text = .cdtEnv$tcl$lang$global[['button']][['4']], width = largeur5)

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
                .cdtData$EnvData$tab$pointSize <- .cdtData$EnvData$dataMapOp$pointSize
        })

        #########
        .cdtData$EnvData$tab$dataMap <- NULL

        tkconfigure(bt.data.maps, command = function(){
            if(trimws(tclvalue(.cdtData$EnvData$donDate)) != "" &
                !is.null(.cdtData$EnvData$varData))
                    CessationCalc.Display.Maps()
        })

        tkconfigure(bt.data.Index.prev, command = function(){
            if(trimws(tclvalue(.cdtData$EnvData$donDate)) != ""){
                donDates <- format(.cdtData$EnvData$output$start.date, "%Y")
                idaty <- which(donDates == trimws(tclvalue(.cdtData$EnvData$donDate)))
                idaty <- idaty - 1
                if(idaty < 1) idaty <- length(donDates)
                tclvalue(.cdtData$EnvData$donDate) <- donDates[idaty]

                ret <- try(read.Data.Map(), silent = TRUE)
                if(inherits(ret, "try-error") | is.null(ret)) return(NULL)

                CessationCalc.Display.Maps()
            }
        })

        tkconfigure(bt.data.Index.next, command = function(){
            if(trimws(tclvalue(.cdtData$EnvData$donDate)) != ""){
                donDates <- format(.cdtData$EnvData$output$start.date, "%Y")
                idaty <- which(donDates == trimws(tclvalue(.cdtData$EnvData$donDate)))
                idaty <- idaty + 1
                if(idaty > length(donDates)) idaty <- 1
                tclvalue(.cdtData$EnvData$donDate) <- donDates[idaty]

                ret <- try(read.Data.Map(), silent = TRUE)
                if(inherits(ret, "try-error") | is.null(ret)) return(NULL)

                CessationCalc.Display.Maps()
            }
        })

        ###############

        tkbind(cb.data.Index, "<<ComboboxSelected>>", function(){
            if(!is.null(.cdtData$EnvData$varData)){
                ret <- try(read.Data.Map(), silent = TRUE)
                if(inherits(ret, "try-error") | is.null(ret)) return(NULL)
            }
        })

        ##############################################

        framePlotType <- tkframe(subfr3)

        .cdtData$EnvData$plot.maps$plot.type <- tclVar("Pixels")

        txt.plotType <- tklabel(framePlotType, text = lang.dlg[['label']][['38']], anchor = 'e', justify = 'right')
        cb.plotType <- ttkcombobox(framePlotType, values = "Pixels", textvariable = .cdtData$EnvData$plot.maps$plot.type, justify = 'center', width = largeur3)

        tkgrid(txt.plotType, row = 0, column = 0, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
        tkgrid(cb.plotType, row = 0, column = 1, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)

        ###############

        tkbind(cb.plotType, "<<ComboboxSelected>>", function(){
            if(!is.null(.cdtData$EnvData$varData)){
                ret <- try(read.Data.Map(), silent = TRUE)
                if(inherits(ret, "try-error") | is.null(ret)) return(NULL)
            }
        })

        ##############################################

        tkgrid(frameDataExist, row = 0, column = 0, sticky = 'we', padx = 1, pady = 1, ipadx = 1, ipady = 1)
        tkgrid(frameDataMap, row = 1, column = 0, sticky = 'we', padx = 1, pady = 3, ipadx = 1, ipady = 1)
        tkgrid(framePlotType, row = 2, column = 0, sticky = '', padx = 1, pady = 3, ipadx = 1, ipady = 1)

    #######################################################################################################

    #Tab4
    subfr4 <- bwTabScrollableFrame(cmd.tab4)

        ##############################################

        frameDataTS <- ttklabelframe(subfr4, text = lang.dlg[['label']][['31']], relief = 'groove')

        typeTSPLOT <- c("Line", "Barplot")
        .cdtData$EnvData$plot.maps$typeTSp <- tclVar("Line")

        cb.typeTSp <- ttkcombobox(frameDataTS, values = typeTSPLOT, textvariable = .cdtData$EnvData$plot.maps$typeTSp, justify = 'center', width = largeur4)
        bt.TsGraph.plot <- ttkbutton(frameDataTS, text = .cdtEnv$tcl$lang$global[['button']][['3']], width = largeur5)
        bt.TSGraphOpt <- ttkbutton(frameDataTS, text = .cdtEnv$tcl$lang$global[['button']][['4']], width = largeur5)

        #################

        tkgrid(cb.typeTSp, row = 0, column = 2, sticky = 'we', rowspan = 1, columnspan = 6, padx = 1, pady = 1, ipadx = 1, ipady = 1)
        tkgrid(bt.TSGraphOpt, row = 1, column = 0, sticky = 'we', rowspan = 1, columnspan = 5, padx = 1, pady = 1, ipadx = 1, ipady = 1)
        tkgrid(bt.TsGraph.plot, row = 1, column = 5, sticky = 'we', rowspan = 1, columnspan = 5, padx = 1, pady = 1, ipadx = 1, ipady = 1)

        #################

        tkconfigure(bt.TSGraphOpt, command = function(){
            suffix.fun <- switch(trimws(tclvalue(.cdtData$EnvData$plot.maps$typeTSp)),
                                    "Barplot" = "Bar",
                                    "Line" = "Line")
            plot.fun <- get(paste0("MapGraph.GraphOptions.", suffix.fun), mode = "function")
            .cdtData$EnvData$TSGraphOp <- plot.fun(.cdtData$EnvData$TSGraphOp)
        })

        #########
        .cdtData$EnvData$tab$dataGraph <- NULL

        tkconfigure(bt.TsGraph.plot, command = function(){
            if(!is.null(.cdtData$EnvData$varData)){
                imgContainer <- CDT.Display.Graph(CessationCalc.plotCessationGraph, .cdtData$EnvData$tab$dataGraph, 'Cessation-Graph')
                .cdtData$EnvData$tab$dataGraph <- imageNotebookTab_unik(imgContainer, .cdtData$EnvData$tab$dataGraph)
            }
        })

        ##############################################

        frameSTNCrds <- ttklabelframe(subfr4, text = lang.dlg[['label']][['32']], relief = 'groove')

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

        ##############################################

        frameSHP <- ttklabelframe(subfr5, text = lang.dlg[['label']][['33']], relief = 'groove')

        .cdtData$EnvData$shp$add.shp <- tclVar(FALSE)
        file.plotShp <- tclVar()
        stateSHP <- "disabled"

        chk.addshp <- tkcheckbutton(frameSHP, variable = .cdtData$EnvData$shp$add.shp, text = lang.dlg[['checkbutton']][['2']], anchor = 'w', justify = 'left')
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
            txt.stnSel <- tklabel(frTS2, text = lang.dlg[['label']][['34']])
            bt.stnID.prev <- ttkbutton(frTS2, text = "<<", width = largeur7)
            bt.stnID.next <- ttkbutton(frTS2, text = ">>", width = largeur7)
            cb.stnID <- ttkcombobox(frTS2, values = stnIDTSPLOT, textvariable = .cdtData$EnvData$plot.maps$stnIDTSp, justify = 'center', width = largeur6)
            tclvalue(.cdtData$EnvData$plot.maps$stnIDTSp) <- stnIDTSPLOT[1]

            tkgrid(txt.stnSel, row = 0, column = 0, sticky = '', rowspan = 1, columnspan = 3, padx = 1, pady = 1, ipadx = 1, ipady = 1)
            tkgrid(bt.stnID.prev, row = 1, column = 0, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
            tkgrid(cb.stnID, row = 1, column = 1, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
            tkgrid(bt.stnID.next, row = 1, column = 2, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)

            ######
            tkconfigure(bt.stnID.prev, command = function(){
                if(!is.null(.cdtData$EnvData$varData)){
                    istn <- which(stnIDTSPLOT == trimws(tclvalue(.cdtData$EnvData$plot.maps$stnIDTSp)))
                    istn <- istn - 1
                    if(istn < 1) istn <- length(stnIDTSPLOT)
                    tclvalue(.cdtData$EnvData$plot.maps$stnIDTSp) <- stnIDTSPLOT[istn]

                    imgContainer <- CDT.Display.Graph(CessationCalc.plotCessationGraph, .cdtData$EnvData$tab$dataGraph, 'Cessation-Graph')
                    .cdtData$EnvData$tab$dataGraph <- imageNotebookTab_unik(imgContainer, .cdtData$EnvData$tab$dataGraph)
                }
            })

            tkconfigure(bt.stnID.next, command = function(){
                if(!is.null(.cdtData$EnvData$varData)){
                    istn <- which(stnIDTSPLOT == trimws(tclvalue(.cdtData$EnvData$plot.maps$stnIDTSp)))
                    istn <- istn + 1
                    if(istn > length(stnIDTSPLOT)) istn <- 1
                    tclvalue(.cdtData$EnvData$plot.maps$stnIDTSp) <- stnIDTSPLOT[istn]

                    imgContainer <- CDT.Display.Graph(CessationCalc.plotCessationGraph, .cdtData$EnvData$tab$dataGraph, 'Cessation-Graph')
                    .cdtData$EnvData$tab$dataGraph <- imageNotebookTab_unik(imgContainer, .cdtData$EnvData$tab$dataGraph)
                }
            })
        }else{
            txt.crdSel <- tklabel(frTS2, text = lang.dlg[['label']][['35']], anchor = 'w', justify = 'left')
            txt.lonLoc <- tklabel(frTS2, text = lang.dlg[['label']][['36']], anchor = 'e', justify = 'right')
            en.lonLoc <- tkentry(frTS2, textvariable = .cdtData$EnvData$plot.maps$lonLOC, width = largeur8)
            txt.latLoc <- tklabel(frTS2, text = lang.dlg[['label']][['37']], anchor = 'e', justify = 'right')
            en.latLoc <- tkentry(frTS2, textvariable = .cdtData$EnvData$plot.maps$latLOC, width = largeur8)
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

    set.Data.Dates <- function(){
        donDates <- format(.cdtData$EnvData$output$start.date, "%Y")
        tkconfigure(cb.data.Index, values = donDates)
        tclvalue(.cdtData$EnvData$donDate) <- donDates[length(donDates)]
        return(0)
    }

    #######################################################################################################

    read.Data.Map <- function(){
        tkconfigure(.cdtEnv$tcl$main$win, cursor = 'watch')
        tcl('update')
        on.exit({
            tkconfigure(.cdtEnv$tcl$main$win, cursor = '')
            tcl('update')
        })

        this.daty <- trimws(tclvalue(.cdtData$EnvData$donDate))
        idt <- which(format(.cdtData$EnvData$output$start.date, "%Y") == this.daty)

        if(.cdtData$EnvData$output$params$data.type == "cdtstation"){
            filePathData <- file.path(.cdtData$EnvData$PathData, "CDTDATASET/CESSATION.rds")
            if(!file.exists(filePathData)){
                Insert.Messages.Out(paste(filePathData, lang.dlg[['message']][['12']]), TRUE, 'e')
                return(NULL)
            }

            change.plot <- trimws(tclvalue(.cdtData$EnvData$plot.maps$plot.type))

            ########
            readVarData <- TRUE
            if(!is.null(.cdtData$EnvData$varData))
                if(!is.null(.cdtData$EnvData$filePathData))
                    if(.cdtData$EnvData$filePathData == filePathData) readVarData <- FALSE

            if(readVarData){
                .cdtData$EnvData$varData$data <- readRDS(filePathData)
                .cdtData$EnvData$filePathData <- filePathData
            }

            ########
            rasterVarData <- TRUE
            if(!rasterVarData)
                if(!is.null(.cdtData$EnvData$varData$rasterDate))
                    if(.cdtData$EnvData$filePathData == filePathData)
                        if(.cdtData$EnvData$varData$rasterDate == this.daty) rasterVarData <- FALSE

            if(!rasterVarData)
                if(.cdtData$EnvData$change.plot != change.plot) rasterVarData <- TRUE

            if(rasterVarData){
                X0 <- .cdtData$EnvData$output$data$lon
                Y0 <- .cdtData$EnvData$output$data$lat
                VAR0 <- as.numeric(.cdtData$EnvData$varData$data[idt, ] - .cdtData$EnvData$output$start.date[idt])

                if(change.plot == "Pixels"){
                    nx <- nx_ny_as.image(diff(range(X0)))
                    ny <- nx_ny_as.image(diff(range(Y0)))
                    tmp <- cdt.as.image(VAR0, nx = nx, ny = ny, pts.xy = cbind(X0, Y0))
                    .cdtData$EnvData$varData$map$x <- tmp$x
                    .cdtData$EnvData$varData$map$y <- tmp$y
                    .cdtData$EnvData$varData$map$z <- tmp$z
                    rm(tmp)
                }

                if(change.plot == "Points"){
                    .cdtData$EnvData$varData$map$x <- X0
                    .cdtData$EnvData$varData$map$y <- Y0
                    .cdtData$EnvData$varData$map$z <- VAR0
                }

                .cdtData$EnvData$varData$rasterDate <- this.daty
                .cdtData$EnvData$change.plot <- change.plot
            }
        }else{
            filePathData <- file.path(.cdtData$EnvData$PathData, "DATA_NetCDF",
                            paste0("cessation_", format(.cdtData$EnvData$output$start.date[idt], "%Y%m%d"), ".nc"))
            if(!file.exists(filePathData)){
                Insert.Messages.Out(paste(filePathData, lang.dlg[['message']][['12']]), TRUE, 'e')
                return(NULL)
            }

            readVarData <- TRUE
            if(!is.null(.cdtData$EnvData$varData))
                if(!is.null(.cdtData$EnvData$filePathData))
                    if(.cdtData$EnvData$filePathData == filePathData) readVarData <- FALSE

            if(readVarData){
                nc <- ncdf4::nc_open(filePathData)
                .cdtData$EnvData$varData$map$x <- nc$dim[[1]]$vals
                .cdtData$EnvData$varData$map$y <- nc$dim[[2]]$vals
                .cdtData$EnvData$varData$map$z <- ncdf4::ncvar_get(nc, varid = nc$var[[1]]$name)
                ncdf4::nc_close(nc)
                .cdtData$EnvData$filePathData <- filePathData
            }

            ###################

            file.CDT.Idx <- file.path(.cdtData$EnvData$PathData, "CDTDATASET/CDTDATASET.rds")

            read.cdt.dataIdx<- TRUE
            if(!is.null(.cdtData$EnvData$cdtdataset))
                if(!is.null(.cdtData$EnvData$file.CDT.Idx))
                    if(.cdtData$EnvData$file.CDT.Idx == file.CDT.Idx) read.cdt.dataIdx <- FALSE
            if(read.cdt.dataIdx){
                .cdtData$EnvData$cdtdataset <- readRDS(file.CDT.Idx)
                .cdtData$EnvData$cdtdataset$fileInfo <- file.CDT.Idx
                .cdtData$EnvData$file.CDT.Idx <- file.CDT.Idx
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
