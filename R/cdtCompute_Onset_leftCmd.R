
OnsetCalcPanelCmd <- function(){
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
        largeur10 <- 7
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
    GeneralParameters <- list(data.type = "cdtstation", 
                              cdtstation = list(prec = "", etp = ""),
                              cdtdataset = list(prec = "", etp = ""),
                              onset.def = list(method = 5, thres.rain.day = 0.85,
                                               total.days = 5, rain.total = 20,
                                               min.rain.day = 3, dryspell = 7, dryspell.days = 21,
                                               evapo.frac = 0.5,
                                               earliest = list(month = 9, day = 1),
                                               latest = list(month = 11, day = 30)
                                            ),
                              min.frac = 0.95,
                              onset.reg = list(region = "One", subdiv = "Latitude",
                                               lat = list(nb = 2, div = list(8)),
                                               shp = list(file = "", attr = "")
                                             ),
                              output = "")

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
                                            xlim = list(is.min = FALSE, min = 1981, is.max = FALSE, max = 2021),
                                            ylim = list(is.min = FALSE, min = 0, is.max = FALSE, max = 100),
                                            axislabs = list(is.xlab = FALSE, xlab = '', is.ylab = FALSE, ylab = ''),
                                            title = list(is.title = FALSE, title = '', position = 'top'),
                                            colors = list(col = "darkblue")
                                        ),
                                    line = list(
                                        xlim = list(is.min = FALSE, min = 1981, is.max = FALSE, max = 2021),
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

    xml.dlg <- file.path(.cdtDir$dirLocal, "languages", "cdtCompute_Onset_leftCmd.xml")
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

        if(GeneralParameters$data.type == 'cdtstation'){
            input.Prec <- tclVar(GeneralParameters$cdtstation$prec)
            input.Etp <- tclVar(GeneralParameters$cdtstation$etp)
            txt.INPrec <- lang.dlg[['label']][['3']]
            txt.INEtp <- lang.dlg[['label']][['4']]
        }else{
            input.Prec <- tclVar(GeneralParameters$cdtdataset$prec)
            input.Etp <- tclVar(GeneralParameters$cdtdataset$etp)
            txt.INPrec <- lang.dlg[['label']][['5']]
            txt.INEtp <- lang.dlg[['label']][['6']]
        }
        txt.INPrec.var <- tclVar(txt.INPrec)
        txt.INEtp.var <- tclVar(txt.INEtp)

        stateETP <- 'disabled'

        txt.datatype <- tklabel(frameInData, text = lang.dlg[['label']][['2']], anchor = 'w', justify = 'left')
        cb.datatype <- ttkcombobox(frameInData, values = CbdatatypeVAL, textvariable = DataType, width = largeur0)

        txt.INPrec <- tklabel(frameInData, text = tclvalue(txt.INPrec.var), textvariable = txt.INPrec.var, anchor = 'w', justify = 'left')
        if(GeneralParameters$data.type == 'cdtstation'){
            cb.en.INPrec <- ttkcombobox(frameInData, values = unlist(listOpenFiles), textvariable = input.Prec, width = largeur1)
        }else{
            cb.en.INPrec <- tkentry(frameInData, textvariable = input.Prec, width = largeur2)
        }
        bt.INPrec <- tkbutton(frameInData, text = "...")

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
        tkgrid(txt.INPrec, row = 1, column = 0, sticky = 'we', rowspan = 1, columnspan = 10, padx = 1, pady = 1, ipadx = 1, ipady = 1)
        tkgrid(cb.en.INPrec, row = 2, column = 0, sticky = 'we', rowspan = 1, columnspan = 9, padx = 0, pady = 1, ipadx = 1, ipady = 1)
        tkgrid(bt.INPrec, row = 2, column = 9, sticky = 'w', rowspan = 1, columnspan = 1, padx = 0, pady = 1, ipadx = 1, ipady = 1)
        tkgrid(txt.INEtp, row = 3, column = 0, sticky = 'we', rowspan = 1, columnspan = 10, padx = 1, pady = 1, ipadx = 1, ipady = 1)
        tkgrid(cb.en.INEtp, row = 4, column = 0, sticky = 'we', rowspan = 1, columnspan = 9, padx = 0, pady = 1, ipadx = 1, ipady = 1)
        tkgrid(bt.INEtp, row = 4, column = 9, sticky = 'w', rowspan = 1, columnspan = 1, padx = 0, pady = 1, ipadx = 1, ipady = 1)

        ############

        helpWidget(cb.datatype, lang.dlg[['tooltip']][['1']], lang.dlg[['status']][['1']])

        if(GeneralParameters$data.type == 'cdtstation'){
            helpWidget(cb.en.INPrec, lang.dlg[['tooltip']][['2']], lang.dlg[['status']][['2']])
            helpWidget(cb.en.INEtp, lang.dlg[['tooltip']][['3']], lang.dlg[['status']][['3']])
            helpWidget(bt.INPrec, lang.dlg[['tooltip']][['6']], lang.dlg[['status']][['6']])
            helpWidget(bt.INEtp, lang.dlg[['tooltip']][['6']], lang.dlg[['status']][['6']])
        }else{
            helpWidget(cb.en.INPrec, lang.dlg[['tooltip']][['4']], lang.dlg[['status']][['4']])
            helpWidget(cb.en.INEtp, lang.dlg[['tooltip']][['5']], lang.dlg[['status']][['5']])
            helpWidget(bt.INPrec, lang.dlg[['tooltip']][['7']], lang.dlg[['status']][['7']])
            helpWidget(bt.INEtp, lang.dlg[['tooltip']][['7']], lang.dlg[['status']][['7']])
        }

        ############

        tkconfigure(bt.INPrec, command = function(){
            if(GeneralParameters$data.type == 'cdtstation'){
                dat.opfiles <- getOpenFiles(.cdtEnv$tcl$main$win)
                if(!is.null(dat.opfiles)){
                    update.OpenFiles('ascii', dat.opfiles)
                    listOpenFiles[[length(listOpenFiles) + 1]] <<- dat.opfiles[[1]]
                    tclvalue(input.Prec) <- dat.opfiles[[1]]
                    lapply(list(cb.en.INPrec, cb.en.INEtp), tkconfigure, values = unlist(listOpenFiles))
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
                    lapply(list(cb.en.INPrec, cb.en.INEtp), tkconfigure, values = unlist(listOpenFiles))
                }
            }else{
                path.rds <- tclvalue(tkgetOpenFile(initialdir = getwd(), filetypes = .cdtEnv$tcl$data$filetypes6))
                tclvalue(input.Etp) <- if(path.rds %in% c("", "NA") | is.na(path.rds)) "" else path.rds
            }
        })

        ############

        tkbind(cb.datatype, "<<ComboboxSelected>>", function(){
            tkdestroy(cb.en.INPrec)
            tclvalue(input.Prec) <- ''

            tkdestroy(cb.en.INEtp)
            tclvalue(input.Etp) <- ''

            stateETP <- 'disabled'

            ###
            if(str_trim(tclvalue(DataType)) == CbdatatypeVAL[1]){
                tclvalue(txt.INPrec.var) <- lang.dlg[['label']][['3']]
                tclvalue(txt.INEtp.var) <- lang.dlg[['label']][['4']]

                cb.en.INPrec <- ttkcombobox(frameInData, values = unlist(listOpenFiles), textvariable = input.Prec, width = largeur1)
                cb.en.INEtp <<- ttkcombobox(frameInData, values = unlist(listOpenFiles), textvariable = input.Etp, width = largeur1, state = stateETP)

                ######
                helpWidget(cb.en.INPrec, lang.dlg[['tooltip']][['2']], lang.dlg[['status']][['2']])
                helpWidget(cb.en.INEtp, lang.dlg[['tooltip']][['3']], lang.dlg[['status']][['3']])
                helpWidget(bt.INPrec, lang.dlg[['tooltip']][['6']], lang.dlg[['status']][['6']])
                helpWidget(bt.INEtp, lang.dlg[['tooltip']][['6']], lang.dlg[['status']][['6']])

                ######
                tkconfigure(bt.INPrec, command = function(){
                    dat.opfiles <- getOpenFiles(.cdtEnv$tcl$main$win)
                    if(!is.null(dat.opfiles)){
                        update.OpenFiles('ascii', dat.opfiles)
                        listOpenFiles[[length(listOpenFiles) + 1]] <<- dat.opfiles[[1]]
                        tclvalue(input.Prec) <- dat.opfiles[[1]]
                        lapply(list(cb.en.INPrec, cb.en.INEtp), tkconfigure, values = unlist(listOpenFiles))
                    }
                })

                tkconfigure(bt.INEtp, state = stateETP, command = function(){
                    dat.opfiles <- getOpenFiles(.cdtEnv$tcl$main$win)
                    if(!is.null(dat.opfiles)){
                        update.OpenFiles('ascii', dat.opfiles)
                        listOpenFiles[[length(listOpenFiles) + 1]] <<- dat.opfiles[[1]]
                        tclvalue(input.Etp) <- dat.opfiles[[1]]
                        lapply(list(cb.en.INPrec, cb.en.INEtp), tkconfigure, values = unlist(listOpenFiles))
                    }
                })
            }

            ###
            if(str_trim(tclvalue(DataType)) == CbdatatypeVAL[2]){
                tclvalue(txt.INPrec.var) <- lang.dlg[['label']][['5']]
                tclvalue(txt.INEtp.var) <- lang.dlg[['label']][['6']]

                cb.en.INPrec <- tkentry(frameInData, textvariable = input.Prec, width = largeur2)
                cb.en.INEtp <<- tkentry(frameInData, textvariable = input.Etp, width = largeur2, state = stateETP)

                ######
                helpWidget(cb.en.INPrec, lang.dlg[['tooltip']][['4']], lang.dlg[['status']][['4']])
                helpWidget(cb.en.INEtp, lang.dlg[['tooltip']][['5']], lang.dlg[['status']][['5']])
                helpWidget(bt.INPrec, lang.dlg[['tooltip']][['7']], lang.dlg[['status']][['7']])
                helpWidget(bt.INEtp, lang.dlg[['tooltip']][['7']], lang.dlg[['status']][['7']])

                ######
                tkconfigure(bt.INPrec, command = function(){
                    path.rds <- tclvalue(tkgetOpenFile(initialdir = getwd(), filetypes = .cdtEnv$tcl$data$filetypes6))
                    tclvalue(input.Prec) <- if(path.rds %in% c("", "NA")) "" else path.rds
                })

                tkconfigure(bt.INEtp, state = stateETP, command = function(){
                    path.rds <- tclvalue(tkgetOpenFile(initialdir = getwd(), filetypes = .cdtEnv$tcl$data$filetypes6))
                    tclvalue(input.Etp) <- if(path.rds %in% c("", "NA")) "" else path.rds
                })
            }

            #######
            tkgrid(cb.en.INPrec, row = 2, column = 0, sticky = 'we', rowspan = 1, columnspan = 9, padx = 0, pady = 1, ipadx = 1, ipady = 1)
            tkgrid(cb.en.INEtp, row = 4, column = 0, sticky = 'we', rowspan = 1, columnspan = 9, padx = 0, pady = 1, ipadx = 1, ipady = 1)
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
            if(str_trim(tclvalue(onset.region)) == CbregionDIV[2]){
                if(str_trim(tclvalue(onset.subdiv)) == CbregionSUBDIV[1]){
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
                        lat.subdiv <<- vector('list', length = as.numeric(str_trim(tclvalue(lat.nbdiv))) - 1)
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
                            lapply(list(cb.shp.file, cb.en.INPrec, cb.en.INEtp), tkconfigure, values = unlist(listOpenFiles))
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
                    if(str_trim(tclvalue(onset.subdiv)) == CbregionSUBDIV[1]){
                        lat <- rep(NA, length(lat.subdiv))
                        for(i in seq_along(lat.subdiv)) lat[i] <- as.numeric(str_trim(tclvalue(lat.subdiv[[i]])))
                        lat <- ifelse(lat > 60 | lat < -60, NA, lat)
                        if(any(is.na(lat))){
                            lat0 <- sapply(lat.subdiv, tclvalue)
                            Insert.Messages.Out(paste(lang.dlg[['message']][['1']], paste0(lat0[is.na(lat)], collapse = " ")), TRUE, 'e')
                            subdiv <- NULL
                        }else{
                            subdiv <- vector('character', as.numeric(str_trim(tclvalue(lat.nbdiv))))
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
            tkgrid(innerRegDef, sticky = 'we')
        }

        OnsetDefinitionCriteria <- function(containerFrame, Parameters, subdiv = NA){
            labelReg <- if(is.na(subdiv)) lang.dlg[['label']][['14']] else paste(lang.dlg[['label']][['14']], "- [", subdiv, "]")

            frameOnset <- ttklabelframe(containerFrame, text = labelReg, relief = 'sunken')

            #########
            onset.method <- tclVar(Parameters$method)
            mon1 <- as.numeric(str_trim(Parameters$earliest$month))
            onset.start.mon <- tclVar(MOIS[mon1])
            onset.start.day <- tclVar(Parameters$earliest$day)
            thres.rain.day <- tclVar(Parameters$thres.rain.day)
            total.rain <- tclVar(Parameters$rain.total)
            total.days <- tclVar(Parameters$total.days)
            evapo.frac <- tclVar(Parameters$evapo.frac)
            min.rain.day <- tclVar(Parameters$min.rain.day)
            dryspell <- tclVar(Parameters$dryspell)
            dryspell.days <- tclVar(Parameters$dryspell.days)
            mon2 <- as.numeric(str_trim(Parameters$latest$month))
            onset.late.mon <- tclVar(MOIS[mon2])
            onset.late.day <- tclVar(Parameters$latest$day)

            ##########

            frMethod <- tkframe(frameOnset)
            txt.method <- tklabel(frMethod, text = lang.dlg[['label']][['15']], anchor = 'w', justify = 'left')
            cb.method <- ttkcombobox(frMethod, values = 1:5, textvariable = onset.method, width = 3, justify = 'center')
            bt.method <- ttkbutton(frMethod, text = lang.dlg[['button']][['2']])

            txt.sepMthd <- tklabel(frMethod, text = "", width = largeur10)

            tkgrid(txt.method, cb.method, txt.sepMthd, bt.method)
            tkgrid.configure(bt.method, sticky = 'e', pady = 3)

            ##########

            tkconfigure(bt.method, command = function(){
                Insert.Messages.Out(lang.dlg[['message']][['2']])
                Insert.Messages.Out(lang.dlg[['message']][['3']])
                Insert.Messages.Out(lang.dlg[['message']][['4']])
                Insert.Messages.Out(lang.dlg[['message']][['5']])
                Insert.Messages.Out(lang.dlg[['message']][['6']])
            })

            frEarliest <- tkframe(frameOnset)
            txt.early1 <- tklabel(frEarliest, text = lang.dlg[['label']][['16']], anchor = 'w', justify = 'left')
            cb.early1 <- ttkcombobox(frEarliest, values = MOIS, textvariable = onset.start.mon, width = 5)
            txt.early2 <- tklabel(frEarliest, text = lang.dlg[['label']][['17']], anchor = 'w', justify = 'left')
            cb.early2 <- ttkcombobox(frEarliest, values = 1:31, textvariable = onset.start.day, width = 4)
            tkgrid(txt.early1, cb.early1, txt.early2, cb.early2)

            frThresRain <- tkframe(frameOnset)
            txt.thresr1 <- tklabel(frThresRain, text = lang.dlg[['label']][['18']], anchor = 'w', justify = 'left')
            en.thresr <- tkentry(frThresRain, textvariable = thres.rain.day, width = 6)
            txt.thresr2 <- tklabel(frThresRain, text = "mm", anchor = 'w', justify = 'left')
            tkgrid(txt.thresr1, en.thresr, txt.thresr2)

            frRainTotal <- tkframe(frameOnset)
            txt.raintot1 <- tklabel(frRainTotal, text = lang.dlg[['label']][['19']], anchor = 'w', justify = 'left')
            en.raintot1 <- tkentry(frRainTotal, textvariable = total.rain, width = 5)
            txt.raintot2 <- tklabel(frRainTotal, text = "mm", anchor = 'w', justify = 'left')
            txt.raintot3 <- tklabel(frRainTotal, text = lang.dlg[['label']][['20']], anchor = 'w', justify = 'left')
            en.raintot2 <- tkentry(frRainTotal, textvariable = total.days, width = 4)
            txt.raintot4 <- tklabel(frRainTotal, text = lang.dlg[['label']][['21']], anchor = 'w', justify = 'left')
            tkgrid(txt.raintot1, en.raintot1, txt.raintot2, txt.raintot3, en.raintot2, txt.raintot4)

            # mthd 2
            frEvapo <- tkframe(frameOnset)
            txt.evapo1 <- tklabel(frEvapo, text = lang.dlg[['label']][['22']], anchor = 'w', justify = 'left')
            en.evapo <- tkentry(frEvapo, textvariable = evapo.frac, width = 5)
            txt.evapo2 <- tklabel(frEvapo, text = lang.dlg[['label']][['23']], anchor = 'w', justify = 'left')
            tkgrid(txt.evapo1, en.evapo, txt.evapo2)

            # mthd 3
            frMinDays <- tkframe(frameOnset)
            txt.minday1 <- tklabel(frMinDays, text = lang.dlg[['label']][['24']], anchor = 'w', justify = 'left')
            en.minday <- tkentry(frMinDays, textvariable = min.rain.day, width = 4)
            txt.minday2 <- tklabel(frMinDays, text = lang.dlg[['label']][['25']], anchor = 'w', justify = 'left')
            tkgrid(txt.minday1, en.minday, txt.minday2)

            # mthd 4
            frDrySpell <- tkframe(frameOnset)
            txt.dryspl1 <- tklabel(frDrySpell, text = lang.dlg[['label']][['26']], anchor = 'w', justify = 'left')
            en.dryspl <- tkentry(frDrySpell, textvariable = dryspell, width = 4)
            txt.dryspl2 <- tklabel(frDrySpell, text = lang.dlg[['label']][['21']], anchor = 'w', justify = 'left')
            tkgrid(txt.dryspl1, en.dryspl, txt.dryspl2)

            frDrySpell1 <- tkframe(frameOnset)
            txt.dryspld1 <- tklabel(frDrySpell1, text = lang.dlg[['label']][['27']], anchor = 'w', justify = 'left')
            en.dryspld <- tkentry(frDrySpell1, textvariable = dryspell.days, width = 4)
            txt.dryspld2 <- tklabel(frDrySpell1, text = lang.dlg[['label']][['21']], anchor = 'w', justify = 'left')
            tkgrid(txt.dryspld1, en.dryspld, txt.dryspld2)

            ########
            frLastest <- tkframe(frameOnset)
            txt.late1 <- tklabel(frLastest, text = lang.dlg[['label']][['28']], anchor = 'w', justify = 'left')
            cb.late1 <- ttkcombobox(frLastest, values = MOIS, textvariable = onset.late.mon, width = 5)
            txt.late2 <- tklabel(frLastest, text = lang.dlg[['label']][['17']], anchor = 'w', justify = 'left')
            cb.late2 <- ttkcombobox(frLastest, values = 1:31, textvariable = onset.late.day, width = 4)
            tkgrid(txt.late1, cb.late1, txt.late2, cb.late2)

            ########
            tkgrid(frMethod, row = 0, sticky = 'we')
            tkgrid(frEarliest, row = 1, sticky = 'we')
            tkgrid(frThresRain, row = 2, sticky = 'we')
            tkgrid(frRainTotal, row = 3, sticky = 'we')

            tkgrid(frMinDays, row = 5, sticky = 'we')
            tkgrid(frDrySpell, row = 6, sticky = 'we')
            tkgrid(frDrySpell1, row = 7, sticky = 'we')

            tkgrid(frLastest, row = 8, sticky = 'we')

            ########

            tkbind(cb.method, "<<ComboboxSelected>>", function(){
                tkdestroy(frEvapo)
                tkdestroy(frMinDays)
                tkdestroy(frDrySpell)
                tkdestroy(frDrySpell1)

                if(str_trim(tclvalue(onset.method)) == '2'){
                    frEvapo <<- tkframe(frameOnset)
                    txt.evapo1 <- tklabel(frEvapo, text = lang.dlg[['label']][['22']], anchor = 'w', justify = 'left')
                    en.evapo <- tkentry(frEvapo, textvariable = evapo.frac, width = 5)
                    txt.evapo2 <- tklabel(frEvapo, text = lang.dlg[['label']][['23']], anchor = 'w', justify = 'left')
                    tkgrid(txt.evapo1, en.evapo, txt.evapo2)

                    tkgrid(frEvapo, row = 4, sticky = 'we')
                }
                if(str_trim(tclvalue(onset.method)) %in% c('3', '4', '5')){
                    if(str_trim(tclvalue(onset.method)) %in% c('3', '5')){
                        frMinDays <<- tkframe(frameOnset)
                        txt.minday1 <- tklabel(frMinDays, text = lang.dlg[['label']][['24']], anchor = 'w', justify = 'left')
                        en.minday <- tkentry(frMinDays, textvariable = min.rain.day, width = 4)
                        txt.minday2 <- tklabel(frMinDays, text = lang.dlg[['label']][['25']], anchor = 'w', justify = 'left')
                        tkgrid(txt.minday1, en.minday, txt.minday2)

                        tkgrid(frMinDays, row = 5, sticky = 'we')
                    }
                    if(str_trim(tclvalue(onset.method)) %in% c('4', '5')){
                        frDrySpell <<- tkframe(frameOnset)
                        txt.dryspl1 <- tklabel(frDrySpell, text = lang.dlg[['label']][['26']], anchor = 'w', justify = 'left')
                        en.dryspl <- tkentry(frDrySpell, textvariable = dryspell, width = 4)
                        txt.dryspl2 <- tklabel(frDrySpell, text = lang.dlg[['label']][['21']], anchor = 'w', justify = 'left')
                        tkgrid(txt.dryspl1, en.dryspl, txt.dryspl2)

                        frDrySpell1 <<- tkframe(frameOnset)
                        txt.dryspld1 <- tklabel(frDrySpell1, text = lang.dlg[['label']][['27']], anchor = 'w', justify = 'left')
                        en.dryspld <- tkentry(frDrySpell1, textvariable = dryspell.days, width = 4)
                        txt.dryspld2 <- tklabel(frDrySpell1, text = lang.dlg[['label']][['21']], anchor = 'w', justify = 'left')
                        tkgrid(txt.dryspld1, en.dryspld, txt.dryspld2)

                        tkgrid(frDrySpell, row = 6, sticky = 'we')
                        tkgrid(frDrySpell1, row = 7, sticky = 'we')
                    }
                }
            })

            ########
            tkgrid(frameOnset, pady = 5)
            tcl('update')

            ########
            return(list(frame = frameOnset,
                        onset.def = list(method = onset.method,
                                        total.days = total.days, rain.total = total.rain,
                                        thres.rain.day = thres.rain.day, min.rain.day = min.rain.day,
                                        dryspell = dryspell, dryspell.days = dryspell.days,
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
            stateSubdv <- if(str_trim(tclvalue(onset.region)) == CbregionDIV[1]) "disabled" else "normal"
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
        lat.subdiv <- vector('list', length = GeneralParameters$onset.reg$lat$nb - 1)
        for(i in seq_along(lat.subdiv))
            lat.subdiv[[i]] <- tclVar(GeneralParameters$onset.reg$lat$div[[i]])

        innerRegDef <- tkframe(frameRegDef)
        OnsetDefinitionInput()
        tcl('update')

        ##############################################

        bt.CalcOnset <- ttkbutton(subfr2, text = lang.dlg[['button']][['3']])

        tkconfigure(bt.CalcOnset, command = function(){
            GeneralParameters$data.type <- datatypeVAL[CbdatatypeVAL %in% str_trim(tclvalue(DataType))]

            if(str_trim(tclvalue(DataType)) == CbdatatypeVAL[1]){
                GeneralParameters$cdtstation$prec <- str_trim(tclvalue(input.Prec))
                GeneralParameters$cdtstation$etp <- str_trim(tclvalue(input.Etp))
            }

            if(str_trim(tclvalue(DataType)) == CbdatatypeVAL[2]){
                GeneralParameters$cdtdataset$prec <- str_trim(tclvalue(input.Prec))
                GeneralParameters$cdtdataset$etp <- str_trim(tclvalue(input.Etp))
            }

            GeneralParameters$output <- str_trim(tclvalue(dir.save))

            GeneralParameters$onset.reg$region <- str_trim(tclvalue(onset.region))
            GeneralParameters$onset.reg$subdiv <- str_trim(tclvalue(onset.subdiv))

            if(str_trim(tclvalue(onset.region)) == CbregionDIV[2]){
                if(str_trim(tclvalue(onset.subdiv)) == CbregionSUBDIV[1]){
                    GeneralParameters$onset.reg$lat$nb <- as.numeric(str_trim(tclvalue(lat.nbdiv)))
                    GeneralParameters$onset.reg$lat$div <- lapply(lat.subdiv, function(x) as.numeric(str_trim(tclvalue(x))))
                }

                if(str_trim(tclvalue(onset.subdiv)) == CbregionSUBDIV[2]){
                    GeneralParameters$onset.reg$shp$file <- str_trim(tclvalue(shp.file))
                    GeneralParameters$onset.reg$shp$attr <- str_trim(tclvalue(shp.attr))
                }
            }

            GeneralParameters$onset.criteria <- lapply(ONSET.vars, function(x){
                                                        x <- x$onset.def
                                                        list(
                                                            method = as.numeric(str_trim(tclvalue(x$method))),
                                                            total.days = as.numeric(str_trim(tclvalue(x$total.days))),
                                                            rain.total = as.numeric(str_trim(tclvalue(x$rain.total))),
                                                            thres.rain.day = as.numeric(str_trim(tclvalue(x$thres.rain.day))),
                                                            min.rain.day = as.numeric(str_trim(tclvalue(x$min.rain.day))),
                                                            dryspell = as.numeric(str_trim(tclvalue(x$dryspell))),
                                                            dryspell.days = as.numeric(str_trim(tclvalue(x$dryspell.days))),
                                                            evapo.frac = as.numeric(str_trim(tclvalue(x$evapo.frac))),
                                                            earliest = list(month = which(MOIS %in% str_trim(tclvalue(x$earliest$month))),
                                                                            day = as.numeric(str_trim(tclvalue(x$earliest$day)))),
                                                            latest = list(month = which(MOIS %in% str_trim(tclvalue(x$latest$month))),
                                                                            day = as.numeric(str_trim(tclvalue(x$latest$day))))
                                                        )
                                                    })

            nbcriteria <- length(unlist(GeneralParameters$onset.def)) * length(GeneralParameters$onset.criteria)
            criteria <- unlist(GeneralParameters$onset.criteria)
            if(any(is.na(criteria)) | length(criteria) != nbcriteria){
                Insert.Messages.Out(lang.dlg[['message']][['7']], TRUE, 'e')
                return(NULL)
            }

            etp.data <- FALSE
            omethods <- sapply(GeneralParameters$onset.criteria, "[[", "method")
            if(any(omethods == 2)){
                if(GeneralParameters$data.type == 'cdtstation'){
                    if(GeneralParameters$cdtstation$etp == "") etp.data <- TRUE
                }else{
                    if(GeneralParameters$cdtdataset$etp == "") etp.data <- TRUE
                }
            }

            if(etp.data){
                tkmessageBox(message = lang.dlg[['message']][['15']], icon = "warning", type = "ok")
                tkconfigure(cb.en.INEtp, state = 'normal')
                tkconfigure(bt.INEtp, state = 'normal')
                return(NULL)
            }else{
                tkconfigure(cb.en.INEtp, state = 'disabled')
                tkconfigure(bt.INEtp, state = 'disabled')
            }

            # assign("GeneralParameters", GeneralParameters, envir = .GlobalEnv)

            Insert.Messages.Out(lang.dlg[['message']][['8']], TRUE, "i")

            tkconfigure(.cdtEnv$tcl$main$win, cursor = 'watch')
            tcl('update')
            ret <- tryCatch({
                                compute_SeasonOnset_Procs(GeneralParameters)
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
                    Insert.Messages.Out(lang.dlg[['message']][['9']], TRUE, "s")

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

            if(file.exists(str_trim(tclvalue(file.dataIndex)))){
                OutIndexdata <- try(readRDS(str_trim(tclvalue(file.dataIndex))), silent = TRUE)
                if(inherits(OutIndexdata, "try-error")){
                    Insert.Messages.Out(lang.dlg[['message']][['11']], TRUE, 'e')
                    Insert.Messages.Out(gsub('[\r\n]', '', OutIndexdata[1]), TRUE, 'e')
                    tkconfigure(cb.data.Index, values = "")
                    tclvalue(.cdtData$EnvData$donDate) <- ""
                    return(NULL)
                }

                .cdtData$EnvData$output <- OutIndexdata
                .cdtData$EnvData$PathData <- dirname(str_trim(tclvalue(file.dataIndex)))
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

            if(str_trim(tclvalue(.cdtData$EnvData$plot.maps$plot.type)) == "Points")
                .cdtData$EnvData$tab$pointSize <- .cdtData$EnvData$dataMapOp$pointSize
        })

        #########
        .cdtData$EnvData$tab$dataMap <- NULL

        tkconfigure(bt.data.maps, command = function(){
            if(str_trim(tclvalue(.cdtData$EnvData$donDate)) != "" &
                !is.null(.cdtData$EnvData$varData))
                    OnsetCalc.Display.Maps()
        })

        tkconfigure(bt.data.Index.prev, command = function(){
            if(str_trim(tclvalue(.cdtData$EnvData$donDate)) != ""){
                donDates <- format(.cdtData$EnvData$output$start.date, "%Y")
                idaty <- which(donDates == str_trim(tclvalue(.cdtData$EnvData$donDate)))
                idaty <- idaty - 1
                if(idaty < 1) idaty <- length(donDates)
                tclvalue(.cdtData$EnvData$donDate) <- donDates[idaty]

                ret <- try(read.Data.Map(), silent = TRUE)
                if(inherits(ret, "try-error") | is.null(ret)) return(NULL)

                OnsetCalc.Display.Maps()
            }
        })

        tkconfigure(bt.data.Index.next, command = function(){
            if(str_trim(tclvalue(.cdtData$EnvData$donDate)) != ""){
                donDates <- format(.cdtData$EnvData$output$start.date, "%Y")
                idaty <- which(donDates == str_trim(tclvalue(.cdtData$EnvData$donDate)))
                idaty <- idaty + 1
                if(idaty > length(donDates)) idaty <- 1
                tclvalue(.cdtData$EnvData$donDate) <- donDates[idaty]

                ret <- try(read.Data.Map(), silent = TRUE)
                if(inherits(ret, "try-error") | is.null(ret)) return(NULL)

                OnsetCalc.Display.Maps()
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
            suffix.fun <- switch(str_trim(tclvalue(.cdtData$EnvData$plot.maps$typeTSp)),
                                    "Barplot" = "Bar",
                                    "Line" = "Line")
            plot.fun <- get(paste0("MapGraph.GraphOptions.", suffix.fun), mode = "function")
            .cdtData$EnvData$TSGraphOp <- plot.fun(.cdtData$EnvData$TSGraphOp)
        })

        #########
        .cdtData$EnvData$tab$dataGraph <- NULL

        tkconfigure(bt.TsGraph.plot, command = function(){
            if(!is.null(.cdtData$EnvData$varData)){
                imgContainer <- CDT.Display.Graph(OnsetCalc.plotOnsetGraph, .cdtData$EnvData$tab$dataGraph, 'Onset-Graph')
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
                    istn <- which(stnIDTSPLOT == str_trim(tclvalue(.cdtData$EnvData$plot.maps$stnIDTSp)))
                    istn <- istn - 1
                    if(istn < 1) istn <- length(stnIDTSPLOT)
                    tclvalue(.cdtData$EnvData$plot.maps$stnIDTSp) <- stnIDTSPLOT[istn]

                    imgContainer <- CDT.Display.Graph(OnsetCalc.plotOnsetGraph, .cdtData$EnvData$tab$dataGraph, 'Onset-Graph')
                    .cdtData$EnvData$tab$dataGraph <- imageNotebookTab_unik(imgContainer, .cdtData$EnvData$tab$dataGraph)
                }
            })

            tkconfigure(bt.stnID.next, command = function(){
                if(!is.null(.cdtData$EnvData$varData)){
                    istn <- which(stnIDTSPLOT == str_trim(tclvalue(.cdtData$EnvData$plot.maps$stnIDTSp)))
                    istn <- istn + 1
                    if(istn > length(stnIDTSPLOT)) istn <- 1
                    tclvalue(.cdtData$EnvData$plot.maps$stnIDTSp) <- stnIDTSPLOT[istn]

                    imgContainer <- CDT.Display.Graph(OnsetCalc.plotOnsetGraph, .cdtData$EnvData$tab$dataGraph, 'Onset-Graph')
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

        this.daty <- str_trim(tclvalue(.cdtData$EnvData$donDate))
        idt <- which(format(.cdtData$EnvData$output$start.date, "%Y") == this.daty)

        if(.cdtData$EnvData$output$params$data.type == "cdtstation"){
            filePathData <- file.path(.cdtData$EnvData$PathData, "CDTDATASET/ONSET.rds")
            if(!file.exists(filePathData)){
                Insert.Messages.Out(paste(filePathData, lang.dlg[['message']][['12']]), TRUE, 'e')
                return(NULL)
            }

            change.plot <- str_trim(tclvalue(.cdtData$EnvData$plot.maps$plot.type))

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
                            paste0("onset_", format(.cdtData$EnvData$output$start.date[idt], "%Y%m%d"), ".nc"))
            if(!file.exists(filePathData)){
                Insert.Messages.Out(paste(filePathData, lang.dlg[['message']][['12']]), TRUE, 'e')
                return(NULL)
            }

            readVarData <- TRUE
            if(!is.null(.cdtData$EnvData$varData))
                if(!is.null(.cdtData$EnvData$filePathData))
                    if(.cdtData$EnvData$filePathData == filePathData) readVarData <- FALSE

            if(readVarData){
                nc <- nc_open(filePathData)
                .cdtData$EnvData$varData$map$x <- nc$dim[[1]]$vals
                .cdtData$EnvData$varData$map$y <- nc$dim[[2]]$vals
                .cdtData$EnvData$varData$map$z <- ncvar_get(nc, varid = nc$var[[1]]$name)
                nc_close(nc)
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
