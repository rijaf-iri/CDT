
computePET_getParams <- function(){
    listOpenFiles <- openFile_ttkcomboList()
    if(WindowsOS()){
        largeur0 <- 14
        largeur1 <- 18
        largeur2 <- 51
        largeur3 <- 49
        largeur4 <- 33
    }else{
        largeur0 <- 14
        largeur1 <- 18
        largeur2 <- 47
        largeur3 <- 46
        largeur4 <- 30
    }

    xml.dlg <- file.path(.cdtDir$dirLocal, "languages", "cdtCompute_PET_dlgBox.xml")
    lang.dlg <- cdtLanguageParse(xml.dlg, .cdtData$Config$lang.iso)

    ############################################

    tt <- tktoplevel()
    tkgrab.set(tt)
    tkfocus(tt)

    frMRG0 <- tkframe(tt, relief = 'raised', borderwidth = 2)
    frMRG1 <- tkframe(tt)
    frMain <- tkframe(frMRG0, relief = "groove", borderwidth = 2)

    ############################################

    inputDataFun <- function(data.type, pet.method){
        tkdestroy(frameInData)
        frameInData <<- tkframe(frameData)

        #######

        if(data.type == 'cdtstation'){
            txt.INTmin <- lang.dlg[['label']][['4']]
            txt.INTmax <- lang.dlg[['label']][['5']]
            txt.INPrec <- lang.dlg[['label']][['6']]
            stateSetNC <- "disabled"
            stateSetNC1 <- "disabled"
        }else if(data.type == 'cdtdataset'){
            input.Tmin <- tclVar(.cdtData$GalParams$cdtdataset$tmin)
            input.Tmax <- tclVar(.cdtData$GalParams$cdtdataset$tmax)
            input.Prec <- tclVar(.cdtData$GalParams$cdtdataset$prec)
            txt.INTmin <- lang.dlg[['label']][['7']]
            txt.INTmax <- lang.dlg[['label']][['8']]
            txt.INPrec <- lang.dlg[['label']][['9']]
            stateSetNC <- "disabled"
            stateSetNC1 <- "disabled"
        }else{
            input.Tmin <- tclVar(.cdtData$GalParams$cdtnetcdf$tmin$dir)
            input.Tmax <- tclVar(.cdtData$GalParams$cdtnetcdf$tmax$dir)
            input.Prec <- tclVar(.cdtData$GalParams$cdtnetcdf$prec$dir)
            txt.INTmin <- lang.dlg[['label']][['10']]
            txt.INTmax <- lang.dlg[['label']][['11']]
            txt.INPrec <- lang.dlg[['label']][['12']]
            stateSetNC <- "normal"
            stateSetNC1 <- if(pet.method == "MHAR") "normal" else "disabled"
        }
        txt.INTmin.var <- tclVar(txt.INTmin)
        txt.INTmax.var <- tclVar(txt.INTmax)
        txt.INPrec.var <- tclVar(txt.INPrec)

        statePrecip <- if(pet.method == "MHAR") "normal" else "disabled"

        ##############
        txt.tmin <- tklabel(frameInData, text = tclvalue(txt.INTmin.var), textvariable = txt.INTmin.var, anchor = 'w', justify = 'left')
        set.tmin <- tkbutton(frameInData, text = .cdtEnv$tcl$lang$global[['button']][['5']], state = stateSetNC)
        txt.tmax <- tklabel(frameInData, text = tclvalue(txt.INTmax.var), textvariable = txt.INTmax.var, anchor = 'w', justify = 'left')
        set.tmax <- tkbutton(frameInData, text = .cdtEnv$tcl$lang$global[['button']][['5']], state = stateSetNC)
        txt.prec <- tklabel(frameInData, text = tclvalue(txt.INPrec.var), textvariable = txt.INPrec.var, anchor = 'w', justify = 'left')
        set.prec <<- tkbutton(frameInData, text = .cdtEnv$tcl$lang$global[['button']][['5']], state = stateSetNC1)

        if(data.type == 'cdtstation'){
            cb.en.tmin <- ttkcombobox(frameInData, values = unlist(listOpenFiles), textvariable = input.Tmin, width = largeur3)
            cb.en.tmax <- ttkcombobox(frameInData, values = unlist(listOpenFiles), textvariable = input.Tmax, width = largeur3)
            cb.en.prec <<- ttkcombobox(frameInData, values = unlist(listOpenFiles), textvariable = input.Prec, width = largeur3, state = statePrecip)
        }else{
            cb.en.tmin <- tkentry(frameInData, textvariable = input.Tmin, width = largeur2)
            cb.en.tmax <- tkentry(frameInData, textvariable = input.Tmax, width = largeur2)
            cb.en.prec <<- tkentry(frameInData, textvariable = input.Prec, width = largeur2, state = statePrecip)
        }
        bt.tmin <- tkbutton(frameInData, text = "...")
        bt.tmax <- tkbutton(frameInData, text = "...")
        bt.prec <<- tkbutton(frameInData, text = "...", state = statePrecip)

        ##############

        tkgrid(txt.tmin, row = 0, column = 0, sticky = 'we', rowspan = 1, columnspan = 3, padx = 1, pady = 0, ipadx = 1, ipady = 1)
        tkgrid(set.tmin, row = 0, column = 3, sticky = 'we', rowspan = 1, columnspan = 2, padx = 1, pady = 0, ipadx = 1, ipady = 1)
        tkgrid(cb.en.tmin, row = 1, column = 0, sticky = 'we', rowspan = 1, columnspan = 4, padx = 0, pady = 0, ipadx = 1, ipady = 1)
        tkgrid(bt.tmin, row = 1, column = 4, sticky = 'w', rowspan = 1, columnspan = 1, padx = 0, pady = 0, ipadx = 1, ipady = 1)

        tkgrid(txt.tmax, row = 2, column = 0, sticky = 'we', rowspan = 1, columnspan = 3, padx = 1, pady = 0, ipadx = 1, ipady = 1)
        tkgrid(set.tmax, row = 2, column = 3, sticky = 'we', rowspan = 1, columnspan = 2, padx = 1, pady = 0, ipadx = 1, ipady = 1)
        tkgrid(cb.en.tmax, row = 3, column = 0, sticky = 'we', rowspan = 1, columnspan = 4, padx = 0, pady = 0, ipadx = 1, ipady = 1)
        tkgrid(bt.tmax, row = 3, column = 4, sticky = 'w', rowspan = 1, columnspan = 1, padx = 0, pady = 0, ipadx = 1, ipady = 1)

        tkgrid(txt.prec, row = 4, column = 0, sticky = 'we', rowspan = 1, columnspan = 3, padx = 1, pady = 0, ipadx = 1, ipady = 1)
        tkgrid(set.prec, row = 4, column = 3, sticky = 'we', rowspan = 1, columnspan = 2, padx = 1, pady = 0, ipadx = 1, ipady = 1)
        tkgrid(cb.en.prec, row = 5, column = 0, sticky = 'we', rowspan = 1, columnspan = 4, padx = 0, pady = 0, ipadx = 1, ipady = 1)
        tkgrid(bt.prec, row = 5, column = 4, sticky = 'w', rowspan = 1, columnspan = 1, padx = 0, pady = 0, ipadx = 1, ipady = 1)

        #############
        if(data.type == 'cdtstation'){
            helpWidget(cb.en.tmin, lang.dlg[['tooltip']][['4']], lang.dlg[['status']][['4']])
            helpWidget(cb.en.tmax, lang.dlg[['tooltip']][['5']], lang.dlg[['status']][['5']])
            helpWidget(cb.en.prec, lang.dlg[['tooltip']][['6']], lang.dlg[['status']][['6']])
            helpWidget(bt.tmin, lang.dlg[['tooltip']][['7']], lang.dlg[['status']][['7']])
            helpWidget(bt.tmax, lang.dlg[['tooltip']][['7']], lang.dlg[['status']][['7']])
            helpWidget(bt.prec, lang.dlg[['tooltip']][['7']], lang.dlg[['status']][['7']])
        }else if(data.type == 'cdtdataset'){
            helpWidget(cb.en.tmin, lang.dlg[['tooltip']][['9']], lang.dlg[['status']][['9']])
            helpWidget(cb.en.tmax, lang.dlg[['tooltip']][['10']], lang.dlg[['status']][['10']])
            helpWidget(cb.en.prec, lang.dlg[['tooltip']][['11']], lang.dlg[['status']][['11']])
            helpWidget(bt.tmin, lang.dlg[['tooltip']][['8']], lang.dlg[['status']][['8']])
            helpWidget(bt.tmax, lang.dlg[['tooltip']][['8']], lang.dlg[['status']][['8']])
            helpWidget(bt.prec, lang.dlg[['tooltip']][['8']], lang.dlg[['status']][['8']])
        }else{
            helpWidget(cb.en.tmin, lang.dlg[['tooltip']][['12']], lang.dlg[['status']][['12']])
            helpWidget(cb.en.tmax, lang.dlg[['tooltip']][['13']], lang.dlg[['status']][['13']])
            helpWidget(cb.en.prec, lang.dlg[['tooltip']][['14']], lang.dlg[['status']][['14']])
            helpWidget(bt.tmin, lang.dlg[['tooltip']][['8']], lang.dlg[['status']][['8']])
            helpWidget(bt.tmax, lang.dlg[['tooltip']][['8']], lang.dlg[['status']][['8']])
            helpWidget(bt.prec, lang.dlg[['tooltip']][['8']], lang.dlg[['status']][['8']])
        }

        ############

        tkconfigure(set.tmin, command = function(){
            tcl('wm', 'attributes', tt, topmost = FALSE)
            .cdtData$GalParams$cdtnetcdf[["tmin"]] <- getInfoNetcdfData(tt, .cdtData$GalParams$cdtnetcdf[["tmin"]],
                                                                        str_trim(tclvalue(input.Tmin)),
                                                                        tclvalue(timeSteps))
            settingTmin <<- 1
            tcl('wm', 'attributes', tt, topmost = TRUE)
        })

        tkconfigure(bt.tmin, command = function(){
            if(data.type == 'cdtstation'){
                tcl('wm', 'attributes', tt, topmost = FALSE)
                dat.opfiles <- getOpenFiles(tt)
                tcl('wm', 'attributes', tt, topmost = TRUE)
                if(!is.null(dat.opfiles)){
                    update.OpenFiles('ascii', dat.opfiles)
                    listOpenFiles[[length(listOpenFiles) + 1]] <<- dat.opfiles[[1]]
                    tclvalue(input.Tmin) <- dat.opfiles[[1]]
                    lapply(list(cb.en.tmin, cb.en.tmax, cb.en.prec), tkconfigure, values = unlist(listOpenFiles))
                }
            }else if(data.type == 'cdtdataset'){
                tcl('wm', 'attributes', tt, topmost = FALSE)
                path.rds <- tclvalue(tkgetOpenFile(initialdir = getwd(), filetypes = .cdtEnv$tcl$data$filetypes6))
                tcl('wm', 'attributes', tt, topmost = TRUE)
                tclvalue(input.Tmin) <- if(path.rds %in% c("", "NA") | is.na(path.rds)) "" else path.rds
            }else{
                tcl('wm', 'attributes', tt, topmost = FALSE)
                dirnc <- tk_choose.dir(getwd(), "")
                tcl('wm', 'attributes', tt, topmost = TRUE)
                tclvalue(input.Tmin) <- if(dirnc %in% c("", "NA") | is.na(dirnc)) "" else dirnc
            }
        })

        ############

        tkconfigure(set.tmax, command = function(){
            tcl('wm', 'attributes', tt, topmost = FALSE)
            .cdtData$GalParams$cdtnetcdf[["tmax"]] <- getInfoNetcdfData(tt, .cdtData$GalParams$cdtnetcdf[["tmax"]],
                                                                        str_trim(tclvalue(input.Tmax)),
                                                                        tclvalue(timeSteps))
            settingTmax <<- 1
            tcl('wm', 'attributes', tt, topmost = TRUE)
        })

        tkconfigure(bt.tmax, command = function(){
            if(data.type == 'cdtstation'){
                tcl('wm', 'attributes', tt, topmost = FALSE)
                dat.opfiles <- getOpenFiles(tt)
                tcl('wm', 'attributes', tt, topmost = TRUE)
                if(!is.null(dat.opfiles)){
                    update.OpenFiles('ascii', dat.opfiles)
                    listOpenFiles[[length(listOpenFiles) + 1]] <<- dat.opfiles[[1]]
                    tclvalue(input.Tmax) <- dat.opfiles[[1]]
                    lapply(list(cb.en.tmin, cb.en.tmax, cb.en.prec), tkconfigure, values = unlist(listOpenFiles))
                }
            }else if(data.type == 'cdtdataset'){
                tcl('wm', 'attributes', tt, topmost = FALSE)
                path.rds <- tclvalue(tkgetOpenFile(initialdir = getwd(), filetypes = .cdtEnv$tcl$data$filetypes6))
                tcl('wm', 'attributes', tt, topmost = TRUE)
                tclvalue(input.Tmax) <- if(path.rds %in% c("", "NA") | is.na(path.rds)) "" else path.rds
            }else{
                tcl('wm', 'attributes', tt, topmost = FALSE)
                dirnc <- tk_choose.dir(getwd(), "")
                tcl('wm', 'attributes', tt, topmost = TRUE)
                tclvalue(input.Tmax) <- if(dirnc %in% c("", "NA") | is.na(dirnc)) "" else dirnc
            }
        })

        ############

        tkconfigure(set.prec, command = function(){
            tcl('wm', 'attributes', tt, topmost = FALSE)
            .cdtData$GalParams$cdtnetcdf[["prec"]] <- getInfoNetcdfData(tt, .cdtData$GalParams$cdtnetcdf[["prec"]],
                                                                        str_trim(tclvalue(input.Prec)),
                                                                        tclvalue(timeSteps))
            settingPrec <<- 1
            tcl('wm', 'attributes', tt, topmost = TRUE)
        })

        tkconfigure(bt.prec, command = function(){
            if(data.type == 'cdtstation'){
                tcl('wm', 'attributes', tt, topmost = FALSE)
                dat.opfiles <- getOpenFiles(tt)
                tcl('wm', 'attributes', tt, topmost = TRUE)
                if(!is.null(dat.opfiles)){
                    update.OpenFiles('ascii', dat.opfiles)
                    listOpenFiles[[length(listOpenFiles) + 1]] <<- dat.opfiles[[1]]
                    tclvalue(input.Prec) <- dat.opfiles[[1]]
                    lapply(list(cb.en.tmin, cb.en.tmax, cb.en.prec), tkconfigure, values = unlist(listOpenFiles))
                }
            }else if(data.type == 'cdtdataset'){
                tcl('wm', 'attributes', tt, topmost = FALSE)
                path.rds <- tclvalue(tkgetOpenFile(initialdir = getwd(), filetypes = .cdtEnv$tcl$data$filetypes6))
                tcl('wm', 'attributes', tt, topmost = TRUE)
                tclvalue(input.Prec) <- if(path.rds %in% c("", "NA") | is.na(path.rds)) "" else path.rds
            }else{
                tcl('wm', 'attributes', tt, topmost = FALSE)
                dirnc <- tk_choose.dir(getwd(), "")
                tcl('wm', 'attributes', tt, topmost = TRUE)
                tclvalue(input.Prec) <- if(dirnc %in% c("", "NA") | is.na(dirnc)) "" else dirnc
            }
        })

        ############
        tkgrid(frameInData)
    }

    ######################

    saveFun <- function(data.type){
        tkdestroy(frSaveIn)
        frSaveIn <<- tkframe(frSave)

        #########
        if(data.type == 'cdtstation'){
            txtSaveDir <- lang.dlg[['label']][['13']]
            isFile <- TRUE
        }else{
            txtSaveDir <- lang.dlg[['label']][['14']]
            isFile <- FALSE
        }
        fileORdir <- tclVar(txtSaveDir)

        txt.file.save <- tklabel(frSaveIn, text = tclvalue(fileORdir), textvariable = fileORdir, anchor = 'w', justify = 'left')
        en.file.save <- tkentry(frSaveIn, textvariable = file.save, width = largeur2)
        bt.file.save <- tkbutton(frSaveIn, text = "...")

        #########
        tkconfigure(bt.file.save, command = function(){
            tcl('wm', 'attributes', tt, topmost = FALSE)
            fileORdir2Save(file.save, isFile = isFile)
            tcl('wm', 'attributes', tt, topmost = TRUE)
        })

        #########
        tkgrid(txt.file.save, row = 0, column = 0, sticky = 'we', rowspan = 1, columnspan = 5, padx = 1, pady = 0, ipadx = 1, ipady = 1)
        tkgrid(en.file.save, row = 1, column = 0, sticky = 'we', rowspan = 1, columnspan = 4, padx = 0, pady = 0, ipadx = 1, ipady = 1)
        tkgrid(bt.file.save, row = 1, column = 4, sticky = 'w', rowspan = 1, columnspan = 1, padx = 0, pady = 0, ipadx = 1, ipady = 1)

        #########
        isHlp <- if(data.type == 'cdtstation') '15' else '16'

        helpWidget(en.file.save, lang.dlg[['tooltip']][[isHlp]], lang.dlg[['status']][[isHlp]])
        helpWidget(bt.file.save, lang.dlg[['tooltip']][['8']], lang.dlg[['status']][['8']])

        #########
        tkgrid(frSaveIn)
    }

    ############################################

    frtimestep <- tkframe(frMain, relief = 'sunken', borderwidth = 2)

    timeSteps <- tclVar()
    cb.periodVAL <- .cdtEnv$tcl$lang$global[['combobox']][['1']][3:6]
    periodVAL <- c('daily', 'pentad', 'dekadal', 'monthly')
    tclvalue(timeSteps) <- cb.periodVAL[periodVAL %in% .cdtData$GalParams$Tstep]

    ref.evap <- tclVar()
    cb.evapVAL <- lang.dlg[['combobox']][['1']]
    evapVAL <- c('HAR', 'MHAR')
    tclvalue(ref.evap) <- cb.evapVAL[evapVAL %in% .cdtData$GalParams$method]

    txt.period <- tklabel(frtimestep, text = lang.dlg[['label']][['1']], anchor = 'e', justify = 'right')
    cb.period <- ttkcombobox(frtimestep, values = cb.periodVAL, textvariable = timeSteps, width = largeur0)
    txt.evap <- tklabel(frtimestep, text = lang.dlg[['label']][['2']], anchor = 'e', justify = 'right')
    cb.evap <- ttkcombobox(frtimestep, values = cb.evapVAL, textvariable = ref.evap, width = largeur1)

    tkgrid(txt.period, row = 0, column = 0, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 5, ipadx = 1, ipady = 1)
    tkgrid(cb.period, row = 0, column = 1, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 5, ipadx = 1, ipady = 1)
    tkgrid(txt.evap, row = 0, column = 2, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 5, ipadx = 1, ipady = 1)
    tkgrid(cb.evap, row = 0, column = 3, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 5, ipadx = 1, ipady = 1)

    helpWidget(cb.period, lang.dlg[['tooltip']][['1']], lang.dlg[['status']][['1']])
    helpWidget(cb.evap, lang.dlg[['tooltip']][['2']], lang.dlg[['status']][['2']])

    ############

    tkbind(cb.evap, "<<ComboboxSelected>>", function(){
        pet.method <- evapVAL[cb.evapVAL %in% str_trim(tclvalue(ref.evap))]
        data.type <- datatypeVAL[CbdatatypeVAL %in% str_trim(tclvalue(DataType))]

        statePrecip <- if(pet.method == 'HAR') "disabled" else "normal"
        tkconfigure(cb.en.prec, state = statePrecip)
        tkconfigure(bt.prec, state = statePrecip)
        stateSetNC1 <- if(data.type == 'cdtnetcdf' & pet.method == 'MHAR') "normal" else "disabled"
        tkconfigure(set.prec, state = stateSetNC1)
    })

    ############################################

    frdatatype <- tkframe(frMain, relief = 'sunken', borderwidth = 2)

    DataType <- tclVar()
    CbdatatypeVAL <- .cdtEnv$tcl$lang$global[['combobox']][['2']][1:3]
    datatypeVAL <- c('cdtstation', 'cdtdataset', 'cdtnetcdf')
    tclvalue(DataType) <- CbdatatypeVAL[datatypeVAL %in% .cdtData$GalParams$data.type]

    txt.datatyp <- tklabel(frdatatype, text = lang.dlg[['label']][['3']], anchor = 'w', justify = 'left')
    cb.datatyp <- ttkcombobox(frdatatype, values = CbdatatypeVAL, textvariable = DataType, width = largeur4)

    tkgrid(txt.datatyp, row = 0, column = 0, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
    tkgrid(cb.datatyp, row = 1, column = 0, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)

    helpWidget(cb.datatyp, lang.dlg[['tooltip']][['3']], lang.dlg[['status']][['3']])

    ###############

    tkbind(cb.datatyp, "<<ComboboxSelected>>", function(){
        pet.method <- evapVAL[cb.evapVAL %in% str_trim(tclvalue(ref.evap))]
        data.type <- datatypeVAL[CbdatatypeVAL %in% str_trim(tclvalue(DataType))]

        inputDataFun(data.type, pet.method)
        saveFun(data.type)
        tkfocus(tt)
    })

    ############################################

    frameData <- tkframe(frMain, relief = 'sunken', borderwidth = 2)

    frameInData <- tkframe(frameData)

    if(.cdtData$GalParams$data.type == 'cdtstation'){
        input.Tmin <- tclVar(.cdtData$GalParams$cdtstation$tmin)
        input.Tmax <- tclVar(.cdtData$GalParams$cdtstation$tmax)
        input.Prec <- tclVar(.cdtData$GalParams$cdtstation$prec)
    }else if(.cdtData$GalParams$data.type == 'cdtdataset'){
        input.Tmin <- tclVar(.cdtData$GalParams$cdtdataset$tmin)
        input.Tmax <- tclVar(.cdtData$GalParams$cdtdataset$tmax)
        input.Prec <- tclVar(.cdtData$GalParams$cdtdataset$prec)
    }else{
        input.Tmin <- tclVar(.cdtData$GalParams$cdtnetcdf$tmin$dir)
        input.Tmax <- tclVar(.cdtData$GalParams$cdtnetcdf$tmax$dir)
        input.Prec <- tclVar(.cdtData$GalParams$cdtnetcdf$prec$dir)
    }

    settingTmin <- .cdtData$GalParams$settingTmin
    settingTmax <- .cdtData$GalParams$settingTmax
    settingPrec <- .cdtData$GalParams$settingPrec

    inputDataFun(.cdtData$GalParams$data.type, .cdtData$GalParams$method)

    ############################################

    frSave <- tkframe(frMain, relief = 'sunken', borderwidth = 2)

    frSaveIn <- tkframe(frSave)

    file.save <- tclVar(.cdtData$GalParams$output)

    saveFun(.cdtData$GalParams$data.type)

    ############################################
    tkgrid(frtimestep, row = 0, column = 0, sticky = 'we', padx = 1, pady = 1, ipadx = 1, ipady = 1)
    tkgrid(frdatatype, row = 1, column = 0, sticky = '', padx = 1, pady = 3, ipadx = 1, ipady = 1)
    tkgrid(frameData, row = 2, column = 0, sticky = 'we', padx = 1, pady = 3, ipadx = 1, ipady = 1)
    tkgrid(frSave, row = 3, column = 0, sticky = 'we', padx = 1, pady = 3, ipadx = 1, ipady = 1)

    ############################################
    
    tkgrid(frMain, row = 0, column = 0, sticky = 'news', padx = 5, pady = 1, ipadx = 1, ipady = 1)

    ############################################

    bt.prm.OK <- ttkbutton(frMRG1, text = .cdtEnv$tcl$lang$global[['button']][['1']])
    bt.prm.CA <- ttkbutton(frMRG1, text = .cdtEnv$tcl$lang$global[['button']][['2']])

    tkconfigure(bt.prm.OK, command = function(){
        .cdtData$GalParams$Tstep <- periodVAL[cb.periodVAL %in% str_trim(tclvalue(timeSteps))]
        .cdtData$GalParams$method <- evapVAL[cb.evapVAL %in% str_trim(tclvalue(ref.evap))]
        .cdtData$GalParams$data.type <- datatypeVAL[CbdatatypeVAL %in% str_trim(tclvalue(DataType))]

        if(str_trim(tclvalue(input.Tmin)) %in% c("", "NA")){
            cdt.tkmessageBox(tt, message = lang.dlg[['message']][['1']], icon = "warning", type = "ok")
            tkwait.window(tt)
        }else if(str_trim(tclvalue(input.Tmax)) %in% c("", "NA")){
            cdt.tkmessageBox(tt, message = lang.dlg[['message']][['2']], icon = "warning", type = "ok")
            tkwait.window(tt)
        }else if(str_trim(tclvalue(input.Prec)) %in% c("", "NA") &
                 .cdtData$GalParams$method == 'MHAR')
        {
            cdt.tkmessageBox(tt, message = lang.dlg[['message']][['3']], icon = "warning", type = "ok")
            tkwait.window(tt)
        }else if(tclvalue(file.save) %in% c("", "NA")){
            cdt.tkmessageBox(tt, message = lang.dlg[['message']][['4']], icon = "warning", type = "ok")
            tkwait.window(tt)
        }else if(.cdtData$GalParams$data.type == 'cdtnetcdf' &
                 is.null(settingTmin))
        {
            cdt.tkmessageBox(tt, message = lang.dlg[['message']][['5']], icon = "warning", type = "ok")
            tkwait.window(tt)
        }else if(.cdtData$GalParams$data.type == 'cdtnetcdf' &
                 is.null(settingTmax))
        {
            cdt.tkmessageBox(tt, message = lang.dlg[['message']][['6']], icon = "warning", type = "ok")
            tkwait.window(tt)
        }else if(.cdtData$GalParams$data.type == 'cdtnetcdf' &
                 .cdtData$GalParams$method == 'MHAR' & is.null(settingPrec))
        {
            cdt.tkmessageBox(tt, message = lang.dlg[['message']][['7']], icon = "warning", type = "ok")
            tkwait.window(tt)
        }else{
            if(.cdtData$GalParams$data.type == 'cdtstation'){
                .cdtData$GalParams$cdtstation$tmin <- str_trim(tclvalue(input.Tmin))
                .cdtData$GalParams$cdtstation$tmax <- str_trim(tclvalue(input.Tmax))
                if(.cdtData$GalParams$method == 'MHAR')
                    .cdtData$GalParams$cdtstation$prec <- str_trim(tclvalue(input.Prec))
            }

            if(.cdtData$GalParams$data.type == 'cdtdataset'){
                .cdtData$GalParams$cdtdataset$tmin <- str_trim(tclvalue(input.Tmin))
                .cdtData$GalParams$cdtdataset$tmax <- str_trim(tclvalue(input.Tmax))
                if(.cdtData$GalParams$method == 'MHAR')
                    .cdtData$GalParams$cdtdataset$prec <- str_trim(tclvalue(input.Prec))
            }

            if(.cdtData$GalParams$data.type == 'cdtnetcdf'){
                .cdtData$GalParams$cdtnetcdf$tmin$dir <- str_trim(tclvalue(input.Tmin))
                .cdtData$GalParams$cdtnetcdf$tmax$dir <- str_trim(tclvalue(input.Tmax))
                if(.cdtData$GalParams$method == 'MHAR')
                    .cdtData$GalParams$cdtnetcdf$prec$dir <- str_trim(tclvalue(input.Prec))
            }

            .cdtData$GalParams$output <- str_trim(tclvalue(file.save))

            .cdtData$GalParams$settingTmin <- settingTmin
            .cdtData$GalParams$settingTmax <- settingTmax
            .cdtData$GalParams$settingPrec <- settingPrec

            .cdtData$GalParams$message <- lang.dlg[['message']]

            tkgrab.release(tt)
            tkdestroy(tt)
            tkfocus(.cdtEnv$tcl$main$win)
        }
    })

    tkconfigure(bt.prm.CA, command = function(){
        tkgrab.release(tt)
        tkdestroy(tt)
        tkfocus(.cdtEnv$tcl$main$win)
    })

    tkgrid(bt.prm.OK, row = 0, column = 0, sticky = 'w', padx = 5, pady = 1, ipadx = 1, ipady = 1)
    tkgrid(bt.prm.CA, row = 0, column = 1, sticky = 'e', padx = 5, pady = 1, ipadx = 1, ipady = 1)

    ############################################

    tkgrid(frMRG0, row = 0, column = 0, sticky = 'nswe', rowspan = 1, columnspan = 2, padx = 1, pady = 1, ipadx = 1, ipady = 1)
    tkgrid(frMRG1, row = 1, column = 1, sticky = 'se', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)

    ############################3
    tkwm.withdraw(tt)
    tcl('update')
    tt.w <- as.integer(tkwinfo("reqwidth", tt))
    tt.h <- as.integer(tkwinfo("reqheight", tt))
    tt.x <- as.integer(.cdtEnv$tcl$data$width.scr*0.5 - tt.w*0.5)
    tt.y <- as.integer(.cdtEnv$tcl$data$height.scr*0.5 - tt.h*0.5)
    tkwm.geometry(tt, paste0('+', tt.x, '+', tt.y))
    tkwm.transient(tt)
    tkwm.title(tt, lang.dlg[['title']])
    tkwm.deiconify(tt)
    tcl('wm', 'attributes', tt, topmost = TRUE)

    tkfocus(tt)
    tkbind(tt, "<Destroy>", function(){
        tkgrab.release(tt)
        tkfocus(.cdtEnv$tcl$main$win)
    })
    tkwait.window(tt)
}
