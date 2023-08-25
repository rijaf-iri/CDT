
computeTvars_getParams <- function(){
    listOpenFiles <- openFile_ttkcomboList()
    if(WindowsOS()){
        largeur0 <- 14
        largeur1 <- 11
        largeur2 <- 46
        largeur3 <- 43
        largeur4 <- 33
    }else{
        largeur0 <- 14
        largeur1 <- 11
        largeur2 <- 40
        largeur3 <- 39
        largeur4 <- 30
    }

    xml.dlg <- file.path(.cdtDir$dirLocal, "languages", "cdtCompute_Tvars_dlgBox.xml")
    lang.dlg <- cdtLanguageParse(xml.dlg, .cdtData$Config$lang.iso)

    ############################################

    tt <- tktoplevel()
    tkgrab.set(tt)
    tkfocus(tt)

    frMRG0 <- tkframe(tt, relief = 'raised', borderwidth = 2)
    frMRG1 <- tkframe(tt)
    frMain <- tkframe(frMRG0, relief = "groove", borderwidth = 2)

    ############################################

    inputDataFun <- function(data.type){
        tkdestroy(frameInData)
        frameInData <<- tkframe(frameData)

        #######

        if(data.type == 'cdtstation'){
            txt.INTmin <- lang.dlg[['label']][['4']]
            txt.INTmax <- lang.dlg[['label']][['5']]
            stateSetNC <- "disabled"
        }else if(data.type == 'cdtdataset'){
            txt.INTmin <- lang.dlg[['label']][['6']]
            txt.INTmax <- lang.dlg[['label']][['7']]
            stateSetNC <- "disabled"
        }else{
            txt.INTmin <- lang.dlg[['label']][['8']]
            txt.INTmax <- lang.dlg[['label']][['9']]
            stateSetNC <- "normal"
        }
        txt.INTmin.var <- tclVar(txt.INTmin)
        txt.INTmax.var <- tclVar(txt.INTmax)

        #######

        txt.tmin <- tklabel(frameInData, text = tclvalue(txt.INTmin.var), textvariable = txt.INTmin.var, anchor = 'w', justify = 'left')
        set.tmin <- ttkbutton(frameInData, text = .cdtEnv$tcl$lang$global[['button']][['5']], state = stateSetNC)
        txt.tmax <- tklabel(frameInData, text = tclvalue(txt.INTmax.var), textvariable = txt.INTmax.var, anchor = 'w', justify = 'left')
        set.tmax <- ttkbutton(frameInData, text = .cdtEnv$tcl$lang$global[['button']][['5']], state = stateSetNC)

        if(data.type == 'cdtstation'){
            cb.en.tmin <- ttkcombobox(frameInData, values = unlist(listOpenFiles), textvariable = input.Tmin, width = largeur3)
            cb.en.tmax <- ttkcombobox(frameInData, values = unlist(listOpenFiles), textvariable = input.Tmax, width = largeur3)
        }else{
            cb.en.tmin <- tkentry(frameInData, textvariable = input.Tmin, width = largeur2)
            cb.en.tmax <- tkentry(frameInData, textvariable = input.Tmax, width = largeur2)
        }
        bt.tmin <- tkbutton(frameInData, text = "...")
        bt.tmax <- tkbutton(frameInData, text = "...")

        ############ 
        tkgrid(txt.tmin, row = 0, column = 0, sticky = 'we', rowspan = 1, columnspan = 3, padx = 1, pady = 0, ipadx = 1, ipady = 1)
        tkgrid(set.tmin, row = 0, column = 3, sticky = 'we', rowspan = 1, columnspan = 2, padx = 1, pady = 0, ipadx = 1, ipady = 1)
        tkgrid(cb.en.tmin, row = 1, column = 0, sticky = 'we', rowspan = 1, columnspan = 4, padx = 0, pady = 0, ipadx = 1, ipady = 1)
        tkgrid(bt.tmin, row = 1, column = 4, sticky = 'w', rowspan = 1, columnspan = 1, padx = 0, pady = 0, ipadx = 1, ipady = 1)

        tkgrid(txt.tmax, row = 2, column = 0, sticky = 'we', rowspan = 1, columnspan = 3, padx = 1, pady = 0, ipadx = 1, ipady = 1)
        tkgrid(set.tmax, row = 2, column = 3, sticky = 'we', rowspan = 1, columnspan = 2, padx = 1, pady = 0, ipadx = 1, ipady = 1)
        tkgrid(cb.en.tmax, row = 3, column = 0, sticky = 'we', rowspan = 1, columnspan = 4, padx = 0, pady = 0, ipadx = 1, ipady = 1)
        tkgrid(bt.tmax, row = 3, column = 4, sticky = 'w', rowspan = 1, columnspan = 1, padx = 0, pady = 0, ipadx = 1, ipady = 1)

        #############
        if(data.type == 'cdtstation'){
            helpWidget(cb.en.tmin, lang.dlg[['tooltip']][['4']], lang.dlg[['status']][['4']])
            helpWidget(cb.en.tmax, lang.dlg[['tooltip']][['5']], lang.dlg[['status']][['5']])
            helpWidget(bt.tmin, lang.dlg[['tooltip']][['6']], lang.dlg[['status']][['6']])
            helpWidget(bt.tmax, lang.dlg[['tooltip']][['6']], lang.dlg[['status']][['6']])
        }else if(data.type == 'cdtdataset'){
            helpWidget(cb.en.tmin, lang.dlg[['tooltip']][['8']], lang.dlg[['status']][['8']])
            helpWidget(cb.en.tmax, lang.dlg[['tooltip']][['9']], lang.dlg[['status']][['9']])
            helpWidget(bt.tmin, lang.dlg[['tooltip']][['7']], lang.dlg[['status']][['7']])
            helpWidget(bt.tmax, lang.dlg[['tooltip']][['7']], lang.dlg[['status']][['7']])
        }else{
            helpWidget(cb.en.tmin, lang.dlg[['tooltip']][['10']], lang.dlg[['status']][['10']])
            helpWidget(cb.en.tmax, lang.dlg[['tooltip']][['11']], lang.dlg[['status']][['11']])
            helpWidget(bt.tmin, lang.dlg[['tooltip']][['7']], lang.dlg[['status']][['7']])
            helpWidget(bt.tmax, lang.dlg[['tooltip']][['7']], lang.dlg[['status']][['7']])
        }

        ############

        tkconfigure(set.tmin, command = function(){
            tcl('wm', 'attributes', tt, topmost = FALSE)
            .cdtData$GalParams$cdtnetcdf[["tmin"]] <- getInfoNetCDFData(tt, .cdtData$GalParams$cdtnetcdf[["tmin"]],
                                                                        trimws(tclvalue(input.Tmin)))
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
                    lapply(list(cb.en.tmin, cb.en.tmax), tkconfigure, values = unlist(listOpenFiles))
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
            .cdtData$GalParams$cdtnetcdf[["tmax"]] <- getInfoNetCDFData(tt, .cdtData$GalParams$cdtnetcdf[["tmax"]],
                                                                        trimws(tclvalue(input.Tmax)))
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
                    lapply(list(cb.en.tmin, cb.en.tmax), tkconfigure, values = unlist(listOpenFiles))
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
        tkgrid(frameInData)
    }

    ######################

    saveFun <- function(data.type){
        tkdestroy(frSaveIn)
        frSaveIn <<- tkframe(frSave)

        #########
        if(data.type == 'cdtstation'){
            txtSaveDir <- lang.dlg[['label']][['10']]
            isFile <- TRUE
        }else{
            txtSaveDir <- lang.dlg[['label']][['11']]
            isFile <- FALSE
        }
        fileORdir <- tclVar(txtSaveDir)

        txt.file.save <- tklabel(frSaveIn, text = tclvalue(fileORdir), textvariable = fileORdir, anchor = 'w', justify = 'left')
        en.file.save <- tkentry(frSaveIn, textvariable = file.save, width = largeur2)
        bt.file.save <- tkbutton(frSaveIn, text = "...")

        #########
        tkgrid(txt.file.save, row = 0, column = 0, sticky = 'we', rowspan = 1, columnspan = 5, padx = 1, pady = 0, ipadx = 1, ipady = 1)
        tkgrid(en.file.save, row = 1, column = 0, sticky = 'we', rowspan = 1, columnspan = 4, padx = 0, pady = 0, ipadx = 1, ipady = 1)
        tkgrid(bt.file.save, row = 1, column = 4, sticky = 'w', rowspan = 1, columnspan = 1, padx = 0, pady = 0, ipadx = 1, ipady = 1)

        isHlp <- if(data.type == 'cdtstation') '12' else '13'

        helpWidget(en.file.save, lang.dlg[['tooltip']][[isHlp]], lang.dlg[['status']][[isHlp]])
        helpWidget(bt.file.save, lang.dlg[['tooltip']][['7']], lang.dlg[['status']][['7']])

        #########

        tkconfigure(bt.file.save, command = function(){
            tcl('wm', 'attributes', tt, topmost = FALSE)
            fileORdir2Save(file.save, isFile = isFile)
            tcl('wm', 'attributes', tt, topmost = TRUE)
        })

        #########
        tkgrid(frSaveIn)
    }

    ############################################

    frtimestep <- tkframe(frMain, relief = 'sunken', borderwidth = 2)

    timeSteps <- tclVar()
    CbperiodVAL <- .cdtEnv$tcl$lang$global[['combobox']][['1']][3:6]
    periodVAL <- c('daily', 'pentad', 'dekadal', 'monthly')
    tclvalue(timeSteps) <- CbperiodVAL[periodVAL %in% .cdtData$GalParams$Tstep]

    temp.variable <- tclVar()
    CbvarsVAL <- lang.dlg[['combobox']][['1']]
    varsVAL <- c('Mean', 'Range')
    tclvalue(temp.variable) <- CbvarsVAL[varsVAL %in% .cdtData$GalParams$variable]

    txt.period <- tklabel(frtimestep, text = lang.dlg[['label']][['1']], anchor = 'e', justify = 'right')
    cb.period <- ttkcombobox(frtimestep, values = CbperiodVAL, textvariable = timeSteps, width = largeur0)
    txt.variable <- tklabel(frtimestep, text = lang.dlg[['label']][['2']], anchor = 'e', justify = 'right')
    cb.variable <- ttkcombobox(frtimestep, values = CbvarsVAL, textvariable = temp.variable, width = largeur1)

    tkgrid(txt.period, row = 0, column = 0, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 5, ipadx = 1, ipady = 1)
    tkgrid(cb.period, row = 0, column = 1, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 5, ipadx = 1, ipady = 1)
    tkgrid(txt.variable, row = 0, column = 2, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 5, ipadx = 1, ipady = 1)
    tkgrid(cb.variable, row = 0, column = 3, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 5, ipadx = 1, ipady = 1)

    helpWidget(cb.period, lang.dlg[['tooltip']][['1']], lang.dlg[['status']][['1']])
    helpWidget(cb.variable, lang.dlg[['tooltip']][['2']], lang.dlg[['status']][['2']])

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
        data.type <- datatypeVAL[CbdatatypeVAL %in% trimws(tclvalue(DataType))]

        inputDataFun(data.type)
        saveFun(data.type)
        tkfocus(tt)
    })

    ############################################

    frameData <- tkframe(frMain, relief = 'sunken', borderwidth = 2)

    frameInData <- tkframe(frameData)

    if(.cdtData$GalParams$data.type == 'cdtstation'){
        input.Tmin <- tclVar(.cdtData$GalParams$cdtstation$tmin)
        input.Tmax <- tclVar(.cdtData$GalParams$cdtstation$tmax)
    }else if(.cdtData$GalParams$data.type == 'cdtdataset'){
        input.Tmin <- tclVar(.cdtData$GalParams$cdtdataset$tmin)
        input.Tmax <- tclVar(.cdtData$GalParams$cdtdataset$tmax)
    }else{
        input.Tmin <- tclVar(.cdtData$GalParams$cdtnetcdf$tmin$dir)
        input.Tmax <- tclVar(.cdtData$GalParams$cdtnetcdf$tmax$dir)
    }

    settingTmin <- .cdtData$GalParams$settingTmin
    settingTmax <- .cdtData$GalParams$settingTmax

    inputDataFun(.cdtData$GalParams$data.type)

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
        .cdtData$GalParams$Tstep <- periodVAL[CbperiodVAL %in% trimws(tclvalue(timeSteps))]
        .cdtData$GalParams$data.type <- datatypeVAL[CbdatatypeVAL %in% trimws(tclvalue(DataType))]
        .cdtData$GalParams$variable <- varsVAL[CbvarsVAL %in% trimws(tclvalue(temp.variable))]

        if(trimws(tclvalue(input.Tmin)) %in% c("", "NA")){
            cdt.tkmessageBox(tt, message = lang.dlg[['message']][['1']], icon = "warning", type = "ok")
            tkwait.window(tt)
        }else if(trimws(tclvalue(input.Tmax)) %in% c("", "NA")){
            cdt.tkmessageBox(tt, message = lang.dlg[['message']][['2']], icon = "warning", type = "ok")
            tkwait.window(tt)
        }else if(tclvalue(file.save) %in% c("", "NA")){
            cdt.tkmessageBox(tt, message = lang.dlg[['message']][['3']], icon = "warning", type = "ok")
            tkwait.window(tt)
        }else if(.cdtData$GalParams$data.type == 'cdtnetcdf' & is.null(settingTmin)){
            cdt.tkmessageBox(tt, message = lang.dlg[['message']][['4']], icon = "warning", type = "ok")
            tkwait.window(tt)
        }else if(.cdtData$GalParams$data.type == 'cdtnetcdf' & is.null(settingTmax)){
            cdt.tkmessageBox(tt, message = lang.dlg[['message']][['5']], icon = "warning", type = "ok")
            tkwait.window(tt)
        }else{
            if(.cdtData$GalParams$data.type == 'cdtstation'){
                .cdtData$GalParams$cdtstation$tmin <- trimws(tclvalue(input.Tmin))
                .cdtData$GalParams$cdtstation$tmax <- trimws(tclvalue(input.Tmax))
            }

            if(.cdtData$GalParams$data.type == 'cdtdataset'){
                .cdtData$GalParams$cdtdataset$tmin <- trimws(tclvalue(input.Tmin))
                .cdtData$GalParams$cdtdataset$tmax <- trimws(tclvalue(input.Tmax))
            }

            if(.cdtData$GalParams$data.type == 'cdtnetcdf'){
                .cdtData$GalParams$cdtnetcdf$tmin$dir <- trimws(tclvalue(input.Tmin))
                .cdtData$GalParams$cdtnetcdf$tmax$dir <- trimws(tclvalue(input.Tmax))
            }

            .cdtData$GalParams$output <- trimws(tclvalue(file.save))

            .cdtData$GalParams$settingTmin <- settingTmin
            .cdtData$GalParams$settingTmax <- settingTmax

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
