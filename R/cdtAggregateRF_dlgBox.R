
AggregateMWin_GetInfo <- function(){
    listOpenFiles <- openFile_ttkcomboList()
    if(WindowsOS()){
        largeur0 <- 48
        largeur1 <- 46
        largeur2 <- 30
        largeur3 <- 11
        largeur4 <- 6
    }else{
        largeur0 <- 43
        largeur1 <- 42
        largeur2 <- 30
        largeur3 <- 11
        largeur4 <- 6
    }

    ####################################

    xml.dlg <- file.path(.cdtDir$dirLocal, "languages", "cdtAggregateRF_dlgBox.xml")
    lang.dlg <- cdtLanguageParse(xml.dlg, .cdtData$Config$lang.iso)

    ####################################

    tt <- tktoplevel()
    tkgrab.set(tt)
    tkfocus(tt)

    frMRG0 <- tkframe(tt, relief = 'raised', borderwidth = 2, padx = 3, pady = 3)
    frMRG1 <- tkframe(tt)
    frAGGRRF <- tkframe(frMRG0, relief = "sunken", borderwidth = 2)

    ####################################

    frtimestep <- ttklabelframe(frAGGRRF, text = lang.dlg[['label']][['1']], labelanchor = "nw", relief = "groove", borderwidth = 2)

    timeSteps <- tclVar()
    CbperiodVAL <- .cdtEnv$tcl$lang$global[['combobox']][['1']][1:6]
    periodVAL <- c('minute', 'hourly', 'daily', 'pentad', 'dekadal', 'monthly')
    tclvalue(timeSteps) <- CbperiodVAL[periodVAL %in% .cdtData$GalParams$tstep]

    retminhr <- set.hour.minute(.cdtData$GalParams$tstep, .cdtData$GalParams$minhour)
    minhour.tclVar <- tclVar(retminhr$val)

    cb.fperiod <- ttkcombobox(frtimestep, values = CbperiodVAL, textvariable = timeSteps, width = largeur2)
    cb.minhour <- ttkcombobox(frtimestep, values = retminhr$cb, textvariable = minhour.tclVar, state = retminhr$state, width = 2)
    txt.fperiod <- tklabel(frtimestep, text = "", justify = "right", anchor = 'e', width = largeur3)

    tkgrid(txt.fperiod, row = 0, column = 0, sticky = 'we', rowspan = 1, columnspan = 5, padx = 1, pady = 0, ipadx = 1, ipady = 0)
    tkgrid(cb.fperiod, row = 0, column = 5, sticky = 'we', rowspan = 1, columnspan = 4, padx = 1, pady = 1, ipadx = 1, ipady = 1)
    tkgrid(cb.minhour, row = 0, column = 9, sticky = 'w', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)

    helpWidget(cb.fperiod, lang.dlg[['tooltip']][['1']], lang.dlg[['status']][['1']])

    #################
    tkbind(cb.fperiod, "<<ComboboxSelected>>", function(){
        intstep <- periodVAL[CbperiodVAL %in% str_trim(tclvalue(timeSteps))]

        minhour <- as.numeric(str_trim(tclvalue(minhour.tclVar)))
        retminhr <- set.hour.minute(intstep, minhour)
        tkconfigure(cb.minhour, values = retminhr$cb, state = retminhr$state)
        tclvalue(minhour.tclVar) <- retminhr$val
    })

    ####################################

    frDataType <- ttklabelframe(frAGGRRF, text = lang.dlg[['label']][['2']], labelanchor = "nw", relief = "groove", borderwidth = 2)

    DataType <- tclVar()
    CbdatatypeVAL <- .cdtEnv$tcl$lang$global[['combobox']][['2']][1:3]
    datatypeVAL <- c('cdtstation', 'cdtdataset', 'cdtnetcdf')
    tclvalue(DataType) <- CbdatatypeVAL[datatypeVAL %in% .cdtData$GalParams$data.type]

    if(.cdtData$GalParams$data.type == 'cdtstation'){
        file.stnfl <- tclVar(.cdtData$GalParams$cdtstation)
        txtFileDir <- lang.dlg[['label']][['3']]
        stateSetData <- "disabled"
    }else if(.cdtData$GalParams$data.type == 'cdtdataset'){
        file.stnfl <- tclVar(.cdtData$GalParams$cdtdataset)
        txtFileDir <- lang.dlg[['label']][['4']]
        stateSetData <- "disabled"
    }else{
        file.stnfl <- tclVar(.cdtData$GalParams$cdtnetcdf$dir)
        txtFileDir <- lang.dlg[['label']][['5']]
        stateSetData <- "normal"
    }
    fileINdir <- tclVar(txtFileDir)

    ##############
    cb.datatype <- ttkcombobox(frDataType, values = CbdatatypeVAL, textvariable = DataType, width = largeur2)
    set.datatype <- ttkbutton(frDataType, text = .cdtEnv$tcl$lang$global[['button']][['5']], state = stateSetData)

    txt.stnfl <- tklabel(frDataType, text = tclvalue(fileINdir), textvariable = fileINdir, anchor = 'w', justify = 'left')
    if(.cdtData$GalParams$data.type == 'cdtstation'){
        cb.stnfl <- ttkcombobox(frDataType, values = unlist(listOpenFiles), textvariable = file.stnfl, width = largeur1)
    }else{
        cb.stnfl <- tkentry(frDataType, textvariable = file.stnfl, width = largeur0)
    }
    bt.stnfl <- tkbutton(frDataType, text = "...")

    ##############

    tkgrid(cb.datatype, row = 0, column = 0, sticky = 'we', rowspan = 1, columnspan = 3, padx = 1, pady = 1, ipadx = 1, ipady = 1)
    tkgrid(set.datatype, row = 0, column = 3, sticky = 'we', rowspan = 1, columnspan = 2, padx = 1, pady = 1, ipadx = 1, ipady = 1)
    tkgrid(txt.stnfl, row = 1, column = 0, sticky = 'we', rowspan = 1, columnspan = 5, padx = 1, pady = 1, ipadx = 1, ipady = 1)
    tkgrid(cb.stnfl, row = 2, column = 0, sticky = 'we', rowspan = 1, columnspan = 4, padx = 0, pady = 1, ipadx = 1, ipady = 1)
    tkgrid(bt.stnfl, row = 2, column = 4, sticky = 'we', rowspan = 1, columnspan = 1, padx = 0, pady = 1, ipadx = 1, ipady = 1)

    if(.cdtData$GalParams$data.type == 'cdtstation'){
        helpWidget(cb.stnfl, lang.dlg[['tooltip']][['2']], lang.dlg[['status']][['2']])
        helpWidget(bt.stnfl, lang.dlg[['tooltip']][['5']], lang.dlg[['status']][['5']])
    }else if(.cdtData$GalParams$data.type == 'cdtdataset'){
        helpWidget(cb.stnfl, lang.dlg[['tooltip']][['3']], lang.dlg[['status']][['3']])
        helpWidget(bt.stnfl, lang.dlg[['tooltip']][['6']], lang.dlg[['status']][['6']])
    }else{
        helpWidget(cb.stnfl, lang.dlg[['tooltip']][['4']], lang.dlg[['status']][['4']])
        helpWidget(bt.stnfl, lang.dlg[['tooltip']][['6']], lang.dlg[['status']][['6']])
    }

    ##############

    settingdone <- .cdtData$GalParams$settingdone

    tkconfigure(set.datatype, command = function(){
        tcl('wm', 'attributes', tt, topmost = FALSE)
        AggregateTS_ncdfData(tt, str_trim(tclvalue(file.stnfl)), tclvalue(timeSteps))
        tcl('wm', 'attributes', tt, topmost = TRUE)
        settingdone <<- 1
    })

    tkconfigure(bt.stnfl, command = function(){
        if(.cdtData$GalParams$data.type == 'cdtstation'){
            tcl('wm', 'attributes', tt, topmost = FALSE)
            dat.opfiles <- getOpenFiles(tt)
            tcl('wm', 'attributes', tt, topmost = TRUE)
            if(!is.null(dat.opfiles)){
                update.OpenFiles('ascii', dat.opfiles)
                listOpenFiles[[length(listOpenFiles) + 1]] <<- dat.opfiles[[1]]
                tclvalue(file.stnfl) <- dat.opfiles[[1]]
                tkconfigure(cb.stnfl, values = unlist(listOpenFiles))
            }
        }else if(.cdtData$GalParams$data.type == 'cdtdataset'){
            tcl('wm', 'attributes', tt, topmost = FALSE)
            path.rds <- tclvalue(tkgetOpenFile(filetypes = .cdtEnv$tcl$data$filetypes6))
            tcl('wm', 'attributes', tt, topmost = TRUE)
            tclvalue(file.stnfl) <- if(path.rds %in% c("", "NA") | is.na(path.rds)) "" else path.rds
        }else{
            tcl('wm', 'attributes', tt, topmost = FALSE)
            dirnc <- tk_choose.dir(getwd(), "")
            tcl('wm', 'attributes', tt, topmost = TRUE)
            tclvalue(file.stnfl) <- if(dirnc %in% c("", "NA") | is.na(dirnc)) "" else dirnc
        }
    })

    ##############

    tkbind(cb.datatype, "<<ComboboxSelected>>", function(){
        tkdestroy(cb.stnfl)
        tclvalue(file.stnfl) <- ''

        ####
        if(str_trim(tclvalue(DataType)) == CbdatatypeVAL[1]){
            tclvalue(fileINdir) <- lang.dlg[['label']][['3']]
            tclvalue(fileORdir) <- lang.dlg[['label']][['11']]

            cb.stnfl <- ttkcombobox(frDataType, values = unlist(listOpenFiles), textvariable = file.stnfl, width = largeur1)

            #######
            tkconfigure(bt.stnfl, command = function(){
                tcl('wm', 'attributes', tt, topmost = FALSE)
                dat.opfiles <- getOpenFiles(tt)
                tcl('wm', 'attributes', tt, topmost = TRUE)
                if(!is.null(dat.opfiles)){
                    update.OpenFiles('ascii', dat.opfiles)
                    listOpenFiles[[length(listOpenFiles) + 1]] <<- dat.opfiles[[1]]
                    tclvalue(file.stnfl) <- dat.opfiles[[1]]
                    tkconfigure(cb.stnfl, values = unlist(listOpenFiles))
                }
            })
            tkconfigure(bt.file.save, command = function(){
                tcl('wm', 'attributes', tt, topmost = FALSE)
                fileORdir2Save(file.save, isFile = TRUE)
                tcl('wm', 'attributes', tt, topmost = TRUE)
            })
            tkconfigure(set.datatype, state = 'disabled')

            #######
            helpWidget(cb.stnfl, lang.dlg[['tooltip']][['2']], lang.dlg[['status']][['2']])
            helpWidget(bt.stnfl, lang.dlg[['tooltip']][['5']], lang.dlg[['status']][['5']])
            helpWidget(en.file.save, lang.dlg[['tooltip']][['11']], lang.dlg[['status']][['11']])
        }

        ####
        if(str_trim(tclvalue(DataType)) == CbdatatypeVAL[2]){
            tclvalue(fileINdir) <- lang.dlg[['label']][['4']]
            tclvalue(fileORdir) <- lang.dlg[['label']][['12']]

            cb.stnfl <- tkentry(frDataType, textvariable = file.stnfl, width = largeur0)

            #######
            tkconfigure(bt.stnfl, command = function(){
                tcl('wm', 'attributes', tt, topmost = FALSE)
                path.rds <- tclvalue(tkgetOpenFile(filetypes = .cdtEnv$tcl$data$filetypes6))
                tcl('wm', 'attributes', tt, topmost = TRUE)
                tclvalue(file.stnfl) <- if(path.rds %in% c("", "NA") | is.na(path.rds)) "" else path.rds
            })

            tkconfigure(bt.file.save, command = function(){
                tcl('wm', 'attributes', tt, topmost = FALSE)
                fileORdir2Save(file.save, isFile = FALSE)
                tcl('wm', 'attributes', tt, topmost = TRUE)
            })
            tkconfigure(set.datatype, state = 'disabled')

            #######
            helpWidget(cb.stnfl, lang.dlg[['tooltip']][['3']], lang.dlg[['status']][['3']])
            helpWidget(bt.stnfl, lang.dlg[['tooltip']][['6']], lang.dlg[['status']][['6']])
            helpWidget(en.file.save, lang.dlg[['tooltip']][['12']], lang.dlg[['status']][['12']])
        }

        ####
        if(str_trim(tclvalue(DataType)) == CbdatatypeVAL[3]){
            tclvalue(fileINdir) <- lang.dlg[['label']][['5']]
            tclvalue(fileORdir) <- lang.dlg[['label']][['12']]

            cb.stnfl <- tkentry(frDataType, textvariable = file.stnfl, width = largeur0)

            #######
            tkconfigure(bt.stnfl, command = function(){
                tcl('wm', 'attributes', tt, topmost = FALSE)
                dirnc <- tk_choose.dir(getwd(), "")
                tcl('wm', 'attributes', tt, topmost = TRUE)
                tclvalue(file.stnfl) <- if(dirnc %in% c("", "NA") | is.na(dirnc)) "" else dirnc
            })

            tkconfigure(set.datatype, command = function(){
                tcl('wm', 'attributes', tt, topmost = FALSE)
                AggregateTS_ncdfData(tt, str_trim(tclvalue(file.stnfl)), tclvalue(timeSteps))
                tcl('wm', 'attributes', tt, topmost = TRUE)
                settingdone <<- 1
            })

            tkconfigure(bt.file.save, command = function(){
                tcl('wm', 'attributes', tt, topmost = FALSE)
                fileORdir2Save(file.save, isFile = FALSE)
                tcl('wm', 'attributes', tt, topmost = TRUE)
            })
            tkconfigure(set.datatype, state = 'normal')

            #######
            helpWidget(cb.stnfl, lang.dlg[['tooltip']][['4']], lang.dlg[['status']][['4']])
            helpWidget(bt.stnfl, lang.dlg[['tooltip']][['6']], lang.dlg[['status']][['6']])
            helpWidget(en.file.save, lang.dlg[['tooltip']][['12']], lang.dlg[['status']][['12']])
        }

        #######
        tkgrid(cb.stnfl, row = 2, column = 0, sticky = 'we', rowspan = 1, columnspan = 4, padx = 0, pady = 1, ipadx = 1, ipady = 1)
        tkfocus(tt)
    })

    ####################################

    frameAggr <- ttklabelframe(frAGGRRF, text = lang.dlg[['label']][['6']], labelanchor = "nw", relief = "groove", borderwidth = 2)

    aggr.fun <- tclVar(.cdtData$GalParams$aggr.series$fun)
    time.win <- tclVar(.cdtData$GalParams$aggr.series$win)
    min.data <- tclVar(.cdtData$GalParams$aggr.series$min.data)
    align.data <- tclVar(.cdtData$GalParams$aggr.series$align)

    AGGRFUN <- c("mean", "sum", "median", "sd", "max", "min")
    ALIGN <- c("center", "left", "right")

    txt.aggfun <- tklabel(frameAggr, text = lang.dlg[['label']][['7']], anchor = 'e', justify = 'right')
    cb.aggfun <- ttkcombobox(frameAggr, values = AGGRFUN, textvariable = aggr.fun, width = largeur4)
    txt.twin <- tklabel(frameAggr, text = lang.dlg[['label']][['8']], anchor = 'e', justify = 'right')
    en.twin <- tkentry(frameAggr, textvariable = time.win, width = 4)
    txt.mindat <- tklabel(frameAggr, text = lang.dlg[['label']][['9']], anchor = 'e', justify = 'right')
    en.mindat <- tkentry(frameAggr, textvariable = min.data, width = 4)
    txt.align <- tklabel(frameAggr, text = lang.dlg[['label']][['10']], anchor = 'e', justify = 'right')
    cb.align <- ttkcombobox(frameAggr, values = ALIGN, textvariable = align.data, width = largeur4)

    tkgrid(txt.aggfun, row = 0, column = 0, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
    tkgrid(cb.aggfun, row = 0, column = 1, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
    tkgrid(txt.twin, row = 0, column = 2, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
    tkgrid(en.twin, row = 0, column = 3, sticky = 'w', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
    tkgrid(txt.mindat, row = 1, column = 0, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
    tkgrid(en.mindat, row = 1, column = 1, sticky = 'w', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
    tkgrid(txt.align, row = 1, column = 2, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
    tkgrid(cb.align, row = 1, column = 3, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)

    helpWidget(cb.aggfun, lang.dlg[['tooltip']][['7']], lang.dlg[['status']][['7']])
    helpWidget(en.twin, lang.dlg[['tooltip']][['8']], lang.dlg[['status']][['8']])
    helpWidget(en.mindat, lang.dlg[['tooltip']][['9']], lang.dlg[['status']][['9']])
    helpWidget(cb.align, lang.dlg[['tooltip']][['10']], lang.dlg[['status']][['10']])

    ####################################

    frSave <- tkframe(frAGGRRF, relief = 'groove', borderwidth = 2)

    file.save <- tclVar(.cdtData$GalParams$output)

    if(.cdtData$GalParams$data.type == 'cdtstation'){
        txtSaveDir <- lang.dlg[['label']][['11']]
        isFile <- TRUE
    }else{
        txtSaveDir <- lang.dlg[['label']][['12']]
        isFile <- FALSE
    }
    fileORdir <- tclVar(txtSaveDir)

    txt.file.save <- tklabel(frSave, text = tclvalue(fileORdir), textvariable = fileORdir, anchor = 'w', justify = 'left')
    en.file.save <- tkentry(frSave, textvariable = file.save, width = largeur0)
    bt.file.save <- tkbutton(frSave, text = "...")

    ########

    tkgrid(txt.file.save, row = 0, column = 0, sticky = 'we', rowspan = 1, columnspan = 5, padx = 1, pady = 0, ipadx = 1, ipady = 1)
    tkgrid(en.file.save, row = 1, column = 0, sticky = 'we', rowspan = 1, columnspan = 4, padx = 0, pady = 0, ipadx = 1, ipady = 1)
    tkgrid(bt.file.save, row = 1, column = 4, sticky = 'we', rowspan = 1, columnspan = 1, padx = 0, pady = 0, ipadx = 1, ipady = 1)

    ihlpSav <- if(.cdtData$GalParams$data.type == 'cdtstation') '11' else '12'
    helpWidget(en.file.save, lang.dlg[['tooltip']][[ihlpSav]], lang.dlg[['status']][[ihlpSav]])
    helpWidget(bt.file.save, lang.dlg[['tooltip']][['6']], lang.dlg[['status']][['6']])

    ########

    tkconfigure(bt.file.save, command = function(){
        tcl('wm', 'attributes', tt, topmost = FALSE)
        fileORdir2Save(file.save, isFile = isFile)
        tcl('wm', 'attributes', tt, topmost = TRUE)
    })

    ############################################
    tkgrid(frtimestep, row = 0, column = 0, sticky = 'we', padx = 1, pady = 1, ipadx = 1, ipady = 1)
    tkgrid(frDataType, row = 1, column = 0, sticky = 'we', padx = 1, pady = 1, ipadx = 1, ipady = 1)
    tkgrid(frameAggr, row = 3, column = 0, sticky = 'we', padx = 1, pady = 1, ipadx = 1, ipady = 1)
    tkgrid(frSave, row = 4, column = 0, sticky = 'we', padx = 1, pady = 1, ipadx = 1, ipady = 1)

    ############################################

    tkgrid(frAGGRRF, row = 0, column = 0, sticky = 'news', padx = 5, pady = 1, ipadx = 1, ipady = 1)

    ############################################

    bt.prm.OK <- ttkbutton(frMRG1, text = .cdtEnv$tcl$lang$global[['button']][['1']])
    bt.prm.CA <- ttkbutton(frMRG1, text = .cdtEnv$tcl$lang$global[['button']][['2']])

    #######
    tkconfigure(bt.prm.OK, command = function(){
        if(str_trim(tclvalue(file.stnfl)) == ""){
            cdt.tkmessageBox(tt, message = lang.dlg[['message']][['1']], icon = "warning", type = "ok")
            tkwait.window(tt)
        }else if(str_trim(tclvalue(file.save)) %in% c("", "NA")){
            cdt.tkmessageBox(tt, message = lang.dlg[['message']][['2']], icon = "warning", type = "ok")
            tkwait.window(tt)
        }else if(str_trim(tclvalue(DataType)) == CbdatatypeVAL[3] & is.null(settingdone)){
            cdt.tkmessageBox(tt, message = lang.dlg[['message']][['3']], icon = "warning", type = "ok")
            tkwait.window(tt)
        }else{
            .cdtData$GalParams$tstep <- periodVAL[CbperiodVAL %in% str_trim(tclvalue(timeSteps))]
            .cdtData$GalParams$minhour <- as.numeric(str_trim(tclvalue(minhour.tclVar)))

            .cdtData$GalParams$data.type <- datatypeVAL[CbdatatypeVAL %in% str_trim(tclvalue(DataType))]

            if(str_trim(tclvalue(DataType)) == CbdatatypeVAL[1])
                .cdtData$GalParams$cdtstation <- str_trim(tclvalue(file.stnfl))
            if(str_trim(tclvalue(DataType)) == CbdatatypeVAL[2])
                .cdtData$GalParams$cdtdataset <- str_trim(tclvalue(file.stnfl))
            if(str_trim(tclvalue(DataType)) == CbdatatypeVAL[3])
                .cdtData$GalParams$cdtnetcdf$dir <- str_trim(tclvalue(file.stnfl))

            .cdtData$GalParams$output <- str_trim(tclvalue(file.save))

            .cdtData$GalParams$aggr.series$fun <- str_trim(tclvalue(aggr.fun))
            .cdtData$GalParams$aggr.series$win <- as.numeric(str_trim(tclvalue(time.win)))
            .cdtData$GalParams$aggr.series$min.data <- as.numeric(str_trim(tclvalue(min.data)))
            .cdtData$GalParams$aggr.series$align <- str_trim(tclvalue(align.data))

            .cdtData$GalParams$settingdone <- settingdone
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

    ############################################

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
    invisible()
}
