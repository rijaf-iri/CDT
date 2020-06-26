
computeWB_getParams <- function(){
    listOpenFiles <- openFile_ttkcomboList()
    if(WindowsOS()){
        largeur1 <- 38
        largeur2 <- 61
        largeur3 <- 58
        largeur4 <- 7
        largeur5 <- 3
    }else{
        largeur1 <- 35
        largeur2 <- 55
        largeur3 <- 54
        largeur4 <- 9
        largeur5 <- 4
    }

    MOIS <- format(ISOdate(2014, 1:12, 1), "%B")

    xml.dlg <- file.path(.cdtDir$dirLocal, "languages", "cdtCompute_WB_dlgBox.xml")
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
            txt.INEtp <- lang.dlg[['label']][['2']]
            txt.INPrec <- lang.dlg[['label']][['3']]
        }else{
            txt.INEtp <- lang.dlg[['label']][['4']]
            txt.INPrec <- lang.dlg[['label']][['5']]
        }
        txt.INEtp.var <- tclVar(txt.INEtp)
        txt.INPrec.var <- tclVar(txt.INPrec)


        ##############
        txt.prec <- tklabel(frameInData, text = tclvalue(txt.INPrec.var), textvariable = txt.INPrec.var, anchor = 'w', justify = 'left')
        txt.etp <- tklabel(frameInData, text = tclvalue(txt.INEtp.var), textvariable = txt.INEtp.var, anchor = 'w', justify = 'left')

        if(data.type == 'cdtstation'){
            cb.en.prec <- ttkcombobox(frameInData, values = unlist(listOpenFiles), textvariable = input.Prec, width = largeur3)
            cb.en.etp <- ttkcombobox(frameInData, values = unlist(listOpenFiles), textvariable = input.Etp, width = largeur3)
        }else{
            cb.en.prec <- tkentry(frameInData, textvariable = input.Prec, width = largeur2)
            cb.en.etp <- tkentry(frameInData, textvariable = input.Etp, width = largeur2)
        }
        bt.prec <- tkbutton(frameInData, text = "...")
        bt.etp <- tkbutton(frameInData, text = "...")

        ############ 

        tkgrid(txt.prec, row = 0, column = 0, sticky = 'we', rowspan = 1, columnspan = 5, padx = 1, pady = 0, ipadx = 1, ipady = 1)
        tkgrid(cb.en.prec, row = 1, column = 0, sticky = 'we', rowspan = 1, columnspan = 4, padx = 0, pady = 0, ipadx = 1, ipady = 1)
        tkgrid(bt.prec, row = 1, column = 4, sticky = 'w', rowspan = 1, columnspan = 1, padx = 0, pady = 0, ipadx = 1, ipady = 1)

        tkgrid(txt.etp, row = 2, column = 0, sticky = 'we', rowspan = 1, columnspan = 5, padx = 1, pady = 0, ipadx = 1, ipady = 1)
        tkgrid(cb.en.etp, row = 3, column = 0, sticky = 'we', rowspan = 1, columnspan = 4, padx = 0, pady = 0, ipadx = 1, ipady = 1)
        tkgrid(bt.etp, row = 3, column = 4, sticky = 'w', rowspan = 1, columnspan = 1, padx = 0, pady = 0, ipadx = 1, ipady = 1)

        #############

        if(data.type == 'cdtstation'){
            helpWidget(cb.en.etp, lang.dlg[['tooltip']][['2']], lang.dlg[['status']][['2']])
            helpWidget(cb.en.prec, lang.dlg[['tooltip']][['3']], lang.dlg[['status']][['3']])
            helpWidget(bt.etp, lang.dlg[['tooltip']][['4']], lang.dlg[['status']][['4']])
            helpWidget(bt.prec, lang.dlg[['tooltip']][['4']], lang.dlg[['status']][['4']])
        }else{
            helpWidget(cb.en.etp, lang.dlg[['tooltip']][['6']], lang.dlg[['status']][['6']])
            helpWidget(cb.en.prec, lang.dlg[['tooltip']][['7']], lang.dlg[['status']][['7']])
            helpWidget(bt.etp, lang.dlg[['tooltip']][['5']], lang.dlg[['status']][['5']])
            helpWidget(bt.prec, lang.dlg[['tooltip']][['5']], lang.dlg[['status']][['5']])
        }

        ############

        tkconfigure(bt.prec, command = function(){
            if(data.type == 'cdtstation'){
                tcl('wm', 'attributes', tt, topmost = FALSE)
                dat.opfiles <- getOpenFiles(tt)
                tcl('wm', 'attributes', tt, topmost = TRUE)
                if(!is.null(dat.opfiles)){
                    update.OpenFiles('ascii', dat.opfiles)
                    listOpenFiles[[length(listOpenFiles) + 1]] <<- dat.opfiles[[1]]
                    tclvalue(input.Prec) <- dat.opfiles[[1]]
                    lapply(list(cb.en.etp, cb.en.prec), tkconfigure, values = unlist(listOpenFiles))
                }
            }else{
                tcl('wm', 'attributes', tt, topmost = FALSE)
                path.rds <- tclvalue(tkgetOpenFile(initialdir = getwd(), filetypes = .cdtEnv$tcl$data$filetypes6))
                tcl('wm', 'attributes', tt, topmost = TRUE)
                tclvalue(input.Prec) <- if(path.rds %in% c("", "NA")) "" else path.rds
            }
        })

        ############

        tkconfigure(bt.etp, command = function(){
            if(data.type == 'cdtstation'){
                tcl('wm', 'attributes', tt, topmost = FALSE)
                dat.opfiles <- getOpenFiles(tt)
                tcl('wm', 'attributes', tt, topmost = TRUE)
                if(!is.null(dat.opfiles)){
                    update.OpenFiles('ascii', dat.opfiles)
                    listOpenFiles[[length(listOpenFiles) + 1]] <<- dat.opfiles[[1]]
                    tclvalue(input.Etp) <- dat.opfiles[[1]]
                    lapply(list(cb.en.etp, cb.en.prec), tkconfigure, values = unlist(listOpenFiles))
                }
            }else{
                tcl('wm', 'attributes', tt, topmost = FALSE)
                path.rds <- tclvalue(tkgetOpenFile(initialdir = getwd(), filetypes = .cdtEnv$tcl$data$filetypes6))
                tcl('wm', 'attributes', tt, topmost = TRUE)
                tclvalue(input.Etp) <- if(path.rds %in% c("", "NA")) "" else path.rds
            }
        })

        ############
        tkgrid(frameInData)
    }

    ############################################

    saveFun <- function(data.type){
        tkdestroy(frSaveIn)
        frSaveIn <<- tkframe(frSave)

        #########
        if(data.type == 'cdtstation'){
            txtSaveDir <- lang.dlg[['label']][['6']]
            isFile <- TRUE
        }else{
            txtSaveDir <- lang.dlg[['label']][['7']]
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
        isHlp <- if(data.type == 'cdtstation') '8' else '9'

        helpWidget(en.file.save, lang.dlg[['tooltip']][[isHlp]], lang.dlg[['status']][[isHlp]])
        helpWidget(bt.file.save, lang.dlg[['tooltip']][['5']], lang.dlg[['status']][['5']])

        #########
        tkgrid(frSaveIn)
    }

    ############################################

    frdatatype <- tkframe(frMain, relief = 'sunken', borderwidth = 2)

    DataType <- tclVar()
    CbdatatypeVAL <- .cdtEnv$tcl$lang$global[['combobox']][['2']][1:2]
    datatypeVAL <- c('cdtstation', 'cdtdataset')
    tclvalue(DataType) <- CbdatatypeVAL[datatypeVAL %in% .cdtData$GalParams$data.type]

    txt.datatyp <- tklabel(frdatatype, text = lang.dlg[['label']][['1']], anchor = 'w', justify = 'left')
    cb.datatyp <- ttkcombobox(frdatatype, values = CbdatatypeVAL, textvariable = DataType, width = largeur1)

    tkgrid(txt.datatyp, row = 0, column = 0, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
    tkgrid(cb.datatyp, row = 1, column = 0, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)

    helpWidget(cb.datatyp, lang.dlg[['tooltip']][['1']], lang.dlg[['status']][['1']])

    ###############

    tkbind(cb.datatyp, "<<ComboboxSelected>>", function(){
        data.type <- datatypeVAL[CbdatatypeVAL %in% str_trim(tclvalue(DataType))]

        inputDataFun(data.type)
        saveFun(data.type)

        tkfocus(tt)
    })

    ############################################

    frameData <- tkframe(frMain, relief = 'sunken', borderwidth = 2)

    frameInData <- tkframe(frameData)

    if(.cdtData$GalParams$data.type == 'cdtstation'){
        input.Etp <- tclVar(.cdtData$GalParams$cdtstation$etp)
        input.Prec <- tclVar(.cdtData$GalParams$cdtstation$prec)
    }else{
        input.Etp <- tclVar(.cdtData$GalParams$cdtdataset$etp)
        input.Prec <- tclVar(.cdtData$GalParams$cdtdataset$prec)
    }

    inputDataFun(.cdtData$GalParams$data.type)

    ############################################

    frSave <- tkframe(frMain, relief = 'sunken', borderwidth = 2)

    frSaveIn <- tkframe(frSave)

    file.save <- tclVar(.cdtData$GalParams$output)

    saveFun(.cdtData$GalParams$data.type)

    ############################################

    frWBalance <- tkframe(frMain, relief = 'sunken', borderwidth = 2)

    frameDate <- tkframe(frWBalance)

    imon <- as.numeric(str_trim(.cdtData$GalParams$hdate$start.month))
    start.month <- tclVar(MOIS[imon])
    start.day <- tclVar(.cdtData$GalParams$hdate$start.day)
    separate.year <- tclVar(.cdtData$GalParams$hdate$separate.year)

    chk.sep.year <- tkcheckbutton(frameDate, variable = separate.year, text = lang.dlg[['checkbutton']][['1']], anchor = 'w', justify = 'left')
    txt.1stdate0 <- tklabel(frameDate, text = lang.dlg[['label']][['8']], anchor = 'e', justify = 'right')
    txt.1stdate1 <- tklabel(frameDate, text = lang.dlg[['label']][['9']], anchor = 'e', justify = 'right')
    cb.1stdate1 <- ttkcombobox(frameDate, values = MOIS, textvariable = start.month, width = 11)
    txt.1stdate2 <- tklabel(frameDate, text = lang.dlg[['label']][['10']], anchor = 'e', justify = 'right')
    cb.1stdate2 <- ttkcombobox(frameDate, values = 1:31, textvariable = start.day, width = 3)

    txt.1stdatesep <- tklabel(frameDate, text = "", width = largeur4)

    ###############

    tkgrid(chk.sep.year, row = 0, column = 0, sticky = 'we', rowspan = 1, columnspan = 6, padx = 1, pady = 1, ipadx = 1, ipady = 1)

    tkgrid(txt.1stdatesep, row = 1, column = 0, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
    tkgrid(txt.1stdate0, row = 1, column = 1, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
    tkgrid(txt.1stdate1, row = 1, column = 2, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
    tkgrid(cb.1stdate1, row = 1, column = 3, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
    tkgrid(txt.1stdate2, row = 1, column = 4, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
    tkgrid(cb.1stdate2, row = 1, column = 5, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)

    ###############

    frameParWB <- tkframe(frWBalance, relief = 'groove', borderwidth = 2)

    start.wb <- tclVar(.cdtData$GalParams$wb$wb1)
    capacity.max <- tclVar(.cdtData$GalParams$swhc$cap.max)
    use.multi.wb <- tclVar(.cdtData$GalParams$wb$multi)
    use.multi.swhc <- tclVar(.cdtData$GalParams$swhc$multi)

    stateMWB <- if(.cdtData$GalParams$wb$multi) "normal" else "disabled"
    stateMSWHC <- if(.cdtData$GalParams$swhc$multi) "normal" else "disabled"

    txt.wb.1stday <- tklabel(frameParWB, text = lang.dlg[['label']][['11']], anchor = 'e', justify = 'right')
    en.wb.1stday <- tkentry(frameParWB, textvariable = start.wb, width = 4)
    chk.wb.1stday <- tkcheckbutton(frameParWB, variable = use.multi.wb, text = lang.dlg[['checkbutton']][['2']], anchor = 'w', justify = 'left')
    bt.wb.1stday <- ttkbutton(frameParWB, text = lang.dlg[['button']][['1']], state = stateMWB)
    txt.wb.swhc <- tklabel(frameParWB, text = lang.dlg[['label']][['12']], anchor = 'e', justify = 'right')
    en.wb.swhc <- tkentry(frameParWB, textvariable = capacity.max, width = 4)
    chk.wb.swhc <- tkcheckbutton(frameParWB, variable = use.multi.swhc, text = lang.dlg[['checkbutton']][['3']], anchor = 'w', justify = 'left')
    bt.wb.swhc <- ttkbutton(frameParWB, text = lang.dlg[['button']][['1']], state = stateMSWHC)

    txt.1stdaysep <- tklabel(frameParWB, text = "", width = largeur5)

    ###############

    tkgrid(txt.wb.1stday, row = 0, column = 0, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
    tkgrid(en.wb.1stday, row = 0, column = 1, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
    tkgrid(txt.1stdaysep, row = 0, column = 2, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
    tkgrid(chk.wb.1stday, row = 0, column = 3, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
    tkgrid(bt.wb.1stday, row = 0, column = 4, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)

    tkgrid(txt.wb.swhc, row = 1, column = 0, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
    tkgrid(en.wb.swhc, row = 1, column = 1, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
    tkgrid(chk.wb.swhc, row = 1, column = 3, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
    tkgrid(bt.wb.swhc, row = 1, column = 4, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)

    ###############
    tkconfigure(bt.wb.1stday, command = function(){
        data.type <- datatypeVAL[CbdatatypeVAL %in% str_trim(tclvalue(DataType))]
        tcl('wm', 'attributes', tt, topmost = FALSE)
        .cdtData$GalParams$wb[["file"]] <- computeWB_get.WB.SWHC(tt, .cdtData$GalParams$wb[["file"]], data.type, "WB")
        tcl('wm', 'attributes', tt, topmost = TRUE)
    })

    tkconfigure(bt.wb.swhc, command = function(){
        data.type <- datatypeVAL[CbdatatypeVAL %in% str_trim(tclvalue(DataType))]
        tcl('wm', 'attributes', tt, topmost = FALSE)
        .cdtData$GalParams$swhc[["file"]] <- computeWB_get.WB.SWHC(tt, .cdtData$GalParams$swhc[["file"]], data.type, "SWHC")
        tcl('wm', 'attributes', tt, topmost = TRUE)
    })

    ###############

    tkbind(chk.wb.1stday, "<Button-1>", function(){
        stateMWB <- if(tclvalue(use.multi.wb) == '0') 'normal' else 'disabled'
        tkconfigure(bt.wb.1stday, state = stateMWB)
    })

    tkbind(chk.wb.swhc, "<Button-1>", function(){
        stateMSWHC <- if(tclvalue(use.multi.swhc) == '0') 'normal' else 'disabled'
        tkconfigure(bt.wb.swhc, state = stateMSWHC)
    })

    ############################################

    tkgrid(frameDate, row = 0, column = 0, sticky = 'we', pady = 3)
    tkgrid(frameParWB, row = 1, column = 0, sticky = 'we')

    ############################################
    tkgrid(frdatatype, row = 1, column = 0, sticky = '', padx = 1, pady = 3, ipadx = 1, ipady = 1)
    tkgrid(frameData, row = 2, column = 0, sticky = 'we', padx = 1, pady = 3, ipadx = 1, ipady = 1)
    tkgrid(frSave, row = 3, column = 0, sticky = 'we', padx = 1, pady = 3, ipadx = 1, ipady = 1)
    tkgrid(frWBalance, row = 4, column = 0, sticky = 'we', padx = 1, pady = 3, ipadx = 1, ipady = 1)

    ############################################

    tkgrid(frMain, row = 0, column = 0, sticky = 'news', padx = 5, pady = 1, ipadx = 1, ipady = 1)

    ############################################

    bt.prm.OK <- ttkbutton(frMRG1, text = .cdtEnv$tcl$lang$global[['button']][['1']])
    bt.prm.CA <- ttkbutton(frMRG1, text = .cdtEnv$tcl$lang$global[['button']][['2']])

    tkconfigure(bt.prm.OK, command = function(){
        if(str_trim(tclvalue(input.Etp)) %in% c("", "NA")){
            cdt.tkmessageBox(tt, message = lang.dlg[['message']][['1']], icon = "warning", type = "ok")
            tkwait.window(tt)
        }else if(str_trim(tclvalue(input.Prec)) %in% c("", "NA")){
            cdt.tkmessageBox(tt, message = lang.dlg[['message']][['2']], icon = "warning", type = "ok")
            tkwait.window(tt)
        }else if(tclvalue(file.save) %in% c("", "NA")){
            cdt.tkmessageBox(tt, message = lang.dlg[['message']][['3']], icon = "warning", type = "ok")
            tkwait.window(tt)
        }else{
            .cdtData$GalParams$data.type <- datatypeVAL[CbdatatypeVAL %in% str_trim(tclvalue(DataType))]

            if(.cdtData$GalParams$data.type == 'cdtstation'){
                .cdtData$GalParams$cdtstation$etp <- str_trim(tclvalue(input.Etp))
                .cdtData$GalParams$cdtstation$prec <- str_trim(tclvalue(input.Prec))
            }

            if(.cdtData$GalParams$data.type == 'cdtdataset'){
                .cdtData$GalParams$cdtdataset$etp <- str_trim(tclvalue(input.Etp))
                .cdtData$GalParams$cdtdataset$prec <- str_trim(tclvalue(input.Prec))
            }

            .cdtData$GalParams$output <- str_trim(tclvalue(file.save))

            .cdtData$GalParams$hdate$start.month <- which(MOIS %in% str_trim(tclvalue(start.month)))
            .cdtData$GalParams$hdate$start.day <- as.numeric(str_trim(tclvalue(start.day)))
            .cdtData$GalParams$hdate$separate.year <- switch(tclvalue(separate.year), '0' = FALSE, '1' = TRUE)

            .cdtData$GalParams$wb$multi <- switch(tclvalue(use.multi.wb), '0' = FALSE, '1' = TRUE)
            .cdtData$GalParams$swhc$multi <- switch(tclvalue(use.multi.swhc), '0' = FALSE, '1' = TRUE)

            .cdtData$GalParams$wb$wb1 <- as.numeric(str_trim(tclvalue(start.wb)))
            .cdtData$GalParams$swhc$cap.max <- as.numeric(str_trim(tclvalue(capacity.max)))

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

############################################################

computeWB_get.WB.SWHC <- function(parent.win, Parameters, dataType, donne)
{
    listOpenFiles <- openFile_ttkcomboList()
    if(WindowsOS()){
        largeur0 <- 46
        largeur1 <- 36
    }else{
        largeur0 <- 46
        largeur1 <- 42
    }

    xml.dlg <- file.path(.cdtDir$dirLocal, "languages", "cdtCompute_WB_dlgBox_1.xml")
    lang.dlg <- cdtLanguageParse(xml.dlg, .cdtData$Config$lang.iso)

    ###################

    tt <- tktoplevel()
    tkgrab.set(tt)
    tkfocus(tt)

    frMRG0 <- tkframe(tt, relief = 'raised', borderwidth = 2)
    frMRG1 <- tkframe(tt)

    ###################

    frFF <- tkframe(frMRG0, relief = "sunken", borderwidth = 2)

    input.file <- tclVar(Parameters)

    ix <- switch(dataType,
                    'cdtstation' = if(donne == "WB") "1" else "2",
                    'cdtdataset' = if(donne == "WB") "3" else "4"
                )

    txt.WB <- tklabel(frFF, text = lang.dlg[['label']][[ix]], anchor = 'w', justify = 'left')
    cb.WB <- ttkcombobox(frFF, values = unlist(listOpenFiles), textvariable = input.file, width = largeur0)
    bt.WB <- tkbutton(frFF, text = "...")
    txta.WB <- tktext(frFF, bg = "white", font = "courier", cursor = "", wrap = "word", height = 6, width = largeur1)

    tkgrid(txt.WB, row = 0, column = 0, sticky = 'we', rowspan = 1, columnspan = 5, padx = 1, pady = 1, ipadx = 1, ipady = 1)
    tkgrid(cb.WB, row = 1, column = 0, sticky = 'we', rowspan = 1, columnspan = 4, padx = 0, pady = 1, ipadx = 1, ipady = 1)
    tkgrid(bt.WB, row = 1, column = 4, sticky = 'w', rowspan = 1, columnspan = 1, padx = 0, pady = 1, ipadx = 1, ipady = 1)
    tkgrid(txta.WB, row = 2, column = 0, sticky = 'we', rowspan = 1, columnspan = 5, padx = 1, pady = 1, ipadx = 1, ipady = 1)

    ############

    tkinsert(txta.WB, "1.0", lang.dlg[['message']][[ix]])
    tkconfigure(txta.WB, state = "disabled")

    ############

    tkconfigure(bt.WB, command = function(){
        if(dataType == 'cdtstation'){
            tcl('wm', 'attributes', tt, topmost = FALSE)
            dat.opfiles <- getOpenFiles(tt)
            tcl('wm', 'attributes', tt, topmost = TRUE)
            if(!is.null(dat.opfiles)){
                update.OpenFiles('ascii', dat.opfiles)
                listOpenFiles[[length(listOpenFiles) + 1]] <<- dat.opfiles[[1]]
                tclvalue(input.file) <- dat.opfiles[[1]]
                tkconfigure(cb.WB, values = unlist(listOpenFiles))
            }
        }else{
            tcl('wm', 'attributes', tt, topmost = FALSE)
            nc.opfiles <- getOpenNetcdf(tt, initialdir = getwd())
            tcl('wm', 'attributes', tt, topmost = TRUE)
            if(!is.null(nc.opfiles)){
                update.OpenFiles('netcdf', nc.opfiles)
                listOpenFiles[[length(listOpenFiles) + 1]] <<- nc.opfiles[[1]]
                tclvalue(input.file) <- nc.opfiles[[1]]
                tkconfigure(cb.WB, values = unlist(listOpenFiles))
            }
        }
    })

    ###################

    tkgrid(frFF, row = 0, column = 0, sticky = 'ew', padx = 1, pady = 1)

    ################################

    bt.prm.OK <- ttkbutton(frMRG1, text = .cdtEnv$tcl$lang$global[['button']][['1']])
    bt.prm.CA <- ttkbutton(frMRG1, text = .cdtEnv$tcl$lang$global[['button']][['2']])

    tkconfigure(bt.prm.OK, command = function(){
        if(str_trim(tclvalue(input.file)) %in% c("", "NA")){
            cdt.tkmessageBox(tt, message = lang.dlg[['message']][['5']], icon = "warning", type = "ok")
            tkwait.window(tt)
        }else{
            Parameters <<- str_trim(tclvalue(input.file))

            tkgrab.release(tt)
            tkdestroy(tt)
            tkfocus(parent.win)
        }
    })

    tkconfigure(bt.prm.CA, command = function(){
        tkgrab.release(tt)
        tkdestroy(tt)
        tkfocus(parent.win)
    })

    tkgrid(bt.prm.OK, row = 0, column = 0, sticky = 'w', padx = 5, pady = 1, ipadx = 1, ipady = 1)
    tkgrid(bt.prm.CA, row = 0, column = 1, sticky = 'e', padx = 5, pady = 1, ipadx = 1, ipady = 1)

    ################################
    tkgrid(frMRG0, row = 0, column = 0, sticky = 'nswe', rowspan = 1, columnspan = 2, padx = 1, pady = 1, ipadx = 1, ipady = 1)
    tkgrid(frMRG1, row = 1, column = 1, sticky = 'se', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)

    ############################
    tkwm.withdraw(tt)
    tcl('update')
    tt.w <- as.integer(tkwinfo("reqwidth", tt))
    tt.h <- as.integer(tkwinfo("reqheight", tt))
    tt.x <- as.integer(.cdtEnv$tcl$data$width.scr*0.5 - tt.w*0.5)
    tt.y <- as.integer(.cdtEnv$tcl$data$height.scr*0.5 - tt.h*0.5)
    tkwm.geometry(tt, paste0('+', tt.x, '+', tt.y))
    tkwm.transient(tt)
    ittle <- if(donne == "WB") '1' else '2'
    tkwm.title(tt, lang.dlg[['tab_title']][[ittle]])
    tkwm.deiconify(tt)
    tcl('wm', 'attributes', tt, topmost = TRUE)

    tkfocus(tt)
    tkbind(tt, "<Destroy>", function(){
        tkgrab.release(tt)
        tkfocus(parent.win)
    })
    tkwait.window(tt)
    return(Parameters)
}
