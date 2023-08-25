CPT.convert_getParams <- function(){
    listOpenFiles <- openFile_ttkcomboList()
    if(WindowsOS()){
        largeur0 <- 41
        largeur1 <- 38
        largeur2 <- 10
        largeur3 <- 32
    }else{
        largeur0 <- 38
        largeur1 <- 37
        largeur2 <- 10
        largeur3 <- 30
    }

    xml.dlg <- file.path(.cdtDir$dirLocal, "languages", "cdtCPTconversion_dlgBox.xml")
    lang.dlg <- cdtLanguageParse(xml.dlg, .cdtData$Config$lang.iso)

    ############################################

    tt <- tktoplevel()
    tkgrab.set(tt)
    tkfocus(tt)

    frMRG0 <- tkframe(tt, relief = 'raised', borderwidth = 2)
    frMRG1 <- tkframe(tt)
    frLeft <- tkframe(frMRG0, relief = "groove", borderwidth = 2)

    ############################################

    frdatatype <- tkframe(frLeft, relief = 'sunken', borderwidth = 2)

    DataType <- tclVar()
    CbdatatypeVAL <- lang.dlg[['combobox']][['1']]
    datatypeVAL <- c('cdtstation', 'cdtnetcdf')
    tclvalue(DataType) <- CbdatatypeVAL[datatypeVAL %in% .cdtData$GalParams$data.type]

    txt.datatyp <- tklabel(frdatatype, text = lang.dlg[['label']][['1']], anchor = 'w', justify = 'left')
    cb.datatyp <- ttkcombobox(frdatatype, values = CbdatatypeVAL, textvariable = DataType, width = largeur3)

    tkgrid(txt.datatyp, row = 0, column = 0, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
    tkgrid(cb.datatyp, row = 1, column = 0, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)

    helpWidget(cb.datatyp, lang.dlg[['tooltip']][['1']], lang.dlg[['status']][['1']])

    ###############

    tkbind(cb.datatyp, "<<ComboboxSelected>>", function(){
        tkdestroy(cb.en.indata)
        tclvalue(input.DataF) <- ''

        ###
        stateSetNC <- if(trimws(tclvalue(DataType)) == CbdatatypeVAL[2]) "normal" else "disabled"
        tkconfigure(set.indata, state = stateSetNC)

        ###
        if(trimws(tclvalue(DataType)) == CbdatatypeVAL[1]){
            tclvalue(txt.INDat.var) <- lang.dlg[['label']][['2']]

            cb.en.indata <- ttkcombobox(frameInData, values = unlist(listOpenFiles), textvariable = input.DataF, width = largeur1)

            ######
            tkconfigure(bt.indata, command = function(){
                tcl('wm', 'attributes', tt, topmost = FALSE)
                dat.opfiles <- getOpenFiles(tt)
                tcl('wm', 'attributes', tt, topmost = TRUE)
                if(!is.null(dat.opfiles)){
                    update.OpenFiles('ascii', dat.opfiles)
                    listOpenFiles[[length(listOpenFiles) + 1]] <<- dat.opfiles[[1]]
                    tclvalue(input.DataF) <- dat.opfiles[[1]]
                    tkconfigure(cb.en.indata, values = unlist(listOpenFiles))
                }
            })

            ######
            helpWidget(cb.en.indata, lang.dlg[['tooltip']][['2']], lang.dlg[['status']][['2']])
            helpWidget(bt.indata, lang.dlg[['tooltip']][['3']], lang.dlg[['status']][['3']])
        }

        ###
        if(trimws(tclvalue(DataType)) == CbdatatypeVAL[2]){
            tclvalue(txt.INDat.var) <- lang.dlg[['label']][['3']]

            cb.en.indata <- tkentry(frameInData, textvariable = input.DataF, width = largeur0)

            ######
            tkconfigure(set.indata, command = function(){
                tcl('wm', 'attributes', tt, topmost = FALSE)
                .cdtData$GalParams[["cdtnetcdf"]] <- CPT.getInfoNetcdfData(tt,
                                                            .cdtData$GalParams[["cdtnetcdf"]],
                                                            trimws(tclvalue(input.DataF)))
                tcl('wm', 'attributes', tt, topmost = TRUE)
                settingNCF <<- 1
            })

            tkconfigure(bt.indata, command = function(){
                tcl('wm', 'attributes', tt, topmost = FALSE)
                dirnc <- tk_choose.dir(getwd(), "")
                tcl('wm', 'attributes', tt, topmost = TRUE)
                tclvalue(input.DataF) <- if(dirnc %in% c("", "NA")) "" else dirnc
            })

            ######
            helpWidget(cb.en.indata, lang.dlg[['tooltip']][['4']], lang.dlg[['status']][['4']])
            helpWidget(bt.indata, lang.dlg[['tooltip']][['5']], lang.dlg[['status']][['5']])
        }

        #######
        tkgrid(cb.en.indata, row = 1, column = 0, sticky = 'we', rowspan = 1, columnspan = 4, padx = 0, pady = 0, ipadx = 1, ipady = 1)

        tkfocus(tt)
    })

    ############################################

    frameInData <- tkframe(frLeft, relief = 'sunken', borderwidth = 2)

    if(.cdtData$GalParams$data.type == 'cdtstation'){
        input.DataF <- tclVar(.cdtData$GalParams$cdtstation)
        txt.INDat <- lang.dlg[['label']][['2']]
        stateSetNC <- "disabled"
    }else{
        input.DataF <- tclVar(.cdtData$GalParams$cdtnetcdf$dir)
        txt.INDat <- lang.dlg[['label']][['3']]
        stateSetNC <- "normal"
    }
    txt.INDat.var <- tclVar(txt.INDat)

    ##############
    txt.indata <- tklabel(frameInData, text = tclvalue(txt.INDat.var), textvariable = txt.INDat.var, anchor = 'w', justify = 'left')
    set.indata <- ttkbutton(frameInData, text = .cdtEnv$tcl$lang$global[['button']][['5']], state = stateSetNC)

    if(.cdtData$GalParams$data.type == 'cdtstation'){
        cb.en.indata <- ttkcombobox(frameInData, values = unlist(listOpenFiles), textvariable = input.DataF, width = largeur1)
    }else{
        cb.en.indata <- tkentry(frameInData, textvariable = input.DataF, width = largeur0)
    }
    bt.indata <- tkbutton(frameInData, text = "...")

    ############
    settingNCF <- .cdtData$GalParams$settingNCF
    tkconfigure(set.indata, command = function(){
        tcl('wm', 'attributes', tt, topmost = FALSE)
        .cdtData$GalParams[["cdtnetcdf"]] <- CPT.getInfoNetcdfData(tt,
                                                    .cdtData$GalParams[["cdtnetcdf"]],
                                                    trimws(tclvalue(input.DataF)))
        tcl('wm', 'attributes', tt, topmost = TRUE)
        settingNCF <<- 1
    })

    tkconfigure(bt.indata, command = function(){
        if(.cdtData$GalParams$data.type == 'cdtstation'){
            tcl('wm', 'attributes', tt, topmost = FALSE)
            dat.opfiles <- getOpenFiles(tt)
            tcl('wm', 'attributes', tt, topmost = TRUE)
            if(!is.null(dat.opfiles)){
                update.OpenFiles('ascii', dat.opfiles)
                listOpenFiles[[length(listOpenFiles) + 1]] <<- dat.opfiles[[1]]
                tclvalue(input.DataF) <- dat.opfiles[[1]]
                tkconfigure(cb.en.indata, values = unlist(listOpenFiles))
            }
        }else{
            tcl('wm', 'attributes', tt, topmost = FALSE)
            dirnc <- tk_choose.dir(getwd(), "")
            tcl('wm', 'attributes', tt, topmost = TRUE)
            tclvalue(input.DataF) <- if(dirnc %in% c("", "NA") | is.na(dirnc)) "" else dirnc
        }
    })

    ############ 
    tkgrid(txt.indata, row = 0, column = 0, sticky = 'we', rowspan = 1, columnspan = 3, padx = 1, pady = 0, ipadx = 1, ipady = 1)
    tkgrid(set.indata, row = 0, column = 3, sticky = 'we', rowspan = 1, columnspan = 2, padx = 1, pady = 0, ipadx = 1, ipady = 1)
    tkgrid(cb.en.indata, row = 1, column = 0, sticky = 'we', rowspan = 1, columnspan = 4, padx = 0, pady = 0, ipadx = 1, ipady = 1)
    tkgrid(bt.indata, row = 1, column = 4, sticky = 'w', rowspan = 1, columnspan = 1, padx = 0, pady = 0, ipadx = 1, ipady = 1)

    #############
    if(.cdtData$GalParams$data.type == 'cdtstation'){
        helpWidget(cb.en.indata, lang.dlg[['tooltip']][['2']], lang.dlg[['status']][['2']])
        helpWidget(bt.indata, lang.dlg[['tooltip']][['3']], lang.dlg[['status']][['3']])
    }else{
        helpWidget(cb.en.indata, lang.dlg[['tooltip']][['4']], lang.dlg[['status']][['4']])
        helpWidget(bt.indata, lang.dlg[['tooltip']][['5']], lang.dlg[['status']][['5']])
    }

    ############################################

    frCPTInfo <- tkframe(frLeft, relief = 'sunken', borderwidth = 2)

    cptinfo.Name <- tclVar(.cdtData$GalParams$cptinfo$name)
    cptinfo.Unit <- tclVar(.cdtData$GalParams$cptinfo$units)
    cptinfo.Miss <- tclVar(.cdtData$GalParams$cptinfo$missval)

    txt.cpt.name <- tklabel(frCPTInfo, text = lang.dlg[['label']][['4']], anchor = 'e', justify = 'right')
    en.cpt.name <- tkentry(frCPTInfo, textvariable = cptinfo.Name, width = largeur2)
    txt.cpt.unit <- tklabel(frCPTInfo, text = lang.dlg[['label']][['5']], anchor = 'e', justify = 'right')
    en.cpt.unit <- tkentry(frCPTInfo, textvariable = cptinfo.Unit, width = largeur2)
    txt.cpt.miss <- tklabel(frCPTInfo, text = lang.dlg[['label']][['6']], anchor = 'e', justify = 'right')
    en.cpt.miss <- tkentry(frCPTInfo, textvariable = cptinfo.Miss, width = largeur2)

    #########
    tkgrid(txt.cpt.name, row = 0, column = 0, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
    tkgrid(en.cpt.name, row = 0, column = 1, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
    tkgrid(txt.cpt.unit, row = 0, column = 2, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
    tkgrid(en.cpt.unit, row = 0, column = 3, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
    tkgrid(txt.cpt.miss, row = 2, column = 0, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
    tkgrid(en.cpt.miss, row = 2, column = 1, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)

    ############################################

    frSave <- tkframe(frLeft, relief = 'sunken', borderwidth = 2)

    file.save <- tclVar(.cdtData$GalParams$output)

    txt.file.save <- tklabel(frSave, text = lang.dlg[['label']][['7']], anchor = 'w', justify = 'left')
    en.file.save <- tkentry(frSave, textvariable = file.save, width = largeur0)
    bt.file.save <- tkbutton(frSave, text = "...")

    #########
    tkconfigure(bt.file.save, command = function(){
        tcl('wm', 'attributes', tt, topmost = FALSE)
        fileORdir2Save(file.save, isFile = TRUE)
        tcl('wm', 'attributes', tt, topmost = TRUE)
    })

    #########
    tkgrid(txt.file.save, row = 0, column = 0, sticky = 'we', rowspan = 1, columnspan = 5, padx = 1, pady = 0, ipadx = 1, ipady = 1)
    tkgrid(en.file.save, row = 1, column = 0, sticky = 'we', rowspan = 1, columnspan = 4, padx = 0, pady = 0, ipadx = 1, ipady = 1)
    tkgrid(bt.file.save, row = 1, column = 4, sticky = 'w', rowspan = 1, columnspan = 1, padx = 0, pady = 0, ipadx = 1, ipady = 1)

    #########

    helpWidget(en.file.save, lang.dlg[['tooltip']][['6']], lang.dlg[['status']][['6']])
    helpWidget(bt.file.save, lang.dlg[['tooltip']][['5']], lang.dlg[['status']][['5']])

    ############################################
    tkgrid(frdatatype, row = 0, column = 0, sticky = '', padx = 1, pady = 3, ipadx = 1, ipady = 1)
    tkgrid(frameInData, row = 1, column = 0, sticky = 'we', padx = 1, pady = 3, ipadx = 1, ipady = 1)
    tkgrid(frCPTInfo, row = 2, column = 0, sticky = 'we', padx = 1, pady = 3, ipadx = 1, ipady = 1)
    tkgrid(frSave, row = 3, column = 0, sticky = 'we', padx = 1, pady = 3, ipadx = 1, ipady = 1)

    ############################################
    
    tkgrid(frLeft, row = 0, column = 0, sticky = 'news', padx = 5, pady = 1, ipadx = 1, ipady = 1)

    ############################################

    bt.prm.OK <- ttkbutton(frMRG1, text = .cdtEnv$tcl$lang$global[['button']][['1']])
    bt.prm.CA <- ttkbutton(frMRG1, text = .cdtEnv$tcl$lang$global[['button']][['2']])

    tkconfigure(bt.prm.OK, command = function(){
        if(trimws(tclvalue(input.DataF)) %in% c("", "NA")){
            cdt.tkmessageBox(tt, message = lang.dlg[['message']][['1']], icon = "warning", type = "ok")
            tkwait.window(tt)
        }else if(tclvalue(file.save) %in% c("", "NA")){
            cdt.tkmessageBox(tt, message = lang.dlg[['message']][['2']], icon = "warning", type = "ok")
            tkwait.window(tt)
        }else if(trimws(tclvalue(DataType)) == CbdatatypeVAL[2] & is.null(settingNCF)){
                cdt.tkmessageBox(tt, message = lang.dlg[['message']][['3']], icon = "warning", type = "ok")
                tkwait.window(tt)
        }else{
            .cdtData$GalParams$data.type <- datatypeVAL[CbdatatypeVAL %in% trimws(tclvalue(DataType))]
            if(trimws(tclvalue(DataType)) == CbdatatypeVAL[1])
                .cdtData$GalParams$cdtstation <- trimws(tclvalue(input.DataF))

            if(trimws(tclvalue(DataType)) == CbdatatypeVAL[2])
                .cdtData$GalParams$cdtnetcdf$dir <- trimws(tclvalue(input.DataF))

            .cdtData$GalParams$cptinfo$name <- trimws(tclvalue(cptinfo.Name))
            .cdtData$GalParams$cptinfo$units <- trimws(tclvalue(cptinfo.Unit))
            .cdtData$GalParams$cptinfo$missval <- trimws(tclvalue(cptinfo.Miss))

            .cdtData$GalParams$output <- trimws(tclvalue(file.save))

            .cdtData$GalParams$settingNCF <- settingNCF
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

