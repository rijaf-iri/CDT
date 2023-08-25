
removeBiasGetInfoWind <- function(){
    if(WindowsOS()){
        largeur0 <- 22
        largeur1 <- 46
        largeur2 <- 35
    }else{
        largeur0 <- 22
        largeur1 <- 42
        largeur2 <- 35
    }

    ####################################

    xml.dlg <- file.path(.cdtDir$dirLocal, "languages", "cdtBias_Correction_Wind_dlgBox.xml")
    lang.dlg <- cdtLanguageParse(xml.dlg, .cdtData$Config$lang.iso)

    ####################################

    tt <- tktoplevel()
    tkgrab.set(tt)
    tkfocus(tt)

    frMRG0 <- tkframe(tt, relief = 'raised', borderwidth = 2, padx = 3, pady = 3)
    frMRG1 <- tkframe(tt)

    ############################################

    getwindSpeed <- function(){
        tkdestroy(fr.windVar)
        fr.windVar <<- tkframe(frInputData)

        txt.ncS <- tklabel(fr.windVar, text = lang.dlg[['label']][['3-1']], anchor = 'w', justify = 'left')
        set.ncS <- ttkbutton(fr.windVar, text = .cdtEnv$tcl$lang$global[['button']][['5']])
        en.ncS <- tkentry(fr.windVar, textvariable = dir.NC_S, width = largeur1)
        bt.ncS <- tkbutton(fr.windVar, text = "...")

        tkgrid(txt.ncS, row = 0, column = 0, sticky = 'we', rowspan = 1, columnspan = 3, padx = 1, pady = 1, ipadx = 1, ipady = 1)
        tkgrid(set.ncS, row = 0, column = 3, sticky = 'we', rowspan = 1, columnspan = 2, padx = 1, pady = 1, ipadx = 1, ipady = 1)
        tkgrid(en.ncS, row = 1, column = 0, sticky = 'we', rowspan = 1, columnspan = 4, padx = 0, pady = 1, ipadx = 1, ipady = 1)
        tkgrid(bt.ncS, row = 1, column = 4, sticky = 'we', rowspan = 1, columnspan = 1, padx = 0, pady = 1, ipadx = 1, ipady = 1)

        helpWidget(en.ncS, lang.dlg[['tooltip']][['5-1']], lang.dlg[['status']][['5-1']])
        helpWidget(bt.ncS, lang.dlg[['tooltip']][['6']], lang.dlg[['status']][['6']])
        helpWidget(set.ncS, lang.dlg[['tooltip']][['7']], lang.dlg[['status']][['7']])

        ######

        tkconfigure(set.ncS, command = function(){
            tcl('wm', 'attributes', tt, topmost = FALSE)
            .cdtData$GalParams[["INPUT.S"]] <- getInfoNetCDFData(tt, .cdtData$GalParams[["INPUT.S"]],
                                                                 trimws(tclvalue(dir.NC_S)))
            tcl('wm', 'attributes', tt, topmost = TRUE)
        })

        tkconfigure(bt.ncS, command = function(){
            tcl('wm', 'attributes', tt, topmost = FALSE)
            dirnc <- tk_choose.dir(getwd(), "")
            tcl('wm', 'attributes', tt, topmost = TRUE)
            tclvalue(dir.NC_S) <- if(!is.na(dirnc)) dirnc else ""
        })

        tkgrid(fr.windVar, row = 1, column = 0, sticky = 'we', rowspan = 1, columnspan = 2)
    }

    getwindData <- function(){
        getwindUVone <- function(){
            tkdestroy(fr.windUV)
            fr.windUV <<- tkframe(fr.windVar)

            txt.ncUV <- tklabel(fr.windUV, text = lang.dlg[['label']][['3-4']], anchor = 'w', justify = 'left')
            set.ncUV <- ttkbutton(fr.windUV, text = .cdtEnv$tcl$lang$global[['button']][['5']])
            en.ncUV <- tkentry(fr.windUV, textvariable = dir.NC_UV, width = largeur1)
            bt.ncUV <- tkbutton(fr.windUV, text = "...")

            tkgrid(txt.ncUV, row = 0, column = 0, sticky = 'we', rowspan = 1, columnspan = 3, padx = 1, pady = 1, ipadx = 1, ipady = 1)
            tkgrid(set.ncUV, row = 0, column = 3, sticky = 'we', rowspan = 1, columnspan = 2, padx = 1, pady = 1, ipadx = 1, ipady = 1)
            tkgrid(en.ncUV, row = 1, column = 0, sticky = 'we', rowspan = 1, columnspan = 4, padx = 0, pady = 1, ipadx = 1, ipady = 1)
            tkgrid(bt.ncUV, row = 1, column = 4, sticky = 'we', rowspan = 1, columnspan = 1, padx = 0, pady = 1, ipadx = 1, ipady = 1)

            helpWidget(en.ncUV, lang.dlg[['tooltip']][['5-4']], lang.dlg[['status']][['5-4']])
            helpWidget(bt.ncUV, lang.dlg[['tooltip']][['6']], lang.dlg[['status']][['6']])
            helpWidget(set.ncUV, lang.dlg[['tooltip']][['7']], lang.dlg[['status']][['7']])

            #####

            tkconfigure(set.ncUV, command = function(){
                tcl('wm', 'attributes', tt, topmost = FALSE)
                .cdtData$GalParams[["INPUT.UV"]] <- getInfoNetCDFDataWind(tt, .cdtData$GalParams[["INPUT.UV"]],
                                                                          trimws(tclvalue(dir.NC_UV)))
                tcl('wm', 'attributes', tt, topmost = TRUE)
            })

            tkconfigure(bt.ncUV, command = function(){
                tcl('wm', 'attributes', tt, topmost = FALSE)
                dirnc <- tk_choose.dir(getwd(), "")
                tcl('wm', 'attributes', tt, topmost = TRUE)
                tclvalue(dir.NC_UV) <- if(!is.na(dirnc)) dirnc else ""
            })

            tkgrid(fr.windUV, row = 5, column = 0, sticky = 'we', rowspan = 1, columnspan = 5)
            tcl('update')
        }

        getwindUVsep <- function(){
            tkdestroy(fr.windUV)
            fr.windUV <<- tkframe(fr.windVar)

            txt.ncU <- tklabel(fr.windUV, text = lang.dlg[['label']][['3-2']], anchor = 'w', justify = 'left')
            set.ncU <- ttkbutton(fr.windUV, text = .cdtEnv$tcl$lang$global[['button']][['5']])
            en.ncU <- tkentry(fr.windUV, textvariable = dir.NC_U, width = largeur1)
            bt.ncU <- tkbutton(fr.windUV, text = "...")

            txt.ncV <- tklabel(fr.windUV, text = lang.dlg[['label']][['3-3']], anchor = 'w', justify = 'left')
            set.ncV <- ttkbutton(fr.windUV, text = .cdtEnv$tcl$lang$global[['button']][['5']])
            en.ncV <- tkentry(fr.windUV, textvariable = dir.NC_V, width = largeur1)
            bt.ncV <- tkbutton(fr.windUV, text = "...")

            tkgrid(txt.ncU, row = 0, column = 0, sticky = 'we', rowspan = 1, columnspan = 3, padx = 1, pady = 1, ipadx = 1, ipady = 1)
            tkgrid(set.ncU, row = 0, column = 3, sticky = 'we', rowspan = 1, columnspan = 2, padx = 1, pady = 1, ipadx = 1, ipady = 1)
            tkgrid(en.ncU, row = 1, column = 0, sticky = 'we', rowspan = 1, columnspan = 4, padx = 0, pady = 1, ipadx = 1, ipady = 1)
            tkgrid(bt.ncU, row = 1, column = 4, sticky = 'we', rowspan = 1, columnspan = 1, padx = 0, pady = 1, ipadx = 1, ipady = 1)
            tkgrid(txt.ncV, row = 2, column = 0, sticky = 'we', rowspan = 1, columnspan = 3, padx = 1, pady = 1, ipadx = 1, ipady = 1)
            tkgrid(set.ncV, row = 2, column = 3, sticky = 'we', rowspan = 1, columnspan = 2, padx = 1, pady = 1, ipadx = 1, ipady = 1)
            tkgrid(en.ncV, row = 3, column = 0, sticky = 'we', rowspan = 1, columnspan = 4, padx = 0, pady = 1, ipadx = 1, ipady = 1)
            tkgrid(bt.ncV, row = 3, column = 4, sticky = 'we', rowspan = 1, columnspan = 1, padx = 0, pady = 1, ipadx = 1, ipady = 1)

            helpWidget(en.ncU, lang.dlg[['tooltip']][['5-2']], lang.dlg[['status']][['5-2']])
            helpWidget(bt.ncU, lang.dlg[['tooltip']][['6']], lang.dlg[['status']][['6']])
            helpWidget(set.ncU, lang.dlg[['tooltip']][['7']], lang.dlg[['status']][['7']])

            helpWidget(en.ncV, lang.dlg[['tooltip']][['5-3']], lang.dlg[['status']][['5-3']])
            helpWidget(bt.ncV, lang.dlg[['tooltip']][['6']], lang.dlg[['status']][['6']])
            helpWidget(set.ncV, lang.dlg[['tooltip']][['7']], lang.dlg[['status']][['7']])

            #####

            tkconfigure(set.ncU, command = function(){
                tcl('wm', 'attributes', tt, topmost = FALSE)
                .cdtData$GalParams[["INPUT.U"]] <- getInfoNetCDFData(tt, .cdtData$GalParams[["INPUT.U"]],
                                                                     trimws(tclvalue(dir.NC_U)))
                tcl('wm', 'attributes', tt, topmost = TRUE)
            })

            tkconfigure(bt.ncU, command = function(){
                tcl('wm', 'attributes', tt, topmost = FALSE)
                dirnc <- tk_choose.dir(getwd(), "")
                tcl('wm', 'attributes', tt, topmost = TRUE)
                tclvalue(dir.NC_U) <- if(!is.na(dirnc)) dirnc else ""
            })

            tkconfigure(set.ncV, command = function(){
                tcl('wm', 'attributes', tt, topmost = FALSE)
                .cdtData$GalParams[["INPUT.V"]] <- getInfoNetCDFData(tt, .cdtData$GalParams[["INPUT.V"]],
                                                                     trimws(tclvalue(dir.NC_V)))
                tcl('wm', 'attributes', tt, topmost = TRUE)
            })

            tkconfigure(bt.ncV, command = function(){
                tcl('wm', 'attributes', tt, topmost = FALSE)
                dirnc <- tk_choose.dir(getwd(), "")
                tcl('wm', 'attributes', tt, topmost = TRUE)
                tclvalue(dir.NC_V) <- if(!is.na(dirnc)) dirnc else ""
            })

            tkgrid(fr.windUV, row = 5, column = 0, sticky = 'we', rowspan = 1, columnspan = 5)
            tcl('update')
        }

        ##########

        tkdestroy(fr.windVar)
        fr.windVar <<- tkframe(frInputData)

        chk.windUV <- tkcheckbutton(fr.windVar, variable = windUV, text = lang.dlg[['checkbutton']][['1']], anchor = 'w', justify = 'left')
        fr.windUV <- tkframe(fr.windVar)

        #####
        tkgrid(chk.windUV, row = 4, column = 0, sticky = 'we', rowspan = 1, columnspan = 5, padx = 1, pady = 1, ipadx = 1, ipady = 1)

        helpWidget(chk.windUV, lang.dlg[['tooltip']][['5-0']], lang.dlg[['status']][['5-0']])

        #####

        if(tclvalue(windUV) == '1') getwindUVone() else getwindUVsep()

        tkbind(chk.windUV, "<Button-1>", function(){
            if(tclvalue(windUV) == '1') getwindUVsep() else getwindUVone()
            if(tclvalue(windUV) == '1') frmtWindDataSep() else frmtWindDataOne()
        })

        tkgrid(fr.windVar, row = 1, column = 0, sticky = 'we', rowspan = 1, columnspan = 2)
    }

    frmtWindSpeed <- function(){
        tkdestroy(fr.outAdjFrmt)
        fr.outAdjFrmt <<- tkframe(frSave)

        txt.frmtS <- tklabel(fr.outAdjFrmt, text = lang.dlg[['label']][['5-1']], anchor = 'w', justify = 'left')
        en.frmtS <- tkentry(fr.outAdjFrmt, textvariable = outFrmtS, width = largeur1)

        tkgrid(txt.frmtS, row = 0, column = 0, sticky = 'we', rowspan = 1, columnspan = 2, padx = 1, pady = 1, ipadx = 1, ipady = 1)
        tkgrid(en.frmtS, row = 1, column = 0, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)

        helpWidget(en.frmtS, lang.dlg[['tooltip']][['9-1']], lang.dlg[['status']][['9-1']])

        tkgrid(fr.outAdjFrmt, row = 2, column = 0, sticky = 'we', rowspan = 1, columnspan = 2)
    }

    frmtWindDataOne <- function(){
        tkdestroy(fr.outAdjFrmt)
        fr.outAdjFrmt <<- tkframe(frSave)

        txt.frmtUV <- tklabel(fr.outAdjFrmt, text = lang.dlg[['label']][['5-4']], anchor = 'w', justify = 'left')
        en.frmtUV <- tkentry(fr.outAdjFrmt, textvariable = outFrmtUV, width = largeur1)

        tkgrid(txt.frmtUV, row = 0, column = 0, sticky = 'we', rowspan = 1, columnspan = 2, padx = 1, pady = 1, ipadx = 1, ipady = 1)
        tkgrid(en.frmtUV, row = 1, column = 0, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)

        helpWidget(en.frmtUV, lang.dlg[['tooltip']][['9-4']], lang.dlg[['status']][['9-4']])

        tkgrid(fr.outAdjFrmt, row = 2, column = 0, sticky = 'we', rowspan = 1, columnspan = 2)
    }

    frmtWindDataSep <- function(){
        tkdestroy(fr.outAdjFrmt)
        fr.outAdjFrmt <<- tkframe(frSave)

        txt.frmtU <- tklabel(fr.outAdjFrmt, text = lang.dlg[['label']][['5-2']], anchor = 'w', justify = 'left')
        en.frmtU <- tkentry(fr.outAdjFrmt, textvariable = outFrmtU, width = largeur1)
        txt.frmtV <- tklabel(fr.outAdjFrmt, text = lang.dlg[['label']][['5-3']], anchor = 'w', justify = 'left')
        en.frmtV <- tkentry(fr.outAdjFrmt, textvariable = outFrmtV, width = largeur1)

        tkgrid(txt.frmtU, row = 0, column = 0, sticky = 'we', rowspan = 1, columnspan = 2, padx = 1, pady = 1, ipadx = 1, ipady = 1)
        tkgrid(en.frmtU, row = 1, column = 0, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
        tkgrid(txt.frmtV, row = 2, column = 0, sticky = 'we', rowspan = 1, columnspan = 2, padx = 1, pady = 1, ipadx = 1, ipady = 1)
        tkgrid(en.frmtV, row = 3, column = 0, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)

        helpWidget(en.frmtU, lang.dlg[['tooltip']][['9-2']], lang.dlg[['status']][['9-2']])
        helpWidget(en.frmtV, lang.dlg[['tooltip']][['9-3']], lang.dlg[['status']][['9-3']])

        tkgrid(fr.outAdjFrmt, row = 2, column = 0, sticky = 'we', rowspan = 1, columnspan = 2)
    }

    ############################################

    frtimestep <- tkframe(frMRG0, relief = 'sunken', borderwidth = 2)

    file.period <- tclVar()
    CbperiodVAL <- .cdtEnv$tcl$lang$global[['combobox']][['1']][3:6]
    periodVAL <- c('daily', 'pentad', 'dekadal', 'monthly')
    tclvalue(file.period) <- CbperiodVAL[periodVAL %in% .cdtData$GalParams$period]

    cb.period <- ttkcombobox(frtimestep, values = CbperiodVAL, textvariable = file.period, justify = 'center', width = largeur0)
    bt.DateRange <- ttkbutton(frtimestep, text = lang.dlg[['button']][['1']], width = largeur0)

    #######

    tkgrid(cb.period, row = 0, column = 0, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
    tkgrid(bt.DateRange, row = 0, column = 1, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)

    helpWidget(cb.period, lang.dlg[['tooltip']][['1']], lang.dlg[['status']][['1']])
    helpWidget(bt.DateRange, lang.dlg[['tooltip']][['2']], lang.dlg[['status']][['2']])

    ###########

    tkconfigure(bt.DateRange, command = function(){
        tstep <- periodVAL[CbperiodVAL %in% trimws(tclvalue(file.period))]
        tcl('wm', 'attributes', tt, topmost = FALSE)
        .cdtData$GalParams[["date.range"]] <- getInfoDateRange(tt, .cdtData$GalParams[["date.range"]], tstep)
        tcl('wm', 'attributes', tt, topmost = TRUE)
    })

    ############################################

    frameBias <- tkframe(frMRG0, relief = 'sunken', borderwidth = 2)

    cb.biasMthd <- lang.dlg[['combobox']][['1']]
    val.biasMthd <- c("mbvar", "mbmon", "qmdist", "qmecdf")

    bias.method <- tclVar()
    tclvalue(bias.method) <- cb.biasMthd[val.biasMthd %in% .cdtData$GalParams$BIAS$method]
    bias.dir <- tclVar(.cdtData$GalParams$BIAS$dir)

    txt.bias <- tklabel(frameBias, text = lang.dlg[['label']][['1']], anchor = 'e', justify = 'right')
    cb.bias <- ttkcombobox(frameBias, values = cb.biasMthd, textvariable = bias.method, width = largeur2)

    txt.bias.dir <- tklabel(frameBias, text = lang.dlg[['label']][['2']], anchor = 'w', justify = 'left')
    en.bias.dir <- tkentry(frameBias, textvariable = bias.dir, width = largeur1)
    bt.bias.dir <- tkbutton(frameBias, text = "...")

    #######

    tkgrid(txt.bias, row = 0, column = 0, sticky = 'e', rowspan = 1, columnspan = 2, padx = 1, pady = 5, ipadx = 1, ipady = 1)
    tkgrid(cb.bias, row = 0, column = 2, sticky = 'we', rowspan = 1, columnspan = 4, padx = 1, pady = 5, ipadx = 1, ipady = 1)

    tkgrid(txt.bias.dir, row = 1, column = 0, sticky = 'we', rowspan = 1, columnspan = 6, padx = 1, pady = 1, ipadx = 1, ipady = 1)
    tkgrid(en.bias.dir, row = 2, column = 0, sticky = 'we', rowspan = 1, columnspan = 5, padx = 0, pady = 1, ipadx = 1, ipady = 1)
    tkgrid(bt.bias.dir, row = 2, column = 5, sticky = 'we', rowspan = 1, columnspan = 1, padx = 0, pady = 1, ipadx = 1, ipady = 1)

    helpWidget(cb.bias, lang.dlg[['tooltip']][['3']], lang.dlg[['status']][['3']])
    helpWidget(en.bias.dir, lang.dlg[['tooltip']][['4']], lang.dlg[['status']][['4']])

    #######

    tkconfigure(bt.bias.dir, command = function(){
        tcl('wm', 'attributes', tt, topmost = FALSE)
        dirbias <- tk_choose.dir(getwd(), "")
        tcl('wm', 'attributes', tt, topmost = TRUE)
        tclvalue(bias.dir) <- if(!is.na(dirbias)) dirbias else ""
    })

    ############################################

    frInputData <- tkframe(frMRG0, relief = 'sunken', borderwidth = 2)

    windVar <- tclVar()
    cbWindVar <- lang.dlg[['combobox']][['2']]
    valWindVar <- c('speed', 'uv-comp')
    tclvalue(windVar) <- cbWindVar[valWindVar %in% .cdtData$GalParams$wvar]

    dir.NC_S <- tclVar(.cdtData$GalParams$INPUT.S$dir)

    windUV <- tclVar(.cdtData$GalParams$one.ncdf)
    dir.NC_U <- tclVar(.cdtData$GalParams$INPUT.U$dir)
    dir.NC_V <- tclVar(.cdtData$GalParams$INPUT.V$dir)
    dir.NC_UV <- tclVar(.cdtData$GalParams$INPUT.UV$dir)

    txt.windVar <- tklabel(frInputData, text = lang.dlg[['label']][['6']], anchor = 'e', justify = 'right')
    cb.windVar <- ttkcombobox(frInputData, values = cbWindVar, textvariable = windVar, justify = 'center', width = largeur0)
    fr.windVar <- tkframe(frInputData)

    if(.cdtData$GalParams$wvar == "speed") getwindSpeed() else getwindData()

    tkgrid(txt.windVar, row = 0, column = 0, sticky = 'e', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
    tkgrid(cb.windVar, row = 0, column = 1, sticky = 'w', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)

    tkbind(cb.windVar, "<<ComboboxSelected>>", function(){
        wvar <- valWindVar[cbWindVar %in% trimws(tclvalue(windVar))]
        if(wvar == "speed") getwindSpeed() else getwindData()
        if(wvar != "speed"){
            if(tclvalue(windUV) == '1') frmtWindDataOne() else frmtWindDataSep()
        }else frmtWindSpeed()
    })

    ############################################

    frSave <- tkframe(frMRG0, relief = 'sunken', borderwidth = 2)

    dir2save <- tclVar(.cdtData$GalParams$output$dir)

    outFrmtS <- tclVar(.cdtData$GalParams$output$format.S)
    outFrmtU <- tclVar(.cdtData$GalParams$output$format.U)
    outFrmtV <- tclVar(.cdtData$GalParams$output$format.V)
    outFrmtUV <- tclVar(.cdtData$GalParams$output$format.UV)

    txt.dir2save <- tklabel(frSave, text = lang.dlg[['label']][['4']], anchor = 'w', justify = 'left')
    en.dir2save <- tkentry(frSave, textvariable = dir2save, width = largeur1)
    bt.dir2save <- tkbutton(frSave, text = "...")

    fr.outAdjFrmt <- tkframe(frSave)

    tkgrid(txt.dir2save, row = 0, column = 0, sticky = 'we', rowspan = 1, columnspan = 2, padx = 1, pady = 2, ipadx = 1, ipady = 1)
    tkgrid(en.dir2save, row = 1, column = 0, sticky = 'we', rowspan = 1, columnspan = 1, padx = 0, pady = 1, ipadx = 1, ipady = 1)
    tkgrid(bt.dir2save, row = 1, column = 1, sticky = 'we', rowspan = 1, columnspan = 1, padx = 0, pady = 1, ipadx = 1, ipady = 1)

    helpWidget(en.dir2save, lang.dlg[['tooltip']][['8']], lang.dlg[['status']][['8']])
    helpWidget(bt.dir2save, lang.dlg[['tooltip']][['6']], lang.dlg[['status']][['6']])

    if(.cdtData$GalParams$wvar != "speed"){
        if(.cdtData$GalParams$one.ncdf) frmtWindDataOne() else frmtWindDataSep()
    }else frmtWindSpeed()

    #####
    tkconfigure(bt.dir2save, command = function(){
        tcl('wm', 'attributes', tt, topmost = FALSE)
        dir2savepth <- tk_choose.dir(.cdtData$GalParams$output$dir, "")
        tcl('wm', 'attributes', tt, topmost = TRUE)
        if(is.na(dir2savepth))
            tclvalue(dir2save) <- .cdtData$GalParams$output$dir
        else{
            dir.create(dir2savepth, showWarnings = FALSE, recursive = TRUE)
            tclvalue(dir2save) <- dir2savepth
        }
    })

    ############################################
    tkgrid(frtimestep, row = 0, column = 0, sticky = '', padx = 1, pady = 1, ipadx = 1, ipady = 1)
    tkgrid(frameBias, row = 1, column = 0, sticky = 'we', padx = 1, pady = 1, ipadx = 1, ipady = 1)
    tkgrid(frInputData, row = 2, column = 0, sticky = 'we', padx = 1, pady = 1, ipadx = 1, ipady = 1)
    tkgrid(frSave, row = 3, column = 0, sticky = 'we', padx = 1, pady = 1, ipadx = 1, ipady = 1)

    ############################################

    bt.prm.OK <- ttkbutton(frMRG1, text = .cdtEnv$tcl$lang$global[['button']][['1']])
    bt.prm.CA <- ttkbutton(frMRG1, text = .cdtEnv$tcl$lang$global[['button']][['2']])

    #######

    tkconfigure(bt.prm.OK, command = function(){
        .cdtData$GalParams$output$dir <- trimws(tclvalue(dir2save))

        if(trimws(tclvalue(dir2save)) %in% c("", "NA")){
            cdt.tkmessageBox(tt, message = lang.dlg[['message']][['3']], icon = "warning", type = "ok")
            tkwait.window(tt)
        }

        .cdtData$GalParams$BIAS$dir <- trimws(tclvalue(bias.dir))

        if(trimws(tclvalue(bias.dir)) %in% c("", "NA")){
            cdt.tkmessageBox(tt, message = lang.dlg[['message']][['2']], icon = "warning", type = "ok")
            tkwait.window(tt)
        }

        .cdtData$GalParams$period <- periodVAL[CbperiodVAL %in% trimws(tclvalue(file.period))]
        .cdtData$GalParams$BIAS$method  <- val.biasMthd[cb.biasMthd %in% trimws(tclvalue(bias.method))]
        .cdtData$GalParams$wvar <- valWindVar[cbWindVar %in% trimws(tclvalue(windVar))]

        if(.cdtData$GalParams$wvar == "speed"){
            .cdtData$GalParams$INPUT.S$dir <- trimws(tclvalue(dir.NC_S))
            .cdtData$GalParams$output$format.S <- trimws(tclvalue(outFrmtS))

            if(.cdtData$GalParams$STN.S == ""){
                cdt.tkmessageBox(tt, message = lang.dlg[['message']][['1-1']], icon = "warning", type = "ok")
                tkwait.window(tt)
            }
        }else{
            .cdtData$GalParams$one.ncdf <- switch(tclvalue(windUV), '0' = FALSE, '1' = TRUE)

            if(.cdtData$GalParams$one.ncdf){
                .cdtData$GalParams$INPUT.UV$dir <- trimws(tclvalue(dir.NC_UV))
                .cdtData$GalParams$output$format.UV <- trimws(tclvalue(outFrmtUV))

                if(.cdtData$GalParams$INPUT.UV$dir %in% c("", "NA")){
                    cdt.tkmessageBox(tt, message = lang.dlg[['message']][['1-4']], icon = "warning", type = "ok")
                    tkwait.window(tt)
                }
            }else{
                .cdtData$GalParams$INPUT.U$dir <- trimws(tclvalue(dir.NC_U))
                .cdtData$GalParams$INPUT.V$dir <- trimws(tclvalue(dir.NC_V))

                .cdtData$GalParams$output$format.U <- trimws(tclvalue(outFrmtU))
                .cdtData$GalParams$output$format.V <- trimws(tclvalue(outFrmtV))

                if(.cdtData$GalParams$INPUT.U$dir %in% c("", "NA")){
                    cdt.tkmessageBox(tt, message = lang.dlg[['message']][['1-2']], icon = "warning", type = "ok")
                    tkwait.window(tt)
                }
                if(.cdtData$GalParams$INPUT.V$dir %in% c("", "NA")){
                    cdt.tkmessageBox(tt, message = lang.dlg[['message']][['1-3']], icon = "warning", type = "ok")
                    tkwait.window(tt)
                }
            }
        }

        .cdtData$GalParams$message <- lang.dlg[['message']]

        tkgrab.release(tt)
        tkdestroy(tt)
        tkfocus(.cdtEnv$tcl$main$win)
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
