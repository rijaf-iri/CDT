
biasCoeffGetInfoClimData <- function(){
    if(WindowsOS()){
        largeur0 <- 22
        largeur1 <- 43
        largeur2 <- 45
        largeur3 <- 35
        largeur4 <- 20
    }else{
        largeur0 <- 22
        largeur1 <- 41
        largeur2 <- 42
        largeur3 <- 35
        largeur4 <- 20
    }

    ####################################

    xml.dlg <- file.path(.cdtDir$dirLocal, "languages", "cdtBias_Coefficient_ClimData_dlgBox.xml")
    lang.dlg <- cdtLanguageParse(xml.dlg, .cdtData$Config$lang.iso)

    ####################################

    tt <- tktoplevel()
    tkgrab.set(tt)
    tkfocus(tt)

    frMRG0 <- tkframe(tt, relief = 'raised', borderwidth = 2, padx = 3, pady = 3)
    frMRG1 <- tkframe(tt)

    ####################################

    region.box <- function(frBox, h){
        txt.boxrg <- tklabel(frBox, text = paste(lang.dlg[['label']][['1']], ":"), width = largeur4, anchor = 'e', justify = 'right')
        txt.boxlo <- tklabel(frBox, text = lang.dlg[['label']][['2']])
        en.boxlo <- tkentry(frBox, textvariable = box.lon, width = 4)
        txt.boxla <- tklabel(frBox, text = lang.dlg[['label']][['3']])
        en.boxla <- tkentry(frBox, textvariable = box.lat, width = 4)

        tkgrid(txt.boxrg, row = 0, column = 0, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
        tkgrid(txt.boxlo, row = 0, column = 1, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
        tkgrid(en.boxlo, row = 0, column = 2, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
        tkgrid(txt.boxla, row = 0, column = 3, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
        tkgrid(en.boxla, row = 0, column = 4, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)

        helpWidget(en.boxlo, lang.dlg[['tooltip']][[h]], lang.dlg[['status']][[h]])
        helpWidget(en.boxla, lang.dlg[['tooltip']][[h]], lang.dlg[['status']][[h]])
    }

    ncdata.chunks <- function(frBox){
        stateC <- if(tclvalue(chunks.exist) == "1") 'normal' else 'disabled'

        chk.ncdata <- tkcheckbutton(frBox, variable = chunks.exist, text = lang.dlg[['checkbutton']][['1']], anchor = 'w', justify = 'left')
        txt.ncdata <- tklabel(frBox, text = lang.dlg[['label']][['11']], anchor = 'e', justify = 'right')
        en.ncdata <- tkentry(frBox, textvariable = chunks.dir, width = largeur2, state = stateC)
        bt.ncdata <- tkbutton(frBox, text = "...", state = stateC)

        tkgrid(chk.ncdata, row = 0, column = 0, sticky = 'w', rowspan = 1, columnspan = 2, padx = 1, pady = 2, ipadx = 1, ipady = 1)
        tkgrid(txt.ncdata, row = 1, column = 0, sticky = 'we', rowspan = 1, columnspan = 2, padx = 1, pady = 2, ipadx = 1, ipady = 1)
        tkgrid(en.ncdata, row = 2, column = 0, sticky = 'we', rowspan = 1, columnspan = 1, pady = 1, ipady = 1)
        tkgrid(bt.ncdata, row = 2, column = 1, sticky = 'we', rowspan = 1, columnspan = 1, pady = 1, ipady = 1)

        helpWidget(chk.ncdata, lang.dlg[['tooltip']][['17']], lang.dlg[['status']][['17']])
        helpWidget(en.ncdata, lang.dlg[['tooltip']][['18']], lang.dlg[['status']][['18']])
        helpWidget(bt.ncdata, lang.dlg[['tooltip']][['19']], lang.dlg[['status']][['19']])

        tkconfigure(bt.ncdata, command = function(){
            tcl('wm', 'attributes', tt, topmost = FALSE)
            dirnc <- tk_choose.dir(getwd(), "")
            tcl('wm', 'attributes', tt, topmost = TRUE)
            tclvalue(chunks.dir) <- if(!is.na(dirnc)) dirnc else ""
        })

        tkbind(chk.ncdata, "<Button-1>", function(){
            stateC <- if(tclvalue(chunks.exist) == "0") 'normal' else 'disabled'
            tkconfigure(en.ncdata, state = stateC)
            tkconfigure(bt.ncdata, state = stateC)

            stateNC <- if(tclvalue(chunks.exist) == "1") 'normal' else 'disabled'
            tkconfigure(set.InNCDF, state = stateNC)
            tkconfigure(en.InNCDF, state = stateNC)
            tkconfigure(bt.InNCDF, state = stateNC)
        })
    }

    bias_method_inputs <- function(biasmthd){
        tkdestroy(fr.biasOpts)
        fr.biasOpts <<- tkframe(frameBias)

        if(biasmthd == "qmecdf"){
            region.box(fr.biasOpts, '1')
        }

        if(biasmthd == "qmdist"){
            txt.distr <- tklabel(fr.biasOpts, text = lang.dlg[['label']][['9']], anchor = 'e', justify = 'right')
            cb.distr <- ttkcombobox(fr.biasOpts, values = cb.distrName, textvariable = distr.name, width = largeur0, justify = 'center')
            txt.supp <- tklabel(fr.biasOpts, text = lang.dlg[['label']][['10']], anchor = 'e', justify = 'right')
            cb.supp <- ttkcombobox(fr.biasOpts, values = cb.tsSupport, textvariable = ts.support, width = largeur0, justify = 'center')
            fr.box <- tkframe(fr.biasOpts, relief = 'groove', borderwidth = 2)

            tsSupp <- val.tsSupport[cb.tsSupport %in% trimws(tclvalue(ts.support))]
            if(tsSupp == "rectbox") region.box(fr.box, '16') else ncdata.chunks(fr.box)

            tkgrid(txt.distr, row = 0, column = 0, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 2, ipadx = 1, ipady = 1)
            tkgrid(cb.distr, row = 0, column = 1, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 2, ipadx = 1, ipady = 1)
            tkgrid(txt.supp, row = 1, column = 0, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 2, ipadx = 1, ipady = 1)
            tkgrid(cb.supp, row = 1, column = 1, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 2, ipadx = 1, ipady = 1)
            tkgrid(fr.box, row = 2, column = 0, sticky = 'we', rowspan = 1, columnspan = 2, padx = 1, pady = 1, ipadx = 1, ipady = 1)

            helpWidget(cb.distr, lang.dlg[['tooltip']][['2']], lang.dlg[['status']][['2']])
            helpWidget(cb.supp, lang.dlg[['tooltip']][['15']], lang.dlg[['status']][['15']])

            tkbind(cb.supp, "<<ComboboxSelected>>", function(){
                tsSupp <- val.tsSupport[cb.tsSupport %in% trimws(tclvalue(ts.support))]
                tkdestroy(fr.box)

                fr.box <<- tkframe(fr.biasOpts, relief = 'groove', borderwidth = 2)
                if(tsSupp == "rectbox") region.box(fr.box, '16') else ncdata.chunks(fr.box)
                tkgrid(fr.box, row = 2, column = 0, sticky = 'we', rowspan = 1, columnspan = 2, padx = 1, pady = 1, ipadx = 1, ipady = 1)

                if(tsSupp == "points"){
                    stateNC <- if(tclvalue(chunks.exist) == "0") 'normal' else 'disabled'
                }else stateNC <- 'normal'
                tkconfigure(set.InNCDF, state = stateNC)
                tkconfigure(en.InNCDF, state = stateNC)
                tkconfigure(bt.InNCDF, state = stateNC)

                stateInterp <- if(tsSupp == 'rectbox') "disabled" else "normal"
                tkconfigure(bt.bias.interp, state = stateInterp)
            })
        }

        if(biasmthd %in% c("qmecdf", "qmdist"))
            tkgrid(fr.biasOpts, row = 2, column = 0, sticky = 'we', rowspan = 1, columnspan = 2, padx = 1, pady = 1, ipadx = 1, ipady = 1)
    }

    ####################################

    frtimestep <- tkframe(frMRG0, relief = 'sunken', borderwidth = 2, pady = 3)

    file.period <- tclVar()
    CbperiodVAL <- .cdtEnv$tcl$lang$global[['combobox']][['1']][3:6]
    periodVAL <- c('daily', 'pentad', 'dekadal', 'monthly')
    tclvalue(file.period) <- CbperiodVAL[periodVAL %in% .cdtData$GalParams$period]

    cb.period <- ttkcombobox(frtimestep, values = CbperiodVAL, textvariable = file.period, width = largeur0)
    bt.baseBias <- ttkbutton(frtimestep, text = lang.dlg[['button']][['1']], width = largeur0)

    #######

    tkgrid(cb.period, row = 0, column = 0, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
    tkgrid(bt.baseBias, row = 0, column = 1, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)

    helpWidget(cb.period, lang.dlg[['tooltip']][['3']], lang.dlg[['status']][['3']])
    helpWidget(bt.baseBias, lang.dlg[['tooltip']][['4']], lang.dlg[['status']][['4']])

    #######

    tkconfigure(bt.baseBias, command = function(){
        tcl('wm', 'attributes', tt, topmost = FALSE)
        .cdtData$GalParams[["base.period"]] <- getInfoBasePeriod(tt, .cdtData$GalParams[["base.period"]])
        tcl('wm', 'attributes', tt, topmost = TRUE)
    })

    ####################################

    frInputData <- tkframe(frMRG0, relief = 'sunken', borderwidth = 2)

    file.stnfl <- tclVar(.cdtData$GalParams$STN.file)
    dir.InNCDF <- tclVar(.cdtData$GalParams$INPUT$dir)

    stateNC <- if(.cdtData$GalParams$BIAS$method == 'qmdist' &&
                  .cdtData$GalParams$BIAS$ts.support == 'points' &&
                  .cdtData$GalParams$BIAS$chunks.exist) 'disabled' else 'normal'

    txt.stnfl <- tklabel(frInputData, text = lang.dlg[['label']][['4']], anchor = 'w', justify = 'left')
    cb.stnfl <- ttkcombobox(frInputData, values = unlist(openFile_ttkcomboList()), textvariable = file.stnfl, width = largeur1)
    addTo_all_Combobox_List(cb.stnfl)
    bt.stnfl <- tkbutton(frInputData, text = "...")
    txt.InNCDF <- tklabel(frInputData, text = lang.dlg[['label']][['5']], anchor = 'w', justify = 'left')
    set.InNCDF <- ttkbutton(frInputData, text = .cdtEnv$tcl$lang$global[['button']][['5']], state = stateNC)
    en.InNCDF <- tkentry(frInputData, textvariable = dir.InNCDF, width = largeur2, state = stateNC)
    bt.InNCDF <- tkbutton(frInputData, text = "...", state = stateNC)

    ######
    tkgrid(txt.stnfl, row = 0, column = 0, sticky = 'we', rowspan = 1, columnspan = 5, padx = 1, pady = 0, ipadx = 1, ipady = 1)
    tkgrid(cb.stnfl, row = 1, column = 0, sticky = 'we', rowspan = 1, columnspan = 4, padx = 0, pady = 0, ipadx = 1, ipady = 1)
    tkgrid(bt.stnfl, row = 1, column = 4, sticky = 'we', rowspan = 1, columnspan = 1, padx = 0, pady = 0, ipadx = 1, ipady = 1)

    tkgrid(txt.InNCDF, row = 2, column = 0, sticky = 'we', rowspan = 1, columnspan = 3, padx = 1, pady = 0, ipadx = 1, ipady = 1)
    tkgrid(set.InNCDF, row = 2, column = 3, sticky = 'we', rowspan = 1, columnspan = 2, padx = 1, pady = 0, ipadx = 1, ipady = 1)
    tkgrid(en.InNCDF, row = 3, column = 0, sticky = 'we', rowspan = 1, columnspan = 4, padx = 0, pady = 0, ipadx = 1, ipady = 1)
    tkgrid(bt.InNCDF, row = 3, column = 4, sticky = 'we', rowspan = 1, columnspan = 1, padx = 0, pady = 0, ipadx = 1, ipady = 1)

    helpWidget(cb.stnfl, lang.dlg[['tooltip']][['5']], lang.dlg[['status']][['5']])
    helpWidget(bt.stnfl, lang.dlg[['tooltip']][['6']], lang.dlg[['status']][['6']])
    helpWidget(en.InNCDF, lang.dlg[['tooltip']][['7']], lang.dlg[['status']][['7']])
    helpWidget(bt.InNCDF, lang.dlg[['tooltip']][['8']], lang.dlg[['status']][['8']])
    helpWidget(set.InNCDF, lang.dlg[['tooltip']][['9']], lang.dlg[['status']][['9']])

    ######
    tkconfigure(bt.stnfl, command = function(){
        tcl('wm', 'attributes', tt, topmost = FALSE)
        dat.opfiles <- getOpenFiles(tt)
        tcl('wm', 'attributes', tt, topmost = TRUE)
        if(!is.null(dat.opfiles)){
            update.OpenFiles('ascii', dat.opfiles)
            tclvalue(file.stnfl) <- dat.opfiles[[1]]
        }
    })

    settingSNC <- .cdtData$GalParams$settingSNC
    tkconfigure(set.InNCDF, command = function(){
        tcl('wm', 'attributes', tt, topmost = FALSE)
        .cdtData$GalParams[["INPUT"]] <- getInfoNetCDFData(tt, .cdtData$GalParams[["INPUT"]],
                                                           trimws(tclvalue(dir.InNCDF)))
        tcl('wm', 'attributes', tt, topmost = TRUE)
        settingSNC <<- 1
    })

    tkconfigure(bt.InNCDF, command = function(){
        tcl('wm', 'attributes', tt, topmost = FALSE)
        dirnc <- tk_choose.dir(getwd(), "")
        tcl('wm', 'attributes', tt, topmost = TRUE)
        tclvalue(dir.InNCDF) <- if(!is.na(dirnc)) dirnc else ""
    })

    ####################################

    frSave <- tkframe(frMRG0, relief = 'sunken', borderwidth = 2)

    dir2save <- tclVar(.cdtData$GalParams$output$dir)

    txt.dir2save <- tklabel(frSave, text = lang.dlg[['label']][['6']], anchor = 'w', justify = 'left')
    en.dir2save <- tkentry(frSave, textvariable = dir2save, width = largeur2)
    bt.dir2save <- tkbutton(frSave, text = "...")

    #####

    tkgrid(txt.dir2save, row = 0, column = 0, sticky = 'we', rowspan = 1, columnspan = 2, padx = 1, pady = 2, ipadx = 1, ipady = 1)
    tkgrid(en.dir2save, row = 1, column = 0, sticky = 'we', rowspan = 1, columnspan = 1, padx = 0, pady = 1, ipadx = 1, ipady = 1)
    tkgrid(bt.dir2save, row = 1, column = 1, sticky = 'we', rowspan = 1, columnspan = 1, padx = 0, pady = 1, ipadx = 1, ipady = 1)

    helpWidget(en.dir2save, lang.dlg[['tooltip']][['10']], lang.dlg[['status']][['10']])
    helpWidget(bt.dir2save, lang.dlg[['tooltip']][['8']], lang.dlg[['status']][['8']])

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

    ####################################

    frameBias <- tkframe(frMRG0, relief = 'sunken', borderwidth = 2, pady = 3)

    cb.biasMthd <- lang.dlg[['combobox']][['1']]
    val.biasMthd <- c("mbvar", "mbmon", "qmdist", "qmecdf")
    bias.method <- tclVar()
    tclvalue(bias.method) <- cb.biasMthd[val.biasMthd %in% .cdtData$GalParams$BIAS$method]

    min.length <- tclVar(.cdtData$GalParams$BIAS$min.length)
    box.lon <- tclVar(.cdtData$GalParams$BIAS$blon)
    box.lat <- tclVar(.cdtData$GalParams$BIAS$blat)

    cb.distrName <- lang.dlg[['combobox']][['2']]
    val.distrName <- c('norm', 'lnorm', 'snorm', 'gamma', 'exp', 'weibull', 'gumbel',
                       'berngamma', 'bernexp', 'bernlnorm', 'bernweibull')
    distr.name <- tclVar()
    tclvalue(distr.name) <- cb.distrName[val.distrName %in% .cdtData$GalParams$BIAS$distr.name]

    cb.tsSupport <- lang.dlg[['combobox']][['3']]
    val.tsSupport <- c('rectbox', 'points')
    ts.support <- tclVar()
    tclvalue(ts.support) <- cb.tsSupport[val.tsSupport %in% .cdtData$GalParams$BIAS$ts.support]

    chunks.exist <- tclVar(.cdtData$GalParams$BIAS$chunks.exist)
    chunks.dir <- tclVar(.cdtData$GalParams$BIAS$chunks.dir)

    txt.bias <- tklabel(frameBias, text = lang.dlg[['label']][['7']], anchor = 'e', justify = 'right')
    cb.bias <- ttkcombobox(frameBias, values = cb.biasMthd, textvariable = bias.method, width = largeur3)

    fr.minstn <- tkframe(frameBias)
    txt.minstn <- tklabel(fr.minstn, text = lang.dlg[['label']][['8']], anchor = 'e', justify = 'right')
    en.minstn <- tkentry(fr.minstn, textvariable = min.length, width = 6)

    ########

    fr.biasOpts <- tkframe(frameBias)

    bias_method_inputs(.cdtData$GalParams$BIAS$method)

    ########
    tkgrid(txt.minstn, row = 0, column = 0, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
    tkgrid(en.minstn, row = 0, column = 1, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)

    tkgrid(txt.bias, row = 0, column = 0, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
    tkgrid(cb.bias, row = 0, column = 1, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
    tkgrid(fr.minstn, row = 1, column = 0, sticky = 'e', rowspan = 1, columnspan = 2, padx = 1, pady = 1, ipadx = 1, ipady = 1)
    tkgrid(fr.biasOpts, row = 2, column = 0, sticky = 'we', rowspan = 1, columnspan = 2, padx = 1, pady = 1, ipadx = 1, ipady = 1)

    helpWidget(cb.bias, lang.dlg[['tooltip']][['11']], lang.dlg[['status']][['11']])
    helpWidget(en.minstn, lang.dlg[['tooltip']][['12']], lang.dlg[['status']][['12']])

    ########

    tkbind(cb.bias, "<<ComboboxSelected>>", function(){
        bsmethod <- val.biasMthd[cb.biasMthd %in% trimws(tclvalue(bias.method))]
        bias_method_inputs(bsmethod)

        tsSupp <- val.tsSupport[cb.tsSupport %in% trimws(tclvalue(ts.support))]
        if(bsmethod %in% c("qmdist", "qmecdf")){
            if(bsmethod == "qmecdf") stateInterp <- "disabled"
            if(bsmethod == "qmdist"){
                stateInterp <- if(tsSupp == 'rectbox') "disabled" else "normal"
            }
        }else{
            stateInterp <- "normal"
        }

        tkconfigure(bt.bias.interp, state = stateInterp)

        stateNC <- if(bsmethod == "qmdist" && tsSupp == 'points') 'disabled' else 'normal'
        tkconfigure(set.InNCDF, state = stateNC)
        tkconfigure(en.InNCDF, state = stateNC)
        tkconfigure(bt.InNCDF, state = stateNC)
    })

    ####################################

    stateInterp <- if(.cdtData$GalParams$BIAS$method == "qmecdf") "disabled" else "normal"

    bt.bias.interp <- ttkbutton(frMRG0, text = lang.dlg[['button']][['2']], state = stateInterp)
    bt.grid.interp <- ttkbutton(frMRG0, text = lang.dlg[['button']][['3']])

    helpWidget(bt.bias.interp, lang.dlg[['tooltip']][['13']], lang.dlg[['status']][['13']])
    helpWidget(bt.grid.interp, lang.dlg[['tooltip']][['14']], lang.dlg[['status']][['14']])

    tkconfigure(bt.bias.interp, command = function(){
        tcl('wm', 'attributes', tt, topmost = FALSE)
        .cdtData$GalParams[["interp"]] <- getInterpolationPars(tt, .cdtData$GalParams[["interp"]], group = 1)
        tcl('wm', 'attributes', tt, topmost = TRUE)
    })

    tkconfigure(bt.grid.interp, command = function(){
        tcl('wm', 'attributes', tt, topmost = FALSE)
        .cdtData$GalParams[["grid"]] <- createGridInterpolation(tt, .cdtData$GalParams[["grid"]])
        tcl('wm', 'attributes', tt, topmost = TRUE)
    })

    ####################################

    tkgrid(frtimestep, row = 0, column = 0, sticky = 'we', padx = 1, pady = 1, ipadx = 1, ipady = 1)
    tkgrid(frInputData, row = 1, column = 0, sticky = 'we', padx = 1, pady = 1, ipadx = 1, ipady = 1)
    tkgrid(frSave, row = 2, column = 0, sticky = 'we', padx = 1, pady = 1, ipadx = 1, ipady = 1)
    tkgrid(frameBias, row = 3, column = 0, sticky = 'we', padx = 1, pady = 1, ipadx = 1, ipady = 1)
    tkgrid(bt.bias.interp, row = 4, column = 0, sticky = 'we', padx = 1, pady = 1, ipadx = 1, ipady = 1)
    tkgrid(bt.grid.interp, row = 5, column = 0, sticky = 'we', padx = 1, pady = 1, ipadx = 1, ipady = 1)

    ############################################

    bt.prm.Opt <- ttkbutton(frMRG1, text = .cdtEnv$tcl$lang$global[['button']][['4']])
    bt.prm.OK <- ttkbutton(frMRG1, text = .cdtEnv$tcl$lang$global[['button']][['1']])
    bt.prm.CA <- ttkbutton(frMRG1, text = .cdtEnv$tcl$lang$global[['button']][['2']])

    #######

    tkconfigure(bt.prm.Opt, command = function(){
        tssupp <- val.tsSupport[cb.tsSupport %in% trimws(tclvalue(ts.support))]
        biasmth <- val.biasMthd[cb.biasMthd %in% trimws(tclvalue(bias.method))]
        distname <- val.distrName[cb.distrName %in% trimws(tclvalue(distr.name))]
        distList <- c('berngamma', 'bernexp', 'bernlnorm', 'bernweibull')
        bernoulli <- biasmth == "qmdist" && distname %in% distList
        interp <- (biasmth %in% c("mbvar", "mbmon") ||
                  (biasmth == "qmdist" && tssupp == "points")) &&
                  .cdtData$GalParams$interp$method %in% c('idw', 'okr')
        mbias <- biasmth %in% c("mbvar", "mbmon")
        rectbox <- biasmth == "qmecdf" || (biasmth == "qmdist" && tssupp == "rectbox")

        biasCoeff_Options(tt, mbias, bernoulli, interp, rectbox)
    })

    tkconfigure(bt.prm.OK, command = function(){
        if(trimws(tclvalue(file.stnfl)) == ""){
            cdt.tkmessageBox(tt, message = lang.dlg[['message']][['1']], icon = "warning", type = "ok")
            tkwait.window(tt)
        }else if(trimws(tclvalue(dir.InNCDF)) %in% c("", "NA")){
            cdt.tkmessageBox(tt, message = lang.dlg[['message']][['2']], icon = "warning", type = "ok")
            tkwait.window(tt)
        }else if(is.null(settingSNC) &&
                 !(trimws(tclvalue(bias.method)) == cb.biasMthd[3] &&
                 trimws(tclvalue(ts.support)) == cb.tsSupport[2] &&
                 tclvalue(chunks.exist) == '1'))
        {
            cdt.tkmessageBox(tt, message = lang.dlg[['message']][['3']], icon = "warning", type = "ok")
            tkwait.window(tt)
        }else if(trimws(tclvalue(dir2save)) %in% c("", "NA")){
            cdt.tkmessageBox(tt, message = lang.dlg[['message']][['4']], icon = "warning", type = "ok")
            tkwait.window(tt)
        }else if((trimws(tclvalue(chunks.dir)) %in% c("", "NA")) &&
                 (trimws(tclvalue(bias.method)) == cb.biasMthd[3] &&
                 trimws(tclvalue(ts.support)) == cb.tsSupport[2] &&
                 tclvalue(chunks.exist) == '1'))
        {
            cdt.tkmessageBox(tt, message = lang.dlg[['message']][['13']], icon = "warning", type = "ok")
            tkwait.window(tt)
        }else{
            .cdtData$GalParams$period <- periodVAL[CbperiodVAL %in% trimws(tclvalue(file.period))]

            .cdtData$GalParams$STN.file <- trimws(tclvalue(file.stnfl))
            .cdtData$GalParams$INPUT$dir <- trimws(tclvalue(dir.InNCDF))
            .cdtData$GalParams$output$dir <- trimws(tclvalue(dir2save))
            .cdtData$GalParams$BIAS$method <- val.biasMthd[cb.biasMthd %in% trimws(tclvalue(bias.method))]
            .cdtData$GalParams$BIAS$min.length <- as.numeric(trimws(tclvalue(min.length)))
            .cdtData$GalParams$BIAS$blon <- as.numeric(trimws(tclvalue(box.lon)))
            .cdtData$GalParams$BIAS$blat <- as.numeric(trimws(tclvalue(box.lat)))
            .cdtData$GalParams$BIAS$distr.name <- val.distrName[cb.distrName %in% trimws(tclvalue(distr.name))]
            .cdtData$GalParams$BIAS$ts.support <- val.tsSupport[cb.tsSupport %in% trimws(tclvalue(ts.support))]

            .cdtData$GalParams$BIAS$chunks.exist <- if(tclvalue(chunks.exist) == '1') TRUE else FALSE
            .cdtData$GalParams$BIAS$chunks.dir <- trimws(tclvalue(chunks.dir))

            .cdtData$GalParams$settingSNC <- settingSNC
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

    tkgrid(bt.prm.Opt, row = 0, column = 0, sticky = 'w', padx = 5, pady = 1, ipadx = 1, ipady = 1)
    tkgrid(bt.prm.OK, row = 0, column = 1, sticky = '', padx = 5, pady = 1, ipadx = 1, ipady = 1)
    tkgrid(bt.prm.CA, row = 0, column = 2, sticky = 'e', padx = 5, pady = 1, ipadx = 1, ipady = 1)

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
