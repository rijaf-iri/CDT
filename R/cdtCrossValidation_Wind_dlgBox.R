
crossValidationInfoWind <- function(){
    listOpenFiles <- openFile_ttkcomboList()
    if(WindowsOS()){
        largeur0 <- 23
        largeur1 <- 47
        largeur2 <- 49
        largeur3 <- 32
        largeur4 <- 18
        largeur5 <- 28
    }else{
        largeur0 <- 23
        largeur1 <- 43
        largeur2 <- 45
        largeur3 <- 32
        largeur4 <- 17
        largeur5 <- 27
    }

    ####################################

    xml.dlg <- file.path(.cdtDir$dirLocal, "languages", "cdtCrossValidation_Wind_dlgBox.xml")
    lang.dlg <- cdtLanguageParse(xml.dlg, .cdtData$Config$lang.iso)

    ####################################

    tt <- tktoplevel()
    tkgrab.set(tt)
    tkfocus(tt)

    frMRG0 <- tkframe(tt, relief = 'raised', borderwidth = 2, padx = 2, pady = 2)
    frMRG1 <- tkframe(tt)

    ####################################

    frtimestep <- tkframe(frMRG0, relief = 'sunken', borderwidth = 2)

    file.period <- tclVar()
    CbperiodVAL <- .cdtEnv$tcl$lang$global[['combobox']][['1']][3:6]
    periodVAL <- c('daily', 'pentad', 'dekadal', 'monthly')
    tclvalue(file.period) <- CbperiodVAL[periodVAL %in% .cdtData$GalParams$period]

    cb.period <- ttkcombobox(frtimestep, values = CbperiodVAL, textvariable = file.period, width = largeur0)
    bt.DateRange <- ttkbutton(frtimestep, text = lang.dlg[['button']][['1']], width = largeur0)

    #######

    tkgrid(cb.period, row = 0, column = 0, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
    tkgrid(bt.DateRange, row = 0, column = 1, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)

    helpWidget(cb.period, lang.dlg[['tooltip']][['1']], lang.dlg[['status']][['1']])
    helpWidget(bt.DateRange, lang.dlg[['tooltip']][['2']], lang.dlg[['status']][['2']])

    ###########

    tkconfigure(bt.DateRange, command = function(){
        tstep <- periodVAL[CbperiodVAL %in% str_trim(tclvalue(file.period))]
        tcl('wm', 'attributes', tt, topmost = FALSE)
        .cdtData$GalParams[["date.range"]] <- getInfoDateRange(tt, .cdtData$GalParams[["date.range"]], tstep)
        tcl('wm', 'attributes', tt, topmost = TRUE)
    })

    ############################################

    frInputData <- tkframe(frMRG0, relief = 'sunken', borderwidth = 2)

    file.stnfl <- tclVar(.cdtData$GalParams$STN.file)
    dir.InNCDF <- tclVar(.cdtData$GalParams$INPUT$dir)

    txt.stnfl <- tklabel(frInputData, text = lang.dlg[['label']][['1']], anchor = 'w', justify = 'left')
    cb.stnfl <- ttkcombobox(frInputData, values = unlist(listOpenFiles), textvariable = file.stnfl, width = largeur1)
    bt.stnfl <- tkbutton(frInputData, text = "...")
    txt.InNCDF <- tklabel(frInputData, text = lang.dlg[['label']][['2']], anchor = 'w', justify = 'left')
    set.InNCDF <- ttkbutton(frInputData, text = .cdtEnv$tcl$lang$global[['button']][['5']])
    en.InNCDF <- tkentry(frInputData, textvariable = dir.InNCDF, width = largeur2)
    bt.InNCDF <- tkbutton(frInputData, text = "...")

    ######
    tkgrid(txt.stnfl, row = 0, column = 0, sticky = 'we', rowspan = 1, columnspan = 5, padx = 1, pady = 0, ipadx = 1, ipady = 1)
    tkgrid(cb.stnfl, row = 1, column = 0, sticky = 'we', rowspan = 1, columnspan = 4, padx = 0, pady = 0, ipadx = 1, ipady = 1)
    tkgrid(bt.stnfl, row = 1, column = 4, sticky = 'we', rowspan = 1, columnspan = 1, padx = 0, pady = 0, ipadx = 1, ipady = 1)

    tkgrid(txt.InNCDF, row = 2, column = 0, sticky = 'we', rowspan = 1, columnspan = 3, padx = 1, pady = 0, ipadx = 1, ipady = 1)
    tkgrid(set.InNCDF, row = 2, column = 3, sticky = 'we', rowspan = 1, columnspan = 2, padx = 1, pady = 0, ipadx = 1, ipady = 1)
    tkgrid(en.InNCDF, row = 3, column = 0, sticky = 'we', rowspan = 1, columnspan = 4, padx = 0, pady = 0, ipadx = 1, ipady = 1)
    tkgrid(bt.InNCDF, row = 3, column = 4, sticky = 'we', rowspan = 1, columnspan = 1, padx = 0, pady = 0, ipadx = 1, ipady = 1)

    helpWidget(cb.stnfl, lang.dlg[['tooltip']][['3']], lang.dlg[['status']][['3']])
    helpWidget(bt.stnfl, lang.dlg[['tooltip']][['4']], lang.dlg[['status']][['4']])
    helpWidget(en.InNCDF, lang.dlg[['tooltip']][['5']], lang.dlg[['status']][['5']])
    helpWidget(bt.InNCDF, lang.dlg[['tooltip']][['6']], lang.dlg[['status']][['6']])
    helpWidget(set.InNCDF, lang.dlg[['tooltip']][['7']], lang.dlg[['status']][['7']])

    ######
    tkconfigure(bt.stnfl, command = function(){
        tcl('wm', 'attributes', tt, topmost = FALSE)
        dat.opfiles <- getOpenFiles(tt)
        tcl('wm', 'attributes', tt, topmost = TRUE)
        if(!is.null(dat.opfiles)){
            update.OpenFiles('ascii', dat.opfiles)
            listOpenFiles[[length(listOpenFiles) + 1]] <<- dat.opfiles[[1]]
            tclvalue(file.stnfl) <- dat.opfiles[[1]]
            lapply(list(cb.stnfl, cb.grddem, cb.infile), tkconfigure, values = unlist(listOpenFiles))
        }
    })

    settingSNC <- .cdtData$GalParams$settingSNC
    tkconfigure(set.InNCDF, command = function(){
        tcl('wm', 'attributes', tt, topmost = FALSE)
        .cdtData$GalParams[["INPUT"]] <- getInfoNetCDFData(tt, .cdtData$GalParams[["INPUT"]],
                                                           str_trim(tclvalue(dir.InNCDF)))
        tcl('wm', 'attributes', tt, topmost = TRUE)
        settingSNC <<- 1
    })

    tkconfigure(bt.InNCDF, command = function(){
        tcl('wm', 'attributes', tt, topmost = FALSE)
        dirnc <- tk_choose.dir(getwd(), "")
        tcl('wm', 'attributes', tt, topmost = TRUE)
        tclvalue(dir.InNCDF) <- if(!is.na(dirnc)) dirnc else ""
    })

    ############################################

    frSave <- tkframe(frMRG0, relief = 'sunken', borderwidth = 2)

    dir2save <- tclVar(.cdtData$GalParams$outdir)

    txt.dir2save <- tklabel(frSave, text = lang.dlg[['label']][['3']], anchor = 'w', justify = 'left')
    en.dir2save <- tkentry(frSave, textvariable = dir2save, width = largeur2)
    bt.dir2save <- tkbutton(frSave, text = "...")

    #####

    tkgrid(txt.dir2save, row = 0, column = 0, sticky = 'we', rowspan = 1, columnspan = 2, padx = 1, pady = 1, ipadx = 1, ipady = 1)
    tkgrid(en.dir2save, row = 1, column = 0, sticky = 'we', rowspan = 1, columnspan = 1, padx = 0, pady = 1, ipadx = 1, ipady = 1)
    tkgrid(bt.dir2save, row = 1, column = 1, sticky = 'we', rowspan = 1, columnspan = 1, padx = 0, pady = 1, ipadx = 1, ipady = 1)

    helpWidget(en.dir2save, lang.dlg[['tooltip']][['8']], lang.dlg[['status']][['8']])
    helpWidget(bt.dir2save, lang.dlg[['tooltip']][['6']], lang.dlg[['status']][['6']])

    #####

    tkconfigure(bt.dir2save, command = function(){
        tcl('wm', 'attributes', tt, topmost = FALSE)
        dir2savepth <- tk_choose.dir(.cdtData$GalParams$outdir, "")
        tcl('wm', 'attributes', tt, topmost = TRUE)
        if(is.na(dir2savepth))
            tclvalue(dir2save) <- .cdtData$GalParams$outdir
        else{
            dir.create(dir2savepth, showWarnings = FALSE, recursive = TRUE)
            tclvalue(dir2save) <- dir2savepth
        }
    })

    ############################################

        auxiliary.variables <- function(mrgmethod){
            tkdestroy(frauxvar)

            frauxvar <<- ttklabelframe(frMerge, text = lang.dlg[['label']][['5']], relief = 'groove', borderwidth = 2)

            if(mrgmethod == "RK"){
                frAUX <- tkframe(frauxvar)

                dem.chk.auxvar <- tkcheckbutton(frAUX, variable = dem.auxvar, text = lang.dlg[['checkbutton']][['1']], anchor = 'w', justify = 'left')
                slope.chk.auxvar <- tkcheckbutton(frAUX, variable = slope.auxvar, text = lang.dlg[['checkbutton']][['2']], anchor = 'w', justify = 'left')
                aspect.chk.auxvar <- tkcheckbutton(frAUX, variable = aspect.auxvar, text = lang.dlg[['checkbutton']][['3']], anchor = 'w', justify = 'left')
                lon.chk.auxvar <- tkcheckbutton(frAUX, variable = lon.auxvar, text = lang.dlg[['checkbutton']][['4']], anchor = 'w', justify = 'left')
                lat.chk.auxvar <- tkcheckbutton(frAUX, variable = lat.auxvar, text = lang.dlg[['checkbutton']][['5']], anchor = 'w', justify = 'left')

                tkgrid(dem.chk.auxvar, row = 0, column = 0, sticky = 'we', rowspan = 1, columnspan = 1, ipady = 1)
                tkgrid(slope.chk.auxvar, row = 0, column = 1, sticky = 'we', rowspan = 1, columnspan = 1, ipady = 1)
                tkgrid(aspect.chk.auxvar, row = 0, column = 2, sticky = 'we', rowspan = 1, columnspan = 1, ipady = 1)
                tkgrid(lon.chk.auxvar, row = 0, column = 3, sticky = 'we', rowspan = 1, columnspan = 1, ipady = 1)
                tkgrid(lat.chk.auxvar, row = 0, column = 4, sticky = 'we', rowspan = 1, columnspan = 1, ipady = 1)

                helpWidget(dem.chk.auxvar, lang.dlg[['tooltip']][['10']], lang.dlg[['status']][['10']])
                helpWidget(slope.chk.auxvar, lang.dlg[['tooltip']][['11']], lang.dlg[['status']][['11']])
                helpWidget(aspect.chk.auxvar, lang.dlg[['tooltip']][['12']], lang.dlg[['status']][['12']])
                helpWidget(lon.chk.auxvar, lang.dlg[['tooltip']][['13']], lang.dlg[['status']][['13']])
                helpWidget(lat.chk.auxvar, lang.dlg[['tooltip']][['14']], lang.dlg[['status']][['14']])

                ###########

                frDEM <- tkframe(frauxvar)

                statedem <- if(tclvalue(dem.auxvar) == "1" |
                               tclvalue(slope.auxvar) == "1" |
                               tclvalue(aspect.auxvar) == "1") "normal" else "disabled"

                txt.grddem <- tklabel(frDEM, text = lang.dlg[['label']][['6']], anchor = 'w', justify = 'left')
                cb.grddem <<- ttkcombobox(frDEM, values = unlist(listOpenFiles), textvariable = demfile.var, width = largeur1, state = statedem)
                bt.grddem <- tkbutton(frDEM, text = "...", state = statedem)

                tkgrid(txt.grddem, row = 0, column = 0, sticky = 'we', rowspan = 1, columnspan = 2, ipady = 1)
                tkgrid(cb.grddem, row = 1, column = 0, sticky = 'we', rowspan = 1, columnspan = 1, ipady = 1)
                tkgrid(bt.grddem, row = 1, column = 1, sticky = 'we', rowspan = 1, columnspan = 1, ipady = 1)

                helpWidget(cb.grddem, lang.dlg[['tooltip']][['15']], lang.dlg[['status']][['15']])
                helpWidget(bt.grddem, lang.dlg[['tooltip']][['4']], lang.dlg[['status']][['4']])

                ###########

                tkgrid(frAUX, row = 0, column = 0, sticky = 'we', rowspan = 1, columnspan = 1, ipady = 1)
                tkgrid(frDEM, row = 1, column = 0, sticky = 'we', rowspan = 1, columnspan = 1, ipady = 1)

                ###########

                tkconfigure(bt.grddem, command = function(){
                    tcl('wm', 'attributes', tt, topmost = FALSE)
                    nc.opfiles <- getOpenNetcdf(tt, initialdir = getwd())
                    tcl('wm', 'attributes', tt, topmost = TRUE)
                    if(!is.null(nc.opfiles)){
                        update.OpenFiles('netcdf', nc.opfiles)
                        listOpenFiles[[length(listOpenFiles) + 1]] <<- nc.opfiles[[1]]
                        tclvalue(demfile.var) <- nc.opfiles[[1]]
                        lapply(list(cb.stnfl, cb.grddem, cb.infile), tkconfigure, values = unlist(listOpenFiles))
                    }
                })

                tkbind(dem.chk.auxvar, "<Button-1>", function(){
                    statedem <- if(tclvalue(dem.auxvar) == "0" |
                                  (tclvalue(slope.auxvar) == "1" |
                                   tclvalue(aspect.auxvar) == "1")) 'normal' else 'disabled'
                    tkconfigure(cb.grddem, state = statedem)
                    tkconfigure(bt.grddem, state = statedem)
                })

                tkbind(slope.chk.auxvar, "<Button-1>", function(){
                    statedem <- if(tclvalue(slope.auxvar) == "0" |
                                  (tclvalue(dem.auxvar) == "1" |
                                   tclvalue(aspect.auxvar) == "1")) 'normal' else 'disabled'
                    tkconfigure(cb.grddem, state = statedem)
                    tkconfigure(bt.grddem, state = statedem)
                })

                tkbind(aspect.chk.auxvar, "<Button-1>", function(){
                    statedem <- if(tclvalue(aspect.auxvar) == "0" |
                                  (tclvalue(slope.auxvar) == "1" |
                                   tclvalue(dem.auxvar) == "1")) 'normal' else 'disabled'
                    tkconfigure(cb.grddem, state = statedem)
                    tkconfigure(bt.grddem, state = statedem)
                })

                ######
                tkgrid(frauxvar, row = 2, column = 0, sticky = 'we', rowspan = 1, columnspan = 2)
            }
        }

    ####################################

    frMerge <- tkframe(frMRG0, relief = 'sunken', borderwidth = 2, pady = 3)

    cb.mrgMthd <- lang.dlg[['combobox']][['1']]
    val.mrgMthd <- c("CSc", "BSc", "SBA", "RK")

    merge.method <- tclVar()
    tclvalue(merge.method) <- cb.mrgMthd[val.mrgMthd %in% .cdtData$GalParams$MRG$method]

    nb.run <- tclVar(.cdtData$GalParams$MRG$nrun)
    pass.ratio <- tclVar(paste0(.cdtData$GalParams$MRG$pass, collapse = ", "))

    dem.auxvar <- tclVar(.cdtData$GalParams$auxvar$dem)
    slope.auxvar <- tclVar(.cdtData$GalParams$auxvar$slope)
    aspect.auxvar <- tclVar(.cdtData$GalParams$auxvar$aspect)
    lon.auxvar <- tclVar(.cdtData$GalParams$auxvar$lon)
    lat.auxvar <- tclVar(.cdtData$GalParams$auxvar$lat)
    demfile.var <- tclVar(.cdtData$GalParams$auxvar$demfile)

    txt.mrg <- tklabel(frMerge, text = lang.dlg[['label']][['7']], anchor = 'e', justify = 'right')
    cb.mrg <- ttkcombobox(frMerge, values = cb.mrgMthd, textvariable = merge.method, width = largeur3)
    frMrgP <- tkframe(frMerge)
    frauxvar <- ttklabelframe(frMerge, text = lang.dlg[['label']][['8']], relief = 'groove', borderwidth = 2)

    cb.grddem <- ttkcombobox(frMrgP, values = "")
    auxiliary.variables(.cdtData$GalParams$MRG$method)

    txt.nrun <- tklabel(frMrgP, text = lang.dlg[['label']][['9']], anchor = 'w', justify = 'left')
    en.nrun <- tkentry(frMrgP, textvariable = nb.run, width = 3)
    txt.pass <- tklabel(frMrgP, text = lang.dlg[['label']][['10']], anchor = 'w', justify = 'left')
    en.pass <- tkentry(frMrgP, textvariable = pass.ratio, width = largeur4)

    tkgrid(txt.nrun, row = 0, column = 0, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
    tkgrid(en.nrun, row = 0, column = 1, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
    tkgrid(txt.pass, row = 0, column = 2, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
    tkgrid(en.pass, row = 0, column = 3, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)

    tkgrid(txt.mrg, row = 0, column = 0, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
    tkgrid(cb.mrg, row = 0, column = 1, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
    tkgrid(frMrgP, row = 1, column = 0, sticky = 'we', rowspan = 1, columnspan = 2, padx = 1, pady = 2, ipadx = 1, ipady = 1)

    helpWidget(cb.mrg, lang.dlg[['tooltip']][['16']], lang.dlg[['status']][['16']])
    helpWidget(en.nrun, lang.dlg[['tooltip']][['17']], lang.dlg[['status']][['17']])
    helpWidget(en.pass, lang.dlg[['tooltip']][['18']], lang.dlg[['status']][['18']])

    ########

    tkbind(cb.mrg, "<<ComboboxSelected>>", function(){
        mrgmethod <- val.mrgMthd[cb.mrgMthd %in% str_trim(tclvalue(merge.method))]
        auxiliary.variables(mrgmethod)
    })

    ############################################

    bt.mrg.interp <- ttkbutton(frMRG0, text = lang.dlg[['button']][['2']])

    helpWidget(bt.mrg.interp, lang.dlg[['tooltip']][['19']], lang.dlg[['status']][['19']])

    tkconfigure(bt.mrg.interp, command = function(){
        mrgmethod <- val.mrgMthd[cb.mrgMthd %in% str_trim(tclvalue(merge.method))]
        stateMethod <- if(mrgmethod %in% c("CSc", "BSc")) "disabled" else "normal"

        tcl('wm', 'attributes', tt, topmost = FALSE)
        .cdtData$GalParams[["interp"]] <- getInterpolationPars1(tt, .cdtData$GalParams[["interp"]], stateMethod)
        tcl('wm', 'attributes', tt, topmost = TRUE)
    })

    ############################################

    frRnoR <- tkframe(frMRG0, relief = 'sunken', borderwidth = 2)

    rnor.mask <- tclVar(.cdtData$GalParams$RnoR$use)
    rnor.wet <- tclVar(.cdtData$GalParams$RnoR$wet)
    rnor.smooth <- tclVar(.cdtData$GalParams$RnoR$smooth)

    stateRnoR <- if(.cdtData$GalParams$RnoR$use) 'normal' else 'disabled'

    chk.rnor.mask <- tkcheckbutton(frRnoR, variable = rnor.mask, text = lang.dlg[['checkbutton']][['6']], anchor = 'w', justify = 'left')
    txt.rnor.wet <- tklabel(frRnoR, text = lang.dlg[['label']][['11']], anchor = 'e', justify = 'right')
    en.rnor.wet <- tkentry(frRnoR, textvariable = rnor.wet, width = 4, state = stateRnoR)
    chk.rnor.smooth <- tkcheckbutton(frRnoR, variable = rnor.smooth, text = lang.dlg[['checkbutton']][['7']], anchor = 'w', justify = 'left', state = stateRnoR)

    tkgrid(chk.rnor.mask, row = 0, column = 0, sticky = 'we', rowspan = 1, columnspan = 3, padx = 1, pady = 1, ipadx = 1, ipady = 1)
    tkgrid(txt.rnor.wet, row = 0, column = 3, sticky = 'e', rowspan = 1, columnspan = 2, padx = 1, pady = 1, ipadx = 1, ipady = 1)
    tkgrid(en.rnor.wet, row = 0, column = 5, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
    tkgrid(chk.rnor.smooth, row = 1, column = 0, sticky = 'we', rowspan = 1, columnspan = 3, padx = 1, pady = 1, ipadx = 1, ipady = 1)

    tkbind(chk.rnor.mask, "<Button-1>", function(){
        stateRnoR <- if(tclvalue(rnor.mask) == '0') 'normal' else 'disabled'
        tkconfigure(en.rnor.wet, state = stateRnoR)
        tkconfigure(chk.rnor.smooth, state = stateRnoR)
    })

    ############################################

        crossvalid.stations <- function(selstn){
            tkdestroy(frstnPar)
            frstnPar <<- tkframe(frSTN, relief = 'groove', borderwidth = 2)

            if(selstn == "file"){
                fr.datatype <- tkframe(frstnPar)

                txt.datatype <- tklabel(fr.datatype, text = lang.dlg[['label']][['13']], anchor = 'e', justify = 'right')
                cb.datatype <- ttkcombobox(fr.datatype, values = CbdatatypeVAL, textvariable = selstn.filetype, width = largeur5)

                tkgrid(txt.datatype, row = 0, column = 0, sticky = 'e', rowspan = 1, columnspan = 1)
                tkgrid(cb.datatype, row = 0, column = 1, sticky = 'we', rowspan = 1, columnspan = 1)

                tkbind(cb.datatype, "<<ComboboxSelected>>", function(){
                    if(str_trim(tclvalue(selstn.filetype)) == CbdatatypeVAL[1])
                        tclvalue(txt.INData.var) <- lang.dlg[['label']][['14']]
                    if(str_trim(tclvalue(selstn.filetype)) == CbdatatypeVAL[2])
                        tclvalue(txt.INData.var) <- lang.dlg[['label']][['15']]
                })

                ###########
                fr.datastn <- tkframe(frstnPar)

                txt.INData <- switch(.cdtData$GalParams$selstn$file.type,
                                     'cdtcoords' = lang.dlg[['label']][['14']],
                                     'cdtstation' = lang.dlg[['label']][['15']]
                                    )
                txt.INData.var <- tclVar(txt.INData)

                txt.infile <- tklabel(fr.datastn, text = tclvalue(txt.INData.var), textvariable = txt.INData.var, anchor = 'w', justify = 'left')
                cb.infile <<- ttkcombobox(fr.datastn, values = unlist(listOpenFiles), textvariable = selstn.filestn, width = largeur1 - 1)
                bt.infile <- tkbutton(fr.datastn, text = "...")

                tkgrid(txt.infile, row = 0, column = 0, sticky = 'we', rowspan = 1, columnspan = 8, padx = 1, pady = 1, ipadx = 1, ipady = 1)
                tkgrid(cb.infile, row = 1, column = 0, sticky = 'we', rowspan = 1, columnspan = 9, padx = 0, pady = 1, ipadx = 1, ipady = 1)
                tkgrid(bt.infile, row = 1, column = 9, sticky = 'w', rowspan = 1, columnspan = 1, padx = 0, pady = 1, ipadx = 1, ipady = 1)

                tkconfigure(bt.infile, command = function(){
                    tcl('wm', 'attributes', tt, topmost = FALSE)
                    dat.opfiles <- getOpenFiles(tt)
                    tcl('wm', 'attributes', tt, topmost = TRUE)
                    if(!is.null(dat.opfiles)){
                        update.OpenFiles('ascii', dat.opfiles)
                        listOpenFiles[[length(listOpenFiles) + 1]] <<- dat.opfiles[[1]]
                        tclvalue(selstn.filestn) <- dat.opfiles[[1]]
                        lapply(list(cb.stnfl, cb.grddem, cb.infile), tkconfigure, values = unlist(listOpenFiles))
                    }
                })

                ###########
                tkgrid(fr.datatype, row = 0, column = 0, sticky = '', rowspan = 1, columnspan = 1, pady = 3)
                tkgrid(fr.datastn, row = 1, column = 0, sticky = 'we', rowspan = 1, columnspan = 1)
            }

            if(selstn == "cdt"){
                txt.minperc <- tklabel(frstnPar, text = lang.dlg[['label']][['16']], anchor = 'w', justify = 'left')
                en.minperc <- tkentry(frstnPar, textvariable = selstn.minperc, width = 4)

                tkgrid(txt.minperc, row = 0, column = 0, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
                tkgrid(en.minperc, row = 0, column = 1, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
            }

            if(selstn %in% c("file", "cdt"))
                tkgrid(frstnPar, row = 1, column = 0, sticky = 'we', rowspan = 1, columnspan = 2, pady = 3, ipady = 3)
        }

    ############################################

    frSTN <- tkframe(frMRG0, relief = 'sunken', borderwidth = 2)

    cb.selectSTN <- lang.dlg[['combobox']][['2']]
    val.selectSTN <- c("all", "file", "cdt")
    select.station <- tclVar()
    tclvalue(select.station) <- cb.selectSTN[val.selectSTN %in% .cdtData$GalParams$selstn$from]

    CbdatatypeVAL <- lang.dlg[['combobox']][['3']]
    datatypeVAL <- c('cdtcoords', 'cdtstation')
    selstn.filetype <- tclVar()
    tclvalue(selstn.filetype) <- CbdatatypeVAL[datatypeVAL %in% .cdtData$GalParams$selstn$file.type]

    selstn.filestn <- tclVar(.cdtData$GalParams$selstn$file.stn)
    selstn.minperc <- tclVar(.cdtData$GalParams$selstn$min.perc)

    txt.selstn <- tklabel(frSTN, text = lang.dlg[['label']][['12']], anchor = 'e', justify = 'right')
    cb.selstn <- ttkcombobox(frSTN, values = cb.selectSTN, textvariable = select.station, width = largeur5)
    frstnPar <- tkframe(frSTN)

    cb.infile <- ttkcombobox(frSTN, values = "")
    crossvalid.stations(.cdtData$GalParams$selstn$from)

    tkgrid(txt.selstn, row = 0, column = 0, sticky = 'e', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
    tkgrid(cb.selstn, row = 0, column = 1, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)

    ########

    tkbind(cb.selstn, "<<ComboboxSelected>>", function(){
        selstn <- val.selectSTN[cb.selectSTN %in% str_trim(tclvalue(select.station))]
        crossvalid.stations(selstn)
    })

    ############################################

    tkgrid(frtimestep, row = 0, column = 0, sticky = '', padx = 1, pady = 1, ipadx = 1, ipady = 1)
    tkgrid(frInputData, row = 1, column = 0, sticky = 'we', padx = 1, pady = 1, ipadx = 1, ipady = 1)
    tkgrid(frSave, row = 2, column = 0, sticky = 'we', padx = 1, pady = 1, ipadx = 1, ipady = 1)
    tkgrid(frMerge, row = 3, column = 0, sticky = 'we', padx = 1, pady = 1, ipadx = 1, ipady = 1)
    tkgrid(bt.mrg.interp, row = 4, column = 0, sticky = 'we', padx = 1, pady = 1, ipadx = 1, ipady = 1)
    tkgrid(frRnoR, row = 5, column = 0, sticky = 'we', padx = 1, pady = 1, ipadx = 1, ipady = 1)
    tkgrid(frSTN, row = 6, column = 0, sticky = 'we', padx = 1, pady = 3, ipadx = 1, ipady = 1)

    ############################################

    bt.prm.OK <- ttkbutton(frMRG1, text = .cdtEnv$tcl$lang$global[['button']][['1']])
    bt.prm.CA <- ttkbutton(frMRG1, text = .cdtEnv$tcl$lang$global[['button']][['2']])

    #######

    tkconfigure(bt.prm.OK, command = function(){
        if(str_trim(tclvalue(file.stnfl)) == ""){
            cdt.tkmessageBox(tt, message = lang.dlg[['message']][['1']], icon = "warning", type = "ok")
            tkwait.window(tt)
        }else if(str_trim(tclvalue(dir.InNCDF)) %in% c("", "NA")){
            cdt.tkmessageBox(tt, message = lang.dlg[['message']][['2']], icon = "warning", type = "ok")
            tkwait.window(tt)
        }else if(str_trim(tclvalue(dir2save)) %in% c("", "NA")){
            cdt.tkmessageBox(tt, message = lang.dlg[['message']][['3']], icon = "warning", type = "ok")
            tkwait.window(tt)
        }else if(is.null(settingSNC)){
            cdt.tkmessageBox(tt, message = lang.dlg[['message']][['4']], icon = "warning", type = "ok")
            tkwait.window(tt)
        }else{
            .cdtData$GalParams$period <- periodVAL[CbperiodVAL %in% str_trim(tclvalue(file.period))]

            .cdtData$GalParams$STN.file <- str_trim(tclvalue(file.stnfl))
            .cdtData$GalParams$INPUT$dir <- str_trim(tclvalue(dir.InNCDF))
            .cdtData$GalParams$outdir <- str_trim(tclvalue(dir2save))

            .cdtData$GalParams$MRG$method  <- val.mrgMthd[cb.mrgMthd %in% str_trim(tclvalue(merge.method))]
            .cdtData$GalParams$MRG$nrun <- as.numeric(str_trim(tclvalue(nb.run)))
             pass <- str_trim(strsplit(tclvalue(pass.ratio), ",")[[1]])
            .cdtData$GalParams$MRG$pass <- as.numeric(pass[pass != ""])
            if(.cdtData$GalParams$MRG$nrun != length(.cdtData$GalParams$MRG$pass)){
                cdt.tkmessageBox(tt, message = lang.dlg[['message']][['6']], icon = "warning", type = "ok")
                tkwait.window(tt)
            }

            .cdtData$GalParams$auxvar$dem <- switch(tclvalue(dem.auxvar), '0' = FALSE, '1' = TRUE)
            .cdtData$GalParams$auxvar$slope <- switch(tclvalue(slope.auxvar), '0' = FALSE, '1' = TRUE)
            .cdtData$GalParams$auxvar$aspect <- switch(tclvalue(aspect.auxvar), '0' = FALSE, '1' = TRUE)
            .cdtData$GalParams$auxvar$lon <- switch(tclvalue(lon.auxvar), '0' = FALSE, '1' = TRUE)
            .cdtData$GalParams$auxvar$lat <- switch(tclvalue(lat.auxvar), '0' = FALSE, '1' = TRUE)
            .cdtData$GalParams$auxvar$demfile <- str_trim(tclvalue(demfile.var))

            if(.cdtData$GalParams$MRG$method == "RK" &
               (.cdtData$GalParams$auxvar$dem |
                .cdtData$GalParams$auxvar$slope |
                .cdtData$GalParams$auxvar$aspect) &
               .cdtData$GalParams$auxvar$demfile == ""
              )
            {
                cdt.tkmessageBox(tt, message = lang.dlg[['message']][['7']], icon = "warning", type = "ok")
                tkwait.window(tt)
            }

            .cdtData$GalParams$RnoR$use <- switch(tclvalue(rnor.mask), '0' = FALSE, '1' = TRUE)
            .cdtData$GalParams$RnoR$smooth <- switch(tclvalue(rnor.smooth), '0' = FALSE, '1' = TRUE)
            .cdtData$GalParams$RnoR$wet <- as.numeric(str_trim(tclvalue(rnor.wet)))

            .cdtData$GalParams$selstn$from <- val.selectSTN[cb.selectSTN %in% str_trim(tclvalue(select.station))]
            .cdtData$GalParams$selstn$file.type <- datatypeVAL[CbdatatypeVAL %in% str_trim(tclvalue(selstn.filetype))]
            .cdtData$GalParams$selstn$file.stn <- str_trim(tclvalue(selstn.filestn))
            .cdtData$GalParams$selstn$min.perc <- as.numeric(str_trim(tclvalue(selstn.minperc)))

            if(.cdtData$GalParams$selstn$from == "file" &
               .cdtData$GalParams$selstn$file.stn == "")
            {
                cdt.tkmessageBox(tt, message = lang.dlg[['message']][['19']], icon = "warning", type = "ok")
                tkwait.window(tt)
            }

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
