
crossValidationInfoRain <- function(){
    listOpenFiles <- openFile_ttkcomboList()
    if(WindowsOS()){
        largeur0 <- 29
        largeur1 <- 57
        largeur2 <- 60
        largeur3 <- 44
        largeur4 <- 27
    }else{
        largeur0 <- 22
        largeur1 <- 41
        largeur2 <- 42
        largeur3 <- 32
        largeur4 <- 17
    }

    ####################################

    # xml.dlg <- file.path(.cdtDir$dirLocal, "languages", "cdtPrecip_CrossValidation_dlgBox.xml")
    # lang.dlg <- cdtLanguageParse(xml.dlg, .cdtData$Config$lang.iso)

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
    bt.DateRange <- ttkbutton(frtimestep, text = "Set Date Range", width = largeur0)

    #######

    tkgrid(cb.period, row = 0, column = 0, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
    tkgrid(bt.DateRange, row = 0, column = 1, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)

    helpWidget(cb.period, 'Select the time step of the data', 'Select the time step of the data')
    helpWidget(bt.DateRange, 'Set the start and end date of data to merge', 'Set the start and end date of data to merge')

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
    dir.InNCDF <- tclVar(.cdtData$GalParams$RFE$dir)

    txt.stnfl <- tklabel(frInputData, text = 'Station data file', anchor = 'w', justify = 'left')
    cb.stnfl <- ttkcombobox(frInputData, values = unlist(listOpenFiles), textvariable = file.stnfl, width = largeur1)
    bt.stnfl <- tkbutton(frInputData, text = "...")
    txt.InNCDF <- tklabel(frInputData, text = 'Directory containing RFE data', anchor = 'w', justify = 'left')
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

    helpWidget(cb.stnfl, 'Select the file from the list', 'Select the file containing the gauge data')
    helpWidget(bt.stnfl, 'Browse file if not listed', 'Browse file if not listed')
    helpWidget(en.InNCDF, 'Enter the full path to directory containing the RFE or Adjusted RFE files', 'Enter the full path to directory containing the RFE or Adjusted RFE files')
    helpWidget(bt.InNCDF, 'Or browse here', 'Or browse here')
    helpWidget(set.InNCDF, 'Setting NetCDF data options', 'Setting NetCDF data options')

    ######
    tkconfigure(bt.stnfl, command = function(){
        tcl('wm', 'attributes', tt, topmost = FALSE)
        dat.opfiles <- getOpenFiles(tt)
        tcl('wm', 'attributes', tt, topmost = TRUE)
        if(!is.null(dat.opfiles)){
            update.OpenFiles('ascii', dat.opfiles)
            listOpenFiles[[length(listOpenFiles) + 1]] <<- dat.opfiles[[1]]
            tclvalue(file.stnfl) <- dat.opfiles[[1]]
            # lapply(list(cb.stnfl, cb.grddem), tkconfigure, values = unlist(listOpenFiles))
            lapply(list(cb.stnfl), tkconfigure, values = unlist(listOpenFiles))
        }
    })

    settingSNC <- .cdtData$GalParams$settingSNC
    tkconfigure(set.InNCDF, command = function(){
        tcl('wm', 'attributes', tt, topmost = FALSE)
        .cdtData$GalParams[["RFE"]] <- getInfoNetcdfData(tt, .cdtData$GalParams[["RFE"]],
                                                         str_trim(tclvalue(dir.InNCDF)),
                                                         str_trim(tclvalue(file.period)))
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

    txt.dir2save <- tklabel(frSave, text = 'Directory to save result', anchor = 'w', justify = 'left')
    en.dir2save <- tkentry(frSave, textvariable = dir2save, width = largeur2)
    bt.dir2save <- tkbutton(frSave, text = "...")

    #####

    tkgrid(txt.dir2save, row = 0, column = 0, sticky = 'we', rowspan = 1, columnspan = 2, padx = 1, pady = 1, ipadx = 1, ipady = 1)
    tkgrid(en.dir2save, row = 1, column = 0, sticky = 'we', rowspan = 1, columnspan = 1, padx = 0, pady = 1, ipadx = 1, ipady = 1)
    tkgrid(bt.dir2save, row = 1, column = 1, sticky = 'we', rowspan = 1, columnspan = 1, padx = 0, pady = 1, ipadx = 1, ipady = 1)

    helpWidget(en.dir2save, 'Enter the full path to directory to save result', 'Enter the full path to directory to save result')
    helpWidget(bt.dir2save, 'or browse here', 'or browse here')

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

        cb.grddem <- NULL

        auxiliary.variables <- function(mrgmethod){
            tkdestroy(frauxvar)

            frauxvar <<- ttklabelframe(frMerge, text = 'Include auxiliary variables', relief = 'groove', borderwidth = 2)

            if(mrgmethod == "RK"){
                frAUX <- tkframe(frauxvar)

                dem.chk.auxvar <- tkcheckbutton(frAUX, variable = dem.auxvar, text = 'Elevation', anchor = 'w', justify = 'left')
                slope.chk.auxvar <- tkcheckbutton(frAUX, variable = slope.auxvar, text = 'Slope', anchor = 'w', justify = 'left')
                aspect.chk.auxvar <- tkcheckbutton(frAUX, variable = aspect.auxvar, text = 'Aspect', anchor = 'w', justify = 'left')
                lon.chk.auxvar <- tkcheckbutton(frAUX, variable = lon.auxvar, text = 'Longitude', anchor = 'w', justify = 'left')
                lat.chk.auxvar <- tkcheckbutton(frAUX, variable = lat.auxvar, text = 'Latitude', anchor = 'w', justify = 'left')

                tkgrid(dem.chk.auxvar, row = 0, column = 0, sticky = 'we', rowspan = 1, columnspan = 1, ipady = 1)
                tkgrid(slope.chk.auxvar, row = 0, column = 1, sticky = 'we', rowspan = 1, columnspan = 1, ipady = 1)
                tkgrid(aspect.chk.auxvar, row = 0, column = 2, sticky = 'we', rowspan = 1, columnspan = 1, ipady = 1)
                tkgrid(lon.chk.auxvar, row = 0, column = 3, sticky = 'we', rowspan = 1, columnspan = 1, ipady = 1)
                tkgrid(lat.chk.auxvar, row = 0, column = 4, sticky = 'we', rowspan = 1, columnspan = 1, ipady = 1)

                helpWidget(dem.chk.auxvar, 'Include elevation data as auxiliary variable', 'Include elevation data as auxiliary variable')
                helpWidget(slope.chk.auxvar, 'Include slope data as auxiliary variable', 'Include slope data as auxiliary variable')
                helpWidget(aspect.chk.auxvar, 'Include aspect data as auxiliary variable', 'Include aspect data as auxiliary variable')
                helpWidget(lon.chk.auxvar, 'Include longitude as auxiliary variable', 'Include longitude as auxiliary variable')
                helpWidget(lat.chk.auxvar, 'Include latitude as auxiliary variable', 'Include latitude as auxiliary variable')

                ###########

                frDEM <- tkframe(frauxvar)

                statedem <- if(tclvalue(dem.auxvar) == "1" |
                               tclvalue(slope.auxvar) == "1" |
                               tclvalue(aspect.auxvar) == "1") "normal" else "disabled"

                txt.grddem <- tklabel(frDEM, text = "Elevation data (NetCDF)",  anchor = 'w', justify = 'left')
                cb.grddem <<- ttkcombobox(frDEM, values = unlist(listOpenFiles), textvariable = demfile.var, width = largeur1, state = statedem)
                bt.grddem <- tkbutton(frDEM, text = "...", state = statedem)

                tkgrid(txt.grddem, row = 0, column = 0, sticky = 'we', rowspan = 1, columnspan = 2, ipady = 1)
                tkgrid(cb.grddem, row = 1, column = 0, sticky = 'we', rowspan = 1, columnspan = 1, ipady = 1)
                tkgrid(bt.grddem, row = 1, column = 1, sticky = 'we', rowspan = 1, columnspan = 1, ipady = 1)

                helpWidget(cb.grddem, 'Select the file in the list', 'File containing the elevation data in NetCDF')
                helpWidget(bt.grddem, 'Browse file if not listed', 'Browse file if not listed')

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
                        lapply(list(cb.stnfl, cb.grddem), tkconfigure, values = unlist(listOpenFiles))
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
                tkgrid(frauxvar, row = 2, column = 0, sticky = 'we', rowspan = 1, columnspan = 2, padx = 1, pady = 2, ipadx = 1, ipady = 1)
            }
        }

    ####################################

    frMerge <- tkframe(frMRG0, relief = 'sunken', borderwidth = 2, pady = 3)

    cb.mrgMthd <- c("Cressman Scheme", "Barnes Scheme",
                     "Simple Bias Adjustment", "Regression Kriging")
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

    txt.mrg <- tklabel(frMerge, text = 'Merging method', anchor = 'e', justify = 'right')
    cb.mrg <- ttkcombobox(frMerge, values = cb.mrgMthd, textvariable = merge.method, width = largeur3)
    frMrgP <- tkframe(frMerge)
    frauxvar <- ttklabelframe(frMerge, text = 'Include auxiliary variables', relief = 'groove', borderwidth = 2)

    auxiliary.variables(.cdtData$GalParams$MRG$method)

    txt.nrun <- tklabel(frMrgP, text = 'Number of nested run', anchor = 'w', justify = 'left')
    en.nrun <- tkentry(frMrgP, textvariable = nb.run, width = 3)
    txt.pass <- tklabel(frMrgP, text = 'Pass ratio', anchor = 'w', justify = 'left')
    en.pass <- tkentry(frMrgP, textvariable = pass.ratio, width = largeur4)

    tkgrid(txt.nrun, row = 0, column = 0, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
    tkgrid(en.nrun, row = 0, column = 1, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
    tkgrid(txt.pass, row = 0, column = 2, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
    tkgrid(en.pass, row = 0, column = 3, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)

    tkgrid(txt.mrg, row = 0, column = 0, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
    tkgrid(cb.mrg, row = 0, column = 1, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
    tkgrid(frMrgP, row = 1, column = 0, sticky = 'we', rowspan = 1, columnspan = 2, padx = 1, pady = 2, ipadx = 1, ipady = 1)

    helpWidget(cb.mrg, 'Method to be used to perform merging', 'Method to be used to perform merging')
    helpWidget(en.nrun, 'Number of the nested run to be performed', 'Number of the nested run to be performed')
    helpWidget(en.pass, 'Fraction of the radius of influence or number maximum of the neighborhood \n to be used for each run, separated by a comma and must be the same number \n of the number of run',
                        'Fraction of the radius of influence or number maximum of the neighborhood \n to be used for each run, separated by a comma and must be the same number \n of the number of run') 

    ########

    tkbind(cb.mrg, "<<ComboboxSelected>>", function(){
        mrgmethod <- val.mrgMthd[cb.mrgMthd %in% str_trim(tclvalue(merge.method))]
        auxiliary.variables(mrgmethod)
    })

    ############################################

    bt.mrg.interp <- ttkbutton(frMRG0, text = "Merging Interpolations Parameters")

    helpWidget(bt.mrg.interp, 'Set the parameters to interpolate the data', 'Set the parameters to interpolate the data')

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

    chk.rnor.mask <- tkcheckbutton(frRnoR, variable = rnor.mask, text = "Apply Rain-no-Rain Mask", anchor = 'w', justify = 'left')
    txt.rnor.wet <- tklabel(frRnoR, text = 'Rainy day threshold', anchor = 'e', justify = 'right')
    en.rnor.wet <- tkentry(frRnoR, textvariable = rnor.wet, width = 4, state = stateRnoR)
    chk.rnor.smooth <- tkcheckbutton(frRnoR, variable = rnor.smooth, text = "Smooth Rain-no-Rain Mask", anchor = 'w', justify = 'left', state = stateRnoR)

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

    tkgrid(frtimestep, row = 0, column = 0, sticky = '', padx = 1, pady = 1, ipadx = 1, ipady = 1)
    tkgrid(frInputData, row = 1, column = 0, sticky = 'we', padx = 1, pady = 1, ipadx = 1, ipady = 1)
    tkgrid(frSave, row = 2, column = 0, sticky = 'we', padx = 1, pady = 1, ipadx = 1, ipady = 1)
    tkgrid(frMerge, row = 3, column = 0, sticky = 'we', padx = 1, pady = 1, ipadx = 1, ipady = 1)
    tkgrid(bt.mrg.interp, row = 4, column = 0, sticky = 'we', padx = 1, pady = 1, ipadx = 1, ipady = 1)
    tkgrid(frRnoR, row = 5, column = 0, sticky = 'we', padx = 1, pady = 1, ipadx = 1, ipady = 1)

    ############################################

    bt.prm.OK <- ttkbutton(frMRG1, text = .cdtEnv$tcl$lang$global[['button']][['1']])
    bt.prm.CA <- ttkbutton(frMRG1, text = .cdtEnv$tcl$lang$global[['button']][['2']])

    #######

    tkconfigure(bt.prm.OK, command = function(){
        if(str_trim(tclvalue(file.stnfl)) == ""){
            cdt.tkmessageBox(tt, message = "Select the file containing the station data", icon = "warning", type = "ok")
            tkwait.window(tt)
        }else if(str_trim(tclvalue(dir.InNCDF)) %in% c("", "NA")){
            cdt.tkmessageBox(tt, message = "Browse or enter the directory containing the RFE files", icon = "warning", type = "ok")
            tkwait.window(tt)
        }else if(str_trim(tclvalue(dir2save)) %in% c("", "NA")){
            cdt.tkmessageBox(tt, message = "Browse or enter the path to directory to save results", icon = "warning", type = "ok")
            tkwait.window(tt)
        }else if(is.null(settingSNC)){
            cdt.tkmessageBox(tt, message = "You have to set the NetCDF files parameters", icon = "warning", type = "ok")
            tkwait.window(tt)
        }else{
            .cdtData$GalParams$period <- periodVAL[CbperiodVAL %in% str_trim(tclvalue(file.period))]

            .cdtData$GalParams$STN.file <- str_trim(tclvalue(file.stnfl))
            .cdtData$GalParams$RFE$dir <- str_trim(tclvalue(dir.InNCDF))
            .cdtData$GalParams$outdir <- str_trim(tclvalue(dir2save))

            .cdtData$GalParams$MRG$method  <- val.mrgMthd[cb.mrgMthd %in% str_trim(tclvalue(merge.method))]
            .cdtData$GalParams$MRG$nrun <- as.numeric(str_trim(tclvalue(nb.run)))
             pass <- str_trim(strsplit(tclvalue(pass.ratio), ",")[[1]])
            .cdtData$GalParams$MRG$pass <- as.numeric(pass[pass != ""])
            if(.cdtData$GalParams$MRG$nrun != length(.cdtData$GalParams$MRG$pass)){
                cdt.tkmessageBox(tt, message = "Number of run is not equal to the number of pass ration", icon = "warning", type = "ok")
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
                cdt.tkmessageBox(tt, message = "You have to provide DEM data in NetCDF format", icon = "warning", type = "ok")
                tkwait.window(tt)
            }

            .cdtData$GalParams$RnoR$use <- switch(tclvalue(rnor.mask), '0' = FALSE, '1' = TRUE)
            .cdtData$GalParams$RnoR$smooth <- switch(tclvalue(rnor.smooth), '0' = FALSE, '1' = TRUE)
            .cdtData$GalParams$RnoR$wet <- as.numeric(str_trim(tclvalue(rnor.wet)))

            .cdtData$GalParams$settingSNC <- settingSNC

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
    tkwm.title(tt, 'Cross-Validation - Settings')
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
