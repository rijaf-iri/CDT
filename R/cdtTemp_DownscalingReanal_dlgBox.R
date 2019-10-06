
Temp_reanalDownGetInfo <- function(){
    listOpenFiles <- openFile_ttkcomboList()
    if(WindowsOS()){
        largeur0 <- 29
        largeur1 <- 57
        largeur2 <- 60
    }else{
        largeur0 <- 22
        largeur1 <- 41
        largeur2 <- 42
    }

    ############################################

    # xml.dlg <- file.path(.cdtDir$dirLocal, "languages", "cdtTemp_DownscalingReanal_dlgBox.xml")
    # lang.dlg <- cdtLanguageParse(xml.dlg, .cdtData$Config$lang.iso)

    ############################################

    tt <- tktoplevel()
    tkgrab.set(tt)
    tkfocus(tt)

    frMRG0 <- tkframe(tt, relief = 'raised', borderwidth = 2)
    frMRG1 <- tkframe(tt)

    ############################################

    frtimestep <- tkframe(frMRG0, relief = 'sunken', borderwidth = 2, pady = 3)


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
    helpWidget(bt.DateRange, 'Set the start and end date for downscaling reanalysis data', 'Set the start and end date for downscaling reanalysis data')

    #######

    tkconfigure(bt.DateRange, command = function(){
        tstep <- periodVAL[CbperiodVAL %in% str_trim(tclvalue(file.period))]
        tcl('wm', 'attributes', tt, topmost = FALSE)
        .cdtData$GalParams[["date.range"]] <- getInfoDateRange(tt, .cdtData$GalParams[["date.range"]], tstep)
        tcl('wm', 'attributes', tt, topmost = TRUE)
    })

    ############################################

    frameINPUT <- tkframe(frMRG0, relief = 'sunken', borderwidth = 2)

    file.coef <- tclVar(str_trim(.cdtData$GalParams$DownCoef.file))
    dir.REANAL <- tclVar(.cdtData$GalParams$REANAL$dir)
    file.grddem <- tclVar(.cdtData$GalParams$DEM.file)

    txt.coeffl <- tklabel(frameINPUT, text = 'Downscaling Coefficients file (*.rds)', anchor = 'w', justify = 'left')
    en.coeffl <- tkentry(frameINPUT, textvariable = file.coef, width = largeur2)
    bt.coeffl <- tkbutton(frameINPUT, text = "...")

    txt.REANAL <- tklabel(frameINPUT, text = 'Directory of Reanalysis files', anchor = 'w', justify = 'left')
    set.REANAL <- ttkbutton(frameINPUT, text = .cdtEnv$tcl$lang$global[['button']][['5']])
    en.REANAL <- tkentry(frameINPUT, textvariable = dir.REANAL, width = largeur2)
    bt.REANAL <- tkbutton(frameINPUT, text = "...")

    txt.grddem <- tklabel(frameINPUT, text = "Elevation data(NetCDF)", anchor = 'w', justify = 'left')
    cb.grddem <- ttkcombobox(frameINPUT, values = unlist(listOpenFiles), textvariable = file.grddem, width = largeur1)
    bt.grddem <- tkbutton(frameINPUT, text = "...")

    #######

    tkgrid(txt.coeffl, row = 0, column = 0, sticky = 'we', rowspan = 1, columnspan = 5, padx = 1, pady = 0, ipadx = 1, ipady = 1)
    tkgrid(en.coeffl, row = 1, column = 0, sticky = 'we', rowspan = 1, columnspan = 4, padx = 0, pady = 0, ipadx = 1, ipady = 1)
    tkgrid(bt.coeffl, row = 1, column = 4, sticky = 'we', rowspan = 1, columnspan = 1, padx = 0, pady = 0, ipadx = 1, ipady = 1)
    tkgrid(txt.REANAL, row = 2, column = 0, sticky = 'we', rowspan = 1, columnspan = 3, padx = 1, pady = 0, ipadx = 1, ipady = 1)
    tkgrid(set.REANAL, row = 2, column = 3, sticky = 'we', rowspan = 1, columnspan = 2, padx = 1, pady = 0, ipadx = 1, ipady = 1)
    tkgrid(en.REANAL, row = 3, column = 0, sticky = 'we', rowspan = 1, columnspan = 4, padx = 0, pady = 0, ipadx = 1, ipady = 1)
    tkgrid(bt.REANAL, row = 3, column = 4, sticky = 'we', rowspan = 1, columnspan = 1, padx = 0, pady = 0, ipadx = 1, ipady = 1)
    tkgrid(txt.grddem, row = 4, column = 0, sticky = 'we', rowspan = 1, columnspan = 5, padx = 1, pady = 0, ipadx = 1, ipady = 1)
    tkgrid(cb.grddem, row = 5, column = 0, sticky = 'we', rowspan = 1, columnspan = 4, padx = 0, pady = 0, ipadx = 1, ipady = 1)
    tkgrid(bt.grddem, row = 5, column = 4, sticky = 'we', rowspan = 1, columnspan = 1, padx = 0, pady = 0, ipadx = 1, ipady = 1)

    helpWidget(en.coeffl, 'Enter the full path of the file containing the downscaling coefficients STN_DEM_GLM_COEF.rds', 'Enter the full path of the file containing the downscaling coefficients STN_DEM_GLM_COEF.rds')
    helpWidget(bt.coeffl, 'or browse here', 'or browse here')
    helpWidget(en.REANAL, 'Enter the full path to directory containing the Reanalysis files', 'Enter the full path to directory containing the Reanalysis files')
    helpWidget(bt.REANAL, 'Or browse here', 'Or browse here')
    helpWidget(set.REANAL, 'Setting netcdf data options', 'Setting netcdf data options')
    helpWidget(cb.grddem, 'Select the file from the list', 'File containing the elevation data in NetCDF')
    helpWidget(bt.grddem, 'Browse file if not listed', 'Browse file if not listed')

    #######

    tkconfigure(bt.coeffl, command = function(){
        tcl('wm', 'attributes', tt, topmost = FALSE)
        file2coef <- tclvalue(tkgetOpenFile(initialdir = getwd(), filetypes = .cdtEnv$tcl$data$filetypes6))
        tcl('wm', 'attributes', tt, topmost = TRUE)
        tclvalue(file.coef) <- if(!is.na(file2coef)) file2coef else ""
    })

    settingSNC <- .cdtData$GalParams$settingSNC
    tkconfigure(set.REANAL, command = function(){
        tcl('wm', 'attributes', tt, topmost = FALSE)
        .cdtData$GalParams[["REANAL"]] <- getInfoNetcdfData(tt, .cdtData$GalParams[["REANAL"]],
                                                         str_trim(tclvalue(dir.REANAL)),
                                                         str_trim(tclvalue(file.period)))
        tcl('wm', 'attributes', tt, topmost = TRUE)
        settingSNC <<- 1
    })

    tkconfigure(bt.REANAL, command = function(){
        tcl('wm', 'attributes', tt, topmost = FALSE)
        dirreanal <- tk_choose.dir(getwd(), "")
        tcl('wm', 'attributes', tt, topmost = TRUE)
        tclvalue(dir.REANAL) <- if(!is.na(dirreanal)) dirreanal else ""
    })

    tkconfigure(bt.grddem, command = function(){
        tcl('wm', 'attributes', tt, topmost = FALSE)
        nc.opfiles <- getOpenNetcdf(tt, initialdir = getwd())
        tcl('wm', 'attributes', tt, topmost = TRUE)
        if(!is.null(nc.opfiles)){
            update.OpenFiles('netcdf', nc.opfiles)
            listOpenFiles[[length(listOpenFiles) + 1]] <<- nc.opfiles[[1]]
            tclvalue(file.grddem) <- nc.opfiles[[1]]
            tkconfigure(cb.grddem, values = unlist(listOpenFiles))
        }
    })

    ############################################

    bt.down.interp <- ttkbutton(frMRG0, text = "Downscaling Interpolations Parameters")
    bt.grid.interp <- ttkbutton(frMRG0, text = "Create Grid for Interpolation")

    helpWidget(bt.down.interp, 'Set the parameters to interpolate the downscaled data', 'Set the parameters to interpolate the downscaled data')
    helpWidget(bt.grid.interp, 'Create the grid to interpolate the downscaled data', 'Create the grid to interpolate the downscaled data')

    #######

    tkconfigure(bt.down.interp, command = function(){
        tcl('wm', 'attributes', tt, topmost = FALSE)
        .cdtData$GalParams[["interp"]] <- getInterpolationPars(tt, .cdtData$GalParams[["interp"]], group = 2)
        tcl('wm', 'attributes', tt, topmost = TRUE)
    })

    tkconfigure(bt.grid.interp, command = function(){
        tcl('wm', 'attributes', tt, topmost = FALSE)
        .cdtData$GalParams[["grid"]] <- createGridInterpolation(tt, .cdtData$GalParams[["grid"]])
        tcl('wm', 'attributes', tt, topmost = TRUE)
    })

    ############################################

    frSave <- tkframe(frMRG0, relief = 'sunken', borderwidth = 2)

    dir2save <- tclVar(.cdtData$GalParams$output$dir)
    outdownff <- tclVar(.cdtData$GalParams$output$format)

    txt.dir2save <- tklabel(frSave, text = 'Directory to save result', anchor = 'w', justify = 'left')
    en.dir2save <- tkentry(frSave, textvariable = dir2save, width = largeur2)
    bt.dir2save <- tkbutton(frSave, text = "...")
    txt.outdownff <- tklabel(frSave, text = 'Downscaled data filename format', anchor = 'w', justify = 'left')
    en.outdownff <- tkentry(frSave, textvariable = outdownff, width = largeur2)

    #####

    tkgrid(txt.dir2save, row = 0, column = 0, sticky = 'we', rowspan = 1, columnspan = 2, padx = 1, pady = 0, ipadx = 1, ipady = 1)
    tkgrid(en.dir2save, row = 1, column = 0, sticky = 'we', rowspan = 1, columnspan = 1, padx = 0, pady = 0, ipadx = 1, ipady = 1)
    tkgrid(bt.dir2save, row = 1, column = 1, sticky = 'w', rowspan = 1, columnspan = 1, padx = 0, pady = 0, ipadx = 1, ipady = 1)
    tkgrid(txt.outdownff, row = 2, column = 0, sticky = 'we', rowspan = 1, columnspan = 2, padx = 1, pady = 0, ipadx = 1, ipady = 1)
    tkgrid(en.outdownff, row = 3, column = 0, sticky = 'we', rowspan = 1, columnspan = 1, padx = 0, pady = 0, ipadx = 1, ipady = 1)

    helpWidget(en.dir2save, 'Enter the full path to directory to save result', 'Enter the full path to directory to save result')
    helpWidget(bt.dir2save, 'or browse here', 'or browse here')
    helpWidget(en.outdownff, 'Format of the downscaled data files names in NetCDF, example: tmax_down_1981011.nc', 'Format of the downscaled data files names in NetCDF, example: tmax_down_1981011.nc')

    #####

    tkconfigure(bt.dir2save, command = function(){
        tcl('wm', 'attributes', tt, topmost = FALSE)
        dir2savepth <- tk_choose.dir(.cdtData$GalParams$output$dir, "")
        tcl('wm', 'attributes', tt, topmost = TRUE)
        if(is.na(dir2savepth)) tclvalue(dir2save) <- .cdtData$GalParams$output$dir
        else{
            dir.create(dir2savepth, showWarnings = FALSE, recursive = TRUE)
            tclvalue(dir2save) <- dir2savepth
        }
    })

    ############################################
    tkgrid(frtimestep, row = 0, column = 0, sticky = 'we', padx = 1, pady = 1, ipadx = 1, ipady = 1)
    tkgrid(frameINPUT, row = 1, column = 0, sticky = 'we', padx = 1, pady = 1, ipadx = 1, ipady = 1)
    tkgrid(bt.down.interp, row = 2, column = 0, sticky = 'we', padx = 1, pady = 1, ipadx = 1, ipady = 1)
    tkgrid(bt.grid.interp, row = 3, column = 0, sticky = 'we', padx = 1, pady = 1, ipadx = 1, ipady = 1)
    tkgrid(frSave, row = 4, column = 0, sticky = 'we', padx = 1, pady = 1, ipadx = 1, ipady = 1)

    ############################################

    bt.prm.OK <- ttkbutton(frMRG1, text = .cdtEnv$tcl$lang$global[['button']][['1']])
    bt.prm.CA <- ttkbutton(frMRG1, text = .cdtEnv$tcl$lang$global[['button']][['2']])

    #########

    tkconfigure(bt.prm.OK, command = function(){
        if(str_trim(tclvalue(file.coef)) == ""){
            cdt.tkmessageBox(tt, message = "Provide the file containing the coefficients to used for downscaling", icon = "warning", type = "ok")
            tkwait.window(tt)
        }else if(str_trim(tclvalue(dir.REANAL)) %in% c("", "NA")){
            cdt.tkmessageBox(tt, message = "Select or enter the path to directory containing the Reanalysis files", icon = "warning", type = "ok")
            tkwait.window(tt)
        }else if(str_trim(tclvalue(file.grddem)) == ""){
            cdt.tkmessageBox(tt, message = "You have to provide DEM data in NetCDF format", icon = "warning", type = "ok")
            tkwait.window(tt)
        }else if(str_trim(tclvalue(dir2save)) %in% c("", "NA")){
            cdt.tkmessageBox(tt, message = "Select or enter the path to directory to save results", icon = "warning", type = "ok")
            tkwait.window(tt)
        }else if(is.null(settingSNC)){
            cdt.tkmessageBox(tt, message = "You have to set the NetCDF files parameters", icon = "warning", type = "ok")
            tkwait.window(tt)
        }else{
            .cdtData$GalParams$DownCoef.file <- str_trim(tclvalue(file.coef))
            .cdtData$GalParams$DEM.file <- str_trim(tclvalue(file.grddem))
            .cdtData$GalParams$REANAL$dir <- str_trim(tclvalue(dir.REANAL))
            .cdtData$GalParams$output$dir <- str_trim(tclvalue(dir2save))
            .cdtData$GalParams$output$format <- str_trim(tclvalue(outdownff))

            .cdtData$GalParams$period <- periodVAL[CbperiodVAL %in% str_trim(tclvalue(file.period))]

            .cdtData$GalParams$settingSNC <- settingSNC

            # .cdtData$GalParams$message <- lang.dlg[['message']]

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

    ############################
    tkwm.withdraw(tt)
    tcl('update')
    tt.w <- as.integer(tkwinfo("reqwidth", tt))
    tt.h <- as.integer(tkwinfo("reqheight", tt))
    tt.x <- as.integer(.cdtEnv$tcl$data$width.scr*0.5 - tt.w*0.5)
    tt.y <- as.integer(.cdtEnv$tcl$data$height.scr*0.5 - tt.h*0.5)
    tkwm.geometry(tt, paste0('+', tt.x, '+', tt.y))
    tkwm.transient(tt)
    tkwm.title(tt, 'Reanalysis Downscaling - Settings')
    tkwm.deiconify(tt)
    tcl('wm', 'attributes', tt, topmost = TRUE)

    tkfocus(tt)
    tkbind(tt, "<Destroy>", function(){
        tkgrab.release(tt)
        tkfocus(.cdtEnv$tcl$main$win)
    })
    tkwait.window(tt)
}
