
Temp_coefDownGetInfo <- function(){
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

    ####################################

    # xml.dlg <- file.path(.cdtDir$dirLocal, "languages", "cdtTemp_DownscalingCoef_dlgBox.xml")
    # lang.dlg <- cdtLanguageParse(xml.dlg, .cdtData$Config$lang.iso)

    ####################################

    tt <- tktoplevel()
    tkgrab.set(tt)
    tkfocus(tt)

    frMRG0 <- tkframe(tt, relief = 'raised', borderwidth = 2)
    frMRG1 <- tkframe(tt)

    ####################################

    frtimestep <- tkframe(frMRG0, relief = 'sunken', borderwidth = 2, pady = 3)

    file.period <- tclVar()
    CbperiodVAL <- .cdtEnv$tcl$lang$global[['combobox']][['1']][3:6]
    periodVAL <- c('daily', 'pentad', 'dekadal', 'monthly')
    tclvalue(file.period) <- CbperiodVAL[periodVAL %in% .cdtData$GalParams$period]

    cb.period <- ttkcombobox(frtimestep, values = CbperiodVAL, textvariable = file.period, width = largeur0)
    bt.BasePeriod <- ttkbutton(frtimestep, text = "Set Base Period", width = largeur0)

    #######

    tkgrid(cb.period, row = 0, column = 0, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
    tkgrid(bt.BasePeriod, row = 0, column = 1, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)

    helpWidget(cb.period, 'Select the time step of the data', 'Select the time step of the data')
    helpWidget(bt.BasePeriod, 'Base period to be used to compute regression parameters between station temperature and elevation', 'Base period to be used to compute regression parameters between station temperature and elevation')

    #######

    tkconfigure(bt.BasePeriod, command = function(){
        tcl('wm', 'attributes', tt, topmost = FALSE)
        .cdtData$GalParams[["base.period"]] <- getInfoBasePeriod(tt, .cdtData$GalParams[["base.period"]])
        tcl('wm', 'attributes', tt, topmost = TRUE)
    })

    ############################################

    frINPUT <- tkframe(frMRG0, relief = 'sunken', borderwidth = 2)

    file.stnfl <- tclVar(.cdtData$GalParams$IO.files$STN.file)
    file.grddem <- tclVar(.cdtData$GalParams$IO.files$DEM.file)

    txt.stnfl <- tklabel(frINPUT, text = 'Station data file', anchor = 'w', justify = 'left')
    cb.stnfl <- ttkcombobox(frINPUT, values = unlist(listOpenFiles), textvariable = file.stnfl, width = largeur1)
    bt.stnfl <- tkbutton(frINPUT, text = "...")
    txt.grddem <- tklabel(frINPUT, text = "Elevation data (NetCDF)", anchor = 'w', justify = 'left')
    cb.grddem <- ttkcombobox(frINPUT, values = unlist(listOpenFiles), textvariable = file.grddem, width = largeur1)
    bt.grddem <- tkbutton(frINPUT, text = "...")

    #####

    tkgrid(txt.stnfl, row = 0, column = 0, sticky = 'we', rowspan = 1, columnspan = 2, padx = 1, pady = 0, ipadx = 1, ipady = 1)
    tkgrid(cb.stnfl, row = 1, column = 0, sticky = 'we', rowspan = 1, columnspan = 1, padx = 0, pady = 0, ipadx = 1, ipady = 1)
    tkgrid(bt.stnfl, row = 1, column = 1, sticky = 'we', rowspan = 1, columnspan = 1, padx = 0, pady = 0, ipadx = 1, ipady = 1)
    tkgrid(txt.grddem, row = 2, column = 0, sticky = 'we', rowspan = 1, columnspan = 2, padx = 1, pady = 0, ipadx = 1, ipady = 1)
    tkgrid(cb.grddem, row = 3, column = 0, sticky = 'we', rowspan = 1, columnspan = 1, padx = 0, pady = 0, ipadx = 1, ipady = 1)
    tkgrid(bt.grddem, row = 3, column = 1, sticky = 'we', rowspan = 1, columnspan = 1, padx = 0, pady = 0, ipadx = 1, ipady = 1)

    helpWidget(cb.stnfl, 'Choose the file in the list', 'Choose the file containing the station data')
    helpWidget(bt.stnfl, 'Browse file if not listed', 'Browse file if not listed')
    helpWidget(cb.grddem, 'Select the file from the list', 'File containing the elevation data in NetCDF format')
    helpWidget(bt.grddem, 'Browse file if not listed', 'Browse file if not listed')

    ######
    tkconfigure(bt.stnfl, command = function(){
        tcl('wm', 'attributes', tt, topmost = FALSE)
        dat.opfiles <- getOpenFiles(tt)
        tcl('wm', 'attributes', tt, topmost = TRUE)
        if(!is.null(dat.opfiles)){
            update.OpenFiles('ascii', dat.opfiles)
            listOpenFiles[[length(listOpenFiles) + 1]] <<- dat.opfiles[[1]]
            tclvalue(file.stnfl) <- dat.opfiles[[1]]
            lapply(list(cb.stnfl, cb.grddem), tkconfigure, values = unlist(listOpenFiles))
        }
    })

    ####
    tkconfigure(bt.grddem, command = function(){
        tcl('wm', 'attributes', tt, topmost = FALSE)
        nc.opfiles <- getOpenNetcdf(tt, initialdir = getwd())
        tcl('wm', 'attributes', tt, topmost = TRUE)
        if(!is.null(nc.opfiles)){
            update.OpenFiles('netcdf', nc.opfiles)
            listOpenFiles[[length(listOpenFiles) + 1]] <<- nc.opfiles[[1]]
            tclvalue(file.grddem) <- nc.opfiles[[1]]
            lapply(list(cb.stnfl, cb.grddem), tkconfigure, values = unlist(listOpenFiles))
        }
    })

    ############################################

    frSave <- tkframe(frMRG0, relief = 'sunken', borderwidth = 2, pady = 3)

    file.save1 <- tclVar(.cdtData$GalParams$IO.files$dir2save)

    txt.file.save <- tklabel(frSave, text = 'Directory to save result', anchor = 'w', justify = 'left')
    en.file.save <- tkentry(frSave, textvariable = file.save1, width = largeur2)
    bt.file.save <- tkbutton(frSave, text = "...")

    #####

    tkgrid(txt.file.save, row = 0, column = 0, sticky = 'we', rowspan = 1, columnspan = 2, padx = 1, pady = 0, ipadx = 1, ipady = 1)
    tkgrid(en.file.save, row = 1, column = 0, sticky = 'we', rowspan = 1, columnspan = 1, padx = 0, pady = 0, ipadx = 1, ipady = 1)
    tkgrid(bt.file.save, row = 1, column = 1, sticky = 'we', rowspan = 1, columnspan = 1, padx = 0, pady = 0, ipadx = 1, ipady = 1)

    helpWidget(en.file.save, 'Enter the full path to directory to save result', 'Enter the full path to directory to save result')
    helpWidget(bt.file.save, 'or browse here', 'or browse here')

    #####

    tkconfigure(bt.file.save, command = function(){
        tcl('wm', 'attributes', tt, topmost = FALSE)
        file2save1 <- tk_choose.dir(.cdtData$GalParams$IO.files$dir2save, "")
        tcl('wm', 'attributes', tt, topmost = TRUE)
        if(is.na(file2save1)) tclvalue(file.save1) <- .cdtData$GalParams$IO.files$dir2save
        else{
            dir.create(file2save1, showWarnings = FALSE, recursive = TRUE)
            tclvalue(file.save1) <- file2save1
        }
    })

    ############################################

    tkgrid(frtimestep, row = 0, column = 0, sticky = 'we', padx = 1, pady = 1, ipadx = 1, ipady = 1)
    tkgrid(frINPUT, row = 1, column = 0, sticky = 'we', padx = 1, pady = 1, ipadx = 1, ipady = 1)
    tkgrid(frSave, row = 2, column = 0, sticky = 'we', padx = 1, pady = 1, ipadx = 1, ipady = 1)

    ############################################

    bt.prm.OK <- ttkbutton(frMRG1, text = .cdtEnv$tcl$lang$global[['button']][['1']])
    bt.prm.CA <- ttkbutton(frMRG1, text = .cdtEnv$tcl$lang$global[['button']][['2']])

    tkconfigure(bt.prm.OK, command = function(){
        if(str_trim(tclvalue(file.stnfl)) == ""){
            cdt.tkmessageBox(tt, message = "Select the file containing the gauge data", icon = "warning", type = "ok")
        }else if(str_trim(tclvalue(file.grddem)) == "" ){
            cdt.tkmessageBox(tt, message = "You have to choose DEM data in NetCDF format", icon = "warning", type = "ok")
            tkwait.window(tt)
        }else if(str_trim(tclvalue(file.save1)) %in% c("", "NA")){
            cdt.tkmessageBox(tt, message = "Select or enter the path to directory to save results", icon = "warning", type = "ok")
            tkwait.window(tt)
        }else{
            .cdtData$GalParams$period <- periodVAL[CbperiodVAL %in% str_trim(tclvalue(file.period))]

            .cdtData$GalParams$IO.files$STN.file <- str_trim(tclvalue(file.stnfl))
            .cdtData$GalParams$IO.files$DEM.file <- str_trim(tclvalue(file.grddem))
            .cdtData$GalParams$IO.files$dir2save <- str_trim(tclvalue(file.save1))

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

    ############################3
    tkwm.withdraw(tt)
    tcl('update')
    tt.w <- as.integer(tkwinfo("reqwidth", tt))
    tt.h <- as.integer(tkwinfo("reqheight", tt))
    tt.x <- as.integer(.cdtEnv$tcl$data$width.scr*0.5 - tt.w*0.5)
    tt.y <- as.integer(.cdtEnv$tcl$data$height.scr*0.5 - tt.h*0.5)
    tkwm.geometry(tt, paste0('+', tt.x, '+', tt.y))
    tkwm.transient(tt)
    tkwm.title(tt, 'Coefficients Downscaling-Settings')
    tkwm.deiconify(tt)
    tcl('wm', 'attributes', tt, topmost = TRUE)

    tkfocus(tt)
    tkbind(tt, "<Destroy>", function(){
        tkgrab.release(tt)
        tkfocus(.cdtEnv$tcl$main$win)
    })
    tkwait.window(tt)
}
