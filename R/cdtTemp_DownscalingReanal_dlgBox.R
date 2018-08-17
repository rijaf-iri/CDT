
Temp_reanalDownGetInfo <- function(){
	listOpenFiles <- openFile_ttkcomboList()
	if(WindowsOS()){
		largeur <- 38
		largeur1 <- 35
		largeur2 <- 19
	}else{
		largeur <- 34
		largeur1 <- 33
		largeur2 <- 16
	}

	# xml.dlg <- file.path(.cdtDir$dirLocal, "languages", "cdtTemp_DownscalingReanal_dlgBox.xml")
	# lang.dlg <- cdtLanguageParse(xml.dlg, .cdtData$Config$lang.iso)

	############################################

	tt <- tktoplevel()
	tkgrab.set(tt)
	tkfocus(tt)

	frMRG0 <- tkframe(tt, relief = 'raised', borderwidth = 2)
	frMRG1 <- tkframe(tt)

	frMain <- tkframe(frMRG0, relief = "groove", borderwidth = 2)

	############################################

	frtimestep <- tkframe(frMain, relief = 'sunken', borderwidth = 2, padx = 3, pady = 3)

	file.period <- tclVar()
	CbperiodVAL <- .cdtEnv$tcl$lang$global[['combobox']][['1']][2:5]
	periodVAL <- c('daily', 'pentad', 'dekadal', 'monthly')
	tclvalue(file.period) <- CbperiodVAL[periodVAL %in% .cdtData$GalParams$period]

	txtdek <- switch(.cdtData$GalParams$period, 'dekadal' = 'Dekad', 'pentad' = 'Pentad', 'Day')
	day.txtVar <- tclVar(txtdek)
	statedate <- if(.cdtData$GalParams$period == 'monthly') 'disabled' else 'normal'

	cb.period <- ttkcombobox(frtimestep, values = CbperiodVAL, textvariable = file.period, width = largeur2)
	bt.DateRange <- ttkbutton(frtimestep, text = "Set Date Range")

	tkconfigure(bt.DateRange, command = function(){
		.cdtData$GalParams[["Down.Date.Range"]] <- getInfoDateRange(.cdtEnv$tcl$main$win,
													.cdtData$GalParams[["Down.Date.Range"]],
													daypendek.lab = tclvalue(day.txtVar),
													state.dek = statedate)
	})

	tkgrid(cb.period, row = 0, column = 0, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
	tkgrid(bt.DateRange, row = 0, column = 1, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)

	infobulle(cb.period, 'Select the time step of the data')
	status.bar.display(cb.period, 'Select the time step of the data')
	infobulle(bt.DateRange, 'Set the start and end date for downscaling reanalysis data')
	status.bar.display(bt.DateRange, 'Set the start and end date for downscaling reanalysis data')

	###########

	tkbind(cb.period, "<<ComboboxSelected>>", function(){
		tclvalue(day.txtVar) <- ifelse(str_trim(tclvalue(file.period)) == CbperiodVAL[3], 'Dekad',
								ifelse(str_trim(tclvalue(file.period)) == CbperiodVAL[2], 'Pentad', 'Day'))
		statedate <<- if(str_trim(tclvalue(file.period)) == CbperiodVAL[4]) 'disabled' else 'normal'
	})

	############################################

	frCoef <- tkframe(frMain, relief = 'sunken', borderwidth = 2)

	file.coef <- tclVar(str_trim(.cdtData$GalParams$DownCoef.file))

	txt.coeffl <- tklabel(frCoef, text = 'Downscaling Coefficients file (*.rds)', anchor = 'w', justify = 'left')
	en.coeffl <- tkentry(frCoef, textvariable = file.coef, width = largeur)
	bt.coeffl <- tkbutton(frCoef, text = "...")

	tkconfigure(bt.coeffl, command = function(){
		file2coef <- tclvalue(tkgetOpenFile(initialdir = getwd(), filetypes = .cdtEnv$tcl$data$filetypes6))
		tclvalue(file.coef) <- if(!is.na(file2coef)) file2coef else ""
	})

	tkgrid(txt.coeffl, row = 0, column = 0, sticky = 'we', rowspan = 1, columnspan = 2, padx = 1, pady = 0, ipadx = 1, ipady = 1)
	tkgrid(en.coeffl, row = 1, column = 0, sticky = 'we', rowspan = 1, columnspan = 1, padx = 0, pady = 0, ipadx = 1, ipady = 1)
	tkgrid(bt.coeffl, row = 1, column = 1, sticky = 'w', rowspan = 1, columnspan = 1, padx = 0, pady = 0, ipadx = 1, ipady = 1)

	infobulle(en.coeffl, 'Enter the full path of the file containing the downscaling coefficients STN_DEM_GLM_COEF.rds')
	status.bar.display(en.coeffl, 'Enter the full path of the file containing\nthe downscaling coefficients STN_DEM_GLM_COEF.rds')
	infobulle(bt.coeffl, 'or browse here')
	status.bar.display(bt.coeffl, 'or browse here')

	############################################

	frReanal <- tkframe(frMain, relief = 'sunken', borderwidth = 2)

	dir.REANAL <- tclVar(.cdtData$GalParams$REANAL$dir)

	txt.REANAL <- tklabel(frReanal, text = 'Directory of Reanalysis files', anchor = 'w', justify = 'left')
	set.REANAL <- ttkbutton(frReanal, text = .cdtEnv$tcl$lang$global[['button']][['5']])
	en.REANAL <- tkentry(frReanal, textvariable = dir.REANAL, width = largeur)
	bt.REANAL <- tkbutton(frReanal, text = "...")

	tkconfigure(set.REANAL, command = function(){
		.cdtData$GalParams[["REANAL"]] <- getInfoNetcdfData(tt, .cdtData$GalParams[["REANAL"]],
														str_trim(tclvalue(dir.REANAL)), str_trim(tclvalue(file.period)))
	})

	tkconfigure(bt.REANAL, command = function(){
		dirreanal <- tk_choose.dir(getwd(), "")
		tclvalue(dir.REANAL) <- if(!is.na(dirreanal)) dirreanal else ""
	})

	tkgrid(txt.REANAL, row = 0, column = 0, sticky = 'we', rowspan = 1, columnspan = 3, padx = 1, pady = 0, ipadx = 1, ipady = 1)
	tkgrid(set.REANAL, row = 0, column = 3, sticky = 'we', rowspan = 1, columnspan = 2, padx = 1, pady = 0, ipadx = 1, ipady = 1)
	tkgrid(en.REANAL, row = 1, column = 0, sticky = 'we', rowspan = 1, columnspan = 4, padx = 0, pady = 0, ipadx = 1, ipady = 1)
	tkgrid(bt.REANAL, row = 1, column = 4, sticky = 'w', rowspan = 1, columnspan = 1, padx = 0, pady = 0, ipadx = 1, ipady = 1)

	infobulle(en.REANAL, 'Enter the full path to directory containing the Reanalysis files')
	status.bar.display(en.REANAL, 'Enter the full path to directory containing the Reanalysis files')
	infobulle(bt.REANAL, 'Or browse here')
	status.bar.display(bt.REANAL, 'Or browse here')
	infobulle(set.REANAL, 'Setting netcdf data options')
	status.bar.display(set.REANAL, 'Setting netcdf data options')

	############################################
	frDEM <- tkframe(frMain, relief = 'sunken', borderwidth = 2)

	file.grddem <- tclVar(.cdtData$GalParams$DEM.file)

	txt.grddem <- tklabel(frDEM, text = "Elevation data(NetCDF)", anchor = 'w', justify = 'left')
	cb.grddem <- ttkcombobox(frDEM, values = unlist(listOpenFiles), textvariable = file.grddem, width = largeur1)
	bt.grddem <- tkbutton(frDEM, text = "...")

	####
	tkconfigure(bt.grddem, command = function(){
		nc.opfiles <- getOpenNetcdf(tt, initialdir = getwd())
		if(!is.null(nc.opfiles)){
			update.OpenFiles('netcdf', nc.opfiles)
			listOpenFiles[[length(listOpenFiles) + 1]] <<- nc.opfiles[[1]]
			tclvalue(file.grddem) <- nc.opfiles[[1]]
			tkconfigure(cb.grddem, values = unlist(listOpenFiles))
		}
	})

	#####
	tkgrid(txt.grddem, row = 0, column = 0, sticky = 'we', rowspan = 1, columnspan = 2, padx = 1, pady = 0, ipadx = 1, ipady = 1)
	tkgrid(cb.grddem, row = 1, column = 0, sticky = 'we', rowspan = 1, columnspan = 1, padx = 0, pady = 0, ipadx = 1, ipady = 1)
	tkgrid(bt.grddem, row = 1, column = 1, sticky = 'w', rowspan = 1, columnspan = 1, padx = 0, pady = 0, ipadx = 1, ipady = 1)

	infobulle(cb.grddem, 'Select the file from the list')
	status.bar.display(cb.grddem, 'File containing the elevation data in netcdf')
	infobulle(bt.grddem, 'Browse file if not listed')
	status.bar.display(bt.grddem, 'Browse file if not listed')

	############################################

	frGrid <- tkframe(frMain, relief = 'sunken', borderwidth = 2, padx = 3, pady = 3)

	varCreateGrd <- tclVar(.cdtData$GalParams$Grid.Creation$grid)
	stategrd <- if(str_trim(.cdtData$GalParams$Grid.Creation$grid) == '2') 'normal' else 'disabled'

	txt.CreateGrd <- tklabel(frGrid, text = 'Create grid for interpolation', anchor = 'w', justify = 'left')
	rbt.CreateGrd1 <- tkradiobutton(frGrid, text = "From DEM", anchor = 'w', justify = 'left')
	rbt.CreateGrd2 <- tkradiobutton(frGrid, text = "New Grid", anchor = 'w', justify = 'left')
	bt.CreateGrd <- tkbutton(frGrid, text = "Create", state = stategrd)
	bt.down.interp <- ttkbutton(frGrid, text = "Downscaling Interpolations Parameters", width = largeur)

	####
	tkconfigure(rbt.CreateGrd1, variable = varCreateGrd, value = "1")
	tkconfigure(rbt.CreateGrd2, variable = varCreateGrd, value = "2")

	tkconfigure(bt.CreateGrd, command = function(){
		.cdtData$GalParams[["Grid.Creation"]] <- getNewGridParams(tt,
												.cdtData$GalParams[["Grid.Creation"]])
	})

	tkconfigure(bt.down.interp, command = function(){
		.cdtData$GalParams[["Interpolation"]] <- getInterpolationPars(tt,
												.cdtData$GalParams[["Interpolation"]], interpChoix = 2)
	})

	#####

	tkgrid(txt.CreateGrd, row = 0, column = 0, sticky = 'we', rowspan = 1, columnspan = 8, padx = 1, pady = 1, ipadx = 1, ipady = 1)
	tkgrid(rbt.CreateGrd1, row = 1, column = 0, sticky = 'we', rowspan = 1, columnspan = 3, padx = 1, pady = 1, ipadx = 1, ipady = 1)
	tkgrid(rbt.CreateGrd2, row = 1, column = 3, sticky = 'we', rowspan = 1, columnspan = 3, padx = 1, pady = 1, ipadx = 1, ipady = 1)
	tkgrid(bt.CreateGrd, row = 1, column = 6, sticky = 'we', rowspan = 1, columnspan = 2, padx = 1, pady = 1, ipadx = 1, ipady = 1)
	tkgrid(bt.down.interp, row = 2, column = 0, sticky = 'we', rowspan = 1, columnspan = 8, padx = 1, pady = 1, ipadx = 1, ipady = 1)

	infobulle(bt.CreateGrd, 'Set the new grid')
	status.bar.display(bt.CreateGrd, 'Set the new grid')
	infobulle(frGrid, 'Create the grid to interpolate the downscaled data')
	status.bar.display(frGrid, 'Create the grid to interpolate the downscaled data')

	###########
	tkbind(rbt.CreateGrd1, "<Button-1>", function(){
		tkconfigure(bt.CreateGrd, state = 'disabled')
	})
	tkbind(rbt.CreateGrd2, "<Button-1>", function(){
		tkconfigure(bt.CreateGrd, state = 'normal')
	})

	############################################

	frSave <- tkframe(frMain, relief = 'sunken', borderwidth = 2)

	dir2save <- tclVar(.cdtData$GalParams$output$dir)
	outdownff <- tclVar(.cdtData$GalParams$output$format)

	txt.dir2save <- tklabel(frSave, text = 'Directory to save result', anchor = 'w', justify = 'left')
	en.dir2save <- tkentry(frSave, textvariable = dir2save, width = largeur)
	bt.dir2save <- tkbutton(frSave, text = "...")
	txt.outdownff <- tklabel(frSave, text = 'Downscaled data filename format', anchor = 'w', justify = 'left')
	en.outdownff <- tkentry(frSave, textvariable = outdownff, width = largeur)

	#####

	tkconfigure(bt.dir2save, command = function(){
		dir2savepth <- tk_choose.dir(.cdtData$GalParams$output$dir, "")
		if(is.na(dir2savepth)) tclvalue(dir2save) <- .cdtData$GalParams$output$dir
		else{
			dir.create(dir2savepth, showWarnings = FALSE, recursive = TRUE)
			tclvalue(dir2save) <- dir2savepth
		}
	})

	#####

	tkgrid(txt.dir2save, row = 0, column = 0, sticky = 'we', rowspan = 1, columnspan = 2, padx = 1, pady = 0, ipadx = 1, ipady = 1)
	tkgrid(en.dir2save, row = 1, column = 0, sticky = 'we', rowspan = 1, columnspan = 1, padx = 0, pady = 0, ipadx = 1, ipady = 1)
	tkgrid(bt.dir2save, row = 1, column = 1, sticky = 'w', rowspan = 1, columnspan = 1, padx = 0, pady = 0, ipadx = 1, ipady = 1)
	tkgrid(txt.outdownff, row = 2, column = 0, sticky = 'we', rowspan = 1, columnspan = 2, padx = 1, pady = 0, ipadx = 1, ipady = 1)
	tkgrid(en.outdownff, row = 3, column = 0, sticky = 'we', rowspan = 1, columnspan = 1, padx = 0, pady = 0, ipadx = 1, ipady = 1)

	infobulle(en.dir2save, 'Enter the full path to directory to save result')
	status.bar.display(en.dir2save, 'Enter the full path to directory to save result')
	infobulle(bt.dir2save, 'or browse here')
	status.bar.display(bt.dir2save, 'or browse here')
	infobulle(en.outdownff, 'Format of the downscaled data files names in NetCDF,\nexample: tmax_down_1981011.nc')
	status.bar.display(en.outdownff, 'Format of the downscaled data files names in NetCDF,\nexample: tmax_down_1981011.nc')

	############################################
	tkgrid(frtimestep, row = 0, column = 0, sticky = '', padx = 1, pady = 1, ipadx = 1, ipady = 1)
	tkgrid(frCoef, row = 1, column = 0, sticky = 'we', padx = 1, pady = 1, ipadx = 1, ipady = 1)
	tkgrid(frReanal, row = 2, column = 0, sticky = 'we', padx = 1, pady = 1, ipadx = 1, ipady = 1)
	tkgrid(frDEM, row = 3, column = 0, sticky = 'we', padx = 1, pady = 1, ipadx = 1, ipady = 1)
	tkgrid(frGrid, row = 4, column = 0, sticky = '', padx = 1, pady = 1, ipadx = 1, ipady = 1)
	tkgrid(frSave, row = 5, column = 0, sticky = 'we', padx = 1, pady = 1, ipadx = 1, ipady = 1)

	############################################

	tkgrid(frMain, row = 0, column = 0, sticky = 'news', padx = 5, pady = 5, ipadx = 1, ipady = 1)

	############################################

	bt.prm.OK <- ttkbutton(frMRG1, text = .cdtEnv$tcl$lang$global[['button']][['1']])
	bt.prm.CA <- ttkbutton(frMRG1, text = .cdtEnv$tcl$lang$global[['button']][['2']])

	#########

	tkconfigure(bt.prm.OK, command = function(){
		if(str_trim(tclvalue(file.coef)) == ""){
			tkmessageBox(message = "Provide the file containing the coefficients to used for downscaling", icon = "warning", type = "ok")
			tkwait.window(tt)
		}else if(str_trim(tclvalue(dir.REANAL)) %in% c("", "NA")){
			tkmessageBox(message = "Select or enter the path to directory containing the Reanalysis files", icon = "warning", type = "ok")
			tkwait.window(tt)
		}else if(str_trim(tclvalue(file.grddem)) == ""){
			tkmessageBox(message = "You have to provide DEM data in NetCDF format", icon = "warning", type = "ok")
			tkwait.window(tt)
		}else if(str_trim(tclvalue(dir2save)) %in% c("", "NA")){
			tkmessageBox(message = "Select or enter the path to directory to save results", icon = "warning", type = "ok")
			tkwait.window(tt)
		}else{
			.cdtData$GalParams$DownCoef.file <- str_trim(tclvalue(file.coef))
			.cdtData$GalParams$DEM.file <- str_trim(tclvalue(file.grddem))
			.cdtData$GalParams$REANAL$dir <- str_trim(tclvalue(dir.REANAL))
			.cdtData$GalParams$output$dir <- str_trim(tclvalue(dir2save))
			.cdtData$GalParams$output$format <- str_trim(tclvalue(outdownff))

			.cdtData$GalParams$period <- periodVAL[CbperiodVAL %in% str_trim(tclvalue(file.period))]
			.cdtData$GalParams$Grid.Creation$grid <- str_trim(tclvalue(varCreateGrd))

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

	tkfocus(tt)
	tkbind(tt, "<Destroy>", function(){
		tkgrab.release(tt)
		tkfocus(.cdtEnv$tcl$main$win)
	})
	tkwait.window(tt)
}
