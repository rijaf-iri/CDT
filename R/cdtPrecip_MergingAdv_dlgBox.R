
mergeGetInfoRain <- function(){
	listOpenFiles <- openFile_ttkcomboList()
	if(WindowsOS()){
		largeur0 <- 19
		largeur1 <- 42
		largeur2 <- 45
		largeur3 <- 28
		largeur4 <- 32
	}else{
		largeur0 <- 16
		largeur1 <- 38
		largeur2 <- 39
		largeur3 <- 17
		largeur4 <- 22
	}

	# xml.dlg <- file.path(.cdtDir$dirLocal, "languages", "cdtPrecip_MergingAdv_dlgBox.xml")
	# lang.dlg <- cdtLanguageParse(xml.dlg, .cdtData$Config$lang.iso)

	####################################

	tt <- tktoplevel()
	tkgrab.set(tt)
	tkfocus(tt)

	frMRG0 <- tkframe(tt, relief = 'raised', borderwidth = 2, padx = 3, pady = 3)
	frMRG1 <- tkframe(tt)

	####################################

	bwnote <- bwNoteBook(frMRG0)
	conf.tab1 <- bwAddTab(bwnote, text = "Input")
	conf.tab2 <- bwAddTab(bwnote, text = "Merging")
	conf.tab3 <- bwAddTab(bwnote, text = "Output")

	bwRaiseTab(bwnote, conf.tab1)
	tkgrid.columnconfigure(conf.tab1, 0, weight = 1)
	tkgrid.columnconfigure(conf.tab2, 0, weight = 1)
	tkgrid.columnconfigure(conf.tab3, 0, weight = 1)

	####################################

	frTab1 <- tkframe(conf.tab1)

		####################################

		frtimestep <- tkframe(frTab1, relief = 'sunken', borderwidth = 2, padx = 3, pady = 3)

		file.period <- tclVar()
		CbperiodVAL <- .cdtEnv$tcl$lang$global[['combobox']][['1']][2:5]
		periodVAL <- c('daily', 'pentad', 'dekadal', 'monthly')
		tclvalue(file.period) <- CbperiodVAL[periodVAL %in% .cdtData$GalParams$period]

		txtdek <- switch(.cdtData$GalParams$period, 'dekadal' = 'Dekad', 'pentad' = 'Pentad', 'Day')
		day.txtVar <- tclVar(txtdek)
		statedate <- if(.cdtData$GalParams$period == 'monthly') 'disabled' else 'normal'

		cb.period <- ttkcombobox(frtimestep, values = CbperiodVAL, textvariable = file.period, width = largeur0)
		bt.DateRange <- ttkbutton(frtimestep, text = "Set Date Range")

		tkconfigure(bt.DateRange, command = function(){
			.cdtData$GalParams[["Merging.Date"]] <- getInfoDateRange(tt,
													.cdtData$GalParams[["Merging.Date"]],
													daypendek.lab = tclvalue(day.txtVar),
													state.dek = statedate)
		})

		tkgrid(cb.period, row = 0, column = 0, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
		tkgrid(bt.DateRange, row = 0, column = 1, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)

		infobulle(cb.period, 'Select the time step of the data')
		status.bar.display(cb.period, 'Select the time step of the data')
		infobulle(bt.DateRange, 'Set the start and end date to merge RFE data')
		status.bar.display(bt.DateRange, 'Set the start and end date to merge RFE data')

		###########

		tkbind(cb.period, "<<ComboboxSelected>>", function(){
			tclvalue(day.txtVar) <- ifelse(str_trim(tclvalue(file.period)) == CbperiodVAL[3], 'Dekad',
									ifelse(str_trim(tclvalue(file.period)) == CbperiodVAL[2], 'Pentad', 'Day'))
			statedate <<- if(str_trim(tclvalue(file.period)) == CbperiodVAL[4]) 'disabled' else 'normal'
		})

		####################################

		frInputData <- tkframe(frTab1, relief = 'sunken', borderwidth = 2, padx = 3, pady = 3)

		file.stnfl <- tclVar(.cdtData$GalParams$STN.file)
		dir.RFE <- tclVar(.cdtData$GalParams$RFE$dir)

		txt.stnfl <- tklabel(frInputData, text = 'Station data file', anchor = 'w', justify = 'left')
		cb.stnfl <- ttkcombobox(frInputData, values = unlist(listOpenFiles), textvariable = file.stnfl, width = largeur1)
		bt.stnfl <- tkbutton(frInputData, text = "...")
		txt.RFE <- tklabel(frInputData, text = 'Directory of RFE or ADJ-RFE files', anchor = 'w', justify = 'left')
		set.RFE <- ttkbutton(frInputData, text = .cdtEnv$tcl$lang$global[['button']][['5']])
		en.RFE <- tkentry(frInputData, textvariable = dir.RFE, width = largeur2)
		bt.RFE <- tkbutton(frInputData, text = "...")

		######
		tkconfigure(bt.stnfl, command = function(){
			dat.opfiles <- getOpenFiles(tt)
			if(!is.null(dat.opfiles)){
				update.OpenFiles('ascii', dat.opfiles)
				listOpenFiles[[length(listOpenFiles) + 1]] <<- dat.opfiles[[1]]
				tclvalue(file.stnfl) <- dat.opfiles[[1]]
				lapply(list(cb.stnfl, cb.grddem, cb.blkshp), tkconfigure, values = unlist(listOpenFiles))
			}
		})

		tkconfigure(set.RFE, command = function(){
			.cdtData$GalParams[["RFE"]] <- getInfoNetcdfData(tt, .cdtData$GalParams[["RFE"]],
															str_trim(tclvalue(dir.RFE)), str_trim(tclvalue(file.period)))
		})

		tkconfigure(bt.RFE, command = function(){
			dirrfe <- tk_choose.dir(getwd(), "")
			tclvalue(dir.RFE) <- if(!is.na(dirrfe)) dirrfe else ""
		})

		######
		tkgrid(txt.stnfl, row = 0, column = 0, sticky = 'we', rowspan = 1, columnspan = 5, padx = 1, pady = 0, ipadx = 1, ipady = 1)
		tkgrid(cb.stnfl, row = 1, column = 0, sticky = 'we', rowspan = 1, columnspan = 4, padx = 0, pady = 0, ipadx = 1, ipady = 1)
		tkgrid(bt.stnfl, row = 1, column = 4, sticky = 'w', rowspan = 1, columnspan = 1, padx = 0, pady = 0, ipadx = 1, ipady = 1)

		tkgrid(txt.RFE, row = 2, column = 0, sticky = 'we', rowspan = 1, columnspan = 3, padx = 1, pady = 0, ipadx = 1, ipady = 1)
		tkgrid(set.RFE, row = 2, column = 3, sticky = 'we', rowspan = 1, columnspan = 2, padx = 1, pady = 0, ipadx = 1, ipady = 1)
		tkgrid(en.RFE, row = 3, column = 0, sticky = 'we', rowspan = 1, columnspan = 4, padx = 0, pady = 0, ipadx = 1, ipady = 1)
		tkgrid(bt.RFE, row = 3, column = 4, sticky = 'w', rowspan = 1, columnspan = 1, padx = 0, pady = 0, ipadx = 1, ipady = 1)

		infobulle(cb.stnfl, 'Select the file from the list')
		status.bar.display(cb.stnfl, 'Select the file containing the gauge data')
		infobulle(bt.stnfl, 'Browse file if not listed')
		status.bar.display(bt.stnfl, 'Browse file if not listed')
		infobulle(en.RFE, 'Enter the full path to directory containing the RFE or Adjusted RFE files')
		status.bar.display(en.RFE, 'Enter the full path to directory containing the RFE or Adjusted RFE files')
		infobulle(bt.RFE, 'Or browse here')
		status.bar.display(bt.RFE, 'Or browse here')
		infobulle(set.RFE, 'Setting netcdf data options')
		status.bar.display(set.RFE, 'Setting netcdf data options')

		####################################

		frDEM <- tkframe(frTab1, relief = 'sunken', borderwidth = 2, padx = 3, pady = 3)

		file.grddem <- tclVar(.cdtData$GalParams$DEM.file)

		statedem <- if(.cdtData$GalParams$blank$blank == "2" |
						.cdtData$GalParams$Grid.Creation$grid == "2" |
						.cdtData$GalParams$auxvar$dem |
						.cdtData$GalParams$auxvar$slope |
						.cdtData$GalParams$auxvar$aspect) 'normal' else 'disabled'

		txt.grddem <- tklabel(frDEM, text = "Elevation data (NetCDF)", anchor = 'w', justify = 'left')
		cb.grddem <- ttkcombobox(frDEM, values = unlist(listOpenFiles), textvariable = file.grddem, state = statedem, width = largeur1)
		bt.grddem <- tkbutton(frDEM, text = "...", state = statedem)

		tkconfigure(bt.grddem, command = function(){
			nc.opfiles <- getOpenNetcdf(tt, initialdir = getwd())
			if(!is.null(nc.opfiles)){
				update.OpenFiles('netcdf', nc.opfiles)
				listOpenFiles[[length(listOpenFiles) + 1]] <<- nc.opfiles[[1]]
				tclvalue(file.grddem) <- nc.opfiles[[1]]
				lapply(list(cb.stnfl, cb.grddem, cb.blkshp), tkconfigure, values = unlist(listOpenFiles))
			}
		})

		tkgrid(txt.grddem, row = 0, column = 0, sticky = 'we', rowspan = 1, columnspan = 2, padx = 1, pady = 1, ipadx = 1, ipady = 1)
		tkgrid(cb.grddem, row = 1, column = 0, sticky = 'we', rowspan = 1, columnspan = 1, padx = 0, pady = 1, ipadx = 1, ipady = 1)
		tkgrid(bt.grddem, row = 1, column = 1, sticky = 'w', rowspan = 1, columnspan = 1, padx = 0, pady = 1, ipadx = 1, ipady = 1)

		infobulle(cb.grddem, 'Select the file in the list')
		status.bar.display(cb.grddem, 'File containing the elevation data in netcdf')
		infobulle(bt.grddem, 'Browse file if not listed')
		status.bar.display(bt.grddem, 'Browse file if not listed')

		####################################

		frLMCoef <- tkframe(frTab1, relief = 'sunken', borderwidth = 2, padx = 3, pady = 3)

		LMCoef.dir <- tclVar(.cdtData$GalParams$LMCOEF$dir.LMCoef)
		stateLMCoef <- if(str_trim(.cdtData$GalParams$Merging$mrg.method) == "Spatio-Temporal LM") 'normal' else 'disabled'

		txt.LMCoef.dir <- tklabel(frLMCoef, text = "Directory of LMCoef files", anchor = 'w', justify = 'left')
		en.LMCoef.dir <- tkentry(frLMCoef, textvariable = LMCoef.dir, state = stateLMCoef, width = largeur2)
		bt.LMCoef.dir <- tkbutton(frLMCoef, text = "...", state = stateLMCoef)

		tkconfigure(bt.LMCoef.dir, command = function(){
			dirLM <- tk_choose.dir(getwd(), "")
			tclvalue(LMCoef.dir) <- if(!is.na(dirLM)) dirLM else ""
		})

		tkgrid(txt.LMCoef.dir, row = 4, column = 0, sticky = 'we', rowspan = 1, columnspan = 6, padx = 1, pady = 1, ipadx = 1, ipady = 1)
		tkgrid(en.LMCoef.dir, row = 5, column = 0, sticky = 'we', rowspan = 1, columnspan = 5, padx = 0, pady = 1, ipadx = 1, ipady = 1)
		tkgrid(bt.LMCoef.dir, row = 5, column = 5, sticky = 'w', rowspan = 1, columnspan = 1, padx = 0, pady = 1, ipadx = 1, ipady = 1)

		infobulle(en.LMCoef.dir, 'Enter the full path to directory containing the LM coefficients files')
		status.bar.display(en.LMCoef.dir, 'Enter the full path to directory containing the LM coefficients files')
		infobulle(bt.LMCoef.dir, 'or browse here')
		status.bar.display(bt.LMCoef.dir, 'or browse here')

		####################################

		tkgrid(frtimestep, row = 0, column = 0, sticky = '', padx = 1, pady = 1, ipadx = 1, ipady = 1)
		tkgrid(frInputData, row = 1, column = 0, sticky = 'we', padx = 1, pady = 1, ipadx = 1, ipady = 1)
		tkgrid(frDEM, row = 2, column = 0, sticky = 'we', padx = 1, pady = 1, ipadx = 1, ipady = 1)
		tkgrid(frLMCoef, row = 3, column = 0, sticky = 'we', padx = 1, pady = 1, ipadx = 1, ipady = 1)

		####################################

		tkgrid(frTab1, padx = 0, pady = 1, ipadx = 1, ipady = 1)

	####################################

	frTab2 <- tkframe(conf.tab2)

		####################################

		frMrg <- tkframe(frTab2, relief = 'sunken', borderwidth = 2, padx = 3, pady = 3)

		cb.MrgMthd <- c("Regression Kriging", "Spatio-Temporal LM", "Simple Bias Adjustment")
		mrg.method <- tclVar(str_trim(.cdtData$GalParams$Merging$mrg.method))
		mrg.min.stn <- tclVar(.cdtData$GalParams$Merging$min.stn)

		varCreateGrd <- tclVar(.cdtData$GalParams$Grid.Creation$grid)

		stategrd <- if(str_trim(.cdtData$GalParams$Grid.Creation$grid) == '3') 'normal' else 'disabled'

		txt.mrg <- tklabel(frMrg, text = 'Merging method', anchor = 'w', justify = 'left')
		cb.mrg <- ttkcombobox(frMrg, values = cb.MrgMthd, textvariable = mrg.method, width = largeur3)
		bt.mrg.interp <- ttkbutton(frMrg, text = "Merging Interpolations Parameters")
		fr.CreateGrd <- ttklabelframe(frMrg, text = 'Grid for Interpolation', relief = 'groove', labelanchor = "n")
		fr.mrgMin <- tkframe(frMrg)

		rbt.grdRFE <- tkradiobutton(fr.CreateGrd, text = "From RFE", variable = varCreateGrd, value = "1", anchor = 'w', justify = 'left')
		rbt.grdDEM <- tkradiobutton(fr.CreateGrd, text = "From DEM", variable = varCreateGrd, value = "2", anchor = 'w', justify = 'left')
		rbt.grdNEW <- tkradiobutton(fr.CreateGrd, text = "New Grid", variable = varCreateGrd, value = "3", anchor = 'w', justify = 'left')
		bt.getNewgrid <- ttkbutton(fr.CreateGrd, text = "Create", state = stategrd)

		txt.min.nbrs.stn <- tklabel(fr.mrgMin, text = 'Minimum number of station', anchor = 'e', justify = 'right')
		en.min.nbrs.stn <- tkentry(fr.mrgMin, width = 4, textvariable = mrg.min.stn, justify = 'right')

		#####

		tkconfigure(bt.mrg.interp, command = function(){
			.cdtData$GalParams[["Merging"]] <- getInterpolationPars(tt, .cdtData$GalParams[["Merging"]], interpChoix = 0)
		})

		tkconfigure(bt.getNewgrid, command = function(){
			.cdtData$GalParams[["Grid.Creation"]] <- getNewGridParams(tt, .cdtData$GalParams[["Grid.Creation"]])
		})

		#####

		tkgrid(txt.mrg, row = 0, column = 0, sticky = 'we', rowspan = 1, columnspan = 2, padx = 1, pady = 1, ipadx = 1, ipady = 1)
		tkgrid(cb.mrg, row = 0, column = 2, sticky = 'we', rowspan = 1, columnspan = 4, padx = 1, pady = 1, ipadx = 1, ipady = 1)
		tkgrid(bt.mrg.interp, row = 1, column = 0, sticky = 'we', rowspan = 1, columnspan = 6, padx = 1, pady = 1, ipadx = 1, ipady = 1)
		tkgrid(fr.CreateGrd, row = 2, column = 0, sticky = 'we', rowspan = 1, columnspan = 6, padx = 1, pady = 1, ipadx = 1, ipady = 1)
		tkgrid(fr.mrgMin, row = 3, column = 0, sticky = '', rowspan = 1, columnspan = 6, padx = 1, pady = 1, ipadx = 1, ipady = 1)

		tkgrid(rbt.grdRFE, row = 0, column = 0, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
		tkgrid(rbt.grdDEM, row = 0, column = 1, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
		tkgrid(rbt.grdNEW, row = 0, column = 2, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
		tkgrid(bt.getNewgrid, row = 1, column = 0, sticky = 'we', rowspan = 1, columnspan = 3, padx = 1, pady = 1, ipadx = 1, ipady = 1)

		tkgrid(txt.min.nbrs.stn, row = 0, column = 0, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
		tkgrid(en.min.nbrs.stn, row = 0, column = 1, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)

		infobulle(cb.mrg, 'Method to be used to perform merging')
		status.bar.display(cb.mrg, 'Method to be used to perform merging')

		infobulle(en.min.nbrs.stn, 'Minimum number of station with data to be used to do the merging')
		status.bar.display(en.min.nbrs.stn, 'Minimum number of station with data to be used to do the merging')

		infobulle(bt.getNewgrid, 'Set the new grid')
		status.bar.display(bt.getNewgrid, 'Set the new grid')

		###############

		tkbind(cb.mrg, "<<ComboboxSelected>>", function(){
			stateLMCoef <- if(tclvalue(mrg.method) == "Spatio-Temporal LM") 'normal' else 'disabled'
			tkconfigure(en.LMCoef.dir, state = stateLMCoef)
			tkconfigure(bt.LMCoef.dir, state = stateLMCoef)
		})

		###########
		tkbind(rbt.grdRFE, "<Button-1>", function(){
			tkconfigure(bt.getNewgrid, state = 'disabled')
			statedem <- if(tclvalue(blankGrd) == "Use DEM" |
						tclvalue(dem.auxvar) == "1" | tclvalue(slope.auxvar) == "1" |
						tclvalue(aspect.auxvar) == "1") 'normal' else 'disabled'
			tkconfigure(cb.grddem, state = statedem)
			tkconfigure(bt.grddem, state = statedem)
		})
		tkbind(rbt.grdDEM, "<Button-1>", function(){
			tkconfigure(bt.getNewgrid, state = 'disabled')
			tkconfigure(cb.grddem, state = 'normal')
			tkconfigure(bt.grddem, state = 'normal')
		})
		tkbind(rbt.grdNEW, "<Button-1>", function(){
			tkconfigure(bt.getNewgrid, state = 'normal')
			statedem <- if(tclvalue(blankGrd) == "Use DEM" |
						tclvalue(dem.auxvar) == "1" | tclvalue(slope.auxvar) == "1" |
						tclvalue(aspect.auxvar) == "1") 'normal' else 'disabled'
			tkconfigure(cb.grddem, state = statedem)
			tkconfigure(bt.grddem, state = statedem)
		})

		####################################

		frRnoR <- tkframe(frTab2, relief = 'sunken', borderwidth = 2, padx = 3, pady = 3)

		use.RnoR <- tclVar(.cdtData$GalParams$RnoR$use.RnoR)
		smooth.RnoR <- tclVar(.cdtData$GalParams$RnoR$smooth.RnoR)

		stateRnoR <- if(.cdtData$GalParams$RnoR$use.RnoR) 'normal' else 'disabled'

		########
		chk.use.rnr <- tkcheckbutton(frRnoR, variable = use.RnoR, text = 'Apply Rain-no-Rain mask', anchor = 'w', justify = 'left')
		chk.smooth.rnr <- tkcheckbutton(frRnoR, variable = smooth.RnoR, text = 'Smooth Rain-no-Rain mask', anchor = 'w', justify = 'left', state = stateRnoR)

		tkgrid(chk.use.rnr, row = 1, column = 0, sticky = 'ew', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
		tkgrid(chk.smooth.rnr, row = 2, column = 0, sticky = 'ew', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)

		infobulle(chk.use.rnr, 'Check this box to apply a mask over no rain area')
		status.bar.display(chk.use.rnr, 'Check this box to apply a mask over no rain area')
		infobulle(chk.smooth.rnr, 'Check this box to smooth the gradient between high value and no rain area')
		status.bar.display(chk.smooth.rnr, 'Check this box to smooth the gradient between high value and no rain area')

		tkbind(chk.use.rnr, "<Button-1>", function(){
			stateRnoR <- if(tclvalue(use.RnoR) == '0') 'normal' else 'disabled'
			tkconfigure(chk.smooth.rnr, state = stateRnoR)
		})

		####################################

		frauxvar <- tkframe(frTab2, relief = 'sunken', borderwidth = 2, padx = 3, pady = 3)

		dem.auxvar <- tclVar(.cdtData$GalParams$auxvar$dem)
		slope.auxvar <- tclVar(.cdtData$GalParams$auxvar$slope)
		aspect.auxvar <- tclVar(.cdtData$GalParams$auxvar$aspect)
		lon.auxvar <- tclVar(.cdtData$GalParams$auxvar$lon)
		lat.auxvar <- tclVar(.cdtData$GalParams$auxvar$lat)

		txt.auxvar <- tklabel(frauxvar, text = 'Include auxiliary variables', anchor = 'w', justify = 'left')
		dem.chk.auxvar <- tkcheckbutton(frauxvar, variable = dem.auxvar, text = 'DEM', anchor = 'w', justify = 'left')
		slope.chk.auxvar <- tkcheckbutton(frauxvar, variable = slope.auxvar, text = 'Slope', anchor = 'w', justify = 'left')
		aspect.chk.auxvar <- tkcheckbutton(frauxvar, variable = aspect.auxvar, text = 'Aspect', anchor = 'w', justify = 'left')
		lon.chk.auxvar <- tkcheckbutton(frauxvar, variable = lon.auxvar, text = 'Lon', anchor = 'w', justify = 'left')
		lat.chk.auxvar <- tkcheckbutton(frauxvar, variable = lat.auxvar, text = 'Lat', anchor = 'w', justify = 'left')

		tkgrid(txt.auxvar, row = 0, column = 0, sticky = 'we', rowspan = 1, columnspan = 5, padx = 1, pady = 1, ipadx = 1, ipady = 1)
		tkgrid(dem.chk.auxvar, row = 1, column = 0, sticky = 'we', rowspan = 1, columnspan = 1, padx = 0, pady = 1, ipadx = 0, ipady = 1)
		tkgrid(slope.chk.auxvar, row = 1, column = 1, sticky = 'we', rowspan = 1, columnspan = 1, padx = 0, pady = 1, ipadx = 0, ipady = 1)
		tkgrid(aspect.chk.auxvar, row = 1, column = 2, sticky = 'we', rowspan = 1, columnspan = 1, padx = 0, pady = 1, ipadx = 0, ipady = 1)
		tkgrid(lon.chk.auxvar, row = 1, column = 3, sticky = 'we', rowspan = 1, columnspan = 1, padx = 0, pady = 1, ipadx = 0, ipady = 1)
		tkgrid(lat.chk.auxvar, row = 1, column = 4, sticky = 'we', rowspan = 1, columnspan = 1, padx = 0, pady = 1, ipadx = 0, ipady = 1)

		infobulle(dem.chk.auxvar, 'Include elevation data as auxiliary variable')
		status.bar.display(dem.chk.auxvar, 'Include elevation data as auxiliary variable')
		infobulle(slope.chk.auxvar, 'Include slope data as auxiliary variable')
		status.bar.display(slope.chk.auxvar, 'Include slope data as auxiliary variable')
		infobulle(aspect.chk.auxvar, 'Include aspect data as auxiliary variable')
		status.bar.display(aspect.chk.auxvar, 'Include aspect data as auxiliary variable')
		infobulle(lon.chk.auxvar, 'Include longitude as auxiliary variable')
		status.bar.display(lon.chk.auxvar, 'Include longitude as auxiliary variable')
		infobulle(lat.chk.auxvar, 'Include latitude as auxiliary variable')
		status.bar.display(lat.chk.auxvar, 'Include latitude as auxiliary variable')

		###########

		tkbind(dem.chk.auxvar, "<Button-1>", function(){
			statedem <- if(tclvalue(dem.auxvar) == "0" |
							(tclvalue(blankGrd) == "Use DEM" |
							tclvalue(slope.auxvar) == "1" | tclvalue(aspect.auxvar) == "1" |
							tclvalue(varCreateGrd) == "2")) 'normal' else 'disabled'
			tkconfigure(cb.grddem, state = statedem)
			tkconfigure(bt.grddem, state = statedem)
		})

		tkbind(slope.chk.auxvar, "<Button-1>", function(){
			statedem <- if(tclvalue(slope.auxvar) == "0" |
							(tclvalue(blankGrd) == "Use DEM" |
							tclvalue(dem.auxvar) == "1" | tclvalue(aspect.auxvar) == "1" |
							tclvalue(varCreateGrd) == "2")) 'normal' else 'disabled'
			tkconfigure(cb.grddem, state = statedem)
			tkconfigure(bt.grddem, state = statedem)
		})

		tkbind(aspect.chk.auxvar, "<Button-1>", function(){
			statedem <- if(tclvalue(aspect.auxvar) == "0" |
							(tclvalue(blankGrd) == "Use DEM" |
							tclvalue(slope.auxvar) == "1" | tclvalue(dem.auxvar) == "1" |
							tclvalue(varCreateGrd) == "2")) 'normal' else 'disabled'
			tkconfigure(cb.grddem, state = statedem)
			tkconfigure(bt.grddem, state = statedem)
		})

		####################################

		tkgrid(frMrg, row = 0, column = 0, sticky = 'we', padx = 1, pady = 1, ipadx = 1, ipady = 1)
		tkgrid(frRnoR, row = 1, column = 0, sticky = 'we', padx = 1, pady = 1, ipadx = 1, ipady = 1)
		tkgrid(frauxvar, row = 2, column = 0, sticky = 'we', padx = 1, pady = 1, ipadx = 1, ipady = 1)

		####################################

		tkgrid(frTab2, padx = 0, pady = 1, ipadx = 1, ipady = 1)

	####################################

	frTab3 <- tkframe(conf.tab3)

		####################################

		frSave <- tkframe(frTab3, relief = 'sunken', borderwidth = 2, padx = 3, pady = 3)

		dir2save <- tclVar(.cdtData$GalParams$output$dir)
		outmrgff <- tclVar(.cdtData$GalParams$output$format)

		txt.dir2save <- tklabel(frSave, text = 'Directory to save result', anchor = 'w', justify = 'left')
		en.dir2save <- tkentry(frSave, textvariable = dir2save, width = largeur2)
		bt.dir2save <- tkbutton(frSave, text = "...")
		txt.outmrgff <- tklabel(frSave, text = 'Merged data filename format', anchor = 'w', justify = 'left')
		en.outmrgff <- tkentry(frSave, textvariable = outmrgff, width = largeur2)

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

		tkgrid(txt.dir2save, row = 0, column = 0, sticky = 'we', rowspan = 1, columnspan = 2, padx = 1, pady = 2, ipadx = 1, ipady = 1)
		tkgrid(en.dir2save, row = 1, column = 0, sticky = 'we', rowspan = 1, columnspan = 1, padx = 0, pady = 1, ipadx = 1, ipady = 1)
		tkgrid(bt.dir2save, row = 1, column = 1, sticky = 'w', rowspan = 1, columnspan = 1, padx = 0, pady = 1, ipadx = 1, ipady = 1)
		tkgrid(txt.outmrgff, row = 2, column = 0, sticky = 'we', rowspan = 1, columnspan = 2, padx = 1, pady = 1, ipadx = 1, ipady = 1)
		tkgrid(en.outmrgff, row = 3, column = 0, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)

		infobulle(en.dir2save, 'Enter the full path to directory to save result')
		status.bar.display(en.dir2save, 'Enter the full path to directory to save result')
		infobulle(bt.dir2save, 'or browse here')
		status.bar.display(bt.dir2save, 'or browse here')
		infobulle(en.outmrgff, 'Format of the merged data files names in NetCDF, example: rr_mrg_1981011_ALL.nc')
		status.bar.display(en.outmrgff, 'Format of the merged data files names in NetCDF, example: rr_mrg_1981011_ALL.nc')

		############################################

		frblank <- tkframe(frTab3, relief = 'sunken', borderwidth = 2, padx = 3, pady = 3)

		blankGrd <- tclVar()
		cb.blankVAL <- c("None", "Use DEM", "Use ESRI shapefile")
		tclvalue(blankGrd) <- switch(str_trim(.cdtData$GalParams$blank$blank), 
										'1' = cb.blankVAL[1], 
										'2' = cb.blankVAL[2],
										'3' = cb.blankVAL[3])

		txt.blankGrd <- tklabel(frblank, text = 'Blank merged data', anchor = 'w', justify = 'left')
		cb.blankGrd <- ttkcombobox(frblank, values = cb.blankVAL, textvariable = blankGrd, width = largeur4)

		#####
		tkgrid(txt.blankGrd, row = 0, column = 0, sticky = 'we', rowspan = 1, columnspan = 2, padx = 1, pady = 1, ipadx = 1, ipady = 1)
		tkgrid(cb.blankGrd, row = 1, column = 0, sticky = 'we', rowspan = 1, columnspan = 1, padx = 0, pady = 1, ipadx = 1, ipady = 1)

		infobulle(cb.blankGrd, 'Blank grid outside the country boundaries or over ocean')
		status.bar.display(cb.blankGrd, 'Blank grid outside the country boundaries or over ocean\ngiven by the DEM mask or the shapefile')

		############################################

		tkbind(cb.blankGrd, "<<ComboboxSelected>>", function(){
			stateshp <- if(tclvalue(blankGrd) == 'Use ESRI shapefile') 'normal' else 'disabled'
			tkconfigure(cb.blkshp, state = stateshp)
			tkconfigure(bt.blkshp, state = stateshp)

			statedem <- if(tclvalue(blankGrd) == "Use DEM" |
							tclvalue(varCreateGrd) == "2" |
							tclvalue(slope.auxvar) == "1" |
							tclvalue(dem.auxvar) == "1" |
							tclvalue(aspect.auxvar) == "1") 'normal' else 'disabled'
			tkconfigure(cb.grddem, state = statedem)
			tkconfigure(bt.grddem, state = statedem)
		})

		############################################

		frSHP <- tkframe(frTab3, relief = 'sunken', borderwidth = 2, padx = 3, pady = 3)

		file.blkshp <- tclVar(.cdtData$GalParams$blank$SHP.file)

		stateshp <- if(str_trim(.cdtData$GalParams$blank$blank) == '3') 'normal' else 'disabled'

		txt.blkshp <- tklabel(frSHP, text = "ESRI shapefiles for blanking", anchor = 'w', justify = 'left')
		cb.blkshp <- ttkcombobox(frSHP, values = unlist(listOpenFiles), textvariable = file.blkshp, state = stateshp, width = largeur1)
		bt.blkshp <- tkbutton(frSHP, text = "...", state = stateshp)

		########

		tkconfigure(bt.blkshp, command = function(){
			shp.opfiles <- getOpenShp(tt)
			if(!is.null(shp.opfiles)){
				update.OpenFiles('shp', shp.opfiles)
				tclvalue(file.blkshp) <- shp.opfiles[[1]]
				listOpenFiles[[length(listOpenFiles) + 1]] <<- shp.opfiles[[1]]
				lapply(list(cb.stnfl, cb.grddem, cb.blkshp), tkconfigure, values = unlist(listOpenFiles))
			}
		})

		#####

		tkgrid(txt.blkshp, row = 0, column = 0, sticky = 'we', rowspan = 1, columnspan = 5, padx = 1, pady = 1, ipadx = 1, ipady = 1)
		tkgrid(cb.blkshp, row = 1, column = 0, sticky = 'we', rowspan = 1, columnspan = 4, padx = 0, pady = 1, ipadx = 1, ipady = 1)
		tkgrid(bt.blkshp, row = 1, column = 4, sticky = 'w', rowspan = 1, columnspan = 1, padx = 0, pady = 1, ipadx = 1, ipady = 1)

		infobulle(cb.blkshp, 'Select the file in the list')
		status.bar.display(cb.blkshp, 'Select the file containing the ESRI shapefiles')
		infobulle(bt.blkshp, 'Browse file if not listed')
		status.bar.display(bt.blkshp, 'Browse file if not listed')

		############################################
		tkgrid(frSave, row = 0, column = 0, sticky = 'we', padx = 1, pady = 1, ipadx = 1, ipady = 1)
		tkgrid(frblank, row = 1, column = 0, sticky = '', padx = 1, pady = 1, ipadx = 1, ipady = 1)
		tkgrid(frSHP, row = 2, column = 0, sticky = 'we', padx = 1, pady = 1, ipadx = 1, ipady = 1)

		####################################

		tkgrid(frTab3, padx = 0, pady = 1, ipadx = 1, ipady = 1)

	####################################

	bt.prm.OK <- ttkbutton(frMRG1, text = .cdtEnv$tcl$lang$global[['button']][['1']])
	bt.prm.CA <- ttkbutton(frMRG1, text = .cdtEnv$tcl$lang$global[['button']][['2']])

	tkconfigure(bt.prm.OK, command = function(){
		if(str_trim(tclvalue(file.stnfl)) == ""){
			tkmessageBox(message = "Select the file containing the station data", icon = "warning", type = "ok")
			tkwait.window(tt)
		}else if(str_trim(tclvalue(dir.RFE)) %in% c("", "NA")){
			tkmessageBox(message = "Browse or enter the directory containing the RFE files", icon = "warning", type = "ok")
			tkwait.window(tt)
		}else if(tclvalue(mrg.method) == "Spatio-Temporal LM" &
			str_trim(tclvalue(LMCoef.dir)) %in% c("", "NA"))
		{
			tkmessageBox(message = "Enter the path to directory containing the LM coefficients", icon = "warning", type = "ok")
			tkwait.window(tt)
		}else if((tclvalue(aspect.auxvar) == "1" | tclvalue(slope.auxvar) == "1" |
				tclvalue(dem.auxvar) == "1" | tclvalue(varCreateGrd) == "2" |
				tclvalue(blankGrd) == "Use DEM") & (str_trim(tclvalue(file.grddem)) == ""))
		{
			tkmessageBox(message = "You have to provide DEM data in NetCDF format", icon = "warning", type = "ok")
			tkwait.window(tt)
		}else if(str_trim(tclvalue(file.blkshp)) == "" & str_trim(tclvalue(blankGrd)) == "Use ESRI shapefile"){
			tkmessageBox(message = "You have to provide the shapefile", icon = "warning", type = "ok")
			tkwait.window(tt)
		}else if(str_trim(tclvalue(dir2save)) %in% c("", "NA")){
			tkmessageBox(message = "Browse or enter the path to directory to save results", icon = "warning", type = "ok")
			tkwait.window(tt)
		}else{
			.cdtData$GalParams$STN.file <- str_trim(tclvalue(file.stnfl))
			.cdtData$GalParams$RFE$dir <- str_trim(tclvalue(dir.RFE))

			.cdtData$GalParams$Merging$mrg.method <- str_trim(tclvalue(mrg.method))
			.cdtData$GalParams$Merging$min.stn <- as.numeric(str_trim(tclvalue(mrg.min.stn)))
			.cdtData$GalParams$Grid.Creation$grid <- tclvalue(varCreateGrd)
			.cdtData$GalParams$LMCOEF$dir.LMCoef <- str_trim(tclvalue(LMCoef.dir))

			.cdtData$GalParams$period <- periodVAL[CbperiodVAL %in% str_trim(tclvalue(file.period))]

			.cdtData$GalParams$RnoR$use.RnoR <- switch(tclvalue(use.RnoR), '0' = FALSE, '1' = TRUE)
			.cdtData$GalParams$RnoR$smooth.RnoR <- switch(tclvalue(smooth.RnoR), '0' = FALSE, '1' = TRUE)

			.cdtData$GalParams$auxvar$dem <- switch(tclvalue(dem.auxvar), '0' = FALSE, '1' = TRUE)
			.cdtData$GalParams$auxvar$slope <- switch(tclvalue(slope.auxvar), '0' = FALSE, '1' = TRUE)
			.cdtData$GalParams$auxvar$aspect <- switch(tclvalue(aspect.auxvar), '0' = FALSE, '1' = TRUE)
			.cdtData$GalParams$auxvar$lon <- switch(tclvalue(lon.auxvar), '0' = FALSE, '1' = TRUE)
			.cdtData$GalParams$auxvar$lat <- switch(tclvalue(lat.auxvar), '0' = FALSE, '1' = TRUE)

			.cdtData$GalParams$DEM.file <- str_trim(tclvalue(file.grddem))

			.cdtData$GalParams$output$dir <- str_trim(tclvalue(dir2save))
			.cdtData$GalParams$output$format <- str_trim(tclvalue(outmrgff))

			.cdtData$GalParams$blank$blank <- switch(str_trim(tclvalue(blankGrd)),
													"None" = '1', "Use DEM" = '2',
													"Use ESRI shapefile" = '3')
			.cdtData$GalParams$blank$SHP.file <- str_trim(tclvalue(file.blkshp))

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

	tkgrid(bt.prm.CA, row = 0, column = 0, sticky = 'w', padx = 5, pady = 1, ipadx = 1, ipady = 1)
	tkgrid(bt.prm.OK, row = 0, column = 1, sticky = 'e', padx = 5, pady = 1, ipadx = 1, ipady = 1)

	####################################

	tkgrid(frMRG0, row = 0, column = 0, sticky = 'nswe', rowspan = 1, columnspan = 2, padx = 1, pady = 1, ipadx = 1, ipady = 1)
	tkgrid(frMRG1, row = 1, column = 1, sticky = 'se', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)

	tcl('update')
	tkgrid(bwnote, sticky = 'nwes')
	tkgrid.columnconfigure(bwnote, 0, weight = 1)

	####################################

	tkwm.withdraw(tt)
	tcl('update')
	tt.w <- as.integer(tkwinfo("reqwidth", tt))
	tt.h <- as.integer(tkwinfo("reqheight", tt))
	tt.x <- as.integer(.cdtEnv$tcl$data$width.scr*0.5 - tt.w*0.5)
	tt.y <- as.integer(.cdtEnv$tcl$data$height.scr*0.5 - tt.h*0.5)
	tkwm.geometry(tt, paste0('+', tt.x, '+', tt.y))
	tkwm.transient(tt)
	tkwm.title(tt, 'Merging data - Settings')
	tkwm.deiconify(tt)

	tkfocus(tt)
	tkbind(tt, "<Destroy>", function(){
		tkgrab.release(tt)
		tkfocus(.cdtEnv$tcl$main$win)
	})
	tkwait.window(tt)
	invisible()
}
