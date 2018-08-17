
Precip_mergeGetInfoALL <- function(){
	listOpenFiles <- openFile_ttkcomboList()
	if(WindowsOS()){
		largeur0 <- 19
		largeur1 <- 42
		largeur2 <- 45
		largeur3 <- 27
		largeur4 <- 28
		largeur5 <- 32
	}else{
		largeur0 <- 16
		largeur1 <- 38
		largeur2 <- 39
		largeur3 <- 21
		largeur4 <- 17
		largeur5 <- 22
	}

	# xml.dlg <- file.path(.cdtDir$dirLocal, "languages", "cdtPrecip_MergingALL_dlgBox.xml")
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
	conf.tab3 <- bwAddTab(bwnote, text = "Bias Coeff")
	conf.tab4 <- bwAddTab(bwnote, text = "LM Coeff")
	conf.tab5 <- bwAddTab(bwnote, text = "Output")

	bwRaiseTab(bwnote, conf.tab1)
	tkgrid.columnconfigure(conf.tab1, 0, weight = 1)
	tkgrid.columnconfigure(conf.tab2, 0, weight = 1)
	tkgrid.columnconfigure(conf.tab3, 0, weight = 1)
	tkgrid.columnconfigure(conf.tab4, 0, weight = 1)
	tkgrid.columnconfigure(conf.tab5, 0, weight = 1)

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
			.cdtData$GalParams[["Merging.Date"]] <- getInfoDateRange(.cdtEnv$tcl$main$win,
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
		txt.RFE <- tklabel(frInputData, text = 'Directory containing RFE data', anchor = 'w', justify = 'left')
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
		infobulle(en.RFE, 'Enter the full path to the directory containing the RFE data')
		status.bar.display(en.RFE, 'Enter the full path to the directory containing the RFE data')
		infobulle(bt.RFE, 'Or browse here')
		status.bar.display(bt.RFE, 'Or browse here')
		infobulle(set.RFE, 'Setting netcdf data options')
		status.bar.display(set.RFE, 'Setting netcdf data options')

		####################################

		frDEM <- tkframe(frTab1, relief = 'sunken', borderwidth = 2, padx = 3, pady = 3)

		file.grddem <- tclVar(.cdtData$GalParams$DEM.file)

		statedem <- if((!.cdtData$GalParams$BIAS$deja.calc &
						.cdtData$GalParams$BIAS$interp.method == "NN") |
						(.cdtData$GalParams$Merging$mrg.method == "Spatio-Temporal LM" &
						!.cdtData$GalParams$LMCOEF$deja.calc &
						.cdtData$GalParams$LMCOEF$interp.method == "NN") |
						.cdtData$GalParams$blank$blank == "2") 'normal' else 'disabled'

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

		tkgrid(frtimestep, row = 0, column = 0, sticky = '', padx = 1, pady = 1, ipadx = 1, ipady = 1)
		tkgrid(frInputData, row = 1, column = 0, sticky = 'we', padx = 1, pady = 1, ipadx = 1, ipady = 1)
		tkgrid(frDEM, row = 2, column = 0, sticky = 'we', padx = 1, pady = 1, ipadx = 1, ipady = 1)

		####################################

		tkgrid(frTab1, padx = 0, pady = 1, ipadx = 1, ipady = 1)

	####################################

	frTab2 <- tkframe(conf.tab2)

		####################################

		frMrg <- tkframe(frTab2, relief = 'sunken', borderwidth = 2, padx = 3, pady = 3)

		cb.MrgMthd <- c("Regression Kriging", "Spatio-Temporal LM", "Simple Bias Adjustment")
		mrg.method <- tclVar(str_trim(.cdtData$GalParams$Merging$mrg.method))
		mrg.min.stn <- tclVar(.cdtData$GalParams$Merging$min.stn)
		mrg.min.non.zero <- tclVar(.cdtData$GalParams$Merging$min.non.zero)

		txt.mrg <- tklabel(frMrg, text = 'Merging method', anchor = 'w', justify = 'left')
		cb.mrg <- ttkcombobox(frMrg, values = cb.MrgMthd, textvariable = mrg.method, width = largeur4)
		bt.mrg.interp <- ttkbutton(frMrg, text = "Merging Interpolations Parameters")

		txt.min.nbrs.stn <- tklabel(frMrg, text = 'Min.Nb.Stn', anchor = 'e', justify = 'right')
		en.min.nbrs.stn <- tkentry(frMrg, width = 4, textvariable = mrg.min.stn, justify = 'right')
		txt.min.non.zero <- tklabel(frMrg, text = 'Min.No.Zero', anchor = 'e', justify = 'right')
		en.min.non.zero <- tkentry(frMrg, width = 4, textvariable = mrg.min.non.zero, justify = 'right')

		tkconfigure(bt.mrg.interp, command = function(){
			.cdtData$GalParams[["Merging"]] <- getInterpolationPars(tt, .cdtData$GalParams[["Merging"]], interpChoix = 0)
		})

		tkgrid(txt.mrg, row = 0, column = 0, sticky = 'we', rowspan = 1, columnspan = 2, padx = 1, pady = 1, ipadx = 1, ipady = 1)
		tkgrid(cb.mrg, row = 0, column = 2, sticky = 'we', rowspan = 1, columnspan = 4, padx = 1, pady = 1, ipadx = 1, ipady = 1)
		tkgrid(bt.mrg.interp, row = 1, column = 0, sticky = 'we', rowspan = 1, columnspan = 6, padx = 1, pady = 1, ipadx = 1, ipady = 1)

		tkgrid(txt.min.nbrs.stn, row = 2, column = 0, sticky = 'e', rowspan = 1, columnspan = 2, padx = 1, pady = 1, ipadx = 1, ipady = 1)
		tkgrid(en.min.nbrs.stn, row = 2, column = 2, sticky = 'w', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
		tkgrid(txt.min.non.zero, row = 2, column = 3, sticky = 'e', rowspan = 1, columnspan = 2, padx = 1, pady = 1, ipadx = 1, ipady = 1)
		tkgrid(en.min.non.zero, row = 2, column = 5, sticky = 'w', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)

		infobulle(cb.mrg, 'Method to be used to perform merging')
		status.bar.display(cb.mrg, 'Method to be used to perform merging')

		infobulle(en.min.nbrs.stn, 'Minimum number of gauges with data to be used to do the merging')
		status.bar.display(en.min.nbrs.stn, 'Minimum number of gauges with data to be used to do the merging')
		infobulle(en.min.non.zero, 'Minimum number of non-zero gauge values to perform the merging')
		status.bar.display(en.min.non.zero, 'Minimum number of non-zero gauge values to perform the merging')

		###############
		tkbind(cb.mrg, "<<ComboboxSelected>>", function(){
			stateLMCoef1 <- if(tclvalue(mrg.method) == "Spatio-Temporal LM") 'normal' else 'disabled'
			stateLMCoef2 <- if(tclvalue(mrg.method) == "Spatio-Temporal LM" & tclvalue(lmcoef.calc) == "0") 'normal' else 'disabled'
			stateLMCoef3 <- if(tclvalue(mrg.method) == "Spatio-Temporal LM" & tclvalue(lmcoef.calc) == "1") 'normal' else 'disabled'
			tkconfigure(chk.LMCoef, state = stateLMCoef1)
			tkconfigure(bt.baseLM, state = stateLMCoef2)
			tkconfigure(bt.LMCoef.interp, state = stateLMCoef2)
			tkconfigure(en.LMCoef.dir, state = stateLMCoef3)
			tkconfigure(bt.LMCoef.dir, state = stateLMCoef3)

			statedem <- if((tclvalue(bias.calc) == "0" &
							.cdtData$GalParams$BIAS$interp.method == "NN") |
							(tclvalue(mrg.method) == "Spatio-Temporal LM" &
							tclvalue(lmcoef.calc) == "0" &
							.cdtData$GalParams$LMCOEF$interp.method == "NN") |
							tclvalue(blankGrd) == "Use DEM") 'normal' else 'disabled'

			tkconfigure(cb.grddem, state = statedem)
			tkconfigure(bt.grddem, state = statedem)
		})

		####################################

		frRnoR <- tkframe(frTab2, relief = 'sunken', borderwidth = 2, padx = 3, pady = 3)

		use.RnoR <- tclVar(.cdtData$GalParams$RnoR$use.RnoR)
		maxdist.RnoR <- tclVar(.cdtData$GalParams$RnoR$maxdist.RnoR)
		smooth.RnoR <- tclVar(.cdtData$GalParams$RnoR$smooth.RnoR)

		stateRnoR <- if(.cdtData$GalParams$RnoR$use.RnoR) 'normal' else 'disabled'

		########
		txt.mrg.pars <- tklabel(frRnoR, text = 'Rain-no-Rain mask', anchor = 'w', justify = 'left')
		chk.use.rnr <- tkcheckbutton(frRnoR, variable = use.RnoR, text = 'Apply Rain-no-Rain mask', anchor = 'w', justify = 'left')
		txt.maxdist.rnr <- tklabel(frRnoR, text = 'maxdist.RnoR', anchor = 'e', justify = 'right')
		en.maxdist.rnr <- tkentry(frRnoR, width = 4, textvariable = maxdist.RnoR, justify = 'right', state = stateRnoR)
		chk.smooth.rnr <- tkcheckbutton(frRnoR, variable = smooth.RnoR, text = 'Smooth Rain-no-Rain mask', anchor = 'w', justify = 'left', state = stateRnoR)

		tkgrid(txt.mrg.pars, row = 0, column = 0, sticky = '', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
		tkgrid(chk.use.rnr, row = 1, column = 0, sticky = 'ew', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
		tkgrid(txt.maxdist.rnr, row = 2, column = 0, sticky = 'e', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
		tkgrid(en.maxdist.rnr, row = 2, column = 1, sticky = 'w', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
		tkgrid(chk.smooth.rnr, row = 3, column = 0, sticky = 'ew', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)

		infobulle(chk.use.rnr, 'Check this box to apply a mask over no rain area')
		status.bar.display(chk.use.rnr, 'Check this box to apply a mask over no rain area')
		infobulle(en.maxdist.rnr, 'Maximum distance (in decimal degrees) to be used to interpolate Rain-noRain mask')
		status.bar.display(en.maxdist.rnr, 'Maximum distance (in decimal degrees) to be used to interpolate Rain-noRain mask')
		infobulle(chk.smooth.rnr, 'Check this box to smooth the gradient between high value and no rain area')
		status.bar.display(chk.smooth.rnr, 'Check this box to smooth the gradient between high value and no rain area')

		tkbind(chk.use.rnr, "<Button-1>", function(){
			stateRnoR <- if(tclvalue(use.RnoR) == '0') 'normal' else 'disabled'
			tkconfigure(en.maxdist.rnr, state = stateRnoR)
			tkconfigure(chk.smooth.rnr, state = stateRnoR)
		})

		####################################

		tkgrid(frMrg, row = 0, column = 0, sticky = 'we', padx = 1, pady = 1, ipadx = 1, ipady = 1)
		tkgrid(frRnoR, row = 1, column = 0, sticky = 'we', padx = 1, pady = 1, ipadx = 1, ipady = 1)

		####################################

		tkgrid(frTab2, padx = 0, pady = 1, ipadx = 1, ipady = 1)

	####################################

	frTab3 <- tkframe(conf.tab3)

		####################################

		frameBias <- tkframe(frTab3, relief = 'sunken', borderwidth = 2, padx = 5, pady = 5)

		cb.biasMthd <- c("Quantile.Mapping", "Multiplicative.Bias.Var", "Multiplicative.Bias.Mon")
		bias.method <- tclVar(str_trim(.cdtData$GalParams$BIAS$bias.method))

		txt.bias <- tklabel(frameBias, text = 'Bias method', anchor = 'w', justify = 'left')
		cb.bias <- ttkcombobox(frameBias, values = cb.biasMthd, textvariable = bias.method, width = largeur3)

		tkgrid(txt.bias, row = 0, column = 0, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
		tkgrid(cb.bias, row = 0, column = 1, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)

		infobulle(cb.bias, 'Select the method to be used to calculate the Bias Factors or Parameters')
		status.bar.display(cb.bias, 'Select the method to be used to calculate the Bias Factors or Parameters')

		####################################

		frameBiasSet <- tkframe(frTab3, relief = 'sunken', borderwidth = 2, padx = 3, pady = 3)

		bias.calc <- tclVar(.cdtData$GalParams$BIAS$deja.calc)
		statebias1 <- if(.cdtData$GalParams$BIAS$deja.calc) 'disabled' else 'normal'

		chk.bias <- tkcheckbutton(frameBiasSet, variable = bias.calc, text =  "Bias factors are already calculated", anchor = 'w', justify = 'left', background = 'lightblue')
		bt.baseBias <- ttkbutton(frameBiasSet, text = "Set Bias Base Period", state = statebias1)
		bt.bias.interp <- ttkbutton(frameBiasSet, text = "Bias Interpolations Parameters", state = statebias1)

		tkconfigure(bt.baseBias, command = function(){
			.cdtData$GalParams[["BIAS"]] <- getInfoBasePeriod(tt, .cdtData$GalParams[["BIAS"]])
		})

		tkconfigure(bt.bias.interp, command = function(){
			.cdtData$GalParams[["BIAS"]] <- getInterpolationPars(tt, .cdtData$GalParams[["BIAS"]], interpChoix = 1)

			statedem <- if((tclvalue(bias.calc) == "0" &
							.cdtData$GalParams$BIAS$interp.method == "NN") |
							(tclvalue(mrg.method) == "Spatio-Temporal LM" &
							tclvalue(lmcoef.calc) == "0" &
							.cdtData$GalParams$LMCOEF$interp.method == "NN") |
							tclvalue(blankGrd) == "Use DEM") 'normal' else 'disabled'

			tkconfigure(cb.grddem, state = statedem)
			tkconfigure(bt.grddem, state = statedem)
		})

		tkgrid(chk.bias, row = 0, column = 0, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
		tkgrid(bt.baseBias, row = 1, column = 0, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
		tkgrid(bt.bias.interp, row = 2, column = 0, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)

		infobulle(chk.bias, 'Check this box if the bias factors or parameters are already calculated')
		status.bar.display(chk.bias, 'Check this box if the bias factors or parameters are already calculated')
		infobulle(bt.baseBias, 'Set the base period to be used to compute bias factors')
		status.bar.display(bt.baseBias, 'Set the base period to be used to compute bias factors')

		###############
		tkbind(chk.bias, "<Button-1>", function(){
			statebias1 <- if(tclvalue(bias.calc) == '1') 'normal' else 'disabled'
			statebias2 <- if(tclvalue(bias.calc) == '0') 'normal' else 'disabled'
			tkconfigure(bt.baseBias, state = statebias1)
			tkconfigure(bt.bias.interp, state = statebias1)
			tkconfigure(en.bias.dir, state = statebias2)
			tkconfigure(bt.bias.dir, state = statebias2)

			statedem <- if((tclvalue(bias.calc) == "1" &
							.cdtData$GalParams$BIAS$interp.method == "NN") |
							(tclvalue(mrg.method) == "Spatio-Temporal LM" &
							tclvalue(lmcoef.calc) == "0" &
							.cdtData$GalParams$LMCOEF$interp.method == "NN") |
							tclvalue(blankGrd) == "Use DEM") 'normal' else 'disabled'

			tkconfigure(cb.grddem, state = statedem)
			tkconfigure(bt.grddem, state = statedem)
		})

		####################################

		frameBiasDir <- tkframe(frTab3, relief = 'sunken', borderwidth = 2, padx = 3, pady = 3)

		bias.dir <- tclVar(.cdtData$GalParams$BIAS$dir.Bias)
		statebias2 <- if(.cdtData$GalParams$BIAS$deja.calc) 'normal' else 'disabled'

		txt.bias.dir <- tklabel(frameBiasDir, text = "Directory of bias files", anchor = 'w', justify = 'left')
		en.bias.dir <- tkentry(frameBiasDir, textvariable = bias.dir, state = statebias2, width = largeur2)
		bt.bias.dir <- tkbutton(frameBiasDir, text = "...", state = statebias2)

		tkconfigure(bt.bias.dir, command = function(){
			dirbias <- tk_choose.dir(getwd(), "")
			tclvalue(bias.dir) <- if(!is.na(dirbias)) dirbias else ""
		})

		tkgrid(txt.bias.dir, row = 0, column = 0, sticky = 'we', rowspan = 1, columnspan = 6, padx = 1, pady = 1, ipadx = 1, ipady = 1)
		tkgrid(en.bias.dir, row = 1, column = 0, sticky = 'we', rowspan = 1, columnspan = 5, padx = 0, pady = 1, ipadx = 1, ipady = 1)
		tkgrid(bt.bias.dir, row = 1, column = 5, sticky = 'w', rowspan = 1, columnspan = 1, padx = 0, pady = 1, ipadx = 1, ipady = 1)

		infobulle(en.bias.dir, 'Enter the full path to directory containing the bias files')
		status.bar.display(en.bias.dir, 'Enter the full path to directory containing the bias files')

		####################################

		tkgrid(frameBias, row = 0, column = 0, sticky = '', padx = 1, pady = 1, ipadx = 1, ipady = 1)
		tkgrid(frameBiasSet, row = 1, column = 0, sticky = '', padx = 1, pady = 1, ipadx = 1, ipady = 1)
		tkgrid(frameBiasDir, row = 2, column = 0, sticky = 'we', padx = 1, pady = 1, ipadx = 1, ipady = 1)

		####################################

		tkgrid(frTab3, padx = 0, pady = 1, ipadx = 1, ipady = 1)

	####################################

	frTab4 <- tkframe(conf.tab4)

		####################################

		frLMCoef <- tkframe(frTab4, relief = 'sunken', borderwidth = 2, padx = 3, pady = 3)

		lmcoef.calc <- tclVar(.cdtData$GalParams$LMCOEF$deja.calc)

		stateLMCoef1 <- if(str_trim(.cdtData$GalParams$Merging$mrg.method) == "Spatio-Temporal LM") 'normal' else 'disabled'
		stateLMCoef2 <- if(str_trim(.cdtData$GalParams$Merging$mrg.method) == "Spatio-Temporal LM" & !.cdtData$GalParams$LMCOEF$deja.calc) 'normal' else 'disabled'
		stateLMCoef3 <- if(str_trim(.cdtData$GalParams$Merging$mrg.method) == "Spatio-Temporal LM" & .cdtData$GalParams$LMCOEF$deja.calc) 'normal' else 'disabled'

		chk.LMCoef <- tkcheckbutton(frLMCoef, variable = lmcoef.calc, text =  "LMCoef are already calculated", state = stateLMCoef1, anchor = 'w', justify = 'left', background = 'lightblue')
		bt.baseLM <- ttkbutton(frLMCoef, text = "Set LMCoef Base Period", state = stateLMCoef2)
		bt.LMCoef.interp <- ttkbutton(frLMCoef, text = "LMCoef Interpolations Parameters", state = stateLMCoef2)

		tkconfigure(bt.baseLM, command = function(){
			.cdtData$GalParams[["LMCOEF"]] <- getInfoBasePeriod(tt, .cdtData$GalParams[["LMCOEF"]])
		})

		tkconfigure(bt.LMCoef.interp, command = function(){
			.cdtData$GalParams[["LMCOEF"]] <- getInterpolationPars(tt, .cdtData$GalParams[["LMCOEF"]], interpChoix = 1)

			statedem <- if((tclvalue(bias.calc) == "0" &
							.cdtData$GalParams$BIAS$interp.method == "NN") |
							(tclvalue(mrg.method) == "Spatio-Temporal LM" &
							tclvalue(lmcoef.calc) == "0" &
							.cdtData$GalParams$LMCOEF$interp.method == "NN") |
							tclvalue(blankGrd) == "Use DEM") 'normal' else 'disabled'

			tkconfigure(cb.grddem, state = statedem)
			tkconfigure(bt.grddem, state = statedem)
		})

		tkgrid(chk.LMCoef, row = 0, column = 0, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
		tkgrid(bt.baseLM, row = 1, column = 0, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
		tkgrid(bt.LMCoef.interp, row = 2, column = 0, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)

		infobulle(chk.LMCoef, 'Check this box if the linear model coefficients are already calculated')
		status.bar.display(chk.LMCoef, 'Check this box if the linear model coefficients are already calculated')
		infobulle(bt.baseLM, 'Start and end year to be used to compute LM coefficients')
		status.bar.display(bt.baseLM, 'Start and end year to be used to compute LM coefficients')

		###############
		tkbind(chk.LMCoef, "<Button-1>", function(){
			stateLMCoef2 <- if(tclvalue(lmcoef.calc) == '1' & tclvalue(mrg.method) == "Spatio-Temporal LM") 'normal' else 'disabled'
			stateLMCoef3 <- if(tclvalue(lmcoef.calc) == '0' & tclvalue(mrg.method) == "Spatio-Temporal LM") 'normal' else 'disabled'
			tkconfigure(bt.baseLM, state = stateLMCoef2)
			tkconfigure(bt.LMCoef.interp, state = stateLMCoef2)
			tkconfigure(en.LMCoef.dir, state = stateLMCoef3)
			tkconfigure(bt.LMCoef.dir, state = stateLMCoef3)

			statedem <- if((tclvalue(bias.calc) == "0" &
							.cdtData$GalParams$BIAS$interp.method == "NN") |
							(tclvalue(mrg.method) == "Spatio-Temporal LM" &
							tclvalue(lmcoef.calc) == "1" &
							.cdtData$GalParams$LMCOEF$interp.method == "NN") |
							tclvalue(blankGrd) == "Use DEM") 'normal' else 'disabled'

			tkconfigure(cb.grddem, state = statedem)
			tkconfigure(bt.grddem, state = statedem)
		})

		####################################

		frLMCoefdir <- tkframe(frTab4, relief = 'sunken', borderwidth = 2, padx = 3, pady = 3)

		LMCoef.dir <- tclVar(.cdtData$GalParams$LMCOEF$dir.LMCoef)

		txt.LMCoef.dir <- tklabel(frLMCoefdir, text = "Directory of LMCoef files", anchor = 'w', justify = 'left')
		en.LMCoef.dir <- tkentry(frLMCoefdir, textvariable = LMCoef.dir, state = stateLMCoef3, width = largeur2)
		bt.LMCoef.dir <- tkbutton(frLMCoefdir, text = "...", state = stateLMCoef3)

		tkconfigure(bt.LMCoef.dir, command = function(){
			dirLM <- tk_choose.dir(getwd(), "")
			tclvalue(LMCoef.dir) <- if(!is.na(dirLM)) dirLM else ""
		})

		tkgrid(txt.LMCoef.dir, row = 0, column = 0, sticky = 'we', rowspan = 1, columnspan = 6, padx = 1, pady = 1, ipadx = 1, ipady = 1)
		tkgrid(en.LMCoef.dir, row = 1, column = 0, sticky = 'we', rowspan = 1, columnspan = 5, padx = 0, pady = 1, ipadx = 1, ipady = 1)
		tkgrid(bt.LMCoef.dir, row = 1, column = 5, sticky = 'w', rowspan = 1, columnspan = 1, padx = 0, pady = 1, ipadx = 1, ipady = 1)

		infobulle(en.LMCoef.dir, 'Enter the full path to directory containing the LM coefficients files')
		status.bar.display(en.LMCoef.dir, 'Enter the full path to directory containing the LM coefficients files')
		infobulle(bt.LMCoef.dir, 'or browse here')
		status.bar.display(bt.LMCoef.dir, 'or browse here')

		####################################

		tkgrid(frLMCoef, row = 0, column = 0, sticky = '', padx = 1, pady = 1, ipadx = 1, ipady = 1)
		tkgrid(frLMCoefdir, row = 1, column = 0, sticky = 'we', padx = 1, pady = 1, ipadx = 1, ipady = 1)

		####################################

		tkgrid(frTab4, padx = 0, pady = 1, ipadx = 1, ipady = 1)

	####################################

	frTab5 <- tkframe(conf.tab5)

		####################################

		frSave <- tkframe(frTab5, relief = 'sunken', borderwidth = 2, padx = 3, pady = 3)

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

		frblank <- tkframe(frTab5, relief = 'sunken', borderwidth = 2, padx = 3, pady = 3)

		blankGrd <- tclVar()
		cb.blankVAL <- c("None", "Use DEM", "Use ESRI shapefile")
		tclvalue(blankGrd) <- switch(str_trim(.cdtData$GalParams$blank$blank), 
										'1' = cb.blankVAL[1], 
										'2' = cb.blankVAL[2],
										'3' = cb.blankVAL[3])

		txt.blankGrd <- tklabel(frblank, text = 'Blank merged data', anchor = 'w', justify = 'left')
		cb.blankGrd <- ttkcombobox(frblank, values = cb.blankVAL, textvariable = blankGrd, width = largeur5)

		#####
		tkgrid(txt.blankGrd, row = 0, column = 0, sticky = 'we', rowspan = 1, columnspan = 2, padx = 1, pady = 1, ipadx = 1, ipady = 1)
		tkgrid(cb.blankGrd, row = 1, column = 0, sticky = 'we', rowspan = 1, columnspan = 1, padx = 0, pady = 1, ipadx = 1, ipady = 1)

		infobulle(cb.blankGrd, 'Blank grid outside the country boundaries or over ocean')
		status.bar.display(cb.blankGrd, 'Blank grid outside the country boundaries  or over ocean\ngiven by the DEM mask or the shapefile')

		############################################

		tkbind(cb.blankGrd, "<<ComboboxSelected>>", function(){
			stateshp <- if(tclvalue(blankGrd) == 'Use ESRI shapefile') 'normal' else 'disabled'
			tkconfigure(cb.blkshp, state = stateshp)
			tkconfigure(bt.blkshp, state = stateshp)

			statedem <- if(tclvalue(blankGrd) == "Use DEM" |
							(tclvalue(bias.calc) == "0" &
							.cdtData$GalParams$BIAS$interp.method == "NN") |
							(tclvalue(mrg.method) == "Spatio-Temporal LM" &
							tclvalue(lmcoef.calc) == "0" &
							.cdtData$GalParams$LMCOEF$interp.method == "NN")) 'normal' else 'disabled'
			tkconfigure(cb.grddem, state = statedem)
			tkconfigure(bt.grddem, state = statedem)
		})

		############################################

		frSHP <- tkframe(frTab5, relief = 'sunken', borderwidth = 2, padx = 3, pady = 3)

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

		tkgrid(frTab5, padx = 0, pady = 1, ipadx = 1, ipady = 1)

	####################################

	bt.prm.OK <- ttkbutton(frMRG1, text = .cdtEnv$tcl$lang$global[['button']][['1']])
	bt.prm.CA <- ttkbutton(frMRG1, text = .cdtEnv$tcl$lang$global[['button']][['2']])

	tkconfigure(bt.prm.OK, command = function(){
		if(str_trim(tclvalue(file.stnfl)) == ""){
			tkmessageBox(message = "Select the file containing the station data", icon = "warning", type = "ok")
			tkwait.window(tt)
		}else if(str_trim(tclvalue(dir.RFE)) %in% c("", "NA")){
			tkmessageBox(message = "Browse or enter the  directory containing the RFE files", icon = "warning", type = "ok")
			tkwait.window(tt)
		}else if(tclvalue(bias.calc) == '1' & str_trim(tclvalue(bias.dir)) %in% c("", "NA"))
		{
			tkmessageBox(message = "Enter the path to directory containing the Bias factors", icon = "warning", type = "ok")
			tkwait.window(tt)
		}else if(tclvalue(mrg.method) == "Spatio-Temporal LM" & tclvalue(lmcoef.calc) == '1' &
			str_trim(tclvalue(LMCoef.dir)) %in% c("", "NA"))
		{
			tkmessageBox(message = "Enter the path to directory containing the lm coefficients", icon = "warning", type = "ok")
			tkwait.window(tt)
		}else if(((tclvalue(bias.calc) == '0' & .cdtData$GalParams$BIAS$interp.method == "NN") |
				(tclvalue(mrg.method) == "Spatio-Temporal LM" & tclvalue(lmcoef.calc) == '0' &
				.cdtData$GalParams$LMCOEF$interp.method == "NN") | tclvalue(blankGrd) == "Use DEM") &
				(str_trim(tclvalue(file.grddem)) == ""))
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

			.cdtData$GalParams$BIAS$bias.method <- str_trim(tclvalue(bias.method))
			.cdtData$GalParams$BIAS$deja.calc <- switch(tclvalue(bias.calc), '0' = FALSE, '1' = TRUE)
			.cdtData$GalParams$BIAS$dir.Bias <- str_trim(tclvalue(bias.dir))

			.cdtData$GalParams$Merging$mrg.method <- str_trim(tclvalue(mrg.method))
			.cdtData$GalParams$Merging$min.stn <- as.numeric(str_trim(tclvalue(mrg.min.stn)))
			.cdtData$GalParams$Merging$min.non.zero <- as.numeric(str_trim(tclvalue(mrg.min.non.zero)))

			.cdtData$GalParams$period <- periodVAL[CbperiodVAL %in% str_trim(tclvalue(file.period))]

			.cdtData$GalParams$LMCOEF$deja.calc <- switch(tclvalue(lmcoef.calc), '0' = FALSE, '1' = TRUE)
			.cdtData$GalParams$LMCOEF$dir.LMCoef <- str_trim(tclvalue(LMCoef.dir))

			.cdtData$GalParams$RnoR$use.RnoR <- switch(tclvalue(use.RnoR), '0' = FALSE, '1' = TRUE)
			.cdtData$GalParams$RnoR$maxdist.RnoR <- as.numeric(str_trim(tclvalue(maxdist.RnoR)))
			.cdtData$GalParams$RnoR$smooth.RnoR <- switch(tclvalue(smooth.RnoR), '0' = FALSE, '1' = TRUE)

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
