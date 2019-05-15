
mergeDekadInfoRain <- function(){
	listOpenFiles <- openFile_ttkcomboList()
	if (WindowsOS()){
		largeur0 <- 45
		largeur1 <- 42
		largeur2 <- 25
	}else{
		largeur0 <- 39
		largeur1 <- 38
		largeur2 <- 20
	}

	# xml.dlg <- file.path(.cdtDir$dirLocal, "languages", "cdtPrecip_UpdateDek_dlgBox.xml")
	# lang.dlg <- cdtLanguageParse(xml.dlg, .cdtData$Config$lang.iso)

	####################################

	tt <- tktoplevel()
	tkgrab.set(tt)
	tkfocus(tt)

	frMRG0 <- tkframe(tt, relief = 'raised', borderwidth = 2)
	frMRG1 <- tkframe(tt)

	bwnote <- bwNoteBook(frMRG0)
	conf.tab1 <- bwAddTab(bwnote, text = "Station Data")
	conf.tab2 <- bwAddTab(bwnote, text = "RFE Data")
	conf.tab3 <- bwAddTab(bwnote, text = "Bias Correction")
	conf.tab4 <- bwAddTab(bwnote, text = "Merging")
	conf.tab5 <- bwAddTab(bwnote, text = "Output")

	bwRaiseTab(bwnote, conf.tab1)
	tkgrid.columnconfigure(conf.tab1, 0, weight = 1)
	tkgrid.columnconfigure(conf.tab2, 0, weight = 1)
	tkgrid.columnconfigure(conf.tab3, 0, weight = 1)
	tkgrid.columnconfigure(conf.tab4, 0, weight = 1)
	tkgrid.columnconfigure(conf.tab5, 0, weight = 1)

	############################################

	frTab1 <- tkframe(conf.tab1)

		####################################

		frDate <- tkframe(frTab1, relief = 'sunken', borderwidth = 2, padx = 3, pady = 3)

		istart.yrs <- tclVar(str_trim(.cdtData$GalParams$Merging.Date$year))
		istart.mon <- tclVar(str_trim(.cdtData$GalParams$Merging.Date$month))
		istart.day <- tclVar(str_trim(.cdtData$GalParams$Merging.Date$dekad))

		date.txt <- tklabel(frDate, text = 'Date', anchor = 'w', justify = 'left')
		yrs.txt <- tklabel(frDate, text = 'Year')
		mon.txt <- tklabel(frDate, text = 'Month')
		day.txt <- tklabel(frDate, text = 'Dekad')

		yrs1.v <- tkentry(frDate, width = 5, textvariable = istart.yrs, justify = "right")
		mon1.v <- tkentry(frDate, width = 5, textvariable = istart.mon, justify = "right")
		day1.v <- tkentry(frDate, width = 5, textvariable = istart.day, justify = "right", state = 'normal')

		tkgrid(yrs.txt, row = 0, column = 1, sticky = 'ew', padx = 1, pady = 1)
		tkgrid(mon.txt, row = 0, column = 2, sticky = 'ew', padx = 1, pady = 1)
		tkgrid(day.txt, row = 0, column = 3, sticky = 'ew', padx = 1, pady = 1)
		tkgrid(date.txt, row = 1, column = 0, sticky = 'ew', padx = 1, pady = 1)
		tkgrid(yrs1.v, row = 1, column = 1, sticky = 'ew', padx = 1, pady = 1)
		tkgrid(mon1.v, row = 1, column = 2, sticky = 'ew', padx = 1, pady = 1)
		tkgrid(day1.v, row = 1, column = 3, sticky = 'ew', padx = 1, pady = 1)

		infobulle(frDate, 'Date of merging')
		status.bar.display(frDate, 'Date of merging')

		###########################################

		frStnfl <- tkframe(frTab1, relief = 'sunken', borderwidth = 2, padx = 3, pady = 3)

		file.stnfl <- tclVar(str_trim(.cdtData$GalParams$STN$file))
		no.stnfl <- tclVar(.cdtData$GalParams$STN$No.Stn.Data)

		if(tclvalue(no.stnfl) == '0'){
			state.stnfl <- 'normal'
			stateMrg <- 'normal'
		}else{
			state.stnfl <- 'disabled'
			stateMrg <- 'disabled'
		}

		chk.stnfl <- tkcheckbutton(frStnfl, variable = no.stnfl, text = 'No station data available', anchor = 'w', justify = 'left')
		txt.stnfl <- tklabel(frStnfl, text = 'Station data file', anchor = 'w', justify = 'left')
		cb.stnfl <- ttkcombobox(frStnfl, values = unlist(listOpenFiles), textvariable = file.stnfl, width = largeur1, state = state.stnfl)
		bt.stnfl <- tkbutton(frStnfl, text = "...", state = state.stnfl)

		tkconfigure(bt.stnfl, command = function(){
			dat.opfiles <- getOpenFiles(tt)
			if(!is.null(dat.opfiles)){
				update.OpenFiles('ascii', dat.opfiles)
				listOpenFiles[[length(listOpenFiles) + 1]] <<- dat.opfiles[[1]]
				tclvalue(file.stnfl) <- dat.opfiles[[1]]
				lapply(list(cb.stnfl, cb.grdrfe, cb.grddem, cb.blkshp), tkconfigure, values = unlist(listOpenFiles))
			}
		})

		tkgrid(chk.stnfl, row = 0, column = 0, sticky = 'we', rowspan = 1, columnspan = 2, padx = 1, pady = 1, ipadx = 1, ipady = 1)
		tkgrid(txt.stnfl, row = 1, column = 0, sticky = 'we', rowspan = 1, columnspan = 2, padx = 1, pady = 1, ipadx = 1, ipady = 1)
		tkgrid(cb.stnfl, row = 2, column = 0, sticky = 'we', rowspan = 1, columnspan = 1, padx = 0, pady = 1, ipadx = 1, ipady = 1)
		tkgrid(bt.stnfl, row = 2, column = 1, sticky = 'w', rowspan = 1, columnspan = 1, padx = 0, pady = 1, ipadx = 1, ipady = 1)

		infobulle(chk.stnfl, 'Check if there is no station data available')
		status.bar.display(chk.stnfl, 'Check if there is no station data available')
		infobulle(cb.stnfl, 'Choose the file in the list')
		status.bar.display(cb.stnfl, 'Choose the file containing the gauge data')
		infobulle(bt.stnfl, 'Browse file if not listed')
		status.bar.display(bt.stnfl, 'Browse file if not listed')

		tkbind(chk.stnfl, "<Button-1>", function(){
			stateSTN <- if(tclvalue(no.stnfl) == '0') 'disabled' else 'normal'
			tkconfigure(cb.stnfl, state = stateSTN)
			tkconfigure(bt.stnfl, state = stateSTN)

			stateMRG <- if(tclvalue(no.stnfl) == '0') 'disabled' else 'normal'
			tkconfigure(cb.mrg, state = stateMRG)
			tkconfigure(bt.mrg.interp, state = stateMRG)
			tkconfigure(en.min.nbrs.stn, state = stateMRG)
			tkconfigure(en.min.non.zero, state = stateMRG)
			tkconfigure(cb.blankGrd, state = stateMRG)
			tkconfigure(chk.use.rnr, state = stateMRG)

			stateRnR <- if(tclvalue(no.stnfl) == '0') 'disabled' else {if(tclvalue(use.RnoR) == '1') 'normal' else 'disabled'}
			tkconfigure(en.maxdist.rnr, state = stateRnR)
			tkconfigure(chk.smooth.rnr, state = stateRnR)

			stateLMC <- if(tclvalue(no.stnfl) == '0') 'disabled' else {if(tclvalue(mrg.method) == "Spatio-Temporal LM") 'normal' else 'disabled'}
			tkconfigure(en.dir.LM, state = stateLMC)
			tkconfigure(bt.dir.LM, state = stateLMC)

			if(tclvalue(no.stnfl) == '0'){
				stateDEM <- 'disabled'
				stateSHP <- 'disabled'
			}else{
				if(tclvalue(blankGrd) == "None"){
					stateDEM <- 'disabled'
					stateSHP <- 'disabled'
				}
				if(tclvalue(blankGrd) == "Use DEM"){
					stateDEM <- 'normal'
					stateSHP <- 'disabled'
				}
				if(tclvalue(blankGrd) == "Use ESRI shapefile"){
					stateDEM <- 'disabled'
					stateSHP <- 'normal'
				}
			}

			tkconfigure(cb.grddem, state = stateDEM)
			tkconfigure(bt.grddem, state = stateDEM)
			tkconfigure(cb.blkshp, state = stateSHP)
			tkconfigure(bt.blkshp, state = stateSHP)
		})

		####################################

		tkgrid(frDate, row = 0, column = 0, sticky = '', padx = 1, pady = 1, ipadx = 1, ipady = 1)
		tkgrid(frStnfl, row = 1, column = 0, sticky = 'we', padx = 1, pady = 1, ipadx = 1, ipady = 1)

		####################################

		tkgrid(frTab1, padx = 0, pady = 1, ipadx = 1, ipady = 1)

	############################################

	frTab2 <- tkframe(conf.tab2)

		####################################
	
		frRfe <- tkframe(frTab2, relief = 'sunken', borderwidth = 2, padx = 3, pady = 3)

		down.rfe <- tclVar(.cdtData$GalParams$RFE$downloaded)
		file.grdrfe <- tclVar(.cdtData$GalParams$RFE$file)

		if(tclvalue(down.rfe) == '1'){
			stateRFE1 <- 'normal'
			stateRFE2 <- 'disabled'
		}else{
			stateRFE1 <- 'disabled'
			stateRFE2 <- 'normal'
		}

		chk.rfe <- tkcheckbutton(frRfe, variable = down.rfe, text = 'RFE data already downloaded', anchor = 'w', justify = 'left')
		txt.grdrfe <- tklabel(frRfe, text = "RFE file", anchor = 'w', justify = 'left')
		cb.grdrfe <- ttkcombobox(frRfe, values = unlist(listOpenFiles), textvariable = file.grdrfe, width = largeur1, state = stateRFE1)
		bt.grdrfe <- tkbutton(frRfe, text = "...", state = stateRFE1)

		tkconfigure(bt.grdrfe, command = function(){
			nc.opfiles <- getOpenNetcdf(tt, initialdir = getwd())
			if(!is.null(nc.opfiles)){
				update.OpenFiles('netcdf', nc.opfiles)
				listOpenFiles[[length(listOpenFiles) + 1]] <<- nc.opfiles[[1]]
				tclvalue(file.grdrfe) <- nc.opfiles[[1]]
				lapply(list(cb.stnfl, cb.grdrfe, cb.grddem, cb.blkshp), tkconfigure, values = unlist(listOpenFiles))
			}
		})

		tkgrid(chk.rfe, row = 0, column = 0, sticky = 'we', rowspan = 1, columnspan = 4, padx = 1, pady = 1, ipadx = 1, ipady = 1)
		tkgrid(txt.grdrfe, row = 1, column = 0, sticky = 'we', rowspan = 1, columnspan = 4, padx = 1, pady = 1, ipadx = 1, ipady = 1)
		tkgrid(cb.grdrfe, row = 2, column = 0, sticky = 'we', rowspan = 1, columnspan = 3, padx = 0, pady = 1, ipadx = 1, ipady = 1)
		tkgrid(bt.grdrfe, row = 2, column = 3, sticky = 'w', rowspan = 1, columnspan = 1, padx = 0, pady = 1, ipadx = 1, ipady = 1)

		infobulle(chk.rfe, 'Check if you have already downloaded RFE data')
		status.bar.display(chk.rfe, 'Check if you have already downloaded RFE data')
		infobulle(cb.grdrfe, 'File containing the RFE data in netcdf')
		status.bar.display(cb.grdrfe, 'File containing the RFE data in netcdf')
		infobulle(bt.grdrfe, 'Browse file if not listed')
		status.bar.display(bt.grdrfe, 'Browse file if not listed')

		####################################

		frDown <- ttklabelframe(frTab2, text = "Download RFE", relief = 'groove')

		data.rfe <- tclVar(.cdtData$GalParams$RFE$source)
		minLon <- tclVar(str_trim(.cdtData$GalParams$bbox$minlon))
		maxLon <- tclVar(str_trim(.cdtData$GalParams$bbox$maxlon))
		minLat <- tclVar(str_trim(.cdtData$GalParams$bbox$minlat))
		maxLat <- tclVar(str_trim(.cdtData$GalParams$bbox$maxlat))

		cb.down.rfe <- ttkcombobox(frDown, values = c('10-DAYS TAMSATv3', '10-DAYS TAMSATv2', '10-DAYS CHIRPSv2.0', '10-DAYS CHIRPv1.0'), textvariable = data.rfe, width = largeur2, state = stateRFE2)
		frbbox <- tkframe(frDown)

		txt.lon <- tklabel(frbbox, text = "Longitude", anchor = 'e', justify = 'right')
		txt.lat <- tklabel(frbbox, text = "Latitude", anchor = 'e', justify = 'right')
		txt.min <- tklabel(frbbox, text = "Min")
		txt.max <- tklabel(frbbox, text = "Max")
		grd_vlon1 <- tkentry(frbbox, width = 5, justify = "right", textvariable = minLon, state = stateRFE2)
		grd_vlon2 <- tkentry(frbbox, width = 5, justify = "right", textvariable = maxLon, state = stateRFE2)
		grd_vlat1 <- tkentry(frbbox, width = 5, justify = "right", textvariable = minLat, state = stateRFE2)
		grd_vlat2 <- tkentry(frbbox, width = 5, justify = "right", textvariable = maxLat, state = stateRFE2)


		tkgrid(cb.down.rfe, row = 0, column = 0, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
		tkgrid(frbbox, row = 1, column = 0, sticky = '', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)

		tkgrid(txt.min, row = 0, column = 2, sticky = "we", rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
		tkgrid(txt.max, row = 0, column = 3, sticky = "we", rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
		tkgrid(txt.lon, row = 1, column = 0, sticky = "e",  rowspan = 1, columnspan = 2, padx = 1, pady = 1, ipadx = 1, ipady = 1)
		tkgrid(grd_vlon1, row = 1, column = 2, sticky = "w", rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
		tkgrid(grd_vlon2, row = 1, column = 3, sticky = "w", rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
		tkgrid(txt.lat, row = 2, column = 0, sticky = "e", rowspan = 1, columnspan = 2, padx = 1, pady = 1, ipadx = 1, ipady = 1)
		tkgrid(grd_vlat1, row = 2, column = 2, sticky = "w", rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
		tkgrid(grd_vlat2, row = 2, column = 3, sticky = "w", rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)

		infobulle(cb.down.rfe, 'Select the data source')
		status.bar.display(cb.down.rfe, 'Select the data source')
		infobulle(grd_vlon1, 'Minimum longitude in degree')
		status.bar.display(grd_vlon1, 'Minimum longitude in degree')
		infobulle(grd_vlon2, 'Maximum longitude in degree')
		status.bar.display(grd_vlon2, 'Maximum longitude in degree')
		infobulle(grd_vlat1, 'Minimum latitude in degree')
		status.bar.display(grd_vlat1, 'Minimum latitude in degree')
		infobulle(grd_vlat2, 'Maximum latitude in degree')
		status.bar.display(grd_vlat2, 'Maximum latitude in degree')

		tkbind(chk.rfe, "<Button-1>", function(){
			stateRFE <- if(tclvalue(down.rfe) == '1') 'disabled' else 'normal'
			tkconfigure(cb.grdrfe, state = stateRFE)
			tkconfigure(bt.grdrfe, state = stateRFE)

			stateGRD <- if(tclvalue(down.rfe) == '1') 'normal' else 'disabled'
			tkconfigure(cb.down.rfe, state = stateGRD)
			tkconfigure(grd_vlon1, state = stateGRD)
			tkconfigure(grd_vlon2, state = stateGRD)
			tkconfigure(grd_vlat1, state = stateGRD)
			tkconfigure(grd_vlat2, state = stateGRD)
		})

		####################################
		tkgrid(frRfe, row = 0, column = 0, sticky = 'we', padx = 1, pady = 1, ipadx = 1, ipady = 1)
		tkgrid(frDown, row = 1, column = 0, sticky = '', padx = 1, pady = 3, ipadx = 1, ipady = 1)

		####################################

		tkgrid(frTab2, padx = 0, pady = 1, ipadx = 1, ipady = 1)

	############################################

	frTab3 <- tkframe(conf.tab3)

		####################################

		frBias <- tkframe(frTab3, relief = 'sunken', borderwidth = 2, padx = 3, pady = 3)

		adj.bias <- tclVar(.cdtData$GalParams$BIAS$Adjust)
		dir.bias <- tclVar(str_trim(.cdtData$GalParams$BIAS$Dir))
		bias.method <- tclVar(str_trim(.cdtData$GalParams$BIAS$method))
		cb.biasMthd <- c("Multiplicative.Bias.Var", "Multiplicative.Bias.Mon", "Quantile.Mapping")

		stateBiasdir <- if(tclvalue(adj.bias) == '1') 'normal' else 'disabled'
		chk.bias <- tkcheckbutton(frBias, variable = adj.bias, text = 'Perform bias correction', anchor = 'w', justify = 'left')
		txt.mth.bias <- tklabel(frBias, text = 'Bias method', anchor = 'e', justify = 'right')
		cb.mth.bias <- ttkcombobox(frBias, values = cb.biasMthd, textvariable = bias.method, state = stateBiasdir, width = largeur2)
		txt.dir.bias <- tklabel(frBias, text = "Directory of bias files", anchor = 'w', justify = 'left')
		en.dir.bias <- tkentry(frBias, textvariable = dir.bias, state = stateBiasdir, width = largeur0)
		bt.dir.bias <- tkbutton(frBias, text = "...", state = stateBiasdir)

		tkconfigure(bt.dir.bias, command = function(){
			dir4bias <- tk_choose.dir(str_trim(.cdtData$GalParams$BIAS$Dir), "")
			tclvalue(dir.bias) <- if(!is.na(dir4bias)) dir4bias else ""
		})

		tkgrid(chk.bias, row = 0, column = 0, sticky = 'we', rowspan = 1, columnspan = 6, padx = 1, pady = 1, ipadx = 1, ipady = 1)
		tkgrid(txt.mth.bias, row = 1, column = 0, sticky = 'e', rowspan = 1, columnspan = 2, padx = 1, pady = 1, ipadx = 1, ipady = 1)
		tkgrid(cb.mth.bias, row = 1, column = 2, sticky = 'w', rowspan = 1, columnspan = 3, padx = 0, pady = 1, ipadx = 1, ipady = 1)
		tkgrid(txt.dir.bias, row = 2, column = 0, sticky = 'we', rowspan = 1, columnspan = 6, padx = 1, pady = 1, ipadx = 1, ipady = 1)
		tkgrid(en.dir.bias, row = 3, column = 0, sticky = 'we', rowspan = 1, columnspan = 5, padx = 0, pady = 1, ipadx = 1, ipady = 1)
		tkgrid(bt.dir.bias, row = 3, column = 5, sticky = 'w', rowspan = 1, columnspan = 1, padx = 0, pady = 1, ipadx = 1, ipady = 1)

		infobulle(chk.bias, 'Check to perform a bias correction of RFE data')
		status.bar.display(chk.bias, 'Check to perform a bias correction of RFE data')
		infobulle(en.dir.bias, 'Enter the full path to directory containing the mean bias files')
		status.bar.display(en.dir.bias, 'Enter the full path to directory containing the mean bias files')
		infobulle(bt.dir.bias, 'or browse here')
		infobulle(cb.mth.bias, 'Method used to calculate Bias Factors or Parameters')
		status.bar.display(cb.mth.bias, 'Method used to calculate Bias Factors or Parameters')

		tkbind(chk.bias, "<Button-1>", function(){
			stateBS <- if(tclvalue(adj.bias) == '1') 'disabled' else 'normal'
			tkconfigure(cb.mth.bias, state = stateBS)
			tkconfigure(en.dir.bias, state = stateBS)
			tkconfigure(bt.dir.bias, state = stateBS)
		})

		####################################
		tkgrid(frBias, row = 0, column = 0, sticky = 'we', padx = 1, pady = 1, ipadx = 1, ipady = 1)

		####################################

		tkgrid(frTab3, padx = 0, pady = 1, ipadx = 1, ipady = 1)

	############################################

	frTab4 <- tkframe(conf.tab4)

		####################################

		frMrg <- tkframe(frTab4, relief = 'sunken', borderwidth = 2, padx = 3, pady = 3)

		cb.MrgMthd <- c("Regression Kriging", "Spatio-Temporal LM", "Simple Bias Adjustment")

		mrg.method <- tclVar(str_trim(.cdtData$GalParams$Merging$mrg.method))
		mrg.min.stn <- tclVar(.cdtData$GalParams$Merging$min.stn)

		txt.mrg <- tklabel(frMrg, text = 'Merging method', anchor = 'e', justify = 'right')
		cb.mrg <- ttkcombobox(frMrg, values = cb.MrgMthd, textvariable = mrg.method, width = largeur2)
		bt.mrg.interp <- ttkbutton(frMrg, text = "Merging Interpolations Parameters")
		fr.mrgMin <- tkframe(frMrg)

		txt.min.nbrs.stn <- tklabel(fr.mrgMin, text = 'Minimum number of station', anchor = 'e', justify = 'right')
		en.min.nbrs.stn <- tkentry(fr.mrgMin, width = 4, textvariable = mrg.min.stn, justify = 'right')

		tkconfigure(bt.mrg.interp, command = function(){
			.cdtData$GalParams[["Merging"]] <- getInterpolationPars(tt, .cdtData$GalParams[["Merging"]], interpChoix = 0)
		})

		tkgrid(txt.mrg, row = 0, column = 0, sticky = 'e', rowspan = 1, columnspan = 2, padx = 1, pady = 1, ipadx = 1, ipady = 1)
		tkgrid(cb.mrg, row = 0, column = 2, sticky = 'w', rowspan = 1, columnspan = 3, padx = 1, pady = 1, ipadx = 1, ipady = 1)
		tkgrid(bt.mrg.interp, row = 1, column = 0, sticky = 'we', rowspan = 1, columnspan = 5, padx = 1, pady = 1, ipadx = 1, ipady = 1)
		tkgrid(fr.mrgMin, row = 2, column = 0, sticky = '', rowspan = 1, columnspan = 5, padx = 1, pady = 1, ipadx = 1, ipady = 1)

		tkgrid(txt.min.nbrs.stn, row = 0, column = 0, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
		tkgrid(en.min.nbrs.stn, row = 0, column = 1, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)

		infobulle(cb.mrg, 'Method to be used to perform merging')
		status.bar.display(cb.mrg, 'Method to be used to perform merging')

		infobulle(en.min.nbrs.stn, 'Minimum number of station with data to be used to do the merging')
		status.bar.display(en.min.nbrs.stn, 'Minimum number of station with data to be used to do the merging')

		###############
		tkbind(cb.mrg, "<<ComboboxSelected>>", function(){
			if(tclvalue(no.stnfl) == '0'){
				stateLM <- if(tclvalue(mrg.method) == "Spatio-Temporal LM") 'normal' else 'disabled'
			}else stateLM <- 'disabled'
			tkconfigure(en.dir.LM, state = stateLM)
			tkconfigure(bt.dir.LM, state = stateLM)
		})

		############################################

		frRnoR <- tkframe(frTab4, relief = 'sunken', borderwidth = 2, padx = 3, pady = 3)

		use.RnoR <- tclVar(.cdtData$GalParams$RnoR$use.RnoR)
		smooth.RnoR <- tclVar(.cdtData$GalParams$RnoR$smooth.RnoR)

		stateRnoR <- if(.cdtData$GalParams$RnoR$use.RnoR) 'normal' else 'disabled'

		########
		chk.use.rnr <- tkcheckbutton(frRnoR, variable = use.RnoR, text = 'Apply Rain-no-Rain mask', anchor = 'w', justify = 'left')
		chk.smooth.rnr <- tkcheckbutton(frRnoR, variable = smooth.RnoR, text = 'Smooth Rain-no-Rain mask', anchor = 'w', justify = 'left', state = stateRnoR)

		tkgrid(chk.use.rnr, row = 0, column = 0, sticky = 'ew', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
		tkgrid(chk.smooth.rnr, row = 1, column = 0, sticky = 'ew', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)

		infobulle(chk.use.rnr, 'Check this box to apply a mask over no rain area')
		status.bar.display(chk.use.rnr, 'Check this box to apply a mask over no rain area')
		infobulle(chk.smooth.rnr, 'Check this box to smooth the gradient between high value and no rain area')
		status.bar.display(chk.smooth.rnr, 'Check this box to smooth the gradient between high value and no rain area')

		tkbind(chk.use.rnr, "<Button-1>", function(){
			stateRnoR <- if(tclvalue(use.RnoR) == '0') 'normal' else 'disabled'
			tkconfigure(chk.smooth.rnr, state = stateRnoR)
		})

		############################################

		frLMcoef <- tkframe(frTab4, relief = 'sunken', borderwidth = 2, padx = 3, pady = 3)

		dir.LMCoef <- tclVar(.cdtData$GalParams$LMCOEF$dir.LMCoef)

		if(tclvalue(no.stnfl) == '0'){
			stateLMCoef <- if(str_trim(tclvalue(mrg.method)) == "Spatio-Temporal LM") 'normal' else 'disabled'
		}else stateLMCoef <- 'disabled'

		txt.dir.LM <- tklabel(frLMcoef, text = "Directory of LMCoef files", anchor = 'w', justify = 'left')
		en.dir.LM <- tkentry(frLMcoef, textvariable = dir.LMCoef, state = stateLMCoef, width = largeur0)
		bt.dir.LM <- tkbutton(frLMcoef, text = "...", state = stateLMCoef)

		tkconfigure(bt.dir.LM, command = function(){
			dirLM <- tk_choose.dir(getwd(), "")
			tclvalue(dir.LMCoef) <- if(!is.na(dirLM)) dirLM else ""
		})

		tkgrid(txt.dir.LM, row = 3, column = 0, sticky = 'we', rowspan = 1, columnspan = 6, padx = 1, pady = 1, ipadx = 1, ipady = 1)
		tkgrid(en.dir.LM, row = 4, column = 0, sticky = 'we', rowspan = 1, columnspan = 5, padx = 0, pady = 1, ipadx = 1, ipady = 1)
		tkgrid(bt.dir.LM, row = 4, column = 5, sticky = 'we', rowspan = 1, columnspan = 1, padx = 0, pady = 1, ipadx = 1, ipady = 1)

		infobulle(en.dir.LM, 'Enter the full path to directory containing the LM coefficients files')
		status.bar.display(en.dir.LM, 'Enter the full path to directory containing the LM coefficients files')
		infobulle(bt.dir.LM, 'or browse here')
		status.bar.display(bt.dir.LM, 'or browse here')

		####################################
		tkgrid(frMrg, row = 0, column = 0, sticky = '', padx = 1, pady = 1, ipadx = 1, ipady = 1)
		tkgrid(frRnoR, row = 1, column = 0, sticky = '', padx = 1, pady = 1, ipadx = 1, ipady = 1)
		tkgrid(frLMcoef, row = 2, column = 0, sticky = 'we', padx = 1, pady = 1, ipadx = 1, ipady = 1)

		####################################

		tkgrid(frTab4, padx = 0, pady = 1, ipadx = 1, ipady = 1)

	############################################

	frTab5 <- tkframe(conf.tab5)

		####################################

		frBlank <- tkframe(frTab5, relief = 'sunken', borderwidth = 2, padx = 3, pady = 3)

		file.grddem <- tclVar(str_trim(.cdtData$GalParams$blank$DEM.file))
		file.blkshp <- tclVar(str_trim(.cdtData$GalParams$blank$SHP.file))

		blankChx <- c("None", "Use DEM", "Use ESRI shapefile")
		blankGrd <- tclVar()
		tclvalue(blankGrd) <- switch(str_trim(.cdtData$GalParams$blank$blank),
										'1' = blankChx[1],
										'2' = blankChx[2],
										'3' = blankChx[3])

		if(str_trim(.cdtData$GalParams$blank$blank) == '2'){
			statedem <- if(tclvalue(no.stnfl) == '0') 'normal' else 'disabled'
		}else statedem <- 'disabled'
		if(str_trim(.cdtData$GalParams$blank$blank) == '3'){
			stateshp <- if(tclvalue(no.stnfl) == '0') 'normal' else 'disabled'
		}else stateshp <- 'disabled'

		txt.blankGrd <- tklabel(frBlank, text = 'Blank', anchor = 'w', justify = 'left')
		cb.blankGrd <- ttkcombobox(frBlank, values = blankChx, textvariable = blankGrd, state = stateMrg, width = largeur2)
		txt.grddem <- tklabel(frBlank, text = "Elevation data(NetCDF)", anchor = 'w', justify = 'left')
		cb.grddem <- ttkcombobox(frBlank, values = unlist(listOpenFiles), textvariable = file.grddem, state = statedem, width = largeur1)
		bt.grddem <- tkbutton(frBlank, text = "...")
		txt.blkshp <- tklabel(frBlank, text = "ESRI shapefiles for blanking", anchor = 'w', justify = 'left')
		cb.blkshp <- ttkcombobox(frBlank, values = unlist(listOpenFiles), textvariable = file.blkshp, state = stateshp, width = largeur1)
		bt.blkshp <- tkbutton(frBlank, text = "...")

		tkconfigure(bt.grddem, state = statedem, command = function(){
			nc.opfiles <- getOpenNetcdf(tt, initialdir = getwd())
			if(!is.null(nc.opfiles)){
				update.OpenFiles('netcdf', nc.opfiles)
				listOpenFiles[[length(listOpenFiles) + 1]] <<- nc.opfiles[[1]]
				tclvalue(file.grddem) <- nc.opfiles[[1]]
				lapply(list(cb.stnfl, cb.grdrfe, cb.grddem, cb.blkshp), tkconfigure, values = unlist(listOpenFiles))
			}
		})

		tkconfigure(bt.blkshp, state = stateshp, command = function(){
			shp.opfiles <- getOpenShp(tt)
			if(!is.null(shp.opfiles)){
				update.OpenFiles('shp', shp.opfiles)
				tclvalue(file.blkshp) <- shp.opfiles[[1]]
				listOpenFiles[[length(listOpenFiles) + 1]] <<- shp.opfiles[[1]]
				lapply(list(cb.stnfl, cb.grdrfe, cb.grddem, cb.blkshp), tkconfigure, values = unlist(listOpenFiles))
			}
		})

		tkgrid(txt.blankGrd, row = 0, column = 0, sticky = 'w', rowspan = 1, columnspan = 1, padx = 1, pady = 3, ipadx = 1, ipady = 1)
		tkgrid(cb.blankGrd, row = 0, column = 1, sticky = 'we', rowspan = 1, columnspan = 3, padx = 1, pady = 3, ipadx = 1, ipady = 1)

		tkgrid(txt.grddem, row = 1, column = 0, sticky = 'we', rowspan = 1, columnspan = 5, padx = 1, pady = 1, ipadx = 1, ipady = 1)
		tkgrid(cb.grddem, row = 2, column = 0, sticky = 'we', rowspan = 1, columnspan = 4, padx = 0, pady = 1, ipadx = 1, ipady = 1)
		tkgrid(bt.grddem, row = 2, column = 4, sticky = 'w', rowspan = 1, columnspan = 1, padx = 0, pady = 1, ipadx = 1, ipady = 1)
		tkgrid(txt.blkshp, row = 3, column = 0, sticky = 'we', rowspan = 1, columnspan = 5, padx = 1, pady = 1, ipadx = 1, ipady = 1)
		tkgrid(cb.blkshp, row = 4, column = 0, sticky = 'we', rowspan = 1, columnspan = 4, padx = 0, pady = 1, ipadx = 1, ipady = 1)
		tkgrid(bt.blkshp, row = 4, column = 4, sticky = 'w', rowspan = 1, columnspan = 1, padx = 0, pady = 1, ipadx = 1, ipady = 1)

		infobulle(cb.blankGrd, 'Blank grid outside the country boundaries or over ocean')
		status.bar.display(cb.blankGrd, 'Blank grid outside the country boundaries  or\nover ocean given by the DEM mask or the shapefile')
		infobulle(cb.grddem, 'Choose the file in the list')
		status.bar.display(cb.grddem, 'Choose the file containing the elevation data in netcdf')
		infobulle(bt.grddem, 'Browse file if not listed')
		status.bar.display(bt.grddem, 'Browse file if not listed')
		infobulle(cb.blkshp, 'Choose the file in the list')
		status.bar.display(cb.blkshp, 'Choose the file containing the ESRI shapefiles')
		infobulle(bt.blkshp, 'Browse file if not listed')
		status.bar.display(bt.blkshp, 'Browse file if not listed')

		tkbind(cb.blankGrd, "<<ComboboxSelected>>", function(){
			if(tclvalue(blankGrd) == "None"){
				tkconfigure(cb.grddem, state = 'disabled')
				tkconfigure(bt.grddem, state = 'disabled')
				tkconfigure(cb.blkshp, state = 'disabled')
				tkconfigure(bt.blkshp, state = 'disabled')
			}
			if(tclvalue(blankGrd) == "Use DEM"){
				tkconfigure(cb.grddem, state = 'normal')
				tkconfigure(bt.grddem, state = 'normal')
				tkconfigure(cb.blkshp, state = 'disabled')
				tkconfigure(bt.blkshp, state = 'disabled')
			}
			if(tclvalue(blankGrd) == "Use ESRI shapefile"){
				tkconfigure(cb.grddem, state = 'disabled')
				tkconfigure(bt.grddem, state = 'disabled')
				tkconfigure(cb.blkshp, state = 'normal')
				tkconfigure(bt.blkshp, state = 'normal')
			}
		})

		############################################
		
		frSavedir <- tkframe(frTab5, relief = 'sunken', borderwidth = 2, padx = 3, pady = 3)

		file.save1 <- tclVar(str_trim(.cdtData$GalParams$output$dir))

		txt.file.save <- tklabel(frSavedir, text = 'Directory to save result', anchor = 'w', justify = 'left')
		en.file.save <- tkentry(frSavedir, textvariable = file.save1, width = largeur0)
		bt.file.save <- tkbutton(frSavedir, text = "...")

		tkconfigure(bt.file.save, command = function(){
			file2save1 <- tk_choose.dir(str_trim(.cdtData$GalParams$output$dir), "")
			if(is.na(file2save1)) tclvalue(file.save1) <- str_trim(.cdtData$GalParams$output$dir)
			else{
				dir.create(file2save1, showWarnings = FALSE, recursive = TRUE)
				tclvalue(file.save1) <- file2save1
			}
		})

		tkgrid(txt.file.save, row = 0, column = 0, sticky = 'we', rowspan = 1, columnspan = 2, padx = 1, pady = 1, ipadx = 1, ipady = 1)
		tkgrid(en.file.save, row = 1, column = 0, sticky = 'we', rowspan = 1, columnspan = 1, padx = 0, pady = 1, ipadx = 1, ipady = 1)
		tkgrid(bt.file.save, row = 1, column = 1, sticky = 'w', rowspan = 1, columnspan = 1, padx = 0, pady = 1, ipadx = 1, ipady = 1)

		infobulle(en.file.save, 'Enter the full path to directory to save result')
		status.bar.display(en.file.save, 'Enter the full path to directory to save result')
		infobulle(bt.file.save, 'or browse here')

		############################################
		tkgrid(frBlank, row = 0, column = 0, sticky = 'we', padx = 1, pady = 1, ipadx = 1, ipady = 1)
		tkgrid(frSavedir, row = 1, column = 0, sticky = 'we', padx = 1, pady = 1, ipadx = 1, ipady = 1)

		############################################

		tkgrid(frTab5, padx = 0, pady = 1, ipadx = 1, ipady = 1)

	############################################
	
	bt.prm.OK <- ttkbutton(frMRG1, text = .cdtEnv$tcl$lang$global[['button']][['1']])
	bt.prm.CA <- ttkbutton(frMRG1, text = .cdtEnv$tcl$lang$global[['button']][['2']])

	tkconfigure(bt.prm.OK, command = function(){
		if(str_trim(tclvalue(file.stnfl)) == "" & str_trim(tclvalue(no.stnfl)) == "0"){
			tkmessageBox(message = "Choose the file containing the gauge data", icon = "warning", type = "ok")
		}else if(str_trim(tclvalue(file.grdrfe)) == "" & str_trim(tclvalue(down.rfe)) == "1"){
			tkmessageBox(message = "Choose RFE file", icon = "warning", type = "ok")
			tkwait.window(tt)
		}else if(str_trim(tclvalue(dir.bias)) %in% c("", "NA") & tclvalue(adj.bias) == "1"){
			tkmessageBox(message = "Choose or enter the directory containing the Mean Bias files", icon = "warning", type = "ok")
			tkwait.window(tt)
		}else if((tclvalue(mrg.method) == "Spatio-Temporal LM") & (tclvalue(no.stnfl) == "0") &
			str_trim(tclvalue(dir.LMCoef)) %in% c("", "NA")){
			tkmessageBox(message = "Enter the path to directory containing the LM Coefficients", icon = "warning", type = "ok")
			tkwait.window(tt)
		}else if(str_trim(tclvalue(blankGrd)) == "Use DEM" & str_trim(tclvalue(file.grddem)) == ""){
			tkmessageBox(message = "You have to provide DEM data in NetCDF format", icon = "warning", type = "ok")
			tkwait.window(tt)
		}else if(str_trim(tclvalue(blankGrd)) == "Use ESRI shapefile" & str_trim(tclvalue(file.blkshp)) == ""){
			tkmessageBox(message = "You have to provide the shapefile", icon = "warning", type = "ok")
			tkwait.window(tt)
		}else if(str_trim(tclvalue(file.save1)) %in% c("", "NA")){
			tkmessageBox(message = "Choose or enter the path to directory to save results", icon = "warning", type = "ok")
			tkwait.window(tt)
		}else{
			.cdtData$GalParams$Merging.Date$year <- str_trim(tclvalue(istart.yrs))
			.cdtData$GalParams$Merging.Date$month <- str_trim(tclvalue(istart.mon))
			.cdtData$GalParams$Merging.Date$dekad <- str_trim(tclvalue(istart.day))

			.cdtData$GalParams$STN$file <- str_trim(tclvalue(file.stnfl))
			.cdtData$GalParams$STN$No.Stn.Data <- switch(str_trim(tclvalue(no.stnfl)), '0' = FALSE, '1' = TRUE)

			.cdtData$GalParams$RFE$downloaded <- switch(str_trim(tclvalue(down.rfe)), '0' = FALSE, '1' = TRUE)
			.cdtData$GalParams$RFE$file <- str_trim(tclvalue(file.grdrfe))
			.cdtData$GalParams$RFE$source <- str_trim(tclvalue(data.rfe))

			.cdtData$GalParams$bbox$minlon <- as.numeric(str_trim(tclvalue(minLon)))
			.cdtData$GalParams$bbox$maxlon <- as.numeric(str_trim(tclvalue(maxLon)))
			.cdtData$GalParams$bbox$minlat <- as.numeric(str_trim(tclvalue(minLat)))
			.cdtData$GalParams$bbox$maxlat <- as.numeric(str_trim(tclvalue(maxLat)))

			.cdtData$GalParams$BIAS$Adjust <- switch(str_trim(tclvalue(adj.bias)), '0' = FALSE, '1' = TRUE)
			.cdtData$GalParams$BIAS$Dir <- str_trim(tclvalue(dir.bias))
			.cdtData$GalParams$BIAS$method <- str_trim(tclvalue(bias.method))

			.cdtData$GalParams$Merging$mrg.method <- str_trim(tclvalue(mrg.method))
			.cdtData$GalParams$Merging$min.stn <- as.numeric(str_trim(tclvalue(mrg.min.stn)))
			.cdtData$GalParams$LMCOEF$dir.LMCoef <- str_trim(tclvalue(dir.LMCoef))

			.cdtData$GalParams$RnoR$use.RnoR <- switch(str_trim(tclvalue(use.RnoR)), '0' = FALSE, '1' = TRUE)
			.cdtData$GalParams$RnoR$smooth.RnoR <- switch(str_trim(tclvalue(smooth.RnoR)), '0' = FALSE, '1' = TRUE)

			.cdtData$GalParams$blank$blank <- switch(str_trim(tclvalue(blankGrd)),
														"None" = '1', "Use DEM" = '2',
														"Use ESRI shapefile" = '3')
			.cdtData$GalParams$blank$DEM.file <- str_trim(tclvalue(file.grddem))
			.cdtData$GalParams$blank$SHP.file <- str_trim(tclvalue(file.blkshp))

			.cdtData$GalParams$output$dir <- str_trim(tclvalue(file.save1))

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

	tcl('update')
	tkgrid(bwnote, sticky = 'nwes')
	tkgrid.columnconfigure(bwnote, 0, weight = 1)

	############################################

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
