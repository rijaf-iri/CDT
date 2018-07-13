
computeTvars_getParams <- function(){
	listOpenFiles <- openFile_ttkcomboList()
	if(WindowsOS()){
		largeur0 <- 12
		largeur1 <- 11
		largeur2 <- 47
		largeur3 <- 44
	}else{
		largeur0 <- 11
		largeur1 <- 10
		largeur2 <- 35
		largeur3 <- 34
	}

	# xml.dlg <- file.path(.cdtDir$dirLocal, "languages", "cdtCompute_Tvars_dlgBox.xml")
	# lang.dlg <- cdtLanguageParse(xml.dlg, .cdtData$Config$lang.iso)

	############################################

	tt <- tktoplevel()
	tkgrab.set(tt)
	tkfocus(tt)

	frMRG0 <- tkframe(tt, relief = 'raised', borderwidth = 2)
	frMRG1 <- tkframe(tt)
	frMain <- tkframe(frMRG0, relief = "groove", borderwidth = 2)

	############################################

	frtimestep <- tkframe(frMain, relief = 'sunken', borderwidth = 2)

	timeSteps <- tclVar()
	cb.periodVAL <- c('Daily data', 'Pentad data', 'Dekadal data', 'Monthly data')
	tclvalue(timeSteps) <- switch(.cdtData$GalParams$Tstep,
									'daily' = cb.periodVAL[1],
									'pentad' = cb.periodVAL[2],
									'dekadal' = cb.periodVAL[3],
									'monthly' = cb.periodVAL[4])
	cb.varsVAL <- c('Mean', 'Range')
	temp.variable <- tclVar(.cdtData$GalParams$variable)

	txt.period <- tklabel(frtimestep, text = 'Time step', anchor = 'e', justify = 'right')
	cb.period <- ttkcombobox(frtimestep, values = cb.periodVAL, textvariable = timeSteps, width = largeur0)
	txt.variable <- tklabel(frtimestep, text = 'Variable', anchor = 'e', justify = 'right')
	cb.variable <- ttkcombobox(frtimestep, values = cb.varsVAL, textvariable = temp.variable, width = largeur1)

	tkgrid(txt.period, row = 0, column = 0, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 5, ipadx = 1, ipady = 1)
	tkgrid(cb.period, row = 0, column = 1, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 5, ipadx = 1, ipady = 1)
	tkgrid(txt.variable, row = 0, column = 2, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 5, ipadx = 1, ipady = 1)
	tkgrid(cb.variable, row = 0, column = 3, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 5, ipadx = 1, ipady = 1)

	infobulle(cb.period, 'Select the time step of the input data')
	status.bar.display(cb.period, 'Select the time step of the input data')
	infobulle(cb.variable, 'Select the variable to calculate')
	status.bar.display(cb.variable, 'Select the variable to calculate')

	############################################

	frdatatype <- tkframe(frMain, relief = 'sunken', borderwidth = 2)

	DataType <- tclVar()
	CbdatatypeVAL <- c('CDT stations data format', 'CDT dataset format (gridded)', 'NetCDF gridded data')
	tclvalue(DataType) <- switch(.cdtData$GalParams$data.type,
								'cdtstation' = CbdatatypeVAL[1],
								'cdtdataset' = CbdatatypeVAL[2],
								'cdtnetcdf' = CbdatatypeVAL[3])

	txt.datatyp <- tklabel(frdatatype, text = 'Format of input data', anchor = 'w', justify = 'left')
	cb.datatyp <- ttkcombobox(frdatatype, values = CbdatatypeVAL, textvariable = DataType, width = largeur3)

	tkgrid(txt.datatyp, row = 0, column = 0, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
	tkgrid(cb.datatyp, row = 1, column = 0, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)

	infobulle(cb.datatyp, 'Select the format of the input data')
	status.bar.display(cb.datatyp, 'Select the format of the input data')

	###############

	tkbind(cb.datatyp, "<<ComboboxSelected>>", function(){
		tkdestroy(cb.en.tmin)
		tclvalue(input.Tmin) <- ''

		tkdestroy(cb.en.tmax)
		tclvalue(input.Tmax) <- ''

		###
		stateSetNC <- if(str_trim(tclvalue(DataType)) == 'NetCDF gridded data') "normal" else "disabled"
		tkconfigure(set.tmin, state = stateSetNC)
		tkconfigure(set.tmax, state = stateSetNC)

		###
		if(str_trim(tclvalue(DataType)) == 'CDT stations data format'){
			tclvalue(txt.INTmin.var) <- 'File containing stations Tmin data'
			tclvalue(txt.INTmax.var) <- 'File containing stations Tmax data'

			cb.en.tmin <- ttkcombobox(frameInData, values = unlist(listOpenFiles), textvariable = input.Tmin, width = largeur3)
			cb.en.tmax <- ttkcombobox(frameInData, values = unlist(listOpenFiles), textvariable = input.Tmax, width = largeur3)

			######
			tkconfigure(bt.tmin, command = function(){
				dat.opfiles <- getOpenFiles(tt)
				if(!is.null(dat.opfiles)){
					update.OpenFiles('ascii', dat.opfiles)
					listOpenFiles[[length(listOpenFiles) + 1]] <<- dat.opfiles[[1]]
					tclvalue(input.Tmin) <- dat.opfiles[[1]]
					lapply(list(cb.en.tmin, cb.en.tmax), tkconfigure, values = unlist(listOpenFiles))
				}
			})

			tkconfigure(bt.tmax, command = function(){
				dat.opfiles <- getOpenFiles(tt)
				if(!is.null(dat.opfiles)){
					update.OpenFiles('ascii', dat.opfiles)
					listOpenFiles[[length(listOpenFiles) + 1]] <<- dat.opfiles[[1]]
					tclvalue(input.Tmax) <- dat.opfiles[[1]]
					lapply(list(cb.en.tmin, cb.en.tmax), tkconfigure, values = unlist(listOpenFiles))
				}
			})

			######
			infobulle(cb.en.tmin, 'Select the file containing the minimum temperature')
			status.bar.display(cb.en.tmin, 'Select the file containing the minimum temperature')
			infobulle(cb.en.tmax, 'Select the file containing the maximum temperature')
			status.bar.display(cb.en.tmax, 'Select the file containing the maximum temperature')
			infobulle(bt.tmin, 'Browse file if not listed')
			status.bar.display(bt.tmin, 'Browse file if not listed')
			infobulle(bt.tmax, 'Browse file if not listed')
			status.bar.display(bt.tmax, 'Browse file if not listed')
		}

		###
		if(str_trim(tclvalue(DataType)) == 'CDT dataset format (gridded)'){
			tclvalue(txt.INTmin.var) <- 'Index file (*.rds) for Tmin data'
			tclvalue(txt.INTmax.var) <- 'Index file (*.rds) for Tmax data'

			cb.en.tmin <- tkentry(frameInData, textvariable = input.Tmin, width = largeur2)
			cb.en.tmax <- tkentry(frameInData, textvariable = input.Tmax, width = largeur2)

			######
			tkconfigure(bt.tmin, command = function(){
				path.rds <- tclvalue(tkgetOpenFile(initialdir = getwd(), filetypes = .cdtEnv$tcl$data$filetypes6))
				tclvalue(input.Tmin) <- if(path.rds %in% c("", "NA")) "" else path.rds
			})

			tkconfigure(bt.tmax, command = function(){
				path.rds <- tclvalue(tkgetOpenFile(initialdir = getwd(), filetypes = .cdtEnv$tcl$data$filetypes6))
				tclvalue(input.Tmax) <- if(path.rds %in% c("", "NA")) "" else path.rds
			})

			######
			infobulle(cb.en.tmin, 'Enter the full path to the file <minimum temperature dataset name>.rds')
			status.bar.display(cb.en.tmin, 'Enter the full path to the file <minimum temperature dataset name>.rds')
			infobulle(cb.en.tmax, 'Enter the full path to the file <maximum temperature dataset name>.rds')
			status.bar.display(cb.en.tmax, 'Enter the full path to the file <maximum temperature dataset name>.rds')
			infobulle(bt.tmin, 'or browse here')
			status.bar.display(bt.tmin, 'or browse here')
			infobulle(bt.tmax, 'or browse here')
			status.bar.display(bt.tmax, 'or browse here')
		}

		###
		if(str_trim(tclvalue(DataType)) == 'NetCDF gridded data'){
			tclvalue(txt.INTmin.var) <- 'Directory of the Tmin NetCDF files'
			tclvalue(txt.INTmax.var) <- 'Directory of the Tmax NetCDF files'

			cb.en.tmin <- tkentry(frameInData, textvariable = input.Tmin, width = largeur2)
			cb.en.tmax <- tkentry(frameInData, textvariable = input.Tmax, width = largeur2)

			######
			tkconfigure(set.tmin, command = function(){
				.cdtData$GalParams$cdtnetcdf[["tmin"]] <- getInfoNetcdfData(tt, .cdtData$GalParams$cdtnetcdf[["tmin"]],
																str_trim(tclvalue(input.Tmin)), tclvalue(timeSteps))
				settingTmin <<- 1
			})

			tkconfigure(set.tmax, command = function(){
				.cdtData$GalParams$cdtnetcdf[["tmax"]] <- getInfoNetcdfData(tt, .cdtData$GalParams$cdtnetcdf[["tmax"]],
																str_trim(tclvalue(input.Tmax)), tclvalue(timeSteps))
				settingTmax <<- 1
			})

			tkconfigure(bt.tmin, command = function(){
				dirnc <- tk_choose.dir(getwd(), "")
				tclvalue(input.Tmin) <- if(dirnc %in% c("", "NA")) "" else dirnc
			})

			tkconfigure(bt.tmax, command = function(){
				dirnc <- tk_choose.dir(getwd(), "")
				tclvalue(input.Tmax) <- if(dirnc %in% c("", "NA")) "" else dirnc
			})

			######
			infobulle(cb.en.tmin, 'Enter the full path to directory containing the minimum temperature files')
			status.bar.display(cb.en.tmin, 'Enter the full path to directory containing the minimum temperature files')
			infobulle(cb.en.tmax, 'Enter the full path to directory containing the maximum temperature files')
			status.bar.display(cb.en.tmax, 'Enter the full path to directory containing the maximum temperature files')
			infobulle(bt.tmin, 'or browse here')
			status.bar.display(bt.tmin, 'or browse here')
			infobulle(bt.tmax, 'or browse here')
			status.bar.display(bt.tmax, 'or browse here')
		}

		#######
		if(str_trim(tclvalue(DataType)) == 'CDT stations data format'){
			txtSaveHelp <- 'Enter the full path of the file to save the result'
			tclvalue(fileORdir) <- 'File to save the result'
			isFile <- TRUE
		}else{
			tclvalue(fileORdir) <- 'Directory to save the result'
			txtSaveHelp <- 'Enter the full path to the directory to save the result'
			isFile <- FALSE
		}

		tkconfigure(bt.file.save, command = function() fileORdir2Save(file.save, isFile = isFile))

		infobulle(en.file.save, txtSaveHelp)
		status.bar.display(en.file.save, txtSaveHelp)
		infobulle(bt.file.save, 'or browse here')
		status.bar.display(bt.file.save, 'or browse here')

		#######
		tkgrid(cb.en.tmin, row = 1, column = 0, sticky = 'we', rowspan = 1, columnspan = 4, padx = 0, pady = 0, ipadx = 1, ipady = 1)
		tkgrid(cb.en.tmax, row = 3, column = 0, sticky = 'we', rowspan = 1, columnspan = 4, padx = 0, pady = 0, ipadx = 1, ipady = 1)
		tkfocus(tt)
	})

	############################################

	frameInData <- tkframe(frMain, relief = 'sunken', borderwidth = 2)

	if(.cdtData$GalParams$data.type == 'cdtstation'){
		input.Tmin <- tclVar(.cdtData$GalParams$cdtstation$tmin)
		input.Tmax <- tclVar(.cdtData$GalParams$cdtstation$tmax)
		txt.INTmin <- 'File containing stations Tmin data'
		txt.INTmax <- 'File containing stations Tmax data'
		stateSetNC <- "disabled"
	}else if(.cdtData$GalParams$data.type == 'cdtdataset'){
		input.Tmin <- tclVar(.cdtData$GalParams$cdtdataset$tmin)
		input.Tmax <- tclVar(.cdtData$GalParams$cdtdataset$tmax)
		txt.INTmin <- 'Index file (*.rds) for Tmin data'
		txt.INTmax <- 'Index file (*.rds) for Tmax data'
		stateSetNC <- "disabled"
	}else{
		input.Tmin <- tclVar(.cdtData$GalParams$cdtnetcdf$tmin$dir)
		input.Tmax <- tclVar(.cdtData$GalParams$cdtnetcdf$tmax$dir)
		txt.INTmin <- 'Directory of the Tmin NetCDF files'
		txt.INTmax <- 'Directory of the Tmax NetCDF files'
		stateSetNC <- "normal"
	}
	txt.INTmin.var <- tclVar(txt.INTmin)
	txt.INTmax.var <- tclVar(txt.INTmax)

	##############
	txt.tmin <- tklabel(frameInData, text = tclvalue(txt.INTmin.var), textvariable = txt.INTmin.var, anchor = 'w', justify = 'left')
	set.tmin <- tkbutton(frameInData, text = "Settings", state = stateSetNC)

	if(.cdtData$GalParams$data.type == 'cdtstation'){
		cb.en.tmin <- ttkcombobox(frameInData, values = unlist(listOpenFiles), textvariable = input.Tmin, width = largeur3)
	}else{
		cb.en.tmin <- tkentry(frameInData, textvariable = input.Tmin, width = largeur2)
	}
	bt.tmin <- tkbutton(frameInData, text = "...")

	############
	settingTmin <- .cdtData$GalParams$settingTmin
	tkconfigure(set.tmin, command = function(){
		.cdtData$GalParams$cdtnetcdf[["tmin"]] <- getInfoNetcdfData(tt, .cdtData$GalParams$cdtnetcdf[["tmin"]],
														str_trim(tclvalue(input.Tmin)), tclvalue(timeSteps))
		settingTmin <<- 1
	})

	tkconfigure(bt.tmin, command = function(){
		if(.cdtData$GalParams$data.type == 'cdtstation'){
			dat.opfiles <- getOpenFiles(tt)
			if(!is.null(dat.opfiles)){
				update.OpenFiles('ascii', dat.opfiles)
				listOpenFiles[[length(listOpenFiles) + 1]] <<- dat.opfiles[[1]]
				tclvalue(input.Tmin) <- dat.opfiles[[1]]
				lapply(list(cb.en.tmin, cb.en.tmax), tkconfigure, values = unlist(listOpenFiles))
			}
		}else if(.cdtData$GalParams$data.type == 'cdtdataset'){
			path.rds <- tclvalue(tkgetOpenFile(initialdir = getwd(), filetypes = .cdtEnv$tcl$data$filetypes6))
			tclvalue(input.Tmin) <- if(path.rds %in% c("", "NA") | is.na(path.rds)) "" else path.rds
		}else{
			dirnc <- tk_choose.dir(getwd(), "")
			tclvalue(input.Tmin) <- if(dirnc %in% c("", "NA") | is.na(dirnc)) "" else dirnc
		}
	})

	##############
	txt.tmax <- tklabel(frameInData, text = tclvalue(txt.INTmax.var), textvariable = txt.INTmax.var, anchor = 'w', justify = 'left')
	set.tmax <- tkbutton(frameInData, text = "Settings", state = stateSetNC)

	if(.cdtData$GalParams$data.type == 'cdtstation'){
		cb.en.tmax <- ttkcombobox(frameInData, values = unlist(listOpenFiles), textvariable = input.Tmax, width = largeur3)
	}else{
		cb.en.tmax <- tkentry(frameInData, textvariable = input.Tmax, width = largeur2)
	}
	bt.tmax <- tkbutton(frameInData, text = "...")

	############
	settingTmax <- .cdtData$GalParams$settingTmax
	tkconfigure(set.tmax, command = function(){
		.cdtData$GalParams$cdtnetcdf[["tmax"]] <- getInfoNetcdfData(tt, .cdtData$GalParams$cdtnetcdf[["tmax"]],
														str_trim(tclvalue(input.Tmax)), tclvalue(timeSteps))
		settingTmax <<- 1
	})

	tkconfigure(bt.tmax, command = function(){
		if(.cdtData$GalParams$data.type == 'cdtstation'){
			dat.opfiles <- getOpenFiles(tt)
			if(!is.null(dat.opfiles)){
				update.OpenFiles('ascii', dat.opfiles)
				listOpenFiles[[length(listOpenFiles) + 1]] <<- dat.opfiles[[1]]
				tclvalue(input.Tmax) <- dat.opfiles[[1]]
				lapply(list(cb.en.tmin, cb.en.tmax), tkconfigure, values = unlist(listOpenFiles))
			}
		}else if(.cdtData$GalParams$data.type == 'cdtdataset'){
			path.rds <- tclvalue(tkgetOpenFile(initialdir = getwd(), filetypes = .cdtEnv$tcl$data$filetypes6))
			tclvalue(input.Tmax) <- if(path.rds %in% c("", "NA") | is.na(path.rds)) "" else path.rds
		}else{
			dirnc <- tk_choose.dir(getwd(), "")
			tclvalue(input.Tmax) <- if(dirnc %in% c("", "NA") | is.na(dirnc)) "" else dirnc
		}
	})

	############ 
	tkgrid(txt.tmin, row = 0, column = 0, sticky = 'we', rowspan = 1, columnspan = 3, padx = 1, pady = 0, ipadx = 1, ipady = 1)
	tkgrid(set.tmin, row = 0, column = 3, sticky = 'we', rowspan = 1, columnspan = 2, padx = 1, pady = 0, ipadx = 1, ipady = 1)
	tkgrid(cb.en.tmin, row = 1, column = 0, sticky = 'we', rowspan = 1, columnspan = 4, padx = 0, pady = 0, ipadx = 1, ipady = 1)
	tkgrid(bt.tmin, row = 1, column = 4, sticky = 'w', rowspan = 1, columnspan = 1, padx = 0, pady = 0, ipadx = 1, ipady = 1)

	tkgrid(txt.tmax, row = 2, column = 0, sticky = 'we', rowspan = 1, columnspan = 3, padx = 1, pady = 0, ipadx = 1, ipady = 1)
	tkgrid(set.tmax, row = 2, column = 3, sticky = 'we', rowspan = 1, columnspan = 2, padx = 1, pady = 0, ipadx = 1, ipady = 1)
	tkgrid(cb.en.tmax, row = 3, column = 0, sticky = 'we', rowspan = 1, columnspan = 4, padx = 0, pady = 0, ipadx = 1, ipady = 1)
	tkgrid(bt.tmax, row = 3, column = 4, sticky = 'w', rowspan = 1, columnspan = 1, padx = 0, pady = 0, ipadx = 1, ipady = 1)

	#############
	if(.cdtData$GalParams$data.type == 'cdtstation'){
		infobulle(cb.en.tmin, 'Select the file containing the minimum temperature')
		status.bar.display(cb.en.tmin, 'Select the file containing the minimum temperature')
		infobulle(cb.en.tmax, 'Select the file containing the maximum temperature')
		status.bar.display(cb.en.tmax, 'Select the file containing the maximum temperature')
		infobulle(bt.tmin, 'Browse file if not listed')
		status.bar.display(bt.tmin, 'Browse file if not listed')
		infobulle(bt.tmax, 'Browse file if not listed')
		status.bar.display(bt.tmax, 'Browse file if not listed')
	}else if(.cdtData$GalParams$data.type == 'cdtdataset'){
		infobulle(cb.en.tmin, 'Enter the full path to the file <minimum temperature dataset name>.rds')
		status.bar.display(cb.en.tmin, 'Enter the full path to the file <minimum temperature dataset name>.rds')
		infobulle(cb.en.tmax, 'Enter the full path to the file <maximum temperature dataset name>.rds')
		status.bar.display(cb.en.tmax, 'Enter the full path to the file <maximum temperature dataset name>.rds')
		infobulle(bt.tmin, 'or browse here')
		status.bar.display(bt.tmin, 'or browse here')
		infobulle(bt.tmax, 'or browse here')
		status.bar.display(bt.tmax, 'or browse here')
	}else{
		infobulle(cb.en.tmin, 'Enter the full path to directory containing the minimum temperature files')
		status.bar.display(cb.en.tmin, 'Enter the full path to directory containing the minimum temperature files')
		infobulle(cb.en.tmax, 'Enter the full path to directory containing the maximum temperature files')
		status.bar.display(cb.en.tmax, 'Enter the full path to directory containing the maximum temperature files')
		infobulle(bt.tmin, 'or browse here')
		status.bar.display(bt.tmin, 'or browse here')
		infobulle(bt.tmax, 'or browse here')
		status.bar.display(bt.tmax, 'or browse here')
	}

	############################################

	frSave <- tkframe(frMain, relief = 'sunken', borderwidth = 2)

	file.save <- tclVar(.cdtData$GalParams$output)

	if(.cdtData$GalParams$data.type == 'cdtstation'){
		txtSaveDir <- 'File to save the output'
		isFile <- TRUE
	}else{
		txtSaveDir <- 'Directory to save the output'
		isFile <- FALSE
	}
	fileORdir <- tclVar(txtSaveDir)

	txt.file.save <- tklabel(frSave, text = tclvalue(fileORdir), textvariable = fileORdir, anchor = 'w', justify = 'left')
	en.file.save <- tkentry(frSave, textvariable = file.save, width = largeur2)
	bt.file.save <- tkbutton(frSave, text = "...")

	#########
	tkconfigure(bt.file.save, command = function() fileORdir2Save(file.save, isFile = isFile))

	#########
	tkgrid(txt.file.save, row = 0, column = 0, sticky = 'we', rowspan = 1, columnspan = 5, padx = 1, pady = 0, ipadx = 1, ipady = 1)
	tkgrid(en.file.save, row = 1, column = 0, sticky = 'we', rowspan = 1, columnspan = 4, padx = 0, pady = 0, ipadx = 1, ipady = 1)
	tkgrid(bt.file.save, row = 1, column = 4, sticky = 'w', rowspan = 1, columnspan = 1, padx = 0, pady = 0, ipadx = 1, ipady = 1)

	#########
	if(.cdtData$GalParams$data.type == 'cdtstation'){
		txtSaveHelp <- 'Enter the full path of the file to save the result'
	}else{
		txtSaveHelp <- 'Directory to save the result'
	}

	infobulle(en.file.save, txtSaveHelp)
	status.bar.display(en.file.save, txtSaveHelp)
	infobulle(bt.file.save, 'or browse here')
	status.bar.display(bt.file.save, 'or browse here')

	############################################
	tkgrid(frtimestep, row = 0, column = 0, sticky = 'we', padx = 1, pady = 1, ipadx = 1, ipady = 1)
	tkgrid(frdatatype, row = 1, column = 0, sticky = '', padx = 1, pady = 3, ipadx = 1, ipady = 1)
	tkgrid(frameInData, row = 2, column = 0, sticky = 'we', padx = 1, pady = 3, ipadx = 1, ipady = 1)
	tkgrid(frSave, row = 3, column = 0, sticky = 'we', padx = 1, pady = 3, ipadx = 1, ipady = 1)

	############################################
	
	tkgrid(frMain, row = 0, column = 0, sticky = 'news', padx = 5, pady = 1, ipadx = 1, ipady = 1)

	############################################

	bt.prm.OK <- ttkbutton(frMRG1, text = .cdtEnv$tcl$lang$global[['button']][['1']])
	bt.prm.CA <- ttkbutton(frMRG1, text = .cdtEnv$tcl$lang$global[['button']][['2']])

	tkconfigure(bt.prm.OK, command = function(){
		if(str_trim(tclvalue(input.Tmin)) %in% c("", "NA")){
			tkmessageBox(message = "No input for minimum temperature", icon = "warning", type = "ok")
			tkwait.window(tt)
		}else if(str_trim(tclvalue(input.Tmax)) %in% c("", "NA")){
			tkmessageBox(message = "No input for maximum temperature", icon = "warning", type = "ok")
			tkwait.window(tt)
		}else if(tclvalue(file.save) %in% c("", "NA")){
			tkmessageBox(message = "Choose a directory or enter the file to save results", icon = "warning", type = "ok")
			tkwait.window(tt)
		}else if(str_trim(tclvalue(DataType)) == 'NetCDF gridded data' & is.null(settingTmin)){
				tkmessageBox(message = "You have to set the Tmin files parameters", icon = "warning", type = "ok")
				tkwait.window(tt)
		}else if(str_trim(tclvalue(DataType)) == 'NetCDF gridded data' & is.null(settingTmax)){
				tkmessageBox(message = "You have to set the Tmax files parameters", icon = "warning", type = "ok")
				tkwait.window(tt)
		}else{
			.cdtData$GalParams$Tstep <- switch(str_trim(tclvalue(timeSteps)), 
												'Daily data' = 'daily',
												'Pentad data' = 'pentad',
												'Dekadal data' = 'dekadal',
												'Monthly data' = 'monthly')
			.cdtData$GalParams$variable <- str_trim(tclvalue(temp.variable))

			.cdtData$GalParams$data.type <- switch(str_trim(tclvalue(DataType)),
												'CDT stations data format' = 'cdtstation',
												'CDT dataset format (gridded)' = 'cdtdataset',
												'NetCDF gridded data' = 'cdtnetcdf')

			if(str_trim(tclvalue(DataType)) == 'CDT stations data format'){
				.cdtData$GalParams$cdtstation$tmin <- str_trim(tclvalue(input.Tmin))
				.cdtData$GalParams$cdtstation$tmax <- str_trim(tclvalue(input.Tmax))
			}

			if(str_trim(tclvalue(DataType)) == 'CDT dataset format (gridded)'){
				.cdtData$GalParams$cdtdataset$tmin <- str_trim(tclvalue(input.Tmin))
				.cdtData$GalParams$cdtdataset$tmax <- str_trim(tclvalue(input.Tmax))
			}

			if(str_trim(tclvalue(DataType)) == 'NetCDF gridded data'){
				.cdtData$GalParams$cdtnetcdf$tmin$dir <- str_trim(tclvalue(input.Tmin))
				.cdtData$GalParams$cdtnetcdf$tmax$dir <- str_trim(tclvalue(input.Tmax))
			}

			.cdtData$GalParams$output <- str_trim(tclvalue(file.save))

			.cdtData$GalParams$settingTmin <- settingTmin
			.cdtData$GalParams$settingTmax <- settingTmax

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
	tkwm.title(tt, 'Compute Temperature Variables')
	tkwm.deiconify(tt)

	tkfocus(tt)
	tkbind(tt, "<Destroy>", function(){
		tkgrab.release(tt)
		tkfocus(.cdtEnv$tcl$main$win)
	})
	tkwait.window(tt)
}
