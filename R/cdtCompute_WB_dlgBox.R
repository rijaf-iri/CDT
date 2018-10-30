
computeWB_getParams <- function(){
	listOpenFiles <- openFile_ttkcomboList()
	if(WindowsOS()){
		largeur1 <- 45
		largeur2 <- 55
		largeur3 <- 52
	}else{
		largeur1 <- 30
		largeur2 <- 41
		largeur3 <- 40
	}

	MOIS <- format(ISOdate(2014, 1:12, 1), "%B")

	# xml.dlg <- file.path(.cdtDir$dirLocal, "languages", "cdtCompute_WB_dlgBox.xml")
	# lang.dlg <- cdtLanguageParse(xml.dlg, .cdtData$Config$lang.iso)

	############################################

	tt <- tktoplevel()
	tkgrab.set(tt)
	tkfocus(tt)

	frMRG0 <- tkframe(tt, relief = 'raised', borderwidth = 2)
	frMRG1 <- tkframe(tt)
	frLeft <- tkframe(frMRG0, relief = "groove", borderwidth = 2)

	############################################

	frdatatype <- tkframe(frLeft, relief = 'sunken', borderwidth = 2)

	DataType <- tclVar()
	CbdatatypeVAL <- .cdtEnv$tcl$lang$global[['combobox']][['2']][1:2]
	datatypeVAL <- c('cdtstation', 'cdtdataset')
	tclvalue(DataType) <- CbdatatypeVAL[datatypeVAL %in% .cdtData$GalParams$data.type]

	txt.datatyp <- tklabel(frdatatype, text = 'Format of input data', anchor = 'w', justify = 'left')
	cb.datatyp <- ttkcombobox(frdatatype, values = CbdatatypeVAL, textvariable = DataType, width = largeur1)

	tkgrid(txt.datatyp, row = 0, column = 0, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
	tkgrid(cb.datatyp, row = 1, column = 0, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)

	infobulle(cb.datatyp, 'Select the format of the input data')
	status.bar.display(cb.datatyp, 'Select the format of the input data')

	###############

	tkbind(cb.datatyp, "<<ComboboxSelected>>", function(){
		tkdestroy(cb.en.etp)
		tclvalue(input.Etp) <- ''

		tkdestroy(cb.en.prec)
		tclvalue(input.Prec) <- ''

		###
		if(str_trim(tclvalue(DataType)) == CbdatatypeVAL[1]){
			tclvalue(txt.INEtp.var) <- 'File containing stations daily PET data'
			tclvalue(txt.INPrec.var) <- 'File containing stations daily Precip data'

			cb.en.etp <- ttkcombobox(frameInData, values = unlist(listOpenFiles), textvariable = input.Etp, width = largeur3)
			cb.en.prec <- ttkcombobox(frameInData, values = unlist(listOpenFiles), textvariable = input.Prec, width = largeur3)

			######
			tkconfigure(bt.etp, command = function(){
				dat.opfiles <- getOpenFiles(tt)
				if(!is.null(dat.opfiles)){
					update.OpenFiles('ascii', dat.opfiles)
					listOpenFiles[[length(listOpenFiles) + 1]] <<- dat.opfiles[[1]]
					tclvalue(input.Etp) <- dat.opfiles[[1]]
					lapply(list(cb.en.etp, cb.en.prec), tkconfigure, values = unlist(listOpenFiles))
				}
			})

			tkconfigure(bt.prec, command = function(){
				dat.opfiles <- getOpenFiles(tt)
				if(!is.null(dat.opfiles)){
					update.OpenFiles('ascii', dat.opfiles)
					listOpenFiles[[length(listOpenFiles) + 1]] <<- dat.opfiles[[1]]
					tclvalue(input.Prec) <- dat.opfiles[[1]]
					lapply(list(cb.en.etp, cb.en.prec), tkconfigure, values = unlist(listOpenFiles))
				}
			})

			######
			infobulle(cb.en.etp, 'Select the file containing the daily pontetial evapotranspiration')
			status.bar.display(cb.en.etp, 'Select the file containing the daily pontetial evapotranspiration')
			infobulle(cb.en.prec, 'Select the file containing the daily precipitation')
			status.bar.display(cb.en.prec, 'Select the file containing the daily precipitation')

			infobulle(bt.etp, 'Browse file if not listed')
			status.bar.display(bt.etp, 'Browse file if not listed')
			infobulle(bt.prec, 'Browse file if not listed')
			status.bar.display(bt.prec, 'Browse file if not listed')
		}

		###
		if(str_trim(tclvalue(DataType)) == CbdatatypeVAL[2]){
			tclvalue(txt.INEtp.var) <- 'Index file (*.rds) for daily PET data'
			tclvalue(txt.INPrec.var) <- 'Index file (*.rds) for daily Precip data'

			cb.en.etp <- tkentry(frameInData, textvariable = input.Etp, width = largeur2)
			cb.en.prec <- tkentry(frameInData, textvariable = input.Prec, width = largeur2)

			######
			tkconfigure(bt.etp, command = function(){
				path.rds <- tclvalue(tkgetOpenFile(initialdir = getwd(), filetypes = .cdtEnv$tcl$data$filetypes6))
				tclvalue(input.Etp) <- if(path.rds %in% c("", "NA")) "" else path.rds
			})

			tkconfigure(bt.prec, command = function(){
				path.rds <- tclvalue(tkgetOpenFile(initialdir = getwd(), filetypes = .cdtEnv$tcl$data$filetypes6))
				tclvalue(input.Prec) <- if(path.rds %in% c("", "NA")) "" else path.rds
			})

			######
			infobulle(cb.en.etp, 'Enter the full path to the file <daily pontetial evapotranspiration dataset name>.rds')
			status.bar.display(cb.en.etp, 'Enter the full path to the file <daily pontetial evapotranspiration dataset name>.rds')
			infobulle(cb.en.prec, 'Enter the full path to the file <daily precipitation dataset name>.rds')
			status.bar.display(cb.en.prec, 'Enter the full path to the file <daily precipitation dataset name>.rds')

			infobulle(bt.etp, 'or browse here')
			status.bar.display(bt.etp, 'or browse here')
			infobulle(bt.prec, 'or browse here')
			status.bar.display(bt.prec, 'or browse here')
		}

		#######
		if(str_trim(tclvalue(DataType)) == CbdatatypeVAL[1]){
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
		tkgrid(cb.en.prec, row = 1, column = 0, sticky = 'we', rowspan = 1, columnspan = 4, padx = 0, pady = 0, ipadx = 1, ipady = 1)
		tkgrid(cb.en.etp, row = 3, column = 0, sticky = 'we', rowspan = 1, columnspan = 4, padx = 0, pady = 0, ipadx = 1, ipady = 1)
		tkfocus(tt)
	})

	############################################

	frameInData <- tkframe(frLeft, relief = 'sunken', borderwidth = 2)

	if(.cdtData$GalParams$data.type == 'cdtstation'){
		input.Etp <- tclVar(.cdtData$GalParams$cdtstation$etp)
		input.Prec <- tclVar(.cdtData$GalParams$cdtstation$prec)
		txt.INEtp <- 'File containing stations daily PET data'
		txt.INPrec <- 'File containing stations daily Precip data'
	}else{
		input.Etp <- tclVar(.cdtData$GalParams$cdtdataset$etp)
		input.Prec <- tclVar(.cdtData$GalParams$cdtdataset$prec)
		txt.INEtp <- 'Index file (*.rds) for daily PET data'
		txt.INPrec <- 'Index file (*.rds) for daily Precip data'
	}
	txt.INEtp.var <- tclVar(txt.INEtp)
	txt.INPrec.var <- tclVar(txt.INPrec)


	##############
	txt.prec <- tklabel(frameInData, text = tclvalue(txt.INPrec.var), textvariable = txt.INPrec.var, anchor = 'w', justify = 'left')

	if(.cdtData$GalParams$data.type == 'cdtstation'){
		cb.en.prec <- ttkcombobox(frameInData, values = unlist(listOpenFiles), textvariable = input.Prec, width = largeur3)
	}else{
		cb.en.prec <- tkentry(frameInData, textvariable = input.Prec, width = largeur2)
	}
	bt.prec <- tkbutton(frameInData, text = "...")

	############

	tkconfigure(bt.prec, command = function(){
		if(.cdtData$GalParams$data.type == 'cdtstation'){
			dat.opfiles <- getOpenFiles(tt)
			if(!is.null(dat.opfiles)){
				update.OpenFiles('ascii', dat.opfiles)
				listOpenFiles[[length(listOpenFiles) + 1]] <<- dat.opfiles[[1]]
				tclvalue(input.Prec) <- dat.opfiles[[1]]
				lapply(list(cb.en.etp, cb.en.prec), tkconfigure, values = unlist(listOpenFiles))
			}
		}else{
			path.rds <- tclvalue(tkgetOpenFile(initialdir = getwd(), filetypes = .cdtEnv$tcl$data$filetypes6))
			tclvalue(input.Prec) <- if(path.rds %in% c("", "NA")) "" else path.rds
		}
	})

	##############
	txt.etp <- tklabel(frameInData, text = tclvalue(txt.INEtp.var), textvariable = txt.INEtp.var, anchor = 'w', justify = 'left')

	if(.cdtData$GalParams$data.type == 'cdtstation'){
		cb.en.etp <- ttkcombobox(frameInData, values = unlist(listOpenFiles), textvariable = input.Etp, width = largeur3)
	}else{
		cb.en.etp <- tkentry(frameInData, textvariable = input.Etp, width = largeur2)
	}
	bt.etp <- tkbutton(frameInData, text = "...")

	############

	tkconfigure(bt.etp, command = function(){
		if(.cdtData$GalParams$data.type == 'cdtstation'){
			dat.opfiles <- getOpenFiles(tt)
			if(!is.null(dat.opfiles)){
				update.OpenFiles('ascii', dat.opfiles)
				listOpenFiles[[length(listOpenFiles) + 1]] <<- dat.opfiles[[1]]
				tclvalue(input.Etp) <- dat.opfiles[[1]]
				lapply(list(cb.en.etp, cb.en.prec), tkconfigure, values = unlist(listOpenFiles))
			}
		}else{
			path.rds <- tclvalue(tkgetOpenFile(initialdir = getwd(), filetypes = .cdtEnv$tcl$data$filetypes6))
			tclvalue(input.Etp) <- if(path.rds %in% c("", "NA")) "" else path.rds
		}
	})

	############ 

	tkgrid(txt.prec, row = 0, column = 0, sticky = 'we', rowspan = 1, columnspan = 5, padx = 1, pady = 0, ipadx = 1, ipady = 1)
	tkgrid(cb.en.prec, row = 1, column = 0, sticky = 'we', rowspan = 1, columnspan = 4, padx = 0, pady = 0, ipadx = 1, ipady = 1)
	tkgrid(bt.prec, row = 1, column = 4, sticky = 'w', rowspan = 1, columnspan = 1, padx = 0, pady = 0, ipadx = 1, ipady = 1)

	tkgrid(txt.etp, row = 2, column = 0, sticky = 'we', rowspan = 1, columnspan = 5, padx = 1, pady = 0, ipadx = 1, ipady = 1)
	tkgrid(cb.en.etp, row = 3, column = 0, sticky = 'we', rowspan = 1, columnspan = 4, padx = 0, pady = 0, ipadx = 1, ipady = 1)
	tkgrid(bt.etp, row = 3, column = 4, sticky = 'w', rowspan = 1, columnspan = 1, padx = 0, pady = 0, ipadx = 1, ipady = 1)

	#############
	if(.cdtData$GalParams$data.type == 'cdtstation'){
		infobulle(cb.en.etp, 'Select the file containing the daily pontetial evapotranspiration')
		status.bar.display(cb.en.etp, 'Select the file containing the daily pontetial evapotranspiration')
		infobulle(cb.en.prec, 'Select the file containing the daily precipitation')
		status.bar.display(cb.en.prec, 'Select the file containing the daily precipitation')

		infobulle(bt.etp, 'Browse file if not listed')
		status.bar.display(bt.etp, 'Browse file if not listed')
		infobulle(bt.prec, 'Browse file if not listed')
		status.bar.display(bt.prec, 'Browse file if not listed')
	}else{
		infobulle(cb.en.etp, 'Enter the full path to the file <daily pontetial evapotranspiration dataset name>.rds')
		status.bar.display(cb.en.etp, 'Enter the full path to the file <daily pontetial evapotranspiration dataset name>.rds')
		infobulle(cb.en.prec, 'Enter the full path to the file <daily precipitation dataset name>.rds')
		status.bar.display(cb.en.prec, 'Enter the full path to the file <daily precipitation dataset name>.rds')

		infobulle(bt.etp, 'or browse here')
		status.bar.display(bt.etp, 'or browse here')
		infobulle(bt.prec, 'or browse here')
		status.bar.display(bt.prec, 'or browse here')
	}

	############################################

	frSave <- tkframe(frLeft, relief = 'sunken', borderwidth = 2)

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

	frWBalance <- tkframe(frLeft, relief = 'sunken', borderwidth = 2)

	imon <- as.numeric(str_trim(.cdtData$GalParams$hdate$start.month))
	start.month <- tclVar(MOIS[imon])
	start.day <- tclVar(.cdtData$GalParams$hdate$start.day)
	separate.year <- tclVar(.cdtData$GalParams$hdate$separate.year)
	start.wb <- tclVar(.cdtData$GalParams$wb$wb1)
	capacity.max <- tclVar(.cdtData$GalParams$swhc$cap.max)

	use.multi.wb <- tclVar(.cdtData$GalParams$wb$multi)
	use.multi.swhc <- tclVar(.cdtData$GalParams$swhc$multi)

	stateMWB <- if(.cdtData$GalParams$wb$multi) "normal" else "disabled"
	stateMSWHC <- if(.cdtData$GalParams$swhc$multi) "normal" else "disabled"

	chk.sep.year <- tkcheckbutton(frWBalance, variable = separate.year, text = 'Compute each year separately', anchor = 'w', justify = 'left')

	txt.1stdate0 <- tklabel(frWBalance, text = "Start Water Balance from", anchor = 'e', justify = 'right')
	txt.1stdate1 <- tklabel(frWBalance, text = "Month", anchor = 'e', justify = 'right')
	cb.1stdate1 <- ttkcombobox(frWBalance, values = MOIS, textvariable = start.month, width = 9)
	txt.1stdate2 <- tklabel(frWBalance, text = "Day", anchor = 'e', justify = 'right')
	cb.1stdate2 <- ttkcombobox(frWBalance, values = 1:31, textvariable = start.day, width = 2)

	txt.wb.1stday <- tklabel(frWBalance, text = "First Day Water Balance", anchor = 'w', justify = 'left')
	en.wb.1stday <- tkentry(frWBalance, textvariable = start.wb, width = 4)

	chk.wb.1stday <- tkcheckbutton(frWBalance, variable = use.multi.wb, text = "Multiple WB", anchor = 'w', justify = 'left')
	bt.wb.1stday <- tkbutton(frWBalance, text = "Set", state = stateMWB)

	txt.wb.swhc <- tklabel(frWBalance, text = "Soil Water Holding Capacity", anchor = 'w', justify = 'left')
	en.wb.swhc <- tkentry(frWBalance, textvariable = capacity.max, width = 4)

	chk.wb.swhc <- tkcheckbutton(frWBalance, variable = use.multi.swhc, text = "Multiple SWHC", anchor = 'w', justify = 'left')
	bt.wb.swhc <- tkbutton(frWBalance, text = "Set", state = stateMSWHC)

	###############
	tkconfigure(bt.wb.1stday, command = function(){
		.cdtData$GalParams$wb[["file"]] <- computeWB_get.WB.SWHC(tt, .cdtData$GalParams$wb[["file"]],
																str_trim(tclvalue(DataType)), "WB")
	})

	tkconfigure(bt.wb.swhc, command = function(){
		.cdtData$GalParams$swhc[["file"]] <- computeWB_get.WB.SWHC(tt, .cdtData$GalParams$swhc[["file"]],
																	str_trim(tclvalue(DataType)), "SWHC")
	})

	###############

	tkgrid(chk.sep.year, row = 0, column = 0, sticky = 'we', rowspan = 1, columnspan = 10, padx = 1, pady = 1, ipadx = 1, ipady = 1)

	tkgrid(txt.1stdate0, row = 1, column = 0, sticky = 'we', rowspan = 1, columnspan = 5, padx = 1, pady = 1, ipadx = 1, ipady = 1)
	tkgrid(txt.1stdate1, row = 1, column = 5, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
	tkgrid(cb.1stdate1, row = 1, column = 6, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
	tkgrid(txt.1stdate2, row = 1, column = 7, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
	tkgrid(cb.1stdate2, row = 1, column = 8, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)

	tkgrid(txt.wb.1stday, row = 2, column = 0, sticky = 'we', rowspan = 1, columnspan = 5, padx = 1, pady = 1, ipadx = 1, ipady = 1)
	tkgrid(en.wb.1stday, row = 2, column = 5, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
	tkgrid(chk.wb.1stday, row = 2, column = 6, sticky = 'we', rowspan = 1, columnspan = 2, padx = 1, pady = 1, ipadx = 1, ipady = 1)
	tkgrid(bt.wb.1stday, row = 2, column = 8, sticky = 'we', rowspan = 1, columnspan = 2, padx = 1, pady = 1, ipadx = 1, ipady = 1)

	tkgrid(txt.wb.swhc, row = 3, column = 0, sticky = 'we', rowspan = 1, columnspan = 5, padx = 1, pady = 1, ipadx = 1, ipady = 1)
	tkgrid(en.wb.swhc, row = 3, column = 5, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
	tkgrid(chk.wb.swhc, row = 3, column = 6, sticky = 'we', rowspan = 1, columnspan = 2, padx = 1, pady = 1, ipadx = 1, ipady = 1)
	tkgrid(bt.wb.swhc, row = 3, column = 8, sticky = 'we', rowspan = 1, columnspan = 2, padx = 1, pady = 1, ipadx = 1, ipady = 1)

	###############

	tkbind(chk.wb.1stday, "<Button-1>", function(){
		stateMWB <- if(tclvalue(use.multi.wb) == '0') 'normal' else 'disabled'
		tkconfigure(bt.wb.1stday, state = stateMWB)
	})

	tkbind(chk.wb.swhc, "<Button-1>", function(){
		stateMSWHC <- if(tclvalue(use.multi.swhc) == '0') 'normal' else 'disabled'
		tkconfigure(bt.wb.swhc, state = stateMSWHC)
	})

	############################################
	tkgrid(frdatatype, row = 1, column = 0, sticky = '', padx = 1, pady = 3, ipadx = 1, ipady = 1)
	tkgrid(frameInData, row = 2, column = 0, sticky = '', padx = 1, pady = 3, ipadx = 1, ipady = 1)
	tkgrid(frSave, row = 3, column = 0, sticky = '', padx = 1, pady = 3, ipadx = 1, ipady = 1)
	tkgrid(frWBalance, row = 4, column = 0, sticky = 'we', padx = 1, pady = 3, ipadx = 1, ipady = 1)

	############################################

	tkgrid(frLeft, row = 0, column = 0, sticky = 'news', padx = 5, pady = 1, ipadx = 1, ipady = 1)

	############################################

	bt.prm.OK <- ttkbutton(frMRG1, text = .cdtEnv$tcl$lang$global[['button']][['1']])
	bt.prm.CA <- ttkbutton(frMRG1, text = .cdtEnv$tcl$lang$global[['button']][['2']])

	tkconfigure(bt.prm.OK, command = function(){
		if(str_trim(tclvalue(input.Etp)) %in% c("", "NA")){
			tkmessageBox(message = "No input for daily potential evapotranspiration", icon = "warning", type = "ok")
			tkwait.window(tt)
		}else if(str_trim(tclvalue(input.Prec)) %in% c("", "NA")){
			tkmessageBox(message = "No input for daily precipitation", icon = "warning", type = "ok")
			tkwait.window(tt)
		}else if(tclvalue(file.save) %in% c("", "NA")){
			tkmessageBox(message = "Choose a directory or enter the file to save results", icon = "warning", type = "ok")
			tkwait.window(tt)
		}else{
			.cdtData$GalParams$data.type <- datatypeVAL[CbdatatypeVAL %in% str_trim(tclvalue(DataType))]

			if(str_trim(tclvalue(DataType)) == CbdatatypeVAL[1]){
				.cdtData$GalParams$cdtstation$etp <- str_trim(tclvalue(input.Etp))
				.cdtData$GalParams$cdtstation$prec <- str_trim(tclvalue(input.Prec))
			}

			if(str_trim(tclvalue(DataType)) == CbdatatypeVAL[2]){
				.cdtData$GalParams$cdtdataset$etp <- str_trim(tclvalue(input.Etp))
				.cdtData$GalParams$cdtdataset$prec <- str_trim(tclvalue(input.Prec))
			}

			.cdtData$GalParams$output <- str_trim(tclvalue(file.save))

			.cdtData$GalParams$hdate$start.month <- which(MOIS %in% str_trim(tclvalue(start.month)))
			.cdtData$GalParams$hdate$start.day <- as.numeric(str_trim(tclvalue(start.day)))
			.cdtData$GalParams$hdate$separate.year <- switch(tclvalue(separate.year), '0' = FALSE, '1' = TRUE)

			.cdtData$GalParams$wb$multi <- switch(tclvalue(use.multi.wb), '0' = FALSE, '1' = TRUE)
			.cdtData$GalParams$swhc$multi <- switch(tclvalue(use.multi.swhc), '0' = FALSE, '1' = TRUE)

			.cdtData$GalParams$wb$wb1 <- as.numeric(str_trim(tclvalue(start.wb)))
			.cdtData$GalParams$swhc$cap.max <- as.numeric(str_trim(tclvalue(capacity.max)))

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
	tkwm.title(tt, 'Water Balance')
	tkwm.deiconify(tt)

	tkfocus(tt)
	tkbind(tt, "<Destroy>", function(){
		tkgrab.release(tt)
		tkfocus(.cdtEnv$tcl$main$win)
	})
	tkwait.window(tt)
}

############################################################

computeWB_get.WB.SWHC <- function(parent.win, Parameters, dataType, donne)
{
	listOpenFiles <- openFile_ttkcomboList()
	if(WindowsOS()){
		largeur <- 45
		largeur1 <- 29
	}else{
		largeur <- 34
		largeur1 <- 30
	}

	# xml.dlg <- file.path(.cdtDir$dirLocal, "languages", "cdtCompute_WB_dlgBox_1.xml")
	# lang.dlg <- cdtLanguageParse(xml.dlg, .cdtData$Config$lang.iso)

	CbdatatypeVAL <- .cdtEnv$tcl$lang$global[['combobox']][['2']][1:2]

	###################

	tt <- tktoplevel()
	tkgrab.set(tt)
	tkfocus(tt)

	frMRG0 <- tkframe(tt, relief = 'raised', borderwidth = 2)
	frMRG1 <- tkframe(tt)

	###################

	frFF <- tkframe(frMRG0, relief = "sunken", borderwidth = 2)

	input.file <- tclVar(Parameters)

	if(dataType == CbdatatypeVAL[1]){
		if(donne == "WB"){
			LABL <- "CDT stations file of the Water Balance on day 1"
			TXTA <- "CDT stations data format file containing the water balance on the first day of calculation, the data must have the same stations as the Precipitation and PET data"
		}else{
			LABL <- "CDT stations file containing the SWHC data"
			TXTA <- "CDT station data format file containing the soil water holding capacity data, the data must have the same stations as the Precipitation and PET data"
		}
	}else{
		if(donne == "WB"){
			LABL <- "NetCDF file of the Water Balance on day 1"
			TXTA <- "A NetCDF file containing the water balance on the first day of calculation, the grid must have the same resolution and extent as the Precipitation and PET"
		}else{
			LABL <- "NetCDF file containing the SWHC data"
			TXTA <- "A NetCDF file containing the soil water holding capacity data, the grid must have the same resolution and extent as the Precipitation and PET"
		}
	}

	txt.WB <- tklabel(frFF, text = LABL, anchor = 'w', justify = 'left')
	cb.WB <- ttkcombobox(frFF, values = unlist(listOpenFiles), textvariable = input.file, width = largeur)
	bt.WB <- tkbutton(frFF, text = "...")
	txta.WB <- tktext(frFF, bg = "white", font = "courier", cursor = "", wrap = "word", height = 7, width = largeur1)

	tkconfigure(bt.WB, command = function(){
		if(dataType == CbdatatypeVAL[1]){
			dat.opfiles <- getOpenFiles(tt)
			if(!is.null(dat.opfiles)){
				update.OpenFiles('ascii', dat.opfiles)
				listOpenFiles[[length(listOpenFiles) + 1]] <<- dat.opfiles[[1]]
				tclvalue(input.file) <- dat.opfiles[[1]]
				tkconfigure(cb.WB, values = unlist(listOpenFiles))
			}
		}else{
			nc.opfiles <- getOpenNetcdf(tt, initialdir = getwd())
			if(!is.null(nc.opfiles)){
				update.OpenFiles('netcdf', nc.opfiles)
				listOpenFiles[[length(listOpenFiles) + 1]] <<- nc.opfiles[[1]]
				tclvalue(input.file) <- nc.opfiles[[1]]
				tkconfigure(cb.WB, values = unlist(listOpenFiles))
			}
		}
	})

	tkgrid(txt.WB, row = 0, column = 0, sticky = 'we', rowspan = 1, columnspan = 5, padx = 1, pady = 1, ipadx = 1, ipady = 1)
	tkgrid(cb.WB, row = 1, column = 0, sticky = 'we', rowspan = 1, columnspan = 4, padx = 0, pady = 1, ipadx = 1, ipady = 1)
	tkgrid(bt.WB, row = 1, column = 4, sticky = 'w', rowspan = 1, columnspan = 1, padx = 0, pady = 1, ipadx = 1, ipady = 1)
	tkgrid(txta.WB, row = 2, column = 0, sticky = 'we', rowspan = 1, columnspan = 5, padx = 1, pady = 1, ipadx = 1, ipady = 1)

	tkinsert(txta.WB, "1.0", TXTA)
	tkconfigure(txta.WB, state = "disabled")

	###################

	tkgrid(frFF, row = 0, column = 0, sticky = 'ew', padx = 1, pady = 1)

	################################

	bt.prm.OK <- ttkbutton(frMRG1, text = .cdtEnv$tcl$lang$global[['button']][['1']])
	bt.prm.CA <- ttkbutton(frMRG1, text = .cdtEnv$tcl$lang$global[['button']][['2']])

	tkconfigure(bt.prm.OK, command = function(){
		if(str_trim(tclvalue(input.file)) %in% c("", "NA")){
			tkmessageBox(message = "No input data found", icon = "warning", type = "ok")
			tkwait.window(tt)
		}else{
			Parameters <<- str_trim(tclvalue(input.file))

			tkgrab.release(tt)
			tkdestroy(tt)
			tkfocus(parent.win)
		}
	})

	tkconfigure(bt.prm.CA, command = function(){
		tkgrab.release(tt)
		tkdestroy(tt)
		tkfocus(parent.win)
	})

	tkgrid(bt.prm.OK, row = 0, column = 0, sticky = 'w', padx = 5, pady = 1, ipadx = 1, ipady = 1)
	tkgrid(bt.prm.CA, row = 0, column = 1, sticky = 'e', padx = 5, pady = 1, ipadx = 1, ipady = 1)

	################################
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
	if(donne == "WB"){
		titre <- 'Water Balance - Initialization'
	}else{
		titre <- 'Soil Water Holding Capacity'
	}
	tkwm.title(tt, titre)
	tkwm.deiconify(tt)

	tkfocus(tt)
	tkbind(tt, "<Destroy>", function(){
		tkgrab.release(tt)
		tkfocus(parent.win)
	})
	tkwait.window(tt)
	return(Parameters)
}
