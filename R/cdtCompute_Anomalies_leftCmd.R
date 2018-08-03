
anomaliesCalcPanelCmd <- function(){
	listOpenFiles <- openFile_ttkcomboList()
	if(WindowsOS()){
		largeur0 <- .cdtEnv$tcl$fun$w.widgets(17)
		largeur1 <- .cdtEnv$tcl$fun$w.widgets(27)
		largeur2 <- .cdtEnv$tcl$fun$w.widgets(29)
		largeur3 <- 20
		largeur4 <- 21
	}else{
		largeur0 <- .cdtEnv$tcl$fun$w.widgets(18)
		largeur1 <- .cdtEnv$tcl$fun$w.widgets(22)
		largeur2 <- .cdtEnv$tcl$fun$w.widgets(23)
		largeur3 <- 15
		largeur4 <- 14
	}

	GeneralParameters <- list(intstep = "dekadal", data.type = "cdtstation", 
							cdtstation = list(file = ""),
							cdtdataset = list(index = ""),
							Dates = list(start.year = 1981, start.mon = 1, start.dek = 1,
										end.year = 2017, end.mon = 12, end.dek = 3),
							climato = list(clim.exist = FALSE, clim.file = "", allyears = TRUE,
											start = 1981, end = 2010, minyear = 20, window = 0),
							anomaly = "Difference", outdir = list(update = FALSE, dir = ""))

	# xml.dlg <- file.path(.cdtDir$dirLocal, "languages", "cdtCompute_Anomalies_leftCmd.xml")
	# lang.dlg <- cdtLanguageParse(xml.dlg, .cdtData$Config$lang.iso)
	# .cdtData$EnvData$message <- lang.dlg[['message']]

	###################

	.cdtEnv$tcl$main$cmd.frame <- tkframe(.cdtEnv$tcl$main$panel.left)

	tknote.cmd <- bwNoteBook(.cdtEnv$tcl$main$cmd.frame)
	cmd.tab1 <- bwAddTab(tknote.cmd, text = "Input")
	cmd.tab2 <- bwAddTab(tknote.cmd, text = "Anomalies")
	cmd.tab3 <- bwAddTab(tknote.cmd, text = "Maps")
	cmd.tab4 <- bwAddTab(tknote.cmd, text = "Graphs")
	cmd.tab5 <- bwAddTab(tknote.cmd, text = "Boundaries")

	bwRaiseTab(tknote.cmd, cmd.tab1)

	tkgrid.columnconfigure(cmd.tab1, 0, weight = 1)
	tkgrid.columnconfigure(cmd.tab2, 0, weight = 1)
	tkgrid.columnconfigure(cmd.tab3, 0, weight = 1)
	tkgrid.columnconfigure(cmd.tab4, 0, weight = 1)
	tkgrid.columnconfigure(cmd.tab5, 0, weight = 1)

	tkgrid.rowconfigure(cmd.tab1, 0, weight = 1)
	tkgrid.rowconfigure(cmd.tab2, 0, weight = 1)
	tkgrid.rowconfigure(cmd.tab3, 0, weight = 1)
	tkgrid.rowconfigure(cmd.tab4, 0, weight = 1)
	tkgrid.rowconfigure(cmd.tab5, 0, weight = 1)

	#######################################################################################################

	#Tab1
	subfr1 <- bwTabScrollableFrame(cmd.tab1)

		#######################

		frameTimeS <- ttklabelframe(subfr1, text = "Time step of input data", relief = 'groove')

		timeSteps <- tclVar()
		CbperiodVAL <- .cdtEnv$tcl$lang$global[['combobox']][['1']][2:5]
		periodVAL <- c('daily', 'pentad', 'dekadal', 'monthly')
		tclvalue(timeSteps) <- CbperiodVAL[periodVAL %in% GeneralParameters$intstep]

		cb.fperiod <- ttkcombobox(frameTimeS, values = CbperiodVAL, textvariable = timeSteps, width = largeur1)

		tkgrid(cb.fperiod, row = 0, column = 0, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)

		infobulle(cb.fperiod, 'Select the time step of the data')
		status.bar.display(cb.fperiod, 'Select the time step of the data')

		############

		tkbind(cb.fperiod, "<<ComboboxSelected>>", function(){
			if(tclvalue(updateAnom) == '0'){
				statedayW <- if(str_trim(tclvalue(timeSteps)) == CbperiodVAL[1] &
								tclvalue(climDataExist) == '0') "normal" else "disabled"
			}else statedayW <- "disabled"

			tkconfigure(en.daywin, state = statedayW)
			tclvalue(day.txtVar) <- ifelse(str_trim(tclvalue(timeSteps)) == CbperiodVAL[3], 'Dekad',
									ifelse(str_trim(tclvalue(timeSteps)) == CbperiodVAL[2], 'Pentad', 'Day'))
			statedate <<- if(tclvalue(timeSteps) == CbperiodVAL[4]) 'disabled' else 'normal'
		})

		#######################

		frameInData <- ttklabelframe(subfr1, text = "Input Data", relief = 'groove')

		DataType <- tclVar()
		CbdatatypeVAL <- .cdtEnv$tcl$lang$global[['combobox']][['2']][1:2]
		datatypeVAL <- c('cdtstation', 'cdtdataset')
		tclvalue(DataType) <- CbdatatypeVAL[datatypeVAL %in% GeneralParameters$data.type]

		if(GeneralParameters$data.type == 'cdtstation'){
			input.file <- tclVar(GeneralParameters$cdtstation$file)
			txt.INData <- 'File containing stations data'
		}
		if(GeneralParameters$data.type == 'cdtdataset'){
			input.file <- tclVar(GeneralParameters$cdtdataset$index)
			txt.INData <- 'Index file (*.rds) of the dataset'
		}
		txt.INData.var <- tclVar(txt.INData)

		txt.datatype <- tklabel(frameInData, text = "Format", anchor = 'w', justify = 'left')
		cb.datatype <- ttkcombobox(frameInData, values = CbdatatypeVAL, textvariable = DataType, width = largeur0)

		txt.infile <- tklabel(frameInData, text = tclvalue(txt.INData.var), textvariable = txt.INData.var, anchor = 'w', justify = 'left')

		if(GeneralParameters$data.type == 'cdtstation'){
			cb.en.infile <- ttkcombobox(frameInData, values = unlist(listOpenFiles), textvariable = input.file, width = largeur1)
		}else{
			cb.en.infile <- tkentry(frameInData, textvariable = input.file, width = largeur2)
		}
		bt.infile <- tkbutton(frameInData, text = "...")

		############

		tkconfigure(bt.infile, command = function(){
			if(GeneralParameters$data.type == 'cdtstation'){
				dat.opfiles <- getOpenFiles(.cdtEnv$tcl$main$win)
				if(!is.null(dat.opfiles)){
					update.OpenFiles('ascii', dat.opfiles)
					listOpenFiles[[length(listOpenFiles) + 1]] <<- dat.opfiles[[1]]
					tclvalue(input.file) <- dat.opfiles[[1]]
					tkconfigure(cb.en.infile, values = unlist(listOpenFiles))
				}
			}
			if(GeneralParameters$data.type == 'cdtdataset'){
				path.rds <- tclvalue(tkgetOpenFile(initialdir = getwd(), initialfile = "", filetypes = .cdtEnv$tcl$data$filetypes6))
				tclvalue(input.file) <- if(path.rds %in% c("", "NA") | is.na(path.rds)) "" else path.rds
			}
		})

		############

		tkgrid(txt.datatype, row = 0, column = 0, sticky = 'we', rowspan = 1, columnspan = 2, padx = 1, pady = 1, ipadx = 1, ipady = 1)
		tkgrid(cb.datatype, row = 0, column = 2, sticky = 'we', rowspan = 1, columnspan = 8, padx = 1, pady = 1, ipadx = 1, ipady = 1)
		tkgrid(txt.infile, row = 1, column = 0, sticky = 'we', rowspan = 1, columnspan = 10, padx = 1, pady = 1, ipadx = 1, ipady = 1)
		tkgrid(cb.en.infile, row = 2, column = 0, sticky = 'we', rowspan = 1, columnspan = 9, padx = 0, pady = 1, ipadx = 1, ipady = 1)
		tkgrid(bt.infile, row = 2, column = 9, sticky = 'w', rowspan = 1, columnspan = 1, padx = 0, pady = 1, ipadx = 1, ipady = 1)

		############
		infobulle(cb.datatype, 'Select the format of the input data')
		status.bar.display(cb.datatype, 'Select the format of the input data')

		if(GeneralParameters$data.type == 'cdtstation'){
			infobulle(cb.en.infile, 'Select the file containing the input data')
			status.bar.display(cb.en.infile, 'Select the file containing the input data')
			infobulle(bt.infile, 'Browse file if not listed')
			status.bar.display(bt.infile, 'Browse file if not listed')
		}
		if(GeneralParameters$data.type == 'cdtdataset'){
			infobulle(cb.en.infile, 'Enter the full path to the file <dataset name>.rds')
			status.bar.display(cb.en.infile, 'Enter the full path to the file <dataset name>.rds')
			infobulle(bt.infile, 'or browse here')
			status.bar.display(bt.infile, 'or browse here')
		}

		############

		tkbind(cb.datatype, "<<ComboboxSelected>>", function(){
			tkdestroy(cb.en.infile)
			tclvalue(input.file) <- ''

			###
			if(str_trim(tclvalue(DataType)) == CbdatatypeVAL[1]){
				tclvalue(txt.INData.var) <- 'File containing stations data'

				cb.en.infile <- ttkcombobox(frameInData, values = unlist(listOpenFiles), textvariable = input.file, width = largeur1)

				tkconfigure(bt.infile, command = function(){
					dat.opfiles <- getOpenFiles(.cdtEnv$tcl$main$win)
					if(!is.null(dat.opfiles)){
						update.OpenFiles('ascii', dat.opfiles)
						listOpenFiles[[length(listOpenFiles) + 1]] <<- dat.opfiles[[1]]
						tclvalue(input.file) <- dat.opfiles[[1]]
						tkconfigure(cb.en.infile, values = unlist(listOpenFiles))
					}
				})

				infobulle(cb.en.infile, 'Select the file containing the input data')
				status.bar.display(cb.en.infile, 'Select the file containing the input data')
				infobulle(bt.infile, 'Browse file if not listed')
				status.bar.display(bt.infile, 'Browse file if not listed')
			}

			###
			if(str_trim(tclvalue(DataType)) == CbdatatypeVAL[2]){
				tclvalue(txt.INData.var) <- 'Index file (*.rds) of the dataset'

				cb.en.infile <- tkentry(frameInData, textvariable = input.file, width = largeur2)

				tkconfigure(bt.infile, command = function(){
					path.rds <- tclvalue(tkgetOpenFile(initialdir = getwd(), filetypes = .cdtEnv$tcl$data$filetypes6))
					tclvalue(input.file) <- if(path.rds %in% c("", "NA") | is.na(path.rds)) "" else path.rds
				})

				infobulle(cb.en.infile, 'Enter the full path to the file <dataset name>.rds')
				status.bar.display(cb.en.infile, 'Enter the full path to the file <dataset name>.rds')
				infobulle(bt.infile, 'or browse here')
				status.bar.display(bt.infile, 'or browse here')
			}

			tkgrid(cb.en.infile, row = 2, column = 0, sticky = 'we', rowspan = 1, columnspan = 9, padx = 0, pady = 1, ipadx = 1, ipady = 1)
		})

		#############################

		frameDirSav <- tkframe(subfr1, relief = 'groove', borderwidth = 2)

		updateAnom <- tclVar(GeneralParameters$outdir$update)
		outAnom <- tclVar(GeneralParameters$outdir$dir)

		if(GeneralParameters$outdir$update){
			txt.upAnom <- 'Path to the file (Anomaly.rds)'
		}else{
			txt.upAnom <- "Directory to save the outputs"
		}
		txt.upAnom.var <- tclVar(txt.upAnom)

		chk.outAnom <- tkcheckbutton(frameDirSav, variable = updateAnom, text = "Update existing anomalies data", anchor = 'w', justify = 'left')
		txt.outAnom <- tklabel(frameDirSav, text = tclvalue(txt.upAnom.var), textvariable = txt.upAnom.var, anchor = 'w', justify = 'left')
		en.outAnom <- tkentry(frameDirSav, textvariable = outAnom, width = largeur2)
		bt.outAnom <- tkbutton(frameDirSav, text = "...")

		######

		tkconfigure(bt.outAnom, command = function(){
			if(GeneralParameters$outdir$update){
				path.anomIdx <- tclvalue(tkgetOpenFile(initialdir = getwd(), filetypes = .cdtEnv$tcl$data$filetypes6))
				tclvalue(outAnom) <- if(path.anomIdx %in% c("", "NA") | is.na(path.anomIdx)) "" else path.anomIdx
			}else{
				dirAnom <- tk_choose.dir(getwd(), "")
				tclvalue(outAnom) <- if(dirAnom %in% c("", "NA") | is.na(dirAnom)) "" else dirAnom
			}
		})

		######
		tkgrid(chk.outAnom, row = 0, column = 0, sticky = 'we', rowspan = 1, columnspan = 5, padx = 1, pady = 0, ipadx = 1, ipady = 1)
		tkgrid(txt.outAnom, row = 1, column = 0, sticky = 'we', rowspan = 1, columnspan = 5, padx = 1, pady = 0, ipadx = 1, ipady = 1)
		tkgrid(en.outAnom, row = 2, column = 0, sticky = 'we', rowspan = 1, columnspan = 4, padx = 0, pady = 0, ipadx = 1, ipady = 1)
		tkgrid(bt.outAnom, row = 2, column = 4, sticky = 'w', rowspan = 1, columnspan = 1, padx = 0, pady = 0, ipadx = 1, ipady = 1)

		infobulle(chk.outAnom, 'Check this box to update an existing anomalies data')
		status.bar.display(chk.outAnom, 'Check this box to update an existing anomalies data')
		if(GeneralParameters$outdir$update){
			infobulle(en.outAnom, 'Enter the full path to the file Anomaly.rds')
			status.bar.display(en.outAnom, 'Enter the full path to the file Anomaly.rds')
		}else{
			infobulle(en.outAnom, 'Enter the full path to directory to save outputs')
			status.bar.display(en.outAnom, 'Enter the full path to directory to save outputs')
		}
		infobulle(bt.outAnom, 'or browse here')
		status.bar.display(bt.outAnom, 'or browse here')

		#######

		tkbind(chk.outAnom, "<Button-1>", function(){
			if(tclvalue(updateAnom) == '0'){
				tclvalue(txt.upAnom.var) <- 'Path to the file (Anomaly.rds)'

				tkconfigure(bt.outAnom, command = function(){
					path.anomIdx <- tclvalue(tkgetOpenFile(initialdir = getwd(), initialfile = "", filetypes = .cdtEnv$tcl$data$filetypes6))
					tclvalue(outAnom) <- if(path.anomIdx %in% c("", "NA") | is.na(path.anomIdx)) "" else path.anomIdx
				})

				infobulle(en.outAnom, 'Enter the full path to the file Anomaly.rds')
				status.bar.display(en.outAnom, 'Enter the full path to the file Anomaly.rds')
			}else{
				tclvalue(txt.upAnom.var) <- "Directory to save the outputs"

				tkconfigure(bt.outAnom, command = function(){
					dirAnom <- tk_choose.dir(getwd(), "")
					tclvalue(outAnom) <- if(dirAnom %in% c("", "NA") | is.na(dirAnom)) "" else dirAnom
				})

				infobulle(en.outAnom, 'Enter the full path to directory to save outputs')
				status.bar.display(en.outAnom, 'Enter the full path to directory to save outputs')
			}

			if(tclvalue(updateAnom) == '1'){
				stateClim.Ex <- 'normal'
				stateClim <- if(tclvalue(climDataExist) == '1') 'normal' else 'disabled'
				stateBaseP <- if(tclvalue(climDataExist) == '1') 'disabled' else 'normal'
				statedayW <- if(str_trim(tclvalue(timeSteps)) == CbperiodVAL[1] & 
								tclvalue(climDataExist) == '0') "normal" else "disabled"
				stateAnomC <- 'normal'
			}else{
				stateClim.Ex <- 'disabled'
				stateClim <- 'disabled'
				stateBaseP <- 'disabled'
				statedayW <- 'disabled'
				stateAnomC <- 'disabled'
			}

			tkconfigure(chk.climIdx, state = stateClim.Ex)
			tkconfigure(en.climIdx, state = stateClim)
			tkconfigure(bt.climIdx, state = stateClim)

			tkconfigure(bt.BasePeriod, state = stateBaseP)
			tkconfigure(en.daywin, state = statedayW)
			tkconfigure(cb.anomaly, state = stateAnomC)
		})

		############################################

		tkgrid(frameTimeS, row = 0, column = 0, sticky = '', padx = 1, pady = 1, ipadx = 1, ipady = 1)
		tkgrid(frameInData, row = 1, column = 0, sticky = 'we', padx = 1, pady = 3, ipadx = 1, ipady = 1)
		tkgrid(frameDirSav, row = 2, column = 0, sticky = 'we', padx = 1, pady = 3, ipadx = 1, ipady = 1)

	#######################################################################################################

	#Tab2
	subfr2 <- bwTabScrollableFrame(cmd.tab2)

		##############################################

		txtdek <- switch(GeneralParameters$intstep, 'dekadal' = 'Dekad', 'pentad' = 'Pentad', 'Day')
		day.txtVar <- tclVar(txtdek)
		statedate <- if(GeneralParameters$intstep == 'monthly') 'disabled' else 'normal'

		btDateRange <- ttkbutton(subfr2, text = "Set Anomalies Date Range")

		tkconfigure(btDateRange, command = function(){
			Params <- GeneralParameters[["Dates"]]
			names(Params) <- c("start.year", "start.mon", "start.day",
								"end.year", "end.mon", "end.day")
			Params <- getInfoDateRange(.cdtEnv$tcl$main$win, Params, daypendek.lab = tclvalue(day.txtVar), state.dek = statedate)
			GeneralParameters$Dates$start.year <<- Params$start.year
			GeneralParameters$Dates$start.mon <<- Params$start.mon
			GeneralParameters$Dates$start.dek <<- Params$start.day
			GeneralParameters$Dates$end.year <<- Params$end.year
			GeneralParameters$Dates$end.mon <<- Params$end.mon
			GeneralParameters$Dates$end.dek <<- Params$end.day
		})

		infobulle(btDateRange, 'Start and end date to calculate the anomalies')
		status.bar.display(btDateRange, 'Start and end date to calculate the anomalies')

		#############################

		frameBaseP <- ttklabelframe(subfr2, text = "Climatology", relief = 'groove')

		climDataExist <- tclVar(GeneralParameters$climato$clim.exist)
		file.ClimIndex <- tclVar(GeneralParameters$climato$clim.file)
		dayWin <- tclVar(GeneralParameters$climato$window)

		if(!GeneralParameters$outdir$update){
			stateClim.Ex <- 'normal'
			stateClim <- if(GeneralParameters$climato$clim.exist) 'normal' else 'disabled'
			stateBaseP <- if(GeneralParameters$climato$clim.exist) 'disabled' else 'normal'
			statedayW <- if(GeneralParameters$intstep == "daily" & 
							!GeneralParameters$climato$clim.exist) "normal" else "disabled"
		}else{
			stateClim.Ex <- 'disabled'
			stateClim <- 'disabled'
			stateBaseP <- 'disabled'
			statedayW <- 'disabled'
		}

		chk.climIdx <- tkcheckbutton(frameBaseP, variable = climDataExist, text = "Climatologies data already computed", anchor = 'w', justify = 'left', state = stateClim.Ex)

		txt.climIdx <- tklabel(frameBaseP, text = 'Path to the file (Climatology.rds)', anchor = 'w', justify = 'left')
		en.climIdx <- tkentry(frameBaseP, textvariable = file.ClimIndex, width = largeur2, state = stateClim)
		bt.climIdx <- tkbutton(frameBaseP, text = "...", state = stateClim)

		bt.BasePeriod <- ttkbutton(frameBaseP, text = "Set Base Period", state = stateBaseP)

		txt.daywin1 <- tklabel(frameBaseP, text = "Centered time window", anchor = 'e', justify = 'right')
		en.daywin <- tkentry(frameBaseP, textvariable = dayWin, width = 3, state = statedayW)
		txt.daywin2 <- tklabel(frameBaseP, text = "days", anchor = 'w', justify = 'left')

		######

		tkconfigure(bt.climIdx, command = function(){
			path.climIdx <- tclvalue(tkgetOpenFile(initialdir = getwd(), filetypes = .cdtEnv$tcl$data$filetypes6))
			tclvalue(file.ClimIndex) <- if(path.climIdx %in% c("", "NA") | is.na(path.climIdx)) "" else path.climIdx
		})

		tkconfigure(bt.BasePeriod, command = function(){
			Params <- GeneralParameters[["climato"]][c('allyears', 'start', 'end', 'minyear')]
			names(Params) <- c('all.years', 'start.year', 'end.year', 'min.year')
			Params <- getInfoBasePeriod(.cdtEnv$tcl$main$win, Params)
			GeneralParameters$climato$allyears <<- Params$all.years
			GeneralParameters$climato$start <<- Params$start.year
			GeneralParameters$climato$end <<- Params$end.year
			GeneralParameters$climato$minyear <<- Params$min.year
		})

		######

		tkgrid(chk.climIdx, row = 0, column = 0, sticky = 'we', rowspan = 1, columnspan = 5, padx = 1, pady = 1, ipadx = 1, ipady = 1)
		tkgrid(txt.climIdx, row = 1, column = 0, sticky = 'we', rowspan = 1, columnspan = 5, padx = 1, pady = 1, ipadx = 1, ipady = 1)
		tkgrid(en.climIdx, row = 2, column = 0, sticky = 'we', rowspan = 1, columnspan = 4, padx = 1, pady = 1, ipadx = 1, ipady = 1)
		tkgrid(bt.climIdx, row = 2, column = 4, sticky = 'w', rowspan = 1, columnspan = 1, padx = 0, pady = 1, ipadx = 1, ipady = 1)
		tkgrid(bt.BasePeriod, row = 3, column = 0, sticky = 'we', rowspan = 1, columnspan = 5, padx = 1, pady = 1, ipadx = 1, ipady = 1)
		tkgrid(txt.daywin1, row = 4, column = 0, sticky = 'e', rowspan = 1, columnspan = 2, padx = 1, pady = 1, ipadx = 1, ipady = 1)
		tkgrid(en.daywin, row = 4, column = 2, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
		tkgrid(txt.daywin2, row = 4, column = 3, sticky = 'w', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)

		infobulle(en.climIdx, 'Enter the full path to the file Climatology.rds')
		status.bar.display(en.climIdx, 'Enter the full path to the file Climatology.rds')
		infobulle(bt.climIdx, 'or browse here')
		status.bar.display(bt.climIdx, 'or browse here')

		infobulle(en.daywin, 'The daily climatology is calculated using a centered (2 x window + 1) time window')
		status.bar.display(en.daywin, 'The daily climatology is calculated using a centered (2 x window + 1) time window')

		#######

		tkbind(chk.climIdx, "<Button-1>", function(){
			if(tclvalue(updateAnom) == '0'){
				stateClim <- if(tclvalue(climDataExist) == '1') 'disabled' else 'normal'
				stateBaseP <- if(tclvalue(climDataExist) == '1') 'normal' else 'disabled'
				statedayW <- if(str_trim(tclvalue(timeSteps)) == CbperiodVAL[1] &
								tclvalue(climDataExist) == '1') 'normal' else 'disabled'
			}else{
				stateClim <- 'disabled'
				stateBaseP <- 'disabled'
				statedayW <- 'disabled'
			}

			tkconfigure(en.climIdx, state = stateClim)
			tkconfigure(bt.climIdx, state = stateClim)

			tkconfigure(bt.BasePeriod, state = stateBaseP)
			tkconfigure(en.daywin, state = statedayW)
		})

		#############################

		frameAnom <- tkframe(subfr2, relief = 'groove', borderwidth = 2)

		AnomType <- c("Difference", "Percentage", "Standardized")
		anomaly <- tclVar(GeneralParameters$anomaly)

		stateAnomC <- if(!GeneralParameters$outdir$update) 'normal' else 'disabled'

		txt.anomaly <- tklabel(frameAnom, text = "Anomaly", anchor = 'e', justify = 'right')
		cb.anomaly <- ttkcombobox(frameAnom, values = AnomType, textvariable = anomaly, width = largeur3, state = stateAnomC)

		tkgrid(txt.anomaly, row = 0, column = 0, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
		tkgrid(cb.anomaly, row = 0, column = 1, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)

		infobulle(cb.anomaly, 'Select the method to calculate the anomalies')
		status.bar.display(cb.anomaly, 'Select the method to calculate the anomalies')

		#############################

		if(!is.null(.cdtData$EnvData$DirExist)){
			stateCaclBut <- if(tclvalue(.cdtData$EnvData$DirExist) == "1") "normal" else "disabled"
		}else stateCaclBut <- "normal"

		calculateBut <- ttkbutton(subfr2, text = "Calculate", state = stateCaclBut)

		#################

		tkconfigure(calculateBut, command = function(){
			GeneralParameters$intstep <- periodVAL[CbperiodVAL %in% str_trim(tclvalue(timeSteps))]
			GeneralParameters$data.type <- datatypeVAL[CbdatatypeVAL %in% str_trim(tclvalue(DataType))]

			if(str_trim(tclvalue(DataType)) == CbdatatypeVAL[1])
				GeneralParameters$cdtstation$file <- str_trim(tclvalue(input.file))
			if(str_trim(tclvalue(DataType)) == CbdatatypeVAL[2])
				GeneralParameters$cdtdataset$index <- str_trim(tclvalue(input.file))

			GeneralParameters$outdir$update <- switch(tclvalue(updateAnom), '0' = FALSE, '1' = TRUE)
			GeneralParameters$outdir$dir <- str_trim(tclvalue(outAnom))

			GeneralParameters$climato$clim.exist <- switch(tclvalue(climDataExist), '0' = FALSE, '1' = TRUE)
			GeneralParameters$climato$clim.file <- str_trim(tclvalue(file.ClimIndex))
			GeneralParameters$climato$window <- as.numeric(str_trim(tclvalue(dayWin)))

			GeneralParameters$anomaly <- str_trim(tclvalue(anomaly))

			Insert.Messages.Out("Calculate anomaly ......")

			tkconfigure(.cdtEnv$tcl$main$win, cursor = 'watch')
			tcl('update')
			ret <- tryCatch(
				{
					anomaliesCalcProcs(GeneralParameters)
				},
				warning = function(w) warningFun(w),
				error = function(e) errorFun(e),
				finally = {
					tkconfigure(.cdtEnv$tcl$main$win, cursor = '')
					tcl('update')
				}
			)

			msg0 <- "Anomaly calculation finished successfully"
			msg1 <- "Anomaly calculation failed"

			if(!is.null(ret)){
				if(ret == 0){
					Insert.Messages.Out(msg0)

					.cdtData$EnvData$plot.maps$data.type <- .cdtData$EnvData$output$params$data.type
					.cdtData$EnvData$plot.maps[c('lon', 'lat', 'id')] <- .cdtData$EnvData$output$data[c('lon', 'lat', 'id')]
					###################

					set.anomaly.dates()
					widgets.Station.Pixel()
					set.plot.type()
					res <- try(read.Anomaly.Map(), silent = TRUE)
					if(inherits(res, "try-error") | is.null(res)) return(NULL)
				}else Insert.Messages.Out(msg1, format = TRUE)
			}else Insert.Messages.Out(msg1, format = TRUE)
		})

		############################################

		tkgrid(btDateRange, row = 0, column = 0, sticky = 'we', padx = 1, pady = 3, ipadx = 1, ipady = 1)
		tkgrid(frameBaseP, row = 1, column = 0, sticky = 'we', padx = 1, pady = 1, ipadx = 1, ipady = 1)
		tkgrid(frameAnom, row = 2, column = 0, sticky = '', padx = 1, pady = 3, ipadx = 1, ipady = 1)
		tkgrid(calculateBut, row = 3, column = 0, sticky = '', padx = 1, pady = 3, ipadx = 1, ipady = 1)

	#######################################################################################################

	#Tab3
	subfr3 <- bwTabScrollableFrame(cmd.tab3)

		##############################################

		frameAnomalyDat <- ttklabelframe(subfr3, text = "Anomalies data", relief = 'groove')

		.cdtData$EnvData$DirExist <- tclVar(0)
		file.AnomIndex <- tclVar()

		stateAnomDat <- if(tclvalue(.cdtData$EnvData$DirExist) == "1") "normal" else "disabled"

		chk.anomIdx <- tkcheckbutton(frameAnomalyDat, variable = .cdtData$EnvData$DirExist, text = "Anomalies data already computed", anchor = 'w', justify = 'left')
		en.anomIdx <- tkentry(frameAnomalyDat, textvariable = file.AnomIndex, width = largeur2, state = stateAnomDat)
		bt.anomIdx <- tkbutton(frameAnomalyDat, text = "...", state = stateAnomDat)

		tkconfigure(bt.anomIdx, command = function(){
			path.Anom <- tclvalue(tkgetOpenFile(initialdir = getwd(), filetypes = .cdtEnv$tcl$data$filetypes6))
			if(path.Anom %in% c("", "NA") | is.na(path.Anom)) return(NULL)
			tclvalue(file.AnomIndex) <- path.Anom

			if(file.exists(str_trim(tclvalue(file.AnomIndex)))){
				OutAnomdata <- try(readRDS(str_trim(tclvalue(file.AnomIndex))), silent = TRUE)
				if(inherits(OutAnomdata, "try-error")){
					Insert.Messages.Out('Unable to load anomalies data', format = TRUE)
					Insert.Messages.Out(gsub('[\r\n]', '', OutAnomdata[1]), format = TRUE)
					tkconfigure(cb.anom.Date, values = "")
					tclvalue(.cdtData$EnvData$anomDate) <- ""
					return(NULL)
				}

				.cdtData$EnvData$output <- OutAnomdata
				.cdtData$EnvData$PathAnom <- dirname(str_trim(tclvalue(file.AnomIndex)))
				.cdtData$EnvData$plot.maps$data.type <- .cdtData$EnvData$output$params$data.type
				.cdtData$EnvData$plot.maps[c('lon', 'lat', 'id')] <- .cdtData$EnvData$output$data[c('lon', 'lat', 'id')]
				###################
				set.anomaly.dates()
				widgets.Station.Pixel()
				set.plot.type()
				ret <- try(read.Anomaly.Map(), silent = TRUE)
				if(inherits(ret, "try-error") | is.null(ret)) return(NULL)
			}
		})

		tkgrid(chk.anomIdx, row = 0, column = 0, sticky = 'we', rowspan = 1, columnspan = 5, padx = 1, pady = 1, ipadx = 1, ipady = 1)
		tkgrid(en.anomIdx, row = 1, column = 0, sticky = 'we', rowspan = 1, columnspan = 4, padx = 1, pady = 1, ipadx = 1, ipady = 1)
		tkgrid(bt.anomIdx, row = 1, column = 4, sticky = 'w', rowspan = 1, columnspan = 1, padx = 0, pady = 1, ipadx = 1, ipady = 1)

		###############

		tkbind(chk.anomIdx, "<Button-1>", function(){
			stateAnomDat <- if(tclvalue(.cdtData$EnvData$DirExist) == '1') 'disabled' else 'normal'
			tkconfigure(en.anomIdx, state = stateAnomDat)
			tkconfigure(bt.anomIdx, state = stateAnomDat)
			stateCaclBut <- if(tclvalue(.cdtData$EnvData$DirExist) == '1') 'normal' else 'disabled'
			tkconfigure(calculateBut, state = stateCaclBut)
		})

		##############################################

		frameAnomalyMap <- ttklabelframe(subfr3, text = "Anomaly Map", relief = 'groove')

		.cdtData$EnvData$anomDate <- tclVar()

		cb.anom.Date <- ttkcombobox(frameAnomalyMap, values = "", textvariable = .cdtData$EnvData$anomDate, width = largeur4, justify = 'center')
		bt.anom.Date.prev <- ttkbutton(frameAnomalyMap, text = "<<", width = 3)
		bt.anom.Date.next <- ttkbutton(frameAnomalyMap, text = ">>", width = 3)
		bt.anom.maps <- ttkbutton(frameAnomalyMap, text = .cdtEnv$tcl$lang$global[['button']][['3']], width = 7)
		bt.anom.MapOpt <- ttkbutton(frameAnomalyMap, text = .cdtEnv$tcl$lang$global[['button']][['4']], width = 7)

		###############

		.cdtData$EnvData$tab$pointSize <- NULL
		.cdtData$EnvData$anomMapOp <- list(presetCol = list(color = 'tim.colors', reverse = FALSE),
											userCol = list(custom = FALSE, color = NULL),
											userLvl = list(custom = FALSE, levels = NULL, equidist = FALSE),
											title = list(user = FALSE, title = ''),
											colkeyLab = list(user = FALSE, label = ''),
											scalebar = list(add = FALSE, pos = 'bottomleft'),
											pointSize = .cdtData$EnvData$tab$pointSize)

		tkconfigure(bt.anom.MapOpt, command = function(){
			if(!is.null(.cdtData$EnvData$anomdata$map)){
				atlevel <- pretty(.cdtData$EnvData$anomdata$map$z, n = 10, min.n = 7)
				if(is.null(.cdtData$EnvData$anomMapOp$userLvl$levels)){
					.cdtData$EnvData$anomMapOp$userLvl$levels <- atlevel
				}else{
					if(!.cdtData$EnvData$anomMapOp$userLvl$custom)
						.cdtData$EnvData$anomMapOp$userLvl$levels <- atlevel
				}
			}
			.cdtData$EnvData$anomMapOp <- MapGraph.MapOptions(.cdtData$EnvData$anomMapOp)

			if(str_trim(tclvalue(.cdtData$EnvData$plot.maps$plot.type)) == "Points")
				.cdtData$EnvData$tab$pointSize <- .cdtData$EnvData$anomMapOp$pointSize
		})

		#########
		.cdtData$EnvData$tab$AnomMap <- NULL

		tkconfigure(bt.anom.maps, command = function(){
			if(str_trim(tclvalue(.cdtData$EnvData$anomDate)) != "" &
				!is.null(.cdtData$EnvData$anomdata))
					anomaliesCalc.Display.Maps()
		})

		tkconfigure(bt.anom.Date.prev, command = function(){
			if(str_trim(tclvalue(.cdtData$EnvData$anomDate)) != ""){
				if(.cdtData$EnvData$output$params$data.type == "cdtstation")
					anomDates <- .cdtData$EnvData$output$data$dates
				else anomDates <- .cdtData$EnvData$output$dates

				idaty <- which(anomDates == str_trim(tclvalue(.cdtData$EnvData$anomDate)))
				idaty <- idaty - 1
				if(idaty < 1) idaty <- length(anomDates)
				tclvalue(.cdtData$EnvData$anomDate) <- anomDates[idaty]

				ret <- try(read.Anomaly.Map(), silent = TRUE)
				if(inherits(ret, "try-error") | is.null(ret)) return(NULL)

				anomaliesCalc.Display.Maps()
			}
		})

		tkconfigure(bt.anom.Date.next, command = function(){
			if(str_trim(tclvalue(.cdtData$EnvData$anomDate)) != ""){
				if(.cdtData$EnvData$output$params$data.type == "cdtstation")
					anomDates <- .cdtData$EnvData$output$data$dates
				else anomDates <- .cdtData$EnvData$output$dates

				idaty <- which(anomDates == str_trim(tclvalue(.cdtData$EnvData$anomDate)))
				idaty <- idaty + 1
				if(idaty > length(anomDates)) idaty <- 1
				tclvalue(.cdtData$EnvData$anomDate) <- anomDates[idaty]

				ret <- try(read.Anomaly.Map(), silent = TRUE)
				if(inherits(ret, "try-error") | is.null(ret)) return(NULL)

				anomaliesCalc.Display.Maps()
			}
		})

		###############

		tkgrid(bt.anom.Date.prev, row = 0, column = 0, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
		tkgrid(cb.anom.Date, row = 0, column = 1, sticky = 'we', rowspan = 1, columnspan = 2, padx = 1, pady = 1, ipadx = 1, ipady = 1)
		tkgrid(bt.anom.Date.next, row = 0, column = 3, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
		tkgrid(bt.anom.maps, row = 0, column = 4, sticky = 'we', rowspan = 1, columnspan = 2, padx = 1, pady = 1, ipadx = 1, ipady = 1)
		tkgrid(bt.anom.MapOpt, row = 1, column = 4, sticky = 'we', rowspan = 1, columnspan = 2, padx = 1, pady = 1, ipadx = 1, ipady = 1)

		###############

		tkbind(cb.anom.Date, "<<ComboboxSelected>>", function(){
			if(!is.null(.cdtData$EnvData$anomdata)){
				ret <- try(read.Anomaly.Map(), silent = TRUE)
				if(inherits(ret, "try-error") | is.null(ret)) return(NULL)
			}
		})

		##############################################

		framePlotType <- tkframe(subfr3)

		.cdtData$EnvData$plot.maps$plot.type <- tclVar("Pixels")

		txt.plotType <- tklabel(framePlotType, text = "Plot Type", anchor = 'e', justify = 'right')
		cb.plotType <- ttkcombobox(framePlotType, values = "Pixels", textvariable = .cdtData$EnvData$plot.maps$plot.type, width = largeur4)

		tkgrid(txt.plotType, row = 0, column = 0, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
		tkgrid(cb.plotType, row = 0, column = 1, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)

		###############

		tkbind(cb.plotType, "<<ComboboxSelected>>", function(){
			if(!is.null(.cdtData$EnvData$anomdata)){
				ret <- try(read.Anomaly.Map(), silent = TRUE)
				if(inherits(ret, "try-error") | is.null(ret)) return(NULL)
			}
		})

		##############################################

		tkgrid(frameAnomalyDat, row = 0, column = 0, sticky = 'we', padx = 1, pady = 1, ipadx = 1, ipady = 1)
		tkgrid(frameAnomalyMap, row = 1, column = 0, sticky = 'we', padx = 1, pady = 3, ipadx = 1, ipady = 1)
		tkgrid(framePlotType, row = 2, column = 0, sticky = '', padx = 1, pady = 3, ipadx = 1, ipady = 1)

	#######################################################################################################

	#Tab4
	subfr4 <- bwTabScrollableFrame(cmd.tab4)

		##############################################

		frameAnomalyTS <- ttklabelframe(subfr4, text = "Anomaly Graph", relief = 'groove')

		typeTSPLOT <- c("Bar", "Line")
		.cdtData$EnvData$plot.maps$typeTSp <- tclVar("Bar")

		cb.typeTSp <- ttkcombobox(frameAnomalyTS, values = typeTSPLOT, textvariable = .cdtData$EnvData$plot.maps$typeTSp, width = largeur4)
		bt.TsGraph.plot <- ttkbutton(frameAnomalyTS, text = .cdtEnv$tcl$lang$global[['button']][['3']], width = 7)
		bt.TSGraphOpt <- ttkbutton(frameAnomalyTS, text = .cdtEnv$tcl$lang$global[['button']][['4']], width = 8)

		#################

		.cdtData$EnvData$TSGraphOp <- list(
									anomaly = list(
											anom = NULL,
											xlim = list(is.min = FALSE, min = "1981-1-1", is.max = FALSE, max = "2017-12-31"),
											ylim = list(is.min = FALSE, min = -100, is.max = FALSE, max = 100),
											axislabs = list(is.xlab = FALSE, xlab = '', is.ylab = FALSE, ylab = ''),
											title = list(is.title = FALSE, title = '', position = 'top'),
											colors = list(negative = "blue", positive = "red")
										),
									line = list(
										xlim = list(is.min = FALSE, min = "1981-1-1", is.max = FALSE, max = "2017-12-31"),
										ylim = list(is.min = FALSE, min = 0, is.max = FALSE, max = 100),
										axislabs = list(is.xlab = FALSE, xlab = '', is.ylab = FALSE, ylab = ''),
										title = list(is.title = FALSE, title = '', position = 'top'),
										plot = list(type = 'both',
											col = list(line = "red", points = "blue"),
											lwd = 2, cex = 1.4),
										legend = NULL)
									)

		tkconfigure(bt.TSGraphOpt, command = function(){
			suffix.fun <- switch(str_trim(tclvalue(.cdtData$EnvData$plot.maps$typeTSp)),
									"Bar" = "Anomaly",
									"Line" = "Line")
			plot.fun <- get(paste0("MapGraph.GraphOptions.", suffix.fun), mode = "function")
			.cdtData$EnvData$TSGraphOp <- plot.fun(.cdtData$EnvData$TSGraphOp)
		})

		#########
		.cdtData$EnvData$tab$AnomGraph <- NULL

		tkconfigure(bt.TsGraph.plot, command = function(){
			if(!is.null(.cdtData$EnvData$anomdata)){
				imgContainer <- CDT.Display.Graph(anomaliesCalc.plotAnomGraph, .cdtData$EnvData$tab$AnomGraph, 'Anomaly-Graph')
				.cdtData$EnvData$tab$AnomGraph <- imageNotebookTab_unik(imgContainer, .cdtData$EnvData$tab$AnomGraph)
			}
		})

		#################

		tkgrid(cb.typeTSp, row = 0, column = 0, sticky = 'we', pady = 1, columnspan = 1)
		tkgrid(bt.TSGraphOpt, row = 0, column = 1, sticky = 'we', padx = 4, pady = 1, columnspan = 1)
		tkgrid(bt.TsGraph.plot, row = 0, column = 2, sticky = 'we', pady = 1, columnspan = 1)

		##############################################

		frameSTNCrds <- ttklabelframe(subfr4, text = "Station/Coordinates", relief = 'groove')

		frTS2 <- tkframe(frameSTNCrds)
		.cdtData$EnvData$plot.maps$lonLOC <- tclVar()
		.cdtData$EnvData$plot.maps$latLOC <- tclVar()
		.cdtData$EnvData$plot.maps$stnIDTSp <- tclVar()

		tkgrid(frTS2, row = 0, column = 0, sticky = 'e', pady = 1)

		##############################################

		tkgrid(frameAnomalyTS, row = 0, column = 0, sticky = 'we', pady = 1)
		tkgrid(frameSTNCrds, row = 1, column = 0, sticky = '', pady = 3)

	#######################################################################################################

	#Tab5
	subfr5 <- bwTabScrollableFrame(cmd.tab5)

		##############################################

		frameSHP <- ttklabelframe(subfr5, text = "Boundaries", relief = 'groove')

		.cdtData$EnvData$shp$add.shp <- tclVar(FALSE)
		file.plotShp <- tclVar()
		stateSHP <- "disabled"

		chk.addshp <- tkcheckbutton(frameSHP, variable = .cdtData$EnvData$shp$add.shp, text = "Add boundaries to Map", anchor = 'w', justify = 'left')
		bt.addshpOpt <- ttkbutton(frameSHP, text = .cdtEnv$tcl$lang$global[['button']][['4']], state = stateSHP)
		cb.addshp <- ttkcombobox(frameSHP, values = unlist(listOpenFiles), textvariable = file.plotShp, width = largeur1, state = stateSHP)
		bt.addshp <- tkbutton(frameSHP, text = "...", state = stateSHP)

		########
		tkconfigure(bt.addshp, command = function(){
			shp.opfiles <- getOpenShp(.cdtEnv$tcl$main$win)
			if(!is.null(shp.opfiles)){
				update.OpenFiles('shp', shp.opfiles)
				tclvalue(file.plotShp) <- shp.opfiles[[1]]
				listOpenFiles[[length(listOpenFiles) + 1]] <<- shp.opfiles[[1]]
				tkconfigure(cb.addshp, values = unlist(listOpenFiles))

				shpofile <- getShpOpenData(file.plotShp)
				if(is.null(shpofile))
					.cdtData$EnvData$shp$ocrds <- NULL
				else
					.cdtData$EnvData$shp$ocrds <- getBoundaries(shpofile[[2]])
			}
		})

		########
		.cdtData$EnvData$SHPOp <- list(col = "black", lwd = 1.5)

		tkconfigure(bt.addshpOpt, command = function(){
			.cdtData$EnvData$SHPOp <- MapGraph.GraphOptions.LineSHP(.cdtData$EnvData$SHPOp)
		})

		########
		tkgrid(chk.addshp, row = 0, column = 0, sticky = 'we', rowspan = 1, columnspan = 6, padx = 1, pady = 1)
		tkgrid(bt.addshpOpt, row = 0, column = 6, sticky = 'we', rowspan = 1, columnspan = 2, padx = 1, pady = 1)
		tkgrid(cb.addshp, row = 1, column = 0, sticky = 'we', rowspan = 1, columnspan = 7, padx = 1, pady = 1)
		tkgrid(bt.addshp, row = 1, column = 7, sticky = 'w', rowspan = 1, columnspan = 1, padx = 0, pady = 1)

		#################
		tkbind(cb.addshp, "<<ComboboxSelected>>", function(){
			shpofile <- getShpOpenData(file.plotShp)
			if(is.null(shpofile))
				.cdtData$EnvData$shp$ocrds <- NULL
			else
				.cdtData$EnvData$shp$ocrds <- getBoundaries(shpofile[[2]])
		})

		tkbind(chk.addshp, "<Button-1>", function(){
			stateSHP <- if(tclvalue(.cdtData$EnvData$shp$add.shp) == "1") "disabled" else "normal"
			tkconfigure(cb.addshp, state = stateSHP)
			tkconfigure(bt.addshp, state = stateSHP)
			tkconfigure(bt.addshpOpt, state = stateSHP)
		})

		##############################################

		tkgrid(frameSHP, row = 0, column = 0, sticky = 'we', pady = 1)

	#######################################################################################################

	widgets.Station.Pixel <- function(){
		tkdestroy(frTS2)
		frTS2 <<- tkframe(frameSTNCrds)

		if(.cdtData$EnvData$output$params$data.type == "cdtstation"){
			stnIDTSPLOT <- .cdtData$EnvData$output$data$id
			txt.stnSel <- tklabel(frTS2, text = "Select station to plot")
			bt.stnID.prev <- ttkbutton(frTS2, text = "<<", width = 6)
			bt.stnID.next <- ttkbutton(frTS2, text = ">>", width = 6)
			cb.stnID <- ttkcombobox(frTS2, values = stnIDTSPLOT, textvariable = .cdtData$EnvData$plot.maps$stnIDTSp, width = largeur4)
			tclvalue(.cdtData$EnvData$plot.maps$stnIDTSp) <- stnIDTSPLOT[1]

			tkconfigure(bt.stnID.prev, command = function(){
				if(!is.null(.cdtData$EnvData$anomdata)){
					istn <- which(stnIDTSPLOT == str_trim(tclvalue(.cdtData$EnvData$plot.maps$stnIDTSp)))
					istn <- istn-1
					if(istn < 1) istn <- length(stnIDTSPLOT)
					tclvalue(.cdtData$EnvData$plot.maps$stnIDTSp) <- stnIDTSPLOT[istn]

					imgContainer <- CDT.Display.Graph(anomaliesCalc.plotAnomGraph, .cdtData$EnvData$tab$AnomGraph, 'Anomaly-Graph')
					.cdtData$EnvData$tab$AnomGraph <- imageNotebookTab_unik(imgContainer, .cdtData$EnvData$tab$AnomGraph)
				}
			})

			tkconfigure(bt.stnID.next, command = function(){
				if(!is.null(.cdtData$EnvData$anomdata)){
					istn <- which(stnIDTSPLOT == str_trim(tclvalue(.cdtData$EnvData$plot.maps$stnIDTSp)))
					istn <- istn+1
					if(istn > length(stnIDTSPLOT)) istn <- 1
					tclvalue(.cdtData$EnvData$plot.maps$stnIDTSp) <- stnIDTSPLOT[istn]

					imgContainer <- CDT.Display.Graph(anomaliesCalc.plotAnomGraph, .cdtData$EnvData$tab$AnomGraph, 'Anomaly-Graph')
					.cdtData$EnvData$tab$AnomGraph <- imageNotebookTab_unik(imgContainer, .cdtData$EnvData$tab$AnomGraph)
				}
			})

			tkgrid(txt.stnSel, row = 0, column = 0, sticky = '', rowspan = 1, columnspan = 3, padx = 1, pady = 1, ipadx = 1, ipady = 1)
			tkgrid(bt.stnID.prev, row = 1, column = 0, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
			tkgrid(cb.stnID, row = 1, column = 1, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
			tkgrid(bt.stnID.next, row = 1, column = 2, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
			tkgrid(cb.stnID, row = 1, column = 1, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
		}else{
			txt.crdSel <- tklabel(frTS2, text = "Enter longitude and latitude to plot", anchor = 'w', justify = 'left')
			txt.lonLoc <- tklabel(frTS2, text = "Longitude", anchor = 'e', justify = 'right')
			en.lonLoc <- tkentry(frTS2, textvariable = .cdtData$EnvData$plot.maps$lonLOC, width = 8)
			txt.latLoc <- tklabel(frTS2, text = "Latitude", anchor = 'e', justify = 'right')
			en.latLoc <- tkentry(frTS2, textvariable = .cdtData$EnvData$plot.maps$latLOC, width = 8)
			stnIDTSPLOT <- ""
			tclvalue(.cdtData$EnvData$plot.maps$stnIDTSp) <- ""

			tkgrid(txt.crdSel, row = 0, column = 0, sticky = 'we', rowspan = 1, columnspan = 4, padx = 1, pady = 1, ipadx = 1, ipady = 1)
			tkgrid(txt.lonLoc, row = 1, column = 0, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
			tkgrid(en.lonLoc, row = 1, column = 1, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
			tkgrid(txt.latLoc, row = 1, column = 2, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
			tkgrid(en.latLoc, row = 1, column = 3, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
		}

		tkgrid(frTS2, row = 0, column = 0, sticky = 'e', pady = 1)
		return(0)
	}

	#################

	set.plot.type <- function(){
		if(.cdtData$EnvData$output$params$data.type == "cdtstation")
		{
			plot.type <- c("Pixels", "Points")
			.cdtData$EnvData$plot.maps$.data.type <- "Points"

			.cdtData$EnvData$anomMapOp$pointSize <- 0.7
		}else{
			plot.type <- c("Pixels", "FilledContour")
			.cdtData$EnvData$plot.maps$.data.type <- "Grid"
		}
		tkconfigure(cb.plotType, values = plot.type)
	}

	#################

	set.anomaly.dates <- function(){
		if(.cdtData$EnvData$output$params$data.type == "cdtstation")
			anomDates <- .cdtData$EnvData$output$data$dates
		else anomDates <- .cdtData$EnvData$output$dates
		tkconfigure(cb.anom.Date, values = anomDates)
		tclvalue(.cdtData$EnvData$anomDate) <- anomDates[1]
		return(0)
	}

	#######################################################################################################

	read.Anomaly.Map <- function(){
		tkconfigure(.cdtEnv$tcl$main$win, cursor = 'watch')
		tcl('update')
		on.exit({
			tkconfigure(.cdtEnv$tcl$main$win, cursor = '')
			tcl('update')
		})

		if(.cdtData$EnvData$output$params$data.type == "cdtstation"){
			fileAnomdata <- file.path(.cdtData$EnvData$PathAnom, "CDTANOM/CDTANOM.rds")
			if(!file.exists(fileAnomdata)){
				Insert.Messages.Out(paste(fileAnomdata, 'not found'), format = TRUE)
				return(NULL)
			}

			change.plot <- str_trim(tclvalue(.cdtData$EnvData$plot.maps$plot.type))

			########
			readAnomData <- TRUE
			if(!is.null(.cdtData$EnvData$anomdata))
				if(!is.null(.cdtData$EnvData$fileAnomdata))
					if(.cdtData$EnvData$fileAnomdata == fileAnomdata) readAnomData <- FALSE

			if(readAnomData){
				.cdtData$EnvData$anomdata$data <- readRDS(fileAnomdata)
				.cdtData$EnvData$fileAnomdata <- fileAnomdata
			}

			########
			rasterAnomData <- TRUE
			if(!rasterAnomData)
				if(!is.null(.cdtData$EnvData$anomdata$rasterDate))
					if(.cdtData$EnvData$fileAnomdata == fileAnomdata)
						if(.cdtData$EnvData$anomdata$rasterDate == str_trim(tclvalue(.cdtData$EnvData$anomDate))) rasterAnomData <- FALSE

			if(!rasterAnomData)
				if(.cdtData$EnvData$change.plot != change.plot) rasterAnomData <- TRUE

			if(rasterAnomData){
				idt <- which(.cdtData$EnvData$output$data$dates == as.numeric(str_trim(tclvalue(.cdtData$EnvData$anomDate))))

				X0 <- .cdtData$EnvData$output$data$lon
				Y0 <- .cdtData$EnvData$output$data$lat
				VAR0 <- as.numeric(.cdtData$EnvData$anomdata$data[idt, ])

				if(change.plot == "Pixels"){
					nx <- nx_ny_as.image(diff(range(X0)))
					ny <- nx_ny_as.image(diff(range(Y0)))
					tmp <- cdt.as.image(VAR0, nx = nx, ny = ny, pts.xy = cbind(X0, Y0))
					.cdtData$EnvData$anomdata$map$x <- tmp$x
					.cdtData$EnvData$anomdata$map$y <- tmp$y
					.cdtData$EnvData$anomdata$map$z <- tmp$z
					rm(tmp)
				}

				if(change.plot == "Points"){
					.cdtData$EnvData$anomdata$map$x <- X0
					.cdtData$EnvData$anomdata$map$y <- Y0
					.cdtData$EnvData$anomdata$map$z <- VAR0
				}

				.cdtData$EnvData$anomdata$rasterDate <- str_trim(tclvalue(.cdtData$EnvData$anomDate))
				.cdtData$EnvData$change.plot <- change.plot
			}
		}else{
			fileAnomdata <- file.path(.cdtData$EnvData$PathAnom, "DATA_NetCDF/CDTANOM",
							paste0("anomaly_", str_trim(tclvalue(.cdtData$EnvData$anomDate)), ".nc"))
			if(!file.exists(fileAnomdata)){
				Insert.Messages.Out(paste(fileAnomdata, 'not found'), format = TRUE)
				return(NULL)
			}

			readAnomData <- TRUE
			if(!is.null(.cdtData$EnvData$anomdata))
				if(!is.null(.cdtData$EnvData$fileAnomdata))
					if(.cdtData$EnvData$fileAnomdata == fileAnomdata) readAnomData <- FALSE

			if(readAnomData){
				nc <- nc_open(fileAnomdata)
				.cdtData$EnvData$anomdata$map$x <- nc$dim[[1]]$vals
				.cdtData$EnvData$anomdata$map$y <- nc$dim[[2]]$vals
				.cdtData$EnvData$anomdata$map$z <- ncvar_get(nc, varid = nc$var[[1]]$name)
				nc_close(nc)
				.cdtData$EnvData$fileAnomdata <- fileAnomdata
			}

			###################

			fileAnomIdx <- file.path(.cdtData$EnvData$PathAnom, "CDTANOM/CDTANOM.rds")

			readAnomIdx <- TRUE
			if(!is.null(.cdtData$EnvData$cdtdataset))
				if(!is.null(.cdtData$EnvData$fileAnomIdx))
					if(.cdtData$EnvData$fileAnomIdx == fileAnomIdx) readAnomIdx <- FALSE
			if(readAnomIdx){
				.cdtData$EnvData$cdtdataset <- readRDS(fileAnomIdx)
				.cdtData$EnvData$cdtdataset$fileInfo <- fileAnomIdx
				.cdtData$EnvData$fileAnomIdx <- fileAnomIdx
			}
		}

		return(0)
	}

	#######################################################################################################

	tkgrid(tknote.cmd, sticky = 'nwes')
	tkgrid.columnconfigure(tknote.cmd, 0, weight = 1)
	tkgrid.rowconfigure(tknote.cmd, 0, weight = 1)

	tcl('update')
	tkgrid(.cdtEnv$tcl$main$cmd.frame, sticky = 'nwes', pady = 1)
	tkgrid.columnconfigure(.cdtEnv$tcl$main$cmd.frame, 0, weight = 1)
	tkgrid.rowconfigure(.cdtEnv$tcl$main$cmd.frame, 0, weight = 1)

	invisible()
}
