
climdexPanelCmd.TT <- function(){
	listOpenFiles <- openFile_ttkcomboList()
	if(WindowsOS()){
		largeur0 <- .cdtEnv$tcl$fun$w.widgets(22)
		largeur1 <- .cdtEnv$tcl$fun$w.widgets(27)
		largeur2 <- .cdtEnv$tcl$fun$w.widgets(29)
		largeur3 <- 28
		largeur4 <- 21
		largeur5 <- 22
	}else{
		largeur0 <- .cdtEnv$tcl$fun$w.widgets(20)
		largeur1 <- .cdtEnv$tcl$fun$w.widgets(22)
		largeur2 <- .cdtEnv$tcl$fun$w.widgets(23)
		largeur3 <- 22
		largeur4 <- 14
		largeur5 <- 14
	}

	GeneralParameters <- list(data.type = "cdtstation",
							cdtstation = list(tx = "", tn = ""),
							cdtdataset = list(tx = "", tn = ""),
							baseYear = list(all.years = TRUE, start.year = 1981, end.year = 2010, min.year = 15, window = 2),
							Indices = list(TXx = TRUE, TXn = TRUE, TX10p = TRUE, TX90p = TRUE, WSDI = TRUE,
											SU = TRUE, ID = TRUE, TNx = TRUE, TNn = TRUE, TN10p = TRUE,
											TN90p = TRUE, CSDI = TRUE, TR = TRUE, FD = TRUE, DTR = TRUE,
											GSL = TRUE, thresGSL = 5, dayGSL = 6, upTX = 25, loTX = 0, upTN = 20, loTN = 0),
							start.july = FALSE, bootstrap = TRUE, output = "")

	# xml.dlg <- file.path(.cdtDir$dirLocal, "languages", "cdtCLIMDEX_TT_leftCmd.xml")
	# lang.dlg <- cdtLanguageParse(xml.dlg, .cdtData$Config$lang.iso)
	# .cdtData$EnvData$message <- lang.dlg[['message']]

	###################

	.cdtEnv$tcl$main$cmd.frame <- tkframe(.cdtEnv$tcl$main$panel.left)

	tknote.cmd <- bwNoteBook(.cdtEnv$tcl$main$cmd.frame)

	cmd.tab1 <- bwAddTab(tknote.cmd, text = "Input")
	cmd.tab2 <- bwAddTab(tknote.cmd, text = "Indices")
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

		frameInData <- ttklabelframe(subfr1, text = 'Input Data', relief = 'groove')

		DataType <- tclVar()
		CbdatatypeVAL <- .cdtEnv$tcl$lang$global[['combobox']][['2']][1:2]
		datatypeVAL <- c('cdtstation', 'cdtdataset')
		tclvalue(DataType) <- CbdatatypeVAL[datatypeVAL %in% GeneralParameters$data.type]

		if(GeneralParameters$data.type == 'cdtstation'){
			input.TX <- tclVar(GeneralParameters$cdtstation$tx)
			input.TN <- tclVar(GeneralParameters$cdtstation$tn)
			txt.INTX <- 'File containing stations daily Tmax data'
			txt.INTN <- 'File containing stations daily Tmin data'
		}else{
			input.TX <- tclVar(GeneralParameters$cdtdataset$tx)
			input.TN <- tclVar(GeneralParameters$cdtdataset$tn)
			txt.INTX <- 'Index file (*.rds) for daily Tmax data'
			txt.INTN <- 'Index file (*.rds) for daily Tmin data'
		}
		txt.INTX.var <- tclVar(txt.INTX)
		txt.INTN.var <- tclVar(txt.INTN)

		txt.datatype <- tklabel(frameInData, text = 'Format', anchor = 'w', justify = 'left')
		cb.datatype <- ttkcombobox(frameInData, values = CbdatatypeVAL, textvariable = DataType, width = largeur0)

		txt.INTX <- tklabel(frameInData, text = tclvalue(txt.INTX.var), textvariable = txt.INTX.var, anchor = 'w', justify = 'left')
		if(GeneralParameters$data.type == 'cdtstation'){
			cb.en.INTX <- ttkcombobox(frameInData, values = unlist(listOpenFiles), textvariable = input.TX, width = largeur1)
		}else{
			cb.en.INTX <- tkentry(frameInData, textvariable = input.TX, width = largeur2)
		}
		bt.INTX <- tkbutton(frameInData, text = "...")

		txt.INTN <- tklabel(frameInData, text = tclvalue(txt.INTN.var), textvariable = txt.INTN.var, anchor = 'w', justify = 'left')
		if(GeneralParameters$data.type == 'cdtstation'){
			cb.en.INTN <- ttkcombobox(frameInData, values = unlist(listOpenFiles), textvariable = input.TN, width = largeur1)
		}else{
			cb.en.INTN <- tkentry(frameInData, textvariable = input.TN, width = largeur2)
		}
		bt.INTN <- tkbutton(frameInData, text = "...")

		############

		tkconfigure(bt.INTX, command = function(){
			if(GeneralParameters$data.type == 'cdtstation'){
				dat.opfiles <- getOpenFiles(.cdtEnv$tcl$main$win)
				if(!is.null(dat.opfiles)){
					update.OpenFiles('ascii', dat.opfiles)
					listOpenFiles[[length(listOpenFiles) + 1]] <<- dat.opfiles[[1]]
					tclvalue(input.TX) <- dat.opfiles[[1]]
					lapply(list(cb.en.INTX, cb.en.INTN), tkconfigure, values = unlist(listOpenFiles))
				}
			}else{
				path.rds <- tclvalue(tkgetOpenFile(initialdir = getwd(), filetypes = .cdtEnv$tcl$data$filetypes6))
				tclvalue(input.TX) <- if(path.rds %in% c("", "NA") | is.na(path.rds)) "" else path.rds
			}
		})

		tkconfigure(bt.INTN, command = function(){
			if(GeneralParameters$data.type == 'cdtstation'){
				dat.opfiles <- getOpenFiles(.cdtEnv$tcl$main$win)
				if(!is.null(dat.opfiles)){
					update.OpenFiles('ascii', dat.opfiles)
					listOpenFiles[[length(listOpenFiles) + 1]] <<- dat.opfiles[[1]]
					tclvalue(input.TN) <- dat.opfiles[[1]]
					lapply(list(cb.en.INTX, cb.en.INTN), tkconfigure, values = unlist(listOpenFiles))
				}
			}else{
				path.rds <- tclvalue(tkgetOpenFile(initialdir = getwd(), filetypes = .cdtEnv$tcl$data$filetypes6))
				tclvalue(input.TN) <- if(path.rds %in% c("", "NA") | is.na(path.rds)) "" else path.rds
			}
		})

		############
		tkgrid(txt.datatype, row = 0, column = 0, sticky = 'we', rowspan = 1, columnspan = 2, padx = 1, pady = 1, ipadx = 1, ipady = 1)
		tkgrid(cb.datatype, row = 0, column = 2, sticky = 'we', rowspan = 1, columnspan = 8, padx = 1, pady = 1, ipadx = 1, ipady = 1)
		tkgrid(txt.INTX, row = 1, column = 0, sticky = 'we', rowspan = 1, columnspan = 10, padx = 1, pady = 1, ipadx = 1, ipady = 1)
		tkgrid(cb.en.INTX, row = 2, column = 0, sticky = 'we', rowspan = 1, columnspan = 9, padx = 0, pady = 1, ipadx = 1, ipady = 1)
		tkgrid(bt.INTX, row = 2, column = 9, sticky = 'w', rowspan = 1, columnspan = 1, padx = 0, pady = 1, ipadx = 1, ipady = 1)
		tkgrid(txt.INTN, row = 3, column = 0, sticky = 'we', rowspan = 1, columnspan = 10, padx = 1, pady = 1, ipadx = 1, ipady = 1)
		tkgrid(cb.en.INTN, row = 4, column = 0, sticky = 'we', rowspan = 1, columnspan = 9, padx = 0, pady = 1, ipadx = 1, ipady = 1)
		tkgrid(bt.INTN, row = 4, column = 9, sticky = 'w', rowspan = 1, columnspan = 1, padx = 0, pady = 1, ipadx = 1, ipady = 1)

		############

		helpWidget(cb.datatype, 'Select the type of input data', 'Select the type of input data')
		if(GeneralParameters$data.type == 'cdtstation'){
			helpWidget(cb.en.INTX, 'Select the file containing the daily maximum temperature data',
									'Select the file containing the daily maximum temperature data')
			helpWidget(cb.en.INTN, 'Select the file containing the daily minimum temperature data',
									'Select the file containing the daily minimum temperature data')
			helpWidget(bt.INTX, 'Browse file if not listed', 'Browse file if not listed')
			helpWidget(bt.INTN, 'Browse file if not listed', 'Browse file if not listed')
		}else{
			helpWidget(cb.en.INTX, 'Enter the full path to the file <daily maximum temperature dataset name>.rds',
									'Enter the full path to the file <daily maximum temperature dataset name>.rds')
			helpWidget(cb.en.INTN, 'Enter the full path to the file <daily minimum temperature dataset name>.rds',
									'Enter the full path to the file <daily minimum temperature dataset name>.rds')
			helpWidget(bt.INTX, 'or browse here', 'or browse here')
			helpWidget(bt.INTN, 'or browse here', 'or browse here')
		}

		############

		tkbind(cb.datatype, "<<ComboboxSelected>>", function(){
			tkdestroy(cb.en.INTX)
			tclvalue(input.TX) <- ''

			tkdestroy(cb.en.INTN)
			tclvalue(input.TN) <- ''

			###
			if(str_trim(tclvalue(DataType)) == CbdatatypeVAL[1]){
				tclvalue(txt.INTX.var) <- 'File containing stations daily Tmax data'
				tclvalue(txt.INTN.var) <- 'File containing stations daily Tmin data'

				cb.en.INTX <- ttkcombobox(frameInData, values = unlist(listOpenFiles), textvariable = input.TX, width = largeur1)
				cb.en.INTN <- ttkcombobox(frameInData, values = unlist(listOpenFiles), textvariable = input.TN, width = largeur1)

				######
				tkconfigure(bt.INTX, command = function(){
					dat.opfiles <- getOpenFiles(.cdtEnv$tcl$main$win)
					if(!is.null(dat.opfiles)){
						update.OpenFiles('ascii', dat.opfiles)
						listOpenFiles[[length(listOpenFiles) + 1]] <<- dat.opfiles[[1]]
						tclvalue(input.TX) <- dat.opfiles[[1]]
						lapply(list(cb.en.INTX, cb.en.INTN), tkconfigure, values = unlist(listOpenFiles))
					}
				})

				tkconfigure(bt.INTN, command = function(){
					dat.opfiles <- getOpenFiles(.cdtEnv$tcl$main$win)
					if(!is.null(dat.opfiles)){
						update.OpenFiles('ascii', dat.opfiles)
						listOpenFiles[[length(listOpenFiles) + 1]] <<- dat.opfiles[[1]]
						tclvalue(input.TN) <- dat.opfiles[[1]]
						lapply(list(cb.en.INTX, cb.en.INTN), tkconfigure, values = unlist(listOpenFiles))
					}
				})

				######
				helpWidget(cb.en.INTX, 'Select the file containing the daily maximum temperature data',
										'Select the file containing the daily maximum temperature data')
				helpWidget(cb.en.INTN, 'Select the file containing the daily minimum temperature data',
										'Select the file containing the daily minimum temperature data')
				helpWidget(bt.INTX, 'Browse file if not listed', 'Browse file if not listed')
				helpWidget(bt.INTN, 'Browse file if not listed', 'Browse file if not listed')
			}

			###
			if(str_trim(tclvalue(DataType)) == CbdatatypeVAL[2]){
				tclvalue(txt.INTX.var) <- 'Index file (*.rds) for daily Tmax data'
				tclvalue(txt.INTN.var) <- 'Index file (*.rds) for daily Tmin data'

				cb.en.INTX <- tkentry(frameInData, textvariable = input.TX, width = largeur2)
				cb.en.INTN <- tkentry(frameInData, textvariable = input.TN, width = largeur2)

				######
				tkconfigure(bt.INTX, command = function(){
					path.rds <- tclvalue(tkgetOpenFile(initialdir = getwd(), filetypes = .cdtEnv$tcl$data$filetypes6))
					tclvalue(input.TX) <- if(path.rds %in% c("", "NA")) "" else path.rds
				})

				tkconfigure(bt.INTN, command = function(){
					path.rds <- tclvalue(tkgetOpenFile(initialdir = getwd(), filetypes = .cdtEnv$tcl$data$filetypes6))
					tclvalue(input.TN) <- if(path.rds %in% c("", "NA")) "" else path.rds
				})

				######
				helpWidget(cb.en.INTX, 'Enter the full path to the file <daily maximum temperature dataset name>.rds',
										'Enter the full path to the file <daily maximum temperature dataset name>.rds')
				helpWidget(cb.en.INTN, 'Enter the full path to the file <daily minimum temperature dataset name>.rds',
										'Enter the full path to the file <daily minimum temperature dataset name>.rds')
				helpWidget(bt.INTX, 'or browse here', 'or browse here')
				helpWidget(bt.INTN, 'or browse here', 'or browse here')
			}

			#######
			tkgrid(cb.en.INTX, row = 2, column = 0, sticky = 'we', rowspan = 1, columnspan = 9, padx = 0, pady = 1, ipadx = 1, ipady = 1)
			tkgrid(cb.en.INTN, row = 4, column = 0, sticky = 'we', rowspan = 1, columnspan = 9, padx = 0, pady = 1, ipadx = 1, ipady = 1)
		})

		############################################

		frameDirSav <- tkframe(subfr1, relief = 'groove', borderwidth = 2)

		dir.save <- tclVar(GeneralParameters$output)

		txt.dir.save <- tklabel(frameDirSav, text = "Directory to save indices", anchor = 'w', justify = 'left')
		en.dir.save <- tkentry(frameDirSav, textvariable = dir.save, width = largeur2)
		bt.dir.save <- tkbutton(frameDirSav, text = "...")

		######
		tkconfigure(bt.dir.save, command = function() fileORdir2Save(dir.save, isFile = FALSE))

		######
		tkgrid(txt.dir.save, row = 0, column = 0, sticky = 'we', rowspan = 1, columnspan = 5, padx = 1, pady = 0, ipadx = 1, ipady = 1)
		tkgrid(en.dir.save, row = 1, column = 0, sticky = 'we', rowspan = 1, columnspan = 4, padx = 0, pady = 0, ipadx = 1, ipady = 1)
		tkgrid(bt.dir.save, row = 1, column = 4, sticky = 'w', rowspan = 1, columnspan = 1, padx = 0, pady = 0, ipadx = 1, ipady = 1)

		helpWidget(en.dir.save, 'Enter the full path to directory to save the calculated indices',
								'Enter the full path to directory to save the calculated indices')
		helpWidget(bt.dir.save, 'or browse here', 'or browse here')

		############################################

		bt.Baseyear <- ttkbutton(subfr1, text = "Set Base Period")
		fr.window <- tkframe(subfr1)

		######
		tkconfigure(bt.Baseyear, command = function(){
			GeneralParameters[["baseYear"]] <<- getInfoBasePeriod(.cdtEnv$tcl$main$win,
														GeneralParameters[["baseYear"]])
		})

		######
		dayWin <- tclVar(GeneralParameters$baseYear$window)

		txt.daywin1 <- tklabel(fr.window, text = "Centred time window", anchor = 'e', justify = 'right')
		en.daywin <- tkentry(fr.window, textvariable = dayWin, width = 3)
		txt.daywin2 <- tklabel(fr.window, text = "days", anchor = 'w', justify = 'left')

		tkgrid(txt.daywin1, en.daywin, txt.daywin2)

		helpWidget(en.daywin, 'The daily percentiles will be calculated using a centered (2 x window + 1) time window',
							'The daily percentiles will be calculated using a centered (2 x window + 1) time window')

		############################################

		tkgrid(frameInData, row = 0, column = 0, sticky = 'we', padx = 1, pady = 3, ipadx = 1, ipady = 1)
		tkgrid(frameDirSav, row = 1, column = 0, sticky = 'we', padx = 1, pady = 3, ipadx = 1, ipady = 1)
		tkgrid(bt.Baseyear, row = 2, column = 0, sticky = 'we', padx = 1, pady = 3, ipadx = 1, ipady = 1)
		tkgrid(fr.window, row = 3, column = 0, sticky = 'we', padx = 1, pady = 3, ipadx = 1, ipady = 1)

	#######################################################################################################

	#Tab2
	subfr2 <- bwTabScrollableFrame(cmd.tab2, hscrlwin = .cdtEnv$tcl$fun$h.scale(43))

	climdex.frame1 <- tkframe(cmd.tab2)
	tkgrid(climdex.frame1, sticky = 'we')
	climdex.frame2 <- tkframe(cmd.tab2)
	tkgrid(climdex.frame2)

		#######################

		frameIndex <- tkframe(subfr2, relief = 'sunken', borderwidth = 2)

		is.TXx <- tclVar(GeneralParameters$Indices$TXx)
		is.TXn <- tclVar(GeneralParameters$Indices$TXn)
		is.TX10p <- tclVar(GeneralParameters$Indices$TX10p)
		is.TX90p <- tclVar(GeneralParameters$Indices$TX90p)
		is.WSDI <- tclVar(GeneralParameters$Indices$WSDI)
		is.SU <- tclVar(GeneralParameters$Indices$SU)
		val.upTX <- tclVar(GeneralParameters$Indices$upTX)
		is.ID <- tclVar(GeneralParameters$Indices$ID)
		val.loTX <- tclVar(GeneralParameters$Indices$loTX)

		is.TNx <- tclVar(GeneralParameters$Indices$TNx)
		is.TNn <- tclVar(GeneralParameters$Indices$TNn)
		is.TN10p <- tclVar(GeneralParameters$Indices$TN10p)
		is.TN90p <- tclVar(GeneralParameters$Indices$TN90p)
		is.CSDI <- tclVar(GeneralParameters$Indices$CSDI)
		is.TR <- tclVar(GeneralParameters$Indices$TR)
		val.upTN <- tclVar(GeneralParameters$Indices$upTN)
		is.FD <- tclVar(GeneralParameters$Indices$FD)
		val.loTN <- tclVar(GeneralParameters$Indices$loTN)

		is.DTR <- tclVar(GeneralParameters$Indices$DTR)
		is.GSL <- tclVar(GeneralParameters$Indices$GSL)
		val.thresGSL <- tclVar(GeneralParameters$Indices$thresGSL)
		val.dayGSL <- tclVar(GeneralParameters$Indices$dayGSL)

		chk.TXn <- tkcheckbutton(frameIndex, variable = is.TXn, text = 'TXn: Monthly minimum value of TX', anchor = 'w', justify = 'left')
		chk.TXx <- tkcheckbutton(frameIndex, variable = is.TXx, text = 'TXx: Monthly maximum value of TX', anchor = 'w', justify = 'left')
		chk.TX10p <- tkcheckbutton(frameIndex, variable = is.TX10p, text = 'TX10p: Percentage of days when TX < 10th percentile', anchor = 'w', justify = 'left')
		chk.TX90p <- tkcheckbutton(frameIndex, variable = is.TX90p, text = 'TX90p: Percentage of days when TX > 90th percentile', anchor = 'w', justify = 'left')
		chk.WSDI <- tkcheckbutton(frameIndex, variable = is.WSDI, text = 'WSDI: Warm spell duration index', anchor = 'w', justify = 'left')

		chk.ID <- tkcheckbutton(frameIndex, variable = is.ID, text = 'ID: Number of icing days when TX < 0C (or user defined threshold)', anchor = 'w', justify = 'left')
		frameID <- tkframe(frameIndex)

		chk.SU <- tkcheckbutton(frameIndex, variable = is.SU, text = 'SU: Number of summer days when TX > 25C (or user defined threshold)', anchor = 'w', justify = 'left')
		frameSU <- tkframe(frameIndex)

		chk.TNn <- tkcheckbutton(frameIndex, variable = is.TNn, text = 'TNn: Monthly minimum value of TN', anchor = 'w', justify = 'left')
		chk.TNx <- tkcheckbutton(frameIndex, variable = is.TNx, text = 'TNx: Monthly maximum value of TN', anchor = 'w', justify = 'left')
		chk.TN10p <- tkcheckbutton(frameIndex, variable = is.TN10p, text = 'TN10p: Percentage of days when TN < 10th percentile', anchor = 'w', justify = 'left')
		chk.TN90p <- tkcheckbutton(frameIndex, variable = is.TN90p, text = 'TN90p: Percentage of days when TN > 90th percentile', anchor = 'w', justify = 'left')
		chk.CSDI <- tkcheckbutton(frameIndex, variable = is.CSDI, text = 'CSDI: Cold spell duration index', anchor = 'w', justify = 'left')

		chk.FD <- tkcheckbutton(frameIndex, variable = is.FD, text = 'FD: Number of frost days when TN < 0C (or user defined threshold)', anchor = 'w', justify = 'left')
		frameFD <- tkframe(frameIndex)

		chk.TR <- tkcheckbutton(frameIndex, variable = is.TR, text = 'TR: Number of tropical nights when TN > 20C (or user defined threshold)', anchor = 'w', justify = 'left')
		frameTR <- tkframe(frameIndex)

		chk.DTR <- tkcheckbutton(frameIndex, variable = is.DTR, text = 'DTR: Daily temperature range', anchor = 'w', justify = 'left')
		chk.GSL <- tkcheckbutton(frameIndex, variable = is.GSL, text = 'GSL: Growing season length', anchor = 'w', justify = 'left')
		frameGSL <- tkframe(frameIndex)

		################

		txt.ID <- tklabel(frameID, text = 'User defined lower threshold of TX', anchor = 'w', justify = 'left')
		en.ID <- tkentry(frameID, width = 4, textvariable = val.loTX, justify = "left")
		tkgrid(txt.ID, en.ID)

		txt.SU <- tklabel(frameSU, text = 'User defined upper threshold of TX', anchor = 'w', justify = 'left')
		en.SU <- tkentry(frameSU, width = 4, textvariable = val.upTX, justify = "left")
		tkgrid(txt.SU, en.SU)

		txt.FD <- tklabel(frameFD, text = 'User defined lower threshold of TN', anchor = 'w', justify = 'left')
		en.FD <- tkentry(frameFD, width = 4, textvariable = val.loTN, justify = "left")
		tkgrid(txt.FD, en.FD)

		txt.TR <- tklabel(frameTR, text = 'User defined upper threshold of TN', anchor = 'w', justify = 'left')
		en.TR <- tkentry(frameTR, width = 4, textvariable = val.upTN, justify = "left")
		tkgrid(txt.TR, en.TR)

		txt.GSL1 <- tklabel(frameGSL, text = 'GSL mean temperature threshold', anchor = 'e', justify = 'right')
		en.GSL1 <- tkentry(frameGSL, width = 4, textvariable = val.thresGSL, justify = "left")
		txt.GSL2 <- tklabel(frameGSL, text = 'GSL consecutive days', anchor = 'e', justify = 'right')
		en.GSL2 <- tkentry(frameGSL, width = 4, textvariable = val.dayGSL, justify = "left")
		tkgrid(txt.GSL1, en.GSL1)
		tkgrid(txt.GSL2, en.GSL2)

		################

		sep_indx1 <- ttkseparator(frameIndex)
		sep_indx2 <- ttkseparator(frameIndex)

		################

		tkgrid(chk.TXn, row = 0, column = 0, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
		tkgrid(chk.TXx, row = 1, column = 0, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
		tkgrid(chk.TX10p, row = 2, column = 0, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
		tkgrid(chk.TX90p, row = 3, column = 0, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
		tkgrid(chk.WSDI, row = 4, column = 0, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)

		tkgrid(chk.ID, row = 5, column = 0, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
		tkgrid(frameID, row = 6, column = 0, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)

		tkgrid(chk.SU, row = 7, column = 0, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
		tkgrid(frameSU, row = 8, column = 0, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)

		tkgrid(sep_indx1, row = 9, column = 0, sticky = 'we', rowspan = 1, columnspan = 1)

		tkgrid(chk.TNn, row = 10, column = 0, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
		tkgrid(chk.TNx, row = 11, column = 0, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
		tkgrid(chk.TN10p, row = 12, column = 0, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
		tkgrid(chk.TN90p, row = 13, column = 0, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
		tkgrid(chk.CSDI, row = 14, column = 0, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)

		tkgrid(chk.FD, row = 15, column = 0, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
		tkgrid(frameFD, row = 16, column = 0, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)

		tkgrid(chk.TR, row = 17, column = 0, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
		tkgrid(frameTR, row = 18, column = 0, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)

		tkgrid(sep_indx2, row = 19, column = 0, sticky = 'we', rowspan = 1, columnspan = 1)

		tkgrid(chk.DTR, row = 20, column = 0, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
		tkgrid(chk.GSL, row = 21, column = 0, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
		tkgrid(frameGSL, row = 22, column = 0, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)

		################

		helpWidget(frameIndex, "Check desired indices to calculate", "Check desired indices to calculate")

		#######################
		tkgrid(frameIndex, row = 0, column = 0, sticky = 'we', padx = 1, pady = 1, ipadx = 1, ipady = 1)
		tcl('update')

		##############################################

		start.july <- tclVar(GeneralParameters$start.july)
		bootstrap <- tclVar(GeneralParameters$bootstrap)

		chk.StartJul <- tkcheckbutton(climdex.frame1, variable = start.july, text = 'Southern Hemisphere: year starting on July', anchor = 'w', justify = 'left')
		chk.Bootstrap <- tkcheckbutton(climdex.frame1, variable = bootstrap, text = 'Use bootstrap resampling', anchor = 'w', justify = 'left')

		tkgrid(chk.StartJul, row = 0, column = 0, sticky = 'we', padx = 1, pady = 1, ipadx = 1, ipady = 1)
		tkgrid(chk.Bootstrap, row = 1, column = 0, sticky = 'we', padx = 1, pady = 1, ipadx = 1, ipady = 1)

		helpWidget(chk.Bootstrap, "Use a bootstrap procedure for the calculation of the base period",
								"Use a bootstrap procedure for the calculation of the base period")

		##############################################

		if(!is.null(.cdtData$EnvData$DirExist)){
			stateCaclBut <- if(tclvalue(.cdtData$EnvData$DirExist) == "1") "normal" else "disabled"
		}else stateCaclBut <- "normal"

		bt.CalcIndices <- ttkbutton(climdex.frame2, text = "Calculate Indices", state = stateCaclBut)

		tkconfigure(bt.CalcIndices, command = function(){
			GeneralParameters$data.type <- datatypeVAL[CbdatatypeVAL %in% str_trim(tclvalue(DataType))]

			if(str_trim(tclvalue(DataType)) == CbdatatypeVAL[1]){
				GeneralParameters$cdtstation$tx <- str_trim(tclvalue(input.TX))
				GeneralParameters$cdtstation$tn <- str_trim(tclvalue(input.TN))
			}

			if(str_trim(tclvalue(DataType)) == CbdatatypeVAL[2]){
				GeneralParameters$cdtdataset$tx <- str_trim(tclvalue(input.TX))
				GeneralParameters$cdtdataset$tn <- str_trim(tclvalue(input.TN))
			}

			GeneralParameters$output <- str_trim(tclvalue(dir.save))
			GeneralParameters$baseYear$window <- as.numeric(str_trim(tclvalue(dayWin)))

			GeneralParameters$Indices$TXx <- switch(tclvalue(is.TXx), '0' = FALSE, '1' = TRUE)
			GeneralParameters$Indices$TXn <- switch(tclvalue(is.TXn), '0' = FALSE, '1' = TRUE)
			GeneralParameters$Indices$TX10p <- switch(tclvalue(is.TX10p), '0' = FALSE, '1' = TRUE)
			GeneralParameters$Indices$TX90p <- switch(tclvalue(is.TX90p), '0' = FALSE, '1' = TRUE)
			GeneralParameters$Indices$WSDI <- switch(tclvalue(is.WSDI), '0' = FALSE, '1' = TRUE)
			GeneralParameters$Indices$SU <- switch(tclvalue(is.SU), '0' = FALSE, '1' = TRUE)
			GeneralParameters$Indices$upTX <- as.numeric(str_trim(tclvalue(val.upTX)))
			GeneralParameters$Indices$ID <- switch(tclvalue(is.ID), '0' = FALSE, '1' = TRUE)
			GeneralParameters$Indices$loTX <- as.numeric(str_trim(tclvalue(val.loTX)))

			GeneralParameters$Indices$TNx <- switch(tclvalue(is.TNx), '0' = FALSE, '1' = TRUE)
			GeneralParameters$Indices$TNn <- switch(tclvalue(is.TNn), '0' = FALSE, '1' = TRUE)
			GeneralParameters$Indices$TN10p <- switch(tclvalue(is.TN10p), '0' = FALSE, '1' = TRUE)
			GeneralParameters$Indices$TN90p <- switch(tclvalue(is.TN90p), '0' = FALSE, '1' = TRUE)
			GeneralParameters$Indices$CSDI <- switch(tclvalue(is.CSDI), '0' = FALSE, '1' = TRUE)
			GeneralParameters$Indices$TR <- switch(tclvalue(is.TR), '0' = FALSE, '1' = TRUE)
			GeneralParameters$Indices$upTN <- as.numeric(str_trim(tclvalue(val.upTN)))
			GeneralParameters$Indices$FD <- switch(tclvalue(is.FD), '0' = FALSE, '1' = TRUE)
			GeneralParameters$Indices$loTN <- as.numeric(str_trim(tclvalue(val.loTN)))

			GeneralParameters$Indices$DTR <- switch(tclvalue(is.DTR), '0' = FALSE, '1' = TRUE)
			GeneralParameters$Indices$GSL <- switch(tclvalue(is.GSL), '0' = FALSE, '1' = TRUE)
			GeneralParameters$Indices$thresGSL <- as.numeric(str_trim(tclvalue(val.thresGSL)))
			GeneralParameters$Indices$dayGSL <- as.numeric(str_trim(tclvalue(val.dayGSL)))

			GeneralParameters$start.july <- switch(tclvalue(start.july), '0' = FALSE, '1' = TRUE)
			GeneralParameters$bootstrap <- switch(tclvalue(bootstrap), '0' = FALSE, '1' = TRUE)

			# assign("GeneralParameters", GeneralParameters, envir = .GlobalEnv)

			Insert.Messages.Out("Calculating Indices (this may take some time) .......")

			tkconfigure(.cdtEnv$tcl$main$win, cursor = 'watch')
			tcl('update')
			ret <- tryCatch(
				{
					climdexCalc.TT(GeneralParameters)
				},
				warning = function(w) warningFun(w),
				error = function(e) errorFun(e),
				finally = {
					tkconfigure(.cdtEnv$tcl$main$win, cursor = '')
					tcl('update')
				}
			)

			if(!is.null(ret)){
				if(ret == 0){
					Insert.Messages.Out("Indices calculation finished successfully")
					## set
					.cdtData$EnvData$plot.maps$data.type <- .cdtData$EnvData$output$params$data.type
					.cdtData$EnvData$plot.maps[c('lon', 'lat', 'id')] <- .cdtData$EnvData$output$data[c('lon', 'lat', 'id')]
					.cdtData$EnvData$indices.data <- "TT"
					###################
					ret <- try(set.vars.dates(), silent = TRUE)
					if(inherits(ret, "try-error") | is.null(ret)) return(NULL)

					widgets.Station.Pixel()
					set.plot.type()
					set.trend.vars()

					ret1 <- try(get.data.Trend(), silent = TRUE)
					if(inherits(ret1, "try-error") | is.null(ret1)) return(NULL)
					ret2 <- try(get.data.Year(), silent = TRUE)
					if(inherits(ret2, "try-error") | is.null(ret2)) return(NULL)
				}else Insert.Messages.Out("Indices calculation failed", format = TRUE)
			}else Insert.Messages.Out("Indices calculation failed", format = TRUE)
		})

		#######################
		tkgrid(bt.CalcIndices, row = 1, column = 0, sticky = 'we', padx = 1, pady = 1, ipadx = 1, ipady = 1)

	#######################################################################################################

	#Tab3
	subfr3 <- bwTabScrollableFrame(cmd.tab3)

		#######################

		frameDataExist <- ttklabelframe(subfr3, text = "CLIMDEX data", relief = 'groove')

		.cdtData$EnvData$DirExist <- tclVar(0)
		file.dataIndex <- tclVar()

		stateExistData <- if(tclvalue(.cdtData$EnvData$DirExist) == "1") "normal" else "disabled"

		chk.dataIdx <- tkcheckbutton(frameDataExist, variable = .cdtData$EnvData$DirExist, text = "CLIMDEX data already computed", anchor = 'w', justify = 'left')
		en.dataIdx <- tkentry(frameDataExist, textvariable = file.dataIndex, width = largeur2, state = stateExistData)
		bt.dataIdx <- tkbutton(frameDataExist, text = "...", state = stateExistData)

		tkconfigure(bt.dataIdx, command = function(){
			path.dataIdx <- tclvalue(tkgetOpenFile(initialdir = getwd(), filetypes = .cdtEnv$tcl$data$filetypes6))
			if(path.dataIdx %in% c("", "NA") | is.na(path.dataIdx)) return(NULL)
			tclvalue(file.dataIndex) <- path.dataIdx

			if(file.exists(str_trim(tclvalue(file.dataIndex)))){
				OutIndexdata <- try(readRDS(str_trim(tclvalue(file.dataIndex))), silent = TRUE)
				if(inherits(OutIndexdata, "try-error")){
					Insert.Messages.Out('Unable to load CLIMDEX data', format = TRUE)
					Insert.Messages.Out(gsub('[\r\n]', '', OutIndexdata[1]), format = TRUE)

					tkconfigure(cb.varstat.var, values = "")
					tclvalue(.cdtData$EnvData$anaVars) <- ""
					tkconfigure(cb.varstat.stat, values = "")
					tclvalue(.cdtData$EnvData$anaStat) <- ""
					tkconfigure(cb.data.Index, values = "")
					tclvalue(.cdtData$EnvData$donDate) <- ""
					return(NULL)
				}

				.cdtData$EnvData$output <- OutIndexdata
				.cdtData$EnvData$PathData <- dirname(str_trim(tclvalue(file.dataIndex)))
				.cdtData$EnvData$plot.maps$data.type <- .cdtData$EnvData$output$params$data.type
				.cdtData$EnvData$plot.maps[c('lon', 'lat', 'id')] <- .cdtData$EnvData$output$data[c('lon', 'lat', 'id')]
				.cdtData$EnvData$indices.data <- "TT"
				###################
				ret <- try(set.vars.dates(), silent = TRUE)
				if(inherits(ret, "try-error") | is.null(ret)) return(NULL)

				widgets.Station.Pixel()
				set.plot.type()
				set.trend.vars()

				ret1 <- try(get.data.Trend(), silent = TRUE)
				if(inherits(ret1, "try-error") | is.null(ret1)) return(NULL)
				ret2 <- try(get.data.Year(), silent = TRUE)
				if(inherits(ret2, "try-error") | is.null(ret2)) return(NULL)
			}
		})

		tkgrid(chk.dataIdx, row = 0, column = 0, sticky = 'we', rowspan = 1, columnspan = 5, padx = 1, pady = 1, ipadx = 1, ipady = 1)
		tkgrid(en.dataIdx, row = 1, column = 0, sticky = 'we', rowspan = 1, columnspan = 4, padx = 1, pady = 1, ipadx = 1, ipady = 1)
		tkgrid(bt.dataIdx, row = 1, column = 4, sticky = 'w', rowspan = 1, columnspan = 1, padx = 0, pady = 1, ipadx = 1, ipady = 1)

		###############
		tkbind(chk.dataIdx, "<Button-1>", function(){
			stateExistData <- if(tclvalue(.cdtData$EnvData$DirExist) == '1') 'disabled' else 'normal'
			tkconfigure(en.dataIdx, state = stateExistData)
			tkconfigure(bt.dataIdx, state = stateExistData)
			stateCaclBut <- if(tclvalue(.cdtData$EnvData$DirExist) == '1') 'normal' else 'disabled'
			tkconfigure(bt.CalcIndices, state = stateCaclBut)
		})

		##############################################

		frameDataStatMap <- ttklabelframe(subfr3, text = "Statistics Maps", relief = 'groove')

		.cdtData$EnvData$anaVars <- tclVar()
		.cdtData$EnvData$anaStat <- tclVar()

		cb.varstat.var <- ttkcombobox(frameDataStatMap, values = "", textvariable = .cdtData$EnvData$anaVars, width = largeur3)
		bt.varstat.maps <- ttkbutton(frameDataStatMap, text = .cdtEnv$tcl$lang$global[['button']][['3']])
		cb.varstat.stat <- ttkcombobox(frameDataStatMap, values = "", textvariable = .cdtData$EnvData$anaStat, width = largeur3)
		bt.varstat.MapOpt <- ttkbutton(frameDataStatMap, text = .cdtEnv$tcl$lang$global[['button']][['4']])

		###################

		.cdtData$EnvData$tab$pointSize.MapStat <- NULL
		.cdtData$EnvData$varstatMapOp <- list(presetCol = list(color = 'tim.colors', reverse = FALSE),
											userCol = list(custom = FALSE, color = NULL),
											userLvl = list(custom = FALSE, levels = NULL, equidist = FALSE),
											title = list(user = FALSE, title = ''),
											colkeyLab = list(user = FALSE, label = ''),
											scalebar = list(add = FALSE, pos = 'bottomleft'),
											pointSize = .cdtData$EnvData$tab$pointSize.MapStat)

		tkconfigure(bt.varstat.MapOpt, command = function(){
			if(!is.null(.cdtData$EnvData$varData$map)){
				atlevel <- pretty(.cdtData$EnvData$varData$map$z, n = 10, min.n = 7)
				if(is.null(.cdtData$EnvData$varstatMapOp$userLvl$levels)){
					.cdtData$EnvData$varstatMapOp$userLvl$levels <- atlevel
				}else{
					if(!.cdtData$EnvData$varstatMapOp$userLvl$custom)
						.cdtData$EnvData$varstatMapOp$userLvl$levels <- atlevel
				}
			}
			.cdtData$EnvData$varstatMapOp <- MapGraph.MapOptions(.cdtData$EnvData$varstatMapOp)

			if(str_trim(tclvalue(.cdtData$EnvData$plot.maps$plot.type)) == "Points")
				.cdtData$EnvData$tab$pointSize.MapStat <- .cdtData$EnvData$varstatMapOp$pointSize
		})

		###################

		.cdtData$EnvData$tab$dataMapStat <- NULL

		tkconfigure(bt.varstat.maps, command = function(){
			if(str_trim(tclvalue(.cdtData$EnvData$anaVars)) != "" &
				str_trim(tclvalue(.cdtData$EnvData$anaStat)) != "")
			{
				ret <- try(get.data.Trend(), silent = TRUE)
				if(inherits(ret, "try-error") | is.null(ret)) return(NULL)
				Climdex.Display.MapsTrend()
			}
		})

		###################

		tkgrid(cb.varstat.var, row = 0, column = 0, sticky = 'we', rowspan = 1, columnspan = 4, padx = 1, pady = 1, ipadx = 1, ipady = 1)
		tkgrid(bt.varstat.maps, row = 0, column = 4, sticky = '', rowspan = 1, columnspan = 2, padx = 1, pady = 1, ipadx = 1, ipady = 1)
		tkgrid(cb.varstat.stat, row = 1, column = 0, sticky = 'we', rowspan = 1, columnspan = 2, padx = 1, pady = 1, ipadx = 1, ipady = 1)
		tkgrid(bt.varstat.MapOpt, row = 1, column = 4, sticky = 'we', rowspan = 1, columnspan = 2, padx = 1, pady = 1, ipadx = 1, ipady = 1)

		###################

		tkbind(cb.varstat.var, "<<ComboboxSelected>>", function(){
			if(!is.null(.cdtData$EnvData$output) &
				str_trim(tclvalue(.cdtData$EnvData$anaVars)) != "")
			{
				if(str_trim(tclvalue(.cdtData$EnvData$anaVars)) == "GSL")
					YEARS <- .cdtData$EnvData$output$year.gsl
				else
					YEARS <- .cdtData$EnvData$output$year

				tkconfigure(cb.data.Index, values = YEARS)
				tclvalue(.cdtData$EnvData$donDate) <- YEARS[length(YEARS)]
			}

			########
			if(str_trim(tclvalue(.cdtData$EnvData$anaVars)) != "" &
				str_trim(tclvalue(.cdtData$EnvData$anaStat)) != "")
			{
				ret1 <- try(get.data.Trend(), silent = TRUE)
				if(inherits(ret1, "try-error") | is.null(ret1)) return(NULL)
			}

			########
			if(!is.null(.cdtData$EnvData$YearData)){
				ret2 <- try(get.data.Year(), silent = TRUE)
				if(inherits(ret2, "try-error") | is.null(ret2)) return(NULL)
			}
		})

		##############################################

		frameDataMap <- ttklabelframe(subfr3, text = "Yearly Maps", relief = 'groove')

		.cdtData$EnvData$donDate <- tclVar()

		cb.data.Index <- ttkcombobox(frameDataMap, values = "", textvariable = .cdtData$EnvData$donDate, width = largeur4, justify = 'center')
		bt.data.Index.prev <- ttkbutton(frameDataMap, text = "<<", width = 3)
		bt.data.Index.next <- ttkbutton(frameDataMap, text = ">>", width = 3)
		bt.data.maps <- ttkbutton(frameDataMap, text = .cdtEnv$tcl$lang$global[['button']][['3']], width = 7)
		bt.data.MapOpt <- ttkbutton(frameDataMap, text = .cdtEnv$tcl$lang$global[['button']][['4']], width = 7)

		###############

		.cdtData$EnvData$tab$pointSize.MapTS <- NULL
		.cdtData$EnvData$dataMapOp <- list(presetCol = list(color = 'tim.colors', reverse = FALSE),
										userCol = list(custom = FALSE, color = NULL),
										userLvl = list(custom = FALSE, levels = NULL, equidist = FALSE),
										title = list(user = FALSE, title = ''),
										colkeyLab = list(user = FALSE, label = ''),
										scalebar = list(add = FALSE, pos = 'bottomleft'),
										pointSize = .cdtData$EnvData$tab$pointSize.MapTS)

		tkconfigure(bt.data.MapOpt, command = function(){
			if(!is.null(.cdtData$EnvData$varData$map)){
				atlevel <- pretty(.cdtData$EnvData$varData$map$z, n = 10, min.n = 7)
				if(is.null(.cdtData$EnvData$dataMapOp$userLvl$levels)){
					.cdtData$EnvData$dataMapOp$userLvl$levels <- atlevel
				}else{
					if(!.cdtData$EnvData$dataMapOp$userLvl$custom)
						.cdtData$EnvData$dataMapOp$userLvl$levels <- atlevel
				}
			}
			.cdtData$EnvData$dataMapOp <- MapGraph.MapOptions(.cdtData$EnvData$dataMapOp)

			if(str_trim(tclvalue(.cdtData$EnvData$plot.maps$plot.type)) == "Points")
				.cdtData$EnvData$tab$pointSize.MapTS <- .cdtData$EnvData$dataMapOp$pointSize
		})

		###############

		.cdtData$EnvData$tab$dataMapTS <- NULL

		tkconfigure(bt.data.maps, command = function(){
			if(str_trim(tclvalue(.cdtData$EnvData$donDate)) != "" &
				!is.null(.cdtData$EnvData$YearData))
					Climdex.Display.MapYear()
		})

		tkconfigure(bt.data.Index.prev, command = function(){
			if(str_trim(tclvalue(.cdtData$EnvData$donDate)) != ""){
				donDates <- .cdtData$EnvData$output$year
				idaty <- which(donDates == str_trim(tclvalue(.cdtData$EnvData$donDate)))
				idaty <- idaty - 1
				if(idaty < 1) idaty <- length(donDates)
				tclvalue(.cdtData$EnvData$donDate) <- donDates[idaty]

				ret <- try(get.data.Year(), silent = TRUE)
				if(inherits(ret, "try-error") | is.null(ret)) return(NULL)

				Climdex.Display.MapYear()
			}
		})

		tkconfigure(bt.data.Index.next, command = function(){
			if(str_trim(tclvalue(.cdtData$EnvData$donDate)) != ""){
				donDates <- .cdtData$EnvData$output$year
				idaty <- which(donDates == str_trim(tclvalue(.cdtData$EnvData$donDate)))
				idaty <- idaty + 1
				if(idaty > length(donDates)) idaty <- 1
				tclvalue(.cdtData$EnvData$donDate) <- donDates[idaty]

				ret <- try(get.data.Year(), silent = TRUE)
				if(inherits(ret, "try-error") | is.null(ret)) return(NULL)

				Climdex.Display.MapYear()
			}
		})

		###############

		tkgrid(bt.data.Index.prev, row = 0, column = 0, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
		tkgrid(cb.data.Index, row = 0, column = 1, sticky = 'we', rowspan = 1, columnspan = 2, padx = 1, pady = 1, ipadx = 1, ipady = 1)
		tkgrid(bt.data.Index.next, row = 0, column = 3, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
		tkgrid(bt.data.maps, row = 0, column = 4, sticky = 'we', rowspan = 1, columnspan = 2, padx = 1, pady = 1, ipadx = 1, ipady = 1)
		tkgrid(bt.data.MapOpt, row = 1, column = 4, sticky = 'we', rowspan = 1, columnspan = 2, padx = 1, pady = 1, ipadx = 1, ipady = 1)

		###############

		tkbind(cb.data.Index, "<<ComboboxSelected>>", function(){
			if(!is.null(.cdtData$EnvData$YearData)){
				ret <- try(get.data.Year(), silent = TRUE)
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
			if(str_trim(tclvalue(.cdtData$EnvData$anaVars)) != "" &
				str_trim(tclvalue(.cdtData$EnvData$anaStat)) != "")
			{
				ret1 <- try(get.data.Trend(), silent = TRUE)
				if(inherits(ret1, "try-error") | is.null(ret1)) return(NULL)
			}

			########
			if(!is.null(.cdtData$EnvData$YearData)){
				ret2 <- try(get.data.Year(), silent = TRUE)
				if(inherits(ret2, "try-error") | is.null(ret2)) return(NULL)
			}
		})

		##############################################

		tkgrid(frameDataExist, row = 0, column = 0, sticky = 'we', padx = 1, pady = 1, ipadx = 1, ipady = 1)
		tkgrid(frameDataStatMap, row = 1, column = 0, sticky = 'we', padx = 1, pady = 3, ipadx = 1, ipady = 1)
		tkgrid(frameDataMap, row = 2, column = 0, sticky = 'we', padx = 1, pady = 1, ipadx = 1, ipady = 1)
		tkgrid(framePlotType, row = 3, column = 0, sticky = '', padx = 1, pady = 3, ipadx = 1, ipady = 1)

	#######################################################################################################

	#Tab4
	subfr4 <- bwTabScrollableFrame(cmd.tab4)

		#######################

		frameDataTS <- ttklabelframe(subfr4, text = "CLIMDEX Graph", relief = 'groove')

		typeTSPLOT <- c("Line", "Barplot")
		.cdtData$EnvData$plot.maps$typeTSp <- tclVar("Line")

		cb.typeTSp <- ttkcombobox(frameDataTS, values = typeTSPLOT, textvariable = .cdtData$EnvData$plot.maps$typeTSp, width = largeur4)
		bt.TsGraph.plot <- ttkbutton(frameDataTS, text = .cdtEnv$tcl$lang$global[['button']][['3']], width = 7)
		bt.TSGraphOpt <- ttkbutton(frameDataTS, text = .cdtEnv$tcl$lang$global[['button']][['4']], width = 8)

		#################

		.cdtData$EnvData$TSGraphOp <- list(
							bar = list(
								xlim = list(is.min = FALSE, min = 1981, is.max = FALSE, max = 2017),
								ylim = list(is.min = FALSE, min = 0, is.max = FALSE, max = 100),
								axislabs = list(is.xlab = FALSE, xlab = '', is.ylab = FALSE, ylab = ''),
								title = list(is.title = FALSE, title = '', position = 'top'),
								colors = list(col = "darkblue")
							),
							line = list(
								xlim = list(is.min = FALSE, min = 1981, is.max = FALSE, max = 2017),
								ylim = list(is.min = FALSE, min = 0, is.max = FALSE, max = 100),
								axislabs = list(is.xlab = FALSE, xlab = '', is.ylab = FALSE, ylab = ''),
								title = list(is.title = FALSE, title = '', position = 'top'),
								plot = list(type = 'both',
									col = list(line = "red", points = "blue"),
									lwd = 2, cex = 1.4),
								legend = list(
									col = list(lowess = "blue", linear = "black"),
									text = list(lowess = "Lowess smoother", linear = "Linear Trend"),
									lwd = list(lowess = 2, linear = 2),
									lty= list(lowess = 2, linear = 1))
							)
						)

		tkconfigure(bt.TSGraphOpt, command = function(){
			suffix.fun <- switch(str_trim(tclvalue(.cdtData$EnvData$plot.maps$typeTSp)),
									"Barplot" = "Bar",
									"Line" = "LineCLIMDEX")
			plot.fun <- get(paste0("MapGraph.GraphOptions.", suffix.fun), mode = "function")
			.cdtData$EnvData$TSGraphOp <- plot.fun(.cdtData$EnvData$TSGraphOp)
		})

		#################

		.cdtData$EnvData$tab$dataGraph <- NULL

		tkconfigure(bt.TsGraph.plot, command = function(){
			if(!is.null(.cdtData$EnvData$YearData)){
				imgContainer <- CDT.Display.Graph(Climdex.plotVarGraph, .cdtData$EnvData$tab$dataGraph, 'Climdex-Graph')
				.cdtData$EnvData$tab$dataGraph <- imageNotebookTab_unik(imgContainer, .cdtData$EnvData$tab$dataGraph)
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

		tkgrid(frameDataTS, row = 0, column = 0, sticky = 'we', pady = 1)
		tkgrid(frameSTNCrds, row = 1, column = 0, sticky = '', pady = 3)


	#######################################################################################################

	#Tab5
	subfr5 <- bwTabScrollableFrame(cmd.tab5)

		#######################

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
			txt.stnSel <- tklabel(frTS2, text = "Select a station to plot")
			bt.stnID.prev <- ttkbutton(frTS2, text = "<<", width = 6)
			bt.stnID.next <- ttkbutton(frTS2, text = ">>", width = 6)
			cb.stnID <- ttkcombobox(frTS2, values = stnIDTSPLOT, textvariable = .cdtData$EnvData$plot.maps$stnIDTSp, width = largeur5)
			tclvalue(.cdtData$EnvData$plot.maps$stnIDTSp) <- stnIDTSPLOT[1]

			tkconfigure(bt.stnID.prev, command = function(){
				if(!is.null(.cdtData$EnvData$YearData)){
					istn <- which(stnIDTSPLOT == str_trim(tclvalue(.cdtData$EnvData$plot.maps$stnIDTSp)))
					istn <- istn - 1
					if(istn < 1) istn <- length(stnIDTSPLOT)
					tclvalue(.cdtData$EnvData$plot.maps$stnIDTSp) <- stnIDTSPLOT[istn]

					imgContainer <- CDT.Display.Graph(Climdex.plotVarGraph, .cdtData$EnvData$tab$dataGraph, 'Climdex-Graph')
					.cdtData$EnvData$tab$dataGraph <- imageNotebookTab_unik(imgContainer, .cdtData$EnvData$tab$dataGraph)
				}
			})

			tkconfigure(bt.stnID.next, command = function(){
				if(!is.null(.cdtData$EnvData$YearData)){
					istn <- which(stnIDTSPLOT == str_trim(tclvalue(.cdtData$EnvData$plot.maps$stnIDTSp)))
					istn <- istn + 1
					if(istn > length(stnIDTSPLOT)) istn <- 1
					tclvalue(.cdtData$EnvData$plot.maps$stnIDTSp) <- stnIDTSPLOT[istn]

					imgContainer <- CDT.Display.Graph(Climdex.plotVarGraph, .cdtData$EnvData$tab$dataGraph, 'Climdex-Graph')
					.cdtData$EnvData$tab$dataGraph <- imageNotebookTab_unik(imgContainer, .cdtData$EnvData$tab$dataGraph)
				}
			})

			tkgrid(txt.stnSel, row = 0, column = 0, sticky = '', rowspan = 1, columnspan = 3, padx = 1, pady = 1, ipadx = 1, ipady = 1)
			tkgrid(bt.stnID.prev, row = 1, column = 0, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
			tkgrid(cb.stnID, row = 1, column = 1, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
			tkgrid(bt.stnID.next, row = 1, column = 2, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
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

	###################

	set.plot.type <- function(){
		if(.cdtData$EnvData$output$params$data.type == "cdtstation")
		{
			plot.type <- c("Pixels", "Points")
			.cdtData$EnvData$plot.maps$.data.type <- "Points"

			.cdtData$EnvData$varstatMapOp$pointSize <- 0.7
			.cdtData$EnvData$dataMapOp$pointSize <- 0.7
		}else{
			plot.type <- c("Pixels", "FilledContour")
			.cdtData$EnvData$plot.maps$.data.type <- "Grid"
		}
		tkconfigure(cb.plotType, values = plot.type)
	}

	###################

	set.trend.vars <- function(){
		.cdtData$EnvData$trend$name <- c("slope", "std.slope", "t.value.slope", "p.value.slope",
										"intercept", "std.intercept", "t.value.intercept",
										"p.value.intercept", "R2", "sigma")
		.cdtData$EnvData$trend$longname <- c(
											"Slope - Estimate", "Slope - Standard Error", "Slope t-value", "Slope p-value Pr(>t)",
											"Intercept - Estimate", "Intercept - Standard Error", "Intercept t-value", "Intercept p-value Pr(>t)",
											"Multiple R-squared", "Residual Standard Error"
											)
		tkconfigure(cb.varstat.stat, values = .cdtData$EnvData$trend$longname)
		tclvalue(.cdtData$EnvData$anaStat) <- .cdtData$EnvData$trend$longname[1]
	}

	###################

	set.vars.dates <- function(){
		indxlst <- c("TXx", "TXn", "TX10p", "TX90p", "WSDI", "SU", "ID",
					"TNx", "TNn", "TN10p", "TN90p", "CSDI", "TR", "FD",
					"DTR", "GSL")

		var.dir.path <- file.path(.cdtData$EnvData$PathData, 'CDTDATASET')
		indxexist <- list.files(var.dir.path)
		indices <- indxexist[indxexist %in% indxlst]
		if(length(indices) == 0){
			Insert.Messages.Out('No Indices data found', format = TRUE)
			return(NULL)
		}

		tkconfigure(cb.varstat.var, values = indices)
		tclvalue(.cdtData$EnvData$anaVars) <- indices[1]

		if(indices[1] == "GSL")
			YEARS <- .cdtData$EnvData$output$year.gsl
		else
			YEARS <- .cdtData$EnvData$output$year

		tkconfigure(cb.data.Index, values = YEARS)
		tclvalue(.cdtData$EnvData$donDate) <- YEARS[length(YEARS)]
		return(0)
	}

	#######################################################################################################


	get.data.Trend <- function(){
		tkconfigure(.cdtEnv$tcl$main$win, cursor = 'watch')
		tcl('update')
		on.exit({
			tkconfigure(.cdtEnv$tcl$main$win, cursor = '')
			tcl('update')
		})

		this.vars <- str_trim(tclvalue(.cdtData$EnvData$anaVars))
		this.trend <- str_trim(tclvalue(.cdtData$EnvData$anaStat))
		ipos <- which(.cdtData$EnvData$trend$longname %in% this.trend)

		readTrendData <- TRUE
		if(!is.null(.cdtData$EnvData$TrendData))
			if(!is.null(.cdtData$EnvData$TrendData$this.vars))
				if(.cdtData$EnvData$TrendData$this.vars == this.vars) readTrendData <- FALSE

		if(.cdtData$EnvData$output$params$data.type == "cdtstation"){
			if(readTrendData){
				filetrend <- file.path(.cdtData$EnvData$PathData, "CDTDATASET", this.vars, "Trend", paste0(this.vars, '.rds'))
				if(!file.exists(filetrend)){
					Insert.Messages.Out(paste(filetrend, "not found"), format = TRUE)
					return(NULL)
				}

				.cdtData$EnvData$TrendData$data <- readRDS(filetrend)
				.cdtData$EnvData$TrendData$this.vars <- this.vars
			}

			#####
			change.plot <- str_trim(tclvalue(.cdtData$EnvData$plot.maps$plot.type))

			getTrend <- TRUE
			if(!is.null(.cdtData$EnvData$TrendData$this.trend))
				if(.cdtData$EnvData$TrendData$this.trend == this.trend) getTrend <- FALSE

			if(!getTrend)
				if(.cdtData$EnvData$change.plot.trend != change.plot) getTrend <- TRUE

			if(getTrend){
				x <- .cdtData$EnvData$output$data$lon
				y <- .cdtData$EnvData$output$data$lat
				tmp <- as.numeric(.cdtData$EnvData$TrendData$data[ipos, ])
				if(change.plot == "Pixels"){
					nx <- nx_ny_as.image(diff(range(x)))
					ny <- nx_ny_as.image(diff(range(y)))
					tmp <- cdt.as.image(tmp, nx = nx, ny = ny, pts.xy = cbind(x, y))
					.cdtData$EnvData$TrendData$map$x <- tmp$x
					.cdtData$EnvData$TrendData$map$y <- tmp$y
					.cdtData$EnvData$TrendData$map$z <- tmp$z
				}

				if(change.plot == "Points"){
					.cdtData$EnvData$TrendData$map$x <- x
					.cdtData$EnvData$TrendData$map$y <- y
					.cdtData$EnvData$TrendData$map$z <- tmp
				}

				.cdtData$EnvData$TrendData$this.trend <- this.trend
				.cdtData$EnvData$change.plot.trend <- change.plot
				rm(tmp, x, y)
			}
		}else{
			if(readTrendData){
				filetrend <- file.path(.cdtData$EnvData$PathData, "DATA_NetCDF", this.vars, "Trend", paste0(this.vars, '.nc'))
				if(!file.exists(filetrend)){
					Insert.Messages.Out(paste(filetrend, "not found"), format = TRUE)
					return(NULL)
				}

				nc <- nc_open(filetrend)
				.cdtData$EnvData$TrendData$data <- lapply(.cdtData$EnvData$trend$name, function(varid) ncvar_get(nc, varid = varid))
				nc_close(nc)
				.cdtData$EnvData$TrendData$this.vars <- this.vars
			}

			.cdtData$EnvData$TrendData$map$x <- .cdtData$EnvData$output$data$x
			.cdtData$EnvData$TrendData$map$y <- .cdtData$EnvData$output$data$y
			.cdtData$EnvData$TrendData$map$z <- .cdtData$EnvData$TrendData$data[[ipos]]

			###################
			file.Index.Trend <- file.path(.cdtData$EnvData$PathData, "CDTDATASET", this.vars, "Trend", paste0(this.vars, '.rds'))

			read.cdt.dataTrend <- TRUE
			if(!is.null(.cdtData$EnvData$cdtdataTrend))
				if(!is.null(.cdtData$EnvData$file.Index.Trend))
					if(.cdtData$EnvData$file.Index.Trend == file.Index.Trend) read.cdt.dataTrend <- FALSE
			if(read.cdt.dataTrend){
				.cdtData$EnvData$cdtdataTrend <- readRDS(file.Index.Trend)
				.cdtData$EnvData$cdtdataTrend$fileInfo <- file.Index.Trend
				.cdtData$EnvData$file.Index.Trend <- file.Index.Trend
			}
		}

		return(0)
	}

	###################

	get.data.Year <- function(){
		tkconfigure(.cdtEnv$tcl$main$win, cursor = 'watch')
		tcl('update')
		on.exit({
			tkconfigure(.cdtEnv$tcl$main$win, cursor = '')
			tcl('update')
		})

		this.vars <- str_trim(tclvalue(.cdtData$EnvData$anaVars))
		this.year <- str_trim(tclvalue(.cdtData$EnvData$donDate))

		if(.cdtData$EnvData$output$params$data.type == "cdtstation"){
			fileyear <- file.path(.cdtData$EnvData$PathData, "CDTDATASET", this.vars,
									"Yearly", paste0(this.vars, '.rds'))

			readYearData <- TRUE
			if(!is.null(.cdtData$EnvData$YearData))
				if(!is.null(.cdtData$EnvData$YearData$fileyear))
					if(.cdtData$EnvData$YearData$fileyear == fileyear) readYearData <- FALSE

			if(readYearData){
				if(!file.exists(fileyear)){
					Insert.Messages.Out(paste(fileyear, "not found"), format = TRUE)
					return(NULL)
				}

				.cdtData$EnvData$YearData$data <- readRDS(fileyear)
				.cdtData$EnvData$YearData$fileyear <- fileyear
			}

			#####
			change.plot <- str_trim(tclvalue(.cdtData$EnvData$plot.maps$plot.type))

			getYEAR <- TRUE
			if(!is.null(.cdtData$EnvData$YearData$this.year))
				if(.cdtData$EnvData$YearData$this.year == this.year) getYEAR <- FALSE

			if(!getYEAR)
				if(.cdtData$EnvData$change.plot.year != change.plot) getYEAR <- TRUE

			if(getYEAR){
				if(this.vars == "GSL")
					YEARS <- .cdtData$EnvData$output$year.gsl
				else
					YEARS <- .cdtData$EnvData$output$year

				ipos <- which(YEARS %in% this.year)
				x <- .cdtData$EnvData$output$data$lon
				y <- .cdtData$EnvData$output$data$lat
				tmp <- as.numeric(.cdtData$EnvData$YearData$data[ipos, ])
				if(change.plot == "Pixels"){
					nx <- nx_ny_as.image(diff(range(x)))
					ny <- nx_ny_as.image(diff(range(y)))
					tmp <- cdt.as.image(tmp, nx = nx, ny = ny, pts.xy = cbind(x, y))
					.cdtData$EnvData$YearData$map$x <- tmp$x
					.cdtData$EnvData$YearData$map$y <- tmp$y
					.cdtData$EnvData$YearData$map$z <- tmp$z
				}

				if(change.plot == "Points"){
					.cdtData$EnvData$YearData$map$x <- x
					.cdtData$EnvData$YearData$map$y <- y
					.cdtData$EnvData$YearData$map$z <- tmp
				}

				.cdtData$EnvData$YearData$this.year <- this.year
				.cdtData$EnvData$change.plot.year <- change.plot
				rm(tmp, x, y)
			}
		}else{
			fileyear <- file.path(.cdtData$EnvData$PathData, "DATA_NetCDF", this.vars,
									"Yearly", paste0(this.vars, "_", this.year, '.nc'))

			readYearData <- TRUE
			if(!is.null(.cdtData$EnvData$YearData))
				if(!is.null(.cdtData$EnvData$YearData$fileyear))
					if(.cdtData$EnvData$YearData$fileyear == fileyear) readYearData <- FALSE

			if(readYearData){
				if(!file.exists(fileyear)){
					Insert.Messages.Out(paste(fileyear, "not found"), format = TRUE)
					return(NULL)
				}

				nc <- nc_open(fileyear)
				.cdtData$EnvData$YearData$map$x <- nc$dim[[1]]$vals
				.cdtData$EnvData$YearData$map$y <- nc$dim[[2]]$vals
				.cdtData$EnvData$YearData$map$z <- ncvar_get(nc, varid = nc$var[[1]]$name)
				nc_close(nc)

				.cdtData$EnvData$YearData$fileyear <- fileyear
			}

			###################
			file.Index.Year <- file.path(.cdtData$EnvData$PathData, "CDTDATASET", this.vars, 'Yearly', paste0(this.vars, '.rds'))

			read.cdt.dataYear <- TRUE
			if(!is.null(.cdtData$EnvData$cdtdataYear))
				if(!is.null(.cdtData$EnvData$file.Index.Year))
					if(.cdtData$EnvData$file.Index.Year == file.Index.Year) read.cdt.dataYear <- FALSE
			if(read.cdt.dataYear){
				.cdtData$EnvData$cdtdataYear <- readRDS(file.Index.Year)
				.cdtData$EnvData$cdtdataYear$fileInfo <- file.Index.Year
				.cdtData$EnvData$file.Index.Year <- file.Index.Year
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
