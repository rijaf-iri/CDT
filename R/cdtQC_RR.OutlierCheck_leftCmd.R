
qcRROutlierCheckPanelCmd <- function(){
	listOpenFiles <- openFile_ttkcomboList()
	if(WindowsOS()){
		largeur0 <- .cdtEnv$tcl$fun$w.widgets(18)
		largeur1 <- .cdtEnv$tcl$fun$w.widgets(27)
		largeur2 <- .cdtEnv$tcl$fun$w.widgets(29)
		largeur3 <- 19
		largeur4 <- 24
	}else{
		largeur0 <- .cdtEnv$tcl$fun$w.widgets(16)
		largeur1 <- .cdtEnv$tcl$fun$w.widgets(21)
		largeur2 <- .cdtEnv$tcl$fun$w.widgets(23)
		largeur3 <- 12
		largeur4 <- 19
	}

	GeneralParameters <- list(intstep = "daily", infile = "", outdir = "",
							elv = list(use = FALSE, dem = TRUE, file = ""),
							params = list(
								voisin = list(min = 4, max = 15, dist = 25, elv = 800),
								precip.max = 300, conf.lev = 99.73,
								spatial = list(ispmax = 1, ispobs = 10, isdmin = 3,
												isdobs = 1, isdq1 = 10, iqrf = 2.8)
								)
							)

	MOIS <- format(ISOdate(2014, 1:12, 1), "%B")

	# xml.dlg <- file.path(.cdtDir$dirLocal, "languages", "cdtQC_RR.OutlierCheck_leftCmd.xml")
	# lang.dlg <- cdtLanguageParse(xml.dlg, .cdtData$Config$lang.iso)
	# .cdtData$EnvData$message <- lang.dlg[['message']]

	###################

	.cdtEnv$tcl$main$cmd.frame <- tkframe(.cdtEnv$tcl$main$panel.left)

	tknote.cmd <- bwNoteBook(.cdtEnv$tcl$main$cmd.frame)
	cmd.tab1 <- bwAddTab(tknote.cmd, text = "Data")
	cmd.tab2 <- bwAddTab(tknote.cmd, text = "QC Outputs")
	cmd.tab3 <- bwAddTab(tknote.cmd, text = "Zoom")
	cmd.tab4 <- bwAddTab(tknote.cmd, text = "Add Layers")

	bwRaiseTab(tknote.cmd, cmd.tab1)

	tkgrid.columnconfigure(cmd.tab1, 0, weight = 1)
	tkgrid.columnconfigure(cmd.tab2, 0, weight = 1)
	tkgrid.columnconfigure(cmd.tab3, 0, weight = 1)
	tkgrid.columnconfigure(cmd.tab4, 0, weight = 1)

	tkgrid.rowconfigure(cmd.tab1, 0, weight = 1)
	tkgrid.rowconfigure(cmd.tab2, 0, weight = 1)
	tkgrid.rowconfigure(cmd.tab3, 0, weight = 1)
	tkgrid.rowconfigure(cmd.tab4, 0, weight = 1)

	#######################################################################################################

	#Tab1
	subfr1 <- bwTabScrollableFrame(cmd.tab1)

		##############################################

		frameTimeS <- ttklabelframe(subfr1, text = "Time step of input data", relief = 'groove')

		timeSteps <- tclVar()
		CbperiodVAL <- .cdtEnv$tcl$lang$global[['combobox']][['1']][2:5]
		periodVAL <- c('daily', 'pentad', 'dekadal', 'monthly')
		tclvalue(timeSteps) <- CbperiodVAL[periodVAL %in% GeneralParameters$intstep]

		cb.fperiod <- ttkcombobox(frameTimeS, values = CbperiodVAL, textvariable = timeSteps, width = largeur0)

		tkgrid(cb.fperiod, row = 0, column = 0, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)

		helpWidget(cb.fperiod, 'Select the time step of the data', 'Select the time step of the data')

		#######################

		frameInData <- ttklabelframe(subfr1, text = "Station Data", relief = 'groove')

		input.file <- tclVar(GeneralParameters$infile)

		txt.infile <- tklabel(frameInData, text = 'File containing stations input data', anchor = 'w', justify = 'left')
		cb.infile <- ttkcombobox(frameInData, values = unlist(listOpenFiles), textvariable = input.file, width = largeur1)
		bt.infile <- tkbutton(frameInData, text = "...")

		tkconfigure(bt.infile, command = function(){
			dat.opfiles <- getOpenFiles(.cdtEnv$tcl$main$win)
			if(!is.null(dat.opfiles)){
				update.OpenFiles('ascii', dat.opfiles)
				listOpenFiles[[length(listOpenFiles) + 1]] <<- dat.opfiles[[1]]
				tclvalue(input.file) <- dat.opfiles[[1]]
				tkconfigure(cb.infile, values = unlist(listOpenFiles))
			}
		})

		tkgrid(txt.infile, row = 0, column = 0, sticky = 'we', rowspan = 1, columnspan = 5, padx = 1, pady = 1, ipadx = 1, ipady = 1)
		tkgrid(cb.infile, row = 1, column = 0, sticky = 'we', rowspan = 1, columnspan = 4, padx = 0, pady = 1, ipadx = 1, ipady = 1)
		tkgrid(bt.infile, row = 1, column = 4, sticky = 'w', rowspan = 1, columnspan = 1, padx = 0, pady = 1, ipadx = 1, ipady = 1)

		helpWidget(cb.infile, 'Select the file containing the input data', 'Select the file containing the input data')
		helpWidget(bt.infile, 'Browse file if not listed', 'Browse file if not listed')

		#######################

		frameParams <- tkframe(subfr1, relief = 'sunken', borderwidth = 2, padx = 3, pady = 3)

		use.elev <- tclVar(GeneralParameters$elv$use)

		chk.elev <- tkcheckbutton(frameParams, variable = use.elev, text = "Use Elevation", anchor = 'w', justify = 'left')
		bt.elev <- ttkbutton(frameParams, text = .cdtEnv$tcl$lang$global[['button']][['5']], state = "disabled")
		bt.params <- ttkbutton(frameParams, text = "Quality Control Parameters")

		tkconfigure(bt.elev, command = function(){
			Params <- GeneralParameters[["elv"]]
			GeneralParameters[["elv"]] <<- getParams.QC.Elevation(Params)
		})

		tkconfigure(bt.params, command = function(){
			Params <- GeneralParameters[["params"]]
			GeneralParameters[["params"]] <<- getParams.RR.OutlierCheck(Params)
		})

		tkgrid(chk.elev, row = 0, column = 0, sticky = 'we', rowspan = 1, columnspan = 2, padx = 1, pady = 1, ipadx = 1, ipady = 1)
		tkgrid(bt.elev, row = 0, column = 2, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
		tkgrid(bt.params, row = 1, column = 0, sticky = 'we', rowspan = 1, columnspan = 3, padx = 1, pady = 1, ipadx = 1, ipady = 1)

		helpWidget(chk.elev, 'Check this box if you want to use elevation data to select neighbors stations',
							'Check this box if you want to use elevation data to select neighbors stations')
		helpWidget(bt.elev, 'Select the elevation data to be used', 'Select the elevation data to be used')
		helpWidget(bt.params, 'Set the parameters of the outliers check', 'Set the parameters of the outliers check')

		###############
		tkbind(chk.elev, "<Button-1>", function(){
			if(tclvalue(QCExist) == '0'){
				stateElev <- if(tclvalue(use.elev) == '1') 'disabled' else 'normal'
				tkconfigure(bt.elev, state = stateElev)
			}
		})

		#######################

		frameDirSav <- tkframe(subfr1, relief = 'groove', borderwidth = 2)

		dir.save <- tclVar(GeneralParameters$outdir)

		txt.dir.save <- tklabel(frameDirSav, text = "Directory to save the outputs", anchor = 'w', justify = 'left')
		en.dir.save <- tkentry(frameDirSav, textvariable = dir.save, width = largeur2)
		bt.dir.save <- tkbutton(frameDirSav, text = "...")

		######
		tkconfigure(bt.dir.save, command = function() fileORdir2Save(dir.save, isFile = FALSE))

		######
		tkgrid(txt.dir.save, row = 0, column = 0, sticky = 'we', rowspan = 1, columnspan = 5, padx = 1, pady = 0, ipadx = 1, ipady = 1)
		tkgrid(en.dir.save, row = 1, column = 0, sticky = 'we', rowspan = 1, columnspan = 4, padx = 0, pady = 0, ipadx = 1, ipady = 1)
		tkgrid(bt.dir.save, row = 1, column = 4, sticky = 'w', rowspan = 1, columnspan = 1, padx = 0, pady = 0, ipadx = 1, ipady = 1)

		helpWidget(en.dir.save, 'Enter the full path to directory to save outputs',
								'Enter the full path to directory to save outputs')
		helpWidget(bt.dir.save, 'or browse here', 'or browse here')

		#############################

		bt.doQC <- ttkbutton(subfr1, text = "Check Outliers")

		tkconfigure(bt.doQC, command = function(){
			GeneralParameters$intstep <- periodVAL[CbperiodVAL %in% str_trim(tclvalue(timeSteps))]
			GeneralParameters$infile <- str_trim(tclvalue(input.file))
			GeneralParameters$outdir <- str_trim(tclvalue(dir.save))
			GeneralParameters$elv$use <- switch(tclvalue(use.elev), '0' = FALSE, '1' = TRUE)

			# assign('GeneralParameters', GeneralParameters, envir = .GlobalEnv)

			Insert.Messages.Out("Check outliers ......")

			tkconfigure(.cdtEnv$tcl$main$win, cursor = 'watch')
			tcl('update')
			ret <- tryCatch(
				{
					qcRROutliersCheckProcs(GeneralParameters)
				},
				warning = function(w) warningFun(w),
				error = function(e) errorFun(e),
				finally = {
					tkconfigure(.cdtEnv$tcl$main$win, cursor = '')
					tcl('update')
				}
			)

			msg0 <- "Outliers check finished successfully"
			msg1 <- "Outliers check failed"

			if(!is.null(ret)){
				if(ret == 0){
					Insert.Messages.Out(msg0)

					if(is.null(.cdtData$EnvData$outqc)){
						Insert.Messages.Out('No suspicious values found')
						return(NULL)
					}

					coords <- .cdtData$EnvData$stn.data[c('lon', 'lat', 'id')]
					.cdtData$EnvData$plot.maps[c('lon', 'lat', 'id')] <- coords

					###############
					set.station.id()
					ret <- try(set.date.outliers(), silent = TRUE)
					if(inherits(ret, "try-error") | is.null(ret)) return(NULL)
					set.initialize.Zoom()
				}else Insert.Messages.Out(msg1, format = TRUE)
			}else Insert.Messages.Out(msg1, format = TRUE)
		})

		#########################################

		tkgrid(frameTimeS, row = 0, column = 0, sticky = '', padx = 1, pady = 1, ipadx = 1, ipady = 1)
		tkgrid(frameInData, row = 1, column = 0, sticky = 'we', padx = 1, pady = 1, ipadx = 1, ipady = 1)
		tkgrid(frameParams, row = 2, column = 0, sticky = '', padx = 1, pady = 1, ipadx = 1, ipady = 1)
		tkgrid(frameDirSav, row = 3, column = 0, sticky = 'we', padx = 1, pady = 1, ipadx = 1, ipady = 1)
		tkgrid(bt.doQC, row = 4, column = 0, sticky = '', padx = 1, pady = 3, ipadx = 1, ipady = 1)

	#######################################################################################################

	#Tab2
	subfr2 <- bwTabScrollableFrame(cmd.tab2)

		##############################################

		frameOutQC <- ttklabelframe(subfr2, text = "QC Data", relief = 'groove')

		QCExist <- tclVar(0)
		file.dataIndex <- tclVar()

		chk.dataIdx <- tkcheckbutton(frameOutQC, variable = QCExist, text = "Load existing QC session", anchor = 'w', justify = 'left')
		en.dataIdx <- tkentry(frameOutQC, textvariable = file.dataIndex, width = largeur2, state = "disabled")
		bt.dataIdx <- tkbutton(frameOutQC, text = "...", state = "disabled")

		tkconfigure(bt.dataIdx, command = function(){
			path.dataIdx <- tclvalue(tkgetOpenFile(initialdir = getwd(), filetypes = .cdtEnv$tcl$data$filetypes6))
			if(path.dataIdx %in% c("", "NA") | is.na(path.dataIdx)) return(NULL)
			tclvalue(file.dataIndex) <- path.dataIdx

			if(file.exists(str_trim(tclvalue(file.dataIndex)))){
				OutQC <- try(readRDS(str_trim(tclvalue(file.dataIndex))), silent = TRUE)
				if(inherits(OutQC, "try-error")){
					Insert.Messages.Out('Unable to load QC output data', format = TRUE)
					Insert.Messages.Out(gsub('[\r\n]', '', OutQC[1]), format = TRUE)
					tkconfigure(.cdtData$EnvData$STN$cb.stnID, values = "")
					tclvalue(.cdtData$EnvData$STN$stnID) <- ""
					tkconfigure(.cdtData$EnvData$STN$cb.QCSP, values = "")
					tclvalue(.cdtData$EnvData$STN$dateSP) <- ""
					return(NULL)
				}

				.cdtData$EnvData$output <- OutQC
				.cdtData$EnvData$PathData <- dirname(str_trim(tclvalue(file.dataIndex)))

				###############
				file.checkd <- file.path(.cdtData$EnvData$PathData, 'CDTDATASET', "QCResults.rds")
				if(!file.exists(file.checkd)){
					Insert.Messages.Out(paste(file.checkd, 'not found'), format = TRUE)
					return(NULL)
				}
				.cdtData$EnvData$outqc <- readRDS(file.checkd)
				if(is.null(.cdtData$EnvData$outqc)){
					Insert.Messages.Out('No suspicious values found')
					return(NULL)
				}

				###############
				file.don <- file.path(.cdtData$EnvData$PathData, 'CDTDATASET', "StationData.rds")
				if(!file.exists(file.don)){
					Insert.Messages.Out(paste(file.don, 'not found'), format = TRUE)
					return(NULL)
				}
				.cdtData$EnvData$stn.data <- readRDS(file.don)

				coords <- .cdtData$EnvData$stn.data[c('lon', 'lat', 'id')]
				.cdtData$EnvData$plot.maps[c('lon', 'lat', 'id')] <- coords

				###############
				set.station.id()
				ret <- try(set.date.outliers(), silent = TRUE)
				if(inherits(ret, "try-error") | is.null(ret)) return(NULL)
				set.initialize.Zoom()
			}
		})

		tkgrid(chk.dataIdx, row = 0, column = 0, sticky = 'we', rowspan = 1, columnspan = 5, padx = 1, pady = 1, ipadx = 1, ipady = 1)
		tkgrid(en.dataIdx, row = 1, column = 0, sticky = 'we', rowspan = 1, columnspan = 4, padx = 1, pady = 1, ipadx = 1, ipady = 1)
		tkgrid(bt.dataIdx, row = 1, column = 4, sticky = 'w', rowspan = 1, columnspan = 1, padx = 0, pady = 1, ipadx = 1, ipady = 1)

		###############
		tkbind(chk.dataIdx, "<Button-1>", function(){
			stateExistData <- if(tclvalue(QCExist) == '1') 'disabled' else 'normal'
			tkconfigure(en.dataIdx, state = stateExistData)
			tkconfigure(bt.dataIdx, state = stateExistData)

			stateQC <- if(tclvalue(QCExist) == '1') 'normal' else 'disabled'
			tkconfigure(cb.fperiod, state = stateQC)
			tkconfigure(cb.infile, state = stateQC)
			tkconfigure(bt.infile, state = stateQC)
			tkconfigure(bt.params, state = stateQC)
			tkconfigure(en.dir.save, state = stateQC)
			tkconfigure(bt.dir.save, state = stateQC)
			tkconfigure(bt.doQC, state = stateQC)

			tkconfigure(chk.elev, state = stateQC)
			if(tclvalue(QCExist) == '1'){
				stateElev <- if(tclvalue(use.elev) == '0') 'disabled' else 'normal'
			}else stateElev <- 'disabled'
			tkconfigure(bt.elev, state = stateElev)
		})

		#######################

		frameStnId <- ttklabelframe(subfr2, text = "Select station", relief = 'groove')

		.cdtData$EnvData$STN$stnID <- tclVar()

		bt.stnID.prev <- ttkbutton(frameStnId, text = "<<", width = 5)
		bt.stnID.next <- ttkbutton(frameStnId, text = ">>", width = 5)
		.cdtData$EnvData$STN$cb.stnID <- ttkcombobox(frameStnId, values = "", textvariable = .cdtData$EnvData$STN$stnID, width = largeur4, justify = 'center')

		tkconfigure(bt.stnID.prev, command = function(){
			if(!is.null(.cdtData$EnvData$outqc)){
				STNID <- .cdtData$EnvData$outqc$stn
				istn <- which(STNID == str_trim(tclvalue(.cdtData$EnvData$STN$stnID)))
				istn <- istn - 1
				if(istn < 1) istn <- length(STNID)
				tclvalue(.cdtData$EnvData$STN$stnID) <- STNID[istn]

				ret <- try(set.date.outliers(), silent = TRUE)
				if(inherits(ret, "try-error") | is.null(ret)) return(NULL)
			}
		})

		tkconfigure(bt.stnID.next, command = function(){
			if(!is.null(.cdtData$EnvData$outqc)){
				STNID <- .cdtData$EnvData$outqc$stn
				istn <- which(STNID == str_trim(tclvalue(.cdtData$EnvData$STN$stnID)))
				istn <- istn + 1
				if(istn > length(STNID)) istn <- 1
				tclvalue(.cdtData$EnvData$STN$stnID) <- STNID[istn]

				ret <- try(set.date.outliers(), silent = TRUE)
				if(inherits(ret, "try-error") | is.null(ret)) return(NULL)
			}
		})

		tkgrid(bt.stnID.prev, row = 0, column = 0, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 2, ipadx = 1, ipady = 1)
		tkgrid(.cdtData$EnvData$STN$cb.stnID, row = 0, column = 1, sticky = 'we', rowspan = 1, columnspan = 2, padx = 1, pady = 2, ipadx = 1, ipady = 1)
		tkgrid(bt.stnID.next, row = 0, column = 3, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 2, ipadx = 1, ipady = 1)

		tkbind(.cdtData$EnvData$STN$cb.stnID, "<<ComboboxSelected>>", function(){
			if(!is.null(.cdtData$EnvData$outqc)){
				ret <- try(set.date.outliers(), silent = TRUE)
				if(inherits(ret, "try-error") | is.null(ret)) return(NULL)
			}
		})

		#######################

		bt.display.QC <- ttkbutton(subfr2, text = "View & Edit Result")

		.cdtData$EnvData$tab$TableStat <- NULL

		tkconfigure(bt.display.QC, command = function(){
			stnid <- str_trim(tclvalue(.cdtData$EnvData$STN$stnID))
			if(stnid == "") return(NULL)
			donQCstat <- .cdtData$EnvData$outqc$res[[stnid]]$outliers
			tab.title <- paste0(stnid, "QC-Output")

			.cdtData$EnvData$tab$TableStat <- tableNotebookTab_unik(donQCstat,
																.cdtData$EnvData$tab$TableStat,
																tab.title, 12, 'outqc')
			tabid <- as.integer(tclvalue(tkindex(.cdtEnv$tcl$main$tknotes, 'current'))) + 1
			table1 <- .cdtData$OpenTab$Data[[tabid]][[2]][[1]]

			.Tcl(paste(table1, 'tag', 'celltag', 'ttnreplace',
				paste(1:as.integer(tclvalue(tkindex(table1, 'end', 'row'))), 8, sep = ',', collapse = ' ')))
			tcl(table1, "tag", "configure", "ttnreplace", bg = "lightgoldenrod1", fg = "blue", anchor = "c")

			.Tcl(paste(table1, 'tag', 'celltag', 'ttchgval',
				paste(1:as.integer(tclvalue(tkindex(table1, 'end', 'row'))), 9, sep = ',', collapse = ' ')))
			tcl(table1, "tag", "configure", "ttchgval", bg = "darkolivegreen1", fg = "red", anchor = "c")
		})

		#######################

		frameStnMon <- ttklabelframe(subfr2, text = 'Plot Outliers by month', relief = 'groove')

		.cdtData$EnvData$STN$month <- tclVar(MOIS[1])

		bt.Mon.prev <- ttkbutton(frameStnMon, text = "<<", width = 4)
		bt.Mon.next <- ttkbutton(frameStnMon, text = ">>", width = 4)
		bt.Mon <- ttkbutton(frameStnMon, text = .cdtEnv$tcl$lang$global[['button']][['3']], width = 7)
		cb.Mon <- ttkcombobox(frameStnMon, values = MOIS, textvariable = .cdtData$EnvData$STN$month, width = largeur3, justify = 'center')

		.cdtData$EnvData$tab$PlotMon <- NULL
		.cdtData$EnvData$tab$ylabMon <- "Precipitation [mm]"

		tkconfigure(bt.Mon.prev, command = function(){
			imois <- which(MOIS == str_trim(tclvalue(.cdtData$EnvData$STN$month)))
			imois <- imois - 1
			if(imois < 1) imois <- 12
			tclvalue(.cdtData$EnvData$STN$month) <- MOIS[imois]

			stnid <- str_trim(tclvalue(.cdtData$EnvData$STN$stnID))
			if(stnid == "") return(NULL)
			tab.title <- paste0(stnid, "-Outliers")

			imgContainer <- qcDislpay_Outliers.Mon(.cdtData$EnvData$tab$PlotMon, tab.title)
			.cdtData$EnvData$tab$PlotMon <- imageNotebookTab_unik(imgContainer, .cdtData$EnvData$tab$PlotMon)
		})

		tkconfigure(bt.Mon.next, command = function(){
			imois <- which(MOIS == str_trim(tclvalue(.cdtData$EnvData$STN$month)))
			imois <- imois + 1
			if(imois > 12) imois <- 1
			tclvalue(.cdtData$EnvData$STN$month) <- MOIS[imois]

			stnid <- str_trim(tclvalue(.cdtData$EnvData$STN$stnID))
			if(stnid == "") return(NULL)
			tab.title <- paste0(stnid, "-Outliers")

			imgContainer <- qcDislpay_Outliers.Mon(.cdtData$EnvData$tab$PlotMon, tab.title)
			.cdtData$EnvData$tab$PlotMon <- imageNotebookTab_unik(imgContainer, .cdtData$EnvData$tab$PlotMon)
		})

		tkconfigure(bt.Mon, command = function(){
			stnid <- str_trim(tclvalue(.cdtData$EnvData$STN$stnID))
			if(stnid == "") return(NULL)
			tab.title <- paste0(stnid, "-Outliers")

			imgContainer <- qcDislpay_Outliers.Mon(.cdtData$EnvData$tab$PlotMon, tab.title)
			.cdtData$EnvData$tab$PlotMon <- imageNotebookTab_unik(imgContainer, .cdtData$EnvData$tab$PlotMon)
		})

		tkgrid(bt.Mon.prev, row = 0, column = 0, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 2, ipadx = 1, ipady = 1)
		tkgrid(cb.Mon, row = 0, column = 1, sticky = 'we', rowspan = 1, columnspan = 2, padx = 1, pady = 2, ipadx = 1, ipady = 1)
		tkgrid(bt.Mon.next, row = 0, column = 3, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 2, ipadx = 1, ipady = 1)
		tkgrid(bt.Mon, row = 0, column = 4, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 2, ipadx = 1, ipady = 1)

		helpWidget(frameStnMon, 'Plot outliers for each month', 'Plot outliers for each month')

		#######################

		frameQCSP <- ttklabelframe(subfr2, text = 'Plot Spatial Check', relief = 'groove')

		.cdtData$EnvData$STN$dateSP <- tclVar()

		bt.QCSP.prev <- ttkbutton(frameQCSP, text = "<<", width = 4)
		bt.QCSP.next <- ttkbutton(frameQCSP, text = ">>", width = 4)
		.cdtData$EnvData$STN$cb.QCSP <- ttkcombobox(frameQCSP, values = "", textvariable = .cdtData$EnvData$STN$dateSP, width = largeur3, justify = 'center')
		bt.QCSP.plot <- ttkbutton(frameQCSP, text = .cdtEnv$tcl$lang$global[['button']][['3']], width = 7)
		bt.QCSP.opt <- ttkbutton(frameQCSP, text = .cdtEnv$tcl$lang$global[['button']][['4']], width = 7, state = "disabled")

		.cdtData$EnvData$STN$Opt <- list(
										stn = list(col = "blue", bg = 'red', pch = 23, cex = 1, txt.col = 'red'),
										vois = list(col = 'red', pch = 20, cex = 0.7, txt.col = 'blue'),
										all = list(col = 'darkred', pch = 20, cex = 0.7, txt.col = 'blue'),
										circle = list(draw = TRUE, lwd = 1.5, col = 'red')
									)

		.cdtData$EnvData$tab$spoutliers <- NULL

		tkconfigure(bt.QCSP.prev, command = function(){
			idaty <- str_trim(tclvalue(.cdtData$EnvData$STN$dateSP))
			if(idaty == "") return(NULL)
			stnid <- str_trim(tclvalue(.cdtData$EnvData$STN$stnID))
			if(stnid == "") return(NULL)

			dateSP <- .cdtData$EnvData$outqc$res[[stnid]]$date
			idaty <- which(dateSP == idaty)
			idaty <- idaty - 1
			if(idaty < 1) idaty <- length(dateSP)
			tclvalue(.cdtData$EnvData$STN$dateSP) <- dateSP[idaty]

			tab.title <- paste0(stnid, "-Spatial.Check")
			imgContainer <- CDT.Display.Points.Zoom(qcPlot_Spatial.Check, .cdtData$EnvData$tab$spoutliers, tab.title)
			.cdtData$EnvData$tab$spoutliers <- imageNotebookTab_unik(imgContainer, .cdtData$EnvData$tab$spoutliers)
		})

		tkconfigure(bt.QCSP.next, command = function(){
			idaty <- str_trim(tclvalue(.cdtData$EnvData$STN$dateSP))
			if(idaty == "") return(NULL)
			stnid <- str_trim(tclvalue(.cdtData$EnvData$STN$stnID))
			if(stnid == "") return(NULL)

			dateSP <- .cdtData$EnvData$outqc$res[[stnid]]$date
			idaty <- which(dateSP == idaty)
			idaty <- idaty + 1
			if(idaty > length(dateSP)) idaty <- 1
			tclvalue(.cdtData$EnvData$STN$dateSP) <- dateSP[idaty]

			tab.title <- paste0(stnid, "-Spatial.Check")
			imgContainer <- CDT.Display.Points.Zoom(qcPlot_Spatial.Check, .cdtData$EnvData$tab$spoutliers, tab.title)
			.cdtData$EnvData$tab$spoutliers <- imageNotebookTab_unik(imgContainer, .cdtData$EnvData$tab$spoutliers)
		})

		tkconfigure(bt.QCSP.plot, command = function(){
			idaty <- str_trim(tclvalue(.cdtData$EnvData$STN$dateSP))
			if(idaty == "") return(NULL)
			stnid <- str_trim(tclvalue(.cdtData$EnvData$STN$stnID))
			if(stnid == "") return(NULL)

			tab.title <- paste0(stnid, "-Spatial.Check")
			imgContainer <- CDT.Display.Points.Zoom(qcPlot_Spatial.Check, .cdtData$EnvData$tab$spoutliers, tab.title)
			.cdtData$EnvData$tab$spoutliers <- imageNotebookTab_unik(imgContainer, .cdtData$EnvData$tab$spoutliers)
		})

		tkgrid(bt.QCSP.prev, row = 0, column = 0, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 2, ipadx = 1, ipady = 1)
		tkgrid(.cdtData$EnvData$STN$cb.QCSP, row = 0, column = 1, sticky = 'we', rowspan = 1, columnspan = 2, padx = 1, pady = 2, ipadx = 1, ipady = 1)
		tkgrid(bt.QCSP.next, row = 0, column = 3, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 2, ipadx = 1, ipady = 1)
		tkgrid(bt.QCSP.plot, row = 0, column = 4, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 2, ipadx = 1, ipady = 1)
		tkgrid(bt.QCSP.opt, row = 1, column = 4, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 2, ipadx = 1, ipady = 1)

		helpWidget(frameQCSP, 'Display map of outliers', 'Display map of outliers')

		#######################

		bt.replace.QC <- ttkbutton(subfr2, text = "Replace Outliers")

		tkconfigure(bt.replace.QC, command = function(){
			if(is.null(.cdtData$EnvData$outqc)) return(NULL)

			is.elv <- if(is.null(.cdtData$EnvData$stn.data$elv)) 3 else 4
			info <- .cdtData$EnvData$output$info[[3]]

			file.stn <- file.path(.cdtData$EnvData$PathData, 'CDTSTATIONS', .cdtData$EnvData$output$info[[1]])
			tmp <- fread(file.stn, header = FALSE, sep = info$sepr, stringsAsFactors = FALSE, colClasses = "character")

			stnid <- str_trim(tclvalue(.cdtData$EnvData$STN$stnID))
			istn <- which(.cdtData$EnvData$stn.data$id == stnid) + 1
			outliers <- .cdtData$EnvData$outqc$res[[stnid]]$outliers

			daty <- str_trim(as.character(outliers$DATE))
			nonNA <- !is.na(daty) & daty != ""
			outliers <- outliers[nonNA, , drop = FALSE]
			daty <- daty[nonNA]
			idaty <- which(.cdtData$EnvData$stn.data$dates %in% daty) + is.elv

			stn.val <- as.numeric(as.character(outliers$STN.VAL))
			not.replace <- as.numeric(as.character(outliers$NOT.REPLACE))
			to.replace <- as.numeric(as.character(outliers$REPLACE.VAL))
			ina <- is.na(not.replace)
			stn.val[ina] <- info$miss.val
			nna <- !is.na(to.replace)
			stn.val[nna] <- to.replace[nna]
			tmp[idaty, istn] <- stn.val

			fwrite(tmp, file = file.stn, quote = FALSE, sep = info$sepr, col.names = FALSE, na = info$miss.val)
			rm(tmp); gc()
			Insert.Messages.Out(paste(stnid, ': replacement done'))
		})

		helpWidget(bt.replace.QC, 'Replaces outliers with the correct values or a missing values',
								'Replaces outliers with the correct values or a missing values')

		#########################################

		tkgrid(frameOutQC, row = 0, column = 0, sticky = 'we', padx = 1, pady = 1, ipadx = 1, ipady = 1)
		tkgrid(frameStnId, row = 1, column = 0, sticky = 'we', padx = 1, pady = 1, ipadx = 1, ipady = 1)
		tkgrid(bt.display.QC, row = 2, column = 0, sticky = 'we', padx = 1, pady = 5, ipadx = 1, ipady = 1)
		tkgrid(frameStnMon, row = 3, column = 0, sticky = 'we', padx = 1, pady = 3, ipadx = 1, ipady = 1)
		tkgrid(frameQCSP, row = 4, column = 0, sticky = 'we', padx = 1, pady = 3, ipadx = 1, ipady = 1)
		tkgrid(bt.replace.QC, row = 5, column = 0, sticky = 'we', padx = 1, pady = 5, ipadx = 1, ipady = 1)

	#######################################################################################################

	#Tab3
	subfr3 <- bwTabScrollableFrame(cmd.tab3)

		##############################################

		.cdtData$EnvData$zoom$xx1 <- tclVar()
		.cdtData$EnvData$zoom$xx2 <- tclVar()
		.cdtData$EnvData$zoom$yy1 <- tclVar()
		.cdtData$EnvData$zoom$yy2 <- tclVar()

		.cdtData$EnvData$zoom$pressButP <- tclVar(0)
		.cdtData$EnvData$zoom$pressButM <- tclVar(0)
		.cdtData$EnvData$zoom$pressButRect <- tclVar(0)
		.cdtData$EnvData$zoom$pressButDrag <- tclVar(0)

		ZoomXYval0 <- NULL

		##############################################

		frameZoom <- ttklabelframe(subfr3, text = "Zoom", relief = 'groove')

		xentr1.zoom <- tkentry(frameZoom, width = 7, justify = "left", textvariable = .cdtData$EnvData$zoom$xx1)
		xentr2.zoom <- tkentry(frameZoom, width = 7, justify = "left", textvariable = .cdtData$EnvData$zoom$xx2)
		yentr1.zoom <- tkentry(frameZoom, width = 7, justify = "left", textvariable = .cdtData$EnvData$zoom$yy1)
		yentr2.zoom <- tkentry(frameZoom, width = 7, justify = "left", textvariable = .cdtData$EnvData$zoom$yy2)
		bt.centre.zoom <- tklabel(frameZoom, image = .cdtEnv$tcl$zoom$img$centre)

		.cdtData$EnvData$zoom$btZoomP <- tkbutton(frameZoom, image = .cdtEnv$tcl$zoom$img$plus, relief = 'raised', bg = 'lightblue', state = 'normal')
		.cdtData$EnvData$zoom$btZoomM <- tkbutton(frameZoom, image = .cdtEnv$tcl$zoom$img$moins, relief = 'raised', bg = 'lightblue', state = 'normal')
		.cdtData$EnvData$zoom$btZoomRect <- tkbutton(frameZoom, image = .cdtEnv$tcl$zoom$img$rect, relief = 'raised', bg = 'lightblue', state = 'normal')
		.cdtData$EnvData$zoom$btPanImg <- tkbutton(frameZoom, image = .cdtEnv$tcl$zoom$img$pan, relief = 'raised', bg = 'lightblue', state = 'normal')
		.cdtData$EnvData$zoom$btRedraw <- tkbutton(frameZoom, image = .cdtEnv$tcl$zoom$img$redraw, relief = 'raised', bg = 'lightblue')
		.cdtData$EnvData$zoom$btReset <- tkbutton(frameZoom, image = .cdtEnv$tcl$zoom$img$reset, relief = 'raised')

		#################
		tkconfigure(.cdtData$EnvData$zoom$btRedraw, command = function(){
			.cdtData$EnvData$ZoomXYval <- as.numeric(c(tclvalue(.cdtData$EnvData$zoom$xx1), tclvalue(.cdtData$EnvData$zoom$xx2),
									 				tclvalue(.cdtData$EnvData$zoom$yy1), tclvalue(.cdtData$EnvData$zoom$yy2)))

			# ZoomXYval
			tabid <- as.numeric(tclvalue(tkindex(.cdtEnv$tcl$main$tknotes, 'current'))) + 1
			if(length(.cdtData$OpenTab$Type) > 0){
				if(.cdtData$OpenTab$Type[[tabid]] == "img" & !is.null(.cdtData$EnvData$tab$spoutliers))
				{
					if(.cdtData$OpenTab$Data[[tabid]][[1]][[1]]$ID == .cdtData$EnvData$tab$spoutliers[[2]])
					{
						refreshPlot(W = .cdtData$OpenTab$Data[[tabid]][[2]][[1]],
									img = .cdtData$OpenTab$Data[[tabid]][[2]][[2]],
									hscale = as.numeric(tclvalue(tkget(.cdtEnv$tcl$toolbar$spinH))),
									vscale = as.numeric(tclvalue(tkget(.cdtEnv$tcl$toolbar$spinV))))
						tkconfigure(.cdtData$EnvData$zoom$btRedraw, relief = 'raised', bg = 'lightblue')
					}
				}
			}
		})

		tkconfigure(.cdtData$EnvData$zoom$btReset, command = function(){
			.cdtData$EnvData$ZoomXYval <- ZoomXYval0
			tclvalue(.cdtData$EnvData$zoom$xx1) <- ZoomXYval0[1]
			tclvalue(.cdtData$EnvData$zoom$xx2) <- ZoomXYval0[2]
			tclvalue(.cdtData$EnvData$zoom$yy1) <- ZoomXYval0[3]
			tclvalue(.cdtData$EnvData$zoom$yy2) <- ZoomXYval0[4]

			# ZoomXYval
			tabid <- as.numeric(tclvalue(tkindex(.cdtEnv$tcl$main$tknotes, 'current'))) + 1
			if(length(.cdtData$OpenTab$Type) > 0){
				if(.cdtData$OpenTab$Type[[tabid]] == "img" & !is.null(.cdtData$EnvData$tab$spoutliers))
				{
					if(.cdtData$OpenTab$Data[[tabid]][[1]][[1]]$ID == .cdtData$EnvData$tab$spoutliers[[2]])
					{
						refreshPlot(W = .cdtData$OpenTab$Data[[tabid]][[2]][[1]],
									img = .cdtData$OpenTab$Data[[tabid]][[2]][[2]],
									hscale = as.numeric(tclvalue(tkget(.cdtEnv$tcl$toolbar$spinH))),
									vscale = as.numeric(tclvalue(tkget(.cdtEnv$tcl$toolbar$spinV))))
						tkconfigure(.cdtData$EnvData$zoom$btRedraw, relief = 'raised', bg = 'lightblue')
					}
				}
			}
		})

		tkgrid(xentr1.zoom, row = 1, column = 0, sticky = 'we', rowspan = 1, columnspan = 1)
		tkgrid(xentr2.zoom, row = 1, column = 2, sticky = 'we', rowspan = 1, columnspan = 1)
		tkgrid(yentr1.zoom, row = 2, column = 1, sticky = 'we', rowspan = 1, columnspan = 1)
		tkgrid(yentr2.zoom, row = 0, column = 1, sticky = 'we', rowspan = 1, columnspan = 1)
		tkgrid(bt.centre.zoom, row = 1, column = 1, sticky = 'nswe', rowspan = 1, columnspan = 1)

		tkgrid(.cdtData$EnvData$zoom$btReset, row = 0, column = 3, sticky = 'nswe', rowspan = 1, columnspan = 1)
		tkgrid(.cdtData$EnvData$zoom$btRedraw, row = 1, column = 3, sticky = 'nswe', rowspan = 1, columnspan = 1)
		tkgrid(.cdtData$EnvData$zoom$btPanImg, row = 2, column = 3, sticky = 'nswe', rowspan = 1, columnspan = 1)

		tkgrid(.cdtData$EnvData$zoom$btZoomP, row = 0, column = 4, sticky = 'nswe', rowspan = 1, columnspan = 1)
		tkgrid(.cdtData$EnvData$zoom$btZoomM, row = 1, column = 4, sticky = 'nswe', rowspan = 1, columnspan = 1)
		tkgrid(.cdtData$EnvData$zoom$btZoomRect, row = 2, column = 4, sticky = 'nswe', rowspan = 1, columnspan = 1)

		infobulle(.cdtData$EnvData$zoom$btZoomP, 'Zoom In')
		status.bar.display(.cdtData$EnvData$zoom$btZoomP, 'Zoom In')
		infobulle(.cdtData$EnvData$zoom$btZoomM, 'Zoom Out')
		status.bar.display(.cdtData$EnvData$zoom$btZoomM, 'Zoom Out')
		infobulle(.cdtData$EnvData$zoom$btZoomRect, 'Zoom Area')
		status.bar.display(.cdtData$EnvData$zoom$btZoomRect, 'Zoom Area')
		infobulle(.cdtData$EnvData$zoom$btPanImg, 'Pan Tool')
		status.bar.display(.cdtData$EnvData$zoom$btPanImg, 'Pan Tool')
		infobulle(.cdtData$EnvData$zoom$btRedraw, 'Redraw Map')
		status.bar.display(.cdtData$EnvData$zoom$btRedraw, 'Redraw Map')
		infobulle(.cdtData$EnvData$zoom$btReset,' Zoom Reset')
		status.bar.display(.cdtData$EnvData$zoom$btReset, 'Zoom Reset')

		##########################################

		initXYval0 <- NA
		initializeButZoom <- function(){
			initXYval0 <<- str_trim(c(tclvalue(.cdtData$EnvData$zoom$xx1), tclvalue(.cdtData$EnvData$zoom$xx2),
									tclvalue(.cdtData$EnvData$zoom$yy1), tclvalue(.cdtData$EnvData$zoom$yy2)))

			tclvalue(.cdtData$EnvData$zoom$pressButP) <- 0
			tclvalue(.cdtData$EnvData$zoom$pressButM) <- 0
			tclvalue(.cdtData$EnvData$zoom$pressButRect) <- 0
			tclvalue(.cdtData$EnvData$zoom$pressButDrag) <- 0

			tkconfigure(.cdtData$EnvData$zoom$btZoomP, relief = 'raised', bg = 'lightblue', state = 'normal')
			tkconfigure(.cdtData$EnvData$zoom$btZoomM, relief = 'raised', bg = 'lightblue', state = 'normal')
			tkconfigure(.cdtData$EnvData$zoom$btZoomRect, relief = 'raised', bg = 'lightblue', state = 'normal')
			tkconfigure(.cdtData$EnvData$zoom$btPanImg, relief = 'raised', bg = 'lightblue', state = 'normal')
		}

		activateButRedraw <- function(){
			initXYval1 <- str_trim(c(tclvalue(.cdtData$EnvData$zoom$xx1), tclvalue(.cdtData$EnvData$zoom$xx2),
									tclvalue(.cdtData$EnvData$zoom$yy1), tclvalue(.cdtData$EnvData$zoom$yy2)))
			if(!all(initXYval0 == initXYval1)) tkconfigure(.cdtData$EnvData$zoom$btRedraw, relief = 'raised', bg = 'red')
		}

		#################
		tkbind(xentr1.zoom, "<FocusIn>", initializeButZoom)
		tkbind(xentr1.zoom, "<FocusOut>", activateButRedraw)

		tkbind(xentr2.zoom, "<FocusIn>", initializeButZoom)
		tkbind(xentr2.zoom, "<FocusOut>", activateButRedraw)

		tkbind(yentr1.zoom, "<FocusIn>", initializeButZoom)
		tkbind(yentr1.zoom, "<FocusOut>", activateButRedraw)

		tkbind(yentr2.zoom, "<FocusIn>", initializeButZoom)
		tkbind(yentr2.zoom, "<FocusOut>", activateButRedraw)

		####
		tkbind(.cdtData$EnvData$zoom$btRedraw, "<Button-1>", function(){
			tclvalue(.cdtData$EnvData$zoom$pressButP) <- 0
			tclvalue(.cdtData$EnvData$zoom$pressButM) <- 0
			tclvalue(.cdtData$EnvData$zoom$pressButRect) <- 0
			tclvalue(.cdtData$EnvData$zoom$pressButDrag) <- 0

			tkconfigure(.cdtData$EnvData$zoom$btRedraw, relief = 'raised', bg = 'lightblue')
			tkconfigure(.cdtData$EnvData$zoom$btZoomP, relief = 'raised', bg = 'lightblue', state = 'normal')
			tkconfigure(.cdtData$EnvData$zoom$btZoomM, relief = 'raised', bg = 'lightblue', state = 'normal')
			tkconfigure(.cdtData$EnvData$zoom$btZoomRect, relief = 'raised', bg = 'lightblue', state = 'normal')
			tkconfigure(.cdtData$EnvData$zoom$btPanImg, relief = 'raised', bg = 'lightblue', state = 'normal')
		})

		tkbind(.cdtData$EnvData$zoom$btReset, "<Button-1>", function(){
			tclvalue(.cdtData$EnvData$zoom$pressButP) <- 0
			tclvalue(.cdtData$EnvData$zoom$pressButM) <- 0
			tclvalue(.cdtData$EnvData$zoom$pressButRect) <- 0
			tclvalue(.cdtData$EnvData$zoom$pressButDrag) <- 0

			tkconfigure(.cdtData$EnvData$zoom$btRedraw, relief = 'raised', bg = 'lightblue')
			tkconfigure(.cdtData$EnvData$zoom$btZoomP, relief = 'raised', bg = 'lightblue', state = 'normal')
			tkconfigure(.cdtData$EnvData$zoom$btZoomM, relief = 'raised', bg = 'lightblue', state = 'normal')
			tkconfigure(.cdtData$EnvData$zoom$btZoomRect, relief = 'raised', bg = 'lightblue', state = 'normal')
			tkconfigure(.cdtData$EnvData$zoom$btPanImg, relief = 'raised', bg = 'lightblue', state = 'normal')
		})

		tkbind(.cdtData$EnvData$zoom$btZoomP, "<Button-1>", function(){
			tclvalue(.cdtData$EnvData$zoom$pressButP) <- 1
			tclvalue(.cdtData$EnvData$zoom$pressButM) <- 0
			tclvalue(.cdtData$EnvData$zoom$pressButRect) <- 0
			tclvalue(.cdtData$EnvData$zoom$pressButDrag) <- 0

			tkconfigure(.cdtData$EnvData$zoom$btRedraw, relief = 'raised', bg = 'lightblue')
			tkconfigure(.cdtData$EnvData$zoom$btZoomP, relief = 'raised', bg = 'red', state = 'disabled')
			tkconfigure(.cdtData$EnvData$zoom$btZoomM, relief = 'raised', bg = 'lightblue', state = 'normal')
			tkconfigure(.cdtData$EnvData$zoom$btZoomRect, relief = 'raised', bg = 'lightblue', state = 'normal')
			tkconfigure(.cdtData$EnvData$zoom$btPanImg, relief = 'raised', bg = 'lightblue', state = 'normal')
		})

		tkbind(.cdtData$EnvData$zoom$btZoomM, "<Button-1>", function(){
			tclvalue(.cdtData$EnvData$zoom$pressButP) <- 0
			tclvalue(.cdtData$EnvData$zoom$pressButM) <- 1
			tclvalue(.cdtData$EnvData$zoom$pressButRect) <- 0
			tclvalue(.cdtData$EnvData$zoom$pressButDrag) <- 0

			tkconfigure(.cdtData$EnvData$zoom$btRedraw, relief = 'raised', bg = 'lightblue')
			tkconfigure(.cdtData$EnvData$zoom$btZoomP, relief = 'raised', bg = 'lightblue', state = 'normal')
			tkconfigure(.cdtData$EnvData$zoom$btZoomM, relief = 'raised', bg = 'red', state = 'disabled')
			tkconfigure(.cdtData$EnvData$zoom$btZoomRect, relief = 'raised', bg = 'lightblue', state = 'normal')
			tkconfigure(.cdtData$EnvData$zoom$btPanImg, relief = 'raised', bg = 'lightblue', state = 'normal')
		})

		tkbind(.cdtData$EnvData$zoom$btZoomRect, "<Button-1>", function(){
			tclvalue(.cdtData$EnvData$zoom$pressButP) <- 0
			tclvalue(.cdtData$EnvData$zoom$pressButM) <- 0
			tclvalue(.cdtData$EnvData$zoom$pressButRect) <- 1
			tclvalue(.cdtData$EnvData$zoom$pressButDrag) <- 0

			tkconfigure(.cdtData$EnvData$zoom$btRedraw, relief = 'raised', bg = 'lightblue')
			tkconfigure(.cdtData$EnvData$zoom$btZoomP, relief = 'raised', bg = 'lightblue', state = 'normal')
			tkconfigure(.cdtData$EnvData$zoom$btZoomM, relief = 'raised', bg = 'lightblue', state = 'normal')
			tkconfigure(.cdtData$EnvData$zoom$btZoomRect, relief = 'raised', bg = 'red', state = 'disabled')
			tkconfigure(.cdtData$EnvData$zoom$btPanImg, relief = 'raised', bg = 'lightblue', state = 'normal')
		})

		tkbind(.cdtData$EnvData$zoom$btPanImg, "<Button-1>", function(){
			tclvalue(.cdtData$EnvData$zoom$pressButP) <- 0
			tclvalue(.cdtData$EnvData$zoom$pressButM) <- 0
			tclvalue(.cdtData$EnvData$zoom$pressButRect) <- 0
			tclvalue(.cdtData$EnvData$zoom$pressButDrag) <- 1

			tkconfigure(.cdtData$EnvData$zoom$btRedraw, relief = 'raised', bg = 'lightblue')
			tkconfigure(.cdtData$EnvData$zoom$btZoomP, relief = 'raised', bg = 'lightblue', state = 'normal')
			tkconfigure(.cdtData$EnvData$zoom$btZoomM, relief = 'raised', bg = 'lightblue', state = 'normal')
			tkconfigure(.cdtData$EnvData$zoom$btZoomRect, relief = 'raised', bg = 'lightblue', state = 'normal')
			tkconfigure(.cdtData$EnvData$zoom$btPanImg, relief = 'raised', bg = 'red', state = 'disabled')
		})

		##########################################

		tkgrid(frameZoom, row = 0, column = 0, sticky = '')

	#######################################################################################################

	#Tab4
	subfr4 <- bwTabScrollableFrame(cmd.tab4)

		##############################################

		frameSHP <- tkframe(subfr4, relief = 'groove', borderwidth = 2)

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

		#######################

		frameDEM <- tkframe(subfr4, relief = 'groove', borderwidth = 2)

		.cdtData$EnvData$dem$add.dem <- tclVar(FALSE)
		file.plotDem <- tclVar()
		stateDEM <- "disabled"

		chk.adddem <- tkcheckbutton(frameDEM, variable = .cdtData$EnvData$dem$add.dem, text = "Add DEM to map", anchor = 'w', justify = 'left')
		cb.adddem <- ttkcombobox(frameDEM, values = unlist(listOpenFiles), textvariable = file.plotDem, width = largeur1, state = stateDEM)
		bt.adddem <- tkbutton(frameDEM, text = "...", state = stateDEM)

		########

		.cdtData$EnvData$dem$Opt <- list(
										user.colors = list(custom = TRUE, color = gray(seq(0.9, 0.1, length = 64))),
										user.levels = list(custom = TRUE, levels = NULL, equidist = TRUE)
									)

		tkconfigure(bt.adddem, command = function(){
			nc.opfiles <- getOpenNetcdf(.cdtEnv$tcl$main$win, initialdir = getwd())
			if(!is.null(nc.opfiles)){
				update.OpenFiles('netcdf', nc.opfiles)
				listOpenFiles[[length(listOpenFiles) + 1]] <<- nc.opfiles[[1]]
				tclvalue(file.plotDem) <- nc.opfiles[[1]]
				tkconfigure(cb.adddem, values = unlist(listOpenFiles))

				demInfo <- getNCDFSampleData(str_trim(tclvalue(file.plotDem)))
				if(is.null(demInfo)){
					Insert.Messages.Out("Unable to read DEM data", format = TRUE)
					tclvalue(file.plotDem) <- ""
					.cdtData$EnvData$dem$dem <- NULL
				}else{
					jfile <- getIndex.AllOpenFiles(str_trim(tclvalue(file.plotDem)))
					ncdata <- .cdtData$OpenFiles$Data[[jfile]][[2]]
					.cdtData$EnvData$dem$dem <- ncdata[c('x', 'y', 'z')]
				}
			}
		})

		########
		tkgrid(chk.adddem, row = 0, column = 0, sticky = 'we', rowspan = 1, columnspan = 5, padx = 1, pady = 1)
		tkgrid(cb.adddem, row = 1, column = 0, sticky = 'we', rowspan = 1, columnspan = 4, padx = 1, pady = 1)
		tkgrid(bt.adddem, row = 1, column = 4, sticky = 'w', rowspan = 1, columnspan = 1, padx = 0, pady = 1)

		########
		tkbind(cb.adddem, "<<ComboboxSelected>>", function(){
			demInfo <- getNCDFSampleData(str_trim(tclvalue(file.plotDem)))
			if(is.null(demInfo)){
				Insert.Messages.Out("Unable to read DEM data", format = TRUE)
				.cdtData$EnvData$dem$dem <- NULL
			}else{
				jfile <- getIndex.AllOpenFiles(str_trim(tclvalue(file.plotDem)))
				ncdata <- .cdtData$OpenFiles$Data[[jfile]][[2]]
				.cdtData$EnvData$dem$dem <- ncdata[c('x', 'y', 'z')]
			}
		})

		tkbind(chk.adddem, "<Button-1>", function(){
			stateDEM <- if(tclvalue(.cdtData$EnvData$dem$add.dem) == "1") "disabled" else "normal"
			tkconfigure(cb.adddem, state = stateDEM)
			tkconfigure(bt.adddem, state = stateDEM)
		})

		#######################

		frameSAT <- tkframe(subfr4, relief = 'groove', borderwidth = 2)

		.cdtData$EnvData$sat$dir <- ""
		.cdtData$EnvData$sat$sample <- ""
		.cdtData$EnvData$sat$format <- "rfe_%s%s%s.nc"
		.cdtData$EnvData$sat$sat.data <- NULL

		.cdtData$EnvData$sat$add.sat <- tclVar(FALSE)
		dir.plotSat <- tclVar()
		stateSAT <- "disabled"

		chk.addSat <- tkcheckbutton(frameSAT, variable = .cdtData$EnvData$sat$add.sat, text = "Add satellite data to map", anchor = 'w', justify = 'left')
		bt.addSatSet <- ttkbutton(frameSAT, text = "Settings", state = stateSAT)
		en.addSat <- tkentry(frameSAT, textvariable = dir.plotSat, width = largeur2, state = stateSAT)
		bt.addSat <- tkbutton(frameSAT, text = "...", state = stateSAT)

		########

		.cdtData$EnvData$sat$Opt <- list(
										user.colors = list(custom = TRUE, color = colorRampPalette(colors()[c(1:4, 8:12)])(100)),
										user.levels = list(custom = TRUE, levels = NULL, equidist = TRUE)
									)

		tkconfigure(bt.addSat, command = function(){
			dirnc <- tk_choose.dir(getwd(), "")
			tclvalue(dir.plotSat) <- if(dirnc %in% c("", "NA") | is.na(dirnc)) "" else dirnc

			.cdtData$EnvData$sat$dir <- str_trim(tclvalue(dir.plotSat))
		})

		tkconfigure(bt.addSatSet, command = function(){
			tstep <- .cdtData$EnvData$output$params$intstep
			TSTEPVAL0 <- .cdtEnv$tcl$lang$global[['combobox']][['1']][2:5]
			periodVAL <- c('daily', 'pentad', 'dekadal', 'monthly')
			timeSteps <- if(is.null(tstep)) TSTEPVAL0[1] else TSTEPVAL0[periodVAL %in% tstep]

			.cdtData$EnvData$sat <- getInfoNetcdfData(.cdtEnv$tcl$main$win, .cdtData$EnvData$sat,
													str_trim(tclvalue(dir.plotSat)), timeSteps)
			sdon <- getNCDFSampleData(.cdtData$EnvData$sat$sample)
			if(is.null(sdon)){
				Insert.Messages.Out("No sample data found", format = TRUE)
				.cdtData$EnvData$sat$sat.data <- NULL
			}else .cdtData$EnvData$sat$sat.data <- sdon
		})

		########
		tkgrid(chk.addSat, row = 0, column = 0, sticky = 'we', rowspan = 1, columnspan = 6, padx = 1, pady = 1)
		tkgrid(bt.addSatSet, row = 0, column = 6, sticky = 'we', rowspan = 1, columnspan = 2, padx = 1, pady = 1)
		tkgrid(en.addSat, row = 1, column = 0, sticky = 'we', rowspan = 1, columnspan = 7, padx = 1, pady = 1)
		tkgrid(bt.addSat, row = 1, column = 7, sticky = 'w', rowspan = 1, columnspan = 1, padx = 0, pady = 1)

		########
		tkbind(chk.addSat, "<Button-1>", function(){
			stateSAT <- if(tclvalue(.cdtData$EnvData$sat$add.sat) == "1") "disabled" else "normal"
			tkconfigure(en.addSat, state = stateSAT)
			tkconfigure(bt.addSat, state = stateSAT)
			tkconfigure(bt.addSatSet, state = stateSAT)
		})

		#######################

		framePlotType <- tkframe(subfr4)

		plot.type <- c("Pixels", "FilledContour")
		.cdtData$EnvData$plot.maps$plot.type <- tclVar("Pixels")

		txt.plotType <- tklabel(framePlotType, text = "Plot Type", anchor = 'e', justify = 'right')
		cb.plotType <- ttkcombobox(framePlotType, values = plot.type, textvariable = .cdtData$EnvData$plot.maps$plot.type, width = largeur3)

		tkgrid(txt.plotType, row = 0, column = 0, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
		tkgrid(cb.plotType, row = 0, column = 1, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)

		##############################################

		tkgrid(frameSHP, row = 0, column = 0, sticky = 'we', padx = 1, pady = 1, ipadx = 1, ipady = 1)
		tkgrid(frameDEM, row = 1, column = 0, sticky = 'we', padx = 1, pady = 3, ipadx = 1, ipady = 1)
		tkgrid(frameSAT, row = 2, column = 0, sticky = 'we', padx = 1, pady = 3, ipadx = 1, ipady = 1)
		tkgrid(framePlotType, row = 3, column = 0, sticky = '', padx = 1, pady = 3, ipadx = 1, ipady = 1)

	#######################################################################################################

	set.station.id <- function(){
		STNID <- .cdtData$EnvData$outqc$stn
		tkconfigure(.cdtData$EnvData$STN$cb.stnID, values = STNID)
		tclvalue(.cdtData$EnvData$STN$stnID) <- STNID[1]
	}

	set.date.outliers <- function(){
		stnid <- str_trim(tclvalue(.cdtData$EnvData$STN$stnID))
		if(stnid == "") return(NULL)
		dateOUT <- .cdtData$EnvData$outqc$res[[stnid]]$date
		tkconfigure(.cdtData$EnvData$STN$cb.QCSP, values = dateOUT)
		tclvalue(.cdtData$EnvData$STN$dateSP) <- dateOUT[1]
		return(0)
	}

	set.initialize.Zoom <- function(){
		donX <- range(.cdtData$EnvData$stn.data$lon, na.rm = TRUE)
		donY <- range(.cdtData$EnvData$stn.data$lat, na.rm = TRUE)
		lo1 <- round(donX[1], 4)
		lo2 <- round(donX[2], 4)
		la1 <- round(donY[1], 4)
		la2 <- round(donY[2], 4)
		ZoomXYval0 <<- c(lo1, lo2, la1, la2)

		tclvalue(.cdtData$EnvData$zoom$xx1) <- lo1
		tclvalue(.cdtData$EnvData$zoom$xx2) <- lo2
		tclvalue(.cdtData$EnvData$zoom$yy1) <- la1
		tclvalue(.cdtData$EnvData$zoom$yy2) <- la2

		.cdtData$EnvData$ZoomXYval <- as.numeric(c(tclvalue(.cdtData$EnvData$zoom$xx1),
												tclvalue(.cdtData$EnvData$zoom$xx2),
												tclvalue(.cdtData$EnvData$zoom$yy1),
												tclvalue(.cdtData$EnvData$zoom$yy2)))
	}

	##########
	.cdtData$EnvData$QC$SaveEdit <- function(dat2sav){
		stnid <- str_trim(tclvalue(.cdtData$EnvData$STN$stnID))
		saved <- FALSE
		if(!is.null(dat2sav)){
			if(!is.null(dat2sav$REPLACE.VAL)){
				.cdtData$EnvData$outqc$res[[stnid]]$outliers <- dat2sav

				file.outqc <- file.path(.cdtData$EnvData$PathData, 'CDTDATASET', "QCResults.rds")
				saveRDS(.cdtData$EnvData$outqc, file.outqc)
				saved <- TRUE
			}
		}

		if(!saved) Insert.Messages.Out('Unable to save table', format = TRUE)
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
