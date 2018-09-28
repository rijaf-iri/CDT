
testHomogeneityPanelCmd <- function(){
	listOpenFiles <- openFile_ttkcomboList()
	if(WindowsOS()){
		largeur0 <- .cdtEnv$tcl$fun$w.widgets(18)
		largeur1 <- .cdtEnv$tcl$fun$w.widgets(27)
		largeur2 <- .cdtEnv$tcl$fun$w.widgets(29)
		largeur3 <- .cdtEnv$tcl$fun$w.widgets(20)
		largeur4 <- .cdtEnv$tcl$fun$w.widgets(19.5)
		largeur5 <- 24
		largeur6 <- 28
	}else{
		largeur0 <- .cdtEnv$tcl$fun$w.widgets(16)
		largeur1 <- .cdtEnv$tcl$fun$w.widgets(22)
		largeur2 <- .cdtEnv$tcl$fun$w.widgets(23)
		largeur3 <- .cdtEnv$tcl$fun$w.widgets(17.5)
		largeur4 <- .cdtEnv$tcl$fun$w.widgets(19)
		largeur5 <- 19
		largeur6 <- 22
	}

	GeneralParameters <- list(intstep = "daily", infile = "", outdir = "",
							stats = list(mthd = 'SNHT', crop = FALSE, h = 0.025, kmax = 10,
										conf.lev = 95, min.len = 24, min.year = 5, min.frac = 0.5),
							series = list(use = FALSE, use.climato = TRUE, diff.ratio = 1, weight = 1,
										voisin = list(min = 2, max = 8, dist = 30, elv = 800, rho = 0.3),
										elv = list(use = FALSE, dem = TRUE, file = ""),
										user = list(refs = FALSE, file = "")
										),
							adj = list(min.mon = 32, min.dek = 32, min.dyp = 32,
										seg.mon = 0, seg.dek = 0, seg.dyp = 0),
							aggr = list(aggr.fun = 'mean', min.frac = 0.95)
							)

	MOIS <- format(ISOdate(2014, 1:12, 1), "%B")

	# xml.dlg <- file.path(.cdtDir$dirLocal, "languages", "cdtQC_RR.OutlierCheck_leftCmd.xml")
	# lang.dlg <- cdtLanguageParse(xml.dlg, .cdtData$Config$lang.iso)
	# .cdtData$EnvData$message <- lang.dlg[['message']]

	###################

	.cdtEnv$tcl$main$cmd.frame <- tkframe(.cdtEnv$tcl$main$panel.left)

	tknote.cmd <- bwNoteBook(.cdtEnv$tcl$main$cmd.frame)
	cmd.tab1 <- bwAddTab(tknote.cmd, text = "Data")
	cmd.tab2 <- bwAddTab(tknote.cmd, text = "Test Outputs")

	bwRaiseTab(tknote.cmd, cmd.tab1)

	tkgrid.columnconfigure(cmd.tab1, 0, weight = 1)
	tkgrid.columnconfigure(cmd.tab2, 0, weight = 1)

	tkgrid.rowconfigure(cmd.tab1, 0, weight = 1)
	tkgrid.rowconfigure(cmd.tab2, 0, weight = 1)

	#######################################################################################################

	#Tab1
	subfr1 <- bwTabScrollableFrame(cmd.tab1)

		#######################

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

		txt.infile <- tklabel(frameInData, text = 'File containing stations candidates series', anchor = 'w', justify = 'left')
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

		helpWidget(cb.infile, 'Select the file containing the candidates series', 'Select the file containing the candidates series')
		helpWidget(bt.infile, 'Browse file if not listed', 'Browse file if not listed')

		#######################

		frameStatH <- ttklabelframe(subfr1, text = "Detection Statistic", relief = 'groove')

		CbMETHOD.MTH <- c("Pettitt Test", "SNHT (Alexandersson & Moberg, 1997)",
						"CUSUM-type (Gallagher et al., 2013)",
						"CUSUM-type with Trend (Gallagher et al., 2013)")
		METHOD.MTH <- c('Pettitt', 'SNHT', 'CUSUM', 'CUSUMtr')
		hom.method <- tclVar()
		tclvalue(hom.method) <- CbMETHOD.MTH[METHOD.MTH %in% GeneralParameters$stats$mthd]

		cb.hom.mthd <- ttkcombobox(frameStatH, values = CbMETHOD.MTH, textvariable = hom.method, width = largeur3, state = 'readonly')
		bt.hom.mthd <- ttkbutton(frameStatH, text = .cdtEnv$tcl$lang$global[['button']][['5']])

		tkconfigure(bt.hom.mthd, command = function(){
			if(str_trim(tclvalue(hom.method)) == CbMETHOD.MTH[2])
				CONF.LEV <- c('90.0', '92.0', '94.0', '95.0', '97.5', '99.0')
			else
				CONF.LEV <- c('90.0', '92.0', '95.0', '97.5', '99.0', '99.9')

			Params <- GeneralParameters[["stats"]]
			GeneralParameters[["stats"]] <<- getParams.HomogMethod(Params, CONF.LEV)
		})

		tkgrid(cb.hom.mthd, row = 0, column = 0, sticky = 'we', rowspan = 1, columnspan = 3, padx = 1, pady = 1, ipadx = 1, ipady = 1)
		tkgrid(bt.hom.mthd, row = 0, column = 3, sticky = 'we', rowspan = 1, columnspan = 2, padx = 1, pady = 1, ipadx = 1, ipady = 1)

		helpWidget(cb.hom.mthd, 'Select a change points detection method', 'Select a change points detection method')
		helpWidget(bt.hom.mthd, 'Set parameters for homogenization procedures', 'Set parameters for homogenization procedures')

		#######################

		frameRefS <- tkframe(subfr1)

		use.RefS <- tclVar(GeneralParameters$series$use)

		chk.RefS <- tkcheckbutton(frameRefS, variable = use.RefS, text = 'Use reference series', anchor = 'w', justify = 'left')
		bt.RefS <- ttkbutton(frameRefS, text = .cdtEnv$tcl$lang$global[['button']][['5']], state = "disabled")

		tkconfigure(bt.RefS, command = function(){
			Params <- GeneralParameters[["series"]]
			GeneralParameters[["series"]] <<- getParams.HomoRefSeries(Params)
		})

		tkgrid(chk.RefS, row = 0, column = 0, sticky = 'we', rowspan = 1, columnspan = 3, padx = 1, pady = 1, ipadx = 1, ipady = 1)
		tkgrid(bt.RefS, row = 0, column = 3, sticky = 'we', rowspan = 1, columnspan = 2, padx = 1, pady = 1, ipadx = 1, ipady = 1)

		helpWidget(chk.RefS, 'Check to use reference series to perform the homogenization test',
							'Check to use reference series to perform the homogenization test')
		helpWidget(bt.RefS, 'Set parameters to create the reference series', 'Set parameters to create the reference series')

		###############
		tkbind(chk.RefS, "<Button-1>", function(){
			if(tclvalue(QCExist) == '0'){
				stateRefS <- if(tclvalue(use.RefS) == '1') 'disabled' else 'normal'
				tkconfigure(bt.RefS, state = stateRefS)
			}
		})

		#######################

		frameAggr <- ttklabelframe(subfr1, text = "Aggregation Function", relief = 'groove')

		AGGRFUN <- c("mean", "sum")
		aggr.fun <- tclVar(GeneralParameters$aggr$aggr.fun)
		min.frac <- tclVar(GeneralParameters$aggr$min.frac)

		txt.Aggr1 <- tklabel(frameAggr, text = 'Function', anchor = 'e', justify = 'right')
		txt.Aggr2 <- tklabel(frameAggr, text = 'Min.Frac', anchor = 'e', justify = 'right')
		cb.Aggr <- ttkcombobox(frameAggr, values = AGGRFUN, textvariable = aggr.fun, width = 10)
		en.Aggr <- tkentry(frameAggr, textvariable = min.frac, width = 6)

		tkgrid(txt.Aggr1, row = 0, column = 0, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
		tkgrid(cb.Aggr, row = 0, column = 1, sticky = 'w', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
		tkgrid(txt.Aggr2, row = 0, column = 2, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
		tkgrid(en.Aggr, row = 0, column = 3, sticky = 'w', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)

		helpWidget(cb.Aggr, 'Function to be used to aggregate data', 'Function to be used to aggregate data')
		helpWidget(en.Aggr, 'Minimum fraction of available data that must be present within each output time step',
							'Minimum fraction of available data that must be present within each output time step')

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

		bt.HomogTest <- ttkbutton(subfr1, text = "Test Homogeneity")

		tkconfigure(bt.HomogTest, command = function(){
			GeneralParameters$intstep <- periodVAL[CbperiodVAL %in% str_trim(tclvalue(timeSteps))]
			GeneralParameters$infile <- str_trim(tclvalue(input.file))
			GeneralParameters$outdir <- str_trim(tclvalue(dir.save))

			GeneralParameters$stats$mthd <- METHOD.MTH[CbMETHOD.MTH %in% str_trim(tclvalue(hom.method))]
			GeneralParameters$series$use <- switch(tclvalue(use.RefS), '0' = FALSE, '1' = TRUE)

			GeneralParameters$aggr$aggr.fun <- str_trim(tclvalue(aggr.fun))
			GeneralParameters$aggr$min.frac <- as.numeric(str_trim(tclvalue(min.frac)))

			assign('GeneralParameters', GeneralParameters, envir = .GlobalEnv)

			Insert.Messages.Out("Test homogeneity ......")

			tkconfigure(.cdtEnv$tcl$main$win, cursor = 'watch')
			tcl('update')
			ret <- tryCatch(
				{
					homogeneityTestProcs(GeneralParameters)
				},
				warning = function(w) warningFun(w),
				error = function(e) errorFun(e),
				finally = {
					tkconfigure(.cdtEnv$tcl$main$win, cursor = '')
					tcl('update')
				}
			)

			msg0 <- "Homogeneity test finished successfully"
			msg1 <- "Homogeneity test failed"

			if(!is.null(ret)){
				if(ret == 0){
					Insert.Messages.Out(msg0)
					set.station.id()
				}else Insert.Messages.Out(msg1, format = TRUE)
			}else Insert.Messages.Out(msg1, format = TRUE)
		})

		#########################################

		tkgrid(frameTimeS, row = 0, column = 0, sticky = '', padx = 1, pady = 1, ipadx = 1, ipady = 1)
		tkgrid(frameInData, row = 1, column = 0, sticky = 'we', padx = 1, pady = 1, ipadx = 1, ipady = 1)
		tkgrid(frameStatH, row = 2, column = 0, sticky = 'we', padx = 1, pady = 1, ipadx = 1, ipady = 1)
		tkgrid(frameRefS, row = 3, column = 0, sticky = '', padx = 1, pady = 1, ipadx = 1, ipady = 1)
		tkgrid(frameAggr, row = 4, column = 0, sticky = '', padx = 1, pady = 1, ipadx = 1, ipady = 1)
		tkgrid(frameDirSav, row = 5, column = 0, sticky = 'we', padx = 1, pady = 1, ipadx = 1, ipady = 1)
		tkgrid(bt.HomogTest, row = 6, column = 0, sticky = '', padx = 1, pady = 1, ipadx = 1, ipady = 1)

	#######################################################################################################

	#Tab2
	subfr2 <- bwTabScrollableFrame(cmd.tab2)

		##############################################

		frameOutQC <- ttklabelframe(subfr2, text = "Homogenization Data", relief = 'groove')

		QCExist <- tclVar(0)
		file.dataIndex <- tclVar()

		chk.dataIdx <- tkcheckbutton(frameOutQC, variable = QCExist, text = "Load existing Homogenization session", anchor = 'w', justify = 'left')
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
					return(NULL)
				}

				.cdtData$EnvData$output <- OutQC
				.cdtData$EnvData$PathData <- dirname(str_trim(tclvalue(file.dataIndex)))

				###############
				file.table <- file.path(.cdtData$EnvData$PathData, 'CDTDATASET', "BreaksPointsTable.rds")
				if(!file.exists(file.table)){
					Insert.Messages.Out(paste(file.table, 'not found'), format = TRUE)
					return(NULL)
				}
				.cdtData$EnvData$cpt.table <- readRDS(file.table)

				###############
				file.table0 <- file.path(.cdtData$EnvData$PathData, 'CDTDATASET', "BreaksPointsTable0.rds")
				if(!file.exists(file.table0)){
					Insert.Messages.Out(paste(file.table0, 'not found'), format = TRUE)
					return(NULL)
				}
				.cdtData$EnvData$cpt.table0 <- readRDS(file.table0)

				###############
				file.candS <- file.path(.cdtData$EnvData$PathData, 'CDTDATASET', "CandidateSeries.rds")
				if(!file.exists(file.candS)){
					Insert.Messages.Out(paste(file.candS, 'not found'), format = TRUE)
					return(NULL)
				}
				.cdtData$EnvData$candS <- readRDS(file.candS)

				###############
				file.testS <- file.path(.cdtData$EnvData$PathData, 'CDTDATASET', "TestSeries.rds")
				if(!file.exists(file.testS)){
					Insert.Messages.Out(paste(file.testS, 'not found'), format = TRUE)
					return(NULL)
				}
				.cdtData$EnvData$testS <- readRDS(file.testS)

				###############
				file.stats <- file.path(.cdtData$EnvData$PathData, 'CDTDATASET', "BreaksPointsStats.rds")
				if(!file.exists(file.stats)){
					Insert.Messages.Out(paste(file.stats, 'not found'), format = TRUE)
					return(NULL)
				}
				.cdtData$EnvData$cpt.stats <- readRDS(file.stats)

				tclvalue(timeSteps) <- CbperiodVAL[periodVAL %in% .cdtData$EnvData$output$params$intstep]

				set.station.id()
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

			tkconfigure(cb.hom.mthd, state = stateQC)
			tkconfigure(bt.hom.mthd, state = stateQC)

			tkconfigure(cb.Aggr, state = stateQC)
			tkconfigure(en.Aggr, state = stateQC)

			tkconfigure(en.dir.save, state = stateQC)
			tkconfigure(bt.dir.save, state = stateQC)
			tkconfigure(bt.HomogTest, state = stateQC)

			tkconfigure(chk.RefS, state = stateQC)
			if(tclvalue(QCExist) == '1'){
				stateRefS <- if(tclvalue(use.RefS) == '0') 'disabled' else 'normal'
			}else stateRefS <- 'disabled'
			tkconfigure(bt.RefS, state = stateRefS)
		})

		#############################

		frameStnId <- ttklabelframe(subfr2, text = "Select station", relief = 'groove')

		.cdtData$EnvData$STN$stnID <- tclVar()

		bt.stnID.prev <- ttkbutton(frameStnId, text = "<<", width = 5)
		bt.stnID.next <- ttkbutton(frameStnId, text = ">>", width = 5)
		.cdtData$EnvData$STN$cb.stnID <- ttkcombobox(frameStnId, values = "", textvariable = .cdtData$EnvData$STN$stnID, width = largeur5, justify = 'center')

		tkconfigure(bt.stnID.prev, command = function(){
			if(!is.null(.cdtData$EnvData$output$data)){
				STNID <- .cdtData$EnvData$output$data$id
				istn <- which(STNID == str_trim(tclvalue(.cdtData$EnvData$STN$stnID)))
				istn <- istn - 1
				if(istn < 1) istn <- length(STNID)
				tclvalue(.cdtData$EnvData$STN$stnID) <- STNID[istn]
			}
		})

		tkconfigure(bt.stnID.next, command = function(){
			if(!is.null(.cdtData$EnvData$output$data)){
				STNID <- .cdtData$EnvData$output$data$id
				istn <- which(STNID == str_trim(tclvalue(.cdtData$EnvData$STN$stnID)))
				istn <- istn + 1
				if(istn > length(STNID)) istn <- 1
				tclvalue(.cdtData$EnvData$STN$stnID) <- STNID[istn]
			}
		})

		tkgrid(bt.stnID.prev, row = 0, column = 0, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 2, ipadx = 1, ipady = 1)
		tkgrid(.cdtData$EnvData$STN$cb.stnID, row = 0, column = 1, sticky = 'we', rowspan = 1, columnspan = 2, padx = 1, pady = 2, ipadx = 1, ipady = 1)
		tkgrid(bt.stnID.next, row = 0, column = 3, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 2, ipadx = 1, ipady = 1)

		#######################

		bt.display.Hom <- ttkbutton(subfr2, text = "View & Edit Result")
		bt.undo.Hom <- ttkbutton(subfr2, text = "Undo Change")

		.cdtData$EnvData$tab$TableStat <- NULL

		tkconfigure(bt.display.Hom, command = function(){
			if(is.null(.cdtData$EnvData$cpt.table)) return(NULL)
			display.cpt.output()
		})

		tkconfigure(bt.undo.Hom, command = function(){
			if(is.null(.cdtData$EnvData$cpt.table)) return(NULL)
			stnid <- str_trim(tclvalue(.cdtData$EnvData$STN$stnID))
			if(stnid == "") return(NULL)
			for(j in 1:3){
				if(is.null(.cdtData$EnvData$cpt.table0[[j]])) next
				replaceNULL <- vector('list', 1)
				names(replaceNULL) <- stnid
				old.cpt.table <- .cdtData$EnvData$cpt.table[[j]]
				.cdtData$EnvData$cpt.table[[j]] <- utils::modifyList(old.cpt.table, replaceNULL, keep.null = TRUE)

				replace0 <- .cdtData$EnvData$cpt.table0[[j]][stnid]
				old.cpt.table <- .cdtData$EnvData$cpt.table[[j]]
				.cdtData$EnvData$cpt.table[[j]] <- utils::modifyList(old.cpt.table, replace0, keep.null = TRUE)
			}

			file.table <- file.path(.cdtData$EnvData$PathData, 'CDTDATASET', "BreaksPointsTable.rds")
			saveRDS(.cdtData$EnvData$cpt.table, file.table)

			display.cpt.output()
		})

		#######################

		framePlot <- tkframe(subfr2)

		PLOTSERIES <- c("Test Series", "Base Series")
		.cdtData$EnvData$plot$plotseries <- tclVar(PLOTSERIES[1])

		cb.Plot.Hom <- ttkcombobox(framePlot, values = PLOTSERIES, textvariable = .cdtData$EnvData$plot$plotseries, width = largeur6)
		bt.Plot.Hom <- ttkbutton(framePlot, text = .cdtEnv$tcl$lang$global[['button']][['3']])

		.cdtData$EnvData$tab$breakpts <- NULL

		tkconfigure(bt.Plot.Hom, command = function(){
			stnid <- str_trim(tclvalue(.cdtData$EnvData$STN$stnID))
			if(stnid == "") return(NULL)
			tab.title <- paste0(stnid, "-BreakPoints")

			imgContainer <- homDislpay_BreakPoints(.cdtData$EnvData$tab$breakpts, tab.title)
			.cdtData$EnvData$tab$breakpts <- imageNotebookTab_unik(imgContainer, .cdtData$EnvData$tab$breakpts)
		})

		tkgrid(cb.Plot.Hom, row = 0, column = 0, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
		tkgrid(bt.Plot.Hom, row = 0, column = 1, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)

		#######################

		frameAdjust <- tkframe(subfr2)

		ADJSERIES <- c("None", "Mean", "Quantile Matching")
		.cdtData$EnvData$adj$adjseries <- tclVar(ADJSERIES[1])

		txt.adjust <- tklabel(frameAdjust, text = 'Series adjustment parameters', anchor = 'w', justify = 'left')
		bt.adjust <- ttkbutton(frameAdjust, text = .cdtEnv$tcl$lang$global[['button']][['5']])
		cb.Adj.Hom <- ttkcombobox(frameAdjust, values = ADJSERIES, textvariable = .cdtData$EnvData$adj$adjseries, width = largeur6)
		bt.Adj.Hom <- ttkbutton(frameAdjust, text = "ADJUST")

		tkconfigure(bt.Adj.Hom, command = function(){
			on.exit({
				tkconfigure(.cdtEnv$tcl$main$win, cursor = '')
				tcl('update')
			})
			tkconfigure(.cdtEnv$tcl$main$win, cursor = 'watch')
			tcl('update')

			stnid <- str_trim(tclvalue(.cdtData$EnvData$STN$stnID))
			if(stnid == "") return(NULL)
			if(is.null(.cdtData$EnvData$cpt.table)) return(NULL)

			STNID <- .cdtData$EnvData$output$data$id
			ids <- which(STNID == stnid)
			adj.mthd <- str_trim(tclvalue(.cdtData$EnvData$adj$adjseries))

			.cdtData$EnvData$output$params[['adj']] <- GeneralParameters[["adj"]]
			parsAdj <- .cdtData$EnvData$output$params[['adj']]
			parsStat <- .cdtData$EnvData$output$params[['stats']]

			Xl <- lapply(.cdtData$EnvData$candS, function(x){
				if(is.null(x)) return(NULL)
				don <- x$data[, ids]
				x$data <- don
				x
			})

			cpt.table <- lapply(.cdtData$EnvData$cpt.table, function(x){
				if(is.null(x)) return(NULL)
				x[[stnid]]
			})
			inull <- sapply(cpt.table, is.null)

			if(adj.mthd != "None" & !all(inull)){
				.cdtData$EnvData$adjS <- homog.AdjustSeries(Xl, cpt.table, parsStat, parsAdj)
			}else{
				.cdtData$EnvData$adjS <- lapply(Xl, function(x){
					if(is.null(x)) return(NULL)
					x$data <- matrix(x$data, nrow = length(x$data), ncol = 3)
					x
				})
			}

			istn <- ids + 1
			idon <- switch(adj.mthd, "None" = 1, "Mean" = 2, "Quantile Matching" = 3)

			is.elv <- if(is.null(.cdtData$EnvData$output$data$elv)) 3 else 4
			info <- .cdtData$EnvData$output$info[[3]]

			ret <- lapply(.cdtData$EnvData$adjS, function(don){
				if(is.null(don)) return(NULL)
				file.stn <- file.path(.cdtData$EnvData$PathData, 'CDTSTATIONS', paste0(toupper(don$tstep), '_', .cdtData$EnvData$output$info[[1]]))
				tmp <- fread(file.stn, header = FALSE, sep = info$sepr, stringsAsFactors = FALSE, colClasses = "character")
				idaty <- seq(nrow(tmp))[-(1:is.elv)]
				tmp[idaty, istn] <- as.character(round(don$data[, idon], 1))
				fwrite(tmp, file = file.stn, quote = FALSE, sep = info$sepr, col.names = FALSE, na = info$miss.val)
			})

			Insert.Messages.Out(paste(stnid, ': Adjustment done'))
		})

		tkconfigure(bt.adjust, command = function(){
			states <- list(day = c('Day', 'normal', 'normal'),
							pen = c('Pentad', 'normal', 'normal'),
							dek = c('Day', 'disabled', 'normal'),
							mon = c('Day', 'disabled', 'disabled'))

			states <- unlist(states[CbperiodVAL %in% str_trim(tclvalue(timeSteps))])
			label <- states[1]
			state.dyp <- states[2]
			state.dek <- states[3]
			Params <- GeneralParameters[["adj"]]
			GeneralParameters[["adj"]] <<- getParams.HomogAdjust(Params, label, state.dyp, state.dek)
		})

		tkgrid(txt.adjust, row = 0, column = 0, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
		tkgrid(bt.adjust, row = 0, column = 1, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
		tkgrid(cb.Adj.Hom, row = 1, column = 0, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
		tkgrid(bt.Adj.Hom, row = 1, column = 1, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)

		helpWidget(bt.adjust, 'Set parameters to adjust the series', 'Set parameters to adjust the series')

		#######################

		framePlotAdj <- tkframe(subfr2)

		.cdtData$EnvData$plot$base <- tclVar(TRUE)
		.cdtData$EnvData$plot$mean <- tclVar(FALSE)
		.cdtData$EnvData$plot$qm <- tclVar(FALSE)

		frAdjSel <- tkframe(framePlotAdj, relief = 'sunken', borderwidth = 2)
		bt.PlotAdj <- ttkbutton(framePlotAdj, text = "Plot Adjusted Series")

		.cdtData$EnvData$tab$adjGraph <- NULL

		tkconfigure(bt.PlotAdj, command = function(){
			if(is.null(.cdtData$EnvData$adjS)){
				Insert.Messages.Out("No adjustment data found", format = TRUE)
				return(NULL)
			}
			stnid <- str_trim(tclvalue(.cdtData$EnvData$STN$stnID))

			imgContainer <- CDT.Display.Graph(homPlot_AdjustedSeries, .cdtData$EnvData$tab$adjGraph, paste0(stnid, '-Adjusted-Series'))
			.cdtData$EnvData$tab$adjGraph<- imageNotebookTab_unik(imgContainer, .cdtData$EnvData$tab$adjGraph)
		})


		chk.Adj.BaseS <- tkcheckbutton(frAdjSel, variable = .cdtData$EnvData$plot$base, text = "Base Series", anchor = 'w', justify = 'left')
		chk.Adj.Mean <- tkcheckbutton(frAdjSel, variable = .cdtData$EnvData$plot$mean, text = "Adjusted by Mean", anchor = 'w', justify = 'left')
		chk.Adj.QM <- tkcheckbutton(frAdjSel, variable = .cdtData$EnvData$plot$qm, text = "Adjusted by QM", anchor = 'w', justify = 'left')

		tkgrid(chk.Adj.BaseS, row = 0, column = 0, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 0, ipadx = 1, ipady = 1)
		tkgrid(chk.Adj.Mean, row = 1, column = 0, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 0, ipadx = 1, ipady = 1)
		tkgrid(chk.Adj.QM, row = 2, column = 0, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 0, ipadx = 1, ipady = 1)

		tkgrid(frAdjSel, row = 0, column = 0, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
		tkgrid(bt.PlotAdj, row = 0, column = 1, sticky = 'we', rowspan = 3, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)

		#######################

		bt.display.Info <- ttkbutton(subfr2, text = "Display Homogeneity Test Info")

		.cdtData$EnvData$tab$infoHom <- NULL

		tkconfigure(bt.display.Info, command = function(){
			stnid <- str_trim(tclvalue(.cdtData$EnvData$STN$stnID))
			if(stnid == "") return(NULL)
			STNID <- .cdtData$EnvData$output$data$id
			ids <- which(STNID == stnid)

			test <- .cdtData$EnvData$output$params$stats$mthd
			stats.test <- c("Pettitt Test", "SNHT (Alexandersson & Moberg, 1997)",
							"CUSUM-type (Gallagher et al., 2013)",
							"CUSUM-type with Trend (Gallagher et al., 2013)")
			sel.test <- c('Pettitt', 'SNHT', 'CUSUM', 'CUSUMtr')

			INFOs <- list(Stations = stnid,
						info = .cdtData$EnvData$output$series$msg[[stnid]],
						statistics = stats.test[sel.test %in% test],
						detection.method = "Binary Segmentation",
						penality = "Modified Bayes Information Criterion",
						changepoints.stats = local({
							cpt.stats <- lapply(seq(.cdtData$EnvData$cpt.stats), function(j){
								x <- .cdtData$EnvData$cpt.stats[[j]]
								if(is.null(x)) return(NULL)
								list(out = x[[stnid]], tstep = .cdtData$EnvData$candS[[j]]$tstep)
							})
							nomS <- sapply(cpt.stats, "[[", "tstep")
							cpt.stats <- lapply(cpt.stats, "[[", "out")
							names(cpt.stats) <- nomS
							cpt.stats
						}),
						reference.series = local({
							mat <- NULL
							if(.cdtData$EnvData$output$params$series$use){
								if(.cdtData$EnvData$output$params$series$user$refs){
									mat <- "Stations provided by User"
								}else{
									mat <- matrix(character(0), 2, 2)
									mat[1, 1] <- "Test series constitution :"
									mat[1, 2] <- c("Difference", "Ratio", "LogRatio")[.cdtData$EnvData$output$params$series$diff.ratio]
									mat[2, 1] <- "Weighting factors :"
									mat[2, 2] <- c("Correlation", "Distance", "Optimal")[.cdtData$EnvData$output$params$series$weight]
								}
							}
							mat
						}),
						neighborhood = .cdtData$EnvData$output$series$voisin1[[ids]])

			.cdtData$EnvData$tab$infoHom <- consolOutNotebookTab_unik(INFOs, .cdtData$EnvData$tab$infoHom, title = "Info-Test-Output")
		})

		#########################################

		tkgrid(frameOutQC, row = 0, column = 0, rowspan = 1, columnspan = 5, sticky = 'we', padx = 1, pady = 1, ipadx = 1, ipady = 1)
		tkgrid(frameStnId, row = 1, column = 0, rowspan = 1, columnspan = 5, sticky = 'we', padx = 1, pady = 1, ipadx = 1, ipady = 1)
		tkgrid(bt.display.Hom, row = 2, column = 0, rowspan = 1, columnspan = 3, sticky = 'we', padx = 1, pady = 1, ipadx = 1, ipady = 1)
		tkgrid(bt.undo.Hom, row = 2, column = 3, rowspan = 1, columnspan = 2, sticky = 'we', padx = 1, pady = 1, ipadx = 1, ipady = 1)
		tkgrid(framePlot, row = 3, column = 0, rowspan = 1, columnspan = 5, sticky = 'we', padx = 1, pady = 1, ipadx = 1, ipady = 1)
		tkgrid(frameAdjust, row = 4, column = 0, rowspan = 1, columnspan = 5, sticky = 'we', padx = 1, pady = 1, ipadx = 1, ipady = 1)
		tkgrid(framePlotAdj, row = 5, column = 0, rowspan = 1, columnspan = 5, sticky = '', padx = 1, pady = 1, ipadx = 1, ipady = 1)
		tkgrid(bt.display.Info, row = 6, column = 0, rowspan = 1, columnspan = 5, sticky = '', padx = 1, pady = 1, ipadx = 1, ipady = 1)


	#######################################################################################################

	set.station.id <- function(){
		STNID <- .cdtData$EnvData$output$data$id
		tkconfigure(.cdtData$EnvData$STN$cb.stnID, values = STNID)
		tclvalue(.cdtData$EnvData$STN$stnID) <- STNID[1]
	}

	display.cpt.output <- function(){
		stnid <- str_trim(tclvalue(.cdtData$EnvData$STN$stnID))
		if(stnid == "") return(NULL)

		cpt.table <- lapply(seq_along(.cdtData$EnvData$cpt.table), function(j){
			x <- .cdtData$EnvData$cpt.table[[j]]
			if(is.null(x)) return(NULL)
			out <- x[[stnid]]
			if(is.null(out)) return(NULL)
			data.frame(Time.Step = .cdtData$EnvData$candS[[j]]$tstep, out, stringsAsFactors = FALSE)
		})
		nb.cpt <- do.call(c, lapply(cpt.table, nrow))
		cpt.table <- do.call(rbind, rbind(cpt.table, NA))
		tab.title <- paste0(stnid, "-Test-Output")

		.cdtData$EnvData$tab$TableStat <- tableNotebookTab_unik(cpt.table,
															.cdtData$EnvData$tab$TableStat,
															tab.title, 12, 'outhom')
		defile.menu.OpenTable()
		tabid <- as.integer(tclvalue(tkindex(.cdtEnv$tcl$main$tknotes, 'current'))) + 1
		table1 <- .cdtData$OpenTab$Data[[tabid]][[2]][[1]]
		TableArray <- .cdtData$OpenTab$Data[[tabid]][[2]][[2]]

		cols <- paste(1:as.numeric(tclvalue(tkindex(table1, 'end', 'row'))), 2, sep = ',', collapse = ' ')
		.Tcl(paste(table1, 'tag', 'celltag', 'datyCol', cols))
		tcl(table1, "tag", "configure", "datyCol", bg = "lightgoldenrod1", anchor = "c")

		if(length(nb.cpt) > 1){
			sep.table <- cumsum(nb.cpt + 1)[-length(nb.cpt)]
			for(j in seq_along(sep.table)){
				rows <- paste(sep.table[j], 1:6, sep = ',', collapse = ' ')
				.Tcl(paste(table1, 'tag', 'celltag', paste0("sepTable", j), rows))
				tcl(table1, "tag", "configure", paste0("sepTable", j), bg = "red")
				.Tcl(paste(table1, "span", paste0(sep.table[j], ",3 0,3")))
				TableArray[sep.table[j], 3] <- "{Do not delete this row}"
			}
		}
	}

	.cdtData$EnvData$HomTest$SaveEdit <- function(dat2sav){
		stnid <- str_trim(tclvalue(.cdtData$EnvData$STN$stnID))

		for(j in 1:3){
			if(is.null(.cdtData$EnvData$cpt.table[[j]])) next
			if(is.null(.cdtData$EnvData$cpt.table[[j]][[stnid]])) next
			replaceNULL <- vector('list', 1)
			names(replaceNULL) <- stnid
			old.cpt.table <- .cdtData$EnvData$cpt.table[[j]]
			.cdtData$EnvData$cpt.table[[j]] <- utils::modifyList(old.cpt.table, replaceNULL, keep.null = TRUE)
		}

		if(!is.null(dat2sav)){
			daty <- as.character(dat2sav$Breakpoints.Date)
			if(length(daty) == 0) return(0)
			ina <- which(is.na(daty))
			s <- c(1, ina + 1)
			if(s[length(s)] > length(daty)) s[length(s)] <- length(daty)
			e <- c(ina - 1, length(daty))
			if(e[1] <= 0) e[1] <- 1
			ie <- e < s
			e[ie] <- s[ie]

			res <- lapply(seq_along(s), function(i){
				x <- dat2sav[s[i]:e[i], , drop = FALSE]
				idt <- str_trim(as.character(x$Breakpoints.Date))
				ix <- is.na(idt) | idt == ""
				if(all(ix)) return(NULL)
				x <- x[!ix, , drop = FALSE]
				tstep <- str_trim(as.character(x$Time.Step))
				tstep <- unique(tstep[!is.na(tstep) & tstep != ""])
				don <- data.frame(x[, -1, drop = FALSE], stringsAsFactors = FALSE)
				list(tstep = tstep, don = don)
			})

			inull <- sapply(res, is.null)

			if(!all(inull)){
				res <- res[!inull]
				tstep <- sapply(res, "[[", "tstep")
				don <- lapply(res, "[[", "don")
				instep1 <- c("daily", "dekadal", "monthly")
				instep2 <- c("pentad", "dekadal", "monthly")
				ii <- c(match(tstep, instep1), match(tstep, instep2))
				ii <- unique(ii[!is.na(ii)])
				for(j in seq_along(ii))
					.cdtData$EnvData$cpt.table[[ii[j]]][[stnid]] <- don[[j]]
			}
		}

		file.table <- file.path(.cdtData$EnvData$PathData, 'CDTDATASET', "BreaksPointsTable.rds")
		saveRDS(.cdtData$EnvData$cpt.table, file.table)
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
