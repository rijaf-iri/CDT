
AssessDataPanelCmd <- function(){
	listOpenFiles <- openFile_ttkcomboList()
	if(WindowsOS()){
		largeur1 <- .cdtEnv$tcl$fun$w.widgets(27)
		largeur2 <- .cdtEnv$tcl$fun$w.widgets(29)
		largeur3 <- 19
		largeur4 <- 21
	}else{
		largeur1 <- .cdtEnv$tcl$fun$w.widgets(22)
		largeur2 <- .cdtEnv$tcl$fun$w.widgets(23)
		largeur3 <- 12
		largeur4 <- 14
	}

	GeneralParameters <- list(intstep = "daily", infile = "", outdir = "")

	# xml.dlg <- file.path(.cdtDir$dirLocal, "languages", "cdtDataAvailab_leftCmd.xml")
	# lang.dlg <- cdtLanguageParse(xml.dlg, .cdtData$Config$lang.iso)
	# .cdtData$EnvData$message <- lang.dlg[['message']]

	###################

	.cdtEnv$tcl$main$cmd.frame <- tkframe(.cdtEnv$tcl$main$panel.left)

	tknote.cmd <- bwNoteBook(.cdtEnv$tcl$main$cmd.frame)
	cmd.tab1 <- bwAddTab(tknote.cmd, text = "Data")
	cmd.tab2 <- bwAddTab(tknote.cmd, text = "Maps")
	cmd.tab3 <- bwAddTab(tknote.cmd, text = "Graphs")
	cmd.tab4 <- bwAddTab(tknote.cmd, text = "Boundaries")

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

		#######################

		frameTimeS <- ttklabelframe(subfr1, text = "Time step of input data", relief = 'groove')

		timeSteps <- tclVar()
		CbperiodVAL <- .cdtEnv$tcl$lang$global[['combobox']][['1']][2:5]
		periodVAL <- c('daily', 'pentad', 'dekadal', 'monthly')
		tclvalue(timeSteps) <- CbperiodVAL[periodVAL %in% GeneralParameters$intstep]

		cb.fperiod <- ttkcombobox(frameTimeS, values = CbperiodVAL, textvariable = timeSteps, width = largeur1)

		tkgrid(cb.fperiod, row = 0, column = 0, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)

		helpWidget(cb.fperiod, 'Select the time step of the data', 'Select the time step of the data')

		#######################

		frameInData <- ttklabelframe(subfr1, text = "Input Data", relief = 'groove')

		input.file <- tclVar(GeneralParameters$infile)

		txt.infile <- tklabel(frameInData, text = 'File containing stations data', anchor = 'w', justify = 'left')
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

		assesDataBut <- ttkbutton(subfr1, text = "Assess Data Availability")

		tkconfigure(assesDataBut, command = function(){
			GeneralParameters$intstep <- periodVAL[CbperiodVAL %in% str_trim(tclvalue(timeSteps))]
			GeneralParameters$infile <- str_trim(tclvalue(input.file))
			GeneralParameters$outdir <- str_trim(tclvalue(dir.save))

			# assign('GeneralParameters', GeneralParameters, envir = .GlobalEnv)

			Insert.Messages.Out("Assess data availability ......")

			tkconfigure(.cdtEnv$tcl$main$win, cursor = 'watch')
			tcl('update')
			ret <- tryCatch(
				{
					AssessDataAvailProcs(GeneralParameters)
				},
				warning = function(w) warningFun(w),
				error = function(e) errorFun(e),
				finally = {
					tkconfigure(.cdtEnv$tcl$main$win, cursor = '')
					tcl('update')
				}
			)

			msg0 <- "Assessing data availability finished successfully"
			msg1 <- "Assessing data availability failed"

			if(!is.null(ret)){
				if(ret == 0){
					Insert.Messages.Out(msg0)

					.cdtData$EnvData$plot.maps$data.type <- "cdtstation"
					.cdtData$EnvData$plot.maps[c('lon', 'lat', 'id')] <- .cdtData$EnvData$output$data[c('lon', 'lat', 'id')]
					###################

					set.Data.Year()
					set.Stations.IDs()
				}else Insert.Messages.Out(msg1, format = TRUE)
			}else Insert.Messages.Out(msg1, format = TRUE)
		})

		##############################################

		tkgrid(frameTimeS, row = 0, column = 0, sticky = '', padx = 1, pady = 1, ipadx = 1, ipady = 1)
		tkgrid(frameInData, row = 1, column = 0, sticky = 'we', padx = 1, pady = 3, ipadx = 1, ipady = 1)
		tkgrid(frameDirSav, row = 2, column = 0, sticky = 'we', padx = 1, pady = 1, ipadx = 1, ipady = 1)
		tkgrid(assesDataBut, row = 3, column = 0, sticky = 'we', padx = 1, pady = 3, ipadx = 1, ipady = 1)

	#######################################################################################################

	#Tab2
	subfr2 <- bwTabScrollableFrame(cmd.tab2)

		#######################

		frameAssesDat <- ttklabelframe(subfr2, text = "Data availability", relief = 'groove')

		.cdtData$EnvData$DirExist <- tclVar(0)
		file.Index <- tclVar()

		stateAssesDat <- if(tclvalue(.cdtData$EnvData$DirExist) == "1") "normal" else "disabled"

		chk.dataIdx <- tkcheckbutton(frameAssesDat, variable = .cdtData$EnvData$DirExist, text = "Data availability output already exist", anchor = 'w', justify = 'left')
		en.dataIdx <- tkentry(frameAssesDat, textvariable = file.Index, width = largeur2, state = stateAssesDat)
		bt.dataIdx <- tkbutton(frameAssesDat, text = "...", state = stateAssesDat)

		tkconfigure(bt.dataIdx, command = function(){
			path.Stat <- tclvalue(tkgetOpenFile(initialdir = getwd(), filetypes = .cdtEnv$tcl$data$filetypes6))
			if(path.Stat %in% c("", "NA") | is.na(path.Stat)) return(NULL)
			tclvalue(file.Index) <- path.Stat

			if(file.exists(str_trim(tclvalue(file.Index)))){
				Outdata <- try(readRDS(str_trim(tclvalue(file.Index))), silent = TRUE)
				if(inherits(Outdata, "try-error")){
					Insert.Messages.Out(gsub('[\r\n]', '', Outdata[1]), format = TRUE)
					Insert.Messages.Out('Unable to load data', format = TRUE)

					.cdtData$EnvData$yearly <- NULL
					.cdtData$EnvData$monthly <- NULL

					tkconfigure(cb.year.Date, values = "")
					tclvalue(.cdtData$EnvData$availDate) <- ""
					tkconfigure(cb.stn.ID, values = "")
					tclvalue(.cdtData$EnvData$plot.maps$stnIDTSp) <- ""
					return(NULL)
				}

				.cdtData$EnvData$output <- Outdata
				.cdtData$EnvData$PathData <- dirname(str_trim(tclvalue(file.Index)))
				.cdtData$EnvData$plot.maps$data.type <- "cdtstation"
				.cdtData$EnvData$plot.maps[c('lon', 'lat', 'id')] <- .cdtData$EnvData$output$data[c('lon', 'lat', 'id')]
				###################

				file.mon <- file.path(.cdtData$EnvData$PathData, 'CDTDATASET', "Monthly_non-missing.rds")
				months <- try(readRDS(file.mon), silent = TRUE)
				if(inherits(months, "try-error")){
					Insert.Messages.Out(gsub('[\r\n]', '', months[1]), format = TRUE)
					Insert.Messages.Out(paste('Unable to read', file.mon), format = TRUE)
					.cdtData$EnvData$monthly <- NULL
					return(NULL)
				}else .cdtData$EnvData$monthly <- months

				file.year <- file.path(.cdtData$EnvData$PathData, 'CDTDATASET', "Yearly_non-missing.rds")
				years <- try(readRDS(file.year), silent = TRUE)
				if(inherits(years, "try-error")){
					Insert.Messages.Out(gsub('[\r\n]', '', years[1]), format = TRUE)
					Insert.Messages.Out(paste('Unable to read', file.year), format = TRUE)
					.cdtData$EnvData$yearly <- NULL
					return(NULL)
				}else .cdtData$EnvData$yearly <- years

				set.Data.Year()
				set.Stations.IDs()
			}
		})

		tkgrid(chk.dataIdx, row = 0, column = 0, sticky = 'we', rowspan = 1, columnspan = 5, padx = 1, pady = 1, ipadx = 1, ipady = 1)
		tkgrid(en.dataIdx, row = 1, column = 0, sticky = 'we', rowspan = 1, columnspan = 4, padx = 1, pady = 1, ipadx = 1, ipady = 1)
		tkgrid(bt.dataIdx, row = 1, column = 4, sticky = 'w', rowspan = 1, columnspan = 1, padx = 0, pady = 1, ipadx = 1, ipady = 1)

		###############

		tkbind(chk.dataIdx, "<Button-1>", function(){
			stateAssesDat <- if(tclvalue(.cdtData$EnvData$DirExist) == '1') 'disabled' else 'normal'
			tkconfigure(en.dataIdx, state = stateAssesDat)
			tkconfigure(bt.dataIdx, state = stateAssesDat)
			stateAssesBut <- if(tclvalue(.cdtData$EnvData$DirExist) == '1') 'normal' else 'disabled'
			tkconfigure(assesDataBut, state = stateAssesBut)
		})

		#######################

		frameAvailab <- ttklabelframe(subfr2, text = "Availability", relief = 'groove')

		bt.avai.Map <- ttkbutton(frameAvailab, text = .cdtEnv$tcl$lang$global[['button']][['3']])
		bt.avai.MapOpt <- ttkbutton(frameAvailab, text = .cdtEnv$tcl$lang$global[['button']][['4']])

		###############

		.cdtData$EnvData$avaiMapOp <- list(presetCol = list(color = 'tim.colors', reverse = FALSE),
											userCol = list(custom = FALSE, color = NULL),
											userLvl = list(custom = FALSE, levels = NULL, equidist = FALSE),
											title = list(user = FALSE, title = ''),
											colkeyLab = list(user = FALSE, label = ''),
											scalebar = list(add = FALSE, pos = 'bottomleft'),
											pointSize = 0.8)

		tkconfigure(bt.avai.MapOpt, command = function(){
			if(!is.null(.cdtData$EnvData$anomdata$map)){
				atlevel <- pretty(.cdtData$EnvData$anomdata$map$z, n = 10, min.n = 7)
				if(is.null(.cdtData$EnvData$avaiMapOp$userLvl$levels)){
					.cdtData$EnvData$avaiMapOp$userLvl$levels <- atlevel
				}else{
					if(!.cdtData$EnvData$avaiMapOp$userLvl$custom)
						.cdtData$EnvData$avaiMapOp$userLvl$levels <- atlevel
				}
			}
			.cdtData$EnvData$avaiMapOp <- MapGraph.MapOptions(.cdtData$EnvData$avaiMapOp)
		})

		###############

		.cdtData$EnvData$tab$availMap <- NULL
		tkconfigure(bt.avai.Map, command = function(){
			if(is.null(.cdtData$EnvData$output)) return(NULL)
			get.DataAvailable.Map()

			typeTSp <- str_trim(tclvalue(.cdtData$EnvData$plot.maps$typeTSp))
			titre.graph <- if(typeTSp == "Chart") "Yearly-Chart" else "Monthly-Table"
			assessData_displayAvailability('Availability-Map', titre.graph)
		})

		###############

		tkgrid(bt.avai.Map, row = 0, column = 0, sticky = 'we', rowspan = 1, columnspan = 1, padx = 10, pady = 1, ipadx = 10, ipady = 1)
		tkgrid(bt.avai.MapOpt, row = 0, column = 1, sticky = 'we', rowspan = 1, columnspan = 1, padx = 10, pady = 1, ipadx = 10, ipady = 1)

		#######################

		frameYearMap <- ttklabelframe(subfr2, text = "Yearly Maps", relief = 'groove')

		.cdtData$EnvData$availDate <- tclVar()

		cb.year.Date <- ttkcombobox(frameYearMap, values = "", textvariable = .cdtData$EnvData$availDate, width = largeur3, justify = 'center')
		bt.year.Date.prev <- ttkbutton(frameYearMap, text = "<<", width = 4)
		bt.year.Date.next <- ttkbutton(frameYearMap, text = ">>", width = 4)
		bt.year.maps <- ttkbutton(frameYearMap, text = .cdtEnv$tcl$lang$global[['button']][['3']], width = 7)
		bt.year.MapOpt <- ttkbutton(frameYearMap, text = .cdtEnv$tcl$lang$global[['button']][['4']], width = 7)

		###############

		.cdtData$EnvData$yearMapOp <- list(presetCol = list(color = 'tim.colors', reverse = FALSE),
											userCol = list(custom = FALSE, color = NULL),
											userLvl = list(custom = FALSE, levels = NULL, equidist = FALSE),
											title = list(user = FALSE, title = ''),
											colkeyLab = list(user = FALSE, label = ''),
											scalebar = list(add = FALSE, pos = 'bottomleft'),
											pointSize = 0.8)

		tkconfigure(bt.year.MapOpt, command = function(){
			if(!is.null(.cdtData$EnvData$anomdata$map)){
				atlevel <- pretty(.cdtData$EnvData$anomdata$map$z, n = 10, min.n = 7)
				if(is.null(.cdtData$EnvData$yearMapOp$userLvl$levels)){
					.cdtData$EnvData$yearMapOp$userLvl$levels <- atlevel
				}else{
					if(!.cdtData$EnvData$yearMapOp$userLvl$custom)
						.cdtData$EnvData$yearMapOp$userLvl$levels <- atlevel
				}
			}
			.cdtData$EnvData$yearMapOp <- MapGraph.MapOptions(.cdtData$EnvData$yearMapOp)
		})

		###############

		.cdtData$EnvData$tab$yearMap <- NULL

		tkconfigure(bt.year.maps, command = function(){
			if(str_trim(tclvalue(.cdtData$EnvData$availDate)) != "" &
				!is.null(.cdtData$EnvData$yearly))
			{
				get.DataYearly.Map()

				typeTSp <- str_trim(tclvalue(.cdtData$EnvData$plot.maps$typeTSp))
				titre.graph <- if(typeTSp == "Chart") "Yearly-Chart" else "Monthly-Table"
				assessData_displayYearlyData('Yearly-Map', titre.graph)
			}
		})

		tkconfigure(bt.year.Date.prev, command = function(){
			if(str_trim(tclvalue(.cdtData$EnvData$availDate)) != ""){
				donDates <- as.character(.cdtData$EnvData$yearly$year)
				idaty <- which(donDates == str_trim(tclvalue(.cdtData$EnvData$availDate)))
				idaty <- idaty - 1
				if(idaty < 1) idaty <- length(donDates)
				tclvalue(.cdtData$EnvData$availDate) <- donDates[idaty]
				get.DataYearly.Map()

				typeTSp <- str_trim(tclvalue(.cdtData$EnvData$plot.maps$typeTSp))
				titre.graph <- if(typeTSp == "Chart") "Yearly-Chart" else "Monthly-Table"
				assessData_displayYearlyData('Yearly-Map', titre.graph)
			}
		})

		tkconfigure(bt.year.Date.next, command = function(){
			if(str_trim(tclvalue(.cdtData$EnvData$availDate)) != ""){
				donDates <- as.character(.cdtData$EnvData$yearly$year)
				idaty <- which(donDates == str_trim(tclvalue(.cdtData$EnvData$availDate)))
				idaty <- idaty + 1
				if(idaty > length(donDates)) idaty <- 1
				tclvalue(.cdtData$EnvData$availDate) <- donDates[idaty]
				get.DataYearly.Map()

				typeTSp <- str_trim(tclvalue(.cdtData$EnvData$plot.maps$typeTSp))
				titre.graph <- if(typeTSp == "Chart") "Yearly-Chart" else "Monthly-Table"
				assessData_displayYearlyData('Yearly-Map', titre.graph)
			}
		})

		###############

		tkgrid(bt.year.Date.prev, row = 0, column = 0, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
		tkgrid(cb.year.Date, row = 0, column = 1, sticky = 'we', rowspan = 1, columnspan = 2, padx = 1, pady = 1, ipadx = 1, ipady = 1)
		tkgrid(bt.year.Date.next, row = 0, column = 3, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
		tkgrid(bt.year.maps, row = 0, column = 4, sticky = 'we', rowspan = 1, columnspan = 2, padx = 1, pady = 1, ipadx = 1, ipady = 1)
		tkgrid(bt.year.MapOpt, row = 1, column = 4, sticky = 'we', rowspan = 1, columnspan = 2, padx = 1, pady = 1, ipadx = 1, ipady = 1)

		#######################

		framePlotType <- tkframe(subfr2)

		.cdtData$EnvData$plot.maps$plot.type <- tclVar("Pixels")

		txt.plotType <- tklabel(framePlotType, text = "Plot Type", anchor = 'e', justify = 'right')
		cb.plotType <- ttkcombobox(framePlotType, values = c("Pixels", "Points"), textvariable = .cdtData$EnvData$plot.maps$plot.type, width = largeur4)

		tkgrid(txt.plotType, row = 0, column = 0, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
		tkgrid(cb.plotType, row = 0, column = 1, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)

		##############################################

		tkgrid(frameAssesDat, row = 0, column = 0, sticky = 'we', padx = 1, pady = 1, ipadx = 1, ipady = 1)
		tkgrid(frameAvailab, row = 1, column = 0, sticky = '', padx = 1, pady = 3, ipadx = 1, ipady = 1)
		tkgrid(frameYearMap, row = 2, column = 0, sticky = 'we', padx = 1, pady = 3, ipadx = 1, ipady = 1)
		tkgrid(framePlotType, row = 3, column = 0, sticky = '', padx = 1, pady = 3, ipadx = 1, ipady = 1)

	#######################################################################################################

	#Tab3
	subfr3 <- bwTabScrollableFrame(cmd.tab3)

		#######################

		frameStation <- ttklabelframe(subfr3, text = "Non-Missing Data Summary", relief = 'groove')

		.cdtData$EnvData$plot.maps$stnIDTSp <- tclVar()
		plotTYPE <- c("Chart", "Table")
		.cdtData$EnvData$plot.maps$typeTSp <- tclVar('Chart')

		cb.stn.ID <- ttkcombobox(frameStation, values = "", textvariable = .cdtData$EnvData$plot.maps$stnIDTSp, width = largeur4, justify = 'center')
		bt.stn.prev <- ttkbutton(frameStation, text = "<<", width = 3)
		bt.stn.next <- ttkbutton(frameStation, text = ">>", width = 3)
		bt.stn.plot <- ttkbutton(frameStation, text = .cdtEnv$tcl$lang$global[['button']][['3']], width = 7)
		bt.stn.opt <- ttkbutton(frameStation, text = .cdtEnv$tcl$lang$global[['button']][['4']], width = 7)
		cb.stn.table <- ttkcombobox(frameStation, values = plotTYPE, textvariable = .cdtData$EnvData$plot.maps$typeTSp, width = largeur4)

		#################

		.cdtData$EnvData$TSGraphOp <- list(
											axislabs = list(is.xlab = FALSE, xlab = '', is.ylab = FALSE, ylab = ''),
											title = list(is.title = FALSE, title = ''),
											colors = list(col = "cyan4")
										)

		tkconfigure(bt.stn.opt, command = function(){
			.cdtData$EnvData$TSGraphOp <- MapGraph.GraphOptions.Assess(.cdtData$EnvData$TSGraphOp)
		})

		###############

		.cdtData$EnvData$tab$yearlyTS <- NULL
		.cdtData$EnvData$tab$Table <- NULL

		tkconfigure(bt.stn.plot, command = function(){
			if(str_trim(tclvalue(.cdtData$EnvData$plot.maps$typeTSp)) == "Chart"){
				if(is.null(.cdtData$EnvData$yearly)) return(NULL)
				imgContainer1 <- assessData_displayYearlyTS(.cdtData$EnvData$tab$yearlyTS, "Yearly-Chart")
				.cdtData$EnvData$tab$yearlyTS <- imageNotebookTab_unik(imgContainer1, .cdtData$EnvData$tab$yearlyTS)
			}else{
				if(is.null(.cdtData$EnvData$monthly)) return(NULL)
				table.mon <- assessData_dataMonthly()
				if(is.null(table.mon)) return(NULL)
				.cdtData$EnvData$tab$Table <- tableNotebookTab_unik(table.mon, .cdtData$EnvData$tab$Table, "Monthly-Table")
			}
		})

		tkconfigure(bt.stn.prev, command = function(){
			stnIDTSPLOT <- .cdtData$EnvData$output$data$id
			istn <- which(stnIDTSPLOT == str_trim(tclvalue(.cdtData$EnvData$plot.maps$stnIDTSp)))
			istn <- istn - 1
			if(istn < 1) istn <- length(stnIDTSPLOT)
			tclvalue(.cdtData$EnvData$plot.maps$stnIDTSp) <- stnIDTSPLOT[istn]

			if(str_trim(tclvalue(.cdtData$EnvData$plot.maps$typeTSp)) == "Chart"){
				if(is.null(.cdtData$EnvData$yearly)) return(NULL)
				imgContainer1 <- assessData_displayYearlyTS(.cdtData$EnvData$tab$yearlyTS, "Yearly-Chart")
				.cdtData$EnvData$tab$yearlyTS <- imageNotebookTab_unik(imgContainer1, .cdtData$EnvData$tab$yearlyTS)
			}else{
				if(is.null(.cdtData$EnvData$monthly)) return(NULL)
				table.mon <- assessData_dataMonthly()
				if(is.null(table.mon)) return(NULL)
				.cdtData$EnvData$tab$Table <- tableNotebookTab_unik(table.mon, .cdtData$EnvData$tab$Table, "Monthly-Table")
			}
		})

		tkconfigure(bt.stn.next, command = function(){
			stnIDTSPLOT <- .cdtData$EnvData$output$data$id
			istn <- which(stnIDTSPLOT == str_trim(tclvalue(.cdtData$EnvData$plot.maps$stnIDTSp)))
			istn <- istn + 1
			if(istn > length(stnIDTSPLOT)) istn <- 1
			tclvalue(.cdtData$EnvData$plot.maps$stnIDTSp) <- stnIDTSPLOT[istn]

			if(str_trim(tclvalue(.cdtData$EnvData$plot.maps$typeTSp)) == "Chart"){
				if(is.null(.cdtData$EnvData$yearly)) return(NULL)
				imgContainer1 <- assessData_displayYearlyTS(.cdtData$EnvData$tab$yearlyTS, "Yearly-Chart")
				.cdtData$EnvData$tab$yearlyTS <- imageNotebookTab_unik(imgContainer1, .cdtData$EnvData$tab$yearlyTS)
			}else{
				if(is.null(.cdtData$EnvData$monthly)) return(NULL)
				table.mon <- assessData_dataMonthly()
				if(is.null(table.mon)) return(NULL)
				.cdtData$EnvData$tab$Table <- tableNotebookTab_unik(table.mon, .cdtData$EnvData$tab$Table, "Monthly-Table")
			}
		})

		###############

		tkgrid(bt.stn.prev, row = 0, column = 0, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
		tkgrid(cb.stn.ID, row = 0, column = 1, sticky = 'we', rowspan = 1, columnspan = 2, padx = 1, pady = 1, ipadx = 1, ipady = 1)
		tkgrid(bt.stn.next, row = 0, column = 3, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
		tkgrid(bt.stn.plot, row = 0, column = 4, sticky = 'we', rowspan = 1, columnspan = 2, padx = 1, pady = 1, ipadx = 1, ipady = 1)
		tkgrid(cb.stn.table, row = 1, column = 1, sticky = 'we', rowspan = 1, columnspan = 2, padx = 1, pady = 1, ipadx = 1, ipady = 1)
		tkgrid(bt.stn.opt, row = 1, column = 4, sticky = 'we', rowspan = 1, columnspan = 2, padx = 1, pady = 1, ipadx = 1, ipady = 1)

		###############

		tkbind(cb.stn.table, "<<ComboboxSelected>>", function(){
			if(str_trim(tclvalue(.cdtData$EnvData$plot.maps$typeTSp)) == "Chart"){
				tkconfigure(bt.stn.plot, text = .cdtEnv$tcl$lang$global[['button']][['3']])
				tkconfigure(bt.stn.opt, state = "normal")
			}else{
				tkconfigure(bt.stn.plot, text = "DISPLAY")
				tkconfigure(bt.stn.opt, state = "disabled")
			}
		})

		#######################

		bt.YearAvg <- ttkbutton(subfr3, text = "Non-Missing Data Annual Average")

		.cdtData$EnvData$tab$stnAvrg
		tkconfigure(bt.YearAvg, command = function(){
			if(is.null(.cdtData$EnvData$PathData)) return(NULL)
			imgContainer <- CDT.Display.Graph(assessData_plotStnAnnual, .cdtData$EnvData$tab$stnAvrg, 'Annual-Average')
			.cdtData$EnvData$tab$stnAvrg <- imageNotebookTab_unik(imgContainer, .cdtData$EnvData$tab$stnAvrg)
		})

		#######################

		bt.AllnonNA <- ttkbutton(subfr3, text = "Non-Missing Data Summary")

		.cdtData$EnvData$tab$stnActive <- NULL
		tkconfigure(bt.AllnonNA, command = function(){
			if(is.null(.cdtData$EnvData$PathData)) return(NULL)
			if(is.null(.cdtData$EnvData$output)) return(NULL)
			imgContainer <- assessData_displayActivities(.cdtData$EnvData$tab$stnActive)
			.cdtData$EnvData$tab$stnActive <- imageNotebookTab_unik(imgContainer, .cdtData$EnvData$tab$stnActive)
		})

		#######################

		bt.DistCor <- ttkbutton(subfr3, text = "Distance - Correlation")

		.cdtData$EnvData$tab$distCor
		tkconfigure(bt.DistCor, command = function(){
			if(is.null(.cdtData$EnvData$PathData)) return(NULL)
			imgContainer <- CDT.Display.Graph(assessData_plotDistCor, .cdtData$EnvData$tab$distCor, 'Distance-Correlation')
			.cdtData$EnvData$tab$distCor <- imageNotebookTab_unik(imgContainer, .cdtData$EnvData$tab$distCor)
		})

		##############################################

		tkgrid(frameStation, row = 0, column = 0, sticky = 'we', padx = 1, pady = 1, ipadx = 1, ipady = 1)
		tkgrid(bt.YearAvg, row = 1, column = 0, sticky = 'we', padx = 1, pady = 3, ipadx = 1, ipady = 1)
		tkgrid(bt.AllnonNA, row = 2, column = 0, sticky = 'we', padx = 1, pady = 3, ipadx = 1, ipady = 1)
		tkgrid(bt.DistCor, row = 3, column = 0, sticky = 'we', padx = 1, pady = 3, ipadx = 1, ipady = 1)

	#######################################################################################################

	#Tab4
	subfr4 <- bwTabScrollableFrame(cmd.tab4)

		#######################

		frameSHP <- ttklabelframe(subfr4, text = "Boundaries", relief = 'groove')

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

	set.Data.Year <- function(){
		YEAR <- .cdtData$EnvData$yearly$year
		tkconfigure(cb.year.Date, values = YEAR)
		tclvalue(.cdtData$EnvData$availDate) <- YEAR[length(YEAR)]
	}

	set.Stations.IDs <- function(){
		stnIDTSPLOT <- .cdtData$EnvData$output$data$id
		tkconfigure(cb.stn.ID, values = stnIDTSPLOT)
		tclvalue(.cdtData$EnvData$plot.maps$stnIDTSp) <- stnIDTSPLOT[1]
	}

	######################

	get.DataAvailable.Map <- function(){
		tkconfigure(.cdtEnv$tcl$main$win, cursor = 'watch')
		tcl('update')
		on.exit({
			tkconfigure(.cdtEnv$tcl$main$win, cursor = '')
			tcl('update')
		})

		change.plot <- str_trim(tclvalue(.cdtData$EnvData$plot.maps$plot.type))

		formatData <- TRUE
		if(!is.null(.cdtData$EnvData$change.plotA))
			if(.cdtData$EnvData$change.plotA == change.plot) formatData <- FALSE

		if(formatData){
			x <- .cdtData$EnvData$output$data$lon
			y <- .cdtData$EnvData$output$data$lat
			tmp <- .cdtData$EnvData$output$Avai

			if(change.plot == "Pixels"){
				nx <- nx_ny_as.image(diff(range(x)))
				ny <- nx_ny_as.image(diff(range(y)))
				tmp <- cdt.as.image(tmp, nx = nx, ny = ny, pts.xy = cbind(x, y))
				.cdtData$EnvData$DataAvail$map$x <- tmp$x
				.cdtData$EnvData$DataAvail$map$y <- tmp$y
				.cdtData$EnvData$DataAvail$map$z <- tmp$z
			}

			if(change.plot == "Points"){
				.cdtData$EnvData$DataAvail$map$x <- x
				.cdtData$EnvData$DataAvail$map$y <- y
				.cdtData$EnvData$DataAvail$map$z <- tmp
			}

			.cdtData$EnvData$change.plotA <- change.plot
		}
	}

	get.DataYearly.Map <- function(){
		tkconfigure(.cdtEnv$tcl$main$win, cursor = 'watch')
		tcl('update')
		on.exit({
			tkconfigure(.cdtEnv$tcl$main$win, cursor = '')
			tcl('update')
		})

		change.plot <- str_trim(tclvalue(.cdtData$EnvData$plot.maps$plot.type))
		this.daty <- str_trim(tclvalue(.cdtData$EnvData$availDate))

		formatData <- TRUE
		if(!is.null(.cdtData$EnvData$DataYear))
			if(!is.null(.cdtData$EnvData$DataYear$this.daty))
				if(.cdtData$EnvData$DataYear$this.daty == this.daty) formatData <- FALSE

		if(!formatData)
			if(.cdtData$EnvData$change.plotY != change.plot) formatData <- TRUE

		if(formatData){
			donDates <- as.character(.cdtData$EnvData$yearly$year)
			idt <- which(donDates == this.daty)
			x <- .cdtData$EnvData$output$data$lon
			y <- .cdtData$EnvData$output$data$lat
			tmp <- as.numeric(.cdtData$EnvData$yearly$data[idt, ])

			if(change.plot == "Pixels"){
				nx <- nx_ny_as.image(diff(range(x)))
				ny <- nx_ny_as.image(diff(range(y)))
				tmp <- cdt.as.image(tmp, nx = nx, ny = ny, pts.xy = cbind(x, y))
				.cdtData$EnvData$DataYear$map$x <- tmp$x
				.cdtData$EnvData$DataYear$map$y <- tmp$y
				.cdtData$EnvData$DataYear$map$z <- tmp$z
			}

			if(change.plot == "Points"){
				.cdtData$EnvData$DataYear$map$x <- x
				.cdtData$EnvData$DataYear$map$y <- y
				.cdtData$EnvData$DataYear$map$z <- tmp
			}

			.cdtData$EnvData$change.plotY <- change.plot
			.cdtData$EnvData$DataYear$this.daty <- this.daty
		}
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
