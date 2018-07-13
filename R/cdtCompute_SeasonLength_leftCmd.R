
SeasonLengthCalcPanelCmd <- function(){
	listOpenFiles <- openFile_ttkcomboList()
	if(WindowsOS()){
		largeur0 <- .cdtEnv$tcl$fun$w.widgets(29)
		largeur1 <- .cdtEnv$tcl$fun$w.widgets(27)
		largeur2 <- 21
	}else{
		largeur0 <- .cdtEnv$tcl$fun$w.widgets(23)
		largeur1 <- .cdtEnv$tcl$fun$w.widgets(21)
		largeur2 <- 14
	}

	GeneralParameters <- list(onset = "", cessation = "", output = "")

	# xml.dlg <- file.path(.cdtDir$dirLocal, "languages", "cdtCompute_SeasonLength_leftCmd.xml")
	# lang.dlg <- cdtLanguageParse(xml.dlg, .cdtData$Config$lang.iso)
	# .cdtData$EnvData$message <- lang.dlg[['message']]

	###################

	.cdtEnv$tcl$main$cmd.frame <- tkframe(.cdtEnv$tcl$main$panel.left)

	tknote.cmd <- bwNoteBook(.cdtEnv$tcl$main$cmd.frame)

	cmd.tab1 <- bwAddTab(tknote.cmd, text = "Season Length")
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

		############################################

		frameInData <- ttklabelframe(subfr1, text = "Onset & Cessation", relief = 'groove')

		input.Onset <- tclVar(GeneralParameters$onset)
		input.Cessation <- tclVar(GeneralParameters$cessation)

		txt.Ons <- tklabel(frameInData, text = 'Path to the onset data <Onset.rds>', anchor = 'w', justify = 'left')
		en.Ons <- tkentry(frameInData, textvariable = input.Onset, width = largeur0)
		bt.Ons <- tkbutton(frameInData, text = "...")

		txt.Ces <- tklabel(frameInData, text = 'Path to the cessation data <Cessation.rds>', anchor = 'w', justify = 'left')
		en.Ces <- tkentry(frameInData, textvariable = input.Cessation, width = largeur0)
		bt.Ces <- tkbutton(frameInData, text = "...")

		tkconfigure(bt.Ons, command = function(){
			path.rds <- tclvalue(tkgetOpenFile(initialdir = getwd(), filetypes = .cdtEnv$tcl$data$filetypes6))
			tclvalue(input.Onset) <- if(path.rds %in% c("", "NA") | is.na(path.rds)) "" else path.rds
		})

		tkconfigure(bt.Ces, command = function(){
			path.rds <- tclvalue(tkgetOpenFile(initialdir = getwd(), filetypes = .cdtEnv$tcl$data$filetypes6))
			tclvalue(input.Cessation) <- if(path.rds %in% c("", "NA") | is.na(path.rds)) "" else path.rds
		})

		tkgrid(txt.Ons, row = 0, column = 0, sticky = 'we', rowspan = 1, columnspan = 10, padx = 1, pady = 1, ipadx = 1, ipady = 1)
		tkgrid(en.Ons, row = 1, column = 0, sticky = 'we', rowspan = 1, columnspan = 9, padx = 0, pady = 1, ipadx = 1, ipady = 1)
		tkgrid(bt.Ons, row = 1, column = 9, sticky = 'w', rowspan = 1, columnspan = 1, padx = 0, pady = 1, ipadx = 1, ipady = 1)
		tkgrid(txt.Ces, row = 2, column = 0, sticky = 'we', rowspan = 1, columnspan = 10, padx = 1, pady = 1, ipadx = 1, ipady = 1)
		tkgrid(en.Ces, row = 3, column = 0, sticky = 'we', rowspan = 1, columnspan = 9, padx = 0, pady = 1, ipadx = 1, ipady = 1)
		tkgrid(bt.Ces, row = 3, column = 9, sticky = 'w', rowspan = 1, columnspan = 1, padx = 0, pady = 1, ipadx = 1, ipady = 1)

		############
		infobulle(en.Ons, 'Enter the full path to the file <Onset.rds>')
		status.bar.display(en.Ons, 'Enter the full path to the file <Onset.rds>')
		infobulle(en.Ces, 'Enter the full path to the file <Cessation.rds>')
		status.bar.display(en.Ces, 'Enter the full path to the file <Cessation.rds>')

		infobulle(bt.Ons, 'or browse here')
		status.bar.display(bt.Ons, 'or browse here')
		infobulle(bt.Ces, 'or browse here')
		status.bar.display(bt.Ces, 'or browse here')

		############################################

		frameDirSav <- tkframe(subfr1, relief = 'groove', borderwidth = 2)

		dir.save <- tclVar(GeneralParameters$output)

		txt.dir.save <- tklabel(frameDirSav, text = "Directory to save results", anchor = 'w', justify = 'left')
		en.dir.save <- tkentry(frameDirSav, textvariable = dir.save, width = largeur0)
		bt.dir.save <- tkbutton(frameDirSav, text = "...")

		######
		tkconfigure(bt.dir.save, command = function() fileORdir2Save(dir.save, isFile = FALSE))

		######
		tkgrid(txt.dir.save, row = 0, column = 0, sticky = 'we', rowspan = 1, columnspan = 5, padx = 1, pady = 0, ipadx = 1, ipady = 1)
		tkgrid(en.dir.save, row = 1, column = 0, sticky = 'we', rowspan = 1, columnspan = 4, padx = 0, pady = 0, ipadx = 1, ipady = 1)
		tkgrid(bt.dir.save, row = 1, column = 4, sticky = 'w', rowspan = 1, columnspan = 1, padx = 0, pady = 0, ipadx = 1, ipady = 1)

		infobulle(en.dir.save, 'Enter the full path to directory to save outputs')
		status.bar.display(en.dir.save, 'Enter the full path to directory to save outputs')
		infobulle(bt.dir.save, 'or browse here')
		status.bar.display(bt.dir.save, 'or browse here')

		############################################

		frameCalc <- tkframe(subfr1)

		if(!is.null(.cdtData$EnvData$DirExist)){
			stateCaclBut <- if(tclvalue(.cdtData$EnvData$DirExist) == "1") "normal" else "disabled"
		}else stateCaclBut <- "normal"

		bt.CalcOnset <- ttkbutton(frameCalc, text = 'Calculate Season Length', state = stateCaclBut)

		tkconfigure(bt.CalcOnset, command = function(){
			GeneralParameters$onset <- str_trim(tclvalue(input.Onset))
			GeneralParameters$cessation <- str_trim(tclvalue(input.Cessation))
			GeneralParameters$output <- str_trim(tclvalue(dir.save))

			# assign('GeneralParameters', GeneralParameters, envir = .GlobalEnv)

			Insert.Messages.Out("Calculate Length of the season ......")

			tkconfigure(.cdtEnv$tcl$main$win, cursor = 'watch')
			tcl('update')
			ret <- tryCatch(
				{
					compute_SeasonLength_Procs(GeneralParameters)
				},
				warning = function(w) warningFun(w),
				error = function(e) errorFun(e),
				finally = {
					tkconfigure(.cdtEnv$tcl$main$win, cursor = '')
					tcl('update')
				}
			)

			msg0 <- "Season Length calculation finished successfully"
			msg1 <- "Season Length calculation failed"

			if(!is.null(ret)){
				if(ret == 0){
					Insert.Messages.Out(msg0)

					.cdtData$EnvData$plot.maps$data.type <- .cdtData$EnvData$output$params$data.type
					.cdtData$EnvData$plot.maps[c('lon', 'lat', 'id')] <- .cdtData$EnvData$output$data[c('lon', 'lat', 'id')]
					###################
					set.Data.Dates()
					widgets.Station.Pixel()
					res <- try(read.Data.Map(), silent = TRUE)
					if(inherits(res, "try-error") | is.null(res)) return(NULL)
				}else Insert.Messages.Out(msg1, format = TRUE)
			}else Insert.Messages.Out(msg1, format = TRUE)
		})

		####################

		tkgrid(bt.CalcOnset, row = 0, column = 0, sticky = 'we', pady = 1)

		############################################

		tkgrid(frameInData, row = 0, column = 0, sticky = 'we', padx = 1, pady = 3, ipadx = 1, ipady = 1)
		tkgrid(frameDirSav, row = 1, column = 0, sticky = 'we', padx = 1, pady = 3, ipadx = 1, ipady = 1)
		tkgrid(frameCalc, row = 2, column = 0, sticky = '', padx = 1, pady = 3, ipadx = 1, ipady = 1)

	#######################################################################################################

	#Tab2
	subfr2 <- bwTabScrollableFrame(cmd.tab2)

		##############################################

		frameDataExist <- ttklabelframe(subfr2, text = "Season Length data", relief = 'groove')

		.cdtData$EnvData$DirExist <- tclVar(0)
		file.dataIndex <- tclVar()

		stateExistData <- if(tclvalue(.cdtData$EnvData$DirExist) == "1") "normal" else "disabled"

		chk.dataIdx <- tkcheckbutton(frameDataExist, variable = .cdtData$EnvData$DirExist, text = "Season length data already computed", anchor = 'w', justify = 'left')
		en.dataIdx <- tkentry(frameDataExist, textvariable = file.dataIndex, width = largeur0, state = stateExistData)
		bt.dataIdx <- tkbutton(frameDataExist, text = "...", state = stateExistData)

		tkconfigure(bt.dataIdx, command = function(){
			path.dataIdx <- tclvalue(tkgetOpenFile(initialdir = getwd(), filetypes = .cdtEnv$tcl$data$filetypes6))
			if(path.dataIdx %in% c("", "NA") | is.na(path.dataIdx)) return(NULL)
			tclvalue(file.dataIndex) <- path.dataIdx

			if(file.exists(str_trim(tclvalue(file.dataIndex)))){
				OutIndexdata <- try(readRDS(str_trim(tclvalue(file.dataIndex))), silent = TRUE)
				if(inherits(OutIndexdata, "try-error")){
					Insert.Messages.Out('Unable to load season length data', format = TRUE)
					Insert.Messages.Out(gsub('[\r\n]', '', OutIndexdata[1]), format = TRUE)
					tkconfigure(cb.data.Index, values = "")
					tclvalue(.cdtData$EnvData$donDate) <- ""
					return(NULL)
				}

				.cdtData$EnvData$output <- OutIndexdata
				.cdtData$EnvData$PathData <- dirname(str_trim(tclvalue(file.dataIndex)))
				.cdtData$EnvData$plot.maps$data.type <- .cdtData$EnvData$output$params$data.type
				.cdtData$EnvData$plot.maps[c('lon', 'lat', 'id')] <- .cdtData$EnvData$output$data[c('lon', 'lat', 'id')]
				###################
				set.Data.Dates()
				widgets.Station.Pixel()
				ret <- try(read.Data.Map(), silent = TRUE)
				if(inherits(ret, "try-error") | is.null(ret)) return(NULL)
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
			tkconfigure(bt.CalcOnset, state = stateCaclBut)
		})

		##############################################

		frameDataMap <- ttklabelframe(subfr2, text = "Season Length Map", relief = 'groove')

		.cdtData$EnvData$donDate <- tclVar()

		cb.data.Index <- ttkcombobox(frameDataMap, values = "", textvariable = .cdtData$EnvData$donDate, width = largeur2)
		bt.data.Index.prev <- ttkbutton(frameDataMap, text = "<<", width = 3)
		bt.data.Index.next <- ttkbutton(frameDataMap, text = ">>", width = 3)
		bt.data.maps <- ttkbutton(frameDataMap, text = .cdtEnv$tcl$lang$global[['button']][['3']], width = 7)
		bt.data.MapOpt <- ttkbutton(frameDataMap, text = .cdtEnv$tcl$lang$global[['button']][['4']], width = 7)

		###############

		.cdtData$EnvData$dataMapOp <- list(presetCol = list(color = 'tim.colors', reverse = FALSE),
											userCol = list(custom = FALSE, color = NULL),
											userLvl = list(custom = FALSE, levels = NULL, equidist = FALSE),
											title = list(user = FALSE, title = ''),
											colkeyLab = list(user = FALSE, label = ''),
											scalebar = list(add = FALSE, pos = 'bottomleft'))

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
			.cdtData$EnvData$dataMapOp <- MapGraph.MapOptions(.cdtEnv$tcl$main$win, .cdtData$EnvData$dataMapOp)
		})

		#########
		.cdtData$EnvData$tab$dataMap <- NULL

		tkconfigure(bt.data.maps, command = function(){
			if(str_trim(tclvalue(.cdtData$EnvData$donDate)) != "" &
				!is.null(.cdtData$EnvData$varData))
					SeasonLengthCalc.Display.Maps()
		})

		tkconfigure(bt.data.Index.prev, command = function(){
			if(str_trim(tclvalue(.cdtData$EnvData$donDate)) != ""){
				donDates <- format(.cdtData$EnvData$output$start.date, "%Y")
				idaty <- which(donDates == str_trim(tclvalue(.cdtData$EnvData$donDate)))
				idaty <- idaty - 1
				if(idaty < 1) idaty <- length(donDates)
				tclvalue(.cdtData$EnvData$donDate) <- donDates[idaty]

				ret <- try(read.Data.Map(), silent = TRUE)
				if(inherits(ret, "try-error") | is.null(ret)) return(NULL)

				SeasonLengthCalc.Display.Maps()
			}
		})

		tkconfigure(bt.data.Index.next, command = function(){
			if(str_trim(tclvalue(.cdtData$EnvData$donDate)) != ""){
				donDates <- format(.cdtData$EnvData$output$start.date, "%Y")
				idaty <- which(donDates == str_trim(tclvalue(.cdtData$EnvData$donDate)))
				idaty <- idaty + 1
				if(idaty > length(donDates)) idaty <- 1
				tclvalue(.cdtData$EnvData$donDate) <- donDates[idaty]

				ret <- try(read.Data.Map(), silent = TRUE)
				if(inherits(ret, "try-error") | is.null(ret)) return(NULL)

				SeasonLengthCalc.Display.Maps()
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
			if(!is.null(.cdtData$EnvData$varData)){
				ret <- try(read.Data.Map(), silent = TRUE)
				if(inherits(ret, "try-error") | is.null(ret)) return(NULL)
			}
		})

		##############################################

		tkgrid(frameDataExist, row = 0, column = 0, sticky = 'we', padx = 1, pady = 1, ipadx = 1, ipady = 1)
		tkgrid(frameDataMap, row = 1, column = 0, sticky = 'we', padx = 1, pady = 3, ipadx = 1, ipady = 1)

	#######################################################################################################

	#Tab3
	subfr3 <- bwTabScrollableFrame(cmd.tab3)

		##############################################

		frameDataTS <- ttklabelframe(subfr3, text = "Season Length Graph", relief = 'groove')

		typeTSPLOT <- c("Line", "Barplot")
		.cdtData$EnvData$plot.maps$typeTSp <- tclVar("Line")

		cb.typeTSp <- ttkcombobox(frameDataTS, values = typeTSPLOT, textvariable = .cdtData$EnvData$plot.maps$typeTSp, width = largeur2)
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
						legend = NULL)
					)

		tkconfigure(bt.TSGraphOpt, command = function(){
			suffix.fun <- switch(str_trim(tclvalue(.cdtData$EnvData$plot.maps$typeTSp)),
									"Barplot" = "Bar",
									"Line" = "Line")
			plot.fun <- get(paste0("MapGraph.GraphOptions.", suffix.fun), mode = "function")
			.cdtData$EnvData$TSGraphOp <- plot.fun(.cdtEnv$tcl$main$win, .cdtData$EnvData$TSGraphOp)
		})

		#########
		.cdtData$EnvData$tab$dataGraph <- NULL

		tkconfigure(bt.TsGraph.plot, command = function(){
			if(!is.null(.cdtData$EnvData$varData)){
				imgContainer <- CDT.Display.Graph(SeasonLengthCalc.plotSeasLenGraph, .cdtData$EnvData$tab$dataGraph, 'Season Length-Graph')
				.cdtData$EnvData$tab$dataGraph <- imageNotebookTab_unik(imgContainer, .cdtData$EnvData$tab$dataGraph)
			}
		})

		#################

		tkgrid(cb.typeTSp, row = 0, column = 0, sticky = 'we', pady = 1, columnspan = 1)
		tkgrid(bt.TSGraphOpt, row = 0, column = 1, sticky = 'we', padx = 4, pady = 1, columnspan = 1)
		tkgrid(bt.TsGraph.plot, row = 0, column = 2, sticky = 'we', pady = 1, columnspan = 1)

		##############################################

		frameSTNCrds <- ttklabelframe(subfr3, text = "Station/Coordinates", relief = 'groove')

		frTS2 <- tkframe(frameSTNCrds)
		.cdtData$EnvData$plot.maps$lonLOC <- tclVar()
		.cdtData$EnvData$plot.maps$latLOC <- tclVar()
		.cdtData$EnvData$plot.maps$stnIDTSp <- tclVar()

		tkgrid(frTS2, row = 0, column = 0, sticky = 'e', pady = 1)

		##############################################

		tkgrid(frameDataTS, row = 0, column = 0, sticky = 'we', pady = 1)
		tkgrid(frameSTNCrds, row = 1, column = 0, sticky = '', pady = 3)

	#######################################################################################################

	#Tab4
	subfr4 <- bwTabScrollableFrame(cmd.tab4)

		##############################################

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
				if(is.null(shpofile)) .cdtData$EnvData$shp$ocrds <- NULL
				.cdtData$EnvData$shp$ocrds <- getBoundaries(shpofile[[2]])
			}
		})

		########
		.cdtData$EnvData$SHPOp <- list(col = "black", lwd = 1.5)

		tkconfigure(bt.addshpOpt, command = function(){
			.cdtData$EnvData$SHPOp <- MapGraph.GraphOptions.LineSHP(.cdtEnv$tcl$main$win, .cdtData$EnvData$SHPOp)
		})

		########
		tkgrid(chk.addshp, row = 0, column = 0, sticky = 'we', rowspan = 1, columnspan = 6, padx = 1, pady = 1)
		tkgrid(bt.addshpOpt, row = 0, column = 6, sticky = 'we', rowspan = 1, columnspan = 2, padx = 1, pady = 1)
		tkgrid(cb.addshp, row = 1, column = 0, sticky = 'we', rowspan = 1, columnspan = 7, padx = 1, pady = 1)
		tkgrid(bt.addshp, row = 1, column = 7, sticky = 'w', rowspan = 1, columnspan = 1, padx = 0, pady = 1)

		#################
		tkbind(cb.addshp, "<<ComboboxSelected>>", function(){
			shpofile <- getShpOpenData(file.plotShp)
			if(is.null(shpofile)) .cdtData$EnvData$shp$ocrds <- NULL
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
			cb.stnID <- ttkcombobox(frTS2, values = stnIDTSPLOT, textvariable = .cdtData$EnvData$plot.maps$stnIDTSp, width = largeur2)
			tclvalue(.cdtData$EnvData$plot.maps$stnIDTSp) <- stnIDTSPLOT[1]

			tkconfigure(bt.stnID.prev, command = function(){
				if(!is.null(.cdtData$EnvData$varData)){
					istn <- which(stnIDTSPLOT == str_trim(tclvalue(.cdtData$EnvData$plot.maps$stnIDTSp)))
					istn <- istn - 1
					if(istn < 1) istn <- length(stnIDTSPLOT)
					tclvalue(.cdtData$EnvData$plot.maps$stnIDTSp) <- stnIDTSPLOT[istn]

					imgContainer <- CDT.Display.Graph(SeasonLengthCalc.plotSeasLenGraph, .cdtData$EnvData$tab$dataGraph, 'Season Length-Graph')
					.cdtData$EnvData$tab$dataGraph <- imageNotebookTab_unik(imgContainer, .cdtData$EnvData$tab$dataGraph)
				}
			})

			tkconfigure(bt.stnID.next, command = function(){
				if(!is.null(.cdtData$EnvData$varData)){
					istn <- which(stnIDTSPLOT == str_trim(tclvalue(.cdtData$EnvData$plot.maps$stnIDTSp)))
					istn <- istn + 1
					if(istn > length(stnIDTSPLOT)) istn <- 1
					tclvalue(.cdtData$EnvData$plot.maps$stnIDTSp) <- stnIDTSPLOT[istn]

					imgContainer <- CDT.Display.Graph(SeasonLengthCalc.plotSeasLenGraph, .cdtData$EnvData$tab$dataGraph, 'Season Length-Graph')
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

	set.Data.Dates <- function(){
		donDates <- format(.cdtData$EnvData$output$start.date, "%Y")
		tkconfigure(cb.data.Index, values = donDates)
		tclvalue(.cdtData$EnvData$donDate) <- donDates[length(donDates)]
		return(0)
	}

	#######################################################################################################

	read.Data.Map <- function(){
		tkconfigure(.cdtEnv$tcl$main$win, cursor = 'watch')
		tcl('update')
		on.exit({
			tkconfigure(.cdtEnv$tcl$main$win, cursor = '')
			tcl('update')
		})

		this.daty <- str_trim(tclvalue(.cdtData$EnvData$donDate))
		idt <- which(format(.cdtData$EnvData$output$start.date, "%Y") == this.daty)

		if(.cdtData$EnvData$output$params$data.type == "cdtstation"){
			filePathData <- file.path(.cdtData$EnvData$PathData, "CDTDATASET/SEASONLENGTH.rds")
			if(!file.exists(filePathData)){
				Insert.Messages.Out(paste(filePathData, 'not found'), format = TRUE)
				return(NULL)
			}

			readVarData <- TRUE
			if(!is.null(.cdtData$EnvData$varData))
				if(!is.null(.cdtData$EnvData$filePathData))
					if(.cdtData$EnvData$filePathData == filePathData) readVarData <- FALSE

			if(readVarData){
				.cdtData$EnvData$varData$data <- readRDS(filePathData)
				.cdtData$EnvData$filePathData <- filePathData
			}

			########
			rasterVarData <- TRUE
			if(!rasterVarData)
				if(!is.null(.cdtData$EnvData$varData$rasterDate))
					if(.cdtData$EnvData$filePathData == filePathData)
						if(.cdtData$EnvData$varData$rasterDate == this.daty) rasterVarData <- FALSE

			if(rasterVarData){
				nx <- nx_ny_as.image(diff(range(.cdtData$EnvData$output$data$lon)))
				ny <- nx_ny_as.image(diff(range(.cdtData$EnvData$output$data$lat)))
				tmp <- as.numeric(.cdtData$EnvData$varData$data[idt, ])

				tmp <- cdt.as.image(tmp, nx = nx, ny = ny,
								pts.xy = cbind(.cdtData$EnvData$output$data$lon, .cdtData$EnvData$output$data$lat))
				.cdtData$EnvData$varData$map$x <- tmp$x
				.cdtData$EnvData$varData$map$y <- tmp$y
				.cdtData$EnvData$varData$map$z <- tmp$z
				.cdtData$EnvData$varData$rasterDate <- this.daty
				rm(tmp)
			}
		}else{
			filePathData <- file.path(.cdtData$EnvData$PathData, "DATA_NetCDF",
							paste0("seasLen_", format(.cdtData$EnvData$output$start.date[idt], "%Y%m%d"), ".nc"))
			if(!file.exists(filePathData)){
				Insert.Messages.Out(paste(filePathData, 'not found'), format = TRUE)
				return(NULL)
			}

			readVarData <- TRUE
			if(!is.null(.cdtData$EnvData$varData))
				if(!is.null(.cdtData$EnvData$filePathData))
					if(.cdtData$EnvData$filePathData == filePathData) readVarData <- FALSE

			if(readVarData){
				nc <- nc_open(filePathData)
				.cdtData$EnvData$varData$map$x <- nc$dim[[1]]$vals
				.cdtData$EnvData$varData$map$y <- nc$dim[[2]]$vals
				.cdtData$EnvData$varData$map$z <- ncvar_get(nc, varid = nc$var[[1]]$name)
				nc_close(nc)
				.cdtData$EnvData$filePathData <- filePathData
			}

			###################

			file.CDT.Idx <- file.path(.cdtData$EnvData$PathData, "CDTDATASET/CDTDATASET.rds")

			read.cdt.dataIdx <- TRUE
			if(!is.null(.cdtData$EnvData$cdtdataset))
				if(!is.null(.cdtData$EnvData$file.CDT.Idx))
					if(.cdtData$EnvData$file.CDT.Idx == file.CDT.Idx) read.cdt.dataIdx <- FALSE
			if(read.cdt.dataIdx){
				.cdtData$EnvData$cdtdataset <- readRDS(file.CDT.Idx)
				.cdtData$EnvData$cdtdataset$fileInfo <- file.CDT.Idx
				.cdtData$EnvData$file.CDT.Idx <- file.CDT.Idx
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
