
PlotCDTStationCmd <- function(){
	listOpenFiles <- openFile_ttkcomboList()
	if(WindowsOS()){
		largeur0 <- .cdtEnv$tcl$fun$w.widgets(18)
		largeur1 <- .cdtEnv$tcl$fun$w.widgets(26)
		largeur3 <- 20
		largeur4 <- 26
	}else{
		largeur0 <- .cdtEnv$tcl$fun$w.widgets(14)
		largeur1 <- .cdtEnv$tcl$fun$w.widgets(21)
		largeur3 <- 14
		largeur4 <- 20
	}

	GeneralParameters <- list(intstep = "dekadal", cdtstation = "",
							date = list(year = 2017, mon = 1, day = 1, other = ""))

	# xml.dlg <- file.path(.cdtDir$dirLocal, "languages", "cdtPlot_StationData_leftCmd.xml")
	# lang.dlg <- cdtLanguageParse(xml.dlg, .cdtData$Config$lang.iso)
	# .cdtData$EnvData$message <- lang.dlg[['message']]

	.cdtData$EnvData$plot.maps$data.type <- "cdtstation"

	###################

	.cdtEnv$tcl$main$cmd.frame <- tkframe(.cdtEnv$tcl$main$panel.left)

	tknote.cmd <- bwNoteBook(.cdtEnv$tcl$main$cmd.frame)

	cmd.tab1 <- bwAddTab(tknote.cmd, text = "Plot CDT Station Data")
	cmd.tab2 <- bwAddTab(tknote.cmd, text = "Boundaries")

	bwRaiseTab(tknote.cmd, cmd.tab1)

	tkgrid.columnconfigure(cmd.tab1, 0, weight = 1)
	tkgrid.columnconfigure(cmd.tab2, 0, weight = 1)

	tkgrid.rowconfigure(cmd.tab1, 0, weight = 1)
	tkgrid.rowconfigure(cmd.tab2, 0, weight = 1)

	#######################################################################################################

	#Tab1
	subfr1 <- bwTabScrollableFrame(cmd.tab1)

		#######################

		frameCDTdata <- ttklabelframe(subfr1, text = "Station Data", relief = 'groove')

		timeSteps <- tclVar()
		CbperiodVAL <- .cdtEnv$tcl$lang$global[['combobox']][['1']][c(2:5, 9)]
		periodVAL <- c('daily', 'pentad', 'dekadal', 'monthly', 'others')
		tclvalue(timeSteps) <- CbperiodVAL[periodVAL %in% GeneralParameters$intstep]

		input.file <- tclVar(GeneralParameters$cdtstation)

		txt.cdtdata1 <- tklabel(frameCDTdata, text = "Time step", anchor = 'w', justify = 'left')
		cb.cdtdata1 <- ttkcombobox(frameCDTdata, values = CbperiodVAL, textvariable = timeSteps, width = largeur0)
		txt.cdtdata2 <- tklabel(frameCDTdata, text = 'File containing CDT stations data', anchor = 'w', justify = 'left')
		cb.cdtdata2 <- ttkcombobox(frameCDTdata, values = unlist(listOpenFiles), textvariable = input.file, width = largeur1)
		bt.cdtdata <- tkbutton(frameCDTdata, text = "...")

		tkconfigure(bt.cdtdata, command = function(){
			dat.opfiles <- getOpenFiles(.cdtEnv$tcl$main$win)
			if(!is.null(dat.opfiles)){
				update.OpenFiles('ascii', dat.opfiles)
				listOpenFiles[[length(listOpenFiles) + 1]] <<- dat.opfiles[[1]]
				tclvalue(input.file) <- dat.opfiles[[1]]
				tkconfigure(cb.cdtdata2, values = unlist(listOpenFiles))

				ret <- try(splitStnData(), silent = TRUE)
				if(inherits(ret, "try-error") | is.null(ret)){
					tclvalue(input.file) <- ""
					.cdtData$EnvData$don <- NULL
					return(NULL)
				}
			}else{
				tclvalue(input.file) <- ""
				.cdtData$EnvData$don <- NULL
				return(NULL)
			}
		})

		############

		tkgrid(txt.cdtdata1, row = 0, column = 0, sticky = 'we', rowspan = 1, columnspan = 2, padx = 1, pady = 1, ipadx = 1, ipady = 1)
		tkgrid(cb.cdtdata1, row = 0, column = 2, sticky = 'we', rowspan = 1, columnspan = 6, padx = 1, pady = 1, ipadx = 1, ipady = 1)
		tkgrid(txt.cdtdata2, row = 1, column = 0, sticky = 'we', rowspan = 1, columnspan = 8, padx = 1, pady = 1, ipadx = 1, ipady = 1)
		tkgrid(cb.cdtdata2, row = 2, column = 0, sticky = 'we', rowspan = 1, columnspan = 9, padx = 0, pady = 1, ipadx = 1, ipady = 1)
		tkgrid(bt.cdtdata, row = 2, column = 9, sticky = 'w', rowspan = 1, columnspan = 1, padx = 0, pady = 1, ipadx = 1, ipady = 1)

		############

		tkbind(cb.cdtdata1, "<<ComboboxSelected>>", function(){
			tkdestroy(frTS1)
			frTS1 <<- tkframe(frTS0)

			if(str_trim(tclvalue(timeSteps)) == CbperiodVAL[5]){
				txt.other <- tklabel(frTS1, text = 'Dates or Index')
				cb.other <<- ttkcombobox(frTS1, values = "", textvariable = date.other, width = 16)

				tkgrid(txt.other, row = 0, column = 0, sticky = 'we', pady = 1, padx = 1)
				tkgrid(cb.other, row = 1, column = 0, sticky = 'we', pady = 1, padx = 1)
			}else{
				stateday <- if(str_trim(tclvalue(timeSteps)) == CbperiodVAL[4]) 'disabled' else 'normal'

				txtdek <- c('Day', 'Pen', 'Dek', 'Day', 'Day')[CbperiodVAL %in% str_trim(tclvalue(timeSteps))]
				day.txtVar <- tclVar(txtdek)

				txt.yrs <- tklabel(frTS1, text = 'Year')
				txt.mon <- tklabel(frTS1, text = 'Month')
				txt.day <- tklabel(frTS1, text = tclvalue(day.txtVar), textvariable = day.txtVar)
				en.yrs <- tkentry(frTS1, width = 5, textvariable = date.year, justify = "center")
				en.mon <- tkentry(frTS1, width = 5, textvariable = date.mon, justify = "center")
				en.day <- tkentry(frTS1, width = 5, textvariable = date.day, justify = "center", state = stateday)

				##############
				tkgrid(txt.yrs, row = 0, column = 0, sticky = 'we', pady = 1, padx = 1)
				tkgrid(txt.mon, row = 0, column = 1, sticky = 'we', pady = 1, padx = 1)
				tkgrid(txt.day, row = 0, column = 2, sticky = 'we', pady = 1, padx = 1)

				tkgrid(en.yrs, row = 1, column = 0, sticky = 'we', pady = 1, padx = 1)
				tkgrid(en.mon, row = 1, column = 1, sticky = 'we', pady = 1, padx = 1)
				tkgrid(en.day, row = 1, column = 2, sticky = 'we', pady = 1, padx = 1)
			}

			##############
			tkgrid(frTS1, row = 0, column = 1, sticky = 'we', pady = 1, rowspan = 2, columnspan = 1)
		})

		tkbind(cb.cdtdata2, "<<ComboboxSelected>>", function(){
			ret <- try(splitStnData(), silent = TRUE)
			if(inherits(ret, "try-error") | is.null(ret)){
				tclvalue(input.file) <- ""
				.cdtData$EnvData$don <- NULL
				return(NULL)
			}
		})

		##############################################

		frameMap <- ttklabelframe(subfr1, text = "Map", relief = 'groove')

		typeMapPLOT <- c("Points", "Pixels")
		.cdtData$EnvData$map$typeMap <- tclVar("Points")
		pointSizeI <- 0.7

		cb.Map.type <- ttkcombobox(frameMap, values = typeMapPLOT, textvariable = .cdtData$EnvData$map$typeMap, width = largeur3)
		bt.Map.plot <- ttkbutton(frameMap, text = .cdtEnv$tcl$lang$global[['button']][['3']], width = 7)
		bt.Map.Opt <- ttkbutton(frameMap, text = .cdtEnv$tcl$lang$global[['button']][['4']], width = 8)
		frTS0 <- tkframe(frameMap)

		##############
		.cdtData$EnvData$dataMapOp <- list(presetCol = list(color = 'tim.colors', reverse = FALSE),
												userCol = list(custom = FALSE, color = NULL),
												userLvl = list(custom = FALSE, levels = NULL, equidist = FALSE),
												title = list(user = FALSE, title = ''),
												colkeyLab = list(user = FALSE, label = ''),
												scalebar = list(add = FALSE, pos = 'bottomleft'),
												pointSize = pointSizeI)

		tkconfigure(bt.Map.Opt, command = function(){
			if(!is.null(.cdtData$EnvData$stndata$map)){
				atlevel <- pretty(.cdtData$EnvData$stndata$map$z, n = 10, min.n = 7)
				if(is.null(.cdtData$EnvData$dataMapOp$userLvl$levels)){
					.cdtData$EnvData$dataMapOp$userLvl$levels <- atlevel
				}else{
					if(!.cdtData$EnvData$dataMapOp$userLvl$custom)
						.cdtData$EnvData$dataMapOp$userLvl$levels <- atlevel
				}
			}
			.cdtData$EnvData$dataMapOp <- MapGraph.MapOptions(.cdtData$EnvData$dataMapOp)

			if(str_trim(tclvalue(.cdtData$EnvData$map$typeMap)) == "Points")
				pointSizeI <<- .cdtData$EnvData$dataMapOp$pointSize
		})

		.cdtData$EnvData$tab$dataMap <- NULL

		tkconfigure(bt.Map.plot, command = function(){
			if(is.null(.cdtData$EnvData$don)) return(NULL)
			ret <- try(getStnMap(), silent = TRUE)
			if(inherits(ret, "try-error") | is.null(ret)) return(NULL)

			####
			CDTdataStation.Display.Maps()
		})

		##############
		tkbind(cb.Map.type, "<<ComboboxSelected>>", function(){
			if(str_trim(tclvalue(.cdtData$EnvData$map$typeMap)) == "Points"){
				.cdtData$EnvData$dataMapOp$pointSize <- pointSizeI
			}else .cdtData$EnvData$dataMapOp$pointSize <- NULL

			getStnMap()
		})

		##############

		bt.date.prev <- ttkbutton(frTS0, text = "<<", width = 6)
		bt.date.next <- ttkbutton(frTS0, text = ">>", width = 6)
		frTS1 <- tkframe(frTS0)

		##############
		date.year <- tclVar(GeneralParameters$date$year)
		date.mon <- tclVar(GeneralParameters$date$mon)
		date.day <- tclVar(GeneralParameters$date$day)
		date.other <- tclVar(GeneralParameters$date$other)

		if(GeneralParameters$intstep == 'others'){
			txt.other <- tklabel(frTS1, text = 'Dates or Index')
			cb.other <- ttkcombobox(frTS1, values = "", textvariable = date.other, width = 16)

			tkgrid(txt.other, row = 0, column = 0, sticky = 'we', pady = 1, padx = 1)
			tkgrid(cb.other, row = 1, column = 0, sticky = 'we', pady = 1, padx = 1)
		}else{
			txtdek <- switch(GeneralParameters$intstep, 'dekadal' = 'Dek', 'pentad' = 'Pen', 'Day')
			day.txtVar <- tclVar(txtdek)
			stateday <- if(GeneralParameters$intstep == 'monthly') 'disabled' else 'normal'

			txt.yrs <- tklabel(frTS1, text = 'Year')
			txt.mon <- tklabel(frTS1, text = 'Month')
			txt.day <- tklabel(frTS1, text = tclvalue(day.txtVar), textvariable = day.txtVar)
			en.yrs <- tkentry(frTS1, width = 5, textvariable = date.year, justify = "center")
			en.mon <- tkentry(frTS1, width = 5, textvariable = date.mon, justify = "center")
			en.day <- tkentry(frTS1, width = 5, textvariable = date.day, justify = "center", state = stateday)

			##############
			tkgrid(txt.yrs, row = 0, column = 0, sticky = 'we', pady = 1, padx = 1)
			tkgrid(txt.mon, row = 0, column = 1, sticky = 'we', pady = 1, padx = 1)
			tkgrid(txt.day, row = 0, column = 2, sticky = 'we', pady = 1, padx = 1)

			tkgrid(en.yrs, row = 1, column = 0, sticky = 'we', pady = 1, padx = 1)
			tkgrid(en.mon, row = 1, column = 1, sticky = 'we', pady = 1, padx = 1)
			tkgrid(en.day, row = 1, column = 2, sticky = 'we', pady = 1, padx = 1)
		}

		##############
		tkconfigure(bt.date.prev, command = function(){
			if(is.null(.cdtData$EnvData$don)) return(NULL) 
			temps <- str_trim(tclvalue(timeSteps))

			if(temps == CbperiodVAL[5]){
				idaty <- which(.cdtData$EnvData$don$dates == str_trim(tclvalue(date.other)))
				idaty <- idaty - 1
				if(idaty < 1) idaty <- length(.cdtData$EnvData$don$dates)
				tclvalue(date.other) <- .cdtData$EnvData$don$dates[idaty]
			}else{
				yrs <- as.numeric(str_trim(tclvalue(date.year)))
				mon <- as.numeric(str_trim(tclvalue(date.mon)))
				dpk <- as.numeric(str_trim(tclvalue(date.day)))

				if(temps == CbperiodVAL[1]) todaty <- paste(yrs, mon, dpk, sep = '-')
				if(temps == CbperiodVAL[2]){
					if(is.na(dpk) | dpk < 1 | dpk > 6){
						Insert.Messages.Out("Pentad must be between 1 and 6", format = TRUE)
						return(NULL)
					}
					todaty <- paste(yrs, mon, dpk, sep = '-')
				}
				if(temps == CbperiodVAL[3]){
					if(is.na(dpk) | dpk < 1 | dpk > 3){
						Insert.Messages.Out("Dekad must be 1, 2 or 3", format = TRUE)
						return(NULL)
					}
					todaty <- paste(yrs, mon, dpk, sep = '-')
				}
				if(temps == CbperiodVAL[4]) todaty <- paste(yrs, mon, 1, sep = '-')

				daty <- try(as.Date(todaty), silent = TRUE)
				if(inherits(daty, "try-error") | is.na(daty)){
					Insert.Messages.Out(paste("Date invalid", todaty), format = TRUE)
					return(NULL)
				}
				if(temps == CbperiodVAL[1]) daty <- daty - 1
				if(temps == CbperiodVAL[2]) daty <- addPentads(daty, -1)
				if(temps == CbperiodVAL[3]) daty <- addDekads(daty, -1)
				if(temps == CbperiodVAL[4]) daty <- addMonths(daty, -1)

				if(daty < .cdtData$EnvData$first.date) daty <- .cdtData$EnvData$last.date
				daty <- format(daty, '%Y%m%d')
				tclvalue(date.year) <- as.numeric(substr(daty, 1, 4))
				tclvalue(date.mon) <- as.numeric(substr(daty, 5, 6))
				tclvalue(date.day) <- as.numeric(substr(daty, 7, 8))
			}

			######
			ret <- try(getStnMap(), silent = TRUE)
			if(inherits(ret, "try-error") | is.null(ret)) return(NULL)

			####
			CDTdataStation.Display.Maps()
		})

		tkconfigure(bt.date.next, command = function(){
			if(is.null(.cdtData$EnvData$don)) return(NULL) 
			temps <- str_trim(tclvalue(timeSteps))

			if(temps == CbperiodVAL[5]){
				idaty <- which(.cdtData$EnvData$don$dates == str_trim(tclvalue(date.other)))
				idaty <- idaty + 1
				if(idaty > length(.cdtData$EnvData$don$dates)) idaty <- 1
				tclvalue(date.other) <- .cdtData$EnvData$don$dates[idaty]
			}else{
				yrs <- as.numeric(str_trim(tclvalue(date.year)))
				mon <- as.numeric(str_trim(tclvalue(date.mon)))
				dpk <- as.numeric(str_trim(tclvalue(date.day)))

				if(temps == CbperiodVAL[2]){
					if(is.na(dpk) | dpk < 1 | dpk > 6){
						Insert.Messages.Out("Pentad must be between 1 and 6", format = TRUE)
						return(NULL)
					}
				}
				if(temps == CbperiodVAL[3]){
					if(is.na(dpk) | dpk < 1 | dpk > 3){
						Insert.Messages.Out("Dekad must be 1, 2 or 3", format = TRUE)
						return(NULL)
					}
				}
				if(temps == CbperiodVAL[4]) dpk <- 1

				todaty <- paste(yrs, mon, dpk, sep = '-')
				daty <- try(as.Date(todaty), silent = TRUE)
				if(inherits(daty, "try-error") | is.na(daty)){
					Insert.Messages.Out(paste("Invalid date", todaty), format = TRUE)
					return(NULL)
				}
				if(temps == CbperiodVAL[1]) daty <- daty + 1
				if(temps == CbperiodVAL[2]) daty <- addPentads(daty, 1)
				if(temps == CbperiodVAL[3]) daty <- addDekads(daty, 1)
				if(temps == CbperiodVAL[4]) daty <- addMonths(daty, 1)

				if(daty > .cdtData$EnvData$last.date) daty <- .cdtData$EnvData$first.date
				daty <- format(daty, '%Y%m%d')
				tclvalue(date.year) <- as.numeric(substr(daty, 1, 4))
				tclvalue(date.mon) <- as.numeric(substr(daty, 5, 6))
				tclvalue(date.day) <- as.numeric(substr(daty, 7, 8))
			}

			######
			ret <- try(getStnMap(), silent = TRUE)
			if(inherits(ret, "try-error") | is.null(ret)) return(NULL)

			####
			CDTdataStation.Display.Maps()
		})

		##############
		tkgrid(frTS1, row = 0, column = 1, sticky = 'we', pady = 1, rowspan = 2, columnspan = 1)
		tkgrid(bt.date.prev, row = 1, column = 0, sticky = 'we', padx = 4, pady = 1, columnspan = 1)
		tkgrid(bt.date.next, row = 1, column = 2, sticky = 'we', padx = 4, pady = 1, columnspan = 1)

		##############
		tkgrid(cb.Map.type, row = 0, column = 0, sticky = 'we', pady = 1, columnspan = 1)
		tkgrid(bt.Map.Opt, row = 0, column = 1, sticky = 'we', padx = 4, pady = 1, columnspan = 1)
		tkgrid(bt.Map.plot, row = 0, column = 2, sticky = 'we', pady = 1, columnspan = 1)
		tkgrid(frTS0, row = 1, column = 0, sticky = '', pady = 1, columnspan = 3)

		##############################################

		frameGraph <- ttklabelframe(subfr1, text = "Graph", relief = 'groove')

		typeTSPLOT <- c("Line", "Barplot")
		.cdtData$EnvData$plot.maps$typeTSp <- tclVar("Line")

		cb.typeTSp <- ttkcombobox(frameGraph, values = typeTSPLOT, textvariable = .cdtData$EnvData$plot.maps$typeTSp, width = largeur3)
		bt.TsGraph.plot <- ttkbutton(frameGraph, text = .cdtEnv$tcl$lang$global[['button']][['3']], width = 7)
		bt.TSGraphOpt <- ttkbutton(frameGraph, text = .cdtEnv$tcl$lang$global[['button']][['4']], width = 8)

		.cdtData$EnvData$TSGraphOp <- list(
					bar = list(
							xlim = list(is.min = FALSE, min = "1981-1-1", is.max = FALSE, max = "2017-12-3"),
							ylim = list(is.min = FALSE, min = 0, is.max = FALSE, max = 200),
							axislabs = list(is.xlab = FALSE, xlab = '', is.ylab = FALSE, ylab = ''),
							title = list(is.title = FALSE, title = '', position = 'top'),
							colors = list(col = "darkblue")
						),
					line = list(
						xlim = list(is.min = FALSE, min = "1981-1-1", is.max = FALSE, max = "2017-12-3"),
						ylim = list(is.min = FALSE, min = 0, is.max = FALSE, max = 200),
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
			.cdtData$EnvData$TSGraphOp <- plot.fun(.cdtData$EnvData$TSGraphOp)
		})

		.cdtData$EnvData$tab$dataGraph <- NULL

		tkconfigure(bt.TsGraph.plot, command = function(){
			if(is.null(.cdtData$EnvData$don)) return(NULL)
			getStnTS()

			####
			titre <- paste('Station -', .cdtData$EnvData$stndata$series$id)
			imgContainer <- CDT.Display.Graph(plotCDTStation.Graph, .cdtData$EnvData$tab$dataGraph, titre)
			.cdtData$EnvData$tab$dataGraph <- imageNotebookTab_unik(imgContainer, .cdtData$EnvData$tab$dataGraph)
		})

		##############

		frTS2 <- tkframe(frameGraph)

		.cdtData$EnvData$plot.maps$stnIDTSp <- tclVar()

		bt.stnID.prev <- ttkbutton(frTS2, text = "<<", width = 5)
		cb.stnID <- ttkcombobox(frTS2, values = "", textvariable = .cdtData$EnvData$plot.maps$stnIDTSp, width = largeur4)
		bt.stnID.next <- ttkbutton(frTS2, text = ">>", width = 5)
		tkgrid(bt.stnID.prev, cb.stnID, bt.stnID.next)

		##############
		tkconfigure(bt.stnID.prev, command = function(){
			if(is.null(.cdtData$EnvData$don)) return(NULL)
			istn <- which(.cdtData$EnvData$don$id == str_trim(tclvalue(.cdtData$EnvData$plot.maps$stnIDTSp)))
			istn <- istn - 1
			if(istn < 1) istn <- length(.cdtData$EnvData$don$id)
			tclvalue(.cdtData$EnvData$plot.maps$stnIDTSp) <- .cdtData$EnvData$don$id[istn]

			getStnTS()

			####
			titre <- paste('Station -', .cdtData$EnvData$stndata$series$id)
			imgContainer <- CDT.Display.Graph(plotCDTStation.Graph, .cdtData$EnvData$tab$dataGraph, titre)
			.cdtData$EnvData$tab$dataGraph <- imageNotebookTab_unik(imgContainer, .cdtData$EnvData$tab$dataGraph)
		})

		tkconfigure(bt.stnID.next, command = function(){
			if(is.null(.cdtData$EnvData$don)) return(NULL)
			istn <- which(.cdtData$EnvData$don$id == str_trim(tclvalue(.cdtData$EnvData$plot.maps$stnIDTSp)))
			istn <- istn + 1
			if(istn > length(.cdtData$EnvData$don$id)) istn <- 1
			tclvalue(.cdtData$EnvData$plot.maps$stnIDTSp) <- .cdtData$EnvData$don$id[istn]

			getStnTS()

			####
			titre <- paste('Station -', .cdtData$EnvData$stndata$series$id)
			imgContainer <- CDT.Display.Graph(plotCDTStation.Graph, .cdtData$EnvData$tab$dataGraph, titre)
			.cdtData$EnvData$tab$dataGraph <- imageNotebookTab_unik(imgContainer, .cdtData$EnvData$tab$dataGraph)
		})

		##############
		tkgrid(cb.typeTSp, row = 0, column = 0, sticky = 'we', pady = 1, columnspan = 1)
		tkgrid(bt.TSGraphOpt, row = 0, column = 1, sticky = 'we', padx = 4, pady = 1, columnspan = 1)
		tkgrid(bt.TsGraph.plot, row = 0, column = 2, sticky = 'we', pady = 1, columnspan = 1)
		tkgrid(frTS2, row = 1, column = 0, sticky = '', pady = 1, columnspan = 3)

		############################################

		tkgrid(frameCDTdata, row = 0, column = 0, sticky = 'we', padx = 1, pady = 1, ipadx = 1, ipady = 1)
		tkgrid(frameMap, row = 1, column = 0, sticky = 'we', padx = 1, pady = 1, ipadx = 1, ipady = 1)
		tkgrid(frameGraph, row = 2, column = 0, sticky = 'we', padx = 1, pady = 1, ipadx = 1, ipady = 1)

	#######################################################################################################

	#Tab2
	subfr2 <- bwTabScrollableFrame(cmd.tab2)

		##############################################

		frameSHP <- ttklabelframe(subfr2, text = "Boundaries", relief = 'groove')

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

	splitStnData <- function(){
		.cdtData$EnvData$stndata <- NULL
		intstep <- periodVAL[CbperiodVAL %in% str_trim(tclvalue(timeSteps))]

		stn.file <- str_trim(tclvalue(input.file))
		don <- getStnOpenData(stn.file)
		if(is.null(don)) return(NULL)

		if(intstep == "others"){
			don <- splitCDTData1(don)
			.cdtData$EnvData$tsdates <- seq_along(don$dates)

			##########
			tkconfigure(cb.other, values = don$dates)
			tclvalue(date.other) <- don$dates[1]
		}else{
			don <- getCDTdataAndDisplayMsg(don, intstep, stn.file)
			if(is.null(don)) return(NULL)

			##########
			en.daty <- don$dates[length(don$dates)]

			if(intstep == "daily"){
				.cdtData$EnvData$tsdates <- as.Date(don$dates, "%Y%m%d")
				dpk <- as.numeric(substr(en.daty, 7, 8))
			}
			if(intstep == "pentad"){
				pen <- c(1, 6, 11, 16, 21, 26)[as.numeric(substr(don$dates, 7, 7))]
				.cdtData$EnvData$tsdates <- as.Date(paste0(substr(don$dates, 1, 6), pen), "%Y%m%d")
				dpk <- as.numeric(substr(en.daty, 7, 7))
			}
			if(intstep == "dekadal"){
				dek <- c(1, 11, 21)[as.numeric(substr(don$dates, 7, 7))]
				.cdtData$EnvData$tsdates <- as.Date(paste0(substr(don$dates, 1, 6), dek), "%Y%m%d")
				dpk <- as.numeric(substr(en.daty, 7, 7))
			}
			if(intstep == "monthly"){
				.cdtData$EnvData$tsdates <- as.Date(paste0(don$dates, 1), "%Y%m%d")
				dpk <- 1
			}

			first.date <- if(intstep == "monthly") paste0(don$dates[1], 1) else don$dates[1]
			last.date <- if(intstep == "monthly") paste0(don$dates[length(don$dates)], 1) else don$dates[length(don$dates)]
			.cdtData$EnvData$first.date <- as.Date(first.date, "%Y%m%d")
			.cdtData$EnvData$last.date <- as.Date(last.date, "%Y%m%d")

			##########
			tclvalue(date.year) <- as.numeric(substr(en.daty, 1, 4))
			tclvalue(date.mon) <- as.numeric(substr(en.daty, 5, 6))
			tclvalue(date.day) <- dpk
		}

		##########
		tkconfigure(cb.stnID, values = don$id)
		tclvalue(.cdtData$EnvData$plot.maps$stnIDTSp) <- don$id[1]

		.cdtData$EnvData$tstep <- intstep
		.cdtData$EnvData$don <- don
		.cdtData$EnvData$plot.maps[c('lon', 'lat', 'id')] <- don[c('lon', 'lat', 'id')]

		##########
		getStnTS()
		getStnMap()
		return(0)
	}

	getStnTS <- function(){
		istn <- which(.cdtData$EnvData$don$id == str_trim(tclvalue(.cdtData$EnvData$plot.maps$stnIDTSp)))
		if(length(istn) == 0){
			.cdtData$EnvData$stndata$series <- NULL
			Insert.Messages.Out(paste(str_trim(tclvalue(.cdtData$EnvData$plot.maps$stnIDTSp)), "doesn't exist"), format = TRUE)
		}else{
			.cdtData$EnvData$stndata$series$ts <- .cdtData$EnvData$don$data[, istn]
			.cdtData$EnvData$stndata$series$id <- str_trim(tclvalue(.cdtData$EnvData$plot.maps$stnIDTSp))
		}
	}

	getStnMap <- function(){
		tkconfigure(.cdtEnv$tcl$main$win, cursor = 'watch')
		tcl('update')
		on.exit({
			tkconfigure(.cdtEnv$tcl$main$win, cursor = '')
			tcl('update')
		})

		typemap <- str_trim(tclvalue(.cdtData$EnvData$map$typeMap))

		if(.cdtData$EnvData$tstep != "others"){
			yrs <- as.numeric(str_trim(tclvalue(date.year)))
			mon <- as.numeric(str_trim(tclvalue(date.mon)))
			dpk <- as.numeric(str_trim(tclvalue(date.day)))
			getSpat <- list(yrs, mon, dpk, typemap)
		}else getSpat <- list(str_trim(tclvalue(date.other)), typemap)

		if(!is.null(.cdtData$EnvData$stndata$spatial)){
			formatSpData <- if(!isTRUE(all.equal(.cdtData$EnvData$stndata$spatial, getSpat))) TRUE else FALSE
		}else formatSpData <- TRUE

		if(formatSpData){
			if(.cdtData$EnvData$tstep != "others"){
				if(.cdtData$EnvData$tstep == "daily")
					daty <- format(as.Date(paste(yrs, mon, dpk, sep = "-")), "%Y%m%d")
				if(.cdtData$EnvData$tstep == "pentad"){
					pen <- as.Date(paste(yrs, mon, dpk, sep = "-"))
					daty <- paste0(format(pen, "%Y%m"), dpk)
				}
				if(.cdtData$EnvData$tstep == "dekadal"){
					dek <- as.Date(paste(yrs, mon, dpk, sep = "-"))
					daty <- paste0(format(dek, "%Y%m"), dpk)
				}
				if(.cdtData$EnvData$tstep == "monthly")
					daty <- format(as.Date(paste(yrs, mon, dpk, sep = "-")), "%Y%m")
			}else daty <- str_trim(tclvalue(date.other))

			idaty <- which(.cdtData$EnvData$don$dates == daty)

			if(length(idaty) == 0){
				.cdtData$EnvData$stndata$map <- NULL
				Insert.Messages.Out("Invalid date or index", format = TRUE)
				return(NULL)
			}else{
				if(typemap == "Points"){
					.cdtData$EnvData$stndata$map$x <- .cdtData$EnvData$don$lon
					.cdtData$EnvData$stndata$map$y <- .cdtData$EnvData$don$lat
					.cdtData$EnvData$stndata$map$z <- as.numeric(.cdtData$EnvData$don$data[idaty, ])
				}

				if(typemap == "Pixels"){
					nx <- nx_ny_as.image(diff(range(.cdtData$EnvData$don$lon)))
					ny <- nx_ny_as.image(diff(range(.cdtData$EnvData$don$lat)))
					tmp <- cdt.as.image(as.numeric(.cdtData$EnvData$don$data[idaty, ]), nx = nx, ny = ny,
										pts.xy = cbind(.cdtData$EnvData$don$lon, .cdtData$EnvData$don$lat))
					.cdtData$EnvData$stndata$map$x <- tmp$x
					.cdtData$EnvData$stndata$map$y <- tmp$y
					.cdtData$EnvData$stndata$map$z <- tmp$z
				}

				.cdtData$EnvData$stndata$map$t <- daty
				.cdtData$EnvData$stndata$map$p <- typemap
			}

			.cdtData$EnvData$stndata$spatial <- getSpat
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
