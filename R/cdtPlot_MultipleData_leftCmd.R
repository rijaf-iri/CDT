
PlotMulitpleDataCmd <- function(){
	listOpenFiles <- openFile_ttkcomboList()
	if(WindowsOS()){
		largeur0 <- .cdtEnv$tcl$fun$w.widgets(22)
		largeur1 <- .cdtEnv$tcl$fun$w.widgets(28)
		largeur2 <- .cdtEnv$tcl$fun$w.widgets(30)
		largeur3 <- 15
		largeur4 <- 12
		data.w <- .cdtEnv$tcl$fun$w.scale(25)
		data.h <- .cdtEnv$tcl$fun$h.scale(40)
	}else{
		largeur0 <- .cdtEnv$tcl$fun$w.widgets(15)
		largeur1 <- .cdtEnv$tcl$fun$w.widgets(21)
		largeur2 <- .cdtEnv$tcl$fun$w.widgets(22)
		largeur3 <- 8
		largeur4 <- 8
		data.w <- .cdtEnv$tcl$fun$w.scale(25)
		data.h <- .cdtEnv$tcl$fun$h.scale(40)
	}

	###################

	GeneralParameters <- list(tstep = 'dekadal', date = list(year = 2017, mon = 1, day = 1, hour = 0))

	# xml.dlg <- file.path(.cdtDir$dirLocal, "languages", "cdtPlot_MultipleData_leftCmd.xml")
	# lang.dlg <- cdtLanguageParse(xml.dlg, .cdtData$Config$lang.iso)

	###################

	.cdtEnv$tcl$main$cmd.frame <- tkframe(.cdtEnv$tcl$main$panel.left)

	tknote.cmd <- bwNoteBook(.cdtEnv$tcl$main$cmd.frame)
	cmd.tab1 <- bwAddTab(tknote.cmd, text = "Input Data")
	cmd.tab2 <- bwAddTab(tknote.cmd, text = "Maps")

	bwRaiseTab(tknote.cmd, cmd.tab1)

	tkgrid.columnconfigure(cmd.tab1, 0, weight = 1)
	tkgrid.columnconfigure(cmd.tab2, 0, weight = 1)

	tkgrid.rowconfigure(cmd.tab1, 0, weight = 1)
	tkgrid.rowconfigure(cmd.tab2, 0, weight = 1)

	#######################################################################################################

	#Tab1
	frameTS <- tkframe(cmd.tab1)
	tkgrid(frameTS)
	subfr1 <- bwTabScrollableFrame(cmd.tab1, hscrlwin = data.h, wscrlwin = data.w)
	frameADD <- tkframe(cmd.tab1)
	tkgrid(frameADD)

		##############################################

		timeSteps <- tclVar()
		CbperiodVAL <- .cdtEnv$tcl$lang$global[['combobox']][['1']][1:5]
		periodVAL <- c('hourly', 'daily', 'pentad', 'dekadal', 'monthly')
		tclvalue(timeSteps) <- CbperiodVAL[periodVAL %in% GeneralParameters$tstep]

		txt.tstep <- tklabel(frameTS, text = 'Time step', anchor = 'e', justify = 'right')
		cb.tstep <- ttkcombobox(frameTS, values = CbperiodVAL, textvariable = timeSteps)

		########
		tkgrid(txt.tstep, row = 0, column = 0, sticky = 'we', rowspan = 1, columnspan = 2, padx = 1, pady = 2, ipadx = 1, ipady = 1)
		tkgrid(cb.tstep, row = 0, column = 2, sticky = 'we', rowspan = 1, columnspan = 3, padx = 1, pady = 2, ipadx = 1, ipady = 1)

		########
		tkbind(cb.tstep, "<<ComboboxSelected>>", function(){
			stateday <- if(str_trim(tclvalue(timeSteps)) == CbperiodVAL[5]) 'disabled' else 'normal'
			statehour <- if(str_trim(tclvalue(timeSteps)) == CbperiodVAL[1]) 'normal' else 'disabled'
			txtdek <- c('Day', 'Day', 'Pen', 'Dek', 'Day')[CbperiodVAL %in% str_trim(tclvalue(timeSteps))]

			tclvalue(day.txtVar) <- txtdek
			tkconfigure(en.day, state = stateday)
			tkconfigure(en.hrs, state = statehour)
		})

		#######################

		CbdatatypeVAL <- .cdtEnv$tcl$lang$global[['combobox']][['2']][c(1, 3)]
		datatypeVAL <- c('cdtstation', 'cdtnetcdf')

		#######################

		bt.AddData <- tkbutton(frameADD, text = "Add Data Sets", bg = 'lightgreen')

		tkconfigure(bt.AddData, command = function(){
			jj <- length(.cdtData$GalParams$DATASETs) + 1

			ids <- isaDataSet()
			.cdtData$GalParams$DATASETs[[jj]] <- list()
			.cdtData$GalParams$DATASETs[[jj]]$pars$data.type <- "cdtstation"
			.cdtData$GalParams$DATASETs[[jj]]$pars$map.type <- "Points"
			.cdtData$GalParams$DATASETs[[jj]]$pars$plot.type <- "Points"
			.cdtData$GalParams$DATASETs[[jj]]$pars$point.size <- 0.8
			.cdtData$GalParams$DATASETs[[jj]]$pars$title <- paste("Data set", ids)
			.cdtData$GalParams$DATASETs[[jj]]$pars$input$dir <- ""
			.cdtData$GalParams$DATASETs[[jj]]$pars$input$sample <- ""
			.cdtData$GalParams$DATASETs[[jj]]$pars$input$format <- "rfe_%s%s%s.nc"

			add.new.datasets(jj, ids)
		})

		tkgrid(bt.AddData, row = 1, column = 0, sticky = 'we', padx = 1, pady = 1, ipadx = 1, ipady = 1)

		#######################

		isaDataSet <- local({
			k <- 0
			function() {
				k <<- k + 1
				return(k)
			}
		})

		add.new.datasets <- function(jj, ids){
			.cdtData$GalParams$DATASETs[[jj]]$tcl$frame <- ttklabelframe(subfr1, text = paste0("Data set#", ids), relief = 'groove')

			.cdtData$GalParams$DATASETs[[jj]]$tcl$data.type <- tclVar()
			tclvalue(.cdtData$GalParams$DATASETs[[jj]]$tcl$data.type) <- CbdatatypeVAL[datatypeVAL %in% .cdtData$GalParams$DATASETs[[jj]]$pars$data.type]

			.cdtData$GalParams$DATASETs[[jj]]$tcl$input.file <- tclVar(.cdtData$GalParams$DATASETs[[jj]]$pars$input$dir)

			stateSetNC <- if(str_trim(tclvalue(.cdtData$GalParams$DATASETs[[jj]]$tcl$data.type)) == CbdatatypeVAL[1]) "disabled" else "normal"

			cb.datatype <- ttkcombobox(.cdtData$GalParams$DATASETs[[jj]]$tcl$frame, values = CbdatatypeVAL, textvariable = .cdtData$GalParams$DATASETs[[jj]]$tcl$data.type, width = largeur0)
			bt.datatype <- ttkbutton(.cdtData$GalParams$DATASETs[[jj]]$tcl$frame, text = .cdtEnv$tcl$lang$global[['button']][['5']], state = stateSetNC)

			if(str_trim(tclvalue(.cdtData$GalParams$DATASETs[[jj]]$tcl$data.type)) == CbdatatypeVAL[1]){
				cb.en.datafile <- ttkcombobox(.cdtData$GalParams$DATASETs[[jj]]$tcl$frame, values = unlist(listOpenFiles), textvariable = .cdtData$GalParams$DATASETs[[jj]]$tcl$input.file, width = largeur1)
			}else{
				cb.en.datafile <- tkentry(.cdtData$GalParams$DATASETs[[jj]]$tcl$frame, textvariable = .cdtData$GalParams$DATASETs[[jj]]$tcl$input.file, width = largeur2)
			}
			bt.datafile <- tkbutton(.cdtData$GalParams$DATASETs[[jj]]$tcl$frame, text = "...")

			bt.Options <- ttkbutton(.cdtData$GalParams$DATASETs[[jj]]$tcl$frame, text = .cdtEnv$tcl$lang$global[['button']][['4']])
			bt.Remove <- ttkbutton(.cdtData$GalParams$DATASETs[[jj]]$tcl$frame, text = "Remove")

			####

			tkconfigure(bt.datatype, command = function(){
				.cdtData$GalParams$DATASETs[[jj]]$pars[["input"]] <- getInfoNetcdfData(.cdtEnv$tcl$main$win, .cdtData$GalParams$DATASETs[[jj]]$pars[["input"]],
																	str_trim(tclvalue(.cdtData$GalParams$DATASETs[[jj]]$tcl$input.file)),
																	str_trim(tclvalue(timeSteps)))
			})

			####

			tkconfigure(bt.datafile, command = function(){
				if(str_trim(tclvalue(.cdtData$GalParams$DATASETs[[jj]]$tcl$data.type)) == CbdatatypeVAL[1]){
					dat.opfiles <- getOpenFiles(.cdtEnv$tcl$main$win)
					if(!is.null(dat.opfiles)){
						update.OpenFiles('ascii', dat.opfiles)
						listOpenFiles[[length(listOpenFiles) + 1]] <<- dat.opfiles[[1]]
						tclvalue(.cdtData$GalParams$DATASETs[[jj]]$tcl$input.file) <- dat.opfiles[[1]]
						tkconfigure(cb.en.datafile, values = unlist(openFile_ttkcomboList()))
						.cdtData$GalParams$DATASETs[[jj]]$pars$input$dir <- str_trim(tclvalue(.cdtData$GalParams$DATASETs[[jj]]$tcl$input.file))
					}
				}else{
					dirnc <- tk_choose.dir(getwd(), "")
					tclvalue(.cdtData$GalParams$DATASETs[[jj]]$tcl$input.file) <- if(dirnc %in% c("", "NA") | is.na(dirnc)) "" else dirnc
					.cdtData$GalParams$DATASETs[[jj]]$pars$input$dir <- str_trim(tclvalue(.cdtData$GalParams$DATASETs[[jj]]$tcl$input.file))
				}

			})

			####

			tkconfigure(bt.Options, command = function(){
				.cdtData$GalParams$DATASETs[[jj]]$pars <- MapGraph.MultiDatasets(.cdtData$GalParams$DATASETs[[jj]]$pars)
			})

			####

			id.fr <- NULL
			tkbind(bt.Remove, "<Button-1>", function(){
				id.fr <<- tclvalue(tkwinfo("parent", bt.Remove))
			})

			tkconfigure(bt.Remove, command = function(){
				id.frame <- sapply(.cdtData$GalParams$DATASETs, function(x) x$tcl$frame$ID)

				ii <- which(id.frame == id.fr)
				tkdestroy(.cdtData$GalParams$DATASETs[[ii]]$tcl$frame)
				.cdtData$GalParams$DATASETs[[ii]] <- NULL
				tcl("update")
			})

			####

			tkgrid(cb.datatype, row = 0, column = 0, sticky = 'we', rowspan = 1, columnspan = 8, padx = 1, pady = 1, ipadx = 1, ipady = 1)
			tkgrid(bt.datatype, row = 0, column = 8, sticky = 'we', rowspan = 1, columnspan = 2, padx = 1, pady = 1, ipadx = 1, ipady = 1)

			tkgrid(cb.en.datafile, row = 1, column = 0, sticky = 'we', rowspan = 1, columnspan = 9, padx = 0, pady = 1, ipadx = 1, ipady = 1)
			tkgrid(bt.datafile, row = 1, column = 9, sticky = 'w', rowspan = 1, columnspan = 1, padx = 0, pady = 1, ipadx = 1, ipady = 1)

			tkgrid(bt.Options, row = 2, column = 6, sticky = 'we', rowspan = 1, columnspan = 2, padx = 1, pady = 1, ipadx = 1, ipady = 1)
			tkgrid(bt.Remove, row = 2, column = 8, sticky = 'we', rowspan = 1, columnspan = 2, padx = 1, pady = 1, ipadx = 1, ipady = 1)


			tkgrid(.cdtData$GalParams$DATASETs[[jj]]$tcl$frame)

			####

			helpWidget(cb.datatype, 'Select the format of the input data', 'Select the format of the input data')
			helpWidget(bt.datatype, 'Set netcdf data (sample and filenames format)',
									'Set netcdf data (sample and filenames format)')

			if(str_trim(tclvalue(.cdtData$GalParams$DATASETs[[jj]]$tcl$data.type)) == CbdatatypeVAL[1]){
				helpWidget(cb.en.datafile, 'Browse file if not listed',
							'Select the file containing the station data')
			}else{
				helpWidget(cb.en.datafile, 'Enter the full path to directory containing the netcdf files',
										'Enter the full path to directory containing the netcdf files')
			}

			helpWidget(bt.datafile, 'or browse here', 'or browse here')

			####

			tkbind(cb.datatype, "<<ComboboxSelected>>", function(){
				tkdestroy(cb.en.datafile)
				tclvalue(.cdtData$GalParams$DATASETs[[jj]]$tcl$input.file) <- ''

				stateSetNC <- if(str_trim(tclvalue(.cdtData$GalParams$DATASETs[[jj]]$tcl$data.type)) == CbdatatypeVAL[1]) "disabled" else "normal"
				tkconfigure(bt.datatype, state = stateSetNC)

				if(str_trim(tclvalue(.cdtData$GalParams$DATASETs[[jj]]$tcl$data.type)) == CbdatatypeVAL[1]){
					cb.en.datafile <- ttkcombobox(.cdtData$GalParams$DATASETs[[jj]]$tcl$frame, values = unlist(listOpenFiles), textvariable = .cdtData$GalParams$DATASETs[[jj]]$tcl$input.file, width = largeur1)

					tkconfigure(bt.datafile, command = function(){
						dat.opfiles <- getOpenFiles(.cdtEnv$tcl$main$win)
						if(!is.null(dat.opfiles)){
							update.OpenFiles('ascii', dat.opfiles)
							listOpenFiles[[length(listOpenFiles) + 1]] <<- dat.opfiles[[1]]
							tclvalue(.cdtData$GalParams$DATASETs[[jj]]$tcl$input.file) <- dat.opfiles[[1]]
							tkconfigure(cb.en.datafile, values = unlist(openFile_ttkcomboList()))
							.cdtData$GalParams$DATASETs[[jj]]$pars$input$dir <- str_trim(tclvalue(.cdtData$GalParams$DATASETs[[jj]]$tcl$input.file))
						}
					})

					helpWidget(cb.en.datafile, 'Browse file if not listed',
								'Select the file containing the station data')

					.cdtData$GalParams$DATASETs[[jj]]$pars$data.type <- 'cdtstation'
					.cdtData$GalParams$DATASETs[[jj]]$pars$map.type <- "Points"
				}

				if(str_trim(tclvalue(.cdtData$GalParams$DATASETs[[jj]]$tcl$data.type)) == CbdatatypeVAL[2]){
					cb.en.datafile <- tkentry(.cdtData$GalParams$DATASETs[[jj]]$tcl$frame, textvariable = .cdtData$GalParams$DATASETs[[jj]]$tcl$input.file, width = largeur2)

					tkconfigure(bt.datatype, command = function(){
						.cdtData$GalParams$DATASETs[[jj]]$pars[["input"]] <- getInfoNetcdfData(.cdtEnv$tcl$main$win, .cdtData$GalParams$DATASETs[[jj]]$pars[["input"]],
																			str_trim(tclvalue(.cdtData$GalParams$DATASETs[[jj]]$tcl$input.file)),
																			str_trim(tclvalue(timeSteps)))
					})

					tkconfigure(bt.datafile, command = function(){
						dirnc <- tk_choose.dir(getwd(), "")
						tclvalue(.cdtData$GalParams$DATASETs[[jj]]$tcl$input.file) <- if(dirnc %in% c("", "NA") | is.na(dirnc)) "" else dirnc
						.cdtData$GalParams$DATASETs[[jj]]$pars$input$dir <- str_trim(tclvalue(.cdtData$GalParams$DATASETs[[jj]]$tcl$input.file))
					})

					helpWidget(cb.en.datafile, 'Enter the full path to directory containing the netcdf files',
											'Enter the full path to directory containing the netcdf files')

					.cdtData$GalParams$DATASETs[[jj]]$pars$data.type <- 'cdtnetcdf'
					.cdtData$GalParams$DATASETs[[jj]]$pars$map.type <- "Grid"
				}

				tkgrid(cb.en.datafile, row = 1, column = 0, sticky = 'we', rowspan = 1, columnspan = 9, padx = 0, pady = 1, ipadx = 1, ipady = 1)
			})
		}

	#######################################################################################################

	#Tab2
	subfr2 <- bwTabScrollableFrame(cmd.tab2)

		##############################################

		framePlotMap <- ttklabelframe(subfr2, text = "Map", relief = 'groove')

		date.year <- tclVar(GeneralParameters$date$year)
		date.mon <- tclVar(GeneralParameters$date$mon)
		date.day <- tclVar(GeneralParameters$date$day)
		date.hour <- tclVar(GeneralParameters$date$hour)

		txtdek <- switch(GeneralParameters$tstep, 'dekadal' = 'Dek', 'pentad' = 'Pen', 'Day')
		day.txtVar <- tclVar(txtdek)
		stateday <- if(GeneralParameters$tstep == 'monthly') 'disabled' else 'normal'
		statehour <- if(GeneralParameters$tstep == 'hourly') 'normal' else 'disabled'

		txt.yrs <- tklabel(framePlotMap, text = 'Year')
		txt.mon <- tklabel(framePlotMap, text = 'Month')
		txt.day <- tklabel(framePlotMap, text = tclvalue(day.txtVar), textvariable = day.txtVar)
		txt.hrs <- tklabel(framePlotMap, text = 'Hour')
		en.yrs <- tkentry(framePlotMap, width = 5, textvariable = date.year, justify = "center")
		en.mon <- tkentry(framePlotMap, width = 6, textvariable = date.mon, justify = "center")
		en.day <- tkentry(framePlotMap, width = 6, textvariable = date.day, justify = "center", state = stateday)
		en.hrs <- tkentry(framePlotMap, width = 5, textvariable = date.hour, justify = "center", state = statehour)

		bt.date.prev <- ttkbutton(framePlotMap, text = "<<", width = largeur4)
		bt.date.next <- ttkbutton(framePlotMap, text = ">>", width = largeur4)
		bt.Map.plot <- ttkbutton(framePlotMap, text = .cdtEnv$tcl$lang$global[['button']][['3']], width = largeur3)
		bt.Map.Opt <- ttkbutton(framePlotMap, text = .cdtEnv$tcl$lang$global[['button']][['4']], width = largeur4)

		##############

		.cdtData$EnvData$tab$multidataMap <- NULL

		tkconfigure(bt.Map.plot, command = function(){
			ret <- try(getData2Plot(), silent = TRUE)
			if(inherits(ret, "try-error") | is.null(ret)) return(NULL)

			imgContainer <- CDT.Display.Graph(MultipleData.Plot.Map, .cdtData$EnvData$tab$multidataMap, "Multiple_Datasets")
			.cdtData$EnvData$tab$multidataMap <- imageNotebookTab_unik(imgContainer, .cdtData$EnvData$tab$multidataMap)
		})

		tkconfigure(bt.date.prev, command = function(){
			if(!is.null(.cdtData$GalParams$donnees$dates)){
				nl <- length(.cdtData$GalParams$donnees$dates)
				date2plot <- .cdtData$GalParams$donnees$date2plot
				ix <- which(.cdtData$GalParams$donnees$dates == date2plot)
				ix <- ix - 1
				if(ix < 1) ix <- nl

				date2plot <- .cdtData$GalParams$donnees$dates[ix]
				tclvalue(date.year) <- substr(date2plot, 1, 4)
				tclvalue(date.mon) <- as.numeric(substr(date2plot, 5, 6))
				if(str_trim(tclvalue(timeSteps)) %in% CbperiodVAL[2:4])
					tclvalue(date.day) <- as.numeric(substr(date2plot, 7, 8))
				if(str_trim(tclvalue(timeSteps)) == CbperiodVAL[1])
					tclvalue(date.hour) <- as.numeric(substr(date2plot, 9, 10))
			}

			ret <- try(getData2Plot(), silent = TRUE)
			if(inherits(ret, "try-error") | is.null(ret)) return(NULL)

			imgContainer <- CDT.Display.Graph(MultipleData.Plot.Map, .cdtData$EnvData$tab$multidataMap, "Multiple_Datasets")
			.cdtData$EnvData$tab$multidataMap <- imageNotebookTab_unik(imgContainer, .cdtData$EnvData$tab$multidataMap)
		})

		tkconfigure(bt.date.next, command = function(){
			if(!is.null(.cdtData$GalParams$donnees$dates)){
				nl <- length(.cdtData$GalParams$donnees$dates)
				date2plot <- .cdtData$GalParams$donnees$date2plot
				ix <- which(.cdtData$GalParams$donnees$dates == date2plot)
				ix <- ix + 1
				if(ix > nl) ix <- 1

				date2plot <- .cdtData$GalParams$donnees$dates[ix]
				tclvalue(date.year) <- substr(date2plot, 1, 4)
				tclvalue(date.mon) <- as.numeric(substr(date2plot, 5, 6))
				if(str_trim(tclvalue(timeSteps)) %in% CbperiodVAL[2:4])
					tclvalue(date.day) <- as.numeric(substr(date2plot, 7, 8))
				if(str_trim(tclvalue(timeSteps)) == CbperiodVAL[1])
					tclvalue(date.hour) <- as.numeric(substr(date2plot, 9, 10))
			}


			ret <- try(getData2Plot(), silent = TRUE)
			if(inherits(ret, "try-error") | is.null(ret)) return(NULL)

			imgContainer <- CDT.Display.Graph(MultipleData.Plot.Map, .cdtData$EnvData$tab$multidataMap, "Multiple_Datasets")
			.cdtData$EnvData$tab$multidataMap <- imageNotebookTab_unik(imgContainer, .cdtData$EnvData$tab$multidataMap)
		})

		##############

		.cdtData$EnvData$dataMapOp <- list(presetCol = list(color = 'tim.colors', reverse = FALSE),
											userCol = list(custom = FALSE, color = NULL),
											userLvl = list(custom = FALSE, levels = NULL, equidist = FALSE),
											title = list(user = FALSE, title = ''),
											colkeyLab = list(user = FALSE, label = ''))

		tkconfigure(bt.Map.Opt, command = function(){
			if(!is.null(.cdtData$EnvData$data.range)){
				atlevel <- pretty(.cdtData$EnvData$data.range[, 3], n = 10, min.n = 7)
				if(is.null(.cdtData$EnvData$dataMapOp$userLvl$levels)){
					.cdtData$EnvData$dataMapOp$userLvl$levels <- atlevel
				}else{
					if(!.cdtData$EnvData$dataMapOp$userLvl$custom)
						.cdtData$EnvData$dataMapOp$userLvl$levels <- atlevel
				}
			}
			.cdtData$EnvData$dataMapOp <- MapGraph.MapOptions(.cdtData$EnvData$dataMapOp)
		})

		##############

		tkgrid(txt.yrs, row = 0, column = 1, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
		tkgrid(txt.mon, row = 0, column = 2, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
		tkgrid(txt.day, row = 0, column = 3, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
		tkgrid(txt.hrs, row = 0, column = 4, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)

		tkgrid(en.yrs, row = 1, column = 1, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
		tkgrid(en.mon, row = 1, column = 2, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
		tkgrid(en.day, row = 1, column = 3, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
		tkgrid(en.hrs, row = 1, column = 4, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)

		tkgrid(bt.date.prev, row = 2, column = 0, sticky = 'we', rowspan = 1, columnspan = 2, padx = 1, pady = 1, ipadx = 1, ipady = 1)
		tkgrid(bt.Map.plot, row = 2, column = 2, sticky = 'we', rowspan = 1, columnspan = 2, padx = 1, pady = 1, ipadx = 1, ipady = 1)
		tkgrid(bt.date.next, row = 2, column = 4, sticky = 'we', rowspan = 1, columnspan = 2, padx = 1, pady = 1, ipadx = 1, ipady = 1)

		tkgrid(bt.Map.Opt, row = 3, column = 4, sticky = 'we', rowspan = 1, columnspan = 2, padx = 1, pady = 1, ipadx = 1, ipady = 1)

		##############################################

		bt.Save <- ttkbutton(subfr2, text = "Save")
		bt.Load <- ttkbutton(subfr2, text = "Load")

		############

		tkconfigure(bt.Save, command = function(){
			if(WindowsOS())
				filename <- tclvalue(tkgetSaveFile(initialfile = "", filetypes = .cdtEnv$tcl$data$filetypes6, defaultextension = TRUE))
			else
				filename <- tclvalue(tkgetSaveFile(initialfile = "", filetypes = .cdtEnv$tcl$data$filetypes6))

			if(filename == "" | filename == 'NA' | is.na(filename)){
				Insert.Messages.Out('Provide a valid file name to save plot data', format = TRUE)
				return(NULL)
			}

			tkconfigure(.cdtEnv$tcl$main$win, cursor = 'watch')
			tcl('update')
			on.exit({
				tkconfigure(.cdtEnv$tcl$main$win, cursor = '')
				tcl('update')
			})

			####

			tstep <- periodVAL[CbperiodVAL %in% str_trim(tclvalue(timeSteps))]

			datasets <- lapply(seq_along(.cdtData$GalParams$DATASETs), function(j){
				if(str_trim(tclvalue(.cdtData$GalParams$DATASETs[[j]]$tcl$data.type)) == CbdatatypeVAL[1]){
					filename <- str_trim(tclvalue(.cdtData$GalParams$DATASETs[[j]]$tcl$input.file))
				}else{
					filename <- .cdtData$GalParams$DATASETs[[j]]$pars$input$sample
				}

				jfile <- getIndex.AllOpenFiles(filename)
				## test if exist
				Type <- .cdtData$OpenFiles$Type[[jfile]]
				Data <- .cdtData$OpenFiles$Data[[jfile]]
				nom <- .cdtData$OpenFiles$Data[[jfile]][[1]]
				PARS <- .cdtData$GalParams$DATASETs[[j]]$pars

				list(names = nom, PARS = PARS, Data = Data, Type = Type)
			})

			shp.data <- NULL
			if(tclvalue(.cdtData$EnvData$shp$add.shp) == "1"){
				nom <- str_trim(tclvalue(file.plotShp))
				if(nom != "" & !is.null(.cdtData$EnvData$shp$ocrds)){
					jfile <- getIndex.AllOpenFiles(nom)
					Type <- .cdtData$OpenFiles$Type[[jfile]]
					Data <- .cdtData$OpenFiles$Data[[jfile]]
					PARS <- .cdtData$EnvData$SHPOp
					ocrds <- .cdtData$EnvData$shp$ocrds
					shp.data <- list(names = nom, PARS = PARS, Data = Data, Type = Type, ocrds = ocrds)
				}
			}

			dates <- list(
						year = as.numeric(str_trim(tclvalue(date.year))),
						mon = as.numeric(str_trim(tclvalue(date.mon))),
						day = as.numeric(str_trim(tclvalue(date.day))),
						hour = as.numeric(str_trim(tclvalue(date.hour)))
						)

			tosave <- list(type = "multiplot", tstep = tstep, data = datasets, shp.data = shp.data,
							dates = dates, Options = .cdtData$EnvData$dataMapOp)
			saveRDS(tosave, filename)

			Insert.Messages.Out('Plot data saved successfully.')
		})

		############

		tkconfigure(bt.Load, command = function(){
			fileopen <- tclvalue(tkgetOpenFile(initialdir = getwd(), initialfile = "", filetypes = .cdtEnv$tcl$data$filetypes6))
			if(fileopen == "" | fileopen == 'NA' | is.na(fileopen)) return(NULL)

			tkconfigure(.cdtEnv$tcl$main$win, cursor = 'watch')
			tcl('update')
			on.exit({
				tkconfigure(.cdtEnv$tcl$main$win, cursor = '')
				tcl('update')
			})

			don <- readRDS(fileopen)

			returnNULL <- FALSE
			if(is.null(don$type)){
				returnNULL <- TRUE
			}else{
				if(don$type != "multiplot")
					returnNULL <- TRUE
			}

			if(returnNULL){
				Insert.Messages.Out('Not multiple data sets plot', format = TRUE)
				return(NULL)
			}

			###
			tclvalue(timeSteps) <- CbperiodVAL[periodVAL %in% don$tstep]

			###

			lapply(don$data, function(x){
				listOpFiles <- sapply(.cdtData$OpenFiles$Data, "[[", 1)
				if(x$name %in% listOpFiles){
					x$name <- paste0(x$name, ".1")
					x$Data[[1]] <- paste0(x$Data[[1]], ".1")
				}

				jj <- length(.cdtData$GalParams$DATASETs) + 1
				ids <- isaDataSet()
				.cdtData$GalParams$DATASETs[[jj]] <- list()
				.cdtData$GalParams$DATASETs[[jj]]$pars <- x$PARS
				add.new.datasets(jj, ids)

				jfile <- length(.cdtData$OpenFiles$Type) + 1
				.cdtData$OpenFiles$Type[[jfile]] <- x$Type
				.cdtData$OpenFiles$Data[[jfile]] <- x$Data
				tkinsert(.cdtEnv$tcl$main$Openfiles, "end", x$name)
			})

			###
			if(!is.null(don$shp.data)){
				listOpFiles <- sapply(.cdtData$OpenFiles$Data, "[[", 1)
				if(don$shp.data$names %in% listOpFiles){
					don$shp.data$names <- paste0(don$shp.data$names, ".1")
					don$shp.data$Data[[1]] <- paste0(don$shp.data$Data[[1]], ".1")
				}
				.cdtData$EnvData$SHPOp <- don$shp.data$PARS
				.cdtData$EnvData$shp$ocrds <- don$shp.data$ocrds

				jfile <- length(.cdtData$OpenFiles$Type) + 1
				.cdtData$OpenFiles$Type[[jfile]] <- don$shp.data$Type
				.cdtData$OpenFiles$Data[[jfile]] <- don$shp.data$Data
				tkinsert(.cdtEnv$tcl$main$Openfiles, "end", don$shp.data$names)

				tclvalue(.cdtData$EnvData$shp$add.shp) <- TRUE
				lapply(list(bt.addshpOpt, cb.addshp, bt.addshp), tkconfigure, state = "normal")
				tkconfigure(cb.addshp, values = unlist(openFile_ttkcomboList()))
				tclvalue(file.plotShp) <- don$shp.data$names
			}

			###
			.cdtData$EnvData$dataMapOp <- don$Options

			###
			tclvalue(date.year) <- don$dates$year
			tclvalue(date.mon) <- don$dates$mon
			tclvalue(date.day) <- don$dates$day
			tclvalue(date.hour) <- don$dates$hour

			stateday <- if(don$tstep == 'monthly') 'disabled' else 'normal'
			statehour <- if(don$tstep == 'hourly') 'normal' else 'disabled'
			txtdek <- switch(don$tstep, 'dekadal' = 'Dek', 'pentad' = 'Pen', 'Day')

			tclvalue(day.txtVar) <- txtdek
			tkconfigure(en.day, state = stateday)
			tkconfigure(en.hrs, state = statehour)
		})

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

		tkgrid(framePlotMap, row = 0, column = 0, sticky = 'we', rowspan = 1, columnspan = 2, padx = 1, pady = 1, ipadx = 1, ipady = 1)
		tkgrid(frameSHP, row = 1, column = 0, sticky = 'we', rowspan = 1, columnspan = 2, padx = 1, pady = 1, ipadx = 1, ipady = 1)
		tkgrid(bt.Save, row = 2, column = 0, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 3, ipadx = 1, ipady = 1)
		tkgrid(bt.Load, row = 2, column = 1, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 3, ipadx = 1, ipady = 1)

	#######################################################################################################

	getDatasets <- function(){
		Infodata <- lapply(.cdtData$GalParams$DATASETs, function(don) don$pars$input[c('dir', 'format')])

		getdatasets <- TRUE
		if(!is.null(.cdtData$GalParams$Infodata))
			if(isTRUE(all.equal(.cdtData$GalParams$Infodata, Infodata))) getdatasets <- FALSE

		if(getdatasets){
			tmp.don <- lapply(.cdtData$GalParams$DATASETs, function(don){
				if(don$pars$data.type == "cdtstation"){
					ret <- list(data = NULL, dates = NULL, msg = paste("Unable to read station data:", don$pars$input$dir))

					dat <- getStnOpenData(don$pars$input$dir)
					if(is.null(dat))  return(ret)

					dat <- splitCDTData0(dat)
					if(is.null(dat))  return(ret)

					ret <- list(data = dat, dates = dat$dates, msg = NULL)
				}else{
					if(str_trim(tclvalue(timeSteps)) == CbperiodVAL[1]){
						daty <- seq(as.POSIXct("1990010100", format = "%Y%m%d%H"), as.POSIXct("2020010100", format = "%Y%m%d%H"), "hour")
						daty <- format(daty, "%Y%m%d%H")
						year <- substr(daty, 1, 4)
						mon <- substr(daty, 5, 6)
						day <- substr(daty, 7, 8)
						hour <- substr(daty, 9, 10)
						ncfiles <- sprintf(don$pars$input$format, year, mon, day, hour)
					}

					if(str_trim(tclvalue(timeSteps)) == CbperiodVAL[2]){
						daty <- seq(as.Date("1960-01-01"), as.Date("2025-01-01"), "day")
						daty <- format(daty, "%Y%m%d")
						year <- substr(daty, 1, 4)
						mon <- substr(daty, 5, 6)
						day <- substr(daty, 7, 8)
						ncfiles <- sprintf(don$pars$input$format, year, mon, day)
					}

					if(str_trim(tclvalue(timeSteps)) == CbperiodVAL[3]){
						daty <- seq(as.Date("1960-01-01"), as.Date("2025-01-01"), "day")
						pen <- as.numeric(format(daty, "%d"))
						daty <- paste0(format(daty, "%Y%m")[pen <= 6], pen[pen <= 6])
						year <- substr(daty, 1, 4)
						mon <- substr(daty, 5, 6)
						day <- substr(daty, 7, 7)
						ncfiles <- sprintf(don$pars$input$format, year, mon, day)
					}

					if(str_trim(tclvalue(timeSteps)) == CbperiodVAL[4]){
						daty <- seq(as.Date("1960-01-01"), as.Date("2025-01-01"), "day")
						dek <- as.numeric(format(daty, "%d"))
						daty <- paste0(format(daty, "%Y%m")[dek <= 3], dek[dek <= 3])
						year <- substr(daty, 1, 4)
						mon <- substr(daty, 5, 6)
						day <- substr(daty, 7, 7)
						ncfiles <- sprintf(don$pars$input$format, year, mon, day)
					}

					if(str_trim(tclvalue(timeSteps)) == CbperiodVAL[5]){
						daty <- seq(as.Date("1950-01-01"), as.Date("2025-01-01"), "month")
						daty <- format(daty, "%Y%m")
						year <- substr(daty, 1, 4)
						mon <- substr(daty, 5, 6)
						ncfiles <- sprintf(don$pars$input$format, year, mon)
					}

					ret <- list(data = NULL, dates = NULL, msg = NULL)

					nc.path <- file.path(don$pars$input$dir, ncfiles)
					nc.exist <- file.exists(nc.path)
					if(!any(nc.exist)){
						ret$msg <- paste("No netcdf files found in:", don$pars$input$dir)
						return(ret)
					}

					ncInfo <- getNCDFSampleData(don$pars$input$sample)
					if(is.null(ncInfo)){
						ret$msg <- "No netcdf data sample found"
						return(ret)
					}

					daty <- daty[nc.exist]
					nc.path <- nc.path[nc.exist]
					dat <- c(ncInfo, list(path = nc.path))
					ret <- list(data = dat, dates = daty, msg = NULL)
				}

				return(ret)
			})

			no.data <- sapply(lapply(tmp.don, "[[", "data"), is.null)
			if(any(no.data)){
				sapply(lapply(tmp.don[no.data], "[[", "msg"), Insert.Messages.Out, format = TRUE)
				return(NULL)
			}

			daty <- Reduce(intersect, lapply(tmp.don, "[[", "dates"))
			if(length(daty) == 0){
				Insert.Messages.Out("Dates do not overlap", format = TRUE)
				return(NULL)
			}

			####
			tclvalue(date.year) <- substr(daty[1], 1, 4)
			tclvalue(date.mon) <- as.numeric(substr(daty[1], 5, 6))
			if(str_trim(tclvalue(timeSteps)) %in% CbperiodVAL[2:4])
				tclvalue(date.day) <- as.numeric(substr(daty[1], 7, 8))
			if(str_trim(tclvalue(timeSteps)) == CbperiodVAL[1])
				tclvalue(date.hour) <- as.numeric(substr(daty[1], 9, 10))

			####
			.cdtData$GalParams$donnees <- list(dates = daty, donnees = tmp.don)
			.cdtData$GalParams$Infodata <- Infodata
		}

		return(0)
	}

	#########################

	getData2Plot <- function(){
		tkconfigure(.cdtEnv$tcl$main$win, cursor = 'watch')
		tcl('update')
		on.exit({
			tkconfigure(.cdtEnv$tcl$main$win, cursor = '')
			tcl('update')
		})

		ret <- try(getDatasets(), silent = TRUE)
		if(inherits(ret, "try-error") | is.null(ret)) return(NULL)

		year <- str_trim(tclvalue(date.year))
		mon <- str_pad(str_trim(tclvalue(date.mon)), 2, pad = "0")
		day <- str_pad(str_trim(tclvalue(date.day)), 2, pad = "0")
		hour <- str_pad(str_trim(tclvalue(date.hour)), 2, pad = "0")

		if(str_trim(tclvalue(timeSteps)) == CbperiodVAL[1])
			daty2plot <- paste0(year, mon, day, hour)
		if(str_trim(tclvalue(timeSteps)) %in% CbperiodVAL[2:4])
			daty2plot <- paste0(year, mon, day)
		if(str_trim(tclvalue(timeSteps)) == CbperiodVAL[5])
			daty2plot <- paste0(year, mon)

		data.Obj <- lapply(seq_along(.cdtData$GalParams$DATASETs), function(jj){
			params <- .cdtData$GalParams$DATASETs[[jj]]
			don <- .cdtData$GalParams$donnees$donnees[[jj]]
			pars.plot <- params$pars[c('map.type', 'plot.type', 'title', 'point.size')]
			if(pars.plot$map.type == "Grid" & pars.plot$plot.type == "Points")
				pars.plot$plot.type <- "Pixels"

			if(params$pars$data.type == "cdtstation"){
				lon <- don$data$lon
				lat <- don$data$lat
				zval <- don$data$data[don$dates == daty2plot, , drop = FALSE]
				zval <- as.numeric(zval[1, ])
				if(length(zval) == 0){
					msg <- paste(daty2plot, "does not exist in", params$pars$input$dir)
					return(list(obj = NULL, msg = msg))
				}

				if(params$pars$plot.type == "Pixels"){
					nx <- nx_ny_as.image(diff(range(lon)))
					ny <- nx_ny_as.image(diff(range(lat)))
					tmp <- cdt.as.image(zval, nx = nx, ny = ny, pts.xy = cbind(lon, lat))
				}else tmp <- list(x = lon, y = lat, z = zval)
			}else{
				ncinfo <- don$data
				ncfile <- ncinfo$path[don$dates == daty2plot]
				if(length(ncfile) == 0){
					msg <- paste("No file:", ncfile)
					return(list(obj = NULL, msg = msg))
				}

				nc <- try(nc_open(ncfile), silent = TRUE)
				if(inherits(nc, "try-error")){
					msg <- paste("Unable to read:", ncfile)
					return(list(obj = NULL, msg = msg))
				}
				zval <- ncvar_get(nc, varid = ncinfo$varid)
				nc_close(nc)
				zval <- if(ncinfo$ilon < ncinfo$ilat) zval[ncinfo$xo, ncinfo$yo] else t(zval)[ncinfo$xo, ncinfo$yo]
				tmp <- list(x = ncinfo$lon, y = ncinfo$lat, z = zval)
			}

			return(list(obj = c(tmp, pars.plot), msg = NULL))
		})

		Obj <- lapply(data.Obj, "[[", "obj")
		no.data <- sapply(Obj, is.null)
		if(any(no.data)) sapply(lapply(data.Obj[no.data], "[[", "msg"), Insert.Messages.Out, format = TRUE)

		.cdtData$GalParams$donnees$date2plot <- daty2plot
		.cdtData$EnvData$data.Obj <- Obj
		.cdtData$EnvData$data.range <- do.call(rbind, lapply(Obj, function(x) sapply(x[c('x', 'y', 'z')], range, na.rm = TRUE)))

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
