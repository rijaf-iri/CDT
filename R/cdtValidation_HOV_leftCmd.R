
Validation.HOV.PanelCmd <- function(clim.var){
	listOpenFiles <- openFile_ttkcomboList()
	if(WindowsOS()){
		largeur0 <- .cdtEnv$tcl$fun$w.widgets(30)
		largeur <- .cdtEnv$tcl$fun$w.widgets(28)
		largeur1 <- .cdtEnv$tcl$fun$w.widgets(30)
		largeur2 <- 30
		largeur3 <- 28
	}else{
		largeur0 <- .cdtEnv$tcl$fun$w.widgets(26)
		largeur <- .cdtEnv$tcl$fun$w.widgets(22)
		largeur1 <- .cdtEnv$tcl$fun$w.widgets(23)
		largeur2 <- 22
		largeur3 <- 20
	}

	GeneralParameters <- fromJSON(file.path(.cdtDir$dirLocal, 'init_params', 'Validation_HOV.json'))
	MOIS <- format(ISOdate(2014, 1:12, 1), "%b")

	# xml.dlg <- file.path(.cdtDir$dirLocal, "languages", "cdtValidation_HOV_leftCmd.xml")
	# lang.dlg <- cdtLanguageParse(xml.dlg, .cdtData$Config$lang.iso)
	# .cdtData$EnvData$message <- lang.dlg[['message']]

	CHXSTATS0 <- c('Correlation', 'Nash-Sutcliffe Efficiency', 'Bias', 'Mean Absolute Error', 'Mean Error', 'Root Mean Square Error')
	CHXSTATS1 <- c('Probability Of Detection', 'False Alarm Ratio', 'Frequency Bias', 'Critical Success Index', 'Heidke Skill Score')
	CHXSTATS2 <- c('Volumetric Hit Index', 'Quantile Probability of Detection', 'Volumetric False Alarm Ratio',
					'Quantile False Alarm Ratio', 'Volumetric Miss Index', 'Volumetric Critical Success Index',
					'Quantile Critical Success Index')
	CHXSTATS <- c(CHXSTATS0, CHXSTATS1, CHXSTATS2)

	##############
	.cdtData$EnvData$zoom$xx1 <- tclVar()
	.cdtData$EnvData$zoom$xx2 <- tclVar()
	.cdtData$EnvData$zoom$yy1 <- tclVar()
	.cdtData$EnvData$zoom$yy2 <- tclVar()

	.cdtData$EnvData$zoom$pressButP <- tclVar(0)
	.cdtData$EnvData$zoom$pressButM <- tclVar(0)
	.cdtData$EnvData$zoom$pressButRect <- tclVar(0)
	.cdtData$EnvData$zoom$pressButDrag <- tclVar(0)

	.cdtData$EnvData$pressGetCoords <- tclVar(0)

	ZoomXYval0 <- NULL

	###################

	.cdtEnv$tcl$main$cmd.frame <- tkframe(.cdtEnv$tcl$main$panel.left)

	tknote.cmd <- bwNoteBook(.cdtEnv$tcl$main$cmd.frame)

	cmd.tab1 <- bwAddTab(tknote.cmd, text = "Input")
	cmd.tab2 <- bwAddTab(tknote.cmd, text = "Extraction")
	cmd.tab3 <- bwAddTab(tknote.cmd, text = "Validation")
	cmd.tab4 <- bwAddTab(tknote.cmd, text = "Plot")
	cmd.tab5 <- bwAddTab(tknote.cmd, text = "Add layers")

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

	##############################################

		frInputData <- ttklabelframe(subfr1, text = "Input data", relief = 'groove')

		file.period <- tclVar()
		CbperiodVAL <- .cdtEnv$tcl$lang$global[['combobox']][['1']][2:5]
		periodVAL <- c('daily', 'pentad', 'dekadal', 'monthly')
		tclvalue(file.period) <- CbperiodVAL[periodVAL %in% GeneralParameters$Tstep]

		file.stnfl <- tclVar(GeneralParameters$STN.file)
		dirNetCDF <- tclVar(GeneralParameters$ncdf.file$dir)

		cb.tstep <- ttkcombobox(frInputData, values = CbperiodVAL, textvariable = file.period)
		txt.stnfl <- tklabel(frInputData, text = 'Station data file', anchor = 'w', justify = 'left')
		cb.stnfl <- ttkcombobox(frInputData, values = unlist(listOpenFiles), textvariable = file.stnfl, width = largeur)
		bt.stnfl <- tkbutton(frInputData, text = "...")

		txt.dir.ncdf <- tklabel(frInputData, text = "Directory of NetCDF files", anchor = 'w', justify = 'left')
		set.dir.ncdf <- ttkbutton(frInputData, text = .cdtEnv$tcl$lang$global[['button']][['5']])
		en.dir.ncdf <- tkentry(frInputData, textvariable = dirNetCDF, width = largeur1)
		bt.dir.ncdf <- tkbutton(frInputData, text = "...")

		#######################

		tkconfigure(bt.stnfl, command = function(){
			dat.opfiles <- getOpenFiles(.cdtEnv$tcl$main$win)
			if(!is.null(dat.opfiles)){
				update.OpenFiles('ascii', dat.opfiles)
				listOpenFiles[[length(listOpenFiles) + 1]] <<- dat.opfiles[[1]]
				tclvalue(file.stnfl) <- dat.opfiles[[1]]
				lapply(list(cb.stnfl, cb.shpF, cb.adddem, cb.addshp), tkconfigure, values = unlist(listOpenFiles))
			}
		})

		tkconfigure(set.dir.ncdf, command = function(){
			GeneralParameters[["ncdf.file"]] <<- getInfoNetcdfData(.cdtEnv$tcl$main$win, GeneralParameters[["ncdf.file"]],
																	str_trim(tclvalue(dirNetCDF)), str_trim(tclvalue(file.period)))
		})

		tkconfigure(bt.dir.ncdf, command = function(){
			dirnc <- tk_choose.dir(getwd(), "")
			tclvalue(dirNetCDF) <- if(!is.na(dirnc)) dirnc else ""
		})

		#######################

		tkgrid(cb.tstep, row = 0, column = 0, sticky = '', rowspan = 1, columnspan = 5, padx = 1, pady = 2, ipadx = 1, ipady = 1)
		tkgrid(txt.stnfl, row = 1, column = 0, sticky = 'we', rowspan = 1, columnspan = 5, padx = 1, pady = 0, ipadx = 1, ipady = 1)
		tkgrid(cb.stnfl, row = 2, column = 0, sticky = 'we', rowspan = 1, columnspan = 4, padx = 0, pady = 0, ipadx = 1, ipady = 1)
		tkgrid(bt.stnfl, row = 2, column = 4, sticky = 'w', rowspan = 1, columnspan = 1, padx = 0, pady = 0, ipadx = 1, ipady = 1)

		tkgrid(txt.dir.ncdf, row = 3, column = 0, sticky = 'we', rowspan = 1, columnspan = 3, padx = 1, pady = 0, ipadx = 1, ipady = 1)
		tkgrid(set.dir.ncdf, row = 3, column = 3, sticky = 'we', rowspan = 1, columnspan = 2, padx = 1, pady = 0, ipadx = 1, ipady = 1)
		tkgrid(en.dir.ncdf, row = 4, column = 0, sticky = 'we', rowspan = 1, columnspan = 4, padx = 0, pady = 0, ipadx = 1, ipady = 1)
		tkgrid(bt.dir.ncdf, row = 4, column = 4, sticky = 'w', rowspan = 1, columnspan = 1, padx = 0, pady = 0, ipadx = 1, ipady = 1)

		infobulle(cb.tstep, 'Select the time step of the data')
		status.bar.display(cb.tstep, 'Select the time step of the data')
		infobulle(cb.stnfl, 'Select the station data from the list')
		status.bar.display(cb.stnfl, 'Select the file containing the station data in CDT format')
		infobulle(bt.stnfl, 'Browse file if not listed')
		status.bar.display(bt.stnfl, 'Browse file if not listed')

		infobulle(en.dir.ncdf, 'Enter the full path to the directory containing the NetCDF files')
		status.bar.display(en.dir.ncdf, 'Enter the full path to the directory containing the NetCDF files')
		infobulle(bt.dir.ncdf, 'Or browse here')
		status.bar.display(bt.dir.ncdf, 'Or browse here')
		infobulle(set.dir.ncdf, 'Setting netcdf data options')
		status.bar.display(set.dir.ncdf, 'Setting netcdf data options')

		#######################

		tkbind(cb.tstep, "<<ComboboxSelected>>", function(){
			tclvalue(day.txtVar) <- ifelse(str_trim(tclvalue(file.period)) == CbperiodVAL[3], 'Dekad',
									ifelse(str_trim(tclvalue(file.period)) == CbperiodVAL[2], 'Pentad', 'Day'))
			statedate <<- if(str_trim(tclvalue(file.period)) == CbperiodVAL[4]) 'disabled' else 'normal'

			AGGREGFUN <- c("mean", "sum", "count")
			if(tclvalue(aggr.data) == "0"){
				stateo0a <- "disabled"
				stateo0b <- "disabled"
				stateo1 <- "disabled"
				stateo2 <- "disabled"
			}else{
				if(str_trim(tclvalue(file.period)) != CbperiodVAL[1]){
					AGGREGFUN <- AGGREGFUN[-3]
					tclvalue(aggr.fun) <- if(tclvalue(aggr.fun) == "count") "sum" else tclvalue(aggr.fun)
				}
				stateo0a <- "readonly"
				stateo0b <- "normal"
				stateo1 <- if(tclvalue(aggr.fun) == "count") "readonly" else "disabled"
				stateo2 <- if(tclvalue(aggr.fun) == "count") "normal" else "disabled"
			}

			tkconfigure(cb.aggfun, values = AGGREGFUN, state = stateo0a)
			# tkconfigure(en.minfrac, state = stateo0b)
			tkconfigure(cb.opfun, state = stateo1)
			tkconfigure(en.opthres, state = stateo2)
			tkconfigure(cb.stats.maps, values = CHXSTATS)
		})

		##############################################

		btDateRange <- ttkbutton(subfr1, text = "Extraction Date Range")

		txtdek <- switch(GeneralParameters$Tstep, 'dekadal' = 'Dekad', 'pentad' = 'Pentad', 'Day')
		day.txtVar <- tclVar(txtdek)
		statedate <- if(GeneralParameters$Tstep == 'monthly') 'disabled' else 'normal'

		tkconfigure(btDateRange, command = function(){
			GeneralParameters[["Extract.Date"]] <<- getInfoDateRange(.cdtEnv$tcl$main$win,
													GeneralParameters[["Extract.Date"]],
													daypendek.lab = tclvalue(day.txtVar),
													state.dek = statedate)
		})

		infobulle(btDateRange, 'Start and end date to perform the merging')
		status.bar.display(btDateRange, 'Start and end date to perform the merging')

		##############################################

		frameDirSav <- ttklabelframe(subfr1, text = "Directory to save result", relief = 'groove')

		file.save1 <- tclVar(GeneralParameters$outdir)

		en.dir.save <- tkentry(frameDirSav, textvariable = file.save1, width = largeur1)
		bt.dir.save <- tkbutton(frameDirSav, text = "...")
		#######################

		tkconfigure(bt.dir.save, command = function() fileORdir2Save(file.save1, isFile = FALSE))

		#############################

		tkgrid(en.dir.save, row = 0, column = 0, sticky = 'we', rowspan = 1, columnspan = 5, padx = 1, pady = 1, ipadx = 1, ipady = 1)
		tkgrid(bt.dir.save, row = 0, column = 5, sticky = 'e', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)

		infobulle(en.dir.save, 'Enter the full path to the directory  to save result')
		status.bar.display(en.dir.save, 'Enter the full path to the directory to save result')
		infobulle(bt.dir.save, 'Browse here the full path to the directory to save result')
		status.bar.display(bt.dir.save, 'Browse here the full path to the directory to save result')

		#############################
		tkgrid(frInputData, row = 0, column = 0, sticky = 'we', padx = 1, pady = 1, ipadx = 1, ipady = 1)
		tkgrid(btDateRange, row = 1, column = 0, sticky = 'we', padx = 1, pady = 3, ipadx = 1, ipady = 1)
		tkgrid(frameDirSav, row = 2, column = 0, sticky = 'we', padx = 1, pady = 1, ipadx = 1, ipady = 1)

	#######################################################################################################

	#Tab2
	subfr2 <- bwTabScrollableFrame(cmd.tab2)

	##############################################

		frameSelect <- ttklabelframe(subfr2, text = "Selection Type", relief = 'groove')

		.cdtData$EnvData$type.select <- tclVar()
		SELECTALL <- c('All Stations', 'Rectangle', 'Polygons')
		TypeSelect <- c('all', 'rect', 'poly')
		tclvalue(.cdtData$EnvData$type.select) <- SELECTALL[TypeSelect %in% GeneralParameters$type.select]

		cb.type.select <- ttkcombobox(frameSelect, values = SELECTALL, textvariable = .cdtData$EnvData$type.select)

		tkgrid(cb.type.select, row = 0, column = 0, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1)

		######
		tkbind(cb.type.select, "<<ComboboxSelected>>", function(){
			.cdtData$EnvData$selectedPolygon <- NULL

			if(tclvalue(.cdtData$EnvData$type.select) == SELECTALL[1]){
				statelonlat <- 'disabled'
				statepolygon <- 'disabled'
			}

			if(tclvalue(.cdtData$EnvData$type.select) == SELECTALL[2]){
				statelonlat <- 'normal'
				statepolygon <- 'disabled'
			}

			if(tclvalue(.cdtData$EnvData$type.select) == SELECTALL[3]){
				statelonlat <- 'disabled'
				statepolygon <- 'normal'

				if(tclvalue(.cdtData$EnvData$namePoly) != ''){
					shpfopen <- getShpOpenData(file.dispShp)
					if(!is.null(shpfopen)){
						shpf <- shpfopen[[2]]
						ids <- as.integer(tclvalue(tcl(.cdtData$EnvData$cb.shpAttr, 'current'))) + 1
						.cdtData$EnvData$selectedPolygon <- getBoundaries(shpf[shpf@data[, ids] == tclvalue(.cdtData$EnvData$namePoly), ])
					}
				}
			}

			tkconfigure(en.minlon, state = statelonlat)
			tkconfigure(en.maxlon, state = statelonlat)
			tkconfigure(en.minlat, state = statelonlat)
			tkconfigure(en.maxlat, state = statelonlat)
			tkconfigure(.cdtData$EnvData$cb.shpAttr, state = statepolygon)
			tkconfigure(cb.Polygon, state = statepolygon)

			##
			tclvalue(.cdtData$EnvData$minlonRect) <- ''
			tclvalue(.cdtData$EnvData$maxlonRect) <- ''
			tclvalue(.cdtData$EnvData$minlatRect) <- ''
			tclvalue(.cdtData$EnvData$maxlatRect) <- ''
			tkconfigure(.cdtData$EnvData$bt.select, relief = 'raised', bg = 'lightblue', state = 'normal')

			tabid <- as.integer(tclvalue(tkindex(.cdtEnv$tcl$main$tknotes, 'current'))) + 1
			if(length(.cdtData$OpenTab$Type) > 0)
			{
				if(.cdtData$OpenTab$Type[[tabid]] == "img" & !is.null(.cdtData$EnvData$tab$MapSelect))
				{
					if(.cdtData$OpenTab$Data[[tabid]][[1]][[1]]$ID  == .cdtData$EnvData$tab$MapSelect[[2]])
					{
						refreshPlot(W = .cdtData$OpenTab$Data[[tabid]][[2]][[1]],
									img = .cdtData$OpenTab$Data[[tabid]][[2]][[2]],
									hscale = as.numeric(tclvalue(tkget(.cdtEnv$tcl$toolbar$spinH))),
									vscale = as.numeric(tclvalue(tkget(.cdtEnv$tcl$toolbar$spinV))))
						tkdelete(tkwinfo('children', .cdtData$OpenTab$Data[[tabid]][[1]][[2]]), 'rect')
					}
				}
			}
		})

		##############################################

		frameShp <- ttklabelframe(subfr2, text = "Boundaries Shapefiles", relief = 'groove')

		file.dispShp <- tclVar(GeneralParameters$shp.file$shp)
		shpAttr <- tclVar(GeneralParameters$shp.file$attr)
		.cdtData$EnvData$namePoly <- tclVar()

		cb.shpF <- ttkcombobox(frameShp, values = unlist(listOpenFiles), textvariable = file.dispShp, width = largeur)
		bt.shpF <- tkbutton(frameShp, text = "...")
		txt.attr.shpF <- tklabel(frameShp, text = "Attribute field to be used and displayed", anchor = 'w', justify = 'left')
		.cdtData$EnvData$cb.shpAttr <- ttkcombobox(frameShp, values='', textvariable = shpAttr, width = largeur0, state = 'disabled')
		cb.Polygon <- ttkcombobox(frameShp, values = '', textvariable = .cdtData$EnvData$namePoly, width = largeur0, state = 'disabled')

		tkgrid(cb.shpF, row = 0, column = 0, sticky = 'we', rowspan = 1, columnspan = 7, padx = 1, pady = 1)
		tkgrid(bt.shpF, row = 0, column = 7, sticky = 'w', rowspan = 1, columnspan = 1, padx = 0, pady = 1)
		tkgrid(txt.attr.shpF, row = 1, column = 0, sticky = 'we', rowspan = 1, columnspan = 8, padx = 1, pady = 1)
		tkgrid(.cdtData$EnvData$cb.shpAttr, row = 2, column = 0, sticky = 'we', rowspan = 1, columnspan = 8, padx = 1, pady = 2)
		tkgrid(cb.Polygon, row = 3, column = 0, sticky = 'we', rowspan = 1, columnspan = 8, padx = 1, pady = 2)

		#######################
		tkconfigure(bt.shpF, command = function(){
			shp.opfiles <- getOpenShp(.cdtEnv$tcl$main$win)
			if(!is.null(shp.opfiles)){
				update.OpenFiles('shp', shp.opfiles)
				tclvalue(file.dispShp) <- shp.opfiles[[1]]
				listOpenFiles[[length(listOpenFiles) + 1]] <<- shp.opfiles[[1]]
				lapply(list(cb.stnfl, cb.shpF, cb.adddem, cb.addshp), tkconfigure, values = unlist(listOpenFiles))

				###
				shpf <- getShpOpenData(file.dispShp)
				dat <- shpf[[2]]@data
				AttrTable <- names(dat)
				tclvalue(shpAttr) <- AttrTable[1]

				# adminN <- as.character(dat[, as.integer(tclvalue(tcl(.cdtData$EnvData$cb.shpAttr, 'current'))) + 1])
				adminN <- as.character(dat[, 1])
				name.poly <- levels(as.factor(adminN))
				if(length(name.poly) < 2) name.poly <- c(name.poly, "")
				tclvalue(.cdtData$EnvData$namePoly) <- name.poly[1]

				tkconfigure(.cdtData$EnvData$cb.shpAttr, values = AttrTable)
				tkconfigure(cb.Polygon, values = name.poly)
			}
		})

		#######################
		tkbind(cb.shpF, "<<ComboboxSelected>>", function(){
			shpf <- getShpOpenData(file.dispShp)
			if(!is.null(shpf)){
				dat <- shpf[[2]]@data
				AttrTable <- names(dat)
				tclvalue(shpAttr) <- AttrTable[1]
				adminN <- as.character(dat[, as.integer(tclvalue(tcl(.cdtData$EnvData$cb.shpAttr, 'current'))) + 1])
				name.poly <- levels(as.factor(adminN))
				if(length(name.poly) < 2) name.poly <- c(name.poly, "")
			}else{
				AttrTable <- ''
				tclvalue(shpAttr) <- ''
				name.poly <- ''
				tclvalue(.cdtData$EnvData$namePoly) <- ''
			}
			tkconfigure(.cdtData$EnvData$cb.shpAttr, values = AttrTable)
			tkconfigure(cb.Polygon, values = name.poly)
		})

		########################
		tkbind(.cdtData$EnvData$cb.shpAttr, "<<ComboboxSelected>>", function(){
			shpf <- getShpOpenData(file.dispShp)
			if(!is.null(shpf)){
				dat <- shpf[[2]]@data
				adminN <- as.character(dat[, as.integer(tclvalue(tcl(.cdtData$EnvData$cb.shpAttr, 'current'))) + 1])
				name.poly <- levels(as.factor(adminN))
				if(length(name.poly) < 2) name.poly <- c(name.poly, "")
			}else{
				name.poly <- ''
			}
			tclvalue(.cdtData$EnvData$namePoly) <- name.poly[1]
			tkconfigure(cb.Polygon, values = name.poly)
		})

		########################
		tkbind(cb.Polygon, "<<ComboboxSelected>>", function(){
			.cdtData$EnvData$selectedPolygon <- NULL
			if(tclvalue(.cdtData$EnvData$namePoly) != ''){
				shpfopen <- getShpOpenData(file.dispShp)
				if(!is.null(shpfopen)){
					shpf <- shpfopen[[2]]
					ids <- as.integer(tclvalue(tcl(.cdtData$EnvData$cb.shpAttr, 'current'))) + 1
					.cdtData$EnvData$selectedPolygon <- getBoundaries(shpf[shpf@data[, ids] == tclvalue(.cdtData$EnvData$namePoly), ])
				}
			}

			tabid <- as.integer(tclvalue(tkindex(.cdtEnv$tcl$main$tknotes, 'current'))) + 1
			if(length(.cdtData$OpenTab$Type) > 0)
			{
				if(.cdtData$OpenTab$Type[[tabid]] == "img" & !is.null(.cdtData$EnvData$tab$MapSelect))
				{
					if(.cdtData$OpenTab$Data[[tabid]][[1]][[1]]$ID  == .cdtData$EnvData$tab$MapSelect[[2]])
					{
						refreshPlot(W = .cdtData$OpenTab$Data[[tabid]][[2]][[1]],
									img = .cdtData$OpenTab$Data[[tabid]][[2]][[2]],
									hscale = as.numeric(tclvalue(tkget(.cdtEnv$tcl$toolbar$spinH))),
									vscale = as.numeric(tclvalue(tkget(.cdtEnv$tcl$toolbar$spinV))))
					}
				}
			}
		})

		##############################################

		frameIMgMan <- tkframe(subfr2)

		#######################

		frameZoom <- ttklabelframe(frameIMgMan, text = "ZOOM", relief = 'groove')

		.cdtData$EnvData$zoom$btZoomP <- tkbutton(frameZoom, image = .cdtEnv$tcl$zoom$img$plus, relief = 'raised', bg = 'lightblue', state = 'normal')
		.cdtData$EnvData$zoom$btZoomM <- tkbutton(frameZoom, image = .cdtEnv$tcl$zoom$img$moins, relief = 'raised', bg = 'lightblue', state = 'normal')
		.cdtData$EnvData$zoom$btZoomRect <- tkbutton(frameZoom, image = .cdtEnv$tcl$zoom$img$rect, relief = 'raised', bg = 'lightblue', state = 'normal')
		.cdtData$EnvData$zoom$btPanImg <- tkbutton(frameZoom, image = .cdtEnv$tcl$zoom$img$pan, relief = 'raised', bg = 'lightblue', state = 'normal')
		.cdtData$EnvData$zoom$btRedraw <- tkbutton(frameZoom, image = .cdtEnv$tcl$zoom$img$redraw, relief = 'raised', state = 'disabled')
		.cdtData$EnvData$zoom$btReset <- tkbutton(frameZoom, image = .cdtEnv$tcl$zoom$img$reset, relief = 'raised')

		#######################
		tkgrid(.cdtData$EnvData$zoom$btZoomP, row = 0, column = 0, sticky = 'nswe', rowspan = 1, columnspan = 1)
		tkgrid(.cdtData$EnvData$zoom$btZoomM, row = 0, column = 1, sticky = 'nswe', rowspan = 1, columnspan = 1)
		tkgrid(.cdtData$EnvData$zoom$btZoomRect, row = 0, column = 2, sticky = 'nswe', rowspan = 1, columnspan = 1)
		tkgrid(.cdtData$EnvData$zoom$btReset, row = 1, column = 0, sticky = 'nswe', rowspan = 1, columnspan = 1)
		tkgrid(.cdtData$EnvData$zoom$btRedraw, row = 1, column = 1, sticky = 'nswe', rowspan = 1, columnspan = 1)
		tkgrid(.cdtData$EnvData$zoom$btPanImg, row = 1, column = 2, sticky = 'nswe', rowspan = 1, columnspan = 1)

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

		##############################################

		frameDisp <- tkframe(frameIMgMan)

		.cdtData$EnvData$minlonRect <- tclVar()
		.cdtData$EnvData$maxlonRect <- tclVar()
		.cdtData$EnvData$minlatRect <- tclVar()
		.cdtData$EnvData$maxlatRect <- tclVar()

		bt.dispMap <- tkbutton(frameDisp, text = "Display Map")
		.cdtData$EnvData$bt.select <- tkbutton(frameDisp, text = "Select", relief = 'raised', bg = 'lightblue')

		txt.minLab <- tklabel(frameDisp, text = 'Min')
		txt.maxLab <- tklabel(frameDisp, text = 'Max')
		txt.lonLab <- tklabel(frameDisp, text = 'Lon', anchor = 'e', justify = 'right')
		txt.latLab <- tklabel(frameDisp, text = 'Lat', anchor = 'e', justify = 'right')
		en.minlon <- tkentry(frameDisp, width = 4, textvariable = .cdtData$EnvData$minlonRect, justify = "left", state = 'disabled')
		en.maxlon <- tkentry(frameDisp, width = 4, textvariable = .cdtData$EnvData$maxlonRect, justify = "left", state = 'disabled')
		en.minlat <- tkentry(frameDisp, width = 4, textvariable = .cdtData$EnvData$minlatRect, justify = "left", state = 'disabled')
		en.maxlat <- tkentry(frameDisp, width = 4, textvariable = .cdtData$EnvData$maxlatRect, justify = "left", state = 'disabled')

		#######################

		.cdtData$EnvData$tab$MapSelect <- NULL

		tkconfigure(bt.dispMap, command = function(){
			donne <- getStnOpenData(file.stnfl)
			shpofile <- getShpOpenData(file.dispShp)
			if(!is.null(donne)){
				.cdtData$EnvData$donne <- donne[1:3, -1]
				lonStn <- as.numeric(.cdtData$EnvData$donne[2, ])
				latStn <- as.numeric(.cdtData$EnvData$donne[3, ])
				lo1 <- min(lonStn, na.rm = TRUE)
				lo2 <- max(lonStn, na.rm = TRUE)
				la1 <- min(latStn, na.rm = TRUE)
				la2 <- max(latStn, na.rm = TRUE)
				plotOK <- TRUE
				shpf <- shpofile[[2]]
				.cdtData$EnvData$ocrds <- getBoundaries(shpf)
				.cdtData$EnvData$shpf <- shpf
			}else{
				plotOK <- FALSE
				Insert.Messages.Out('No station data found', format = TRUE)
			}

			if(tclvalue(.cdtData$EnvData$type.select) == SELECTALL[3] & plotOK){
				if(!is.null(shpofile)){
					shpf <- shpofile[[2]]
					.cdtData$EnvData$ocrds <- getBoundaries(shpf)
					.cdtData$EnvData$shpf <- shpf
					bbxshp <- round(bbox(shpf), 4)
					lo1 <- min(lo1, bbxshp[1, 1])
					lo2 <- max(lo2, bbxshp[1, 2])
					la1 <- min(la1, bbxshp[2, 1])
					la2 <- max(la2, bbxshp[2, 2])
					plotOK <- TRUE
				}else{
					plotOK <- FALSE
					Insert.Messages.Out('No ESRI shapfile for administrative boundaries found', format = TRUE)
				}
			}

			if(plotOK){
				ZoomXYval0 <<- c(lo1, lo2, la1, la2)
				tclvalue(.cdtData$EnvData$zoom$xx1) <- lo1
				tclvalue(.cdtData$EnvData$zoom$xx2) <- lo2
				tclvalue(.cdtData$EnvData$zoom$yy1) <- la1
				tclvalue(.cdtData$EnvData$zoom$yy2) <- la2
				.cdtData$EnvData$ZoomXYval <- ZoomXYval0

				imgContainer <- displayMap4Validation(.cdtData$EnvData$tab$MapSelect)
				.cdtData$EnvData$tab$MapSelect <- imageNotebookTab_unik(imgContainer, .cdtData$EnvData$tab$MapSelect)
			}
		})

		#######################

		tkgrid(bt.dispMap, row = 0, column = 0, sticky = 'we', rowspan = 1, columnspan = 2)
		tkgrid(.cdtData$EnvData$bt.select, row = 0, column = 2, sticky = 'we', rowspan = 1, columnspan = 1)
		tkgrid(txt.minLab, row = 1, column = 1, sticky = 'we', rowspan = 1, columnspan = 1)
		tkgrid(txt.maxLab, row = 1, column = 2, sticky = 'we', rowspan = 1, columnspan = 1)
		tkgrid(txt.lonLab, row = 2, column = 0, sticky = 'we', rowspan = 1, columnspan = 1)
		tkgrid(txt.latLab, row = 3, column = 0, sticky = 'we', rowspan = 1, columnspan = 1)
		tkgrid(en.minlon, row = 2, column = 1, sticky = 'we', rowspan = 1, columnspan = 1)
		tkgrid(en.maxlon, row = 2, column = 2, sticky = 'we', rowspan = 1, columnspan = 1)
		tkgrid(en.minlat, row = 3, column = 1, sticky = 'we', rowspan = 1, columnspan = 1)
		tkgrid(en.maxlat, row = 3, column = 2, sticky = 'we', rowspan = 1, columnspan = 1)

		#######################
		tkgrid(frameZoom, row = 0, column = 0, sticky = 'ns', columnspan = 2, ipady = 5)
		tkgrid(frameDisp, row = 0, column = 2, sticky = 'we', columnspan = 4)

		##############################################

		if(!is.null(.cdtData$EnvData$hovd)){
			stateBTEx <- if(tclvalue(.cdtData$EnvData$hovd) == "1") "normal" else "disabled"
		}else stateBTEx <- "normal"

		bt.extract.station <- ttkbutton(subfr2, text = "Extract Data for Validation", state = stateBTEx)

		tkconfigure(bt.extract.station, command = function(){
			GeneralParameters$clim.var <- clim.var
			GeneralParameters$Tstep <- periodVAL[CbperiodVAL %in% str_trim(tclvalue(file.period))]
			GeneralParameters$STN.file <- str_trim(tclvalue(file.stnfl))
			GeneralParameters$ncdf.file$dir <- str_trim(tclvalue(dirNetCDF))
			GeneralParameters$outdir <- str_trim(tclvalue(file.save1))

			GeneralParameters$shp.file$shp <- str_trim(tclvalue(file.dispShp))
			GeneralParameters$shp.file$attr <- str_trim(tclvalue(shpAttr))

			GeneralParameters$type.select <- TypeSelect[SELECTALL %in% str_trim(tclvalue(.cdtData$EnvData$type.select))]

			GeneralParameters$Geom <- NULL
			GeneralParameters$Geom$minlon <- as.numeric(str_trim(tclvalue(.cdtData$EnvData$minlonRect)))
			GeneralParameters$Geom$maxlon <- as.numeric(str_trim(tclvalue(.cdtData$EnvData$maxlonRect)))
			GeneralParameters$Geom$minlat <- as.numeric(str_trim(tclvalue(.cdtData$EnvData$minlatRect)))
			GeneralParameters$Geom$maxlat <- as.numeric(str_trim(tclvalue(.cdtData$EnvData$maxlatRect)))
			GeneralParameters$Geom$namePoly <- str_trim(tclvalue(.cdtData$EnvData$namePoly))

			# assign('GeneralParameters', GeneralParameters, envir = .GlobalEnv)

			Insert.Messages.Out("Extract data .................")

			tkconfigure(.cdtEnv$tcl$main$win, cursor = 'watch')
			tcl('update')
			ret <- tryCatch(
				{
					HOV_DataExtraction(GeneralParameters)
				},
				warning = function(w) warningFun(w),
				error = function(e) errorFun(e),
				finally = {
					tkconfigure(.cdtEnv$tcl$main$win, cursor = '')
					tcl('update')
				}
			)

			msg0 <- "Data extraction finished successfully"
			msg1 <- "Data extraction failed"
			if(!is.null(ret)){
				if(ret == 0){
					Insert.Messages.Out(msg0)
				}else Insert.Messages.Out(msg1, format = TRUE)
			}else Insert.Messages.Out(msg1, format = TRUE)
		})

		#######################

		tkgrid(frameSelect, row = 0, column = 0, sticky = '')
		tkgrid(frameShp, row = 1, column = 0, sticky = 'we', pady = 3)
		tkgrid(frameIMgMan, row = 2, column = 0, sticky = 'we', pady = 3)
		tkgrid(bt.extract.station, row = 3, column = 0, sticky = 'we', pady = 3)

		##############################################

		tkconfigure(.cdtData$EnvData$zoom$btReset, command = function(){
			.cdtData$EnvData$ZoomXYval <- ZoomXYval0
			tclvalue(.cdtData$EnvData$zoom$xx1) <- ZoomXYval0[1]
			tclvalue(.cdtData$EnvData$zoom$xx2) <- ZoomXYval0[2]
			tclvalue(.cdtData$EnvData$zoom$yy1) <- ZoomXYval0[3]
			tclvalue(.cdtData$EnvData$zoom$yy2) <- ZoomXYval0[4]
			
			tabid <- as.numeric(tclvalue(tkindex(.cdtEnv$tcl$main$tknotes, 'current'))) + 1
			if(length(.cdtData$OpenTab$Type) > 0){
				if(.cdtData$OpenTab$Type[[tabid]] == "img" & !is.null(.cdtData$EnvData$tab$MapSelect))
				{
					if(.cdtData$OpenTab$Data[[tabid]][[1]][[1]]$ID  == .cdtData$EnvData$tab$MapSelect[[2]])
					{
						refreshPlot(W = .cdtData$OpenTab$Data[[tabid]][[2]][[1]],
									img = .cdtData$OpenTab$Data[[tabid]][[2]][[2]],
									hscale = as.numeric(tclvalue(tkget(.cdtEnv$tcl$toolbar$spinH))),
									vscale = as.numeric(tclvalue(tkget(.cdtEnv$tcl$toolbar$spinV))))
					}
				}
			}
		})

		##########################

		tkbind(.cdtData$EnvData$zoom$btReset, "<Button-1>", function(){
			tclvalue(.cdtData$EnvData$zoom$pressButP) <- 0
			tclvalue(.cdtData$EnvData$zoom$pressButM) <- 0
			tclvalue(.cdtData$EnvData$zoom$pressButRect) <- 0
			tclvalue(.cdtData$EnvData$zoom$pressButDrag) <- 0

			tclvalue(.cdtData$EnvData$pressGetCoords) <- 0

			tkconfigure(.cdtData$EnvData$zoom$btZoomP, relief = 'raised', bg = 'lightblue', state = 'normal')
			tkconfigure(.cdtData$EnvData$zoom$btZoomM, relief = 'raised', bg = 'lightblue', state = 'normal')
			tkconfigure(.cdtData$EnvData$zoom$btZoomRect, relief = 'raised', bg = 'lightblue', state = 'normal')
			tkconfigure(.cdtData$EnvData$zoom$btPanImg, relief = 'raised', bg = 'lightblue', state = 'normal')

			tkconfigure(.cdtData$EnvData$bt.select, relief = 'raised', bg = 'lightblue', state = 'normal')
		})

		tkbind(.cdtData$EnvData$zoom$btZoomP, "<Button-1>", function(){
			tclvalue(.cdtData$EnvData$zoom$pressButP) <- 1
			tclvalue(.cdtData$EnvData$zoom$pressButM) <- 0
			tclvalue(.cdtData$EnvData$zoom$pressButRect) <- 0
			tclvalue(.cdtData$EnvData$zoom$pressButDrag) <- 0

			tclvalue(.cdtData$EnvData$pressGetCoords) <- 0

			tkconfigure(.cdtData$EnvData$zoom$btZoomP, relief = 'raised', bg = 'red', state = 'disabled')
			tkconfigure(.cdtData$EnvData$zoom$btZoomM, relief = 'raised', bg = 'lightblue', state = 'normal')
			tkconfigure(.cdtData$EnvData$zoom$btZoomRect, relief = 'raised', bg = 'lightblue', state = 'normal')
			tkconfigure(.cdtData$EnvData$zoom$btPanImg, relief = 'raised', bg = 'lightblue', state = 'normal')

			tkconfigure(.cdtData$EnvData$bt.select, relief = 'raised', bg = 'lightblue', state = 'normal')
		})

		tkbind(.cdtData$EnvData$zoom$btZoomM, "<Button-1>", function(){
			tclvalue(.cdtData$EnvData$zoom$pressButP) <- 0
			tclvalue(.cdtData$EnvData$zoom$pressButM) <- 1
			tclvalue(.cdtData$EnvData$zoom$pressButRect) <- 0
			tclvalue(.cdtData$EnvData$zoom$pressButDrag) <- 0

			tclvalue(.cdtData$EnvData$pressGetCoords) <- 0

			tkconfigure(.cdtData$EnvData$zoom$btZoomP, relief = 'raised', bg = 'lightblue', state = 'normal')
			tkconfigure(.cdtData$EnvData$zoom$btZoomM, relief = 'raised', bg = 'red', state = 'disabled')
			tkconfigure(.cdtData$EnvData$zoom$btZoomRect, relief = 'raised', bg = 'lightblue', state = 'normal')
			tkconfigure(.cdtData$EnvData$zoom$btPanImg, relief = 'raised', bg = 'lightblue', state = 'normal')

			tkconfigure(.cdtData$EnvData$bt.select, relief = 'raised', bg = 'lightblue', state = 'normal')
		})

		tkbind(.cdtData$EnvData$zoom$btZoomRect, "<Button-1>", function(){
			tclvalue(.cdtData$EnvData$zoom$pressButP) <- 0
			tclvalue(.cdtData$EnvData$zoom$pressButM) <- 0
			tclvalue(.cdtData$EnvData$zoom$pressButRect) <- 1
			tclvalue(.cdtData$EnvData$zoom$pressButDrag) <- 0

			tclvalue(.cdtData$EnvData$pressGetCoords) <- 0

			tkconfigure(.cdtData$EnvData$zoom$btZoomP, relief = 'raised', bg = 'lightblue', state = 'normal')
			tkconfigure(.cdtData$EnvData$zoom$btZoomM, relief = 'raised', bg = 'lightblue', state = 'normal')
			tkconfigure(.cdtData$EnvData$zoom$btZoomRect, relief = 'raised', bg = 'red', state = 'disabled')
			tkconfigure(.cdtData$EnvData$zoom$btPanImg, relief = 'raised', bg = 'lightblue', state = 'normal')

			tkconfigure(.cdtData$EnvData$bt.select, relief = 'raised', bg = 'lightblue', state = 'normal')
		})

		tkbind(.cdtData$EnvData$zoom$btPanImg, "<Button-1>", function(){
			tclvalue(.cdtData$EnvData$zoom$pressButP) <- 0
			tclvalue(.cdtData$EnvData$zoom$pressButM) <- 0
			tclvalue(.cdtData$EnvData$zoom$pressButRect) <- 0
			tclvalue(.cdtData$EnvData$zoom$pressButDrag) <- 1

			tclvalue(.cdtData$EnvData$pressGetCoords) <- 0

			tkconfigure(.cdtData$EnvData$zoom$btZoomP, relief = 'raised', bg = 'lightblue', state = 'normal')
			tkconfigure(.cdtData$EnvData$zoom$btZoomM, relief = 'raised', bg = 'lightblue', state = 'normal')
			tkconfigure(.cdtData$EnvData$zoom$btZoomRect, relief = 'raised', bg = 'lightblue', state = 'normal')
			tkconfigure(.cdtData$EnvData$zoom$btPanImg, relief = 'raised', bg = 'red', state = 'disabled')

			tkconfigure(.cdtData$EnvData$bt.select, relief = 'raised', bg = 'lightblue', state = 'normal')
		})

		tkbind(.cdtData$EnvData$bt.select, "<Button-1>", function(){
			tclvalue(.cdtData$EnvData$zoom$pressButP) <- 0
			tclvalue(.cdtData$EnvData$zoom$pressButM) <- 0
			tclvalue(.cdtData$EnvData$zoom$pressButRect) <- 0
			tclvalue(.cdtData$EnvData$zoom$pressButDrag) <- 0

			tclvalue(.cdtData$EnvData$pressGetCoords) <- 1

			tkconfigure(.cdtData$EnvData$zoom$btZoomP, relief = 'raised', bg = 'lightblue', state = 'normal')
			tkconfigure(.cdtData$EnvData$zoom$btZoomM, relief = 'raised', bg = 'lightblue', state = 'normal')
			tkconfigure(.cdtData$EnvData$zoom$btZoomRect, relief = 'raised', bg = 'lightblue', state = 'normal')
			tkconfigure(.cdtData$EnvData$zoom$btPanImg, relief = 'raised', bg = 'lightblue', state = 'normal')

			tkconfigure(.cdtData$EnvData$bt.select, relief = 'raised', bg = 'red', state = 'disabled')
		})

	#######################################################################################################

	#Tab3
	subfr3 <- bwTabScrollableFrame(cmd.tab3)

	##############################################

		frameHOV <- ttklabelframe(subfr3, text = "Hold-Out Validation data", relief = 'groove')

		.cdtData$EnvData$hovd <- tclVar(0)
		file.hovd <- tclVar()

		stateHOVd <- if(tclvalue(.cdtData$EnvData$hovd) == "1") "normal" else "disabled"

		chk.hovd <- tkcheckbutton(frameHOV, variable = .cdtData$EnvData$hovd, text = "Hold-Out Validation already performed", anchor = 'w', justify = 'left')
		en.hovd <- tkentry(frameHOV, textvariable = file.hovd, width = largeur1, state = stateHOVd)
		bt.hovd <- tkbutton(frameHOV, text = "...", state = stateHOVd)

		tkconfigure(bt.hovd, command = function(){
			path.hovd <- tclvalue(tkgetOpenFile(initialdir = getwd(), filetypes = .cdtEnv$tcl$data$filetypes6))
			if(path.hovd == "") return(NULL)
			tclvalue(file.hovd) <- path.hovd

			if(file.exists(str_trim(tclvalue(file.hovd)))){
				hovd.data <- try(readRDS(str_trim(tclvalue(file.hovd))), silent = TRUE)
				if(inherits(hovd.data, "try-error")){
					Insert.Messages.Out('Unable to load Hold-Out Validation data', format = TRUE)
					Insert.Messages.Out(gsub('[\r\n]', '', hovd.data[1]), format = TRUE)
					return(NULL)
				}
				.cdtData$EnvData$file.hovd <- str_trim(tclvalue(file.hovd))
				.cdtData$EnvData$GeneralParameters <- hovd.data$GeneralParameters
				.cdtData$EnvData$cdtData <- hovd.data$cdtData
				.cdtData$EnvData$stnData <- hovd.data$stnData
				.cdtData$EnvData$ncdfData <- hovd.data$ncdfData

				if(!is.null(hovd.data$opDATA)){
					.cdtData$EnvData$opDATA <- hovd.data$opDATA
					.cdtData$EnvData$Statistics <- hovd.data$Statistics
				}

				###
				tclvalue(file.period) <- CbperiodVAL[periodVAL %in% hovd.data$GeneralParameters$Tstep]

				##
				AGGREGFUN <- c("mean", "sum", "count")
				if(tclvalue(aggr.data) == "1"){
					if(tclvalue(file.period) != CbperiodVAL[1]){
						AGGREGFUN <- AGGREGFUN[-3]
						tclvalue(aggr.fun) <- if(tclvalue(aggr.fun) == "count") "sum" else tclvalue(aggr.fun)
					}
					stateo0a <- "readonly"
				}else stateo0a <- "disabled"
				tkconfigure(cb.aggfun, values = AGGREGFUN, state = stateo0a)

				if(!is.null(.cdtData$EnvData$opDATA$id)){
					stateDispSTN <- if(str_trim(tclvalue(stat.data)) == STATDATATYPE[3]) 'normal' else 'disabled'
					tkconfigure(cb.stat.sel, values = .cdtData$EnvData$opDATA$id, state = stateDispSTN)
					tclvalue(stn.stat.tab) <- .cdtData$EnvData$opDATA$id[1]
					tkconfigure(bt.stat.prev, state = stateDispSTN)
					tkconfigure(bt.stat.next, state = stateDispSTN)
					stateMaps <- if(str_trim(tclvalue(stat.data)) == STATDATATYPE[3]) 'normal' else 'disabled'
					tkconfigure(cb.stats.maps, state = stateMaps)
					tkconfigure(bt.stats.maps, state = stateMaps)
					tkconfigure(cb.plot.type, state = stateMaps)

					stateStnID <- if(str_trim(tclvalue(stat.data)) == STATDATATYPE[3]) 'normal' else 'disabled'
					tkconfigure(cb.stn.graph, values = .cdtData$EnvData$opDATA$id, state = stateStnID)
					tclvalue(.cdtData$EnvData$stnIDGraph) <- .cdtData$EnvData$opDATA$id[1]
					tkconfigure(bt.stn.graph.prev, state = stateStnID)
					tkconfigure(bt.stn.graph.next, state = stateStnID)

					TYPEGRAPH <- c("Scatter", "CDF", "Lines")
					if(str_trim(tclvalue(stat.data)) == STATDATATYPE[1]){
						TYPEGRAPH <- c("Scatter", "CDF")
						if(tclvalue(.cdtData$EnvData$type.graph) == "Lines")
							tclvalue(.cdtData$EnvData$type.graph) <- "Scatter"
					}
					tkconfigure(cb.stats.graph, values = TYPEGRAPH)
				}
			}
		})

		tkgrid(chk.hovd, row = 0, column = 0, sticky = 'we', rowspan = 1, columnspan = 5, padx = 1, pady = 1, ipadx = 1, ipady = 1)
		tkgrid(en.hovd, row = 1, column = 0, sticky = 'we', rowspan = 1, columnspan = 4, padx = 1, pady = 1, ipadx = 1, ipady = 1)
		tkgrid(bt.hovd, row = 1, column = 4, sticky = 'w', rowspan = 1, columnspan = 1, padx = 0, pady = 1, ipadx = 1, ipady = 1)

		###############
		tkbind(chk.hovd, "<Button-1>", function(){
			stateHOVd <- if(tclvalue(.cdtData$EnvData$hovd) == '1') 'disabled' else 'normal'
			tkconfigure(en.hovd, state = stateHOVd)
			tkconfigure(bt.hovd, state = stateHOVd)
			stateBTEx <- if(tclvalue(.cdtData$EnvData$hovd) == '1') 'normal' else 'disabled'
			tkconfigure(bt.extract.station, state = stateBTEx)
		})

		##############################################

		frameSeason <- ttklabelframe(subfr3, text = "Years & Season", relief = 'groove')

		mon1 <- as.numeric(str_trim(GeneralParameters$date.range$start.month))
		mon2 <- as.numeric(str_trim(GeneralParameters$date.range$end.month))
		start.mois <- tclVar(MOIS[mon1])
		end.mois <- tclVar(MOIS[mon2])
		start.year <- tclVar(GeneralParameters$date.range$start.year)
		end.year <- tclVar(GeneralParameters$date.range$end.year)

		fr.seas <- ttklabelframe(frameSeason, text = 'Season', relief = 'sunken', labelanchor = "n", borderwidth = 2)
		fr.year <- ttklabelframe(frameSeason, text = 'Years', relief = 'sunken', labelanchor = "n", borderwidth = 2)

		txt.to1 <- tklabel(fr.year, text = '-to-')
		en.years1 <- tkentry(fr.year, width = 5, textvariable = start.year, justify = 'right')
		en.years2 <- tkentry(fr.year, width = 5, textvariable = end.year, justify = 'right')

		txt.to2 <- tklabel(fr.seas, text = '-to-')
		cb.month1 <- ttkcombobox(fr.seas, values = MOIS, textvariable = start.mois, width = 4)
		cb.month2 <- ttkcombobox(fr.seas, values = MOIS, textvariable = end.mois, width = 4)

		tkgrid(en.years1, row = 0, column = 0, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1)
		tkgrid(txt.to1, row = 0, column = 1, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1)
		tkgrid(en.years2, row = 0, column = 2, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1)

		tkgrid(cb.month1, row = 0, column = 0, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1)
		tkgrid(txt.to2, row = 0, column = 1, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1)
		tkgrid(cb.month2, row = 0, column = 2, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1)

		tkgrid(fr.seas, row = 0, column = 0, sticky = 'ns', rowspan = 1, columnspan = 1, padx = 3, pady = 1)
		tkgrid(fr.year, row = 0, column = 1, sticky = 'ns', rowspan = 1, columnspan = 1, padx = 3, pady = 1)

		infobulle(en.years1, 'Start year of the period to calculate the statistics')
		status.bar.display(en.years1, 'Start year of the period to calculate the statistics')
		infobulle(en.years2, 'End year of the period to calculate the statistics')
		status.bar.display(en.years2, 'End year of the period to calculate the statistics')
		infobulle(cb.month1, 'Start month of the period to calculate the statistics')
		status.bar.display(cb.month1, 'Start month of the period to calculate the statistics')
		infobulle(cb.month2, 'End month of the season to calculate the statistics')
		status.bar.display(cb.month2, 'End month of the season to calculate the statistics')

		##############################################

		frameAggr <- ttklabelframe(subfr3, text = "Data aggregation", relief = 'groove')

		aggr.data <- tclVar(GeneralParameters$aggr.series$aggr.data)
		aggr.fun <- tclVar(GeneralParameters$aggr.series$aggr.fun)
		# min.frac <- tclVar(GeneralParameters$aggr.series$min.frac)
		opr.fun <- tclVar(GeneralParameters$aggr.series$opr.fun)
		opr.thres <- tclVar(GeneralParameters$aggr.series$opr.thres)

		AGGREGFUN <- c("mean", "sum", "count")
		if(GeneralParameters$Tstep != 'daily' & !GeneralParameters$aggr.series$aggr.data) AGGREGFUN <- AGGREGFUN[-3]
		if(!GeneralParameters$aggr.series$aggr.data){
			stateo0a <- 'disabled'
			stateo0b <- 'disabled'
			stateo1 <- 'disabled'
			stateo2 <- 'disabled'
		}else{
			stateo0a <- 'readonly'
			stateo0b <- 'normal'
			stateo1 <- if(str_trim(GeneralParameters$aggr.series$aggr.fun) == "count") 'readonly' else 'disabled'
			stateo2 <- if(str_trim(GeneralParameters$aggr.series$aggr.fun) == "count") 'normal' else 'disabled'
		}

		chk.aggrdata <- tkcheckbutton(frameAggr, variable = aggr.data, text = "Aggregate data", anchor = 'w', justify = 'left')
		txt.aggfun <- tklabel(frameAggr, text = 'Function', anchor = 'w', justify = 'left')
		cb.aggfun <- ttkcombobox(frameAggr, values = AGGREGFUN, textvariable = aggr.fun, width = 6, state = stateo0a)
		# txt.minfrac <- tklabel(frameAggr, text = 'Min.Frac', anchor = 'w', justify = 'left')
		# en.minfrac <- tkentry(frameAggr, textvariable = min.frac, width = 6, state = stateo0b)
		txt.opfun <- tklabel(frameAggr, text = 'Operator', anchor = 'w', justify = 'left')
		cb.opfun <- ttkcombobox(frameAggr, values = c(">=", ">", "<=", "<"), textvariable = opr.fun, width = 6, state = stateo1)
		txt.opthres <- tklabel(frameAggr, text = 'Threshold', anchor = 'w', justify = 'left')
		en.opthres <- tkentry(frameAggr, textvariable = opr.thres, width = 6, state = stateo2)

		tkgrid(chk.aggrdata, row = 0, column = 0, sticky = 'we', rowspan = 1, columnspan = 2, padx = 1, pady = 1, ipadx = 1, ipady = 1)
		tkgrid(txt.aggfun, row = 0, column = 2, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
		tkgrid(cb.aggfun, row = 0, column = 3, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
		# tkgrid(txt.minfrac, row = 1, column = 2, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
		# tkgrid(en.minfrac, row = 1, column = 3, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
		tkgrid(txt.opfun, row = 1, column = 0, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
		tkgrid(cb.opfun, row = 1, column = 1, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
		tkgrid(txt.opthres, row = 1, column = 2, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
		tkgrid(en.opthres, row = 1, column = 3, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)

		infobulle(cb.aggfun, 'Function that have to be applied for aggregating from daily/dekadal/monthly into\na higher time step (e.g., for precipitation FUN=sum and for temperature FUN=mean)')
		status.bar.display(cb.aggfun, 'Function that have to be applied for aggregating from daily/dekadal/monthly into\na higher time step (e.g., for precipitation FUN=sum and for temperature FUN=mean)')
		# infobulle(en.minfrac, 'Minimum fraction of available data that must be present within each output time step')
		# status.bar.display(en.minfrac, 'Minimum fraction of available data that must be present within each output time step')
		infobulle(cb.opfun, 'Select the comparison operator to be used to match event')
		status.bar.display(cb.opfun, 'Select the comparison operator to be used to match event')
		infobulle(en.opthres, 'User defined threshold applied to count event')
		status.bar.display(en.opthres, 'User defined threshold applied to count event')

		#################
		tkbind(cb.aggfun, "<<ComboboxSelected>>", function(){
			stateo1 <- if(tclvalue(aggr.fun) == "count") "readonly" else "disabled"
			stateo2 <- if(tclvalue(aggr.fun) == "count") "normal" else "disabled"
			tkconfigure(cb.opfun, state = stateo1)
			tkconfigure(en.opthres, state = stateo2)
		})

		tkbind(chk.aggrdata, "<Button-1>", function(){
			if(tclvalue(aggr.data) == "1"){
				stateo0a <- 'disabled'
				stateo0b <- 'disabled'
				stateo1 <- 'disabled'
				stateo2 <- 'disabled'
			}else{
				stateo0a <- 'readonly'
				stateo0b <- 'normal'
				stateo1 <- if(tclvalue(aggr.fun) == "count") 'readonly' else 'disabled'
				stateo2 <- if(tclvalue(aggr.fun) == "count") 'normal' else 'disabled'
			}

			tkconfigure(cb.aggfun, state = stateo0a)
			# tkconfigure(en.minfrac, state = stateo0b)
			tkconfigure(cb.opfun, state = stateo1)
			tkconfigure(en.opthres, state = stateo2)
			tkconfigure(cb.stats.maps, values = CHXSTATS)
		})

		#############################

		STATDATATYPE <- c('All Data', 'Spatial Average', 'Per station')
		StatDataT <- c('all', 'avg', 'stn')
		stat.data <- tclVar()
		tclvalue(stat.data) <- STATDATATYPE[StatDataT %in% GeneralParameters$stat.data]

		cb.stat.data <- ttkcombobox(subfr3, values = STATDATATYPE, textvariable = stat.data, width = largeur0)

		infobulle(cb.stat.data, 'Use all data or a spatial average or station by station to calculate the statistics')
		status.bar.display(cb.stat.data, 'Use all data or a spatial average or station by station to calculate the statistics')

		#################
		tkbind(cb.stat.data, "<<ComboboxSelected>>", function(){
			stateDispSTN <- if(str_trim(tclvalue(stat.data)) == STATDATATYPE[3]) 'normal' else 'disabled'
			tkconfigure(bt.stat.prev, state = stateDispSTN)
			tkconfigure(cb.stat.sel, state = stateDispSTN)
			tkconfigure(bt.stat.next, state = stateDispSTN)
			stateMaps <- if(str_trim(tclvalue(stat.data)) == STATDATATYPE[3]) 'normal' else 'disabled'
			tkconfigure(cb.stats.maps, state = stateMaps)
			tkconfigure(bt.stats.maps, state = stateMaps)
			tkconfigure(cb.plot.type, state = stateMaps)
			stateStnID <- if(str_trim(tclvalue(stat.data)) == STATDATATYPE[3]) 'normal' else 'disabled'
			tkconfigure(cb.stn.graph, state = stateStnID)
			tkconfigure(bt.stn.graph.prev, state = stateStnID)
			tkconfigure(bt.stn.graph.next, state = stateStnID)

			TYPEGRAPH <- c("Scatter", "CDF", "Lines")
			if(str_trim(tclvalue(stat.data)) == STATDATATYPE[1]){
				TYPEGRAPH <- c("Scatter", "CDF")
				if(tclvalue(.cdtData$EnvData$type.graph) == "Lines")
					tclvalue(.cdtData$EnvData$type.graph) <- "Scatter"
			}
			tkconfigure(cb.stats.graph, values = TYPEGRAPH)
		})

		##############################################

		frameDicho <- ttklabelframe(subfr3, text = "Dichotomous validation", relief = 'groove')

		if(clim.var == 'RR') trhesVal <- 1
		if(clim.var == 'TT') trhesVal <- 20
		dicho.thres <- tclVar(trhesVal)
		# dicho.thres <- tclVar(GeneralParameters$dicho.fcst$opr.thres)
		dicho.opr <- tclVar(GeneralParameters$dicho.fcst$opr.fun)

		txt.dicho <- tklabel(frameDicho, text = 'Threshold', anchor = 'w', justify = 'left')
		cb.dicho <- ttkcombobox(frameDicho, values = c(">=", ">", "<=", "<"), textvariable = dicho.opr, width = 4, state = 'readonly')
		en.dicho <- tkentry(frameDicho, textvariable = dicho.thres, width = 6)

		tkgrid(txt.dicho, row = 0, column = 0, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
		tkgrid(cb.dicho, row = 0, column = 1, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
		tkgrid(en.dicho, row = 0, column = 2, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)

		infobulle(en.dicho, 'Threshold to be specified to separate "yes" and "no" events')
		status.bar.display(en.dicho, 'Threshold to be specified to separate "yes" and "no" events')

		##############################################

		bt.calc.stat <- ttkbutton(subfr3, text = "Calculate Statistics")

		tkconfigure(bt.calc.stat, command = function(){
			GeneralParameters$date.range$start.month <- which(MOIS %in% str_trim(tclvalue(start.mois)))
			GeneralParameters$date.range$end.month <- which(MOIS %in% str_trim(tclvalue(end.mois)))
			GeneralParameters$date.range$start.year <- as.numeric(str_trim(tclvalue(start.year)))
			GeneralParameters$date.range$end.year <- as.numeric(str_trim(tclvalue(end.year)))

			GeneralParameters$aggr.series$aggr.data <- switch(tclvalue(aggr.data), '0' = FALSE, '1' = TRUE)
			GeneralParameters$aggr.series$aggr.fun <- str_trim(tclvalue(aggr.fun))
			# GeneralParameters$aggr.series$min.frac <- as.numeric(str_trim(tclvalue(min.frac)))
			GeneralParameters$aggr.series$opr.fun <- str_trim(tclvalue(opr.fun))
			GeneralParameters$aggr.series$opr.thres <- as.numeric(str_trim(tclvalue(opr.thres)))

			GeneralParameters$stat.data <- StatDataT[STATDATATYPE %in% str_trim(tclvalue(stat.data))]

			GeneralParameters$dicho.fcst$opr.thres <- as.numeric(str_trim(tclvalue(dicho.thres)))
			GeneralParameters$dicho.fcst$opr.fun <- str_trim(tclvalue(dicho.opr))

			#####
			GeneralParameters$STN.file <- str_trim(tclvalue(file.stnfl))
			GeneralParameters$outdir <- str_trim(tclvalue(file.save1))

			# assign('GeneralParameters', GeneralParameters, envir = .GlobalEnv)

			Insert.Messages.Out("Validation .................")

			tkconfigure(.cdtEnv$tcl$main$win, cursor = 'watch')
			tcl('update')
			ret <- tryCatch(
				{
					ValidationDataProcs(GeneralParameters)
				},
				warning = function(w) warningFun(w),
				error = function(e) errorFun(e),
				finally = {
					tkconfigure(.cdtEnv$tcl$main$win, cursor = '')
					tcl('update')
				}
			)

			msg0 <- "Statistics calculation finished successfully"
			msg1 <- "Validation failed"
			if(!is.null(ret)){
				if(ret == 0){
					Insert.Messages.Out(msg0)

					if(str_trim(tclvalue(stat.data)) == STATDATATYPE[3]){
						tkconfigure(cb.stat.sel, values = .cdtData$EnvData$opDATA$id)
						tclvalue(stn.stat.tab) <- .cdtData$EnvData$opDATA$id[1]

						tkconfigure(cb.stn.graph, values = .cdtData$EnvData$opDATA$id, state = 'normal')
						tclvalue(.cdtData$EnvData$stnIDGraph) <- .cdtData$EnvData$opDATA$id[1]
					}
				}else Insert.Messages.Out(msg1, format = TRUE)
			}else Insert.Messages.Out(msg1, format = TRUE)
		})

		#############################
		tkgrid(frameHOV, row = 0, column = 0, sticky = 'we')
		tkgrid(frameSeason, row = 1, column = 0, sticky = 'we', pady = 1)
		tkgrid(frameAggr, row = 2, column = 0, sticky = 'we', pady = 1)
		tkgrid(cb.stat.data, row = 3, column = 0, sticky = 'we', pady = 3)
		tkgrid(frameDicho, row = 4, column = 0, sticky = '', pady = 3)
		tkgrid(bt.calc.stat, row = 5, column = 0, sticky = 'we', pady = 3)

	#######################################################################################################

	#Tab4
	subfr4 <- bwTabScrollableFrame(cmd.tab4)

	##############################################

		frameStatTab <- ttklabelframe(subfr4, text = "Display Statistics Table", relief = 'groove')

		STATIONIDS <- ''
		stn.stat.tab <- tclVar()
		stateDispSTN <- if(GeneralParameters$stat.data == 'stn') 'normal' else 'disabled'

		bt.stat.disp <- ttkbutton(frameStatTab, text = "Display Table")
		bt.stat.prev <- ttkbutton(frameStatTab, text = "<<", state = stateDispSTN, width = 4)
		bt.stat.next <- ttkbutton(frameStatTab, text = ">>", state = stateDispSTN, width = 4)
		cb.stat.sel <- ttkcombobox(frameStatTab, values = STATIONIDS, textvariable = stn.stat.tab, width = largeur3, state = stateDispSTN,  justify = 'center')

		################
		.cdtData$EnvData$tab$validStat <- NULL

		tkconfigure(bt.stat.disp, command = function(){
			if(!is.null(.cdtData$EnvData$Statistics)){
				if(str_trim(tclvalue(stat.data)) == STATDATATYPE[1]){
					don <- .cdtData$EnvData$Statistics$ALL
					dat2disp <- data.frame(Name = rownames(don$statistics), Statistics = don$statistics, Description = don$description)
					titleTab <- 'All-Data Statistics'
				}
				if(str_trim(tclvalue(stat.data)) == STATDATATYPE[2]){
					don <- .cdtData$EnvData$Statistics$AVG
					dat2disp <- data.frame(Name = rownames(don$statistics), Statistics = don$statistics, Description = don$description)
					titleTab <- 'Spatial-Average Statistics'
				}
				if(str_trim(tclvalue(stat.data)) == STATDATATYPE[3]){
					don <- .cdtData$EnvData$Statistics$STN
					istn <- which(.cdtData$EnvData$opDATA$id == str_trim(tclvalue(stn.stat.tab)))
					dat2disp <- data.frame(Name = rownames(don$statistics), Statistics = don$statistics[, istn], Description = don$description)
					titleTab <- paste(tclvalue(stn.stat.tab), 'Statistics')
				}

				.cdtData$EnvData$tab$validStat <- tableNotebookTab_unik(dat2disp, .cdtData$EnvData$tab$validStat, titleTab, 12)
			}
		})

		tkconfigure(bt.stat.prev, command = function(){
			if(!is.null(.cdtData$EnvData$Statistics)){
				don <- .cdtData$EnvData$Statistics$STN
				istn <- which(.cdtData$EnvData$opDATA$id == str_trim(tclvalue(stn.stat.tab)))
				istn <- istn - 1
				if(istn < 1) istn <- length(.cdtData$EnvData$opDATA$id)
				tclvalue(stn.stat.tab) <- .cdtData$EnvData$opDATA$id[istn]

				dat2disp <- data.frame(Name = rownames(don$statistics), Statistics = don$statistics[, istn], Description = don$description)
				titleTab <- paste(tclvalue(stn.stat.tab), 'Statistics')

				.cdtData$EnvData$tab$validStat <- tableNotebookTab_unik(dat2disp, .cdtData$EnvData$tab$validStat, titleTab, 12)
			}
		})

		tkconfigure(bt.stat.next, command = function(){
			if(!is.null(.cdtData$EnvData$Statistics)){
				don <- .cdtData$EnvData$Statistics$STN
				istn <- which(.cdtData$EnvData$opDATA$id == str_trim(tclvalue(stn.stat.tab)))
				istn <- istn + 1
				if(istn > length(.cdtData$EnvData$opDATA$id)) istn <- 1
				tclvalue(stn.stat.tab) <- .cdtData$EnvData$opDATA$id[istn]

				dat2disp <- data.frame(Name = rownames(don$statistics), Statistics = don$statistics[, istn], Description = don$description)
				titleTab <- paste(tclvalue(stn.stat.tab), 'Statistics')

				.cdtData$EnvData$tab$validStat <- tableNotebookTab_unik(dat2disp, .cdtData$EnvData$tab$validStat, titleTab, 12)
			}
		})

		tkgrid(bt.stat.disp, row = 0, column = 0, sticky = 'we', rowspan = 1, columnspan = 5, padx = 1, pady = 1, ipadx = 1, ipady = 1)
		tkgrid(bt.stat.prev, row = 1, column = 0, sticky = 'e', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
		tkgrid(cb.stat.sel, row = 1, column = 1, sticky = 'we', rowspan = 1, columnspan = 3, padx = 1, pady = 1, ipadx = 1, ipady = 1)
		tkgrid(bt.stat.next, row = 1, column = 4, sticky = 'w', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)

		##############################################

		frameMap <- ttklabelframe(subfr4, text = "Statistics Maps", relief = 'groove')

		.cdtData$EnvData$statistics <- tclVar('Correlation')
		typeMapPLOT <- c("Points", "Pixels")
		.cdtData$EnvData$typeMap <- tclVar("Points")

		stateMaps <- if(GeneralParameters$stat.data == 'stn') 'normal' else 'disabled'

		.cdtData$EnvData$statMapOp <- list(presetCol = list(color = 'tim.colors', reverse = FALSE),
											userCol = list(custom = FALSE, color = NULL),
											userLvl = list(custom = FALSE, levels = NULL, equidist = FALSE),
											title = list(user = FALSE, title = ''),
											colkeyLab = list(user = FALSE, label = ''),
											scalebar = list(add = FALSE, pos = 'bottomleft'),
											pointSize = 0.7)

		cb.stats.maps <- ttkcombobox(frameMap, values = CHXSTATS, textvariable = .cdtData$EnvData$statistics, width = largeur2, state = stateMaps)
		bt.stats.maps <- ttkbutton(frameMap, text = .cdtEnv$tcl$lang$global[['button']][['3']], state = stateMaps)
		txt.plot.type <- tklabel(frameMap, text = "Plot Type", anchor = "e", justify = "right")
		cb.plot.type <- ttkcombobox(frameMap, values = typeMapPLOT, textvariable = .cdtData$EnvData$typeMap, width = 5, state = stateMaps)

		.cdtData$EnvData$tab$Maps <- NULL

		tkconfigure(bt.stats.maps, command = function(){
			if(!is.null(.cdtData$EnvData$Statistics)){
				.cdtData$EnvData$xlim.maps <- range(.cdtData$EnvData$opDATA$lon, na.rm = TRUE)
				.cdtData$EnvData$ylim.maps <- range(.cdtData$EnvData$opDATA$lat, na.rm = TRUE)
				.cdtData$EnvData$plot.maps$data.type <- "cdtstation"
				.cdtData$EnvData$plot.maps$lon <- .cdtData$EnvData$opDATA$lon
				.cdtData$EnvData$plot.maps$lat <- .cdtData$EnvData$opDATA$lat
				.cdtData$EnvData$plot.maps$id <- .cdtData$EnvData$opDATA$id

				Validation.DisplayStatMaps()
			}
		})

		tkgrid(cb.stats.maps, row = 0, column = 0, sticky = 'we', rowspan = 1, columnspan = 3, padx = 1, pady = 2, ipadx = 1, ipady = 1)
		tkgrid(bt.stats.maps, row = 0, column = 3, sticky = 'we', rowspan = 1, columnspan = 2, padx = 1, pady = 2, ipadx = 1, ipady = 1)
		tkgrid(txt.plot.type, row = 1, column = 0, sticky = 'e', rowspan = 1, columnspan = 2, padx = 1, pady = 2, ipadx = 1, ipady = 1)
		tkgrid(cb.plot.type, row = 1, column = 2, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 2, ipadx = 1, ipady = 1)

		##############################################

		frameGraph <- ttklabelframe(subfr4, text = "Graphs", relief = 'groove')

		TYPEGRAPH <- c("Scatter", "CDF", 'Lines')
		if(GeneralParameters$stat.data == 'all') TYPEGRAPH <- c("Scatter", "CDF")

		.cdtData$EnvData$type.graph <- tclVar("Scatter")
		STNIDGRAPH <- ""
		.cdtData$EnvData$stnIDGraph <- tclVar()
		stateStnID <- "disabled"

		cb.stats.graph <- ttkcombobox(frameGraph, values = TYPEGRAPH, textvariable = .cdtData$EnvData$type.graph, width = largeur2)
		bt.stats.graph <- ttkbutton(frameGraph, text = .cdtEnv$tcl$lang$global[['button']][['3']])
		cb.stn.graph <- ttkcombobox(frameGraph, values = STNIDGRAPH, textvariable = .cdtData$EnvData$stnIDGraph, width = largeur3, state = stateStnID, justify = 'center')
		bt.stn.graph.prev <- ttkbutton(frameGraph, text = "<<", state = stateStnID, width = 4)
		bt.stn.graph.next <- ttkbutton(frameGraph, text = ">>", state = stateStnID, width = 4)

		##############
		.cdtData$EnvData$tab$Graph <- NULL

		tkconfigure(bt.stats.graph, command = function(){
			if(!is.null(.cdtData$EnvData$Statistics)){
				imgContainer <- CDT.Display.Graph(Validation.plotGraph, .cdtData$EnvData$tab$Graph, 'Validation-Plot')
				.cdtData$EnvData$tab$Graph <- imageNotebookTab_unik(imgContainer, .cdtData$EnvData$tab$Graph)
			}
		})

		tkconfigure(bt.stn.graph.prev, command = function(){
			if(!is.null(.cdtData$EnvData$Statistics)){
				istn <- which(.cdtData$EnvData$opDATA$id == str_trim(tclvalue(.cdtData$EnvData$stnIDGraph)))
				istn <- istn - 1
				if(istn < 1) istn <- length(.cdtData$EnvData$opDATA$id)
				tclvalue(.cdtData$EnvData$stnIDGraph) <- .cdtData$EnvData$opDATA$id[istn]

				imgContainer <- CDT.Display.Graph(Validation.plotGraph, .cdtData$EnvData$tab$Graph, 'Validation-Plot')
				.cdtData$EnvData$tab$Graph <- imageNotebookTab_unik(imgContainer, .cdtData$EnvData$tab$Graph)
			}
		})

		tkconfigure(bt.stn.graph.next, command = function(){
			if(!is.null(.cdtData$EnvData$Statistics)){
				istn <- which(.cdtData$EnvData$opDATA$id == str_trim(tclvalue(.cdtData$EnvData$stnIDGraph)))
				istn <- istn + 1
				if(istn > length(.cdtData$EnvData$opDATA$id)) istn <- 1
				tclvalue(.cdtData$EnvData$stnIDGraph) <- .cdtData$EnvData$opDATA$id[istn]

				imgContainer <- CDT.Display.Graph(Validation.plotGraph, .cdtData$EnvData$tab$Graph, 'Validation-Plot')
				.cdtData$EnvData$tab$Graph <- imageNotebookTab_unik(imgContainer, .cdtData$EnvData$tab$Graph)
			}
		})

		tkgrid(cb.stats.graph, row = 0, column = 0, sticky = 'we', rowspan = 1, columnspan = 12, padx = 1, pady = 2, ipadx = 1, ipady = 1)
		tkgrid(bt.stats.graph, row = 0, column = 12, sticky = 'we', rowspan = 1, columnspan = 6, padx = 1, pady = 2, ipadx = 1, ipady = 1)
		tkgrid(bt.stn.graph.prev, row = 1, column = 0, sticky = 'we', rowspan = 1, columnspan = 3, padx = 1, pady = 2, ipadx = 1, ipady = 1)
		tkgrid(cb.stn.graph, row = 1, column = 3, sticky = 'we', rowspan = 1, columnspan = 12, padx = 1, pady = 2, ipadx = 1, ipady = 1)
		tkgrid(bt.stn.graph.next, row = 1, column = 15, sticky = 'we', rowspan = 1, columnspan = 3, padx = 1, pady = 2, ipadx = 1, ipady = 1)

		#############################
		tkgrid(frameStatTab, row = 0, column = 0, sticky = 'we')
		tkgrid(frameMap, row = 1, column = 0, sticky = 'we', pady = 3)
		tkgrid(frameGraph, row = 2, column = 0, sticky = 'we', pady = 1)

	#######################################################################################################

	#Tab5
	subfr5 <- bwTabScrollableFrame(cmd.tab5)

	##############################################

		frameSHP <- ttklabelframe(subfr5, text = "Boundaries", relief = 'groove')

		.cdtData$EnvData$add.shp <- tclVar(GeneralParameters$add.to.plot$add.shp)
		file.plotShp <- tclVar(GeneralParameters$add.to.plot$shp.file)

		stateSHP <- if(GeneralParameters$add.to.plot$add.shp) "normal" else "disabled"

		chk.addshp <- tkcheckbutton(frameSHP, variable = .cdtData$EnvData$add.shp, text = "Add boundaries to Map", anchor = 'w', justify = 'left')
		cb.addshp <- ttkcombobox(frameSHP, values = unlist(listOpenFiles), textvariable = file.plotShp, width = largeur, state = stateSHP)
		bt.addshp <- tkbutton(frameSHP, text = "...", state = stateSHP)

		########
		tkconfigure(bt.addshp, command = function(){
			shp.opfiles <- getOpenShp(.cdtEnv$tcl$main$win)
			if(!is.null(shp.opfiles)){
				update.OpenFiles('shp', shp.opfiles)
				tclvalue(file.plotShp) <- shp.opfiles[[1]]
				listOpenFiles[[length(listOpenFiles) + 1]] <<- shp.opfiles[[1]]

				lapply(list(cb.stnfl, cb.shpF, cb.adddem, cb.addshp), tkconfigure, values = unlist(listOpenFiles))

				shpofile <- getShpOpenData(file.plotShp)
				if(is.null(shpofile))
					.cdtData$EnvData$shp <- NULL
				else
					.cdtData$EnvData$shp <- getBoundaries(shpofile[[2]])
			}
		})

		tkgrid(chk.addshp, row = 0, column = 0, sticky = 'we', rowspan = 1, columnspan = 5, padx = 1, pady = 1, ipadx = 1, ipady = 1)
		tkgrid(cb.addshp, row = 1, column = 0, sticky = 'we', rowspan = 1, columnspan = 4, padx = 1, pady = 1, ipadx = 1, ipady = 1)
		tkgrid(bt.addshp, row = 1, column = 4, sticky = 'w', rowspan = 1, columnspan = 1, padx = 0, pady = 1, ipadx = 1, ipady = 1)

		#################
		tkbind(cb.addshp, "<<ComboboxSelected>>", function(){
			shpofile <- getShpOpenData(file.plotShp)
			if(is.null(shpofile))
				.cdtData$EnvData$shp <- NULL
			else
				.cdtData$EnvData$shp <- getBoundaries(shpofile[[2]])
		})

		tkbind(chk.addshp, "<Button-1>", function(){
			stateSHP <- if(tclvalue(.cdtData$EnvData$add.shp) == "1") "disabled" else "normal"
			tkconfigure(cb.addshp, state = stateSHP)
			tkconfigure(bt.addshp, state = stateSHP)
		})

		##############################################

		frameDEM <- ttklabelframe(subfr5, text = "DEM", relief = 'groove')

		.cdtData$EnvData$add.dem <- tclVar(GeneralParameters$add.to.plot$add.dem)
		file.grddem <- tclVar(GeneralParameters$add.to.plot$dem.file)

		stateDEM <- if(GeneralParameters$add.to.plot$add.dem) "normal" else "disabled"

		chk.adddem <- tkcheckbutton(frameDEM, variable = .cdtData$EnvData$add.dem, text = "Add DEM  to the Map", anchor = 'w', justify = 'left')
		cb.adddem <- ttkcombobox(frameDEM, values = unlist(listOpenFiles), textvariable = file.grddem, width = largeur, state = stateDEM)
		bt.adddem <- tkbutton(frameDEM, text = "...", state = stateDEM)

		tkconfigure(bt.adddem, command = function(){
			nc.opfiles <- getOpenNetcdf(.cdtEnv$tcl$main$win, initialdir = getwd())
			if(!is.null(nc.opfiles)){
				update.OpenFiles('netcdf', nc.opfiles)
				listOpenFiles[[length(listOpenFiles) + 1]] <<- nc.opfiles[[1]]
				tclvalue(file.grddem) <- nc.opfiles[[1]]

				lapply(list(cb.stnfl, cb.shpF, cb.adddem, cb.addshp), tkconfigure, values = unlist(listOpenFiles))

				demData <- getNCDFSampleData(str_trim(tclvalue(file.grddem)))
				if(!is.null(demData)){
					jfile <- getIndex.AllOpenFiles(str_trim(tclvalue(file.grddem)))
					demData <- .cdtData$OpenFiles$Data[[jfile]][[2]]
					.cdtData$EnvData$dem$elv <- demData[c('x', 'y', 'z')]

					demr <- raster(demData[c('x', 'y', 'z')])
					slope <- terrain(demr, opt = 'slope')
					aspect <- terrain(demr, opt = 'aspect')
					hill <- hillShade(slope, aspect, angle = 40, direction = 270)
					hill <- t(as.matrix(hill))
					hill <- hill[, rev(seq(ncol(hill)))]
					.cdtData$EnvData$dem$hill <- list(x = demData$x, y = demData$y, z = hill)

					rm(demData, demr, slope, aspect, hill)
				}else .cdtData$EnvData$dem <- NULL
			}
		})

		tkgrid(chk.adddem, row = 0, column = 0, sticky = 'we', rowspan = 1, columnspan = 5, padx = 1, pady = 1, ipadx = 1, ipady = 1)
		tkgrid(cb.adddem, row = 1, column = 0, sticky = 'we', rowspan = 1, columnspan = 4, padx = 1, pady = 1, ipadx = 1, ipady = 1)
		tkgrid(bt.adddem, row = 1, column = 4, sticky = 'w', rowspan = 1, columnspan = 1, padx = 0, pady = 1, ipadx = 1, ipady = 1)

		#################
		tkbind(cb.adddem, "<<ComboboxSelected>>", function(){
			demData <- getNCDFSampleData(str_trim(tclvalue(file.grddem)))
			if(!is.null(demData)){
				jfile <- getIndex.AllOpenFiles(str_trim(tclvalue(file.grddem)))
				demData <- .cdtData$OpenFiles$Data[[jfile]][[2]]
				.cdtData$EnvData$dem$elv <- demData[c('x', 'y', 'z')]

				demr <- raster(demData[c('x', 'y', 'z')])
				slope <- terrain(demr, opt = 'slope')
				aspect <- terrain(demr, opt = 'aspect')
				hill <- hillShade(slope, aspect, angle = 40, direction = 270)
				hill <- t(as.matrix(hill))
				hill <- hill[, rev(seq(ncol(hill)))]
				.cdtData$EnvData$dem$hill <- list(x = demData$x, y = demData$y, z = hill)

				rm(demData, demr, slope, aspect, hill)
			}else .cdtData$EnvData$dem <- NULL
		})

		tkbind(chk.adddem, "<Button-1>", function(){
			stateDEM <- if(tclvalue(.cdtData$EnvData$add.dem) == "1") "disabled" else "normal"
			tkconfigure(cb.adddem, state = stateDEM)
			tkconfigure(bt.adddem, state = stateDEM)
		})

		#############################
		tkgrid(frameSHP, row = 3, column = 0, sticky = 'we', pady = 1)
		tkgrid(frameDEM, row = 4, column = 0, sticky = 'we', pady = 1)

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
