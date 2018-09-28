
ExtractDataPanelCmd <- function(){
	listOpenFiles <- openFile_ttkcomboList()
	if(WindowsOS()){
		largeur1 <- .cdtEnv$tcl$fun$w.widgets(28)
		largeur2 <- .cdtEnv$tcl$fun$w.widgets(26)
		largeur3 <- .cdtEnv$tcl$fun$w.widgets(14)
		largeur4 <- .cdtEnv$tcl$fun$w.widgets(24)
		largeur5 <- .cdtEnv$tcl$fun$w.widgets(28)
	}else{
		largeur1 <- .cdtEnv$tcl$fun$w.widgets(22)
		largeur2 <- .cdtEnv$tcl$fun$w.widgets(23)
		largeur3 <- .cdtEnv$tcl$fun$w.widgets(14)
		largeur4 <- .cdtEnv$tcl$fun$w.widgets(19)
		largeur5 <- .cdtEnv$tcl$fun$w.widgets(23)
	}

	MOIS <- format(ISOdate(2014, 1:12, 1), "%B")
	GeneralParameters <- fromJSON(file.path(.cdtDir$dirLocal, 'init_params', 'Extract_time_series.json'))
	GeneralParameters$Geom <- list(minlon = '', maxlon = '', minlat = '', maxlat = '',
								padlon = '', padlat = '', namePoly = '', multiObj = NULL)
	.cdtData$EnvData$multiptspoly <- NULL

	# xml.dlg <- file.path(.cdtDir$dirLocal, "languages", "cdtExtractData_leftCmd.xml")
	# lang.dlg <- cdtLanguageParse(xml.dlg, .cdtData$Config$lang.iso)
	# .cdtData$EnvData$message <- lang.dlg[['message']]

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
	cmd.tab2 <- bwAddTab(tknote.cmd, text = "Time series")
	cmd.tab3 <- bwAddTab(tknote.cmd, text = "Location")
	cmd.tab4 <- bwAddTab(tknote.cmd, text = "Output")

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

		##########################################

		frameTimeS <- ttklabelframe(subfr1, text = "Time step of input data", relief = 'groove')

		timeSteps <- tclVar()
		CbperiodVAL <- .cdtEnv$tcl$lang$global[['combobox']][['1']][1:5]
		periodVAL <- c('hourly', 'daily', 'pentad', 'dekadal', 'monthly')
		tclvalue(timeSteps) <- CbperiodVAL[periodVAL %in% GeneralParameters$in.series]

		cb.fperiod <- ttkcombobox(frameTimeS, values = CbperiodVAL, textvariable = timeSteps, width = largeur1)

		tkgrid(cb.fperiod, row = 0, column = 0, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)

		infobulle(cb.fperiod, 'Select the time step of the data')
		status.bar.display(cb.fperiod, 'Select the time step of the data')

		#################
		tkbind(cb.fperiod, "<<ComboboxSelected>>", function(){
			if(str_trim(tclvalue(timeSteps)) == CbperiodVAL[1])
			{
				stateHR <<- 'normal'
				stateDR <<- 'normal'
				OUTSeries <- CbOUTSeries
				tclvalue(DayDek.txtVar) <- 'Day'
			}
			if(str_trim(tclvalue(timeSteps)) == CbperiodVAL[2])
			{
				stateHR <<- 'disabled'
				stateDR <<- 'normal'
				OUTSeries <- CbOUTSeries[-1]
				tclvalue(DayDek.txtVar) <- 'Day'
				tclvalue(out.series) <- if(str_trim(tclvalue(out.series)) == CbOUTSeries[1]) CbOUTSeries[2] else tclvalue(out.series)
			}
			if(str_trim(tclvalue(timeSteps)) == CbperiodVAL[3])
			{
				stateHR <<- 'disabled'
				stateDR <<- 'normal'
				OUTSeries <- CbOUTSeries[-(1:2)]
				tclvalue(DayDek.txtVar) <- 'Pentad'
				tclvalue(out.series) <- if(str_trim(tclvalue(out.series)) %in% CbOUTSeries[1:2]) CbOUTSeries[3] else tclvalue(out.series)
			}
			if(str_trim(tclvalue(timeSteps)) == CbperiodVAL[4])
			{
				stateHR <<- 'disabled'
				stateDR <<- 'normal'
				OUTSeries <- CbOUTSeries[-(1:3)]
				tclvalue(DayDek.txtVar) <- 'Dekad'
				tclvalue(out.series) <- if(str_trim(tclvalue(out.series)) %in% CbOUTSeries[1:3]) CbOUTSeries[4] else tclvalue(out.series)
			}
			if(str_trim(tclvalue(timeSteps)) == CbperiodVAL[5])
			{
				stateHR <<- 'disabled'
				stateDR <<- 'disabled'
				OUTSeries <- CbOUTSeries[-(1:4)]
				tclvalue(out.series) <- if(str_trim(tclvalue(out.series)) %in% CbOUTSeries[1:4]) CbOUTSeries[5] else tclvalue(out.series)
			}

			tkconfigure(cb.outTS, values = OUTSeries)

			####
			AGGREGFUN <- c("mean", "sum", "count")
			if(
				(str_trim(tclvalue(out.series)) == CbOUTSeries[1] & str_trim(tclvalue(timeSteps)) == CbperiodVAL[1]) |
				(str_trim(tclvalue(out.series)) == CbOUTSeries[2] & str_trim(tclvalue(timeSteps)) == CbperiodVAL[2]) |
				(str_trim(tclvalue(out.series)) == CbOUTSeries[3] & str_trim(tclvalue(timeSteps)) == CbperiodVAL[3]) |
				(str_trim(tclvalue(out.series)) == CbOUTSeries[4] & str_trim(tclvalue(timeSteps)) == CbperiodVAL[4]) |
				(str_trim(tclvalue(out.series)) == CbOUTSeries[5] & str_trim(tclvalue(timeSteps)) == CbperiodVAL[5])
				)
			{
				stateo0a <- "disabled"
				stateo0b <- "disabled"
				stateo1 <- "disabled"
				stateo2 <- "disabled"
			}else{
				if(!str_trim(tclvalue(timeSteps)) %in% CbperiodVAL[1:2])
				{
					AGGREGFUN <- AGGREGFUN[-3]
					tclvalue(aggr.fun) <- if(tclvalue(aggr.fun) == "count") "sum" else tclvalue(aggr.fun)
				}
				stateo0a <- "readonly"
				stateo0b <- "normal"
				stateo1 <- if(tclvalue(aggr.fun) == "count") "readonly" else "disabled"
				stateo2 <- if(tclvalue(aggr.fun) == "count") "normal" else "disabled"
			}

			tkconfigure(cb.aggfun, values = AGGREGFUN, state = stateo0a)
			tkconfigure(en.minfrac, state = stateo0b)
			tkconfigure(cb.opfun, state = stateo1)
			tkconfigure(en.opthres, state = stateo2)

			####
			if(str_trim(tclvalue(timeSteps)) == CbperiodVAL[1])
			{
				OUTTypeSeries <- c(CbOUTTypeSeries[1], '')
				tclvalue(type.series) <- CbOUTTypeSeries[1]
			}else{
				OUTTypeSeries <- CbOUTTypeSeries
				tclvalue(type.series) <- if(str_trim(tclvalue(type.series)) == "") CbOUTTypeSeries[1] else tclvalue(type.series)
			}
			tkconfigure(cb.typeseries, values = OUTTypeSeries)

			stateClim0 <- if(str_trim(tclvalue(type.series)) %in% c(CbOUTTypeSeries[1], '')) 'disabled' else 'normal'
			tkconfigure(bt.BasePeriod, state = stateClim0)

			stateClim2 <- if(str_trim(tclvalue(timeSteps)) != CbperiodVAL[1])
							{
								if(str_trim(tclvalue(timeSteps)) == CbperiodVAL[2] &
									str_trim(tclvalue(type.series)) != CbOUTTypeSeries[1]) "normal" else "disable"
							}else "disabled"
			tkconfigure(en.winsize, state = stateClim2)
		})

		#############################
		frameNC <- tkframe(subfr1, relief = 'groove', borderwidth = 2)

		ncDIR <- tclVar(GeneralParameters$ncdf.file$dir)
		ncSample <- tclVar(GeneralParameters$ncdf.file$sample)
		ncFormat <- tclVar(GeneralParameters$ncdf.file$format)

		txt.ncdr <- tklabel(frameNC, text = 'Directory containing the NetCDF data', anchor = 'w', justify = 'left')
		en.ncdr <- tkentry(frameNC, textvariable = ncDIR, width = largeur2)
		bt.ncdr <- tkbutton(frameNC, text = "...")
		txt.ncfl <- tklabel(frameNC, text = "NetCDF data sample file", anchor = 'w', justify = 'left')
		cb.ncfl <- ttkcombobox(frameNC, values = unlist(listOpenFiles), textvariable = ncSample, width = largeur1)
		bt.ncfl <- tkbutton(frameNC, text = "...")
		txt.ncff <- tklabel(frameNC, text = "Filename format", anchor = 'e', justify = 'right')
		en.ncff <- tkentry(frameNC, textvariable = ncFormat, width = largeur3)

		#################
		tkconfigure(bt.ncdr, command = function(){
			dir4cdf <- tk_choose.dir(getwd(), "")
			tclvalue(ncDIR) <- if(is.na(dir4cdf)) "" else dir4cdf
		})

		tkconfigure(bt.ncfl, command = function(){
			initialdir <- if(file.exists(tclvalue(ncDIR))) tclvalue(ncDIR) else getwd()
			nc.opfiles <- getOpenNetcdf(.cdtEnv$tcl$main$win, initialdir = initialdir)
			if(!is.null(nc.opfiles)){
				update.OpenFiles('netcdf', nc.opfiles)
				listOpenFiles[[length(listOpenFiles) + 1]] <<- nc.opfiles[[1]]
				tclvalue(ncSample) <- nc.opfiles[[1]]
				lapply(list(cb.ncfl, cb.shpF), tkconfigure, values = unlist(listOpenFiles))
			}
		})

		tkgrid(txt.ncdr, row = 0, column = 0, sticky = 'we', rowspan = 1, columnspan = 6, padx = 1, pady = 0, ipadx = 1, ipady = 1)
		tkgrid(en.ncdr, row = 1, column = 0, sticky = 'we', rowspan = 1, columnspan = 5, padx = 0, pady = 0, ipadx = 1, ipady = 1)
		tkgrid(bt.ncdr, row = 1, column = 5, sticky = 'w', rowspan = 1, columnspan = 1, padx = 0, pady = 0, ipadx = 1, ipady = 1)
		tkgrid(txt.ncfl, row = 2, column = 0, sticky = 'we', rowspan = 1, columnspan = 6, padx = 1, pady = 0, ipadx = 1, ipady = 1)
		tkgrid(cb.ncfl, row = 3, column = 0, sticky = 'we', rowspan = 1, columnspan = 5, padx = 0, pady = 0, ipadx = 1, ipady = 1)
		tkgrid(bt.ncfl, row = 3, column = 5, sticky = 'w', rowspan = 1, columnspan = 1, padx = 0, pady = 0, ipadx = 1, ipady = 1)
		tkgrid(txt.ncff, row = 4, column = 0, sticky = 'e', rowspan = 1, columnspan = 2, padx = 1, pady = 1, ipadx = 1, ipady = 1)
		tkgrid(en.ncff, row = 4, column = 2, sticky = 'we', rowspan = 1, columnspan = 4, padx = 1, pady = 1, ipadx = 1, ipady = 1)

		infobulle(en.ncdr, 'Enter the full path to directory containing the NetCDF data')
		status.bar.display(en.ncdr, 'Enter the full path to directory containing the NetCDF data')
		status.bar.display(cb.ncfl, 'File containing a sample of the data in netcdf')
		infobulle(bt.ncfl, 'Browse file if not listed')
		infobulle(en.ncff, 'Enter the filename format of netcdf data, example: rr_mrg_19830125_CLM.nc')
		status.bar.display(en.ncff, 'Enter the filename format of netcdf data, example: rr_mrg_19830125_CLM.nc')

		#############################

		frameSHP <- tkframe(subfr1, relief = 'groove', borderwidth = 2)

		shpFile <- tclVar(GeneralParameters$shp.file$shp)
		shpAttr <- tclVar(GeneralParameters$shp.file$attr)

		txt.shpF <- tklabel(frameSHP, text = "Shapefile for Administrative Boundaries", anchor = 'w', justify = 'left')
		cb.shpF <- ttkcombobox(frameSHP, values = unlist(listOpenFiles), textvariable = shpFile, width = largeur1)
		bt.shpF <- tkbutton(frameSHP, text = "...")
		txt.shpAttr <- tklabel(frameSHP, text = "Attribute field to be used and displayed", anchor = 'w', justify = 'left')
		.cdtData$EnvData$cb.shpAttr <- ttkcombobox(frameSHP, values = '', textvariable = shpAttr)

		#################
		tkconfigure(bt.shpF, command = function(){
			shp.opfiles <- getOpenShp(.cdtEnv$tcl$main$win)
			if(!is.null(shp.opfiles)){
				update.OpenFiles('shp', shp.opfiles)
				tclvalue(shpFile) <- shp.opfiles[[1]]
				listOpenFiles[[length(listOpenFiles) + 1]] <<- shp.opfiles[[1]]
				lapply(list(cb.ncfl, cb.shpF), tkconfigure, values = unlist(listOpenFiles))

				shpf <- getShpOpenData(shpFile)
				dat <- shpf[[2]]@data
				AttrTable <- names(dat)
				tkconfigure(.cdtData$EnvData$cb.shpAttr, values = AttrTable)
				tclvalue(shpAttr) <- AttrTable[1]

				adminN <- as.character(dat[, as.numeric(tclvalue(tcl(.cdtData$EnvData$cb.shpAttr, 'current'))) + 1])
				cbAttrTable <- levels(as.factor(adminN))
				tkconfigure(cb.Polygon, values = cbAttrTable)
				tclvalue(.cdtData$EnvData$namePoly) <- cbAttrTable[1]
			}
		})

		tkgrid(txt.shpF, row = 0, column = 0, sticky = 'we', rowspan = 1, columnspan = 6, padx = 1, pady = 0, ipadx = 1, ipady = 1)
		tkgrid(cb.shpF, row = 1, column = 0, sticky = 'we', rowspan = 1, columnspan = 5, padx = 0, pady = 0, ipadx = 1, ipady = 1)
		tkgrid(bt.shpF, row = 1, column = 5, sticky = 'w', rowspan = 1, columnspan = 1, padx = 0, pady = 0, ipadx = 1, ipady = 1)
		tkgrid(txt.shpAttr, row = 2, column = 0, sticky = 'we', rowspan = 1, columnspan = 6, padx = 1, pady = 1, ipadx = 1, ipady = 1)
		tkgrid(.cdtData$EnvData$cb.shpAttr, row = 3, column = 0, sticky = 'we', rowspan = 1, columnspan = 5, padx = 1, pady = 1, ipadx = 1, ipady = 1)

		#######################
		tkbind(cb.shpF, "<<ComboboxSelected>>", function(){
			shpf <- getShpOpenData(shpFile)
			dat <- shpf[[2]]@data
			AttrTable <- names(dat)
			tkconfigure(.cdtData$EnvData$cb.shpAttr, values = AttrTable)
			tclvalue(shpAttr) <- AttrTable[1]

			adminN <- as.character(dat[, as.numeric(tclvalue(tcl(.cdtData$EnvData$cb.shpAttr, 'current'))) + 1])
			cbAttrTable <- levels(as.factor(adminN))
			tkconfigure(cb.Polygon, values = cbAttrTable)
			tclvalue(.cdtData$EnvData$namePoly) <- cbAttrTable[1]
		})

		tkbind(.cdtData$EnvData$cb.shpAttr, "<<ComboboxSelected>>", function(){
			shpf <- getShpOpenData(shpFile)
			dat <- shpf[[2]]@data
			adminN <- as.character(dat[, as.numeric(tclvalue(tcl(.cdtData$EnvData$cb.shpAttr, 'current'))) + 1])
			cbAttrTable <- levels(as.factor(adminN))
			tkconfigure(cb.Polygon, values = cbAttrTable)
			tclvalue(.cdtData$EnvData$namePoly) <- cbAttrTable[1]
		})

		#############################

		openAttrSHP <- ttkbutton(subfr1, text = "Open Attribute Table")
		displayMapSHP <- ttkbutton(subfr1, text = "Display Map", width = 15)

		#################

		.cdtData$EnvData$tab$TableAttr <- NULL

		tkconfigure(openAttrSHP, command = function(){
			shpf <- getShpOpenData(shpFile)
			if(!is.null(shpf))
				.cdtData$EnvData$tab$TableAttr <- tableNotebookTab_unik(shpf[[2]]@data, .cdtData$EnvData$tab$TableAttr, shpf[[1]], 10)
		})

		#################

		.cdtData$EnvData$tab$MapSelect <- NULL

		tkconfigure(displayMapSHP, command = function(){
			shpofile <- getShpOpenData(shpFile)
			if(!is.null(shpofile))
			{
				shpf <- shpofile[[2]]
				.cdtData$EnvData$shpf <- shpf
				.cdtData$EnvData$ocrds <- getBoundaries(shpf)

				lo1 <- round(bbox(shpf)[1, 1], 4)
				lo2 <- round(bbox(shpf)[1, 2], 4)
				la1 <- round(bbox(shpf)[2, 1], 4)
				la2 <- round(bbox(shpf)[2, 2], 4)
				ZoomXYval0 <<- c(lo1, lo2, la1, la2)

				tclvalue(.cdtData$EnvData$zoom$xx1) <- lo1
				tclvalue(.cdtData$EnvData$zoom$xx2) <- lo2
				tclvalue(.cdtData$EnvData$zoom$yy1) <- la1
				tclvalue(.cdtData$EnvData$zoom$yy2) <- la2

				.cdtData$EnvData$ZoomXYval <- ZoomXYval0

				imgContainer <- displayMap4Extraction(.cdtData$EnvData$tab$MapSelect)
				.cdtData$EnvData$tab$MapSelect <- imageNotebookTab_unik(imgContainer, .cdtData$EnvData$tab$MapSelect)
			}
			else
				Insert.Messages.Out('Provide the ESRI shapefile of the administrative boundaries', format = TRUE)
		})

		#############################
		tkgrid(frameTimeS, row = 0, column = 0, columnspan = 2, sticky = '', pady = 1)
		tkgrid(frameNC, row = 1, column = 0, columnspan = 2, sticky = 'we', pady = 3)
		tkgrid(frameSHP, row = 2, column = 0, columnspan = 2, sticky = 'we', pady = 3)
		tkgrid(openAttrSHP, row = 3, column = 0, columnspan = 1, sticky = 'we', pady = 3)
		tkgrid(displayMapSHP, row = 3, column = 1, columnspan = 1, sticky = 'we', pady = 3)

	#######################################################################################################

	#Tab2
	subfr2 <- bwTabScrollableFrame(cmd.tab2)

	##########################################

		frameDate <- ttklabelframe(subfr2, text = "Date Range", relief = 'groove')

		txtdek <- switch(GeneralParameters$in.series, 'dekadal' = 'Dekad', 'pentad' = 'Pentad', 'Day')
		DayDek.txtVar <- tclVar(txtdek)

		mon1 <- as.numeric(str_trim(GeneralParameters$date.range$start.month))
		mon2 <- as.numeric(str_trim(GeneralParameters$date.range$end.month))

		startMonth <- tclVar(MOIS[mon1])
		endMonth <- tclVar(MOIS[mon2])

		if(GeneralParameters$in.series == 'daily'){
			stateHR <- 'disabled'
			stateDR <- "normal"
		}else if(GeneralParameters$in.series == 'pentad'){
			stateHR <- 'disabled'
			stateDR <- "normal"
			if(as.numeric(str_trim(GeneralParameters$date.range$start.day)) > 6)
				GeneralParameters$date.range$start.day <- 6
			if(as.numeric(str_trim(GeneralParameters$date.range$end.day)) > 6)
				GeneralParameters$date.range$end.day <- 6
		}else if(GeneralParameters$in.series == 'dekadal'){
			stateHR <- 'disabled'
			stateDR <- "normal"
			if(as.numeric(str_trim(GeneralParameters$date.range$start.day)) > 3)
				GeneralParameters$date.range$start.day <- 3
			if(as.numeric(str_trim(GeneralParameters$date.range$end.day)) > 3)
				GeneralParameters$date.range$end.day <- 3
		}else if(GeneralParameters$in.series == 'monthly'){
			stateHR <- 'disabled'
			stateDR <- "disabled"
		}else{
			stateHR <- "normal"
			stateDR <- "normal"
		}

		if(GeneralParameters$out.series$out.series %in% c('seasonal3', 'seasonal6', 'annual'))
		{
			stateMon <- "disabled"
			tclvalue(startMonth) <- MOIS[as.numeric(str_trim(GeneralParameters$out.series$start.seas))]
			tclvalue(endMonth) <- MOIS[as.numeric(str_trim(GeneralParameters$out.series$end.seas))]
		}else stateMon <- "normal"

		bt.Date.Range <- ttkbutton(frameDate, text = "Set Date Range")
		txt.month <- tklabel(frameDate, text = "Months to extract", anchor = 'w', justify = 'left')
		fr.Month <- tkframe(frameDate)

		txt.startMonth <- tklabel(fr.Month, text = "From", anchor = 'e', justify = 'right')
		cb.startMonth <- ttkcombobox(fr.Month, values = MOIS, textvariable = startMonth, width = 10, state = stateMon)
		txt.endMonth <- tklabel(fr.Month, text = "to")
		cb.endMonth <- ttkcombobox(fr.Month, values = MOIS, textvariable = endMonth, width = 10, state = stateMon)

		tkgrid(txt.startMonth, row = 0, column = 0, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
		tkgrid(cb.startMonth, row = 0, column = 1, sticky = 'we', rowspan = 1, columnspan = 3, padx = 1, pady = 1, ipadx = 1, ipady = 1)
		tkgrid(txt.endMonth, row = 0, column = 4, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
		tkgrid(cb.endMonth, row = 0, column = 5, sticky = 'we', rowspan = 1, columnspan = 3, padx = 1, pady = 1, ipadx = 1, ipady = 1)

		tkconfigure(bt.Date.Range, command = function(){
			Params <- GeneralParameters[["date.range"]]
			Params <- getInfoDateRange(.cdtEnv$tcl$main$win, Params,
										daypendek.lab = tclvalue(DayDek.txtVar),
										state.dek = stateDR,
										state.hour = stateHR)
			GeneralParameters$date.range$start.year <<- Params$start.year
			GeneralParameters$date.range$start.mon <<- Params$start.mon
			GeneralParameters$date.range$start.dek <<- Params$start.day
			GeneralParameters$date.range$start.hour <<- Params$start.hour
			GeneralParameters$date.range$end.year <<- Params$end.year
			GeneralParameters$date.range$end.mon <<- Params$end.mon
			GeneralParameters$date.range$end.dek <<- Params$end.day
			GeneralParameters$date.range$end.hour <<- Params$end.hour
		})

		tkgrid(bt.Date.Range, row = 0, column = 0, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
		tkgrid(txt.month, row = 1, column = 0, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
		tkgrid(fr.Month, row = 2, column = 0, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)

		infobulle(bt.Date.Range, 'Start and end date to be extracted')
		status.bar.display(bt.Date.Range, 'Start and end date to be extracted')

		#############################

		frameTSOut <- ttklabelframe(subfr2, text = "Output time step", relief = 'groove')

		CbOUTSeries <- c('Hourly', 'Daily', 'Pentad', 'Dekadal', 'Monthly', '3-Months', '6-Months', 'Annual')
		OUTseriesV <- c('hourly', 'daily', 'pentad', 'dekadal', 'monthly', 'seasonal3', 'seasonal6', 'annual')

		if(GeneralParameters$in.series == 'hourly') idx.out <- 1:8
		if(GeneralParameters$in.series == 'daily') idx.out <- -1
		if(GeneralParameters$in.series == 'pentad') idx.out <- -(1:2)
		if(GeneralParameters$in.series == 'dekadal') idx.out <- -(1:3)
		if(GeneralParameters$in.series == 'monthly') idx.out <- -(1:4)

		OUTSeries <- CbOUTSeries[idx.out]

		out.series <- tclVar()
		tclvalue(out.series) <- CbOUTSeries[OUTseriesV %in% GeneralParameters$out.series$out.series]

		mon.s <- as.numeric(str_trim(GeneralParameters$out.series$start.seas))
		start.seas <- tclVar(MOIS[mon.s])

		if(GeneralParameters$out.series$out.series %in% OUTseriesV[6:8])
		{
			if(GeneralParameters$out.series$out.series == OUTseriesV[6]) lenSeas <- 2
			if(GeneralParameters$out.series$out.series == OUTseriesV[7]) lenSeas <- 5
			if(GeneralParameters$out.series$out.series == OUTseriesV[8]) lenSeas <- 11
			mon.e <- (mon.s + lenSeas) %% 12
			mon.e[mon.e == 0] <- 12
		}
		else
			mon.e <- as.numeric(str_trim(GeneralParameters$out.series$end.seas))

		end.seas <- tclVar(MOIS[mon.e])

		stateTSOut <- if(GeneralParameters$in.series %in% OUTseriesV[1:5]) "disabled" else "normal"

		txt.outTS <- tklabel(frameTSOut, text = "Time step", anchor = 'e', justify = 'right')
		cb.outTS <- ttkcombobox(frameTSOut, values = OUTSeries, textvariable = out.series, width = 10)
		txt.startSeas <- tklabel(frameTSOut, text = 'Start month', anchor = 'e', justify = 'right')
		cb.startSeas <- ttkcombobox(frameTSOut, values = MOIS, textvariable = start.seas, width = 10, state = stateTSOut)
		cb.endSeas <- ttkcombobox(frameTSOut, values = MOIS, textvariable = end.seas, width = 10, state = "disabled")

		tkgrid(txt.outTS, row = 0, column = 0, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
		tkgrid(cb.outTS, row = 0, column = 1, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
		tkgrid(txt.startSeas, row = 1, column = 0, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
		tkgrid(cb.startSeas, row = 1, column = 1, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
		tkgrid(cb.endSeas, row = 1, column = 2, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)

		infobulle(cb.outTS, "Select the time step of the output time series")
		status.bar.display(cb.outTS, "Select the time step of the output time series")
		infobulle(cb.startSeas, 'Select the start month of the season or year')
		status.bar.display(cb.startSeas, 'Select the start month of the season or year')

		#################
		tkbind(cb.outTS, "<<ComboboxSelected>>", function(){
			if(str_trim(tclvalue(out.series)) %in% CbOUTSeries[1:5])
			{
				stateTSOut <- "disabled"
				stateMon <- "normal"

				startMonth.v <- MOIS[1]
				endMonth.v <- MOIS[12]
			}else{
				stateTSOut <- "normal"
				stateMon <- "disabled"

				if(str_trim(tclvalue(out.series)) == CbOUTSeries[6]) len <- 2
				if(str_trim(tclvalue(out.series)) == CbOUTSeries[7]) len <- 5
				if(str_trim(tclvalue(out.series)) == CbOUTSeries[8]) len <- 11
				mon1 <- which(MOIS %in% str_trim(tclvalue(start.seas)))
				mon1 <- (mon1 + len) %% 12
				mon1[mon1 == 0] <- 12
				tclvalue(end.seas) <- MOIS[mon1]

				startMonth.v <- tclvalue(start.seas)
				endMonth.v <- tclvalue(end.seas)
			}

			tkconfigure(cb.startSeas, state = stateTSOut)
			tkconfigure(cb.startMonth, state = stateMon)
			tkconfigure(cb.endMonth, state = stateMon)
			tclvalue(startMonth) <- startMonth.v
			tclvalue(endMonth) <- endMonth.v

			######
			AGGREGFUN <- c("mean", "sum", "count")
			if(
				(str_trim(tclvalue(out.series)) == CbOUTSeries[1] & str_trim(tclvalue(timeSteps)) == CbperiodVAL[1]) |
				(str_trim(tclvalue(out.series)) == CbOUTSeries[2] & str_trim(tclvalue(timeSteps)) == CbperiodVAL[2]) |
				(str_trim(tclvalue(out.series)) == CbOUTSeries[3] & str_trim(tclvalue(timeSteps)) == CbperiodVAL[3]) |
				(str_trim(tclvalue(out.series)) == CbOUTSeries[4] & str_trim(tclvalue(timeSteps)) == CbperiodVAL[4]) |
				(str_trim(tclvalue(out.series)) == CbOUTSeries[5] & str_trim(tclvalue(timeSteps)) == CbperiodVAL[5])
				)
			{
				stateo0a <- "disabled"
				stateo0b <- "disabled"
				stateo1 <- "disabled"
				stateo2 <- "disabled"
			}else{
				if(!str_trim(tclvalue(timeSteps)) %in% CbperiodVAL[1:2])
				{
					AGGREGFUN <- AGGREGFUN[-3]
					tclvalue(aggr.fun) <- if(tclvalue(aggr.fun) == "count") "sum" else tclvalue(aggr.fun)
				}
				stateo0a <- "readonly"
				stateo0b <- "normal"
				stateo1 <- if(tclvalue(aggr.fun) == "count") "readonly" else "disabled"
				stateo2 <- if(tclvalue(aggr.fun) == "count") "normal" else "disabled"
			}

			tkconfigure(cb.aggfun, values = AGGREGFUN, state = stateo0a)
			tkconfigure(en.minfrac, state = stateo0b)
			tkconfigure(cb.opfun, state = stateo1)
			tkconfigure(en.opthres, state = stateo2)
		})

		tkbind(cb.startSeas, "<<ComboboxSelected>>", function(){
			if(str_trim(tclvalue(out.series)) == CbOUTSeries[6]) len <- 2
			if(str_trim(tclvalue(out.series)) == CbOUTSeries[7]) len <- 5
			if(str_trim(tclvalue(out.series)) == CbOUTSeries[8]) len <- 11
			mon1 <- which(MOIS %in% str_trim(tclvalue(start.seas)))
			mon1 <- (mon1 + len) %% 12
			mon1[mon1 == 0] <- 12
			tclvalue(end.seas) <- MOIS[mon1]

			tclvalue(startMonth) <- tclvalue(start.seas)
			tclvalue(endMonth) <- tclvalue(end.seas)
		})

		#############################

		frameAggr <- ttklabelframe(subfr2, text = "Time series aggregation", relief = 'groove')

		aggr.fun <- tclVar(GeneralParameters$aggr.series$aggr.fun)
		min.frac <- tclVar(GeneralParameters$aggr.series$min.frac)
		opr.fun <- tclVar(GeneralParameters$aggr.series$opr.fun)
		opr.thres <- tclVar(GeneralParameters$aggr.series$opr.thres)

		AGGREGFUN <- c("mean", "sum", "count")
		if(GeneralParameters$in.series %in% c('hourly', 'daily') &
			!GeneralParameters$out.series$out.series %in% c('hourly', 'daily'))
				AGGREGFUN <- AGGREGFUN[-3]

		if(GeneralParameters$in.series == GeneralParameters$out.series$out.series)
		{
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

		txt.aggfun <- tklabel(frameAggr, text = 'Function', anchor = 'w', justify = 'left')
		cb.aggfun <- ttkcombobox(frameAggr, values = AGGREGFUN, textvariable = aggr.fun, width = 6, state = stateo0a)
		txt.minfrac <- tklabel(frameAggr, text = 'Min.Frac', anchor = 'w', justify = 'left')
		en.minfrac <- tkentry(frameAggr, textvariable = min.frac, width = 6, state = stateo0b)
		txt.opfun <- tklabel(frameAggr, text = 'Operator', anchor = 'w', justify = 'left')
		cb.opfun <- ttkcombobox(frameAggr, values = c(">=", ">", "<=", "<"), textvariable = opr.fun, width = 6, state = stateo1)
		txt.opthres <- tklabel(frameAggr, text = 'Threshold', anchor = 'w', justify = 'left')
		en.opthres <- tkentry(frameAggr, textvariable = opr.thres, width = 6, width = 6, state = stateo2)

		tkgrid(txt.aggfun, row = 0, column = 0, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
		tkgrid(cb.aggfun, row = 0, column = 1, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
		tkgrid(txt.minfrac, row = 0, column = 2, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
		tkgrid(en.minfrac, row = 0, column = 3, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
		tkgrid(txt.opfun, row = 1, column = 0, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
		tkgrid(cb.opfun, row = 1, column = 1, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
		tkgrid(txt.opthres, row = 1, column = 2, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
		tkgrid(en.opthres, row = 1, column = 3, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)

		infobulle(cb.aggfun, 'Function that have to be applied for aggregating from daily/dekadal/monthly into\na higher time step (e.g., for precipitation FUN=sum and for temperature FUN=mean)')
		status.bar.display(cb.aggfun, 'Function that have to be applied for aggregating from daily/dekadal/monthly into\na higher time step (e.g., for precipitation FUN=sum and for temperature FUN=mean)')
		infobulle(en.minfrac, 'Minimum fraction of available data that must be present within each output time step')
		status.bar.display(en.minfrac, 'Minimum fraction of available data that must be present within each output time step')
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

		#############################
		tkgrid(frameDate, row = 0, column = 0, sticky = '', padx = 1, pady = 1, ipadx = 1, ipady = 1)
		tkgrid(frameTSOut, row = 1, column = 0, sticky = 'we', padx = 1, pady = 3, ipadx = 1, ipady = 1)
		tkgrid(frameAggr, row = 2, column = 0, sticky = '', padx = 1, pady = 3, ipadx = 1, ipady = 1)

	######################################################################################################

	#Tab3
	subfr3 <- bwTabScrollableFrame(cmd.tab3)

	##########################################

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
				if(.cdtData$OpenTab$Type[[tabid]] == "img" & !is.null(.cdtData$EnvData$tab$MapSelect))
				{
					if(.cdtData$OpenTab$Data[[tabid]][[1]][[1]]$ID == .cdtData$EnvData$tab$MapSelect[[2]])
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
				if(.cdtData$OpenTab$Type[[tabid]] == "img" & !is.null(.cdtData$EnvData$tab$MapSelect))
				{
					if(.cdtData$OpenTab$Data[[tabid]][[1]][[1]]$ID == .cdtData$EnvData$tab$MapSelect[[2]])
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

			tclvalue(.cdtData$EnvData$pressGetCoords) <- 0

			tkconfigure(.cdtData$EnvData$zoom$btZoomP, relief = 'raised', bg = 'lightblue', state = 'normal')
			tkconfigure(.cdtData$EnvData$zoom$btZoomM, relief = 'raised', bg = 'lightblue', state = 'normal')
			tkconfigure(.cdtData$EnvData$zoom$btZoomRect, relief = 'raised', bg = 'lightblue', state = 'normal')
			tkconfigure(.cdtData$EnvData$zoom$btPanImg, relief = 'raised', bg = 'lightblue', state = 'normal')

			tkconfigure(.cdtData$EnvData$bt.GETArea, relief = 'raised', bg = 'lightblue', state = 'normal')
			stateADD <- if(str_trim(tclvalue(.cdtData$EnvData$type.extract)) %in% typeEXTRACT[c(2, 5)]) "normal" else "disabled"
			tkconfigure(.cdtData$EnvData$bt.ADDObj, relief = 'raised', bg = 'lightblue', state = stateADD)
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

			tclvalue(.cdtData$EnvData$pressGetCoords) <- 0

			tkconfigure(.cdtData$EnvData$zoom$btRedraw, relief = 'raised', bg = 'lightblue')
			tkconfigure(.cdtData$EnvData$zoom$btZoomP, relief = 'raised', bg = 'lightblue', state = 'normal')
			tkconfigure(.cdtData$EnvData$zoom$btZoomM, relief = 'raised', bg = 'lightblue', state = 'normal')
			tkconfigure(.cdtData$EnvData$zoom$btZoomRect, relief = 'raised', bg = 'lightblue', state = 'normal')
			tkconfigure(.cdtData$EnvData$zoom$btPanImg, relief = 'raised', bg = 'lightblue', state = 'normal')

			tkconfigure(.cdtData$EnvData$bt.GETArea, relief = 'raised', bg = 'lightblue', state = 'normal')
			stateADD <- if(str_trim(tclvalue(.cdtData$EnvData$type.extract)) %in% typeEXTRACT[c(2, 5)]) "normal" else "disabled"
			tkconfigure(.cdtData$EnvData$bt.ADDObj, relief = 'raised', bg = 'lightblue', state = stateADD)
		})

		tkbind(.cdtData$EnvData$zoom$btReset, "<Button-1>", function(){
			tclvalue(.cdtData$EnvData$zoom$pressButP) <- 0
			tclvalue(.cdtData$EnvData$zoom$pressButM) <- 0
			tclvalue(.cdtData$EnvData$zoom$pressButRect) <- 0
			tclvalue(.cdtData$EnvData$zoom$pressButDrag) <- 0

			tclvalue(.cdtData$EnvData$pressGetCoords) <- 0

			tkconfigure(.cdtData$EnvData$zoom$btRedraw, relief = 'raised', bg = 'lightblue')
			tkconfigure(.cdtData$EnvData$zoom$btZoomP, relief = 'raised', bg = 'lightblue', state = 'normal')
			tkconfigure(.cdtData$EnvData$zoom$btZoomM, relief = 'raised', bg = 'lightblue', state = 'normal')
			tkconfigure(.cdtData$EnvData$zoom$btZoomRect, relief = 'raised', bg = 'lightblue', state = 'normal')
			tkconfigure(.cdtData$EnvData$zoom$btPanImg, relief = 'raised', bg = 'lightblue', state = 'normal')

			tkconfigure(.cdtData$EnvData$bt.GETArea, relief = 'raised', bg = 'lightblue', state = 'normal')
			stateADD <- if(str_trim(tclvalue(.cdtData$EnvData$type.extract)) %in% typeEXTRACT[c(2, 5)]) "normal" else "disabled"
			tkconfigure(.cdtData$EnvData$bt.ADDObj, relief = 'raised', bg = 'lightblue', state = stateADD)
		})

		tkbind(.cdtData$EnvData$zoom$btZoomP, "<Button-1>", function(){
			tclvalue(.cdtData$EnvData$zoom$pressButP) <- 1
			tclvalue(.cdtData$EnvData$zoom$pressButM) <- 0
			tclvalue(.cdtData$EnvData$zoom$pressButRect) <- 0
			tclvalue(.cdtData$EnvData$zoom$pressButDrag) <- 0

			tclvalue(.cdtData$EnvData$pressGetCoords) <- 0

			tkconfigure(.cdtData$EnvData$zoom$btRedraw, relief = 'raised', bg = 'lightblue')
			tkconfigure(.cdtData$EnvData$zoom$btZoomP, relief = 'raised', bg = 'red', state = 'disabled')
			tkconfigure(.cdtData$EnvData$zoom$btZoomM, relief = 'raised', bg = 'lightblue', state = 'normal')
			tkconfigure(.cdtData$EnvData$zoom$btZoomRect, relief = 'raised', bg = 'lightblue', state = 'normal')
			tkconfigure(.cdtData$EnvData$zoom$btPanImg, relief = 'raised', bg = 'lightblue', state = 'normal')

			tkconfigure(.cdtData$EnvData$bt.GETArea, relief = 'raised', bg = 'lightblue', state = 'normal')
			stateADD <- if(str_trim(tclvalue(.cdtData$EnvData$type.extract)) %in% typeEXTRACT[c(2, 5)]) "normal" else "disabled"
			tkconfigure(.cdtData$EnvData$bt.ADDObj, relief = 'raised', bg = 'lightblue', state = stateADD)
		})

		tkbind(.cdtData$EnvData$zoom$btZoomM, "<Button-1>", function(){
			tclvalue(.cdtData$EnvData$zoom$pressButP) <- 0
			tclvalue(.cdtData$EnvData$zoom$pressButM) <- 1
			tclvalue(.cdtData$EnvData$zoom$pressButRect) <- 0
			tclvalue(.cdtData$EnvData$zoom$pressButDrag) <- 0

			tclvalue(.cdtData$EnvData$pressGetCoords) <- 0

			tkconfigure(.cdtData$EnvData$zoom$btRedraw, relief = 'raised', bg = 'lightblue')
			tkconfigure(.cdtData$EnvData$zoom$btZoomP, relief = 'raised', bg = 'lightblue', state = 'normal')
			tkconfigure(.cdtData$EnvData$zoom$btZoomM, relief = 'raised', bg = 'red', state = 'disabled')
			tkconfigure(.cdtData$EnvData$zoom$btZoomRect, relief = 'raised', bg = 'lightblue', state = 'normal')
			tkconfigure(.cdtData$EnvData$zoom$btPanImg, relief = 'raised', bg = 'lightblue', state = 'normal')

			tkconfigure(.cdtData$EnvData$bt.GETArea, relief = 'raised', bg = 'lightblue', state = 'normal')
			stateADD <- if(str_trim(tclvalue(.cdtData$EnvData$type.extract)) %in% typeEXTRACT[c(2, 5)]) "normal" else "disabled"
			tkconfigure(.cdtData$EnvData$bt.ADDObj, relief = 'raised', bg = 'lightblue', state = stateADD)
		})

		tkbind(.cdtData$EnvData$zoom$btZoomRect, "<Button-1>", function(){
			tclvalue(.cdtData$EnvData$zoom$pressButP) <- 0
			tclvalue(.cdtData$EnvData$zoom$pressButM) <- 0
			tclvalue(.cdtData$EnvData$zoom$pressButRect) <- 1
			tclvalue(.cdtData$EnvData$zoom$pressButDrag) <- 0

			tclvalue(.cdtData$EnvData$pressGetCoords) <- 0

			tkconfigure(.cdtData$EnvData$zoom$btRedraw, relief = 'raised', bg = 'lightblue')
			tkconfigure(.cdtData$EnvData$zoom$btZoomP, relief = 'raised', bg = 'lightblue', state = 'normal')
			tkconfigure(.cdtData$EnvData$zoom$btZoomM, relief = 'raised', bg = 'lightblue', state = 'normal')
			tkconfigure(.cdtData$EnvData$zoom$btZoomRect, relief = 'raised', bg = 'red', state = 'disabled')
			tkconfigure(.cdtData$EnvData$zoom$btPanImg, relief = 'raised', bg = 'lightblue', state = 'normal')

			tkconfigure(.cdtData$EnvData$bt.GETArea, relief = 'raised', bg = 'lightblue', state = 'normal')
			stateADD <- if(str_trim(tclvalue(.cdtData$EnvData$type.extract)) %in% typeEXTRACT[c(2, 5)]) "normal" else "disabled"
			tkconfigure(.cdtData$EnvData$bt.ADDObj, relief = 'raised', bg = 'lightblue', state = stateADD)
		})

		tkbind(.cdtData$EnvData$zoom$btPanImg, "<Button-1>", function(){
			tclvalue(.cdtData$EnvData$zoom$pressButP) <- 0
			tclvalue(.cdtData$EnvData$zoom$pressButM) <- 0
			tclvalue(.cdtData$EnvData$zoom$pressButRect) <- 0
			tclvalue(.cdtData$EnvData$zoom$pressButDrag) <- 1

			tclvalue(.cdtData$EnvData$pressGetCoords) <- 0

			tkconfigure(.cdtData$EnvData$zoom$btRedraw, relief = 'raised', bg = 'lightblue')
			tkconfigure(.cdtData$EnvData$zoom$btZoomP, relief = 'raised', bg = 'lightblue', state = 'normal')
			tkconfigure(.cdtData$EnvData$zoom$btZoomM, relief = 'raised', bg = 'lightblue', state = 'normal')
			tkconfigure(.cdtData$EnvData$zoom$btZoomRect, relief = 'raised', bg = 'lightblue', state = 'normal')
			tkconfigure(.cdtData$EnvData$zoom$btPanImg, relief = 'raised', bg = 'red', state = 'disabled')

			tkconfigure(.cdtData$EnvData$bt.GETArea, relief = 'raised', bg = 'lightblue', state = 'normal')
			stateADD <- if(str_trim(tclvalue(.cdtData$EnvData$type.extract)) %in% typeEXTRACT[c(2, 5)]) "normal" else "disabled"
			tkconfigure(.cdtData$EnvData$bt.ADDObj, relief = 'raised', bg = 'lightblue', state = stateADD)
		})

		#############################

		frameExtract <- ttklabelframe(subfr3, text = "Extraction Type", relief = 'groove')

		typeEXTRACT <- c('Point', 'Multiple Points', 'Rectangle', 'Polygon', 'Multiple Polygons')
		typeEXTRACTV <- c('point', 'mpoint', 'rect', 'poly', 'mpoly')

		.cdtData$EnvData$type.extract <- tclVar()
		tclvalue(.cdtData$EnvData$type.extract) <- typeEXTRACT[typeEXTRACTV %in% GeneralParameters$type.extract]

		.cdtData$EnvData$minlonRect <- tclVar(GeneralParameters$Geom$minlon)
		.cdtData$EnvData$maxlonRect <- tclVar(GeneralParameters$Geom$maxlon)
		.cdtData$EnvData$minlatRect <- tclVar(GeneralParameters$Geom$minlat)
		.cdtData$EnvData$maxlatRect <- tclVar(GeneralParameters$Geom$maxlat)
		.cdtData$EnvData$namePoly <- tclVar(GeneralParameters$Geom$namePoly)

		padLon <- tclVar(GeneralParameters$Geom$padlon)
		padLat <- tclVar(GeneralParameters$Geom$padlat)

		pointrect <- tclVar(typeEXTRACT[1])
		minrect <- tclVar()
		maxrect <- tclVar()

		statePts <- if(GeneralParameters$type.extract %in% c('point', 'mpoint')) "normal" else "disabled"
		stateRct <- if(GeneralParameters$type.extract == 'rect') "normal" else "disabled"
		statePad <- "normal"
		statePol <- if(GeneralParameters$type.extract %in% c('poly', 'mpoly')) "normal" else "disabled"
		stateADD <- if(GeneralParameters$type.extract %in% c('mpoint', 'mpoly')) "normal" else "disabled"

		txt.Type <- tklabel(frameExtract, text = "Type", anchor = 'e', justify = 'right')
		cb.TypeExtr <- ttkcombobox(frameExtract, values = typeEXTRACT, textvariable = .cdtData$EnvData$type.extract, width = largeur4)

		txt.PointRect <- tklabel(frameExtract, text = tclvalue(pointrect), textvariable = pointrect, anchor = 'e', justify = 'right', bg = 'green')
		txt.MIN <- tklabel(frameExtract, text = tclvalue(minrect), textvariable = minrect)
		txt.MAX <- tklabel(frameExtract, text = tclvalue(maxrect), textvariable = maxrect)
		txt.PAD <- tklabel(frameExtract, text = "Pad")

		txt.LON <- tklabel(frameExtract, text = 'Longitude', anchor = 'e', justify = 'right')
		en.minlon <- tkentry(frameExtract, width = 7, textvariable = .cdtData$EnvData$minlonRect, justify = "left", state = statePts)
		en.maxlon <- tkentry(frameExtract, width = 7, textvariable = .cdtData$EnvData$maxlonRect, justify = "left", state = stateRct)
		txt.PlusM1 <- tklabel(frameExtract, text = '\u00B1')
		en.PadLon <- tkentry(frameExtract, width = 4, textvariable = padLon, justify = "left", state = statePad)

		txt.LAT <- tklabel(frameExtract, text = 'Latitude', anchor = 'e', justify = 'right')
		en.minlat <- tkentry(frameExtract, width = 7, textvariable = .cdtData$EnvData$minlatRect, justify = "left", state = statePts)
		en.maxlat <- tkentry(frameExtract, width = 7, textvariable = .cdtData$EnvData$maxlatRect, justify = "left", state = stateRct)
		txt.PlusM2 <- tklabel(frameExtract, text = '\u00B1')
		en.PadLat <- tkentry(frameExtract, width = 4, textvariable = padLat, justify = "left", state = statePad)

		txt.Polygon <- tklabel(frameExtract, text = "Polygon", anchor = 'e', justify = 'right', bg = 'green')
		cb.Polygon <- ttkcombobox(frameExtract, values = '', textvariable = .cdtData$EnvData$namePoly, state = statePol, width = largeur4)

		.cdtData$EnvData$bt.ADDObj <- tkbutton(frameExtract, text = "ADD", relief = 'raised', bg = 'lightblue', state = stateADD)
		.cdtData$EnvData$bt.GETArea <- tkbutton(frameExtract, text = "GET", relief = 'raised', bg = 'lightblue', state = 'normal')

		#################
		nbpts <- 1
		retMultiP <- NULL

		tkconfigure(.cdtData$EnvData$bt.ADDObj, command = function(){
			if(.cdtData$EnvData$dlgBoxOpen){
				if(str_trim(tclvalue(.cdtData$EnvData$type.extract)) == typeEXTRACT[2])
				{
					tkinsert(retMultiP$textObj, "end", paste(paste('Pts', nbpts, sep = ''),
														tclvalue(.cdtData$EnvData$minlonRect),
														tclvalue(.cdtData$EnvData$minlatRect), "\n"))
					nbpts <<- nbpts + 1
				}
				if(str_trim(tclvalue(.cdtData$EnvData$type.extract)) == typeEXTRACT[5])
				{
					tkinsert(retMultiP$textObj, "end", paste(tclvalue(.cdtData$EnvData$namePoly), "\n"))
				}
			}
			stateADD <- if(str_trim(tclvalue(.cdtData$EnvData$type.extract)) %in% typeEXTRACT[c(2, 5)]) "normal" else "disabled"
			tkconfigure(.cdtData$EnvData$bt.ADDObj, relief = 'raised', bg = 'lightblue', state = stateADD)
		})

		tkgrid(txt.Type, row = 0, column = 0, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 2, ipadx = 1, ipady = 1)
		tkgrid(cb.TypeExtr, row = 0, column = 1, sticky = 'we', rowspan = 1, columnspan = 4, padx = 1, pady = 2, ipadx = 1, ipady = 1)

		tkgrid(txt.PointRect, row = 1, column = 0, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
		tkgrid(txt.MIN, row = 1, column = 1, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
		tkgrid(txt.MAX, row = 1, column = 2, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
		tkgrid(txt.PAD, row = 1, column = 4, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)

		tkgrid(txt.LON, row = 2, column = 0, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
		tkgrid(en.minlon, row = 2, column = 1, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
		tkgrid(en.maxlon, row = 2, column = 2, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
		tkgrid(txt.PlusM1, row = 2, column = 3, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
		tkgrid(en.PadLon, row = 2, column = 4, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)

		tkgrid(txt.LAT, row = 3, column = 0, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
		tkgrid(en.minlat, row = 3, column = 1, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
		tkgrid(en.maxlat, row = 3, column = 2, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
		tkgrid(txt.PlusM2, row = 3, column = 3, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
		tkgrid(en.PadLat, row = 3, column = 4, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)

		tkgrid(txt.Polygon, row = 4, column = 0, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 2, ipadx = 1, ipady = 1)
		tkgrid(cb.Polygon, row = 4, column = 1, sticky = 'we', rowspan = 1, columnspan = 4, padx = 1, pady = 2, ipadx = 1, ipady = 1)

		tkgrid(.cdtData$EnvData$bt.ADDObj, row = 5, column = 0, sticky = 'we', rowspan = 1, columnspan = 2, padx = 1, pady = 1, ipadx = 1, ipady = 1)
		tkgrid(.cdtData$EnvData$bt.GETArea, row = 5, column = 2, sticky = 'we', rowspan = 1, columnspan = 3, padx = 1, pady = 1, ipadx = 1, ipady = 1)

		infobulle(en.PadLon, 'Add value in decimal degree to get\nrectangle centered at the target points')
		status.bar.display(en.PadLon, 'Add value in decimal degree to get\nrectangle centered at the target points')
		infobulle(en.PadLat, 'Add value in decimal degree to get rectangle\ncentered at the target points')
		status.bar.display(en.PadLat, 'Add value in decimal degree to get\nrectangle centered at the target points')
		infobulle(.cdtData$EnvData$bt.GETArea, 'Before to click on the map to select object, click here to activate the function')
		status.bar.display(.cdtData$EnvData$bt.GETArea, 'Before to click on the map to select object, click here to activate the function')
		infobulle(.cdtData$EnvData$bt.ADDObj, 'Multiple Points&Polygons: after selecting object from the map\nclick here to add the object to the list')
		status.bar.display(.cdtData$EnvData$bt.ADDObj, 'Multiple Points&Polygons: after selecting object from the map\nclick here to add the object to the list')

		#################
		tkbind(cb.Polygon, "<<ComboboxSelected>>", function(){
			.cdtData$EnvData$selectedPolygon <- NULL
			if(tclvalue(.cdtData$EnvData$namePoly) != ''){
				shpfopen <- getShpOpenData(shpFile)
				shpf <- shpfopen[[2]]
				ids <- as.numeric(tclvalue(tcl(.cdtData$EnvData$cb.shpAttr, 'current'))) + 1
				.cdtData$EnvData$selectedPolygon <- getBoundaries(shpf[shpf@data[, ids] == tclvalue(.cdtData$EnvData$namePoly), ])
			}

			# selectedPolygon
			tabid <- as.numeric(tclvalue(tkindex(.cdtEnv$tcl$main$tknotes, 'current'))) + 1
			if(length(.cdtData$OpenTab$Type) > 0){
				if(.cdtData$OpenTab$Type[[tabid]] == "img" & !is.null(.cdtData$EnvData$tab$MapSelect))
				{
					if(.cdtData$OpenTab$Data[[tabid]][[1]][[1]]$ID == .cdtData$EnvData$tab$MapSelect[[2]])
					{
						refreshPlot(W = .cdtData$OpenTab$Data[[tabid]][[2]][[1]],
									img = .cdtData$OpenTab$Data[[tabid]][[2]][[2]],
									hscale = as.numeric(tclvalue(tkget(.cdtEnv$tcl$toolbar$spinH))),
									vscale = as.numeric(tclvalue(tkget(.cdtEnv$tcl$toolbar$spinV))))
					}
				}
			}
		})

		##################
		tkbind(.cdtData$EnvData$bt.GETArea, "<Button-1>", function(){
			tclvalue(.cdtData$EnvData$zoom$pressButP) <- 0
			tclvalue(.cdtData$EnvData$zoom$pressButM) <- 0
			tclvalue(.cdtData$EnvData$zoom$pressButRect) <- 0
			tclvalue(.cdtData$EnvData$zoom$pressButDrag) <- 0

			tclvalue(.cdtData$EnvData$pressGetCoords) <- 1

			tkconfigure(.cdtData$EnvData$zoom$btRedraw, relief = 'raised', bg = 'lightblue')

			tkconfigure(.cdtData$EnvData$zoom$btZoomP, relief = 'raised', bg = 'lightblue', state = 'normal')
			tkconfigure(.cdtData$EnvData$zoom$btZoomM, relief = 'raised', bg = 'lightblue', state = 'normal')
			tkconfigure(.cdtData$EnvData$zoom$btZoomRect, relief = 'raised', bg = 'lightblue', state = 'normal')
			tkconfigure(.cdtData$EnvData$zoom$btPanImg, relief = 'raised', bg = 'lightblue', state = 'normal')

			tkconfigure(.cdtData$EnvData$bt.GETArea, relief = 'raised', bg = 'red', state = 'disabled')
			stateADD <- if(str_trim(tclvalue(.cdtData$EnvData$type.extract)) %in% typeEXTRACT[c(2, 5)]) "normal" else "disabled"
			tkconfigure(.cdtData$EnvData$bt.ADDObj, relief = 'raised', bg = 'lightblue', state = stateADD)
		})

		##################
		tkbind(cb.TypeExtr, "<<ComboboxSelected>>", function(){
			.cdtData$EnvData$selectedPolygon <- NULL
			OutFileFormat <- CbOutFileFormat

			if(str_trim(tclvalue(.cdtData$EnvData$type.extract)) == typeEXTRACT[1])
			{
				if(!is.null(retMultiP$win)) tkdestroy(retMultiP$win)

				statePts <- 'normal'
				stateRct <- 'disabled'
				statePad <- 'normal'
				statePol <- 'disabled'
				stateADD <- 'disabled'

				pointrectVal <- 'Point'
				minrectVal <- ''
				maxrectVal <- ''

				stateSpAv <- 'disabled'
				spatAverg <- '0'
				txtfileORdir <- 'File to save extracted data'
				colfileORdir <- 'lightblue'
				isFile <- TRUE
				OutFileFormat <- OutFileFormat[1:2]
				out.formatVAL <- if(str_trim(tclvalue(out.format)) %in% CbOutFileFormat[3:4]) OutFileFormat[1] else tclvalue(out.format)
			}

			##
			if(str_trim(tclvalue(.cdtData$EnvData$type.extract)) == typeEXTRACT[3])
			{
				if(!is.null(retMultiP$win)) tkdestroy(retMultiP$win)

				statePts <- 'normal'
				stateRct <- 'normal'
				statePad <- 'disabled'
				statePol <- 'disabled'
				stateADD <- 'disabled'

				pointrectVal <- 'Rectangle'
				minrectVal <- 'Min'
				maxrectVal <- 'Max'

				stateSpAv <- if(str_trim(tclvalue(out.format)) == CbOutFileFormat[2]) 'normal' else 'disabled'
				if(str_trim(tclvalue(out.format)) %in% CbOutFileFormat[3:4]){
					spatAverg <- "0"
				}else if(str_trim(tclvalue(out.format)) == CbOutFileFormat[1]){
					spatAverg <- "1"
				}else spatAverg <- tclvalue(spatAverage)

				if(str_trim(tclvalue(out.format)) == CbOutFileFormat[3]){
					txtfileORdir <- 'Directory to save extracted data'
					colfileORdir <- 'lightgreen'
					isFile <- FALSE
				}else{
					txtfileORdir <- 'File to save extracted data'
					colfileORdir <- 'lightblue'
					isFile <- TRUE
				}

				out.formatVAL <- tclvalue(out.format)
			}

			##
			if(str_trim(tclvalue(.cdtData$EnvData$type.extract)) == typeEXTRACT[4])
			{
				if(!is.null(retMultiP$win)) tkdestroy(retMultiP$win)

				statePts <- 'disabled'
				stateRct <- 'disabled'
				statePad <- 'disabled'
				statePol <- 'normal'
				stateADD <- 'disabled'

				pointrectVal <- tclvalue(pointrect)
				minrectVal <- ''
				maxrectVal <- ''

				stateSpAv <- if(str_trim(tclvalue(out.format)) == CbOutFileFormat[2]) 'normal' else 'disabled'
				if(str_trim(tclvalue(out.format)) %in% CbOutFileFormat[3:4]){
					spatAverg <- "0"
				}else if(str_trim(tclvalue(out.format)) == CbOutFileFormat[1]){
					spatAverg <- "1"
				}else spatAverg <- tclvalue(spatAverage)

				if(str_trim(tclvalue(out.format)) == CbOutFileFormat[3]){
					txtfileORdir <- 'Directory to save extracted data'
					colfileORdir <- 'lightgreen'
					isFile <- FALSE
				}else{
					txtfileORdir <- 'File to save extracted data'
					colfileORdir <- 'lightblue'
					isFile <- TRUE
				}

				out.formatVAL <- tclvalue(out.format)

				if(tclvalue(.cdtData$EnvData$namePoly) != '')
				{
					shpfopen <- getShpOpenData(shpFile)
					shpf <- shpfopen[[2]]
					ids <- as.numeric(tclvalue(tcl(.cdtData$EnvData$cb.shpAttr, 'current'))) + 1
					.cdtData$EnvData$selectedPolygon <- getBoundaries(shpf[shpf@data[, ids] == tclvalue(.cdtData$EnvData$namePoly), ])
				}
			}

			##
			if(str_trim(tclvalue(.cdtData$EnvData$type.extract)) == typeEXTRACT[2])
			{
				if(!is.null(retMultiP$win)) tkdestroy(retMultiP$win)
				retMultiP <<- extractTS.previewWin(c('normal', 'disabled'),
													list(.cdtData$EnvData$cb.shpAttr, shpFile),
													"mpoint")
				statePts <- 'normal'
				stateRct <- 'disabled'
				statePad <- 'normal'
				statePol <- 'disabled'
				stateADD <- 'normal'

				pointrectVal <- 'Point'
				minrectVal <- ''
				maxrectVal <- ''

				stateSpAv <- 'disabled'
				spatAverg <- '0'
				txtfileORdir <- 'File to save extracted data'
				colfileORdir <- 'lightblue'
				isFile <- TRUE
				OutFileFormat <- OutFileFormat[1:2]
				out.formatVAL <- if(str_trim(tclvalue(out.format)) %in% CbOutFileFormat[3:4]) OutFileFormat[1] else tclvalue(out.format)
			}

			##
			if(str_trim(tclvalue(.cdtData$EnvData$type.extract)) == typeEXTRACT[5])
			{
				if(!is.null(retMultiP$win)) tkdestroy(retMultiP$win)
				retMultiP <<- extractTS.previewWin(c('disabled', 'normal'),
													list(.cdtData$EnvData$cb.shpAttr, shpFile),
													"mpoly")
				statePts <- 'disabled'
				stateRct <- 'disabled'
				statePad <- 'disabled'
				statePol <- 'normal'
				stateADD <- 'normal'

				pointrectVal <- tclvalue(pointrect)
				minrectVal <- ''
				maxrectVal <- ''

				stateSpAv <- 'disabled'
				spatAverg <- '0'
				txtfileORdir <- 'File to save extracted data'
				colfileORdir <- 'lightblue'
				isFile <- TRUE
				OutFileFormat <- OutFileFormat[1:2]
				out.formatVAL <- if(str_trim(tclvalue(out.format)) %in% CbOutFileFormat[3:4]) OutFileFormat[1] else tclvalue(out.format)

				if(tclvalue(.cdtData$EnvData$namePoly) != ''){
					shpfopen <- getShpOpenData(shpFile)
					shpf <- shpfopen[[2]]
					ids <- as.numeric(tclvalue(tcl(.cdtData$EnvData$cb.shpAttr, 'current')))+1
					.cdtData$EnvData$selectedPolygon <- getBoundaries(shpf[shpf@data[, ids] == tclvalue(.cdtData$EnvData$namePoly), ])
				}
			}

			tkconfigure(en.minlon, state = statePts)
			tkconfigure(en.minlat, state = statePts)
			tkconfigure(en.maxlon, state = stateRct)
			tkconfigure(en.maxlat, state = stateRct)
			tkconfigure(en.PadLon, state = statePad)
			tkconfigure(en.PadLat, state = statePad)

			tkconfigure(cb.Polygon, state = statePol)
			tkconfigure(.cdtData$EnvData$bt.ADDObj, state = stateADD)

			tclvalue(pointrect) <- pointrectVal
			tclvalue(minrect) <- minrectVal
			tclvalue(maxrect) <- maxrectVal

			##
			tkconfigure(chk.SpAvrg, state = stateSpAv)
			tclvalue(spatAverage) <- spatAverg
			tkconfigure(txt.saveData, bg = colfileORdir)
			tclvalue(fileORdir) <- txtfileORdir
			tkconfigure(bt.saveData, command = function() fileORdir2Save(file2save, isFile = isFile))

			tkconfigure(cb.outFormat, values = OutFileFormat)
			tclvalue(out.format) <- out.formatVAL

			##
			tclvalue(.cdtData$EnvData$minlonRect) <- ''
			tclvalue(.cdtData$EnvData$maxlonRect) <- ''
			tclvalue(.cdtData$EnvData$minlatRect) <- ''
			tclvalue(.cdtData$EnvData$maxlatRect) <- ''
			tkconfigure(.cdtData$EnvData$bt.GETArea, relief = 'raised', bg = 'lightblue', state = 'normal')

			# selectedPolygon
			tabid <- as.numeric(tclvalue(tkindex(.cdtEnv$tcl$main$tknotes, 'current'))) + 1
			if(length(.cdtData$OpenTab$Type) > 0){
				if(.cdtData$OpenTab$Type[[tabid]] == "img" & !is.null(.cdtData$EnvData$tab$MapSelect))
				{
					if(.cdtData$OpenTab$Data[[tabid]][[1]][[1]]$ID == .cdtData$EnvData$tab$MapSelect[[2]])
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

		#############################
		tkgrid(frameZoom, row = 0, column = 0, sticky = '')
		tkgrid(frameExtract, row = 1, column = 0, sticky = 'we', pady = 3)

	######################################################################################################

	#Tab4
	subfr4 <- bwTabScrollableFrame(cmd.tab4)

	##########################################

		frameOutTypeS <- ttklabelframe(subfr4, text = "Output Time series", relief = 'groove')

		CbOUTTypeSeries <- c('Raw Time Series', 'Anomalies', 'Standardized Anomalies', 'Climatologies')
		OUTTypeSeriesV <- c('rawts', 'anom', 'stanom', 'climato')

		if(GeneralParameters$in.series == 'hourly')
		{
			OUTTypeSeries <- c(CbOUTTypeSeries[1], '')
			type.series <- tclVar(OUTTypeSeries[1])
		}else{
			OUTTypeSeries <- CbOUTTypeSeries
			type.series <- tclVar()
			tclvalue(type.series) <- CbOUTTypeSeries[OUTTypeSeriesV %in% GeneralParameters$type.series]
		}

		winsize <- tclVar(GeneralParameters$climato$winsize)

		stateClim0 <- if(GeneralParameters$type.series == 'rawts') 'disabled' else 'normal'
		stateClim2 <- if(GeneralParameters$in.series != 'hourly')
						{
							if(GeneralParameters$in.series == 'daily' &
								GeneralParameters$type.series != 'rawts') "normal" else "disabled"
						}else "disabled"

		cb.typeseries <- ttkcombobox(frameOutTypeS, values = OUTTypeSeries, textvariable = type.series, width = largeur1)
		bt.BasePeriod <- ttkbutton(frameOutTypeS, text = "Set Base Period", state = stateClim0)
		txt.winsize1 <- tklabel(frameOutTypeS, text = "Centered time window", anchor = 'e', justify = 'right')
		en.winsize <- tkentry(frameOutTypeS, textvariable = winsize, width = 3, state = stateClim2)
		txt.winsize2 <- tklabel(frameOutTypeS, text = "days", anchor = 'w', justify = 'left')

		tkconfigure(bt.BasePeriod, command = function(){
			Params <- GeneralParameters[["climato"]][c('all.years', 'start.year', 'end.year', 'min.year')]
			Params <- getInfoBasePeriod(.cdtEnv$tcl$main$win, Params)
			GeneralParameters$climato$all.years <<- Params$all.years
			GeneralParameters$climato$start.year <<- Params$start.year
			GeneralParameters$climato$end.year <<- Params$end.year
			GeneralParameters$climato$min.year <<- Params$min.year
		})

		tkgrid(cb.typeseries, row = 0, column = 0, sticky = 'we', rowspan = 1, columnspan = 5, padx = 1, pady = 1, ipadx = 1, ipady = 1)
		tkgrid(bt.BasePeriod, row = 1, column = 0, sticky = 'we', rowspan = 1, columnspan = 5, padx = 1, pady = 1, ipadx = 1, ipady = 1)
		tkgrid(txt.winsize1, row = 2, column = 0, sticky = 'e', rowspan = 1, columnspan = 3, padx = 1, pady = 0, ipadx = 1, ipady = 1)
		tkgrid(en.winsize, row = 2, column = 3, sticky = 'e', rowspan = 1, columnspan = 1, padx = 1, pady = 0, ipadx = 1, ipady = 1)
		tkgrid(txt.winsize2, row = 2, column = 4, sticky = 'w', rowspan = 1, columnspan = 1, padx = 1, pady = 0, ipadx = 1, ipady = 1)


		infobulle(cb.typeseries, 'Select the type of output data')
		status.bar.display(cb.typeseries, 'Select the type of output data')
		infobulle(en.winsize, "The daily climatology is calculated using a centered (2 x window + 1) time window")
		status.bar.display(en.winsize, "The daily climatology is calculated using a centered (2 x window + 1) time window")

		#################
		tkbind(cb.typeseries, "<<ComboboxSelected>>", function(){
			if(!str_trim(tclvalue(type.series)) %in% c(CbOUTTypeSeries[1], ''))
			{
				stateClim0 <- "normal"
				stateClim2 <- if(str_trim(tclvalue(timeSteps)) == CbperiodVAL[2]) "normal" else "disabled"
			}else{
				stateClim0 <- "disabled"
				stateClim2 <- "disabled"
			}

			tkconfigure(bt.BasePeriod, state = stateClim0)
			tkconfigure(en.winsize, state = stateClim2)
		})

		#############################

		frameOutFormat <- ttklabelframe(subfr4, text = "Output File Format", relief = 'groove')

		CbOutFileFormat <- c('CDT Format', 'CPT Format', 'NetCDF', 'Time|Lat|Lon|Value Format')
		OutFileFormatV <- c('cdt', 'cpt', 'ncdf', 'tyxz')

		OutFileFormat <- if(GeneralParameters$type.extract %in% c("point", "mpoint", "mpoly")) CbOutFileFormat[1:2] else CbOutFileFormat
		out.format <- tclVar()
		tclvalue(out.format) <- CbOutFileFormat[OutFileFormatV %in% GeneralParameters$out.data$format]

		cb.outFormat <- ttkcombobox(frameOutFormat, values = OutFileFormat, textvariable = out.format, width = largeur1)

		tkgrid(cb.outFormat, row = 0, column = 0, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)

		infobulle(cb.outFormat, 'Select the format of the output data')
		status.bar.display(cb.outFormat, 'Select the format of the output data')

		#################
		tkbind(cb.outFormat, "<<ComboboxSelected>>", function(){
			if(str_trim(tclvalue(.cdtData$EnvData$type.extract)) %in% typeEXTRACT[c(1, 2, 5)])
			{
				txtfileORdir <- 'File to save extracted data'
				colfileORdir <- 'lightblue'
				isFile <- TRUE

				spatAverg <- "0"
				stateSpAv <- 'disabled'
			}else{
				if(str_trim(tclvalue(out.format)) == CbOutFileFormat[3]){
					txtfileORdir <- 'Directory to save extracted data'
					colfileORdir <- 'lightgreen'
					isFile <- FALSE
				}else{
					txtfileORdir <- 'File to save extracted data'
					colfileORdir <- 'lightblue'
					isFile <- TRUE
				}

				if(str_trim(tclvalue(out.format)) %in% CbOutFileFormat[3:4]){
					spatAverg <- "0"
				}else if(str_trim(tclvalue(out.format)) == CbOutFileFormat[1]){
					spatAverg <- "1"
				}else spatAverg <- tclvalue(spatAverage)

				stateSpAv <- if(str_trim(tclvalue(out.format)) == CbOutFileFormat[2]) 'normal' else 'disabled'
			}

			tkconfigure(chk.SpAvrg, state = stateSpAv)
			tclvalue(spatAverage) <- spatAverg

			tkconfigure(txt.saveData, bg = colfileORdir)
			tclvalue(fileORdir) <- txtfileORdir
			tkconfigure(bt.saveData, command = function() fileORdir2Save(file2save, isFile = isFile))
		})

		#############################
		frameSpAvrg <- tkframe(subfr4, relief = 'groove', borderwidth = 2)

		spatAverage <- tclVar(GeneralParameters$out.data$sp.avrg)
		stateSpAv <- if(GeneralParameters$type.extract %in% c("point", "mpoint", "mpoly")) 'disabled' else 'normal'

		txt.SpAvrg <- tklabel(frameSpAvrg, text = 'Saptially Average Over Selected Area', anchor = 'e', justify = 'right')
		chk.SpAvrg <- tkcheckbutton(frameSpAvrg, variable = spatAverage, state = stateSpAv)

		tkgrid(txt.SpAvrg, row = 0, column = 0, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 3, ipadx = 1, ipady = 1)
		tkgrid(chk.SpAvrg, row = 0, column = 1, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 3, ipadx = 1, ipady = 1)

		#############################
		frameSave <- tkframe(subfr4, relief = 'groove', borderwidth = 2)

		if(GeneralParameters$out.data$sp.avrg)
		{
			txtfileORdir <- 'File to save extracted data'
			colfileORdir <- 'lightblue'
			isFile <- TRUE
		}else{
			if((GeneralParameters$type.extract %in% c("rect", "poly")) &
				GeneralParameters$out.data$format == "ncdf")
			{
				txtfileORdir <- 'Directory to save extracted data'
				colfileORdir <- 'lightgreen'
				isFile <- FALSE
			}else{
				txtfileORdir <- 'File to save extracted data'
				colfileORdir <- 'lightblue'
				isFile <- TRUE
			}
		}

		fileORdir <- tclVar(txtfileORdir)
		file2save <- tclVar(GeneralParameters$out.data$outdir)

		txt.saveData <- tklabel(frameSave, text = tclvalue(fileORdir), textvariable = fileORdir, anchor = 'w', justify = 'left', bg = colfileORdir)
		en.saveData <- tkentry(frameSave, textvariable = file2save, width = largeur5)
		bt.saveData <- tkbutton(frameSave, text = "...")

		tkconfigure(bt.saveData, command = function() fileORdir2Save(file2save, isFile = isFile))

		tkgrid(txt.saveData, row = 0, column = 0, sticky = 'we', rowspan = 1, columnspan = 5, padx = 1, pady = 0, ipadx = 1, ipady = 1)
		tkgrid(en.saveData, row = 1, column = 0, sticky = 'we', rowspan = 1, columnspan = 4, padx = 0, pady = 0, ipadx = 1, ipady = 1)
		tkgrid(bt.saveData, row = 1, column = 4, sticky = 'w', rowspan = 1, columnspan = 1, padx = 0, pady = 0, ipadx = 1, ipady = 1)

		infobulle(en.saveData, 'Enter the full path to directory to save extracted data')
		status.bar.display(en.saveData, 'Enter the full path to directory to save extracted data')
		infobulle(bt.saveData, 'or browse here')
		status.bar.display(bt.saveData, 'or browse here')

		#############################

		bt.Extract.Data <- ttkbutton(subfr4, text = "EXTRACT DATA")

		tkconfigure(bt.Extract.Data, command = function(){
			GeneralParameters$in.series <- periodVAL[CbperiodVAL %in% str_trim(tclvalue(timeSteps))]

			GeneralParameters$ncdf.file$dir <- str_trim(tclvalue(ncDIR))
			GeneralParameters$ncdf.file$sample <- str_trim(tclvalue(ncSample))
			GeneralParameters$ncdf.file$format <- str_trim(tclvalue(ncFormat))

			GeneralParameters$shp.file$shp <- str_trim(tclvalue(shpFile))
			GeneralParameters$shp.file$attr <- str_trim(tclvalue(shpAttr))

			GeneralParameters$date.range$start.month <- which(MOIS %in% str_trim(tclvalue(startMonth)))
			GeneralParameters$date.range$end.month <- which(MOIS %in% str_trim(tclvalue(endMonth)))

			GeneralParameters$out.series$out.series <- OUTseriesV[CbOUTSeries %in% str_trim(tclvalue(out.series))]

			GeneralParameters$out.series$start.seas <- which(MOIS %in% str_trim(tclvalue(start.seas)))
			GeneralParameters$out.series$end.seas <- which(MOIS %in% str_trim(tclvalue(end.seas)))

			GeneralParameters$aggr.series$aggr.fun <- str_trim(tclvalue(aggr.fun))
			GeneralParameters$aggr.series$min.frac <- as.numeric(str_trim(tclvalue(min.frac)))
			GeneralParameters$aggr.series$opr.fun <- str_trim(tclvalue(opr.fun))
			GeneralParameters$aggr.series$opr.thres <- as.numeric(str_trim(tclvalue(opr.thres)))

			GeneralParameters$type.series <- OUTTypeSeriesV[CbOUTTypeSeries %in% str_trim(tclvalue(type.series))]

			GeneralParameters$climato$winsize <- as.numeric(str_trim(tclvalue(winsize)))

			GeneralParameters$type.extract <- typeEXTRACTV[typeEXTRACT %in% str_trim(tclvalue(.cdtData$EnvData$type.extract))]

			GeneralParameters$Geom$minlon <- as.numeric(str_trim(tclvalue(.cdtData$EnvData$minlonRect)))
			GeneralParameters$Geom$maxlon <- as.numeric(str_trim(tclvalue(.cdtData$EnvData$maxlonRect)))
			GeneralParameters$Geom$minlat <- as.numeric(str_trim(tclvalue(.cdtData$EnvData$minlatRect)))
			GeneralParameters$Geom$maxlat <- as.numeric(str_trim(tclvalue(.cdtData$EnvData$maxlatRect)))
			GeneralParameters$Geom$padlon <- as.numeric(str_trim(tclvalue(padLon)))
			GeneralParameters$Geom$padlat <- as.numeric(str_trim(tclvalue(padLat)))
			GeneralParameters$Geom$namePoly <- str_trim(tclvalue(.cdtData$EnvData$namePoly))
			GeneralParameters$Geom$multiObj <- .cdtData$EnvData$multiptspoly

			GeneralParameters$out.data$format <- OutFileFormatV[CbOutFileFormat %in% str_trim(tclvalue(out.format))]

			GeneralParameters$out.data$sp.avrg <- switch(tclvalue(spatAverage), '0' = FALSE, '1' = TRUE)
			GeneralParameters$out.data$outdir <- str_trim(tclvalue(file2save))

			# assign('GeneralParameters', GeneralParameters, envir = .GlobalEnv)

			Insert.Messages.Out("Extraction .................")

			tkconfigure(.cdtEnv$tcl$main$win, cursor = 'watch')
			tcl('update')
			ret <- tryCatch(
				{
					ExtractDataProcs(GeneralParameters)
				},
				warning = function(w) warningFun(w),
				error = function(e) errorFun(e),
				finally = {
					tkconfigure(.cdtEnv$tcl$main$win, cursor = '')
					tcl('update')
				}
			)

			if(!is.null(ret)){
				if(ret == 0) Insert.Messages.Out("Extraction finished successfully")
				else Insert.Messages.Out("Extraction failed", format = TRUE)
			}else Insert.Messages.Out("Extraction failed", format = TRUE)
		})

		#############################
		tkgrid(frameOutTypeS, row = 0, column = 0, sticky = '', padx = 1, pady = 1, ipadx = 1, ipady = 1)
		tkgrid(frameOutFormat, row = 1, column = 0, sticky = '', padx = 1, pady = 3, ipadx = 1, ipady = 1)
		tkgrid(frameSpAvrg, row = 2, column = 0, sticky = '', padx = 1, pady = 3, ipadx = 1, ipady = 1)
		tkgrid(frameSave, row = 3, column = 0, sticky = 'we', padx = 1, pady = 3, ipadx = 1, ipady = 1)
		tkgrid(bt.Extract.Data, row = 4, column = 0, sticky = 'we', padx = 1, pady = 3, ipadx = 1, ipady = 1)

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
