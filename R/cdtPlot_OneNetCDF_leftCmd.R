
PlotOneNetCDFFileCmd <- function(){
	listOpenFiles <- openFile_ttkcomboList()
	if(WindowsOS()){
		largeur0 <- .cdtEnv$tcl$fun$w.widgets(28)
		largeur1 <- 18
	}else{
		largeur0 <- .cdtEnv$tcl$fun$w.widgets(22)
		largeur1 <- 16
	}

	# xml.dlg <- file.path(.cdtDir$dirLocal, "languages", "cdtPlot_OneNetCDF_leftCmd.xml")
	# lang.dlg <- cdtLanguageParse(xml.dlg, .cdtData$Config$lang.iso)

	.cdtData$EnvData$plot.maps$data.type <- "cdtnetcdf"

	###################

	.cdtEnv$tcl$main$cmd.frame <- tkframe(.cdtEnv$tcl$main$panel.left)

	tknote.cmd <- bwNoteBook(.cdtEnv$tcl$main$cmd.frame)
	cmd.tab1 <- bwAddTab(tknote.cmd, text = "Plot NetCDF Data")

	bwRaiseTab(tknote.cmd, cmd.tab1)

	tkgrid.columnconfigure(cmd.tab1, 0, weight = 1)
	tkgrid.rowconfigure(cmd.tab1, 0, weight = 1)

	#######################################################################################################

	#Tab1
	subfr1 <- bwTabScrollableFrame(cmd.tab1)

		#######################

		frameNC <- tkframe(subfr1, relief = 'groove', borderwidth = 2)

		ncdf.file <- tclVar()

		txt.ncfl <- tklabel(frameNC, text = "NetCDF data file", anchor = 'w', justify = 'left')
		cb.ncfl <- ttkcombobox(frameNC, values = unlist(listOpenFiles), textvariable = ncdf.file, width = largeur0)
		bt.ncfl <- tkbutton(frameNC, text = "...")

		tkconfigure(bt.ncfl, command = function(){
			tkconfigure(.cdtEnv$tcl$main$win, cursor = 'watch')
			tcl('update')
			on.exit({
				tkconfigure(.cdtEnv$tcl$main$win, cursor = '')
				tcl('update')
			})

			nc.opfiles <- getOpenNetcdf(.cdtEnv$tcl$main$win)
			if(!is.null(nc.opfiles)){
				update.OpenFiles('netcdf', nc.opfiles)
				listOpenFiles[[length(listOpenFiles) + 1]] <<- nc.opfiles[[1]]
				tclvalue(ncdf.file) <- nc.opfiles[[1]]
				lapply(list(cb.ncfl, cb.addshp), tkconfigure, values = unlist(listOpenFiles))
			}
		})

		tkgrid(txt.ncfl, row = 0, column = 0, sticky = 'we', rowspan = 1, columnspan = 6, padx = 1, pady = 0, ipadx = 1, ipady = 1)
		tkgrid(cb.ncfl, row = 1, column = 0, sticky = 'we', rowspan = 1, columnspan = 5, padx = 0, pady = 0, ipadx = 1, ipady = 1)
		tkgrid(bt.ncfl, row = 1, column = 5, sticky = 'w', rowspan = 1, columnspan = 1, padx = 0, pady = 0, ipadx = 1, ipady = 1)

		##############################################

		frameMap <- tkframe(subfr1)

		bt.nc.MapOpt <- ttkbutton(frameMap, text = .cdtEnv$tcl$lang$global[['button']][['4']], width = largeur1)
		bt.nc.maps <- ttkbutton(frameMap, text = .cdtEnv$tcl$lang$global[['button']][['3']], width = largeur1)

		###################

		.cdtData$EnvData$ncMapOp <- list(presetCol = list(color = 'tim.colors', reverse = FALSE),
										userCol = list(custom = FALSE, color = NULL),
										userLvl = list(custom = FALSE, levels = NULL, equidist = FALSE),
										title = list(user = FALSE, title = ''),
										colkeyLab = list(user = FALSE, label = ''),
										scalebar = list(add = FALSE, pos = 'bottomleft'))

		tkconfigure(bt.nc.MapOpt, command = function(){
			if(!is.null(.cdtData$EnvData$ncData$map)){
				atlevel <- pretty(.cdtData$EnvData$ncData$map$z, n = 10, min.n = 7)
				if(is.null(.cdtData$EnvData$ncMapOp$userLvl$levels)){
					.cdtData$EnvData$ncMapOp$userLvl$levels <- atlevel
				}else{
					if(!.cdtData$EnvData$ncMapOp$userLvl$custom)
						.cdtData$EnvData$ncMapOp$userLvl$levels <- atlevel
				}
			}
			.cdtData$EnvData$ncMapOp <- MapGraph.MapOptions(.cdtData$EnvData$ncMapOp)
		})

		###################

		.cdtData$EnvData$tab$dataNCMap <- NULL

		tkconfigure(bt.nc.maps, command = function(){
			if(str_trim(tclvalue(ncdf.file)) != ""){
				ret <- try(get.NCDF.DATA(), silent = TRUE)
				if(inherits(ret, "try-error") | is.null(ret)) return(NULL)

				tab.title <- paste('Map -', .cdtData$EnvData$ncData$file2plot)
				imgContainer <- CDT.Display.Graph(PlotNetCDFdataMaps, .cdtData$EnvData$tab$dataNCMap, tab.title)
				.cdtData$EnvData$tab$dataNCMap <- imageNotebookTab_unik(imgContainer, .cdtData$EnvData$tab$dataNCMap)
			}
		})

		tkgrid(bt.nc.MapOpt, row = 0, column = 0, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1)
		tkgrid(bt.nc.maps, row = 0, column = 1, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1)

		##############################################

		framePlotType <- tkframe(subfr1)

		.cdtData$EnvData$plot.maps$.data.type <- "Grid"
		plot.type <- c("Pixels", "FilledContour")
		.cdtData$EnvData$plot.maps$plot.type <- tclVar("Pixels")

		txt.plotType <- tklabel(framePlotType, text = "Plot Type", anchor = 'e', justify = 'right')
		cb.plotType <- ttkcombobox(framePlotType, values = plot.type, textvariable = .cdtData$EnvData$plot.maps$plot.type, width = largeur1)

		tkgrid(txt.plotType, row = 0, column = 0, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
		tkgrid(cb.plotType, row = 0, column = 1, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)

		##############################################

		frameBlank <- tkframe(subfr1)

		blankGrid <- tclVar(0)

		chk.grid <- tkcheckbutton(frameBlank, variable = blankGrid, text = 'Blank grid outside the boundaries', anchor = 'w', justify = 'left')
		tkgrid(chk.grid)

		tkbind(chk.grid, "<Button-1>", function(){
			if(tclvalue(blankGrid) == "1"){
				stateSHP <- if(tclvalue(.cdtData$EnvData$shp$add.shp) == "0") "disabled" else "normal"
			}else stateSHP <- "normal"
			tkconfigure(cb.addshp, state = stateSHP)
			tkconfigure(bt.addshp, state = stateSHP)
		})

		##############################################

		frameSHP <- ttklabelframe(subfr1, text = "Boundaries", relief = 'groove')

		.cdtData$EnvData$shp$add.shp <- tclVar(0)
		file.plotShp <- tclVar()
		stateSHP <- "disabled"

		chk.addshp <- tkcheckbutton(frameSHP, variable = .cdtData$EnvData$shp$add.shp, text = "Add boundaries to Map", anchor = 'w', justify = 'left')
		bt.addshpOpt <- ttkbutton(frameSHP, text = .cdtEnv$tcl$lang$global[['button']][['4']], state = stateSHP)
		cb.addshp <- ttkcombobox(frameSHP, values = unlist(listOpenFiles), textvariable = file.plotShp, width = largeur0, state = stateSHP)
		bt.addshp <- tkbutton(frameSHP, text = "...", state = stateSHP)

		########
		tkconfigure(bt.addshp, command = function(){
			shp.opfiles <- getOpenShp(.cdtEnv$tcl$main$win)
			if(!is.null(shp.opfiles)){
				update.OpenFiles('shp', shp.opfiles)
				tclvalue(file.plotShp) <- shp.opfiles[[1]]
				listOpenFiles[[length(listOpenFiles) + 1]] <<- shp.opfiles[[1]]
				lapply(list(cb.ncfl, cb.addshp), tkconfigure, values = unlist(listOpenFiles))

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

		infobulle(cb.addshp, 'Select the file containing the ESRI shapefiles')
		status.bar.display(cb.addshp, 'Select the file containing the ESRI shapefiles')
		infobulle(bt.addshp, 'Browse file if not listed')
		status.bar.display(bt.addshp, 'Browse file if not listed')

		#################
		tkbind(cb.addshp, "<<ComboboxSelected>>", function(){
			shpofile <- getShpOpenData(file.plotShp)
			if(is.null(shpofile))
				.cdtData$EnvData$shp$ocrds <- NULL
			else
				.cdtData$EnvData$shp$ocrds <- getBoundaries(shpofile[[2]])
		})

		tkbind(chk.addshp, "<Button-1>", function(){
			if(tclvalue(.cdtData$EnvData$shp$add.shp) == "1"){
				stateSHP <- if(tclvalue(blankGrid) == "0") "disabled" else "normal"
			}else stateSHP <- "normal"
			tkconfigure(cb.addshp, state = stateSHP)
			tkconfigure(bt.addshp, state = stateSHP)

			stateSHP1 <- if(tclvalue(.cdtData$EnvData$shp$add.shp) == "1") "disabled" else "normal"
			tkconfigure(bt.addshpOpt, state = stateSHP1)
		})

		############################################

		tkgrid(frameNC, row = 0, column = 0, sticky = 'we', padx = 1, pady = 1, ipadx = 1, ipady = 1)
		tkgrid(frameMap, row = 1, column = 0, sticky = '', padx = 1, pady = 5, ipadx = 1, ipady = 1)
		tkgrid(framePlotType, row = 2, column = 0, sticky = '', padx = 1, pady = 3, ipadx = 1, ipady = 1)
		tkgrid(frameBlank, row = 3, column = 0, sticky = 'we', padx = 1, pady = 1, ipadx = 1, ipady = 1)
		tkgrid(frameSHP, row = 4, column = 0, sticky = 'we', padx = 1, pady = 1, ipadx = 1, ipady = 1)

	#######################################################################################################

	get.NCDF.DATA <- function(){
		tkconfigure(.cdtEnv$tcl$main$win, cursor = 'watch')
		tcl('update')
		on.exit({
			tkconfigure(.cdtEnv$tcl$main$win, cursor = '')
			tcl('update')
		})

		loaded.nc <- list(str_trim(tclvalue(ncdf.file)), tclvalue(blankGrid))

		getNCFiles <- TRUE
		if(!is.null(.cdtData$EnvData$loaded.nc))
			getNCFiles <- if(!isTRUE(all.equal(.cdtData$EnvData$loaded.nc, loaded.nc))) TRUE else FALSE

		if(getNCFiles){
			ncdata <- getNcdfOpenData(str_trim(tclvalue(ncdf.file)))
			.cdtData$EnvData$ncData$map$x <- ncdata[[2]]$x
			.cdtData$EnvData$ncData$map$y <- ncdata[[2]]$y

			if(tclvalue(blankGrid) == "1"){
				shpdata <- getShpOpenData(file.plotShp)[[2]]
				if(is.null(shpdata)){
					Insert.Messages.Out("No shapefiles found", format = TRUE)
					return(NULL)
				}
				nc.grid <- list(lon = ncdata[[2]]$x, lat = ncdata[[2]]$y)
				shpdata[['vtmp']] <- 1
				mask <- over(defSpatialPixels(nc.grid), shpdata)[, 'vtmp']
				dim(mask) <- sapply(nc.grid, length)
				.cdtData$EnvData$ncData$map$z <- ncdata[[2]]$z * mask
			}else .cdtData$EnvData$ncData$map$z <- ncdata[[2]]$z

			.cdtData$EnvData$ncData$file2plot <- ncdata[[1]]
			.cdtData$EnvData$loaded.nc <- loaded.nc
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
