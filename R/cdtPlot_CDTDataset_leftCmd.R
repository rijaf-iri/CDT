
PlotCDTDatasetCmd <- function(){
	listOpenFiles <- openFile_ttkcomboList()
	if(WindowsOS()){
		largeur0 <- .cdtEnv$tcl$fun$w.widgets(28)
		largeur1 <- .cdtEnv$tcl$fun$w.widgets(26)
		largeur2 <- 20
	}else{
		largeur0 <- .cdtEnv$tcl$fun$w.widgets(23)
		largeur1 <- .cdtEnv$tcl$fun$w.widgets(22)
		largeur2 <- 14
	}

	# xml.dlg <- file.path(.cdtDir$dirLocal, "languages", "cdtPlot_CDTDataset_leftCmd.xml")
	# lang.dlg <- cdtLanguageParse(xml.dlg, .cdtData$Config$lang.iso)
	# .cdtData$EnvData$message <- lang.dlg[['message']]

	.cdtData$EnvData$plot.maps$data.type <- 'cdtdataset'

	###################

	.cdtEnv$tcl$main$cmd.frame <- tkframe(.cdtEnv$tcl$main$panel.left)

	tknote.cmd <- bwNoteBook(.cdtEnv$tcl$main$cmd.frame)

	cmd.tab1 <- bwAddTab(tknote.cmd, text = "Plot CDT Dataset")
	cmd.tab2 <- bwAddTab(tknote.cmd, text = "Graphs")

	bwRaiseTab(tknote.cmd, cmd.tab1)

	tkgrid.columnconfigure(cmd.tab1, 0, weight = 1)
	tkgrid.columnconfigure(cmd.tab2, 0, weight = 1)

	tkgrid.rowconfigure(cmd.tab1, 0, weight = 1)
	tkgrid.rowconfigure(cmd.tab2, 0, weight = 1)

	#######################################################################################################

	#Tab1
	subfr1 <- bwTabScrollableFrame(cmd.tab1)

		#######################

		frameData <- ttklabelframe(subfr1, text = "CDT Dataset", relief = 'groove')

		file.index.data <- tclVar()

		txt.cdtdata <- tklabel(frameData, text = 'Index file (*.rds) of the dataset', anchor = 'w', justify = 'left')
		en.cdtdata <- tkentry(frameData, textvariable = file.index.data, width = largeur0)
		bt.cdtdata <- tkbutton(frameData, text = "...")

		tkconfigure(bt.cdtdata, command = function(){
			path.rds <- tclvalue(tkgetOpenFile(initialdir = getwd(), filetypes = .cdtEnv$tcl$data$filetypes6))
			tclvalue(file.index.data) <- if(path.rds %in% c("", "NA") | is.na(path.rds)) "" else path.rds

			ret <- try(get.CDT.dataset.Idx(), silent = TRUE)
			if(inherits(ret, "try-error") | is.null(ret)) return(NULL)
		})

		tkgrid(txt.cdtdata, row = 0, column = 0, sticky = 'we', rowspan = 1, columnspan = 6, padx = 1, pady = 0, ipadx = 1, ipady = 1)
		tkgrid(en.cdtdata, row = 1, column = 0, sticky = 'we', rowspan = 1, columnspan = 5, padx = 0, pady = 0, ipadx = 1, ipady = 1)
		tkgrid(bt.cdtdata, row = 1, column = 5, sticky = 'w', rowspan = 1, columnspan = 1, padx = 0, pady = 0, ipadx = 1, ipady = 1)

		##############################################

		frameSHP <- tkframe(subfr1, relief = 'groove', borderwidth = 2)

		shpFile <- tclVar()
		shpAttr <- tclVar()

		txt.addshp <- tklabel(frameSHP, text = "Shapefile for Administrative Boundaries", anchor = 'w', justify = 'left')
		cb.addshp <- ttkcombobox(frameSHP, values = unlist(listOpenFiles), textvariable = shpFile, width = largeur1)
		bt.addshp <- tkbutton(frameSHP, text = "...")

		txt.attrshp <- tklabel(frameSHP, text = "Attribute field to be displayed", anchor = 'w', justify = 'left')
		cb.attrshp <- ttkcombobox(frameSHP, values = "", textvariable = shpAttr, width = largeur1)

		bt.TableAttr <- ttkbutton(frameSHP, text = "Open Attribute Table")
		bt.MapPixel <- ttkbutton(frameSHP, text = "Display Map")

		########
		tkconfigure(bt.addshp, command = function(){
			shp.opfiles <- getOpenShp(.cdtEnv$tcl$main$win)
			if(!is.null(shp.opfiles)){
				update.OpenFiles('shp', shp.opfiles)
				tclvalue(shpFile) <- shp.opfiles[[1]]
				listOpenFiles[[length(listOpenFiles) + 1]] <<- shp.opfiles[[1]]
				tkconfigure(cb.addshp, values = unlist(listOpenFiles))

				shpf <- getShpOpenData(shpFile)
				if(is.null(shpf)){
					.cdtData$EnvData$shp$data <- NULL
					.cdtData$EnvData$shp$ocrds <- NULL
					return(NULL)
				}

				AttrTable <- names(shpf[[2]]@data)
				tkconfigure(cb.attrshp, values = AttrTable)
				tclvalue(shpAttr) <- AttrTable[1]

				.cdtData$EnvData$shp$data <- shpf
				.cdtData$EnvData$shp$ocrds <- getBoundaries(shpf[[2]])

				.cdtData$EnvData$plot.maps$shp$display <- TRUE
				.cdtData$EnvData$plot.maps$shp$shp <- shpf[[2]]
				.cdtData$EnvData$plot.maps$shp$field <- cb.attrshp
			}else return(NULL)

			ret <- try(get.CDT.dataset.Idx(), silent = TRUE)
			if(inherits(ret, "try-error") | is.null(ret)) return(NULL)
		})

		########

		.cdtData$EnvData$tab$TableAttr <- NULL

		tkconfigure(bt.TableAttr, command = function(){
			shpf <- .cdtData$EnvData$shp$data
			if(!is.null(shpf))
				.cdtData$EnvData$tab$TableAttr <- tableNotebookTab_unik(shpf[[2]]@data, .cdtData$EnvData$tab$TableAttr, shpf[[1]], 10)
		})

		########
		.cdtData$EnvData$tab$MapSelect <- NULL

		tkconfigure(bt.MapPixel, command = function(){
			if(!is.null(.cdtData$EnvData$map) |
				!is.null(.cdtData$EnvData$shp$ocrds))
					CDTdataset.Display.Map()
		})

		########

		tkgrid(txt.addshp, row = 0, column = 0, sticky = 'we', rowspan = 1, columnspan = 8, padx = 1, pady = 1)
		tkgrid(cb.addshp, row = 1, column = 0, sticky = 'we', rowspan = 1, columnspan = 7, padx = 1, pady = 1)
		tkgrid(bt.addshp, row = 1, column = 7, sticky = 'w', rowspan = 1, columnspan = 1, padx = 0, pady = 1)

		tkgrid(txt.attrshp, row = 2, column = 0, sticky = 'we', rowspan = 1, columnspan = 8, padx = 1, pady = 1)
		tkgrid(cb.attrshp, row = 3, column = 0, sticky = 'we', rowspan = 1, columnspan = 7, padx = 1, pady = 1)

		tkgrid(bt.TableAttr, row = 4, column = 0, sticky = 'we', rowspan = 1, columnspan = 4, padx = 1, pady = 1)
		tkgrid(bt.MapPixel, row = 4, column = 4, sticky = 'we', rowspan = 1, columnspan = 8, padx = 1, pady = 1)

		########

		tkbind(cb.addshp, "<<ComboboxSelected>>", function(){
			shpf <- getShpOpenData(shpFile)
			if(is.null(shpf)){
				.cdtData$EnvData$shp$data <- NULL
				.cdtData$EnvData$shp$ocrds <- NULL
				return(NULL)
			}

			AttrTable <- names(shpf[[2]]@data)
			tkconfigure(cb.attrshp, values = AttrTable)
			tclvalue(shpAttr) <- AttrTable[1]

			.cdtData$EnvData$shp$data <- shpf
			.cdtData$EnvData$shp$ocrds <- getBoundaries(shpf[[2]])

			.cdtData$EnvData$plot.maps$shp$display <- TRUE
			.cdtData$EnvData$plot.maps$shp$shp <- shpf[[2]]
			.cdtData$EnvData$plot.maps$shp$field <- cb.attrshp

			ret <- try(get.CDT.dataset.Idx(), silent = TRUE)
			if(inherits(ret, "try-error") | is.null(ret)) return(NULL)
		})

		############################################

		tkgrid(frameData, row = 0, column = 0, sticky = 'we', padx = 1, pady = 1, ipadx = 1, ipady = 1)
		tkgrid(frameSHP, row = 1, column = 0, sticky = 'we', padx = 1, pady = 1, ipadx = 1, ipady = 1)

	#######################################################################################################

	#Tab2
	subfr2 <- bwTabScrollableFrame(cmd.tab2)

		##############################################

		frameGraph <- ttklabelframe(subfr2, text = "Graph", relief = 'groove')

		#################

		frGph1 <- tkframe(frameGraph)

		typeTSPLOT <- c("Line", "Barplot")
		.cdtData$EnvData$plot.maps$typeTSp <- tclVar("Line")

		cb.typeTSp <- ttkcombobox(frGph1, values = typeTSPLOT, textvariable = .cdtData$EnvData$plot.maps$typeTSp, width = largeur2)
		bt.TsGraph.plot <- ttkbutton(frGph1, text = .cdtEnv$tcl$lang$global[['button']][['3']], width = 7)
		bt.TSGraphOpt <- ttkbutton(frGph1, text = .cdtEnv$tcl$lang$global[['button']][['4']], width = 8)

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

		#########

		.cdtData$EnvData$tab$dataGraph <- NULL

		tkconfigure(bt.TsGraph.plot, command = function(){
			if(!is.null(.cdtData$EnvData$cdtdataset)){
				imgContainer <- CDT.Display.Graph(CDTdataset.Plot.Graph, .cdtData$EnvData$tab$dataGraph, "CDT Dataset - TS")
				.cdtData$EnvData$tab$dataGraph <- imageNotebookTab_unik(imgContainer, .cdtData$EnvData$tab$dataGraph)
			}
		})

		#########

		tkgrid(cb.typeTSp, row = 0, column = 0, sticky = 'we', pady = 1, columnspan = 1)
		tkgrid(bt.TSGraphOpt, row = 0, column = 1, sticky = 'we', padx = 4, pady = 1, columnspan = 1)
		tkgrid(bt.TsGraph.plot, row = 0, column = 2, sticky = 'we', pady = 1, columnspan = 1)

		#################

		frGph2 <- tkframe(frameGraph)

		.cdtData$EnvData$plot.maps$lonLOC <- tclVar()
		.cdtData$EnvData$plot.maps$latLOC <- tclVar()

		txt.crdSel <- tklabel(frGph2, text = "Enter longitude and latitude to plot", anchor = 'w', justify = 'left')
		txt.lonLoc <- tklabel(frGph2, text = "Longitude", anchor = 'e', justify = 'right')
		en.lonLoc <- tkentry(frGph2, textvariable = .cdtData$EnvData$plot.maps$lonLOC, width = 8)
		txt.latLoc <- tklabel(frGph2, text = "Latitude", anchor = 'e', justify = 'right')
		en.latLoc <- tkentry(frGph2, textvariable = .cdtData$EnvData$plot.maps$latLOC, width = 8)

		tkgrid(txt.crdSel, row = 0, column = 0, sticky = 'we', rowspan = 1, columnspan = 4, padx = 1, pady = 1, ipadx = 1, ipady = 1)
		tkgrid(txt.lonLoc, row = 1, column = 0, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
		tkgrid(en.lonLoc, row = 1, column = 1, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
		tkgrid(txt.latLoc, row = 1, column = 2, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
		tkgrid(en.latLoc, row = 1, column = 3, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)

		#################

		frGph3 <- tkframe(frameGraph)

		.cdtData$EnvData$plot.maps$lonPAD <- tclVar('0.0')
		.cdtData$EnvData$plot.maps$latPAD <- tclVar('0.0')

		txt.spPAD <- tklabel(frGph3, text = "Spatial Average (Padding)", anchor = 'w', justify = 'left')
		txt.lonPAD <- tklabel(frGph3, text = "Longitude \u00B1", anchor = 'e', justify = 'right')
		en.lonPAD <- tkentry(frGph3, textvariable = .cdtData$EnvData$plot.maps$lonPAD, width = 6)
		txt.latPAD <- tklabel(frGph3, text = "Latitude \u00B1", anchor = 'e', justify = 'right')
		en.latPAD <- tkentry(frGph3, textvariable = .cdtData$EnvData$plot.maps$latPAD, width = 6)

		tkgrid(txt.spPAD, row = 0, column = 0, sticky = 'we', rowspan = 1, columnspan = 4, padx = 1, pady = 1, ipadx = 1, ipady = 1)
		tkgrid(txt.lonPAD, row = 1, column = 0, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
		tkgrid(en.lonPAD, row = 1, column = 1, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
		tkgrid(txt.latPAD, row = 1, column = 2, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
		tkgrid(en.latPAD, row = 1, column = 3, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)


		infobulle(en.lonPAD, 'Add value in decimal degree to get rectangle centered at the target points')
		status.bar.display(en.lonPAD, 'Add value in decimal degree to get rectangle centered at the target points')
		infobulle(en.latPAD, 'Add value in decimal degree to get rectangle centered at the target points')
		status.bar.display(en.latPAD, 'Add value in decimal degree to get rectangle centered at the target points')

		#################
		tkgrid(frGph1, row = 0, column = 0, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
		tkgrid(frGph2, row = 1, column = 0, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
		tkgrid(frGph3, row = 2, column = 0, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)

		############################################

		tkgrid(frameGraph, row = 0, column = 0, sticky = 'we', padx = 1, pady = 1, ipadx = 1, ipady = 1)

	#######################################################################################################

	get.CDT.dataset.Idx <- function(){
		tkconfigure(.cdtEnv$tcl$main$win, cursor = 'watch')
		tcl('update')
		on.exit({
			tkconfigure(.cdtEnv$tcl$main$win, cursor = '')
			tcl('update')
		})

		file.CDT.Idx <- str_trim(tclvalue(file.index.data))
		if(file.CDT.Idx == "") return(NULL)

		read.cdt.dataIdx <- TRUE
		if(!is.null(.cdtData$EnvData$cdtdataset))
			if(!is.null(.cdtData$EnvData$file.CDT.Idx))
				if(.cdtData$EnvData$file.CDT.Idx == file.CDT.Idx) read.cdt.dataIdx <- FALSE

		if(read.cdt.dataIdx){
			if(file.exists(file.CDT.Idx)){
				OutIndexdata <- try(readRDS(file.CDT.Idx), silent = TRUE)
				if(inherits(OutIndexdata, "try-error")){
					Insert.Messages.Out('Unable to load the dataset', format = TRUE)
					Insert.Messages.Out(gsub('[\r\n]', '', OutIndexdata[1]), format = TRUE)
					.cdtData$EnvData$cdtdataset <- NULL
					return(NULL)
				}
				.cdtData$EnvData$cdtdataset <- OutIndexdata
				.cdtData$EnvData$cdtdataset$fileInfo <- file.CDT.Idx
				.cdtData$EnvData$file.CDT.Idx <- file.CDT.Idx

				####
				chunkfile <- sort(unique(OutIndexdata$colInfo$index))
				chunkcalc <- split(chunkfile, ceiling(chunkfile / OutIndexdata$chunkfac))
				do.parChunk <- if(OutIndexdata$chunkfac > length(chunkcalc)) TRUE else FALSE
				do.parCALC <- if(do.parChunk) FALSE else TRUE

				is.parallel <- doparallel(do.parCALC & (length(chunkcalc) > 10))
				`%parLoop%` <- is.parallel$dofun
				moy <- foreach(jj = seq_along(chunkcalc), .packages = "doParallel") %parLoop% {
					don.data <- readCdtDatasetChunk.sequence(chunkcalc[[jj]], file.CDT.Idx, do.par = do.parChunk)
					don.data <- don.data[OutIndexdata$dateInfo$index, , drop = FALSE]
					moy <- colMeans(don.data, na.rm = TRUE)
					moy[is.nan(moy) | is.infinite(moy)] <- NA
					return(moy)
				}
				if(is.parallel$stop) stopCluster(is.parallel$cluster)

				moy <- do.call(c, moy)
				moy <- moy[OutIndexdata$colInfo$order]
				dim(moy) <- sapply(OutIndexdata$coords$mat, length)
				.cdtData$EnvData$map <- c(OutIndexdata$coords$mat, list(z = moy))
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
