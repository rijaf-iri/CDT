
anomaliesCalc.plotAnomMaps <- function(){
	don <- .cdtData$EnvData$anomdata$map
	anomMapOp <- .cdtData$EnvData$anomMapOp

	## titre
	if(!anomMapOp$title$user){
		titre1 <- switch(.cdtData$EnvData$output$params$anomaly,
						"Difference" = "Anomaly:",
						"Percentage" = "Anomaly (% of Mean):",
						"Standardized" = "Standardized Anomaly:")
		titre <- paste(titre1, str_trim(tclvalue(.cdtData$EnvData$anomDate)))
	}else titre <- anomMapOp$title$title

	#################
	## colorscale title
	if(anomMapOp$colkeyLab$user){
		legend.texta <- anomMapOp$colkeyLab$label
	}else legend.texta <- NULL

	#################
	## breaks
	brks <- image.plot_Legend_pars(don$z, anomMapOp$userLvl, anomMapOp$userCol, anomMapOp$presetCol)
	don$z <- don$z + 1e-15
	breaks <- brks$breaks
	zlim <- brks$legend.breaks$zlim
	breaks2 <- brks$legend.breaks$breaks
	kolor <- brks$colors
	breaks1 <- brks$legend.axis$at
	lab.breaks <- brks$legend.axis$labels

	## legend label
	legendLabel <- lab.breaks

	#################
	### shape files
	shpf <- .cdtData$EnvData$shp
	ocrds <- if(tclvalue(shpf$add.shp) == "1" & !is.null(shpf$ocrds)) shpf$ocrds else matrix(NA, 1, 2)

	#################

	if(all(is.na(ocrds[, 1])) | all(is.na(ocrds[, 2]))){
		xlim <- range(don$x, na.rm = TRUE)
		ylim <- range(don$y, na.rm = TRUE)
	}else{
		xlim <- range(range(don$x, na.rm = TRUE), range(ocrds[, 1], na.rm = TRUE))
		ylim <- range(range(don$y, na.rm = TRUE), range(ocrds[, 2], na.rm = TRUE))
	}

	#################

	if(diff(xlim) > diff(ylim)){
		horizontal <- TRUE
		legend.mar <- 3.5
		legend.width <- 0.7
		mar <- c(7, 4, 2.5, 2.5)
		legend.args <- if(!is.null(legend.texta)) list(text = legend.texta, cex = 0.8, side = 1, line = 2) else NULL
	}else{
		horizontal <- FALSE
		legend.mar <- 6.2
		mar <- c(4, 4, 2.5, 6)
		legend.width <- 0.9
		line <- if(max(nchar(as.character(breaks))) > 4) 3 else 2
		legend.args <- if(!is.null(legend.texta)) list(text = legend.texta, cex = 0.8, side = 4, line = line) else NULL
	}

	#################

	opar <- par(mar = mar)
	plot(1, xlim = xlim, ylim = ylim, xlab = "", ylab = "", type = "n", xaxt = 'n', yaxt = 'n')
	axlabs <- LatLonAxisLabels(axTicks(1), axTicks(2))
	axis(side = 1, at = axTicks(1), labels = axlabs$xaxl, tcl = -0.2, cex.axis = 0.8)
	axis(side = 2, at = axTicks(2), labels = axlabs$yaxl, tcl = -0.2, las = 1, cex.axis = 0.8)
	title(main = titre, cex.main = 1, font.main = 2)

	image(don, breaks = breaks, col = kolor, xaxt = 'n', yaxt = 'n', add = TRUE)
	image.plot(zlim = zlim, breaks = breaks2, col = kolor, horizontal = horizontal,
				legend.only = TRUE, legend.mar = legend.mar, legend.width = legend.width,
				legend.args = legend.args, axis.args = list(at = breaks1, labels = legendLabel,
				cex.axis = 0.7, font = 2, tcl = -0.3, mgp = c(0, 0.5, 0)), legend.shrink = 0.8)

	abline(h = axTicks(2), v = axTicks(1), col = "lightgray", lty = 3)
	lines(ocrds[, 1], ocrds[, 2], lwd = .cdtData$EnvData$SHPOp$lwd, col = .cdtData$EnvData$SHPOp$col)

	## scale bar
	if(anomMapOp$scalebar$add){
		if(anomMapOp$scalebar$pos == 'bottomleft') posx <- 0.05
		if(anomMapOp$scalebar$pos == 'bottomcenter') posx <- 0.425
		if(anomMapOp$scalebar$pos == 'bottomright') posx <- 0.75
		posy <- 0.08

		scalebarX <- grconvertX(posx, "npc")
		scalebarY <- grconvertY(posy, "npc")

		map.scale(x = scalebarX, y = scalebarY, relwidth = 0.15, metric = TRUE, ratio = FALSE, cex = 0.7, font = 2)
	}

	plt <- par("plt")
	usr <- par("usr")
	par(opar)
	return(list(par = c(plt, usr)))
}

#######################################

anomaliesCalc.plotAnomGraph <- function(){
	TSGraphOp <- .cdtData$EnvData$TSGraphOp

	if(.cdtData$EnvData$output$params$data.type == "cdtstation"){
		ixy <- which(.cdtData$EnvData$output$data$id == str_trim(tclvalue(.cdtData$EnvData$plot.maps$stnIDTSp)))
		if(length(ixy) == 0){
			Insert.Messages.Out("Station not found", format = TRUE)
			return(NULL)
		}
		don <- .cdtData$EnvData$anomdata$data[, ixy]
		daty <- .cdtData$EnvData$output$data$dates
		.cdtData$EnvData$location <- paste0("Station: ", .cdtData$EnvData$output$data$id[ixy])
		titre3 <- paste0("(", .cdtData$EnvData$output$data$id[ixy], ")")
	}else{
		cdtdataset <- .cdtData$EnvData$cdtdataset
		xlon <- cdtdataset$coords$mat$x
		xlat <- cdtdataset$coords$mat$y
		ilon <- as.numeric(str_trim(tclvalue(.cdtData$EnvData$plot.maps$lonLOC)))
		ilat <- as.numeric(str_trim(tclvalue(.cdtData$EnvData$plot.maps$latLOC)))

		iclo <- findInterval(ilon, xlon)
		ilo <- iclo + (2 * ilon > xlon[iclo] + xlon[iclo + 1])
		icla <- findInterval(ilat, xlat)
		ila <- icla + (2 * ilat > xlat[icla] + xlat[icla + 1])

		if(is.na(ilo) | is.na(ila)){
			Insert.Messages.Out("Coordinates outside of data range", format = TRUE)
			return(NULL)
		}
		ixy <- ilo + length(xlon) * (ila - 1)

		don <- readCdtDatasetChunk.locations(ixy, cdtdataset$fileInfo, cdtdataset, do.par = FALSE)
		don <- as.numeric(don$data[, 1])
		daty <- cdtdataset$dateInfo$date
		.cdtData$EnvData$location <- paste0("Longitude: ", round(ilon, 5), ", Latitude: ", round(ilat, 5))
		titre3 <- ""
	}

	#########

	if(.cdtData$EnvData$output$params$intstep == "daily"){
		titre1 <- "Daily"
		daty <- as.Date(daty, "%Y%m%d")
	}
	if(.cdtData$EnvData$output$params$intstep == "pentad"){
		titre1 <- "Pentad"
		seqtime <- as.Date(daty, "%Y%m%d")
		daty <- as.Date(paste0(format(seqtime, "%Y-%m-"), c(1, 6, 11, 16, 21, 26)[as.numeric(format(seqtime, "%d"))]))
	}
	if(.cdtData$EnvData$output$params$intstep == "dekadal"){
		titre1 <- "Dekadal"
		seqtime <- as.Date(daty, "%Y%m%d")
		daty <- as.Date(paste0(format(seqtime, "%Y-%m-"), c(1, 11, 21)[as.numeric(format(seqtime, "%d"))]))
	}
	if(.cdtData$EnvData$output$params$intstep == "monthly"){
		titre1 <- "Monthly"
		daty <- as.Date(paste0(daty, "01"), "%Y%m%d")
	}

	titre2 <- switch(.cdtData$EnvData$output$params$anomaly,
					"Difference" = "Anomaly",
					"Percentage" = "Anomaly (% of Mean)",
					"Standardized" = "Standardized Anomaly")

	titre <- paste(titre1, titre2, titre3)

	#########

	GRAPHTYPE <- str_trim(tclvalue(.cdtData$EnvData$plot.maps$typeTSp))
	if(GRAPHTYPE == "Line") optsgph <- TSGraphOp$line
	if(GRAPHTYPE == "Bar") optsgph <- TSGraphOp$anomaly

	xlim <- range(daty, na.rm = TRUE)
	if(optsgph$xlim$is.min){
		xx <- strsplit(optsgph$xlim$min, "-")[[1]]
		x3 <- as.numeric(xx[3])
		if(.cdtData$EnvData$output$params$intstep == "pentad"){
			if(is.na(x3) | x3 < 1 | x3 > 6){
				Insert.Messages.Out("xlim: pentad must be  between 1 and 6", format = TRUE)
				return(NULL)
			}
			x3 <- c(1, 6, 11, 16, 21, 26)[x3]
		}
		if(.cdtData$EnvData$output$params$intstep == "dekadal"){
			if(is.na(x3) | x3 < 1 | x3 > 3){
				Insert.Messages.Out("xlim: dekad must be 1, 2 or 3", format = TRUE)
				return(NULL)
			}
			x3 <- c(1, 11, 21)[x3]
		}
		if(.cdtData$EnvData$output$params$intstep == "monthly") x3 <- 1
		x1 <- as.numeric(xx[1])
		x2 <- str_pad(as.numeric(xx[2]), 2, pad = "0")
		x3 <- str_pad(x3, 2, pad = "0")
		xx <- as.Date(paste0(x1, x2, x3), "%Y%m%d")
		if(is.na(xx)){
			Insert.Messages.Out("xlim: invalid date", format = TRUE)
			return(NULL)
		}
		xlim[1] <- xx
	}
	if(optsgph$xlim$is.max){
		xx <- strsplit(optsgph$xlim$max, "-")[[1]]
		x3 <- as.numeric(xx[3])
		if(.cdtData$EnvData$output$params$intstep == "pentad"){
			if(is.na(x3) | x3 < 1 | x3 > 6){
				Insert.Messages.Out("xlim: pentad must be  between 1 and 6", format = TRUE)
				return(NULL)
			}
			x3 <- c(1, 6, 11, 16, 21, 26)[x3]
		}
		if(.cdtData$EnvData$output$params$intstep == "dekadal"){
			if(is.na(x3) | x3 < 1 | x3 > 3){
				Insert.Messages.Out("xlim: dekad must be 1, 2 or 3", format = TRUE)
				return(NULL)
			}
			x3 <- c(1, 11, 21)[x3]
		}
		if(.cdtData$EnvData$output$params$intstep == "monthly") x3 <- 1
		x1 <- as.numeric(xx[1])
		x2 <- str_pad(as.numeric(xx[2]), 2, pad = "0")
		x3 <- str_pad(x3, 2, pad = "0")
		xx <- as.Date(paste0(x1, x2, x3), "%Y%m%d")
		if(is.na(xx)){
			Insert.Messages.Out("xlim: invalid date", format = TRUE)
			return(NULL)
		}
		xlim[2] <- xx
	}
	idt <- daty >= xlim[1] & daty <= xlim[2]
	daty <- daty[idt]
	don <- don[idt]
	ylim <- range(pretty(don))
	if(optsgph$ylim$is.min) ylim[1] <- optsgph$ylim$min
	if(optsgph$ylim$is.max) ylim[2] <- optsgph$ylim$max

	xlab <- if(optsgph$axislabs$is.xlab) optsgph$axislabs$xlab else ''
	ylab <- if(optsgph$axislabs$is.ylab) optsgph$axislabs$ylab else ''

	if(optsgph$title$is.title){
		titre <- optsgph$title$title
		titre.pos <- optsgph$title$position
	}else{
		titre <- titre
		titre.pos <- "top"
	}

	#########

	if(GRAPHTYPE == "Line"){
		graphs.plot.line(daty, don, xlim = xlim, ylim = ylim,
						xlab = xlab, ylab = ylab, ylab.sub = NULL,
						title = titre, title.position = titre.pos, axis.font = 1,
						plotl = optsgph$plot, legends = NULL,
						location = .cdtData$EnvData$location)
	}

	if(GRAPHTYPE == "Bar"){
		loko <- c(optsgph$colors$negative, optsgph$colors$positive)

		graphs.plot.bar.Anomaly(daty, don, period = NULL, percent = FALSE,
								xlim = xlim, ylim = ylim, xlab = xlab, ylab = ylab, ylab.sub = NULL,
								title = titre, title.position = titre.pos, axis.font = 1,
								barcol = loko, location = .cdtData$EnvData$location)
	}
}

##############################################################################

anomaliesCalc.Display.Maps <- function(){
	if(is.null(.cdtData$EnvData)) return(NULL)
	if(is.null(.cdtData$EnvData$output)) return(NULL)

	imgContainer <- CDT.Display.Map.inter(anomaliesCalc.plotAnomMaps, .cdtData$EnvData$tab$AnomMap, 'Anomaly-Map')
	.cdtData$EnvData$tab$AnomMap <- imageNotebookTab_unik(imgContainer, .cdtData$EnvData$tab$AnomMap)

	###############
	tkbind(imgContainer[[2]], "<Button-1>", function(W, x, y){
		if(.cdtData$EnvData$plot.maps$data.type == "cdtstation"){
			xyid <- getIDLatLonCoords(W, x, y, imgContainer[[3]], getStnIDLabel,
							stn.coords = .cdtData$EnvData$plot.maps[c('lon', 'lat', 'id')])
			if(xyid$plotTS)
				tclvalue(.cdtData$EnvData$plot.maps$stnIDTSp) <- xyid$crd
		}else{
			xyid <- getIDLatLonCoords(W, x, y, imgContainer[[3]], getPixelLatlon)
			if(xyid$plotTS){
				tclvalue(.cdtData$EnvData$plot.maps$lonLOC) <- xyid$crd$x
				tclvalue(.cdtData$EnvData$plot.maps$latLOC) <- xyid$crd$y
			}
		}

		if(xyid$plotTS){
			imgContainer1 <- CDT.Display.Graph(anomaliesCalc.plotAnomGraph, .cdtData$EnvData$tab$AnomGraph, 'Anomaly-Graph')
			.cdtData$EnvData$tab$AnomGraph <- imageNotebookTab_unik(imgContainer1, .cdtData$EnvData$tab$AnomGraph)
		}
	})
}
