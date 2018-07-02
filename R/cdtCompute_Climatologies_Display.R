
climatologiesCalc.plotClimMaps <- function(){
	don <- .cdtData$EnvData$climdata$map
	climMapOp <- .cdtData$EnvData$climMapOp

	## titre
	if(!climMapOp$title$user){
		titre2 <- if(.cdtData$EnvData$climdata$Var == "CDTMEAN") "mean" else "standard deviation"
		year1 <- .cdtData$EnvData$output$params$climato$start
		year2 <- .cdtData$EnvData$output$params$climato$end
		titre3 <- paste0("(", year1, "-", year2, ")")

		if(.cdtData$EnvData$output$params$intstep == "daily"){
			titre1 <- "Daily"
			titre4 <- "for"
			seqtime <- format(seq(as.Date('2015-1-1'), as.Date('2015-12-31'), 'day'), "%B %d")
		}
		if(.cdtData$EnvData$output$params$intstep == "pentad"){
			titre1 <- "Pentad"
			titre4 <- "for the"
			seqtime <- seq(as.Date('2015-1-1'), as.Date('2015-12-31'), 'day')
			pen <- findInterval(as.numeric(format(seqtime, "%d")), c(1, 5, 10, 15, 20, 25, 31), rightmost.closed = TRUE, left.open = TRUE)
			seqtime <- as.Date(names(split(seq_along(seqtime), paste0(format(seqtime, "%Y-%m-"), pen))))
			ordNum <- c('1st', '2nd', '3rd', '4th', '5th', '6th')
			seqtime <- paste(ordNum[as.numeric(format(seqtime, "%d"))], "pentad of", format(seqtime, "%B"))
		}
		if(.cdtData$EnvData$output$params$intstep == "dekadal"){
			titre1 <- "Dekadal"
			titre4 <- "for the"
			seqtime <- seq(as.Date('2015-1-1'), as.Date('2015-12-31'), 'day')
			dek <- findInterval(as.numeric(format(seqtime, "%d")), c(1, 10, 20, 31), rightmost.closed = TRUE, left.open = TRUE)
			seqtime <- as.Date(names(split(seq_along(seqtime), paste0(format(seqtime, "%Y-%m-"), dek))))
			ordNum <- c('1st', '2nd', '3rd')
			seqtime <- paste(ordNum[as.numeric(format(seqtime, "%d"))], "dekad of", format(seqtime, "%B"))
		}
		if(.cdtData$EnvData$output$params$intstep == "monthly"){
			titre1 <- "Monthly"
			titre4 <- "for"
			seqtime <- format(seq(as.Date('2015-1-1'), as.Date('2015-12-31'), 'month'), "%B")
		}

		ipos <- as.numeric(str_trim(tclvalue(.cdtData$EnvData$climDate)))
		titre5 <- seqtime[ipos]
		titre <- paste(titre1, titre2, titre3, titre4, titre5)
	}else titre <- climMapOp$title$title

	#################
	## colorscale title
	if(climMapOp$colkeyLab$user){
		legend.texta <- climMapOp$colkeyLab$label
	}else legend.texta <- NULL

	#################
	## breaks
	brks <- image.plot_Legend_pars(don$z, climMapOp$userLvl, climMapOp$userCol, climMapOp$presetCol)
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
	if(climMapOp$scalebar$add){
		if(climMapOp$scalebar$pos == 'bottomleft') posx <- 0.05
		if(climMapOp$scalebar$pos == 'bottomcenter') posx <- 0.425
		if(climMapOp$scalebar$pos == 'bottomright') posx <- 0.75
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

climatologiesCalc.plotClimGraph <- function(){
	TSGraphOp <- .cdtData$EnvData$TSGraphOp

	if(.cdtData$EnvData$output$params$data.type == "cdtstation"){
		ixy <- which(.cdtData$EnvData$output$data$id == str_trim(tclvalue(.cdtData$EnvData$plot.maps$stnIDTSp)))
		if(length(ixy) == 0){
			Insert.Messages.Out(.cdtData$EnvData[['message']][['12']], format = TRUE)
			return(NULL)
		}
		don <- .cdtData$EnvData$climdata$data[, ixy]
		idaty <- .cdtData$EnvData$output$index
		.cdtData$EnvData$location <- paste0("Station: ", .cdtData$EnvData$output$data$id[ixy])
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
			Insert.Messages.Out(.cdtData$EnvData[['message']][['13']], format = TRUE)
			return(NULL)
		}
		ixy <- ilo + length(xlon) * (ila - 1)

		don <- readCdtDatasetChunk.locations(ixy, cdtdataset$fileInfo, cdtdataset, do.par = FALSE)
		don <- as.numeric(don$data[, 1])
		idaty <- cdtdataset$dateInfo$date
		.cdtData$EnvData$location <- paste0("Longitude: ", round(ilon, 5), ", Latitude: ", round(ilat, 5))
	}

	#########

	if(.cdtData$EnvData$output$params$intstep == "daily"){
		titre1 <- "Daily"
		seqtime <- seq(as.Date('2015-1-1'), as.Date('2015-12-31'), 'day')
	}
	if(.cdtData$EnvData$output$params$intstep == "pentad"){
		titre1 <- "Pentad"
		seqtime <- seq(as.Date('2015-1-1'), as.Date('2015-12-31'), 'day')
		pen <- findInterval(as.numeric(format(seqtime, "%d")), c(1, 5, 10, 15, 20, 26, 31), rightmost.closed = TRUE, left.open = TRUE)
		seqtime <- as.Date(names(split(seq_along(seqtime), paste0(format(seqtime, "%Y-%m-"), pen))))
		seqtime <- as.Date(paste0(format(seqtime, "%Y-%m-"), c(1, 6, 11, 16, 21, 26)[as.numeric(format(seqtime, "%d"))]))
	}
	if(.cdtData$EnvData$output$params$intstep == "dekadal"){
		titre1 <- "Dekadal"
		seqtime <- seq(as.Date('2015-1-1'), as.Date('2015-12-31'), 'day')
		dek <- findInterval(as.numeric(format(seqtime, "%d")), c(1, 10, 20, 31), rightmost.closed = TRUE, left.open = TRUE)
		seqtime <- as.Date(names(split(seq_along(seqtime), paste0(format(seqtime, "%Y-%m-"), dek))))
		seqtime <- as.Date(paste0(format(seqtime, "%Y-%m-"), c(1, 11, 21)[as.numeric(format(seqtime, "%d"))]))
	}
	if(.cdtData$EnvData$output$params$intstep == "monthly"){
		titre1 <- "Monthly"
		seqtime <- seq(as.Date('2015-1-1'), as.Date('2015-12-31'), 'month')
	}
	daty <- seqtime[idaty]

	titre2 <- if(.cdtData$EnvData$climdata$Var == "CDTMEAN") "mean" else "standard deviation"
	titre <- paste(titre1, titre2)

	#########

	GRAPHTYPE <- str_trim(tclvalue(.cdtData$EnvData$plot.maps$typeTSp))
	if(GRAPHTYPE == "Line") optsgph <- TSGraphOp$line
	if(GRAPHTYPE == "Barplot") optsgph <- TSGraphOp$bar

	xlim <- range(daty, na.rm = TRUE)
	if(optsgph$xlim$is.min){
		xx <- strsplit(optsgph$xlim$min, "-")[[1]]
		x1 <- as.numeric(xx[1])
		if(is.na(x1) | x1 < 1 | x1 > 12){
			Insert.Messages.Out(.cdtData$EnvData[['message']][['14']], format = TRUE)
			return(NULL)
		}
		x2 <- as.numeric(xx[2])
		if(.cdtData$EnvData$output$params$intstep == "pentad"){
			if(is.na(x2) | x2 < 1 | x2 > 6){
				Insert.Messages.Out(.cdtData$EnvData[['message']][['15']], format = TRUE)
				return(NULL)
			}
			x2 <- c(1, 6, 11, 16, 21, 26)[x2]
		}
		if(.cdtData$EnvData$output$params$intstep == "dekadal"){
			if(is.na(x2) | x2 < 1 | x2 > 3){
				Insert.Messages.Out(.cdtData$EnvData[['message']][['16']], format = TRUE)
				return(NULL)
			}
			x2 <- c(1, 11, 21)[x2]
		}
		if(.cdtData$EnvData$output$params$intstep == "monthly") x2 <- 1
		x1 <- str_pad(x1, 2, pad = "0")
		x2 <- str_pad(x2, 2, pad = "0")
		xx <- as.Date(paste0(2015, x1, x2), "%Y%m%d")
		if(is.na(xx)){
			Insert.Messages.Out(.cdtData$EnvData[['message']][['17']], format = TRUE)
			return(NULL)
		}
		xlim[1] <- xx
	}
	if(optsgph$xlim$is.max){
		xx <- strsplit(optsgph$xlim$max, "-")[[1]]
		x1 <- as.numeric(xx[1])
		if(is.na(x1) | x1 < 1 | x1 > 12){
			Insert.Messages.Out(.cdtData$EnvData[['message']][['14']], format = TRUE)
			return(NULL)
		}
		x2 <- as.numeric(xx[2])
		if(.cdtData$EnvData$output$params$intstep == "pentad"){
			if(is.na(x2) | x2 < 1 | x2 > 6){
				Insert.Messages.Out(.cdtData$EnvData[['message']][['15']], format = TRUE)
				return(NULL)
			}
			x2 <- c(1, 6, 11, 16, 21, 26)[x2]
		}
		if(.cdtData$EnvData$output$params$intstep == "dekadal"){
			if(is.na(x2) | x2 < 1 | x2 > 3){
				Insert.Messages.Out(.cdtData$EnvData[['message']][['16']], format = TRUE)
				return(NULL)
			}
			x2 <- c(1, 11, 21)[x2]
		}
		if(.cdtData$EnvData$output$params$intstep == "monthly") x2 <- 1
		x1 <- str_pad(x1, 2, pad = "0")
		x2 <- str_pad(x2, 2, pad = "0")
		xx <- as.Date(paste0(2015, x1, x2), "%Y%m%d")
		if(is.na(xx)){
			Insert.Messages.Out(.cdtData$EnvData[['message']][['17']], format = TRUE)
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

	if(GRAPHTYPE == "Barplot"){
		graphs.plot.bar(daty, don, xlim = xlim, ylim = ylim,
						xlab = xlab, ylab = ylab, ylab.sub = NULL,
						title = titre, title.position = titre.pos, axis.font = 1,
						barcol = optsgph$colors$col,
						location = .cdtData$EnvData$location)
	}
}

##############################################################################

climatologiesCalc.Display.Maps <- function(){
	if(is.null(.cdtData$EnvData)) return(NULL)
	if(is.null(.cdtData$EnvData$output)) return(NULL)

	imgContainer <- CDT.Display.Map.inter(climatologiesCalc.plotClimMaps, .cdtData$EnvData$tab$ClimMap, 'Climatology-Map')
	.cdtData$EnvData$tab$ClimMap <- imageNotebookTab_unik(imgContainer, .cdtData$EnvData$tab$ClimMap)

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
			imgContainer1 <- CDT.Display.Graph(climatologiesCalc.plotClimGraph, .cdtData$EnvData$tab$ClimGraph, 'Climatology-Graph')
			.cdtData$EnvData$tab$ClimGraph <- imageNotebookTab_unik(imgContainer1, .cdtData$EnvData$tab$ClimGraph)
		}
	})
}

