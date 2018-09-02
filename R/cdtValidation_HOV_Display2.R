
HOValidation.plotStatMaps <- function(){
	mapstat <- str_trim(tclvalue(.cdtData$EnvData$statistics))
	istat <- which(.cdtData$EnvData$Statistics$STN$description == mapstat)
	don <- .cdtData$EnvData$Statistics$STN$statistics[istat, ]

	dataMapOp <- .cdtData$EnvData$statMapOp
	typeMap <- str_trim(tclvalue(.cdtData$EnvData$typeMap))

	# xna <- .cdtData$EnvData$opDATA$lon[is.na(don)]
	# yna <- .cdtData$EnvData$opDATA$lat[is.na(don)]

	xx <- .cdtData$EnvData$opDATA$lon
	yy <- .cdtData$EnvData$opDATA$lat
	if(typeMap == "Pixels"){
		nx <- nx_ny_as.image(diff(range(xx)))
		ny <- nx_ny_as.image(diff(range(yy)))
		don <- cdt.as.image(don, pts.xy = cbind(xx, yy), nx = nx, ny = ny)
	}else{
		don <- list(x = xx, y = yy, z = don)
	}

	map.args <- cdt.plotmap.args0(don, user.levels = dataMapOp$userLvl,
										user.colors = dataMapOp$userCol,
										preset.colors = dataMapOp$presetCol)

	ocrds <- .cdtData$EnvData$shp

	#################

	xlim <- .cdtData$EnvData$xlim.maps
	ylim <- .cdtData$EnvData$ylim.maps

	opar <- par(mar = map.args$mar)
	plot(1, xlim = xlim, ylim = ylim, xlab = "", ylab = "", type = "n", xaxt = 'n', yaxt = 'n')
	axlabs <- LatLonAxisLabels(axTicks(1), axTicks(2))

	if(tclvalue(.cdtData$EnvData$add.dem) == "1" & !is.null(.cdtData$EnvData$dem)){
		image(.cdtData$EnvData$dem$elv, col = gray.colors(256), add = TRUE)
		# image(.cdtData$EnvData$dem$hill, col = gray.colors(256), add = TRUE)
	}

	axis(side = 1, at = axTicks(1), labels = axlabs$xaxl, tck = -0.01, cex.axis = 0.8)
	axis(side = 2, at = axTicks(2), labels = axlabs$yaxl, tck = -0.01, las = 1, cex.axis = 0.8)
	title(main = mapstat, cex.main = 1, font.main = 2)

	abline(h = axTicks(2), v = axTicks(1), col = "lightgray", lty = 3)
	if(tclvalue(.cdtData$EnvData$add.shp) == "1") lines(ocrds[, 1], ocrds[, 2], lwd = 1.5)

	if(typeMap == "Points"){
		kolor.p <- map.args$kolor[findInterval(map.args$don$z, map.args$breaks, rightmost.closed = TRUE, left.open = TRUE)]
		points(map.args$don$x, map.args$don$y, col = kolor.p, cex = dataMapOp$pointSize, pch = 20)
	}else{
		image(map.args$don, breaks = map.args$breaks, col = map.args$kolor, xaxt = 'n', yaxt = 'n', add = TRUE)
	}

	# if(length(xna) > 0) points(xna, yna, pch = '*')

	image.plot(zlim = map.args$zlim, breaks = map.args$breaks2, col = map.args$kolor, horizontal = map.args$horizontal,
				legend.only = TRUE, legend.mar = map.args$legend.mar, legend.width = map.args$legend.width,
				legend.args = NULL, axis.args = list(at = map.args$breaks1, labels = map.args$labels,
				cex.axis = 0.7, font = 2, tcl = -0.3, mgp = c(0, 0.5, 0)), legend.shrink = 0.8)

	plt <- par("plt")
	usr <- par("usr")
	par(opar)
	return(list(par = c(plt, usr)))
}

###############################

HOValidation.plotGraph <- function(){
	if(.cdtData$EnvData$GeneralParameters$stat.data == 'all'){
		x <- c(.cdtData$EnvData$opDATA$stnStatData)
		y <- c(.cdtData$EnvData$opDATA$ncStatData)
		title <- "All Data"
	}
	if(.cdtData$EnvData$GeneralParameters$stat.data == 'avg'){
		x <- rowMeans(.cdtData$EnvData$opDATA$stnStatData, na.rm = TRUE)
		y <- rowMeans(.cdtData$EnvData$opDATA$ncStatData, na.rm = TRUE)
		title <- "Spatial Average"
	}
	if(.cdtData$EnvData$GeneralParameters$stat.data == 'stn'){
		istn <- which(.cdtData$EnvData$opDATA$id == tclvalue(.cdtData$EnvData$stnIDGraph))
		x <- .cdtData$EnvData$opDATA$stnStatData[, istn]
		y <- .cdtData$EnvData$opDATA$ncStatData[, istn]
		title <- tclvalue(.cdtData$EnvData$stnIDGraph)
	}

	##############
	AggrSeries <- .cdtData$EnvData$opDATA$AggrSeries
	if(AggrSeries$aggr.fun == "count"){
		units <- paste0("(Number of day ", AggrSeries$opr.fun, " ", AggrSeries$opr.thres, ")")
	}else{
		units <- if(.cdtData$EnvData$GeneralParameters$clim.var == "RR") "(mm)" else "(°C)"
	}

	##############
	plotType <- tclvalue(.cdtData$EnvData$type.graph)

	## choix xlim&ylim default
	xmin <- min(c(x, y), na.rm = TRUE)
	xmin <- ifelse(is.infinite(xmin), 0, xmin)
	xmax <- max(c(x, y), na.rm = TRUE)
	xmax <- ifelse(is.infinite(xmax), 0, xmax)

	if(plotType == "Scatter"){
		xlim <- c(xmin, xmax)
		ylim <- c(xmin, xmax)

		xlab <- paste('Station', units)
		ylab <- paste('Estimate', units)

		legendlab <- NA
	}
	if(plotType == "CDF"){
		xlim <- c(xmin, xmax)
		ylim <- c(0, 1)

		xlab <- if(.cdtData$EnvData$GeneralParameters$clim.var == "RR") "Rainfall" else "Temperature"
		xlab <- paste(xlab, units)
		ylab <- "Cumulative density"

		legendlab <- c('Station', 'Estimate')
	}
	if(plotType == "Lines"){
		xlim <- range(.cdtData$EnvData$opDATA$temps, na.rm = TRUE)
		ylim <- c(xmin, xmax)

		xlab <- ""
		ylab <- if(.cdtData$EnvData$GeneralParameters$clim.var == "RR") "Rainfall" else "Temperature"
		ylab <- paste(ylab, units)

		legendlab <- c('Station', 'Estimate')
	}

	##############

	if(plotType == "Scatter"){
		plot(1, xlim = xlim, ylim = ylim, type = 'n', xlab = xlab, ylab = ylab, main = title)
		abline(h = axTicks(2), col = "lightgray", lty = "dotted")
		abline(v = axTicks(1), col = "lightgray", lty = "dotted")
		points(x, y, pch = 20, col = 'grey10', cex = 0.7)
		abline(a = 0, b = 1, lwd = 2, col = 'red')
	}

	if(plotType == "CDF"){
		plot(1, xlim = xlim, ylim = ylim, type = 'n', xlab = xlab, ylab = ylab, main = title)
		abline(h = axTicks(2), col = "lightgray", lty = "dotted")
		abline(v = axTicks(1), col = "lightgray", lty = "dotted")

		if(any(!is.na(x)) & any(!is.na(y))){
			xax <- seq(min(c(x, y), na.rm = TRUE), max(c(x, y), na.rm = TRUE), length.out = 1000)
			fx <- ecdf(x)
			fy <- ecdf(y)
			lines(xax, fx(xax), lwd = 2, col = 'blue', type = 'l')
			lines(xax, fy(xax), lwd = 2, col = 'red', type = 'l')
		}
		legend('bottomright', legendlab, col = c('blue', 'red'), lwd = 3, bg = 'lightgoldenrodyellow')
	}

	if(plotType == "Lines"){
		layout(matrix(1:2, ncol = 1), widths = 1, heights = c(0.9, 0.1), respect = FALSE)
		op <- par(mar = c(3, 4, 2, 2))
		plot(.cdtData$EnvData$opDATA$temps, x, xlim = xlim, ylim = ylim, type = 'n', xaxt = 'n', xlab = xlab, ylab = ylab, main = title)

		abline(h = axTicks(2), col = "lightgray", lty = "dotted")

		xTck <- axTicks.Date(.cdtData$EnvData$opDATA$temps, 1)
		if(as.numeric(diff(xlim)) > 1095){
			xminor <- seq(as.Date(paste0(format(xlim[1], "%Y"), "-01-01")),
						as.Date(paste0(as.numeric(format(xlim[2], "%Y")) + 1, "-01-01")), "year")
			xminor <- xminor[!xminor %in% xTck]
		}else xminor <- NULL
		abline(v = xTck, col = "lightgray", lty = "dotted")
		axis.Date(1, at = xTck, font = 1, cex.axis = 1)
		if(length(xminor) > 0) axis.Date(1, at = xminor, labels = NA, tcl = par("tcl") * 0.5)

		lines(.cdtData$EnvData$opDATA$temps, x, lwd = 2, col = 'blue', type = 'l')
		lines(.cdtData$EnvData$opDATA$temps, y, lwd = 2, col = 'red', type = 'l')
		par(op)

		op <- par(mar = c(0, 4, 0, 2))
		plot.new()
		legend('top', 'groups', legend = legendlab, col = c('blue', 'red'), lwd = 3, lty = 1, horiz = TRUE)
		par(op)
	}
}

###############################

HOValidation.DisplayStatMaps <- function(){
	if(is.null(.cdtData$EnvData)) return(NULL)
	if(is.null(.cdtData$EnvData$opDATA)) return(NULL)

	imgContainer <- CDT.Display.Map.inter(HOValidation.plotStatMaps, .cdtData$EnvData$tab$Maps, 'Statistics-Maps')
	.cdtData$EnvData$tab$Maps <- imageNotebookTab_unik(imgContainer, .cdtData$EnvData$tab$Maps)

	###############
	tkbind(imgContainer[[2]], "<Button-1>", function(W, x, y){
		xyid <- getIDLatLonCoords(W, x, y, imgContainer[[3]], getStnIDLabel,
							stn.coords = .cdtData$EnvData$plot.maps[c('lon', 'lat', 'id')])

		if(xyid$plotTS){
			imgContainer1 <- CDT.Display.Graph(HOValidation.plotGraph, .cdtData$EnvData$tab$Graph, 'Validation-Plot')
			.cdtData$EnvData$tab$Graph <- imageNotebookTab_unik(imgContainer1, .cdtData$EnvData$tab$Graph)
		}
	})
}
