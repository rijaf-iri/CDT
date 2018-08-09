
StnChkCoordsPlotMap <- function(){
	MapOp <- .cdtData$EnvData$MapOp
	don <- .cdtData$EnvData$Maps.Disp

	id <- rep(1, nrow(don))
	id[don$StatusX == "orange"] <- 2
	id[don$StatusX == "red"] <- 3
	loko <- c(MapOp$blue$col, MapOp$orange$col, MapOp$red$col)[id]
	pch <- c(MapOp$blue$pch, MapOp$orange$pch, MapOp$red$pch)[id]
	cex <- c(MapOp$blue$cex, MapOp$orange$cex, MapOp$red$cex)[id]

	ocrds <- .cdtData$EnvData$shp$ocrds
	if(is.null(ocrds)) ocrds <- matrix(NA, 1, 2)

	#######
	xmin <- .cdtData$EnvData$ZoomXYval[1]
	xmax <- .cdtData$EnvData$ZoomXYval[2]
	ymin <- .cdtData$EnvData$ZoomXYval[3]
	ymax <- .cdtData$EnvData$ZoomXYval[4]

	if(is.na(xmin) | is.null(xmin) | is.infinite(xmin)){
		Insert.Messages.Out('Longitude min not valid', format = TRUE)
		return(NULL)
	}
	if(is.na(xmax) | is.null(xmax) | is.infinite(xmax)){
		Insert.Messages.Out('Longitude max not valid', format = TRUE)
		return(NULL)
	}
	if(is.na(ymin) | is.null(ymin) | is.infinite(ymin)){
		Insert.Messages.Out('Latitude min not valid', format = TRUE)
		return(NULL)
	}
	if(is.na(ymax) | is.null(ymax) | is.infinite(ymax)){
		Insert.Messages.Out('Latitude max not valid', format = TRUE)
		return(NULL)
	}

	#######
	opar <- par(mar = c(4, 4, 2, 2))
	plot(1, xlim = c(xmin, xmax), ylim = c(ymin, ymax), xlab = "", ylab = "", type = "n", xaxt = 'n', yaxt = 'n')
	lines(ocrds[, 1], ocrds[, 2], lwd = MapOp$shp$lwd, col = MapOp$shp$col)
	abline(h = axTicks(2), v = axTicks(1) , col = "lightgray", lty = 3)
	axlabs <- LatLonAxisLabels(axTicks(1), axTicks(2))
	axis(side = 1, at = axTicks(1), labels = axlabs$xaxl, tck = -0.01, cex.axis = 0.8)
	axis(side = 2, at = axTicks(2), labels = axlabs$yaxl, tck = -0.01, las = 1, cex.axis = 0.8)

	points(don$LonX, don$LatX, col = loko, pch = pch, cex = cex)

	plt <- par("plt")
	usr <- par("usr")
	par(opar)

	return(list(par = c(plt, usr)))
}
