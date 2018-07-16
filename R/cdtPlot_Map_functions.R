
cdt.plotmap.args <- function(don, MapOp, shpf, legend.text = NULL, label.fun = identity, ...)
{
	## colorscale title
	if(MapOp$colkeyLab$user){
		legend.texta <- MapOp$colkeyLab$label
		if(str_trim(legend.texta) == "") legend.texta <- NULL
	}else legend.texta <- legend.text

	#################
	## breaks
	brks <- image.plot_Legend_pars(don$z, MapOp$userLvl, MapOp$userCol, MapOp$presetCol)
	don$z <- don$z + 1e-15
	breaks <- brks$breaks
	zlim <- brks$legend.breaks$zlim
	breaks2 <- brks$legend.breaks$breaks
	kolor <- brks$colors
	breaks1 <- brks$legend.axis$at
	lab.breaks <- brks$legend.axis$labels

	## legend label
	legendLabel <- label.fun(lab.breaks, ...)

	#################
	### shape files
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

	list(don = don, horizontal = horizontal, kolor = kolor,
		mar = mar, xlim = xlim, ylim = ylim, zlim = zlim, ocrds = ocrds,
		breaks = breaks, breaks1 = breaks1, breaks2 = breaks2,
		legend.mar = legend.mar, legend.width = legend.width,
		legend.args = legend.args, legendLabel = legendLabel)
}

####################################################################################################

# data.type = c("Points", "Grid")
# data.type = "Points"; plot.type = c("Pixels", "Points")
# data.type = "Grid"; plot.type = c("Pixels", "FilledContour")

cdt.plotmap.fun <- function(don, horizontal, kolor,
							xlim, ylim, zlim,
							breaks, breaks1, breaks2,
							legend.mar, legend.width,
							legend.args, legendLabel,
							titre, ocrds, SHPOp, MapOp = NULL,
							data.type = "Points", plot.type = "Pixels")
{
	plot(1, xlim = xlim, ylim = ylim, xlab = "", ylab = "", type = "n", xaxt = 'n', yaxt = 'n')
	axlabs <- LatLonAxisLabels(axTicks(1), axTicks(2))
	axis(side = 1, at = axTicks(1), labels = axlabs$xaxl, tcl = -0.2, cex.axis = 0.8)
	axis(side = 2, at = axTicks(2), labels = axlabs$yaxl, tcl = -0.2, las = 1, cex.axis = 0.8)
	title(main = titre, cex.main = 1, font.main = 2)

	if(plot.type %in% c("Pixels", "FilledContour")){
		if(plot.type == "Pixels")
			image(don, breaks = breaks, col = kolor, xaxt = 'n', yaxt = 'n', add = TRUE)

		if(data.type == "Grid" & plot.type == "FilledContour")
			.filled.contour(don$x, don$y, don$z, levels = breaks, col = kolor)

		abline(h = axTicks(2), v = axTicks(1), col = "lightgray", lty = 3)
		lines(ocrds[, 1], ocrds[, 2], lwd = SHPOp$lwd, col = SHPOp$col)
	}else{
		if(data.type == "Points" & plot.type == "Points"){
			abline(h = axTicks(2), v = axTicks(1), col = "lightgray", lty = 3)
			lines(ocrds[, 1], ocrds[, 2], lwd = SHPOp$lwd, col = SHPOp$col)

			kolor.p <- kolor[findInterval(don$z, breaks, rightmost.closed = TRUE, left.open = TRUE)]
			points(don$x, don$y, col = kolor.p, cex = MapOp$pointSize, pch = 20)
		}
	}

	image.plot(zlim = zlim, breaks = breaks2, col = kolor, horizontal = horizontal,
				legend.only = TRUE, legend.mar = legend.mar, legend.width = legend.width,
				legend.args = legend.args, axis.args = list(at = breaks1, labels = legendLabel,
				cex.axis = 0.7, font = 2, tcl = -0.3, mgp = c(0, 0.5, 0)), legend.shrink = 0.8)

	plt <- par("plt")
	usr <- par("usr")
	return(list(par = c(plt, usr)))
}

####################################################################################################

cdt.plotmap.scalebar <- function(scalebar)
{
	if(scalebar$add){
		if(scalebar$pos == 'bottomleft') posx <- 0.05
		if(scalebar$pos == 'bottomcenter') posx <- 0.425
		if(scalebar$pos == 'bottomright') posx <- 0.75
		posy <- 0.08

		scalebarX <- grconvertX(posx, "npc")
		scalebarY <- grconvertY(posy, "npc")

		map.scale(x = scalebarX, y = scalebarY, relwidth = 0.15, metric = TRUE, ratio = FALSE, cex = 0.7, font = 2)
	}
}

