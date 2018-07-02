
spatialAnalysis.plotStatMaps <- function(){
	don <- .cdtData$EnvData$don
	climMapOp <- .cdtData$EnvData$climMapOp

	## titre
	if(!climMapOp$title$user){
		params <- .cdtData$EnvData$statpars$params
		titre1 <- str_to_title(params$time.series$out.series)
		titre2 <- tclvalue(.cdtData$EnvData$climStat)
		titre3 <- switch(params$analysis.method$mth.fun,
						"percentile" = paste0("(", params$analysis.method$mth.perc, "th", ")"),
						"frequency" = paste0("(", params$analysis.method$low.thres, " < X < ",
												params$analysis.method$up.thres, ")"),
						"trend" = {
							if(params$analysis.method$trend.unit == 1) "per year"
							if(params$analysis.method$trend.unit == 2) "over"
							if(params$analysis.method$trend.unit == 3) "/ average (in %)"
						},
						NULL)
		titre4 <- tclvalue(.cdtData$EnvData$climDate)
		titre <- paste(titre1, titre2, titre3, titre4)
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

	# if(length(xna) > 0) points(xna, yna, pch = '*')
	image(don, breaks = breaks, col = kolor, xaxt = 'n', yaxt = 'n', add = TRUE)
	image.plot(zlim = zlim, breaks = breaks2, col = kolor, horizontal = horizontal,
				legend.only = TRUE, legend.mar = legend.mar, legend.width = legend.width,
				legend.args = legend.args, axis.args = list(at = breaks1, labels = legendLabel,
				cex.axis = 0.7, font = 2, tcl = -0.3, mgp = c(0, 0.5, 0)), legend.shrink = 0.8)

	if(str_trim(tclvalue(.cdtData$EnvData$climStat)) == "Trend"){
		if(.cdtData$EnvData$statpars$params$data.type == "cdtstation"){
			ipvl <- !is.na(don$p.value) & don$p.value < 0.05
			if(any(ipvl)){
				# points(don$x0[ipvl], don$y0[ipvl], col = adjustcolor('gray40', alpha.f = 0.8))
				points(don$x0[ipvl], don$y0[ipvl], pch = 19, cex = 0.5)
			}
		}else{
			ipvl <- c(don$pval)
			ipvl <- !is.na(ipvl) & ipvl < 0.05
			if(any(ipvl)){
				grd <- don$x[2] - don$x[1]
				dd <- expand.grid(x = don$x, y = don$y)
				coordinates(dd) <- ~x+y
				dd <- dd[ipvl, ]
				buffer <- gBuffer(dd, width = grd * 1.02)

				dd <- disaggregate(buffer)
				centr <- coordinates(dd)
				bbx <- lapply(seq_along(dd), function(i) bbox(dd[i]))
				esp <- if(grd > 0.25) 0.25 else grd * 5
				esp <- if(esp > 0.25) 0.25 else esp
				dd <- lapply(seq_along(bbx), function(i){	
					xpt <- c(rev(seq(centr[i, 1], bbx[[i]][1, 1], -esp)[-1]), seq(centr[i, 1], bbx[[i]][1, 2], esp))
					ypt <- c(rev(seq(centr[i, 2], bbx[[i]][2, 1], -esp)[-1]), seq(centr[i, 2], bbx[[i]][2, 2], esp))
					xy <- expand.grid(x = xpt, y = ypt)
					coordinates(xy) <- ~x+y
					ij <- as.logical(over(xy, dd[i]))
					ij[is.na(ij)] <- FALSE
					coordinates(xy[ij, ])
				})
				dd <- do.call(rbind, dd)
				# points(dd[, 1], dd[, 2], pch = 15, cex = 0.3, col = adjustcolor('gray20', alpha.f = 0.9))
				points(dd[, 1], dd[, 2], pch = 15, cex = 0.3)
			}
		}
	}

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

spatialAnalysis.plotTSMaps <- function(){
	TSMapOp <- .cdtData$EnvData$TSMapOp

	if(tclvalue(.cdtData$EnvData$TSData) == "Data")
		don <- .cdtData$EnvData$tsdata
	if(tclvalue(.cdtData$EnvData$TSData) == "Anomaly")
		don <- .cdtData$EnvData$anomData

	if(!TSMapOp$title$user){
		if(str_trim(tclvalue(.cdtData$EnvData$TSData)) == "Data"){
			params <- .cdtData$EnvData$statpars$params
			titre1 <- str_to_title(params$time.series$out.series)
			titre2 <- switch(params$aggr.series$aggr.fun, "sum" = "total", "mean" = "average", "count" = "number")
			titre3 <- if(params$aggr.series$aggr.fun == "count")
							paste("(", params$aggr.series$opr.fun, params$aggr.series$opr.thres, ")") else NULL
			titre4 <- tclvalue(.cdtData$EnvData$TSDate)
			titre <- paste(titre1, titre2, titre3, titre4)
		}

		if(str_trim(tclvalue(.cdtData$EnvData$TSData)) == "Anomaly"){
			params <- don$params
			titre1 <- str_to_title(params$time.series$out.series)
			titre2 <- "anomaly"
			titre3 <- if(params$analysis.method$perc.anom) "% of mean" else NULL
			titre4 <- tclvalue(.cdtData$EnvData$TSDate)
			titre <- paste(titre1, titre2, titre3, titre4)
		}
	}else titre <- TSMapOp$title$title

	#################
	## colorscale title
	if(TSMapOp$colkeyLab$user){
		legend.texta <- TSMapOp$colkeyLab$label
	}else legend.texta <- NULL

	#################
	## breaks
	brks <- image.plot_Legend_pars(don$z, TSMapOp$userLvl, TSMapOp$userCol, TSMapOp$presetCol)
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

	# if(length(xna) > 0) points(xna, yna, pch = '*')
	image(don, breaks = breaks, col = kolor, xaxt = 'n', yaxt = 'n', add = TRUE)
	image.plot(zlim = zlim, breaks = breaks2, col = kolor, horizontal = horizontal,
				legend.only = TRUE, legend.mar = legend.mar, legend.width = legend.width,
				legend.args = legend.args, axis.args = list(at = breaks1, labels = legendLabel,
				cex.axis = 0.7, font = 2, tcl = -0.3, mgp = c(0, 0.5, 0)), legend.shrink = 0.8)

	abline(h = axTicks(2), v = axTicks(1), col = "lightgray", lty = 3)
	lines(ocrds[, 1], ocrds[, 2], lwd = .cdtData$EnvData$SHPOp$lwd, col = .cdtData$EnvData$SHPOp$col)

	## scale bar
	if(TSMapOp$scalebar$add){
		if(TSMapOp$scalebar$pos == 'bottomleft') posx <- 0.05
		if(TSMapOp$scalebar$pos == 'bottomcenter') posx <- 0.425
		if(TSMapOp$scalebar$pos == 'bottomright') posx <- 0.75
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

spatialAnalysis.plotTSGraph <- function(){
	TSGraphOp <- .cdtData$EnvData$TSGraphOp

	if(.cdtData$EnvData$statpars$params$data.type == "cdtstation"){
		ixy <- which(.cdtData$EnvData$tsdata$id == str_trim(tclvalue(.cdtData$EnvData$plot.maps$stnIDTSp)))
		if(length(ixy) == 0){
			Insert.Messages.Out("Station not found", format = TRUE)
			return(NULL)
		}
		don <- .cdtData$EnvData$tsdata$data[, ixy]
		dates <- .cdtData$EnvData$tsdata$date
		daty <- as.numeric(substr(dates, 1, 4))
		.cdtData$EnvData$location <- paste0("Station: ", .cdtData$EnvData$tsdata$id[ixy])
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
		dates <- cdtdataset$dateInfo$date

		######
		year1 <- substr(dates, 1, 4) 
		mon1 <- substr(dates, 6, 7)
		year2 <- substr(dates, 9, 12)
		mon2 <- substr(dates, 14, 15)
		if(all(year1 == year2)){
			if(all(mon1 == mon2)) dateTS <- paste0(year1, mon1)
			else{
				dateTS <- if(mon1 == "01" & mon2 == "12") year1 else dates
			}
		}else dateTS <- dates
		ipos <- which(.cdtData$EnvData$statpars$stats == str_trim(tclvalue(.cdtData$EnvData$climDate)))
		idaty <- dateTS %in% .cdtData$EnvData$statpars$timeseries[[ipos]][[2]]
		dates <- dateTS[idaty]
		don <- don[idaty]

		daty <- as.numeric(substr(dates, 1, 4))
		.cdtData$EnvData$location <- paste0("Longitude: ", round(ilon, 5), ", Latitude: ", round(ilat, 5))
	}

	#########
	GRAPHTYPE <- str_trim(tclvalue(.cdtData$EnvData$plot.maps$typeTSp))

	#### ENSO
	if(GRAPHTYPE %in% c("ENSO-Line", "ENSO-Barplot", "ENSO-Proba")){
		if(nchar(dates[1]) == 4){
			start.mon <- paste0(dates, "0115")
			end.mon <- paste0(dates, "1215")
		}
		if(nchar(dates[1]) == 6){
			start.mon <- paste0(dates, "15")
			end.mon <- paste0(dates, "15")
		}
		if(nchar(dates[1]) == 15){
			dates <- lapply(strsplit(dates, '_'), function(x) format(as.Date(paste0(x, "-15")), "%Y%m%d"))
			start.mon <- sapply(dates, '[[', 1)
			end.mon <- sapply(dates, '[[', 2)
		}

		ijoni <- cdt.index.flexseason(start.mon, end.mon, .cdtData$EnvData$ONI$date, "monthly")
		oni <- sapply(ijoni$index, function(x) mean(.cdtData$EnvData$ONI$data[x], na.rm = TRUE))
		oni[length(ijoni$nba) == 0] <- NA
		oni[is.nan(oni)] <- NA
		oni <- ifelse(oni >= 0.5, 3, ifelse(oni <= -0.5, 1, 2))
	}

	########

	xlab0 <- ""
	ylab0 <- ""

	#########

	optsgph <- switch(GRAPHTYPE,
				"Line" = TSGraphOp$line,
				"Barplot" = TSGraphOp$bar,
				"ENSO-Line" = TSGraphOp$line.enso,
				"ENSO-Barplot" = TSGraphOp$bar.enso,
				"Anomaly" = TSGraphOp$anomaly,
				"Probability" = TSGraphOp$proba,
				"ENSO-Proba" = TSGraphOp$proba.enso)

	## xlim, ylim, xlab, ylab
	if(GRAPHTYPE %in% c("Probability", "ENSO-Proba")){
		xlim <- range(don, na.rm = TRUE)
		if(optsgph$xlim$is.min) xlim[1] <- as.numeric(optsgph$xlim$min)
		if(optsgph$xlim$is.max) xlim[2] <- as.numeric(optsgph$xlim$max)
		ylim <- c(0, 100)
		ylab0 <- "Probability of Exceeding"
	}else{
		xlim <- range(daty, na.rm = TRUE)
		if(optsgph$xlim$is.min) xlim[1] <- as.numeric(optsgph$xlim$min)
		if(optsgph$xlim$is.max) xlim[2] <- as.numeric(optsgph$xlim$max)
		idt <- daty >= xlim[1] & daty <= xlim[2]
		daty <- daty[idt]
		don <- don[idt]
		ylim <- range(pretty(don))
		if(GRAPHTYPE == "Anomaly")
			if(optsgph$anom$perc.anom) ylab0 <- "Anomaly (% of Mean)"
	}

	if(optsgph$ylim$is.min) ylim[1] <- optsgph$ylim$min
	if(optsgph$ylim$is.max) ylim[2] <- optsgph$ylim$max

	xlab <- if(optsgph$axislabs$is.xlab) optsgph$axislabs$xlab else xlab0
	ylab <- if(optsgph$axislabs$is.ylab) optsgph$axislabs$ylab else ylab0

	if(optsgph$title$is.title){
		titre <- optsgph$title$title
		titre.pos <- optsgph$title$position
	}else{
		titre <- ""
		titre.pos <- "top"
	}

	#########

	if(GRAPHTYPE == "Line"){
		legends <- NULL
		if(optsgph$legend$is$mean){
			legends$add$mean <- optsgph$legend$add$mean
			legends$col$mean <- optsgph$legend$col$mean
			legends$text$mean <- optsgph$legend$text$mean
			legends$lwd$mean <- optsgph$legend$lwd$mean
		}else{
			if(tclvalue(.cdtData$EnvData$plot.maps$averageTSp) == "1") legends$add$mean <- TRUE
		}
		if(optsgph$legend$is$linear){
			legends$add$linear <- optsgph$legend$add$linear
			legends$col$linear <- optsgph$legend$col$linear
			legends$text$linear <- optsgph$legend$text$linear
			legends$lwd$linear <- optsgph$legend$lwd$linear
		}else{
			if(tclvalue(.cdtData$EnvData$plot.maps$trendTSp) == "1") legends$add$linear <- TRUE
		}
		if(optsgph$legend$is$tercile){
			legends$add$tercile <- optsgph$legend$add$tercile
			legends$col$tercile1 <- optsgph$legend$col$tercile1
			legends$text$tercile1 <- optsgph$legend$text$tercile1
			legends$col$tercile2 <- optsgph$legend$col$tercile2
			legends$text$tercile2 <- optsgph$legend$text$tercile2
			legends$lwd$tercile <- optsgph$legend$lwd$tercile
		}else{
			if(tclvalue(.cdtData$EnvData$plot.maps$tercileTSp) == "1") legends$add$tercile <- TRUE
		}

		graphs.plot.line(daty, don, xlim = xlim, ylim = ylim,
						xlab = xlab, ylab = ylab, ylab.sub = NULL,
						title = titre, title.position = titre.pos, axis.font = 1,
						plotl = optsgph$plot, legends = legends,
						location = .cdtData$EnvData$location)
	}

	if(GRAPHTYPE == "Barplot"){
		graphs.plot.bar(daty, don, xlim = xlim, ylim = ylim,
						xlab = xlab, ylab = ylab, ylab.sub = NULL,
						title = titre, title.position = titre.pos, axis.font = 1,
						barcol = optsgph$colors$col,
						location = .cdtData$EnvData$location)
	}

	if(GRAPHTYPE == "ENSO-Line"){
		oni <- oni[idt]

		legends <- NULL
		if(optsgph$legend$is$mean){
			legends$add$mean <- optsgph$legend$add$mean
			legends$col$mean <- optsgph$legend$col$mean
			legends$text$mean <- optsgph$legend$text$mean
			legends$lwd$mean <- optsgph$legend$lwd$mean
		}else{
			if(tclvalue(.cdtData$EnvData$plot.maps$averageTSp) == "1") legends$add$mean <- TRUE
		}
		if(optsgph$legend$is$linear){
			legends$add$linear <- optsgph$legend$add$linear
			legends$col$linear <- optsgph$legend$col$linear
			legends$text$linear <- optsgph$legend$text$linear
			legends$lwd$linear <- optsgph$legend$lwd$linear
		}else{
			if(tclvalue(.cdtData$EnvData$plot.maps$trendTSp) == "1") legends$add$linear <- TRUE
		}
		if(optsgph$legend$is$tercile){
			legends$add$tercile <- optsgph$legend$add$tercile
			legends$col$tercile1 <- optsgph$legend$col$tercile1
			legends$text$tercile1 <- optsgph$legend$text$tercile1
			legends$col$tercile2 <- optsgph$legend$col$tercile2
			legends$text$tercile2 <- optsgph$legend$text$tercile2
			legends$lwd$tercile <- optsgph$legend$lwd$tercile
		}else{
			if(tclvalue(.cdtData$EnvData$plot.maps$tercileTSp) == "1") legends$add$tercile <- TRUE
		}

		graphs.plot.line.ENSO(daty, don, oni, xlim = xlim, ylim = ylim,
							xlab = xlab, ylab = ylab, ylab.sub = NULL,
							title = titre, title.position = titre.pos, axis.font = 1,
							plotl = optsgph$plot, legends = legends,
							location = .cdtData$EnvData$location)
	}

	if(GRAPHTYPE == "ENSO-Barplot"){
		oni <- oni[idt]
		graphs.plot.bar.ENSO(daty, don, oni, xlim = xlim, ylim = ylim,
							xlab = xlab, ylab = ylab, ylab.sub = NULL,
							title = titre, title.position = titre.pos, axis.font = 1,
							barcol = optsgph$colors$col, location = .cdtData$EnvData$location)
	}

	if(GRAPHTYPE == "Anomaly"){
		if(!optsgph$ylim$is.min & !optsgph$ylim$is.max) ylim <- NULL
		loko <- c(optsgph$colors$negative, optsgph$colors$positive)

		period <- range(daty, na.rm = TRUE)
		if(optsgph$anom$basePeriod){
			startYr <- optsgph$anom$startYr.anom
			endYr <- optsgph$anom$endYr.anom
			period <- c(startYr, endYr)
		}

		graphs.plot.bar.Anomaly(daty, don, period = period, percent = optsgph$anom$perc.anom,
								xlim = xlim, ylim = ylim, xlab = xlab, ylab = ylab, ylab.sub = NULL,
								title = titre, title.position = titre.pos, axis.font = 1,
								barcol = loko, location = .cdtData$EnvData$location)
	}

	if(GRAPHTYPE == "Probability"){
		graphs.plot.proba(don, xlim = xlim, ylim = ylim,
						xlab = xlab, xlab.sub = NULL, ylab = ylab,
						title = titre, title.position = titre.pos, axis.font = 1,
						proba = list(theoretical = optsgph$proba$theoretical),
						plotp = optsgph$proba, plotl = optsgph$plot,
						location = .cdtData$EnvData$location)
	}

	if(GRAPHTYPE == "ENSO-Proba"){
		graphs.plot.proba.ENSO(don, oni, xlim = xlim, ylim = ylim,
							xlab = xlab, xlab.sub = NULL, ylab = ylab,
							title = titre, title.position = titre.pos, axis.font = 1,
							plotl = optsgph$plot, location = .cdtData$EnvData$location)
	}
}

##############################################################################

spatialAnalysis.DisplayStatMaps <- function(){
	if(is.null(.cdtData$EnvData)) return(NULL)
	if(is.null(.cdtData$EnvData$statpars)) return(NULL)

	.cdtData$EnvData$plot.maps[c('lon', 'lat', 'id')] <- .cdtData$EnvData$don[c('x0', 'y0', 'id')]

	imgContainer <- CDT.Display.Map.inter(spatialAnalysis.plotStatMaps, .cdtData$EnvData$tab$climMap, 'Clim-Analysis-Maps')
	.cdtData$EnvData$tab$climMap <- imageNotebookTab_unik(imgContainer, .cdtData$EnvData$tab$climMap)

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
			imgContainer1 <- CDT.Display.Graph(spatialAnalysis.plotTSGraph, .cdtData$EnvData$tab$TSplot, 'Time-Series-Plot')
			.cdtData$EnvData$tab$TSplot <- imageNotebookTab_unik(imgContainer1, .cdtData$EnvData$tab$TSplot)
		}
	})
}

#######################################

spatialAnalysis.DisplayTSMaps <- function(){
	if(is.null(.cdtData$EnvData)) return(NULL)
	if(is.null(.cdtData$EnvData$statpars)) return(NULL)

	.cdtData$EnvData$plot.maps[c('lon', 'lat', 'id')] <- .cdtData$EnvData$tsdata[c('x0', 'y0', 'id')]

	imgContainer <- CDT.Display.Map.inter(spatialAnalysis.plotTSMaps, .cdtData$EnvData$tab$TSMap, 'Aggregated-Data')
	.cdtData$EnvData$tab$TSMap <- imageNotebookTab_unik(imgContainer, .cdtData$EnvData$tab$TSMap)

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
			imgContainer1 <- CDT.Display.Graph(spatialAnalysis.plotTSGraph, .cdtData$EnvData$tab$TSplot, 'Time-Series-Plot')
			.cdtData$EnvData$tab$TSplot <- imageNotebookTab_unik(imgContainer1, .cdtData$EnvData$tab$TSplot)
		}
	})
}

