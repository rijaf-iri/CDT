
dailyRainAnalysis.plotMapVarStats <- function(){
	don <- .cdtData$EnvData$statData$map
	dataMapOp <- .cdtData$EnvData$varstatMapOp

	## titre
	if(!dataMapOp$title$user){
		varstats <- .cdtData$EnvData$output$exist.vars.dates
		this.var <- .cdtData$EnvData$now$this.vars
		this.stat <- .cdtData$EnvData$now$this.stats

		infos <- varstats[[this.var]][[this.stat]]

		titre1 <- switch(this.var,
						"TOTALRAIN" = 'Total Rainfall',
						"RAININT" = 'Rainfall Intensity',
						"WETDAY" = 'Number of Wet Days',
						"DRYDAY" = 'Number of Dry Days',
						"WETSPELL" = 'Number of Wet Spells',
						"DRYSPELL" = 'Number of Dry Spells')

		var.def <- switch(this.var, "TOTALRAIN" = '', "RAININT" = '',
						"WETDAY" = paste0('(RR  >= ', infos$pars[1], ' mm)'),
						"DRYDAY" = paste0('(RR  < ', infos$pars[1], ' mm)'),
						"WETSPELL" = paste0('(spell: ', infos$pars[2], ' days)'),
						"DRYSPELL" = paste0('(spell: ', infos$pars[2], ' days)'))

		titre2 <- switch(this.stat,
						'mean' = 'Mean',
						'stdev' = 'Standard deviation',
						'coefvar' = 'Coefficient of variation',
						'proba' = 'Probability of exceeding')

		units <- switch(this.var, "TOTALRAIN" = 'mm', "RAININT" = 'mm/day', "WETDAY" = 'days',
						"DRYDAY" = 'days', "WETSPELL" = 'spells', "DRYSPELL" = 'spells')

		proba.def <- switch(this.stat, 'mean' = '', 'stdev' = '', 'coefvar' = '',
							'proba' = paste0('(', infos$pars[3], " ", units, ')'))

		period.def <- paste0(infos$year[1, 1], '_', infos$year[2, 2], '/', 
					paste0(format(as.Date(paste0("2000-", strsplit(infos$season, "_")[[1]])), "%b-%d"), collapse = "_"))
		period.def <- paste0("[", period.def, "]")

		titre <- paste(titre1, var.def,";" , titre2, proba.def, period.def)
	}else titre <- dataMapOp$title$title

	#################
	## colorscale title
	if(dataMapOp$colkeyLab$user){
		legend.texta <- dataMapOp$colkeyLab$label
	}else legend.texta <- NULL

	#################
	## breaks
	brks <- image.plot_Legend_pars(don$z, dataMapOp$userLvl, dataMapOp$userCol, dataMapOp$presetCol)
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
	if(dataMapOp$scalebar$add){
		if(dataMapOp$scalebar$pos == 'bottomleft') posx <- 0.05
		if(dataMapOp$scalebar$pos == 'bottomcenter') posx <- 0.425
		if(dataMapOp$scalebar$pos == 'bottomright') posx <- 0.75
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

dailyRainAnalysis.plotMapVarTS <- function(){
	don <- .cdtData$EnvData$tsData$map
	dataMapOp <- .cdtData$EnvData$dataMapOp

	## titre
	if(!dataMapOp$title$user){
		varstats <- .cdtData$EnvData$output$exist.vars.dates
		this.var <- .cdtData$EnvData$now$this.vars
		this.stat <- .cdtData$EnvData$now$this.stats
		this.daty <- str_trim(tclvalue(.cdtData$EnvData$donDate))

		infos <- varstats[[this.var]][[this.stat]]

		titre1 <- switch(this.var,
						"TOTALRAIN" = 'Total Rainfall',
						"RAININT" = 'Rainfall Intensity',
						"WETDAY" = 'Number of Wet Days',
						"DRYDAY" = 'Number of Dry Days',
						"WETSPELL" = 'Number of Wet Spells',
						"DRYSPELL" = 'Number of Dry Spells')

		var.def <- switch(this.var, "TOTALRAIN" = '', "RAININT" = '',
						"WETDAY" = paste0('(RR  >= ', infos$pars[1], ' mm)'),
						"DRYDAY" = paste0('(RR  < ', infos$pars[1], ' mm)'),
						"WETSPELL" = paste0('(spell: ', infos$pars[2], ' days)'),
						"DRYSPELL" = paste0('(spell: ', infos$pars[2], ' days)'))

		titre <- paste(titre1, var.def, paste0("[", this.daty, "]"))
	}else titre <- dataMapOp$title$title

	#################
	## colorscale title
	if(dataMapOp$colkeyLab$user){
		legend.texta <- dataMapOp$colkeyLab$label
	}else legend.texta <- NULL

	#################
	## breaks
	brks <- image.plot_Legend_pars(don$z, dataMapOp$userLvl, dataMapOp$userCol, dataMapOp$presetCol)
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
	if(dataMapOp$scalebar$add){
		if(dataMapOp$scalebar$pos == 'bottomleft') posx <- 0.05
		if(dataMapOp$scalebar$pos == 'bottomcenter') posx <- 0.425
		if(dataMapOp$scalebar$pos == 'bottomright') posx <- 0.75
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

dailyRainAnalysis.plotVarGraph <- function(){
	TSGraphOp <- .cdtData$EnvData$TSGraphOp
	varstats <- .cdtData$EnvData$output$exist.vars.dates
	this.var <- .cdtData$EnvData$now$this.vars
	this.stat <- .cdtData$EnvData$now$this.stats
	infos <- varstats[[this.var]][[this.stat]]

	if(.cdtData$EnvData$output$params$data.type == "cdtstation"){
		ixy <- which(.cdtData$EnvData$output$data$id == str_trim(tclvalue(.cdtData$EnvData$plot.maps$stnIDTSp)))
		if(length(ixy) == 0){
			Insert.Messages.Out("Station not found", format = TRUE)
			return(NULL)
		}
		don <- as.numeric(.cdtData$EnvData$tsData$data[, ixy])
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
			Insert.Messages.Out("Coordinates outside of data range", format = TRUE)
			return(NULL)
		}
		ixy <- ilo + length(xlon) * (ila - 1)

		don <- readCdtDatasetChunk.locations(ixy, cdtdataset$fileInfo, cdtdataset, do.par = FALSE)
		don <- as.numeric(don$data[, 1])
		.cdtData$EnvData$location <- paste0("Longitude: ", round(ilon, 5), ", Latitude: ", round(ilat, 5))
	}

	daty <- as.numeric(substr(varstats[[this.var]]$date, 1, 4))

	########

	titre1 <- switch(this.var,
					"TOTALRAIN" = 'Total Rainfall',
					"RAININT" = 'Rainfall Intensity',
					"WETDAY" = 'Number of Wet Days',
					"DRYDAY" = 'Number of Dry Days',
					"WETSPELL" = 'Number of Wet Spells',
					"DRYSPELL" = 'Number of Dry Spells')

	var.def <- switch(this.var, "TOTALRAIN" = '', "RAININT" = '',
					"WETDAY" = paste0('(RR  >= ', infos$pars[1], ' mm)'),
					"DRYDAY" = paste0('(RR  < ', infos$pars[1], ' mm)'),
					"WETSPELL" = paste0('(spell: ', infos$pars[2], ' days)'),
					"DRYSPELL" = paste0('(spell: ', infos$pars[2], ' days)'))

	titre <- paste(titre1, var.def)

	########

	xlab0 <- ""
	ylab0 <- ""

	#########

	GRAPHTYPE <- str_trim(tclvalue(.cdtData$EnvData$plot.maps$typeTSp))
	optsgph <- switch(GRAPHTYPE,
				"Line" = TSGraphOp$line,
				"Barplot" = TSGraphOp$bar,
				"Probability" = TSGraphOp$proba,
				"Anomaly" = TSGraphOp$anomaly)

	## xlim, ylim, xlab, ylab
	if(GRAPHTYPE == "Probability"){
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
		titre <- if(GRAPHTYPE == "Anomaly") paste("Anomaly:", titre) else titre
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

	if(GRAPHTYPE == "Probability"){
		graphs.plot.proba(don, xlim = xlim, ylim = ylim,
						xlab = xlab, xlab.sub = NULL, ylab = ylab,
						title = titre, title.position = titre.pos, axis.font = 1,
						proba = list(theoretical = optsgph$proba$theoretical),
						plotp = optsgph$proba, plotl = optsgph$plot,
						location = .cdtData$EnvData$location)
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
}

##############################################################################

dailyRainAnalysis.Display.MapsVarStats <- function(){
	if(is.null(.cdtData$EnvData)) return(NULL)
	if(is.null(.cdtData$EnvData$output)) return(NULL)

	imgContainer <- CDT.Display.Map.inter(dailyRainAnalysis.plotMapVarStats, .cdtData$EnvData$tab$dataMapStat, 'Analysis-Stats-Map')
	.cdtData$EnvData$tab$dataMapStat <- imageNotebookTab_unik(imgContainer, .cdtData$EnvData$tab$dataMapStat)

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
			imgContainer1 <- CDT.Display.Graph(dailyRainAnalysis.plotVarGraph, .cdtData$EnvData$tab$dataGraph, 'Analysis-Graph')
			.cdtData$EnvData$tab$dataGraph <- imageNotebookTab_unik(imgContainer1, .cdtData$EnvData$tab$dataGraph)
		}
	})
}

#######################################

dailyRainAnalysis.Display.MapVarTS <- function(){
	if(is.null(.cdtData$EnvData)) return(NULL)
	if(is.null(.cdtData$EnvData$output)) return(NULL)
	imgContainer <- CDT.Display.Map.inter(dailyRainAnalysis.plotMapVarTS, .cdtData$EnvData$tab$dataMapTS, 'Analysis-Var-Map')
	.cdtData$EnvData$tab$dataMapTS <- imageNotebookTab_unik(imgContainer, .cdtData$EnvData$tab$dataMapTS)

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
			imgContainer1 <- CDT.Display.Graph(dailyRainAnalysis.plotVarGraph, .cdtData$EnvData$tab$dataGraph, 'Analysis-Graph')
			.cdtData$EnvData$tab$dataGraph <- imageNotebookTab_unik(imgContainer1, .cdtData$EnvData$tab$dataGraph)
		}
	})
}
