
Climdex.plotMapTrend <- function(){
	don <- .cdtData$EnvData$TrendData$map
	dataMapOp <- .cdtData$EnvData$varstatMapOp

	## titre
	if(!dataMapOp$title$user){
		this.vars <- str_trim(tclvalue(.cdtData$EnvData$anaVars))
		this.trend <- str_trim(tclvalue(.cdtData$EnvData$anaStat))

		.titre <- paste(this.vars, ":", this.trend)
	}else .titre <- dataMapOp$title$title

	#################

	.data.type <- .cdtData$EnvData$plot.maps$.data.type
	.plot.type <- str_trim(tclvalue(.cdtData$EnvData$plot.maps$plot.type))
	map.args <- cdt.plotmap.args(don, dataMapOp, .cdtData$EnvData$shp)

	opar <- par(mar = map.args$mar)
	map.args.add <- list(titre = .titre,
						SHPOp = .cdtData$EnvData$SHPOp,
						MapOp = dataMapOp,
						data.type = .data.type,
						plot.type = .plot.type)
	map.args <- map.args[!(names(map.args) %in% "mar")]
	map.args <- c(map.args, map.args.add)
	par.plot <- do.call(cdt.plotmap.fun, map.args)

	## scale bar
	cdt.plotmap.scalebar(dataMapOp$scalebar)

	par(opar)

	return(par.plot)
}

#######################################

Climdex.plotMapYear <- function(){
	don <- .cdtData$EnvData$YearData$map
	dataMapOp <- .cdtData$EnvData$dataMapOp

	## titre
	if(!dataMapOp$title$user){
		if(.cdtData$EnvData$indices.data == "RR")
			.titre <- switch(str_trim(tclvalue(.cdtData$EnvData$anaVars)),
							"Rx1day" = "Yearly maximum 1-day precipitation",
							"Rx5day" = "Yearly maximum consecutive 5-day precipitation",
							"SDII" = "Simple pricipitation intensity index",
							"R10mm" = "Annual count of days when PRCP >= 10mm",
							"R20mm" = "Annual count of days when PRCP >= 20mm",
							"Rnnmm" = paste0("Annual count of days when PRCP >= ", .cdtData$EnvData$output$params$Indices$thres.Rnnmm, "mm"),
							"CDD" = "Maximum length of dry spell,\nmaximum number of consecutive days with RR < 1mm",
							"CWD" = "Maximum length of wet spell,\nmaximum number of consecutive days with RR >= 1mm",
							"R95pTOT" = "Annual total PRCP when RR > 95 percentile",
							"R99pTOT" = "Annual total PRCP when RR > 99 percentile",
							"PRCPTOT" = "Annual total precipitation in wet days")

		if(.cdtData$EnvData$indices.data == "TT")
			.titre <- switch(str_trim(tclvalue(.cdtData$EnvData$anaVars)),
							"TXn" = "Yearly minimum value of daily maximum temperature",
							"TXx" = "Yearly maximum value of daily maximum temperature",
							"TX10p" = "Percentage of days when TX < 10th percentile",
							"TX90p" = "Percentage of days when TX > 90th percentile",
							"WSDI" = "Warm spell duration index",
							"SU" = "Number of summer days",
							"ID" = "Number of icing days",
							"TNn" = "Yearly minimum value of daily minimum temperature",
							"TNx" = "Yearly maximum value of daily minimum temperature",
							"TN10p" = "Percentage of days when TN < 10th percentile",
							"TN90p" = "Percentage of days when TN > 90th percentile",
							"CSDI" = "Cold spell duration index",
							"FD" = "Number of frost days",
							"TR" = "Number of tropical nights",
							"DTR" = "Daily temperature range",
							"GSL" = "Growing season length")
	}else .titre <- dataMapOp$title$title

	#################

	if(.cdtData$EnvData$indices.data == "RR")
		legend.texta <- switch(str_trim(tclvalue(.cdtData$EnvData$anaVars)),
								"Rx1day" = "mm",
								"Rx5day" = "mm",
								"SDII" = "mm",
								"R10mm" = "days",
								"R20mm" = "days",
								"Rnnmm" = "days",
								"CDD" = "days",
								"CWD" = "days",
								"R95pTOT" = "mm",
								"R99pTOT" = "mm",
								"PRCPTOT" = "mm")

	if(.cdtData$EnvData$indices.data == "TT")
		legend.texta <- switch(str_trim(tclvalue(.cdtData$EnvData$anaVars)),
								"TXn" = "degC",
								"TXx" = "degC",
								"TX10p" = "%",
								"TX90p" = "%",
								"WSDI" = "days",
								"SU" = "days",
								"ID" = "days",
								"TNn" = "degC",
								"TNx" = "degC",
								"TN10p" = "%",
								"TN90p" = "%",
								"CSDI" = "days",
								"FD" = "days",
								"TR" = "days",
								"DTR" = "degC",
								"GSL" = "days")

	legend.texta <- paste("Units:",  legend.texta)

	#################

	.data.type <- .cdtData$EnvData$plot.maps$.data.type
	.plot.type <- str_trim(tclvalue(.cdtData$EnvData$plot.maps$plot.type))
	map.args <- cdt.plotmap.args(don, dataMapOp, .cdtData$EnvData$shp, legend.text = legend.texta)

	opar <- par(mar = map.args$mar)
	map.args.add <- list(titre = .titre,
						SHPOp = .cdtData$EnvData$SHPOp,
						# MapOp = dataMapOp,
						data.type = .data.type,
						plot.type = .plot.type)
	map.args <- map.args[!(names(map.args) %in% "mar")]
	map.args <- c(map.args, map.args.add)
	par.plot <- do.call(cdt.plotmap.fun, map.args)

	## scale bar
	cdt.plotmap.scalebar(dataMapOp$scalebar)

	par(opar)

	return(par.plot)
}

#######################################

Climdex.plotVarGraph <- function(){
	TSGraphOp <- .cdtData$EnvData$TSGraphOp
	this.vars <- str_trim(tclvalue(.cdtData$EnvData$anaVars))

	if(.cdtData$EnvData$output$params$data.type == "cdtstation"){
		ixy <- which(.cdtData$EnvData$output$data$id == str_trim(tclvalue(.cdtData$EnvData$plot.maps$stnIDTSp)))
		if(length(ixy) == 0){
			Insert.Messages.Out("Station not found", format = TRUE)
			return(NULL)
		}

		trend <- .cdtData$EnvData$TrendData$data[, ixy]
		don <- as.numeric(.cdtData$EnvData$YearData$data[, ixy])
		.cdtData$EnvData$location <- paste0("Station: ", .cdtData$EnvData$output$data$id[ixy])
		titre <- paste0("(", .cdtData$EnvData$output$data$id[ixy], ")")
	}else{
		xloc <- as.numeric(str_trim(tclvalue(.cdtData$EnvData$plot.maps$lonLOC)))
		yloc <- as.numeric(str_trim(tclvalue(.cdtData$EnvData$plot.maps$latLOC)))
		xyloc <- cdtdataset.extarct.TS(.cdtData$EnvData$cdtdataYear, .cdtData$EnvData$cdtdataYear$fileInfo, xloc, yloc)
		if(is.null(xyloc)) return(NULL)
		don <- as.numeric(xyloc$data)
		rm(xyloc)
		xyloc <- cdtdataset.extarct.TS(.cdtData$EnvData$cdtdataTrend, .cdtData$EnvData$cdtdataTrend$fileInfo, xloc, yloc)
		if(is.null(xyloc)) return(NULL)
		trend <- as.numeric(xyloc$data)
		.cdtData$EnvData$location <- paste0("Longitude: ", round(xloc, 5), ", Latitude: ", round(yloc, 5))
		titre <- ""
	}

	if(this.vars == "GSL")
		daty <- as.numeric(.cdtData$EnvData$output$year.gsl)
	else
		daty <- as.numeric(.cdtData$EnvData$output$year)

	titre <- paste(this.vars, titre)

	#########

	GRAPHTYPE <- str_trim(tclvalue(.cdtData$EnvData$plot.maps$typeTSp))
	if(GRAPHTYPE == "Line") optsgph <- TSGraphOp$line
	if(GRAPHTYPE == "Barplot") optsgph <- TSGraphOp$bar

	xlim <- range(daty, na.rm = TRUE)
	if(optsgph$xlim$is.min) xlim[1] <- as.numeric(optsgph$xlim$min)
	if(optsgph$xlim$is.max) xlim[2] <- as.numeric(optsgph$xlim$max)
	idt <- daty >= xlim[1] & daty <= xlim[2]
	daty <- daty[idt]
	don <- don[idt]
	ylim <- range(pretty(don))
	if(optsgph$ylim$is.min) ylim[1] <- optsgph$ylim$min
	if(optsgph$ylim$is.max) ylim[2] <- optsgph$ylim$max

	xlab <- if(optsgph$axislabs$is.xlab) optsgph$axislabs$xlab else 'Year'
	ylab <- if(optsgph$axislabs$is.ylab) optsgph$axislabs$ylab else this.vars

	if(optsgph$title$is.title){
		titre <- optsgph$title$title
		titre.pos <- optsgph$title$position
	}else{
		titre <- titre
		titre.pos <- "top"
	}

	#########

	if(GRAPHTYPE == "Line"){
		ret <- climdex.plot.line(daty, don, trend, xlim = xlim, ylim = ylim,
						xlab = xlab, ylab = ylab, title = titre,
						title.position = titre.pos, axis.font = 1,
						plotl = optsgph$plot, legends = optsgph$legend,
						location = .cdtData$EnvData$location)
	}

	if(GRAPHTYPE == "Barplot"){
		ret <- climdex.plot.bar(daty, don, trend, xlim = xlim, ylim = ylim,
						xlab = xlab, ylab = ylab, title = titre,
						title.position = titre.pos, axis.font = 1,
						barcol = optsgph$colors$col,
						location = .cdtData$EnvData$location)
	}

	return(ret)
}

##############################################################################

Climdex.Display.MapsTrend <- function(){
	if(is.null(.cdtData$EnvData)) return(NULL)
	if(is.null(.cdtData$EnvData$output)) return(NULL)

	imgContainer <- CDT.Display.Map.inter(Climdex.plotMapTrend, .cdtData$EnvData$tab$dataMapStat, 'Climdex-Trend-Map')
	.cdtData$EnvData$tab$dataMapStat <- imageNotebookTab_unik(imgContainer, .cdtData$EnvData$tab$dataMapStat)

	###############
	tkbind(imgContainer[[2]], "<Button-1>", function(W, x, y){
		if(is.null(.cdtData$EnvData$plot.maps$data.type)) return(NULL)
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
			imgContainer1 <- CDT.Display.Graph(Climdex.plotVarGraph, .cdtData$EnvData$tab$dataGraph, 'Climdex-Graph')
			.cdtData$EnvData$tab$dataGraph <- imageNotebookTab_unik(imgContainer1, .cdtData$EnvData$tab$dataGraph)
		}
	})
}

#######################################

Climdex.Display.MapYear <- function(){
	if(is.null(.cdtData$EnvData)) return(NULL)
	if(is.null(.cdtData$EnvData$output)) return(NULL)

	imgContainer <- CDT.Display.Map.inter(Climdex.plotMapYear, .cdtData$EnvData$tab$dataMapTS, 'Climdex-Yearly-Map')
	.cdtData$EnvData$tab$dataMapTS <- imageNotebookTab_unik(imgContainer, .cdtData$EnvData$tab$dataMapTS)

	###############
	tkbind(imgContainer[[2]], "<Button-1>", function(W, x, y){	
		if(is.null(.cdtData$EnvData$plot.maps$data.type)) return(NULL)
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
			imgContainer1 <- CDT.Display.Graph(Climdex.plotVarGraph, .cdtData$EnvData$tab$dataGraph, 'Climdex-Graph')
			.cdtData$EnvData$tab$dataGraph <- imageNotebookTab_unik(imgContainer1, .cdtData$EnvData$tab$dataGraph)
		}
	})
}
