
assessData_plotStnAnnual <- function(){
	ylab <- "Number of stations"
	titre <- "Average number of stations reporting each year"

	year <- .cdtData$EnvData$output$year
	stn <- .cdtData$EnvData$output$nb.stn.year

	xlim <- range(year)
	ylim <- range(stn, na.rm = TRUE)

	opar <- par(mar = c(3.5, 4, 3, 1.5))
	plot(1, type = 'n', axes = FALSE, xlim = xlim, ylim = ylim, xlab = '', ylab = ylab, main = titre)

	minTck <- axTicks(2)
	minTck <- minTck[-length(minTck)] + diff(minTck) / 2
	minTck <- c(min(axTicks(2)) - diff(minTck)[1] / 2, minTck, max(axTicks(2)) + diff(minTck)[1] / 2)
	abline(h = axTicks(2), col = "lightgray", lty = "solid", lwd = 0.8)
	abline(h = minTck, col = "lightgray", lty = "dotted")

	lines(year, stn[, 1], type = "h", lwd = 10, lend = "butt", col = 4)
	points(year, stn[, 1], pch = 20, col = 2, cex = 0.5)
	ix <- stn[, 3] - stn[, 2] > 0
	arrows(year[ix], stn[ix, 2], year[ix], stn[ix, 3], angle = 90, code = 3, length = 0.03, col = 2)

	xTck <- axTicks(1)
	xTck <- xTck[sapply(xTck, function(e) min(abs(c(e%%1, e%%1 - 1))) < 1e-10)]
	if(as.numeric(diff(xlim)) > 5){
		xminor <- seq(floor(xlim[1]), floor(xlim[2]), 1)
		xminor <- xminor[!xminor %in% xTck]
	}else xminor <- NULL

	axis(1, at = xTck)
	if(length(xminor) > 0) axis(1, at = xminor, labels = NA, tcl = par("tcl") * 0.5)
	axis(2, at = axTicks(2), las = 1)
	box(bty = 'l')
	box(bty = '7', col = 'gray')
	par(opar)
}

##############################

assessData_plotDistCor <- function(){
	dstcor <- readRDS(file.path(.cdtData$EnvData$PathData, 'CDTDATASET', "Distance_Correlation.rds"))
	xlab <- "Distance (km)"
	ylab <- "Correlation"

	xys <- dstcor$summary
	xlim <- range(xys[, 1], na.rm = TRUE)
	ylim <- c(min(xys[, 2], na.rm = TRUE), max(xys[, 4], na.rm = TRUE))

	op <- par(mar = c(3.5, 4, 2, 2))
	plot(1, type = 'n', xlim = xlim, ylim = ylim, xlab = '', ylab = '')
	polygon(c(rev(xys[, 1]), xys[, 1]), c(rev(xys[, 4]), xys[, 2]), col = "#97FFFF", border = NA)
	lines(xys[, 1], xys[, 3], col = 'blue', lwd = 2)
	lines(dstcor$loess$x, dstcor$loess$y, lwd = 2, col = 'red')
	abline(v = axTicks(1), h = axTicks(2), col = 'lightgrey', lty = "dotted")
	mtext(xlab, side = 1, line = 2)
	mtext(ylab, side = 2, line = 2.5)

	legend(x = 'topright', legend = c("5/95th Percentile", "Median", 'Loess smooth'), bg = 'gray97',
			col = c("#97FFFF", 'blue', 'red'), lty = c(0, 1, 1), lwd = c(0, 2, 2),
			pch = c(22, NA, NA), pt.bg = c("#97FFFF", NA, NA), pt.cex = 2)
	par(op)
}

##############################

assessData_plotActivities <- function(YLayoutDiv){
	stn <- readRDS(file.path(.cdtData$EnvData$PathData, 'CDTDATASET', "Station_Activities.rds"))

	xlim <- range(stn$date)
	ylim <- c(0, max(pretty(stn$work)))
	ylab <- 'Number of stations'
	text.legend <- c('Active Stations', 'Reported Data')
	col.avail <- 'pink'
	col.work <- 'cyan1'
	col.legend <- c(col.work, col.avail)

	layout(matrix(1:2, ncol = 1), widths = 1, heights = YLayoutDiv, respect = FALSE)
	op <- par(mar = c(0, 0, 0, 0))
	plot.new()
	legend("center", "groups", legend = text.legend, fill = col.legend, horiz = TRUE)
	par(op)
	op <- par(mar = c(3.5, 4, 0, 2))
	plot(stn$date, stn$work, type = 'n', xaxt = 'n', xlab = '', ylab = ylab, xlim = xlim, ylim = ylim)

	xTck <- axTicks.Date(stn$date, 1)
	if(as.numeric(diff(xlim)) > 1095){
		xminor <- seq(as.Date(paste0(format(xlim[1], "%Y"), "-01-01")),
					as.Date(paste0(as.numeric(format(xlim[2], "%Y")) + 1, "-01-01")), "year")
		xminor <- xminor[!xminor %in% xTck]
	}else xminor <- NULL
	axis.Date(1, at = xTck)
	if(length(xminor) > 0) axis.Date(1, at = xminor, labels = NA, tcl = par("tcl") * 0.5)

	polygon(c(rev(stn$date), stn$date), c(rev(stn$avai), rep(0, length(stn$date))), col = col.avail, border = NA)
	polygon(c(rev(stn$date), stn$date), c(rev(stn$work), stn$avai), col = col.work, border = NA)

	abline(h = axTicks(2), col = "lightgray", lty = "dotted")
	abline(v = xTck, col = "lightgray", lty = "dotted")

	box()
	plt <- par("plt")
	usr <- par("usr")
	par(op)
	return(list(par = c(plt, usr)))
}

##############################

assessData_plotAvailability <- function(){
	don <- .cdtData$EnvData$DataAvail$map
	dataMapOp <- .cdtData$EnvData$avaiMapOp

	## titre
	if(!dataMapOp$title$user){
		.titre <- "Data Availability (in %)"
	}else .titre <- dataMapOp$title$title

	#################

	.data.type <- "Points"
	.plot.type <- str_trim(tclvalue(.cdtData$EnvData$plot.maps$plot.type))
	map.args <- cdt.plotmap.args(don, dataMapOp, .cdtData$EnvData$shp)

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

##############################

assessData_plotYearlyData <- function(){
	don <- .cdtData$EnvData$DataYear$map
	dataMapOp <- .cdtData$EnvData$yearMapOp

	## titre
	if(!dataMapOp$title$user){
		this.daty <- str_trim(tclvalue(.cdtData$EnvData$availDate))
		.titre <- paste("Data Availability", this.daty, "(days)")
	}else .titre <- dataMapOp$title$title

	#################

	.data.type <- "Points"
	.plot.type <- str_trim(tclvalue(.cdtData$EnvData$plot.maps$plot.type))
	map.args <- cdt.plotmap.args(don, dataMapOp, .cdtData$EnvData$shp)

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


##############################

assessData_plotYearlyTS <- function(){
	optsgph <- .cdtData$EnvData$TSGraphOp
	IDSTN <- .cdtData$EnvData$output$data$id
	ixy <- which(IDSTN == str_trim(tclvalue(.cdtData$EnvData$plot.maps$stnIDTSp)))
	if(length(ixy) == 0){
		Insert.Messages.Out("Station not found", format = TRUE)
		return(NULL)
	}

	don <- as.numeric(.cdtData$EnvData$yearly$data[, ixy])
	location <- paste0("Station: ", IDSTN[ixy])

	xlab <- if(optsgph$axislabs$is.xlab) optsgph$axislabs$xlab else ''
	ylab <- if(optsgph$axislabs$is.ylab) optsgph$axislabs$ylab else 'Number of Non-Missing data/Year'
	titre <- if(optsgph$title$is.title) optsgph$title$title else IDSTN[ixy]

	year <- .cdtData$EnvData$yearly$year
	at.tick <- seq_len(length(year) + 1)
	at.lab <- seq_along(year) - 0.5

	max.ylim <- switch(.cdtData$EnvData$output$params$intstep,
					'daily' = 400, 'pentad' = 80, 'dekadal' = 40, 'monthly' = 14)

	opar <- par(mar = c(3.5, 4, 3, 1.5))
	barplot(don, space = 0, ylim = c(0, max.ylim), xaxt = 'n', yaxt = 'n',
			col = optsgph$colors$col, main = titre)
	axis(side = 2, at = axTicks(2))
	axis(side = 1, at = at.tick - 1, labels = FALSE, tcl = par("tcl") * 0.7)
	axis(side = 1, at = at.lab , tick = FALSE, labels = year)
	abline(h = axTicks(2), col = "lightgray", lty = "dotted")
	mtext(xlab, side = 1, line = 2)
	mtext(ylab, side = 2, line = 2.5)

	mtext(location, side = 3, outer = FALSE, adj = 1, line = 0, cex = 0.6)
	box()
	plt <- par("plt")
	usr <- par("usr")
	par(opar)

	return(list(par = c(plt, usr)))
}

######################################################################################################

assessData_dateLabel <- function(x){
	intstep <- .cdtData$EnvData$output$params$intstep
	daty <- as.Date(x, origin = '1970-1-1')
	if(intstep == 'daily')
		labdates <- format(daty, '%d-%b-%Y')
	if(intstep %in% c('pentad', 'dekadal')){
		brks <- if(intstep == 'dekadal') c(1, 10, 20, 31) else c(1, 5, 10, 15, 20, 25, 31)
		day <- as.numeric(format(daty, '%d'))
		pdk <- findInterval(day, brks, rightmost.closed = TRUE, left.open = TRUE)
		labdates <- paste0(pdk, '-', format(daty, '%b-%Y'))
	}
	if(intstep == 'monthly')
		labdates <- format(daty,'%b-%Y')

	return(labdates)
}

assessData_displayActivities <- function(notebookTab){
	varplot <- c("parPlotSize1", "parPlotSize2", "parPlotSize3", "parPlotSize4",
				"usrCoords1", "usrCoords2", "usrCoords3", "usrCoords4")
	parPltCrd <- setNames(lapply(varplot, function(x) assign(x, tclVar(), env = parent.frame())), varplot)

	YLayoutDiv <- c(0.1, 1)

	plotIt <- function(){
		op <- par(bg = "white")
		pltusr <- assessData_plotActivities(YLayoutDiv)
		par(op)
		for(j in seq_along(varplot)) tclvalue(parPltCrd[[varplot[j]]]) <- pltusr$par[j]
	}

	#########
	onglet <- imageNotebookTab_open(notebookTab, 'Station-Activities')
	hscale <- as.numeric(tclvalue(tkget(.cdtEnv$tcl$toolbar$spinH)))
	vscale <- as.numeric(tclvalue(tkget(.cdtEnv$tcl$toolbar$spinV)))

	img <- tkrplot(onglet[[2]], fun = plotIt, hscale = hscale, vscale = vscale)
	tkgrid(img)
	tkgrid.rowconfigure(img, 0, weight = 1)
	tkgrid.columnconfigure(img, 0, weight = 1)
	tcl("update")

	#########

	tkbind(img, "<Motion>", function(W, x, y){
		xyMouse <- mouseMouvment(W, x, y, parPltCrd, ydiv = c(0, 1 / sum(YLayoutDiv)))

		frxcoord <- if(xyMouse$inout) '' else assessData_dateLabel(xyMouse$x)
		frycoord <- if(xyMouse$inout) '' else round(xyMouse$y)

		tclvalue(.cdtEnv$tcl$status$xcrd) <- frxcoord
		tclvalue(.cdtEnv$tcl$status$ycrd) <- frycoord
	})

	tkbind(img, "<Enter>", function() tkconfigure(img, cursor = 'crosshair'))
	tkbind(img, "<Leave>", function() tkconfigure(img, cursor = ''))

	return(list(onglet, img))
}

##############################

assessData_displayYearlyTS <- function(notebookTab, title.graph){
	varplot <- c("parPlotSize1", "parPlotSize2", "parPlotSize3", "parPlotSize4",
				"usrCoords1", "usrCoords2", "usrCoords3", "usrCoords4")
	parPltCrd <- setNames(lapply(varplot, function(x) assign(x, tclVar(), env = parent.frame())), varplot)

	plotIt <- function(){
		op <- par(bg = "white")
		pltusr <- assessData_plotYearlyTS()
		par(op)
		for(j in seq_along(varplot)) tclvalue(parPltCrd[[varplot[j]]]) <- pltusr$par[j]
	}

	#########
	onglet <- imageNotebookTab_open(notebookTab, title.graph)
	hscale <- as.numeric(tclvalue(tkget(.cdtEnv$tcl$toolbar$spinH)))
	vscale <- as.numeric(tclvalue(tkget(.cdtEnv$tcl$toolbar$spinV)))

	img <- tkrplot(onglet[[2]], fun = plotIt, hscale = hscale, vscale = vscale)
	tkgrid(img)
	tkgrid.rowconfigure(img, 0, weight = 1)
	tkgrid.columnconfigure(img, 0, weight = 1)
	tcl("update")

	#########
	year <- .cdtData$EnvData$yearly$year

	#########

	tkbind(img, "<Motion>", function(W, x, y){
		xyMouse <- mouseMouvment(W, x, y, parPltCrd)

		ipos <- ceiling(xyMouse$x)
		yearAxisRange <- ipos < 1 | ipos > length(year) | xyMouse$inout
		frxcoord <- ifelse(yearAxisRange, '', year[ipos])
		frycoord <- ifelse(xyMouse$inout, '', round(xyMouse$y))

		tclvalue(.cdtEnv$tcl$status$xcrd) <- frxcoord
		tclvalue(.cdtEnv$tcl$status$ycrd) <- frycoord
	})

	tkbind(img, "<Enter>", function() tkconfigure(img, cursor = 'crosshair'))
	tkbind(img, "<Leave>", function() tkconfigure(img, cursor = ''))

	return(list(onglet, img))
}

##############################

assessData_displayAvailability <- function(title.map, title.graph){
	imgContainer <- CDT.Display.Map.inter(assessData_plotAvailability, .cdtData$EnvData$tab$availMap, title.map)
	.cdtData$EnvData$tab$availMap <- imageNotebookTab_unik(imgContainer, .cdtData$EnvData$tab$availMap)

	###############
	tkbind(imgContainer[[2]], "<Button-1>", function(W, x, y){
		xyid <- getIDLatLonCoords(W, x, y, imgContainer[[3]], getStnIDLabel,
						stn.coords = .cdtData$EnvData$plot.maps[c('lon', 'lat', 'id')])
		if(xyid$plotTS)
			tclvalue(.cdtData$EnvData$plot.maps$stnIDTSp) <- xyid$crd

		if(xyid$plotTS){
			if(str_trim(tclvalue(.cdtData$EnvData$plot.maps$typeTSp)) == "Chart"){
				if(is.null(.cdtData$EnvData$yearly)) return(NULL)
				imgContainer1 <- assessData_displayYearlyTS(.cdtData$EnvData$tab$yearlyTS, title.graph)
				.cdtData$EnvData$tab$yearlyTS <- imageNotebookTab_unik(imgContainer1, .cdtData$EnvData$tab$yearlyTS)
			}

			if(str_trim(tclvalue(.cdtData$EnvData$plot.maps$typeTSp)) == "Table"){
				if(is.null(.cdtData$EnvData$monthly)) return(NULL)
				table.mon <- assessData_dataMonthly()
				if(is.null(table.mon)) return(NULL)
				.cdtData$EnvData$tab$Table <- tableNotebookTab_unik(table.mon, .cdtData$EnvData$tab$Table, title.graph)
			}
		}
	})
}

##############################

assessData_displayYearlyData <- function(title.map, title.graph){
	imgContainer <- CDT.Display.Map.inter(assessData_plotYearlyData, .cdtData$EnvData$tab$yearMap, title.map)
	.cdtData$EnvData$tab$yearMap <- imageNotebookTab_unik(imgContainer, .cdtData$EnvData$tab$yearMap)

	###############
	tkbind(imgContainer[[2]], "<Button-1>", function(W, x, y){
		xyid <- getIDLatLonCoords(W, x, y, imgContainer[[3]], getStnIDLabel,
						stn.coords = .cdtData$EnvData$plot.maps[c('lon', 'lat', 'id')])
		if(xyid$plotTS)
			tclvalue(.cdtData$EnvData$plot.maps$stnIDTSp) <- xyid$crd

		if(xyid$plotTS){
			if(str_trim(tclvalue(.cdtData$EnvData$plot.maps$typeTSp)) == "Chart"){
				if(is.null(.cdtData$EnvData$yearly)) return(NULL)
				imgContainer1 <- assessData_displayYearlyTS(.cdtData$EnvData$tab$yearlyTS, title.graph)
				.cdtData$EnvData$tab$yearlyTS <- imageNotebookTab_unik(imgContainer1, .cdtData$EnvData$tab$yearlyTS)
			}

			if(str_trim(tclvalue(.cdtData$EnvData$plot.maps$typeTSp)) == "Table"){
				if(is.null(.cdtData$EnvData$monthly)) return(NULL)
				table.mon <- assessData_dataMonthly()
				if(is.null(table.mon)) return(NULL)
				.cdtData$EnvData$tab$Table <- tableNotebookTab_unik(table.mon, .cdtData$EnvData$tab$Table, title.graph)
			}
		}
	})
}

##############################

assessData_dataMonthly <- function(){
	IDSTN <- .cdtData$EnvData$output$data$id
	ixy <- which(IDSTN == str_trim(tclvalue(.cdtData$EnvData$plot.maps$stnIDTSp)))
	if(length(ixy) == 0){
		Insert.Messages.Out("Station not found", format = TRUE)
		return(NULL)
	}

	data.mon <- .cdtData$EnvData$monthly
	year <- data.mon$year
	mon <- data.mon$mon
	don <- as.numeric(data.mon$data[, ixy])
	MOIS <- format(ISOdate(2014, 1:12, 1), "%b")

	res <- reshapeXYZ2Matrix(cbind(year, mon, don))
	head <- c(paste0('STN', '::', IDSTN[ixy]), MOIS[as.integer(res$y)])
	res <- rbind(head, cbind(res$x, res$z))
	res <- data.frame(res, stringsAsFactors = FALSE)
	names(res) <- NULL
	row.names(res) <- NULL
	return(res)
}
