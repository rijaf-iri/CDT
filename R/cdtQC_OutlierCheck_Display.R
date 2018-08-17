
qcPlot_Outliers.Mon <- function(){
	MOIS <- format(ISOdate(2014, 1:12, 1), "%B")
	imois <- which(MOIS == str_trim(tclvalue(.cdtData$EnvData$STN$month)))
	stnid <- str_trim(tclvalue(.cdtData$EnvData$STN$stnID))
	istn <- which(.cdtData$EnvData$stn.data$id == stnid)
	don <- .cdtData$EnvData$stn.data$data[, istn]
	daty <- .cdtData$EnvData$stn.data$dates

	mo <- which(as.numeric(substr(daty, 5, 6)) == imois)
	don <- don[mo]
	daty <- daty[mo]

	ina <- which(!is.na(don))
	don <- don[ina[1]:ina[length(ina)]]
	daty <- daty[ina[1]:ina[length(ina)]]

	if(length(daty) > 0 & !all(is.na(don)))
	{
		xlim <- c(0, length(daty) + 1)
		ylim <- range(pretty(don))
		plotOK <- TRUE
	}else{
		daty <- "2000011"
		don <- NA
		xlim <- c(0, 2)
		ylim <- c(0, 1)
		plotOK <- FALSE
	}

	if(plotOK){
		outlier <- .cdtData$EnvData$outqc$res[[stnid]]$outliers
		iout <- as.character(outlier$OUT.TEMORAL)
		iout <- as.character(outlier$DATE)[!is.na(iout) & iout != ""]
		if(length(iout)){
			iout <- match(iout, daty)
			iout <- iout[!is.na(iout)]
			if(length(iout)) vout <- don[iout]
		}

		idx.year <- split(seq_along(daty), substr(daty, 1, 4))
		at.labx <- sapply(idx.year, mean)
		labx <- as.numeric(names(idx.year))
		at.tickx <- c(1, sapply(idx.year[-1], '[[', 1) + 0.5, length(daty))
	}else{
		iout <- integer(0)
		at.tickx <- 1
		at.labx <- 1
		labx <- 1
	}

	opar <- par(mar = c(3.5, 4, 3, 1.5))
	plot(1, xlim = xlim, ylim = ylim, type = 'n', xaxt = 'n', yaxt = 'n', xlab = '', ylab = '')

	minTck <- axTicks(2)
	minTck <- minTck[-length(minTck)] + diff(minTck) / 2
	minTck <- c(min(axTicks(2)) - diff(minTck)[1] / 2, minTck, max(axTicks(2)) + diff(minTck)[1] / 2)
	abline(h = axTicks(2), col = "lightgray", lty = "solid", lwd = 0.8)
	abline(h = minTck, col = "lightgray", lty = "dotted")
	abline(v = at.tickx, col = "lightgray", lty = "dotted")

	axis(side = 2, at = axTicks(2))
	axis(side = 1, at = at.tickx, labels = FALSE)
	axis(side = 1, at = at.labx , tick = FALSE, labels = labx)

	lines(don, type = "h", lwd = 1, lend = "butt")
	mtext(.cdtData$EnvData$tab$ylabMon, side = 2, line = 2.5)
	title(main = paste(stnid, "-", MOIS[imois]))

	if(length(iout)){
		lines(iout, vout, type = "h", lwd = 2, col = 2, lend = "butt")
		points(iout, vout, col = 2, pch = 6, cex = 1)
	}

	plt <- par("plt")
	usr <- par("usr")
	par(opar)

	return(list(par = c(plt, usr), dates = daty))
}

#################################################################

qcPlot_Spatial.Check <- function(){
	ptsOp <- .cdtData$EnvData$STN$Opt

	#######
	stnid <- str_trim(tclvalue(.cdtData$EnvData$STN$stnID))
	daty <- str_trim(tclvalue(.cdtData$EnvData$STN$dateSP))
	idaty <- which(.cdtData$EnvData$stn.data$dates == daty)

	don <- as.numeric(.cdtData$EnvData$stn.data$data[idaty, ])
	don <- cbind(.cdtData$EnvData$stn.data$lon, .cdtData$EnvData$stn.data$lat, don)

	istn <- .cdtData$EnvData$stn.data$id == stnid
	stn0 <- as.numeric(don[istn, ])
	stn0.x <- stn0[1]
	stn0.y <- stn0[2]
	stn0.z <- stn0[3]

	stn1.x <- NA
	stn1.y <- NA
	stn1.z <- NA
	vois <- .cdtData$EnvData$stn.data$id[.cdtData$EnvData$outqc$res[[stnid]]$stn]
	ivois <- .cdtData$EnvData$stn.data$id %in% vois
	if(any(ivois)){
		stn1 <- don[ivois, , drop = FALSE]
		stn1 <- stn1[!is.na(stn1[, 3]), , drop = FALSE]
		if(nrow(stn1)){
			stn1.x <- stn1[, 1]
			stn1.y <- stn1[, 2]
			stn1.z <- stn1[, 3]
		}
		don <- don[!(istn | ivois), ]
	}else don <- don[!istn, , drop = FALSE]

	stn2.x <- NA
	stn2.y <- NA
	stn2.z <- NA
	don <- don[!is.na(don[, 3]), , drop = FALSE]
	if(nrow(don)){
		stn2.x <- don[, 1]
		stn2.y <- don[, 2]
		stn2.z <- don[, 3]
	}

	#######
	if(ptsOp$circle$draw){
		dst <- .cdtData$EnvData$output$params$params$voisin$dist
		radius <- km2deg(dst, stn0.y)
		theta <- seq(0, 2 * pi, length = 200)

		xc <- stn0.x + radius * cos(theta)
		yc <- stn0.y + radius * sin(theta)
	}

	#######
	shpf <- .cdtData$EnvData$shp
	ocrds <- if(tclvalue(shpf$add.shp) == "1" & !is.null(shpf$ocrds)) shpf$ocrds else matrix(NA, 1, 2)
	SHPOp <- .cdtData$EnvData$SHPOp

	#######
	dem.data <- .cdtData$EnvData$dem
	don.dem <- NULL
	if(tclvalue(dem.data$add.dem) == "1"){
		if(!is.null(dem.data$dem)){
			don.dem <- dem.data$dem
			don.dem$z[don.dem$z < 0] <- 0
			Opts <- dem.data$Opt
			Opts$user.levels$levels <- pretty(don.dem$z, n = 10, min.n = 5)
		}
	}

	#######
	sat.data <- .cdtData$EnvData$sat
	don.sat <- NULL
	if(tclvalue(sat.data$add.sat) == "1"){
		if(!is.null(sat.data$sat.data)){
			ncfile <- switch(.cdtData$EnvData$output$params$intstep,
								'daily' = sprintf(sat.data$format, substr(daty, 1, 4),
												substr(daty, 5, 6), substr(daty, 7, 8)),
								'pentad' = sprintf(sat.data$format, substr(daty, 1, 4),
												substr(daty, 5, 6), substr(daty, 7, 7)),
								'dekadal' = sprintf(sat.data$format, substr(daty, 1, 4),
												substr(daty, 5, 6), substr(daty, 7, 7)),
								'monthly' = sprintf(sat.data$format, substr(daty, 1, 4),
												substr(daty, 5, 6))
							)

			ncfile <- file.path(sat.data$dir, ncfile)
			if(file.exists(ncfile)){
				ncinfo <- sat.data$sat.data
				nc <- nc_open(ncfile)
				don.sat <- ncvar_get(nc, varid = ncinfo$varid)
				nc_close(nc)
				don.sat <- transposeNCDFData(don.sat, ncinfo)
				don.sat <- list(x = ncinfo$lon, y = ncinfo$lat, z = don.sat)
				Opts <- sat.data$Opt
				Opts$user.levels$levels <- pretty(don.sat$z, n = 10, min.n = 5)
			}else{
				Insert.Messages.Out(paste(ncfile, "does not exist"), format = TRUE)
			}
		}
	}

	#######
	## SAT at top

	if(!is.null(don.dem) & !is.null(don.sat)){
		plot.grid <- TRUE
		pars <- cdt.plotmap.args0(don.sat, user.levels = Opts$user.levels, user.colors = Opts$user.colors)
		mar <- pars$mar
	}else if(is.null(don.dem) & !is.null(don.sat)){
		plot.grid <- TRUE
		pars <- cdt.plotmap.args0(don.sat, user.levels = Opts$user.levels, user.colors = Opts$user.colors)
		mar <- pars$mar
	}else if(!is.null(don.dem) & is.null(don.sat)){
		plot.grid <- TRUE
		pars <- cdt.plotmap.args0(don.dem, user.levels = Opts$user.levels, user.colors = Opts$user.colors)
		mar <- pars$mar
	}else{
		mar <- c(4, 4, 2, 2)
		plot.grid <- FALSE
	}

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
	opar <- par(mar = mar)
	plot(1, xlim = c(xmin, xmax), ylim = c(ymin, ymax), xlab = "", ylab = "", type = "n", xaxt = 'n', yaxt = 'n')
	axlabs <- LatLonAxisLabels(axTicks(1), axTicks(2))
	axis(side = 1, at = axTicks(1), labels = axlabs$xaxl, tck = -0.01, cex.axis = 0.8)
	axis(side = 2, at = axTicks(2), labels = axlabs$yaxl, tck = -0.01, las = 1, cex.axis = 0.8)

	if(plot.grid){
		plot.type <- str_trim(tclvalue(.cdtData$EnvData$plot.maps$plot.type))
		if(plot.type == "FilledContour")
			.filled.contour(pars$don$x, pars$don$y, pars$don$z, levels = pars$breaks, col = pars$kolor)
		if(plot.type == "Pixels")
			image(pars$don, breaks = pars$breaks, col = pars$kolor, xaxt = 'n', yaxt = 'n', add = TRUE)

		image.plot(zlim = pars$zlim, breaks = pars$breaks2, col = pars$kolor, horizontal = pars$horizontal,
					legend.only = TRUE, legend.mar = pars$legend.mar, legend.width = pars$legend.width,
					legend.args = NULL, axis.args = list(at = pars$breaks1, labels = pars$labels,
					cex.axis = 0.7, font = 2, tcl = -0.3, mgp = c(0, 0.5, 0)), legend.shrink = 0.8)
	}

	abline(h = axTicks(2), v = axTicks(1) , col = "lightgray", lty = 3)

	lines(ocrds[, 1], ocrds[, 2], lwd = SHPOp$lwd, col = SHPOp$col)

	if(ptsOp$circle$draw) lines(xc, yc, lwd = ptsOp$circle$lwd, col = ptsOp$circle$col)
	points(stn2.x, stn2.y, col = ptsOp$all$col, pch = ptsOp$all$pch, cex = ptsOp$all$cex)
	points(stn1.x, stn1.y, col = ptsOp$vois$col, pch = ptsOp$vois$pch, cex = ptsOp$vois$cex)
	points(stn0.x, stn0.y, col = ptsOp$stn$col, bg = ptsOp$stn$bg, pch = ptsOp$stn$pch, cex = ptsOp$stn$cex)

	text(stn0.x, stn0.y, labels = stn0.z, pos = 1, cex = 0.6, col = ptsOp$stn$txt.col)
	text(stn1.x, stn1.y, labels = stn1.z, pos = 1, cex = 0.6, col = ptsOp$vois$txt.col)
	text(stn2.x, stn2.y, labels = stn2.z, pos = 1, cex = 0.6, col = ptsOp$all$txt.col)

	title(main = paste('STN:', stnid, '- Date:', daty), cex.main = 1, font.main = 1)
	box()

	plt <- par("plt")
	usr <- par("usr")
	par(opar)

	return(list(par = c(plt, usr)))
}

######################################################################################################

qcDislpay_Outliers.Mon <- function(notebookTab, tab.title){
	varplot <- c("parPlotSize1", "parPlotSize2", "parPlotSize3", "parPlotSize4",
				"usrCoords1", "usrCoords2", "usrCoords3", "usrCoords4")
	parPltCrd <- setNames(lapply(varplot, function(x) assign(x, tclVar(), env = parent.frame())), varplot)

	daty <- NULL
	plotIt <- function(){
		op <- par(bg = "white")
		pltusr <- qcPlot_Outliers.Mon()
		par(op)
		for(j in seq_along(varplot)) tclvalue(parPltCrd[[varplot[j]]]) <- pltusr$par[j]
		daty <<- pltusr$dates
	}

	#########
	onglet <- imageNotebookTab_open(notebookTab, tab.title)
	hscale <- as.numeric(tclvalue(tkget(.cdtEnv$tcl$toolbar$spinH)))
	vscale <- as.numeric(tclvalue(tkget(.cdtEnv$tcl$toolbar$spinV)))

	img <- tkrplot(onglet[[2]], fun = plotIt, hscale = hscale, vscale = vscale)
	tkgrid(img)
	tkgrid.rowconfigure(img, 0, weight = 1)
	tkgrid.columnconfigure(img, 0, weight = 1)
	tcl("update")

	#########
	intstep <- .cdtData$EnvData$output$params$intstep

	#########
	tkbind(img, "<Motion>", function(W, x, y){
		xyMouse <- mouseMouvment(W, x, y, parPltCrd)

		ipos <- round(xyMouse$x)
		datyRange <- ipos < 1 | ipos > length(daty) | xyMouse$inout

		frxcoord <- if(datyRange) '' else format.plot.date.label(daty[ipos], intstep)
		frycoord <- if(xyMouse$inout) '' else round(xyMouse$y, 1)

		tclvalue(.cdtEnv$tcl$status$xcrd) <- frxcoord
		tclvalue(.cdtEnv$tcl$status$ycrd) <- frycoord
	})

	tkbind(img, "<Enter>", function() tkconfigure(img, cursor = 'crosshair'))
	tkbind(img, "<Leave>", function() tkconfigure(img, cursor = ''))

	return(list(onglet, img))
}
