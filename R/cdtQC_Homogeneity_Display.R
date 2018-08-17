
homPlot_AdjustedSeries <- function(){
	plotBase <- as.logical(as.integer(tclvalue(.cdtData$EnvData$plot$base)))
	plotMean <- as.logical(as.integer(tclvalue(.cdtData$EnvData$plot$mean)))
	plotQM <- as.logical(as.integer(tclvalue(.cdtData$EnvData$plot$qm)))

	kolors <- c('black', 'red', 'blue')
	linetype <- c(1, 2, 3)
	textlenged <- c("Base series", "Adjusted by mean", "Adjusted by QM")

	iselect <- which(c(plotBase, plotMean, plotQM))
	xcol <- if(length(iselect) > 0) kolors[iselect] else "white"
	xlty <- if(length(iselect) > 0) linetype[iselect] else 0
	xtxt <- if(length(iselect) > 0) textlenged[iselect] else ""

	don <- .cdtData$EnvData$adjS
	inull <- sapply(don, is.null)
	don <- don[!inull]

	############

	if(length(don) == 3){
		mlayout <- matrix(1:4, ncol = 1)
		height <- c(0.2, 1, 1, 1)
		mar <- list(c(3, 5.5, 0, 2), c(0, 5.5, 0, 2), c(0, 5.5, 0, 2))
	}

	if(length(don) == 2){
		mlayout <- matrix(1:3, ncol = 1)
		height <- c(0.2, 1, 1)
		mar <- list(c(3, 5.5, 0, 2), c(0, 5.5, 0, 2))
	}

	if(length(don) == 1){
		mlayout <- matrix(1:2, ncol = 1)
		height <- c(0.2, 1)
		mar <- list(c(3, 5.5, 0, 2))
	}

	############
	plot.pars <- lapply(seq(length(don)), function(j){
		x <- don[[j]]
		list(mar = mar[[j]],
			date = format.plot.date(x$date, x$tstep),
			ylab = tools::toTitleCase(x$tstep),
			ylim = range(pretty(x$data)))
	})

	############
	xlim <- range(do.call(c, lapply(plot.pars, function(x) range(x$date))))
	vgrid <- seq(xlim[1], xlim[2] + 365, 'year')

	############

	layout(mlayout, widths = 1, heights = height, respect = FALSE)


	op <- par(mar = c(0, 5.5, 0.1, 2))
	plot.new()
	legend("center", "groups", xtxt, lty = xlty, col = xcol, lwd = 3, horiz = TRUE)
	par(op)

	############

	ret <- lapply(rev(seq(length(don))), function(j){
		xaxt <- if(j == 1) NULL else "n"
		op <- par(mar = plot.pars[[j]]$mar)
		plot(plot.pars[[j]]$date, don[[j]]$data[, 1], type = 'n', xaxt = xaxt, yaxt = 'n',
			xlab = '', ylab = '', xlim = xlim, ylim = plot.pars[[j]]$ylim)
		abline(h = axTicks(2), col = "lightgray", lty = "dotted")
		abline(v = vgrid, col = "lightgray", lty = "dotted")
		axis(2, at = axTicks(2), las = 2)
		if(j == 1) axis.Date(1, at = vgrid, labels = NA, tcl = par("tcl") * 0.5)
		mtext(plot.pars[[j]]$ylab, side = 2, line = 3.2)

		if(plotBase) lines(plot.pars[[j]]$date, don[[j]]$data[, 1], lty = linetype[1], col = kolors[1], lwd = 1.5)
		if(plotMean) lines(plot.pars[[j]]$date, don[[j]]$data[, 2], lty = linetype[2], col = kolors[2], lwd = 1.5)
		if(plotQM) lines(plot.pars[[j]]$date, don[[j]]$data[, 3], lty = linetype[3], col = kolors[3], lwd = 1.5)

		plt <- par("plt")
		usr <- par("usr")
		par(op)
		c(plt, usr)
	})
}

homPlot_BreakPoints <- function(){
	plotseries <- str_trim(tclvalue(.cdtData$EnvData$plot$plotseries))
	stnid <- str_trim(tclvalue(.cdtData$EnvData$STN$stnID))
	STNID <- .cdtData$EnvData$output$data$id
	ids <- which(STNID == stnid)

	############
	if(plotseries == "Test Series"){
		don <- lapply(.cdtData$EnvData$testS, function(x){
			if(is.null(x)) return(NULL)
			don <- x$data[, ids]
			x$data <- don
			x
		})
	}
	if(plotseries == "Base Series"){
		don <- lapply(.cdtData$EnvData$candS, function(x){
			if(is.null(x)) return(NULL)
			don <- x$data[, ids]
			x$data <- don
			x
		})
	}

	############
	cpt.dates <- lapply(.cdtData$EnvData$cpt.table, function(x){
		if(is.null(x)) return(NULL)
		out <- x[[stnid]]
		if(is.null(out)) return(NULL)
		daty <- str_trim(as.character(out$Breakpoints.Date))
		valid.date <- !is.na(daty) & daty != ""
		if(!any(valid.date)) return(NULL)
		signf <- str_trim(as.character(out$Signif.Test))
		daty <- daty[valid.date]
		signf <- signf[valid.date]
		isg <- signf %in% "****"
		inew <- signf %in% "new"

		daty0 <- daty[isg]
		daty1 <- daty[inew]
		daty2 <- daty[!isg & !inew]
		list(signf = if(length(daty0)) daty0 else NULL,
			new = if(length(daty1)) daty1 else NULL,
			nosignf = if(length(daty2)) daty2 else NULL)
	})

	cpt.index <- lapply(seq_along(cpt.dates), function(j){
		x <- cpt.dates[[j]]
		if(is.null(x)) return(NULL)
		daty <- do.call(c, x)
		which(don[[j]]$date %in% daty)
	})

	############
	inull <- sapply(don, is.null)
	don <- don[!inull]
	cpt.dates <- cpt.dates[!inull]
	cpt.index <- cpt.index[!inull]

	############
	v.seg.breaks <- lapply(seq_along(cpt.dates), function(j){
		x <- cpt.dates[[j]]
		if(is.null(x)) return(NULL)
		lapply(x, function(d){
			if(is.null(d)) return(NULL)
			format.plot.date(d, don[[j]]$tstep)
		})
	})

	############
	hom.mthd <- .cdtData$EnvData$output$params$stats$mthd
	func.seg <- if(hom.mthd == 'CUSUMtr') getTrend.cptSeg else getMean.cptSeg

	cpt.seg <- lapply(seq_along(don), function(j){
		x <- don[[j]]$data
		cpt <- cpt.index[[j]]
		if(length(cpt) == 0) return(NULL)
		func.seg(x, cpt)
	})

	############
	breaks.color <- list(signf = "blueviolet", new = "blue", nosignf = "cyan")

	if(length(don) == 3){
		mlayout <- matrix(1:3, ncol = 1)
		height <- c(1, 1, 1)
		mar <- list(c(3, 4.5, 0, 2), c(0, 4.5, 0, 2), c(0, 4.5, 2, 2))
	}

	if(length(don) == 2){
		mlayout <- matrix(1:2, ncol = 1)
		height <- c(1, 1)
		mar <- list(c(3, 4.5, 0, 2), c(0, 4.5, 2, 2))
	}

	if(length(don) == 1){
		mlayout <- matrix(1, ncol = 1)
		height <- 1
		mar <- list(c(3, 4.5, 2, 2))
	}

	############
	plot.pars <- lapply(seq(length(don)), function(j){
		x <- don[[j]]
		list(mar = mar[[j]],
			date = format.plot.date(x$date, x$tstep),
			ylab = tools::toTitleCase(x$tstep))
	})

	############
	xlim <- range(do.call(c, lapply(plot.pars, function(x) range(x$date))))
	vgrid <- seq(xlim[1], xlim[2] + 365, 'year')

	############
	layout(mlayout, widths = 1, heights = height, respect = FALSE)
	ret <- lapply(rev(seq(length(don))), function(j){
		xaxt <- if(j == 1) NULL else "n"
		op <- par(mar = plot.pars[[j]]$mar)
		plot(plot.pars[[j]]$date, don[[j]]$data, type = 'n', xaxt = xaxt, xlab = '', ylab = '', xlim = xlim)
		abline(h = axTicks(2), col = "lightgray", lty = "dotted")
		abline(v = vgrid, col = "lightgray", lty = "dotted")
		if(j == 1) axis.Date(1, at = vgrid, labels = NA, tcl = par("tcl") * 0.5)
		mtext(plot.pars[[j]]$ylab, side = 2, line = 2.5)

		seg <- cpt.seg[[j]]
		v.breaks <- v.seg.breaks[[j]]

		lines(plot.pars[[j]]$date, don[[j]]$data)
		if(!is.null(seg)) lines(plot.pars[[j]]$date, seg, col = "red", lwd = 2)
		for(i in 1:3){
			vbrks <- v.breaks[[i]]
			if(is.null(vbrks)) next
			pos <- which(plot.pars[[j]]$date %in% vbrks)
			points(vbrks, seg[pos], col = breaks.color[[i]], cex = 1.5)
			vertic <- rep(-100, length(vbrks))
			segments(vbrks, vertic, vbrks, seg[pos], col = breaks.color[[i]], lwd = 2, lty = '1373')
		}

		plt <- par("plt")
		usr <- par("usr")
		par(op)
		c(plt, usr)
	})

	return(ret)
}

######################################################################################################

homDislpay_BreakPoints <- function(notebookTab, tab.title){
	nb.plot <- length(which(!sapply(.cdtData$EnvData$testS, is.null)))
	intstep1 <- sapply(.cdtData$EnvData$testS, '[[', 'tstep')[1]

	varplot <- c("parPlotSize1", "parPlotSize2", "parPlotSize3", "parPlotSize4",
				"usrCoords1", "usrCoords2", "usrCoords3", "usrCoords4")
	parPltCrd <- lapply(seq(nb.plot), function(i){
		setNames(lapply(varplot, function(x) assign(x, tclVar(), env = parent.frame())), varplot)
	})

	plotIt <- function(){
		op <- par(bg = "white")
		pltusr <- homPlot_BreakPoints()
		par(op)
		for(i in seq(nb.plot))
			for(j in seq_along(varplot))
				tclvalue(parPltCrd[[i]][[varplot[j]]]) <- pltusr[[i]][j]
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

	tkbind(img, "<Motion>", function(W, x, y){
		frxcoord <- ''
		frycoord <- ''

		if(nb.plot == 3){
			xyMon <- mouseMouvment(W, x, y, parPltCrd[[1]], ydiv = c(2/3, 1))
			xyDek <- mouseMouvment(W, x, y, parPltCrd[[2]], ydiv = c(1/3, 2/3))
			xyDly <- mouseMouvment(W, x, y, parPltCrd[[3]], ydiv = c(0, 1/3))

			if(xyDly$xym$y >= 0 & xyDly$xym$y < 1/3){
				frxcoord <- if(xyDly$inout) '' else format.plot.date.label(xyDly$x, intstep1)
				frycoord <- if(xyDly$inout) '' else round(xyDly$y, 2)
			}else if(xyDek$xym$y >= 1/3 & xyDek$xym$y < 2/3){
				frxcoord <- if(xyDek$inout) '' else format.plot.date.label(xyDek$x, "dekadal")
				frycoord <- if(xyDek$inout) '' else round(xyDek$y, 2)
			}else if(xyMon$xym$y >= 2/3 & xyMon$xym$y < 1){
				frxcoord <- if(xyMon$inout) '' else format.plot.date.label(xyMon$x, "monthly")
				frycoord <- if(xyMon$inout) '' else round(xyMon$y, 2)
			}else{
				frxcoord <- ''
				frycoord <- ''
			}
		}

		if(nb.plot == 2){
			xyMon <- mouseMouvment(W, x, y, parPltCrd[[1]], ydiv = c(1/2, 1))
			xyDek <- mouseMouvment(W, x, y, parPltCrd[[2]], ydiv = c(0, 1/2))

			if(xyDek$xym$y >= 0 & xyDek$xym$y < 1/2){
				frxcoord <- if(xyDek$inout) '' else format.plot.date.label(xyDek$x, "dekadal")
				frycoord <- if(xyDek$inout) '' else round(xyDek$y, 2)
			}else if(xyMon$xym$y >= 1/2 & xyMon$xym$y < 1){
				frxcoord <- if(xyMon$inout) '' else format.plot.date.label(xyMon$x, "monthly")
				frycoord <- if(xyMon$inout) '' else round(xyMon$y, 2)
			}else{
				frxcoord <- ''
				frycoord <- ''
			}
		}

		if(nb.plot == 1){
			xyMon <- mouseMouvment(W, x, y, parPltCrd[[1]], ydiv = c(0, 1))

			frxcoord <- if(xyMon$inout) '' else format.plot.date.label(xyMon$x, "monthly")
			frycoord <- if(xyMon$inout) '' else round(xyMon$y, 2)
		}

		tclvalue(.cdtEnv$tcl$status$xcrd) <- frxcoord
		tclvalue(.cdtEnv$tcl$status$ycrd) <- frycoord
	})

	tkbind(img, "<Enter>", function() tkconfigure(img, cursor = 'crosshair'))
	tkbind(img, "<Leave>", function() tkconfigure(img, cursor = ''))

	return(list(onglet, img))
}
