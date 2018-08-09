
## Format lat-lon axis label
## old: a remplacer
# axlabsFun <- if(Sys.info()["sysname"] == "Windows") LatLonAxisLabels else LatLonAxisLabels1

LatLonAxisLabels <- function(axis.x, axis.y){
	axis.x <- ifelse(axis.x > 180, -360 + axis.x, axis.x)

	sym1 <- ifelse(axis.x < 0 & axis.x > -180, 'W', ifelse(axis.x > 0 & axis.x < 180, 'E', ''))
	if(WindowsOS()){
		sym1 <- paste("paste(", paste(abs(axis.x), "*degree"), ",", sym1, ")", collapse = ',')
		lon_lab <- eval(parse(text = paste0("expression(", sym1, ")")))
	}else lon_lab <- paste0(abs(axis.x), "°", sym1)

	sym2 <- ifelse(axis.y < 0, 'S', ifelse(axis.y > 0, 'N', ''))
	if(WindowsOS()){
		sym2 <- paste("paste(", paste(abs(axis.y), "*degree"), ",", sym2, ")", collapse = ',')
		lat_lab <- eval(parse(text = paste0("expression(", sym2, ")")))
	}else lat_lab <- paste0(abs(axis.y), "°", sym2)

	return(list(xaxl = lon_lab, yaxl = lat_lab))
}

########################################################################

## Layout for lattice plot
manageLayout <- function(nbPlot, transpose = FALSE){
	 # nmaxPlot = n*n+n
	n <- 5
	nLayout <- cbind(c(rep(1:n, seq(1, n * 2, 2)), rep(n + 1, n)), rep(1:n, seq(2, n * 2, 2)))
	dimLayout <- nLayout[nbPlot, ]
	if(transpose) dimLayout <- rev(dimLayout)

	matdim <- dimLayout[1] * dimLayout[2]
	mat <- rep(NA, matdim)
	mat[((matdim - nbPlot) + 1):matdim] <- 1:nbPlot
	matLayout <- matrix(mat, ncol = dimLayout[1], nrow = dimLayout[2], byrow = TRUE)
	line1 <- matLayout[1, ]
	line1 <- line1[!is.na(line1)]
	ltmp <- rep(NA, dimLayout[1])
	ltmp[line1] <- line1
	matLayout[1, ] <- ltmp
	matLayout <- matLayout[dimLayout[2]:1, ]
	orderLayout <- c(t(matLayout))
	orderLayout <- orderLayout[!is.na(orderLayout)]

	return(list(dim = dimLayout, order = orderLayout))
}

################
## Axis ticks for levelplot
parAxisPlotFun <- function(x, factor = 0.04){
	#factor = 0.04 par defaut pour R
	x <- x[!is.na(x)]
	if(length(x) > 1) xlim <- range(x) + c(-1, 1) * factor * diff(range(x))
	else if(length(x) == 1 & x != 0) xlim <- x + c(-1, 1) * factor * abs(x)
	else xlim <- factor * c(-1, 1)
	xtick <- pretty(x)
	bInf <- min(xtick[xtick >= min(xlim)])
	bSup <- max(xtick[xtick <= max(xlim)])
	intervl <- length(which(xtick >= bInf & xtick <= bSup)) - 1
	ticks <- axTicks(side = 1, usr = xlim, axp = c(bInf, bSup, intervl))
	return(list(usr = xlim, axp = ticks))
}

########################################################################

## axis Date ticks
# https://stackoverflow.com/a/39307885
# This functions is almost a copy of axis.Date
axTicks.Date <- function(x, side = 1){
	x <- as.Date(x)
	range <- par("usr")[if(side %% 2) 1L:2L else 3:4L]
	range[1L] <- ceiling(range[1L])
	range[2L] <- floor(range[2L])
	d <- range[2L] - range[1L]
	z <- c(range, x[is.finite(x)])
	class(z) <- "Date"
	if(d < 7) format <- "%a"
	if(d < 100){
		z <- structure(pretty(z), class = "Date")
		format <- "%b %d"
	}else if(d < 1.1 * 365){
		zz <- as.POSIXlt(z)
		zz$mday <- 1
		zz$mon <- pretty(zz$mon)
		m <- length(zz$mon)
		m <- rep.int(zz$year[1L], m)
		zz$year <- c(m, m + 1)
		z <- as.Date(zz)
		format <- "%b"
	}else{
		zz <- as.POSIXlt(z)
		zz$mday <- 1
		zz$mon <- 0
		zz$year <- pretty(zz$year)
		z <- as.Date(zz)
		format <- "%Y"
	}
	keep <- z >= range[1L] & z <= range[2L]
	z <- z[keep]
	z <- sort(unique(z))
	class(z) <- "Date"
	z
}

########################################################################

## image.plot colorkey parameters
image.plot_Legend_pars <- function(Zmat, user.levels, user.colors, preset.colors)
{
	brks0 <- pretty(Zmat, n = 10, min.n = 5)
	brks0 <- if(length(brks0) > 0) brks0 else c(0, 1)
	breaks <- if(user.levels$custom) user.levels$levels else brks0
	breaks[length(breaks)] <- breaks[length(breaks)] + 1e-15

	## legend label breaks
	legend.label <- breaks

	breaks1 <- if(user.levels$equidist) seq(0, 1, length.out = length(breaks)) else breaks

	Zmat <- Zmat + 1e-15
	Zrange <- if(all(is.na(Zmat))) c(0, 1) else range(Zmat, na.rm = TRUE)

	brks0 <- range(brks0)
	brks1 <- range(breaks)
	brn0 <- min(brks0[1], brks1[1])
	brn1 <- max(brks0[2], brks1[2])
	if(brn0 == breaks[1]) brn0 <- brn0 - 1
	if(brn1 == breaks[length(breaks)]) brn1 <- brn1 + 1
	if(brn0 > Zrange[1]) brn0 <- Zrange[1]
	if(brn1 < Zrange[2]) brn1 <- Zrange[2]
	breaks <- c(brn0, breaks, brn1)
	tbrks2 <- breaks1[c(1, length(breaks1))] + diff(range(breaks1)) * 0.02 * c(-1, 1)
	breaks2 <- c(tbrks2[1], breaks1, tbrks2[2])
	zlim <- range(breaks2)

	## colors
	if(user.colors$custom){
		kolFonction <- colorRampPalette(user.colors$color)
		kolor <- kolFonction(length(breaks) - 1)
	}else{
		kolFonction <- get(preset.colors$color, mode = "function")
		kolor <- kolFonction(length(breaks) - 1)
		if(preset.colors$reverse) kolor <- rev(kolor)
	}
	
	## bin
	## < x1; [x1, x2[; [x2, x3[; ...; [xn-1, xn]; > xn

	list(breaks = breaks, legend.breaks = list(zlim = zlim, breaks = breaks2),
		legend.axis = list(at = breaks1, labels = legend.label), colors = kolor)
}

