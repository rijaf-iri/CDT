
graphs.plot.line <- function(x, y, xlim = NULL, ylim = NULL, origindate = NULL,
									xlab = '', ylab = '', ylab.sub = NULL,
									title = '', title.position = 'top', axis.font = 1,
									plotl = NULL, legends = NULL, location = NULL)
{
	if(is.null(plotl$type)) plotl$type <- 'both'
	if(is.null(plotl$col$line)) plotl$col$line <- 'red'
	if(is.null(plotl$col$points)) plotl$col$points <- 'blue'
	if(is.null(plotl$lwd)) plotl$lwd <- 2
	if(is.null(plotl$cex)) plotl$cex <- 1.4

	if(is.null(legends$add$mean)) legends$add$mean <- FALSE
	if(is.null(legends$add$tercile)) legends$add$tercile <- FALSE
	if(is.null(legends$add$linear)) legends$add$linear <- FALSE
	if(is.null(legends$col$mean)) legends$col$mean <- "black"
	if(is.null(legends$col$tercile1)) legends$col$tercile1 <- "green"
	if(is.null(legends$col$tercile2)) legends$col$tercile2 <- "blue"
	if(is.null(legends$col$linear)) legends$col$linear <- "purple3"
	if(is.null(legends$text$mean)) legends$text$mean <- "Average"
	if(is.null(legends$text$tercile1)) legends$text$tercile1 <- "Tercile 0.33333"
	if(is.null(legends$text$tercile2)) legends$text$tercile2 <- "Tercile 0.66666"
	if(is.null(legends$text$linear)) legends$text$linear <- "Trend line"
	if(is.null(legends$lwd$mean)) legends$lwd$mean <- 2
	if(is.null(legends$lwd$tercile)) legends$lwd$tercile <- 2
	if(is.null(legends$lwd$linear)) legends$lwd$linear <- 2

	if(length(y[!is.na(y)]) == 0){
		x0 <- seq_along(x)
		if(length(x0) == 0) x <- x0
		y <- rep(0, length(x0))
		plot(x, y, type = 'n', yaxt = 'n', xlab = xlab, ylab = ylab, main = title)
		Insert.Messages.Out("No data to plot", format = TRUE)
		return(NULL)
	}
	if(is.null(xlim)) xlim <- range(x, na.rm = TRUE)
	if(is.null(ylim)) ylim <- range(pretty(y))

	if(xlim[1] == xlim[2]) xlim <- xlim[1]+c(-0.5, 0.5)
	if(xlim[2]-xlim[1] == 1) xlim <- xlim+c(-0.5, 0.5)

	nylab <- max(nchar(as.character(pretty(y))), na.rm = TRUE)
	line.ylab <- if(nylab < 2) 2 else nylab

	draw.title <- if(missing(title) | str_trim(title) == "") FALSE else TRUE
	plt.h <- if(legends$add$mean | legends$add$tercile | legends$add$linear) 0.13 else 0.05
	nr.ylab <- str_count(ylab, pattern = "\n")
	par.mar.2 <- ifelse(ylab == '', 6.0,
					ifelse(nr.ylab == 0, 6.5,
					ifelse(nr.ylab == 1, 7.5, 8.8)))
	par.mar.2 <- par.mar.2 + nylab/6

	if(draw.title){
		if(missing(title.position)) title.position <- 'top'
		nr.title <- str_count(title, pattern = "\n")
		ttl.h <- if(nr.title == 0) 0.1 else if(nr.title == 1) 0.13 else 0.19
		if(title.position == 'bottom'){
			plot.position <- matrix(1:3, ncol = 1)
			plot.heights <- c(0.9, plt.h, ttl.h)
			par.plot <- c(3.5, par.mar.2, 2.1, 2.1)
			par.legend <- c(0, par.mar.2, 0, 2.1)
			par.title <- c(1, par.mar.2, 0, 2.1)
		}else{
			plot.position <- matrix(c(3, 1, 2), ncol = 1)
			plot.heights <- c(ttl.h, 0.9, plt.h)
			par.plot <- c(3.5, par.mar.2, 1.5, 2.1)
			par.legend <- c(1, par.mar.2, 0, 2.1)
			par.title <- c(0, par.mar.2, 1, 2.1)
		}
	}else{
		plot.position <- matrix(1:3, ncol = 1)
		plot.heights <- c(0.9, plt.h, 0.01)
		par.plot <- c(3.5, par.mar.2, 2.1, 2.1)
		par.legend <- c(0, par.mar.2, 0, 2.1)
		par.title <- c(0, par.mar.2, 0, 2.1)
	}

	layout(plot.position, widths = 1, heights = plot.heights, respect = FALSE)

	op <- par(mar = par.plot)
	plot(x, y, type = 'n', xaxt = 'n', yaxt = 'n', xlab = '', ylab = '', xlim = xlim, ylim = ylim)

	if(is(x, "Date")){
		xTck <- axTicks.Date(x, 1)
		axis.foo <- axis.Date
		if(as.numeric(diff(xlim)) > 1095){
			xminor <- seq(as.Date(paste0(format(xlim[1], "%Y"), "-01-01")),
						as.Date(paste0(as.numeric(format(xlim[2], "%Y"))+1, "-01-01")), "year")
			xminor <- xminor[!xminor%in%xTck]
		}else xminor <- NULL
	}else{
		xTck <- axTicks(1)
		xTck <- xTck[sapply(xTck, function(e) min(abs(c(e%%1, e%%1-1))) < 1e-10)]
		axis.foo <- axis
		if(as.numeric(diff(xlim)) > 5){
			xminor <- seq(floor(xlim[1]), floor(xlim[2]), 1)
			xminor <- xminor[!xminor%in%xTck]
		}else xminor <- NULL
	}

	axis.foo(1, at = xTck, font = axis.font, cex.axis = 1.5)
	if(length(xminor) > 0) axis.foo(1, at = xminor, labels = NA, tcl = par("tcl")*0.5)
	if(!is.null(origindate)){
		yaxlab <- format(as.Date(axTicks(2), origin = origindate), '%d-%b')
		axis(2, at = axTicks(2), labels = yaxlab, las = 2, font = axis.font, cex.axis = 1.5)
	}else axis(2, at = axTicks(2), font = axis.font, las = 1, cex.axis = 1.5)

	mtext(xlab, side = 1, line = 2.5)
	# line <- if(max(nchar(as.character(axTicks(2)))) > 2) 4 else 3
	if(!is.null(ylab.sub)){
		mtext(ylab, side = 2, line = line.ylab+1)
		mtext(ylab.sub, side = 2, line = line.ylab, font = 3, cex = 0.8)
	}else mtext(ylab, side = 2, line = line.ylab)
	if(!is.null(location)) mtext(location, side = 3, outer = FALSE, adj = 1, line = 0, cex = 0.6) 

	abline(h = axTicks(2), col = "lightgray", lty = "dotted")
	abline(v = xTck, col = "lightgray", lty = "dotted")

	if(plotl$type == 'both') lines(x, y, type = 'o', col = plotl$col$line, lwd = plotl$lwd,
									pch = 21, bg = plotl$col$points, cex = plotl$cex)
	if(plotl$type == 'line') lines(x, y, type = 'l', col = plotl$col$line, lwd = plotl$lwd)

	collegend <- NULL
	txtlegend <- NULL
	if(legends$add$mean){
		moy <- mean(y, na.rm = TRUE)
		abline(h = moy, col = legends$col$mean, lwd = legends$lwd$mean)
		collegend <- c(collegend, legends$col$mean)
		txtlegend <- c(txtlegend, paste(legends$text$mean, "[", round(moy, 4), "]"))
	}
	if(legends$add$linear){
		reglm <- lm(y~x)
		abline(reglm, col = legends$col$linear, lwd = legends$lwd$linear)
		collegend <- c(collegend, legends$col$linear)
		txtlegend <- c(txtlegend, paste(legends$text$linear, "[",
								"Intercept:", round(reglm$coef[1], 4), ";",
								"Slope:", round(reglm$coef[2], 4), "]"))
	}
	if(legends$add$tercile){
		terc <- quantile8(y, probs = c(0.33333, 0.66667))
		abline(h = terc[1], col = legends$col$tercile1, lwd = legends$lwd$tercile)
		abline(h = terc[2], col = legends$col$tercile2, lwd = legends$lwd$tercile)
		collegend <- c(collegend, legends$col$tercile1, legends$col$tercile2)
		txtlegend <- c(txtlegend, paste(legends$text$tercile1, "[", round(terc[1], 4), "]"),
								  paste(legends$text$tercile2, "[", round(terc[2], 4), "]"))
	}
	par(op)

	op <- par(mar = par.legend)
	if(legends$add$mean | legends$add$tercile | legends$add$linear){
		plot.new()
		ncol <- if(length(txtlegend) > 1) 2 else 1
		legend("center", "groups", legend = txtlegend, col = collegend, lwd = 3, ncol = ncol, cex = 1.2)
	}else plot.new()
	par(op)

	op <- par(mar = par.title)
	if(draw.title){
		plot(1, type = 'n', xaxt = 'n', yaxt = 'n', xlab = '', ylab = '')
		bbx <- par("usr")
		rect(bbx[1], bbx[3], bbx[2], bbx[4], col = "ghostwhite")
		text(1, 1, title, cex = 1.5, font = 2)
	}else plot.new()
	par(op)
}

graphs.plot.bar <- function(x, y, xlim = NULL, ylim = NULL, origindate = NULL,
									xlab = '', ylab = '', ylab.sub = NULL,
									title = '', title.position = 'top', axis.font = 1,
									barcol = "darkblue", location = NULL)
{
	if(length(y[!is.na(y)]) == 0){
		x0 <- seq_along(x)
		if(length(x0) == 0) x <- x0
		y <- rep(0, length(x0))
		plot(x, y, type = 'n', yaxt = 'n', xlab = xlab, ylab = ylab, main = title)
		Insert.Messages.Out("No data to plot", format = TRUE)
		return(NULL)
	}
	if(is.null(xlim)) xlim <- range(x, na.rm = TRUE)
	if(is.null(ylim)) ylim <- range(pretty(y))

	if(xlim[1] == xlim[2]) xlim <- xlim+c(-0.5, 0.5)
	if(xlim[2]-xlim[1] == 1) xlim <- xlim+c(-0.5, 0.5)

	nylab <- max(nchar(as.character(pretty(y))), na.rm = TRUE)
	line.ylab <- if(nylab < 2) 2 else nylab

	draw.title <- if(missing(title) | str_trim(title) == "") FALSE else TRUE
	nr.ylab <- str_count(ylab, pattern = "\n")
	par.mar.2 <- ifelse(ylab == '', 4.5,
					ifelse(nr.ylab == 0, 5.1,
					ifelse(nr.ylab == 1, 5.5, 6.0)))
	par.mar.2 <- par.mar.2 + nylab/6

	if(draw.title){
		if(missing(title.position)) title.position <- 'top'
		nr.title <- str_count(title, pattern = "\n")
		ttl.h <- if(nr.title == 0) 0.1 else if(nr.title == 1) 0.13 else 0.19
		if(title.position == 'bottom'){
			plot.position <- matrix(1:2, ncol = 1)
			plot.heights <- c(0.9, ttl.h)
			par.plot <- c(3.5, par.mar.2, 2.1, 2.1)
			par.title <- c(1, par.mar.2, 0, 2.1)
		}else{
			plot.position <- matrix(c(2, 1), ncol = 1)
			plot.heights <- c(ttl.h, 0.9)
			par.plot <- c(3.5, par.mar.2, 1.5, 2.1)
			par.title <- c(0, par.mar.2, 1, 2.1)
		}
	}else{
		plot.position <- matrix(1:2, ncol = 1)
		plot.heights <- c(0.9, 0.01)
		par.plot <- c(3.5, par.mar.2, 2.1, 2.1)
		par.title <- c(0, par.mar.2, 0, 2.1)
	}

	layout(plot.position, widths = 1, heights = plot.heights, respect = FALSE)

	op <- par(mar = par.plot)
	plot(x, y, type = 'n', xlab = '', ylab = '', axes = FALSE, xlim = xlim, ylim = ylim)
	minTck <- axTicks(2)
	minTck <- minTck[-length(minTck)] + diff(minTck)/2
	minTck <-c(min(axTicks(2))-diff(minTck)[1]/2, minTck, max(axTicks(2))+diff(minTck)[1]/2)
	abline(h = axTicks(2), col = "lightgray", lty = "solid", lwd = 0.8)
	abline(h = minTck, col = "lightgray", lty = "dotted")

	if(is(x, "Date")){
		xTck <- axTicks.Date(x, 1)
		axis.foo <- axis.Date
		bar.width <- round(60*(as.numeric(diff(range(xlim)))/min(as.numeric(diff(x)), na.rm = TRUE))^(-0.508775))
		if(as.numeric(diff(xlim)) > 1095){
			xminor <- seq(as.Date(paste0(format(xlim[1], "%Y"), "-01-01")),
						as.Date(paste0(as.numeric(format(xlim[2], "%Y"))+1, "-01-01")), "year")
			xminor <- xminor[!xminor%in%xTck]
		}else xminor <- NULL
	}else{
		xTck <- axTicks(1)
		# xTck <- xTck[!xTck%%1]
		xTck <- xTck[sapply(xTck, function(e) min(abs(c(e%%1, e%%1-1))) < 1e-10)]
		axis.foo <- axis
		bar.width <- round(60*as.numeric(diff(range(xlim)))^(-0.508775))
		if(as.numeric(diff(xlim)) > 5){
			xminor <- seq(floor(xlim[1]), floor(xlim[2]), 1)
			xminor <- xminor[!xminor%in%xTck]
		}else xminor <- NULL
	}

	lines(x, y, type = "h", lwd = bar.width, lend = "butt", col = barcol)

	axis.foo(1, at = xTck, font = axis.font)
	if(length(xminor) > 0) axis.foo(1, at = xminor, labels = NA, tcl = par("tcl")*0.5)
	if(!is.null(origindate)){
		yaxlab <- format(as.Date(axTicks(2), origin = origindate), '%d-%b')
		axis(2, at = axTicks(2), labels = yaxlab, las = 2, font = axis.font)
	}else axis(2, at = axTicks(2), font = axis.font, las = 1)

	mtext(xlab, side = 1, line = 2.5)
	if(!is.null(ylab.sub)){
		mtext(ylab, side = 2, line = line.ylab+1)
		mtext(ylab.sub, side = 2, line = line.ylab, font = 3, cex = 0.8)
	}else mtext(ylab, side = 2, line = line.ylab)

	box(bty = 'l')
	box(bty = '7', col = 'gray')
	if(!is.null(location)) mtext(location, side = 3, outer = FALSE, adj = 1, line = 0, cex = 0.6) 
	par(op)

	op <- par(mar = par.title)
	if(draw.title){
		plot(1, type = 'n', xaxt = 'n', yaxt = 'n', xlab = '', ylab = '')
		bbx <- par("usr")
		rect(bbx[1], bbx[3], bbx[2], bbx[4], col = "ghostwhite")
		text(1, 1, title, cex = 0.9, font = 2)
	}else plot.new()
	par(op)
}

graphs.plot.proba <- function(dat, xlim = NULL, ylim = NULL, origindate = NULL,
									xlab = '', xlab.sub = NULL, ylab = "Probability of Exceeding",
									title = '', title.position = 'bottom', axis.font = 1,
									proba = NULL, plotl = NULL, plotp = NULL, location = NULL)
{
	if(is.null(plotl$type)) plotl$type <- 'both'
	if(is.null(plotl$col$line)) plotl$col$line <- "blue"
	if(is.null(plotl$col$points)) plotl$col$points <- "lightblue"
	if(is.null(plotl$lwd)) plotl$lwd <- 2
	if(is.null(plotl$cex)) plotl$cex <- 0.8

	if(is.null(plotp$col)) plotp$col <- "black"
	if(is.null(plotp$lwd)) plotp$lwd <- 2

	if(is.null(proba$theoretical)) proba$theoretical <- FALSE
	if(is.null(proba$gof.c)) proba$gof.c <- 'ad'
	if(is.null(proba$distr)) proba$distr <- c("norm", "snorm", "lnorm", "gamma", "weibull")

	####
	dat <- dat[!is.na(dat)]
	if(length(dat) < 7){
		x <- y <- 1:100
		plot(x, y, type = 'n', yaxt = 'n', xlab = xlab, ylab = ylab, main = title)
		Insert.Messages.Out("Not enough data to fit a distribution", format = TRUE)
		return(NULL)
	}

	if(is.null(ylim)) xlim <- range(dat, na.rm = TRUE)
	if(is.null(ylim)) ylim <- c(0, 100)

	draw.title <- if(missing(title) | str_trim(title) == "") FALSE else TRUE
	nr.xlab <- str_count(xlab, pattern = "\n")
	line.xlab <- ifelse(xlab == '', 1,
					ifelse(nr.xlab == 0, 2,
					ifelse(nr.xlab == 1, 3, 4)))
	par.mar.1 <- ifelse(xlab == '', 3.1,
					ifelse(nr.xlab == 0, 3.5,
					ifelse(nr.xlab == 1, 4.1, 5.4)))
	nr.ylab <- str_count(ylab, pattern = "\n")
	par.mar.2 <- ifelse(ylab == '', 4.5,
					ifelse(nr.ylab == 0, 4.5,
					ifelse(nr.ylab == 1, 5.5, 6.5)))
	if(draw.title){
		if(missing(title.position)) title.position <- 'top'
		nr.title <- str_count(title, pattern = "\n")
		ttl.h <- if(nr.title == 0) 0.1 else if(nr.title == 1) 0.13 else 0.19
		if(title.position == 'bottom'){
			plot.position <- matrix(1:2, ncol = 1)
			plot.heights <- c(0.9, ttl.h)
			par.plot <- c(par.mar.1, par.mar.2, 2.1, 2.1)
			par.title <- c(1, par.mar.2, 0, 2.1)
		}else{
			plot.position <- matrix(c(2, 1), ncol = 1)
			plot.heights <- c(ttl.h, 0.9)
			par.plot <- c(par.mar.1, par.mar.2, 1.5, 2.1)
			par.title <- c(0, par.mar.2, 1, 2.1)
		}
	}else{
		plot.position <- matrix(1:2, ncol = 1)
		plot.heights <- c(0.9, 0.01)
		par.plot <-  c(par.mar.1, par.mar.2, 2.1, 2.1)
		par.title <- c(0, par.mar.2, 0, 2.1)
	}

	layout(plot.position, widths = 1, heights = plot.heights, respect = FALSE)

	op <- par(mar = par.plot)
	plot(1, type = 'n', xaxt = 'n', yaxt = 'n', xlim = xlim, ylim = ylim, xlab = '', ylab = '')
	xminTck <- axTicks(1)
	xminTck <- xminTck[-length(xminTck)] + diff(xminTck)/2
	xminTck <-c(min(axTicks(2))-diff(xminTck)[1]/2, xminTck, max(axTicks(2))+diff(xminTck)[1]/2)
	yminTck <- axTicks(2)
	yminTck <- yminTck[-length(yminTck)] + diff(yminTck)/2
	yminTck <-c(min(axTicks(2))-diff(yminTck)[1]/2, yminTck, max(axTicks(2))+diff(yminTck)[1]/2)
	abline(h = axTicks(2), col = "lightgray", lty = "solid", lwd = 0.8)
	abline(h = yminTck, col = "lightgray", lty = "dotted")
	abline(v = axTicks(1), col = "lightgray", lty = "solid", lwd = 0.8)
	abline(v = xminTck, col = "lightgray", lty = "dotted")

 	if(!is.null(origindate)){
		xaxlab <- format(as.Date(axTicks(1), origin = origindate), '%d-%b')
		axis(1, at = axTicks(1), labels = xaxlab, font = axis.font)
	}else axis(1, at = axTicks(1), font = axis.font)
	mtext(xlab, side = 1, line = line.xlab)
	if(!is.null(xlab.sub)) mtext(xlab.sub, side = 1, line = line.xlab+1, font = 3, cex = 0.8)

	yaxlab <- paste0(axTicks(2), "%")
	axis(2, at = axTicks(2), labels = yaxlab, las = 2, font = axis.font)
	mtext(ylab, side = 2, line = 3)

	####
	fn <- ecdf(dat)
	x <- sort(dat)
	y <- 100*(1-fn(x))

	if(plotl$type == 'both') lines(x, y, type = 'o', col = plotl$col$line, lwd = plotl$lwd,
									pch = 21, bg = plotl$col$points, cex = plotl$cex)
	if(plotl$type == 'line') lines(x, y, type = 'l', col = plotl$col$line, lwd = plotl$lwd)

	####
	if(proba$theoretical){
		fit.distrs <- fit.distributions(x, proba$distr)
		if(!is.null(fit.distrs)){
			gof <- try(gofstat(fit.distrs), silent = TRUE)
			if(!inherits(gof, "try-error")){
				imin <- which.min(gof[[proba$gof.c]])
				plotTheo <- TRUE
			}else plotTheo <- FALSE
		}else plotTheo <- FALSE

		if(plotTheo){
			selected.distr <- fit.distrs[[imin]]$distname
			selected.pars <- as.list(fit.distrs[[imin]]$estimate)
			pdists <- function(x){
				foo <- get(paste0("p", selected.distr), mode = "function")
				do.call(foo, c(list(q = x), selected.pars))
			}
			curve(100 * (1 - pdists(x)), from = xlim[1], to = xlim[2], add = TRUE, lwd = plotp$lwd, col = plotp$col)
			legend("topright", 
				c(paste0("distr: ", selected.distr), sapply(seq_along(selected.pars),
						function(j) paste0(names(selected.pars)[j], ": ", round(selected.pars[[j]], 5)))),
				box.lwd = 0, box.col = "gray97", bg = "gray98", cex = 0.6)
		}
	}
	if(!is.null(location)) mtext(location, side = 3, outer = FALSE, adj = 1, line = 0, cex = 0.6)
	box()
	par(op)

	op <- par(mar = par.title)
	if(draw.title){
		plot(1, type = 'n', xaxt = 'n', yaxt = 'n', xlab = '', ylab = '')
		bbx <- par("usr")
		rect(bbx[1], bbx[3], bbx[2], bbx[4], col = "ghostwhite")
		text(1, 1, title, cex = 0.9, font = 2)
	}else plot.new()
	par(op)
}

graphs.plot.line.ENSO <- function(x, y, oni, xlim = NULL, ylim = NULL, origindate = NULL,
										xlab = '', ylab = '', ylab.sub = NULL,
										title = '', title.position = 'top', axis.font = 1,
										plotl = NULL, legends = NULL, location = NULL)
{
	if(is.null(plotl$col$line)) plotl$col$line <- 'black'
	if(is.null(plotl$col$points)) plotl$col$points <- c("blue", "gray", "red")
	if(is.null(plotl$lwd)) plotl$lwd <- 2
	if(is.null(plotl$cex)) plotl$cex <- 2

	if(is.null(legends$add$mean)) legends$add$mean <- FALSE
	if(is.null(legends$add$tercile)) legends$add$tercile <- FALSE
	if(is.null(legends$add$linear)) legends$add$linear <- FALSE
	if(is.null(legends$col$mean)) legends$col$mean <- "darkblue"
	if(is.null(legends$col$tercile1)) legends$col$tercile1 <- "chartreuse4"
	if(is.null(legends$col$tercile2)) legends$col$tercile2 <- "darkgoldenrod4"
	if(is.null(legends$col$linear)) legends$col$linear <- "purple3"
	if(is.null(legends$text$mean)) legends$text$mean <- "Average"
	if(is.null(legends$text$tercile1)) legends$text$tercile1 <- "Tercile 0.33333"
	if(is.null(legends$text$tercile2)) legends$text$tercile2 <- "Tercile 0.66666"
	if(is.null(legends$text$linear)) legends$text$linear <- "Trend line"
	if(is.null(legends$lwd$mean)) legends$lwd$mean <- 2
	if(is.null(legends$lwd$tercile)) legends$lwd$tercile <- 2
	if(is.null(legends$lwd$linear)) legends$lwd$linear <- 2

	if(length(y[!is.na(y)]) == 0){
		x0 <- seq_along(x)
		if(length(x0) == 0) x <- x0
		y <- rep(0, length(x0))
		plot(x, y, type = 'n', yaxt = 'n', xlab = '', ylab = '', main = title)
		Insert.Messages.Out("No data to plot", format = TRUE)
		return(NULL)
	}

	if(is.null(xlim)) xlim <- range(x, na.rm = TRUE)
	if(is.null(ylim)) ylim <- range(pretty(y))

	if(xlim[1] == xlim[2]) xlim <- xlim+c(-0.5, 0.5)
	if(xlim[2]-xlim[1] == 1) xlim <- xlim+c(-0.5, 0.5)

	draw.title <- if(missing(title) | str_trim(title) == "") FALSE else TRUE
	nr.ylab <- str_count(ylab, pattern = "\n")
	par.mar.2 <- ifelse(ylab == '', 6.0,
					ifelse(nr.ylab == 0, 6.5,
					ifelse(nr.ylab == 1, 7.5, 8.8)))
	if(draw.title){
		if(missing(title.position)) title.position <- 'top'
		nr.title <- str_count(title, pattern = "\n")
		ttl.h <- if(nr.title == 0) 0.1 else if(nr.title == 1) 0.13 else 0.19
		if(title.position == 'bottom'){
			plot.position <- matrix(1:3, ncol = 1)
			plot.heights <- c(0.9, 0.2, ttl.h)
			par.plot <- c(3.5, par.mar.2, 2.1, 2.1)
			par.legend <- c(0, par.mar.2, 0, 2.1)
			par.title <- c(1, par.mar.2, 0, 2.1)
		}else{
			plot.position <- matrix(c(3, 1, 2), ncol = 1)
			plot.heights <- c(ttl.h, 0.9, 0.2)
			par.plot <- c(3.5, par.mar.2, 1.5, 2.1)
			par.legend <- c(1, par.mar.2, 0, 2.1)
			par.title <- c(0, par.mar.2, 1, 2.1)
		}
	}else{
		plot.position <- matrix(1:3, ncol = 1)
		plot.heights <- c(0.9, 0.2, 0.01)
		par.plot <- c(3.5, par.mar.2, 2.1, 2.1)
		par.legend <- c(0, par.mar.2, 0, 2.1)
		par.title <- c(0, par.mar.2, 0, 2.1)
	}

	layout(plot.position, widths = 1, heights = plot.heights, respect = FALSE)

	op <- par(mar = par.plot)
	plot(x, y, type = 'n', xaxt = 'n', yaxt = 'n', xlab = '', ylab = '', xlim = xlim, ylim = ylim)

	xTck <- axTicks(1)
	xTck <- xTck[sapply(xTck, function(e) min(abs(c(e%%1, e%%1-1))) < 1e-10)]
	if(as.numeric(diff(xlim)) > 5){
		xminor <- seq(floor(xlim[1]), floor(xlim[2]), 1)
		xminor <- xminor[!xminor%in%xTck]
	}else xminor <- NULL

	abline(h = axTicks(2), col = "lightgray", lty = "dotted")
	abline(v = xTck, col = "lightgray", lty = "dotted")

	lines(x, y, col = plotl$col$line, lwd = plotl$lwd)
	points(x, y, pch = 21, col = plotl$col$line, bg = plotl$col$points[oni], cex = plotl$cex)

	collegend <- NULL
	txtlegend <- NULL
	if(legends$add$mean){
		abline(h = mean(y, na.rm = TRUE), col = legends$col$mean, lwd = legends$lwd$mean)
		collegend <- c(collegend, legends$col$mean)
		txtlegend <- c(txtlegend, legends$text$mean)
	}
	if(legends$add$linear){
		abline(lm(y~x), col = legends$col$linear, lwd = legends$lwd$linear)
		collegend <- c(collegend, legends$col$linear)
		txtlegend <- c(txtlegend, legends$text$linear)
	}
	if(legends$add$tercile){
		terc <- quantile8(y, probs = c(0.33333, 0.66667))
		abline(h = terc[1], col = legends$col$tercile1, lwd = legends$lwd$tercile)
		abline(h = terc[2], col = legends$col$tercile2, lwd = legends$lwd$tercile)
		collegend <- c(collegend, legends$col$tercile1, legends$col$tercile2)
		txtlegend <- c(txtlegend, legends$text$tercile1, legends$text$tercile2)
	}

	axis(1, at = xTck, font = axis.font, cex.axis = 1.5)
	if(length(xminor) > 0) axis(1, at = xminor, labels = NA, tcl = par("tcl")*0.5)
	mtext(xlab, side = 1, line = 2.5)
	if(!is.null(origindate)){
		yaxlab <- format(as.Date(axTicks(2), origin = origindate), '%d-%b')
		axis(2, at = axTicks(2), labels = yaxlab, las = 2, font = axis.font, cex.axis = 1.5)
	}else axis(2, at = axTicks(2), font = axis.font, las = 1, cex.axis = 1.5)

	line <- if(max(nchar(as.character(axTicks(2)))) > 2) 4 else 3
	if(!is.null(ylab.sub)){
		mtext(ylab, side = 2, line = line+1)
		mtext(ylab.sub, side = 2, line = line, font = 3, cex = 0.8)
	}else mtext(ylab, side = 2, line = line)
	if(!is.null(location)) mtext(location, side = 3, outer = FALSE, adj = 1, line = 0, cex = 0.6) 
	par(op)

	nino <- c('La Niña', 'Neutral', 'El Niño')
	txtlegend <- if(legends$add$mean | legends$add$linear | legends$add$tercile) c(nino, txtlegend) else nino
	collegend <- if(legends$add$mean | legends$add$linear | legends$add$tercile) c(rep(plotl$col$line, 3), collegend) else rep(plotl$col$line, 3)

	op <- par(mar = par.legend)
	plot.new()
	legend("center", "groups", legend = txtlegend, col = collegend, pch = c(rep(21, 3), rep(NA, 4)),
			pt.bg = c(plotl$col$points, rep(NA, 4)), pt.cex = c(rep(2, 3), rep(NA, 4)),
			pt.lwd = c(rep(1, 3), rep(NA, 4)), lwd = 3, ncol = 3, cex = 1.2)
	par(op)

	op <- par(mar = par.title)
	if(draw.title){
		plot(1, type = 'n', xaxt = 'n', yaxt = 'n', xlab = '', ylab = '')
		bbx <- par("usr")
		rect(bbx[1], bbx[3], bbx[2], bbx[4], col = "ghostwhite")
		text(1, 1, title, cex = 1.5, font = 2)
	}else plot.new()
	par(op)
}

graphs.plot.bar.ENSO <- function(x, y, oni, xlim = NULL, ylim = NULL, origindate = NULL,
										xlab = '', ylab = '', ylab.sub = NULL,
										title = '', title.position = 'top', axis.font = 1,
										barcol = c("blue", "gray", "red"), location = NULL)
{
	if(length(y[!is.na(y)]) == 0){
		x0 <- seq_along(x)
		if(length(x0) == 0) x <- x0
		y <- rep(0, length(x0))
		plot(x, y, type = 'n', yaxt = 'n', xlab = xlab, ylab = ylab, main = title)
		Insert.Messages.Out("No data to plot", format = TRUE)
		return(NULL)
	}
	if(is.null(xlim)) xlim <- range(x, na.rm = TRUE)
	if(is.null(ylim)) ylim <- range(pretty(y))

	if(xlim[1] == xlim[2]) xlim <- xlim+c(-0.5, 0.5)
	if(xlim[2]-xlim[1] == 1) xlim <- xlim+c(-0.5, 0.5)

	draw.title <- if(missing(title) | str_trim(title) == "") FALSE else TRUE
	nr.ylab <- str_count(ylab, pattern = "\n")
	par.mar.2 <- ifelse(ylab == '', 6.0,
					ifelse(nr.ylab == 0, 6.5,
					ifelse(nr.ylab == 1, 7.5, 8.8)))
	if(draw.title){
		if(missing(title.position)) title.position <- 'top'
		nr.title <- str_count(title, pattern = "\n")
		ttl.h <- if(nr.title == 0) 0.1 else if(nr.title == 1) 0.13 else 0.19
		if(title.position == 'bottom'){
			plot.position <- matrix(1:3, ncol = 1)
			plot.heights <- c(0.9, 0.13, ttl.h)
			par.plot <- c(3.5, par.mar.2, 2.1, 2.1)
			par.legend <- c(0, par.mar.2, 0, 2.1)
			par.title <- c(1, par.mar.2, 0, 2.1)
		}else{
			plot.position <- matrix(c(3, 1, 2), ncol = 1)
			plot.heights <- c(ttl.h, 0.9, 0.13)
			par.plot <- c(3.5, par.mar.2, 1.5, 2.1)
			par.legend <- c(1, par.mar.2, 0, 2.1)
			par.title <- c(0, par.mar.2, 1, 2.1)
		}
	}else{
		plot.position <- matrix(1:3, ncol = 1)
		plot.heights <- c(0.9, 0.1, 0.01)
		par.plot <- c(3.5, par.mar.2, 2.1, 2.1)
		par.legend <- c(0, par.mar.2, 0, 2.1)
		par.title <- c(0, par.mar.2, 0, 2.1)
	}

	layout(plot.position, widths = 1, heights = plot.heights, respect = FALSE)

	op <- par(mar = par.plot)
	plot(x, y, type = 'n', xlab = '', ylab = '', axes = FALSE, xlim = xlim, ylim = ylim)
	minTck <- axTicks(2)
	minTck <- minTck[-length(minTck)] + diff(minTck)/2
	minTck <-c(min(axTicks(2))-diff(minTck)[1]/2, minTck, max(axTicks(2))+diff(minTck)[1]/2)
	abline(h = axTicks(2), col = "lightgray", lty = "solid", lwd = 0.8)
	abline(h = minTck, col = "lightgray", lty = "dotted")

	bar.width <- round(60*diff(range(xlim))^(-0.508775))
	lines(x, y, type = "h", lwd = bar.width, lend = "butt", col = barcol[oni])

	xTck <- axTicks(1)
	xTck <- xTck[sapply(xTck, function(e) min(abs(c(e%%1, e%%1-1))) < 1e-10)]
	if(as.numeric(diff(xlim)) > 5){
		xminor <- seq(floor(xlim[1]), floor(xlim[2]), 1)
		xminor <- xminor[!xminor%in%xTck]
	}else xminor <- NULL

	axis(1, at = xTck, font = axis.font, cex.axis = 1.5)
	if(length(xminor) > 0) axis(1, at = xminor, labels = NA, tcl = par("tcl")*0.5)
	mtext(xlab, side = 1, line = 2.5)
	# axis(2, at = axTicks(2), las = 1, font = axis.font, cex.axis = 1.5)
	if(!is.null(origindate)){
		yaxlab <- format(as.Date(axTicks(2), origin = origindate), '%d-%b')
		axis(2, at = axTicks(2), labels = yaxlab, las = 2, font = axis.font, cex.axis = 1.5)
	}else axis(2, at = axTicks(2), font = axis.font, las = 1, cex.axis = 1.5)


	line <- if(max(nchar(as.character(axTicks(2)))) > 2) 4 else 3
	if(!is.null(ylab.sub)){
		mtext(ylab, side = 2, line = line+1)
		mtext(ylab.sub, side = 2, line = line, font = 3, cex = 0.8)
	}else mtext(ylab, side = 2, line = line)

	box(bty = 'l')
	box(bty = '7', col = 'gray')
	if(!is.null(location)) mtext(location, side = 3, outer = FALSE, adj = 1, line = 0, cex = 0.6) 
	par(op)

	op <- par(mar = par.legend)
	plot.new()
	nino <- c('La Niña', 'Neutral', 'El Niño')
	legend("center", "groups", legend = nino, fill = barcol, horiz = TRUE, cex = 1.2)
	par(op)

	op <- par(mar = par.title)
	if(draw.title){
		plot(1, type = 'n', xaxt = 'n', yaxt = 'n', xlab = '', ylab = '')
		bbx <- par("usr")
		rect(bbx[1], bbx[3], bbx[2], bbx[4], col = "ghostwhite")
		text(1, 1, title, cex = 1.5, font = 2)
	}else plot.new()
	par(op)
}

graphs.plot.proba.ENSO <- function(dat, oni, xlim = NULL, ylim = NULL, origindate = NULL,
											xlab = '', xlab.sub = NULL, ylab = "Probability of Exceeding",
	 										title = '', title.position = 'bottom', axis.font = 1,
	 										plotl = NULL, location = NULL)
{
	if(is.null(plotl$type)) plotl$type <- 'both'
	if(is.null(plotl$lwd)) plotl$lwd <- 2
	if(is.null(plotl$cex)) plotl$cex <- 1.4
	if(is.null(plotl$all$line)) plotl$all$line <- "black"
	if(is.null(plotl$all$points)) plotl$all$points <- "lightgray"
	if(is.null(plotl$nino$line)) plotl$nino$line <- "red"
	if(is.null(plotl$nino$points)) plotl$nino$points <- "lightpink"
	if(is.null(plotl$nina$line)) plotl$nina$line <- "blue"
	if(is.null(plotl$nina$points)) plotl$nina$points <- "lightblue"
	if(is.null(plotl$neutre$line)) plotl$neutre$line <- "gray"
	if(is.null(plotl$neutre$points)) plotl$neutre$points <- "lightgray"

	dat <- dat[!is.na(dat)]
	if(length(dat) < 7){
		x <- y <- 1:100
		plot(x, y, type = 'n', yaxt = 'n', xlab = xlab, ylab = ylab, main = title)
		Insert.Messages.Out("Not enough data to fit a distribution", format = TRUE)
		return(NULL)
	}

	if(is.null(ylim)) xlim <- range(dat, na.rm = TRUE)
	if(is.null(ylim)) ylim <- c(0, 100)

	draw.title <- if(missing(title) | str_trim(title) == "") FALSE else TRUE
	nr.xlab <- str_count(xlab, pattern = "\n")
	line.xlab <- ifelse(xlab == '', 1,
					ifelse(nr.xlab == 0, 2.5,
					ifelse(nr.xlab == 1, 4, 5.5)))
	par.mar.1 <- ifelse(xlab == '', 3.0,
					ifelse(nr.xlab == 0, 4.0,
					ifelse(nr.xlab == 1, 5.0, 6.5)))
	nr.ylab <- str_count(ylab, pattern = "\n")
	par.mar.2 <- ifelse(ylab == '', 6.0,
					ifelse(nr.ylab == 0, 6.5,
					ifelse(nr.ylab == 1, 7.0, 8.5)))
	if(draw.title){
		if(missing(title.position)) title.position <- 'top'
		nr.title <- str_count(title, pattern = "\n")
		ttl.h <- if(nr.title == 0) 0.1 else if(nr.title == 1) 0.13 else 0.19
		if(title.position == 'bottom'){
			plot.position <- matrix(1:3, ncol = 1)
			plot.heights <- c(0.9, 0.13, ttl.h)
			par.plot <- c(par.mar.1, par.mar.2, 2.1, 2.1)
			par.legend <- c(0, par.mar.2, 0, 2.1)
			par.title <- c(1, par.mar.2, 0, 2.1)
		}else{
			plot.position <- matrix(c(3, 1, 2), ncol = 1)
			plot.heights <- c(ttl.h, 0.9, 0.13)
			par.plot <- c(par.mar.1, par.mar.2, 2.1, 2.1)
			par.legend <- c(1, par.mar.2, 0, 2.1)
			par.title <- c(0, par.mar.2, 1, 2.1)
		}
	}else{
		plot.position <- matrix(1:3, ncol = 1)
		plot.heights <- c(0.9, 0.15, 0.01)
		par.plot <-  c(par.mar.1, par.mar.2, 2.1, 2.1)
		par.legend <- c(0, par.mar.2, 0, 2.1)
		par.title <- c(0, par.mar.2, 0, 2.1)
	}

	layout(plot.position, widths = 1, heights = plot.heights, respect = FALSE)

	op <- par(mar = par.plot)
	plot(1, type = 'n', xaxt = 'n', yaxt = 'n', xlim = xlim, ylim = ylim, xlab = '', ylab = '')
	xminTck <- axTicks(1)
	xminTck <- xminTck[-length(xminTck)] + diff(xminTck)/2
	xminTck <-c(min(axTicks(2))-diff(xminTck)[1]/2, xminTck, max(axTicks(2))+diff(xminTck)[1]/2)
	yminTck <- axTicks(2)
	yminTck <- yminTck[-length(yminTck)] + diff(yminTck)/2
	yminTck <-c(min(axTicks(2))-diff(yminTck)[1]/2, yminTck, max(axTicks(2))+diff(yminTck)[1]/2)
	abline(h = axTicks(2), col = "lightgray", lty = "solid", lwd = 0.8)
	abline(h = yminTck, col = "lightgray", lty = "dotted")
	abline(v = axTicks(1), col = "lightgray", lty = "solid", lwd = 0.8)
	abline(v = xminTck, col = "lightgray", lty = "dotted")

 	if(!is.null(origindate)){
		xaxlab <- format(as.Date(axTicks(1), origin = origindate), '%d-%b')
		axis(1, at = axTicks(1), labels = xaxlab, font = axis.font, cex.axis = 1.5)
	}else axis(1, at = axTicks(1), font = axis.font, cex.axis = 1.5)
	mtext(xlab, side = 1, line = line.xlab)
	if(!is.null(xlab.sub)) mtext(xlab.sub, side = 1, line = line.xlab+1, font = 3, cex = 0.8)

	yaxlab <- paste0(axTicks(2), "%")
	axis(2, at = axTicks(2), labels = yaxlab, las = 2, font = axis.font, cex.axis = 1.5)
	mtext(ylab, side = 2, line = 4)

	####
	fn0 <- ecdf(dat)
	x0 <- sort(dat)
	y0 <- 100*(1-fn0(x0))

	x1 <- sort(dat[oni == 1])
	fn1 <- ecdf(x1)
	y1 <- 100*(1-fn1(x1))

	x2 <- sort(dat[oni == 2])
	fn2 <- ecdf(x2)
	y2 <- 100*(1-fn2(x2))

	x3 <- sort(dat[oni == 3])
	fn3 <- ecdf(x3)
	y3 <- 100*(1-fn3(x3))

	if(plotl$type == 'both'){
		lines(x0, y0, type = 'o', col = plotl$all$line, lwd = plotl$lwd, pch = 21, bg = plotl$all$points, cex = plotl$cex)
		lines(x1, y1, type = 'o', col = plotl$nina$line, lwd = plotl$lwd, pch = 21, bg = plotl$nina$points, cex = plotl$cex)
		lines(x2, y2, type = 'o', col = plotl$neutre$line, lwd = plotl$lwd, pch = 21, bg = plotl$neutre$points, cex = plotl$cex)
		lines(x3, y3, type = 'o', col = plotl$nino$line, lwd = plotl$lwd, pch = 21, bg = plotl$nino$points, cex = plotl$cex)
	}
	if(plotl$type == 'line'){
		lines(x0, y0, type = 'l', col = plotl$all$line, lwd = plotl$lwd)
		lines(x1, y1, type = 'l', col = plotl$nina$line, lwd = plotl$lwd)
		lines(x2, y2, type = 'l', col = plotl$neutre$line, lwd = plotl$lwd)
		lines(x3, y3, type = 'l', col = plotl$nino$line, lwd = plotl$lwd)
	}

	if(!is.null(location)) mtext(location, side = 3, outer = FALSE, adj = 1, line = 0, cex = 0.6) 
	par(op)

	op <- par(mar = par.legend)
	plot.new()
	if(plotl$type == 'both'){
		legend("center", "groups", legend = c('All years', 'La Niña', 'Neutral', 'El Niño'),
				col = c(plotl$all$line, plotl$nina$line, plotl$neutre$line, plotl$nino$line),
				lwd = 2, lty = 1, pch = 21, horiz = TRUE, cex = 1.4,
				pt.bg = c(plotl$all$points, plotl$nina$points, plotl$neutre$points, plotl$nino$points))
	}

	if(plotl$type == 'line'){
		legend("center", "groups", legend = c('All years', 'La Niña', 'Neutral', 'El Niño'),
				col = c(plotl$all$line, plotl$nina$line, plotl$neutre$line, plotl$nino$line),
				lwd = 2, lty = 1, horiz = TRUE)
	}
	par(op)

	op <- par(mar = par.title)
	if(draw.title){
		plot(1, type = 'n', xaxt = 'n', yaxt = 'n', xlab = '', ylab = '')
		bbx <- par("usr")
		rect(bbx[1], bbx[3], bbx[2], bbx[4], col = "ghostwhite")
		text(1, 1, title, cex = 1.5, font = 2)
	}
	par(op)
}

graphs.plot.bar.Anomaly <- function(x, y, period = c(1981, 2010), percent = TRUE,
											xlim = NULL, ylim = NULL, xlab = '', ylab = '', ylab.sub = NULL,
											title = '', title.position = 'top', axis.font = 1,
											barcol = c("blue", "red"), location = NULL)
{
	if(length(y[!is.na(y)]) < 10){
		x0 <- seq_along(x)
		if(length(x0) == 0) x <- x0
		y <- rep(0, length(x0))
		plot(x, y, type = 'n', yaxt = 'n', xlab = xlab, ylab = ylab, main = title)
		if(is.null(period)) Insert.Messages.Out("No data to plot", format = TRUE)
		else Insert.Messages.Out("Not enough data to compute climatology", format = TRUE)
		return(NULL)
	}

	if(!is.null(period)){
		moy <- mean(y[x >= period[1] & x <= period[2]], na.rm = TRUE)
		y <- if(percent) 100*(y-moy)/(moy+0.01) else y-moy
	}

	if(is.null(xlim)) xlim <- range(x, na.rm = TRUE)
	if(is.null(ylim)) ylim <- range(pretty(y))

	if(xlim[1] == xlim[2]) xlim <- xlim[1]+c(-0.5, 0.5)
	if(xlim[2]-xlim[1] == 1) xlim <- xlim+c(-0.5, 0.5)

	draw.title <- if(missing(title) | str_trim(title) == "") FALSE else TRUE
	nr.ylab <- str_count(ylab, pattern = "\n")
	par.mar.2 <- ifelse(ylab == '', 4.5,
					ifelse(nr.ylab == 0, 5.1,
					ifelse(nr.ylab == 1, 5.5, 6.5)))
	if(draw.title){
		if(missing(title.position)) title.position <- 'top'
		nr.title <- str_count(title, pattern = "\n")
		ttl.h <- if(nr.title == 0) 0.1 else if(nr.title == 1) 0.13 else 0.19
		if(title.position == 'bottom'){
			plot.position <- matrix(1:2, ncol = 1)
			plot.heights <- c(0.9, ttl.h)
			par.plot <- c(3.5, par.mar.2, 2.1, 2.1)
			par.title <- c(1, par.mar.2, 0, 2.1)
		}else{
			plot.position <- matrix(c(2, 1), ncol = 1)
			plot.heights <- c(ttl.h, 0.9)
			par.plot <- c(3.5, par.mar.2, 1.5, 2.1)
			par.title <- c(0, par.mar.2, 1, 2.1)
		}
	}else{
		plot.position <- matrix(1:2, ncol = 1)
		plot.heights <- c(0.9, 0.01)
		par.plot <- c(3.5, par.mar.2, 2.1, 2.1)
		par.title <- c(0, par.mar.2, 0, 2.1)
	}

	layout(plot.position, widths = 1, heights = plot.heights, respect = FALSE)

	op <- par(mar = par.plot)
	plot(x, y, type = 'n', xlab = '', ylab = '', axes = FALSE, xlim = xlim, ylim = ylim)
	minTck <- axTicks(2)
	minTck <- minTck[-length(minTck)] + diff(minTck)/2
	minTck <-c(min(axTicks(2))-diff(minTck)[1]/2, minTck, max(axTicks(2))+diff(minTck)[1]/2)
	abline(h = axTicks(2), col = "lightgray", lty = "solid", lwd = 0.8)
	abline(h = minTck, col = "lightgray", lty = "dotted")

	if(is(x, "Date")){
		xTck <- axTicks.Date(x, 1)
		axis.foo <- axis.Date
		bar.width <- round(58*(as.numeric(diff(range(xlim)))/min(as.numeric(diff(x)), na.rm = TRUE))^(-0.508775))
		if(as.numeric(diff(xlim)) > 1095){
			xminor <- seq(as.Date(paste0(format(xlim[1], "%Y"), "-01-01")),
						as.Date(paste0(as.numeric(format(xlim[2], "%Y"))+1, "-01-01")), "year")
			xminor <- xminor[!xminor%in%xTck]
		}else xminor <- NULL
	}else{
		xTck <- axTicks(1)
		xTck <- xTck[sapply(xTck, function(e) min(abs(c(e%%1, e%%1-1))) < 1e-10)]
		axis.foo <- axis
		bar.width <- round(60*as.numeric(diff(range(xlim)))^(-0.508775))
		if(as.numeric(diff(xlim)) > 5){
			xminor <- seq(floor(xlim[1]), floor(xlim[2]), 1)
			xminor <- xminor[!xminor%in%xTck]
		}else xminor <- NULL
	}

	kol <- ifelse(y > 0, 2, 1)
	lines(x, y, type = "h", lwd = bar.width, lend = "butt", col = barcol[kol])

	axis.foo(1, at = xTck, font = axis.font)
	if(length(xminor) > 0) axis.foo(1, at = xminor, labels = NA, tcl = par("tcl")*0.5)
	axis(2, at = axTicks(2), las = 1, font = axis.font)

	mtext(xlab, side = 1, line = 2.5)
	line <- if(max(nchar(as.character(axTicks(2)))) > 2) 3 else 2
	if(!is.null(ylab.sub)){
		mtext(ylab, side = 2, line = line+1)
		mtext(ylab.sub, side = 2, line = line, font = 3, cex = 0.8)
	}else mtext(ylab, side = 2, line = line)

	box(bty = 'l')
	box(bty = '7', col = 'gray')
	if(!is.null(location)) mtext(location, side = 3, outer = FALSE, adj = 1, line = 0, cex = 0.6) 
	par(op)

	op <- par(mar = par.title)
	if(draw.title){
		plot(1, type = 'n', xaxt = 'n', yaxt = 'n', xlab = '', ylab = '')
		bbx <- par("usr")
		rect(bbx[1], bbx[3], bbx[2], bbx[4], col = "ghostwhite")
		text(1, 1, title, cex = 0.9, font = 2)
	}else plot.new()
	par(op)
}

####################################################################################################

graphs.plot.bar.line <- function(x, y, y0 = 0, yticks = NULL,
								xlim = NULL, ylim = NULL, xlab = '', ylab = '', ylab.sub = NULL,
								title = '', title.position = 'top', axis.font = 1,
								barcol = c("blue", "red"), plot.line = NULL,
								location = NULL)
{
	if(is.null(plot.line$plot)) plot.line$plot <- FALSE
	if(is.null(plot.line$col)) plot.line$col <- "black"
	if(is.null(plot.line$lwd)) plot.line$lwd <- 1.5

	if(length(y[!is.na(y)]) == 0){
		x0 <- seq_along(x)
		if(length(x0) == 0) x <- x0
		y <- rep(0, length(x0))
		plot(x, y, type = 'n', yaxt = 'n', xlab = xlab, ylab = ylab, main = title)
		Insert.Messages.Out("No data to plot", format = TRUE)
		return(NULL)
	}

	if(is.null(xlim)) xlim <- range(x, na.rm = TRUE)
	if(is.null(ylim)) ylim <- range(pretty(y))

	if(xlim[1] == xlim[2]) xlim <- xlim[1]+c(-0.5, 0.5)
	if(xlim[2]-xlim[1] == 1) xlim <- xlim+c(-0.5, 0.5)

	draw.title <- if(missing(title) | str_trim(title) == "") FALSE else TRUE
	nr.ylab <- str_count(ylab, pattern = "\n")
	par.mar.2 <- ifelse(ylab == '', 4.5,
					ifelse(nr.ylab == 0, 5.1,
					ifelse(nr.ylab == 1, 5.5, 6.5)))
	if(draw.title){
		if(missing(title.position)) title.position <- 'top'
		nr.title <- str_count(title, pattern = "\n")
		ttl.h <- if(nr.title == 0) 0.1 else if(nr.title == 1) 0.13 else 0.19
		if(title.position == 'bottom'){
			plot.position <- matrix(1:2, ncol = 1)
			plot.heights <- c(0.9, ttl.h)
			par.plot <- c(3.5, par.mar.2, 2.1, 2.1)
			par.title <- c(1, par.mar.2, 0, 2.1)
		}else{
			plot.position <- matrix(c(2, 1), ncol = 1)
			plot.heights <- c(ttl.h, 0.9)
			par.plot <- c(3.5, par.mar.2, 1.5, 2.1)
			par.title <- c(0, par.mar.2, 1, 2.1)
		}
	}else{
		plot.position <- matrix(1:2, ncol = 1)
		plot.heights <- c(0.9, 0.01)
		par.plot <- c(3.5, par.mar.2, 2.1, 2.1)
		par.title <- c(0, par.mar.2, 0, 2.1)
	}

	layout(plot.position, widths = 1, heights = plot.heights, respect = FALSE)

	op <- par(mar = par.plot)
	plot(x, y, type = 'n', xlab = '', ylab = '', axes = FALSE, xlim = xlim, ylim = ylim)

	if(is.null(yticks)){
		minTck <- axTicks(2)
		minTck <- minTck[-length(minTck)] + diff(minTck)/2
		minTck <-c(min(axTicks(2))-diff(minTck)[1]/2, minTck, max(axTicks(2))+diff(minTck)[1]/2)
		abline(h = axTicks(2), col = "lightgray", lty = "solid", lwd = 0.8)
		abline(h = minTck, col = "lightgray", lty = "dotted")
		yTck <- axTicks(2)
		ylas <- 1
	}else{
		abline(h = yticks, col = "lightgray", lty = "solid", lwd = 0.8)
		yTck <- yticks
		ylas <- 2
	}

	if(is(x, "Date")){
		xTck <- axTicks.Date(x, 1)
		axis.foo <- axis.Date
		bar.width <- round(58*(as.numeric(diff(range(xlim)))/min(as.numeric(diff(x)), na.rm = TRUE))^(-0.508775))
		if(as.numeric(diff(xlim)) > 1095){
			xminor <- seq(as.Date(paste0(format(xlim[1], "%Y"), "-01-01")),
						as.Date(paste0(as.numeric(format(xlim[2], "%Y"))+1, "-01-01")), "year")
			xminor <- xminor[!xminor%in%xTck]
		}else xminor <- NULL
	}else{
		xTck <- axTicks(1)
		xTck <- xTck[sapply(xTck, function(e) min(abs(c(e%%1, e%%1-1))) < 1e-10)]
		axis.foo <- axis
		bar.width <- round(60*as.numeric(diff(range(xlim)))^(-0.508775))
		if(as.numeric(diff(xlim)) > 5){
			xminor <- seq(floor(xlim[1]), floor(xlim[2]), 1)
			xminor <- xminor[!xminor%in%xTck]
		}else xminor <- NULL
	}
	abline(v = xTck, col = "lightgray", lty = "dotted")

	kol <- ifelse(y >= y0, 2, 1)
	lines(x, y, type = "h", lwd = bar.width, lend = "butt", col = barcol[kol])
	abline(h = y0, col = "lightgray", lty = "solid", lwd = 0.8)
	if(plot.line$plot) lines(x, y, lwd = plot.line$lwd, col = plot.line$col)

	axis.foo(1, at = xTck, font = axis.font)
	if(length(xminor) > 0) axis.foo(1, at = xminor, labels = NA, tcl = par("tcl")*0.5)
	axis(2, at = yTck, las = ylas, font = axis.font)

	mtext(xlab, side = 1, line = 2.1)
	line <- if(max(nchar(as.character(axTicks(2)))) > 2) 3.8 else 2.5
	if(!is.null(ylab.sub)){
		mtext(ylab, side = 2, line = line+1)
		mtext(ylab.sub, side = 2, line = line, font = 3, cex = 0.8)
	}else mtext(ylab, side = 2, line = line)

	box(bty = 'l')
	box(bty = '7', col = 'black')
	if(!is.null(location)) mtext(location, side = 3, outer = FALSE, adj = 1, line = 0, cex = 0.6) 
	par(op)

	op <- par(mar = par.title)
	if(draw.title){
		plot(1, type = 'n', xaxt = 'n', yaxt = 'n', xlab = '', ylab = '')
		bbx <- par("usr")
		rect(bbx[1], bbx[3], bbx[2], bbx[4], col = "ghostwhite")
		text(1, 1, title, cex = 0.9, font = 2)
	}else plot.new()
	par(op)
}

graphs.plot.polygon <- function(x, y, y0 = 0, yticks = NULL,
								xlim = NULL, ylim = NULL, xlab = '', ylab = '', ylab.sub = NULL,
								title = '', title.position = 'top', axis.font = 1,
								fillcol = c("blue", "red"), plot.line = NULL,
								location = NULL)
{
	if(is.null(plot.line$plot)) plot.line$plot <- FALSE
	if(is.null(plot.line$col)) plot.line$col <- "black"
	if(is.null(plot.line$lwd)) plot.line$lwd <- 1.5

	if(length(y[!is.na(y)]) == 0){
		x0 <- seq_along(x)
		if(length(x0) == 0) x <- x0
		y <- rep(0, length(x0))
		plot(x, y, type = 'n', yaxt = 'n', xlab = xlab, ylab = ylab, main = title)
		Insert.Messages.Out("No data to plot", format = TRUE)
		return(NULL)
	}

	if(is.null(xlim)) xlim <- range(x, na.rm = TRUE)
	if(is.null(ylim)) ylim <- range(pretty(y))

	if(xlim[1] == xlim[2]) xlim <- xlim[1]+c(-0.5, 0.5)
	if(xlim[2]-xlim[1] == 1) xlim <- xlim+c(-0.5, 0.5)

	draw.title <- if(missing(title) | str_trim(title) == "") FALSE else TRUE
	nr.ylab <- str_count(ylab, pattern = "\n")
	par.mar.2 <- ifelse(ylab == '', 4.5,
					ifelse(nr.ylab == 0, 5.1,
					ifelse(nr.ylab == 1, 5.5, 6.5)))
	if(draw.title){
		if(missing(title.position)) title.position <- 'top'
		nr.title <- str_count(title, pattern = "\n")
		ttl.h <- if(nr.title == 0) 0.1 else if(nr.title == 1) 0.13 else 0.19
		if(title.position == 'bottom'){
			plot.position <- matrix(1:2, ncol = 1)
			plot.heights <- c(0.9, ttl.h)
			par.plot <- c(3.5, par.mar.2, 2.1, 2.1)
			par.title <- c(1, par.mar.2, 0, 2.1)
		}else{
			plot.position <- matrix(c(2, 1), ncol = 1)
			plot.heights <- c(ttl.h, 0.9)
			par.plot <- c(3.5, par.mar.2, 1.5, 2.1)
			par.title <- c(0, par.mar.2, 1, 2.1)
		}
	}else{
		plot.position <- matrix(1:2, ncol = 1)
		plot.heights <- c(0.9, 0.01)
		par.plot <- c(3.5, par.mar.2, 2.1, 2.1)
		par.title <- c(0, par.mar.2, 0, 2.1)
	}

	layout(plot.position, widths = 1, heights = plot.heights, respect = FALSE)

	op <- par(mar = par.plot)
	plot(x, y, type = 'n', xlab = '', ylab = '', axes = FALSE, xlim = xlim, ylim = ylim)

	if(is.null(yticks)){
		minTck <- axTicks(2)
		minTck <- minTck[-length(minTck)] + diff(minTck)/2
		minTck <-c(min(axTicks(2))-diff(minTck)[1]/2, minTck, max(axTicks(2))+diff(minTck)[1]/2)
		abline(h = axTicks(2), col = "lightgray", lty = "solid", lwd = 0.8)
		abline(h = minTck, col = "lightgray", lty = "dotted")
		yTck <- axTicks(2)
		ylas <- 1
	}else{
		abline(h = yticks, col = "lightgray", lty = "solid", lwd = 0.8)
		yTck <- yticks
		ylas <- 2
	}

	if(is(x, "Date")){
		xTck <- axTicks.Date(x, 1)
		axis.foo <- axis.Date
		bar.width <- round(58*(as.numeric(diff(range(xlim)))/min(as.numeric(diff(x)), na.rm = TRUE))^(-0.508775))
		if(as.numeric(diff(xlim)) > 1095){
			xminor <- seq(as.Date(paste0(format(xlim[1], "%Y"), "-01-01")),
						as.Date(paste0(as.numeric(format(xlim[2], "%Y"))+1, "-01-01")), "year")
			xminor <- xminor[!xminor%in%xTck]
		}else xminor <- NULL
	}else{
		xTck <- axTicks(1)
		xTck <- xTck[sapply(xTck, function(e) min(abs(c(e%%1, e%%1-1))) < 1e-10)]
		axis.foo <- axis
		bar.width <- round(60*as.numeric(diff(range(xlim)))^(-0.508775))
		if(as.numeric(diff(xlim)) > 5){
			xminor <- seq(floor(xlim[1]), floor(xlim[2]), 1)
			xminor <- xminor[!xminor%in%xTck]
		}else xminor <- NULL
	}
	abline(v = xTck, col = "lightgray", lty = "dotted")

	polys <- split.polygons.with_missing(as.numeric(x), rep(y0, length(y)), y)

	for(j in seq_along(polys)){
		P <- polys[[j]]
		polygon(P$x, P$y, col = fillcol[P$z], border = NA)
	}
	if(plot.line$plot) lines(x, y, lwd = plot.line$lwd, col = plot.line$col)
	abline(h = y0, col = "lightgray", lty = "solid", lwd = 0.8)

	axis.foo(1, at = xTck, font = axis.font)
	if(length(xminor) > 0) axis.foo(1, at = xminor, labels = NA, tcl = par("tcl")*0.5)
	axis(2, at = yTck, las = ylas, font = axis.font)

	mtext(xlab, side = 1, line = 2.1)
	line <- if(max(nchar(as.character(axTicks(2)))) > 2) 3.8 else 2.5
	if(!is.null(ylab.sub)){
		mtext(ylab, side = 2, line = line+1)
		mtext(ylab.sub, side = 2, line = line, font = 3, cex = 0.8)
	}else mtext(ylab, side = 2, line = line)

	box(bty = 'l')
	box(bty = '7', col = 'black')
	if(!is.null(location)) mtext(location, side = 3, outer = FALSE, adj = 1, line = 0, cex = 0.6) 
	par(op)

	op <- par(mar = par.title)
	if(draw.title){
		plot(1, type = 'n', xaxt = 'n', yaxt = 'n', xlab = '', ylab = '')
		bbx <- par("usr")
		rect(bbx[1], bbx[3], bbx[2], bbx[4], col = "ghostwhite")
		text(1, 1, title, cex = 0.9, font = 2)
	}else plot.new()
	par(op)
}

####################################################################################################

picsa.plot.daily <- function(dates, prec, location, thres.rain = 1, axis.font = 1){
	vtimes <- table.annuel()
	vmmdd <- paste0(str_pad(vtimes[, 2], 2, pad = '0'), str_pad(vtimes[, 1], 2, pad = '0'))
	years <- as.numeric(substr(dates, 1, 4))
	mmdd <- substr(dates, 5, 8)
	mmdd[mmdd == '0229'] <- '0228'
	yday <- match(mmdd, vmmdd)
	dfplot <- data.frame(yy = years, day = yday)
	xlim <- range(dfplot$yy, na.rm = TRUE)
	rnor <- prec > thres.rain

	layout(matrix(1:2, ncol = 1), widths = 1, heights = c(0.9, 0.1), respect = FALSE)
	op <- par(mar = c(3.1, 5.1, 2.1, 2.1))
	plot(dfplot$yy, dfplot$day, type = 'n', xaxt = 'n', yaxt = 'n', xlab = '', ylab = '', ylim = c(0, 380))

	xTck <- axTicks(1)
	yTck <- c(1, 91, 182, 274, 365)-1
	# yTck <- c(0, 100, 200, 300, 360)

	if(as.numeric(diff(xlim)) > 5){
		xminor <- seq(floor(xlim[1]), floor(xlim[2]), 1)
		xminor <- xminor[!xminor%in%xTck]
	}else xminor <- NULL
	yminor <- c(32, 60, 121, 152, 213, 244, 305, 335)-1
	# yminor <- seq(0, 370, 10)

	axis(1, at = xTck, font = axis.font)
	if(length(xminor) > 0) axis(1, at = xminor, labels = NA, tcl = par("tcl")*0.5)
	mtext('Year', side = 1, line = 2)

	yaxlab <- format(as.Date(yTck, origin = "2017-1-1"), '%d-%b')
	axis(2, at = yTck, labels = yaxlab, las = 2, font = axis.font, cex.axis = 1)
	axis(2, at = yminor, labels = NA, tcl = par("tcl")*0.5)
	# axis(2, at = yTck, font = axis.font, las = 2)
	# axis(2, at = yminor, labels = NA, tcl = par("tcl")*0.5)
	# mtext('Day of Year', side = 2, line = 3.5)

	abline(h = yTck, col = "lightgray", lty = "dotted")
	abline(v = xTck, col = "lightgray", lty = "dotted")

	points(dfplot$yy[!rnor], dfplot$day[!rnor], pch = 15, col = "khaki", cex = 0.4)
	points(dfplot$yy[rnor], dfplot$day[rnor], pch = 20, col = "blue", cex = 0.3)

	mtext(location, side = 3, outer = FALSE, adj = 1, line = 0, cex = 0.6) 

	legend(x = 'topright', legend = c("Rain", "Dry", 'NA'), bty = "n",
			fill = c("blue", "khaki", NA), horiz = TRUE, cex = 0.8, inset = -0.01)
	par(op)

	op <- par(mar = c(1, 5.1, 0, 2.1))
	plot(1, type = 'n', xaxt = 'n', yaxt = 'n', xlab = '', ylab = '')
	bbx <- par("usr")
	rect(bbx[1], bbx[3], bbx[2], bbx[4], col = "ghostwhite")
	text(1, 1, "Rain Present", cex = 0.9, font = 2)
	par(op)
}

picsa.plot.TxTn <- function(x, tmax, tmin, location, axis.font = 1){
	ylim <- range(c(pretty(tmin), pretty(tmax)))

	layout(matrix(1:2, ncol = 1), widths = 1, heights = c(0.9, 0.1), respect = FALSE)
	op <- par(mar = c(3, 4, 2, 2))
	plot(x, tmin, type = 'n', xaxt = 'n', yaxt = 'n', xlab = '', ylab = 'Temperature (°C)', ylim = ylim)

	abline(h = axTicks(2), col = "lightgray", lty = "dotted")
	abline(v = axTicks(1), col = "lightgray", lty = "dotted")

	mtext('Year', side = 1, line = 2)
	axis(1, at = axTicks(1), font = axis.font)
	axis(2, at = axTicks(2), las = 1, font = axis.font)

	lines(x, tmin, col = 'blue', lwd = 2)
	lines(x, tmax, col = 'red', lwd = 2)

	abline(lm(tmax~x), lwd = 2)
	abline(lm(tmin~x), lwd = 2)
	mtext(location, side = 3, outer = FALSE, adj = 1, line = 0, cex = 0.6) 
	par(op)

	op <- par(mar = c(0, 4, 0, 2))
	plot.new()
	legend("top", "groups", legend = c('Tmax', 'Tmin', 'Trend line'), col = c('red', 'blue', 'black'), lwd = 3, lty = 1, horiz = TRUE)
	par(op)
}
