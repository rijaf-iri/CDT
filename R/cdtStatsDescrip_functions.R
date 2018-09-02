
# ### NA count
# funMissMAT <- function(x, DATA){
# 	MAT <- is.na(DATA[x, , drop = FALSE])
# 	colSums(MAT)
# }

# ### Aggregation
# funAggrMAT <- function(x, DATA, pars){
# 	x <- x[!is.na(x)]
# 	if(length(x) == 0) return(rep(NA, ncol(DATA)))
# 	MAT <- DATA[x, , drop = FALSE]
# 	if(pars$aggr.fun == "max") res <- colMaxs(MAT, na.rm = TRUE)
# 	if(pars$aggr.fun == "min") res <- colMins(MAT, na.rm = TRUE)
# 	if(pars$aggr.fun == "mean") res <- colMeans(MAT, na.rm = TRUE)
# 	if(pars$aggr.fun == "sum") res <- colSums(MAT, na.rm = TRUE)
# 	if(pars$aggr.fun == "count"){
# 		count.fun <- get(pars$count.fun, mode = "function")
# 		MAT <- count.fun(MAT, pars$count.thres) & !is.na(MAT)
# 		res <- colSums(MAT, na.rm = TRUE)
# 	}
# 	return(res)
# }

# funAggrVEC <- function(x, DATA, pars){
# 	x <- x[!is.na(x)]
# 	if(length(x) == 0) return(NA)
# 	VEC <- DATA[x]
# 	VEC <- VEC[!is.na(VEC)]
# 	if(length(VEC) == 0) return(NA)
# 	if(pars$aggr.fun == "max") res <- max(VEC)
# 	if(pars$aggr.fun == "min") res <- min(VEC)
# 	if(pars$aggr.fun == "mean") res <- mean(VEC)
# 	if(pars$aggr.fun == "sum") res <- sum(VEC)
# 	if(pars$aggr.fun == "count"){
# 		count.fun <- get(pars$count.fun, mode = "function")
# 		VEC <- count.fun(VEC, pars$count.thres)
# 		res <- sum(VEC)
# 	}
# 	return(res)
# }

#############################################

cdt.aggregate <- function(MAT,
							pars = list(
								aggr.fun = "sum",
								opr.fun = ">",
								opr.thres = 0,
								rle.fun = ">=",
								rle.thres = 6)
							)
{
	if(pars$aggr.fun == "max") res <- matrixStats::colMaxs(MAT, na.rm = TRUE)
	if(pars$aggr.fun == "min") res <- matrixStats::colMins(MAT, na.rm = TRUE)
	if(pars$aggr.fun == "median") res <- matrixStats::colMedians(MAT, na.rm = TRUE)
	if(pars$aggr.fun == "mean") res <- colMeans(MAT, na.rm = TRUE)
	if(pars$aggr.fun == "sum") res <- colSums(MAT, na.rm = TRUE)

	if(pars$aggr.fun %in% c("count", "count.rle.list", "count.rle.nb",
							"count.rle.max", "count.rle.min"))
	{
		count.fun <- get(pars$opr.fun, mode = "function")
		MAT <- count.fun(MAT, pars$opr.thres) & !is.na(MAT)

		if(pars$aggr.fun != "count")
		{
			res <- lapply(seq(ncol(MAT)), function(j){
				rr <- rle(MAT[, j])
				rr <- rr$lengths[rr$values]
				if(length(rr) == 0) rr <- 0
				rr
			})
			if(pars$aggr.fun == "count.rle.max")
				res <- sapply(res, max)
			if(pars$aggr.fun == "count.rle.min")
				res <- sapply(res, min)
			if(pars$aggr.fun == "count.rle.nb"){
				rle.fun <- get(pars$rle.fun, mode = "function")
				res <- sapply(res, function(x) sum(rle.fun(x, pars$rle.thres)))
			}
		}else res <- colSums(MAT, na.rm = TRUE)
	}

	if(pars$aggr.fun != "count.rle.list")
		res[is.nan(res) | is.infinite(res)] <- NA

	return(res)
}

cdt.data.aggregate <- function(MAT, index,
								pars = list(min.frac = 0.95,
											aggr.fun = "sum",
											opr.fun = ">",
											opr.thres = 0,
											rle.fun = ">=",
											rle.thres = 6)
								)
{
	data <- lapply(index, function(ix){
		don <- MAT[ix, , drop = FALSE]
		miss <- (colSums(!is.na(don))/nrow(don)) < pars$min.frac
		out <- rep(NA, ncol(don))
		if(all(miss)) return(out)
		out[!miss] <- cdt.aggregate(don[, !miss, drop = FALSE], pars = pars)
		out
	})
	do.call(rbind, data)
}

#############################################

cdt.data.analysis <- function(MAT, FUN,
						trend = list(year = NA, min.year = 10, unit = 1),
						percentile = 90,
						freq.thres = list(low = NA, up = NA)
						)
{
	nc <- ncol(MAT)
	nr <- nrow(MAT)
	nNA <- colSums(!is.na(MAT)) > 2
	MAT <- MAT[, nNA, drop = FALSE]

	out <- if(FUN == "trend") matrix(NA, nrow = 4, ncol = nc) else rep(NA, nc)
	if(ncol(MAT) == 0) retrun(out)

	if(FUN == "mean") res <- colMeans(MAT, na.rm = TRUE)
	if(FUN == "median") res <- matrixStats::colMedians(MAT, na.rm = TRUE)
	if(FUN == "std") res <- matrixStats::colSds(MAT, na.rm = TRUE)
	if(FUN == "cv") res <- 100 * matrixStats::colSds(MAT, na.rm = TRUE) / colMeans(MAT, na.rm = TRUE)
	if(FUN == "trend"){
		res <- regression.Vector(trend$year, MAT, trend$min.year)
		if(trend$unit == 2) res[1, ] <- as.numeric(res[1, ]) * (diff(range(trend$year, na.rm = TRUE)) + 1)
		if(trend$unit == 3) res[1, ] <- 100 * as.numeric(res[1, ]) * (diff(range(trend$year, na.rm = TRUE)) + 1) / colMeans(MAT, na.rm = TRUE)
		res <- round(res[c(1, 2, 4, 9), , drop = FALSE], 3)
	}
	if(FUN == "percentile"){
		probs <- percentile / 100
		res <- matrixStats::colQuantiles(MAT, probs = probs, na.rm = TRUE, type = 8)
	}
	if(FUN == "frequency"){
		MAT <- (MAT >= freq.thres$low) & (MAT <= freq.thres$up) & !is.na(MAT)
		res <- colSums(MAT, na.rm = TRUE)
	}
	res[is.nan(res) | is.infinite(res)] <- NA

	if(FUN == "trend"){
		out[, nNA] <- res
		dimnames(out)[[1]] <- dimnames(res)[[1]]
	}else out[nNA] <- res

	return(out)
}

#############################################

cdt.daily.statistics <- function(MAT, STATS = "tot.rain",
								pars = list(min.frac = 0.95,
											drywet.day = 1,
											drywet.spell = 7)
								)
{
	if(STATS == "tot.rain") res <- colSums(MAT, na.rm = TRUE)
	if(STATS == "rain.int") res <- colMeans(MAT, na.rm = TRUE)
	if(STATS == "nb.wet.day") res <- colSums(!is.na(MAT) & MAT >= pars$drywet.day)
	if(STATS == "nb.dry.day") res <- colSums(!is.na(MAT) & MAT < pars$drywet.day)
	if(STATS == "nb.wet.spell"){
		wetday <- !is.na(MAT) & MAT >= pars$drywet.day
		wspl <- lapply(seq(ncol(wetday)), function(j){
			x <- rle(wetday[, j])
			x <- x$lengths[x$values]
			if(length(x) > 0) length(which(x >= pars$drywet.spell)) else 0
		})
		res <- do.call(c, wspl)
	}
	if(STATS == "nb.dry.spell"){
		dryday <- !is.na(MAT) & MAT < pars$drywet.day
		dspl <- lapply(seq(ncol(dryday)), function(j){
			x <- rle(dryday[, j])
			x <- x$lengths[x$values]
			if(length(x) > 0) length(which(x >= pars$drywet.spell)) else 0
		})
		res <- do.call(c, dspl)
	}
	ina <- colSums(!is.na(MAT))/nrow(MAT) < pars$min.frac
	res[ina] <- NA
	return(res)
}

#############################################

## Rolling function
.rollfun.vec <- function(x, win, fun, na.rm, min.data, na.pad, fill, align)
{
	conv <- if(fun == "convolve") TRUE else FALSE
	fun <- match.fun(fun)
	nl <- length(x)
	xna <- xx <- rep(NA, nl - win + 1)
	for(k in seq(nl - win + 1)){
		if(conv){
			vx <- x[seq(k, k + win - 1, 1)]
			vx <- vx[!is.na(vx)]
			ix <- length(vx)
			xx[k] <- if(ix > 1) convolve(vx, rep(1/ix, ix), type = "filter") else NA
		}else xx[k] <- fun(x[seq(k, k + win - 1, 1)], na.rm = na.rm)
		xna[k] <- sum(!is.na(x[seq(k, k + win - 1, 1)]))
	}
	xx[is.nan(xx) | is.infinite(xx)] <- NA
	xx[xna < min.data] <- NA

	if(na.pad){
		if(align == "right"){
			xx <-
				if(fill)
					c(rep(xx[1], win - 1), xx)
				else
					c(rep(NA, win - 1), xx)
		}
		if(align == "left"){
			xx <-
				if(fill)
					c(xx, rep(xx[length(xx)], win - 1))
				else
					c(xx, rep(NA, win - 1))
		}
		if(align == "center"){
			before <- floor((win - 1) / 2)
			after <- ceiling((win - 1) / 2)
			xx <- 
				if(fill) 
					c(rep(xx[1], before), xx, rep(xx[length(xx)], after))
				else
					c(rep(NA, before), xx, rep(NA, after))
		}
	}
	return(xx)
}

.rollfun.mat <- function(x, win, fun, na.rm, min.data, na.pad, fill, align)
{
	nl <- nrow(x)
	nc <- ncol(x)
	xna <- xx <- matrix(NA, nrow = nl - win + 1, ncol = nc)

	if(fun == "sum") foo <- colSums
	if(fun == "mean") foo <- colMeans
	if(fun == "median") foo <- matrixStats::colMedians
	if(fun == "max") foo <- matrixStats::colMaxs
	if(fun == "min") foo <- matrixStats::colMins
	if(fun == "sd") foo <- matrixStats::colSds
	for(k in seq(nl - win + 1)){
		xx[k, ] <- foo(x[seq(k, k + win - 1, 1), , drop = FALSE], na.rm = na.rm)
		xna[k, ] <- colSums(!is.na(x[seq(k, k + win - 1, 1), , drop = FALSE]))
	}
	xx[is.nan(xx) | is.infinite(xx)] <- NA
	xx[xna < min.data] <- NA

	if(na.pad){
		if(align == "right"){
			xx <- 
				if(fill)
					rbind(matrix(xx[1, ], win - 1, nc, byrow = TRUE), xx)
				else
					rbind(matrix(NA, win - 1, nc), xx)
		}
		if(align == "left"){
			xx <-
				if(fill)
					rbind(xx, matrix(xx[nrow(xx), ], win - 1, nc, byrow = TRUE))
				else
					rbind(xx, matrix(NA, win - 1, nc))
		}
		if(align == "center"){
			before <- floor((win - 1) / 2)
			after <- ceiling((win - 1) / 2)
			xx <-
				if(fill)
					rbind(matrix(xx[1, ], before, nc, byrow = TRUE), xx, matrix(xx[nrow(xx), ], after, nc, byrow = TRUE))
				else
					rbind(matrix(NA, before, nc), xx, matrix(NA, after, nc))
		}
	}
	return(xx)
}

## to export
cdt.roll.fun <- function(x, win, fun = "sum", na.rm = FALSE,
						min.data = win, na.pad = TRUE, fill = FALSE,
						align = c("center", "left", "right")
					)
{
	# vector, fun: sum, mean, median, sd, max, min, convolve
	# matrix, fun: sum, mean, median, sd, max, min
	if(is.matrix(x)) foo <- .rollfun.mat
	else if(is.vector(x)) foo <- .rollfun.vec
	else return(NULL)

	align <- align[1]
	if(min.data > win) min.data <- win

	foo(x, win, fun, na.rm, min.data, na.pad, fill, align)
}

#############################################

## Climatology mean & sd
.cdt.Climatologies <- function(index.clim, data.mat, min.year, tstep, daily.win)
{
	div <- if(tstep == "daily") 2 * daily.win + 1 else 1
	tstep.miss <- (sapply(index.clim$index, length) / div) < min.year
	tmp <- rep(NA, ncol(data.mat))

	dat.clim <- lapply(seq_along(index.clim$id), function(jj){
		if(tstep.miss[jj]) return(list(moy = tmp, sds = tmp))
		xx <- data.mat[index.clim$index[[jj]], , drop = FALSE]
		ina <- (colSums(!is.na(xx)) / div) < min.year
		if(all(ina)) return(list(moy = tmp, sds = tmp))
		moy <- tmp
		moy[!ina] <- colMeans(xx[, !ina, drop = FALSE], na.rm = TRUE)
		sds <- tmp
		sds[!ina] <- matrixStats::colSds(xx[, !ina, drop = FALSE], na.rm = TRUE)
		moy[is.nan(moy)] <- NA
		list(moy = moy, sds = sds)
	})

	dat.moy <- do.call(rbind, lapply(dat.clim, "[[", "moy"))
	dat.sds <- do.call(rbind, lapply(dat.clim, "[[", "sds"))

	return(list(mean = dat.moy, sd = dat.sds))
}

## Climatology percentiles
.cdt.quantile.Climatologies <- function(index.clim, data.mat, probs, min.year,
										tstep, daily.win, type = 8)
{
	div <- if(tstep == "daily") 2 * daily.win + 1 else 1
	tstep.miss <- (sapply(index.clim$index, length) / div) < min.year
	tmp <- matrix(NA, ncol(data.mat), length(probs))

	dat.clim <- lapply(seq_along(index.clim$id), function(jj){
		if(tstep.miss[jj]) return(tmp)
		xx <- data.mat[index.clim$index[[jj]], , drop = FALSE]
		ina <- (colSums(!is.na(xx)) / div) < min.year
		if(all(ina)) return(tmp)
		xquant <- matrixStats::colQuantiles(xx[, !ina, drop = FALSE], probs = probs, na.rm = TRUE, type = type)
		if(length(probs) == 1) xquant <- matrix(xquant, ncol = 1)
		out <- tmp
		out[!ina, ] <- xquant
		out
	})

	quant <- lapply(seq_along(probs), function(j) do.call(rbind, lapply(dat.clim, "[", , j)))
	names(quant) <- paste0(probs * 100, "%")

	return(quant)
}

## to export
cdt.Climatologies <- function(data.mat, dates,
								tstep = "dekadal",
								pars.clim = list(
										all.years = TRUE,
										start.year = 1981,
										end.year = 2010,
										min.year = 15,
										daily.win = 0)
								)
{
	year <- as.numeric(substr(dates, 1, 4))
	if(length(unique(year)) < pars.clim$min.year)
		stop("No enough data to compute climatology")

	iyear <- rep(TRUE, length(year))
	if(!pars.clim$all.years)
		iyear <- year >= pars.clim$start.year & year <= pars.clim$end.year
	dates <- dates[iyear]
	data.mat <- data.mat[iyear, , drop = FALSE]

	index <- cdt.index.Climatologies(dates, tstep, pars.clim$daily.win)
	dat.clim <- .cdt.Climatologies(index, data.mat, pars.clim$min.year, tstep, pars.clim$daily.win)

	return(list(id = index$id, mean = dat.clim$mean, sd = dat.clim$sd))
}

#############################################

## Anomaly
.cdt.Anomalies <- function(index.anom, data.mat, data.mean, data.sds, FUN)
{
	data.mat <- data.mat[index.anom[, 2], , drop = FALSE]
	data.mean <- data.mean[index.anom[, 3], , drop = FALSE]
	data.sds <- data.sds[index.anom[, 3], , drop = FALSE]
	anom <- switch(FUN,
				"Difference" = data.mat - data.mean,
				"Percentage" = 100 * (data.mat - data.mean) / (data.mean + 0.001),
				"Standardized" = (data.mat - data.mean) / data.sds
			)
	return(anom)
}

## to export
cdt.Anomalies <- function(data.mat, dates,
							tstep = "dekadal",
							date.range = NULL,
							FUN = c("Difference", "Percentage", "Standardized"),
							climatology = FALSE,
							data.clim = list(mean = NULL, sd = NULL),
							pars.clim = list(
									all.years = TRUE,
									start.year = 1981,
									end.year = 2010,
									min.year = 15,
									daily.win = 0)
						)
{
	# date.range = c(start = 2018011, end = 2018063)	
	FUN <- FUN[1]
	index.clim <- NULL
	if(climatology){
		if(is.null(data.clim$mean)) stop("Climatology mean does not find.")
		if(!is.matrix(data.clim$mean)) stop("Climatology mean must be a matrix.")
		if(FUN == "Standardized"){
			if(is.null(data.clim$sd)) stop("Climatology SD does not find.")
			if(!is.matrix(data.clim$sd)) stop("Climatology SD must be a matrix.")
		}
		id <- switch(tstep, "daily" = 365, "pentad" = 72, "dekadal" = 36, "monthly" = 12)
		data.clim$id <- seq(id)
		index.clim$id <- seq(id)
	}else{
		data.clim <- cdt.Climatologies(data.mat, dates, tstep, pars.clim)
		index.clim$id <- data.clim$id
	}

	if(!is.null(date.range)){
		if(tstep == "monthly"){
			daty0 <- as.Date(paste0(dates, 15), "%Y%m%d")
			start.daty <- as.Date(paste0(date.range$start, 15), "%Y%m%d")
			end.daty <- as.Date(paste0(date.range$end, 15), "%Y%m%d")
		}else{
			daty0 <- as.Date(dates, "%Y%m%d")
			start.daty <- as.Date(as.character(date.range$start), "%Y%m%d")
			end.daty <- as.Date(as.character(date.range$end), "%Y%m%d")
		}
		iyear <- daty0 >= start.daty & daty0 <= end.daty
		dates <- dates[iyear]
		if(length(dates) == 0) stop("No data to compute anomaly")
		data.mat <- data.mat[iyear, , drop = FALSE]
	}

	index <- cdt.index.Anomalies(dates, index.clim, tstep)
	index.anom <- index$index
	date.anom <- index$date

	data.sds <- if(FUN == "Standardized") data.clim$sd else NULL
	anom <- .cdt.Anomalies(index.anom, data.mat, data.clim$mean, data.sds, FUN)

	return(list(data.anom = list(date = date.anom, anomaly = anom), data.clim = data.clim))
}

#############################################

ValidationStatisticsFun <- function(x, y, dichotomous){
	nbcol <- ncol(x)
	nbrow <- nrow(x)
	naCol <- colSums(is.na(x))
	colNA <- naCol == nbrow
	nbX <- nbrow - naCol
	xmy <- y - x
	mnx <- colMeans(x, na.rm = TRUE)
	mny <- colMeans(y, na.rm = TRUE)
	varx <- matrixStats::colVars(x, na.rm = TRUE)
	vary <- matrixStats::colVars(y, na.rm = TRUE)
	X1 <- x - matrix(mnx, nbrow, nbcol, byrow = TRUE)
	Y1 <- y - matrix(mny, nbrow, nbcol, byrow = TRUE)

	sum1 <- colSums(xmy^2, na.rm = TRUE)
	sum1[colNA] <- NA
	sum2 <- colSums(X1^2, na.rm = TRUE)
	sum2[colNA] <- NA

	NSE <- 1 - sum1/sum2
	MAE <- colMeans(abs(xmy), na.rm = TRUE)
	ME <- colMeans(xmy, na.rm = TRUE)
	BIAS <- colSums(y, na.rm = TRUE) / colSums(x, na.rm = TRUE)
	RMSE <- sqrt(colMeans(xmy^2, na.rm = TRUE))

	COV <- colSums(X1 * Y1, na.rm = TRUE) / (nbX - 1)
	COV[colNA] <- NA
	CORR <- sqrt(COV^2 / (varx * vary))

	##################
	# Dichotomous
	thres <- dichotomous$opr.thres
	fun <- get(dichotomous$opr.fun, mode = "function")

	YesOBS <- fun(x, thres)
	YesFCST <- fun(y, thres)

	##################

	# hit
	Hit <- YesOBS & YesFCST
	# false alarm
	False <- !YesOBS & YesFCST
	# miss
	Miss <- YesOBS & !YesFCST
	# correct negative
	TrueNull <- !YesOBS & !YesFCST

	##################

	N1 <- colSums(Hit, na.rm = TRUE)
	N1[colNA] <- NA

	N2 <- colSums(False, na.rm = TRUE)
	N2[colNA] <- NA

	N3 <- colSums(Miss, na.rm = TRUE)
	N3[colNA] <- NA

	N4 <- colSums(TrueNull, na.rm = TRUE)
	N4[colNA] <- NA

	##################

	N <- N1 + N2 + N3 + N4

	FBS <- (N1 + N2) / (N1 + N3)
	CSI <- N1 / (N - N4)
	POD <- N1 / (N1 + N3)
	FAR <- N2 / (N1 + N2)

	C0 <- N1 + N4
	E0 <- ((N1 + N3) * (N1 + N2) + (N2 + N4) * (N3 + N4)) / N
	HSS <- (C0 - E0) / (N - E0)

	##################

	Fct.hit <- y
	Fct.hit[!Hit] <- NA
	Fct.hit.v <- colSums(Fct.hit, na.rm = TRUE)
	Fct.hit.i <- colSums(!is.na(Fct.hit), na.rm = TRUE)
	Fct.hit.v[colNA] <- NA
	Fct.hit.i[colNA] <- NA

	Fct.false <- y
	Fct.false[!False] <- NA
	Fct.false.v <- colSums(Fct.false, na.rm = TRUE)
	Fct.false.i <- colSums(!is.na(Fct.false), na.rm = TRUE)
	Fct.false.v[colNA] <- NA
	Fct.false.i[colNA] <- NA

	Obs.miss <- x
	Obs.miss[!Miss] <- NA
	Obs.miss.v <- colSums(Obs.miss, na.rm = TRUE)
	Obs.miss.i <- colSums(!is.na(Obs.miss), na.rm = TRUE)
	Obs.miss.v[colNA] <- NA
	Obs.miss.i[colNA] <- NA

	##################

	# Volumetric Hit Index
	VHI <- Fct.hit.v / (Fct.hit.v + Obs.miss.v)

	# Quantile Probability of Detection
	QPOD <- Fct.hit.i / (Fct.hit.i +Obs.miss.i)

	# Volumetric False Alarm Ratio
	VFAR <- Fct.false.v / (Fct.hit.v + Fct.false.v)

	# Quantile False Alarm Ratio
	QFAR <- Fct.false.i / (Fct.hit.i + Fct.false.i)

	# Volumetric Miss Index
	VMI <- Obs.miss.v / (Fct.hit.v + Obs.miss.v)

	# Volumetric Critical Success Index 
	VCSI <- Fct.hit.v / (Fct.hit.v + Obs.miss.v + Fct.false.v)

	# Quantile Critical Success Index
	QCSI <- Fct.hit.i / (Fct.hit.i + Obs.miss.i + Fct.false.i)

	##################

	STATS <- rbind(CORR, NSE, BIAS, MAE, ME, RMSE, POD, FAR, FBS, CSI, HSS,
					VHI, QPOD, VFAR, QFAR, VMI, VCSI, QCSI)
	descrip <- c('Correlation', 'Nash-Sutcliffe Efficiency', 'Bias', 'Mean Absolute Error',
				'Mean Error', 'Root Mean Square Error', 'Probability Of Detection', 'False Alarm Ratio',
				'Frequency Bias', 'Critical Success Index', 'Heidke Skill Score',
				'Volumetric Hit Index', 'Quantile Probability of Detection', 'Volumetric False Alarm Ratio',
				'Quantile False Alarm Ratio', 'Volumetric Miss Index', 'Volumetric Critical Success Index',
				'Quantile Critical Success Index')

	STATS[is.nan(STATS)] <- NA
	STATS[is.infinite(STATS)] <- NA
	STATS <- round(STATS, 3)
	return(list(statistics = STATS, description = descrip))
}



