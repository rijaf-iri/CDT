
homog.AggregateSeries <- function(don, intstep, aggr.pars)
{
	dek.index <- mon.index <- NULL
	dek.dat <- mon.dat <- NULL
	if(intstep == "daily"){
		dek.index <- cdt.index.aggregate(don$dates, "daily", "dekadal")
		dek.dat <- cdt.data.aggregate(don$data, dek.index$index, pars = aggr.pars)
		dimnames(dek.dat) <- NULL
		mon.index <- cdt.index.aggregate(don$dates, "daily", "monthly")
		mon.dat <- cdt.data.aggregate(don$data, mon.index$index, pars = aggr.pars)
		dimnames(mon.dat) <- NULL

		aggrS <- list(don1 = list(tstep = "daily", date = don$dates, data = don$data),
					don2 = list(tstep = "dekadal", date = dek.index$date, data = dek.dat),
					don3 = list(tstep = "monthly", date = mon.index$date, data = mon.dat))
	}

	if(intstep == "pentad"){
		dek.index <- cdt.index.aggregate(don$dates, "pentad", "dekadal")
		dek.dat <- cdt.data.aggregate(don$data, dek.index$index, pars = aggr.pars)
		dimnames(dek.dat) <- NULL
		mon.index <- cdt.index.aggregate(don$dates, "pentad", "monthly")
		mon.dat <- cdt.data.aggregate(don$data, mon.index$index, pars = aggr.pars)
		dimnames(mon.dat) <- NULL

		aggrS <- list(don1 = list(tstep = "pentad", date = don$dates, data = don$data),
					don2 = list(tstep = "dekadal", date = dek.index$date, data = dek.dat),
					don3 = list(tstep = "monthly", date = mon.index$date, data = mon.dat))
	}

	if(intstep == "dekadal"){
		mon.index <- cdt.index.aggregate(don$dates, "dekadal", "monthly")
		mon.dat <- cdt.data.aggregate(don$data, mon.index$index, pars = aggr.pars)
		dimnames(mon.dat) <- NULL

		aggrS <- list(don1 = NULL,
					don2 = list(tstep = "dekadal", date = don$dates, data = don$data),
					don3 = list(tstep = "monthly", date = mon.index$date, data = mon.dat))
	}

	if(intstep == "monthly"){
		aggrS <- list(don1 = NULL, don2 = NULL,
					don3 = list(tstep = "monthly", date = don$dates, data = don$data))
	}
	rm(dek.index, dek.dat, mon.index, mon.dat)

	return(aggrS)
}

homog.DiffRatioUser.Simple <- function(x, opr = 1){
	if(is.null(x)) return(x)
	moy <- colMeans(x$data, na.rm = TRUE)
	moy[moy == 0] <- moy[moy == 0] + 0.1
	FUN <- if(opr == 1) "-" else "/"
	sweep(x$data, 2, moy, FUN = FUN)
}

homog.DiffRatioUser.Climato <- function(x, opr = 1){
	if(is.null(x)) return(x)
	index.clim <- cdt.index.Climatologies(x$date, x$tstep, 5)
	index.anom <- cdt.index.Anomalies(x$date, index.clim, x$tstep)
	moy <- lapply(index.clim$index, function(j) colMeans(x$data[j, , drop = FALSE], na.rm = TRUE))
	moy <- do.call(rbind, moy)
	moy[moy == 0] <- moy[moy == 0] + 0.1
	data.mat <- x$data[index.anom$index[, 2], , drop = FALSE]
	moy <- moy[index.anom$index[, 3], , drop = FALSE]
	FUN <- if(opr == 1) `-` else `/`
	ret <- FUN(data.mat, moy)
	dimnames(ret) <- NULL
	return(ret)
}

homog.RefSeriesUser <- function(candS, refrS, parSeries)
{
	if(parSeries$use.climato)
		FUN <- homog.DiffRatioUser.Climato
	else 
		FUN <- homog.DiffRatioUser.Simple
	xcand <- lapply(candS, FUN, opr = parSeries$diff.ratio)
	xrefs <- lapply(refrS, FUN, opr = parSeries$diff.ratio)
	ret <- lapply(seq_along(candS), function(j){
		out <- candS[[j]]
		if(is.null(out)) return(NULL)
		opr.fun <- if(parSeries$diff.ratio == 1) `-` else `/`
		res <- opr.fun(xcand[[j]], xrefs[[j]])
		res[is.nan(res)] <- 0
		res[is.infinite(res)] <- NA
		out$data <- res
		out
	})
	names(ret) <- names(candS)
	return(ret)
}

homog.TestSeries.noRef <- function(x, pars, id = NULL)
{
	if(is.null(x)) return(x)
	if(is.null(id)) id <- seq(ncol(x$data))
	X <- x$data[, id, drop = FALSE]
	if(pars$use.climato){
		anom <- cdt.Anomalies(X, x$date, x$tstep,
							FUN = "Standardized",
							pars.clim = list(all.years = TRUE,
								min.year = 5, daily.win = 5))
		
		x$data[, id] <- anom$data.anom$anomaly
	}else{
		moy <- colMeans(X, na.rm = TRUE)
		x$data[, id] <- X - moy
	}
	return(x)
}

###########

homog.DiffRatioRef.Simple <- function(x, opr = 1){
	moy <- colMeans(x, na.rm = TRUE)
	moy[moy == 0] <- moy[moy == 0] + 0.1
	FUN <- if(opr == 1) "-" else "/"
	sweep(x, 2, moy, FUN = FUN)
}

homog.DiffRatioRef.Climato <- function(x, index.clim, index.anom, opr = 1){
	moy <- lapply(index.clim$index, function(j) colMeans(x[j, , drop = FALSE], na.rm = TRUE))
	moy <- do.call(rbind, moy)
	moy[moy == 0] <- moy[moy == 0] + 0.1
	data.mat <- x[index.anom$index[, 2], , drop = FALSE]
	moy <- moy[index.anom$index[, 3], , drop = FALSE]
	FUN <- if(opr == 1) `-` else `/`
	ret <- FUN(data.mat, moy)
	dimnames(ret) <- NULL
	return(ret)
}

homog.TestSeries.withRef <- function(x, pars, voisin, id = NULL){
	if(is.null(x)) return(x)
	if(is.null(id)) id <- seq(ncol(x$data))

	if(pars$use.climato){
		index.clim <- cdt.index.Climatologies(x$date, x$tstep, 5)
		index.anom <- cdt.index.Anomalies(x$date, index.clim, x$tstep)
		XdiffRatio <- homog.DiffRatioRef.Climato(x$data, index.clim, index.anom, pars$diff.ratio)
	}else{
		XdiffRatio <- homog.DiffRatioRef.Simple(x$data, pars$diff.ratio)
	}

	ret <- lapply(voisin[id], function(v){
		Y <- XdiffRatio[, v$stn, drop = FALSE]
		x0 <- Y[, 1]
		Y <- Y[, -1, drop = FALSE]
		ina <- rowSums(!is.na(Y)) < pars$voisin$min

		weight <- matrix(v$lambda0, nrow(Y), ncol(Y), byrow = TRUE)
		weight[is.na(Y)] <- NA
		Y <- rowSums(weight * Y, na.rm = TRUE) / rowSums(weight, na.rm = TRUE)
		Y[ina] <- NA
		if(pars$diff.ratio == 1) out <- x0 - Y
		if(pars$diff.ratio == 2) out <- x0 / Y
		if(pars$diff.ratio == 3) out <- log(x0 / Y)
		out[is.nan(out) | is.infinite(out)] <- NA
		return(out)
	})

	ret <- do.call(cbind, ret)
	x$data[, id] <- ret
	return(x)
}

###########

homog.AdjustSeries <- function(Xl, cpt.table, parsStat, parsAdj){
	parsAdj <- matrix(unlist(parsAdj), 3, 2)
	parsAdj <- parsAdj[3:1, , drop = FALSE]

	ret <- lapply(seq_along(Xl), function(j){
		don <- Xl[[j]]
		if(is.null(don)) return(NULL)
		cpt <- cpt.table[[j]]
		if(is.null(cpt)){
			don$data <- matrix(don$data, nrow = length(don$data), ncol = 3)
			return(don)
		}

		cpt.dates <- as.character(cpt$Breakpoints.Date)
		cpt.dates <- cpt.dates[!is.na(cpt.dates) & cpt.dates != ""]
		if(length(cpt.dates) == 0){
			don$data <- matrix(don$data, nrow = length(don$data), ncol = 3)
			return(don)
		}

		cpt.idx <- which(don$date %in% cpt.dates)
		min.adj <- parsAdj[j, 1]
		minf <- switch(don$tstep, "daily" = 30, "pentad" = 6, "dekadal" = 3, "monthly" = 1)
		min.adj <- min.adj * minf
		SegAdj <- parsAdj[j, 2]

		if(parsStat$mthd == "CUSUMtr"){
			adj.func.mean <- AdjustT.byMean
			adj.func.qm <- AdjustT.byQM
		}else{
			adj.func.mean <- AdjustM.byMean
			adj.func.qm <- AdjustM.byQM
		}

		ret.mean <- adj.func.mean(don$data, cpt.idx, min.adj, SegAdj)
		ret.qm <- adj.func.qm(don$data, cpt.idx, min.adj, SegAdj)

		if(!is.null(ret.mean$msg))
			Insert.Messages.Out(ret.mean$msg, , format = TRUE)

		don$data <- cbind(don$data, ret.mean$res, ret.qm$res)
		return(don)
	})
	names(ret) <- names(Xl)

	return(ret)
}

