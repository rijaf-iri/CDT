
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
								opr.thres = 0)
							)
{
	if(pars$aggr.fun == "max") res <- matrixStats::colMaxs(MAT, na.rm = TRUE)
	if(pars$aggr.fun == "min") res <- matrixStats::colMins(MAT, na.rm = TRUE)
	if(pars$aggr.fun == "median") res <- matrixStats::colMedians(MAT, na.rm = TRUE)
	if(pars$aggr.fun == "mean") res <- colMeans(MAT, na.rm = TRUE)
	if(pars$aggr.fun == "sum") res <- colSums(MAT, na.rm = TRUE)
	if(pars$aggr.fun == "count"){
		count.fun <- get(pars$opr.fun, mode = "function")
		MAT <- count.fun(MAT, pars$opr.thres) & !is.na(MAT)
		res <- colSums(MAT, na.rm = TRUE)
	}
	return(res)
}

cdt.data.aggregate <- function(MAT, index,
								pars = list(min.frac = 0.95,
											aggr.fun = "sum",
											opr.fun = ">",
											opr.thres = 0)
								)
{
	data <- lapply(index, function(ix){
		don <- MAT[ix, , drop = FALSE]
		miss <- (colSums(is.na(don))/nrow(don)) >= pars$min.frac

		out <- cdt.aggregate(don, pars = pars)
		out[miss] <- NA
		out[is.nan(out) | is.infinite(out)] <- NA
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
