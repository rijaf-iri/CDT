
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

#############################################

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

## Rolling function
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

