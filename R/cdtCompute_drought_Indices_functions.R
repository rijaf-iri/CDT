
SPEI_function <- function(data.mat, tscale = 1, frequency = 12, distribution = 'Gamma')
{
	min.non.na <- 5
	nl <- nrow(data.mat)
	don.tmp0 <- data.mat * NA
	icol <- which(colSums(!is.na(data.mat)) >= min.non.na)
	if(length(icol) == 0) return(don.tmp0)
	data.mat <- data.mat[, icol, drop = FALSE]

	don.tmp <- data.mat
	if(tscale > 1){
		don.tmp <- don.tmp0
		for(k in 1:(nl - tscale + 1))
			don.tmp[k + tscale - 1, ] <- colSums(data.mat[k:(k + tscale - 1), , drop = FALSE], na.rm = TRUE)
	}

	estime.pars.fun <- switch(distribution,
						"Gamma" = list(lmomco::pargam, lmomco::cdfgam),
						"Pearson Type III" = list(lmomco::parpe3, lmomco::cdfpe3),
						"log-Logistic" = list(lmomco::parglo, lmomco::cdfglo))

	estime.pars <- function(x){
		x <- x[!is.na(x)]
		n <- length(x)
		if(n < min.non.na) return(NULL)
		if(length(unique(x)) == 1)
			x <- x + runif(n, min = 0.1, max = 0.5)
		lmr <- lmomco::lmoms(x)
		if(!lmomco::are.lmom.valid(lmr)) return(NULL)
		estime.pars.fun[[1]](lmr)
	}

	spi <- lapply(1:frequency, function(k){
		iseq <- seq(k + tscale - 1, nl, frequency)
		don.mon <- don.tmp[iseq, , drop = FALSE]
		spi.out <- don.mon * NA

		no.na <- colSums(!is.na(don.mon))
		icol1 <- no.na >= min.non.na
		if(!any(icol1)) return(spi.out)
		don.mon <- don.mon[, icol1, drop = FALSE]

		if(distribution %in% c("Gamma", "Pearson Type III", "log-Logistic")){
			don.mon1 <- don.mon
			SPI <- don.mon * NA

			if(distribution %in% c("Gamma", "Pearson Type III")){
				pzero <- colSums(!is.na(don.mon) & don.mon == 0) / colSums(!is.na(don.mon))
				don.mon[don.mon <= 0] <- NA
			}

			PARS <- lapply(seq(ncol(don.mon)), function(j) estime.pars(don.mon[, j]))
			inull <- sapply(PARS, is.null)
			if(all(inull)) return(spi.out)

			PARS <- PARS[!inull]
			don.mon1 <- don.mon1[, !inull, drop = FALSE]

			spi <- lapply(seq(ncol(don.mon1)), function(j){
				# qnorm(estime.pars.fun[[2]](don.mon1[, j], PARS[[j]]))
				ina <- !is.na(don.mon1[, j])
				out <- don.mon1[, j] * NA
				if(length(which(ina)) < (min.non.na - 1)) return(out)
				out[ina] <- qnorm(estime.pars.fun[[2]](don.mon1[ina, j], PARS[[j]]))
				out[is.infinite(out) & sign(out) == -1] <- -5
				out[is.infinite(out) & sign(out) == 1] <- 5
				return(out)
			})
			spi <- do.call(cbind, spi)

			if(distribution %in% c("Gamma", "Pearson Type III")){
				pzero <- matrix(pzero[!inull], nrow(spi), ncol(spi), byrow = TRUE)
				SPI[, !inull] <- qnorm(pzero + (1 - pzero) * pnorm(spi))
			}else SPI[, !inull] <- spi
		}

		if(distribution == 'Z-Score'){
			don.sd <- matrixStats::colSds(don.mon, na.rm = TRUE)
			don.mean <- colMeans(don.mon, na.rm = TRUE)
			SPI <- sweep(sweep(don.mon, 2, don.mean, FUN = "-"), 2, don.sd, FUN = "/")
			SPI[is.nan(SPI) | is.infinite(SPI)] <- 0
		}

		spi.out[, icol1] <- SPI
		return(spi.out)
	})

	for(k in 1:frequency)
		don.tmp[seq(k + tscale - 1, nl, frequency), ] <- spi[[k]]
	don.tmp0[, icol] <- don.tmp
	rm(spi, don.tmp)
	return(don.tmp0)
}

#########################################################################

SPEI_Aggregate_data <- function(data.mat, tscale = 1)
{
	nl <- nrow(data.mat)
	if(tscale > 1){
		don.tmp <- data.mat * NA
		for(k in 1:(nl - tscale + 1))
			don.tmp[k + tscale - 1, ] <- colSums(data.mat[k:(k + tscale - 1), , drop = FALSE], na.rm = TRUE)
	}else don.tmp <- data.mat
	return(don.tmp)
}

#########################################################################

SPEI_Compute_params <- function(data.mat, tscale = 1, frequency = 12, distribution = 'Gamma')
{
	min.non.na <- 5
	nl <- nrow(data.mat)
	dist.params <- matrix(list(NA), nrow = frequency, ncol = ncol(data.mat))
	icol <- which(colSums(!is.na(data.mat)) >= min.non.na)
	if(length(icol) == 0) return(dist.params)
	data.mat <- data.mat[, icol, drop = FALSE]

	estime.pars.fun <- switch(distribution,
							"Gamma" = lmomco::pargam,
							"Pearson Type III" = lmomco::parpe3,
							"log-Logistic" = lmomco::parglo)

	estime.pars <- function(x){
		x <- x[!is.na(x)]
		n <- length(x)
		if(n < min.non.na) return(NULL)
		if(length(unique(x)) == 1)
			x <- x + runif(n, min = 0.1, max = 0.5)
		lmr <- lmomco::lmoms(x)
		if(!lmomco::are.lmom.valid(lmr)) return(NULL)
		estime.pars.fun(lmr)
	}

	params <- lapply(1:frequency, function(k){
		iseq <- seq(k + tscale - 1, nl, frequency)
		don.mon <- data.mat[iseq, , drop = FALSE]
		pars.out <- matrix(list(NA), nrow = 1, ncol = ncol(don.mon))

		no.na <- colSums(!is.na(don.mon))
		icol1 <- no.na >= min.non.na
		if(!any(icol1)) return(pars.out)
		don.mon <- don.mon[, icol1, drop = FALSE]

		if(distribution %in% c("Gamma", "Pearson Type III", "log-Logistic")){
			if(distribution %in% c("Gamma", "Pearson Type III")){
				pzero <- colSums(!is.na(don.mon) & don.mon == 0) / colSums(!is.na(don.mon))
				don.mon[don.mon <= 0] <- NA
			}

			PARS <- lapply(seq(ncol(don.mon)), function(j){
				prs <- estime.pars(don.mon[, j])
				if(is.null(prs)) return(list(NA))
				if(distribution == "log-Logistic") prs else c(prs, list(pzero = pzero[j]))
			})
		}

		if(distribution == 'Z-Score'){
			don.sd <- matrixStats::colSds(don.mon, na.rm = TRUE)
			don.mean <- colMeans(don.mon, na.rm = TRUE)
			PARS <- lapply(seq_along(don.mean), function(j) list(mean = don.mean[j], sd = don.sd[j]))
		}

		pars.out[, icol1] <- PARS
		return(pars.out)
	})

	dist.params[, icol] <- do.call(rbind, params)
	return(dist.params)
}

#########################################################################

SPEI_computation <- function(data.mat, params, tscale = 1, frequency = 12, distribution = 'Gamma')
{
	nl <- nrow(data.mat)
	nc <- ncol(data.mat)
	don.tmp0 <- data.mat * NA

	cdf.fun <- switch(distribution,
					"Gamma" = lmomco::cdfgam,
					"Pearson Type III" = lmomco::cdfpe3,
					"log-Logistic" = lmomco::cdfglo)

	spi.out <- lapply(1:frequency, function(k){
		iseq <- seq(k + tscale - 1, nl, frequency)
		don.tmp <- data.mat[iseq, , drop = FALSE]
		if(distribution %in% c("Gamma", "Pearson Type III", "log-Logistic")){
			spi <- lapply(seq(nc), function(j){
				pars <- params[k, j][[1]]
				ina <- !is.na(don.tmp[, j])
				out <- don.tmp[, j] * NA
				if(is.na(pars[[1]]) | length(which(ina)) < 4) return(out)
				out[ina] <- qnorm(cdf.fun(don.tmp[ina, j], pars))
				out[is.infinite(out) & sign(out) == -1] <- -5
				out[is.infinite(out) & sign(out) == 1] <- 5
				return(out)
			})
			spi <- do.call(cbind, spi)

			if(distribution %in% c("Gamma", "Pearson Type III")){
				pzero <- sapply(params[k, ], function(x){
					pz <- if(is.list(x)) x$pzero else NULL
					if(is.null(pz)) pz <- NA
					pz
				})
				pzero <- matrix(pzero, nrow(spi), ncol(spi), byrow = TRUE)
				spi <- qnorm(pzero + (1 - pzero) * pnorm(spi))
			}
		}

		if(distribution == 'Z-Score'){
			don.sd <- sapply(params[k, ], '[[', 'sd')
			don.mean <- sapply(params[k, ], '[[', 'mean')
			spi <- sweep(sweep(don.tmp, 2, don.mean, FUN = "-"), 2, don.sd, FUN = "/")
			spi[is.nan(spi) | is.infinite(spi)] <- 0
		}

		return(spi)
	})

	for(k in 1:frequency) don.tmp0[seq(k + tscale - 1, nl, frequency), ] <- spi.out[[k]]
	rm(spi.out)
	return(don.tmp0)
}

#########################################################################

Deciles_function <- function(data.mat, dates, base.period, tscale = 1, outfreq = "month")
{
	data.mat <- SPEI_Aggregate_data(data.mat, tscale)

	if(!base.period$all.years){
		year <- as.numeric(substr(dates, 1, 4))
		iyear <- year >= base.period$start.year & year <= base.period$end.year
		dates0 <- dates[iyear]
		data.mat0 <- data.mat[iyear, , drop = FALSE]
	}else{
		dates0 <- dates
		data.mat0 <- data.mat
	}

	col <- if(outfreq == "dekad") 7 else 6
	index <- split(seq_along(dates0), substr(dates0, 5, col))

	if(outfreq == "dekad"){
		dek <- expand.grid(1:3, stringr::str_pad(1:12, 2, pad = "0"))
		dek <- apply(dek[, 2:1], 1, paste0, collapse = '')
		index.pars <- match(names(index), dek)
		mdk <- substr(dates, 5, 7)
		index.dec <- match(mdk, dek)
	}else{
		index.pars <- as.numeric(names(index))
		index.dec <- as.numeric(substr(dates, 5, 6))
	}

	tstep.miss <- sapply(index, length) < base.period$min.year
	nc <- ncol(data.mat0)
	empty <- lapply(seq(nc), rep, x = NA, length.out = 11)

	decile.pars <- lapply(seq_along(index), function(jj){
		if(tstep.miss[jj]) return(empty)
		xx <- data.mat0[index[[jj]], , drop = FALSE]
		ina <- colSums(!is.na(xx)) < base.period$min.year
		decile <- matrixStats::colQuantiles(xx, probs = seq(0, 1, 0.1), na.rm = TRUE, type = 5)
		decile <- transPose(decile)
		decile[, ina] <- NA
		dimnames(decile) <- NULL
		lapply(seq(nc), function(i) decile[, i])
	})
	decile.pars <- do.call(rbind, decile.pars)

	data.decile <- data.mat * NA
	for(j in seq_along(index.pars)){
		ij <- index.dec == index.pars[j]
		if(!any(ij)) next
		don <- data.mat[ij, , drop = FALSE]
		decile <- do.call(cbind, decile.pars[j, ])
		icol <- colSums(is.na(decile)) == 0
		if(!any(icol)) next
		don <- don[, icol, drop = FALSE]

		tmp <- don
		for(i in seq(ncol(don)))
			tmp[, i] <- findInterval(don[, i], decile[, i], rightmost.closed = TRUE)
		data.decile[ij, icol] <- tmp
	}

	return(data.decile)
}

#########################################################################

Decile_Compute_params <- function(data.mat, dates, index.order, base.period, outfreq)
{
	if(!base.period$all.years){
		year <- as.numeric(substr(dates, 1, 4))
		iyear <- year >= base.period$start.year & year <= base.period$end.year
		dates <- dates[iyear]
		data.mat <- data.mat[iyear, , drop = FALSE]
	}

	col <- if(outfreq == "dekad") 7 else 6
	index <- split(seq_along(dates), substr(dates, 5, col))

	tstep.miss <- sapply(index, length) < base.period$min.year
	nc <- ncol(data.mat)
	empty <- lapply(seq(nc), rep, x = NA, length.out = 11)

	data.decile <- lapply(seq_along(index), function(jj){
		if(tstep.miss[jj]) return(empty)
		xx <- data.mat[index[[jj]], , drop = FALSE]
		ina <- colSums(!is.na(xx)) < base.period$min.year
		decile <- matrixStats::colQuantiles(xx, probs = seq(0, 1, 0.1), na.rm = TRUE, type = 5)
		decile <- transPose(decile)
		decile[, ina] <- NA
		dimnames(decile) <- NULL
		lapply(seq(nc), function(i) decile[, i])
	})

	data.decile <- do.call(rbind, data.decile)
	if(outfreq == "dekad"){
		dek <- expand.grid(1:3, stringr::str_pad(1:12, 2, pad = "0"))
		dek <- apply(dek[, 2:1], 1, paste0, collapse = '')
		index.out <- match(names(index), dek)
	}else index.out <- as.numeric(names(index))
	data.decile <- data.decile[match(index.order, index.out), , drop=FALSE]
	return(data.decile)
}

#########################################################################

Decile_computation <- function(data.mat, index, params)
{
	data.decile <- data.mat * NA
	for(j in seq_along(params$index)){
		ij <- index == params$index[j]
		if(!any(ij)) next
		don <- data.mat[ij, , drop = FALSE]
		decile <- do.call(cbind, params$decile[j, ])
		icol <- colSums(is.na(decile)) == 0
		if(!any(icol)) next
		don <- don[, icol, drop = FALSE]

		tmp <- don
		for(i in seq(ncol(don)))
			tmp[, i] <- findInterval(don[, i], decile[, i], rightmost.closed = TRUE)
		data.decile[ij, icol] <- tmp
	}

	return(data.decile)
}

Decile_cut <- function(x){
	x[x < 1] <- 0
	x[x == 1] <- 1
	x[x %in% 2:3] <- 2
	x[x %in% 4:7] <- 3
	x[x %in% 8:9] <- 4
	x[x == 10] <- 5
	x[x > 10] <- 6
	x + 1
}


