
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
    if(ncol(MAT) == 0) return(out)

    if(FUN == "mean")
        res <- colMeans(MAT, na.rm = TRUE)
    if(FUN == "median")
        res <- matrixStats::colMedians(MAT, na.rm = TRUE)
    if(FUN == "std")
        res <- matrixStats::colSds(MAT, na.rm = TRUE)
    if(FUN == "cv")
        res <- 100 * matrixStats::colSds(MAT, na.rm = TRUE) / colMeans(MAT, na.rm = TRUE)
    if(FUN == "trend"){
        res <- regression.Vector(trend$year, MAT, trend$min.year)
        if(trend$unit == 2)
            res[1, ] <- as.numeric(res[1, ]) * (diff(range(trend$year, na.rm = TRUE)) + 1)
        if(trend$unit == 3)
            res[1, ] <- 100 * as.numeric(res[1, ]) * (diff(range(trend$year, na.rm = TRUE)) + 1) / colMeans(MAT, na.rm = TRUE)
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
    if(STATS == "tot.rain")
        res <- colSums(MAT, na.rm = TRUE)
    if(STATS == "rain.int")
        res <- colMeans(MAT, na.rm = TRUE)
    if(STATS == "nb.wet.day")
        res <- colSums(!is.na(MAT) & MAT >= pars$drywet.day)
    if(STATS == "nb.dry.day")
        res <- colSums(!is.na(MAT) & MAT < pars$drywet.day)
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

############################################

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

