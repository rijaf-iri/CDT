
climdex.north.south <- function(index.year.N, index.year.S,
                                start.july, latitude, min.frac)
{
    y.N <- substr(index.year.N$date, 1, 4)
    ifull.year.N <- (index.year.N$nba / index.year.N$nb0) >= min.frac
    year.complet <- y.N

    if(start.july){
        y.S <- substr(index.year.S$date, 1, 4)
        ifull.year.S <- (index.year.S$nba / index.year.S$nb0) >= min.frac

        len.S <- length(y.S)
        len.N <- length(y.N)

        south <- latitude < 0
        idx.s <- which(south)
        idx.n <- which(!south)
        ldx.S <- length(idx.s)
        ldx.N <- length(idx.n)

        is.south <- if(ldx.S > 0) TRUE else FALSE
        is.north <- if(ldx.N > 0) TRUE else FALSE

        if(is.south & !is.north){
            y.nrow.s <- len.S
            d.ncol.s <- ldx.S
            y.nrow.n <- NULL
            d.ncol.n <- NULL
        }
        if(is.north & !is.south){
            y.nrow.s <- NULL
            d.ncol.s <- NULL
            y.nrow.n <- len.N
            d.ncol.n <- ldx.N
        }
        if(is.north & is.south){
            y.nrow.s <- len.S
            d.ncol.s <- ldx.S
            y.nrow.n <- len.N
            d.ncol.n <- ldx.N
        }

        if(is.south & is.north){
            if(len.N > len.S){
                fill.data.N <- rep(TRUE, y.nrow.n)
                fill.data.S <- !is.na(match(y.N, y.S))
                year.complet <- y.N
            }
            if(len.N < len.S){
                fill.data.N <- !is.na(match(y.S, y.N))
                fill.data.S <- rep(TRUE, y.nrow.s)
                year.complet <- y.S
            }
            if(len.N == len.S){
                fill.data.N <- rep(TRUE, y.nrow.s)
                fill.data.S <- rep(TRUE, y.nrow.s)
                year.complet <- y.S
            }
        }else{
            if(is.south){
                fill.data.S <- rep(TRUE, y.nrow.s)
                fill.data.N <- NULL
                year.complet <- y.S
            }
            if(is.north){
                fill.data.S <- NULL
                fill.data.N <- rep(TRUE, y.nrow.n)
                year.complet <- y.N
            }
        }

        index.N <- list(is.north = is.north, idx = idx.n, nrow = y.nrow.n, ncol = d.ncol.n,
                        index = index.year.N$index, full = ifull.year.N, fill = fill.data.N)
        index.S <- list(is.south = is.south, idx = idx.s, nrow = y.nrow.s, ncol = d.ncol.s,
                        index = index.year.S$index, full = ifull.year.S, fill = fill.data.S)
    }else{
        index.N <- list(index = index.year.N$index, full = ifull.year.N)
        index.S <- NULL
    }

    list(year = year.complet, index.N = index.N, index.S = index.S)
}

#########################################################

climdex_aggr.fun <- function(don, start.july, ndim,
                             pars.agrr, pars.trend, index.NS,
                             month.data = FALSE, index.M = NULL,
                             Exceedance.rate = FALSE)
{
    # month
    if(month.data){
        out.m <- matrix(NA, ndim$m.nrow, ndim$ncol)
        out.m[index.M$full, ] <- cdt.data.aggregate(don, index.M$index[index.M$full], pars = pars.agrr)
        if(Exceedance.rate)
            out.m <- out.m * 100 / index.M$nb0
    }else out.m <- NULL

    # year
    index.N <- index.NS$index.N
    index.S <- index.NS$index.S
    out.y <- matrix(NA, ndim$y.nrow, ndim$ncol)
    if(start.july){
        if(index.S$is.south){
            idx.s <- index.S$index[index.S$full]
            if(length(idx.s) > 0){
                out.y.s <- matrix(NA, index.S$nrow, index.S$ncol)
                out.y.s[index.S$full, ] <- cdt.data.aggregate(don[, index.S$idx, drop = FALSE], idx.s, pars = pars.agrr)
                out.y[index.S$fill, index.S$idx] <- out.y.s
            }
        }
        if(index.N$is.north){
            idx.n <- index.N$index[index.N$full]
            if(length(idx.n) > 0){
                out.y.n <- matrix(NA, index.N$nrow, index.N$ncol)
                out.y.n[index.N$full, ] <- cdt.data.aggregate(don[, index.N$idx, drop = FALSE], idx.n, pars = pars.agrr)
                out.y[index.N$fill, index.N$idx] <- out.y.n
            }
        }
    }else{
        idx <- index.N$index[index.N$full]
        if(length(idx) > 0)
            out.y[index.N$full, ] <- cdt.data.aggregate(don, idx, pars = pars.agrr)
    }

    if(Exceedance.rate)
        out.y <- out.y * 100 / nb.Day.Of.Year(pars.trend$year)

    out.tr <- regression.Vector(pars.trend$year, out.y, pars.trend$min.year)
    out.tr[is.nan(out.tr) | is.infinite(out.tr)] <- NA

    return(list(year = out.y, trend = out.tr, month = out.m))
}

#########################################################

climdex.write.cdtstation <- function(don, year, outdir, namedir, head)
{
    dir.csv <- file.path(outdir, 'CDTSTATIONS', namedir)
    dir.csv.year <- file.path(dir.csv, 'Yearly')
    dir.create(dir.csv.year, showWarnings = FALSE, recursive = TRUE)
    dir.csv.trend <- file.path(dir.csv, 'Trend')
    dir.create(dir.csv.trend, showWarnings = FALSE, recursive = TRUE)

    don.year <- round(don$year, 1)
    don.year[is.na(don.year)] <- .cdtData$Config$missval
    don.year <- rbind(cbind(c('ID.STN', 'LON', 'YEAR/LAT'), head), cbind(year, don.year))
    nom.trend <- c("slope", "std.slope", "t-value.slope", "p-value.slope", "intercept",
                   "std.intercept", "t-value.intercept", "p-value.intercept", "R2", "sigma")
    don.trend <- rbind(cbind(c('ID.STN', 'LON', 'VARS/LAT'), head),
                        cbind(nom.trend, round(don$trend, 6)))
                    # cbind(dimnames(don$trend)[[1]], round(don$trend, 6)))
    writeFiles(don.year, file.path(dir.csv.year, paste0(namedir, '.csv')))
    writeFiles(don.trend, file.path(dir.csv.trend, paste0(namedir, '.csv')))

    dir.rds <- file.path(outdir, 'CDTDATASET', namedir)
    dir.rds.year <- file.path(dir.rds, 'Yearly')
    dir.create(dir.rds.year, showWarnings = FALSE, recursive = TRUE)
    dir.rds.trend <- file.path(dir.rds, 'Trend')
    dir.create(dir.rds.trend, showWarnings = FALSE, recursive = TRUE)
    file.year.gz <- gzfile(file.path(dir.rds.year, paste0(namedir, '.rds')), compression = 9)
    file.trend.gz <- gzfile(file.path(dir.rds.trend, paste0(namedir, '.rds')), compression = 9)

    saveRDS(don$year, file = file.year.gz)
    close(file.year.gz)
    saveRDS(don$trend, file = file.trend.gz)
    close(file.trend.gz)
}

#########################################################

climdex.GSL <- function(data.mat,
                        pars = list(min.frac = 0.95,
                                    thres = 5,
                                    days = 6))
{
    nl <- nrow(data.mat)
    miss <- (colSums(!is.na(data.mat)) / nl) < pars$min.frac
    out <- rep(NA, ncol(data.mat))
    if(all(miss)) return(out)
    data.mat <- data.mat[, !miss, drop = FALSE]
    tstart <- !is.na(data.mat) & data.mat > pars$thres
    nogsl <- colSums(tstart) == 0
    gsl <- rep(NA, ncol(tstart))
    if(all(nogsl)){
        out[!miss] <- gsl
        return(out)
    }
    tstart <- tstart[, !nogsl, drop = FALSE]
    res <- rep(NA, ncol(tstart))
    for(j in seq_along(res)){
        rr <- rle(tstart[, j])
        rl <- rr$lengths >= pars$days & rr$values
        if(!any(rl)) next
        rc <- cumsum(rr$lengths)
        ir <- which(rl)[1]
        S <- if(ir == 1) 1 else rc[ir - 1] + 1
        rr <- rle(!is.na(data.mat[, !miss & !nogsl, drop = FALSE][, j]) & !tstart[, j])
        rl <- rr$lengths >= pars$days & rr$values
        if(any(rl)){
            rc <- cumsum(rr$lengths)
            ir <- which(rl & rc > S)[1]
            E <- if(is.na(ir)) nl else rc[ir - 1] + 1
        }else E <- nl
        res[j] <- E - S
    }
    gsl[!nogsl] <- res
    out[!miss] <- gsl
    return(out)
}

#########################################################

climdex_GSL.fun <- function(don, dates, index.NS, ndim,
                            pars.aggr, pars.trend)
{
    dates <- as.Date(dates, "%Y%m%d")
    full.dates <- seq(min(dates), max(dates), 'day')
    if(length(dates) < length(full.dates)){
        ix <- match(full.dates, dates)
        # dates <- full.dates
        don <- don[ix, , drop = FALSE]
    }

    index.N <- index.NS$index.N
    index.S <- index.NS$index.S
    out.y <- matrix(NA, ndim$y.nrow, ndim$ncol)

    if(index.S$is.south){
        idx.s <- index.S$index[index.S$full]
        if(length(idx.s) > 0){
            out.y.s <- matrix(NA, index.S$nrow, index.S$ncol)
            dat <- don[, index.S$idx, drop = FALSE]
            ret <- lapply(idx.s, function(ix){
                climdex.GSL(dat[ix, , drop = FALSE], pars.aggr)
            })
            out.y.s[index.S$full, ] <- do.call(rbind, ret)
            out.y[index.S$fill, index.S$idx] <- out.y.s
        }
    }
    if(index.N$is.north){
        idx.n <- index.N$index[index.N$full]
        if(length(idx.n) > 0){
            out.y.n <- matrix(NA, index.N$nrow, index.N$ncol)
            dat <- don[, index.N$idx, drop = FALSE]
            ret <- lapply(idx.n, function(ix){
                climdex.GSL(dat[ix, , drop = FALSE], pars.aggr)
            })
            out.y.n[index.N$full, ] <- do.call(rbind, ret)
            out.y[index.N$fill, index.N$idx] <- out.y.n
        }
    }

    out.tr <- regression.Vector(pars.trend$year, out.y, pars.trend$min.year)
    out.tr[is.nan(out.tr) | is.infinite(out.tr)] <- NA

    return(list(year = out.y, trend = out.tr, month = NULL))
}
