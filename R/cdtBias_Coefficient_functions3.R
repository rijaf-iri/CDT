
multiplicative.bias.index <- function(dates, bias.method, tstep){
    idx <- NULL
    if(bias.method == "mbmon")
        times.stn <- as.numeric(substr(dates, 5, 6))

    if(bias.method == "mbvar"){
        if(tstep == "daily"){
            endmon <- c(31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31)
            vtimes <- cbind(do.call(c, sapply(endmon, seq)) , rep(1:12, endmon), 1:365)
            xdaty <- paste(as.numeric(substr(dates, 7, 8)),
                           as.numeric(substr(dates, 5, 6)),
                           sep = '_')
            xvtm <- paste(vtimes[, 1], vtimes[, 2], sep = '_')
            times.stn <- vtimes[match(xdaty, xvtm), 3]
            times.stn[is.na(times.stn)] <- 59

            ## Add  +/- 5 days
            ix5days <- lapply(unique(times.stn), function(nt){
                ix1 <- which(times.stn == nt)
                ix1 <- c(sapply(ix1, function(x) x + (-5:5)))
                cbind(nt, ix1[ix1 > 0 & ix1 <= length(dates)])
            })
            ix5days <- do.call(rbind, ix5days)
            times.stn <- ix5days[, 1]
            idx <- ix5days[, 2]
        }

        if(tstep == "pentad"){
            vtimes <- cbind(expand.grid(1:6, 1:12), 1:72)
            xdaty <- paste(as.numeric(substr(dates, 7, 7)),
                           as.numeric(substr(dates, 5, 6)),
                           sep = '_')
            xvtm <- paste(vtimes[, 1], vtimes[, 2], sep = '_')
            times.stn <- vtimes[match(xdaty, xvtm), 3]
        }

        if(tstep == "dekadal"){
            vtimes <- cbind(expand.grid(1:3, 1:12), 1:36)
            xdaty <- paste(as.numeric(substr(dates, 7, 7)),
                           as.numeric(substr(dates, 5, 6)),
                           sep = '_')
            xvtm <- paste(vtimes[, 1], vtimes[, 2], sep = '_')
            times.stn <- vtimes[match(xdaty, xvtm), 3]
        }

        if(tstep == "monthly")
            times.stn <- as.numeric(substr(dates, 5, 6))
    }

    index <- split(seq_along(times.stn), times.stn)

    list(index = index, idx = idx)
}

multiplicative.bias.fun <- function(stn, grd, variable, min.length){
    ina <- is.na(stn) | is.na(grd)
    stn[ina] <- NA
    grd[ina] <- NA
    if(variable == "rain"){
        izero <- !ina & (stn < 1e-02 & grd < 1e-02)
        stn[izero] <- NA
        grd[izero] <- NA
    }
    len <- colSums(!is.na(stn))

    biasFun <- biascoeff.getOption("mulBiasFunction")
    if(biasFun == "mean"){
        stn <- colSums(stn, na.rm = TRUE)
        grd <- colSums(grd, na.rm = TRUE)
    }
    if(biasFun == "median"){
        stn <- matrixStats::colMedians(stn, na.rm = TRUE)
        grd <- matrixStats::colMedians(grd, na.rm = TRUE)
    }

    bs <- stn/grd
    bs[len < min.length] <- NA
    # bs[is.na(bs)] <- 1

    if(variable == "rain"){
        bs[is.infinite(bs)] <- 1
        bs[is.nan(bs)] <- 1
        bs[bs < 0.01] <- 0.01
        bs[bs > 3] <- 3
    }else if(variable == "temp"){
        bs[is.infinite(bs)] <- 1.5
        bs[is.nan(bs)] <- 1
        # bs[bs < 0] <- 1
        bs[bs < 0.6] <- 0.6
        bs[bs > 1.5] <- 1.5
    }else{
        bs[is.infinite(bs)] <- 1.5
        bs[is.nan(bs)] <- 1
        bs[bs < 0.6] <- 0.6
        bs[bs > 1.5] <- 1.5
    }

    return(bs)
}

#' @exportS3Method NULL
quantile.mapping.berndistr <- function(x, par.stn, par.grd, distr.name, thres){
    z <- x
    iz <- !is.na(z) & (z <= thres)
    z[iz] <- 0

    pfun <- get(paste0('p', distr.name), mode = 'function')
    qfun <- get(paste0('q', distr.name), mode = 'function')

    na.stn <- Reduce('&', lapply(par.stn, function(v) !is.na(v)))
    na.grd <- Reduce('&', lapply(par.grd, function(v) !is.na(v)))
    ix <- !is.na(x) & na.stn & na.grd

    par.grd <- lapply(par.grd, function(p) p[ix])
    par.stn <- lapply(par.stn, function(p) p[ix])

    pval <- do.call(pfun, c(list(q = z[ix]), par.grd))
    res <- do.call(qfun, c(list(p = pval), par.stn))
    miss <- is.na(res) | is.nan(res) | is.infinite(res)
    res[miss] <- x[ix][miss]
    x[ix] <- res

    return(list(data = x, index = ix))
}

#' @exportS3Method NULL
quantile.mapping.statsdistr <- function(x, par.stn, par.grd, distr.name){
    pkg_env <- as.environment("package:stats")
    pfun <- get(paste0('p', distr.name), mode = 'function', envir = pkg_env)
    qfun <- get(paste0('q', distr.name), mode = 'function', envir = pkg_env)

    na.stn <- Reduce('&', lapply(par.stn, function(v) !is.na(v)))
    na.grd <- Reduce('&', lapply(par.grd, function(v) !is.na(v)))
    ix <- !is.na(x) & na.stn & na.grd

    par.grd <- lapply(par.grd, function(p) p[ix])
    par.stn <- lapply(par.stn, function(p) p[ix])
    y <- x[ix]

    pval <- do.call(pfun, c(list(q = y), par.grd))
    res <- do.call(qfun, c(list(p = pval), par.stn))
    miss <- is.na(res) | is.nan(res) | is.infinite(res)
    res[miss] <- y[miss]
    x[ix] <- res

    return(list(data = x, index = ix))
}

#' @exportS3Method NULL
quantile.mapping.ecdf <- function(x, par.stn, par.grd){
    na.stn <- sapply(par.stn, is.null)
    na.grd <- sapply(par.grd, is.null)
    ix <- !is.na(x) & !na.stn & !na.grd

    par.grd <- par.grd[ix]
    par.stn <- par.stn[ix]
    y <- x[ix]

    res <- mapply(function(p, q, v) stats::quantile(q, p(v)), par.grd, par.stn, y)
    miss <- is.na(res) | is.nan(res) | is.infinite(res)
    res[miss] <- y[miss]
    x[ix] <- res

    return(list(data = x, index = ix))
}

# ##1
# system.time({
#     out1 <- rep(NA, length(y))
#     for(i in 1:length(y)){
#         pval <- par.grd[[i]](y[i])
#         out1[i] <- stats::quantile(par.stn[[i]], pval)
#     }
# })
# ##2
# system.time(
#     out2 <- mapply(function(p, q, v) stats::quantile(q, p(v)), par.grd, par.stn, y)
# )

# # all(round(out1-out2, 10) == 0)
