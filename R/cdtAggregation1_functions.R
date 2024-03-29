
cdt.aggregate <- function(MAT, pars){
    pars0 <- list(aggr.fun = "sum", opr.fun = ">", opr.thres = 0,
                  rle.fun = ">=", rle.thres = 6)
    miss.args <- !names(pars0) %in% names(pars)
    if(any(miss.args)) pars <- c(pars, pars0[miss.args])

    # ex: x <- c(F, F, T, T, T, T, T, T, F, F, F, F, T, T, F, T, T, T, F, T)
    #  res <- c(6, 2, 3, 1)
    # count: number of T in x, here 12
    # count.rle.max : maximum number of consecutive T in x, here 6
    # count.rle.min : minimum number of consecutive T in x, here 1
    # count.rle.nb  : number of consecutive T in x which satisfy rle.fun and rle.thres
    # if rle.fun = '>' and rle.thres = 2 then the result is 2,  (count of 6 and 3)
    # count.rle.list: return res

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
            if(pars$aggr.fun != "count.rle.list"){
                count.rle.fun <- switch(pars$aggr.fun,
                                        "count.rle.max" = max,
                                        "count.rle.min" = min, 
                                        "count.rle.nb" = 
                                                function(x){
                                                    rle.fun <- get(pars$rle.fun, mode = "function")
                                                    sum(rle.fun(x, pars$rle.thres))
                                                }
                                     )
                res <- sapply(res, count.rle.fun)
            }
        }else{
            res <- colSums(MAT, na.rm = TRUE)
        }
    }else{
        aggr.fun <- switch(pars$aggr.fun,
                           "max" = matrixStats::colMaxs,
                           "min" = matrixStats::colMins,
                           "median" = matrixStats::colMedians,
                           "var" = matrixStats::colVars,
                           "sd" = matrixStats::colSds,
                           "mean" = colMeans,
                           "sum" = colSums
                         )
        res <- aggr.fun(MAT, na.rm = TRUE)
    }

    if(pars$aggr.fun != "count.rle.list")
        res[is.nan(res) | is.infinite(res)] <- NA

    return(res)
}

#############################################

cdt.data.aggregate <- function(MAT, index, pars)
{
    pars0 <- list(min.frac = 0.1, aggr.fun = "sum", opr.fun = ">",
                  opr.thres = 0, rle.fun = ">=", rle.thres = 6)
    miss.args <- !names(pars0) %in% names(pars)
    if(any(miss.args)) pars <- c(pars, pars0[miss.args])

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
## with multiple min.frac

cdt.data.aggregateTS <- function(MAT, index, pars)
{
    pars0 <- list(aggr.fun = "sum", opr.fun = ">",
                  opr.thres = 0, rle.fun = ">=", rle.thres = 6)
    miss.args <- !names(pars0) %in% names(pars)
    if(any(miss.args)) pars <- c(pars, pars0[miss.args])
    nc <- ncol(MAT)

    if(pars$min.frac$unique){
        full <- (index$nba / index$nb0) >= pars$min.frac$all
    }else{
        full <- sapply(index$nb.mon, function(x){
            all(x$nba / x$nb0 >= pars$min.frac$month[x$mo])
        })
    }

    data <- lapply(seq_along(index$index), function(j){
        out <- rep(NA, nc)
        if(!full[j]) return(out)
        don <- MAT[index$index[[j]], , drop = FALSE]
        if(pars$min.frac$unique){
            miss <- (colSums(!is.na(don)) / index$nb0[j]) < pars$min.frac$all
        }else{
            ix <- index$nb.mon[[j]]
            ii <- split(seq_along(ix$tsmo), ix$tsmo)
            miss <- lapply(seq_along(ii), function(i){
                colSums(!is.na(don[ii[[i]], , drop = FALSE]))/ix$nb0[i] < pars$min.frac$month[ix$mo[i]]
            })
            miss <- do.call(rbind, miss)
            miss <- apply(miss, 2, any)
        }
        if(all(miss)) return(out)
        out[!miss] <- cdt.aggregate(don[, !miss, drop = FALSE], pars)
        out
    })

    do.call(rbind, data)
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
            xx[k] <- if(ix > 1) stats::convolve(vx, rep(1/ix, ix), type = "filter") else NA
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

    foo <- switch(fun,
                  "sum" = colSums,
                  "mean" = colMeans,
                  "median" = matrixStats::colMedians,
                  "max" = matrixStats::colMaxs,
                  "min" = matrixStats::colMins,
                  "var" = matrixStats::colVars,
                  "sd" = matrixStats::colSds
                )

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
    # vector, fun: sum, mean, median, var, sd, max, min, convolve
    # matrix, fun: sum, mean, median, var, sd, max, min
    if(is.matrix(x)) foo <- .rollfun.mat
    else if(is.vector(x)) foo <- .rollfun.vec
    else return(NULL)

    align <- match.arg(align)
    if(min.data > win) min.data <- win

    foo(x, win, fun, na.rm, min.data, na.pad, fill, align)
}
