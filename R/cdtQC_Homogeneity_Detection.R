
cdt.multiBreaks.Detection <- function(x, cost.func, min.int, h = NULL){
    ## binary segmentation
    change.detectionF <- function(x, s, e, rang, change.res){
        len <- e - s + 1
        len1 <- sum(!is.na(x))

        is.change <- NULL
        if(len >= min.int && len1 >= min.int) is.change <- cost.func(x, h)

        if(!is.null(is.change) && change.count <= 500){
            kk <- is.change$index.change
            out <- list(bounds = c(rang[1], rang[len], kk + rang[1] - 1), test = is.change)
            s1 <- 1
            e1 <- kk - sample(1:3, 1)
            s2 <- kk + sample(1:3, 1)
            e2 <- len
            if(e1 <= 0) e1 <- s1
            if(s2 > len) s2 <- e2
            change.detectionF(x[s1:e1], s1, e1, rang[s1:e1], change.res)
            change.detectionF(x[s2:e2], s2, e2, rang[s2:e2], change.res)
            change.res[[change.count]] <<- out
        }
        change.count <<- change.count + 1
    }

    change.res <- vector('list', 500)
    change.count <- 1
    n <- length(x)
    rang <- 1:n

    change.detectionF(x, 1, n, rang, change.res)
    change.res <- change.res[!sapply(change.res, is.null)]
    change.bounds <- do.call(rbind, lapply(change.res, "[[", "bounds"))
    change.stats <- do.call(rbind, lapply(change.res, "[[", "test"))

    res <- NULL
    if(!is.null(change.stats)){
        change.stats <- matrix(unlist(change.stats), ncol = 3)
        res <- data.frame(change.bounds, change.stats)
        res <- res[, -4]
        names(res) <- c('start', 'end', 'cpt.index', 'statistics', 'max.conf.lev')
    }

    return(res)
}

cdt.mean.cpts <- function(x, cpt){
    nl <- length(x)
    ints <- c(1, sort(cpt), nl)
    nt <- length(ints)
    mns <- numeric(nl)
    for(i in seq(nt - 1)){
        moy <- mean(x[ints[i]:ints[i + 1]], na.rm = TRUE)
        mns[ints[i]:ints[i + 1]] <- rep(moy, ints[i + 1] - ints[i] + 1)
    }
    mns[is.na(x)] <- NA
    return(mns)
}

cdt.changepoints <- function(x, obj, pars){
    n <- length(x)
    tobj <- obj[order(obj$statistics, decreasing = TRUE), , drop = FALSE]
    tobj <- tobj[tobj$max.conf.lev >= pars$conf.lev, , drop = FALSE]
    kmax <- min(pars$kmax, nrow(tobj))
    tobj <- tobj[1:kmax, , drop = FALSE]
    cpt <- tobj$cpt.index
    ncpt <- length(cpt)
    mbic.curve <- numeric(ncpt + 1)
    mbic.curve[1] <- n/2 * log(stats::var(x, na.rm = TRUE))
    for(i in ncpt:1){
        xc <- cpt[1:i]
        mncpt <- sum((x - cdt.mean.cpts(x, xc))^2, na.rm = TRUE)
        nlc <- length(xc)
        if(nlc > 0)
            slog <- 1.5 * nlc * log(n) + 0.5 * sum(log(diff(c(0, sort(xc), n)) / n))
        else
            slog <- 0.5 * sum(log(diff(c(0, n)) / n))

        mbic.curve[i + 1] <- (n/2) * log(mncpt / n) + slog
    }
    pmin <- as.integer(stats::median(which.min(mbic.curve)))

    changepoints <- NULL
    if(pmin > 1){
        changepoints$res <- tobj
        changepoints$cpt$mbic.curve <- mbic.curve
        changepoints$cpt$index <- cpt[1:(pmin - 1)]
        changepoints$cpt$number <- pmin - 1
    }

    return(changepoints)
}

getMean.cptSeg <- function(x, cpt){
    n <- length(x)
    cpt <- sort(cpt)
    ints <- c(1, cpt, n + 1)
    nt <- length(ints)
    st <- ints[1:(nt - 1)]
    ed <- ints[2:nt] - 1
    mns <- numeric(n)
    for(i in 1:(nt - 1)){
        ix <- st[i]:ed[i]
        ll <- ed[i] - st[i] + 1
        mx <- mean(x[ix], na.rm = TRUE)
        mns[ix] <- rep(mx, ll)
    }
    # mns[is.na(x)] <- NA

    return(mns)
}

getTrend.cptSeg0 <- function(x, cpt){
    n <- length(x)
    sx <- sum(x * ((1:n) - ((n + 1)/2)), na.rm = TRUE)
    beta <- 12 * sx / (n * (n + 1) * (n - 1))
    cpt <- sort(cpt)
    ints <- c(1, cpt, n + 1)
    nt <- length(ints)
    st <- ints[1:(nt - 1)]
    ed <- ints[2:nt] - 1
    mns <- numeric(n)
    for(i in 1:(nt - 1)){
        ix <- st[i]:ed[i]
        ll <- ed[i] - st[i] + 1
        mx <- mean(x[ix], na.rm = TRUE)
        mns[ix] <- mx - beta * ((ll + 1)/2) + beta * (1:ll)
    }
    # mns[is.na(x)] <- NA

    return(mns)
}

getTrend.cptSeg <- function(x, cpt){
    n <- length(x)
    cpt <- sort(cpt)
    ints <- c(1, cpt, n + 1)
    nt <- length(ints)
    st <- ints[1:(nt - 1)]
    ed <- ints[2:nt] - 1
    mns <- numeric(n)
    for(i in 1:(nt - 1)){
        ix <- st[i]:ed[i]
        ll <- ed[i] - st[i] + 1
        x1 <- x[ix]
        n1 <- length(x1)
        sx <- sum(x1 * ((1:n1) - ((n1 + 1)/2)), na.rm = TRUE)
        beta <- 12 * sx / (n1 * (n1 + 1) * (n1 - 1))
        mx <- mean(x1, na.rm = TRUE)
        alpha <- mx - beta * ((ll + 1)/2)
        mns[ix] <- alpha + beta * (1:ll)
    }
    # mns[is.na(x)] <- NA

    return(mns)
}

AdjustM.byMean <- function(x, cpt, min.adj, SegAdj = 0){
    message <- .cdtData$EnvData[['message']]

    #####
    n <- length(x)
    ints <- c(1, cpt, n + 1)
    nt <- length(ints)
    st <- ints[1:(nt - 1)]
    ed <- ints[2:nt] - 1
    mus <- numeric(nt - 1)
    res <- numeric(n)
    for(i in 1:(nt - 1))
        mus[i] <- mean(x[st[i]:ed[i]], na.rm = TRUE)
    if(SegAdj == 0){
        xlen <- x[st[nt - 1]:ed[nt - 1]]
        xlen <- xlen[!is.na(xlen)]
        if(length(xlen) >= min.adj){
            for(i in 1:(nt - 1)){
                ix <- st[i]:ed[i]
                difSeg <- mus[i] - mus[nt - 1]
                res[ix] <- x[ix] - difSeg
            }
            msg <- NULL
        }else{
            res <- x
            msg <- message[['21']]
        }
    }else if(SegAdj > 0 & SegAdj <= (nt - 1)){
        xlen <- x[st[SegAdj]:ed[SegAdj]]
        xlen <- xlen[!is.na(xlen)]
        if(length(xlen) >= min.adj){
            for(i in 1:(nt - 1)){
                ix <- st[i]:ed[i]
                difSeg <- mus[i] - mus[SegAdj]
                res[ix] <- x[ix] - difSeg
            }
            msg <- NULL
        }else{
            res <- x
            msg <- message[['21']]
        }
    }else{
        res <- x
        msg <- message[['22']]
    }
    return(list(res = res, msg = msg))
}

AdjustT.byMean <- function(x, cpt, min.adj, SegAdj = 0){
    message <- .cdtData$EnvData[['message']]

    #####
    n <- length(x)
    xs <- sum(x * ((1:n) - ((n + 1)/2)), na.rm = TRUE)
    beta <- 12 * xs /(n * (n + 1) * (n - 1))
    ints <- c(1, cpt, n + 1)
    nt <- length(ints)
    st <- ints[1:(nt - 1)]
    ed <- ints[2:nt] - 1
    mus <- numeric(nt - 1)
    res <- numeric(n)
    for(i in 1:(nt - 1)){
        ix <- st[i]:ed[i]
        ll <- ed[i] - st[i] + 1
        mus[i] <- mean(x[ix], na.rm = TRUE) - beta * ((ll + 1) / 2)
    }
    if(SegAdj == 0){
        xlen <- x[st[nt - 1]:ed[nt - 1]]
        xlen <- xlen[!is.na(xlen)]
        if(length(xlen) >= min.adj){
            for(i in 1:(nt - 1)){
                ix <- st[i]:ed[i]
                difSeg <- (mus[i] - mus[nt - 1]) - beta * (st[i] - st[nt - 1])
                res[ix] <- x[ix] - difSeg
            }
            msg <- NULL
        }else{
            res <- x
            msg <- message[['21']]
        }
    }else if(SegAdj > 0 & SegAdj <= (nt - 1)){
        xlen <- x[st[SegAdj]:ed[SegAdj]]
        xlen <- xlen[!is.na(xlen)]
        if(length(xlen) >= min.adj){
            for(i in 1:(nt - 1)){
                ix <- st[i]:ed[i]
                difSeg <- (mus[i] - mus[SegAdj]) - beta * (st[i] - st[SegAdj])
                res[ix] <- x[ix] - difSeg
            }
            msg <- NULL
        }else{
            res <- x
            msg <- message[['21']]
        }
    }else{
        res <- x
        msg <- message[['22']]
    }
    return(list(res = res, msg = msg))
}

AdjustM.byQM <- function(x, cpt, min.adj, SegAdj = 0){
    message <- .cdtData$EnvData[['message']]

    #####
    n <- length(x)
    ints <- c(1, cpt, n + 1)
    nt <- length(ints)
    st <- ints[1:(nt - 1)]
    ed <- ints[2:nt] - 1
    res <- numeric(n)
    for(i in 1:(nt - 1)) assign(paste0('F', i), stats::ecdf(x[st[i]:ed[i]]))
    if(SegAdj == 0){
        xlen <- x[st[nt - 1]:ed[nt - 1]]
        xlen <- xlen[!is.na(xlen)]
        if(length(xlen) >= min.adj){
            fy <- function(t) stats::quantile(get(paste0('F', nt - 1), mode = "function"), t)
            for(i in 1:(nt - 1)){
                ix <- st[i]:ed[i]
                res[ix] <- fy(get(paste0('F', i), mode = "function")(x[ix]))
            }
            res[st[nt - 1]:ed[nt - 1]] <- x[st[nt - 1]:ed[nt - 1]]
            msg <- NULL
        }else{
            res <- x
            msg <- message[['21']]
        }
    }else if(SegAdj > 0 & SegAdj <= (nt - 1)){
        xlen <- x[st[SegAdj]:ed[SegAdj]]
        xlen <- xlen[!is.na(xlen)]
        if(length(xlen) >= min.adj){
            fy <- function(t) stats::quantile(get(paste0('F', SegAdj), mode = "function"), t)
            for(i in 1:(nt - 1)){
                ix <- st[i]:ed[i]
                res[ix] <- fy(get(paste0('F', i), mode = "function")(x[ix]))
            }
            res[st[SegAdj]:ed[SegAdj]] <- x[st[SegAdj]:ed[SegAdj]]
            msg <- NULL
        }else{
            res <- x
            msg <- message[['21']]
        }
    }else{
        res <- x
        msg <- message[['22']]
    }
    return(list(res = res, msg = msg))
}

AdjustT.byQM <- function(x, cpt, min.adj, SegAdj = 0){
    message <- .cdtData$EnvData[['message']]

    #####
    n <- length(x)
    ints <- c(1, cpt, n + 1)
    nt <- length(ints)
    st <- ints[1:(nt - 1)]
    ed <- ints[2:nt] - 1

    beta <- numeric(nt - 1)
    trend <- numeric(n)
    res <- numeric(n)

    for(i in 1:(nt - 1)){
        ix <- st[i]:ed[i]
        x1 <- x[ix]
        n1 <- length(x1)
        sx <- sum(x1 * ((1:n1) - ((n1 + 1)/2)), na.rm = TRUE)
        beta[i] <- 12 * sx / (n1 * (n1 + 1) * (n1 - 1))
        moy <- mean(x1, na.rm = TRUE)
        trend[ix] <- moy - (beta[i] * ((n1 + 1)/2)) + beta[i] * (1:n1)
    }
    xt <- x - trend

    for(i in 1:(nt - 1)) assign(paste0('F', i), stats::ecdf(xt[st[i]:ed[i]]))
    if(SegAdj == 0){
        xlen <- x[st[nt - 1]:ed[nt - 1]]
        xlen <- xlen[!is.na(xlen)]
        if(length(xlen) >= min.adj){
            fy <- function(t) stats::quantile(get(paste0('F', nt - 1), mode = "function"), t)
            for(i in 1:(nt - 1)){
                ix <- st[i]:ed[i]
                res[ix] <- fy(get(paste0('F', i), mode = "function")(xt[ix]))
            }
            res <- res + trend
            res[st[nt - 1]:ed[nt - 1]] <- x[st[nt - 1]:ed[nt - 1]]
            msg <- NULL
        }else{
            res <- x
            msg <- message[['21']]
        }
    }else if(SegAdj > 0 & SegAdj <= (nt - 1)){
        xlen <- x[st[SegAdj]:ed[SegAdj]]
        xlen <- xlen[!is.na(xlen)]
        if(length(xlen) >= min.adj){
            fy <- function(t) stats::quantile(get(paste0('F', SegAdj), mode = "function"), t)
            for(i in 1:(nt - 1)){
                ix <- st[i]:ed[i]
                res[ix] <- fy(get(paste0('F', i), mode = "function")(xt[ix]))
            }
            res <- res + trend
            res[st[SegAdj]:ed[SegAdj]] <- x[st[SegAdj]:ed[SegAdj]]
            msg <- NULL
        }else{
            res <- x
            msg <- message[['21']]
        }
    }else{
        res <- x
        msg <- message[['22']]
    }
    return(list(res = res, msg = msg))
}
