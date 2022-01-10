 
## stn: a CDT stations data objects
## min.dist: distance in decimal degree
## min.prec: minimum percentage

select.Station.DistPerc <- function(stn, min.dist = 1.0, min.perc = 80,
                                    start.year = 1981, end.year = 2010)
{
    years <- as.numeric(substr(stn$dates, 1, 4))
    it <- years >= start.year & years <= end.year
    stn$dates <- stn$dates[it]
    stn$data <- stn$data[it, , drop = FALSE]

    nna <- colSums(!is.na(stn$data))
    istn <- (nna/length(stn$dates)) >= (min.perc/100)

    if(!any(istn)) return(NULL)

    stn$id <- stn$id[istn]
    stn$lon <- stn$lon[istn]
    stn$lat <- stn$lat[istn]
    stn$dates <- stn$dates
    stn$data <- stn$data[, istn, drop = FALSE]
    nna <- nna[istn]

    dst <- stats::dist(cbind(stn$lon, stn$lat), method = "euclidean")
    dst <- as.matrix(dst)
    diag(dst) <- NA
    ix <- which(!is.na(dst) & dst < min.dist, arr.ind = TRUE)
    ix <- t(apply(ix, 1, sort))
    ix <- ix[!duplicated(ix), , drop = FALSE]

    xx <- lapply(unique(ix[, 1]), function(i){
        unique(c(ix[ix[, 1] == i, ]))
    })

    ix0 <- setdiff(seq_along(stn$id), unique(unlist(xx)))

    intr <- rep(TRUE, length(xx))
    out <- vector(mode = 'list', length(xx))
    for(j in seq_along(xx)){
        if(!intr[j]) next
        xa <- sapply(xx, function(x) any(xx[[j]] %in% x))
        if(sum(xa) == 1){
            out[[j]] <- xx[[j]]
            intr[j] <- FALSE
        }else{
            out[[j]] <- unique(unlist(xx[xa]))
            intr[xa] <- FALSE
        }
    }

    out <- out[!sapply(out, is.null)]
    ix1 <- sapply(out, function(x){
        x[which.max(nna[x])]
    })

    ix <- sort(c(ix0, ix1))

    stn$id <- stn$id[ix]
    stn$lon <- stn$lon[ix]
    stn$lat <- stn$lat[ix]
    stn$data <- stn$data[, ix, drop = FALSE]

    return(stn)
}


select.Station.Validation <- function(df, perc = 20){
    dst <- stats::dist(df[, c('lon', 'lat')], method = "euclidean")
    dmat <- as.matrix(dst)
    hklust <- stats::hclust(dst, method = 'average')

    nl <- length(df$id)
    n0 <- round(nl * perc/100)
    minSize <- floor(nl / n0)
    repeat{
        df$k <- dynamicTreeCut::cutreeDynamic(hklust, minClusterSize = minSize, method = "hybrid",
                                                distM = dmat, deepSplit = 4, verbose = 0)
        nClust <- length(unique(df$k))
        minSize <- minSize - 1
       if(minSize < 3) break
       if(n0 <= nClust) break
    }

    ipt <- lapply(seq(nClust), function(j){
        ij <- df$k == j
        io <- order(df$k[ij], decreasing = TRUE)
        which(ij)[io[1:2]]
    })
    ipt <- sort.int(do.call(c, ipt))
    dp <- df[ipt, , drop = FALSE]

    dmat <- dmat[ipt, ipt]
    dimnames(dmat) <- list(seq(nrow(dmat)), seq(ncol(dmat)))
    ir <- sample.int(nrow(dmat), nClust)
    repeat{
        ir0 <- ir
        for (i in 1:nClust){
            mm <- dmat[ir[-i], -ir[-i], drop = FALSE]
            k <- which.max(mm[(1:ncol(mm) - 1) * nrow(mm) + max.col(t(-mm))])
            ir[i] <- as.numeric(dimnames(mm)[[2]][k])
        }
        if(identical(ir0, ir)) break
    }

    dp[ir, c("id", 'lon', 'lat'), drop = FALSE]
}

