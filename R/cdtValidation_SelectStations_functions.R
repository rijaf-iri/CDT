
select.Station.Validation <- function(df, perc = 20){
    nb <- ceiling(length(df$id) * perc/100)
    stnID <- df$id
    stnSel <- as.matrix(df[, c('lon', 'lat')])
    mdist <- as.matrix(stats::dist(stnSel))
    mdist[upper.tri(mdist)] <- NA
    diag(mdist) <- NA

    while(nrow(stnSel) > nb){
        dst <- min(mdist, na.rm = TRUE)
        im <- which(mdist == dst, arr.ind = TRUE)
        im <- im[1, 1]
        stnSel <- stnSel[-im, , drop = FALSE]
        mdist <- mdist[-im, -im, drop = FALSE]
        stnID <- stnID[-im]
    }

    df[df$id %in% stnID, ]
}

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
