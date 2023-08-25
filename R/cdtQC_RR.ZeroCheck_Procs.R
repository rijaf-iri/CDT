
qcRRZeroCheckProcs <- function(GeneralParameters){
    don <- getStnOpenData(GeneralParameters$infile)
    if(is.null(don)) return(NULL)
    head <- don[1:4, 1]
    don <- getCDTdataAndDisplayMsg(don, 'daily', GeneralParameters$infile)
    if(is.null(don)) return(NULL)

    ###################
    outdir <- file.path(GeneralParameters$outdir, "FALSE.ZEROS.CHECK_data")
    dataCDTdir <- file.path(outdir, 'CDTDATASET')
    dir.create(dataCDTdir, showWarnings = FALSE, recursive = TRUE)
    dataSTNdir <- file.path(outdir, 'CDTSTATIONS')
    dir.create(dataSTNdir, showWarnings = FALSE, recursive = TRUE)
    file.stn <- file.path(dataSTNdir, GeneralParameters$infile)

    ###################
    don.info <- getStnOpenDataInfo(GeneralParameters$infile)
    if(don.info[[3]]$sepr == "") don.info[[3]]$sepr <- " "
    don.info[[3]]$header <- FALSE
    don.info[[3]]$skip <- 0
    if(is.null(don$elv)) head <- head[1:3]
    don0 <- rbind(cbind(head, do.call(rbind, don[c('id', 'lon', 'lat', 'elv')])),
                  cbind(don$dates, don$data))
    utils::write.table(don0, file = file.stn,
                sep = don.info[[3]]$sepr, na = don.info[[3]]$miss.val,
                col.names = FALSE, row.names = FALSE, quote = FALSE)
    rm(don0)

    ###################

    index.mon <- split(seq_along(don$dates), substr(don$dates, 1, 6))
    params <- GeneralParameters$params

    ###################

    parsL <- doparallel.cond(length(don$id) >= 50)
    checkd <- cdt.foreach(seq_along(don$id), parsL,
                          GUI = TRUE, progress = TRUE,
                          FUN = function(stn)
    {
        istn <- seq_along(don$lon)
        crd0 <- cbind(don$lon[stn], don$lat[stn])
        crds <- do.call(cbind, don[c('lon', 'lat')])
        dist <- as.numeric(fields::rdist.earth(crd0, crds, miles = FALSE))
        io <- order(dist)
        istn <- istn[io]
        dist <- dist[io]
        idst <- dist <= params$max.dist
        idst[istn == stn] <- FALSE
        istn <- istn[idst]
        dist <- dist[idst]
        if(length(istn) < params$min.nbrs) return(NULL)
        if(length(istn) > params$max.nbrs){
            istn <- istn[1:(params$max.nbrs + 1)]
            dist <- dist[1:(params$max.nbrs + 1)]
        }

        res <- lapply(index.mon, function(ix){
            x0 <- don$data[ix, stn]
            x0 <- x0[!is.na(x0)]
            if(length(x0) < params$min.days) return(NULL)

            zr0 <- 100 * sum(x0 == 0) / length(x0)
            ## percentage of zero greater than 75%
            if(zr0 < 75) return(NULL)

            x <- don$data[ix, istn, drop = FALSE]
            ina <- colSums(is.na(x)) > params$min.days
            dst <- dist[!ina]
            if(length(dst) < params$min.nbrs) return(NULL)
            x <- x[, !ina, drop = FALSE]

            nl <- colSums(!is.na(x))
            zr <- 100 * colSums(!is.na(x) & x == 0) / nl
            zr[zr == 0] <- 0.01

            m.zr <- mean(zr)
            ratio <- zr0 / m.zr
            inv.dst <- 1/(dst + 0.001)^2
            ratio1 <- sum(inv.dst * (zr0/zr)) / sum(inv.dst)
            stats <- c(round(zr0, 1), round(zr[1], 1), round(dst[1], 2), round(m.zr, 1),
                       round(max(zr), 1), round(ratio, 3), round(ratio1, 3))
            ret <- NULL
            if(ratio >= params$min.thrs)
                ret <- list(stat = stats, stn = istn[!ina], dist = round(dst, 2))

            return(ret)
        })

        inull <- sapply(res, is.null)
        if(all(inull)) return(NULL)
        res <- res[!inull]
        return(res)
    })

    ###################

    inull <- sapply(checkd, is.null)
    if(!all(inull)){
        nameStat <- c("STN.ID", "YYYYMM", "STN.VAL", "NEAR.STN.VAL", "NEAR.STN.DIST",
                      "NGHBR.AVRG", "NGHBR.MAX", "RATIO.NGHBR.AVRG", "RATIO.NGHBR.IDW")
        stn <- don$id[!inull]
        checkd <- checkd[!inull]
        checkd <- lapply(seq_along(checkd), function(j){
            stat <- do.call(rbind, lapply(checkd[[j]], "[[", "stat"))
            dimnames(stat)[[1]] <- NULL
            stat <- cbind(stn[j], names(checkd[[j]]), stat)
            stat <- data.frame(stat, stringsAsFactors = FALSE)
            names(stat) <- nameStat
            dist <- lapply(checkd[[j]], "[[", "dist")
            istn <- lapply(checkd[[j]], "[[", "stn")
            list(date = names(checkd[[j]]), stat = stat, dist = dist, stn = istn)
        })
        names(checkd) <- stn
        checkd <- list(res = checkd, stn = stn)
    }else checkd <- NULL

    ###################
    .cdtData$EnvData$outzeros <- checkd
    ix <- c('id', 'lon', 'lat', 'dates', 'data')
    if(!is.null(don$elv)) ix <- c(ix, 'elv')
    .cdtData$EnvData$stn.data <- don[ix]

    output <- list(params = GeneralParameters, index = index.mon, info = don.info)
    .cdtData$EnvData$output <- output
    .cdtData$EnvData$PathData <- outdir

    ###################
    file.index <- file.path(outdir, "FalseZeros.rds")
    file.checkd <- file.path(dataCDTdir, "FalseZerosResults.rds")
    file.don <- file.path(dataCDTdir, "StationData.rds")

    saveRDS(output, file.index)
    saveRDS(checkd, file.checkd)
    saveRDS(don[ix], file.don)

    return(0)
}
