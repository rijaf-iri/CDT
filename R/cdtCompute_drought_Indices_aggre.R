
drought.indices.monit.stn <- function(don, file.aggr){
    don0 <- readRDS(file.aggr)
    ix <- match(don$dates, don0$dates)
    ix <- ix[!is.na(ix)]
    iy <- match(don0$dates, don$dates)
    iy <- iy[!is.na(iy)]
    if(length(ix) == 0){
        don0$dates <- c(don0$dates, don$dates)
        don0$data <- rbind(don0$data, don$data)
    }else{
        don0$dates <- c(don0$dates, don$dates[-iy])
        don0$data[ix, ] <- don$data[iy, , drop = FALSE]
        don0$data <- rbind(don0$data, don$data[-iy, , drop = FALSE])
    }
    return(don0)
}

drought.indices.aggr.stn <- function(don, idaty, agg.index, ifull, file.aggr, monitoring)
{
    don.data <- don$data[idaty, , drop = FALSE]

    data.aggr <- matrix(NA, length(agg.index$index), ncol(don.data))
    pars <- list(min.frac = 0.95, aggr.fun = "sum")
    data.aggr[ifull, ] <- cdt.data.aggregate(don.data, agg.index$index[ifull], pars)

    don <- list(id = don$id, lon = don$lon, lat = don$lat, dates = agg.index$date, data = data.aggr)
    if(monitoring) don <- drought.indices.monit.stn(don, file.aggr)
    return(don)
}

drought.indices.update.stn <- function(don, idaty, file.aggr){
    don$dates <- don$dates[idaty]
    don$data <- don$data[idaty, , drop = FALSE]
    don <- drought.indices.monit.stn(don, file.aggr)
    return(don)
}

##################################

drought.indices.monit.cdt <- function(index.out, file.aggr){
    don0 <- readRDS(file.aggr)
    index.out0 <- don0
    ix <- match(index.out$dateInfo$date, index.out0$dateInfo$date)
    ix <- ix[!is.na(ix)]
    iy <- match(index.out0$dateInfo$date, index.out$dateInfo$date)
    iy <- iy[!is.na(iy)]
    if(length(ix) == 0){
        index.out0$dateInfo$date <- c(index.out0$dateInfo$date, index.out$dateInfo$date)
        index.out0$dateInfo$index <- c(index.out0$dateInfo$index, max(index.out0$dateInfo$index) + index.out$dateInfo$index)
    }else{
        index.out0$dateInfo$date <- c(index.out0$dateInfo$date, index.out$dateInfo$date[-iy])
        index.out0$dateInfo$index <- c(index.out0$dateInfo$index, max(index.out0$dateInfo$index) + seq_along(index.out$dateInfo$index[-iy]))
    }
    return(list(index = index.out0, don = don0, ix = ix, iy = iy))
}

drought.indices.index.cdt <- function(don, outstep, outdates, file.aggr, monitoring)
{
    index.out <- don
    index.out$TimeStep <- outstep
    index.out$dateInfo$date <- outdates
    index.out$dateInfo$index <- seq_along(outdates)

    if(monitoring)
        index.out <- drought.indices.monit.cdt(index.out, file.aggr)
    else
        index.out <- list(index = index.out, don = NULL, ix = NULL, iy = NULL)
    return(index.out)
}

drought.indices.update.index.cdt <- function(index.out, idaty, file.aggr){
    index.out$dateInfo$date <- index.out$dateInfo$date[idaty]
    index.out$dateInfo$index <- seq_along(index.out$dateInfo$date)
    index.out <- drought.indices.monit.cdt(index.out, file.aggr)
    return(index.out)
}

drought.indices.concatenate.cdt <- function(don.data, index.out, file.aggr, chunkcalc, do.parChunk)
{
    don0 <- index.out$don
    ix <- index.out$ix
    iy <- index.out$iy
    cdtParallelCond <- .cdtData$Config[c('dopar', 'detect.cores', 'nb.cores')]
    don.data0 <- readCdtDatasetChunk.sequence(chunkcalc, file.aggr, cdtParallelCond, do.par = do.parChunk)
    if(length(ix) == 0){
        don.data0 <- rbind(don.data0, don.data)
    }else{
        don.data0[don0$dateInfo$index[ix], ] <- don.data[iy, , drop = FALSE]
        don.data0 <- rbind(don.data0, don.data[-iy, , drop = FALSE])
    }
    return(don.data0)
}

drought.indices.aggr.cdt <- function(don, idaty, index, ifull, file.aggr, monitoring,
                                    cdtdataset, index.out, datadir, chunkcalc, do.parChunk)
{
    cdtParallelCond <- .cdtData$Config[c('dopar', 'detect.cores', 'nb.cores')]
    don.data <- readCdtDatasetChunk.sequence(chunkcalc, cdtdataset, cdtParallelCond, do.par = do.parChunk)
    don.data <- don.data[don$dateInfo$index, , drop = FALSE]
    don.data <- don.data[idaty, , drop = FALSE]

    data.aggr <- matrix(NA, length(index), ncol(don.data))
    pars <- list(min.frac = 0.95, aggr.fun = "sum")
    data.aggr[ifull, ] <- cdt.data.aggregate(don.data, index[ifull], pars)
    if(monitoring)
        data.aggr <- drought.indices.concatenate.cdt(data.aggr, index.out, file.aggr, chunkcalc, do.parChunk)

    writeCdtDatasetChunk.sequence(data.aggr, chunkcalc, index.out$index, datadir, cdtParallelCond, do.par = do.parChunk)
    rm(data.aggr, don.data); gc()
    invisible()
}

drought.indices.update.cdt <- function(don, idaty, file.aggr, cdtdataset, index.out,
                                        datadir, chunkcalc, do.parChunk)
{
    cdtParallelCond <- .cdtData$Config[c('dopar', 'detect.cores', 'nb.cores')]
    don.data <- readCdtDatasetChunk.sequence(chunkcalc, cdtdataset, cdtParallelCond, do.par = do.parChunk)
    don.data <- don.data[don$dateInfo$index, , drop = FALSE]
    don.data <- don.data[idaty, , drop = FALSE]
    don.data <- drought.indices.concatenate.cdt(don.data, index.out, file.aggr, chunkcalc, do.parChunk)

    writeCdtDatasetChunk.sequence(don.data, chunkcalc, index.out$index, datadir, cdtParallelCond, do.par = do.parChunk)
    rm(don.data); gc()
    invisible()
}
