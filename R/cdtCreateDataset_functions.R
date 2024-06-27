
##### read chunk files (sequential)
# chunk files
# fileInfo <- "~/PRECIP/PRECIP.rds"
# cdtData <- readRDS(fileInfo) OR separate cdtdataset info files
# don <- readCdtDatasetChunk.sequence(loc, fileInfo, cdtData, do.par = TRUE)
# don <- readCdtDatasetChunk.sequence(loc, fileInfo, do.par = TRUE)
# return matrix,  row: all dates, col: sum of chunk col number

readCdtDatasetChunk.sequence <- function(chunk, fileInfo, parllCond, do.par = TRUE)
{
    datadir <- file.path(dirname(fileInfo), "DATA")

    parsL <- doparallel.cond(length(chunk) >= 20 & do.par, parllCond)
    # parsL <- doparallel.cond(length(chunk) >= 20 & do.par)
    don <- cdt.foreach(chunk, parsL = parsL, FUN = function(j)
    {
        file.rds <- file.path(datadir, paste0(j, ".rds"))
        x <- readRDS(file.rds)
        x
    })

    do.call(cbind, don)
}

####################

writeCdtDatasetChunk.sequence <- function(mat, chunk, cdtData, datadir, parllCond, do.par = TRUE)
{
    col.grp <- cdtData$colInfo$index[cdtData$colInfo$index %in% chunk]
    col.grp <- split(seq(ncol(mat)), col.grp)

    parsL <- doparallel.cond(length(chunk) >= 20 & do.par, parllCond)
    # parsL <- doparallel.cond(length(chunk) >= 20 & do.par)
    don <- cdt.foreach(seq_along(chunk), parsL = parsL, FUN = function(j)
    {
        tmp <- mat[, col.grp[[j]], drop = FALSE]
        file.rds <- file.path(datadir, paste0(chunk[j], ".rds"))
        con <- gzfile(file.rds, compression = 5)
        open(con, "wb")
        saveRDS(tmp, con)
        close(con)
    })

    return(0)
}

####################
##### read several dates, fileInfo and datadir are located in a separated directories
# dates: day, pentad, dekad, month 
# dates <- c('2006021', ...., '2006061')
# fileInfo <- "~/PRECIP/PRECIP.rds"
# datadir <- "~/ClimatoAnalysis_monthly_PRECIP_dek/Aggregated_PRECIP_dek/DATA"
# don <- readCdtDatasetChunk.sepdir.dates.order(fileInfo, dates, do.par = TRUE)
# return matrix,  row: date, col: expand x y coords reorder

readCdtDatasetChunk.sepdir.dates.order <- function(fileInfo, datadir, dates, parllCond, do.par = TRUE,
                                                   coords = FALSE, onedate = FALSE)
{
    cdtdata <- readRDS(fileInfo)
    chunk <- seq(max(cdtdata$colInfo$index))
    idaty <- cdtdata$dateInfo$index[match(dates, cdtdata$dateInfo$date)]
    # dates <- dates[!is.na(idaty)]
    idaty <- idaty[!is.na(idaty)]
    if(length(idaty) == 0) return(NULL)
    if(onedate) idaty <- idaty[1]

    parsL <- doparallel.cond(length(chunk) >= 50 & do.par, parllCond)
    # parsL <- doparallel.cond(length(chunk) >= 50 & do.par)
    don <- cdt.foreach(chunk, parsL = parsL, FUN = function(j)
    {
        file.rds <- file.path(datadir, paste0(j, ".rds"))
        x <- readRDS(file.rds)
        x[idaty, , drop = FALSE]
    })

    don <- do.call(cbind, don)
    don <- don[, cdtdata$colInfo$order, drop = FALSE]
    if(onedate){
        dim(don) <- sapply(cdtdata$coords$mat, length)
        return(c(cdtdata$coords$mat, list(z = don)))
    }
    if(coords) return(c(cdtdata$coords$mat, list(z = don)))
    return(don)
}

####################
##### read several dates
# dates: day, pentad, dekad, month 
# dates <- c('2006021', ...., '2006061')
# fileInfo <- "~/PRECIP/PRECIP.rds"
# don <- readCdtDatasetChunk.multi.dates.order(fileInfo, dates)
# return matrix,  row: date (same dates length), col: expand x y coords
# coords = TRUE; list(x = xcoord, y = ycoord, z = matrix{row: date (same dates length), col: expand x y coords})
# onedate = TRUE; list(x = xcoord, y = ycoord, z = matrix), used by image

readCdtDatasetChunk.multi.dates.order <- function(fileInfo, dates, parllCond, do.par = TRUE,
                                                  coords = FALSE, onedate = FALSE)
{
    datadir <- file.path(dirname(fileInfo), "DATA")
    cdtdata <- readRDS(fileInfo)
    chunk <- seq(max(cdtdata$colInfo$index))
    idaty <- cdtdata$dateInfo$index[match(dates, cdtdata$dateInfo$date)]
    # dates <- dates[!is.na(idaty)]
    idaty <- idaty[!is.na(idaty)]
    if(onedate) idaty <- idaty[1]

    parsL <- doparallel.cond(length(chunk) >= 50 & do.par, parllCond)
    # parsL <- doparallel.cond(length(chunk) >= 50 & do.par)
    don <- cdt.foreach(chunk, parsL = parsL, FUN = function(j)
    {
        file.rds <- file.path(datadir, paste0(j, ".rds"))
        x <- readRDS(file.rds)
        x[idaty, , drop = FALSE]
    })

    don <- do.call(cbind, don)
    don <- don[, cdtdata$colInfo$order, drop = FALSE]
    if(onedate){
        dim(don) <- sapply(cdtdata$coords$mat, length)
        return(c(cdtdata$coords$mat, list(z = don)))
    }
    if(coords) return(c(cdtdata$coords$mat, list(z = don)))
    return(don)
}

########################
##### read pixels
# loc: index of column from expand x y coords, (from sp::over OR cdt.which | findInterval)
# fileInfo <- "~/PRECIP/PRECIP.rds"
# cdtData <- readRDS(fileInfo) OR separate cdtdataset info files
# don <- readCdtDatasetChunk.locations(loc, fileInfo, cdtData, do.par = TRUE)
# don <- readCdtDatasetChunk.locations(loc, fileInfo, do.par = TRUE)
# return matrix,  row: all dates, col: correspond to loc (same length as loc)

readCdtDatasetChunk.locations <- function(loc, fileInfo, cdtData = NULL, chunkDir = "DATA",
                                          parllCond, do.par = TRUE)
{
    if(is.null(cdtData)) cdtData <- readRDS(fileInfo)
    datadir <- file.path(dirname(fileInfo), chunkDir)

    id <- match(loc, cdtData$colInfo$id)
    col.id <- split(cdtData$colInfo$id[id], cdtData$colInfo$index[id])
    chunk <- as.numeric(names(col.id))
    grp <- cdtData$colInfo$index%in%chunk
    col.grp <- split(cdtData$colInfo$id[grp], cdtData$colInfo$index[grp])
    # idx <- lapply(seq_along(chunk), function(j) which(col.grp[[j]]%in%col.id[[j]]))
    idx <- lapply(seq_along(chunk), function(j) match(col.id[[j]], col.grp[[j]]))
    xcrd <- do.call(c, lapply(seq_along(chunk), function(j) col.grp[[j]][idx[[j]]]))
    coords <- cdtData$coords$df[match(xcrd, cdtData$colInfo$id), , drop = FALSE]
    rownames(coords) <- NULL

    parsL <- doparallel.cond(length(chunk) >= 50 & do.par, parllCond)
    # parsL <- doparallel.cond(length(chunk) >= 50 & do.par)
    don <- cdt.foreach(seq_along(chunk), parsL = parsL, FUN = function(j)
    {
        file.rds <- file.path(datadir, paste0(chunk[j], ".rds"))
        x <- readRDS(file.rds)
        x[, idx[[j]], drop = FALSE]
    })

    don <- do.call(cbind, don)
    list(coords = coords, data = don)
}

###########################################################

# cdtdataset <- .cdtData$EnvData$cdtdataset
# fileInfo <- cdtdataset$fileInfo
# xloc <- as.numeric(trimws(tclvalue(.cdtData$EnvData$plot.maps$lonLOC)))
# yloc <- as.numeric(trimws(tclvalue(.cdtData$EnvData$plot.maps$latLOC)))

# padx <- as.numeric(trimws(tclvalue(.cdtData$EnvData$plot.maps$lonPAD)))
# pady <- as.numeric(trimws(tclvalue(.cdtData$EnvData$plot.maps$latPAD)))

cdtdataset.extarct.TS <- function(cdtdataset, fileInfo, xloc, yloc, padx = 0, pady = 0, ...){
    if(padx == 0 & pady == 0)
        cdtdataset.one.pixel(cdtdataset, fileInfo, xloc, yloc, ...)
    else
        cdtdataset.pad.pixel(cdtdataset, fileInfo, xloc, yloc, padx, pady, ...)
}

## extract one pixel
cdtdataset.one.pixel <- function(cdtdataset, fileInfo, xloc, yloc, ...){
    xlon <- cdtdataset$coords$mat$x
    xlat <- cdtdataset$coords$mat$y

    ilon <- xloc
    ilat <- yloc

    iclo <- findInterval(ilon, xlon)
    ilo <- iclo + (2 * ilon > xlon[iclo] + xlon[iclo + 1])
    icla <- findInterval(ilat, xlat)
    ila <- icla + (2 * ilat > xlat[icla] + xlat[icla + 1])

    if(length(ilo) == 0 | length(ila) == 0){
        Insert.Messages.Out(.cdtEnv$tcl$lang$global[['message']][['7']], format = TRUE)
        return(NULL)
    }

    if(is.na(ilo) | is.na(ila)){
        Insert.Messages.Out(.cdtEnv$tcl$lang$global[['message']][['7']], format = TRUE)
        return(NULL)
    }
    ixy <- ilo + length(xlon) * (ila - 1)

    cdtParallelCond <- .cdtData$Config$parallel
    don <- readCdtDatasetChunk.locations(ixy, fileInfo, cdtdataset, parllCond = cdtParallelCond, do.par = FALSE, ...)
    coords <- don$coords
    don <- don$data[cdtdataset$dateInfo$index, 1]
    daty <- cdtdataset$dateInfo$date

    list(data = don, date = daty, coords = coords)
}

## spatially averaged over a rectangle (padding)
cdtdataset.pad.pixel <- function(cdtdataset, fileInfo, xloc, yloc, padx, pady, ...){
    xlon <- cdtdataset$coords$mat$x
    xlat <- cdtdataset$coords$mat$y

    nx <- xlon[2] - xlon[1]
    ny <- xlat[2] - xlat[1]
    padx <- round(padx / nx)
    pady <- round(pady / ny)
    voisin <- expand.grid(x = xloc + nx * (-padx:padx), y =  yloc + ny * (-pady:pady))

    ilon <- voisin$x
    ilat <- voisin$y

    iclo <- findInterval(ilon, xlon)
    ilo <- iclo + (2 * ilon > xlon[iclo] + xlon[iclo + 1])
    icla <- findInterval(ilat, xlat)
    ila <- icla + (2 * ilat > xlat[icla] + xlat[icla + 1])

    if(length(ilo) == 0 | length(ila) == 0){
        Insert.Messages.Out(.cdtEnv$tcl$lang$global[['message']][['7']], format = TRUE)
        return(NULL)
    }

    ina <- !is.na(ilo) & !is.na(ila)
    if(all(!ina)){
        Insert.Messages.Out(.cdtEnv$tcl$lang$global[['message']][['7']], format = TRUE)
        return(NULL)
    }
    ixy <- ilo[ina] + length(xlon) * (ila[ina] - 1)

    cdtParallelCond <- .cdtData$Config$parallel
    don <- readCdtDatasetChunk.locations(ixy, fileInfo, cdtdataset, parllCond = cdtParallelCond, do.par = FALSE, ...)
    don <- rowMeans(don$data, na.rm = TRUE)
    don <- don[cdtdataset$dateInfo$index]
    daty <- cdtdataset$dateInfo$date

    list(data = don, date = daty)
}

###############

## create new index file from new grid
cdtdataset.new.index <- function(index, grid){
    len.lon <- length(grid$lon)
    len.lat <- length(grid$lat)

    nxy.chunksize <- round(sqrt(index$chunksize))
    seqlon <- seq_along(grid$lon)
    seqlat <- seq_along(grid$lat)
    seqcol <- cbind(id = seq(len.lon * len.lat), expand.grid(x = seqlon, y = seqlat))

    split.lon <- split(seqlon, ceiling(seqlon / nxy.chunksize))
    split.lat <- split(seqlat, ceiling(seqlat / nxy.chunksize))
    xgrid <- expand.grid(x = seq_along(split.lon), y = seq_along(split.lat))

    xarrg <- lapply(seq(nrow(xgrid)), function(j){
        crd <- expand.grid(x = grid$lon[split.lon[[xgrid$x[j]]]],
                           y = grid$lat[split.lat[[xgrid$y[j]]]])
        id <- seqcol$id[(seqcol$x %in% split.lon[[xgrid$x[j]]]) &
                        (seqcol$y %in% split.lat[[xgrid$y[j]]])]
        list(coords = crd, id = id, grp = rep(j, length(id)))
    })

    col.idx <- lapply(xarrg, function(x) x$id)
    col.id <- do.call(c, col.idx)
    col.grp <- do.call(c, lapply(xarrg, function(x) x$grp))
    col.order <- order(col.id)

    index$chunksize <- nxy.chunksize * nxy.chunksize
    index$coords$mat <- list(x = grid$lon, y = grid$lat)
    index$colInfo <- list(id = col.id, index = col.grp, order = col.order)
    index$coords$df <- do.call(rbind, lapply(xarrg, function(x) x$coords))
    attr(index$coords$df, "out.attrs") <- NULL

    return(index)
}
