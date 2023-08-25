
cdtDataset_readData <- function(){
    datarepo <- file.path(.cdtData$GalParams$output$dir, .cdtData$GalParams$output$data.name)
    datadir <- file.path(datarepo, 'DATA')
    datafileIdx <- file.path(datarepo, paste0(.cdtData$GalParams$output$data.name, '.rds'))

    #######
    if(.cdtData$GalParams$Update){
        Insert.Messages.Out(.cdtData$GalParams[['message']][['6']], TRUE, "i")
        if(!dir.exists(datarepo)){
            Insert.Messages.Out(.cdtData$GalParams[['message']][['7']], format = TRUE)
            return(NULL)
        }
        if(!dir.exists(datadir)){
            Insert.Messages.Out(.cdtData$GalParams[['message']][['8']], format = TRUE)
            return(NULL)
        }
        if(!file.exists(datafileIdx)){
            Insert.Messages.Out(paste(paste0(.cdtData$GalParams$output$data.name, '.rds'),
                                .cdtData$GalParams[['message']][['9']]), format = TRUE)
            return(NULL)
        }
        cdtTmpVar <- try(readRDS(datafileIdx), silent = TRUE)
        if(inherits(cdtTmpVar, "try-error")){
            Insert.Messages.Out(paste(.cdtData$GalParams[['message']][['10']], datafileIdx), format = TRUE)
            return(NULL)
        }
        chunksize <- cdtTmpVar$chunksize
    }else{
        Insert.Messages.Out(.cdtData$GalParams[['message']][['11']], TRUE, "i")
        if(!dir.exists(datarepo))
            dir.create(datarepo, showWarnings = FALSE, recursive = TRUE)
        if(!dir.exists(datadir))
            dir.create(datadir, showWarnings = FALSE, recursive = TRUE)
        cdtTmpVar <- NULL
        cdtTmpVar$TimeStep <- .cdtData$GalParams$tstep
        chunksize <- .cdtData$GalParams$chunk$chunksize
        cdtTmpVar$chunkfac <- .cdtData$GalParams$chunk$chunkfac
    }

    ##################
    ncDataInfo <- getNCDFSampleData(.cdtData$GalParams$NCDF$sample)
    if(is.null(ncDataInfo)){
        Insert.Messages.Out(.cdtData$GalParams[['message']][['12']], TRUE, "e")
        return(NULL)
    }

    ##################

    ncInfo <- ncInfo.with.date.range(.cdtData$GalParams$NCDF,
                                     .cdtData$GalParams$date.range,
                                     .cdtData$GalParams$tstep)
    if(is.null(ncInfo)){
        Insert.Messages.Out(.cdtData$GalParams[['message']][['13']], TRUE, "e")
        return(NULL)
    }
    ncInfo$ncinfo <- ncDataInfo

    # ncInfo$ncinfo <- ncDataInfo[c('ilon', 'ilat', 'varid')]

    ncInfo$dates <- ncInfo$dates[ncInfo$exist]
    ncInfo$ncfiles <- ncInfo$ncfiles[ncInfo$exist]
    ncInfo$exist <- ncInfo$exist[ncInfo$exist]

    ###################
    if(.cdtData$GalParams$Update){
        readDate <- !ncInfo$dates %in% cdtTmpVar$dateInfo$date
        if(!any(readDate)){
            Insert.Messages.Out(.cdtData$GalParams[['message']][['14']], TRUE, "w")
            return(NULL)
        }
        ncInfo$dates <- ncInfo$dates[readDate]
        ncInfo$ncfiles <- ncInfo$ncfiles[readDate]
        ncInfo$exist <- ncInfo$exist[readDate]
    }

    ##################
    nc.lon <- ncInfo$ncinfo$lon
    nc.lat <- ncInfo$ncinfo$lat
    len.lon <- ncInfo$ncinfo$nx
    len.lat <- ncInfo$ncinfo$ny

    ##################

    if(.cdtData$GalParams$Update){
        SP1 <- cdtTmpVar$coords$mat
        SP1 <- defSpatialPixels(list(lon = SP1$x, lat = SP1$y))
        SP2 <- defSpatialPixels(list(lon = nc.lon, lat = nc.lat))
        if(is.diffSpatialPixelsObj(SP1, SP2, tol = 1e-04)){
            Insert.Messages.Out(.cdtData$GalParams[['message']][['15']], TRUE, "e")
            return(NULL)
        }
        rm(SP1, SP2)
    }

    ###################

    ## square chunk
    nxy.chunksize <- round(sqrt(chunksize))
    seqlon <- seq_along(nc.lon)
    seqlat <- seq_along(nc.lat)
    seqcol <- cbind(id = seq(len.lon * len.lat), expand.grid(x = seqlon, y = seqlat))

    split.lon <- split(seqlon, ceiling(seqlon / nxy.chunksize))
    split.lat <- split(seqlat, ceiling(seqlat / nxy.chunksize))
    xgrid <- expand.grid(x = seq_along(split.lon), y = seq_along(split.lat))

    xarrg <- lapply(seq(nrow(xgrid)), function(j){
        crd <- expand.grid(x = nc.lon[split.lon[[xgrid$x[j]]]],
                           y = nc.lat[split.lat[[xgrid$y[j]]]])
        id <- seqcol$id[(seqcol$x %in% split.lon[[xgrid$x[j]]]) &
                        (seqcol$y %in% split.lat[[xgrid$y[j]]])]
        list(coords = crd, id = id, grp = rep(j, length(id)))
    })

    col.idx <- lapply(xarrg, function(x) x$id)
    col.id <- do.call(c, col.idx)
    col.grp <- do.call(c, lapply(xarrg, function(x) x$grp))
    xy.exp <- do.call(rbind, lapply(xarrg, function(x) x$coords))
    col.order <- order(col.id)

    ###################

    if(!.cdtData$GalParams$Update){
        cdtTmpVar$chunksize <- nxy.chunksize * nxy.chunksize
        cdtTmpVar$coords$mat <- list(x = nc.lon, y = nc.lat)
        cdtTmpVar$coords$df <- xy.exp
        attr(cdtTmpVar$coords$df, "out.attrs") <- NULL
        cdtTmpVar$colInfo <- list(id = col.id, index = col.grp, order = col.order)
        cdtTmpVar$varInfo <- ncInfo$ncinfo$varinfo
    }

    #########################################################

    Insert.Messages.Out(.cdtData$GalParams[['message']][['16']], TRUE, "i")

    chunkdate <- split(seq_along(ncInfo$dates), ceiling(seq_along(ncInfo$dates) / 365))

    parsL <- doparallel.cond(length(chunkdate) >= 10)
    ret <- cdt.foreach(seq_along(chunkdate), parsL = parsL, FUN = function(jj)
    {
        retdat <- lapply(chunkdate[[jj]], function(j){
            nc <- try(ncdf4::nc_open(ncInfo$ncfiles[j]), silent = TRUE)
            if(inherits(nc, "try-error")) return(NULL)
            vars <- ncdf4::ncvar_get(nc, varid = ncInfo$ncinfo$varid)
            ncdf4::nc_close(nc)
            vars <- transposeNCDFData(vars, ncInfo$ncinfo)
            c(vars)
        })
        inull <- sapply(retdat, is.null)
        retdat <- do.call(rbind, retdat)
        retdaty <- ncInfo$dates[chunkdate[[jj]]][!inull]

        saveRDS(retdat, file = file.path(datadir, paste0(jj, "_v.rds")))
        saveRDS(retdaty, file = file.path(datadir, paste0(jj, "_d.rds")))
        return(0)
    })

    Insert.Messages.Out(.cdtData$GalParams[['message']][['17']], TRUE, "s")

    ###################

    ncDaty <- lapply(seq_along(chunkdate), function(jj){
        file.tmp <- file.path(datadir, paste0(jj, "_d.rds"))
        dd <- readRDS(file.tmp)
        unlink(file.tmp)
        return(dd)
    })
    ncDaty <- do.call(c, ncDaty)

    ###################

    Insert.Messages.Out(.cdtData$GalParams[['message']][['18']], TRUE, "i")

    parsL <- doparallel.cond(length(col.idx) >= 20)

    ret <- lapply(seq_along(chunkdate), function(jj){
        file.tmp <- file.path(datadir, paste0(jj, "_v.rds"))
        tmp <- readRDS(file.tmp)
        unlink(file.tmp)

        col.idx <- col.idx
        datadir <- datadir

        ret <- cdt.foreach(seq_along(col.idx), parsL = parsL, FUN = function(j)
        {
            chk <- tmp[, col.idx[[j]], drop = FALSE]
            file.rds <- file.path(datadir, paste0(j, ".rds"))
            if(file.exists(file.rds)){
                y <- readRDS(file.rds)
                chk <- rbind(y, chk)
            }

            con <- gzfile(file.rds, compression = 7)
            open(con, "wb")
            saveRDS(chk, con)
            close(con)

            return(0)
        })

        return(0)
    })

    Insert.Messages.Out(.cdtData$GalParams[['message']][['19']], TRUE, "s")

    #########################################################

    idx <- seq(length(ncDaty))
    if(.cdtData$GalParams$Update){
        Adates <- c(cdtTmpVar$dateInfo$date, ncDaty)
        Aindex <- c(cdtTmpVar$dateInfo$index, max(cdtTmpVar$dateInfo$index) + idx)
    }else{
        Adates <- ncDaty
        Aindex <- idx
    }
    odaty <- order(Adates)
    cdtTmpVar$dateInfo <- list(date = Adates[odaty], index = Aindex[odaty])

    con <- gzfile(datafileIdx, compression = 6)
    open(con, "wb")
    saveRDS(cdtTmpVar, con)
    close(con)

    return(0)
}

###########################################################

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

