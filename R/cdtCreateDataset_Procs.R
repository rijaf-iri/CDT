
cdtDataset_readData_1 <- function(){
    message <- .cdtData$GalParams[['message']]
    datarepo <- file.path(.cdtData$GalParams$output$dir, .cdtData$GalParams$output$data.name)
    datadir <- file.path(datarepo, 'DATA')
    datafileIdx <- file.path(datarepo, paste0(.cdtData$GalParams$output$data.name, '.rds'))

    #######
    if(.cdtData$GalParams$Update){
        Insert.Messages.Out(message[['6']], TRUE, "i")
        if(!dir.exists(datarepo)){
            Insert.Messages.Out(message[['7']], format = TRUE)
            return(NULL)
        }
        if(!dir.exists(datadir)){
            Insert.Messages.Out(message[['8']], format = TRUE)
            return(NULL)
        }
        if(!file.exists(datafileIdx)){
            data.name <- paste0(.cdtData$GalParams$output$data.name, '.rds')
            Insert.Messages.Out(paste(data.name, message[['9']]), format = TRUE)
            return(NULL)
        }
        cdtTmpVar <- try(readRDS(datafileIdx), silent = TRUE)
        if(inherits(cdtTmpVar, "try-error")){
            Insert.Messages.Out(paste(message[['10']], datafileIdx), format = TRUE)
            return(NULL)
        }
        chunksize <- cdtTmpVar$chunksize
    }else{
        Insert.Messages.Out(message[['11']], TRUE, "i")
        if(!dir.exists(datadir)){
            dir.create(datadir, showWarnings = FALSE, recursive = TRUE)
        }else{
            Insert.Messages.Out(paste(message[['23']], datarepo), TRUE, "e")
            return(NULL)
        }
        cdtTmpVar <- NULL
        cdtTmpVar$TimeStep <- .cdtData$GalParams$tstep
        cdtTmpVar$minhour <- .cdtData$GalParams$minhour
        chunksize <- .cdtData$GalParams$chunk$chunksize
        cdtTmpVar$chunkfac <- .cdtData$GalParams$chunk$chunkfac
    }

    ##################
    ncDataInfo <- getNCDFSampleData(.cdtData$GalParams$NCDF$sample)
    if(is.null(ncDataInfo)){
        Insert.Messages.Out(message[['12']], TRUE, "e")
        return(NULL)
    }

    ##################

    ncInfo <- ncInfo.with.date.range(.cdtData$GalParams$NCDF,
                                     .cdtData$GalParams$date.range,
                                     .cdtData$GalParams$tstep,
                                     .cdtData$GalParams$minhour)
    if(is.null(ncInfo)){
        Insert.Messages.Out(message[['13']], TRUE, "e")
        return(NULL)
    }
    ncInfo$ncinfo <- ncDataInfo

    ncInfo$dates <- ncInfo$dates[ncInfo$exist]
    ncInfo$ncfiles <- ncInfo$ncfiles[ncInfo$exist]
    ncInfo$exist <- ncInfo$exist[ncInfo$exist]

    ###################
    if(.cdtData$GalParams$Update){
        readDate <- !ncInfo$dates %in% cdtTmpVar$dateInfo$date
        if(!any(readDate)){
            Insert.Messages.Out(message[['14']], TRUE, "w")
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
            Insert.Messages.Out(message[['15']], TRUE, "e")
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

    ###################

    Insert.Messages.Out(message[['16']], TRUE, "i")

    chunkdate <- split(seq_along(ncInfo$dates), ceiling(seq_along(ncInfo$dates) / 100))
    checkerFile <- file.path(dirname(ncInfo$ncfiles[1]), '.checker')
    if(file.exists(checkerFile)) unlink(checkerFile)

    parsL <- doparallel.cond(length(chunkdate) >= 5)
    ret <- cdt.foreach(seq_along(chunkdate), parsL = parsL, GUI = TRUE,
                       progress = TRUE, FUN = function(jj)
    {
        if(file.exists(checkerFile)) return(NULL)

        retdat <- lapply(chunkdate[[jj]], function(j){
            nc <- try(ncdf4::nc_open(ncInfo$ncfiles[j]), silent = TRUE)
            if(inherits(nc, "try-error")){
                write('', checkerFile)
                msg <- paste(as.character(nc), '\n', message[['20']], ncInfo$ncfiles[j])
                return(list(data = NULL, msg = msg))
            }
            vars <- ncdf4::ncvar_get(nc, varid = ncInfo$ncinfo$varid)
            ncdf4::nc_close(nc)

            if(ncInfo$ncinfo$nx != nrow(vars) || ncInfo$ncinfo$ny != ncol(vars)){
                write('', checkerFile)
                msg <- paste(message[['21']], ncInfo$ncfiles[j])
                return(list(data = NULL, msg = msg))
            }

            vars <- transposeNCDFData(vars, ncInfo$ncinfo)
            list(data = c(vars), msg = NULL)
        })

        msgs <- lapply(retdat, '[[', 'msg')
        inull <- !sapply(msgs, is.null)
        if(any(inull)) return(msgs[inull])

        retdat <- lapply(retdat, '[[', 'data')
        retdat <- do.call(rbind, retdat)
        retdaty <- ncInfo$dates[chunkdate[[jj]]]

        saveRDS(retdat, file = file.path(datadir, paste0(jj, "_v.rds")))
        saveRDS(retdaty, file = file.path(datadir, paste0(jj, "_d.rds")))

        return(NULL)
    })

    if(file.exists(checkerFile)) unlink(checkerFile)

    inull <- !sapply(ret, is.null)
    if(any(inull)){
        msgs <- unlist(ret[inull])
        for(j in seq_along(msgs)) Insert.Messages.Out(msgs[j], TRUE, "e")
        Insert.Messages.Out(message[['22']], TRUE, "e")
        return(NULL)
    }

    Insert.Messages.Out(message[['17']], TRUE, "s")

    ###################

    Insert.Messages.Out(message[['18']], TRUE, "i")

    ncDaty <- lapply(seq_along(chunkdate), function(jj){
        file.tmp <- file.path(datadir, paste0(jj, "_d.rds"))
        dd <- readRDS(file.tmp)
        unlink(file.tmp)
        return(dd)
    })
    ncDaty <- do.call(c, ncDaty)

    whichParL <- length(col.idx) > length(chunkdate)
    parsL0 <- doparallel.cond(whichParL && length(chunkdate) >= 5)
    parsL1 <- doparallel.cond(whichParL && length(col.idx) >= 50)
    ret <- cdt.foreach(seq_along(chunkdate), parsL = parsL0, GUI = TRUE,
                       progress = TRUE, FUN = function(jj)
    {
        file.tmp <- file.path(datadir, paste0(jj, "_v.rds"))
        tmp <- readRDS(file.tmp)
        unlink(file.tmp)

        ret <- cdt.foreach(seq_along(col.idx), parsL = parsL1,
                           FUN = function(j)
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

    Insert.Messages.Out(message[['19']], TRUE, "s")

    ####################

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

#########################################################

cdtDataset_readData_2 <- function(){
    message <- .cdtData$GalParams[['message']]
    datarepo <- file.path(.cdtData$GalParams$output$dir, .cdtData$GalParams$output$data.name)
    datadir <- file.path(datarepo, 'DATA')
    datafileIdx <- file.path(datarepo, paste0(.cdtData$GalParams$output$data.name, '.rds'))

    #######
    if(.cdtData$GalParams$Update){
        Insert.Messages.Out(message[['6']], TRUE, "i")
        if(!dir.exists(datarepo)){
            Insert.Messages.Out(message[['7']], format = TRUE)
            return(NULL)
        }
        if(!dir.exists(datadir)){
            Insert.Messages.Out(message[['8']], format = TRUE)
            return(NULL)
        }
        if(!file.exists(datafileIdx)){
            data.name <- paste0(.cdtData$GalParams$output$data.name, '.rds')
            Insert.Messages.Out(paste(data.name, message[['9']]), format = TRUE)
            return(NULL)
        }
        cdtTmpVar <- try(readRDS(datafileIdx), silent = TRUE)
        if(inherits(cdtTmpVar, "try-error")){
            Insert.Messages.Out(paste(message[['10']], datafileIdx), format = TRUE)
            return(NULL)
        }
        chunksize <- cdtTmpVar$chunksize
    }else{
        Insert.Messages.Out(message[['11']], TRUE, "i")
        if(!dir.exists(datadir)){
            dir.create(datadir, showWarnings = FALSE, recursive = TRUE)
        }else{
            Insert.Messages.Out(paste(message[['23']], datarepo), TRUE, "e")
            return(NULL)
        }
        cdtTmpVar <- NULL
        cdtTmpVar$TimeStep <- .cdtData$GalParams$tstep
        cdtTmpVar$minhour <- .cdtData$GalParams$minhour
        chunksize <- .cdtData$GalParams$chunk$chunksize
        cdtTmpVar$chunkfac <- .cdtData$GalParams$chunk$chunkfac
    }

    ##################
    ncDataInfo <- getNCDFSampleData(.cdtData$GalParams$NCDF$sample)
    if(is.null(ncDataInfo)){
        Insert.Messages.Out(message[['12']], TRUE, "e")
        return(NULL)
    }

    ##################

    ncInfo <- ncInfo.with.date.range(.cdtData$GalParams$NCDF,
                                     .cdtData$GalParams$date.range,
                                     .cdtData$GalParams$tstep,
                                     .cdtData$GalParams$minhour)
    if(is.null(ncInfo)){
        Insert.Messages.Out(message[['13']], TRUE, "e")
        return(NULL)
    }
    ncInfo$ncinfo <- ncDataInfo

    ncInfo$dates <- ncInfo$dates[ncInfo$exist]
    ncInfo$ncfiles <- ncInfo$ncfiles[ncInfo$exist]
    ncInfo$exist <- ncInfo$exist[ncInfo$exist]

    ###################
    if(.cdtData$GalParams$Update){
        readDate <- !ncInfo$dates %in% cdtTmpVar$dateInfo$date
        if(!any(readDate)){
            Insert.Messages.Out(message[['14']], TRUE, "w")
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
            Insert.Messages.Out(message[['15']], TRUE, "e")
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

    ####################

    Insert.Messages.Out(message[['16']], TRUE, "i")

    checkerFile <- file.path(dirname(ncInfo$ncfiles[1]), '.checker')
    if(file.exists(checkerFile)) unlink(checkerFile)

    parsL <- doparallel.cond(length(which(ncInfo$exist)) >= 100)
    ret <- cdt.foreach(seq_along(ncInfo$ncfiles), parsL = parsL, GUI = TRUE,
                       progress = TRUE, FUN = function(jj)
    {
        if(file.exists(checkerFile)) return(NULL)

        nc <- try(ncdf4::nc_open(ncInfo$ncfiles[jj]), silent = TRUE)
        if(inherits(nc, "try-error")){
            write('', checkerFile)
            msg <- paste(as.character(nc), '\n', message[['20']], ncInfo$ncfiles[jj])
            return(msg)
        }
        xvar <- ncdf4::ncvar_get(nc, varid = ncInfo$ncinfo$varid)
        ncdf4::nc_close(nc)
        if(ncInfo$ncinfo$nx != nrow(xvar) || ncInfo$ncinfo$ny != ncol(xvar)){
            write('', checkerFile)
            msg <- paste(message[['21']], ncInfo$ncfiles[jj])
            return(msg)
        }
        xvar <- transposeNCDFData(xvar, ncInfo$ncinfo)

        lp_chunks <- sample(seq_along(col.idx), length(col.idx))
        for(l in lp_chunks){
            tmp <- xvar[col.idx[[l]]]
            tmp <- c(ncInfo$dates[jj], tmp)
            tmp <- paste0(tmp, collapse = ',')
            tmp <- paste0(tmp, '\n')
            datafile <- file.path(datadir, paste0('chunk_', l))
            cat(tmp, file = datafile, sep = '\n', append = TRUE)
        }

        return(NULL)
    })

    if(file.exists(checkerFile)) unlink(checkerFile)

    inull <- !sapply(ret, is.null)
    if(any(inull)){
        msgs <- unlist(ret[inull])
        for(j in seq_along(msgs)) Insert.Messages.Out(msgs[j], TRUE, "e")
        Insert.Messages.Out(message[['22']], TRUE, "e")
        return(NULL)
    }

    Insert.Messages.Out(message[['17']], TRUE, "s")

    ###################

    Insert.Messages.Out(message[['18']], TRUE, "i")

    ncDaty <- ncInfo$dates

    parsL <- doparallel.cond(length(col.idx) >= 50)
    ret <- cdt.foreach(seq_along(col.idx), parsL = parsL, GUI = TRUE,
                       progress = TRUE, FUN = function(jj)
    {
        file.tmp <- file.path(datadir, paste0('chunk_', jj))
        tmp <- readLines(file.tmp)
        unlink(file.tmp)

        tmp <- trimws(tmp)
        tmp <- tmp[tmp != ""]
        tmp <- strsplit(tmp, ',')
        tmp <- do.call(rbind, tmp)
        dtmp <- tmp[, 1]
        tmp <- tmp[, -1, drop = FALSE]

        it <- match(ncDaty, dtmp)
        tmp <- tmp[it, , drop = FALSE]
        ntmp <- dim(tmp)
        tmp <- as.numeric(tmp)
        dim(tmp) <- ntmp

        file.rds <- file.path(datadir, paste0(jj, ".rds"))
        if(file.exists(file.rds)){
            y <- readRDS(file.rds)
            tmp <- rbind(y, tmp)
        }

        con <- gzfile(file.rds, compression = 7)
        open(con, "wb")
        saveRDS(tmp, con)
        close(con)

        return(0)
    })

    Insert.Messages.Out(message[['19']], TRUE, "s")

    ###################

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
