
computeTvarsProcs <- function(){
    Insert.Messages.Out(paste("Compute", .cdtData$GalParams$Tstep, tolower(.cdtData$GalParams$variable), "temperature ......"), TRUE, "i")

    if(.cdtData$GalParams$data.type == "cdtstation"){
        tmin <- getStnOpenData(.cdtData$GalParams$cdtstation$tmin)
        if(is.null(tmin)) return(NULL)
        tmin <- getCDTdataAndDisplayMsg(tmin, .cdtData$GalParams$Tstep, .cdtData$GalParams$cdtstation$tmin)
        if(is.null(tmin)) return(NULL)

        tmax <- getStnOpenData(.cdtData$GalParams$cdtstation$tmax)
        if(is.null(tmax)) return(NULL)
        tmax <- getCDTdataAndDisplayMsg(tmax, .cdtData$GalParams$Tstep, .cdtData$GalParams$cdtstation$tmax)
        if(is.null(tmax)) return(NULL)

        if(!any(tmin$id %in% tmax$id)){
            Insert.Messages.Out("Tmin & Tmax stations do not match", format = TRUE)
            return(NULL)
        }

        if(!any(tmin$dates %in% tmax$dates)){
            Insert.Messages.Out("Tmin & Tmax dates do not overlap", format = TRUE)
            return(NULL)
        }

        ##################
        idaty <- match(tmin$dates, tmax$dates)
        idaty <- idaty[!is.na(idaty)]
        daty <- tmax$dates[idaty]
        tmax$data <- tmax$data[idaty, , drop = FALSE]
        tmin$data <- tmin$data[tmin$dates %in% tmax$dates, , drop = FALSE]

        ##################
        id <- match(tmin$id, tmax$id)
        id <- id[!is.na(id)]
        stn.id <- tmax$id[id]
        stn.lon <- tmax$lon[id]
        stn.lat <- tmax$lat[id]
        tmax$data <- tmax$data[, id, drop = FALSE]
        tmin$data <- tmin$data[, tmin$id %in% tmax$id, drop = FALSE]

        ##################
        if(.cdtData$GalParams$variable == "Mean") outdon <- (tmax$data + tmin$data) / 2
        if(.cdtData$GalParams$variable == "Range") outdon <- tmax$data - tmin$data
        outdon <- round(outdon, 1)

        ##################
        outdon[is.na(outdon)] <- .cdtData$Config$missval
        xhead <- rbind(stn.id, stn.lon, stn.lat)
        chead <- c('ID.STN', 'LON', paste0(toupper(.cdtData$GalParams$Tstep), '/LAT'))
        infohead <- cbind(chead, xhead)

        outdon <- rbind(infohead, cbind(daty, outdon))
        writeFiles(outdon, .cdtData$GalParams$output)
        rm(tmin, tmax, outdon)
    }

    #######################################################
    if(.cdtData$GalParams$data.type == "cdtnetcdf"){
        tnDataInfo <- getNCDFSampleData(.cdtData$GalParams$cdtnetcdf$tmin$sample)
        if(is.null(tnDataInfo)){
            Insert.Messages.Out("No Tmin data sample found", format = TRUE)
            return(NULL)
        }
        txDataInfo <- getNCDFSampleData(.cdtData$GalParams$cdtnetcdf$tmax$sample)
        if(is.null(txDataInfo)){
            Insert.Messages.Out("No Tmax data sample found", format = TRUE)
            return(NULL)
        }

        ##################
        SP1 <- defSpatialPixels(tnDataInfo[c('lon', 'lat')])
        SP2 <- defSpatialPixels(txDataInfo[c('lon', 'lat')])
        if(is.diffSpatialPixelsObj(SP1, SP2, tol = 1e-04)){
            Insert.Messages.Out("Tmin & Tmax have different resolution or bbox", format = TRUE)
            return(NULL)
        }
        rm(SP1, SP2)

        ##################
        tstep <- .cdtData$GalParams$Tstep
        months <- 1:12
        start.date <- as.Date(.cdtData$GalParams$cdtnetcdf$range[1])
        end.date <- as.Date(.cdtData$GalParams$cdtnetcdf$range[2])

        tmin.DIR <- .cdtData$GalParams$cdtnetcdf$tmin$dir
        tmin.Format <- .cdtData$GalParams$cdtnetcdf$tmin$format
        tmin.errmsg <- "Tmin data not found"
        tminInfo <- ncFilesInfo(tstep, start.date, end.date, months, tmin.DIR, tmin.Format, tmin.errmsg)
        if(is.null(tminInfo)) return(NULL)
        tminInfo$ncinfo <- list(xo = tnDataInfo$ilon, yo = tnDataInfo$ilat, varid = tnDataInfo$varid)

        tminInfo$dates <- tminInfo$dates[tminInfo$exist]
        tminInfo$nc.files <- tminInfo$nc.files[tminInfo$exist]
        tminInfo$exist <- tminInfo$exist[tminInfo$exist]

        tmax.DIR <- .cdtData$GalParams$cdtnetcdf$tmax$dir
        tmax.Format <- .cdtData$GalParams$cdtnetcdf$tmax$format
        tmax.errmsg <- "Tmax data not found"
        tmaxInfo <- ncFilesInfo(tstep, start.date, end.date, months, tmax.DIR, tmax.Format, tmax.errmsg)
        if(is.null(tmaxInfo)) return(NULL)
        tmaxInfo$ncinfo <- list(xo = txDataInfo$ilon, yo = txDataInfo$ilat, varid = txDataInfo$varid)

        tmaxInfo$dates <- tmaxInfo$dates[tmaxInfo$exist]
        tmaxInfo$nc.files <- tmaxInfo$nc.files[tmaxInfo$exist]
        tmaxInfo$exist <- tmaxInfo$exist[tmaxInfo$exist]

        ##################

        if(!any(tminInfo$dates %in% tmaxInfo$dates)){
            Insert.Messages.Out("Tmin & Tmax dates do not match", format = TRUE)
            return(NULL)
        }

        ##################

        tndaty <- match(tmaxInfo$dates, tminInfo$dates)
        tndaty <- tndaty[!is.na(tndaty)]
        tminInfo$dates <- tminInfo$dates[tndaty]
        tminInfo$nc.files <- tminInfo$nc.files[tndaty]
        tminInfo$exist <- tminInfo$exist[tndaty]

        txdaty <- tmaxInfo$dates %in% tminInfo$dates
        tmaxInfo$dates <- tmaxInfo$dates[txdaty]
        tmaxInfo$nc.files <- tmaxInfo$nc.files[txdaty]
        tmaxInfo$exist <- tmaxInfo$exist[txdaty]

        ##################
        nc <- nc_open(tminInfo$nc.files[1])
        tminvarid <- tminInfo$ncinfo$varid
        nc.lon <- nc$var[[tminvarid]]$dim[[tminInfo$ncinfo$xo]]$vals
        nc.lat <- nc$var[[tminvarid]]$dim[[tminInfo$ncinfo$yo]]$vals
        varInfo <- nc$var[[tminvarid]][c('prec', 'units', 'missval')]
        nc_close(nc)

        xo.tn <- order(nc.lon)
        nc.lon <- nc.lon[xo.tn]
        yo.tn <- order(nc.lat)
        nc.lat <- nc.lat[yo.tn]
        len.lon <- length(nc.lon)
        len.lat <- length(nc.lat)

        nc <- nc_open(tmaxInfo$nc.files[1])
        tmaxvarid <- tmaxInfo$ncinfo$varid
        nc.lon1 <- nc$var[[tmaxvarid]]$dim[[tmaxInfo$ncinfo$xo]]$vals
        nc.lat1 <- nc$var[[tmaxvarid]]$dim[[tmaxInfo$ncinfo$yo]]$vals
        nc_close(nc)
        xo.tx <- order(nc.lon1)
        yo.tx <- order(nc.lat1)

        ##################
        dx <- ncdim_def("Lon", "degreeE", nc.lon)
        dy <- ncdim_def("Lat", "degreeN", nc.lat)
        xy.dim <- list(dx, dy)

        if(.cdtData$GalParams$variable == "Mean"){
            ncname <- "tmean"
            longname <- paste(stringr::str_to_title(.cdtData$GalParams$Tstep), "mean temperature")
        }
        if(.cdtData$GalParams$variable == "Range"){
            ncname <- "range"
            longname <- paste(stringr::str_to_title(.cdtData$GalParams$Tstep), "temperature range")
        }
        grdNC <- ncvar_def(ncname, varInfo$units, xy.dim, varInfo$missval, longname = longname, prec = varInfo$prec, compression = 6)

        ##################
        ncdir <- paste0("TEMP_", .cdtData$GalParams$variable, "_", .cdtData$GalParams$Tstep)
        outNCDIR <- file.path(.cdtData$GalParams$output, ncdir)
        dir.create(outNCDIR, showWarnings = FALSE, recursive = TRUE)
        ncprefix <- paste0("temp_", tolower(.cdtData$GalParams$variable))

        ##################
        variable <- .cdtData$GalParams$variable

        ##################

        parsL <- doparallel.cond(length(tminInfo$nc.files) >= 180)
        ret <- cdt.foreach(seq_along(tminInfo$nc.files), parsL, GUI = TRUE,
                           progress = TRUE, FUN = function(jj)
        {
            nc <- try(ncdf4::nc_open(tminInfo$nc.files[jj]), silent = TRUE)
            if(inherits(nc, "try-error")) return(NULL)
            tmin <- ncdf4::ncvar_get(nc, varid = tminInfo$ncinfo$varid)
            ncdf4::nc_close(nc)
            tmin <- if(tminInfo$ncinfo$xo < tminInfo$ncinfo$yo) tmin[xo.tn, yo.tn] else t(tmin)[xo.tn, yo.tn]

            ##################
            nc <- try(ncdf4::nc_open(tmaxInfo$nc.files[jj]), silent = TRUE)
            if(inherits(nc, "try-error")) return(NULL)
            tmax <- ncdf4::ncvar_get(nc, varid = tmaxInfo$ncinfo$varid)
            ncdf4::nc_close(nc)
            tmax <- if(tmaxInfo$ncinfo$xo < tmaxInfo$ncinfo$yo) tmax[xo.tx, yo.tx] else t(tmax)[xo.tx, yo.tx]

            ##################
            if(variable == "Mean") outdon <- (tmax + tmin) / 2
            if(variable == "Range") outdon <- tmax - tmin
            outdon[is.na(outdon)] <- varInfo$missval

            ##################
            outfile <- file.path(outNCDIR, paste0(ncprefix, "_", tminInfo$dates[jj], ".nc"))
            nc <- ncdf4::nc_create(outfile, grdNC)
            ncdf4::ncvar_put(nc, grdNC, outdon)
            ncdf4::nc_close(nc)

            rm(tmin, tmax, outdon); gc()
            return(0)
        })

        rm(tnDataInfo, txDataInfo, tminInfo, tmaxInfo)
    }

    #######################################################
    if(.cdtData$GalParams$data.type == "cdtdataset"){
        tmin <- try(readRDS(.cdtData$GalParams$cdtdataset$tmin), silent = TRUE)
        if(inherits(tmin, "try-error")){
            Insert.Messages.Out(paste("Unable to read", .cdtData$GalParams$cdtdataset$tmin), format = TRUE)
            return(NULL)
        }
        if(.cdtData$GalParams$Tstep != tmin$TimeStep){
            Insert.Messages.Out(paste("Tmin dataset is not a", .cdtData$GalParams$Tstep, "data"), format = TRUE)
            return(NULL)
        }

        tmax <- try(readRDS(.cdtData$GalParams$cdtdataset$tmax), silent = TRUE)
        if(inherits(tmax, "try-error")){
            Insert.Messages.Out(paste("Unable to read", .cdtData$GalParams$cdtdataset$tmax), format = TRUE)
            return(NULL)
        }
        if(.cdtData$GalParams$Tstep != tmax$TimeStep){
            Insert.Messages.Out(paste("Tmax dataset is not a", .cdtData$GalParams$Tstep, "data"), format = TRUE)
            return(NULL)
        }

        ##################
        SP1 <- defSpatialPixels(list(lon = tmin$coords$mat$x, lat = tmin$coords$mat$y))
        SP2 <- defSpatialPixels(list(lon = tmax$coords$mat$x, lat = tmax$coords$mat$y))
        if(is.diffSpatialPixelsObj(SP1, SP2, tol = 1e-04)){
            Insert.Messages.Out("Tmin & Tmax have different resolution or bbox", format = TRUE)
            return(NULL)
        }
        rm(SP1, SP2)

        ##################
        if(tmin$chunksize != tmax$chunksize){
            Insert.Messages.Out("Tmin & Tmax have different chunk size", format = TRUE)
            return(NULL)
        }

        ##################
        if(!any(tmin$dateInfo$date %in% tmax$dateInfo$date)){
            Insert.Messages.Out("Tmin & Tmax dates do not match", format = TRUE)
            return(NULL)
        }

        txdaty <- match(tmin$dateInfo$date, tmax$dateInfo$date)
        txdaty <- txdaty[!is.na(txdaty)]
        tmax$dateInfo$date <- tmax$dateInfo$date[txdaty]
        tmax$dateInfo$index <- tmax$dateInfo$index[txdaty]

        tndaty <- tmin$dateInfo$date %in% tmax$dateInfo$date
        tmin$dateInfo$date <- tmin$dateInfo$date[tndaty]
        tmin$dateInfo$index <- tmin$dateInfo$index[tndaty]

        ##################
        if(.cdtData$GalParams$variable == "Mean"){
            ncname <- "tmean"
            longname <- paste(stringr::str_to_title(.cdtData$GalParams$Tstep), "mean temperature")
        }
        if(.cdtData$GalParams$variable == "Range"){
            ncname <- "range"
            longname <- paste(stringr::str_to_title(.cdtData$GalParams$Tstep), "temperature range")
        }
        index.out <- tmin
        index.out$varInfo$name <- ncname
        index.out$varInfo$longname <- longname

        dataset <- paste0("TEMP_", .cdtData$GalParams$variable, "_", .cdtData$GalParams$Tstep)
        outDIR <- file.path(.cdtData$GalParams$output, dataset)
        dir.create(outDIR, showWarnings = FALSE, recursive = TRUE)

        datadir <- file.path(outDIR, 'DATA')
        dir.create(datadir, showWarnings = FALSE, recursive = TRUE)
        datafileIdx <- file.path(outDIR, paste0(dataset, '.rds'))

        ##################

        chunkfile <- sort(unique(tmin$colInfo$index))
        chunkcalc <- split(chunkfile, ceiling(chunkfile / tmin$chunkfac))

        do.parChunk <- if(tmin$chunkfac > length(chunkcalc)) TRUE else FALSE
        do.parCALC <- if(do.parChunk) FALSE else TRUE

        ##########
        GalParams <- .cdtData$GalParams
        cdtParallelCond <- .cdtData$Config[c('dopar', 'detect.cores', 'nb.cores')]

        ##########
        parsL <- doparallel.cond(do.parCALC & (length(chunkcalc) > 20))
        ret <- cdt.foreach(seq_along(chunkcalc), parsL, GUI = TRUE,
                           progress = TRUE, FUN = function(j)
        {
            tn <- readCdtDatasetChunk.sequence(chunkcalc[[j]], GalParams$cdtdataset$tmin, cdtParallelCond, do.par = do.parChunk)
            tn <- tn[tmin$dateInfo$index, , drop = FALSE]

            tx <- readCdtDatasetChunk.sequence(chunkcalc[[j]], GalParams$cdtdataset$tmax, cdtParallelCond, do.par = do.parChunk)
            tx <- tx[tmax$dateInfo$index, , drop = FALSE]

            if(GalParams$variable == "Mean") outdon <- (tx + tn) / 2
            if(GalParams$variable == "Range") outdon <- tx - tn

            writeCdtDatasetChunk.sequence(outdon, chunkcalc[[j]], index.out, datadir, cdtParallelCond, do.par = do.parChunk)

            rm(tn, tx, outdon); gc()
            return(0)
        })

        ##################

        index.out$dateInfo$index <- seq_along(tmin$dateInfo$date)

        con <- gzfile(datafileIdx, compression = 6)
        open(con, "wb")
        saveRDS(index.out, con)
        close(con)

        rm(tmin, tmax, index.out)
    }

    return(0)
}
