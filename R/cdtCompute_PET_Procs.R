
computePETProcs <- function(){
    Insert.Messages.Out(paste("Compute", .cdtData$GalParams$Tstep, "PET ......"), TRUE, "i")

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
            Insert.Messages.Out("Tmin & Tmax dates do not match", format = TRUE)
            return(NULL)
        }

        if(.cdtData$GalParams$method == "MHAR"){
            prec <- getStnOpenData(.cdtData$GalParams$cdtstation$prec)
            if(is.null(prec)) return(NULL)
            prec <- getCDTdataAndDisplayMsg(prec, .cdtData$GalParams$Tstep, .cdtData$GalParams$cdtstation$prec)
            if(is.null(prec)) return(NULL)

            if(length(Reduce(intersect, list(tmin$id, tmax$id, prec$id))) == 0){
                Insert.Messages.Out("Tmin, Tmax & Precip stations do not match", format = TRUE)
                return(NULL)
            }
            if(length(Reduce(intersect, list(tmin$dates, tmax$dates, prec$dates))) == 0){
                Insert.Messages.Out("Tmin, Tmax & Precip dates do not match", format = TRUE)
                return(NULL)
            }
        }

        ##################
        inx <- match(tmin$dates, tmax$dates)
        inx <- inx[!is.na(inx)]
        tmax$dates <- tmax$dates[inx]

        if(.cdtData$GalParams$method == "HAR"){
            daty <- tmax$dates
            tmax$data <- tmax$data[inx, , drop = FALSE]
            tmin$data <- tmin$data[tmin$dates %in% tmax$dates, , drop = FALSE]
        }else{
            inp <- match(tmin$dates, prec$dates)
            inp <- inp[!is.na(inp)]
            prec$dates <- prec$dates[inp]

            ixp <- match(tmax$dates, prec$dates)
            ixp <- ixp[!is.na(ixp)]

            daty <- prec$dates[ixp]
            prec$data <- prec$data[ixp, , drop = FALSE]
            tmax$data <- tmax$data[tmax$dates %in% prec$dates, , drop = FALSE]
            tmin$data <- tmin$data[tmin$dates %in% prec$dates, , drop = FALSE]
        }

        ##################
        jnx <- match(tmin$id, tmax$id)
        jnx <- jnx[!is.na(jnx)]
        tmax$id <- tmax$id[jnx]

        if(.cdtData$GalParams$method == "HAR"){
            stn.id <- tmax$id
            stn.lon <- tmax$lon[jnx]
            stn.lat <- tmax$lat[jnx]
            tmax$data <- tmax$data[, jnx, drop = FALSE]
            tmin$data <- tmin$data[, tmin$id %in% tmax$id, drop = FALSE]
        }else{
            jnp <- match(tmin$id, prec$id)
            jnp <- jnp[!is.na(jnp)]
            prec$id <- prec$id[jnp]

            jxp <- match(tmax$id, prec$id)
            jxp <- jxp[!is.na(jxp)]

            stn.id <- prec$id[jxp]
            stn.lon <- prec$lon[jxp]
            stn.lat <- prec$lat[jxp]
            prec$data <- prec$data[, jxp, drop = FALSE]
            tmax$data <- tmax$data[, tmax$id %in% prec$id, drop = FALSE]
            tmin$data <- tmin$data[, tmin$id %in% prec$id, drop = FALSE]
        }

        ##################
        Ra <- Extraterrestrial.Radiation(stn.lat, .cdtData$GalParams$Tstep)
        indx <- cdt.index.RA.PET(daty, .cdtData$GalParams$Tstep)
        Ra <- Ra[indx$index, , drop = FALSE]
        prec <- if(.cdtData$GalParams$method == "MHAR") prec else NULL
        etp <- Ref.ET.Hargreaves(tmax$data, tmin$data, Ra, .cdtData$GalParams$Tstep, prec$data)
        etp <- indx$multi * etp
        etp <- round(etp, 1)

        ##################
        etp[is.na(etp)] <- .cdtData$Config$missval
        xhead <- rbind(stn.id, stn.lon, stn.lat)
        chead <- c('ID.STN', 'LON', paste0(toupper(.cdtData$GalParams$Tstep), '/LAT'))
        infohead <- cbind(chead, xhead)

        etp <- rbind(infohead, cbind(daty, etp))
        writeFiles(etp, .cdtData$GalParams$output)
        rm(tmax, tmin, prec, etp, Ra)
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

        if(.cdtData$GalParams$method == "MHAR"){
            rrDataInfo <- getNCDFSampleData(.cdtData$GalParams$cdtnetcdf$prec$sample)
            if(is.null(rrDataInfo)){
                Insert.Messages.Out("No Precip data sample found", format = TRUE)
                return(NULL)
            }
        }

        ##################
        SP1 <- defSpatialPixels(tnDataInfo[c('lon', 'lat')])
        SP2 <- defSpatialPixels(txDataInfo[c('lon', 'lat')])
        if(is.diffSpatialPixelsObj(SP1, SP2, tol = 1e-04)){
            Insert.Messages.Out("Tmin & Tmax have different resolution or bbox", format = TRUE)
            return(NULL)
        }
        if(.cdtData$GalParams$method == "MHAR"){
            SP3 <- defSpatialPixels(rrDataInfo[c('lon', 'lat')])
            if(is.diffSpatialPixelsObj(SP1, SP3, tol = 1e-04)){
                Insert.Messages.Out("Precip, Tmin & Tmax have different resolution or bbox", format = TRUE)
                return(NULL)
            }
            rm(SP3)
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

        if(.cdtData$GalParams$method == "MHAR"){
            prec.DIR <- .cdtData$GalParams$cdtnetcdf$prec$dir
            prec.Format <- .cdtData$GalParams$cdtnetcdf$prec$format
            prec.errmsg <- "Precip data not found"
            precInfo <- ncFilesInfo(tstep, start.date, end.date, months, prec.DIR, prec.Format, prec.errmsg)
            if(is.null(tmaxInfo)) return(NULL)
            precInfo$ncinfo <- list(xo = rrDataInfo$ilon, yo = rrDataInfo$ilat, varid = rrDataInfo$varid)

            precInfo$dates <- precInfo$dates[precInfo$exist]
            precInfo$nc.files <- precInfo$nc.files[precInfo$exist]
            precInfo$exist <- precInfo$exist[precInfo$exist]
            rm(rrDataInfo)
        }

        rm(tnDataInfo, txDataInfo)

        ##################

        if(!any(tminInfo$dates %in% tmaxInfo$dates)){
            Insert.Messages.Out("Tmin & Tmax dates do not match", format = TRUE)
            return(NULL)
        }

        if(.cdtData$GalParams$method == "MHAR"){
            if(length(Reduce(intersect, list(tminInfo$dates, tmaxInfo$dates, precInfo$dates))) == 0){
                Insert.Messages.Out("Tmin, Tmax & Precip dates do not match", format = TRUE)
                return(NULL)
            }
        }

        ##################
        inx <- match(tminInfo$dates, tmaxInfo$dates)
        inx <- inx[!is.na(inx)]
        tmaxInfo$dates <- tmaxInfo$dates[inx]

        if(.cdtData$GalParams$method == "HAR"){
            tmaxInfo$nc.files <- tmaxInfo$nc.files[inx]
            tmaxInfo$exist <- tmaxInfo$exist[inx]

            tndaty <- tminInfo$dates %in% tmaxInfo$dates
            tminInfo$dates <- tminInfo$dates[tndaty]
            tminInfo$nc.files <- tminInfo$nc.files[tndaty]
            tminInfo$exist <- tminInfo$exist[tndaty]
        }else{
            inp <- match(tminInfo$dates, precInfo$dates)
            inp <- inp[!is.na(inp)]
            precInfo$dates <- precInfo$dates[inp]

            ixp <- match(tmaxInfo$dates, precInfo$dates)
            ixp <- ixp[!is.na(ixp)]

            precInfo$dates <- precInfo$dates[ixp]
            precInfo$nc.files <- precInfo$nc.files[ixp]
            precInfo$exist <- precInfo$exist[ixp]

            txdaty <- tmaxInfo$dates %in% precInfo$dates
            tmaxInfo$dates <- tmaxInfo$dates[txdaty]
            tmaxInfo$nc.files <- tmaxInfo$nc.files[txdaty]
            tmaxInfo$exist <- tmaxInfo$exist[txdaty]

            tndaty <- tminInfo$dates %in% precInfo$dates
            tminInfo$dates <- tminInfo$dates[tndaty]
            tminInfo$nc.files <- tminInfo$nc.files[tndaty]
            tminInfo$exist <- tminInfo$exist[tndaty]
        }

        ##################
        nc <- nc_open(tminInfo$nc.files[1])
        nc.lon <- nc$var[[tminInfo$ncinfo$varid]]$dim[[tminInfo$ncinfo$xo]]$vals
        nc.lat <- nc$var[[tminInfo$ncinfo$varid]]$dim[[tminInfo$ncinfo$yo]]$vals
        nc_close(nc)

        xo.tn <- order(nc.lon)
        nc.lon <- nc.lon[xo.tn]
        yo.tn <- order(nc.lat)
        nc.lat <- nc.lat[yo.tn]
        len.lon <- length(nc.lon)
        len.lat <- length(nc.lat)

        nc <- nc_open(tmaxInfo$nc.files[1])
        nc.lon1 <- nc$var[[tmaxInfo$ncinfo$varid]]$dim[[tmaxInfo$ncinfo$xo]]$vals
        nc.lat1 <- nc$var[[tmaxInfo$ncinfo$varid]]$dim[[tmaxInfo$ncinfo$yo]]$vals
        nc_close(nc)
        xo.tx <- order(nc.lon1)
        yo.tx <- order(nc.lat1)

        if(.cdtData$GalParams$method == "MHAR"){
            nc <- nc_open(precInfo$nc.files[1])
            nc.lon2 <- nc$var[[precInfo$ncinfo$varid]]$dim[[precInfo$ncinfo$xo]]$vals
            nc.lat2 <- nc$var[[precInfo$ncinfo$varid]]$dim[[precInfo$ncinfo$yo]]$vals
            nc_close(nc)
            xo.rr <- order(nc.lon2)
            yo.rr <- order(nc.lat2)
        }

        ##################

        Ra <- Extraterrestrial.Radiation(nc.lat, .cdtData$GalParams$Tstep)
        indx <- cdt.index.RA.PET(tminInfo$dates, .cdtData$GalParams$Tstep)

        ##################
        dx <- ncdim_def("Lon", "degreeE", nc.lon)
        dy <- ncdim_def("Lat", "degreeN", nc.lat)
        xy.dim <- list(dx, dy)

        if(.cdtData$GalParams$method == "MHAR")
            longname <- paste("Modified Hargreaves", .cdtData$GalParams$Tstep, "evapotranspiration")
        else
            longname <- paste("Hargreaves", .cdtData$GalParams$Tstep, "evapotranspiration")
        grdNC <- ncvar_def("pet", 'mm', xy.dim, -99, longname = longname, prec = 'float', compression = 9)

        ##################
        ncdir <- paste0("PET_", .cdtData$GalParams$Tstep)
        outNCDIR <- file.path(.cdtData$GalParams$output, ncdir)
        dir.create(outNCDIR, showWarnings = FALSE, recursive = TRUE)

        ##################
        etp.method <- .cdtData$GalParams$method
        Tstep <- .cdtData$GalParams$Tstep

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
            if(etp.method == "MHAR"){
                nc <- try(ncdf4::nc_open(precInfo$nc.files[jj]), silent = TRUE)
                if(inherits(nc, "try-error")) return(NULL)
                prec <- ncdf4::ncvar_get(nc, varid = precInfo$ncinfo$varid)
                ncdf4::nc_close(nc)
                prec <- if(precInfo$ncinfo$xo < precInfo$ncinfo$yo) prec[xo.rr, yo.rr] else t(prec)[xo.rr, yo.rr]
            }else prec <- NULL

            ##################

            Ra.mat <- matrix(Ra[indx$index[jj], ], nrow = len.lon, ncol = len.lat, byrow = TRUE)
            etp <- Ref.ET.Hargreaves(tmax, tmin, Ra.mat, Tstep, prec)
            etp <- indx$multi[jj] * etp
            etp[is.na(etp)] <- -99

            ##################
            outfile <- file.path(outNCDIR, paste0("pet_", tminInfo$dates[jj], ".nc"))
            nc <- ncdf4::nc_create(outfile, grdNC)
            ncdf4::ncvar_put(nc, grdNC, etp)
            ncdf4::nc_close(nc)

            rm(tmin, tmax, etp, Ra.mat); gc()
            return(0)
        })

        rm(tminInfo, tmaxInfo, Ra)
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

        if(.cdtData$GalParams$method == "MHAR"){
            prec <- try(readRDS(.cdtData$GalParams$cdtdataset$prec), silent = TRUE)
            if(inherits(prec, "try-error")){
                Insert.Messages.Out(paste("Unable to read", .cdtData$GalParams$cdtdataset$prec), format = TRUE)
                return(NULL)
            }
            if(.cdtData$GalParams$Tstep != prec$TimeStep){
                Insert.Messages.Out(paste("Precip dataset is not a", .cdtData$GalParams$Tstep, "data"), format = TRUE)
                return(NULL)
            }
        }

        ##################
        SP1 <- defSpatialPixels(list(lon = tmin$coords$mat$x, lat = tmin$coords$mat$y))
        SP2 <- defSpatialPixels(list(lon = tmax$coords$mat$x, lat = tmax$coords$mat$y))
        if(is.diffSpatialPixelsObj(SP1, SP2, tol = 1e-04)){
            Insert.Messages.Out("Tmin & Tmax have different resolution or bbox", format = TRUE)
            return(NULL)
        }

        if(.cdtData$GalParams$method == "MHAR"){
            SP3 <- defSpatialPixels(list(lon = prec$coords$mat$x, lat = prec$coords$mat$y))
            if(is.diffSpatialPixelsObj(SP1, SP3, tol = 1e-04)){
                Insert.Messages.Out("Precip, Tmin & Tmax have different resolution or bbox", format = TRUE)
                return(NULL)
            }
            rm(SP3)
        }
        rm(SP1, SP2)

        ##################
        if(tmin$chunksize != tmax$chunksize){
            Insert.Messages.Out("Tmin & Tmax have different chunk size", format = TRUE)
            return(NULL)
        }

        if(.cdtData$GalParams$method == "MHAR"){
            if(tmin$chunksize != prec$chunksize){
                Insert.Messages.Out("Precip, Tmin & Tmax have different chunk size", format = TRUE)
                return(NULL)
            }
        }

        ##################
        if(!any(tmin$dateInfo$date %in% tmax$dateInfo$date)){
            Insert.Messages.Out("Tmin & Tmax dates do not match", format = TRUE)
            return(NULL)
        }

        if(.cdtData$GalParams$method == "MHAR"){
            if(length(Reduce(intersect, list(tmin$dateInfo$date, tmax$dateInfo$date, prec$dateInfo$date))) == 0){
                Insert.Messages.Out("Tmin, Tmax & Precip dates do not match", format = TRUE)
                return(NULL)
            }
        }

        ##################
        inx <- match(tmin$dateInfo$date, tmax$dateInfo$date)
        inx <- inx[!is.na(inx)]
        tmax$dateInfo$date <- tmax$dateInfo$date[inx]

        if(.cdtData$GalParams$method == "HAR"){
            tmax$dateInfo$index <- tmax$dateInfo$index[inx]

            tndaty <- tmin$dateInfo$date %in% tmax$dateInfo$date
            tmin$dateInfo$date <- tmin$dateInfo$date[tndaty]
            tmin$dateInfo$index <- tmin$dateInfo$index[tndaty]
        }else{
            inp <- match(tmin$dateInfo$date, prec$dateInfo$date)
            inp <- inp[!is.na(inp)]
            prec$dateInfo$date <- prec$dateInfo$date[inp]

            ixp <- match(tmax$dateInfo$date, prec$dateInfo$date)
            ixp <- ixp[!is.na(ixp)]

            prec$dateInfo$date <- prec$dateInfo$date[ixp]
            prec$dateInfo$index <- prec$dateInfo$index[ixp]

            txdaty <- tmax$dateInfo$date %in% prec$dateInfo$date
            tmax$dateInfo$date <- tmax$dateInfo$date[txdaty]
            tmax$dateInfo$index <- tmax$dateInfo$index[txdaty]

            tndaty <- tmin$dateInfo$date %in% prec$dateInfo$date
            tmin$dateInfo$date <- tmin$dateInfo$date[tndaty]
            tmin$dateInfo$index <- tmin$dateInfo$index[tndaty]
        }

        ##################

        daty <- tmin$dateInfo$date
        indx <- cdt.index.RA.PET(daty, .cdtData$GalParams$Tstep)

        ##################

        if(.cdtData$GalParams$method == "MHAR")
            longname <- paste("Modified Hargreaves", .cdtData$GalParams$Tstep, "evapotranspiration")
        else
            longname <- paste("Hargreaves", .cdtData$GalParams$Tstep, "evapotranspiration")

        index.out <- tmin
        index.out$varInfo$name <- "pet"
        index.out$varInfo$units <- "mm"
        index.out$varInfo$longname <- longname

        dataset <- paste0("PET_", .cdtData$GalParams$Tstep)
        outDIR <- file.path(.cdtData$GalParams$output, dataset)
        dir.create(outDIR, showWarnings = FALSE, recursive = TRUE)

        datadir <- file.path(outDIR, 'DATA')
        dir.create(datadir, showWarnings = FALSE, recursive = TRUE)
        datafileIdx <- file.path(outDIR, paste0(dataset, '.rds'))

        ##################

        chunkfile <- sort(unique(tmin$colInfo$index))
        chunkcalc <- split(chunkfile, ceiling(chunkfile/tmin$chunkfac))

        do.parChunk <- if(tmin$chunkfac > length(chunkcalc)) TRUE else FALSE
        do.parCALC <- if(do.parChunk) FALSE else TRUE

        ##################
        GalParams <- .cdtData$GalParams
        cdtParallelCond <- .cdtData$Config[c('dopar', 'detect.cores', 'nb.cores')]

        ##################
        parsL <- doparallel.cond(do.parCALC & (length(chunkcalc) > 10))
        ret <- cdt.foreach(seq_along(chunkcalc), parsL, GUI = TRUE,
                           progress = TRUE, FUN = function(j)
        {
            tn <- readCdtDatasetChunk.sequence(chunkcalc[[j]], GalParams$cdtdataset$tmin, cdtParallelCond, do.par = do.parChunk)
            tn <- tn[tmin$dateInfo$index, , drop = FALSE]

            tx <- readCdtDatasetChunk.sequence(chunkcalc[[j]], GalParams$cdtdataset$tmax, cdtParallelCond, do.par = do.parChunk)
            tx <- tx[tmax$dateInfo$index, , drop = FALSE]

            if(GalParams$method == "MHAR"){
                rr <- readCdtDatasetChunk.sequence(chunkcalc[[j]], GalParams$cdtdataset$prec, cdtParallelCond, do.par = do.parChunk)
                rr <- rr[prec$dateInfo$index, , drop = FALSE]
            }else rr <- NULL

            lat <- tmin$coords$df$y[tmin$colInfo$index %in% chunkcalc[[j]]]
            Ra <- Extraterrestrial.Radiation(lat, GalParams$Tstep)
            Ra <- Ra[indx$index, , drop = FALSE]
            etp <- Ref.ET.Hargreaves(tx, tn, Ra, GalParams$Tstep, rr)
            etp <- indx$multi * etp

            writeCdtDatasetChunk.sequence(etp, chunkcalc[[j]], index.out, datadir, cdtParallelCond, do.par = do.parChunk)

            rm(tn, tx, rr, Ra, etp); gc()
            return(0)
        })

        ##################

        index.out$dateInfo$date <- daty
        index.out$dateInfo$index <- seq(length(daty))

        con <- gzfile(datafileIdx, compression = 6)
        open(con, "wb")
        saveRDS(index.out, con)
        close(con)

        rm(tmin, tmax, index.out)
    }

    return(0)
}
