
anomaliesCalcProcs <- function(GeneralParameters){
    freqData <- GeneralParameters$intstep

    #############
    ## pour parallel
    GeneralParameters <- GeneralParameters
    cdtParallelCond <- .cdtData$Config[c('dopar', 'detect.cores', 'nb.cores')]

    #############

    if(GeneralParameters$outdir$update){
        don.anom <- try(readRDS(GeneralParameters$outdir$dir), silent = TRUE)
        if(inherits(don.anom, "try-error")){
            Insert.Messages.Out(paste("Unable to read", GeneralParameters$outdir$dir), format = TRUE)
            return(NULL)
        }
        if(freqData != don.anom$params$intstep){
            Insert.Messages.Out(paste("Previous saved anomalies data are not a", freqData), format = TRUE)
            return(NULL)
        }

        if(GeneralParameters$data.type != don.anom$params$data.type){
            Insert.Messages.Out(paste("Previous saved anomalies data are not a", GeneralParameters$data.type), format = TRUE)
            return(NULL)
        }
    }else{
        if(!dir.exists(GeneralParameters$outdir$dir)){
            Insert.Messages.Out(paste(GeneralParameters$outdir$dir, "did not find"), format = TRUE)
            return(NULL)
        }

        if(GeneralParameters$climato$clim.exist){
            don.climato <- try(readRDS(GeneralParameters$climato$clim.file), silent = TRUE)
            if(inherits(don.climato, "try-error")){
                Insert.Messages.Out(paste("Unable to read", GeneralParameters$climato$clim.file), format = TRUE)
                return(NULL)
            }

            if(freqData != don.climato$params$intstep){
                Insert.Messages.Out(paste("The climatologies data are not a", freqData, "data"), format = TRUE)
                return(NULL)
            }

            if(GeneralParameters$data.type != don.climato$params$data.type){
                Insert.Messages.Out(paste("Climatologies data are not a", GeneralParameters$data.type), format = TRUE)
                return(NULL)
            }
        }else{
            allyears <- GeneralParameters$climato$allyears
            year1 <- GeneralParameters$climato$start
            year2 <- GeneralParameters$climato$end
            minyear <- GeneralParameters$climato$minyear
            xwin <- GeneralParameters$climato$window

            if(any(is.na(c(year1, year2, minyear)))){
                Insert.Messages.Out("Invalid base period", format = TRUE)
                return(NULL)
            }
        }
    }

    #####################################################

    if(GeneralParameters$data.type == "cdtstation"){
        don <- getStnOpenData(GeneralParameters$cdtstation$file)
        if(is.null(don)) return(NULL)
        don <- getCDTdataAndDisplayMsg(don, freqData, GeneralParameters$cdtstation$file)
        if(is.null(don)) return(NULL)

        daty <- don$dates
        year <- as.numeric(substr(daty, 1, 4))
    }

    ##############################

    if(GeneralParameters$data.type == "cdtdataset"){
        don <- try(readRDS(GeneralParameters$cdtdataset$index), silent = TRUE)
        if(inherits(don, "try-error")){
            Insert.Messages.Out(paste("Unable to read", GeneralParameters$cdtdataset$index), format = TRUE)
            return(NULL)
        }
        if(freqData != don$TimeStep){
            Insert.Messages.Out(paste("The dataset is not a", freqData, "data"), format = TRUE)
            return(NULL)
        }

        daty <- don$dateInfo$date
        year <- as.numeric(substr(daty, 1, 4))
    }

    #####################################################

    if(GeneralParameters$data.type == "cdtstation"){
        if(GeneralParameters$outdir$update){
            if(length(don$id) != length(don.anom$data$id)){
                Insert.Messages.Out("Number of stations from the input and the previous saved anomalies data do not match", format = TRUE)
                return(NULL)
            }
            
            if(any(don$id != don.anom$data$id)){
                Insert.Messages.Out("Order of stations from the input and the previous saved anomalies data do not match", format = TRUE)
                return(NULL)
            }

            dat.moy <- try(readRDS(don.anom$mean.file), silent = TRUE)
            if(inherits(dat.moy, "try-error")){
                Insert.Messages.Out(paste("Unable to read", don.anom$mean.file), format = TRUE)
                return(NULL)
            }
            if(don.anom$params$anomaly == "Standardized"){
                dat.sds <- try(readRDS(don.anom$sds.file), silent = TRUE)
                if(inherits(dat.sds, "try-error")){
                    Insert.Messages.Out(paste("Unable to read", don.anom$sds.file), format = TRUE)
                    return(NULL)
                }
            }

            clim.index.file <- file.path(dirname(dirname(don.anom$mean.file)), "Climatology.rds")
            don.climato <- try(readRDS(clim.index.file), silent = TRUE)
            if(inherits(don.climato, "try-error")){
                Insert.Messages.Out(paste("Unable to read", clim.index.file), format = TRUE)
                return(NULL)
            }

            index0 <- NULL
            index0$id <- don.climato$index
            anomaly.fonct <- don.anom$params$anomaly
        }else{
            if(GeneralParameters$climato$clim.exist){
                if(length(don$id) != length(don.climato$data$id)){
                    Insert.Messages.Out("Number of stations from data and the climatology do not match", format = TRUE)
                    return(NULL)
                }
                
                if(any(don$id != don.climato$data$id)){
                    Insert.Messages.Out("Order of stations from data and the climatology do not match", format = TRUE)
                    return(NULL)
                }

                mean.file <- file.path(dirname(GeneralParameters$climato$clim.file), 'CDTMEAN', 'CDTMEAN.rds')
                sds.file <- file.path(dirname(GeneralParameters$climato$clim.file), 'CDTSTD', 'CDTSTD.rds')

                dat.moy <- try(readRDS(mean.file), silent = TRUE)
                if(inherits(dat.moy, "try-error")){
                    Insert.Messages.Out(paste("Unable to read", mean.file), format = TRUE)
                    return(NULL)
                }
                if(GeneralParameters$anomaly == "Standardized"){
                    dat.sds <- try(readRDS(sds.file), silent = TRUE)
                    if(inherits(dat.sds, "try-error")){
                        Insert.Messages.Out(paste("Unable to read", sds.file), format = TRUE)
                        return(NULL)
                    }
                }
                index0 <- NULL
                index0$id <- don.climato$index
            }else{
                if(length(unique(year)) < minyear){
                    Insert.Messages.Out("No enough data to calculate climatologies", format = TRUE)
                    return(NULL)
                }

                iyear <- if(allyears) rep(TRUE, length(year)) else year >= year1 & year <= year2
                daty0 <- daty[iyear]
                index0 <- cdt.index.Climatologies(daty0, freqData, xwin)

                don0 <- don$data[iyear, , drop = FALSE]

                dat.clim <- .cdt.Climatologies(index0, don0, minyear, freqData, xwin)
                dat.moy <- round(dat.clim$mean, 1)
                dat.sds <- round(dat.clim$sd, 1)

                rm(dat.clim, don0, daty0)
            }
            anomaly.fonct <- GeneralParameters$anomaly
        }
    }

    ##############################

    if(GeneralParameters$data.type == "cdtdataset"){
        if(GeneralParameters$outdir$update){
            outDIR <- dirname(GeneralParameters$outdir$dir)
            index.file.moy <- don.anom$mean.file
            index.file.sds <- don.anom$sds.file

            index.file.anomal <- file.path(outDIR, "CDTANOM", "CDTANOM.rds")
            index.out <- try(readRDS(index.file.anomal), silent = TRUE)
            if(inherits(index.out, "try-error")){
                Insert.Messages.Out(paste("Unable to read", index.file.anomal), format = TRUE)
                return(NULL)
            }

            SP1 <- defSpatialPixels(list(lon = don$coords$mat$x, lat = don$coords$mat$y))
            SP2 <- defSpatialPixels(list(lon = index.out$coords$mat$x, lat = index.out$coords$mat$y))
            if(is.diffSpatialPixelsObj(SP1, SP2, tol = 1e-04)){
                Insert.Messages.Out("The dataset & anomaly data have different resolution or bbox", format = TRUE)
                return(NULL)
            }
            rm(SP1, SP2)

            clim.index.file <- file.path(dirname(dirname(index.file.moy)), "Climatology.rds")
            don.climato <- try(readRDS(clim.index.file), silent = TRUE)
            if(inherits(don.climato, "try-error")){
                Insert.Messages.Out(paste("Unable to read", clim.index.file), format = TRUE)
                return(NULL)
            }

            index0 <- NULL
            index0$id <- don.climato$index
            rm(don.climato)
        }else{
            outDIR <- file.path(GeneralParameters$outdir$dir, "ANOMALIES_data")
            dir.create(outDIR, showWarnings = FALSE, recursive = TRUE)

            if(GeneralParameters$climato$clim.exist){
                index.file.moy <- file.path(dirname(GeneralParameters$climato$clim.file), 'CDTMEAN', 'CDTMEAN.rds')
                index.file.sds <- file.path(dirname(GeneralParameters$climato$clim.file), 'CDTSTD', 'CDTSTD.rds')

                index.data.climato <- try(readRDS(index.file.moy), silent = TRUE)
                if(inherits(index.data.climato, "try-error")){
                    Insert.Messages.Out(paste("Unable to read", index.file.moy), format = TRUE)
                    return(NULL)
                }

                SP1 <- defSpatialPixels(list(lon = don$coords$mat$x, lat = don$coords$mat$y))
                SP2 <- defSpatialPixels(list(lon = index.data.climato$coords$mat$x, lat = index.data.climato$coords$mat$y))
                if(is.diffSpatialPixelsObj(SP1, SP2, tol = 1e-04)){
                    Insert.Messages.Out("The dataset & climatology have different resolution or bbox", format = TRUE)
                    return(NULL)
                }
                rm(SP1, SP2)

                index0 <- NULL
                index0$id <- don.climato$index
                rm(index.data.climato)
            }else{
                if(length(unique(year)) < minyear){
                    Insert.Messages.Out("No enough data to calculate climatologies", format = TRUE)
                    return(NULL)
                }

                ##################
                out.climato.index <- gzfile(file.path(outDIR, "Climatology.rds"), compression = 7)

                ncdfOUT1 <- file.path(outDIR, 'DATA_NetCDF', 'CDTMEAN')
                dir.create(ncdfOUT1, showWarnings = FALSE, recursive = TRUE)
                ncdfOUT2 <- file.path(outDIR, 'DATA_NetCDF', 'CDTSTD')
                dir.create(ncdfOUT2, showWarnings = FALSE, recursive = TRUE)

                datadir1 <- file.path(outDIR, 'CDTMEAN', 'DATA')
                dir.create(datadir1, showWarnings = FALSE, recursive = TRUE)
                index.file.moy <- file.path(outDIR, 'CDTMEAN', 'CDTMEAN.rds')
                datadir2 <- file.path(outDIR, 'CDTSTD', 'DATA')
                dir.create(datadir2, showWarnings = FALSE, recursive = TRUE)
                index.file.sds <- file.path(outDIR, 'CDTSTD', 'CDTSTD.rds')

                ##################

                iyear <- if(allyears) rep(TRUE, length(year)) else year >= year1 & year <= year2
                daty0 <- daty[iyear]
                index0 <- cdt.index.Climatologies(daty0, freqData, xwin)

                ##################

                params.clim <- c(GeneralParameters[c("intstep", "data.type", "cdtstation", "cdtdataset", "cdtnetcdf")],
                                list(climato = GeneralParameters$climato[c("start", "end", "minyear", "window")],
                                    out.dir = GeneralParameters$outdir$dir))
                output.clim <- list(params = params.clim, index = index0$id)

                saveRDS(output.clim, out.climato.index)
                close(out.climato.index)

                ##################

                index.out.clim <- don
                index.out.clim$varInfo$longname <- paste(freqData, "climatology from:", don$varInfo$longname)

                index.out.clim$dateInfo$date <- index0$id
                index.out.clim$dateInfo$index <- seq_along(index0$id)

                index.file.moy.gz <- gzfile(index.file.moy, compression = 7)
                saveRDS(index.out.clim, index.file.moy.gz)
                close(index.file.moy.gz)

                index.file.sds.gz <- gzfile(index.file.sds, compression = 7)
                saveRDS(index.out.clim, index.file.sds.gz)
                close(index.file.sds.gz)

                #########################################

                chunkfile <- sort(unique(don$colInfo$index))
                chunkcalc <- split(chunkfile, ceiling(chunkfile / don$chunkfac))

                do.parChunk <- if(don$chunkfac > length(chunkcalc)) TRUE else FALSE
                do.parCALC <- if(do.parChunk) FALSE else TRUE

                parsL <- doparallel.cond(do.parCALC & (length(chunkcalc) > 10))
                ret <- cdt.foreach(seq_along(chunkcalc), parsL, GUI = TRUE,
                                   progress = TRUE, FUN = function(jj)
                {
                    don.data <- readCdtDatasetChunk.sequence(chunkcalc[[jj]], GeneralParameters$cdtdataset$index, cdtParallelCond, do.par = do.parChunk)
                    don.data <- don.data[don$dateInfo$index, , drop = FALSE]
                    don.data <- don.data[iyear, , drop = FALSE]

                    dat.clim <- .cdt.Climatologies(index0, don.data, minyear, freqData, xwin)

                    writeCdtDatasetChunk.sequence(dat.clim$mean, chunkcalc[[jj]], index.out.clim, datadir1, cdtParallelCond, do.par = do.parChunk)
                    writeCdtDatasetChunk.sequence(dat.clim$sd, chunkcalc[[jj]], index.out.clim, datadir2, cdtParallelCond, do.par = do.parChunk)
                    rm(dat.clim, don.data); gc()
                })

                ##########################################

                x <- index.out.clim$coords$mat$x
                y <- index.out.clim$coords$mat$y
                dx <- ncdim_def("Lon", "degreeE", x)
                dy <- ncdim_def("Lat", "degreeN", y)
                xy.dim <- list(dx, dy)
                nc.grd <- ncvar_def(index.out.clim$varInfo$name, index.out.clim$varInfo$units, xy.dim, -99,
                                    index.out.clim$varInfo$longname, "float", compression = 9)

                ######################
                ret <- lapply(index0$id, function(id){
                    dat.moy <- readCdtDatasetChunk.multi.dates.order(index.file.moy, id, cdtParallelCond, onedate = TRUE)
                    dat.moy <- dat.moy$z
                    dat.moy[is.na(dat.moy)] <- -99
                    filenc <- file.path(ncdfOUT1, paste0("clim_", id, ".nc"))
                    nc <- nc_create(filenc, nc.grd)
                    ncvar_put(nc, nc.grd, dat.moy)
                    nc_close(nc)

                    dat.sds <- readCdtDatasetChunk.multi.dates.order(index.file.sds, id, cdtParallelCond, onedate = TRUE)
                    dat.sds <- dat.sds$z
                    dat.sds[is.na(dat.sds)] <- -99
                    filenc <- file.path(ncdfOUT2, paste0("clim_", id, ".nc"))
                    nc <- nc_create(filenc, nc.grd)
                    ncvar_put(nc, nc.grd, dat.sds)
                    nc_close(nc)

                    return(0)
                })
            }
        }
    }

    #####################################################

    ### anom
    daty0 <- if(freqData == "monthly") as.Date(paste0(daty, 1), "%Y%m%d") else as.Date(daty, "%Y%m%d")

    daty.range <- get.range.date.time(GeneralParameters$date.range, freqData)

    iyear <- daty0 >= daty.range$start & daty0 <= daty.range$end
    daty <- daty[iyear]
    if(length(daty) == 0){
        Insert.Messages.Out("No data to compute anomaly", format = TRUE)
        return(NULL)
    }
    rm(daty0)

    ########

    index1 <- cdt.index.Anomalies(daty, index0, freqData)
    daty <- index1$date
    index1 <- index1$index

    #####################################################

    if(GeneralParameters$data.type == "cdtstation"){
        don$data <- don$data[iyear, , drop = FALSE]

        data.sds <- if(anomaly.fonct == "Standardized") dat.sds else NULL
        anom <- .cdt.Anomalies(index1, don$data, dat.moy, data.sds, anomaly.fonct)

        #########################################

        if(GeneralParameters$outdir$update){
            anom.dir <- dirname(GeneralParameters$outdir$dir)
            anom.file.rds <- file.path(anom.dir, 'CDTANOM', 'CDTANOM.rds')
            anom.file.csv <- file.path(anom.dir, 'CDTSTATIONS', paste0(freqData, "_Anomaly.csv"))

            data.anom.rds <- readRDS(anom.file.rds)
            data.anom.csv <- read.table(anom.file.csv, sep = ",", colClasses = "character", stringsAsFactors = FALSE)
            infohead <- data.anom.csv[1:3, , drop = FALSE]
            data.anom.csv <- data.anom.csv[-(1:3), -1]

            ixold <- match(daty, don.anom$data$dates)
            ixold <- ixold[!is.na(ixold)]

            if(length(ixold) == 0){
                don.anom$data$dates <- c(don.anom$data$dates, daty)
                data.anom.rds <- rbind(data.anom.rds, anom)
                anom[is.na(anom)] <- .cdtData$Config$missval.anom
                anom <- as.data.frame(anom)
                names(anom) <- names(data.anom.csv)
                data.anom.csv <- rbind(data.anom.csv, anom)
            }else{
                don.anom$data$dates <- c(don.anom$data$dates[-ixold], daty)
                data.anom.rds <- rbind(data.anom.rds[-ixold, , drop = FALSE], anom)
                anom[is.na(anom)] <- .cdtData$Config$missval.anom
                anom <- as.data.frame(anom)
                names(anom) <- names(data.anom.csv)
                data.anom.csv <- rbind(data.anom.csv[-ixold, , drop = FALSE], anom)
            }

            saveRDS(data.anom.rds, anom.file.rds)

            tmp <- data.frame(cbind(don.anom$data$dates, data.anom.csv))
            names(tmp) <- names(infohead)
            data.anom.csv <- rbind(infohead, tmp)
            writeFiles(data.anom.csv, anom.file.csv)

            saveRDS(don.anom, GeneralParameters$outdir$dir)
            .cdtData$EnvData$output <- don.anom
            .cdtData$EnvData$PathAnom <- anom.dir

            rm(tmp, don, anom, data.anom.csv, data.anom.rds, don.anom)
        }else{
            outDIR <- file.path(GeneralParameters$outdir$dir, "ANOMALIES_data")
            dir.create(outDIR, showWarnings = FALSE, recursive = TRUE)
            out.dat.index <- gzfile(file.path(outDIR, "Anomaly.rds"), compression = 7)

            datadir <- file.path(outDIR, 'CDTSTATIONS')
            dir.create(datadir, showWarnings = FALSE, recursive = TRUE)
            out.cdt.anomal <- file.path(datadir, paste0(freqData, "_Anomaly.csv"))

            dataOUT3 <- file.path(outDIR, 'CDTANOM')
            dir.create(dataOUT3, showWarnings = FALSE, recursive = TRUE)
            out.cdt.anom <- gzfile(file.path(dataOUT3, 'CDTANOM.rds'), compression = 7)

            #############
            saveRDS(anom, out.cdt.anom)
            close(out.cdt.anom)

            #############
            xhead <- rbind(don$id, don$lon, don$lat)
            chead <- c('ID.STN', 'LON', paste0(toupper(freqData), "/LAT"))
            infohead <- cbind(chead, xhead)

            #############
            anom <- round(anom, 2)
            anom[is.na(anom)] <- .cdtData$Config$missval.anom
            anom <- rbind(infohead, cbind(daty, anom))
            writeFiles(anom, out.cdt.anomal)

            #############
            if(GeneralParameters$climato$clim.exist){
                output <- list(params = GeneralParameters,
                            data = list(id = don$id, lon = don$lon, lat = don$lat, dates = daty),
                            mean.file = mean.file, sds.file = sds.file)
                .cdtData$EnvData$output <- output
                .cdtData$EnvData$PathAnom <- outDIR

                saveRDS(output, out.dat.index)
                close(out.dat.index)
            }else{
                out.cdt.clim.moy <- file.path(datadir, paste0(freqData, "_Climatology_mean.csv"))
                out.cdt.clim.sds <- file.path(datadir, paste0(freqData, "_Climatology_std.csv"))

                out.climato.index <- gzfile(file.path(outDIR, "Climatology.rds"), compression = 7)

                dataOUT1 <- file.path(outDIR, 'CDTMEAN')
                dir.create(dataOUT1, showWarnings = FALSE, recursive = TRUE)
                index.file.moy <- file.path(dataOUT1, 'CDTMEAN.rds')
                out.cdt.moy <- gzfile(index.file.moy, compression = 7)

                dataOUT2 <- file.path(outDIR, 'CDTSTD')
                dir.create(dataOUT2, showWarnings = FALSE, recursive = TRUE)
                index.file.sds <- file.path(dataOUT2, 'CDTSTD.rds')
                out.cdt.sds <- gzfile(index.file.sds, compression = 7)

                output <- list(params = GeneralParameters,
                            data = list(id = don$id, lon = don$lon, lat = don$lat, dates = daty),
                            mean.file = index.file.moy, sds.file = index.file.sds)
                .cdtData$EnvData$output <- output
                .cdtData$EnvData$PathAnom <- outDIR

                params.clim <- c(GeneralParameters[c("intstep", "data.type", "cdtstation", "cdtdataset", "cdtnetcdf")],
                                list(climato = GeneralParameters$climato[c("start", "end", "minyear", "window")],
                                    out.dir = GeneralParameters$outdir$dir))
                output.clim <- list(params = params.clim,
                            data = list(id = don$id, lon = don$lon, lat = don$lat),
                            index = index0$id)

                saveRDS(output.clim, out.climato.index)
                close(out.climato.index)
                saveRDS(output, out.dat.index)
                close(out.dat.index)
                saveRDS(dat.moy, out.cdt.moy)
                close(out.cdt.moy)
                saveRDS(dat.sds, out.cdt.sds)
                close(out.cdt.sds)

                ##################
                infohead[3, 1] <- "INDEX/LAT"
                dat.moy[is.na(dat.moy)] <- .cdtData$Config$missval
                dat.moy <- rbind(infohead, cbind(index0$id, dat.moy))
                writeFiles(dat.moy, out.cdt.clim.moy)

                dat.sds[is.na(dat.sds)] <- .cdtData$Config$missval
                dat.sds <- rbind(infohead, cbind(index0$id, dat.sds))
                writeFiles(dat.sds, out.cdt.clim.sds)
            }
            rm(output, don, anom)
        }
    }

    ##############################

    if(GeneralParameters$data.type == "cdtdataset"){
        ncdfOUT3 <- file.path(outDIR, 'DATA_NetCDF', 'CDTANOM')
        datadir3 <- file.path(outDIR, 'CDTANOM', 'DATA')
        index.file.anomal <- file.path(outDIR, 'CDTANOM', 'CDTANOM.rds')

        if(GeneralParameters$outdir$update){
            anomaly.fonct <- don.anom$params$anomaly
            out.anom.index <- gzfile(GeneralParameters$outdir$dir, compression = 7)

            ixold <- match(daty, don.anom$dates)
            ixold <- ixold[!is.na(ixold)]

            if(length(ixold) == 0){
                newdaty <- c(index.out$dateInfo$date, daty)
                newindex <- c(index.out$dateInfo$index, max(index.out$dateInfo$index) + seq_along(daty))
            }else{
                newdaty <- c(index.out$dateInfo$date[-ixold], daty)
                newindex <- c(index.out$dateInfo$index[-ixold], max(index.out$dateInfo$index[-ixold]) + seq_along(daty))
            }

            ##########
            index.out$dateInfo$date <- newdaty
            index.out$dateInfo$index <- newindex
            saveRDS(index.out, index.file.anomal)

            don.anom$dates <- newdaty
            saveRDS(don.anom, out.anom.index)
            close(out.anom.index)

            .cdtData$EnvData$output <- don.anom
            .cdtData$EnvData$PathAnom <- outDIR
        }else{
            anomaly.fonct <- GeneralParameters$anomaly
            out.anom.index <- gzfile(file.path(outDIR, "Anomaly.rds"), compression = 7)

            dir.create(ncdfOUT3, showWarnings = FALSE, recursive = TRUE)
            dir.create(datadir3, showWarnings = FALSE, recursive = TRUE)

            index.out <- don
            index.out$varInfo$longname <- paste(freqData, "anomaly from:", don$varInfo$longname)
            index.out$dateInfo$date <- daty
            index.out$dateInfo$index <- seq_along(daty)

            ##########
            saveRDS(index.out, index.file.anomal)

            output <- list(params = GeneralParameters, dates = daty, mean.file = index.file.moy, sds.file = index.file.sds)
            saveRDS(output, out.anom.index)
            close(out.anom.index)

            .cdtData$EnvData$output <- output
            .cdtData$EnvData$PathAnom <- outDIR
        }

        #########################################

        chunkfile <- sort(unique(don$colInfo$index))
        chunkcalc <- split(chunkfile, ceiling(chunkfile / don$chunkfac))

        do.parChunk <- if(don$chunkfac > length(chunkcalc)) TRUE else FALSE
        do.parCALC <- if(do.parChunk) FALSE else TRUE

        parsL <- doparallel.cond(do.parCALC & (length(chunkcalc) > 10))
        ret <- cdt.foreach(seq_along(chunkcalc), parsL, GUI = TRUE,
                           progress = TRUE, FUN = function(jj)
        {
            don.data <- readCdtDatasetChunk.sequence(chunkcalc[[jj]], GeneralParameters$cdtdataset$index, cdtParallelCond, do.par = do.parChunk)
            don.data <- don.data[don$dateInfo$index, , drop = FALSE]
            don.data <- don.data[iyear, , drop = FALSE]

            dat.moy <- readCdtDatasetChunk.sequence(chunkcalc[[jj]], index.file.moy, cdtParallelCond, do.par = do.parChunk)

            data.sds <- NULL
            if(anomaly.fonct == "Standardized")
                data.sds <- readCdtDatasetChunk.sequence(chunkcalc[[jj]], index.file.sds, cdtParallelCond, do.par = do.parChunk)
            anom <- .cdt.Anomalies(index1, don.data, dat.moy, data.sds, anomaly.fonct)

            if(GeneralParameters$outdir$update){
                dat.anom <- readCdtDatasetChunk.sequence(chunkcalc[[jj]], index.file.anomal, cdtParallelCond, do.par = do.parChunk)
                if(length(ixold) == 0){
                    anom <- rbind(dat.anom, anom)
                }else{
                    anom <- rbind(dat.anom[-ixold, , drop = FALSE], anom)
                }
                rm(dat.anom)
            }
            writeCdtDatasetChunk.sequence(anom, chunkcalc[[jj]], index.out, datadir3, cdtParallelCond, do.par = do.parChunk)

            rm(don.data, dat.moy, anom); gc()
        })

        ##########################################

        x <- index.out$coords$mat$x
        y <- index.out$coords$mat$y
        nx <- length(x)
        ny <- length(y)
        dx <- ncdim_def("Lon", "degreeE", x)
        dy <- ncdim_def("Lat", "degreeN", y)
        xy.dim <- list(dx, dy)
        if(anomaly.fonct == "Difference") units <- index.out$varInfo$units
        if(anomaly.fonct == "Percentage") units <- "percentage"
        if(anomaly.fonct == "Standardized") units <- ""
        nc.grd <- ncvar_def("anom", units, xy.dim, -9999, index.out$varInfo$longname, "float", compression = 9)

        ######################

        chunkfile <- sort(unique(don$colInfo$index))
        datyread <- split(daty, ceiling(seq_along(daty) / 50))

        do.parChunk <- if(length(chunkfile) > length(datyread)) TRUE else FALSE
        do.parCALC <- if(do.parChunk) FALSE else TRUE

        parsL <- doparallel.cond(do.parCALC & (length(datyread) > 50))
        ret <- cdt.foreach(seq_along(datyread), parsL, GUI = TRUE,
                           progress = TRUE, FUN = function(jj)
        {
            daty0 <- datyread[[jj]]
            dat.anom <- readCdtDatasetChunk.multi.dates.order(index.file.anomal, daty0, cdtParallelCond, do.par = do.parChunk)

            for(j in seq_along(daty0)){
                anom <- dat.anom[j, ]
                dim(anom) <- c(nx, ny)
                anom[is.na(anom) | is.nan(anom) | is.infinite(anom)] <- -9999

                filenc <- file.path(ncdfOUT3, paste0("anomaly_", daty0[j], ".nc"))
                nc <- ncdf4::nc_create(filenc, nc.grd)
                ncdf4::ncvar_put(nc, nc.grd, anom)
                ncdf4::nc_close(nc)
            }
            rm(daty0, dat.anom, anom); gc()
            return(0)
        })

        rm(don, index1, index.out)
    }

    return(0)
}
