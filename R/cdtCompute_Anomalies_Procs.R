
anomaliesCalcProcs <- function(GeneralParameters){
    GUI <- TRUE
    progress <- TRUE

    #############
    ## pour parallel
    GeneralParameters <- GeneralParameters
    cdtParallelCond <- .cdtData$Config[c('dopar', 'detect.cores', 'nb.cores')]

    intstep <- GeneralParameters$intstep
    outstep <- GeneralParameters$outstep

    #############

    if(GeneralParameters$outdir$update){
        don.anom <- try(readRDS(GeneralParameters$outdir$dir), silent = TRUE)
        if(inherits(don.anom, "try-error")){
            Insert.Messages.Out(paste(.cdtData$EnvData[['message']][['7']], GeneralParameters$outdir$dir), TRUE, 'e')
            return(NULL)
        }
        if(outstep != don.anom$params$outstep){
            Insert.Messages.Out(paste(.cdtData$EnvData[['message']][['8']], outstep), TRUE, 'e')
            return(NULL)
        }

        ## check seasonal outstep (start month and length)

        if(GeneralParameters$data.type != don.anom$params$data.type){
            Insert.Messages.Out(paste(.cdtData$EnvData[['message']][['8']], GeneralParameters$data.type), TRUE, 'e')
            return(NULL)
        }
    }else{
        if(!dir.exists(GeneralParameters$outdir$dir)){
            Insert.Messages.Out(paste(GeneralParameters$outdir$dir, .cdtData$EnvData[['message']][['6']]), TRUE, 'e')
            return(NULL)
        }

        if(GeneralParameters$climato$clim.exist){
            don.climato <- try(readRDS(GeneralParameters$climato$clim.file), silent = TRUE)
            if(inherits(don.climato, "try-error")){
                Insert.Messages.Out(paste(.cdtData$EnvData[['message']][['7']], GeneralParameters$climato$clim.file), TRUE, "e")
                return(NULL)
            }

            if(outstep != don.climato$params$outstep){
                Insert.Messages.Out(paste(.cdtData$EnvData[['message']][['9']], outstep), TRUE, 'e')
                return(NULL)
            }

            if(GeneralParameters$data.type != don.climato$params$data.type){
                Insert.Messages.Out(paste(.cdtData$EnvData[['message']][['9']], GeneralParameters$data.type), TRUE, 'e')
                return(NULL)
            }
        }else{
            allyears <- GeneralParameters$climato$all.years
            year1 <- GeneralParameters$climato$start.year
            year2 <- GeneralParameters$climato$end.year
            minyear <- GeneralParameters$climato$min.year
            xwin <- GeneralParameters$climato$window

            if(any(is.na(c(year1, year2, minyear)))){
                Insert.Messages.Out(.cdtData$EnvData[['message']][['10']], TRUE, 'e')
                return(NULL)
            }
        }
    }

    #####################################################

    if(GeneralParameters$data.type == "cdtstation"){
        don <- getStnOpenData(GeneralParameters$cdtstation$file)
        if(is.null(don)) return(NULL)
        don <- getCDTdataAndDisplayMsg(don, intstep, GeneralParameters$cdtstation$file)
        if(is.null(don)) return(NULL)
        miss.val <- getStnOpenDataInfo(GeneralParameters$cdtstation$file)[[3]]$miss.val

        daty <- don$dates
        # year <- as.numeric(substr(daty, 1, 4))
    }

    if(GeneralParameters$data.type == "cdtdataset"){
        don <- try(readRDS(GeneralParameters$cdtdataset$index), silent = TRUE)
        if(inherits(don, "try-error")){
            Insert.Messages.Out(paste(.cdtData$EnvData[['message']][['7']], GeneralParameters$cdtdataset$index), TRUE, 'e')
            return(NULL)
        }
        if(intstep != don$TimeStep){
            Insert.Messages.Out(paste(.cdtData$EnvData[['message']][['11']], intstep), TRUE, 'e')
            return(NULL)
        }

        daty <- don$dateInfo$date
        # year <- as.numeric(substr(daty, 1, 4))
    }

    #####################################################

    computeCLIMATO <- FALSE

    if(GeneralParameters$data.type == "cdtstation"){
        if(GeneralParameters$outdir$update){
            outDIR <- dirname(GeneralParameters$outdir$dir)

            if(length(don$id) != length(don.anom$data$id)){
                Insert.Messages.Out(.cdtData$EnvData[['message']][['13']], TRUE, 'e')
                return(NULL)
            }

            if(any(don$id != don.anom$data$id)){
                Insert.Messages.Out(.cdtData$EnvData[['message']][['14']], TRUE, 'e')
                return(NULL)
            }

            mean.file <- file.path(outDIR, 'CDTMEAN', 'CDTMEAN.rds')
            sds.file <- file.path(outDIR, 'CDTSTD', 'CDTSTD.rds')

            dat.moy <- try(readRDS(mean.file), silent = TRUE)
            if(inherits(dat.moy, "try-error")){
                Insert.Messages.Out(paste(.cdtData$EnvData[['message']][['7']], don.anom$mean.file), TRUE, 'e')
                return(NULL)
            }

            if(don.anom$params$anomaly == "Standardized"){
                dat.sds <- try(readRDS(sds.file), silent = TRUE)
                if(inherits(dat.sds, "try-error")){
                    Insert.Messages.Out(paste(.cdtData$EnvData[['message']][['7']], don.anom$sds.file), TRUE, 'e')
                    return(NULL)
                }
            }

            clim.index.file <- file.path(outDIR, "Climatology.rds")
            don.climato <- try(readRDS(clim.index.file), silent = TRUE)
            if(inherits(don.climato, "try-error")){
                Insert.Messages.Out(paste(.cdtData$EnvData[['message']][['7']], clim.index.file), TRUE, 'e')
                return(NULL)
            }

            index0 <- NULL
            index0$id <- don.climato$index
            anomaly.fonct <- don.anom$params$anomaly
            datadir <- file.path(outDIR, 'CDTSTATIONS')
        }else{
            outDIR <- file.path(GeneralParameters$outdir$dir, "ANOMALIES_data")
            dir.create(outDIR, showWarnings = FALSE, recursive = TRUE)

            if(GeneralParameters$climato$clim.exist){
                if(length(don$id) != length(don.climato$data$id)){
                    Insert.Messages.Out(.cdtData$EnvData[['message']][['15']], TRUE, 'e')
                    return(NULL)
                }
                
                if(any(don$id != don.climato$data$id)){
                    Insert.Messages.Out(.cdtData$EnvData[['message']][['16']], TRUE, 'e')
                    return(NULL)
                }

                dirClim <- dirname(GeneralParameters$climato$clim.file)
                mean.file <- file.path(dirClim, 'CDTMEAN', 'CDTMEAN.rds')
                sds.file <- file.path(dirClim, 'CDTSTD', 'CDTSTD.rds')

                dat.moy <- try(readRDS(mean.file), silent = TRUE)
                if(inherits(dat.moy, "try-error")){
                    Insert.Messages.Out(paste(.cdtData$EnvData[['message']][['7']], mean.file), TRUE, 'e')
                    return(NULL)
                }

                if(GeneralParameters$anomaly == "Standardized"){
                    dat.sds <- try(readRDS(sds.file), silent = TRUE)
                    if(inherits(dat.sds, "try-error")){
                        Insert.Messages.Out(paste(.cdtData$EnvData[['message']][['7']], sds.file), TRUE, 'e')
                        return(NULL)
                    }
                }

                file.copy(file.path(dirClim, 'CDTMEAN'), outDIR, recursive = TRUE, overwrite = TRUE)
                file.copy(file.path(dirClim, 'CDTSTD'), outDIR, recursive = TRUE, overwrite = TRUE)
                file.copy(GeneralParameters$climato$clim.file, outDIR, recursive = TRUE, overwrite = TRUE)

                index0 <- NULL
                index0$id <- don.climato$index
            }else computeCLIMATO <- TRUE

            anomaly.fonct <- GeneralParameters$anomaly
            datadir <- file.path(outDIR, 'CDTSTATIONS')
            dir.create(datadir, showWarnings = FALSE, recursive = TRUE)
        }
    }

    ##############################

    if(GeneralParameters$data.type == "cdtdataset"){
        if(GeneralParameters$outdir$update){
            outDIR <- dirname(GeneralParameters$outdir$dir)

            index.file.moy <- file.path(outDIR, 'CDTMEAN', 'CDTMEAN.rds')
            index.file.sds <- file.path(outDIR, 'CDTSTD', 'CDTSTD.rds')
            index.file.anomal <- file.path(outDIR, "CDTANOM", "CDTANOM.rds")

            index.out <- try(readRDS(index.file.anomal), silent = TRUE)
            if(inherits(index.out, "try-error")){
                Insert.Messages.Out(paste(.cdtData$EnvData[['message']][['7']], index.file.anomal), TRUE, 'e')
                return(NULL)
            }

            SP1 <- defSpatialPixels(list(lon = don$coords$mat$x, lat = don$coords$mat$y))
            SP2 <- defSpatialPixels(list(lon = index.out$coords$mat$x, lat = index.out$coords$mat$y))
            if(is.diffSpatialPixelsObj(SP1, SP2, tol = 1e-04)){
                Insert.Messages.Out(.cdtData$EnvData[['message']][['17']], TRUE, 'e')
                return(NULL)
            }
            rm(SP1, SP2)

            clim.index.file <- file.path(outDIR, "Climatology.rds")
            don.climato <- try(readRDS(clim.index.file), silent = TRUE)
            if(inherits(don.climato, "try-error")){
                Insert.Messages.Out(paste(.cdtData$EnvData[['message']][['7']], clim.index.file), TRUE, 'e')
                return(NULL)
            }

            index0 <- NULL
            index0$id <- don.climato$index
            rm(don.climato)
        }else{
            outDIR <- file.path(GeneralParameters$outdir$dir, "ANOMALIES_data")
            dir.create(outDIR, showWarnings = FALSE, recursive = TRUE)

            if(GeneralParameters$climato$clim.exist){
                dirClim <- dirname(GeneralParameters$climato$clim.file)
                index.file.moy <- file.path(dirClim, 'CDTMEAN', 'CDTMEAN.rds')
                index.file.sds <- file.path(dirClim, 'CDTSTD', 'CDTSTD.rds')

                index.data.climato <- try(readRDS(index.file.moy), silent = TRUE)
                if(inherits(index.data.climato, "try-error")){
                    Insert.Messages.Out(paste(.cdtData$EnvData[['message']][['7']], index.file.moy), TRUE, 'e')
                    return(NULL)
                }

                SP1 <- defSpatialPixels(list(lon = don$coords$mat$x, lat = don$coords$mat$y))
                SP2 <- defSpatialPixels(list(lon = index.data.climato$coords$mat$x, lat = index.data.climato$coords$mat$y))
                if(is.diffSpatialPixelsObj(SP1, SP2, tol = 1e-04)){
                    Insert.Messages.Out(.cdtData$EnvData[['message']][['18']], format = TRUE)
                    return(NULL)
                }
                rm(SP1, SP2)

                file.copy(file.path(dirClim, 'CDTMEAN'), outDIR, recursive = TRUE, overwrite = TRUE)
                file.copy(file.path(dirClim, 'CDTSTD'), outDIR, recursive = TRUE, overwrite = TRUE)
                file.copy(GeneralParameters$climato$clim.file, outDIR, recursive = TRUE, overwrite = TRUE)

                index0 <- NULL
                index0$id <- don.climato$index
                rm(index.data.climato)
            }else computeCLIMATO <- TRUE
        }
    }

    #####################################################

    # Aggrgate
    if(intstep != outstep){
        Insert.Messages.Out(.cdtData$EnvData[['message']][['19']], TRUE, "i")

        idatyAggr <- rep(TRUE, length(daty))
        if(GeneralParameters$outdir$update | GeneralParameters$climato$clim.exist){
            daty0 <- if(intstep == "monthly") paste0(daty, 1) else daty
            daty0 <- as.Date(daty0, "%Y%m%d")
            daty.range <- get.range.date.time(GeneralParameters$date.range, intstep)
            idatyAggr <- daty0 >= daty.range$start & daty0 <= daty.range$end
            daty <- daty[idatyAggr]
        }

        ########################

        startMonth <- GeneralParameters$seasonal$start.mon
        seasonLength <- GeneralParameters$seasonal$length.mon
        aggr.pars <- GeneralParameters$aggr.series

        agg.index <- cdt.index.aggregate(daty, intstep, outstep,
                                         seasonLength = seasonLength,
                                         startMonth = startMonth)

        if(aggr.pars$min.frac$unique){
            ifull <- (agg.index$nba / agg.index$nb0) >= aggr.pars$min.frac$all
        }else{
            ifull <- sapply(agg.index$nb.mon, function(x){
                all(x$nba / x$nb0 >= aggr.pars$min.frac$month[x$mo])
            })
        }

        if(all(!ifull)){
            Insert.Messages.Out(.cdtData$EnvData[['message']][['21']], TRUE, 'e')
            return(NULL)
        }

        daty <- agg.index$date

        #########################

        if(GeneralParameters$data.type == 'cdtstation'){
            don$data <- cdt.data.aggregateTS(don$data[idatyAggr, , drop = FALSE], agg.index, aggr.pars)
            don$dates <- daty
            don$data <- round(don$data, 5)

            out.cdt.aggregate <- file.path(datadir, paste0("Aggregated_data_", outstep, ".csv"))

            if(GeneralParameters$outdir$update & file.exists(out.cdt.aggregate)){
                don0 <- try(read.table(out.cdt.aggregate, sep = ',', na.strings = miss.val,
                            colClasses = 'character', stringsAsFactors = FALSE), silent = TRUE)
                if(inherits(don0, "try-error")){
                    Insert.Messages.Out(paste(.cdtData$EnvData[['message']][['7']], out.cdt.aggregate), TRUE, 'e')
                    return(NULL)
                }

                capition <- don0[1:3, 1]
                don0 <- splitCDTData1(don0)
                headers <- do.call(rbind, don0[c('id', 'lon', 'lat')])
                i0 <- don0$dates %in% daty
                if(any(i0)){
                    don0$dates <- don0$dates[!i0]
                    don0$data <- don0$data[!i0, , drop = FALSE]
                }

                daty <- c(don0$dates, daty)
                don$data <- rbind(don0$data, don$data)
                o0 <- order(daty)
                daty <- daty[o0]
                don$data <- don$data[o0, , drop = FALSE]
            }else{
                headers <- do.call(rbind, don[c('id', 'lon', 'lat')])
                capition <- c('Stations', 'LON', paste(toupper(outstep), 'LAT', sep = '/'))
            }

            entete <- cbind(capition, headers)
            cdtdata <- rbind(entete, cbind(daty, don$data))
            writeFiles(cdtdata, out.cdt.aggregate, na = miss.val)
            rm(cdtdata)
        }

        if(GeneralParameters$data.type == "cdtdataset"){
            dataset.name <- paste0("Aggregated_Data_", outstep)
            outputDIR <- file.path(outDIR, dataset.name)
            dataDIR <- file.path(outputDIR, "DATA")
            dir.create(dataDIR, showWarnings = FALSE, recursive = TRUE)
            file.index <- file.path(outputDIR, paste0(dataset.name, ".rds"))

            ##########

            index.agg <- don
            index.agg$TimeStep <- outstep

            ##########

            readPrevAggr <- FALSE
            if(GeneralParameters$outdir$update & file.exists(file.index)){
                don0 <- try(readRDS(file.index), silent = TRUE)
                if(inherits(don0, "try-error")){
                    Insert.Messages.Out(paste(.cdtData$EnvData[['message']][['7']], file.index), TRUE, 'e')
                    return(NULL)
                }

                don0Index <- don0$dateInfo$index
                i0 <- don0$dateInfo$date %in% daty
                if(any(i0)){
                    don0$dateInfo$date <- don0$dateInfo$date[!i0]
                    don0$dateInfo$index <- don0$dateInfo$index[!i0]
                    ## order: remove missing index, if exist
                    don0$dateInfo$index <- order(don0$dateInfo$index)
                }

                index.agg$dateInfo$date <- c(don0$dateInfo$date, daty)
                index.agg$dateInfo$index <- c(don0$dateInfo$index, max(don0$dateInfo$index) + seq_along(daty))

                o0 <- order(index.agg$dateInfo$date)
                index.agg$dateInfo$date <- index.agg$dateInfo$date[o0]
                index.agg$dateInfo$index <- index.agg$dateInfo$index[o0]
                daty <- index.agg$dateInfo$date

                readPrevAggr <- TRUE
            }else{
                index.agg$dateInfo$date <- daty
                index.agg$dateInfo$index <- seq_along(daty)
            }

            ##########

            con <- gzfile(file.index, compression = 7)
            open(con, "wb")
            saveRDS(index.agg, con)
            close(con)

            ##########

            chunkfile <- sort(unique(don$colInfo$index))
            chunkcalc <- split(chunkfile, ceiling(chunkfile / don$chunkfac))
     
            ##########
            do.parChunk <- if(don$chunkfac > length(chunkcalc)) TRUE else FALSE
            do.parCALC <- if(do.parChunk) FALSE else TRUE
            parsL <- doparallel.cond(do.parCALC & (length(chunkcalc) > 10))

            ret <- cdt.foreach(seq_along(chunkcalc), parsL, GUI, progress, FUN = function(jj)
            {
                don.data <- readCdtDatasetChunk.sequence(chunkcalc[[jj]], GeneralParameters$cdtdataset$index, cdtParallelCond, do.par = do.parChunk)
                don.data <- don.data[don$dateInfo$index, , drop = FALSE]
                don.data <- don.data[idatyAggr, , drop = FALSE]
                cdtdata <- cdt.data.aggregateTS(don.data, agg.index, aggr.pars)

                if(readPrevAggr){
                    don.data0 <- readCdtDatasetChunk.sequence(chunkcalc[[jj]], file.index, cdtParallelCond, do.par = do.parChunk)
                    don.data0 <- don.data0[don0Index, , drop = FALSE]
                    don.data0 <- don.data0[!i0, , drop = FALSE]
                    cdtdata <- rbind(don.data0, cdtdata)
                    cdtdata <- cdtdata[o0, , drop = FALSE]
                }

                writeCdtDatasetChunk.sequence(cdtdata, chunkcalc[[jj]], index.agg, dataDIR, cdtParallelCond, do.par = do.parChunk)
                rm(don.data, cdtdata); gc()
                return(0)
            })

            don <- index.agg
            GeneralParameters$cdtdataset$index <- file.index
        }
 
        # intstep <- outstep
        Insert.Messages.Out(.cdtData$EnvData[['message']][['20']], TRUE, "s")
    }

    #####################################################

    # climatology
    if(computeCLIMATO){
        Insert.Messages.Out(.cdtData$EnvData[['message']][['22']], TRUE, "i")

        daty0 <- daty
        year <- as.numeric(substr(daty0, 1, 4))
        iyear <- if(allyears) rep(TRUE, length(year)) else year >= year1 & year <= year2
        daty0 <- daty0[iyear]
        year <- year[iyear]

        if(length(unique(year)) < minyear){
            Insert.Messages.Out(.cdtData$EnvData[['message']][['23']], TRUE, 'e')
            return(NULL)
        }
        GeneralParameters$climato$start.year <- min(year)
        GeneralParameters$climato$end.year <- max(year)

        index <- cdt.index.Climatologies(daty0, outstep, xwin)

        index0 <- NULL
        index0$id <- index$id

        #####################################################

        if(GeneralParameters$data.type == "cdtstation"){
            don$data <- don$data[iyear, , drop = FALSE]
            dat.clim <- .cdt.Climatologies(index, don$data, minyear, outstep, xwin)
            dat.moy <- round(dat.clim$mean, 2)
            dat.sds <- round(dat.clim$sd, 2)

            rm(dat.clim)

            #########################################

            out.cdt.clim.moy <- file.path(datadir, paste0(outstep, "_Climatology_mean.csv"))
            out.cdt.clim.sds <- file.path(datadir, paste0(outstep, "_Climatology_std.csv"))

            dataOUT1 <- file.path(outDIR, 'CDTMEAN')
            dir.create(dataOUT1, showWarnings = FALSE, recursive = TRUE)
            index.file.moy <- file.path(dataOUT1, 'CDTMEAN.rds')

            dataOUT2 <- file.path(outDIR, 'CDTSTD')
            dir.create(dataOUT2, showWarnings = FALSE, recursive = TRUE)
            index.file.sds <- file.path(dataOUT2, 'CDTSTD.rds')

            file.index.clim <- file.path(outDIR, "Climatology.rds")

            ##################

            output <- list(params = GeneralParameters,
                           data = list(id = don$id, lon = don$lon, lat = don$lat),
                           index = index$id)

            out.dat.index <- gzfile(file.index.clim, compression = 7)
            saveRDS(output, out.dat.index)
            close(out.dat.index)

            out.cdt.moy <- gzfile(index.file.moy, compression = 7)
            saveRDS(dat.moy, out.cdt.moy)
            close(out.cdt.moy)

            out.cdt.sds <- gzfile(index.file.sds, compression = 7)
            saveRDS(dat.sds, out.cdt.sds)
            close(out.cdt.sds)

            ##################

            xhead <- rbind(don$id, don$lon, don$lat)
            chead <- c('ID.STN', 'LON', 'INDEX/LAT')
            infohead <- cbind(chead, xhead)

            dat.moy1 <- rbind(infohead, cbind(index$id, dat.moy))
            writeFiles(dat.moy1, out.cdt.clim.moy, na = '-99')

            dat.sds1 <- rbind(infohead, cbind(index$id, dat.sds))
            writeFiles(dat.sds1, out.cdt.clim.sds, na = '-99')

            Insert.Messages.Out(.cdtData$EnvData[['message']][['24']], TRUE, "s")
            rm(dat.moy1, dat.sds1)
        }

        #####################################################

        if(GeneralParameters$data.type == "cdtdataset"){
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

            file.index.clim <- file.path(outDIR, "Climatology.rds")

            ##################

            output <- list(params = GeneralParameters, index = index$id)

            out.dat.index <- gzfile(file.index.clim, compression = 7)
            saveRDS(output, out.dat.index)
            close(out.dat.index)

            ##################

            index.out <- don
            index.out$varInfo$longname <- paste(outstep, "climatology from:", don$varInfo$longname)
            index.out$dateInfo$date <- index$id
            index.out$dateInfo$index <- seq_along(index$id)

            file.index.mean <- gzfile(index.file.moy, compression = 7)
            saveRDS(index.out, file.index.mean)
            close(file.index.mean)

            file.index.sds <- gzfile(index.file.sds, compression = 7)
            saveRDS(index.out, file.index.sds)
            close(file.index.sds)

            #########################################

            chunkfile <- sort(unique(don$colInfo$index))
            chunkcalc <- split(chunkfile, ceiling(chunkfile / don$chunkfac))

            do.parChunk <- if(don$chunkfac > length(chunkcalc)) TRUE else FALSE
            do.parCALC <- if(do.parChunk) FALSE else TRUE
            parsL <- doparallel.cond(do.parCALC & (length(chunkcalc) > 10))
            ret <- cdt.foreach(seq_along(chunkcalc), parsL, GUI = GUI,
                               progress = progress, FUN = function(jj)
            {
                don.data <- readCdtDatasetChunk.sequence(chunkcalc[[jj]], GeneralParameters$cdtdataset$index, cdtParallelCond, do.par = do.parChunk)
                don.data <- don.data[don$dateInfo$index, , drop = FALSE]
                don.data <- don.data[iyear, , drop = FALSE]

                dat.clim <- .cdt.Climatologies(index, don.data, minyear, outstep, xwin)

                writeCdtDatasetChunk.sequence(dat.clim$mean, chunkcalc[[jj]], index.out, datadir1, cdtParallelCond, do.par = do.parChunk)
                writeCdtDatasetChunk.sequence(dat.clim$sd, chunkcalc[[jj]], index.out, datadir2, cdtParallelCond, do.par = do.parChunk)
                rm(dat.clim, don.data); gc()
            })

            Insert.Messages.Out(.cdtData$EnvData[['message']][['24']], TRUE, "s")

            ##########################################

            Insert.Messages.Out(.cdtData$EnvData[['message']][['25']], TRUE, "i")

            x <- index.out$coords$mat$x
            y <- index.out$coords$mat$y
            dx <- ncdim_def("Lon", "degreeE", x)
            dy <- ncdim_def("Lat", "degreeN", y)
            xy.dim <- list(dx, dy)
            nc.grd <- ncvar_def(index.out$varInfo$name, index.out$varInfo$units, xy.dim, -99, index.out$varInfo$longname, "float", compression = 9)

            ######################

            ret <- cdt.foreach(seq_along(index$id), doparallel.cond(FALSE),
                               GUI = GUI, progress = progress, FUN = function(jj)
            {
                id <- index$id[jj]
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
            Insert.Messages.Out(.cdtData$EnvData[['message']][['26']], TRUE, "s")

            rm(index, index.out)
        }
    }

    #####################################################

    # anomaly
    Insert.Messages.Out(.cdtData$EnvData[['message']][['2']], TRUE, "i")

    date.range <- GeneralParameters$date.range
    if(outstep == "annual"){
        iyearAnom <- daty >= date.range$start.year & daty <= date.range$end.year
        datyAnom <- daty[iyearAnom]
    }else if(outstep == "seasonal"){
        daty.range <- get.range.date.time(date.range, 'monthly')
        daty0 <- do.call(c, lapply(strsplit(daty, "_"), '[[', 1))
        daty0 <- as.Date(paste0(daty0, '-1'))
        iyearAnom <- daty0 >= daty.range$start & daty0 <= daty.range$end
        datyAnom <- daty[iyearAnom]
    }else{
        daty0 <- if(outstep == "monthly") paste0(daty, 1) else daty
        daty0 <- as.Date(daty0, "%Y%m%d")
        daty.range <- get.range.date.time(date.range, outstep)
        iyearAnom <- daty0 >= daty.range$start & daty0 <= daty.range$end
        datyAnom <- daty[iyearAnom]
    }

    index1 <- cdt.index.Anomalies(datyAnom, index0, outstep)
    daty <- index1$date
    index1 <- index1$index

    #####################################################

    if(GeneralParameters$data.type == "cdtstation"){
        don$data <- don$data[iyearAnom, , drop = FALSE]

        data.sds <- if(anomaly.fonct == "Standardized") dat.sds else NULL
        anom <- .cdt.Anomalies(index1, don$data, dat.moy, data.sds, anomaly.fonct)

        #########################################

        if(GeneralParameters$outdir$update){
            anom.file.rds <- file.path(outDIR, 'CDTANOM', 'CDTANOM.rds')
            anom.file.csv <- file.path(outDIR, 'CDTSTATIONS', paste0(outstep, "_Anomaly.csv"))

            data.anom.rds <- readRDS(anom.file.rds)
            data.anom.csv <- read.table(anom.file.csv, sep = ",", colClasses = "character", stringsAsFactors = FALSE)
            infohead <- data.anom.csv[1:3, , drop = FALSE]
            data.anom.csv <- data.anom.csv[-(1:3), -1]

            ixold <- match(daty, don.anom$data$dates)
            ixold <- ixold[!is.na(ixold)]

            if(length(ixold) == 0){
                don.anom$data$dates <- c(don.anom$data$dates, daty)
                data.anom.rds <- rbind(data.anom.rds, anom)
                anom <- as.data.frame(anom)
                names(anom) <- names(data.anom.csv)
                data.anom.csv <- rbind(data.anom.csv, anom)
            }else{
                don.anom$data$dates <- c(don.anom$data$dates[-ixold], daty)
                data.anom.rds <- rbind(data.anom.rds[-ixold, , drop = FALSE], anom)
                anom <- as.data.frame(anom)
                names(anom) <- names(data.anom.csv)
                data.anom.csv <- rbind(data.anom.csv[-ixold, , drop = FALSE], anom)
            }

            saveRDS(data.anom.rds, anom.file.rds)

            tmp <- data.frame(cbind(don.anom$data$dates, data.anom.csv))
            names(tmp) <- names(infohead)
            data.anom.csv <- rbind(infohead, tmp)
            writeFiles(data.anom.csv, anom.file.csv, na = .cdtData$Config$missval.anom)

            saveRDS(don.anom, GeneralParameters$outdir$dir)
            .cdtData$EnvData$output <- don.anom
            .cdtData$EnvData$PathAnom <- outDIR

            rm(tmp, don, anom, data.anom.csv, data.anom.rds, don.anom)
        }else{
            out.dat.index <- gzfile(file.path(outDIR, "Anomaly.rds"), compression = 7)
            out.cdt.anomal <- file.path(datadir, paste0(outstep, "_Anomaly.csv"))

            dataOUT3 <- file.path(outDIR, 'CDTANOM')
            dir.create(dataOUT3, showWarnings = FALSE, recursive = TRUE)
            out.cdt.anom <- gzfile(file.path(dataOUT3, 'CDTANOM.rds'), compression = 7)

            #############
            saveRDS(anom, out.cdt.anom)
            close(out.cdt.anom)

            #############
            xhead <- rbind(don$id, don$lon, don$lat)
            chead <- c('ID.STN', 'LON', paste0(toupper(outstep), "/LAT"))
            infohead <- cbind(chead, xhead)

            #############
            anom <- round(anom, 2)
            anom <- rbind(infohead, cbind(daty, anom))
            writeFiles(anom, out.cdt.anomal, na = .cdtData$Config$missval.anom)

            #############
            output <- list(params = GeneralParameters,
                           data = don[c('id', 'lon', 'lat', 'dates')])

            .cdtData$EnvData$output <- output
            .cdtData$EnvData$PathAnom <- outDIR

            saveRDS(output, out.dat.index)
            close(out.dat.index)

            rm(output, don, anom)
        }

        Insert.Messages.Out(.cdtData$EnvData[['message']][['2a']], TRUE, "s")
    }

    #####################################################

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
            index.out$varInfo$longname <- paste(outstep, "anomaly from:", don$varInfo$longname)
            index.out$dateInfo$date <- daty
            index.out$dateInfo$index <- seq_along(daty)

            ##########
            saveRDS(index.out, index.file.anomal)

            output <- list(params = GeneralParameters, dates = daty)
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
        ret <- cdt.foreach(seq_along(chunkcalc), parsL, GUI = GUI,
                           progress = progress, FUN = function(jj)
        {
            don.data <- readCdtDatasetChunk.sequence(chunkcalc[[jj]], GeneralParameters$cdtdataset$index, cdtParallelCond, do.par = do.parChunk)
            don.data <- don.data[don$dateInfo$index, , drop = FALSE]
            don.data <- don.data[iyearAnom, , drop = FALSE]

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

        Insert.Messages.Out(.cdtData$EnvData[['message']][['2a']], TRUE, "s")

        #########################################

        Insert.Messages.Out(.cdtData$EnvData[['message']][['25']], TRUE, "i")

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

        #########################################

        chunkfile <- sort(unique(don$colInfo$index))
        datyread <- split(daty, ceiling(seq_along(daty) / 50))

        do.parChunk <- if(length(chunkfile) > length(datyread)) TRUE else FALSE
        do.parCALC <- if(do.parChunk) FALSE else TRUE

        parsL <- doparallel.cond(do.parCALC & (length(datyread) > 50))
        ret <- cdt.foreach(seq_along(datyread), parsL, GUI = GUI,
                           progress = progress, FUN = function(jj)
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

        Insert.Messages.Out(.cdtData$EnvData[['message']][['26']], TRUE, "s")

        rm(don, index1, index.out)
    }

    return(0)
}
