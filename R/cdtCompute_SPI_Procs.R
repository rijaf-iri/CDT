
computeSPIProcs <- function(GeneralParameters){
    message <- .cdtData$EnvData$message
    freqData <- GeneralParameters$intstep
    input.file <- if(GeneralParameters$data.type == 'cdtstation') GeneralParameters$cdtstation else GeneralParameters$cdtdataset

    cdtParallelCond <- .cdtData$Config[c('dopar', 'detect.cores', 'nb.cores')]

    if(input.file %in% c("", "NA")){
        Insert.Messages.Out(message[['12']], TRUE, 'e')
        return(NULL)
    }

    if(GeneralParameters$monitoring){
        outDIR <- dirname(GeneralParameters$outdir)
        if(!dir.exists(outDIR) | !file.exists(GeneralParameters$outdir)){
            Insert.Messages.Out(message[['13']], TRUE, 'e')
            return(NULL)
        }
        dataCDTdir <- file.path(outDIR, 'CDTDATASET')
        dataPARSdir <- file.path(outDIR, 'PARAMS')

        out.spi.index <- GeneralParameters$outdir
    }else{
        if(!dir.exists(GeneralParameters$outdir)){
            Insert.Messages.Out(message[['14']], TRUE, 'e')
            Insert.Messages.Out(paste(message[['15']], getwd()))
            GeneralParameters$outdir <- getwd()
        }
        outDIR <- file.path(GeneralParameters$outdir, "SPI_data")
        dir.create(outDIR, showWarnings = FALSE, recursive = TRUE)
        dataCDTdir <- file.path(outDIR, 'CDTDATASET')
        dir.create(dataCDTdir, showWarnings = FALSE, recursive = TRUE)
        dataPARSdir <- file.path(outDIR, 'PARAMS')
        dir.create(dataPARSdir, showWarnings = FALSE, recursive = TRUE)

        out.spi.index <- file.path(outDIR, "SPI.rds")
    }

    ##########

    if(GeneralParameters$outfreq == "dekad") outstep <- "dekadal"
    if(GeneralParameters$outfreq == "month") outstep <- "monthly"

    ##########
    dataOUT <- file.path(outDIR, paste0(toupper(outstep), "_data"))
    if(GeneralParameters$data.type == "cdtdataset") datadir <- file.path(dataOUT, 'DATA')
    file.aggr <- file.path(dataOUT, paste0(toupper(outstep), "_data.rds"))

    if(GeneralParameters$monitoring){
        if(!file.exists(file.aggr)){
            Insert.Messages.Out(paste(file.aggr, message[['6']]), TRUE, 'e')
            return(NULL)
        }
    }else{
        dir.create(dataOUT, showWarnings = FALSE, recursive = TRUE)
        if(GeneralParameters$data.type == "cdtdataset")
            dir.create(datadir, showWarnings = FALSE, recursive = TRUE)
    }

    #####################
    if(GeneralParameters$data.type == "cdtstation"){
        don <- getStnOpenData(GeneralParameters$cdtstation)
        if(is.null(don)) return(NULL)
        don <- getCDTdataAndDisplayMsg(don, GeneralParameters$intstep, GeneralParameters$cdtstation)
        if(is.null(don)) return(NULL)

        daty <- don$dates

        if(GeneralParameters$monitoring){
            SP1 <- list(id = don$id, lon = don$lon, lat = don$lat)
            SP2 <- readRDS(out.spi.index)
            SP2 <- SP2$data
            if(!isTRUE(all.equal(SP1, SP2))){
                Insert.Messages.Out(message[['16']], TRUE, 'e')
                return(NULL)
            }
            rm(SP1, SP2)
        }
    }

    if(GeneralParameters$data.type == "cdtdataset"){
        don <- try(readRDS(GeneralParameters$cdtdataset), silent = TRUE)
        if(inherits(don, "try-error")){
            Insert.Messages.Out(paste(message[['17']], GeneralParameters$cdtdataset), TRUE, 'e')
            return(NULL)
        }

        daty <- don$dateInfo$date

        if(GeneralParameters$monitoring){
            SP1 <- defSpatialPixels(list(lon = don$coords$mat$x, lat = don$coords$mat$y))
            SP2 <- readRDS(out.spi.index)
            SP2 <- defSpatialPixels(list(lon = SP2$data$x, lat = SP2$data$y))
            if(is.diffSpatialPixelsObj(SP1, SP2, tol = 1e-04)){
                Insert.Messages.Out(message[['18']], TRUE, 'e')
                return(NULL)
            }
            rm(SP1, SP2)
        }
    }

    #####################

    spi.frequency <- if(GeneralParameters$outfreq == "dekad") 36 else 12
    spi.tscale <- GeneralParameters$tscale
    spi.distribution <- GeneralParameters$distr
    spi.out.suffix <- if(GeneralParameters$outfreq == "dekad") "1dek" else paste0(GeneralParameters$tscale, "mon")

    #####################

    idaty0 <- rep(TRUE, length(daty))
    if(GeneralParameters$monitoring){
        daty.mon <- GeneralParameters$dates
        if(GeneralParameters$outfreq == "dekad"){
            dek1 <- daty.mon$dek1
            dek2 <- daty.mon$dek2
            if(freqData == "daily"){
                dek1 <- c(1, 11, 21)[dek1]
                dek2 <- c(10, 20, Day.Of.Month(daty.mon$year2, daty.mon$mon2))[dek2]
            }
            if(freqData == "pentad"){
                dek1 <- c(1, 3, 5)[dek1]
                dek2 <- c(2, 4, 6)[dek2]
            }
            start.moni <- as.Date(paste(daty.mon$year1, daty.mon$mon1, dek1, sep = '-'))
            end.moni <- as.Date(paste(daty.mon$year2, daty.mon$mon2, dek2, sep = '-'))
        }

        if(GeneralParameters$outfreq == "month"){
            start.moni <- as.Date(paste(daty.mon$year1, daty.mon$mon1, 1, sep = '-'))
            if(spi.tscale > 1) start.moni <- addMonths(start.moni, -(spi.tscale - 1))
            if(freqData == "daily") fin <- Day.Of.Month(daty.mon$year2, daty.mon$mon2)
            if(freqData == "pentad") fin <- 6
            if(freqData == "dekadal") fin <- 3
            if(freqData == "monthly") fin <- 1
            end.moni <- as.Date(paste(daty.mon$year2, daty.mon$mon2, fin, sep = '-'))
        }

        daty0 <- if(freqData == "monthly") paste0(daty, "01") else daty
        daty0 <- as.Date(daty0, "%Y%m%d")
        idaty0 <- daty0 >= start.moni & daty0 <= end.moni
        if(!any(idaty0)){
            Insert.Messages.Out(message[['19']], TRUE, 'e')
            return(NULL)
        }
        daty <- daty[idaty0]
    }

    #####################
    GeneralParameters <- GeneralParameters

    toAggr <- list(input.file, freqData, GeneralParameters$outfreq, idaty0)

    aggregatData <- FALSE
    if((GeneralParameters$outfreq == "dekad" & freqData != 'dekadal') |
        (GeneralParameters$outfreq == "month" & freqData != 'monthly'))
    {
        if(is.null(.cdtData$EnvData$toAggr)){
            aggregatData <- TRUE
        }else{
            aggregatData <- if(!isTRUE(all.equal(.cdtData$EnvData$toAggr, toAggr))) TRUE else FALSE
        }
    }

    if(aggregatData){
        txtAggr <- if(GeneralParameters$outfreq == "dekad") "dekadal" else "monthly"
        Insert.Messages.Out(paste(message[['20']], txtAggr, "......"), TRUE, "i")

        outfreq <- switch(GeneralParameters$outfreq, "dekad" = "dekadal", "month" = "monthly")
        agg.index <- cdt.index.aggregate(daty, freqData, outfreq)

        ifull <- (agg.index$nba / agg.index$nb0) >= 0.95
        outdates <- agg.index$date
        index <- agg.index$index

        ##########
        if(GeneralParameters$data.type == "cdtstation"){
            don <- drought.indices.aggr.stn(don, idaty0, agg.index, ifull, file.aggr, GeneralParameters$monitoring)

            out.file.gz <- gzfile(file.aggr, compression = 7)
            saveRDS(don, out.file.gz)
            close(out.file.gz)
        }

        if(GeneralParameters$data.type == "cdtdataset"){
            index.out0 <- drought.indices.index.cdt(don, outstep, outdates, file.aggr, GeneralParameters$monitoring)

            out.file.gz <- gzfile(file.aggr, compression = 7)
            saveRDS(index.out0$index, out.file.gz)
            close(out.file.gz)

            ##########
            chunkfile <- sort(unique(don$colInfo$index))
            chunkcalc <- split(chunkfile, ceiling(chunkfile / don$chunkfac))

            do.parChunk <- if(don$chunkfac > length(chunkcalc)) TRUE else FALSE
            do.parCALC <- if(do.parChunk) FALSE else TRUE

            parsL <- doparallel.cond(do.parCALC & (length(chunkcalc) > 10))
            ret <- cdt.foreach(seq_along(chunkcalc), parsL, GUI = TRUE,
                               progress = TRUE, FUN = function(chkj)
            {
                drought.indices.aggr.cdt(don, idaty0, agg.index$index, ifull, file.aggr,
                                         GeneralParameters$monitoring, GeneralParameters$cdtdataset,
                                         index.out0, datadir, chunkcalc[[chkj]],
                                         do.parChunk, cdtParallelCond)
            })

            don <- index.out0$index
            rm(index.out0)
        }

        .cdtData$EnvData$toAggr <- toAggr
        Insert.Messages.Out(paste(txtAggr, message[['21']]), TRUE, "s")
        Insert.Messages.Out(message[['22']], TRUE, "i")
    }else{
        if((GeneralParameters$outfreq == "dekad" & freqData != 'dekadal') |
            (GeneralParameters$outfreq == "month" & freqData != 'monthly'))
        {
            don <- readRDS(file.aggr)
        }else{
            if(GeneralParameters$data.type == "cdtstation"){
                if(GeneralParameters$monitoring)
                    don <- drought.indices.update.stn(don, idaty0, file.aggr)

                out.file.gz <- gzfile(file.aggr, compression = 7)
                saveRDS(don, out.file.gz)
                close(out.file.gz)
            }

            if(GeneralParameters$data.type == "cdtdataset"){
                index.out <- don

                if(GeneralParameters$monitoring){
                    index.out0 <- drought.indices.update.index.cdt(index.out, idaty0, file.aggr)

                    ##########
                    chunkfile <- sort(unique(don$colInfo$index))
                    chunkcalc <- split(chunkfile, ceiling(chunkfile / don$chunkfac))

                    do.parChunk <- if(don$chunkfac > length(chunkcalc)) TRUE else FALSE
                    do.parCALC <- if(do.parChunk) FALSE else TRUE

                    parsL <- doparallel.cond(do.parCALC & (length(chunkcalc) > 10))
                    ret <- cdt.foreach(seq_along(chunkcalc), parsL, GUI = TRUE,
                                       progress = TRUE, FUN = function(chkj)
                    {
                        drought.indices.update.cdt(don, idaty0, file.aggr,
                                                   GeneralParameters$cdtdataset,
                                                   index.out0, datadir, chunkcalc[[chkj]],
                                                   do.parChunk, cdtParallelCond)
                    })

                    index.out0 <- index.out0$index
                }else{
                    file.copy(file.path(dirname(GeneralParameters$cdtdataset), "DATA"),
                                dataOUT, overwrite = TRUE, recursive = TRUE)
                    index.out0 <- index.out
                }

                don <- index.out0

                out.file.gz <- gzfile(file.aggr, compression = 7)
                saveRDS(index.out0, out.file.gz)
                close(out.file.gz)

                rm(index.out0, index.out)
            }
        }
    }

    #####################

    if(GeneralParameters$data.type == "cdtstation") dtemps <- don$dates
    if(GeneralParameters$data.type == "cdtdataset") dtemps <- don$dateInfo$date

    idaty <- rep(TRUE, length(dtemps))
    if(GeneralParameters$monitoring){
        daty.mon <- GeneralParameters$dates
        if(GeneralParameters$outfreq == "dekad"){
            start.moni <- as.Date(paste(daty.mon$year1, daty.mon$mon1, daty.mon$dek1, sep = '-'))
            end.moni <- as.Date(paste(daty.mon$year2, daty.mon$mon2, daty.mon$dek2, sep = '-'))
            idaty <- seq(start.moni, end.moni, 'day')
            dek <- as.numeric(format(idaty, "%d"))
            idaty <- paste0(format(idaty, "%Y%m"), dek)[dek <= 3]
        }
        if(GeneralParameters$outfreq == "month"){
            start.moni <- as.Date(paste(daty.mon$year1, daty.mon$mon1, 1, sep = '-'))
            if(spi.tscale > 1) start.moni <- addMonths(start.moni, -(spi.tscale - 1))
            end.moni <- as.Date(paste(daty.mon$year2, daty.mon$mon2, 1, sep = '-'))
            idaty <- format(seq(start.moni, end.moni, 'month'), "%Y%m")
        }
        idaty <- dtemps %in% idaty
    }

    dtemps <- dtemps[idaty]
    nl <- length(dtemps)
    temps <- sapply(1:spi.frequency, function(k){
        if(k + spi.tscale - 1 > nl) return(NA)
        iseq <- seq(k + spi.tscale - 1, nl, spi.frequency)
        dates <- dtemps[iseq][1]
        if(GeneralParameters$outfreq == "month") out <- as.numeric(substr(dates, 5, 6))
        if(GeneralParameters$outfreq == "dekad"){
            dek <- expand.grid(1:3, str_pad(1:12, 2, pad = "0"))
            out <- which(paste0(dek[, 2], dek[, 1]) == substr(dates, 5, 7))
        }
        return(out)
    })
    temps <- temps[!is.na(temps)]
    spi.frequency <- length(temps)

    ###########
    idt <- seq_along(dtemps)
    idt <- if(spi.tscale > 1) idt[-(1:(spi.tscale - 1))] else idt
    daty <- dtemps[idt]

    #####################
    if(GeneralParameters$data.type == "cdtstation"){
        dataSTNdir <- file.path(outDIR, 'CDTSTATIONS')
        file.SPI.csv <- file.path(dataSTNdir, paste0("SPI_", spi.out.suffix, ".csv"))
        file.SPI.rds <- file.path(dataCDTdir, paste0("SPI_", spi.out.suffix, ".rds"))

        data.mat <- SPEI_Aggregate_data(don$data[idaty, , drop = FALSE], spi.tscale)

        ###########
        if(GeneralParameters$monitoring){
            file.PARS.rds <- file.path(dataPARSdir, paste0("SPI_", spi.out.suffix, ".rds"))
            if(!file.exists(file.PARS.rds)){
                Insert.Messages.Out(paste(message[['23']], ':', file.PARS.rds), TRUE, 'e')
                return(NULL)
            }
            spi.params <- readRDS(file.PARS.rds)
            spi.distribution <- spi.params$distr
            params <- spi.params$pars
            params <- params[match(temps, spi.params$index), , drop = FALSE]
        }else{
            dir.create(dataSTNdir, showWarnings = FALSE, recursive = TRUE)
            file.PARS.rds <- file.path(dataPARSdir, paste0("SPI_", spi.out.suffix, ".rds"))
            params <- SPEI_Compute_params(data.mat, spi.tscale, spi.frequency, spi.distribution)
            spi.params <- list(index = temps, pars = params, distr = spi.distribution)

            out.file.gz <- gzfile(file.PARS.rds, compression = 7)
            saveRDS(spi.params, out.file.gz)
            close(out.file.gz)
        }

        ###########

        data.spi <- SPEI_computation(data.mat, params, spi.tscale, spi.frequency, spi.distribution)

        ###########
        if(GeneralParameters$monitoring){
            data.spi0 <- readRDS(file.SPI.rds)
            out.cdt.spi <- list()
            data.spi <- data.spi[idt, , drop = FALSE]

            ix <- match(daty, data.spi0$date)
            ix <- ix[!is.na(ix)]
            iy <- match(data.spi0$date, daty)
            iy <- iy[!is.na(iy)]
            if(length(ix) == 0){
                out.cdt.spi$date <- c(data.spi0$date, daty)
                out.cdt.spi$spi <- rbind(data.spi0$spi, data.spi)
            }else{
                out.cdt.spi$date <- c(data.spi0$date, daty[-iy])
                data.spi0$spi[ix, ] <- data.spi[iy, , drop = FALSE]
                out.cdt.spi$spi <- rbind(data.spi0$spi, data.spi[-iy, , drop = FALSE])
            }

            rm(data.spi0)
        }else out.cdt.spi <- list(date = don$dates, spi = data.spi)

        out.file.gz <- gzfile(file.SPI.rds, compression = 7)
        saveRDS(out.cdt.spi, out.file.gz)
        close(out.file.gz)

        ###########
        xhead <- rbind(don$id, don$lon, don$lat)
        chead <- c('ID.STN', 'LON', 'DATE/LAT')
        infohead <- cbind(chead, xhead)

        xdata <- do.call(cbind, out.cdt.spi)
        if(spi.tscale > 1) xdata <- xdata[-(1:(spi.tscale - 1)), ]
        data.spi <- rbind(infohead, xdata)
        data.spi[is.na(data.spi)] <- .cdtData$Config$missval.anom
        writeFiles(data.spi, file.SPI.csv)

        output <- list(params = GeneralParameters,
                       data = list(id = don$id, lon = don$lon, lat = don$lat))
        rm(don, data.spi, out.cdt.spi, xdata)
    }

    if(GeneralParameters$data.type == "cdtdataset"){
        dataNCdir <- file.path(outDIR, 'DATA_NetCDF', paste0("SPI_", spi.out.suffix))
        dataSPIdir <- file.path(dataCDTdir, paste0("SPI_", spi.out.suffix), "DATA")
        file.spi.index <- file.path(dirname(dataSPIdir), paste0("SPI_", spi.out.suffix, ".rds"))
        dataPARAMdir <- file.path(dataPARSdir, paste0("SPI_", spi.out.suffix), "DATA")
        file.PARS.index <- file.path(dirname(dataPARAMdir), paste0("SPI_", spi.out.suffix, ".rds"))

        #########################################

        if(GeneralParameters$monitoring){
            index.spi <- readRDS(file.spi.index)
            ix <- match(daty, index.spi$dateInfo$date)
            ix <- ix[!is.na(ix)]
            iy <- match(index.spi$dateInfo$date, daty)
            iy <- iy[!is.na(iy)]

            if(length(ix) == 0){
                index.spi$dateInfo$date <- c(index.spi$dateInfo$date, daty)
                index.spi$dateInfo$index <- c(index.spi$dateInfo$index, max(index.spi$dateInfo$index) + seq_along(daty))
            }else{
                index.spi$dateInfo$date <- c(index.spi$dateInfo$date, daty[-iy])
                index.spi$dateInfo$index <- c(index.spi$dateInfo$index, max(index.spi$dateInfo$index) + seq_along(daty[-iy]))
            }

            if(!file.exists(file.PARS.index)){
                Insert.Messages.Out(paste(message[['23']], ':', file.PARS.index), TRUE, 'e')
                return(NULL)
            }

            ##########
            index.pars <- readRDS(file.PARS.index)
            spi.distribution <- index.pars$varInfo$name
        }else{
            dir.create(dataNCdir, showWarnings = FALSE, recursive = TRUE)
            dir.create(dataSPIdir, showWarnings = FALSE, recursive = TRUE)
            dir.create(dataPARAMdir, showWarnings = FALSE, recursive = TRUE)

            ##########
            index.spi <- don
            index.spi$varInfo$name <- "spi"
            index.spi$varInfo$prec <- "float"
            index.spi$varInfo$units <- ""
            index.spi$varInfo$longname <- "Standardized Precipitation Index"

            ##########
            index.pars <- don
            index.pars$varInfo$name <- spi.distribution
            index.pars$varInfo$prec <- ""
            index.pars$varInfo$units <- ""
            index.pars$varInfo$longname <- spi.distribution
            index.pars$dateInfo$date <- temps
            index.pars$dateInfo$index <- seq_along(temps)

            out.file.gz <- gzfile(file.PARS.index, compression = 7)
            saveRDS(index.pars, out.file.gz)
            close(out.file.gz)
        }

        ###########

        out.file.gz <- gzfile(file.spi.index, compression = 7)
        saveRDS(index.spi, out.file.gz)
        close(out.file.gz)

        #########################################

        chunkfile <- sort(unique(don$colInfo$index))
        chunkcalc <- split(chunkfile, ceiling(chunkfile / don$chunkfac))

        do.parChunk <- if(don$chunkfac > length(chunkcalc)) TRUE else FALSE
        do.parCALC <- if(do.parChunk) FALSE else TRUE

        parsL <- doparallel.cond(do.parCALC & (length(chunkcalc) > 10))
        ret <- cdt.foreach(seq_along(chunkcalc), parsL, GUI = TRUE,
                           progress = TRUE, FUN = function(chkj)
        {
            don.data <- readCdtDatasetChunk.sequence(chunkcalc[[chkj]], file.aggr, cdtParallelCond, do.par = do.parChunk)
            don.data <- don.data[don$dateInfo$index, , drop = FALSE]
            don.data <- don.data[idaty, , drop = FALSE]

            data.mat <- SPEI_Aggregate_data(don.data, spi.tscale)

            ###########
            if(GeneralParameters$monitoring){
                params <- readCdtDatasetChunk.sequence(chunkcalc[[chkj]], file.PARS.index, cdtParallelCond, do.par = do.parChunk)
                params <- params[match(temps, index.pars$dateInfo$date), , drop = FALSE]
            }else{
                params <- SPEI_Compute_params(data.mat, spi.tscale, spi.frequency, spi.distribution)
                writeCdtDatasetChunk.sequence(params, chunkcalc[[chkj]], index.pars, dataPARAMdir, cdtParallelCond, do.par = do.parChunk)
            }

            ###########

            data.spi <- SPEI_computation(data.mat, params, spi.tscale, spi.frequency, spi.distribution)

            ###########
            if(GeneralParameters$monitoring){
                data.spi0 <- readCdtDatasetChunk.sequence(chunkcalc[[chkj]], file.spi.index, cdtParallelCond, do.par = do.parChunk)
                data.spi <- data.spi[idt, , drop = FALSE]

                if(length(ix) == 0){
                    data.spi0 <- rbind(data.spi0, data.spi)
                }else{
                    data.spi0[ix, ] <- data.spi[iy, , drop = FALSE]
                    data.spi0 <- rbind(data.spi0, data.spi[-iy, , drop = FALSE])
                }
            }else data.spi0 <- data.spi

            writeCdtDatasetChunk.sequence(data.spi0, chunkcalc[[chkj]], index.spi, dataSPIdir, cdtParallelCond, do.par = do.parChunk)
            rm(data.spi, data.spi0, don.data, data.mat, params); gc()
        })

        ######################

        output <- list(params = GeneralParameters, data = index.spi$coords$mat)

        ######################

        x <- index.spi$coords$mat$x
        y <- index.spi$coords$mat$y
        nx <- length(x)
        ny <- length(y)
        dx <- ncdim_def("Lon", "degreeE", x)
        dy <- ncdim_def("Lat", "degreeN", y)
        xy.dim <- list(dx, dy)
        nc.grd <- ncvar_def(index.spi$varInfo$name, index.spi$varInfo$units, xy.dim, -9999, index.spi$varInfo$longname, "float", compression = 9)

        ######################

        chunkfile <- sort(unique(index.spi$colInfo$index))
        datyread <- split(daty, ceiling(seq_along(daty) / 50))

        do.parChunk <- if(length(chunkfile) > length(datyread)) TRUE else FALSE
        do.parCALC <- if(do.parChunk) FALSE else TRUE

        parsL <- doparallel.cond(do.parCALC & (length(datyread) > 30))
        ret <- cdt.foreach(seq_along(datyread), parsL, GUI = TRUE,
                           progress = TRUE, FUN = function(jj)
        {
            daty0 <- datyread[[jj]]
            dat.spi <- readCdtDatasetChunk.sepdir.dates.order(file.spi.index, dataSPIdir, daty0, cdtParallelCond, do.par = do.parChunk)

            dat.spi[is.nan(dat.spi)] <- NA
            dat.spi[is.infinite(dat.spi)] <- NA

            for(j in seq_along(daty0)){
                spi <- dat.spi[j, ]
                dim(spi) <- c(nx, ny)

                spi[is.na(spi)] <- -9999

                filenc <- file.path(dataNCdir, paste0("spi_", daty0[j], ".nc"))
                nc <- ncdf4::nc_create(filenc, nc.grd)
                ncdf4::ncvar_put(nc, nc.grd, spi)
                ncdf4::nc_close(nc)
            }
            rm(daty0, dat.spi, spi); gc()
            return(0)
        })

        rm(don, index.spi)
    }

    .cdtData$EnvData$output <- output
    .cdtData$EnvData$PathData <- outDIR
    out.spi.index <- gzfile(out.spi.index, compression = 7)
    saveRDS(output, out.spi.index)
    close(out.spi.index)

    return(0)
}
