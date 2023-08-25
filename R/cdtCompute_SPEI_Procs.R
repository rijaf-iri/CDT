
computeSPEIProcs <- function(GeneralParameters){
    message <- .cdtData$EnvData$message
    freqData <- GeneralParameters$intstep
    input.PREC <- if(GeneralParameters$data.type == 'cdtstation') GeneralParameters$cdtstation$prec else GeneralParameters$cdtdataset$prec
    input.ETP <- if(GeneralParameters$data.type == 'cdtstation') GeneralParameters$cdtstation$etp else GeneralParameters$cdtdataset$etp

    cdtParallelCond <- .cdtData$Config$parallel

    if(input.PREC %in% c("", "NA")){
        Insert.Messages.Out(message[['12']], TRUE, 'e')
        return(NULL)
    }

    if(input.ETP %in% c("", "NA")){
        Insert.Messages.Out(message[['13']], TRUE, 'e')
        return(NULL)
    }

    if(GeneralParameters$monitoring){
        outDIR <- dirname(GeneralParameters$outdir)
        if(!dir.exists(outDIR) | !file.exists(GeneralParameters$outdir)){
            Insert.Messages.Out(message[['14']], TRUE, 'e')
            return(NULL)
        }
        dataCDTdir <- file.path(outDIR, 'CDTDATASET')
        dataPARSdir <- file.path(outDIR, 'PARAMS')

        out.spi.index <- GeneralParameters$outdir
    }else{
        if(!dir.exists(GeneralParameters$outdir)){
            Insert.Messages.Out(message[['15']], TRUE, 'e')
            Insert.Messages.Out(paste(message[['16']], getwd()))
            GeneralParameters$outdir <- getwd()
        }
        outDIR <- file.path(GeneralParameters$outdir, "SPEI_data")
        dir.create(outDIR, showWarnings = FALSE, recursive = TRUE)
        dataCDTdir <- file.path(outDIR, 'CDTDATASET')
        dir.create(dataCDTdir, showWarnings = FALSE, recursive = TRUE)
        dataPARSdir <- file.path(outDIR, 'PARAMS')
        dir.create(dataPARSdir, showWarnings = FALSE, recursive = TRUE)

        out.spi.index <- file.path(outDIR, "SPEI.rds")
    }

    ##########

    if(GeneralParameters$outfreq == "dekad") outstep <- "dekadal"
    if(GeneralParameters$outfreq == "month") outstep <- "monthly"

    ##########
    dataOUT.prec <- file.path(outDIR, paste0(toupper(outstep), "_precip"))
    dataOUT.etp <- file.path(outDIR, paste0(toupper(outstep), "_pet"))
    if(GeneralParameters$data.type == "cdtdataset"){
        datadir.prec <- file.path(dataOUT.prec, 'DATA')
        datadir.etp <- file.path(dataOUT.etp, 'DATA')
    }
    file.aggr.prec <- file.path(dataOUT.prec, paste0(toupper(outstep), "_precip.rds"))
    file.aggr.etp <- file.path(dataOUT.etp, paste0(toupper(outstep), "_pet.rds"))

    if(GeneralParameters$monitoring){
        if(!file.exists(file.aggr.prec)){
            Insert.Messages.Out(paste(file.aggr.prec, message[['17']]), TRUE, 'e')
            return(NULL)
        }
        if(!file.exists(file.aggr.etp)){
            Insert.Messages.Out(paste(file.aggr.etp, message[['17']]), TRUE, 'e')
            return(NULL)
        }
    }else{
        dir.create(dataOUT.prec, showWarnings = FALSE, recursive = TRUE)
        dir.create(dataOUT.etp, showWarnings = FALSE, recursive = TRUE)
        if(GeneralParameters$data.type == "cdtdataset"){
            dir.create(datadir.prec, showWarnings = FALSE, recursive = TRUE)
            dir.create(datadir.etp, showWarnings = FALSE, recursive = TRUE)
        }
    }

    #####################
    if(GeneralParameters$data.type == "cdtstation"){
        prec <- getStnOpenData(GeneralParameters$cdtstation$prec)
        if(is.null(prec)) return(NULL)
        prec <- getCDTdataAndDisplayMsg(prec, GeneralParameters$intstep, GeneralParameters$cdtstation$prec)
        if(is.null(prec)) return(NULL)

        etp <- getStnOpenData(GeneralParameters$cdtstation$etp)
        if(is.null(etp)) return(NULL)
        etp <- getCDTdataAndDisplayMsg(etp, GeneralParameters$intstep, GeneralParameters$cdtstation$etp)
        if(is.null(etp)) return(NULL)

        if(!any(prec$id %in% etp$id)){
            Insert.Messages.Out(message[['18']], TRUE, 'e')
            return(NULL)
        }
        if(!any(prec$dates %in% etp$dates)){
            Insert.Messages.Out(message[['19']], TRUE, 'e')
            return(NULL)
        }

        ##################
        inx <- match(prec$dates, etp$dates)
        inx <- inx[!is.na(inx)]
        etp$dates <- etp$dates[inx]
        etp$data <- etp$data[inx, , drop = FALSE]
        dx <- prec$dates %in% etp$dates
        prec$data <- prec$data[dx, , drop = FALSE]
        prec$dates <- prec$dates[dx]

        daty <- etp$dates

        ##################
        jnx <- match(prec$id, etp$id)
        jnx <- jnx[!is.na(jnx)]
        etp$id <- etp$id[jnx]

        stn.id <- etp$id
        stn.lon <- etp$lon[jnx]
        stn.lat <- etp$lat[jnx]

        etp$data <- etp$data[, jnx, drop = FALSE]
        prec$data <- prec$data[, prec$id %in% etp$id, drop = FALSE]

        if(GeneralParameters$monitoring){
            SP1 <- list(id = stn.id, lon = stn.lon, lat = stn.lat)
            SP2 <- readRDS(out.spi.index)
            SP2 <- SP2$data
            if(!isTRUE(all.equal(SP1, SP2))){
                Insert.Messages.Out(message[['20']], TRUE, 'e')
                return(NULL)
            }
            rm(SP1, SP2)
        }
    }

    if(GeneralParameters$data.type == "cdtdataset"){
        prec <- try(readRDS(GeneralParameters$cdtdataset$prec), silent = TRUE)
        if(inherits(prec, "try-error")){
            Insert.Messages.Out(paste(message[['21']], GeneralParameters$cdtdataset$prec), TRUE, 'e')
            return(NULL)
        }

        if(freqData != prec$TimeStep){
            Insert.Messages.Out(paste(message[['22']], freqData), TRUE, 'e')
            return(NULL)
        }

        etp <- try(readRDS(GeneralParameters$cdtdataset$etp), silent = TRUE)
        if(inherits(etp, "try-error")){
            Insert.Messages.Out(paste(message[['21']], GeneralParameters$cdtdataset$etp), TRUE, 'e')
            return(NULL)
        }
        if(freqData != etp$TimeStep){
            Insert.Messages.Out(paste(message[['23']], freqData), TRUE, 'e')
            return(NULL)
        }

        ##################
        SP1 <- defSpatialPixels(list(lon = prec$coords$mat$x, lat = prec$coords$mat$y))
        SP2 <- defSpatialPixels(list(lon = etp$coords$mat$x, lat = etp$coords$mat$y))
        if(is.diffSpatialPixelsObj(SP1, SP2, tol = 1e-04)){
            Insert.Messages.Out(message[['24']], TRUE, 'e')
            return(NULL)
        }
        rm(SP1, SP2)

        ##################
        if(prec$chunksize != etp$chunksize){
            Insert.Messages.Out(message[['25']], TRUE, 'e')
            return(NULL)
        }

        ##################
        if(!any(prec$dateInfo$date %in% etp$dateInfo$date)){
            Insert.Messages.Out(message[['26']], TRUE, 'e')
            return(NULL)
        }

        ##################
        inx <- match(prec$dateInfo$date, etp$dateInfo$date)
        inx <- inx[!is.na(inx)]
        etp$dateInfo$date <- etp$dateInfo$date[inx]
        etp$dateInfo$index <- etp$dateInfo$index[inx]

        pdaty <- prec$dateInfo$date %in% etp$dateInfo$date
        prec$dateInfo$date <- prec$dateInfo$date[pdaty]
        prec$dateInfo$index <- prec$dateInfo$index[pdaty]

        daty <- prec$dateInfo$date

        if(GeneralParameters$monitoring){
            SP1 <- defSpatialPixels(list(lon = prec$coords$mat$x, lat = prec$coords$mat$y))
            SP2 <- readRDS(out.spi.index)
            SP2 <- defSpatialPixels(list(lon = SP2$data$x, lat = SP2$data$y))
            if(is.diffSpatialPixelsObj(SP1, SP2, tol = 1e-04)){
                Insert.Messages.Out(message[['27']], TRUE, 'e')
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
            Insert.Messages.Out(message[['28']], TRUE, 'e')
            return(NULL)
        }
        daty <- daty[idaty0]
    }else idaty0 <- rep(TRUE, length(daty))

    #####################

    GeneralParameters <- GeneralParameters

    toAggr <- list(input.PREC, input.ETP, freqData, GeneralParameters$outfreq, idaty0)

    if((GeneralParameters$outfreq == "dekad" & freqData != 'dekadal') |
        (GeneralParameters$outfreq == "month" & freqData != 'monthly'))
    {
        if(is.null(.cdtData$EnvData$toAggr)){
            aggregatData <- TRUE
        }else{
            aggregatData <- if(!isTRUE(all.equal(.cdtData$EnvData$toAggr, toAggr))) TRUE else FALSE
        }
    }else aggregatData <- FALSE

    if(aggregatData){
        txtAggr <- if(GeneralParameters$outfreq == "dekad") "dekadal" else "monthly"
        Insert.Messages.Out(paste(message[['29']], txtAggr, " ......"), TRUE, "i")

        outfreq <- switch(GeneralParameters$outfreq, "dekad" = "dekadal", "month" = "monthly")
        agg.index <- cdt.index.aggregate(daty, freqData, outfreq)

        ifull <- (agg.index$nba / agg.index$nb0) >= 0.95
        outdates <- agg.index$date
        index <- agg.index$index

        ##########
        if(GeneralParameters$data.type == "cdtstation"){
            prec <- drought.indices.aggr.stn(prec, idaty0, agg.index, ifull, file.aggr.prec, GeneralParameters$monitoring)

            out.file.prec.gz <- gzfile(file.aggr.prec, compression = 7)
            saveRDS(prec, out.file.prec.gz)
            close(out.file.prec.gz)

            etp <- drought.indices.aggr.stn(etp, idaty0, agg.index, ifull, file.aggr.etp, GeneralParameters$monitoring)

            out.file.etp.gz <- gzfile(file.aggr.etp, compression = 7)
            saveRDS(etp, out.file.etp.gz)
            close(out.file.etp.gz)
        }

        if(GeneralParameters$data.type == "cdtdataset"){
            index.prec0 <- drought.indices.index.cdt(prec, outstep, outdates, file.aggr.prec, GeneralParameters$monitoring)

            out.file.prec.gz <- gzfile(file.aggr.prec, compression = 7)
            saveRDS(index.prec0$index, out.file.prec.gz)
            close(out.file.prec.gz)

            ##########
            index.etp0 <- drought.indices.index.cdt(etp, outstep, outdates, file.aggr.etp, GeneralParameters$monitoring)

            out.file.etp.gz <- gzfile(file.aggr.etp, compression = 7)
            saveRDS(index.etp0$index, out.file.etp.gz)
            close(out.file.etp.gz)

            ##########
            chunkfile <- sort(unique(prec$colInfo$index))
            chunkcalc <- split(chunkfile, ceiling(chunkfile / prec$chunkfac))

            do.parChunk <- if(prec$chunkfac > length(chunkcalc)) TRUE else FALSE
            do.parCALC <- if(do.parChunk) FALSE else TRUE

            parsL <- doparallel.cond(do.parCALC & (length(chunkcalc) > 10))
            ret <- cdt.foreach(seq_along(chunkcalc), parsL, GUI = TRUE,
                               progress = TRUE, FUN = function(chkj)
            {
                drought.indices.aggr.cdt(prec, idaty0, agg.index$index, ifull, file.aggr.prec,
                                         GeneralParameters$monitoring, GeneralParameters$cdtdataset$prec,
                                         index.prec0, datadir.prec, chunkcalc[[chkj]],
                                         do.parChunk, cdtParallelCond)

                drought.indices.aggr.cdt(etp, idaty0, agg.index$index, ifull, file.aggr.etp,
                                         GeneralParameters$monitoring, GeneralParameters$cdtdataset$etp,
                                         index.etp0, datadir.etp, chunkcalc[[chkj]],
                                         do.parChunk, cdtParallelCond)
            })

            prec <- index.prec0$index
            etp <- index.etp0$index
            rm(index.prec0, index.etp0)
        }

        .cdtData$EnvData$toAggr <- toAggr
        Insert.Messages.Out(paste(txtAggr, message[['30']]), TRUE, "s")
        Insert.Messages.Out(message[['31']], TRUE, "i")
    }else{
        if((GeneralParameters$outfreq == "dekad" & freqData != 'dekadal') |
           (GeneralParameters$outfreq == "month" & freqData != 'monthly'))
        {
            prec <- readRDS(file.aggr.prec)
            etp <- readRDS(file.aggr.etp)
        }else{
            if(GeneralParameters$data.type == "cdtstation"){
                if(GeneralParameters$monitoring){
                    prec <- drought.indices.update.stn(prec, idaty0, file.aggr.prec)
                    etp <- drought.indices.update.stn(etp, idaty0, file.aggr.etp)
                }

                out.file.prec.gz <- gzfile(file.aggr.prec, compression = 7)
                saveRDS(prec, out.file.prec.gz)
                close(out.file.prec.gz)

                out.file.etp.gz <- gzfile(file.aggr.etp, compression = 7)
                saveRDS(etp, out.file.etp.gz)
                close(out.file.etp.gz)
            }

            if(GeneralParameters$data.type == "cdtdataset"){
                index.prec <- prec
                index.etp <- etp

                if(GeneralParameters$monitoring){
                    index.prec0 <- drought.indices.update.index.cdt(index.prec, idaty0, file.aggr.prec)
                    index.etp0 <- drought.indices.update.index.cdt(index.etp, idaty0, file.aggr.etp)

                    ##########
                    chunkfile <- sort(unique(prec$colInfo$index))
                    chunkcalc <- split(chunkfile, ceiling(chunkfile / prec$chunkfac))

                    do.parChunk <- if(prec$chunkfac > length(chunkcalc)) TRUE else FALSE
                    do.parCALC <- if(do.parChunk) FALSE else TRUE

                    parsL <- doparallel.cond(do.parCALC & (length(chunkcalc) > 10))
                    ret <- cdt.foreach(seq_along(chunkcalc), parsL, GUI = TRUE,
                                       progress = TRUE, FUN = function(chkj)
                    {
                        drought.indices.update.cdt(prec, idaty0, file.aggr.prec,
                                                   GeneralParameters$cdtdataset$prec,
                                                   index.prec0, datadir.prec, chunkcalc[[chkj]],
                                                   do.parChunk, cdtParallelCond)

                        drought.indices.update.cdt(etp, idaty0, file.aggr.etp,
                                                   GeneralParameters$cdtdataset$etp,
                                                   index.etp0, datadir.etp, chunkcalc[[chkj]],
                                                   do.parChunk, cdtParallelCond)
                    })

                    index.prec0 <- index.prec0$index
                    index.etp0 <- index.etp0$index
                }else{
                    file.copy(file.path(dirname(GeneralParameters$cdtdataset$prec), "DATA"),
                                dataOUT.prec, overwrite = TRUE, recursive = TRUE)
                    index.prec0 <- index.prec

                    file.copy(file.path(dirname(GeneralParameters$cdtdataset$etp), "DATA"),
                                dataOUT.etp, overwrite = TRUE, recursive = TRUE)
                    index.etp0 <- index.etp
                }

                prec <- index.prec0
                etp <- index.etp0

                out.file.prec.gz <- gzfile(file.aggr.prec, compression = 7)
                saveRDS(index.prec0, out.file.prec.gz)
                close(out.file.prec.gz)

                out.file.etp.gz <- gzfile(file.aggr.etp, compression = 7)
                saveRDS(index.etp0, out.file.etp.gz)
                close(out.file.etp.gz)

                rm(index.prec0, index.prec, index.etp0, index.etp); gc()
            }
        }
    }

    #####################

    if(GeneralParameters$data.type == "cdtstation") dtemps <- prec$dates
    if(GeneralParameters$data.type == "cdtdataset") dtemps <- prec$dateInfo$date

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
    }else idaty <- rep(TRUE, length(dtemps))

    dtemps <- dtemps[idaty]
    nl <- length(dtemps)
    temps <- sapply(1:spi.frequency, function(k){
        if(k + spi.tscale - 1 > nl) return(NA)
        iseq <- seq(k + spi.tscale - 1, nl, spi.frequency)
        dates <- dtemps[iseq][1]
        if(GeneralParameters$outfreq == "month") out <- as.numeric(substr(dates, 5, 6))
        if(GeneralParameters$outfreq == "dekad"){
            dek <- expand.grid(1:3, stringr::str_pad(1:12, 2, pad = "0"))
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
        file.SPI.csv <- file.path(dataSTNdir, paste0("SPEI_", spi.out.suffix, ".csv"))
        file.SPI.rds <- file.path(dataCDTdir, paste0("SPEI_", spi.out.suffix, ".rds"))

        data.mon <- prec$data[idaty, , drop = FALSE] - etp$data[idaty, , drop = FALSE]
        data.mat <- SPEI_Aggregate_data(data.mon, spi.tscale)

        ###########
        if(GeneralParameters$monitoring){
            file.PARS.rds <- file.path(dataPARSdir, paste0("SPEI_", spi.out.suffix, ".rds"))
            if(!file.exists(file.PARS.rds)){
                Insert.Messages.Out(paste(message[['32']], ':', file.PARS.rds), TRUE, 'e')
                return(NULL)
            }
            spi.params <- readRDS(file.PARS.rds)
            spi.distribution <- spi.params$distr
            params <- spi.params$pars
            params <- params[match(temps, spi.params$index), , drop = FALSE]
        }else{
            dir.create(dataSTNdir, showWarnings = FALSE, recursive = TRUE)
            file.PARS.rds <- file.path(dataPARSdir, paste0("SPEI_", spi.out.suffix, ".rds"))
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
        }else out.cdt.spi <- list(date = prec$dates, spi = data.spi)

        out.file.gz <- gzfile(file.SPI.rds, compression = 7)
        saveRDS(out.cdt.spi, out.file.gz)
        close(out.file.gz)

        ###########
        xhead <- rbind(stn.id, stn.lon, stn.lat)
        chead <- c('ID.STN', 'LON', 'DATE/LAT')
        infohead <- cbind(chead, xhead)

        xdata <- do.call(cbind, out.cdt.spi)
        if(spi.tscale > 1) xdata <- xdata[-(1:(spi.tscale - 1)), ]
        data.spi <- rbind(infohead, xdata)
        data.spi[is.na(data.spi)] <- .cdtData$Config$missval.anom
        writeFiles(data.spi, file.SPI.csv)

        output <- list(params = GeneralParameters,
                        data = list(id = stn.id, lon = stn.lon, lat = stn.lat))
        rm(prec, etp, data.mon, data.spi, out.cdt.spi, xdata)
    }

    if(GeneralParameters$data.type == "cdtdataset"){
        dataNCdir <- file.path(outDIR, 'DATA_NetCDF', paste0("SPEI_", spi.out.suffix))
        dataSPIdir <- file.path(dataCDTdir, paste0("SPEI_", spi.out.suffix), "DATA")
        file.spi.index <- file.path(dirname(dataSPIdir), paste0("SPEI_", spi.out.suffix, ".rds"))
        dataPARAMdir <- file.path(dataPARSdir, paste0("SPEI_", spi.out.suffix), "DATA")
        file.PARS.index <- file.path(dirname(dataPARAMdir), paste0("SPEI_", spi.out.suffix, ".rds"))

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
                Insert.Messages.Out(paste(message[['32']], ':', file.PARS.index), TRUE, 'e')
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
            index.spi <- prec
            index.spi$varInfo$name <- "spi"
            index.spi$varInfo$prec <- "float"
            index.spi$varInfo$units <- ""
            index.spi$varInfo$longname <- "Standardized Precipitation Index"

            ##########
            index.pars <- prec
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

        chunkfile <- sort(unique(prec$colInfo$index))
        chunkcalc <- split(chunkfile, ceiling(chunkfile / prec$chunkfac))

        do.parChunk <- if(prec$chunkfac > length(chunkcalc)) TRUE else FALSE
        do.parCALC <- if(do.parChunk) FALSE else TRUE

        parsL <- doparallel.cond(do.parCALC & (length(chunkcalc) > 10))
        ret <- cdt.foreach(seq_along(chunkcalc), parsL, GUI = TRUE,
                           progress = TRUE, FUN = function(chkj)
        {
            prec.data <- readCdtDatasetChunk.sequence(chunkcalc[[chkj]], file.aggr.prec, cdtParallelCond, do.par = do.parChunk)
            prec.data <- prec.data[prec$dateInfo$index, , drop = FALSE]
            prec.data <- prec.data[idaty, , drop = FALSE]

            etp.data <- readCdtDatasetChunk.sequence(chunkcalc[[chkj]], file.aggr.etp, cdtParallelCond, do.par = do.parChunk)
            etp.data <- etp.data[etp$dateInfo$index, , drop = FALSE]
            etp.data <- etp.data[idaty, , drop = FALSE]

            data.mat <- SPEI_Aggregate_data(prec.data - etp.data, spi.tscale)

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
            rm(data.spi, data.spi0, prec.data, etp.data, data.mat, params); gc()
        })

        ######################

        output <- list(params = GeneralParameters, data = index.spi$coords$mat)

        ######################

        x <- index.spi$coords$mat$x
        y <- index.spi$coords$mat$y
        nx <- length(x)
        ny <- length(y)
        dx <- ncdf4::ncdim_def("Lon", "degreeE", x)
        dy <- ncdf4::ncdim_def("Lat", "degreeN", y)
        xy.dim <- list(dx, dy)
        nc.grd <- ncdf4::ncvar_def(index.spi$varInfo$name, index.spi$varInfo$units, xy.dim, -9999, index.spi$varInfo$longname, "float", compression = 9)

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

                filenc <- file.path(dataNCdir, paste0("spei_", daty0[j], ".nc"))
                nc <- ncdf4::nc_create(filenc, nc.grd)
                ncdf4::ncvar_put(nc, nc.grd, spi)
                ncdf4::nc_close(nc)
            }
            rm(daty0, dat.spi, spi); gc()
            return(0)
        })

        rm(prec, etp, index.spi)
    }

    .cdtData$EnvData$output <- output
    .cdtData$EnvData$PathData <- outDIR
    out.spi.index <- gzfile(out.spi.index, compression = 7)
    saveRDS(output, out.spi.index)
    close(out.spi.index)

    return(0)
}
