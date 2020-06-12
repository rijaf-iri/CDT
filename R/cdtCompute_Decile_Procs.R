
computeDecileProcs <- function(GeneralParameters){
    message <- .cdtData$EnvData$message
    freqData <- GeneralParameters$intstep
    input.file <- if(GeneralParameters$data.type == 'cdtstation') GeneralParameters$cdtstation else GeneralParameters$cdtdataset
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

        out.decile.index <- GeneralParameters$outdir
    }else{
        if(!dir.exists(GeneralParameters$outdir)){
            Insert.Messages.Out(message[['14']], TRUE, 'e')
            Insert.Messages.Out(paste(message[['15']], getwd()))
            GeneralParameters$outdir <- getwd()
        }
        outDIR <- file.path(GeneralParameters$outdir, "DECILE_data")
        dir.create(outDIR, showWarnings = FALSE, recursive = TRUE)
        dataCDTdir <- file.path(outDIR, 'CDTDATASET')
        dir.create(dataCDTdir, showWarnings = FALSE, recursive = TRUE)
        dataPARSdir <- file.path(outDIR, 'PARAMS')
        dir.create(dataPARSdir, showWarnings = FALSE, recursive = TRUE)

        out.decile.index <- file.path(outDIR, "Decile.rds")
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
            SP2 <- readRDS(out.decile.index)
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
            SP2 <- readRDS(out.decile.index)
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

    decile.out.suffix <- if(GeneralParameters$outfreq == "dekad") "1dek" else paste0(GeneralParameters$tscale, "mon")
    base.period <- GeneralParameters$base.period

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
            Insert.Messages.Out(message[['19']], TRUE, 'e')
            return(NULL)
        }
        daty <- daty[idaty0]
    }else idaty0 <- rep(TRUE, length(daty))

    #####################

    GeneralParameters <- GeneralParameters
    cdtParallelCond <- .cdtData$Config[c('dopar', 'detect.cores', 'nb.cores')]

    #####################

    toAggr <- list(input.file, freqData, GeneralParameters$outfreq, idaty0)

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
        if(GeneralParameters$outfreq == "month")
            out <- as.numeric(substr(dates, 5, 6))
        if(GeneralParameters$outfreq == "dekad"){
            dek <- expand.grid(1:3, str_pad(1:12, 2, pad = "0"))
            out <- which(paste0(dek[, 2], dek[, 1]) == substr(dates, 5, 7))
        }
        return(out)
    })
    temps <- temps[!is.na(temps)]

    ###########

    if(GeneralParameters$outfreq == "month")
        index.dec <- as.numeric(substr(dtemps, 5, 6))
    if(GeneralParameters$outfreq == "dekad"){
        mdk <- substr(dtemps, 5, 7)
        dek <- expand.grid(1:3, stringr::str_pad(1:12, 2, pad = "0"))
        dek <- apply(dek[, 2:1], 1, paste0, collapse = '')
        index.dec <- match(mdk, dek)
    }

    ###########

    if(GeneralParameters$monitoring){
        idt <- seq_along(dtemps)
        idt <- if(spi.tscale > 1) idt[-(1:(spi.tscale - 1))] else idt
        daty <- dtemps[idt]
    }

    #####################
    if(GeneralParameters$data.type == "cdtstation"){
        dataSTNdir <- file.path(outDIR, 'CDTSTATIONS')
        file.Decile.csv <- file.path(dataSTNdir, paste0("Decile_", decile.out.suffix, ".csv"))
        file.Decile.rds <- file.path(dataCDTdir, paste0("Decile_", decile.out.suffix, ".rds"))

        don.data <- don$data[idaty, , drop = FALSE]
        don.dates <- don$dates[idaty]
        data.mat <- SPEI_Aggregate_data(don.data, spi.tscale)

        ###########
        if(GeneralParameters$monitoring){
            file.PARS.rds <- file.path(dataPARSdir, paste0("Decile_", decile.out.suffix, ".rds"))
            if(!file.exists(file.PARS.rds)){
                Insert.Messages.Out(paste(message[['23']], ':', file.PARS.rds), TRUE, 'e')
                return(NULL)
            }
            decile.params <- readRDS(file.PARS.rds)
            itmp <- match(temps, decile.params$index)
            decile.params$index <- decile.params$index[itmp]
            decile.params$decile <- decile.params$decile[itmp, , drop = FALSE]
        }else{
            dir.create(dataSTNdir, showWarnings = FALSE, recursive = TRUE)
            file.PARS.rds <- file.path(dataPARSdir, paste0("Decile_", decile.out.suffix, ".rds"))
            decile.params <- list(index = temps)
            decile.params$decile <- Decile_Compute_params(data.mat, don.dates, temps, base.period, GeneralParameters$outfreq)

            out.file.gz <- gzfile(file.PARS.rds, compression = 7)
            saveRDS(decile.params, out.file.gz)
            close(out.file.gz)
        }

        ###########

        data.decile <- Decile_computation(data.mat, index.dec, decile.params)

        ###########
        if(GeneralParameters$monitoring){
            data.decile0 <- readRDS(file.Decile.rds)
            out.cdt.decile <- list()
            data.decile <- data.decile[idt, , drop = FALSE]

            ix <- match(daty, data.decile0$date)
            ix <- ix[!is.na(ix)]
            iy <- match(data.decile0$date, daty)
            iy <- iy[!is.na(iy)]
            if(length(ix) == 0){
                out.cdt.decile$date <- c(data.decile0$date, daty)
                out.cdt.decile$decile <- rbind(data.decile0$decile, data.decile)
            }else{
                out.cdt.decile$date <- c(data.decile0$date, daty[-iy])
                data.decile0$decile[ix, ] <- data.decile[iy, , drop = FALSE]
                out.cdt.decile$decile <- rbind(data.decile0$decile, data.decile[-iy, , drop = FALSE])
            }

            rm(data.decile0)
        }else out.cdt.decile <- list(date = don$dates, decile = data.decile)

        out.file.gz <- gzfile(file.Decile.rds, compression = 7)
        saveRDS(out.cdt.decile, out.file.gz)
        close(out.file.gz)

        ###########
        xhead <- rbind(don$id, don$lon, don$lat)
        chead <- c('ID.STN', 'LON', 'DATE/LAT')
        infohead <- cbind(chead, xhead)

        xdata <- do.call(cbind, out.cdt.decile)
        if(spi.tscale > 1) xdata <- xdata[-(1:(spi.tscale - 1)), ]
        data.decile <- rbind(infohead, xdata)
        data.decile[is.na(data.decile)] <- .cdtData$Config$missval.anom
        writeFiles(data.decile, file.Decile.csv)

        output <- list(params = GeneralParameters,
                        data = list(id = don$id, lon = don$lon, lat = don$lat))
        rm(don, data.decile, out.cdt.decile, xdata, don.data)
    }

    if(GeneralParameters$data.type == "cdtdataset"){
        dataNCdir <- file.path(outDIR, 'DATA_NetCDF', paste0("Decile_", decile.out.suffix))
        dataSPIdir <- file.path(dataCDTdir, paste0("Decile_", decile.out.suffix), "DATA")
        file.decile.index <- file.path(dirname(dataSPIdir), paste0("Decile_", decile.out.suffix, ".rds"))
        dataPARAMdir <- file.path(dataPARSdir, paste0("Decile_", decile.out.suffix), "DATA")
        file.PARS.index <- file.path(dirname(dataPARAMdir), paste0("Decile_", decile.out.suffix, ".rds"))

        #########################################

        if(GeneralParameters$monitoring){
            index.decile <- readRDS(file.decile.index)
            ix <- match(daty, index.decile$dateInfo$date)
            ix <- ix[!is.na(ix)]
            iy <- match(index.decile$dateInfo$date, daty)
            iy <- iy[!is.na(iy)]

            if(length(ix) == 0){
                index.decile$dateInfo$date <- c(index.decile$dateInfo$date, daty)
                index.decile$dateInfo$index <- c(index.decile$dateInfo$index, max(index.decile$dateInfo$index) + seq_along(daty))
            }else{
                index.decile$dateInfo$date <- c(index.decile$dateInfo$date, daty[-iy])
                index.decile$dateInfo$index <- c(index.decile$dateInfo$index, max(index.decile$dateInfo$index) + seq_along(daty[-iy]))
            }

            if(!file.exists(file.PARS.index)){
                Insert.Messages.Out(paste(message[['23']], ':', file.PARS.index), TRUE, 'e')
                return(NULL)
            }

            ##########
            index.pars <- readRDS(file.PARS.index)
            itmp <- match(temps, index.pars$dateInfo$date)
        }else{
            dir.create(dataNCdir, showWarnings = FALSE, recursive = TRUE)
            dir.create(dataSPIdir, showWarnings = FALSE, recursive = TRUE)
            dir.create(dataPARAMdir, showWarnings = FALSE, recursive = TRUE)

            ##########
            index.decile <- don
            index.decile$varInfo$name <- "decile"
            index.decile$varInfo$prec <- "short"
            index.decile$varInfo$units <- ""
            index.decile$varInfo$longname <- "Deciles"

            ##########
            index.pars <- don
            index.pars$varInfo$name <- "decile"
            index.pars$varInfo$prec <- ""
            index.pars$varInfo$units <- ""
            index.pars$varInfo$longname <- "Decile data"
            index.pars$dateInfo$date <- temps
            index.pars$dateInfo$index <- seq_along(temps)

            out.file.gz <- gzfile(file.PARS.index, compression = 7)
            saveRDS(index.pars, out.file.gz)
            close(out.file.gz)
        }

        ###########

        out.file.gz <- gzfile(file.decile.index, compression = 7)
        saveRDS(index.decile, out.file.gz)
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
            don.dates <- don$dateInfo$date[idaty]

            data.mat <- SPEI_Aggregate_data(don.data, spi.tscale)

            ###########
            if(GeneralParameters$monitoring){
                decile <- readCdtDatasetChunk.sequence(chunkcalc[[chkj]], file.PARS.index, cdtParallelCond, do.par = do.parChunk)
                decile <- decile[itmp, , drop = FALSE]
                index.pars$dateInfo$date <- index.pars$dateInfo$date[itmp]
            }else{
                decile <- Decile_Compute_params(data.mat, don.dates, temps, base.period, GeneralParameters$outfreq)
                writeCdtDatasetChunk.sequence(decile, chunkcalc[[chkj]], index.pars, dataPARAMdir, cdtParallelCond, do.par = do.parChunk)
            }
            decile.params <- list(index = index.pars$dateInfo$date, decile = decile)

            ###########

            data.decile <- Decile_computation(data.mat, index.dec, decile.params)

            ###########
            if(GeneralParameters$monitoring){
                data.decile0 <- readCdtDatasetChunk.sequence(chunkcalc[[chkj]], file.decile.index, cdtParallelCond, do.par = do.parChunk)
                data.decile <- data.decile[idt, , drop = FALSE]

                if(length(ix) == 0){
                    data.decile0 <- rbind(data.decile0, data.decile)
                }else{
                    data.decile0[ix, ] <- data.decile[iy, , drop = FALSE]
                    data.decile0 <- rbind(data.decile0, data.decile[-iy, , drop = FALSE])
                }
            }else data.decile0 <- data.decile

            writeCdtDatasetChunk.sequence(data.decile0, chunkcalc[[chkj]], index.decile, dataSPIdir, cdtParallelCond, do.par = do.parChunk)
            rm(data.decile, data.decile0, don.data, data.mat, decile.params, decile); gc()
        })

        ######################

        output <- list(params = GeneralParameters, data = index.decile$coords$mat)

        ######################

        x <- index.decile$coords$mat$x
        y <- index.decile$coords$mat$y
        nx <- length(x)
        ny <- length(y)
        dx <- ncdim_def("Lon", "degreeE", x)
        dy <- ncdim_def("Lat", "degreeN", y)
        xy.dim <- list(dx, dy)
        nc.grd <- ncvar_def(index.decile$varInfo$name, index.decile$varInfo$units, xy.dim, -9999, index.decile$varInfo$longname, "short", compression = 9)

        ######################

        chunkfile <- sort(unique(index.decile$colInfo$index))
        datyread <- split(daty, ceiling(seq_along(daty) / 50))

        do.parChunk <- if(length(chunkfile) > length(datyread)) TRUE else FALSE
        do.parCALC <- if(do.parChunk) FALSE else TRUE

        parsL <- doparallel.cond(do.parCALC & (length(datyread) > 30))
        ret <- cdt.foreach(seq_along(datyread), parsL, GUI = TRUE,
                           progress = TRUE, FUN = function(jj)
        {
            daty0 <- datyread[[jj]]
            dat.decile <- readCdtDatasetChunk.sepdir.dates.order(file.decile.index, dataSPIdir, daty0, cdtParallelCond, do.par = do.parChunk)

            dat.decile[is.nan(dat.decile)] <- NA
            dat.decile[is.infinite(dat.decile)] <- NA

            for(j in seq_along(daty0)){
                decile <- dat.decile[j, ]
                dim(decile) <- c(nx, ny)

                decile[is.na(decile)] <- -9999

                filenc <- file.path(dataNCdir, paste0("decile_", daty0[j], ".nc"))
                nc <- ncdf4::nc_create(filenc, nc.grd)
                ncdf4::ncvar_put(nc, nc.grd, decile)
                ncdf4::nc_close(nc)
            }
            rm(daty0, dat.decile, decile); gc()
            return(0)
        })

        rm(don, index.decile)
    }

    .cdtData$EnvData$output <- output
    .cdtData$EnvData$PathData <- outDIR
    out.decile.index <- gzfile(out.decile.index, compression = 7)
    saveRDS(output, out.decile.index)
    close(out.decile.index)

    return(0)
}
