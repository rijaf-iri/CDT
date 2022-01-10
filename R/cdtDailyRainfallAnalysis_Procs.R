
dailyRainAnalysisCalcProcs <- function(GeneralParameters){
    message <- .cdtData$EnvData$message

    if(!dir.exists(GeneralParameters$output)){
        Insert.Messages.Out(paste(GeneralParameters$output, message[['5']]), TRUE, 'e')
        return(NULL)
    }

    #############
    fcdtdataset <- GeneralParameters$cdtdataset
    fcdtstation <- GeneralParameters$cdtstation

    pars.seas <- GeneralParameters$seas
    pars.stat <- GeneralParameters$stats
    pars.def <- GeneralParameters$def

    start.year <- pars.seas$startYear
    end.year <- pars.seas$endYear
    start.mon <- pars.seas$startMon
    start.day <- pars.seas$startDay
    end.mon <- pars.seas$endMon
    end.day <- pars.seas$endDay

    drywet.day <- pars.def$drywet.day
    drywet.day <- if(drywet.day == 0) 1e-8 else drywet.day
    drywet.spell <- pars.def$drywet.spell
    aggr.pars <- list(min.frac = pars.seas$min.frac,
                      drywet.day = drywet.day,
                      drywet.spell = drywet.spell)

    #############

    if(any(is.na(c(start.mon, start.day, end.mon, end.day)))){
        Insert.Messages.Out(message[['6']], TRUE, 'e')
        return(NULL)
    }

    if(!pars.seas$all.years &
        any(is.na(c(start.year, end.year))))
    {
        Insert.Messages.Out(message[['7']], TRUE, 'e')
        return(NULL)
    }

    #######################

    stats.directory <- switch(pars.stat$daily,
                              'tot.rain' = "TOTALRAIN",
                              'rain.int' = "RAININT",
                              'nb.wet.day' = "WETDAY",
                              'nb.dry.day' = "DRYDAY",
                              'nb.wet.spell' = "WETSPELL",
                              'nb.dry.spell' = "DRYSPELL")

    stats.season <- paste0(start.mon, "-", start.day, "_", end.mon, "-", end.day)

    ################################################

    if(GeneralParameters$data.type == "cdtstation"){
        don <- getStnOpenData(fcdtstation)
        if(is.null(don)) return(NULL)
        don <- getCDTdataAndDisplayMsg(don, "daily", fcdtstation)
        if(is.null(don)) return(NULL)

        daty <- don$dates
    }

    if(GeneralParameters$data.type == "cdtdataset"){
        don <- try(readRDS(fcdtdataset), silent = TRUE)
        if(inherits(don, "try-error")){
            Insert.Messages.Out(paste(message[['8']], fcdtdataset), TRUE, 'e')
            return(NULL)
        }
        if(don$TimeStep != "daily"){
            Insert.Messages.Out(message[['9']], TRUE, 'e')
            return(NULL)
        }

        daty <- don$dateInfo$date
    }

    year <- as.numeric(substr(daty, 1, 4))
    if(pars.seas$all.years){
        start.year <- min(year, na.rm = TRUE)
        end.year <- max(year, na.rm = TRUE)
    }

    iyear <- year >= start.year & year <= end.year
    daty <- daty[iyear]

    # if(GeneralParameters$data.type == "cdtstation")
    #     don$data <- don$data[iyear, , drop = FALSE]

    index <- cdt.index.DailySeason(daty, start.mon, start.day, end.mon, end.day)

    miss.day <- sapply(seq_along(index$index), function(jj){
        xdx <- index$index[[jj]]
        xdx <- xdx[!is.na(xdx)]
        length.day <- as.numeric(diff(as.Date(index$date[[jj]], '%Y%m%d')) + 1)
        length(xdx) / length.day < pars.seas$min.frac
    })

    out.daty <- sapply(index$date, paste0, collapse = '_')

    #########################################

    outDIR <- file.path(GeneralParameters$output, "DAILY.RAINFALL.ANALYSIS_data")
    dir.create(outDIR, showWarnings = FALSE, recursive = TRUE)

    ################################################

    toAggr <- list(index, pars.stat$daily, pars.seas$min.frac,
                   c(start.year, end.year),
                   c(start.mon, start.day, end.mon, end.day),
                   c(drywet.day, drywet.spell))

    if(is.null(.cdtData$EnvData$toAggr)){
        aggregatData <- TRUE
    }else{
        aggregatData <- if(!isTRUE(all.equal(.cdtData$EnvData$toAggr, toAggr))) TRUE else FALSE
    }

    if(aggregatData){
        Insert.Messages.Out(message[['10']], TRUE, "i")

        if(GeneralParameters$data.type == "cdtstation"){
            dataOUT <- file.path(outDIR, stats.directory)
            dir.create(dataOUT, showWarnings = FALSE, recursive = TRUE)
            out.cdt.rds <- gzfile(file.path(dataOUT, paste0(stats.directory, '.rds')), compression = 7)
            out.cdt.csv <- file.path(dataOUT, paste0(stats.directory, '.csv'))

            don$data <- don$data[iyear, , drop = FALSE]

            dat.analys <- lapply(seq_along(index$index), function(jj){
                if(miss.day[[jj]]) return(rep(NA, length(don$id)))
                cdt.daily.statistics(don$data[index$index[[jj]], , drop = FALSE],
                                     STATS = pars.stat$daily, pars = aggr.pars)
            })
            dat.analys <- do.call(rbind, dat.analys)
            index.out <- NULL

            ##################

            xhead <- rbind(don$id, don$lon, don$lat)
            chead <- c('ID.STN', 'LON', 'SEASON/LAT')
            infohead <- cbind(chead, xhead)

            dat.tmp <- dat.analys
            dat.tmp[is.na(dat.tmp)] <- .cdtData$Config$missval
            dat.tmp <- rbind(infohead, cbind(out.daty, dat.tmp))
            writeFiles(dat.tmp, out.cdt.csv)
            saveRDS(dat.analys, out.cdt.rds)
            close(out.cdt.rds)
            rm(dat.tmp)
        }

        if(GeneralParameters$data.type == "cdtdataset"){
            datadir <- file.path(outDIR, stats.directory, 'DATA')
            dir.create(datadir, showWarnings = FALSE, recursive = TRUE)
            file.index <- file.path(outDIR, stats.directory, paste0(stats.directory, '.rds'))

            ncdfOUT.seas <- file.path(outDIR, stats.directory, 'DATA_NetCDF')
            dir.create(ncdfOUT.seas, showWarnings = FALSE, recursive = TRUE)

            ##################

            index.out <- don

            CbDailyStatsVAL <- c('Total Rainfall', 'Rainfall Intensity', 'Number of Wet Days',
                                 'Number of Dry Days', 'Number of Wet Spells', 'Number of Dry Spells')
            stats.prefix <- switch(pars.stat$daily,
                                   'tot.rain' = CbDailyStatsVAL[1],
                                   'rain.int' = CbDailyStatsVAL[2],
                                   'nb.wet.day' = CbDailyStatsVAL[3],
                                   'nb.dry.day' = CbDailyStatsVAL[4],
                                   'nb.wet.spell' = CbDailyStatsVAL[5],
                                   'nb.dry.spell' = CbDailyStatsVAL[6])

            stats.params <- switch(pars.stat$daily,
                                   'tot.rain' = "",
                                   'rain.int' = "",
                                   'nb.wet.day' = paste0("wet day rr >= ", drywet.day, ' mm'),
                                   'nb.dry.day' = paste0("dry day rr < ", drywet.day, ' mm'),
                                   'nb.wet.spell' = paste0("wet day rr >= ", drywet.day, ' mm', ' & ', "wet spell >= ", drywet.spell, ' days'),
                                   'nb.dry.spell' = paste0("dry day rr < ", drywet.day, ' mm', ' & ', "dry spell >= ", drywet.spell, ' days'))

            index.out$varInfo$name <- pars.stat$daily
            index.out$varInfo$prec <- 'float'
            index.out$varInfo$units <- switch(pars.stat$daily,
                                              'tot.rain' = "mm",
                                              'rain.int' = "mm/day",
                                              'nb.wet.day' = "days",
                                              'nb.dry.day' = "days",
                                              'nb.wet.spell' = "spells",
                                              'nb.dry.spell' = "spells")
            index.out$varInfo$longname <- paste(stats.prefix, ":", stats.season, ";", stats.params)
            
            index.out$TimeStep <- "seasonal"
            index.out$dateInfo$date <- out.daty
            index.out$dateInfo$index <- seq_along(index$date)

            file.index.gz <- gzfile(file.index, compression = 7)
            saveRDS(index.out, file.index.gz)
            close(file.index.gz)
            
            #########################################

            cdtParallelCond <- .cdtData$Config$parallel

            chunkfile <- sort(unique(don$colInfo$index))
            chunkcalc <- split(chunkfile, ceiling(chunkfile/don$chunkfac))

            do.parChunk <- if(don$chunkfac > length(chunkcalc)) TRUE else FALSE
            do.parCALC <- if(do.parChunk) FALSE else TRUE

            parsL <- doparallel.cond(do.parCALC & (length(chunkcalc) > 10))
            ret <- cdt.foreach(seq_along(chunkcalc), parsL, GUI = TRUE,
                               progress = TRUE, FUN = function(chkj)
            {
                don.data <- readCdtDatasetChunk.sequence(chunkcalc[[chkj]], fcdtdataset, cdtParallelCond, do.par = do.parChunk)
                don.data <- don.data[don$dateInfo$index, , drop = FALSE]
                don.data <- don.data[iyear, , drop = FALSE]

                dat.analys <- lapply(seq_along(index$index), function(jj){
                    if(miss.day[[jj]]) return(rep(NA, ncol(don.data)))
                    cdt.daily.statistics(don.data[index$index[[jj]], , drop = FALSE],
                                         STATS = pars.stat$daily, pars = aggr.pars)
                })
                dat.analys <- do.call(rbind, dat.analys)

                writeCdtDatasetChunk.sequence(dat.analys, chunkcalc[[chkj]], index.out, datadir, cdtParallelCond, do.par = do.parChunk)
                rm(dat.analys, don.data); gc()
            })

            ######################
            x <- index.out$coords$mat$x
            y <- index.out$coords$mat$y
            dx <- ncdim_def("Lon", "degreeE", x)
            dy <- ncdim_def("Lat", "degreeN", y)
            xy.dim <- list(dx, dy)
            nc.grd <- ncvar_def(index.out$varInfo$name, index.out$varInfo$units, xy.dim, -99, index.out$varInfo$longname, "float", compression = 9)

            ######################

            dat.analys <- readCdtDatasetChunk.multi.dates.order(file.index, out.daty, cdtParallelCond)

            for(j in seq_along(out.daty)){
                ncdat.seas <- dat.analys[j, ]
                dim(ncdat.seas) <- c(length(x), length(y))
                ncdat.seas[is.na(ncdat.seas)] <- -99

                filenc <- file.path(ncdfOUT.seas, paste0('Seas_', out.daty[j], '.nc'))
                nc <- nc_create(filenc, nc.grd)
                ncvar_put(nc, nc.grd, ncdat.seas)
                nc_close(nc)
            }
        }

        .cdtData$EnvData$dat.analys <- dat.analys
        .cdtData$EnvData$index.out <- index.out
        .cdtData$EnvData$toAggr <- toAggr

        Insert.Messages.Out(message[['11']], TRUE, "s")
    }else{
        dat.analys <- .cdtData$EnvData$dat.analys
        index.out <- .cdtData$EnvData$index.out
    }

    ################################################

    if(pars.stat$yearly == "mean")
        dat.yearly <- colMeans(dat.analys, na.rm = TRUE)
    if(pars.stat$yearly == "stdev")
        dat.yearly <- matrixStats::colSds(dat.analys, na.rm = TRUE)
    if(pars.stat$yearly == "coefvar")
        dat.yearly <- 100 * matrixStats::colSds(dat.analys, na.rm = TRUE) / colMeans(dat.analys, na.rm = TRUE)
    if(pars.stat$yearly == "proba")
        dat.yearly <- 100 * colSums(!is.na(dat.analys) & dat.analys >= pars.def$proba.thres)/colSums(!is.na(dat.analys))

    ################################################

    out.index.file <- file.path(outDIR, "DailyRainfallAnalysis.rds")

    if(file.exists(out.index.file)){
        outIdx <- readRDS(out.index.file)
        calc.stats <- outIdx$exist.vars.dates
    }else calc.stats <- list()
    
    calc.stats[[stats.directory]]$date <- out.daty
    daty.range <- out.daty[c(1, length(out.daty))]
    daty.range <- cbind(substr(daty.range, 1, 4), substr(daty.range, 10, 13))
    vars.def <- c(pars.def$drywet.day, pars.def$drywet.spell, pars.def$proba.thres)
    calc.stats[[stats.directory]][[pars.stat$yearly]] <- list(year = daty.range, season = stats.season, pars = vars.def)

    last.vars <- c(stats.directory, pars.stat$yearly)

    ################################################

    Insert.Messages.Out(message[['12']], TRUE, "i")

    if(GeneralParameters$data.type == "cdtstation"){
        stn.data <- list(id = don$id, lon = don$lon, lat = don$lat)
        output <- list(params = GeneralParameters, data = stn.data, exist.vars.dates = calc.stats, last = last.vars)

        ##################
        datadir <- file.path(outDIR, 'CDTSTATIONS_STATS')
        dir.create(datadir, showWarnings = FALSE, recursive = TRUE)

        dataOUT <- file.path(outDIR, 'CDTDATASET')
        dir.create(dataOUT, showWarnings = FALSE, recursive = TRUE)

        out.cdt.stats.csv <- file.path(datadir, paste0(stats.directory, '_', pars.stat$yearly, '.csv'))
        out.cdt.stats.rds <- file.path(dataOUT, paste0(stats.directory, '_', pars.stat$yearly, '.rds'))

        ##################

        con <- gzfile(out.cdt.stats.rds, compression = 7)
        open(con, "wb")
        saveRDS(dat.yearly, con)
        close(con)

        ##################
        xhead <- rbind(don$id, don$lon, don$lat)
        chead <- c('ID.STN', 'LON', 'STAT/LAT')
        infohead <- cbind(chead, xhead)

        dat.yearly <- round(dat.yearly, 3)
        dat.yearly[is.na(dat.yearly)] <- -99
        dat.yearly <- rbind(infohead, c(paste0(pars.stat$yearly, ':', stats.season), dat.yearly))
        writeFiles(dat.yearly, out.cdt.stats.csv)
    }

    if(GeneralParameters$data.type == "cdtdataset"){
        output <- list(params = GeneralParameters, exist.vars.dates = calc.stats, last = last.vars)

        ######################
        ncdfOUT <- file.path(outDIR, 'DATA_NetCDF_STATS')
        dir.create(ncdfOUT, showWarnings = FALSE, recursive = TRUE)

        nom <- paste0(pars.stat$yearly, ".", index.out$varInfo$name)
        nom_long <- paste(pars.stat$yearly, index.out$varInfo$longname)

        if(pars.stat$yearly == "mean") unite <- index.out$varInfo$units
        if(pars.stat$yearly == "stdev") unite <- index.out$varInfo$units
        if(pars.stat$yearly == "coefvar") unite <- "%"
        if(pars.stat$yearly == "proba") unite <- "%"

        x <- index.out$coords$mat$x
        y <- index.out$coords$mat$y
        dx <- ncdim_def("Lon", "degreeE", x)
        dy <- ncdim_def("Lat", "degreeN", y)
        xy.dim <- list(dx, dy)
        nc.grd <- ncvar_def(nom, unite, xy.dim, -99, nom_long, "float", compression = 9)

        ######################

        dim(dat.yearly) <- c(length(x), length(y))
        dat.yearly[is.na(dat.yearly)] <- -99

        filenc <- file.path(ncdfOUT, paste0(stats.directory, '_', pars.stat$yearly, '.nc'))
        nc <- nc_create(filenc, nc.grd)
        ncvar_put(nc, nc.grd, dat.yearly)
        nc_close(nc)
    }

    Insert.Messages.Out(message[['13']], TRUE, "s")

    ##################
    con <- gzfile(out.index.file, compression = 9)
    open(con, "wb")
    saveRDS(output, con)
    close(con)

    ##################
    .cdtData$EnvData$output <- output
    .cdtData$EnvData$PathData <- outDIR

    return(0)
}
