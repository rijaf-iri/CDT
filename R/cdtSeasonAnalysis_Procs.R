
compute_RainySeasonData <- function(GeneralParameters){
    message <- .cdtData$EnvData$message

    if(!dir.exists(GeneralParameters$output)){
        Insert.Messages.Out(paste(GeneralParameters$output, message[['5']]), TRUE, 'e')
        return(NULL)
    }

    if(is.na(GeneralParameters$min.frac)){
        Insert.Messages.Out(message[['7']], TRUE, 'e')
        return(NULL)
    }

    if(is.na(GeneralParameters$dryday)){
        Insert.Messages.Out(message[['8']], TRUE, 'e')
        return(NULL)
    }else{
        if(GeneralParameters$dryday == 0) GeneralParameters$dryday <- 0.001
    }

    ##########################################

    onset <- try(readRDS(GeneralParameters$onset), silent = TRUE)
    if(inherits(onset, "try-error")){
        Insert.Messages.Out(paste(message[['9']], GeneralParameters$onset), TRUE, 'e')
        return(NULL)
    }

    cessation <- try(readRDS(GeneralParameters$cessation), silent = TRUE)
    if(inherits(cessation, "try-error")){
        Insert.Messages.Out(paste(message[['9']], GeneralParameters$cessation), TRUE, 'e')
        return(NULL)
    }

    if(onset$params$data.type != cessation$params$data.type){
        Insert.Messages.Out(message[['10']], TRUE, 'e')
        return(NULL)
    }

    ##########################################

    start.mon <- as.numeric(format(onset$start.date[1], "%m"))
    start.day <- as.numeric(format(onset$start.date[1], "%d"))
    index <- cdt.index.DailyYears(onset$ts.date, start.mon, start.day)

    idx.ons <- seq(nrow(index$range.date))
    idx.cess <- sapply(idx.ons, function(j){
        pos <- which(as.Date(index$range.date[j, 1], "%Y%m%d") <= cessation$start.date &
                    as.Date(index$range.date[j, 2], "%Y%m%d") >= cessation$start.date)
        if(length(pos) == 0) pos <- NA
        pos
    })

    if(all(is.na(idx.cess))){
        Insert.Messages.Out(message[['11']], TRUE, 'e')
        return(NULL)
    }

    idx.ons <- idx.ons[!is.na(idx.cess)]
    idx.cess <- idx.cess[!is.na(idx.cess)]
    range.date <- index$range.date[idx.ons, , drop = FALSE]

    ##########################################

    if(onset$params$data.type == "cdtstation"){
        if(!any(onset$data$id %in% cessation$data$id)){
            Insert.Messages.Out(message[['12']], TRUE, 'e')
            return(NULL)
        }

        onset.file <- file.path(dirname(GeneralParameters$onset), 'CDTDATASET', "ONSET.rds")
        cessa.file <- file.path(dirname(GeneralParameters$cessation), 'CDTDATASET', "CESSATION.rds")
        precip.file <- file.path(dirname(GeneralParameters$onset), 'CDTDATASET',"PRECIP.rds")

        onset$onset <- readRDS(onset.file)
        cessation$cessation <- readRDS(cessa.file)
        onset$data$prec <- readRDS(precip.file)

        ##################
        jnx <- match(onset$data$id, cessation$data$id)
        jnx <- jnx[!is.na(jnx)]
        cessation$data$id <- cessation$data$id[jnx]

        stn.id <- cessation$data$id
        stn.lon <- cessation$data$lon[jnx]
        stn.lat <- cessation$data$lat[jnx]
        cessation$cessation <- cessation$cessation[, jnx, drop = FALSE]

        inx <- onset$data$id %in% cessation$data$id
        onset$onset <- onset$onset[, inx, drop = FALSE]

        prec <- onset$data$prec[, inx, drop = FALSE]
        daty <- onset$data$date

        ##################

        onset$start.date <- onset$start.date[idx.ons]
        onset$onset <- onset$onset[idx.ons, , drop = FALSE]
        cessation$cessation <- cessation$cessation[idx.cess, , drop = FALSE]
    }

    ##################

    if(onset$params$data.type == "cdtdataset"){
        onset.file <- file.path(dirname(GeneralParameters$onset), "CDTDATASET", "CDTDATASET.rds")
        cessa.file <- file.path(dirname(GeneralParameters$cessation), "CDTDATASET", "CDTDATASET.rds")

        onset.index <- try(readRDS(onset.file), silent = TRUE)
        if(inherits(onset.index, "try-error")){
            Insert.Messages.Out(paste(message[['9']], onset.file), TRUE, 'e')
            return(NULL)
        }

        cessa.index <- try(readRDS(cessa.file), silent = TRUE)
        if(inherits(cessa.index, "try-error")){
            Insert.Messages.Out(paste(message[['9']], cessa.file), TRUE, 'e')
            return(NULL)
        }

        ##################
        SP1 <- defSpatialPixels(list(lon = onset.index$coords$mat$x, lat = onset.index$coords$mat$y))
        SP2 <- defSpatialPixels(list(lon = cessa.index$coords$mat$x, lat = cessa.index$coords$mat$y))
        if(is.diffSpatialPixelsObj(SP1, SP2, tol = 1e-04)){
            Insert.Messages.Out(message[['13']], TRUE, 'e')
            return(NULL)
        }

        if(onset.index$chunksize != cessa.index$chunksize){
            Insert.Messages.Out(message[['14']], TRUE, 'e')
            return(NULL)
        }

        ##################

        prec <- try(readRDS(onset$params$cdtdataset$prec), silent = TRUE)
        if(inherits(prec, "try-error")){
            Insert.Messages.Out(paste(message[['15']], ":", onset$params$cdtdataset$prec), TRUE, 'e')
            return(NULL)
        }

        SP3 <- defSpatialPixels(list(lon = prec$coords$mat$x, lat = prec$coords$mat$y))
        if(is.diffSpatialPixelsObj(SP1, SP3, tol = 1e-04)){
            Insert.Messages.Out(message[['16']], TRUE, 'e')
            return(NULL)
        }
        rm(SP2, SP3)
    }

    ##########################################

    if(GeneralParameters$seastot$useTotal){
        if(GeneralParameters$seastot$data.type != onset$params$data.type){
            Insert.Messages.Out(message[['17']], TRUE, 'e')
            return(NULL)
        }

        if(GeneralParameters$seastot$data.type == "cdtstation"){
            prec1 <- getStnOpenData(GeneralParameters$seastot$cdtstation$prec)
            if(is.null(prec1)) return(NULL)
            prec1 <- getCDTdataAndDisplayMsg(prec1, GeneralParameters$seastot$Tstep, GeneralParameters$seastot$cdtstation$prec)
            if(is.null(prec1)) return(NULL)

            if(!any(stn.id %in% prec1$id)){
                Insert.Messages.Out(message[['18']], TRUE, 'e')
                return(NULL)
            }

            ##################
            daty1 <- prec1$dates
            ## test if date intersect daty and daty1

            ##################
            jnx <- match(stn.id, prec1$id)
            jnx <- jnx[!is.na(jnx)]
            prec1$id <- prec1$id[jnx]

            inx <- stn.id %in% prec1$id
            prec <- prec[, inx, drop = FALSE]
            onset$onset <- onset$onset[, inx, drop = FALSE]
            cessation$cessation <- cessation$cessation[, inx, drop = FALSE]

            stn.id <- prec1$id
            stn.lon <- prec1$lon[jnx]
            stn.lat <- prec1$lat[jnx]
            prec1 <- prec1$data[, jnx, drop = FALSE]
        }

        ##################

        if(GeneralParameters$seastot$data.type == "cdtdataset"){
            prec1 <- try(readRDS(GeneralParameters$seastot$cdtdataset$prec), silent = TRUE)
            if(inherits(prec1, "try-error")){
                Insert.Messages.Out(paste(message[['9']], GeneralParameters$seastot$cdtdataset$prec), TRUE, 'e')
                return(NULL)
            }

            SP2 <- defSpatialPixels(list(lon = prec1$coords$mat$x, lat = prec1$coords$mat$y))
            if(is.diffSpatialPixelsObj(SP1, SP2, tol = 1e-04)){
                Insert.Messages.Out(message[['19']], TRUE, 'e')
                return(NULL)
            }
            rm(SP1, SP2)

            ##################
            if(onset.index$chunksize != prec1$chunksize){
                Insert.Messages.Out(message[['20']], TRUE, 'e')
                return(NULL)
            }
        }
    }

    ##########################################

    if(onset$params$data.type == "cdtstation"){
        outDIR <- file.path(GeneralParameters$output, "RAINY.SEASON.ANALYSIS_data")
        dir.create(outDIR, showWarnings = FALSE, recursive = TRUE)

        datadir <- file.path(outDIR, 'CDTSTATIONS')
        dir.create(datadir, showWarnings = FALSE, recursive = TRUE)

        dataOUT <- file.path(outDIR, 'CDTDATASET')
        dir.create(dataOUT, showWarnings = FALSE, recursive = TRUE)

        #########################################

        output <- list(params = GeneralParameters, data.type = onset$params$data.type,
                        data = list(id = stn.id, lon = stn.lon, lat = stn.lat, date = daty),
                        start.date = onset$start.date)

        .cdtData$EnvData$output <- output
        .cdtData$EnvData$PathPicsa <- outDIR

        file.out.rds <- gzfile(file.path(outDIR, "RainySeasonAnalysis.rds"), compression = 7)
        file.daily.precip <- gzfile(file.path(dataOUT, 'Daily_precip.rds'), compression = 7)
        saveRDS(output, file.out.rds)
        saveRDS(prec, file.daily.precip)

        close(file.out.rds)
        close(file.daily.precip)

        ###################
        coldaty <- format(onset$start.date, "%Y%m%d")
        xhead <- rbind(stn.id, stn.lon, stn.lat)
        chead <- c('ID.STN', 'LON', 'START.DATE/LAT')
        infohead <- cbind(chead, xhead)

        #########################################

        ## index season
        dimONSET <- dim(onset$onset)
        ONSET <- format(onset$onset, "%Y%m%d")
        dim(ONSET) <- dimONSET
        CESSAT <- format(cessation$cessation, "%Y%m%d")
        dim(CESSAT) <- dimONSET

        indexDaily <- lapply(seq_along(onset$start.date), function(j){
                        cdt.index.flexseason(ONSET[j, ], CESSAT[j, ], daty, "daily")
                    })

        nb.init.daily <- lapply(indexDaily, "[[", "nb0")
        nb.init.daily <- do.call(rbind, nb.init.daily)
        nb.nonNA.daily <- lapply(indexDaily, "[[", "nba")
        nb.nonNA.daily <- do.call(rbind, nb.nonNA.daily)
        indexDaily <- lapply(indexDaily, "[[", "index")
        indexDaily <- do.call(rbind, indexDaily)

        if(GeneralParameters$seastot$useTotal){
            indexSeason <- lapply(seq_along(onset$start.date), function(j){
                            cdt.index.flexseason(ONSET[j, ], CESSAT[j, ], daty1, GeneralParameters$seastot$Tstep)
                        })

            nb.init.seas <- lapply(indexSeason, "[[", "nb0")
            nb.init.seas <- do.call(rbind, nb.init.seas)
            nb.nonNA.seas <- lapply(indexSeason, "[[", "nba")
            nb.nonNA.seas <- do.call(rbind, nb.nonNA.seas)
            indexSeason <- lapply(indexSeason, "[[", "index")
            indexSeason <- do.call(rbind, indexSeason)
        }

        #########################################

        ONSET[is.na(ONSET)] <- .cdtData$Config$missval
        ONSET <- rbind(infohead, cbind(coldaty, ONSET))
        writeFiles(ONSET, file.path(datadir, "Onset_date.csv"))
        rm(ONSET)

        CESSAT[is.na(CESSAT)] <- .cdtData$Config$missval
        CESSAT <- rbind(infohead, cbind(coldaty, CESSAT))
        writeFiles(CESSAT, file.path(datadir, "Cessation_date.csv"))
        rm(CESSAT); gc()

        #########################################

        ## season length
        SEASON.LENGTH <- cessation$cessation - onset$onset
        SEASON.LENGTH[SEASON.LENGTH < 0] <- NA

        ######
        file.seas.len <- gzfile(file.path(dataOUT, 'Season_length.rds'), compression = 7)
        saveRDS(SEASON.LENGTH, file.seas.len)
        close(file.seas.len)

        SEASON.LENGTH[is.na(SEASON.LENGTH)] <- .cdtData$Config$missval
        SEASON.LENGTH <- rbind(infohead, cbind(coldaty, SEASON.LENGTH))
        writeFiles(SEASON.LENGTH, file.path(datadir, "Season_length.csv"))
        rm(SEASON.LENGTH)

        ###################
        ## season onset
        ONSET <- onset$onset - onset$start.date

        ######
        file.onset.rds <- gzfile(file.path(dataOUT, 'Onset_days.rds'), compression = 7)
        saveRDS(ONSET, file.onset.rds)
        close(file.onset.rds)

        ONSET[is.na(ONSET)] <- .cdtData$Config$missval
        ONSET <- rbind(infohead, cbind(coldaty, ONSET))
        writeFiles(ONSET, file.path(datadir, "Onset_days.csv"))
        rm(ONSET)

        ###################
        ## season cessation
        CESSAT <- cessation$cessation - onset$start.date

        ######
        file.cessation.rds <- gzfile(file.path(dataOUT, 'Cessation_days.rds'), compression = 7)
        saveRDS(CESSAT, file.cessation.rds)
        close(file.cessation.rds)

        CESSAT[is.na(CESSAT)] <- .cdtData$Config$missval
        CESSAT <- rbind(infohead, cbind(coldaty, CESSAT))
        writeFiles(CESSAT, file.path(datadir, "Cessation_days.csv"))
        rm(CESSAT); gc()

        #########################################

        datDaily <- lapply(seq(ncol(indexDaily)), function(j){
            idx <- indexDaily[, j]
            out <- rep(NA, length(idx))
            rout <- as.list(out)
            ina <- nb.nonNA.daily[, j] < 2
            if(all(ina)) return(list(nona = out, rr = rout))
            idx <- idx[!ina]
            rr <- prec[unlist(idx), j]
            out[!ina] <- sapply(utils::relist(is.na(rr), idx), sum)
            rout[!ina] <- utils::relist(rr, idx)
            nona <- 1 - (out / nb.init.daily[, j])
            nona[is.na(nona)] <- 0
            list(nona = nona, rr = rout)
        })
        FracDaily <- do.call(cbind, lapply(datDaily, '[[', 'nona'))
        FracDaily[FracDaily < GeneralParameters$min.frac] <- NA
        PREC <- do.call(cbind, lapply(datDaily, '[[', 'rr'))
        PREC[is.na(FracDaily)] <- NA
        rm(datDaily, prec)

        if(GeneralParameters$seastot$useTotal){
            datSeas <- lapply(seq(ncol(indexSeason)), function(j){
                idx <- indexSeason[, j]
                out <- rep(NA, length(idx))
                rout <- as.list(out)
                ina <- nb.nonNA.seas[, j] < 2
                if(all(ina)) return(list(nona = out, rr = rout))
                idx <- idx[!ina]
                rr <- prec1[unlist(idx), j]
                out[!ina] <- sapply(utils::relist(is.na(rr), idx), sum)
                rout[!ina] <- utils::relist(rr, idx)
                nona <- 1 - (out / nb.init.seas[, j])
                nona[is.na(nona)] <- 0
                list(nona = nona, rr = rout)
            })
            FracSeas <- do.call(cbind, lapply(datSeas, '[[', 'nona'))
            FracSeas[FracSeas < GeneralParameters$min.frac] <- NA
            PREC1 <- do.call(cbind, lapply(datSeas, '[[', 'rr'))
            PREC1[is.na(FracSeas)] <- NA
            rm(datSeas, FracSeas, prec1)
        }

        ###################

        ## seasonal total
        RAINTOTAL <- sapply(if(GeneralParameters$seastot$useTotal) PREC1 else PREC, sum, na.rm = TRUE)
        dim(RAINTOTAL) <- dim(PREC)
        RAINTOTAL[is.na(FracDaily)] <- NA

        ######
        file.raintot.rds <- gzfile(file.path(dataOUT, 'Seasonal_rain_amount.rds'), compression = 7)
        saveRDS(RAINTOTAL, file.raintot.rds)
        close(file.raintot.rds)

        RAINTOTAL[is.na(RAINTOTAL)] <- .cdtData$Config$missval
        RAINTOTAL <- rbind(infohead, cbind(coldaty, RAINTOTAL))
        writeFiles(RAINTOTAL, file.path(datadir, "Seasonal_rain_amount.csv"))
        rm(RAINTOTAL); gc()

        if(GeneralParameters$seastot$useTotal) rm(PREC1)

        ###################
        ## number of rainy day
        NBRAINDAYS <- sapply(PREC, function(x) sum(!is.na(x) & x >= GeneralParameters$dryday))
        dim(NBRAINDAYS) <- dim(PREC)
        NBRAINDAYS[is.na(FracDaily)] <- NA

        ######
        file.ndday.rds <- gzfile(file.path(dataOUT, 'Number_rainy_day.rds'), compression = 7)
        saveRDS(NBRAINDAYS, file.ndday.rds)
        close(file.ndday.rds)

        NBRAINDAYS[is.na(NBRAINDAYS)] <- .cdtData$Config$missval
        NBRAINDAYS <- rbind(infohead, cbind(coldaty, NBRAINDAYS))
        writeFiles(NBRAINDAYS, file.path(datadir, "Number_rainy_day.csv"))
        rm(NBRAINDAYS)

        ###################
        ## maximum 24 hour
        RAINMAX24H <- sapply(PREC, function(x) if(all(is.na(x))) NA else max(x, na.rm = TRUE))
        dim(RAINMAX24H) <- dim(PREC)
        RAINMAX24H[is.na(FracDaily)] <- NA

        ######
        file.max24h.rds <- gzfile(file.path(dataOUT, 'Maximum_rain_daily.rds'), compression = 7)
        saveRDS(RAINMAX24H, file.max24h.rds)
        close(file.max24h.rds)

        RAINMAX24H[is.na(RAINMAX24H)] <- .cdtData$Config$missval
        RAINMAX24H <- rbind(infohead, cbind(coldaty, RAINMAX24H))
        writeFiles(RAINMAX24H, file.path(datadir, "Maximum_rain_daily.csv"))
        rm(RAINMAX24H); gc()

        ###################
        ## Quantile 95th, number, total
        xtmp <- lapply(seq(ncol(PREC)), function(j){
            rr <- PREC[, j]
            rrs <- unlist(rr)
            q95th <- quantile8(rrs[rrs >= GeneralParameters$dryday], probs = 0.95)
            rrs <- utils::relist(!is.na(rrs) & rrs >= q95th, rr)
            nbQ95th <- sapply(rrs, sum)
            TotQ95th <- mapply(function(x, y) sum(x[y]), x = rr, y = rrs)
            list(q95 = q95th, nbq95 = nbQ95th, totq95 = TotQ95th)
        })

        Q95th <- round(do.call(cbind, lapply(xtmp, '[[', 'q95')), 1)
        NbQ95th <- do.call(cbind, lapply(xtmp, '[[', 'nbq95'))
        NbQ95th[is.na(FracDaily)] <- NA
        TotalQ95th <- do.call(cbind, lapply(xtmp, '[[', 'totq95'))
        TotalQ95th[is.na(FracDaily)] <- NA
        rm(xtmp)

        ######
        file.percent.rds <- gzfile(file.path(dataOUT, 'Percentile_95th.rds'), compression = 7)
        saveRDS(Q95th, file.percent.rds)
        close(file.percent.rds)

        Q95th[is.na(Q95th)] <- .cdtData$Config$missval
        Q95th <- rbind(infohead, cbind("Perc95th", Q95th))
        writeFiles(Q95th, file.path(datadir, "Percentile_95th.csv"))
        rm(Q95th)

        ######
        file.sup95.rds <- gzfile(file.path(dataOUT, 'Number_day_above_Perc95th.rds'), compression = 7)
        saveRDS(NbQ95th, file.sup95.rds)
        close(file.sup95.rds)

        NbQ95th[is.na(NbQ95th)] <- .cdtData$Config$missval
        NbQ95th <- rbind(infohead, cbind(coldaty, NbQ95th))
        writeFiles(NbQ95th, file.path(datadir, "Number_day_above_Perc95th.csv"))
        rm(NbQ95th)

        ######
        file.tot95.rds <- gzfile(file.path(dataOUT, 'Total_rain_above_Perc95th.rds'), compression = 7)
        saveRDS(TotalQ95th, file.tot95.rds)
        close(file.tot95.rds)

        TotalQ95th[is.na(TotalQ95th)] <- .cdtData$Config$missval
        TotalQ95th <- rbind(infohead, cbind(coldaty, TotalQ95th))
        writeFiles(TotalQ95th, file.path(datadir, "Total_rain_above_Perc95th.csv"))
        rm(TotalQ95th); gc()

        ###################
        ## Dry Spell
        DRYSPELLS <- lapply(seq(ncol(PREC)), function(j){
            rr <- PREC[, j]
            rrs <- unlist(rr)
            rrs <- utils::relist(!is.na(rrs) & rrs < GeneralParameters$dryday, rr)
            rr <- lapply(rrs, rle)
            rr <- lapply(rr, function(x) x$lengths[x$values])
            rr[sapply(rr, length) == 0] <- 0
            rr
        })
        DRYSPELLS <- do.call(cbind, DRYSPELLS)
        DRYSPELLS[is.na(FracDaily)] <- NA

        ###################
        file.dryspell.rds <- gzfile(file.path(dataOUT, 'Dry_Spells.rds'), compression = 7)
        saveRDS(DRYSPELLS, file.dryspell.rds)
        close(file.dryspell.rds)

        #########
        DRYSPELLmax <- sapply(DRYSPELLS, function(x) if(all(is.na(x))) NA else max(x, na.rm = TRUE))
        dim(DRYSPELLmax) <- dim(PREC)
        DRYSPELLmax[is.na(FracDaily)] <- NA

        file.dryspellmax.rds <- gzfile(file.path(dataOUT, 'Longest_dry_spell.rds'), compression = 7)
        saveRDS(DRYSPELLmax, file.dryspellmax.rds)
        close(file.dryspellmax.rds)

        DRYSPELLmax[is.na(DRYSPELLmax)] <- .cdtData$Config$missval
        DRYSPELLmax <- rbind(infohead, cbind(coldaty, DRYSPELLmax))
        writeFiles(DRYSPELLmax, file.path(datadir, "Longest_dry_spell.csv"))
        rm(DRYSPELLmax)

        #########
        DRYSPELL5 <- sapply(DRYSPELLS, function(x) sum(!is.na(x) & x >= 5))
        dim(DRYSPELL5) <- dim(PREC)
        DRYSPELL5[is.na(FracDaily)] <- NA

        DRYSPELL5[is.na(DRYSPELL5)] <- .cdtData$Config$missval
        DRYSPELL5 <- rbind(infohead, cbind(coldaty, DRYSPELL5))
        writeFiles(DRYSPELL5, file.path(datadir, "Dry_Spell_5days.csv"))
        rm(DRYSPELL5)

        #########
        DRYSPELL7 <- sapply(DRYSPELLS, function(x) sum(!is.na(x) & x >= 7))
        dim(DRYSPELL7) <- dim(PREC)
        DRYSPELL7[is.na(FracDaily)] <- NA

        DRYSPELL7[is.na(DRYSPELL7)] <- .cdtData$Config$missval
        DRYSPELL7 <- rbind(infohead, cbind(coldaty, DRYSPELL7))
        writeFiles(DRYSPELL7, file.path(datadir, "Dry_Spell_7days.csv"))
        rm(DRYSPELL7)

        #########
        DRYSPELL10 <- sapply(DRYSPELLS, function(x) sum(!is.na(x) & x >= 10))
        dim(DRYSPELL10) <- dim(PREC)
        DRYSPELL10[is.na(FracDaily)] <- NA

        DRYSPELL10[is.na(DRYSPELL10)] <- .cdtData$Config$missval
        DRYSPELL10 <- rbind(infohead, cbind(coldaty, DRYSPELL10))
        writeFiles(DRYSPELL10, file.path(datadir, "Dry_Spell_10days.csv"))
        rm(DRYSPELL10)

        #########
        DRYSPELL15 <- sapply(DRYSPELLS, function(x) sum(!is.na(x) & x >= 15))
        dim(DRYSPELL15) <- dim(PREC)
        DRYSPELL15[is.na(FracDaily)] <- NA

        DRYSPELL15[is.na(DRYSPELL15)] <- .cdtData$Config$missval
        DRYSPELL15 <- rbind(infohead, cbind(coldaty, DRYSPELL15))
        writeFiles(DRYSPELL15, file.path(datadir, "Dry_Spell_15days.csv"))
        rm(DRYSPELL15)

        rm(PREC, DRYSPELLS, FracDaily); gc()
    }

    ##########################################

    if(onset$params$data.type == "cdtdataset"){
        outDIR <- file.path(GeneralParameters$output, "RAINY.SEASON.ANALYSIS_data")
        dir.create(outDIR, showWarnings = FALSE, recursive = TRUE)

        ncdfOUT <- file.path(outDIR, 'DATA_NetCDF')
        dir.create(ncdfOUT, showWarnings = FALSE, recursive = TRUE)

        dataOUT <- file.path(outDIR, 'CDTDATASET')
        dir.create(dataOUT, showWarnings = FALSE, recursive = TRUE)

        datafileIdx <- file.path(dataOUT, 'CDTDATASET.rds')

        Season_length.Dir <- file.path(dataOUT, 'Season_length')
        dir.create(Season_length.Dir, showWarnings = FALSE, recursive = TRUE)
        Onset_days.Dir <- file.path(dataOUT, 'Onset_days')
        dir.create(Onset_days.Dir, showWarnings = FALSE, recursive = TRUE)
        Cessation_days.Dir <- file.path(dataOUT, 'Cessation_days')
        dir.create(Cessation_days.Dir, showWarnings = FALSE, recursive = TRUE)
        Seasonal_rain_amount.Dir <- file.path(dataOUT, 'Seasonal_rain_amount')
        dir.create(Seasonal_rain_amount.Dir, showWarnings = FALSE, recursive = TRUE)
        Number_rainy_day.Dir <- file.path(dataOUT, 'Number_rainy_day')
        dir.create(Number_rainy_day.Dir, showWarnings = FALSE, recursive = TRUE)
        Maximum_rain_daily.Dir <- file.path(dataOUT, 'Maximum_rain_daily')
        dir.create(Maximum_rain_daily.Dir, showWarnings = FALSE, recursive = TRUE)
        Percentile_95th.Dir <- file.path(dataOUT, 'Percentile_95th')
        dir.create(Percentile_95th.Dir, showWarnings = FALSE, recursive = TRUE)
        Number_day_above_Perc95th.Dir <- file.path(dataOUT, 'Number_day_above_Perc95th')
        dir.create(Number_day_above_Perc95th.Dir, showWarnings = FALSE, recursive = TRUE)
        Total_rain_above_Perc95th.Dir <- file.path(dataOUT, 'Total_rain_above_Perc95th')
        dir.create(Total_rain_above_Perc95th.Dir, showWarnings = FALSE, recursive = TRUE)
        Dry_Spells.Dir <- file.path(dataOUT, 'Dry_Spells')
        dir.create(Dry_Spells.Dir, showWarnings = FALSE, recursive = TRUE)

        #########################################

        onset$start.date <- onset$start.date[idx.ons]

        index.out <- onset.index
        index.out$varInfo$name <- "picsa"
        index.out$varInfo$units <- ""
        index.out$varInfo$longname <- "All seasonal data"

        index.out$dateInfo$date <- format(onset$start.date, "%Y%m%d")
        index.out$dateInfo$index <- seq_along(onset$start.date)

        con <- gzfile(datafileIdx, compression = 6)
        open(con, "wb")
        saveRDS(index.out, con)
        close(con)

        #################
        output <- list(params = GeneralParameters, data.type = onset$params$data.type,
                        daily.precip = onset$params$cdtdataset$prec, 
                        start.date = onset$start.date, range.date = range.date)

        .cdtData$EnvData$output <- output
        .cdtData$EnvData$PathPicsa <- outDIR

        file.index.rainyseas <- gzfile(file.path(outDIR, "RainySeasonAnalysis.rds"), compression = 7)
        saveRDS(output, file.index.rainyseas)
        close(file.index.rainyseas)

        #########################################

        chunkfile <- sort(unique(onset.index$colInfo$index))
        chunkcalc <- split(chunkfile, ceiling(chunkfile / onset.index$chunkfac))

        do.parChunk <- if(onset.index$chunkfac > length(chunkcalc)) TRUE else FALSE
        do.parCALC <- if(do.parChunk) FALSE else TRUE

        GeneralParameters <- GeneralParameters
        cdtParallelCond <- .cdtData$Config$parallel

        parsL <- doparallel.cond(do.parCALC & (length(chunkcalc) > 10))
        ret <- cdt.foreach(seq_along(chunkcalc), parsL, GUI = TRUE,
                           progress = TRUE, FUN = function(jj)
        {
            ons.data <- readCdtDatasetChunk.sequence(chunkcalc[[jj]], onset.file, cdtParallelCond, do.par = do.parChunk)
            ons.data <- ons.data[idx.ons, , drop = FALSE]

            cess.data <- readCdtDatasetChunk.sequence(chunkcalc[[jj]], cessa.file, cdtParallelCond, do.par = do.parChunk)
            cess.data <- cess.data[idx.cess, , drop = FALSE]

            #########################################

            ## index season
            dimONSET <- dim(ons.data)
            ONSET <- format(as.Date(ons.data, origin = "1970-1-1"), "%Y%m%d")
            dim(ONSET) <- dimONSET
            CESSAT <- format(as.Date(cess.data, origin = "1970-1-1"), "%Y%m%d")
            dim(CESSAT) <- dimONSET

            indexDaily <- lapply(seq_along(onset$start.date), function(j){
                            cdt.index.flexseason(ONSET[j, ], CESSAT[j, ], prec$dateInfo$date, "daily")
                        })

            nb.init.daily <- lapply(indexDaily, "[[", "nb0")
            nb.init.daily <- do.call(rbind, nb.init.daily)
            nb.nonNA.daily <- lapply(indexDaily, "[[", "nba")
            nb.nonNA.daily <- do.call(rbind, nb.nonNA.daily)
            indexDaily <- lapply(indexDaily, "[[", "index")
            indexDaily <- do.call(rbind, indexDaily)

            if(GeneralParameters$seastot$useTotal){
                indexSeason <- lapply(seq_along(onset$start.date), function(j){
                                cdt.index.flexseason(ONSET[j, ], CESSAT[j, ], prec1$dateInfo$date, GeneralParameters$seastot$Tstep)
                            })

                nb.init.seas <- lapply(indexSeason, "[[", "nb0")
                nb.init.seas <- do.call(rbind, nb.init.seas)
                nb.nonNA.seas <- lapply(indexSeason, "[[", "nba")
                nb.nonNA.seas <- do.call(rbind, nb.nonNA.seas)
                indexSeason <- lapply(indexSeason, "[[", "index")
                indexSeason <- do.call(rbind, indexSeason)
            }

            rm(ONSET, CESSAT); gc()

            #########################################

            ## season length
            SEASON.LENGTH <- cess.data - ons.data

            ######
            writeCdtDatasetChunk.sequence(SEASON.LENGTH, chunkcalc[[jj]], index.out, Season_length.Dir, cdtParallelCond, do.par = do.parChunk)
            rm(SEASON.LENGTH); gc()

            ###################
            ## season onset
            ONSET <- ons.data - as.integer(onset$start.date)

            ######
            writeCdtDatasetChunk.sequence(ONSET, chunkcalc[[jj]], index.out, Onset_days.Dir, cdtParallelCond, do.par = do.parChunk)
            rm(ONSET); gc()

            ###################
            ## season cessation
            CESSAT <- cess.data - as.integer(onset$start.date)

            ######
            writeCdtDatasetChunk.sequence(CESSAT, chunkcalc[[jj]], index.out, Cessation_days.Dir, cdtParallelCond, do.par = do.parChunk)
            rm(CESSAT)

            rm(cess.data, ons.data); gc()

            #########################################

            prec.data <- readCdtDatasetChunk.sequence(chunkcalc[[jj]], onset$params$cdtdataset$prec, cdtParallelCond, do.par = do.parChunk)
            prec.data <- prec.data[prec$dateInfo$index, , drop = FALSE]

            datDaily <- lapply(seq(ncol(indexDaily)), function(j){
                idx <- indexDaily[, j]
                out <- rep(NA, length(idx))
                rout <- as.list(out)
                ina <- nb.nonNA.daily[, j] < 2
                if(all(ina)) return(list(nona = out, rr = rout))
                idx <- idx[!ina]
                rr <- prec.data[unlist(idx), j]
                out[!ina] <- sapply(utils::relist(is.na(rr), idx), sum)
                rout[!ina] <- utils::relist(rr, idx)
                nona <- 1 - (out / nb.init.daily[, j])
                nona[is.na(nona)] <- 0
                list(nona = nona, rr = rout)
            })
            FracDaily <- do.call(cbind, lapply(datDaily, '[[', 'nona'))
            FracDaily[FracDaily < GeneralParameters$min.frac] <- NA
            PREC <- do.call(cbind, lapply(datDaily, '[[', 'rr'))
            PREC[is.na(FracDaily)] <- NA
            rm(datDaily, prec.data); gc()

            if(GeneralParameters$seastot$useTotal){
                prec1.data <- readCdtDatasetChunk.sequence(chunkcalc[[jj]], GeneralParameters$seastot$cdtdataset$prec, cdtParallelCond, do.par = do.parChunk)
                prec1.data <- prec1.data[prec1$dateInfo$index, , drop = FALSE]

                datSeas <- lapply(seq(ncol(indexSeason)), function(j){
                    idx <- indexSeason[, j]
                    out <- rep(NA, length(idx))
                    rout <- as.list(out)
                    ina <- nb.nonNA.seas[, j] < 2
                    if(all(ina)) return(list(nona = out, rr = rout))
                    idx <- idx[!ina]
                    rr <- prec1.data[unlist(idx), j]
                    out[!ina] <- sapply(utils::relist(is.na(rr), idx), sum)
                    rout[!ina] <- utils::relist(rr, idx)
                    nona <- 1 - (out / nb.init.seas[, j])
                    nona[is.na(nona)] <- 0
                    list(nona = nona, rr = rout)
                })
                FracSeas <- do.call(cbind, lapply(datSeas, '[[', 'nona'))
                FracSeas[FracSeas < GeneralParameters$min.frac] <- NA
                PREC1 <- do.call(cbind, lapply(datSeas, '[[', 'rr'))
                PREC1[is.na(FracSeas)] <- NA
                rm(datSeas, FracSeas, prec1.data); gc()
            }

            ###################

            ## seasonal total
            RAINTOTAL <- sapply(if(GeneralParameters$seastot$useTotal) PREC1 else PREC, sum, na.rm = TRUE)
            dim(RAINTOTAL) <- dim(PREC)
            RAINTOTAL[is.na(FracDaily)] <- NA

            ######
            writeCdtDatasetChunk.sequence(RAINTOTAL, chunkcalc[[jj]], index.out, Seasonal_rain_amount.Dir, cdtParallelCond, do.par = do.parChunk)
            rm(RAINTOTAL); gc()
            if(GeneralParameters$seastot$useTotal) rm(PREC1)

            ###################
            ## number of rainy day
            NBRAINDAYS <- sapply(PREC, function(x) sum(!is.na(x) & x >= GeneralParameters$dryday))
            dim(NBRAINDAYS) <- dim(PREC)
            NBRAINDAYS[is.na(FracDaily)] <- NA

            ######
            writeCdtDatasetChunk.sequence(NBRAINDAYS, chunkcalc[[jj]], index.out, Number_rainy_day.Dir, cdtParallelCond, do.par = do.parChunk)
            rm(NBRAINDAYS); gc()

            ###################
            ## maximum 24 hour
            RAINMAX24H <- sapply(PREC, function(x) if(all(is.na(x))) NA else max(x, na.rm = TRUE))
            dim(RAINMAX24H) <- dim(PREC)
            RAINMAX24H[is.na(FracDaily)] <- NA

            ######
            writeCdtDatasetChunk.sequence(RAINMAX24H, chunkcalc[[jj]], index.out, Maximum_rain_daily.Dir, cdtParallelCond, do.par = do.parChunk)
            rm(RAINMAX24H); gc()

            ###################
            ## Quantile 95th, number, total
            xtmp <- lapply(seq(ncol(PREC)), function(j){
                rr <- PREC[, j]
                rrs <- unlist(rr)
                q95th <- quantile8(rrs[rrs >= GeneralParameters$dryday], probs = 0.95)
                rrs <- utils::relist(!is.na(rrs) & rrs >= q95th, rr)
                nbQ95th <- sapply(rrs, sum)
                TotQ95th <- mapply(function(x, y) sum(x[y]), x = rr, y = rrs)
                list(q95 = q95th, nbq95 = nbQ95th, totq95 = TotQ95th)
            })

            Q95th <- round(do.call(cbind, lapply(xtmp, '[[', 'q95')), 1)
            NbQ95th <- do.call(cbind, lapply(xtmp, '[[', 'nbq95'))
            NbQ95th[is.na(FracDaily)] <- NA
            TotalQ95th <- do.call(cbind, lapply(xtmp, '[[', 'totq95'))
            TotalQ95th[is.na(FracDaily)] <- NA
            rm(xtmp); gc()

            ######
            writeCdtDatasetChunk.sequence(Q95th, chunkcalc[[jj]], index.out, Percentile_95th.Dir, cdtParallelCond, do.par = do.parChunk)
            rm(Q95th)

            ######
            writeCdtDatasetChunk.sequence(NbQ95th, chunkcalc[[jj]], index.out, Number_day_above_Perc95th.Dir, cdtParallelCond, do.par = do.parChunk)
            rm(NbQ95th); gc()

            ######
            writeCdtDatasetChunk.sequence(TotalQ95th, chunkcalc[[jj]], index.out, Total_rain_above_Perc95th.Dir, cdtParallelCond, do.par = do.parChunk)
            rm(TotalQ95th); gc()

            ###################
            ## Dry Spell
            DRYSPELLS <- lapply(seq(ncol(PREC)), function(j){
                rr <- PREC[, j]
                rrs <- unlist(rr)
                rrs <- utils::relist(!is.na(rrs) & rrs < GeneralParameters$dryday, rr)
                rr <- lapply(rrs, rle)
                rr <- lapply(rr, function(x) x$lengths[x$values])
                rr[sapply(rr, length) == 0] <- 0
                rr
            })
            DRYSPELLS <- do.call(cbind, DRYSPELLS)
            DRYSPELLS[is.na(FracDaily)] <- NA

            #######
            writeCdtDatasetChunk.sequence(DRYSPELLS, chunkcalc[[jj]], index.out, Dry_Spells.Dir, cdtParallelCond, do.par = do.parChunk)

            rm(PREC, DRYSPELLS, FracDaily); gc()

            return(0)
        })

        rm(prec)

        ##########################################

        Insert.Messages.Out(message[['21']], TRUE, 'i')

        Season_length.Nc <- file.path(ncdfOUT, 'Season_length')
        dir.create(Season_length.Nc, showWarnings = FALSE, recursive = TRUE)
        Onset_days.Nc <- file.path(ncdfOUT, 'Onset_days')
        dir.create(Onset_days.Nc, showWarnings = FALSE, recursive = TRUE)
        Cessation_days.Nc <- file.path(ncdfOUT, 'Cessation_days')
        dir.create(Cessation_days.Nc, showWarnings = FALSE, recursive = TRUE)
        Seasonal_rain_amount.Nc <- file.path(ncdfOUT, 'Seasonal_rain_amount')
        dir.create(Seasonal_rain_amount.Nc, showWarnings = FALSE, recursive = TRUE)
        Number_rainy_day.Nc <- file.path(ncdfOUT, 'Number_rainy_day')
        dir.create(Number_rainy_day.Nc, showWarnings = FALSE, recursive = TRUE)
        Maximum_rain_daily.Nc <- file.path(ncdfOUT, 'Maximum_rain_daily')
        dir.create(Maximum_rain_daily.Nc, showWarnings = FALSE, recursive = TRUE)
        Number_day_above_Perc95th.Nc <- file.path(ncdfOUT, 'Number_day_above_Perc95th')
        dir.create(Number_day_above_Perc95th.Nc, showWarnings = FALSE, recursive = TRUE)
        Total_rain_above_Perc95th.Nc <- file.path(ncdfOUT, 'Total_rain_above_Perc95th')
        dir.create(Total_rain_above_Perc95th.Nc, showWarnings = FALSE, recursive = TRUE)

        Dry_Spell_7days.Nc <- file.path(ncdfOUT, 'Dry_Spell_7days')
        dir.create(Dry_Spell_7days.Nc, showWarnings = FALSE, recursive = TRUE)
        Longest_dry_spell.Nc <- file.path(ncdfOUT, 'Longest_dry_spell')
        dir.create(Longest_dry_spell.Nc, showWarnings = FALSE, recursive = TRUE)

        Percentile_95th.Nc <- file.path(ncdfOUT, 'Percentile_95th')
        dir.create(Percentile_95th.Nc, showWarnings = FALSE, recursive = TRUE)

        ##########################################

        start.date <- format(onset$start.date, "%Y%m%d")
        chunkdate <- split(start.date, ceiling(seq_along(start.date) / 10))

        x <- index.out$coords$mat$x
        y <- index.out$coords$mat$y
        nx <- length(x)
        ny <- length(y)
        dx <- ncdf4::ncdim_def("Lon", "degreeE", x)
        dy <- ncdf4::ncdim_def("Lat", "degreeN", y)
        xy.dim <- list(dx, dy)

        ######################
        q95 <- lapply(chunkfile, function(j){
            file.rds <- file.path(Percentile_95th.Dir, paste0(j, ".rds"))
            readRDS(file.rds)
        })
        q95 <- do.call(cbind, q95)
        q95 <- q95[, index.out$colInfo$order]
        q95[is.na(q95)] <- -99
        nc.grd <- ncdf4::ncvar_def("q95", "mm", xy.dim, -99, "95th percentile", "short", shuffle = TRUE, compression = 9)
        filenc <- file.path(Percentile_95th.Nc, "data_95th_perc.nc")
        nc <- ncdf4::nc_create(filenc, nc.grd)
        ncdf4::ncvar_put(nc, nc.grd, matrix(q95, nx, ny))
        ncdf4::nc_close(nc)

        ######################
        ret <- lapply(chunkdate, function(dates){

            seasL <- readCdtDatasetChunk.sepdir.dates.order(datafileIdx, Season_length.Dir, dates, cdtParallelCond)
            seasL[is.na(seasL)] <- -99
            nc.grd <- ncdf4::ncvar_def("seas.len", "days", xy.dim, -99, "Length of the rainy season", "short", shuffle = TRUE, compression = 9)
            for(j in seq_along(dates)){
                filenc <- file.path(Season_length.Nc, paste0("data_", dates[j], ".nc"))
                nc <- ncdf4::nc_create(filenc, nc.grd)
                ncdf4::ncvar_put(nc, nc.grd, matrix(seasL[j, ], nx, ny))
                ncdf4::nc_close(nc)
            }
            rm(seasL)

            onsetD <- readCdtDatasetChunk.sepdir.dates.order(datafileIdx, Onset_days.Dir, dates, cdtParallelCond)
            onsetD[is.na(onsetD)] <- -99
            for(j in seq_along(dates)){
                units <- paste("days since", dates[j])
                nc.grd <- ncdf4::ncvar_def("onset", units, xy.dim, -99, "Starting dates of the rainy season", "short", shuffle = TRUE, compression = 9)
                filenc <- file.path(Onset_days.Nc, paste0("data_", dates[j], ".nc"))
                nc <- ncdf4::nc_create(filenc, nc.grd)
                ncdf4::ncvar_put(nc, nc.grd, matrix(onsetD[j, ], nx, ny))
                ncdf4::nc_close(nc)
            }
            rm(onsetD)

            cessaD <- readCdtDatasetChunk.sepdir.dates.order(datafileIdx, Cessation_days.Dir, dates, cdtParallelCond)
            cessaD[is.na(cessaD)] <- -99
            for(j in seq_along(dates)){
                units <- paste("days since", dates[j])
                nc.grd <- ncdf4::ncvar_def("cessation", units, xy.dim, -99, "Ending dates of the rainy season", "short", shuffle = TRUE, compression = 9)
                filenc <- file.path(Cessation_days.Nc, paste0("data_", dates[j], ".nc"))
                nc <- ncdf4::nc_create(filenc, nc.grd)
                ncdf4::ncvar_put(nc, nc.grd, matrix(cessaD[j, ], nx, ny))
                ncdf4::nc_close(nc)
            }
            rm(cessaD)

            seasTot <- readCdtDatasetChunk.sepdir.dates.order(datafileIdx, Seasonal_rain_amount.Dir, dates, cdtParallelCond)
            seasTot[is.na(seasTot)] <- -99
            nc.grd <- ncdf4::ncvar_def("seas.precip", "mm", xy.dim, -99, "Seasonal rainfall amounts", "short", shuffle = TRUE, compression = 9)
            for(j in seq_along(dates)){
                filenc <- file.path(Seasonal_rain_amount.Nc, paste0("data_", dates[j], ".nc"))
                nc <- ncdf4::nc_create(filenc, nc.grd)
                ncdf4::ncvar_put(nc, nc.grd, matrix(seasTot[j, ], nx, ny))
                ncdf4::nc_close(nc)
            }
            rm(seasTot)

            nbDay <- readCdtDatasetChunk.sepdir.dates.order(datafileIdx, Number_rainy_day.Dir, dates, cdtParallelCond)
            nbDay[is.na(nbDay)] <- -99
            nc.grd <- ncdf4::ncvar_def("nb.rain", "days", xy.dim, -99, "Seasonal number of rainy days", "short", shuffle = TRUE, compression = 9)
            for(j in seq_along(dates)){
                filenc <- file.path(Number_rainy_day.Nc, paste0("data_", dates[j], ".nc"))
                nc <- ncdf4::nc_create(filenc, nc.grd)
                ncdf4::ncvar_put(nc, nc.grd, matrix(nbDay[j, ], nx, ny))
                ncdf4::nc_close(nc)
            }
            rm(nbDay)

            max24h <- readCdtDatasetChunk.sepdir.dates.order(datafileIdx, Maximum_rain_daily.Dir, dates, cdtParallelCond)
            max24h[is.na(max24h)] <- -99
            nc.grd <- ncdf4::ncvar_def("max24h", "mm", xy.dim, -99, 'Seasonal maximum of daily rainfall', "short", shuffle = TRUE, compression = 9)
            for(j in seq_along(dates)){
                filenc <- file.path(Maximum_rain_daily.Nc, paste0("data_", dates[j], ".nc"))
                nc <- ncdf4::nc_create(filenc, nc.grd)
                ncdf4::ncvar_put(nc, nc.grd, matrix(max24h[j, ], nx, ny))
                ncdf4::nc_close(nc)
            }
            rm(max24h)

            nb95 <- readCdtDatasetChunk.sepdir.dates.order(datafileIdx, Number_day_above_Perc95th.Dir, dates, cdtParallelCond)
            nb95[is.na(nb95)] <- -99
            nc.grd <- ncdf4::ncvar_def("nbq95th", "days", xy.dim, -99, 'Seasonal count of days when RR > 95th percentile', "short", shuffle = TRUE, compression = 9)
            for(j in seq_along(dates)){
                filenc <- file.path(Number_day_above_Perc95th.Nc, paste0("data_", dates[j], ".nc"))
                nc <- ncdf4::nc_create(filenc, nc.grd)
                ncdf4::ncvar_put(nc, nc.grd, matrix(nb95[j, ], nx, ny))
                ncdf4::nc_close(nc)
            }
            rm(nb95)

            tot95 <- readCdtDatasetChunk.sepdir.dates.order(datafileIdx, Total_rain_above_Perc95th.Dir, dates, cdtParallelCond)
            tot95[is.na(tot95)] <- -99
            nc.grd <- ncdf4::ncvar_def("totq95th", "mm", xy.dim, -99, 'Seasonal total of precipitation when RR > 95th percentile', "short", shuffle = TRUE, compression = 9)
            for(j in seq_along(dates)){
                filenc <- file.path(Total_rain_above_Perc95th.Nc, paste0("data_", dates[j], ".nc"))
                nc <- ncdf4::nc_create(filenc, nc.grd)
                ncdf4::ncvar_put(nc, nc.grd, matrix(tot95[j, ], nx, ny))
                ncdf4::nc_close(nc)
            }
            rm(tot95)

            drySpell <- readCdtDatasetChunk.sepdir.dates.order(datafileIdx, Dry_Spells.Dir, dates, cdtParallelCond)

            drySpell7 <- sapply(drySpell, function(x) sum(!is.na(x) & x >= 7))
            dim(drySpell7) <- dim(drySpell)
            drySpell7[is.na(drySpell7)] <- -99
            nc.drySpell7 <- ncdf4::ncvar_def("dryspell7", "count", xy.dim, -99, 'Number of Dry Spells greater than 7 consecutive days', "short", shuffle = TRUE, compression = 9)

            drySpellmax <- sapply(drySpell, function(x) if(all(is.na(x))) NA else max(x, na.rm = TRUE))
            dim(drySpellmax) <- dim(drySpell)
            drySpellmax[is.na(drySpellmax) | is.infinite(drySpellmax)] <- -99
            nc.drySpellmax <- ncdf4::ncvar_def("dryspellmax", "days", xy.dim, -99, "Longest dry spell", "short", shuffle = TRUE, compression = 9)

            for(j in seq_along(dates)){
                filenc <- file.path(Dry_Spell_7days.Nc, paste0("data_", dates[j], ".nc"))
                nc <- ncdf4::nc_create(filenc, nc.drySpell7)
                ncdf4::ncvar_put(nc, nc.drySpell7, matrix(drySpell7[j, ], nx, ny))
                ncdf4::nc_close(nc)

                filenc <- file.path(Longest_dry_spell.Nc, paste0("data_", dates[j], ".nc"))
                nc <- ncdf4::nc_create(filenc, nc.drySpellmax)
                ncdf4::ncvar_put(nc, nc.drySpellmax, matrix(drySpellmax[j, ], nx, ny))
                ncdf4::nc_close(nc)
            }

            return(0)
        })

        Insert.Messages.Out(message[['22']], TRUE, 's')
    }

    rm(onset, cessation)

    return(0)
}
