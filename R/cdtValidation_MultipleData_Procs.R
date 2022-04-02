
getData_Validation_MultipleData <- function(GeneralParameters){
    message <- .cdtData$EnvData$message
    inputInfo <- GeneralParameters[c('intstep', 'STN.file', 'VALID.files')]
    readstnData <- TRUE
    if(!is.null(.cdtData$EnvData$inputInfo))
        if(isTRUE(all.equal(.cdtData$EnvData$inputInfo, inputInfo)))
            readstnData <- FALSE

    if(readstnData){
        stnInfo1 <- getStnOpenDataInfo(GeneralParameters$STN.file)
        readstnData1 <- TRUE
        if(!is.null(.cdtData$EnvData$stnData)){
            readstnData1 <- FALSE
            if(!isTRUE(all.equal(.cdtData$EnvData$stnData$stnInfo, stnInfo1))){
                readstnData1 <- TRUE
                .cdtData$EnvData$stnData <- NULL
            }
        }

        if(readstnData1){
            cdtTmpVar <- getStnOpenData(GeneralParameters$STN.file)
            if(is.null(cdtTmpVar)){
                msg <- paste(message[['5']], ":", GeneralParameters$STN.file)
                Insert.Messages.Out(msg, TRUE, "e")
                return(NULL)
            }

            cdtTmpVar <- getCDTdataAndDisplayMsg(cdtTmpVar,
                                                 GeneralParameters$intstep,
                                                 GeneralParameters$STN.file)
            if(is.null(cdtTmpVar)){
                msg <- paste(message[['5a']], ":", GeneralParameters$STN.file)
                Insert.Messages.Out(msg, TRUE, "e")
                return(NULL)
            }

            cdtTmpVar <- cdtTmpVar[c('id', 'lon', 'lat', 'dates', 'data')]
            .cdtData$EnvData$stnData$index <- seq_along(cdtTmpVar$dates)
            .cdtData$EnvData$stnData$data <- cdtTmpVar
            .cdtData$EnvData$stnData$stnInfo <- stnInfo1
        }

        stnInfo2 <- lapply(GeneralParameters$VALID.files, getStnOpenDataInfo)
        readstnData2 <- TRUE
        if(!is.null(.cdtData$EnvData$vldData)){
            readstnData2 <- FALSE
            if(!isTRUE(all.equal(.cdtData$EnvData$vldData$stnInfo, stnInfo2))){
                readstnData2 <- TRUE
                .cdtData$EnvData$vldData <- NULL
            }
        }

        if(readstnData2){
            cdtTmpVar <- lapply(GeneralParameters$VALID.files, function(stn){
                dat <- getStnOpenData(stn)
                ret <- list(data = NULL, dates = NULL, msg = paste(message[['5']], ":", stn))
                if(is.null(dat)) return(ret)
                dat <- getCDTdataAndDisplayMsg(dat, GeneralParameters$intstep, stn)
                ret <- list(data = NULL, dates = NULL, msg = paste(message[['5a']], ":", stn))
                if(is.null(dat)) return(ret)
                ret <- list(data = dat[c('id', 'lon', 'lat', 'dates', 'data')], dates = dat$dates, msg = NULL)
                return(ret)
            })

            no.data <- sapply(lapply(cdtTmpVar, "[[", "data"), is.null)
            if(any(no.data)){
                sapply(lapply(cdtTmpVar[no.data], "[[", "msg"), Insert.Messages.Out, format = TRUE)
                return(NULL)
            }

            ids <- Reduce(intersect, lapply(lapply(cdtTmpVar, "[[", "data"), "[[", "id"))
            if(length(ids) == 0){
                Insert.Messages.Out(message[['7']], TRUE, "e")
                return(NULL)
            }

            daty <- Reduce(intersect, lapply(cdtTmpVar, "[[", "dates"))
            if(length(daty) == 0){
                Insert.Messages.Out(message[['8']], TRUE, "e")
                return(NULL)
            }

            .cdtData$EnvData$vldData$index <- seq_along(daty)
            .cdtData$EnvData$vldData$dates <- daty
            .cdtData$EnvData$vldData$ids <- ids
            .cdtData$EnvData$vldData$data <- lapply(cdtTmpVar, "[[", "data")
            .cdtData$EnvData$vldData$stnInfo <- stnInfo2
        }

        if(readstnData1 | readstnData2){
            if(GeneralParameters$outdir %in% c("", "NA")){
                Insert.Messages.Out(message[['6']], TRUE, "e")
                return(NULL)
            }

            if(!any(.cdtData$EnvData$stnData$data$id %in% .cdtData$EnvData$vldData$ids)){
                Insert.Messages.Out(message[['7']], TRUE, 'e')
                return(NULL)
            }
            if(!any(.cdtData$EnvData$stnData$data$dates %in% .cdtData$EnvData$vldData$dates)){
                Insert.Messages.Out(message[['8']], TRUE, 'e')
                return(NULL)
            }

            tmp.don <- lapply(.cdtData$EnvData$vldData$data, function(don){
                inx <- match(.cdtData$EnvData$vldData$dates, don$dates)
                don$data <- don$data[inx, , drop = FALSE]
                jnx <- match(.cdtData$EnvData$vldData$ids, don$id)
                don$lon <- don$lon[jnx]
                don$lat <- don$lat[jnx]
                don$data <- don$data[, jnx, drop = FALSE]
                don
            })

            stn.don <- NULL
            inx <- match(.cdtData$EnvData$vldData$dates, .cdtData$EnvData$stnData$data$dates)
            stn.don$data <- .cdtData$EnvData$stnData$data$data[inx, , drop = FALSE]
            jnx <- match(.cdtData$EnvData$vldData$ids, .cdtData$EnvData$stnData$data$id)
            stn.don$data <- stn.don$data[, jnx, drop = FALSE]

            lon <- do.call(cbind, lapply(tmp.don, "[[", "lon"))
            lat <- do.call(cbind, lapply(tmp.don, "[[", "lat"))
            lon <- apply(lon, 1, function(x) x[!is.na(x)][1])
            lat <- apply(lat, 1, function(x) x[!is.na(x)][1])

            nid <- apply(stn.don$data, 2, function(x) !all(is.na(x)))

            .cdtData$EnvData$cdtData$dates <- .cdtData$EnvData$vldData$dates
            .cdtData$EnvData$cdtData$info$id <- .cdtData$EnvData$vldData$ids[nid]
            .cdtData$EnvData$cdtData$info$lon <- lon[nid]
            .cdtData$EnvData$cdtData$info$lat <- lat[nid]

            tmp.don <- lapply(tmp.don, "[[", "data")
            .cdtData$EnvData$cdtData$fcst <- lapply(tmp.don, function(don) don[, nid, drop = FALSE])
            .cdtData$EnvData$cdtData$obs <- stn.don$data[, nid, drop = FALSE]

            .cdtData$EnvData$GeneralParameters <- GeneralParameters

            ##################
            outValidation <- file.path(GeneralParameters$outdir, paste0('VALIDATION_',
                                       tools::file_path_sans_ext(GeneralParameters$STN.file)))
            dir.create(outValidation, showWarnings = FALSE, recursive = TRUE)

            xhead <- do.call(rbind, .cdtData$EnvData$cdtData$info[c('id', 'lon', 'lat')])
            xhead <- cbind(c("STN", "LON", "DATE/LAT"), xhead)

            obs2file <- cbind(.cdtData$EnvData$cdtData$dates, .cdtData$EnvData$cdtData$obs)
            obs2file <- rbind(xhead, obs2file)
            obs2file[is.na(obs2file)] <- -99

            fcst2files <- lapply(.cdtData$EnvData$cdtData$fcst, function(don){
                tmp <- cbind(.cdtData$EnvData$cdtData$dates, don)
                tmp <- rbind(xhead, tmp)
                tmp[is.na(tmp)] <- -99
                tmp
            })

            dirCDTdata <- file.path(outValidation, "VALIDATION_DATA")
            dir.create(dirCDTdata, showWarnings = FALSE, recursive = TRUE)
            writeFiles(obs2file, file.path(dirCDTdata, "Observations.csv"))

            VALID.names <- gsub(" ", "_", GeneralParameters$VALID.names)

            ret <- lapply(seq_along(fcst2files), function(j){
                tmpfile <- file.path(dirCDTdata, paste0(VALID.names[j], ".csv"))
                writeFiles(fcst2files[[j]], tmpfile)
            })

            fileValidOut <- file.path(outValidation, "VALIDATION_DATA_OUT.rds")

            .cdtData$EnvData$VALID.names <- GeneralParameters$VALID.names
            stats.data <- .cdtData$EnvData[c('GeneralParameters', 'cdtData', 'VALID.names')]
            saveRDS(stats.data, file = fileValidOut)
        }

        .cdtData$EnvData$inputInfo <- inputInfo
    }

    return(0)
}

procs_Validation_MultipleData <- function(GeneralParameters){
    if(GeneralParameters$validExist){
        outValidation <- dirname(.cdtData$EnvData$file.hovd)
    }else{
        outValidation <- file.path(GeneralParameters$outdir, paste0('VALIDATION_',
                                   tools::file_path_sans_ext(GeneralParameters$STN.file)))
    }

    ##########################

    message <- .cdtData$EnvData$message

    if(is.null(.cdtData$EnvData$cdtData)){
        Insert.Messages.Out(message[['9']], TRUE, 'e')
        return(NULL)
    }

    xvargal <- c("date.range", "aggr.series", "dicho.fcst", "volume.stat",
                 "stat.data", "add.to.plot", "clim.var")
    .cdtData$EnvData$GeneralParameters[xvargal] <- GeneralParameters[xvargal]
    GeneralParameters <- .cdtData$EnvData$GeneralParameters
    clim.var <- GeneralParameters$clim.var
    timestep <- GeneralParameters$intstep
    startYear <- GeneralParameters$date.range$start.year
    endYear <- GeneralParameters$date.range$end.year
    startMonth <- GeneralParameters$date.range$start.month
    endMonth <- GeneralParameters$date.range$end.month
    aggr.pars <- GeneralParameters$aggr.series

    dstart <- as.Date(paste0(startYear, "-1-1"))
    dend <- as.Date(paste0(endYear, "-12-31"))

    if(timestep == "daily"){
        dates <- format(seq(dstart, dend, 'day'), '%Y%m%d')
    }
    if(timestep == "pentad"){
        dates <- seq(dstart, dend, 'day')
        dates <- paste0(format(dates[which(as.numeric(format(dates, '%d')) <= 6)], '%Y%m'),
                        as.numeric(format(dates[which(as.numeric(format(dates, '%d')) <= 6)], '%d')))
    }
    if(timestep == "dekadal"){
        dates <- seq(dstart, dend, 'day')
        dates <- paste0(format(dates[which(as.numeric(format(dates, '%d')) <= 3)], '%Y%m'),
                        as.numeric(format(dates[which(as.numeric(format(dates, '%d')) <= 3)], '%d')))
    }
    if(timestep == "monthly"){
        dates <- format(seq(dstart, dend, 'month'), '%Y%m')
    }

    seasonLength <- (endMonth - startMonth + 1) %% 12
    seasonLength <- ifelse(seasonLength == 0, 12, seasonLength)
    monthtoValid <- (startMonth:(startMonth + (seasonLength - 1))) %% 12
    monthtoValid[monthtoValid == 0] <- 12

    imonValid <- as.numeric(substr(dates, 5, 6)) %in% monthtoValid
    dates <- dates[imonValid]

    idaty <- dates %in% .cdtData$EnvData$cdtData$dates
    dates <- dates[idaty]
    dates0 <- dates

    .cdtData$EnvData$opDATA$id <- .cdtData$EnvData$cdtData$info$id
    .cdtData$EnvData$opDATA$lon <- .cdtData$EnvData$cdtData$info$lon
    .cdtData$EnvData$opDATA$lat <- .cdtData$EnvData$cdtData$info$lat

    ###################

    out.series <- if(seasonLength == 1) 'monthly' else 'seasonal'
    AggrSeries <- c(GeneralParameters$aggr.series, list(out.series = out.series))

    ###################

    if(is.null(.cdtData$EnvData$Statistics) |
       !isTRUE(all.equal(.cdtData$EnvData$opDATA$dates, dates0)) |
       !isTRUE(all.equal(.cdtData$EnvData$opDATA$AggrSeries, AggrSeries)))
    {
        idx <- match(dates0, .cdtData$EnvData$cdtData$dates)
        .cdtData$EnvData$opDATA$dates <- dates0
        .cdtData$EnvData$opDATA$stn <- .cdtData$EnvData$cdtData$obs[idx, , drop = FALSE]
        .cdtData$EnvData$opDATA$ncdf <- lapply(.cdtData$EnvData$cdtData$fcst, function(don) don[idx, , drop = FALSE])

        ###################
        xtm <- .cdtData$EnvData$opDATA$dates
        if(timestep == "daily")
            temps <- as.Date(xtm, format = "%Y%m%d")
        if(timestep == "pentad")
            temps <- as.Date(paste0(substr(xtm, 1, 6), c(3, 8, 13, 18, 23, 28)[as.numeric(substr(xtm, 7, 7))]), format = "%Y%m%d")
        if(timestep == "dekadal")
            temps <- as.Date(paste0(substr(xtm, 1, 6), c(5, 15, 25)[as.numeric(substr(xtm, 7, 7))]), format = "%Y%m%d")
        if(timestep == "monthly")
            temps <- as.Date(paste0(xtm, 15), format = "%Y%m%d")

        ###################

        if(aggr.pars$aggr.data){
            Insert.Messages.Out(message[['10']], TRUE, 'i')

            agg.index <- cdt.index.aggregate(.cdtData$EnvData$opDATA$dates,
                                             timestep, out.series,
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
                Insert.Messages.Out(message[['11']], TRUE, 'e')
                return(NULL)
            }

            AggrcdtData <- cdt.data.aggregateTS(.cdtData$EnvData$opDATA$stn, agg.index, aggr.pars)
            AggrncdfData <- lapply(.cdtData$EnvData$opDATA$ncdf, function(don) cdt.data.aggregateTS(don, agg.index, aggr.pars))

            temps <- do.call(c, lapply(agg.index$index, function(x) mean(temps[x])))
            Insert.Messages.Out(message[['12']], TRUE, 's')
        }else{
            AggrcdtData <- .cdtData$EnvData$opDATA$stn
            AggrncdfData <- .cdtData$EnvData$opDATA$ncdf
        }

        .cdtData$EnvData$opDATA$stnStatData <- AggrcdtData
        .cdtData$EnvData$opDATA$ncStatData <- AggrncdfData
        .cdtData$EnvData$opDATA$temps <- temps
        .cdtData$EnvData$opDATA$AggrSeries <- AggrSeries
    }

    #######

    Insert.Messages.Out(message[['13']], TRUE, 'i')

    AggrcdtData <- .cdtData$EnvData$opDATA$stnStatData
    AggrncdfData <- .cdtData$EnvData$opDATA$ncStatData
    temps <- .cdtData$EnvData$opDATA$temps

    #######

    volSTAT <- GeneralParameters$volume.stat
    dicho <- GeneralParameters$dicho.fcst
    dichoFun <- paste0("Cat.thres.", dicho$fun)
    description1 <- "Threshold for Categorical Statistics"
    description2 <- "Threshold for Volumetric Statistics"

    ####### STN
    cont.stats <- lapply(AggrncdfData, function(X) cdtValidation.Cont.Stats(AggrcdtData, X))
    catg.stats <- lapply(AggrncdfData, function(X) cdtValidation.Categ.Stats(AggrcdtData, X, dicho))

    if(clim.var == "RR"){
        if(volSTAT$user){
            if(volSTAT$one.thres){
                quant.thres <- volSTAT$user.val
            }else{
                if(volSTAT$user.file == ""){
                    Insert.Messages.Out(message[['14']], TRUE, 'e')
                    return(NULL)
                }
                quant.thres <- getStnOpenData(volSTAT$user.file)
                if(is.null(quant.thres)) return(NULL)
                quant.thres <- splitCDTData1(quant.thres)
                if(is.null(quant.thres)) return(NULL)

                if(!any(.cdtData$EnvData$opDATA$id %in% quant.thres$id)){
                    Insert.Messages.Out(message[['15']], TRUE, 'e')
                    return(NULL)
                }

                inx <- match(.cdtData$EnvData$opDATA$id, quant.thres$id)
                quant.thres <- quant.thres$data[1, inx]
            }
            voln <- 'value'
        }else{
            # don <- if(volSTAT$from == "obs") AggrcdtData else AggrncdfData
            don <- AggrcdtData
            years <- as.numeric(format(temps, "%Y"))
            if(length(unique(years)) < volSTAT$period$min.year){
                Insert.Messages.Out(message[['16']], TRUE, 'e')
                return(NULL)
            }
            iyear <- if(volSTAT$period$all.years)
                        rep(TRUE, length(years))
                     else
                        years >= volSTAT$period$start.year & years <= volSTAT$period$end.year
            years <- years[iyear]
            don <- don[iyear, , drop = FALSE]
            naPerc <- colSums(!is.na(don))/length(years) < volSTAT$period$min.year/length(unique(years))
            quant.thres <- matrixStats::colQuantiles(don, probs = volSTAT$perc/100, na.rm = TRUE, type = 8)
            quant.thres[naPerc] <- NA
            rm(don)
            voln <- paste0(volSTAT$from, ".P", volSTAT$perc)
        }
        volume.stats <- lapply(AggrncdfData, function(X) cdtVolumetricQuantileStats(AggrcdtData, X, quant.thres))

        quant.thres <- round(quant.thres, 1)
        descrp1 <- description1
        descrp2 <- description2
        score1 <- NA
        score2 <- NA
    }else{
        volume.stats <- NULL

        quant.thres <- NULL
        descrp1 <- description1
        descrp2 <- NULL
        score1 <- NA
        score2 <- NULL
    }

    tmp <- list(cont = cont.stats, catg = catg.stats, volume = volume.stats)
    aux <- list(desciption = c(descrp1, descrp2), score = c(score1, score2))

    tmp$statNames <- as.character(do.call(c, lapply(tmp, function(x) rownames(x[[1]]$statistics))))
    nl <- length(tmp$statNames)
    tmp$statNames[nl + 1] <- dichoFun
    if(clim.var == "RR") tmp$statNames[nl + 2] <- paste0("Vol.thres.", voln)
    tmp$threshold <- as.numeric(quant.thres)
    tmp$aux <- aux
    .cdtData$EnvData$Statistics$STN <- tmp

    ####### ALL
    matSTN <- matrix(AggrcdtData, ncol = 1)
    matCDF <- lapply(AggrncdfData, function(X) matrix(X, ncol = 1))

    cont.stats <- lapply(matCDF, function(X) cdtValidation.Cont.Stats(matSTN, X))
    catg.stats <- lapply(matCDF, function(X) cdtValidation.Categ.Stats(matSTN, X, dicho))

    if(clim.var == "RR"){
        if(volSTAT$user){
            quant.thres <- volSTAT$user.val
            voln <- 'value'
        }else{
            # don <- if(volSTAT$from == "obs") matSTN else matCDF
            don <- matSTN
            quant.thres <- stats::quantile(don, probs = volSTAT$perc/100, na.rm = TRUE, type = 8)
            voln <- paste0(volSTAT$from, ".P", volSTAT$perc)
        }

        volume.stats <- lapply(matCDF, function(X) cdtVolumetricQuantileStats(matSTN, X, quant.thres))

        quant.thres <- round(quant.thres, 1)
        descrp1 <- description1
        descrp2 <- description2
        score1 <- NA
        score2 <- NA
    }else{
        volume.stats <- NULL

        quant.thres <- NULL
        descrp1 <- description1
        descrp2 <- NULL
        score1 <- NA
        score2 <- NULL
    }

    tmp <- list(cont = cont.stats, catg = catg.stats, volume = volume.stats)
    aux <- list(desciption = c(descrp1, descrp2), score = c(score1, score2))

    tmp$statNames <- as.character(do.call(c, lapply(tmp, function(x) rownames(x[[1]]$statistics))))
    nl <- length(tmp$statNames)
    tmp$statNames[nl + 1] <- dichoFun
    if(clim.var == "RR") tmp$statNames[nl + 2] <- paste0("Vol.thres.", voln)
    tmp$threshold <- as.numeric(quant.thres)
    tmp$aux <- aux
    .cdtData$EnvData$Statistics$ALL <- tmp

    ####### AVG
    matSTN <- matrix(rowMeans(AggrcdtData, na.rm = TRUE), ncol = 1)
    matCDF <- lapply(AggrncdfData, function(X) matrix(rowMeans(X, na.rm = TRUE), ncol = 1))

    cont.stats <- lapply(matCDF, function(X) cdtValidation.Cont.Stats(matSTN, X))
    catg.stats <- lapply(matCDF, function(X) cdtValidation.Categ.Stats(matSTN, X, dicho))

    if(clim.var == "RR"){
        if(volSTAT$user){
            quant.thres <- volSTAT$user.val
            voln <- 'value'
        }else{
            # don <- if(volSTAT$from == "obs") matSTN else matCDF
            don <- matSTN
            years <- as.numeric(format(temps, "%Y"))
            if(length(unique(years)) < volSTAT$period$min.year){
                Insert.Messages.Out(message[['16']], TRUE, 'e')
                return(NULL)
            }
            iyear <- if(volSTAT$period$all.years)
                        rep(TRUE, length(years))
                     else
                        years >= volSTAT$period$start.year & years <= volSTAT$period$end.year
            years <- years[iyear]
            don <- don[iyear, , drop = FALSE]
            naPerc <- colSums(!is.na(don))/length(years) < volSTAT$period$min.year/length(unique(years))
            quant.thres <- stats::quantile(don, probs = volSTAT$perc/100, na.rm = TRUE, type = 8)
            if(naPerc) quant.thres <- NA
            voln <- paste0(volSTAT$from, ".P", volSTAT$perc)
        }

        volume.stats <- lapply(matCDF, function(X) cdtVolumetricQuantileStats(matSTN, X, quant.thres))

        quant.thres <- round(quant.thres, 1)
        descrp1 <- description1
        descrp2 <- description2
        score1 <- NA
        score2 <- NA
    }else{
        volume.stats <- NULL

        quant.thres <- NULL
        descrp1 <- description1
        descrp2 <- NULL
        score1 <- NA
        score2 <- NULL
    }

    tmp <- list(cont = cont.stats, catg = catg.stats, volume = volume.stats)
    aux <- list(desciption = c(descrp1, descrp2), score = c(score1, score2))

    tmp$statNames <- as.character(do.call(c, lapply(tmp, function(x) rownames(x[[1]]$statistics))))
    nl <- length(tmp$statNames)
    tmp$statNames[nl + 1] <- dichoFun
    if(clim.var == "RR") tmp$statNames[nl + 2] <- paste0("Vol.thres.", voln)
    tmp$threshold <- as.numeric(quant.thres)
    tmp$aux <- aux
    .cdtData$EnvData$Statistics$AVG <- tmp

    ###################

    fileValidOut <- file.path(outValidation, "VALIDATION_DATA_OUT.rds")
    hovd.data <- .cdtData$EnvData[c('opDATA', 'Statistics', 'GeneralParameters', 'cdtData', 'VALID.names')]
    saveRDS(hovd.data, file = fileValidOut)

    ###################
    dirSTATS <- file.path(outValidation, "STATISTICS_DATA")
    dir.create(dirSTATS, showWarnings = FALSE, recursive = TRUE)

    mois <- format(ISOdate(2014, 1:12, 1), "%b")
    annes <- paste0(startYear, '-', endYear)
    lmois <- paste0(mois[startMonth], '-', mois[endMonth])
    lsais <- if(aggr.pars$aggr.data) out.series else timestep
    filename_suffix <- paste0(lsais, "_", lmois, "_", annes)

    ###################

    NOMS <- c('Name', .cdtData$EnvData$VALID.names, 'Description', 'Perfect.Score')

    stat.ALL <- .cdtData$EnvData$Statistics$ALL
    cont.stats <- do.call(cbind, lapply(stat.ALL$cont, "[[", "statistics"))
    catg.stats <- do.call(cbind, lapply(stat.ALL$catg, "[[", "statistics"))
    volume.stats <- do.call(cbind, lapply(stat.ALL$volume, "[[", "statistics"))

    tmp <- rbind(cont.stats, catg.stats, volume.stats, dicho$thres, stat.ALL$threshold)
    descrp <- c(stat.ALL$cont[[1]]$description, stat.ALL$catg[[1]]$description,
                stat.ALL$volume[[1]]$description, stat.ALL$aux$desciption)
    pscores <- c(stat.ALL$cont[[1]]$perfect.score, stat.ALL$catg[[1]]$perfect.score,
                 stat.ALL$volume[[1]]$perfect.score, stat.ALL$aux$score)
    stat.ALL <- data.frame(stat.ALL$statNames, tmp, descrp, pscores)
    names(stat.ALL) <- NOMS

    file.stat.all <- file.path(dirSTATS, paste0("All_Data_Statistics_", filename_suffix, ".csv"))
    writeFiles(stat.ALL, file.stat.all, col.names = TRUE)

    ######
    stat.AVG <- .cdtData$EnvData$Statistics$AVG
    cont.stats <- do.call(cbind, lapply(stat.AVG$cont, "[[", "statistics"))
    catg.stats <- do.call(cbind, lapply(stat.AVG$catg, "[[", "statistics"))
    volume.stats <- do.call(cbind, lapply(stat.AVG$volume, "[[", "statistics"))

    tmp <- rbind(cont.stats, catg.stats, volume.stats, dicho$thres, stat.AVG$threshold)
    descrp <- c(stat.AVG$cont[[1]]$description, stat.AVG$catg[[1]]$description,
                stat.AVG$volume[[1]]$description, stat.AVG$aux$desciption)
    pscores <- c(stat.AVG$cont[[1]]$perfect.score, stat.AVG$catg[[1]]$perfect.score,
                 stat.AVG$volume[[1]]$perfect.score, stat.AVG$aux$score)
    stat.AVG <- data.frame(stat.AVG$statNames, tmp, descrp, pscores)
    names(stat.AVG) <- NOMS

    file.stat.avg <- file.path(dirSTATS, paste0("Spatial_Average_Statistics_", filename_suffix, ".csv"))
    writeFiles(stat.AVG, file.stat.avg, col.names = TRUE)

    ######

    headinfo <- cbind(c("STATION", "LON", "STATS/LAT"),
                      do.call(rbind, .cdtData$EnvData$opDATA[c('id', 'lon', 'lat')]))
    VALID.names <- gsub(" ", "_", .cdtData$EnvData$VALID.names)
    VALID.names <- paste0(VALID.names, "_", filename_suffix, ".csv")

    for(j in seq_along(.cdtData$EnvData$VALID.names)){
        stat.STN <- .cdtData$EnvData$Statistics$STN
        cont.stats <- stat.STN$cont[[j]]$statistics
        catg.stats <- stat.STN$catg[[j]]$statistics
        volume.stats <- stat.STN$volume[[j]]$statistics

        tmp <- rbind(cont.stats, catg.stats, volume.stats, dicho$thres, stat.STN$threshold)
        stat.STN <- cbind(stat.STN$statNames, tmp)
        stat.STN <- rbind(headinfo, stat.STN)

        file.stat.stn <- file.path(dirSTATS, VALID.names[j])
        writeFiles(stat.STN, file.stat.stn)
    }

    Insert.Messages.Out(message[['17']], TRUE, 's')

    return(0)
}
