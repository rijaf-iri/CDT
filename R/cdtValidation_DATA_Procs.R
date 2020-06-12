
ValidationDataProcs <- function(GeneralParameters){
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

    xvargal <- c("date.range", "aggr.series", "dicho.fcst", "stat.data", "volume.stat", "add.to.plot")
    .cdtData$EnvData$GeneralParameters[xvargal] <- GeneralParameters[xvargal]
    GeneralParameters <- .cdtData$EnvData$GeneralParameters
    clim.var <- GeneralParameters$clim.var
    timestep <- GeneralParameters$Tstep
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
    AggrSeries <- c(aggr.pars, list(out.series = out.series))

    ###################

    if(is.null(.cdtData$EnvData$Statistics) |
       !isTRUE(all.equal(.cdtData$EnvData$opDATA$dates, dates0)) |
       !isTRUE(all.equal(.cdtData$EnvData$opDATA$AggrSeries, AggrSeries)))
    {
        idx <- match(dates0, .cdtData$EnvData$cdtData$dates)
        .cdtData$EnvData$opDATA$dates <- dates0
        .cdtData$EnvData$opDATA$stn <- .cdtData$EnvData$cdtData$obs[idx, , drop = FALSE]
        .cdtData$EnvData$opDATA$ncdf <- .cdtData$EnvData$cdtData$fcst[idx, , drop = FALSE]

        inNA <- is.na(.cdtData$EnvData$opDATA$stn) | is.na(.cdtData$EnvData$opDATA$ncdf)
        .cdtData$EnvData$opDATA$stn[inNA] <- NA
        .cdtData$EnvData$opDATA$ncdf[inNA] <- NA

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
            AggrncdfData <- cdt.data.aggregateTS(.cdtData$EnvData$opDATA$ncdf, agg.index, aggr.pars)

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

    Insert.Messages.Out(message[['13']], TRUE, 'i')

    #######
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
    cont.stats <- cdtValidation.Cont.Stats(AggrcdtData, AggrncdfData)
    catg.stats <- cdtValidation.Categ.Stats(AggrcdtData, AggrncdfData, dicho)

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
            don <- if(volSTAT$from == "obs") AggrcdtData else AggrncdfData
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
        volume.stats <- cdtVolumetricQuantileStats(AggrcdtData, AggrncdfData, quant.thres)

        quant.thres <- round(quant.thres, 1)
        descrp1 <- description1
        descrp2 <- description2
        score1 <- NA
        score2 <- NA
    }else{
        volume.stats <- list(statistics = NULL, description = NULL, perfect.score = NULL)

        quant.thres <- NULL
        descrp1 <- description1
        descrp2 <- NULL
        score1 <- NA
        score2 <- NULL
    }

    tmp <- list(
                statistics = rbind(cont.stats$statistics, catg.stats$statistics, volume.stats$statistics,
                                   dicho$thres, quant.thres),
                description = c(cont.stats$description, catg.stats$description, volume.stats$description,
                                descrp1, descrp2),
                perfect.score = c(cont.stats$perfect.score, catg.stats$perfect.score, volume.stats$perfect.score,
                                  score1, score2)
               )

    tmp$statNames <- rownames(tmp$statistics)
    nl <- length(tmp$statNames)
    if(clim.var == "RR"){
        tmp$statNames[nl] <- paste0("Vol.thres.", voln)
        tmp$statNames[nl - 1] <- dichoFun
    }else tmp$statNames[nl] <- dichoFun
    tmp$threshold <- as.numeric(quant.thres)
    .cdtData$EnvData$Statistics$STN <- tmp

    ####### ALL
    matSTN <- matrix(AggrcdtData, ncol = 1)
    matCDF <- matrix(AggrncdfData, ncol = 1)

    cont.stats <- cdtValidation.Cont.Stats(matSTN, matCDF)
    catg.stats <- cdtValidation.Categ.Stats(matSTN, matCDF, dicho)

    if(clim.var == "RR"){
        if(volSTAT$user){
            quant.thres <- volSTAT$user.val
            voln <- 'value'
        }else{
            don <- if(volSTAT$from == "obs") matSTN else matCDF
            quant.thres <- stats::quantile(don, probs = volSTAT$perc/100, na.rm = TRUE, type = 8)
            voln <- paste0(volSTAT$from, ".P", volSTAT$perc)
        }

        volume.stats <- cdtVolumetricQuantileStats(matSTN, matCDF, quant.thres)

        quant.thres <- round(quant.thres, 1)
        descrp1 <- description1
        descrp2 <- description2
        score1 <- NA
        score2 <- NA
    }else{
        volume.stats <- list(statistics = NULL, description = NULL, perfect.score = NULL)

        quant.thres <- NULL
        descrp1 <- description1
        descrp2 <- NULL
        score1 <- NA
        score2 <- NULL
    }

    tmp <- list(
                statistics = rbind(cont.stats$statistics, catg.stats$statistics, volume.stats$statistics,
                                   dicho$thres, quant.thres),
                description = c(cont.stats$description, catg.stats$description, volume.stats$description,
                                descrp1, descrp2),
                perfect.score = c(cont.stats$perfect.score, catg.stats$perfect.score, volume.stats$perfect.score,
                                  score1, score2)
               )

    tmp$statNames <- rownames(tmp$statistics)
    nl <- length(tmp$statNames)
    if(clim.var == "RR"){
        tmp$statNames[nl] <- paste0("Vol.thres.", voln)
        tmp$statNames[nl - 1] <- dichoFun
    }else tmp$statNames[nl] <- dichoFun
    tmp$threshold <- as.numeric(quant.thres)
    .cdtData$EnvData$Statistics$ALL <- tmp

    ####### Spatial AVG
    matSTN <- matrix(rowMeans(AggrcdtData, na.rm = TRUE), ncol = 1)
    matCDF <- matrix(rowMeans(AggrncdfData, na.rm = TRUE), ncol = 1)

    cont.stats <- cdtValidation.Cont.Stats(matSTN, matCDF)
    catg.stats <- cdtValidation.Categ.Stats(matSTN, matCDF, dicho)

    if(clim.var == "RR"){
        if(volSTAT$user){
            quant.thres <- volSTAT$user.val
            voln <- 'value'
        }else{
            don <- if(volSTAT$from == "obs") matSTN else matCDF
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

        volume.stats <- cdtVolumetricQuantileStats(matSTN, matCDF, quant.thres)

        quant.thres <- round(quant.thres, 1)
        descrp1 <- description1
        descrp2 <- description2
        score1 <- NA
        score2 <- NA
    }else{
        volume.stats <- list(statistics = NULL, description = NULL, perfect.score = NULL)

        quant.thres <- NULL
        descrp1 <- description1
        descrp2 <- NULL
        score1 <- NA
        score2 <- NULL
    }

    tmp <- list(
                statistics = rbind(cont.stats$statistics, catg.stats$statistics, volume.stats$statistics,
                                   dicho$thres, quant.thres),
                description = c(cont.stats$description, catg.stats$description, volume.stats$description,
                                descrp1, descrp2),
                perfect.score = c(cont.stats$perfect.score, catg.stats$perfect.score, volume.stats$perfect.score,
                                  score1, score2)
               )

    tmp$statNames <- rownames(tmp$statistics)
    nl <- length(tmp$statNames)
    if(clim.var == "RR"){
        tmp$statNames[nl] <- paste0("Vol.thres.", voln)
        tmp$statNames[nl - 1] <- dichoFun
    }else tmp$statNames[nl] <- dichoFun
    tmp$threshold <- as.numeric(quant.thres)
    .cdtData$EnvData$Statistics$AVG <- tmp

    ###################

    fileValidOut <- file.path(outValidation, "VALIDATION_DATA_OUT.rds")
    # hovd.data <- .cdtData$EnvData[c('opDATA', 'Statistics', 'GeneralParameters', 'cdtData')]
    hovd.data <- .cdtData$EnvData[c('opDATA', 'Statistics', 'GeneralParameters', 'cdtData', 'stnData', 'ncdfData')]
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
    NOMS <- c('Name', 'Statistics', 'Description', 'Perfect.Score')

    stat.ALL <- .cdtData$EnvData$Statistics$ALL
    stat.ALL <- data.frame(stat.ALL$statNames, stat.ALL$statistics, stat.ALL$description, stat.ALL$perfect.score)
    names(stat.ALL) <- NOMS
    file.stat.all <- file.path(dirSTATS, paste0("All_Data_Statistics_", filename_suffix, ".csv"))
    writeFiles(stat.ALL, file.stat.all, col.names = TRUE)

    stat.AVG <- .cdtData$EnvData$Statistics$AVG
    stat.AVG <- data.frame(stat.AVG$statNames, stat.AVG$statistics, stat.AVG$description, stat.AVG$perfect.score)
    names(stat.AVG) <- NOMS
    file.stat.avg <- file.path(dirSTATS, paste0("Spatial_Average_Statistics_", filename_suffix, ".csv"))
    writeFiles(stat.AVG, file.stat.avg, col.names = TRUE)

    headinfo <- cbind(c("STATION", "LON", "STATS/LAT"),
                      do.call(rbind, .cdtData$EnvData$opDATA[c('id', 'lon', 'lat')]))
    stat.STN <- .cdtData$EnvData$Statistics$STN
    stat.STN <- cbind(stat.STN$statNames, stat.STN$statistics)
    stat.STN <- rbind(headinfo, stat.STN)
    file.stat.stn <- file.path(dirSTATS, paste0("Stations_Statistics_", filename_suffix, ".csv"))
    writeFiles(stat.STN, file.stat.stn)

    Insert.Messages.Out(message[['17']], TRUE, 's')

    return(0)
}
