
ValidationDataProcs <- function(GeneralParameters){
    data.exist <- tclvalue(.cdtData$EnvData$hovd) == "1"
    if(data.exist){
        outValidation <- dirname(.cdtData$EnvData$file.hovd)
    }else{
        outValidation <- file.path(GeneralParameters$outdir, paste0('VALIDATION_',
                                   tools::file_path_sans_ext(GeneralParameters$STN.file)))
    }

    if(is.null(.cdtData$EnvData$cdtData)){
        Insert.Messages.Out("No data found", format = TRUE)
        return(NULL)
    }

    xvargal <- c("date.range", "aggr.series", "dicho.fcst", "stat.data", "add.to.plot")
    .cdtData$EnvData$GeneralParameters[xvargal] <- GeneralParameters[xvargal]
    GeneralParameters <- .cdtData$EnvData$GeneralParameters
    clim.var <- GeneralParameters$clim.var
    timestep <- GeneralParameters$Tstep
    startYear <- GeneralParameters$date.range$start.year
    endYear <- GeneralParameters$date.range$end.year
    startMonth <- GeneralParameters$date.range$start.month
    endMonth <- GeneralParameters$date.range$end.month

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
    AggrSeries <- c(GeneralParameters$aggr.series,
                    list(rle.fun = ">=", rle.thres = 6, out.series = out.series))

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

        if(GeneralParameters$aggr.series$aggr.data){
            Insert.Messages.Out('Aggregate data ...', TRUE, 'i')

            agg.index <- cdt.index.aggregate(.cdtData$EnvData$opDATA$dates, timestep,
                                            out.series, seasonLength = seasonLength, startMonth = startMonth)

            AggrcdtData <- cdt.data.aggregate(.cdtData$EnvData$opDATA$stn, agg.index$index, pars = AggrSeries)
            AggrncdfData <- cdt.data.aggregate(.cdtData$EnvData$opDATA$ncdf, agg.index$index, pars = AggrSeries)

            imiss <- agg.index$nba/agg.index$nb0 < GeneralParameters$aggr.series$min.frac
            AggrcdtData[imiss] <- NA
            AggrncdfData[imiss] <- NA

            temps <- do.call(c, lapply(agg.index$index, function(x) mean(temps[x])))
            Insert.Messages.Out('Aggregating data finished', TRUE, 's')
        }else{
            AggrcdtData <- .cdtData$EnvData$opDATA$stn
            AggrncdfData <- .cdtData$EnvData$opDATA$ncdf
        }

        .cdtData$EnvData$opDATA$stnStatData <- AggrcdtData
        .cdtData$EnvData$opDATA$ncStatData <- AggrncdfData
        .cdtData$EnvData$opDATA$temps <- temps
        .cdtData$EnvData$opDATA$AggrSeries <- AggrSeries
    }

    Insert.Messages.Out('Calculate statistics ...', TRUE, 'i')

    AggrcdtData <- .cdtData$EnvData$opDATA$stnStatData
    AggrncdfData <- .cdtData$EnvData$opDATA$ncStatData
    dichotomous <- GeneralParameters$dicho.fcst
    ## change from GeneralParameters leftCmd
    names(dichotomous) <- c("fun", "thres")

    #######
    cont.stats <- cdtValidation.Cont.Stats(AggrcdtData, AggrncdfData)
    catg.stats <- cdtValidation.Categ.Stats(AggrcdtData, AggrncdfData, dichotomous)
    volume.stats <- list(statistics = NULL, description = NULL, perfect.score = NULL)
    if(clim.var == "RR"){
        # compute quantile for each stations
        # option in leftCmd select file input cdt station used to compute quantile
        # option in leftCmd to select all quantile to compute 5%, 10%, 50% ....
        # different quantile for each stations
        # loop through quantile
        # for one value
        quant.thres <- dichotomous$thres
        volume.stats <- cdtVolumetricQuantileStats(AggrcdtData, AggrncdfData, quant.thres)
    }

    tmp <- list(
                statistics = rbind(cont.stats$statistics, catg.stats$statistics, volume.stats$statistics),
                description = c(cont.stats$description, catg.stats$description, volume.stats$description),
                perfect.score = c(cont.stats$perfect.score, catg.stats$perfect.score, volume.stats$perfect.score)
               )

    .cdtData$EnvData$Statistics$STN <- tmp

    #######
    matSTN <- matrix(AggrcdtData, ncol = 1)
    matCDF <- matrix(AggrncdfData, ncol = 1)

    cont.stats <- cdtValidation.Cont.Stats(matSTN, matCDF)
    catg.stats <- cdtValidation.Categ.Stats(matSTN, matCDF, dichotomous)
    volume.stats <- list(statistics = NULL, description = NULL, perfect.score = NULL)
    if(clim.var == "RR"){
        # compute quantile for each stations
        # option in leftCmd select file input cdt station used to compute quantile
        # option in leftCmd to select all quantile to compute 5%, 10%, 50% ....
        # different quantile for each stations
        # loop through quantile
        # for one value
        quant.thres <- dichotomous$thres
        volume.stats <- cdtVolumetricQuantileStats(matSTN, matCDF, quant.thres)
    }

    tmp <- list(
                statistics = rbind(cont.stats$statistics, catg.stats$statistics, volume.stats$statistics),
                description = c(cont.stats$description, catg.stats$description, volume.stats$description),
                perfect.score = c(cont.stats$perfect.score, catg.stats$perfect.score, volume.stats$perfect.score)
               )

    .cdtData$EnvData$Statistics$ALL <- tmp

    #######
    matSTN <- matrix(rowMeans(AggrcdtData, na.rm = TRUE), ncol = 1)
    matCDF <- matrix(rowMeans(AggrncdfData, na.rm = TRUE), ncol = 1)

    cont.stats <- cdtValidation.Cont.Stats(matSTN, matCDF)
    catg.stats <- cdtValidation.Categ.Stats(matSTN, matCDF, dichotomous)
    volume.stats <- list(statistics = NULL, description = NULL, perfect.score = NULL)
    if(clim.var == "RR"){
        # compute quantile for each stations
        # option in leftCmd select file input cdt station used to compute quantile
        # option in leftCmd to select all quantile to compute 5%, 10%, 50% ....
        # different quantile for each stations
        # loop through quantile
        # for one value
        quant.thres <- dichotomous$thres
        volume.stats <- cdtVolumetricQuantileStats(matSTN, matCDF, quant.thres)
    }

    tmp <- list(
                statistics = rbind(cont.stats$statistics, catg.stats$statistics, volume.stats$statistics),
                description = c(cont.stats$description, catg.stats$description, volume.stats$description),
                perfect.score = c(cont.stats$perfect.score, catg.stats$perfect.score, volume.stats$perfect.score)
               )

    .cdtData$EnvData$Statistics$AVG <- tmp

    ###################

    fileValidOut <- file.path(outValidation, "VALIDATION_DATA_OUT.rds")
    # hovd.data <- .cdtData$EnvData[c('opDATA', 'Statistics', 'GeneralParameters', 'cdtData')]
    hovd.data <- .cdtData$EnvData[c('opDATA', 'Statistics', 'GeneralParameters', 'cdtData', 'stnData', 'ncdfData')]
    saveRDS(hovd.data, file = fileValidOut)

    ###################
    # write to table
    dirSTATS <- file.path(outValidation, "STATS_DATA")
    dir.create(dirSTATS, showWarnings = FALSE, recursive = TRUE)

    stat.ALL <- .cdtData$EnvData$Statistics$ALL
    stat.ALL <- data.frame(Name = rownames(stat.ALL$statistics), Statistics = stat.ALL$statistics, Description = stat.ALL$description)
    file.stat.all <- file.path(dirSTATS, "All_Data_Statistics.csv")
    writeFiles(stat.ALL, file.stat.all)

    stat.AVG <- .cdtData$EnvData$Statistics$AVG
    stat.AVG <- data.frame(Name = rownames(stat.AVG$statistics), Statistics = stat.AVG$statistics, Description = stat.AVG$description)
    file.stat.avg <- file.path(dirSTATS, "Spatial_Average_Statistics.csv")
    writeFiles(stat.AVG, file.stat.avg)

    headinfo <- cbind(c("Station", "LON", "STATS/LAT"), do.call(rbind, .cdtData$EnvData$opDATA[c('id', 'lon', 'lat')]))
    stat.STN <- .cdtData$EnvData$Statistics$STN$statistics
    stat.STN <- cbind(rownames(stat.STN), stat.STN)
    stat.STN <- rbind(headinfo, stat.STN)
    file.stat.stn <- file.path(dirSTATS, "Stations_Statistics.csv")
    writeFiles(stat.STN, file.stat.stn)

    Insert.Messages.Out('Statistics calculation done!', TRUE, 's')

    return(0)
}
