
read_ValidationMultipleData <- function(GeneralParameters){
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
                msg <- paste("Unable to read:", GeneralParameters$STN.file)
                Insert.Messages.Out(msg, TRUE, "e")
                return(NULL)
            }

            cdtTmpVar <- getCDTdataAndDisplayMsg(cdtTmpVar,
                                                 GeneralParameters$intstep,
                                                 GeneralParameters$STN.file)
            if(is.null(cdtTmpVar)){
                msg <- paste("Unable to parse:", GeneralParameters$STN.file)
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
                ret <- list(data = NULL, dates = NULL, msg = paste("Unable to read:", stn))
                if(is.null(dat)) return(ret)
                dat <- getCDTdataAndDisplayMsg(dat, GeneralParameters$intstep, stn)
                ret <- list(data = NULL, dates = NULL, msg = paste("Unable to parse:", stn))
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
                Insert.Messages.Out("IDs of the stations data do not match", TRUE, "e")
                return(NULL)
            }

            daty <- Reduce(intersect, lapply(cdtTmpVar, "[[", "dates"))
            if(length(daty) == 0){
                Insert.Messages.Out("Dates do not overlap", TRUE, "e")
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
                Insert.Messages.Out("Directory to save results doesn't exist", TRUE, "e")
                return(NULL)
            }

            if(!any(.cdtData$EnvData$stnData$data$id %in% .cdtData$EnvData$vldData$ids)){
                Insert.Messages.Out("IDs of the stations data do not match", format = TRUE)
                return(NULL)
            }
            if(!any(.cdtData$EnvData$stnData$data$dates %in% .cdtData$EnvData$vldData$dates)){
                Insert.Messages.Out("The dates of the stations data do not match", format = TRUE)
                return(NULL)
            }

            tmp.don <- lapply(.cdtData$EnvData$vldData$data, function(don){
                inx <- match(.cdtData$EnvData$vldData$dates, don$dates)
                # inx <- inx[!is.na(inx)]
                # don$dates <- don$dates[inx]
                don$data <- don$data[inx, , drop = FALSE]
                jnx <- match(.cdtData$EnvData$vldData$ids, don$id)
                # jnx <- jnx[!is.na(jnx)]
                # don$id <- don$id[jnx]
                don$lon <- don$lon[jnx]
                don$lat <- don$lat[jnx]
                don$data <- don$data[, jnx, drop = FALSE]
                don
            })

            stn.don <- NULL
            inx <- match(.cdtData$EnvData$vldData$dates, .cdtData$EnvData$stnData$data$dates)
            # inx <- inx[!is.na(inx)]
            # stn.don$dates <- .cdtData$EnvData$stnData$data$dates[inx]
            stn.don$data <- .cdtData$EnvData$stnData$data$data[inx, , drop = FALSE]
            jnx <- match(.cdtData$EnvData$vldData$ids, .cdtData$EnvData$stnData$data$id)
            # jnx <- jnx[!is.na(jnx)]
            # stn.don$id <- .cdtData$EnvData$stnData$data$id[jnx]
            # stn.don$lon <- .cdtData$EnvData$stnData$data$lon[jnx]
            # stn.don$lat <- .cdtData$EnvData$stnData$data$lat[jnx]
            stn.don$data <- stn.don$data[, jnx, drop = FALSE]

            lon <- do.call(cbind, lapply(tmp.don, "[[", "lon"))
            lat <- do.call(cbind, lapply(tmp.don, "[[", "lat"))
            lon <- apply(lon, 1, function(x) x[!is.na(x)][1])
            lat <- apply(lat, 1, function(x) x[!is.na(x)][1])

            .cdtData$EnvData$cdtData$dates <- .cdtData$EnvData$vldData$dates
            .cdtData$EnvData$cdtData$info$id <- .cdtData$EnvData$vldData$ids
            .cdtData$EnvData$cdtData$info$lon <- lon
            .cdtData$EnvData$cdtData$info$lat <- lat

            .cdtData$EnvData$cdtData$fcst <- lapply(tmp.don, "[[", "data")
            .cdtData$EnvData$cdtData$obs <- stn.don$data

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

            # stats.data <- .cdtData$EnvData[c('GeneralParameters', 'cdtData', 'vldData', 'stnData')]
            stats.data <- .cdtData$EnvData[c('GeneralParameters', 'cdtData')]
            saveRDS(stats.data, file = fileValidOut)
        }

        .cdtData$EnvData$inputInfo <- inputInfo
    }

    return(0)
}

ValidationMultipleDataProcs <- function(GeneralParameters){
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

    xvargal <- c("date.range", "aggr.series", "dicho.fcst", "stat.data", "add.to.plot", "clim.var", "VALID.names")
    .cdtData$EnvData$GeneralParameters[xvargal] <- GeneralParameters[xvargal]
    GeneralParameters <- .cdtData$EnvData$GeneralParameters
    clim.var <- GeneralParameters$clim.var
    timestep <- GeneralParameters$intstep
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
        .cdtData$EnvData$opDATA$ncdf <- lapply(.cdtData$EnvData$cdtData$fcst, function(don) don[idx, , drop = FALSE])

        # inNA <- is.na(.cdtData$EnvData$opDATA$stn) | is.na(.cdtData$EnvData$opDATA$ncdf)
        # .cdtData$EnvData$opDATA$stn[inNA] <- NA
        # .cdtData$EnvData$opDATA$ncdf[inNA] <- NA

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
            AggrncdfData <- lapply(.cdtData$EnvData$opDATA$ncdf, function(don) cdt.data.aggregate(don, agg.index$index, pars = AggrSeries))

            imiss <- agg.index$nba/agg.index$nb0 < GeneralParameters$aggr.series$min.frac
            AggrcdtData[imiss] <- NA
            AggrncdfData <- lapply(AggrncdfData, function(don){
                                      don[imiss] <- NA
                                      don
                                   })

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

    ####### STN
    cont.stats <- lapply(AggrncdfData, function(X) cdtValidation.Cont.Stats(AggrcdtData, X))
    catg.stats <- lapply(AggrncdfData, function(X) cdtValidation.Categ.Stats(AggrcdtData, X, dichotomous))

    volume.stats <- NULL
    # volume.stats <- list(statistics = NULL, description = NULL, perfect.score = NULL)
    if(clim.var == "RR"){
        ## unique values
        quant.thres <- dichotomous$thres
        ## quantile from stndata
        # quant.thres <- matrixStats::colQuantiles(AggrcdtData, probs = 0.5, na.rm = TRUE, type = 8L)
        ## quantile from users, read from CDT files
        # quant.thres <- NA## 
        volume.stats <- lapply(AggrncdfData, function(X) cdtVolumetricQuantileStats(AggrcdtData, X, quant.thres))
    }

    .cdtData$EnvData$Statistics$STN <- list(cont = cont.stats, catg = catg.stats, volume = volume.stats)

    # tmp <- list(
    #             statistics = rbind(cont.stats$statistics, catg.stats$statistics, volume.stats$statistics),
    #             description = c(cont.stats$description, catg.stats$description, volume.stats$description),
    #             perfect.score = c(cont.stats$perfect.score, catg.stats$perfect.score, volume.stats$perfect.score)
    #            )

    # .cdtData$EnvData$Statistics$STN <- tmp

    ####### ALL
    matSTN <- matrix(AggrcdtData, ncol = 1)
    matCDF <- lapply(AggrncdfData, function(X) matrix(X, ncol = 1))

    cont.stats <- lapply(matCDF, function(X) cdtValidation.Cont.Stats(matSTN, X))
    catg.stats <- lapply(matCDF, function(X) cdtValidation.Categ.Stats(matSTN, X, dichotomous))

    volume.stats <- NULL
    # volume.stats <- list(statistics = NULL, description = NULL, perfect.score = NULL)
    if(clim.var == "RR"){
        quant.thres <- dichotomous$thres
        volume.stats <- lapply(matCDF, function(X) cdtVolumetricQuantileStats(matSTN, X, quant.thres))
    }

    .cdtData$EnvData$Statistics$ALL <- list(cont = cont.stats, catg = catg.stats, volume = volume.stats)

    # tmp <- list(
    #             statistics = rbind(cont.stats$statistics, catg.stats$statistics, volume.stats$statistics),
    #             description = c(cont.stats$description, catg.stats$description, volume.stats$description),
    #             perfect.score = c(cont.stats$perfect.score, catg.stats$perfect.score, volume.stats$perfect.score)
    #            )

    # .cdtData$EnvData$Statistics$ALL <- tmp

    ####### AVG
    matSTN <- matrix(rowMeans(AggrcdtData, na.rm = TRUE), ncol = 1)
    matCDF <- lapply(AggrncdfData, function(X) matrix(rowMeans(X, na.rm = TRUE), ncol = 1))

    cont.stats <- lapply(matCDF, function(X) cdtValidation.Cont.Stats(matSTN, X))
    catg.stats <- lapply(matCDF, function(X) cdtValidation.Categ.Stats(matSTN, X, dichotomous))

    volume.stats <- NULL
    # volume.stats <- list(statistics = NULL, description = NULL, perfect.score = NULL)
    if(clim.var == "RR"){
        quant.thres <- dichotomous$thres
        volume.stats <- lapply(matCDF, function(X) cdtVolumetricQuantileStats(matSTN, X, quant.thres))
    }

    .cdtData$EnvData$Statistics$AVG <- list(cont = cont.stats, catg = catg.stats, volume = volume.stats)

    # tmp <- list(
    #             statistics = rbind(cont.stats$statistics, catg.stats$statistics, volume.stats$statistics),
    #             description = c(cont.stats$description, catg.stats$description, volume.stats$description),
    #             perfect.score = c(cont.stats$perfect.score, catg.stats$perfect.score, volume.stats$perfect.score)
    #            )

    # .cdtData$EnvData$Statistics$AVG <- tmp

    ###################

    fileValidOut <- file.path(outValidation, "VALIDATION_DATA_OUT.rds")
    hovd.data <- .cdtData$EnvData[c('opDATA', 'Statistics', 'GeneralParameters', 'cdtData')]
    saveRDS(hovd.data, file = fileValidOut)

    ###################

    if(clim.var != "RR"){
        volume.stats <- NULL
        descrp3 <- NULL
        # pscores3 <- NULL
    }

    # write to table
    dirSTATS <- file.path(outValidation, "STATISTICS_DATA")
    dir.create(dirSTATS, showWarnings = FALSE, recursive = TRUE)

    stat.ALL <- .cdtData$EnvData$Statistics$ALL
    cont.stats <- do.call(cbind, lapply(stat.ALL$cont, "[[", "statistics"))
    descrp1 <- stat.ALL$cont[[1]]$description
    # pscores1 <- stat.ALL$cont[[1]]$perfect.score

    catg.stats <- do.call(cbind, lapply(stat.ALL$catg, "[[", "statistics"))
    descrp2 <- stat.ALL$catg[[1]]$description
    # pscores2 <- stat.ALL$catg[[1]]$perfect.score

    if(clim.var == "RR"){
        volume.stats <- do.call(cbind, lapply(stat.ALL$volume, "[[", "statistics"))
        descrp3 <- stat.ALL$volume[[1]]$description
        # pscores3 <- stat.ALL$volume[[1]]$perfect.score
    }

    stat.ALL <- rbind(cont.stats, catg.stats, volume.stats)
    descrp <- c(descrp1, descrp2, descrp3)
    stat.ALL <- data.frame(Name = rownames(stat.ALL), Statistics = stat.ALL, Description = descrp)
    names(stat.ALL) <- c('Name', GeneralParameters$VALID.names, 'Description')
    file.stat.all <- file.path(dirSTATS, "All_Data_Statistics.csv")
    writeFiles(stat.ALL, file.stat.all, col.names = TRUE)

    ######
    stat.AVG <- .cdtData$EnvData$Statistics$AVG
    cont.stats <- do.call(cbind, lapply(stat.AVG$cont, "[[", "statistics"))
    descrp1 <- stat.AVG$cont[[1]]$description
    # pscores1 <- stat.AVG$cont[[1]]$perfect.score

    catg.stats <- do.call(cbind, lapply(stat.AVG$catg, "[[", "statistics"))
    descrp2 <- stat.AVG$catg[[1]]$description
    # pscores2 <- stat.AVG$catg[[1]]$perfect.score

    if(clim.var == "RR"){
        volume.stats <- do.call(cbind, lapply(stat.AVG$volume, "[[", "statistics"))
        descrp3 <- stat.AVG$volume[[1]]$description
        # pscores3 <- stat.AVG$volume[[1]]$perfect.score
    }

    stat.AVG <- rbind(cont.stats, catg.stats, volume.stats)
    descrp <- c(descrp1, descrp2, descrp3)
    stat.AVG <- data.frame(Name = rownames(stat.AVG), Statistics = stat.AVG, Description = descrp)
    names(stat.AVG) <- c('Name', GeneralParameters$VALID.names, 'Description')
    file.stat.avg <- file.path(dirSTATS, "Spatial_Average_Statistics.csv")
    writeFiles(stat.AVG, file.stat.avg, col.names = TRUE)

    ######

    headinfo <- cbind(c("Station", "LON", "STATS/LAT"), do.call(rbind, .cdtData$EnvData$opDATA[c('id', 'lon', 'lat')]))
    VALID.names <- gsub(" ", "_", GeneralParameters$VALID.names)

    for(j in seq_along(GeneralParameters$VALID.names)){
        stat.STN <- .cdtData$EnvData$Statistics$STN
        cont.stats <- stat.STN$cont[[j]]$statistics
        catg.stats <- stat.STN$catg[[j]]$statistics
        if(clim.var == "RR")
            volume.stats <- stat.STN$volume[[j]]$statistics

        stat.STN <- rbind(cont.stats, catg.stats, volume.stats)
        stat.STN <- cbind(rownames(stat.STN), stat.STN)
        stat.STN <- rbind(headinfo, stat.STN)

        file.stat.stn <- file.path(dirSTATS, paste0(VALID.names[j], ".csv"))
        writeFiles(stat.STN, file.stat.stn, col.names = TRUE)
    }

    Insert.Messages.Out('Statistics calculation done!', TRUE, 's')

    return(0)
}
