
getData_Validation_GridData <- function(GeneralParameters){
    message <- .cdtData$EnvData$message
    Tstep <- GeneralParameters$Tstep
    inputInfo <- GeneralParameters[c('Tstep', 'clim.var', 'CDT.index1', 'CDT.index2')]
    readcdtData <- TRUE
    if(!is.null(.cdtData$EnvData$cdtInfo$inputInfo))
        if(isTRUE(all.equal(.cdtData$EnvData$cdtInfo$inputInfo, inputInfo)))
            readcdtData <- FALSE

    if(readcdtData){
        if(!is.null(.cdtData$EnvData$cdtInfo$cdt1)){
            if(.cdtData$EnvData$cdtInfo$cdt1$info != GeneralParameters$CDT.index1){
                readcdtData1 <- TRUE
                .cdtData$EnvData$cdtInfo$cdt1 <- NULL
            }else readcdtData1 <- FALSE
        }else readcdtData1 <- TRUE

        if(readcdtData1){
            cdtTmpVar1 <- try(readRDS(GeneralParameters$CDT.index1), silent = TRUE)
            if(inherits(cdtTmpVar1, "try-error")){
                Insert.Messages.Out(message[['18-1']], TRUE, "e")
                return(NULL)
            }
            if(GeneralParameters$Tstep != cdtTmpVar1$TimeStep){
                msg <- paste(message[['18-2']], GeneralParameters$Tstep)
                Insert.Messages.Out(msg, TRUE, "e")
                return(NULL)
            }

            # cdtTmpVar1$index <- seq_along(cdtTmpVar1$dateInfo$date)
            .cdtData$EnvData$cdtInfo$cdt1$info <- GeneralParameters$CDT.index1
        }

        if(!is.null(.cdtData$EnvData$cdtInfo$cdt2)){
            if(.cdtData$EnvData$cdtInfo$cdt2$info != GeneralParameters$CDT.index2){
                readcdtData2 <- TRUE
                .cdtData$EnvData$cdtInfo$cdt2 <- NULL
            }else readcdtData2 <- FALSE
        }else readcdtData2 <- TRUE

        if(readcdtData2){
            cdtTmpVar2 <- try(readRDS(GeneralParameters$CDT.index2), silent = TRUE)
            if(inherits(cdtTmpVar2, "try-error")){
                Insert.Messages.Out(message[['19-1']], TRUE, 'e')
                return(NULL)
            }
            if(GeneralParameters$Tstep != cdtTmpVar2$TimeStep){
                msg <- paste(message[['19-2']], GeneralParameters$Tstep)
                Insert.Messages.Out(msg, TRUE, "e")
                return(2)
            }

            # cdtTmpVar2$index <- seq_along(cdtTmpVar2$dateInfo$date)
            .cdtData$EnvData$cdtInfo$cdt2$info <- GeneralParameters$CDT.index2
        }

        if(readcdtData1 | readcdtData2){
            if(GeneralParameters$outdir %in% c("", "NA")){
                Insert.Messages.Out(message[['5']], TRUE, 'e')
                return(NULL)
            }

            SP1 <- defSpatialPixels(list(lon = cdtTmpVar1$coords$mat$x,
                                         lat = cdtTmpVar1$coords$mat$y))
            SP2 <- defSpatialPixels(list(lon = cdtTmpVar2$coords$mat$x,
                                         lat = cdtTmpVar2$coords$mat$y))

            if(is.diffSpatialPixelsObj(SP1, SP2, tol = 1e-04)){
                Insert.Messages.Out(message[['6']], TRUE, 'e')
                return(NULL)
            }

            if(cdtTmpVar1$chunksize != cdtTmpVar2$chunksize)
            {
                Insert.Messages.Out(message[['7']], TRUE, 'e')
                return(NULL)
            }

            if(length(cdtTmpVar1$dateInfo$date) != length(cdtTmpVar2$dateInfo$date))
            {
                Insert.Messages.Out(message[['8']], TRUE, 'e')
                return(NULL)
            }

            if(any(cdtTmpVar1$dateInfo$date != cdtTmpVar2$dateInfo$date))
            {
                Insert.Messages.Out(message[['8']], TRUE, 'e')
                return(NULL)
            }

            .cdtData$EnvData$GeneralParameters <- GeneralParameters
            .cdtData$EnvData$cdtData <- cdtTmpVar1

            ##################
            fileCDT1 <- tools::file_path_sans_ext(basename(GeneralParameters$CDT.index1))
            outValidation <- file.path(GeneralParameters$outdir, paste0('VALIDATION_', fileCDT1))
            dir.create(outValidation, showWarnings = FALSE, recursive = TRUE)

            .cdtData$EnvData$outValidation <- outValidation

            fileValidOut <- file.path(outValidation, "VALIDATION_DATA_OUT.rds")
            stats.data <- .cdtData$EnvData[c('GeneralParameters', 'cdtData')]
            saveRDS(stats.data, file = fileValidOut)
        }

        .cdtData$EnvData$cdtInfo$inputInfo <- inputInfo
    }

    return(0)
}

procs_Validation_GridData <- function(GeneralParameters){
    GUI <- TRUE
    progress <- TRUE

    if(GeneralParameters$validExist){
        outValidation <- dirname(.cdtData$EnvData$file.hovd)
    }else{
        outValidation <- .cdtData$EnvData$outValidation
    }

    message <- .cdtData$EnvData$message

    if(is.null(.cdtData$EnvData$cdtData)){
        Insert.Messages.Out(message[['9']], TRUE, 'e')
        return(NULL)
    }

    xvargal <- c("date.range", "aggr.series", "dicho.fcst", "volume.stat", "add.to.plot")
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

    idaty <- dates %in% .cdtData$EnvData$cdtData$dateInfo$date
    dates <- dates[idaty]
    dates0 <- dates

    # .cdtData$EnvData$opDATA$id <- .cdtData$EnvData$cdtData$info$id
    # .cdtData$EnvData$opDATA$lon <- .cdtData$EnvData$cdtData$info$lon
    # .cdtData$EnvData$opDATA$lat <- .cdtData$EnvData$cdtData$info$lat

    ###################

    out.series <- if(seasonLength == 1) 'monthly' else 'seasonal'
    AggrSeries <- c(aggr.pars, list(out.series = out.series))

    ###################

    if(is.null(.cdtData$EnvData$Statistics) |
       !isTRUE(all.equal(.cdtData$EnvData$opDATA$dates, dates0)) |
       !isTRUE(all.equal(.cdtData$EnvData$opDATA$AggrSeries, AggrSeries)))
    {
        idx <- match(dates0, .cdtData$EnvData$cdtData$dateInfo$date)
        # .cdtData$EnvData$opDATA$dates <- dates0
        .cdtData$EnvData$opDATA$dates <- .cdtData$EnvData$cdtData$dateInfo$date[idx]

        # .cdtData$EnvData$opDATA$stn <- .cdtData$EnvData$cdtData$obs[idx, , drop = FALSE]
        # .cdtData$EnvData$opDATA$ncdf <- .cdtData$EnvData$cdtData$fcst[idx, , drop = FALSE]

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

            ###################

            index_file.input <- .cdtData$EnvData$cdtData

            index_file.agg <- .cdtData$EnvData$cdtData
            index_file.agg$TimeStep <- out.series
            index_file.agg$dateInfo$date <- agg.index$date
            index_file.agg$dateInfo$index <- seq_along(agg.index$date)

            ###################

            aggrDir <- "AGGREGATED_DATA"

            data.name1 <- paste0("DATA_OBS_", aggr.pars$aggr.fun, "_", out.series)
            outputDIR1 <- file.path(outValidation, aggrDir, data.name1)
            dataDIR1 <- file.path(outputDIR1, "DATA")
            dir.create(dataDIR1, showWarnings = FALSE, recursive = TRUE)
            file.index1 <- file.path(outputDIR1, paste0(data.name1, ".rds"))
            infile.index1 <- .cdtData$EnvData$cdtInfo$cdt1$info

            con <- gzfile(file.index1, compression = 7)
            open(con, "wb")
            saveRDS(index_file.agg, con)
            close(con)

            data.name2 <- paste0("DATA_VLD_", aggr.pars$aggr.fun, "_", out.series)
            outputDIR2 <- file.path(outValidation, aggrDir, data.name2)
            dataDIR2 <- file.path(outputDIR2, "DATA")
            dir.create(dataDIR2, showWarnings = FALSE, recursive = TRUE)
            file.index2 <- file.path(outputDIR2, paste0(data.name2, ".rds"))
            infile.index2 <- .cdtData$EnvData$cdtInfo$cdt2$info

            con <- gzfile(file.index2, compression = 7)
            open(con, "wb")
            saveRDS(index_file.agg, con)
            close(con)

            ###################

            chunkfile <- sort(unique(index_file.agg$colInfo$index))
            chunkcalc <- split(chunkfile, ceiling(chunkfile / index_file.agg$chunkfac))
            cdtParallelCond <- .cdtData$Config$parallel

            do.parChunk <- if(index_file.agg$chunkfac > length(chunkcalc)) TRUE else FALSE
            do.parCALC <- if(do.parChunk) FALSE else TRUE
            parsL <- doparallel.cond(do.parCALC & (length(chunkcalc) > 10))

            ###################

            Insert.Messages.Out(message[['10-1']], TRUE, 'i')

            ret1 <- cdt.foreach(seq_along(chunkcalc), parsL, GUI, progress, FUN = function(jj)
            {
                don.data <- readCdtDatasetChunk.sequence(chunkcalc[[jj]], infile.index1,
                                                         cdtParallelCond, do.parChunk)
                don.data <- don.data[index_file.input$dateInfo$index, , drop = FALSE]
                don.data <- don.data[idx, , drop = FALSE]
                cdtdata <- cdt.data.aggregateTS(don.data, agg.index, aggr.pars)
                writeCdtDatasetChunk.sequence(cdtdata, chunkcalc[[jj]], index_file.agg,
                                              dataDIR1, cdtParallelCond, do.parChunk)
                rm(don.data, cdtdata); gc()
                return(0)
            })

            Insert.Messages.Out(message[['10-2']], TRUE, 'i')

            ret2 <- cdt.foreach(seq_along(chunkcalc), parsL, GUI, progress, FUN = function(jj)
            {
                don.data <- readCdtDatasetChunk.sequence(chunkcalc[[jj]], infile.index2,
                                                         cdtParallelCond, do.parChunk)
                don.data <- don.data[index_file.input$dateInfo$index, , drop = FALSE]
                don.data <- don.data[idx, , drop = FALSE]
                cdtdata <- cdt.data.aggregateTS(don.data, agg.index, aggr.pars)
                writeCdtDatasetChunk.sequence(cdtdata, chunkcalc[[jj]], index_file.agg,
                                              dataDIR2, cdtParallelCond, do.parChunk)
                rm(don.data, cdtdata); gc()
                return(0)
            })

            cdtData <- index_file.agg
            IndexOBS <- file.index1
            IndexVLD <- file.index2
            idx <- index_file.agg$dateInfo$index

            # AggrcdtData <- cdt.data.aggregateTS(.cdtData$EnvData$opDATA$stn, agg.index, aggr.pars)
            # AggrncdfData <- cdt.data.aggregateTS(.cdtData$EnvData$opDATA$ncdf, agg.index, aggr.pars)

            temps <- do.call(c, lapply(agg.index$index, function(x) mean(temps[x])))
            Insert.Messages.Out(message[['12']], TRUE, 's')
        }else{
            cdtData <- .cdtData$EnvData$cdtData
            IndexOBS <- .cdtData$EnvData$cdtInfo$cdt1$info
            IndexVLD <- .cdtData$EnvData$cdtInfo$cdt2$info

            # AggrcdtData <- .cdtData$EnvData$opDATA$stn
            # AggrncdfData <- .cdtData$EnvData$opDATA$ncdf
        }

        .cdtData$EnvData$opDATA$cdtData <- cdtData
        .cdtData$EnvData$opDATA$IndexOBS <- IndexOBS
        .cdtData$EnvData$opDATA$IndexVLD <- IndexVLD
        .cdtData$EnvData$opDATA$temps <- temps
        .cdtData$EnvData$opDATA$idx <- idx
        .cdtData$EnvData$opDATA$AggrSeries <- AggrSeries
    }

    Insert.Messages.Out(message[['13']], TRUE, 'i')

    #######

    cdtData <- .cdtData$EnvData$opDATA$cdtData
    IndexOBS <- .cdtData$EnvData$opDATA$IndexOBS
    IndexVLD <- .cdtData$EnvData$opDATA$IndexVLD
    temps <- .cdtData$EnvData$opDATA$temps
    idx <- .cdtData$EnvData$opDATA$idx
 
    #######
    volSTAT <- GeneralParameters$volume.stat
    dicho <- GeneralParameters$dicho.fcst
    # dichoFun <- paste0("Cat.thres.", dicho$fun)
    # description1 <- "Threshold for Categorical Statistics"
    # description2 <- "Threshold for Volumetric Statistics"

    #######

    don <- matrix(rep(1, 10), 10, 1)
    cont.stats <- cdtValidation.Cont.Stats(don, don)
    catg.stats <- cdtValidation.Categ.Stats(don, don, dicho)

    if(clim.var == "RR"){
        if(volSTAT$user){
            if(volSTAT$one.thres){
                quant.thres <- volSTAT$user.val
            }else{
                if(volSTAT$user.file == ""){
                    Insert.Messages.Out(message[['14']], TRUE, 'e')
                    return(NULL)
                }

                ncdata <- getNcdfOpenData(volSTAT$user.file)
                crds.thres <- expand.grid(x = ncdata[[2]]$x, y = ncdata[[2]]$y)
                
                inx <- crds.thres[cdtData$colInfo$id, , drop = FALSE] == cdtData$coords$df
                if(!all(inx)){
                    Insert.Messages.Out(message[['15']], TRUE, 'e')
                    return(NULL)
                }

                quant.thres <- ncdata[[2]]$z[cdtData$colInfo$id]
            }
            # voln <- 'value'
        }else{
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
            # voln <- paste0(volSTAT$from, ".P", volSTAT$perc)
        }

        volume.stats <- cdtVolumetricQuantileStats(don, don, 1)
    }else{
        volume.stats <- list(statistics = NULL, description = NULL, perfect.score = NULL)
    }

    #######

    statInfo <- list(
                    stat.name = rownames(rbind(cont.stats$statistics, catg.stats$statistics, volume.stats$statistics)),
                    description = c(cont.stats$description, catg.stats$description, volume.stats$description),
                    perfect.score = c(cont.stats$perfect.score, catg.stats$perfect.score, volume.stats$perfect.score)
                )

    cdtStats <- cdtData[!names(cdtData) %in% c('varInfo', 'dateInfo')]
    cdtStats$statInfo <- statInfo

    dirSTATS <- file.path(outValidation, "STATISTICS_DATA")
    dataSTATS <- file.path(dirSTATS, "DATA")
    dir.create(dataSTATS, showWarnings = FALSE, recursive = TRUE)

    fileStats <- file.path(dirSTATS, "STATISTICS_DATA.rds")
    saveRDS(cdtStats, file = fileStats)

    #######
    chunkfile <- sort(unique(cdtData$colInfo$index))
    chunkcalc <- split(chunkfile, ceiling(chunkfile / cdtData$chunkfac))
    cdtParallelCond <- .cdtData$Config$parallel

    do.parChunk <- if(cdtData$chunkfac > length(chunkcalc)) TRUE else FALSE
    do.parCALC <- if(do.parChunk) FALSE else TRUE
    parsL <- doparallel.cond(do.parCALC & (length(chunkcalc) > 10))

    ret <- cdt.foreach(seq_along(chunkcalc), parsL, GUI, progress, FUN = function(jj)
    {
        don.obs <- readCdtDatasetChunk.sequence(chunkcalc[[jj]], IndexOBS,
                                                cdtParallelCond, do.parChunk)
        don.obs <- don.obs[cdtData$dateInfo$index, , drop = FALSE]
        don.obs <- don.obs[idx, , drop = FALSE]

        don.vld <- readCdtDatasetChunk.sequence(chunkcalc[[jj]], IndexVLD,
                                                cdtParallelCond, do.parChunk)
        don.vld <- don.vld[cdtData$dateInfo$index, , drop = FALSE]
        don.vld <- don.vld[idx, , drop = FALSE]

        cont.stats <- cdtValidation.Cont.Stats(don.obs, don.vld)
        catg.stats <- cdtValidation.Categ.Stats(don.obs, don.vld, dicho)

        if(clim.var == "RR"){
            if(!volSTAT$user){
                don <- if(volSTAT$from == "obs") don.obs else don.vld
                don <- don[iyear, , drop = FALSE]

                naPerc <- colSums(!is.na(don))/length(years) < volSTAT$period$min.year/length(unique(years))
                quant.thres <- matrixStats::colQuantiles(don, probs = volSTAT$perc/100, na.rm = TRUE, type = 8)
                quant.thres[naPerc] <- NA
            }

            volume.stats <- cdtVolumetricQuantileStats(don.obs, don.vld, quant.thres)
        }else{
            volume.stats <- list(statistics = NULL)
        }

        statistics <- rbind(cont.stats$statistics, catg.stats$statistics, volume.stats$statistics)
        writeCdtDatasetChunk.sequence(statistics, chunkcalc[[jj]], cdtStats, dataSTATS, cdtParallelCond, do.parChunk)

        return(0)
    })


}
