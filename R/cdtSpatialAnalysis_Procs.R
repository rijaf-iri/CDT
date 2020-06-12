
spatialAnalysisProcs <- function(GeneralParameters){
    message <- .cdtData$EnvData$message
    inFile <- GeneralParameters$in.file
    intstep <- GeneralParameters$in.tstep
    outstep <- GeneralParameters$out.series$tstep
    datatype <- GeneralParameters$data.type

    startMonth <- GeneralParameters$out.series$start.mon
    seasonLength <- GeneralParameters$out.series$length.mon
    aggr.pars <- GeneralParameters$aggr.series

    use.years <- GeneralParameters$use.years
    use.month <- GeneralParameters$use.month
    analysis <- GeneralParameters$analysis

    ## pour parallel
    cdtParallelCond <- .cdtData$Config[c('dopar', 'detect.cores', 'nb.cores')]

    #############

    if(inFile %in% c("", "NA")){
        Insert.Messages.Out(message[['9']], TRUE, 'e')
        return(NULL)
    }

    if(!dir.exists(GeneralParameters$out.dir)){
        Insert.Messages.Out(message[['10']], TRUE, 'e')
        Insert.Messages.Out(paste(message[['11']], getwd()), TRUE, "w")
        GeneralParameters$out.dir <- getwd()
    }

    #############
    outputDIR <- file.path(GeneralParameters$out.dir, paste0("SPATIAL.ANALYSIS_",
                           toupper(outstep), "_", tools::file_path_sans_ext(basename(inFile))))
    dir.create(outputDIR, showWarnings = FALSE, recursive = TRUE)

    #############

    if(datatype == 'cdtstation'){
        cdtdata <- getStnOpenData(inFile)
        if(is.null(cdtdata)) return(NULL)
        cdtdata <- getCDTdataAndDisplayMsg(cdtdata, intstep, inFile)
        if(is.null(cdtdata)) return(NULL)
        cdtdata <- cdtdata[c('id', 'lon', 'lat', 'dates', 'data')]
        miss.val <- getStnOpenDataInfo(inFile)[[3]]$miss.val

        daty <- cdtdata$dates
    }

    if(datatype == 'cdtdataset'){
        cdtdata <- try(readRDS(inFile), silent = TRUE)
        if(inherits(cdtdata, "try-error")){
            Insert.Messages.Out(paste(message[['12']], inFile), TRUE, 'e')
            return(NULL)
        }
        if(intstep != cdtdata$TimeStep){
            Insert.Messages.Out(paste(message[['13']], intstep), TRUE, 'e')
            return(NULL)
        }

        daty <- cdtdata$dateInfo$date
    }

    ################################################

    ANALYSIS <- c('Mean', 'Median', 'Standard_deviation', 'Trend',
                  'Coefficient_of_variation', 'Percentiles',
                  'Frequency', 'Anomaly')

    analysis.dir <- switch(analysis$method,
                           'mean' = ANALYSIS[1],
                           'median' = ANALYSIS[2],
                           'std' = ANALYSIS[3],
                           'trend' = ANALYSIS[4],
                           'cv' = ANALYSIS[5],
                           'percentile' = ANALYSIS[6],
                           'frequency' = ANALYSIS[7],
                           'anomaly' = ANALYSIS[8]
                         )

    outAnaDIR <- file.path(outputDIR, analysis.dir)
    dir.create(outAnaDIR, showWarnings = FALSE, recursive = TRUE)

    outTSDIR <- file.path(outputDIR, "Aggregated_TimeSeries")
    dir.create(outTSDIR, showWarnings = FALSE, recursive = TRUE)

    ####################################################

    notAggr <- intstep == 'monthly' & outstep == 'monthly'
    aggregatData <- FALSE

    if(!notAggr){
        agg.index <- cdt.index.aggregate(daty, intstep, outstep,
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
            Insert.Messages.Out(message[['14']], TRUE, 'e')
            return(NULL)
        }

        odaty <- agg.index$date
        if(outstep == "monthly"){
            yyyymm <- paste0(substr(odaty, 1, 4), '-', substr(odaty, 5, 6))
            odaty <- paste0(yyyymm, '_', yyyymm)
        }
        if(outstep == "annual"){
            odaty <- paste0(odaty, '-01', '_', odaty, '-12')
        }

        ######
        toAggr <- list(agg.index, aggr.pars, intstep)

        if(is.null(.cdtData$EnvTmp$toAggr)){
            aggregatData <- TRUE
        }else{
            aggregatData <- if(!isTRUE(all.equal(.cdtData$EnvTmp$toAggr, toAggr))) TRUE else FALSE
        }

        if(aggregatData){
            Insert.Messages.Out(message[['15']], TRUE, "i")

            if(datatype == 'cdtstation'){
                AggrData <- cdt.data.aggregateTS(cdtdata$data, agg.index, aggr.pars)

                if(outstep == 'monthly'){
                    out.cdt.aggregate <- file.path(outTSDIR, paste0("Aggregated_data_", outstep, ".csv"))

                    headers <- do.call(rbind, cdtdata[c('id', 'lon', 'lat')])
                    capition <- c('ID.STN', 'LON', paste(toupper(outstep), 'LAT', sep = '/'))

                    entete <- cbind(capition, headers)
                    don <- rbind(entete, cbind(agg.index$date, AggrData))
                    writeFiles(don, out.cdt.aggregate, na = miss.val)
                    rm(don)
                }
            }

            if(datatype == 'cdtdataset'){
                outDatasetDIR <- file.path(outputDIR, paste0("Aggregated_",
                                           tools::file_path_sans_ext(basename(inFile))))
                outChunkDIR <- file.path(outDatasetDIR, "DATA")
                dir.create(outChunkDIR, showWarnings = FALSE, recursive = TRUE)

                chunkfile <- sort(unique(cdtdata$colInfo$index))
                chunkcalc <- split(chunkfile, ceiling(chunkfile / cdtdata$chunkfac))

                do.parChunk <- if(cdtdata$chunkfac > length(chunkcalc)) TRUE else FALSE
                do.parCALC <- if(do.parChunk) FALSE else TRUE

                parsL <- doparallel.cond(do.parCALC & (length(chunkcalc) > 5))
                ret <- cdt.foreach(seq_along(chunkcalc), parsL, GUI = TRUE,
                                   progress = TRUE, FUN = function(ll)
                {
                    don <- readCdtDatasetChunk.sequence(chunkcalc[[ll]], inFile, cdtParallelCond, do.par = do.parChunk)
                    don <- don[cdtdata$dateInfo$index, , drop = FALSE]
                    AggrData <- cdt.data.aggregateTS(don, agg.index, aggr.pars)
                    writeCdtDatasetChunk.sequence(AggrData, chunkcalc[[ll]], cdtdata, outChunkDIR, cdtParallelCond, do.par = do.parChunk)
                    rm(AggrData, don)
                    return(0)
                })

                #### write index data
                AggrData <- cdtdata

                AggrData$TimeStep <- outstep
                AggrData$dateInfo$date <- odaty
                AggrData$dateInfo$index <- seq_along(odaty)

                datafileIdx <- file.path(outDatasetDIR, basename(inFile))
                con <- gzfile(datafileIdx, compression = 6)
                open(con, "wb")
                saveRDS(AggrData, con)
                close(con)
                AggrData$file <- datafileIdx
            }

            .cdtData$EnvTmp$AggrData <- AggrData
            .cdtData$EnvTmp$toAggr <- toAggr
            Insert.Messages.Out(message[['16']], TRUE, "s")
        }else AggrData <- .cdtData$EnvTmp$AggrData
    }else{
        if(datatype == 'cdtstation'){
            AggrData <- cdtdata$data
        }

        if(datatype == 'cdtdataset'){
            AggrData <- cdtdata
            AggrData$file <- inFile
        }

        yyyymm <- paste0(substr(daty, 1, 4), '-', substr(daty, 5, 6))
        odaty <- paste0(yyyymm, '_', yyyymm)
    }

    ####################################################

    Insert.Messages.Out(message[['19']], TRUE, 'i')

    if(!use.years$all.years){
        if(use.years$nseq.years)
            yeartoAna <- use.years$custom.years
        else
          yeartoAna <- use.years$start.year:use.years$end.year
    }else{
       yeartoAna <- as.numeric(substr(odaty, 1, 4))
    }

    if(outstep == 'monthly'){
        if(!use.month$nseq.months){
            startmon <- use.month$start.month
            endmon <- use.month$end.month
            seasonlen <- (endmon - startmon + 1) %% 12
            seasonlen[seasonlen == 0] <- 12
            monthtoAna <- (startmon:(startmon + (seasonlen - 1))) %% 12
            monthtoAna[monthtoAna == 0] <- 12
        }else monthtoAna <- use.month$custom.months
    }else monthtoAna <- 1:12
        
    itmp <- (as.numeric(substr(odaty, 1, 4)) %in% yeartoAna) &
            (as.numeric(substr(odaty, 6, 7)) %in% monthtoAna)

    if(!any(itmp)){
        Insert.Messages.Out(paste(message[['17']], intstep), TRUE, 'e')
        return(NULL)
    }

    ####################################################

    odaty <- odaty[itmp]

    if(outstep == "monthly"){
        ixm <- substr(odaty, 6, 7)
        ixm <- tapply(seq(length(odaty)), ixm, identity)
        # yyear <- as.numeric(substr(odaty[ixm[[1]]], 1, 4))
        odaty <- lapply(ixm, function(j) odaty[j])
    }else{
        ixm <- if(datatype == 'cdtstation') nrow(AggrData) else length(AggrData$dateInfo$index)
        ixm <- list(seq(ixm)[itmp])
        # yyear <- as.numeric(substr(odaty, 1, 4))
        odaty <- list(odaty)
    }

    ####################################################

    if(outstep == 'monthly') seasonLength <- 1
    if(outstep == 'annual'){
        seasonLength <- 12
        outstep <- "seasonal"
    }

    pars.anom <- analysis$anomaly
    pars.trend <- analysis$trend

    ####################################################

    if(datatype == 'cdtstation'){
        AggrData <- AggrData[itmp, , drop = FALSE]

        if(analysis$method != "anomaly"){
            if(outstep == "monthly"){
                AggrNA <- lapply(ixm, function(x){
                    MAT <- AggrData[x, , drop = FALSE]
                    matrix(1 - (colSums(is.na(MAT)) / nrow(MAT)), nrow = 1)
                })
            }else{
                AggrNA <- list(matrix(1 - (colSums(is.na(AggrData)) / nrow(AggrData)), nrow = 1))
            }
        }

        if(analysis$method == "anomaly"){
            iyear <- lapply(odaty, function(x){
                years1 <- as.numeric(substr(x, 1, 4))
                iy1 <- if(pars.anom$all.years)
                            rep(TRUE, length(years1))
                       else
                            years1 >= pars.anom$start.year & years1 <= pars.anom$end.year
                years2 <- as.numeric(substr(x, 9, 12))
                iy2 <- if(pars.anom$all.years)
                            rep(TRUE, length(years2))
                       else
                            years2 >= pars.anom$start.year & years2 <= pars.anom$end.year
                iy1 & iy2
            })
            minyear <- sapply(iyear, function(x) sum(x)) < pars.anom$min.year

            #########
            climatoMean <- lapply(ixm, function(ix){
                MAT <- AggrData[ix, , drop = FALSE]
                moy <- colMeans(MAT, na.rm = TRUE)
                nna <- colSums(!is.na(MAT)) < pars.anom$min.year
                moy[nna] <- NA
                moy
            })

            climatoMean[minyear] <- lapply(climatoMean[minyear], function(x){
                x[] <- NA
                x
            })

            AnalysData <- lapply(seq_along(ixm), function(jj){
                don <- AggrData[ixm[[jj]], , drop = FALSE]
                clim <- climatoMean[[jj]]
                anom <- sweep(don, 2, clim, FUN = "-")
                if(pars.anom$perc) anom <- 100 * sweep(anom, 2, clim + 0.001, FUN = "/")
                anom
            })
            AnalysData <- do.call(rbind, AnalysData)
            AnalysData <- AnalysData[order(unlist(ixm)), , drop = FALSE]
        }else if(analysis$method == "trend"){
            AnalysData <- lapply(seq_along(ixm), function(jj){
                MAT <- AggrData[ixm[[jj]], , drop = FALSE]
                year <- as.numeric(substr(odaty[[jj]], 1, 4))
                cdt.data.analysis(MAT, 'trend',
                                    trend = list(
                                        year = year,
                                        min.year = pars.trend$min.year,
                                        unit = pars.trend$unit
                                    )
                                )
            })
        }else{
            AnalysData <- lapply(ixm, function(ix){
                MAT <- AggrData[ix, , drop = FALSE]
                cdt.data.analysis(MAT, analysis$method,
                                    percentile = analysis$percentile,
                                    freq.thres = analysis$frequency
                                )
            })
            AnalysData <- do.call(rbind, AnalysData)
        }
    }

    if(datatype == 'cdtdataset'){
        if(analysis$method == "anomaly"){
            outDatasetDIR <- file.path(outputDIR, paste0("Aggregated_",
                                       tools::file_path_sans_ext(basename(inFile))))
            outChunkAnom <- file.path(outDatasetDIR, "TMP_ANOMALY")
            dir.create(outChunkAnom, showWarnings = FALSE, recursive = TRUE)

            #########
            pars.anom <- analysis$anomaly

            iyear <- lapply(odaty, function(x){
                years1 <- as.numeric(substr(x, 1, 4))
                iy1 <- if(pars.anom$all.years)
                            rep(TRUE, length(years1))
                       else
                            years1 >= pars.anom$start.year & years1 <= pars.anom$end.year
                years2 <- as.numeric(substr(x, 9, 12))
                iy2 <- if(pars.anom$all.years)
                            rep(TRUE, length(years2))
                       else
                            years2 >= pars.anom$start.year & years2 <= pars.anom$end.year
                iy1 & iy2
            })
            minyear <- sapply(iyear, function(x) sum(x)) < pars.anom$min.year
        }

        #########

        yyear <- as.numeric(substr(odaty[[1]], 1, 4))

        #########
        chunkfile <- sort(unique(AggrData$colInfo$index))
        chunkcalc <- split(chunkfile, ceiling(chunkfile / cdtdata$chunkfac))

        do.parChunk <- if(cdtdata$chunkfac > length(chunkcalc)) TRUE else FALSE
        do.parCALC <- if(do.parChunk) FALSE else TRUE

        parsL <- doparallel.cond(do.parCALC & (length(chunkcalc) > 5))
        AnalysData <- cdt.foreach(seq_along(chunkcalc), parsL, GUI = TRUE,
                                  progress = TRUE, FUN = function(ll)
        {
            don <- readCdtDatasetChunk.sequence(chunkcalc[[ll]], AggrData$file, cdtParallelCond, do.par = do.parChunk)
            don <- don[AggrData$dateInfo$index, , drop = FALSE]
            don <- don[itmp, , drop = FALSE]

            if(analysis$method != "anomaly"){
                if(outstep == "monthly"){
                    AggrNA <- lapply(ixm, function(x){
                        MAT <- don[x, , drop = FALSE]
                        matrix(1 - (colSums(is.na(MAT)) / nrow(MAT)), nrow = 1)
                    })
                }else{
                    AggrNA <- list(matrix(1 - (colSums(is.na(don)) / nrow(don)), nrow = 1))
                }
            }

            if(analysis$method == "anomaly"){
                climatoMean <- lapply(ixm, function(ix){
                    MAT <- don[ix, , drop = FALSE]
                    moy <- colMeans(MAT, na.rm = TRUE)
                    nna <- colSums(!is.na(MAT)) < pars.anom$min.year
                    moy[nna] <- NA
                    moy
                })

                climatoMean[minyear] <- lapply(climatoMean[minyear], function(x){
                    x[] <- NA
                    x
                })

                AnalysData <- lapply(seq_along(ixm), function(jj){
                    don <- don[ixm[[jj]], , drop = FALSE]
                    clim <- climatoMean[[jj]]
                    anom <- sweep(don, 2, clim, FUN = "-")
                    if(pars.anom$perc) anom <- 100 * sweep(anom, 2, clim + 0.001, FUN = "/")
                    anom
                })

                AnalysData <- do.call(rbind, AnalysData)
                AnalysData <- AnalysData[order(unlist(ixm)), , drop = FALSE]
                writeCdtDatasetChunk.sequence(AnalysData, chunkcalc[[ll]], AggrData, outChunkAnom, cdtParallelCond, do.par = do.parChunk)

                return(NULL)
            }else if(analysis$method == "trend"){
                AnalysData <- lapply(ixm, function(ix){
                    MAT <- don[ix, , drop = FALSE]
                    cdt.data.analysis(MAT, 'trend',
                                        trend = list(
                                            year = yyear,
                                            min.year = pars.trend$min.year,
                                            unit = pars.trend$unit
                                        )
                                    )
                })

                return(list(Data = AnalysData, NonMiss = AggrNA))
            }else{
                AnalysData <- lapply(ixm, function(ix){
                    MAT <- don[ix, , drop = FALSE]
                    cdt.data.analysis(MAT, analysis$method,
                                        percentile = analysis$percentile,
                                        freq.thres = analysis$frequency
                                    )
                })
                AnalysData <- do.call(rbind, AnalysData)

                return(list(Data = AnalysData, NonMiss = AggrNA))
            }
        })

        if(analysis$method != "anomaly"){
            AggrNA <- lapply(AnalysData, function(x) x$NonMiss)
            AggrNA <- do.call(mapply, c(cbind, AggrNA, SIMPLIFY = FALSE))
            AnalysData <- lapply(AnalysData, function(x) x$Data)
            if(analysis$method == "trend"){
                AnalysData <- do.call(mapply, c(cbind, AnalysData, SIMPLIFY = FALSE))
            }else{
                AnalysData <- do.call(cbind, AnalysData)
            }
        }
    }

    Insert.Messages.Out(message[['20']], TRUE, "s")

    ################################################

    Insert.Messages.Out(message[['21']], TRUE, "i")

    outAna.dates <- rep(NA, length(ixm))
    outTS.dates <- vector(mode = "list", length = length(ixm))

    ################################################

    if(datatype == 'cdtdataset'){
        lon <- AggrData$coords$mat$x
        lat <- AggrData$coords$mat$y
        iorder <- AggrData$colInfo$order
        nx <- length(lon)
        ny <- length(lat)
        dx <- ncdim_def("Lon", "degreeE", lon)
        dy <- ncdim_def("Lat", "degreeN", lat)
        xy.dim <- list(dx, dy)

        if(analysis$method != "anomaly"){
            grdNA <- ncvar_def("nonNA", "", xy.dim, NA, longname = "Fraction of the available data", prec = "float", compression = 9)

            if(analysis$method == "trend"){
                if(pars.trend$unit == 1)
                    trend.longname <- "change or trend/year"
                if(pars.trend$unit == 2)
                    trend.longname <- "change or trend over the period"
                if(pars.trend$unit == 3)
                    trend.longname <- "change or trend/average (in %)"

                grd.slp <- ncvar_def("trend", "", xy.dim, NA, longname = trend.longname, prec = "float", compression = 9)
                grd.std.slp <- ncvar_def("std.slope", "", xy.dim, NA, longname = "Slope error", prec = "float", compression = 9)
                grd.pvalue <- ncvar_def("pvalue", "", xy.dim, NA, longname = "P-value", prec = "float", compression = 9)
                grd.r2 <- ncvar_def("r2", "", xy.dim, NA, longname = "Coefficient of determination R2", prec = "float", compression = 9)

                out.vars <- list(grd.slp, grd.std.slp, grd.pvalue, grd.r2, grdNA)
            }else{
                if(analysis$method == "mean"){
                    nc.var <- "mean"
                    longname.mon <- "Mean"
                }
                if(analysis$method == "median"){
                    nc.var <- "med"
                    longname.mon <- "Median"
                }
                if(analysis$method == "std"){
                    nc.var <- "std"
                    longname.mon <- "Standard deviation"
                }
                if(analysis$method == "cv"){
                    nc.var <- "cv"
                    longname.mon <- "Coefficient of variation"
                }
                if(analysis$method == "percentile"){
                    nc.var <- "perc"
                    longname.mon <- paste0(analysis$percentile, "th Percentile")
                }
                if(analysis$method == "frequency"){
                    nc.var <- "ferq"
                    longname.mon <- "Frequency, number of event every 10 years"
                }
 
                grdOut <- ncvar_def(nc.var, "", xy.dim, NA, longname = longname.mon, prec = "float", compression = 9)

                out.vars <- list(grdOut, grdNA)
            }

            ## Analysis
            for(jj in seq_along(ixm)){
                yrsAna <- paste0(range(yeartoAna), collapse = "-")
                if(seasonLength <= 12){
                    year1 <- substr(odaty[[jj]][1], 1, 4) 
                    mon1 <- substr(odaty[[jj]][1], 6, 7)
                    year2 <- substr(odaty[[jj]][1], 9, 12)
                    mon2 <- substr(odaty[[jj]][1], 14, 15)
                    if(year1 == year2){
                        if(mon1 != mon2){
                            dateAna <- if(mon1 == "01" & mon2 == "12") yrsAna else paste0(yrsAna, "_", paste0(mon1, "-", mon2))
                        }else dateAna <- paste0(yrsAna, "_", mon1)
                    }else dateAna <- paste0(yrsAna, "_", paste0(mon1, "-", mon2))
                }else dateAna <- paste0(yrsAna, "_", seasonLength)

                outAna.dates[jj] <- dateAna
                outfile <- file.path(outAnaDIR, paste0(analysis$method, "_", dateAna, ".nc"))
                nc <- nc_create(outfile, out.vars)
                if(analysis$method == "trend"){
                    mat.slp <- as.numeric(AnalysData[[jj]][1, ])
                    mat.slp <- matrix(mat.slp[iorder], ncol = ny, nrow = nx)
                    mat.std.slp <- as.numeric(AnalysData[[jj]][2, ])
                    mat.std.slp <- matrix(mat.std.slp[iorder], ncol = ny, nrow = nx)
                    mat.pval <- as.numeric(AnalysData[[jj]][3, ])
                    mat.pval <- matrix(mat.pval[iorder], ncol = ny, nrow = nx)
                    mat.r2 <- as.numeric(AnalysData[[jj]][4, ])
                    mat.r2 <- matrix(mat.r2[iorder], ncol = ny, nrow = nx)
                    mat.na <- as.numeric(AggrNA[[jj]])
                    mat.na <- matrix(mat.na[iorder], ncol = ny, nrow = nx)
                    
                    ncvar_put(nc, out.vars[[1]], mat.slp)
                    ncvar_put(nc, out.vars[[2]], mat.std.slp)
                    ncvar_put(nc, out.vars[[3]], mat.pval)
                    ncvar_put(nc, out.vars[[4]], mat.r2)
                    ncvar_put(nc, out.vars[[5]], mat.na)
                }else{
                    mat.out <- as.numeric(AnalysData[jj, ])
                    mat.out <- matrix(mat.out[iorder], ncol = ny, nrow = nx)
                    mat.na <- as.numeric(AggrNA[[jj]])
                    mat.na <- matrix(mat.na[iorder], ncol = ny, nrow = nx)
                    ncvar_put(nc, out.vars[[1]], mat.out)
                    ncvar_put(nc, out.vars[[2]], mat.na)
                }
                nc_close(nc)
            }
        }

        if(analysis$method == "anomaly"){
            longname <- paste("Anomaly", if(pars.anom$perc) "in percentage of mean" else NULL)
            grdAnom <- ncvar_def("anom", "", xy.dim, NA, longname = longname, prec = "float", compression = 9)
            for(jj in seq_along(ixm)){
                tsdaty <- odaty[[jj]]
                AnalysData <- readCdtDatasetChunk.sepdir.dates.order(AggrData$file, outChunkAnom, tsdaty, cdtParallelCond)
                outTSdaty <- vector(mode = "character", length = length(tsdaty))

                for(ii in seq_along(tsdaty)){
                    year1 <- substr(tsdaty[ii], 1, 4) 
                    mon1 <- substr(tsdaty[ii], 6, 7)
                    year2 <- substr(tsdaty[ii], 9, 12)
                    mon2 <- substr(tsdaty[ii], 14, 15)
                    if(year1 == year2){
                        if(mon1 == mon2) dateTS <- paste0(year1, mon1)
                        else{
                            dateTS <- if(mon1 == "01" & mon2 == "12") year1 else tsdaty[ii]
                        }
                    }else dateTS <- tsdaty[ii]

                    outTSdaty[ii] <- dateTS
                    outfile <- file.path(outAnaDIR, paste0(analysis$method, "_", dateTS, ".nc"))
                    nc <- nc_create(outfile, grdAnom)
                    ncvar_put(nc, grdAnom, matrix(as.numeric(AnalysData[ii, ]), ncol = ny, nrow = nx))
                    nc_close(nc)
                }

                #####
                yrsAna <- paste0(range(yeartoAna), collapse = "-")
                if(seasonLength <= 12){
                    year1 <- substr(tsdaty[1], 1, 4) 
                    mon1 <- substr(tsdaty[1], 6, 7)
                    year2 <- substr(tsdaty[1], 9, 12)
                    mon2 <- substr(tsdaty[1], 14, 15)
                    if(year1 == year2){
                        if(mon1 != mon2){
                            dateAna <- if(mon1 == "01" & mon2 == "12") yrsAna else paste0(yrsAna, "_", paste0(mon1, "-", mon2))
                        }else dateAna <- paste0(yrsAna, "_", mon1)
                    }else dateAna <- paste0(yrsAna, "_", paste0(mon1, "-", mon2))
                }else dateAna <- paste0(yrsAna, "_", seasonLength)

                outAna.dates[jj] <- dateAna
            }
            unlink(outChunkAnom, recursive = TRUE)
        }

        ## Time series
        if(!notAggr & aggregatData){
            grdTS <- ncvar_def("ts", "", xy.dim, NA, longname = "Time series", prec = "float", compression = 9)

            for(jj in seq_along(ixm)){
                tsdaty <- odaty[[jj]]
                aggrdatTS <- readCdtDatasetChunk.multi.dates.order(AggrData$file, tsdaty, cdtParallelCond)
                outTSdaty <- vector(mode = "character", length = length(tsdaty))

                for(ii in seq_along(tsdaty)){
                    year1 <- substr(tsdaty[ii], 1, 4) 
                    mon1 <- substr(tsdaty[ii], 6, 7)
                    year2 <- substr(tsdaty[ii], 9, 12)
                    mon2 <- substr(tsdaty[ii], 14, 15)
                    if(year1 == year2){
                        if(mon1 == mon2) dateTS <- paste0(year1, mon1)
                        else{
                            dateTS <- if(mon1 == "01" & mon2 == "12") year1 else tsdaty[ii]
                        }
                    }else dateTS <- tsdaty[ii]

                    outTSdaty[ii] <- dateTS
                    outfile <- file.path(outTSDIR, paste0("outTS", "_", dateTS, ".nc"))
                    nc <- nc_create(outfile, grdTS)
                    ncvar_put(nc, grdTS, matrix(as.numeric(aggrdatTS[ii, ]), ncol = ny, nrow = nx))
                    nc_close(nc)
                }
                outTS.dates[[jj]] <- list(outAna.dates[jj], outTSdaty)
            }
            .cdtData$EnvTmp$outTS.dates <- outTS.dates
        }else outTS.dates <- .cdtData$EnvTmp$outTS.dates
    }else{
        xhead <- rbind(cdtdata$id, cdtdata$lon, cdtdata$lat)
        infohead <- cbind(c('ID.STN', 'LON', 'VARS/LAT'), xhead)

        ## Analysis
        if(analysis$method != "anomaly"){
            for(jj in seq_along(ixm)){
                yrsAna <- paste0(range(yeartoAna), collapse = "-")
                if(seasonLength <= 12){
                    year1 <- substr(odaty[[jj]][1], 1, 4) 
                    mon1 <- substr(odaty[[jj]][1], 6, 7)
                    year2 <- substr(odaty[[jj]][1], 9, 12)
                    mon2 <- substr(odaty[[jj]][1], 14, 15)
                    if(year1 == year2){
                        if(mon1 != mon2){
                            dateAna <- if(mon1 == "01" & mon2 == "12") yrsAna else paste0(yrsAna, "_", paste0(mon1, "-", mon2))
                        }else dateAna <- paste0(yrsAna, "_", mon1)
                    }else dateAna <- paste0(yrsAna, "_", paste0(mon1, "-", mon2))
                }else dateAna <- paste0(yrsAna, "_", seasonLength)

                outAna.dates[jj] <- dateAna
                outfile <- file.path(outAnaDIR, paste0(analysis$method, "_", dateAna, ".csv"))
                nonmiss <- cbind("Non-missing-data", round(AggrNA[[jj]], 3))

                if(analysis$method == "trend"){
                    if(pars.trend$unit == 1)
                        trend.longname <- "change or trend/year"
                    if(pars.trend$unit == 2)
                        trend.longname <- "change or trend over the period"
                    if(pars.trend$unit == 3)
                        trend.longname <- "change or trend/average (in %)"
                    outdata <- cbind(c(trend.longname, rownames(AnalysData[[jj]])[-1]), AnalysData[[jj]])
                }else{
                    outdata <- cbind(paste0(analysis$method, "_", dateAna), round(AnalysData[jj, , drop = FALSE], 3))
                }

                outdata <- rbind(infohead, outdata, nonmiss)
                outdata[is.na(outdata)] <- .cdtData$Config$missval
                writeFiles(outdata, outfile)
            }
        }

        if(analysis$method == "anomaly"){
            headInfo <- cbind(c('ID.STN', 'LON', 'DATE/LAT'), xhead)
            for(jj in seq_along(ixm)){
                yrsAna <- paste0(range(yeartoAna), collapse = "-")
                year1 <- substr(odaty[[jj]], 1, 4) 
                mon1 <- substr(odaty[[jj]], 6, 7)
                year2 <- substr(odaty[[jj]], 9, 12)
                mon2 <- substr(odaty[[jj]], 14, 15)
                if(all(year1 == year2)){
                    if(all(mon1 == mon2)){
                        dateTS <- paste0(year1, mon1)
                        outDFile <- paste0(yrsAna, "_", mon1[1])
                    }else{
                        if(all(mon1 == "01") & all(mon2 == "12")){
                            dateTS <- year1
                            outDFile <- yrsAna
                        }else{
                            dateTS <- odaty[[jj]]
                            outDFile <- paste0(yrsAna, "_", paste0(mon1[1], "-", mon2[1]))
                        }
                    }
                }else{
                    dateTS <- odaty[[jj]]
                    outDFile <- paste0(yrsAna, "_", paste0(mon1[1], "-", mon2[1]))
                }
                outDFile <- if(seasonLength <= 12) outDFile else paste0(yrsAna, "_", seasonLength)

                outfile <- file.path(outAnaDIR, paste0(analysis$method, "_", outDFile, ".csv"))
                outdata <- cbind(dateTS, round(AnalysData[ixm[[jj]], , drop = FALSE], 2))
                outdata <- rbind(headInfo, outdata)
                outdata[is.na(outdata)] <- .cdtData$Config$missval.anom
                writeFiles(outdata, outfile)

                ####
                if(seasonLength <= 12){
                    year1 <- substr(odaty[[jj]][1], 1, 4) 
                    mon1 <- substr(odaty[[jj]][1], 6, 7)
                    year2 <- substr(odaty[[jj]][1], 9, 12)
                    mon2 <- substr(odaty[[jj]][1], 14, 15)
                    if(year1 == year2){
                        if(mon1 != mon2){
                            dateAna <- if(mon1 == "01" & mon2 == "12") yrsAna else paste0(yrsAna, "_", paste0(mon1, "-", mon2))
                        }else dateAna <- paste0(yrsAna, "_", mon1)
                    }else dateAna <- paste0(yrsAna, "_", paste0(mon1, "-", mon2))
                }else dateAna <- paste0(yrsAna, "_", seasonLength)

                outAna.dates[jj] <- dateAna
            }
        }

        ## Time series
        if(!notAggr & aggregatData){
            headInfo <- cbind(c('ID.STN', 'LON', 'DATE/LAT'), xhead)
            for(jj in seq_along(ixm)){
                yrsAna <- paste0(range(yeartoAna), collapse = "-")
                year1 <- substr(odaty[[jj]], 1, 4) 
                mon1 <- substr(odaty[[jj]], 6, 7)
                year2 <- substr(odaty[[jj]], 9, 12)
                mon2 <- substr(odaty[[jj]], 14, 15)
                if(all(year1 == year2)){
                    if(all(mon1 == mon2)){
                        dateTS <- paste0(year1, mon1)
                        outDFile <- paste0(yrsAna, "_", mon1[1])
                    }else{
                        if(all(mon1 == "01") & all(mon2 == "12")){
                            dateTS <- year1
                            outDFile <- yrsAna
                        }else{
                            dateTS <- odaty[[jj]]
                            outDFile <- paste0(yrsAna, "_", paste0(mon1[1], "-", mon2[1]))
                        }
                    }
                }else{
                    dateTS <- odaty[[jj]]
                    outDFile <- paste0(yrsAna, "_", paste0(mon1[1], "-", mon2[1]))
                }
                outDFile <- if(seasonLength <= 12) outDFile else paste0(yrsAna, "_", seasonLength)

                outTS.dates[[jj]] <- list(outDFile, dateTS)
                outfile <- file.path(outTSDIR, paste0("outTS", "_", outDFile, ".csv"))
                outdata <- cbind(dateTS, round(AggrData[ixm[[jj]], , drop = FALSE], 3))
                outdata <- rbind(headInfo, outdata)
                outdata[is.na(outdata)] <- .cdtData$Config$missval
                writeFiles(outdata, outfile)
            }
            .cdtData$EnvTmp$outTS.dates <- outTS.dates
        }else outTS.dates <- .cdtData$EnvTmp$outTS.dates
    }

    out <- NULL
    out$params <- GeneralParameters
    out$monthtoAna <- monthtoAna
    out$stats <- outAna.dates
    out$timeseries <- outTS.dates
    saveRDS(out, file = file.path(outAnaDIR, "params.rds"))

    Insert.Messages.Out(message[['22']], TRUE, "s")

    dirAnalysis <- list.dirs(outputDIR, full.names = FALSE, recursive = FALSE)
    dirAnalysis <- ANALYSIS[ANALYSIS %in% dirAnalysis]
    outStat <- list(Stats = dirAnalysis, last = analysis.dir)
    saveRDS(outStat, file = file.path(outputDIR, "SpatialAnalysis.rds"))
    .cdtData$EnvData$DirStat <- outStat
    .cdtData$EnvData$PathStat <- outputDIR
    return(0)
}
