
climdexCalc.TT <- function(GeneralParameters){
    if(!dir.exists(GeneralParameters$output))
    {
        Insert.Messages.Out(paste(GeneralParameters$output, "did not find"), format = TRUE)
        return(NULL)
    }

    if(!GeneralParameters$baseYear$all.years &
        any(is.na(GeneralParameters$baseYear[c('start.year', 'end.year')])))
    {
        Insert.Messages.Out("Invalid year range", format = TRUE)
        return(NULL)
    }

    ################################################

    is.TXx <- GeneralParameters$Indices$TXx
    is.TXn <- GeneralParameters$Indices$TXn
    is.TX10p <- GeneralParameters$Indices$TX10p
    is.TX90p <- GeneralParameters$Indices$TX90p
    is.WSDI <- GeneralParameters$Indices$WSDI
    is.SU <- GeneralParameters$Indices$SU
    is.ID <- GeneralParameters$Indices$ID
    is.TNx <- GeneralParameters$Indices$TNx
    is.TNn <- GeneralParameters$Indices$TNn
    is.TN10p <- GeneralParameters$Indices$TN10p
    is.TN90p <- GeneralParameters$Indices$TN90p
    is.CSDI <- GeneralParameters$Indices$CSDI
    is.TR <- GeneralParameters$Indices$TR
    is.FD <- GeneralParameters$Indices$FD
    is.DTR <- GeneralParameters$Indices$DTR
    is.GSL <- GeneralParameters$Indices$GSL

    indxTX <- c("TXx", "TXn", "TX10p", "TX90p", "WSDI", "SU", "ID")
    indxTN <- c("TNx", "TNn", "TN10p", "TN90p", "CSDI", "TR", "FD")
    indxTXN <- c("DTR", "GSL")

    is.indxTX <- unlist(GeneralParameters$Indices[indxTX])
    is.indxTN <- unlist(GeneralParameters$Indices[indxTN])
    is.indxTXN <- unlist(GeneralParameters$Indices[indxTXN])

    indxlst <- c(indxTX, indxTN, indxTXN)
    is.indxlst <- c(is.indxTX, is.indxTN, is.indxTXN)

    ################################################

    noTmax <- FALSE
    noTmin <- FALSE

    if(GeneralParameters$data.type == "cdtstation")
    {
        if(GeneralParameters$cdtstation$tx %in% c("", "NA"))
        {
            Insert.Messages.Out('No daily maximum temperature data found', format = TRUE)
            Insert.Messages.Out(paste(paste(indxTX, collapse = ", "), ", DTR, GSL", "will not be calculated"), format = TRUE)
            noTmax <- TRUE
        }
        if(GeneralParameters$cdtstation$tn %in% c("", "NA"))
        {
            Insert.Messages.Out('No daily minimum temperature data found', format = TRUE)
            Insert.Messages.Out(paste(paste(indxTN, collapse = ", "), ", DTR, GSL", "will not be calculated"), format = TRUE)
            noTmin <- TRUE
        }
    }

    if(GeneralParameters$data.type == "cdtdataset")
    {
        if(GeneralParameters$cdtdataset$tx %in% c("", "NA"))
        {
            Insert.Messages.Out('No daily maximum temperature data found', format = TRUE)
            noTmax <- TRUE
        }
        if(GeneralParameters$cdtdataset$tn %in% c("", "NA"))
        {
            Insert.Messages.Out('No daily minimum temperature data found', format = TRUE)
            noTmin <- TRUE
        }
    }

    if(noTmax & noTmin) return(NULL)

    jTmaxmin <- !noTmax & !noTmin
    jTmax <- !noTmax & noTmin
    jTmin <- noTmax & !noTmin

    if((jTmaxmin & !any(is.indxTXN)) |
        (jTmax & !any(is.indxTX)) |
        (jTmin & !any(is.indxTN)))
    {
        Insert.Messages.Out('No indices selected.', format = TRUE)
        return(0)
    }

    ################################################

    if(GeneralParameters$data.type == "cdtstation")
    {
        if(!noTmin){
            tmin <- getStnOpenData(GeneralParameters$cdtstation$tn)
            if(is.null(tmin)) return(NULL)
            tmin <- getCDTdataAndDisplayMsg(tmin, "daily", GeneralParameters$cdtstation$tn)
            if(is.null(tmin)) return(NULL)
        }

        if(!noTmax){
            tmax <- getStnOpenData(GeneralParameters$cdtstation$tx)
            if(is.null(tmax)) return(NULL)
            tmax <- getCDTdataAndDisplayMsg(tmax, "daily", GeneralParameters$cdtstation$tx)
            if(is.null(tmax)) return(NULL)
        }

        if(jTmaxmin){
            if(!any(tmin$id %in% tmax$id)){
                Insert.Messages.Out("Tmin & Tmax stations do not match", format = TRUE)
                return(NULL)
            }

            if(!any(tmin$dates %in% tmax$dates)){
                Insert.Messages.Out("Tmin & Tmax dates do not overlap", format = TRUE)
                return(NULL)
            }

            ##################
            idaty <- match(tmin$dates, tmax$dates)
            idaty <- idaty[!is.na(idaty)]

            daty <- tmax$dates[idaty]

            tmax$data <- tmax$data[idaty, , drop = FALSE]
            tmin$data <- tmin$data[tmin$dates %in% tmax$dates, , drop = FALSE]

            ##################
            id <- match(tmin$id, tmax$id)
            id <- id[!is.na(id)]

            stn.id <- tmax$id[id]
            stn.lon <- tmax$lon[id]
            stn.lat <- tmax$lat[id]

            tmax$data <- tmax$data[, id, drop = FALSE]
            tmin$data <- tmin$data[, tmin$id %in% tmax$id, drop = FALSE]
        }

        if(jTmax){
            stn.id <- tmax$id
            stn.lon <- tmax$lon
            stn.lat <- tmax$lat

            daty <- tmax$dates
        }
        if(jTmin){
            stn.id <- tmin$id
            stn.lon <- tmin$lon
            stn.lat <- tmin$lat

            daty <- tmin$dates
        }
    }

    ###########################

    if(GeneralParameters$data.type == "cdtdataset")
    {
        if(!noTmin){
            tmin <- try(readRDS(GeneralParameters$cdtdataset$tn), silent = TRUE)
            if(inherits(tmin, "try-error")){
                Insert.Messages.Out(paste("Unable to read", GeneralParameters$cdtdataset$tn), format = TRUE)
                return(NULL)
            }
            if(tmin$TimeStep != "daily"){
                Insert.Messages.Out("Tmin dataset is not a daily data", format = TRUE)
                return(NULL)
            }
        }

        if(!noTmax){
            tmax <- try(readRDS(GeneralParameters$cdtdataset$tx), silent = TRUE)
            if(inherits(tmax, "try-error")){
                Insert.Messages.Out(paste("Unable to read", GeneralParameters$cdtdataset$tx), format = TRUE)
                return(NULL)
            }
            if(tmax$TimeStep != "daily"){
                Insert.Messages.Out("Tmax dataset is not a daily data", format = TRUE)
                return(NULL)
            }
        }

        if(jTmaxmin){
            SP1 <- defSpatialPixels(list(lon = tmin$coords$mat$x, lat = tmin$coords$mat$y))
            SP2 <- defSpatialPixels(list(lon = tmax$coords$mat$x, lat = tmax$coords$mat$y))
            if(is.diffSpatialPixelsObj(SP1, SP2, tol = 1e-04)){
                Insert.Messages.Out("Tmin & Tmax have different resolution or bbox", format = TRUE)
                return(NULL)
            }
            rm(SP1, SP2)

            ##################
            if(tmin$chunksize != tmax$chunksize){
                Insert.Messages.Out("Tmin & Tmax have different chunk size", format = TRUE)
                return(NULL)
            }

            ##################
            if(!any(tmin$dateInfo$date %in% tmax$dateInfo$date)){
                Insert.Messages.Out("Tmin & Tmax dates do not match", format = TRUE)
                return(NULL)
            }

            txdaty <- match(tmin$dateInfo$date, tmax$dateInfo$date)
            txdaty <- txdaty[!is.na(txdaty)]
            tmax$dateInfo$date <- tmax$dateInfo$date[txdaty]
            tmax$dateInfo$index <- tmax$dateInfo$index[txdaty]

            tndaty <- tmin$dateInfo$date %in% tmax$dateInfo$date
            tmin$dateInfo$date <- tmin$dateInfo$date[tndaty]
            tmin$dateInfo$index <- tmin$dateInfo$index[tndaty]

            index.out <- tmin
            daty <- tmin$dateInfo$date
        }

        if(jTmax){
            index.out <- tmax
            daty <- tmax$dateInfo$index
        }
        if(jTmin){
            index.out <- tmin
            daty <- tmin$dateInfo$index
        }
    }

    ################################################

    # index.mon <- cdt.index.aggregate(daty, "daily", "monthly")
    # ifull.mon <- (index.mon$nba / index.mon$nb0) >= 0.9
    # m.nrow <- length(index.mon$date)
    # index.M <- list(index = index.mon$index, full = ifull.mon, nb0 = index.mon$nb0)

    index.year.N <- cdt.index.aggregate(daty, "daily", "seasonal", seasonLength = 12, startMonth = 1)
    index.year.S <- cdt.index.aggregate(daty, "daily", "seasonal", seasonLength = 12, startMonth = 7)

    #########################################

    outDIR <- file.path(GeneralParameters$output, "CLIMDEX_TEMP_data")
    dir.create(outDIR, showWarnings = FALSE, recursive = TRUE)
    file.index <- file.path(outDIR, "climdex.rds")

    #########################################

    if(GeneralParameters$data.type == "cdtstation")
    {
        xhead <- rbind(stn.id, stn.lon, stn.lat)

        ################################
        index.NS <- climdex.north.south(index.year.N, index.year.S,
                                GeneralParameters$start.july, stn.lat, 0.9)
        y.nrow <- length(index.NS$year)
        d.ncol <- length(stn.id)

        # ndim <- list(ncol = d.ncol, y.nrow = y.nrow, m.nrow = m.nrow)
        ndim <- list(ncol = d.ncol, y.nrow = y.nrow, m.nrow = NA)
        pars.trend <- list(year = as.numeric(index.NS$year), min.year = GeneralParameters$baseYear$min.year)

        ################################
        # TXn: Monthly minimum value of daily maximum temperature
        if(is.TXn & (jTmaxmin | jTmax)){
            pars.TXn <- list(min.frac = 0.95, aggr.fun = "min")
            TXn <- climdex_aggr.fun(tmax$data, GeneralParameters$start.july,
                                    ndim, pars.TXn, pars.trend, index.NS,
                                    month.data = FALSE, index.M = NULL)

            climdex.write.cdtstation(TXn, index.NS$year, outDIR, "TXn", xhead)
            rm(TXn)
        }

        ################################
        # TXx: Monthly maximum value of daily maximum temperature
        if(is.TXx & (jTmaxmin | jTmax)){
            pars.TXx <- list(min.frac = 0.95, aggr.fun = "max")
            TXx <- climdex_aggr.fun(tmax$data, GeneralParameters$start.july,
                                    ndim, pars.TXx, pars.trend, index.NS,
                                    month.data = FALSE, index.M = NULL)

            climdex.write.cdtstation(TXx, index.NS$year, outDIR, "TXx", xhead)
            rm(TXx)
        }

        ################################
        # TNn: Monthly minimum value of daily minimum temperature
        if(is.TNn & (jTmaxmin | jTmin)){
            pars.TNn <- list(min.frac = 0.95, aggr.fun = "min")
            TNn <- climdex_aggr.fun(tmin$data, GeneralParameters$start.july,
                                    ndim, pars.TNn, pars.trend, index.NS,
                                    month.data = FALSE, index.M = NULL)

            climdex.write.cdtstation(TNn, index.NS$year, outDIR, "TNn", xhead)
            rm(TNn)
        }

        ################################
        # TNx: Monthly maximum value of daily minimum temperature
        if(is.TNx & (jTmaxmin | jTmin)){
            pars.TNx <- list(min.frac = 0.95, aggr.fun = "max")
            TNx <- climdex_aggr.fun(tmin$data, GeneralParameters$start.july,
                                    ndim, pars.TNx, pars.trend, index.NS,
                                    month.data = FALSE, index.M = NULL)

            climdex.write.cdtstation(TNx, index.NS$year, outDIR, "TNx", xhead)
            rm(TNx)
        }

        ################################
        # SU: Number of summer days
        if(is.SU & (jTmaxmin | jTmax)){
            pars.SU <- list(min.frac = 0.95, aggr.fun = "count",
                            opr.fun = ">", opr.thres = GeneralParameters$Indices$upTX)
            SU <- climdex_aggr.fun(tmax$data, GeneralParameters$start.july,
                                    ndim, pars.SU, pars.trend, index.NS)
            climdex.write.cdtstation(SU, index.NS$year, outDIR, "SU", xhead)
            rm(SU)
        }

        ################################
        # ID: Number of icing days
        if(is.ID & (jTmaxmin | jTmax)){
            pars.ID <- list(min.frac = 0.95, aggr.fun = "count",
                            opr.fun = "<", opr.thres = GeneralParameters$Indices$loTX)
            ID <- climdex_aggr.fun(tmax$data, GeneralParameters$start.july,
                                    ndim, pars.ID, pars.trend, index.NS)
            climdex.write.cdtstation(ID, index.NS$year, outDIR, "ID", xhead)
            rm(ID)
        }

        ################################
        # FD: Number of frost days
        if(is.FD & (jTmaxmin | jTmin)){
            pars.FD <- list(min.frac = 0.95, aggr.fun = "count",
                            opr.fun = "<", opr.thres = GeneralParameters$Indices$loTN)
            FD <- climdex_aggr.fun(tmin$data, GeneralParameters$start.july,
                                    ndim, pars.FD, pars.trend, index.NS)
            climdex.write.cdtstation(FD, index.NS$year, outDIR, "FD", xhead)
            rm(FD)
        }

        ################################
        # TR: Number of tropical nights
        if(is.TR & (jTmaxmin | jTmin)){
            pars.TR <- list(min.frac = 0.95, aggr.fun = "count",
                            opr.fun = ">", opr.thres = GeneralParameters$Indices$upTN)
            TR <- climdex_aggr.fun(tmin$data, GeneralParameters$start.july,
                                    ndim, pars.TR, pars.trend, index.NS)
            climdex.write.cdtstation(TR, index.NS$year, outDIR, "TR", xhead)
            rm(TR)
        }

        ################################
        if(is.TN10p | is.TN90p | is.TX10p | is.TX90p | is.WSDI | is.CSDI)
        {
            year <- as.numeric(substr(daty, 1, 4))
            if(!GeneralParameters$baseYear$all.years){
                iyear <- year >= GeneralParameters$baseYear$start.year &
                        year <= GeneralParameters$baseYear$end.year
            }else iyear <- rep(TRUE, length(year))

            index.clim <- cdt.index.Climatologies(daty[iyear], 'daily', GeneralParameters$baseYear$window)

            if(GeneralParameters$bootstrap & (is.TN10p | is.TN90p | is.TX10p | is.TX90p))
            {
                AllYearBoot <- lapply(index.clim$index, function(ix){
                    nx <- length(ix)
                    yr <- substr(daty[iyear][ix], 1, 4)
                    year.clm <- unique(yr)
                    nBoot <- length(year.clm) - 1
                    yearBoot <- matrix(0, nrow = nx, ncol = nBoot)

                    for(k in 1:nBoot){
                        kyr <- yr %in% year.clm[k]
                        s2 <- ix[!kyr]
                        nout <- length(which(kyr))
                        s1 <- ix[yr %in% year.clm[k + 1]]
                        if(length(s1) > nout) s1 <- s1[1:nout]
                        if(length(s1) < nout) s1 <- c(s1, sample(s2, nout - length(s1)))
                        yearBoot[, k] <- c(s1, s2)
                    }
                    yearBoot
                })

                nBoot <- ncol(AllYearBoot[[1]])
                sqClim <- seq_along(index.clim$index)

                if(jTmaxmin | jTmin){
                    Q1090 <- Reduce('+', lapply(seq(nBoot), function(i){
                            index.clim0 <- index.clim
                            index.clim0$index <- lapply(sqClim, function(j) AllYearBoot[[j]][, i])

                            xquant <- .cdt.quantile.Climatologies(index.clim0, tmin$data[iyear, , drop = FALSE],
                                                                probs = c(0.1, 0.9), GeneralParameters$baseYear$min.year,
                                                                'daily', GeneralParameters$baseYear$window)
                            xquant[[1]] <- xquant[[1]] - 1e-05
                            xquant[[2]] <- xquant[[2]] + 1e-05
                            do.call(rbind, xquant)
                        })
                    )

                    Q1090 <- Q1090 / nBoot
                    tminQ1090 <- list("10%" = NULL, "90%" = NULL)
                    tminQ1090[["10%"]] <- Q1090[sqClim, , drop = FALSE]
                    tminQ1090[["90%"]] <- Q1090[sqClim[length(sqClim)] + sqClim, , drop = FALSE]
                    rm(Q1090)
                }


                if(jTmaxmin | jTmax){
                    Q1090 <- Reduce('+', lapply(seq(nBoot), function(i){
                            index.clim0 <- index.clim
                            index.clim0$index <- lapply(sqClim, function(j) AllYearBoot[[j]][, i])

                            xquant <- .cdt.quantile.Climatologies(index.clim0, tmax$data[iyear, , drop = FALSE],
                                                                probs = c(0.1, 0.9), GeneralParameters$baseYear$min.year,
                                                                'daily', GeneralParameters$baseYear$window)
                            xquant[[1]] <- xquant[[1]] - 1e-05
                            xquant[[2]] <- xquant[[2]] + 1e-05
                            do.call(rbind, xquant)
                        })
                    )

                    Q1090 <- Q1090 / nBoot
                    tmaxQ1090 <- list("10%" = NULL, "90%" = NULL)
                    tmaxQ1090[["10%"]] <- Q1090[sqClim, , drop = FALSE]
                    tmaxQ1090[["90%"]] <- Q1090[sqClim[length(sqClim)] + sqClim, , drop = FALSE]
                    rm(Q1090)
                }

                rm(AllYearBoot)
            }else{
                if(jTmaxmin | jTmin){
                    tminQ1090 <- .cdt.quantile.Climatologies(index.clim, tmin$data[iyear, , drop = FALSE],
                                                            probs = c(0.1, 0.9), GeneralParameters$baseYear$min.year,
                                                            'daily', GeneralParameters$baseYear$window)
                
                    tminQ1090[[1]] <- tminQ1090[[1]] - 1e-05
                    tminQ1090[[2]] <- tminQ1090[[2]] + 1e-05
                }

                if(jTmaxmin | jTmax){
                    tmaxQ1090 <- .cdt.quantile.Climatologies(index.clim, tmax$data[iyear, , drop = FALSE],
                                                            probs = c(0.1, 0.9), GeneralParameters$baseYear$min.year,
                                                            'daily', GeneralParameters$baseYear$window)

                    tmaxQ1090[[1]] <- tmaxQ1090[[1]] - 1e-05
                    tmaxQ1090[[2]] <- tmaxQ1090[[2]] + 1e-05
                }
            }

            index.anom <- cdt.index.Anomalies(daty, index.clim, "daily")
        }

        ################################
        # TX10p: Percentage of days when TX < 10th percentile
        if(is.TX10p & (jTmaxmin | jTmax)){
            Q10 <- .cdt.Anomalies(index.anom$index, tmax$data, tmaxQ1090[["10%"]], NULL, "Difference")
            pars.Q10 <- list(min.frac = 0.95, aggr.fun = "count", opr.fun = "<", opr.thres = 0)
            TX10p <- climdex_aggr.fun(Q10, GeneralParameters$start.july,
                                    ndim, pars.Q10, pars.trend, index.NS,
                                    month.data = FALSE, index.M = NULL,
                                    Exceedance.rate = TRUE)
            rm(Q10)
            climdex.write.cdtstation(TX10p, index.NS$year, outDIR, "TX10p", xhead)
            rm(TX10p)
        }

        ################################
        # TX90p: Percentage of days when TX > 90th percentile
        if(is.TX90p & (jTmaxmin | jTmax)){
            Q90 <- .cdt.Anomalies(index.anom$index, tmax$data, tmaxQ1090[["90%"]], NULL, "Difference")
            pars.Q90 <- list(min.frac = 0.95, aggr.fun = "count", opr.fun = ">", opr.thres = 0)
            TX90p <- climdex_aggr.fun(Q90, GeneralParameters$start.july,
                                    ndim, pars.Q90, pars.trend, index.NS,
                                    month.data = FALSE, index.M = NULL,
                                    Exceedance.rate = TRUE)
            rm(Q90)
            climdex.write.cdtstation(TX90p, index.NS$year, outDIR, "TX90p", xhead)
            rm(TX90p)
        }

        ################################
        # TN10p: Percentage of days when TN < 10th percentile
        if(is.TN10p & (jTmaxmin | jTmin)){
            Q10 <- .cdt.Anomalies(index.anom$index, tmin$data, tminQ1090[["10%"]], NULL, "Difference")
            pars.Q10 <- list(min.frac = 0.95, aggr.fun = "count", opr.fun = "<", opr.thres = 0)
            TN10p <- climdex_aggr.fun(Q10, GeneralParameters$start.july,
                                    ndim, pars.Q10, pars.trend, index.NS,
                                    month.data = FALSE, index.M = NULL,
                                    Exceedance.rate = TRUE)
            rm(Q10)
            climdex.write.cdtstation(TN10p, index.NS$year, outDIR, "TN10p", xhead)
            rm(TN10p)
        }

        ################################
        # TN90p: Percentage of days when TN > 90th percentile
        if(is.TN90p & (jTmaxmin | jTmin)){
            Q90 <- .cdt.Anomalies(index.anom$index, tmin$data, tminQ1090[["90%"]], NULL, "Difference")
            pars.Q90 <- list(min.frac = 0.95, aggr.fun = "count", opr.fun = ">", opr.thres = 0)
            TN90p <- climdex_aggr.fun(Q90, GeneralParameters$start.july,
                                    ndim, pars.Q90, pars.trend, index.NS,
                                    month.data = FALSE, index.M = NULL,
                                    Exceedance.rate = TRUE)
            rm(Q90)
            climdex.write.cdtstation(TN90p, index.NS$year, outDIR, "TN90p", xhead)
            rm(TN90p)
        }

        ################################
        # WSDI: Warm spell duration index
        if(is.WSDI & (jTmaxmin | jTmax)){
            Q90 <- .cdt.Anomalies(index.anom$index, tmax$data, tmaxQ1090[["90%"]], NULL, "Difference")
            pars.WSDI <- list(min.frac = 0.95, aggr.fun = "count.rle.nb",
                                opr.fun = ">", opr.thres = 0,
                                rle.fun = ">=", rle.thres = 6)
            WSDI <- climdex_aggr.fun(Q90, GeneralParameters$start.july,
                                    ndim, pars.WSDI, pars.trend, index.NS)
            rm(Q90)
            climdex.write.cdtstation(WSDI, index.NS$year, outDIR, "WSDI", xhead)
            rm(WSDI)
        }

        ################################
        # CSDI: Cold spell duration index
        if(is.CSDI & (jTmaxmin | jTmin)){
            Q10 <- .cdt.Anomalies(index.anom$index, tmin$data, tminQ1090[["10%"]], NULL, "Difference")
            pars.CSDI <- list(min.frac = 0.95, aggr.fun = "count.rle.nb",
                                opr.fun = "<", opr.thres = 0,
                                rle.fun = ">=", rle.thres = 6)
            CSDI <- climdex_aggr.fun(Q10, GeneralParameters$start.july,
                                    ndim, pars.CSDI, pars.trend, index.NS)
            rm(Q10)
            climdex.write.cdtstation(CSDI, index.NS$year, outDIR, "CSDI", xhead)
            rm(CSDI)
        }

        ################################
        # DTR: Daily temperature range
        if(is.DTR & jTmaxmin){
            tmp <- tmax$data - tmin$data
            pars.DTR <- list(min.frac = 0.95, aggr.fun = "mean")
            DTR <- climdex_aggr.fun(tmp, GeneralParameters$start.july,
                                    ndim, pars.DTR, pars.trend, index.NS,
                                    month.data = FALSE, index.M = NULL)
            rm(tmp)
            climdex.write.cdtstation(DTR, index.NS$year, outDIR, "DTR", xhead)
            rm(DTR)
        }

        ################################
        # Year indices
        year.idx <- index.NS$year

        ################################
        # GSL: Growing season length
        if(is.GSL & jTmaxmin){
            tmean <- (tmax$data + tmin$data)/2
            index.NS <- climdex.north.south(index.year.N, index.year.S, TRUE, stn.lat, 0.9)
            ndim <- list(ncol = length(stn.lat), y.nrow = length(index.NS$year))
            pars.trend <- list(year = as.numeric(index.NS$year), min.year = GeneralParameters$baseYear$min.year)
            pars.GSL <- list(min.frac = 0.95,
                            thres = GeneralParameters$Indices$thresGSL,
                            days = GeneralParameters$Indices$dayGSL)
            GSL <- climdex_GSL.fun(tmean, daty, index.NS, ndim, pars.GSL, pars.trend)
            rm(tmean)
            climdex.write.cdtstation(GSL, index.NS$year, outDIR, "GSL", xhead)
            rm(GSL)
        }

        ######################

        output <- list(params = GeneralParameters,
                        year = year.idx, year.gsl = index.NS$year,
                        data = list(id = stn.id, lon = stn.lon, lat = stn.lat))
    }

    #########################################

    if(GeneralParameters$data.type == "cdtdataset")
    {
        dir.cdtdata <- file.path(outDIR, 'CDTDATASET')
        dir.create(dir.cdtdata, showWarnings = FALSE, recursive = TRUE)
        dir.netcdf <- file.path(outDIR, 'DATA_NetCDF')
        dir.create(dir.netcdf, showWarnings = FALSE, recursive = TRUE)

        ################################

        latitude <- index.out$coord$df$y
        index.NS <- climdex.north.south(index.year.N, index.year.S,
                                GeneralParameters$start.july, latitude, 0.9)
        y.nrow <- length(index.NS$year)
        pars.trend <- list(year = as.numeric(index.NS$year), min.year = GeneralParameters$baseYear$min.year)

        index.NS.GSL <- climdex.north.south(index.year.N, index.year.S, TRUE, latitude, 0.9)
        pars.trend.GSL <- list(year = as.numeric(index.NS.GSL$year), min.year = GeneralParameters$baseYear$min.year)

        ################################

        index.data.year <- index.out
        index.data.year$dateInfo$date <- index.NS$year
        index.data.year$dateInfo$index <- seq_along(index.NS$year)
        index.data.year$TimeStep <- "annual"

        index.data.year.GSL <- index.data.year
        index.data.year.GSL$dateInfo$date <- index.NS.GSL$year
        index.data.year.GSL$dateInfo$index <- seq_along(index.NS.GSL$year)

        trend.vars <- c("slope", "std.slope", "t-value.slope", "p-value.slope",
                        "intercept", "std.intercept", "t-value.intercept",
                        "p-value.intercept", "R2", "sigma")
        index.data.trend <- index.out
        index.data.trend$dateInfo$date <- trend.vars
        index.data.trend$dateInfo$index <- 1:10
        index.data.trend$TimeStep <- "others"

        varInfo <- lapply(indxlst[is.indxlst], function(x){
            if(x == "TXn"){
                varinfo <- list(name = x, prec = "float", units = "degC",
                                longname = "Monthly minimum value of daily maximum temperature")
            }
            if(x == "TXx"){
                varinfo <- list(name = x, prec = "float", units = "degC",
                                longname = "Monthly maximum value of daily maximum temperature")
            }
            if(x == "TX10p"){
                varinfo <- list(name = x, prec = "float", units = "%",
                                longname = "Percentage of days when TX < 10th percentile")
            }
            if(x == "TX90p"){
                varinfo <- list(name = x, prec = "float", units = "%",
                                longname = "Percentage of days when TX > 90th percentile")
            }
            if(x == "WSDI"){
                varinfo <- list(name = x, prec = "short", units = "days",
                                longname = "Warm spell duration index")
            }
            if(x == "SU"){
                varinfo <- list(name = x, prec = "short", units = "days",
                                longname = "Number of summer days")
            }
            if(x == "ID"){
                varinfo <- list(name = x, prec = "short", units = "days",
                                longname = "Number of icing days")
            }
            if(x == "TNn"){
                varinfo <- list(name = x, prec = "float", units = "degC",
                                longname = "Monthly minimum value of daily minimum temperature")
            }
            if(x == "TNx"){
                varinfo <- list(name = x, prec = "float", units = "degC",
                                longname = "Monthly maximum value of daily minimum temperature")
            }
            if(x == "TN10p"){
                varinfo <- list(name = x, prec = "float", units = "%",
                                longname = "Percentage of days when TN < 10th percentile")
            }
            if(x == "TN90p"){
                varinfo <- list(name = x, prec = "float", units = "%",
                                longname = "Percentage of days when TN > 90th percentile")
            }
            if(x == "CSDI"){
                varinfo <- list(name = x, prec = "short", units = "days",
                                longname = "Cold spell duration index")
            }
            if(x == "FD"){
                varinfo <- list(name = x, prec = "short", units = "days",
                                longname = "Number of frost days")
            }
            if(x == "TR"){
                varinfo <- list(name = x, prec = "short", units = "days",
                                longname = "Number of tropical nights")
            }
            if(x == "DTR"){
                varinfo <- list(name = x, prec = "float", units = "degC",
                                longname = "Daily temperature range")
            }
            if(x == "GSL"){
                varinfo <- list(name = x, prec = "short", units = "days",
                                longname = "Growing season length")
            }

            return(varinfo)
        })
        names(varInfo) <- indxlst[is.indxlst]

        ################################

        paths.index <- lapply(indxlst[is.indxlst], function(x){
            dir.var <- file.path(dir.cdtdata, x)
            dir.year <- file.path(dir.var, 'Yearly')
            dir.trend <- file.path(dir.var, 'Trend')
            dir.year.data <- file.path(dir.year, "DATA")
            dir.trend.data <- file.path(dir.trend, "DATA")
            dir.create(dir.year.data, showWarnings = FALSE, recursive = TRUE)
            dir.create(dir.trend.data, showWarnings = FALSE, recursive = TRUE)

            file.index.year <- file.path(dir.year, paste0(x, '.rds'))
            file.index.year.gz <- gzfile(file.index.year, compression = 7)
            if(x == "GSL"){
                index.data.year.GSL$varInfo <- varInfo[[x]]
                saveRDS(index.data.year.GSL, file.index.year.gz)
            }else{
                index.data.year$varInfo <- varInfo[[x]]
                saveRDS(index.data.year, file.index.year.gz)
            }
            close(file.index.year.gz)

            file.index.trend <- file.path(dir.trend, paste0(x, '.rds'))
            file.index.trend.gz <- gzfile(file.index.trend, compression = 7)
            index.data.trend$varInfo <- varInfo[[x]]
            saveRDS(index.data.trend, file.index.trend.gz)
            close(file.index.trend.gz)

            paths <- list(dir.year.data = dir.year.data,
                        dir.trend.data = dir.trend.data,
                        file.index.year = file.index.year,
                        file.index.trend = file.index.trend)
            return(paths)
        })
        names(paths.index) <- indxlst[is.indxlst]

        ################################

        chunkfile <- sort(unique(index.out$colInfo$index))
        chunkcalc <- split(chunkfile, ceiling(chunkfile / index.out$chunkfac))

        do.parChunk <- if(index.out$chunkfac > length(chunkcalc)) TRUE else FALSE
        do.parCALC <- if(do.parChunk) FALSE else TRUE

        GeneralParameters <- GeneralParameters
        parsL <- doparallel.cond(do.parCALC & (length(chunkcalc) > 10))
        ret <- cdt.foreach(seq_along(chunkcalc), parsL, GUI = TRUE,
                           progress = TRUE, FUN = function(chkj)
        {
            if(!noTmin){
                tmin.data <- readCdtDatasetChunk.sequence(chunkcalc[[chkj]], GeneralParameters$cdtdataset$tn, do.par = do.parChunk)
                tmin.data <- tmin.data[tmin$dateInfo$index, , drop = FALSE]
                d.ncol <- ncol(tmin.data)
            }
            if(!noTmax){
                tmax.data <- readCdtDatasetChunk.sequence(chunkcalc[[chkj]], GeneralParameters$cdtdataset$tx, do.par = do.parChunk)
                tmax.data <- tmax.data[tmax$dateInfo$index, , drop = FALSE]
                d.ncol <- ncol(tmax.data)
            }

            ndim <- list(ncol = d.ncol, y.nrow = y.nrow, m.nrow = NA)

            latitude <- index.out$coord$df$y[index.out$colInfo$index %in% chunkcalc[[chkj]]]
            index.NS0 <- climdex.north.south(index.year.N, index.year.S, GeneralParameters$start.july, latitude, 0.9)
            index.NS1 <- index.NS
            index.NS1$index.S$idx <- index.NS0$index.S$idx
            index.NS1$index.S$ncol <- index.NS0$index.S$ncol
            index.NS1$index.N$idx <- index.NS0$index.N$idx
            index.NS1$index.N$ncol <- index.NS0$index.N$ncol
            rm(index.NS0)

            ################################
            # TXn: Monthly minimum value of daily maximum temperature
            if(is.TXn & (jTmaxmin | jTmax)){
                pars.TXn <- list(min.frac = 0.95, aggr.fun = "min")
                TXn <- climdex_aggr.fun(tmax.data, GeneralParameters$start.july,
                                        ndim, pars.TXn, pars.trend, index.NS1,
                                        month.data = FALSE, index.M = NULL)
                writeCdtDatasetChunk.sequence(TXn$year, chunkcalc[[chkj]], index.data.year,
                                            paths.index[["TXn"]]$dir.year.data, do.par = do.parChunk)
                writeCdtDatasetChunk.sequence(TXn$trend, chunkcalc[[chkj]], index.data.trend,
                                            paths.index[["TXn"]]$dir.trend.data, do.par = do.parChunk)
                rm(TXn)
            }

            ################################
            # TXx: Monthly maximum value of daily maximum temperature
            if(is.TXx & (jTmaxmin | jTmax)){
                pars.TXx <- list(min.frac = 0.95, aggr.fun = "max")
                TXx <- climdex_aggr.fun(tmax.data, GeneralParameters$start.july,
                                        ndim, pars.TXx, pars.trend, index.NS1,
                                        month.data = FALSE, index.M = NULL)
                writeCdtDatasetChunk.sequence(TXx$year, chunkcalc[[chkj]], index.data.year,
                                            paths.index[["TXx"]]$dir.year.data, do.par = do.parChunk)
                writeCdtDatasetChunk.sequence(TXx$trend, chunkcalc[[chkj]], index.data.trend,
                                            paths.index[["TXx"]]$dir.trend.data, do.par = do.parChunk)
                rm(TXx)
            }

            ################################
            # TNn: Monthly minimum value of daily minimum temperature
            if(is.TNn & (jTmaxmin | jTmin)){
                pars.TNn <- list(min.frac = 0.95, aggr.fun = "min")
                TNn <- climdex_aggr.fun(tmin.data, GeneralParameters$start.july,
                                        ndim, pars.TNn, pars.trend, index.NS1,
                                        month.data = FALSE, index.M = NULL)
                writeCdtDatasetChunk.sequence(TNn$year, chunkcalc[[chkj]], index.data.year,
                                            paths.index[["TNn"]]$dir.year.data, do.par = do.parChunk)
                writeCdtDatasetChunk.sequence(TNn$trend, chunkcalc[[chkj]], index.data.trend,
                                            paths.index[["TNn"]]$dir.trend.data, do.par = do.parChunk)
                rm(TNn)
            }

            ################################
            # TNx: Monthly maximum value of daily minimum temperature
            if(is.TNx & (jTmaxmin | jTmin)){
                pars.TNx <- list(min.frac = 0.95, aggr.fun = "max")
                TNx <- climdex_aggr.fun(tmin.data, GeneralParameters$start.july,
                                        ndim, pars.TNx, pars.trend, index.NS1,
                                        month.data = FALSE, index.M = NULL)
                writeCdtDatasetChunk.sequence(TNx$year, chunkcalc[[chkj]], index.data.year,
                                            paths.index[["TNx"]]$dir.year.data, do.par = do.parChunk)
                writeCdtDatasetChunk.sequence(TNx$trend, chunkcalc[[chkj]], index.data.trend,
                                            paths.index[["TNx"]]$dir.trend.data, do.par = do.parChunk)
                rm(TNx)
            }

            ################################
            # SU: Number of summer days
            if(is.SU & (jTmaxmin | jTmax)){
                pars.SU <- list(min.frac = 0.95, aggr.fun = "count",
                                opr.fun = ">", opr.thres = GeneralParameters$Indices$upTX)
                SU <- climdex_aggr.fun(tmax.data, GeneralParameters$start.july,
                                        ndim, pars.SU, pars.trend, index.NS1)
                writeCdtDatasetChunk.sequence(SU$year, chunkcalc[[chkj]], index.data.year,
                                            paths.index[["SU"]]$dir.year.data, do.par = do.parChunk)
                writeCdtDatasetChunk.sequence(SU$trend, chunkcalc[[chkj]], index.data.trend,
                                            paths.index[["SU"]]$dir.trend.data, do.par = do.parChunk)
                rm(SU)
            }

            ################################
            # ID: Number of icing days
            if(is.ID & (jTmaxmin | jTmax)){
                pars.ID <- list(min.frac = 0.95, aggr.fun = "count",
                                opr.fun = "<", opr.thres = GeneralParameters$Indices$loTX)
                ID <- climdex_aggr.fun(tmax.data, GeneralParameters$start.july,
                                        ndim, pars.ID, pars.trend, index.NS1)
                writeCdtDatasetChunk.sequence(ID$year, chunkcalc[[chkj]], index.data.year,
                                            paths.index[["ID"]]$dir.year.data, do.par = do.parChunk)
                writeCdtDatasetChunk.sequence(ID$trend, chunkcalc[[chkj]], index.data.trend,
                                            paths.index[["ID"]]$dir.trend.data, do.par = do.parChunk)
                rm(ID)
            }

            ################################
            # FD: Number of frost days
            if(is.FD & (jTmaxmin | jTmin)){
                pars.FD <- list(min.frac = 0.95, aggr.fun = "count",
                                opr.fun = "<", opr.thres = GeneralParameters$Indices$loTN)
                FD <- climdex_aggr.fun(tmin.data, GeneralParameters$start.july,
                                        ndim, pars.FD, pars.trend, index.NS1)
                writeCdtDatasetChunk.sequence(FD$year, chunkcalc[[chkj]], index.data.year,
                                            paths.index[["FD"]]$dir.year.data, do.par = do.parChunk)
                writeCdtDatasetChunk.sequence(FD$trend, chunkcalc[[chkj]], index.data.trend,
                                            paths.index[["FD"]]$dir.trend.data, do.par = do.parChunk)
                rm(FD)
            }

            ################################
            # TR: Number of tropical nights
            if(is.TR & (jTmaxmin | jTmin)){
                pars.TR <- list(min.frac = 0.95, aggr.fun = "count",
                                opr.fun = ">", opr.thres = GeneralParameters$Indices$upTN)
                TR <- climdex_aggr.fun(tmin.data, GeneralParameters$start.july,
                                        ndim, pars.TR, pars.trend, index.NS1)
                writeCdtDatasetChunk.sequence(TR$year, chunkcalc[[chkj]], index.data.year,
                                            paths.index[["TR"]]$dir.year.data, do.par = do.parChunk)
                writeCdtDatasetChunk.sequence(TR$trend, chunkcalc[[chkj]], index.data.trend,
                                            paths.index[["TR"]]$dir.trend.data, do.par = do.parChunk)
                rm(TR)
            }

            ################################
            if(is.TN10p | is.TN90p | is.TX10p | is.TX90p | is.WSDI | is.CSDI)
            {
                year <- as.numeric(substr(daty, 1, 4))
                if(!GeneralParameters$baseYear$all.years){
                    iyear <- year >= GeneralParameters$baseYear$start.year &
                            year <= GeneralParameters$baseYear$end.year
                }else iyear <- rep(TRUE, length(year))

                index.clim <- cdt.index.Climatologies(daty[iyear], 'daily', GeneralParameters$baseYear$window)

                if(GeneralParameters$bootstrap & (is.TN10p | is.TN90p | is.TX10p | is.TX90p))
                {
                    AllYearBoot <- lapply(index.clim$index, function(ix){
                        nx <- length(ix)
                        yr <- substr(daty[iyear][ix], 1, 4)
                        year.clm <- unique(yr)
                        nBoot <- length(year.clm) - 1
                        yearBoot <- matrix(0, nrow = nx, ncol = nBoot)

                        for(k in 1:nBoot){
                            kyr <- yr %in% year.clm[k]
                            s2 <- ix[!kyr]
                            nout <- length(which(kyr))
                            s1 <- ix[yr %in% year.clm[k + 1]]
                            if(length(s1) > nout) s1 <- s1[1:nout]
                            if(length(s1) < nout) s1 <- c(s1, sample(s2, nout - length(s1)))
                            yearBoot[, k] <- c(s1, s2)
                        }
                        yearBoot
                    })

                    nBoot <- ncol(AllYearBoot[[1]])
                    sqClim <- seq_along(index.clim$index)

                    if(jTmaxmin | jTmin){
                        Q1090 <- Reduce('+', lapply(seq(nBoot), function(i){
                                index.clim0 <- index.clim
                                index.clim0$index <- lapply(sqClim, function(j) AllYearBoot[[j]][, i])

                                xquant <- .cdt.quantile.Climatologies(index.clim0, tmin.data[iyear, , drop = FALSE],
                                                                    probs = c(0.1, 0.9), GeneralParameters$baseYear$min.year,
                                                                    'daily', GeneralParameters$baseYear$window)
                                xquant[[1]] <- xquant[[1]] - 1e-05
                                xquant[[2]] <- xquant[[2]] + 1e-05
                                do.call(rbind, xquant)
                            })
                        )

                        Q1090 <- Q1090 / nBoot
                        tminQ1090 <- list("10%" = NULL, "90%" = NULL)
                        tminQ1090[["10%"]] <- Q1090[sqClim, , drop = FALSE]
                        tminQ1090[["90%"]] <- Q1090[sqClim[length(sqClim)] + sqClim, , drop = FALSE]
                        rm(Q1090)
                    }


                    if(jTmaxmin | jTmax){
                        Q1090 <- Reduce('+', lapply(seq(nBoot), function(i){
                                index.clim0 <- index.clim
                                index.clim0$index <- lapply(sqClim, function(j) AllYearBoot[[j]][, i])

                                xquant <- .cdt.quantile.Climatologies(index.clim0, tmax.data[iyear, , drop = FALSE],
                                                                    probs = c(0.1, 0.9), GeneralParameters$baseYear$min.year,
                                                                    'daily', GeneralParameters$baseYear$window)
                                xquant[[1]] <- xquant[[1]] - 1e-05
                                xquant[[2]] <- xquant[[2]] + 1e-05
                                do.call(rbind, xquant)
                            })
                        )

                        Q1090 <- Q1090 / nBoot
                        tmaxQ1090 <- list("10%" = NULL, "90%" = NULL)
                        tmaxQ1090[["10%"]] <- Q1090[sqClim, , drop = FALSE]
                        tmaxQ1090[["90%"]] <- Q1090[sqClim[length(sqClim)] + sqClim, , drop = FALSE]
                        rm(Q1090)
                    }

                    rm(AllYearBoot)
                }else{
                    if(jTmaxmin | jTmin){
                        tminQ1090 <- .cdt.quantile.Climatologies(index.clim, tmin.data[iyear, , drop = FALSE],
                                                                probs = c(0.1, 0.9), GeneralParameters$baseYear$min.year,
                                                                'daily', GeneralParameters$baseYear$window)
                    
                        tminQ1090[[1]] <- tminQ1090[[1]] - 1e-05
                        tminQ1090[[2]] <- tminQ1090[[2]] + 1e-05
                    }

                    if(jTmaxmin | jTmax){
                        tmaxQ1090 <- .cdt.quantile.Climatologies(index.clim, tmax.data[iyear, , drop = FALSE],
                                                                probs = c(0.1, 0.9), GeneralParameters$baseYear$min.year,
                                                                'daily', GeneralParameters$baseYear$window)

                        tmaxQ1090[[1]] <- tmaxQ1090[[1]] - 1e-05
                        tmaxQ1090[[2]] <- tmaxQ1090[[2]] + 1e-05
                    }
                }

                index.anom <- cdt.index.Anomalies(daty, index.clim, "daily")
            }

            ################################
            # TX10p: Percentage of days when TX < 10th percentile
            if(is.TX10p & (jTmaxmin | jTmax)){
                Q10 <- .cdt.Anomalies(index.anom$index, tmax.data, tmaxQ1090[["10%"]], NULL, "Difference")
                pars.Q10 <- list(min.frac = 0.95, aggr.fun = "count", opr.fun = "<", opr.thres = 0)
                TX10p <- climdex_aggr.fun(Q10, GeneralParameters$start.july,
                                        ndim, pars.Q10, pars.trend, index.NS1,
                                        month.data = FALSE, index.M = NULL,
                                        Exceedance.rate = TRUE)
                rm(Q10)
                writeCdtDatasetChunk.sequence(TX10p$year, chunkcalc[[chkj]], index.data.year,
                                            paths.index[["TX10p"]]$dir.year.data, do.par = do.parChunk)
                writeCdtDatasetChunk.sequence(TX10p$trend, chunkcalc[[chkj]], index.data.trend,
                                            paths.index[["TX10p"]]$dir.trend.data, do.par = do.parChunk)
                rm(TX10p)
            }

            ################################
            # TX90p: Percentage of days when TX > 90th percentile
            if(is.TX90p & (jTmaxmin | jTmax)){
                Q90 <- .cdt.Anomalies(index.anom$index, tmax.data, tmaxQ1090[["90%"]], NULL, "Difference")
                pars.Q90 <- list(min.frac = 0.95, aggr.fun = "count", opr.fun = ">", opr.thres = 0)
                TX90p <- climdex_aggr.fun(Q90, GeneralParameters$start.july,
                                        ndim, pars.Q90, pars.trend, index.NS1,
                                        month.data = FALSE, index.M = NULL,
                                        Exceedance.rate = TRUE)
                rm(Q90)
                writeCdtDatasetChunk.sequence(TX90p$year, chunkcalc[[chkj]], index.data.year,
                                            paths.index[["TX90p"]]$dir.year.data, do.par = do.parChunk)
                writeCdtDatasetChunk.sequence(TX90p$trend, chunkcalc[[chkj]], index.data.trend,
                                            paths.index[["TX90p"]]$dir.trend.data, do.par = do.parChunk)
                rm(TX90p)
            }

            ################################
            # TN10p: Percentage of days when TN < 10th percentile
            if(is.TN10p & (jTmaxmin | jTmin)){
                Q10 <- .cdt.Anomalies(index.anom$index, tmin.data, tminQ1090[["10%"]], NULL, "Difference")
                pars.Q10 <- list(min.frac = 0.95, aggr.fun = "count", opr.fun = "<", opr.thres = 0)
                TN10p <- climdex_aggr.fun(Q10, GeneralParameters$start.july,
                                        ndim, pars.Q10, pars.trend, index.NS1,
                                        month.data = FALSE, index.M = NULL,
                                        Exceedance.rate = TRUE)
                rm(Q10)
                writeCdtDatasetChunk.sequence(TN10p$year, chunkcalc[[chkj]], index.data.year,
                                            paths.index[["TN10p"]]$dir.year.data, do.par = do.parChunk)
                writeCdtDatasetChunk.sequence(TN10p$trend, chunkcalc[[chkj]], index.data.trend,
                                            paths.index[["TN10p"]]$dir.trend.data, do.par = do.parChunk)
                rm(TN10p)
            }

            ################################
            # TN90p: Percentage of days when TN > 90th percentile
            if(is.TN90p & (jTmaxmin | jTmin)){
                Q90 <- .cdt.Anomalies(index.anom$index, tmin.data, tminQ1090[["90%"]], NULL, "Difference")
                pars.Q90 <- list(min.frac = 0.95, aggr.fun = "count", opr.fun = ">", opr.thres = 0)
                TN90p <- climdex_aggr.fun(Q90, GeneralParameters$start.july,
                                        ndim, pars.Q90, pars.trend, index.NS1,
                                        month.data = FALSE, index.M = NULL,
                                        Exceedance.rate = TRUE)
                rm(Q90)
                writeCdtDatasetChunk.sequence(TN90p$year, chunkcalc[[chkj]], index.data.year,
                                            paths.index[["TN90p"]]$dir.year.data, do.par = do.parChunk)
                writeCdtDatasetChunk.sequence(TN90p$trend, chunkcalc[[chkj]], index.data.trend,
                                            paths.index[["TN90p"]]$dir.trend.data, do.par = do.parChunk)
                rm(TN90p)
            }

            ################################
            # WSDI: Warm spell duration index
            if(is.WSDI & (jTmaxmin | jTmax)){
                Q90 <- .cdt.Anomalies(index.anom$index, tmax.data, tmaxQ1090[["90%"]], NULL, "Difference")
                pars.WSDI <- list(min.frac = 0.95, aggr.fun = "count.rle.nb",
                                    opr.fun = ">", opr.thres = 0,
                                    rle.fun = ">=", rle.thres = 6)
                WSDI <- climdex_aggr.fun(Q90, GeneralParameters$start.july,
                                        ndim, pars.WSDI, pars.trend, index.NS1)
                rm(Q90)
                writeCdtDatasetChunk.sequence(WSDI$year, chunkcalc[[chkj]], index.data.year,
                                            paths.index[["WSDI"]]$dir.year.data, do.par = do.parChunk)
                writeCdtDatasetChunk.sequence(WSDI$trend, chunkcalc[[chkj]], index.data.trend,
                                            paths.index[["WSDI"]]$dir.trend.data, do.par = do.parChunk)
                rm(WSDI)
            }

            ################################
            # CSDI: Cold spell duration index
            if(is.CSDI & (jTmaxmin | jTmin)){
                Q10 <- .cdt.Anomalies(index.anom$index, tmin.data, tminQ1090[["10%"]], NULL, "Difference")
                pars.CSDI <- list(min.frac = 0.95, aggr.fun = "count.rle.nb",
                                    opr.fun = "<", opr.thres = 0,
                                    rle.fun = ">=", rle.thres = 6)
                CSDI <- climdex_aggr.fun(Q10, GeneralParameters$start.july,
                                        ndim, pars.CSDI, pars.trend, index.NS1)
                rm(Q10)
                writeCdtDatasetChunk.sequence(CSDI$year, chunkcalc[[chkj]], index.data.year,
                                            paths.index[["CSDI"]]$dir.year.data, do.par = do.parChunk)
                writeCdtDatasetChunk.sequence(CSDI$trend, chunkcalc[[chkj]], index.data.trend,
                                            paths.index[["CSDI"]]$dir.trend.data, do.par = do.parChunk)
                rm(CSDI)
            }

            ################################
            # DTR: Daily temperature range
            if(is.DTR & jTmaxmin){
                tmp <- tmax.data - tmin.data
                pars.DTR <- list(min.frac = 0.95, aggr.fun = "mean")
                DTR <- climdex_aggr.fun(tmp, GeneralParameters$start.july,
                                        ndim, pars.DTR, pars.trend, index.NS1,
                                        month.data = FALSE, index.M = NULL)
                rm(tmp)
                writeCdtDatasetChunk.sequence(DTR$year, chunkcalc[[chkj]], index.data.year,
                                            paths.index[["DTR"]]$dir.year.data, do.par = do.parChunk)
                writeCdtDatasetChunk.sequence(DTR$trend, chunkcalc[[chkj]], index.data.trend,
                                            paths.index[["DTR"]]$dir.trend.data, do.par = do.parChunk)
                rm(DTR)
            }

            ################################
            # GSL: Growing season length
            if(is.GSL & jTmaxmin){
                index.NS0 <- climdex.north.south(index.year.N, index.year.S, TRUE, latitude, 0.9)
                index.NS1 <- index.NS.GSL
                index.NS1$index.S$idx <- index.NS0$index.S$idx
                index.NS1$index.S$ncol <- index.NS0$index.S$ncol
                index.NS1$index.N$idx <- index.NS0$index.N$idx
                index.NS1$index.N$ncol <- index.NS0$index.N$ncol
                rm(index.NS0)

                ################################

                tmean <- (tmax.data + tmin.data)/2

                ndim <- list(ncol = d.ncol, y.nrow = length(index.NS.GSL$year))
                pars.GSL <- list(min.frac = 0.95,
                                thres = GeneralParameters$Indices$thresGSL,
                                days = GeneralParameters$Indices$dayGSL)
                GSL <- climdex_GSL.fun(tmean, daty, index.NS1, ndim, pars.GSL, pars.trend.GSL)
                rm(tmean)
                writeCdtDatasetChunk.sequence(GSL$year, chunkcalc[[chkj]], index.data.year,
                                            paths.index[["GSL"]]$dir.year.data, do.par = do.parChunk)
                writeCdtDatasetChunk.sequence(GSL$trend, chunkcalc[[chkj]], index.data.trend,
                                            paths.index[["GSL"]]$dir.trend.data, do.par = do.parChunk)
                rm(GSL)
            }

            ################################
            if(!noTmin) rm(tmin.data)
            if(!noTmax) rm(tmax.data)
            rm(index.NS1); gc()
        })

        ######################

        x <- index.out$coords$mat$x
        y <- index.out$coords$mat$y
        nx <- length(x)
        ny <- length(y)
        dx <- ncdim_def("Lon", "degreeE", x)
        dy <- ncdim_def("Lat", "degreeN", y)
        xy.dim <- list(dx, dy)

        ######################
        trend.vars.name <- c("slope", "std.slope", "t.value.slope", "p.value.slope",
                            "intercept", "std.intercept", "t.value.intercept",
                            "p.value.intercept", "R2", "sigma")
        trend.vars.longname <- c(
                                "Slope - Estimate", "Slope - Standard Error", "Slope t-value", "Slope p-value Pr(>t)",
                                "Intercept - Estimate", "Intercept - Standard Error", "Intercept t-value", "Intercept p-value Pr(>t)",
                                "Multiple R-squared", "Residual Standard Error"
                                )
        trend.vars.grd <- lapply(seq_along(trend.vars.name), function(j){
            grd <- ncvar_def(trend.vars.name[j], "", xy.dim, NA, longname = trend.vars.longname[j], prec = "float", compression = 9)
            grd
        })

        ######################

        ret <- lapply(indxlst[is.indxlst], function(nom.idx){
            dir.year <- file.path(dir.netcdf, nom.idx, 'Yearly')
            dir.trend <- file.path(dir.netcdf, nom.idx, 'Trend')
            dir.create(dir.year, showWarnings = FALSE, recursive = TRUE)
            dir.create(dir.trend, showWarnings = FALSE, recursive = TRUE)

            YEAR <- if(nom.idx == "GSL") index.NS.GSL$year else index.NS$year

            vars <- varInfo[[nom.idx]]
            nc.grd <- ncvar_def(vars$name, vars$units, xy.dim, -99, vars$longname, vars$prec, compression = 9)
            data.year <- readCdtDatasetChunk.multi.dates.order(paths.index[[nom.idx]]$file.index.year, YEAR)
            for(j in seq_along(YEAR)){
                don.year <- data.year[j, ]
                dim(don.year) <- c(nx, ny)
                don.year[is.na(don.year)] <- -99

                filenc <- file.path(dir.year, paste0(nom.idx, "_", YEAR[j], ".nc"))
                nc <- nc_create(filenc, nc.grd)
                ncvar_put(nc, nc.grd, don.year)
                nc_close(nc)
            }
            rm(don.year, data.year)

            data.trend <- readCdtDatasetChunk.multi.dates.order(paths.index[[nom.idx]]$file.index.trend, trend.vars)
            outfile <- file.path(dir.trend, paste0(nom.idx, ".nc"))
            nc <- nc_create(outfile, trend.vars.grd)
            for(j in seq_along(trend.vars.grd)){
                trend.tmp <- data.trend[j, ]
                dim(trend.tmp) <- c(nx, ny)
                ncvar_put(nc, trend.vars.grd[[j]], trend.tmp)
            }
            nc_close(nc)
            rm(trend.tmp, data.trend)
            return(0)
        })

        ######################

        output <- list(params = GeneralParameters,
                        year = index.NS$year,
                        year.gsl = index.NS.GSL$year,
                        data = index.out$coords$mat)
    }

    #########################################

    .cdtData$EnvData$output <- output
    .cdtData$EnvData$PathData <- outDIR

    con <- gzfile(file.index, compression = 7)
    open(con, "wb")
    saveRDS(output, con)
    close(con)

    return(0)
}
