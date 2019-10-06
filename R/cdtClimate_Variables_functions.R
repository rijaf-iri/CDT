
Extraterrestrial.Radiation <- function(lat, tstep = "daily"){
    phi <- pi * (lat) / 180
    dates <- seq(as.Date("2001-1-1"), as.Date("2001-12-31"), 'day')

    J <- as.numeric(strftime(dates, format = "%j"))
    fJ <- 2 * pi * J / 365
    dr <- 1 + 0.033 * cos(fJ)
    delta <- 0.409 * sin(fJ - 1.39)

    ws <- matrix(NA, nrow = 365, ncol = length(lat))
    sin2 <- cos2 <- ws
    for(j in seq_along(lat)){
        ws[, j] <- acos(-tan(phi[j]) * tan(delta))
        sin2[, j] <- sin(phi[j]) * sin(delta)
        cos2[, j] <- cos(phi[j]) * cos(delta)
    } 
    Ra <- (37.58603 * dr) * (ws * sin2 + cos2 * sin(ws))
    Ra[Ra < 0] <- 0
    if(tstep == "pentad"){
        irow <- as.numeric(format(dates, "%d")) %in% c(3, 8, 13, 18, 23, 28)
        Ra <- Ra[irow, , drop = FALSE]
    }
    if(tstep == "dekadal"){
        irow <- as.numeric(format(dates, "%d")) %in% c(5, 15, 25)
        Ra <- Ra[irow, , drop = FALSE]
    }
    if(tstep == "monthly"){
        irow <- as.numeric(format(dates, "%d")) == 15
        Ra <- Ra[irow, , drop = FALSE]
    }
    # Ra in MJ m-2 d-1
    return(Ra)
}

## Hargreaves evapotranspiration
Ref.ET.Hargreaves <- function(Tmax, Tmin, Ra, tstep = "daily", Precip = NULL){
    TM <- (Tmax + Tmin) / 2
    TR <- Tmax - Tmin
    TR[TR < 0] <- 0
    if(is.null(Precip)){
        # Original Hargreaves equation
        coefs <- if(tstep == "daily") c(0.0028, 19.1869) else c(0.0023, 17.8)
        ETP <- coefs[1] * (0.408 * Ra) * (TM + coefs[2]) * TR^0.5
    }else{
        # Hargreaves modified method
        coefs <- if(tstep == "daily") c(0.0019, 21.0584, 0.0874, 0.6278) else c(0.0013, 17, 0.0123, 0.76)
        PRE <- (TR - coefs[3] * Precip)^coefs[4]
        PRE[is.nan(PRE)] <- 0
        ETP <- coefs[1] * (0.408 * Ra) * (TM + coefs[2]) * PRE
    }
    # mm/day
    return(ETP)
}

## Water balance
Water.Balance <- function(rain, etp, capacity.max = 100, wb1 = 0){
    rain[is.na(rain)] <- 0
    etp[is.na(etp)] <- 0

    ndays <- nrow(rain)
    nbstn <- ncol(rain)

    if(length(wb1) == 1) wb1 <- rep(wb1, nbstn)
    minwb <- rep(0, nbstn)
    maxwb <- if(length(capacity.max) == 1) rep(capacity.max, nbstn) else capacity.max
    water.balance <- matrix(NA, nrow = ndays, ncol = nbstn)
    water.balance[1, ] <- wb1

    # simple water balance
    for(iday in 2:ndays){
        water.balance[iday, ] <- water.balance[iday-1, ] + rain[iday, ] - etp[iday, ]
        water.balance[iday, ] <- pmax(minwb, pmin(maxwb, water.balance[iday, ]))
    }

    return(water.balance)
}

## Compute Onset date
Season.Onset <- function(dates, precip, evapo = NULL, onset.pars, min.frac)
{
    method <- onset.pars$method
    total.days <- onset.pars$total.days
    rain.total <- onset.pars$rain.total
    min.rain.day <- onset.pars$min.rain.day
    dryspell.days <- onset.pars$dryspell.days
    dryspell <- onset.pars$dryspell
    thres.rain.day <- onset.pars$thres.rain.day
    etp.frac <- onset.pars$evapo.frac
    initCol <- ncol(precip)

    yearO <- if(onset.pars$latest$month <= onset.pars$earliest$month) 2015 else 2014
    search.days <- as.numeric(as.Date(paste(yearO, onset.pars$latest$month, onset.pars$latest$day, sep = '-'))
                    - as.Date(paste(2014, onset.pars$earliest$month, onset.pars$earliest$day, sep = '-'))) + 1

    wsearch <- 1:search.days
    if(length(wsearch) > nrow(precip)) wsearch <- wsearch[seq(nrow(precip))]
    MATdeb <- precip[wsearch, , drop = FALSE]
    dates <- dates[wsearch]
    if(method == 2) ETPdeb <- evapo[wsearch, , drop = FALSE]

    ## remove NA
    colID <- colSums(is.na(MATdeb)) / nrow(MATdeb) > (1 - min.frac)
    if(method == 2){
        etpID <- colSums(is.na(ETPdeb)) / nrow(ETPdeb) > (1 - min.frac)
        colID <- colID | etpID
    }
    colretenu <- seq(initCol)[!colID]
    MATdeb <- MATdeb[, !colID, drop = FALSE]
    if(method == 2) ETPdeb <- ETPdeb[, !colID, drop = FALSE]
    if(ncol(MATdeb) == 0) return(rep(NA, initCol))

    MATdeb[is.na(MATdeb)] <- 0
    if(method == 2) ETPdeb[is.na(ETPdeb)] <- 0

    MATdeb.truncated <- rbind(matrix(0, nrow = total.days, ncol = ncol(MATdeb)), utils::head(MATdeb, -total.days))
    if(method == 2) ETPdeb.truncated <- rbind(matrix(0, nrow = total.days, ncol = ncol(ETPdeb)), utils::head(ETPdeb, -total.days))

    MATtotal <- (matrixStats::colCumsums(MATdeb) - matrixStats::colCumsums(MATdeb.truncated))
    if(method == 2) ETPtotal <- (matrixStats::colCumsums(ETPdeb) - matrixStats::colCumsums(ETPdeb.truncated))

    istotal <- if(method == 2) MATtotal >= etp.frac * ETPtotal else MATtotal >= rain.total

    ## remove no onset
    colID <- colSums(istotal) == 0
    colretenu <- colretenu[!colID]
    istotal <- istotal[, !colID, drop = FALSE]
    MATdeb <- MATdeb[, !colID, drop = FALSE]
    if(ncol(istotal) == 0) return(rep(NA, initCol))

    ## remove 1st day onset
    colID <- colSums(istotal) == nrow(istotal)
    colDEB <- colretenu[colID]
    colretenu <- colretenu[!colID]
    istotal <- istotal[, !colID, drop = FALSE]
    MATdeb <- MATdeb[, !colID, drop = FALSE]
    if(ncol(istotal) == 0){
        res <- rep(NA, initCol)
        if(length(colDEB) > 0) res[colDEB] <- 1
        return(dates[res])
    }

    onset <- lapply(seq(ncol(istotal)), function(j){
        y <- istotal[, j]
        ipos <- which(y)

        if(method %in% 1:2){
            istart <- ipos[1]
        }else{
            x <- MATdeb[, j] >= thres.rain.day

            if(method %in% 4:5){
                is.onset <- sapply(ipos, function(i){
                    x1 <- !x[i + (1:dryspell.days)]
                    x1 <- x1[!is.na(x1)]
                    x2 <- rle(x1)
                    !any(x2$lengths[x2$values] >= dryspell)
                })

                if(!any(is.onset)) return(NA)
                ipos <- ipos[is.onset]
            }

            if(method == 4) istart <- ipos[1]

            if(method %in% c(3, 5)){
                istart <- NA
                for(i in ipos){
                    io <- i - (total.days:1) + 1
                    io <- io[io > 0]
                    if(sum(x[io]) >= min.rain.day){
                        istart <- i
                        break
                    }
                }
            }
        }

        istart
    })
    onset <- do.call(c, onset)

    res <- rep(NA, initCol)
    if(length(colDEB) > 0) res[colDEB] <- 1
    res[colretenu] <- onset
    if(all(is.na(res))) res else dates[res]
}

## Onset date wrapper
cdt.Season.Onset <- function(dates, subdiv, criteria,
                            rr.data, et.data = NULL,
                            min.frac = 0.95)
{
    res <- lapply(seq_along(subdiv), function(j){
        onset.pars <- criteria[[j]]
        index <- cdt.index.DailyYears(dates, onset.pars$earliest$month, onset.pars$earliest$day)
        len.index <- length(index$index)

        precip <- rr.data[, subdiv[[j]], drop = FALSE]
        evapo <- if(onset.pars$method == 2) et.data[, subdiv[[j]], drop = FALSE] else NULL

        ons <- lapply(seq(len.index), function(ii){
            idx <- index$index[[ii]]
            rr <- precip[idx, , drop = FALSE]
            evp <- evapo[idx, , drop = FALSE]
            daty <- dates[idx]
            min.frac <- if(ii == len.index) 0 else min.frac
            Season.Onset(daty, rr, evp, onset.pars, min.frac)
        })
        ons <- do.call(rbind, ons)
        start.date <- as.character(index$range.date[, 1])
        list(onset = ons, start = start.date)
    })

    start.date <- lapply(res, function(x) as.Date(x$start, "%Y%m%d"))
    start.date <- as.Date(matrixStats::rowMins(do.call(cbind, start.date)), origin = "1970-1-1")
    onset <- matrix(NA, length(start.date), ncol(rr.data))
    for(j in seq_along(subdiv)) onset[, subdiv[[j]]] <- res[[j]]$onset

    onset.num <- as.Date(onset, "%Y%m%d")
    dim(onset.num) <- dim(onset)

    list(onset.date = onset, onset.num = onset.num, start.date = start.date)
}

## Compute Cessation date using water balance
Season.Cessation.WB <- function(dates, waterbalance, onset.pars, min.frac)
{
    total.days <- onset.pars$total.days
    min.wb <- onset.pars$min.wb
    if(min.wb == 0) min.wb <- 0.01
    initCol <- ncol(waterbalance)

    yearO <- if(onset.pars$latest$month <= onset.pars$earliest$month) 2015 else 2014
    search.days <- as.numeric(as.Date(paste(yearO, onset.pars$latest$month, onset.pars$latest$day, sep = '-'))
                    - as.Date(paste(2014, onset.pars$earliest$month, onset.pars$earliest$day, sep = '-'))) + 1

    wsearch <- 1:search.days
    if(length(wsearch) > nrow(waterbalance)) wsearch <- wsearch[seq(nrow(waterbalance))]
    MATdeb <- waterbalance[wsearch, , drop = FALSE]
    dates <- dates[wsearch]

    ## remove NA col
    colID <- colSums(is.na(MATdeb)) / nrow(MATdeb) > (1 - min.frac)
    colretenu <- seq(initCol)[!colID]
    MATdeb <- MATdeb[, !colID, drop = FALSE]
    if(ncol(MATdeb) == 0) return(rep(NA, initCol))

    #### MATdeb[is.na(MATdeb)] <- 0
    dropWB <- MATdeb < min.wb & !is.na(MATdeb)

    ## remove no cessation or replace to the latest date
    colID <- colSums(dropWB) == 0
    colFIN <- colretenu[colID]
    colretenu <- colretenu[!colID]
    dropWB <- dropWB[, !colID, drop = FALSE]
    ## MATdeb <- MATdeb[, !colID, drop = FALSE]

    # if(ncol(dropWB) == 0) return(rep(NA, initCol))
    if(ncol(dropWB) == 0){
        res <- rep(NA, initCol)
        if(length(colFIN) > 0) res[colFIN] <- length(wsearch)
        return(dates[res])
    }

    ## remove 1st day cessation
    colID <- colSums(dropWB) == nrow(dropWB)
    colDEB <- colretenu[colID]
    colretenu <- colretenu[!colID]
    dropWB <- dropWB[, !colID, drop = FALSE]
    ## MATdeb <- MATdeb[, !colID, drop = FALSE]
    if(ncol(dropWB) == 0){
        res <- rep(NA, initCol)
        if(length(colDEB) > 0) res[colDEB] <- 1
        return(dates[res])
    }

    cess <- lapply(seq(ncol(dropWB)), function(j){
        xx <- dropWB[, j]
        rle.succ <- rle(xx)
        isfin <- rle.succ$lengths >= total.days & rle.succ$values
        # if(!any(isfin)) return(NA)
        if(!any(isfin)) return(length(xx))
        pos <- cumsum(rle.succ$lengths)
        ipos <- which(isfin) - 1
        pos <- if(ipos[1] == 0) 1 else pos[ipos[1]] + 1
        pos
    })
    cess <- do.call(c, cess)

    res <- rep(NA, initCol)
    if(length(colDEB) > 0) res[colDEB] <- 1
    if(length(colFIN) > 0) res[colFIN] <- length(wsearch)
    res[colretenu] <- cess
    if(all(is.na(res))) res else dates[res]
}

## Compute Cessation date using cumulated rainfall and evapotranspiration
Season.Cessation.RR_PET <- function(dates, rr.data, et.data, onset.pars, min.frac)
{
    initCol <- ncol(rr.data)

    yearO <- if(onset.pars$latest$month <= onset.pars$earliest$month) 2015 else 2014
    search.days <- as.numeric(as.Date(paste(yearO, onset.pars$latest$month, onset.pars$latest$day, sep = '-'))
                    - as.Date(paste(2014, onset.pars$earliest$month, onset.pars$earliest$day, sep = '-'))) + 1

    wsearch <- seq(search.days + onset.pars$accum.day - 1)
    if(length(wsearch) > nrow(rr.data)) wsearch <- wsearch[seq(nrow(rr.data))]

    MATrr <- rr.data[wsearch, , drop = FALSE]
    MATet <- et.data[wsearch, , drop = FALSE]
    dates <- dates[wsearch][-seq(onset.pars$accum.day - 1)]

    ## remove NA col
    colID0 <- colSums(is.na(MATrr)) / nrow(MATrr) > (1 - min.frac)
    colID1 <- colSums(is.na(MATet)) / nrow(MATet) > (1 - min.frac)
    colID <- colID0 & colID1
    colretenu <- seq(initCol)[!colID]
    MATrr <- MATrr[, !colID, drop = FALSE]
    MATet <- MATet[, !colID, drop = FALSE]
    if(ncol(MATrr) == 0) return(rep(NA, initCol))

    min.data <- onset.pars$accum.day - 1
    if(min.data < 1) min.data <- 1
    MATrr <- cdt.roll.fun(MATrr, onset.pars$accum.day, "sum", na.rm = TRUE, min.data = min.data, align = "right")
    MATet <- cdt.roll.fun(MATet, onset.pars$accum.day, "sum", na.rm = TRUE, min.data = min.data, align = "right")
    MATrr <- MATrr[-seq(onset.pars$accum.day - 1), , drop = FALSE]
    MATet <- MATet[-seq(onset.pars$accum.day - 1), , drop = FALSE]
    matnc <- ncol(MATrr)
    matnr <- nrow(MATrr)

    cess <- lapply(seq(matnc), function(j){
        rr <- MATrr[, j]
        et <- MATet[, j]
        nonNA <- !is.na(rr) & !is.na(et)
        rrinf <- nonNA & (rr < et * onset.pars$evapo.frac)
        if(onset.pars$accum.method == 2){
            rrsup <- nonNA & (rr > et)
            if(any(rrsup)){
                isup <- which(rrsup)
                jsup <- isup[length(isup)]
                xsup <- rep(FALSE, matnr)
                xsup[jsup:matnr] <- TRUE
                ###
                all.isup <- isup[diff(isup) > 1]
                cs <- which(rrinf & xsup)[1]
                while (is.na(cs) & length(all.isup) > 0)
                {
                    jsup <- all.isup[length(all.isup)]
                    xsup <- rep(FALSE, matnr)
                    xsup[jsup:matnr] <- TRUE
                    cs <- which(rrinf & xsup)[1]
                    all.isup <- all.isup[-length(all.isup)]
                }
                ###
                rrinf <- rrinf & xsup
                if(all(!rrinf)) rrinf[matnr] <- TRUE
            }
        }
        cs <- which(rrinf)[1]
        ## replace no cessation to the latest date
        # if(length(cs) == 0) cs <- matnr
        if(length(cs) == 0 | is.na(cs)) cs <- matnr
        cs
    })
    cess <- do.call(c, cess)

    res <- rep(NA, initCol)
    res[colretenu] <- cess
    if(all(is.na(res))) res else dates[res]
}

## Cessation date wrapper
cdt.Season.Cessation <- function(dates, subdiv, criteria,
                            wb.data = NULL, rr.data = NULL, et.data = NULL,
                            min.frac = 0.95)
{
    res <- lapply(seq_along(subdiv), function(j){
        onset.pars <- criteria[[j]]
        index <- cdt.index.DailyYears(dates, onset.pars$earliest$month, onset.pars$earliest$day)
        len.index <- length(index$index)

        if(onset.pars$method == 1){
            cess <- lapply(seq(len.index), function(ii){
                idx <- index$index[[ii]]
                watb <- wb.data[idx, subdiv[[j]], drop = FALSE]
                daty <- dates[idx]
                min.frac <- if(ii == len.index) 0 else min.frac
                Season.Cessation.WB(daty, watb, onset.pars, min.frac)
            })
        }
        if(onset.pars$method == 2){
            start.roll <- as.Date(paste(2015, onset.pars$earliest$month, onset.pars$earliest$day, sep = "-"))
            start.roll <- start.roll - onset.pars$accum.day + 1
            start.mon <- as.numeric(format(start.roll, "%m"))
            start.day <- as.numeric(format(start.roll, "%d"))
            index.roll <- cdt.index.DailyYears(dates, start.mon, start.day)

            cess <- lapply(seq(len.index), function(ii){
                idx <- index.roll$index[[ii]]
                rr <- rr.data[idx, subdiv[[j]], drop = FALSE]
                etp <- et.data[idx, subdiv[[j]], drop = FALSE]
                daty <- dates[idx]
                min.frac <- if(ii == len.index) 0 else min.frac
                Season.Cessation.RR_PET(daty, rr, etp, onset.pars, min.frac)
            })
        }
        cess <- do.call(rbind, cess)
        start.date <- as.character(index$range.date[, 1])
        list(cessation = cess, start = start.date)
    })

    start.date <- lapply(res, function(x) as.Date(x$start, "%Y%m%d"))
    start.date <- as.Date(matrixStats::rowMins(do.call(cbind, start.date)), origin = "1970-1-1")
    nbpts <- if(!is.null(wb.data)) ncol(wb.data) else ncol(rr.data)
    cessation <- matrix(NA, length(start.date), nbpts)
    for(j in seq_along(subdiv)) cessation[, subdiv[[j]]] <- res[[j]]$cessation

    cessation.num <- as.Date(cessation, "%Y%m%d")
    dim(cessation.num) <- dim(cessation)
    list(cessation.date = cessation, cessation.num = cessation.num, start.date = start.date)
}


