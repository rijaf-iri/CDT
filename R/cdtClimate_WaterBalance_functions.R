
## Water balance
Water.Balance <- function(rain, etp, capacity.max = 100, wb1 = 0){
    rr_na <- is.na(rain)
    et_na <- is.na(etp)
    rain[rr_na] <- 0
    etp[et_na] <- 0

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

    ina <- colSums(!rr_na & !et_na) == 0
    water.balance[, ina] <- NA

    return(water.balance)
}

#############################

# http://iridl.ldeo.columbia.edu/dochelp/Documentation/details/index.html?func=:Water_Balance

runoff_fun <- function(precip, win = 7){
    cumR <- cdt.roll.fun(precip/2.0, win, align = "right")
    tmp <- cumR * ifelse(precip >= 12.5, 1, 0)

    classI <- c(6.25, 6.3, 19.0, 31.7, 44.4, 57.1, 69.9)
    tmpC <- findInterval(c(tmp), classI)
    dim(tmpC) <- dim(tmp)

    PPcoefs <- c(0.0, 0.0, 0.0, 0.858, -0.895, 0.0028, -1.14, 0.042, 0.0026, -2.34,
                 0.12, 0.0026, -2.36, 0.19, 0.0026, -2.78, 0.25, 0.0026, -3.17, 0.32,
                 0.0024, -4.21, 0.438, 0.0018)
    PPn <- matrix(PPcoefs, ncol = 3, byrow = TRUE)

    tmpPPn <- log(precip)
    tmpPPn <- lapply(0:2, function(i) exp(i * tmpPPn))
    tmp <- lapply(1:3, function(i){
        Pcoef <- PPn[, i][c(tmpC + 1)]
        dim(Pcoef) <- dim(tmpC)
        tmpPPn[[i]] * Pcoef
    })
    tmp <- Reduce('+', tmp)
    tmp[tmp < 0] <- 0
    tmp[is.nan(tmp)] <- 0

    return(tmp)
}

# crops <- list(InitCond = 0.33, calib = 0.49,
#               InitKcStart = 1.05, InitDays = 65,
#               VegKcStart = 1.1, VegDays = 30, 
#               MidKcStart = 1.2, MidDays = 30, 
#               LateKcStart = 1.2, LateDays = 30,
#               LateKcEnd = 0.95,
#               PlantDday = 1, PlantDmonth = 10)

crop_coefficient_fun  <- function(crops, dates){
    kcVal <- do.call(c, crops[c('InitKcStart', 'VegKcStart', 'MidKcStart',
                                'LateKcStart', 'LateKcEnd')])
    kcVal <- c(kcVal, 1)

    lenDays <- do.call(c, crops[c('InitDays', 'VegDays', 'MidDays', 'LateDays')])
    eDays <- cumsum(lenDays)
    sDays <- c(1, eDays + 1)
    eDays <- c(eDays, sDays[length(sDays)] + 1)
    lenDays <- eDays - sDays + 1

    kcY <- rep(0, 365)
    for(j in seq_along(lenDays))
        kcY[sDays[j]:eDays[j]] <- (kcVal[j+1] - kcVal[j]) / lenDays[j]
    kcY[1] <- crops$InitKcStart
    kcY <- cumsum(kcY)

    PlantD <- as.Date(paste(2001, crops$PlantDmonth, crops$PlantDday, sep = "-"))
    dateKc <- PlantD + 0:364
    monKc <- format(dateKc, "%m%d")

    monTS <- substr(dates, 5, 8)
    im <- match(monTS, monKc)

    im[is.na(im)] <- which(monKc == "0228")
    Kc <- kcY[im]
    return(Kc)
}

# Peff: effective precipitation, matrix(nday, npts)
# ETc: crop evapotranspiration, matrix(nday, npts)
# TWA: total available water, vector(npts)

water_balance_fun <- function(Peff, ETc, TWA, crops){
    Peff[is.na(Peff)] <- 0
    ETc[is.na(ETc)] <- 0
    TWA[is.na(TWA)] <- 0

    ndays <- nrow(Peff)
    nbstn <- ncol(Peff)
    wb <- matrix(NA, nrow = ndays, ncol = nbstn)

    wb0 <- crops$InitCond * TWA
    ks0 <- pmin(wb0 / (crops$calib * TWA), 1)
    wb[1, ] <- wb0 + Peff[1, ] - ks0 * ETc[1, ]
    wb[1, ] <- pmin(pmax(wb[1, ], 0), TWA)

    for(iday in 2:ndays){
        ks <- pmin(wb[iday - 1, ] / (crops$calib * TWA), 1)
        wb[iday, ] <- wb[iday - 1, ] + Peff[iday, ] - ks * ETc[iday, ]
        wb[iday, ] <- pmin(pmax(wb[iday, ], 0), TWA)
    }

    return(wb)
}

wb_etcr_drain_fun <- function(Peff, ETc, TWA, crops){
    Peff[is.na(Peff)] <- 0
    ETc[is.na(ETc)] <- 0
    TWA[is.na(TWA)] <- 0

    ndays <- nrow(Peff)
    nbstn <- ncol(Peff)
    wb <- matrix(NA, nrow = ndays, ncol = nbstn)
    drain <- etcr <- wb

    wb0 <- crops$InitCond * TWA
    ks0 <- pmin(wb0 / (crops$calib * TWA), 1)
    etcr[1, ] <- ks0 * ETc[1, ]
    wb[1, ] <- wb0 + Peff[1, ] - etcr[1, ]
    wb[1, ] <- pmin(pmax(wb[1, ], 0), TWA)
    # drain[1, ] <- (Peff[1, ] - etcr[1, ]) - (wb[1, ] - wb[0, ])
    drain[1, ] <- 0

    for(iday in 2:ndays){
        ks <- pmin(wb[iday - 1, ] / (crops$calib * TWA), 1)
        etcr[iday, ] <- ks * ETc[iday, ]
        tmp <- Peff[iday, ] - etcr[iday, ]
        wb[iday, ] <- wb[iday - 1, ] + tmp
        wb[iday, ] <- pmin(pmax(wb[iday, ], 0), TWA)
        drain[iday, ] <- tmp - wb[iday, ] + wb[iday - 1, ]
    }

    return(list(wb = wb, etcr = etcr, drain = drain))
}

# WB: crop water balance, matrix(nday, npts)
# ETc: crop evapotranspiration, matrix(nday, npts)
# TWA: total available water, vector(npts)

reduced_ETcrop_fun <- function(WB, ETc, TWA, crops){
    WB[is.na(WB)] <- 0
    ETc[is.na(ETc)] <- 0
    TWA[is.na(TWA)] <- 0

    ndays <- nrow(WB)
    nbstn <- ncol(WB)
    etc_r <- matrix(NA, nrow = ndays, ncol = nbstn)

    wb0 <- crops$InitCond * TWA
    ks0 <- pmin(wb0 / (crops$calib * TWA), 1)
    etc_r[1, ] <- ks0 * ETc[1, ]

    for(iday in 2:ndays){
        ks <- pmin(WB[iday - 1, ] / (crops$calib * TWA), 1)
        etc_r[iday, ] <- ks * ETc[iday, ]
    }

    return(etc_r)
}
