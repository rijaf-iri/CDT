
cdtValidation.Cont.Stats <- function(x, y){
    ina <- is.na(x) | is.na(y)
    x[ina] <- NA
    y[ina] <- NA

    nbcol <- ncol(x)
    nbrow <- nrow(x)
    naCol <- colSums(is.na(x))
    colNA <- naCol == nbrow
    nbX <- nbrow - naCol
    xmy <- y - x
    mnx <- colMeans(x, na.rm = TRUE)
    mny <- colMeans(y, na.rm = TRUE)
    varx <- matrixStats::colVars(x, na.rm = TRUE)
    vary <- matrixStats::colVars(y, na.rm = TRUE)
    X1 <- x - matrix(mnx, nbrow, nbcol, byrow = TRUE)
    Y1 <- y - matrix(mny, nbrow, nbcol, byrow = TRUE)
    YX1 <- y - matrix(mnx, nbrow, nbcol, byrow = TRUE)

    j <- 1
    sumj <- colSums(abs(xmy)^j, na.rm = TRUE)
    sumj[colNA] <- NA

    sum1 <- colSums(xmy^2, na.rm = TRUE)
    sum1[colNA] <- NA

    sum2 <- colSums(X1^2, na.rm = TRUE)
    sum2[colNA] <- NA

    sum3 <- colSums((xmy / x)^2, na.rm = TRUE)
    sum3[colNA] <- NA

    sum4 <- colSums((abs(YX1) + abs(X1))^2)
    sum4[colNA] <- NA

    ##################

    COV <- colSums(X1 * Y1, na.rm = TRUE) / (nbX - 1)
    COV[colNA] <- NA
    CORR <- sqrt(COV^2 / (varx * vary))

    abs_b <- abs(colSums(x * y, na.rm = TRUE) / colSums(x^2, na.rm = TRUE))
    BR2 <- ifelse(abs_b <= 1, abs_b * CORR^2, CORR^2 / abs_b)

   ##################

    # (Multiplicative) bias (1)
    BIAS <- colSums(y, na.rm = TRUE) / colSums(x, na.rm = TRUE)
    # Percent Bias (0)
    PBIAS <- 100 * colSums(xmy, na.rm = TRUE) / colSums(x, na.rm = TRUE)
    # Mean error (0)
    ME <- colMeans(xmy, na.rm = TRUE)

    # Mean absolute error (0)
    MAE <- colMeans(abs(xmy), na.rm = TRUE)
    # Root mean square error (0)
    RMSE <- sqrt(colMeans(xmy^2, na.rm = TRUE))

    # Nash-Sutcliffe Efficiency (1)
    NSE <- 1 - sum1 / sum2
    # Modified Nash-Sutcliffe efficiency (1)
    MNSE <- 1 - sumj / colSums(abs(X1)^j, na.rm = TRUE)
    # Relative Nash-Sutcliffe efficiency (1)
    RNSE <- 1 - sum3 / (sum2 / mnx^2)

    ##################

    # Index of Agreement (1)
    IOA <- 1 - sum1 / sum4
    # Modified index of agreement (1)
    MIOA <- 1 - sumj / colSums(abs(YX1) + abs(X1)^j)
    # Relative Index of Agreement (1)
    RIOA <- 1 - sum3 / (sum4 / mnx^2)

    ##################

    stats <- rbind(CORR, BR2, BIAS, PBIAS, ME, MAE, RMSE, NSE, MNSE, RNSE, IOA, MIOA, RIOA)
    descrip <- c('Correlation', 'Coefficient of determination (R2) multiplied by the regression slope',
                 'Bias', 'Percent Bias', 'Mean Error', 'Mean Absolute Error', 'Root Mean Square Error',
                 'Nash-Sutcliffe Efficiency', 'Modified Nash-Sutcliffe efficiency', 'Relative Nash-Sutcliffe efficiency',
                 'Index of Agreement', 'Modified index of agreement', 'Relative Index of Agreement')
    pscore <- c(1, 1, 1, 0, 0, 0, 0, 1, 1, 1, 1, 1, 1)

    stats[is.nan(stats)] <- NA
    stats[is.infinite(stats)] <- NA
    stats <- round(stats, 3)
    return(list(statistics = stats, description = descrip, perfect.score = pscore))
}

##################

cdtValidation.Categ.Stats <- function(x, y, dichotomous = list(thres = 1, fun = ">=")){
    ina <- is.na(x) | is.na(y)
    x[ina] <- NA
    y[ina] <- NA

    nbrow <- nrow(x)
    naCol <- colSums(is.na(x))
    colNA <- naCol == nbrow

    thres <- dichotomous$thres
    fun <- get(dichotomous$fun, mode = "function")
    YesOBS <- fun(x, thres)
    YesFCST <- fun(y, thres)

    ##################

    # hit
    Hit <- YesOBS & YesFCST
    # false alarm
    False <- !YesOBS & YesFCST
    # miss
    Miss <- YesOBS & !YesFCST
    # correct negative
    TrueNull <- !YesOBS & !YesFCST

    ##################

    # hit
    N1 <- colSums(Hit, na.rm = TRUE)
    N1[colNA] <- NA

    # false alarm
    N2 <- colSums(False, na.rm = TRUE)
    N2[colNA] <- NA

    # miss
    N3 <- colSums(Miss, na.rm = TRUE)
    N3[colNA] <- NA

    # correct negative
    N4 <- colSums(TrueNull, na.rm = TRUE)
    N4[colNA] <- NA

    ##################

    N <- N1 + N2 + N3 + N4

    FBS <- (N1 + N2) / (N1 + N3)
    CSI <- N1 / (N - N4)
    POD <- N1 / (N1 + N3)
    FAR <- N2 / (N1 + N2)
    POFD <- N2 / (N4 + N2)

    C0 <- N1 + N4
    E0 <- ((N1 + N3) * (N1 + N2) + (N2 + N4) * (N3 + N4)) / N
    HSS <- (C0 - E0) / (N - E0)

    ##################

    stats <- rbind(POD, POFD, FAR, FBS, CSI, HSS)
    descrip <- c('Probability Of Detection', 'Probability Of False Detection',
                 'False Alarm Ratio', 'Frequency Bias (Bias score)',
                 'Critical Success Index', 'Heidke Skill Score')
    pscore <- c(1, 0, 0, 1, 1, 1)

    stats[is.nan(stats)] <- NA
    stats[is.infinite(stats)] <- NA
    stats <- round(stats, 3)
    return(list(statistics = stats, description = descrip, perfect.score = pscore))
}

#######################################################
# POD ~ VHI
# FAR ~ VFAR
# MISS = 1-POD ~ VMI
# CSI ~ VCSI

cdtVolumetricQuantileStats <- function(x, y, thres = NULL){
    ina <- is.na(x) | is.na(y)
    x[ina] <- NA
    y[ina] <- NA

    nbcol <- ncol(x)
    nbrow <- nrow(x)
    if(is.null(thres)) stop("Threshold must not be null")


    if(length(thres) == 1){
       thres <- rep(thres, nbcol)
    }else{
        if(length(thres) != nbcol)
            stop("Length of threshold differs from the number of stations")

        if(all(is.na(thres)))
            stop("All threshold values are missing")
    }

    naThres <- is.na(thres)
    naCol <- colSums(is.na(x))
    colNA <- naCol == nbrow

    ##################

    SIM.gt <- sweep(y, 2, thres, FUN = ">")
    SIM.ge <- sweep(y, 2, thres, FUN = ">=")
    SIM.le <- sweep(y, 2, thres, FUN = "<=")
    OBS.gt <- sweep(x, 2, thres, FUN = ">")
    OBS.ge <- sweep(x, 2, thres, FUN = ">=")
    OBS.le <- sweep(x, 2, thres, FUN = "<=")

    ##################

    SIM.v <- y
    SIM.v[!SIM.ge] <- NA
    OBS.v <- x
    OBS.v[!OBS.ge] <- NA

    MQE.v <- colMeans(SIM.v - OBS.v, na.rm = TRUE)
    MQE.v[colNA] <- NA
    SIM.v <- colSums(SIM.v, na.rm = TRUE)
    SIM.v[colNA] <- NA
    OBS.v <- colSums(OBS.v, na.rm = TRUE)
    OBS.v[colNA] <- NA

    ##################

    Hit <- SIM.gt & OBS.gt
    False <- SIM.gt & OBS.le
    Miss <- SIM.le & OBS.gt

    ##################

    SIM.hit <- y
    SIM.hit[!Hit] <- NA
    SIM.hit.v <- colSums(SIM.hit, na.rm = TRUE)
    SIM.hit.i <- colSums(!is.na(SIM.hit), na.rm = TRUE)
    SIM.hit.v[colNA] <- NA
    SIM.hit.i[colNA] <- NA

    SIM.false <- y
    SIM.false[!False] <- NA
    SIM.false.v <- colSums(SIM.false, na.rm = TRUE)
    SIM.false.i <- colSums(!is.na(SIM.false), na.rm = TRUE)
    SIM.false.v[colNA] <- NA
    SIM.false.i[colNA] <- NA

    OBS.miss <- x
    OBS.miss[!Miss] <- NA
    OBS.miss.v <- colSums(OBS.miss, na.rm = TRUE)
    OBS.miss.i <- colSums(!is.na(OBS.miss), na.rm = TRUE)
    OBS.miss.v[colNA] <- NA
    OBS.miss.i[colNA] <- NA

    ##################

    # Mean Quantile Bias (1)
    MQB <- SIM.v / OBS.v
    # Mean Quantile Error (0)
    MQE <- MQE.v
    # Volumetric Hit Index (1) QPOD
    VHI <- SIM.hit.v / (SIM.hit.v + OBS.miss.v)
    # Volumetric False Alarm Ratio (0) QFAR
    VFAR <- SIM.false.v / (SIM.hit.v + SIM.false.v)
    # Volumetric Miss Index (0) QMISS
    VMI <- OBS.miss.v / (SIM.hit.v + OBS.miss.v)
    # Volumetric Critical Success Index (1) QCSI
    VCSI <- SIM.hit.v / (SIM.hit.v + OBS.miss.v + SIM.false.v)
    # Quantile Probability of Detection (1)
    QPOD <- SIM.hit.i / (SIM.hit.i + OBS.miss.i)
    # Quantile Miss Index (0)
    QMISS <- 1 - QPOD
    # Quantile False Alarm Ratio (0)
    QFAR <- SIM.false.i / (SIM.hit.i + SIM.false.i)
    # Quantile Critical Success Index (1)
    QCSI <- SIM.hit.i / (SIM.hit.i + OBS.miss.i + SIM.false.i)

    stats <- rbind(MQB, MQE, VHI, QPOD, VFAR, QFAR, VMI, QMISS, VCSI, QCSI)
    descrip <- c("Mean Quantile Bias", "Mean Quantile Error",
                 "Volumetric Hit Index", "Quantile Probability of Detection",
                 "Volumetric False Alarm Ratio", "Quantile False Alarm Ratio",
                 "Volumetric Miss Index", "Quantile Miss Index",
                 "Volumetric Critical Success Index", "Quantile Critical Success Index")
    pscore <- c(1, 0, 1, 1, 0, 0, 0, 0, 1, 1)
    stats[is.nan(stats)] <- NA
    stats[is.infinite(stats)] <- NA
    stats <- round(stats, 3)
    stats[, naThres] <- NA

    return(list(statistics = stats, description = descrip, perfect.score = pscore))
}
