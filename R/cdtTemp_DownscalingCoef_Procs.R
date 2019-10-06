
Temp_execCoefDown <- function(){
    outdir <- file.path(.cdtData$GalParams$IO.files$dir2save, paste0('CoefDownTemp', '_',
                        tools::file_path_sans_ext(.cdtData$GalParams$IO.files$STN.file)))
    dir.create(outdir, showWarnings = FALSE, recursive = TRUE)

    Insert.Messages.Out('Computing regression parameters ...', TRUE, "i")

    ## get data
    stnData <- getStnOpenData(.cdtData$GalParams$IO.files$STN.file)
    if(is.null(stnData)) return(NULL)
    stnData <- getCDTdataAndDisplayMsg(stnData, .cdtData$GalParams$period,
                                        .cdtData$GalParams$IO.files$STN.file)
    if(is.null(stnData)) return(NULL)

    ## get elevation data
    demInfo <- getNCDFSampleData(.cdtData$GalParams$IO.files$DEM.file)
    if(is.null(demInfo)){
        Insert.Messages.Out("Unable to read DEM data", format = TRUE)
        return(NULL)
    }

    jfile <- getIndex.AllOpenFiles(.cdtData$GalParams$IO.files$DEM.file)
    demData <- .cdtData$OpenFiles$Data[[jfile]][[2]]
    demData$lon <- demData$x
    demData$lat <- demData$y

    ## Compute regression parameters between station temperature and elevation
    .cdtData$GalParams$paramsGlmCoef <- list(stnData = stnData, demData = demData, outdir = outdir)
    ret <- Temp_CoefDownscaling()
    rm(stnData, demData)
    gc()
    if(!is.null(ret)){
        if(ret == 0) return(0)
        else return(ret)
    } else return(NULL)
}

#######################################################################################

Temp_CoefDownscaling <- function(){
    Insert.Messages.Out('Compute downscaling coefficients ...', TRUE, "i")

    paramsGlmCoef <- .cdtData$GalParams$paramsGlmCoef
    stnData <- paramsGlmCoef$stnData
    demData <- paramsGlmCoef$demData

    ijdem <- grid2pointINDEX(stnData, demData)
    stnData$dem <- demData$z[ijdem]
    stnData$dem[stnData$dem < 0] <- 0

    allyears <- .cdtData$GalParams$base.period$all.years
    year1 <- .cdtData$GalParams$base.period$start.year
    year2 <- .cdtData$GalParams$base.period$end.year
    minyear <- .cdtData$GalParams$base.period$min.year
    years <- as.numeric(substr(stnData$dates, 1, 4))

    if(length(unique(years)) < minyear){
        Insert.Messages.Out("No enough data to calculate the coefficients", format = TRUE)
        return(NULL)
    }

    iyrCoef <- if(allyears) rep(TRUE, length(years)) else years >= year1 & years <= year2

    stnData$data <- stnData$data[iyrCoef, , drop = FALSE]
    stnData$dates <- stnData$dates[iyrCoef]

    coef <- Coeff.downscaling.LapseRate(stnData, useClimato = FALSE, standardize = TRUE)

    outfile <- file.path(paramsGlmCoef$outdir, 'STN_DEM_GLM_COEF.txt')
    write.table(coef$coeff, file = outfile, col.names = TRUE, row.names = FALSE, quote = FALSE)

    outfile.rds <- file.path(paramsGlmCoef$outdir, 'STN_DEM_GLM_COEF.rds')
    saveRDS(coef, file = outfile.rds)

    Insert.Messages.Out('Computing downscaling coefficients finished', TRUE, "s")
    rm(stnData, demData)
    gc()
    return(0)
}

#######################################################################################

Coeff.downscaling.LapseRate <- function(stnData, useClimato = TRUE, aspect.slope = FALSE,
                                        polynomial = FALSE, order = 1, standardize = FALSE)
{
    if(aspect.slope) polynomial <- FALSE
    # Constant lapse rate
    months <- as.numeric(substr(stnData$dates, 5, 6))

    lm.res <- lapply(1:12, function(m){
        ix <- which(months == m)
        data.lm <- stnData$data[ix, , drop = FALSE]
        if(useClimato){
            ## climatologies
            if(polynomial){
                lon.lm <- stnData$lon
                lat.lm <- stnData$lat
            }
            if(aspect.slope){
                asp.lm <- stnData$asp
                slp.lm <- stnData$slp
            }
            dem.lm <- stnData$dem
            data.lm <- colMeans(data.lm, na.rm = TRUE)
            data.lm[is.nan(data.lm)] <- NA
        }else{
            ## whole data
            len.dat <- length(ix)
            if(polynomial){
                lon.lm <- rep(stnData$lon, len.dat)
                lat.lm <- rep(stnData$lat, len.dat)
            }
            if(aspect.slope){
                asp.lm <- rep(stnData$asp, len.dat)
                slp.lm <- rep(stnData$slp, len.dat)
            }
            dem.lm <- rep(stnData$dem, len.dat)
            data.lm <- c(t(data.lm))
        }
        data.lm <- data.frame(z = dem.lm, v = data.lm)
        if(aspect.slope)
            data.lm <- data.frame(s = slp.lm, a = asp.lm, data.lm)
        if(polynomial)
            data.lm <- data.frame(x = lon.lm, y = lat.lm, data.lm)

        ina <- !is.na(data.lm$z) & !is.na(data.lm$v)
        ## skip if all data NA
        if(!any(ina)) return(NULL)

        data.lm <- data.lm[ina, , drop = FALSE]
        ## skip if variance null or too small
        sds <- sd(data.lm$v)
        if(!is.na(sds) & sds < 1e-9) return(NULL)

        #####
        moy <- NULL
        sds <- NULL
        if(standardize){
            data.lm0 <- as.matrix(data.lm)
            moy <- colMeans(data.lm0, na.rm = TRUE)
            sds <- matrixStats::colSds(data.lm0, na.rm = TRUE)
            names(sds) <- names(moy)
            data.lm <- sweep(sweep(data.lm, 2, moy, FUN = "-"), 2, sds, FUN = "/")
            data.lm$v[data.lm$v < -3] <- -3
            rm(data.lm0)
        }

        #####
        if(polynomial)
            formule <- if(order == 1) v ~ x + y + z else v ~ x + y + I(x*x) + I(y*y) + I(x*y) + z
        else
            formule <- if(aspect.slope) v ~ s + a + z else v ~ z

        mod.lm <- glm(as.formula(formule), data = data.lm)
        remove.data <- c("residuals", "fitted.values", "effects", "linear.predictors",
                         "weights", "prior.weights", "y", "model", "data")
        mod.lm[remove.data] <- NULL
        mod.lm[["qr"]][["qr"]] <- NULL
        mod.lm$std.pars <- list(mean = moy, sd = sds)
        return(mod.lm)
    })

    inull <- sapply(lm.res, is.null)
    if(all(inull)) return(NULL)

    if(polynomial){
        nomc <- if(order == 1)
                    c("Intercept", "lon", "lat", "elv")
                else
                    c("Intercept", "lon", "lat", "lon2", "lat2", "lonlat", "elv")
        fitting <- "polynomial"
    }else{
        nomc <- if(aspect.slope)
                    c("Intercept", "slope", "aspect", "elv")
                else
                    c("Intercept", "elv")
        fitting <- if(aspect.slope) "aspect.slope" else "elevation"
    }
    coef.lm <- matrix(NA, 12, length(nomc))
    dimnames(coef.lm)[[2]] <- nomc
    coef.lm[!inull, ] <- do.call(rbind, lapply(lm.res, stats::coefficients))

    coef.idx <- 1:12
    coef.idx[inull] <- NA
    if(any(inull)){
        tmp <- coef.idx
        while(any(inull)){
            coef2 <- tmp
            shiftUp <- c(coef2[-1], coef2[1])
            tmp[inull] <- shiftUp[inull]
            tmp[!inull] <- coef.idx[!inull]
            inull <- is.na(tmp)
            if(any(inull)){
                tmp1 <- tmp
                shiftDown <- c(coef2[12], coef2[-12])
                tmp1[inull] <- shiftDown[inull]
                tmp1[!inull] <- tmp[!inull]
                tmp <- tmp1
                inull <- is.na(tmp)
            }
        }
        coef.idx <- tmp
    }

    return(list(model = lm.res[coef.idx], coeff = coef.lm[coef.idx, ],
                fitting = fitting, standardize = standardize))
}
