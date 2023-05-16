
multiplicative.bias.index <- function(dates, bias.method, tstep){
    idx <- NULL
    if(bias.method == "mbmon")
        times.stn <- as.numeric(substr(dates, 5, 6))

    if(bias.method == "mbvar"){
        if(tstep == "daily"){
            endmon <- c(31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31)
            vtimes <- cbind(do.call(c, sapply(endmon, seq)) , rep(1:12, endmon), 1:365)
            xdaty <- paste(as.numeric(substr(dates, 7, 8)),
                           as.numeric(substr(dates, 5, 6)),
                           sep = '_')
            xvtm <- paste(vtimes[, 1], vtimes[, 2], sep = '_')
            times.stn <- vtimes[match(xdaty, xvtm), 3]
            times.stn[is.na(times.stn)] <- 59

            ## Add  +/- 5 days
            ix5days <- lapply(unique(times.stn), function(nt){
                ix1 <- which(times.stn == nt)
                ix1 <- c(sapply(ix1, function(x) x + (-5:5)))
                cbind(nt, ix1[ix1 > 0 & ix1 <= length(dates)])
            })
            ix5days <- do.call(rbind, ix5days)
            times.stn <- ix5days[, 1]
            idx <- ix5days[, 2]
        }

        if(tstep == "pentad"){
            vtimes <- cbind(expand.grid(1:6, 1:12), 1:72)
            xdaty <- paste(as.numeric(substr(dates, 7, 7)),
                           as.numeric(substr(dates, 5, 6)),
                           sep = '_')
            xvtm <- paste(vtimes[, 1], vtimes[, 2], sep = '_')
            times.stn <- vtimes[match(xdaty, xvtm), 3]
        }

        if(tstep == "dekadal"){
            vtimes <- cbind(expand.grid(1:3, 1:12), 1:36)
            xdaty <- paste(as.numeric(substr(dates, 7, 7)),
                           as.numeric(substr(dates, 5, 6)),
                           sep = '_')
            xvtm <- paste(vtimes[, 1], vtimes[, 2], sep = '_')
            times.stn <- vtimes[match(xdaty, xvtm), 3]
        }

        if(tstep == "monthly")
            times.stn <- as.numeric(substr(dates, 5, 6))
    }

    index <- split(seq_along(times.stn), times.stn)

    list(index = index, idx = idx)
}

multiplicative.bias.fun <- function(stn, grd, variable, min.length){
    ina <- is.na(stn) | is.na(grd)
    stn[ina] <- NA
    grd[ina] <- NA
    # if(variable == "rain"){
    #     izero <- !ina & (stn == 0 & grd == 0)
    #     stn[izero] <- NA
    #     grd[izero] <- NA
    # }
    len <- colSums(!ina)

    biasFun <- biascoeff.getOption("mulBiasFunction")
    if(biasFun == "mean"){
        stn <- colSums(stn, na.rm = TRUE)
        grd <- colSums(grd, na.rm = TRUE)
    }
    if(biasFun == "median"){
        stn <- matrixStats::colMedians(stn, na.rm = TRUE)
        grd <- matrixStats::colMedians(grd, na.rm = TRUE)
    }

    bs <- stn/grd
    bs[len < min.length] <- NA
    # bs[is.na(bs)] <- 1

    if(variable == "rain"){
        bs[is.infinite(bs)] <- 1
        bs[is.nan(bs)] <- 1
        bs[bs < 0.01] <- 0.01
        bs[bs > 3] <- 3
    }else if(variable == "temp"){
        bs[is.infinite(bs)] <- 1.5
        bs[is.nan(bs)] <- 1
        # bs[bs < 0] <- 1
        bs[bs < 0.6] <- 0.6
        bs[bs > 1.5] <- 1.5
    }else{
        bs[is.infinite(bs)] <- 1.5
        bs[is.nan(bs)] <- 1
        bs[bs < 0.6] <- 0.6
        bs[bs > 1.5] <- 1.5
    }

    return(bs)
}

outputTest <- function(X, months = 1:12){
    H0.test <- vector(mode = 'list', length = 12)
    H0.test[months] <- lapply(X[months], function(mon){
        sapply(mon, function(stn) if(!is.null(stn)) stn$h0 else 'null')
    })
    return(H0.test)
}

extractDistrParams <- function(X, months = 1:12){
    pars <- vector(mode = 'list', length = 12)
    pars[months] <- lapply(X[months], function(mon){
        parstn <- lapply(mon, function(stn){
            if(!is.null(stn)){
                fitdist <- stn$fitted.distr
                if(!is.null(fitdist)) fitdist$estimate else NA
            }else NA
        })
        nom <- lapply(parstn, function(x) if(length(x) > 1) names(x) else NA)
        nom <- stats::na.omit(do.call(rbind, nom))[1, ]
        parstn <- do.call(rbind, parstn)
        dimnames(parstn)[[2]] <- nom
        parstn
    })
    pars[months] <- rapply(pars[months], f = function(x) ifelse(is.nan(x) | is.infinite(x), NA, x), how = "replace")
    return(pars)
}

quantile.mapping.BGamma <- function(x, pars.stn, pars.rfe, thres){
    res <- x
    p.rfe <- 1 - pars.rfe$prob
    ix <- !is.na(x) & (x > thres)
    pgam <- stats::pgamma(x[ix], scale = pars.rfe$scale[ix], shape = pars.rfe$shape[ix])
    p.rfe[ix] <- p.rfe[ix] + pars.rfe$prob[ix] * pgam
    # p.rfe[p.rfe > 0.999] <- 0.99

    ip <- p.rfe > (1 - pars.stn$prob)
    ip[is.na(ip)] <- FALSE
    pp <- (pars.stn$prob[ip] + p.rfe[ip] - 1) / pars.stn$prob[ip]
    # pp[pp > 0.999] <- 0.99
    res[ip] <- stats::qgamma(pp, scale = pars.stn$scale[ip], shape = pars.stn$shape[ip])

    miss <- is.na(res) | is.nan(res) | is.infinite(res)
    res[miss] <- x[miss]
    res[is.na(x)] <- NA

    # res[x < thres] <- 0

    # ## high values
    # moy <- pars.rfe$shape * pars.rfe$scale
    # ssd <- sqrt(pars.rfe$shape * pars.rfe$scale^2)
    # ix <- (res - moy) / ssd > 4
    # ix[is.na(ix)] <- FALSE
    # res[ix] <- x[ix]

    return(res)
}


quantile.mapping.BGamma1 <- function(x, pars.stn, pars.rfe, thres){
    # pr <- qmap::pberngamma(x, prob = pars.rfe$prob, scale = pars.rfe$scale, shape = pars.rfe$shape)
    # res <- qmap::qberngamma(pr, prob = pars.stn$prob, scale = pars.stn$scale, shape = pars.stn$shape)
    # dim(res) <- dim(x)

    p.rfe <- 1 - pars.rfe$prob
    # ix <- !is.na(x) & x > 0
    ix <- !is.na(x) & (x > thres)
    pgam <- pgamma(x[ix], scale = pars.rfe$scale[ix], shape = pars.rfe$shape[ix])
    p.rfe[ix] <- 1 - pars.rfe$prob[ix] + pars.rfe$prob[ix] * pgam

    res <- rep(0, length(p.rfe))
    ip <- p.rfe > (1 - pars.stn$prob)
    ip[is.na(ip)] <- FALSE
    pp <- (pars.stn$prob[ip] + p.rfe[ip] - 1)/pars.stn$prob[ip]
    res[ip] <- qgamma(pp, scale = pars.stn$scale[ip], shape = pars.stn$shape[ip])
    dim(res) <- dim(x)

    miss <- is.na(res) | is.nan(res) | is.infinite(res)
    res[miss] <- x[miss]
    res[is.na(x)] <- NA

    # res[x < thres] <- 0

    return(res)
}

quantile.mapping.Gauss <- function(x, pars.stn, pars.reanal){
    p.reanal <- x
    ix <- !is.na(x)
    p.reanal[ix] <- pnorm(x[ix], mean = pars.reanal$mean[ix], sd = pars.reanal$sd[ix])
    # p.reanal[ix][p.reanal[ix] < 0.001] <- 0.01
    # p.reanal[ix][p.reanal[ix] > 0.999] <- 0.99
    res <- qnorm(p.reanal, mean = pars.stn$mean, sd = pars.stn$sd)

    miss <- is.na(res) | is.nan(res) | is.infinite(res)
    res[miss] <- x[miss]

    ## high values
    # ix <- abs((res - pars.reanal$mean) / pars.reanal$sd) > 4
    # ix[is.na(ix)] <- FALSE
    # res[ix] <- x[ix]

    return(res)
}

#############################################################################

ComputeBiasCoefficients <- function(stnData, ncInfo, params,
                                    variable, outdir, GUI = TRUE)
{
    xml.dlg <- file.path(.cdtDir$dirLocal, "languages", "cdtBias_functions1.xml")
    lang.msg <- cdtLanguageParse(xml.dlg, .cdtData$Config$lang.iso)
    lang.msg <- lang.msg[['message']]

    Insert.Messages.Out(lang.msg[['1']], TRUE, "i", GUI)

    tstep <- params$period
    bias.method <- params$BIAS$method
    min.length <- params$BIAS$min.length

    biasOpts <- biascoeff.options()

    ############### 


    if(bias.method %in% c("mbvar", "mbmon")){
        Insert.Messages.Out(lang.msg[['2']], TRUE, "i", GUI)

        ptsData <- stnData[c('lon', 'lat')]
        grdData <- readNetCDFData2Points(ncInfo, ptsData, GUI)

        istn <- match(ncInfo$dates, stnData$dates)
        istn <- istn[!is.na(istn)]
        stnData$dates <- stnData$dates[istn]
        stnData$data <- stnData$data[istn, , drop = FALSE]
        igrd <- ncInfo$dates %in% stnData$dates
        ncInfo$dates <- ncInfo$dates[igrd]
        grdData <- grdData[igrd, , drop = FALSE]

        biasData <- list(dates = stnData$dates,
                         # dates = ncInfo$dates,
                         lon = stnData$lon,
                         lat = stnData$lat,
                         stn = stnData$data,
                         grd = grdData)

        Insert.Messages.Out(lang.msg[['3']], TRUE, "s", GUI)
    }

    if(bias.method == "qmdist"){
        stn_index <- split(seq_along(stnData$dates),
                           substr(stnData$dates, 5, 6))

        grd_index <- split(seq_along(ncInfo$dates),
                           substr(ncInfo$dates, 5, 6))

        biasData <- stnData[c('lon', 'lat')]

        distrInfo <- switch(params$BIAS$distr.name,
                            'norm' = list(pars = c('mean', 'sd'), longname = "Normal"),
                            'lnorm' = list(pars = c('meanlog', 'sdlog'), longname = "Log-Normal"),
                            'snorm' = list(pars = c('mean', 'sd', 'xi'), longname = "Skew Normal"),
                            'gamma' = list(pars = c('shape', 'scale'), longname = "Gamma"),
                            'exp' = list(pars = c('rate'), longname = "Exponential"),
                            'weibull' = list(pars = c('shape', 'scale'), longname = "Weibull"),
                            'gumbel' = list(pars = c('loc', 'scale'), longname = "Gumbel"),
                            'berngamma' = list(pars = c('proba', 'shape', 'scale'), longname = "Bernoulli-Gamma"),
                            'bernexp' = list(pars = c('proba', 'rate'), longname = "Bernoulli-Exponential"),
                            'bernlnorm' = list(pars = c('proba', 'meanlog', 'sdlog'), longname = "Bernoulli-Log-Normal"),
                            'bernweibull' = list(pars = c('proba', 'shape', 'scale'), longname = "Bernoulli-Weibull"))

        distrInfo$name <- params$BIAS$distr.name
        distrInfo$probThres <- NA
        # distrP <- list(name = params$BIAS$distr.name, pars = distr_pars)
        # probThres <- NA
        if(params$BIAS$distr.name %in%
           c('berngamma', 'bernexp', 'bernlnorm', 'bernweibull'))
                distrInfo$probThres <- biasOpts$qmdistRainyDayThres

        # ptsData <- stnData[c('lon', 'lat')]
        # bx <- diff(sapply(ptsData, range))
        # dg <- sqrt(bx[1]^2 + bx[2]^2)
        # bx <- 0.0178 * dg + 0.199
        # boxregion <- list(blon = bx, blat = bx)
        # grdData <- readNetCDFData2PtsAggrBox(ncInfo, ptsData, boxregion, GUI)

        # grd <- grdData$data[, grdData$index == 1, drop = FALSE]
        # xgrd <- grdData$data[, grdData$index == 2, drop = FALSE]
        # xlon <- grdData$lon[grdData$index == 2]
        # xlat <- grdData$lat[grdData$index == 2]

        # coarsegrid <- data.frame(lon = xlon, lat = xlat)
        # coordinates(coarsegrid) <- c('lon', 'lat')
        # xypts <- as.data.frame(ptsData)
        # coordinates(xypts) <- c('lon', 'lat')
        # buffer.out <- rgeos::gBuffer(xypts, width = bx)
        # icoarse.out <- as.logical(over(coarsegrid, buffer.out))
        # icoarse.out[is.na(icoarse.out)] <- FALSE
        # coarsegrid <- coarsegrid[!icoarse.out, ]
        # xgrd <- xgrd[, !icoarse.out, drop = FALSE]
        # xlon <- xlon[!icoarse.out]
        # xlat <- xlat[!icoarse.out]

        # istn <- match(ncInfo$dates, stnData$dates)
        # istn <- istn[!is.na(istn)]
        # stnData$dates <- stnData$dates[istn]
        # stnData$data <- stnData$data[istn, , drop = FALSE]
        # igrd <- ncInfo$dates %in% stnData$dates
        # ncInfo$dates <- ncInfo$dates[igrd]
        # grd <- grd[igrd, , drop = FALSE]
        # xgrd <- xgrd[igrd, , drop = FALSE]

        # biasData <- list(dates = stnData$dates,
        #                  # dates = ncInfo$dates,
        #                  slon = stnData$lon,
        #                  slat = stnData$lat,
        #                  stn = stnData$data,
        #                  grd = grd, xgrd = xgrd,
        #                  xlon = xlon, xlat = xlat)
    }

    if(bias.method == "qmecdf"){
        Insert.Messages.Out(lang.msg[['2']], TRUE, "i", GUI)

        boxregion <- params$BIAS[c('blon', 'blat')]
        dg <- sqrt(boxregion$blon^2 + boxregion$blat^2)
        grdData <- readNetCDFData2AggrBox(ncInfo, boxregion, GUI)

        if(biasOpts$qmecdfBoxInterp == "mean"){
            xypts <- as.data.frame(stnData[c('lon', 'lat')])
            sp::coordinates(xypts) <- c('lon', 'lat')
            stn <- lapply(seq_along(grdData$spbox), function(j){
                ix <- over(xypts, grdData$spbox[j])
                ix <- which(!is.na(ix))
                if(length(ix) == 0) return(NA)
                rowMeans(stnData$data[, ix, drop = FALSE], na.rm = TRUE)
            })
            stn <- do.call(cbind, stn)
            stn[is.nan(stn)] <- NA
        }

        ## interpolate stn at grdData$spbox
        if(biasOpts$qmecdfBoxInterp == "idw"){
            dst <- distance.Matrix(sp::coordinates(grdData$spbox),
                                   cbind(stnData$lon, stnData$lat))

            dst[dst > biasOpts$qmecdfBoxMaxdist] <- NA
            Wk <- 1/dst^2
            inf <- is.infinite(Wk)
            Wk[inf] <- NA
            if(any(inf))
                Wk[inf] <- 2 * max(Wk, na.rm = TRUE)

            stn <- matrix(NA, length(stnData$dates), length(grdData$spbox))
            for(j in seq_along(grdData$spbox)){
                WK <- matrix(Wk[, j], nrow(stnData$data), ncol(stnData$data), byrow = TRUE)
                ina <- is.na(stnData$data) | is.na(WK)
                WK[ina] <- NA
                stn[, j] <- rowSums(WK * stnData$data, na.rm = TRUE) / rowSums(WK, na.rm = TRUE)
            }
        }

        ########
        bbx.crd <- expand.grid(grdData[c('lon', 'lat')])

        istn <- match(ncInfo$dates, stnData$dates)
        istn <- istn[!is.na(istn)]
        stnData$dates <- stnData$dates[istn]
        stn <- stn[istn, , drop = FALSE]
        igrd <- ncInfo$dates %in% stnData$dates
        ncInfo$dates <- ncInfo$dates[igrd]
        grdData$data <- grdData$data[igrd, , drop = FALSE]

        biasData <- list(dates = stnData$dates,
                         # dates = ncInfo$dates,
                         lon = bbx.crd$lon,
                         lat = bbx.crd$lat,
                         stn = stn,
                         grd = grdData$data,
                         diag = dg)

        Insert.Messages.Out(lang.msg[['3']], TRUE, "s", GUI)
    }

    ############### 

    if(bias.method %in% c("mbvar", "mbmon")){
        biasIndex <- multiplicative.bias.index(biasData$dates, bias.method, tstep)
        if(tstep == "daily" & bias.method == "mbvar"){
            biasData$stn <- biasData$stn[biasIndex$idx, , drop = FALSE]
            biasData$grd <- biasData$grd[biasIndex$idx, , drop = FALSE]
        }

        bias <- lapply(biasIndex$index, function(ix){
            stn <- biasData$stn[ix, , drop = FALSE]
            grd <- biasData$grd[ix, , drop = FALSE]
            multiplicative.bias.fun(stn, grd, variable, min.length)
        })

        bias <- do.call(rbind, bias)

        bias.pars <- list(method = bias.method, bias = bias,
                          data = biasData, params = params)
    }

    if(bias.method == "qmdist"){
        Insert.Messages.Out(lang.msg[['4']], TRUE, "i", GUI)

        stnPars <- lapply(1:12, function(m){
            dat <- stnData$data[stn_index[[m]], , drop = FALSE]

            # ## check other variables
            # if(variable == "rain")
            #     berngamma_dist_params(dat, min.length, biasOpts$qmdistRainyDayThres)
            # else
            #     normal_dist_params(dat, min.length)

            get_distr_parameters(dat, distrInfo, min.length, distrInfo$probThres)
        })

        grdPars <- lapply(1:12, function(m){
            ncfiles <- ncInfo$ncfiles[grd_index[[m]]]
            ncexist <- ncInfo$exist[grd_index[[m]]]

            if(sum(ncexist) < min.length) return(NULL)

            dat <- lapply(seq_along(ncfiles), function(j){
                if(!ncexist[j]) return(NA)
                nc <- ncdf4::nc_open(ncfiles[j])
                z <- ncdf4::ncvar_get(nc, ncInfo$ncinfo$varid)
                ncdf4::nc_close(nc)
                z <- transposeNCDFData(z, ncInfo$ncinfo)

                if(ncInfo$ncgrid$regrid){
                    don <- c(ncInfo$ncinfo[c('lon', 'lat')], list(z = z))
                    don <- cdt.interp.surface.grid(don, ncInfo$ncgrid[c('lon', 'lat')])
                    z <- don$z
                }

                c(z)
            })

            dat <- do.call(rbind, dat)

            ## check other variables
            # if(variable == "rain"){
            #     pars <- berngamma_dist_params(dat, min.length, biasOpts$qmdistRainyDayThres)
            #     nbvar <- 3
            # }else{
            #     pars <- normal_dist_params(dat, min.length)
            #     nbvar <- 2
            # }

            # for(j in 1:nbvar)
            #     dim(pars[[j]]) <- c(ncInfo$ncgrid$nlon, ncInfo$ncgrid$nlat)

            # return(pars)

            pars <- get_distr_parameters(dat, distrInfo, min.length, distrInfo$probThres)
            pars <- lapply(pars, function(x){
                dim(x) <- c(ncInfo$ncgrid$nlon, ncInfo$ncgrid$nlat)
                x
            })

            return(pars)
        })

        params$distrInfo <- distrInfo
        bias <- list(stn = stnPars, grd = grdPars)
        bias.pars <- list(method = "qmdist", bias = bias,
                          data = biasData, params = params)

        ############### 

        ## perform.test <- params$BIAS$stat.test
        # perform.test <- switch(variable,
        #                       "rain" = params$BIAS$stat.test,
        #                       "temp" = params$BIAS$stat.test)
        # fun.test <- switch(variable,
        #                    "rain" = fit.berngamma.rain,
        #                    "temp" = fit.norm.temp)

        # pkgs <- if(variable == "rain") "qmap" else NULL

        # imonth <- as.numeric(substr(biasData$dates, 5, 6))
        # parsDistr <- vector(mode = 'list', length = 12)

        # parsL <- doparallel.cond(TRUE)
        # parsDistr[1:12] <- cdt.foreach(1:12, parsL, GUI, progress = TRUE,
        #                                .packages = pkgs, FUN = function(m)
        # {
        #     ix <- which(imonth == m)
        #     if(length(ix) == 0) return(NULL)
        #     stn <- biasData$stn[ix, , drop = FALSE]
        #     grd <- biasData$grd[ix, , drop = FALSE]
        #     xgrd <- biasData$xgrd[ix, , drop = FALSE]

        #     llstn <- seq(ncol(stn))
        #     llgrd <- seq(ncol(xgrd))

        #     stn.p <- lapply(llstn, function(j) fun.test(stn[, j], min.length))
        #     grd.p <- lapply(llstn, function(j) fun.test(grd[, j], min.length))
        #     xgrd.p <- lapply(llgrd, function(j) fun.test(xgrd[, j], min.length))
          
        #     list(stn = stn.p, grd = grd.p, xgrd = xgrd.p)
        # })

        # inull <- sapply(parsDistr, is.null)
        # months <- (1:12)[!inull]

        # ####
        # pars.Obs.Stn <- lapply(parsDistr, '[[', 'stn')
        # pars.Obs.Grd <- lapply(parsDistr, '[[', 'grd')
        # pars.Crs.Grd <- lapply(parsDistr, '[[', 'xgrd')

        # ####

        # test.stn <- outputTest(pars.Obs.Stn, months)
        # test.grd <- outputTest(pars.Obs.Grd, months)
        # test.xgrd <- outputTest(pars.Crs.Grd, months)

        # pars.Stn <- extractDistrParams(pars.Obs.Stn, months)
        # pars.Grd <- extractDistrParams(pars.Obs.Grd, months)
        # pars.xGrd <- extractDistrParams(pars.Crs.Grd, months)

        # parsTEST <- vector(mode = 'list', length = 12)
        # if(perform.test){
        #     parsTEST[months] <- lapply(months, function(j){
        #         istn <- test.stn[[j]] == 'yes' & test.grd[[j]] == 'yes'
        #         igrd <- test.xgrd[[j]] == 'yes'
        #         list(grd = igrd, stn = istn)
        #     })
        # }else{
        #     parsTEST[months] <- lapply(months, function(j){
        #         istn0 <- test.stn[[j]] == 'null' | test.grd[[j]] == 'null'
        #         igrd0 <- test.xgrd[[j]] == 'null'
        #         istn <- rep(TRUE, length(test.stn[[j]]))
        #         igrd <- rep(TRUE, length(test.xgrd[[j]]))
        #         istn <- istn & !istn0
        #         igrd <- igrd & !igrd0
        #         list(grd = igrd, stn = istn)
        #     })
        # }

        # parsTEST.Grd <- lapply(parsTEST, '[[', 'grd')
        # parsTEST.Stn <- lapply(parsTEST, '[[', 'stn')

        # bias <- list(test.stn = parsTEST.Stn, test.grd = parsTEST.Grd, stn = pars.Stn,
        #              grd = pars.Grd, xgrd = pars.xGrd)
        # fits <- list(stn = pars.Obs.Stn, grd = pars.Obs.Grd, xgrd = pars.Crs.Grd)

        # bias.pars <- list(method = "qmdist", bias = bias, fit = fits,
        #                   data = biasData, params = params)

        Insert.Messages.Out(lang.msg[['5']], TRUE, "s", GUI)
    }

    if(bias.method == "qmecdf"){
        imonth <- as.numeric(substr(biasData$dates, 5, 6))
        funEcdf <- vector(mode = 'list', length = 12)

        funEcdf[1:12] <- lapply(1:12, function(m){
            ix <- which(imonth == m)
            if(length(ix) == 0) return(NULL)
            stn <- biasData$stn[ix, , drop = FALSE]
            grd <- biasData$grd[ix, , drop = FALSE]
            nc <- ncol(stn)

            tstn <- colSums(!is.na(stn)) >= min.length
            tgrd <- colSums(!is.na(grd)) >= min.length
            inc <- tstn & tgrd
            if(!any(inc)) return(NULL)

            llnc <- seq(nc)
            llnc <- llnc[inc]
            stn <- stn[, inc, drop = FALSE]
            grd <- grd[, inc, drop = FALSE]

            stn.ecdf <- vector(mode = 'list', length = nc)
            grd.ecdf <- vector(mode = 'list', length = nc)

            llstn <- seq(ncol(stn))
            stn.ecdf[llnc] <- lapply(llstn, function(j) ecdf(stn[, j]))
            grd.ecdf[llnc] <- lapply(llstn, function(j) ecdf(grd[, j]))

            list(stn = stn.ecdf, grd = grd.ecdf)
        })

        fun.Stn <- lapply(funEcdf, '[[', 'stn')
        fun.Grd <- lapply(funEcdf, '[[', 'grd')
        bias <- list(stn = fun.Stn, grd = fun.Grd)

        bias.pars <- list(method = "qmecdf", bias = bias,
                          data = biasData, params = params)
    }

    saveRDS(bias.pars, file = file.path(outdir, "BIAS_PARAMS.rds"))

    Insert.Messages.Out(lang.msg[['6']], TRUE, "s", GUI)

    return(bias.pars)
}

#############################################################################

biasInterpolation <- function(interp.method, locbs, interp.grid,
                              maxdist = Inf, nmax = Inf, nmin = 0,
                              bGrd = NULL, vgm.model = NULL, minstn = 10)
{
    if(interp.method %in% c("idw", "okr")){
        vgm <- NULL
        if(interp.method == "okr"){
            if(length(locbs$bs) > minstn){
                exp.var <- gstat::variogram(bs~1, locations = locbs, cressie = TRUE)
                vgm <- try(gstat::fit.variogram(exp.var, gstat::vgm(vgm.model)), silent = TRUE)
                if(!inherits(vgm, "try-error")) vgm <- NULL
                # vgm <- try(automap::autofitVariogram(bs~1, input_data = locbs, model = vgm.model, cressie = TRUE), silent = TRUE)
                # vgm <- if(!inherits(vgm, "try-error")) vgm$var_model else NULL
            }
        }

        grd.bs <- gstat::krige(bs~1, locations = locbs, newdata = interp.grid, model = vgm,
                               block = bGrd, nmin = nmin, nmax = nmax, maxdist = maxdist,
                               debug.level = 0)
        ## extreme values from okr
        grd.bs <- grd.bs$var1.pred
    }

    if(interp.method == "nns"){
        grd.bs <- gstat::krige(bs~1, locations = locbs, newdata = interp.grid,
                               nmax = 1, maxdist = maxdist, debug.level = 0)
        grd.bs <- grd.bs$var1.pred
    }

    if(interp.method == "nn3d"){
        grd.bs <- gstat::krige(bs~1, locations = ~lon+lat+elv, data = locbs,
                               newdata = interp.grid, nmax = 1, debug.level = 0)
        grd.bs <- grd.bs$var1.pred
    }

    return(grd.bs)
}

InterpolateBiasCoefficients <- function(bias.pars, xy.grid, variable,
                                        outdir, demData, GUI = TRUE)
{
    xml.dlg <- file.path(.cdtDir$dirLocal, "languages", "cdtBias_functions2.xml")
    lang.msg <- cdtLanguageParse(xml.dlg, .cdtData$Config$lang.iso)
    lang.msg <- lang.msg[['message']]

    Insert.Messages.Out(lang.msg[['1']], TRUE, "i", GUI)

    bias.method <- bias.pars$params$BIAS$method
    pars.interp <- bias.pars$params$interp

    interp.method <- pars.interp$method

    dim.grid <- sapply(xy.grid, length)

    dx <- ncdf4::ncdim_def("Lon", "degreeE", xy.grid$lon)
    dy <- ncdf4::ncdim_def("Lat", "degreeN", xy.grid$lat)

    biasOpts <- biascoeff.options()

    #############

    if(bias.method %in% c("mbvar", "mbmon")){
        ncoutBS <- ncdf4::ncvar_def("bias", "", list(dx, dy), NA,
                                    longname = "Multiplicative Mean Bias Factor",
                                    prec = "float", compression = 9)
        bias <- bias.pars$bias
        maxdist <- pars.interp$maxdist

        #############

        if(interp.method %in% c("idw", "okr", "nns")){
            interp.grid <- defSpatialPixels(xy.grid)
            locations.bs <- as.data.frame(bias.pars$data[c('lon', 'lat')])
            sp::coordinates(locations.bs) <- ~lon+lat
        }

        if(interp.method == "nn3d"){
            interp.grid <- expand.grid(lon = xy.grid$lon, lat = xy.grid$lat)
            interp.grid <- data.frame(interp.grid, elv = c(demData$z))
            stn.crds <- bias.pars$data[c('lon', 'lat')]
            locations.bs <- as.data.frame(stn.crds)

            dem1 <- demData[c('x', 'y')]
            names(dem1) <- c('lon', 'lat')
            ijx <- grid2pointINDEX(stn.crds, dem1)
            locations.bs$elv <- demData$z[ijx]

            elvNA <- is.na(locations.bs$elv)
            locations.bs <- locations.bs[!elvNA, ]

            ixs <- cdt.nn.index.all(interp.grid[, 1:2, drop = FALSE],
                                    locations.bs[, 1:2, drop = FALSE],
                                    maxdist)
            #############
            interp.grid <- scale(interp.grid)
            interp.grid <- as.data.frame(interp.grid)
            locations.bs <- scale(locations.bs)
            locations.bs <- as.data.frame(locations.bs)
        }

        #############
        if(interp.method %in% c("idw", "okr")){
            nmin <- pars.interp$nmin
            nmax <- pars.interp$nmax
            minstn <- pars.interp$minstn
            vgm.model <- pars.interp$vgm.model

            bGrd <- NULL
            if(pars.interp$use.block){
                bGrd <- switch(biasOpts$blockType,
                                "userdefined" = createBlock(biasOpts$blockSize),
                                "gaussian" = biasOpts$blockSize
                               )
            }
        }

        #############

        parsL <- doparallel.cond(TRUE)
        ret <- cdt.foreach(seq(nrow(bias)), parsL, GUI, progress = TRUE, FUN = function(jj)
        {
            ncoutf <- file.path(outdir, paste0("STN_GRD_Bias_Coeff_", jj, ".nc"))

            #####
            bsjj <- as.numeric(bias[jj, ])
            ina <- !is.na(bsjj)

            if(length(which(ina)) < 3){
                grd.bs <- array(1, dim.grid)
                nc <- ncdf4::nc_create(ncoutf, ncoutBS)
                ncdf4::ncvar_put(nc, ncoutBS, grd.bs)
                ncdf4::nc_close(nc)
                return(NULL)
            }

            #####
            locbs <- locations.bs

            if(interp.method %in% c("idw", "okr", "nns")){
                locbs$bs <- bsjj
                locbs <- locbs[ina, ]
            }else{
                locbs$bs <- bsjj[!elvNA]
                locbs <- locbs[ina[!elvNA], ]
            }

            #####

            if(interp.method %in% c("idw", "okr"))
                grd.bs <- biasInterpolation(interp.method, locbs, interp.grid, maxdist, nmax, nmin, bGrd, vgm.model, minstn)
            if(interp.method == "nns")
                grd.bs <- biasInterpolation(interp.method, locbs, interp.grid, maxdist)
            if(interp.method == "nn3d"){
                grd.bs <- biasInterpolation(interp.method, locbs, interp.grid)
                grd.bs[!ixs] <- NA
            }
            dim(grd.bs) <- dim.grid

            #####
            if(variable == "rain"){
                grd.bs[grd.bs < 0.01] <- 0.01
                grd.bs[grd.bs > 3] <- 3
            }else if(variable == "temp"){
                grd.bs[grd.bs < 0.6] <- 0.6
                grd.bs[grd.bs > 1.5] <- 1.5
            }else{
                grd.bs[grd.bs < 0.6] <- 0.6
                grd.bs[grd.bs > 1.5] <- 1.5
            }

            grd.bs[is.na(grd.bs)] <- 1
            nc <- ncdf4::nc_create(ncoutf, ncoutBS)
            ncdf4::ncvar_put(nc, ncoutBS, grd.bs)
            ncdf4::nc_close(nc)
            return(0)
        })
    }

    if(bias.method == "qmdist"){
        ncoutPARS <- lapply(seq_along(bias.pars$params$distrInfo$pars), function(j){
            ncdf4::ncvar_def(bias.pars$params$distrInfo$pars[j], "", list(dx, dy), NA,
                             longname = paste(bias.pars$params$distrInfo$longname, "distribution :",
                                              bias.pars$params$distrInfo$pars[j], "parameter"),
                             prec = "float", compression = 9)
            })


        if(variable == "rain"){
            PROB <- ncdf4::ncvar_def("prob", "", list(dx, dy), NA,
                                     longname = "Probability of non-zero event Bernoulli-Gamma distribution",
                                     prec = "float", compression = 9)
            SHAPE <- ncdf4::ncvar_def("shape", "", list(dx, dy), NA,
                                      longname = "Shape parameters of the gamma distribution",
                                      prec = "float", compression = 9)
            SCALE <- ncdf4::ncvar_def("scale", "", list(dx, dy), NA,
                                      longname = "Scale parameters of the gamma distribution",
                                      prec = "float", compression = 9)
            ncoutPARS <- list(PROB, SHAPE, SCALE)
            nbvar <- 3
            distrname <- "BernGamma"
        }else if(variable == "temp"){
            MEAN <- ncdf4::ncvar_def("mean", "degC", list(dx, dy), NA,
                                     longname = "Means normal distribution",
                                     prec = "float", compression = 9)
            SD <- ncdf4::ncvar_def("sd", "degC", list(dx, dy), NA,
                                   longname = "Standard deviations normal distribution",
                                   prec = "float", compression = 9)
            ncoutPARS <- list(MEAN, SD)
            nbvar <- 2
            distrname <- "Gaussian"
        }else{
            MEAN <- ncdf4::ncvar_def("mean", "degC", list(dx, dy), NA,
                                     longname = "Means normal distribution",
                                     prec = "float", compression = 9)
            SD <- ncdf4::ncvar_def("sd", "degC", list(dx, dy), NA,
                                   longname = "Standard deviations normal distribution",
                                   prec = "float", compression = 9)
            ncoutPARS <- list(MEAN, SD)
            nbvar <- 2
            distrname <- "Gaussian"
        }

        #############

        bias <- bias.pars$bias
        maxdist <- pars.interp$maxdist

        #############

        if(interp.method %in% c("idw", "okr", "nns")){
            interp.grid <- defSpatialPixels(xy.grid)

            locations.stn <- as.data.frame(bias.pars$data[c('lon', 'lat')])
            names(locations.stn) <- c('lon', 'lat')
            sp::coordinates(locations.stn) <- ~lon+lat

            # grd1 <- as.data.frame(bias.pars$data[c('slon', 'slat')])
            # names(grd1) <- c('lon', 'lat')
            # grd2 <- as.data.frame(bias.pars$data[c('xlon', 'xlat')])
            # names(grd2) <- c('lon', 'lat')

            # locations.grd <- rbind(grd1, grd2)
            # coordinates(locations.grd) <- ~lon+lat
        }

        if(interp.method == "nn3d"){
            interp.grid <- expand.grid(lon = xy.grid$lon, lat = xy.grid$lat)
            interp.grid <- data.frame(interp.grid, elv = c(demData$z))

            stn.crds <- bias.pars$data[c('lon', 'lat')]
            names(stn.crds) <- c('lon', 'lat')

            dem1 <- demData[c('x', 'y')]
            names(dem1) <- c('lon', 'lat')
            ijstn <- grid2pointINDEX(stn.crds, dem1)
            stn.elv <- demData$z[ijstn]

            locations.stn <- as.data.frame(stn.crds)
            locations.stn$elv <- stn.elv
            elvNAs <- is.na(locations.stn$elv)
            locations.stn <- locations.stn[!elvNAs, ]

            ixs <- cdt.nn.index.all(interp.grid[, 1:2, drop = FALSE],
                                    locations.stn[, 1:2, drop = FALSE],
                                    maxdist)

            # stn.crds <- bias.pars$data[c('slon', 'slat')]
            # names(stn.crds) <- c('lon', 'lat')
            # grd.crds <- bias.pars$data[c('xlon', 'xlat')]
            # names(grd.crds) <- c('lon', 'lat')

            # dem1 <- demData[c('x', 'y')]
            # names(dem1) <- c('lon', 'lat')
            # ijstn <- grid2pointINDEX(stn.crds, dem1)
            # stn.elv <- demData$z[ijstn]
            # ijgrd <- grid2pointINDEX(grd.crds, dem1)
            # grd.elv <- demData$z[ijgrd]

            # locations.stn <- as.data.frame(stn.crds)
            # locations.stn$elv <- stn.elv
            # elvNAs <- is.na(locations.stn$elv)
            # locations.stn <- locations.stn[!elvNAs, ]

            # ixs <- cdt.nn.index.all(interp.grid[, 1:2, drop = FALSE],
            #                         locations.stn[, 1:2, drop = FALSE],
            #                         maxdist)

            # grd1 <- as.data.frame(stn.crds)
            # grd2 <- as.data.frame(grd.crds)
            # locations.grd <- rbind(grd1, grd2)
            # locations.grd$elv <- c(stn.elv, grd.elv)
            # elvNAg <- is.na(locations.grd$elv)
            # locations.grd <- locations.grd[!elvNAg, ]

            # ixg <- cdt.nn.index.all(interp.grid[, 1:2, drop = FALSE],
            #                         locations.stn[, 1:2, drop = FALSE],
            #                         maxdist)
            #############
            interp.grid <- scale(interp.grid)
            interp.grid <- as.data.frame(interp.grid)

            locations.stn <- scale(locations.stn)
            locations.stn <- as.data.frame(locations.stn)

            # locations.grd <- scale(locations.grd)
            # locations.grd <- as.data.frame(locations.grd)
        }

        #############
        if(interp.method %in% c("idw", "okr")){
            nmin <- pars.interp$nmin
            nmax <- pars.interp$nmax
            minstn <- pars.interp$minstn
            vgm.model <- pars.interp$vgm.model

            bGrd <- NULL
            if(pars.interp$use.block){
                bGrd <- switch(biasOpts$blockType,
                                "userdefined" = createBlock(biasOpts$blockSize),
                                "gaussian" = biasOpts$blockSize
                               )
            }
        }

        #############

        parsL <- doparallel.cond(TRUE)
        ret <- cdt.foreach(1:12, parsL, GUI, progress = TRUE, FUN = function(m)
        {
            ncoutf <- file.path(outdir, paste0("STN_Bias_Pars_", distrname, "_", m, ".nc"))

            pars.mon <- lapply(1:nbvar, function(j){
                bsjj <- bias$stn[[m]][[j]]
                ina <- !is.na(bsjj)

                if(sum(ina) < 5) return(NULL)

                ####
                locbs <- locations.stn

                if(interp.method %in% c("idw", "okr", "nns")){
                    locbs$bs <- bsjj
                    locbs <- locbs[ina, ]
                }else{
                    locbs$bs <- bsjj[!elvNAs]
                    locbs <- locbs[ina[!elvNAs], ]
                }

                if(interp.method %in% c("idw", "okr"))
                    grd.bs <- biasInterpolation(interp.method, locbs, interp.grid, maxdist, nmax, nmin, bGrd, vgm.model, minstn)
                if(interp.method == "nns")
                    grd.bs <- biasInterpolation(interp.method, locbs, interp.grid, maxdist)
                if(interp.method == "nn3d"){
                    grd.bs <- biasInterpolation(interp.method, locbs, interp.grid)
                    grd.bs[!ixs] <- NA
                }

                dim(grd.bs) <- dim.grid

                if(variable == "rain"){
                    if(j == 1){
                        grd.bs[grd.bs < 0] <- 0.01
                        grd.bs[grd.bs > 1] <- 0.99
                    }else grd.bs[grd.bs < 0] <- NA
                }else if(variable == "temp"){
                    ## standard deviation params
                    if(j == 2) grd.bs[grd.bs < 1e-3] <- 1e-3
                }else{
                    if(j == 2) grd.bs[grd.bs < 1e-3] <- 1e-3
                }

                grd.bs
            })

            nc <- ncdf4::nc_create(ncoutf, ncoutPARS)
            for(j in 1:nbvar){
                mat <- pars.mon[[j]]
                if(is.null(mat)) mat <- array(NA, dim.grid)
                ncdf4::ncvar_put(nc, ncoutPARS[[j]], mat)
            }
            ncdf4::nc_close(nc)

            return(0)
        })

        ############

        ret <- lapply(1:12, function(m){
            ncoutf <- file.path(outdir, paste0("GRD_Bias_Pars_", distrname, "_", m, ".nc"))

            nc <- ncdf4::nc_create(ncoutf, ncoutPARS)
            for(j in 1:nbvar){
                mat <- bias$grd[[m]][[j]]
                if(is.null(mat)) mat <- array(NA, dim.grid)
                ncdf4::ncvar_put(nc, ncoutPARS[[j]], mat)
            }
            ncdf4::nc_close(nc)

            return(0)
        })

        #############

        # parsL <- doparallel.cond(TRUE)
        # ret <- cdt.foreach(1:12, parsL, GUI, progress = TRUE, FUN = function(m)
        # {
        #     ncoutf <- file.path(outdir, paste0("STN_Bias_Pars_", distrname, "_", m, ".nc"))

        #     pars.mon <- lapply(1:nbvar, function(j){
        #         bsjj <- bias$stn[[m]][, j]
        #         if(is.null(bsjj)) return(NULL)

        #         if((variable == "rain" & j > 1) | variable == "temp"){
        #             bsjj[!bias$test.stn[[m]]] <- NA
        #         } 
        #         ina <- !is.na(bsjj)

        #         if(length(which(ina)) < 3) return(NULL)

        #         ####
        #         locbs <- locations.stn

        #         if(interp.method %in% c("idw", "okr", "nns")){
        #             locbs$bs <- bsjj
        #             locbs <- locbs[ina, ]
        #         }else{
        #             locbs$bs <- bsjj[!elvNAs]
        #             locbs <- locbs[ina[!elvNAs], ]
        #         }

        #         if(interp.method %in% c("idw", "okr"))
        #             grd.bs <- biasInterpolation(interp.method, locbs, interp.grid, maxdist, nmax, nmin, bGrd, vgm.model, minstn)
        #         if(interp.method == "nns")
        #             grd.bs <- biasInterpolation(interp.method, locbs, interp.grid, maxdist)
        #         if(interp.method == "nn3d"){
        #             grd.bs <- biasInterpolation(interp.method, locbs, interp.grid)
        #             grd.bs[!ixs] <- NA
        #         }

        #         dim(grd.bs) <- dim.grid

        #         if(variable == "rain"){
        #             if(j == 1){
        #                 grd.bs[grd.bs < 0] <- 0.01
        #                 grd.bs[grd.bs > 1] <- 0.99
        #             }else grd.bs[grd.bs < 0] <- NA
        #         }

        #         if(variable == "temp"){
        #             if(j == 2) grd.bs[grd.bs < 0] <- NA
        #         }

        #         grd.bs
        #     })

        #     nc <- ncdf4::nc_create(ncoutf, ncoutPARS)
        #     for(j in 1:nbvar){
        #         mat <- pars.mon[[j]]
        #         if(is.null(mat)) mat <- array(NA, dim.grid)
        #         ncdf4::ncvar_put(nc, ncoutPARS[[j]], mat)
        #     }
        #     ncdf4::nc_close(nc)

        #     return(0)
        # })

        ############

        # parsL <- doparallel.cond(TRUE)
        # ret <- cdt.foreach(1:12, parsL, GUI, progress = TRUE, FUN = function(m)
        # {
        #     ncoutf <- file.path(outdir, paste0("GRD_Bias_Pars_", distrname, "_", m, ".nc"))

        #     pars.mon <- lapply(1:nbvar, function(j){
        #         xgrd <- bias$xgrd[[m]][, j]
        #         grd <- bias$grd[[m]][, j]
        #         if(is.null(grd)) return(NULL)

        #         bsjj <- c(grd, xgrd)
        #         if((variable == "rain" & j > 1) | variable == "temp"){
        #             bsjj[c(!bias$test.stn[[m]], !bias$test.grd[[m]])] <- NA
        #         }
        #         ina <- !is.na(bsjj)

        #         if(length(which(ina)) < 3) return(NULL)

        #         ####
        #         locbs <- locations.grd

        #         if(interp.method %in% c("idw", "okr", "nns")){
        #             locbs$bs <- bsjj
        #             locbs <- locbs[ina, ]
        #         }else{
        #             locbs$bs <- bsjj[!elvNAg]
        #             locbs <- locbs[ina[!elvNAg], ]
        #         }

        #         if(interp.method %in% c("idw", "okr"))
        #             grd.bs <- biasInterpolation(interp.method, locbs, interp.grid, maxdist, nmax, nmin, bGrd, vgm.model, minstn)
        #         if(interp.method == "nns")
        #             grd.bs <- biasInterpolation(interp.method, locbs, interp.grid, maxdist)
        #         if(interp.method == "nn3d"){
        #             grd.bs <- biasInterpolation(interp.method, locbs, interp.grid)
        #             grd.bs[!ixs] <- NA
        #         }

        #         dim(grd.bs) <- dim.grid

        #         if(variable == "rain"){
        #             if(j == 1){
        #                 grd.bs[grd.bs < 0] <- 0.01
        #                 grd.bs[grd.bs > 1] <- 0.99
        #             }else grd.bs[grd.bs < 0] <- NA
        #         }

        #         if(variable == "temp"){
        #             if(j == 2) grd.bs[grd.bs < 0] <- NA
        #         }

        #         grd.bs
        #     })

        #     nc <- ncdf4::nc_create(ncoutf, ncoutPARS)
        #     for(j in 1:nbvar){
        #         mat <- pars.mon[[j]]
        #         if(is.null(mat)) mat <- array(NA, dim.grid)
        #         ncdf4::ncvar_put(nc, ncoutPARS[[j]], mat)
        #     }
        #     ncdf4::nc_close(nc)

        #     return(0)
        # })
    }

    if(bias.method == "qmecdf"){
        voisin <- 4
        grd <- as.matrix(expand.grid(xy.grid))
        stn <- do.call(cbind, bias.pars$data[c('lon', 'lat')])
        dst <- fields::rdist(stn, grd)
        ord <- apply(dst, 2, order)
        dst <- apply(dst, 2, sort)
        ord <- ord[1:voisin, , drop = FALSE]
        dst <- dst[1:voisin, , drop = FALSE] / (2 * bias.pars$data$diag)
        Wk <- exp(-(dst / 0.33)^2)

        grd <- list(lon = xy.grid$lon, lat = xy.grid$lat, id = ord, dist = dst, Wk = Wk)

        saveRDS(grd, file = file.path(outdir, "GRID_DATA.rds"))
    }

    #####
    Insert.Messages.Out(lang.msg[['2']], TRUE, "s", GUI)
    return(0)
}

#############################################################################

readBiasFiles <- function(params, variable, GUI = TRUE){
    xml.dlg <- file.path(.cdtDir$dirLocal, "languages", "cdtBias_functions3.xml")
    lang.msg <- cdtLanguageParse(xml.dlg, .cdtData$Config$lang.iso)
    lang.msg <- lang.msg[['message']]

    Insert.Messages.Out(lang.msg[['1']], TRUE, "i", GUI)

    if(params$BIAS$method %in% c("mbvar", "mbmon")){
        if(params$BIAS$method == "mbvar"){
            nbias <- switch(params$period,
                            "daily" = 365,
                            "pentad" = 72,
                            "dekadal" = 36,
                            "monthly" = 12)
        }
        if(params$BIAS$method == "mbmon") nbias <- 12


        biasFiles <- file.path(params$BIAS$dir, sprintf("STN_GRD_Bias_Coeff_%s.nc", seq(nbias)))
        bs.exist <- sapply(biasFiles, file.exists)

        if(any(!bs.exist)){
            miss.bias <- biasFiles[!bs.exist]
            if(length(miss.bias) == nbias){
                Insert.Messages.Out(lang.msg[['2']], TRUE, "e", GUI)
                return(NULL)
            }else{
                for(j in seq_along(miss.bias))
                    Insert.Messages.Out(paste(miss.bias[j], lang.msg[['7']]), TRUE, "w", GUI)
                Insert.Messages.Out(lang.msg[['3']], TRUE, "w", GUI)
            }
        }

        nc <- ncdf4::nc_open(biasFiles[which(bs.exist)[1]])
        lon <- nc$dim[[1]]$vals
        lat <- nc$dim[[2]]$vals
        ncdf4::nc_close(nc)

        BIAS <- vector(mode = 'list', length = nbias)
        BIAS[which(bs.exist)] <- lapply(which(bs.exist), function(m){
            nc <- ncdf4::nc_open(biasFiles[m])
            xvar <- ncdf4::ncvar_get(nc, varid = nc$var[[1]]$name)
            ncdf4::nc_close(nc)
            xvar
        })
        
        bias <- list(lon = lon, lat = lat, bias = BIAS)
    }

    if(params$BIAS$method == "qmdist"){
        ## check other variables (rh, pres, rad, wind)
        distrname <- if(variable == "rain") "BernGamma" else "Gaussian"
        stnfiles <- file.path(params$BIAS$dir, sprintf("STN_Bias_Pars_%s_%s.nc", distrname, 1:12))
        grdfiles <- file.path(params$BIAS$dir, sprintf("GRD_Bias_Pars_%s_%s.nc", distrname, 1:12))

        stn.exist <- sapply(stnfiles, file.exists)
        grd.exist <- sapply(grdfiles, file.exists)

        if(any(!stn.exist) | any(!grd.exist)){
            miss.stn <- stnfiles[!stn.exist]
            miss.grd <- grdfiles[!grd.exist]
            if(length(miss.stn) == 12){
                Insert.Messages.Out(lang.msg[['4']], TRUE, "e", GUI)
                return(NULL)
            }else if(length(miss.grd) == 12){
                Insert.Messages.Out(lang.msg[['5']], TRUE, "e", GUI)
                return(NULL)
            }else if(all(!stn.exist | !grd.exist)){
                Insert.Messages.Out(lang.msg[['6']], TRUE, "e", GUI)
                return(NULL)
            }else{
                for(j in seq_along(miss.stn))
                    Insert.Messages.Out(paste(miss.stn[j], lang.msg[['7']]), TRUE, "w", GUI)
                for(j in seq_along(miss.grd))
                    Insert.Messages.Out(paste(miss.grd[j], lang.msg[['7']]), TRUE, "w", GUI)
                Insert.Messages.Out(lang.msg[['8']], TRUE, "w", GUI)
            }
        }

        bs.exist <- stn.exist & grd.exist

        nc <- ncdf4::nc_open(stnfiles[which(bs.exist)[1]])
        lon <- nc$dim[[1]]$vals
        lat <- nc$dim[[2]]$vals
        ncdf4::nc_close(nc)

        PARS.stn <- vector(mode = 'list', length = 12)
        PARS.grd <- vector(mode = 'list', length = 12)
        ## check other variables (rh, pres, rad, wind)
        varids <- if(variable == "rain") c("prob", "scale", "shape") else c("mean", "sd")

        PARS.stn[which(bs.exist)] <- lapply(which(bs.exist), function(m){
            nc <- ncdf4::nc_open(stnfiles[m])
            pars <- lapply(varids, function(varid) ncdf4::ncvar_get(nc, varid))
            ncdf4::nc_close(nc)
            names(pars) <- varids
            pars
        })

        PARS.grd[which(bs.exist)] <- lapply(which(bs.exist), function(m){
            nc <- ncdf4::nc_open(grdfiles[m])
            pars <- lapply(varids, function(varid) ncdf4::ncvar_get(nc, varid))
            ncdf4::nc_close(nc)
            names(pars) <- varids
            pars
        })

        bias <- list(lon = lon, lat = lat, stn = PARS.stn, grd = PARS.grd)
    }

    if(params$BIAS$method == "qmecdf"){
        biasfiles <- file.path(params$BIAS$dir, "BIAS_PARAMS.rds")
        if(!file.exists(biasfiles)){
            Insert.Messages.Out(lang.msg[['9']], TRUE, "e", GUI)
            return(NULL)
        }
        grdfiles <- file.path(params$BIAS$dir, "GRID_DATA.rds")
        if(!file.exists(grdfiles)){
            Insert.Messages.Out(lang.msg[['10']], TRUE, "e", GUI)
            return(NULL)
        }

        biasD <- readRDS(biasfiles)
        grd <- readRDS(grdfiles)
        bias <- c(grd, biasD$bias)
    }

    Insert.Messages.Out(lang.msg[['11']], TRUE, "s", GUI)
    return(bias)
}

applyBiasCorrection <- function(bias, ncInfo, outdir, params, variable, GUI = TRUE){
    xml.dlg <- file.path(.cdtDir$dirLocal, "languages", "cdtBias_functions4.xml")
    lang.msg <- cdtLanguageParse(xml.dlg, .cdtData$Config$lang.iso)
    lang.msg <- lang.msg[['message']]

    Insert.Messages.Out(lang.msg[['1']], TRUE, "i", GUI)

    varinfo <- ncInfo$ncinfo$varinfo
    dx <- ncdf4::ncdim_def("Lon", "degree_east", bias$lon)
    dy <- ncdf4::ncdim_def("Lat", "degree_north", bias$lat)
    xy.dim <- list(dx, dy)
    grd.bsadj <- ncdf4::ncvar_def(varinfo$name, varinfo$units, xy.dim, varinfo$missval,
                                  longname = paste("Bias Corrected", varinfo$longname),
                                  prec = varinfo$prec, compression = 9)

    ######  regrid

    biasGrd <- bias[c("lon", "lat")]
    ncdfGrd <- ncInfo$ncinfo[c("lon", "lat")]
    is.regridNCDF <- is.diffSpatialPixelsObj(defSpatialPixels(biasGrd),
                                             defSpatialPixels(ncdfGrd),
                                             tol = 1e-03)
    #####

    yrs <- substr(ncInfo$dates, 1, 4)
    mon <- substr(ncInfo$dates, 5, 6)
    if(params$period == 'daily'){
        day <- substr(ncInfo$dates, 7, 8)
        ncoutf <- sprintf(params$output$format, yrs, mon, day)
    }
    if(params$period %in% c('pentad', 'dekadal')){
        dk <- substr(ncInfo$dates, 7, 7)
        ncoutf <- sprintf(params$output$format, yrs, mon, dk)
    }
    if(params$period == 'monthly')
        ncoutf <- sprintf(params$output$format, yrs, mon)

    ncoutfiles <- file.path(outdir, ncoutf)

    biasOpts <- biascoeff.options()

    ################ 

    if(params$BIAS$method == "mbvar"){
        if(params$period == 'daily'){
            ann <- as.numeric(substr(ncInfo$dates, 1, 4))
            iday <- as.numeric(strftime(as.Date(ncInfo$dates, format = '%Y%m%d'), format = '%j'))
            ijt <- ifelse(is.leapyears(ann) & iday > 59, iday - 1, iday)
        }
        if(params$period == 'pentad'){
            mon <- as.numeric(substr(ncInfo$dates, 5, 6))
            pen <- as.numeric(substr(ncInfo$dates, 7, 7))
            mon.pen <- paste0(mon, "_", pen)
            annual.pen <- expand.grid(pen = 1:6, mon = 1:12)
            annual.pen <- paste0(annual.pen$mon, "_", annual.pen$pen)
            ijt <- match(mon.pen, annual.pen)
        }
        if(params$period == 'dekadal'){
            mon <- as.numeric(substr(ncInfo$dates, 5, 6))
            dek <- as.numeric(substr(ncInfo$dates, 7, 7))
            mon.dek <- paste0(mon, "_", dek)
            annual.dek <- expand.grid(dek = 1:3, mon = 1:12)
            annual.dek <- paste0(annual.dek$mon, "_", annual.dek$dek)
            ijt <- match(mon.dek, annual.dek)
        }
        if(params$period == 'monthly'){
            ijt <- as.numeric(substr(ncInfo$dates, 5, 6))
        }
    }else{
        ijt <- as.numeric(substr(ncInfo$dates, 5, 6))
    }

    ################ 

    if(params$BIAS$method == "qmecdf")
        mat.dim <- sapply(bias[c('lon', 'lat')], length)

    ncinfo <- ncInfo$ncinfo[c('varid', 'ilon', 'ilat', 'xo', 'yo')]

    ################ 

    # parsL <- doparallel.cond(length(ncInfo$ncfiles) > 10)
    parsL <- doparallel.cond(FALSE)

    ret <- cdt.foreach(seq_along(ncInfo$ncfiles), parsL, GUI,
                       progress = TRUE, FUN = function(jj)
    {
        if(!ncInfo$exist[jj]) return(NULL)

        nc <- ncdf4::nc_open(ncInfo$ncfiles[jj])
        xval <- ncdf4::ncvar_get(nc, varid = ncinfo$varid)
        ncdf4::nc_close(nc)
        xval <- transposeNCDFData(xval, ncinfo)

        if(is.regridNCDF){
            ncObj <- c(ncdfGrd, list(z = xval))
            ncObj <- cdt.interp.surface.grid(ncObj, biasGrd)
            xval <- ncObj$z
        }

        if(params$BIAS$method %in% c("mbvar", "mbmon")){
            xadj <- xval * bias$bias[[ijt[jj]]]
        }

        if(params$BIAS$method == "qmdist"){
            pars.stn <- bias$stn[[ijt[jj]]]
            pars.grd <- bias$grd[[ijt[jj]]]

            if(variable == "rain"){
                # xadj <- quantile.mapping.BGamma(xval, pars.stn, pars.grd,
                #                                 biasOpts$qmdistRainyDayThres)
                foo <- if(biasOpts$qmdistTest == 1) quantile.mapping.BGamma else quantile.mapping.BGamma1
                xadj <- foo(xval, pars.stn, pars.grd, biasOpts$qmdistRainyDayThres)
            }else if(variable == "temp"){
                xadj <- quantile.mapping.Gauss(xval, pars.stn, pars.grd)
            }else{
                xadj <- quantile.mapping.Gauss(xval, pars.stn, pars.grd)
            }
        }

        if(params$BIAS$method == "qmecdf"){
            fun.stn <- bias$stn[[ijt[jj]]]
            fun.grd <- bias$grd[[ijt[jj]]]
            xadj <- xval

            for(j in seq(ncol(bias$id))){
                fobs <- fun.stn[bias$id[, j]]
                fgrd <- fun.grd[bias$id[, j]]
                inull <- sapply(fobs, is.null) | sapply(fgrd, is.null)
                if(all(inull)) next
                fobs <- fobs[!inull]
                fgrd <- fgrd[!inull]
                wg <- bias$Wk[!inull, j]
                ix <- c(arrayInd(j, mat.dim))
                xv <- sapply(seq_along(fobs), function(i){
                    foo <- function(t) quantile(fobs[[i]], t)
                    foo(fgrd[[i]](xval[ix[1], ix[2]]))
                })
                xadj[ix[1], ix[2]] <- sum(wg * xv) / sum(wg)
            }
        }

        if(variable == "rain") xadj[xadj < 0] <- 0
        if(variable == "rad") xadj[xadj < 0] <- 0
        if(variable == "rh"){
            xadj[xadj < 0] <- 0
            xadj[xadj > 100] <- 100
        }

        xadj[is.na(xadj)] <- varinfo$missval

        nc <- ncdf4::nc_create(ncoutfiles[jj], grd.bsadj)
        ncdf4::ncvar_put(nc, grd.bsadj, xadj)
        ncdf4::nc_close(nc)

        return(0)
    })

    Insert.Messages.Out(lang.msg[['2']], TRUE, "s", GUI)

    return(0)
}
