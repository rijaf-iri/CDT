
## to replace and remove

# R/cdtBias_Correction_Precip_Cmd.R
# cdtBiasCorrectPrecipCMD

# R/cdtBias_Correction_ClimData_Cmd.R
# cdtBiasCorrectTempCMD

# R/cdtBias_Correction_Wind_Procs.R
# biasCorrectionWind

####################
# R/cdtBias_Correction_ClimData_adjust.R
# applyBiasCorrection
# replaced by applyBiasCorrectionClimData

# R/cdtBias_Correction_ClimData_read.R
# readBiasFiles
# replaced by readBiasCoefficientFiles

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
                    foo <- function(t) stats::quantile(fobs[[i]], t)
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


#' @exportS3Method NULL
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

## to delete
#' @exportS3Method NULL
quantile.mapping.BGamma1 <- function(x, pars.stn, pars.rfe, thres){
    # pr <- qmap::pberngamma(x, prob = pars.rfe$prob, scale = pars.rfe$scale, shape = pars.rfe$shape)
    # res <- qmap::qberngamma(pr, prob = pars.stn$prob, scale = pars.stn$scale, shape = pars.stn$shape)
    # dim(res) <- dim(x)

    p.rfe <- 1 - pars.rfe$prob
    # ix <- !is.na(x) & x > 0
    ix <- !is.na(x) & (x > thres)
    pgam <- stats::pgamma(x[ix], scale = pars.rfe$scale[ix], shape = pars.rfe$shape[ix])
    p.rfe[ix] <- 1 - pars.rfe$prob[ix] + pars.rfe$prob[ix] * pgam

    res <- rep(0, length(p.rfe))
    ip <- p.rfe > (1 - pars.stn$prob)
    ip[is.na(ip)] <- FALSE
    pp <- (pars.stn$prob[ip] + p.rfe[ip] - 1)/pars.stn$prob[ip]
    res[ip] <- stats::qgamma(pp, scale = pars.stn$scale[ip], shape = pars.stn$shape[ip])
    dim(res) <- dim(x)

    miss <- is.na(res) | is.nan(res) | is.infinite(res)
    res[miss] <- x[miss]
    res[is.na(x)] <- NA

    # res[x < thres] <- 0

    return(res)
}

## to delete
#' @exportS3Method NULL
quantile.mapping.Gauss <- function(x, pars.stn, pars.reanal){
    p.reanal <- x
    ix <- !is.na(x)
    p.reanal[ix] <- stats::pnorm(x[ix], mean = pars.reanal$mean[ix], sd = pars.reanal$sd[ix])
    # p.reanal[ix][p.reanal[ix] < 0.001] <- 0.01
    # p.reanal[ix][p.reanal[ix] > 0.999] <- 0.99
    res <- stats::qnorm(p.reanal, mean = pars.stn$mean, sd = pars.stn$sd)

    miss <- is.na(res) | is.nan(res) | is.infinite(res)
    res[miss] <- x[miss]

    ## high values
    # ix <- abs((res - pars.reanal$mean) / pars.reanal$sd) > 4
    # ix[is.na(ix)] <- FALSE
    # res[ix] <- x[ix]

    return(res)
}
