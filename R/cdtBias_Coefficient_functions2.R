
biasCoefficients_MBIAS <- function(stnData, ncInfo, params, variable, lang.msg, GUI){
    tstep <- params$period
    bias.method <- params$BIAS$method
    min.length <- params$BIAS$min.length

    ############### 
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
                     lon_stn = stnData$lon,
                     lat_stn = stnData$lat,
                     stn = stnData$data,
                     grd = grdData,
                     lon = ncInfo$ncgrid$lon,
                     lat = ncInfo$ncgrid$lat)

    Insert.Messages.Out(lang.msg[['3']], TRUE, "s", GUI)

    ############### 
    Insert.Messages.Out(lang.msg[['1']], TRUE, "i", GUI)

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

    out_params <- params[!names(params) %in% c("settingSNC", "message")]
    out_params$options <- biascoeff.options()
    bias.pars <- list(method = bias.method, bias = bias,
                      data = biasData, params = out_params)

    Insert.Messages.Out(lang.msg[['6']], TRUE, "s", GUI)

    return(bias.pars)
}

biasCoefficients_QMECDF <- function(stnData, ncInfo, params, lang.msg, GUI){
    bias.method <- params$BIAS$method
    min.length <- params$BIAS$min.length

    ############### 
    Insert.Messages.Out(lang.msg[['2']], TRUE, "i", GUI)

    boxregion <- params$BIAS[c('blon', 'blat')]
    biasData <- regrid_stn_grd_box(stnData, ncInfo, boxregion, GUI)

    Insert.Messages.Out(lang.msg[['3']], TRUE, "s", GUI)

    ############### 
    Insert.Messages.Out(lang.msg[['1']], TRUE, "i", GUI)

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
        stn.ecdf[llnc] <- lapply(llstn, function(j) stats::ecdf(stn[, j]))
        grd.ecdf[llnc] <- lapply(llstn, function(j) stats::ecdf(grd[, j]))

        list(stn = stn.ecdf, grd = grd.ecdf)
    })

    fun.Stn <- lapply(funEcdf, '[[', 'stn')
    fun.Grd <- lapply(funEcdf, '[[', 'grd')
    ecdf_fun <- list(stn = fun.Stn, grd = fun.Grd)

    out_params <- params[!names(params) %in% c("settingSNC", "message")]
    out_params$options <- biascoeff.options()

    bias.pars <- list(method = bias.method,
                      ecdf_fun = ecdf_fun,
                      data = biasData,
                      params = out_params)

    Insert.Messages.Out(lang.msg[['6']], TRUE, "s", GUI)

    return(bias.pars)
}

biasCoefficients_QMDIST_BOX <- function(stnData, ncInfo, params, lang.msg, GUI){
    Insert.Messages.Out(lang.msg[['2']], TRUE, "i", GUI)

    boxregion <- params$BIAS[c('blon', 'blat')]
    biasData <- regrid_stn_grd_box(stnData, ncInfo, boxregion, GUI)

    Insert.Messages.Out(lang.msg[['3']], TRUE, "s", GUI)

    ############### 
    Insert.Messages.Out(lang.msg[['1']], TRUE, "i", GUI)

    distrInfo <- get_distribution_infos(params$BIAS$distr.name)

    imonth <- as.numeric(substr(biasData$dates, 5, 6))
    parsDistr <- vector(mode = 'list', length = 12)
    parsMiss <- vector(mode = 'list', length = length(distrInfo$pars))
    names(parsMiss) <- distrInfo$pars

    parsDistr[1:12] <- lapply(1:12, function(m){
        ix <- which(imonth == m)
        if(length(ix) == 0) return(NULL)
        stn <- biasData$stn[ix, , drop = FALSE]
        grd <- biasData$grd[ix, , drop = FALSE]
        nc <- ncol(stn)

        tstn <- colSums(!is.na(stn)) >= params$BIAS$min.length
        tgrd <- colSums(!is.na(grd)) >= params$BIAS$min.length
        inc <- tstn & tgrd
        if(!any(inc)){
            stn.params <- lapply(parsMiss, function(n) rep(NA, nc))
            grd.params <- lapply(parsMiss, function(n) rep(NA, nc))
            ret <- list(stn = stn.params, grd = grd.params)
            return(ret)
        }

        stn <- stn[, inc, drop = FALSE]
        stn.params <- get_distr_parameters(stn, distrInfo)
        stn.params <- lapply(stn.params, function(x){
            tmp <- rep(NA, nc)
            tmp[inc] <- x
            tmp
        })

        grd <- grd[, inc, drop = FALSE]
        grd.params <- get_distr_parameters(grd, distrInfo)
        grd.params <- lapply(grd.params, function(x){
            tmp <- rep(NA, nc)
            tmp[inc] <- x
            tmp
        })

        list(stn = stn.params, grd = grd.params)
    })

    params.Stn <- lapply(parsDistr, '[[', 'stn')
    params.Grd <- lapply(parsDistr, '[[', 'grd')
    distr_params <- list(stn = params.Stn, grd = params.Grd)

    out_params <- params[!names(params) %in% c("settingSNC", "message")]
    out_params$options <- biascoeff.options()

    bias.pars <- list(method = params$BIAS$method,
                      distr_params = distr_params,
                      distr_info = distrInfo,
                      data = biasData,
                      params = out_params)

    Insert.Messages.Out(lang.msg[['6']], TRUE, "s", GUI)

    return(bias.pars)
}

biasCoefficients_QMDIST_PTS <- function(stnData, ncInfo, params, outdir, lang.msg, GUI){
    Insert.Messages.Out(lang.msg[['2']], TRUE, "i", GUI)

    if(params$BIAS$chunks.exist){
        datadir <- params$BIAS$chunks.dir
        if(!dir.exists(datadir)){
            Insert.Messages.Out(lang.msg[['10']], TRUE, "e", GUI)
            return(NULL)
        }
        indexfile <- file.path(datadir, 'index.rds')
        if(!file.exists(indexfile)){
            Insert.Messages.Out(lang.msg[['11']], TRUE, "e", GUI)
            return(NULL)
        }

        biasData <- readRDS(indexfile)
        if(WindowsOS()){
            file.copy(datadir, outdir, overwrite = TRUE, recursive = TRUE)
        }else{
            file.symlink(datadir, outdir)
        }
    }else{
        datadir <- file.path(outdir, 'NCDATA_CHUNKS')
        if(dir.exists(datadir)) unlink(datadir, recursive = TRUE)
        dir.create(datadir, showWarnings = FALSE, recursive = TRUE)

        biasData <- readNetCDFData2Directory(ncInfo, datadir, GUI)
    }

    biasDates <- intersect(ncInfo$dates[ncInfo$exist], stnData$dates)
    istn <- match(biasDates, stnData$dates)
    stnData$dates <- stnData$dates[istn]
    stnData$data <- stnData$data[istn, , drop = FALSE]

    biasData$dates <- biasDates
    biasData$lon_stn <- stnData$lon
    biasData$lat_stn <- stnData$lat

    Insert.Messages.Out(lang.msg[['3']], TRUE, "s", GUI)

    ############### 
    Insert.Messages.Out(lang.msg[['1']], TRUE, "i", GUI)

    distrInfo <- get_distribution_infos(params$BIAS$distr.name)

    imonth <- as.numeric(substr(biasDates, 5, 6))
    parsDistrStn <- vector(mode = 'list', length = 12)
    parsDistrGrd <- vector(mode = 'list', length = 12)
    parsMiss <- vector(mode = 'list', length = length(distrInfo$pars))
    names(parsMiss) <- distrInfo$pars

    parsDistrStn[1:12] <- lapply(1:12, function(m){
        ix <- which(imonth == m)
        if(length(ix) == 0) return(NULL)
        stn <- stnData$data[ix, , drop = FALSE]
        nc <- ncol(stn)

        inc <- colSums(!is.na(stn)) >= params$BIAS$min.length
        if(!any(inc)){
            ret <- lapply(parsMiss, function(n) rep(NA, nc))
            return(ret)
        }

        stn <- stn[, inc, drop = FALSE]
        stn.params <- get_distr_parameters(stn, distrInfo)

        lapply(stn.params, function(x){
            tmp <- rep(NA, nc)
            tmp[inc] <- x
            tmp
        })
    })

    Insert.Messages.Out(lang.msg[['7']], TRUE, "i", GUI)

    parsL <- doparallel.cond(length(biasData$chunks) > 20)
    ret_pars <- cdt.foreach(seq_along(biasData$chunks), parsL, GUI,
                            progress = TRUE, FUN = function(l){
        datafile <- file.path(datadir, paste0('chunk_', l))
        tmp <- readLines(datafile)
        tmp <- strsplit(tmp, ',')
        tmp <- do.call(rbind, tmp)
        daty <- tmp[, 1]
        tmp <- tmp[, -1, drop = FALSE]

        it <- match(biasDates, daty)
        tmp <- tmp[it, , drop = FALSE]

        ntmp <- dim(tmp)
        tmp <- as.numeric(tmp)
        dim(tmp) <- ntmp

        parsTmp <- vector(mode = 'list', length = 12)
        parsTmp[1:12] <- lapply(1:12, function(m){
            ix <- which(imonth == m)
            if(length(ix) == 0) return(NULL)

            grd <- tmp[ix, , drop = FALSE]
            nc <- ncol(grd)

            inc <- colSums(!is.na(grd)) >= params$BIAS$min.length
            if(!any(inc)){
                ret <- lapply(parsMiss, function(n) rep(NA, nc))
                return(ret)
            }

            grd <- grd[, inc, drop = FALSE]
            tmp.params <- get_distr_parameters(grd, distrInfo)

            lapply(tmp.params, function(x){
                v <- rep(NA, nc)
                v[inc] <- x
                v
            })
        })
        
        return(parsTmp)
    })

    Insert.Messages.Out(lang.msg[['5']], TRUE, "s", GUI)

    parsDistrGrd[1:12] <- lapply(1:12, function(m){
        tmp <- lapply(ret_pars, '[[', m)
        pars <- lapply(distrInfo$pars, function(n){
            x <- lapply(tmp, '[[', n)
            do.call(c, x)
        })
        names(pars) <- distrInfo$pars

        if(is.null(pars[[1]])) pars <- NULL

        return(pars)
    })

    distr_params <- list(stn = parsDistrStn, grd = parsDistrGrd)

    out_params <- params[!names(params) %in% c("settingSNC", "message")]
    out_params$options <- biascoeff.options()

    bias.pars <- list(method = params$BIAS$method,
                      distr_params = distr_params,
                      distr_info = distrInfo,
                      data = biasData,
                      params = out_params)

    Insert.Messages.Out(lang.msg[['6']], TRUE, "s", GUI)

    return(bias.pars)
}

get_distribution_infos <- function(distr.name){
    distrInfo <- switch(distr.name,
                        'norm' = list(pars = c('mean', 'sd'), longname = "Normal"),
                        'lnorm' = list(pars = c('meanlog', 'sdlog'), longname = "Log-Normal"),
                        'snorm' = list(pars = c('mean', 'sd', 'xi'), longname = "Skew Normal"),
                        'gamma' = list(pars = c('shape', 'scale'), longname = "Gamma"),
                        'exp' = list(pars = c('rate'), longname = "Exponential"),
                        'weibull' = list(pars = c('shape', 'scale'), longname = "Weibull"),
                        'gumbel' = list(pars = c('loc', 'scale'), longname = "Gumbel"),
                        'berngamma' = list(pars = c('prob', 'shape', 'scale'), longname = "Bernoulli-Gamma"),
                        'bernexp' = list(pars = c('prob', 'rate'), longname = "Bernoulli-Exponential"),
                        'bernlnorm' = list(pars = c('prob', 'meanlog', 'sdlog'), longname = "Bernoulli-Log-Normal"),
                        'bernweibull' = list(pars = c('prob', 'shape', 'scale'), longname = "Bernoulli-Weibull"))

    distrInfo$name <- distr.name
    distr_bern <- c('berngamma', 'bernexp', 'bernlnorm', 'bernweibull')
    if(distr.name %in% distr_bern){
        distrInfo$thres <- biascoeff.getOption("rainyEventThres")
    }

    return(distrInfo)
}

regrid_stn_grd_box <- function(stnData, ncInfo, boxregion, GUI){
    aggr_stn_method <- biascoeff.getOption("aggrBoxMethodStation")
    aggr_grd_method <- biascoeff.getOption("aggrBoxMethodGrid")
    grdData <- readNetCDFData2AggrBox(ncInfo, boxregion, aggr_grd_method, GUI)

    xypts <- as.data.frame(stnData[c('lon', 'lat')])
    sp::coordinates(xypts) <- c('lon', 'lat')

    crdsBox <- sp::coordinates(grdData$spbox)
    crdsStn <- sp::coordinates(xypts)

    stn <- lapply(seq_along(grdData$spbox), function(j){
        ix <- sp::over(xypts, grdData$spbox[j])
        ix <- which(!is.na(ix))
        if(length(ix) == 0) return(NA)
        dat <- stnData$data[, ix, drop = FALSE]
        if(aggr_stn_method == "mean"){
            ret <- rowMeans(dat, na.rm = TRUE)
        }
        ## interpolate stn at grdData$spbox
        if(aggr_stn_method == "idw"){
            dst <- distance.Matrix(crdsBox[j, , drop = FALSE],
                                   crdsStn[ix, , drop = FALSE])
            Wk <- 1/dst^2
            # if coord box = coord stn,
            # relocate station at 1e-05 degree (~1m)
            Wk[is.infinite(Wk)] <- 1e+10
            Wdat <- sweep(dat, 2, Wk, FUN = "*")
            ret <- rowSums(Wdat, na.rm = TRUE) / sum(Wk)
        }
        return(ret)
    })
    stn <- do.call(cbind, stn)
    stn[is.nan(stn)] <- NA

    ########
    istn <- match(ncInfo$dates, stnData$dates)
    istn <- istn[!is.na(istn)]
    stnData$dates <- stnData$dates[istn]
    stn <- stn[istn, , drop = FALSE]
    igrd <- ncInfo$dates %in% stnData$dates
    # ncInfo$dates <- ncInfo$dates[igrd]
    grdData$data <- grdData$data[igrd, , drop = FALSE]

    dg <- sqrt(boxregion$blon^2 + boxregion$blat^2)

    list(dates = stnData$dates,
         lon_stn = stnData$lon,
         lat_stn = stnData$lat,
         lon_box = grdData$lon,
         lat_box = grdData$lat,
         stn = stn,
         grd = grdData$data,
         lon = ncInfo$ncgrid$lon,
         lat = ncInfo$ncgrid$lat,
         diag = dg)
}
