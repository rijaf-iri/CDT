
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

biasCoefficients_MBIAS_wind <- function(stnData, ncInfo, params, lang.msg, GUI){
    tstep <- params$period
    bias.method <- params$BIAS$method
    min.length <- params$BIAS$min.length
    uv_one_netcdf <- params$one.ncdf

    ############### 
    Insert.Messages.Out(lang.msg[['2']], TRUE, "i", GUI)

    ptsData <- stnData[c('lon', 'lat')]
    grdData <- readNetCDFData2Points_wind(ncInfo, ptsData, uv_one_netcdf, GUI)

    if(uv_one_netcdf){
        istn <- match(ncInfo$UV$dates, stnData$dates)
    }else{
        istn <- match(ncInfo$V$dates, stnData$dates)
    }
    istn <- istn[!is.na(istn)]
    stnData$dates <- stnData$dates[istn]
    stnData$U <- stnData$U[istn, , drop = FALSE]
    stnData$V <- stnData$V[istn, , drop = FALSE]

    if(uv_one_netcdf){
        igrd <- ncInfo$UV$dates %in% stnData$dates
        ncInfo$UV$dates <- ncInfo$UV$dates[igrd]
    }else{
        igrd <- ncInfo$V$dates %in% stnData$dates
        ncInfo$U$dates <- ncInfo$U$dates[igrd]
        ncInfo$V$dates <- ncInfo$V$dates[igrd]
    }
    grdData$U <- grdData$U[igrd, , drop = FALSE]
    grdData$V <- grdData$V[igrd, , drop = FALSE]

    biasData <- list(dates = stnData$dates,
                     lon_stn = stnData$lon,
                     lat_stn = stnData$lat,
                     u_stn = stnData$U,
                     v_stn = stnData$V,
                     u_grd = grdData$U,
                     v_grd = grdData$V,
                     lon = ncInfo$ncgrid$lon,
                     lat = ncInfo$ncgrid$lat)

    Insert.Messages.Out(lang.msg[['3']], TRUE, "s", GUI)

    ############### 
    Insert.Messages.Out(lang.msg[['1']], TRUE, "i", GUI)

    biasIndex <- multiplicative.bias.index(biasData$dates, bias.method, tstep)
    if(tstep == "daily" & bias.method == "mbvar"){
        biasData$u_stn <- biasData$u_stn[biasIndex$idx, , drop = FALSE]
        biasData$v_stn <- biasData$v_stn[biasIndex$idx, , drop = FALSE]
        biasData$u_grd <- biasData$u_grd[biasIndex$idx, , drop = FALSE]
        biasData$v_grd <- biasData$v_grd[biasIndex$idx, , drop = FALSE]
    }

    bias <- lapply(biasIndex$index, function(ix){
        u_stn <- biasData$u_stn[ix, , drop = FALSE]
        u_grd <- biasData$u_grd[ix, , drop = FALSE]
        u_bs <- multiplicative.bias.fun(u_stn, u_grd, 'ugrd', min.length)
        v_stn <- biasData$v_stn[ix, , drop = FALSE]
        v_grd <- biasData$v_grd[ix, , drop = FALSE]
        v_bs <- multiplicative.bias.fun(v_stn, v_grd, 'vgrd', min.length)
        list(U = u_bs, V = v_bs)
    })

    u_bs <- do.call(rbind, lapply(bias, '[[', 'U'))
    v_bs <- do.call(rbind, lapply(bias, '[[', 'V'))
    bias <- list(U = u_bs, V = v_bs)

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

biasCoefficients_QMECDF_wind <- function(stnData, ncInfo, params, lang.msg, GUI){
    bias.method <- params$BIAS$method
    min.length <- params$BIAS$min.length
    uv_one_netcdf <- params$one.ncdf

    ############### 
    Insert.Messages.Out(lang.msg[['2']], TRUE, "i", GUI)

    boxregion <- params$BIAS[c('blon', 'blat')]
    biasData <- regrid_stn_grd_box_wind(stnData, ncInfo, boxregion, uv_one_netcdf, GUI)

    Insert.Messages.Out(lang.msg[['3']], TRUE, "s", GUI)

    ############### 
    Insert.Messages.Out(lang.msg[['1']], TRUE, "i", GUI)

    imonth <- as.numeric(substr(biasData$dates, 5, 6))
    funEcdf <- vector(mode = 'list', length = 12)

    funEcdf[1:12] <- lapply(1:12, function(m){
        ix <- which(imonth == m)
        if(length(ix) == 0) return(NULL)

        #######
        u_stn <- biasData$u_stn[ix, , drop = FALSE]
        u_grd <- biasData$u_grd[ix, , drop = FALSE]
        u_nc <- ncol(u_stn)

        u_tstn <- colSums(!is.na(u_stn)) >= min.length
        u_tgrd <- colSums(!is.na(u_grd)) >= min.length
        u_inc <- u_tstn & u_tgrd
        if(!any(u_inc)) return(NULL)

        u_llnc <- seq(u_nc)
        u_llnc <- u_llnc[u_inc]
        u_stn <- u_stn[, u_inc, drop = FALSE]
        u_grd <- u_grd[, u_inc, drop = FALSE]

        u_stn.ecdf <- vector(mode = 'list', length = u_nc)
        u_grd.ecdf <- vector(mode = 'list', length = u_nc)

        u_llstn <- seq(ncol(u_stn))
        u_stn.ecdf[u_llnc] <- lapply(u_llstn, function(j) stats::ecdf(u_stn[, j]))
        u_grd.ecdf[u_llnc] <- lapply(u_llstn, function(j) stats::ecdf(u_grd[, j]))

        ##########

        v_stn <- biasData$v_stn[ix, , drop = FALSE]
        v_grd <- biasData$v_grd[ix, , drop = FALSE]
        v_nc <- ncol(v_stn)

        v_tstn <- colSums(!is.na(v_stn)) >= min.length
        v_tgrd <- colSums(!is.na(v_grd)) >= min.length
        v_inc <- v_tstn & v_tgrd
        if(!any(v_inc)) return(NULL)

        v_llnc <- seq(v_nc)
        v_llnc <- v_llnc[v_inc]
        v_stn <- v_stn[, v_inc, drop = FALSE]
        v_grd <- v_grd[, v_inc, drop = FALSE]

        v_stn.ecdf <- vector(mode = 'list', length = v_nc)
        v_grd.ecdf <- vector(mode = 'list', length = v_nc)

        v_llstn <- seq(ncol(v_stn))
        v_stn.ecdf[v_llnc] <- lapply(v_llstn, function(j) stats::ecdf(v_stn[, j]))
        v_grd.ecdf[v_llnc] <- lapply(v_llstn, function(j) stats::ecdf(v_grd[, j]))

        ##########

        list(u_stn = u_stn.ecdf, u_grd = u_grd.ecdf,
             v_stn = v_stn.ecdf, v_grd = v_grd.ecdf)
    })

    fun.u_Stn <- lapply(funEcdf, '[[', 'u_stn')
    fun.u_Grd <- lapply(funEcdf, '[[', 'u_grd')

    fun.v_Stn <- lapply(funEcdf, '[[', 'v_stn')
    fun.v_Grd <- lapply(funEcdf, '[[', 'v_grd')

    ecdf_fun <- list(u_stn = fun.u_Stn, u_grd = fun.u_Grd,
                     v_stn = fun.v_Stn, v_grd = fun.v_Grd)

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

biasCoefficients_QMDIST_BOX_wind <- function(stnData, ncInfo, params, lang.msg, GUI){
    Insert.Messages.Out(lang.msg[['2']], TRUE, "i", GUI)

    uv_one_netcdf <- params$one.ncdf
    boxregion <- params$BIAS[c('blon', 'blat')]
    biasData <- regrid_stn_grd_box_wind(stnData, ncInfo, boxregion, uv_one_netcdf, GUI)

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

        u_stn <- biasData$u_stn[ix, , drop = FALSE]
        u_grd <- biasData$u_grd[ix, , drop = FALSE]
        u_nc <- ncol(u_stn)

        v_stn <- biasData$v_stn[ix, , drop = FALSE]
        v_grd <- biasData$v_grd[ix, , drop = FALSE]
        v_nc <- ncol(v_stn)

        u_tstn <- colSums(!is.na(u_stn)) >= params$BIAS$min.length
        u_tgrd <- colSums(!is.na(u_grd)) >= params$BIAS$min.length
        v_tstn <- colSums(!is.na(v_stn)) >= params$BIAS$min.length
        v_tgrd <- colSums(!is.na(v_grd)) >= params$BIAS$min.length
        inc <- u_tstn & u_tgrd & v_tstn & v_tgrd
        if(!any(inc)){
            u_stn.p <- lapply(parsMiss, function(n) rep(NA, u_nc))
            u_grd.p <- lapply(parsMiss, function(n) rep(NA, u_nc))
            v_stn.p <- lapply(parsMiss, function(n) rep(NA, v_nc))
            v_grd.p <- lapply(parsMiss, function(n) rep(NA, v_nc))
            ret <- list(u_stn = u_stn.p, u_grd = u_grd.p,
                        v_stn = v_stn.p, v_grd = v_grd.p)
            return(ret)
        }

        u_inc <- u_tstn & u_tgrd
        u_stn <- u_stn[, u_inc, drop = FALSE]
        u_stn.params <- get_distr_parameters(u_stn, distrInfo)
        u_stn.params <- lapply(u_stn.params, function(x){
            tmp <- rep(NA, u_nc)
            tmp[u_inc] <- x
            tmp
        })

        v_inc <- v_tstn & v_tgrd
        v_stn <- v_stn[, v_inc, drop = FALSE]
        v_stn.params <- get_distr_parameters(v_stn, distrInfo)
        v_stn.params <- lapply(v_stn.params, function(x){
            tmp <- rep(NA, v_nc)
            tmp[v_inc] <- x
            tmp
        })

        u_grd <- u_grd[, u_inc, drop = FALSE]
        u_grd.params <- get_distr_parameters(u_grd, distrInfo)
        u_grd.params <- lapply(u_grd.params, function(x){
            tmp <- rep(NA, u_nc)
            tmp[u_inc] <- x
            tmp
        })

        v_grd <- v_grd[, v_inc, drop = FALSE]
        v_grd.params <- get_distr_parameters(v_grd, distrInfo)
        v_grd.params <- lapply(v_grd.params, function(x){
            tmp <- rep(NA, v_nc)
            tmp[v_inc] <- x
            tmp
        })

        list(u_stn = u_stn.params, u_grd = u_grd.params,
             v_stn = v_stn.params, v_grd = v_grd.params)
    })

    params.u_Stn <- lapply(parsDistr, '[[', 'u_stn')
    params.u_Grd <- lapply(parsDistr, '[[', 'u_grd')

    params.v_Stn <- lapply(parsDistr, '[[', 'v_stn')
    params.v_Grd <- lapply(parsDistr, '[[', 'v_grd')

    distr_params <- list(u_stn = params.u_Stn, u_grd = params.u_Grd,
                         v_stn = params.v_Stn, v_grd = params.v_Grd)

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

biasCoefficients_QMDIST_PTS_wind <- function(stnData, ncInfo, params, outdir, lang.msg, GUI){
    Insert.Messages.Out(lang.msg[['2']], TRUE, "i", GUI)

    uv_one_netcdf <- params$one.ncdf

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

        biasData <- readNetCDFData2Directory_wind(ncInfo, datadir, uv_one_netcdf, GUI)
    }

    if(uv_one_netcdf){
        ncDates <- ncInfo$UV$dates[ncInfo$UV$exist]
    }else{
        ncDates <- ncInfo$V$dates[ncInfo$V$exist]
    }
    biasDates <- intersect(ncDates, stnData$dates)
    istn <- match(biasDates, stnData$dates)
    stnData$dates <- stnData$dates[istn]
    stnData$U <- stnData$U[istn, , drop = FALSE]
    stnData$V <- stnData$V[istn, , drop = FALSE]

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

        u_stn <- stnData$U[ix, , drop = FALSE]
        u_nc <- ncol(u_stn)
        v_stn <- stnData$V[ix, , drop = FALSE]
        v_nc <- ncol(v_stn)

        u_inc <- colSums(!is.na(u_stn)) >= params$BIAS$min.length
        v_inc <- colSums(!is.na(v_stn)) >= params$BIAS$min.length
        if(!any(u_inc) | !any(v_inc)){
            u_ret <- lapply(parsMiss, function(n) rep(NA, u_nc))
            v_ret <- lapply(parsMiss, function(n) rep(NA, v_nc))
            ret <- list(u_stn = u_ret, v_stn = v_ret)
            return(ret)
        }

        u_stn <- u_stn[, u_inc, drop = FALSE]
        u_stn.params <- get_distr_parameters(u_stn, distrInfo)
        u_ret <- lapply(u_stn.params, function(x){
            tmp <- rep(NA, u_nc)
            tmp[u_inc] <- x
            tmp
        })

        v_stn <- v_stn[, v_inc, drop = FALSE]
        v_stn.params <- get_distr_parameters(v_stn, distrInfo)
        v_ret <- lapply(v_stn.params, function(x){
            tmp <- rep(NA, v_nc)
            tmp[v_inc] <- x
            tmp
        })

        list(u_stn = u_ret, v_stn = v_ret)
    })

    Insert.Messages.Out(lang.msg[['7']], TRUE, "i", GUI)

    parsL <- doparallel.cond(length(biasData$chunks) > 20)
    ret_pars <- cdt.foreach(seq_along(biasData$chunks), parsL, GUI,
                            progress = TRUE, FUN = function(l){
        res <- lapply(c('U', 'V'), function(uv){
            datafile <- file.path(datadir, uv, paste0('chunk_', l))
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
        names(res) <- c('u_grd', 'v_grd')
        return(res)
    })

    Insert.Messages.Out(lang.msg[['5']], TRUE, "s", GUI)

    u_grd <- lapply(ret_pars, '[[', 'u_grd')
    v_grd <- lapply(ret_pars, '[[', 'v_grd')

    parsDistrGrd[1:12] <- lapply(1:12, function(m){
        u_tmp <- lapply(u_grd, '[[', m)
        u_pars <- lapply(distrInfo$pars, function(n){
            x <- lapply(u_tmp, '[[', n)
            do.call(c, x)
        })
        names(u_pars) <- distrInfo$pars

        v_tmp <- lapply(v_grd, '[[', m)
        v_pars <- lapply(distrInfo$pars, function(n){
            x <- lapply(v_tmp, '[[', n)
            do.call(c, x)
        })
        names(v_pars) <- distrInfo$pars

        if(is.null(u_pars[[1]]) | is.null(v_pars[[1]])){
            u_pars <- NULL
            v_pars <- NULL
        }

        return(list(u_grd = u_pars, v_grd = v_pars))
    })

    u_stn <- lapply(parsDistrStn, '[[', 'u_stn')
    v_stn <- lapply(parsDistrStn, '[[', 'v_stn')
    u_grd <- lapply(parsDistrGrd, '[[', 'u_grd')
    v_grd <- lapply(parsDistrGrd, '[[', 'v_grd')

    distr_params <- list(u_stn = u_stn, v_stn = v_stn,
                         u_grd = u_grd, v_grd = v_grd)

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

regrid_stn_grd_box_wind <- function(stnData, ncInfo, boxregion, oneNetCDF, GUI){
    aggr_stn_method <- biascoeff.getOption("aggrBoxMethodStation")
    aggr_grd_method <- biascoeff.getOption("aggrBoxMethodGrid")
    grdData <- readNetCDFData2AggrBox_wind(ncInfo, boxregion, aggr_grd_method, oneNetCDF, GUI)

    xypts <- as.data.frame(stnData[c('lon', 'lat')])
    sp::coordinates(xypts) <- c('lon', 'lat')

    crdsBox <- sp::coordinates(grdData$spbox)
    crdsStn <- sp::coordinates(xypts)

    stn <- lapply(seq_along(grdData$spbox), function(j){
        ix <- sp::over(xypts, grdData$spbox[j])
        ix <- which(!is.na(ix))
        if(length(ix) == 0) return(list(U = NA, V = NA))
        u_dat <- stnData$U[, ix, drop = FALSE]
        v_dat <- stnData$V[, ix, drop = FALSE]
        if(aggr_stn_method == "mean"){
            u_ret <- rowMeans(u_dat, na.rm = TRUE)
            v_ret <- rowMeans(v_dat, na.rm = TRUE)
        }
        ## interpolate stn at grdData$spbox
        if(aggr_stn_method == "idw"){
            dst <- distance.Matrix(crdsBox[j, , drop = FALSE],
                                   crdsStn[ix, , drop = FALSE])
            Wk <- 1/dst^2
            # if coord box = coord stn,
            # relocate station at 1e-05 degree (~1m)
            Wk[is.infinite(Wk)] <- 1e+10
            u_Wdat <- sweep(u_dat, 2, Wk, FUN = "*")
            u_ret <- rowSums(u_Wdat, na.rm = TRUE) / sum(Wk)
            v_Wdat <- sweep(v_dat, 2, Wk, FUN = "*")
            v_ret <- rowSums(v_Wdat, na.rm = TRUE) / sum(Wk)
        }
        return(list(U = u_ret, V = v_ret))
    })
    
    u_stn <- do.call(cbind, lapply(stn, '[[', 'U'))
    u_stn[is.nan(u_stn)] <- NA
    v_stn <- do.call(cbind, lapply(stn, '[[', 'V'))
    v_stn[is.nan(v_stn)] <- NA

    ########
    if(oneNetCDF){
        istn <- match(ncInfo$UV$dates, stnData$dates)
    }else{
        istn <- match(ncInfo$V$dates, stnData$dates)
    }
    istn <- istn[!is.na(istn)]
    stnData$dates <- stnData$dates[istn]
    u_stn <- u_stn[istn, , drop = FALSE]
    v_stn <- v_stn[istn, , drop = FALSE]

    if(oneNetCDF){
        igrd <- ncInfo$UV$dates %in% stnData$dates
    }else{
        igrd <- ncInfo$V$dates %in% stnData$dates
    }
    grdData$data$U <- grdData$data$U[igrd, , drop = FALSE]
    grdData$data$V <- grdData$data$V[igrd, , drop = FALSE]

    dg <- sqrt(boxregion$blon^2 + boxregion$blat^2)

    list(dates = stnData$dates,
         lon_stn = stnData$lon,
         lat_stn = stnData$lat,
         lon_box = grdData$lon,
         lat_box = grdData$lat,
         u_stn = u_stn,
         v_stn = v_stn,
         u_grd = grdData$data$U,
         v_grd = grdData$data$V,
         lon = ncInfo$ncgrid$lon,
         lat = ncInfo$ncgrid$lat,
         diag = dg)
}
