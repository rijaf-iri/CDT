
readBiasCoefficientFiles <- function(params, GUI = TRUE){
    xml.dlg <- file.path(.cdtDir$dirLocal, "languages", "cdtBias_Correction_ClimData_read.xml")
    lang.msg <- cdtLanguageParse(xml.dlg, .cdtData$Config$lang.iso)
    lang.msg <- lang.msg[['message']]

    Insert.Messages.Out(lang.msg[['1']], TRUE, "i", GUI)

    if(!dir.exists(params$BIAS$dir)){
        Insert.Messages.Out(paste(lang.msg[['2']], params$BIAS$dir), TRUE, "e", GUI)
        return(NULL)
    }

    bias_parFile <- file.path(params$BIAS$dir, 'input_parameters.rds')
    if(!file.exists(bias_parFile)){
        Insert.Messages.Out(paste(lang.msg[['3']], params$BIAS$dir), TRUE, "e", GUI)
        return(NULL)
    }
    bias_pars <- readRDS(bias_parFile)
    if(params$BIAS$method != bias_pars$params$BIAS$method){
        mthd <- params$bias_list$txt[params$bias_list$val == params$BIAS$method]
        Insert.Messages.Out(paste(lang.msg[['4']], mthd), TRUE, "e", GUI)
        return(NULL)
    }

    if(params$BIAS$method %in% c("mbvar", "mbmon")){
        if(params$BIAS$method == "mbvar"){
            if(params$period != bias_pars$params$period){
                txtTstep <- .cdtEnv$tcl$lang$global[['combobox']][['1']][3:6]
                valTstep <- c('daily', 'pentad', 'dekadal', 'monthly')
                txtT <- txtTstep[valTstep == params$period]
                Insert.Messages.Out(paste(lang.msg[['5']], txtT), TRUE, "e", GUI)
                return(NULL)
            }

            nbias <- switch(params$period, "daily" = 365, "pentad" = 72,
                            "dekadal" = 36, "monthly" = 12)
        }else{
            nbias <- 12
        }

        biasFiles <- file.path(params$BIAS$dir, paste0("STN_GRD_Bias_Coeff_", seq(nbias), ".nc"))
        biasExist <- file.exists(biasFiles)
        if(!any(biasExist)){
            Insert.Messages.Out(lang.msg[['6']], TRUE, "e", GUI)
            return(NULL)
        }

        biasData <- vector(mode = 'list', length = nbias)
        biasData[which(biasExist)] <- lapply(seq(nbias)[biasExist], function(m){
            nc <- ncdf4::nc_open(biasFiles[m])
            xvar <- ncdf4::ncvar_get(nc, varid = "bias")
            ncdf4::nc_close(nc)
            xvar
        })
        nc <- ncdf4::nc_open(biasFiles[biasExist][1])
        coords <- list(
                    glon = nc$dim[[1]]$vals,
                    glat = nc$dim[[2]]$vals)
        ncdf4::nc_close(nc)
    }

    if(params$BIAS$method == "qmecdf"){
        stnFiles <- file.path(params$BIAS$dir, paste0("STN_ecdf_", 1:12, ".rds"))
        grdFiles <- file.path(params$BIAS$dir, paste0("GRD_ecdf_", 1:12, ".rds"))
        biasExist <- file.exists(stnFiles) & file.exists(grdFiles)
        if(!any(biasExist)){
            Insert.Messages.Out(lang.msg[['7']], TRUE, "e", GUI)
            return(NULL)
        }

        biasData <- vector(mode = 'list', length = 12)
        biasData[which(biasExist)] <- lapply((1:12)[biasExist], function(m){
            stn <- readRDS(stnFiles[m])
            grd <- readRDS(grdFiles[m])
            list(stn = stn$data, grd = grd$data)
        })

        coords <- readRDS(stnFiles[biasExist][1])
        coords <- coords[names(coords) != 'data']
        coords$glon <- bias_pars$interp$lon
        coords$glat <- bias_pars$interp$lat
    }

    if(params$BIAS$method == "qmdist"){
        stnFiles <- paste0("STN_Distr_Pars_", bias_pars$distr_info$name, "_", 1:12, ".nc")
        stnFiles <- file.path(params$BIAS$dir, stnFiles)
        grdFiles <- paste0("GRD_Distr_Pars_", bias_pars$distr_info$name, "_", 1:12, ".nc")
        grdFiles <- file.path(params$BIAS$dir, grdFiles)
        biasExist <- file.exists(stnFiles) & file.exists(grdFiles)
        if(!any(biasExist)){
            Insert.Messages.Out(lang.msg[['7']], TRUE, "e", GUI)
            return(NULL)
        }

        biasData <- vector(mode = 'list', length = 12)
        biasData[which(biasExist)] <- lapply((1:12)[biasExist], function(m){
            nc <- ncdf4::nc_open(stnFiles[m])
            stn <- lapply(bias_pars$distr_info$pars, function(p){
                xvar <- ncdf4::ncvar_get(nc, varid = p)
                xvar[is.nan(xvar)] <- NA
                xvar
            })
            ncdf4::nc_close(nc)
            names(stn) <- bias_pars$distr_info$pars

            nc <- ncdf4::nc_open(grdFiles[m])
            grd <- lapply(bias_pars$distr_info$pars, function(p){
                xvar <- ncdf4::ncvar_get(nc, varid = p)
                xvar[is.nan(xvar)] <- NA
                xvar
            })
            ncdf4::nc_close(nc)
            names(grd) <- bias_pars$distr_info$pars

            list(stn = stn, grd = grd)
        })

        if(bias_pars$params$BIAS$ts.support == "points"){
            nc <- ncdf4::nc_open(stnFiles[biasExist][1])
            coords <- list(
                        glon = nc$dim[[1]]$vals,
                        glat = nc$dim[[2]]$vals)
            ncdf4::nc_close(nc)
        }else{
            nc <- ncdf4::nc_open(stnFiles[biasExist][1])
            coords <- list(
                        lon = nc$dim[[1]]$vals,
                        lat = nc$dim[[2]]$vals)
            ncdf4::nc_close(nc)
            coords$glon <- bias_pars$interp$lon
            coords$glat <- bias_pars$interp$lat
        }
    }

    if(any(!biasExist)){
        if(params$BIAS$method == "mbvar"){
            valMsg <- c('11', '12', '13', '14')
            valTstep <- c('daily', 'pentad', 'dekadal', 'monthly')
            im <- valMsg[valTstep == params$period]
        }else{
            im <- '14'
        }
        dd <- which(!biasExist)
        dd <- paste0(dd, collapse = ', ')
        msg <- paste(lang.msg[['8']], lang.msg[[im]], ':', dd)
        Insert.Messages.Out(msg, TRUE, "w", GUI)
        Insert.Messages.Out(lang.msg[['9']], TRUE, "w", GUI)
    }

    coefData <- list(bias = biasData, coords = coords, pars = bias_pars)

    Insert.Messages.Out(lang.msg[['10']], TRUE, "s", GUI)

    return(coefData)
}
