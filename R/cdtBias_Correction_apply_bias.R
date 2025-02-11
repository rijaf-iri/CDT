
applyBiasCorrectionClimData <- function(coefBias, ncInfo, outdir, params, variable, GUI = TRUE){
    xml.dlg <- file.path(.cdtDir$dirLocal, "languages", "cdtBias_Correction_apply_bias.xml")
    lang.msg <- cdtLanguageParse(xml.dlg, .cdtData$Config$lang.iso)
    lang.msg <- lang.msg[['message']]

    Insert.Messages.Out(lang.msg[['1']], TRUE, "i", GUI)

    ############

    if(variable == 'wspd'){
        params$output$format <- params$output$format.S
    }
    missval <- -99
    datainfo <- merging.getOption('netCDFDataDef')[[variable]]
    varinfo <- ncInfo$ncinfo$varinfo

    ############

    newCoords <- coefBias$coords[c("glon", "glat")]
    names(newCoords) <- c("lon", "lat")
    ncCoords <- ncInfo$ncinfo[c("lon", "lat")]
    is.regridNCDF <- is.diffSpatialPixelsObj(defSpatialPixels(newCoords),
                                             defSpatialPixels(ncCoords),
                                             tol = 1e-03)
    ############

    dx <- ncdf4::ncdim_def("Lon", "degree_east", newCoords$lon)
    dy <- ncdf4::ncdim_def("Lat", "degree_north", newCoords$lat)
    xy.dim <- list(dx, dy)
    grd.ncout <- ncdf4::ncvar_def(varinfo$name, varinfo$units, xy.dim, missval,
                                  longname = paste("Bias Corrected", varinfo$longname),
                                  prec = varinfo$prec, compression = 9)
    ############

    dates <- ncInfo$dates[ncInfo$exist]
    ncInfo$ncfiles <- ncInfo$ncfiles[ncInfo$exist]

    tdate <- table.format.date.time1(params$period, dates)
    if(params$period == 'monthly'){
        ncout <- sprintf(params$output$format, tdate[, 1], tdate[, 2])
    }else{
        ncout <- sprintf(params$output$format, tdate[, 1], tdate[, 2], tdate[, 3])
    }
    ncoutFiles <- file.path(outdir, ncout)

    if(params$BIAS$method == "mbvar" && params$period != 'monthly'){
        index <- as.numeric(tdate[, 4])
        if(params$period == 'daily'){
            index <- ifelse(is.leapyears(as.numeric(tdate[, 1])) & index > 59, index - 1, index)
        }
    }else{
        index <- as.numeric(tdate[, 2])
    }

    ############

    ncinfo <- ncInfo$ncinfo[c('varid', 'ilon', 'ilat', 'xo', 'yo', 'nx', 'ny')]

    checkerFile <- file.path(dirname(ncInfo$ncfiles[1]), '.checker')
    if(file.exists(checkerFile)) unlink(checkerFile)

    parsL <- doparallel.cond(length(ncInfo$ncfiles) > 100)
    ret <- cdt.foreach(seq_along(ncInfo$ncfiles), parsL, GUI,
                       progress = TRUE, FUN = function(jj)
    {
        if(file.exists(checkerFile)) return(NULL)

        nc <- try(ncdf4::nc_open(ncInfo$ncfiles[jj]), silent = TRUE)
        if(inherits(nc, "try-error")){
            write('', checkerFile)
            msg <- paste(as.character(nc), '\n', lang.msg[['3']], ncInfo$ncfiles[jj])
            return(msg)
        }
        xvar <- ncdf4::ncvar_get(nc, varid = ncinfo$varid)
        ncdf4::nc_close(nc)
        if(ncinfo$nx != nrow(xvar) || ncinfo$ny != ncol(xvar)){
            write('', checkerFile)
            msg <- paste(lang.msg[['4']], ncInfo$ncfiles[jj])
            return(msg)
        }
        xvar <- transposeNCDFData(xvar, ncinfo)

        if(is.regridNCDF){
            tmp <- c(ncCoords, list(z = xvar))
            xvar <- cdt.interp.surface.grid(tmp, newCoords)
            xvar <- xvar$z
        }

        ########

        biasData <- coefBias$bias[[index[jj]]]

        if(is.null(biasData)){
            xadj <- xvar
            xadj[xadj < datainfo$min] <- datainfo$min
            xadj[xadj > datainfo$max] <- datainfo$max
            xadj[is.na(xadj)] <- missval
            nc <- ncdf4::nc_create(ncoutFiles[jj], grd.ncout)
            ncdf4::ncvar_put(nc, grd.ncout, xadj)
            ncdf4::nc_close(nc)

            return(NULL)
        }

        ########

        if(params$BIAS$method %in% c("mbvar", "mbmon")){
            xadj <- xvar * biasData
        }

        if(params$BIAS$method == "qmecdf"){
            interp <- coefBias$pars$interp
            tmp <- lapply(seq(nrow(interp$id)), function(l){
                par.stn <- biasData$stn[interp$id[l, ]]
                par.grd <- biasData$grd[interp$id[l, ]]
                quantile.mapping.ecdf(c(xvar), par.stn, par.grd)
            })

            xval <- do.call(rbind, lapply(tmp, '[[', 'data'))
            xadj <- colSums(interp$wk * xval, na.rm = TRUE) / colSums(interp$wk, na.rm = TRUE)
            # ina <- Reduce('|', lapply(tmp, '[[', 'index'))
            # xadj[!ina] <- xvar[!ina]
            dim(xadj) <- dim(xvar)
        }

        if(params$BIAS$method == "qmdist"){
            distr.name <- coefBias$pars$distr_info$name
            thres <- coefBias$pars$distr_info$thres

            if(coefBias$pars$params$BIAS$ts.support == "rectbox"){
                interp <- coefBias$pars$interp
                tmp <- lapply(seq(nrow(interp$id)), function(l){
                    par.stn <- lapply(biasData$stn, function(pr) pr[interp$id[l, ]])
                    par.grd <- lapply(biasData$grd, function(pr) pr[interp$id[l, ]])
                    if(is.null(thres)){
                        quantile.mapping.statsdistr(c(xvar), par.stn, par.grd, distr.name)
                    }else{
                        quantile.mapping.berndistr(c(xvar), par.stn, par.grd, distr.name, thres)
                    }
                })

                xval <- do.call(rbind, lapply(tmp, '[[', 'data'))
                xadj <- colSums(interp$wk * xval, na.rm = TRUE) / colSums(interp$wk, na.rm = TRUE)
                # ina <- Reduce('|', lapply(tmp, '[[', 'index'))
                # xadj[!ina] <- xvar[!ina]
                dim(xadj) <- dim(xvar)
            }else{
                par.stn <- biasData$stn
                par.grd <- biasData$grd
                if(is.null(thres)){
                    xval <- quantile.mapping.statsdistr(xvar, par.stn, par.grd, distr.name)
                }else{
                    xval <- quantile.mapping.berndistr(xvar, par.stn, par.grd, distr.name, thres)
                }
                xadj <- xval$data
            }
        }

        ########
        xadj[xadj < datainfo$min] <- datainfo$min
        xadj[xadj > datainfo$max] <- datainfo$max
        xadj[is.na(xadj)] <- missval
        nc <- ncdf4::nc_create(ncoutFiles[jj], grd.ncout)
        ncdf4::ncvar_put(nc, grd.ncout, xadj)
        ncdf4::nc_close(nc)

        return(NULL)
    })

    if(file.exists(checkerFile)) unlink(checkerFile)

    inull <- !sapply(ret, is.null)
    if(any(inull)){
        msgs <- unlist(ret[inull])
        for(j in seq_along(msgs)) Insert.Messages.Out(msgs[j], TRUE, "w", GUI)
    }

    Insert.Messages.Out(lang.msg[['2']], TRUE, "s", GUI)

    return(0)
}

applyBiasCorrectionWind <- function(coefBias, ncInfo, outdir, params, GUI = TRUE){
    xml.dlg <- file.path(.cdtDir$dirLocal, "languages", "cdtBias_Correction_apply_bias.xml")
    lang.msg <- cdtLanguageParse(xml.dlg, .cdtData$Config$lang.iso)
    lang.msg <- lang.msg[['message']]

    Insert.Messages.Out(lang.msg[['1']], TRUE, "i", GUI)

    ############
    uv_one_netcdf <- params$one.ncdf

    if(uv_one_netcdf){
        varinfo <- ncInfo$ncinfo$UV$varinfo
    }else{
        # varinfo <- ncInfo$ncinfo$U$varinfo
        varinfo <- ncInfo$ncinfo$V$varinfo
    }

    ############

    newCoords <- coefBias$coords[c("glon", "glat")]
    names(newCoords) <- c("lon", "lat")
    ncCoords <- ncInfo$ncinfo[c("lon", "lat")]
    is.regridNCDF <- is.diffSpatialPixelsObj(defSpatialPixels(newCoords),
                                             defSpatialPixels(ncCoords),
                                             tol = 1e-03)
    ############

    dx <- ncdf4::ncdim_def("Lon", "degree_east", newCoords$lon)
    dy <- ncdf4::ncdim_def("Lat", "degree_north", newCoords$lat)
    xy.dim <- list(dx, dy)
    adj.dim <- sapply(newCoords, length)
    missval <- -99

    if(uv_one_netcdf){
        uvgrd.ncout <- list(
            ncdf4::ncvar_def('ugrd', varinfo$units, xy.dim, missval,
                             longname = "Bias Corrected U-wind",
                             prec = varinfo$prec, compression = 9),
            ncdf4::ncvar_def('vgrd', varinfo$units, xy.dim, missval,
                             longname = "Bias Corrected V-wind",
                             prec = varinfo$prec, compression = 9)
        )
    }else{
        ugrd.ncout <- ncdf4::ncvar_def('ugrd', varinfo$units, xy.dim, missval,
                                       longname = "Bias Corrected U-wind",
                                       prec = varinfo$prec, compression = 9)
        vgrd.ncout <- ncdf4::ncvar_def('vgrd', varinfo$units, xy.dim, missval,
                                       longname = "Bias Corrected V-wind",
                                       prec = varinfo$prec, compression = 9)
    }

    ############

    if(uv_one_netcdf){
        dates <- ncInfo$UV$dates[ncInfo$UV$exist]
        ncInfo$UV$ncfiles <- ncInfo$UV$ncfiles[ncInfo$UV$exist]
    }else{
        dates <- ncInfo$U$dates[ncInfo$U$exist]
        # dates <- ncInfo$V$dates[ncInfo$V$exist]
        ncInfo$U$ncfiles <- ncInfo$U$ncfiles[ncInfo$U$exist]
        ncInfo$V$ncfiles <- ncInfo$V$ncfiles[ncInfo$V$exist]
    }

    tdate <- table.format.date.time1(params$period, dates)

    if(params$period == 'monthly'){
        if(uv_one_netcdf){
            uv_ncout <- sprintf(params$output$format.UV, tdate[, 1], tdate[, 2])
        }else{
            u_ncout <- sprintf(params$output$format.U, tdate[, 1], tdate[, 2])
            v_ncout <- sprintf(params$output$format.V, tdate[, 1], tdate[, 2])
        }
    }else{
        if(uv_one_netcdf){
            uv_ncout <- sprintf(params$output$format.UV, tdate[, 1], tdate[, 2], tdate[, 3])
        }else{
            u_ncout <- sprintf(params$output$format.U, tdate[, 1], tdate[, 2], tdate[, 3])
            v_ncout <- sprintf(params$output$format.V, tdate[, 1], tdate[, 2], tdate[, 3])
        }
    }

    if(uv_one_netcdf){
        uv_ncoutFiles <- file.path(outdir, uv_ncout)
    }else{
        u_ncoutFiles <- file.path(outdir, u_ncout)
        v_ncoutFiles <- file.path(outdir, v_ncout)
    }

    if(params$BIAS$method == "mbvar" && params$period != 'monthly'){
        index <- as.numeric(tdate[, 4])
        if(params$period == 'daily'){
            index <- ifelse(is.leapyears(as.numeric(tdate[, 1])) & index > 59, index - 1, index)
        }
    }else{
        index <- as.numeric(tdate[, 2])
    }

    ############

    if(uv_one_netcdf){
        checkerFile <- file.path(dirname(ncInfo$UV$ncfiles[1]), '.checker')
        parsL <- doparallel.cond(length(which(ncInfo$UV$exist)) >= 100)
        seq_ncfiles <- seq_along(ncInfo$UV$ncfiles)
    }else{
        checkerFile <- file.path(dirname(ncInfo$V$ncfiles[1]), '.checker')
        parsL <- doparallel.cond(length(which(ncInfo$V$exist)) >= 100)
        seq_ncfiles <- seq_along(ncInfo$V$ncfiles)
    }
    if(file.exists(checkerFile)) unlink(checkerFile)

    ret <- cdt.foreach(seq_ncfiles, parsL, GUI,
                       progress = TRUE, FUN = function(jj)
    {
        if(file.exists(checkerFile)) return(NULL)

        if(uv_one_netcdf){
            nc <- try(ncdf4::nc_open(ncInfo$UV$ncfiles[jj]), silent = TRUE)
            if(inherits(nc, "try-error")){
                write('', checkerFile)
                msg <- paste(as.character(nc), '\n', lang.msg[['3']], ncInfo$UV$ncfiles[jj])
                return(msg)
            }
            u_xvar <- ncdf4::ncvar_get(nc, varid = ncInfo$ncinfo$varidU)
            v_xvar <- ncdf4::ncvar_get(nc, varid = ncInfo$ncinfo$varidV)
            ncdf4::nc_close(nc)
            if(ncInfo$ncinfo$UV$nx != nrow(u_xvar) ||
               ncInfo$ncinfo$UV$ny != ncol(u_xvar)){
                write('', checkerFile)
                msg <- paste(lang.msg[['4']], ncInfo$UV$ncfiles[jj])
                return(msg)
            }
            u_xvar <- transposeNCDFData(u_xvar, ncInfo$ncinfo$UV)
            v_xvar <- transposeNCDFData(v_xvar, ncInfo$ncinfo$UV)
            wind_xvar <- list(u_xvar, v_xvar)

            if(is.regridNCDF){
                wind_xvar <- lapply(wind_xvar, function(uv_xvar){
                    tmp <- c(ncCoords, list(z = uv_xvar))
                    xvar <- cdt.interp.surface.grid(tmp, newCoords)
                    xvar$z
                })
            }
        }else{
            wind_xvar <- lapply(c('U', 'V'), function(uv){
                nc <- try(ncdf4::nc_open(ncInfo[[uv]]$ncfiles[jj]), silent = TRUE)
                if(inherits(nc, "try-error")){
                    write('', checkerFile)
                    msg <- paste(as.character(nc), '\n', lang.msg[['3']], ncInfo[[uv]]$ncfiles[jj])
                    return(list(data = NULL, msg = msg))
                }
                varid <- paste0('varid', uv)
                xvar <- ncdf4::ncvar_get(nc, varid = ncInfo$ncinfo[[varid]])
                ncdf4::nc_close(nc)
                if(ncInfo$ncinfo[[uv]]$nx != nrow(xvar) |
                   ncInfo$ncinfo[[uv]]$ny != ncol(xvar)){
                    write('', checkerFile)
                    msg <- paste(lang.msg[['4']], ncInfo[[uv]]$ncfiles[jj])
                    return(list(data = NULL, msg = msg))
                }
                xvar <- transposeNCDFData(xvar, ncInfo$ncinfo[[uv]])

                if(is.regridNCDF){
                    tmp <- c(ncCoords, list(z = xvar))
                    xvar <- cdt.interp.surface.grid(tmp, newCoords)
                    xvar <- xvar$z
                }

                return(list(data = xvar, msg = NULL))
            })
            msgs <- lapply(wind_xvar, '[[', 'msg')
            inull <- !sapply(msgs, is.null)
            if(any(inull)) return(unlist(msgs[inull]))
            wind_xvar <- lapply(wind_xvar, '[[', 'data')
        }

        ########

        biasData <- coefBias$bias[[index[jj]]]

        if(is.null(biasData)){
            xadj <- lapply(wind_xvar, function(xvar){
                xvar[xvar < -120] <- -120
                xvar[xvar > 120] <- 120
                xvar[is.na(xvar)] <- missval
                xvar
            })
            if(uv_one_netcdf){
                nc <- ncdf4::nc_create(uv_ncoutFiles[jj], uvgrd.ncout)
                ncdf4::ncvar_put(nc, uvgrd.ncout[[1]], xadj[[1]])
                ncdf4::ncvar_put(nc, uvgrd.ncout[[2]], xadj[[2]])
                ncdf4::nc_close(nc)
            }else{
                nc <- ncdf4::nc_create(u_ncoutFiles[jj], ugrd.ncout)
                ncdf4::ncvar_put(nc, ugrd.ncout, xadj[[1]])
                ncdf4::nc_close(nc)

                nc <- ncdf4::nc_create(v_ncoutFiles[jj], vgrd.ncout)
                ncdf4::ncvar_put(nc, vgrd.ncout, xadj[[2]])
                ncdf4::nc_close(nc)
            }
            return(NULL)
        }

        ########

        if(params$BIAS$method %in% c("mbvar", "mbmon")){
            xadj <- lapply(1:2, function(k){
                wind_xvar[[k]] * biasData[[k]]
            })
        }

        if(params$BIAS$method == "qmecdf"){
            interp <- coefBias$pars$interp
            tmp <- lapply(seq(nrow(interp$id)), function(l){
                u_par.stn <- biasData$u_stn[interp$id[l, ]]
                u_par.grd <- biasData$u_grd[interp$id[l, ]]
                u_adj <- quantile.mapping.ecdf(c(wind_xvar[[1]]), u_par.stn, u_par.grd)

                v_par.stn <- biasData$v_stn[interp$id[l, ]]
                v_par.grd <- biasData$v_grd[interp$id[l, ]]
                v_adj <- quantile.mapping.ecdf(c(wind_xvar[[2]]), v_par.stn, v_par.grd)

                list(U = u_adj, V = v_adj)
            })

            xadj <- lapply(c('U', 'V'), function(uv){
                xx <- lapply(tmp, '[[', uv)
                xval <- do.call(rbind, lapply(xx, '[[', 'data'))
                xval <- colSums(interp$wk * xval, na.rm = TRUE) / colSums(interp$wk, na.rm = TRUE)
                dim(xval) <- adj.dim
                xval
            })
        }

        if(params$BIAS$method == "qmdist"){
            distr.name <- coefBias$pars$distr_info$name
            thres <- coefBias$pars$distr_info$thres

            if(coefBias$pars$params$BIAS$ts.support == "rectbox"){
                interp <- coefBias$pars$interp
                tmp <- lapply(seq(nrow(interp$id)), function(l){
                    u_par.stn <- lapply(biasData$u_stn, function(pr) pr[interp$id[l, ]])
                    u_par.grd <- lapply(biasData$u_grd, function(pr) pr[interp$id[l, ]])
                    v_par.stn <- lapply(biasData$v_stn, function(pr) pr[interp$id[l, ]])
                    v_par.grd <- lapply(biasData$v_grd, function(pr) pr[interp$id[l, ]])

                    if(is.null(thres)){
                        u_adj <- quantile.mapping.statsdistr(c(wind_xvar[[1]]), u_par.stn, u_par.grd, distr.name)
                        v_adj <- quantile.mapping.statsdistr(c(wind_xvar[[2]]), v_par.stn, v_par.grd, distr.name)
                    }else{
                        u_adj <- quantile.mapping.berndistr(c(wind_xvar[[1]]), u_par.stn, u_par.grd, distr.name, thres)
                        v_adj <- quantile.mapping.berndistr(c(wind_xvar[[2]]), v_par.stn, v_par.grd, distr.name, thres)
                    }

                    list(U = u_adj, V = v_adj)
                })

                xadj <- lapply(c('U', 'V'), function(uv){
                    xx <- lapply(tmp, '[[', uv)
                    xval <- do.call(rbind, lapply(xx, '[[', 'data'))
                    xval <- colSums(interp$wk * xval, na.rm = TRUE) / colSums(interp$wk, na.rm = TRUE)
                    dim(xval) <- adj.dim
                    xval
                })
            }else{
                u_par.stn <- biasData$u_stn
                u_par.grd <- biasData$u_grd
                v_par.stn <- biasData$v_stn
                v_par.grd <- biasData$v_grd
                if(is.null(thres)){
                    u_xval <- quantile.mapping.statsdistr(wind_xvar[[1]], u_par.stn, u_par.grd, distr.name)
                    v_xval <- quantile.mapping.statsdistr(wind_xvar[[2]], v_par.stn, v_par.grd, distr.name)
                }else{
                    u_xval <- quantile.mapping.berndistr(wind_xvar[[1]], u_par.stn, u_par.grd, distr.name, thres)
                    v_xval <- quantile.mapping.berndistr(wind_xvar[[2]], v_par.stn, v_par.grd, distr.name, thres)
                }
                
                xadj <- list(u_xval$data, v_xval$data)
            }
        }

        ########

        xadj <- lapply(xadj, function(uv){
            uv[uv < -120] <- -120
            uv[uv > 120] <- 120
            uv[is.na(uv)] <- missval
            uv
        })

        if(uv_one_netcdf){
            nc <- ncdf4::nc_create(uv_ncoutFiles[jj], uvgrd.ncout)
            ncdf4::ncvar_put(nc, uvgrd.ncout[[1]], xadj[[1]])
            ncdf4::ncvar_put(nc, uvgrd.ncout[[2]], xadj[[2]])
            ncdf4::nc_close(nc)
        }else{
            nc <- ncdf4::nc_create(u_ncoutFiles[jj], ugrd.ncout)
            ncdf4::ncvar_put(nc, ugrd.ncout, xadj[[1]])
            ncdf4::nc_close(nc)

            nc <- ncdf4::nc_create(v_ncoutFiles[jj], vgrd.ncout)
            ncdf4::ncvar_put(nc, vgrd.ncout, xadj[[2]])
            ncdf4::nc_close(nc)
        }

        return(NULL)
    })

    if(file.exists(checkerFile)) unlink(checkerFile)

    inull <- !sapply(ret, is.null)
    if(any(inull)){
        msgs <- unlist(ret[inull])
        for(j in seq_along(msgs)) Insert.Messages.Out(msgs[j], TRUE, "w", GUI)
    }

    Insert.Messages.Out(lang.msg[['2']], TRUE, "s", GUI)

    return(0)
}
