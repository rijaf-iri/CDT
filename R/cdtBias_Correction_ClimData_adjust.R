
applyBiasCorrectionClimData <- function(coefBias, ncInfo, outdir, params, variable, GUI = TRUE){
    xml.dlg <- file.path(.cdtDir$dirLocal, "languages", "cdtBias_Correction_ClimData_adjust.xml")
    lang.msg <- cdtLanguageParse(xml.dlg, .cdtData$Config$lang.iso)
    lang.msg <- lang.msg[['message']]

    Insert.Messages.Out(lang.msg[['1']], TRUE, "i", GUI)

    ############
    if(variable %in% c("ugrd", "vgrd")){
        missval <- -9999
    }else{
        missval <- -99
    }

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
    ncfiles <- ncInfo$ncfiles[ncInfo$exist]

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

