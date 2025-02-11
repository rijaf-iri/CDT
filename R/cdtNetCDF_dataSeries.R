
ncInfo.no.date.range <- function(ncdf, intstep){
    regx <- switch(intstep,
                   "minute" = c("([0-9]{4})", rep("([0-9]{2})", 4)),
                   "hourly" = c("([0-9]{4})", rep("([0-9]{2})", 3)),
                   "daily" = c("([0-9]{4})", rep("([0-9]{2})", 2)),
                   "pentad" = c("([0-9]{4})", "([0-9]{2})", "([0-9]{1})"),
                   "dekadal" = c("([0-9]{4})", "([0-9]{2})", "([0-9]{1})"),
                   "monthly" = c("([0-9]{4})", "([0-9]{1,2})")
                )

    pattern <- ncdf$format
    for(j in seq_along(regx)) pattern <- sub("%s", regx[j], pattern)
    ncfiles <- list.files(ncdf$dir, pattern)
    if(length(ncfiles) == 0) return(NULL)

    daty <- stringr::str_match(ncfiles, pattern)
    daty <- daty[, -1, drop = FALSE]
    daty <- apply(daty, 1, function(x) paste0(x, collapse = ""))

    nc.path <- file.path(ncdf$dir, ncfiles)
    nc.exist <- file.exists(nc.path)
    if(!any(nc.exist)) return(NULL)

    ret <- list(dates = daty, ncfiles = nc.path, exist = nc.exist)
    return(ret)
}

##############################################

ncInfo.dates.table <- function(ncdf, dates.table){
    dates <- dates.table[, -ncol(dates.table), drop = FALSE]
    ldates <- split(dates, col(dates))
    dates <- do.call(paste0, ldates)

    ncfiles <- do.call(sprintf, c(list(fmt = ncdf$format), ldates))

    nc.path <- file.path(ncdf$dir, ncfiles)
    nc.exist <- file.exists(nc.path)
    if(!any(nc.exist)) return(NULL)

    list(dates = dates, ncfiles = nc.path, exist = nc.exist)
}

ncInfo.with.date.range <- function(ncdf, date.range, tstep, minhour = NA){
    dates <- table.format.date.time(tstep, date.range, minhour)
    ncInfo.dates.table(ncdf, dates)
}

ncInfo.from.date.vector <- function(ncdf, dates, tstep){
    dates <- table.format.date.time1(tstep, dates)
    ncInfo.dates.table(ncdf, dates)
}

get.ncInfo.params <-function(netcdf.data, date.range, tstep, minhour = NA){
    ncInfo <- ncInfo.with.date.range(netcdf.data, date.range, tstep, minhour)
    if(is.null(ncInfo)) return(NULL)

    varid <- netcdf.data$varid
    nc <- ncdf4::nc_open(ncInfo$ncfiles[ncInfo$exist][1])
    lon <- nc$var[[varid]]$dim[[netcdf.data$ilon]]$vals
    lat <- nc$var[[varid]]$dim[[netcdf.data$ilat]]$vals
    varinfo <- nc$var[[varid]][c('name', 'prec', 'units', 'longname', 'missval')]
    ncdf4::nc_close(nc)

    xo <- order(lon)
    lon <- lon[xo]
    yo <- order(lat)
    lat <- lat[yo]
    ncInfo$ncinfo <- list(varid = varid, lon = lon, lat = lat,
                          ilon = netcdf.data$ilon, ilat = netcdf.data$ilat,
                          xo = xo, yo = yo, varinfo = varinfo)
    return(ncInfo)
}

get.ncData.value <- function(ncdf_data){
    ncData <- NULL
    nc <- ncdf4::nc_open(ncdf_data$file)
    ncData$lon <- nc$var[[ncdf_data$varid]]$dim[[ncdf_data$ilon]]$vals
    ncData$lat <- nc$var[[ncdf_data$varid]]$dim[[ncdf_data$ilat]]$vals
    ncData$z <- ncdf4::ncvar_get(nc, ncdf_data$varid)
    ncdf4::nc_close(nc)

    xo <- order(ncData$lon)
    ncData$lon <- ncData$lon[xo]
    yo <- order(ncData$lat)
    ncData$lat <- ncData$lat[yo]
    ncData$z <- if(ncdf_data$ilon < ncdf_data$ilat) ncData$z[xo, yo] else t(ncData$z)[xo, yo]

    return(ncData)
}

##############################################

ncOutput.Files <- function(output, date.range, tstep, minhour = NA){
    dates <- table.format.date.time(tstep, date.range, minhour)
    dates <- dates[, -ncol(dates), drop = FALSE]
    dates <- split(dates, col(dates))
    ncfiles <- do.call(sprintf, c(list(fmt = output$format), dates))

    return(file.path(output$dir, ncfiles))
}

##############################################

ncFilesInfoSeq <- function(ncDir, ncFileFormat, error.msg){
    nc.files <- list.files(ncDir, sub("%S", ".+", ncFileFormat))
    if(length(nc.files) == 0){
        Insert.Messages.Out(error.msg, format = TRUE)
        return(NULL)
    }
    frmt <- strsplit(ncFileFormat, "%S")[[1]]

    seq_dat <- gsub(frmt[1], "", gsub(frmt[2], "", nc.files))
    nb_only <- grepl("^[0-9]+$", seq_dat)
    seq_order <- seq_along(seq_dat)
    if(all(nb_only)){
        nmax <- max(nchar(seq_dat))
        seq_dat <- stringr::str_pad(seq_dat, nmax, pad = "0")
        seq_order <- order(seq_dat)
    }else{
        nb_mixed <- gregexpr("[[:digit:]]+", seq_dat)
        ch_mixed <- gregexpr("[[:alpha:]]+", seq_dat)
        nb_cont <- sapply(nb_mixed, function(x) x[1] > 0)
        ch_cont <- sapply(ch_mixed, function(x) x[1] > 0)
        if(all(nb_cont) & !all(ch_cont)){
            nb_mixed <- regmatches(seq_dat, nb_mixed)
            nb_cont <- diff(range(sapply(nb_mixed, length)))
            if(nb_cont == 0){
                nb_mixed <- do.call(rbind, nb_mixed)
                nb_mixed <- apply(nb_mixed, 2, as.numeric)
                seq_order <- sort.filename.data(nb_mixed)
            }
        }
        if(!all(nb_cont) & all(ch_cont)){
            ch_mixed <- regmatches(seq_dat, ch_mixed)
            ch_cont <- diff(range(sapply(ch_mixed, length)))
            if(ch_cont == 0){
                ch_mixed <- do.call(rbind, ch_mixed)
                seq_order <- sort.filename.data(ch_mixed)
            }
        }
        if(all(nb_cont) & all(ch_cont)){
            nb_mixed <- regmatches(seq_dat, nb_mixed)
            nb_cont <- diff(range(sapply(nb_mixed, length)))
            ch_mixed <- regmatches(seq_dat, ch_mixed)
            ch_cont <- diff(range(sapply(ch_mixed, length)))
            if(nb_cont == 0 & ch_cont == 0){
                nb_mixed <- do.call(rbind, nb_mixed)
                nb_mixed <- apply(nb_mixed, 2, as.numeric)
                ch_mixed <- do.call(rbind, ch_mixed)
                seq_order <- sort.filename.data(data.frame(ch_mixed, nb_mixed))
            }
            if(nb_cont == 0 & ch_cont != 0){
                nb_mixed <- do.call(rbind, nb_mixed)
                nb_mixed <- apply(nb_mixed, 2, as.numeric)
                seq_order <- sort.filename.data(nb_mixed)
            }
            if(nb_cont != 0 & ch_cont == 0){
                ch_mixed <- do.call(rbind, ch_mixed)
                seq_order <- sort.filename.data(ch_mixed)
            }
        }
    }

    nc.files <- nc.files[seq_order]
    return(nc.files)
}

##############################################

### old
## List of available NetCDF files
ncFilesInfo <- function(Tstep, start.date, end.date, months,
                        ncDir, ncFileFormat, error.msg)
{
    if(Tstep == 'daily'){
        dates <- format(seq(start.date, end.date, 'day'), '%Y%m%d')
        ncDataFiles <- file.path(ncDir, sprintf(ncFileFormat, substr(dates, 1, 4),
                                        substr(dates, 5, 6), substr(dates, 7, 8)))
    }
    if(Tstep == 'pentad'){
        dates <- seq(start.date,  end.date, 'day')
        dates <- paste0(format(dates[which(as.numeric(format(dates, '%d')) <= 6)], '%Y%m'),
                    as.numeric(format(dates[which(as.numeric(format(dates, '%d')) <= 6)], '%d')))
        ncDataFiles <- file.path(ncDir, sprintf(ncFileFormat, substr(dates, 1, 4),
                                        substr(dates, 5, 6), substr(dates, 7, 7)))
    }
    if(Tstep == 'dekadal'){
        dates <- seq(start.date,  end.date, 'day')
        dates <- paste0(format(dates[which(as.numeric(format(dates, '%d')) <= 3)], '%Y%m'),
                    as.numeric(format(dates[which(as.numeric(format(dates, '%d')) <= 3)], '%d')))
        ncDataFiles <- file.path(ncDir, sprintf(ncFileFormat, substr(dates, 1, 4),
                                        substr(dates, 5, 6), substr(dates, 7, 7)))
    }
    if(Tstep == 'monthly'){
        dates <- format(seq(start.date, end.date, 'month'), '%Y%m')
        ncDataFiles <- file.path(ncDir, sprintf(ncFileFormat, substr(dates, 1, 4),
                                                substr(dates, 5, 6)))
    }
    months.dates <- as.numeric(substr(dates, 5, 6))
    imo <- months.dates %in% months
    dates <- dates[imo]
    ncDataFiles <- ncDataFiles[imo]

    existFl <- unlist(lapply(ncDataFiles, file.exists))
    if(!any(existFl)){
        Insert.Messages.Out(error.msg, format = TRUE)
        return(NULL)
    }
    return(list(dates = dates, nc.files = ncDataFiles, exist = existFl))
}

##############################################

readNetCDFData2Points <- function(ncInfo, stnCoords, GUI = TRUE){
    ncCoords <- ncInfo$ncinfo[c('lon', 'lat')]
    ijx <- grid2pointINDEX(stnCoords, ncCoords)

    ncInfo <- ncInfo
    checkerFile <- file.path(dirname(ncInfo$ncfiles[1]), '.checker')
    if(file.exists(checkerFile)) unlink(checkerFile)

    parsL <- doparallel.cond(length(which(ncInfo$exist)) >= 50)
    ncdata <- cdt.foreach(seq_along(ncInfo$ncfiles), parsL, GUI,
                          progress = TRUE, FUN = function(jj)
    {
        if(file.exists(checkerFile)) return(list(data = NULL, msg = NULL))

        xvar <- NA
        if(ncInfo$exist[jj]){
            nc <- try(ncdf4::nc_open(ncInfo$ncfiles[jj]), silent = TRUE)
            if(inherits(nc, "try-error")){
                write('', checkerFile)
                msg <- paste(as.character(nc), '\n', 'Unable to open', ncInfo$ncfiles[jj])
                return(list(data = NULL, msg = msg))
            }
            xvar <- ncdf4::ncvar_get(nc, varid = ncInfo$ncinfo$varid)
            ncdf4::nc_close(nc)
            if(ncInfo$ncinfo$nx != nrow(xvar) |
               ncInfo$ncinfo$ny != ncol(xvar)){
                write('', checkerFile)
                msg <- paste('Grids do not match', ncInfo$ncfiles[jj])
                return(list(data = NULL, msg = msg))
            }
            xvar <- transposeNCDFData(xvar, ncInfo$ncinfo)
            xvar <- xvar[ijx]
        }

        return(list(data = xvar, msg = NULL))
    })

    if(file.exists(checkerFile)) unlink(checkerFile)

    msgs <- lapply(ncdata, '[[', 'msg')
    inull <- !sapply(msgs, is.null)
    if(any(inull)){
        msgs <- unlist(msgs[inull])
        for(j in seq_along(msgs)) Insert.Messages.Out(msgs[j], TRUE, "e", GUI)
        stop('Extracting netCDF data at station locations failed.')
    }

    ncdata <- lapply(ncdata, '[[', 'data')
    do.call(rbind, ncdata)
}

readNetCDFData2Points_wind <- function(ncInfo, stnCoords, oneNetCDF, GUI = TRUE){
    ncCoords <- ncInfo$ncinfo[c('lon', 'lat')]
    ijx <- grid2pointINDEX(stnCoords, ncCoords)

    ncInfo <- ncInfo
    oneNetCDF <- oneNetCDF
    if(oneNetCDF){
        checkerFile <- file.path(dirname(ncInfo$UV$ncfiles[1]), '.checker')
        parsL <- doparallel.cond(length(which(ncInfo$UV$exist)) >= 50)
        seq_ncfiles <- seq_along(ncInfo$UV$ncfiles)
    }else{
        checkerFile <- file.path(dirname(ncInfo$V$ncfiles[1]), '.checker')
        parsL <- doparallel.cond(length(which(ncInfo$V$exist)) >= 50)
        seq_ncfiles <- seq_along(ncInfo$V$ncfiles)
    }
    if(file.exists(checkerFile)) unlink(checkerFile)

    ncdata <- cdt.foreach(seq_ncfiles, parsL, GUI,
                          progress = TRUE, FUN = function(jj)
    {
        if(file.exists(checkerFile)) return(list(data = NULL, msg = NULL))

        u_xvar <- NA
        v_xvar <- NA
        if(oneNetCDF){
            if(ncInfo$UV$exist[jj]){
                nc <- try(ncdf4::nc_open(ncInfo$UV$ncfiles[jj]), silent = TRUE)
                if(inherits(nc, "try-error")){
                    write('', checkerFile)
                    msg <- paste(as.character(nc), '\n', 'Unable to open', ncInfo$UV$ncfiles[jj])
                    return(list(data = NULL, msg = msg))
                }
                u_xvar <- ncdf4::ncvar_get(nc, varid = ncInfo$ncinfo$varidU)
                v_xvar <- ncdf4::ncvar_get(nc, varid = ncInfo$ncinfo$varidV)
                ncdf4::nc_close(nc)
                if(ncInfo$ncinfo$UV$nx != nrow(u_xvar) |
                   ncInfo$ncinfo$UV$ny != ncol(u_xvar)){
                    write('', checkerFile)
                    msg <- paste('Grids do not match', ncInfo$UV$ncfiles[jj])
                    return(list(data = NULL, msg = msg))
                }
                u_xvar <- transposeNCDFData(u_xvar, ncInfo$ncinfo$UV)
                u_xvar <- u_xvar[ijx]
                v_xvar <- transposeNCDFData(v_xvar, ncInfo$ncinfo$UV)
                v_xvar <- v_xvar[ijx]
            }
        }else{
            tmp <- lapply(c('U', 'V'), function(uv){
                out <- list(data = NA, msg = NULL)
                if(ncInfo[[uv]]$exist[jj]){
                    nc <- try(ncdf4::nc_open(ncInfo[[uv]]$ncfiles[jj]), silent = TRUE)
                    if(inherits(nc, "try-error")){
                        write('', checkerFile)
                        msg <- paste(as.character(nc), '\n', 'Unable to open', ncInfo[[uv]]$ncfiles[jj])
                        return(list(data = NULL, msg = msg))
                    }
                    varid <- paste0('varid', uv)
                    xvar <- ncdf4::ncvar_get(nc, varid = ncInfo$ncinfo[[varid]])
                    ncdf4::nc_close(nc)
                    if(ncInfo$ncinfo[[uv]]$nx != nrow(xvar) |
                       ncInfo$ncinfo[[uv]]$ny != ncol(xvar)){
                        write('', checkerFile)
                        msg <- paste('Grids do not match', ncInfo[[uv]]$ncfiles[jj])
                        return(list(data = NULL, msg = msg))
                    }
                    xvar <- transposeNCDFData(xvar, ncInfo$ncinfo[[uv]])
                    xvar <- xvar[ijx]
                    out <- list(data = xvar, msg = NULL)
                }
                return(out)
            })

            msgs <- lapply(tmp, '[[', 'msg')
            inull <- !sapply(msgs, is.null)
            if(any(inull)){
                msgs <- unlist(msgs[inull])
                return(list(data = NULL, msg = msgs))
            }
            tmp <- lapply(tmp, '[[', 'data')
            u_xvar <- tmp[[1]]
            v_xvar <- tmp[[2]]
        }

        return(list(data = list(U = u_xvar, V = v_xvar), msg = NULL))
    })

    if(file.exists(checkerFile)) unlink(checkerFile)

    msgs <- lapply(ncdata, '[[', 'msg')
    inull <- !sapply(msgs, is.null)
    if(any(inull)){
        msgs <- unlist(msgs[inull])
        for(j in seq_along(msgs)) Insert.Messages.Out(msgs[j], TRUE, "e", GUI)
        stop('Extracting netCDF data at station locations failed.')
    }

    ncdata <- lapply(ncdata, '[[', 'data')
    u_grd <- lapply(ncdata, '[[', 'U')
    u_grd <- do.call(rbind, u_grd)
    v_grd <- lapply(ncdata, '[[', 'V')
    v_grd <- do.call(rbind, v_grd)
    list(U = u_grd, V = v_grd)
}

readNetCDFData2AggrBox <- function(ncInfo, boxregion, spmethod = 'bilinear', GUI = TRUE){
    rlon <- range(ncInfo$ncinfo$lon)
    dlon <- floor(diff(rlon)/boxregion$blon)
    rlat <- range(ncInfo$ncinfo$lat)
    dlat <- floor(diff(rlat)/boxregion$blat)
    xlon <- seq(rlon[1], rlon[2], length.out = dlon)
    xlat <- seq(rlat[1], rlat[2], length.out = dlat)

    boxCoords <- list(lon = xlon, lat = xlat)
    ncCoords <- ncInfo$ncinfo[c('lon', 'lat')]
    spxybox <- defSpatialPixels(boxCoords)
    spxybox <- methods::as(spxybox, "SpatialPolygons")

    if(spmethod == 'weightedAverage'){
        spxygrd <- defSpatialPixels(ncCoords)
        spxygrd$z <- seq_along(spxygrd)
        spxygrd <- raster::raster(spxygrd)
        ij2xtr <- raster::extract(spxygrd, spxybox, weights = TRUE,
                                  normalizeWeights = TRUE, cellnumbers = TRUE)
    }

    ####
    ncInfo <- ncInfo
    spmethod <- spmethod
    checkerFile <- file.path(dirname(ncInfo$ncfiles[1]), '.checker')
    if(file.exists(checkerFile)) unlink(checkerFile)

    parsL <- doparallel.cond(length(which(ncInfo$exist)) >= 50)
    ncdata <- cdt.foreach(seq_along(ncInfo$ncfiles), parsL, GUI,
                          progress = TRUE, FUN = function(jj)
    {
        if(file.exists(checkerFile)) return(list(data = NULL, msg = NULL))

        xvar <- NA
        if(ncInfo$exist[jj]){
            nc <- try(ncdf4::nc_open(ncInfo$ncfiles[jj]), silent = TRUE)
            if(inherits(nc, "try-error")){
                write('', checkerFile)
                msg <- paste(as.character(nc), '\n', 'Unable to open', ncInfo$ncfiles[jj])
                return(list(data = NULL, msg = msg))
            }
            xvar <- ncdf4::ncvar_get(nc, varid = ncInfo$ncinfo$varid)
            ncdf4::nc_close(nc)
            if(ncInfo$ncinfo$nx != nrow(xvar) |
               ncInfo$ncinfo$ny != ncol(xvar)){
                write('', checkerFile)
                msg <- paste('Grids do not match', ncInfo$ncfiles[jj])
                return(list(data = NULL, msg = msg))
            }
            xvar <- transposeNCDFData(xvar, ncInfo$ncinfo)

            if(spmethod == 'bilinear'){
                tmp <- c(ncCoords, list(z = xvar))
                xvar <- cdt.interp.surface.grid(tmp, boxCoords)
                xvar <- c(xvar$z)
            }else{
                xvar <- sapply(seq_along(ij2xtr), function(ii){
                    ix <- ij2xtr[[ii]]
                    mat <- xvar[ix[, "value"]]
                    if(nrow(ix) > 1){
                        mat <- mat * ix[, "weight"]
                        mat <- mat[!is.na(mat)]
                        mat <- if(length(mat) > 0) sum(mat) else NA
                    }
                    return(mat)
                })
            }
        }

        return(list(data = xvar, msg = NULL))
    })

    if(file.exists(checkerFile)) unlink(checkerFile)

    msgs <- lapply(ncdata, '[[', 'msg')
    inull <- !sapply(msgs, is.null)
    if(any(inull)){
        msgs <- unlist(msgs[inull])
        for(j in seq_along(msgs)) Insert.Messages.Out(msgs[j], TRUE, "e", GUI)
        stop('Aggregating netCDF data into box failed.')
    }

    ncdata <- lapply(ncdata, '[[', 'data')
    ncdata <- do.call(rbind, ncdata)

    list(lon = xlon, lat = xlat, spbox = spxybox, data = ncdata)
}

readNetCDFData2AggrBox_wind <- function(ncInfo, boxregion, spmethod = 'bilinear', oneNetCDF = TRUE, GUI = TRUE){
    rlon <- range(ncInfo$ncinfo$lon)
    rlat <- range(ncInfo$ncinfo$lat)
    dlon <- floor(diff(rlon)/boxregion$blon)
    dlat <- floor(diff(rlat)/boxregion$blat)
    xlon <- seq(rlon[1], rlon[2], length.out = dlon)
    xlat <- seq(rlat[1], rlat[2], length.out = dlat)

    boxCoords <- list(lon = xlon, lat = xlat)
    ncCoords <- ncInfo$ncinfo[c('lon', 'lat')]
    spxybox <- defSpatialPixels(boxCoords)
    spxybox <- methods::as(spxybox, "SpatialPolygons")

    if(spmethod == 'weightedAverage'){
        spxygrd <- defSpatialPixels(ncCoords)
        spxygrd$z <- seq_along(spxygrd)
        spxygrd <- raster::raster(spxygrd)
        ij2xtr <- raster::extract(spxygrd, spxybox, weights = TRUE,
                                  normalizeWeights = TRUE, cellnumbers = TRUE)
    }

    ####
    ncInfo <- ncInfo
    spmethod <- spmethod
    oneNetCDF <- oneNetCDF

    if(oneNetCDF){
        checkerFile <- file.path(dirname(ncInfo$UV$ncfiles[1]), '.checker')
        parsL <- doparallel.cond(length(which(ncInfo$UV$exist)) >= 50)
        seq_ncfiles <- seq_along(ncInfo$UV$ncfiles)
    }else{
        checkerFile <- file.path(dirname(ncInfo$V$ncfiles[1]), '.checker')
        parsL <- doparallel.cond(length(which(ncInfo$V$exist)) >= 50)
        seq_ncfiles <- seq_along(ncInfo$V$ncfiles)
    }
    if(file.exists(checkerFile)) unlink(checkerFile)

    ncdata <- cdt.foreach(seq_ncfiles, parsL, GUI,
                          progress = TRUE, FUN = function(jj)
    {
        if(file.exists(checkerFile)) return(list(data = NULL, msg = NULL))

        u_xvar <- NA
        v_xvar <- NA
        if(oneNetCDF){
            if(ncInfo$UV$exist[jj]){
                nc <- try(ncdf4::nc_open(ncInfo$UV$ncfiles[jj]), silent = TRUE)
                if(inherits(nc, "try-error")){
                    write('', checkerFile)
                    msg <- paste(as.character(nc), '\n', 'Unable to open', ncInfo$UV$ncfiles[jj])
                    return(list(data = NULL, msg = msg))
                }
                u_xvar <- ncdf4::ncvar_get(nc, varid = ncInfo$ncinfo$varidU)
                v_xvar <- ncdf4::ncvar_get(nc, varid = ncInfo$ncinfo$varidV)
                ncdf4::nc_close(nc)
                if(ncInfo$ncinfo$UV$nx != nrow(u_xvar) |
                   ncInfo$ncinfo$UV$ny != ncol(u_xvar)){
                    write('', checkerFile)
                    msg <- paste('Grids do not match', ncInfo$UV$ncfiles[jj])
                    return(list(data = NULL, msg = msg))
                }
                u_xvar <- transposeNCDFData(u_xvar, ncInfo$ncinfo$UV)
                v_xvar <- transposeNCDFData(v_xvar, ncInfo$ncinfo$UV)
                wind_xvar <- list(u_xvar, v_xvar)

                if(spmethod == 'bilinear'){
                    tmp <- lapply(wind_xvar, function(uv_xvar){
                        tmp <- c(ncCoords, list(z = uv_xvar))
                        xvar <- cdt.interp.surface.grid(tmp, boxCoords)
                        c(xvar$z)
                    })
                }else{
                    tmp <- lapply(wind_xvar, function(uv_xvar){
                        xvar <- sapply(seq_along(ij2xtr), function(ii){
                            ix <- ij2xtr[[ii]]
                            mat <- uv_xvar[ix[, "value"]]
                            if(nrow(ix) > 1){
                                mat <- mat * ix[, "weight"]
                                mat <- mat[!is.na(mat)]
                                mat <- if(length(mat) > 0) sum(mat) else NA
                            }
                            return(mat)
                        })
                        return(xvar)
                    })
                }

                u_xvar <- tmp[[1]]
                v_xvar <- tmp[[2]]
            }
        }else{
            tmp <- lapply(c('U', 'V'), function(uv){
                out <- list(data = NA, msg = NULL)
                if(ncInfo[[uv]]$exist[jj]){
                    nc <- try(ncdf4::nc_open(ncInfo[[uv]]$ncfiles[jj]), silent = TRUE)
                    if(inherits(nc, "try-error")){
                        write('', checkerFile)
                        msg <- paste(as.character(nc), '\n', 'Unable to open', ncInfo[[uv]]$ncfiles[jj])
                        return(list(data = NULL, msg = msg))
                    }
                    varid <- paste0('varid', uv)
                    xvar <- ncdf4::ncvar_get(nc, varid = ncInfo$ncinfo[[varid]])
                    ncdf4::nc_close(nc)
                    if(ncInfo$ncinfo[[uv]]$nx != nrow(xvar) |
                       ncInfo$ncinfo[[uv]]$ny != ncol(xvar)){
                        write('', checkerFile)
                        msg <- paste('Grids do not match', ncInfo[[uv]]$ncfiles[jj])
                        return(list(data = NULL, msg = msg))
                    }
                    xvar <- transposeNCDFData(xvar, ncInfo$ncinfo[[uv]])

                    if(spmethod == 'bilinear'){
                        tmp <- c(ncCoords, list(z = xvar))
                        xvar <- cdt.interp.surface.grid(tmp, boxCoords)
                        xvar <- c(xvar$z)
                    }else{
                        xvar <- sapply(seq_along(ij2xtr), function(ii){
                            ix <- ij2xtr[[ii]]
                            mat <- xvar[ix[, "value"]]
                            if(nrow(ix) > 1){
                                mat <- mat * ix[, "weight"]
                                mat <- mat[!is.na(mat)]
                                mat <- if(length(mat) > 0) sum(mat) else NA
                            }
                            return(mat)
                        })
                    }
                    out <- list(data = xvar, msg = NULL)
                }
                return(out)
            })

            msgs <- lapply(tmp, '[[', 'msg')
            inull <- !sapply(msgs, is.null)
            if(any(inull)){
                msgs <- unlist(msgs[inull])
                return(list(data = NULL, msg = msgs))
            }
            tmp <- lapply(tmp, '[[', 'data')
            u_xvar <- tmp[[1]]
            v_xvar <- tmp[[2]]
        }

        return(list(data = list(U = u_xvar, V = v_xvar), msg = NULL))
    })

    if(file.exists(checkerFile)) unlink(checkerFile)

    msgs <- lapply(ncdata, '[[', 'msg')
    inull <- !sapply(msgs, is.null)
    if(any(inull)){
        msgs <- unlist(msgs[inull])
        for(j in seq_along(msgs)) Insert.Messages.Out(msgs[j], TRUE, "e", GUI)
        stop('Aggregating netCDF data into box failed.')
    }

    ncdata <- lapply(ncdata, '[[', 'data')
    u_grd <- lapply(ncdata, '[[', 'U')
    u_grd <- do.call(rbind, u_grd)
    v_grd <- lapply(ncdata, '[[', 'V')
    v_grd <- do.call(rbind, v_grd)
    ncdata <- list(U = u_grd, V = v_grd)

    list(lon = xlon, lat = xlat, spbox = spxybox, data = ncdata)
}

readNetCDFData2Directory <- function(ncInfo, datadir, GUI = TRUE){
    ncCoords <- ncInfo$ncinfo[c('lon', 'lat')]
    newCoords <- ncInfo$ncgrid[c('lon', 'lat')]
    crdGRD <- expand.grid(newCoords)
    seqGRD <- seq(nrow(crdGRD))
    chunks <- split(seqGRD, ceiling(seqGRD / 500))

    ####
    ncInfo <- ncInfo
    datadir <- datadir
    checkerFile <- file.path(dirname(ncInfo$ncfiles[1]), '.checker')
    if(file.exists(checkerFile)) unlink(checkerFile)

    parsL <- doparallel.cond(length(which(ncInfo$exist)) >= 50)
    msgs <- cdt.foreach(seq_along(ncInfo$ncfiles), parsL, GUI,
                          progress = TRUE, FUN = function(jj)
    {
        if(file.exists(checkerFile)) return(NULL)

        if(ncInfo$exist[jj]){
            nc <- try(ncdf4::nc_open(ncInfo$ncfiles[jj]), silent = TRUE)
            if(inherits(nc, "try-error")){
                write('', checkerFile)
                msg <- paste(as.character(nc), '\n', 'Unable to open', ncInfo$ncfiles[jj])
                return(msg)
            }
            xvar <- ncdf4::ncvar_get(nc, varid = ncInfo$ncinfo$varid)
            ncdf4::nc_close(nc)
            if(ncInfo$ncinfo$nx != nrow(xvar) |
               ncInfo$ncinfo$ny != ncol(xvar)){
                write('', checkerFile)
                msg <- paste('Grids do not match', ncInfo$ncfiles[jj])
                return(msg)
            }
            xvar <- transposeNCDFData(xvar, ncInfo$ncinfo)

            if(ncInfo$ncgrid$regrid){
                tmp <- c(ncCoords, list(z = xvar))
                xvar <- cdt.interp.surface.grid(tmp, newCoords)
                xvar <- xvar$z
            }

            lchunks <- sample(seq_along(chunks), length(chunks))
            for(l in lchunks){
                tmp <- round(xvar[chunks[[l]]], 2)
                tmp <- c(ncInfo$dates[jj], tmp)
                tmp <- paste0(tmp, collapse = ',')
                tmp <- paste0(tmp, '\n')
                datafile <- file.path(datadir, paste0('chunk_', l))
                cat(tmp, file = datafile, sep = '\n', append = TRUE)
            }
        }

        return(NULL)
    })

    if(file.exists(checkerFile)) unlink(checkerFile)

    inull <- !sapply(msgs, is.null)
    if(any(inull)){
        msgs <- unlist(msgs[inull])
        for(j in seq_along(msgs)) Insert.Messages.Out(msgs[j], TRUE, "e", GUI)
        stop('Formatting netCDF data into chunks failed.')
    }

    Insert.Messages.Out("Compress chunks data", TRUE, "i", GUI)

    parsL <- doparallel.cond(length(chunks) >= 50)
    ret <- cdt.foreach(seq_along(chunks), parsL, GUI,
                       progress = TRUE, FUN = function(j)
    {
        datafile <- file.path(datadir, paste0('chunk_', j))
        tmp <- readLines(datafile)
        unlink(datafile)
        tmp <- trimws(tmp)
        tmp <- tmp[tmp != ""]
        con <- gzfile(datafile, open = "wb", compression = 9)
        cat(tmp, file = con, sep = '\n')
        close(con)
    })

    Insert.Messages.Out("Compressing chunks data done", TRUE, "s", GUI)

    out <- list(lon = ncInfo$ncgrid$lon, lat = ncInfo$ncgrid$lat, chunks = chunks)

    indexfile <- file.path(datadir, 'index.rds')
    saveRDS(out, file = indexfile)

    return(out)
}

readNetCDFData2Directory_wind <- function(ncInfo, datadir, oneNetCDF = TRUE, GUI = TRUE){
    ncCoords <- ncInfo$ncinfo[c('lon', 'lat')]
    newCoords <- ncInfo$ncgrid[c('lon', 'lat')]
    crdGRD <- expand.grid(newCoords)
    seqGRD <- seq(nrow(crdGRD))
    chunks <- split(seqGRD, ceiling(seqGRD / 500))

    ####
    ncInfo <- ncInfo
    oneNetCDF <- oneNetCDF
    datadir <- datadir

    datadirU <- file.path(datadir, 'U')
    dir.create(datadirU, showWarnings = FALSE, recursive = TRUE)
    datadirV <- file.path(datadir, 'V')
    dir.create(datadirV, showWarnings = FALSE, recursive = TRUE)
    datadirUV <- c(datadirU, datadirV)

    if(oneNetCDF){
        checkerFile <- file.path(dirname(ncInfo$UV$ncfiles[1]), '.checker')
        parsL <- doparallel.cond(length(which(ncInfo$UV$exist)) >= 50)
        seq_ncfiles <- seq_along(ncInfo$UV$ncfiles)
    }else{
        checkerFile <- file.path(dirname(ncInfo$V$ncfiles[1]), '.checker')
        parsL <- doparallel.cond(length(which(ncInfo$V$exist)) >= 50)
        seq_ncfiles <- seq_along(ncInfo$V$ncfiles)
    }
    if(file.exists(checkerFile)) unlink(checkerFile)

    msgs <- cdt.foreach(seq_ncfiles, parsL, GUI,
                          progress = TRUE, FUN = function(jj)
    {
        if(file.exists(checkerFile)) return(NULL)

        if(oneNetCDF){
            if(ncInfo$UV$exist[jj]){
                nc <- try(ncdf4::nc_open(ncInfo$UV$ncfiles[jj]), silent = TRUE)
                if(inherits(nc, "try-error")){
                    write('', checkerFile)
                    msg <- paste(as.character(nc), '\n', 'Unable to open', ncInfo$UV$ncfiles[jj])
                    return(msg)
                }
                u_xvar <- ncdf4::ncvar_get(nc, varid = ncInfo$ncinfo$varidU)
                v_xvar <- ncdf4::ncvar_get(nc, varid = ncInfo$ncinfo$varidV)
                ncdf4::nc_close(nc)
                if(ncInfo$ncinfo$UV$nx != nrow(u_xvar) |
                   ncInfo$ncinfo$UV$ny != ncol(u_xvar)){
                    write('', checkerFile)
                    msg <- paste('Grids do not match', ncInfo$UV$ncfiles[jj])
                    return(msg)
                }
                u_xvar <- transposeNCDFData(u_xvar, ncInfo$ncinfo$UV)
                v_xvar <- transposeNCDFData(v_xvar, ncInfo$ncinfo$UV)
                wind_xvar <- list(u_xvar, v_xvar)

                if(ncInfo$ncgrid$regrid){
                    wind_xvar <- lapply(wind_xvar, function(uv_xvar){
                        tmp <- c(ncCoords, list(z = uv_xvar))
                        xvar <- cdt.interp.surface.grid(tmp, newCoords)
                        xvar$z
                    })
                }

                lchunks <- sample(seq_along(chunks), length(chunks))
                for(l in lchunks){
                    for(k in 1:2){
                        tmp <- round(wind_xvar[[k]][chunks[[l]]], 2)
                        tmp <- c(ncInfo$UV$dates[jj], tmp)
                        tmp <- paste0(tmp, collapse = ',')
                        tmp <- paste0(tmp, '\n')
                        datafile <- file.path(datadirUV[k], paste0('chunk_', l))
                        cat(tmp, file = datafile, sep = '\n', append = TRUE)
                    }
                }
            }
        }else{
            msgs <- lapply(c('U', 'V'), function(uv){
                if(ncInfo[[uv]]$exist[jj]){
                    nc <- try(ncdf4::nc_open(ncInfo[[uv]]$ncfiles[jj]), silent = TRUE)
                    if(inherits(nc, "try-error")){
                        write('', checkerFile)
                        msg <- paste(as.character(nc), '\n', 'Unable to open', ncInfo[[uv]]$ncfiles[jj])
                        return(msg)
                    }

                    varid <- paste0('varid', uv)
                    xvar <- ncdf4::ncvar_get(nc, varid = ncInfo$ncinfo[[varid]])
                    ncdf4::nc_close(nc)
                    if(ncInfo$ncinfo[[uv]]$nx != nrow(xvar) |
                       ncInfo$ncinfo[[uv]]$ny != ncol(xvar)){
                        write('', checkerFile)
                        msg <- paste('Grids do not match', ncInfo[[uv]]$ncfiles[jj])
                        return(msg)
                    }
                    xvar <- transposeNCDFData(xvar, ncInfo$ncinfo[[uv]])

                    if(ncInfo$ncgrid$regrid){
                        tmp <- c(ncCoords, list(z = xvar))
                        xvar <- cdt.interp.surface.grid(tmp, newCoords)
                        xvar <- xvar$z
                    }

                    lchunks <- sample(seq_along(chunks), length(chunks))
                    for(l in lchunks){
                        tmp <- round(xvar[chunks[[l]]], 2)
                        tmp <- c(ncInfo[[uv]]$dates[jj], tmp)
                        tmp <- paste0(tmp, collapse = ',')
                        tmp <- paste0(tmp, '\n')
                        datafile <- file.path(datadir, uv, paste0('chunk_', l))
                        cat(tmp, file = datafile, sep = '\n', append = TRUE)
                    }
                }
                return(NULL)
            })

            inull <- !sapply(msgs, is.null)
            if(any(inull)){
                return(unlist(msgs[inull]))
            }
        }

        return(NULL)
    })

    if(file.exists(checkerFile)) unlink(checkerFile)

    inull <- !sapply(msgs, is.null)
    if(any(inull)){
        msgs <- unlist(msgs[inull])
        for(j in seq_along(msgs)) Insert.Messages.Out(msgs[j], TRUE, "e", GUI)
        stop('Formatting netCDF data into chunks failed.')
    }

    Insert.Messages.Out("Compress chunks data", TRUE, "i", GUI)

    parsL <- doparallel.cond(length(chunks) >= 50)
    ret <- cdt.foreach(seq_along(chunks), parsL, GUI,
                       progress = TRUE, FUN = function(j)
    {
        for(uv in c('U', 'V')){
            datafile <- file.path(datadir, uv, paste0('chunk_', j))
            tmp <- readLines(datafile)
            unlink(datafile)
            tmp <- trimws(tmp)
            tmp <- tmp[tmp != ""]
            con <- gzfile(datafile, open = "wb", compression = 9)
            cat(tmp, file = con, sep = '\n')
            close(con)
        }
    })

    Insert.Messages.Out("Compressing chunks data done", TRUE, "s", GUI)

    out <- list(lon = ncInfo$ncgrid$lon, lat = ncInfo$ncgrid$lat, chunks = chunks)

    indexfile <- file.path(datadir, 'index.rds')
    saveRDS(out, file = indexfile)

    return(out)
}
