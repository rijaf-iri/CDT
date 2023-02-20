
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
    daty <- apply(daty[, -1], 1, function(x) paste0(x, collapse = ""))

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

get.ncInfo.params <-function(netcdf.data, date.range, tstep){
    ncInfo <- ncInfo.with.date.range(netcdf.data, date.range, tstep)
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
        seq_dat <- str_pad(seq_dat, nmax, pad = "0")
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

## used by fill dek temp, remplacer
## Read ncdf files, extract at a set points
read.NetCDF.Data2Points <- function(read.ncdf.parms, list.lonlat.pts){
    Insert.Messages.Out(read.ncdf.parms$msg$start, TRUE, "i")

    ncInfo <- do.call(ncFilesInfo, c(read.ncdf.parms$ncfiles, error.msg = read.ncdf.parms$errmsg))
    if(is.null(ncInfo)) return(NULL)

    ncinfo <- read.ncdf.parms$ncinfo
    lon <- ncinfo$lon
    lat <- ncinfo$lat
    nlon <- ncinfo$nx
    nlat <- ncinfo$ny

    ijx <- grid2pointINDEX(list.lonlat.pts, list(lon = lon, lat = lat))

    parsL <- doparallel.cond(length(which(ncInfo$exist)) >= 100)
    ncdata <- cdt.foreach(seq_along(ncInfo$nc.files), parsL = parsL, FUN = function(jj)
    {
        xvar <- NULL
        if(ncInfo$exist[jj]){
            nc <- try(ncdf4::nc_open(ncInfo$nc.files[jj]), silent = TRUE)
            if(inherits(nc, "try-error")) return(NULL)
            xvar <- ncdf4::ncvar_get(nc, varid = ncinfo$varid)
            ncdf4::nc_close(nc)
            if(nlon != nrow(xvar) | nlat != ncol(xvar)) return(NULL)
            xvar <- transposeNCDFData(xvar, ncinfo)
            xvar <- xvar[ijx]
        }
        xvar
    })

    ret <- list(dates = ncInfo$dates, data = ncdata, lon = lon, lat = lat,
                lon.pts = list.lonlat.pts$lon, lat.pts = list.lonlat.pts$lat)
    Insert.Messages.Out(read.ncdf.parms$msg$end, TRUE, "s")
    return(ret)
}

##############################################

readNetCDFData2Points <- function(ncInfo, lonlatpts, GUI = TRUE){
    ijx <- grid2pointINDEX(lonlatpts,
                           list(lon = ncInfo$ncinfo$lon,
                                lat = ncInfo$ncinfo$lat)
                           )
    
    # #%REMOVE
    transposeNCDFData <- transposeNCDFData
    # #%REMOVE

    ncInfo <- ncInfo
    parsL <- doparallel.cond(length(which(ncInfo$exist)) >= 180)
    ncdata <- cdt.foreach(seq_along(ncInfo$ncfiles), parsL, GUI,
                          progress = TRUE, FUN = function(jj)
    {
        xvar <- NA
        if(ncInfo$exist[jj]){
            nc <- try(ncdf4::nc_open(ncInfo$ncfiles[jj]), silent = TRUE)
            if(inherits(nc, "try-error")) return(NA)
            xvar <- ncdf4::ncvar_get(nc, varid = ncInfo$ncinfo$varid)
            ncdf4::nc_close(nc)
            if(ncInfo$ncinfo$nx != nrow(xvar) |
               ncInfo$ncinfo$ny != ncol(xvar)) return(NA)
            xvar <- transposeNCDFData(xvar, ncInfo$ncinfo)
            xvar <- xvar[ijx]
        }
        return(xvar)
    })

    do.call(rbind, ncdata)
}


readNetCDFData2AggrBox <- function(ncInfo, boxregion, GUI = TRUE){
    rlon <- range(ncInfo$ncinfo$lon)
    rlat <- range(ncInfo$ncinfo$lat)
    xlon <- seq(rlon[1], rlon[2], boxregion$blon)
    xlat <- seq(rlat[1], rlat[2], boxregion$blat)

    spxygrd <- defSpatialPixels(ncInfo$ncinfo[c('lon', 'lat')])
    spxygrd$z <- seq_along(spxygrd)
    spxygrd <- raster::raster(spxygrd)
    spxybox <- defSpatialPixels(list(lon = xlon, lat = xlat))
    spxybox <- as(spxybox, "SpatialPolygons")
    ij2xtr <- raster::extract(spxygrd, spxybox, weights = TRUE,
                              normalizeWeights = TRUE, cellnumbers = TRUE)

    # #%REMOVE
    transposeNCDFData <- transposeNCDFData
    # #%REMOVE

    ncInfo <- ncInfo
    parsL <- doparallel.cond(length(which(ncInfo$exist)) >= 180)
    ncdata <- cdt.foreach(seq_along(ncInfo$ncfiles), parsL, GUI,
                          progress = TRUE, FUN = function(jj)
    {
        xvar <- NA
        if(ncInfo$exist[jj]){
            nc <- try(ncdf4::nc_open(ncInfo$ncfiles[jj]), silent = TRUE)
            if(inherits(nc, "try-error")) return(NA)
            xvar <- ncdf4::ncvar_get(nc, varid = ncInfo$ncinfo$varid)
            ncdf4::nc_close(nc)
            if(ncInfo$ncinfo$nx != nrow(xvar) |
               ncInfo$ncinfo$ny != ncol(xvar)) return(NA)

            xvar <- transposeNCDFData(xvar, ncInfo$ncinfo)
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
        return(xvar)
    })

    ncdata <- do.call(rbind, ncdata)
    list(lon = xlon, lat = xlat, spbox = spxybox, data = ncdata)
}

readNetCDFData2PtsAggrBox <- function(ncInfo, lonlatpts, boxregion, GUI = TRUE){
    ijx <- grid2pointINDEX(lonlatpts,
                           list(lon = ncInfo$ncinfo$lon,
                                lat = ncInfo$ncinfo$lat)
                           )
    rlon <- range(ncInfo$ncinfo$lon)
    rlat <- range(ncInfo$ncinfo$lat)
    xlon <- seq(rlon[1], rlon[2], boxregion$blon)
    xlat <- seq(rlat[1], rlat[2], boxregion$blat)

    spxygrd <- defSpatialPixels(ncInfo$ncinfo[c('lon', 'lat')])
    spxygrd$z <- seq_along(spxygrd)
    spxygrd <- raster::raster(spxygrd)
    spxybox <- defSpatialPixels(list(lon = xlon, lat = xlat))
    spxybox <- as(spxybox, "SpatialPolygons")
    ij2xtr <- raster::extract(spxygrd, spxybox, weights = TRUE,
                              normalizeWeights = TRUE, cellnumbers = TRUE)

    # #%REMOVE
    transposeNCDFData <- transposeNCDFData
    # #%REMOVE

    ncInfo <- ncInfo
    parsL <- doparallel.cond(length(which(ncInfo$exist)) >= 180)
    ncdata <- cdt.foreach(seq_along(ncInfo$ncfiles), parsL, GUI,
                          progress = TRUE, FUN = function(jj)
    {
        xvar <- NA
        if(ncInfo$exist[jj]){
            nc <- try(ncdf4::nc_open(ncInfo$ncfiles[jj]), silent = TRUE)
            if(inherits(nc, "try-error")) return(NA)
            xvar <- ncdf4::ncvar_get(nc, varid = ncInfo$ncinfo$varid)
            ncdf4::nc_close(nc)
            if(ncInfo$ncinfo$nx != nrow(xvar) |
               ncInfo$ncinfo$ny != ncol(xvar)) return(NA)

            xvar <- transposeNCDFData(xvar, ncInfo$ncinfo)

            xvar1 <- xvar[ijx]
            xvar2 <- sapply(seq_along(ij2xtr), function(ii){
                ix <- ij2xtr[[ii]]
                mat <- xvar[ix[, "value"]]
                if(nrow(ix) > 1){
                    mat <- mat * ix[, "weight"]
                    mat <- mat[!is.na(mat)]
                    mat <- if(length(mat) > 0) sum(mat) else NA
                }
                return(mat)
            })

            xvar <- c(xvar1, xvar2)
        }
        return(xvar)
    })

    ncdata <- do.call(rbind, ncdata)
    xy <- expand.grid(xlon, xlat)
    lon <- c(lonlatpts$lon, xy[, 1])
    lat <- c(lonlatpts$lat, xy[, 2])
    index <- c(rep(1, length(lonlatpts$lon)), rep(2, length(xlon) * length(xlat)))

    list(lon = lon, lat = lat, index = index, data = ncdata)
}

