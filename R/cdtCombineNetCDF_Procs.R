
combine.netcdf_writeNC <- function(){
    GalParams <- .cdtData$GalParams
    Insert.Messages.Out(GalParams[['message']][['1']], TRUE, "i")

    rdate <- table.format.date.time(GalParams$tstep, GalParams$date.range, GalParams$minhour)
    rdate0 <- rdate
    if(GalParams$tstep == 'pentad'){
        rdate0[, 3] <- c(1, 6, 11, 16, 21, 26)[as.numeric(rdate0[, 3])]
    }
    if(GalParams$tstep == 'dekadal'){
        rdate0[, 3] <- c(1, 11, 21)[as.numeric(rdate0[, 3])]
    }

    dateOpts <- switch(GalParams$tstep,
                       'minute' = list(nc = 5, fun = as.POSIXct,
                                       format = "%Y-%m-%d-%H-%M",
                                       units = "seconds since 1970-01-01 00:00:00"),
                       'hourly' = list(nc = 4, fun = as.POSIXct,
                                       format = "%Y-%m-%d-%H", 
                                       units = "seconds since 1970-01-01 00:00:00"),
                       'daily' = list(nc = 3, fun = as.Date,
                                      format = "%Y-%m-%d",
                                      units = "days since 1970-01-01"),
                       'pentad' = list(nc = 3, fun = as.Date,
                                       format = "%Y-%m-%d",
                                       units = "days since 1970-01-01"),
                       'dekadal' = list(nc = 3, fun = as.Date,
                                        format = "%Y-%m-%d",
                                        units = "days since 1970-01-01"),
                       'monthly' = list(nc = 3, fun = as.Date,
                                        format = "%Y-%m-%d",
                                        units = "days since 1970-01-01")
                      )

    datetimes <- apply(rdate0[, 1:dateOpts$nc], 1, paste, collapse = "-")
    datetimes <- dateOpts$fun(datetimes, format = dateOpts$format)
    datetimes <- as.numeric(datetimes)

    dtimes <- rdate[, -ncol(rdate), drop = FALSE]
    dtimes <- split(dtimes, col(dtimes))
    ncfiles <- do.call(sprintf, c(list(fmt = GalParams$ncdf$format), dtimes))
    dtimes <- do.call(paste0, dtimes)

    ncpath <- file.path(GalParams$ncdf$dir, ncfiles)
    existFl <- unlist(lapply(ncpath, file.exists))

    if(!any(existFl)){
        Insert.Messages.Out(GalParams[['message']][['4']], TRUE, "e")
        return(NULL)
    }

    datetimes <- datetimes[existFl]
    ncpath <- ncpath[existFl]
    dtimes <- dtimes[existFl]

    ##################

    ncinfo <- getNCDFSampleData(GalParams$ncdf$sample)
    if(is.null(ncinfo)){
        Insert.Messages.Out(GalParams[['message']][['5']], TRUE, "e")
        return(NULL)
    }

    nlon <- length(ncinfo$lon)
    nlat <- length(ncinfo$lat)

    dx <- ncdf4::ncdim_def("lon", "degreeE", ncinfo$lon, longname = "Longitude")
    dy <- ncdf4::ncdim_def("lat", "degreeN", ncinfo$lat, longname = "Latitude")
    dt <- ncdf4::ncdim_def("time", dateOpts$units, datetimes,
                           calendar =  "standard", longname = "Time")

    ncgrd <- ncdf4::ncvar_def(ncinfo$varinfo$name, ncinfo$varinfo$units, list(dx, dy, dt),
                              ncinfo$varinfo$missval, ncinfo$varinfo$longname,
                              ncinfo$varinfo$prec, compression = 6)

    ncout <- ncdf4::nc_create(GalParams$file2save, ncgrd)

    for(j in seq_along(ncpath)){
        nc <- ncdf4::nc_open(ncpath[j])
        zval <- ncdf4::ncvar_get(nc, varid = ncinfo$varid)
        ncdf4::nc_close(nc)

        zval <- transposeNCDFData(zval, ncinfo)
        zval[is.na(zval)] <- ncinfo$varinfo$missval

        ncdf4::ncvar_put(ncout, ncgrd, zval, start = c(1, 1, j), count = c(nlon, nlat, 1))
    }

    ncdf4::ncatt_put(ncout, "lon", "axis", "X")
    ncdf4::ncatt_put(ncout, "lat", "axis", "Y")
    ncdf4::ncatt_put(ncout, "time", "axis", "T")
    ncdf4::ncatt_put(ncout, 0, "title", ncinfo$varinfo$longname)
    ncdf4::nc_close(ncout)

    return(0)
}
