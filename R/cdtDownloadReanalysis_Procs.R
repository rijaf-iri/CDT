
exec.download_Reanalysis <- function(){
    on.exit({
        tkconfigure(.cdtEnv$tcl$main$win, cursor = '')
        tcl('update')
    })
    tkconfigure(.cdtEnv$tcl$main$win, cursor = 'watch')
    tcl('update')

   download.fun <- switch(.cdtData$GalParams$prod,
                          "jra55" = switch(.cdtData$GalParams$src,
                                           "rda.ucar.edu - ds628.0 - 3Hourly" = jra55_dods.download.rda.ucar,
                                           # "rda.ucar.edu - ds628.0" = jra55_ncss.download.rda.ucar,
                                           "rda.ucar.edu - ds628.8-NRT - 3Hourly" = jra55_nrt.download.rda.ucar,
                                           NULL),
                           "merra2" =  switch(.cdtData$GalParams$src,
                                              "disc.gsfc.nasa.gov - Hourly" = merra2_hourly.download.earthdata,
                                              "disc.gsfc.nasa.gov - Land - Hourly" = merra2_land.download.earthdata,
                                              "disc.gsfc.nasa.gov - Daily" = merra2_daily.download.earthdata,
                                              "iridl.ldeo.columbia.edu - Daily" = merra2.download.iridl,
                                              NULL), 
                           "era5" =  switch(.cdtData$GalParams$src,
                                            "cds.climate.copernicus.eu - ERA5 - Hourly" = era5_singleLev.download.cds,
                                            "cds.climate.copernicus.eu - ERA5-Land - Hourly" = era5_Land.download.cds,
                                            NULL)
                        )
    if(is.null(download.fun)){
        Insert.Messages.Out("Not available", TRUE, "w")
        return(0)
    }
    ret <- download.fun(.cdtData$GalParams)
    return(ret)
}

########

reanalysis.write.ncdf <- function(dat, ncpars, ncfile){
    dx <- ncdf4::ncdim_def("Lon", "degreeE", dat$x, longname = "Longitude")
    dy <- ncdf4::ncdim_def("Lat", "degreeN", dat$y, longname = "Latitude")

    if(length(ncpars$name) == 1){
        ncgrd <- ncdf4::ncvar_def(ncpars$name, ncpars$units,
                                  list(dx, dy), ncpars$missval,
                                  ncpars$longname, ncpars$prec,
                                  compression = 6)
        dat$z[is.na(dat$z)] <- ncpars$missval
        nc <- ncdf4::nc_create(ncfile, ncgrd)
        ncdf4::ncvar_put(nc, ncgrd, dat$z)
        ncdf4::nc_close(nc)
    }else{
        ncgrd <- lapply(seq_along(ncpars$name), function(j){
            ncdf4::ncvar_def(ncpars$name[j], ncpars$units[j],
                            list(dx, dy), ncpars$missval,
                            ncpars$longname[j], ncpars$prec,
                            compression = 6)
        })
        dat$z <- lapply(dat$z, function(x){
            x[is.na(x)] <- ncpars$missval
            x
        })

        nc <- ncdf4::nc_create(ncfile, ncgrd)
        for(j in seq_along(ncgrd))
            ncdf4::ncvar_put(nc, ncgrd[[j]], dat$z[[j]])
        ncdf4::nc_close(nc)
    }

    invisible()
}

########

hourly.start.end.time <- function(x){
    x <- paste(unlist(x), collapse = "-")
    as.POSIXct(x, tz = "UTC", format = "%Y-%m-%d-%H")
}

daily.start.end.time <- function(x){
    x <- paste(unlist(x), collapse = "-")
    as.Date(x,format = "%Y-%m-%d")
}

jra55.start.end.time <- function(x){
    div3 <- x[[4]] %% 3
    if(div3 != 0) x[[4]] <- x[[4]] - div3
    x <- paste(unlist(x), collapse = "-")
    as.POSIXct(x, tz = "GMT", format = "%Y-%m-%d-%H")
}

jra55.regrid.data <- function(lon, lat, don){
    rlon <- range(lon)
    rlat <- range(lat)
    lon_g <- seq(rlon[1], rlon[2], length.out = length(lon))
    lat_g <- seq(rlat[1], rlat[2], length.out = length(lat))
    obj0 <- list(lon = lon, lat = lat, z = don)
    obj_g <- cdt.interp.surface.grid(obj0, list(lon = lon_g, lat = lat_g))

    return(obj_g)
}

