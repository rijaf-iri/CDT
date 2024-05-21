
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
                          "jra3q" = switch(.cdtData$GalParams$src,
                                           "rda.ucar.edu - ds640.0 - Hourly" = jra3q_dods.download.rda.ucar,
                                           "rda.ucar.edu - ds640.1-NRT - Hourly" = jra3q_nrt.download.rda.ucar,
                                           NULL),
                           "merra2" = switch(.cdtData$GalParams$src,
                                             "disc.gsfc.nasa.gov - Hourly" = merra2_hourly.download.earthdata,
                                             "disc.gsfc.nasa.gov - Land - Hourly" = merra2_hourly.download.earthdata,
                                             "disc.gsfc.nasa.gov - Daily" = merra2_daily.download.earthdata,
                                             "iridl.ldeo.columbia.edu - Daily" = merra2.download.iridl,
                                             NULL),
                           "era5" = switch(.cdtData$GalParams$src,
                                           "cds.climate.copernicus.eu - ERA5 - Hourly" = era5.cds.download,
                                           "cds.climate.copernicus.eu - ERA5-Land - Hourly" = era5.cds.download,
                                           NULL),
                           "nasap" = switch(.cdtData$GalParams$src,
                                            "power.larc.nasa.gov - Agroclimatology - Daily" = nasa_power_ag_daily.download,
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

check.coverage_Reanalysis <- function(){
    on.exit({
        tkconfigure(.cdtEnv$tcl$main$win, cursor = '')
        tcl('update')
    })
    tkconfigure(.cdtEnv$tcl$main$win, cursor = 'watch')
    tcl('update')

   download.fun <- switch(.cdtData$GalParams$prod,
                          "jra55" = switch(.cdtData$GalParams$src,
                                           "rda.ucar.edu - ds628.0 - 3Hourly" = jra55_dods.coverage.rda.ucar,
                                           "rda.ucar.edu - ds628.8-NRT - 3Hourly" = jra55_nrt.coverage.rda.ucar,
                                           NULL),
                          "jra3q" = switch(.cdtData$GalParams$src,
                                           "rda.ucar.edu - ds640.0 - Hourly" = jra3q_dods.coverage.rda.ucar,
                                           "rda.ucar.edu - ds640.1-NRT - Hourly" = jra3q_nrt.coverage.rda.ucar,
                                           NULL),
                          "merra2" = switch(.cdtData$GalParams$src,
                                            "disc.gsfc.nasa.gov - Hourly" = merra2_hourly.coverage.earthdata,
                                            "disc.gsfc.nasa.gov - Land - Hourly" = merra2_hourly.coverage.earthdata,
                                            "disc.gsfc.nasa.gov - Daily" = merra2_daily.coverage.earthdata,
                                            "iridl.ldeo.columbia.edu - Daily" = merra2.coverage.iridl,
                                            NULL),
                          "era5" = switch(.cdtData$GalParams$src,
                                          "cds.climate.copernicus.eu - ERA5 - Hourly" = era5.cds.coverage,
                                          "cds.climate.copernicus.eu - ERA5-Land - Hourly" = era5.cds.coverage,
                                          NULL),
                          "nasap" = switch(.cdtData$GalParams$src,
                                          "power.larc.nasa.gov - Agroclimatology - Daily" = nasa_power_ag_daily.coverage,
                                          NULL)
                        )

    if(is.null(download.fun)){
        return(list(name = 'Unknown', timestep = 'Unknown'))
    }

    ret <- download.fun(.cdtData$GalParams)
    if(!is.null(ret$end)){
        txt_reanal <- paste("Reanalysis :", ret$name)
        txt_tstep <- paste("Temporal resolution :", ret$timestep)
        txt_start <- paste("Start date :", ret$start)
        txt_end <- paste("End date :", ret$end)
        disp <- paste(txt_reanal, txt_tstep, txt_start, txt_end, sep = '\n')

        containertab <- Display_Output_Console_Tab(disp, .cdtData$GalParams$reanalysis, cat)
        ntab <- update.OpenTabs('ctxt', containertab)
        tkselect(.cdtEnv$tcl$main$tknotes, ntab)
    }

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
                                  compression = 9)
        dat$z[is.na(dat$z)] <- ncpars$missval
        nc <- ncdf4::nc_create(ncfile, ncgrd)
        ncdf4::ncvar_put(nc, ncgrd, dat$z)
        ncdf4::nc_close(nc)
    }else{
        ncgrd <- lapply(seq_along(ncpars$name), function(j){
            ncdf4::ncvar_def(ncpars$name[j], ncpars$units[j],
                            list(dx, dy), ncpars$missval,
                            ncpars$longname[j], ncpars$prec,
                            compression = 9)
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

jra55.regrid.data <- function(lon, lat, don){
    rlon <- range(lon)
    rlat <- range(lat)
    lon_g <- seq(rlon[1], rlon[2], length.out = length(lon))
    lat_g <- seq(rlat[1], rlat[2], length.out = length(lat))
    obj0 <- list(lon = lon, lat = lat, z = don)
    obj_g <- cdt.interp.surface.grid(obj0, list(lon = lon_g, lat = lat_g))

    return(obj_g)
}

########

get_reanalysis.variables <- function(rfile){
    vFile <- file.path(.cdtDir$Root, "reanalysis", rfile)
    opts <- utils::read.table(vFile, sep = ',', header = TRUE, na.strings = '',
                              stringsAsFactors = FALSE)
    ix <- which(!is.na(opts$cdt_var))
    ie <- c(ix[-1] - 1, nrow(opts))

    xx <- lapply(seq_along(ix), function(i){
        x <- opts[ix[i]:ie[i], ]
        x <- as.list(x)
        lapply(x, function(v){
            v <- v[!is.na(v)]
            if(length(v) == 0) v <- NULL
            v
        })
    })
    names(xx) <- sapply(xx, '[[', 'cdt_var')
    return(xx)
}

