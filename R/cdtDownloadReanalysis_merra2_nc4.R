
merra2.download.earthdata <- function(GalParams, nbfile = 1, GUI = TRUE, verbose = TRUE){
    on.exit(curl::handle_reset(handle))

    xlon <- seq(-180., 180., 0.625)
    xlat <- seq(-90., 90., 0.5)

    ix <- xlon >= GalParams$bbox$minlon & xlon <= GalParams$bbox$maxlon
    iy <- xlat >= GalParams$bbox$minlat & xlat <= GalParams$bbox$maxlat

    if(!any(ix) | !any(iy)) return(-2)

    ilon <- range(which(ix)) - c(2, 1)
    lon <- paste0("[", ilon[1], ":", ilon[2], "]")
    ilat <- range(which(iy)) - 1
    lat <- paste0("[", ilat[1], ":", ilat[2], "]")

    start <- GalParams$date.range[paste0('start.', c('year', 'mon', 'day'))]
    start <- as.Date(paste(unlist(start), collapse = "-"))
    end <- GalParams$date.range[paste0('end.', c('year', 'mon', 'day'))]
    end <- as.Date(paste(unlist(end), collapse = "-"))

    daty <- seq(start, end, "day")
    years <- format(daty, "%Y")
    month <- format(daty, "%m")
    daty <- format(daty, "%Y%m%d")

    opendap <- "https://goldsmr4.gesdisc.eosdis.nasa.gov/opendap/MERRA2/M2SDNXSLV.5.12.4"
    nc4EndPt <- paste0("MERRA2_400.statD_2d_slv_Nx.", daty, ".nc4.nc4")
    baseUrl <- paste(opendap, years, month, nc4EndPt, sep = "/")

    ######################
    varname <- switch(GalParams$var,
                      "tmax" = "T2MMAX",
                      "tmin" = "T2MMIN",
                      "tmean" = "T2MMEAN",
                      NULL
                     )

    req <- paste0(varname, "[0:0]", lat, lon, ",time,", "lat", lat, ",", "lon", lon)
    urls <- paste0(baseUrl, "?", req)

    ######################
    data.name <- "MERRA-2 Daily"
    outdir <- file.path(GalParams$dir2save, paste0("MERRA2_daily_disc_", GalParams$var))
    dir.create(outdir, showWarnings = FALSE, recursive = TRUE)

    destfiles <- file.path(outdir, paste0(GalParams$var, "_", daty, ".nc"))

    ######################

    handle <- curl::new_handle()
    curl::handle_setopt(handle, username = GalParams$login$usr, password = GalParams$login$pwd)

    ret <- cdt.download.data(urls, destfiles, destfiles, nbfile, GUI,
                             verbose, data.name, merra2.download.data,
                             handle = handle)

    return(ret)
}

#################################################################################

merra2.download.data <- function(lnk, dest, ncfl, handle){
    dc <- try(curl::curl_download(lnk, dest, handle = handle), silent = TRUE)

    xx <- basename(dest)
    if(!inherits(dc, "try-error")){
        ret <- merra2.format.data(dest)
        if(ret == 0) xx <- NULL
    }

    return(xx)
}

merra2.format.data <- function(ncfl){
    nc <- ncdf4::nc_open(ncfl)
    varid <- nc$var[[1]]$name
    lon <- nc$dim[['lon']]$vals
    lat <- nc$dim[['lat']]$vals
    val <- ncdf4::ncvar_get(nc, varid)
    ncdf4::nc_close(nc)
    unlink(ncfl)

    info <- switch(varid,
                   'T2MMAX' = list(name = 'tmax', units = 'degC',
                                   longname = '2-meter maximum air temperature'),
                   'T2MMIN' = list(name = 'tmin', units = 'degC',
                                   longname = '2-meter minimum air temperature'),
                   'T2MMEAN' = list(name = 'tmean', units = 'degC',
                                    longname = '2-meter average air temperature'),
                    NULL
                  )
    missval <- -99

    dx <- ncdf4::ncdim_def("lon", "degrees_east", lon, longname = "longitude")
    dy <- ncdf4::ncdim_def("lat", "degrees_north", lat, longname = "latitude")
    ncgrd <- ncdf4::ncvar_def(info$name, info$units, list(dx, dy), missval,
                              info$longname, "float", compression = 6)

    val <- val - 273.15
    val[is.na(val)] <- missval
    nc <- ncdf4::nc_create(ncfl, ncgrd)
    ncdf4::ncvar_put(nc, ncgrd, val)
    ncdf4::nc_close(nc)

    return(0)
}

