
merra2.coverage.iridl <- function(GalParams){
    out <- list(name = "The second Modern-Era Retrospective analysis for Research and Applications (MERRA-2)", timestep = "daily")
    varid <- switch(GalParams$var,
                      "tmax" = ".t2mmax",
                      "tmin" = ".t2mmin",
                      "tair" = ".t2mmean",
                      "prcp" = ".tprecmax"
                    )
    baseurl <- "https://iridl.ldeo.columbia.edu/SOURCES/.NASA/.GSFC/.MERRA2/.Anl_MonoLev"
    url <- paste0(baseurl, '/', varid)
    end_date <- iridl.get.end_date(url, 'daily')
    end_date <- as.Date(end_date, '%Y%m%d')
    out$end <- format(end_date, '%Y-%m-%d')
    out$start <- "1980-01-01"

    return(out)
}

merra2.download.iridl <- function(GalParams, nbfile = 3, GUI = TRUE, verbose = TRUE){
    dlpath <- "https://iridl.ldeo.columbia.edu/SOURCES/.NASA/.GSFC/.MERRA2/.Anl_MonoLev"
    varid <- switch(GalParams$var,
                      "tmax" = ".t2mmax",
                      "tmin" = ".t2mmin",
                      "tair" = ".t2mmean",
                      "prcp" = ".tprecmax"
                    )
    rlon <- unlist(GalParams$bbox[c('minlon', 'maxlon')])
    rlon <- paste(c('X', rlon, 'RANGE'), collapse = "/")
    rlat <- unlist(GalParams$bbox[c('minlat', 'maxlat')])
    rlat <- paste(c('Y', rlat, 'RANGE'), collapse = "/")
    if(GalParams$var == "prcp"){
        units <- "86400/mul//units/%28mm%29/def"
    }else{
        units <- "273.15/sub//units/%28Celcius_scale%29/def"
    }

    rdate <- iridl.format.date("daily", GalParams$date.range)
    if(is.null(rdate)) return(-2)
    urls <- urltools::url_encode(paste0("(", rdate$dates, ")"))
    urls <- paste0("T", "/", urls, "/", "VALUE")

    urls <- paste(dlpath, varid, rlon, rlat, urls, units, 'data.nc', sep = "/")

    #########

    data.name <- "MERRA-2 daily IRI DL"
    dir.name <- "MERRA2_daily_data_IRI"

    outdir <- file.path(GalParams$dir2save, dir.name)
    outdir <- file.path(outdir, paste0('MERRA2_', GalParams$var))
    dir.create(outdir, showWarnings = FALSE, recursive = TRUE)

    destfiles <- file.path(outdir, paste0(GalParams$var, "_", rdate$out, "_K.nc"))
    ncfiles <- file.path(outdir, paste0(GalParams$var, "_", rdate$out, ".nc"))

    ret <- cdt.download.data(urls, destfiles, ncfiles, nbfile, GUI, verbose,
                             data.name, merra2_iridl.download.data)

    return(ret)
}

merra2_iridl.download.data <- function(lnk, dest, ncfl){
    on.exit(unlink(dest))

    xx <- basename(dest)
    dc <- try(curl::curl_download(lnk, dest), silent = TRUE)

    if(!inherits(dc, "try-error")){
        ret <- merra2_iridl.format.data(dest, ncfl)
        if(ret == 0) xx <- NULL
    }

    return(xx)
}

merra2_iridl.format.data <- function(dest, ncfl){
    nc <- ncdf4::nc_open(dest)
    varid <- nc$var[[1]]$name
    lon <- nc$dim[['X']]$vals
    lat <- nc$dim[['Y']]$vals
    val <- ncdf4::ncvar_get(nc, varid)
    ncdf4::nc_close(nc)

    val[is.nan(val)] <- NA
    val <- round(val, 2)

    info <- switch(varid,
                   't2mmax' = list(name = 'tmax', units = 'degC',
                                   longname = '2-meter maximum air temperature'),
                   't2mmin' = list(name = 'tmin', units = 'degC',
                                   longname = '2-meter minimum air temperature'),
                   't2mmean' = list(name = 'tmean', units = 'degC',
                                    longname = '2-meter average air temperature'),
                   'tprecmax' = list(name = 'precip', units = 'mm',
                                    longname = 'maximum total precipitation'),
                    NULL
                  )
    missval <- -9999

    dx <- ncdf4::ncdim_def("lon", "degrees_east", lon, longname = "longitude")
    dy <- ncdf4::ncdim_def("lat", "degrees_north", lat, longname = "latitude")
    ncgrd <- ncdf4::ncvar_def(info$name, info$units, list(dx, dy), missval,
                              info$longname, "float", compression = 9)

    val[is.na(val)] <- missval
    nc <- ncdf4::nc_create(ncfl, ncgrd)
    ncdf4::ncvar_put(nc, ncgrd, val)
    ncdf4::nc_close(nc)

    return(0)
}
