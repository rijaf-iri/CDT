
merra2_daily.coverage.earthdata <- function(GalParams){
    out <- list(name = "The second Modern-Era Retrospective analysis for Research and Applications (MERRA-2)", timestep = "daily")

    opendap_url <- "https://goldsmr4.gesdisc.eosdis.nasa.gov/opendap/MERRA2"
    dataset <- "M2SDNXSLV.5.12.4"
    streams <- 400
    nc4format <- paste0("MERRA2_", 400, ".statD_2d_slv_Nx.%s.nc4")

    baseurl <- file.path(opendap_url, dataset)
    url <- file.path(baseurl, 'contents.html')
    end_d <- opendap.gesdisc.dates(url, 'directory', datetype = 'year')
    if(is.null(end_d)) return(out)
    end_year <- end_d[length(end_d)]

    url <- file.path(baseurl, end_year, 'contents.html')
    end_d <- opendap.gesdisc.dates(url, 'directory', datetype = 'month', diryear = end_year)
    if(is.null(end_d)) return(out)
    end_mon <- end_d[length(end_d)]

    url <- file.path(baseurl, end_year, end_mon, 'contents.html')
    end_d <- opendap.gesdisc.dates(url, 'file', nc4format)
    if(is.null(end_d)) return(out)
    end_d <- end_d[length(end_d)]
    end_d <- as.Date(end_d, '%Y%m%d')
    out$end <- format(end_d, '%Y-%m-%d')
    out$start <- "1980-01-01"

    return(out)
}

merra2_daily.download.earthdata <- function(GalParams, nbfile = 3, GUI = TRUE, verbose = TRUE){
    xlon <- seq(-180., 180., 0.625)
    xlat <- seq(-90., 90., 0.5)

    ix <- xlon >= GalParams$bbox$minlon & xlon <= GalParams$bbox$maxlon
    iy <- xlat >= GalParams$bbox$minlat & xlat <= GalParams$bbox$maxlat

    if(!any(ix) | !any(iy)){
        Insert.Messages.Out("Invalid area of interest", TRUE, "e", GUI)
        return(-2)
    }

    ilon <- range(which(ix)) - c(2, 1)
    lon <- paste0("[", ilon[1], ":", ilon[2], "]")
    ilat <- range(which(iy)) - 1
    lat <- paste0("[", ilat[1], ":", ilat[2], "]")

    daty <- seq.format.date.time('daily', GalParams$date.range)
    if(is.null(daty)) return(-2)

    years <- format(daty, "%Y")
    month <- format(daty, "%m")
    daty <- format(daty, "%Y%m%d")

    #######
    ## assimilation stream version
    streams <- cut(as.numeric(years), 
        breaks = c(1980, 1992, 2001, 2011, 2100),
        labels = c(100, 200, 300, 400), 
        include.lowest = TRUE, right = FALSE)

    opendap <- "https://goldsmr4.gesdisc.eosdis.nasa.gov/opendap/MERRA2"
    dataset <- "M2SDNXSLV.5.12.4"
    nc4EndPt <- paste0("MERRA2_", streams, ".statD_2d_slv_Nx.", daty, ".nc4.nc4")
    baseUrl <- paste(opendap, dataset, years, month, nc4EndPt, sep = "/")

    ##########
    varname <- switch(GalParams$var,
                      "tmax" = "T2MMAX",
                      "tmin" = "T2MMIN",
                      "tair" = "T2MMEAN",
                      "prcp" = "TPRECMAX",
                      NULL
                     )

    req <- paste0(varname, "[0:0]", lat, lon, ",time,", "lat", lat, ",", "lon", lon)
    urls <- paste0(baseUrl, "?", req)

    ##########
    data.name <- "MERRA-2 Daily GES DISC"
    dir.name <- "MERRA2_Daily_data_disc"

    outdir <- file.path(GalParams$dir2save, dir.name)
    outdir <- file.path(outdir, paste0('MERRA2_', GalParams$var))
    dir.create(outdir, showWarnings = FALSE, recursive = TRUE)

    destfiles <- file.path(outdir, paste0(GalParams$var, "_", daty, ".nc"))

    ##########
    auth <- earthdata.curlopts(outdir, GalParams$login)
    handle <- curl::new_handle()
    curl::handle_setopt(handle,
                        netrc_file = auth$netrc,
                        cookiefile = auth$cookie,
                        cookiejar = auth$cookie)
    on.exit({
        curl::handle_reset(handle)
        unlink(auth$cookie)
        unlink(auth$netrc)
    })

    ret <- cdt.download.data(urls, destfiles, destfiles, nbfile, GUI,
                             verbose, data.name, merra2_daily.download.data,
                             handle = handle)
    return(ret)
}

merra2_daily.download.data <- function(lnk, dest, ncfl, handle){
    dc <- try(curl::curl_download(lnk, dest, handle = handle), silent = TRUE)

    xx <- basename(dest)
    if(!inherits(dc, "try-error")){
        ret <- merra2_daily.format.data(dest)
        if(ret == 0) xx <- NULL
    }

    return(xx)
}

merra2_daily.format.data <- function(ncfl){
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
                   'TPRECMAX' = list(name = 'precip', units = 'mm',
                                    longname = 'maximum total precipitation'),
                    NULL
                  )
    missval <- -9999

    dx <- ncdf4::ncdim_def("lon", "degrees_east", lon, longname = "longitude")
    dy <- ncdf4::ncdim_def("lat", "degrees_north", lat, longname = "latitude")
    ncgrd <- ncdf4::ncvar_def(info$name, info$units, list(dx, dy), missval,
                              info$longname, "float", compression = 9)
    if(varid == 'TPRECMAX'){
        val <- val * 86400 
    }else{
        val <- val - 273.15
    }
    val <- round(val, 2)
    val[is.na(val)] <- missval
    nc <- ncdf4::nc_create(ncfl, ncgrd)
    ncdf4::ncvar_put(nc, ncgrd, val)
    ncdf4::nc_close(nc)

    return(0)
}
