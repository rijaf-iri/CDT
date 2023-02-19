
nasa_power_ag_daily.download <- function(GalParams, nbfile = 1, GUI = TRUE, verbose = TRUE){
    slon <- c(GalParams$bbox$minlon, GalParams$bbox$maxlon)
    slat <- c(GalParams$bbox$minlat, GalParams$bbox$maxlat)

    if(diff(slon) > 10){
        slon <- seq(slon[1], slon[2], 10)
        if(slon[length(slon)] != GalParams$bbox$maxlon)
            slon <- c(slon, GalParams$bbox$maxlon)
    }

    if(diff(slat) > 10){
        slat <- seq(slat[1], slat[2], 10)
        if(slat[length(slat)] != GalParams$bbox$maxlat)
            slat <- c(slat, GalParams$bbox$maxlat)
    }

    nlon <- length(slon)
    nlat <- length(slat)
    ndiv <- (nlon - 1) * (nlat - 1)
    mat_bbx <- matrix(seq(ndiv), nrow = nlon - 1, ncol = nlat - 1) 
    bbx <- vector(mode = 'list', length = ndiv)
    k <- 1
    for(j in 2:nlat){
        for(i in 2:nlon){
            bbx[[k]] <- list('longitude-min' = slon[i - 1],
                             'longitude-max' = slon[i],
                             'latitude-min' = slat[j - 1], 
                             'latitude-max' = slat[j])
            k <- k + 1
        }
    }

    query_bbox <- sapply(seq_along(bbx), function(k){
        query_bbx <- sapply(seq_along(bbx[[k]]), function(j){
           paste(names(bbx[[k]][j]), bbx[[k]][[j]], sep = '=')
        })
        paste0(query_bbx, collapse = '&')
    })

    #################

    start <- GalParams$date.range[paste0('start.', c('year', 'mon', 'day'))]
    start <- daily.start.end.time(start)
    end <- GalParams$date.range[paste0('end.', c('year', 'mon', 'day'))]
    end <- daily.start.end.time(end)

    seqDays <- seq(start, end, "day")
    query_dates <- format(seqDays, "%Y%m%d")

    #################

    nasap_var <- switch(GalParams$var,
         "wind" = c('WS10M', 'WS10M_MAX', 'WS10M_MIN',
                    'WS10M_RANGE', 'WD10M',
                    'WS50M', 'WS50M_MAX', 'WS50M_MIN',
                    'WS50M_RANGE', 'WD50M', 'WS2M'),
         "pres" = "PS",
         "hum" = c('QV2M', 'RH2M'), 
         "prcp" = "PRECTOTCORR",
         "temp" = c('T2M', 'T2MDEW', 'T2MWET', 'TS',
                    'T2M_RANGE', 'T2M_MAX', 'T2M_MIN'),
         "rad" = c('ALLSKY_SFC_SW_DWN', 'CLRSKY_SFC_SW_DWN',
                   'ALLSKY_KT', 'ALLSKY_SFC_LW_DWN', 'ALLSKY_SFC_PAR_TOT',
                   'CLRSKY_SFC_PAR_TOT', 'ALLSKY_SFC_UVA',
                   'ALLSKY_SFC_UVB', 'ALLSKY_SFC_UV_INDEX'),
        NULL)

    #################

    api_url <- "https://power.larc.nasa.gov/api/temporal/daily/regional"
    query_comm <- "community=AG"
    query_format <- "format=NETCDF"

    query_pars <- paste0(nasap_var, collapse = ',')
    query_pars <- paste('parameters', query_pars, sep = '=')

    query_vars <- sapply(seq_along(query_dates), function(j){
        q_start <- paste0('start=', query_dates[j])
        q_end <- paste0('end=', query_dates[j])
        query <- c(query_pars, q_start, q_end, query_comm, query_format)
        paste0(query, collapse = '&')
    })

    urls <- paste0(api_url, "?", query_vars)
    urls <- lapply(seq_along(urls), function(j){
        paste0(urls[j], '&', query_bbox)
    })

    #################

    data.name <- "NASA POWER Agroclimatology"
    dir.name <- "NASA-POWER_daily_data"
    outdir <- file.path(GalParams$dir2save, dir.name)
    dir.create(outdir, showWarnings = FALSE, recursive = TRUE)

    outdir <- file.path(outdir, paste0('NASA-POWER_', GalParams$var))
    dir.create(outdir, showWarnings = FALSE, recursive = TRUE)

    destfiles <- lapply(seq_along(query_dates), function(j){
        ncf <- paste0(GalParams$var, "_", query_dates[j], "_", seq_along(urls[[j]]), ".nc")
        file.path(outdir, ncf)
    })

    ncfiles <- file.path(outdir, paste0(GalParams$var, "_", query_dates, ".nc"))

    #################

    ret <- cdt.download.data(urls, destfiles, ncfiles,
                             nbfile, GUI, verbose, data.name,
                             nasa_power_ag_daily.download.data,
                             pars = mat_bbx)

    return(ret)
}

nasa_power_ag_daily.download.data <- function(lnk, dest, ncfl, pars){
    on.exit(lapply(dest, unlink))

    dest <- dest[[1]]
    lnk <- lnk[[1]]

    xx <- basename(ncfl)

    dc <- lapply(seq_along(lnk), function(j){
         ret <- try(curl::curl_download(lnk[j], dest[j]), silent = TRUE)
         if(inherits(ret, "try-error")) 1 else 0
    })

    if(all(unlist(dc) == 0)){
        ret <- nasa_power_ag_daily.format.data(dest, ncfl, pars)
        if(ret == 0) xx <- NULL
        # xx <- NULL
    }

    return(xx)
}

nasa_power_ag_daily.format.data <- function(dest, ncfl, pars){
    nc <- ncdf4::nc_open(dest[1])
    nc_vars <- lapply(seq_along(nc$var), function(j){
        list(name = nc$var[[j]]$name, units = nc$var[[j]]$units,
             prec = nc$var[[j]]$prec, longname = nc$var[[j]]$longname,
             missval = nc$var[[j]]$missval,
             standard_name = ncdf4::ncatt_get(nc, nc$var[[j]]$name, attname = "standard_name")$value
            )
    })
    time_dim <- nc$dim[['time']][c('name', 'units', 'vals')]
    ncdf4::nc_close(nc)

    don <- lapply(dest, function(ncf){
        nc <- ncdf4::nc_open(ncf)
        coords <- list(x = nc$dim[['lon']]$vals,
                       y = nc$dim[['lat']]$vals)
        data <- lapply(nc_vars, function(vr){
            ncdf4::ncvar_get(nc, vr$name)
        })
        names(data) <- sapply(nc_vars, function(vr) vr$name)
        ncdf4::nc_close(nc)

        list(coords = coords, data = data)
    })

    coords <- lapply(don, '[[', 'coords')
    lon <- do.call(c, lapply(coords[pars[, 1]], '[[', 'x'))
    lat <- do.call(c, lapply(coords[pars[1, ]], '[[', 'y'))

    don <- lapply(don, '[[', 'data')
    don <- lapply(nc_vars, function(vr){
        x <- lapply(don, '[[', vr$name)
        x <- lapply(seq(ncol(pars)), function(j){
            do.call(rbind, x[pars[, j]])
        })
        do.call(cbind, x)
    })

    don <- lapply(seq_along(don), function(j){
        x <- don[[j]]
        x[is.na(x)] <- nc_vars[[j]]$missval
        n <- dim(x)
        dim(x) <- c(n, 1)
        x
    })

    #######
    dx <- ncdf4::ncdim_def("lon", "degrees_east", lon, longname = "Longitude")
    dy <- ncdf4::ncdim_def("lat", "degrees_north", lat, longname = "Latitude")
    dt <- ncdf4::ncdim_def(time_dim$name, time_dim$units, time_dim$vals, longname = "Time")

    if(length(don) == 1){
        ncgrd <- ncdf4::ncvar_def(nc_vars[[1]]$name, nc_vars[[1]]$units,
                                  list(dx, dy, dt), nc_vars[[1]]$missval,
                                  nc_vars[[1]]$longname, nc_vars[[1]]$prec,
                                  compression = 6)

        nc <- ncdf4::nc_create(ncfl, ncgrd)
        ncdf4::ncvar_put(nc, ncgrd, don[[1]])
        ncdf4::ncatt_put(nc, nc_vars[[1]]$name, "standard_name", nc_vars[[1]]$standard_name, prec = "text")
        ncdf4::nc_close(nc)
    }else{
        ncgrd <- lapply(seq_along(nc_vars), function(j){
            ncdf4::ncvar_def(nc_vars[[j]]$name, nc_vars[[j]]$units,
                             list(dx, dy, dt), nc_vars[[j]]$missval,
                             nc_vars[[j]]$longname, nc_vars[[j]]$prec,
                             compression = 6)

        })

        nc <- ncdf4::nc_create(ncfl, ncgrd)
        for(j in seq_along(ncgrd)){
            ncdf4::ncvar_put(nc, ncgrd[[j]], don[[j]])
            ncdf4::ncatt_put(nc, nc_vars[[j]]$name, "standard_name", nc_vars[[j]]$standard_name, prec = "text")
        }
        ncdf4::nc_close(nc)
    }

    return(0)
}
