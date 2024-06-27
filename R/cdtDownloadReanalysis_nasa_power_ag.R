nasa_power_ag_daily.coverage <- function(GalParams){
    r_name <- "NASA Prediction of Worldwide Energy Resources (POWER)"
    out <- list(name = r_name, timestep = "daily")

    today <- Sys.Date()
    last_mon <- Sys.Date() - 30
    date1 <- format(last_mon, '%Y%m%d')
    date2 <- format(today, '%Y%m%d')
    url <- "https://power.larc.nasa.gov/api/temporal/daily/point"
    query <- list(parameters = "T2M", community = "AG",
                  longitude = 47, latitude = -19,
                  start = date1, end = date2,
                  format = "JSON")
    ret <- httr::GET(url, query = query)
    if(httr::status_code(ret) != 200){
        Insert.Messages.httr(ret)
        return(out)
    }
    ret <- httr::content(ret)
    ret <- ret$properties$parameter$T2M
    dates <- names(ret)
    ret <- do.call(c, ret)
    dates <- dates[ret != -999]
    end_d <- dates[length(dates)]
    end_d <- as.Date(end_d, '%Y%m%d')
    out$end <- format(end_d, '%Y-%m-%d')
    if(GalParams$var){
        out$start <- "1984-01-01"
    }else{
        out$start <- "1981-01-01"
    }

    return(out)
}

nasa_power_ag_daily.download <- function(GalParams, nbfile = 3, GUI = TRUE, verbose = TRUE){
    slon <- c(GalParams$bbox$minlon, GalParams$bbox$maxlon)
    slat <- c(GalParams$bbox$minlat, GalParams$bbox$maxlat)

    if(diff(slon) > 8){
        slon <- seq(slon[1], slon[2], 8)
        if(slon[length(slon)] != GalParams$bbox$maxlon)
            slon <- c(slon, GalParams$bbox$maxlon)
        nx <- length(slon)
        if((slon[nx] - slon[nx - 1]) < 2){
            slon[nx - 1] <- slon[nx - 1] - 2
        }
    }

    if(diff(slat) > 8){
        slat <- seq(slat[1], slat[2], 8)
        if(slat[length(slat)] != GalParams$bbox$maxlat)
            slat <- c(slat, GalParams$bbox$maxlat)
        ny <- length(slat)
        if((slat[ny] - slat[ny - 1]) < 2){
            slat[ny - 1] <- slat[ny - 1] - 2
        }
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

    seqDays <- seq.format.date.time('daily', GalParams$date.range)
    if(is.null(seqDays)) return(-2)
    query_dates <- format(seqDays, "%Y%m%d")

    #################

    opts <- get_reanalysis.variables('nasa_power_ag_options.csv')
    opts <- opts[[GalParams$var]]

    opts$convert <- NULL
    if(!is.null(opts$units_fun)){
        opts$convert <- list(fun = as.character(opts$units_fun),
                             args = as.character(opts$units_args))
    }

    #################

    api_url <- "https://power.larc.nasa.gov/api/temporal/daily/regional"
    query_comm <- "community=AG"
    query_format <- "format=NETCDF"

    query_pars <- paste0(opts$var_name, collapse = ',')
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
                             pars = mat_bbx, opts = opts)

    return(ret)
}

nasa_power_ag_daily.download.data <- function(lnk, dest, ncfl, pars, opts, GUI = TRUE){
    on.exit(lapply(dest, unlink))

    dest <- dest[[1]]
    lnk <- lnk[[1]]

    xx <- basename(ncfl)

    dc <- lapply(seq_along(lnk), function(j){
         ret <- try(curl::curl_download(lnk[j], dest[j]), silent = TRUE)
         rc <- 0
         if(inherits(ret, "try-error")){
            msg <- gsub('[\r\n]', '', ret[1])
            Insert.Messages.Out(msg, TRUE, "w", GUI)
            rc <- 1
         }
         rc
    })

    if(all(unlist(dc) == 0)){
        ret <- nasa_power_ag_daily.format.data(dest, ncfl, pars, opts)
        if(ret == 0) xx <- NULL
    }

    return(xx)
}

nasa_power_ag_daily.format.data <- function(dest, ncfl, pars, opts){
    don <- lapply(seq_along(dest), function(j){
        nc <- ncdf4::nc_open(dest[j])
        val <- lapply(opts$var_name, function(v){
            x <- ncdf4::ncvar_get(nc, v)
            if(!is.null(opts$convert)){
                x <- eval_function(opts$convert$fun, opts$convert$args, x)
            }
            x
        })
        names(val) <- opts$var_name
        coords <- list(x = nc$dim[['lon']]$vals,
                       y = nc$dim[['lat']]$vals)
        ncdf4::nc_close(nc)
        list(coords = coords, data = val)
    })

    coords <- lapply(don, '[[', 'coords')
    lon <- do.call(c, lapply(coords[pars[, 1]], '[[', 'x'))
    lat <- do.call(c, lapply(coords[pars[1, ]], '[[', 'y'))

    don <- lapply(don, '[[', 'data')
    don <- lapply(opts$var_name, function(v){
        x <- lapply(don, '[[', v)
        x <- lapply(seq(ncol(pars)), function(j){
            do.call(rbind, x[pars[, j]])
        })
        do.call(cbind, x)
    })

    #######
    dx <- ncdf4::ncdim_def("lon", "degrees_east", lon, longname = "Longitude")
    dy <- ncdf4::ncdim_def("lat", "degrees_north", lat, longname = "Latitude")
    dxy <- list(dx, dy)

    if(length(don) == 1){
        ncgrd <- ncdf4::ncvar_def(opts$nc_name, opts$nc_units, dxy, -999,
                                  opts$nc_longname, 'float', compression = 9)
        tmp <- don[[1]]
        tmp[is.na(tmp)] <- -999
        nc <- ncdf4::nc_create(ncfl, ncgrd)
        ncdf4::ncvar_put(nc, ncgrd, tmp)
        ncdf4::nc_close(nc)
    }else{
        ncgrd <- lapply(seq_along(opts$var_name), function(j){
            ncdf4::ncvar_def(opts$nc_name[j], opts$nc_units[j], dxy, -999,
                             opts$nc_longname[j], 'float', compression = 9)
        })
        nc <- ncdf4::nc_create(ncfl, ncgrd)
        for(j in seq_along(ncgrd)){
            tmp <- don[[j]]
            tmp[is.na(tmp)] <- -999
            ncdf4::ncvar_put(nc, ncgrd[[j]], tmp)
        }
        ncdf4::nc_close(nc)
    }

    return(0)
}
