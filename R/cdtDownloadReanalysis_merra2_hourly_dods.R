
merra2_hourly.coverage.earthdata <- function(GalParams){
    r_name <- "Modern-Era Retrospective analysis for Research and Applications Version 2"
    product <- if(GalParams$src == "disc.gsfc.nasa.gov - Hourly") "MERRA-2" else "MERRA-2 Land"
    r_name <- paste0(r_name, " (", product, ")")
    out <- list(name = r_name, timestep = "hourly")

    if(GalParams$src == "disc.gsfc.nasa.gov - Hourly"){
        opts_file <- 'merra2_dods_options.csv'
    }else{
        opts_file <- 'merra2_land_dods_options.csv'
    }
    opts <- get_reanalysis.variables(opts_file)
    opts <- opts[[GalParams$var]]

    opendap_url <- "https://goldsmr4.gesdisc.eosdis.nasa.gov/opendap/MERRA2"
    merra2_version <- "5.12.4"
    query_paths <- paste(opts$dap_path, merra2_version, sep = ".")
    streams <- 400
    query_stream <- paste0("MERRA2_", streams)
    nc4format <- paste0(query_stream, ".", opts$var_path, ".%s.nc4")

    baseurl <- file.path(opendap_url, query_paths)
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
    end_d <- as.POSIXct(paste0(end_d, 23), format = '%Y%m%d%H', tz = 'UTC')
    out$end <- format(end_d, '%Y-%m-%d %H:%M:%S')
    out$start <- "1980-01-01 00:00:00"

    return(out)
}

merra2_hourly.download.earthdata <- function(GalParams, nbfile = 3, GUI = TRUE, verbose = TRUE){
    xlon <- seq(-180., 180., 0.625)
    xlat <- seq(-90., 90., 0.5)

    ix <- xlon >= GalParams$bbox$minlon & xlon <= GalParams$bbox$maxlon
    iy <- xlat >= GalParams$bbox$minlat & xlat <= GalParams$bbox$maxlat

    if(!any(ix) | !any(iy)){
        Insert.Messages.Out("Invalid area of interest", TRUE, "e", GUI)
        return(-2)
    }

    ilon <- range(which(ix)) - c(2, 1)
    ilat <- range(which(iy)) - 1
    query_lon <- paste0("[", ilon[1], ":", ilon[2], "]")
    query_lat <- paste0("[", ilat[1], ":", ilat[2], "]")

    #######
    seq_times <- seq.format.date.time('hourly', GalParams$date.range, 1)
    if(is.null(seq_times)) return(-2)

    dates <- format(seq_times, "%Y%m%d")
    idates <- split(seq_along(dates), dates)
    query_hours <- lapply(idates, function(j){
        hours <- as.numeric(format(seq_times[j], '%H'))

        ## method 1
        # dfs <- diff(hours)
        # it <- which(dfs > 1) + 1
        # tt <- cumsum(seq_along(hours) %in% it)
        # hh <- split(hours, tt)
        # sapply(hh, function(x) paste0(range(x), collapse = ':'))

        ## method 2
        dfs <- diff(hours)
        it <- which(c(1, dfs) > 1)
        st <- c(1, it)
        et <- c(st - 1, length(hours))
        paste(hours[st], hours[et], sep = ":")
    })

    query_times <- lapply(seq_along(idates), function(j){
        nl <- length(query_hours[[j]])
        daty <- rep(names(idates[j]), nl)
        list(date = daty,
             year = substr(daty, 1, 4),
             month = substr(daty, 5, 6),
             hours = query_hours[[j]])
    })

    query_dates <- do.call(c, lapply(query_times, '[[', 'date'))
    query_years <- do.call(c, lapply(query_times, '[[', 'year'))
    query_months <- do.call(c, lapply(query_times, '[[', 'month'))
    query_hours <- do.call(c, lapply(query_times, '[[', 'hours'))
    file_hours <- gsub('\\:', '-', query_hours)
    query_hours <- paste0('[', query_hours, ']')

    #######
    ## assimilation stream version
    query_stream <- cut(as.numeric(query_years), 
        breaks = c(1980, 1992, 2001, 2011, 2100),
        labels = c(100, 200, 300, 400), 
        include.lowest = TRUE, right = FALSE)

    ########
    if(GalParams$src == "disc.gsfc.nasa.gov - Hourly"){
        opts_file <- 'merra2_dods_options.csv'
    }else{
        opts_file <- 'merra2_land_dods_options.csv'
    }
    opts <- get_reanalysis.variables(opts_file)
    opts <- opts[[GalParams$var]]

    opts$convert <- NULL
    if(!is.null(opts$units_fun)){
        opts$convert <- list(fun = as.character(opts$units_fun),
                             args = as.character(opts$units_args))
    }

    #######
    opendap_url <- "https://goldsmr4.gesdisc.eosdis.nasa.gov/opendap/MERRA2"
    merra2_version <- "5.12.4"

    query_paths <- paste(opts$dap_path, merra2_version, sep = ".")
    query_stream <- paste0("MERRA2_", query_stream)
    query_endpoints <- paste(query_stream, opts$var_path, query_dates, "nc4.nc4", sep = ".")
    query_urls <- paste(opendap_url, query_paths, query_years, query_months, query_endpoints, sep = "/")

    query_vars <- sapply(seq_along(query_urls), function(j){
        x <- sapply(opts$var_name, function(v){
            paste0(v, query_hours[j], query_lat, query_lon)
        })
        paste0(x, collapse = ",")
    })

    query_dims <- paste0("time", query_hours, ",lat", query_lat, ",lon", query_lon)
    query_opendap <- paste0(query_vars, ",", query_dims)
    urls <- paste0(query_urls, "?", query_opendap)

    ######################

    if(GalParams$src == "disc.gsfc.nasa.gov - Hourly"){
        data.name <- "MERRA2 Hourly GES DISC"
        dir.name <- "MERRA2_1Hr_data"
    }else{
        data.name <- "MERRA2 Land Hourly GES DISC"
        dir.name <- "MERRA2_Land_1Hr_data"
    }
    outdir <- file.path(GalParams$dir2save, dir.name)
    outdir <- file.path(outdir, paste0('MERRA2_', GalParams$var))
    dir.create(outdir, showWarnings = FALSE, recursive = TRUE)

    destfiles <- paste0(GalParams$var, "_", query_dates, "_", file_hours, ".nc")
    destfiles <- file.path(outdir, destfiles)

    ######################
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
                             verbose, data.name, merra2_dods.download.data,
                             opts = opts, handle = handle)

    return(ret)
}

merra2_dods.download.data <- function(lnk, dest, ncfl, opts, handle, GUI = TRUE){
    on.exit(unlink(dest))
    xx <- basename(dest)
    dc <- try(curl::curl_download(lnk, dest, handle = handle), silent = TRUE)

    if(!inherits(dc, "try-error")){
        ret <- merra2_dods.format.data(dest, opts)
        if(ret == 0) xx <- NULL
    }else{
        msg <- gsub('[\r\n]', '', dc[1])
        Insert.Messages.Out(msg, TRUE, "w", GUI)
    }

    return(xx)
}

merra2_dods.format.data <- function(dest, opts){
    nc <- ncdf4::nc_open(dest)
    lon <- nc$dim[['lon']]$vals
    lat <- nc$dim[['lat']]$vals
    times <- nc$dim[['time']]$vals
    t_unit <- nc$dim[['time']]$units
    val <- lapply(opts$var_name, function(v){
        ncdf4::ncvar_get(nc, v, collapse_degen = FALSE)
    })
    names(val) <- opts$var_name
    ncdf4::nc_close(nc)

    units(times) <- units::as_units(t_unit)
    times <- as.POSIXct(times, tz = "UTC")
    hours <- format(times, "%Y%m%d%H")
    ncfiles <- paste0(opts$cdt_var, "_", hours, ".nc")
    ncfiles <- file.path(dirname(dest), ncfiles)

    val <- lapply(val, function(v){
        if(!is.null(opts$convert)){
            v <- eval_function(opts$convert$fun, opts$convert$args, v)
        }
        v
    })

    if(opts$cdt_var == "hum"){
        val[["T2M"]] <- val[["T2M"]] - 273.15
        val[["T2MDEW"]] <- val[["T2MDEW"]] - 273.15
        val[["T2MWET"]] <- val[["T2MWET"]] - 273.15
        val[["PS"]] <- val[["PS"]] / 100
        rh <- relative_humidity(val[["T2M"]], val[["T2MDEW"]])

        val <- list(val[["T2M"]], val[["PS"]], val[["T2MDEW"]],
                    val[["T2MWET"]], val[["QV2M"]], rh)
    }

    ncpars <- list(name = opts$nc_name, units = opts$nc_units,
                   longname = opts$nc_longname,
                   prec = "float", missval = -9999)

    for(j in seq_along(ncfiles)){
        don <- lapply(val, function(v) v[, , j])
        if(length(don) == 1) don <- don[[1]]
        dat <- list(x = lon, y = lat, z = don)
        reanalysis.write.ncdf(dat, ncpars, ncfiles[j])
    }

    return(0)
}
