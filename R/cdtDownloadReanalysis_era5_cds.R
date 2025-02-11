
era5.cds.coverage <- function(GalParams){
    if(GalParams$src == "cds.climate.copernicus.eu - ERA5 - Hourly"){
        r_name <- "Fifth generation ECMWF reanalysis (ERA5) on single levels"
        endpoint <- "reanalysis-era5-single-levels"
    }else{
        r_name <- "Fifth generation ECMWF reanalysis (ERA5-Land)"
        endpoint <- "reanalysis-era5-land"
    }
    out <- list(name = r_name, timestep = "hourly")
    base_url <- "https://cds.climate.copernicus.eu"
    api_pth <- "api/catalogue/v1/collections"
    url <- file.path(base_url, api_pth, endpoint)
    ret <- httr::GET(url)
    if(httr::status_code(ret) != 200){
        Insert.Messages.httr(ret)
        return(out)
    }
    ret <- httr::content(ret)
    start_d <- ret$extent$temporal$interval[[1]][[1]]
    start_d <- as.POSIXct(start_d, format = "%Y-%m-%dT%H:%M:%SZ", tz = "UTC")
    out$start <- format(start_d, "%Y-%m-%d %H:%M:%S")
    end_d <- ret$extent$temporal$interval[[1]][[2]]
    end_d <- as.POSIXct(end_d, format = "%Y-%m-%dT%H:%M:%SZ", tz = "UTC")
    out$end <- format(end_d, "%Y-%m-%d %H:%M:%S")

    return(out)
}

era5.cds.download <- function(GalParams, nbfile = 1, GUI = TRUE, verbose = TRUE){
    area <- GalParams$bbox[c('maxlat', 'minlon', 'minlat', 'maxlon')]
    area <- paste(unlist(area), collapse = "/")

    seqTime <- seq.format.date.time('hourly', GalParams$date.range, 1)
    if(is.null(seqTime)) return(NULL)
    ymd <- split(seqTime, format(seqTime, "%Y%m%d"))
    daty <- names(ymd)

    time_request <- lapply(ymd, function(x){
        list(
            year = format(x[1], "%Y"),
            month = format(x[1], "%m"),
            day = format(x[1], "%d"),
            time = format(x, "%H:%M")
        )
    })

    era5_prod <- switch(GalParams$src,
            "cds.climate.copernicus.eu - ERA5 - Hourly" = {
                list(pars.file = "era5_singleLev_options.csv",
                     resource = "reanalysis-era5-single-levels",
                     data.name = "ERA5 Hourly",
                     dir.name  = "ERA5_1Hr_SingleLevels",
                     dir.prefix = "ERA5")
            },
            "cds.climate.copernicus.eu - ERA5-Land - Hourly" = {
                list(pars.file = "era5_Land_options.csv",
                     resource = "reanalysis-era5-land",
                     data.name = "ERA5-Land Hourly",
                     dir.name  = "ERA5_1Hr_Land",
                     dir.prefix = "ERA5_Land")
            }, NULL)

    if(is.null(era5_prod)){
        Insert.Messages.Out("Unknown ERA5 resource", TRUE, "e")
        return(NULL)
    }

    opts <- get_reanalysis.variables(era5_prod$pars.file)
    opts <- opts[[GalParams$var]]

    opts$convert <- NULL
    if(!is.null(opts$units_fun)){
        uvar <- opts$varid[match(opts$units_var, opts$nc_name)]
        opts$convert <- list(fun = trimws(as.character(opts$units_fun)),
                             args = trimws(as.character(opts$units_args)),
                             var = uvar)
    }

    request <- list(
        dataset_short_name = "reanalysis-era5-single-levels",
        product_type = 'reanalysis',
        format = 'netcdf',
        download_format = 'unarchived',
        variable = opts$api_var,
        area = area
    )

    api_url <- "https://cds.climate.copernicus.eu/api"
    api_endpoints <- file.path(api_url, "retrieve/v1/processes", request$dataset_short_name, "execute")

    pars <- list(api_pat = GalParams$login$usr, request = request, api_url = api_url,
                 api_endpoints = api_endpoints, opts = opts)

    ######################

    outdir <- file.path(GalParams$dir2save, era5_prod$dir.name)
    outdir <- file.path(outdir, paste0(era5_prod$dir.prefix, "_", GalParams$var))
    dir.create(outdir, showWarnings = FALSE, recursive = TRUE)

    destfiles <- file.path(outdir, paste0(GalParams$var, "_", daty, ".nc"))

    ret <- cdt.download.data(time_request, destfiles, destfiles, nbfile, GUI,
                             verbose, era5_prod$data.name, era5.download.data,
                             pars = pars)
    return(ret)
}

era5.download.data <- function(lnk, dest, ncfl, pars){
    on.exit(unlink(dest))
    xx <- basename(dest)

    request <- c(pars$request, lnk[[1]])

    Insert.Messages.Out(paste("Sending request:", basename(ncfl), '...'), TRUE, "i")

    res <- era5.cds.send.request(pars$api_endpoints, pars$api_pat, request)
    if(is.null(res)) return(xx)

    resc <- httr::content(res)
    task_status <- resc$status
    if(task_status == "failed"){
        Insert.Messages.Out("INFO Request failed", TRUE, "e")
        Insert.Messages.Out(paste('Message:', resc$message), TRUE, "e")
        Insert.Messages.Out(paste('Reason', resc$reason), TRUE, "e")
        Insert.Messages.Out(paste('Detail', resc$detail), TRUE, "e")
        return(xx)
    }
    if(task_status == "queued"){
        Insert.Messages.Out("INFO Request is queued", TRUE, "i")
        Insert.Messages.Out("INFO Request is running ....", TRUE, "i")
    }
    if(task_status == "accepted"){
        Insert.Messages.Out("INFO Request is accepted", TRUE, "i")
        Insert.Messages.Out("INFO Request is running ....", TRUE, "i")
    }

    task_url <- resc$links[[2]]$href
    count <- 0

    while(task_status != "successful"){
        Sys.sleep(3)
        res <- era5.cds.retrieve.task(task_url, pars$api_pat)
        if(is.null(res)){
            if(count > 10){
                break
            }else{
                next
                count <- count + 1
            }
            
        }
        if(httr::status_code(res) > 300){
            if(count > 10){
                break
            }else{
                next
                count <- count + 1
            }
        }
        resc <- httr::content(res)
        task_status <- resc$status
        if(task_status == "successful"){
            task_url <- resc$links[[2]]$href
            job_url <- resc$links[[1]]$href
        }else{
            task_url <- resc$links[[1]]$href
        }
    }

    if(task_status != "successful") return(xx)

    res <- era5.cds.write.data(task_url, dest, pars$api_pat)
    if(is.null(res)) return(xx)

    ## delete task
    res <- era5.cds.delete.task(job_url, pars$api_pat)

    ret <- era5.cds.format.data(dest, pars$opts)
    if(ret == 0) xx <- NULL

    return(xx)
}

era5.cds.format.data <- function(ncfl, pars){
    nc <- ncdf4::nc_open(ncfl)
    lon <- nc$dim[['longitude']]$vals
    lat <- nc$dim[['latitude']]$vals
    time <- nc$dim[['valid_time']]$vals
    t_unit <- nc$dim[['valid_time']]$units
    val <- lapply(pars$varid, function(v){
        ncdf4::ncvar_get(nc, v, collapse_degen = FALSE)
    })
    names(val) <- pars$varid
    ncdf4::nc_close(nc)

    ncpars <- list(name = pars$nc_name, units = pars$nc_units,
                   longname = pars$nc_longname,
                   prec = "float", missval = -9999)

    units(time) <- units::as_units(t_unit)
    time <- as.POSIXct(time, tz = "UTC")
    hours <- format(time, "%Y%m%d%H")
    ncfiles <- paste0(pars$cdt_var, "_", hours, ".nc")
    ncfiles <- file.path(dirname(ncfl), ncfiles)

    ox <- order(lon)
    lon <- lon[ox]
    oy <- order(lat)
    lat <- lat[oy]

    val <- lapply(pars$varid, function(v){
        x <- val[[v]]
        x <- x[ox, oy, , drop = FALSE]
        x[is.nan(x)] <- NA
        if(!is.null(pars$convert)){
            if(v %in% pars$convert$var){
                ip <- pars$convert$var %in% v
                x <- eval_function(pars$convert$fun[ip], pars$convert$args[ip], x)
            }
        }
        x
    })
    names(val) <- pars$varid

    if(pars$cdt_var == "hum"){
        rh <- relative_humidity(val[['t2m']], val[['d2m']])
        spfh <- specific_humidity(val[['d2m']], val[['sp']])
        val <- c(val, list(rn = rh, q = spfh))
    }

    for(j in seq_along(ncfiles)){
        don <- lapply(val, function(v) v[, , j])
 
        if(length(don) == 1) don <- don[[1]]
        dat <- list(x = lon, y = lat, z = don)

        reanalysis.write.ncdf(dat, ncpars, ncfiles[j])
    }

    return(0)
}

era5.cds.send.request <- function(url_api, pat, request){
    httr_headers <- httr::add_headers("PRIVATE-TOKEN" = pat)
    res <- try(httr::VERB("POST", url_api, httr_headers,
                          body = list(inputs = request),
                          encode = "json"),
               silent = TRUE)
    if(inherits(res, "try-error")){
        Insert.Messages.Out(gsub('[\r\n]', '', res[1]), TRUE, "e")
        res <- NULL
    }
    if (httr::http_error(res)){
        rep <- httr::content(res)
        Insert.Messages.Out(paste('Failed: sending request to', rep$instance), TRUE, "e")
        Insert.Messages.Out(paste('Message:', rep$title), TRUE, "e")
        Insert.Messages.Out(paste('Detail', rep$detail), TRUE, "e")
        res <- NULL
    }

    return(res)
}

era5.cds.retrieve.task <- function(task_url, pat){
    httr_headers <- httr::add_headers("PRIVATE-TOKEN" = pat)
    res <- try(httr::GET(task_url, httr_headers, encode = "json"), silent = TRUE)
    if(inherits(res, "try-error")){
        Insert.Messages.Out(gsub('[\r\n]', '', res[1]), TRUE, "e")
        res <- NULL
    }
    if(httr::http_error(res)){
        rep <- httr::content(res)
        Insert.Messages.Out('Retrieving task failed', TRUE, "e")
        Insert.Messages.Out(paste('Message:', rep$title), TRUE, "e")
        Insert.Messages.Out(paste('Detail', rep$detail), TRUE, "e")
        res <- NULL
    }

    return(res)
}

era5.cds.get.nclink <- function(task_url, pat){
    httr_headers <- httr::add_headers("PRIVATE-TOKEN" = pat)
    res <- try(httr::GET(task_url, httr_headers, encode = "json"), silent = TRUE)
    if(inherits(res, "try-error")){
        Insert.Messages.Out(gsub('[\r\n]', '', res[1]), TRUE, "e")
        res <- NULL
    }
    if(httr::http_error(res)){
        rep <- httr::content(res)
        Insert.Messages.Out('Retrieving netcdf file failed', TRUE, "e")
        Insert.Messages.Out(paste('Message:', rep$title), TRUE, "e")
        Insert.Messages.Out(paste('Detail', rep$detail), TRUE, "e")
        res <- NULL
    }
    rep <- httr::content(res)
    res <- rep$asset$value$href
    return(res)
}

era5.cds.write.data <- function(task_url, dest, pat){
    nc_url <- era5.cds.get.nclink(task_url, pat)
    if(is.null(nc_url)) return(NULL)

    ## write to disk
    Insert.Messages.Out(paste("Downloading data:", basename(dest)), TRUE, "i")
    res <- httr::GET(nc_url, httr::write_disk(dest, overwrite = TRUE))
    if(httr::status_code(res) != 200){
        Insert.Messages.Out(paste("Downloading", basename(dest), "failed"), TRUE, "e")
        Insert.Messages.Out(httr::content(res, 'text'))
        return(NULL)
    }

    return(res)
}

era5.cds.delete.task <- function(job_url, pat){
    httr_headers <- httr::add_headers("PRIVATE-TOKEN" = pat)
    res <- try(httr::DELETE(job_url, httr_headers), silent = TRUE)
    if(inherits(res, "try-error")){
        Insert.Messages.Out(gsub('[\r\n]', '', res[1]), TRUE, "e")
        res <- NULL
    }
    if(httr::http_error(res)){
        rep <- httr::content(res)
        Insert.Messages.Out('Deleting task failed', TRUE, "w")
        Insert.Messages.Out(paste('Detail:', rep$detail), TRUE, "w")
        res <- NULL
    }

    return(res)
}
