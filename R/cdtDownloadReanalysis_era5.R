
era5.download.cds <- function(GalParams, nbfile = 1, GUI = TRUE, verbose = TRUE){
    ## time out in second
    timeout <- 60
    url <- "https://cds.climate.copernicus.eu"

    api_key <- era5.cds.get.apikey(url, GalParams$login$usr, GalParams$login$pwd)
    if(!is.list(api_key)) return(api_key)

    area <- GalParams$bbox[c('maxlat', 'minlon', 'minlat', 'maxlon')]
    area <- paste(unlist(area), collapse = "/")

    start <- GalParams$date.range[paste0('start.', c('year', 'mon', 'day', 'hour'))]
    start <- era5.start.end.time(start)
    end <- GalParams$date.range[paste0('end.', c('year', 'mon', 'day', 'hour'))]
    end <- era5.start.end.time(end)

    varname <- switch(GalParams$var,
                      "tmax" = "maximum_2m_temperature_since_previous_post_processing",
                      "tmin" = "minimum_2m_temperature_since_previous_post_processing",
                      "tmean" = "2m_temperature",
                      NULL)

    seqTime <- seq(start, end, "hour")
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

    request <- list(
        product_type = 'reanalysis',
        format = 'netcdf',
        variable = varname,
        area = area
    )

    api_url <- paste(url, "api", "v2", sep = "/")
    api_endpoints <- paste(api_url, "resources", "reanalysis-era5-single-levels", sep = "/")

    pars <- list(api_key = api_key, request = request, api_url = api_url,
    			 api_endpoints = api_endpoints, timeout = timeout)

    ######################
    data.name <- "ERA-5 Hourly"
    outdir <- file.path(GalParams$dir2save, paste0("ERA5_hourly_cds_", GalParams$var))
    dir.create(outdir, showWarnings = FALSE, recursive = TRUE)

    destfiles <- file.path(outdir, paste0(GalParams$var, "_", daty, ".nc"))

    ######################

    ret <- cdt.download.data(time_request, destfiles, destfiles, nbfile, GUI,
                             verbose, data.name, era5.download.data, pars = pars)

    return(ret)
}

#################################################################################

era5.download.data <- function(lnk, dest, ncfl, pars){
    on.exit(unlink(dest))
    xx <- basename(dest)

    request <- c(pars$request, lnk[[1]])
    user <- pars$api_key$uid
    key <- pars$api_key$key

    res <- era5.cds.send.request(pars$api_endpoints, user, key, request)
    if(is.null(res)) return(xx)

    resc <- httr::content(res)
    task_status <- resc$state

    if(task_status == "failed") return(xx)
    task_url <- paste(pars$api_url, "tasks", resc$request_id, sep = "/")

    ## time out
    systime <- Sys.time()

    while(task_status != "completed"){
        Sys.sleep(1)

        res <- era5.cds.retrieve.task(task_url, user, key)
        if(is.null(res)) break
        if(httr::status_code(res) > 300) break

        resc <- httr::content(res)
        task_status <- resc$state

        timeout <- difftime(Sys.time(), systime, units = "secs")
        if(timeout > pars$timeout) break
    }

    if(task_status != "completed") return(xx)

    ## write to disk
    res <- httr::GET(resc$location, httr::write_disk(dest, overwrite = TRUE))
    if(httr::status_code(res) != 200) return(xx)

    ## delete task
    res <- era5.cds.delete.task(task_url, user, key)
    # if(is.null(res)) return(NULL)
    # if(httr::status_code(res) != 204) return(NULL)

    ret <- era5.format.data(dest)
    if(ret == 0) xx <- NULL

    return(xx)
}

era5.format.data <- function(ncfl){
    nc <- ncdf4::nc_open(ncfl)
    varid <- nc$var[[1]]$name
    lon <- nc$dim[['longitude']]$vals
    lat <- nc$dim[['latitude']]$vals
    time <- nc$dim[['time']]$vals
    t_unit <- nc$dim[['time']]$units
    val <- ncdf4::ncvar_get(nc, varid)
    ncdf4::nc_close(nc)

    info <- switch(varid,
                   'mx2t' = list(name = 'tmax', units = 'degC',
                                 longname = 'Maximum temperature at 2 meter'),
                   'mn2t' = list(name = 'tmin', units = 'degC',
                                   longname = 'Minimum temperature at 2 meter'),
                   't2m' = list(name = 'tmean', units = 'degC',
                                    longname = 'Average temperature at 2 meter'),
                    NULL
                  )
    missval <- -99

    units(time) <- units::as_units(t_unit)
    time <- as.POSIXct(time, tz = "UTC")
    hours <- format(time, "%Y%m%d%H")
    ncfiles <- paste0(info$name, "_", hours, ".nc")
    ncfiles <- file.path(dirname(ncfl), ncfiles)

    ox <- order(lon)
    lon <- lon[ox]
    oy <- order(lat)
    lat <- lat[oy]

    dx <- ncdf4::ncdim_def("lon", "degrees_east", lon, longname = "longitude")
    dy <- ncdf4::ncdim_def("lat", "degrees_north", lat, longname = "latitude")
    ncgrd <- ncdf4::ncvar_def(info$name, info$units, list(dx, dy), missval,
                              info$longname, "float", compression = 6)

    val <- val - 273.15
    val <- val[ox, oy, ]
    val[is.na(val)] <- missval

    for(j in seq_along(ncfiles)){
        nc <- ncdf4::nc_create(ncfiles[j], ncgrd)
        ncdf4::ncvar_put(nc, ncgrd, val[, , j])
        ncdf4::nc_close(nc)
    }
    
    return(0)
}

era5.cds.get.apikey <- function(url, usr, pwd){
    url_user <- paste0(url, "/user/login?destination=user")
    url_api <- paste0(url, "/api/v2.ui/users/me")

    page_session <- rvest::html_session(url_user)
    page_form <- rvest::html_form(page_session)[[1]]
    fill_form <- rvest::set_values(page_form, name = usr, pass = pwd)
    session <- rvest::submit_form(page_session, fill_form)
    if(httr::status_code(session) != 200) return(-3)

    apikey <- rvest::jump_to(session, url_api)
    if(httr::status_code(apikey) != 200) return(-4)

    objkey <- readBin(apikey$response$content, what = "json")
    objkey <- jsonlite::fromJSON(objkey)

    list(uid = objkey$uid, key = objkey$api_key)
}

era5.start.end.time <- function(x){
    x <- paste(unlist(x), collapse = "-")
    as.POSIXct(x, tz = "UTC", format = "%Y-%m-%d-%H")
}

era5.cds.send.request <- function(url_api, user, key, request){
    res <- httr::POST(
                      url_api,
                      httr::authenticate(user, key),
                      httr::add_headers("Accept" = "application/json",
                                        "Content-Type" = "application/json"),
                      body = request,
                      encode = "json"
                    )
    if (httr::http_error(res)) res <- NULL

    return(res)
}

era5.cds.retrieve.task <- function(task_url, user, key){
    res <- httr::GET(
                     task_url,
                     httr::authenticate(user, key),
                     httr::add_headers("Accept" = "application/json",
                                       "Content-Type" = "application/json")
                    )
    if(httr::http_error(res)) res <- NULL

    return(res)
}

era5.cds.delete.task <- function(task_url, user, key){
    res <- httr::DELETE(
                        task_url,
                        httr::authenticate(user, key),
                        httr::add_headers("Accept" = "application/json",
                                          "Content-Type" = "application/json")
                      )
    if(httr::http_error(res)) res <- NULL

    return(res)
}
