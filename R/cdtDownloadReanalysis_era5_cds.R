#' Open CDS profile web page.
#'
#' Open Climate Data Store profile web page in the default browser.
#' 
#' @export

open_cds_profile <- function(){
    url <- "https://cds.climate.copernicus.eu/profile"
    utils::browseURL(url)
}

#' Save CDS personal access token.
#'
#' Save CDS personal access token into a local file.
#'
#' @param key CDS personal access token.
#' 
#' @examples
#' 
#' \dontrun{
#' library(CDT)
#' save_cds_personal_token('10xasdxf2-ws12a-xdfs-zx3x-z1z2z3z4z5z')
#' }
#' 
#' @export

save_cds_personal_token <- function(key){
    if(missing(key)){
        stop("The personal access token is not supplied.")
    }
    fileL <- file.path(.cdtDir$dirLocal, "config", "auth")
    if(!file.exists(fileL)){
        auth <- new.env()
        auth$era5$pat <- key
    }else{
        auth <- readRDS(fileL)
        auth$era5$pat <- key
    }
    saveRDS(auth, fileL)
}

era5.cds.coverage <- function(GalParams){
    if(GalParams$src == "cds.climate.copernicus.eu - ERA5 - Hourly"){
        r_name <- "Fifth generation ECMWF reanalysis (ERA5) on single levels"
        endpoint <- "reanalysis-era5-single-levels"
        start <- '1940-01-01 00:00:00'
    }else{
        r_name <- "Fifth generation ECMWF reanalysis (ERA5-Land)"
        endpoint <- "reanalysis-era5-land"
        start <- '1950-01-01 00:00:00'
    }
    out <- list(name = r_name, timestep = "hourly", start = start)
    base_url <- "https://cds.climate.copernicus.eu"
    api_pth <- "api/v2.ui/resources"
    url <- file.path(base_url, api_pth, endpoint)
    ret <- httr::GET(url)
    if(httr::status_code(ret) != 200){
        Insert.Messages.httr(ret)
        return(out)
    }
    ret <- httr::content(ret)
    end_d <- ret$structured_data$temporalCoverage
    end_d <- strsplit(end_d, '/')
    end_d <- paste(end_d[[1]][2], '23:00:00')
    out$end <- end_d

    return(out)
}

era5.cds.download <- function(GalParams, nbfile = 1, GUI = TRUE, verbose = TRUE){
    url <- "https://cds.climate.copernicus.eu"
    api_url <- paste(url, "api", "v2", sep = "/")
    api_key <- era5.cds.get.apikey(url, GalParams$login$usr, GalParams$login$pwd)
    if(is.null(api_key)) return(NULL)

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
        opts$convert <- list(fun = as.character(opts$units_fun),
                             args = as.character(opts$units_args))
    }

    request <- list(
        product_type = 'reanalysis',
        format = 'netcdf',
        variable = opts$api_var,
        area = area
    )

    api_url <- paste(url, "api", "v2", sep = "/")
    api_endpoints <- paste(api_url, "resources", era5_prod$resource, sep = "/")

    pars <- list(api_key = api_key, request = request, api_url = api_url,
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
    user <- pars$api_key$uid
    key <- pars$api_key$key

    Insert.Messages.Out(paste("Sending request:", basename(ncfl), '...'), TRUE, "i")

    res <- era5.cds.send.request(pars$api_endpoints, user, key, request)
    if(is.null(res)) return(xx)

    resc <- httr::content(res)
    task_status <- resc$state
    if(task_status == "failed"){
        Insert.Messages.Out("INFO Request failed", TRUE, "e")
        Insert.Messages.Out(paste('Message:', resc$message), TRUE, "e")
        Insert.Messages.Out(paste('Reason', resc$reason), TRUE, "e")
        return(xx)
    }
    if(task_status == "queued"){
        Insert.Messages.Out("INFO Request is queued", TRUE, "i")
        Insert.Messages.Out("INFO Request is running ....", TRUE, "i")
    }
    task_url <- paste(pars$api_url, "tasks", resc$request_id, sep = "/")

    while(task_status != "completed"){
        Sys.sleep(5)
        res <- era5.cds.retrieve.task(task_url, user, key)
        if(is.null(res)) break
        resc <- httr::content(res)
        if(httr::status_code(res) > 300) break
        task_status <- resc$state
    }

    if(task_status != "completed") return(xx)

    ## write to disk
    Insert.Messages.Out(paste("Downloading data:", basename(ncfl)), TRUE, "i")
    res <- httr::GET(resc$location, httr::write_disk(dest, overwrite = TRUE))
    if(httr::status_code(res) != 200){
        Insert.Messages.Out(paste("Downloading", basename(ncfl), "failed"), TRUE, "e")
        Insert.Messages.Out(httr::content(res,'text'))
        return(xx)
    }

    ## delete task
    res <- era5.cds.delete.task(task_url, user, key)
    ret <- era5.cds.format.data(dest, pars$opts)
    if(ret == 0) xx <- NULL

    return(xx)
}

era5.cds.format.data <- function(ncfl, pars){
    nc <- ncdf4::nc_open(ncfl)
    lon <- nc$dim[['longitude']]$vals
    lat <- nc$dim[['latitude']]$vals
    time <- nc$dim[['time']]$vals
    t_unit <- nc$dim[['time']]$units
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

    val <- lapply(val, function(v){
        x <- v[ox, oy, , drop = FALSE]
        x[is.nan(x)] <- NA
        if(!is.null(pars$convert)){
            x <- eval_function(pars$convert$fun, pars$convert$args, x)
        }
        x
    })

    if(pars$cdt_var == "hum"){
        val[['t2m']] <- val[['t2m']] - 273.15
        val[['d2m']] <- val[['d2m']] - 273.15
        val[['sp']] <- val[['sp']] / 100

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

era5.cds.get.apikey <- function(url, usr, pwd){
    url_user <- paste0(url, "/user/login?destination=user")
    url_api <- paste0(url, "/api/v2.ui/users/me")

    page_session <- rvest::html_session(url_user)
    page_form <- rvest::html_form(page_session)[[1]]
    fill_form <- rvest::set_values(page_form, name = usr, pass = pwd)
    session <- rvest::submit_form(page_session, fill_form)
    if(httr::status_code(session) != 200){
        rep <- rawToChar(session$response$content)
        Insert.Messages.Out(rep)
        return(NULL)
    }

    apikey <- rvest::jump_to(session, url_api)
    if(httr::status_code(apikey) != 200){
        rep <- rawToChar(apikey$response$content)
        rep <- jsonlite::fromJSON(rep)
        Insert.Messages.Out(paste('Failed to get api key', rep$url), TRUE, "e")
        Insert.Messages.Out(paste('Message:', rep$message), TRUE, "e")
        Insert.Messages.Out(paste('Reason', rep$reason), TRUE, "e")
        return(NULL)
    }

    objkey <- readBin(apikey$response$content, what = "json")
    objkey <- jsonlite::fromJSON(objkey)

    list(uid = objkey$uid, key = objkey$api_key)
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
    if (httr::http_error(res)){
        rep <- httr::content(res)
        Insert.Messages.Out(paste('Failed: sending request to', rep$url), TRUE, "e")
        Insert.Messages.Out(paste('Message:', rep$message), TRUE, "e")
        Insert.Messages.Out(paste('Reason', rep$reason), TRUE, "e")
        res <- NULL
    }

    return(res)
}

era5.cds.retrieve.task <- function(task_url, user, key){
    res <- httr::GET(
                     task_url,
                     httr::authenticate(user, key),
                     httr::add_headers("Accept" = "application/json",
                                       "Content-Type" = "application/json")
                    )
    if(httr::http_error(res)){
        rep <- httr::content(res)
        Insert.Messages.Out('Retrieving task failed', TRUE, "e")
        Insert.Messages.Out(paste('Message:', rep$message), TRUE, "e")
        Insert.Messages.Out(paste('Reason', rep$reason), TRUE, "e")
        res <- NULL
    }

    return(res)
}

era5.cds.delete.task <- function(task_url, user, key){
    res <- httr::DELETE(
                        task_url,
                        httr::authenticate(user, key),
                        httr::add_headers("Accept" = "application/json",
                                          "Content-Type" = "application/json")
                      )
    if(httr::http_error(res)){
        rep <- httr::content(res)
        Insert.Messages.Out('Deleting task failed', TRUE, "w")
        Insert.Messages.Out(paste('Message:', rep$message), TRUE, "w")
        Insert.Messages.Out(paste('Reason', rep$reason), TRUE, "w")
        res <- NULL
    }

    return(res)
}
