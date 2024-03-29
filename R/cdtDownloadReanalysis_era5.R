
era5.cds.download <- function(GalParams, nbfile = 1, GUI = TRUE, verbose = TRUE){
    url <- "https://cds.climate.copernicus.eu"
    api_url <- paste(url, "api", "v2", sep = "/")
    api_key <- era5.cds.get.apikey(url, GalParams$login$usr, GalParams$login$pwd)
    if(is.null(api_key)) return(NULL)

    area <- GalParams$bbox[c('maxlat', 'minlon', 'minlat', 'maxlon')]
    area <- paste(unlist(area), collapse = "/")

    start <- GalParams$date.range[paste0('start.', c('year', 'mon', 'day', 'hour'))]
    start <- hourly.start.end.time(start)
    end <- GalParams$date.range[paste0('end.', c('year', 'mon', 'day', 'hour'))]
    end <- hourly.start.end.time(end)

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

    era5_prod <- switch(GalParams$src,
            "cds.climate.copernicus.eu - ERA5 - Hourly" = {
                list(pars.fun = era5.singleLev.variables,
                     resource = "reanalysis-era5-single-levels",
                     data.name = "ERA5 Hourly",
                     dir.name  = "ERA5_1Hr_SingleLevels",
                     dir.prefix = "ERA5")
            },
            "cds.climate.copernicus.eu - ERA5-Land - Hourly" = {
                list(pars.fun = era5.Land.variables,
                     resource = "reanalysis-era5-land",
                     data.name = "ERA5-Land Hourly",
                     dir.name  = "ERA5_Land_1Hr_SingleLevels",
                     dir.prefix = "ERA5_Land")
            }, NULL)

    if(is.null(era5_prod)){
        Insert.Messages.Out("Unknown ERA5 resource", TRUE, "e")
        return(NULL)
    }

    era5_params <- era5_prod$pars.fun(GalParams$var)

    request <- list(
        product_type = 'reanalysis',
        format = 'netcdf',
        variable = era5_params$cds_variable,
        area = area
    )

    api_url <- paste(url, "api", "v2", sep = "/")
    api_endpoints <- paste(api_url, "resources", era5_prod$resource, sep = "/")

    pars <- list(api_key = api_key, request = request, api_url = api_url,
                 api_endpoints = api_endpoints, nc = era5_params$ncpars)

    ######################

    outdir <- file.path(GalParams$dir2save, era5_prod$dir.name)
    dir.create(outdir, showWarnings = FALSE, recursive = TRUE)

    outdir <- file.path(outdir, paste0(era5_prod$dir.prefix, "_", GalParams$var))
    dir.create(outdir, showWarnings = FALSE, recursive = TRUE)

    destfiles <- file.path(outdir, paste0(GalParams$var, "_", daty, ".nc"))

    ######################

    ret <- cdt.download.data(time_request, destfiles, destfiles, nbfile, GUI,
                             verbose, era5_prod$data.name, era5.download.data,
                             pars = pars)

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

    ret <- era5.format.data(dest, pars$nc)
    if(ret == 0) xx <- NULL

    return(xx)
}

era5.format.data <- function(ncfl, pars){
    nc <- ncdf4::nc_open(ncfl)
    lon <- nc$dim[['longitude']]$vals
    lat <- nc$dim[['latitude']]$vals
    time <- nc$dim[['time']]$vals
    t_unit <- nc$dim[['time']]$units
    val <- lapply(pars$era_name, function(v) ncdf4::ncvar_get(nc, v))
    names(val) <- pars$era_name
    ncdf4::nc_close(nc)

    units(time) <- units::as_units(t_unit)
    time <- as.POSIXct(time, tz = "UTC")
    hours <- format(time, "%Y%m%d%H")
    ncfiles <- paste0(pars$var, "_", hours, ".nc")
    ncfiles <- file.path(dirname(ncfl), ncfiles)

    ox <- order(lon)
    lon <- lon[ox]
    oy <- order(lat)
    lat <- lat[oy]

    val <- lapply(val, function(v){
        x <- v[ox, oy, ]
        x[is.nan(x)] <- NA

        if(!is.null(pars$convert)){
            pars$convert$args <- c(list(x), pars$convert$args)
            x <- do.call(pars$convert$fun, pars$convert$args)
        }

        x
    })

    if(pars$var == "hum"){
        val[['t2m']] <- val[['t2m']] - 273.15
        val[['d2m']] <- val[['d2m']] - 273.15
        val[['sp']] <- val[['sp']] / 100

        rh <- relative_humidity(val[['t2m']], val[['d2m']])
        spfh <- specific_humidity(val[['d2m']], val[['sp']])

        val <- c(list(rn = rh, q = spfh), val)
    }

    for(j in seq_along(ncfiles)){
        don <- lapply(val, function(v) v[, , j])
 
        if(length(don) == 1) don <- don[[1]]
        dat <- list(x = lon, y = lat, z = don)

        reanalysis.write.ncdf(dat, pars, ncfiles[j])
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

#################################################################################

era5.singleLev.variables <- function(var_params){
    era5_var <- switch(var_params,
                       "tmax" = list(api = "maximum_2m_temperature_since_previous_post_processing",
                                     nc = "mx2t"),
                       "tmin" = list(api = "minimum_2m_temperature_since_previous_post_processing",
                                     nc = "mn2t"),
                       "tair" = list(api = "2m_temperature", nc = "t2m"),
                       "wind" = list(api = c("10m_u_component_of_wind", "10m_v_component_of_wind"),
                                     nc = c("u10", "v10")),
                       "hum" = list(api = c("2m_temperature", "2m_dewpoint_temperature", "surface_pressure"),
                                            nc = c("t2m", "d2m", "sp")),
                       "pres" = list(api = "surface_pressure", nc = "sp"),
                       "prmsl" = list(api = "mean_sea_level_pressure", nc = "msl"),
                       "cloud" = list(api = c("total_cloud_cover", "high_cloud_cover", "medium_cloud_cover",
                                              "low_cloud_cover", "cloud_base_height"),
                                            nc = c("tcc", "hcc", "mcc", "lcc", "cbh")),
                       "rad_avg" = list(api = c("mean_surface_downward_long_wave_radiation_flux_clear_sky",
                                                "mean_surface_downward_short_wave_radiation_flux_clear_sky",
                                                "mean_surface_direct_short_wave_radiation_flux_clear_sky",
                                                "mean_surface_net_long_wave_radiation_flux_clear_sky",
                                                "mean_surface_net_short_wave_radiation_flux_clear_sky",
                                                "mean_top_net_long_wave_radiation_flux_clear_sky",
                                                "mean_top_net_short_wave_radiation_flux_clear_sky",
                                                "mean_surface_downward_short_wave_radiation_flux",
                                                "mean_surface_downward_long_wave_radiation_flux",
                                                "mean_surface_net_short_wave_radiation_flux",
                                                "mean_surface_net_long_wave_radiation_flux",
                                                "mean_top_net_short_wave_radiation_flux",
                                                "mean_top_net_long_wave_radiation_flux",
                                                "mean_top_downward_short_wave_radiation_flux",
                                                "mean_surface_direct_short_wave_radiation_flux"),
                                        nc = c("msdwlwrfcs", "msdwswrfcs", "msdrswrfcs", "msnlwrfcs",
                                               "msnswrfcs", "mtnlwrfcs", "mtnswrfcs", "msdwswrf",
                                               "msdwlwrf", "msnswrf", "msnlwrf", "mtnswrf",
                                               "mtnlwrf", "mtdwswrf", "msdrswrf")),
                       "rad_acc" = list(api = c('clear_sky_direct_solar_radiation_at_surface',
                                                'surface_net_solar_radiation_clear_sky',
                                                'surface_net_thermal_radiation_clear_sky',
                                                'surface_solar_radiation_downward_clear_sky',
                                                'surface_thermal_radiation_downward_clear_sky',
                                                'top_net_solar_radiation_clear_sky',
                                                'top_net_thermal_radiation_clear_sky',
                                                'surface_net_solar_radiation',
                                                'surface_net_thermal_radiation',
                                                'surface_solar_radiation_downwards',
                                                'surface_thermal_radiation_downwards',
                                                'top_net_solar_radiation',
                                                'top_net_thermal_radiation',
                                                'total_sky_direct_solar_radiation_at_surface'),
                                        nc = c("cdir", "ssrc", "strc", "ssrdc", "strdc", "tsrc", "ttrc",
                                               "ssr", "str", "ssrd", "strd", "tsr", "ttr", "fdir")),
                       "prcp" = list(api = c("total_precipitation", "large_scale_precipitation",
                                             "convective_precipitation"),
                                     nc = c("tp", "lsp", "cp")),
                       "evp" = list(api = "evaporation", nc = "e"),
                       "pet" = list(api = "potential_evaporation", nc = "pev"),
                       "runoff" = list(api = c("runoff", "surface_runoff", "sub_surface_runoff"),
                                       nc = c("ro", "sro", "ssro")),
                       "soilm" = list(api = c('volumetric_soil_water_layer_1', 'volumetric_soil_water_layer_2',
                                              'volumetric_soil_water_layer_3', 'volumetric_soil_water_layer_4'),
                                      nc = c('swvl1', 'swvl2', 'swvl3', 'swvl4')),
                       "soilt" = list(api = c('soil_temperature_level_1', 'soil_temperature_level_2',
                                              'soil_temperature_level_3', 'soil_temperature_level_4'),
                                      nc = c('stl1', 'stl2', 'stl3', 'stl4')),
                       "tsg" = list(api = "skin_temperature", nc = "skt"),
                       "heat_avg" = list(api = c('mean_surface_latent_heat_flux',
                                                 'mean_surface_sensible_heat_flux'),
                                         nc = c('mslhf', 'msshf')),
                       "heat_acc" = list(api = c('surface_latent_heat_flux',
                                                 'surface_sensible_heat_flux'),
                                         nc = c('slhf', 'sshf')),
                      NULL)

    longname <- switch(var_params,
                       "tmax" = "Maximum temperature at 2 meter",
                       "tmin" = "Minimum temperature at 2 meter",
                       "tair" = "Air temperature at 2 meter",
                       "wind" = c("U-wind at 10 m above ground",
                                  "V-wind at 10 m above ground"),
                       "hum" = c("Relative humidity at 2 meter",
                                 "Specific humidity at 2 meter",
                                 "Air temperature at 2 meter",
                                 "Dewpoint temperature at 2 meter",
                                 "Surface pressure"),
                       "pres" = "Surface pressure",
                       "prmsl" = "Mean sea level pressure",
                       "cloud" = c("Total cloud cover", "High cloud cover",
                                   "Medium cloud cover", "Low cloud cover",
                                   "Cloud base height"),
                       "rad_avg" = c("Clear sky mean surface downward longwave radiation flux",
                                 "Clear sky mean surface downward shortwave radiation flux",
                                 "Clear sky mean surface direct shortwave radiation flux",
                                 "Clear sky mean surface net longwave radiation flux",
                                 "Clear sky mean surface net shortwave radiation flux",
                                 "Clear sky mean top net longwave radiation flux",
                                 "Clear sky mean top net shortwave radiation flux",
                                 "Mean surface downward shortwave radiation flux",
                                 "Mean surface downward longwave radiation flux",
                                 "Mean surface net shortwave radiation flux",
                                 "Mean surface net longwave radiation flux",
                                 "Mean top net shortwave radiation flux",
                                 "Mean top net longwave radiation flux",
                                 "Mean top downward shortwave radiation flux",
                                 "Mean surface direct shortwave radiation flux"),
                       "rad_acc" = c("Clear sky direct solar radiation at surface",
                                     "Clear sky surface net solar radiation",
                                     "Clear sky surface net thermal radiation",
                                     "Clear sky surface solar radiation downward",
                                     "Clear sky surface thermal radiation downward",
                                     "Clear sky top net solar radiation",
                                     "Clear sky top net thermal radiation",
                                     "Surface net solar radiation",
                                     "Surface net thermal radiation",
                                     "Surface solar radiation downwards",
                                     "Surface thermal radiation downwards",
                                     "Top net solar radiation",
                                     "Top net thermal radiation",
                                     "Total sky direct solar radiation at surface"),
                       "prcp" = c("Total precipitation",
                                  "Large-scale precipitation",
                                  "Convective precipitation"),
                       "evp" = "Evaporation",
                       "pet" = "Potential evaporation",
                       "runoff" = c("Runoff", "Surface runoff", "Sub-surface runoff"),
                       "soilm" = c("Volumetric soil water layer 1, 0 - 7cm",
                                   "Volumetric soil water layer 2, 7 - 28cm",
                                   "Volumetric soil water layer 3, 28 - 100cm",
                                   "Volumetric soil water layer 4, 100 - 289cm"),
                       "soilt" = c("Soil temperature level 1, 0 - 7cm",
                                   "Soil temperature level 2, 7 - 28cm",
                                   "Soil temperature level 3, 28 - 100cm",
                                   "Soil temperature level 4, 100 - 289cm"),
                       "tsg" = "Skin temperature",
                       "heat_avg" = c("Mean surface latent heat flux",
                                     "Mean surface sensible heat flux"),
                       "heat_acc" = c("Surface latent heat flux",
                                     "Surface sensible heat flux"),
                       NULL)

    units <- switch(var_params,
                    "tmax" = "degC",
                    "tmin" = "degC",
                    "tair" = "degC",
                    "wind" = c('m/s', 'm/s'),
                    "hum" = c('%', 'kg/kg', 'degC', 'degC', 'hPa'),
                    "pres" = 'hPa',
                    "prmsl" = 'hPa',
                    "cloud" = c('fraction', 'fraction', 'fraction', 'fraction', 'm'),
                    "rad_avg" = c("W/m2", "W/m2", "W/m2", "W/m2",
                                  "W/m2", "W/m2", "W/m2", "W/m2",
                                  "W/m2", "W/m2", "W/m2", "W/m2",
                                  "W/m2", "W/m2", "W/m2"),
                    "rad_acc" = c("J/m2", "J/m2", "J/m2", "J/m2",
                                  "J/m2", "J/m2", "J/m2", "J/m2",
                                  "J/m2", "J/m2", "J/m2", "J/m2",
                                  "J/m2", "J/m2"),
                    "prcp" = c("mm", "mm", "mm"),
                    "evp" = "mm",
                    "pet" = "mm",
                    "runoff" = c("mm", "mm", "mm"),
                    "soilm" = c("m3/m3", "m3/m3", "m3/m3", "m3/m3"),
                    "soilt" = c("degC", "degC", "degC", "degC"),
                    "tsg" = "degC",
                    "heat_avg" = c("W/m2", "W/m2"),
                    "heat_acc" = c("J/m2", "J/m2"),
                    NULL)

    convert_units <- switch(var_params,
                          "tmax" = list(fun = "-", args = list(273.15)),
                          "tmin" = list(fun = "-", args = list(273.15)),
                          "tair" = list(fun = "-", args = list(273.15)),
                          "wind" = NULL,
                          "hum" = NULL,
                          "pres" = list(fun = "/", args = list(100)),
                          "prmsl" = list(fun = "/", args = list(100)),
                          "cloud" = NULL,
                          "rad_avg" = NULL,
                          "rad_acc" = NULL,
                          "prcp" = list(fun = "*", args = list(1000)),
                          "evp" = list(fun = "*", args = list(1000)),
                          "pet" = list(fun = "*", args = list(1000)),
                          "runoff" = list(fun = "*", args = list(1000)),
                          "soilm" = NULL,
                          "soilt" = list(fun = "-", args = list(273.15)),
                          "tsg" = list(fun = "-", args = list(273.15)),
                          "heat_avg" = NULL,
                          "heat_acc" = NULL,
                          NULL)

    name <- switch(var_params,
                   "wind" = c('ugrd', 'vgrd'),
                   "hum" = c('rh', 'q', 'tm', 'td', 'pr'),
                   "cloud" = c("tcc", "hcc", "mcc", "lcc", "cbh"),
                   "rad_avg" = c("msdwlwrfcs", "msdwswrfcs", "msdrswrfcs", "msnlwrfcs",
                                 "msnswrfcs", "mtnlwrfcs", "mtnswrfcs", "msdwswrf",
                                 "msdwlwrf", "msnswrf", "msnlwrf", "mtnswrf",
                                 "mtnlwrf", "mtdwswrf", "msdrswrf"),
                   "rad_acc" = c("cdir", "ssrc", "strc", "ssrdc", "strdc", "tsrc", "ttrc",
                                 "ssr", "str", "ssrd", "strd", "tsr", "ttr", "fdir"),
                   "prcp" = c('ptot', 'plrgscl', 'pconv'),
                   "pet" = "pev",
                   "runoff" = c("ro", "sro", "ssro"),
                   "soilm" = c('swvl1', 'swvl2', 'swvl3', 'swvl4'),
                   "soilt" = c('stl1', 'stl2', 'stl3', 'stl4'),
                   "heat_avg" = c('mslhf', 'msshf'),
                   "heat_acc" = c('slhf', 'sshf'),
                   var_params)

    ncpars <- list(name = name, units = units, longname = longname,
                   prec = "float", missval = -9999, convert = convert_units,
                   era_name = era5_var$nc, var = var_params)

    list(ncpars = ncpars, cds_variable = era5_var$api)
}

era5.Land.variables <- function(var_params){
    era5_var <- switch(var_params,
                       "tair" = list(api = "2m_temperature", nc = "t2m"),
                       "wind" = list(api = c("10m_u_component_of_wind", "10m_v_component_of_wind"),
                                     nc = c("u10", "v10")),
                       "hum" = list(api = c("2m_temperature", "2m_dewpoint_temperature", "surface_pressure"),
                                            nc = c("t2m", "d2m", "sp")),
                       "pres" = list(api = "surface_pressure", nc = "sp"),
                       "rad_acc" = list(api = c("surface_net_solar_radiation",
                                                "surface_net_thermal_radiation",
                                                "surface_solar_radiation_downwards",
                                                "surface_thermal_radiation_downwards"),
                                        nc = c("ssr", "str", "ssrd", "strd")),
                       "prcp" = list(api = "total_precipitation", nc = "tp"),
                       "evp" = list(api = "total_evaporation", nc = "e"),
                       "pet" = list(api = "potential_evaporation", nc = "pev"),
                       "runoff" = list(api = c("runoff", "surface_runoff", "sub_surface_runoff"),
                                       nc = c("ro", "sro", "ssro")),
                       "soilm" = list(api = c('volumetric_soil_water_layer_1', 'volumetric_soil_water_layer_2',
                                              'volumetric_soil_water_layer_3', 'volumetric_soil_water_layer_4'),
                                      nc = c('swvl1', 'swvl2', 'swvl3', 'swvl4')),
                       "soilt" = list(api = c('soil_temperature_level_1', 'soil_temperature_level_2',
                                              'soil_temperature_level_3', 'soil_temperature_level_4'),
                                      nc = c('stl1', 'stl2', 'stl3', 'stl4')),
                       "tsg" = list(api = "skin_temperature", nc = "skt"),
                       "heat_acc" = list(api = c('surface_latent_heat_flux',
                                                 'surface_sensible_heat_flux'),
                                         nc = c('slhf', 'sshf')),
                        NULL)

    longname <- switch(var_params,
                       "tair" = "Air temperature at 2 meter",
                       "wind" = c("U-wind at 10 m above ground",
                                  "V-wind at 10 m above ground"),
                       "hum" = c("Relative humidity at 2 meter",
                                 "Specific humidity at 2 meter",
                                 "Air temperature at 2 meter",
                                 "Dewpoint temperature at 2 meter",
                                 "Surface pressure"),
                       "pres" = "Surface pressure",
                       "rad_acc" = c("Surface net solar radiation",
                                     "Surface net thermal radiation",
                                     "Surface solar radiation downwards",
                                     "Surface thermal radiation downwards"),
                       "prcp" = "Total precipitation",
                       "evp" = "Total evaporation",
                       "pet" = "Potential evaporation",
                       "runoff" = c("Runoff", "Surface runoff", "Sub-surface runoff"),
                       "soilm" = c("Volumetric soil water layer 1, 0 - 7cm",
                                   "Volumetric soil water layer 2, 7 - 28cm",
                                   "Volumetric soil water layer 3, 28 - 100cm",
                                   "Volumetric soil water layer 4, 100 - 289cm"),
                       "soilt" = c("Soil temperature level 1, 0 - 7cm",
                                   "Soil temperature level 2, 7 - 28cm",
                                   "Soil temperature level 3, 28 - 100cm",
                                   "Soil temperature level 4, 100 - 289cm"),
                       "tsg" = "Skin temperature",
                       "heat_acc" = c("Surface latent heat flux",
                                     "Surface sensible heat flux"),
                       NULL)

    units <- switch(var_params,
                    "tair" = "degC",
                    "wind" = c('m/s', 'm/s'),
                    "hum" = c('%', 'kg/kg', 'degC', 'degC', 'hPa'),
                    "pres" = 'hPa',
                    "rad_acc" = c("J/m2", "J/m2", "J/m2", "J/m2"),
                    "prcp" = "mm",
                    "evp" = "mm",
                    "pet" = "mm",
                    "runoff" = c("mm", "mm", "mm"),
                    "soilm" = c("m3/m3", "m3/m3", "m3/m3", "m3/m3"),
                    "soilt" = c("degC", "degC", "degC", "degC"),
                    "tsg" = "degC",
                    "heat_acc" = c("J/m2", "J/m2"),
                    NULL)

    convert_units <- switch(var_params,
                          "tair" = list(fun = "-", args = list(273.15)),
                          "wind" = NULL,
                          "hum" = NULL,
                          "pres" = list(fun = "/", args = list(100)),
                          "rad_acc" = NULL,
                          "prcp" = list(fun = "*", args = list(1000)),
                          "evp" = list(fun = "*", args = list(1000)),
                          "pet" = list(fun = "*", args = list(1000)),
                          "runoff" = list(fun = "*", args = list(1000)),
                          "soilm" = NULL,
                          "soilt" = list(fun = "-", args = list(273.15)),
                          "tsg" = list(fun = "-", args = list(273.15)),
                          "heat_acc" = NULL,
                          NULL)

    name <- switch(var_params,
                   "wind" = c('ugrd', 'vgrd'),
                   "hum" = c('rh', 'q', 'tm', 'td', 'pr'),
                   "rad_acc" = c("ssr", "str", "ssrd", "strd"),
                   "prcp" = 'ptot',
                   "pet" = "pev",
                   "runoff" = c("ro", "sro", "ssro"),
                   "soilm" = c('swvl1', 'swvl2', 'swvl3', 'swvl4'),
                   "soilt" = c('stl1', 'stl2', 'stl3', 'stl4'),
                   "heat_acc" = c('slhf', 'sshf'),
                   var_params)

    ncpars <- list(name = name, units = units, longname = longname,
                   prec = "float", missval = -9999, convert = convert_units,
                   era_name = era5_var$nc, var = var_params)

    list(ncpars = ncpars, cds_variable = era5_var$api)
}
