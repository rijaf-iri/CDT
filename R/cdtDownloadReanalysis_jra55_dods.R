
jra55_dods.download.rda.ucar <- function(GalParams, nbfile = 1, GUI = TRUE, verbose = TRUE){
    jracrd0 <- file.path(.cdtDir$Root, "data", "JRA55_Coords.rds")
    jra.crd <- readRDS(jracrd0)
    xlon <- jra.crd$lon
    xlon <- ((xlon + 180) %% 360) - 180
    xlat <- jra.crd$lat

    ix <- xlon >= GalParams$bbox$minlon & xlon <= GalParams$bbox$maxlon
    iy <- xlat >= GalParams$bbox$minlat & xlat <= GalParams$bbox$maxlat

    if(!any(ix) | !any(iy)){
        Insert.Messages.Out("Invalid area of interest", TRUE, "e", GUI)
        return(-2)
    }

    rix <- rle(ix)
    ie <- cumsum(rix$lengths)
    is <- c(1, (ie + 1)[-length(ie)])
    ix <- lapply(seq_along(is), function(j) is[j]:ie[j])
    ilon <- lapply(ix[rix$values], function(x) range(x) - 1 )
    ilat <- range(which(iy)) - 1
    query_lon <- lapply(ilon, function(x) paste0("[", x[1], ":", 1, ":", x[2], "]"))
    query_lat <- paste0("[", ilat[1], ":", 1, ":", ilat[2], "]")

    #############

    jra_dods <- "https://thredds.rda.ucar.edu/thredds/dodsC/aggregations/g/ds628.0"
    jra_var <- switch(GalParams$var,
                    "tmax" = list(pth = "31/TwoD", 
                                  var = "Maximum_temperature_height_above_ground_3_Hour",
                                  origin = 1, fac = 4, height = 1,
                                  type = 1, 
                                  timeoffset = 1, varoffset = 1, search = 1),
                    "tmin" = list(pth = "31/TwoD", 
                                  var = "Minimum_temperature_height_above_ground_3_Hour",
                                  origin = 1, fac = 4, height = 1,
                                  type = 1, 
                                  varoffset = 1, timeoffset = 1, search = 1),
                    "tair" = list(pth = "27/TwoD",
                                  var = "Temperature_height_above_ground",
                                  origin = 1, fac = 4, height = 1,
                                  type = 2, 
                                  varoffset = 1, timeoffset = 1, search = 1),
                    "wind" = list(pth = "27/TwoD",
                                  var = c("u-component_of_wind_height_above_ground",
                                          "v-component_of_wind_height_above_ground"),
                                  origin = 1, fac = 4, height = c(1, 1),
                                  type = 2, 
                                  varoffset = c(1, 1), timeoffset = 1, search = 1),
                    "hum" = list(pth = "27/TwoD",
                                 var = c("Relative_humidity_height_above_ground",
                                         "Specific_humidity_height_above_ground"),
                                  origin = 1, fac = 4, height = c(1, 1),
                                  type = 2, 
                                  varoffset = c(1, 1), timeoffset = 1, search = 1),
                    "pres" = list(pth = "27/TwoD", var = "Pressure_surface",
                                  origin = 1, fac = 4, height = 0,
                                  type = 2, 
                                  varoffset = 1, timeoffset = 1, search = 1),
                    "prmsl" = list(pth = "27/TwoD", 
                                   var = "Pressure_reduced_to_MSL_msl",
                                  origin = 1, fac = 4, height = 0,
                                  type = 2, 
                                  varoffset = 1, timeoffset = 1, search = 1),
                    "cloud" =  list(pth = "27/TwoD",
                                    var = c("Total_cloud_cover_layer_between_two_isobaric_layer",
                                            "High_cloud_cover_layer_between_two_isobaric_layer",
                                            "Medium_cloud_cover_layer_between_two_isobaric_layer",
                                            "Low_cloud_cover_layer_between_two_isobaric_layer"),
                                  origin = 1, fac = 4, height = c(1, 1, 1, 1),
                                  type = 2, 
                                  varoffset = c(1, 1, 1, 1), timeoffset = 1, search = 1),
                    "rad_avg" = list(pth = "21/TP",
                                     var = c("Clear_sky_downward_longwave_radiation_flux_surface_3_Hour_Average",
                                             "Clear_sky_downward_solar_radiation_flux_surface_3_Hour_Average",
                                             "Clear_sky_upward_longwave_radiation_flux_atmosphere_top_3_Hour_Average",
                                             "Clear_sky_upward_solar_radiation_flux_surface_3_Hour_Average",
                                             "Clear_sky_upward_solar_radiation_flux_atmosphere_top_3_Hour_Average",
                                             "Downward_longwave_radiation_flux_surface_3_Hour_Average",
                                             "Downward_solar_radiation_flux_surface_3_Hour_Average",
                                             "Downward_solar_radiation_flux_atmosphere_top_3_Hour_Average",
                                             "Upward_longwave_radiation_flux_surface_3_Hour_Average",
                                             "Upward_longwave_radiation_flux_atmosphere_top_3_Hour_Average",
                                             "Upward_solar_radiation_flux_surface_3_Hour_Average",
                                             "Upward_solar_radiation_flux_atmosphere_top_3_Hour_Average"),
                                  origin = 2, fac = 8, height = rep(0, 12),
                                  type = 0, 
                                  varoffset = rep(0, 12), timeoffset = 0, search = 2),
                    "prcp" = list(pth = "21/TP",
                                  var = c("Total_precipitation_surface_3_Hour_Average",
                                          "Large_scale_precipitation_surface_3_Hour_Average",
                                          "Convective_precipitation_surface_3_Hour_Average"),
                                  origin = 2, fac = 8, height = c(0, 0, 0),
                                  type = 0, 
                                  varoffset = c(0, 0, 0), timeoffset = 0, search = 2),
                    "evp" = list(pth = "21/TP",
                                 var = "Evaporation_surface_3_Hour_Average",
                                 origin = 2, fac = 8, height = 0,
                                 type = 0, 
                                 varoffset = 0, timeoffset = 0, search = 2),
                    "pet" = list(pth = "25/TP",
                                 var = "Evapotranspiration_surface_3_Hour_Average",
                                 origin = 2, fac = 8, height = 0,
                                 type = 0, 
                                 varoffset = 0, timeoffset = 0, search = 2),
                    "runoff" = list(pth = "25/TP",
                                    var = c("Water_run-off_surface_3_Hour_Average",
                                            "Water_run-off_bottom_of_model_3_Hour_Average"),
                                    origin = 2, fac = 8, height = c(0, 1),
                                    type = 0, 
                                    varoffset = c(0, 0), timeoffset = 0, search = 2),
                    "soilm" = list(pth = "16/TwoD",
                                   var = c("Soil_wetness_underground_layer",
                                           "Mass_concentration_of_condensed_water_in_soil_underground_layer"),
                                   origin = 1, fac = 4, height = c(2, 2),
                                   type = 2, 
                                   varoffset = c(1, 1), timeoffset = 1, search = 1),
                    "soilt" = list(pth = "16/TwoD", 
                                   var = "Soil_temperature_entire_soil",
                                   origin = 1, fac = 4, height = 1,
                                   type = 2, 
                                   varoffset = 1, timeoffset = 1, search = 1),
                    "tsg" = list(pth = "16/TwoD",
                                 var = "Ground_temperature_surface",
                                 origin = 1, fac = 4, height = 0,
                                 type = 2, 
                                 varoffset = 1, timeoffset = 1, search = 1),
                    "heat_avg" = list(pth = "21/TP",
                                      var = c("Latent_heat_flux_surface_3_Hour_Average",
                                             "Sensible_heat_flux_surface_3_Hour_Average"),
                                      origin = 2, fac = 8, height = c(0, 0),
                                      type = 0, 
                                      varoffset = c(0, 0), timeoffset = 0, search = 2),
                    "ghflx" = list(pth = "25/TP",
                                   var = "Ground_heat_flux_surface_3_Hour_Average",
                                   origin = 2, fac = 8, height = 0,
                                   type = 0, 
                                   varoffset = 0, timeoffset = 0, search = 2),
                    NULL)

    if(is.null(jra_var)){
        Insert.Messages.Out("Unknown variable", TRUE, "e", GUI)
        return(-2)
    }

    #############
    query_height <- lapply(jra_var$height, function(i) {
        out <- NULL
        if(i == 0) out <- NULL
        if(i == 1) out <- '[0:1:0]'
        if(i == 2) out <- '[0:1:2]'
        out
    })

    req_search <- NULL
    if(jra_var$search == 1) req_search <- "Float64 reftime\\[reftime"
    if(jra_var$search == 2) req_search <- "Float64 time\\[time"
    time_origin <- NULL
    if(jra_var$origin == 1) time_origin <- "1957-12-31T18:00:00Z"
    if(jra_var$origin == 2) time_origin <- "1958-01-01T00:00:00Z"

    ## type = 0, "21/TP", "25/TP"
    ## type = 1, "31/TwoD"
    ## type = 2, "16/TwoD", "27/TwoD"

    ## origin = 1, "1957-12-31T18:00:00Z"
    ## origin = 2, "1958-01-01T00:00:00Z"

    ## search = 1, "Float64 reftime\\[reftime"
    ## search = 2, "Float64 time\\[time"

    ## height = 0, NULL
    ## height = 1, [0:1:0]
    ## height = 2, [0:1:2]

    ## timeoffset = 0, NULL
    ## timeoffset = 1, [0:1:0]

    ## varoffset = 0, NULL
    ## varoffset = 1, [0:1:0]

    #############

    dods_page <- paste0(jra_dods, "/", jra_var$pth, ".html")
    dds <- xml2::read_html(dods_page)
    dds <- xml2::xml_find_all(dds, "//pre")
    dds <- xml2::xml_text(dds)
    dds <- strsplit(dds, '\n')[[1]]

    isrch <- grep(req_search, dds)[1]
    last_incr <- dds[isrch]
    last_incr <- stringr::str_match(last_incr, "\\[\\s*(.*?)\\s*\\]")
    last_incr <- last_incr[ ,2]
    last_incr <- trimws(strsplit(last_incr, "=")[[1]][2])
    last_incr <- as.numeric(last_incr)

    #############

    origin <- as.POSIXct(time_origin, tz = "GMT", format = "%Y-%m-%dT%H:%M:%SZ")

    start <- GalParams$date.range[paste0('start.', c('year', 'mon', 'day', 'hour'))]
    start <- jra55.start.end.time(start)
    end <- GalParams$date.range[paste0('end.', c('year', 'mon', 'day', 'hour'))]
    end <- jra55.start.end.time(end)

    last_time <- origin + 24 * 3600 * last_incr/jra_var$fac
    last_time <- last_time - 3 * 3600
    lastT <- format(last_time, "%Y-%m-%d %H:%M:%S")
    msg <- paste("Last date of available data", lastT)
    if(end > last_time){
        end <- last_time
        Insert.Messages.Out(msg, TRUE, "i", GUI)
    }
    if(start > end){
        Insert.Messages.Out(msg, TRUE, "e", GUI)
        return(-2)
    }

    if(jra_var$timeoffset == 1){
        start <- start - 3 * 3600
        end <- end - 3 * 3600
    }

    seq_times <- seq(start, end, "3 hours")

    #############

    split_reftime <- sapply(seq_times, function(x){
        floor(as.numeric(x - origin, units = "days") * jra_var$fac)
    })
    query_reftime <- sapply(split_reftime, function(x) paste0("[", x, ":", 1, ":", x, "]"))

    #############

    urls <- lapply(seq_along(query_reftime), function(j){
        time_h <- as.numeric(format(seq_times[j], '%H'))
        if(jra_var$timeoffset == 1){
            if((time_h %% 2) == 0){
                query_timeOffset <- '[0:1:0]'
            }else{
                query_timeOffset <- '[1:1:1]'
            }
        }else{
            query_timeOffset <- NULL
        }

        sapply(seq_along(query_lon), function(i){
            dods <- paste0(jra_dods, "/", jra_var$pth, ".ascii")
            req_time <- paste0("time", query_reftime[j], query_timeOffset)
            req_time <- utils::URLencode(req_time, reserved = TRUE)

            req_var <- sapply(seq_along(jra_var$var), function(v){
                if(jra_var$varoffset[v] == 1){
                    if((time_h %% 2) == 0){
                        query_varOffset <- '[0:1:0]'
                    }else{
                        query_varOffset <- '[1:1:1]'
                    }
                }else{
                    query_varOffset <- NULL
                }

                paste0(jra_var$var[v], query_reftime[j], query_varOffset,
                       query_height[[v]], query_lat, query_lon[[i]])
            })

            req_var <- paste(req_var, collapse = ",")
            req_var <- utils::URLencode(req_var, reserved = TRUE)
            paste0(dods, "?", req_time, ",", req_var)
        })
    })

    # #############
    # ## def inside urls loop
    # query_timeOffset <- if(jra_var$timeoffset == 1) '[0:1:0]' else NULL
    # query_varOffset <- lapply(jra_var$varoffset, function(i) if(i == 1) '[0:1:0]' else NULL)

    # urls <- lapply(seq_along(query_reftime), function(j){
    #     sapply(seq_along(query_lon), function(i){
    #         dods <- paste0(jra_dods, "/", jra_var$pth, ".ascii")
    #         req_time <- paste0("time", query_reftime[j], query_timeOffset)
    #         # req_time <- utils::URLencode(req_time, reserved = TRUE)

    #         req_var <- sapply(seq_along(jra_var$var), function(v){
    #             paste0(jra_var$var[v], query_reftime[j], query_varOffset[[v]],
    #                    query_height[[v]], query_lat, query_lon[[i]])
    #         })

    #         req_var <- paste(req_var, collapse = ",")
    #         # req_var <- utils::URLencode(req_var, reserved = TRUE)
    #         paste0(dods, "?", req_time, ",", req_var)
    #     })
    # })

    #############

    longname <- switch(GalParams$var,
                       "tmax" = "Maximum temperature at 2 m above ground",
                       "tmin" = "Minimum temperature at 2 m above ground",
                       "tair" = "Air temperature at 2 m above ground",
                       "wind" = c("U-wind at 10 m above ground",
                                  "V-wind at 10 m above ground"),
                       "hum" = c("Relative humidity at 2 m above ground",
                                  "Specific humidity at 2 m above ground"),
                       "pres" = "Pressure at ground or water surface",
                       "prmsl" = "Pressure reduced to mean sea level",
                       "cloud" = c("Total cloud cover at 90 - 1100 hPa",
                                   "High cloud cover at 90 - 500 hPa",
                                   "Medium cloud cover at 500 - 850 hPa",
                                   "Low cloud cover at 850 - 1100 hPa"),
                       "rad_avg" = c("Clear sky downward longwave radiation flux at ground or water surface",
                                     "Clear sky downward solar radiation flux at ground or water surface",
                                     "Clear sky upward longwave radiation flux at nominal top of atmosphere",
                                     "Clear sky upward solar radiation flux at ground or water surface",
                                     "Clear sky upward solar radiation flux at nominal top of atmosphere",
                                     "Downward longwave radiation flux at ground or water surface",
                                     "Downward solar radiation flux at ground or water surface",
                                     "Downward solar radiation flux at nominal top of atmosphere",
                                     "Upward longwave radiation flux at ground or water surface",
                                     "Upward longwave radiation flux at nominal top of atmosphere",
                                     "Upward solar radiation flux at ground or water surface",
                                     "Upward solar radiation flux at nominal top of atmosphere"),
                       "prcp" = c("Total precipitation at ground or water surface",
                                  "Large scale precipitation at ground or water surface",
                                  "Convective precipitation at ground or water surface"),
                       "evp" = " Evaporation at ground or water surface",
                       "pet" = "Evapotranspiration at ground surface",
                       "runoff" = c("Water run-off at ground surface",
                                    "Water run-off at the bottom of land surface model"),
                       "soilm" = c("Soil wetness at underground layer 1",
                                   "Soil wetness at underground layer 2",
                                   "Soil wetness at underground layer 3",
                                   "Mass concentration of condensed water in soil at underground layer 1",
                                   "Mass concentration of condensed water in soil at underground layer 2",
                                   "Mass concentration of condensed water in soil at underground layer 3"),
                       "soilt" = "Soil temperature at the entire soil layer",
                       "tsg" = "Ground temperature at ground surface",
                       "heat_avg" = c("Latent heat flux at ground or water surface",
                                      "Sensible heat flux at ground or water surface"),
                       "ghflx" = "Ground heat flux at ground surface",
                        NULL)

    units <- switch(GalParams$var,
                    "tmax" = "degC",
                    "tmin" = "degC",
                    "tair" = "degC",
                    "wind" = c('m/s', 'm/s'),
                    "hum" = c('%', 'kg/kg'),
                    "pres" = "hPa",
                    "prmsl" = "hPa",
                    "cloud" = c('%', '%', '%', '%'),
                    "rad_avg" = c("W/m2", "W/m2", "W/m2", "W/m2",
                                  "W/m2", "W/m2", "W/m2", "W/m2",
                                  "W/m2", "W/m2", "W/m2", "W/m2"),
                    "prcp" = c("mm", "mm", "mm"),
                    "evp" = "mm",
                    "pet" = "mm",
                    "runoff" = c("mm", "mm"),
                    "soilm" = c("proportion", "proportion", "proportion",
                                "kg/m3", "kg/m3", "kg/m3"),
                    "soilt" = "degC",
                    "tsg" = "degC",
                    "heat_avg" = c("W/m2", "W/m2"),
                    "ghflx" = "W/m2",
                    NULL)

    convert_units <- switch(GalParams$var,
                          "tmax" = list(fun = "-", args = list(273.15)),
                          "tmin" = list(fun = "-", args = list(273.15)),
                          "tair" = list(fun = "-", args = list(273.15)),
                          "wind" = NULL,
                          "hum" = NULL,
                          "pres" = list(fun = "/", args = list(100)),
                          "prmsl" = list(fun = "/", args = list(100)),
                          "cloud" = NULL,
                          "rad_avg" = NULL,
                          "prcp" = list(fun = "*", args = list(3/24)),
                          "evp" = list(fun = "*", args = list(3/24)),
                          "pet" = list(fun = "*", args = list(0.035 * 3/24)),
                          "runoff" = list(fun = "*", args = list(3/24)),
                          "soilm" = NULL,
                          "soilt" = list(fun = "-", args = list(273.15)),
                          "tsg" = list(fun = "-", args = list(273.15)),
                          "heat_avg" = NULL,
                          "ghflx" = NULL,
                          NULL)

    name <- switch(GalParams$var,
                   "wind" = c('ugrd', 'vgrd'),
                   "hum" = c('rh', 'spfh'),
                   "cloud" = c('tcdc', 'hcdc', 'mcdc', 'lcdc'),
                   "rad_avg" = c('csdlf_sfc', 'csdsf_sfc', 'csulf_top', 'csusf_sfc',
                                 'csusf_top', 'dlwrf_sfc', 'dswrf_sfc', 'dswrf_top',
                                 'ulwrf_sfc', 'ulwrf_top', 'uswrf_sfc', 'uswrf_top'),
                   "prcp" = c('ptot', 'plrgscl', 'pconv'),
                   "runoff" = c('ro_sfc', 'ro_bmod'),
                   "soilm" = c("soilw_l1", "soilw_l2", "soilw_l3",
                               "smc_l1", "smc_l2", "smc_l3"),
                   "heat_avg" = c("lhflx", "lsflx"),
                   GalParams$var)

    ncpars <- list(name = name, units = units, longname = longname, prec = "float", missval = -9999)
    pars <- list(nc = ncpars, convert = convert_units, txtvar = jra_var$var,
                 var = GalParams$var, origin = jra_var$origin,
                 timetype = jra_var$type, 
                 timeoffset = jra_var$timeoffset)

    ######################

    data.name <- "JRA-55 3 Hourly"
    dir.name <- "JRA55_3Hr_data"
    outdir <- file.path(GalParams$dir2save, dir.name)
    dir.create(outdir, showWarnings = FALSE, recursive = TRUE)

    outdir <- file.path(outdir, paste0('JRA55_', GalParams$var))
    dir.create(outdir, showWarnings = FALSE, recursive = TRUE)

    filenames <- format(seq_times, '%Y%m%d%H')
    ncfiles_time <- filenames

    destfiles <- lapply(seq_along(urls), function(j){
        sapply(seq_along(query_lon), function(i)
            file.path(outdir, paste0(GalParams$var, "_", filenames[j], "_", i, ".txt")))
    })

    ##########

    ret <- cdt.download.data(urls, destfiles, ncfiles_time, nbfile, GUI, verbose,
                             data.name, jra55_dods.download.data, pars = pars)

    return(ret)
}

jra55_dods.download.data <- function(lnk, dest, ncfl, pars){
    on.exit(lapply(dest, unlink))

    dest <- dest[[1]]
    lnk <- lnk[[1]]
    xx <- ncfl

    dc <- lapply(seq_along(lnk), function(j){
         ret <- try(curl::curl_download(lnk[j], dest[j]), silent = TRUE)
         if(inherits(ret, "try-error")) 1 else 0
    })

    if(all(unlist(dc) == 0)){
        ret <- jra55_dods.format.data(dest, pars)
        if(ret == 0) xx <- NULL
    }

    return(xx)
}

#########################

jra55_dods.format.data <- function(dest, pars){
    dat <- lapply(dest, jra55_dods.parse.ascii, pars = pars)
    if(length(dest) == 2){
        ## merge the 2 data (longitude west & east)
    }else dat <- dat[[1]]

    ncdir <- dirname(dest[1])

    ncfile <- format(dat$time, '%Y%m%d%H')
    ncfile <- paste0(pars$var, '_', ncfile, '.nc')
    ncfile <- file.path(ncdir, ncfile)

    jra55_dods.write.ncdf(dat, pars, ncfile)

    return(0)
}

jra55_dods.write.ncdf <- function(dat, pars, ncfile){
    dx <- ncdf4::ncdim_def("Lon", "degreeE", dat$var[[1]]$lon, longname = "Longitude")
    dy <- ncdf4::ncdim_def("Lat", "degreeN", dat$var[[1]]$lat, longname = "Latitude")

    ncgrd <- lapply(seq_along(pars$nc$name), function(j){
        ncdf4::ncvar_def(pars$nc$name[j], pars$nc$units[j],
                        list(dx, dy), pars$nc$missval,
                        pars$nc$longname[j], pars$nc$prec,
                        compression = 6)
    })

    don <- lapply(pars$txtvar, function(vr){
        x <- dat$var[[vr]]$data
        z <- lapply(x, function(z){
            z[is.na(z)] <- pars$nc$missval
            z
        })
        z
    })

    don <- do.call(c, don)

    nc <- ncdf4::nc_create(ncfile, ncgrd)
    for(j in seq_along(ncgrd))
        ncdf4::ncvar_put(nc, ncgrd[[j]], don[[j]])
    ncdf4::nc_close(nc)

    invisible()
}

jra55_dods.parse.ascii <- function(filetxt, pars){
    jra <- readLines(filetxt)
    jra <- trimws(jra)

    time_origin <- NULL
    if(pars$origin == 1) time_origin <- "1957-12-31T18:00:00Z"
    if(pars$origin == 2) time_origin <- "1958-01-01T00:00:00Z"

    origin <- as.POSIXct(time_origin, tz = "GMT", format = "%Y-%m-%dT%H:%M:%SZ")

    times <- jra55_dods.get.var(jra, 'time')
    if(is.null(times)) return(NULL)

    if(pars$timeoffset == 1){
        times <- as.numeric(trimws(times[[1]][-1]))
        ## or
        # reftime <- jra55_dods.get.var(jra, pars$txtvar[1], 'reftime')
        # reftime <- as.numeric(trimws(reftime[[1]]))
        # timeoffset <- jra55_dods.get.var(jra, pars$txtvar[1], 'timeOffset')
        # timeoffset <- as.numeric(trimws(timeoffset[[1]]))
        # times <- reftime + timeoffset

        ## timetype = 1, code 31 
        if(pars$timetype == 1){
            times <- times + 1.5
        }
        ## timetype = 2, code 27, 16
        # times <- times + 0
    }else{
        times <- as.numeric(trimws(times[[1]]))
        times <- times - 1.5
    }

    times <- as.POSIXct(times * 3600, origin = origin, tz = "GMT")

    dat_var <- lapply(pars$txtvar, function(vr){
        don <- jra55_dods.get.data(jra, vr)
        if(is.null(don)) return(NULL)

        xo <- order(don$lon)
        yo <- order(don$lat)
        don$lon <- don$lon[xo]
        don$lat <- don$lat[yo]

        tmp <- lapply(don$data, function(x){
            x <- t(x[yo, xo])
            x[is.nan(x)] <- NA
            round(x, 12)
        })

        if(!is.null(pars$convert)){
            tmp <- lapply(tmp, function(x){
                convert_args <- c(list(x), pars$convert$args)
                do.call(pars$convert$fun, convert_args)
            })
        }

        don$data <- tmp
        don
    })
    names(dat_var) <- pars$txtvar

    inull <- sapply(dat_var, is.null)
    if(any(inull)) return(NULL)

    return(list(time = times, var = dat_var))
}

jra55_dods.get.data <- function(x, vr){
    val <- jra55_dods.get.var(x, vr, vr)
    if(is.null(val)) return(NULL)

    index <- lapply(val, '[[', 1)
    index <- lapply(index, jra55_dods.get.index)
    index <- do.call(rbind, index)
    ix_hgt <- index[, ncol(index) - 1]

    val <- lapply(val, '[', -1)
    val <- lapply(val, trimws)
    val <- lapply(val, as.numeric)
    val <- do.call(rbind, val)

    ix_hgt <- split(seq(nrow(val)), ix_hgt)
    val <- lapply(ix_hgt, function(i) val[i, , drop = FALSE])

    lon <- jra55_dods.get.var(x, vr, 'lon')
    if(is.null(lon)) return(NULL)

    lon <- as.numeric(trimws(lon[[1]]))
    lon <- ((lon + 180) %% 360) - 180

    lat <- jra55_dods.get.var(x, vr, 'lat')
    if(is.null(lat)) return(NULL)

    lat <- as.numeric(trimws(lat[[1]]))

    list(lon = lon, lat = lat, data = val)
}

jra55_dods.get.var <- function(x, vr, pr = NULL){
    if(is.null(pr)){
        sx <- paste0("^", vr, "\\[")
    }else{
        sx <- paste0("^", vr, "\\.", pr, "\\[")
    }

    i1 <- grep(sx, x)
    if(length(i1) == 0) return(NULL)

    i2 <- jra55_dods.split.line(x, i1)
    v <- x[(i1 + 1):(i2 - 1)]
    v <- strsplit(v, ",")

    return(v)
}

jra55_dods.split.line <- function(x, s){
    nl <- 0
    for(i in s:length(x)){
        if(x[i] == ""){
            nl <- i
            break
        }
    }

    return(nl)
}

jra55_dods.get.index <- function(x){
    ix <- gregexpr("(?<=\\[).*?(?=\\])", x, perl = TRUE)
    v <- regmatches(x, ix)
    as.numeric(v[[1]])
}
