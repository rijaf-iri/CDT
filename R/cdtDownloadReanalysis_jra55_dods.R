
jra55_dods.download.rda.ucar <- function(GalParams, nbfile = 1, GUI = TRUE, verbose = TRUE){
    on.exit({
        curl::handle_reset(handle)
        curl::handle_reset(handle_down)
    })

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

    ### login for xml parsing
    login_url <- "https://rda.ucar.edu/cgi-bin/login"
    postfields <- paste0("email=", GalParams$login$usr,
                         "&passwd=", GalParams$login$pwd,
                         "&action=login")

    handle <- curl::new_handle()
    curl::handle_setopt(handle, postfields = postfields)
    res <- curl::curl_fetch_memory(login_url, handle)
    curl::handle_cookies(handle)

    ### downloading
    handle_down <- curl::new_handle()
    curl::handle_setopt(handle_down, username = GalParams$login$usr, password = GalParams$login$pwd)

    #############

    jra_var <- switch(GalParams$var,
                    "tmax" = list(pth = "31/TwoD", var = "Maximum_temperature_height_above_ground_3_Hour"),
                    "tmin" = list(pth = "31/TwoD", var = "Minimum_temperature_height_above_ground_3_Hour"),
                    "tair" = list(pth = "27/TwoD", var = "Temperature_height_above_ground"),
                    "wind" = list(pth = "27/TwoD",
                                  var = c("u-component_of_wind_height_above_ground",
                                          "v-component_of_wind_height_above_ground")
                                  ),
                    "hum" = list(pth = "27/TwoD",
                                 var = c("Relative_humidity_height_above_ground",
                                         "Specific_humidity_height_above_ground")
                                  ),
                    "pres" = list(pth = "27/TwoD", var = "Pressure_surface"),
                    "prmsl" = list(pth = "27/TwoD", var = "Pressure_reduced_to_MSL_msl"),
                    "cloud" =  list(pth = "27/TwoD",
                                    var = c("Total_cloud_cover_layer_between_two_isobaric_layer",
                                            "High_cloud_cover_layer_between_two_isobaric_layer",
                                            "Medium_cloud_cover_layer_between_two_isobaric_layer",
                                            "Low_cloud_cover_layer_between_two_isobaric_layer")
                                  ),
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
                                             "Upward_solar_radiation_flux_atmosphere_top_3_Hour_Average")
                                     ),
                    "prcp" = list(pth = "21/TP",
                                  var = c("Total_precipitation_surface_3_Hour_Average",
                                          "Large_scale_precipitation_surface_3_Hour_Average",
                                          "Convective_precipitation_surface_3_Hour_Average")
                                  ),
                    "evp" = list(pth = "21/TP", var = "Evaporation_surface_3_Hour_Average"),
                    "pet" = list(pth = "25/TP", var = "Evapotranspiration_surface_3_Hour_Average"),
                    "runoff" = list(pth = "25/TP",
                                    var = c("Water_run-off_surface_3_Hour_Average",
                                            "Water_run-off_bottom_of_model_3_Hour_Average")
                                        ),
                    "soilm" = list(pth = "16/TwoD",
                                   var = c("Soil_wetness_underground_layer",
                                           "Mass_concentration_of_condensed_water_in_soil_underground_layer")
                                   ),
                    "soilt" = list(pth = "16/TwoD", var = "Soil_temperature_entire_soil"),
                    "tsg" = list(pth = "16/TwoD", var = "Ground_temperature_surface"),
                    "heat_avg" = list(pth = "21/TP",
                                     var = c("Latent_heat_flux_surface_3_Hour_Average",
                                             "Sensible_heat_flux_surface_3_Hour_Average")
                                    ),
                    "ghflx" = list(pth = "25/TP", var = "Ground_heat_flux_surface_3_Hour_Average"),
                    NULL)

    #############

    pars1 <- c("tmax", "tmin", "tair", "wind", "hum",
               "pres", "prmsl", "cloud", "soilm", "soilt", "tsg")
    pars2 <- c("rad_avg", "prcp", "evp", "pet", "runoff", "ghflx", "heat_avg")

    if(GalParams$var %in% pars1){
        times_pars <- list(origin = "1957-12-31T18:00:00Z", fac = 4)
        srch <- "Float64 reftime\\[reftime"
        times_type <- "matrix"
    }else if(GalParams$var %in% pars2){
        times_pars <- list(origin = "1958-01-01T00:00:00Z", fac = 8)
        srch <- "Float64 time\\[time"
        times_type <- "vector"
    }else{
        times_pars <- NULL
        srch <- NULL
        times_type <- NULL
    }

    #############

    jra_dods <- "https://rda.ucar.edu/thredds/dodsC/aggregations/g/ds628.0"

    dods_page <- paste0(jra_dods, "/", jra_var$pth, ".html")
    dds <- xml2::read_html(dods_page)
    dds <- xml2::xml_find_all(dds, "//pre")
    dds <- xml2::xml_text(dds)
    dds <- strsplit(dds, '\n')[[1]]

    isrch <- grep(srch, dds)[1]
    last_incr <- dds[isrch]
    last_incr <- stringr::str_match(last_incr, "\\[\\s*(.*?)\\s*\\]")
    last_incr <- last_incr[ ,2]
    last_incr <- trimws(strsplit(last_incr, "=")[[1]][2])
    last_incr <- as.numeric(last_incr)

    #############

    origin <- as.POSIXct(times_pars$origin, tz = "GMT", format = "%Y-%m-%dT%H:%M:%SZ")
    start <- GalParams$date.range[paste0('start.', c('year', 'mon', 'day', 'hour'))]
    start <- jra55.start.end.time(start)
    end <- GalParams$date.range[paste0('end.', c('year', 'mon', 'day', 'hour'))]
    end <- jra55.start.end.time(end)

    last_time <- origin + 24 * 3600 * last_incr/times_pars$fac
    if(end > last_time) end <- last_time
    if(start > end){
        lastT <- format(last_time, "%Y-%m-%d %H:%M:%S")
        Insert.Messages.Out(paste("Last date of available data", lastT), TRUE, "e", GUI)
        return(-2)
    }

    seq_times <- seq(start, end, "3 hours")
    split_times <- split(seq_times, ceiling(seq_along(seq_times)/40))
    range_times <- lapply(split_times, function(x) range(x))

    filenames <- sapply(range_times, function(x){
        tt <- format(x, '%Y%m%d%H')
        paste(tt, collapse = "-")
    })

    split_reftime <- lapply(range_times, function(x){
        floor(as.numeric(x - origin, units = "days") * times_pars$fac) - 1
    })

    query_reftime <- lapply(split_reftime, function(x) paste0("[", x[1], ":", 1, ":", x[2], "]"))

    #############

    query_times <- switch(GalParams$var,
                          "rad_avg" = NULL,
                          "prcp" = NULL,
                          "evp" = NULL,
                          "pet" = NULL,
                          "runoff" = NULL,
                          "ghflx" = NULL,
                          "heat_avg" = NULL,
                          "[0:1:1]")

    query_height <- switch(GalParams$var,
                           "pres" = NULL,
                           "prmsl" = NULL,
                           "rad_avg" = NULL,
                           "prcp" = NULL,
                           "evp" = NULL,
                           "pet" = NULL,
                           "runoff" = NULL,
                           "ghflx" = NULL,
                           "heat_avg" = NULL,
                           "soilm" = "[0:1:2]",
                           "tsg" = NULL,
                           "[0:1:0]")

    urls <- lapply(seq_along(query_reftime), function(j){
        sapply(seq_along(query_lon), function(i){
            dods <- paste0(jra_dods, "/", jra_var$pth, ".ascii")
            req_time <- paste0("time", query_reftime[[j]], query_times)
            req_var <- sapply(jra_var$var, function(v){
                if(v == "Water_run-off_bottom_of_model_3_Hour_Average")
                    query_height <- "[0:1:0]"

                paste0(v, query_reftime[[j]], query_times, query_height,
                       query_lat, query_lon[[i]])
            })
            req_var <- paste(req_var, collapse = ",")
            paste0(dods, "?", req_time, ",", req_var)
        })
    })

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
                 var = GalParams$var, timetype = times_type,
                 origin = times_pars$origin)

    ######################

    data.name <- "JRA-55 3 Hourly"
    dir.name <- "JRA55_3Hr_data"
    outdir <- file.path(GalParams$dir2save, dir.name)
    dir.create(outdir, showWarnings = FALSE, recursive = TRUE)

    outdir <- file.path(outdir, paste0('JRA55_', GalParams$var))
    dir.create(outdir, showWarnings = FALSE, recursive = TRUE)

    destfiles <- lapply(seq_along(urls), function(j){
        sapply(seq_along(query_lon), function(i)
            file.path(outdir, paste0(GalParams$var, "_", filenames[j], "_", i, ".txt")))
    })

    ncfiles_time <- lapply(split_times, format, "%Y%m%d%H")

    ##########

    ret <- cdt.download.data(urls, destfiles, ncfiles_time, nbfile, GUI,
                             verbose, data.name, jra55_dods.download.data,
                             handle = handle_down, pars = pars)

    return(ret)
}

jra55_dods.download.data <- function(lnk, dest, ncfl, handle, pars){
    on.exit(lapply(dest, unlink))

    dest <- dest[[1]]
    lnk <- lnk[[1]]
    ncfl <- ncfl[[1]]

    xx <- paste(ncfl[1], ncfl[length(ncfl)], sep = "-")

    dc <- lapply(seq_along(lnk), function(j){
         ret <- try(curl::curl_download(lnk[j], dest[j], handle = handle), silent = TRUE)
         if(inherits(ret, "try-error")) 1 else 0
    })

    if(all(unlist(dc) == 0)){
        ret <- jra55_dods.format.data(dest, ncfl, pars)
        if(ret == 0) xx <- NULL
        xx <- NULL
    }

    return(xx)
}

jra55_dods.format.data <- function(dest, ncfl, pars){
    dat <- lapply(dest, jra55_dods.parse.ascii, pars = pars)
    don <- dat[[1]]
    if(length(dat) == 2){
        don <- lapply(seq_along(pars$txtvar), function(v){
            x <- don[[v]]
            y <- dat[[2]][[v]]
            x$lon <- c(x$lon, y$lon)

            n <- length(x$dim)
            x$dim[n] <- x$dim[n] + y$dim[n]

            tmp <- lapply(seq_along(x$data), function(j){
                cbind(x$data[[j]], y$data[[j]])
            })
            names(tmp) <- names(x$data)
            x$data <- tmp

            x
        })
    }

    don <- lapply(seq_along(pars$txtvar), function(v){
        dd <- don[[v]]
        xo <- order(dd$lon)
        yo <- order(dd$lat)
        dd$lon <- dd$lon[xo]
        dd$lat <- dd$lat[yo]

        it <- dd$time %in% ncfl
        dd$time <- dd$time[it]
        dd$data <- dd$data[it]
        dd$index <- dd$index[it]

        dd$data <- lapply(dd$data, function(d){
            x <- t(d[yo, xo])
            x[is.nan(x)] <- NA
            if(!is.null(pars$convert)){
                pars$convert$args <- c(list(x), pars$convert$args)
                x <- do.call(pars$convert$fun, pars$convert$args)
            }

            x <- jra55.regrid.data(dd$lon, dd$lat, x)
            x$z
        })

        rlon <- range(dd$lon)
        rlat <- range(dd$lat)
        dd$lon <- seq(rlon[1], rlon[2], length.out = length(dd$lon))
        dd$lat <- seq(rlat[1], rlat[2], length.out = length(dd$lat))

        dd
    })

    ncdir <- dirname(dest[1])
    times <- unique(don[[1]]$time)
    lon <- don[[1]]$lon
    lat <- don[[1]]$lat

    for(j in seq_along(times)){
        if(length(pars$txtvar) == 1){
            dat <- list(x = lon, y = lat, z = don[[1]]$data[[j]])
        }else{
            if(pars$var == "soilm"){
                zval <- lapply(seq_along(pars$txtvar), function(v){
                    il <- don[[v]]$time %in% times[j]
                    ix <- sort(unlist(don[[v]]$index[il]))
                    don[[v]]$data[il][ix]
                })

                zval <- c(zval[[1]], zval[[2]])
            }else{
                zval <- lapply(seq_along(pars$txtvar), function(v){
                    don[[v]]$data[[j]]
                })
            }

            dat <- list(x = lon, y = lat, z = zval)
        }

        ncpth <- file.path(ncdir, paste0(pars$var, "_", times[j], ".nc"))
        reanalysis.write.ncdf(dat, pars$nc, ncpth)
    }

    return(0)
}

jra55_dods.parse.ascii <- function(filetxt, pars){
    jra <- readLines(filetxt)
    jra <- trimws(jra)

    times <- jra55_dods.get.var(jra, 'time')
    times <- times$data

    origin <- as.POSIXct(pars$origin, tz = "GMT", format = "%Y-%m-%dT%H:%M:%SZ")

    if(pars$timetype == "vector"){
        times <- as.numeric(trimws(times[[1]]))
        times <- as.POSIXct(times * 3600, origin = origin, tz = "GMT")
    }else{
        times <- lapply(times, '[', -1)
        times <- lapply(times, function(x){
            y <- as.numeric(trimws(x))
            as.POSIXct(y * 3600, origin = origin, tz = "GMT")
        })
    }

    dat_var <- lapply(pars$txtvar, function(vr){
        don <- jra55_dods.get.data(jra, vr)
        if(pars$timetype == "vector"){
            vtimes <- format(times, "%Y%m%d%H")
        }else{
            vtimes <- sapply(don$index, function(i){
                format(times[[i[1]]][i[2]], "%Y%m%d%H")
            })
        }

        index <- lapply(don$index, '[', -(1:2))

        list(dim = don$dim, lon = don$lon, lat = don$lat,
             index = index, time = vtimes, data = don$data)
    })

    return(dat_var)
}

jra55_dods.get.data <- function(x, vr){
    val <- jra55_dods.get.var(x, vr, vr)
    nvar <- val$dim
    val <- val$data

    iv <- lapply(val, '[', 1)
    iv <- lapply(iv, jra55_dods.get.index)
    iv <- do.call(rbind, iv) + 1

    val <- lapply(val, '[', -1)
    val <- lapply(val, trimws)
    val <- lapply(val, as.numeric)
    val <- do.call(rbind, val)

    nr <- seq(nrow(val))
    ir <- split(nr, ceiling(nr / nvar[length(nvar) - 1]))
    arr <- lapply(ir, function(i){
        dat <- val[i, , drop = FALSE]
        ix <- iv[i, -ncol(iv), drop = FALSE]
        ix <- ix[1, ]
        list(index = ix, data = dat)
    })

    lon <- jra55_dods.get.var(x, vr, 'lon')
    lon <- as.numeric(trimws(lon$data[[1]]))
    lon <- ((lon + 180) %% 360) - 180

    lat <- jra55_dods.get.var(x, vr, 'lat')
    lat <- as.numeric(trimws(lat$data[[1]]))

    list(dim = nvar, lon = lon, lat = lat,
         index = lapply(arr, '[[', 'index'),
         data = lapply(arr, '[[', 'data'))
}

jra55_dods.get.var <- function(x, vr, pr = NULL){
    if(is.null(pr)){
        sx <- paste0("^", vr, "\\[")
    }else{
        sx <- paste0("^", vr, "\\.", pr, "\\[")
    }

    i1 <- grep(sx, x)
    i2 <- jra55_dods.split.line(x, i1)
    v <- x[(i1 + 1):(i2 - 1)]
    n <- jra55_dods.get.index(x[i1])
    v <- strsplit(v, ",")

    list(dim = n, data = v)
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
