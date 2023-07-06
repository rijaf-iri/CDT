
jra55_ncss.download.rda.ucar <- function(GalParams, nbfile = 1, GUI = TRUE, verbose = TRUE){
    on.exit({
        curl::handle_reset(handle)
        curl::handle_reset(handle_down)
    })

    xlon <- seq(-179.9997300, 179.4377600, length.out = 640)
    xlat <- seq(-89.5700900, 89.5700900, length.out = 320)
    ix <- xlon >= GalParams$bbox$minlon & xlon <= GalParams$bbox$maxlon
    iy <- xlat >= GalParams$bbox$minlat & xlat <= GalParams$bbox$maxlat

    if(!any(ix) | !any(iy)){
        Insert.Messages.Out("Invalid area of interest", TRUE, "e", GUI)
        return(-2)
    }

    ######################

    query_bbox <- list(west = GalParams$bbox$minlon,
                       east = GalParams$bbox$maxlon,
                       south = GalParams$bbox$minlat,
                       north = GalParams$bbox$maxlat)

    ######################

    start <- GalParams$date.range[paste0('start.', c('year', 'mon', 'day', 'hour'))]
    start <- jra55.start.end.time(start)
    end <- GalParams$date.range[paste0('end.', c('year', 'mon', 'day', 'hour'))]
    end <- jra55.start.end.time(end)

    rtimes <- seq(start, end, "3 hours")
    ftimes <- format(rtimes, '%Y-%m-%dT%H:00:00Z')

    query_times <- paste0("time=", ftimes)

    ######################

    # jra_ncss <- "https://rda.ucar.edu/thredds/ncss/grid/aggregations/g/ds628.0"
    jra_ncss <- "https://thredds.rda.ucar.edu/thredds/ncss/grid/aggregations/g/ds628.0"

    query_add <- list(horizStride = 1, vertStride = 1,
                      accept = "netcdf4-classic", addLatLon = "true")

    jra_var <- switch(GalParams$var,
                    "tmax" = list(pth = "31/TwoD", var = "Maximum_temperature_height_above_ground"),
                    "tmin" = list(pth = "31/TwoD", var = "Minimum_temperature_height_above_ground"),
                    "tmean" = list(pth = "27/TwoD", var = "Temperature_height_above_ground"),
                    "wind" =  list(pth = "27/TwoD",
                                   var = c("u-component_of_wind_height_above_ground",
                                           "v-component_of_wind_height_above_ground")
                                  ),
                    "hum" =  list(pth = "27/TwoD",
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
                    "rad" = list(pth = "22/TP",
                                 var = c("Clear_sky_downward_longwave_radiation_flux_surface_Average",
                                         "Clear_sky_downward_solar_radiation_flux_surface_Average",
                                         "Clear_sky_upward_longwave_radiation_flux_atmosphere_top_Average",
                                         "Clear_sky_upward_solar_radiation_flux_surface_Average",
                                         "Clear_sky_upward_solar_radiation_flux_atmosphere_top_Average",
                                         "Downward_longwave_radiation_flux_surface_Average",
                                         "Downward_solar_radiation_flux_surface_Average",
                                         "Downward_solar_radiation_flux_atmosphere_top_Average",
                                         "Upward_longwave_radiation_flux_surface_Average",
                                         "Upward_longwave_radiation_flux_atmosphere_top_Average",
                                         "Upward_solar_radiation_flux_surface_Average",
                                         "Upward_solar_radiation_flux_atmosphere_top_Average")
                                 ),
                    "prcp" = list(pth = "22/TP",
                                  var = c("Total_precipitation_surface_Average",
                                          "Large_scale_precipitation_surface_Average",
                                          "Convective_precipitation_surface_Average")
                                  ),
                    "evp" = list(pth = "22/TP", var = "Evaporation_surface_Average"),
                    "pet" = list(pth = "25/TP", var = "Evapotranspiration_surface_Average"),
                    "runoff" = list(pth = "25/TP", var = "Water_run-off_surface_Average"),
                    "gflx" = list(pth = "25/TP", var = "Ground_heat_flux_surface_Average"),
                    "soilm" = list(pth = "16/TwoD",
                                   var = c("Soil_wetness_underground_layer",
                                           "Mass_concentration_of_condensed_water_in_soil_underground_layer")
                                   ),
                    "soilt" = list(pth = "16/TwoD", var = "Soil_temperature_entire_soil"),
                    "tsg" = list(pth = "16/TwoD", var = "Ground_temperature_surface"),
                    NULL)

    ##########

    longname <- switch(GalParams$var,
                       "tmax" = "JRA55 3 Hourly Maximum temperature at 2 m above ground",
                       "tmin" = "JRA55 3 Hourly Minimum temperature at 2 m above ground",
                       "tmean" = "JRA55 3 Hourly Mean temperature at 2 m above ground",
                       "wind" = c("JRA55 3 Hourly U-wind at 10 m above ground",
                                  "JRA55 3 Hourly V-wind at 10 m above ground"),
                       "hum" = c("JRA55 3 Hourly Relative humidity at 2 m above ground",
                                 "JRA55 3 Hourly Specific humidity at 2 m above ground"),
                       "pres" = "JRA55 3 Hourly Pressure at ground or water surface",
                       "prmsl" = "JRA55 3 Hourly Pressure reduced to mean sea level",
                       "cloud" = c("JRA55 3 Hourly Total cloud cover at 90 - 1100 hPa",
                                   "JRA55 3 Hourly High cloud cover at 90 - 500 hPa",
                                   "JRA55 3 Hourly Medium cloud cover at 500 - 850 hPa",
                                   "JRA55 3 Hourly Low cloud cover at 850 - 1100 hPa"),
                       "rad" = c("JRA55 3 Hourly Clear sky downward longwave radiation flux at ground or water surface",
                                 "JRA55 3 Hourly Clear sky downward solar radiation flux at ground or water surface",
                                 "JRA55 3 Hourly Clear sky upward longwave radiation flux at nominal top of atmosphere",
                                 "JRA55 3 Hourly Clear sky upward solar radiation flux at ground or water surface",
                                 "JRA55 3 Hourly Clear sky upward solar radiation flux at nominal top of atmosphere",
                                 "JRA55 3 Hourly Downward longwave radiation flux at ground or water surface",
                                 "JRA55 3 Hourly Downward solar radiation flux at ground or water surface",
                                 "JRA55 3 Hourly Downward solar radiation flux at nominal top of atmosphere",
                                 "JRA55 3 Hourly Upward longwave radiation flux at ground or water surface",
                                 "JRA55 3 Hourly Upward longwave radiation flux at nominal top of atmosphere",
                                 "JRA55 3 Hourly Upward solar radiation flux at ground or water surface",
                                 "JRA55 3 Hourly Upward solar radiation flux at nominal top of atmosphere"),
                       "prcp" = c("JRA55 3 Hourly Total precipitation at ground or water surface",
                                  "JRA55 3 Hourly Large scale precipitation at ground or water surface",
                                  "JRA55 3 Hourly Convective precipitation at ground or water surface"),
                       "evp" = "JRA55 3 Hourly Evaporation at ground or water surface",
                       "pet" = "JRA55 3 Hourly Evapotranspiration at ground surface",
                       "runoff" = "JRA55 3 Hourly Water run-off at ground surface",
                       "gflx" = "JRA55 3 Hourly Ground heat flux at ground surface",
                       "soilm" = c("JRA55 3 Hourly Soil wetness at underground layer 1",
                                   "JRA55 3 Hourly Soil wetness at underground layer 2",
                                   "JRA55 3 Hourly Soil wetness at underground layer 3",
                                   "JRA55 3 Hourly Mass concentration of condensed water in soil at underground layer 1",
                                   "JRA55 3 Hourly Mass concentration of condensed water in soil at underground layer 2",
                                   "JRA55 3 Hourly Mass concentration of condensed water in soil at underground layer 3"),
                       "soilt" = "JRA55 3 Hourly Soil temperature at the entire soil layer",
                       "tsg" = "JRA55 3 Hourly Ground temperature at ground surface",
                        NULL)

    units <- switch(GalParams$var,
                    "tmax" = "degC",
                    "tmin" = "degC",
                    "tmean" = "degC",
                    "wind" = c('m/s', 'm/s'),
                    "hum" = c("%", "kg/kg"),
                    "pres" = "hPa",
                    "prmsl" = "hPa",
                    "cloud" = c('%', '%', '%', '%'),
                    "rad" = c("W/m2", "W/m2", "W/m2", "W/m2",
                              "W/m2", "W/m2", "W/m2", "W/m2",
                              "W/m2", "W/m2", "W/m2", "W/m2"),
                    "prcp" = c("mm", "mm", "mm"),
                    "evp" = "mm",
                    "pet" = "mm",
                    "runoff" = "mm",
                    "gflx" = "W/m2",
                    "soilm" = c("proportion", "proportion", "proportion",
                                "kg/m3", "kg/m3", "kg/m3"),
                    "soilt" = "degC",
                    "tsg" = "degC",
                    NULL)

    convert_units <- switch(GalParams$var,
                          "tmax" = list(fun = "-", args = list(273.15)),
                          "tmin" = list(fun = "-", args = list(273.15)),
                          "tmean" = list(fun = "-", args = list(273.15)),
                          "wind" = NULL,
                          "hum" = NULL,
                          "pres" = list(fun = "/", args = list(100)),
                          "prmsl" = list(fun = "/", args = list(100)),
                          "cloud" = NULL,
                          "rad" = NULL,
                          "prcp" = list(fun = "*", args = list(3/24)),
                          "evp" = list(fun = "*", args = list(3/24)),
                          "pet" = list(fun = "*", args = list(0.035 * 3/24)),
                          "runoff" = list(fun = "*", args = list(3/24)),
                          "gflx" = NULL,
                          "soilm" = NULL,
                          "soilt" = list(fun = "-", args = list(273.15)),
                          "tsg" = list(fun = "-", args = list(273.15)),
                          NULL)

    name <- switch(GalParams$var,
                   "wind" = c('ugrd', 'vgrd'),
                   "hum" = c('rh', 'spfh'),
                   "cloud" = c('tcdc', 'hcdc', 'mcdc', 'lcdc'),
                   "rad" = c('csdlf_sfc', 'csdsf_sfc', 'csulf_top', 'csusf_sfc',
                             'csusf_top', 'dlwrf_sfc', 'dswrf_sfc', 'dswrf_top',
                             'ulwrf_sfc', 'ulwrf_top', 'uswrf_sfc', 'uswrf_top'),
                   "prcp" = c('ptot', 'plrgscl', 'pconv'),
                   "soilm" = c("soilw_l1", "soilw_l2", "soilw_l3",
                               "smc_l1", "smc_l2", "smc_l3"),
                   GalParams$var)

    ncpars <- list(name = name, units = units, longname = longname, prec = "float",
                   missval = -9999, convert = convert_units, nc4var = jra_var$var,
                   var = GalParams$var)

    ######################

    query_sub <- c(query_bbox, query_add)
    query_sub <- sapply(seq_along(query_sub), function(j){
                    paste(names(query_sub[j]), query_sub[[j]], sep = "=")
                  })
    query_sub <- paste0(query_sub, collapse = "&")

    query_var <- sapply(jra_var$var, function(x) paste0("var=", x))
    query_var <- paste0(query_var, collapse = "&")

    query <- paste(query_var, query_times, query_sub, sep = "&")

    ##########

    url_pth <- paste(jra_ncss, jra_var$pth, sep = "/")
    urls <- paste0(url_pth, "?", query)

    daty <- format(rtimes, "%Y%m%d%H")
    ncfiles <- sprintf(paste0(GalParams$var, "_%s.nc"), daty)
    destfiles <- sprintf(paste0("tmp_data", "_%s.nc4"), daty)

    ##########

    data.name <- "JRA-55 3 Hourly"
    dir.name <- "JRA55_3Hr_data"
    outdir <- file.path(GalParams$dir2save, dir.name)
    dir.create(outdir, showWarnings = FALSE, recursive = TRUE)

    outdir <- file.path(outdir, paste0('JRA55_', GalParams$var))
    dir.create(outdir, showWarnings = FALSE, recursive = TRUE)

    destfiles <- file.path(outdir, destfiles)
    ncfiles <- file.path(outdir, ncfiles)

    ##########

    ### login (no needs just check if the is able to login)
    login_url <- "https://rda.ucar.edu/cgi-bin/login"
    postfields <- paste0("email=", GalParams$login$usr,
                         "&passwd=", GalParams$login$pwd,
                         "&action=login")

    handle <- curl::new_handle()
    curl::handle_setopt(handle, postfields = postfields)
    res <- curl::curl_fetch_memory(login_url, handle)
    if(res$status_code != 200){
        Insert.Messages.Out("Unable to login to https://rda.ucar.edu", TRUE, "e", GUI)
        return(-2)
    }
    curl::handle_cookies(handle)

    ### downloading
    handle_down <- curl::new_handle()
    curl::handle_setopt(handle_down, username = GalParams$login$usr, password = GalParams$login$pwd)

    ##########

    ret <- cdt.download.data(urls, destfiles, ncfiles, nbfile, GUI,
                             verbose, data.name, jra55_ncss.download.data,
                             handle = handle_down, ncpars = ncpars)

    return(ret)
}

jra55_ncss.download.data <- function(lnk, dest, ncfl, handle, ncpars){
    xx <- basename(ncfl)
    dc <- try(curl::curl_download(lnk, dest, handle = handle), silent = TRUE)

    if(!inherits(dc, "try-error")){
        ret <- jra55_ncss.format.data(dest, ncfl, ncpars)
        if(ret == 0){
            unlink(dest)
            xx <- NULL
        }
    }

    return(xx)
}

jra55_ncss.format.data <- function(dest, ncfl, ncpars){
    nc <- ncdf4::nc_open(dest)

    lon <- nc$dim$longitude$vals
    lon <- ((lon + 180) %% 360) - 180
    lat <- nc$dim$latitude$vals

    xo <- order(lon)
    yo <- order(lat)
    lon <- lon[xo]
    lat <- lat[yo]

    if(ncpars$var == "soilm"){
        dat <- lapply(seq_along(ncpars$nc4var), function(j){
            x <- ncdf4::ncvar_get(nc, ncpars$nc4var[j])
            x[is.nan(x)] <- NA
            x[xo, yo, ]
        })
        dat <- do.call(abind::abind, c(dat, list(along = 3)))
        # dat <- lapply(seq(dim(dat)[3]), abind::asub, x = dat, dims = 3)
        dat <- lapply(seq(dim(dat)[3]), function(j) dat[, , j])
    }else{
        dat <- lapply(seq_along(ncpars$nc4var), function(j){
            x <- ncdf4::ncvar_get(nc, ncpars$nc4var[j])
            x[is.nan(x)] <- NA
            if(!is.null(ncpars$convert)){
                ncpars$convert$args <- c(list(x), ncpars$convert$args)
                x <- do.call(ncpars$convert$fun, ncpars$convert$args)
            }
            x[xo, yo]
        })
    }

    ncdf4::nc_close(nc)

    don <- lapply(dat, function(x){
        jra55.regrid.data(lon, lat, x)
    })
    don <- list(x = don[[1]]$x, y = don[[1]]$y,
                z = lapply(don, '[[', 'z'))
    if(length(don$z) == 1) don$z <- don$z[[1]]

    reanalysis.write.ncdf(don, ncpars, ncfl)

    return(0)
}

