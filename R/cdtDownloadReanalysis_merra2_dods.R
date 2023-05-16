
merra2_hourly.download.earthdata <- function(GalParams, nbfile = 1, GUI = TRUE, verbose = TRUE){
    xlon <- seq(-180., 180., 0.625)
    xlat <- seq(-90., 90., 0.5)

    ix <- xlon >= GalParams$bbox$minlon & xlon <= GalParams$bbox$maxlon
    iy <- xlat >= GalParams$bbox$minlat & xlat <= GalParams$bbox$maxlat

    if(!any(ix) | !any(iy)) return(-2)

    ilon <- range(which(ix)) - c(2, 1)
    ilat <- range(which(iy)) - 1
    query_lon <- paste0("[", ilon[1], ":", ilon[2], "]")
    query_lat <- paste0("[", ilat[1], ":", ilat[2], "]")

    start <- GalParams$date.range[paste0('start.', c('year', 'mon', 'day', 'hour'))]
    start <- hourly.start.end.time(start)
    end <- GalParams$date.range[paste0('end.', c('year', 'mon', 'day', 'hour'))]
    end <- hourly.start.end.time(end)

    seqDays <- seq(start, end, "day")
    query_dates <- format(seqDays, "%Y%m%d")
    query_years <- format(seqDays, "%Y")
    query_months <- format(seqDays, "%m")
    query_hours <- "[0:23]"

    #######
    ## assimilation stream version
    query_stream <- cut(as.numeric(query_years), 
        breaks = c(1980, 1992, 2001, 2011, 2100),
        labels = c(100, 200, 300, 400), 
        include.lowest = TRUE, right = FALSE)

    ######

    startH <- as.numeric(format(start, '%H'))
    endH <- as.numeric(format(end, '%H'))

    rm_hours <- NULL
    if(startH != 0){
        ih <- start - (1:startH) * 3600
        rm_hours <- c(rm_hours, format(ih, "%Y%m%d%H"))
    }

    if(endH != 23){
        ih <- end + (1:(23 - endH)) * 3600
        rm_hours <- c(rm_hours, format(ih, "%Y%m%d%H"))
    }

    ######################

    merra2_var <- switch(GalParams$var,
                    "tair" = list(pth = c("M2T1NXSLV", "tavg1_2d_slv_Nx"), var = "T2M"),
                    "wind" = list(pth = c("M2T1NXSLV", "tavg1_2d_slv_Nx"),
                                  var = c("U2M", "V2M", "U10M", "V10M", "U50M", "V50M")),
                    "hum" = list(pth = c("M2T1NXSLV", "tavg1_2d_slv_Nx"),
                                 var = c("QV2M", "T2M", "PS", "T2MDEW", "T2MWET")),
                    "pres" = list(pth = c("M2T1NXSLV", "tavg1_2d_slv_Nx"), var = "PS"),
                    "prmsl" = list(pth = c("M2T1NXSLV", "tavg1_2d_slv_Nx"), var = "SLP"),
                    "cloud" = list(pth = c("M2T1NXRAD", "tavg1_2d_rad_Nx"),
                                   var = c("CLDHGH", "CLDLOW", "CLDMID", "CLDTOT")),
                    "rad_avg" = list(pth = c("M2T1NXRAD", "tavg1_2d_rad_Nx"),
                                     var = c("LWGABCLR", "LWGABCLRCLN", "LWGNTCLR", "LWGNTCLRCLN",
                                             "LWTUPCLR", "LWTUPCLRCLN", "SWGDNCLR", "SWGNTCLR",
                                             "SWGNTCLRCLN", "SWTNTCLR", "SWTNTCLRCLN",
                                             "LWGAB", "LWGEM", "LWGNT", "LWTUP", "SWGDN",
                                             "SWGNT", "SWGNTCLN", "SWTDN", "SWTNT", "SWTNTCLN")),
                    "prcp" = list(pth = c("M2T1NXFLX", "tavg1_2d_flx_Nx"),
                                  var = c("PRECTOT", "PRECTOTCORR", "PGENTOT", "PREVTOT",
                                          "PRECCON", "PRECANV", "PRECLSC")),
                    "evp" = list(pth = c("M2T1NXFLX", "tavg1_2d_flx_Nx"), var = "EVAP"),
                    "tsg" = list(pth = c("M2T1NXFLX", "tavg1_2d_flx_Nx"), var = "TSH"),
                    "heat_avg" = list(pth = c("M2T1NXFLX", "tavg1_2d_flx_Nx"),
                                      var = c("EFLUX", "HFLUX")),
                    "ghflx" = list(pth = c("M2T1NXFLX", "tavg1_2d_flx_Nx"), var = "GHTSKIN"),
                    NULL)

    longname <- switch(GalParams$var,
                       "tair" = "Air temperature at 2-meter",
                       "wind" = c("Eastward wind at 2-meter",
                                  "Northward wind at 2-meter",
                                  "Eastward wind at 10-meter",
                                  "Northward wind at 10-meter",
                                  "Eastward wind at 50 meters",
                                  "Northward wind at 50 meters"),
                       "hum" = c("Relative humidity at 2-meter",
                                 "Specific humidity at 2-meter",
                                 "Air temperature at 2-meter",
                                 "Dew point temperature at 2-meter",
                                 "Wet bulb temperature at 2-meter",
                                 "Surface pressure"),
                       "pres" = "Surface pressure",
                       "prmsl" = "Sea level pressure",
                       "cloud" = c("Cloud area fraction for high clouds",
                                   "Cloud area fraction for low clouds",
                                   "Cloud area fraction for middle clouds",
                                   "Total cloud area fraction"),
                        "rad_avg" = c("Surface absorbed longwave radiation assuming clear sky",
                                      "Surface absorbed longwave radiation assuming clear sky and no aerosol",
                                      "Surface net downward longwave flux assuming clear sky",
                                      "Surface net downward longwave flux assuming clear sky and no aerosol",
                                      "Upwelling longwave flux at TOA assuming clear sky",
                                      "Upwelling longwave flux at TOA assuming clear sky and no aerosol",
                                      "Surface incoming shortwave flux assuming clear sky",
                                      "Surface net downward shortwave flux assuming clear sky",
                                      "Surface net downward shortwave flux assuming clear sky and no aerosol",
                                      "TOA net downward shortwave flux assuming clear sky",
                                      "TOA net downward shortwave flux assuming clear sky and no aerosol",
                                      "Surface absorbed longwave radiation",
                                      "Longwave flux emitted from surface",
                                      "Surface net downward longwave flux",
                                      "Upwelling longwave flux at TOA",
                                      "Surface incoming shortwave flux",
                                      "Surface net downward shortwave flux",
                                      "Surface net downward shortwave flux assuming no aerosol",
                                      "TOAincoming shortwave flux",
                                      "TOA net downward shortwave flux",
                                      "TOA net downward shortwave flux assuming no aerosol"),
                        "prcp" = c("Total precipitation from atm model physics",
                                   "Bias corrected total precipitation",
                                   "Total column production of precipitation",
                                   "Total column re-evaporation/sublimation of precipitation",
                                   "Convective precipitation",
                                   "Anvil precipitation",
                                   "Nonanvil large scale precipitation"),
                        "evp" = "Surface evaporation",
                        "tsg" = "Effective surface skin temperature",
                        "heat_avg" = c("Latent heat flux (positive upward)",
                                       "Sensible heat flux (positive upward)"),
                        "ghflx" = "Ground heating for skin temperature",
                        NULL)

    units <- switch(GalParams$var,
                    "tair" = "degC",
                    "wind" = c('m/s', 'm/s', 'm/s', 'm/s', 'm/s', 'm/s'),
                    "hum" = c('%', 'kg/kg', 'degC', 'degC', 'degC', 'hPa'),
                    "pres" = "hPa",
                    "prmsl" = "hPa",
                    "cloud" = c('fraction', 'fraction', 'fraction', 'fraction'),
                    "rad_avg" = c("W/m2", "W/m2", "W/m2", "W/m2", "W/m2", "W/m2", "W/m2",
                                  "W/m2", "W/m2", "W/m2", "W/m2", "W/m2", "W/m2", "W/m2",
                                  "W/m2", "W/m2", "W/m2", "W/m2", "W/m2", "W/m2", "W/m2"),
                    "prcp" = c("mm", "mm", "mm", "mm", "mm", "mm", "mm"),
                    "evp" = "mm",
                    "tsg" = "degC",
                    "heat_avg" = c("W/m2", "W/m2"),
                    "ghflx" = "W/m2",
                    NULL)

    convert_units <- switch(GalParams$var,
                          "tair" = list(fun = "-", args = list(273.15)),
                          "wind" = NULL,
                          "hum" = NULL,
                          "pres" = list(fun = "/", args = list(100)),
                          "prmsl" = list(fun = "/", args = list(100)),
                          "cloud" = NULL,
                          "rad_avg" = NULL,
                          "prcp" = list(fun = "*", args = list(3600)),
                          "evp" = list(fun = "*", args = list(3600)),
                          "tsg" = list(fun = "-", args = list(273.15)),
                          "heat_avg" = NULL,
                          "ghflx" = NULL,
                          NULL)

    name <- switch(GalParams$var,
                   "wind" = c("u2m", "v2m", "u10m", "v10m", "u50m", "v50m"),
                   "hum" = c("rh2m", "qv2m", "t2m", "t2mdew", "t2mwet", "ps"),
                   "cloud" = c("cldhgh", "cldlow", "cldmid", "cldtot"),
                   "rad_avg" = c("lwgabclr", "lwgabclrcln", "lwgntclr", "lwgntclrcln",
                                 "lwtupclr", "lwtupclrcln", "swgdnclr", "swgntclr",
                                 "swgntclrcln", "swtntclr", "swtntclrcln",
                                 "lwgab", "lwgem", "lwgnt", "lwtup", "swgdn",
                                 "swgnt", "swgntcln", "swtdn", "swtnt", "swtntcln"),
                   "prcp" = c("prectot", "prectotcorr", "pgentot", "prevtot",
                              "preccon", "precanv", "preclsc"),
                   "heat_avg" = c("eflux", "hflux"),
                   GalParams$var)

    ncpars <- list(name = name, units = units, longname = longname,
                   prec = "float", missval = -9999, convert = convert_units,
                   merra_name = merra2_var$var, var = GalParams$var)

    ######################

    opendap_url <- "https://goldsmr4.gesdisc.eosdis.nasa.gov/opendap/MERRA2"
    merra2_version <- "5.12.4"

    query_paths <- paste(merra2_var$pth[1], merra2_version, sep = ".")
    query_stream <- paste0("MERRA2_", query_stream)
    query_endpoints <- paste(query_stream, merra2_var$pth[2], query_dates, "nc4.nc4", sep = ".")
    query_urls <- paste(opendap_url, query_paths, query_years, query_months, query_endpoints, sep = "/")

    query_vars <- sapply(seq_along(query_urls), function(j){
        x <- sapply(merra2_var$var, function(v){
            paste0(v, query_hours, query_lat, query_lon)
        })
        paste0(x, collapse = ",")
    })

    query_dims <- paste0("time,lat", query_lat, ",lon", query_lon)
    query_opendap <- paste0(query_vars, ",", query_dims)
    urls <- paste0(query_urls, "?", query_opendap)

    ######################

    data.name <- "MERRA2 Hourly GES DISC"
    dir.name <- "MERRA2_1Hr_data"
    outdir <- file.path(GalParams$dir2save, dir.name)
    dir.create(outdir, showWarnings = FALSE, recursive = TRUE)

    outdir <- file.path(outdir, paste0('MERRA2_', GalParams$var))
    dir.create(outdir, showWarnings = FALSE, recursive = TRUE)

    destfiles <- file.path(outdir, paste0(GalParams$var, "_", query_dates, ".nc"))
    if(!is.null(rm_hours))
        rm_files <- file.path(outdir, paste0(GalParams$var, "_", rm_hours, ".nc"))

    ######################
    ret <- cdt.download.data(urls, destfiles, destfiles, nbfile, GUI,
                             verbose, data.name, merra2_dods.download.data,
                             login = GalParams$login, pars = ncpars)

    if(!is.null(rm_hours)) lapply(rm_files, unlink)

    return(ret)
}

merra2_land.download.earthdata <- function(GalParams, nbfile = 1, GUI = TRUE, verbose = TRUE){
    xlon <- seq(-180., 180., 0.625)
    xlat <- seq(-90., 90., 0.5)

    ix <- xlon >= GalParams$bbox$minlon & xlon <= GalParams$bbox$maxlon
    iy <- xlat >= GalParams$bbox$minlat & xlat <= GalParams$bbox$maxlat

    if(!any(ix) | !any(iy)) return(-2)

    ilon <- range(which(ix)) - c(2, 1)
    ilat <- range(which(iy)) - 1
    query_lon <- paste0("[", ilon[1], ":", ilon[2], "]")
    query_lat <- paste0("[", ilat[1], ":", ilat[2], "]")

    start <- GalParams$date.range[paste0('start.', c('year', 'mon', 'day', 'hour'))]
    start <- hourly.start.end.time(start)
    end <- GalParams$date.range[paste0('end.', c('year', 'mon', 'day', 'hour'))]
    end <- hourly.start.end.time(end)

    seqDays <- seq(start, end, "day")
    query_dates <- format(seqDays, "%Y%m%d")
    query_years <- format(seqDays, "%Y")
    query_months <- format(seqDays, "%m")
    query_hours <- "[0:23]"

    #######
    ## assimilation stream version
    query_stream <- cut(as.numeric(query_years), 
        breaks = c(1980, 1992, 2001, 2011, 2100),
        labels = c(100, 200, 300, 400), 
        include.lowest = TRUE, right = FALSE)

    #######

    startH <- as.numeric(format(start, '%H'))
    endH <- as.numeric(format(end, '%H'))

    rm_hours <- NULL
    if(startH != 0){
        ih <- start - (1:startH) * 3600
        rm_hours <- c(rm_hours, format(ih, "%Y%m%d%H"))
    }

    if(endH != 23){
        ih <- end + (1:(23 - endH)) * 3600
        rm_hours <- c(rm_hours, format(ih, "%Y%m%d%H"))
    }

    ######################

    merra2_var <- switch(GalParams$var,
                    "rad_avg" = list(pth = c("M2T1NXLND", "tavg1_2d_lnd_Nx"),
                                     var = c("SWLAND", "LWLAND", "PARDFLAND", "PARDRLAND")),
                    "prcp" = list(pth = c("M2T1NXLND", "tavg1_2d_lnd_Nx"),
                                  var = "PRECTOTLAND"),
                    "pet" = list(pth = c("M2T1NXLND", "tavg1_2d_lnd_Nx"), var = "EVLAND"),
                    "runoff" = list(pth = c("M2T1NXLND", "tavg1_2d_lnd_Nx"), var = c("RUNOFF", "BASEFLOW")),
                    "soilm" = list(pth = c("M2T1NXLND", "tavg1_2d_lnd_Nx"),
                                   var = c("GWETTOP", "GWETROOT", "GWETPROF", "SFMC", "RZMC", "PRMC")),
                    "soilt" = list(pth = c("M2T1NXLND", "tavg1_2d_lnd_Nx"),
                                    var = c("TSOIL1", "TSOIL2", "TSOIL3", "TSOIL4", "TSOIL5", "TSOIL6")),
                    "tsg" = list(pth = c("M2T1NXLND", "tavg1_2d_lnd_Nx"), var = "TSURF"),
                    "heat_avg" = list(pth = c("M2T1NXLND", "tavg1_2d_lnd_Nx"),
                                     var = c("LHLAND", "SHLAND")),
                    "ghflx" = list(pth = c("M2T1NXLND", "tavg1_2d_lnd_Nx"), var = "GHLAND"),
                    NULL)

    longname <- switch(GalParams$var,
                       "rad_avg" = c("Net shortwave land",
                                     "Net longwave land",
                                     "Surface downwelling par diffuse flux",
                                     "Surface downwelling par beam flux"),
                       "prcp" = "Total precipitation land; bias corrected",
                       "pet" = "Evapotranspiration land",
                       "runoff" = c("Overland runoff including throughflow", "Baseflow"),
                       "soilm" = c("Surface soil wetness, 0-5 cm",
                                   "Root zone soil wetness, 0-100 cm",
                                   "Average profile soil moisture, surface-bedrock",
                                   "Water surface layer, 0-5 cm",
                                   "Water root zone, 0-100 cm",
                                   "Water profile, surface-bedrock"),
                       "soilt" = c("Soil temperatures layer 1",
                                   "Soil temperatures layer 2",
                                   "Soil temperatures layer 3",
                                   "Soil temperatures layer 4",
                                   "Soil temperatures layer 5",
                                   "Soil temperatures layer 6"),
                       "tsg" = "Surface temperature of land including snow",
                       "heat_avg" = c("Latent heat flux land",
                                      "Sensible heat flux land"),
                       "ghflx" = "Ground heating land",
                        NULL)

    units <- switch(GalParams$var,
                    "rad_avg" = c("W/m2", "W/m2", "W/m2", "W/m2"),
                    "prcp" = "mm",
                    "pet" = "mm",
                    "runoff" = c("mm", "mm"),
                    "soilm" = c("fraction", "fraction", "fraction",
                                "m3/m3", "m3/m3", "m3/m3"),
                    "soilt" = c("degC", "degC", "degC", "degC", "degC", "degC"),
                    "tsg" = "degC",
                    "heat_avg" = c("W/m2", "W/m2"),
                    "ghflx" = "W/m2",
                    NULL)

    convert_units <- switch(GalParams$var,
                          "rad_avg" = NULL,
                          "prcp" = list(fun = "*", args = list(3600)),
                          "pet" = list(fun = "*", args = list(3600)),
                          "runoff" = list(fun = "*", args = list(3600)),
                          "soilm" = NULL,
                          "soilt" = list(fun = "-", args = list(273.15)),
                          "tsg" = list(fun = "-", args = list(273.15)),
                          "heat_avg" = NULL,
                          "ghflx" = NULL,
                          NULL)

    name <- switch(GalParams$var,
                   "rad_avg" = c("swland", "lwland", "pardfland", "pardrland"),
                   "runoff" = c("runoff", "baseflow"),
                   "soilm" = c("soilw_l1", "soilw_l2", "soilw_l3",
                               "smc_l1", "smc_l2", "smc_l3"),
                   "soilt" = c("tsoil1", "tsoil2", "tsoil3", "tsoil4",
                               "tsoil5", "tsoil6"),
                   "tsg" = "tsurf",
                   "heat_avg" = c("lhland", "shland"),
                   "ghflx" = "ghland",
                   GalParams$var)

    ncpars <- list(name = name, units = units, longname = longname,
                   prec = "float", missval = -9999, convert = convert_units,
                   merra_name = merra2_var$var, var = GalParams$var)

    ######################

    opendap_url <- "https://goldsmr4.gesdisc.eosdis.nasa.gov/opendap/MERRA2"
    merra2_version <- "5.12.4"

    query_paths <- paste(merra2_var$pth[1], merra2_version, sep = ".")
    query_stream <- paste0("MERRA2_", query_stream)
    query_endpoints <- paste(query_stream, merra2_var$pth[2], query_dates, "nc4.nc4", sep = ".")
    query_urls <- paste(opendap_url, query_paths, query_years, query_months, query_endpoints, sep = "/")

    query_vars <- sapply(seq_along(query_urls), function(j){
        x <- sapply(merra2_var$var, function(v){
            paste0(v, query_hours, query_lat, query_lon)
        })
        paste0(x, collapse = ",")
    })

    query_dims <- paste0("time,lat", query_lat, ",lon", query_lon)
    query_opendap <- paste0(query_vars, ",", query_dims)
    urls <- paste0(query_urls, "?", query_opendap)

    ######################

    data.name <- "MERRA2 Land Hourly GES DISC"
    dir.name <- "MERRA2_Land_1Hr_data"
    outdir <- file.path(GalParams$dir2save, dir.name)
    dir.create(outdir, showWarnings = FALSE, recursive = TRUE)

    outdir <- file.path(outdir, paste0('MERRA2_', GalParams$var))
    dir.create(outdir, showWarnings = FALSE, recursive = TRUE)

    destfiles <- file.path(outdir, paste0(GalParams$var, "_", query_dates, ".nc"))
    if(!is.null(rm_hours))
        rm_files <- file.path(outdir, paste0(GalParams$var, "_", rm_hours, ".nc"))

    ######################
    ret <- cdt.download.data(urls, destfiles, destfiles, nbfile, GUI,
                             verbose, data.name, merra2_dods.download.data,
                             login = GalParams$login, pars = ncpars)

    if(!is.null(rm_hours)) lapply(rm_files, unlink)

    return(ret)
}

merra2_dods.download.data <- function(lnk, dest, ncfl, pars, login){
    on.exit({
        unlink(dest)
        curl::handle_reset(handle)
    })

    xx <- basename(dest)

    handle <- curl::new_handle()
    curl::handle_setopt(handle,
                        username = login$usr,
                        password = login$pwd)
    dc <- try(curl::curl_download(lnk, dest, handle = handle), silent = TRUE)

    if(!inherits(dc, "try-error")){
        ret <- merra2_dods.format.data(dest, pars)
        if(ret == 0) xx <- NULL
    }

    return(xx)
}

merra2_dods.format.data <- function(dest, pars){
    nc <- ncdf4::nc_open(dest)
    lon <- nc$dim[['lon']]$vals
    lat <- nc$dim[['lat']]$vals
    times <- nc$dim[['time']]$vals
    t_unit <- nc$dim[['time']]$units
    val <- lapply(pars$merra_name, function(v) ncdf4::ncvar_get(nc, v))
    names(val) <- pars$merra_name
    ncdf4::nc_close(nc)

    units(times) <- units::as_units(t_unit)
    times <- as.POSIXct(times, tz = "UTC")
    hours <- format(times, "%Y%m%d%H")
    ncfiles <- paste0(pars$var, "_", hours, ".nc")
    ncfiles <- file.path(dirname(dest), ncfiles)

    val <- lapply(val, function(v){
        if(!is.null(pars$convert)){
            pars$convert$args <- c(list(v), pars$convert$args)
            v <- do.call(pars$convert$fun, pars$convert$args)
        }

        v
    })

    if(pars$var == "hum"){
        val[["T2M"]] <- val[["T2M"]] - 273.15
        val[["T2MDEW"]] <- val[["T2MDEW"]] - 273.15
        val[["T2MWET"]] <- val[["T2MWET"]] - 273.15
        val[["PS"]] <- val[["PS"]] / 100
        rh <- relative_humidity(val[["T2M"]], val[["T2MDEW"]])

        val <- list(rh, val[["QV2M"]], val[["T2M"]], val[["T2MDEW"]],
                    val[["T2MWET"]], val[["PS"]])
    }

    for(j in seq_along(ncfiles)){
        don <- lapply(val, function(v) v[, , j])
 
        if(length(don) == 1) don <- don[[1]]
        dat <- list(x = lon, y = lat, z = don)

        reanalysis.write.ncdf(dat, pars, ncfiles[j])
    }

    return(0)
}
