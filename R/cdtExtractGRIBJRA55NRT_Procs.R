
jra55_nrt.extract.downloaded.grib <- function(GUI = TRUE){
    Insert.Messages.Out(.cdtData$GalParams[['message']][['1']], TRUE, "i")

    xlon <- seq(-179.9997300, 179.4377600, length.out = 640)
    xlat <- seq(-89.5700900, 89.5700900, length.out = 320)
    ix <- xlon >= .cdtData$GalParams$bbox$minlon & xlon <= .cdtData$GalParams$bbox$maxlon
    iy <- xlat >= .cdtData$GalParams$bbox$minlat & xlat <= .cdtData$GalParams$bbox$maxlat

    if(!any(ix) | !any(iy)){
        Insert.Messages.Out(.cdtData$GalParams[['message']][['4']], TRUE, "e", GUI)
        return(-2)
    }

    wgrib_exe <- file.path(.cdtDir$dirLocal, 'wgrib', 'wgrib')
    if(!file.exists(wgrib_exe)){
        if(WindowsOS()){
            wgrib_exe <- file.path(.cdtDir$dirLocal, 'wgrib', 'wgrib.exe')
            if(!file.exists(wgrib_exe)){
                Insert.Messages.Out(.cdtData$GalParams[['message']][['5']], TRUE, "e", GUI)
                return(-2)
            }
        }else{
            Insert.Messages.Out(.cdtData$GalParams[['message']][['5']], TRUE, "e", GUI)
            return(-2)
        }
    }


    ######################

    jra_prefix <- switch(.cdtData$GalParams$var,
                          "tmax" = "minmax_surf",
                          "tmin" = "minmax_surf",
                          "tair" = "fcst_surf",
                          "wind" = "fcst_surf",
                          "hum" = "fcst_surf",
                          "pres" = "fcst_surf",
                          "prmsl" = "fcst_surf",
                          "cloud" = "fcst_surf",
                          "rad_avg" = "fcst_phy2m",
                          "prcp" = "fcst_phy2m",
                          "evp" = "fcst_phy2m",
                          "pet" = "fcst_phyland",
                          "runoff" = "fcst_phyland",
                          "soilm" = "fcst_land",
                          "soilt" = "fcst_land",
                          "tsg" = "fcst_land",
                          "heat_avg" = "fcst_phy2m",
                          "ghflx" = "fcst_phyland",
                          NULL)

    longname <- switch(.cdtData$GalParams$var,
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
                       "evp" = "Evaporation at ground or water surface",
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

    units <- switch(.cdtData$GalParams$var,
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

    convert_units <- switch(.cdtData$GalParams$var,
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

    name <- switch(.cdtData$GalParams$var,
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
                   .cdtData$GalParams$var)

    gribpars <- switch(.cdtData$GalParams$var,
                       "tmax" = list(var = "TMAX", hgt = "2 m above gnd"),
                       "tmin" = list(var = "TMIN", hgt = "2 m above gnd"),
                       "tair" = list(var = "TMP", hgt = "2 m above gnd"),
                       "wind" = list(var = c("UGRD", "VGRD"),
                                     hgt = c("10 m above gnd", "10 m above gnd")),
                       "hum" = list(var = c("RH", "SPFH"),
                                    hgt = c("2 m above gnd", "2 m above gnd")),
                       "pres" = list(var = "PRES", hgt = "sfc"),
                       "prmsl" = list(var = "PRMSL", hgt = "MSL"),
                       "cloud" = list(var = c("TCDC", "HCDC", "MCDC", "LCDC"),
                                      hgt = c("90-1100 mb", "90-500 mb",
                                              "500-850 mb", "850-1100 mb")),
                       "rad_avg" = list(var = c("CSDLF", "CSDSF", "CSULF", "CSUSF",
                                                "CSUSF", "DLWRF", "DSWRF", "DSWRF",
                                                "ULWRF", "ULWRF", "USWRF", "USWRF"),
                                        hgt = c("sfc", "sfc", "nom. top", "sfc",
                                                "nom. top", "sfc", "sfc", "nom. top",
                                                "sfc", "nom. top", "sfc", "nom. top")),
                       "prcp" = list(var = c("TPRAT", "LPRAT", "CPRAT"),
                                     hgt = c("sfc", "sfc", "sfc")),
                       "evp" = list(var = "EVP", hgt = "sfc"),
                       "pet" = list(var = "LTRS", hgt = "sfc"),
                       "runoff" = list(var = c("ROF", "ROF"), hgt = c("sfc", "bot land sfc model")),
                       "soilm" = list(var = c("SoilW", "SoilW", "SoilW", "SMC", "SMC", "SMC"),
                                      hgt = c("soil layer 1", "soil layer 2", "soil layer 3",
                                              "soil layer 1", "soil layer 2", "soil layer 3")),
                       "soilt" = list(var = "SoilT", hgt = "soil column"),
                       "tsg" = list(var = "TSG", hgt = "sfc"),
                       "heat_avg" = list(var = c("LHTFL", "SHTFL"), hgt = c("sfc", "sfc")),
                       "ghflx" = list(var = "GFLX", hgt = "sfc"),
                        NULL)

    ncpars <- list(name = name, units = units, longname = longname,
                   prec = "float", missval = -9999, convert = convert_units)

    ######################

    start <- .cdtData$GalParams$date.range[paste0('start.', c('year', 'mon', 'day', 'hour'))]
    start <- jra55.start.end.time(start)
    end <- .cdtData$GalParams$date.range[paste0('end.', c('year', 'mon', 'day', 'hour'))]
    end <- jra55.start.end.time(end)

    rtimes <- seq(start, end, "3 hours")
    ymdh <- format(rtimes, '%Y%m%d%H')

    gribfiles <- paste0(jra_prefix, ".", ymdh)
    gribfiles <- file.path(.cdtData$GalParams$dir.grib, gribfiles)
    gribexist <- file.exists(gribfiles)

    if(!any(gribexist)){
        Insert.Messages.Out(.cdtData$GalParams[['message']][['6']], TRUE, "e", GUI)
        msg <- paste(.cdtData$GalParams[['message']][['7']], jra_prefix)
        Insert.Messages.Out(msg, TRUE, "e", GUI)
        return(-2)
    }

    gribfiles <- gribfiles[gribexist]
    ymdh <- ymdh[gribexist]

    ncfiles <- sprintf(paste0(.cdtData$GalParams$var, "_%s.nc"), ymdh)
    extrdir <- paste0('Extracted_', .cdtData$GalParams$var)

    extrdir <- file.path(.cdtData$GalParams$dir2save, extrdir)
    dir.create(extrdir, showWarnings = FALSE, recursive = TRUE)

    ncfiles <- file.path(extrdir, ncfiles)

    ######################

    bbox <- .cdtData$GalParams$bbox

    parsL <- doparallel.cond(length(gribfiles) > 100)
    ret <- cdt.foreach(seq_along(gribfiles), parsL, GUI, progress = TRUE, FUN = function(g){
        xx <- basename(gribfiles[g])

        if(length(gribpars$var) == 1){
            don <- jra55_nrt.extract.grib(gribfiles[g], gribpars$var, gribpars$hgt, wgrib_exe)
            dat <- don$data
            msg <- don$msg

            if(is.null(dat)){
                cat(msg, '\n')
                # Insert.Messages.Out(msg, TRUE, "e", GUI)
                return(xx)
            }
        }else{
            don <- lapply(seq_along(gribpars$var), function(j){
                jra55_nrt.extract.grib(gribfiles[g], gribpars$var[j], gribpars$hgt[j], wgrib_exe)
            })
            dat <- lapply(don, '[[', 'data')
            msg <- lapply(don, '[[', 'msg')

            inull <- sapply(dat, is.null)
            if(any(inull)){
                msg <- msg[inull]
                for(j in seq_along(msg)){
                    cat(msg[[j]], '\n')
                    # Insert.Messages.Out(msg[[j]], TRUE, "e", GUI)
                }

                return(xx)
            }

            dat <- list(x = dat[[1]]$x, y = dat[[1]]$y,
                        z = lapply(dat, '[[', 'z'))
        }

        ret <- jra55_nrt.format.grib(dat, bbox, ncpars, ncfiles[g])

        if(ret == 0) xx <- NULL

        return(xx)
    })

    inull <- !sapply(ret, is.null)

    if(any(inull)){
        data.name <- "JRA-55 NRT 3 Hourly"
        data.files <- paste(.cdtData$GalParams[['message']][['8']], unlist(ret[inull]))
        xx <- paste(data.name, paste(data.files, collapse = "\n"), sep = "\n")

        if(GUI){
            containertab <- Display_Output_Console_Tab(xx, data.name, cat)
            ntab <- update.OpenTabs('ctxt', containertab)
            tkselect(.cdtEnv$tcl$main$tknotes, ntab)
        }else{
            cat(xx, "\n")
        }
    }

    return(0)
}

