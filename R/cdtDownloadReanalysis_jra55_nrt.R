
jra55_nrt.download.rda.ucar <- function(GalParams, nbfile = 1, GUI = TRUE, verbose = TRUE){
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

    wgrib_exe <- file.path(.cdtDir$dirLocal, 'wgrib', 'wgrib')
    if(!file.exists(wgrib_exe)){
        if(WindowsOS()){
            wgrib_exe <- file.path(.cdtDir$dirLocal, 'wgrib', 'wgrib.exe')
            if(!file.exists(wgrib_exe)){
                Insert.Messages.Out("wgrib not found", TRUE, "e", GUI)
                return(-2)
            }
        }else{
            Insert.Messages.Out("wgrib not found", TRUE, "e", GUI)
            return(-2)
        }
    }

    ######################

    start <- GalParams$date.range[paste0('start.', c('year', 'mon', 'day', 'hour'))]
    start <- jra55.start.end.time(start)
    end <- GalParams$date.range[paste0('end.', c('year', 'mon', 'day', 'hour'))]
    end <- jra55.start.end.time(end)

    rtimes <- seq(start, end, "3 hours")
    yrmo <- format(rtimes, '%Y%m')
    ymdh <- format(rtimes, '%Y%m%d%H')

    ######################

    # jra_nrt <- "https://rda.ucar.edu/data/ds628.8"
    jra_nrt <- "https://data.rda.ucar.edu/ds628.8"
    jra_pth <- switch(GalParams$var,
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

    filenames <- paste0(jra_pth, ".", ymdh)
    urls <- file.path(jra_nrt, jra_pth, yrmo, filenames)
    ncfiles <- sprintf(paste0(GalParams$var, "_%s.nc"), ymdh)

    pars <- list(wgrib = wgrib_exe, GUI = GUI)

    ######################

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

    ncpars <- list(name = name, units = units, longname = longname,
                   prec = "float", missval = -9999, convert = convert_units)

    gribpars <- switch(GalParams$var,
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

    ######################

    data.name <- "JRA-55 NRT 3 Hourly"
    dir.name <- "JRA55_NRT_3Hr_data"
    outdir <- file.path(GalParams$dir2save, dir.name)
    dir.create(outdir, showWarnings = FALSE, recursive = TRUE)

    gribdir <- file.path(outdir, paste0('GRIB_', jra_pth))
    extrdir <- file.path(outdir, paste0('Extracted_', GalParams$var))
    dir.create(gribdir, showWarnings = FALSE, recursive = TRUE)
    dir.create(extrdir, showWarnings = FALSE, recursive = TRUE)

    destfiles <- file.path(gribdir, filenames)
    ncfiles <- file.path(extrdir, ncfiles)

    ######################

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

    ret <- cdt.download.data(urls, destfiles, ncfiles, nbfile, GUI,
                             verbose, data.name, jra55_nrt.download.data,
                             handle = handle_down, ncpars = ncpars, gribpars = gribpars,
                             bbox = GalParams$bbox, pars = pars)

    return(ret)
}

jra55_nrt.download.data <- function(lnk, dest, ncfl, handle, ncpars, gribpars, bbox, pars){
    xx <- basename(dest)
    dc <- try(curl::curl_download(lnk, dest, handle = handle), silent = TRUE)

    if(!inherits(dc, "try-error")){
        if(length(gribpars$var) == 1){
            don <- jra55_nrt.extract.grib(dest, gribpars$var, gribpars$hgt, pars$wgrib)
            dat <- don$data
            msg <- don$msg

            if(is.null(dat)){
                Insert.Messages.Out(msg, TRUE, "e", pars$GUI)
                return(xx)
            }
        }else{
            don <- lapply(seq_along(gribpars$var), function(j){
                jra55_nrt.extract.grib(dest, gribpars$var[j], gribpars$hgt[j], pars$wgrib)
            })
            dat <- lapply(don, '[[', 'data')
            msg <- lapply(don, '[[', 'msg')

            inull <- sapply(dat, is.null)
            if(any(inull)){
                msg <- msg[inull]
                for(j in seq_along(msg))
                    Insert.Messages.Out(msg[[j]], TRUE, "e", pars$GUI)

                return(xx)

                ## if some data are null

                # if(all(inull)) return(xx)
                # dat <- dat[!inull]
                # tmp <- ncpars[c('name', 'units', 'longname')]
                # tmp <- lapply(tmp, function(x) x[!inull])
                # ncpars[c('name', 'units', 'longname')] <- tmp
            }

            dat <- list(x = dat[[1]]$x, y = dat[[1]]$y,
                        z = lapply(dat, '[[', 'z'))
        }

        ret <- jra55_nrt.format.grib(dat, bbox, ncpars, ncfl)

        if(ret == 0) xx <- NULL
    }

    return(xx)
}

jra55_nrt.format.grib <- function(dat, bbox, ncpars, ncfl){
    ix <- dat$x >= bbox$minlon & dat$x <= bbox$maxlon
    iy <- dat$y >= bbox$minlat & dat$y <= bbox$maxlat

    lon <- dat$x[ix]
    lat <- dat$y[iy]

    if(length(ncpars$name) == 1){
        don <- dat$z[ix, iy]
        if(!is.null(ncpars$convert)){
            ncpars$convert$args <- c(list(don), ncpars$convert$args)
            don <- do.call(ncpars$convert$fun, ncpars$convert$args)
        }
        don <- jra55.regrid.data(lon, lat, don)
    }else{
        don <- lapply(dat$z, function(x) x[ix, iy])
        if(!is.null(ncpars$convert)){
            don <- lapply(don, function(x){
                ncpars$convert$args <- c(list(x), ncpars$convert$args)
                do.call(ncpars$convert$fun, ncpars$convert$args)
            })
        }
        don <- lapply(don, function(x){
            jra55.regrid.data(lon, lat, x)
        })
        don <- list(x = don[[1]]$x, y = don[[1]]$y,
            z = lapply(don, '[[', 'z'))
    }

    reanalysis.write.ncdf(don, ncpars, ncfl)

    return(0)
}

jra55_nrt.extract.grib <- function(grib_file, variable, level, wgrib_exe){
    var_lvl <- paste0(':', variable, ':', level, ':')
    inventory <- system(paste(wgrib_exe, grib_file, "-s"), intern = TRUE)

    if(length(inventory) == 0){
        msg <- paste("Unable to read GRIB file", grib_file)
       return(list(data = NULL, msg = msg)) 
    }

    inv.file <- file.path(tempdir(), 'invetory_file')
    out.file <- file.path(tempdir(), 'grib_text')

    ivr <- grep(var_lvl, inventory)
    inventory <- inventory[ivr]
    cat(inventory, file = inv.file, sep = '\n')

    ret <- try(
                system2(wgrib_exe, args = c("-i -text -o", out.file, grib_file),
                        stdin = inv.file, wait = TRUE, stdout = TRUE),
                silent = TRUE)

    if(inherits(ret, "try-error")){
        msg <- paste("Unable to extract the variable", variable,
                     "from GRIB file", grib_file)
       unlink(inv.file)
       unlink(out.file)
       return(list(data = NULL, msg = msg))
    }

    ######

    grib_info <- system(paste(wgrib_exe, "-V -d", ivr, grib_file), intern = TRUE)
    grib_info <- trimws(grib_info)
    grib_info <- grib_info[grib_info != ""]
    lon_pos <- grep('long', grib_info)
    lon_pos <- lon_pos[length(lon_pos)]
    lon_nb <- (lon_pos + 1):(length(grib_info) - 1)
    lon_nbpoint <- trimws(unlist(strsplit(grib_info[lon_nb], " ")))
    lon_nbpoint <- lon_nbpoint[lon_nbpoint != ""]
    lon_nbpoint <- as.numeric(lon_nbpoint)

    ######

    crd_file <- file.path(.cdtDir$Root, "data", "JRA55_Coords.rds")
    crd_gauss <- readRDS(crd_file)

    lon_reg <- lapply(lon_nbpoint, function(lo){
        seq(0, 360 - 0.562, length.out = lo)
    })

    lon_gauss <- crd_gauss$lon
    lon_gauss[lon_gauss < 0] <- 360 + lon_gauss[lon_gauss < 0]

    ######

    tmp <- scan(out.file, skip = 1, quiet = TRUE)
    if(variable %in% c("EVP", "LTRS")){
        tmp[tmp > 1e+10] <- NA
    }
    ie <- cumsum(lon_nbpoint)
    is <- c(1, ie[-length(ie)] + 1)
    tmp <- lapply(seq_along(is), function(j) tmp[is[j]:ie[j]])
    tmp <- lapply(seq_along(lon_nbpoint), function(j){
        x <- tmp[[j]]

        if(lon_nbpoint[j] < 640){
            xlo <- lon_reg[[j]]
            if(sum(!is.na(x)) > 3){
                val <- stats::approx(xlo, x, lon_gauss, rule = 2)
                x <- val$y
            }else{
                x <- rep(NA, 640)
            }
        }

        return(x)
    })
    tmp <- do.call(rbind, tmp)

    lon <- crd_gauss$lon
    lon <- ((lon + 180) %% 360) - 180
    lat <- crd_gauss$lat

    xo <- order(lon)
    yo <- order(lat)
    lon <- lon[xo]
    lat <- lat[yo]
    tmp <- t(tmp[yo, xo])
    dat <- list(x = lon, y = lat, z = tmp)

   unlink(inv.file)
   unlink(out.file)

    return(list(data = dat, msg = NULL))
}
