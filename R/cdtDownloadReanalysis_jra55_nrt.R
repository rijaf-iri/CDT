
jra55_nrt.coverage.rda.ucar <- function(GalParams){
    out <- list(name = "Japanese 55-year Reanalysis, Near Real-Time", timestep = "3 hourly")
    url_nrt <- "https://rda.ucar.edu/datasets/ds628.8/"
    ret <- httr::GET(url_nrt)
    if(httr::status_code(ret) != 200){
        Insert.Messages.httr(ret)
        return(out)
    }
    ret <- httr::content(ret)
    ret <- rvest::html_nodes(ret, xpath = "//span[@id='DP1']")
    ret <- rvest::html_nodes(ret, ".ms-3")
    ret <- as.list(rvest::html_text(ret))
    ret <- lapply(ret, function(x){
        if(!grepl('Minimum-Maximum', x)) return(NULL)
        x <- strsplit(x, '\\(')
        x <- x[[1]][1]
        x <- strsplit(x, 'to')
        x <- trimws(x[[1]])
        as.POSIXct(x, format = '%Y-%m-%d %H:%M +%S', tz = 'GMT')
    })
    inull <- sapply(ret, is.null)
    if(all(inull)) return(out)
    ret <- ret[!inull]
    ret <- format(ret[[1]], '%Y-%m-%d %H:%M:%S')
    out$start <- ret[1]
    out$end <- ret[2]
    return(out)
}

jra55_nrt.download.rda.ucar <- function(GalParams, nbfile = 1, GUI = TRUE, verbose = TRUE){
    xlon <- seq(-179.9997300, 179.4377600, length.out = 640)
    xlat <- seq(-89.5700900, 89.5700900, length.out = 320)
    ix <- xlon >= GalParams$bbox$minlon & xlon <= GalParams$bbox$maxlon
    iy <- xlat >= GalParams$bbox$minlat & xlat <= GalParams$bbox$maxlat

    if(!any(ix) | !any(iy)){
        Insert.Messages.Out("Invalid area of interest", TRUE, "e", GUI)
        return(-2)
    }

    ###########
    wgrib <- if(WindowsOS()) 'wgrib.exe' else 'wgrib'
    wgrib_exe <- file.path(.cdtDir$dirLocal, 'wgrib', 'wgrib')
    if(!file.exists(wgrib_exe)){
        Insert.Messages.Out("wgrib not found", TRUE, "e", GUI)
        return(-2)
    }

    ############
    rtimes <- seq.format.date.time('hourly', GalParams$date.range, 3)
    if(is.null(rtimes)) return(-2)
    yrmo <- format(rtimes, '%Y%m')
    ymdh <- format(rtimes, '%Y%m%d%H')

    ############
    opts <- get_reanalysis.variables('jra55_nrt_options.csv')
    opts <- opts[[GalParams$var]]

    jra_nrt <- "https://data.rda.ucar.edu/ds628.8"
    filenames <- paste0(opts$grib_path, ".", ymdh)
    urls <- file.path(jra_nrt, opts$grib_path, yrmo, filenames)
    ncfiles <- sprintf(paste0(GalParams$var, "_%s.nc"), ymdh)

    pars <- list(wgrib = wgrib_exe, GUI = GUI)
    convert_units <- if(is.null(opts$units_fun)) NULL else list(fun = opts$units_fun, args = opts$units_args)
    ncpars <- list(name = opts$nc_name, units = opts$nc_units, longname = opts$nc_longname,
                   prec = "float", missval = -9999, convert = convert_units)
    gribpars <- list(var = opts$grib_name, hgt = opts$grib_height)

    ############

    data.name <- "JRA-55 NRT 3 Hourly"
    dir.name <- "JRA55_NRT_3Hr_data"
    outdir <- file.path(GalParams$dir2save, dir.name)
    gribdir <- file.path(outdir, paste0('GRIB_', opts$grib_path))
    extrdir <- file.path(outdir, paste0('Extracted_', GalParams$var))
    dir.create(gribdir, showWarnings = FALSE, recursive = TRUE)
    dir.create(extrdir, showWarnings = FALSE, recursive = TRUE)

    destfiles <- file.path(gribdir, filenames)
    ncfiles <- file.path(extrdir, ncfiles)

    ############

    ret <- cdt.download.data(urls, destfiles, ncfiles, nbfile, GUI,
                             verbose, data.name, jra55_nrt.download.data,
                             ncpars = ncpars, gribpars = gribpars,
                             bbox = GalParams$bbox, pars = pars)
    return(ret)
}

jra55_nrt.download.data <- function(lnk, dest, ncfl, ncpars, gribpars, bbox, pars){
    xx <- basename(dest)
    dc <- try(curl::curl_download(lnk, dest), silent = TRUE)

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
            don <- eval_function(ncpars$convert$fun, ncpars$convert$args, don)
        }
        don <- jra55.regrid.data(lon, lat, don)
    }else{
        don <- lapply(dat$z, function(x) x[ix, iy])
        if(!is.null(ncpars$convert)){
            don <- lapply(don, function(x){
                eval_function(ncpars$convert$fun, ncpars$convert$args, x)
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
    on.exit({
       unlink(inv.file)
       unlink(out.file)
    })

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
    if(variable %in% c("EVP", "LTRS", "ROF", "SoilT", "SoilW", "TSG")){
        tmp[tmp > 1e+6] <- NA
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

    return(list(data = dat, msg = NULL))
}
