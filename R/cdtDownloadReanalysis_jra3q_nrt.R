
jra3q_nrt.coverage.rda.ucar <- function(GalParams){
    out <- list(name = "Japanese Reanalysis for Three Quarters of a Century, Near Real-Time", timestep = "hourly")
    url_nrt <- "https://rda.ucar.edu/datasets/ds640.1/"
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
        if(!grepl('minmax_surf', x)) return(NULL)
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

jra3q_nrt.download.rda.ucar <- function(GalParams, nbfile = 1, GUI = TRUE, verbose = TRUE){
    crd_file <- file.path(.cdtDir$Root, "data", "JRA3Q_Coords.rds")
    crd_gauss <- readRDS(crd_file)
    crd_gauss$lon <- ((crd_gauss$lon + 180) %% 360) - 180
    xo <- order(crd_gauss$lon)
    yo <- order(crd_gauss$lat)
    xlon <- crd_gauss$lon[xo]
    xlat <- crd_gauss$lat[yo]

    ix <- xlon >= GalParams$bbox$minlon & xlon <= GalParams$bbox$maxlon
    iy <- xlat >= GalParams$bbox$minlat & xlat <= GalParams$bbox$maxlat

    if(!any(ix) | !any(iy)){
        Insert.Messages.Out("Invalid area of interest", TRUE, "e", GUI)
        return(-2)
    }

    xycoords <- list(xlon = xlon[ix], xlat = xlat[iy])
    bbox_coords <- c(GalParams$bbox, xycoords)

    ###########
    wgrib2 <- if(WindowsOS()) 'wgrib2.exe' else 'wgrib2'
    wgrib_exe <- file.path(.cdtDir$dirLocal, 'wgrib2', wgrib2)
    if(!file.exists(wgrib_exe)){
        Insert.Messages.Out("wgrib2 not found", TRUE, "e", GUI)
        return(-2)
    }

    ###########
    rtimes <- seq.format.date.time('hourly', GalParams$date.range, 1)
    if(is.null(rtimes)) return(-2)
    yrmo <- format(rtimes, '%Y%m')
    ymdh <- format(rtimes, '%Y%m%d%H')

    ############
    opts <- get_reanalysis.variables('jra3q_nrt_options.csv')
    opts <- opts[[GalParams$var]]

    jra_nrt <- "https://data.rda.ucar.edu/ds640.1"
    filenames <- paste0(opts$grib_path, ".", ymdh)
    urls <- file.path(jra_nrt, opts$grib_path, yrmo, filenames)
    ncfiles <- sprintf(paste0(GalParams$var, "_%s.nc"), ymdh)

    pars <- list(wgrib = wgrib_exe, GUI = GUI)
    convert_units <- if(is.null(opts$units_fun)) NULL else list(fun = opts$units_fun, args = opts$units_args)
    ncpars <- list(name = opts$nc_name, units = opts$nc_units, longname = opts$nc_longname,
                   prec = "float", missval = -9999, convert = convert_units)
    gribpars <- list(var = opts$grib_name, hgt = opts$grib_height, srch = opts$grib_search)

    ############
    data.name <- "JRA-3Q NRT Hourly"
    dir.name <- "JRA3Q_NRT_1Hr_data"
    outdir <- file.path(GalParams$dir2save, dir.name)
    gribdir <- file.path(outdir, paste0('GRIB_', opts$grib_path))
    extrdir <- file.path(outdir, paste0('Extracted_', GalParams$var))
    dir.create(gribdir, showWarnings = FALSE, recursive = TRUE)
    dir.create(extrdir, showWarnings = FALSE, recursive = TRUE)

    destfiles <- file.path(gribdir, filenames)
    ncfiles <- file.path(extrdir, ncfiles)

    ############

    ret <- cdt.download.data(urls, destfiles, ncfiles, nbfile, GUI,
                             verbose, data.name, jra3q_nrt.download.data,
                             ncpars = ncpars, gribpars = gribpars,
                             bbox = bbox_coords, pars = pars)
    return(ret)

}

jra3q_nrt.download.data <- function(lnk, dest, ncfl, ncpars, gribpars, bbox, pars){
    xx <- basename(dest)
    dc <- try(curl::curl_download(lnk, dest), silent = TRUE)

    if(!inherits(dc, "try-error")){
        if(length(gribpars$var) == 1){
            don <- jra3q_nrt.extract.grib2(dest, gribpars$var, gribpars$hgt,
                                           gribpars$srch, bbox, pars$wgrib)
            dat <- don$data
            msg <- don$msg

            if(is.null(dat)){
                Insert.Messages.Out(msg, TRUE, "e", pars$GUI)
                return(xx)
            }
        }else{
            don <- lapply(seq_along(gribpars$var), function(j){
                jra3q_nrt.extract.grib2(dest, gribpars$var[j], gribpars$hgt[j],
                                        gribpars$srch[j], bbox, pars$wgrib)
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

        ret <- jra3q_nrt.format.grib2(dat, ncpars, ncfl)

        if(ret == 0) xx <- NULL
    }

    return(xx)
}

jra3q_nrt.format.grib2 <- function(dat, ncpars, ncfl){
    if(!is.null(ncpars$convert)){
        if(length(ncpars$name) == 1){
            dat$z <- eval_function(ncpars$convert$fun, ncpars$convert$args, dat$z)
        }else{
            dat$z <- lapply(dat$z, function(x){
                eval_function(ncpars$convert$fun, ncpars$convert$args, x)
            })
        }
    }

    reanalysis.write.ncdf(dat, ncpars, ncfl)

    return(0)
}

jra3q_nrt.extract.grib2 <- function(grib_file, variable, level, search, bbox, wgrib_exe){
    var_lvl <- paste0(':', variable, ':', level, ':')
    inventory <- system(paste(wgrib_exe, grib_file, "-s"), intern = TRUE)

    inv.file <- file.path(tempdir(), 'invetory_file')
    out.file <- file.path(tempdir(), 'grib_text')
    on.exit({
       unlink(inv.file)
       unlink(out.file)
    })

    ivr <- grep(var_lvl, inventory)
    if(!is.null(search)){
        isrch <- grep(search, inventory[ivr])
        ivr <- ivr[isrch]
    }
    if(length(ivr) == 0){
        msg <- paste("Unable to extract the variable", var_lvl,
                     "from GRIB2 file", grib_file)
       return(list(data = NULL, msg = msg))
    }
    inventory <- inventory[ivr]
    cat(inventory, file = inv.file, sep = '\n')

    ret <- try(
               system2(wgrib_exe, args = c('-i', grib_file, '-rpn "-999:swap:merge"', '-spread', out.file),
                       stdin = inv.file, wait = TRUE, stdout = TRUE),
               silent = TRUE)
    if(inherits(ret, "try-error")){
        msg <- paste("Unable to extract the variable", variable,
                     "from GRIB2 file", grib_file)
       return(list(data = NULL, msg = msg))
    }

    tmp <- utils::read.table(out.file, sep = ",", skip = 1,
                             col.names = c('x', 'y', 'z'))
    tmp$x <- ((tmp$x + 180) %% 360) - 180
    ix <- tmp$x >= bbox$minlon & tmp$x <= bbox$maxlon
    iy <- tmp$y >= bbox$minlat & tmp$y <= bbox$maxlat
    tmp <- tmp[ix & iy, ]
    tmp$z[tmp$z == -999] <- NA

    xygrid <- list(x = bbox$xlon, y = bbox$xlat)
    tmp <- jra3q.regrid.data(tmp, xygrid)

    return(list(data = tmp, msg = NULL))
}

jra3q.regrid.data <- function(tmp, xygrid){
    index_y <- split(seq_along(tmp$y), tmp$y)
    tmp_g <- lapply(index_y, function(iy){
        xdat <- tmp[iy, ]
        if(all(is.na(xdat$z))) return(rep(NA, length(xygrid$x)))
        new_grid <- data.frame(x = xygrid$x, y = xdat$y[1])
        xdat <- xdat[!is.na(xdat$z), ]
        x_g <- gstat::krige(z~1, locations = ~x+y, data = xdat,
                            newdata = new_grid, nmax = 1, maxdist = 0.375,
                            debug.level = 0)
        x_g$var1.pred
    })

    tmp_g <- do.call(cbind, tmp_g)
    dimnames(tmp_g) <- NULL
    xygrid$z <- tmp_g

    return(xygrid)
}
