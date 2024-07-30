
jra3q_dods.coverage.rda.ucar <- function(GalParams){
    out <- list(name = "Japanese Reanalysis for Three Quarters of a Century", timestep = "hourly")
    jra3q_dods <- "https://thredds.rda.ucar.edu/thredds/catalog/files/g/ds640.0/"

    opts <- get_reanalysis.variables('jra3q_dods_options.csv')
    opts <- opts[[GalParams$var]]

    catalog <- paste0(jra3q_dods, opts$dap_path, '/catalog.html')
    tmp <- jra3q_dods.rda.ucar.dates(catalog, 'directory')
    if(is.null(tmp)) return(out)
    end_d <- tmp[length(tmp)]

    url <- paste0(jra3q_dods, opts$dap_path, '/', end_d, '/catalog.html')
    tmp <- jra3q_dods.rda.ucar.dates(url, 'file')
    if(is.null(tmp)) return(out)
    end_d <- tmp[length(tmp)]

    end_d <- as.POSIXct(end_d, format = '%Y%m%d%H', tz = 'UTC')
    out$end <- format(end_d, '%Y-%m-%d %H:%M:%S')
    out$start <- "1965-01-01 00:00:00"

    return(out)
}

jra3q_dods.rda.ucar.table <- function(url){
    ret <- httr::GET(url)
    if(httr::status_code(ret) != 200){
        Insert.Messages.httr(ret)
        return(NULL)
    }
    ret <- httr::content(ret)

    tmp <- rvest::html_table(ret, fill = TRUE)[[1]]
    tmp <- as.data.frame(tmp[-1, ])
    tmp[tmp[, 1] != "", 1]
}

jra3q_dods.rda.ucar.dates <- function(url, type){
    tmp <- jra3q_dods.rda.ucar.table(url)
    if(length(tmp) == 0) return(NULL)

    if(type == 'file'){
        ret <- strsplit(tmp, '\\.')
        ret <- sapply(ret, function(x) utils::tail(x, n = 2)[1])
        ret <- trimws(ret)
        ret <- sapply(strsplit(ret, '_'), '[[', 2)
    }else if(type == 'directory'){
        ret <- gsub('[^[:digit:]]', '', tmp)
    }else return(NULL)

    ret <- ret[ret != ""]
    ret <- ret[nchar(ret) >= 6]
    if(length(ret) == 0) return(NULL)
    ret <- sort(ret)

    return(ret)
}

jra3q_dods.download.rda.ucar <- function(GalParams, nbfile = 4, GUI = TRUE, verbose = TRUE){
    crd_file <- file.path(.cdtDir$Root, "data", "JRA3Q_Coords.rds")
    crd_gauss <- readRDS(crd_file)
    xlat <- crd_gauss$lat
    xlon <- ((crd_gauss$lon + 180) %% 360) - 180

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

    opts <- get_reanalysis.variables('jra3q_dods_options.csv')
    opts <- opts[[GalParams$var]]
    fcst_phy <- opts$dap_path %in% c('fcst_phyland', 'fcst_phy2m')
    soil_data <- opts$cdt_var %in% c("soilm", "soilt")

    seq_times <- seq.format.date.time('hourly', GalParams$date.range, 1)
    if(is.null(seq_times)) return(-2)
    if(fcst_phy) seq_times <- seq_times + 3600
    yrmo <- format(seq_times, '%Y%m')
    nbday <- nb.Day.Of.Month(yrmo)

    start_mon <- paste0(yrmo, '0100')
    end_mon <- paste0(yrmo, nbday, '23')

    if(opts$dap_path == "fcst_land"){
        days <- as.numeric(format(seq_times, '%d'))
        i16 <- days >= 16
        start_mon[i16] <- paste0(yrmo[i16], '1600')
        end_mon[!i16] <- paste0(yrmo[!i16], '1523')
    }

    # by n hourly
    brks_hr <- "6 hour"
    origin <- as.POSIXct(start_mon, format = '%Y%m%d%H', tz = 'UTC')
    istart_mon <- split(seq_along(start_mon), start_mon)
    req_time <- lapply(istart_mon, function(j){
        stime <- seq_times[j]
        org <- origin[j][1]
        smon <- start_mon[j][1]
        emon <- end_mon[j][1]
        ct <- cut(stime, breaks = brks_hr)
        it <- split(seq_along(stime), ct)
        nl <- sapply(it, length)
        it <- it[nl > 0]
        rtime <- lapply(it, function(i){
            tt <- range(stime[i])
            ff <- paste0(format(tt, '%Y%m%d%H'), collapse = '_')
            rt <- as.numeric(tt - org, units = "hours")
            qtime <- paste0("[", rt[1], ":", 1, ":", rt[2], "]")
            list(fname = ff, qtime = qtime)
        })
        fname <- sapply(rtime, '[[', 'fname')
        qtime <- sapply(rtime, '[[', 'qtime')
        list(fname = fname, qtime = qtime,
             smon = rep(smon, length(qtime)),
             emon = rep(emon, length(qtime)))
    })

    start_mon <- unname(do.call(c, lapply(req_time, '[[', 'smon')))
    end_mon <- unname(do.call(c, lapply(req_time, '[[', 'emon')))
    query_time <- unname(do.call(c, lapply(req_time, '[[', 'qtime')))
    filenames <- unname(do.call(c, lapply(req_time, '[[', 'fname')))
    ncfiles_time <- filenames
    pth_yrmo <- substr(start_mon, 1, 6)

    #############

    jra_dods <- "https://thredds.rda.ucar.edu/thredds/dodsC/files/g/ds640.0"
    req_depth <- if(soil_data) '[0:1:6]' else NULL

    urls <- lapply(seq_along(query_time), function(j){
        lapply(seq_along(opts$jra_var), function(l){
            endpoints <- paste0('jra3q.', opts$dap_path, '.', opts$jra_code[l], '.', opts$jra_var[l],
                                '.', start_mon[j], '_', end_mon[j], '.nc.ascii?', opts$jra_var[l])
            endpoints <- paste0(endpoints, query_time[j], req_depth, query_lat)
            dods <- paste(jra_dods, opts$dap_path, pth_yrmo[j], endpoints, sep = '/')
            sapply(seq_along(query_lon), function(i){
                paste0(dods, query_lon[[i]])
            })
        })
    })

    convert_units <- if(is.null(opts$units_fun)) NULL else list(fun = opts$units_fun, args = opts$units_args)
    opts$convert <- convert_units

    #############

    data.name <- "JRA-3Q Hourly"
    dir.name <- "JRA3Q_1Hr_data"
    outdir <- file.path(GalParams$dir2save, dir.name)
    outdir <- file.path(outdir, paste0('JRA3Q_', GalParams$var))
    dir.create(outdir, showWarnings = FALSE, recursive = TRUE)

    destfiles <- lapply(seq_along(urls), function(j){
        lapply(seq_along(opts$jra_var), function(l){
            sapply(seq_along(query_lon), function(i){
                tmp_file <- paste0(opts$cdt_var, "_", opts$jra_var[l],
                                   "_", filenames[j], "_", i, ".txt")
                file.path(outdir, tmp_file)
            })
        })
    })

    ##########

    ret <- cdt.download.data(urls, destfiles, ncfiles_time, nbfile, GUI, verbose,
                             data.name, jra3q_dods.download.data, opts = opts)
    return(ret)
}

jra3q_dods.download.data <- function(lnk, dest, ncfl, opts){
    on.exit(lapply(dest, unlink))

    dest <- dest[[1]]
    lnk <- lnk[[1]]
    xx <- ncfl

    tmpdir <- dirname(dirname(dest[[1]][1]))
    error_files <- paste0(basename(dirname(dest[[1]][1])), '_error.txt')
    error_files <- file.path(tmpdir, error_files)

    dc <- lapply(seq_along(lnk), function(j){
        lapply(seq_along(lnk[[j]]), function(i){
            ret <- try(curl::curl_download(lnk[[j]][i], dest[[j]][i]), silent = TRUE)
            rc <- 0
            if(inherits(ret, "try-error")){
                msg <- gsub('[\r\n]', '', ret[1])
                cat(msg, file = error_files, sep = '\n', append = TRUE)
                rc <- 1
            }
            rc
        })
    })

    if(all(unlist(dc) == 0)){
        ret <- jra3q_dods.format.data(dest, opts)
        if(ret == 0) xx <- NULL
    }

    return(xx)
}

jra3q_dods.format.data <- function(dest, opts){
    soil_data <- opts$cdt_var %in% c("soilm", "soilt")
    fcst_phy <- opts$dap_path %in% c('fcst_phyland', 'fcst_phy2m')
    dat <- lapply(seq_along(dest), function(j){
        tmp <- lapply(dest[[j]], jra3q_dods.parse.ascii,
                      jra_var = opts$jra_var[j],
                      cdt_var = opts$cdt_var,
                      convert = opts$convert)
        times <- tmp[[1]]$times
        if(fcst_phy) times <- times - 3600
        depth <- tmp[[1]]$depth
        lat <- tmp[[1]]$lat

        if(length(tmp) == 2){
            lon <- c(tmp[[1]]$lon, tmp[[2]]$lon)
            xo <- order(lon)
            lon <- lon[xo]
            don <- lapply(seq_along(tmp[[1]]$data), function(i){
                if(soil_data){
                    lapply(seq_along(tmp[[1]]$data[[i]]), function(l){
                        xx <- rbind(tmp[[1]]$data[[i]][[l]], tmp[[2]]$data[[i]][[l]])
                        xx[xo, ]
                    })
                }else{
                    xx <- rbind(tmp[[1]]$data[[i]], tmp[[2]]$data[[i]])
                    xx[xo, ]
                }
            })
        }else{
            lon <- tmp[[1]]$lon
            don <- tmp[[1]]$data
        }

        list(times = times, lon = lon, lat = lat, depth = depth, data = don)
    })

    ncdir <- dirname(dest[[1]][1])
    ncfiles <- format(dat[[1]]$times, '%Y%m%d%H')
    ncfiles <- paste0(opts$cdt_var, '_', ncfiles, '.nc')
    ncfiles <- file.path(ncdir, ncfiles)

    dx <- ncdf4::ncdim_def("Lon", "degreeE", dat[[1]]$lon, longname = "Longitude")
    dy <- ncdf4::ncdim_def("Lat", "degreeN", dat[[1]]$lat, longname = "Latitude")

    if(soil_data){
        depth <- dat[[1]]$depth
        n_depth <- paste(c(0, utils::head(depth, -1)), depth[-1], sep = '-')
        n_depth <- paste(n_depth, 'm')
        ncpars <- lapply(seq_along(dat), function(j){
            nc_longname <- paste(opts$nc_longname[j], n_depth)
            nc_name <- paste0(opts$nc_name[j], '_l', seq_along(n_depth))
            nc_units <- rep(opts$nc_units[j], length(n_depth))
            list(name = nc_name, longname = nc_longname, units = nc_units)
        })
        ncpars <- Reduce(append.list, ncpars)
        ncgrd <- lapply(seq_along(ncpars$name), function(j){
            ncdf4::ncvar_def(ncpars$name[j], ncpars$units[j],
                            list(dx, dy), -9999, ncpars$longname[j],
                            'float', compression = 9)
        })

        nd <- length(depth)
        for(j in seq_along(ncfiles)){
            nc <- ncdf4::nc_create(ncfiles[j], ncgrd)
            for(i in seq_along(dat)){
                for(l in 1:nd){
                    don <- dat[[i]]$data[[j]][[l]]
                    don[is.na(don)] <- -9999
                    k <- nd * (i - 1) + l
                    ncdf4::ncvar_put(nc, ncgrd[[k]], don)
                }
            }
            ncdf4::nc_close(nc)
        }
    }else{
        ncgrd <- lapply(seq_along(opts$nc_name), function(j){
            ncdf4::ncvar_def(opts$nc_name[j], opts$nc_units[j],
                            list(dx, dy), -9999, opts$nc_longname[j],
                            "float", compression = 9)
        })
        for(j in seq_along(ncfiles)){
            nc <- ncdf4::nc_create(ncfiles[j], ncgrd)
            for(i in seq_along(dat)){
                don <- dat[[i]]$data[[j]]
                don[is.na(don)] <- -9999
                ncdf4::ncvar_put(nc, ncgrd[[i]], don)
            }
            ncdf4::nc_close(nc)
        }
    }

    return(0)
}

jra3q_dods.parse.ascii <- function(filetxt, jra_var, cdt_var, convert){
    jra <- readLines(filetxt)
    jra <- trimws(jra)
    soil_data <- cdt_var %in% c("soilm", "soilt")

    tt <- jra3q_dods.get.var(jra, jra_var, 'time')
    tt <- as.numeric(trimws(tt[[1]]))
    lon <- jra3q_dods.get.var(jra, jra_var, 'lon')
    lon <- as.numeric(trimws(lon[[1]]))
    lat <- jra3q_dods.get.var(jra, jra_var, 'lat')
    lat <- as.numeric(trimws(lat[[1]]))
    if(soil_data){
        depth <- jra3q_dods.get.var(jra, jra_var, 'depth_below_land_surface')
        depth <- as.numeric(trimws(depth[[1]]))
        # name_dim <- c('time', 'depth', 'lat', 'lon')
    }else{
        depth <- NULL
        # name_dim <- c('time', 'lat', 'lon')
    }

    ###
    ndim <- jra3q_dods.get.dim(jra, jra_var, jra_var)
    dat <- jra3q_dods.get.var(jra, jra_var, jra_var)
    index <- lapply(dat, '[', 1)
    index <- lapply(index, jra3q_dods.get.index)
    index <- lapply(index, function(ix){
        n <- utils::tail(ndim, 1)
        ix <- matrix(ix + 1, nrow = 1)
        cbind(ix[rep(1, n), , drop = FALSE], seq(n))
    })
    index <- do.call(rbind, index)

    dat <- lapply(dat, '[', -1)
    dat <- do.call(c, dat)
    dat <- as.numeric(trimws(dat))
    # _FillValue: 9.999E20
    dat[dat > 1e+20] <- NA

    if(!is.null(convert)){
        dat <- eval_function(convert$fun, convert$args, dat)
    }

    ###
    itime <- split(seq(nrow(index)), index[, 1])
    if(soil_data){
        tmp <- lapply(itime, function(j){
            ix <- index[j, -1]
            xx <- dat[j]
            idepth <- split(seq(nrow(ix)), ix[, 1])
            lapply(idepth, function(i){
                iy <- ix[i, -1]
                don <- cbind(lon[iy[, 2]], lat[iy[, 1]], xx[i])
                don <- reshapeXYZ2Matrix(don)
                don$z
            })
        })
    }else{
        tmp <- lapply(itime, function(j){
            ix <- index[j, -1]
            xx <- dat[j]
            don <- cbind(lon[ix[, 2]], lat[ix[, 1]], xx)
            don <- reshapeXYZ2Matrix(don)
            don$z
        })
    }

    times <- as.POSIXct(tt * 3600, origin = '1900-01-01', tz = 'UTC')
    lat <- sort(lat)
    lon <- ((lon + 180) %% 360) - 180

    list(times = times, depth = depth, lon = lon, lat = lat, data = tmp)
}

jra3q_dods.get.var <- function(x, vr, pr = NULL){
    if(is.null(pr)){
        sx <- paste0("^", vr, "\\[")
    }else{
        sx <- paste0("^", vr, "\\.", pr, "\\[")
    }

    i1 <- grep(sx, x)
    if(length(i1) == 0) return(NULL)

    i2 <- jra3q_dods.split.line(x, i1)
    v <- x[(i1 + 1):(i2 - 1)]
    v <- strsplit(v, ",")

    return(v)
}

jra3q_dods.get.dim <- function(x, vr, pr = NULL){
    if(is.null(pr)){
        sx <- paste0("^", vr, "\\[")
    }else{
        sx <- paste0("^", vr, "\\.", pr, "\\[")
    }
    i1 <- grep(sx, x)
    jra3q_dods.get.index(x[i1])
}

jra3q_dods.split.line <- function(x, s){
    nl <- 0
    for(i in s:length(x)){
        if(x[i] == ""){
            nl <- i
            break
        }
    }
    return(nl)
}

jra3q_dods.get.index <- function(x){
    ix <- gregexpr("(?<=\\[).*?(?=\\])", x, perl = TRUE)
    v <- regmatches(x, ix)
    as.numeric(v[[1]])
}

