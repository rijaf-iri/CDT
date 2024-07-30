
jra55_dods.coverage.rda.ucar <- function(GalParams){
    opts <- get_reanalysis.variables('jra55_dods_options.csv')
    opts <- opts[[GalParams$var]]

    jra_dods <- "https://thredds.rda.ucar.edu/thredds/dodsC/aggregations/g/ds628.0"
    dods_page <- paste0(jra_dods, "/", opts$dap_path, ".html")
    dds <- xml2::read_html(dods_page)
    dds <- xml2::xml_find_all(dds, "//pre")
    dds <- xml2::xml_text(dds)
    dds <- strsplit(dds, '\n')[[1]]

    if(opts$search_string == 1) req_search <- "Float64 reftime\\[reftime"
    if(opts$search_string == 2) req_search <- "Float64 time\\[time"
    if(opts$time_origin == 1) time_origin <- "1957-12-31T18:00:00Z"
    if(opts$time_origin == 2) time_origin <- "1958-01-01T00:00:00Z"

    isrch <- grep(req_search, dds)[1]
    last_incr <- dds[isrch]
    last_incr <- stringr::str_match(last_incr, "\\[\\s*(.*?)\\s*\\]")
    last_incr <- last_incr[ ,2]
    last_incr <- trimws(strsplit(last_incr, "=")[[1]][2])
    last_incr <- as.numeric(last_incr)

    origin <- as.POSIXct(time_origin, tz = "GMT", format = "%Y-%m-%dT%H:%M:%SZ")

    last_time <- origin + 24 * 3600 * last_incr/opts$time_factor
    last_time <- last_time - 3 * 3600

    out <- list(name = "Japanese 55-year Reanalysis", timestep = "3 hourly")

    out$end <- format(last_time, '%Y-%m-%d %H:%M:%S')
    out$start <- format(origin, '%Y-%m-%d %H:%M:%S')

    return(out)
}

jra55_dods.download.rda.ucar <- function(GalParams, nbfile = 8, GUI = TRUE, verbose = TRUE){
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

    seq_times <- seq.format.date.time('hourly', GalParams$date.range, 3)
    if(is.null(seq_times)) return(-2)
    start <- min(seq_times)
    end <- max(seq_times)

    ret <- jra55_dods.coverage.rda.ucar(GalParams)
    origin <- as.POSIXct(ret$start, format = '%Y-%m-%d %H:%M:%S', tz = 'UTC')
    last_time <- as.POSIXct(ret$end, format = '%Y-%m-%d %H:%M:%S', tz = 'UTC')

    msg <- paste("Last date of available data", ret$end)
    if(end > last_time){
        seq_times <- seq_times[seq_times <= last_time]
        end <- max(seq_times)
        Insert.Messages.Out(msg, TRUE, "i", GUI)
    }
    if(start > end){
        Insert.Messages.Out(msg, TRUE, "e", GUI)
        return(-2)
    }

    #############

    opts <- get_reanalysis.variables('jra55_dods_options.csv')
    opts <- opts[[GalParams$var]]

    if(opts$timeoffset == 1){
        # start <- start - 3 * 3600
        # end <- end - 3 * 3600
        seq_times <- seq_times  - 3 * 3600
    }

    #############

    split_reftime <- sapply(seq_times, function(x){
        floor(as.numeric(x - origin, units = "days") * opts$time_factor)
    })
    query_reftime <- sapply(split_reftime, function(x) paste0("[", x, ":", 1, ":", x, "]"))

    query_height <- lapply(opts$var_height, function(i) {
        out <- NULL
        if(i == 0) out <- NULL
        if(i == 1) out <- '[0:1:0]'
        if(i == 2) out <- '[0:1:2]'
        out
    })

    #############

    jra_dods <- "https://thredds.rda.ucar.edu/thredds/dodsC/aggregations/g/ds628.0"

    urls <- lapply(seq_along(query_reftime), function(j){
        time_h <- as.numeric(format(seq_times[j], '%H'))
        if(opts$timeoffset == 1){
            if((time_h %% 2) == 0){
                query_timeOffset <- '[0:1:0]'
            }else{
                query_timeOffset <- '[1:1:1]'
            }
        }else{
            query_timeOffset <- NULL
        }

        sapply(seq_along(query_lon), function(i){
            dods <- paste0(jra_dods, "/", opts$dap_path, ".ascii")
            req_time <- paste0("time", query_reftime[j], query_timeOffset)
            req_time <- utils::URLencode(req_time, reserved = TRUE)

            req_var <- sapply(seq_along(opts$var_name), function(v){
                if(opts$varoffset[v] == 1){
                    if((time_h %% 2) == 0){
                        query_varOffset <- '[0:1:0]'
                    }else{
                        query_varOffset <- '[1:1:1]'
                    }
                }else{
                    query_varOffset <- NULL
                }

                paste0(opts$var_name[v], query_reftime[j], query_varOffset,
                       query_height[[v]], query_lat, query_lon[[i]])
            })

            req_var <- paste(req_var, collapse = ",")
            req_var <- utils::URLencode(req_var, reserved = TRUE)
            paste0(dods, "?", req_time, ",", req_var)
        })
    })

    ncpars <- list(name = opts$nc_name, units = opts$nc_units, longname = opts$nc_longname, prec = "float", missval = -9999)
    convert_units <- if(is.null(opts$units_fun)) NULL else list(fun = opts$units_fun, args = opts$units_args)
    pars <- list(nc = ncpars, convert = convert_units, txtvar = opts$var_name,
                 var = GalParams$var, origin = opts$time_origin,
                 timetype = opts$type_path, timeoffset = opts$timeoffset)

    ######################

    data.name <- "JRA-55 3 Hourly"
    dir.name <- "JRA55_3Hr_data"
    outdir <- file.path(GalParams$dir2save, dir.name)
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

    tmpdir <- dirname(dirname(dest[1]))
    error_files <- paste0(basename(dirname(dest[1])), '_error.txt')
    error_files <- file.path(tmpdir, error_files)

    dc <- lapply(seq_along(lnk), function(j){
         ret <- try(curl::curl_download(lnk[j], dest[j]), silent = TRUE)
         rc <- 0
         if(inherits(ret, "try-error")){
            msg <- gsub('[\r\n]', '', ret[1])
            cat(msg, file = error_files, sep = '\n', append = TRUE)
            rc <- 1
         }
         rc
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
        tmp1 <- dat[[1]]
        tmp2 <- dat[[2]]
        nvars <- names(tmp1$var)
        tmp <- lapply(seq_along(nvars), function(j){
            x1 <- tmp1$var[[nvars[j]]]
            x2 <- tmp2$var[[nvars[j]]]
            lon <- c(x1$lon, x2$lon)
            lat <- x1$lat
            xo <- order(lon)
            lon <- lon[xo]
            xdat <- lapply(seq_along(x1$data), function(i){
                y <- rbind(x1$data[[i]], x2$data[[i]])
                y[xo, , drop = FALSE]
            })
            names(xdat) <- names(x1$data)
            list(lon = lon, lat = lat, data = xdat)
        })
        names(tmp) <- nvars
        dat <- list(time = dat[[1]]$time, var = tmp)
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
                eval_function(pars$convert$fun, pars$convert$args, x)
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
