
jra55.download.rda.ucar <- function(GalParams, nbfile = 1, GUI = TRUE, verbose = TRUE){
    on.exit(curl::handle_reset(handle))

    jracrd0 <- file.path(.cdtDir$Root, "data", "JRA55_Coords.rds")
    jra.crd <- readRDS(jracrd0)
    xlon <- jra.crd$lon
    xlon <- ((xlon + 180) %% 360) - 180
    xlat <- jra.crd$lat

    ix <- xlon >= GalParams$bbox$minlon & xlon <= GalParams$bbox$maxlon
    iy <- xlat >= GalParams$bbox$minlat & xlat <= GalParams$bbox$maxlat

    if(!any(ix) | !any(iy)) return(-2)

    rix <- rle(ix)
    ie <- cumsum(rix$lengths)
    is <- c(1, (ie + 1)[-length(ie)])
    ix <- lapply(seq_along(is), function(j) is[j]:ie[j])
    ilon <- lapply(ix[rix$values], function(x) range(x) - 1 )
    lon <- lapply(ilon, function(x) paste0("[", x[1], ":", 1, ":", x[2], "]"))

    ilat <- range(which(iy)) - 1
    lat <- paste0("[", ilat[1], ":", 1, ":", ilat[2], "]")

    ######################
    height <- "[0:1:0]"
    times <- "[0:1:1]"

    start <- GalParams$date.range[paste0('start.', c('year', 'mon', 'day', 'hour'))]
    start <- jra55.start.end.time(start)
    end <- GalParams$date.range[paste0('end.', c('year', 'mon', 'day', 'hour'))]
    end <- jra55.start.end.time(end)

    ncfiles.range <- seq(start, end, "3 hours")
    split_years <- split(ncfiles.range, format(ncfiles.range, "%Y"))
    start_end <- lapply(split_years, function(x) split(x, ceiling(seq_along(x)/80)))
    start_end <- unlist(start_end, recursive = FALSE)
    ncfiles.range <- lapply(start_end, function(x) range(x))
    start_years <- sapply(ncfiles.range, function(y) as.numeric(format(y[1], "%Y")))

    start_end <- lapply(seq_along(start_end), function(j){
        stime <- paste0(start_years[j] - 1, "-12-31T18:00:00Z")
        origin <- as.POSIXct(stime, tz = "GMT", format = "%Y-%m-%dT%H:%M:%SZ")
        floor(as.numeric(ncfiles.range[[j]] - origin, units = "days") * 8) - 1
    })

    rftime <- sapply(start_end, function(x) paste0("[", x[1], ":", 1, ":", x[2], "]"))
    filenames <- sapply(seq_along(start_end), function(j){
        paste(c(start_years[j], start_end[[j]]), collapse = "-")
    })

    endpoint_years <- paste0("JRA-55_3-Hourly_Model_Resolution_2-Dimensional_Minimum-Maximum_Diagnostic_Fields-", start_years, ".ascii")

    ######################
    varname <- switch(GalParams$var,
                      "tmax" = "Maximum_temperature_height_above_ground_3_Hour",
                      "tmin" = "Minimum_temperature_height_above_ground_3_Hour",
                      NULL)
    longname <- switch(GalParams$var,
                       "tmax" = "JRA55 3 Hourly Maximum temperature at 2 m above ground",
                       "tmin" = "JRA55 3 Hourly Minimum temperature at 2 m above ground",
                          NULL)
    pars <- list(varid = GalParams$var, varname = varname, longname = longname)
    dods <- paste0("https://rda.ucar.edu/thredds/dodsC/aggregations/g/ds628.0/31/", endpoint_years)

    urls <- lapply(seq_along(rftime), function(j){
        sapply(lon, function(x){
            req <- paste0(varname, rftime[j], height, lat, x)
            paste0(dods[j], "?", req)
        })
    })

    ######################
    data.name <- "JRA-55 3 Hourly"
    outdir <- file.path(GalParams$dir2save, paste0("JRA55_3Hr_", GalParams$var))
    dir.create(outdir, showWarnings = FALSE, recursive = TRUE)

    destfiles <- lapply(seq_along(urls), function(j){
        sapply(seq_along(lon), function(i)
            file.path(outdir, paste0(varname, "_", filenames[j], "_", i, ".txt")))
    })

    ######################

    handle <- curl::new_handle()
    curl::handle_setopt(handle, username = GalParams$login$usr, password = GalParams$login$pwd)

    ret <- cdt.download.data(urls, destfiles, ncfiles.range, nbfile, GUI,
                             verbose, data.name, jra55.download.data,
                             handle = handle, pars = pars)

    return(ret)
}

#################################################################################

jra55.download.data <- function(lnk, dest, ncfl, handle, pars){
    on.exit(lapply(dest, unlink))
    
    dest <- dest[[1]]
    lnk <- lnk[[1]]
    ncfl <- ncfl[[1]]
    xx <- paste(format(ncfl, "%Y%m%d%H"), collapse = "-")

    dc <- lapply(seq_along(lnk), function(j){
         ret <- try(curl::curl_download(lnk[j], dest[j], handle = handle), silent = TRUE)
         if(inherits(ret, "try-error")) 1 else 0
    })

    if(all(unlist(dc) == 0)){
        ret <- jra55.format.data(dest, pars)
        if(ret == 0) xx <- NULL
    }
    return(xx)
}

jra55.format.data <- function(dest, pars){
    dat <- lapply(dest, jra55.parse.dods.ascii, varname = pars$varname)
    don <- dat[[1]]
    if(length(dat) == 2){
        don$lon <- c(dat[[2]]$lon, don$lon)
        tmp <- lapply(seq_along(don$data), function(j){
            rbind(dat[[2]]$data[[j]], don$data[[j]])
        })
        names(tmp) <- names(don$data)
        don$data <- tmp
    }

    don <- jra55.regrid.data(don)
    jra55.write.ncdf(don, pars$varid, pars$longname, dirname(dest[1]))
   
    return(0)
}

jra55.start.end.time <- function(x){
    div3 <- x[[4]] %% 3
    if(div3 != 0) x[[4]] <- x[[4]] - div3
    x <- paste(unlist(x), collapse = "-")
    as.POSIXct(x, tz = "GMT", format = "%Y-%m-%d-%H")
}

jra55.parse.dods.mat <- function(x){
    xx <- strsplit(x, ",")[[1]]
    tt <- regmatches(xx[1], gregexpr("(?<=\\[).*?(?=\\])", xx[1], perl = TRUE))[[1]]
    vv <- unlist(strsplit(xx[-1], ","), use.names = FALSE)
    vv <- as.numeric(str_trim(vv))
    list(tt, vv)
}

jra55.parse.dods.ascii <- function(filetxt, varname){
    pattern <- list(paste0("^", varname, ".", varname),
                    paste0("^", varname, ".", "time"),
                    paste0("^", varname, ".", "height_above_ground"),
                    paste0("^", varname, ".", "lat"),
                    paste0("^", varname, ".", "lon")
                  )

    jra <- readLines(filetxt)
    jra <- str_trim(jra)
    ix <- sapply(pattern, grep, x = jra)
    nl <- length(ix)
    ix <- lapply(seq(nl), function(j){
        if(j < nl)
            (ix[j] + 1):(ix[j + 1] - 1)
        else
            ix[j] + 1
    })

    don <- lapply(ix, function(j) jra[j])
    don <- lapply(don, function(x) x[x != ""])

    lon <- as.numeric(str_trim(strsplit(don[[5]], ",")[[1]]))
    lon <- ((lon + 180) %% 360) - 180
    lat <- as.numeric(str_trim(strsplit(don[[4]], ",")[[1]]))
    ox <- order(lon)
    oy <- order(lat)
    lon <- lon[ox]
    lat <- lat[oy]

    oyear <- sub(varname, "", basename(filetxt))
    oyear <- strsplit(oyear, "_|-")[[1]]
    oyear <- oyear[oyear != ""]
    oyear <- as.numeric(oyear[1]) - 1
    oyear <- paste0(oyear, "-12-31T18:00:00Z")
    origin <- as.POSIXct(oyear, tz = "GMT", format = "%Y-%m-%dT%H:%M:%SZ")

    times <- as.numeric(str_trim(strsplit(don[[2]], ",")[[1]]))
    times <- as.POSIXct(times * 3600, origin = origin, tz = "GMT")

    dat <- lapply(don[[1]], jra55.parse.dods.mat)
    id.crd <- do.call(rbind, lapply(dat, "[[", 1))
    dat <- do.call(rbind, lapply(dat, "[[", 2))

    tstep <- factor(id.crd[, 1], levels = unique(id.crd[, 1]))
    ix <- split(seq_along(tstep), tstep)

    dat <- lapply(ix, function(j){
        x <- t(dat[j, , drop = FALSE])[ox, oy]
        x[is.nan(x)] <- NA
        x - 273.15
    })

    list(time = times, lon = lon, lat = lat, data = dat)
}

jra55.regrid.data <- function(dat){
    lon0 <- dat$lon
    lat0 <- dat$lat
    rlon <- range(lon0)
    rlat <- range(lat0)
    dat$lon <- seq(rlon[1], rlon[2], length.out = length(lon0))
    dat$lat <- seq(rlat[1], rlat[2], length.out = length(lat0))
    don <- lapply(dat$data, function(x){
        obj0 <- list(lon = lon0, lat = lat0, z = x)
        obj1 <- cdt.interp.surface.grid(obj0, dat[c('lon', 'lat')])
        obj1$z
    })
    dat$data <- don
    dat
}

jra55.write.ncdf <- function(dat, varid, longname, outdir){
    hours <- format(dat$time, "%Y%m%d%H")
    ncfiles <- file.path(outdir, paste0(varid, "_", hours, ".nc"))

    dx <- ncdf4::ncdim_def("Lon", "degreeE", dat$lon, longname = "Longitude")
    dy <- ncdf4::ncdim_def("Lat", "degreeN", dat$lat, longname = "Latitude")
    missval <- -99
    ncgrd <- ncdf4::ncvar_def(varid, "degC", list(dx, dy), missval,
                              longname, "float", compression = 6)
    for(j in seq_along(ncfiles)){
        don <- dat$data[[j]]
        don[is.na(don)] <- missval
        nc <- ncdf4::nc_create(ncfiles[j], ncgrd)
        ncdf4::ncvar_put(nc, ncgrd, don)
        ncdf4::nc_close(nc)
    }
    invisible()
}
