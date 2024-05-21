
trmm3b42v7.download.gesdisc <- function(GalParams, nbfile = 1, GUI = TRUE, verbose = TRUE){
    info <- trmm3b42v7.info.gesdisc(GalParams)
    if(is.null(info)) return(-3)

    ######
    data.name <- paste0("TRMM_", info$dataname, "_v7_", info$data.tres)
    outdir <- file.path(GalParams$dir2save, data.name)
    if(!dir.exists(outdir))
        dir.create(outdir, showWarnings = FALSE, recursive = TRUE)
    destfiles <- file.path(outdir, paste0('tmp_', info$ncfiles))
    ncfiles <- file.path(outdir, info$ncfiles)

    ######
    lon <- seq(-179.875, 179.875, 0.25)
    if(GalParams$rfe.src == "trmm3b42v7-gb"){
        lat <- seq(-49.875, 49.875, 0.25)
    }else{
        lat <- seq(-59.875, 59.875, 0.25)
    }

    ix <- lon >= GalParams$bbox$minlon & lon <= GalParams$bbox$maxlon
    iy <- lat >= GalParams$bbox$minlat & lat <= GalParams$bbox$maxlat

    if(!any(ix) | !any(iy)) return(-2)

    ilon <- range(which(ix)) + c(-2, 0)
    if(ilon[1] < 0) ilon[1] <- 0
    ilat <- range(which(iy)) + c(-2, 0)
    if(ilat[1] < 0) ilat[1] <- 0

    sublon <- paste0("[", ilon[1], ":1:", ilon[2], "]")
    sublat <- paste0("[", ilat[1], ":1:", ilat[2], "]")

    if(info$pars$transpose){
        request <- sprintf(info$region, sublon, sublat, sublon, sublat)
    }else{
        request <- sprintf(info$region, sublat, sublon, sublon, sublat)
    }
    # request <- utils::URLencode(request, reserved = TRUE)
    urls <- paste0(info$urls, "?", request)

    ########
    auth <- earthdata.curlopts(outdir, GalParams$login)
    handle <- curl::new_handle()
    curl::handle_setopt(handle,
                        netrc_file = auth$netrc,
                        cookiefile = auth$cookie,
                        cookiejar = auth$cookie)
    on.exit({
        curl::handle_reset(handle)
        unlink(auth$cookie)
        unlink(auth$netrc)
    })

    ret <- cdt.download.data(urls, destfiles, ncfiles, nbfile, GUI, verbose,
                             data.name, trmm3b42v7.download.data,
                             pars = info$pars, handle = handle)
    return(ret)
}

trmm3b42v7.info.gesdisc <- function(GalParams){
    rdate <- table.format.date.time(GalParams$tstep, GalParams$date.range, GalParams$minhour)
    if(is.null(rdate)) return(NULL)

    if(GalParams$tstep == "hourly"){
        if(GalParams$minhour == 3){
            year_dir <- rdate[, 1]
            doy_dir <- rdate[, 5]
            zh <- rdate[, 4] == '00'
            if(any(zh)){
                zerod <- paste0(year_dir[zh], doy_dir[zh])
                zerod <- as.Date(zerod, format = '%Y%j')
                zerod <- zerod - 1
                year_dir[zh] <- format(zerod, '%Y')
                doy_dir[zh] <- format(zerod, '%j')
            }

            if(GalParams$rfe.src == "trmm3b42v7-gb"){
                dataname <- "3B42"
                level <- "TRMM_L3"
                dataset <- "TRMM_3B42.7"
                nc4format <- "3B42.%s.%s.7.HDF.nc4"
                ncformat <- "trmm3b42_%s%s%s%s.nc"
                region <- "precipitation%s%s,nlon%s,nlat%s"
                pars <- list(transpose = TRUE, factor = 3,
                             varid = 'precipitation', lon = 'nlon', lat = 'nlat',
                             longname = "Precipitation estimate")
            }else if(GalParams$rfe.src == "trmm3b42rtv7-gb"){
                dataname <- "3B42RT"
                level <- "TRMM_RT"
                dataset <- "TRMM_3B42RT.7"
                nc4format <- "3B42RT.%s%s.%s.nc4.nc4"
                ncformat <- "trmm3b42rt_%s%s%s%s.nc"
                region <- "precipitation%s%s,lon%s,lat%s"
                pars <- list(transpose = FALSE, factor = 3,
                             varid = 'precipitation', lon = 'lon', lat = 'lat',
                             longname = "Near real time precipitation estimate")
            }else return(NULL)

            dd <- paste0(rdate[, 1], rdate[, 2], rdate[, 3])
            if(GalParams$rfe.src == "trmm3b42rtv7-gb"){
                breaks <- as.POSIXct("2012110703", format = '%Y%m%d%H', tz = 'UTC')
                times <- apply(rdate[, 1:4], 1, paste, collapse = "")
                times <- as.POSIXct(times, format = '%Y%m%d%H', tz = 'UTC')
                minorv <- ifelse(times <= breaks, '7R2', '7')
                nc4files <- sprintf(nc4format, dd, rdate[, 4], minorv)
            }else{
                nc4files <- sprintf(nc4format, dd, rdate[, 4])
            }

            paths <- file.path(level, dataset, year_dir, doy_dir)
            ncfiles <- sprintf(ncformat, rdate[, 1], rdate[, 2], rdate[, 3], rdate[, 4])
            data.tres <- paste0(GalParams$minhour, GalParams$tstep)
        }else return(NULL)
    }else if(GalParams$tstep == "daily"){
        if(GalParams$rfe.src == "trmm3b42v7-gb"){
            dataname <- "3B42"
            level <- "TRMM_L3"
            dataset <- "TRMM_3B42_Daily.7"
            nc4format <- "3B42_Daily.%s.7.nc4.nc4"
            ncformat <- "trmm3b42_%s%s%s.nc"
            region <- "precipitation%s%s,lon%s,lat%s"
            pars <- list(transpose = TRUE, factor = 1,
                         varid = 'precipitation', lon = 'lon', lat = 'lat',
                         longname = "Precipitation (combined microwave-IR) estimate with gauge calibration over land")
        }else if(GalParams$rfe.src == "trmm3b42rtv7-gb"){
            dataname <- "3B42RT"
            level <- "TRMM_RT"
            dataset <- "TRMM_3B42RT_Daily.7"
            nc4format <- "3B42RT_Daily.%s.7.nc4.nc4"
            ncformat <- "trmm3b42rt_%s%s%s.nc"
            region <- "precipitation%s%s,lon%s,lat%s"
            pars <- list(transpose = TRUE, factor = 1,
                         varid = 'precipitation', lon = 'lon', lat = 'lat',
                         longname = "Precipitation (combined microwave-IR) estimate")
        }else return(NULL)

        dd <- paste0(rdate[, 1], rdate[, 2], rdate[, 3])
        nc4files <- sprintf(nc4format, dd)
        paths <- file.path(level, dataset, rdate[, 1], rdate[, 2])
        ncfiles <- sprintf(ncformat, rdate[, 1], rdate[, 2], rdate[, 3])
        data.tres <- GalParams$tstep
    }else return(NULL)

    opendap <- "https://disc2.gesdisc.eosdis.nasa.gov/opendap"
    urls <- file.path(opendap, paths, nc4files)

    list(urls = urls, ncfiles = ncfiles, dataname = dataname,
         region = region, data.tres = data.tres, pars = pars)
}

trmm3b42v7.coverage.gesdisc <- function(GalParams){
    out <- list(name = NULL, timestep = NULL)
    if(GalParams$tstep == 'hourly'){
        out$timestep <- paste(GalParams$minhour, GalParams$tstep)
        if(GalParams$rfe.src == "trmm3b42v7-gb"){
            out$name <- "TRMM 3B42 Version 7"
            out$start <- "1998010100"
            out$end <- "2019123121"
        }else{
            out$name <- "TRMM 3B42RT Version 7"
            out$start <- "2000030100"
            out$end <- "2020010100"
        }
    }else{
        out$timestep <- GalParams$tstep
        if(GalParams$rfe.src == "trmm3b42v7-gb"){
            out$name <- "TRMM 3B42 Version 7"
            out$start <- "19980101"
            out$end <- "20191230"
        }else{
            out$name <- "TRMM 3B42RT Version 7"
            out$start <- "20000301"
            out$end <- "20191231"
        }
    }

    return(out)
}

#################################################################################

trmm3b42v7.download.data <- function(lnk, dest, ncfl, pars, handle, GUI = TRUE){
    on.exit(unlink(dest))
    xx <- basename(dest)

    dc <- try(curl::curl_download(lnk, dest, handle = handle), silent = TRUE)
    if(!inherits(dc, "try-error")){
        ret <- trmm3b42v7.format.data(dest, ncfl, pars)
        ret = 0
        if(ret == 0) xx <- NULL
    }else{
        msg <- gsub('[\r\n]', '', dc[1])
        Insert.Messages.Out(msg, TRUE, "e", GUI)
    }

    return(xx)
}

trmm3b42v7.format.data <- function(dest, ncfl, pars){
    nc <- try(ncdf4::nc_open(dest), silent = TRUE)
    ret <- 1
    if(!inherits(nc, "try-error")){
        lon <- nc$dim[[pars$lon]]$vals
        lat <- nc$dim[[pars$lat]]$vals
        prcp <- ncdf4::ncvar_get(nc, varid = pars$varid)
        ncdf4::nc_close(nc)
        if(pars$transpose) prcp <- t(prcp)
        prcp <- prcp * pars$factor
        prcp <- round(prcp, 2)
        prcp[is.na(prcp)] <- -99

        dx <- ncdf4::ncdim_def("lon", "degrees_east", lon, longname = "Longitude")
        dy <- ncdf4::ncdim_def("lat", "degrees_north", lat, longname = "Latitude")
        ncgrd <- ncdf4::ncvar_def("precip", "mm", list(dx, dy), -99,
                                  pars$longname, "float", compression = 9)

        nc <- ncdf4::nc_create(ncfl, ncgrd)
        ncdf4::ncvar_put(nc, ncgrd, prcp)
        ncdf4::nc_close(nc)

        ret <- 0
    }

    return(ret)
}
