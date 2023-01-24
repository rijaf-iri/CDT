
## toexport
trmm3b42v7.download.dods <- function(GalParams, nbfile = 3, GUI = TRUE, verbose = TRUE){
    on.exit(curl::handle_reset(handle))

    rdate <- table.format.date.time(GalParams$tstep, GalParams$date.range, GalParams$minhour)

    if(GalParams$tstep == "daily"){
        ncfiles <- sprintf("trmm3b42_%s%s%s.nc", rdate[, 1], rdate[, 2], rdate[, 3])
        nc4files <- sprintf("3B42_Daily.%s%s%s.7.nc4.nc4", rdate[, 1], rdate[, 2], rdate[, 3])
        paths <- file.path("TRMM_L3", "TRMM_3B42_Daily.7", rdate[, 1], rdate[, 2])
        longname <- "Daily accumulated precipitation (combined microwave-IR) estimate with gauge calibration over land"
        region <- "?precipitation%s%s,lon%s,lat%s"
        data.tres <- GalParams$tstep
        pars <- list(fac = 1, transpose = TRUE)
    }
    else if(GalParams$tstep == "hourly"){
        if(GalParams$minhour == 3){
            ncfiles <- sprintf("trmm3b42_%s%s%s%s.nc", rdate[, 1], rdate[, 2], rdate[, 3], rdate[, 4])
            nc4files <- sprintf("3B42.%s%s%s.%s.7.HDF.nc4", rdate[, 1], rdate[, 2], rdate[, 3], rdate[, 4])
            daty <- apply(rdate[, 1:4], 1, paste0, collapse = "")
            daty <- as.POSIXct(daty, tz = "UTC", format = "%Y%m%d%H") - 1
            paths <- file.path("TRMM_L3", "TRMM_3B42.7", format(daty, "%Y"),
                               strftime(daty, format = "%j", tz = "UTC"))
            longname <- "TRMM 3-Hour 0.25 TRMM and Other-GPI Calibration Rainfall Data"
            region <- "?precipitation%s%s,nlon%s,nlat%s"
            data.tres <- paste0(GalParams$minhour, GalParams$tstep)
            pars <- list(fac = 3, transpose = TRUE)
        }else return(-1)
    }else return(-1)

    #############
    lon <- seq(-179.875, 179.875, 0.25)
    lat <- seq(-49.875, 49.875, 0.25)

    ix <- lon >= GalParams$bbox$minlon & lon <= GalParams$bbox$maxlon
    iy <- lat >= GalParams$bbox$minlat & lat <= GalParams$bbox$maxlat

    if(!any(ix) | !any(iy)) return(-2)

    ilon <- range(which(ix)) + c(-2, 0)
    if(ilon[1] < 0) ilon[1] <- 0
    ilat <- range(which(iy)) + c(-2, 0)
    if(ilat[1] < 0) ilat[1] <- 0

    sublon <- paste0("[", ilon[1], ":", ilon[2], "]")
    sublat <- paste0("[", ilat[1], ":", ilat[2], "]")
    request <- sprintf(region, sublon, sublat, sublon, sublat)

    #############
    opendap <- "https://disc2.gesdisc.eosdis.nasa.gov/opendap"
    urls <- paste0(file.path(opendap, paths, nc4files), request)

    #############
    dx <- ncdf4::ncdim_def("Lon", "degreeE", lon[(ilon[1]:ilon[2]) + 1], longname = "Longitude")
    dy <- ncdf4::ncdim_def("Lat", "degreeN", lat[(ilat[1]:ilat[2]) + 1], longname = "Latitude")
    ncgrd <- ncdf4::ncvar_def("precip", "mm", list(dx, dy), -99, longname, "float", compression = 6)

    handle <- curl::new_handle()
    curl::handle_setopt(handle, username = GalParams$login$usr, password = GalParams$login$pwd)
    #############

    data.name <- paste0("TRMM_3B42_v7_", data.tres)
    outdir <- file.path(GalParams$dir2save, data.name)
    dir.create(outdir, showWarnings = FALSE, recursive = TRUE)
    destfiles <- file.path(outdir, nc4files)
    ncfiles <- file.path(outdir, ncfiles)

    ret <- cdt.download.data(urls, destfiles, ncfiles, nbfile, GUI,
                             verbose, data.name, trmm3b42v7.download.data,
                             handle = handle, ncgrd = ncgrd, pars = pars)

    return(ret)
}

## toexport
trmm3b42v7rt.download.dods <- function(GalParams, nbfile = 3, GUI = TRUE, verbose = TRUE){
    on.exit(curl::handle_reset(handle))

    rdate <- table.format.date.time(GalParams$tstep, GalParams$date.range, GalParams$minhour)

    if(GalParams$tstep == "daily"){
        ncfiles <- sprintf("trmm3b42rt_%s%s%s.nc", rdate[, 1], rdate[, 2], rdate[, 3])
        nc4files <- sprintf("3B42RT_Daily.%s%s%s.7.nc4.nc4", rdate[, 1], rdate[, 2], rdate[, 3])
        paths <- file.path("TRMM_RT", "TRMM_3B42RT_Daily.7", rdate[, 1], rdate[, 2])
        longname <- "Daily accumulated precipitation (combined microwave-IR) estimate"
        region <- "?precipitation%s%s,lon%s,lat%s"
        data.tres <- GalParams$tstep
        pars <- list(fac = 1, transpose = TRUE)
    }
    else if(GalParams$tstep == "hourly"){
        if(GalParams$minhour == 3){
            ncfiles <- sprintf("trmm3b42rt_%s%s%s%s.nc", rdate[, 1], rdate[, 2], rdate[, 3], rdate[, 4])
            daty <- apply(rdate[, 1:4], 1, paste0, collapse = "")
            daty <- as.POSIXct(daty, tz = "UTC", format = "%Y%m%d%H") - 1
            sep7Rdate <- as.POSIXct("2012110703", tz = "UTC", format = "%Y%m%d%H")
            sep7R <- ifelse(daty <= sep7Rdate, "7R2", "7")
            nc4format <- paste0("3B42RT.%s%s%s%s.", sep7R, ".nc4.nc4")
            nc4files <- sprintf(nc4format, rdate[, 1], rdate[, 2], rdate[, 3], rdate[, 4])
            doy <- strftime(daty, format = "%j", tz = "UTC")
            paths <- file.path("TRMM_RT", "TRMM_3B42RT.7", format(daty, "%Y"), doy)
            longname <- "TMPA TRMM_3B42RT Near Real Time Precipitation"
            region <- "?precipitation%s%s,lat%s,lon%s"
            data.tres <- paste0(GalParams$minhour, GalParams$tstep)
            pars <- list(fac = 3, transpose = FALSE)
        }else return(-1)
    }else return(-1)

    #############
    lon <- seq(-179.875, 179.875, 0.25)
    lat <- seq(-59.875, 59.875, 0.25)

    ix <- lon >= GalParams$bbox$minlon & lon <= GalParams$bbox$maxlon
    iy <- lat >= GalParams$bbox$minlat & lat <= GalParams$bbox$maxlat

    if(!any(ix) | !any(iy)) return(-2)

    ilon <- range(which(ix)) + c(-2, 0)
    if(ilon[1] < 0) ilon[1] <- 0
    ilat <- range(which(iy)) + c(-2, 0)
    if(ilat[1] < 0) ilat[1] <- 0

    sublon <- paste0("[", ilon[1], ":", ilon[2], "]")
    sublat <- paste0("[", ilat[1], ":", ilat[2], "]")

    if(GalParams$tstep == "daily"){
        request <- sprintf(region, sublon, sublat, sublon, sublat)
    }else if(GalParams$tstep == "hourly"){
        if(GalParams$minhour == 3)
            request <- sprintf(region, sublat, sublon, sublat, sublon)
    }

    #############
    opendap <- "https://disc2.gesdisc.eosdis.nasa.gov/opendap"
    urls <- paste0(file.path(opendap, paths, nc4files), request)

    #############
    dx <- ncdf4::ncdim_def("lon", "degreeE", lon[(ilon[1]:ilon[2]) + 1], longname = "Longitude")
    dy <- ncdf4::ncdim_def("lat", "degreeN", lat[(ilat[1]:ilat[2]) + 1], longname = "Latitude")
    ncgrd <- ncdf4::ncvar_def("precip", "mm", list(dx, dy), -99, longname, "float", compression = 6)

    handle <- curl::new_handle()
    curl::handle_setopt(handle, username = GalParams$login$usr, password = GalParams$login$pwd)
    #############

    data.name <- paste0("TRMM_3B42RT_v7_", data.tres)
    outdir <- file.path(GalParams$dir2save, data.name)
    dir.create(outdir, showWarnings = FALSE, recursive = TRUE)
    destfiles <- file.path(outdir, nc4files)
    ncfiles <- file.path(outdir, ncfiles)

    ret <- cdt.download.data(urls, destfiles, ncfiles, nbfile, GUI,
                             verbose, data.name, trmm3b42v7.download.data,
                             handle = handle, ncgrd = ncgrd, pars = pars)

    return(ret)
}

#################################################################################

trmm3b42v7.download.data <- function(lnk, dest, ncfl, handle, ncgrd, pars){
    on.exit(unlink(dest))
    xx <- basename(dest)

    dc <- try(curl::curl_download(lnk, dest, handle = handle), silent = TRUE)
    if(!inherits(dc, "try-error")){
        ret <- trmm3b42v7.format.data(dest, ncfl, ncgrd, pars)
        if(ret == 0) xx <- NULL
    }
    return(xx)
}

trmm3b42v7.format.data <- function(dest, ncfl, ncgrd, pars){
    nc <- try(ncdf4::nc_open(dest), silent = TRUE)
    ret <- 1
    if(!inherits(nc, "try-error")){
        z <- ncdf4::ncvar_get(nc, "precipitation")
        ncdf4::nc_close(nc)

        if(pars$transpose) z <- t(z)
        z <- z * pars$fac
        z[is.na(z)] <- ncgrd$missval

        nc <- ncdf4::nc_create(ncfl, ncgrd)
        ncdf4::ncvar_put(nc, ncgrd, z)
        ncdf4::nc_close(nc)

        ret <- 0
    }

    return(ret)
}
