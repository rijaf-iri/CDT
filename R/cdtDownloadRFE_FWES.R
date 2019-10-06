
fews.download.data.tif <- function(lnk, dest, ncfl, bbox, arc){
    xx <- basename(dest)

    link.exist <- try(readLines(lnk, 1), silent = TRUE)
    if(inherits(link.exist, "try-error")) return(xx)

    dc <- try(curl::curl_download(lnk, dest), silent = TRUE)
    if(!inherits(dc, "try-error")){
        tmpdir <- dirname(ncfl)
        tmpf <- file.path(tmpdir, gsub("\\.zip$", "", basename(dest)))
        utils::unzip(dest, exdir = tmpdir)

        ret <- fews.extract.data.tif(tmpf, ncfl, bbox, arc)

        unlink(tmpf)
        ##
        if(ret == 0){
            xx <- NULL
        }else{
            unlink(dest)
        }
    }
    return(xx)
}

fews.extract.data.tif <- function(tmpf, ncfl, bbox, arc){
    xr <- try(raster::raster(tmpf), silent = TRUE)
    ret <- 1
    if(!inherits(xr, "try-error")){
        ex <- raster::extent(bbox$minlon, bbox$maxlon,
                             bbox$minlat, bbox$maxlat)
        rc <- raster::crop(xr, ex)
        xy <- sp::coordinates(rc)

        x <- sort(unique(xy[, 1]))
        y <- sort(unique(xy[, 2]))
        z <- raster::as.matrix(rc)
        z <- t(z)[, rev(seq_along(y))]
        z[z < 0] <- NA

        dx <- ncdf4::ncdim_def("lon", "degreeE", x, longname = "Longitude")
        dy <- ncdf4::ncdim_def("lat", "degreeN", y, longname = "Latitude")
        missval <- -99
        if(arc){
            name <- "arc2"
            longname <- "Africa Rainfall Climatology version 2"
        }else{
            name <- "rfev2"
            longname <- "Estimated Precipitation RFEv2"
        }

        ncgrd <- ncdf4::ncvar_def(name, "mm", list(dx, dy), missval,
                                  longname, "float", compression = 9)
        z[is.na(z)] <- missval

        nc <- ncdf4::nc_create(ncfl, ncgrd)
        ncdf4::ncvar_put(nc, ncgrd, z)
        ncdf4::nc_close(nc)

        ret <- 0
    }
    return(ret)
}

fews.download.data.bin <- function(lnk, dest, ncfl, bbox, region){
    xx <- basename(dest)

    link.exist <- try(readLines(lnk, 1), silent = TRUE)
    if(inherits(link.exist, "try-error")) return(xx)

    dc <- try(curl::curl_download(lnk, dest), silent = TRUE)
    if(!inherits(dc, "try-error")){
        tmpdir <- dirname(ncfl)
        tmpf <- file.path(tmpdir, gsub("\\.gz$", "", basename(dest)))
        R.utils::gunzip(dest, tmpf, remove = FALSE, overwrite = TRUE)

        ret <- fews.extract.data.bin(tmpf, ncfl, bbox, region)

        unlink(tmpf)
        ##
        if(ret == 0){
            xx <- NULL
        }else{
            unlink(dest)
        }
    }
    return(xx)
}

fews.extract.data.bin <- function(tmpf, ncfl, bbox, region){
    on.exit(close(con))

    pars <- switch(region,
                "africa" = list(nx = 751, ny = 801,
                                x = seq(-20, 55, 0.1),
                                y = seq(-40, 40, 0.1)
                            ),
                "southasia" = list(nx = 401, ny = 301,
                                    x = seq(70, 110, 0.1),
                                    y = seq(5, 35, 0.1)
                                )
            )

    con <- file(tmpf, open = "rb")
    val <- try(readBin(con, numeric(), pars$nx * pars$ny, 4, endian = "big"), silent = TRUE)
    ret <- 1
    if(!inherits(val, "try-error")){
        lon <- pars$x
        lat <- pars$y

        ix <- lon >= bbox$minlon & lon <= bbox$maxlon
        iy <- lat >= bbox$minlat & lat <= bbox$maxlat
        lon <- lon[ix]
        lat <- lat[iy]

        # val[val == -999] <- NA
        val[val < 0] <- NA
        val <- matrix(val, pars$nx, pars$ny)
        val <- val[ix, iy]

        dx <- ncdf4::ncdim_def("lon", "degreeE", lon, longname = "Longitude")
        dy <- ncdf4::ncdim_def("lat", "degreeN", lat, longname = "Latitude")
        missval <- -999
        longname <- "Estimated Precipitation RFEv2"
        ncgrd <- ncdf4::ncvar_def("rfev2", "mm", list(dx, dy), missval,
                                  longname, "float", compression = 9)
        val[is.na(val)] <- missval

        nc <- ncdf4::nc_create(ncfl, ncgrd)
        ncdf4::ncvar_put(nc, ncgrd, val)
        ncdf4::nc_close(nc)

        ret <- 0
    }

    return(ret)
}
