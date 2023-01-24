
## toexport
chirp.download.iridl <- function(GalParams, nbfile = 3, GUI = TRUE, verbose = TRUE){
    dlpath <- "https://iridl.ldeo.columbia.edu/SOURCES/.UCSB/.CHIRP/.v1p0"
    vartime <- switch(GalParams$tstep,
                      "daily" = ".daily/.prcp",
                      "dekadal" = ".dekad/.prcp",
                      "monthly" = ".dekad/.prcp"
                     )

    rlon <- unlist(GalParams$bbox[c('minlon', 'maxlon')])
    rlon <- paste(c('X', rlon, 'RANGE'), collapse = "/")
    rlat <- unlist(GalParams$bbox[c('minlat', 'maxlat')])
    rlat <- paste(c('Y', rlat, 'RANGE'), collapse = "/")

    aggr <- if(GalParams$tstep == "monthly") "monthlyAverage/3./mul" else NULL
 
    rdate <- iridl.format.date(GalParams$tstep, GalParams$date.range)
    urls <- urltools::url_encode(paste0("(", rdate$dates, ")"))
    urls <- paste0("T", "/", urls, "/", "VALUE")

    urls <- paste(dlpath, vartime, rlon, rlat, aggr, urls, 'data.nc', sep = "/")

    #########
    data.name <- paste0("CHIRP_", GalParams$tstep)
    outdir <- file.path(GalParams$dir2save, data.name)
    dir.create(outdir, showWarnings = FALSE, recursive = TRUE)
    destfiles <- file.path(outdir, paste0("chirp_", rdate$out, ".nc"))

    ret <- cdt.download.data(urls, destfiles, destfiles, nbfile, GUI,
                             verbose, data.name, iridl.download.data)

    return(ret)
}

## toexport
chirps.download.iridl <- function(GalParams, nbfile = 3, GUI = TRUE, verbose = TRUE){
    dlpath <- "https://iridl.ldeo.columbia.edu/SOURCES/.UCSB/.CHIRPS/.v2p0"
    vardir <- switch(GalParams$tstep,
                      "daily" = ".daily-improved/.global/.0p05/.prcp",
                      "dekadal" = ".dekad/.prcp",
                      "monthly" = ".monthly/.global/.precipitation"
                    )

    rlon <- unlist(GalParams$bbox[c('minlon', 'maxlon')])
    rlon <- paste(c('X', rlon, 'RANGE'), collapse = "/")
    rlat <- unlist(GalParams$bbox[c('minlat', 'maxlat')])
    rlat <- paste(c('Y', rlat, 'RANGE'), collapse = "/")
 
    rdate <- iridl.format.date(GalParams$tstep, GalParams$date.range)
    urls <- urltools::url_encode(paste0("(", rdate$dates, ")"))
    urls <- paste0("T", "/", urls, "/", "VALUE")

    urls <- paste(dlpath, vardir, rlon, rlat, urls, 'data.nc', sep = "/")

    #########
    data.name <- paste0("CHIRPSv2_", GalParams$tstep)
    outdir <- file.path(GalParams$dir2save, data.name)
    dir.create(outdir, showWarnings = FALSE, recursive = TRUE)
    destfiles <- file.path(outdir, paste0("chirps_", rdate$out, ".nc"))

    ret <- cdt.download.data(urls, destfiles, destfiles, nbfile, GUI,
                             verbose, data.name, iridl.download.data)

    return(ret)
}

## toexport
chirp.download.ucsb <- function(GalParams, nbfile = 3, GUI = TRUE, verbose = TRUE){
    bbx <- GalParams$bbox
    rdate <- table.format.date.time(GalParams$tstep, GalParams$date.range)
    ftp.ucsb <- "https://data.chc.ucsb.edu/products/CHIRP"

    if(GalParams$tstep == "daily"){
        data.dir <- "global"
        baseurl <- "daily"
        frmt <- ifelse(as.numeric(rdate[, 1]) < 1990, "chirp.%s.%s.%s.tif.gz", "chirp.%s.%s.%s.tif")
        filename <- sprintf(frmt, rdate[, 1], rdate[, 2], rdate[, 3])
        urls <- file.path(ftp.ucsb, baseurl, rdate[, 1], filename)
        ncfiles <- sprintf("chirp_%s%s%s.nc", rdate[, 1], rdate[, 2], rdate[, 3])
        type <- "tif"
    }
    else if(GalParams$tstep == "pentad"){
        if((bbx$minlon >= -19.975 && bbx$maxlon <= 54.975) &&
           (bbx$minlat >= -39.975 && bbx$maxlat <= 39.975))
        {
            data.dir <- "africa"
            baseurl <- "pentads/africa"
            filename <- sprintf("chirp.%s.%s.%s.tif", rdate[, 1], rdate[, 2], rdate[, 3])
        }
        else if((bbx$minlon >= -92.975 && bbx$maxlon <= -57.025) &&
                (bbx$minlat >= 6.025 && bbx$maxlat <= 23.475))
        {
            data.dir <- "camer-carib"
            baseurl <- "pentads/camer-carib"
            filename <- sprintf("chirp.%s.%s.%s.tif", rdate[, 1], rdate[, 2], rdate[, 3])
        }
        else if((bbx$minlon >= 65.025 && bbx$maxlon <= 99.975) &&
                (bbx$minlat >= 5.025 && bbx$maxlat <= 39.975))
        {
            data.dir <- "india"
            baseurl <- "pentads/india"
            filename <- sprintf("chirp.%s.%s.%s.tif", rdate[, 1], rdate[, 2], rdate[, 3])
        }else{
            data.dir <- "global"
            baseurl <- "pentads"
            filename <- sprintf("CHIRP.%s.%s.%s.tif", rdate[, 1], rdate[, 2], rdate[, 3])
        }

        urls <- file.path(ftp.ucsb, baseurl, filename)
        ncfiles <- sprintf("chirp_%s%s%s.nc", rdate[, 1], rdate[, 2], rdate[, 3])
        type <- "tif"
    }
    else if(GalParams$tstep == "dekadal"){
        if((bbx$minlon >= -19.975 && bbx$maxlon <= 54.975) &&
           (bbx$minlat >= -39.975 && bbx$maxlat <= 39.975))
        {
            data.dir <- "africa"
            baseurl <- "bils/dekads/africa"
            filename <- sprintf("chirp%s%s.tar.gz", rdate[, 1], rdate[, 4])
            type <- "bil"
        }
        else if((bbx$minlon >= -92.975 && bbx$maxlon <= -57.025) &&
                (bbx$minlat >= 6.025 && bbx$maxlat <= 23.475))
        {
            data.dir <- "camer-carib"
            baseurl <- "dekads/camer-carib"
            filename <- sprintf("chirp.%s.%s.%s.tif", rdate[, 1], rdate[, 2], rdate[, 3])
            type <- "tif"
        }else{
            data.dir <- "global"
            baseurl <- "dekads"
            filename <- sprintf("CHIRP.%s.%s.%s.tif", rdate[, 1], rdate[, 2], rdate[, 3])
            type <- "tif"
        }

        urls <- file.path(ftp.ucsb, baseurl, filename)
        ncfiles <- sprintf("chirp_%s%s%s.nc", rdate[, 1], rdate[, 2], rdate[, 3])
    }
    else if(GalParams$tstep == "monthly"){
        data.dir <- "global"
        baseurl <- "monthly"
        filename <- sprintf("CHIRP.%s.%s.tif", rdate[, 1], rdate[, 2])
        urls <- file.path(ftp.ucsb, baseurl, filename)
        ncfiles <- sprintf("chirp_%s%s.nc", rdate[, 1], rdate[, 2])
        type <- "tif"

        # if((bbx$minlon >= -19.975 && bbx$maxlon <= 54.975) &&
        #    (bbx$minlat >= -39.975 && bbx$maxlat <= 39.975))
        # {
        #     ## starting from 2014-01,  before this date take global data
        #     data.dir <- "africa"
        #     baseurl <- "monthly/africa"
        #     filename <- sprintf("CHIRP.%s.%s.tif", rdate[, 1], rdate[, 2])
        # }
        # else if((bbx$minlon >= -92.975 && bbx$maxlon <= -57.025) &&
        #         (bbx$minlat >= 6.025 && bbx$maxlat <= 23.475))
        # {
        #     ## starting from 2014-01,  before this date take global data
        #     data.dir <- "camer-carib"
        #     baseurl <- "monthly/camer-carib"
        #     filename <- sprintf("CHIRP.%s.%s.tif", rdate[, 1], rdate[, 2])
        # }else{
        #     data.dir <- "global"
        #     baseurl <- "monthly"
        #     filename <- sprintf("CHIRP.%s.%s.tif", rdate[, 1], rdate[, 2])
        # }

        # urls <- file.path(ftp.ucsb, baseurl, filename)
        # ncfiles <- sprintf("chirp_%s%s.nc", rdate[, 1], rdate[, 2])
        # type <- "tif"
    }else return(-1)

    #########
    data.name <- paste0("CHIRP_", GalParams$tstep)
    outdir <- file.path(GalParams$dir2save, data.name)
    extrdir <- file.path(outdir, "Extracted")
    dir.create(extrdir, showWarnings = FALSE, recursive = TRUE)
    origdir <- file.path(outdir, paste0("Data_", data.dir))
    dir.create(origdir, showWarnings = FALSE, recursive = TRUE)
    destfiles <- file.path(origdir, filename)
    ncfiles <- file.path(extrdir, ncfiles)

    ret <- cdt.download.data(urls, destfiles, ncfiles, nbfile, GUI,
                             verbose, data.name, chirps.download.data,
                             bbox = GalParams$bbox, type = type)

    return(ret)
}

## toexport
chirps.download.ucsb <- function(GalParams, nbfile = 3, GUI = TRUE, verbose = TRUE){
    bbx <- GalParams$bbox
    rdate <- table.format.date.time(GalParams$tstep, GalParams$date.range)
    ftp.ucsb <- "https://data.chc.ucsb.edu/products/CHIRPS-2.0"

    if(GalParams$tstep == "daily"){
        if((bbx$minlon >= -19.975 && bbx$maxlon <= 54.975) &&
           (bbx$minlat >= -39.975 && bbx$maxlat <= 39.975))
        {
            data.dir <- "africa"
            baseurl <- "africa_daily/tifs/p05"
        }else{
            data.dir <- "global"
            baseurl <- "global_daily/tifs/p05"
        }

        filename <- sprintf("chirps-v2.0.%s.%s.%s.tif.gz", rdate[, 1], rdate[, 2], rdate[, 3])
        urls <- file.path(ftp.ucsb, baseurl, rdate[, 1], filename)
        ncfiles <- sprintf("chirps_%s%s%s.nc", rdate[, 1], rdate[, 2], rdate[, 3])
        type <- "tif"
    }
    else if(GalParams$tstep == "pentad"){
        if((bbx$minlon >= -19.975 && bbx$maxlon <= 54.975) &&
           (bbx$minlat >= -39.975 && bbx$maxlat <= 39.975))
        {
            data.dir <- "africa"
            baseurl <- "africa_pentad/bils"
        }
        else if((bbx$minlon >= -92.975 && bbx$maxlon <= -57.025) &&
                (bbx$minlat >= 6.025 && bbx$maxlat <= 23.475))
        {
            data.dir <- "camer-carib"
            baseurl <- "camer-carib_pentad/bils"
        }else{
            data.dir <- "global"
            baseurl <- "global_pentad/bils"
        }

        filename <- sprintf("v2p0chirps%s%s.tar.gz", rdate[, 1], rdate[, 4])
        urls <- file.path(ftp.ucsb, baseurl, filename)
        ncfiles <- sprintf("chirps_%s%s%s.nc", rdate[, 1], rdate[, 2], rdate[, 3])
        type <- "bil"
    }
    else if(GalParams$tstep == "dekadal"){
        if((bbx$minlon >= -19.975 && bbx$maxlon <= 54.975) &&
           (bbx$minlat >= -39.975 && bbx$maxlat <= 39.975))
        {
            data.dir <- "africa"
            baseurl <- "africa_dekad/bils"
        }
        else if((bbx$minlon >= -92.975 && bbx$maxlon <= -57.025) &&
                (bbx$minlat >= 6.025 && bbx$maxlat <= 23.475))
        {
            data.dir <- "camer-carib"
            baseurl <- "camer-carib_dekad/bils"
        }else{
            data.dir <- "global"
            baseurl <- "global_dekad/bils"
        }

        filename <- sprintf("v2p0chirps%s%s.tar.gz", rdate[, 1], rdate[, 4])
        urls <- file.path(ftp.ucsb, baseurl, filename)
        ncfiles <- sprintf("chirps_%s%s%s.nc", rdate[, 1], rdate[, 2], rdate[, 3])
        type <- "bil"

        # exception
        if(data.dir == "global"){
            ix <- basename(urls) == "v2p0chirps201825.tar.gz"
            urls[ix] <- file.path(ftp.ucsb, baseurl, "v2p0chirps201825.tar")
        }
    }
    else if(GalParams$tstep == "monthly"){
        if((bbx$minlon >= -19.975 && bbx$maxlon <= 54.975) &&
           (bbx$minlat >= -39.975 && bbx$maxlat <= 39.975))
        {
            data.dir <- "africa"
            baseurl <- "africa_monthly/bils"
        }
        else if((bbx$minlon >= -92.975 && bbx$maxlon <= -57.025) &&
                (bbx$minlat >= 6.025 && bbx$maxlat <= 23.475))
        {
            data.dir <- "camer-carib"
            baseurl <- "camer-carib_monthly/bils"
        }else{
            data.dir <- "global"
            baseurl <- "global_monthly/bils"
        }

        filename <- sprintf("v2p0chirps%s%s.tar.gz", rdate[, 1], rdate[, 2])
        urls <- file.path(ftp.ucsb, baseurl, filename)
        ncfiles <- sprintf("chirps_%s%s.nc", rdate[, 1], rdate[, 2])
        type <- "bil"
    }else return(-1)

    #########
    data.name <- paste0("CHIRPSv2_", GalParams$tstep)
    outdir <- file.path(GalParams$dir2save, data.name)
    extrdir <- file.path(outdir, "Extracted")
    dir.create(extrdir, showWarnings = FALSE, recursive = TRUE)
    origdir <- file.path(outdir, paste0("Data_", data.dir))
    dir.create(origdir, showWarnings = FALSE, recursive = TRUE)
    destfiles <- file.path(origdir, filename)
    ncfiles <- file.path(extrdir, ncfiles)

    ret <- cdt.download.data(urls, destfiles, ncfiles, nbfile, GUI,
                             verbose, data.name, chirps.download.data,
                             bbox = GalParams$bbox, type = type)

    return(ret)
}

## toexport
chirps.6hrAF.download.ucsb <- function(GalParams, nbfile = 3, GUI = TRUE, verbose = TRUE){
    rdate <- table.format.date.time(GalParams$tstep, GalParams$date.range, GalParams$minhour)
    ftp.ucsb <- "https://data.chc.ucsb.edu/products/CHIRPS-2.0/africa_6-hourly"
    data.dir <- "africa"
    daty <- as.Date(paste(rdate[, 1], rdate[, 2], rdate[, 3], sep = "-"))
    baseurl <- ifelse(daty <= as.Date("2015-07-31"), "p1_bin", "p1_bin/extra_step")
    daty <- format(daty, "%Y%m")
    filename <- sprintf("rfe_gdas.bin.%s%s%s%s.gz", rdate[, 1], rdate[, 2], rdate[, 3], rdate[, 4])
    urls <- file.path(ftp.ucsb, baseurl, daty, filename)
    ncfiles <- sprintf("chirps_%s%s%s%s.nc", rdate[, 1], rdate[, 2], rdate[, 3], rdate[, 4])

    #########
    data.name <- paste0("CHIRPSv2_", GalParams$minhour, GalParams$tstep)
    outdir <- file.path(GalParams$dir2save, data.name)
    extrdir <- file.path(outdir, "Extracted")
    dir.create(extrdir, showWarnings = FALSE, recursive = TRUE)
    origdir <- file.path(outdir, paste0("Data_", data.dir))
    dir.create(origdir, showWarnings = FALSE, recursive = TRUE)
    destfiles <- file.path(origdir, filename)
    ncfiles <- file.path(extrdir, ncfiles)

    ret <- cdt.download.data(urls, destfiles, ncfiles, nbfile, GUI,
                             verbose, data.name, chirps.6hrAF.download.data,
                             bbox = GalParams$bbox)

    return(ret)
}

#################################################################################

chirps.download.data <- function(lnk, dest, ncfl, bbox, type){
    xx <- basename(dest)

    link.exist <- try(readLines(lnk, 1), silent = TRUE)
    if(inherits(link.exist, "try-error")) return(xx)

    dc <- try(curl::curl_download(lnk, dest), silent = TRUE)
    if(!inherits(dc, "try-error")){
        tmpdir <- dirname(ncfl)

        if(grepl("\\.tar\\.gz$", dest, ignore.case = TRUE))
        {
            utils::untar(dest, exdir = tmpdir)
            # utils::untar(dest, exdir = tmpdir, compressed = TRUE,
            #              tar = Sys.getenv("R_INSTALL_TAR", "internal"))

            ff <- gsub("\\.tar\\.gz$", "", basename(dest))
            if(type == "bil"){
                tmpf <- file.path(tmpdir, paste0(ff, ".bil"))
                tmphdr <- file.path(tmpdir, paste0(ff, ".hdr"))
            }
            if(type == "tif")
                tmpf <- file.path(tmpdir, paste0(ff, ".tif"))

            delete <- TRUE
        }
        else if(grepl("\\.gz$", dest, ignore.case = TRUE) &
           !grepl("\\.tar\\.", dest, ignore.case = TRUE))
        {
            tmpf <- file.path(tmpdir, gsub("\\.gz$", "", basename(dest)))
            R.utils::gunzip(dest, tmpf, remove = FALSE, overwrite = TRUE)
            delete <- TRUE
        }
        else{
            tmpf <- dest
            delete <- FALSE
        }

        ##
        ret <- chirps.extract.data(tmpf, ncfl, bbox)
        if(delete){
            unlink(tmpf)
            if(type == "bil") unlink(tmphdr)
        }

        ##
        if(ret == 0){
            xx <- NULL
        }else{
            unlink(dest)
        }
    }
    return(xx)
}

chirps.extract.data <- function(dest, ncfl, bbox){
    xr <- try(raster::raster(dest), silent = TRUE)
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
        ncgrd <- ncdf4::ncvar_def("rfe", "mm", list(dx, dy), missval,
                                  "Rainfall Estimate", "float",
                                  compression = 9)
        z[is.na(z)] <- missval

        nc <- ncdf4::nc_create(ncfl, ncgrd)
        ncdf4::ncvar_put(nc, ncgrd, z)
        ncdf4::nc_close(nc)

        ret <- 0
    }
    return(ret)
}

chirps.6hrAF.download.data <- function(lnk, dest, ncfl, bbox){
    xx <- basename(dest)

    link.exist <- try(readLines(lnk, 1), silent = TRUE)
    if(inherits(link.exist, "try-error")) return(xx)

    dc <- try(curl::curl_download(lnk, dest), silent = TRUE)
    if(!inherits(dc, "try-error")){
        tmpf <- file.path(dirname(ncfl), gsub("\\.gz$", "", basename(dest)))
        R.utils::gunzip(dest, tmpf, remove = FALSE, overwrite = TRUE)

        ##
        ret <- chirps.6hrAF.extract.data(tmpf, ncfl, bbox)
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

chirps.6hrAF.extract.data <- function(tmpf, ncfl, bbox){
    on.exit(close(con))
    con <- file(tmpf, open = "rb")
    val <- try(readBin(con, numeric(), 751 * 801, 4, endian = "big"), silent = TRUE)
    ret <- 1
    if(!inherits(val, "try-error")){
        val[val < 0] <- NA
        val <- matrix(val, 751, 801)
        lon <- seq(-20, 55, 0.1)
        lat <- seq(-40, 40, 0.1)

        ix <- lon >= bbox$minlon & lon <= bbox$maxlon
        iy <- lat >= bbox$minlat & lat <= bbox$maxlat
        lon <- lon[ix]
        lat <- lat[iy]
        val <- val[ix, iy]

        dx <- ncdf4::ncdim_def("lon", "degreeE", lon, longname = "Longitude")
        dy <- ncdf4::ncdim_def("lat", "degreeN", lat, longname = "Latitude")
        missval <- -99
        ncgrd <- ncdf4::ncvar_def("rfe", "kg/m2", list(dx, dy), missval,
                                  "Rainfall Estimate", "float",
                                  compression = 9)
        val[is.na(val)] <- missval

        nc <- ncdf4::nc_create(ncfl, ncgrd)
        ncdf4::ncvar_put(nc, ncgrd, val)
        ncdf4::nc_close(nc)

        ret <- 0
    }

    return(ret)
}
