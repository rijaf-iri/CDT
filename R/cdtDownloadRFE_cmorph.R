
## toexport
cmorph_raw.download.iridl <- function(GalParams, nbfile = 3, GUI = TRUE, verbose = TRUE){
    dlpath <- "http://iridl.ldeo.columbia.edu/SOURCES/.NOAA/.NCEP/.CPC/.CMORPH/.V0px/.RAW"
    vardir <- switch(GalParams$tstep,
                      "hourly" = ".3-hourly/.prcp",
                      "daily" = ".daily/.prcp"
                    )

    rlon <- unlist(GalParams$bbox[c('minlon', 'maxlon')])
    rlon <- paste(c('X', rlon, 'RANGE'), collapse = "/")
    rlat <- unlist(GalParams$bbox[c('minlat', 'maxlat')])
    rlat <- paste(c('Y', rlat, 'RANGE'), collapse = "/")

   if(GalParams$tstep == "daily"){
        rdate <- iridl.format.date(GalParams$tstep, GalParams$date.range)
        urls <- urltools::url_encode(paste0("(", rdate$dates, ")"))
        date.out <- rdate$out
        data.tres <- GalParams$tstep
    }
    else if(GalParams$tstep == "hourly"){
        rdate <- table.format.date.time(GalParams$tstep, GalParams$date.range, GalParams$minhour)
        mois <- c("Jan", "Feb", "Mar", "Apr", "May", "Jun",
                  "Jul", "Aug", "Sep", "Oct", "Nov", "Dec")
        endh <- str_pad(as.numeric(rdate[, 4]) + GalParams$minhour, 2, pad = "0")
        hours <- paste0(paste0(rdate[, 4], "00"), "-", paste0(endh, "00"))
        date.out <- paste0(rdate[, 1], rdate[, 2], rdate[, 3], rdate[, 4])
        rdate <- paste(hours, rdate[, 3], mois[as.numeric(rdate[, 2])], rdate[, 1])
        urls <- urltools::url_encode(paste0("(", rdate, ")"))
        data.tres <- paste0(GalParams$minhour, GalParams$tstep)
    }else return(-1)

    urls <- paste0("T", "/", urls, "/", "VALUE")
    urls <- paste(dlpath, vardir, rlon, rlat, urls, 'data.nc', sep = "/")

    #########
    data.name <- paste0("CMORPH_V0.x_RAW_", data.tres)
    outdir <- file.path(GalParams$dir2save, data.name)
    dir.create(outdir, showWarnings = FALSE, recursive = TRUE)
    destfiles <- file.path(outdir, paste0("cmorph-v0.x-raw_", date.out, ".nc"))

    ret <- cdt.download.data(urls, destfiles, destfiles, nbfile, GUI,
                             verbose, data.name, iridl.download.data)

    return(ret)
}

## toexport
cmorph.download.cpc.ncep <- function(GalParams, nbfile = 3, GUI = TRUE, verbose = TRUE){
    rdate <- table.format.date.time(GalParams$tstep, GalParams$date.range, GalParams$minhour)

    if(GalParams$tstep == "daily"){
        if(GalParams$rfe.src == "cmorphv1.0bld-gb"){
            ftp.dir <- file.path("CMORPH_V1.0/BLD/0.25deg-DLY_EOD/GLB", rdate[, 1], paste0(rdate[, 1], rdate[, 2]))
            filename <- sprintf("CMORPH_V1.0BETA_BLD_0.25deg-DLY_EOD_%s%s%s.gz", rdate[, 1], rdate[, 2], rdate[, 3])
            dataname <- "CMORPH_V1.0_BLD"
            ncflname <- "cmorph-v1.0-bld"
            pars <- list(type = 1,
                         longname = "Gauge - CMORPH_Adj Blended Analysis of daily precipitation ending at EOD")
        }
        else if(GalParams$rfe.src == "cmorphrtv0.xbld-gb"){
            ftp.dir <- file.path("CMORPH_RT/BLD", rdate[, 1], paste0(rdate[, 1], rdate[, 2]))
            filename <- sprintf("CMORPH_V0.x_BLD_0.25deg-DLY_EOD_%s%s%s.gz", rdate[, 1], rdate[, 2], rdate[, 3])
            dataname <- "CMORPH_RT_V0.x_BLD"
            ncflname <- "cmorph-rt-v0.x-bld"
            pars <- list(type = 1,
                         longname = "Blended daily precipitation ending at GTS gauge EOD")
       }
        else if(GalParams$rfe.src == "cmorphv1.0adj-gb"){
            ftp.dir <- file.path("CMORPH_V1.0/CRT/0.25deg-DLY_00Z", rdate[, 1], paste0(rdate[, 1], rdate[, 2]))
            filename <- sprintf("CMORPH_V1.0_ADJ_0.25deg-DLY_00Z_%s%s%s.bz2", rdate[, 1], rdate[, 2], rdate[, 3])
            dataname <- "CMORPH_V1.0_ADJ"
            ncflname <- "cmorph-v1.0-adj"
            pars <- list(type = 2,
                         longname = 'Bias-corrected CMORPH Version 1.0BETA Version, daily precipitation from 00Z-24Z')
        }
        else if(GalParams$rfe.src == "cmorphrtv0.xadj-gb"){
            ftp.dir <- "CMORPH_RT/ICDR/0.25deg-DLY_00Z"
            filename <- sprintf("CMORPH_V0.x_ADJ_0.25deg-DLY_00Z_%s%s%s.nc", rdate[, 1], rdate[, 2], rdate[, 3])
            dataname <- "CMORPH_V0.x_ICDR"
            ncflname <- "cmorph-v0.x-adj"
            pars <- list(type = 3,
                         longname = "NOAA Interim Climate Data Record (ICDR) of CPC Morphing Technique (CMORPH) High Resolution Global Precipitation Estimates")
        }
        else if(GalParams$rfe.src == "cmorphv0.xraw-gb"){
            ftp.dir <- file.path("CMORPH_V0.x/RAW/0.25deg-DLY_00Z", rdate[, 1], paste0(rdate[, 1], rdate[, 2]))
            filename <- sprintf("CMORPH_V0.x_RAW_0.25deg-DLY_00Z_%s%s%s.gz", rdate[, 1], rdate[, 2], rdate[, 3])
            dataname <- "CMORPH_V0.x_RAW"
            ncflname <- "cmorph-v0.x-raw"
            pars <- list(type = 2,
                         longname = "Satellite only precipitation estimates from 00Z-24Z")
        }else return(-1)

        data.tres <- GalParams$tstep
        ncfiles <- paste0(ncflname, "_", rdate[, 1], rdate[, 2], rdate[, 3], ".nc")
    }
    else if(GalParams$tstep == "hourly"){
        if(GalParams$minhour == 1){
            if(GalParams$rfe.src == "cmorphrtv0.xadj-gb"){
                ftp.dir <- "CMORPH_RT/ICDR/0.25deg-HLY"
                filename <- sprintf("CMORPH_V0.x_ADJ_0.25deg-HLY_%s%s%s%s.nc", rdate[, 1], rdate[, 2], rdate[, 3], rdate[, 4])
                dataname <- "CMORPH_V0.x_ICDR"
                ncfiles <- paste0("cmorph-v0.x-adj_", rdate[, 1], rdate[, 2], rdate[, 3], rdate[, 4], ".nc")
                pars <- list(type = 3,
                             longname = "NOAA Interim Climate Data Record (ICDR) of CPC Morphing Technique (CMORPH) High Resolution Global Precipitation Estimates")
            }else return(-1)
        }
        else if(GalParams$minhour == 3){
            if(GalParams$rfe.src == "cmorphv1.0adj-gb"){
                ftp.dir <- file.path("CMORPH_V1.0/CRT/0.25deg-3HLY", rdate[, 1], paste0(rdate[, 1], rdate[, 2]))
                filename <- sprintf("CMORPH_V1.0_ADJ_0.25deg-3HLY_%s%s%s.bz2", rdate[, 1], rdate[, 2], rdate[, 3])
                ix <- !duplicated(filename)
                ftp.dir <- ftp.dir[ix]
                filename <- filename[ix]
                dataname <- "CMORPH_V1.0_ADJ"
                ncfiles <- paste0("cmorph-v1.0-adj_", rdate[, 1], rdate[, 2], rdate[, 3], rdate[, 4], ".nc")
                ncfiles <- split(ncfiles, paste0(rdate[, 1], rdate[, 2], rdate[, 3]))
                pars <- list(type = 4,
                             longname = "RAW CMORPH integrated satellite precipitation estimates")
            }
            else if(GalParams$rfe.src == "cmorphv0.xraw-gb"){
                ftp.dir <- file.path("CMORPH_V0.x/RAW/0.25deg-3HLY", rdate[, 1], paste0(rdate[, 1], rdate[, 2]))
                filename <- sprintf("CMORPH_V0.x_RAW_0.25deg-3HLY_%s%s%s.gz", rdate[, 1], rdate[, 2], rdate[, 3])
                ix <- !duplicated(filename)
                ftp.dir <- ftp.dir[ix]
                filename <- filename[ix]
                dataname <- "CMORPH_V0.x_RAW"
                ncfiles <- paste0("cmorph-v0.x-raw_", rdate[, 1], rdate[, 2], rdate[, 3], rdate[, 4], ".nc")
                ncfiles <- split(ncfiles, paste0(rdate[, 1], rdate[, 2], rdate[, 3]))
                pars <- list(type = 4,
                             longname = "Combined microwave estimates")
            }else return(-1)
        }else return(-1)

        data.tres <- paste0(GalParams$minhour, GalParams$tstep)
    }
    else if(GalParams$tstep == "minute"){
        if(GalParams$minhour == 30){
            if(GalParams$rfe.src == "cmorphrtv0.xadj-gb"){
                ftp.dir <- "CMORPH_RT/ICDR/8km-30min"
                filename <- sprintf("CMORPH_V0.x_ADJ_8km-30min_%s%s%s%s.nc", rdate[, 1], rdate[, 2], rdate[, 3], rdate[, 4])
                ix <- !duplicated(filename)
                filename <- filename[ix]
                dataname <- "CMORPH_V0.x_ICDR"
                ncfiles <- paste0("cmorph-v0.x-adj_", rdate[, 1], rdate[, 2], rdate[, 3], rdate[, 4], rdate[, 5], ".nc")
                ncfiles <- split(ncfiles, paste0(rdate[, 1], rdate[, 2], rdate[, 3], rdate[, 4]))
                pars <- list(type = 5,
                             longname = "NOAA Interim Climate Data Record (ICDR) of CPC Morphing Technique (CMORPH) High Resolution Global Precipitation Estimates")
            }
            else if(GalParams$rfe.src == "cmorphv0.xrt-gb"){
                ftp.dir <- file.path("CMORPH_RT/GLOBE/data", rdate[, 1], paste0(rdate[, 1], rdate[, 2]))
                filename <- sprintf("CMORPH_V0.x_RT_8km-30min_%s%s%s%s.gz", rdate[, 1], rdate[, 2], rdate[, 3], rdate[, 4])
                ix <- !duplicated(filename)
                ftp.dir <- ftp.dir[ix]
                filename <- filename[ix]
                dataname <- "CMORPH_V0.x_RT"
                ncfiles <- paste0("cmorph-v0.x-rt_", rdate[, 1], rdate[, 2], rdate[, 3], rdate[, 4], rdate[, 5], ".nc")
                ncfiles <- split(ncfiles, paste0(rdate[, 1], rdate[, 2], rdate[, 3], rdate[, 4]))
                pars <- list(type = 6,
                             longname = "CMORPH Rain accumulation (Real-Time Version)")
            }
            else if(GalParams$rfe.src == "cmorphv0.xraw-gb"){
                ftp.dir <- file.path("CMORPH_V0.x/RAW/8km-30min", rdate[, 1], paste0(rdate[, 1], rdate[, 2]))
                filename <- sprintf("CMORPH_V0.x_RAW_8km-30min_%s%s%s%s.gz", rdate[, 1], rdate[, 2], rdate[, 3], rdate[, 4])
                ix <- !duplicated(filename)
                ftp.dir <- ftp.dir[ix]
                filename <- filename[ix]
                dataname <- "CMORPH_V0.x_RAW"
                ncfiles <- paste0("cmorph-v0.x-raw_", rdate[, 1], rdate[, 2], rdate[, 3], rdate[, 4], rdate[, 5], ".nc")
                ncfiles <- split(ncfiles, paste0(rdate[, 1], rdate[, 2], rdate[, 3], rdate[, 4]))
                pars <- list(type = 6,
                             longname = "CMORPH Precipitation estimates")
            }else return(-1)
        }else return(-1)

        data.tres <- paste0(GalParams$minhour, GalParams$tstep)
    }else return(-1)

    #########

    ftp.cpc <- "ftp://ftp.cpc.ncep.noaa.gov/precip"
    urls <- file.path(ftp.cpc, ftp.dir, filename)

    #########
    data.name <- paste0(dataname, "_", data.tres)
    outdir <- file.path(GalParams$dir2save, data.name)
    extrdir <- file.path(outdir, "Extracted")
    dir.create(extrdir, showWarnings = FALSE, recursive = TRUE)
    origdir <- file.path(outdir, "Data_Global")
    dir.create(origdir, showWarnings = FALSE, recursive = TRUE)
    destfiles <- file.path(origdir, filename)
    if(pars$type %in% 4:6){
        ncfiles <- lapply(ncfiles, function(x) file.path(extrdir, x))
    }else{
        ncfiles <- file.path(extrdir, ncfiles)
    }


    ret <- cdt.download.data(urls, destfiles, ncfiles, nbfile, GUI,
                             verbose, data.name, cmorph.download.data,
                             bbox = GalParams$bbox, pars = pars)

    return(ret)
}

#################################################################################

cmorph.download.data <- function(lnk, dest, ncfl, bbox, pars){
    xx <- basename(dest)

    link.exist <- try(readLines(lnk, 1), silent = TRUE)
    if(inherits(link.exist, "try-error")) return(xx)

    dc <- try(curl::curl_download(lnk, dest), silent = TRUE)
    if(!inherits(dc, "try-error")){
        tmpdir <- dirname(ncfl[[1]][1])

        if(grepl("\\.gz$", dest, ignore.case = TRUE))
        {
            tmpf <- file.path(tmpdir, gsub("\\.gz$", "", basename(dest)))
            R.utils::gunzip(dest, tmpf, remove = FALSE, overwrite = TRUE)
            delete <- TRUE
        }
        else if(grepl("\\.bz2$", dest, ignore.case = TRUE)){
            tmpf <- file.path(tmpdir, gsub("[.]bz2$", "", basename(dest)))
            R.utils::bunzip2(dest, tmpf, remove = FALSE, overwrite = TRUE)
            delete <- TRUE
        }
        else if(grepl("\\.nc$", dest, ignore.case = TRUE)){
            tmpf <- dest
            delete <- FALSE
        }else return(xx)

        ##
        extract.data <- paste0("cmorph.extract.data.", pars$type)
        extract.data <- get(extract.data, mode = "function")
        ret <- extract.data(tmpf, ncfl, bbox, pars)
        if(delete) unlink(tmpf)

        ##
        if(ret == 0){
            xx <- NULL
        }else{
            unlink(dest)
        }
    }
    return(xx)
}

cmorph.extract.data.1 <- function(tmpf, ncfl, bbox, pars){
    on.exit(close(con))
    con <- file(tmpf, open = "rb")
    val <- try(readBin(con, numeric(), 720 * 1440, 4, endian = "little"), silent = TRUE)
    ret <- 1
    if(!inherits(val, "try-error")){
        lon <- seq(0.125, length.out = 1440, by = 0.25)
        lon <- ((lon + 180) %% 360) - 180
        lat <- seq(-89.875, length.out = 720, by = 0.25)

        ############
        ox <- order(lon)
        lon <- lon[ox]
        ix <- lon >= bbox$minlon & lon <= bbox$maxlon
        iy <- lat >= bbox$minlat & lat <= bbox$maxlat
        lon <- lon[ix]
        lat <- lat[iy]

        ############
        missval <- -999.0
        dx <- ncdf4::ncdim_def("lon", "degreeE", lon, longname = "Longitude")
        dy <- ncdf4::ncdim_def("lat", "degreeN", lat, longname = "Latitude")
        ncgrd <- ncdf4::ncvar_def("precip", "mm", list(dx, dy), missval,
                                  pars$longname, "float", compression = 9)

        ############
        val <- matrix(val, 1440, 720)
        val <- val[ox, ]
        val <- val[ix, iy]

        ############
        nc <- ncdf4::nc_create(ncfl, ncgrd)
        ncdf4::ncvar_put(nc, ncgrd, val)
        ncdf4::nc_close(nc)

        ret <- 0
    }

    return(ret)
}

cmorph.extract.data.2 <- function(tmpf, ncfl, bbox, pars){
    on.exit(close(con))
    con <- file(tmpf, open = "rb")
    val <- try(readBin(con, numeric(), 480 * 1440, 4, endian = "little"), silent = TRUE)
    ret <- 1
    if(!inherits(val, "try-error")){
        lon <- seq(0.125, length.out = 1440, by = 0.25)
        lon <- ((lon + 180) %% 360) - 180
        lat <- seq(-59.875, length.out = 480, by = 0.25)

        ############
        ox <- order(lon)
        lon <- lon[ox]
        ix <- lon >= bbox$minlon & lon <= bbox$maxlon
        iy <- lat >= bbox$minlat & lat <= bbox$maxlat
        lon <- lon[ix]
        lat <- lat[iy]

        ############
        missval <- -999.0
        dx <- ncdf4::ncdim_def("lon", "degreeE", lon, longname = "Longitude")
        dy <- ncdf4::ncdim_def("lat", "degreeN", lat, longname = "Latitude")
        ncgrd <- ncdf4::ncvar_def("precip", "mm", list(dx, dy), missval,
                                  pars$longname, "float", compression = 9)

        ############
        val <- matrix(val, 1440, 480)
        val <- val[ox, ]
        val <- val[ix, iy]

        ############
        nc <- ncdf4::nc_create(ncfl, ncgrd)
        ncdf4::ncvar_put(nc, ncgrd, val)
        ncdf4::nc_close(nc)

        ret <- 0
    }

    return(ret)
}

###########################################################################

cmorph.extract.data.3 <- function(tmpf, ncfl, bbox, pars){
    nc <- try(ncdf4::nc_open(tmpf), silent = TRUE)
    ret <- 1
    if(!inherits(nc, "try-error")){
        varid <- "cmorph"
        lon <- nc$var[[varid]]$dim[[1]]$vals
        lat <- nc$var[[varid]]$dim[[2]]$vals

        lon <- ((lon + 180) %% 360) - 180
        ix <- lon >= bbox$minlon & lon <= bbox$maxlon
        iy <- lat >= bbox$minlat & lat <= bbox$maxlat
        start <- c(which(ix)[1], which(iy)[1], 1)
        count <- c(diff(range(which(ix))) + 1, diff(range(which(iy))) + 1, 1)

        val <- ncdf4::ncvar_get(nc, varid, start, count)
        ncdf4::nc_close(nc)

        ############
        missval <- -999.0
        lon <- lon[ix]
        lat <- lat[iy]
        ox <- order(lon)
        lon <- lon[ox]
        val <- val[ox, ]
        val[is.na(val)] <- missval

        ############
        dx <- ncdf4::ncdim_def("lon", "degreeE", lon, longname = "Longitude")
        dy <- ncdf4::ncdim_def("lat", "degreeN", lat, longname = "Latitude")
        ncgrd <- ncdf4::ncvar_def("precip", "mm", list(dx, dy), missval,
                                  pars$longname, "float", compression = 9)

        ############
        nc <- ncdf4::nc_create(ncfl, ncgrd)
        ncdf4::ncvar_put(nc, ncgrd, val)
        ncdf4::nc_close(nc)

        ret <- 0
    }
    return(ret)
}


cmorph.extract.data.4 <- function(tmpf, ncfl, bbox, pars){
    on.exit(close(con))
    con <- file(tmpf, open = "rb")
    val <- try(readBin(con, numeric(), 8 * 480 * 1440, 4, endian = "little"), silent = TRUE)
    ret <- 1
    if(!inherits(val, "try-error")){
        lon <- seq(0.125, length.out = 1440, by = 0.25)
        lon <- ((lon + 180) %% 360) - 180
        lat <- seq(-59.875, length.out = 480, by = 0.25)

        ############
        ox <- order(lon)
        lon <- lon[ox]
        ix <- lon >= bbox$minlon & lon <= bbox$maxlon
        iy <- lat >= bbox$minlat & lat <= bbox$maxlat
        lon <- lon[ix]
        lat <- lat[iy]

        ############
        missval <- -999.0
        dx <- ncdf4::ncdim_def("lon", "degreeE", lon, longname = "Longitude")
        dy <- ncdf4::ncdim_def("lat", "degreeN", lat, longname = "Latitude")
        ncgrd <- ncdf4::ncvar_def("precip", "mm", list(dx, dy), missval,
                                  pars$longname, "float", compression = 9)

        ############
        val <- array(val, c(1440, 480, 8))
        val <- val[ox, , ]
        val <- val[ix, iy, ]

        ############
        ncfl <- ncfl[[1]]
        daty <- gsub(".*_|\\.nc$", "", basename(ncfl))
        ix <- (as.numeric(substr(daty, 9, 10)) / 3) + 1
        val <- val[, , ix, drop = FALSE]

        ############
        for(j in seq_along(ncfl)){
            nc <- ncdf4::nc_create(ncfl[j], ncgrd)
            ncdf4::ncvar_put(nc, ncgrd, val[, , j])
            ncdf4::nc_close(nc)
        }

        ret <- 0
    }

    return(ret)
}

cmorph.extract.data.5 <- function(tmpf, ncfl, bbox, pars){
    nc <- try(ncdf4::nc_open(tmpf), silent = TRUE)
    ret <- 1
    if(!inherits(nc, "try-error")){
        varid <- "cmorph"
        lon <- nc$var[[varid]]$dim[[1]]$vals
        lat <- nc$var[[varid]]$dim[[2]]$vals
        # as.POSIXct(nc$var[[varid]]$dim[[3]]$vals, tz = 'UTC', origin = '1970-01-01 00:00:00')

        lon <- ((lon + 180) %% 360) - 180
        ix <- lon >= bbox$minlon & lon <= bbox$maxlon
        iy <- lat >= bbox$minlat & lat <= bbox$maxlat
        start <- c(which(ix)[1], which(iy)[1], 1)
        count <- c(diff(range(which(ix))) + 1, diff(range(which(iy))) + 1, 2)

        val <- ncdf4::ncvar_get(nc, varid, start, count)
        ncdf4::nc_close(nc)

        ############
        missval <- -999.0
        lon <- lon[ix]
        lat <- lat[iy]
        ox <- order(lon)
        lon <- lon[ox]
        val <- val[ox, , ]
        ## mm/hr to mm
        val <- val * 0.5
        val[is.na(val)] <- missval

        ############
        dx <- ncdf4::ncdim_def("lon", "degreeE", lon, longname = "Longitude")
        dy <- ncdf4::ncdim_def("lat", "degreeN", lat, longname = "Latitude")
        ncgrd <- ncdf4::ncvar_def("precip", "mm", list(dx, dy), missval,
                                  pars$longname, "float", compression = 9)

        ############
        ncfl <- ncfl[[1]]
        daty <- gsub(".*_|\\.nc$", "", basename(ncfl))
        ix <- (as.numeric(substr(daty, 11, 12)) / 30) + 1
        val <- val[, , ix, drop = FALSE]

        ############
        for(j in seq_along(ncfl)){
            nc <- ncdf4::nc_create(ncfl[j], ncgrd)
            ncdf4::ncvar_put(nc, ncgrd, val[, , j])
            ncdf4::nc_close(nc)
        }

        ret <- 0
    }

    return(ret)
}

cmorph.extract.data.6 <- function(tmpf, ncfl, bbox, pars){
    on.exit(close(con))
    con <- file(tmpf, open = "rb")
    val <- try(readBin(con, numeric(), 2 * 1649 * 4948, 4, endian = "little"), silent = TRUE)
    ret <- 1
    if(!inherits(val, "try-error")){
        lon <- seq(0.0363783345, length.out = 4948, by = 0.072756669)
        lon <- ((lon + 180) %% 360) - 180
        lat <- seq(-59.963614312, length.out = 1649, by = 0.072771376)

        ############
        ox <- order(lon)
        lon <- lon[ox]
        ix <- lon >= bbox$minlon & lon <= bbox$maxlon
        iy <- lat >= bbox$minlat & lat <= bbox$maxlat
        lon <- lon[ix]
        lat <- lat[iy]

        ############
        missval <- -999.0
        dx <- ncdf4::ncdim_def("lon", "degreeE", lon, longname = "Longitude")
        dy <- ncdf4::ncdim_def("lat", "degreeN", lat, longname = "Latitude")
        ncgrd <- ncdf4::ncvar_def("precip", "mm", list(dx, dy), missval,
                                  pars$longname, "float", compression = 9)
        ############
        val <- array(val, c(4948, 1649, 2))
        val <- val[ox, , ]
        val <- val[ix, iy, ]
        ## mm/hr to mm
        val <- val * 0.5

        ############
        ncfl <- ncfl[[1]]
        daty <- gsub(".*_|\\.nc$", "", basename(ncfl))
        ix <- (as.numeric(substr(daty, 11, 12)) / 30) + 1
        val <- val[, , ix, drop = FALSE]

        ############
        for(j in seq_along(ncfl)){
            nc <- ncdf4::nc_create(ncfl[j], ncgrd)
            ncdf4::ncvar_put(nc, ncgrd, val[, , j])
            ncdf4::nc_close(nc)
        }

        ret <- 0
    }

    return(ret)
}
