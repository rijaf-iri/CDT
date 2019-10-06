
## toexport
persiann.download.uci <- function(GalParams, nbfile = 3, GUI = TRUE, verbose = TRUE){
    rdate <- table.format.date.time(GalParams$tstep, GalParams$date.range, GalParams$minhour)
    ftp.uci <- "ftp://persiann.eng.uci.edu/CHRSdata/PERSIANN"

    if(GalParams$tstep == "hourly"){
        if(GalParams$minhour == 1){
            baseurl <- file.path("hrly", rdate[, 1])
            filename <- sprintf("m6s4rr%s%s%s.bin.gz", substr(rdate[, 1], 3, 4), rdate[, 5], rdate[, 4])
        }
        else if(GalParams$minhour == 3){
            baseurl <- "3hrly"
            baseurl <- ifelse(rdate[, 1] == "2019" & (as.numeric(rdate[, 5]) > 147),
                              baseurl, file.path(baseurl, rdate[, 1]))
            filename <- sprintf("m6s4_3h%s%s%s.bin.gz", substr(rdate[, 1], 3, 4), rdate[, 5], rdate[, 4])
        }
        else if(GalParams$minhour == 6){
            baseurl <- "6hrly"
            filename <- sprintf("m6s4_6h%s%s%s.bin.gz", substr(rdate[, 1], 3, 4), rdate[, 5], rdate[, 4])
        }else return(-1)

        ncfiles <- sprintf("persiann_%s%s%s%s.nc", rdate[, 1], rdate[, 2], rdate[, 3], rdate[, 4])
    }
    else if(GalParams$tstep == "daily"){
        baseurl <- "daily"
        filename <- sprintf("ms6s4_d%s%s.bin.gz", substr(rdate[, 1], 3, 4), rdate[, 4])
        ncfiles <- sprintf("persiann_%s%s%s.nc", rdate[, 1], rdate[, 2], rdate[, 3])
    }
    else if(GalParams$tstep == "monthly"){
        baseurl <- "mthly"
        filename <- sprintf("ms6s4_m%s%s.bin.gz", substr(rdate[, 1], 3, 4), rdate[, 2])
        ncfiles <- sprintf("persiann_%s%s.nc", rdate[, 1], rdate[, 2])
    }else return(-1)

    urls <- file.path(ftp.uci, baseurl, filename)

    #########
    data.name <- paste0("PERSIANN_", GalParams$tstep)
    outdir <- file.path(GalParams$dir2save, data.name)
    extrdir <- file.path(outdir, "Extracted")
    dir.create(extrdir, showWarnings = FALSE, recursive = TRUE)
    origdir <- file.path(outdir, "Data_Global")
    dir.create(origdir, showWarnings = FALSE, recursive = TRUE)
    destfiles <- file.path(origdir, filename)
    ncfiles <- file.path(extrdir, ncfiles)

    ret <- cdt.download.data(urls, destfiles, ncfiles, nbfile, GUI,
                             verbose, data.name, persiann.download.data.uci,
                             bbox = GalParams$bbox, cdr = FALSE)

    return(ret)
}

## toexport
persiann_cdr.download.uci <- function(GalParams, nbfile = 3, GUI = TRUE, verbose = TRUE){
    rdate <- table.format.date.time(GalParams$tstep, GalParams$date.range)
    ftp.uci <- "ftp://persiann.eng.uci.edu/CHRSdata/PERSIANN-CDR"

    if(GalParams$tstep == "daily"){
        baseurl <- "daily"
        filename <- sprintf("aB1_d%s%s.bin.gz", substr(rdate[, 1], 3, 4), rdate[, 4])
        ncfiles <- sprintf("persiann-cdr_%s%s%s.nc", rdate[, 1], rdate[, 2], rdate[, 3])
    }
    else if(GalParams$tstep == "monthly"){
        baseurl <- "mthly"
        filename <- sprintf("aB1_m%s%s.bin.gz", substr(rdate[, 1], 3, 4), rdate[, 2])
        ncfiles <- sprintf("persiann-cdr_%s%s.nc", rdate[, 1], rdate[, 2])
    }else return(-1)

    urls <- file.path(ftp.uci, baseurl, filename)

    #########
    data.name <- paste0("PERSIANN-CDR_", GalParams$tstep)
    outdir <- file.path(GalParams$dir2save, data.name)
    extrdir <- file.path(outdir, "Extracted")
    dir.create(extrdir, showWarnings = FALSE, recursive = TRUE)
    origdir <- file.path(outdir, "Data_Global")
    dir.create(origdir, showWarnings = FALSE, recursive = TRUE)
    destfiles <- file.path(origdir, filename)
    ncfiles <- file.path(extrdir, ncfiles)

    ret <- cdt.download.data(urls, destfiles, ncfiles, nbfile, GUI,
                             verbose, data.name, persiann.download.data.uci,
                             bbox = GalParams$bbox, cdr = TRUE)

    return(ret)
}

## toexport
## not in GUI
persiann_cdr.download.ncei <- function(GalParams, nbfile = 3, GUI = TRUE, verbose = TRUE){
    rdate <- table.format.date.time(GalParams$tstep, GalParams$date.range)
    url.ncei <- "https://www.ncei.noaa.gov/data/precipitation-persiann/access"

    daty <- paste0(rdate[, 1], rdate[, 2], rdate[, 3])
    url.year <- file.path(url.ncei, unique(rdate[, 1]))

    xres <- lapply(seq_along(url.year), function(j){
        x <- readLines(url.year[j])
        x <- XML::htmlParse(x, asText = TRUE)
        x <- XML::readHTMLTable(x)
        x <- as.character(x[[1]]$Name)
        x <- x[grepl("PERSIANN-CDR_v01r01_", x)]
        cbind(url.year[j], x)
    })
    xres <- do.call(rbind, xres)
    daty1 <- substr(xres[, 2], 21, 28)
    ix <- daty1 %in% daty
    xres <- xres[ix, , drop = FALSE]

    urls <- file.path(xres[, 1], xres[, 2])
    filename <- xres[, 2]
    ncfiles <- paste0("persiann-cdr_", daty1[ix], ".nc")

    ix1 <- !daty %in% daty1
    if(any(ix1)){
        not.available <- paste0("PERSIANN-CDR_v01r01_", daty[ix1], "_[*].nc")
        urls <- c(rep(NA, length(not.available)), urls)
        filename <- c(not.available, filename)
        ncfiles <- c(rep(NA, length(not.available)), ncfiles)
    }

    #########
    data.name <- paste0("PERSIANN-CDR_v01r01_", GalParams$tstep)
    outdir <- file.path(GalParams$dir2save, data.name)
    extrdir <- file.path(outdir, "Extracted")
    dir.create(extrdir, showWarnings = FALSE, recursive = TRUE)
    origdir <- file.path(outdir, "Data_Global")
    dir.create(origdir, showWarnings = FALSE, recursive = TRUE)
    destfiles <- file.path(origdir, filename)
    ncfiles <- file.path(extrdir, ncfiles)

    ret <- cdt.download.data(urls, destfiles, ncfiles, nbfile, GUI,
                             verbose, data.name, persiann.download.data.ncei,
                             bbox = GalParams$bbox)

    return(ret)
}

#################################################################################

persiann.download.data.uci <- function(lnk, dest, ncfl, bbox, cdr){
    xx <- basename(dest)

    link.exist <- try(readLines(lnk, 1), silent = TRUE)
    if(inherits(link.exist, "try-error")) return(xx)

    dc <- try(curl::curl_download(lnk, dest), silent = TRUE)
    if(!inherits(dc, "try-error")){
        tmpdir <- dirname(ncfl)
        tmpf <- file.path(tmpdir, gsub("\\.gz$", "", basename(dest)))
        R.utils::gunzip(dest, tmpf, remove = FALSE, overwrite = TRUE)

        ret <- persiann.extract.data.uci(tmpf, ncfl, bbox, cdr)

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

persiann.extract.data.uci <- function(tmpf, ncfl, bbox, cdr){
    on.exit(close(con))
    endian <- if(cdr) "little" else "big"
    con <- file(tmpf, open = "rb")
    val <- try(readBin(con, numeric(), 480 * 1440, 4, endian = endian), silent = TRUE)
    ret <- 1
    if(!inherits(val, "try-error")){
        lon <- seq(0.125, 359.875, 0.25)
        lon <- ((lon + 180) %% 360) - 180
        lat <- seq(59.875, -59.875, -0.25)

        ox <- order(lon)
        oy <- order(lat)
        lon <- lon[ox]
        lat <- lat[oy]

        ix <- lon >= bbox$minlon & lon <= bbox$maxlon
        iy <- lat >= bbox$minlat & lat <= bbox$maxlat
        lon <- lon[ix]
        lat <- lat[iy]

        val[val == -9999] <- NA
        val[is.infinite(val)] <- NA
        val <- matrix(val, 1440, 480)
        val <- val[ox, oy]
        val <- val[ix, iy]

        dx <- ncdf4::ncdim_def("lon", "degreeE", lon, longname = "Longitude")
        dy <- ncdf4::ncdim_def("lat", "degreeN", lat, longname = "Latitude")
        missval <- -9999
        longname <- "Precipitation Estimation from Remotely Sensed Information using Artificial Neural Networks"
        if(cdr) longname <- paste("NOAA Climate Data Record (CDR) of", longname)
        ncgrd <- ncdf4::ncvar_def("precip", "mm", list(dx, dy), missval, longname, "float", compression = 9)
        val[is.na(val)] <- missval

        nc <- ncdf4::nc_create(ncfl, ncgrd)
        ncdf4::ncvar_put(nc, ncgrd, val)
        ncdf4::nc_close(nc)

        ret <- 0
    }

    return(ret)
}

persiann.download.data.ncei <- function(lnk, dest, ncfl, bbox){
    xx <- basename(dest)
    if(is.na(lnk)) return(xx)

    link.exist <- try(readLines(lnk, 1), silent = TRUE)
    if(inherits(link.exist, "try-error")) return(xx)

    dc <- try(curl::curl_download(lnk, dest), silent = TRUE)
    if(!inherits(dc, "try-error")){
        ret <- persiann.extract.data.ncei(dest, ncfl, bbox)
        if(ret == 0){
            xx <- NULL
        }else{
            unlink(dest)
        }
    }
    return(xx)
}

persiann.extract.data.ncei <- function(dest, ncfl, bbox){
    nc <- try(ncdf4::nc_open(dest), silent = TRUE)
    ret <- 1
    if(!inherits(nc, "try-error")){
        varid <- "precipitation"
        lon <- nc$var[[varid]]$dim[[2]]$vals
        lat <- nc$var[[varid]]$dim[[1]]$vals

        lon <- ((lon + 180) %% 360) - 180
        ix <- lon >= bbox$minlon & lon <= bbox$maxlon
        iy <- lat >= bbox$minlat & lat <= bbox$maxlat
        start <- c(which(iy)[1], which(ix)[1], 1)
        count <- c(diff(range(which(iy))) + 1, diff(range(which(ix))) + 1, 1)

        val <- ncdf4::ncvar_get(nc, varid, start, count)
        ncdf4::nc_close(nc)

        lon <- lon[ix]
        lat <- lat[iy]

        ox <- order(lon)
        oy <- order(lat)

        lon <- lon[ox]
        lat <- lat[oy]
        val <- t(val)[ox, oy]

        dx <- ncdf4::ncdim_def("lon", "degreeE", lon, longname = "Longitude")
        dy <- ncdf4::ncdim_def("lat", "degreeN", lat, longname = "Latitude")
        missval <- -9999
        ncgrd <- ncdf4::ncvar_def("precip", "mm", list(dx, dy), missval,
                                  "NOAA Climate Data Record of PERSIANN-CDR daily precipitation",
                                  "float", compression = 9)
        val[is.na(val)] <- missval

        nc <- ncdf4::nc_create(ncfl, ncgrd)
        ncdf4::ncvar_put(nc, ncgrd, val)
        ncdf4::nc_close(nc)

        ret <- 0
    }
    return(ret)
}
