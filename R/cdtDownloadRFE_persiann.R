
persiann.download.uci <- function(GalParams, nbfile = 3, GUI = TRUE, verbose = TRUE){
    info <- persiann.info.uci(GalParams)
    if(is.null(info)) return(-3)

    data.name <- paste0(info$dataname, '_', info$data.tres)
    outdir <- file.path(GalParams$dir2save, data.name)
    extrdir <- file.path(outdir, "Extracted")
    if(!dir.exists(extrdir))
        dir.create(extrdir, showWarnings = FALSE, recursive = TRUE)
    origdir <- file.path(outdir, "Data_global")
    if(!dir.exists(origdir))
        dir.create(origdir, showWarnings = FALSE, recursive = TRUE)
    destfiles <- file.path(origdir, info$filename)
    ncfiles <- file.path(extrdir, info$ncfiles)

    ret <- cdt.download.data(info$urls, destfiles, ncfiles, nbfile, GUI,
                             verbose, data.name, persiann.download.data,
                             bbox = GalParams$bbox, pars = info$pars)

    return(ret)
}

persiann.coverage.uci <- function(GalParams){
    if(GalParams$tstep %in% c('minute', 'hourly')){
        timestep <- paste(GalParams$minhour, GalParams$tstep)
    }else{
        timestep <- GalParams$tstep
    }
    out <- list(name = GalParams$rfe.src, timestep = timestep)

    info <- persiann.info.uci(GalParams)
    if(is.null(info)) return(out)

    out$name <- info$dataname
    url <- file.path(info$baseurl, info$dirpath)

    if(info$deppath == 0){
        tmp <- persiann.uci.dates(url, 'file', info$fileformat, info$dateformat)
        if(is.null(tmp)) return(out)
        out$end <- tmp[length(tmp)]
    }else{
        tmp <- persiann.uci.dates(url, 'directory')
        if(is.null(tmp)) return(out)

        end_d <- tmp[length(tmp)]
        this_year <- as.numeric(format(Sys.time(), '%Y'))
        end_year <- as.numeric(end_d)
        if(end_year > this_year) end_year <- this_year
        seq_dates <- seq(end_year - 1, end_year, 1)
        seq_dates <- rev(seq_dates)

        for(j in seq_along(seq_dates)){
            url <- file.path(info$baseurl, info$dirpath, seq_dates[j])
            ret <- persiann.uci.table(url)
            if(is.null(ret)) return(out)
            if(length(ret) > 0) break
        }

        end_d <- persiann.uci.end_dates(ret, info$fileformat, info$dateformat)
        if(is.null(end_d)) return(out)
        out$end <- end_d[length(end_d)]
    }

    end_frmt <- switch(GalParams$tstep,
                       "minute" = "%Y%m%d%H%M",
                       "hourly" = "%Y%m%d%H",
                       "daily" = "%Y%m%d",
                       "monthly" = "%Y%m")
    out$end <- format(out$end, end_frmt)
    out$start <- switch(GalParams$rfe.src,
                    "persiann.pdir-gb" = {
                        switch(GalParams$tstep,
                           "minute" = "202109010000",
                           "hourly" = "2000030100",
                           "daily" = "20000301",
                           "monthly" = "200003")
                    },
                    "persiann.ccs-gb" = {
                        switch(GalParams$tstep,
                           "hourly" = "2003010100",
                           "daily" = "20030101",
                           "monthly" = "200301")
                    },
                     "persiann.cdr-gb" = {
                        switch(GalParams$tstep,
                           "hourly" = "1983010100",
                           "daily" = "19830101",
                           "monthly" = "198301")
                    },
                    "persiann-gb" = {
                        switch(GalParams$tstep,
                           "hourly" = "2000030100",
                           "daily" = "20000301",
                           "monthly" = "200003")
                    }
                )
    return(out)
}

persiann.info.uci <- function(GalParams){
    rdate <- table.format.date.time(GalParams$tstep, GalParams$date.range, GalParams$minhour)
    if(is.null(rdate)) return(NULL)

    yy_frmt <- substr(rdate[, 1], 3, 4)
    ftp.uci <- "https://persiann.eng.uci.edu/CHRSdata"

    if(GalParams$tstep == "minute"){
        if(GalParams$minhour == 30){
            if(GalParams$rfe.src == "persiann.pdir-gb"){
                dirpath <- "30PDIR"
                deppath <- 0
                dataname <- "PDIR-Now"
                dateformat <- "%y%m%d%H%M"
                fileformat <- "pdirnow%s%s%s%s%s.bin.gz"
                minutes <- ifelse(rdate[, 5] == '00', '15', '45')
                filename <- sprintf(fileformat, yy_frmt, rdate[, 2], rdate[, 3], rdate[, 4], minutes)
                urls <- file.path(ftp.uci, dirpath, filename)
                ncfiles <- paste0("pdir-now_", rdate[, 1], rdate[, 2], rdate[, 3], rdate[, 4], rdate[, 5], ".nc")
                pars <- list(factor = 0.01, what = integer(), bytes = 2, endian = "little")
            }else return(NULL)
        }else return(NULL)
        data.tres <- paste0(GalParams$minhour, GalParams$tstep)
    }else if(GalParams$tstep == "hourly"){
        if(GalParams$minhour == 1){
            if(GalParams$rfe.src == "persiann.pdir-gb"){
                dirpath <- "PDIRNow/PDIRNow1hourly"
                deppath <- 1
                dataname <- "PDIR-Now"
                dateformat <- "%y%m%d%H"
                fileformat <- "pdirnow1h%s%s%s%s.bin.gz"
                filename <- sprintf(fileformat, yy_frmt, rdate[, 2], rdate[, 3], rdate[, 4])
                urls <- file.path(ftp.uci, dirpath, rdate[, 1], filename)
                ncfiles <- paste0("pdir-now_", rdate[, 1], rdate[, 2], rdate[, 3], rdate[, 4], ".nc")
                pars <- list(factor = 0.01, what = integer(), bytes = 2, endian = "little")
            }else if(GalParams$rfe.src == "persiann.ccs-gb"){
                dirpath <- "PERSIANN-CCS/hrly"
                deppath <- 1
                dataname <- "PERSIANN-CCS"
                dateformat <- "%y%j%H"
                fileformat <- "rgccs1h%s%s%s.bin.gz"
                filename <- sprintf(fileformat, yy_frmt, rdate[, 5], rdate[, 4])
                urls <- file.path(ftp.uci, dirpath, rdate[, 1], filename)
                ncfiles <- paste0("persiann-css_", rdate[, 1], rdate[, 2], rdate[, 3], rdate[, 4], ".nc")
                pars <- list(factor = 0.01, what = integer(), bytes = 2, endian = "big")
            }else return(NULL)
        }else if(GalParams$minhour == 3){
            if(GalParams$rfe.src == "persiann.pdir-gb"){
                dirpath <- "PDIRNow/PDIRNow3hourly"
                deppath <- 0
                dataname <- "PDIR-Now"
                dateformat <- "%y%m%d%H"
                fileformat <- "pdirnow3h%s%s%s%s.bin.gz"
                filename <- sprintf(fileformat, yy_frmt, rdate[, 2], rdate[, 3], rdate[, 4])
                urls <- file.path(ftp.uci, dirpath, filename)
                ncfiles <- paste0("pdir-now_", rdate[, 1], rdate[, 2], rdate[, 3], rdate[, 4], ".nc")
                pars <- list(factor = 1, what = numeric(), bytes = 4, endian = "little")
            }else if(GalParams$rfe.src == "persiann.ccs-gb"){
                dirpath <- "PERSIANN-CCS/3hrly"
                deppath <- 0
                dataname <- "PERSIANN-CCS"
                dateformat <- "%y%j%H"
                fileformat <- "rgccs3h%s%s%s.bin.gz"
                filename <- sprintf(fileformat, yy_frmt, rdate[, 5], rdate[, 4])
                urls <- file.path(ftp.uci, dirpath, filename)
                ncfiles <- paste0("persiann-css_", rdate[, 1], rdate[, 2], rdate[, 3], rdate[, 4], ".nc")
                pars <- list(factor = 0.01, what = integer(), bytes = 2, endian = "big")
            }else if(GalParams$rfe.src == "persiann.cdr-gb"){
                dirpath <- "PERSIANN-CDR/adj_3hB1"
                deppath <- 0
                dataname <- "PERSIANN-CDR"
                dateformat <- "%y%j%H"
                fileformat <- "aB1rr%s%s%s.bin.gz"
                filename <- sprintf(fileformat, yy_frmt, rdate[, 5], rdate[, 4])
                urls <- file.path(ftp.uci, dirpath, filename)
                ncfiles <- paste0("persiann-cdr_", rdate[, 1], rdate[, 2], rdate[, 3], rdate[, 4], ".nc")
                pars <- list(factor = 3, what = numeric(), bytes = 4, endian = "little")
            }else if(GalParams$rfe.src == "persiann-gb"){
                dirpath <- "PERSIANN/3hrly"
                deppath <- 0
                dataname <- "PERSIANN"
                dateformat <- "%y%j%H"
                fileformat <- "m6s4_3h%s%s%s.bin.gz"
                year <- as.numeric(rdate[, 1])
                dirpath1 <- paste0(dirpath, '/', year)
                dirpath1[year >= 2021] <- dirpath
                filename <- sprintf(fileformat, yy_frmt, rdate[, 5], rdate[, 4])
                urls <- file.path(ftp.uci, dirpath1, filename)
                ncfiles <- paste0("persiann_", rdate[, 1], rdate[, 2], rdate[, 3], rdate[, 4], ".nc")
                pars <- list(factor = 1, what = numeric(), bytes = 4, endian = "big")
            }else return(NULL)
        }else if(GalParams$minhour == 6){
            if(GalParams$rfe.src == "persiann.pdir-gb"){
                dirpath <- "PDIRNow/PDIRNow6hourly"
                deppath <- 0
                dataname <- "PDIR-Now"
                dateformat <- "%y%m%d%H"
                fileformat <- "pdirnow6h%s%s%s%s.bin.gz"
                filename <- sprintf(fileformat, yy_frmt, rdate[, 2], rdate[, 3], rdate[, 4])
                urls <- file.path(ftp.uci, dirpath, filename)
                ncfiles <- paste0("pdir-now_", rdate[, 1], rdate[, 2], rdate[, 3], rdate[, 4], ".nc")
                pars <- list(factor = 1, what = numeric(), bytes = 4, endian = "little")
            }else if(GalParams$rfe.src == "persiann.ccs-gb"){
                dirpath <- "PERSIANN-CCS/6hrly"
                deppath <- 0
                dataname <- "PERSIANN-CCS"
                dateformat <- "%y%j%H"
                fileformat <- "rgccs6h%s%s%s.bin.gz"
                filename <- sprintf(fileformat, yy_frmt, rdate[, 5], rdate[, 4])
                urls <- file.path(ftp.uci, dirpath, filename)
                ncfiles <- paste0("persiann-css_", rdate[, 1], rdate[, 2], rdate[, 3], rdate[, 4], ".nc")
                pars <- list(factor = 0.01, what = integer(), bytes = 2, endian = "big")
            }else if(GalParams$rfe.src == "persiann-gb"){
                dirpath <- "PERSIANN/6hrly"
                deppath <- 0
                dataname <- "PERSIANN"
                dateformat <- "%y%j%H"
                fileformat <- "m6s4_6h%s%s%s.bin.gz"
                filename <- sprintf(fileformat, yy_frmt, rdate[, 5], rdate[, 4])
                urls <- file.path(ftp.uci, dirpath, filename)
                ncfiles <- paste0("persiann_", rdate[, 1], rdate[, 2], rdate[, 3], rdate[, 4], ".nc")
                pars <- list(factor = 1, what = numeric(), bytes = 4, endian = "big")
            }else return(NULL)
        }else return(NULL)
        data.tres <- paste0(GalParams$minhour, GalParams$tstep)
    }else if(GalParams$tstep == "daily"){
        if(GalParams$rfe.src == "persiann.pdir-gb"){
            dirpath <- "PDIRNow/PDIRNowdaily"
            deppath <- 0
            dataname <- "PDIR-Now"
            dateformat <- "%y%m%d"
            fileformat <- "pdirnow1d%s%s%s.bin.gz"
            filename <- sprintf(fileformat, yy_frmt, rdate[, 2], rdate[, 3])
            urls <- file.path(ftp.uci, dirpath, filename)
            ncfiles <- paste0("pdir-now_", rdate[, 1], rdate[, 2], rdate[, 3], ".nc")
            pars <- list(factor = 1, what = numeric(), bytes = 4, endian = "little")
        }else if(GalParams$rfe.src == "persiann.ccs-gb"){
            dirpath <- "PERSIANN-CCS/daily"
            deppath <- 0
            dataname <- "PERSIANN-CCS"
            dateformat <- "%y%j"
            fileformat <- "rgccs1d%s%s.bin.gz"
            filename <- sprintf(fileformat, yy_frmt, rdate[, 4])
            urls <- file.path(ftp.uci, dirpath, filename)
            ncfiles <- paste0("persiann-css_", rdate[, 1], rdate[, 2], rdate[, 3], ".nc")
            pars <- list(factor = 1, what = numeric(), bytes = 4, endian = "big")
        }else if(GalParams$rfe.src == "persiann.cdr-gb"){
            dirpath <- "PERSIANN-CDR/daily"
            deppath <- 0
            dataname <- "PERSIANN-CDR"
            dateformat <- "%y%j"
            fileformat <- "aB1_d%s%s.bin.gz"
            filename <- sprintf(fileformat, yy_frmt, rdate[, 4])
            urls <- file.path(ftp.uci, dirpath, filename)
            ncfiles <- paste0("persiann-cdr_", rdate[, 1], rdate[, 2], rdate[, 3], ".nc")
            pars <- list(factor = 1, what = numeric(), bytes = 4, endian = "little")
        }else if(GalParams$rfe.src == "persiann-gb"){
            dirpath <- "PERSIANN/daily"
            deppath <- 0
            dataname <- "PERSIANN"
            dateformat <- "%y%j"
            fileformat <- "ms6s4_d%s%s.bin.gz"
            filename <- sprintf(fileformat, yy_frmt, rdate[, 4])
            urls <- file.path(ftp.uci, dirpath, filename)
            ncfiles <- paste0("persiann_", rdate[, 1], rdate[, 2], rdate[, 3], ".nc")
            pars <- list(factor = 1, what = numeric(), bytes = 4, endian = "big")
        }else return(NULL)
        data.tres <- GalParams$tstep
    }else if(GalParams$tstep == "monthly"){
        if(GalParams$rfe.src == "persiann.pdir-gb"){
            dirpath <- "PDIRNow/PDIRNowmonthly"
            deppath <- 0
            dataname <- "PDIR-Now"
            dateformat <- "%y%m"
            fileformat <- "pdirnow1mon%s%s.bin.gz"
            filename <- sprintf(fileformat, yy_frmt, rdate[, 2])
            urls <- file.path(ftp.uci, dirpath, filename)
            ncfiles <- paste0("pdir-now_", rdate[, 1], rdate[, 2], ".nc")
            pars <- list(factor = 1, what = numeric(), bytes = 4, endian = "little")
        }else if(GalParams$rfe.src == "persiann.ccs-gb"){
            dirpath <- "PERSIANN-CCS/mthly"
            deppath <- 0
            dataname <- "PERSIANN-CCS"
            dateformat <- "%y%m"
            fileformat <- "rgccs1m%s%s.bin.gz"
            filename <- sprintf(fileformat, yy_frmt, rdate[, 2])
            urls <- file.path(ftp.uci, dirpath, filename)
            ncfiles <- paste0("persiann-css_", rdate[, 1], rdate[, 2], ".nc")
            pars <- list(factor = 1, what = numeric(), bytes = 4, endian = "big")
        }else if(GalParams$rfe.src == "persiann.cdr-gb"){
            dirpath <- "PERSIANN-CDR/mthly"
            deppath <- 0
            dataname <- "PERSIANN-CDR"
            dateformat <- "%y%m"
            fileformat <- "aB1_m%s%s.bin.gz"
            filename <- sprintf(fileformat, yy_frmt, rdate[, 2])
            urls <- file.path(ftp.uci, dirpath, filename)
            ncfiles <- paste0("persiann-cdr_", rdate[, 1], rdate[, 2], ".nc")
            pars <- list(factor = 1, what = numeric(), bytes = 4, endian = "little")
        }else if(GalParams$rfe.src == "persiann-gb"){
            dirpath <- "PERSIANN/mthly"
            deppath <- 0
            dataname <- "PERSIANN"
            dateformat <- "%y%m"
            fileformat <- "ms6s4_m%s%s.bin.gz"
            filename <- sprintf(fileformat, yy_frmt, rdate[, 2])
            urls <- file.path(ftp.uci, dirpath, filename)
            ncfiles <- paste0("persiann_", rdate[, 1], rdate[, 2], ".nc")
            pars <- list(factor = 1, what = numeric(), bytes = 4, endian = "big")
        }else return(NULL)
        data.tres <- GalParams$tstep
    }else return(NULL)

    if(GalParams$rfe.src %in% c("persiann.pdir-gb", "persiann.ccs-gb")){
        coords <- list(lon0 = 0.02, lat0 = 59.98, nlon = 9000, nlat = 3000, res = 0.04)
    }else{
        coords <- list(lon0 = 0.125, lat0 = 59.875, nlon = 1440, nlat = 480, res = 0.25)
    }

    list(dirpath = dirpath, deppath = deppath, 
         fileformat = fileformat, dateformat = dateformat, 
         filename = filename, urls = urls, ncfiles = ncfiles,
         dataname = dataname, pars = c(pars, coords),
         data.tres = data.tres, baseurl = ftp.uci)
}

persiann.uci.table <- function(url){
    ret <- httr::GET(url)
    if(httr::status_code(ret) != 200){
        Insert.Messages.httr(ret)
        return(NULL)
    }
    ret <- httr::content(ret)

    tmp <- rvest::html_table(ret, fill = TRUE)[[1]]
    tmp <- as.data.frame(tmp[-(1:2), ])
    tmp <- tmp[tmp[, 2] != "", 2]
    tmp <- gsub(".bi..>", ".bin.gz", tmp)
    return(tmp)
}

persiann.uci.dates <- function(url, type, fileformat = NA, dateformat = NA){
    tmp <- persiann.uci.table(url)
    if(length(tmp) == 0) return(NULL)

    if(type == 'file'){
        ret <- extract_filename_dates(tmp, fileformat)
        if(is.null(ret)) return(NULL)
        ret <- gsub('[^[:digit:]]', '', ret)
    }else if(type == 'directory'){
        ret <- gsub('[^[:digit:]]', '', tmp)
    }else return(NULL)
    ret <- ret[ret != ""]
    ret <- ret[nchar(ret) >= 4]
    if(length(ret) == 0) return(NULL)
    if(type == 'file'){
        if(dateformat == "%y%m"){
            ret <- paste0(ret, '01')
            dateformat <- "%y%m%d"
        }
        times <- as.POSIXct(ret, format = dateformat, tz = 'UTC')
        times <- times[!is.na(times)]
        if(length(times) == 0) return(NULL)
        times <- sort(times)
    }else{
        times <- sort(ret)
    }
    return(times)
}

persiann.uci.end_dates <- function(tmp, fileformat, dateformat){
    ret <- extract_filename_dates(tmp, fileformat)
    if(is.null(ret)) return(NULL)
    ret <- ret[ret != ""]
    if(length(ret) == 0) return(NULL)
    if(dateformat == "%y%m"){
        ret <- paste0(ret, '01')
        dateformat <- "%y%m%d"
    }
    times <- as.POSIXct(ret, format = dateformat, tz = 'UTC')
    times <- times[!is.na(times)]
    if(length(times) == 0) return(NULL)
    times <- sort(times)

    return(times)
}

#################################################################################

persiann.download.data <- function(lnk, dest, ncfl, bbox, pars, GUI = TRUE){
    xx <- basename(dest)

    link.exist <- try(readLines(lnk, 1), silent = TRUE)
    if(inherits(link.exist, "try-error")){
        msg <- gsub('[\r\n]', '', link.exist)
        Insert.Messages.Out(msg, TRUE, "e", GUI)
        return(xx)
    }

    dc <- try(curl::curl_download(lnk, dest), silent = TRUE)
    if(!inherits(dc, "try-error")){
        tmpdir <- dirname(ncfl)
        tmpf <- file.path(tmpdir, gsub("\\.gz$", "", basename(dest)))
        R.utils::gunzip(dest, tmpf, remove = FALSE, overwrite = TRUE)
        ret <- persiann.extract.bin(tmpf, ncfl, bbox, pars)
        unlink(tmpf)

        if(ret == 0){
            xx <- NULL
        }else{
            unlink(dest)
        }
    }else{
        msg <- gsub('[\r\n]', '', dc[1])
        Insert.Messages.Out(msg, TRUE, "e", GUI)
    }

    return(xx)
}

persiann.extract.bin <- function(tmpf, ncfl, bbox, pars){
    con <- file(tmpf, open = "rb")
    on.exit(close(con))
    val <- try(readBin(con, pars$what, pars$nlon * pars$nlat,
                       pars$bytes, endian = pars$endian),
               silent = TRUE)
    ret <- 1
    if(!inherits(val, "try-error")){
        missval <- -9999

        lon <- seq(pars$lon0, length.out = pars$nlon, by = pars$res)
        lon <- ((lon + 180) %% 360) - 180
        lat <- seq(pars$lat0, length.out = pars$nlat, by = -pars$res)

        ox <- order(lon)
        oy <- order(lat)
        lon <- lon[ox]
        lat <- lat[oy]

        val[val == missval] <- NA
        val[is.infinite(val) | is.nan(val)] <- NA
        val <- matrix(val, pars$nlon, pars$nlat)
        val <- val[ox, oy]

        ix <- lon >= bbox$minlon & lon <= bbox$maxlon
        iy <- lat >= bbox$minlat & lat <= bbox$maxlat
        lon <- lon[ix]
        lat <- lat[iy]
        val <- val[ix, iy]
        val <- round(val * pars$factor, 1)

        val[is.na(val)] <- missval

        dx <- ncdf4::ncdim_def("lon", "degreeE", lon, longname = "Longitude")
        dy <- ncdf4::ncdim_def("lat", "degreeN", lat, longname = "Latitude")
        longname <- "Precipitation Estimation from Remotely Sensed Information using Artificial Neural Networks"
        ncgrd <- ncdf4::ncvar_def("precip", "mm", list(dx, dy), missval, longname, "float", compression = 9)

        nc <- ncdf4::nc_create(ncfl, ncgrd)
        ncdf4::ncvar_put(nc, ncgrd, val)
        ncdf4::nc_close(nc)

        ret <- 0
    }

    return(ret)
}

#################################################################################

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

persiann.download.data.ncei <- function(lnk, dest, ncfl, bbox, GUI = TRUE){
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
    }else{
        msg <- gsub('[\r\n]', '', dc[1])
        Insert.Messages.Out(msg, TRUE, "w", GUI)
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
