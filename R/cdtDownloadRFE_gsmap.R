
gsmap.download.jaxa <- function(GalParams, nbfile = 3, GUI = TRUE, verbose = TRUE){
    info <- gsmap.info.jaxa(GalParams)
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

    handle <- curl::new_handle()
    curl::handle_setopt(handle,
                        username = GalParams$login$usr,
                        password = GalParams$login$pwd)
    on.exit(curl::handle_reset(handle))

    ret <- cdt.download.data(info$urls, destfiles, ncfiles, nbfile, GUI,
                             verbose, data.name, gsmap.download.data,
                             bbox = GalParams$bbox, handle = handle,
                             pars = info$pars)

    return(ret)
}

gsmap.coverage.jaxa <- function(GalParams){
    if(GalParams$tstep %in% c('minute', 'hourly')){
        timestep <- paste(GalParams$minhour, GalParams$tstep)
    }else{
        timestep <- GalParams$tstep
    }
    out <- list(name = GalParams$rfe.src, timestep = timestep)

    info <- gsmap.info.jaxa(GalParams)
    if(is.null(info)) return(out)

    dataname <- strsplit(info$dataname, '_')[[1]]
    out$name <- paste0(dataname, collapse = ' ')

    url <- file.path(info$baseurl, info$dirpath, '')
    tmp <- gsmap.jaxa.dates(url, GalParams$login, 'directory')
    if(is.null(tmp)) return(out)
    end_d <- tmp[length(tmp)]
    url <- file.path(info$baseurl, info$dirpath, end_d, '')

    if(info$deppath == 1){
        end_d <- gsmap.jaxa.dates(url, GalParams$login, 'file', info$fileformat)
        if(is.null(end_d)) return(out)
        end_d <- end_d[length(end_d)]

        if(GalParams$tstep == "dekadal"){
            dek <- substr(end_d, 7, 8)
            end_d <- substr(end_d, 1, 6)
            dek <- which(c('01', '11', '21') == dek)
            end_d <- paste0(end_d, dek)
        }
    }else{
        end_d <- gsmap.jaxa.dates(url, GalParams$login, 'directory')
        if(is.null(end_d)) return(out)
        end_d <- end_d[length(end_d)]
        url <- paste0(url, end_d, '/')
        end_d <- gsmap.jaxa.dates(url, GalParams$login, 'directory')
        if(is.null(end_d)) return(out)
        end_d <- end_d[length(end_d)]
        url <- paste0(url, end_d, '/')
        end_d <- gsmap.jaxa.dates(url, GalParams$login, 'file', info$fileformat)
        if(is.null(end_d)) return(out)
        end_d <- end_d[length(end_d)]
    }

    out$end <- end_d
    out$start <- info$start_d

    return(out)
}

gsmap.info.jaxa <- function(GalParams){
    rdate <- table.format.date.time(GalParams$tstep, GalParams$date.range, GalParams$minhour)
    if(is.null(rdate)) return(NULL)

    if(GalParams$tstep == "minute"){
        if(GalParams$minhour == 30){
            if(GalParams$rfe.src == "gsmap.now-gb"){
                dataname <- 'GSMaP_NOW'
                dirpath <- 'now/half_hour'
                deppath <- 3
                dirformat <- "%s/%s/%s"
                fileformat <- 'gsmap_now.%s%s%s.%s%s.dat.gz'
                ncformat <- 'gsmap_now_%s%s%s%s%s.nc'
                start_d <- "201703290000"
            }else if(GalParams$rfe.src == "gsmap.g.now-gb"){
                dataname <- 'GSMaP_Gauge_NOW'
                dirpath <- 'now/half_hour_G'
                deppath <- 3
                dirformat <- "%s/%s/%s"
                fileformat <- 'gsmap_gauge_now.%s%s%s.%s%s.dat.gz'
                ncformat <- 'gsmap_gauge_now_%s%s%s%s%s.nc'
                start_d <- "201906270500"
            }else return(NULL)

            filename <- sprintf(fileformat, rdate[, 1], rdate[, 2], rdate[, 3], rdate[, 4], rdate[, 5])
            ncfiles <- sprintf(ncformat, rdate[, 1], rdate[, 2], rdate[, 3], rdate[, 4], rdate[, 5])
            dirpath1 <- sprintf(dirformat, rdate[, 1], rdate[, 2], rdate[, 3])
        }else return(NULL)
        data.tres <- paste0(GalParams$minhour, GalParams$tstep)
    }else if(GalParams$tstep == "hourly"){
        if(GalParams$minhour == 1){
            if(GalParams$rfe.src == "gsmap.nrtv8-gb"){
                dataname <- 'GSMaP_NRT_v8'
                dirpath <- 'realtime_ver/v8/archive'
                deppath <- 3
                dirformat <- "%s/%s/%s"
                fileformat <- 'gsmap_nrt.%s%s%s.%s00.dat.gz'
                ncformat <- 'gsmap_nrt.v8_%s%s%s%s.nc'
                start_d <- "2021120100"
            }else if(GalParams$rfe.src == "gsmap.g.nrtv8-gb"){
                dataname <- 'GSMaP_Gauge_NRT_v8'
                dirpath <- 'realtime_ver/v8/hourly_G'
                deppath <- 3
                dirformat <- "%s/%s/%s"
                fileformat <- 'gsmap_gauge.%s%s%s.%s00.dat.gz'
                ncformat <- 'gsmap_gauge_nrt.v8_%s%s%s%s.nc'
                start_d <- "2021120100"
            }else if(GalParams$rfe.src == "gsmap.nrtv7-gb"){
                dataname <- 'GSMaP_NRT_v7'
                dirpath <- 'realtime_ver/v7/archive'
                deppath <- 3
                dirformat <- "%s/%s/%s"
                fileformat <- 'gsmap_nrt.%s%s%s.%s00.dat.gz'
                ncformat <- 'gsmap_nrt.v7_%s%s%s%s.nc'
                start_d <- "2017040100"
            }else if(GalParams$rfe.src == "gsmap.g.nrtv7-gb"){
                dataname <- 'GSMaP_Gauge_NRT_v7'
                dirpath <- 'realtime_ver/v7/hourly_G'
                deppath <- 3
                dirformat <- "%s/%s/%s"
                fileformat <- 'gsmap_gauge.%s%s%s.%s00.dat.gz'
                ncformat <- 'gsmap_gauge_nrt.v7_%s%s%s%s.nc'
                start_d <- "2017040100"
            }else if(GalParams$rfe.src == "gsmap.nrtv6-gb"){
                dataname <- 'GSMaP_NRT_v6'
                dirpath <- 'realtime_ver/v6/archive'
                deppath <- 3
                dirformat <- "%s/%s/%s"
                fileformat <- 'gsmap_nrt.%s%s%s.%s00.dat.gz'
                ncformat <- 'gsmap_nrt.v6_%s%s%s%s.nc'
                start_d <- "2000030100"
            }else if(GalParams$rfe.src == "gsmap.g.nrtv6-gb"){
                dataname <- 'GSMaP_Gauge_NRT_v6'
                dirpath <- 'realtime_ver/v6/hourly_G'
                deppath <- 3
                dirformat <- "%s/%s/%s"
                fileformat <- 'gsmap_gauge.%s%s%s.%s00.dat.gz'
                ncformat <- 'gsmap_gauge_nrt.v6_%s%s%s%s.nc'
                start_d <- "2000040100"
            }else return(NULL)

            filename <- sprintf(fileformat, rdate[, 1], rdate[, 2], rdate[, 3], rdate[, 4])
            ncfiles <- sprintf(ncformat, rdate[, 1], rdate[, 2], rdate[, 3], rdate[, 4])
            dirpath1 <- sprintf(dirformat, rdate[, 1], rdate[, 2], rdate[, 3])
        }else return(NULL)
        data.tres <- paste0(GalParams$minhour, GalParams$tstep)
    }else if(GalParams$tstep == "daily"){
        if(GalParams$rfe.src == "gsmap.nrtv8.00-gb"){
            dataname <- 'GSMaP_NRT_v8_00Z-23Z'
            dirpath <- 'realtime_ver/v8/daily0.1/00Z-23Z'
            deppath <- 1
            dirformat <- "%s%s"
            fileformat <- 'gsmap_nrt.%s%s%s.0.1d.daily.00Z-23Z.dat.gz'
            ncformat <- 'gsmap_nrt.v8_00z_%s%s%s.nc'
            start_d <- "20211201"
        }else if(GalParams$rfe.src == "gsmap.nrtv8.12-gb"){
            dataname <- 'GSMaP_NRT_v8_12Z-11Z'
            dirpath <- 'realtime_ver/v8/daily0.1/p12Z-11Z'
            deppath <- 1
            dirformat <- "%s%s"
            fileformat <- 'gsmap_nrt.%s%s%s.0.1d.daily.p12Z-11Z.dat.gz'
            ncformat <- 'gsmap_nrt.v8_12z_%s%s%s.nc'
            start_d <- "20211202"
        }else if(GalParams$rfe.src == "gsmap.g.nrtv8.00-gb"){
            dataname <- 'GSMaP_Gauge_NRT_v8_00Z-23Z'
            dirpath <- 'realtime_ver/v8/daily0.1_G/00Z-23Z'
            deppath <- 1
            dirformat <- "%s%s"
            fileformat <- 'gsmap_gauge.%s%s%s.0.1d.daily.00Z-23Z.dat.gz'
            ncformat <- 'gsmap_gauge_nrt.v8_00z_%s%s%s.nc'
            start_d <- "20211201"
        }else if(GalParams$rfe.src == "gsmap.g.nrtv8.12-gb"){
            dataname <- 'GSMaP_Gauge_NRT_v8_12Z-11Z'
            dirpath <- 'realtime_ver/v8/daily0.1_G/p12Z-11Z'
            deppath <- 1
            dirformat <- "%s%s"
            fileformat <- 'gsmap_gauge.%s%s%s.0.1d.daily.p12Z-11Z.dat.gz'
            ncformat <- 'gsmap_gauge_nrt.v8_12z_%s%s%s.nc'
            start_d <- "20211202"
        }else if(GalParams$rfe.src == "gsmap.nrtv7.00-gb"){
            dataname <- 'GSMaP_NRT_v7_00Z-23Z'
            dirpath <- 'realtime_ver/v7/daily0.1/00Z-23Z'
            deppath <- 1
            dirformat <- "%s%s"
            fileformat <- 'gsmap_nrt.%s%s%s.0.1d.daily.00Z-23Z.dat.gz'
            ncformat <- 'gsmap_nrt.v7_00z_%s%s%s.nc'
            start_d <- "20170401"
        }else if(GalParams$rfe.src == "gsmap.nrtv7.12-gb"){
            dataname <- 'GSMaP_NRT_v7_12Z-11Z'
            dirpath <- 'realtime_ver/v7/daily0.1/p12Z-11Z'
            deppath <- 1
            dirformat <- "%s%s"
            fileformat <- 'gsmap_nrt.%s%s%s.0.1d.daily.p12Z-11Z.dat.gz'
            ncformat <- 'gsmap_nrt.v7_12z_%s%s%s.nc'
            start_d <- "20170401"
        }else if(GalParams$rfe.src == "gsmap.g.nrtv7.00-gb"){
            dataname <- 'GSMaP_Gauge_NRT_v7_00Z-23Z'
            dirpath <- 'realtime_ver/v7/daily0.1_G/00Z-23Z'
            deppath <- 1
            dirformat <- "%s%s"
            fileformat <- 'gsmap_gauge.%s%s%s.0.1d.daily.00Z-23Z.dat.gz'
            ncformat <- 'gsmap_gauge_nrt.v7_00z_%s%s%s.nc'
            start_d <- "20170401"
        }else if(GalParams$rfe.src == "gsmap.g.nrtv7.12-gb"){
            dataname <- 'GSMaP_Gauge_NRT_v7_12Z-11Z'
            dirpath <- 'realtime_ver/v7/daily0.1_G/p12Z-11Z'
            deppath <- 1
            dirformat <- "%s%s"
            fileformat <- 'gsmap_gauge.%s%s%s.0.1d.daily.p12Z-11Z.dat.gz'
            ncformat <- 'gsmap_gauge_nrt.v7_12z_%s%s%s.nc'
            start_d <- "20170401"
        }else if(GalParams$rfe.src == "gsmap.nrtv6.00-gb"){
            dataname <- 'GSMaP_NRT_v6_00Z-23Z'
            dirpath <- 'realtime_ver/v6/daily0.1/00Z-23Z'
            deppath <- 1
            dirformat <- "%s%s"
            fileformat <- 'gsmap_nrt.%s%s%s.0.1d.daily.00Z-23Z.dat.gz'
            ncformat <- 'gsmap_nrt.v6_00z_%s%s%s.nc'
            start_d <- "20000301"
        }else if(GalParams$rfe.src == "gsmap.nrtv6.12-gb"){
            dataname <- 'GSMaP_NRT_v6_12Z-11Z'
            dirpath <- 'realtime_ver/v6/daily0.1/p12Z-11Z'
            deppath <- 1
            dirformat <- "%s%s"
            fileformat <- 'gsmap_nrt.%s%s%s.0.1d.daily.p12Z-11Z.dat.gz'
            ncformat <- 'gsmap_nrt.v6_12z_%s%s%s.nc'
            start_d <- "20000302"
        }else if(GalParams$rfe.src == "gsmap.g.nrtv6.00-gb"){
            dataname <- 'GSMaP_Gauge_NRT_v6_00Z-23Z'
            dirpath <- 'realtime_ver/v6/daily0.1_G/00Z-23Z'
            deppath <- 1
            dirformat <- "%s%s"
            fileformat <- 'gsmap_gauge.%s%s%s.0.1d.daily.00Z-23Z.dat.gz'
            ncformat <- 'gsmap_gauge_nrt.v6_00z_%s%s%s.nc'
            start_d <- "20000401"
        }else if(GalParams$rfe.src == "gsmap.g.nrtv6.12-gb"){
            dataname <- 'GSMaP_Gauge_NRT_v6_12Z-11Z'
            dirpath <- 'realtime_ver/v6/daily0.1_G/p12Z-11Z'
            deppath <- 1
            dirformat <- "%s%s"
            fileformat <- 'gsmap_gauge.%s%s%s.0.1d.daily.p12Z-11Z.dat.gz'
            ncformat <- 'gsmap_gauge_nrt.v6_12z_%s%s%s.nc'
            start_d <- "20000401"
        }else if(GalParams$rfe.src == "gsmap.clm-gb"){
            dataname <- 'GSMaP_CLM'
            dirpath <- 'climate/gnrt6/daily'
            deppath <- 1
            dirformat <- "%s%s"
            fileformat <- 'gsmap_gnrt6.%s%s%s.0.1d.daily.00Z-23Z.dat.gz'
            ncformat <- 'gsmap_clm_00z_%s%s%s.nc'
            start_d <- "20000401"
        }else return(NULL)

        filename <- sprintf(fileformat, rdate[, 1], rdate[, 2], rdate[, 3])
        ncfiles <- sprintf(ncformat, rdate[, 1], rdate[, 2], rdate[, 3])
        dirpath1 <- sprintf(dirformat, rdate[, 1], rdate[, 2])
        data.tres <- GalParams$tstep
    }else if(GalParams$tstep == "dekadal"){
        if(GalParams$rfe.src == "gsmap.clm-gb"){
            dataname <- 'GSMaP_CLM'
            dirpath <- 'climate/gnrt6/10days'
            deppath <- 1
            dirformat <- "%s"
            fileformat <- 'gsmap_gnrt6.S%s_E%s.0.1d.10days.dat.gz'
            ncformat <- 'gsmap_clm_%s%s%s.nc'
            start_d <- "2000041"
        }else return(NULL)

        dek1 <- c('01', 11, 21)[as.numeric(rdate[, 3])]
        daty <- paste0(rdate[, 1], rdate[, 2], rdate[, 3])
        dek2 <- nb.Day.Of.Month(daty)
        dek2[rdate[, 3] == "1"] <- 10
        dek2[rdate[, 3] == "2"] <- 20
        dek1 <- paste0(rdate[, 1], rdate[, 2], dek1)
        dek2 <- paste0(rdate[, 1], rdate[, 2], dek2)

        filename <- sprintf(fileformat, dek1, dek2)
        ncfiles <- sprintf(ncformat, rdate[, 1], rdate[, 2], rdate[, 3])
        dirpath1 <- sprintf(dirformat, rdate[, 1])
        data.tres <- GalParams$tstep
    }else if(GalParams$tstep == "monthly"){
        if(GalParams$rfe.src == "gsmap.nrtv8-gb"){
            dataname <- 'GSMaP_NRT_v8'
            dirpath <- 'realtime_ver/v8/monthly'
            deppath <- 1
            dirformat <- "%s"
            fileformat <- 'gsmap_nrt.%s%s.0.1d.monthly.dat.gz'
            ncformat <- 'gsmap_nrt.v8_%s%s.nc'
            start_d <- "202112"
        }else if(GalParams$rfe.src == "gsmap.g.nrtv8-gb"){
            dataname <- 'GSMaP_Gauge_NRT_v8'
            dirpath <- 'realtime_ver/v8/monthly_G'
            deppath <- 1
            dirformat <- "%s"
            fileformat <- 'gsmap_gauge.%s%s.0.1d.monthly.dat.gz'
            ncformat <- 'gsmap_gauge_nrt.v8_%s%s.nc'
            start_d <- "202112"
        }else if(GalParams$rfe.src == "gsmap.nrtv7-gb"){
            dataname <- 'GSMaP_NRT_v7'
            dirpath <- 'realtime_ver/v7/monthly'
            deppath <- 1
            dirformat <- "%s"
            fileformat <- 'gsmap_nrt.%s%s.0.1d.monthly.dat.gz'
            ncformat <- 'gsmap_nrt.v7_%s%s.nc'
            start_d <- "201704"
        }else if(GalParams$rfe.src == "gsmap.g.nrtv7-gb"){
            dataname <- 'GSMaP_Gauge_NRT_v7'
            dirpath <- 'realtime_ver/v7/monthly_G'
            deppath <- 1
            dirformat <- "%s"
            fileformat <- 'gsmap_gauge.%s%s.0.1d.monthly.dat.gz'
            ncformat <- 'gsmap_gauge_nrt.v7_%s%s.nc'
            start_d <- "201704"
        }else if(GalParams$rfe.src == "gsmap.nrtv6-gb"){
            dataname <- 'GSMaP_NRT_v6'
            dirpath <- 'realtime_ver/v6/monthly'
            deppath <- 1
            dirformat <- "%s"
            fileformat <- 'gsmap_nrt.%s%s.0.1d.monthly.dat.gz'
            ncformat <- 'gsmap_nrt.v6_%s%s.nc'
            start_d <- "200003"
        }else if(GalParams$rfe.src == "gsmap.g.nrtv6-gb"){
            dataname <- 'GSMaP_Gauge_NRT_v6'
            dirpath <- 'realtime_ver/v6/monthly_G'
            deppath <- 1
            dirformat <- "%s"
            fileformat <- 'gsmap_gauge.%s%s.0.1d.monthly.dat.gz'
            ncformat <- 'gsmap_gauge_nrt.v6_%s%s.nc'
            start_d <- "200004"
        }else if(GalParams$rfe.src == "gsmap.clm-gb"){
            dataname <- 'GSMaP_CLM'
            dirpath <- 'climate/gnrt6/monthly'
            deppath <- 1
            dirformat <- "%s"
            fileformat <- 'gsmap_gnrt6.%s%s.0.1d.monthly.dat.gz'
            ncformat <- 'gsmap_clm_%s%s.nc'
            start_d <- "200004"
        }else return(NULL)

        filename <- sprintf(fileformat, rdate[, 1], rdate[, 2])
        ncfiles <- sprintf(ncformat, rdate[, 1], rdate[, 2])
        dirpath1 <- sprintf(dirformat, rdate[, 1])
        data.tres <- GalParams$tstep
    }else return(NULL)

    ftp.dir <- file.path(dirpath, dirpath1)
    ftp.jaxa <- "ftp://hokusai.eorc.jaxa.jp"
    urls <- file.path(ftp.jaxa, ftp.dir, filename)
    pars <- list(tstep = GalParams$tstep)

    list(dirpath = dirpath, deppath = deppath, fileformat = fileformat,
         filename = filename, dataname = dataname, ncfiles = ncfiles,
         urls = urls, data.tres = data.tres, baseurl = ftp.jaxa,
         start_d = start_d, pars = pars)
}

gsmap.jaxa.table <- function(url, login){
    handle <- curl::new_handle()
    curl::handle_setopt(handle, username = login$usr, password = login$pwd,
                        ftp_use_epsv = TRUE, dirlistonly = TRUE)
    conn <- try(curl::curl(url = url, open = "r", handle = handle), silent = TRUE)
    on.exit({
        curl::handle_reset(handle)
        close(conn)
    })

    if(inherits(conn, "try-error")){
        Insert.Messages.Out(conn[1], TRUE, "e", TRUE)
        return(NULL)
    }

    tmp <- readLines(conn)
    return(tmp)
}

gsmap.jaxa.dates <- function(url, login, type, fileformat = NA){
    tmp <- gsmap.jaxa.table(url, login)
    if(length(tmp) == 0) return(NULL)

    if(type == 'file'){
        ret <- extract_filename_dates(tmp, fileformat)
        if(is.null(ret)) return(NULL)
    }else if(type == 'directory'){
        ret <- trimws(tmp)
        ret <- gsub('[^[:digit:]]', '', ret)
    }else return(NULL)

    ret <- ret[ret != ""]
    if(length(ret) == 0) return(NULL)
    ret <- sort(ret)

    return(ret)
}

#################################################################################

gsmap.download.data <- function(lnk, dest, ncfl, bbox,
                                handle, pars, GUI = TRUE)
{
    xx <- basename(dest)

    dc <- try(curl::curl_download(lnk, dest, handle = handle), silent = TRUE)
    if(!inherits(dc, "try-error")){
        tmpdir <- dirname(ncfl)
        tmpf <- file.path(tmpdir, gsub("\\.gz$", "", basename(dest)))
        R.utils::gunzip(dest, tmpf, remove = FALSE, overwrite = TRUE)
        ret <- gsmap.extract.bin(tmpf, ncfl, bbox, pars)
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

gsmap.extract.bin <- function(tmpf, ncfl, bbox, pars){
    con <- file(tmpf, open = "rb")
    on.exit(close(con))
    nx <- 3600
    ny <- 1200
    nl <- nx * ny

    if(pars$tstep == 'monthly'){
        val <- try(readBin(con, numeric(), 2 * nl, 4, endian = "little"), silent = TRUE)
        if(inherits(val, "try-error")) return(1)
        val1 <- val[1:nl]
        val2 <- val[(nl + 1):(2 * nl)]
        val1[val1 < 0] <- NA
        val <- val1 * val2
    }else{
        val <- try(readBin(con, numeric(), nl, 4, endian = "little"), silent = TRUE)
        if(inherits(val, "try-error")) return(1)
        val[val < 0] <- NA
    }

    lon <- seq(0.05, length.out = nx, by = 0.1)
    lon <- ((lon + 180) %% 360) - 180
    lat <- seq(59.95, length.out = ny, by = -0.1)

    ox <- order(lon)
    oy <- order(lat)
    lon <- lon[ox]
    lat <- lat[oy]

    val[is.infinite(val) | is.nan(val)] <- NA
    val <- matrix(val, nx, ny)
    val <- val[ox, oy]

    ix <- lon >= bbox$minlon & lon <= bbox$maxlon
    iy <- lat >= bbox$minlat & lat <= bbox$maxlat
    lon <- lon[ix]
    lat <- lat[iy]
    val <- val[ix, iy]

    if(pars$tstep == 'minute'){
        val <- val * 0.5
    }
    if(pars$tstep == 'daily'){
        val <- val * 24
    }
    if(pars$tstep == 'dekadal'){
        daty <- gsub(".*_|\\.nc$", "", basename(ncfl))
        ndek <- nb.Day.Of.Dekad(daty)
        val <- val * 24 * ndek
    }
    val <- round(val, 2)
    val[is.na(val)] <- -99

    dx <- ncdf4::ncdim_def("lon", "degrees_east", lon, longname = "Longitude")
    dy <- ncdf4::ncdim_def("lat", "degrees_north", lat, longname = "Latitude")
    ncgrd <- ncdf4::ncvar_def("precip", "mm", list(dx, dy), -99,
                              "Precipitation estimates", "float", compression = 9)

    nc <- ncdf4::nc_create(ncfl, ncgrd)
    ncdf4::ncvar_put(nc, ncgrd, val)
    ncdf4::nc_close(nc)

    return(0)
}
