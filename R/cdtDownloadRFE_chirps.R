
chirps.download.iridl <- function(GalParams, nbfile = 3, GUI = TRUE, verbose = TRUE){
    info <- chirps.info.iridl(GalParams)
    if(is.null(info)) return(-3)

    if(GalParams$rfe.src == "chirpsv2-gb"){
        dir_name <- "CHIRPSv2"
        file_name <- "chirps"
    }else{
        dir_name <- "CHIRP"
        file_name <- "chirp"
    }

    data.name <- paste0(dir_name, "_", GalParams$tstep)
    outdir <- file.path(GalParams$dir2save, data.name)
    if(!dir.exists(outdir))
        dir.create(outdir, showWarnings = FALSE, recursive = TRUE)
    destfiles <- paste0(file_name, "_", info$dates, ".nc")
    destfiles <- file.path(outdir, destfiles)

    urls <- paste0(info$baseurl, '/', info$pars)

    ret <- cdt.download.data(urls, destfiles, destfiles, nbfile, GUI,
                             verbose, data.name, iridl.download.data)

    return(ret)
}

chirps.coverage.iridl <- function(GalParams){
    info <- chirps.info.iridl(GalParams)
    if(is.null(info)) return(-3)

    if(GalParams$rfe.src == "chirpsv2-gb"){
        name <- "CHIRPS version 2.0"
    }else{
        name <- "CHIRP version 1.0"
    }

    end_date <- iridl.get.end_date(info$baseurl, GalParams$tstep)
    start_date <- switch(GalParams$tstep,
                         "daily" = "19810101",
                         "dekadal" = "1981011",
                         "monthly" = "198101"
                        )
    list(name = name, timestep = GalParams$tstep,
         start = start_date, end = end_date)
}

chirps.info.iridl <- function(GalParams){
    iridl_ucsb <- "https://iridl.ldeo.columbia.edu/SOURCES/.UCSB"
    if(GalParams$rfe.src == "chirpsv2-gb"){
        dlpath <- paste0(iridl_ucsb, "/.CHIRPS/.v2p0")
        vardir <- switch(GalParams$tstep,
                          "daily" = ".daily-improved/.global/.0p05/.prcp",
                          "dekadal" = ".dekad/.prcp",
                          "monthly" = ".monthly/.global/.precipitation"
                        )
    }else if(GalParams$rfe.src == "chirp-gb"){
        dlpath <- paste0(iridl_ucsb, "/.CHIRP/.v1p0")
        vardir <- switch(GalParams$tstep,
                          "daily" = ".daily/.prcp",
                          "dekadal" = ".dekad/.prcp",
                          "monthly" = ".dekad/.prcp/monthlyAverage/3./mul"
                         )
    }else return(NULL)

    url <- file.path(dlpath, vardir)

    rlon <- unlist(GalParams$bbox[c('minlon', 'maxlon')])
    rlon <- paste(c('X', rlon, 'RANGE'), collapse = "/")
    rlat <- unlist(GalParams$bbox[c('minlat', 'maxlat')])
    rlat <- paste(c('Y', rlat, 'RANGE'), collapse = "/")

    rdate <- iridl.format.date(GalParams$tstep, GalParams$date.range)
    if(is.null(rdate)) return(NULL)
    rtime <- urltools::url_encode(paste0("(", rdate$dates, ")"))
    rtime <- paste0("T", "/", rtime, "/", "VALUE")

    pars <- paste(rlon, rlat, rtime, 'data.nc', sep = "/")
    info <- list(baseurl = url, pars = pars, dates = rdate$out)

    return(info)
}

chirps.download.ucsb <- function(GalParams, nbfile = 3, GUI = TRUE, verbose = TRUE){
    info <- chirps.info.ucsb(GalParams)
    if(is.null(info)) return(-3)

    if(GalParams$rfe.src == "chirpsv2-gb"){
        data.name <- paste0("CHIRPSv2_", GalParams$tstep)
    }else if(GalParams$rfe.src == "chirp-gb"){
        data.name <- paste0("CHIRP_", GalParams$tstep)
    }else if(GalParams$rfe.src == "chirpsv2-af"){
        data.name <- paste0("CHIRPSv2_", GalParams$minhour, GalParams$tstep)
    }else return(-1)

    outdir <- file.path(GalParams$dir2save, data.name)
    extrdir <- file.path(outdir, "Extracted")
    if(!dir.exists(extrdir))
        dir.create(extrdir, showWarnings = FALSE, recursive = TRUE)
    origdir <- file.path(outdir, paste0("Data_", info$datadir))
    if(!dir.exists(origdir))
        dir.create(origdir, showWarnings = FALSE, recursive = TRUE)
    destfiles <- file.path(origdir, info$filename)
    ncfiles <- file.path(extrdir, info$ncfiles)

    ret <- cdt.download.data(info$urls, destfiles, ncfiles, nbfile, GUI,
                             verbose, data.name, chirps.download.data,
                             bbox = GalParams$bbox, type = info$type)

    return(ret)
}

chirps.coverage.ucsb <- function(GalParams){
    if(GalParams$tstep == 'hourly'){
        timestep <- paste(GalParams$minhour, GalParams$tstep)
    }else{
        timestep <- GalParams$tstep
    }
    out <- list(name = GalParams$rfe.src, timestep = timestep)

    info <- chirps.info.ucsb(GalParams)
    if(is.null(info)) return(out)
    out$name <- info$dataname

    sysT <- Sys.time()
    this_year <- as.numeric(format(sysT, '%Y'))

    if(info$deppath == 0){
        url <- file.path(info$baseurl, info$dirpath)
        tmp <- chirps.ucsb.dates(url, 'file', info$fileformat)
        if(is.null(tmp)) return(out)

        yy <- as.numeric(substr(tmp, 1, 4))
        iy <- yy >= 1981 & yy <= this_year
        tmp <- tmp[iy]
        out$end <- tmp[length(tmp)]
    }else if(info$deppath == 1){
        url <- file.path(info$baseurl, info$dirpath)
        tmp <- chirps.ucsb.dates(url, 'directory')
        if(is.null(tmp)) return(out)

        end_d <- tmp[length(tmp)]
        if(info$type == 'bin'){
            # if(GalParams$rfe.src == "chirpsv2-af") 6 hourly data
            end_mon <- as.Date(paste0(end_d, '01'), '%Y%m%d')
            this_mon <- as.Date(paste0(format(sysT, '%Y-%m-'), '01'))
            if(end_mon > this_mon) end_mon <- this_mon
            start_mon <- as.Date(paste0(format(end_mon - 60, '%Y-%m-'), '01'))
            seq_dates <- seq(start_mon, end_mon, 'month')
            seq_dates <- format(seq_dates, '%Y%m')
        }else{
            end_year <- as.numeric(end_d)
            if(end_year > this_year) end_year <- this_year
            seq_dates <- seq(end_year - 2, end_year, 1)
        }
        seq_dates <- rev(seq_dates)

        for(j in seq_along(seq_dates)){
            url <- file.path(info$baseurl, info$dirpath, seq_dates[j])
            ret <- chirps.ucsb.table(url)
            if(is.null(ret)) return(out)
            if(length(ret) > 0) break
        }

        end_d <- chirps.ucsb.end_dates(ret, info$fileformat)
        if(is.null(end_d)) return(out)

        yy <- as.numeric(substr(end_d, 1, 4))
        iy <- yy >= 1981 & yy <= this_year
        end_d <- end_d[iy]
        out$end <- end_d[length(end_d)]
    }else return(out)

    if(info$type == 'bil'){
        if(GalParams$tstep %in% c("pentad", "dekadal")){
            year <- substr(out$end, 1, 4)
            ip <- as.integer(substr(out$end, 5, 6))
            np <- switch(GalParams$tstep, "pentad" = 6, "dekadal" = 3)
            mat_d <- cbind(sprintf('%02d', rep(1:12, each = np)),
                           sprintf('%02d', rep(1:np, 12)))
            mon <- mat_d[ip, 1]
            pdk <- as.numeric(mat_d[ip, 2])
            end_d <- paste0(year, mon, pdk)
        }else{
            end_d <- out$end
        }
        out$end <- end_d
    }

    out$start <- switch(GalParams$tstep,
                        "hourly" = "1981010100",
                        "daily" = "19810101",
                        "pentad" = "1981011",
                        "dekadal" = "1981011",
                        "monthly" = "198101"
                       )
    return(out)
}

chirps.info.ucsb <- function(GalParams){
    rdate <- table.format.date.time(GalParams$tstep, GalParams$date.range, GalParams$minhour)
    if(is.null(rdate)) return(NULL)

    bbx <- GalParams$bbox
    africa <- (bbx$minlon >= -19.975 && bbx$maxlon <= 54.975) &&
              (bbx$minlat >= -39.975 && bbx$maxlat <= 39.975)
    camer_carib <- (bbx$minlon >= -92.975 && bbx$maxlon <= -57.025) &&
                   (bbx$minlat >= 6.025 && bbx$maxlat <= 23.475)
    india <- (bbx$minlon >= 65.025 && bbx$maxlon <= 99.975) &&
             (bbx$minlat >= 5.025 && bbx$maxlat <= 39.975)

    global <- GalParams$chirps.global
    ftp.ucsb <- "https://data.chc.ucsb.edu/products"

    if(GalParams$tstep == "daily"){
        if(GalParams$rfe.src == "chirpsv2-gb"){
            if(africa && !global){
                data.dir <- "africa"
                ## 800K
                # dirpath <- "CHIRPS-2.0/africa_daily/tifs/p05"
                # fileformat <- "chirps-v2.0.%s.%s.%s.tif.gz"
                # filename <- sprintf(fileformat, rdate[, 1], rdate[, 2], rdate[, 3])
                # type <- "tif"
                ## 180K
                dirpath <- "CHIRPS-2.0/africa_daily/bils/p05"
                fileformat <- "v2p0chirps%s%s%s.tar.gz"
                filename <- sprintf(fileformat, rdate[, 1], rdate[, 2], rdate[, 3])
                type <- "bil"
            }else{
                data.dir <- "global"
                dirpath <- "CHIRPS-2.0/global_daily/tifs/p05"
                fileformat <- "chirps-v2.0.%s.%s.%s.tif.gz"
                filename <- sprintf(fileformat, rdate[, 1], rdate[, 2], rdate[, 3])
                type <- "tif"
            }
            ncformat <- "chirps_%s%s%s.nc"
            dataname <- 'CHIRPS version 2.0'
        }else if(GalParams$rfe.src == "chirp-gb"){
            data.dir <- "global"
            dirpath <- "CHIRP/daily"
            fileformat <- "chirp.%s.%s.%s.tif.gz"
            filename <- sprintf(fileformat, rdate[, 1], rdate[, 2], rdate[, 3])
            type <- "tif"
            ncformat <- "chirp_%s%s%s.nc"
            dataname <- 'CHIRP version 1.0'
        }else return(NULL)

        urls <- file.path(ftp.ucsb, dirpath, rdate[, 1], filename)
        ncfiles <- sprintf(ncformat, rdate[, 1], rdate[, 2], rdate[, 3])
        deppath <- 1
    }
    else if(GalParams$tstep == "pentad"){
        if(GalParams$rfe.src == "chirpsv2-gb"){
            if(africa && !global){
                data.dir <- "africa"
                ## 400K
                dirpath <- "CHIRPS-2.0/africa_pentad/bils"
                fileformat <- "v2p0chirps%s%s.tar.gz"
                filename <- sprintf(fileformat, rdate[, 1], rdate[, 4])
                type <- "bil"
                ## 4M
                # dirpath <- "CHIRPS-2.0/africa_pentad/tifs"
                # fileformat <- "chirps-v2.0.%s.%s.%s.tif.gz"
                # filename <- sprintf(fileformat, rdate[, 1], rdate[, 2], , rdate[, 3])
                # type <- "tif"
            }else if(camer_carib && !global){
                data.dir <- "camer-carib"
                ## 40k
                dirpath <- "CHIRPS-2.0/camer-carib_pentad/bils"
                fileformat <- "v2p0chirps%s%s.tar.gz"
                filename <- sprintf(fileformat, rdate[, 1], rdate[, 4])
                type <- "bil"
                ## 260k
                # dirpath <- "CHIRPS-2.0/camer-carib_pentad/tifs"
                # fileformat <- "chirps-v2.0.%s.%s.%s.tif.gz"
                # filename <- sprintf(fileformat, rdate[, 1], rdate[, 2], , rdate[, 3])
                # type <- "tif"
            }else{
                data.dir <- "global"
                ## 1.8M
                dirpath <- "CHIRPS-2.0/global_pentad/bils"
                fileformat <- "v2p0chirps%s%s.tar.gz"
                filename <- sprintf(fileformat, rdate[, 1], rdate[, 4])
                type <- "bil"
                ## 14M
                # dirpath <- "CHIRPS-2.0/global_pentad/tifs"
                # fileformat <- "chirps-v2.0.%s.%s.%s.tif.gz"
                # filename <- sprintf(fileformat, rdate[, 1], rdate[, 2], , rdate[, 3])
                # type <- "tif"
            }
            ncformat <- "chirps_%s%s%s.nc"
            dataname <- 'CHIRPS version 2.0'
        }else if(GalParams$rfe.src == "chirp-gb"){
            if(africa && !global){
                data.dir <- "africa"
                ## 9.2M
                # dirpath <- "CHIRP/pentads/africa"
                # fileformat <- "chirp.%s.%s.%s.tif"
                # filename <- sprintf(fileformat, rdate[, 1], rdate[, 2], rdate[, 3])
                # type <- "tif"
                ## 700K
                dirpath <- "CHIRP/bils/pentads/africa"
                fileformat <- "chirp%s%s.tar.gz"
                filename <- sprintf(fileformat, rdate[, 1], rdate[, 4])
                type <- "bil"
            }else if(camer_carib && !global){
                data.dir <- "camer-carib"
                ## 1M
                # dirpath <- "CHIRP/pentads/camer-carib"
                # fileformat <- "chirp.%s.%s.%s.tif"
                # filename <- sprintf(fileformat, rdate[, 1], rdate[, 2], rdate[, 3])
                # type <- "tif"
                ## 100K
                dirpath <- "CHIRP/bils/pentads/camer-carib"
                fileformat <- "chirp%s%s.tar.gz"
                filename <- sprintf(fileformat, rdate[, 1], rdate[, 4])
                type <- "bil"
            }else if(india && !global){
                data.dir <- "india"
                ## 1.9M
                dirpath <- "CHIRP/pentads/india"
                fileformat <- "chirp.%s.%s.%s.tif"
                filename <- sprintf(fileformat, rdate[, 1], rdate[, 2], rdate[, 3])
                type <- "tif"
            }else{
                data.dir <- "global"
                ## 55M
                dirpath <- "CHIRP/pentads"
                fileformat <- "CHIRP.%s.%s.%s.tif"
                filename <- sprintf(fileformat, rdate[, 1], rdate[, 2], rdate[, 3])
                type <- "tif"
            }
            ncformat <- "chirp_%s%s%s.nc"
            dataname <- 'CHIRP version 1.0'
        }else return(NULL)

        urls <- file.path(ftp.ucsb, dirpath, filename)
        ncfiles <- sprintf(ncformat, rdate[, 1], rdate[, 2], rdate[, 3])
        deppath <- 0
    }
    else if(GalParams$tstep == "dekadal"){
        if(GalParams$rfe.src == "chirpsv2-gb"){
            if(africa && !global){
                data.dir <- "africa"
                ## 500K
                dirpath <- "CHIRPS-2.0/africa_dekad/bils"
                fileformat <- "v2p0chirps%s%s.tar.gz"
                filename <- sprintf(fileformat, rdate[, 1], rdate[, 4])
                type <- "bil"
                ## 4M
                # dirpath <- "CHIRPS-2.0/africa_dekad/tifs"
                # fileformat <- "chirps-v2.0.%s.%s.%s.tif.gz"
                # filename <- sprintf(fileformat, rdate[, 1], rdate[, 2], rdate[, 3])
                # type <- "tif"
            }else if(camer_carib && !global){
                data.dir <- "camer-carib"
                ## 80k
                dirpath <- "CHIRPS-2.0/camer-carib_dekad/bils"
                fileformat <- "v2p0chirps%s%s.tar.gz"
                filename <- sprintf(fileformat, rdate[, 1], rdate[, 4])
                type <- "bil"
                ## 250K
                # dirpath <- "CHIRPS-2.0/camer-carib_dekad/tifs"
                # fileformat <- "chirps-v2.0.%s.%s.%s.tif.gz"
                # filename <- sprintf(fileformat, rdate[, 1], rdate[, 2], rdate[, 3])
                # type <- "tif"
            }else{
                data.dir <- "global"
                ## 2.3M
                dirpath <- "CHIRPS-2.0/global_dekad/bils"
                fileformat <- "v2p0chirps%s%s.tar.gz"
                filename <- sprintf(fileformat, rdate[, 1], rdate[, 4])
                type <- "bil"
                ## 14M
                # dirpath <- "CHIRPS-2.0/global_dekad/tifs"
                # fileformat <- "chirps-v2.0.%s.%s.%s.tif.gz"
                # filename <- sprintf(fileformat, rdate[, 1], rdate[, 2], rdate[, 3])
                # type <- "tif"
            }
            ncformat <- "chirps_%s%s%s.nc"
            dataname <- 'CHIRPS version 2.0'
        }else if(GalParams$rfe.src == "chirp-gb"){
            if(africa && !global){
                data.dir <- "africa"
                ## 9.2M
                # dirpath <- "CHIRP/dekads/africa"
                # fileformat <- "chirp.%s.%s.%s.tif"
                # filename <- sprintf(fileformat, rdate[, 1], rdate[, 2], rdate[, 3])
                # type <- "tif"
                ## 1M
                dirpath <- "CHIRP/bils/dekads/africa"
                fileformat <- "chirp%s%s.tar.gz"
                filename <- sprintf(fileformat, rdate[, 1], rdate[, 4])
                type <- "bil"
            }else if(camer_carib && !global){
                data.dir <- "camer-carib"
                ## 1M
                dirpath <- "CHIRP/dekads/camer-carib"
                fileformat <- "chirp.%s.%s.%s.tif"
                filename <- sprintf(fileformat, rdate[, 1], rdate[, 2], rdate[, 3])
                type <- "tif"
                ## 200K
                # dirpath <- "CHIRP/bils/dekads/camer-carib"
                # fileformat <- "chirp%s%s.tar.gz"
                # filename <- sprintf(fileformat, rdate[, 1], rdate[, 4])
                # type <- "bil"
            }else{
                data.dir <- "global"
                ## 55M
                dirpath <- "CHIRP/dekads"
                fileformat <- "CHIRP.%s.%s.%s.tif"
                filename <- sprintf(fileformat, rdate[, 1], rdate[, 2], rdate[, 3])
                type <- "tif"
            }
            ncformat <- "chirp_%s%s%s.nc"
            dataname <- 'CHIRP version 1.0'
        }else return(NULL)

        urls <- file.path(ftp.ucsb, dirpath, filename)
        ncfiles <- sprintf(ncformat, rdate[, 1], rdate[, 2], rdate[, 3])
        deppath <- 0

        # exception
        if(GalParams$rfe.src == "chirpsv2-gb" && data.dir == "global"){
            ix <- basename(urls) == "v2p0chirps201825.tar.gz"
            urls[ix] <- file.path(ftp.ucsb, dirpath, "v2p0chirps201825.tar")
        }
    }
    else if(GalParams$tstep == "monthly"){
        if(GalParams$rfe.src == "chirpsv2-gb"){
            if(africa && !global){
                data.dir <- "africa"
                ## 700K
                dirpath <- "CHIRPS-2.0/africa_monthly/bils"
                fileformat <- "v2p0chirps%s%s.tar.gz"
                filename <- sprintf(fileformat, rdate[, 1], rdate[, 2])
                type <- "bil"
                ## 4.3M
                # dirpath <- "CHIRPS-2.0/africa_monthly/tifs"
                # fileformat <- "chirps-v2.0.%s.%s.tif.gz"
                # filename <- sprintf(fileformat, rdate[, 1], rdate[, 2])
                # type <- "tif"
            }else if(camer_carib && !global){
                data.dir <- "camer-carib"
                ## 90K
                dirpath <- "CHIRPS-2.0/camer-carib_monthly/bils"
                fileformat <- "v2p0chirps%s%s.tar.gz"
                filename <- sprintf(fileformat, rdate[, 1], rdate[, 2])
                type <- "bil"
                ## 260K
                # dirpath <- "CHIRPS-2.0/camer-carib_monthly/tifs"
                # fileformat <- "chirps-v2.0.%s.%s.tif.gz"
                # filename <- sprintf(fileformat, rdate[, 1], rdate[, 2])
                # type <- "tif"
            }else{
                data.dir <- "global"
                ## 3.3M
                dirpath <- "CHIRPS-2.0/global_monthly/bils"
                fileformat <- "v2p0chirps%s%s.tar.gz"
                filename <- sprintf(fileformat, rdate[, 1], rdate[, 2])
                type <- "bil"
                ## 14M
                # dirpath <- "CHIRPS-2.0/global_monthly/tifs"
                # fileformat <- "chirps-v2.0.%s.%s.tif.gz"
                # filename <- sprintf(fileformat, rdate[, 1], rdate[, 2])
                # type <- "tif"
            }
            ncformat <- "chirps_%s%s.nc"
            dataname <- 'CHIRPS version 2.0'
        }else if(GalParams$rfe.src == "chirp-gb"){
            years <- as.numeric(rdate[, 1])
            b2014 <- all(years < 2014)
            a2014 <- all(years >= 2014)
            if(africa && a2014 && !global){
                ## period 2014-01 to present
                data.dir <- "africa_tif"
                ## 9.2M
                dirpath <- "CHIRP/monthly/africa"
                fileformat <- "CHIRP.%s.%s.tif"
                filename <- sprintf(fileformat, rdate[, 1], rdate[, 2])
                type <- "tif"
            }else if(africa && b2014 && !global){
                ## period 1981-01 to 2013-12
                data.dir <- "africa_bil"
                ## 1M
                dirpath <- "CHIRP/bils/monthly/africa"
                fileformat <- "chirp%s%s.tar.gz"
                filename <- sprintf(fileformat, rdate[, 1], rdate[, 2])
                type <- "bil"
            }else if(camer_carib && a2014 && !global){
                ## period 2014-01 to present
                data.dir <- "camer-carib_tif"
                ## 1M
                dirpath <- "CHIRP/monthly/camer-carib"
                fileformat <- "CHIRP.%s.%s.tif"
                filename <- sprintf(fileformat, rdate[, 1], rdate[, 2])
                type <- "tif"
            }else if(camer_carib && b2014 && !global){
                ## period 1981-01 to 2013-12
                data.dir <- "camer-carib_bil"
                ## 300K
                dirpath <- "CHIRP/bils/monthly/camer-carib"
                fileformat <- "chirp%s%s.tar.gz"
                filename <- sprintf(fileformat, rdate[, 1], rdate[, 2])
                type <- "bil"
            }else{
                data.dir <- "global"
                ## 55M
                dirpath <- "CHIRP/monthly"
                fileformat <- "CHIRP.%s.%s.tif"
                filename <- sprintf(fileformat, rdate[, 1], rdate[, 2])
                type <- "tif"
            }
            ncformat <- "chirp_%s%s.nc"
            dataname <- 'CHIRP version 1.0'
        }else return(NULL)

        urls <- file.path(ftp.ucsb, dirpath, filename)
        ncfiles <- sprintf(ncformat, rdate[, 1], rdate[, 2])
        deppath <- 0
    }else if(GalParams$tstep == "hourly"){
        if(GalParams$rfe.src == "chirpsv2-af"){
            data.dir <- "africa"
            dirpath <- "CHIRPS-2.0/africa_6-hourly/p1_bin/extra_step"
            daty <- as.Date(paste(rdate[, 1], rdate[, 2], rdate[, 3], sep = "-"))
            vpath <- ifelse(daty <= as.Date("2015-07-31"), "p1_bin", "p1_bin/extra_step")
            vpath <- paste0("CHIRPS-2.0/africa_6-hourly/", vpath)
            vdir <- paste0(rdate[, 1], rdate[, 2])
            fileformat <- "rfe_gdas.bin.%s%s%s%s.gz"
            filename <- sprintf(fileformat, rdate[, 1], rdate[, 2], rdate[, 3], rdate[, 4])
            type <- "bin"
            ncformat <- "chirps_%s%s%s%s.nc"
            dataname <- 'CHIRPS version 2.0'

            urls <- file.path(ftp.ucsb, vpath, vdir, filename)
            ncfiles <- sprintf(ncformat, rdate[, 1], rdate[, 2], rdate[, 3], rdate[, 4])
            deppath <- 1
        }else return(NULL)
    }else return(NULL)

    list(dirpath = dirpath, deppath = deppath, datadir = data.dir, 
         fileformat = fileformat, filename = filename, type = type,
         ncfiles = ncfiles, urls = urls, baseurl = ftp.ucsb,
         dataname = dataname)
}

chirps.ucsb.table <- function(url){
    ret <- httr::GET(url)
    if(httr::status_code(ret) != 200){
        Insert.Messages.httr(ret)
        return(NULL)
    }
    ret <- httr::content(ret)

    # ret <- try(rvest::read_html(url), silent = TRUE)
    # if(inherits(ret, 'try-error')){
    #     Insert.Messages.Out(gsub('[\r\n]', '', ret), TRUE, "e")
    #     return(NULL)
    # }

    tmp <- rvest::html_table(ret, fill = TRUE)[[1]]
    tmp <- as.data.frame(tmp[-(1:2), ])
    tmp[tmp[, 2] != "", 2]
}

chirps.ucsb.dates <- function(url, type, fileformat = NA){
    tmp <- chirps.ucsb.table(url)
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
    ret <- sort(ret)

    return(ret)
}

chirps.ucsb.end_dates <- function(tmp, fileformat){
    ret <- extract_filename_dates(tmp, fileformat)
    if(is.null(ret)) return(NULL)
    ret <- ret[ret != ""]
    if(length(ret) == 0) return(NULL)
    ret <- sort(ret)

    return(ret)
}

#################################################################################

chirps.download.data <- function(lnk, dest, ncfl, bbox, type, GUI = TRUE){
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
        tmp <- chirps.decompress.file(dest, tmpdir, type)
        if(type == "bin"){
            ret <- chirps.extract.bin(tmp$file, ncfl, bbox)
        }else{
            ret <- chirps.extract.tif_bil(tmp$file, ncfl, bbox)
        }

        if(tmp$delete){
            unlink(tmp$file)
            if(type == "bil") unlink(tmp$hdr)
        }

        ##
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

chirps.decompress.file <- function(dest, tmpdir, type){
    tmphdr <- NULL

    if(grepl("\\.tar\\.gz$", dest, ignore.case = TRUE)){
        utils::untar(dest, exdir = tmpdir)
        # utils::untar(dest, exdir = tmpdir, compressed = TRUE,
        #              tar = Sys.getenv("R_INSTALL_TAR", "internal"))
        ff <- gsub("\\.tar\\.gz$", "", basename(dest))
        if(type == "bil"){
            tmpf <- file.path(tmpdir, paste0(ff, ".bil"))
            tmphdr <- file.path(tmpdir, paste0(ff, ".hdr"))
        }
        if(type == "tif"){
            tmpf <- file.path(tmpdir, paste0(ff, ".tif"))
        }
        delete <- TRUE
    }else if(grepl("[^\\.tar]\\.gz$", dest, ignore.case = TRUE)){
        tmpf <- file.path(tmpdir, gsub("\\.gz$", "", basename(dest)))
        R.utils::gunzip(dest, tmpf, remove = FALSE, overwrite = TRUE)
        delete <- TRUE
    }else if(grepl("\\.tar$", dest, ignore.case = TRUE)){
        utils::untar(dest, exdir = tmpdir)
        ff <- gsub("\\.tar$", "", basename(dest))
        if(type == "bil"){
            tmpf <- file.path(tmpdir, paste0(ff, ".bil"))
            tmphdr <- file.path(tmpdir, paste0(ff, ".hdr"))
        }
        if(type == "tif"){
            tmpf <- file.path(tmpdir, paste0(ff, ".tif"))
        }
        delete <- TRUE
    }else{
        tmpf <- dest
        delete <- FALSE
    }

    out <- list(file = tmpf, hdr = tmphdr, delete = delete)

    return(out)
}

chirps.extract.tif_bil <- function(dest, ncfl, bbox){
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
        ncgrd <- ncdf4::ncvar_def("precip", "mm", list(dx, dy), missval,
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

chirps.extract.bin <- function(dest, ncfl, bbox){
    con <- file(dest, open = "rb")
    on.exit(close(con))
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
        ## convert from kg/m2 to mm
        ## 6 hourly data val * 86400/4
        val <- round(val * 21600, 2)

        dx <- ncdf4::ncdim_def("lon", "degreeE", lon, longname = "Longitude")
        dy <- ncdf4::ncdim_def("lat", "degreeN", lat, longname = "Latitude")
        missval <- -99
        ncgrd <- ncdf4::ncvar_def("precip", "mm", list(dx, dy), missval,
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
