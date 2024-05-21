
cmorph_raw.download.iridl <- function(GalParams, nbfile = 3, GUI = TRUE, verbose = TRUE){
    dlpath <- "https://iridl.ldeo.columbia.edu/SOURCES/.NOAA/.NCEP/.CPC/.CMORPH/.V0px/.RAW"
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
        endh <- stringr::str_pad(as.numeric(rdate[, 4]) + GalParams$minhour, 2, pad = "0")
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
    if(!dir.exists(outdir))
        dir.create(outdir, showWarnings = FALSE, recursive = TRUE)
    destfiles <- file.path(outdir, paste0("cmorph-v0.x-raw_", date.out, ".nc"))

    ret <- cdt.download.data(urls, destfiles, destfiles, nbfile, GUI,
                             verbose, data.name, iridl.download.data)

    return(ret)
}

cmorph_raw.coverage.iridl <- function(GalParams){
    dlpath <- "https://iridl.ldeo.columbia.edu/SOURCES/.NOAA/.NCEP/.CPC/.CMORPH/.V0px/.RAW"
    vardir <- switch(GalParams$tstep,
                      "hourly" = ".3-hourly/.prcp",
                      "daily" = ".daily/.prcp"
                    )
    url <- paste0(dlpath, '/', vardir)
    end_date <- iridl.get.end_date(url, GalParams$tstep)

    start_date <- switch(GalParams$tstep,
                         "daily" = "20140601",
                         "hourly" = "2015050100"
                        )
    list(name = "CMORPH V0.x RAW",
         timestep = GalParams$tstep,
         start = start_date, end = end_date)
}

cmorph.download.cpc.ncep <- function(GalParams, nbfile = 3, GUI = TRUE, verbose = TRUE){
    info <- cmorph.info.cpc.ncep(GalParams)
    if(is.null(info)) return(-3)
    urls <- file.path(info$baseurl, info$ftp.dir, info$filename)

    #########
    data.name <- paste0(info$dataname, "_", info$data.tres)
    outdir <- file.path(GalParams$dir2save, data.name)
    extrdir <- file.path(outdir, "Extracted")
    if(!dir.exists(extrdir))
        dir.create(extrdir, showWarnings = FALSE, recursive = TRUE)
    origdir <- file.path(outdir, "Data_Global")
    if(!dir.exists(origdir))
        dir.create(origdir, showWarnings = FALSE, recursive = TRUE)
    destfiles <- file.path(origdir, info$filename)
    if(info$pars$type %in% 4:6){
        ncfiles <- lapply(info$ncfiles, function(x) file.path(extrdir, x))
    }else{
        ncfiles <- file.path(extrdir, info$ncfiles)
    }

    ret <- cdt.download.data(urls, destfiles, ncfiles, nbfile, GUI,
                             verbose, data.name, cmorph.download.data,
                             bbox = GalParams$bbox, pars = info$pars)

    return(ret)
}

cmorph.coverage.cpc.ncep <- function(GalParams){
    if(GalParams$tstep %in% c('minute', 'hourly')){
        timestep <- paste(GalParams$minhour, GalParams$tstep)
    }else{
        timestep <- GalParams$tstep
    }
    out <- list(name = GalParams$rfe.src, timestep = timestep)

    info <- cmorph.info.cpc.ncep(GalParams)
    if(is.null(info)) return(out)

    dataname <- strsplit(info$dataname, '_')[[1]]
    out$name <- paste0(dataname, collapse = ' ')
    url <- file.path(info$baseurl, info$dirpath)

    if(info$deppath == 0){
        tmp <- cmorph.cpc.ncep.dates(url, 'file', info$fileformat)
        if(is.null(tmp)) return(out)

        out$start <- tmp[1]
        out$end <- tmp[length(tmp)]
    }else if(info$deppath == 2){
        tmp <- cmorph.cpc.ncep.dates(url, 'directory')
        if(is.null(tmp)) return(out)
        tmp <- tmp[as.numeric(tmp) > 1997]

        ## start
        url <- file.path(info$baseurl, info$dirpath, tmp[1])
        start_d <- cmorph.cpc.ncep.dates(url, 'directory')
        if(is.null(start_d)) return(out)

        url <- file.path(info$baseurl, info$dirpath, tmp[1], start_d[1])
        start_d <- cmorph.cpc.ncep.dates(url, 'file', info$fileformat)
        if(is.null(start_d)) return(out)

        out$start <- start_d[1]

        ## end
        url <- file.path(info$baseurl, info$dirpath, tmp[length(tmp)])
        end_d <- cmorph.cpc.ncep.dates(url, 'directory')
        if(is.null(end_d)) return(out)

        end_mon <- as.Date(paste0(end_d[length(end_d)], '01'), '%Y%m%d')
        this_mon <- as.Date(paste0(format(Sys.time(), '%Y-%m-'), '01'))
        if(end_mon > this_mon) end_mon <- this_mon
        start_mon <- as.Date(paste0(format(end_mon - 1800, '%Y-%m-'), '01'))
        seq_mon <- seq(start_mon, end_mon, 'month')
        seq_mon <- paste0(format(seq_mon, '%Y'), '/', format(seq_mon, '%Y%m'))
        seq_mon <- rev(seq_mon)

        for(j in seq_along(seq_mon)){
            url <- file.path(info$baseurl, info$dirpath, seq_mon[j])
            ret <- cmorph.cpc.ncep.table(url)
            if(is.null(ret)) return(out)
            if(length(ret) > 0) break
        }

        end_d <- cmorph.cpc.ncep.end_dates(ret, info$fileformat)
        if(is.null(end_d)) return(out)
        out$end <- end_d[length(end_d)]
    }else if(info$deppath == 3){
        tmp <- cmorph.cpc.ncep.dates(url, 'directory')
        if(is.null(tmp)) return(out)

        url <- file.path(info$baseurl, info$dirpath, tmp[length(tmp)])
        end_d <- cmorph.cpc.ncep.dates(url, 'directory')
        if(is.null(end_d)) return(out)

        url <- file.path(info$baseurl, info$dirpath, tmp[length(tmp)], end_d[length(end_d)])
        end_d <- cmorph.cpc.ncep.dates(url, 'directory')
        if(is.null(end_d)) return(out)

        end_day <- as.Date(end_d[length(end_d)], '%Y%m%d')
        this_day <- as.Date(format(Sys.time(), '%Y-%m-%d'))
        if(end_day > this_day) end_day <- this_day

        start_day <- end_day - 30
        seq_day <- seq(start_day, end_day, 'day')
        seq_day <- paste0(format(seq_day, '%Y'), '/',
                          format(seq_day, '%Y%m'), '/',
                          format(seq_day, '%Y%m%d'))
        seq_day <- rev(seq_day)

        for(j in seq_along(seq_day)){
            url <- file.path(info$baseurl, info$dirpath, seq_day[j])
            ret <- cmorph.cpc.ncep.table(url)
            if(is.null(ret)) return(out)
            if(length(ret) > 0) break
        }

        end_d <- cmorph.cpc.ncep.end_dates(ret, info$fileformat)
        if(is.null(end_d)) return(out)
        out$end <- end_d[length(end_d)]
        out$start <- "202301010000"
    }else return(out)

    if(GalParams$tstep == 'minute' && GalParams$rfe.src != "cmorph2xnrt-gb"){
        out$start <- paste0(out$start, '00')
        out$end <- paste0(out$end, '30')
    }

    return(out)
}

cmorph.info.cpc.ncep <- function(GalParams){
    ftp.cpc <- "https://ftp.cpc.ncep.noaa.gov/precip"
    rdate <- table.format.date.time(GalParams$tstep, GalParams$date.range, GalParams$minhour)
    if(is.null(rdate)) return(NULL)

    if(GalParams$tstep == "daily"){
        if(GalParams$rfe.src == "cmorphv1.0bld-gb"){
            dirpath <- "CMORPH_V1.0/BLD/0.25deg-DLY_EOD/GLB"
            deppath <- 2
            ftp.dir <- file.path(dirpath, rdate[, 1], paste0(rdate[, 1], rdate[, 2]))
            fileformat <- "CMORPH_V1.0BETA_BLD_0.25deg-DLY_EOD_%s%s%s.gz"
            filename <- sprintf(fileformat, rdate[, 1], rdate[, 2], rdate[, 3])
            dataname <- "CMORPH_V1.0_BLD"
            ncflname <- "cmorph-v1.0-bld"
            nclongname <- "Gauge - CMORPH_Adj Blended Analysis of daily precipitation ending at EOD"
            pars <- list(type = 1, longname = nclongname)
        }
        else if(GalParams$rfe.src == "cmorphrtv0.xbld-gb"){
            dirpath <- "CMORPH_RT/BLD"
            deppath <- 2
            ftp.dir <- file.path(dirpath, rdate[, 1], paste0(rdate[, 1], rdate[, 2]))
            fileformat <- "CMORPH_V0.x_BLD_0.25deg-DLY_EOD_%s%s%s.gz"
            filename <- sprintf(fileformat, rdate[, 1], rdate[, 2], rdate[, 3])
            dataname <- "CMORPH_RT_V0.x_BLD"
            ncflname <- "cmorph-rt-v0.x-bld"
            nclongname <- "Blended daily precipitation ending at GTS gauge EOD"
            pars <- list(type = 1, longname = nclongname)
       }
        else if(GalParams$rfe.src == "cmorphv1.0adj-gb"){
            dirpath <- "CMORPH_V1.0/CRT/0.25deg-DLY_00Z"
            deppath <- 2
            ftp.dir <- file.path(dirpath, rdate[, 1], paste0(rdate[, 1], rdate[, 2]))
            fileformat <- "CMORPH_V1.0_ADJ_0.25deg-DLY_00Z_%s%s%s.bz2"
            filename <- sprintf(fileformat, rdate[, 1], rdate[, 2], rdate[, 3])
            dataname <- "CMORPH_V1.0_ADJ"
            ncflname <- "cmorph-v1.0-adj"
            nclongname <- 'Bias-corrected CMORPH Version 1.0BETA Version, daily precipitation from 00Z-24Z'
            pars <- list(type = 2, longname = nclongname)
        }
        else if(GalParams$rfe.src == "cmorphrtv0.xadj-gb"){
            dirpath <- "CMORPH_RT/ICDR/0.25deg-DLY_00Z"
            deppath <- 0
            ftp.dir <- dirpath
            fileformat <- "CMORPH_V0.x_ADJ_0.25deg-DLY_00Z_%s%s%s.nc"
            filename <- sprintf(fileformat, rdate[, 1], rdate[, 2], rdate[, 3])
            dataname <- "CMORPH_V0.x_ICDR"
            ncflname <- "cmorph-v0.x-adj"
            nclongname <- "NOAA Interim Climate Data Record (ICDR) of CPC Morphing Technique (CMORPH) High Resolution Global Precipitation Estimates"
            pars <- list(type = 3, longname = nclongname)
        }
        else if(GalParams$rfe.src == "cmorphv0.xraw-gb"){
            dirpath <- "CMORPH_V0.x/RAW/0.25deg-DLY_00Z"
            deppath <- 2
            ftp.dir <- file.path(dirpath, rdate[, 1], paste0(rdate[, 1], rdate[, 2]))
            fileformat <- "CMORPH_V0.x_RAW_0.25deg-DLY_00Z_%s%s%s.gz"
            filename <- sprintf(fileformat, rdate[, 1], rdate[, 2], rdate[, 3])
            dataname <- "CMORPH_V0.x_RAW"
            ncflname <- "cmorph-v0.x-raw"
            nclongname <- "Satellite only precipitation estimates from 00Z-24Z"
            pars <- list(type = 2, longname = nclongname)
        }else return(NULL)

        data.tres <- GalParams$tstep
        ncfiles <- paste0(ncflname, "_", rdate[, 1], rdate[, 2], rdate[, 3], ".nc")
    }
    else if(GalParams$tstep == "hourly"){
        if(GalParams$minhour == 1){
            if(GalParams$rfe.src == "cmorphrtv0.xadj-gb"){
                dirpath <- "CMORPH_RT/ICDR/0.25deg-HLY"
                deppath <- 0
                ftp.dir <- dirpath
                fileformat <- "CMORPH_V0.x_ADJ_0.25deg-HLY_%s%s%s%s.nc"
                filename <- sprintf(fileformat, rdate[, 1], rdate[, 2], rdate[, 3], rdate[, 4])
                dataname <- "CMORPH_V0.x_ICDR"
                ncfiles <- paste0("cmorph-v0.x-adj_", rdate[, 1], rdate[, 2], rdate[, 3], rdate[, 4], ".nc")
                nclongname <- "NOAA Interim Climate Data Record (ICDR) of CPC Morphing Technique (CMORPH) High Resolution Global Precipitation Estimates"
                pars <- list(type = 3, longname = nclongname)
            }else return(NULL)
        }
        else if(GalParams$minhour == 3){
            if(GalParams$rfe.src == "cmorphv1.0adj-gb"){
                dirpath <- "CMORPH_V1.0/CRT/0.25deg-3HLY"
                deppath <- 2
                ftp.dir <- file.path(dirpath, rdate[, 1], paste0(rdate[, 1], rdate[, 2]))
                fileformat <- "CMORPH_V1.0_ADJ_0.25deg-3HLY_%s%s%s.bz2"
                filename <- sprintf(fileformat, rdate[, 1], rdate[, 2], rdate[, 3])
                ix <- !duplicated(filename)
                ftp.dir <- ftp.dir[ix]
                filename <- filename[ix]
                dataname <- "CMORPH_V1.0_ADJ"
                ncfiles <- paste0("cmorph-v1.0-adj_", rdate[, 1], rdate[, 2], rdate[, 3], rdate[, 4], ".nc")
                ncfiles <- split(ncfiles, paste0(rdate[, 1], rdate[, 2], rdate[, 3]))
                nclongname <- "RAW CMORPH integrated satellite precipitation estimates"
                pars <- list(type = 4, longname = nclongname)
            }
            else if(GalParams$rfe.src == "cmorphv0.xraw-gb"){
                dirpath <- "CMORPH_V0.x/RAW/0.25deg-3HLY"
                deppath <- 2
                ftp.dir <- file.path(dirpath, rdate[, 1], paste0(rdate[, 1], rdate[, 2]))
                fileformat <- "CMORPH_V0.x_RAW_0.25deg-3HLY_%s%s%s.gz"
                filename <- sprintf(fileformat, rdate[, 1], rdate[, 2], rdate[, 3])
                ix <- !duplicated(filename)
                ftp.dir <- ftp.dir[ix]
                filename <- filename[ix]
                dataname <- "CMORPH_V0.x_RAW"
                ncfiles <- paste0("cmorph-v0.x-raw_", rdate[, 1], rdate[, 2], rdate[, 3], rdate[, 4], ".nc")
                ncfiles <- split(ncfiles, paste0(rdate[, 1], rdate[, 2], rdate[, 3]))
                nclongname <- "Combined microwave estimates"
                pars <- list(type = 4, longname = nclongname)
            }else return(NULL)
        }else return(NULL)

        data.tres <- paste0(GalParams$minhour, GalParams$tstep)
    }
    else if(GalParams$tstep == "minute"){
        if(GalParams$minhour == 30){
            if(GalParams$rfe.src == "cmorphrtv0.xadj-gb"){
                dirpath <- "CMORPH_RT/ICDR/8km-30min"
                deppath <- 0
                ftp.dir <- dirpath
                fileformat <- "CMORPH_V0.x_ADJ_8km-30min_%s%s%s%s.nc"
                filename <- sprintf(fileformat, rdate[, 1], rdate[, 2], rdate[, 3], rdate[, 4])
                ix <- !duplicated(filename)
                filename <- filename[ix]
                dataname <- "CMORPH_V0.x_ICDR"
                ncfiles <- paste0("cmorph-v0.x-adj_", rdate[, 1], rdate[, 2], rdate[, 3], rdate[, 4], rdate[, 5], ".nc")
                ncfiles <- split(ncfiles, paste0(rdate[, 1], rdate[, 2], rdate[, 3], rdate[, 4]))
                nclongname <- "NOAA Interim Climate Data Record (ICDR) of CPC Morphing Technique (CMORPH) High Resolution Global Precipitation Estimates"
                pars <- list(type = 5, longname = nclongname)
            }
            else if(GalParams$rfe.src == "cmorphv0.xrt-gb"){
                dirpath <- "CMORPH_RT/GLOBE/data"
                deppath <- 2
                ftp.dir <- file.path(dirpath, rdate[, 1], paste0(rdate[, 1], rdate[, 2]))
                fileformat <- "CMORPH_V0.x_RT_8km-30min_%s%s%s%s.gz"
                filename <- sprintf(fileformat, rdate[, 1], rdate[, 2], rdate[, 3], rdate[, 4])
                ix <- !duplicated(filename)
                ftp.dir <- ftp.dir[ix]
                filename <- filename[ix]
                dataname <- "CMORPH_V0.x_RT"
                ncfiles <- paste0("cmorph-v0.x-rt_", rdate[, 1], rdate[, 2], rdate[, 3], rdate[, 4], rdate[, 5], ".nc")
                ncfiles <- split(ncfiles, paste0(rdate[, 1], rdate[, 2], rdate[, 3], rdate[, 4]))
                nclongname <- "CMORPH Rain accumulation (Real-Time Version)"
                pars <- list(type = 6, longname = nclongname)
            }
            else if(GalParams$rfe.src == "cmorphv0.xraw-gb"){
                dirpath <- "CMORPH_V0.x/RAW/8km-30min"
                deppath <- 2
                ftp.dir <- file.path(dirpath, rdate[, 1], paste0(rdate[, 1], rdate[, 2]))
                fileformat <- "CMORPH_V0.x_RAW_8km-30min_%s%s%s%s.gz"
                filename <- sprintf(fileformat, rdate[, 1], rdate[, 2], rdate[, 3], rdate[, 4])
                ix <- !duplicated(filename)
                ftp.dir <- ftp.dir[ix]
                filename <- filename[ix]
                dataname <- "CMORPH_V0.x_RAW"
                ncfiles <- paste0("cmorph-v0.x-raw_", rdate[, 1], rdate[, 2], rdate[, 3], rdate[, 4], rdate[, 5], ".nc")
                ncfiles <- split(ncfiles, paste0(rdate[, 1], rdate[, 2], rdate[, 3], rdate[, 4]))
                nclongname <- "CMORPH Precipitation estimates"
                pars <- list(type = 6, longname = nclongname)
            }
            else if(GalParams$rfe.src == "cmorph2xnrt-gb"){
                dirpath <- "CMORPH2/CMORPH2NRT/DATA"
                deppath <- 3
                ftp.dir <- file.path(dirpath, rdate[, 1], paste0(rdate[, 1], rdate[, 2]),
                                     paste0(rdate[, 1], rdate[, 2], rdate[, 3]))
                fileformat <- "CMORPH2_0.25deg-30min_%s%s%s%s%s.RT.nc"
                filename <- sprintf(fileformat, rdate[, 1], rdate[, 2], rdate[, 3], rdate[, 4], rdate[, 5])
                ix <- !duplicated(filename)
                ftp.dir <- ftp.dir[ix]
                filename <- filename[ix]
                dataname <- "CMORPH2x_NRT"
                ncfiles <- paste0("cmorph2x-nrt_", rdate[, 1], rdate[, 2], rdate[, 3], rdate[, 4], rdate[, 5], ".nc")
                nclongname <- "CMORPH2 NRT Precipitation estimates"
                pars <- list(type = 7, longname = nclongname)
            }else return(NULL)
        }else return(NULL)

        data.tres <- paste0(GalParams$minhour, GalParams$tstep)
    }else return(NULL)

    list(dirpath = dirpath, deppath = deppath, ftp.dir = ftp.dir,
         fileformat = fileformat, filename = filename, dataname = dataname,
         ncfiles = ncfiles, pars = pars, data.tres = data.tres, baseurl = ftp.cpc)
}

cmorph.cpc.ncep.table <- function(url){
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
    tmp[tmp[, 1] != "", 1]
}

cmorph.cpc.ncep.dates <- function(url, type, fileformat = NA){
    tmp <- cmorph.cpc.ncep.table(url)
    if(length(tmp) == 0) return(NULL)

    if(type == 'file'){
        ret <- extract_filename_dates(tmp, fileformat)
        if(is.null(ret)) return(NULL)
    }else if(type == 'directory'){
        ret <- gsub('[^[:digit:]]', '', tmp)
    }else return(NULL)
    ret <- ret[ret != ""]
    if(length(ret) == 0) return(NULL)
    ret <- sort(ret)

    return(ret)
}

cmorph.cpc.ncep.end_dates <- function(tmp, fileformat){
    ret <- extract_filename_dates(tmp, fileformat)
    if(is.null(ret)) return(NULL)
    ret <- ret[ret != ""]
    if(length(ret) == 0) return(NULL)
    ret <- sort(ret)

    return(ret)
}

#################################################################################

cmorph.download.data <- function(lnk, dest, ncfl, bbox, pars, GUI = TRUE){
    xx <- basename(dest)

    link.exist <- try(readLines(lnk, 1), silent = TRUE)
    if(inherits(link.exist, "try-error")){
        msg <- gsub('[\r\n]', '', link.exist)
        Insert.Messages.Out(msg, TRUE, "e", GUI)
        return(xx)
    }

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
    }else{
        msg <- gsub('[\r\n]', '', dc[1])
        Insert.Messages.Out(msg, TRUE, "e", GUI)
    }

    return(xx)
}

###########################################################################

cmorph.extract.data.1 <- function(tmpf, ncfl, bbox, pars){
    con <- file(tmpf, open = "rb")
    on.exit(close(con))
    val <- try(readBin(con, numeric(), 720 * 1440, 4, endian = "little"), silent = TRUE)
    ret <- 1
    if(!inherits(val, "try-error")){
        lon <- seq(0.125, length.out = 1440, by = 0.25)
        lon <- ((lon + 180) %% 360) - 180
        lat <- seq(-89.875, length.out = 720, by = 0.25)
        missval <- -999.0

        ox <- order(lon)
        lon <- lon[ox]
        ix <- lon >= bbox$minlon & lon <= bbox$maxlon
        iy <- lat >= bbox$minlat & lat <= bbox$maxlat
        lon <- lon[ix]
        lat <- lat[iy]

        val <- matrix(val, 1440, 720)
        val <- val[ox, ]
        val <- val[ix, iy]
        val <- round(val, 2)
        val[is.na(val)] <- missval

        dx <- ncdf4::ncdim_def("lon", "degreeE", lon, longname = "Longitude")
        dy <- ncdf4::ncdim_def("lat", "degreeN", lat, longname = "Latitude")
        ncgrd <- ncdf4::ncvar_def("precip", "mm", list(dx, dy), missval,
                                  pars$longname, "float", compression = 9)

        nc <- ncdf4::nc_create(ncfl, ncgrd)
        ncdf4::ncvar_put(nc, ncgrd, val)
        ncdf4::nc_close(nc)

        ret <- 0
    }

    return(ret)
}

cmorph.extract.data.2 <- function(tmpf, ncfl, bbox, pars){
    con <- file(tmpf, open = "rb")
    on.exit(close(con))
    val <- try(readBin(con, numeric(), 480 * 1440, 4, endian = "little"), silent = TRUE)
    ret <- 1
    if(!inherits(val, "try-error")){
        lon <- seq(0.125, length.out = 1440, by = 0.25)
        lon <- ((lon + 180) %% 360) - 180
        lat <- seq(-59.875, length.out = 480, by = 0.25)
        missval <- -999.0

        ox <- order(lon)
        lon <- lon[ox]
        ix <- lon >= bbox$minlon & lon <= bbox$maxlon
        iy <- lat >= bbox$minlat & lat <= bbox$maxlat
        lon <- lon[ix]
        lat <- lat[iy]

        val <- matrix(val, 1440, 480)
        val <- val[ox, ]
        val <- val[ix, iy]
        val <- round(val, 2)
        val[is.na(val)] <- missval

        dx <- ncdf4::ncdim_def("lon", "degreeE", lon, longname = "Longitude")
        dy <- ncdf4::ncdim_def("lat", "degreeN", lat, longname = "Latitude")
        ncgrd <- ncdf4::ncvar_def("precip", "mm", list(dx, dy), missval,
                                  pars$longname, "float", compression = 9)

        nc <- ncdf4::nc_create(ncfl, ncgrd)
        ncdf4::ncvar_put(nc, ncgrd, val)
        ncdf4::nc_close(nc)

        ret <- 0
    }

    return(ret)
}

cmorph.extract.data.3 <- function(tmpf, ncfl, bbox, pars){
    nc <- try(ncdf4::nc_open(tmpf), silent = TRUE)
    ret <- 1
    if(!inherits(nc, "try-error")){
        varid <- "cmorph"
        lon <- nc$var[[varid]]$dim[[1]]$vals
        lat <- nc$var[[varid]]$dim[[2]]$vals
        val <- ncdf4::ncvar_get(nc, varid)
        ncdf4::nc_close(nc)

        lon <- ((lon + 180) %% 360) - 180
        ix <- lon >= bbox$minlon & lon <= bbox$maxlon
        iy <- lat >= bbox$minlat & lat <= bbox$maxlat
        lon <- lon[ix]
        lat <- lat[iy]
        ox <- order(lon)
        lon <- lon[ox]

        missval <- -999.0
        val <- val[ix, iy]
        val <- val[ox, ]
        val <- round(val, 2)
        val[is.na(val)] <- missval

        dx <- ncdf4::ncdim_def("lon", "degreeE", lon, longname = "Longitude")
        dy <- ncdf4::ncdim_def("lat", "degreeN", lat, longname = "Latitude")
        ncgrd <- ncdf4::ncvar_def("precip", "mm", list(dx, dy), missval,
                                  pars$longname, "float", compression = 9)

        nc <- ncdf4::nc_create(ncfl, ncgrd)
        ncdf4::ncvar_put(nc, ncgrd, val)
        ncdf4::nc_close(nc)

        ret <- 0
    }
    return(ret)
}

cmorph.extract.data.4 <- function(tmpf, ncfl, bbox, pars){
    con <- file(tmpf, open = "rb")
    on.exit(close(con))
    val <- try(readBin(con, numeric(), 8 * 480 * 1440, 4, endian = "little"), silent = TRUE)
    ret <- 1
    if(!inherits(val, "try-error")){
        lon <- seq(0.125, length.out = 1440, by = 0.25)
        lon <- ((lon + 180) %% 360) - 180
        lat <- seq(-59.875, length.out = 480, by = 0.25)
        missval <- -999.0

        ox <- order(lon)
        lon <- lon[ox]
        ix <- lon >= bbox$minlon & lon <= bbox$maxlon
        iy <- lat >= bbox$minlat & lat <= bbox$maxlat
        lon <- lon[ix]
        lat <- lat[iy]

        val <- array(val, c(1440, 480, 8))
        val <- val[ox, , ]
        val <- val[ix, iy, ]
        val <- round(val, 2)
        val[is.na(val)] <- missval

        dx <- ncdf4::ncdim_def("lon", "degreeE", lon, longname = "Longitude")
        dy <- ncdf4::ncdim_def("lat", "degreeN", lat, longname = "Latitude")
        ncgrd <- ncdf4::ncvar_def("precip", "mm", list(dx, dy), missval,
                                  pars$longname, "float", compression = 9)

        ncfl <- ncfl[[1]]
        daty <- gsub(".*_|\\.nc$", "", basename(ncfl))
        ix <- (as.numeric(substr(daty, 9, 10)) / 3) + 1
        val <- val[, , ix, drop = FALSE]

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
    con <- file(tmpf, open = "rb")
    on.exit(close(con))
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
        val[is.na(val)] <- missval

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

cmorph.extract.data.7 <- function(tmpf, ncfl, bbox, pars){
    nc <- try(ncdf4::nc_open(tmpf), silent = TRUE)
    ret <- 1
    if(!inherits(nc, "try-error")){
        varid <- "cmorph"
        lon <- nc$var[[varid]]$dim[[1]]$vals
        lat <- nc$var[[varid]]$dim[[2]]$vals
        val <- ncdf4::ncvar_get(nc, varid)
        ncdf4::nc_close(nc)

        ############
        lon <- ((lon + 180) %% 360) - 180
        ix <- lon >= bbox$minlon & lon <= bbox$maxlon
        iy <- lat >= bbox$minlat & lat <= bbox$maxlat
        lon <- lon[ix]
        lat <- lat[iy]
        ox <- order(lon)
        lon <- lon[ox]

        ############
        missval <- -999.0
        val <- val[ix, iy]
        val <- val[ox, ]
        ## mm/hr to mm
        val <- val * 0.5
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
