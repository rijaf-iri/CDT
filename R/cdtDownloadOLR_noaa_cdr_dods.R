

# https://www.ncei.noaa.gov/products/climate-data-records/atmospheric


# https://www.ncei.noaa.gov/products/climate-data-records/outgoing-longwave-radiation-daily
# https://www.ncei.noaa.gov/thredds/catalog/cdr/olr-daily/catalog.html
# https://www.ncei.noaa.gov/thredds/dodsC/cdr/olr-daily/olr-daily_v01r02_20220101_20221231.nc.html

# https://www.ncei.noaa.gov/products/climate-data-records/outgoing-longwave-radiation-monthly
# https://www.ncei.noaa.gov/thredds/dodsC/cdr/olr-monthly/olr-monthly_v02r07_197901_202311.nc.html

## cdtDownloadOLR_noaa_cdr_dods.R

    # ## Mada
    # bbox <- list(minlon = 42, maxlon = 52, minlat = -26, maxlat = -11)
    # ## west africa, burkina, mali, nireg
    # bbox <- list(minlon = -6, maxlon = 6, minlat = 9, maxlat = 16)

    # # cdtParameters_initialization.R

    # action <- "down.OLR"

    # date.range <- list(start.year = 2022, start.mon = 12, start.dek = 1,
    #                    start.pen = 1, start.day = 27,
    #                    start.hour = 0, start.min = 0,
    #                    end.year = 2023, end.mon = 1, end.dek = 3,
    #                    end.pen = 6, end.day = 5,
    #                    end.hour = 23, end.min = 55)

    # # ret.params <- list(action = action,
    # #                    tstep = "daily",
    # #                    var = "total",
    # #                    date.range = date.range,
    # #                    bbox = .cdtData$Config$region,
    # #                    dir2save = getwd()
    # #                 )

    # GalParams <- list(action = action,
    #                    tstep = "daily",
    #                    var = "total",
    #                    date.range = date.range,
    #                    bbox = region,
    #                    dir2save = "/Users/rijaf/Desktop/DATA/ZMD2023"
    #                 )

noaa_olr_cdr.ncei.noaa.gov <- function(GalParams, nbfile = 1, GUI = TRUE, verbose = TRUE){
    xlon <- seq(0.5, 360, 1)
    xlon <- ((xlon + 180) %% 360) - 180
    xlat <- seq(-89.5, 89.5, 1)

    ix <- xlon >= GalParams$bbox$minlon & xlon <= GalParams$bbox$maxlon
    iy <- xlat >= GalParams$bbox$minlat & xlat <= GalParams$bbox$maxlat

    if(!any(ix) | !any(iy)){
        Insert.Messages.Out("Invalid area of interest", TRUE, "e", GUI)
        return(-2)
    }

    rix <- rle(ix)
    ie <- cumsum(rix$lengths)
    is <- c(1, (ie + 1)[-length(ie)])
    ix <- lapply(seq_along(is), function(j) is[j]:ie[j])
    ilon <- lapply(ix[rix$values], function(x) range(x) - 1 )
    ilat <- range(which(iy)) - 1

    query_lon <- lapply(ilon, function(x) paste0("[", x[1], ":", 1, ":", x[2], "]"))
    query_lat <- paste0("[", ilat[1], ":", 1, ":", ilat[2], "]")

    #############
    opendap_url <- "https://www.ncei.noaa.gov/thredds/dodsC/cdr/olr-daily"

    seqDays <- seq.format.date.time('daily', GalParams$date.range)
    if(is.null(seqDays)) return(-2)
    years <- format(seqDays, '%Y')

    format <- "%Y-%m-%d %H:%M:%S"
    time_now <- Sys.time()
    time_now <- format(time_now, format, tz = "UTC")
    time_now <- strptime(time_now, format, tz = "UTC")
    time_now <- as.Date(time_now)
    year_now <- format(time_now, '%Y')

    year_prelim <- years == year_now

    dods_nc0 <- NULL
    req_days0 <- NULL
    out_date0 <- NULL
    if(any(!year_prelim)){
        dods_frmt <- "olr-daily_v01r02_%s0101_%s1231.nc.ascii"
        yyr <- years[!year_prelim]
        dods_nc0 <- sprintf(dods_frmt, yyr, yyr)

        req_days0 <- seqDays[!year_prelim]
        out_date0 <- format(req_days0, '%Y%m%d')
        req_days0 <- as.numeric(format(req_days0, '%j')) - 1
    }

    dods_nc1 <- NULL
    req_days1 <- NULL
    out_date1 <- NULL
    if(any(year_prelim)){
        yyr <- years[year_prelim]
        sdate <- paste0(yyr[1], '0101')
        edate <- format(time_now - 1:3, '%Y%m%d')
        to_check <- paste0('olr-daily_v01r02-preliminary_', sdate, '_', edate, '.nc')
        res_check <- sapply(to_check, function(chk){
            url_check <- paste0(opendap_url, '/', chk, '.html')
            ret <- try(suppressWarnings(readLines(url_check, n = 1)), silent = TRUE)
            if(inherits(ret, "try-error")) FALSE else TRUE
        })
        if(any(res_check)){
            dods_nc1 <- rep(paste0(to_check[res_check], '.ascii'), length(yyr))
            req_days1 <- seqDays[year_prelim]
            out_date1 <- format(req_days1, '%Y%m%d')
            req_days1 <- as.numeric(format(req_days1, '%j')) - 1
        }
    }

    dods_nc <- c(dods_nc0, dods_nc1)
    req_days <- c(req_days0, req_days1)
    out_dates <- c(out_date0, out_date1)

    #############

    urls <- lapply(seq_along(dods_nc), function(j){
        dods <- file.path(opendap_url, dods_nc[j])
        query_time <- sprintf("[%s:1:%s]", req_days[j], req_days[j])

        sapply(seq_along(query_lon), function(i){
            req_var <- paste0('olr', query_time, query_lat, query_lon[[i]])
            paste0(dods, '?', req_var)
        })
    })

    #########
    data.name <- paste0("NOAA-OLR_CDR_", GalParams$tstep)
    outdir <- file.path(GalParams$dir2save, data.name)
    dir.create(outdir, showWarnings = FALSE, recursive = TRUE)

    destfiles <- lapply(seq_along(urls), function(j){
        sapply(seq_along(query_lon), function(i)
            file.path(outdir, paste0("tmp_olr_", out_dates[j], "_", i, ".txt")))
    })

    ret <- cdt.download.data(urls, destfiles, out_dates, nbfile, GUI,
                             verbose, data.name, noaa_olr_cdr.download.data)

    return(ret)
}


noaa_olr_cdr.download.data <- function(lnk, dest, ncfl, pars, GUI = TRUE){
    on.exit(lapply(dest, unlink))

    dest <- dest[[1]]
    lnk <- lnk[[1]]
    xx <- ncfl

    dc <- lapply(seq_along(lnk), function(j){
         ret <- try(curl::curl_download(lnk[j], dest[j]), silent = TRUE)
         rc <- 0
         if(inherits(ret, "try-error")){
            msg <- gsub('[\r\n]', '', ret[1])
            Insert.Messages.Out(msg, TRUE, "w", GUI)
            rc <- 1
         }
         rc
    })

    if(all(unlist(dc) == 0)){
        ret <- noaa_olr_cdr.format.data(dest)
        if(ret == 0) xx <- NULL
    }

    return(xx)
}

noaa_olr_cdr.format.data <- function(dest){
    ncdir <- dirname(dest[1])


    return(0)
}

## time
## units: days since 1970-01-01 00:00:00
# as.Date(19353.5, origin = "1970-01-01")
# as.Date(19362.5, origin = "1970-01-01")

