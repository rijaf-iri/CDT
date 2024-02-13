
    ## cdtDownloadOLR_noaa_cbo_iridl.R
    ## Download NOAA Outgoing Longwave Radiation (OLR) -- (Menu)
    ## CPC Blended OLR (CBO) Version 1 -- (text at the top of dialog box)
    ## data source info
    # https://iridl.ldeo.columbia.edu/SOURCES/.NOAA/.NCEP/.CPC/.CBO_V1/
    # https://ftp.cpc.ncep.noaa.gov/precip/CBO_V1/READ_ME/CBO_V1_Readme.txt
    # https://ftp.cpc.ncep.noaa.gov/precip/CBO_V1/READ_ME/CPC_New_OLR_20230907.pdf

    # bbox <- list(minlon = 42, maxlon = 52, minlat = -26, maxlat = -11.5)

    # # cdtParameters_initialization.R

    # action <- "down.OLR"

    # date.range <- list(start.year = 2020, start.mon = 1, start.dek = 1,
    #                    start.pen = 1, start.day = 1,
    #                    start.hour = 0, start.min = 0,
    #                    end.year = 2020, end.mon = 1, end.dek = 3,
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
    #                    bbox = bbox,
    #                    dir2save = "/Users/rijaf/Desktop/DATA/ZMD2023"
    #                 )


noaa_olr_cbo.download.iridl <- function(GalParams, nbfile = 1, GUI = TRUE, verbose = TRUE){
    dlpath <- "https://iridl.ldeo.columbia.edu/SOURCES/.NOAA/.NCEP/.CPC/.CBO_V1/.r0p25"
    tspth <- switch(GalParams$tstep,
                    "daily" = ".DLY",
                    "monthly" = ".MON"
                    )
    vardir <- switch(GalParams$var,
                      "total" = ".olr",
                      "anomaly" = ".anom"
                    )
    dlpath <- paste(dlpath, tspth, ".combined", vardir, sep = '/')

    rlon <- unlist(GalParams$bbox[c('minlon', 'maxlon')])
    rlon <- paste(c('X', rlon, 'RANGE'), collapse = "/")
    rlat <- unlist(GalParams$bbox[c('minlat', 'maxlat')])
    rlat <- paste(c('Y', rlat, 'RANGE'), collapse = "/")

    rdate <- iridl.format.date(GalParams$tstep, GalParams$date.range)
    urls <- urltools::url_encode(paste0("(", rdate$dates, ")"))
    urls <- paste0("T", "/", urls, "/", "VALUE")

    urls <- paste(dlpath, rlon, rlat, urls, 'data.nc', sep = "/")

    #########
    data.name <- paste0("NOAA-OLR_CBO_", GalParams$tstep)
    outdir <- file.path(GalParams$dir2save, data.name, toupper(GalParams$var))
    dir.create(outdir, showWarnings = FALSE, recursive = TRUE)
    destfiles <- file.path(outdir, paste0("olr_", GalParams$var, "_", rdate$out, ".nc"))

    ret <- cdt.download.data(urls, destfiles, destfiles, nbfile, GUI,
                             verbose, data.name, iridl.download.data)

    return(ret)
}
