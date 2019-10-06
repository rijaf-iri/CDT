
## toexport
rfev2_af.download.iridl <- function(GalParams, nbfile = 3, GUI = TRUE, verbose = TRUE){
    dlpath <- "https://iridl.ldeo.columbia.edu/SOURCES/.NOAA/.NCEP/.CPC/.FEWS/.Africa"
    vardir <- switch(GalParams$tstep,
                      "daily" = ".DAILY/.RFEv2/.est_prcp",
                      "dekadal" = ".TEN-DAY/.RFEv2/.est_prcp"
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
    data.name <- paste0("RFEv2_", GalParams$tstep)
    outdir <- file.path(GalParams$dir2save, data.name)
    dir.create(outdir, showWarnings = FALSE, recursive = TRUE)
    destfiles <- file.path(outdir, paste0("rfev2_", rdate$out, ".nc"))

    ret <- cdt.download.data(urls, destfiles, destfiles, nbfile, GUI,
                             verbose, data.name, iridl.download.data)

    return(ret)
}

## toexport
rfev2_sa.download.iridl <- function(GalParams, nbfile = 3, GUI = TRUE, verbose = TRUE){
    dlpath <- "http://iridl.ldeo.columbia.edu/SOURCES/.NOAA/.NCEP/.CPC/.FEWS/.SAsia/.RFEv2/.DAILY/.est_prcp"

    rlon <- unlist(GalParams$bbox[c('minlon', 'maxlon')])
    rlon <- paste(c('X', rlon, 'RANGE'), collapse = "/")
    rlat <- unlist(GalParams$bbox[c('minlat', 'maxlat')])
    rlat <- paste(c('Y', rlat, 'RANGE'), collapse = "/")

    aggr <- if(GalParams$tstep == "dekadal") "1.0/dekadalAverage/T/differential_mul" else NULL

    rdate <- iridl.format.date(GalParams$tstep, GalParams$date.range)
    urls <- urltools::url_encode(paste0("(", rdate$dates, ")"))
    urls <- paste0("T", "/", urls, "/", "VALUE")

    urls <- paste(dlpath, rlon, rlat, aggr, urls, 'data.nc', sep = "/")

    #########
    data.name <- paste0("RFEv2_", GalParams$tstep)
    outdir <- file.path(GalParams$dir2save, data.name)
    dir.create(outdir, showWarnings = FALSE, recursive = TRUE)
    destfiles <- file.path(outdir, paste0("rfev2_", rdate$out, ".nc"))

    ret <- cdt.download.data(urls, destfiles, destfiles, nbfile, GUI,
                             verbose, data.name, iridl.download.data)

    return(ret)
}

## toexport
rfev2_af.download.cpc.noaa.tif <- function(GalParams, nbfile = 3, GUI = TRUE, verbose = TRUE){
    rdate <- table.format.date.time(GalParams$tstep, GalParams$date.range)
    ftp.cpc <- "ftp://ftp.cpc.ncep.noaa.gov/fews/fewsdata/africa/rfe2/geotiff"
    filename <- sprintf("africa_rfe.%s%s%s.tif.zip", rdate[, 1], rdate[, 2], rdate[, 3])
    urls <- file.path(ftp.cpc, filename)
    ncfiles <- sprintf("rfev2_%s%s%s.nc", rdate[, 1], rdate[, 2], rdate[, 3])

    #########
    data.name <- paste0("RFEv2_", GalParams$tstep)
    outdir <- file.path(GalParams$dir2save, data.name)
    extrdir <- file.path(outdir, "Extracted")
    dir.create(extrdir, showWarnings = FALSE, recursive = TRUE)
    origdir <- file.path(outdir, "Data_Africa")
    dir.create(origdir, showWarnings = FALSE, recursive = TRUE)
    destfiles <- file.path(origdir, filename)
    ncfiles <- file.path(extrdir, ncfiles)

    ret <- cdt.download.data(urls, destfiles, ncfiles, nbfile, GUI,
                             verbose, data.name, fews.download.data.tif,
                             bbox = GalParams$bbox, arc = FALSE)

    return(ret)
}

## toexport
rfev2_af.download.cpc.noaa.bin <- function(GalParams, nbfile = 3, GUI = TRUE, verbose = TRUE){
    rdate <- table.format.date.time(GalParams$tstep, GalParams$date.range)
    ftp.cpc <- "ftp://ftp.cpc.ncep.noaa.gov/fews/fewsdata/africa/rfe2/bin"
    filename <- sprintf("all_products.bin.%s%s%s.gz", rdate[, 1], rdate[, 2], rdate[, 3])
    urls <- file.path(ftp.cpc, filename)
    ncfiles <- sprintf("rfev2_%s%s%s.nc", rdate[, 1], rdate[, 2], rdate[, 3])

    #########
    data.name <- paste0("RFEv2_", GalParams$tstep)
    outdir <- file.path(GalParams$dir2save, data.name)
    extrdir <- file.path(outdir, "Extracted")
    dir.create(extrdir, showWarnings = FALSE, recursive = TRUE)
    origdir <- file.path(outdir, "Data_Africa")
    dir.create(origdir, showWarnings = FALSE, recursive = TRUE)
    destfiles <- file.path(origdir, filename)
    ncfiles <- file.path(extrdir, ncfiles)

    ret <- cdt.download.data(urls, destfiles, ncfiles, nbfile, GUI,
                             verbose, data.name, fews.download.data.bin,
                             bbox = GalParams$bbox, region = "africa")

    return(ret)
}

## toexport
rfev2_sa.download.cpc.noaa.bin <- function(GalParams, nbfile = 3, GUI = TRUE, verbose = TRUE){
    rdate <- table.format.date.time(GalParams$tstep, GalParams$date.range)
    ftp.cpc <- "ftp://ftp.cpc.ncep.noaa.gov/fews/S.Asia/data"
    filename <- sprintf("cpc_rfe_v2.0_sa_dly.bin.%s%s%s.gz", rdate[, 1], rdate[, 2], rdate[, 3])
    urls <- file.path(ftp.cpc, filename)
    ncfiles <- sprintf("rfev2_%s%s%s.nc", rdate[, 1], rdate[, 2], rdate[, 3])

    #########
    data.name <- paste0("RFEv2_", GalParams$tstep)
    outdir <- file.path(GalParams$dir2save, data.name)
    extrdir <- file.path(outdir, "Extracted")
    dir.create(extrdir, showWarnings = FALSE, recursive = TRUE)
    origdir <- file.path(outdir, "Data_SouthAsia")
    dir.create(origdir, showWarnings = FALSE, recursive = TRUE)
    destfiles <- file.path(origdir, filename)
    ncfiles <- file.path(extrdir, ncfiles)

    ret <- cdt.download.data(urls, destfiles, ncfiles, nbfile, GUI,
                             verbose, data.name, fews.download.data.bin,
                             bbox = GalParams$bbox, region = "southasia")

    return(ret)
}
