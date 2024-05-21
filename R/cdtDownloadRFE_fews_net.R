
fews.download.iridl <- function(GalParams, nbfile = 3, GUI = TRUE, verbose = TRUE){
    info <- fews.info.iridl(GalParams)
    if(is.null(info)) return(-3)

    bbx <- unlist(GalParams$bbox)
    if(info$region == "africa"){
        africa <- intersect_rectangles(bbx, c(-20, 55, -40, 40))
        if(!africa) return(-2)
    }else{
        southasia <- intersect_rectangles(bbx, c(70, 110, 5, 35))
        if(!southasia) return(-2)
    }

    data.name <- paste0(info$dirname, "_", GalParams$tstep)
    outdir <- file.path(GalParams$dir2save, data.name)
    if(!dir.exists(outdir))
        dir.create(outdir, showWarnings = FALSE, recursive = TRUE)
    destfiles <- file.path(outdir, info$ncfiles)
    urls <- paste0(info$baseurl, '/', info$pars)

    ret <- cdt.download.data(urls, destfiles, destfiles, nbfile, GUI,
                             verbose, data.name, iridl.download.data)
    return(ret)
}

fews.coverage.iridl <- function(GalParams){
    info <- fews.info.iridl(GalParams)
    if(is.null(info)) return(-3)

    if(GalParams$rfe.src == "arc2-af"){
        start_date <- switch(GalParams$tstep,
                             "daily" = "19830101",
                             "dekadal" = "1983011",
                             "monthly" = "198301"
                            )
        rfename <- "FEWS-NET ARC2"
    }else if(GalParams$rfe.src == "rfev2-af"){
        start_date <- switch(GalParams$tstep,
                             "daily" = "20001031",
                             "dekadal" = "1999121"
                            )
        rfename <- "FEWS-NET RFEv2"
    }else if(GalParams$rfe.src == "rfev2-sa"){
        start_date <- switch(GalParams$tstep,
                             "daily" = "20010501",
                             "dekadal" = "2001051"
                            )

        rfename <- "FEWS-NET RFEv2"
    }

    end_date <- iridl.get.end_date(info$baseurl, GalParams$tstep)

    list(name = rfename, timestep = GalParams$tstep,
         start = start_date, end = end_date)
}

fews.info.iridl <- function(GalParams){
    rdate <- iridl.format.date(GalParams$tstep, GalParams$date.range)
    if(is.null(rdate)) return(NULL)

    iridl_fews <- "https://iridl.ldeo.columbia.edu/SOURCES/.NOAA/.NCEP/.CPC/.FEWS"

    if(GalParams$rfe.src == "arc2-af"){
        vardir <- switch(GalParams$tstep,
                          "daily" = ".Africa/.DAILY/.ARC2/.daily/.est_prcp",
                          "dekadal" = ".Africa/.TEN-DAY/.ARC2/.est_prcp",
                          "monthly" = ".Africa/.DAILY/.ARC2/.monthly/.est_prcp"
                        )
        ncfiles <- paste0("arc2_", rdate$out, ".nc")
        dirname <- "ARC2"
        region <- "africa"
    }else if(GalParams$rfe.src == "rfev2-af"){
        vardir <- switch(GalParams$tstep,
                         "daily" = ".Africa/.DAILY/.RFEv2/.est_prcp",
                         "dekadal" = ".Africa/.TEN-DAY/.RFEv2/.est_prcp"
                        )
        ncfiles <- paste0("rfev2_", rdate$out, ".nc")
        dirname <- "RFEv2"
        region <- "africa"
    }else if(GalParams$rfe.src == "rfev2-sa"){
        vardir <- switch(GalParams$tstep,
                         "daily" = ".SAsia/.RFEv2/.DAILY/.est_prcp",
                         "dekadal" = ".SAsia/.RFEv2/.DAILY/.est_prcp/1.0/dekadalAverage/T/differential_mul"
                        )
        ncfiles <- paste0("rfev2_", rdate$out, ".nc")
        dirname <- "RFEv2"
        region <- "south-asia"
    }else return(NULL)

    url <- file.path(iridl_fews, vardir)

    rlon <- unlist(GalParams$bbox[c('minlon', 'maxlon')])
    rlon <- paste(c('X', rlon, 'RANGE'), collapse = "/")
    rlat <- unlist(GalParams$bbox[c('minlat', 'maxlat')])
    rlat <- paste(c('Y', rlat, 'RANGE'), collapse = "/")
    rtime <- urltools::url_encode(paste0("(", rdate$dates, ")"))
    rtime <- paste0("T", "/", rtime, "/", "VALUE")
    pars <- paste(rlon, rlat, rtime, 'data.nc', sep = "/")

    info <- list(baseurl = url, pars = pars, region = region,
                 ncfiles = ncfiles, dirname = dirname)
    return(info)
}

fews.download.cpc.noaa <- function(GalParams, nbfile = 3, GUI = TRUE, verbose = TRUE){
    info <- fews.info.cpc.noaa(GalParams)
    if(is.null(info)) return(-3)

    bbx <- unlist(GalParams$bbox)
    if(info$region == "africa"){
        africa <- intersect_rectangles(bbx, c(-20, 55, -40, 40))
        if(!africa) return(-2)
    }else{
        southasia <- intersect_rectangles(bbx, c(70, 110, 5, 35))
        if(!southasia) return(-2)
    }

    data.name <- paste0(info$dirname, "_", GalParams$tstep)
    outdir <- file.path(GalParams$dir2save, data.name)
    extrdir <- file.path(outdir, "Extracted")
    if(!dir.exists(extrdir))
        dir.create(extrdir, showWarnings = FALSE, recursive = TRUE)
    origdir <- paste0("Data_", info$region, "_", info$type)
    origdir <- file.path(outdir, origdir)
    if(!dir.exists(origdir))
        dir.create(origdir, showWarnings = FALSE, recursive = TRUE)
    destfiles <- file.path(origdir, info$filename)
    ncfiles <- file.path(extrdir, info$ncfiles)

    ret <- cdt.download.data(info$urls, destfiles, ncfiles, nbfile, GUI,
                             verbose, data.name, fews.download.data,
                             bbox = GalParams$bbox, region = info$region,
                             type = info$type, arc = info$arc)
    return(ret)
}

fews.coverage.cpc.noaa <- function(GalParams){
    rfename <- switch(GalParams$rfe.src,
                      "arc2-af" = "FEWS-NET ARC2",
                      "rfev2-af" = "FEWS-NET RFEv2",
                      "rfev2-sa" = "FEWS-NET RFEv2"
                    )
    out <- list(name = rfename, timestep = GalParams$tstep)

    info <- fews.info.cpc.noaa(GalParams)
    if(is.null(info)) return(out)

    url <- file.path(info$baseurl, info$dirpath)
    tmp <- fews.cpc.noaa.dates(url, info$fileformat)
    if(is.null(tmp)) return(out)

    out$end <- tmp[length(tmp)]
    out$start <- switch(GalParams$rfe.src,
                        "arc2-af" = "19830101",
                        "rfev2-af" = "20010101",
                        "rfev2-sa" = "20010501"
                        )
    return(out)
}

fews.info.cpc.noaa <- function(GalParams){
    if(GalParams$rfe.src == "arc2-af"){
        if(GalParams$fewsAFdaily.type == "bin"){
            dirpath <- "fewsdata/africa/arc2/bin"
            fileformat <- "daily_clim.bin.%s%s%s.gz"
            type <- "bin"
        }else if(GalParams$fewsAFdaily.type == "tif"){
            dirpath <- "fewsdata/africa/arc2/geotiff"
            fileformat <- "africa_arc.%s%s%s.tif.zip"
            type <- "tif"
        }
        ncformat <- "arc2_%s%s%s.nc"
        dirname <- "ARC2"
        region <- "africa"
        arc <- TRUE
    }else if(GalParams$rfe.src == "rfev2-af"){
        if(GalParams$fewsAFdaily.type == "bin"){
            dirpath <- "fewsdata/africa/rfe2/bin"
            fileformat <- "all_products.bin.%s%s%s.gz"
            type <- "bin"
        }else if(GalParams$fewsAFdaily.type == "tif"){
            dirpath <- "fewsdata/africa/rfe2/geotiff"
            fileformat <- "africa_rfe.%s%s%s.tif.zip"
            type <- "tif"
        }
        ncformat <- "rfev2_%s%s%s.nc"
        dirname <- "RFEv2"
        region <- "africa"
        arc <- FALSE
    }else if(GalParams$rfe.src == "rfev2-sa"){
        dirpath <- "S.Asia/data"
        fileformat <- "cpc_rfe_v2.0_sa_dly.bin.%s%s%s.gz"
        type <- "bin"
        ncformat <- "rfev2_%s%s%s.nc"
        dirname <- "RFEv2"
        region <- "south-asia"
        arc <- FALSE
    }else return(NULL)

    rdate <- table.format.date.time(GalParams$tstep, GalParams$date.range)
    if(is.null(rdate)) return(NULL)

    ftp.fews <- "https://ftp.cpc.ncep.noaa.gov/fews"
    filename <- sprintf(fileformat, rdate[, 1], rdate[, 2], rdate[, 3])
    urls <- file.path(ftp.fews, dirpath, filename)
    ncfiles <- sprintf(ncformat, rdate[, 1], rdate[, 2], rdate[, 3])

    list(dirpath = dirpath, fileformat = fileformat, type = type,
         dirname = dirname, ncfiles = ncfiles, filename = filename,
         urls = urls, region = region, baseurl = ftp.fews, arc = arc)
}

fews.cpc.noaa.table <- function(url){
    ret <- httr::GET(url)
    if(httr::status_code(ret) != 200){
        Insert.Messages.httr(ret)
        return(NULL)
    }
    ret <- httr::content(ret)

    tmp <- rvest::html_table(ret, fill = TRUE)[[1]]
    tmp <- as.data.frame(tmp[-(1:2), ])
    tmp[tmp[, 1] != "", 1]
}

fews.cpc.noaa.dates <- function(url, fileformat){
    tmp <- fews.cpc.noaa.table(url)
    if(length(tmp) == 0) return(NULL)

    ret <- extract_filename_dates(tmp, fileformat)
    if(is.null(ret)) return(NULL)

    ret <- ret[ret != ""]
    if(length(ret) == 0) return(NULL)
    ret <- sort(ret)

    return(ret)
}

#################################################################################

fews.download.data <- function(lnk, dest, ncfl, bbox, region,
                               type, arc, GUI = TRUE)
{
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
        tmpf <- fews.decompress.file(dest, tmpdir)

        if(type == "bin"){
            ret <- fews.extract.bin(tmpf, ncfl, bbox, region, arc)
        }else{
            ret <- fews.extract.tif(tmpf, ncfl, bbox, arc)
        }

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

fews.decompress.file <- function(dest, tmpdir){
    if(grepl("\\.gz$", dest, ignore.case = TRUE)){
        tmpf <- file.path(tmpdir, gsub("\\.gz$", "", basename(dest)))
        R.utils::gunzip(dest, tmpf, remove = FALSE, overwrite = TRUE)
    }else if(grepl("\\.zip$", dest, ignore.case = TRUE)){
        tmpf <- file.path(tmpdir, gsub("\\.zip$", "", basename(dest)))
        utils::unzip(dest, exdir = tmpdir)
    }

    return(tmpf)
}

fews.extract.tif <- function(tmpf, ncfl, bbox, arc){
    xr <- try(raster::raster(tmpf), silent = TRUE)
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
        if(arc){
            name <- "arc2"
            longname <- "Africa Rainfall Climatology version 2"
        }else{
            name <- "rfev2"
            longname <- "Estimated Precipitation RFEv2"
        }

        ncgrd <- ncdf4::ncvar_def(name, "mm", list(dx, dy), missval,
                                  longname, "float", compression = 9)
        z[is.na(z)] <- missval

        nc <- ncdf4::nc_create(ncfl, ncgrd)
        ncdf4::ncvar_put(nc, ncgrd, z)
        ncdf4::nc_close(nc)

        ret <- 0
    }
    return(ret)
}

fews.extract.bin <- function(tmpf, ncfl, bbox, region, arc){
    pars <- switch(region,
                "africa" = list(nx = 751, ny = 801,
                                x = seq(-20, length.out = 751, by = 0.1),
                                y = seq(-40, length.out = 801, by = 0.1)
                              ),
                "south-asia" = list(nx = 401, ny = 301,
                                    x = seq(70, length.out = 401, by = 0.1),
                                    y = seq(5, length.out = 301, by = 0.1)
                                )
            )

    con <- file(tmpf, open = "rb")
    on.exit(close(con))
    val <- try(readBin(con, numeric(), pars$nx * pars$ny, 4, endian = "big"), silent = TRUE)
    ret <- 1
    if(!inherits(val, "try-error")){
        lon <- pars$x
        lat <- pars$y

        ix <- lon >= bbox$minlon & lon <= bbox$maxlon
        iy <- lat >= bbox$minlat & lat <= bbox$maxlat
        lon <- lon[ix]
        lat <- lat[iy]

        # val[val == -999] <- NA
        val[val < 0] <- NA
        val <- matrix(val, pars$nx, pars$ny)
        val <- val[ix, iy]

        dx <- ncdf4::ncdim_def("lon", "degreeE", lon, longname = "Longitude")
        dy <- ncdf4::ncdim_def("lat", "degreeN", lat, longname = "Latitude")
        missval <- -999

        if(arc){
            name <- "arc2"
            longname <- "Africa Rainfall Climatology version 2"
        }else{
            name <- "rfev2"
            longname <- "Estimated Precipitation RFEv2"
        }

        ncgrd <- ncdf4::ncvar_def(name, "mm", list(dx, dy), missval,
                                  longname, "float", compression = 9)
        val[is.na(val)] <- missval

        nc <- ncdf4::nc_create(ncfl, ncgrd)
        ncdf4::ncvar_put(nc, ncgrd, val)
        ncdf4::nc_close(nc)

        ret <- 0
    }

    return(ret)
}
