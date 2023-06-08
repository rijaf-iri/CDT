
## toexport
tamsat3.0.download.iridl <- function(GalParams, nbfile = 3, GUI = TRUE, verbose = TRUE){
    dlpath <- "https://iridl.ldeo.columbia.edu/SOURCES/.Reading/.Meteorology/.TAMSAT/.TARCAT/.v3p0"

    vartime <- paste0(".", GalParams$tstep, "/.rfe")
    rlon <- unlist(GalParams$bbox[c('minlon', 'maxlon')])
    rlon <- paste(c('X', rlon, 'RANGE'), collapse = "/")
    rlat <- unlist(GalParams$bbox[c('minlat', 'maxlat')])
    rlat <- paste(c('Y', rlat, 'RANGE'), collapse = "/")

    rdate <- iridl.format.date(GalParams$tstep, GalParams$date.range)
    urls <- urltools::url_encode(paste0("(", rdate$dates, ")"))

    urls <- paste0("T", "/", urls, "/", "VALUE")
    urls <- paste(dlpath, vartime, rlon, rlat, urls, 'data.nc', sep = "/")

    #########
    data.name <- paste0("TAMSATv3_", GalParams$tstep)
    outdir <- file.path(GalParams$dir2save, data.name)
    dir.create(outdir, showWarnings = FALSE, recursive = TRUE)
    destfiles <- file.path(outdir, paste0("tamsatv3_", rdate$out, ".nc"))

    ret <- cdt.download.data(urls, destfiles, destfiles, nbfile, GUI,
                             verbose, data.name, iridl.download.data)
    return(ret)
}

## toexport
tamsat3.1.download.iridl <- function(GalParams, nbfile = 3, GUI = TRUE, verbose = TRUE){
    dlpath <- "https://iridl.ldeo.columbia.edu/SOURCES/.Reading/.Meteorology/.TAMSAT/.TARCAT/.v3p1"
    vartime <- switch(GalParams$tstep,
                      "daily" = ".daily/.rfe_filled",
                      "dekadal" = ".dekadal/.rfe_filled",
                      "monthly" = ".monthly/.rfe_filled"
                     )

    rlon <- unlist(GalParams$bbox[c('minlon', 'maxlon')])
    rlon <- paste(c('X', rlon, 'RANGE'), collapse = "/")
    rlat <- unlist(GalParams$bbox[c('minlat', 'maxlat')])
    rlat <- paste(c('Y', rlat, 'RANGE'), collapse = "/")

    rdate <- iridl.format.date(GalParams$tstep, GalParams$date.range)
    urls <- urltools::url_encode(paste0("(", rdate$dates, ")"))
    urls <- paste("T", urls, "VALUE", sep = "/")
    
    urls <- paste(dlpath, vartime, rlon, rlat, urls, 'data.nc', sep = "/")

    #########
    data.name <- paste0("TAMSATv3.1_", GalParams$tstep)
    outdir <- file.path(GalParams$dir2save, data.name)
    dir.create(outdir, showWarnings = FALSE, recursive = TRUE)
    destfiles <- file.path(outdir, paste0("tamsatv3.1_", rdate$out, ".nc"))

    ret <- cdt.download.data(urls, destfiles, destfiles, nbfile, GUI,
                             verbose, data.name, iridl.download.data)
    return(ret)
}

## toexport
tamsatv3.1.download.reading <- function(GalParams, nbfile = 3, GUI = TRUE, verbose = TRUE){
    # baseurl <- "https://www.tamsat.org.uk/public_data/data/v3.1"
    baseurl <- "http://www.tamsat.org.uk/public_data/data/v3.1"
    fileformat <- switch(GalParams$tstep,
                         "daily" = "rfe%s_%s_%s.v3.1.nc", 
                         "pentad" = "rfe%s_%s-pt%s.v3.1.nc",
                         "dekadal" = "rfe%s_%s-dk%s.v3.1.nc",
                         "monthly" = "rfe%s_%s.v3.1.nc"
                        )
    timestep <- switch(GalParams$tstep, "pentad" = "pentadal", GalParams$tstep)
    rdate <- table.format.date.time(GalParams$tstep, GalParams$date.range)
    ncfiles0 <- sprintf(fileformat, rdate[, 1], rdate[, 2], rdate[, 3])
    urls <- file.path(baseurl, timestep, rdate[, 1], rdate[, 2], ncfiles0)

    #########
    data.name <- paste0("TAMSATv3.1_", GalParams$tstep)
    outdir <- file.path(GalParams$dir2save, data.name)
    extrdir <- file.path(outdir, "Extracted")
    dir.create(extrdir, showWarnings = FALSE, recursive = TRUE)
    origdir <- file.path(outdir, "Data_Africa")
    dir.create(origdir, showWarnings = FALSE, recursive = TRUE)
    destfiles <- file.path(origdir, ncfiles0)
    ncfiles <- file.path(extrdir, ncfiles0)

    ret <- cdt.download.data(urls, destfiles, ncfiles, nbfile, GUI, verbose,
                             data.name, tamsat.download.data,
                             bbox = GalParams$bbox, version = "3.1")

    return(ret)
}

tamsatv3.0.download.reading <- function(GalParams, nbfile = 3, GUI = TRUE, verbose = TRUE){
    # baseurl <- "https://www.tamsat.org.uk/public_data/TAMSAT3"
    baseurl <- "http://www.tamsat.org.uk/public_data/TAMSAT3"
    fileformat <- switch(GalParams$tstep,
                         "daily" = "rfe%s_%s_%s.v3.nc", 
                         "pentad" = "rfe%s_%s-pt%s.v3.nc",
                         "dekadal" = "rfe%s_%s-dk%s.v3.nc",
                         "monthly" = "rfe%s_%s.v3.nc"
                        )
    rdate <- table.format.date.time(GalParams$tstep, GalParams$date.range)
    ncfiles0 <- sprintf(fileformat, rdate[, 1], rdate[, 2], rdate[, 3])
    urls <- file.path(baseurl, rdate[, 1], rdate[, 2], ncfiles0)

    #########
    data.name <- paste0("TAMSATv3_", GalParams$tstep)
    outdir <- file.path(GalParams$dir2save, data.name)
    extrdir <- file.path(outdir, "Extracted")
    dir.create(extrdir, showWarnings = FALSE, recursive = TRUE)
    origdir <- file.path(outdir, "Data_Africa")
    dir.create(origdir, showWarnings = FALSE, recursive = TRUE)
    destfiles <- file.path(origdir, ncfiles0)
    ncfiles <- file.path(extrdir, ncfiles0)

    ret <- cdt.download.data(urls, destfiles, ncfiles, nbfile, GUI, verbose,
                             data.name, tamsat.download.data,
                             bbox = GalParams$bbox, version = "3.0")

    return(ret)
}

#################################################################################

tamsat.download.data <- function(lnk, dest, ncfl, bbox, version){
    xx <- basename(dest)

    link.exist <- try(readLines(lnk, 1), silent = TRUE)
    if(inherits(link.exist, "try-error")) return(xx)

    dc <- try(curl::curl_download(lnk, dest), silent = TRUE)
    if(!inherits(dc, "try-error")){
        ret <- tamsat.extract.data(dest, ncfl, bbox, version)
        if(ret == 0){
            xx <- NULL
        }else{
            unlink(dest)
        }
    }
    return(xx)
}

tamsat.extract.data <- function(dest, ncfl, bbox, version){
    nc <- try(ncdf4::nc_open(dest), silent = TRUE)
    ret <- 1
    if(!inherits(nc, "try-error")){
        lon <- nc$var[['rfe']]$dim[[1]]$vals
        lat <- nc$var[['rfe']]$dim[[2]]$vals
        ix <- lon >= bbox$minlon & lon <= bbox$maxlon
        iy <- lat >= bbox$minlat & lat <= bbox$maxlat
        start <- c(which(ix)[1], which(iy)[1], 1)
        count <- c(diff(range(which(ix))) + 1, diff(range(which(iy))) + 1, 1)
        val <- ncdf4::ncvar_get(nc, "rfe", start, count)
        if(version == "3.1")
            if(all(is.na(val)))
                val <- ncdf4::ncvar_get(nc, "rfe_filled", start, count)
        ncdf4::nc_close(nc)

        lon <- lon[ix]
        lat <- lat[iy]
        oy <- order(lat)
        val <- val[, oy]
        lat <- lat[oy]

        dx <- ncdf4::ncdim_def("lon", "degreeE", lon, longname = "Longitude")
        dy <- ncdf4::ncdim_def("lat", "degreeN", lat, longname = "Latitude")
        missval <- -99
        ncgrd <- ncdf4::ncvar_def("rfe", "mm", list(dx, dy), missval,
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
