
tamsat.download.iridl <- function(GalParams, nbfile = 3, GUI = TRUE, verbose = TRUE){
    info <- tamsat.info.iridl(GalParams)
    if(is.null(info)) return(-3)

    if(GalParams$rfe.src == "tamsatv3.1-af"){
        dir_name <- "TAMSATv3.1"
        file_name <- "tamsatv3.1"
    }else{
        dir_name <- "TAMSATv3"
        file_name <- "tamsatv3"
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

tamsat.coverage.iridl <- function(GalParams){
    info <- tamsat.info.iridl(GalParams)
    if(is.null(info)) return(-3)

    if(GalParams$rfe.src == "tamsatv3.1-af"){
        name <- "TAMSAT version 3.1"
        start_date <- switch(GalParams$tstep,
                             "daily" = "19830101",
                             "dekadal" = "1983011",
                             "monthly" = "198301"
                            )
    }else{
        name <- "TAMSAT version 3.0"
        start_date <- switch(GalParams$tstep,
                             "daily" = "19830111",
                             "dekadal" = "1983012",
                             "monthly" = "198301"
                            )
    }
    end_date <- iridl.get.end_date(info$baseurl, GalParams$tstep)

    list(name = name, timestep = GalParams$tstep,
         start = start_date, end = end_date)
}

tamsat.info.iridl <- function(GalParams){
    iridl_tamsat <- "https://iridl.ldeo.columbia.edu/SOURCES/.Reading/.Meteorology/.TAMSAT/.TARCAT"
    if(GalParams$rfe.src == "tamsatv3.1-af"){
        vardir <- switch(GalParams$tstep,
                         "daily" = ".v3p1/.daily/.rfe_filled",
                         "dekadal" = ".v3p1/.dekadal/.rfe_filled",
                         "monthly" = ".v3p1/.monthly/.rfe_filled"
                        )
    }else if(GalParams$rfe.src == "tamsatv3-af"){
        vardir <- paste0(".v3p0/.", GalParams$tstep, "/.rfe")
    }else return(NULL)

    url <- file.path(iridl_tamsat, vardir)

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

######

tamsat.download.reading <- function(GalParams, nbfile = 3, GUI = TRUE, verbose = TRUE){
    info <- tamsat.info.reading(GalParams)
    if(is.null(info)) return(-3)

    if(GalParams$rfe.src == "tamsatv3.1-af"){
        dir_name <- "TAMSATv3.1"
        version <- "3.1"
    }else{
        dir_name <- "TAMSATv3"
        version <- "3.0"
    }

    data.name <- paste0(dir_name, "_", GalParams$tstep)
    outdir <- file.path(GalParams$dir2save, data.name)
    extrdir <- file.path(outdir, "Extracted")
    if(!dir.exists(extrdir))
        dir.create(extrdir, showWarnings = FALSE, recursive = TRUE)
    origdir <- file.path(outdir, "Data_Africa")
    if(!dir.exists(origdir))
        dir.create(origdir, showWarnings = FALSE, recursive = TRUE)

    destfiles <- file.path(origdir, info$ncfiles)
    ncfiles <- file.path(extrdir, info$ncfiles)

    ret <- cdt.download.data(info$urls, destfiles, ncfiles, nbfile, GUI,
                             verbose, data.name, tamsat.download.data,
                             bbox = GalParams$bbox, version = version)

    return(ret)
}

tamsat.coverage.reading <- function(GalParams){
    if(GalParams$tstep == 'hourly'){
        timestep <- paste(GalParams$minhour, GalParams$tstep)
    }else{
        timestep <- GalParams$tstep
    }
    out <- list(name = GalParams$rfe.src, timestep = timestep)
    info <- tamsat.info.reading(GalParams)
    if(is.null(info)) return(out)

    end_date <- tamsat.reading.end_date(info$baseurl, info$fileformat)
    if(is.null(end_date)) return(out)
    out$end <- end_date

    if(GalParams$rfe.src == "tamsatv3.1-af"){
        out$start <- switch(GalParams$tstep,
                            "daily" = "19830101",
                            "pentad" = "1983011",
                            "dekadal" = "1983011",
                            "monthly" = "198301"
                           )
        out$name <- "TAMSAT version 3.1"
    }else{
        out$start <- switch(GalParams$tstep,
                            "daily" = "19830111",
                            "pentad" = "1983013",
                            "dekadal" = "1983012",
                            "monthly" = "198301"
                           )
        out$name <- "TAMSAT version 3.0"
    }

    return(out)
}

tamsat.info.reading <- function(GalParams){
    rdate <- table.format.date.time(GalParams$tstep, GalParams$date.range)
    if(is.null(rdate)) return(NULL)

    timestep <- switch(GalParams$tstep, "pentad" = "pentadal", GalParams$tstep)
    tamsat_url <- "https://data.tamsat.org.uk/public_data"

    if(GalParams$rfe.src == "tamsatv3.1-af"){
        baseurl <- paste0(tamsat_url, "/data/v3.1/", timestep)
        fileformat <- switch(timestep,
                             "daily" = "rfe%s_%s_%s.v3.1.nc",
                             "pentadal" = "rfe%s_%s-pt%s.v3.1.nc",
                             "dekadal" = "rfe%s_%s-dk%s.v3.1.nc",
                             "monthly" = "rfe%s_%s.v3.1.nc"
                            )
    }else if(GalParams$rfe.src == "tamsatv3-af"){
        baseurl <- paste0(tamsat_url, "/TAMSAT3")
        fileformat <- switch(timestep,
                             "daily" = "rfe%s_%s_%s.v3.nc",
                             "pentadal" = "rfe%s_%s-pt%s.v3.nc",
                             "dekadal" = "rfe%s_%s-dk%s.v3.nc",
                             "monthly" = "rfe%s_%s.v3.nc"
                            )
    }else return(NULL)

    ncfiles0 <- sprintf(fileformat, rdate[, 1], rdate[, 2], rdate[, 3])
    urls <- file.path(baseurl, rdate[, 1], rdate[, 2], ncfiles0)

    list(baseurl = baseurl, fileformat = fileformat,
         ncfiles = ncfiles0, urls = urls)
}

########

tamsat.reading.end_date <- function(data_url, fileformat){
    year <- tamsat.reading.dates(data_url, "directory")
    if(is.null(year)) return(NULL)

    end_year <- year[length(year)]
    url_mo2 <- file.path(data_url, end_year)
    end_mon <- tamsat.reading.dates(url_mo2, "directory")
    if(is.null(end_mon)) return(NULL)
    end_mon1 <- end_mon[length(end_mon)]
    url_file2 <- file.path(url_mo2, end_mon1)
    end_file <- tamsat.reading.dates(url_file2, "file")
    if(is.null(end_file)) return(NULL)

    daty0 <- as.Date(paste(end_year, end_mon1, '15', sep = '-'))
    daty0 <- addMonths(daty0, -1)
    url_file0 <- file.path(data_url, format(daty0, '%Y/%m'))
    end_file0 <- tamsat.reading.dates(url_file0, "file")
    if(is.null(end_file0)) return(NULL)

    end_file <- c(end_file0, end_file)

    #####
    frmt <- fileformat
    reg <- gregexpr("[^a-zA-Z0-9\\%s]", frmt)
    if(reg[[1]][1] != -1){
        reg <- regmatches(frmt, reg)[[1]]
        reg <- reg[!duplicated(reg)]
        for(p in reg){
            pt0 <- paste0('\\', p)
            pt1 <- paste0('\\', pt0)
            frmt <- gsub(pt0, pt1, frmt)
        }
    }
    pattern <- gsub("%s", "[0-9]{1,}", frmt)

    end_file <- end_file[grepl(pattern, end_file)]
    if(length(end_file) == 0) return(NULL)
    end_d <- extract_filename_dates(end_file, fileformat)
    end_d <- sort(end_d)[length(end_d)]

    return(end_d)
}

tamsat.reading.dates <- function(url, type = "directory"){
    ret <- httr::GET(url)
    if(httr::status_code(ret) != 200){
        Insert.Messages.httr(ret)
        return(NULL)
    }
    ret <- httr::content(ret)
    name <- sapply(ret, '[[', 'name')
    rtype <- sapply(ret, '[[', 'type')
    name <- name[rtype == type]
    if(type == "directory"){
        name <- sort(name[!grepl('[^[:digit:]]', name)])
    }

    return(name)
}

#################################################################################

tamsat.download.data <- function(lnk, dest, ncfl, bbox, version, GUI = TRUE){
    xx <- basename(dest)

    link.exist <- try(readLines(lnk, 1), silent = TRUE)
    if(inherits(link.exist, "try-error")){
        msg <- gsub('[\r\n]', '', link.exist)
        Insert.Messages.Out(msg, TRUE, "e", GUI)
        return(xx)
    }

    handle <- curl::new_handle()
    curl::handle_setopt(handle, ssl_verifyhost = 0, ssl_verifypeer = 0)

    dc <- try(curl::curl_download(lnk, dest, handle = handle), silent = TRUE)
    if(!inherits(dc, "try-error")){
        ret <- tamsat.extract.data(dest, ncfl, bbox, version)
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
