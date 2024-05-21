#' Connect Tidyverse API to Google Drive.
#'
#' Connect Tidyverse API to Google Drive.
#'
#' @param email email for the Google account.
#' 
#' @export

connect_tidyverse_api <- function(email){
    googledrive::drive_auth(email = email)
}

mswep.download.gdrive <- function(GalParams, nbfile = 3, GUI = TRUE, verbose = TRUE){
    info <- mswep.info.gdrive(GalParams)
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

    ret <- cdt.download.data(destfiles, destfiles, ncfiles, nbfile, GUI, verbose,
                             data.name, mswep.download.data, datadir = info$dirpath,
                             bbox = GalParams$bbox, email = GalParams$login$usr)

    return(ret)
}

mswep.coverage.gdrive <- function(GalParams){
    if(GalParams$tstep == 'hourly'){
        timestep <- paste(GalParams$minhour, GalParams$tstep)
    }else{
        timestep <- GalParams$tstep
    }
    out <- list(name = GalParams$rfe.src, timestep = timestep)

    info <- mswep.info.gdrive(GalParams)
    if(is.null(info)) return(out)

    dataname <- strsplit(info$dataname, '_')[[1]]
    out$name <- paste0(dataname, collapse = ' ')

    options(googledrive_quiet = TRUE)
    googledrive::drive_auth(email = GalParams$login$usr)
    mtype <- "mimeType='application/x-netcdf'"
    tmp1 <- try(googledrive::drive_ls(path = info$dirpath, n_max = 5,
                                      order_by = "name", q = mtype),
                silent = TRUE)
    if(inherits(tmp1, "try-error")){
        Insert.Messages.Out(tmp1[1], TRUE, "e", TRUE)
        return(out)
    }
    tmp2 <- try(googledrive::drive_ls(path = info$dirpath, n_max = 5,
                                      order_by = "name desc", q = mtype),
                silent = TRUE)
    if(inherits(tmp2, "try-error")){
        Insert.Messages.Out(tmp2[1], TRUE, "e", TRUE)
        return(out)
    }

    start_d <- extract_filename_dates(tmp1$name, info$fileformat)
    end_d <- extract_filename_dates(tmp2$name, info$fileformat)

    if(GalParams$tstep == "hourly"){
        start_d <- as.POSIXct(start_d, format = '%Y%j%H', tz = 'UTC')
        end_d <- as.POSIXct(end_d, format = '%Y%j%H', tz = 'UTC')
        outformat <- '%Y%m%d%H'
    }else if(GalParams$tstep == "daily"){
        start_d <- as.Date(start_d, '%Y%j')
        end_d <- as.Date(end_d, '%Y%j')
        outformat <- '%Y%m%d'
    }else if(GalParams$tstep == "monthly"){
        start_d <- as.Date(paste0(start_d, '01'), '%Y%m%d')
        end_d <- as.Date(paste0(end_d, '01'), '%Y%m%d')
        outformat <- '%Y%m'
    }else return(out)

    start_d <- start_d[!is.na(start_d)]
    if(length(start_d) == 0) return(out)
    end_d <- end_d[!is.na(end_d)]
    if(length(end_d) == 0) return(out)

    out$start <- format(min(start_d), outformat)
    out$end <- format(max(end_d), outformat)

    return(out)
}

mswep.info.gdrive <- function(GalParams){
    rdate <- table.format.date.time(GalParams$tstep, GalParams$date.range, GalParams$minhour)
    if(is.null(rdate)) return(NULL)

    if(GalParams$tstep == "hourly"){
        if(GalParams$minhour == 3){
            if(GalParams$rfe.src == "mswep.nrt-gb"){
                dataname <- 'MSWEP_NRT_v2.8'
                dirpath <- 'MSWEP_V280/NRT/3hourly'
                fileformat <- '%s%s.%s.nc'
                ncformat <- 'mswep_v2.8_%s%s%s%s.nc'
            }else if(GalParams$rfe.src == "mswep.past-gb"){
                dataname <- 'MSWEP_Past_v2.8'
                dirpath <- 'MSWEP_V280/Past/3hourly'
                fileformat <- '%s%s.%s.nc'
                ncformat <- 'mswep_v2.8_%s%s%s%s.nc'
            }else if(GalParams$rfe.src == "mswep.pastng-gb"){
                dataname <- 'MSWEP_Past_nogauge_v2.8'
                dirpath <- 'MSWEP_V280/Past_nogauge/3hourly'
                fileformat <- '%s%s.%s.nc'
                ncformat <- 'mswep_nogauge_v2.8_%s%s%s%s.nc'
            }else return(NULL)

            filename <- sprintf(fileformat, rdate[, 1], rdate[, 5], rdate[, 4])
            ncfiles <- sprintf(ncformat, rdate[, 1], rdate[, 2], rdate[, 3], rdate[, 4])
        }else return(NULL)
        data.tres <- paste0(GalParams$minhour, GalParams$tstep)
    }else if(GalParams$tstep == "daily"){
        if(GalParams$rfe.src == "mswep.nrt-gb"){
            dataname <- 'MSWEP_NRT_v2.8'
            dirpath <- 'MSWEP_V280/NRT/Daily'
            fileformat <- '%s%s.nc'
            ncformat <- 'mswep_v2.8_%s%s%s.nc'
        }else if(GalParams$rfe.src == "mswep.past-gb"){
            dataname <- 'MSWEP_Past_v2.8'
            dirpath <- 'MSWEP_V280/Past/Daily'
            fileformat <- '%s%s.nc'
            ncformat <- 'mswep_v2.8_%s%s%s.nc'
        }else if(GalParams$rfe.src == "mswep.pastng-gb"){
            dataname <- 'MSWEP_Past_nogauge_v2.8'
            dirpath <- 'MSWEP_V280/Past_nogauge/Daily'
            fileformat <- '%s%s.nc'
            ncformat <- 'mswep_nogauge_v2.8_%s%s%s.nc'
        }else return(NULL)

        filename <- sprintf(fileformat, rdate[, 1], rdate[, 4])
        ncfiles <- sprintf(ncformat, rdate[, 1], rdate[, 2], rdate[, 3])

        data.tres <- GalParams$tstep
    }else if(GalParams$tstep == "monthly"){
        if(GalParams$rfe.src == "mswep.nrt-gb"){
            dataname <- 'MSWEP_NRT_v2.8'
            dirpath <- 'MSWEP_V280/NRT/Monthly'
            fileformat <- '%s%s.nc'
            ncformat <- 'mswep_v2.8_%s%s.nc'
        }else if(GalParams$rfe.src == "mswep.past-gb"){
            dataname <- 'MSWEP_Past_v2.8'
            dirpath <- 'MSWEP_V280/Past/Monthly'
            fileformat <- '%s%s.nc'
            ncformat <- 'mswep_v2.8_%s%s.nc'
        }else if(GalParams$rfe.src == "mswep.pastng-gb"){
            dataname <- 'MSWEP_Past_nogauge_v2.8'
            dirpath <- 'MSWEP_V280/Past_nogauge/Monthly'
            fileformat <- '%s%s.nc'
            ncformat <- 'mswep_nogauge_v2.8_%s%s.nc'
        }else return(NULL)

        filename <- sprintf(fileformat, rdate[, 1], rdate[, 2])
        ncfiles <- sprintf(ncformat, rdate[, 1], rdate[, 2])

        data.tres <- GalParams$tstep
    }else return(NULL)

    list(dirpath = dirpath, fileformat = fileformat, filename = filename,
         dataname = dataname, ncfiles = ncfiles, data.tres = data.tres)
}

mswep.download.data <- function(lnk, dest, ncfl, datadir,
                                bbox, email, GUI = TRUE)
{
    googledrive::drive_auth(email = email)
    xx <- basename(dest)
    query <- paste0("name = '", xx, "'")
    id_file <- try(googledrive::drive_ls(path = datadir, q = query), silent = TRUE)
    if(inherits(id_file, "try-error")){
        Insert.Messages.Out(id_file[1], TRUE, "e", GUI)
        return(xx)
    }
    if(length(id_file$id) == 0){
        msg <- paste('No file:', paste0(datadir, '/', xx), 'found.')
        Insert.Messages.Out(msg, TRUE, "w", GUI)
        return(xx)
    }

    res <- try(googledrive::drive_download(id_file, path = dest, overwrite = TRUE), silent = TRUE)
    if(!inherits(res, "try-error")){
        ret <- mswep.extract.region(dest, ncfl, bbox)
        if(ret == 0) xx <- NULL
    }else{
        Insert.Messages.Out(res[1], TRUE, "e", GUI)
    }

    return(xx)
}

mswep.extract.region <- function(dest, ncfl, bbox){
    nc <- try(ncdf4::nc_open(dest), silent = TRUE)
    ret <- 1
    if(!inherits(nc, "try-error")){
        varid <- "precipitation"
        lon <- nc$var[[varid]]$dim[[1]]$vals
        lat <- nc$var[[varid]]$dim[[2]]$vals
        val <- ncdf4::ncvar_get(nc, varid)
        ncdf4::nc_close(nc)

        oy <- order(lat)
        lat <- lat[oy]
        val <- val[, oy]

        ix <- lon >= bbox$minlon & lon <= bbox$maxlon
        iy <- lat >= bbox$minlat & lat <= bbox$maxlat
        lon <- lon[ix]
        lat <- lat[iy]
        val <- val[ix, iy]

        val <- round(val, 2)
        val[is.na(val)] <- -99

        dx <- ncdf4::ncdim_def("lon", "degrees_east", lon, longname = "Longitude")
        dy <- ncdf4::ncdim_def("lat", "degrees_north", lat, longname = "Latitude")
        ncgrd <- ncdf4::ncvar_def("precip", "mm", list(dx, dy), -99,
                                  "Precipitation estimates", "float", compression = 9)

        nc <- ncdf4::nc_create(ncfl, ncgrd)
        ncdf4::ncvar_put(nc, ncgrd, val)
        ncdf4::nc_close(nc)

        ret <- 0
    }

    return(ret)
}