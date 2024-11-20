
gpm_imerg.download.gesdisc <- function(GalParams, GUI = TRUE, verbose = TRUE){
    info <- gpm_imerg.info.gesdisc(GalParams)
    if(is.null(info)) return(-3)

    ######
    data.name <- paste0("GPM_L3_IMERG_V", info$version, "_", info$pars$type, "_", info$data.tres)
    outdir <- file.path(GalParams$dir2save, data.name)
    if(!dir.exists(outdir))
        dir.create(outdir, showWarnings = FALSE, recursive = TRUE)
    destfiles <- file.path(outdir, paste0('tmp_', info$ncfiles))
    ncfiles <- file.path(outdir, info$ncfiles)

    ######
    lon <- seq(-179.9, 179.9, 0.1)
    lat <- seq(-89.9, 89.9, 0.1)

    ix <- lon >= GalParams$bbox$minlon & lon <= GalParams$bbox$maxlon
    iy <- lat >= GalParams$bbox$minlat & lat <= GalParams$bbox$maxlat

    if(!any(ix) | !any(iy)) return(-2)

    ilon <- range(which(ix)) + c(-1, 0)
    if(ilon[1] < 0) ilon[1] <- 0
    ilat <- range(which(iy)) + c(-1, 0)
    if(ilat[1] < 0) ilat[1] <- 0
    sublon <- paste0("[", ilon[1], ":1:", ilon[2], "]")
    sublat <- paste0("[", ilat[1], ":1:", ilat[2], "]")

    request <- sprintf("%s[0:1:0]%s%s,lon%s,lat%s", info$pars$varid, sublon, sublat, sublon, sublat)
    # request <- utils::URLencode(request, reserved = TRUE)
    urls <- paste0(info$urls, "?", request)

    ######
    auth <- earthdata.curlopts(outdir, GalParams$login)
    handle <- curl::new_handle()
    curl::handle_setopt(handle,
                        netrc_file = auth$netrc,
                        cookiefile = auth$cookie,
                        cookiejar = auth$cookie)
    on.exit({
        curl::handle_reset(handle)
        unlink(auth$cookie)
        unlink(auth$netrc)
    })
    nbfile <- 1

    ret <- cdt.download.data(urls, destfiles, ncfiles, nbfile, GUI, verbose,
                             data.name, gpm_imerg.download.data,
                             pars = info$pars, handle = handle)
    return(ret)
}

gpm_imerg.coverage.gesdisc <- function(GalParams){
    if(GalParams$tstep == 'minute'){
        timestep <- paste(GalParams$minhour, GalParams$tstep)
    }else{
        timestep <- GalParams$tstep
    }
    out <- list(name = GalParams$rfe.src, timestep = timestep)
    info <- gpm_imerg.info.gesdisc(GalParams)
    if(is.null(info)) return(out)

    out$name <- paste('GPM IMERG', info$pars$type, 'Version', info$version)

    baseurl <- file.path(info$opendap, info$level, info$dataset)
    url <- file.path(baseurl, 'contents.html')
    end_d <- opendap.gesdisc.dates(url, 'directory', datetype = 'year')
    if(is.null(end_d)) return(out)
    end_year <- end_d[length(end_d)]

    if(GalParams$tstep == 'minute'){
        url <- file.path(baseurl, end_year, 'contents.html')
        end_d <- opendap.gesdisc.dates(url, 'directory', datetype = 'doy', diryear = end_year)
        if(is.null(end_d)) return(out)
        end_doy <- end_d[length(end_d)]
        url <- file.path(baseurl, end_year, end_doy, 'contents.html')
        fileformat <- tools::file_path_sans_ext(info$nc4format)
        end_d <- opendap.gesdisc.dates(url, 'file', fileformat)
        if(is.null(end_d)) return(out)
        out$end <- substr(end_d[length(end_d)], 1, 12)
    }else if(GalParams$tstep == 'daily'){
        url <- file.path(baseurl, end_year, 'contents.html')
        end_d <- opendap.gesdisc.dates(url, 'directory', datetype = 'month', diryear = end_year)
        if(is.null(end_d)) return(out)
        end_mon <- end_d[length(end_d)]
        url <- file.path(baseurl, end_year, end_mon, 'contents.html')
        fileformat <- tools::file_path_sans_ext(info$nc4format)
        end_d <- opendap.gesdisc.dates(url, 'file', fileformat)
        if(is.null(end_d)) return(out)
        out$end <- end_d[length(end_d)]
    }else{
        url <- file.path(baseurl, end_year, 'contents.html')
        fileformat <- tools::file_path_sans_ext(info$nc4format)
        end_d <- opendap.gesdisc.dates(url, 'file', fileformat)
        if(is.null(end_d)) return(out)
        out$end <- substr(end_d[length(end_d)], 1, 6)
    }

    out$start <- switch(GalParams$tstep,
                        "minute" = "200006010000",
                        "daily" = "20000601",
                        "monthly" = "200006")
    return(out)
}

gpm_imerg.info.gesdisc <- function(GalParams){
    rdate <- table.format.date.time(GalParams$tstep, GalParams$date.range, GalParams$minhour)
    if(is.null(rdate)) return(NULL)

    opendap <- "https://gpm1.gesdisc.eosdis.nasa.gov/opendap"
    level <- "GPM_L3"

    if(GalParams$tstep == "minute"){
        if(GalParams$minhour == 30){
            if(GalParams$rfe.src == "gpm.imerg.f-gb"){
                ## version 6
                # version <- '06'
                # dataset <- "GPM_3IMERGHH.06"
                # nc4format <- "3B-HHR.MS.MRG.3IMERG.%s-S%s-E%s.%s.V06B.HDF5.nc4"
                # ncvarid <- 'precipitationCal'
                ## version 7
                version <- '07'
                dataset <- "GPM_3IMERGHH.07"
                nc4format <- "3B-HHR.MS.MRG.3IMERG.%s-S%s-E%s.%s.V07B.HDF5.nc4"
                ncvarid <- 'precipitation'
                ncformat <- "imerg_final_%s%s%s%s%s.nc"
                type <- "FINAL"
                # https://gpm1.gesdisc.eosdis.nasa.gov/opendap/GPM_L3/GPM_3IMERGHH.07/2024/182/3B-HHR.MS.MRG.3IMERG.20240630-S233000-E235959.1410.V07B.HDF5
            }else if(GalParams$rfe.src == "gpm.imerg.l-gb"){
                ## version 6
                # version <- '06'
                # dataset <- "GPM_3IMERGHHL.06"
                # nc4format <- "3B-HHR-L.MS.MRG.3IMERG.%s-S%s-E%s.%s.V06B.HDF5.nc4"
                # ncvarid <- 'precipitationCal'
                # ncformat <- "imerg_late_%s%s%s%s%s.nc"
                # type <- "LATE"
                # https://gpm1.gesdisc.eosdis.nasa.gov/opendap/GPM_L3/GPM_3IMERGHHL.06/2024/154/3B-HHR-L.MS.MRG.3IMERG.20240602-S183000-E185959.1110.V06E.HDF5
                ## version 7
                version <- '07'
                dataset <- "GPM_3IMERGHHL.07"
                nc4format <- "3B-HHR-L.MS.MRG.3IMERG.%s-S%s-E%s.%s.V07B.HDF5.nc4"
                ncvarid <- 'precipitation'
                ncformat <- "imerg_late_%s%s%s%s%s.nc"
                type <- "LATE"
                # https://gpm1.gesdisc.eosdis.nasa.gov/opendap/GPM_L3/GPM_3IMERGHHL.07/2024/324/3B-HHR-L.MS.MRG.3IMERG.20241119-S123000-E125959.0750.V07B.HDF5
            }else if(GalParams$rfe.src == "gpm.imerg.e-gb"){
                ## version 6
                # version <- '06'
                # dataset <- "GPM_3IMERGHHE.06"
                # nc4format <- "3B-HHR-E.MS.MRG.3IMERG.%s-S%s-E%s.%s.V06B.HDF5.nc4"
                # ncvarid <- 'precipitationCal'
                # ncformat <- "imerg_early_%s%s%s%s%s.nc"
                # type <- "EARLY"
                # https://gpm1.gesdisc.eosdis.nasa.gov/opendap/GPM_L3/GPM_3IMERGHHE.06/2024/155/3B-HHR-E.MS.MRG.3IMERG.20240603-S043000-E045959.0270.V06E.HDF5
                ## version 7
                version <- '07'
                dataset <- "GPM_3IMERGHHE.07"
                nc4format <- "3B-HHR-E.MS.MRG.3IMERG.%s-S%s-E%s.%s.V07B.HDF5.nc4"
                ncvarid <- 'precipitation'
                ncformat <- "imerg_early_%s%s%s%s%s.nc"
                type <- "EARLY"
                # https://gpm1.gesdisc.eosdis.nasa.gov/opendap/GPM_L3/GPM_3IMERGHHE.07/2024/324/3B-HHR-E.MS.MRG.3IMERG.20241119-S223000-E225959.1350.V07B.HDF5
            }else return(NULL)

            dd <- paste0(rdate[, 1], rdate[, 2], rdate[, 3])
            ss <- paste0(rdate[, 4], rdate[, 5], "00")
            ee <- ifelse(rdate[, 5] == "00", "29", "59")
            ee <- paste0(rdate[, 4], ee, "59")
            mm <- as.numeric(rdate[, 4]) * 60 + as.numeric(rdate[, 5])
            mm <- sprintf("%04d", mm)

            nc4files <- sprintf(nc4format, dd, ss, ee, mm)
            paths <- file.path(level, dataset, rdate[, 1], rdate[, 6])
            ncfiles <- sprintf(ncformat, rdate[, 1], rdate[, 2], rdate[, 3], rdate[, 4], rdate[, 5])

            data.tres <- paste0(GalParams$minhour, GalParams$tstep)
            unitfactor <- 0.5

            ## version 6: exception minor version
            # if(GalParams$rfe.src %in% c("gpm.imerg.l-gb", "gpm.imerg.e-gb")){
            #     breaks <- switch(GalParams$rfe.src,
            #                 "gpm.imerg.l-gb" = c('20220508 153000', '20230701 133000', '20231108 013000'),
            #                 "gpm.imerg.e-gb" = c('20220509 013000', '20230701 233000', '20231108 123000')
            #                 )
            #     breaks <- as.POSIXct(breaks, format = '%Y%m%d %H%M%S', tz = 'UTC')
            #     times <- apply(rdate[, 1:5], 1, paste, collapse = "")
            #     times <- as.POSIXct(times, format = '%Y%m%d%H%M', tz = 'UTC')
            #     ix <- findInterval(times, breaks, rightmost.closed = FALSE, left.open = TRUE)
            #     minorv <- c('B', 'C', 'D', 'E')
            #     substr(nc4files, 57, 57) <- minorv[ix + 1]
            # }
        }else return(NULL)
    }else if(GalParams$tstep == "daily"){
        if(GalParams$rfe.src == "gpm.imerg.f-gb"){
            ## version 6
            # version <- '06'
            # dataset <- "GPM_3IMERGDF.06"
            # nc4format <- "3B-DAY.MS.MRG.3IMERG.%s-S000000-E235959.V06.nc4.nc4"
            # ncvarid <- 'precipitationCal'
            ## version 7
            version <- '07'
            dataset <- "GPM_3IMERGDF.07"
            nc4format <- "3B-DAY.MS.MRG.3IMERG.%s-S000000-E235959.V07B.nc4.nc4"
            ncvarid <- 'precipitation'
            ncformat <- "imerg_final_%s%s%s.nc"
            type <- "FINAL"
            # https://gpm1.gesdisc.eosdis.nasa.gov/opendap/GPM_L3/GPM_3IMERGDF.07/2024/06/3B-DAY.MS.MRG.3IMERG.20240630-S000000-E235959.V07B.nc4
        }else if(GalParams$rfe.src == "gpm.imerg.l-gb"){
            ## version 6
            # version <- '06'
            # dataset <- "GPM_3IMERGDL.06"
            # nc4format <- "3B-DAY-L.MS.MRG.3IMERG.%s-S000000-E235959.V06.nc4.nc4"
            # ncvarid <- 'precipitationCal'
            # ncformat <- "imerg_late_%s%s%s.nc"
            # type <- "LATE"
            # https://gpm1.gesdisc.eosdis.nasa.gov/opendap/GPM_L3/GPM_3IMERGDL.06/2024/06/3B-DAY-L.MS.MRG.3IMERG.20240601-S000000-E235959.V06.nc4
            ## version 7
            version <- '07'
            dataset <- "GPM_3IMERGDL.07"
            nc4format <- "3B-DAY-L.MS.MRG.3IMERG.%s-S000000-E235959.V07B.nc4.nc4"
            ncvarid <- 'precipitation'
            ncformat <- "imerg_late_%s%s%s.nc"
            type <- "LATE"
            # https://gpm1.gesdisc.eosdis.nasa.gov/opendap/GPM_L3/GPM_3IMERGDL.07/2024/11/3B-DAY-L.MS.MRG.3IMERG.20241118-S000000-E235959.V07B.nc4
        }else if(GalParams$rfe.src == "gpm.imerg.e-gb"){
            ## version 6
            # version <- '06'
            # dataset <- "GPM_3IMERGDE.06"
            # nc4format <- "3B-DAY-E.MS.MRG.3IMERG.%s-S000000-E235959.V06.nc4.nc4"
            # ncvarid <- 'precipitationCal'
            # ncformat <- "imerg_early_%s%s%s.nc"
            # type <- "EARLY"
            # https://gpm1.gesdisc.eosdis.nasa.gov/opendap/GPM_L3/GPM_3IMERGDE.06/2024/06/3B-DAY-E.MS.MRG.3IMERG.20240602-S000000-E235959.V06.nc4
            ## version 7
            version <- '07'
            dataset <- "GPM_3IMERGDE.07"
            nc4format <- "3B-DAY-E.MS.MRG.3IMERG.%s-S000000-E235959.V07B.nc4.nc4"
            ncvarid <- 'precipitation'
            ncformat <- "imerg_early_%s%s%s.nc"
            type <- "EARLY"
            # https://gpm1.gesdisc.eosdis.nasa.gov/opendap/GPM_L3/GPM_3IMERGDE.07/2024/11/3B-DAY-E.MS.MRG.3IMERG.20241118-S000000-E235959.V07B.nc4
        }else return(NULL)

        dd <- paste0(rdate[, 1], rdate[, 2], rdate[, 3])
        nc4files <- sprintf(nc4format, dd)
        paths <- file.path(level, dataset, rdate[, 1], rdate[, 2])
        ncfiles <- sprintf(ncformat, rdate[, 1], rdate[, 2], rdate[, 3])

        data.tres <- GalParams$tstep
        unitfactor <- 1
    }else if(GalParams$tstep == "monthly"){
        ## version 6
        # version <- '06'
        # dataset <- "GPM_3IMERGM.06"
        # nc4format <- "3B-MO.MS.MRG.3IMERG.%s-S000000-E235959.%s.V06B.HDF5.nc4"
        # ncvarid <- 'precipitation'
        ## version 7
        version <- '07'
        dataset <- "GPM_3IMERGM.07"
        nc4format <- "3B-MO.MS.MRG.3IMERG.%s-S000000-E235959.%s.V07B.HDF5.nc4"
        ncvarid <- 'precipitation'
        ncformat <- "imerg_final_%s%s.nc"
        type <- "FINAL"
        # https://gpm1.gesdisc.eosdis.nasa.gov/opendap/GPM_L3/GPM_3IMERGM.07/2024/3B-MO.MS.MRG.3IMERG.20240601-S000000-E235959.06.V07B.HDF5

        dd <- paste0(rdate[, 1], rdate[, 2], "01")
        nc4files <- sprintf(nc4format, dd, rdate[, 2])
        paths <- file.path(level, dataset, rdate[, 1])
        ncfiles <- sprintf(ncformat, rdate[, 1], rdate[, 2])

        data.tres <- GalParams$tstep
        unitfactor <- 0
    }else return(NULL)

    urls <- file.path(opendap, paths, nc4files)
    pars <- list(factor = unitfactor, varid = ncvarid,
                 tstep = GalParams$tstep, type = type)

    list(opendap = opendap, level = level, dataset = dataset,
         nc4format = nc4format, urls = urls, ncfiles = ncfiles,
         data.tres = data.tres, pars = pars, version = version)
}

#########################################################

gpm_imerg.download.data <- function(lnk, dest, ncfl, pars, handle){
    on.exit(unlink(dest))
    xx <- basename(dest)

    dc <- try(curl::curl_download(lnk, dest, handle = handle), silent = TRUE)
    if(!inherits(dc, "try-error")){
        ret <- gpm_imerg.format.data(dest, ncfl, pars)
        ret = 0
        if(ret == 0) xx <- NULL
    }else{
        msg <- gsub('[\r\n]', '', dc[1])
        tmpdir <- dirname(dest)
        error_files <- paste0(basename(tmpdir), '_error.txt')
        error_files <- file.path(dirname(tmpdir), error_files)
        cat(msg, file = error_files, sep = '\n', append = TRUE)
    }

    return(xx)
}

gpm_imerg.format.data <- function(dest, ncfl, pars){
    nc <- try(ncdf4::nc_open(dest), silent = TRUE)
    ret <- 1
    if(!inherits(nc, "try-error")){
        lon <- nc$dim$lon$vals
        lat <- nc$dim$lat$vals
        prcp <- ncdf4::ncvar_get(nc, varid = pars$varid)

        long_name <- ncdf4::ncatt_get(nc, pars$varid, 'LongName')
        if(long_name$hasatt){
            longname <- gsub('\n', '', long_name$value)
            longname <- gsub('\\s+', ' ', longname)
        }else{
            longname <- nc$var[[pars$varid]]$longname
            if(longname == pars$varid){
                longname <- "Precipitation estimates from various precipitation-relevant satellite passive microwave"
            }
        }
        ncdf4::nc_close(nc)

        if(pars$factor == 0){
            daty <- gsub(".*_|\\.nc$", "", basename(ncfl))
            if(pars$tstep == "monthly"){
                yr <- substr(daty, 1, 4)
                mo <- substr(daty, 5, 6)
                fac <- Day.Of.Month(yr, mo)
                prcp <- prcp * 24 * fac
            }
        }else{
            prcp <- prcp * pars$factor
        }

        prcp <- t(prcp)
        prcp <- round(prcp, 2)
        prcp[is.na(prcp)] <- -99

        dx <- ncdf4::ncdim_def("lon", "degrees_east", lon, longname = "Longitude")
        dy <- ncdf4::ncdim_def("lat", "degrees_north", lat, longname = "Latitude")
        ncgrd <- ncdf4::ncvar_def("precip", "mm", list(dx, dy), -99,
                                  longname, "float", compression = 9)

        nc <- ncdf4::nc_create(ncfl, ncgrd)
        ncdf4::ncvar_put(nc, ncgrd, prcp)
        ncdf4::nc_close(nc)

        ret <- 0
    }

    return(ret)
}
