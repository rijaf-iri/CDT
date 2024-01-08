
## toexport
gpm_imerg.download.dods <- function(GalParams, nbfile = 1, GUI = TRUE, verbose = TRUE){
    on.exit(curl::handle_reset(handle))

    rdate <- table.format.date.time(GalParams$tstep, GalParams$date.range, GalParams$minhour)
    # rdate <- table.format.date.time("daily", GalParams$date.range, GalParams$minhour)

    if(GalParams$tstep == "minute"){
        if(GalParams$minhour == 30){
            if(GalParams$rfe.src == "gpm.imerg.f-gb"){
                nc4files <- "3B-HHR.MS.MRG.3IMERG.%s-S%s-E%s.%s.V06B.HDF5.nc4"
                paths <- "GPM_3IMERGHH.06"
                ncfiles <- "imerg_final_%s%s%s%s%s.nc"
                type <- "FINAL"
            }
            else if(GalParams$rfe.src == "gpm.imerg.l-gb"){
                nc4files <- "3B-HHR-L.MS.MRG.3IMERG.%s-S%s-E%s.%s.V06B.HDF5.nc4"
                paths <- "GPM_3IMERGHHL.06"
                ncfiles <- "imerg_late_%s%s%s%s%s.nc"
                type <- "LATE"
            }
            else if(GalParams$rfe.src == "gpm.imerg.e-gb"){
                nc4files <- "3B-HHR-E.MS.MRG.3IMERG.%s-S%s-E%s.%s.V06B.HDF5.nc4"
                paths <- "GPM_3IMERGHHE.06"
                ncfiles <- "imerg_early_%s%s%s%s%s.nc"
                type <- "EARLY"
            }else return(-1)

            dd <- paste0(rdate[, 1], rdate[, 2], rdate[, 3])
            ss <- paste0(rdate[, 4], rdate[, 5], "00")
            ee <- ifelse(rdate[, 5] == "00", "29", "59")
            ee <- paste0(rdate[, 4], ee, "59")
            mm <- as.numeric(rdate[, 4]) * 60 + as.numeric(rdate[, 5])
            mm <- stringr::str_pad(mm, 4, pad = "0")

            nc4files <- sprintf(nc4files, dd, ss, ee, mm)
            paths <- file.path("GPM_L3", paths, rdate[, 1], rdate[, 6])
            ncfiles <- sprintf(ncfiles, rdate[, 1], rdate[, 2], rdate[, 3], rdate[, 4], rdate[, 5])

            data.tres <- paste0(GalParams$minhour, GalParams$tstep)
            pars <- list(fac = 0.5, varid = "precipitationCal", tstep = GalParams$tstep)
        }else return(-1)
    }
    else if(GalParams$tstep == "daily"){
        if(GalParams$rfe.src == "gpm.imerg.f-gb"){
            nc4files <- "3B-DAY.MS.MRG.3IMERG.%s-S000000-E235959.V06.nc4.nc4"
            paths <- "GPM_3IMERGDF.06"
            ncfiles <- "imerg_final_%s%s%s.nc"
            type <- "FINAL"
        }
        else if(GalParams$rfe.src == "gpm.imerg.l-gb"){
            nc4files <- "3B-DAY-L.MS.MRG.3IMERG.%s-S000000-E235959.V06.nc4.nc4"
            paths <- "GPM_3IMERGDL.06"
            ncfiles <- "imerg_late_%s%s%s.nc"
            type <- "LATE"
        }
        else if(GalParams$rfe.src == "gpm.imerg.e-gb"){
            nc4files <- "3B-DAY-E.MS.MRG.3IMERG.%s-S000000-E235959.V06.nc4.nc4"
            paths <- "GPM_3IMERGDE.06"
            ncfiles <- "imerg_early_%s%s%s.nc"
            type <- "EARLY"
        }else return(-1)

        dd <- paste0(rdate[, 1], rdate[, 2], rdate[, 3])
        nc4files <- sprintf(nc4files, dd)
        paths <- file.path("GPM_L3", paths, rdate[, 1], rdate[, 2])
        ncfiles <- sprintf(ncfiles, rdate[, 1], rdate[, 2], rdate[, 3])

        data.tres <- GalParams$tstep
        pars <- list(fac = 1, varid = "precipitationCal", tstep = GalParams$tstep)
    }
    else if(GalParams$tstep == "monthly"){
        dd <- paste0(rdate[, 1], rdate[, 2], "01")
        nc4files <- sprintf("3B-MO.MS.MRG.3IMERG.%s-S000000-E235959.%s.V06B.HDF5.nc4", dd, rdate[, 2])
        paths <- file.path("GPM_L3", "GPM_3IMERGM.06", rdate[, 1])
        ncfiles <- sprintf("imerg_final_%s%s.nc", rdate[, 1], rdate[, 2])
        data.tres <- GalParams$tstep
        type <- "FINAL"
        pars <- list(fac = 0, varid = "precipitation", tstep = GalParams$tstep)
    }else return(-1)

    #############
    lon <- seq(-179.9, 179.9, 0.1)
    lat <- seq(-89.9, 89.9, 0.1)

    ix <- lon >= GalParams$bbox$minlon & lon <= GalParams$bbox$maxlon
    iy <- lat >= GalParams$bbox$minlat & lat <= GalParams$bbox$maxlat

    if(!any(ix) | !any(iy)) return(-2)

    ilon <- range(which(ix)) + c(-1, 0)
    if(ilon[1] < 0) ilon[1] <- 0
    ilat <- range(which(iy)) + c(-1, 0)
    if(ilat[1] < 0) ilat[1] <- 0
    sublon <- paste0("[", ilon[1], ":", ilon[2], "]")
    sublat <- paste0("[", ilat[1], ":", ilat[2], "]")

    #############
    opendap <- "https://gpm1.gesdisc.eosdis.nasa.gov/opendap"
    request <- sprintf("?%s[0:0]%s%s,time,lon%s,lat%s", pars$varid, sublon, sublat, sublon, sublat)
    urls <- paste0(file.path(opendap, paths, nc4files), request)

    #############
    if(GalParams$rfe.src == "gpm.imerg.f-gb"){
        longname <- "Precipitation (combined microwave-IR) estimate with gauge calibration"
    }else{
        longname <- "Precipitation (combined microwave-IR) estimate with climatological calibration"
    }

    dx <- ncdf4::ncdim_def("Lon", "degreeE", lon[ilon[1]:ilon[2]] + 0.05, longname = "Longitude")
    dy <- ncdf4::ncdim_def("Lat", "degreeN", lat[ilat[1]:ilat[2]] + 0.05, longname = "Latitude")
    ncgrd <- ncdf4::ncvar_def("precip", "mm", list(dx, dy), -99, longname, "float", compression = 9)

    handle <- curl::new_handle()
    curl::handle_setopt(handle, username = GalParams$login$usr, password = GalParams$login$pwd)

    #############
    data.name <- paste0("GPM_L3_IMERG_V6_", type, "_", data.tres)
    outdir <- file.path(GalParams$dir2save, data.name)
    dir.create(outdir, showWarnings = FALSE, recursive = TRUE)
    destfiles <- file.path(outdir, nc4files)
    ncfiles <- file.path(outdir, ncfiles)

    ret <- cdt.download.data(urls, destfiles, ncfiles, nbfile, GUI,
                             verbose, data.name, gpm_imerg.download.data,
                             handle = handle, ncgrd = ncgrd, pars = pars)

    return(ret)
}

# https://gpm1.gesdisc.eosdis.nasa.gov/opendap/GPM_L3/GPM_3IMERGDL.06/2022/10/3B-DAY-L.MS.MRG.3IMERG.20221011-S000000-E235959.V06.nc4.nc4?precipitationCal[0:0][1799:1959][1009:1139],time,lon[1799:1959],lat[1009:1139]
# https://gpm1.gesdisc.eosdis.nasa.gov/opendap/GPM_L3/GPM_3IMERGDL.06/2022/10/3B-DAY-L.MS.MRG.3IMERG.20221011-S000000-E235959.V06.nc4.nc4

#################################################################################

gpm_imerg.download.data <- function(lnk, dest, ncfl, handle, ncgrd, pars, GUI = TRUE){
    on.exit(unlink(dest))
    xx <- basename(dest)

    dc <- try(curl::curl_download(lnk, dest, handle = handle), silent = TRUE)
    if(!inherits(dc, "try-error")){
        ret <- gpm_imerg.format.data(dest, ncfl, ncgrd, pars)
        if(ret == 0) xx <- NULL
    }else{
        msg <- gsub('[\r\n]', '', dc[1])
        Insert.Messages.Out(msg, TRUE, "w", GUI)
    }

    return(xx)
}

gpm_imerg.format.data <- function(dest, ncfl, ncgrd, pars){
    nc <- try(ncdf4::nc_open(dest), silent = TRUE)
    ret <- 1
    if(!inherits(nc, "try-error")){
        z <- ncdf4::ncvar_get(nc, pars$varid)
        ncdf4::nc_close(nc)

        z <- t(z)
        if(pars$fac == 0){
            daty <- gsub(".*_|\\.nc$", "", basename(ncfl))
            if(pars$tstep == "monthly"){
                yr <- substr(daty, 1, 4)
                mo <- substr(daty, 5, 6)
                fac <- Day.Of.Month(yr, mo)
                z <- z * 24 * fac
            }
        }else{
            z <- z * pars$fac
        }
        z[is.na(z)] <- ncgrd$missval

        nc <- ncdf4::nc_create(ncfl, ncgrd)
        ncdf4::ncvar_put(nc, ncgrd, z)
        ncdf4::nc_close(nc)

        ret <- 0
    }

    return(ret)
}

