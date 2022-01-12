
get.netcdf.time <- function(time.dim){
    temps <- time.dim$units
    calendar <- time.dim$calendar
    if(grepl("since", temps, ignore.case = TRUE)){
        tmp <- trimws(strsplit(temps, " ")[[1]])
        tmp <- tmp[(grep("since", tmp) + 1):length(tmp)]
        tmp <- paste0(tmp, collapse = " ")
        tmp <- trimws(gsub("[^0-9]", " ", tmp))
        tmp <- trimws(strsplit(tmp, " ")[[1]])
        tmp <- tmp[tmp != ""]
        if(length(tmp) > 6)
            tmp <- c(tmp[1:5], paste0(tmp[6], ".", tmp[7]))
        tmp <- as.numeric(tmp)
        tmp <- as.list(tmp)
        nmt <- c('year', 'month', 'day', 'hour', 'min', 'sec')
        names(tmp) <- nmt[seq(length(tmp))]
        origin <- do.call(ISOdate, as.list(tmp))

        if(grepl("second", temps, ignore.case = TRUE)){
            units <- "seconds"
            daty <- as.POSIXct(time.dim$vals, origin = origin, tz = "GMT")
        }else if(grepl("minute", temps, ignore.case = TRUE)){
            units <- "minutes"
            daty <- as.POSIXct(time.dim$vals * 60, origin = origin, tz = "GMT")
        }else if(grepl("hour", temps, ignore.case = TRUE)){
            units <- "hours"
            daty <- as.POSIXct(time.dim$vals * 3600, origin = origin, tz = "GMT")
        }else if(grepl("day", temps, ignore.case = TRUE)){
            units <- "days"
            daty <- as.Date(time.dim$vals, origin = origin)
        }else if(grepl("month", temps, ignore.case = TRUE)){
            units <- "months"
            foo <- function(n) seq(as.Date(origin), by = paste(n, "months"), length = 2)[2]
            daty <- do.call(c, lapply(time.dim$vals, foo))
        }else{
            origin <- NULL
            units <- temps
            daty <- time.dim$vals
            warning(paste("Unknown time units:", temps), "\n")
        }

        if(calendar %in% c("365_day", "365_days", "365 days",
                           "365days", "noleap", "no_leap"))
        {
            syear <- as.numeric(format(origin, '%Y'))
            eyear <- as.numeric(format(daty, '%Y'))
            nbdays <- sum(is.leapyears(syear:eyear))
            if(units == "days"){
                daty <- daty + nbdays
            }
            if(units %in% c("seconds", "minutes", "hours")){
                daty <- daty + nbdays * 60 * 60 * 24
            }
        }
    }else{
        if(grepl("julian", temps, ignore.case = TRUE)){
            units <- "julian"
            # Unix Time
            origin <- "1970-01-01"
            daty <- as.POSIXct((time.dim$vals - 2440587.5) * 86400, origin = origin, tz = "GMT")
        }else{
            origin <- NULL
            units <- temps
            daty <- time.dim$vals
            warning(paste("Unknown time units:", temps), "\n")
        }
    }
    c(time.dim[c('name', 'len')], list(origin = origin, units = units, vals = daty))
}

format.netcdf.time <- function(ncpars, time.dim){
    time.name <- which(ncpars$var.dim == ncpars$dim[[3]]$name)
    ret <- get.netcdf.time(time.dim)
    daty <- ret$vals
    if(!is.null(ret$origin)){
        if(ret$len > 1){
            rdaty <- lapply(c("%Y", "%m", "%d", "%H", "%M", "%S"),
                            function(v) format(daty, v))
            for(it in 6:1){
                if(all.equal.elements(rdaty[[it]]))
                   rdaty[[it]] <- NULL
               else
                    break
            }
            daty <- do.call(paste0, rdaty)
        }else{
            frmt <- switch(ret$units,
                           "seconds" = "%Y%m%d%H%M%S",
                           "minutes" = "%Y%m%d%H%M",
                           "hours" = "%Y%m%d%H",
                           "days" = "%Y%m%d",
                           "months" = "%Y%m",
                                   "%Y%m%d%H%M%S")
            daty <- format(daty, frmt)
        }
    }

    return(daty)
}

###################################################################

split_3d.netcdf_writeNC <- function(){
    Insert.Messages.Out(.cdtData$GalParams[['message']][['4']], TRUE, "i")

    ncpars <- .cdtData$GalParams$ncpars
    outdir <- .cdtData$GalParams$output
    outdir <- file.path(outdir, "CDT_NetCDF_Format")
    dir.create(outdir, showWarnings = FALSE, recursive = TRUE)

    nc.dir <- dirname(.cdtData$GalParams$ncdf$file)
    if(.cdtData$GalParams$nbfile == "several"){
        error.msg <- .cdtData$GalParams[['message']][['2']]
        nc.format <- .cdtData$GalParams$ncdf$format
        ncfiles <- ncFilesInfoSeq(nc.dir, nc.format, error.msg)
    }else{
        ncfiles <- basename(.cdtData$GalParams$ncdf$file)
    }

    ##################

    dx <- do.call(ncdf4::ncdim_def, 
                    c(ncpars$dim[[1]][c('name', 'units', 'vals')],
                      list(longname = "Longitude")))
    dy <- do.call(ncdf4::ncdim_def,
                    c(ncpars$dim[[2]][c('name', 'units', 'vals')],
                      list(longname = "Latitude")))
    outnc <- ncdf4::ncvar_def(ncpars$var$name, ncpars$var$units,
                              list(dx, dy), -99999, ncpars$var$longname,
                              ncpars$var$prec, compression = 9)

    ##################

    nlon <- ncpars$dim[[1]]$len
    nlat <- ncpars$dim[[2]]$len
    start <- rep(NA, ncpars$var$ndims)
    count <- rep(NA, ncpars$var$ndims)

    posx <- which(ncpars$var.dim == ncpars$dim[[1]]$name)
    posy <- which(ncpars$var.dim == ncpars$dim[[2]]$name)
    post <- which(ncpars$var.dim == ncpars$dim[[3]]$name)

    start[posx] <- 1
    start[posy] <- 1
    count[posx] <- nlon
    count[posy] <- nlat
    count[post] <- 1

    if(ncpars$var$ndims > 3){
        for(j in 4:ncpars$var$ndims){
            pos <- which(ncpars$var.dim == ncpars$dim[[j]]$name)
            start[pos] <- ncpars$dim[[j]]$sel
            count[pos] <- 1
        }
    }

    ##################

    ret <- lapply(seq_along(ncfiles), function(jj){
        ncin <- ncdf4::nc_open(file.path(nc.dir, ncfiles[jj]))
        time.dim <- ncin$dim[[ncpars$dim[[3]]$name]]
        daty <- format.netcdf.time(ncpars, time.dim)

        for(i in seq_along(daty)){
            start[post] <- i
            tmp <- ncdf4::ncvar_get(ncin, varid = ncpars$var$name,
                                    start = start, count = count,
                                    collapse_degen = TRUE)

            if(ncpars$rev) tmp <- t(tmp)
            tmp <- tmp[ncpars$dim[[1]]$order, ncpars$dim[[2]]$order]
            tmp[is.na(tmp)] <- -99999

            ncfile <- paste0(ncpars$var$name, "_", daty[i], ".nc")
            ncpath <- file.path(outdir, ncfile)
            ncout <- ncdf4::nc_create(ncpath, outnc)
            ncdf4::ncvar_put(ncout, outnc, tmp)
            ncdf4::nc_close(ncout)
        }

        ncdf4::nc_close(ncin)
        return(0)
    })

    return(0)
}
