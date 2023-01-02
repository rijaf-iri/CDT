#' Create CDT dataset format.
#'
#' Create CDT dataset from a multidimensional netCDF data.
#' 
#' @param cdtdata_info named list, providing the information about the CDT dataset.
#' \itemize{
#' \item{\code{dir}: }{character, full path to the directory to save the created CDT dataset.}
#' \item{\code{name}: }{character, name of the dataset to use.}
#' }
#' @param netcdf_data named list, providing the input netCDF data to be converted to a CDT dataset format.
#' \itemize{
#' \item{\code{timestep}: }{character, the time step of the netCDF data.}
#' \item{\code{dir}: }{character, full path to the directory containing the netCDF files.}
#' \item{\code{format}: }{character, format of the netCDF file names. Example: \code{'precip_\%S.nc'}}
#' }
#' @param ncvar_info named list, providing the name of the variable and longitude, latitude, time of the netCDF data.
#' \itemize{
#' \item{\code{varid}: }{character, the name of the variable to be converted.}
#' \item{\code{lon_dim}: }{character, the name of the longitude dimension.}
#' \item{\code{lat_dim}: }{character, the name of the latitude dimension.}
#' \item{\code{time_dim}: }{character, the name of the time dimension.}
#' }
#' @param extra_ncdim named list, providing the values of the dimensions other than the longitude,
#'latitude and time to be extracted if the netCDF data contains more than 3 dimensions.
#' Default \code{NULL}, the data does not have extra dimension.
#' @param conversion_fun a function to transform the data or to convert the units.
#' @param bbox named list, providing the region to be extracted in the form list(minlon, maxlon, minlat, maxlat). Default \code{NULL}.
#' @param chunk named list containing the size of the chunk \code{size} and the number of chunk to be used for each computation \code{fac}.
#'
#' @return A directory named after the element \code{name} of \code{cdtdata_info}, containing the CDT dataset.
#' 
#' @examples
#' 
#' \dontrun{
#' 
#' cdtdata_info <- list(dir = "~/CDTDATASET/MERRA2", name = "PRECIP")
#' netcdf_data <- list(timestep = 'daily', dir = "~/DATA/MERRA2/daily_precip", format = 'merra-2_precip_%S.nc')
#' ncvar_info <- list(varid = 'precip', lon_dim = 'lon', lat_dim = 'lat', time_dim = 'time')
#' convert_units <- function(x){
#'    # kg m-2 s-1 to mm day-1
#'    x * 86400
#' }
#' 
#' CDT::cdtDataset_readData_multiDim(cdtdata_info, netcdf_data, ncvar_info, conversion_fun = convert_units)
#' 
#' }
#' 
#' @export

cdtDataset_readData_multiDim <- function(cdtdata_info, netcdf_data, ncvar_info,
                                         extra_ncdim = NULL, conversion_fun = NULL,
                                         bbox = NULL, chunk = list(size = 100, fac = 2))
{
    pattern <- get_ncfile_pattern(netcdf_data$format)
    ncfiles <- list.files(netcdf_data$dir, pattern)
    if(length(ncfiles) == 0){
        cat('No netCDF files found\n')
        return(NULL)
    }

    #######
    nc <- ncdf4::nc_open(file.path(netcdf_data$dir, ncfiles[1]))
    lola <- get_lonlat_values(nc, ncvar_info)
    if(!is.null(lola$msg)){
        cat(lola$msg, '\n')
        return(NULL)
    }
    vinfo <- c('name', 'units', 'longname', 'prec', 'missval')
    varinfo <- nc$var[[ncvar_info$varid]][vinfo]
    ncdf4::nc_close(nc)

    nc.lon <- lola$lon
    nc.lat <- lola$lat
    len.lon <- length(nc.lon)
    len.lat <- length(nc.lat)
    ilon <- lola$ix
    ilat <- lola$iy

    if(!is.null(bbox)){
        ixlo <- nc.lon >= bbox$minlon & nc.lon <= bbox$maxlon
        ixla <- nc.lat >= bbox$minlat & nc.lat <= bbox$maxlat

        nc.lon <- nc.lon[ixlo]
        nc.lat <- nc.lat[ixla]
        len.lon <- length(nc.lon)
        len.lat <- length(nc.lat)
        ilon <- ilon[ixlo]
        ilat <- ilat[ixla]
    }

    ret_msg <- check_ncfiles_dim(netcdf_data$dir, ncfiles, ncvar_info)
    if(!is.null(ret_msg)){
        cat(ret_msg, '\n')
        return(NULL)
    }

    #######
    nxy.chunksize <- round(sqrt(chunk$size))
    seqlon <- seq_along(nc.lon)
    seqlat <- seq_along(nc.lat)
    seqcol <- cbind(id = seq(len.lon * len.lat), expand.grid(x = seqlon, y = seqlat))

    split.lon <- split(seqlon, ceiling(seqlon / nxy.chunksize))
    split.lat <- split(seqlat, ceiling(seqlat / nxy.chunksize))
    xgrid <- expand.grid(x = seq_along(split.lon), y = seq_along(split.lat))

    xarrg <- lapply(seq(nrow(xgrid)), function(j){
        crd <- expand.grid(x = nc.lon[split.lon[[xgrid$x[j]]]],
                           y = nc.lat[split.lat[[xgrid$y[j]]]])
        id <- seqcol$id[(seqcol$x %in% split.lon[[xgrid$x[j]]]) &
                        (seqcol$y %in% split.lat[[xgrid$y[j]]])]
        list(coords = crd, id = id, grp = rep(j, length(id)))
    })

    col.idx <- lapply(xarrg, function(x) x$id)
    col.id <- do.call(c, col.idx)
    col.grp <- do.call(c, lapply(xarrg, function(x) x$grp))
    xy.exp <- do.call(rbind, lapply(xarrg, function(x) x$coords))
    col.order <- order(col.id)

    #######

    cdtTmpVar <- NULL
    cdtTmpVar$TimeStep <- netcdf_data$timestep
    cdtTmpVar$chunksize <- nxy.chunksize * nxy.chunksize
    cdtTmpVar$chunkfac <- chunk$fac
    cdtTmpVar$coords$mat <- list(x = nc.lon, y = nc.lat)
    cdtTmpVar$coords$df <- xy.exp
    attr(cdtTmpVar$coords$df, "out.attrs") <- NULL
    cdtTmpVar$colInfo <- list(id = col.id, index = col.grp, order = col.order)
    cdtTmpVar$varInfo <- varinfo

    #######

    datarepo <- file.path(cdtdata_info$dir, cdtdata_info$name)
    if(dir.exists(datarepo)) unlink(datarepo, recursive = TRUE, force = TRUE)
    dir.create(datarepo, showWarnings = FALSE, recursive = TRUE)
    datadir <- file.path(datarepo, 'DATA')
    dir.create(datadir, showWarnings = FALSE, recursive = TRUE)

    datafileIdx <- file.path(datarepo, paste0(cdtdata_info$name, '.rds'))

    #######

    parsL <- doparallel.cond(length(col.idx) >= 20)

    for(j in seq_along(ncfiles)){
        nc <- ncdf4::nc_open(file.path(netcdf_data$dir, ncfiles[j]))
        vars <- ncdf4::ncvar_get(nc, varid = ncvar_info$varid)
        odim <- get_ncdim_order(nc, ncvar_info)
        daty <- format_netcdf_time(nc, nc$dim[[ncvar_info$time_dim]])
        ncdf4::nc_close(nc)

        if(is.null(extra_ncdim)){
            if(length(dim(vars)) > 3){
                cat(paste('File:', ncfiles[j], '; the variable',
                    ncvar_info$varid, 'has', length(odim$dimsize),
                    'dimensions but no extra_ncdim args is provided\n'))
                return(NULL)
            }
        }

        dimname <- odim$dimname
        if(!is.null(extra_ncdim)){
            xdim <- lapply(seq_along(odim$dimsize), function(i){
                1:odim$dimsize[i]
            })
            names(xdim) <- odim$dimname
            for(n in seq_along(extra_ncdim)){
                xdim[[names(extra_ncdim)[n]]] <- extra_ncdim[[n]]
            }

            vars <- do.call(`[`, c(list(vars), xdim))
            dimname <- dimname[!dimname %in% names(extra_ncdim)]
        }

        ilo <- which(dimname == ncvar_info$lon_dim)
        ila <- which(dimname == ncvar_info$lat_dim)
        itt <- which(dimname == ncvar_info$time_dim)

        vars <- aperm(vars, c(ilo, ila, itt), resize = TRUE)
        vars <- vars[ilon, ilat, ]
        vdim <- dim(vars)
        dim(vars) <- c(vdim[1] * vdim[2], vdim[3])

        if(!is.null(conversion_fun)){
            vars <- conversion_fun(vars)
        }

        ret <- cdt.foreach(seq_along(col.idx), parsL = parsL, FUN = function(l)
        {
            # ret <- lapply(seq_along(col.idx), function(l){
            chk <- vars[col.idx[[l]], , drop = FALSE]
            chk <- t(chk)

            file.rds <- file.path(datadir, paste0(l, ".rds"))
            if(file.exists(file.rds)){
                y <- readRDS(file.rds)
                chk <- rbind(y, chk)
            }

            con <- gzfile(file.rds, compression = 7)
            open(con, "wb")
            saveRDS(chk, con)
            close(con)

            return(0)
        })

        idx <- seq(length(daty))
        if(length(cdtTmpVar$dateInfo$date) > 0){
            Adates <- c(cdtTmpVar$dateInfo$date, daty)
            Aindex <- c(cdtTmpVar$dateInfo$index, max(cdtTmpVar$dateInfo$index) + idx)
        }else{
            Adates <- daty
            Aindex <- idx
        }

        odaty <- order(Adates)
        cdtTmpVar$dateInfo <- list(date = Adates[odaty], index = Aindex[odaty])
    }

    con <- gzfile(datafileIdx, compression = 6)
    open(con, "wb")
    saveRDS(cdtTmpVar, con)
    close(con)

    return(0)
}


get_ncfile_pattern <- function(ncformat){
    pattern <- gsub("\\.", "\\\\.", ncformat)
    pattern <- gsub("\\+", "\\\\+", pattern)
    pattern <- gsub("-", "\\\\-", pattern)
    pattern <- gsub("_", "\\\\_", pattern)
    pattern <- gsub("%S", ".+", pattern)
    paste0('^', pattern, '$')
}


get_lonlat_values <- function(nc, ncvar_info){
    ncdim_name <- sapply(nc$var[[ncvar_info$varid]]$dim, '[[', 'name')
    ilo <- which(ncdim_name == ncvar_info$lon_dim)
    ila <- which(ncdim_name == ncvar_info$lat_dim)
    out <- list(lon = NULL, lat = NULL, msg = NULL)
    if(length(ilo) == 0){
        out$msg <- 'Longitude dimension name does not match'
        return(out)
    }
    if(length(ila) == 0){
        out$msg <- 'Latitude dimension name does not match'
        return(out)
    }
    out$lon <- nc$var[[ncvar_info$varid]]$dim[[ilo]]$vals
    out$lat <- nc$var[[ncvar_info$varid]]$dim[[ila]]$vals

    out$ix <- order(out$lon)
    out$iy <- order(out$lat)

    out$lon <- out$lon[out$ix]
    out$lat <- out$lat[out$iy]

    return(out)
}

get_ncdim_order <- function(nc, ncvar_info){
    ncdim_name <- sapply(nc$var[[ncvar_info$varid]]$dim, '[[', 'name')
    ncdim_size <- sapply(nc$var[[ncvar_info$varid]]$dim, '[[', 'len')
    list(dimname = ncdim_name, dimsize = ncdim_size)
}

check_ncfiles_dim <- function(nc_dir, nc_files, ncvar_info){
    nc <- ncdf4::nc_open(file.path(nc_dir, nc_files[1]))
    lola <- get_lonlat_values(nc, ncvar_info)
    if(!is.null(lola$msg)){
        return(lola$msg)
    }
    lon <- lola$lon
    lat <- lola$lat
    var_dim <- sapply(nc$var[[ncvar_info$varid]]$dim, '[[', 'name')
    ncdf4::nc_close(nc)

    for(j in seq_along(nc_files)[-1]){
        nc <- ncdf4::nc_open(file.path(nc_dir, nc_files[j]))
        lola <- get_lonlat_values(nc, ncvar_info)
        if(!is.null(lola$msg)){
            msg <- paste('File:', nc_files[j], ";", lola$msg)
            return(msg)
        }
        xlo <- lola$lon
        xla <- lola$lat
        xdim <- sapply(nc$var[[ncvar_info$varid]]$dim, '[[', 'name')
        ncdf4::nc_close(nc)

        if(!setequal(lon, xlo)){
            msg <- paste('File:', nc_files[j], ";",
                "Longitude has different dimension")
            return(msg)
        }
        if(!setequal(lat, xla)){
            msg <- paste('File:', nc_files[j], ";",
                "Latitude has different dimension")
            return(msg)
        }
        if(!setequal(var_dim, xdim)){
            msg <- paste('File:', nc_files[j],
                "has different dimensions")
            return(msg)
        }
    }

    return(NULL)
}

get_netcdf_time <- function(time.dim){
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

format_netcdf_time <- function(nc, time.dim){
    if(is.null(time.dim$calendar)){
        attr <- ncdf4::ncatt_get(nc, 0, 'calendar')
        if(attr$hasatt){
            time.dim$calendar <- attr$value
        }else{
            time.dim$calendar <- "standard"
        }
    }
    ret <- get_netcdf_time(time.dim)

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
