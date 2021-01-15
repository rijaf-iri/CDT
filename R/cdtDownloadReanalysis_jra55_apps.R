
#' Submitting subset requests for JRA55 minimum and maximum temperature at 2 meter above ground.
#'
#' Function to request a subset of JRA55 minimum and maximum temperature at 2 meter above ground.
#' 
#' @param variable the variable to be requested, available options are "tmax": maximum temperature and "tmin" minimum temperature.
#' @param email your email address used as login on https://rda.ucar.edu/.
#' @param password your password.
#' @param bbox a named list providing the bounding box of the area to extract. The names of components must minlon, maxlon, minlat and maxlat.
#' @param start_time a named list providing the start time to extract. The names of components must year, month, day and hour
#' @param end_time a named list providing the end time to extract. The names of components must year, month, day and hour
#' 
#' @return A summary of your request and the link to download the data will be send via email.
#' In addition a summary of your request will be displayed on the console.
#' 
#' @export

jra55.send.request <- function(variable = "tmax", email = NULL, password = NULL,
                               bbox = list(minlon = 42, maxlon = 52, minlat = -26, maxlat = -11),
                               start_time = list(year = 2020, month = 11, day = 1, hour = 0),
                               end_time = list(year = 2020, month = 11, day = 30, hour = 21)
                              )
{
    param <- switch(variable, "tmax" = "T MAX", "tmin" = "T MIN")

    start <- jra55.start.end.time(start_time)
    start <- format(start, "%Y%m%d%H%M")
    end <- jra55.start.end.time(end_time)
    end <- format(end, "%Y%m%d%H%M")

    request <- list(
        dataset = "ds628.0",
        date = paste0(start, "/to/", end),
        param = param,
        oformat = "netCDF",
        nlat = bbox$maxlat,
        slat = bbox$minlat,
        wlon = bbox$minlon,
        elon = bbox$maxlon
    )
    request <- jsonlite::toJSON(request, auto_unbox = TRUE)

    handle <- curl::new_handle()
    curl::handle_setopt(handle, username = email, password = password)
    curl::handle_setopt(handle, copypostfields = request)
    curl::handle_setheaders(handle,
                            "Accept" = "application/json",
                            "Content-Type" = "application/json",
                            "charset" = "UTF-8"
                           )
    app_url <- "https://rda.ucar.edu/apps/request"
    res <- curl::curl_fetch_memory(app_url, handle = handle)

    cat(rawToChar(res$content), "\n")
}


#' Splitting and formatting the JRA55 3 hourly downloaded data.
#'
#' Function to split and format the downloaded JRA55 minimum and maximum temperature at 2 meter above ground.
#' 
#' @param dirNCDF full path to the folder containing the downloaded netCDF files.
#' If the data are compressed wit "tar" or "zip", make sure to untar or unzip all files
#' and put the uncompressed netCDF files under this folder
#' @param dirOUT full path to the folder you want to save the formatted data.
#' 
#' @export

jra55.format.ncdf <- function(dirNCDF, dirOUT){
    ncfiles <- list.files(dirNCDF, ".+\\.nc$")
    if(length(ncfiles) == 0)
        stop("No NetCDF files found\n")

    ncpaths <- file.path(dirNCDF, ncfiles)

    nc <- ncdf4::nc_open(ncpaths[1])
    varid <- nc$var[[1]]$name
    dname <- sapply(nc$dim, "[[", "name")
    nlon <- dname[grep("lon", dname)]
    nlat <- dname[grep("lat", dname)]
    lat <- nc$dim[[nlat]]$vals
    lon <- nc$dim[[nlon]]$vals
    ncdf4::nc_close(nc)

    lon <- ((lon + 180) %% 360) - 180

    xo <- order(lon)
    yo <- order(lat)

    varname <- switch(varid,
                      "TMAX_GDS4_HTGL" = "tmax",
                      "TMIN_GDS4_HTGL" = "tmin"
                     )
    longname <- switch(varid,
                       "TMAX_GDS4_HTGL" = "TMAX JRA55 3 hourly",
                       "TMIN_GDS4_HTGL" = "TMIN JRA55 3 hourly"
                     )
    dir3hr <- switch(varid,
                     "TMAX_GDS4_HTGL" = "JRA55_3Hr_tmax",
                     "TMIN_GDS4_HTGL" = "JRA55_3Hr_tmin"
                    )
    outdir <- file.path(dirOUT, dir3hr)
    dir.create(outdir, showWarnings = FALSE, recursive = TRUE)

    missval <- -99

    dx <- ncdf4::ncdim_def("Lon", "degreeE", lon[xo], longname = "Longitude")
    dy <- ncdf4::ncdim_def("Lat", "degreeN", lat[yo], longname = "Latitude")
    ncgrd <- ncdf4::ncvar_def(varname, "degC", list(dx, dy), missval,
                              longname, "float", compression = 6)

    for(jj in seq_along(ncfiles)){
        nc <- ncdf4::nc_open(ncpaths[jj])
        # init_time0 <- nc$dim[['initial_time0_hours']]$vals
        # t_units <- nc$dim[['initial_time0_hours']]$units
        # 
        fcst_time <- nc$dim[["forecast_time1"]]$vals
        initial_time0 <- ncdf4::ncvar_get(nc, varid = "initial_time0_encoded")
        val <- ncdf4::ncvar_get(nc, varid)
        ncdf4::nc_close(nc)

        if(length(dim(val)) == 2){
            ncout <- file.path(outdir, paste0(varname, "_", initial_time0, ".nc"))
            val <- val - 273.15
            val[is.na(val)] <- missval

            nc <- ncdf4::nc_create(ncout, ncgrd)
            ncdf4::ncvar_put(nc, ncgrd, val[xo, yo])
            ncdf4::nc_close(nc)
        }else{
            # units(init_time0) <- units::as_units(t_units)
            # times <- as.POSIXct(init_time0, tz = "UTC")
            times <- strptime(initial_time0, "%Y%m%d%H", tz = "UTC")
            # 
            timestamp <- c(rbind(times + fcst_time[1] * 3600, times + fcst_time[2] * 3600))
            daty <- as.POSIXct(timestamp, origin = "1970-01-01", tz = "UTC")
            outdaty <- format(daty, "%Y%m%d%H")
            ncout <- file.path(outdir, paste0(varname, "_", outdaty, ".nc"))

            dim_val <- dim(val)
            dim(val) <- c(dim_val[1:2], dim_val[3] * dim_val[4])

            val <- val - 273.15
            val[is.na(val)] <- missval

            for(kk in 2:(length(ncout) - 1)){
                nc <- ncdf4::nc_create(ncout[kk], ncgrd)
                ncdf4::ncvar_put(nc, ncgrd, val[xo, yo, kk])
                ncdf4::nc_close(nc)
            }
        }
    }

    cat("Formatting data finished successfully\n")
}
