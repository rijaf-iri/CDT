
#' Daily surface and mean sea level pressure conversion for CDT station data format.
#'
#' Function to convert surface and mean sea level pressure in both way.
#' 
#' @param convert character, the direction of conversion. Valid options: \code{"sfc2msl"} and \code{"msl2sfc"}.
#' \itemize{
#' \item{\code{"sfc2msl"}: }{conversion from surface to mean sea level pressure.}
#' \item{\code{"msl2sfc"}: }{conversion from mean sea level to surface pressure.}
#' }
#' @param pres.data named list, providing the pressure data in CDT station data format. Units: hPa.
#' \itemize{
#' \item{\code{file}: }{character, full path to the file containing the stations data}
#' \item{\code{sep}: }{character, column separator of the data}
#' \item{\code{na.strings}: }{character, missing values flag}
#' }
#' @param temp.data character, source of temperature data. Valid options: \code{"maxmin"} and \code{"mean"}.
#' \itemize{
#' \item{\code{"maxmin"}: }{the mean temperature is computed from tme minimum and maximum temperature.}
#' \item{\code{"mean"}: }{the temperature data is from a mean temperature data.}
#' }
#' @param tmax.data named list, if \code{temp.data} is equal to \code{"maxmin"}, 
#' providing the maximum temperature data in CDT station data format. Units: degree Celsius.
#' See \code{pres.data} for the elements of the list.
#' @param tmin.data named list, if \code{temp.data} is equal to \code{"maxmin"}, 
#' providing the minimum temperature data in CDT station data format. Units: degree Celsius.
#' See \code{pres.data} for the elements of the list.
#' @param tmean.data named list, if \code{temp.data} is equal to \code{"mean"}, 
#' providing the mean temperature data in CDT station data format. Units: degree Celsius.
#' See \code{pres.data} for the elements of the list.
#' @param elev.from character, source of the elevation data. Valid options: \code{"inputPresData"} and \code{"cdtCrdFile"}.
#' \itemize{
#' \item{\code{"inputPresData"}: }{the elevation data is from the pressure data.}
#' \item{\code{"cdtCrdFile"}: }{the elevation data is from a CDT coordinates file. The ID must be the same as the pressure data.}
#' }
#' @param elev.data named list, if \code{elev.from} is equal to \code{"cdtCrdFile"},
#' providing the CDT coordinate file containing the elevation data. Units: meters.
#' \itemize{
#' \item{\code{file}: }{character, full path to the CDT coordinate file}
#' \item{\code{sep}: }{character, column separator of the data}
#' \item{\code{na.strings}: }{character, missing values flag}
#' }
#' @param output named list, the elements of the list are
#' \itemize{
#' \item{\code{file}: }{character, full path to the file to save the converted pressure}
#' \item{\code{sep}: }{character, column separator of the data}
#' \item{\code{na.strings}: }{character, missing values flag}
#' }
#' 
#' @export

pressure_conversion_station <- function(convert = 'sfc2msl',
                                        pres.data = list(file = "", sep = ",", na.strings = "-99"),
                                        temp.data = 'maxmin',
                                        tmax.data = list(file = "", sep = ",", na.strings = "-99"),
                                        tmin.data = list(file = "", sep = ",", na.strings = "-99"),
                                        tmean.data = list(file = "", sep = ",", na.strings = "-99"),
                                        elev.from = 'inputPresData', 
                                        elev.data = list(file = '', sep = ",", na.strings = "-99"),
                                        output = list(file = '', sep = ",", na.strings = "-99")
                                       )
{
    stnpars_read <- list(stringsAsFactors = FALSE, colClasses = "character")
    pres_pars <- list(file = "", sep = ",", na.strings = "-99")
    pres.data <- init.default.list.args(pres.data, pres_pars)
    presData <- do.call(read.table, c(pres.data, stnpars_read))
    presData <- splitCDTData0(presData, GUI = FALSE)
    if(is.null(presData)){
        stop('Unable to read pressure data')
        # return(NULL)
    }

    if(elev.from == 'inputPresData'){
        if(is.null(presData$elv)){
            stop('No elevation data found from the pressure data')
            # return(NULL)
        }
        elev <- presData$elv
    }else if(elev.from == 'cdtCrdFile'){
        elev_pars <- list(file = "", sep = ",", na.strings = "-99", header = TRUE)
        elev.data <- init.default.list.args(elev.data, elev_pars)
        elevData <- do.call(read.table, c(elev.data, stnpars_read))
        idElev <- elevData[, 1]
        # elevData <- list(id = elevData[, 1],
        #                  lon = as.numeric(elevData[, 3]),
        #                  lat = as.numeric(elevData[, 4]),
        #                  elv = as.numeric(elevData[, 5]))

        if(length(presData$id) != length(idElev)){
            stop('Pressure and elevation data stations do not match')
            # return(NULL)
        }

        if(!all(presData$id == idElev)){
            stop('Pressure and elevation data stations do not match')
            # return(NULL)
        }

        elev <- as.numeric(elevData[, 5])
    }else{
        stop('Unknown elevation data source')
        # return(NULL)
    }

    if(temp.data == 'mean'){
        tmean_pars <- list(file = "", sep = ",", na.strings = "-99")
        tmean.data <- init.default.list.args(tmean.data, tmean_pars)
        tempData <- do.call(read.table, c(tmean.data, stnpars_read))
        tempData <- splitCDTData0(tempData, GUI = FALSE)
        if(is.null(tempData)){
            stop('Unable to read TMEAN data')
            # return(NULL)
        }
    }else if(temp.data == 'maxmin'){
        tmax_pars <- list(file = "", sep = ",", na.strings = "-99")
        tmax.data <- init.default.list.args(tmax.data, tmax_pars)
        tmaxData <- do.call(read.table, c(tmax.data, stnpars_read))
        tmaxData <- splitCDTData0(tmaxData, GUI = FALSE)
        if(is.null(tmaxData)){
            stop('Unable to read TMAX data')
            # return(NULL)
        }

        tmin_pars <- list(file = "", sep = ",", na.strings = "-99")
        tmin.data <- init.default.list.args(tmin.data, tmin_pars)
        tminData <- do.call(read.table, c(tmin.data, stnpars_read))
        tminData <- splitCDTData0(tminData, GUI = FALSE)
        if(is.null(tminData)){
            stop('Unable to read TMIN data')
            # return(NULL)
        }

        if(length(tmaxData$id) != length(tminData$id)){
            stop('Maximum and Minimum temperature data stations do not match')
            # return(NULL)
        }
        if(!all(tmaxData$id == tminData$id)){
            stop('Maximum and Minimum temperature data stations do not match')
            # return(NULL)
        }

        it <- match(tmaxData$dates, tminData$dates)
        tminData$dates <- tminData$dates[it]
        tminData$data <- tminData$data[it, , drop = FALSE]
        tempData <- tmaxData
        tempData$data <- (tmaxData$data + tminData$data)/2
    }else{
        stop('Unknown temperature data source')
        # return(NULL)
    }

    if(length(presData$id) != length(tempData$id)){
        stop('Pressure and temperature data stations do not match')
        # return(NULL)
    }

    if(!all(presData$id == tempData$id)){
        stop('Pressure and temperature data stations do not match')
        # return(NULL)
    }

    it <- match(presData$dates, tempData$dates)
    tempData$dates <- tempData$dates[it]
    tempData$data <- tempData$data[it, , drop = FALSE]

    mslData <- mSLP_checkDim(presData$data, tempData$data, elev, presData$lat)
    msl <- do.call(mSLP_corrections, mslData)

    if(convert == 'sfc2msl'){
        out <- presData$data * 10^msl
    }else if(convert == 'msl2sfc'){
        out <- presData$data / (10^msl)
    }else{
        stop('Unknown conversion type')
        # return(NULL)
    }
    presData$data <- round(out, 2)

    capt <- c('ID', 'LON', 'LAT')
    if(!is.null(presData$elv)) capt <- c(capt, 'ELV')

    dat <- do.call(rbind, presData[c('id', 'lon', 'lat', 'elv', 'data')])
    dat <- cbind(c(capt, presData$dates), dat)

    utils::write.table(dat, file = output$file, sep = output$sep, na = output$na.strings,
                       row.names = FALSE, col.names = FALSE, quote = FALSE)
    return(0)
}

#' Daily surface and mean sea level pressure conversion for a netCDF dataset.
#'
#' Function to convert surface and mean sea level pressure in both way.
#' 
#' @param convert character, the direction of conversion. Valid options: \code{"sfc2msl"} and \code{"msl2sfc"}.
#' \itemize{
#' \item{\code{"sfc2msl"}: }{conversion from surface to mean sea level pressure.}
#' \item{\code{"msl2sfc"}: }{conversion from mean sea level to surface pressure.}
#' }
#' @param start.date character, the start date of the data to be converted in the form \code{YYYY-MM-DD}.
#' @param end.date character, the end date of the data to be converted in the form \code{YYYY-MM-DD}.
#' @param pres.data named list, providing the pressure netCDF dataset. Units: hPa. 
#' Surface pressure or mean sea level pressure, depending on \code{convert}.
#' \itemize{
#' \item{\code{dir}: }{character, full path to the directory containing the netCDF files.}
#' \item{\code{format}: }{character, format of the netCDF file names}
#' \item{\code{varid}: }{character, name of the variable to read from the netCDF data}
#' \item{\code{ilon}: }{integer, order for the longitude dimension of the variable. 
#' Example: if the variable "pres" has the dimension order [Lat, Lon] then \code{ilon} must be 2}
#' \item{\code{ilat}: }{integer, order for the latitude dimension of the variable.}
#' }
#' @param temp.data character, source of temperature data. Valid options: \code{"maxmin"} and \code{"mean"}.
#' \itemize{
#' \item{\code{"maxmin"}: }{the mean temperature is computed from tme minimum and maximum temperature.}
#' \item{\code{"mean"}: }{the temperature data is from a mean temperature data.}
#' }
#' @param tmax.data named list, if \code{temp.data} is equal to \code{"maxmin"}, 
#' providing the maximum temperature netCDF dataset. Units: degree Celsius.
#' See \code{pres.data} for the elements of the list.
#' @param tmin.data named list, if \code{temp.data} is equal to \code{"maxmin"}, 
#' providing the minimum temperature netCDF dataset. Units: degree Celsius.
#' See \code{pres.data} for the elements of the list.
#' @param tmean.data named list, if \code{temp.data} is equal to \code{"mean"}, 
#' providing the mean temperature netCDF dataset. Units: degree Celsius.
#' See \code{pres.data} for the elements of the list.
#' @param elev.data named list, providing the Digital Elevation Model in netCDF format. Units: meters.
#' \itemize{
#' \item{\code{file}: }{character, full path to the netCDF file containing the elevation data.}
#' \item{\code{varid}: }{character, name of the variable to read from the netCDF data.}
#' \item{\code{ilon}: }{integer, order for the longitude dimension of the variable.}
#' \item{\code{ilat}: }{integer, order for the latitude dimension of the variable.}
#' }
#' @param output named list, the elements of the list are
#' \itemize{
#' \item{\code{dir}: }{character, full path to the directory to save the converted pressure}
#' \item{\code{format}: }{character, format of the output netCDF file names, the dates are represented by \code{\%S}}
#' }
#' 
#' @export

pressure_conversion_netcdf <- function(convert = 'sfc2msl',
                                       start.date = '1991-01-01', end.date = '2020-12-31', 
                                       pres.data = list(dir = "", format = "pres_%s%s%s.nc",
                                                        varid = "pres", ilon = 1, ilat = 2),
                                       temp.data = 'maxmin',
                                       tmax.data = list(dir = "", format = "tmax_%s%s%s.nc",
                                                        varid = "tmax", ilon = 1, ilat = 2),
                                       tmin.data = list(dir = "", format = "tmin_%s%s%s.nc",
                                                        varid = "tmin", ilon = 1, ilat = 2),
                                       tmean.data = list(dir = "", format = "tmean_%s%s%s.nc",
                                                        varid = "tmean", ilon = 1, ilat = 2),
                                       elev.data = list(file = "", varid = "z", ilon = 1, ilat = 2),
                                       output = list(dir = "", format = "prmsl_%S.nc")
                                      )
{
    if(!is.null(pres.data$dir)){
        if(!dir.exists(pres.data$dir)){
            msg <- paste("Folder containing the netCDF pressure data does not exist", ":", pres.data$dir)
            stop(msg)
            # return(NULL)
        }else{
            pres_pars <- list(dir = "", format = "pres_%s%s%s.nc",
                              varid = "pres", ilon = 1, ilat = 2)
            pres.data <- init.default.list.args(pres.data, pres_pars)
        }
    }else{
        stop("No folder containing the netCDF pressure data provided")
        # return(NULL)
    }

    ##########

    if(temp.data == 'mean'){
        if(!is.null(tmean.data$dir)){
            if(!dir.exists(tmean.data$dir)){
                msg <- paste("Folder containing the netCDF TMEAN data does not exist", ":", tmean.data$dir)
                stop(msg)
                # return(NULL)
            }else{
                tmean_pars <- list(dir = "", format = "tmean_%s%s%s.nc",
                                  varid = "tmean", ilon = 1, ilat = 2)
                tmean.data <- init.default.list.args(tmean.data, tmean_pars)
            }
        }else{
            stop("No folder containing the netCDF TMEAN data provided")
            # return(NULL)
        }
    }else if(temp.data == 'maxmin'){
        if(!is.null(tmax.data$dir)){
            if(!dir.exists(tmax.data$dir)){
                msg <- paste("Folder containing the netCDF TMAX data does not exist", ":", tmax.data$dir)
                stop(msg)
                # return(NULL)
            }else{
                tmax_pars <- list(dir = "", format = "tmax_%s%s%s.nc",
                                  varid = "tmax", ilon = 1, ilat = 2)
                tmax.data <- init.default.list.args(tmax.data, tmax_pars)
            }
        }else{
            stop("No folder containing the netCDF TMAX data provided")
            # return(NULL)
        }

        if(!is.null(tmin.data$dir)){
            if(!dir.exists(tmin.data$dir)){
                msg <- paste("Folder containing the netCDF TMIN data does not exist", ":", tmin.data$dir)
                stop(msg)
                # return(NULL)
            }else{
                tmin_pars <- list(dir = "", format = "tmin_%s%s%s.nc",
                                  varid = "tmin", ilon = 1, ilat = 2)
                tmin.data <- init.default.list.args(tmin.data, tmin_pars)
            }
        }else{
            stop("No folder containing the netCDF TMIN data provided")
            # return(NULL)
        }
    }else{
        stop('Unknown temperature data source')
        # return(NULL)
    }

    ##########

    if(file.exists(elev.data$file)){
        elev_pars <- list(file = "", varid = "z", ilon = 1, ilat = 2)
        elev.data <- init.default.list.args(elev.data, elev_pars)
    }else{
        stop("The netCDF file containing the elevation does not exist")
        # return(NULL)
    }

    demData <- get.ncData.value(elev.data)
    demData$z[demData$z < 0] <- 0
    dem_grd <- defSpatialPixels(demData)

    ##########

    output_pars <- list(dir = "", format = "prmsl_%S.nc")
    output <- init.default.list.args(output, output_pars)
    output$format <- gsub('%S', '%s', output$format)

    ##########

    start_date <- as.Date(start.date)
    end_date <- as.Date(end.date)
    date.range <- list(start.year = as.numeric(format(start_date, '%Y')),
                       start.mon = as.numeric(format(start_date, '%m')),
                       start.day = as.numeric(format(start_date, '%d')),
                       end.year = as.numeric(format(end_date, '%Y')),
                       end.mon = as.numeric(format(end_date, '%m')),
                       end.day = as.numeric(format(end_date, '%d')))

    ##########

    pres_ncInfo <- get.ncInfo.params(pres.data, date.range, 'daily')
    if(is.null(pres_ncInfo)){
        stop("No netCDF pressure files found")
        # return(NULL)
    }

    pres_grd <- defSpatialPixels(pres_ncInfo$ncinfo[c('lon', 'lat')])
    pres_ncInfo$dates <- pres_ncInfo$dates[pres_ncInfo$exist]
    pres_ncInfo$ncfiles <- pres_ncInfo$ncfiles[pres_ncInfo$exist]

    ##########

    if(temp.data == 'maxmin'){
        tmax_ncInfo <- get.ncInfo.params(tmax.data, date.range, 'daily')
        if(is.null(tmax_ncInfo)){
            stop("No netCDF TMAX files found")
            # return(NULL)
        }
        tmin_ncInfo <- get.ncInfo.params(tmin.data, date.range, 'daily')
        if(is.null(tmin_ncInfo)){
            stop("No netCDF TMIAN files found")
            # return(NULL)
        }

        tmax_grd <- defSpatialPixels(tmax_ncInfo$ncinfo[c('lon', 'lat')])
        tmin_grd <- defSpatialPixels(tmin_ncInfo$ncinfo[c('lon', 'lat')])
        maxmin_diff <- is.diffSpatialPixelsObj(tmax_grd, tmin_grd, tol = 1e-07)

        if(maxmin_diff){
            stop('Maximum and Minimum temperature grid do not match')
            # return(NULL)
        }

        tmax_diff <- is.diffSpatialPixelsObj(pres_grd, tmax_grd, tol = 1e-07)
        if(tmax_diff){
            stop('Pressure and temperature grid do not match')
            # return(NULL)
        }

        ######

        tmax_ncInfo$dates <- tmax_ncInfo$dates[tmax_ncInfo$exist]
        tmax_ncInfo$ncfiles <- tmax_ncInfo$ncfiles[tmax_ncInfo$exist]

        tmin_ncInfo$dates <- tmin_ncInfo$dates[tmin_ncInfo$exist]
        tmin_ncInfo$ncfiles <- tmin_ncInfo$ncfiles[tmin_ncInfo$exist]

        ######

        tdates <- intersect(tmax_ncInfo$dates, tmin_ncInfo$dates)
        if(length(tdates) == 0){
            stop("Maximum and Minimum temperature dates do not overlap")
            # return(NULL)
        }

        itmx <- match(tdates, tmax_ncInfo$dates)
        tmax_ncInfo$dates <- tmax_ncInfo$dates[itmx]
        tmax_ncInfo$ncfiles <- tmax_ncInfo$ncfiles[itmx]

        itmn <- match(tdates, tmin_ncInfo$dates)
        tmin_ncInfo$dates <- tmin_ncInfo$dates[itmn]
        tmin_ncInfo$ncfiles <- tmin_ncInfo$ncfiles[itmn]

        idates <- intersect(pres_ncInfo$dates, tdates)
        if(length(idates) == 0){
            stop('Pressure and temperature dates do not overlap')
            # return(NULL)
        }

        ipres <- match(idates, pres_ncInfo$dates)
        pres_ncInfo$dates <- pres_ncInfo$dates[ipres]
        pres_ncInfo$ncfiles <- pres_ncInfo$ncfiles[ipres]

        itmx <- match(idates, tmax_ncInfo$dates)
        tmax_ncInfo$dates <- tmax_ncInfo$dates[itmx]
        tmax_ncInfo$ncfiles <- tmax_ncInfo$ncfiles[itmx]

        itmn <- match(idates, tmin_ncInfo$dates)
        tmin_ncInfo$dates <- tmin_ncInfo$dates[itmn]
        tmin_ncInfo$ncfiles <- tmin_ncInfo$ncfiles[itmn]
    }else{
        tmean_ncInfo <- get.ncInfo.params(tmean.data, date.range, 'daily')
        if(is.null(tmean_ncInfo)){
            stop("No netCDF TMEAN files found")
            # return(NULL)
        }

        tmean_grd <- defSpatialPixels(tmean_ncInfo$ncinfo[c('lon', 'lat')])
        tmean_diff <- is.diffSpatialPixelsObj(pres_grd, tmean_grd, tol = 1e-07)
        if(tmean_diff){
            stop('Pressure and temperature grid do not match')
            # return(NULL)
        }

        #####

        tmean_ncInfo$dates <- tmean_ncInfo$dates[tmean_ncInfo$exist]
        tmean_ncInfo$ncfiles <- tmean_ncInfo$ncfiles[tmean_ncInfo$exist]

        #####

        idates <- intersect(pres_ncInfo$dates, tmean_ncInfo$dates)
        if(length(idates) == 0){
            stop('Pressure and temperature dates do not overlap')
            # return(NULL)
        }

        ipres <- match(idates, pres_ncInfo$dates)
        pres_ncInfo$dates <- pres_ncInfo$dates[ipres]
        pres_ncInfo$ncfiles <- pres_ncInfo$ncfiles[ipres]

        itm <- match(idates, tmean_ncInfo$dates)
        tmean_ncInfo$dates <- tmean_ncInfo$dates[itm]
        tmean_ncInfo$ncfiles <- tmean_ncInfo$ncfiles[itm]
    }

    ##########

    is.regridDEM <- is.diffSpatialPixelsObj(pres_grd, dem_grd, tol = 1e-07)
    if(is.regridDEM){
        demData <- cdt.interp.surface.grid(demData, pres_ncInfo$ncinfo[c('lon', 'lat')])
        names(demData) <- c('lon', 'lat', 'z')
    }else demData <- demData[c('lon', 'lat', 'z')]
    demData$z[demData$z < 0] <- 0

    ##########

    dx <- ncdf4::ncdim_def("Lon", "degreeE", pres_ncInfo$ncinfo$lon)
    dy <- ncdf4::ncdim_def("Lat", "degreeN", pres_ncInfo$ncinfo$lat)
    xydim <- list(dx, dy)

    if(convert == 'sfc2msl'){
        varname <- 'prmsl'
        longname <- "Mean sea level pressure"
    }else if(convert == 'msl2sfc'){
        varname <- 'pres'
        longname <- "Surface pressure"
    }else{
        stop('Unknown conversion type')
        # return(NULL)
    }
    ncmissval <- -9999
    grdncout <- ncdf4::ncvar_def(varname, 'hPa', xydim, ncmissval, prec = 'float',
                              longname = longname, compression = 9)

    ##########

    cdt.file.conf <- file.path(.cdtDir$dirLocal, "config", "cdt_config.json")
    Config <- jsonlite::fromJSON(cdt.file.conf)
    Config <- rapply(Config, trimws, classes = "character", how = "replace")
    .cdtData$Config$parallel <- Config$parallel

    parsL <- doparallel.cond(length(pres_ncInfo$dates) >= 180)

    ret <- cdt.foreach(seq_along(pres_ncInfo$dates), parsL, GUI = FALSE,
                       progress = FALSE, FUN = function(jj)
    {
        nc <- ncdf4::nc_open(pres_ncInfo$ncfiles[jj])
        pres <- ncdf4::ncvar_get(nc, varid = pres_ncInfo$ncinfo$varid)
        ncdf4::nc_close(nc)
        pres <- transposeNCDFData(pres, pres_ncInfo$ncinfo)

        if(temp.data == 'maxmin'){
            nc <- ncdf4::nc_open(tmax_ncInfo$ncfiles[jj])
            tmax <- ncdf4::ncvar_get(nc, varid = tmax_ncInfo$ncinfo$varid)
            ncdf4::nc_close(nc)
            tmax <- transposeNCDFData(tmax, tmax_ncInfo$ncinfo)

            nc <- ncdf4::nc_open(tmin_ncInfo$ncfiles[jj])
            tmin <- ncdf4::ncvar_get(nc, varid = tmin_ncInfo$ncinfo$varid)
            ncdf4::nc_close(nc)
            tmin <- transposeNCDFData(tmin, tmin_ncInfo$ncinfo)

            temp <- (tmax + tmin)/2
        }else{
            nc <- ncdf4::nc_open(tmean_ncInfo$ncfiles[jj])
            temp <- ncdf4::ncvar_get(nc, varid = tmean_ncInfo$ncinfo$varid)
            ncdf4::nc_close(nc)
            temp <- transposeNCDFData(temp, tmean_ncInfo$ncinfo)
        }

        mslData <- mSLP_checkDim(pres, temp, demData$z, as.vector(pres_ncInfo$ncinfo$lat))
        msl <- do.call(mSLP_corrections, mslData)
        out <- switch(convert, 'sfc2msl' = pres * 10^msl, 'msl2sfc' = pres / (10^msl))
        out[is.na(out) | is.nan(out) | is.infinite(out)] <- ncmissval

        outfl <- file.path(output$dir, sprintf(output$format, pres_ncInfo$dates[jj]))
        nc <- ncdf4::nc_create(outfl, grdncout)
        ncdf4::ncvar_put(nc, grdncout, out)
        ncdf4::nc_close(nc)
    })

    return(0)
}

output = list(dir = "", format = "prmsl_%s%s%s.nc")

mSLP_checkDim <- function(pres, temp, elev, lat){
    if(is.vector(pres) && is.vector(elev)){
        if(!is.vector(temp)){
            stop("'temp' must be a vector with the same length as 'pres'")
        }
        if(!is.vector(lat)){
            stop("'lat' must be a vector with the same length as 'pres'")
        }
        len <- c(length(pres), length(temp), length(temp), length(temp))
        if(!all((len[1] - len) == 0)){
            stop("'pres', 'temp', 'elev' and 'lat' must have the same length")
        }
    }else if(is.matrix(pres) && is.vector(elev)){
        if(!is.matrix(temp)){
            stop("'temp' must be a matrix with the same dimensions as 'pres'")
        }
        if(!is.vector(lat)){
            stop("'lat' must be a vector with the same length as the number of column of 'pres'")
        }
        if(!all(dim(pres) == dim(temp))){
            stop("'pres' and 'temp' must have the same dimensions")
        }
        if(ncol(pres) != length(elev)){
            stop("'elev' must be a vector with the same length as the number of column of 'pres'")
        }
        if(ncol(pres) != length(lat)){
            stop("'lat' must be a vector with the same length as the number of column of 'pres'")
        }
        elev <- matrix(elev, nrow(pres), length(elev), byrow = TRUE)
        lat <- matrix(lat, nrow(pres), length(lat), byrow = TRUE)
    }else if(is.matrix(pres) && is.matrix(elev)){
        if(!is.matrix(temp)){
            stop("'temp' must be a matrix with the same dimensions as 'pres'")
        }
        if(!is.vector(lat)){
            stop("'lat' must be a vector with the same length as the number of column of 'pres'")
        }
        if(!all(dim(pres) == dim(temp))){
            stop("'pres' and 'temp' must have the same dimensions")
        }
        if(!all(dim(pres) == dim(elev))){
            stop("'pres' and 'elev' must have the same dimensions")
        }
        if(ncol(pres) != length(lat)){
            stop("'lat' must be a vector with the same length as the number of column of 'pres'")
        }
        lat <- matrix(lat, nrow(pres), length(lat), byrow = TRUE)
    }else{
        stop("Unknown data dimensions")
    }

    list(pres = pres, temp = temp, elev = elev, lat = lat)
}

mSLP_corrections <- function(pres, temp, elev, lat){
    # constants
    # barometric pressure of the air column (hPa)
    b <- 1013.25
    # barometrics constant (m)
    K <- 18400.0
    # coefficient of thermal expansion of the air
    a <- 0.0037
    # constant depending on the figure of the earth
    k <- 0.0026
    # lr is calculated assuming a temperature gradient of 0.5 degC/100 metres. (C/m)
    lr <- 0.005
    # radius of the earth (m)
    R <- 6367324

    # convert latitude  to radians
    phi <- lat * pi / 180
    # delta: altitude difference. Sea level 0m
    dZ <- elev - 0
    # average temperature of air column
    at = temp + (lr * dZ) / 2;
    # vapor pressure [hPa]
    e <- 10^(7.5 * at / (237.3 + at)) * 6.1078
    # correction for atmospheric temperature
    corT <- 1 + a * at
    # correction for humidity
    corH <- 1 / (1 - 0.378 * (e / b))
    # correction for asphericity of earth (latitude)
    corE <- 1 / (1 - (k * cos(2 * phi)))
    # correction for variation of gravity with height
    corG <- 1 + elev / R

    msl <- dZ / (K * corT * corH * corE * corG)

    return(msl)
}
