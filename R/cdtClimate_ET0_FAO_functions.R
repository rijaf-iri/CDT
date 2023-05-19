#' Compute daily reference evapotranspiration for CDT station data format.
#'
#' Function to compute daily reference evapotranspiration using the FAO Penman-Monteith equation.
#' @param tmax.data named list, providing the maximum temperature data in CDT station data format. Units: degree Celsius.
#' \itemize{
#' \item{\code{file}: }{character, full path to the file containing the stations data}
#' \item{\code{sep}: }{character, column separator of the data}
#' \item{\code{na.strings}: }{character, missing values flag}
#' }
#' @param tmin.data named list, providing the minimum temperature data in CDT station data format. Units: degree Celsius.
#' See \code{tmax.data} for the elements of the list.
#' @param rh.from character, source of relative humidity data. Valid options: \code{"minmax"} and \code{"mean"}.
#' \itemize{
#' \item{\code{"minmax"}: }{the humidity data are from the minimum and maximum relative humidity.}
#' \item{\code{"mean"}: }{the humidity data is from a mean relative humidity data.}
#' }
#' @param rhmax.data named list, if \code{rh.from} is equal to \code{"minmax"},
#' providing the maximum relative humidity data in CDT station data format. Units: percentage.
#' See \code{tmax.data} for the elements of the list.
#' @param rhmin.data named list, if \code{rh.from} is equal to \code{"minmax"},
#' providing the minimum relative humidity data in CDT station data format. Units: percentage.
#' See \code{tmax.data} for the elements of the list.
#' @param rhmean.data named list, if \code{rh.from} is equal to \code{"mean"},
#' providing the mean relative humidity data in CDT station data format. Units: percentage.
#' See \code{tmax.data} for the elements of the list.
#' @param wff10.data named list, providing the wind speed at 10 meters data in CDT station data format. Units: m/s.
#' See \code{tmax.data} for the elements of the list.
#' @param pres.data named list, providing the surface pressure data in CDT station data format. Units: hPa.
#' See \code{tmax.data} for the elements of the list.
#' @param rad.data named list, providing the solar radiation flux data in CDT station data format. Units: W/m^2.
#' See \code{tmax.data} for the elements of the list.
#' @param elev.from character, source of the elevation data. Valid options: \code{"inputPresData"}, \code{"cdtCrdFile"} and \code{"netcdfDEM"}.
#' \itemize{
#' \item{\code{"inputTmaxData"}: }{the elevation data is from the maximum temperature data.}
#' \item{\code{"cdtCrdFile"}: }{the elevation data is from a CDT coordinates file. The ID must be the same as the maximum temperature data.}
#' \item{\code{"netcdfDEM"}: }{the elevation data is from the pressure data.}
#' }
#' Default is \code{"netcdfDEM"}.
#' @param elev.data named list, if \code{elev.from} is equal to \code{"cdtCrdFile"},
#' set the elements of the list providing the CDT coordinate file containing the elevation data. Units: meters.
#' \itemize{
#' \item{\code{file}: }{character, full path to the CDT coordinate file}
#' \item{\code{sep}: }{character, column separator of the data}
#' \item{\code{na.strings}: }{character, missing values flag}
#' }
#' If \code{elev.from} is equal to \code{"netcdfDEM"} set the elements of the list providing the Digital Elevation Model in netCDF format. Units: meters.
#' \itemize{
#' \item{\code{file}: }{character, full path to the netCDF file containing the elevation data.}
#' \item{\code{varid}: }{character, name of the variable to read from the netCDF data.}
#' \item{\code{ilon}: }{integer, order for the longitude dimension of the variable.}
#' \item{\code{ilat}: }{integer, order for the latitude dimension of the variable.}
#' }
#' @param output named list, the elements of the list are
#' \itemize{
#' \item{\code{file}: }{character, full path to the file to save the calculated reference evapotranspiration}
#' \item{\code{sep}: }{character, column separator of the data}
#' \item{\code{na.strings}: }{character, missing values flag}
#' }
#' 
#' @references 
#' Allen, R.G., Pereira, L.S., Raes, D., and Smith, M. 1998. Crop evapotranspiration: Guidelines for computing crop requirements. Irrigation and Drainage PaperNo. 56, FAO, Rome, Italy 300 p.
#' \url{https://www.fao.org/3/X0490E/x0490e00.htm}
#' 
#' @export

ET0_Penman_Monteith_FAO_station <- function(
       tmax.data = list(file = "", sep = ",", na.strings = "-99"),
       tmin.data = list(file = "", sep = ",", na.strings = "-99"),
       rh.from = 'minmax',
       rhmax.data = list(file = "", sep = ",", na.strings = "-99"),
       rhmin.data = list(file = "", sep = ",", na.strings = "-99"),
       rhmean.data = list(file = "", sep = ",", na.strings = "-99"),
       wff10.data = list(file = "", sep = ",", na.strings = "-99"),
       pres.data = list(file = "", sep = ",", na.strings = "-99"),
       rad.data = list(file = "", sep = ",", na.strings = "-99"),
       elev.from = 'netcdfDEM',
       elev.data = list(file = "", varid = "z", ilon = 1, ilat = 2),
       output = list(file = '', sep = ",", na.strings = "-99")
    )
{
    stnpars_read <- list(stringsAsFactors = FALSE, colClasses = "character")

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

    idstn <- intersect(tmaxData$id, tminData$id)
    if(length(idstn) == 0){
        stop('Maximum and Minimum temperature data stations do not match')
        # return(NULL)
    }
    dates <- intersect(tmaxData$dates, tminData$dates)
    if(length(dates) == 0){
        stop('Maximum and Minimum temperature dates do not overlap')
        # return(NULL)
    }

    if(rh.from == 'minmax'){
        rhmax_pars <- list(file = "", sep = ",", na.strings = "-99")
        rhmax.data <- init.default.list.args(rhmax.data, rhmax_pars)
        rhmaxData <- do.call(read.table, c(rhmax.data, stnpars_read))
        rhmaxData <- splitCDTData0(rhmaxData, GUI = FALSE)
        if(is.null(rhmaxData)){
            stop('Unable to read RHMAX data')
            # return(NULL)
        }

        idstn <- intersect(idstn, rhmaxData$id)
        if(length(idstn) == 0){
            stop('Temperatures and RHMAX stations do not match')
            # return(NULL)
        }
        dates <- intersect(dates, rhmaxData$dates)
        if(length(dates) == 0){
            stop('Temperatures and RHMAX dates do not overlap')
            # return(NULL)
        }

        rhmin_pars <- list(file = "", sep = ",", na.strings = "-99")
        rhmin.data <- init.default.list.args(rhmin.data, rhmin_pars)
        rhminData <- do.call(read.table, c(rhmin.data, stnpars_read))
        rhminData <- splitCDTData0(rhminData, GUI = FALSE)
        if(is.null(rhminData)){
            stop('Unable to read RHMIN data')
            # return(NULL)
        }

        idstn <- intersect(idstn, rhminData$id)
        if(length(idstn) == 0){
            stop('Temperatures and RHMIN stations do not match')
            # return(NULL)
        }
        dates <- intersect(dates, rhminData$dates)
        if(length(dates) == 0){
            stop('Temperatures and RHMIN dates do not overlap')
            # return(NULL)
        }
    }else if(rh.from == 'mean'){
        rhmean_pars <- list(file = "", sep = ",", na.strings = "-99")
        rhmean.data <- init.default.list.args(rhmean.data, rhmean_pars)
        rhmeanData <- do.call(read.table, c(rhmean.data, stnpars_read))
        rhmeanData <- splitCDTData0(rhmeanData, GUI = FALSE)
        if(is.null(rhmeanData)){
            stop('Unable to read RHMEAN data')
            # return(NULL)
        }

        idstn <- intersect(idstn, rhmeanData$id)
        if(length(idstn) == 0){
            stop('Temperatures and RHMEAN stations do not match')
            # return(NULL)
        }
        dates <- intersect(dates, rhmeanData$dates)
        if(length(dates) == 0){
            stop('Temperatures and RHMEAN dates do not overlap')
            # return(NULL)
        }
    }else{
        stop('Unknown humidity data source')
        # return(NULL)
    }

    wff10_pars <- list(file = "", sep = ",", na.strings = "-99")
    wff10.data <- init.default.list.args(wff10.data, wff10_pars)
    wff10Data <- do.call(read.table, c(wff10.data, stnpars_read))
    wff10Data <- splitCDTData0(wff10Data, GUI = FALSE)
    if(is.null(wff10Data)){
        stop('Unable to read wind speed data at 10 meters')
        # return(NULL)
    }

    idstn <- intersect(idstn, wff10Data$id)
    if(length(idstn) == 0){
        stop('Temperatures and wind speed stations do not match')
        # return(NULL)
    }
    dates <- intersect(dates, wff10Data$dates)
    if(length(dates) == 0){
        stop('Temperature and wind speed dates do not overlap')
        # return(NULL)
    }

    pres_pars <- list(file = "", sep = ",", na.strings = "-99")
    pres.data <- init.default.list.args(pres.data, pres_pars)
    presData <- do.call(read.table, c(pres.data, stnpars_read))
    presData <- splitCDTData0(presData, GUI = FALSE)
    if(is.null(presData)){
        stop('Unable to read pressure data')
        # return(NULL)
    }

    idstn <- intersect(idstn, presData$id)
    if(length(idstn) == 0){
        stop('Temperatures and pressure stations do not match')
        # return(NULL)
    }
    dates <- intersect(dates, presData$dates)
    if(length(dates) == 0){
        stop('Temperature and pressure dates do not overlap')
        # return(NULL)
    }

    rad_pars <- list(file = "", sep = ",", na.strings = "-99")
    rad.data <- init.default.list.args(rad.data, rad_pars)
    radData <- do.call(read.table, c(rad.data, stnpars_read))
    radData <- splitCDTData0(radData, GUI = FALSE)
    if(is.null(radData)){
        stop('Unable to read radiation data')
        # return(NULL)
    }

    idstn <- intersect(idstn, radData$id)
    if(length(idstn) == 0){
        stop('Temperatures and radiation stations do not match')
        # return(NULL)
    }
    dates <- intersect(dates, radData$dates)
    if(length(dates) == 0){
        stop('Temperature and radiation dates do not overlap')
        # return(NULL)
    }

    ############

    output_pars <- list(file = '', sep = ",", na.strings = "-99")
    output <- init.default.list.args(output, output_pars)

    if(trimws(output$file) == ''){
        stop('No file to save the computed evapotranspiration')
        # return(NULL)
    }

    ############
    istn <- match(idstn, tmaxData$id)
    out <- tmaxData
    out$id <- idstn
    out$lon <- tmaxData$lon[istn]
    out$lat <- tmaxData$lat[istn]
    out$elv <- tmaxData$elv[istn]
    out$dates <- dates
    out$data <- NA

    #############
    if(elev.from == 'inputTmaxData'){
        if(is.null(out$elv)){
            stop('No elevation data found from the maximum temperature data')
            # return(NULL)
        }
        if(all(is.na(out$elv))){
            stop('All elevation data from the maximum temperature data are missing')
            # return(NULL)
        }
    }else if(elev.from == 'cdtCrdFile'){
        elev_pars <- list(file = "", sep = ",", na.strings = "-99", header = TRUE)
        elev.data <- init.default.list.args(elev.data, elev_pars)
        elevData <- do.call(read.table, c(elev.data, stnpars_read))
        idElev <- match(idstn, elevData[, 1])
        if(length(idElev) == 0){
            stop('Temperature data and coordinates file stations IDs do not match')
            # return(NULL)
        }
        out$elv <- as.numeric(elevData[idElev, 5])
        if(all(is.na(out$elv))){
            stop('No elevation data found from the CDT coordinates file')
            # return(NULL)
        }
    }else if(elev.from == 'netcdfDEM'){
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
        elv_loc <- do.call(data.frame, out[c('lon', 'lat')])
        sp::coordinates(elv_loc) <- c('lon', 'lat')
        out$elv <- demData$z[sp::over(elv_loc, dem_grd)]
        if(all(is.na(out$elv))){
            stop('All elevation data from DEM are missing')
            # return(NULL)
        }
    }else{
        stop('Unknown elevation data source')
        # return(NULL)
    }

    ################
    tmaxData <- tmaxData$data[match(dates, tmaxData$dates), match(idstn, tmaxData$id)]
    tminData <- tminData$data[match(dates, tminData$dates), match(idstn, tminData$id)]
    if(rh.from == 'minmax'){
        rhmaxData <- rhmaxData$data[match(dates, rhmaxData$dates), match(idstn, rhmaxData$id)]
        rhminData <- rhminData$data[match(dates, rhminData$dates), match(idstn, rhminData$id)]
    }else{
        rhmeanData <- rhmeanData$data[match(dates, rhmeanData$dates), match(idstn, rhmeanData$id)]
    }
    wff10Data <- wff10Data$data[match(dates, wff10Data$dates), match(idstn, wff10Data$id)]
    presData <- presData$data[match(dates, presData$dates), match(idstn, presData$id)]
    radData <- radData$data[match(dates, radData$dates), match(idstn, radData$id)]

    elevData <- matrix(out$elv, nrow = length(out$dates), ncol = length(out$elv), byrow = TRUE)

    ################

    # daily extraterrestrial radiation W/m^2
    Ra <- extraterrestrial_radiation_daily(out$lat, out$dates)
    # convert to MJ m^-2 day^-1
    Ra <- 0.0864 * Ra

    ########
    if(rh.from == 'minmax'){
        rhmeanData <- NA
    }else{
        rhmaxData <- NA
        rhminData <- NA
    }

    et0 <- ET0_FAO(tmaxData, tminData, wff10Data,
                   presData, radData, Ra, elevData, 
                   rh.from, rhmaxData, rhminData, rhmeanData)

    et0[is.nan(et0) | is.infinite(et0)] <- NA
    out$data <- round(et0, 1)

    ################

    capt <- c('ID', 'LON', 'LAT', 'ELV')
    dat <- do.call(rbind, out[c('id', 'lon', 'lat', 'elv', 'data')])
    dat <- cbind(c(capt, out$dates), dat)

    utils::write.table(dat, file = output$file, sep = output$sep, na = output$na.strings,
                       row.names = FALSE, col.names = FALSE, quote = FALSE)
    return(0)
}

#' Compute daily reference evapotranspiration for a netCDF dataset.
#'
#' Function to compute daily reference evapotranspiration using the FAO Penman-Monteith equation.
#' @param start.date character, the start date of the data to be calculated in the form \code{YYYY-MM-DD}.
#' @param end.date character, the end date of the data to be calculated in the form \code{YYYY-MM-DD}.
#' @param tmax.data named list, providing the maximum temperature netCDF dataset. Units: degree Celsius.
#' \itemize{
#' \item{\code{dir}: }{character, full path to the directory containing the netCDF files.}
#' \item{\code{format}: }{character, format of the netCDF file names}
#' \item{\code{varid}: }{character, name of the variable to read from the netCDF data}
#' \item{\code{ilon}: }{integer, order for the longitude dimension of the variable. 
#' Example: if the variable "pres" has the dimension order [Lat, Lon] then \code{ilon} must be 2}
#' \item{\code{ilat}: }{integer, order for the latitude dimension of the variable.}
#' }
#' @param tmin.data named list, providing the minimum temperature netCDF dataset. Units: degree Celsius.
#' See \code{tmax.data} for the elements of the list.
#' @param rh.from character, source of relative humidity data. Valid options: \code{"minmax"} and \code{"mean"}.
#' \itemize{
#' \item{\code{"minmax"}: }{the humidity data are from the minimum and maximum relative humidity.}
#' \item{\code{"mean"}: }{the humidity data is from a mean relative humidity data.}
#' }
#' @param rhmax.data named list, if \code{rh.from} is equal to \code{"minmax"},
#' providing the maximum relative humidity netCDF dataset. Units: percentage.
#' See \code{tmax.data} for the elements of the list.
#' @param rhmin.data named list, if \code{rh.from} is equal to \code{"minmax"},
#' providing the minimum relative humidity netCDF dataset. Units: percentage.
#' See \code{tmax.data} for the elements of the list.
#' @param rhmean.data named list, if \code{rh.from} is equal to \code{"mean"},
#' providing the mean relative humidity netCDF dataset. Units: percentage.
#' See \code{tmax.data} for the elements of the list.
#' @param wff10.data named list, providing the wind speed at 10 meters netCDF dataset. Units: m/s.
#' See \code{tmax.data} for the elements of the list.
#' @param pres.data named list, providing the surface pressure netCDF dataset. Units: hPa.
#' See \code{tmax.data} for the elements of the list.
#' @param rad.data named list, providing the solar radiation flux netCDF dataset. Units: W/m^2.
#' See \code{tmax.data} for the elements of the list.
#' @param elev.data named list, providing the Digital Elevation Model in netCDF format. Units: meters.
#' \itemize{
#' \item{\code{file}: }{character, full path to the netCDF file containing the elevation data.}
#' \item{\code{varid}: }{character, name of the variable to read from the netCDF data.}
#' \item{\code{ilon}: }{integer, order for the longitude dimension of the variable.}
#' \item{\code{ilat}: }{integer, order for the latitude dimension of the variable.}
#' }
#' @param output named list, the elements of the list are
#' \itemize{
#' \item{\code{dir}: }{character, full path to the directory to save the calculated reference evapotranspiration}
#' \item{\code{format}: }{character, format of the output netCDF file names, the dates are represented by \code{\%S}}
#' }
#' 
#' @references 
#' Allen, R.G., Pereira, L.S., Raes, D., and Smith, M. 1998. Crop evapotranspiration: Guidelines for computing crop requirements. Irrigation and Drainage PaperNo. 56, FAO, Rome, Italy 300 p.
#' \url{https://www.fao.org/3/X0490E/x0490e00.htm}
#' 
#' @export

ET0_Penman_Monteith_FAO_netcdf <- function(start.date = '1991-01-01', end.date = '2020-12-31',
       tmax.data = list(dir = "", format = "tmax_%s%s%s.nc", varid = "tmax", ilon = 1, ilat = 2),
       tmin.data = list(dir = "", format = "tmin_%s%s%s.nc", varid = "tmin", ilon = 1, ilat = 2),
       rh.from = 'minmax',
       rhmax.data = list(dir = "", format = "rhmax_%s%s%s.nc", varid = "rhmax", ilon = 1, ilat = 2),
       rhmin.data = list(dir = "", format = "rhmin_%s%s%s.nc", varid = "rhmin", ilon = 1, ilat = 2),
       rhmean.data = list(dir = "", format = "rhmean_%s%s%s.nc", varid = "rhmean", ilon = 1, ilat = 2),
       wff10.data = list(dir = "", format = "wff_%s%s%s.nc", varid = "wsp", ilon = 1, ilat = 2),
       pres.data = list(dir = "", format = "pres_%s%s%s.nc", varid = "pres", ilon = 1, ilat = 2),
       rad.data = list(dir = "", format = "rad_%s%s%s.nc", varid = "rad", ilon = 1, ilat = 2),
       elev.data = list(file = "", varid = "z", ilon = 1, ilat = 2),
       output = list(dir = "", format = "et0_%S.nc")
    )
{
    if(!is.null(tmax.data$dir)){
        if(!dir.exists(tmax.data$dir)){
            msg <- paste("Folder containing the netCDF maximum temperature data does not exist", ":", tmax.data$dir)
            stop(msg)
            # return(NULL)
        }else{
            tmax_pars <- list(dir = "", format = "tmax_%s%s%s.nc", varid = "tmax", ilon = 1, ilat = 2)
            tmax.data <- init.default.list.args(tmax.data, tmax_pars)
        }
    }else{
        stop("No folder containing the netCDF maximum temperature data provided")
        # return(NULL)
    }
    if(!is.null(tmin.data$dir)){
        if(!dir.exists(tmin.data$dir)){
            msg <- paste("Folder containing the netCDF minimum temperature data does not exist", ":", tmin.data$dir)
            stop(msg)
            # return(NULL)
        }else{
            tmin_pars <- list(dir = "", format = "tmin_%s%s%s.nc", varid = "tmin", ilon = 1, ilat = 2)
            tmin.data <- init.default.list.args(tmin.data, tmin_pars)
        }
    }else{
        stop("No folder containing the netCDF minimum temperature data provided")
        # return(NULL)
    }
    if(rh.from == 'minmax'){
        if(!is.null(rhmax.data$dir)){
            if(!dir.exists(rhmax.data$dir)){
                msg <- paste("Folder containing the netCDF maximum relative humidity data does not exist", ":", rhmax.data$dir)
                stop(msg)
                # return(NULL)
            }else{
                rhmax_pars <- list(dir = "", format = "rhmax_%s%s%s.nc", varid = "rhmax", ilon = 1, ilat = 2)
                rhmax.data <- init.default.list.args(rhmax.data, rhmax_pars)
            }
        }else{
            stop("No folder containing the netCDF maximum relative humidity data provided")
            # return(NULL)
        }
        if(!is.null(rhmin.data$dir)){
            if(!dir.exists(rhmin.data$dir)){
                msg <- paste("Folder containing the netCDF minimum relative humidity data does not exist", ":", rhmin.data$dir)
                stop(msg)
                # return(NULL)
            }else{
                rhmin_pars <- list(dir = "", format = "rhmin_%s%s%s.nc", varid = "rhmin", ilon = 1, ilat = 2)
                rhmin.data <- init.default.list.args(rhmin.data, rhmin_pars)
            }
        }else{
            stop("No folder containing the netCDF minimum relative humidity data provided")
            # return(NULL)
        }
    }else if(rh.from == 'mean'){
        if(!is.null(rhmean.data$dir)){
            if(!dir.exists(rhmean.data$dir)){
                msg <- paste("Folder containing the netCDF mean relative humidity data does not exist", ":", rhmean.data$dir)
                stop(msg)
                # return(NULL)
            }else{
                rhmean_pars <- list(dir = "", format = "rhmean_%s%s%s.nc", varid = "rhmean", ilon = 1, ilat = 2)
                rhmean.data <- init.default.list.args(rhmean.data, rhmean_pars)
            }
        }else{
            stop("No folder containing the netCDF mean relative humidity data provided")
            # return(NULL)
        }
    }else{
        stop('Unknown humidity data source')
        # return(NULL)
    }
    if(!is.null(wff10.data$dir)){
        if(!dir.exists(wff10.data$dir)){
            msg <- paste("Folder containing the netCDF wind speed at 10m data does not exist", ":", wff10.data$dir)
            stop(msg)
            # return(NULL)
        }else{
            wff10_pars <- list(dir = "", format = "wff_%s%s%s.nc", varid = "wsp", ilon = 1, ilat = 2)
            wff10.data <- init.default.list.args(wff10.data, wff10_pars)
        }
    }else{
        stop("No folder containing the netCDF wind speed at 10m data provided")
        # return(NULL)
    }
    if(!is.null(pres.data$dir)){
        if(!dir.exists(pres.data$dir)){
            msg <- paste("Folder containing the netCDF surface pressure data does not exist", ":", pres.data$dir)
            stop(msg)
            # return(NULL)
        }else{
            pres_pars <- list(dir = "", format = "pres_%s%s%s.nc", varid = "pres", ilon = 1, ilat = 2)
            pres.data <- init.default.list.args(pres.data, pres_pars)
        }
    }else{
        stop("No folder containing the netCDF surface pressure data provided")
        # return(NULL)
    }
    if(!is.null(rad.data$dir)){
        if(!dir.exists(rad.data$dir)){
            msg <- paste("Folder containing the netCDF solar radiation flux data does not exist", ":", rad.data$dir)
            stop(msg)
            # return(NULL)
        }else{
            rad_pars <- list(dir = "", format = "rad_%s%s%s.nc", varid = "rad", ilon = 1, ilat = 2)
            rad.data <- init.default.list.args(rad.data, rad_pars)
        }
    }else{
        stop("No folder containing the netCDF solar radiation flux data provided")
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

    output_pars <- list(dir = "", format = "et0_%S.nc")
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

    tmax_ncInfo <- get.ncInfo.params(tmax.data, date.range, 'daily')
    if(is.null(tmax_ncInfo)){
        stop("No netCDF maximum temperature files found")
        # return(NULL)
    }
    tmin_ncInfo <- get.ncInfo.params(tmin.data, date.range, 'daily')
    if(is.null(tmin_ncInfo)){
        stop("No netCDF minimum temperature files found")
        # return(NULL)
    }
    if(rh.from == 'minmax'){
        rhmax_ncInfo <- get.ncInfo.params(rhmax.data, date.range, 'daily')
        if(is.null(rhmax_ncInfo)){
            stop("No netCDF maximum relative humidity files found")
            # return(NULL)
        }
        rhmin_ncInfo <- get.ncInfo.params(rhmin.data, date.range, 'daily')
        if(is.null(rhmin_ncInfo)){
            stop("No netCDF minimum relative humidity files found")
            # return(NULL)
        }
    }else{
        rhmean_ncInfo <- get.ncInfo.params(rhmean.data, date.range, 'daily')
        if(is.null(rhmean_ncInfo)){
            stop("No netCDF mean relative humidity files found")
            # return(NULL)
        }
    }
    wff10_ncInfo <- get.ncInfo.params(wff10.data, date.range, 'daily')
    if(is.null(wff10_ncInfo)){
        stop("No netCDF wind speed files found")
        # return(NULL)
    }
    pres_ncInfo <- get.ncInfo.params(pres.data, date.range, 'daily')
    if(is.null(pres_ncInfo)){
        stop("No netCDF surface pressure files found")
        # return(NULL)
    }
    rad_ncInfo <- get.ncInfo.params(rad.data, date.range, 'daily')
    if(is.null(rad_ncInfo)){
        stop("No netCDF radiation files found")
        # return(NULL)
    }

    ##########

    tmax_grd <- defSpatialPixels(tmax_ncInfo$ncinfo[c('lon', 'lat')])
    tmin_grd <- defSpatialPixels(tmin_ncInfo$ncinfo[c('lon', 'lat')])
    if(rh.from == 'minmax'){
        rhmax_grd <- defSpatialPixels(rhmax_ncInfo$ncinfo[c('lon', 'lat')])
        rhmin_grd <- defSpatialPixels(rhmin_ncInfo$ncinfo[c('lon', 'lat')])
    }else{
        rhmean_grd <- defSpatialPixels(rhmean_ncInfo$ncinfo[c('lon', 'lat')])
    }
    wff10_grd <- defSpatialPixels(wff10_ncInfo$ncinfo[c('lon', 'lat')])
    pres_grd <- defSpatialPixels(pres_ncInfo$ncinfo[c('lon', 'lat')])
    rad_grd <- defSpatialPixels(rad_ncInfo$ncinfo[c('lon', 'lat')])

    ## check grid
    ## match dates overlap

    ##########

    is.regridDEM <- is.diffSpatialPixelsObj(tmax_grd, dem_grd, tol = 1e-07)
    if(is.regridDEM){
        demData <- cdt.interp.surface.grid(demData, pres_ncInfo$ncinfo[c('lon', 'lat')])
        names(demData) <- c('lon', 'lat', 'z')
    }else demData <- demData[c('lon', 'lat', 'z')]
    demData$z[demData$z < 0] <- 0

    ##########

    dx <- ncdf4::ncdim_def("Lon", "degreeE", tmax_ncInfo$ncinfo$lon)
    dy <- ncdf4::ncdim_def("Lat", "degreeN", tmax_ncInfo$ncinfo$lat)
    xydim <- list(dx, dy)

    varname <- 'et0'
    longname <- "Reference evapotranspiration using FAO Penman-Monteith equation"
    ncmissval <- -99

    grdncout <- ncdf4::ncvar_def(varname, 'mm/day', xydim, ncmissval, prec = 'float',
                              longname = longname, compression = 9)
    ##########

    cdt.file.conf <- file.path(.cdtDir$dirLocal, "config", "cdt_config.json")
    Config <- jsonlite::fromJSON(cdt.file.conf)
    Config <- rapply(Config, trimws, classes = "character", how = "replace")
    .cdtData$Config$parallel <- Config$parallel

    parsL <- doparallel.cond(length(tmax_ncInfo$dates) >= 180)

    ret <- cdt.foreach(seq_along(tmax_ncInfo$dates), parsL, GUI = FALSE,
                       progress = FALSE, FUN = function(jj)
    {
        nc <- ncdf4::nc_open(tmax_ncInfo$ncfiles[jj])
        tmax <- ncdf4::ncvar_get(nc, varid = tmax_ncInfo$ncinfo$varid)
        ncdf4::nc_close(nc)
        tmax <- transposeNCDFData(tmax, tmax_ncInfo$ncinfo)

        nc <- ncdf4::nc_open(tmin_ncInfo$ncfiles[jj])
        tmin <- ncdf4::ncvar_get(nc, varid = tmin_ncInfo$ncinfo$varid)
        ncdf4::nc_close(nc)
        tmin <- transposeNCDFData(tmin, tmin_ncInfo$ncinfo)

        if(rh.from == 'minmax'){
            nc <- ncdf4::nc_open(rhmax_ncInfo$ncfiles[jj])
            rhmax <- ncdf4::ncvar_get(nc, varid = rhmax_ncInfo$ncinfo$varid)
            ncdf4::nc_close(nc)
            rhmax <- transposeNCDFData(rhmax, rhmax_ncInfo$ncinfo)

            nc <- ncdf4::nc_open(rhmin_ncInfo$ncfiles[jj])
            rhmin <- ncdf4::ncvar_get(nc, varid = rhmin_ncInfo$ncinfo$varid)
            ncdf4::nc_close(nc)
            rhmin <- transposeNCDFData(rhmin, rhmin_ncInfo$ncinfo)
        }else{
            nc <- ncdf4::nc_open(rhmean_ncInfo$ncfiles[jj])
            rhmean <- ncdf4::ncvar_get(nc, varid = rhmean_ncInfo$ncinfo$varid)
            ncdf4::nc_close(nc)
            rhmean <- transposeNCDFData(rhmean, rhmean_ncInfo$ncinfo)
        }

        nc <- ncdf4::nc_open(wff10_ncInfo$ncfiles[jj])
        wff10 <- ncdf4::ncvar_get(nc, varid = wff10_ncInfo$ncinfo$varid)
        ncdf4::nc_close(nc)
        wff10 <- transposeNCDFData(wff10, wff10_ncInfo$ncinfo)

        nc <- ncdf4::nc_open(pres_ncInfo$ncfiles[jj])
        pres <- ncdf4::ncvar_get(nc, varid = pres_ncInfo$ncinfo$varid)
        ncdf4::nc_close(nc)
        pres <- transposeNCDFData(pres, pres_ncInfo$ncinfo)

        nc <- ncdf4::nc_open(rad_ncInfo$ncfiles[jj])
        rad <- ncdf4::ncvar_get(nc, varid = rad_ncInfo$ncinfo$varid)
        ncdf4::nc_close(nc)
        rad <- transposeNCDFData(rad, rad_ncInfo$ncinfo)

        ########
        # daily extraterrestrial radiation W/m^2
        Ra <- extraterrestrial_radiation_daily(tmax_ncInfo$ncinfo$lat, tmax_ncInfo$dates[jj])
        # convert to MJ m^-2 day^-1
        Ra <- 0.0864 * Ra
        Ra <- matrix(Ra, nrow(tmax), ncol(tmax), byrow = TRUE)

        ########
        if(rh.from == 'minmax'){
            rhmean <- NA
        }else{
            rhmax <- NA
            rhmin <- NA
        }

        et0 <- ET0_FAO(tmax, tmin, wff10,
                       pres, rad, Ra, demData$z,
                       rh.from, rhmax, rhmin, rhmean)
        
        et0[is.na(et0) | is.nan(et0) | is.infinite(et0)] <- ncmissval

        outfl <- file.path(output$dir, sprintf(output$format, tmax_ncInfo$dates[jj]))
        nc <- ncdf4::nc_create(outfl, grdncout)
        ncdf4::ncvar_put(nc, grdncout, et0)
        ncdf4::nc_close(nc)
    })

    return(0)
}

SVP_FAO <- function(tmp){
    0.6108 * exp(17.27 * tmp/(tmp + 237.3))
}

ET0_FAO <- function(tmax, tmin, wff10, pres, rad, Ra, elev,
                    rhfrom = 'minmax', rhmax = NA, rhmin = NA, rhmean = NA)
{
    ## wind speed at 2m
    u2 <- wff10 * (4.87/log(67.8 * 10 - 5.42))
    ## temperature mean
    tmean <- (tmax + tmin)/2
    ## Slope of saturation vapour pressure curve
    D <- 4098 * SVP_FAO(tmean) / (tmean + 237.3)^2
    ## Psychrometric constant
    g <- 0.665 * 0.001 * (pres * 0.1)

    ###
    denom <- D + g * (1 + 0.34 * u2)
    numer0 <- 900 / (tmean + 273)

    ## Vapour pressure deficit
    es <- (SVP_FAO(tmax) + SVP_FAO(tmin))/2

    if(rhfrom == 'minmax'){
        ## from rhmin and rhmax
        ea <- (SVP_FAO(tmin) * rhmax * 0.01 + SVP_FAO(tmax) * rhmin * 0.01)/2
    }else if(rhfrom == 'mean'){
        ## from rhmean
        ea <- rhmean * 0.01 * (SVP_FAO(tmin) + SVP_FAO(tmax))/2
    }else{
        return(NULL)
    }
    vap_deficit = es - ea

    # Radiation
    # convert to MJ m^-2 day^-1
    Rs <- 0.0864 * rad

    # Clear-sky solar radiation
    Rs0 <- Ra * (0.75 + 2 * 1e-5 * elev)

    # Net solar or net shortwave radiation
    albedo <- 0.23
    Rns <- (1 - albedo) * Rs

    # Net longwave radiation
    var1 <- 4.903 * 1e-9 * ((tmax + 273.16)^4 + (tmin + 273.16)^4)/2
    var2 <- 0.34 - 0.14 * sqrt(ea)
    var3 <- 1.35 * (Rs/Rs0) - 0.35
    Rnl <- var1 * var2 * var3

    # Net radiation
    Rn <- Rns - Rnl

    # soil heat flux density in MJ m-2 day-1
    G <- 0
    numer1 <- 0.408 * (Rn - G)

    et0 <- (numer1 * D + g * numer0 * u2 * vap_deficit)/denom

    return(et0)
}
