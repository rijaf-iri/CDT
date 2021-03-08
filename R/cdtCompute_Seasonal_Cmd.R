#' Computing seasonal data for netCDF dataset.
#'
#' Compute seasonal data for netCDF dataset.
#'
#' @param season.def named list, season definition.
#' \itemize{
#'   \item{\code{start.month}: }{integer, start month of the season}
#'   \item{\code{season.length}: }{integer, length of the season}
#' }
#' @param period named list, the period of data to compute.
#' \itemize{
#'   \item{\code{start.year}: }{integer, start year of the period}
#'   \item{\code{end.year}: }{integer, the end year}
#' }
#' @param netcdf.data named list, netCDF data info.
#' \itemize{
#' \item{\code{time.step}: }{character, the time step of the netCDF data. Available options: \code{"daily"}, \code{"pentad"}, \code{"dekadal"}, \code{"monthly"}}
#' \item{\code{dir}: }{character, full path to the directory containing the netCDF files.}
#' \item{\code{format}: }{character, format of the netCDF file names}
#' \item{\code{varid}: }{character, name of the variable to read from the netCDF data}
#' \item{\code{ilon}: }{integer, order for the longitude dimension of the variable. 
#' Example: if the variable "precip" has the dimension order [Lat, Lon] then \code{ilon} must be 2}
#' \item{\code{ilat}: }{integer, order for the latitude dimension of the variable.}
#' }
#' @param season.min.frac numeric, used to aggregate the seasonal data, minimum fraction of non-missing values for each season.
#' @param aggregation.fun character, aggregation function to use. Options are: "sum", "mean", "median", "max", "min", "sd", "var", "count", "user".
#' @param count.opr character, the comparison operator to use when \code{aggregation.fun} is \code{"count"}.
#' @param count.thres the threshold to use when \code{aggregation.fun} is \code{"count"}.
#' @param ... when \code{aggregation.fun} is \code{"user"}, the user function and additional arguments to be passed to the user's function.
#' 
#' @return A named list
#' 
#' @export

cdtComputeSeasonal_netcdf <- function(
                                       season.def = list(start.month = 1, season.length = 3),
                                       period = list(start.year = 1981, end.year = 2010),
                                       netcdf.data = list(time.step = "dekadal", dir = "",
                                                          format = "tmax_mrg_%s%s%s.nc",
                                                          varid = "temp", ilon = 1, ilat = 2),
                                       season.min.frac = 0.95,
                                       aggregation.fun = "mean", count.opr = ">=", count.thres = 1,
                                       ...
                                    )
{
    GUI <- FALSE
    progress <- FALSE

    if(!is.null(netcdf.data$dir)){
        if(!dir.exists(netcdf.data$dir)){
            msg <- paste("Folder containing the netCDF data does not exist", ":", netcdf.data$dir)
            Insert.Messages.Out(msg, TRUE, "e", GUI)
            return(NULL)
        }else{
            ncdata_pars <- list(time.step = "dekadal", dir = "", format = "tmax_mrg_%s%s%s.nc",
                                varid = "temp", ilon = 1, ilat = 2)
            netcdf.data <- init.default.list.args(netcdf.data, ncdata_pars)
        }
    }else{
        Insert.Messages.Out("No folder containing the netCDF data provided", TRUE, "e", GUI)
        return(NULL)
    }

    start <- paste0(period$start.year, '-1-1')
    end <- paste0(period$end.year, '-12-31')
    step <- if(netcdf.data$time.step == "monthly") "month" else "day"
    dates <- seq(as.Date(start), as.Date(end), step)
    year <- format(dates, "%Y")
    mon <- format(dates, "%m")
    day <- format(dates, "%d")

    if(netcdf.data$time.step == "daily"){
        ncfiles <- sprintf(netcdf.data$format, year, mon, day)
        daty <- paste0(year, mon, day)
    }else if(netcdf.data$time.step == "pentad"){
        pen <- as.numeric(day)
        ipn <- pen <= 6
        ncfiles <- sprintf(netcdf.data$format, year[ipn], mon[ipn], pen[ipn])
        daty <- paste0(year[ipn], mon[ipn], pen[ipn])
    }else if(netcdf.data$time.step == "dekadal"){
        dek <- as.numeric(day)
        idk <- dek <= 3
        ncfiles <- sprintf(netcdf.data$format, year[idk], mon[idk], dek[idk])
        daty <- paste0(year[idk], mon[idk], dek[idk])
    }else if(netcdf.data$time.step == "monthly"){
        ncfiles <- sprintf(netcdf.data$format, year, mon)
        daty <- paste0(year, mon)
    }else{
        Insert.Messages.Out("Unknown netCDF time step", TRUE, "e", GUI)
        return(NULL)
    }

    ncpaths <- file.path(netcdf.data$dir, ncfiles)
    ifiles <- file.exists(ncpaths)
    if(!any(ifiles)){
        Insert.Messages.Out("No netCDF files found", TRUE, "e", GUI)
        return(NULL)
    }
    ncpaths <- ncpaths[ifiles]
    daty <- daty[ifiles]

    varid <- netcdf.data$varid
    nc <- ncdf4::nc_open(ncpaths[1])
    lon <- nc$var[[varid]]$dim[[netcdf.data$ilon]]$vals
    lat <- nc$var[[varid]]$dim[[netcdf.data$ilat]]$vals
    ncdf4::nc_close(nc)

    xo <- order(lon)
    lon <- lon[xo]
    yo <- order(lat)
    lat <- lat[yo]
    ncinfo <- list(xo = xo, yo = yo,
                   ilon = netcdf.data$ilon,
                   ilat = netcdf.data$ilat)
    nlon <- length(lon)
    nlat <- length(lat)

    index <- cdt.index.aggregate(daty, netcdf.data$time.step, "seasonal",
                                 seasonLength = season.def$season.length,
                                 startMonth = season.def$start.month)
    ########
    args_user <- list(...)
    fun_user <- args_user$fun
    args_user <- args_user[!names(args_user) %in% "fun"]

    ########
    cdtLocalConfigData()
    parsL <- doparallel.cond(length(index$index) >= 10)

    seas_data <- cdt.foreach(seq_along(index$index), parsL, GUI, progress, FUN = function(i)
    {
        if(index$nba[i]/index$nb0[i] < season.min.frac)
            return(matrix(NA, nlon, nlat))

        ncdon <- lapply(index$index[[i]], function(j){
            nc <- ncdf4::nc_open(ncpaths[j])
            don <- ncdf4::ncvar_get(nc, varid)
            ncdf4::nc_close(nc)
            don <- transposeNCDFData(don, ncinfo)
            c(don)
        })
        ncdon <- do.call(rbind, ncdon)
        ina <- colSums(!is.na(ncdon))/index$nb0[i] < season.min.frac

        if(aggregation.fun == "count"){
            fun <- get(count.opr, mode = "function")
            aggr <- fun(ncdon, count.thres) & !is.na(ncdon)
            aggr <- colSums(aggr, na.rm = TRUE)
        }else if(aggregation.fun == "user"){
            args_user$mat <- ncdon
            aggr <- do.call(fun_user, args_user)
        }else{
            fun <- switch(aggregation.fun,
                          "sum" = colSums,
                          "mean" = colMeans,
                          "median" = matrixStats::colMedians,
                          "max" = matrixStats::colMaxs,
                          "min" = matrixStats::colMins,
                          "sd" = matrixStats::colSds,
                          "var" = matrixStats::colVars
                         )
            aggr <- fun(ncdon, na.rm = TRUE)
        }

        aggr[is.nan(aggr) | is.infinite(aggr)] <- NA
        aggr[ina] <- NA
        dim(aggr) <- c(nlon, nlat)
        aggr
    })
    names(seas_data) <- index$date

    list(lon = lon, lat = lat, seas = index$date, data = seas_data)
}

#' Computing seasonal data for CDT stations data.
#'
#' Compute seasonal data for CDT stations data.
#'
#' @param season.def named list, season definition.
#' \itemize{
#'   \item{\code{start.month}: }{integer, start month of the season}
#'   \item{\code{season.length}: }{integer, length of the season}
#' }
#' @param period named list, the period of data to compute.
#' \itemize{
#'   \item{\code{start.year}: }{integer, start year of the period}
#'   \item{\code{end.year}: }{integer, the end year}
#' }
#' @param station.data named list, stations data info.
#' \itemize{
#'   \item{\code{time.step}: }{character, the time step of the station data. Available options: \code{"daily"}, \code{"pentad"}, \code{"dekadal"}, \code{"monthly"}}
#'   \item{\code{file}: }{character, full path to the file containing the CDT data}
#'   \item{\code{sep}: }{character, the column's separator of the data}
#'   \item{\code{na.strings}: }{character, the missing values flag}
#' }
#' @param season.min.frac numeric, used to aggregate the seasonal data, minimum fraction of non-missing values for each season.
#' @param aggregation.fun character, aggregation function to use. Options are: "sum", "mean", "median", "max", "min", "sd", "var", "count", "user".
#' @param count.opr character, the comparison operator to use when \code{aggregation.fun} is \code{"count"}.
#' @param count.thres numeric, the threshold to use when \code{aggregation.fun} is \code{"count"}.
#' @param ... when \code{aggregation.fun} is \code{"user"}, the user function and additional arguments to be passed to the user's function.
#' 
#' @return A named list
#' 
#' @export

cdtComputeSeasonal_cdtstn <- function(
                                       season.def = list(start.month = 1, season.length = 3),
                                       period = list(start.year = 1981, end.year = 2010),
                                       station.data = list(time.step = "dekadal", file = "",
                                                           sep = ",", na.strings = "-99"),
                                       season.min.frac = 0.95,
                                       aggregation.fun = "sum", count.opr = ">=", count.thres = 1,
                                       ...
                                    )
{
    GUI <- FALSE
    if(!is.null(station.data$file)){
        if(!file.exists(station.data$file)){
            msg <- paste("File containing the station data does not exist", ":", station.data$file)
            Insert.Messages.Out(msg, TRUE, "e", GUI)
            return(NULL)
        }else{
            stndata_pars <- list(time.step = "dekadal",file = "", sep = ",", na.strings = "-99")
            station.data <- init.default.list.args(station.data, stndata_pars)
        }
    }else{
        Insert.Messages.Out("No station data file provided", TRUE, "e", GUI)
        return(NULL)
    }

    cdtfile <- station.data[c('file', 'sep', 'na.strings')]
    stn <- do.call(readCDTStationData, cdtfile)
    nbstn <- length(stn$id)
    years <- as.numeric(substr(stn$dates, 1, 4))
    iyr <- years >= period$start.year & years <= period$end.year
    stn$dates <- stn$dates[iyr]
    stn$data <- stn$data[iyr, , drop = FALSE]

    index <- cdt.index.aggregate(stn$dates, station.data$time.step, "seasonal",
                                 seasonLength = season.def$season.length,
                                 startMonth = season.def$start.month)
    ########
    args_user <- list(...)
    fun_user <- args_user$fun
    args_user <- args_user[!names(args_user) %in% "fun"]

    ########
    cdtLocalConfigData()
    GUI <- FALSE
    progress <- FALSE
    parsL <- doparallel.cond(length(index$index) > 50)

    seas_data <- cdt.foreach(seq_along(index$index), parsL, GUI, progress, FUN = function(i)
    {
        if(index$nba[i]/index$nb0[i] < season.min.frac)
            return(rep(NA, nbstn))

        ncdon <- stn$data[index$index[[i]], , drop = FALSE]
        ina <- colSums(!is.na(ncdon))/index$nb0[i] < season.min.frac

        if(aggregation.fun == "count"){
            fun <- get(count.opr, mode = "function")
            aggr <- fun(ncdon, count.thres) & !is.na(ncdon)
            aggr <- colSums(aggr, na.rm = TRUE)
        }else if(aggregation.fun == "user"){
            args_user$mat <- ncdon
            aggr <- do.call(fun_user, args_user)
        }else{
            fun <- switch(aggregation.fun,
                          "sum" = colSums,
                          "mean" = colMeans,
                          "median" = matrixStats::colMedians,
                          "max" = matrixStats::colMaxs,
                          "min" = matrixStats::colMins,
                          "sd" = matrixStats::colSds,
                          "var" = matrixStats::colVars
                         )
            aggr <- fun(ncdon, na.rm = TRUE)
        }

        aggr[is.nan(aggr) | is.infinite(aggr)] <- NA
        aggr[ina] <- NA
        aggr
    })
    seas_data <- do.call(rbind, seas_data)

    c(stn[c('id', 'lon', 'lat', 'elv')],
      list(seas = index$date, data = seas_data)
     )
}

