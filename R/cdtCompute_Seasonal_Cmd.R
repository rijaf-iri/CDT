#' Computing seasonal data for netCDF dataset.
#'
#' Compute seasonal data for netCDF dataset.
#'
#' @param season.def a named list, season definition.
#' @param period a named list, the period of data to compute.
#' @param netcdf.data a named list, netCDF data info.
#' @param season.min.frac used to aggregate the seasonal data, minimum fraction of non-missing values for each season.
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
        stop("Unknown netCDF time step")
    }

    ncpaths <- file.path(netcdf.data$dir, ncfiles)
    ifiles <- file.exists(ncpaths)
    if(!any(ifiles)){
        stop("No netCDF files found")
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
    GUI <- FALSE
    progress <- FALSE
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
#' @param season.def a named list, season definition.
#' @param period a named list, the period of data to compute.
#' @param station.data a named list, stations data info.
#' @param season.min.frac used to aggregate the seasonal data, minimum fraction of non-missing values for each season.
#' @param aggregation.fun character, aggregation function to use. Options are: "sum", "mean", "median", "max", "min", "sd", "var", "count", "user".
#' @param count.opr character, the comparison operator to use when \code{aggregation.fun} is \code{"count"}.
#' @param count.thres the threshold to use when \code{aggregation.fun} is \code{"count"}.
#' @param ... when \code{aggregation.fun} is \code{"user"}, the user function and additional arguments to be passed to the user's function.
#' 
#' @return A named list
#' 
#' @export

cdtComputeSeasonal_cdtstn <- function(
                                       season.def = list(start.month = 1, season.length = 3),
                                       period = list(start.year = 1981, end.year = 2010),
                                       station.data = list(time.step = "dekadal", file = "",
                                                           sep = ",", missing = "-99"),
                                       season.min.frac = 0.95,
                                       aggregation.fun = "sum", count.opr = ">=", count.thres = 1,
                                       ...
                                    )
{
    stn <- readCDTStationData(station.data$file, station.data$sep, station.data$missing)
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

