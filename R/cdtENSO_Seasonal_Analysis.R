
#' ENSO seasonal climate variable analysis.
#'
#' Probability of seasonal climate variable tercile conditioned on ENSO.
#' 
#' @param seasonal.data seasonal data, output from \code{cdtComputeSeasonal_netcdf}.
#' @param seasonal.class character vector of length 3, indicating the tercile class of the season. Examples, \cr
#' Temperature: c("Cold", "Normal", "Hot")
#' Precipitation: c("Dry", "Normal", "Wet")
#' @param enso.index Oceanic Nino Index (ONI) data, output from \code{oni.cpc.ncep.noaa} or \code{oni.iridl.ldeo}.
#' @param min.frac used to compute the probability of each ENSO phase, minimum fraction of non-missing values for each ENSO phase.
#' 
#' @return A named list
#' 
#' @examples
#' 
#' \dontrun{
#' library(CDT)
#' 
#' ## Compute seasonal temperature average
#' seas_data <- cdtComputeSeasonal_netcdf(
#'     season.def = list(start.year = 1981, end.year = 2020,
#'                       start.month = 12, season.length = 3),
#'     netcdf.data = list(time.step = "dekadal",
#'                        dir = "~/DATA/TMAX/MERGED_tmax_dekad",
#'                        format = "tmax_mrg_%s%s%s_ALL.nc",
#'                        varid = "temp", ilon = 1, ilat = 2),
#'     season.min.frac = 0.95,
#'     aggregation.fun = "mean"
#' )
#' 
#' ## ONI from CDT internal data
#' enso_index <- oni.cpc.ncep.noaa(12, 3)
#' 
#' ## compute and download ONI from IRI Data Library
#' # enso_index <- oni.iridl.ldeo(1981, 3)
#' 
#' seas_class <- c("Cold", "Normal", "Hot")
#' enso_data <- cdtENSOAnalysis_netcdf(seas_data, seas_class, enso_index, 0.8)
#' 
#' library(fields)
#' image.plot(enso_data$lon, enso_data$lat, enso_data$enso$LaNina$Hot)
#' image.plot(enso_data$lon, enso_data$lat, enso_data$enso$Neutral$Cold)
#' image.plot(enso_data$lon, enso_data$lat, enso_data$enso$ElNino$Normal)
#' }
#' 
#' @export

cdtENSOAnalysis_netcdf <- function(seasonal.data, seasonal.class,
                                   enso.index, min.frac = 0.5)
{
    seas_out <- lapply(seasonal.data$data, c)
    seas_out <- do.call(rbind, seas_out)
    terc <- matrixStats::colQuantiles(seas_out, probs = c(0.33333, 0.66667),
                                      type = 8, na.rm = TRUE)
    seas_class <- Map(
        function(x, q){
            if(is.na(q[1]))
                return(rep(NA, length(x)))
            if(q[2] - q[1] > 0)
                q[1] <- q[1] + 1e-15
            findInterval(x, q)
        },
        split(seas_out, col(seas_out)),
        split(terc, row(terc))
    )
    seas_class <- do.call(cbind, seas_class)

    oni_class <- findInterval(enso.index$oni, c(-0.5 + 1e-15, 0.5))
    ioni <- match(seasonal.data$seas, enso.index$date)
    ioni <- ioni[!is.na(ioni)]
    oni_date <- enso.index$date[ioni]
    oni_class <- oni_class[ioni]
    seas_class <- seas_class[seasonal.data$seas %in% oni_date, , drop = FALSE]

    nlon <- length(seasonal.data$lon)
    nlat <- length(seasonal.data$lat)

    LaNina <- season.class(seas_class, oni_class, 0, c(nlon, nlat), seasonal.class, min.frac)
    Neutral <- season.class(seas_class, oni_class, 1, c(nlon, nlat), seasonal.class, min.frac)
    ElNino <- season.class(seas_class, oni_class, 2, c(nlon, nlat), seasonal.class, min.frac)

    list(lon = seasonal.data$lon,
         lat = seasonal.data$lat,
         enso = list(
                LaNina = LaNina,
                Neutral = Neutral,
                ElNino = ElNino
            )
        )
}

season.class <- function(seas_class, oni_class, enso_phase,
                         proba_dim, cond_names, min_frac = 0.5)
{
    ## seas_class: matrix with dim = length(season) x (nlon * nlat) 
    ## Temperature: 0:Cold, 1:Normal, 2:Hot
    ## Precipitation: 0:Dry, 1:Normal, 2:Wet
    ## oni_class: vector with length = length(season) -->  0:LaNina, 1:Neutral, 2:ElNino
    ## enso_phase: integer -->  0:LaNina, 1:Neutral, 2:ElNino
    ## proba_dim: vector c(nlon, nlat)
    ## Temperature: cond_names = c("Cold", "Normal", "Hot")
    ## Precipitation: cond_names = c("Dry", "Normal", "Wet")
    ## min_frac: minimum fraction of non-missing

    dat <- seas_class[oni_class == enso_phase, , drop = FALSE]
    nbCol <- colSums(!is.na(dat))
    ina <- nbCol / nrow(dat) < min_frac
    dat <- matrixStats::colTabulates(dat, values = 0:2)
    dat <- sweep(dat, 1, nbCol, "/")
    dat[ina, ] <- NA
    dat <- split(dat, col(dat))
    dat <- lapply(dat, array, dim = proba_dim)
    names(dat) <- cond_names

    return(dat)
}
