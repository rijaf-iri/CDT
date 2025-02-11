
#' Options Controlling the merging parameters.
#'
#' Functions to handle settings used by the merging.
#' 
#' @param ... using one or more arguments of the form name = value.
#'   Existing values can be retrieved by supplying the names (as character strings) of the components as unnamed arguments.
#'  
#' @details
#' Available options are
#' \itemize{ 
#'   \item \code{mrgMinNumberSTN}: integer, minimum number of stations to perform the merging.
#'          If the number of stations is less than \code{mrgMinNumberSTN}, then no merging will be performed and the input netCDF will be taken
#'   \item \code{rkMinNumberSTN}: integer, minimum number of stations to perform a regression kriging.
#'          If the number of stations is less than \code{rkMinNumberSTN}, then the merging method will be replaced by a simple bias adjustment
#'   \item \code{vgmMinNumberSTN}: integer, minimum number of station to compute the empirical variogram.
#'          If the number of stations is less than \code{vgmMinNumberSTN}, then the variogram will not be computed and the interpolation method will be replaced by IDW
#'   \item \code{useLocalInterpolation}: logical, use local interpolation, if \code{FALSE} a global interpolation will be use and 
#'         the arguments \code{nmin}, \code{nmax} and \code{maxdist} from \code{interp.method} will not be considered
#'   \item \code{powerWeightIDW}: numeric, inverse distance weighting power
#'   \item \code{powerWeightShepard}: numeric, modified Shepard weighting power
#'   \item \code{powerWeightBarnes}: numeric, Barnes interpolation weighting power
#'   \item \code{addCoarseGrid}: logical, add a coarse grid to interpolate the residuals.
#'          The coarse grid will be created by resampling the grid of the input data.
#'          As CDT uses an additive model, the residual values at the coarse grid will be set to zero
#'   \item \code{saveGridBuffer}: logical, save the buffer of coarse grid used to interpolate the residuals. Default is \code{FALSE}
#'   \item \code{saveRnoR}: logical, save the rain-no-rain mask. Default is \code{FALSE}
#'   \item \code{RnoRModel}: character, model to use to compute the rain-no-rain mask. Options are: \code{"logit"}, \code{"additive"}. Default is \code{"logit"}
#'   \item \code{RnoRCutOff}: integer, the method to be used to define the decision boundaries of the rain-no-rain mask. Options are: \code{1, 2, 3}
#'        \itemize{
#'           \item \strong{option}: \code{1}
#'  %%
#'  \deqn{mask = \left\{
#'    \begin{array}{l l}
#'    0 & \quad \mbox{if } rnr < 0.5 \\
#'    1 & \quad \mbox{if } rnr \geq 0.5
#'    \end{array} \right.
#'  }{mask = if(rnr < 0) 0 else 1}
#'  %%
#'           \item \strong{option}: \code{2}
#'  %%
#'  \deqn{mask = \left\{
#'    \begin{array}{l l}
#'    0 & \quad \mbox{if } rnr < 0.1 \\
#'    rnr & \quad \mbox{if } rnr \geq 0.1
#'    \end{array} \right.
#'  }{mask = if(rnr < 0.1) 0 else rnr}
#'  %%
#'           \item \strong{option}: \code{3}
#'  %%
#'  \deqn{mask = \left\{
#'    \begin{array}{l l}
#'    0 & \quad \mbox{if } rnr < 0.25 \\
#'    rnr & \quad \mbox{if } 0.25 \leq rnr < 0.75 \\
#'    1 & \quad \mbox{if } rnr \geq 0.75
#'    \end{array} \right.
#'  }{mask = if(rnr < 0.25) 0 else if(rnr >= 0.25 & rnr < 0.75) rnr else 1}
#'  %%
#'          }
#'     where \eqn{rnr} is the interpolated rain-no-rain values. Default is \code{3}.
#'   \item \code{RnoRaddCoarse}: logical, use the coarse grid to create the rain-no-rain mask. Default is \code{FALSE}
#'   \item \code{RnoRUseMerged}: logical, if \code{TRUE} the merged data is used to compute the rain-no-rain mask, otherwise the input gridded data is used. Default is \code{FALSE}
#'   \item \code{RnoRSmoothingPixels}: integer, the number of pixels from the target pixel to be used to smooth the rain-no-rain mask. Default is 2.
#'   \item \code{blockType}: character, the method to use to create the block estimation when the argument \code{use.block} from \code{interp.method} is \code{TRUE}.
#'          Available options are \code{"gaussian"} and \code{"userdefined"}
#'          \itemize{
#'             \item \code{"gaussian"}: using Gaussian quadrature method
#'             \item \code{"userdefined"}: user defined block
#'        }
#'    \item \code{blockSize}: vector, the size of the block.
#'          \itemize{
#'             \item \code{"gaussian"}: vector of length 2 in the form \code{c(width_x, width_y)}
#'             \item \code{"userdefined"}: vector of length 4 in the form \code{c(width_x, by_x, width_y, by_y)}
#'        }
#'    \item \code{netCDFDataDef}: named list, netCDF variable definition
#' }
#' 
#' @export

merging.options <- function(...){
    ## copied from lattice.options
    new <- list(...)
    if(is.null(names(new)) && length(new) == 1 && is.list(new[[1]])) new <- new[[1]]
    old <- .cdtMRG$merging.options
    if(length(new) == 0) return(old)
    
    nm <- names(new)
    if (is.null(nm)) return(old[unlist(new)])

    isNamed <- nm != ""
    if (any(!isNamed)) nm[!isNamed] <- unlist(new[!isNamed])
    retVal <- old[nm]
    names(retVal) <- nm
    nm <- nm[isNamed]

    .cdtMRG$merging.options <- utils::modifyList(old, new[nm])

    invisible(retVal)
}

#' Setting options for netCDF variable.
#'
#' Functions to set the netCDF variables definition used by the merging.
#' 
#' @param variable character, name of the climate variable to merge. Available options: \code{"rain"}, \code{"temp"}, \code{"rh"}, \code{"pres"}, \code{"prmsl"}, \code{"rad"}, \code{"wspd"}, \code{"ugrd"}, \code{"vgrd"}.
#' \itemize{
#' \item \code{"rain"}: rainfall data
#' \item \code{"temp"}: temperature data
#' \item \code{"rh"}: relative humidity data
#' \item \code{"pres"}: surface pressure data
#' \item \code{"prmsl"}: pressure at mean sea level
#' \item \code{"rad"}: radiation data
#' \item \code{"wspd"}: wind speed data
#' \item \code{"ugrd"}: zonal wind components, E-W speed
#' \item \code{"vgrd"}: meridional wind components, N-S speed
#' }
#' @param values named list, giving the netCDF variable definition and data limits.
#' The elements of the list are
#' \itemize{
#' \item \code{name}: character, name of the variable to be created
#' \item \code{units}: character, the units of the variable
#' \item \code{longname}: character, longer name for the variable
#' \item \code{standardname}: character, standard name for the variable
#' \item \code{min}: numeric, possible minimum value of the dataset
#' \item \code{max}: numeric, possible maximum value of the dataset
#' }
#' 
#' @examples
#' 
#' \dontrun{
#' ## All elements of the list
#' merging.options.netcdf('rain', values = list(name = 'precip', units = 'mm/day',
#'                            longname = 'Daily precipitation', standardname = 'Precipitation',
#'                            min = 0, max = 120))
#' 
#' ## Partial elements
#' merging.options.netcdf('temp', values = list(name = 'tmax', longname = 'Daily maximum temperature'))
#' }
#' 
#' @export

merging.options.netcdf <- function(variable, values){
    mrgOpts <- merging.options()
    if(is.null(mrgOpts$netCDFDataDef[[variable]])){
        stop('Unknown variable!')
    }

    init_values <- mrgOpts$netCDFDataDef[[variable]]
    values <- init.default.list.args(values, init_values)
    mrgOpts$netCDFDataDef[[variable]] <- values
    merging.options(netCDFDataDef = mrgOpts$netCDFDataDef)
}

merging.getOption <- function(name)
{
    get("merging.options", envir = .cdtMRG)[[name]]
}

.defaultMrgOptions <- function(){
    list(
         mrgMinNumberSTN = 10,
         rkMinNumberSTN = 20,
         vgmMinNumberSTN = 20,
         useLocalInterpolation = TRUE,
         powerWeightIDW = 2,
         powerWeightShepard = 0.7,
         powerWeightBarnes = 0.5,
         addCoarseGrid = TRUE,
         saveGridBuffer = FALSE,
         saveRnoR = FALSE,
         RnoRModel = "logit", 
         RnoRCutOff = 3,
         RnoRaddCoarse = TRUE,
         RnoRUseMerged = FALSE,
         RnoRSmoothingPixels = 2,
         ## blockType "gaussian" or "userdefined"
         blockType = "gaussian",
         blockSize = c(1, 1),
         ## netCDF data definition
         netCDFDataDef = list(
            "rain" = list(name = "precip", units = "mm", min = 0, max = 5000,
                          standardname = "Rainfall",
                          longname = "Merged Station-Satellite Rainfall"),
            "temp" = list(name = "temp", units = "C", min = -40, max = 50,
                          standardname = "Temperature",
                          longname = "Downscaled Temperature Reanalysis merged with station"),
            "rh" = list(name = "rh", units = "%", min = 0, max = 100,
                        standardname = "Relative humidity",
                        longname = "Downscaled Relative Humidity Reanalysis merged with station"),
            "pres" = list(name = "pres", units = "hPa", min = 700, max = 1100,
                          standardname = "Surface pressure",
                          longname = "Downscaled Surface Pressure Reanalysis merged with station"),
            "prmsl" = list(name = "prmsl", units = "hPa", min = 850, max = 1100,
                          standardname = "Mean sea level pressure",
                          longname = "Downscaled MSL Pressure Reanalysis merged with station"),
            "rad" = list(name = "rad", units = "W/m2", min = 0, max = 1300,
                          standardname = "Radiation",
                          longname = "Downscaled Radiation Reanalysis merged with station"),
            "wspd" = list(name = "wspd", units = "m/s", min = 0, max = 120,
                          standardname = "Wind speed",
                          longname = "Gridded wind speed merged with station"),
            "ugrd" = list(name = "ugrd", units = "m/s", min = -120, max = 120,
                          standardname = "Zonal wind speed",
                          longname = "Gridded wind U-component merged with station"),
            "vgrd" = list(name = "vgrd", units = "m/s", min = -120, max = 120,
                          standardname = "Meridional wind speed",
                          longname = "Gridded wind V-component merged with station")
            )
        )
}
