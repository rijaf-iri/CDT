
#' Options Controlling the bias coefficients parameters.
#'
#' Functions to handle settings used by the bias coefficients computation.
#' 
#' @param ... using one or more arguments of the form name = value.
#'   Existing values can be retrieved by supplying the names (as character strings) of the components as unnamed arguments.
#' @details
#' Available options are
#' \itemize{ 
#'   \item{\code{mulBiasFunction}: }{character, the function to be used to compute the multiplicative bias coefficients. Options are "mean" (default), "median"}
#'   \item{\code{aggrBoxMethodStation}: }{character, the interpolation method to use to aggregate stations data within the grid box. Options are "idw" (default), "mean"}
#'   \item{\code{aggrBoxMethodGrid}: }{character, the interpolation method to use to aggregate gridded data within the grid box. Options are "bilinear" (default), "weightedAverage".
#'         The method \code{"weightedAverage"} is a spatially weighted averages of the pixels within the box, the weights are the area of the pixel falling inside the box}
#'   \item{\code{maxBoxNeighbor}: }{numeric, maximum number of nearest neighbor boxes to use when fitting and interpolating a grid point.}
#'   \item{\code{rainyEventThres}: }{numeric, the rain threshold to use when computing the occurrence of rainy event in case of \strong{Quantile Mapping with Fitted Distribution}}
#'   \item{\code{blockType}: }{character, the method to use to create the block estimation when the argument \code{use.block} from \code{interp.method} is \code{TRUE}.
#'          Available options are \code{"gaussian"} and \code{"userdefined"}
#'          \itemize{
#'             \item{\code{"gaussian"}: }{using Gaussian quadrature method}
#'             \item{\code{"userdefined"}: }{user defined block}
#'        }
#'    }
#'    \item{\code{blockSize}: }{vector, the size of the block.
#'          \itemize{
#'             \item{\code{"gaussian"}: }{vector of length 2 in the form \code{c(width_x, width_y)}}
#'             \item{\code{"userdefined"}: }{vector of length 4 in the form \code{c(width_x, by_x, width_y, by_y)}}
#'        }
#'    }
#'   \item{\code{addCoarseGrid}: }{logical, add a coarse grid to interpolate the bias coefficients or the parameters of the distribution for IDW and Ordinary Kriging method.
#'          The coarse grid will be created by resampling the grid of the input data.
#'          The values at the coarse grid will be set to 1 for multiplicative bias method and the values of the distribution parameters from gridded data for the fitted distribution method.}
#'   \item{\code{saveCoarseGrid}: }{logical, save the buffer of coarse grid used to interpolate the bias coefficients or the parameters of the distribution. Default is \code{FALSE}}
#' }
#' 
#' @export

biascoeff.options <- function(...){
    ## copied from lattice.options
    new <- list(...)
    if(is.null(names(new)) && length(new) == 1 && is.list(new[[1]])) new <- new[[1]]
    old <- .cdtMRG$biascoeff.options
    if(length(new) == 0) return(old)
    
    nm <- names(new)
    if (is.null(nm)) return(old[unlist(new)])

    isNamed <- nm != ""
    if (any(!isNamed)) nm[!isNamed] <- unlist(new[!isNamed])
    retVal <- old[nm]
    names(retVal) <- nm
    nm <- nm[isNamed]

    .cdtMRG$biascoeff.options <- utils::modifyList(old, new[nm])

    invisible(retVal)
}

biascoeff.getOption <- function(name)
{
    get("biascoeff.options", envir = .cdtMRG)[[name]]
}

.defaultBiasCoefOptions <- function(){
    list(
         ## mulBiasFunction "mean" or "median"
         mulBiasFunction = "mean",
         ## aggrBoxStation "idw" or "mean"
         aggrBoxMethodStation = "idw",
         # aggrBoxMethodGrid "bilinear" or "weightedAverage"
         aggrBoxMethodGrid = "bilinear",
         ## maxBoxNeighbor
         maxBoxNeighbor = 4,
         ## Rainy event threshold
         rainyEventThres = 1,
         ## blockType "gaussian" or "userdefined"
         blockType = "gaussian",
         blockSize = c(1, 1),
         ## add coarse grid
         addCoarseGrid = TRUE,
         saveCoarseGrid = FALSE
        )
}