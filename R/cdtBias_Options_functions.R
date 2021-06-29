
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
#'   \item{\code{qmecdfBoxInterp}: }{character, the interpolation method to use in case of \strong{Quantile Mapping with Empirical Distribution}. Options are "idw" (default), "means"}
#'   \item{\code{qmecdfBoxMaxdist}: }{numeric, maximum distance to use if \code{qmecdfBoxInterp} is \code{TRUE}}
#'   \item{\code{qmdistRainyDayThres}: }{numeric, rainy day threshold to use when computing the occurrence of rainy days in case of \strong{Quantile Mapping with Fitted Distribution}}
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
         ## qmecdfBoxInterp "idw" or "means"
         qmecdfBoxInterp = "idw",
         ## if qmecdfBoxInterp = "idw"
         qmecdfBoxMaxdist = 4,
         ## Rainy day threshold
         qmdistRainyDayThres = 1,
         qmdistTest = 1,
         ## blockType "gaussian" or "userdefined"
         blockType = "gaussian",
         blockSize = c(1, 1)
        )
}