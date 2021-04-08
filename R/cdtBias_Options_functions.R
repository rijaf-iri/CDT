
#' Options Controlling the bias coefficients parameters.
#'
#' Functions to handle settings used by the bias coefficients computation.
#' 
#' @param ... using one or more arguments of the form name = value.
#'   Existing values can be retrieved by supplying the names (as character strings) of the components as unnamed arguments.
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
         ## blockType "matrix" or "vector"
         blockType = "vector",
         blockFac = 10,
         blockLen = 10,
         blockSize = c(2, 2)
        )
}