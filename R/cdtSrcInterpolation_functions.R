#' CDT interpolation methods
#' 
#' This function interpolates stations data on a rectangular grid in the x-y plane.
#' The radius of influence for the interpolation method varies for each grid point. 
#' 
#' @param locations A matrix containing the coordinates of the station data. The 1st column is the longitude and the 2nd coulmn the latitude. 
#' @param values A vector containing the values of the station. The length of \code{values} must be equal to the number of row of \code{locations}.
#' @param newdata A matrix containing the coordinates of the grid to interpolate. The 1st column is the longitude and the 2nd coulmn the latitude.
#' @param nmin The minimum number of stations to be used to interpolate a grid point.
#' @param nmax The maximum number of stations to be used to interpolate a grid point.
#' @param method Method to calculate weights for the interpolation. Should be \strong{"idw"} (default), \strong{"shepard"}, \strong{"spheremap"} or \strong{"kriging"}.
#' \itemize{
#'  \item \strong{"idw"}: Inverse distance weighted
#'  \item \strong{"shepard"}: Modified Shepard interpolation
#'  \item \strong{"spheremap"}: Spheremap interpolation method
#'  \item \strong{"kriging"}: Ordinary kriging
#' }
#' @param spheric If \code{FALSE} (default), then a Cartesian distance will be computed. If set to \code{TRUE}, a spherical distance will be computed.
#' @param vgm A \code{gstat} variogram model. Example: \code{vgm(10.2, "Sph", 2.7, 1.1)}.
#' @param p The power to use in weight calculation.
#' 
#' @return A matrix containing the interpolated data. The 1st and 2nd columns are the same as \code{newdata} and the 3rd column contains the interpolated values.
#' 
#' @export

cdtInterp <- function(locations, values, newdata, nmin = 4, nmax = 10,
                    method = "idw", spheric = FALSE, vgm = NULL, p = 2.0)
{
    if(nrow(locations) != length(values)) stop("The length of 'values' must be equal to the number of row of 'locations'")
    if(method == "kriging" && is.null(vgm)) stop("No variogram model found.")
    locations <- as.matrix(locations[, 1:2, drop = FALSE])
    newdata <- as.matrix(newdata[, 1:2, drop = FALSE])

    interp.res <- switch(method,
        "idw" = idw.interp(locations, values, newdata, nmin, nmax, spheric, p),
        "shepard" = shepard.interp(locations, values, newdata, nmin, nmax, spheric, p),
        "spheremap" = spheremap.interp(locations, values, newdata, nmin, nmax, spheric),
        "kriging" = kriging.interp(locations, values, newdata, vgm, nmin, nmax, spheric),
        cat("Unknown interpolation method\n")
    )

    return(interp.res)
}

idw.interp <- function(locations, values, newdata, nmin, nmax, spheric, p = 2){
    # locations: matrix X Y
    # values: vector
    # newdata: matrix X Y
    # nim: integer
    # nmax: integer
    # spheric: logical
    # p: real
    nGrd <- nrow(newdata)
    outM <- matrix(double(1), nrow = nGrd, ncol = 3)
    nStn <- length(values)
    out <- .Fortran("idw_interp", locations, as.double(values),
                    as.integer(nStn), newdata, as.integer(nGrd),
                    as.integer(nmin), as.integer(nmax), as.integer(spheric),
                    as.double(p), outM = outM)
    out$outM
}

shepard.interp <- function(locations, values, newdata, nmin, nmax, spheric, p = 0.7){
    nGrd <- nrow(newdata)
    outM <- matrix(double(1), nrow = nGrd, ncol = 3)
    nStn <- length(values)
    out <- .Fortran("shepard_interp", locations, as.double(values),
                    as.integer(nStn), newdata, as.integer(nGrd),
                    as.integer(nmin), as.integer(nmax), as.integer(spheric),
                    as.double(p), outM = outM)
    out$outM
}

barnes.interp <- function(locations, values, newdata, nmin, nmax, spheric, p = 0.5){
    nGrd <- nrow(newdata)
    outM <- matrix(double(1), nrow = nGrd, ncol = 3)
    nStn <- length(values)
    out <- .Fortran("barnes_interp", locations, as.double(values),
                    as.integer(nStn), newdata, as.integer(nGrd),
                    as.integer(nmin), as.integer(nmax), as.integer(spheric),
                    as.double(p), outM = outM)
    out$outM
}

cressman.interp <- function(locations, values, newdata, nmin, nmax, spheric){
    nGrd <- nrow(newdata)
    outM <- matrix(double(1), nrow = nGrd, ncol = 3)
    nStn <- length(values)
    out <- .Fortran("cressman_interp", locations, as.double(values),
                    as.integer(nStn), newdata, as.integer(nGrd),
                    as.integer(nmin), as.integer(nmax), as.integer(spheric),
                    outM = outM)
    out$outM
}

kriging.interp <- function(locations, values, newdata, vgm, nmin, nmax, spheric){
    # locations: matrix X Y
    # values: vector
    # newdata: matrix X Y
    # vgm: gstat variogram model object
    # nim: integer
    # nmax: integer
    # spheric: logical

    nGrd <- nrow(newdata)
    outM <- matrix(double(1), nrow = nGrd, ncol = 4)
    nStn <- length(values)

    model <- switch(as.character(vgm$model[2]), 
                    "Gau" = 1,
                    "Exp" = 2,
                    "Sph" = 3, 
                    "Pen" = 4)
    model <- as.integer(model)
    nug <- as.double(vgm$psill[1])
    sill <- as.double(vgm$psill[2])
    rg <- as.double(vgm$range[2])

    out <- .Fortran("kriging_interp", locations, as.double(values),
                    as.integer(nStn), newdata, as.integer(nGrd),
                    as.integer(nmin), as.integer(nmax), as.integer(spheric),
                    model, nug, sill, rg, outM = outM)

    out <- out$outM
    out[is.nan(out)] <- NA
    out
}

spheremap.interp <- function(locations, values, newdata, nmin, nmax, spheric){
    nGrd <- nrow(newdata)
    outM <- matrix(double(1), nrow = nGrd, ncol = 3)
    nStn <- length(values)
    out <- .Fortran("spheremap_interp", locations, as.double(values),
                    as.integer(nStn), newdata, as.integer(nGrd),
                    as.integer(nmin), as.integer(nmax), as.integer(spheric),
                    outM = outM)
    out$outM
}
