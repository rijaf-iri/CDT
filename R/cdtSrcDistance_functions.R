#' Distance matrix
#' 
#' Distance matrix between two sets of points
#'
#' @param x a 2 column matrix or data.frame (first column is longitude, second is latitude); or a SpatialPoints*, SpatialPixels* or SpatialGrid* object.
#' @param y Same as \code{x}. If \code{x} is a Spatial object, \code{y} can take a different Spatial object other than \code{x}.
#' @param spheric If \code{FALSE} (default), then a Cartesian distance will be computed. If set to \code{TRUE}, a spherical distance (using a standard great circle method) will be computed.
#' @return Matrix of distances. The row represent \code{y} and the column \code{x}.
#' 
#' @export

distance.Matrix <- function(x, y, spheric = FALSE){
    if(inherits(x, "matrix") && inherits(y, "matrix")){
        x <- x[, 1:2, drop = FALSE]
        y <- y[, 1:2, drop = FALSE]
    }else if(inherits(x, "data.frame") && inherits(y, "data.frame")){
        x <- as.matrix(x[, 1:2, drop = FALSE])
        y <- as.matrix(y[, 1:2, drop = FALSE])
    }else{
        class.x <- substr(class(x), 1, 7)
        class.y <- substr(class(y), 1, 7)
        if(class.x == "Spatial" && class.y == "Spatial"){
            if(!identical(proj4string(x), proj4string(y)))
                stop("x and y have different coordinate reference systems")
            x <- as.matrix(coordinates(x))
            y <- as.matrix(coordinates(y))
        }else{
            stop("x and y must be a matrix, data.frame or sp Spatial object")
        }
    }

    distance.matrix(x, y, spheric)
}

distance.vector <- function(x, y, spheric){
    # x: vector c(x, y)
    # y: matrix X Y
    x <- as.numeric(x)
    y <- as.matrix(y)
    storage.mode(x) <- "double"
    storage.mode(y) <- "double"
    nr <- nrow(y)
    out <- vector("numeric", length = nr)
    out <- .Fortran("distance_vector", x, y, as.integer(nr), as.integer(spheric), dst = out)
    out$dst
}

distance.matrix <- function(x, y, spheric){
    # x: matrix X Y
    # y: matrix X Y
    x <- as.matrix(x)
    y <- as.matrix(y)
    storage.mode(x) <- "double"
    storage.mode(y) <- "double"
    n1 <- nrow(x)
    n2 <- nrow(y)
    dst <- matrix(double(1), nrow = n2, ncol = n1)
    out <- .Fortran("distance_matrix", x, y, as.integer(n1), as.integer(n2),
                    as.integer(spheric), dst = dst)
    out$dst
}

########################################
