
#' Options Controlling the blanking buffer outside the polygons boundaries.
#'
#' Functions to handle the width of buffer outside the polygons boundaries.
#' 
#' @param ... using one or more arguments of the form name = value.
#'   Existing values can be retrieved by supplying the names (as character strings) of the components as unnamed arguments.
#' @details
#' Available options are
#' \itemize{ 
#'    \item{\code{bufferOption}: }{character, options are \code{"default"} or \code{"user"}.
#'         \itemize{
#'              \item{\code{"default"}: }{a buffer of 4 times of the spatial resolution of the gridded data is used}
#'              \item{\code{"user"}: }{the width of the buffer will be defined by the user in \code{bufferWidth}}
#'            }
#'         }
#'    \item{\code{bufferWidth}: }{numeric, the width of the buffer outside the polygons in decimal degree}
#' }
#' 
#' @export

blanking.options <- function(...){
    ## copied from lattice.options
    new <- list(...)
    if(is.null(names(new)) && length(new) == 1 && is.list(new[[1]])) new <- new[[1]]
    old <- .cdtMRG$blanking.options
    if(length(new) == 0) return(old)
    
    nm <- names(new)
    if (is.null(nm)) return(old[unlist(new)])

    isNamed <- nm != ""
    if (any(!isNamed)) nm[!isNamed] <- unlist(new[!isNamed])
    retVal <- old[nm]
    names(retVal) <- nm
    nm <- nm[isNamed]

    .cdtMRG$blanking.options <- utils::modifyList(old, new[nm])

    invisible(retVal)
}

.defaultBlankingOptions <- function(){
    list(
         bufferOption = "user",
         bufferWidth = 0
        )
}

#######

## create mask, blanking
# ncgrid: named list(lon, lat)

create.mask.grid <- function(shp_sf, ncgrid){
    sf_type <- sf::st_geometry_type(shp_sf, by_geometry = FALSE)
    sf_type <- as.character(sf_type)[1]
    if(!(sf_type == "MULTIPOLYGON" | sf_type == "POLYGON")){
        stop('Shapefile must be a polygons')
    }

    width <- min(diff(sapply(ncgrid, range)) / (sapply(ncgrid, length) - 1))

    shp <- sf::st_geometry(shp_sf)
    shp <- sf::st_union(shp)
    shp <- sf::st_simplify(shp, preserveTopology = TRUE, dTolerance = width / 4)

    blkOpts <- blanking.options()
    if(blkOpts$bufferOption == "default"){
        buffer <- 4 * width
    }else{
        buffer <- blkOpts$bufferWidth
    }

    if(buffer > 0){
        shp <- sf::st_buffer(shp, dist = buffer)
    }

    xygrid <- defRegularGrid(ncgrid)
    mask <- sf::st_intersects(xygrid, shp)
    mask <- sapply(mask, length)
    mask <- ifelse(mask == 0, NA, 1)
    dim(mask) <- sapply(ncgrid, length)

    return(mask)
}


# create.mask.grid <- function(shp, ncgrid){
#     width <- mean(diff(sapply(ncgrid, range)) / (sapply(ncgrid, length) - 1))
#     shp <- methods::as(shp, "SpatialPolygons")
#     shp <- rgeos::gUnaryUnion(shp)
#     shp <- rgeos::gSimplify(shp, tol = width / 4, topologyPreserve = TRUE)

#     blkOpts <- blanking.options()
#     if(blkOpts$bufferOption == "default"){
#         buffer <- 4 * width
#     }else{
#         buffer <- blkOpts$bufferWidth
#     }

#     if(buffer > 0){
#         shp <- rgeos::gBuffer(shp, width = buffer)
#     }

#     slot.shp <- methods::slot(shp, "polygons")
#     shp.df <- data.frame(vtmp = rep(1, length(slot.shp)))
#     row.names(shp.df) <- sapply(slot.shp, function(x) methods::slot(x, "ID"))
#     shp <- sp::SpatialPolygonsDataFrame(shp, shp.df)
#     mask <- sp::over(defSpatialPixels(ncgrid), shp)[, 'vtmp']
#     dim(mask) <- sapply(ncgrid, length)
#     return(mask)
# }
