#' Geom points extraction
#'
#' Geom points extraction.
#' 
#' @param gridObj sp SpatialPixels
#' @param points data.frame(name, longitude, latitude)
#' @param padxy c(padx, pady) in decimal degree
#' 
#' @return A named list
#' 
#' @export

extractGeomPoints <- function(gridObj, points, padxy){
    padxy[is.na(padxy)] <- 0
    nxy <- gridObj@grid@cellsize
    padx <- round(padxy[1]/nxy[1])
    pady <- round(padxy[2]/nxy[2])

    voisin <- lapply(seq(nrow(points)), function(j){
                    xx <- points[j, 2] + nxy[1] * (-padx:padx)
                    yy <- points[j, 3] + nxy[2] * (-pady:pady)
                    if(length(xx) > 1 | length(yy) > 1){
                        xy <- defSpatialPixels(list(lon = xx, lat = yy))
                    }else{
                        xy <- data.frame(lon = xx, lat = yy)
                        sp::coordinates(xy) <- ~lon+lat
                    }
                    return(xy)
                })
    ij2xtr <- lapply(voisin, sp::over, y = gridObj)
    na_pts <- sapply(ij2xtr, function(x) !any(!is.na(x)))
    if(all(na_pts)) return(NULL)
    list(headinfo = points, ij2xtr = ij2xtr)
}

#' Geom polygons extraction
#'
#' Geom polygons extraction.
#' 
#' @param gridObj sp SpatialPixels
#' @param shpf sp SpatialPolygonsDataFrame (from shapefile)
#' @param attr_name attribute name, 'GID_1'
#' @param attr_values vector of the polygons to extract, c('RWA.1_1', 'RWA.5_1')
#' 
#' @return A named list
#' 
#' @export

extractGeomPolys <- function(gridObj, shpf, attr_name, attr_values){
    iattr <- as.character(sf::st_drop_geometry(shpf[, attr_name])[, 1])
    shpf.union <- stats::aggregate(iattr, list(iattr), identity)
    shpf.union$x <- seq_along(unique(iattr))
    shpf.geom <- lapply(shpf.union$x, function(j){
        y <- shpf[iattr == shpf.union$Group.1[j], ]
        sf::st_union(sf::st_geometry(y))
    })
    shpf.geom <- do.call(c, shpf.geom)
    sf::st_geometry(shpf.union) <- sf::st_sfc(shpf.geom)

    idPoly <- match(trimws(attr_values), trimws(shpf.union$Group.1))
    shpf.regOI <- shpf.union[idPoly, ]
    ctr_poly <- round(sf::st_coordinates(sf::st_centroid(shpf.regOI)), 5)
    headinfo <- cbind(as.character(shpf.regOI$Group.1), ctr_poly)
    headinfo[, 1] <- substr(gsub("[^[:alnum:]]", "", headinfo[, 1]), 1, 25)
    headinfo[, 1] <- stringi::stri_trans_general(str = headinfo[, 1], id = "Latin-ASCII")
    dimnames(headinfo) <- NULL

    polyRas <- gridObj
    polyRas$z <- seq_along(gridObj)
    polyRas <- raster::raster(polyRas)
    shpf.regOI <- sf::as_Spatial(shpf.regOI)

    ij2xtr <- raster::extract(polyRas, shpf.regOI, weights = TRUE,
                        normalizeWeights = TRUE, cellnumbers = TRUE)

    na_pts <- sapply(ij2xtr, is.null)
    if(all(na_pts)) return(NULL)
    list(headinfo = headinfo, ij2xtr = ij2xtr)
}

# extractGeomPolys <- function(gridObj, shpf, attr_name, attr_values){
#     iattr <- as.character(shpf@data[, attr_name])
#     shpf.union <- maptools::unionSpatialPolygons(shpf, iattr)
#     shpf.df <- stats::aggregate(methods::as(shpf, "data.frame")[, 1], list(iattr), identity)
#     shpf.df$x <- seq(shpf.union)
#     row.names(shpf.df) <- sapply(methods::slot(shpf.union, "polygons"), function(x) methods::slot(x, "ID"))

#     shpf.union <- sp::SpatialPolygonsDataFrame(shpf.union, shpf.df)

#     # idPoly <- trimws(shpf.union@data$Group.1) %in% trimws(attr_values)
#     idPoly <- match(trimws(attr_values), trimws(shpf.union@data$Group.1))
#     shpf.regOI <- shpf.union[idPoly, ]
    
#     headinfo <- cbind(as.character(shpf.regOI@data$Group.1),
#                       round(sp::coordinates(shpf.regOI), 5))
#     headinfo[, 1] <- substr(gsub("[^[:alnum:]]", "", headinfo[, 1]), 1, 25)
#     headinfo[, 1] <- stringi::stri_trans_general(str = headinfo[, 1], id = "Latin-ASCII")

#     polyRas <- gridObj
#     polyRas$z <- seq_along(gridObj)
#     polyRas <- raster::raster(polyRas)
#     ij2xtr <- raster::extract(polyRas, shpf.regOI, weights = TRUE,
#                         normalizeWeights = TRUE, cellnumbers = TRUE)

#     na_pts <- sapply(ij2xtr, is.null)
#     if(all(na_pts)) return(NULL)
#     list(headinfo = headinfo, ij2xtr = ij2xtr)
# }

######

#' Geom points extraction
#'
#' Geom points extraction.
#' 
#' @param gridObj sp SpatialPixels
#' @param bbox the bounding box of the rectangle list(minlon = 33, maxlon = 35, minlat = -14, maxlat = -10)
#' 
#' @return A named list
#' 
#' @export

extractGeomRect <- function(gridObj, bbox){
    poly <- cbind(c(bbox$minlon, bbox$maxlon, bbox$maxlon, bbox$minlon, bbox$minlon),
                  c(bbox$minlat, bbox$minlat, bbox$maxlat, bbox$maxlat, bbox$minlat))
    rectPoly <- sp::Polygon(poly)
    rectPoly <- sp::Polygons(list(rectPoly), "p1")
    geomPoly <- sp::SpatialPolygons(list(rectPoly), 1:1)

    headinfo <- matrix(c('Rectangle', rowMeans(sp::bbox(geomPoly))), nrow = 1)

    polyRas <- gridObj
    polyRas$z <- seq_along(gridObj)
    polyRas <- raster::raster(polyRas)
    ij2xtr <- raster::extract(polyRas, geomPoly, weights = TRUE,
                              normalizeWeights = TRUE, cellnumbers = TRUE)

    if(is.null(ij2xtr[[1]])) return(NULL)
    list(headinfo = headinfo, ij2xtr = ij2xtr)
}
