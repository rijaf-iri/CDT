
coarse_grid_resolution <- function(newgrid, step){
    nx <- newgrid@grid@cells.dim[1]
    ny <- newgrid@grid@cells.dim[2]
    resx <- newgrid@grid@cellsize[1]
    resy <- newgrid@grid@cellsize[2]

    ix <- seq(1, nx, step)
    iy <- seq(1, ny, step)

    if(ix[length(ix)] < nx){
        if((nx - ix[length(ix)]) > step/2){
            ix <- c(ix, nx)
        }else{
            ix <- c(ix[-length(ix)], nx)
        }
    }

    if(iy[length(iy)] < ny){
        if((ny - iy[length(iy)]) > step/2){
            iy <- c(iy, ny)
        }else{
            iy <- c(iy[-length(iy)], ny)
        }
    }

    ixy <- expand.grid(ix, iy)
    icoarse <- ixy[, 1] + ((ixy[, 2] - 1) * nx)
    coarsegrid <- methods::as(newgrid[icoarse, ], "SpatialPixels")

    resx_c <- resx * step
    resy_c <- resy * step

    radius <- mean(c(resx_c, resx_c))

    list(coarsegrid = coarsegrid,
         radius = radius,
         icoarse = icoarse)
}

##############################

## grid: sp SpatialPixels object
## buffer: sp SpatialPolygons object
index_grid_inside_buffer <- function(grid, buffer){
    ic <- as.logical(sp::over(grid, buffer))
    ic[is.na(ic)] <- FALSE
    ic
}

## buffer1: inside
## buffer2: outside
index_grid_between_buffer <- function(grid, buffer1, buffer2){
    ic1 <- index_grid_inside_buffer(grid, buffer1)
    ic2 <- index_grid_inside_buffer(grid, buffer2)
    !ic1 & ic2
}

# points_coords: matrix lon & lat
# grid_coords: matrix lon & lat
# radius: distance in decimal degree, 
# all grids with distances from points
# less than radius will be removed 
# (set to FALSE)
remove_grid_near_points <- function(points_coords, grid_coords, radius){
    mdist <- fields::rdist(points_coords, grid_coords)
    colSums(mdist < radius) == 0
}

##############################

create_grid_buffer <- function(locations.stn, newgrid,
                               saveGridBuffer = FALSE,
                               fileGridBuffer = "")
{
    resxy <- max(newgrid@grid@cellsize)
    if(resxy <= 0.1){
        steps <- c(10, 7, 4)
        fc <- 0.9
    }else if(resxy > 0.1 & resxy <= 0.2){
        steps <- c(5, 2)
        fc <- 0.9
    }else if(resxy > 0.2 & resxy <= 0.3){
        steps <- c(3, 1)
        fc <- 1
    }else{
        steps <- 1
        fc <- 1.5
    }
    # steps <- c(10, 7, 4)
    # fc <- 1
    # fc <- 0.5
    # fc <- 0.9

    tmp <- lapply(steps, function(s) coarse_grid_resolution(newgrid, s))
    idst <- remove_grid_near_points(locations.stn@coords,
                                    tmp[[1]]$coarsegrid@coords,
                                    tmp[[1]]$radius)
    jw <- 2
    while(!any(idst) && jw <= length(steps)){
        idst <- remove_grid_near_points(locations.stn@coords,
                                        tmp[[jw]]$coarsegrid@coords,
                                        tmp[[jw]]$radius)
        tmp <- tmp[-1]
        jw <- jw + 1
    }

    if(any(idst)){
        coarse <- icoarse <- vector("list", length = length(tmp))
        buffer_sf0 <- sf::st_buffer(sf::st_as_sf(tmp[[1]]$coarsegrid[!idst, ]), dist = fc * tmp[[1]]$radius)
        buffer_sf0 <- sf::st_union(buffer_sf0)
        buffer_sp0 <- sf::as_Spatial(buffer_sf0)

        for(j in seq_along(tmp)){
            buffer_sf1 <- sf::st_buffer(buffer_sf0, dist = 2 * tmp[[j]]$radius)
            buffer_sf1 <- sf::st_union(buffer_sf1)
            buffer_sp1 <- sf::as_Spatial(buffer_sf1)

            ic <- index_grid_between_buffer(tmp[[j]]$coarsegrid, buffer_sp0, buffer_sp1)
            coarse[[j]] <- tmp[[j]]$coarsegrid[ic, ]
            icoarse[[j]] <- tmp[[j]]$icoarse[ic]

            buffer_sf0 <- buffer_sf1
            buffer_sp0 <- buffer_sp1
        }

        amount <- max(newgrid@grid@cellsize)/10
        coarse <- lapply(coarse, function(x){
            s <- sf::st_as_sf(x)
            sf::st_jitter(s, amount)
        })
        coarse <- do.call(rbind, coarse)
        coarse <- sf::as_Spatial(coarse)
        icoarse <- unname(do.call(c, icoarse))
        igrid <- as.logical(sp::over(newgrid, buffer_sp1))
        igrid[is.na(igrid)] <- FALSE
    }else{
        buffer_sp1 <- NULL
        icoarse <- numeric(0)
        coarse <- methods::as(newgrid[1, ], "SpatialPoints")
        coarse <- coarse[-1]
        igrid <- rep(TRUE, length(newgrid))
    }

    if(saveGridBuffer){
        stn <- methods::as(locations.stn, 'SpatialPoints')
        out_grid <- list(stn = stn)
        out_grid$buffer <- buffer_sp1
        out_grid$coarse <- coarse
        # out_grid$newgrid <- newgrid[igrid, ]
        saveRDS(out_grid, fileGridBuffer, compress = "bzip2")
    }

    list(coarse = coarse, igrid = igrid, icoarse = icoarse)
}

##############################################################

### sf

# coarse_grid_resolution <- function(newgrid, step){
#     nx <- newgrid@grid@cells.dim[1]
#     ny <- newgrid@grid@cells.dim[2]
#     resx <- newgrid@grid@cellsize[1]
#     resy <- newgrid@grid@cellsize[2]

#     ix <- seq(1, nx, step)
#     iy <- seq(1, ny, step)

#     if(ix[length(ix)] < nx){
#         if((nx - ix[length(ix)]) > step/2){
#             ix <- c(ix, nx)
#         }else{
#             ix <- c(ix[-length(ix)], nx)
#         }
#     }

#     if(iy[length(iy)] < ny){
#         if((ny - iy[length(iy)]) > step/2){
#             iy <- c(iy, ny)
#         }else{
#             iy <- c(iy[-length(iy)], ny)
#         }
#     }

#     ixy <- expand.grid(ix, iy)
#     icoarse <- ixy[, 1] + ((ixy[, 2] - 1) * nx)
#     coarse <- sf::st_as_sf(newgrid[icoarse, ])

#     resx_c <- resx * step
#     resy_c <- resy * step

#     radius <- mean(c(resx_c, resx_c))

#     list(coarse = coarse,
#          radius = radius,
#          icoarse = icoarse)
# }

# ##############################

# ## grid: sf point object
# ## buffer: sf mutlipolygon object

# # index_grid_inside_buffer <- function(grid, buffer){
# #     iover <- sf::st_intersects(grid, buffer)
# #     sapply(iover, function(s) if(length(s) == 0) FALSE else TRUE)
# # }
# index_grid_inside_buffer <- function(grid, buffer){
#     grid <- sf::as_Spatial(grid)
#     buffer <- sf::as_Spatial(buffer)
#     ic <- as.logical(sp::over(grid, buffer))
#     ic[is.na(ic)] <- FALSE
#     ic
# }

# ## buffer1: inside
# ## buffer2: outside
# index_grid_between_buffer <- function(grid, buffer1, buffer2){
#     ic1 <- index_grid_inside_buffer(grid, buffer1)
#     ic2 <- index_grid_inside_buffer(grid, buffer2)
#     !ic1 & ic2
# }

# # points_coords: matrix lon & lat
# # grid_coords: matrix lon & lat
# # radius: distance in decimal degree, 
# # all grids with distances from points
# # less than radius will be removed 
# # (set to FALSE)
# remove_grid_near_points <- function(points_coords, grid_coords, radius){
#     mdist <- fields::rdist(points_coords, grid_coords)
#     colSums(mdist < radius) == 0
# }

# ##############################

# create_grid_buffer <- function(locations.stn, newgrid,
#                                saveGridBuffer = FALSE,
#                                fileGridBuffer = "")
# {
#     steps <- c(10, 7, 4)
#     tmp <- lapply(steps, function(s) coarse_grid_resolution(newgrid, s))
#     idst <- remove_grid_near_points(locations.stn@coords,
#                                     sf::st_coordinates(tmp[[1]]$coarse),
#                                     tmp[[1]]$radius)
#     jw <- 2
#     while(!any(idst) && jw <= length(steps)){
#         idst <- remove_grid_near_points(locations.stn@coords,
#                                         sf::st_coordinates(tmp[[jw]]$coarse),
#                                         tmp[[jw]]$radius)
#         tmp <- tmp[-1]
#         jw <- jw + 1
#     }

#     # fc <- 1
#     # fc <- 0.5
#     fc <- 0.9

#     if(any(idst)){
#         coarse <- icoarse <- vector("list", length = length(tmp))
#         buffer_sf0 <- sf::st_buffer(tmp[[1]]$coarse[!idst, ], dist = fc * tmp[[1]]$radius)
#         buffer_sf0 <- sf::st_union(buffer_sf0)

#         for(j in seq_along(tmp)){
#             buffer_sf1 <- sf::st_buffer(buffer_sf0, dist = 2 * tmp[[j]]$radius)
#             buffer_sf1 <- sf::st_union(buffer_sf1)

#             ic <- index_grid_between_buffer(tmp[[j]]$coarse, buffer_sf0, buffer_sf1)
#             coarse[[j]] <- tmp[[j]]$coarse[ic, ]
#             icoarse[[j]] <- tmp[[j]]$icoarse[ic]

#             buffer_sf0 <- buffer_sf1
#         }

#         amount <- max(newgrid@grid@cellsize)/10
#         coarse <- lapply(coarse, function(x){
#             sf::st_jitter(x, amount)
#         })
#         coarse <- do.call(rbind, coarse)
#         icoarse <- unname(do.call(c, icoarse))
#         buffer_sp1 <- sf::as_Spatial(buffer_sf1)
#         igrid <- as.logical(sp::over(newgrid, buffer_sp1))
#         igrid[is.na(igrid)] <- FALSE
#     }else{
#         buffer_sp1 <- NULL
#         icoarse <- numeric(0)
#         coarse <- sf::st_as_sf(newgrid[1, ])[-1, ]
#         igrid <- rep(TRUE, length(newgrid))
#     }

#     if(saveGridBuffer){
#         out_grid <- list(stn = locations.stn)
#         out_grid$buffer <- buffer_sp1
#         if(any(idst)){
#             out_grid$coarse <- sf::as_Spatial(coarse)
#         }else{
#             out_grid$coarse <- methods::as(newgrid[1, ], "SpatialPoints")[-1]
#         }
#         out_grid$newgrid <- newgrid[igrid, ]
#         saveRDS(out_grid, fileGridBuffer)
#     }

#     list(coarse = coarse, igrid = igrid, icoarse = icoarse)
# }
