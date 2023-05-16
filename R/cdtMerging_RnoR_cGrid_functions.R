
rain_no_rain.mask_log <- function(locations.stn, newgrid, nmax)
{
    glm.binom <- tryCatch(
            glm(rnr.stn ~ grd, data = locations.stn, family = stats::binomial(link = "logit")),
            error=function(e) e, warning=function(w) w
        )
    if(inherits(glm.binom, "warning") | inherits(glm.binom, "error")) return(NULL)

    rnr <- NULL
    if(!is.na(glm.binom$coef[2])){
        locations.stn$rnr.res <- residuals(glm.binom)
        rnr.trend <- predict(glm.binom, newdata = newgrid, type = 'link')

        rnr.res.grd <- gstat::krige(rnr.res~1, locations = locations.stn, newdata = newgrid,
                                    nmax = nmax, set = list(idp = 4.0), debug.level = 0)

        rnr <- rnr.trend + rnr.res.grd$var1.pred
        rnr <- exp(rnr) / (1 + exp(rnr))

        rnr[is.na(rnr)] <- 1
    }

    return(rnr)
}

rain_no_rain.mask_add <- function(locations.stn, newgrid, nmax)
{
    locations.stn$rnr.res <- locations.stn$rnr.stn - locations.stn$rnr.grd
    rnr.trend <- newgrid$rnr.grd

    rnr.res.grd <- gstat::krige(rnr.res~1, locations = locations.stn, newdata = newgrid,
                                nmax = nmax, set = list(idp = 4.0), debug.level = 0)

    rnr <- rnr.trend + rnr.res.grd$var1.pred
    rnr[rnr < 0] <- 0
    rnr[rnr > 1] <- 1

    rnr[is.na(rnr)] <- 1

    return(rnr)
}

rain_no_rain.cut_off <- function(rnr, RnoRCutOff){
    if(RnoRCutOff == 1){
        rnr[rnr >= 0.5] <- 1
        rnr[rnr < 0.5] <- 0
    }else if(RnoRCutOff == 2){
        rnr[rnr < 0.1] <- 0
    }else{
        rnr[rnr < 0.25] <- 0
        rnr[rnr > 0.75] <- 1
    }
  
    return(rnr)
}

###############################

coarse_grid_space <- function(res, resCoarseGrid){
    space <- resCoarseGrid/res
    space <- ceiling(space)
    space[space == 0] <- 1
    space
}

create_grid_buffer <- function(locations.stn, newgrid,
                               saveGridBuffer = FALSE,
                               fileGridBuffer = "",
                               useLocalInterpolation = TRUE,
                               resCoarseGrid = 0.5
                              )
{
    nx <- newgrid@grid@cells.dim[1]
    ny <- newgrid@grid@cells.dim[2]
    resx <- newgrid@grid@cellsize[1]
    resy <- newgrid@grid@cellsize[2]
    rx <- coarse_grid_space(resx, resCoarseGrid)
    ry <- coarse_grid_space(resy, resCoarseGrid)

    radius <- 2 * max(c(rx, ry)) * mean(c(resx, resy))

    ix <- seq(1, nx, rx)
    iy <- seq(1, ny, ry)
    if(nx - ix[length(ix)] > 1) ix <- c(ix[-length(ix)], nx)
    if(ny - iy[length(iy)] > 1) iy <- c(iy[-length(iy)], ny)

    ixy <- expand.grid(ix, iy)
    icoarse <- ixy[, 1] + ((ixy[, 2] - 1) * nx)
    coarsegrid <- as(newgrid[icoarse, ], "SpatialPixels")

    resx_c <- resx * rx
    resy_c <- resy * ry

    #####
    if(saveGridBuffer){
        out_grid <- list(stn = locations.stn)
        out_grid$coarse0 <- coarsegrid
        out_grid$resx_c <- resx_c
        out_grid$resy_c <- resy_c
    }
    #####

    xgrd <- lapply(as.list(data.frame(coarsegrid@coords)), unique)
    # change stn to any temporary variable with new values
    loc.stn <- cdt.as.image(locations.stn$stn, locations.stn@coords, xgrd, regrid = TRUE)
    loc.stn <- cbind(do.call(expand.grid, loc.stn[c('x', 'y')]), z = c(loc.stn$z))
    loc.stn <- loc.stn[!is.na(loc.stn$z), , drop = FALSE]
    coordinates(loc.stn) <- c('x', 'y')

    dst <- fields::rdist(locations.stn@coords, coarsegrid@coords)
    dst <- colSums(dst < 0.5 * radius) == 0
    coarsegrid <- coarsegrid[dst, ]
    icoarse <- icoarse[dst]

    width <- if(useLocalInterpolation) 1.25 * radius else 2.5

    buffer.out <- rgeos::gBuffer(loc.stn, width = width)
    icoarse.out <- as.logical(sp::over(coarsegrid, buffer.out))
    icoarse.out[is.na(icoarse.out)] <- FALSE
    coarsegrid <- coarsegrid[icoarse.out, ]
    icoarse <- icoarse[icoarse.out]

    igrid <- as.logical(sp::over(newgrid, buffer.out))
    igrid[is.na(igrid)] <- FALSE

    #####
    if(saveGridBuffer){
        out_grid$buffer <- buffer.out
        out_grid$icoarse <- icoarse
        out_grid$coarse1 <- coarsegrid
        out_grid$igrid <- igrid
        saveRDS(out_grid, fileGridBuffer)
    }
    #####

    list(igrid = igrid, coarse = coarsegrid, icoarse = icoarse,
         resx_c = resx_c, resy_c = resy_c)
}
