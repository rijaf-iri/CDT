
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
#'   \item{\code{saveGridBuffer}: }{logical, save the buffer of coarse grid used to interpolate the residuals. Default is \code{FALSE}}
#'   \item{\code{dirGridBuffer}: }{in case \code{saveGridBuffer} is \code{TRUE}, the full path to the directory to save the buffer data}
#'   \item{\code{saveRnoR}: }{logical, save the rain-no-rain mask. Default is \code{FALSE}}
#'   \item{\code{dirRnoR}: }{in case \code{saveRnoR} is \code{TRUE}, the full path to the directory to save the mask data}
#'   \item{\code{RnoRModel}: }{model to use to compute the rain-no-rain mask. Options are: \code{"logit"}, \code{"additive"}. Default is \code{"logit"}}
#'   \item{\code{RnoRCutOff}: }{integer, the method to be used to define the decision boundaries of the rain-no-rain mask. Options are: \code{1, 2, 3}
#'        \itemize{
#'           \item{\strong{option}: \code{1}}{
#'  %%
#'  \deqn{mask = \left\{
#'    \begin{array}{l l}
#'    0 & \quad \mbox{if } rnr < 0.5 \\
#'    1 & \quad \mbox{if } rnr \geq 0.5
#'    \end{array} \right.
#'  }{mask = if(rnr < 0) 0 else 1}
#'  %%
#'              }
#'           \item{\strong{option}: \code{2}}{
#'  %%
#'  \deqn{mask = \left\{
#'    \begin{array}{l l}
#'    0 & \quad \mbox{if } rnr < 0.1 \\
#'    rnr & \quad \mbox{if } rnr \geq 0.1
#'    \end{array} \right.
#'  }{mask = if(rnr < 0.1) 0 else rnr}
#'  %%
#'              }
#'           \item{\strong{option}: \code{3}}{
#'  %%
#'  \deqn{mask = \left\{
#'    \begin{array}{l l}
#'    0 & \quad \mbox{if } rnr < 0.25 \\
#'    rnr & \quad \mbox{if } 0.25 \leq rnr < 0.75 \\
#'    1 & \quad \mbox{if } rnr \geq 0.75
#'    \end{array} \right.
#'  }{mask = if(rnr < 0.25) 0 else if(rnr >= 0.25 & rnr < 0.75) rnr else 1}
#'  %%
#'                }
#'          }
#'     where \eqn{rnr} is the interpolated rain-no-rain values. Default is \code{3}.
#'     }
#'   \item{\code{RnoRaddCoarse}: }{logical, use the coarse grid to create the rain-no-rain mask. Default is \code{FALSE}}
#'   \item{\code{RnoRUseMerged}: }{logical, if \code{TRUE} the merged data is used to compute the rain-no-rain mask, otherwise the input gridded data is used. Default is \code{FALSE}}
#'   \item{\code{RnoRSmoothingPixels}: }{the number of pixels from the target pixel to be used to smooth the rain-no-rain mask. Default is 2.}
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

merging.getOption <- function(name)
{
    get("merging.options", envir = .cdtMRG)[[name]]
}

.defaultMrgOptions <- function(){
    list(saveGridBuffer = FALSE,
         dirGridBuffer = path.expand("~"),
         saveRnoR = FALSE,
         dirRnoR = path.expand("~"),
         ## RnoR model: "logit", "additive"
         RnoRModel = "logit", 
         RnoRCutOff = 3,
         RnoRaddCoarse = FALSE,
         RnoRUseMerged = FALSE,
         RnoRSmoothingPixels = 2
        )
}

###############################

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

coarse_grid_space <- function(res){
    space <- (0.4 / res)^0.9
    space <- ceiling(space)
    space[space == 0] <- 1
    space
}

create_grid_buffer <- function(locations.stn, newgrid,
                               saveGridBuffer = FALSE,
                               fileGridBuffer = "")
{
    nx <- newgrid@grid@cells.dim[1]
    ny <- newgrid@grid@cells.dim[2]
    resx <- newgrid@grid@cellsize[1]
    resy <- newgrid@grid@cellsize[2]
    rx <- coarse_grid_space(resx)
    ry <- coarse_grid_space(resy)

    radius <- 2 * max(c(rx, ry)) * mean(c(resx, resy))

    ix <- seq(1, nx, rx)
    iy <- seq(1, ny, ry)
    if(nx - ix[length(ix)] > 1) ix <- c(ix, nx)
    if(ny - iy[length(iy)] > 1) iy <- c(iy, ny)

    ixy <- expand.grid(ix, iy)
    icoarse <- ixy[, 1] + ((ixy[, 2] - 1) * nx)
    coarsegrid <- as(newgrid[icoarse, ], "SpatialPixels")

    #####
    if(saveGridBuffer){
        out_grid <- list(stn = locations.stn)
        out_grid$coarse0 <- coarsegrid
    }
    #####

    xgrd <- lapply(as.list(data.frame(coarsegrid@coords)), unique)
    loc.stn <- cdt.as.image(locations.stn$stn, locations.stn@coords, xgrd, regrid = TRUE)
    loc.stn <- cbind(do.call(expand.grid, loc.stn[c('x', 'y')]), z = c(loc.stn$z))
    loc.stn <- loc.stn[!is.na(loc.stn$z), , drop = FALSE]
    coordinates(loc.stn) <- c('x', 'y')

    dst <- fields::rdist(locations.stn@coords, coarsegrid@coords)
    dst <- colSums(dst < 0.5 * radius) == 0
    coarsegrid <- coarsegrid[dst, ]
    icoarse <- icoarse[dst]

    buffer.out <- rgeos::gBuffer(loc.stn, width = 1.25 * radius)
    icoarse.out <- as.logical(over(coarsegrid, buffer.out))
    icoarse.out[is.na(icoarse.out)] <- FALSE
    coarsegrid <- coarsegrid[icoarse.out, ]
    icoarse <- icoarse[icoarse.out]

    igrid <- as.logical(over(newgrid, buffer.out))
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

    list(igrid = igrid, coarse = coarsegrid, icoarse = icoarse)
}

###############################

merging.functions <- function(locations.stn, newgrid, params,
                              formuleRK, nc.date, log.file,
                              mrgOpts)
{
    spheric <- FALSE
    interp.method <- switch(params$MRG$method,
                            "CSc" = "cressman",
                            "BSc" = "barnes",
                            params$interp$method)

    if(params$RnoR$use){
        wet.day <- params$RnoR$wet
        if(wet.day <= 0) wet.day <- wet.day + 1e-13
        RnoR_out <- array(NA, newgrid@grid@cells.dim)
        RnoR_get <- 0
        RnoR_nmax <- params$interp$nmax[params$MRG$nrun]
    }

    if(interp.method %in% c("idw", "okr")){
        bGrd <- NULL
        if(params$interp$use.block)
            bGrd <- createBlock(newgrid@grid@cellsize, 1, 5)
    }

    ###############

    saveGridBuffer <- mrgOpts$saveGridBuffer
    dirGridBuffer <- mrgOpts$dirGridBuffer

    if(saveGridBuffer){
        dirMthd <- paste0(params$MRG$method, "-", interp.method)
        dirBuff <- file.path(dirGridBuffer, paste0("MRG_GRID_BUFFER_", dirMthd))
        if(!dir.exists(dirBuff))
            dir.create(dirBuff, showWarnings = FALSE, recursive = TRUE)

        fileGridBuffer <- file.path(dirBuff, paste0("grid_buffer_", nc.date, ".rds"))
        xy.grid <- create_grid_buffer(locations.stn, newgrid,
                                      saveGridBuffer, fileGridBuffer)
    }else{
        xy.grid <- create_grid_buffer(locations.stn, newgrid)
    }

    ###############

    igrid <- xy.grid$igrid
    icoarse <- xy.grid$icoarse
    coarsegrid <- xy.grid$coarse

    ###############

    for(pass in seq(params$MRG$nrun)){
        newdata0 <- newgrid[igrid, ]
        locations.stn$grd <- over(locations.stn, newdata0)$grd
        locations.stn <- locations.stn[!is.na(locations.stn$grd), ]

        if(length(locations.stn) < 5){
            cat(paste(nc.date, ":", paste("not enough station data pass#", pass), "|",
                "Output: gridded data", "\n"), file = log.file, append = TRUE)
            out.mrg <- matrix(newgrid@data$grd,
                              ncol = newgrid@grid@cells.dim[2],
                              nrow = newgrid@grid@cells.dim[1])
            return(out.mrg)
        }

        ###########

        sp.trend <- newdata0@data$grd
        xres <- locations.stn$stn - locations.stn$grd

        if(params$MRG$method == "RK"){
            if(var(locations.stn$stn) < 1e-07 |
               var(locations.stn$grd, na.rm = TRUE) < 1e-07)
            {
                cat(paste(nc.date, ":", paste("Zero variance @ GLM pass#", pass), "|",
                    "Simple Bias Adjustment", "\n"), file = log.file, append = TRUE)
            }else{
                glm.stn <- glm(formuleRK, data = locations.stn, family = stats::gaussian)
                if(any(is.na(glm.stn$coefficients[-1])) | glm.stn$coefficients[2] < 0){
                    cat(paste(nc.date, ":", paste("Invalid GLM coeffs pass#", pass), "|",
                        "Simple Bias Adjustment", "\n"), file = log.file, append = TRUE)
                }else{
                    sp.trend <- predict(glm.stn, newdata = newdata0)
                    # sp.trend <- predict(glm.stn, newdata = newgrid)
                    ina.out <- is.na(sp.trend)
                    sp.trend[ina.out] <- newdata0@data$grd[ina.out]
                    # sp.trend[ina.out] <- newgrid@data$grd[ina.out]
                    xres <- rep(NA, length(locations.stn))
                    if(length(glm.stn$na.action) > 0)
                        xres[-glm.stn$na.action] <- glm.stn$residuals
                    else
                        xres <- glm.stn$residuals
                
                    # sp.trend <- sp.trend[igrid]
                    # xres <- xres[igrid]
                }
            }
        }

        #########

        loc.stn <- locations.stn
        loc.stn$res <- xres
        loc.stn <- loc.stn['res']

        #########

        vgm <- NULL
        if(interp.method == "okr"){
            calc.vgm <- if(length(loc.stn$res) > params$interp$minstn &
                           var(loc.stn$res) > 1e-15) TRUE else FALSE
            if(calc.vgm){
                exp.var <- gstat::variogram(res~1, locations = loc.stn, cressie = TRUE)
                vgm <- try(gstat::fit.variogram(exp.var, gstat::vgm(params$interp$vgm.model)), silent = TRUE)
                if(inherits(vgm, "try-error")){
                    cat(paste(nc.date, ":", paste("Unable to compute variogram pass#", pass), "|",
                        "Interpolation using IDW", "\n"), file = log.file, append = TRUE)
                    interp.method <- "idw"
                }
            }else{
                cat(paste(nc.date, ":", paste("Unable to compute variogram pass#", pass), "|",
                    "Interpolation using IDW", "\n"), file = log.file, append = TRUE)
                interp.method <- "idw"
            }
        }

        #########

        if(length(coarsegrid) > 0){
            coarse_interp <- coarsegrid
            coarse_interp$res <- rep(0, length(coarse_interp))
            row.names(loc.stn) <- 1:length(loc.stn)
            row.names(coarse_interp) <- length(loc.stn) + (1:length(coarse_interp))
            loc.stn <- maptools::spRbind(loc.stn, coarse_interp)
        }

        #########

        nmin <- params$interp$nmin[pass]
        nmax <- params$interp$nmax[pass]

        #########

        if(params$interp$vargrd){
            if(interp.method %in% c("idw", "okr")){
                interp.res <- gstat::krige(res ~ 1, locations = loc.stn, newdata = newdata0, model = vgm,
                                           block = bGrd, nmin = nmin, nmax = nmax, debug.level = 0)
                interp.res <- interp.res$var1.pred
            }else{
                interp.res <- switch(interp.method,
                    "barnes" = barnes.interp(loc.stn@coords, loc.stn$res, newdata0@coords, nmin, nmax, spheric, p = 0.5),
                    "cressman" = cressman.interp(loc.stn@coords, loc.stn$res, newdata0@coords, nmin, nmax, spheric),
                    # "idw" = idw.interp(loc.stn@coords, loc.stn$res, newdata0@coords, nmin, nmax, spheric, p = 2),
                    "shepard" = shepard.interp(loc.stn@coords, loc.stn$res, newdata0@coords, nmin, nmax, spheric, p = 0.7),
                    "sphere" = spheremap.interp(loc.stn@coords, loc.stn$res, newdata0@coords, nmin, nmax, spheric),
                    # "okr" = kriging.interp(loc.stn@coords, loc.stn$res, newdata0@coords, vgm, nmin, nmax, spheric)
                )
                interp.res <- interp.res[, 3]
            }
        }else{
            maxdist <- params$interp$maxdist[pass]
            if(interp.method %in% c("idw", "okr")){
                interp.res <- gstat::krige(res ~ 1, locations = loc.stn, newdata = newdata0, model = vgm,
                                           block = bGrd, nmin = nmin, nmax = nmax, maxdist = maxdist, debug.level = 0)
                interp.res <- interp.res$var1.pred
            }else{
                ## replace
                interp.res <- switch(interp.method,
                    "barnes" = barnes.interp(loc.stn@coords, loc.stn$res, newdata0@coords, nmin, nmax, spheric, p = 0.5),
                    "cressman" = cressman.interp(loc.stn@coords, loc.stn$res, newdata0@coords, nmin, nmax, spheric),
                    # "idw" = idw.interp(loc.stn@coords, loc.stn$res, newdata0@coords, nmin, nmax, spheric, p = 2),
                    "shepard" = shepard.interp(loc.stn@coords, loc.stn$res, newdata0@coords, nmin, nmax, spheric, p = 0.7),
                    "sphere" = spheremap.interp(loc.stn@coords, loc.stn$res, newdata0@coords, nmin, nmax, spheric),
                    # "okr" = kriging.interp(loc.stn@coords, loc.stn$res, newdata0@coords, vgm, nmin, nmax, spheric)
                )
                interp.res <- interp.res[, 3]
            }
        }

        #########

        out.mrg <- newgrid@data$grd
        out.mrg[igrid] <- sp.trend + interp.res
        if(!params$MRG$negative) out.mrg[out.mrg < 0] <- 0
        ina <- is.na(out.mrg)
        out.mrg[ina] <- newgrid@data$grd[ina]

        #########

        if(params$RnoR$use){
            RnoRCutOff <- mrgOpts$RnoRCutOff

            if(mrgOpts$RnoRUseMerged){
                ## rfe data: fresh merged data
                newdata0$grd <- out.mrg[igrid]
            }else{
                ## rfe data: input gridded data
                newdata0$grd <- newgrid@data$grd[igrid]
            }
            newdata0$rnr.grd <- ifelse(newdata0$grd < wet.day, 0, 1)

            ######
            loc.stn <- locations.stn
            rnr_stn <- ifelse(loc.stn$stn < wet.day, 0, 1)
            loc.stn <- loc.stn['grd']
            loc.stn$rnr.stn <- rnr_stn
            loc.stn$rnr.grd <- ifelse(loc.stn$grd < wet.day, 0, 1)

            ######
            ## add coarse grid to locations.stn
            if(mrgOpts$RnoRUseMerged){
                if(length(coarsegrid) > 0){
                    coarse_rnr <- coarsegrid
                    if(mrgOpts$RnoRUseMerged){
                        ## rfe data: merged data
                        coarse_rnr$grd <- out.mrg[icoarse]
                    }else{
                        ## rfe data: input data
                        coarse_rnr$grd <- newgrid@data$grd[icoarse]
                    }

                    rnr_coarse <- ifelse(coarse_rnr$grd < wet.day, 0, 1)
                    coarse_rnr$rnr.stn <- rnr_coarse
                    coarse_rnr$rnr.grd <- rnr_coarse

                    row.names(loc.stn) <- 1:length(loc.stn)
                    row.names(coarse_rnr) <- length(loc.stn) + (1:length(coarse_rnr))
                    loc.stn <- maptools::spRbind(loc.stn, coarse_rnr)
                    loc.stn <- loc.stn[!is.na(loc.stn$grd), ]
                }
            }

            ######

            if(mrgOpts$RnoRModel == "logit"){
                rain_no_rain_fun <- rain_no_rain.mask_log
                rnr_nmax <- RnoR_nmax
            }else{
                rain_no_rain_fun <- rain_no_rain.mask_add
                rnr_nmax <- nmax
                # rnr_nmax <- RnoR_nmax
            }

            rnr0 <- rain_no_rain_fun(loc.stn, newdata0, rnr_nmax)
            rnr <- array(1, newgrid@grid@cells.dim)
            if(!is.null(rnr0)){
                rnr0 <- rain_no_rain.cut_off(rnr0, RnoRCutOff)
                rnr[igrid] <- rnr0
                if(params$RnoR$smooth)
                    rnr <- (2 * rnr + smooth.matrix(rnr, mrgOpts$RnoRSmoothingPixels))/3
                RnoR_out <- rnr
                RnoR_get <- pass
            }

            out.mrg <- out.mrg * c(rnr)
        }

        newgrid@data$grd <- out.mrg
    }

    #########

    if(params$RnoR$use & mrgOpts$saveRnoR){
        dirRnoR <- file.path(mrgOpts$dirRnoR, paste0('RnoR_DATA_', RnoRCutOff))
        if(!dir.exists(dirRnoR))
            dir.create(dirRnoR, showWarnings = FALSE, recursive = TRUE)

        rnrfile <- file.path(dirRnoR, paste0('rnr_', nc.date, '.rds'))

        xygrd <- lapply(as.list(data.frame(newgrid@coords)), unique)
        rnor <- list(x = xygrd$lon, y = xygrd$lat, z = RnoR_out, pass = RnoR_get)
        saveRDS(rnor, file = rnrfile)
    }

    #########

    dim(out.mrg) <- newgrid@grid@cells.dim

    return(out.mrg)
}

###############################

cdtMerging <- function(stnData, ncInfo, xy.grid, params, variable,
                       demData, outdir, mask = NULL, GUI = TRUE)
{
    log.file <- file.path(outdir, "log_file.txt")
    ncinfo <- ncInfo$ncinfo
    varinfo <- switch(variable,
                      "rain" = list(name = "precip",
                                    units = "mm",
                                    missval = -99,
                                    longname = "Merged Station-Satellite Rainfall",
                                    prec = {
                                            if(params$prec$from.data)
                                                ncinfo$varinfo$prec
                                            else
                                                params$prec$prec
                                           }
                                    ),
                      "temp" = list(name = "temp",
                                    units = "C",
                                    missval = -99,
                                    longname = "Downscaled Reanalysis merged with station",
                                    prec = ncinfo$varinfo$prec
                                    )
                    )

    params$MRG$negative <- switch(variable, "rain" = FALSE, "temp" = TRUE)

    ##################

    dx <- ncdim_def("Lon", "degree_east", xy.grid$lon)
    dy <- ncdim_def("Lat", "degree_north", xy.grid$lat)
    shuffle <- if(varinfo$prec %in% c("integer", "short")) TRUE else FALSE
    grd.nc.out <- ncvar_def(varinfo$name, varinfo$units, list(dx, dy), varinfo$missval,
                            longname = varinfo$longname, prec = varinfo$prec,
                            shuffle = shuffle, compression = 9)

    ##################

    newgrid <- defSpatialPixels(xy.grid)

    nmin <- ceiling(params$MRG$pass * params$interp$nmin)
    nmax <- ceiling(params$MRG$pass * params$interp$nmax)
    nmax <- ifelse(nmax - nmin < 2, nmax + 2, nmax)
    params$interp$nmin <- nmin
    params$interp$nmax <- nmax

    if(!params$interp$vargrd){
        maxdist <- params$MRG$pass * params$interp$maxdist
        params$interp$maxdist <- maxdist
    }else{
        bx <- diff(sapply(stnData[c('lon', 'lat')], range))
        # dg <- sqrt(bx[1]^2 + bx[2]^2) / 4
        dg <- sqrt(bx[1]^2 + bx[2]^2)
        dg <- 0.08 * dg + 0.199
        params$interp$maxdist <- params$MRG$pass * dg
    }

    locations.stn <- as.data.frame(stnData[c('lon', 'lat')])
    coordinates(locations.stn) <- c('lon', 'lat')
    ijs <- over(locations.stn, newgrid)
    locations.stn$stn <- rep(NA,  length(locations.stn))

    ##################

    xy.data <- defSpatialPixels(ncinfo[c('lon', 'lat')])
    # ijgs <- over(locations.stn, xy.data)

    is.regridNCDF <- is.diffSpatialPixelsObj(newgrid, xy.data, tol = 1e-07)
    ijnc <- NULL
    if(is.regridNCDF) ijnc <- over(newgrid, xy.data)

    ##################

    is.auxvar <- rep(FALSE, 5)
    formuleRK <- NULL
    if(params$MRG$method == "RK"){
        auxvar <- c('dem', 'slp', 'asp', 'alon', 'alat')
        is.auxvar <- unlist(params$auxvar[1:5])
        if(any(is.auxvar)){
            formuleRK <- formula(paste0('stn', '~', 'grd', '+',
                                 paste(auxvar[is.auxvar], collapse = '+')))
        }else{
            formuleRK <- formula(paste0('stn', '~', 'grd'))
        }

        if(is.auxvar['dem']) newgrid$dem <- c(demData$z)
        if(is.auxvar['slope'] | is.auxvar['aspect']){
            slpasp <- raster.slope.aspect(demData)
            if(is.auxvar['slope']) newgrid$slp <- c(slpasp$slope)
            if(is.auxvar['aspect']) newgrid$asp <- c(slpasp$aspect)
        }
        if(is.auxvar['lon']) newgrid$alon <- newgrid@coords[, 'lon']
        if(is.auxvar['lat']) newgrid$alat <- newgrid@coords[, 'lat']
        if(any(is.auxvar))
            locations.stn@data <- newgrid@data[ijs, , drop = FALSE]
    }

    ##################

    args <- methods::formalArgs(cdtMerging)
    for(v in args) assign(v, get(v), envir = environment())

    mrgOpts <- merging.options()

    parsL <- doparallel.cond(length(ncInfo$ncfiles) > 20)
    ret <- cdt.foreach(seq_along(ncInfo$ncfiles), parsL, GUI,
                       progress = TRUE, .packages = "sp",
                       FUN = function(jj)
    {
        if(ncInfo$exist[jj]){
            nc <- ncdf4::nc_open(ncInfo$ncfiles[jj])
            nc.val <- ncdf4::ncvar_get(nc, varid = ncinfo$varid)
            ncdf4::nc_close(nc)
            nc.val <- transposeNCDFData(nc.val, ncinfo)
        }else{
            msg <- paste(ncInfo$dates[jj], ":", "no NetCDF data",
                         "|", "no file generated", "\n")
            cat(msg, file = log.file, append = TRUE)
            return(-1)
        }

        if(all(is.na(nc.val))){
            msg <- paste(ncInfo$dates[jj], ":", "all NetCDF data are missing",
                         "|", "no file generated", "\n")
            cat(msg, file = log.file, append = TRUE)
            return(-1)
        }

        ######

        # locations.stn$grd <- nc.val[ijgs]
        newgrid$grd <- if(is.null(ijnc)) c(nc.val) else nc.val[ijnc]

        donne.stn <- stnData$data[which(stnData$date == ncInfo$dates[jj]), , drop = FALSE]
        if(nrow(donne.stn) == 0){
            msg <- paste(ncInfo$dates[jj], ":", "no station data", "|",
                         "No merging performed, output equals to the input NetCDF data", "\n")
            cat(msg, file = log.file, append = TRUE)
            write.merging.output(jj, nc.val, grd.nc.out, outdir,
                                 varinfo, ncInfo, params, mask)
            return(0)
        }

        locations.stn$stn <- as.numeric(donne.stn[1, ])
        noNA <- !is.na(locations.stn$stn)
        locations.stn <- locations.stn[noNA, ]
        donne.len <- length(locations.stn)

        if(donne.len == 0){
            msg <- paste(ncInfo$dates[jj], ":", "no station data", "|",
                         "No merging performed, output equals to the input NetCDF data", "\n")
            cat(msg, file = log.file, append = TRUE)
            write.merging.output(jj, nc.val, grd.nc.out, outdir,
                                 varinfo, ncInfo, params, mask)
            return(0)
        }

        if(donne.len > 0 & donne.len < 5){
            msg <- paste(ncInfo$dates[jj], ":", "not enough station data", "|",
                         "No merging performed, output equals to the input NetCDF data", "\n")
            cat(msg, file = log.file, append = TRUE)
            write.merging.output(jj, nc.val, grd.nc.out, outdir,
                                 varinfo, ncInfo, params, mask)
            return(0)
        }

        if(params$MRG$method == "RK" & any(is.auxvar)){
            loc.data <- !is.na(locations.stn@data)
            loc.data <- split(loc.data, col(loc.data))
            nna <- Reduce("&", loc.data)
            if(length(which(nna)) < 5){
                msg <- paste(ncInfo$dates[jj], ":", "not enough spatial points data", "|",
                             "No merging performed, output equals to the input NetCDF data", "\n")
                cat(msg, file = log.file, append = TRUE)
                write.merging.output(jj, nc.val, grd.nc.out, outdir,
                                     varinfo, ncInfo, params, mask)
                return(0)
            }
        }

        ######

        out.mrg <- merging.functions(locations.stn, newgrid, params,
                                     formuleRK, ncInfo$dates[jj],
                                     log.file, mrgOpts)

        write.merging.output(jj, out.mrg, grd.nc.out, outdir,
                             varinfo, ncInfo, params, mask)

        return(0)
    })

    ret <- do.call(c, ret)
    if(any(ret == -1)) return(-1)
    return(0)
}

###############################

write.merging.output <- function(jj, out.mrg, grd.nc.out, outdir, 
                                 varinfo, ncInfo, params, mask)
{
    if(!is.null(mask)) out.mrg[is.na(mask)] <- varinfo$missval

    out.mrg[is.na(out.mrg)] <- varinfo$missval

    year <- substr(ncInfo$dates[jj], 1, 4)
    month <- substr(ncInfo$dates[jj], 5, 6)
    if(params$period == 'daily'){
        ncfile <- sprintf(params$output$format, year, month, substr(ncInfo$dates[jj], 7, 8))
    }else if(params$period %in% c('pentad', 'dekadal')){
        ncfile <- sprintf(params$output$format, year, month, substr(ncInfo$dates[jj], 7, 7))
    }else ncfile <- sprintf(params$output$format, year, month)

    out.nc.file <- file.path(outdir, ncfile)
    nc <- ncdf4::nc_create(out.nc.file, grd.nc.out)
    ncdf4::ncvar_put(nc, grd.nc.out, out.mrg)
    ncdf4::nc_close(nc)

    return(0)
}
