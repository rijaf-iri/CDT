
rain_no_rain.mask <- function(locations.stn, newgrid, nmax)
{
    glm.binom <- tryCatch(
            glm(rnr ~ grd, data = locations.stn, family = stats::binomial(link = "logit")),
            error=function(e) e, warning=function(w) w
        )

    if(inherits(glm.binom, "warning") | inherits(glm.binom, "error")) return(NULL)

    rnr <- NULL
    if(!is.na(glm.binom$coef[2])){
        locations.stn$rnr.res <- residuals(glm.binom)
        rnr.trend <- predict(glm.binom, newdata = newgrid, type = 'link')

        rnr.res.grd <- gstat::krige(rnr.res~1, locations = locations.stn, newdata = newgrid, nmax = nmax, debug.level = 0)
        rnr <- rnr.trend + rnr.res.grd$var1.pred

        rnr <- exp(rnr) / (1 + exp(rnr))
        ### decision boundary = 0.5
        rnr[rnr >= 0.5] <- 1
        rnr[rnr < 0.5] <- 0
        rnr[is.na(rnr)] <- 1
    }

    return(rnr)
}

#######################

create_grid_buffer <- function(locations.stn, newgrid, radius, spheric)
{
    nx <- newgrid@grid@cells.dim[1]
    ny <- newgrid@grid@cells.dim[2]
    rx <- as.integer(radius/newgrid@grid@cellsize[1])
    ry <- as.integer(radius/newgrid@grid@cellsize[2])
    ix <- seq(1, newgrid@grid@cells.dim[1], rx)
    iy <- seq(1, newgrid@grid@cells.dim[2], ry)
    if(nx - ix[length(ix)] > rx/3) ix <- c(ix, nx)
    if(ny - iy[length(iy)] > ry/3) iy <- c(iy, ny)
    ixy <- expand.grid(ix, iy)
    ixy <- ixy[, 1] + ((ixy[, 2] - 1) * nx)
    coarsegrid <- as(newgrid[ixy, ], "SpatialPixels") 
    if(spheric){
        ctr <- rowSums(coarsegrid@bbox)/2
        pts <- c(ctr[1] + radius * cos(pi/4), ctr[2] + radius * sin(pi/4))
        radS <- distance.vector(pts, matrix(ctr, nrow = 1), spheric)
    }else radS <- radius

    dst <- distance.matrix(locations.stn@coords, coarsegrid@coords, spheric)
    dst <- rowSums(dst < 0.5 * radS) == 0
    coarsegrid <- coarsegrid[dst, ]

    buffer.out <- rgeos::gBuffer(locations.stn, width = 2 * radius)
    icoarse.out <- as.logical(over(coarsegrid, buffer.out))
    icoarse.out[is.na(icoarse.out)] <- FALSE
    coarsegrid <- coarsegrid[icoarse.out, ]

    buffer.grid <- rgeos::gBuffer(locations.stn, width = radius)
    igrid <- as.logical(over(newgrid, buffer.grid))
    igrid[is.na(igrid)] <- FALSE
    newdata0 <- newgrid[igrid, ]
    list(grid.buff = newdata0, ij = igrid, coarse = coarsegrid)
}

#######################

merging.functions <- function(locations.stn, newgrid, params,
                              formuleRK, nc.date, log.file)
{
    spheric <- FALSE
    interp.method <- switch(params$MRG$method,
                            "CSc" = "cressman",
                            "BSc" = "barnes",
                            params$interp$method)

    if(params$RnoR$use){
        wet.day <- params$RnoR$wet
        if(wet.day <= 0) wet.day <- wet.day + 1e-13
        locations.stn$rnr <- ifelse(locations.stn$stn < wet.day, 0, 1)
    }

    if(interp.method %in% c("idw", "okr")){
        bGrd <- NULL
        if(params$interp$use.block)
            bGrd <- createBlock(newgrid@grid@cellsize, 1, 5)
    }

    for(pass in seq(params$MRG$nrun)){
        xy.grid <- create_grid_buffer(locations.stn, newgrid, params$interp$maxdist[pass], spheric)
        newdata0 <- xy.grid$grid.buff

        igrid <- xy.grid$ij
        coarsegrid <- xy.grid$coarse
        coarsegrid$res <- rep(0, length(coarsegrid))

        ###########

        locations.stn$grd <- over(locations.stn, newdata0)$grd
        locations.stn <- locations.stn[!is.na(locations.stn$grd), ]

        ###########

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
                    ina.out <- is.na(sp.trend)
                    sp.trend[ina.out] <- newdata0@data$grd[ina.out]
                    xres <- rep(NA, length(locations.stn))
                    if(length(glm.stn$na.action) > 0)
                        xres[-glm.stn$na.action] <- glm.stn$residuals
                    else
                        xres <- glm.stn$residuals
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
            row.names(loc.stn) <- 1:length(loc.stn)
            row.names(coarsegrid) <- length(loc.stn) + (1:length(coarsegrid))
            loc.stn <- maptools::spRbind(loc.stn, coarsegrid)
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
                    "barnes" = barnes.interp(loc.stn@coords, loc.stn$res, newdata0@coords, nmin, nmax, spheric, p = 2),
                    "cressman" = cressman.interp(loc.stn@coords, loc.stn$res, newdata0@coords, nmin, nmax, spheric),
                    # "idw" = idw.interp(loc.stn@coords, loc.stn$res, newdata0@coords, nmin, nmax, spheric, p = 2),
                    "shepard" = shepard.interp(loc.stn@coords, loc.stn$res, newdata0@coords, nmin, nmax, spheric, p = 2),
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
                    "barnes" = barnes.interp(loc.stn@coords, loc.stn$res, newdata0@coords, nmin, nmax, spheric, p = 2),
                    "cressman" = cressman.interp(loc.stn@coords, loc.stn$res, newdata0@coords, nmin, nmax, spheric),
                    # "idw" = idw.interp(loc.stn@coords, loc.stn$res, newdata0@coords, nmin, nmax, spheric, p = 2),
                    "shepard" = shepard.interp(loc.stn@coords, loc.stn$res, newdata0@coords, nmin, nmax, spheric, p = 2),
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
            newdata0$grd <- out.mrg[igrid]
            rnr0 <- rain_no_rain.mask(locations.stn, newdata0, nmax)
            if(!is.null(rnr0)){
                rnr <- array(1, newgrid@grid@cells.dim)
                rnr[igrid] <- rnr0

               if(params$RnoR$smooth){
                    if(pass == params$MRG$nrun){
                        ij <- over(locations.stn, as(newgrid, "SpatialPixels"))
                        rnr[ij] <- locations.stn$rnr
                    }
                    rnr <- (2 * rnr + smooth.matrix(rnr, 2))/3
                }
                out.mrg <- out.mrg * c(rnr)
            }
        }

        #########

        newgrid@data$grd <- out.mrg
    }

    dim(out.mrg) <- newgrid@grid@cells.dim

    return(out.mrg)
}

#######################

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
        if(any(is.auxvar)) locations.stn@data <- newgrid@data[ijs, , drop = FALSE]
    }

    ##################

    args <- methods::formalArgs(cdtMerging)
    for(v in args) assign(v, get(v), envir = environment())

    parsL <- doparallel.cond(length(ncInfo$ncfiles) > 30)
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
            cat(paste(ncInfo$dates[jj], ":", "no netcdf data", "|",
                "no file generated", "\n"), file = log.file, append = TRUE)
            return(-1)
        }

        if(all(is.na(nc.val))){
            cat(paste(ncInfo$dates[jj], ":", "all netcdf data are missing", "|",
                "no file generated", "\n"), file = log.file, append = TRUE)
            return(-1)
        }

        ######

        # locations.stn$grd <- nc.val[ijgs]
        newgrid$grd <- if(is.null(ijnc)) c(nc.val) else nc.val[ijnc]

        donne.stn <- stnData$data[which(stnData$date == ncInfo$dates[jj]), , drop = FALSE]
        if(nrow(donne.stn) == 0){
            cat(paste(ncInfo$dates[jj], ":", "no station data", "|",
                "no file generated", "\n"), file = log.file, append = TRUE)
            return(-1)
        }

        locations.stn$stn <- as.numeric(donne.stn[1, ])
        noNA <- !is.na(locations.stn$stn)
        locations.stn <- locations.stn[noNA, ]

        if(length(locations.stn) < 5){
            cat(paste(ncInfo$dates[jj], ":", "not enough station data", "|",
                "no file generated", "\n"), file = log.file, append = TRUE)
            return(-1)
        }

        if(params$MRG$method == "RK" & any(is.auxvar)){
            loc.data <- !is.na(locations.stn@data)
            loc.data <- split(loc.data, col(loc.data))
            nna <- Reduce("&", loc.data)
            if(length(which(nna)) < 5){
                cat(paste(ncInfo$dates[jj], ":", "not enough spatial points data", "|",
                    "no file generated", "\n"), file = log.file, append = TRUE)
                return(-1)
            }
        }

        ######

        out.mrg <- merging.functions(locations.stn, newgrid, params,
                                     formuleRK, ncInfo$dates[jj], log.file)

        ###################
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
    })

    ret <- do.call(c, ret)
    if(any(ret == -1)) return(-1)
    return(0)
}

