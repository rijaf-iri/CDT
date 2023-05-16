
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
                                    longname = "Downscaled Temperature Reanalysis merged with station",
                                    prec = ncinfo$varinfo$prec
                                    ),
                      "rh" = list(name = "rh",
                                  units = "%",
                                  missval = -99,
                                  longname = "Downscaled Relative Humidity Reanalysis merged with station",
                                  prec = ncinfo$varinfo$prec
                                 ),
                      "pres" = list(name = "pres",
                                    units = "hPa",
                                    missval = -99,
                                    longname = "Downscaled Pressure Reanalysis merged with station",
                                    prec = ncinfo$varinfo$prec
                                   ),
                      "prmsl" = list(name = "prmsl",
                                    units = "hPa",
                                    missval = -99,
                                    longname = "Downscaled MSL Reanalysis merged with station",
                                    prec = ncinfo$varinfo$prec
                                   ),
                      "rad" = list(name = "rad",
                                   units = "W/m2",
                                   missval = -99,
                                   longname = "Downscaled Radiation Reanalysis merged with station",
                                   prec = ncinfo$varinfo$prec
                                 )
                    )

    params$MRG$limits <- switch(variable,
                                "rain" = c(0, 5000),
                                "temp" = c(-40, 50),
                                "rh" = c(0, 100),
                                "pres" = c(700, 1100),
                                "prmsl" = c(850, 1100),
                                "rad" = c(0, 1300),
                                NULL)

    ##################

    dx <- ncdf4::ncdim_def("Lon", "degree_east", xy.grid$lon)
    dy <- ncdf4::ncdim_def("Lat", "degree_north", xy.grid$lat)
    shuffle <- if(varinfo$prec %in% c("integer", "short")) TRUE else FALSE
    grd.nc.out <- ncdf4::ncvar_def(varinfo$name, varinfo$units, list(dx, dy), varinfo$missval,
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
    locations.stn$stn <- rep(NA, length(locations.stn))

    ##################

    xy.data <- defSpatialPixels(ncinfo[c('lon', 'lat')])

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

        newgrid$grd <- if(is.null(ijnc)) c(nc.val) else nc.val[ijnc]

        donne.stn <- stnData$data[which(stnData$date == ncInfo$dates[jj]), , drop = FALSE]
        if(nrow(donne.stn) == 0){
            msg <- paste(ncInfo$dates[jj], ":", "no station data", "|",
                         "No merging performed, output equals to the input NetCDF data", "\n")
            cat(msg, file = log.file, append = TRUE)

            nc.val <- newgrid$grd
            dim(nc.val) <- newgrid@grid@cells.dim
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

            nc.val <- newgrid$grd
            dim(nc.val) <- newgrid@grid@cells.dim
            write.merging.output(jj, nc.val, grd.nc.out, outdir,
                                 varinfo, ncInfo, params, mask)
            return(0)
        }

        if(donne.len > 0 & donne.len < mrgOpts$mrgMinNumberSTN){
            msg <- paste(ncInfo$dates[jj], ":", "not enough station data", "|",
                         "No merging performed, output equals to the input NetCDF data", "\n")
            cat(msg, file = log.file, append = TRUE)

            nc.val <- newgrid$grd
            dim(nc.val) <- newgrid@grid@cells.dim
            write.merging.output(jj, nc.val, grd.nc.out, outdir,
                                 varinfo, ncInfo, params, mask)
            return(0)
        }

        if(params$MRG$method == "RK" & any(is.auxvar)){
            loc.data <- !is.na(locations.stn@data)
            loc.data <- split(loc.data, col(loc.data))
            nna <- Reduce("&", loc.data)
            if(length(which(nna)) < mrgOpts$rkMinNumberSTN){
                msg <- paste(ncInfo$dates[jj], ":", "not enough spatial points data", "|",
                             "No merging performed, output equals to the input NetCDF data", "\n")
                cat(msg, file = log.file, append = TRUE)

                nc.val <- newgrid$grd
                dim(nc.val) <- newgrid@grid@cells.dim
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
        if(params$interp$use.block){
            bGrd <- switch(mrgOpts$blockType,
                            "userdefined" = createBlock(mrgOpts$blockSize),
                            "gaussian" = mrgOpts$blockSize
                           )
        }
    }

    ###############

    saveGridBuffer <- mrgOpts$saveGridBuffer
    dirGridBuffer <- mrgOpts$dirGridBuffer
    fileGridBuffer <- ""

    if(saveGridBuffer){
        dirMthd <- paste0(params$MRG$method, "-", interp.method)
        dirBuff <- file.path(dirGridBuffer, paste0("MRG_GRID_BUFFER_", dirMthd))
        if(!dir.exists(dirBuff))
            dir.create(dirBuff, showWarnings = FALSE, recursive = TRUE)

        fileGridBuffer <- file.path(dirBuff, paste0("grid_buffer_", nc.date, ".rds"))
    }

    xy.grid <- create_grid_buffer(locations.stn, newgrid,
                                  saveGridBuffer, fileGridBuffer,
                                  mrgOpts$useLocalInterpolation,
                                  mrgOpts$resCoarseGrid
                                )

    igrid <- xy.grid$igrid
    icoarse <- xy.grid$icoarse
    coarsegrid <- xy.grid$coarse

    ###############

    for(pass in seq(params$MRG$nrun)){
        newdata0 <- newgrid[igrid, ]
        locations.stn$grd <- over(locations.stn, newdata0)$grd
        locations.stn <- locations.stn[!is.na(locations.stn$grd), ]

        if(length(locations.stn) < mrgOpts$mrgMinNumberSTN){
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
            calc.vgm <- if(length(loc.stn$res) > mrgOpts$vgmMinNumberSTN &
                           var(loc.stn$res) > 1e-15) TRUE else FALSE
            if(calc.vgm){
                exp.var <- gstat::variogram(res~1, locations = loc.stn, cressie = TRUE)
                vgm <- try(gstat::fit.variogram(exp.var, gstat::vgm(params$interp$vgm.model)), silent = TRUE)
                if(inherits(vgm, "try-error")){
                    cat(paste(nc.date, ":", paste("Unable to compute variogram ( Error ) pass#", pass), "|",
                        "Interpolation using IDW", "\n"), file = log.file, append = TRUE)
                    interp.method <- "idw"
                    vgm <- NULL
                }
                if(vgm$range[2] < 0){
                    cat(paste(nc.date, ":", paste("Variogram range is negative pass#", pass), "|",
                        "Interpolation using IDW", "\n"), file = log.file, append = TRUE)
                    interp.method <- "idw"
                    vgm <- NULL
                }
            }else{
                if(length(loc.stn$res) <= mrgOpts$vgmMinNumberSTN)
                    vmsg <- "not enough station data"
                if(var(loc.stn$res) <= 1e-15)
                    vmsg <- "zero variance station data"
                cat(paste(nc.date, ":", paste("Unable to compute variogram (", vmsg, ") pass#", pass), "|",
                    "Interpolation using IDW", "\n"), file = log.file, append = TRUE)
                interp.method <- "idw"
            }
        }

        #########

        if(mrgOpts$addCoarseGrid & length(coarsegrid) > 0){
            coarse_interp <- coarsegrid
            coarse_interp$res <- rep(0, length(coarse_interp))
            row.names(loc.stn) <- 1:length(loc.stn)
            row.names(coarse_interp) <- length(loc.stn) + (1:length(coarse_interp))
            loc.stn <- maptools::spRbind(loc.stn, coarse_interp)
        }

        #########

        ## change maxdist Inf to finite value for interp other than idw and okr
        if(mrgOpts$useLocalInterpolation){
            nmin <- params$interp$nmin[pass]
            nmax <- params$interp$nmax[pass]
            maxdist <- if(params$interp$vargrd) 180 else params$interp$maxdist[pass]
            # maxdist <- if(params$interp$vargrd) Inf else params$interp$maxdist[pass]
        }else{
            nmin <- 0
            nmax <- 1000
            ## 180 degree
            maxdist <- 180
            # nmax <- Inf
            # maxdist <- Inf
        }

        interp.res <- merging.residuals.interp(loc.stn, newdata0, interp.method,
                                               params$interp$vargrd, nmin, nmax,
                                               maxdist, bGrd, vgm, spheric, mrgOpts
                                              )
        #########

        out.mrg <- newgrid@data$grd
        out.mrg[igrid] <- sp.trend + interp.res

        out.mrg[out.mrg < params$MRG$limits[1]] <- params$MRG$limits[1]
        out.mrg[out.mrg > params$MRG$limits[2]] <- params$MRG$limits[2]

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
            if(mrgOpts$RnoRaddCoarse){
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

merging.residuals.interp <- function(locations, newdata, interp.method,
                                     var.grid, nmin, nmax, maxdist = Inf,
                                     block = NULL, vgm = NULL, spheric = FALSE,
                                     mrgOpts = list(powerWeightIDW = 2,
                                                    powerWeightShepard = 0.7,
                                                    powerWeightBarnes = 0.5))
{
    if(var.grid){
        if(interp.method %in% c("idw", "okr")){
            interp.res <- gstat::krige(res ~ 1, locations = locations, newdata = newdata, model = vgm,
                                       block = block, nmin = nmin, nmax = nmax,
                                       set = list(idp = mrgOpts$powerWeightIDW), debug.level = 0)
            interp.res <- interp.res$var1.pred
        }else{
            interp.res <- switch(interp.method,
                "barnes" = barnes.interp(locations@coords, locations$res, newdata@coords, nmin, nmax, spheric, p = mrgOpts$powerWeightBarnes),
                "cressman" = cressman.interp(locations@coords, locations$res, newdata@coords, nmin, nmax, spheric),
                # "idw" = idw.interp(locations@coords, locations$res, newdata@coords, nmin, nmax, spheric, p = mrgOpts$powerWeightIDW),
                "shepard" = shepard.interp(locations@coords, locations$res, newdata@coords, nmin, nmax, spheric, p = mrgOpts$powerWeightShepard),
                "sphere" = spheremap.interp(locations@coords, locations$res, newdata@coords, nmin, nmax, spheric),
                # "okr" = kriging.interp(locations@coords, locations$res, newdata@coords, vgm, nmin, nmax, spheric)
            )
            interp.res <- interp.res[, 3]
        }
    }else{
        if(interp.method %in% c("idw", "okr")){
            interp.res <- gstat::krige(res ~ 1, locations = locations, newdata = newdata, model = vgm,
                                       block = block, nmin = nmin, nmax = nmax, maxdist = maxdist,
                                       set = list(idp = mrgOpts$powerWeightIDW), debug.level = 0)
            interp.res <- interp.res$var1.pred
        }else{
            ## replace
            interp.res <- switch(interp.method,
                "barnes" = barnes.interp(locations@coords, locations$res, newdata@coords, nmin, nmax, spheric, p = mrgOpts$powerWeightBarnes),
                "cressman" = cressman.interp(locations@coords, locations$res, newdata@coords, nmin, nmax, spheric),
                # "idw" = idw.interp(locations@coords, locations$res, newdata@coords, nmin, nmax, spheric, p = mrgOpts$powerWeightIDW),
                "shepard" = shepard.interp(locations@coords, locations$res, newdata@coords, nmin, nmax, spheric, p = mrgOpts$powerWeightShepard),
                "sphere" = spheremap.interp(locations@coords, locations$res, newdata@coords, nmin, nmax, spheric),
                # "okr" = kriging.interp(locations@coords, locations$res, newdata@coords, vgm, nmin, nmax, spheric)
            )
            interp.res <- interp.res[, 3]
        }
    }

    return(interp.res)
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
