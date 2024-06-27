
merging.functions <- function(locations.stn, newgrid, params, formuleRK,
                              nc.date, outdir, mrgOpts, gridBuffer)
{
    log.file <- file.path(outdir, "log_file.txt")
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

    igrid <- gridBuffer$igrid
    icoarse <- gridBuffer$icoarse
    coarsegrid <- gridBuffer$coarse

    ###############

    for(pass in seq(params$MRG$nrun)){
        newdata0 <- newgrid[igrid, ]
        # locations.stn$grd <- sp::over(locations.stn, newdata0)$grd
        locations.stn$grd <- sp::over(locations.stn, newgrid)$grd
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
            if(stats::var(locations.stn$stn) < 1e-07 |
               stats::var(locations.stn$grd, na.rm = TRUE) < 1e-07)
            {
                cat(paste(nc.date, ":", paste("Zero variance @ GLM pass#", pass), "|",
                    "Simple Bias Adjustment", "\n"), file = log.file, append = TRUE)
            }else{
                glm.stn <- stats::glm(formuleRK, data = locations.stn, family = stats::gaussian)
                if(any(is.na(glm.stn$coefficients[-1])) | glm.stn$coefficients[2] < 0){
                    cat(paste(nc.date, ":", paste("Invalid GLM coeffs pass#", pass), "|",
                        "Simple Bias Adjustment", "\n"), file = log.file, append = TRUE)
                }else{
                    sp.trend <- stats::predict(glm.stn, newdata = newdata0)
                    # sp.trend <- predict(glm.stn, newdata = newgrid)
                    ina.out <- is.na(sp.trend)
                    sp.trend[ina.out] <- newdata0@data$grd[ina.out]
                    # sp.trend[ina.out] <- newgrid@data$grd[ina.out]
                    xres <- rep(NA, length(locations.stn))
                    if(length(glm.stn$na.action) > 0){
                        xres[-glm.stn$na.action] <- glm.stn$residuals
                    }else{
                        xres <- glm.stn$residuals
                    }
                
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
        ## move down to exclude coarse grid when computing vgm
        if(mrgOpts$addCoarseGrid && length(coarsegrid) > 0){
            coarse_interp <- coarsegrid
            coarse_interp$res <- rep(0, length(coarse_interp))
            row.names(loc.stn) <- 1:length(loc.stn)
            row.names(coarse_interp) <- length(loc.stn) + (1:length(coarse_interp))
            loc.stn <- rbind(sf::st_as_sf(loc.stn), sf::st_as_sf(coarse_interp))
            loc.stn <- sf::as_Spatial(loc.stn)
        }

        #########
        vgm <- NULL
        if(interp.method == "okr"){
            calc.vgm <- if(length(loc.stn$res) > mrgOpts$vgmMinNumberSTN &
                           stats::var(loc.stn$res) > 1e-15) TRUE else FALSE
            if(calc.vgm){
                exp.var <- gstat::variogram(res~1, locations = loc.stn, cressie = TRUE)
                vgm <- try(gstat::fit.variogram(exp.var, gstat::vgm(params$interp$vgm.model)), silent = TRUE)
                msgErr <- NULL
                if(inherits(vgm, "try-error")){
                    cat(paste(nc.date, ":", paste("Unable to compute variogram ( Error ) pass#", pass), "|",
                        "Interpolation using IDW", "\n"), file = log.file, append = TRUE)
                    interp.method <- "idw"
                    vgm <- NULL
                    msgErr <- as.character(vgm)
                }

                if(params$interp$vgm_save){
                    fileVgm <- file.path(outdir, params$interp$vgm_dir, paste0("variogram_", nc.date, "_", pass, ".rds"))
                    outVgm <- list(data = loc.stn, sample = exp.var, vgm = vgm, valid = vgm$range[2] >= 0, msg = msgErr)
                    saveRDS(outVgm, fileVgm)
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
                if(stats::var(loc.stn$res) <= 1e-15)
                    vmsg <- "zero variance station data"
                cat(paste(nc.date, ":", paste("Unable to compute variogram (", vmsg, ") pass#", pass), "|",
                    "Interpolation using IDW", "\n"), file = log.file, append = TRUE)
                interp.method <- "idw"
            }
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
                ## rfe data: input gridded data (default)
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
                        ## rfe data: input data (default)
                        coarse_rnr$grd <- newgrid@data$grd[icoarse]
                    }

                    rnr_coarse <- ifelse(coarse_rnr$grd < wet.day, 0, 1)
                    coarse_rnr$rnr.stn <- rnr_coarse
                    coarse_rnr$rnr.grd <- rnr_coarse

                    row.names(loc.stn) <- 1:length(loc.stn)
                    row.names(coarse_rnr) <- length(loc.stn) + (1:length(coarse_rnr))
                    loc.stn <- rbind(sf::st_as_sf(loc.stn), sf::st_as_sf(coarse_rnr))
                    loc.stn <- sf::as_Spatial(loc.stn)
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

    if(params$RnoR$use && mrgOpts$saveRnoR){
        rnrfile <- file.path(outdir, 'RAIN-NO-RAIN', paste0('rnr_', nc.date, '.rds'))
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
