
interp_biasCoeff_MBIAS <- function(coefData, demData, outdir, variable, lang.msg, GUI){
    Insert.Messages.Out(lang.msg[['8']], TRUE, "i", GUI)

    grdCoords <- coefData$data[c('lon', 'lat')]
    stnCoords <- coefData$data[c('lon_stn', 'lat_stn')]
    names(stnCoords) <- c('lon', 'lat')

    dx <- ncdf4::ncdim_def("Lon", "degree_east", grdCoords$lon)
    dy <- ncdf4::ncdim_def("Lat", "degree_north", grdCoords$lat)
    ncoutBS <- ncdf4::ncvar_def("bias", "", list(dx, dy), NA,
                                longname = "Multiplicative Mean Bias Factor",
                                prec = "float", compression = 9)

    interp <- get_biasInterp_params(stnCoords, grdCoords, demData, coefData$params$interp)

    if(interp$addcoarse && interp$savecoarse &&
       interp$pars$method %in% c("idw", "okr")){
        coarsedir <- file.path(outdir, "GRID_BUFFER")
        dir.create(coarsedir, showWarnings = FALSE, recursive = TRUE)
    }
    if(interp$pars$method == "okr"){
        vgmdir <- file.path(outdir, "VARIOGRAM")
        dir.create(vgmdir, showWarnings = FALSE, recursive = TRUE)
    }
    biasdir <- file.path(outdir, "BIAS_DATA")
    dir.create(biasdir, showWarnings = FALSE, recursive = TRUE)

    parsL <- doparallel.cond(TRUE)
    ret <- cdt.foreach(seq(nrow(coefData$bias)), parsL, GUI, progress = TRUE, FUN = function(jj)
    {
        ncfile <- file.path(biasdir, paste0("STN_GRD_Bias_Coeff_", jj, ".nc"))

        bsjj <- as.numeric(coefData$bias[jj, ])
        ina <- !is.na(bsjj)

        if(length(which(ina)) < 3){
            grd.bs <- array(1, c(interp$nlon, interp$nlat))
            nc <- ncdf4::nc_create(ncfile, ncoutBS)
            ncdf4::ncvar_put(nc, ncoutBS, grd.bs)
            ncdf4::nc_close(nc)
            return(NULL)
        }

        #####
        locbs <- interp$points

        if(interp$pars$method %in% c("idw", "okr", "nns")){
            locbs$Coef <- bsjj
            locbs <- locbs[ina, ]
        }else{
            locbs$Coef <- bsjj[!interp$elvNA]
            locbs <- locbs[ina[!interp$elvNA], ]
        }

        #####
        if(interp$pars$method %in% c("idw", "okr")){
            coarsefile <- ""
            if(interp$addcoarse && interp$savecoarse){
                coarsefile <- file.path(coarsedir, paste0("grid_buffer_", jj, ".rds"))
            }
            if(interp$pars$method == "idw"){
                grd.bs <- interp_biasCoeff_idw(interp, locbs, coarsefile)
            }
            if(interp$pars$method == "okr"){
                vgmfile <- file.path(vgmdir, paste0("variogram_", jj, ".rds"))
                grd.bs <- interp_biasCoeff_okr(interp, locbs, coarsefile, vgmfile)
            }
        }

        if(interp$pars$method == "nns"){
            grd.bs <- interp_biasCoeff_nns(interp, locbs)
        }

        if(interp$pars$method == "nn3d"){
            grd.bs <- interp_biasCoeff_nn3d(interp, locbs)
        }

        grd.bs <- matrix(grd.bs, interp$nlon, interp$nlat)

        #####
        if(variable == "rain"){
            grd.bs[grd.bs < 0.01] <- 0.01
            grd.bs[grd.bs > 3] <- 3
        }else if(variable == "temp"){
            grd.bs[grd.bs < 0.6] <- 0.6
            grd.bs[grd.bs > 1.5] <- 1.5
        }else{
            grd.bs[grd.bs < 0.6] <- 0.6
            grd.bs[grd.bs > 1.5] <- 1.5
        }

        grd.bs[is.na(grd.bs)] <- 1
        nc <- ncdf4::nc_create(ncfile, ncoutBS)
        ncdf4::ncvar_put(nc, ncoutBS, grd.bs)
        ncdf4::nc_close(nc)
        return(0)
    })

    Insert.Messages.Out(lang.msg[['9']], TRUE, "s", GUI)

    parsFile <- file.path(biasdir, 'input_parameters.rds')
    saveRDS(coefData["params"], parsFile)

    return(0)
}

interp_biasCoeff_MBIAS_wind <- function(coefData, demData, outdir, lang.msg, GUI){
    Insert.Messages.Out(lang.msg[['8']], TRUE, "i", GUI)

    grdCoords <- coefData$data[c('lon', 'lat')]
    stnCoords <- coefData$data[c('lon_stn', 'lat_stn')]
    names(stnCoords) <- c('lon', 'lat')

    dx <- ncdf4::ncdim_def("Lon", "degree_east", grdCoords$lon)
    dy <- ncdf4::ncdim_def("Lat", "degree_north", grdCoords$lat)
    ncoutBS <- list(
        ncdf4::ncvar_def("u_bias", "", list(dx, dy), NA,
                         longname = "U-Wind Multiplicative Mean Bias Factor",
                         prec = "float", compression = 9),
        ncdf4::ncvar_def("v_bias", "", list(dx, dy), NA,
                         longname = "V-Wind Multiplicative Mean Bias Factor",
                         prec = "float", compression = 9)
    )

    interp <- get_biasInterp_params(stnCoords, grdCoords, demData, coefData$params$interp)

    if(interp$addcoarse && interp$savecoarse &&
       interp$pars$method %in% c("idw", "okr")){
        coarsedir <- file.path(outdir, "GRID_BUFFER")
        dir.create(coarsedir, showWarnings = FALSE, recursive = TRUE)
    }
    if(interp$pars$method == "okr"){
        vgmdir <- file.path(outdir, "VARIOGRAM")
        dir.create(vgmdir, showWarnings = FALSE, recursive = TRUE)
    }
    biasdir <- file.path(outdir, "BIAS_DATA")
    dir.create(biasdir, showWarnings = FALSE, recursive = TRUE)

    parsL <- doparallel.cond(TRUE)
    ret <- cdt.foreach(seq(nrow(coefData$bias$U)), parsL, GUI, progress = TRUE, FUN = function(jj)
    {
        ncfile <- file.path(biasdir, paste0("STN_GRD_Bias_Coeff_", jj, ".nc"))

        u_bsjj <- as.numeric(coefData$bias$U[jj, ])
        u_ina <- !is.na(u_bsjj)
        v_bsjj <- as.numeric(coefData$bias$V[jj, ])
        v_ina <- !is.na(v_bsjj)
        uv_bias <- list(u_bsjj, v_bsjj)
        uv_ina <- list(u_ina, v_ina)
        uv_name <- c('u-wind', 'v-wind')

        if(length(which(u_ina)) < 3 || length(which(v_ina)) < 3){
            grd.bs <- array(1, c(interp$nlon, interp$nlat))
            nc <- ncdf4::nc_create(ncfile, ncoutBS)
            ncdf4::ncvar_put(nc, ncoutBS[[1]], grd.bs)
            ncdf4::ncvar_put(nc, ncoutBS[[2]], grd.bs)
            ncdf4::nc_close(nc)
            return(NULL)
        }

        #####

        grd_bias <- lapply(1:2, function(ll){
            locbs <- interp$points

            if(interp$pars$method %in% c("idw", "okr", "nns")){
                locbs$Coef <- uv_bias[[ll]]
                locbs <- locbs[uv_ina[[ll]], ]
            }else{
                locbs$Coef <- uv_bias[[ll]][!interp$elvNA]
                locbs <- locbs[uv_ina[[ll]][!interp$elvNA], ]
            }

            #####
            if(interp$pars$method %in% c("idw", "okr")){
                coarsefile <- ""
                if(interp$addcoarse && interp$savecoarse){
                    coarsefile <- file.path(coarsedir, paste0(uv_name[ll], "_grid_buffer_", jj, ".rds"))
                }
                if(interp$pars$method == "idw"){
                    grd.bs <- interp_biasCoeff_idw(interp, locbs, coarsefile)
                }
                if(interp$pars$method == "okr"){
                    vgmfile <- file.path(vgmdir, paste0(uv_name[ll], "_variogram_", jj, ".rds"))
                    grd.bs <- interp_biasCoeff_okr(interp, locbs, coarsefile, vgmfile)
                }
            }

            if(interp$pars$method == "nns"){
                grd.bs <- interp_biasCoeff_nns(interp, locbs)
            }

            if(interp$pars$method == "nn3d"){
                grd.bs <- interp_biasCoeff_nn3d(interp, locbs)
            }

            grd.bs <- matrix(grd.bs, interp$nlon, interp$nlat)

            #####
            grd.bs[grd.bs < 0.6] <- 0.6
            grd.bs[grd.bs > 1.5] <- 1.5
            grd.bs[is.na(grd.bs)] <- 1

            grd.bs
        })

        nc <- ncdf4::nc_create(ncfile, ncoutBS)
        ncdf4::ncvar_put(nc, ncoutBS[[1]], grd_bias[[1]])
        ncdf4::ncvar_put(nc, ncoutBS[[2]], grd_bias[[2]])
        ncdf4::nc_close(nc)
        return(0)
    })

    Insert.Messages.Out(lang.msg[['9']], TRUE, "s", GUI)

    parsFile <- file.path(biasdir, 'input_parameters.rds')
    saveRDS(coefData["params"], parsFile)

    return(0)
}

interp_biasCoeff_nns <- function(interp, locbs){
    grd_Coef <- gstat::krige(Coef~1, locations = locbs, newdata = interp$grid,
                             nmax = 1, maxdist = interp$pars$maxdist,
                             debug.level = 0)
    out <- grd_Coef$var1.pred
    out[is.na(out)] <- 1

    return(out)
}

interp_biasCoeff_nn3d <- function(interp, locbs){
    grd_Coef <- gstat::krige(Coef~1, locations = ~lon+lat+elv, data = locbs,
                             newdata = interp$grid, nmax = 1, debug.level = 0)
    out <- grd_Coef$var1.pred
    out[!interp$index] <- NA
    out[is.na(out)] <- 1

    return(out)
}

interp_biasCoeff_okr <- function(interp, locbs, coarsefile, vgmfile){
    newgrid <- interp$grid
    igrid <- rep(TRUE, length(newgrid))
    ## move down to exclude coarse when computing vgm
    if(interp$addcoarse){
        gridBuffer <- create_grid_buffer(locbs, interp$grid, interp$savecoarse, coarsefile)

        if(length(gridBuffer$coarse) > 0){
            coarse_interp <- gridBuffer$coarse
            coarse_interp$Coef <- rep(1, length(coarse_interp))
            row.names(locbs) <- 1:length(locbs)
            row.names(coarse_interp) <- length(locbs) + (1:length(coarse_interp))
            locbs <- rbind(sf::st_as_sf(locbs), sf::st_as_sf(coarse_interp))
            locbs <- sf::as_Spatial(locbs)
        }
        igrid <- gridBuffer$igrid
        newgrid <- interp$grid[igrid, ]
    }

    vgm <- NULL
    varCoef <- stats::var(locbs$Coef)
    if(length(locbs$Coef) >= interp$pars$minstn && varCoef > 1e-15){
        exp.var <- gstat::variogram(Coef~1, locations = locbs, cressie = TRUE)
        vgm <- try(gstat::fit.variogram(exp.var, gstat::vgm(interp$pars$vgm.model)), silent = TRUE)
        msgErr <- NULL
        if(inherits(vgm, "try-error")){
            vgm <- NULL
            msgErr <- as.character(vgm)
        }

        outVgm <- list(data = locbs, sample = exp.var, vgm = vgm, valid = vgm$range[2] >= 0, msg = msgErr)
        saveRDS(outVgm, vgmfile)

        if(vgm$range[2] < 0) vgm <- NULL
    }

    grd_Coef <- gstat::krige(Coef~1, locations = locbs, newdata = newgrid, model = vgm, block = interp$block,
                             nmin = interp$pars$nmin, nmax = interp$pars$nmax, maxdist = interp$pars$maxdist,
                             debug.level = 0)
    out <- rep(1, length(interp$grid))
    out[igrid] <- grd_Coef$var1.pred

    return(out)
}

interp_biasCoeff_idw <- function(interp, locbs, coarsefile){
    newgrid <- interp$grid
    igrid <- rep(TRUE, length(newgrid))
    if(interp$addcoarse){
        gridBuffer <- create_grid_buffer(locbs, interp$grid, interp$savecoarse, coarsefile)

        if(length(gridBuffer$coarse) > 0){
            coarse_interp <- gridBuffer$coarse
            coarse_interp$Coef <- rep(1, length(coarse_interp))
            row.names(locbs) <- 1:length(locbs)
            row.names(coarse_interp) <- length(locbs) + (1:length(coarse_interp))
            locbs <- rbind(sf::st_as_sf(locbs), sf::st_as_sf(coarse_interp))
            locbs <- sf::as_Spatial(locbs)
        }
        igrid <- gridBuffer$igrid
        newgrid <- interp$grid[igrid, ]
    }

    grd_Coef <- gstat::krige(Coef~1, locations = locbs, newdata = newgrid, block = interp$block,
                             nmin = interp$pars$nmin, nmax = interp$pars$nmax, maxdist = interp$pars$maxdist,
                             debug.level = 0)
    out <- rep(1, length(interp$grid))
    out[igrid] <- grd_Coef$var1.pred

    return(out)
}
