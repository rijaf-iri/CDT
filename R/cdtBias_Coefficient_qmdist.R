
write_distrParms_QMDIST <- function(coefData, outdir, lang.msg, GUI){
    Insert.Messages.Out(lang.msg[['8']], TRUE, "i", GUI)

    coords <- coefData$data[c('lon_box', 'lat_box')]
    nlon <- length(coords$lon_box)
    nlat <- length(coords$lat_box)

    dx <- ncdf4::ncdim_def("Lon", "degree_east", coords$lon_box)
    dy <- ncdf4::ncdim_def("Lat", "degree_north", coords$lat_box)

    ncoutParams <- lapply(coefData$distr_info$pars, function(p){
        longname <- paste(coefData$distr_info$longname, 'parameter', p)
        ncdf4::ncvar_def(p, "", list(dx, dy), NA, longname, "float", compression = 9)
    })

    biasdir <- file.path(outdir, "BIAS_DATA")
    dir.create(biasdir, showWarnings = FALSE, recursive = TRUE)

    for(jj in 1:12){
        stn_pars <- coefData$distr_params$stn[[jj]]
        grd_pars <- coefData$distr_params$grd[[jj]]
        if(is.null(stn_pars) | is.null(grd_pars)) next

        stn_miss <- sapply(stn_pars, function(x) all(is.na(x)))
        grd_miss <- sapply(grd_pars, function(x) all(is.na(x)))
        if(any(stn_miss) | any(grd_miss)) next

        stn_pars <- lapply(coefData$distr_info$pars, function(nn){
            pr <- stn_pars[[nn]]
            dim(pr) <- c(nlon, nlat)
            pr
        })
        grd_pars <- lapply(coefData$distr_info$pars, function(nn){
            pr <- grd_pars[[nn]]
            dim(pr) <- c(nlon, nlat)
            pr
        })

        stn_file <- file.path(biasdir, paste0("STN_Distr_Pars_", coefData$distr_info$name, "_", jj, ".nc"))
        nc <- ncdf4::nc_create(stn_file, ncoutParams)
        for(j in seq_along(coefData$distr_info$pars))
            ncdf4::ncvar_put(nc, ncoutParams[[j]], stn_pars[[j]])
        ncdf4::nc_close(nc)

        grd_file <- file.path(biasdir, paste0("GRD_Distr_Pars_", coefData$distr_info$name, "_", jj, ".nc"))
        nc <- ncdf4::nc_create(grd_file, ncoutParams)
        for(j in seq_along(coefData$distr_info$pars))
            ncdf4::ncvar_put(nc, ncoutParams[[j]], grd_pars[[j]])
        ncdf4::nc_close(nc)
    }

    ##########

    voisin <- biascoeff.getOption("maxBoxNeighbor")
    boxGrid <- as.matrix(expand.grid(coords))
    gcoords <- coefData$data[c('lon', 'lat')]
    newGrid <- as.matrix(expand.grid(gcoords))
    dst <- fields::rdist(boxGrid, newGrid)
    ord <- apply(dst, 2, order)
    dst <- apply(dst, 2, sort)
    ord <- ord[1:voisin, , drop = FALSE]
    dst <- dst[1:voisin, , drop = FALSE] / (2 * coefData$data$diag)
    wk <- exp(-(dst / 0.33)^2)

    out_pars <- coefData[c("distr_info", "params")]
    out_pars$interp <- list(lon = gcoords$lon, lat = gcoords$lat, id = ord, wk = wk)

    parsFile <- file.path(biasdir, 'input_parameters.rds')
    saveRDS(out_pars, parsFile)

    Insert.Messages.Out(lang.msg[['9']], TRUE, "s", GUI)

    return(0)
}

interp_distrParms_QMDIST <- function(coefData, demData, outdir, lang.msg, GUI){
    Insert.Messages.Out(lang.msg[['8']], TRUE, "i", GUI)

    grdCoords <- coefData$data[c('lon', 'lat')]
    stnCoords <- coefData$data[c('lon_stn', 'lat_stn')]
    names(stnCoords) <- c('lon', 'lat')

    dx <- ncdf4::ncdim_def("Lon", "degree_east", grdCoords$lon)
    dy <- ncdf4::ncdim_def("Lat", "degree_north", grdCoords$lat)

    ncoutParams <- lapply(coefData$distr_info$pars, function(p){
        longname <- paste(coefData$distr_info$longname, 'parameter', p)
        ncdf4::ncvar_def(p, "", list(dx, dy), NA, longname, "float", compression = 9)
    })

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
    ret <- cdt.foreach(1:12, parsL, GUI, progress = TRUE, FUN = function(jj)
    {
        stn_pars <- coefData$distr_params$stn[[jj]]
        grd_pars <- coefData$distr_params$grd[[jj]]
        if(is.null(stn_pars) | is.null(grd_pars)) return(NULL)

        stn_miss <- sapply(stn_pars, function(x) length(which(!is.na(x))) < 3)
        grd_miss <- sapply(grd_pars, function(x) length(which(!is.na(x))) < 10)
        if(any(stn_miss) | any(grd_miss)) return(NULL)

        #####
        distrPars <- lapply(coefData$distr_info$pars, function(nn){
            sdpar <- stn_pars[[nn]]
            ina <- !is.na(sdpar)
            dataGrd <- grd_pars[[nn]]
            
            min_val <- min(c(sdpar, dataGrd), na.rm = TRUE)
            max_val <- max(c(sdpar, dataGrd), na.rm = TRUE)

            locStn <- interp$points
            if(interp$pars$method %in% c("idw", "okr", "nns")){
                locStn$par <- sdpar
                locStn <- locStn[ina, ]
            }else{
                locStn$par <- sdpar[!interp$elvNA]
                locStn <- locStn[ina[!interp$elvNA], ]
            }

            #####
            if(interp$pars$method %in% c("idw", "okr")){
                coarsefile <- ""
                if(interp$addcoarse && interp$savecoarse){
                    coarsefile <- file.path(coarsedir, paste0("grid_buffer_", jj, ".rds"))
                }
                if(interp$pars$method == "idw"){
                    grd.bs <- interp_distrParams_idw(interp, locStn, dataGrd, coarsefile)
                }
                if(interp$pars$method == "okr"){
                    vgmfile <- file.path(vgmdir, paste0("variogram_", jj, ".rds"))
                    grd.bs <- interp_distrParams_okr(interp, locStn, dataGrd, coarsefile, vgmfile)
                }
            }

            if(interp$pars$method == "nns"){
                grd.bs <- interp_distrParams_nns(interp, locStn, dataGrd)
            }

            if(interp$pars$method == "nn3d"){
                grd.bs <- interp_distrParams_nn3d(interp, locStn, dataGrd)
            }

            # miss <- is.na(grd.bs) | is.na(dataGrd)
            # grd.bs[miss] <- NA
            # dataGrd[miss] <- NA
            grd.bs[grd.bs < min_val] <- min_val
            grd.bs[grd.bs > max_val] <- max_val

            grd.bs <- matrix(grd.bs, interp$nlon, interp$nlat)
            dataGrd <- matrix(dataGrd, interp$nlon, interp$nlat)

            list(stn = grd.bs, grd = dataGrd)
        })

        stn_pars <- lapply(distrPars, '[[', 'stn')
        grd_pars <- lapply(distrPars, '[[', 'grd')

        #####
        stn_file <- file.path(biasdir, paste0("STN_Distr_Pars_", coefData$distr_info$name, "_", jj, ".nc"))
        nc <- ncdf4::nc_create(stn_file, ncoutParams)
        for(j in seq_along(coefData$distr_info$pars))
            ncdf4::ncvar_put(nc, ncoutParams[[j]], stn_pars[[j]])
        ncdf4::nc_close(nc)

        grd_file <- file.path(biasdir, paste0("GRD_Distr_Pars_", coefData$distr_info$name, "_", jj, ".nc"))
        nc <- ncdf4::nc_create(grd_file, ncoutParams)
        for(j in seq_along(coefData$distr_info$pars))
            ncdf4::ncvar_put(nc, ncoutParams[[j]], grd_pars[[j]])
        ncdf4::nc_close(nc)

        return(0)
    })

    Insert.Messages.Out(lang.msg[['9']], TRUE, "s", GUI)

    parsFile <- file.path(biasdir, 'input_parameters.rds')
    saveRDS(coefData[c("distr_info", "params")], parsFile)

    return(0)
}

interp_distrParams_nns <- function(interp, locStn, dataGrd){
    grd_Par <- gstat::krige(par~1, locations = locStn, newdata = interp$grid,
                            nmax = 1, maxdist = interp$pars$maxdist,
                            debug.level = 0)
    out <- grd_Par$var1.pred
    ina <- is.na(out)
    out[ina] <- dataGrd[ina]

    return(out)
}

interp_distrParams_nn3d <- function(interp, locStn, dataGrd){
    grd_Par <- gstat::krige(par~1, locations = ~lon+lat+elv, data = locStn,
                            newdata = interp$grid, nmax = 1, debug.level = 0)
    out <- grd_Par$var1.pred
    out[!interp$index] <- NA
    ina <- is.na(out)
    out[ina] <- dataGrd[ina]

    return(out)
}

interp_distrParams_okr <- function(interp, locStn, dataGrd, coarsefile, vgmfile){
    newgrid <- interp$grid
    igrid <- rep(TRUE, length(newgrid))
    ## move down to exclude coarse when computing vgm
    if(interp$addcoarse){
        gridBuffer <- create_grid_buffer(locStn, interp$grid, interp$savecoarse, coarsefile)

        if(length(gridBuffer$coarse) > 0){
            coarse_interp <- gridBuffer$coarse
            coarse_interp$par <- dataGrd[gridBuffer$icoarse]
            row.names(locStn) <- 1:length(locStn)
            row.names(coarse_interp) <- length(locStn) + (1:length(coarse_interp))
            locStn <- rbind(sf::st_as_sf(locStn), sf::st_as_sf(coarse_interp))
            locStn <- sf::as_Spatial(locStn)
            locStn <- locStn[!is.na(locStn$par), ]
        }
        igrid <- gridBuffer$igrid
        newgrid <- interp$grid[igrid, ]
    }

    vgm <- NULL
    varPar <- stats::var(locStn$par)
    if(length(locStn$par) >= interp$pars$minstn && varPar > 1e-15){
        exp.var <- gstat::variogram(par~1, locations = locStn, cressie = TRUE)
        vgm <- try(gstat::fit.variogram(exp.var, gstat::vgm(interp$pars$vgm.model)), silent = TRUE)
        msgErr <- NULL
        if(inherits(vgm, "try-error")){
            vgm <- NULL
            msgErr <- as.character(vgm)
        }

        outVgm <- list(data = locStn, sample = exp.var, vgm = vgm, valid = vgm$range[2] >= 0, msg = msgErr)
        saveRDS(outVgm, vgmfile)

        if(vgm$range[2] < 0) vgm <- NULL
    }

    grd_Par <- gstat::krige(par~1, locations = locStn, newdata = newgrid, model = vgm, block = interp$block,
                            nmin = interp$pars$nmin, nmax = interp$pars$nmax, maxdist = interp$pars$maxdist,
                            debug.level = 0)
    out <- dataGrd
    out[igrid] <- grd_Par$var1.pred

    return(out)
}

interp_distrParams_idw <- function(interp, locStn, dataGrd, coarsefile){
    newgrid <- interp$grid
    igrid <- rep(TRUE, length(newgrid))
    if(interp$addcoarse){
        gridBuffer <- create_grid_buffer(locStn, interp$grid, interp$savecoarse, coarsefile)

        if(length(gridBuffer$coarse) > 0){
            coarse_interp <- gridBuffer$coarse
            coarse_interp$par <- dataGrd[gridBuffer$icoarse]
            row.names(locStn) <- 1:length(locStn)
            row.names(coarse_interp) <- length(locStn) + (1:length(coarse_interp))
            locStn <- rbind(sf::st_as_sf(locStn), sf::st_as_sf(coarse_interp))
            locStn <- sf::as_Spatial(locStn)
            locStn <- locStn[!is.na(locStn$par), ]
        }
        igrid <- gridBuffer$igrid
        newgrid <- interp$grid[igrid, ]
    }

    grd_Par <- gstat::krige(par~1, locations = locStn, newdata = newgrid, block = interp$block,
                            nmin = interp$pars$nmin, nmax = interp$pars$nmax, maxdist = interp$pars$maxdist,
                            debug.level = 0)
    out <- dataGrd
    out[igrid] <- grd_Par$var1.pred

    return(out)
}

