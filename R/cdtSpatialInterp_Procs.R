
readStnDataInterp <- function(GeneralParameters){
    intstep <- GeneralParameters$intstep
    stnFile <- GeneralParameters$cdtstation

    inputInfo <- GeneralParameters[c('intstep', 'minhour', 'cdtstation', 'date.range')]

    readstnData <- TRUE
    if(!is.null(.cdtData$EnvData$inputInfo))
        if(isTRUE(all.equal(.cdtData$EnvData$inputInfo, inputInfo)))
            readstnData <- FALSE

    if(readstnData){
        stnInfo <- getStnOpenDataInfo(stnFile)
        if(!is.null(.cdtData$EnvData$stnInfo)){
            if(!isTRUE(all.equal(.cdtData$EnvData$stnInfo, stnInfo))){
                readstnData1 <- TRUE
                .cdtData$EnvData$stnData1 <- NULL
            }else readstnData1 <- FALSE
        }else readstnData1 <- TRUE

        if(readstnData1 || readstnData){
            don <- getStnOpenData(stnFile)
            if(is.null(don)) return(NULL)

            if(intstep == "others"){
                don <- splitCDTData1(don)
            }else{
                don <- getCDTdataAndDisplayMsg(don, intstep, stnFile)
                if(is.null(don)) return(NULL)
                don <- don[c('id', 'lon', 'lat', 'dates', 'elv', 'data')]
            }

            .cdtData$EnvData$stnData <- don
            .cdtData$EnvData$stnInfo <- stnInfo
        }
        .cdtData$EnvData$inputInfo <- inputInfo
    }

    return(0)
}

######################

interpStationsProcs <- function(GeneralParameters, GUI = TRUE){
    message <- .cdtData$EnvData[['message']]
    intstep <- GeneralParameters$intstep
    minhour <- GeneralParameters$minhour
    date.range <- GeneralParameters$date.range
    stnFile <- GeneralParameters$cdtstation

    outdir <- file.path(GeneralParameters$outdir, paste0('INTEPORLATION_',
                        tools::file_path_sans_ext(stnFile)))
    dir.create(outdir, showWarnings = FALSE, recursive = TRUE)
    ncdfOUT <- file.path(outdir, 'DATA_NetCDF')
    dir.create(ncdfOUT, showWarnings = FALSE, recursive = TRUE)
    cdtOUT <- file.path(outdir, 'CDTDATASET')
    dir.create(cdtOUT, showWarnings = FALSE, recursive = TRUE)

    .cdtData$EnvData$ncdfOUT <- ncdfOUT

    #####################

    ret <- readStnDataInterp(GeneralParameters)
    if(is.null(ret)) return(NULL)
    don <- .cdtData$EnvData$stnData
    if(is.null(don)) return(NULL)

    #####################
    if(intstep != "others"){
        rdates <- get.range.date.time(date.range, intstep, minhour)
        if(intstep == "minute"){
            dates <- as.POSIXct(don$dates, format = "%Y%m%d%H%M")
        }else if(intstep == "hourly"){
            dates <- as.POSIXct(don$dates, format = "%Y%m%d%H")
        }else if(intstep == "monthly"){
            dates <- as.Date(paste0(don$dates, "01"), "%Y%m%d")
        }else{
            dates <- as.Date(don$dates, "%Y%m%d")
        }

        idaty <- dates >= rdates[1] & dates <= rdates[2]
        if(!any(idaty)){
            Insert.Messages.Out(message[['11']], TRUE, "e")
            return(NULL)
        }

        don$dates <- don$dates[idaty]
        don$data <- don$data[idaty, , drop = FALSE]
        dates <- dates[idaty]
        .cdtData$EnvData$first.date <- dates[1]
        .cdtData$EnvData$last.date <- dates[length(dates)]
    }

    .cdtData$EnvData$stnData <- don
    
    outRDS <- list(stn = don, params = GeneralParameters,
                   first.date = .cdtData$EnvData$first.date,
                   last.date = .cdtData$EnvData$last.date)
    saveRDS(outRDS, file = file.path(outdir, 'SpatialInterpolation.rds'))

    #####################
    if(GeneralParameters$grid$from == "new"){
        grdInfo <- GeneralParameters$grid$bbox
        grd.lon <- seq(grdInfo$minlon, grdInfo$maxlon, grdInfo$reslon)
        grd.lat <- seq(grdInfo$minlat, grdInfo$maxlat, grdInfo$reslat)
    }

    if(GeneralParameters$grid$from == "ncdf"){
        grdInfo <- getNCDFSampleData(GeneralParameters$grid$ncfile)
        if(is.null(grdInfo)){
            Insert.Messages.Out(message[['12']], TRUE, "e")
            return(NULL)
        }
        grd.lon <- grdInfo$lon
        grd.lat <- grdInfo$lat
    }

    nlon <- length(grd.lon)
    nlat <- length(grd.lat)

    #####################

    miss.val <- -9999

    dx <- ncdf4::ncdim_def("Lon", "degree_east", grd.lon)
    dy <- ncdf4::ncdim_def("Lat", "degree_north", grd.lat)
    grd.nc.out <- ncdf4::ncvar_def("var", "", list(dx, dy), miss.val,
                            longname = paste("Interpolated data from", stnFile),
                            prec = "float", compression = 9)

    #####################

    xy.grid <- list(lon = grd.lon, lat = grd.lat)
    newgrid <- defSpatialPixels(xy.grid)
    locations.stn <- as.data.frame(don[c('lon', 'lat')])
    sp::coordinates(locations.stn) <- c('lon', 'lat')
    ijs <- sp::over(locations.stn, newgrid)
    locations.stn$stn <- rep(NA, length(locations.stn))

    #####################
    interp <- GeneralParameters$interp
    nmin <- interp$nmin
    nmax <- interp$nmax
    maxdist <- interp$maxdist
    negVal <- GeneralParameters$negative
    blankGrid <- GeneralParameters$blank

    if(interp$method %in% c("idw", "okr", "ukr")){
        bGrd <- NULL
        if(interp$use.block)
            bGrd <- createBlock(c(1, 1, 1, 1))
    }

    #####################

    if(interp$method %in% c("ukr", "nn3d")){
        demData <- getNcdfOpenData(interp$demfile)
        if(is.null(demData)){
            Insert.Messages.Out(message[['13']], TRUE, "e")
            return(NULL)
        }

        demData <- demData[[2]][c('x', 'y', 'z')]
        names(demData) <- c('lon', 'lat', 'z')
        demData <- cdt.interp.surface.grid(demData, xy.grid)
    }

    #####################

    if(blankGrid$blank){
        shpdata <- getShpOpenData(blankGrid$shpf)[[2]]
        if(is.null(shpdata)){
            Insert.Messages.Out(message[['14']], TRUE, "e")
            return(NULL)
        }
        maskGrid <- create.mask.grid(shpdata, xy.grid)
    }

    #####################

    is.auxvar <- rep(FALSE, 5)
    if(interp$method == "ukr"){
        auxvar <- c('dem', 'slp', 'asp', 'alon', 'alat')
        is.auxvar <- unlist(interp$auxvar[1:5])
        formuleUK <- stats::formula(paste0('stn', '~', paste(auxvar[is.auxvar], collapse = '+')))

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

    #####################

    if(interp$method == "nn3d"){
        newgrid <- data.frame(sp::coordinates(newgrid), elv = c(demData$z))
        locations.stn <- data.frame(sp::coordinates(locations.stn), elv = demData$z[ijs])
        locations.stn <- locations.stn[!is.na(locations.stn$elv), , drop = FALSE]
        if(nrow(locations.stn) == 0){
            Insert.Messages.Out(message[['15']], TRUE, "e")
            return(NULL)
        }

        ixs <- cdt.nn.index.all(newgrid [, 1:2, drop = FALSE],
                                locations.stn[, 1:2, drop = FALSE],
                                maxdist)

        newgrid <- scale(newgrid)
        newgrid <- as.data.frame(newgrid)
        locations.stn <- scale(locations.stn)
        locations.stn <- as.data.frame(locations.stn)
    }

    #####################

    parsL <- doparallel.cond(length(don$dates) > 30)

    ret <- cdt.foreach(seq_along(don$dates), parsL, GUI,
                       progress = TRUE, FUN = function(jj)
    {
        vgm <- NULL

        locations.stn$stn <- as.numeric(don$data[jj, ])
        locations.stn <- locations.stn[!is.na(locations.stn$stn), ]
        nstn <- length(locations.stn)
        if(nstn == 0){
            msg <- list(msg = message[['16']], status = NULL)
            return(msg)
        }

        if(interp$method == "ukr" & any(is.auxvar)){
            loc.data <- !is.na(locations.stn@data)
            loc.data <- split(loc.data, col(loc.data))
            nna <- Reduce("&", loc.data)
            locations.stn <- locations.stn[nna, ]
            nstn <- length(locations.stn)
            if(nstn == 0){
                msg <- list(msg = message[['16']], status = NULL)
                return(msg)
            }
        }

        ######
        if(interp$method == "okr"){
            if(nstn < interp$minstn){
                msg <- list(msg = message[['17']], status = NULL)
                return(msg)
            }
            if(stats::var(locations.stn$stn) < 1e-15){
                msg <- list(msg = paste(message[['18']], ':', 'variance null'), status = NULL)
                return(msg)
            }

            exp.var <- gstat::variogram(stn~1, locations = locations.stn, cressie = TRUE)

            ######
            kappa <- c(0.05, seq(0.2, 2, 0.1), 5, 10)
            nugget0 <- min(exp.var$gamma)
            range0 <- 0.5 * max(exp.var$dist)
            sill0 <- mean(c(max(exp.var$gamma), stats::median(exp.var$gamma)))
            psill0 <- sill0 - nugget0

            vgm_list <- lapply(interp$vgm.model, function(vg){
                # vm <- gstat::vgm(vg, psill = psill0, range = range0,
                #                  nugget = nugget0, kappa = 0.5)
                # try(gstat::fit.variogram(exp.var, vm, fit.kappa = TRUE), silent = TRUE)

                vm <- gstat::vgm(vg, psill = psill0, range = range0, nugget = nugget0)
                try(gstat::fit.variogram(exp.var, vm, fit.kappa = kappa), silent = TRUE)
            })
            names(vgm_list) <- interp$vgm.model
            vgm_error <- sapply(vgm_list, function(v) inherits(v, "try-error"))
            if(all(vgm_error)){
                msg <- sapply(vgm_list, function(v) gsub('[\r\n]', '', as.character(v)))
                msg <- paste0(msg, collapse = '\n')
                msg <- paste(message[['18']], ":", msg)
                msg <- list(msg = msg, status = NULL)
                return(msg)
            }
            vgm_list <- vgm_list[!vgm_error]
            vgm_range <- sapply(vgm_list, function(v) if(v$range[2] <= 0) FALSE else TRUE)
            if(!any(vgm_range)){
                msg1 <- "All variogram model,"
                msg2 <- 'variogram range can never be negative'
                msg <- paste(msg1, msg2)
                msg <- paste(message[['18']], ":", msg)
                msg <- list(msg = msg, status = NULL)
                return(msg)
            }
            class(vgm_list) <- c("variogramModelList", "list")
            vgm <- try(gstat::fit.variogram(exp.var, vgm_list, fit.kappa = TRUE), silent = TRUE)

            ######
            # vgm <- try(gstat::fit.variogram(exp.var, gstat::vgm(interp$vgm.model)), silent = TRUE)

            if(inherits(vgm, "try-error")){
                msg <- paste(message[['18']], ":", gsub('[\r\n]', '', as.character(vgm)))
                msg <- list(msg = msg, status = NULL)
                return(msg)
            }

            # vgm <- try(automap::autofitVariogram(stn~1, input_data = locations.stn, model = interp$vgm.model, cressie = TRUE), silent = TRUE)
            # if(inherits(vgm, "try-error")){
            #     msg <- paste(message[['18']], ":", gsub('[\r\n]', '', as.character(vgm)))
            #     msg <- list(msg = msg, status = NULL)
            #     return(msg)
            # }else{
            #     vgm <- vgm$var_model
            # }
        }

        if(interp$method == "ukr"){
            if(nstn < interp$minstn){
                msg <- list(msg = message[['17']], status = NULL)
                return(msg)
            }

            vars <- matrixStats::colVars(as.matrix(locations.stn@data))
            if(all(vars < 1e-15)){
                msg <- list(msg = paste(message[['18']], ':', 'variance null'), status = NULL)
                return(msg)
            }

            exp.var <- gstat::variogram(formuleUK, locations = locations.stn, cressie = TRUE)

            ######
            kappa <- c(0.05, seq(0.2, 2, 0.1), 5, 10)
            nugget0 <- min(exp.var$gamma)
            range0 <- 0.5 * max(exp.var$dist)
            sill0 <- mean(c(max(exp.var$gamma), stats::median(exp.var$gamma)))
            psill0 <- sill0 - nugget0

            vgm_list <- lapply(interp$vgm.model, function(vg){
                # vm <- gstat::vgm(vg, psill = psill0, range = range0,
                #                  nugget = nugget0, kappa = 0.5)
                # try(gstat::fit.variogram(exp.var, vm, fit.kappa = TRUE), silent = TRUE)

                vm <- gstat::vgm(vg, psill = psill0, range = range0, nugget = nugget0)
                try(gstat::fit.variogram(exp.var, vm, fit.kappa = kappa), silent = TRUE)
            })
            names(vgm_list) <- interp$vgm.model
            vgm_error <- sapply(vgm_list, function(v) inherits(v, "try-error"))
            if(all(vgm_error)){
                msg <- sapply(vgm_list, function(v) gsub('[\r\n]', '', as.character(v)))
                msg <- paste0(msg, collapse = '\n')
                msg <- paste(message[['18']], ":", msg)
                msg <- list(msg = msg, status = NULL)
                return(msg)
            }
            vgm_list <- vgm_list[!vgm_error]
            vgm_range <- sapply(vgm_list, function(v) if(v$range[2] <= 0) FALSE else TRUE)
            if(!any(vgm_range)){
                msg1 <- "All variogram model,"
                msg2 <- 'variogram range can never be negative'
                msg <- paste(msg1, msg2)
                msg <- paste(message[['18']], ":", msg)
                msg <- list(msg = msg, status = NULL)
                return(msg)
            }
            class(vgm_list) <- c("variogramModelList", "list")
            vgm <- try(gstat::fit.variogram(exp.var, vgm_list, fit.kappa = TRUE), silent = TRUE)

            ######
            # vgm <- try(gstat::fit.variogram(exp.var, gstat::vgm(interp$vgm.model)), silent = TRUE)

            if(inherits(vgm, "try-error")){
                msg <- paste(message[['18']], ":", gsub('[\r\n]', '', as.character(vgm)))
                msg <- list(msg = msg, status = NULL)
                return(msg)
            }

            # vgm <- try(automap::autofitVariogram(formuleUK, input_data = locations.stn, model = interp$vgm.model, cressie = TRUE), silent = TRUE)
            # if(inherits(vgm, "try-error")){
            #     msg <- paste(message[['18']], ":", gsub('[\r\n]', '', as.character(vgm)))
            #     msg <- list(msg = msg, status = NULL)
            #     return(msg)
            # }else{
            #     vgm <- vgm$var_model
            # }
        }

        #######
        ## add options for the power
        if(interp$method %in% c("idw", "okr")){
            interp.stn <- gstat::krige(stn ~ 1, locations = locations.stn, newdata = newgrid, model = vgm,
                                       block = bGrd, nmin = nmin, nmax = nmax, maxdist = maxdist, debug.level = 0)
            interp.stn <- interp.stn$var1.pred
        }
        if(interp$method == "ukr"){
            interp.stn <- gstat::krige(formuleUK, locations = locations.stn, newdata = newgrid, model = vgm,
                                       block = bGrd, nmin = nmin, nmax = nmax, debug.level = 0)
            interp.stn <- interp.stn$var1.pred
        }
        if(interp$method == "nns"){
            interp.stn <- gstat::krige(stn ~ 1, locations = locations.stn, newdata = newgrid,
                                   nmax = 1, maxdist = maxdist, debug.level = 0)
            interp.stn <- interp.stn$var1.pred
        }
        if(interp$method == "nn3d"){
            interp.stn <- gstat::krige(stn ~ 1, locations = ~lon+lat+elv, data = locations.stn,
                                       newdata = newgrid, nmax = 1, debug.level = 0)
            interp.stn <- interp.stn$var1.pred
            interp.stn[!ixs] <- NA
        }
        if(interp$method == "shepard"){
            interp.stn <- shepard.interp(locations.stn@coords, locations.stn$stn, newgrid@coords, nmin, nmax, FALSE, p = 0.7)
            interp.stn <- interp.stn[, 3]
        }
        if(interp$method == "sphere"){
            interp.stn <- spheremap.interp(locations.stn@coords, locations.stn$stn, newgrid@coords, nmin, nmax, FALSE)
            interp.stn <- interp.stn[, 3]
        }

        #######
        out <- matrix(interp.stn, nlon, nlat)
        if(negVal$set) out[out < 0] <- negVal$value
        if(blankGrid$blank) out <- out * maskGrid
        out[is.na(out)] <- miss.val

        ncfile <- paste0("stn_interp_", don$dates[jj], ".nc")
        out.nc.file <- file.path(ncdfOUT, ncfile)
        nc <- ncdf4::nc_create(out.nc.file, grd.nc.out)
        ncdf4::ncvar_put(nc, grd.nc.out, out)
        ncdf4::nc_close(nc)

        return(list(msg = "OK", status = 0, vgm = vgm))
    })

    #####################

    if(interp$method %in% c("okr", "ukr")){
        vgm <- lapply(ret, '[[', 'vgm')
        names(vgm) <- don$dates
        saveRDS(vgm, file = file.path(cdtOUT, "Fitted_variogram.rds"))
    }

    ierror <- sapply(ret, function(x) is.null(x$status))
    if(any(ierror)){
        msg <- lapply(ret, '[[', 'msg')[ierror]
        names(msg) <- don$dates[ierror]
        saveRDS(msg, file = file.path(cdtOUT, "Error_log.rds"))

        error.mgs <- paste(names(msg), do.call(c, msg))
        xx <- paste(error.mgs, collapse = "\n")

        if(GUI){
            containertab <- Display_Output_Console_Tab(xx, "Interp-Errors", cat)
            ntab <- update.OpenTabs('ctxt', containertab)
            tkselect(.cdtEnv$tcl$main$tknotes, ntab)
        }else{
            cat(xx, "\n")
        }
        return(1)
    }

    return(0)
}
