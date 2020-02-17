
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

        if(readstnData1){
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
            Insert.Messages.Out("Station data do not overlap to the selected date range", TRUE, "e")
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
            Insert.Messages.Out("Unable to read the NetCDF file to create the interpolation grid", TRUE, "e")
            return(NULL)
        }
        grd.lon <- grdInfo$lon
        grd.lat <- grdInfo$lat
    }

    nlon <- length(grd.lon)
    nlat <- length(grd.lat)

    #####################

    miss.val <- -9999

    dx <- ncdim_def("Lon", "degree_east", grd.lon)
    dy <- ncdim_def("Lat", "degree_north", grd.lat)
    grd.nc.out <- ncvar_def("var", "", list(dx, dy), miss.val,
                            longname = paste("Interpolated data from", stnFile),
                            prec = "float", compression = 9)

    #####################

    xy.grid <- list(lon = grd.lon, lat = grd.lat)
    newgrid <- defSpatialPixels(xy.grid)
    locations.stn <- as.data.frame(don[c('lon', 'lat')])
    coordinates(locations.stn) <- c('lon', 'lat')
    ijs <- over(locations.stn, newgrid)
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
            bGrd <- createBlock(newgrid@grid@cellsize, 1, 5)
    }

    #####################

    if(interp$method %in% c("ukr", "nn3d")){
        demData <- getNcdfOpenData(interp$demfile)
        if(is.null(demData)){
            Insert.Messages.Out("Unable to read the elevation data", TRUE, "e")
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
            Insert.Messages.Out("No shapefiles found", TRUE, "e")
            return(NULL)
        }
        maskGrid <- create.mask.grid(shpdata, xy.grid)
    }

    #####################

    is.auxvar <- rep(FALSE, 5)
    if(interp$method == "ukr"){
        auxvar <- c('dem', 'slp', 'asp', 'alon', 'alat')
        is.auxvar <- unlist(interp$auxvar[1:5])
        formuleUK <- formula(paste0('stn', '~', paste(auxvar[is.auxvar], collapse = '+')))

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
        newgrid <- data.frame(coordinates(newgrid), elv = c(demData$z))
        locations.stn <- data.frame(coordinates(locations.stn), elv = demData$z[ijs])
        locations.stn <- locations.stn[!is.na(locations.stn$elv), , drop = FALSE]
        if(nrow(locations.stn) == 0){
            Insert.Messages.Out("All elevation data at station location are missing", TRUE, "e")
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
                       progress = TRUE, .packages = "sp",
                       FUN = function(jj)
    {
        vgm <- NULL

        locations.stn$stn <- as.numeric(don$data[jj, ])
        locations.stn <- locations.stn[!is.na(locations.stn$stn), ]
        nstn <- length(locations.stn)
        if(nstn == 0){
            msg <- list(msg = "No data to interpolate", status = NULL)
            return(msg)
        }

        if(interp$method == "ukr" & any(is.auxvar)){
            loc.data <- !is.na(locations.stn@data)
            loc.data <- split(loc.data, col(loc.data))
            nna <- Reduce("&", loc.data)
            locations.stn <- locations.stn[nna, ]
            nstn <- length(locations.stn)
            if(nstn == 0){
                msg <- list(msg = "No data to interpolate", status = NULL)
                return(msg)
            }
        }

        ######
        if(interp$method == "okr"){
            if(nstn < interp$minstn){
                msg <- list(msg = "No enough data to compute variogram", status = NULL)
                return(msg)
            }
            if(var(locations.stn$stn) < 1e-15){
                msg <- list(msg = "Unable to compute variogram: variance null", status = NULL)
                return(msg)
            }

            exp.var <- gstat::variogram(stn~1, locations = locations.stn, cressie = TRUE)
            vgm <- try(gstat::fit.variogram(exp.var, gstat::vgm(interp$vgm.model)), silent = TRUE)
            if(inherits(vgm, "try-error")){
                msg <- paste("Unable to compute variogram:", gsub('[\r\n]', '', as.character(vgm)))
                msg <- list(msg = msg, status = NULL)
                return(msg)
            }
        }

        if(interp$method == "ukr"){
            if(nstn < interp$minstn){
                msg <- list(msg = "No enough data to compute variogram", status = NULL)
                return(msg)
            }

            vars <- matrixStats::colVars(as.matrix(locations.stn@data))
            if(all(vars < 1e-15)){
                msg <- list(msg = "Unable to compute variogram: variance null", status = NULL)
                return(msg)
            }

            exp.var <- gstat::variogram(formuleUK, locations = locations.stn, cressie = TRUE)
            vgm <- try(gstat::fit.variogram(exp.var, gstat::vgm(interp$vgm.model)), silent = TRUE)
            if(inherits(vgm, "try-error")){
                msg <- paste("Unable to compute variogram:", gsub('[\r\n]', '', as.character(vgm)))
                msg <- list(msg = msg, status = NULL)
                return(msg)
            }
        }

        #######
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
            interp.stn <- shepard.interp(locations.stn@coords, locations.stn$stn, newgrid@coords, nmin, nmax, FALSE, p = 2)
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
