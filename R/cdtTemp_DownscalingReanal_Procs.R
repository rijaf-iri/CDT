
Temp_execDownscaling <- function(){
    message <- .cdtData$GalParams[['message']]
    daty <- get.range.date.time(.cdtData$GalParams$date.range,
                                .cdtData$GalParams$period)
    if(.cdtData$GalParams$period == 'monthly'){
        xdeb <- format(daty$start, "%b%Y")
        xfin <- format(daty$end, "%b%Y")
    }else{
        xdeb <- paste0(as.numeric(format(daty$start, "%d")), format(daty$start, "%b%Y"))
        xfin <- paste0(as.numeric(format(daty$start, "%d")), format(daty$end, "%b%Y"))
    }
    
    output.dir <- .cdtData$GalParams$output
    outdir <- file.path(output.dir$dir, paste('Downscaled_Reanalysis', xdeb, xfin, sep = '_'))
    dir.create(outdir, showWarnings = FALSE, recursive = TRUE)
    output.dir$dir <- outdir

    Insert.Messages.Out(message[['8']], TRUE, "i")

    ################
    CoefFile <- str_trim(.cdtData$GalParams$DownCoef.file)
    if(!file.exists(CoefFile)){
        Insert.Messages.Out(paste(CoefFile, message[['9']]), TRUE, 'e')
        return(NULL)
    }

    downCoef <- try(readRDS(CoefFile), silent = TRUE)
    if(inherits(downCoef, "try-error")){
        Insert.Messages.Out(message[['10']], TRUE, 'e')
        return(NULL)
    }

    ################
    ## Reanalysis sample file
    reanalInfo <- getNCDFSampleData(.cdtData$GalParams$REANAL$sample)
    if(is.null(reanalInfo)) return(NULL)

    ################
    ## get elevation data
    demInfo <- getNCDFSampleData(.cdtData$GalParams$DEM.file)
    if(is.null(demInfo)){
        Insert.Messages.Out(message[['11']], TRUE, 'e')
        return(NULL)
    }

    jfile <- getIndex.AllOpenFiles(.cdtData$GalParams$DEM.file)
    demData <- .cdtData$OpenFiles$Data[[jfile]][[2]]
    demData$lon <- demData$x
    demData$lat <- demData$y

    ###############
    ## create grid for interpolation
    if(.cdtData$GalParams$grid$from == "ncdf"){
        if(.cdtData$GalParams$DEM.file == .cdtData$GalParams$grid$ncfile){
            grd.lon <- demData$lon
            grd.lat <- demData$lat
        }else{
            grdInfo <- getNCDFSampleData(.cdtData$GalParams$grid$ncfile)
            if(is.null(grdInfo)){
                Insert.Messages.Out(message[['12']], TRUE, 'e')
                return(NULL)
            }
            grd.lon <- grdInfo$lon
            grd.lat <- grdInfo$lat
        }
    }
    if(.cdtData$GalParams$grid$from == "new"){
        grdInfo <- .cdtData$GalParams$grid$bbox
        grd.lon <- seq(grdInfo$minlon, grdInfo$maxlon, grdInfo$reslon)
        grd.lat <- seq(grdInfo$minlat, grdInfo$maxlat, grdInfo$reslat)
    }
    if(.cdtData$GalParams$grid$from == "data"){
        grd.lon <- reanalInfo$lon
        grd.lat <- reanalInfo$lat
    }

    nlon0 <- length(grd.lon)
    nlat0 <- length(grd.lat)
    xy.grid <- list(lon = grd.lon, lat = grd.lat)

    ## DEM data at new grid
    is.regridDEM <- is.diffSpatialPixelsObj(defSpatialPixels(xy.grid),
                                            defSpatialPixels(demData),
                                            tol = 1e-07)
    if(is.regridDEM){
        demGrid <- cdt.interp.surface.grid(demData, xy.grid)
        demGrid <- demGrid$z
    }else{
        demGrid <- demData$z
    }
    demGrid[demGrid < 0] <- 0

    ################

    ncInfo <- ncInfo.with.date.range(.cdtData$GalParams$REANAL,
                                     .cdtData$GalParams$date.range,
                                     .cdtData$GalParams$period)
    if(is.null(ncInfo)){
        Insert.Messages.Out(message[['13']], TRUE, "e")
        return(NULL)
    }
    ncInfo$ncinfo <- reanalInfo

    ################

    outncfiles <- ncOutput.Files(output.dir, .cdtData$GalParams$date.range, .cdtData$GalParams$period)

    ################
    .cdtData$GalParams$paramsDownscl <- list(demGrid = demGrid, downCoef = downCoef,
                                             reanalData = ncInfo, xy.grid = xy.grid,
                                             outncfiles = outncfiles)
    ret <- Temp_ReanalysisDownscaling()
    rm(demData, demGrid)
    gc()
    if(!is.null(ret)){
        if(ret == 0) return(0)
        else return(ret)
    }else return(NULL)
}

#################################################################################################

Temp_ReanalysisDownscaling <- function(){
    paramsDownscl <- .cdtData$GalParams$paramsDownscl

    ncInfos <- paramsDownscl$reanalData
    ncinfo <- ncInfos$ncinfo
    outncfiles <- paramsDownscl$outncfiles

    ###############

    lon.reanl <- ncinfo$lon
    lat.reanl <- ncinfo$lat
    nlon.r <- ncinfo$nx
    nlat.r <- ncinfo$ny
    xrnl <- lon.reanl[2] - lon.reanl[1]
    yrnl <- lat.reanl[2] - lat.reanl[1]
    res.max <- max(xrnl, yrnl)

    ###############

    demGrid <- paramsDownscl$demGrid
    xy.grid <- paramsDownscl$xy.grid

    ###############
    #Defines netcdf output dims
    dx <- ncdf4::ncdim_def("Lon", "degreeE", xy.grid$lon)
    dy <- ncdf4::ncdim_def("Lat", "degreeN", xy.grid$lat)
    ncoutTT <- ncdf4::ncvar_def("temp", "DegC", list(dx, dy), -99,
                                longname = "Dwonscaled temperature from reanalysis data",
                                prec = "float", compression = 9)

    ###############
    ## coeff
    downCoef <- paramsDownscl$downCoef

    ## elevation
    if(downCoef$fitting == "elevation") data.grid <- data.frame(z = c(demGrid))

    if(downCoef$standardize){
        data.grid0 <- as.matrix(data.grid)
        moy <- colMeans(data.grid0, na.rm = TRUE)
        sds <- matrixStats::colSds(data.grid0, na.rm = TRUE)
        data.grid <- sweep(sweep(data.grid, 2, moy, FUN = "-"), 2, sds, FUN = "/")
        rm(data.grid0, moy, sds)
    }

    ###############
    ## DEM at reanalysis grid

    dem.reanal <- list(lon = lon.reanl, lat = lat.reanl)
    dem.reanal$lon <- c(lon.reanl[1] - xrnl, lon.reanl, lon.reanl[nlon.r] + xrnl)
    dem.reanal$lat <- c(lat.reanl[1] - yrnl, lat.reanl, lat.reanl[nlat.r] + yrnl)

    ## add margin at the edge
    dem.reanal <- cdt.aggregate.grid(c(xy.grid, list(z = demGrid)), grid.list = dem.reanal, FUN = mean, na.rm = TRUE)
    dem.reanal <- dem.reanal$z[-c(1, nlon.r + 2), -c(1, nlat.r + 2)]

    ## elevation
    if(downCoef$fitting == "elevation") data.reanal <- data.frame(z = c(dem.reanal))

    if(downCoef$standardize){
        data.reanal0 <- as.matrix(data.reanal)
        moy <- colMeans(data.reanal0, na.rm = TRUE)
        sds <- matrixStats::colSds(data.reanal0, na.rm = TRUE)
        data.reanal <- sweep(sweep(data.reanal, 2, moy, FUN = "-"), 2, sds, FUN = "/")
        rm(data.reanal0, moy, sds)
    }

    ##############
    ## create interpolation grid
    interp.method <- .cdtData$GalParams$interp$method

    if(interp.method %in% c("idw", "okr")){
        nmin <- .cdtData$GalParams$interp$nmin
        nmax <- .cdtData$GalParams$interp$nmax
        minstn <- .cdtData$GalParams$interp$minstn
        vgm.model <- .cdtData$GalParams$interp$vgm.model
        maxdist <- .cdtData$GalParams$interp$maxdist
        maxdist <- if(maxdist < res.max) sqrt(2) * res.max else maxdist

        interp.grid <- defSpatialPixels(xy.grid)

        bGrd <- NULL
        if(.cdtData$GalParams$interp$use.block) 
            bGrd <- createBlock(interp.grid@grid@cellsize, 1, 5)

        locations.reanl <- expand.grid(lon = lon.reanl, lat = lat.reanl)
        coordinates(locations.reanl) <- ~lon+lat
    }

    ###############

    Insert.Messages.Out(.cdtData$GalParams[['message']][['14']], TRUE, "i")

    parsL <- doparallel.cond(length(which(ncInfos$exist)) >= 30)
    ret <- cdt.foreach(seq_along(ncInfos$ncfiles), parsL, GUI = TRUE,
                       progress = TRUE, FUN = function(jj)
    {
         if(ncInfos$exist[jj]){
            nc <- ncdf4::nc_open(ncInfos$ncfiles[jj])
            reanlTT <- ncdf4::ncvar_get(nc, varid = ncinfo$varid)
            ncdf4::nc_close(nc)
            reanlTT <- transposeNCDFData(reanlTT, ncinfo)
        }else return(NULL)

        date.reanl <- ncInfos$dates[[jj]]
        mon <- as.numeric(substr(date.reanl, 5, 6))

        if(downCoef$standardize){
            meanTT <- downCoef$model[[mon]]$std.pars$mean['v']
            sdTT <- downCoef$model[[mon]]$std.pars$sd['v']
            reanlTT <- (reanlTT - meanTT) / sdTT
        }

        resid <- reanlTT - predict(downCoef$model[[mon]], newdata = data.reanal)

        ############
        if(interp.method == 'blin'){
            resid[is.na(resid)] <- 0
            residObj <- list(lon = lon.reanl, lat = lat.reanl, z = resid)
            residInterp <- cdt.interp.surface.grid(residObj, xy.grid)
            residInterp <- residInterp$z
        }else{
            locations.reanl$res <- c(resid)
            locations.reanl0 <- locations.reanl[!is.na(locations.reanl$res), ]
            if(length(locations.reanl0$res) < 2) return(NULL)

            vgm <- NULL
            if(interp.method == "okr"){
                if(length(locations.reanl0$res) > minstn){
                    exp_var <- gstat::variogram(res~1, locations = locations.reanl0, cressie = TRUE)
                    vgm <- try(gstat::fit.variogram(exp_var, gstat::vgm(vgm.model)), silent = TRUE)
                    if(!inherits(vgm, "try-error")) vgm <- NULL
                }
            }

            grd.temp <- gstat::krige(res~1, locations = locations.reanl0, newdata = interp.grid, model = vgm,
                                    block = bGrd, nmin = nmin, nmax = nmax, maxdist = maxdist, debug.level = 0)
            residInterp <- grd.temp$var1.pred
            dim(residInterp) <- sapply(xy.grid, length)
        }

        ############
        residInterp[is.na(residInterp)] <- 0
        downTT <- predict(downCoef$model[[mon]], newdata = data.grid) + residInterp
        if(downCoef$standardize) downTT <- downTT * sdTT + meanTT
        downTT[is.na(downTT)] <- -99

        ############
        nc <- ncdf4::nc_create(outncfiles[jj], ncoutTT)
        ncdf4::ncvar_put(nc, ncoutTT, downTT)
        ncdf4::nc_close(nc)
        rm(residInterp, downTT, resid, reanlTT)
    })

    Insert.Messages.Out(.cdtData$GalParams[['message']][['15']], TRUE, "s")
    rm(demGrid, interp.grid)
    gc()
    return(0)
}
