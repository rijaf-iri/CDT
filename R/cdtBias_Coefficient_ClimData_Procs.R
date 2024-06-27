
computeBiasCoeffClimData <- function(){
    message <- .cdtData$GalParams[['message']]

    varClim <- gsub("coefbias\\.", "", .cdtData$GalParams$action)
    dirBias <- paste0('BIAS_DATA_', toupper(varClim))
    outdir <- file.path(.cdtData$GalParams$output$dir, dirBias)
    if(dir.exists(outdir)){
        tmp <- list.files(.cdtData$GalParams$output$dir, paste0('^', dirBias, '_[0-9]+'), include.dirs = TRUE)
        if(length(tmp) == 0){
            tmp <- 1
        }else{
            tmp <- gsub(paste0(dirBias, "_"), "", tmp)
            tmp <- max(as.numeric(tmp)) + 1
        }
        outdir <- file.path(.cdtData$GalParams$output$dir, paste0(dirBias, "_", tmp))
    }
    dir.create(outdir, showWarnings = FALSE, recursive = TRUE)

    Insert.Messages.Out(message[['7']], TRUE, "i")

    ##################
    ## station data
    stnData <- getStnOpenData(.cdtData$GalParams$STN.file)
    stnData <- getCDTdataAndDisplayMsg(stnData, .cdtData$GalParams$period,
                                       .cdtData$GalParams$STN.file)
    if(is.null(stnData)) return(NULL)

    ##################
    ## NetCDF sample file
    ncDataInfo <- getNCDFSampleData(.cdtData$GalParams$INPUT$sample)
    if(is.null(ncDataInfo)){
        Insert.Messages.Out(message[['8']], TRUE, 'e')
        return(NULL)
    }

    ##################
    ## DEM data
    demData <- NULL
    if(.cdtData$GalParams$interp$method == "nn3d"){
        demInfo <- getNCDFSampleData(.cdtData$GalParams$interp$demfile)
        if(is.null(demInfo)){
            Insert.Messages.Out(message[['9']], TRUE, "e")
            return(NULL)
        }
        jfile <- getIndex.AllOpenFiles(.cdtData$GalParams$interp$demfile)
        demData <- .cdtData$OpenFiles$Data[[jfile]][[2]]
        demData$z[demData$z < 0] <- 0
    }

    ##################
    ##Create grid for interpolation

    if(.cdtData$GalParams$grid$from == "data"){
        grd.lon <- ncDataInfo$lon
        grd.lat <- ncDataInfo$lat
    }

    if(.cdtData$GalParams$grid$from == "new"){
        grdInfo <- .cdtData$GalParams$grid$bbox
        grd.lon <- seq(grdInfo$minlon, grdInfo$maxlon, grdInfo$reslon)
        grd.lat <- seq(grdInfo$minlat, grdInfo$maxlat, grdInfo$reslat)
    }

    if(.cdtData$GalParams$grid$from == "ncdf"){
        grdInfo <- getNCDFSampleData(.cdtData$GalParams$grid$ncfile)
        if(is.null(grdInfo)){
            Insert.Messages.Out(message[['10']], TRUE, "e")
            return(NULL)
        }
        grd.lon <- grdInfo$lon
        grd.lat <- grdInfo$lat
    }

    xy.grid <- list(lon = grd.lon, lat = grd.lat)

    ##################
    ## regrid DEM data

    if(!is.null(demData)){
        demData$lon <- demData$x
        demData$lat <- demData$y
        is.regridDEM <- is.diffSpatialPixelsObj(defSpatialPixels(xy.grid),
                                                defSpatialPixels(demData),
                                                tol = 1e-07)
        if(is.regridDEM){
            demData <- cdt.interp.surface.grid(demData, xy.grid)
        }else demData <- demData[c('x', 'y', 'z')]
        demData$z[demData$z < 0] <- 0
    }

    ##################
    ## Get NetCDF data info

    ncInfoBias <- ncInfo.no.date.range(.cdtData$GalParams$INPUT,
                                       .cdtData$GalParams$period)
    if(is.null(ncInfoBias)){
        Insert.Messages.Out(message[['11']], TRUE, "e")
        return(NULL)
    }
    ncInfoBias$ncinfo <- ncDataInfo

    ##################
    ## Regrid NetCDF data

    is.regridNC <- is.diffSpatialPixelsObj(defSpatialPixels(xy.grid),
                                           defSpatialPixels(ncDataInfo),
                                           tol = 1e-07)
    ncInfoBias$ncgrid <- c(list(regrid = is.regridNC), xy.grid,
                           list(nlon = length(xy.grid$lon),
                                nlat = length(xy.grid$lat)))

    ##################

    allyears <- .cdtData$GalParams$base.period$all.years
    year1 <- .cdtData$GalParams$base.period$start.year
    year2 <- .cdtData$GalParams$base.period$end.year
    minyear <- .cdtData$GalParams$base.period$min.year

    years.stn <- as.numeric(substr(stnData$dates, 1, 4))
    years.nc <- as.numeric(substr(ncInfoBias$dates[ncInfoBias$exist], 1, 4))
    years <- intersect(years.stn, years.nc)

    if(length(unique(years)) < minyear){
        Insert.Messages.Out(message[['12']], TRUE, 'e')
        return(NULL)
    }

    iyrUse <- if(allyears) rep(TRUE, length(years)) else years >= year1 & years <= year2
    ystn <- years.stn %in% years[iyrUse]
    ync <- years.nc %in% years[iyrUse]

    stnData$data <- stnData$data[ystn, , drop = FALSE]
    stnData$dates <- stnData$dates[ystn]
    ncInfoBias$dates <- ncInfoBias$dates[ync]
    ncInfoBias$ncfiles <- ncInfoBias$ncfiles[ync]
    ncInfoBias$exist <- ncInfoBias$exist[ync]

    ##################

    ret <- cdtBiasCoefficients(stnData = stnData, ncInfo = ncInfoBias, demData = demData,
                               params = .cdtData$GalParams, variable = varClim, outdir = outdir)

    if(!is.null(ret)){
        if(ret == 0) return(0)
        else return(ret)
    }else return(NULL)
}
