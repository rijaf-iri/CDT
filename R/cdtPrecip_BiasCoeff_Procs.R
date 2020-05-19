
execBiasRain <- function(){
    message <- .cdtData$GalParams[['message']]
    outdir <- file.path(.cdtData$GalParams$output$dir, paste0('BIAS_Data_',
                        tools::file_path_sans_ext(.cdtData$GalParams$STN.file)))
    dir.create(outdir, showWarnings = FALSE, recursive = TRUE)

    Insert.Messages.Out(message[['7']], TRUE, "i")

    #######get data
    stnData <- getStnOpenData(.cdtData$GalParams$STN.file)
    stnData <- getCDTdataAndDisplayMsg(stnData, .cdtData$GalParams$period,
                                       .cdtData$GalParams$STN.file)
    if(is.null(stnData)) return(NULL)

    ##################
    ## RFE sample file
    rfeDataInfo <- getNCDFSampleData(.cdtData$GalParams$RFE$sample)
    if(is.null(rfeDataInfo)){
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
        grd.lon <- rfeDataInfo$lon
        grd.lat <- rfeDataInfo$lat
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
        if(is.regridDEM)
            demData <- cdt.interp.surface.grid(demData, xy.grid)
        demData$z[demData$z < 0] <- 0
    }

    ##################
    ## Get RFE data info

    ncInfoBias <- ncInfo.no.date.range(.cdtData$GalParams$RFE,
                                       .cdtData$GalParams$period)
    if(is.null(ncInfoBias)){
        Insert.Messages.Out(message[['11']], TRUE, "e")
        return(NULL)
    }
    ncInfoBias$ncinfo <- rfeDataInfo

    ##################

    allyears <- .cdtData$GalParams$base.period$all.years
    year1 <- .cdtData$GalParams$base.period$start.year
    year2 <- .cdtData$GalParams$base.period$end.year
    minyear <- .cdtData$GalParams$base.period$min.year

    years.stn <- as.numeric(substr(stnData$dates, 1, 4))
    years.rfe <- as.numeric(substr(ncInfoBias$dates[ncInfoBias$exist], 1, 4))
    years <- intersect(years.stn, years.rfe)

    if(length(unique(years)) < minyear){
        Insert.Messages.Out(message[['12']], TRUE, 'e')
        return(NULL)
    }

    iyrUse <- if(allyears) rep(TRUE, length(years)) else years >= year1 & years <= year2
    ystn <- years.stn %in% years[iyrUse]
    yrfe <- years.rfe %in% years[iyrUse]

    stnData$data <- stnData$data[ystn, , drop = FALSE]
    stnData$dates <- stnData$dates[ystn]
    ncInfoBias$dates <- ncInfoBias$dates[yrfe]
    ncInfoBias$ncfiles <- ncInfoBias$ncfiles[yrfe]
    ncInfoBias$exist <- ncInfoBias$exist[yrfe]

    bias.pars <- ComputeBiasCoefficients(stnData = stnData, ncInfo = ncInfoBias,
                                         params = .cdtData$GalParams, variable = "rain",
                                         outdir = outdir)
    ret <- InterpolateBiasCoefficients(bias.pars, xy.grid, variable = "rain",
                                       outdir = outdir, demData = demData)
    if(!is.null(ret)){
        if(ret == 0) return(0)
        else return(ret)
    }else return(NULL)
}
