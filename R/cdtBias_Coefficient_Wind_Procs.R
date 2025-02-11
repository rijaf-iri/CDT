
computeBiasCoeffWind <- function(){
    message <- .cdtData$GalParams[['message']]

    is_wind_speed <- .cdtData$GalParams$wvar == 'speed'
    uv_one_netcdf <- .cdtData$GalParams$one.ncdf

    if(is_wind_speed){
        dirBias <- 'BIAS_DATA_WSPD'
    }else{
        dirBias <- 'BIAS_DATA_WIND'
    }
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

    #######
    ## get station data
    if(is_wind_speed){
        stnData <- getStnOpenData(.cdtData$GalParams$STN.S)
        stnData <- getCDTdataAndDisplayMsg(stnData, .cdtData$GalParams$period,
                                           .cdtData$GalParams$STN.S)
        if(is.null(stnData)) return(NULL)
    }else{
        stnU <- getStnOpenData(.cdtData$GalParams$STN.U)
        stnU <- getCDTdataAndDisplayMsg(stnU, .cdtData$GalParams$period,
                                        .cdtData$GalParams$STN.U)
        if(is.null(stnU)){
            Insert.Messages.Out(message[['13']], TRUE, 'e')
            return(NULL)
        }
        stnV <- getStnOpenData(.cdtData$GalParams$STN.V)
        stnV <- getCDTdataAndDisplayMsg(stnV, .cdtData$GalParams$period,
                                        .cdtData$GalParams$STN.V)
        if(is.null(stnV)){
            Insert.Messages.Out(message[['14']], TRUE, 'e')
            return(NULL)
        }

        if(length(stnU$id) != length(stnV$id)){
            Insert.Messages.Out(message[['15']], TRUE, 'e')
            return(NULL)
        }
        if(any(stnU$id != stnV$id)){
            Insert.Messages.Out(message[['15']], TRUE, 'e')
            return(NULL)
        }

        stn_daty <- intersect(stnU$dates, stnV$dates)
        if(length(stn_daty) == 0){
            Insert.Messages.Out(message[['16']], TRUE, 'e')
            return(NULL)
        }

        stnData <- stnU[c('id', 'lon', 'lat', 'elv')]
        stnData$dates <- stn_daty
        stnData$U <- stnU$data[match(stn_daty, stnU$dates), , drop = FALSE]
        stnData$V <- stnV$data[match(stn_daty, stnV$dates), , drop = FALSE]
    }

    ##################
    ## NetCDF sample file

    if(is_wind_speed){
        ncDataInfo <- getNCDFSampleData(.cdtData$GalParams$INPUT.S$sample)
        if(is.null(ncDataInfo)){
            Insert.Messages.Out(message[['8-1']], TRUE, 'e')
            return(NULL)
        }
    }else{
        if(uv_one_netcdf){
            ncUVInfo <- getNCDFSampleData(.cdtData$GalParams$INPUT.UV$sample)
            if(is.null(ncUVInfo)){
                Insert.Messages.Out(message[['8-2']], TRUE, 'e')
                return(NULL)
            }

            ncDataInfo <- NULL
            ncDataInfo$lon <- ncUVInfo$lon
            ncDataInfo$lat <- ncUVInfo$lat
            ncDataInfo$UV <- ncUVInfo[!names(ncUVInfo) %in% c('lon', 'lat')]
            ncDataInfo$varidU <- .cdtData$GalParams$INPUT.UV$U
            ncDataInfo$varidV <- .cdtData$GalParams$INPUT.UV$V
        }else{
            ncUInfo <- getNCDFSampleData(.cdtData$GalParams$INPUT.U$sample)
            if(is.null(ncUInfo)){
                Insert.Messages.Out(message[['8-3']], TRUE, 'e')
                return(NULL)
            }
            ncVInfo <- getNCDFSampleData(.cdtData$GalParams$INPUT.V$sample)
            if(is.null(ncVInfo)){
                Insert.Messages.Out(message[['8-4']], TRUE, 'e')
                return(NULL)
            }

            diff_grid <- is.diffSpatialPixelsObj(defSpatialPixels(ncUInfo[c('lon', 'lat')]),
                                                 defSpatialPixels(ncVInfo[c('lon', 'lat')]),
                                                 tol = 1e-07)
            if(diff_grid){
                Insert.Messages.Out(message[['8-5']], TRUE, 'e')
                return(NULL)
            }

            ncDataInfo <- NULL
            ncDataInfo$lon <- ncUInfo$lon
            ncDataInfo$lat <- ncUInfo$lat
            ncDataInfo$U <- ncUInfo[!names(ncUInfo) %in% c('lon', 'lat')]
            ncDataInfo$V <- ncVInfo[!names(ncVInfo) %in% c('lon', 'lat')]
            ncDataInfo$varidU <- ncUInfo$varinfo$name
            ncDataInfo$varidV <- ncVInfo$varinfo$name
        }
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
        if(is.regridDEM)
            demData <- cdt.interp.surface.grid(demData, xy.grid)
        demData$z[demData$z < 0] <- 0
    }

    ##################
    ## Get NetCDF data info

    if(is_wind_speed){
        ncInfoBias <- ncInfo.no.date.range(.cdtData$GalParams$INPUT.S,
                                           .cdtData$GalParams$period)
        if(is.null(ncInfoBias)){
            Insert.Messages.Out(message[['11-1']], TRUE, "e")
            return(NULL)
        }
    }else{
        if(uv_one_netcdf){
            ncInfoUV <- ncInfo.no.date.range(.cdtData$GalParams$INPUT.UV,
                                             .cdtData$GalParams$period)
            if(is.null(ncInfoUV)){
                Insert.Messages.Out(message[['11-2']], TRUE, "e")
                return(NULL)
            }
            ncInfoBias <- NULL
            ncInfoBias$UV <- ncInfoUV
        }else{
            ncInfoU <- ncInfo.no.date.range(.cdtData$GalParams$INPUT.U,
                                            .cdtData$GalParams$period)
            if(is.null(ncInfoU)){
                Insert.Messages.Out(message[['11-3']], TRUE, "e")
                return(NULL)
            }
            ncInfoV <- ncInfo.no.date.range(.cdtData$GalParams$INPUT.V,
                                            .cdtData$GalParams$period)
            if(is.null(ncInfoV)){
                Insert.Messages.Out(message[['11-4']], TRUE, "e")
                return(NULL)
            }

            nc_daty <- intersect(ncInfoU$dates[ncInfoU$exist], ncInfoV$dates[ncInfoV$exist])
            if(length(nc_daty) == 0){
                Insert.Messages.Out(message[['17']], TRUE, 'e')
                return(NULL)
            }

            ncInfoBias <- NULL
            itU <- match(nc_daty, ncInfoU$dates)
            ncInfoBias$U <- lapply(ncInfoU, function(x) x[itU])
            itV <- match(nc_daty, ncInfoV$dates)
            ncInfoBias$V <- lapply(ncInfoV, function(x) x[itV])
        }
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
    if(is_wind_speed){
        years.nc <- as.numeric(substr(ncInfoBias$dates[ncInfoBias$exist], 1, 4))
    }else{
        if(uv_one_netcdf){
            years.nc <- as.numeric(substr(ncInfoBias$UV$dates[ncInfoBias$UV$exist], 1, 4))
        }else{
            years.nc <- as.numeric(substr(ncInfoBias$U$dates[ncInfoBias$U$exist], 1, 4))
        }  
    }
    years <- intersect(years.stn, years.nc)

    if(length(unique(years)) < minyear){
        Insert.Messages.Out(message[['12']], TRUE, 'e')
        return(NULL)
    }

    iyrUse <- if(allyears) rep(TRUE, length(years)) else years >= year1 & years <= year2
    ystn <- years.stn %in% years[iyrUse]
    ync <- years.nc %in% years[iyrUse]

    if(is_wind_speed){
        stnData$data <- stnData$data[ystn, , drop = FALSE]
        stnData$dates <- stnData$dates[ystn]

        ncInfoBias$dates <- ncInfoBias$dates[ync]
        ncInfoBias$ncfiles <- ncInfoBias$ncfiles[ync]
        ncInfoBias$exist <- ncInfoBias$exist[ync]
    }else{
        stnData$U <- stnData$U[ystn, , drop = FALSE]
        stnData$V <- stnData$V[ystn, , drop = FALSE]
        stnData$dates <- stnData$dates[ystn]
        if(uv_one_netcdf){
            ncInfoBias$UV$dates <- ncInfoBias$UV$dates[ync]
            ncInfoBias$UV$ncfiles <- ncInfoBias$UV$ncfiles[ync]
            ncInfoBias$UV$exist <- ncInfoBias$UV$exist[ync]
        }else{
            ncInfoBias$U$dates <- ncInfoBias$U$dates[ync]
            ncInfoBias$U$ncfiles <- ncInfoBias$U$ncfiles[ync]
            ncInfoBias$U$exist <- ncInfoBias$U$exist[ync]

            ncInfoBias$V$dates <- ncInfoBias$V$dates[ync]
            ncInfoBias$V$ncfiles <- ncInfoBias$V$ncfiles[ync]
            ncInfoBias$V$exist <- ncInfoBias$V$exist[ync]
        }
    }

    ##################

    if(is_wind_speed){
        ret <- cdtBiasCoefficients(stnData = stnData, ncInfo = ncInfoBias, demData = demData,
                                   params = .cdtData$GalParams, variable = "wspd", outdir = outdir)
    }else{
        ret <- cdtBiasCoefficientsWind(stnData = stnData, ncInfo = ncInfoBias, demData = demData,
                                       params = .cdtData$GalParams, outdir = outdir)
    }

    ret <- 0
    if(!is.null(ret)){
        if(ret == 0) return(0)
        else return(ret)
    }else return(NULL)
}
