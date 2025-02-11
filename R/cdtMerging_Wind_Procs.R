
mergingWindData <- function(){
    message <- .cdtData$GalParams[['message']]
    Insert.Messages.Out(message[['16']], TRUE, "i")

    is_wind_speed <- .cdtData$GalParams$wvar == 'speed'
    uv_one_netcdf <- .cdtData$GalParams$one.ncdf

    daty <- seq.format.date.time(.cdtData$GalParams$period,
                                 .cdtData$GalParams$date.range,
                                 .cdtData$GalParams$minhour)
    dtrg <- merged_date_range_filename(daty, .cdtData$GalParams$period)

    dirMRGClim <- paste('MERGED_WIND_Data', dtrg$start, dtrg$end, sep = '_')
    outdir <- file.path(.cdtData$GalParams$output$dir, dirMRGClim)

    if(is_wind_speed){
        dir.create(file.path(outdir, 'DATA'), showWarnings = FALSE, recursive = TRUE)
    }else{
        if(uv_one_netcdf){
            dir.create(file.path(outdir, 'DATA'), showWarnings = FALSE, recursive = TRUE)
        }else{
            dir.create(file.path(outdir, 'UGRD'), showWarnings = FALSE, recursive = TRUE)
            dir.create(file.path(outdir, 'VGRD'), showWarnings = FALSE, recursive = TRUE)
        }
    }

    mrgOpts <- merging.options()

    if(mrgOpts$saveGridBuffer)
        dir.create(file.path(outdir, "GRID_BUFFER"), showWarnings = FALSE, recursive = TRUE)

    if(.cdtData$GalParams$interp$method == "okr"){
        .cdtData$GalParams$interp$vgm_save <- TRUE
        if(is_wind_speed){
            .cdtData$GalParams$interp$vgm_dir <- "VARIOGRAM"
            dir.create(file.path(outdir, "VARIOGRAM"), showWarnings = FALSE, recursive = TRUE)
        }else{
            dir.create(file.path(outdir, "VARIOGRAM_U"), showWarnings = FALSE, recursive = TRUE)
            dir.create(file.path(outdir, "VARIOGRAM_V"), showWarnings = FALSE, recursive = TRUE)
        }
    }

    ##################

    out_params <- .cdtData$GalParams
    out_params <- out_params[!names(out_params) %in% "message"]
    if(is_wind_speed){
        out_params[c('STN.U', 'STN.V', 'one.ncdf', 'INPUT.UV', 'INPUT.U', 'INPUT.V')] <- NULL
        out_params$output$format.UV <- NULL
        out_params$output$format.U <- NULL
        out_params$output$format.V <- NULL
    }else{
        if(uv_one_netcdf){
            out_params[c('STN.S', 'INPUT.S', 'INPUT.U', 'INPUT.V')] <- NULL
            out_params$output$format.U <- NULL
            out_params$output$format.V <- NULL
        }else{
            out_params[c('STN.S', 'INPUT.S', 'INPUT.UV')] <- NULL
            out_params$output$format.UV <- NULL
        }
        out_params$output$format.S <- NULL
    }

    out_params$merging_options <- mrgOpts
    out_params$blanking_options <- blanking.options()
    saveRDS(out_params, file.path(outdir, 'merging_parameters.rds'))

    ##################

    Insert.Messages.Out(message[['17']], TRUE, "i")

    ## station data
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
            Insert.Messages.Out(message[['26']], TRUE, 'e')
            return(NULL)
        }
        stnV <- getStnOpenData(.cdtData$GalParams$STN.V)
        stnV <- getCDTdataAndDisplayMsg(stnV, .cdtData$GalParams$period,
                                        .cdtData$GalParams$STN.V)
        if(is.null(stnV)){
            Insert.Messages.Out(message[['27']], TRUE, 'e')
            return(NULL)
        }

        if(length(stnU$id) != length(stnV$id)){
            Insert.Messages.Out(message[['28']], TRUE, 'e')
            return(NULL)
        }
        if(!all(stnU$id == stnV$id)){
            Insert.Messages.Out(message[['28']], TRUE, 'e')
            return(NULL)
        }

        stn_daty <- intersect(stnU$dates, stnV$dates)
        if(length(stn_daty) == 0){
            Insert.Messages.Out(message[['29']], TRUE, 'e')
            return(NULL)
        }

        stnData <- stnU[c('id', 'lon', 'lat', 'elv')]
        stnData$date <- stn_daty
        stnData$U <- stnU$data[match(stn_daty, stnU$dates), , drop = FALSE]
        stnData$V <- stnV$data[match(stn_daty, stnV$dates), , drop = FALSE]
    }

    ##################
    ## NetCDF sample file
    if(is_wind_speed){
        ncDataInfo <- getNCDFSampleData(.cdtData$GalParams$INPUT.S$sample)
        if(is.null(ncDataInfo)){
            Insert.Messages.Out(message[['18-1']], TRUE, 'e')
            return(NULL)
        }
    }else{
        if(uv_one_netcdf){
            ncUVInfo <- getNCDFSampleData(.cdtData$GalParams$INPUT.UV$sample)
            if(is.null(ncUVInfo)){
                Insert.Messages.Out(message[['18-2']], TRUE, 'e')
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
                Insert.Messages.Out(message[['18-3']], TRUE, 'e')
                return(NULL)
            }
            ncVInfo <- getNCDFSampleData(.cdtData$GalParams$INPUT.V$sample)
            if(is.null(ncVInfo)){
                Insert.Messages.Out(message[['18-4']], TRUE, 'e')
                return(NULL)
            }

            diff_grid <- is.diffSpatialPixelsObj(defSpatialPixels(ncUInfo[c('lon', 'lat')]),
                                                 defSpatialPixels(ncVInfo[c('lon', 'lat')]),
                                                 tol = 1e-07)
            if(diff_grid){
                Insert.Messages.Out(message[['30']], TRUE, 'e')
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
    if(.cdtData$GalParams$MRG$method == "RK" &
       (.cdtData$GalParams$auxvar$dem |
        .cdtData$GalParams$auxvar$slope |
        .cdtData$GalParams$auxvar$aspect)
      )
    {
        demInfo <- getNCDFSampleData(.cdtData$GalParams$auxvar$demfile)
        if(is.null(demInfo)){
            Insert.Messages.Out(message[['19']], TRUE, "e")
            return(NULL)
        }
        jfile <- getIndex.AllOpenFiles(.cdtData$GalParams$auxvar$demfile)
        demData <- .cdtData$OpenFiles$Data[[jfile]][[2]]
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
            Insert.Messages.Out(message[['20']], TRUE, "e")
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

    if(is_wind_speed){
        ncInfo <- ncInfo.with.date.range(.cdtData$GalParams$INPUT.S,
                                         .cdtData$GalParams$date.range,
                                         .cdtData$GalParams$period,
                                         .cdtData$GalParams$minhour)
        if(is.null(ncInfo)){
            Insert.Messages.Out(message[['21-1']], TRUE, "e")
            return(NULL)
        }
    }else{
        if(uv_one_netcdf){
            ncInfoUV <- ncInfo.with.date.range(.cdtData$GalParams$INPUT.UV,
                                             .cdtData$GalParams$date.range,
                                             .cdtData$GalParams$period,
                                             .cdtData$GalParams$minhour)
            if(is.null(ncInfoUV)){
                Insert.Messages.Out(message[['21-2']], TRUE, "e")
                return(NULL)
            }
            ncInfo <- NULL
            ncInfo$UV <- ncInfoUV
        }else{
            ncInfoU <- ncInfo.with.date.range(.cdtData$GalParams$INPUT.U,
                                              .cdtData$GalParams$date.range,
                                              .cdtData$GalParams$period,
                                              .cdtData$GalParams$minhour)
            if(is.null(ncInfoU)){
                Insert.Messages.Out(message[['21-3']], TRUE, "e")
                return(NULL)
            }
            ncInfoV <- ncInfo.with.date.range(.cdtData$GalParams$INPUT.V,
                                              .cdtData$GalParams$date.range,
                                              .cdtData$GalParams$period,
                                              .cdtData$GalParams$minhour)
            if(is.null(ncInfoV)){
                Insert.Messages.Out(message[['21-4']], TRUE, "e")
                return(NULL)
            }

            nc_daty <- intersect(ncInfoU$dates[ncInfoU$exist], ncInfoV$dates[ncInfoV$exist])
            if(length(nc_daty) == 0){
                Insert.Messages.Out(message[['29']], TRUE, 'e')
                return(NULL)
            }

            ncInfo <- NULL
            itU <- match(nc_daty, ncInfoU$dates)
            ncInfo$U <- lapply(ncInfoU, function(x) x[itU])
            itV <- match(nc_daty, ncInfoV$dates)
            ncInfo$V <- lapply(ncInfoV, function(x) x[itV])
        }
    }

    ncInfo$ncinfo <- ncDataInfo

    ##################
    ## blanking
    outMask <- NULL

    if(.cdtData$GalParams$blank$data){
        shpd <- getShpOpenData(.cdtData$GalParams$blank$shpf)[[2]]
        outMask <- create.mask.grid(shpd, xy.grid)
    }

    Insert.Messages.Out(message[['22']], TRUE, "s")

    ##################

    Insert.Messages.Out(message[['23']], TRUE, "i")

    .cdtData$GalParams$RnoR <- list(use = FALSE, wet = 1.0, smooth = FALSE)

    if(is_wind_speed){
        params <- .cdtData$GalParams
        params$output$format <- params$output$format.S
        ret <- cdtMerging(stnData = stnData, ncInfo = ncInfo, xy.grid = xy.grid, params = params,
                          variable = "wspd", demData = demData, outdir = outdir, mask = outMask)
    }else{
        ret <- cdtMergingWind(stnData = stnData, ncInfo = ncInfo, xy.grid = xy.grid,
                              params = .cdtData$GalParams, demData = demData, 
                              outdir = outdir, mask = outMask)
    }

    if(!is.null(ret)){
        if(ret != 0){
          file_log <- file.path(outdir, "log_file.txt")
          Insert.Messages.Out(paste(message[['24']], file_log), TRUE, "w")
        }
    }else return(NULL)

    Insert.Messages.Out(message[['25']], TRUE, "s")
    return(0)
}
