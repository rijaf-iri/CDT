
biasCorrectionWind <- function(){
    message <- .cdtData$GalParams[['message']]
    Insert.Messages.Out(message[['7']], TRUE, "i")

    is_wind_speed <- .cdtData$GalParams$wvar == 'speed'
    uv_one_netcdf <- .cdtData$GalParams$one.ncdf

    daty <- seq.format.date.time(.cdtData$GalParams$period,
                                 .cdtData$GalParams$date.range)
    dtrg <- merged_date_range_filename(daty, .cdtData$GalParams$period)

    varClim <- if(is_wind_speed) 'WSPD' else 'WIND'
    dirADJClim <- paste('ADJUSTED', varClim, 'Data', dtrg$start, dtrg$end, sep = '_')
    outdir <- file.path(.cdtData$GalParams$output$dir, dirADJClim)
    dir.create(outdir, showWarnings = FALSE, recursive = TRUE)

    ##################
    ## READ BIAS FILSES

    if(is_wind_speed){
        coefBias <- readBiasCoefficientFiles(.cdtData$GalParams, GUI = TRUE)
    }else{
        coefBias <- readBiasCoefficientFiles_wind(.cdtData$GalParams, GUI = TRUE)
    }
    if(is.null(coefBias)) return(NULL)

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
    ## Get NetCDF data info

    if(is_wind_speed){
        ncInfo <- ncInfo.with.date.range(.cdtData$GalParams$INPUT.S,
                                         .cdtData$GalParams$date.range,
                                         .cdtData$GalParams$period,
                                         .cdtData$GalParams$minhour)
        if(is.null(ncInfo)){
            Insert.Messages.Out(message[['9-1']], TRUE, "e")
            return(NULL)
        }
    }else{
        if(uv_one_netcdf){
            ncInfoUV <- ncInfo.with.date.range(.cdtData$GalParams$INPUT.UV,
                                               .cdtData$GalParams$date.range,
                                               .cdtData$GalParams$period,
                                               .cdtData$GalParams$minhour)
            if(is.null(ncInfoUV)){
                Insert.Messages.Out(message[['9-2']], TRUE, "e")
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
                Insert.Messages.Out(message[['9-3']], TRUE, "e")
                return(NULL)
            }
            ncInfoV <- ncInfo.with.date.range(.cdtData$GalParams$INPUT.V,
                                              .cdtData$GalParams$date.range,
                                              .cdtData$GalParams$period,
                                              .cdtData$GalParams$minhour)
            if(is.null(ncInfoV)){
                Insert.Messages.Out(message[['9-4']], TRUE, "e")
                return(NULL)
            }

            nc_daty <- intersect(ncInfoU$dates[ncInfoU$exist], ncInfoV$dates[ncInfoV$exist])
            if(length(nc_daty) == 0){
                Insert.Messages.Out(message[['9-5']], TRUE, 'e')
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

    bbox1 <- sapply(ncDataInfo[c("lon", "lat")], range)
    bbox2 <- sapply(coefBias$coords[c("glon", "glat")], range)
    btx <- max(bbox1[1, 1], bbox2[1, 1])
    bty <- max(bbox1[1, 2], bbox2[1, 2])
    upx <- min(bbox1[2, 1], bbox2[2, 1])
    upy <- min(bbox1[2, 2], bbox2[2, 2])

    if(btx >= upx | bty >= upy){
        Insert.Messages.Out(message[['10']], TRUE, "e")
        return(NULL)
    }

    if(is_wind_speed){
        ret <- applyBiasCorrectionClimData(coefBias, ncInfo, outdir, 
                                           params = .cdtData$GalParams,
                                           variable = "wspd", GUI = TRUE)
    }else{
        ret <- applyBiasCorrectionWind(coefBias, ncInfo, outdir, 
                                       params = .cdtData$GalParams,
                                       GUI = TRUE)
    }

    if(!is.null(ret)){
        if(ret == 0) return(0)
        else return(ret)
    }else return(NULL)
}
