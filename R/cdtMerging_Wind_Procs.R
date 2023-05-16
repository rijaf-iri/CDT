
mergingWind <- function(){
    message <- .cdtData$GalParams[['message']]
    Insert.Messages.Out(message[['10']], TRUE, "i")

    daty <- get.range.date.time(.cdtData$GalParams$date.range,
                                .cdtData$GalParams$period)
    if(.cdtData$GalParams$period == 'monthly'){
        xdeb <- format(daty$start, "%b%Y")
        xfin <- format(daty$end, "%b%Y")
    }else{
        xdeb <- paste0(as.numeric(format(daty$start, "%d")), format(daty$start, "%b%Y"))
        xfin <- paste0(as.numeric(format(daty$end, "%d")), format(daty$end, "%b%Y"))
    }

    outdir <- file.path(.cdtData$GalParams$output$dir,
                        paste('MERGED_Wind_Data', xdeb, xfin, sep = '_'))
    dir.create(outdir, showWarnings = FALSE, recursive = TRUE)

    ##################

    Insert.Messages.Out(message[['11']], TRUE, "i")

    #######get data
    stnData <- getStnOpenData(.cdtData$GalParams$STN.file)
    stnData <- getCDTdataAndDisplayMsg(stnData, .cdtData$GalParams$period,
                                       .cdtData$GalParams$STN.file)
    if(is.null(stnData)) return(NULL)

    ##################
    ## RFE sample file
    rfeDataInfo <- getNCDFSampleData(.cdtData$GalParams$INPUT$sample)
    if(is.null(rfeDataInfo)){
        Insert.Messages.Out(message[['12']], TRUE, 'e')
        return(NULL)
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
            Insert.Messages.Out(message[['13']], TRUE, "e")
            return(NULL)
        }
        jfile <- getIndex.AllOpenFiles(.cdtData$GalParams$auxvar$demfile)
        demData <- .cdtData$OpenFiles$Data[[jfile]][[2]]
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
            Insert.Messages.Out(message[['14']], TRUE, "e")
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
    ## Get RFE data info

    ncInfo <- ncInfo.with.date.range(.cdtData$GalParams$INPUT,
                                     .cdtData$GalParams$date.range,
                                     .cdtData$GalParams$period)
    if(is.null(ncInfo)){
        Insert.Messages.Out(message[['15']], TRUE, "e")
        return(NULL)
    }
    ncInfo$ncinfo <- rfeDataInfo

    ##################
    ## blanking
    outMask <- NULL

    if(.cdtData$GalParams$blank$data){
        shpd <- getShpOpenData(.cdtData$GalParams$blank$shpf)[[2]]
        outMask <- create.mask.grid(shpd, xy.grid)
    }

    Insert.Messages.Out(message[['16']], TRUE, "s")

    ##################

    Insert.Messages.Out(message[['17']], TRUE, "i")

    ret <- cdtMerging(stnData = stnData, ncInfo = ncInfo, xy.grid = xy.grid, params = .cdtData$GalParams,
                      variable = "rain", demData = demData, outdir = outdir, mask = outMask)

    if(!is.null(ret)){
        if(ret != 0){
          Insert.Messages.Out(paste(message[['18']],
                              file.path(outdir, "log_file.txt")), TRUE, "w")
        }
    }else return(NULL)

    Insert.Messages.Out(message[['19']], TRUE, "s")
    return(0)
}
