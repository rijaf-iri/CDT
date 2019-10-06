
execCrossValidTEMP <- function(){
    Insert.Messages.Out('Start cross-validation ...', TRUE, "i")

    daty <- get.range.date.time(.cdtData$GalParams$date.range,
                                .cdtData$GalParams$period)
    if(.cdtData$GalParams$period == 'monthly'){
        xdeb <- format(daty$start, "%b%Y")
        xfin <- format(daty$end, "%b%Y")
    }else{
        xdeb <- paste0(as.numeric(format(daty$start, "%d")), format(daty$start, "%b%Y"))
        xfin <- paste0(as.numeric(format(daty$start, "%d")), format(daty$end, "%b%Y"))
    }

    outdir <- file.path(.cdtData$GalParams$outdir,
                         paste('CrossValidation_Temp_Data', xdeb, xfin, sep = '_'))
    dir.create(outdir, showWarnings = FALSE, recursive = TRUE)

    ##################
    ### get data
    stnData <- getStnOpenData(.cdtData$GalParams$STN.file)
    stnData <- getCDTdataAndDisplayMsg(stnData, .cdtData$GalParams$period,
                                       .cdtData$GalParams$STN.file)
    if(is.null(stnData)) return(NULL)

    ##################
    ## Temp sample file
    tmpDataInfo <- getNCDFSampleData(.cdtData$GalParams$TEMP$sample)
    if(is.null(tmpDataInfo)){
        Insert.Messages.Out("No temperature data sample found", format = TRUE)
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
            Insert.Messages.Out("No elevation data found", TRUE, "e")
            return(NULL)
        }
        jfile <- getIndex.AllOpenFiles(.cdtData$GalParams$auxvar$demfile)
        demData <- .cdtData$OpenFiles$Data[[jfile]][[2]]
    }

    ##################
    ##Create grid for interpolation

    grd.lon <- tmpDataInfo$lon
    grd.lat <- tmpDataInfo$lat
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
    ## Get Temp data info

    ncInfo <- ncInfo.with.date.range(.cdtData$GalParams$TEMP,
                                     .cdtData$GalParams$date.range,
                                     .cdtData$GalParams$period)
    if(is.null(ncInfo)){
        Insert.Messages.Out("Temperature data not found", TRUE, "e")
        return(NULL)
    }
    ncInfo$ncinfo <- tmpDataInfo

    ##################

    df <- as.data.frame(stnData[c("id", 'lon', 'lat')])
    df$nb <- colSums(!is.na(stnData$data))
    stn.valid <- select.Station.Validation(df, perc = 20)
    stn.valid <- as.character(stn.valid$id)
    stn.valid <- which(stnData$id %in% stn.valid)

    ##################

    ret <- cdtMergingLOOCV(stnData = stnData, stnVID = stn.valid, ncInfo = ncInfo, xy.grid = xy.grid, 
                           params = .cdtData$GalParams, variable = "temp", demData = demData,
                           outdir = outdir)

    if(!is.null(ret)){
        if(ret != 0){
          Insert.Messages.Out(paste('Unable to process some files, see',
                              file.path(outdir, "log_file.txt"), 'for details'), TRUE, "w")  
        }
    }else return(NULL)

    Insert.Messages.Out('Cross-Validation done', TRUE, "s")
    return(0)
}
