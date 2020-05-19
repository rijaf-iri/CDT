
execCrossValidRain <- function(){
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

    outdir <- file.path(.cdtData$GalParams$outdir,
                         paste('CrossValidation_Precip_Data', xdeb, xfin, sep = '_'))
    dir.create(outdir, showWarnings = FALSE, recursive = TRUE)

    ##################
    ### get data
    stnData <- getStnOpenData(.cdtData$GalParams$STN.file)
    stnData <- getCDTdataAndDisplayMsg(stnData, .cdtData$GalParams$period,
                                       .cdtData$GalParams$STN.file)
    if(is.null(stnData)) return(NULL)

    ##################
    ## RFE sample file
    rfeDataInfo <- getNCDFSampleData(.cdtData$GalParams$RFE$sample)
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

    grd.lon <- rfeDataInfo$lon
    grd.lat <- rfeDataInfo$lat
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

    ncInfo <- ncInfo.with.date.range(.cdtData$GalParams$RFE,
                                     .cdtData$GalParams$date.range,
                                     .cdtData$GalParams$period)
    if(is.null(ncInfo)){
        Insert.Messages.Out(message[['14']], TRUE, "e")
        return(NULL)
    }
    ncInfo$ncinfo <- rfeDataInfo

    ##################
    ## options user provide list of station

    df <- as.data.frame(stnData[c("id", 'lon', 'lat')])
    df$nb <- colSums(!is.na(stnData$data[stnData$dates %in% ncInfo$dates, , drop = FALSE]))

    df <- df[df$nb > 0, , drop = FALSE]
    if(nrow(df) == 0){
        Insert.Messages.Out(message[['15']], TRUE, "e")
        return(NULL)
    }

    ## at least 50%
    df <- df[df$nb / length(ncInfo$dates) >= 0.5, , drop = FALSE]
    if(nrow(df) == 0){
        Insert.Messages.Out(message[['16']], TRUE, "e")
        return(NULL)
    }

    ## stn.valid <- as.character(df$id)

    # stn.valid <- read.table('/Users/rijaf/Desktop/work/UNMA/PRECIP/Validation/valid_stations.csv', sep = ',', header = TRUE)
    stn.valid <- select.Station.Validation(df, perc = 20)
    stn.valid <- as.character(stn.valid$id)

    stn.valid <- which(stnData$id %in% stn.valid)

    ##################

    ret <- cdtMergingLOOCV(stnData = stnData, stnVID = stn.valid, ncInfo = ncInfo, xy.grid = xy.grid, 
                           params = .cdtData$GalParams, variable = "rain", demData = demData,
                           outdir = outdir)

    if(!is.null(ret)){
        if(ret != 0){
          Insert.Messages.Out(paste(message[['17']],
                              file.path(outdir, "log_file.txt")), TRUE, "w")  
        }
    }else return(NULL)

    Insert.Messages.Out(message[['18']], TRUE, "s")
    return(0)
}
