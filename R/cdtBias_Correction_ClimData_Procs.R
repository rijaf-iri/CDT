
biasCorrectionClimData <- function(){
    message <- .cdtData$GalParams[['message']]
    Insert.Messages.Out(message[['7']], TRUE, "i")

    ##################

    varClim <- gsub("rmbias\\.", "", .cdtData$GalParams$action)
    daty <- seq.format.date.time(.cdtData$GalParams$period,
                                 .cdtData$GalParams$date.range)
    dtrg <- merged_date_range_filename(daty, .cdtData$GalParams$period)
    dirADJClim <- paste('ADJUSTED', toupper(varClim), 'Data', dtrg$start, dtrg$end, sep = '_')
    outdir <- file.path(.cdtData$GalParams$output$dir, dirADJClim)
    dir.create(outdir, showWarnings = FALSE, recursive = TRUE)

    ##################
    ## READ BIAS FILSES

    coefBias <- readBiasCoefficientFiles(.cdtData$GalParams, GUI = TRUE)
    if(is.null(coefBias)) return(NULL)

    ##################
    ## NetCDF sample file
    ncDataInfo <- getNCDFSampleData(.cdtData$GalParams$INPUT$sample)
    if(is.null(ncDataInfo)){
        Insert.Messages.Out(message[['8']], TRUE, 'e')
        return(NULL)
    }

    ##################
    ncInfo <- ncInfo.with.date.range(.cdtData$GalParams$INPUT,
                                     .cdtData$GalParams$date.range,
                                     .cdtData$GalParams$period)
    if(is.null(ncInfo)){
        Insert.Messages.Out(message[['9']], TRUE, "e")
        return(NULL)
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

    ret <- applyBiasCorrectionClimData(coefBias, ncInfo, outdir, 
                                       params = .cdtData$GalParams,
                                       variable = varClim, GUI = TRUE)

    if(!is.null(ret)){
        if(ret == 0) return(0)
        else return(ret)
    }else return(NULL)
}
