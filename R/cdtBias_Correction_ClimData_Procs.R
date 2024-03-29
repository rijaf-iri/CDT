
biasCorrectionClimData <- function(){
    message <- .cdtData$GalParams[['message']]
    Insert.Messages.Out(message[['7']], TRUE, "i")

    daty <- get.range.date.time(.cdtData$GalParams$date.range,
                                .cdtData$GalParams$period)
    if(.cdtData$GalParams$period == 'monthly'){
        xdeb <- format(daty$start, "%b%Y")
        xfin <- format(daty$end, "%b%Y")
    }else{
        xdeb <- paste0(as.numeric(format(daty$start, "%d")), format(daty$start, "%b%Y"))
        xfin <- paste0(as.numeric(format(daty$end, "%d")), format(daty$end, "%b%Y"))
    }

    varClim <- gsub("rmbias\\.", "", .cdtData$GalParams$action)

    dirADJClim <- paste('ADJUSTED', toupper(varClim), 'Data', xdeb, xfin, sep = '_')
    outdir <- file.path(.cdtData$GalParams$output$dir, dirADJClim)
    dir.create(outdir, showWarnings = FALSE, recursive = TRUE)

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
    ## READ BIAS FILSES

    BIAS <- readBiasFiles(params = .cdtData$GalParams, variable = varClim)
    if(is.null(BIAS)) return(NULL)

    bbox1 <- sapply(ncDataInfo[c("lon", "lat")], range)
    bbox2 <- sapply(BIAS[c("lon", "lat")], range)
    btx <- max(bbox1[1, 1], bbox2[1, 1])
    bty <- max(bbox1[1, 2], bbox2[1, 2])
    upx <- min(bbox1[2, 1], bbox2[2, 1])
    upy <- min(bbox1[2, 2], bbox2[2, 2])

    if(btx >= upx | bty >= upy){
        Insert.Messages.Out(message[['10']], TRUE, "e")
        return(NULL)
    }

    # diff.grid <- is.diffSpatialPixelsObj(defSpatialPixels(BIAS[c("lon", "lat")]),
    #                                      defSpatialPixels(ncDataInfo[c("lon", "lat")]),
    #                                      tol = 1e-03)
    # if(diff.grid){
    #     Insert.Messages.Out(message[['11']], TRUE, "e")
    #     return(NULL)
    # }

    ret <- applyBiasCorrection(BIAS, ncInfo, outdir,
                               params = .cdtData$GalParams,
                               variable = varClim)

    if(!is.null(ret)){
        if(ret == 0) return(0)
        else return(ret)
    }else return(NULL)
}
