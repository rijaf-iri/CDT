
execAdjBiasRain <- function(){
    Insert.Messages.Out('Adjusting Gauge-RFE bias ...', TRUE, "i")

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
                         paste('ADJUSTED_Precip_Data', xdeb, xfin, sep = '_'))
    dir.create(outdir, showWarnings = FALSE, recursive = TRUE)

    ##################
    ## RFE sample file
    rfeDataInfo <- getNCDFSampleData(.cdtData$GalParams$RFE$sample)
    if(is.null(rfeDataInfo)){
        Insert.Messages.Out("No RFE data sample found", format = TRUE)
        return(NULL)
    }

    ##################

    ncInfo <- ncInfo.with.date.range(.cdtData$GalParams$RFE,
                                     .cdtData$GalParams$date.range,
                                     .cdtData$GalParams$period)
    if(is.null(ncInfo)){
        Insert.Messages.Out("RFE data not found", TRUE, "e")
        return(NULL)
    }
    ncInfo$ncinfo <- rfeDataInfo

    ##################
    ## READ BIAS FILSES

    BIAS <- readBiasFiles(params = .cdtData$GalParams, variable = "rain")
    if(is.null(BIAS)) return(NULL)

    bbox1 <- sapply(rfeDataInfo[c("lon", "lat")], range)
    bbox2 <- sapply(BIAS[c("lon", "lat")], range)
    btx <- max(bbox1[1, 1], bbox2[1, 1])
    bty <- max(bbox1[1, 2], bbox2[1, 2])
    upx <- min(bbox1[2, 1], bbox2[2, 1])
    upy <- min(bbox1[2, 2], bbox2[2, 2])

    if(btx >= upx | bty >= upy){
        Insert.Messages.Out("Bias and the precipitation data are in the different region", TRUE, "e")
        return(NULL)
    }

    diff.grid <- is.diffSpatialPixelsObj(defSpatialPixels(BIAS[c("lon", "lat")]),
                                         defSpatialPixels(rfeDataInfo[c("lon", "lat")]),
                                         tol = 1e-03)
    if(diff.grid){
        Insert.Messages.Out("Bias and the precipitation data have different grid", TRUE, "e")
        return(NULL)
    }

    ret <- applyBiasCorrection(BIAS, ncInfo, outdir, params = .cdtData$GalParams, variable = "rain")

    if(!is.null(ret)){
        if(ret == 0) return(0)
        else return(ret)
    }else return(NULL)
}
