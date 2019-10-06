
execAjdBiasDownTemp <- function(){
    Insert.Messages.Out('Adjustment of downscaled data ...', TRUE, "i")

    daty <- get.range.date.time(.cdtData$GalParams$date.range,
                                .cdtData$GalParams$period)
    if(.cdtData$GalParams$period == 'monthly'){
        xdeb <- format(daty$start, "%b%Y")
        xfin <- format(daty$end, "%b%Y")
    }else{
        xdeb <- paste0(as.numeric(format(daty$start, "%d")), format(daty$start, "%b%Y"))
        xfin <- paste0(as.numeric(format(daty$start, "%d")), format(daty$end, "%b%Y"))
    }

    outdir <- file.path(.cdtData$GalParams$output$dir,
                         paste('ADJUSTED_Temp_Data', xdeb, xfin, sep = '_'))
    dir.create(outdir, showWarnings = FALSE, recursive = TRUE)

    ##################
    ## TEMP sample file
    tmpDataInfo <- getNCDFSampleData(.cdtData$GalParams$TEMP$sample)
    if(is.null(tmpDataInfo)){
        Insert.Messages.Out("No downscaled data sample found", format = TRUE)
        return(NULL)
    }

    ##################

    ncInfo <- ncInfo.with.date.range(.cdtData$GalParams$TEMP,
                                     .cdtData$GalParams$date.range,
                                     .cdtData$GalParams$period)
    if(is.null(ncInfo)){
        Insert.Messages.Out("Downscaled data not found", TRUE, "e")
        return(NULL)
    }
    ncInfo$ncinfo <- tmpDataInfo

    ##################
    ## READ BIAS FILSES

    BIAS <- readBiasFiles(params = .cdtData$GalParams, variable = "temp")
    if(is.null(BIAS)) return(NULL)

    bbox1 <- sapply(tmpDataInfo[c("lon", "lat")], range)
    bbox2 <- sapply(BIAS[c("lon", "lat")], range)
    btx <- max(bbox1[1, 1], bbox2[1, 1])
    bty <- max(bbox1[1, 2], bbox2[1, 2])
    upx <- min(bbox1[2, 1], bbox2[2, 1])
    upy <- min(bbox1[2, 2], bbox2[2, 2])

    if(btx >= upx | bty >= upy){
        Insert.Messages.Out("Bias and the temperature data are in the different region", TRUE, "e")
        return(NULL)
    }

    diff.grid <- is.diffSpatialPixelsObj(defSpatialPixels(BIAS[c("lon", "lat")]),
                                         defSpatialPixels(tmpDataInfo[c("lon", "lat")]),
                                         tol = 1e-03)
    if(diff.grid){
        Insert.Messages.Out("Bias and the temperature data have different grid", TRUE, "e")
        return(NULL)
    }

    ret <- applyBiasCorrection(BIAS, ncInfo, outdir, params = .cdtData$GalParams, variable = "temp")

    if(!is.null(ret)){
        if(ret == 0) return(0)
        else return(ret)
    }else return(NULL)
}
