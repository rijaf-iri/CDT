
crossValidationExecClimData <- function(){
    message <- .cdtData$GalParams[['message']]
    Insert.Messages.Out(message[['10']], TRUE, "i")

    varClim <- gsub("crossv\\.", "", .cdtData$GalParams$action)
    if(varClim == 'pres'){
        if(.cdtData$GalParams$prmsl) varClim <- "prmsl"
    }

    daty <- seq.format.date.time(.cdtData$GalParams$period,
                                 .cdtData$GalParams$date.range,
                                 .cdtData$GalParams$minhour)
    dtrg <- merged_date_range_filename(daty, .cdtData$GalParams$period)

    dirMRGClim <- paste('CrossValidation', toupper(varClim), 'Data', dtrg$start, dtrg$end, sep = '_')
    outdir <- file.path(.cdtData$GalParams$outdir, dirMRGClim)
    dir.create(file.path(outdir, 'DATA'), showWarnings = FALSE, recursive = TRUE)

    merging.options(saveGridBuffer = FALSE, saveRnoR = FALSE)

    ##################
    ## station data
    stnData <- getStnOpenData(.cdtData$GalParams$STN.file)
    stnData <- getCDTdataAndDisplayMsg(stnData, .cdtData$GalParams$period,
                                       .cdtData$GalParams$STN.file)
    if(is.null(stnData)) return(NULL)

    ##################
    ## NetCDF sample file
    ncDataInfo <- getNCDFSampleData(.cdtData$GalParams$INPUT$sample)
    if(is.null(ncDataInfo)){
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

    grd.lon <- ncDataInfo$lon
    grd.lat <- ncDataInfo$lat
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

    ncInfo <- ncInfo.with.date.range(.cdtData$GalParams$INPUT,
                                     .cdtData$GalParams$date.range,
                                     .cdtData$GalParams$period,
                                     .cdtData$GalParams$minhour)
    if(is.null(ncInfo)){
        Insert.Messages.Out(message[['14']], TRUE, "e")
        return(NULL)
    }
    ncInfo$ncinfo <- ncDataInfo

    ##################
    ## select station for cross-validation

    nbNA <- colSums(!is.na(stnData$data[stnData$dates %in% ncInfo$dates, , drop = FALSE]))

    if(!any(nbNA > 0)){
        Insert.Messages.Out(message[['15']], TRUE, "e")
        return(NULL)
    }

    if(.cdtData$GalParams$selstn$from == "file"){
        df <- getStnOpenData(.cdtData$GalParams$selstn$file.stn)
        if(is.null(df)) return(NULL)

        if(.cdtData$GalParams$selstn$file.type == 'cdtstation'){
            df <- splitCDTData0(df)
            if(is.null(df)) return(NULL)
            df <- as.data.frame(df[c("id", 'lon', 'lat')])
        }

        istn <- as.character(df[, 1]) %in% stnData$id
        if(!any(istn)){
            Insert.Messages.Out(message[['20']], TRUE, "e")
            return(NULL)
        }

        if(any(!istn)){
            outlist <- list(message[['21']], df[!istn, , drop = FALSE])
            containertab <- Display_Output_Console_Tab(outlist, title = .cdtData$GalParams$selstn$file.stn)
            ntab <- update.OpenTabs('ctxt', containertab)
            tkselect(.cdtEnv$tcl$main$tknotes, ntab)
        }

        stn.valid <- as.character(df[istn, 1])
        ix <- match(stn.valid, stnData$id)
        stn.valid <- stn.valid[nbNA[ix] > 0]
        if(length(stn.valid) == 0){
            Insert.Messages.Out(message[['15']], TRUE, "e")
            return(NULL)
        }
    }

    if(.cdtData$GalParams$selstn$from == "cdt"){
        istn <- nbNA / length(ncInfo$dates) >= (.cdtData$GalParams$selstn$min.perc / 100)
        if(!any(istn)){
            Insert.Messages.Out(message[['16']], TRUE, "e")
            return(NULL)
        }
        df <- as.data.frame(stnData[c("id", 'lon', 'lat')])
        df <- df[istn, , drop = FALSE]

        stn.valid <- select.Station.Validation(df, perc = 20)
        stn.valid <- as.character(stn.valid$id)
    }

    if(.cdtData$GalParams$selstn$from == "all"){
        stn.valid <- stnData$id[nbNA > 0]
        if(length(stn.valid) == 0){
            Insert.Messages.Out(message[['15']], TRUE, "e")
            return(NULL)
        }
    }

    stn.valid <- which(stnData$id %in% stn.valid)

    ##################

    if(.cdtData$GalParams$action != "crossv.rain")
        .cdtData$GalParams$RnoR <- list(use = FALSE, wet = 1.0, smooth = FALSE)

    ret <- cdtMergingLOOCV(stnData = stnData, stnVID = stn.valid, ncInfo = ncInfo, xy.grid = xy.grid, 
                           params = .cdtData$GalParams, variable = varClim, demData = demData,
                           outdir = outdir)

    out_params <- .cdtData$GalParams
    out_params <- out_params[!names(out_params) %in% c("settingSNC", "message")]
    out_params$options <- merging.options()
    saveRDS(out_params, file.path(outdir, 'merging_parameters.rds'))

    if(!is.null(ret)){
        if(ret != 0){
          file_log <- file.path(outdir, "log_file.txt")
          Insert.Messages.Out(paste(message[['17']], file_log), TRUE, "w")
        }
    }else return(NULL)

    Insert.Messages.Out(message[['18']], TRUE, "s")
    return(0)
}
