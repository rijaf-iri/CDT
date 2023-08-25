
HOV_DataExtraction <- function(GeneralParameters){
    message <- .cdtData$EnvData$message

    if(GeneralParameters$outdir %in% c("", "NA")){
        Insert.Messages.Out(message[['6']], TRUE, "e")
        return(NULL)
    }

    # outValidation <- file.path(GeneralParameters$outdir, paste0('VALIDATION_',
    #                            basename(GeneralParameters$ncdf.file$dir)))
    outValidation <- file.path(GeneralParameters$outdir, paste0('VALIDATION_',
                               tools::file_path_sans_ext(GeneralParameters$STN.file)))
    dir.create(outValidation, showWarnings = FALSE, recursive = TRUE)

    ##################
    ## Get data

    stnInfo <- getStnOpenDataInfo(GeneralParameters$STN.file)
    if(!is.null(.cdtData$EnvData$stnData)){
        if(!isTRUE(all.equal(.cdtData$EnvData$stnData$stnInfo, stnInfo))){
            readstnData <- TRUE
            .cdtData$EnvData$stnData <- NULL
        }else readstnData <- FALSE
    }else readstnData <- TRUE

    if(readstnData){
        cdtTmpVar <- getStnOpenData(GeneralParameters$STN.file)
        cdtTmpVar <- getCDTdataAndDisplayMsg(cdtTmpVar, GeneralParameters$Tstep, GeneralParameters$STN.file)
        if(is.null(cdtTmpVar)) return(NULL)
        cdtTmpVar <- cdtTmpVar[c('id', 'lon', 'lat', 'dates', 'data')]
        cdtTmpVar$index <- seq_along(cdtTmpVar$dates)
        .cdtData$EnvData$stnData <- cdtTmpVar
        .cdtData$EnvData$stnData$stnInfo <- stnInfo
    }

    ###################
    ## define geometry
    if(GeneralParameters$type.select != "all"){
        if(GeneralParameters$type.select == "rect"){
            ilon <- .cdtData$EnvData$stnData$lon >= GeneralParameters$Geom$minlon &
                    .cdtData$EnvData$stnData$lon <= GeneralParameters$Geom$maxlon
            ilat <- .cdtData$EnvData$stnData$lat >= GeneralParameters$Geom$minlat &
                    .cdtData$EnvData$stnData$lat <= GeneralParameters$Geom$maxlat
            ixy <- ilon & ilat
        }

        if(GeneralParameters$type.select == "poly"){
            shp.dat <- getShpOpenData(GeneralParameters$shp.file$shp)[[2]]
            shp <- shp.dat[as.character(shp.dat@data[, GeneralParameters$shp.file$attr]) == GeneralParameters$Geom$namePoly, ]
            pts.dat <- data.frame(x = .cdtData$EnvData$stnData$lon, y = .cdtData$EnvData$stnData$lat)
            sp::coordinates(pts.dat)<- ~x+y
            ixy <- unname(!is.na(sp::over(pts.dat, sp::geometry(shp))))
        }

        if(!any(ixy)){
            Insert.Messages.Out(message[['18']], TRUE, 'e')
            return(NULL)
        }
    }else ixy <- rep(TRUE, length(.cdtData$EnvData$stnData$lon))

    ##################
    ## ncdf data sample file
    ncDataInfo <- getNCDFSampleData(GeneralParameters$ncdf.file$sample)
    if(is.null(ncDataInfo)){
        Insert.Messages.Out(message[['19']], TRUE, 'e')
        return(NULL)
    }

    ##################
    # Get NCDF data info

    ncInfo <- ncInfo.with.date.range(GeneralParameters$ncdf.file,
                                     GeneralParameters$Extract.Date,
                                     GeneralParameters$Tstep)
    if(is.null(ncInfo)){
        Insert.Messages.Out(message[['20']], TRUE, "e")
        return(NULL)
    }

    ##################

    if(length(intersect(.cdtData$EnvData$stnData$dates, ncInfo$dates[ncInfo$exist])) == 0){
        Insert.Messages.Out(message[['21']], TRUE, 'e')
        return(NULL)
    }

    ##################

    dates <- ncInfo$date[ncInfo$exist]
    ncPATH <- ncInfo$ncfiles[ncInfo$exist]

    ijx <- grid2pointINDEX(.cdtData$EnvData$stnData, ncDataInfo)

    ##################

    ncdataInfo <- c(GeneralParameters$ncdf.file$dir, GeneralParameters$ncdf.file$format)
    bindncdfData <- FALSE
    if(!is.null(.cdtData$EnvData$ncdfData)){
        iexist <- dates %in% .cdtData$EnvData$ncdfData$dates
        if(all(iexist)){
            if(!isTRUE(all.equal(.cdtData$EnvData$ncdfData$ncdataInfo, ncdataInfo))){
                readNcdfData <- TRUE
                .cdtData$EnvData$ncdfData <- NULL
            }else readNcdfData <- FALSE
        }else{
            if(isTRUE(all.equal(.cdtData$EnvData$ncdfData$ncdataInfo, ncdataInfo))){
                bindncdfData <- TRUE
                if(any(iexist)){
                    dates <- dates[!iexist]
                    ncPATH <- ncPATH[!iexist]
                }
            }else .cdtData$EnvData$ncdfData <- NULL
            readNcdfData <- TRUE
        }
    }else readNcdfData <- TRUE

    if(readNcdfData){
        Insert.Messages.Out(message[['22']], TRUE, "i")

        parsL <- doparallel.cond(length(ncPATH) >= 180)
        ncData <- cdt.foreach(seq_along(ncPATH), parsL, GUI = TRUE,
                              progress = TRUE, FUN = function(jj)
        {
            nc <- ncdf4::nc_open(ncPATH[jj])
            vars <- ncdf4::ncvar_get(nc, varid = ncDataInfo$varid)
            ncdf4::nc_close(nc)
            vars <- transposeNCDFData(vars, ncDataInfo)
            vars[ijx]
        })
 
        Insert.Messages.Out(message[['23']], TRUE, "s")

        ncData <- do.call(rbind, ncData)

        cdtTmpVar <- NULL
        idx <- seq(length(dates))
        if(bindncdfData){
            cdtTmpVar$data <- rbind(.cdtData$EnvData$ncdfData$data, ncData)
            cdtTmpVar$dates <- c(.cdtData$EnvData$ncdfData$dates, dates)
            cdtTmpVar$index <- c(.cdtData$EnvData$ncdfData$index, max(.cdtData$EnvData$ncdfData$index) + idx)
        }else{
            cdtTmpVar$data <- ncData
            cdtTmpVar$dates <- dates
            cdtTmpVar$index <- idx
        }
        odaty <- order(cdtTmpVar$dates)
        cdtTmpVar$dates <- cdtTmpVar$dates[odaty]
        cdtTmpVar$index <- cdtTmpVar$index[odaty]

        cdtTmpVar$ncdataInfo <- ncdataInfo
        .cdtData$EnvData$ncdfData <- cdtTmpVar
    }

    idx.stn <- match(.cdtData$EnvData$ncdfData$dates, .cdtData$EnvData$stnData$dates)
    idx.stn <- .cdtData$EnvData$stnData$index[stats::na.omit(idx.stn)]
    idx.ncdf <- which(.cdtData$EnvData$ncdfData$dates %in% .cdtData$EnvData$stnData$dates[idx.stn])

    .cdtData$EnvData$cdtData$info$id <- .cdtData$EnvData$stnData$id[ixy]
    .cdtData$EnvData$cdtData$info$lon <- .cdtData$EnvData$stnData$lon[ixy]
    .cdtData$EnvData$cdtData$info$lat <- .cdtData$EnvData$stnData$lat[ixy]
    .cdtData$EnvData$cdtData$dates <- .cdtData$EnvData$stnData$dates[idx.stn]
    .cdtData$EnvData$cdtData$obs <- .cdtData$EnvData$stnData$data[idx.stn, ixy, drop = FALSE]
    .cdtData$EnvData$cdtData$fcst <- .cdtData$EnvData$ncdfData$data[idx.ncdf, ixy, drop = FALSE]
    .cdtData$EnvData$GeneralParameters <- GeneralParameters

    xhead <- cbind(c("STN", "LON", "DATE/LAT"),
                   rbind(.cdtData$EnvData$cdtData$info$id,
                   .cdtData$EnvData$cdtData$info$lon,
                   .cdtData$EnvData$cdtData$info$lat))
    obs2file <- rbind(xhead, cbind(.cdtData$EnvData$cdtData$dates,
                                   .cdtData$EnvData$cdtData$obs))
    fcst2file <- rbind(xhead, cbind(.cdtData$EnvData$cdtData$dates,
                                    .cdtData$EnvData$cdtData$fcst))
    obs2file[is.na(obs2file)] <- .cdtData$Config$missval
    fcst2file[is.na(fcst2file)] <- .cdtData$Config$missval

    dirCDTdata <- file.path(outValidation, "OBS_GRD_DATA")
    dir.create(dirCDTdata, showWarnings = FALSE, recursive = TRUE)
    writeFiles(obs2file, file.path(dirCDTdata, "Observations.csv"))
    writeFiles(fcst2file, file.path(dirCDTdata, "Gridded_at_Obs_Loc.csv"))

    fileValidOut <- file.path(outValidation, "VALIDATION_DATA_OUT.rds")

    hovd.data <- .cdtData$EnvData[c('GeneralParameters', 'cdtData', 'stnData', 'ncdfData')]
    saveRDS(hovd.data, file = fileValidOut)

    Insert.Messages.Out(message[['24']], TRUE, 's')
    return(0)
}
