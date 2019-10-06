
STAT_ValidationDataExec <- function(GeneralParameters){
    freqData <- GeneralParameters$Tstep
    inputInfo <- GeneralParameters[c('Tstep', 'clim.var', 'STN.file1', 'STN.file2')]
    readstnData <- TRUE
    if(!is.null(.cdtData$EnvData$inputInfo))
        if(isTRUE(all.equal(.cdtData$EnvData$inputInfo, inputInfo)))
            readstnData <- FALSE

    if(readstnData){
        stnInfo1 <- getStnOpenDataInfo(GeneralParameters$STN.file1)
        if(!is.null(.cdtData$EnvData$stnData1)){
            if(!isTRUE(all.equal(.cdtData$EnvData$stnData1$stnInfo, stnInfo1))){
                readstnData1 <- TRUE
                .cdtData$EnvData$stnData1 <- NULL
            }else readstnData1 <- FALSE
        }else readstnData1 <- TRUE

        if(readstnData1){
            cdtTmpVar <- getStnOpenData(GeneralParameters$STN.file1)
            cdtTmpVar <- getCDTdataAndDisplayMsg(cdtTmpVar, freqData, GeneralParameters$STN.file1)
            if(is.null(cdtTmpVar)) return(NULL)
            cdtTmpVar <- cdtTmpVar[c('id', 'lon', 'lat', 'dates', 'data')]
            cdtTmpVar$index <- seq_along(cdtTmpVar$dates)
            .cdtData$EnvData$stnData1 <- cdtTmpVar
            .cdtData$EnvData$stnData1$stnInfo <- stnInfo1
        }

        stnInfo2 <- getStnOpenDataInfo(GeneralParameters$STN.file2)
        if(!is.null(.cdtData$EnvData$stnData2)){
            if(!isTRUE(all.equal(.cdtData$EnvData$stnData2$stnInfo, stnInfo2))){
                readstnData2 <- TRUE
                .cdtData$EnvData$stnData2 <- NULL
            }else readstnData2 <- FALSE
        }else readstnData2 <- TRUE

        if(readstnData2){
            cdtTmpVar <- getStnOpenData(GeneralParameters$STN.file2)
            cdtTmpVar <- getCDTdataAndDisplayMsg(cdtTmpVar, freqData, GeneralParameters$STN.file2)
            if(is.null(cdtTmpVar)) return(NULL)
            cdtTmpVar <- cdtTmpVar[c('id', 'lon', 'lat', 'dates', 'data')]
            cdtTmpVar$index <- seq_along(cdtTmpVar$dates)
            .cdtData$EnvData$stnData2 <- cdtTmpVar
            .cdtData$EnvData$stnData2$stnInfo <- stnInfo2
        }

        if(readstnData1 | readstnData2){
            if(GeneralParameters$outdir %in% c("", "NA")){
                Insert.Messages.Out("Directory to save results doesn't exist", format = TRUE)
                return(NULL)
            }

            if(!any(.cdtData$EnvData$stnData1$id %in% .cdtData$EnvData$stnData2$id)){
                Insert.Messages.Out("IDs of the two stations data do not match", format = TRUE)
                return(NULL)
            }
            if(!any(.cdtData$EnvData$stnData1$dates %in% .cdtData$EnvData$stnData2$dates)){
                Insert.Messages.Out("The dates of the two stations data do not match", format = TRUE)
                return(NULL)
            }

            inx <- match(.cdtData$EnvData$stnData1$dates, .cdtData$EnvData$stnData2$dates)
            inx <- inx[!is.na(inx)]
            dates2 <- .cdtData$EnvData$stnData2$dates[inx]
            data2 <- .cdtData$EnvData$stnData2$data[inx, , drop = FALSE]
            data1 <- .cdtData$EnvData$stnData1$data[.cdtData$EnvData$stnData1$dates %in% dates2, , drop = FALSE]
            .cdtData$EnvData$cdtData$dates <- dates2

            jnx <- match(.cdtData$EnvData$stnData1$id, .cdtData$EnvData$stnData2$id)
            jnx <- jnx[!is.na(jnx)]
            id2 <- .cdtData$EnvData$stnData2$id[jnx]

            .cdtData$EnvData$cdtData$info$id <- id2
            .cdtData$EnvData$cdtData$info$lon <- .cdtData$EnvData$stnData2$lon[jnx]
            .cdtData$EnvData$cdtData$info$lat <- .cdtData$EnvData$stnData2$lat[jnx]
            .cdtData$EnvData$cdtData$fcst <- data2[, jnx, drop = FALSE]
            .cdtData$EnvData$cdtData$obs <- data1[, .cdtData$EnvData$stnData1$id %in% id2, drop = FALSE]

            .cdtData$EnvData$GeneralParameters <- GeneralParameters

            ##################
            outValidation <- file.path(GeneralParameters$outdir, paste0('VALIDATION_',
                                       tools::file_path_sans_ext(GeneralParameters$STN.file1)))
            dir.create(outValidation, showWarnings = FALSE, recursive = TRUE)

            xhead <- do.call(rbind, .cdtData$EnvData$cdtData$info[c('id', 'lon', 'lat')])
            xhead <- cbind(c("STN", "LON", "DATE/LAT"), xhead)

            obs2file <- cbind(.cdtData$EnvData$cdtData$dates, .cdtData$EnvData$cdtData$obs)
            obs2file <- rbind(xhead, obs2file)
            fcst2file <- cbind(.cdtData$EnvData$cdtData$dates, .cdtData$EnvData$cdtData$fcst)
            fcst2file <- rbind(xhead, fcst2file)
            obs2file[is.na(obs2file)] <- -99
            fcst2file[is.na(fcst2file)] <- -99

            dirCDTdata <- file.path(outValidation, "OBS_GRD_DATA")
            dir.create(dirCDTdata, showWarnings = FALSE, recursive = TRUE)
            writeFiles(obs2file, file.path(dirCDTdata, "Observations.csv"))
            writeFiles(fcst2file, file.path(dirCDTdata, "Gridded_at_Obs_Loc.csv"))

            fileValidOut <- file.path(outValidation, "VALIDATION_DATA_OUT.rds")

            stats.data <- .cdtData$EnvData[c('GeneralParameters', 'cdtData')]
            saveRDS(stats.data, file = fileValidOut)
        }

        .cdtData$EnvData$inputInfo <- inputInfo
    }

    return(0)
}
