
CPT.convertProcs <- function(){
    Insert.Messages.Out(.cdtData$GalParams[['message']][['6']], TRUE, "i")

    cptInfo <- list(name = .cdtData$GalParams$cptinfo$name,
                    units = .cdtData$GalParams$cptinfo$units,
                    missval = .cdtData$GalParams$cptinfo$missval)

    if(.cdtData$GalParams$data.type == "cdtstation"){
        cdtdata <- getStnOpenData(.cdtData$GalParams$cdtstation)
        if(is.null(cdtdata)) return(NULL)

        ret <- CPT.convertStationData.Files(cdtdata, .cdtData$GalParams$output, cptInfo)
    }

    if(.cdtData$GalParams$data.type == "cdtnetcdf"){
        ncDataInfo <- getNCDFSampleData(.cdtData$GalParams$cdtnetcdf$sample)
        if(is.null(ncDataInfo)){
            Insert.Messages.Out(.cdtData$GalParams[['message']][['7']], format = TRUE)
            return(NULL)
        }

        ncInfo <- c(.cdtData$GalParams$cdtnetcdf[c('dir', 'format')],
                    ncDataInfo[c('ilon', 'ilat', 'varid')])
        ret <- CPT.convertGridData.Files(ncInfo, .cdtData$GalParams$output, cptInfo)
    }

    return(ret)
}
