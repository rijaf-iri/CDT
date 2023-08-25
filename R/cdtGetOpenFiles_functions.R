
## Get index of selected file from .cdtData$OpenFiles$Data
getIndex.AllOpenFiles <- function(nomfile){
    if(inherits(nomfile, "tclVar")){
        fileio <- trimws(tclvalue(nomfile))
    }else if(is.character(nomfile)){
        fileio <- nomfile
    }else return(NULL)

    if(fileio != ""){
        all.open.file <- do.call(c, lapply(.cdtData$OpenFiles$Data, "[[", 1))
        jfile <- which(all.open.file == fileio)
        return(jfile)
    }

    return(NULL)
}

##############################################
## Get stn data in the list (all open files)
## return CDT data format
getStnOpenData <- function(file.stnfl){
    jfile <- getIndex.AllOpenFiles(file.stnfl)
    donne <- NULL
    if(length(jfile) > 0){
        if(.cdtData$OpenFiles$Type[[jfile]] == "ascii")
            donne <- .cdtData$OpenFiles$Data[[jfile]][[2]]
    }
    return(donne)
}

getStnOpenDataInfo <- function(file.stnfl){
    jfile <- getIndex.AllOpenFiles(file.stnfl)
    info <- NULL
    if(length(jfile) > 0){
        if(.cdtData$OpenFiles$Type[[jfile]] == "ascii")
            info <- .cdtData$OpenFiles$Data[[jfile]][c(1, 3:4)]
    }
    return(info)
}

getCDTdataAndDisplayMsg <- function(donne, tstep, filename){
    if(is.null(donne)) return(NULL)
    donne <- splitCDTData(donne, tstep)
    if(is.null(donne)) return(NULL)

    ###############
    cdt.file.conf <- file.path(.cdtDir$dirLocal, "config", "cdt_config.json")
    Config <- jsonlite::fromJSON(cdt.file.conf)
    xml.dlg <- file.path(.cdtDir$dirLocal, "languages", "cdtParseStationData_functions.xml")
    lang.dlg <- cdtLanguageParse(xml.dlg, Config$lang.iso)

    ###############
    outlist <- list()
    if(!is.null(donne$duplicated.stnID))
        outlist <- c(outlist, list(lang.dlg[['message']][['9']], as.matrix(donne$duplicated.stnID)))
    if(!is.null(donne$duplicated.coords))
        outlist <- c(outlist, list(lang.dlg[['message']][['10']], as.matrix(donne$duplicated.coords)))
    if(!is.null(donne$missing.coords))
        outlist <- c(outlist, list(lang.dlg[['message']][['11']], as.matrix(donne$missing.coords)))
    if(!is.null(donne$duplicated.dates)){
        tmp0 <- donne$duplicated.dates$date
        tmp0 <- matrix(c(tmp0, rep("", 10 - (length(tmp0) %% 10))), ncol = 10, byrow = TRUE)
        outlist <- c(outlist, list(lang.dlg[['message']][['12']], tmp0))
    }
    if(!is.null(donne$wrong.dates)){
        tmp0 <- donne$wrong.dates$date
        tmp0 <- matrix(c(tmp0, rep("", 10 - (length(tmp0) %% 10))), ncol = 10, byrow = TRUE)
        outlist <- c(outlist, list(lang.dlg[['message']][['13']], tmp0))
    }
    if(!is.null(donne$missing.dates)){
        tmp0 <- donne$missing.dates$date
        tmp0 <- matrix(c(tmp0, rep("", 10 - (length(tmp0) %% 10))), ncol = 10, byrow = TRUE)
        outlist <- c(outlist, list(lang.dlg[['message']][['14']], tmp0))
    }

    if(length(outlist) > 0){
        containertab <- Display_Output_Console_Tab(outlist, title = filename)
        ntab <- update.OpenTabs('ctxt', containertab)
        tkselect(.cdtEnv$tcl$main$tknotes, ntab)
    }

    return(donne)
}

getCDTTSdataAndDisplayMsg <- function(donne, period, filefrmt, datefrmt, filename, display = TRUE){
    if(is.null(donne)) return(NULL)
    donne <- splitTsData(donne, period, filefrmt, datefrmt)
    if(is.null(donne)) return(NULL)

    ###############
    cdt.file.conf <- file.path(.cdtDir$dirLocal, "config", "cdt_config.json")
    Config <- jsonlite::fromJSON(cdt.file.conf)
    xml.dlg <- file.path(.cdtDir$dirLocal, "languages", "cdtParseStationData_functions.xml")
    lang.dlg <- cdtLanguageParse(xml.dlg, Config$lang.iso)

    ###############
    outlist <- list()
    if(!is.null(donne$duplicated.dates)){
        tmp0 <- donne$duplicated.dates$date
        tmp0 <- matrix(c(tmp0, rep("", 10 - (length(tmp0) %% 10))), ncol = 10, byrow = TRUE)
        outlist <- c(outlist, list(lang.dlg[['message']][['12']], tmp0))
    }
    if(!is.null(donne$wrong.dates)){
        tmp0 <- donne$wrong.dates$date
        tmp0 <- matrix(c(tmp0, rep("", 10 - (length(tmp0) %% 10))), ncol = 10, byrow = TRUE)
        outlist <- c(outlist, list(lang.dlg[['message']][['13']], tmp0))
    }
    if(!is.null(donne$missing.dates)){
        tmp0 <- donne$missing.dates$date
        tmp0 <- matrix(c(tmp0, rep("", 10 - (length(tmp0) %% 10))), ncol = 10, byrow = TRUE)
        outlist <- c(outlist, list(lang.dlg[['message']][['14']], tmp0))
    }

    if(length(outlist) > 0 & display){
        containertab <- Display_Output_Console_Tab(outlist, title = filename)
        ntab <- update.OpenTabs('ctxt', containertab)
        tkselect(.cdtEnv$tcl$main$tknotes, ntab)
    }

    return(donne)
}

##############################################

## Get NetCDF sample data  in the list (all open files)
getNCDFSampleData <- function(file.netcdf){
    jfile <- getIndex.AllOpenFiles(file.netcdf)
    nclist <- NULL
    if(length(jfile) > 0){
        if(.cdtData$OpenFiles$Type[[jfile]] == "netcdf"){
            ncdata <- .cdtData$OpenFiles$Data[[jfile]][[2]]
            nclist <- c(list(lon = ncdata$x, lat = ncdata$y),
                        ncdata[c('varid', 'ilon', 'ilat',
                                'xo', 'yo', 'nx', 'ny',
                                'varinfo', 'diminfo')])
        }
    }
    return(nclist)
}

##############################################

## get NetCDF data  in the list (all open files)
## return $lon $lat $val
getNcdfOpenData <- function(file.netcdf){
    jfile <- getIndex.AllOpenFiles(file.netcdf)
    nc <- NULL
    if(length(jfile) > 0){
        if(.cdtData$OpenFiles$Type[[jfile]] == "netcdf")
            nc <- .cdtData$OpenFiles$Data[[jfile]]
    }
    return(nc)
}

##############################################

## Get shp file in the list (all open files)
## return [[1]] name [[2]] shp [[3]] path
getShpOpenData <- function(shp){
    jfile <- getIndex.AllOpenFiles(shp)
    shpf <- NULL
    if(length(jfile) > 0){
        if(.cdtData$OpenFiles$Type[[jfile]] == "shp")
            shpf <- .cdtData$OpenFiles$Data[[jfile]]
    }
    return(shpf)
}
