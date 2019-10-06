ExecDownload_DEM <- function(){
    on.exit({
        tkconfigure(.cdtEnv$tcl$main$win, cursor = '')
        tcl('update')
    })
    tkconfigure(.cdtEnv$tcl$main$win, cursor = 'watch')
    tcl('update')

    ## DEM NOAA NGDC ETOPO2v2: ETOPO2v2c Global Gridded 2-minute elevation and bathymetric data.
    url <- 'http://iridl.ldeo.columbia.edu/SOURCES/.NOAA/.NGDC/.ETOPO2v2/.z'
    area <- paste('X', .cdtData$GalParams$minlon - 0.1, .cdtData$GalParams$maxlon + 0.1, 'RANGEEDGES',
                  'Y', .cdtData$GalParams$minlat - 0.1, .cdtData$GalParams$maxlat + 0.1, 'RANGEEDGES',
                   sep = '/')
    destfile <- file.path(.cdtData$GalParams$dir2save, 'DEM_2_Arc-Minute.nc')
    link <- paste(url, area, 'data.nc', sep = '/')
    ret <- try(utils::download.file(link, destfile, method = "auto", quiet = TRUE, mode = "wb", cacheOK = TRUE), silent = TRUE)

    if(inherits(ret, "try-error")){
        Insert.Messages.Out(.cdtData$GalParams[['message']][['5']], format = TRUE)
        Insert.Messages.Out(gsub('[\r\n]', '', ret[1]), format = TRUE)
    }else{
        if(ret == 0)
            Insert.Messages.Out(.cdtData$GalParams[['message']][['4']], TRUE, "s")
        else
            Insert.Messages.Out(.cdtData$GalParams[['message']][['5']], format = TRUE)
    }

    ## NOAA NGDC ETOPO1: ETOPO1 Grid Registered 1 Arc-Minute Global Relief Model
    url1 <- 'http://iridl.ldeo.columbia.edu/SOURCES/.NOAA/.NGDC/.ETOPO1/.z_bedrock'
    area1 <- paste('lon', .cdtData$GalParams$minlon - 0.1, .cdtData$GalParams$maxlon + 0.1, 'RANGEEDGES',
                   'lat', .cdtData$GalParams$minlat - 0.1, .cdtData$GalParams$maxlat + 0.1, 'RANGEEDGES',
                    sep = '/')
    destfile1 <- file.path(.cdtData$GalParams$dir2save, 'DEM_1_Arc-Minute.nc')
    link1 <- paste(url1, area1, 'data.nc', sep = '/')
    ret1 <- try(utils::download.file(link1, destfile1, method = "auto", quiet = TRUE, mode = "wb", cacheOK = TRUE), silent = TRUE)

    if(inherits(ret1, "try-error")){
        Insert.Messages.Out(.cdtData$GalParams[['message']][['7']], format = TRUE)
        Insert.Messages.Out(gsub('[\r\n]', '', ret1[1]), format = TRUE)
    }else{
        if(ret1 == 0)
            Insert.Messages.Out(.cdtData$GalParams[['message']][['6']], TRUE, "s")
        else
            Insert.Messages.Out(.cdtData$GalParams[['message']][['7']], format = TRUE)
    }
    return(0)
}
