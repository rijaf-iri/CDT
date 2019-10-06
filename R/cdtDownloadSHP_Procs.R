
ExecDownload_GADM <- function(){
    on.exit({
        tkconfigure(.cdtEnv$tcl$main$win, cursor = '')
        tcl('update')
    })
    tkconfigure(.cdtEnv$tcl$main$win, cursor = 'watch')
    tcl('update')

    baseURL <- 'https://biogeo.ucdavis.edu/data/gadm3.6/Rsp/'
    layer <- paste('gadm36', .cdtData$GalParams$cntr_iso3, .cdtData$GalParams$level, 'sp', sep = "_")
    urlfl <- paste0(baseURL, layer, '.rds')
    destfl <- paste0(tempfile(), '.rds')
    ret <- try(utils::download.file(urlfl, destfl, method = "auto", quiet = TRUE, mode = "wb", cacheOK = TRUE), silent = TRUE)
    if(inherits(ret, "try-error")){
        Insert.Messages.Out(.cdtData$GalParams[['message']][['5']], format = TRUE)
        Insert.Messages.Out(gsub('[\r\n]', '', ret[1]), format = TRUE)
        return(NULL)
    }else{
        if(ret != 0){
            Insert.Messages.Out(.cdtData$GalParams[['message']][['5']], format = TRUE)
            return(NULL)
        }
    }

    shp <- readRDS(destfl)
    rgdal::writeOGR(shp, dsn = .cdtData$GalParams$dir2save, layer = layer, driver = "ESRI Shapefile")
    unlink(destfl)
    Insert.Messages.Out(.cdtData$GalParams[['message']][['4']], TRUE, "s")
    return(0)
}
