
ExecDownload_GADM <- function(){
    on.exit({
        tkconfigure(.cdtEnv$tcl$main$win, cursor = '')
        tcl('update')
    })
    tkconfigure(.cdtEnv$tcl$main$win, cursor = 'watch')
    tcl('update')

    baseURL <- 'https://geodata.ucdavis.edu/gadm/'
    baseURL <- paste0(baseURL, 'gadm', .cdtData$GalParams$version)
    gadm_ver <- gsub('\\.', '', .cdtData$GalParams$version)
    layer <- paste0('gadm', gadm_ver, '_', 
                    .cdtData$GalParams$cntr_iso3, '_',
                    .cdtData$GalParams$level)
    gadm_frmt <- switch(.cdtData$GalParams$version,
                        '3.6' = c('Rsf', '_sf', '.rds'),
                        '4.0' = c('Rsf', '_sf', '.rds'),
                        '4.1' = c('json', '', '.json.zip')
                       )
    urlfl <- paste0(baseURL, '/', gadm_frmt[1], '/', layer, gadm_frmt[2], gadm_frmt[3])
    destfl <- paste0(.cdtData$GalParams$dir2save, '/', layer, gadm_frmt[2], gadm_frmt[3])

    handle <- curl::new_handle()
    curl::handle_setopt(handle, ssl_verifypeer = FALSE)
    ret <- try(curl::curl_download(urlfl, destfl, handle = handle), silent = TRUE)

    if(inherits(ret, "try-error")){
        Insert.Messages.Out(.cdtData$GalParams[['message']][['5']], format = TRUE)
        Insert.Messages.Out(gsub('[\r\n]', '', ret[1]), format = TRUE)
        return(NULL)
    }

    if(.cdtData$GalParams$version == '4.1'){
        utils::unzip(destfl, exdir = .cdtData$GalParams$dir2save)
        tmpf <- gsub("\\.zip$", "", basename(destfl))
        destfl <- file.path(.cdtData$GalParams$dir2save, tmpf)
    }

    shp <- switch(.cdtData$GalParams$version,
                '3.6' = local({
                            sf_data <- readRDS(destfl)
                            sf::st_crs(sf_data) <- sf::st_crs(sf_data)
                            sf_data
                        }),
                '4.0' = local({
                            readRDS(destfl)
                        }),
                '4.1' = local({
                            sf::st_read(destfl, quiet = TRUE)
                        })
            )

    shpfile <- file.path(.cdtData$GalParams$dir2save, paste0(layer, '.shp'))
    sf::st_write(shp, shpfile, driver = "ESRI Shapefile", quiet = TRUE, append = FALSE)

    Insert.Messages.Out(.cdtData$GalParams[['message']][['4']], TRUE, "s")
    return(0)
}
