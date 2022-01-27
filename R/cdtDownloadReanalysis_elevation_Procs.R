
download_ReanalysisDEM <- function(){
    on.exit({
        tkconfigure(.cdtEnv$tcl$main$win, cursor = '')
        tcl('update')
    })
    tkconfigure(.cdtEnv$tcl$main$win, cursor = 'watch')
    tcl('update')

    url <- 'http://iridl.ldeo.columbia.edu/home/.rijaf/.Reanalysis_Surface_Geopotential'
    area <- paste('X', .cdtData$GalParams$minlon - 0.1, .cdtData$GalParams$maxlon + 0.1, 'RANGEEDGES',
                  'Y', .cdtData$GalParams$minlat - 0.1, .cdtData$GalParams$maxlat + 0.1, 'RANGEEDGES',
                   sep = '/')

    reanal <- switch(.cdtData$GalParams$prod,
                     "JRA-55" = ".JRA55",
                     "MERRA-2" = ".MERRA2",
                     "ERA5" = ".ERA5",
                     "ERA5-Land" = ".ERA5_Land")

    destfile <- tempfile()
    link <- paste(url, reanal, '.sgeoph', area, 'data.nc', sep = '/')
    ret <- try(utils::download.file(link, destfile, method = "auto", quiet = TRUE, mode = "wb", cacheOK = TRUE), silent = TRUE)

    if(inherits(ret, "try-error")){
        Insert.Messages.Out(.cdtData$GalParams[['message']][['5']], TRUE, "e")
        Insert.Messages.Out(gsub('[\r\n]', '', ret[1]), TRUE, "e")
    }else{
        if(ret == 0)
            Insert.Messages.Out(.cdtData$GalParams[['message']][['4']], TRUE, "s")
        else
            Insert.Messages.Out(.cdtData$GalParams[['message']][['5']], TRUE, "e")
    }

    demfile <- file.path(.cdtData$GalParams$dir2save, paste0(.cdtData$GalParams$prod, "_DEM.nc"))

    nc <- ncdf4::nc_open(destfile)
    lon <- nc$dim$X$vals
    lat <- nc$dim$Y$vals
    geo <- ncdf4::ncvar_get(nc, 'sgeoph')
    ncdf4::nc_close(nc)

    dem <- (geo * 6371008.7714) / (9.80665 * 6371008.7714 - geo)
    dem[is.na(dem)] <- -9999

    dx <- ncdf4::ncdim_def("Lon", "degreeE", lon)
    dy <- ncdf4::ncdim_def("Lat", "degreeN", lat)
    xydim <- list(dx, dy)
    longname <- "Reanalysis Elevation Data"
    grddem <- ncdf4::ncvar_def("dem", "m", xydim, -9999, prec = "float",
                               longname = longname, compression = 6)

    nc <- ncdf4::nc_create(demfile, grddem)
    ncdf4::ncvar_put(nc, grddem, geo)
    ncdf4::nc_close(nc)

    Insert.Messages.Out(.cdtData$GalParams[['message']][['4']], TRUE, "s")

    return(0)
}
