
exec.download_Reanalysis <- function(){
    on.exit({
        tkconfigure(.cdtEnv$tcl$main$win, cursor = '')
        tcl('update')
    })
    tkconfigure(.cdtEnv$tcl$main$win, cursor = 'watch')
    tcl('update')

   download.fun <- switch(.cdtData$GalParams$prod,
                          "jra55" = switch(.cdtData$GalParams$src,
                                           "rda.ucar.edu" = jra55.download.rda.ucar,
                                           NULL),
                           "merra2" =  switch(.cdtData$GalParams$src,
                                              "iridl.ldeo.columbia.edu" = merra2.download.iridl,
                                              "disc.gsfc.nasa.gov" = merra2.download.earthdata,
                                              NULL), 
                           "era5" =  switch(.cdtData$GalParams$src,
                                            "cds.climate.copernicus.eu" = era5.download.cds,
                                            NULL)
                        )
    if(is.null(download.fun)){
        Insert.Messages.Out("Not available", TRUE, "w")
        return(0)
    }
    ret <- download.fun(.cdtData$GalParams)
    return(ret)
}
