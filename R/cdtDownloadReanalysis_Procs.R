
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
                                    "jra.kishou.go.jp" = NULL,
                                    NULL),
                        "merra2" =  switch(.cdtData$GalParams$src,
                                    "iridl.ldeo.columbia.edu" = merra2.download.iridl,
                                    "disc.gsfc.nasa.gov" = NULL,
                                    NULL), 
                        "era5" =  switch(.cdtData$GalParams$src,
                                    "cds.climate.copernicus.eu" = NULL,
                                    NULL)
                        )
    if(is.null(download.fun)){
        Insert.Messages.Out("Under construction", TRUE, "w")
        return(0)
    }
    ret <- download.fun(.cdtData$GalParams)
    return(ret)
}
