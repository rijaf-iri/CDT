
exec.download_RFE <- function(){
    on.exit({
        tkconfigure(.cdtEnv$tcl$main$win, cursor = '')
        tcl('update')
    })
    tkconfigure(.cdtEnv$tcl$main$win, cursor = 'watch')
    tcl('update')

    download.fun <- switch(.cdtData$GalParams$rfe.src,
                    "tamsatv3.1-af" = local({
                            url.down <- if(.cdtData$GalParams$iridl.src)
                                            tamsat3.1.download.iridl
                                        else
                                            tamsatv3.1.download.reading

                            switch(.cdtData$GalParams$tstep,
                                    "daily" = url.down,
                                    "pentad" = tamsatv3.1.download.reading,
                                    "dekadal" = url.down,
                                    "monthly" = url.down
                                )
                            }),
                    "tamsatv3-af" = local({
                            url.down <- if(.cdtData$GalParams$iridl.src)
                                            tamsat3.0.download.iridl
                                        else
                                            tamsatv3.0.download.reading

                            switch(.cdtData$GalParams$tstep,
                                    "daily" = url.down,
                                    "pentad" = tamsatv3.0.download.reading,
                                    "dekadal" = url.down,
                                    "monthly" = url.down
                                )
                            }),
                    "chirp-gb" = local({
                            url.down <- if(.cdtData$GalParams$iridl.src)
                                            chirp.download.iridl
                                        else
                                            chirp.download.ucsb

                            switch(.cdtData$GalParams$tstep,
                                   "daily" = url.down,
                                   "pentad" = chirp.download.ucsb,
                                   "dekadal" = url.down,
                                   "monthly" = url.down
                                )
                            }),
                    "chirpsv2-gb" = local({
                            url.down <- if(.cdtData$GalParams$iridl.src)
                                            chirps.download.iridl
                                        else
                                            chirps.download.ucsb

                            switch(.cdtData$GalParams$tstep,
                                   "daily" = url.down,
                                   "pentad" = chirps.download.ucsb,
                                   "dekadal" = url.down,
                                   "monthly" = url.down
                                )
                            }),
                    "chirpsv2-af" = chirps.6hrAF.download.ucsb,
                    "persiann-gb" = persiann.download.uci,
                    "persianncdr-gb" = persiann_cdr.download.uci,
                    "arc2-af" = local({
                            url.down <- if(.cdtData$GalParams$iridl.src)
                                            arc2.download.iridl
                                        else
                                            # arc2.download.cpc.noaa.tif
                                            arc2.download.cpc.noaa.bin

                            switch(.cdtData$GalParams$tstep,
                                   "daily" = url.down,
                                   "dekadal" = arc2.download.iridl,
                                   "monthly" = arc2.download.iridl
                                )
                            }),
                    "rfev2-af" = local({
                            url.down <- if(.cdtData$GalParams$iridl.src)
                                            rfev2_af.download.iridl
                                        else
                                            rfev2_af.download.cpc.noaa.bin

                            switch(.cdtData$GalParams$tstep,
                                   "daily" = url.down,
                                   "dekadal" = rfev2_af.download.iridl,
                                )
                            }),
                    "rfev2-sa" = local({
                            url.down <- if(.cdtData$GalParams$iridl.src)
                                            rfev2_sa.download.iridl
                                        else
                                            rfev2_sa.download.cpc.noaa.bin

                            switch(.cdtData$GalParams$tstep,
                                   "daily" = url.down,
                                   "dekadal" = rfev2_sa.download.iridl,
                                )
                            }),
                    "trmm3b42v7-gb" = trmm3b42v7.download.dods,
                    "trmm3b42rtv7-gb" = trmm3b42v7rt.download.dods,
                    "gpm.imerg.f-gb" = gpm_imerg.download.dods,
                    "gpm.imerg.l-gb" = gpm_imerg.download.dods,
                    "gpm.imerg.e-gb" = gpm_imerg.download.dods,
                    "cmorphv0.xraw-gb" = local({
                                url.down <- if(.cdtData$GalParams$iridl.src)
                                                cmorph_raw.download.iridl
                                            else
                                                cmorph.download.cpc.ncep

                                switch(.cdtData$GalParams$tstep,
                                       "minute" = cmorph.download.cpc.ncep,
                                       "hourly" = url.down,
                                       "daily" = url.down
                                    )
                            }),
                    "cmorphv1.0bld-gb" = cmorph.download.cpc.ncep,
                    "cmorphrtv0.xbld-gb" = cmorph.download.cpc.ncep,
                    "cmorphv1.0adj-gb" = cmorph.download.cpc.ncep,
                    "cmorphrtv0.xadj-gb" = cmorph.download.cpc.ncep,
                    "cmorphv0.xrt-gb" = cmorph.download.cpc.ncep,

                    #########
                    NULL
                )

    if(is.null(download.fun)) return(-1)
    ret <- download.fun(.cdtData$GalParams)
    return(ret)
}
