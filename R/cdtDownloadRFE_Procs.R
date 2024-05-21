
exec.download_RFE <- function(){
    on.exit({
        tkconfigure(.cdtEnv$tcl$main$win, cursor = '')
        tcl('update')
    })
    tkconfigure(.cdtEnv$tcl$main$win, cursor = 'watch')
    tcl('update')

    download.fun <- switch(.cdtData$GalParams$rfe.src,
                    "tamsatv3.1-af" = download_tamsat_data(.cdtData$GalParams, 'download'),
                    "tamsatv3-af" = download_tamsat_data(.cdtData$GalParams, 'download'),

                    "chirp-gb" = download_chirps_data(.cdtData$GalParams, 'download'),
                    "chirpsv2-gb" = download_chirps_data(.cdtData$GalParams, 'download'),
                    "chirpsv2-af" = chirps.download.ucsb,

                    "cmorphv0.xraw-gb" = download_cmorph_raw_data(.cdtData$GalParams, 'download'),
                    "cmorphv1.0bld-gb" = cmorph.download.cpc.ncep,
                    "cmorphrtv0.xbld-gb" = cmorph.download.cpc.ncep,
                    "cmorphv1.0adj-gb" = cmorph.download.cpc.ncep,
                    "cmorphrtv0.xadj-gb" = cmorph.download.cpc.ncep,
                    "cmorphv0.xrt-gb" = cmorph.download.cpc.ncep,
                    "cmorph2xnrt-gb" = cmorph.download.cpc.ncep,

                    "arc2-af" = download_fews_data(.cdtData$GalParams, 'download'),
                    "rfev2-af" = download_fews_data(.cdtData$GalParams, 'download'),
                    "rfev2-sa" = download_fews_data(.cdtData$GalParams, 'download'),

                    "persiann.pdir-gb" = persiann.download.uci,
                    "persiann.ccs-gb" = persiann.download.uci,
                    "persiann.cdr-gb" = persiann.download.uci,
                    "persiann-gb" = persiann.download.uci,

                    "gpm.imerg.f-gb" = gpm_imerg.download.gesdisc,
                    "gpm.imerg.l-gb" = gpm_imerg.download.gesdisc,
                    "gpm.imerg.e-gb" = gpm_imerg.download.gesdisc,

                    "trmm3b42v7-gb" = trmm3b42v7.download.gesdisc,
                    "trmm3b42rtv7-gb" = trmm3b42v7.download.gesdisc,

                    "gsmap.now-gb" = gsmap.download.jaxa,
                    "gsmap.g.now-gb" = gsmap.download.jaxa,
                    "gsmap.nrtv8-gb" = gsmap.download.jaxa,
                    "gsmap.g.nrtv8-gb" = gsmap.download.jaxa,
                    "gsmap.nrtv7-gb" = gsmap.download.jaxa,
                    "gsmap.g.nrtv7-gb" = gsmap.download.jaxa,
                    "gsmap.nrtv6-gb" = gsmap.download.jaxa,
                    "gsmap.g.nrtv6-gb" = gsmap.download.jaxa,
                    "gsmap.nrtv8.00-gb" = gsmap.download.jaxa,
                    "gsmap.nrtv8.12-gb" = gsmap.download.jaxa,
                    "gsmap.g.nrtv8.00-gb" = gsmap.download.jaxa,
                    "gsmap.g.nrtv8.12-gb" = gsmap.download.jaxa,
                    "gsmap.nrtv7.00-gb" = gsmap.download.jaxa,
                    "gsmap.nrtv7.12-gb" = gsmap.download.jaxa,
                    "gsmap.g.nrtv7.00-gb" = gsmap.download.jaxa,
                    "gsmap.g.nrtv7.12-gb" = gsmap.download.jaxa,
                    "gsmap.nrtv6.00-gb" = gsmap.download.jaxa,
                    "gsmap.nrtv6.12-gb" = gsmap.download.jaxa,
                    "gsmap.g.nrtv6.00-gb" = gsmap.download.jaxa,
                    "gsmap.g.nrtv6.12-gb" = gsmap.download.jaxa,
                    "gsmap.clm-gb" = gsmap.download.jaxa,

                    "mswep.nrt-gb" = mswep.download.gdrive,
                    "mswep.past-gb" = mswep.download.gdrive,
                    "mswep.pastng-gb" = mswep.download.gdrive,

                    #########
                    NULL
                )

    if(is.null(download.fun)) return(-1)
    ret <- download.fun(.cdtData$GalParams)
    return(ret)
}

exec.check_coverage_RFE <- function(){
    on.exit({
        tkconfigure(.cdtEnv$tcl$main$win, cursor = '')
        tcl('update')
    })
    tkconfigure(.cdtEnv$tcl$main$win, cursor = 'watch')
    tcl('update')

    download.fun <- switch(.cdtData$GalParams$rfe.src,
                    "tamsatv3.1-af" = download_tamsat_data(.cdtData$GalParams, 'coverage'),
                    "tamsatv3-af" = download_tamsat_data(.cdtData$GalParams, 'coverage'),

                    "cmorphv0.xraw-gb" = download_cmorph_raw_data(.cdtData$GalParams, 'coverage'),
                    "cmorphv1.0bld-gb" = cmorph.coverage.cpc.ncep,
                    "cmorphrtv0.xbld-gb" = cmorph.coverage.cpc.ncep,
                    "cmorphv1.0adj-gb" = cmorph.coverage.cpc.ncep,
                    "cmorphrtv0.xadj-gb" = cmorph.coverage.cpc.ncep,
                    "cmorphv0.xrt-gb" = cmorph.coverage.cpc.ncep,
                    "cmorph2xnrt-gb" = cmorph.coverage.cpc.ncep,

                    "chirp-gb" = download_chirps_data(.cdtData$GalParams, 'coverage'),
                    "chirpsv2-gb" = download_chirps_data(.cdtData$GalParams, 'coverage'),
                    "chirpsv2-af" = chirps.coverage.ucsb,

                    "arc2-af" = download_fews_data(.cdtData$GalParams, 'coverage'),
                    "rfev2-af" = download_fews_data(.cdtData$GalParams, 'coverage'),
                    "rfev2-sa" = download_fews_data(.cdtData$GalParams, 'coverage'),

                    "persiann.pdir-gb" = persiann.coverage.uci,
                    "persiann.ccs-gb" = persiann.coverage.uci,
                    "persiann.cdr-gb" = persiann.coverage.uci,
                    "persiann-gb" = persiann.coverage.uci,

                    "gpm.imerg.f-gb" = gpm_imerg.coverage.gesdisc,
                    "gpm.imerg.l-gb" = gpm_imerg.coverage.gesdisc,
                    "gpm.imerg.e-gb" = gpm_imerg.coverage.gesdisc,

                    "trmm3b42v7-gb" = trmm3b42v7.coverage.gesdisc,
                    "trmm3b42rtv7-gb" = trmm3b42v7.coverage.gesdisc,

                    "gsmap.now-gb" = gsmap.coverage.jaxa,
                    "gsmap.g.now-gb" = gsmap.coverage.jaxa,
                    "gsmap.nrtv8-gb" = gsmap.coverage.jaxa,
                    "gsmap.g.nrtv8-gb" = gsmap.coverage.jaxa,
                    "gsmap.nrtv7-gb" = gsmap.coverage.jaxa,
                    "gsmap.g.nrtv7-gb" = gsmap.coverage.jaxa,
                    "gsmap.nrtv6-gb" = gsmap.coverage.jaxa,
                    "gsmap.g.nrtv6-gb" = gsmap.coverage.jaxa,
                    "gsmap.nrtv8.00-gb" = gsmap.coverage.jaxa,
                    "gsmap.nrtv8.12-gb" = gsmap.coverage.jaxa,
                    "gsmap.g.nrtv8.00-gb" = gsmap.coverage.jaxa,
                    "gsmap.g.nrtv8.12-gb" = gsmap.coverage.jaxa,
                    "gsmap.nrtv7.00-gb" = gsmap.coverage.jaxa,
                    "gsmap.nrtv7.12-gb" = gsmap.coverage.jaxa,
                    "gsmap.g.nrtv7.00-gb" = gsmap.coverage.jaxa,
                    "gsmap.g.nrtv7.12-gb" = gsmap.coverage.jaxa,
                    "gsmap.nrtv6.00-gb" = gsmap.coverage.jaxa,
                    "gsmap.nrtv6.12-gb" = gsmap.coverage.jaxa,
                    "gsmap.g.nrtv6.00-gb" = gsmap.coverage.jaxa,
                    "gsmap.g.nrtv6.12-gb" = gsmap.coverage.jaxa,
                    "gsmap.clm-gb" = gsmap.coverage.jaxa,

                    "mswep.nrt-gb" = mswep.coverage.gdrive,
                    "mswep.past-gb" = mswep.coverage.gdrive,
                    "mswep.pastng-gb" = mswep.coverage.gdrive,

                    #########
                    NULL
                )

    if(is.null(download.fun)){
        return(list(name = 'Unknown', timestep = 'Unknown'))
    }

    GalParams <- .cdtData$GalParams
    GalParams$date.range$from.file <- FALSE
    GalParams$date.range$start.year <- 2024
    GalParams$date.range$end.year <- 2024
    GalParams$date.range$end.mon <- GalParams$date.range$start.mon
    GalParams$date.range$end.dek <- GalParams$date.range$start.dek
    GalParams$date.range$end.pen <- GalParams$date.range$start.pen
    GalParams$date.range$end.day <- GalParams$date.range$start.day
    GalParams$date.range$hour.day <- GalParams$date.range$hour.day

    ret <- download.fun(GalParams)
    if(!is.null(ret$end)){
        txt_rfe <- paste("RFE :", ret$name)
        txt_tstep <- paste("Temporal resolution :", ret$timestep)
        tstep <- switch(.cdtData$GalParams$tstep,
                 "minute" = local({
                    dates <- as.POSIXct(c(ret$start, ret$end), format = '%Y%m%d%H%M', tz = 'UTC')
                    format(dates, '%Y-%m-%d %H:%M:%S')
                 }),
                 "hourly" = local({
                    dates <- c(ret$start, ret$end)
                    if(.cdtData$GalParams$rfe.src %in% c('cmorphv1.0adj-gb', 'cmorphv0.xraw-gb')){
                        dates[1] <- paste0(dates[1], '00')
                        dates[2] <- paste0(dates[2], '21')
                    }
                    dates <- as.POSIXct(dates, format = '%Y%m%d%H', tz = 'UTC')
                    format(dates, '%Y-%m-%d %H:%M:%S')
                 }),
                 "daily" = local({
                    dates <- as.Date(c(ret$start, ret$end), '%Y%m%d')
                    format(dates, '%Y-%m-%d')
                 }),
                 "pentad" = local({
                    dates <- as.Date(c(ret$start, ret$end), '%Y%m%d')
                    pt <- as.numeric(format(dates, '%d'))
                    paste0(format(dates, '%Y-%m-'), pt)
                 }),
                 "dekadal" = local({
                    dates <- as.Date(c(ret$start, ret$end), '%Y%m%d')
                    dk <- as.numeric(format(dates, '%d'))
                    paste0(format(dates, '%Y-%m-'), dk)
                }),
                 "monthly" = local({
                    dates <- as.Date(paste0(c(ret$start, ret$end), '01'), '%Y%m%d')
                    format(dates, '%Y-%m')
                 })
             )
        txt_start <- paste("Start date :", tstep[1])
        txt_end <- paste("End date :", tstep[2])
        disp <- paste(txt_rfe, txt_tstep, txt_start, txt_end, sep = '\n')

        containertab <- Display_Output_Console_Tab(disp, ret$name, cat)
        ntab <- update.OpenTabs('ctxt', containertab)
        tkselect(.cdtEnv$tcl$main$tknotes, ntab)
    }

    return(ret)
}

download_chirps_data <- function(GalParams, type){
    iridl_fun <- paste0("chirps.", type, ".iridl")
    iridl_fun <- get(iridl_fun, mode = "function")
    ucsb_fun <- paste0("chirps.", type, ".ucsb")
    ucsb_fun <- get(ucsb_fun, mode = "function")
    url_down <- if(GalParams$iridl.src) iridl_fun else ucsb_fun

    switch(GalParams$tstep,
           "daily" = url_down,
           "pentad" = ucsb_fun,
           "dekadal" = url_down,
           "monthly" = url_down
        )
}

download_cmorph_raw_data <- function(GalParams, type){
    iridl_fun <- paste0("cmorph_raw.", type, ".iridl")
    iridl_fun <- get(iridl_fun, mode = "function")
    ncep_fun <- paste0("cmorph.", type, ".cpc.ncep")
    ncep_fun <- get(ncep_fun, mode = "function")
    url_down <- if(GalParams$iridl.src) iridl_fun else ncep_fun

    switch(GalParams$tstep,
           "minute" = ncep_fun,
           "hourly" = url_down,
           "daily" = url_down
        )
}

download_tamsat_data <- function(GalParams, type){
    iridl_fun <- paste0("tamsat.", type, ".iridl")
    iridl_fun <- get(iridl_fun, mode = "function")
    tamsat_fun <- paste0("tamsat.", type, ".reading")
    tamsat_fun <- get(tamsat_fun, mode = "function")
    url_down <- if(GalParams$iridl.src) iridl_fun else tamsat_fun

    switch(GalParams$tstep,
           "daily" = url_down,
           "pentad" = tamsat_fun,
           "dekadal" = url_down,
           "monthly" = url_down
        )
}

download_fews_data <- function(GalParams, type){
    iridl_fun <- paste0("fews.", type, ".iridl")
    iridl_fun <- get(iridl_fun, mode = "function")
    fews_fun <- paste0("fews.", type, ".cpc.noaa")
    fews_fun <- get(fews_fun, mode = "function")
    url_down <- if(GalParams$iridl.src) iridl_fun else fews_fun

    switch(GalParams$tstep,
           "daily" = url_down,
           "dekadal" = iridl_fun,
           "monthly" = iridl_fun
        )
}

