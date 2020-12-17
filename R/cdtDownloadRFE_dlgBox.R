
rfe.product.list <- function(tstep = "dekadal", minhour = NULL){
    ## pwd: TRMM 3B42 v7, TRMM 3B42RT v7
    cbname <- name <- NULL
    ## minute
    if(tstep == "minute"){
        ## 30 min
        if(minhour == 30){
            cbname <- c(
                        # "CMORPH V1.0 CRT (Global)", 
                        "CMORPH RT V0.x ICDR (Global)",
                        "CMORPH V0.x RT (Global)", "CMORPH V0.x RAW (Global)",
                        "GPM L3 IMERG V06 Final (Global)", "GPM L3 IMERG V06 Late (Global)",
                        "GPM L3 IMERG V06 Early (Global)")
            name <- c(
                      # "cmorphv1.0adj-gb", 
                      "cmorphrtv0.xadj-gb", "cmorphv0.xrt-gb",
                      "cmorphv0.xraw-gb", "gpm.imerg.f-gb",
                      "gpm.imerg.l-gb", "gpm.imerg.e-gb")
        }
    }

    ## hourly
    if(tstep == "hourly"){
        ## 1 hour
        if(minhour == 1){
            cbname <- c("PERSIANN (Global)", "CMORPH RT V0.x ICDR (Global)")
            name <- c("persiann-gb", "cmorphrtv0.xadj-gb")
        }

        ## 3 hour
        if(minhour == 3){
            cbname <- c("PERSIANN (Global)", "TRMM 3B42 v7 (Global)", "TRMM 3B42RT v7 (Global)",
                        "CMORPH V1.0 CRT (Global)", "CMORPH V0.x RAW (Global)")
            name <- c("persiann-gb", "trmm3b42v7-gb", "trmm3b42rtv7-gb",
                      "cmorphv1.0adj-gb", "cmorphv0.xraw-gb")
        }

        ## 6 hour
        if(minhour == 6){
            cbname <- c("CHIRPS v2.0 (Africa)", "PERSIANN (Global)")
            name <- c("chirpsv2-af", "persiann-gb")
        }
    }

    ## daily
    if(tstep == "daily"){
        cbname <- c("TAMSAT v3.1 (Africa)", "TAMSAT v3.0 (Africa)", "CHIRP (Global)", "CHIRPS v2.0 (Global)",
                    "NOAA-CPC ARC2 (Africa)", "NOAA-CPC RFEv2 (Africa)", "NOAA-CPC RFEv2 (South Asia)",
                    "PERSIANN-CDR (Global)", "PERSIANN (Global)",
                    "TRMM 3B42 v7 (Global)", "TRMM 3B42RT v7 (Global)",
                    "CMORPH V1.0 BLD (Global)", "CMORPH RT V0.x BLD (Global)",
                    "CMORPH V1.0 CRT (Global)", "CMORPH RT V0.x ICDR (Global)",
                    "CMORPH V0.x RAW (Global)", "GPM L3 IMERG V06 Final (Global)",
                    "GPM L3 IMERG V06 Late (Global)", "GPM L3 IMERG V06 Early (Global)")
        name <- c("tamsatv3.1-af", "tamsatv3-af", "chirp-gb", "chirpsv2-gb", "arc2-af", "rfev2-af", "rfev2-sa",
                  "persianncdr-gb", "persiann-gb", "trmm3b42v7-gb", "trmm3b42rtv7-gb",
                  "cmorphv1.0bld-gb", "cmorphrtv0.xbld-gb", "cmorphv1.0adj-gb", "cmorphrtv0.xadj-gb",
                  "cmorphv0.xraw-gb", "gpm.imerg.f-gb", "gpm.imerg.l-gb", "gpm.imerg.e-gb")
    }

    ## pentad
    if(tstep == "pentad"){
        cbname <- c("TAMSAT v3.1 (Africa)", "TAMSAT v3.0 (Africa)", "CHIRP (Global)", "CHIRPS v2.0 (Global)")
        name <- c("tamsatv3.1-af", "tamsatv3-af", "chirp-gb", "chirpsv2-gb")
    }

    ## dekad
    if(tstep == "dekadal"){
        cbname <- c("TAMSAT v3.1 (Africa)", "TAMSAT v3.0 (Africa)", "CHIRP (Global)", "CHIRPS v2.0 (Global)",
                    "NOAA-CPC ARC2 (Africa)", "NOAA-CPC RFEv2 (Africa)", "NOAA-CPC RFEv2 (South Asia)")
        name <- c("tamsatv3.1-af", "tamsatv3-af", "chirp-gb", "chirpsv2-gb", "arc2-af", "rfev2-af", "rfev2-sa")
    }

    ## monthly
    if(tstep == "monthly"){
        cbname <- c("TAMSAT v3.1 (Africa)", "TAMSAT v3.0 (Africa)", "CHIRP (Global)", "CHIRPS v2.0 (Global)",
                    "ARC2 (Africa)", "PERSIANN-CDR (Global)", "PERSIANN (Global)",
                    "GPM L3 IMERG V06 Final (Global)")
        name <- c("tamsatv3.1-af", "tamsatv3-af", "chirp-gb", "chirpsv2-gb", "arc2-af",
                  "persianncdr-gb", "persiann-gb", "gpm.imerg.f-gb")
    }

    return(list(cbname = cbname, name = name))
}

################################################

rfe.product.source <- function(src, tstep, minhour = NULL){
    ## minute
    if(tstep == "minute"){
        # if(minhour == 15){

        # }

        if(minhour == 30){
            urls <- switch(src,
                # "cmorphv1.0adj-gb" = c("ftp://ftp.cpc.ncep.noaa.gov/precip/CMORPH_V1.0/CRT/8km-30min",
                #                        "ftp://ftp.cpc.ncep.noaa.gov/precip/CMORPH_V1.0/CMORPH_V1.0_README.txt"),
                "cmorphrtv0.xadj-gb" = c("ftp://ftp.cpc.ncep.noaa.gov/precip/CMORPH_RT/ICDR/8km-30min",
                                         "ftp://ftp.cpc.ncep.noaa.gov/precip/CMORPH_V0.x/CMORPH_V0.x_README.txt"),
                "cmorphv0.xrt-gb" = "ftp://ftp.cpc.ncep.noaa.gov/precip/CMORPH_RT/GLOBE/data",
                "cmorphv0.xraw-gb" = c("ftp://ftp.cpc.ncep.noaa.gov/precip/CMORPH_V0.x/RAW/8km-30min",
                                       "ftp://ftp.cpc.ncep.noaa.gov/precip/CMORPH_V0.x/CMORPH_V0.x_README.txt"),
                "gpm.imerg.f-gb" = c("https://disc.gsfc.nasa.gov/datasets/GPM_3IMERGHH_V06/summary?keywords=IMERG",
                                     "https://pmm.nasa.gov/data-access/downloads/gpm"),
                "gpm.imerg.l-gb" = c("https://disc.gsfc.nasa.gov/datasets/GPM_3IMERGHHL_V06/summary?keywords=IMERG",
                                     "https://pmm.nasa.gov/data-access/downloads/gpm"),
                "gpm.imerg.e-gb" = c("https://disc.gsfc.nasa.gov/datasets/GPM_3IMERGHHE_V06/summary?keywords=IMERG",
                                     "https://pmm.nasa.gov/data-access/downloads/gpm")
            )
        }
    }

    ## hourly
    if(tstep == "hourly"){
        ## 1 hour
        if(minhour == 1){
            urls <- switch(src,
                "persiann-gb" = c("https://chrsdata.eng.uci.edu",
                                  "ftp://persiann.eng.uci.edu/CHRSdata/PERSIANN/hrly",
                                  "ftp://persiann.eng.uci.edu/CHRSdata/PERSIANN/hrly/readme.hrly"),
                "cmorphrtv0.xadj-gb" = c("ftp://ftp.cpc.ncep.noaa.gov/precip/CMORPH_RT/ICDR/0.25deg-HLY",
                                         "ftp://ftp.cpc.ncep.noaa.gov/precip/CMORPH_V0.x/CMORPH_V0.x_README.txt")
            )
        }

        ## 3 hour
        if(minhour == 3){
            urls <- switch(src,
                "persiann-gb" = c("https://chrsdata.eng.uci.edu",
                                  "ftp://persiann.eng.uci.edu/CHRSdata/PERSIANN/3hrly",
                                  "ftp://persiann.eng.uci.edu/CHRSdata/PERSIANN/3hrly/readme.3hrly"),
                "trmm3b42v7-gb" = "https://disc.gsfc.nasa.gov/datasets/TRMM_3B42_V7/summary?keywords=TRMM_3B42",
                "trmm3b42rtv7-gb" = "https://disc.gsfc.nasa.gov/datasets/TRMM_3B42RT_V7/summary?keywords=TRMM_3B42",
                "cmorphv1.0adj-gb" = c("ftp://ftp.cpc.ncep.noaa.gov/precip/CMORPH_V1.0/CRT/0.25deg-3HLY",
                                       "ftp://ftp.cpc.ncep.noaa.gov/precip/CMORPH_V1.0/CMORPH_V1.0_README.txt"),
                "cmorphv0.xraw-gb" = c("https://iridl.ldeo.columbia.edu/SOURCES/.NOAA/.NCEP/.CPC/.CMORPH/.V0px/.RAW/.3-hourly/.prcp",
                                       "ftp://ftp.cpc.ncep.noaa.gov/precip/CMORPH_V0.x/RAW/0.25deg-3HLY",
                                       "ftp://ftp.cpc.ncep.noaa.gov/precip/CMORPH_V0.x/CMORPH_V0.x_README.txt")
            )
        }

        ## 6 hour
        if(minhour == 6){
            urls <- switch(src,
                "chirpsv2-af" = c("ftp://ftp.chc.ucsb.edu/pub/org/chg/products/CHIRPS-2.0/africa_6-hourly",
                                  "ftp://ftp.chc.ucsb.edu/pub/org/chg/products/CHIRPS-2.0/africa_6-hourly/README.6-hourly.txt",
                                  "ftp://ftp.chc.ucsb.edu/pub/org/chg/products/CHIRPS-2.0/africa_6-hourly/p1_bin/extra_step/readme_chirps6hrly.txt"
                                ),
                "persiann-gb" = c("https://chrsdata.eng.uci.edu",
                                  "ftp://persiann.eng.uci.edu/CHRSdata/PERSIANN/6hrly")
            )
        }
    }

    ## daily
    if(tstep == "daily"){
        urls <- switch(src,
            "tamsatv3.1-af" = c("http://iridl.ldeo.columbia.edu/SOURCES/.Reading/.Meteorology/.TAMSAT/.TARCAT/.v3p1/.daily/.rfe",
                                "http://www.tamsat.org.uk/sites/data-download/index.html"),
            "tamsatv3-af" = c("http://iridl.ldeo.columbia.edu/SOURCES/.Reading/.Meteorology/.TAMSAT/.TARCAT/.v3p0/.daily/.rfe",
                              "http://www.tamsat.org.uk/data"),
            "chirp-gb" = c("http://iridl.ldeo.columbia.edu/SOURCES/.UCSB/.CHIRP/.v1p0/.daily/.prcp",
                           "ftp://ftp.chc.ucsb.edu/pub/org/chg/products/CHIRP/daily"),
            "chirpsv2-gb" = c("http://iridl.ldeo.columbia.edu/SOURCES/.UCSB/.CHIRPS/.v2p0/.daily-improved/.global/.0p05/.prcp",
                              "ftp://ftp.chc.ucsb.edu/pub/org/chg/products/CHIRPS-2.0/global_daily/tifs/p05"),
            "arc2-af" = c("https://iridl.ldeo.columbia.edu/SOURCES/.NOAA/.NCEP/.CPC/.FEWS/.Africa/.DAILY/.ARC2/.daily/.est_prcp",
                          "ftp://ftp.cpc.ncep.noaa.gov/fews/fewsdata/africa/arc2"),
            "rfev2-af" = c("https://iridl.ldeo.columbia.edu/SOURCES/.NOAA/.NCEP/.CPC/.FEWS/.Africa/.DAILY/.RFEv2/.est_prcp",
                           "ftp://ftp.cpc.ncep.noaa.gov/fews/fewsdata/africa/rfe2",
                           "ftp://ftp.cpc.ncep.noaa.gov/fews/fewsdata/africa/rfe2/RFE_readme.txt"),
            "rfev2-sa" = c("http://iridl.ldeo.columbia.edu/SOURCES/.NOAA/.NCEP/.CPC/.FEWS/.SAsia/.RFEv2/.DAILY/.est_prcp",
                           "ftp://ftp.cpc.ncep.noaa.gov/fews/S.Asia",
                           "ftp://ftp.cpc.ncep.noaa.gov/fews/S.Asia/SOUTH_ASIA_README.txt"),
            "persianncdr-gb" = c("https://chrsdata.eng.uci.edu",
                                 "ftp://persiann.eng.uci.edu/CHRSdata/PERSIANN-CDR/daily"),
            "persiann-gb" = c("https://chrsdata.eng.uci.edu",
                              "ftp://persiann.eng.uci.edu/CHRSdata/PERSIANN/daily"),
            "trmm3b42v7-gb" = "https://disc.gsfc.nasa.gov/datasets/TRMM_3B42_Daily_V7/summary?keywords=TRMM_3B42",
            "trmm3b42rtv7-gb" = "https://disc.gsfc.nasa.gov/datasets/TRMM_3B42RT_Daily_V7/summary?keywords=TRMM_3B42",
            "cmorphv1.0bld-gb" = c("ftp://ftp.cpc.ncep.noaa.gov/precip/CMORPH_V1.0/BLD/0.25deg-DLY_EOD/GLB",
                                   "ftp://ftp.cpc.ncep.noaa.gov/precip/CMORPH_V1.0/CMORPH_V1.0_README.txt"),
            "cmorphrtv0.xbld-gb" = "ftp://ftp.cpc.ncep.noaa.gov/precip/CMORPH_RT/BLD",
            "cmorphv1.0adj-gb" = c("ftp://ftp.cpc.ncep.noaa.gov/precip/CMORPH_V1.0/CRT/0.25deg-DLY_00Z",
                                   "ftp://ftp.cpc.ncep.noaa.gov/precip/CMORPH_V1.0/CMORPH_V1.0_README.txt"),
            "cmorphrtv0.xadj-gb" = c("ftp://ftp.cpc.ncep.noaa.gov/precip/CMORPH_RT/ICDR/0.25deg-DLY_00Z",
                                     "ftp://ftp.cpc.ncep.noaa.gov/precip/CMORPH_V0.x/CMORPH_V0.x_README.txt"),
            "cmorphv0.xraw-gb" = c("https://iridl.ldeo.columbia.edu/SOURCES/.NOAA/.NCEP/.CPC/.CMORPH/.V0px/.RAW/.daily/.prcp",
                                   "ftp://ftp.cpc.ncep.noaa.gov/precip/CMORPH_V0.x/RAW/0.25deg-DLY_00Z",
                                   "ftp://ftp.cpc.ncep.noaa.gov/precip/CMORPH_V0.x/CMORPH_V0.x_README.txt"),
            "gpm.imerg.f-gb" = c("https://disc.gsfc.nasa.gov/datasets/GPM_3IMERGDF_V06/summary?keywords=IMERG",
                                 "https://pmm.nasa.gov/data-access/downloads/gpm"),
            "gpm.imerg.l-gb" = c("https://disc.gsfc.nasa.gov/datasets/GPM_3IMERGDL_V06/summary?keywords=IMERG",
                                 "https://pmm.nasa.gov/data-access/downloads/gpm"),
            "gpm.imerg.e-gb" = c("https://disc.gsfc.nasa.gov/datasets/GPM_3IMERGDE_V06/summary?keywords=IMERG",
                                 "https://pmm.nasa.gov/data-access/downloads/gpm")
        )
    }

    ## pentad
    if(tstep == "pentad"){
        urls <- switch(src,
            "tamsatv3.1-af" = "http://www.tamsat.org.uk/sites/data-download/index.html",
            "tamsatv3-af" = "http://www.tamsat.org.uk/data",
            "chirp-gb" = "ftp://ftp.chc.ucsb.edu/pub/org/chg/products/CHIRP/pentads",
            "chirpsv2-gb" = "ftp://ftp.chc.ucsb.edu/pub/org/chg/products/CHIRPS-2.0/global_pentad/tifs"
        )
    }

    ## dekad
    if(tstep == "dekadal"){
        urls <- switch(src,
            "tamsatv3.1-af" = c("http://iridl.ldeo.columbia.edu/SOURCES/.Reading/.Meteorology/.TAMSAT/.TARCAT/.v3p1/.dekadal/.rfe",
                                "http://www.tamsat.org.uk/sites/data-download/index.html"),
            "tamsatv3-af" = c("http://iridl.ldeo.columbia.edu/SOURCES/.Reading/.Meteorology/.TAMSAT/.TARCAT/.v3p0/.dekadal/.rfe",
                              "http://www.tamsat.org.uk/data"),
            "chirp-gb" = c("http://iridl.ldeo.columbia.edu/SOURCES/.UCSB/.CHIRP/.v1p0/.dekad/.prcp",
                           "ftp://ftp.chc.ucsb.edu/pub/org/chg/products/CHIRP/dekads"),
            "chirpsv2-gb" = c("http://iridl.ldeo.columbia.edu/SOURCES/.UCSB/.CHIRPS/.v2p0/.dekad/.prcp",
                              "ftp://ftp.chc.ucsb.edu/pub/org/chg/products/CHIRPS-2.0/global_dekad/tifs"),
            "arc2-af" = c("https://iridl.ldeo.columbia.edu/SOURCES/.NOAA/.NCEP/.CPC/.FEWS/.Africa/.TEN-DAY/.ARC2/.est_prcp",
                          "ftp://ftp.cpc.ncep.noaa.gov/fews/fewsdata/africa/arc2"),
            "rfev2-af" = c("https://iridl.ldeo.columbia.edu/SOURCES/.NOAA/.NCEP/.CPC/.FEWS/.Africa/.TEN-DAY/.RFEv2/.est_prcp",
                           "ftp://ftp.cpc.ncep.noaa.gov/fews/fewsdata/africa"),
            "rfev2-sa" = c("http://iridl.ldeo.columbia.edu/SOURCES/.NOAA/.NCEP/.CPC/.FEWS/.SAsia/.RFEv2/.DAILY/.est_prcp/1.0/dekadalAverage/T/differential_mul",
                           "ftp://ftp.cpc.ncep.noaa.gov/fews/S.Asia")
        )
    }

    ## monthly
    if(tstep == "monthly"){
        urls <- switch(src,
            "tamsatv3.1-af" = c("http://iridl.ldeo.columbia.edu/SOURCES/.Reading/.Meteorology/.TAMSAT/.TARCAT/.v3p1/.monthly/.rfe",
                                "http://www.tamsat.org.uk/sites/data-download/index.html"),
            "tamsatv3-af" = c("http://iridl.ldeo.columbia.edu/SOURCES/.Reading/.Meteorology/.TAMSAT/.TARCAT/.v3p0/.monthly/.rfe",
                              "http://www.tamsat.org.uk/data"),
            "chirp-gb" = c("http://iridl.ldeo.columbia.edu/SOURCES/.UCSB/.CHIRP/.v1p0/.dekad/.prcp/monthlyAverage/3.0/mul",
                           "ftp://ftp.chc.ucsb.edu/pub/org/chg/products/CHIRP/monthly"),
            "chirpsv2-gb" = c("http://iridl.ldeo.columbia.edu/SOURCES/.UCSB/.CHIRPS/.v2p0/.monthly/.global/.precipitation",
                              "ftp://ftp.chc.ucsb.edu/pub/org/chg/products/CHIRPS-2.0/global_monthly/tifs"),
            "arc2-af" = c("https://iridl.ldeo.columbia.edu/SOURCES/.NOAA/.NCEP/.CPC/.FEWS/.Africa/.DAILY/.ARC2/.monthly/.est_prcp",
                          "ftp://ftp.cpc.ncep.noaa.gov/fews/fewsdata/africa/arc2"),
            "persianncdr-gb" = c("https://chrsdata.eng.uci.edu",
                                 "ftp://persiann.eng.uci.edu/CHRSdata/PERSIANN-CDR/mthly"),
            "persiann-gb" = c("https://chrsdata.eng.uci.edu",
                              "ftp://persiann.eng.uci.edu/CHRSdata/PERSIANN/mthly"),
            "gpm.imerg.f-gb" = c("https://disc.gsfc.nasa.gov/datasets/GPM_3IMERGM_V06/summary?keywords=IMERG",
                                 "https://pmm.nasa.gov/data-access/downloads/gpm")
        )
    }

    tmpf <- tempfile(fileext = ".html")
    cat("<html>\n<body>\n", file = tmpf)
    cat("<h3>Data source and documentation</h3>\n", file = tmpf, append = TRUE)
    cat("<p><i> Click on the links:</i></p>\n", file = tmpf, append = TRUE)
    cat("<ul>\n", file = tmpf, append = TRUE)
    for(lk in urls)
        cat(paste0("<li><a href=",  lk, ">",  lk, "</a></li>\n"), file = tmpf, append = TRUE)
    cat("</ul>\n", file = tmpf, append = TRUE)
    cat("</body>\n</html>\n", file = tmpf, append = TRUE)

    return(tmpf)
}

rfe.iridl.ulrs <- function(src, tstep){
    ret <- FALSE
    if(src == "tamsatv3.1-af" & (tstep %in% c("daily", "dekadal", "monthly"))) ret <- TRUE
    if(src == "tamsatv3-af" & (tstep %in% c("daily", "dekadal", "monthly"))) ret <- TRUE
    if(src == "chirp-gb" & (tstep %in% c("daily", "dekadal", "monthly"))) ret <- TRUE
    if(src == "chirpsv2-gb" & (tstep %in% c("daily", "dekadal", "monthly"))) ret <- TRUE
    if(src == "arc2-af" & (tstep %in% c("daily", "dekadal", "monthly"))) ret <- TRUE
    if(src == "rfev2-af" & (tstep %in% c("daily", "dekadal"))) ret <- TRUE
    if(src == "rfev2-sa" & (tstep %in% c("daily", "dekadal"))) ret <- TRUE
    if(src == "cmorphv0.xraw-gb" & (tstep %in% c("hourly", "daily"))) ret <- TRUE

    return(ret)
}

rfe.need.usrpwd <- function(src){
    usrpwd <- FALSE
    urllog <- ""
    if(src %in% c("trmm3b42v7-gb", "trmm3b42rtv7-gb", "gpm.imerg.f-gb",
                  "gpm.imerg.l-gb", "gpm.imerg.e-gb"))
    {
        usrpwd <- TRUE
        urllog <- "https://urs.earthdata.nasa.gov"
    }
    list(usrpwd = usrpwd, urllog = urllog)
}

################################################

download_RFE <- function(){
    if(WindowsOS()){
        largeur0 <- 54
        largeur1 <- 27
        largeur2 <- 18
        largeur3 <- 33
        largeur4 <- 22
        largeur5 <- 15
    }else{
        largeur0 <- 52
        largeur1 <- 27
        largeur2 <- 18
        largeur3 <- 33
        largeur4 <- 22
        largeur5 <- 15
    }

    xml.dlg <- file.path(.cdtDir$dirLocal, "languages", "cdtDownloadRFE_dlgBox.xml")
    lang.dlg <- cdtLanguageParse(xml.dlg, .cdtData$Config$lang.iso)

    #########
    tt <- tktoplevel()
    tkgrab.set(tt)
    tkfocus(tt)

    frGrd0 <- tkframe(tt, relief = 'raised', borderwidth = 2)
    frGrd1 <- tkframe(tt)

    ###################################################

    frRFE <- tkframe(frGrd0, relief = 'sunken', bd = 2)

    timeStep <- tclVar()
    CbperiodVAL <- .cdtEnv$tcl$lang$global[['combobox']][['1']][1:6]
    periodVAL <- c('minute', 'hourly', 'daily', 'pentad', 'dekadal', 'monthly')
    tclvalue(timeStep) <- CbperiodVAL[periodVAL %in% .cdtData$GalParams$tstep]

    if(.cdtData$GalParams$tstep %in% c("minute", "hourly")){
        if(.cdtData$GalParams$tstep == "minute"){
            # CbminhourVAL <- c(15, 30)
            CbminhourVAL <- 30
            minhour.txt <- lang.dlg[['label']][['1']]
        }
        if(.cdtData$GalParams$tstep == "hourly"){
            CbminhourVAL <- c(1, 3, 6)
            minhour.txt <- lang.dlg[['label']][['2']]
        }

        minhour.val <- .cdtData$GalParams$minhour

        if(is.na(minhour.val)){
            minhour.val <- CbminhourVAL[1]
        }else{
            if(!minhour.val %in% CbminhourVAL)
                minhour.val <- CbminhourVAL[1]
        }

        minhour.state <- "normal"
    }else{
        CbminhourVAL <- ""
        minhour.val <- ""
        minhour.state <- "disabled"
        minhour.txt <- ""
    }

    minhour.tclVar <- tclVar(minhour.val)
    minhour.txtVar <- tclVar(minhour.txt)

    #################

    RFESrc <- tclVar()
    rfedata <- rfe.product.list(.cdtData$GalParams$tstep, .cdtData$GalParams$minhour)
    CbRFEVAL <- rfedata$cbname
    RFEVAL <- rfedata$name
    tclvalue(RFESrc) <- CbRFEVAL[RFEVAL %in% .cdtData$GalParams$rfe.src]

    #################

    need.pwd <- rfe.need.usrpwd(.cdtData$GalParams$rfe.src)
    statepwd <- if(need.pwd$usrpwd) "normal" else "disabled"

    url.log <- tclVar(need.pwd$urllog)
    username <- tclVar(.cdtData$GalParams$login$usr)
    password <- tclVar(.cdtData$GalParams$login$pwd)

    #################

    if(rfe.iridl.ulrs(.cdtData$GalParams$rfe.src, .cdtData$GalParams$tstep)){
        stateiridl <- "normal"
        valiridl <- .cdtData$GalParams$iridl.src
    }else{
        stateiridl <- "disabled"
        valiridl <- FALSE
    }
    iridl.src <- tclVar(valiridl)

    #################

    txt.tres <- tklabel(frRFE, text = lang.dlg[['label']][['3']], anchor = 'w', justify = 'left')
    cb.tres <- ttkcombobox(frRFE, values = CbperiodVAL, textvariable = timeStep, width = largeur2)
    txt.sat <- tklabel(frRFE, text = lang.dlg[['label']][['4']], anchor = 'w', justify = 'left')
    cb.sat <- ttkcombobox(frRFE, values = CbRFEVAL, textvariable = RFESrc, width = largeur3)

    cb.mhI <- ttkcombobox(frRFE, values = CbminhourVAL, textvariable = minhour.tclVar, width = 3, state = minhour.state)
    txt.mhI <- tklabel(frRFE, text = tclvalue(minhour.txtVar), textvariable = minhour.txtVar, anchor = 'w', justify = 'left')
    bt.range <- ttkbutton(frRFE, text = lang.dlg[['button']][['3']])

    txt.log1 <- tklabel(frRFE, text = lang.dlg[['label']][['5']], anchor = 'e', justify = 'right')
    txt.log2 <- tklabel(frRFE, text = tclvalue(url.log), textvariable = url.log, anchor = 'w', justify = 'left')

    frUSRPWD <- tkframe(frRFE)
    txt.usr <- tklabel(frUSRPWD, text = lang.dlg[['label']][['6']], anchor = 'e', justify = 'right')
    en.usr <- tkentry(frUSRPWD, textvariable = username, state = statepwd, width = largeur4, justify = "left")
    txt.pwd <- tklabel(frUSRPWD, text = lang.dlg[['label']][['7']], anchor = 'e', justify = 'right')
    en.pwd <- tkentry(frUSRPWD, textvariable = password, show = "*", state = statepwd, width = largeur5, justify = "left")

    bt.info <- ttkbutton(frRFE, text = lang.dlg[['button']][['4']])

    chk.iridl <- tkcheckbutton(frRFE, variable = iridl.src, text = lang.dlg[['label']][['14']], anchor = 'w', justify = 'left', state = stateiridl)

    #################

    tkconfigure(bt.range, command = function(){
        tstep <- periodVAL[CbperiodVAL %in% str_trim(tclvalue(timeStep))]
        tcl('wm', 'attributes', tt, topmost = FALSE)
        .cdtData$GalParams[["date.range"]] <- getInfoDateRange(tt, .cdtData$GalParams[["date.range"]], tstep)
        tcl('wm', 'attributes', tt, topmost = TRUE)
    })

    tkconfigure(bt.info, command = function(){
        tstep <- periodVAL[CbperiodVAL %in% str_trim(tclvalue(timeStep))]
        minhour <- as.numeric(str_trim(tclvalue(minhour.tclVar)))
        src <- RFEVAL[CbRFEVAL %in% str_trim(tclvalue(RFESrc))]
        urls <- rfe.product.source(src, tstep, minhour)
        utils::browseURL(paste0('file://', urls))
    })

    #################

    tkgrid(txt.usr, row = 0, column = 0, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
    tkgrid(en.usr, row = 0, column = 1, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
    tkgrid(txt.pwd, row = 0, column = 2, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
    tkgrid(en.pwd, row = 0, column = 3, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)

    #################

    tkgrid(txt.tres, row = 0, column = 0, sticky = 'we', rowspan = 1, columnspan = 4, padx = 1, pady = 1, ipadx = 1, ipady = 1)
    tkgrid(cb.tres, row = 1, column = 0, sticky = 'we', rowspan = 1, columnspan = 4, padx = 1, pady = 1, ipadx = 1, ipady = 1)
    tkgrid(txt.sat, row = 0, column = 4, sticky = 'we', rowspan = 1, columnspan = 4, padx = 1, pady = 1, ipadx = 1, ipady = 1)
    tkgrid(cb.sat, row = 1, column = 4, sticky = 'we', rowspan = 1, columnspan = 4, padx = 1, pady = 1, ipadx = 1, ipady = 1)

    tkgrid(cb.mhI, row = 2, column = 2, sticky = 'e', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
    tkgrid(txt.mhI, row = 2, column = 3, sticky = 'w', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)

    tkgrid(bt.range, row = 2, column = 4, sticky = 'we', rowspan = 1, columnspan = 4, padx = 1, pady = 1, ipadx = 1, ipady = 1)

    tkgrid(txt.log1, row = 4, column = 0, sticky = 'we', rowspan = 1, columnspan = 2, padx = 1, pady = 3, ipadx = 1, ipady = 1)
    tkgrid(txt.log2, row = 4, column = 2, sticky = 'we', rowspan = 1, columnspan = 6, padx = 1, pady = 3, ipadx = 1, ipady = 1)
    tkgrid(frUSRPWD, row = 5, column = 0, sticky = 'we', rowspan = 1, columnspan = 8, padx = 1, pady = 1, ipadx = 1, ipady = 1)

    tkgrid(bt.info, row = 6, column = 3, sticky = 'we', rowspan = 1, columnspan = 4, padx = 1, pady = 5, ipadx = 1, ipady = 1)

    tkgrid(chk.iridl, row = 7, column = 0, sticky = 'we', rowspan = 1, columnspan = 8, padx = 1, pady = 2, ipadx = 1, ipady = 1)

    helpWidget(cb.tres, lang.dlg[['tooltip']][['1']], lang.dlg[['status']][['1']])
    helpWidget(cb.mhI, lang.dlg[['tooltip']][['2']], lang.dlg[['status']][['2']])
    helpWidget(cb.sat, lang.dlg[['tooltip']][['2a']], lang.dlg[['status']][['2a']])
    helpWidget(bt.range, lang.dlg[['tooltip']][['2b']], lang.dlg[['status']][['2b']])

    ######################

    tkbind(cb.tres, "<<ComboboxSelected>>", function(){
        rfe.src <- RFEVAL[CbRFEVAL %in% str_trim(tclvalue(RFESrc))]
        tstep <- periodVAL[CbperiodVAL %in% str_trim(tclvalue(timeStep))]
        minhour.val <- as.numeric(str_trim(tclvalue(minhour.tclVar)))

        ########
        ## minute & hourly
        if(tstep %in% c("minute", "hourly")){
            if(tstep == "minute"){
                # CbminhourVAL <- c(15, 30)
                CbminhourVAL <- 30
                minhour.txt <- lang.dlg[['label']][['1']]
            }
            if(tstep == "hourly"){
                CbminhourVAL <- c(1, 3, 6)
                minhour.txt <- lang.dlg[['label']][['2']]
            }

            if(is.na(minhour.val)){
                minhour.val <- CbminhourVAL[1]
            }else{
                if(!minhour.val %in% CbminhourVAL)
                    minhour.val <- CbminhourVAL[1]
            }

            minhour.state <- "normal"
        }else{
            CbminhourVAL <- ""
            minhour.val <- ""
            minhour.state <- "disabled"
            minhour.txt <- ""
        }

        tkconfigure(cb.mhI, values = CbminhourVAL, state = minhour.state)
        tclvalue(minhour.tclVar) <- minhour.val
        tclvalue(minhour.txtVar) <- minhour.txt

        ########
        rfedata <- rfe.product.list(tstep, minhour.val)
        CbRFEVAL <<- rfedata$cbname
        RFEVAL <<- rfedata$name
 
        tkconfigure(cb.sat, values = CbRFEVAL)
        if(!str_trim(tclvalue(RFESrc)) %in% CbRFEVAL) tclvalue(RFESrc) <- CbRFEVAL[1]

        ########
        need.pwd <- rfe.need.usrpwd(rfe.src)
        statepwd <- if(need.pwd$usrpwd) "normal" else "disabled"

        tclvalue(url.log) <- need.pwd$urllog
        tkconfigure(en.usr, state = statepwd)
        tkconfigure(en.pwd, state = statepwd)

        ########
        if(rfe.iridl.ulrs(rfe.src, tstep)){
            stateiridl <- "normal"
            valiridl <- tclvalue(iridl.src)
        }else{
            stateiridl <- "disabled"
            valiridl <- FALSE
        }
        tclvalue(iridl.src) <- valiridl
        tkconfigure(chk.iridl, state = stateiridl)
    })

    ####
    tkbind(cb.mhI, "<<ComboboxSelected>>", function(){
        tstep <- periodVAL[CbperiodVAL %in% str_trim(tclvalue(timeStep))]
        rfe.src <- RFEVAL[CbRFEVAL %in% str_trim(tclvalue(RFESrc))]
        minhour.val <- as.numeric(str_trim(tclvalue(minhour.tclVar)))

        ########
        rfedata <- rfe.product.list(tstep, minhour.val)
        CbRFEVAL <<- rfedata$cbname
        RFEVAL <<- rfedata$name

        tkconfigure(cb.sat, values = CbRFEVAL)
        if(!str_trim(tclvalue(RFESrc)) %in% CbRFEVAL) tclvalue(RFESrc) <- CbRFEVAL[1]

        ########
        need.pwd <- rfe.need.usrpwd(rfe.src)
        statepwd <- if(need.pwd$usrpwd) "normal" else "disabled"

        tclvalue(url.log) <- need.pwd$urllog
        tkconfigure(en.usr, state = statepwd)
        tkconfigure(en.pwd, state = statepwd)
    })

    ####
    tkbind(cb.sat, "<<ComboboxSelected>>", function(){
        rfe.src <- RFEVAL[CbRFEVAL %in% str_trim(tclvalue(RFESrc))]
        tstep <- periodVAL[CbperiodVAL %in% str_trim(tclvalue(timeStep))]

        ########
        need.pwd <- rfe.need.usrpwd(rfe.src)
        statepwd <- if(need.pwd$usrpwd) "normal" else "disabled"

        tclvalue(url.log) <- need.pwd$urllog
        tkconfigure(en.usr, state = statepwd)
        tkconfigure(en.pwd, state = statepwd)

        ########
        if(rfe.iridl.ulrs(rfe.src, tstep)){
            stateiridl <- "normal"
            valiridl <- tclvalue(iridl.src)
        }else{
            stateiridl <- "disabled"
            valiridl <- FALSE
        }
        tclvalue(iridl.src) <- valiridl
        tkconfigure(chk.iridl, state = stateiridl)
    })

    ###################################################

    frRegion <- tkframe(frGrd0, relief = 'sunken', bd = 2)

    minLon <- tclVar(.cdtData$GalParams$bbox$minlon)
    maxLon <- tclVar(.cdtData$GalParams$bbox$maxlon)
    minLat <- tclVar(.cdtData$GalParams$bbox$minlat)
    maxLat <- tclVar(.cdtData$GalParams$bbox$maxlat)

    fr_grd <- ttklabelframe(frRegion, text = lang.dlg[['label']][['8']], relief = "groove", borderwidth = 2)

    grd_llon <- tklabel(fr_grd, text = lang.dlg[['label']][['9']], anchor = 'e', justify = 'right', width = largeur1)
    grd_llat <- tklabel(fr_grd, text = lang.dlg[['label']][['10']], anchor = 'e', justify = 'right')
    grd_lb1 <- tklabel(fr_grd, text = lang.dlg[['label']][['11']])
    grd_lb2 <- tklabel(fr_grd, text = lang.dlg[['label']][['12']])
    grd_vlon1 <- tkentry(fr_grd, width = 8, justify = "right", textvariable = minLon)
    grd_vlon2 <- tkentry(fr_grd, width = 8, justify = "right", textvariable = maxLon)
    grd_vlat1 <- tkentry(fr_grd, width = 8, justify = "right", textvariable = minLat)
    grd_vlat2 <- tkentry(fr_grd, width = 8, justify = "right", textvariable = maxLat)

    tkgrid(grd_lb1, row = 0, column = 1, sticky = "ew")
    tkgrid(grd_lb2, row = 0, column = 2, sticky = "ew")
    tkgrid(grd_llon, row = 1, column = 0, sticky = "ew")
    tkgrid(grd_vlon1, row = 1, column = 1, sticky = "ew")
    tkgrid(grd_vlon2, row = 1, column = 2, sticky = "ew")
    tkgrid(grd_llat, row = 2, column = 0, sticky = "ew")
    tkgrid(grd_vlat1, row = 2, column = 1, sticky = "ew")
    tkgrid(grd_vlat2, row = 2, column = 2, sticky = "ew")

    tkgrid(fr_grd, row = 0, column = 0, sticky = "ew", padx = 5, pady = 5)

    helpWidget(grd_vlon1, lang.dlg[['tooltip']][['3']], lang.dlg[['status']][['3']])
    helpWidget(grd_vlon2, lang.dlg[['tooltip']][['4']], lang.dlg[['status']][['4']])
    helpWidget(grd_vlat1, lang.dlg[['tooltip']][['5']], lang.dlg[['status']][['5']])
    helpWidget(grd_vlat2, lang.dlg[['tooltip']][['6']], lang.dlg[['status']][['6']])

    ###################################################

    frDirsave <- tkframe(frGrd0, relief = 'sunken', bd = 2)

    dir2save <- tclVar(.cdtData$GalParams$dir2save)

    txt.dir.save <- tklabel(frDirsave, text = lang.dlg[['label']][['13']], anchor = 'w', justify = 'left')
    en.dir.save <- tkentry(frDirsave, textvariable = dir2save, width = largeur0)
    bt.dir.save <- tkbutton(frDirsave, text = "...")

    ###
    tkconfigure(bt.dir.save, command = function(){
        tcl('wm', 'attributes', tt, topmost = FALSE)
        dir2savepth <- tk_choose.dir(.cdtData$GalParams$dir2save, "")
        tcl('wm', 'attributes', tt, topmost = TRUE)
        if(is.na(dir2savepth)) tclvalue(dir2save) <- .cdtData$GalParams$dir2save
        else{
            dir.create(dir2savepth, showWarnings = FALSE, recursive = TRUE)
            tclvalue(dir2save) <- dir2savepth
        }
    })

    ###
    tkgrid(txt.dir.save, row = 0, column = 0, sticky = 'we', rowspan = 1, columnspan = 2, padx = 1, pady = 1, ipadx = 1, ipady = 1)
    tkgrid(en.dir.save, row = 1, column = 0, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
    tkgrid(bt.dir.save, row = 1, column = 1, sticky = 'w', rowspan = 1, columnspan = 1, padx = 0, pady = 1, ipadx = 1, ipady = 1)

    helpWidget(en.dir.save, lang.dlg[['tooltip']][['7']], lang.dlg[['status']][['7']])
    helpWidget(bt.dir.save, lang.dlg[['tooltip']][['8']], lang.dlg[['status']][['8']])

    ######
    tkgrid(frRFE, row = 0, column = 0, sticky = 'we', rowspan = 1, columnspan = 1, padx = 5, pady = 5, ipadx = 1, ipady = 1)
    tkgrid(frRegion, row = 1, column = 0, sticky = '', rowspan = 1, columnspan = 1, padx = 5, pady = 5, ipadx = 1, ipady = 1)
    tkgrid(frDirsave, row = 2, column = 0, sticky = 'we', rowspan = 1, columnspan = 1, padx = 5, pady = 5, ipadx = 1, ipady = 1)

    ###################################################

    btOK <- ttkbutton(frGrd1, text = lang.dlg[['button']][['1']])
    btCA <- ttkbutton(frGrd1, text = lang.dlg[['button']][['2']])

    tkconfigure(btOK, command = function(){
        if(str_trim(tclvalue(dir2save)) %in% c("", "NA")){
            cdt.tkmessageBox(tt, message = lang.dlg[['message']][['1']], icon = "warning", type = "ok")
            tkwait.window(tt)
        }else{
            tkgrab.release(tt)
            tkdestroy(tt)
            tkfocus(.cdtEnv$tcl$main$win)
            tcl('update')

            .cdtData$GalParams$tstep <- periodVAL[CbperiodVAL %in% str_trim(tclvalue(timeStep))]
            .cdtData$GalParams$rfe.src <- RFEVAL[CbRFEVAL %in% str_trim(tclvalue(RFESrc))]
            .cdtData$GalParams$minhour <- as.numeric(str_trim(tclvalue(minhour.tclVar)))
            .cdtData$GalParams$iridl.src <- switch(tclvalue(iridl.src), '0' = FALSE, '1' = TRUE)

            .cdtData$GalParams$login$usr <- str_trim(tclvalue(username))
            .cdtData$GalParams$login$pwd <- str_trim(tclvalue(password))

            .cdtData$GalParams$dir2save <- str_trim(tclvalue(dir2save))

            .cdtData$GalParams$bbox$minlon <- as.numeric(tclvalue(minLon))
            .cdtData$GalParams$bbox$maxlon <- as.numeric(tclvalue(maxLon))
            .cdtData$GalParams$bbox$minlat <- as.numeric(tclvalue(minLat))
            .cdtData$GalParams$bbox$maxlat <- as.numeric(tclvalue(maxLat))

            .cdtData$GalParams$message <- lang.dlg[['message']]

            if(testConnection()){
                Insert.Messages.Out(lang.dlg[['message']][['3']], TRUE, "i")
                ret <- try(exec.download_RFE(), silent = TRUE)
                if(!inherits(ret, "try-error")){
                    if(ret == 0)
                        Insert.Messages.Out(lang.dlg[['message']][['4']], TRUE, "s")
                    if(ret == 1)
                        Insert.Messages.Out(lang.dlg[['message']][['6']], TRUE, "w")
                    if(ret == -1)
                        Insert.Messages.Out(lang.dlg[['message']][['7']], TRUE, "w")
                    if(ret == -2)
                        Insert.Messages.Out(lang.dlg[['message']][['8']], TRUE, "w")
                }else{
                    Insert.Messages.Out(gsub('[\r\n]', '', ret[1]), TRUE, "e")
                    Insert.Messages.Out(lang.dlg[['message']][['5']], TRUE, "e")
                }
            }else{
                Insert.Messages.Out(lang.dlg[['message']][['2']], format = TRUE)
                return(NULL)
            }
        }
    })

    tkconfigure(btCA, command = function(){
        tkgrab.release(tt)
        tkdestroy(tt)
        tkfocus(.cdtEnv$tcl$main$win)
    })

    tkgrid(btCA, row = 0, column = 0, sticky = 'w', padx = 5, pady = 1, ipadx = 1, ipady = 1)
    tkgrid(btOK, row = 0, column = 1, sticky = 'e', padx = 5, pady = 1, ipadx = 1, ipady = 1)

    #####
    tkgrid(frGrd0, row = 0, column = 0, sticky = 'nswe', rowspan = 1, columnspan = 2, padx = 1, pady = 1, ipadx = 1, ipady = 1)
    tkgrid(frGrd1, row = 1, column = 1, sticky = 'se', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)

    ###################################################

    tkwm.withdraw(tt)
    tcl('update')
    tt.w <- as.integer(tkwinfo("reqwidth", tt))
    tt.h <- as.integer(tkwinfo("reqheight", tt))
    tt.x <- as.integer(.cdtEnv$tcl$data$width.scr*0.5 - tt.w*0.5)
    tt.y <- as.integer(.cdtEnv$tcl$data$height.scr*0.5 - tt.h*0.5)
    tkwm.geometry(tt, paste0('+', tt.x, '+', tt.y))
    tkwm.transient(tt)
    tkwm.title(tt, lang.dlg[['title']])
    tkwm.deiconify(tt)
    tcl('wm', 'attributes', tt, topmost = TRUE)

    tkfocus(tt)
    tkbind(tt, "<Destroy>", function(){
        tkgrab.release(tt)
        tkfocus(.cdtEnv$tcl$main$win)
    })
    tkwait.window(tt)
}
