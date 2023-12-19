
reanal.product.source <- function(prod){
    data.source <- switch(prod,
                          "jra55" = c("rda.ucar.edu - ds628.0 - 3Hourly",
                                      "rda.ucar.edu - ds628.8-NRT - 3Hourly"),
                          "merra2" = c("disc.gsfc.nasa.gov - Hourly",
                                       "disc.gsfc.nasa.gov - Land - Hourly",
                                       "disc.gsfc.nasa.gov - Daily",
                                       "iridl.ldeo.columbia.edu - Daily"),
                          "era5" = c("cds.climate.copernicus.eu - ERA5 - Hourly",
                                     "cds.climate.copernicus.eu - ERA5-Land - Hourly")
                        )
    return(data.source)
}

reanal.product.info <- function(prod, src){
    if(prod == "jra55"){
        urls <- switch(src,
                    "rda.ucar.edu - ds628.0 - 3Hourly" = c("https://rda.ucar.edu/datasets/ds628.0/",
                                                           # "https://rda.ucar.edu/datasets/ds628.0/docs/JRA-55.handbook_TL319_en.pdf",
                                                           "https://jra.kishou.go.jp/JRA-55/document/JRA-55_handbook_TL319_en.pdf"),
                    "rda.ucar.edu - ds628.8-NRT - 3Hourly" = c("https://rda.ucar.edu/datasets/ds628.8/",
                                                               # "https://rda.ucar.edu/datasets/ds628.0/docs/JRA-55.handbook_TL319_en.pdf",
                                                               "https://jra.kishou.go.jp/JRA-55/document/JRA-55_handbook_TL319_en.pdf")
                )
    }

    if(prod == "merra2"){
        urls <- switch(src,
                    "disc.gsfc.nasa.gov - Hourly" = c("https://disc.gsfc.nasa.gov/datasets/M2T1NXSLV_5.12.4/summary",
                                                      "https://disc.gsfc.nasa.gov/datasets/M2T1NXFLX_5.12.4/summary",
                                                      "https://disc.gsfc.nasa.gov/datasets/M2I1NXASM_5.12.4/summary",
                                                      "https://disc.gsfc.nasa.gov/datasets/M2T1NXRAD_5.12.4/summary",
                                                      "https://gmao.gsfc.nasa.gov/reanalysis/MERRA-2/"),
                    "disc.gsfc.nasa.gov - Land - Hourly" = c("https://disc.gsfc.nasa.gov/datasets/M2T1NXLND_5.12.4/summary",
                                                             "https://daac.gsfc.nasa.gov/information/documents?title=MERRA-2%20Data%20Access%20%E2%80%93%20Quick%20Guide",
                                                             "https://gmao.gsfc.nasa.gov/reanalysis/MERRA-2/"),
                    "disc.gsfc.nasa.gov - Daily" = c("https://disc.gsfc.nasa.gov/datasets/M2SDNXSLV_5.12.4/summary",
                                                     "https://daac.gsfc.nasa.gov/information/documents?title=MERRA-2%20Data%20Access%20%E2%80%93%20Quick%20Guide",
                                                     "https://gmao.gsfc.nasa.gov/reanalysis/MERRA-2/"),
                    "iridl.ldeo.columbia.edu - Daily" = c("https://iridl.ldeo.columbia.edu/SOURCES/.NASA/.GSFC/.MERRA2/.Anl_MonoLev/",
                                                           "https://gmao.gsfc.nasa.gov/reanalysis/MERRA-2/")
                )
    }

    if(prod == "era5"){
        urls <- switch(src,
                    "cds.climate.copernicus.eu - ERA5 - Hourly" = c(
                           "https://cds.climate.copernicus.eu/cdsapp#!/dataset/reanalysis-era5-single-levels?tab=overview",
                           "https://cds.climate.copernicus.eu/cdsapp#!/dataset/reanalysis-era5-single-levels-preliminary-back-extension?tab=overview",
                           "https://confluence.ecmwf.int/display/CKB/ERA5%3A+data+documentation"
                        ),
                    "cds.climate.copernicus.eu - ERA5-Land - Hourly" = c(
                           "https://cds.climate.copernicus.eu/cdsapp#!/dataset/reanalysis-era5-land?tab=overview",
                           "https://confluence.ecmwf.int/display/CKB/ERA5-Land%3A+data+documentation"
                       )
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

reanal.need.usrpwd <- function(prod, src){
    usrpwd <- FALSE
    urllog <- ""

    # if(prod == "jra55"){
    #     if(src == "rda.ucar.edu - ds628.8-NRT - 3Hourly"){
    #         usrpwd <- TRUE
    #         urllog <- "https://rda.ucar.edu"
    #     }
    # }

    if(prod == "merra2"){
        if(src != "iridl.ldeo.columbia.edu - Daily"){
            usrpwd <- TRUE
            urllog <- "https://urs.earthdata.nasa.gov"
        }
    }

    if(prod == "era5"){
        usrpwd <- TRUE
        urllog <- "https://cds.climate.copernicus.eu"
    }

    list(usrpwd = usrpwd, urllog = urllog)
}

#######################################

download_Reanalysis <- function(){
    if(WindowsOS()){
        largeur0 <- 64
        largeur1 <- 27
        largeur2 <- 37
        largeur3 <- 20
        largeur5 <- 42
        largeur6 <- 30
        largeur7 <- 15
    }else{
        largeur0 <- 61
        largeur1 <- 27
        largeur2 <- 37
        largeur3 <- 20
        largeur5 <- 42
        largeur6 <- 30
        largeur7 <- 15
    }

    xml.dlg <- file.path(.cdtDir$dirLocal, "languages", "cdtDownloadReanalysis_dlgBox.xml")
    lang.dlg <- cdtLanguageParse(xml.dlg, .cdtData$Config$lang.iso)

    #########
    tt <- tktoplevel()
    tkgrab.set(tt)
    tkfocus(tt)

    frGrd0 <- tkframe(tt, relief = 'raised', borderwidth = 2)
    frGrd1 <- tkframe(tt)

    ###################################################

    frRFE <- tkframe(frGrd0, relief = 'sunken', bd = 2)

    downVar <- tclVar()

    CbvarsVAL0 <- lang.dlg[['combobox']][['1']]

    varsVAL0 <- c('tmax', 'tmin', 'tair', 'wind', 'hum', 'pres', 'prmsl',
                  'cloud', 'rad_avg', 'rad_acc', 'prcp', 'evp', 'pet', 'runoff',
                  'soilm', 'soilt', 'tsg', 'heat_avg', 'heat_acc', 'ghflx'
                 )

    if(.cdtData$GalParams$prod == "jra55"){
        excl_vr <- varsVAL0 %in% c('rad_acc', 'heat_acc')
        CbvarsVAL <- CbvarsVAL0[!excl_vr]
        varsVAL <- varsVAL0[!excl_vr]
    }else if(.cdtData$GalParams$prod == "era5"){
        if(.cdtData$GalParams$src == "cds.climate.copernicus.eu - ERA5 - Hourly"){
            excl_vr <- varsVAL0 %in% c('ghflx')
        }else{
            excl_vr <- varsVAL0 %in% c('tmax', 'tmin', 'prmsl', 'cloud',
                                       'rad_avg', 'heat_avg', 'ghflx')
        }
        CbvarsVAL <- CbvarsVAL0[!excl_vr]
        varsVAL <- varsVAL0[!excl_vr]
    }else{
        if(.cdtData$GalParams$src == "disc.gsfc.nasa.gov - Hourly"){
            excl_vr <- varsVAL0 %in% c('tmax', 'tmin', 'rad_acc', 'evp', 'runoff',
                                       'soilm', 'soilt', 'heat_acc')
            CbvarsVAL <- CbvarsVAL0[!excl_vr]
            varsVAL <- varsVAL0[!excl_vr]
        }else if(.cdtData$GalParams$src == "disc.gsfc.nasa.gov - Land - Hourly"){
            excl_vr <- varsVAL0 %in% c('tmax', 'tmin', 'tair', 'wind', 'hum', 'pres',
                                       'prmsl', 'cloud', 'evp', 'rad_acc', 'heat_acc')
            CbvarsVAL <- CbvarsVAL0[!excl_vr]
            varsVAL <- varsVAL0[!excl_vr]
        }else{
            CbvarsVAL <- CbvarsVAL0[1:3]
            varsVAL <- varsVAL0[1:3]
        }
    }

    tclvalue(downVar) <- CbvarsVAL[varsVAL %in% .cdtData$GalParams$var]

    reanalProd <- tclVar()
    CbprodVAL <- c("JRA-55", "MERRA-2", "ERA5")
    prodVAL <- c("jra55", "merra2", "era5")
    tclvalue(reanalProd) <- CbprodVAL[prodVAL %in% .cdtData$GalParams$prod]

    reanalSrc <- tclVar(.cdtData$GalParams$src)
    CbsrcVAL <- reanal.product.source(.cdtData$GalParams$prod)

    need.pwd <- reanal.need.usrpwd(.cdtData$GalParams$prod, .cdtData$GalParams$src)
    statepwd <- if(need.pwd$usrpwd) "normal" else "disabled"

    url.log <- tclVar(need.pwd$urllog)
    username <- tclVar(.cdtData$GalParams$login$usr)
    password <- tclVar(.cdtData$GalParams$login$pwd)

    ###########################

    frREAN <- tkframe(frRFE)
    txt.prod <- tklabel(frREAN, text = lang.dlg[['label']][['2']], anchor = 'w', justify = 'left')
    cb.prod <- ttkcombobox(frREAN, values = CbprodVAL, textvariable = reanalProd, width = largeur3)
    txt.src <- tklabel(frREAN, text = lang.dlg[['label']][['3']], anchor = 'w', justify = 'right')
    cb.src <- ttkcombobox(frREAN, values = CbsrcVAL, textvariable = reanalSrc, width = largeur5)

    frVARDATE <- tkframe(frRFE)
    txt.vars <- tklabel(frVARDATE, text = lang.dlg[['label']][['1']], anchor = 'w', justify = 'left')
    cb.vars <- ttkcombobox(frVARDATE, values = CbvarsVAL, textvariable = downVar, width = largeur2)

    bt.range <- ttkbutton(frRFE, text = lang.dlg[['button']][['3']])

    frLOGIN <- tkframe(frRFE)
    txt.log1 <- tklabel(frLOGIN, text = lang.dlg[['label']][['4']], anchor = 'e', justify = 'right')
    txt.log2 <- tklabel(frLOGIN, text = tclvalue(url.log), textvariable = url.log, anchor = 'w', justify = 'left')

    frUSER <- tkframe(frRFE)
    txt.usr <- tklabel(frUSER, text = lang.dlg[['label']][['5']], anchor = 'e', justify = 'right')
    en.usr <- tkentry(frUSER, textvariable = username, state = statepwd, width = largeur6, justify = "left")
    txt.pwd <- tklabel(frUSER, text = lang.dlg[['label']][['6']], anchor = 'e', justify = 'right')
    en.pwd <- tkentry(frUSER, textvariable = password, show = "*", state = statepwd, width = largeur7, justify = "left")

    bt.info <- ttkbutton(frRFE, text = lang.dlg[['button']][['4']])

    ###########################

    tkconfigure(bt.range, command = function(){
         tstep <- switch(trimws(tclvalue(reanalSrc)),
                         "rda.ucar.edu - ds628.0 - 3Hourly" = "hourly",
                         "rda.ucar.edu - ds628.8-NRT - 3Hourly" = "hourly",
                         "disc.gsfc.nasa.gov - Hourly" = "hourly",
                         "disc.gsfc.nasa.gov - Land - Hourly" = "hourly",
                         "disc.gsfc.nasa.gov - Daily" = "daily",
                         "iridl.ldeo.columbia.edu - Daily" = "daily",
                         "cds.climate.copernicus.eu - ERA5 - Hourly" = "hourly",
                         "cds.climate.copernicus.eu - ERA5-Land - Hourly" = "hourly")

        tcl('wm', 'attributes', tt, topmost = FALSE)
        .cdtData$GalParams[["date.range"]] <- getInfoDateRange(tt, .cdtData$GalParams[["date.range"]], tstep)
        tcl('wm', 'attributes', tt, topmost = TRUE)
    })

    tkconfigure(bt.info, command = function(){
        prod <- prodVAL[CbprodVAL %in% trimws(tclvalue(reanalProd))]
        src <- trimws(tclvalue(reanalSrc))
        urls <- reanal.product.info(prod, src)
        utils::browseURL(paste0('file://', urls))
    })

    ###########################

    tkgrid(txt.prod, row = 0, column = 0, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
    tkgrid(cb.prod, row = 1, column = 0, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
    tkgrid(txt.src, row = 0, column = 1, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
    tkgrid(cb.src, row = 1, column = 1, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)

    tkgrid(txt.vars, row = 0, column = 0, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
    tkgrid(cb.vars, row = 1, column = 0, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)

    tkgrid(txt.log1, row = 0, column = 0, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
    tkgrid(txt.log2, row = 0, column = 1, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)

    tkgrid(txt.usr, row = 0, column = 0, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
    tkgrid(en.usr, row = 0, column = 1, sticky = 'we', rowspan = 1, columnspan = 4, padx = 1, pady = 1, ipadx = 1, ipady = 1)
    tkgrid(txt.pwd, row = 0, column = 5, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
    tkgrid(en.pwd, row = 0, column = 6, sticky = 'we', rowspan = 1, columnspan = 2, padx = 1, pady = 1, ipadx = 1, ipady = 1)

    tkgrid(frREAN, row = 0, column = 0, sticky = 'we', rowspan = 1, columnspan = 8, padx = 1, pady = 1, ipadx = 1, ipady = 1)
    tkgrid(frVARDATE, row = 1, column = 0, sticky = 'we', rowspan = 1, columnspan = 5, padx = 1, pady = 1, ipadx = 1, ipady = 1)
    tkgrid(bt.range, row = 1, column = 5, sticky = 'we', rowspan = 1, columnspan = 3, padx = 1, pady = 1, ipadx = 1, ipady = 1)
    tkgrid(frLOGIN, row = 2, column = 0, sticky = 'we', rowspan = 1, columnspan = 8, padx = 1, pady = 1, ipadx = 1, ipady = 1)
    tkgrid(frUSER, row = 3, column = 0, sticky = 'we', rowspan = 1, columnspan = 8, padx = 1, pady = 1, ipadx = 1, ipady = 1)

    tkgrid(bt.info, row = 4, column = 1, sticky = 'we', rowspan = 1, columnspan = 6, padx = 1, pady = 1, ipadx = 1, ipady = 1)

    helpWidget(cb.vars, lang.dlg[['tooltip']][['1']], lang.dlg[['status']][['1']])
    helpWidget(cb.prod, lang.dlg[['tooltip']][['2']], lang.dlg[['status']][['2']])
    helpWidget(cb.src, lang.dlg[['tooltip']][['2a']], lang.dlg[['status']][['2a']])
    helpWidget(bt.range, lang.dlg[['tooltip']][['2b']], lang.dlg[['status']][['2b']])

    ###########################

    tkbind(cb.prod, "<<ComboboxSelected>>", function(){
        prod <- prodVAL[CbprodVAL %in% trimws(tclvalue(reanalProd))]
        CbsrcVAL <- reanal.product.source(prod)

        tkconfigure(cb.src, values = CbsrcVAL)
        tclvalue(reanalSrc) <- CbsrcVAL[1]

        ########
        src <- trimws(tclvalue(reanalSrc))

        need.pwd <- reanal.need.usrpwd(prod, src)
        statepwd <- if(need.pwd$usrpwd) "normal" else "disabled"

        tclvalue(url.log) <- need.pwd$urllog
        tkconfigure(en.usr, state = statepwd)
        tkconfigure(en.pwd, state = statepwd)

        ########
        if(prod == "jra55"){
            excl_vr <- varsVAL0 %in% c('rad_acc', 'heat_acc')
            CbvarsVAL <<- CbvarsVAL0[!excl_vr]
            varsVAL <<- varsVAL0[!excl_vr]
        }else if(prod == "era5"){
            if(src == "cds.climate.copernicus.eu - ERA5 - Hourly"){
                excl_vr <- varsVAL0 %in% c('ghflx')
            }else{
                excl_vr <- varsVAL0 %in% c('tmax', 'tmin', 'prmsl', 'cloud',
                                           'rad_avg', 'heat_avg', 'ghflx')
            }
            CbvarsVAL <<- CbvarsVAL0[!excl_vr]
            varsVAL <<- varsVAL0[!excl_vr]
        }else{
            if(src == "disc.gsfc.nasa.gov - Hourly"){
                excl_vr <- varsVAL0 %in% c('tmax', 'tmin', 'rad_acc', 'evp', 'runoff',
                                           'soilm', 'soilt', 'heat_acc')
                CbvarsVAL <<- CbvarsVAL0[!excl_vr]
                varsVAL <<- varsVAL0[!excl_vr]
            }else if(src == "disc.gsfc.nasa.gov - Land - Hourly"){
                excl_vr <- varsVAL0 %in% c('tmax', 'tmin', 'tair', 'wind', 'hum', 'pres',
                                           'prmsl', 'cloud', 'evp', 'rad_acc', 'heat_acc')
                CbvarsVAL <<- CbvarsVAL0[!excl_vr]
                varsVAL <<- varsVAL0[!excl_vr]
            }else{
                CbvarsVAL <<- CbvarsVAL0[1:3]
                varsVAL <<- varsVAL0[1:3]
            }
        }

        tkconfigure(cb.vars, values = CbvarsVAL)
        if(!trimws(tclvalue(downVar)) %in% CbvarsVAL)
            tclvalue(downVar) <- CbvarsVAL[1]
    })

    tkbind(cb.src, "<<ComboboxSelected>>", function(){
        prod <- prodVAL[CbprodVAL %in% trimws(tclvalue(reanalProd))]
        src <- trimws(tclvalue(reanalSrc))

        ########
        if(prod == "jra55"){
            excl_vr <- varsVAL0 %in% c('rad_acc', 'heat_acc')
            CbvarsVAL <<- CbvarsVAL0[!excl_vr]
            varsVAL <<- varsVAL0[!excl_vr]
        }else if(prod == "era5"){
            if(src == "cds.climate.copernicus.eu - ERA5 - Hourly"){
                excl_vr <- varsVAL0 %in% c('ghflx')
            }else{
                excl_vr <- varsVAL0 %in% c('tmax', 'tmin', 'prmsl', 'cloud',
                                           'rad_avg', 'heat_avg', 'ghflx')
            }
            CbvarsVAL <<- CbvarsVAL0[!excl_vr]
            varsVAL <<- varsVAL0[!excl_vr]
        }else{
            if(src == "disc.gsfc.nasa.gov - Hourly"){
                excl_vr <- varsVAL0 %in% c('tmax', 'tmin', 'rad_acc', 'evp', 'runoff',
                                           'soilm', 'soilt', 'heat_acc')
                CbvarsVAL <<- CbvarsVAL0[!excl_vr]
                varsVAL <<- varsVAL0[!excl_vr]
            }else if(src == "disc.gsfc.nasa.gov - Land - Hourly"){
                excl_vr <- varsVAL0 %in% c('tmax', 'tmin', 'tair', 'wind', 'hum', 'pres',
                                           'prmsl', 'cloud', 'evp', 'rad_acc', 'heat_acc')
                CbvarsVAL <<- CbvarsVAL0[!excl_vr]
                varsVAL <<- varsVAL0[!excl_vr]
            }else{
                CbvarsVAL <<- CbvarsVAL0[1:3]
                varsVAL <<- varsVAL0[1:3]
            }
        }

        tkconfigure(cb.vars, values = CbvarsVAL)
        if(!trimws(tclvalue(downVar)) %in% CbvarsVAL)
            tclvalue(downVar) <- CbvarsVAL[1]

        ########
        need.pwd <- reanal.need.usrpwd(prod, src)
        statepwd <- if(need.pwd$usrpwd) "normal" else "disabled"

        tclvalue(url.log) <- need.pwd$urllog
        tkconfigure(en.usr, state = statepwd)
        tkconfigure(en.pwd, state = statepwd)
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

    ###########################

    tkgrid(frRFE, row = 0, column = 0, sticky = 'we', rowspan = 1, columnspan = 1, padx = 5, pady = 5, ipadx = 1, ipady = 1)
    tkgrid(frRegion, row = 1, column = 0, sticky = '', rowspan = 1, columnspan = 1, padx = 5, pady = 5, ipadx = 1, ipady = 1)
    tkgrid(frDirsave, row = 2, column = 0, sticky = 'we', rowspan = 1, columnspan = 1, padx = 5, pady = 5, ipadx = 1, ipady = 1)

    ###################################################

    btOK <- ttkbutton(frGrd1, text = lang.dlg[['button']][['1']])
    btCA <- ttkbutton(frGrd1, text = lang.dlg[['button']][['2']])

    tkconfigure(btOK, command = function(){
        if(trimws(tclvalue(dir2save)) %in% c("", "NA")){
            cdt.tkmessageBox(tt, message = lang.dlg[['message']][['1']], icon = "warning", type = "ok")
            tkwait.window(tt)
        }else{
            tkgrab.release(tt)
            tkdestroy(tt)
            tkfocus(.cdtEnv$tcl$main$win)
            tcl('update')

            .cdtData$GalParams$var <- varsVAL[CbvarsVAL %in% trimws(tclvalue(downVar))]
            .cdtData$GalParams$prod <- prodVAL[CbprodVAL %in% trimws(tclvalue(reanalProd))]
            .cdtData$GalParams$src <- trimws(tclvalue(reanalSrc))

            .cdtData$GalParams$login$usr <- trimws(tclvalue(username))
            .cdtData$GalParams$login$pwd <- trimws(tclvalue(password))

            .cdtData$GalParams$dir2save <- trimws(tclvalue(dir2save))

            .cdtData$GalParams$bbox$minlon <- as.numeric(tclvalue(minLon))
            .cdtData$GalParams$bbox$maxlon <- as.numeric(tclvalue(maxLon))
            .cdtData$GalParams$bbox$minlat <- as.numeric(tclvalue(minLat))
            .cdtData$GalParams$bbox$maxlat <- as.numeric(tclvalue(maxLat))

            .cdtData$GalParams$message <- lang.dlg[['message']]

            if(testConnection()){
                Insert.Messages.Out(lang.dlg[['message']][['3']], TRUE, "i")
                ret <- try(exec.download_Reanalysis(), silent = TRUE)
                # ret <- 0
                if(!inherits(ret, "try-error")){
                    if(ret == 0)
                        Insert.Messages.Out(lang.dlg[['message']][['4']], TRUE, "s")
                    if(ret == 1)
                        Insert.Messages.Out(lang.dlg[['message']][['6']], TRUE, "w")
                    if(ret == -1)
                        Insert.Messages.Out(lang.dlg[['message']][['7']], TRUE, "w")
                    if(ret == -2)
                        Insert.Messages.Out(lang.dlg[['message']][['8']], TRUE, "w")
                    if(ret == -3)
                        Insert.Messages.Out(lang.dlg[['message']][['9']], TRUE, "e")
                    if(ret == -4)
                        Insert.Messages.Out(lang.dlg[['message']][['10']], TRUE, "e")
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
