
extractGRIB_JRA55NRT <- function(){
    if(WindowsOS()){
        largeur0 <- 47
        largeur1 <- 48
        largeur2 <- 25
    }else{
        largeur0 <- 44
        largeur1 <- 45
        largeur2 <- 25
    }

    xml.dlg <- file.path(.cdtDir$dirLocal, "languages", "cdtExtractGRIBJRA55NRT_dlgBox.xml")
    lang.dlg <- cdtLanguageParse(xml.dlg, .cdtData$Config$lang.iso)

    ###################

    tt <- tktoplevel()
    tkgrab.set(tt)
    tkfocus(tt)

    frMRG0 <- tkframe(tt, relief = 'raised', borderwidth = 2)
    frMRG1 <- tkframe(tt)

    ############################################

    frGRIB <- tkframe(frMRG0, relief = "sunken", borderwidth = 2)

    dir.grib <- tclVar(.cdtData$GalParams$dir.grib)

    txt.grib <- tklabel(frGRIB, text = lang.dlg[['label']][['1']], anchor = 'w', justify = 'left')
    cb.grib <- tkentry(frGRIB, textvariable = dir.grib, width = largeur1)
    bt.grib <- tkbutton(frGRIB, text = "...")

    tkgrid(txt.grib, row = 0, column = 0, sticky = 'we', rowspan = 1, columnspan = 2, padx = 1, pady = 1, ipadx = 1, ipady = 1)
    tkgrid(cb.grib, row = 1, column = 0, sticky = 'we', rowspan = 1, columnspan = 1, padx = 0, pady = 1, ipadx = 1, ipady = 1)
    tkgrid(bt.grib, row = 1, column = 1, sticky = 'we', rowspan = 1, columnspan = 1, padx = 0, pady = 1, ipadx = 1, ipady = 1)

    helpWidget(cb.grib, lang.dlg[['tooltip']][['1']], lang.dlg[['status']][['1']])
    helpWidget(bt.grib, lang.dlg[['tooltip']][['2']], lang.dlg[['status']][['2']])

    ########
    tkconfigure(bt.grib, command = function(){
        tcl('wm', 'attributes', tt, topmost = FALSE)
        dirGrib <- tk_choose.dir(getwd(), "")
        tcl('wm', 'attributes', tt, topmost = TRUE)
        tclvalue(dir.grib) <- if(!is.na(dirGrib)) dirGrib else ""
    })

    ############################################

    frVAR <- tkframe(frMRG0, relief = "sunken", borderwidth = 2)

    extractVar <- tclVar()

    CbvarsVAL <- lang.dlg[['combobox']][['1']]

    varsVAL <- c('tmax', 'tmin', 'tair', 'wind', 'hum', 'pres', 'prmsl',
                  'cloud', 'rad_avg', 'prcp', 'evp', 'pet', 'runoff',
                  'soilm', 'soilt', 'tsg', 'heat_avg', 'ghflx')

    tclvalue(extractVar) <- CbvarsVAL[varsVAL %in% .cdtData$GalParams$var]

    txt.exVar <- tklabel(frVAR, text = lang.dlg[['label']][['2']], anchor = 'w', justify = 'left')
    cb.exVar <- ttkcombobox(frVAR, values = CbvarsVAL, textvariable = extractVar, width = largeur0)

    tkgrid(txt.exVar, row = 0, column = 0, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
    tkgrid(cb.exVar, row = 1, column = 0, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)

    helpWidget(cb.exVar, lang.dlg[['tooltip']][['3']], lang.dlg[['status']][['3']])

    ############################################

    btRange <- ttkbutton(frMRG0, text = lang.dlg[['button']][['1']])

    tkconfigure(btRange, command = function(){
        tcl('wm', 'attributes', tt, topmost = FALSE)
        .cdtData$GalParams[["date.range"]] <- getInfoDateRange(tt, .cdtData$GalParams[["date.range"]], "hourly")
        tcl('wm', 'attributes', tt, topmost = TRUE)
    })

    ############################################

    frRegion <- tkframe(frMRG0, relief = 'sunken', bd = 2)

    minLon <- tclVar(.cdtData$GalParams$bbox$minlon)
    maxLon <- tclVar(.cdtData$GalParams$bbox$maxlon)
    minLat <- tclVar(.cdtData$GalParams$bbox$minlat)
    maxLat <- tclVar(.cdtData$GalParams$bbox$maxlat)

    txt_AOI <- tklabel(frRegion, text = lang.dlg[['label']][['3']], anchor = 'w', justify = 'left')

    grd_llon <- tklabel(frRegion, text = lang.dlg[['label']][['4']], anchor = 'e', justify = 'right', width = largeur2)
    grd_llat <- tklabel(frRegion, text = lang.dlg[['label']][['5']], anchor = 'e', justify = 'right')
    grd_lb1 <- tklabel(frRegion, text = lang.dlg[['label']][['6']])
    grd_lb2 <- tklabel(frRegion, text = lang.dlg[['label']][['7']])
    grd_vlon1 <- tkentry(frRegion, width = 8, justify = "right", textvariable = minLon)
    grd_vlon2 <- tkentry(frRegion, width = 8, justify = "right", textvariable = maxLon)
    grd_vlat1 <- tkentry(frRegion, width = 8, justify = "right", textvariable = minLat)
    grd_vlat2 <- tkentry(frRegion, width = 8, justify = "right", textvariable = maxLat)

    tkgrid(txt_AOI, row = 0, column = 0, sticky = 'we', columnspan = 3, pady = 1, ipady = 1)
    tkgrid(grd_lb1, row = 1, column = 1, sticky = "ew")
    tkgrid(grd_lb2, row = 1, column = 2, sticky = "ew")
    tkgrid(grd_llon, row = 2, column = 0, sticky = "ew")
    tkgrid(grd_vlon1, row = 2, column = 1, sticky = "ew")
    tkgrid(grd_vlon2, row = 2, column = 2, sticky = "ew")
    tkgrid(grd_llat, row = 3, column = 0, sticky = "ew")
    tkgrid(grd_vlat1, row = 3, column = 1, sticky = "ew")
    tkgrid(grd_vlat2, row = 3, column = 2, sticky = "ew")

    ############################################

    frSave <- tkframe(frMRG0, relief = 'sunken', borderwidth = 2)

    dir2save <- tclVar(.cdtData$GalParams$dir2save)

    txt.dirsave <- tklabel(frSave, text = lang.dlg[['label']][['8']], anchor = 'w', justify = 'left')
    en.dirsave <- tkentry(frSave, textvariable = dir2save, width = largeur1)
    bt.dirsave <- tkbutton(frSave, text = "...")

    #########
    tkconfigure(bt.dirsave, command = function(){
        initialdir <- if(str_trim(.cdtData$GalParams$dir2save) != "") .cdtData$GalParams$dir2save else getwd()
        tcl('wm', 'attributes', tt, topmost = FALSE)
        dir2savepth <- tk_choose.dir(initialdir, "")
        tcl('wm', 'attributes', tt, topmost = TRUE)
        if(is.na(dir2savepth)) tclvalue(dir2save) <- initialdir
        else{
            dir.create(dir2savepth, showWarnings = FALSE, recursive = TRUE)
            tclvalue(dir2save) <- dir2savepth
        }
    })

    #########
    tkgrid(txt.dirsave, row = 0, column = 0, sticky = 'we', rowspan = 1, columnspan = 5, padx = 1, pady = 0, ipadx = 1, ipady = 1)
    tkgrid(en.dirsave, row = 1, column = 0, sticky = 'we', rowspan = 1, columnspan = 4, padx = 0, pady = 0, ipadx = 1, ipady = 1)
    tkgrid(bt.dirsave, row = 1, column = 4, sticky = 'we', rowspan = 1, columnspan = 1, padx = 0, pady = 0, ipadx = 1, ipady = 1)

    #########

    helpWidget(en.dirsave, lang.dlg[['tooltip']][['4']], lang.dlg[['status']][['4']])
    helpWidget(bt.dirsave, lang.dlg[['tooltip']][['5']], lang.dlg[['status']][['5']])

    ############################################
    tkgrid(frGRIB, row = 0, column = 0, sticky = 'we', padx = 1, pady = 1, ipadx = 1, ipady = 1)
    tkgrid(frVAR, row = 1, column = 0, sticky = 'we', padx = 1, pady = 1, ipadx = 1, ipady = 1)
    tkgrid(btRange, row = 2, column = 0, sticky = 'we', padx = 1, pady = 6, ipadx = 1, ipady = 1)
    tkgrid(frRegion, row = 3, column = 0, sticky = 'we', padx = 1, pady = 1, ipadx = 1, ipady = 1)
    tkgrid(frSave, row = 4, column = 0, sticky = 'we', padx = 1, pady = 1, ipadx = 1, ipady = 1)

    ############################################

    bt.prm.OK <- ttkbutton(frMRG1, text = .cdtEnv$tcl$lang$global[['button']][['1']])
    bt.prm.CA <- ttkbutton(frMRG1, text = .cdtEnv$tcl$lang$global[['button']][['2']])

    ####
    tkconfigure(bt.prm.OK, command = function(){
        if(str_trim(tclvalue(dir.grib)) %in% c("", "NA")){
            cdt.tkmessageBox(tt, message = lang.dlg[['message']][['2']], icon = "warning", type = "ok")
            tkwait.window(tt)
        }else if(str_trim(tclvalue(dir2save)) %in% c("", "NA")){
            cdt.tkmessageBox(tt, message = lang.dlg[['message']][['3']], icon = "warning", type = "ok")
            tkwait.window(tt)
        }else{
            .cdtData$GalParams$dir.grib <- str_trim(tclvalue(dir.grib))
            .cdtData$GalParams$dir2save <- str_trim(tclvalue(dir2save))

            .cdtData$GalParams$bbox$minlon <- as.numeric(tclvalue(minLon))
            .cdtData$GalParams$bbox$maxlon <- as.numeric(tclvalue(maxLon))
            .cdtData$GalParams$bbox$minlat <- as.numeric(tclvalue(minLat))
            .cdtData$GalParams$bbox$maxlat <- as.numeric(tclvalue(maxLat))

            .cdtData$GalParams$var <- varsVAL[CbvarsVAL %in% str_trim(tclvalue(extractVar))]

            .cdtData$GalParams$message <- lang.dlg[['message']]

            tkgrab.release(tt)
            tkdestroy(tt)
            tkfocus(.cdtEnv$tcl$main$win)
        }
    })

    tkconfigure(bt.prm.CA, command = function(){
        tkgrab.release(tt)
        tkdestroy(tt)
        tkfocus(.cdtEnv$tcl$main$win)
    })

    ####
    tkgrid(bt.prm.OK, row = 0, column = 0, sticky = 'w', padx = 5, pady = 1, ipadx = 1, ipady = 1)
    tkgrid(bt.prm.CA, row = 0, column = 1, sticky = 'e', padx = 5, pady = 1, ipadx = 1, ipady = 1)

    ############################################
    
    tkgrid(frMRG0, row = 0, column = 0, sticky = 'nswe', rowspan = 1, columnspan = 2, padx = 1, pady = 1, ipadx = 1, ipady = 1)
    tkgrid(frMRG1, row = 1, column = 1, sticky = 'se', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)

    ###########################
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
    tkbind(tt, "<Destroy>", function() {
        tkgrab.release(tt)
        tkfocus(.cdtEnv$tcl$main$win)
    })
    tkwait.window(tt)
}
