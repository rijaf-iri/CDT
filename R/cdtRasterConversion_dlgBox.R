
rasterData.convert_getParams <- function(){
    listOpenFiles <- openFile_ttkcomboList()
    if(WindowsOS()){
        largeur0 <- 21
        largeur1 <- 56
        largeur2 <- 47
        largeur3 <- 10
        largeur4 <- 8
    }else{
        largeur0 <- 21
        largeur1 <- 51
        largeur2 <- 44
        largeur3 <- 10
        largeur4 <- 8
    }

    xml.dlg <- file.path(.cdtDir$dirLocal, "languages", "cdtRasterConversion_dlgBox.xml")
    lang.dlg <- cdtLanguageParse(xml.dlg, .cdtData$Config$lang.iso)

    ############################################

    tt <- tktoplevel()
    tkgrab.set(tt)
    tkfocus(tt)

    frMRG0 <- tkframe(tt, relief = 'raised', borderwidth = 2)
    frMRG1 <- tkframe(tt)
    frConversion <- tkframe(frMRG0, relief = "sunken", borderwidth = 2)

    ############################################

    frConvFormat <- ttklabelframe(frConversion, text = lang.dlg[['label']][['1']], labelanchor = "nw", relief = "groove", borderwidth = 2)

    FORMAT.CONV1 <- c("NetCDF", "GeoTiff", "ESRI .hdr Labelled")

    OriginData <- tclVar()
    tclvalue(OriginData) <- switch(.cdtData$GalParams$type.in,
                                   'nc' = FORMAT.CONV1[1],
                                   'tif' = FORMAT.CONV1[2],
                                   'bil' = FORMAT.CONV1[2])

    FORMAT.CONV2 <- switch(.cdtData$GalParams$type.in,
                           'nc' = c("GeoTiff", "ESRI .hdr Labelled"),
                           'tif' = c("NetCDF", "ESRI .hdr Labelled"),
                           'bil' = c("NetCDF", "GeoTiff"))

    ConvertData <- tclVar()
    tclvalue(ConvertData) <- switch(.cdtData$GalParams$type.out,
                                    'nc' = FORMAT.CONV1[1],
                                    'tif' = FORMAT.CONV1[2],
                                    'bil' = FORMAT.CONV1[2])

    cb.in.data <- ttkcombobox(frConvFormat, values = FORMAT.CONV1, textvariable = OriginData, width = largeur0)
    cb.out.data <- ttkcombobox(frConvFormat, values = FORMAT.CONV2, textvariable = ConvertData, width = largeur0)
    txt.conv.data1 <- tklabel(frConvFormat, text = lang.dlg[['label']][['2']])
    txt.conv.data2 <- tklabel(frConvFormat, text = lang.dlg[['label']][['3']])

    tkgrid(txt.conv.data1, row = 0, column = 0, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
    tkgrid(cb.in.data, row = 0, column = 1, sticky = 'we', rowspan = 1, columnspan = 8, padx = 1, pady = 1, ipadx = 1, ipady = 1)
    tkgrid(txt.conv.data2, row = 0, column = 9, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
    tkgrid(cb.out.data, row = 0, column = 10, sticky = 'we', rowspan = 1, columnspan = 8, padx = 1, pady = 1, ipadx = 1, ipady = 1)

    tkbind(cb.in.data, "<<ComboboxSelected>>", function(){
        FORMAT.CONV2 <- switch(trimws(tclvalue(OriginData)),
                               "NetCDF" = c("GeoTiff", "ESRI .hdr Labelled"),
                               "GeoTiff" = c("NetCDF", "ESRI .hdr Labelled"),
                               "ESRI .hdr Labelled" = c("NetCDF", "GeoTiff"))

        tkconfigure(cb.out.data, values = FORMAT.CONV2)
        tclvalue(ConvertData) <- if(tclvalue(ConvertData) == tclvalue(OriginData)) FORMAT.CONV2[1] else tclvalue(ConvertData)

        ###
        stateNC <- if(trimws(tclvalue(ConvertData)) == "NetCDF") "normal" else "disabled"
        tkconfigure(en.nc.name,state = stateNC)
        tkconfigure(en.nc.unit,state = stateNC)
        tkconfigure(en.nc.miss,state = stateNC)
        tkconfigure(en.nc.lname,state = stateNC)

        ###
        tclvalue(textINdir) <- switch(trimws(tclvalue(OriginData)), 
                                      "NetCDF" = lang.dlg[['label']][['5']],
                                      "GeoTiff" = lang.dlg[['label']][['6']],
                                      "ESRI .hdr Labelled" = lang.dlg[['label']][['7']])
    })

    tkbind(cb.out.data, "<<ComboboxSelected>>", function(){
        stateNC <- if(trimws(tclvalue(ConvertData)) == "NetCDF") "normal" else "disabled"
        tkconfigure(en.nc.name,state = stateNC)
        tkconfigure(en.nc.unit,state = stateNC)
        tkconfigure(en.nc.miss,state = stateNC)
        tkconfigure(en.nc.lname,state = stateNC)
    })

    ############################################

    frDirectory <- ttklabelframe(frConversion, text = lang.dlg[['label']][['4']], labelanchor = "nw", relief = "groove", borderwidth = 2)

    textDir <- switch(.cdtData$GalParams$type.in, 
                      'nc' = lang.dlg[['label']][['5']],
                      'tif' = lang.dlg[['label']][['6']],
                      'bil' = lang.dlg[['label']][['7']])

    textINdir <- tclVar(textDir)
    dir.input <- tclVar(.cdtData$GalParams$dir.in)
    dir.output <- tclVar(.cdtData$GalParams$dir.out)

    txt.in.dir <- tklabel(frDirectory, text = tclvalue(textINdir), textvariable = textINdir, anchor = 'w', justify = 'left')
    en.in.dir <- tkentry(frDirectory, textvariable = dir.input, width = largeur1)
    bt.in.dir <- tkbutton(frDirectory, text = "...")
    txt.out.dir <- tklabel(frDirectory, text = lang.dlg[['label']][['8']], anchor = 'w', justify = 'left')
    en.out.dir <- tkentry(frDirectory, textvariable = dir.output, width = largeur1)
    bt.out.dir <- tkbutton(frDirectory, text = "...")

    tkconfigure(bt.in.dir, command = function(){
        tcl('wm', 'attributes', tt, topmost = FALSE)
        fileORdir2Save(dir.input, isFile = FALSE)
        tcl('wm', 'attributes', tt, topmost = TRUE)
    })
    tkconfigure(bt.out.dir, command = function(){
        tcl('wm', 'attributes', tt, topmost = FALSE)
        fileORdir2Save(dir.output, isFile = FALSE)
        tcl('wm', 'attributes', tt, topmost = TRUE)
    })

    tkgrid(txt.in.dir, row = 0, column = 0, sticky = 'we', rowspan = 1, columnspan = 5, padx = 1, pady = 1, ipadx = 1, ipady = 1)
    tkgrid(en.in.dir, row = 1, column = 0, sticky = 'we', rowspan = 1, columnspan = 4, padx = 0, pady = 1, ipadx = 1, ipady = 1)
    tkgrid(bt.in.dir, row = 1, column = 4, sticky = 'we', rowspan = 1, columnspan = 1, padx = 0, pady = 1, ipadx = 1, ipady = 1)
    tkgrid(txt.out.dir, row = 2, column = 0, sticky = 'we', rowspan = 1, columnspan = 5, padx = 1, pady = 1, ipadx = 1, ipady = 1)
    tkgrid(en.out.dir, row = 3, column = 0, sticky = 'we', rowspan = 1, columnspan = 4, padx = 0, pady = 1, ipadx = 1, ipady = 1)
    tkgrid(bt.out.dir, row = 3, column = 4, sticky = 'we', rowspan = 1, columnspan = 1, padx = 0, pady = 1, ipadx = 1, ipady = 1)

    ############################################

    frNetcdf <- ttklabelframe(frConversion, text = lang.dlg[['label']][['9']], labelanchor = "nw", relief = "groove", borderwidth = 2)

    nc.name <- tclVar(.cdtData$GalParams$nc.opts$varname)
    nc.unit <- tclVar(.cdtData$GalParams$nc.opts$varunit)
    nc.missval <- tclVar(.cdtData$GalParams$nc.opts$missval)
    nc.lname <- tclVar(.cdtData$GalParams$nc.opts$longname)

    stateNC <- if(.cdtData$GalParams$type.out == "nc") "normal" else "disabled"

    txt.nc.name <- tklabel(frNetcdf, text = 'Var.name', anchor = 'e', justify = 'right')
    en.nc.name <- tkentry(frNetcdf, textvariable = nc.name, width = largeur3, state = stateNC)
    txt.nc.unit <- tklabel(frNetcdf, text = 'Var.unit', anchor = 'w', justify = 'left')
    en.nc.unit <- tkentry(frNetcdf, textvariable = nc.unit, width = largeur4, state = stateNC)
    txt.nc.miss <- tklabel(frNetcdf, text = 'Miss.val', anchor = 'w', justify = 'left')
    en.nc.miss <- tkentry(frNetcdf, textvariable = nc.missval, width = largeur4, state = stateNC)
    txt.nc.lname <- tklabel(frNetcdf, text = 'Var.longname', anchor = 'e', justify = 'right')
    en.nc.lname <- tkentry(frNetcdf, textvariable = nc.lname, width = largeur2, state = stateNC)

    tkgrid(txt.nc.name, row = 0, column = 0, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
    tkgrid(en.nc.name, row = 0, column = 1, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
    tkgrid(txt.nc.unit, row = 0, column = 2, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
    tkgrid(en.nc.unit, row = 0, column = 3, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
    tkgrid(txt.nc.miss, row = 0, column = 4, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
    tkgrid(en.nc.miss, row = 0, column = 5, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
    tkgrid(txt.nc.lname, row = 1, column = 0, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
    tkgrid(en.nc.lname, row = 1, column = 1, sticky = 'we', rowspan = 1, columnspan = 5, padx = 1, pady = 1, ipadx = 1, ipady = 1)

    ############################################
    tkgrid(frConvFormat, row = 0, column = 0, sticky = '', padx = 1, pady = 3, ipadx = 1, ipady = 1)
    tkgrid(frDirectory, row = 1, column = 0, sticky = 'we', padx = 1, pady = 3, ipadx = 1, ipady = 1)
    tkgrid(frNetcdf, row = 2, column = 0, sticky = 'we', padx = 1, pady = 3, ipadx = 1, ipady = 1)

    ############################################
    
    tkgrid(frConversion, row = 0, column = 0, sticky = 'news', padx = 5, pady = 1, ipadx = 1, ipady = 1)

    ############################################

    bt.prm.OK <- ttkbutton(frMRG1, text = .cdtEnv$tcl$lang$global[['button']][['1']])
    bt.prm.CA <- ttkbutton(frMRG1, text = .cdtEnv$tcl$lang$global[['button']][['2']])

    tkconfigure(bt.prm.OK, command = function(){
        if(trimws(tclvalue(dir.input)) %in% c("", "NA")){
            cdt.tkmessageBox(tt, message = lang.dlg[['message']][['1']], icon = "warning", type = "ok")
            tkwait.window(tt)
        }else if(trimws(tclvalue(dir.output)) %in% c("", "NA")){
            cdt.tkmessageBox(tt, message = lang.dlg[['message']][['2']], icon = "warning", type = "ok")
            tkwait.window(tt)
        }else{
            .cdtData$GalParams$type.in <- switch(trimws(tclvalue(OriginData)),
                                                 "NetCDF" = "nc",
                                                 "GeoTiff" = "tif",
                                                 "ESRI .hdr Labelled" = "bil")

            .cdtData$GalParams$type.out <- switch(trimws(tclvalue(ConvertData)),
                                                  "NetCDF" = "nc",
                                                  "GeoTiff" = "tif",
                                                  "ESRI .hdr Labelled" = "bil")

            .cdtData$GalParams$dir.in <- trimws(tclvalue(dir.input))
            .cdtData$GalParams$dir.out <- trimws(tclvalue(dir.output))

            .cdtData$GalParams$nc.opts$varname <- trimws(tclvalue(nc.name))
            .cdtData$GalParams$nc.opts$varunit <- trimws(tclvalue(nc.unit))
            .cdtData$GalParams$nc.opts$missval <- as.numeric(trimws(tclvalue(nc.missval)))
            .cdtData$GalParams$nc.opts$longname <- trimws(tclvalue(nc.lname))

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

    tkgrid(bt.prm.OK, row = 0, column = 0, sticky = 'w', padx = 5, pady = 1, ipadx = 1, ipady = 1)
    tkgrid(bt.prm.CA, row = 0, column = 1, sticky = 'e', padx = 5, pady = 1, ipadx = 1, ipady = 1)

    ############################################

    tkgrid(frMRG0, row = 0, column = 0, sticky = 'nswe', rowspan = 1, columnspan = 2, padx = 1, pady = 1, ipadx = 1, ipady = 1)
    tkgrid(frMRG1, row = 1, column = 1, sticky = 'se', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)

    ############################3
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
