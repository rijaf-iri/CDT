
adjGetInfoTempDownReanal <- function(){
    listOpenFiles <- openFile_ttkcomboList()
    if(WindowsOS()){
        largeur0 <- 29
        largeur1 <- 60
        largeur2 <- 45
    }else{
        largeur0 <- 22
        largeur1 <- 42
        largeur2 <- 35
    }

    ####################################

    # xml.dlg <- file.path(.cdtDir$dirLocal, "languages", "cdtTemp_BiasCorrect_dlgBox.xml")
    # lang.dlg <- cdtLanguageParse(xml.dlg, .cdtData$Config$lang.iso)

    ####################################

    tt <- tktoplevel()
    tkgrab.set(tt)
    tkfocus(tt)

    frMRG0 <- tkframe(tt, relief = 'raised', borderwidth = 2, padx = 3, pady = 3)
    frMRG1 <- tkframe(tt)

    ############################################

    frtimestep <- tkframe(frMRG0, relief = 'sunken', borderwidth = 2)

    file.period <- tclVar()
    CbperiodVAL <- .cdtEnv$tcl$lang$global[['combobox']][['1']][3:6]
    periodVAL <- c('daily', 'pentad', 'dekadal', 'monthly')
    tclvalue(file.period) <- CbperiodVAL[periodVAL %in% .cdtData$GalParams$period]

    cb.period <- ttkcombobox(frtimestep, values = CbperiodVAL, textvariable = file.period, width = largeur0)
    bt.DateRange <- ttkbutton(frtimestep, text = "Set Date Range", width = largeur0)

    #######

    tkgrid(cb.period, row = 0, column = 0, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
    tkgrid(bt.DateRange, row = 0, column = 1, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)

    helpWidget(cb.period, 'Select the time step of the data', 'Select the time step of the data')
    helpWidget(bt.DateRange, 'Set the start and end date of data to correct', 'Set the start and end date of data to correct')

    ###########

    tkconfigure(bt.DateRange, command = function(){
        tstep <- periodVAL[CbperiodVAL %in% str_trim(tclvalue(file.period))]
        tcl('wm', 'attributes', tt, topmost = FALSE)
        .cdtData$GalParams[["date.range"]] <- getInfoDateRange(tt, .cdtData$GalParams[["date.range"]], tstep)
        tcl('wm', 'attributes', tt, topmost = TRUE)
    })

    ############################################

    frameBias <- tkframe(frMRG0, relief = 'sunken', borderwidth = 2)

    cb.biasMthd <- c("Multiplicative Bias Time Step Variable", "Multiplicative Bias for Each Month",
                     "Quantile Mapping with Fitted Distribution", "Quantile Mapping with Empirical Distribution")
    val.biasMthd <- c("mbvar", "mbmon", "qmdist", "qmecdf")

    bias.method <- tclVar()
    tclvalue(bias.method) <- cb.biasMthd[val.biasMthd %in% .cdtData$GalParams$BIAS$method]
    bias.dir <- tclVar(.cdtData$GalParams$BIAS$dir)

    txt.bias <- tklabel(frameBias, text = 'Bias method', anchor = 'e', justify = 'right')
    cb.bias <- ttkcombobox(frameBias, values = cb.biasMthd, textvariable = bias.method, width = largeur2)

    txt.bias.dir <- tklabel(frameBias, text = "Directory of bias files", anchor = 'w', justify = 'left')
    en.bias.dir <- tkentry(frameBias, textvariable = bias.dir, width = largeur1)
    bt.bias.dir <- tkbutton(frameBias, text = "...")

    #######

    tkgrid(txt.bias, row = 0, column = 0, sticky = 'e', rowspan = 1, columnspan = 2, padx = 1, pady = 5, ipadx = 1, ipady = 1)
    tkgrid(cb.bias, row = 0, column = 2, sticky = 'we', rowspan = 1, columnspan = 4, padx = 1, pady = 5, ipadx = 1, ipady = 1)

    tkgrid(txt.bias.dir, row = 1, column = 0, sticky = 'we', rowspan = 1, columnspan = 6, padx = 1, pady = 1, ipadx = 1, ipady = 1)
    tkgrid(en.bias.dir, row = 2, column = 0, sticky = 'we', rowspan = 1, columnspan = 5, padx = 0, pady = 1, ipadx = 1, ipady = 1)
    tkgrid(bt.bias.dir, row = 2, column = 5, sticky = 'we', rowspan = 1, columnspan = 1, padx = 0, pady = 1, ipadx = 1, ipady = 1)

    helpWidget(cb.bias, 'Select the method used to calculate the Bias Factors or Parameters', 'Select the method used to calculate the Bias Factors or Parameters')
    helpWidget(en.bias.dir, 'Enter the full path to directory containing the bias files', 'Enter the full path to directory containing the bias files')

    #######

    tkconfigure(bt.bias.dir, command = function(){
        tcl('wm', 'attributes', tt, topmost = FALSE)
        dirbias <- tk_choose.dir(getwd(), "")
        tcl('wm', 'attributes', tt, topmost = TRUE)
        tclvalue(bias.dir) <- if(!is.na(dirbias)) dirbias else ""
    })


    ############################################

    frameInNC <- tkframe(frMRG0, relief = 'sunken', borderwidth = 2)

    dir.InNCDF <- tclVar(.cdtData$GalParams$TEMP$dir)

    txt.InNCDF <- tklabel(frameInNC, text = 'Downscaled data directory', anchor = 'w', justify = 'left')
    set.InNCDF <- ttkbutton(frameInNC, text = .cdtEnv$tcl$lang$global[['button']][['5']])
    en.InNCDF <- tkentry(frameInNC, textvariable = dir.InNCDF, width = largeur1)
    bt.InNCDF <- tkbutton(frameInNC, text = "...")

    ######
    tkgrid(txt.InNCDF, row = 0, column = 0, sticky = 'we', rowspan = 1, columnspan = 3, padx = 1, pady = 0, ipadx = 1, ipady = 1)
    tkgrid(set.InNCDF, row = 0, column = 3, sticky = 'we', rowspan = 1, columnspan = 2, padx = 1, pady = 0, ipadx = 1, ipady = 1)
    tkgrid(en.InNCDF, row = 1, column = 0, sticky = 'we', rowspan = 1, columnspan = 4, padx = 0, pady = 0, ipadx = 1, ipady = 1)
    tkgrid(bt.InNCDF, row = 1, column = 4, sticky = 'we', rowspan = 1, columnspan = 1, padx = 0, pady = 0, ipadx = 1, ipady = 1)

    helpWidget(en.InNCDF, 'Enter the full path to the directory containing the downscaled data', 'Enter the full path to the directory containing the downscaled data')
    helpWidget(bt.InNCDF, 'Or browse here', 'Or browse here')
    helpWidget(set.InNCDF, 'Setting NetCDF data options', 'Setting NetCDF data options')

    ######

    settingSNC <- .cdtData$GalParams$settingSNC
    tkconfigure(set.InNCDF, command = function(){
        tcl('wm', 'attributes', tt, topmost = FALSE)
        .cdtData$GalParams[["TEMP"]] <- getInfoNetcdfData(tt, .cdtData$GalParams[["TEMP"]],
                                                         str_trim(tclvalue(dir.InNCDF)),
                                                         str_trim(tclvalue(file.period)))
        tcl('wm', 'attributes', tt, topmost = TRUE)
        settingSNC <<- 1
    })

    tkconfigure(bt.InNCDF, command = function(){
        tcl('wm', 'attributes', tt, topmost = FALSE)
        dirnc <- tk_choose.dir(getwd(), "")
        tcl('wm', 'attributes', tt, topmost = TRUE)
        tclvalue(dir.InNCDF) <- if(!is.na(dirnc)) dirnc else ""
    })

    ############################################

    frSave <- tkframe(frMRG0, relief = 'sunken', borderwidth = 2)

    dir2save <- tclVar(.cdtData$GalParams$output$dir)
    outmrgff <- tclVar(.cdtData$GalParams$output$format)

    txt.dir2save <- tklabel(frSave, text = 'Directory to save result', anchor = 'w', justify = 'left')
    en.dir2save <- tkentry(frSave, textvariable = dir2save, width = largeur1)
    bt.dir2save <- tkbutton(frSave, text = "...")
    txt.outmrgff <- tklabel(frSave, text = 'Adjusted data filename format', anchor = 'w', justify = 'left')
    en.outmrgff <- tkentry(frSave, textvariable = outmrgff, width = largeur1)

    #####

    tkgrid(txt.dir2save, row = 0, column = 0, sticky = 'we', rowspan = 1, columnspan = 2, padx = 1, pady = 2, ipadx = 1, ipady = 1)
    tkgrid(en.dir2save, row = 1, column = 0, sticky = 'we', rowspan = 1, columnspan = 1, padx = 0, pady = 1, ipadx = 1, ipady = 1)
    tkgrid(bt.dir2save, row = 1, column = 1, sticky = 'we', rowspan = 1, columnspan = 1, padx = 0, pady = 1, ipadx = 1, ipady = 1)
    tkgrid(txt.outmrgff, row = 2, column = 0, sticky = 'we', rowspan = 1, columnspan = 2, padx = 1, pady = 1, ipadx = 1, ipady = 1)
    tkgrid(en.outmrgff, row = 3, column = 0, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)

    helpWidget(en.dir2save, 'Enter the full path to directory to save result', 'Enter the full path to directory to save result')
    helpWidget(bt.dir2save, 'or browse here', 'or browse here')
    helpWidget(en.outmrgff, 'Format of the adjusted temperature filenames in NetCDF, example: tmax_adj_1983011.nc', 'Format of the adjusted temperature filenames in NetCDF, example: tmax_adj_1983011.nc')

    #####

    tkconfigure(bt.dir2save, command = function(){
        tcl('wm', 'attributes', tt, topmost = FALSE)
        dir2savepth <- tk_choose.dir(.cdtData$GalParams$output$dir, "")
        tcl('wm', 'attributes', tt, topmost = TRUE)
        if(is.na(dir2savepth))
            tclvalue(dir2save) <- .cdtData$GalParams$output$dir
        else{
            dir.create(dir2savepth, showWarnings = FALSE, recursive = TRUE)
            tclvalue(dir2save) <- dir2savepth
        }
    })

    ############################################
    tkgrid(frtimestep, row = 0, column = 0, sticky = '', padx = 1, pady = 1, ipadx = 1, ipady = 1)
    tkgrid(frameBias, row = 1, column = 0, sticky = 'we', padx = 1, pady = 1, ipadx = 1, ipady = 1)
    tkgrid(frameInNC, row = 2, column = 0, sticky = 'we', padx = 1, pady = 1, ipadx = 1, ipady = 1)
    tkgrid(frSave, row = 3, column = 0, sticky = 'we', padx = 1, pady = 1, ipadx = 1, ipady = 1)

    ############################################

    bt.prm.OK <- ttkbutton(frMRG1, text = .cdtEnv$tcl$lang$global[['button']][['1']])
    bt.prm.CA <- ttkbutton(frMRG1, text = .cdtEnv$tcl$lang$global[['button']][['2']])

    #######

    tkconfigure(bt.prm.OK, command = function(){
        if(str_trim(tclvalue(dir.InNCDF)) %in% c("", "NA")){
            cdt.tkmessageBox(tt, message = "Select or enter the  directory containing the downscaled files", icon = "warning", type = "ok")
            tkwait.window(tt)
        }else if(str_trim(tclvalue(bias.dir)) %in% c("", "NA")){
            cdt.tkmessageBox(tt, message = "Enter the path to directory containing the Bias factors", icon = "warning", type = "ok")
            tkwait.window(tt)
        }else if(str_trim(tclvalue(dir2save)) %in% c("", "NA")){
            cdt.tkmessageBox(tt, message = "Browse or enter the path to directory to save results", icon = "warning", type = "ok")
            tkwait.window(tt)
        }else if(is.null(settingSNC)){
            cdt.tkmessageBox(tt, message = "You have to set the NetCDF files parameters", icon = "warning", type = "ok")
            tkwait.window(tt)
        }else{
            .cdtData$GalParams$period <- periodVAL[CbperiodVAL %in% str_trim(tclvalue(file.period))]
            
            .cdtData$GalParams$BIAS$dir <- str_trim(tclvalue(bias.dir))
            .cdtData$GalParams$BIAS$method  <- val.biasMthd[cb.biasMthd %in% str_trim(tclvalue(bias.method))]

            .cdtData$GalParams$TEMP$dir <- str_trim(tclvalue(dir.InNCDF))

            .cdtData$GalParams$output$dir <- str_trim(tclvalue(dir2save))
            .cdtData$GalParams$output$format <- str_trim(tclvalue(outmrgff))

            .cdtData$GalParams$settingSNC <- settingSNC

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

    ############################################

    tkwm.withdraw(tt)
    tcl('update')
    tt.w <- as.integer(tkwinfo("reqwidth", tt))
    tt.h <- as.integer(tkwinfo("reqheight", tt))
    tt.x <- as.integer(.cdtEnv$tcl$data$width.scr*0.5 - tt.w*0.5)
    tt.y <- as.integer(.cdtEnv$tcl$data$height.scr*0.5 - tt.h*0.5)
    tkwm.geometry(tt, paste0('+', tt.x, '+', tt.y))
    tkwm.transient(tt)
    tkwm.title(tt, 'Bias Correction - Settings')
    tkwm.deiconify(tt)
    tcl('wm', 'attributes', tt, topmost = TRUE)

    tkfocus(tt)
    tkbind(tt, "<Destroy>", function(){
        tkgrab.release(tt)
        tkfocus(.cdtEnv$tcl$main$win)
    })
    tkwait.window(tt)
    invisible()
}
