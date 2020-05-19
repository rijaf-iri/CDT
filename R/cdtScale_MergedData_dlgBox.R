
Merging_ScaleDataInfo <- function(){
    listOpenFiles <- openFile_ttkcomboList()
    if(WindowsOS()){
        largeur1 <- 29
        largeur2 <- 60
    }else{
        largeur1 <- 22
        largeur2 <- 42
    }

    ####################################

    xml.dlg <- file.path(.cdtDir$dirLocal, "languages", "cdtScale_MergedData_dlgBox.xml")
    lang.dlg <- cdtLanguageParse(xml.dlg, .cdtData$Config$lang.iso)

    ####################################

    tt <- tktoplevel()
    tkgrab.set(tt)
    tkfocus(tt)

    frMRG0 <- tkframe(tt, relief = 'raised', borderwidth = 2, padx = 3, pady = 3)
    frMRG1 <- tkframe(tt)

    ############################################

    frDate <- tkframe(frMRG0, relief = 'sunken', borderwidth = 2, padx = 3, pady = 3)

    file.period <- tclVar()
    CbperiodVAL <- .cdtEnv$tcl$lang$global[['combobox']][['1']][3:5]
    periodVAL <- c('daily', 'pentad', 'dekadal')
    tclvalue(file.period) <- CbperiodVAL[periodVAL %in% .cdtData$GalParams$mrg.data$tstep]

    cb.period <- ttkcombobox(frDate, values = CbperiodVAL, textvariable = file.period, width = largeur1)
    bt.DateRange <- ttkbutton(frDate, text = lang.dlg[['button']][['1']], width = largeur1)

    tkgrid(cb.period, row = 0, column = 0, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
    tkgrid(bt.DateRange, row = 0, column = 1, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)

    helpWidget(cb.period, lang.dlg[['tooltip']][['1']], lang.dlg[['status']][['1']])
    helpWidget(bt.DateRange, lang.dlg[['tooltip']][['2']], lang.dlg[['status']][['2']])

    tkconfigure(bt.DateRange, command = function(){
        tstep <- periodVAL[CbperiodVAL %in% str_trim(tclvalue(file.period))]
        tcl('wm', 'attributes', tt, topmost = FALSE)
        .cdtData$GalParams[["date.range"]] <- getInfoDateRange(tt, .cdtData$GalParams[["date.range"]], tstep)
        tcl('wm', 'attributes', tt, topmost = TRUE)
    })

    ############################################

    frmrgData <- tkframe(frMRG0, relief = 'sunken', borderwidth = 2, padx = 3, pady = 3)

    dir.mrgData <- tclVar(.cdtData$GalParams$mrg.data$dir)

    txt.mrgData <- tklabel(frmrgData, text = lang.dlg[['label']][['1']], anchor = 'w', justify = 'left')
    set.mrgData <- ttkbutton(frmrgData, text = .cdtEnv$tcl$lang$global[['button']][['5']])
    en.mrgData <- tkentry(frmrgData, textvariable = dir.mrgData, width = largeur2)
    bt.mrgData <- tkbutton(frmrgData, text = "...")

    tkgrid(txt.mrgData, row = 0, column = 0, sticky = 'we', rowspan = 1, columnspan = 3, padx = 1, pady = 1, ipadx = 1, ipady = 1)
    tkgrid(set.mrgData, row = 0, column = 3, sticky = 'we', rowspan = 1, columnspan = 2, padx = 1, pady = 1, ipadx = 1, ipady = 1)
    tkgrid(en.mrgData, row = 1, column = 0, sticky = 'we', rowspan = 1, columnspan = 4, padx = 0, pady = 1, ipadx = 1, ipady = 1)
    tkgrid(bt.mrgData, row = 1, column = 4, sticky = 'w', rowspan = 1, columnspan = 1, padx = 0, pady = 1, ipadx = 1, ipady = 1)

    helpWidget(en.mrgData, lang.dlg[['tooltip']][['3']], lang.dlg[['status']][['3']])
    helpWidget(bt.mrgData, lang.dlg[['tooltip']][['4']], lang.dlg[['status']][['4']])
    helpWidget(set.mrgData, lang.dlg[['tooltip']][['5']], lang.dlg[['status']][['5']])

    tkconfigure(set.mrgData, command = function(){
        tcl('wm', 'attributes', tt, topmost = FALSE)
        .cdtData$GalParams[["mrg.data"]] <- getInfoNetcdfData(tt, .cdtData$GalParams[["mrg.data"]],
                                                              str_trim(tclvalue(dir.mrgData)),
                                                              str_trim(tclvalue(file.period)))
        tcl('wm', 'attributes', tt, topmost = TRUE)
    })

    tkconfigure(bt.mrgData, command = function(){
        tcl('wm', 'attributes', tt, topmost = FALSE)
        dirmrg <- tk_choose.dir(getwd(), "")
        tclvalue(dir.mrgData) <- if(!is.na(dirmrg)) dirmrg else ""
        tcl('wm', 'attributes', tt, topmost = TRUE)
    })

    ############################################

    frScaleData <- tkframe(frMRG0, relief = 'sunken', borderwidth = 2, padx = 3, pady = 3)

    scale.dir <- tclVar(.cdtData$GalParams$scale.data$dir)

    txt.scaledata <- tklabel(frScaleData, text = lang.dlg[['label']][['2']], anchor = 'w', justify = 'left')
    set.scaledata <- ttkbutton(frScaleData, text = .cdtEnv$tcl$lang$global[['button']][['5']])
    en.scaledata <- tkentry(frScaleData, textvariable = scale.dir, width = largeur2)
    bt.scaledata <- tkbutton(frScaleData, text = "...")

    tkgrid(txt.scaledata, row = 0, column = 0, sticky = 'we', rowspan = 1, columnspan = 3, padx = 1, pady = 1, ipadx = 1, ipady = 1)
    tkgrid(set.scaledata, row = 0, column = 3, sticky = 'we', rowspan = 1, columnspan = 2, padx = 1, pady = 1, ipadx = 1, ipady = 1)
    tkgrid(en.scaledata, row = 1, column = 0, sticky = 'we', rowspan = 1, columnspan = 4, padx = 0, pady = 1, ipadx = 1, ipady = 1)
    tkgrid(bt.scaledata, row = 1, column = 4, sticky = 'w', rowspan = 1, columnspan = 1, padx = 0, pady = 1, ipadx = 1, ipady = 1)

    helpWidget(en.scaledata, lang.dlg[['tooltip']][['6']], lang.dlg[['status']][['6']])
    helpWidget(bt.scaledata, lang.dlg[['tooltip']][['4']], lang.dlg[['status']][['4']])
    helpWidget(set.scaledata, lang.dlg[['tooltip']][['5']], lang.dlg[['status']][['5']])

    tkconfigure(set.scaledata, command = function(){
        tcl('wm', 'attributes', tt, topmost = FALSE)
        .cdtData$GalParams[["scale.data"]] <- getInfoNetcdfData(tt, .cdtData$GalParams[["scale.data"]],
                                                                str_trim(tclvalue(scale.dir)),
                                                                str_trim(tclvalue(file.period)), scale = TRUE)
        tcl('wm', 'attributes', tt, topmost = TRUE)
    })

    tkconfigure(bt.scaledata, command = function(){
        tcl('wm', 'attributes', tt, topmost = FALSE)
        dirscale <- tk_choose.dir(getwd(), "")
        tcl('wm', 'attributes', tt, topmost = TRUE)
        tclvalue(scale.dir) <- if(!is.na(dirscale)) dirscale else ""
    })

    ############################################

    frScaleFun <- tkframe(frMRG0, relief = 'sunken', borderwidth = 2, padx = 3, pady = 3)

    scale.fun <- tclVar(.cdtData$GalParams$scale.data$fun)

    txt.scalefun <- tklabel(frScaleFun, text = lang.dlg[['label']][['3']], anchor = 'w', justify = 'left')
    cb.scalefun <- ttkcombobox(frScaleFun, values = c("sum", "mean"), textvariable = scale.fun, width = 8)

    tkgrid(txt.scalefun, row = 0, column = 0, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
    tkgrid(cb.scalefun, row = 0, column = 1, sticky = 'w', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)

    ############################################

    frSave <- tkframe(frMRG0, relief = 'sunken', borderwidth = 2, padx = 3, pady = 3)

    dir2save <- tclVar(.cdtData$GalParams$outdir)

    txt.dir2save <- tklabel(frSave, text = lang.dlg[['label']][['4']], anchor = 'w', justify = 'left')
    en.dir2save <- tkentry(frSave, textvariable = dir2save, width = largeur2)
    bt.dir2save <- tkbutton(frSave, text = "...")

    #####

    tkgrid(txt.dir2save, row = 0, column = 0, sticky = 'we', rowspan = 1, columnspan = 2, padx = 1, pady = 2, ipadx = 1, ipady = 1)
    tkgrid(en.dir2save, row = 1, column = 0, sticky = 'we', rowspan = 1, columnspan = 1, padx = 0, pady = 1, ipadx = 1, ipady = 1)
    tkgrid(bt.dir2save, row = 1, column = 1, sticky = 'w', rowspan = 1, columnspan = 1, padx = 0, pady = 1, ipadx = 1, ipady = 1)

    helpWidget(en.dir2save, lang.dlg[['tooltip']][['7']], lang.dlg[['status']][['7']])
    helpWidget(bt.dir2save, lang.dlg[['tooltip']][['4']], lang.dlg[['status']][['4']])

    #####

    tkconfigure(bt.dir2save, command = function(){
        dir2savepth <- tk_choose.dir(.cdtData$GalParams$outdir, "")
        if(is.na(dir2savepth)) tclvalue(dir2save) <- .cdtData$GalParams$outdir
        else{
            dir.create(dir2savepth, showWarnings = FALSE, recursive = TRUE)
            tclvalue(dir2save) <- dir2savepth
        }
    })

    ############################################
    tkgrid(frDate, row = 0, column = 0, sticky = '', padx = 1, pady = 1, ipadx = 1, ipady = 1)
    tkgrid(frmrgData, row = 1, column = 0, sticky = 'we', padx = 1, pady = 1, ipadx = 1, ipady = 1)
    tkgrid(frScaleData, row = 3, column = 0, sticky = 'we', padx = 1, pady = 1, ipadx = 1, ipady = 1)
    tkgrid(frScaleFun, row = 4, column = 0, sticky = '', padx = 1, pady = 1, ipadx = 1, ipady = 1)
    tkgrid(frSave, row = 5, column = 0, sticky = 'we', padx = 1, pady = 1, ipadx = 1, ipady = 1)

    ############################################

    bt.prm.OK <- ttkbutton(frMRG1, text = .cdtEnv$tcl$lang$global[['button']][['1']])
    bt.prm.CA <- ttkbutton(frMRG1, text = .cdtEnv$tcl$lang$global[['button']][['2']])

    #######

    tkconfigure(bt.prm.OK, command = function(){
        if(str_trim(tclvalue(dir.mrgData)) %in% c("", "NA")){
            cdt.tkmessageBox(tt, message = lang.dlg[['message']][['1']], icon = "warning", type = "ok")
            tkwait.window(tt)
        }else if(str_trim(tclvalue(scale.dir)) %in% c("", "NA")){
            cdt.tkmessageBox(tt, message = lang.dlg[['message']][['2']], icon = "warning", type = "ok")
            tkwait.window(tt)
        }else if(str_trim(tclvalue(dir2save)) %in% c("", "NA")){
            cdt.tkmessageBox(tt, message = lang.dlg[['message']][['3']], icon = "warning", type = "ok")
            tkwait.window(tt)
        }else{
            .cdtData$GalParams$mrg.data$tstep <- periodVAL[CbperiodVAL %in% str_trim(tclvalue(file.period))]

            .cdtData$GalParams$mrg.data$dir <- str_trim(tclvalue(dir.mrgData))
            .cdtData$GalParams$scale.data$dir <- str_trim(tclvalue(scale.dir))
            .cdtData$GalParams$scale.data$fun <- str_trim(tclvalue(scale.fun))
            .cdtData$GalParams$outdir <- str_trim(tclvalue(dir2save))

            .cdtData$GalParams$message <- lang.dlg[['message']]

            tkgrab.release(tt)
            tkdestroy(tt)
            tkfocus(.cdtEnv$tcl$main$win)
        }
    })

    #######

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
    invisible()
}
