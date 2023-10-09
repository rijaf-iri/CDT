
Format_CDT_Input_Station_Data <- function(){
    if(WindowsOS()){
        largeur0 <- 48
        largeur1 <- 24
        largeur2 <- 23
    }else{
        largeur0 <- 45
        largeur1 <- 24
        largeur2 <- 23
    }

    xml.dlg <- file.path(.cdtDir$dirLocal, "languages", "cdtFormatInputStationData_dlgBox_0.xml")
    lang.dlg <- cdtLanguageParse(xml.dlg, .cdtData$Config$lang.iso)

    ############################################

    tt <- tktoplevel()
    tkgrab.set(tt)
    tkfocus(tt)

    frMRG0 <- tkframe(tt, relief = 'raised', borderwidth = 2)
    frMRG1 <- tkframe(tt)

    ############################################

    frDate <- tkframe(frMRG0, relief = 'sunken', borderwidth = 2)

    file.period <- tclVar()
    cb.periodVAL <- .cdtEnv$tcl$lang$global[['combobox']][['1']][c(3, 5, 6)]
    periodVAL <- c('daily', 'dekadal', 'monthly')
    tclvalue(file.period) <- cb.periodVAL[periodVAL %in% .cdtData$GalParams$tstep]

    #########
    cb.period <- ttkcombobox(frDate, values = cb.periodVAL, textvariable = file.period, width = largeur1)
    bt.period <- ttkbutton(frDate, text = lang.dlg[['button']][['1']], width = largeur2)

    tkconfigure(bt.period, command = function(){
        tstep <- periodVAL[cb.periodVAL %in% trimws(tclvalue(file.period))]
        .cdtData$GalParams[["date.range"]] <- getInfoDateRange(tt, .cdtData$GalParams[["date.range"]], tstep)
    })

    tkgrid(cb.period, row = 0, column = 0, sticky = 'we', padx = 1, pady = 3, ipadx = 1, ipady = 1)
    tkgrid(bt.period, row = 0, column = 1, sticky = 'we', padx = 1, pady = 3, ipadx = 1, ipady = 1)

    helpWidget(cb.period, lang.dlg[['tooltip']][['1']], lang.dlg[['status']][['1']])
    helpWidget(bt.period, lang.dlg[['tooltip']][['2']], lang.dlg[['status']][['2']])

    ############################################
    frMinPerc <- tkframe(frMRG0, relief = 'sunken', borderwidth = 2)

    minperc <- tclVar(.cdtData$GalParams$min.perc)

    minperc.lab <- tklabel(frMinPerc, text = lang.dlg[['label']][['1']], anchor = 'e', justify = 'right')
    minperc.ent <- tkentry(frMinPerc, width = 4, textvariable = minperc, justify = "right")

    tkgrid(minperc.lab, row = 1, column = 0, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 3, ipadx = 1, ipady = 1)
    tkgrid(minperc.ent, row = 1, column = 1, sticky = 'w', rowspan = 1, columnspan = 1, padx = 1, pady = 3, ipadx = 1, ipady = 1)

    helpWidget(minperc.ent, lang.dlg[['tooltip']][['3']], lang.dlg[['status']][['3']])

    ############################################
    frData <- ttklabelframe(frMRG0, text = lang.dlg[['label']][['6']], labelanchor = "nw", relief = 'sunken', borderwidth = 2)

    data.type <- tclVar()
    FilesTYPE <- lang.dlg[['combobox']][['1']]
    FilesTYPEin <- c('Multiple', 'Single')
    tclvalue(data.type) <- FilesTYPE[FilesTYPEin %in% .cdtData$GalParams$data.type]

    cb.dataType <- ttkcombobox(frData, values = FilesTYPE, textvariable = data.type, width = largeur1)
    bt.dataType <- ttkbutton(frData, text = lang.dlg[['button']][['2']], width = largeur2)

    if(tclvalue(data.type) == FilesTYPE[1]) dataType.Fun <- multipleFileCDTFormat
    if(tclvalue(data.type) == FilesTYPE[2]) dataType.Fun <- singleFileCDTFormat

    settingdone <- NULL
    tkconfigure(bt.dataType, command = function(){
        tcl('wm', 'attributes', tt, topmost = FALSE)
        dataType.Fun(tt, cb.periodVAL, tclvalue(file.period))
        tcl('wm', 'attributes', tt, topmost = TRUE)
        settingdone <<- 1
    })

    tkgrid(cb.dataType, row = 1, column = 0, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 3, ipadx = 1, ipady = 1)
    tkgrid(bt.dataType, row = 1, column = 1, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 3, ipadx = 1, ipady = 1)

    helpWidget(frData, lang.dlg[['tooltip']][['6']], lang.dlg[['status']][['6']])

    tkbind(cb.dataType,"<<ComboboxSelected>>", function(){
        if(tclvalue(data.type) == FilesTYPE[1]) dataType.Fun <- multipleFileCDTFormat
        if(tclvalue(data.type) == FilesTYPE[2]) dataType.Fun <- singleFileCDTFormat

        tkconfigure(bt.dataType, command = function(){
            tcl('wm', 'attributes', tt, topmost = FALSE)
            dataType.Fun(tt, cb.periodVAL, tclvalue(file.period))
            tcl('wm', 'attributes', tt, topmost = TRUE)
            settingdone <<- 1
        })
    })

    ############################################
    frSave <- tkframe(frMRG0, relief = 'sunken', borderwidth = 2)

    file.save1 <- tclVar(.cdtData$GalParams$IO.files$File2Save)

    txt.file.save <- tklabel(frSave, text = lang.dlg[['label']][['5']], anchor = 'w', justify = 'left')
    en.file.save <- tkentry(frSave, textvariable = file.save1, width = largeur0)
    bt.file.save <- tkbutton(frSave, text = "...")

    #####

    tkconfigure(bt.file.save, command = function(){
        tcl('wm', 'attributes', tt, topmost = FALSE)
        file2save1 <- tk_get_SaveFile(filetypes = .cdtEnv$tcl$data$filetypesA)
        tcl('wm', 'attributes', tt, topmost = TRUE)
        tclvalue(file.save1) <- if(is.na(file2save1)) .cdtData$GalParams$IO.files$File2Save else file2save1
    })

    #####

    tkgrid(txt.file.save, row = 0, column = 0, sticky = 'we', rowspan = 1, columnspan = 2, padx = 1, pady = 0, ipadx = 1, ipady = 1)
    tkgrid(en.file.save, row = 1, column = 0, sticky = 'we', rowspan = 1, columnspan = 1, padx = 0, pady = 0, ipadx = 1, ipady = 1)
    tkgrid(bt.file.save, row = 1, column = 1, sticky = 'we', rowspan = 1, columnspan = 1, padx = 0, pady = 0, ipadx = 1, ipady = 1)

    helpWidget(en.file.save, lang.dlg[['tooltip']][['4']], lang.dlg[['status']][['4']])
    helpWidget(bt.file.save, lang.dlg[['tooltip']][['5']], lang.dlg[['status']][['5']])

    ############################################
    tkgrid(frDate, row = 0, column = 0, sticky = 'we', padx = 5, pady = 3, ipadx = 1, ipady = 1)
    tkgrid(frMinPerc, row = 1, column = 0, sticky = '', padx = 5, pady = 3, ipadx = 1, ipady = 1)
    tkgrid(frData, row = 2, column = 0, sticky = 'we', padx = 5, pady = 3, ipadx = 1, ipady = 1)
    tkgrid(frSave, row = 3, column = 0, sticky = 'we', padx = 5, pady = 3, ipadx = 1, ipady = 1)

    ############################################

    bt.prm.OK <- ttkbutton(frMRG1, text = .cdtEnv$tcl$lang$global[['button']][['1']])
    bt.prm.CA <- ttkbutton(frMRG1, text = .cdtEnv$tcl$lang$global[['button']][['2']])

    #######

    tkconfigure(bt.prm.OK, command = function(){
        if(trimws(tclvalue(file.save1)) %in% c("" , "NA")){
            cdt.tkmessageBox(tt, message = lang.dlg[['message']][['1']], icon = "warning", type = "ok")
            tkwait.window(tt)
        }else{
            .cdtData$GalParams$tstep <- periodVAL[cb.periodVAL %in% trimws(tclvalue(file.period))]
            .cdtData$GalParams$data.type <- FilesTYPEin[FilesTYPE %in% trimws(tclvalue(data.type))]

            .cdtData$GalParams$IO.files$File2Save <- trimws(tclvalue(file.save1))
            .cdtData$GalParams$min.perc <- as.numeric(trimws(tclvalue(minperc)))

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
}

################################################################

multipleFileCDTFormat <- function(top.win, tstep.list, tstep){
    listOpenFiles <- openFile_ttkcomboList()
    if(WindowsOS()){
        largeur0 <- 40
        largeur1 <- 16
    }else{
        largeur0 <- 38
        largeur1 <- 18
    }

    xml.dlg <- file.path(.cdtDir$dirLocal, "languages", "cdtFormatInputStationData_dlgBox_1.xml")
    lang.dlg <- cdtLanguageParse(xml.dlg, .cdtData$Config$lang.iso)

    ###################

    tt1 <- tktoplevel()
    tkgrab.set(tt1)
    tkfocus(tt1)

    frMRG0 <- tkframe(tt1, relief = 'raised', borderwidth = 2)
    frMRG1 <- tkframe(tt1)

    ###################

    frFileFormat <- tkframe(frMRG0, relief = 'sunken', borderwidth = 2)

    ####
    fr.fileformat1 <- ttklabelframe(frFileFormat, text = lang.dlg[['label']][['1']], labelanchor = "nw", relief = "groove", borderwidth = 2)

    rbffrmt <- tclVar(.cdtData$GalParams$Multiple.File$file.format)

    ffrmt1 <- tkradiobutton(fr.fileformat1, text = lang.dlg[['radiobutton']][['1']][1], anchor = 'w', justify = 'left')
    ffrmt2 <- tkradiobutton(fr.fileformat1, text = lang.dlg[['radiobutton']][['1']][2], anchor = 'w', justify = 'left', width = largeur1)

    tkconfigure(ffrmt1, variable = rbffrmt, value = "1")
    tkconfigure(ffrmt2, variable = rbffrmt, value = "3")

    tkgrid(ffrmt1, row = 0, column = 0, sticky = "we")
    tkgrid(ffrmt2, row = 1, column = 0, sticky = "we")

    helpWidget(ffrmt1, lang.dlg[['tooltip']][['1']], lang.dlg[['status']][['1']])
    helpWidget(ffrmt2, lang.dlg[['tooltip']][['2']], lang.dlg[['status']][['2']])

    #####
    fr.fileformat2 <- ttklabelframe(frFileFormat, text = lang.dlg[['label']][['2']], labelanchor = "nw", relief = "groove", borderwidth = 2)

    rbdtfrmt <- tclVar(.cdtData$GalParams$Multiple.File$date.format)

    if(tstep == tstep.list[1]){
        txtdtfrmt1 <- "YYYYMMDD"
        txtdtfrmt2 <- "YYYY MM DD"
    }
    if(tstep == tstep.list[2]){
        txtdtfrmt1 <- "YYYYMMD"
        txtdtfrmt2 <- "YYYY MM D"
    }
    if(tstep == tstep.list[3]){
        txtdtfrmt1 <- "YYYYMM"
        txtdtfrmt2 <- "YYYY MM"
    }

    dtfrmt1 <- tkradiobutton(fr.fileformat2, text = txtdtfrmt1, anchor = 'w', justify = 'left')
    dtfrmt2 <- tkradiobutton(fr.fileformat2, text = txtdtfrmt2, anchor = 'w', justify = 'left', width = largeur1)
    dtfrmt3 <- tklabel(fr.fileformat2, text = '')

    tkconfigure(dtfrmt1, variable = rbdtfrmt, value = "1")
    tkconfigure(dtfrmt2, variable = rbdtfrmt, value = "3")

    tkgrid(dtfrmt1, row = 0, column = 0, sticky = "we")
    tkgrid(dtfrmt2, row = 1, column = 0, sticky = "we")
    tkgrid(dtfrmt3, row = 3, column = 0, sticky = "we")

    helpWidget(dtfrmt1, lang.dlg[['tooltip']][['3']], lang.dlg[['status']][['3']])
    helpWidget(dtfrmt2, lang.dlg[['tooltip']][['4']], lang.dlg[['status']][['4']])

    ###################
    tkgrid(fr.fileformat1, row = 0, column = 0, sticky = 'nswe', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
    tkgrid(fr.fileformat2, row = 0, column = 1, sticky = 'nswe', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)

    ###################

    frInput <- tkframe(frMRG0, relief = 'sunken', borderwidth = 2)

    dir.stn <- tclVar(.cdtData$GalParams$IO.files$STN.dir)
    file.sample <- tclVar(.cdtData$GalParams$IO.files$STN.sample.file)
    file.coords <- tclVar(.cdtData$GalParams$IO.files$STN.coords.file)
    include.elv <- tclVar(.cdtData$GalParams$Multiple.File$include.elev)

    txt.dir.stn <- tklabel(frInput, text = lang.dlg[['label']][['3']], anchor = 'w', justify = 'left')
    en.dir.stn <- tkentry(frInput, textvariable = dir.stn)
    bt.dir.stn <- tkbutton(frInput, text = "...")

    txt.file.stn.sample <- tklabel(frInput, text = lang.dlg[['label']][['4']], anchor = 'w', justify = 'left')
    cb.file.stn.sample <- ttkcombobox(frInput, values = unlist(listOpenFiles), textvariable = file.sample, width = largeur0)
    bt.file.stn.sample <- tkbutton(frInput, text = "...")

    txt.file.stn.info <- tklabel(frInput, text = lang.dlg[['label']][['5']], anchor = 'w', justify = 'left')
    cb.file.stn.info <- ttkcombobox(frInput, values = unlist(listOpenFiles), textvariable = file.coords, width = largeur0)
    bt.file.stn.info <- tkbutton(frInput, text = "...")

    chk.include.elv <- tkcheckbutton(frInput, variable = include.elv, text = lang.dlg[['checkbutton']][['1']], anchor = 'w', justify = 'left')

    ###########
    tkconfigure(bt.dir.stn, command = function(){
        tcl('wm', 'attributes', tt1, topmost = FALSE)
        dir4stn <- tk_choose.dir(.cdtData$GalParams$IO.files$STN.dir, "")
        tclvalue(dir.stn) <- if(!is.na(dir4stn)) dir4stn else ""
        tcl('wm', 'attributes', tt1, topmost = TRUE)
    })

    tkconfigure(bt.file.stn.sample, command = function(){
        tcl('wm', 'attributes', tt1, topmost = FALSE)
        dat.opfiles <- getOpenFiles(tt1, initialdir = tclvalue(dir.stn))
        tcl('wm', 'attributes', tt1, topmost = TRUE)
        if(!is.null(dat.opfiles)){
            update.OpenFiles('ascii', dat.opfiles)
            listOpenFiles[[length(listOpenFiles) + 1]] <<- dat.opfiles[[1]]
            tclvalue(file.sample) <- dat.opfiles[[1]]
            lapply(list(cb.file.stn.sample, cb.file.stn.info), tkconfigure, values = unlist(listOpenFiles))
        }
    })

    tkconfigure(bt.file.stn.info, command = function(){
        tcl('wm', 'attributes', tt1, topmost = FALSE)
        dat.opfiles <- getOpenFiles(tt1)
        tcl('wm', 'attributes', tt1, topmost = TRUE)
        if(!is.null(dat.opfiles)){
            update.OpenFiles('ascii', dat.opfiles)
            listOpenFiles[[length(listOpenFiles)+1]] <<- dat.opfiles[[1]]
            tclvalue(file.coords) <- dat.opfiles[[1]]
            lapply(list(cb.file.stn.sample, cb.file.stn.info), tkconfigure, values = unlist(listOpenFiles))
        }
    })

    ###########

    tkgrid(txt.dir.stn, row = 0, column = 0, sticky = 'we', rowspan = 1, columnspan = 5, padx = 1, pady = 0, ipadx = 1, ipady = 1)
    tkgrid(en.dir.stn, row = 1, column = 0, sticky = 'we', rowspan = 1, columnspan = 4, padx = 0, pady = 0, ipadx = 1, ipady = 1)
    tkgrid(bt.dir.stn, row = 1, column = 4, sticky = 'we', rowspan = 1, columnspan = 1, padx = 0, pady = 0, ipadx = 1, ipady = 1)
    tkgrid(txt.file.stn.sample, row = 2, column = 0, sticky = 'we', rowspan = 1, columnspan = 5, padx = 1, pady = 0, ipadx = 1, ipady = 1)
    tkgrid(cb.file.stn.sample, row = 3, column = 0, sticky = 'we', rowspan = 1, columnspan = 4, padx = 0, pady = 0, ipadx = 1, ipady = 1)
    tkgrid(bt.file.stn.sample, row = 3, column = 4, sticky = 'we', rowspan = 1, columnspan = 1, padx = 0, pady = 0, ipadx = 1, ipady = 1)
    tkgrid(txt.file.stn.info, row = 4, column = 0, sticky = 'we', rowspan = 1, columnspan = 5, padx = 1, pady = 0, ipadx = 1, ipady = 1)
    tkgrid(cb.file.stn.info, row = 5, column = 0, sticky = 'we', rowspan = 1, columnspan = 4, padx = 0, pady = 0, ipadx = 1, ipady = 1)
    tkgrid(bt.file.stn.info, row = 5, column = 4, sticky = 'we', rowspan = 1, columnspan = 1, padx = 0, pady = 0, ipadx = 1, ipady = 1)
    tkgrid(chk.include.elv, row = 6, column = 0, sticky = 'we', rowspan = 1, columnspan = 5, padx = 1, pady = 1, ipadx = 1, ipady = 1)

    ###########

    helpWidget(en.dir.stn, lang.dlg[['tooltip']][['5']], lang.dlg[['status']][['5']])
    helpWidget(bt.dir.stn, lang.dlg[['tooltip']][['6']], lang.dlg[['status']][['6']])
    helpWidget(cb.file.stn.sample, lang.dlg[['tooltip']][['7']], lang.dlg[['status']][['7']])
    helpWidget(bt.file.stn.sample, lang.dlg[['tooltip']][['8']], lang.dlg[['status']][['8']])
    helpWidget(cb.file.stn.info, lang.dlg[['tooltip']][['9']], lang.dlg[['status']][['9']])
    helpWidget(bt.file.stn.info, lang.dlg[['tooltip']][['10']], lang.dlg[['status']][['10']])
    helpWidget(chk.include.elv, lang.dlg[['tooltip']][['11']], lang.dlg[['status']][['11']])

    ###################
    tkgrid(frInput, row = 0, column = 0, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
    tkgrid(frFileFormat, row = 1, column = 0, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)

    ################################

    bt.prm.OK <- ttkbutton(frMRG1, text = .cdtEnv$tcl$lang$global[['button']][['1']])
    bt.prm.CA <- ttkbutton(frMRG1, text = .cdtEnv$tcl$lang$global[['button']][['2']])

    tkconfigure(bt.prm.OK, command = function(){
        if(trimws(tclvalue(dir.stn)) == "" | trimws(tclvalue(dir.stn)) == "NA"){
            cdt.tkmessageBox(tt1, message = lang.dlg[['message']][['1']], icon = "warning", type = "ok")
            tkwait.window(tt1)
        }else if(trimws(tclvalue(file.sample)) == ""){
            cdt.tkmessageBox(tt1, message = lang.dlg[['message']][['2']], icon = "warning", type = "ok")
            tkwait.window(tt1)
        }else if(trimws(tclvalue(file.coords)) == ""){
            cdt.tkmessageBox(tt1, message = lang.dlg[['message']][['3']], icon = "warning", type = "ok")
            tkwait.window(tt1)
        }else{
            .cdtData$GalParams$IO.files$STN.dir <- trimws(tclvalue(dir.stn))
            .cdtData$GalParams$IO.files$STN.sample.file <- trimws(tclvalue(file.sample))
            .cdtData$GalParams$IO.files$STN.coords.file <- trimws(tclvalue(file.coords))
            .cdtData$GalParams$Multiple.File$file.format <- tclvalue(rbffrmt)
            .cdtData$GalParams$Multiple.File$date.format <- tclvalue(rbdtfrmt)
            .cdtData$GalParams$Multiple.File$include.elev <- switch(tclvalue(include.elv), '0' = FALSE, '1' = TRUE)

            tkgrab.release(tt1)
            tkdestroy(tt1)
            tkfocus(top.win)
        }
    })

    tkconfigure(bt.prm.CA, command = function(){
        tkgrab.release(tt1)
        tkdestroy(tt1)
        tkfocus(top.win)
    })

    tkgrid(bt.prm.OK, row = 0, column = 0, sticky = 'w', padx = 5, pady = 1, ipadx = 1, ipady = 1)
    tkgrid(bt.prm.CA, row = 0, column = 1, sticky = 'e', padx = 5, pady = 1, ipadx = 1, ipady = 1)

    ################################
    tkgrid(frMRG0, row = 0, column = 0, sticky = 'nswe', rowspan = 1, columnspan = 2, padx = 1, pady = 1, ipadx = 1, ipady = 1)
    tkgrid(frMRG1, row = 1, column = 1, sticky = 'se', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)

    tkwm.withdraw(tt1)
    tcl('update')
    tt.w <- as.integer(tkwinfo("reqwidth", tt1))
    tt.h <- as.integer(tkwinfo("reqheight", tt1))
    tt.x <- as.integer(.cdtEnv$tcl$data$width.scr*0.5 - tt.w*0.5)
    tt.y <- as.integer(.cdtEnv$tcl$data$height.scr*0.5 - tt.h*0.5)
    tkwm.geometry(tt1, paste0('+', tt.x, '+', tt.y))
    tkwm.transient(tt1)
    tkwm.title(tt1, lang.dlg[['title']])
    tkwm.deiconify(tt1)
    tcl('wm', 'attributes', tt1, topmost = TRUE)

    tkfocus(tt1)
    tkbind(tt1, "<Destroy>", function(){
        tkgrab.release(tt1)
        tkfocus(top.win)
    })
    tkwait.window(tt1)
}

################################################################

singleFileCDTFormat <- function(top.win, tstep.list, tstep){
    listOpenFiles <- openFile_ttkcomboList()
    if(WindowsOS()){
        largeur0 <- 43
        largeur1 <- 20
    }else{
        largeur0 <- 41
        largeur1 <- 20
    }

    xml.dlg <- file.path(.cdtDir$dirLocal, "languages", "cdtFormatInputStationData_dlgBox_2.xml")
    lang.dlg <- cdtLanguageParse(xml.dlg, .cdtData$Config$lang.iso)

    ###################

    tt1 <- tktoplevel()
    tkgrab.set(tt1)
    tkfocus(tt1)

    frMRG0 <- tkframe(tt1, relief = 'raised', borderwidth = 2)
    frMRG1 <- tkframe(tt1)

    ###################

    frInput <- tkframe(frMRG0, relief = 'sunken', borderwidth = 2)

    file.stnfl <- tclVar(.cdtData$GalParams$IO.files$STN.single.file)
    file.coords <- tclVar(.cdtData$GalParams$IO.files$STN.coords.file)
    include.elv <- tclVar(.cdtData$GalParams$Single.File$include.elev)
    coords.infile <- tclVar(.cdtData$GalParams$Single.File$coords.included)

    statecrds <- if(tclvalue(coords.infile) == '1') 'disabled' else 'normal'
    txt.stnfl <- tklabel(frInput, text = lang.dlg[['label']][['1']], anchor = 'w', justify = 'left')
    cb.stnfl <- ttkcombobox(frInput, values = unlist(listOpenFiles), textvariable = file.stnfl, width = largeur0)
    bt.stnfl <- tkbutton(frInput, text = "...")

    chk.coords.infile <- tkcheckbutton(frInput, variable = coords.infile, text = lang.dlg[['checkbutton']][['1']], anchor = 'w', justify = 'left')
    txt.coords <- tklabel(frInput, text = lang.dlg[['label']][['2']], anchor = 'w', justify = 'left')
    cb.coords <- ttkcombobox(frInput, values = unlist(listOpenFiles), textvariable = file.coords, state = statecrds, width = largeur0)
    bt.coords <- tkbutton(frInput, text = "...", state = statecrds)

    chk.include.elv <- tkcheckbutton(frInput, variable = include.elv, text = lang.dlg[['checkbutton']][['2']], anchor = 'w', justify = 'left')

    ######
    tkconfigure(bt.stnfl, command = function(){
        tcl('wm', 'attributes', tt1, topmost = FALSE)
        dat.opfiles <- getOpenFiles(tt1)
        tcl('wm', 'attributes', tt1, topmost = TRUE)
        if(!is.null(dat.opfiles)){
            update.OpenFiles('ascii', dat.opfiles)
            listOpenFiles[[length(listOpenFiles)+1]] <<- dat.opfiles[[1]]
            tclvalue(file.stnfl) <- dat.opfiles[[1]]
            lapply(list(cb.stnfl, cb.coords), tkconfigure, values = unlist(listOpenFiles))
        }
    })

    tkconfigure(bt.coords, command = function(){
        tcl('wm', 'attributes', tt1, topmost = FALSE)
        dat.opfiles <- getOpenFiles(tt1)
        tcl('wm', 'attributes', tt1, topmost = TRUE)
        if(!is.null(dat.opfiles)){
            update.OpenFiles('ascii', dat.opfiles)
            listOpenFiles[[length(listOpenFiles)+1]] <<- dat.opfiles[[1]]
            tclvalue(file.coords) <- dat.opfiles[[1]]
            lapply(list(cb.stnfl, cb.coords), tkconfigure, values = unlist(listOpenFiles))
        }
    })

    ######

    tkgrid(txt.stnfl, row = 0, column = 0, sticky = 'we', rowspan = 1, columnspan = 2, padx = 1, pady = 0, ipadx = 1, ipady = 1)
    tkgrid(cb.stnfl, row = 1, column = 0, sticky = 'we', rowspan = 1, columnspan = 1, padx = 0, pady = 0, ipadx = 1, ipady = 1)
    tkgrid(bt.stnfl, row = 1, column = 1, sticky = 'w', rowspan = 1, columnspan = 1, padx = 0, pady = 0, ipadx = 1, ipady = 1)
    tkgrid(chk.coords.infile, row = 2, column = 0, sticky = 'we', rowspan = 1, columnspan = 2, padx = 1, pady = 2, ipadx = 1, ipady = 1)

    tkgrid(txt.coords, row = 3, column = 0, sticky = 'we', rowspan = 1, columnspan = 2, padx = 1, pady = 0, ipadx = 1, ipady = 1)
    tkgrid(cb.coords, row = 4, column = 0, sticky = 'we', rowspan = 1, columnspan = 1, padx = 0, pady = 0, ipadx = 1, ipady = 1)
    tkgrid(bt.coords, row = 4, column = 1, sticky = 'w', rowspan = 1, columnspan = 1, padx = 0, pady = 0, ipadx = 1, ipady = 1)
    tkgrid(chk.include.elv, row = 5, column = 0, sticky = 'we', rowspan = 1, columnspan = 2, padx = 1, pady = 2, ipadx = 1, ipady = 1)

    ###################

    helpWidget(cb.stnfl, lang.dlg[['tooltip']][['1']], lang.dlg[['status']][['1']])
    helpWidget(bt.stnfl, lang.dlg[['tooltip']][['2']], lang.dlg[['status']][['2']])
    helpWidget(cb.coords, lang.dlg[['tooltip']][['3']], lang.dlg[['status']][['3']])
    helpWidget(bt.coords, lang.dlg[['tooltip']][['4']], lang.dlg[['status']][['4']])
    helpWidget(chk.include.elv, lang.dlg[['tooltip']][['5']], lang.dlg[['status']][['5']])

    ###################
    tkbind(chk.coords.infile, "<Button-1>", function(){
        if(tclvalue(coords.infile) == '1'){
            statecrds <- 'normal'
            statecrds1 <- 'disabled'
            if(tclvalue(include.elv) == '0') stateElv <- 'disabled'
        }else{
            statecrds <- 'disabled'
            statecrds1 <- 'normal'
            stateElv <- if(tclvalue(include.elv) == '0') 'disabled' else 'normal'
        }
        tkconfigure(cb.coords, state = statecrds)
        tkconfigure(bt.coords, state = statecrds)
        tkconfigure(en.col.lon, state = statecrds1)
        tkconfigure(en.col.lat, state = statecrds1)
        tkconfigure(en.col.elv, state = stateElv)
    })

    ###################

    frColIdx <- tkframe(frMRG0, relief = 'sunken', borderwidth = 2)

    stn.id <- tclVar(.cdtData$GalParams$Single.File$col.stn.id)
    stn.lon <- tclVar(.cdtData$GalParams$Single.File$col.stn.lon)
    stn.lat <- tclVar(.cdtData$GalParams$Single.File$col.stn.lat)
    stn.elv <- tclVar(.cdtData$GalParams$Single.File$col.stn.elv)
    stn.year <- tclVar(.cdtData$GalParams$Single.File$col.year)
    stn.mon <- tclVar(.cdtData$GalParams$Single.File$col.month)
    stn.day <- tclVar(.cdtData$GalParams$Single.File$col.day.dek)
    stn.data <- tclVar(.cdtData$GalParams$Single.File$col.start.data)

    nb.column <- tclVar()
    if(tstep == tstep.list[1]){
        cb.nbcolVAL <- c('1 column', '31 columns')
        tclvalue(nb.column) <- switch(trimws(.cdtData$GalParams$Single.File$nb.column),
                                        '1' = cb.nbcolVAL[1],
                                        '31' = cb.nbcolVAL[2],
                                            cb.nbcolVAL[2])
        statemon <- 'normal'
        statedaydek <- if(tclvalue(nb.column) == '1 column') 'normal' else 'disabled'
    }
    if(tstep == tstep.list[2]){
        cb.nbcolVAL <- c('1 column', '3 columns', '36 columns')
        tclvalue(nb.column) <- switch(trimws(.cdtData$GalParams$Single.File$nb.column),
                                        '1' = cb.nbcolVAL[1],
                                        '3' = cb.nbcolVAL[2],
                                        '36' = cb.nbcolVAL[3],
                                            cb.nbcolVAL[3])
        statemon <- if(tclvalue(nb.column) == '36 columns') 'disabled' else 'normal'
        statedaydek <- if(tclvalue(nb.column) == '1 column') 'normal' else 'disabled'
    }
    if(tstep == tstep.list[3]){
        cb.nbcolVAL <- c('1 column', '12 columns')
        tclvalue(nb.column) <- switch(trimws(.cdtData$GalParams$Single.File$nb.column),
                                        '1' = cb.nbcolVAL[1],
                                        '12' = cb.nbcolVAL[2],
                                            cb.nbcolVAL[2])
        statemon <- if(tclvalue(nb.column) == '12 columns') 'disabled' else 'normal'
        statedaydek <- 'disabled'
    }

    stateCrds <- if(.cdtData$GalParams$Single.File$coords.included) 'normal' else 'disabled'

    if(.cdtData$GalParams$Single.File$include.elev){
        stateElv <- if(.cdtData$GalParams$Single.File$coords.included) 'normal' else 'disabled'
    }else stateElv <- 'disabled'

    txt.nb.col <- tklabel(frColIdx, text = lang.dlg[['label']][['4']], anchor = 'e', justify = 'right')
    cb.nb.col <- ttkcombobox(frColIdx, values = cb.nbcolVAL, textvariable = nb.column, width = largeur1)

    txt.col.idx <- tklabel(frColIdx, text = lang.dlg[['label']][['3']], anchor = 'w', justify = 'left')

    txt.col.id <- tklabel(frColIdx, text = 'COL.ID', anchor = 'e', justify = 'right')
    txt.col.data <- tklabel(frColIdx, text = 'COL.DATA', anchor = 'e', justify = 'right')
    txt.col.lon <- tklabel(frColIdx, text = 'COL.LON', anchor = 'e', justify = 'right')
    txt.col.lat <- tklabel(frColIdx, text = 'COL.LAT', anchor = 'e', justify = 'right')
    txt.col.elv <- tklabel(frColIdx, text = 'COL.ELEV', anchor = 'e', justify = 'right')
    txt.col.year <- tklabel(frColIdx, text = 'COL.YEAR', anchor = 'e', justify = 'right')
    txt.col.mon <- tklabel(frColIdx, text = 'COL.MONTH', anchor = 'e', justify = 'right')
    txt.col.day <- tklabel(frColIdx, text = 'COL.DAY/DEK', anchor = 'e', justify = 'right')

    en.col.id <- tkentry(frColIdx, textvariable = stn.id, width = 4)
    en.col.data <- tkentry(frColIdx, textvariable = stn.data, width = 4)
    en.col.lon <- tkentry(frColIdx, textvariable = stn.lon, width = 3, state = stateCrds)
    en.col.lat <- tkentry(frColIdx, textvariable = stn.lat, width = 3, state = stateCrds)
    en.col.elv <- tkentry(frColIdx, textvariable = stn.elv, width = 3, state = stateElv)
    en.col.year <- tkentry(frColIdx, textvariable = stn.year, width = 3)
    en.col.mon <- tkentry(frColIdx, textvariable = stn.mon, width = 3, state = statemon)
    en.col.day <- tkentry(frColIdx, textvariable = stn.day, width = 3, state = statedaydek)

    ###################
    tkgrid(txt.nb.col, row = 0, column = 0, sticky = 'we', rowspan = 1, columnspan = 3, padx = 1, pady = 5, ipadx = 1, ipady = 1)
    tkgrid(cb.nb.col, row = 0, column = 3, sticky = 'we', rowspan = 1, columnspan = 3, padx = 1, pady = 5, ipadx = 1, ipady = 1)

    tkgrid(txt.col.idx, row = 1, column = 0, sticky = 'we', rowspan = 1, columnspan = 6, padx = 1, pady = 0, ipadx = 1, ipady = 1)

    tkgrid(txt.col.id, row = 2, column = 0, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
    tkgrid(en.col.id, row = 2, column = 1, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
    tkgrid(txt.col.data, row = 4, column = 0, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
    tkgrid(en.col.data, row = 4, column = 1, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)

    tkgrid(txt.col.lon, row = 2, column = 2, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
    tkgrid(en.col.lon, row = 2, column = 3, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
    tkgrid(txt.col.lat, row = 3, column = 2, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
    tkgrid(en.col.lat, row = 3, column = 3, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
    tkgrid(txt.col.elv, row = 4, column = 2, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
    tkgrid(en.col.elv, row = 4, column = 3, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)

    tkgrid(txt.col.year, row = 2, column = 4, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
    tkgrid(en.col.year, row = 2, column = 5, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
    tkgrid(txt.col.mon, row = 3, column = 4, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
    tkgrid(en.col.mon, row = 3, column = 5, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
    tkgrid(txt.col.day, row = 4, column = 4, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
    tkgrid(en.col.day, row = 4, column = 5, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)

    ###

    helpWidget(en.col.id, lang.dlg[['tooltip']][['6']], lang.dlg[['status']][['6']])
    helpWidget(en.col.data, lang.dlg[['tooltip']][['7']], lang.dlg[['status']][['7']])
    helpWidget(en.col.lon, lang.dlg[['tooltip']][['8']], lang.dlg[['status']][['8']])
    helpWidget(en.col.lat, lang.dlg[['tooltip']][['9']], lang.dlg[['status']][['9']])
    helpWidget(en.col.elv, lang.dlg[['tooltip']][['10']], lang.dlg[['status']][['10']])
    helpWidget(en.col.year, lang.dlg[['tooltip']][['11']], lang.dlg[['status']][['11']])
    helpWidget(en.col.mon, lang.dlg[['tooltip']][['12']], lang.dlg[['status']][['12']])
    helpWidget(en.col.day, lang.dlg[['tooltip']][['13']], lang.dlg[['status']][['13']])
    helpWidget(cb.nb.col, lang.dlg[['tooltip']][['14']], lang.dlg[['status']][['14']])

    ###################

    tkbind(chk.include.elv, "<Button-1>", function(){
        if(tclvalue(include.elv) == '0'){
            stateElv <- if(tclvalue(coords.infile) == '1') 'normal' else 'disabled'
        }else{
            if(tclvalue(coords.infile) == '0') stateElv <- 'disabled'
        }
        tkconfigure(en.col.elv, state = stateElv)
    })

    tkbind(cb.nb.col, "<<ComboboxSelected>>", function(){
        if(tstep == tstep.list[1]){
            statemon <- 'normal'
            statedaydek <- if(tclvalue(nb.column) == '1 column') 'normal' else 'disabled'
        }
        if(tstep == tstep.list[2]){
            statemon <- if(tclvalue(nb.column) == '36 columns') 'disabled' else 'normal'
            statedaydek <- if(tclvalue(nb.column) == '1 column') 'normal' else 'disabled'
        }
        if(tstep == tstep.list[3]){
            statemon <- if(tclvalue(nb.column) == '12 columns') 'disabled' else 'normal'
            statedaydek <- 'disabled'
        }
        tkconfigure(en.col.mon, state = statemon)
        tkconfigure(en.col.day, state = statedaydek)
    })

    ###################
    tkgrid(frInput, row = 0, column = 0, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
    tkgrid(frColIdx, row = 1, column = 0, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)

    ################################

    bt.prm.OK <- ttkbutton(frMRG1, text = .cdtEnv$tcl$lang$global[['button']][['1']])
    bt.prm.CA <- ttkbutton(frMRG1, text = .cdtEnv$tcl$lang$global[['button']][['2']])

    tkconfigure(bt.prm.OK, command = function(){
        if(trimws(tclvalue(file.stnfl)) == ""){
            cdt.tkmessageBox(tt1, message = lang.dlg[['message']][['1']], icon = "warning", type = "ok")
        }else if(tclvalue(coords.infile) == '0' & trimws(tclvalue(file.coords)) == ""){
            cdt.tkmessageBox(tt1, message = lang.dlg[['message']][['2']], icon = "warning", type = "ok")
            tkwait.window(tt1)
        }else{
            .cdtData$GalParams$IO.files$STN.single.file <- trimws(tclvalue(file.stnfl))
            .cdtData$GalParams$IO.files$STN.coords.file <- trimws(tclvalue(file.coords))
            .cdtData$GalParams$Single.File$include.elev <- switch(tclvalue(include.elv), '0' = FALSE, '1' = TRUE)
            .cdtData$GalParams$Single.File$coords.included <- switch(tclvalue(coords.infile), '0' = FALSE, '1' = TRUE)

            .cdtData$GalParams$Single.File$col.stn.id <- as.numeric(trimws(tclvalue(stn.id)))
            .cdtData$GalParams$Single.File$col.stn.lon <- as.numeric(trimws(tclvalue(stn.lon)))
            .cdtData$GalParams$Single.File$col.stn.lat <- as.numeric(trimws(tclvalue(stn.lat)))
            .cdtData$GalParams$Single.File$col.stn.elv <- as.numeric(trimws(tclvalue(stn.elv)))
            .cdtData$GalParams$Single.File$col.year <- as.numeric(trimws(tclvalue(stn.year)))
            .cdtData$GalParams$Single.File$col.month <- as.numeric(trimws(tclvalue(stn.mon)))
            .cdtData$GalParams$Single.File$col.day.dek <- as.numeric(trimws(tclvalue(stn.day)))
            .cdtData$GalParams$Single.File$col.start.data <- as.numeric(trimws(tclvalue(stn.data)))
            if(tstep == tstep.list[1]){
                .cdtData$GalParams$Single.File$nb.column <- switch(trimws(tclvalue(nb.column)),
                                                                '1 column' = 1, '31 columns' = 31)
            }
            if(tstep == tstep.list[2]){
                .cdtData$GalParams$Single.File$nb.column <- switch(trimws(tclvalue(nb.column)),
                                                                '1 column' = 1, '3 columns' = 3, '36 columns' = 36)
            }
            if(tstep == tstep.list[3]){
                .cdtData$GalParams$Single.File$nb.column <- switch(trimws(tclvalue(nb.column)),
                                                                '1 column' = 1, '12 columns' = 12)
            }

            tkgrab.release(tt1)
            tkdestroy(tt1)
            tkfocus(top.win)
        }
    })

    tkconfigure(bt.prm.CA, command = function(){
        tkgrab.release(tt1)
        tkdestroy(tt1)
        tkfocus(top.win)
    })

    tkgrid(bt.prm.OK, row = 0, column = 0, sticky = 'w', padx = 5, pady = 1, ipadx = 1, ipady = 1)
    tkgrid(bt.prm.CA, row = 0, column = 1, sticky = 'e', padx = 5, pady = 1, ipadx = 1, ipady = 1)

    ################################
    tkgrid(frMRG0, row = 0, column = 0, sticky = 'nswe', rowspan = 1, columnspan = 2, padx = 1, pady = 1, ipadx = 1, ipady = 1)
    tkgrid(frMRG1, row = 1, column = 1, sticky = 'se', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)

    tkwm.withdraw(tt1)
    tcl('update')
    tt.w <- as.integer(tkwinfo("reqwidth", tt1))
    tt.h <- as.integer(tkwinfo("reqheight", tt1))
    tt.x <- as.integer(.cdtEnv$tcl$data$width.scr*0.5 - tt.w*0.5)
    tt.y <- as.integer(.cdtEnv$tcl$data$height.scr*0.5 - tt.h*0.5)
    tkwm.geometry(tt1, paste0('+', tt.x, '+', tt.y))
    tkwm.transient(tt1)
    tkwm.title(tt1, lang.dlg[['title']])
    tkwm.deiconify(tt1)
    tcl('wm', 'attributes', tt1, topmost = TRUE)

    tkfocus(tt1)
    tkbind(tt1, "<Destroy>", function(){
        tkgrab.release(tt1)
        tkfocus(top.win)
    })
    tkwait.window(tt1)
}
