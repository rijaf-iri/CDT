
dataOperation_Inputs <- function(parent, data.type){
    if(WindowsOS()){
        largeur0 <- 42
        largeur1 <- 44
        data.w <- 410
        data.h <- 350
    }else{
        largeur0 <- 42
        largeur1 <- 43
        data.w <- 410
        data.h <- 350
    }

    xml.dlg <- file.path(.cdtDir$dirLocal, "languages", "cdtDataOperation_inputs_dlgBox.xml")
    lang.dlg <- cdtLanguageParse(xml.dlg, .cdtData$Config$lang.iso)

    ###################

    tt <- tktoplevel()
    tkgrab.set(tt)
    tkfocus(tt)

    frMRG0 <- tkframe(tt, relief = 'raised', borderwidth = 2)
    frMRG1 <- tkframe(tt)

    ############################################

    frInput <- bwTabScrollableFrame(frMRG0, hscrlwin = data.h, wscrlwin = data.w)
    frameBas <- tkframe(frMRG0)
    tkgrid(frameBas, sticky = '', padx = 1, pady = 1, ipadx = 1, ipady = 1)

    ############################################

    get.id.datasets <- function(widget){
        id.widget <- tclvalue(tkwinfo("parent", widget))
        id.frame <- sapply(.cdtData$GalParams$DATASETs, function(x) x$tcl$frame$ID)
        which(id.frame == id.widget)
    }

    add.new.datasets <- function(jj, state){
        .cdtData$GalParams$DATASETs[[jj]]$tcl$frame <- ttklabelframe(frInput, text = paste(lang.dlg[['label']][['1']], paste0("X", jj)), relief = 'groove')

        .cdtData$GalParams$DATASETs[[jj]]$tcl$input.file <- tclVar(.cdtData$GalParams$DATASETs[[jj]]$pars$dir)
        .cdtData$GalParams$DATASETs[[jj]]$tcl$constant <- tclVar(.cdtData$GalParams$DATASETs[[jj]]$const)

        is_const <- .cdtData$GalParams$DATASETs[[jj]]$const
        if(data.type == 'cdtstation'){
            il <- if(is_const) '5' else '2'
            stateSetNC <- "disabled"
            help1 <- if(is_const) '10' else '2'
            help2 <- '5'
        }else if(data.type == 'cdtdataset'){
            il <- if(is_const) '6' else '4'
            stateSetNC <- "disabled"
            help1 <- if(is_const) '11' else '3'
            help2 <- if(is_const) '5' else '6'
        }else{
            il <- if(is_const) '6' else '3'
            stateSetNC <- if(is_const) "disabled" else "normal"
            help1 <- if(is_const) '11' else '4'
            help2 <- if(is_const) '5' else '6'
        }
        txt.Label.var <- tclVar(lang.dlg[['label']][[il]])

        txt.Input <- tklabel(.cdtData$GalParams$DATASETs[[jj]]$tcl$frame, text = tclvalue(txt.Label.var), textvariable = txt.Label.var, anchor = 'w', justify = 'left')
        set.Input <- ttkbutton(.cdtData$GalParams$DATASETs[[jj]]$tcl$frame, text = .cdtEnv$tcl$lang$global[['button']][['5']], state = stateSetNC)

        if(data.type == 'cdtstation'){
            cb.en.Input <- ttkcombobox(.cdtData$GalParams$DATASETs[[jj]]$tcl$frame,
                                       values = unlist(openFile_ttkcomboList()),
                                       textvariable = .cdtData$GalParams$DATASETs[[jj]]$tcl$input.file,
                                       width = largeur0)
            addTo_all_Combobox_List(cb.en.Input)
        }else{
            if(is_const){
                cb.en.Input <- ttkcombobox(.cdtData$GalParams$DATASETs[[jj]]$tcl$frame,
                                           values = unlist(openFile_ttkcomboList()),
                                           textvariable = .cdtData$GalParams$DATASETs[[jj]]$tcl$input.file,
                                           width = largeur0)
                addTo_all_Combobox_List(cb.en.Input)
            }else{
                cb.en.Input <- tkentry(.cdtData$GalParams$DATASETs[[jj]]$tcl$frame,
                                       textvariable = .cdtData$GalParams$DATASETs[[jj]]$tcl$input.file,
                                       width = largeur1)
            }
        }
        bt.en.Input <- tkbutton(.cdtData$GalParams$DATASETs[[jj]]$tcl$frame, text = "...")
        bt.Remove <- ttkbutton(.cdtData$GalParams$DATASETs[[jj]]$tcl$frame, text = lang.dlg[['button']][['2']], state = state)
        chk.Constant <- tkcheckbutton(.cdtData$GalParams$DATASETs[[jj]]$tcl$frame, variable = .cdtData$GalParams$DATASETs[[jj]]$tcl$constant,
                                      text = lang.dlg[['checkbutton']][['1']], anchor = 'w', justify = 'left')

        tkgrid(txt.Input, row = 0, column = 0, sticky = 'we', rowspan = 1, columnspan = 3, padx = 1, pady = 0, ipadx = 1, ipady = 1)
        tkgrid(set.Input, row = 0, column = 3, sticky = 'we', rowspan = 1, columnspan = 2, padx = 1, pady = 0, ipadx = 1, ipady = 1)
        tkgrid(cb.en.Input, row = 1, column = 0, sticky = 'we', rowspan = 1, columnspan = 4, padx = 0, pady = 0, ipadx = 1, ipady = 1)
        tkgrid(bt.en.Input, row = 1, column = 4, sticky = 'w', rowspan = 1, columnspan = 1, padx = 0, pady = 0, ipadx = 1, ipady = 1)
        tkgrid(bt.Remove, row = 2, column = 3, sticky = 'we', rowspan = 1, columnspan = 2, padx = 1, pady = 0, ipadx = 1, ipady = 1)
        if(state == "normal"){
            tkgrid(chk.Constant, row = 2, column = 0, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 0, ipadx = 1, ipady = 1)
        }

        tkgrid(.cdtData$GalParams$DATASETs[[jj]]$tcl$frame)
        tcl("update")

        helpWidget(cb.en.Input, lang.dlg[['tooltip']][[help1]], lang.dlg[['status']][[help1]])
        helpWidget(bt.en.Input, lang.dlg[['tooltip']][[help2]], lang.dlg[['status']][[help2]])
        if(state == 'normal'){
            helpWidget(bt.Remove, lang.dlg[['tooltip']][['7']], lang.dlg[['status']][['7']])
            helpWidget(chk.Constant, lang.dlg[['tooltip']][['9']], lang.dlg[['status']][['9']])
        }
        if(data.type == 'cdtnetcdf'){
            helpWidget(set.Input, lang.dlg[['tooltip']][['8']], lang.dlg[['status']][['8']])
        }

        #####

        tkbind(chk.Constant, "<Button-1>", function(){
            ii <- get.id.datasets(chk.Constant)
            is_const <- tclvalue(.cdtData$GalParams$DATASETs[[ii]]$tcl$constant)
            is_const <- if(is_const == '1') FALSE else TRUE

            if(data.type == 'cdtstation'){
                il <- if(is_const) '5' else '2'
                stateSetNC <- "disabled"
                help1 <- if(is_const) '10' else '2'
                help2 <- '5'
            }else if(data.type == 'cdtdataset'){
                il <- if(is_const) '6' else '4'
                stateSetNC <- "disabled"
                help1 <- if(is_const) '11' else '3'
                help2 <- if(is_const) '5' else '6'
            }else{
                il <- if(is_const) '6' else '3'
                stateSetNC <- if(is_const) "disabled" else "normal"
                help1 <- if(is_const) '11' else '4'
                help2 <- if(is_const) '5' else '6'
            }

            tclvalue(txt.Label.var) <- lang.dlg[['label']][[il]]
            tkconfigure(set.Input, state = stateSetNC)

            if(data.type != 'cdtstation'){
                tkdestroy(cb.en.Input)

                if(is_const){
                    cb.en.Input <<- ttkcombobox(.cdtData$GalParams$DATASETs[[ii]]$tcl$frame,
                                                values = unlist(openFile_ttkcomboList()),
                                                textvariable = .cdtData$GalParams$DATASETs[[ii]]$tcl$input.file,
                                                width = largeur0)
                    addTo_all_Combobox_List(cb.en.Input)
                }else{
                    cb.en.Input <<- tkentry(.cdtData$GalParams$DATASETs[[ii]]$tcl$frame,
                                            textvariable = .cdtData$GalParams$DATASETs[[ii]]$tcl$input.file,
                                            width = largeur1)
                }

                tkgrid(cb.en.Input, row = 1, column = 0, sticky = 'we', rowspan = 1, columnspan = 4, padx = 0, pady = 0, ipadx = 1, ipady = 1)
            }

            helpWidget(cb.en.Input, lang.dlg[['tooltip']][[help1]], lang.dlg[['status']][[help1]])
            helpWidget(bt.en.Input, lang.dlg[['tooltip']][[help2]], lang.dlg[['status']][[help2]])
        })

        tkconfigure(set.Input, command = function(){
            tcl('wm', 'attributes', tt, topmost = FALSE)
            ii <- get.id.datasets(set.Input)
            .cdtData$GalParams$DATASETs[[ii]]$pars <- getInfoNetCDFData(tt, .cdtData$GalParams$DATASETs[[ii]]$pars,
                                                                        trimws(tclvalue(.cdtData$GalParams$DATASETs[[ii]]$tcl$input.file)))
            tcl('wm', 'attributes', tt, topmost = TRUE)
        })

        tkconfigure(bt.en.Input, command = function(){
            ii <- get.id.datasets(bt.en.Input)
            if(data.type == 'cdtstation'){
                tcl('wm', 'attributes', tt, topmost = FALSE)
                dat.opfiles <- getOpenFiles(tt)
                tcl('wm', 'attributes', tt, topmost = TRUE)
                if(!is.null(dat.opfiles)){
                    update.OpenFiles('ascii', dat.opfiles)
                    tclvalue(.cdtData$GalParams$DATASETs[[ii]]$tcl$input.file) <- dat.opfiles[[1]]
                }
            }else{
                is_const <- tclvalue(.cdtData$GalParams$DATASETs[[ii]]$tcl$constant)
                if(is_const == "1"){
                    tcl('wm', 'attributes', tt, topmost = FALSE)
                    nc.opfiles <- getOpenNetcdf(tt, initialdir = getwd())
                    tcl('wm', 'attributes', tt, topmost = TRUE)
                    if(!is.null(nc.opfiles)){
                        update.OpenFiles('netcdf', nc.opfiles)
                        tclvalue(.cdtData$GalParams$DATASETs[[ii]]$tcl$input.file) <- nc.opfiles[[1]]
                    }
                }else{
                    if(data.type == 'cdtdataset'){
                        tcl('wm', 'attributes', tt, topmost = FALSE)
                        path.rds <- tclvalue(tkgetOpenFile(initialdir = getwd(), filetypes = .cdtEnv$tcl$data$filetypes6))
                        tcl('wm', 'attributes', tt, topmost = TRUE)
                        tclvalue(.cdtData$GalParams$DATASETs[[ii]]$tcl$input.file) <- if(path.rds %in% c("", "NA") | is.na(path.rds)) "" else path.rds
                    }else{
                        tcl('wm', 'attributes', tt, topmost = FALSE)
                        dirnc <- tk_choose.dir(getwd(), "")
                        tcl('wm', 'attributes', tt, topmost = TRUE)
                        tclvalue(.cdtData$GalParams$DATASETs[[ii]]$tcl$input.file) <- if(dirnc %in% c("", "NA") | is.na(dirnc)) "" else dirnc
                    }
                }
            }
        })

        ####

        tkconfigure(bt.Remove, command = function(){
            ii <- get.id.datasets(bt.Remove)
            tkdestroy(.cdtData$GalParams$DATASETs[[ii]]$tcl$frame)
            .cdtData$GalParams$DATASETs[[ii]] <- NULL

            for(i in seq_along(.cdtData$GalParams$DATASETs)){
                textLab <- paste(lang.dlg[['label']][['1']], paste0("X", i))
                tkconfigure(.cdtData$GalParams$DATASETs[[i]]$tcl$frame, text = textLab)
            }

            tcl("update")
        })
    }

    ############################################

    for(jj in 1:length(.cdtData$GalParams$inputs)){
        .cdtData$GalParams$DATASETs[[jj]] <- list()
        inputFile <- .cdtData$GalParams$inputs[[paste0('file', jj)]]
        .cdtData$GalParams$DATASETs[[jj]]$pars <- inputFile
        inputConst <- .cdtData$GalParams$constant[[paste0('const', jj)]]
        .cdtData$GalParams$DATASETs[[jj]]$const <- inputConst

        stateRem <- if(jj == 1) "disabled" else "normal"
        add.new.datasets(jj, stateRem)
    }

    ############################################

    txt.AddData <- tklabel(frameBas, text = '', width = largeur1)
    bt.AddData <- tkbutton(frameBas, text = lang.dlg[['button']][['1']], bg = 'lightgreen')

    tkgrid(txt.AddData, row = 0, column = 0, sticky = 'we', padx = 1, pady = 1, ipadx = 1, ipady = 1)
    tkgrid(bt.AddData, row = 1, column = 0, sticky = 'we', padx = 1, pady = 1, ipadx = 1, ipady = 1)


    helpWidget(bt.AddData, lang.dlg[['tooltip']][['1']], lang.dlg[['status']][['1']])

    tkconfigure(bt.AddData, command = function(){
        jj <- length(.cdtData$GalParams$DATASETs) + 1
        .cdtData$GalParams$DATASETs[[jj]] <- list()
        .cdtData$GalParams$DATASETs[[jj]]$pars <- list(dir = "", sample = "", format = "rr_mrg_%s%s%s.nc")
        .cdtData$GalParams$DATASETs[[jj]]$const <- FALSE

        add.new.datasets(jj, 'normal')
    })

    ############################################

    bt.prm.OK <- ttkbutton(frMRG1, text = .cdtEnv$tcl$lang$global[['button']][['1']])
    bt.prm.CA <- ttkbutton(frMRG1, text = .cdtEnv$tcl$lang$global[['button']][['2']])

    ####
    tkconfigure(bt.prm.OK, command = function(){
        inputFiles <- lapply(.cdtData$GalParams$DATASETs, function(don){
                trimws(tclvalue(don$tcl$input.file))
        })
        numFiles <- seq_along(.cdtData$GalParams$DATASETs)
        nonFiles <- sapply(inputFiles, function(x) x %in% c("", "NA"))

        if(any(nonFiles)){
            nbf <- paste0(numFiles[nonFiles], collapse = ", ")
            msg <- paste(lang.dlg[['message']][['1']], "-", lang.dlg[['message']][['2']], ":", nbf)
            cdt.tkmessageBox(tt, message = msg, icon = "warning", type = "ok")
            tkwait.window(tt)
        }

        inputConst <- lapply(.cdtData$GalParams$DATASETs, function(don){
            if(tclvalue(don$tcl$constant) == '1') TRUE else FALSE
        })

        for(jj in seq_along(.cdtData$GalParams$DATASETs)){
            .cdtData$GalParams$inputs[[paste0('file', jj)]]$dir <- inputFiles[[jj]]

            if(.cdtData$GalParams$datatype == "cdtnetcdf"){
                .cdtData$GalParams$inputs[[paste0('file', jj)]]$sample <- .cdtData$GalParams$DATASETs[[jj]]$pars$sample
                .cdtData$GalParams$inputs[[paste0('file', jj)]]$format <- .cdtData$GalParams$DATASETs[[jj]]$pars$format
            }
            .cdtData$GalParams$constant[[paste0('const', jj)]] <- inputConst[[jj]]
        }

        for(jj in seq_along(.cdtData$GalParams$DATASETs))
            tkdestroy(.cdtData$GalParams$DATASETs[[jj]]$tcl$frame)

        .cdtData$GalParams$DATASETs <- NULL

        tkgrab.release(tt)
        tkdestroy(tt)
        tkfocus(parent)
    })

    tkconfigure(bt.prm.CA, command = function(){
        for(jj in seq_along(.cdtData$GalParams$DATASETs))
            tkdestroy(.cdtData$GalParams$DATASETs[[jj]]$tcl$frame)

        .cdtData$GalParams$DATASETs <- NULL

        tkgrab.release(tt)
        tkdestroy(tt)
        tkfocus(parent)
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
        tkfocus(parent)
    })
    tkwait.window(tt)
}
