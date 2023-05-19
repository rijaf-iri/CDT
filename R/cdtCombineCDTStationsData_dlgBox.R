merge2CDTdata_getParams <- function(){
    if(WindowsOS()){
        largeur1 <- 45
        largeur2 <- 30
        data.w <- 410
        data.h <- 320
    }else{
        largeur1 <- 45
        largeur2 <- 30
        data.w <- 410
        data.h <- 320
    }

    ############################################

    tt <- tktoplevel()
    tkgrab.set(tt)
    tkfocus(tt)

    frMRG0 <- tkframe(tt, relief = 'raised', borderwidth = 2)
    frMRG1 <- tkframe(tt)

    cbLists <- new.env()
    cbLists$cb <- list()

    ############################################

    xml.dlg <- file.path(.cdtDir$dirLocal, "languages", "cdtCombineCDTStationsData_dlgBox.xml")
    lang.dlg <- cdtLanguageParse(xml.dlg, .cdtData$Config$lang.iso)

    #############

    frInput <- bwTabScrollableFrame(frMRG0, hscrlwin = data.h, wscrlwin = data.w)
    frameBas <- tkframe(frMRG0)
    tkgrid(frameBas, sticky = 'swe', padx = 1, pady = 1, ipadx = 1, ipady = 1)

    #############

    add.new.datasets <- function(jj, state){
        .cdtData$GalParams$DATASETs[[jj]]$tcl$frame <- ttklabelframe(frInput, text = paste0(lang.dlg[['label']][['1']], " #", jj), relief = 'groove')

        .cdtData$GalParams$DATASETs[[jj]]$tcl$input.file <- tclVar(.cdtData$GalParams$DATASETs[[jj]]$pars$file)

        cb.datafile <- ttkcombobox(.cdtData$GalParams$DATASETs[[jj]]$tcl$frame, values = unlist(openFile_ttkcomboList()),
                                   textvariable = .cdtData$GalParams$DATASETs[[jj]]$tcl$input.file, width = largeur2)
        bt.datafile <- tkbutton(.cdtData$GalParams$DATASETs[[jj]]$tcl$frame, text = "...")
        bt.Remove <- ttkbutton(.cdtData$GalParams$DATASETs[[jj]]$tcl$frame, text = lang.dlg[['button']][['2']], state = state)

        tkgrid(cb.datafile, row = 0, column = 0, sticky = 'we', rowspan = 1, columnspan = 9, padx = 0, pady = 1, ipadx = 1, ipady = 1)
        tkgrid(bt.datafile, row = 0, column = 9, sticky = 'w', rowspan = 1, columnspan = 1, padx = 0, pady = 1, ipadx = 1, ipady = 1)
        tkgrid(bt.Remove, row = 0, column = 10, sticky = 'e', rowspan = 1, columnspan = 3, padx = 1, pady = 1, ipadx = 1, ipady = 1)

        tkgrid(.cdtData$GalParams$DATASETs[[jj]]$tcl$frame)

        cbLists$cb[[length(cbLists$cb) + 1]] <- cb.datafile

        #####

        tkconfigure(bt.datafile, command = function(){
            dat.opfiles <- getOpenFiles(tt)
            if(!is.null(dat.opfiles)){
                update.OpenFiles('ascii', dat.opfiles)
                tclvalue(.cdtData$GalParams$DATASETs[[jj]]$tcl$input.file) <- dat.opfiles[[1]]

                lapply(cbLists$cb, function(x){
                    if(as.integer(tkwinfo('exists', x)) == 1)
                        tkconfigure(x, values = unlist(openFile_ttkcomboList()))
                })

                .cdtData$GalParams$DATASETs[[jj]]$pars$file <- str_trim(tclvalue(.cdtData$GalParams$DATASETs[[jj]]$tcl$input.file))
            }
        })

        ####

        id.fr <- NULL
        tkbind(bt.Remove, "<Button-1>", function(){
            id.fr <<- tclvalue(tkwinfo("parent", bt.Remove))
            # jj <<- jj - 1
        })

        tkconfigure(bt.Remove, command = function(){
            id.frame <- sapply(.cdtData$GalParams$DATASETs, function(x) x$tcl$frame$ID)

            ii <- which(id.frame == id.fr)
            tkdestroy(.cdtData$GalParams$DATASETs[[ii]]$tcl$frame)
            .cdtData$GalParams$DATASETs[[ii]] <- NULL

            for(ii in seq_along(.cdtData$GalParams$DATASETs)){
                textLab <- paste0(lang.dlg[['label']][['1']], " #", ii)
                tkconfigure(.cdtData$GalParams$DATASETs[[ii]]$tcl$frame, text = textLab)
            }

            tcl("update")
        })
    }

    #######################################

    for(jj in 1:length(.cdtData$GalParams$inputs)){
        .cdtData$GalParams$DATASETs[[jj]] <- list()
        inputFile <- .cdtData$GalParams$inputs[[paste0('file', jj)]]
        .cdtData$GalParams$DATASETs[[jj]]$pars$file <- inputFile

        stateRem <- if(jj %in% 1:2) "disabled" else "normal"
        add.new.datasets(jj, stateRem)
    }

    #######################################

    bt.AddData <- tkbutton(frameBas, text = lang.dlg[['button']][['3']], bg = 'lightgreen')

    tkconfigure(bt.AddData, command = function(){
        jj <- length(.cdtData$GalParams$DATASETs) + 1

        .cdtData$GalParams$DATASETs[[jj]] <- list()
        .cdtData$GalParams$DATASETs[[jj]]$pars$file <- ""

        add.new.datasets(jj, 'normal')
    })

    #######################################

    frameSave <- ttklabelframe(frameBas, text = lang.dlg[['label']][['3']], relief = 'groove')

    file.save1 <- tclVar(.cdtData$GalParams$file2save)

    enFileSave <- tkentry(frameSave, textvariable = file.save1, width = largeur1)
    btFileSave <- tkbutton(frameSave, text = "...")

    tkgrid(enFileSave, row = 0, column = 0, sticky = 'we', rowspan = 1, columnspan = 1, padx = 0, pady = 0, ipadx = 1, ipady = 1)
    tkgrid(btFileSave, row = 0, column = 1, sticky = 'we', rowspan = 1, columnspan = 1, padx = 0, pady = 0, ipadx = 1, ipady = 1)

    tkconfigure(btFileSave, command = function(){
        tcl('wm', 'attributes', tt, topmost = FALSE)
        file2save1 <- tk_get_SaveFile(filetypes = .cdtEnv$tcl$data$filetypesA)
        tclvalue(file.save1) <- if(is.na(file2save1)) "" else file2save1
        tcl('wm', 'attributes', tt, topmost = TRUE)
    })

    ##############################################

    tkgrid(bt.AddData, sticky = 'we', padx = 1, pady = 1, ipadx = 1, ipady = 1)
    tkgrid(frameSave, sticky = 'we', padx = 1, pady = 1, ipadx = 1, ipady = 1)

    ############################################

    bt.prm.OK <- ttkbutton(frMRG1, text = .cdtEnv$tcl$lang$global[['button']][['1']])
    bt.prm.CA <- ttkbutton(frMRG1, text = .cdtEnv$tcl$lang$global[['button']][['2']])

    tkconfigure(bt.prm.OK, command = function(){
        inputFiles <- lapply(.cdtData$GalParams$DATASETs, function(don){
                str_trim(tclvalue(don$tcl$input.file))
        })
        numFiles <- seq_along(.cdtData$GalParams$DATASETs)
        nonFiles <- sapply(inputFiles, function(x) x %in% c("", "NA"))

        if(any(nonFiles)){
            nbf <- paste0(numFiles[nonFiles], collapse = ", ")
            msg <- paste(lang.dlg[['message']][['8']], "-", lang.dlg[['message']][['9']], ":", nbf)
            cdt.tkmessageBox(tt, message = msg, icon = "warning", type = "ok")
            tkwait.window(tt)
        }else if(length(numFiles) < 2){
            cdt.tkmessageBox(tt, message = lang.dlg[['message']][['11']], icon = "warning", type = "ok")
            tkwait.window(tt)
        }else if(str_trim(tclvalue(file.save1)) %in% c("", "NA")){
            cdt.tkmessageBox(tt, message = lang.dlg[['message']][['10']], icon = "warning", type = "ok")
            tkwait.window(tt)
        }else{
            .cdtData$GalParams$inputs <- inputFiles
            names(.cdtData$GalParams$inputs) <- paste0("file", numFiles)

            .cdtData$GalParams$file2save <- str_trim(tclvalue(file.save1))
            .cdtData$GalParams$message <- lang.dlg[['message']]

            for(jj in seq_along(.cdtData$GalParams$DATASETs))
                tkdestroy(.cdtData$GalParams$DATASETs[[jj]]$tcl$frame)

            .cdtData$GalParams$DATASETs <- NULL

            tkgrab.release(tt)
            tkdestroy(tt)
            tkfocus(.cdtEnv$tcl$main$win)
        }
    })

    tkconfigure(bt.prm.CA, command = function(){
        for(jj in seq_along(.cdtData$GalParams$DATASETs))
            tkdestroy(.cdtData$GalParams$DATASETs[[jj]]$tcl$frame)

        .cdtData$GalParams$DATASETs <- NULL

        tkgrab.release(tt)
        tkdestroy(tt)
        tkfocus(.cdtEnv$tcl$main$win)
    })

    tkgrid(bt.prm.OK, row = 0, column = 0, sticky = 'w', padx = 5, pady = 1, ipadx = 1, ipady = 1)
    tkgrid(bt.prm.CA, row = 0, column = 1, sticky = 'e', padx = 5, pady = 1, ipadx = 1, ipady = 1)

    ############################################

    tkgrid(frMRG0, row = 0, column = 0, sticky = 'nswe', rowspan = 1, columnspan = 2, padx = 1, pady = 1, ipadx = 1, ipady = 1)
    tkgrid(frMRG1, row = 1, column = 1, sticky = 'se', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)

    ############################
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
