
dataOperation_GetInfo <- function(){
    if(WindowsOS()){
        largeur0 <- 42
        largeur1 <- 44
        largeur2 <- 33
        largeur3 <- 43
        largeur4 <- 46
        largeur5 <- 23
        data.w <- 410
        data.h <- 240
    }else{
        largeur0 <- 42
        largeur1 <- 43
        largeur2 <- 30
        largeur3 <- 43
        largeur4 <- 45
        largeur5 <- 23
        data.w <- 410
        data.h <- 240
    }

    xml.dlg <- file.path(.cdtDir$dirLocal, "languages", "cdtDataOperation_dlgBox.xml")
    lang.dlg <- cdtLanguageParse(xml.dlg, .cdtData$Config$lang.iso)

    ###################

    tt <- tktoplevel()
    tkgrab.set(tt)
    tkfocus(tt)

    frMRG0 <- tkframe(tt, relief = 'raised', borderwidth = 2)
    frMRG1 <- tkframe(tt)

    cbLists <- new.env()
    cbLists$cb <- list()

    ############################################

    frameHaut <- tkframe(frMRG0)
    tkgrid(frameHaut, sticky = 'nwe', padx = 1, pady = 1, ipadx = 1, ipady = 1)
    frInput <- bwTabScrollableFrame(frMRG0, hscrlwin = data.h, wscrlwin = data.w)
    frameBas <- tkframe(frMRG0)
    tkgrid(frameBas, sticky = 'swe', padx = 1, pady = 1, ipadx = 1, ipady = 1)

    ############################################

    saveFun <- function(data.type){
            tkdestroy(frSaveIn)
            frSaveIn <<- tkframe(frSave)

            #########
            if(data.type == 'cdtstation'){
                txtSaveDir <- lang.dlg[['label']][['7']]
                isFile <- TRUE
            }else{
                txtSaveDir <- lang.dlg[['label']][['8']]
                isFile <- FALSE
            }
            fileORdir <- tclVar(txtSaveDir)

            txt.file.save <- tklabel(frSaveIn, text = tclvalue(fileORdir), textvariable = fileORdir, anchor = 'w', justify = 'left')
            en.file.save <- tkentry(frSaveIn, textvariable = file.save, width = largeur4)
            bt.file.save <- tkbutton(frSaveIn, text = "...")

            #########
            tkgrid(txt.file.save, row = 0, column = 0, sticky = 'we', rowspan = 1, columnspan = 5, padx = 1, pady = 0, ipadx = 1, ipady = 1)
            tkgrid(en.file.save, row = 1, column = 0, sticky = 'we', rowspan = 1, columnspan = 4, padx = 0, pady = 0, ipadx = 1, ipady = 1)
            tkgrid(bt.file.save, row = 1, column = 4, sticky = 'w', rowspan = 1, columnspan = 1, padx = 0, pady = 0, ipadx = 1, ipady = 1)

            #########

            tkconfigure(bt.file.save, command = function(){
                tcl('wm', 'attributes', tt, topmost = FALSE)
                fileORdir2Save(file.save, isFile = isFile)
                tcl('wm', 'attributes', tt, topmost = TRUE)
            })

            #########
            tkgrid(frSaveIn)
        }

    #############

    add.new.datasets <- function(jj, data.type, state){
        .cdtData$GalParams$DATASETs[[jj]]$tcl$frame <- ttklabelframe(frInput, text = paste(lang.dlg[['label']][['2']], paste0("X", jj)), relief = 'groove')

        .cdtData$GalParams$DATASETs[[jj]]$tcl$input.file <- tclVar(.cdtData$GalParams$DATASETs[[jj]]$pars$dir)

        if(data.type == 'cdtstation'){
            txt.Label <- lang.dlg[['label']][['3']]
            stateSetNC <- "disabled"
        }else if(data.type == 'cdtdataset'){
            txt.Label <- lang.dlg[['label']][['5']]
            stateSetNC <- "disabled"
        }else{
            txt.Label <- lang.dlg[['label']][['4']]
            stateSetNC <- "normal"
        }
        txt.Label.var <- tclVar(txt.Label)

        txt.Input <- tklabel(.cdtData$GalParams$DATASETs[[jj]]$tcl$frame, text = tclvalue(txt.Label.var), textvariable = txt.Label.var, anchor = 'w', justify = 'left')
        set.Input <- ttkbutton(.cdtData$GalParams$DATASETs[[jj]]$tcl$frame, text = .cdtEnv$tcl$lang$global[['button']][['5']], state = stateSetNC)

        if(data.type == 'cdtstation'){
            cb.en.Input <- ttkcombobox(.cdtData$GalParams$DATASETs[[jj]]$tcl$frame,
                                       values = unlist(openFile_ttkcomboList()),
                                       textvariable = .cdtData$GalParams$DATASETs[[jj]]$tcl$input.file,
                                       width = largeur0)
        }else{
            cb.en.Input <- tkentry(.cdtData$GalParams$DATASETs[[jj]]$tcl$frame,
                                   textvariable = .cdtData$GalParams$DATASETs[[jj]]$tcl$input.file,
                                   width = largeur1)
        }
        bt.en.Input <- tkbutton(.cdtData$GalParams$DATASETs[[jj]]$tcl$frame, text = "...")
        bt.Remove <- ttkbutton(.cdtData$GalParams$DATASETs[[jj]]$tcl$frame, text = lang.dlg[['button']][['2']], state = state)

        cbLists$cb[[length(cbLists$cb) + 1]] <- cb.en.Input

        tkgrid(txt.Input, row = 0, column = 0, sticky = 'we', rowspan = 1, columnspan = 3, padx = 1, pady = 0, ipadx = 1, ipady = 1)
        tkgrid(set.Input, row = 0, column = 3, sticky = 'we', rowspan = 1, columnspan = 2, padx = 1, pady = 0, ipadx = 1, ipady = 1)
        tkgrid(cb.en.Input, row = 1, column = 0, sticky = 'we', rowspan = 1, columnspan = 4, padx = 0, pady = 0, ipadx = 1, ipady = 1)
        tkgrid(bt.en.Input, row = 1, column = 4, sticky = 'w', rowspan = 1, columnspan = 1, padx = 0, pady = 0, ipadx = 1, ipady = 1)
        tkgrid(bt.Remove, row = 2, column = 3, sticky = 'we', rowspan = 1, columnspan = 2, padx = 1, pady = 0, ipadx = 1, ipady = 1)

        tkgrid(.cdtData$GalParams$DATASETs[[jj]]$tcl$frame)

        tcl("update")

        #####

        tkconfigure(set.Input, command = function(){
            tcl('wm', 'attributes', tt, topmost = FALSE)
            .cdtData$GalParams$DATASETs[[jj]]$pars <- getInfoNetCDFData(tt, .cdtData$GalParams$DATASETs[[jj]]$pars,
                                                                        str_trim(tclvalue(.cdtData$GalParams$DATASETs[[jj]]$tcl$input.file)))
            tcl('wm', 'attributes', tt, topmost = TRUE)
        })

        tkconfigure(bt.en.Input, command = function(){
            if(data.type == 'cdtstation'){
                tcl('wm', 'attributes', tt, topmost = FALSE)
                dat.opfiles <- getOpenFiles(tt)
                tcl('wm', 'attributes', tt, topmost = TRUE)
                if(!is.null(dat.opfiles)){
                    update.OpenFiles('ascii', dat.opfiles)
                    tclvalue(.cdtData$GalParams$DATASETs[[jj]]$tcl$input.file) <- dat.opfiles[[1]]
                    lapply(cbLists$cb, function(x){
                        if(as.integer(tkwinfo('exists', x)) == 1)
                            tkconfigure(x, values = unlist(openFile_ttkcomboList()))
                    })
                }
            }else if(data.type == 'cdtdataset'){
                tcl('wm', 'attributes', tt, topmost = FALSE)
                path.rds <- tclvalue(tkgetOpenFile(initialdir = getwd(), filetypes = .cdtEnv$tcl$data$filetypes6))
                tcl('wm', 'attributes', tt, topmost = TRUE)
                tclvalue(.cdtData$GalParams$DATASETs[[jj]]$tcl$input.file) <- if(path.rds %in% c("", "NA") | is.na(path.rds)) "" else path.rds
            }else{
                tcl('wm', 'attributes', tt, topmost = FALSE)
                dirnc <- tk_choose.dir(getwd(), "")
                tcl('wm', 'attributes', tt, topmost = TRUE)
                tclvalue(.cdtData$GalParams$DATASETs[[jj]]$tcl$input.file) <- if(dirnc %in% c("", "NA") | is.na(dirnc)) "" else dirnc
            }
        })

        ####

        id.fr <- NULL
        tkbind(bt.Remove, "<Button-1>", function(){
            id.fr <<- tclvalue(tkwinfo("parent", bt.Remove))
        })

        tkconfigure(bt.Remove, command = function(){
            id.frame <- sapply(.cdtData$GalParams$DATASETs, function(x) x$tcl$frame$ID)

            ii <- which(id.frame == id.fr)
            tkdestroy(.cdtData$GalParams$DATASETs[[ii]]$tcl$frame)
            .cdtData$GalParams$DATASETs[[ii]] <- NULL

            for(ii in seq_along(.cdtData$GalParams$DATASETs)){
                textLab <- paste(lang.dlg[['label']][['2']], paste0("X", ii))
                tkconfigure(.cdtData$GalParams$DATASETs[[ii]]$tcl$frame, text = textLab)
            }

            tcl("update")
        })
    }

    ############################################

    frTstep <- tkframe(frameHaut, relief = "sunken", borderwidth = 2)

    timeStep <- tclVar()
    CbperiodVAL <- .cdtEnv$tcl$lang$global[['combobox']][['1']][c(1:6, 10)]
    periodVAL <- c('minute', 'hourly', 'daily', 'pentad', 'dekadal', 'monthly', 'others')
    tclvalue(timeStep) <- CbperiodVAL[periodVAL %in% .cdtData$GalParams$tstep]

    retminhr <- set.hour.minute(.cdtData$GalParams$tstep, .cdtData$GalParams$minhour)
    minhour.tclVar <- tclVar(retminhr$val)

    txt.tstep <- tklabel(frTstep, text = lang.dlg[['label']][['9']], anchor = 'w', justify = 'left')
    cb.tstep <- ttkcombobox(frTstep, values = CbperiodVAL, textvariable = timeStep, justify = 'center', width = largeur5)
    cb.minhour <- ttkcombobox(frTstep, values = retminhr$cb, textvariable = minhour.tclVar, state = retminhr$state, width = 2)

    tkgrid(txt.tstep, row = 0, column = 0, sticky = 'we', rowspan = 1, columnspan = 2, padx = 1, pady = 1, ipadx = 1, ipady = 1)
    tkgrid(cb.tstep, row = 0, column = 2, sticky = 'we', rowspan = 1, columnspan = 6, padx = 1, pady = 1, ipadx = 1, ipady = 1)
    tkgrid(cb.minhour, row = 0, column = 8, sticky = 'we', rowspan = 1, columnspan = 2, padx = 0, pady = 1, ipadx = 1, ipady = 1)

    ############

    tkbind(cb.tstep, "<<ComboboxSelected>>", function(){
        intstep <- periodVAL[CbperiodVAL %in% str_trim(tclvalue(timeStep))]
        minhour <- as.numeric(str_trim(tclvalue(minhour.tclVar)))
        retminhr <- set.hour.minute(intstep, minhour)
        tkconfigure(cb.minhour, values = retminhr$cb, state = retminhr$state)
        tclvalue(minhour.tclVar) <- retminhr$val
    })

    ############################################

    frdatatype <- tkframe(frameHaut, relief = 'sunken', borderwidth = 2)

    DataType <- tclVar()
    CbdatatypeVAL <- .cdtEnv$tcl$lang$global[['combobox']][['2']][1:3]
    datatypeVAL <- c('cdtstation', 'cdtdataset', 'cdtnetcdf')
    tclvalue(DataType) <- CbdatatypeVAL[datatypeVAL %in% .cdtData$GalParams$datatype]

    txt.datatyp <- tklabel(frdatatype, text = lang.dlg[['label']][['1']], anchor = 'w', justify = 'left')
    cb.datatyp <- ttkcombobox(frdatatype, values = CbdatatypeVAL, textvariable = DataType, justify = 'center', width = largeur2)

    tkgrid(txt.datatyp, row = 0, column = 0, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
    tkgrid(cb.datatyp, row = 0, column = 1, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)

    #############

    tkbind(cb.datatyp, "<<ComboboxSelected>>", function(){
        data.type <- datatypeVAL[CbdatatypeVAL %in% str_trim(tclvalue(DataType))]

        ######
        for(jj in seq_along(.cdtData$GalParams$DATASETs))
            tkdestroy(.cdtData$GalParams$DATASETs[[jj]]$tcl$frame)

        .cdtData$GalParams$DATASETs <- NULL

        .cdtData$GalParams$DATASETs[[1]] <- list()
        inputFile <- .cdtData$GalParams$inputs[[paste0('file', 1)]]
        .cdtData$GalParams$DATASETs[[1]]$pars <- inputFile

        add.new.datasets(1, data.type, 'disabled')

        ######
        stateVarinfo <- if(data.type == "cdtstation") "disabled" else "normal"
        tkconfigure(bt.Varinfo, state = stateVarinfo)

        ######
        saveFun(data.type)
        tkfocus(tt)
    })

    ############################################

    tkgrid(frTstep, row = 0, column = 0, sticky = 'e', padx = 1, pady = 1, ipadx = 1, ipady = 1)
    tkgrid(frdatatype, row = 1, column = 0, sticky = 'we', padx = 1, pady = 5, ipadx = 1, ipady = 1)

    ############################################

    for(jj in 1:length(.cdtData$GalParams$inputs)){
        .cdtData$GalParams$DATASETs[[jj]] <- list()
        inputFile <- .cdtData$GalParams$inputs[[paste0('file', jj)]]
        .cdtData$GalParams$DATASETs[[jj]]$pars <- inputFile

        stateRem <- if(jj == 1) "disabled" else "normal"
        add.new.datasets(jj, .cdtData$GalParams$datatype, stateRem)
    }

    ############################################

    bt.AddData <- tkbutton(frameBas, text = lang.dlg[['button']][['1']], bg = 'lightgreen')

    tkconfigure(bt.AddData, command = function(){
        data.type <- datatypeVAL[CbdatatypeVAL %in% str_trim(tclvalue(DataType))]

        jj <- length(.cdtData$GalParams$DATASETs) + 1
        .cdtData$GalParams$DATASETs[[jj]] <- list()
        .cdtData$GalParams$DATASETs[[jj]]$pars <- list(dir = "", sample = "", format = "rr_mrg_%s%s%s.nc")

        add.new.datasets(jj, data.type, 'normal')
    })

    ############################################

    frFormula <- tkframe(frameBas, relief = 'sunken', borderwidth = 2)

    yscr.Formula <- tkscrollbar(frFormula, repeatinterval = 4, command = function(...) tkyview(text.Formula, ...))
    text.Formula <- tktext(frFormula, bg = "white", wrap = "word", height = 2, width = largeur3,
                            yscrollcommand = function(...) tkset(yscr.Formula, ...))

    tkgrid(text.Formula, yscr.Formula)
    tkgrid.configure(yscr.Formula, sticky = "ns")
    tkgrid.configure(text.Formula, sticky = 'nswe')

    if(!is.na(.cdtData$GalParams$formula)) tkinsert(text.Formula, "end", .cdtData$GalParams$formula)

    helpWidget(text.Formula, lang.dlg[['tooltip']][['1']], lang.dlg[['status']][['1']])

    ############################################

    stateVarinfo <- if(.cdtData$GalParams$datatype == "cdtstation") "disabled" else "normal"

    bt.Varinfo <- tkbutton(frameBas, text = lang.dlg[['button']][['3']], bg = 'lightblue', state = stateVarinfo)

    tkconfigure(bt.Varinfo, command = function(){
        data.type <- datatypeVAL[CbdatatypeVAL %in% str_trim(tclvalue(DataType))]
        inputFile <- str_trim(tclvalue(.cdtData$GalParams$DATASETs[[1]]$tcl$input.file))

        initVar <- .cdtData$GalParams$varinfo
        varInfo <- NULL

        if(data.type == "cdtnetcdf"){
            inputFile <- .cdtData$GalParams$DATASETs[[1]]$pars$sample
            if(inputFile != ""){
                varInfo <- getNCDFSampleData(inputFile)
                varInfo <- varInfo$varinfo
            }
        }

        if(data.type == "cdtdataset"){
            inputFile <- str_trim(tclvalue(.cdtData$GalParams$DATASETs[[1]]$tcl$input.file))
            if(inputFile != "" & file.exists(inputFile)){
                varInfo <- readRDS(inputFile)
                varInfo <- varInfo$varInfo
            }
        }

        if(!is.null(varInfo)){
            if(initVar$name == ""){
                initVar$name <- varInfo$name
                initVar$units <- varInfo$units
                initVar$prec <- varInfo$prec
            }
            if(initVar$longname == "")
                initVar$longname <- varInfo$longname
        }

        # .cdtData$GalParams$varinfo <- initVar

        tcl('wm', 'attributes', tt, topmost = FALSE)
        .cdtData$GalParams$varinfo <- getNetCDFvarInfo(tt, initVar)
        tcl('wm', 'attributes', tt, topmost = TRUE)
    })

    ############################################

    frSave <- tkframe(frameBas, relief = 'sunken', borderwidth = 2)

    frSaveIn <- tkframe(frSave)

    file.save <- tclVar(.cdtData$GalParams$output)

    saveFun(.cdtData$GalParams$datatype)

    ############################################

    tkgrid(bt.AddData, row = 0, column = 0, sticky = 'we', padx = 1, pady = 3, ipadx = 1, ipady = 1)
    tkgrid(frFormula, row = 1, column = 0, sticky = 'we', padx = 1, pady = 1, ipadx = 1, ipady = 1)
    tkgrid(bt.Varinfo, row = 2, column = 0, sticky = 'we', padx = 1, pady = 3, ipadx = 1, ipady = 1)
    tkgrid(frSave, row = 3, column = 0, sticky = 'we', padx = 1, pady = 1, ipadx = 1, ipady = 1)

    ############################################

    bt.prm.OK <- ttkbutton(frMRG1, text = .cdtEnv$tcl$lang$global[['button']][['1']])
    bt.prm.CA <- ttkbutton(frMRG1, text = .cdtEnv$tcl$lang$global[['button']][['2']])

    ####
    tkconfigure(bt.prm.OK, command = function(){
        .cdtData$GalParams$tstep <- periodVAL[CbperiodVAL %in% str_trim(tclvalue(timeStep))]
        .cdtData$GalParams$minhour <- as.numeric(str_trim(tclvalue(minhour.tclVar)))
        .cdtData$GalParams$datatype <- datatypeVAL[CbdatatypeVAL %in% str_trim(tclvalue(DataType))]

        if(str_trim(tclvalue(file.save)) %in% c("", "NA")){
            imsg <- if(.cdtData$GalParams$datatype == "cdtstation") "2" else 3
            cdt.tkmessageBox(tt, message = lang.dlg[['message']][[imsg]], icon = "warning", type = "ok")
            tkwait.window(tt)
        }

        .cdtData$GalParams$output <- str_trim(tclvalue(file.save))

        inputFiles <- lapply(.cdtData$GalParams$DATASETs, function(don){
                str_trim(tclvalue(don$tcl$input.file))
        })
        numFiles <- seq_along(.cdtData$GalParams$DATASETs)
        nonFiles <- sapply(inputFiles, function(x) x %in% c("", "NA"))

        if(any(nonFiles)){
            nbf <- paste0(numFiles[nonFiles], collapse = ", ")
            msg <- paste(lang.dlg[['message']][['4']], "-", lang.dlg[['message']][['5']], ":", nbf)
            cdt.tkmessageBox(tt, message = msg, icon = "warning", type = "ok")
            tkwait.window(tt)
        }

        for(jj in seq_along(.cdtData$GalParams$DATASETs)){
            .cdtData$GalParams$inputs[[paste0('file', jj)]]$dir <- inputFiles[[jj]]

            if(.cdtData$GalParams$datatype == "cdtnetcdf"){
                .cdtData$GalParams$inputs[[paste0('file', jj)]]$sample <- .cdtData$GalParams$DATASETs[[jj]]$pars$sample
                .cdtData$GalParams$inputs[[paste0('file', jj)]]$format <- .cdtData$GalParams$DATASETs[[jj]]$pars$format
            }
        }

        txtFormula <- str_trim(tclvalue(tkget(text.Formula, "0.0", "end")))
        txtFormula <- gsub("[\t\r\n]", "", txtFormula)
        posVar <- gregexpr('X[0-9]+', txtFormula)
        valVar <- regmatches(txtFormula, posVar)
        valVar <- sort(unique(valVar[[1]]))

        if(length(.cdtData$GalParams$inputs) != length(valVar)){
            cdt.tkmessageBox(tt, message = lang.dlg[['message']][['6']], icon = "warning", type = "ok")
            tkwait.window(tt)
        }

        .cdtData$GalParams$formula <- txtFormula

        #### test formula
        testData <- lapply(seq_along(valVar), function(j) j + 1)
        names(testData) <- valVar
        testOut <- NULL

        for(j in seq_along(valVar))
            txtFormula <- gsub(paste0("X", j), paste0("testData[['X", j, "']]"), txtFormula)

        testFun <- paste("testOut =", txtFormula)
        ret <- try(eval(parse(text = testFun)), silent = TRUE)
        if(inherits(ret, "try-error")){
            cdt.tkmessageBox(tt, message = lang.dlg[['message']][['8']], icon = "warning", type = "ok")
            tkwait.window(tt)
        }
        if(is.null(testOut)){
            cdt.tkmessageBox(tt, message = lang.dlg[['message']][['8']], icon = "warning", type = "ok")
            tkwait.window(tt)
        }

        ########

        if(.cdtData$GalParams$datatype != "cdtstation"){
            if(.cdtData$GalParams$varinfo$name == "" |
               .cdtData$GalParams$varinfo$longname == ""){
                cdt.tkmessageBox(tt, message = lang.dlg[['message']][['7']], icon = "warning", type = "ok")
                tkwait.window(tt)
            }
        }

        .cdtData$GalParams$message <- lang.dlg[['message']]

        for(jj in seq_along(.cdtData$GalParams$DATASETs))
            tkdestroy(.cdtData$GalParams$DATASETs[[jj]]$tcl$frame)

        .cdtData$GalParams$DATASETs <- NULL

        tkgrab.release(tt)
        tkdestroy(tt)
        tkfocus(.cdtEnv$tcl$main$win)
    })

    tkconfigure(bt.prm.CA, command = function(){
        for(jj in seq_along(.cdtData$GalParams$DATASETs))
            tkdestroy(.cdtData$GalParams$DATASETs[[jj]]$tcl$frame)

        .cdtData$GalParams$DATASETs <- NULL

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
