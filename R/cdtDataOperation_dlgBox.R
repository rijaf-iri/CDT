
dataOperation_GetInfo <- function(){
    if(WindowsOS()){
        largeur2 <- 33
        largeur3 <- 43
        largeur4 <- 46
        largeur5 <- 23
    }else{
        largeur2 <- 30
        largeur3 <- 43
        largeur4 <- 45
        largeur5 <- 23
    }

    xml.dlg <- file.path(.cdtDir$dirLocal, "languages", "cdtDataOperation_dlgBox.xml")
    lang.dlg <- cdtLanguageParse(xml.dlg, .cdtData$Config$lang.iso)

    ###################

    tt <- tktoplevel()
    tkgrab.set(tt)
    tkfocus(tt)

    frMRG0 <- tkframe(tt, relief = 'raised', borderwidth = 2)
    frMRG1 <- tkframe(tt)

    ############################################

    saveFun <- function(data.type){
        tkdestroy(frSaveIn)
        frSaveIn <<- tkframe(frSave)

        #########
        if(data.type == 'cdtstation'){
            txtSaveDir <- lang.dlg[['label']][['3']]
            helpE <- '3'
            isFile <- TRUE
        }else{
            txtSaveDir <- lang.dlg[['label']][['4']]
            helpE <- '4'
            isFile <- FALSE
        }
        fileORdir <- tclVar(txtSaveDir)

        txt.file.save <- tklabel(frSaveIn, text = tclvalue(fileORdir), textvariable = fileORdir, anchor = 'w', justify = 'left')
        en.file.save <- tkentry(frSaveIn, textvariable = file.save, width = largeur4)
        bt.file.save <- tkbutton(frSaveIn, text = "...")

        tkgrid(txt.file.save, row = 0, column = 0, sticky = 'we', rowspan = 1, columnspan = 5, padx = 1, pady = 0, ipadx = 1, ipady = 1)
        tkgrid(en.file.save, row = 1, column = 0, sticky = 'we', rowspan = 1, columnspan = 4, padx = 0, pady = 0, ipadx = 1, ipady = 1)
        tkgrid(bt.file.save, row = 1, column = 4, sticky = 'w', rowspan = 1, columnspan = 1, padx = 0, pady = 0, ipadx = 1, ipady = 1)

        helpWidget(en.file.save, lang.dlg[['tooltip']][[helpE]], lang.dlg[['status']][[helpE]])
        helpWidget(bt.file.save, lang.dlg[['tooltip']][['5']], lang.dlg[['status']][['5']])

        #########

        tkconfigure(bt.file.save, command = function(){
            tcl('wm', 'attributes', tt, topmost = FALSE)
            fileORdir2Save(file.save, isFile = isFile)
            tcl('wm', 'attributes', tt, topmost = TRUE)
        })

        #########

        if(data.type == 'cdtnetcdf'){
            txt.ncout.frmt <- tklabel(frSaveIn, text = lang.dlg[['label']][['6']], anchor = 'w', justify = 'left')
            en.ncout.frmt <- tkentry(frSaveIn, textvariable = ncout.format, width = largeur4)

            tkgrid(txt.ncout.frmt, row = 2, column = 0, sticky = 'we', rowspan = 1, columnspan = 5, padx = 1, pady = 0, ipadx = 1, ipady = 1)
            tkgrid(en.ncout.frmt, row = 3, column = 0, sticky = 'we', rowspan = 1, columnspan = 4, padx = 0, pady = 0, ipadx = 1, ipady = 1)

            helpWidget(en.ncout.frmt, lang.dlg[['tooltip']][['6']], lang.dlg[['status']][['6']])
        }

        if(data.type == 'cdtdataset'){
            txt.dset.name <- tklabel(frSaveIn, text = lang.dlg[['label']][['7']], anchor = 'w', justify = 'left')
            en.dset.name <- tkentry(frSaveIn, textvariable = dataset.name, width = largeur4)

            tkgrid(txt.dset.name, row = 2, column = 0, sticky = 'we', rowspan = 1, columnspan = 5, padx = 1, pady = 0, ipadx = 1, ipady = 1)
            tkgrid(en.dset.name, row = 3, column = 0, sticky = 'we', rowspan = 1, columnspan = 4, padx = 0, pady = 0, ipadx = 1, ipady = 1)

            helpWidget(en.dset.name, lang.dlg[['tooltip']][['7']], lang.dlg[['status']][['7']])
        }

        #########

        if(data.type %in% c('cdtnetcdf', 'cdtdataset')){
            bt.Varinfo <- tkbutton(frSaveIn, text = lang.dlg[['button']][['2']])

            tkgrid(bt.Varinfo, row = 4, column = 0, sticky = 'we', rowspan = 1, columnspan = 5, padx = 1, pady = 3, ipadx = 1, ipady = 1)

            helpWidget(bt.Varinfo, lang.dlg[['tooltip']][['8']], lang.dlg[['status']][['8']])

            tkconfigure(bt.Varinfo, command = function(){
                initVar <- .cdtData$GalParams$varinfo
                varInfo <- NULL

                if(data.type == "cdtnetcdf"){
                    inputFile <- .cdtData$GalParams$inputs$file1$sample
                    if(inputFile != ""){
                        varInfo <- getNCDFSampleData(inputFile)
                        varInfo <- varInfo$varinfo
                    }
                }

                if(data.type == "cdtdataset"){
                    inputFile <- .cdtData$GalParams$inputs$file1$dir
                    if(inputFile != "" && file.exists(inputFile)){
                        varInfo <- readRDS(inputFile)
                        varInfo <- varInfo$varInfo
                    }
                }

                if(!is.null(varInfo)){
                    if(initVar$name != varInfo$name && initVar$name == "") initVar$name <- varInfo$name
                    if(initVar$units != varInfo$units && initVar$units == "") initVar$units <- varInfo$units
                    if(initVar$prec != varInfo$prec && initVar$prec == "float") initVar$prec <- varInfo$prec
                    if(initVar$longname != varInfo$longname && initVar$longname == "") initVar$longname <- varInfo$longname
                    if(initVar$missval != varInfo$missval && initVar$missval == -9999) initVar$missval <- varInfo$missval
                }

                tcl('wm', 'attributes', tt, topmost = FALSE)
                .cdtData$GalParams$varinfo <- getNetCDFvarInfo(tt, initVar)
                tcl('wm', 'attributes', tt, topmost = TRUE)
            })
        }

        #########

        tkgrid(frSaveIn)
    }

    ############################################

    frTstep <- tkframe(frMRG0, relief = "sunken", borderwidth = 2)

    timeStep <- tclVar()
    CbperiodVAL <- .cdtEnv$tcl$lang$global[['combobox']][['1']][c(1:6, 10)]
    periodVAL <- c('minute', 'hourly', 'daily', 'pentad', 'dekadal', 'monthly', 'others')
    tclvalue(timeStep) <- CbperiodVAL[periodVAL %in% .cdtData$GalParams$tstep]

    retminhr <- set.hour.minute(.cdtData$GalParams$tstep, .cdtData$GalParams$minhour)
    minhour.tclVar <- tclVar(retminhr$val)

    txt.tstep <- tklabel(frTstep, text = lang.dlg[['label']][['5']], anchor = 'w', justify = 'left')
    cb.tstep <- ttkcombobox(frTstep, values = CbperiodVAL, textvariable = timeStep, justify = 'center', width = largeur5)
    cb.minhour <- ttkcombobox(frTstep, values = retminhr$cb, textvariable = minhour.tclVar, state = retminhr$state, width = 2)

    tkgrid(txt.tstep, row = 0, column = 0, sticky = 'we', rowspan = 1, columnspan = 2, padx = 1, pady = 1, ipadx = 1, ipady = 1)
    tkgrid(cb.tstep, row = 0, column = 2, sticky = 'we', rowspan = 1, columnspan = 6, padx = 1, pady = 1, ipadx = 1, ipady = 1)
    tkgrid(cb.minhour, row = 0, column = 8, sticky = 'we', rowspan = 1, columnspan = 2, padx = 0, pady = 1, ipadx = 1, ipady = 1)

    ############

    tkbind(cb.tstep, "<<ComboboxSelected>>", function(){
        intstep <- periodVAL[CbperiodVAL %in% trimws(tclvalue(timeStep))]
        minhour <- as.numeric(trimws(tclvalue(minhour.tclVar)))
        retminhr <- set.hour.minute(intstep, minhour)
        tkconfigure(cb.minhour, values = retminhr$cb, state = retminhr$state)
        tclvalue(minhour.tclVar) <- retminhr$val
    })

    ############################################

    frdatatype <- tkframe(frMRG0, relief = 'sunken', borderwidth = 2)

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
        data.type <- datatypeVAL[CbdatatypeVAL %in% trimws(tclvalue(DataType))]

        .cdtData$GalParams$inputs <- list(file1 = list(dir = "", sample = "", format = "rr_mrg_%s%s%s.nc"))
        .cdtData$GalParams$constant <- list(const1 = FALSE)

        .cdtData$GalParams$DATASETs <- NULL
        .cdtData$GalParams$DATASETs[[1]] <- list()
        .cdtData$GalParams$DATASETs[[1]]$pars <- .cdtData$GalParams$inputs[[paste0('file', 1)]]
        .cdtData$GalParams$DATASETs[[1]]$const <- .cdtData$GalParams$constant[[paste0('const', 1)]]

        saveFun(data.type)
        tkfocus(tt)
    })

    ############################################

    tkgrid(frTstep, row = 0, column = 0, sticky = 'e', padx = 1, pady = 1, ipadx = 1, ipady = 1)
    tkgrid(frdatatype, row = 1, column = 0, sticky = 'we', padx = 1, pady = 5, ipadx = 1, ipady = 1)

    ############################################

    bt.AddData <- tkbutton(frMRG0, text = lang.dlg[['button']][['1']], bg = 'lightgreen')

    helpWidget(bt.AddData, lang.dlg[['tooltip']][['2']], lang.dlg[['status']][['2']])

    tkconfigure(bt.AddData, command = function(){
        data.type <- datatypeVAL[CbdatatypeVAL %in% trimws(tclvalue(DataType))]
        dataOperation_Inputs(tt, data.type)
    })

    ############################################

    labFormula <- tklabel(frMRG0, text = lang.dlg[['label']][['2']], anchor = 'w', justify = 'left')

    frFormula <- tkframe(frMRG0, relief = 'sunken', borderwidth = 2)

    yscr.Formula <- tkscrollbar(frFormula, repeatinterval = 4, command = function(...) tkyview(text.Formula, ...))
    text.Formula <- tktext(frFormula, bg = "white", wrap = "word", height = 10, width = largeur3,
                            yscrollcommand = function(...) tkset(yscr.Formula, ...))

    tkgrid(text.Formula, yscr.Formula)
    tkgrid.configure(yscr.Formula, sticky = "ns")
    tkgrid.configure(text.Formula, sticky = 'nswe')

    if(!is.na(.cdtData$GalParams$formula)) tkinsert(text.Formula, "end", .cdtData$GalParams$formula)

    helpWidget(text.Formula, lang.dlg[['tooltip']][['1']], lang.dlg[['status']][['1']])

    ############################################

    frSave <- tkframe(frMRG0, relief = 'sunken', borderwidth = 2)

    frSaveIn <- tkframe(frSave)

    file.save <- tclVar(.cdtData$GalParams$output)
    ncout.format <- tclVar(.cdtData$GalParams$ncoutformat)
    dataset.name <- tclVar(.cdtData$GalParams$dataset.name)

    saveFun(.cdtData$GalParams$datatype)

    ############################################

    tkgrid(frTstep, row = 0, column = 0, sticky = 'e', padx = 1, pady = 1, ipadx = 1, ipady = 1)
    tkgrid(frdatatype, row = 1, column = 0, sticky = 'we', padx = 1, pady = 5, ipadx = 1, ipady = 1)
    tkgrid(bt.AddData, row = 2, column = 0, sticky = 'we', padx = 1, pady = 3, ipadx = 1, ipady = 1)
    tkgrid(labFormula, row = 3, column = 0, sticky = 'we', padx = 1, pady = 3, ipadx = 1, ipady = 1)
    tkgrid(frFormula, row = 4, column = 0, sticky = 'we', padx = 1, pady = 1, ipadx = 1, ipady = 1)
    tkgrid(frSave, row = 5, column = 0, sticky = 'we', padx = 1, pady = 1, ipadx = 1, ipady = 1)

    ############################################

    bt.prm.OK <- ttkbutton(frMRG1, text = .cdtEnv$tcl$lang$global[['button']][['1']])
    bt.prm.CA <- ttkbutton(frMRG1, text = .cdtEnv$tcl$lang$global[['button']][['2']])

    ####
    tkconfigure(bt.prm.OK, command = function(){
        .cdtData$GalParams$tstep <- periodVAL[CbperiodVAL %in% trimws(tclvalue(timeStep))]
        .cdtData$GalParams$minhour <- as.numeric(trimws(tclvalue(minhour.tclVar)))
        .cdtData$GalParams$datatype <- datatypeVAL[CbdatatypeVAL %in% trimws(tclvalue(DataType))]

        if(trimws(tclvalue(file.save)) %in% c("", "NA")){
            imsg <- if(.cdtData$GalParams$datatype == "cdtstation") "2" else "3"
            cdt.tkmessageBox(tt, message = lang.dlg[['message']][[imsg]], icon = "warning", type = "ok")
            tkwait.window(tt)
        }

        .cdtData$GalParams$output <- trimws(tclvalue(file.save))
        if(.cdtData$GalParams$datatype == "cdtnetcdf"){
            .cdtData$GalParams$ncoutformat <- trimws(tclvalue(ncout.format))
        }
        if(.cdtData$GalParams$datatype == "cdtdataset"){
            .cdtData$GalParams$dataset.name <- trimws(tclvalue(dataset.name))
            if(.cdtData$GalParams$dataset.name == ""){
                cdt.tkmessageBox(tt, message = lang.dlg[['message']][['19']], icon = "warning", type = "ok")
                tkwait.window(tt)
            }
        }

        txtFormula <- trimws(tclvalue(tkget(text.Formula, "0.0", "end")))
        txtFormula <- gsub("[\t\r\n]", "", txtFormula)
        posVar <- gregexpr('X[0-9]+', txtFormula)
        valVar <- regmatches(txtFormula, posVar)
        valVar <- sort(unique(valVar[[1]]))

        if(length(.cdtData$GalParams$inputs) != length(valVar)){
            cdt.tkmessageBox(tt, message = lang.dlg[['message']][['6']], icon = "warning", type = "ok")
            tkwait.window(tt)
        }

        is_crds <- grepl('lat|lon', txtFormula, ignore.case = TRUE)
        if(is_crds){
            txtFormula <- gsub('lon|Lon', 'LON', txtFormula)
            txtFormula <- gsub('lat|Lat', 'LAT', txtFormula)
        }

        .cdtData$GalParams$formula <- txtFormula

        #### test formula
        testData <- lapply(seq_along(valVar), function(j) j + 1)
        names(testData) <- valVar
        testOut <- NULL

        if(is_crds){
            txtFormula <- gsub('LON', 10, txtFormula)
            txtFormula <- gsub('LAT', 10, txtFormula)
        }

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

        tkgrab.release(tt)
        tkdestroy(tt)
        tkfocus(.cdtEnv$tcl$main$win)
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
