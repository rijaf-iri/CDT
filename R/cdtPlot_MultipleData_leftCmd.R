
PlotMulitpleDataCmd <- function(){
    listOpenFiles <- openFile_ttkcomboList()
    if(WindowsOS()){
        largeur0 <- 22
        largeur1 <- 33
        largeur2 <- 36
        largeur3 <- 20
        largeur4 <- 11
        data.w <- 360
        data.h <- 350
    }else{
        largeur0 <- 21
        largeur1 <- 32
        largeur2 <- 33
        largeur3 <- 20
        largeur4 <- 11
        data.w <- 360
        data.h <- 351
    }

    ###################

    GeneralParameters <- list(intstep = "dekadal", minhour = 1,
                              date = list(year = 2021, mon = 1, day = 1, hour = 1, min = 0))

    .cdtData$EnvData$dataMapOp <- list(presetCol = list(color = 'tim.colors', reverse = FALSE),
                                        userCol = list(custom = FALSE, color = NULL),
                                        userLvl = list(custom = FALSE, levels = NULL, equidist = FALSE),
                                        title = list(user = FALSE, title = ''),
                                        colkeyLab = list(user = FALSE, label = ''))

    .cdtData$EnvData$SHPOp <- list(col = "black", lwd = 1.5)

    ###################

    xml.dlg <- file.path(.cdtDir$dirLocal, "languages", "cdtPlot_MultipleData_leftCmd.xml")
    lang.dlg <- cdtLanguageParse(xml.dlg, .cdtData$Config$lang.iso)

    ###################

    .cdtEnv$tcl$main$cmd.frame <- tkframe(.cdtEnv$tcl$main$panel.left)

    tknote.cmd <- bwNoteBook(.cdtEnv$tcl$main$cmd.frame)
    cmd.tab1 <- bwAddTab(tknote.cmd, text = lang.dlg[['tab_title']][['1']])
    cmd.tab2 <- bwAddTab(tknote.cmd, text = lang.dlg[['tab_title']][['2']])
    cmd.tab3 <- bwAddTab(tknote.cmd, text = lang.dlg[['tab_title']][['3']])

    bwRaiseTab(tknote.cmd, cmd.tab1)

    tkgrid.columnconfigure(cmd.tab1, 0, weight = 1)
    tkgrid.columnconfigure(cmd.tab2, 0, weight = 1)
    tkgrid.columnconfigure(cmd.tab3, 0, weight = 1)

    tkgrid.rowconfigure(cmd.tab1, 0, weight = 1)
    tkgrid.rowconfigure(cmd.tab2, 0, weight = 1)
    tkgrid.rowconfigure(cmd.tab3, 0, weight = 1)

    #######################################################################################################

    date.time.selection <- function(intstep, frTS0){
        txtdek <- switch(intstep,
                         'dekadal' = lang.dlg[['label']][['8']],
                         'pentad'  = lang.dlg[['label']][['9']],
                                     lang.dlg[['label']][['10']])
        day.txtVar <- tclVar(txtdek)

        stateday <- if(intstep == 'monthly') 'disabled' else 'normal'
        statehour <- if(intstep %in% c('minute', 'hourly')) 'normal' else 'disabled'
        statemin <- if(intstep == 'minute') 'normal' else 'disabled'

        txt.yrs <- tklabel(frTS0, text = lang.dlg[['label']][['6']])
        txt.mon <- tklabel(frTS0, text = lang.dlg[['label']][['7']])
        txt.day <- tklabel(frTS0, text = tclvalue(day.txtVar), textvariable = day.txtVar)
        txt.hrs <- tklabel(frTS0, text = lang.dlg[['label']][['11']])
        txt.min <- tklabel(frTS0, text = lang.dlg[['label']][['12']])

        en.yrs <- tkentry(frTS0, width = 5, textvariable = date.year, justify = "center")
        en.mon <- tkentry(frTS0, width = 5, textvariable = date.mon, justify = "center")
        en.day <- tkentry(frTS0, width = 5, textvariable = date.day, justify = "center", state = stateday)
        en.hrs <- tkentry(frTS0, width = 5, textvariable = date.hour, justify = "center", state = statehour)
        en.min <- tkentry(frTS0, width = 5, textvariable = date.min, justify = "center", state = statemin)

        ##############
        tkgrid(txt.yrs, row = 0, column = 0, sticky = 'we', pady = 1, padx = 1)
        tkgrid(txt.mon, row = 0, column = 1, sticky = 'we', pady = 1, padx = 1)
        tkgrid(txt.day, row = 0, column = 2, sticky = 'we', pady = 1, padx = 1)
        tkgrid(txt.hrs, row = 0, column = 3, sticky = 'we', pady = 1, padx = 1)
        tkgrid(txt.min, row = 0, column = 4, sticky = 'we', pady = 1, padx = 1)

        tkgrid(en.yrs, row = 1, column = 0, sticky = 'we', pady = 1, padx = 1)
        tkgrid(en.mon, row = 1, column = 1, sticky = 'we', pady = 1, padx = 1)
        tkgrid(en.day, row = 1, column = 2, sticky = 'we', pady = 1, padx = 1)
        tkgrid(en.hrs, row = 1, column = 3, sticky = 'we', pady = 1, padx = 1)
        tkgrid(en.min, row = 1, column = 4, sticky = 'we', pady = 1, padx = 1)
    }

    ################

    isaDataSet <- local({
        k <- 0
        function() {
            k <<- k + 1
            return(k)
        }
    })

    add.new.datasets <- function(jj, ids){
        .cdtData$GalParams$DATASETs[[jj]]$tcl$frame <- ttklabelframe(subfr1, text = paste0(lang.dlg[['label']][['2']], ids), relief = 'groove')

        .cdtData$GalParams$DATASETs[[jj]]$tcl$data.type <- tclVar()
        tclvalue(.cdtData$GalParams$DATASETs[[jj]]$tcl$data.type) <- CbdatatypeVAL[datatypeVAL %in% .cdtData$GalParams$DATASETs[[jj]]$pars$data.type]

        .cdtData$GalParams$DATASETs[[jj]]$tcl$input.file <- tclVar(.cdtData$GalParams$DATASETs[[jj]]$pars$input$dir)

        stateSetNC <- if(trimws(tclvalue(.cdtData$GalParams$DATASETs[[jj]]$tcl$data.type)) == CbdatatypeVAL[1]) "disabled" else "normal"

        cb.datatype <- ttkcombobox(.cdtData$GalParams$DATASETs[[jj]]$tcl$frame, values = CbdatatypeVAL,
                                   textvariable = .cdtData$GalParams$DATASETs[[jj]]$tcl$data.type, width = largeur0)
        bt.datatype <- ttkbutton(.cdtData$GalParams$DATASETs[[jj]]$tcl$frame,
                                 text = .cdtEnv$tcl$lang$global[['button']][['5']], state = stateSetNC)

        if(trimws(tclvalue(.cdtData$GalParams$DATASETs[[jj]]$tcl$data.type)) == CbdatatypeVAL[1]){
            cb.en.datafile <- ttkcombobox(.cdtData$GalParams$DATASETs[[jj]]$tcl$frame, values = unlist(listOpenFiles),
                                          textvariable = .cdtData$GalParams$DATASETs[[jj]]$tcl$input.file, width = largeur1)
        }else{
            cb.en.datafile <- tkentry(.cdtData$GalParams$DATASETs[[jj]]$tcl$frame,
                                      textvariable = .cdtData$GalParams$DATASETs[[jj]]$tcl$input.file, width = largeur2)
        }
        bt.datafile <- tkbutton(.cdtData$GalParams$DATASETs[[jj]]$tcl$frame, text = "...")

        bt.Options <- ttkbutton(.cdtData$GalParams$DATASETs[[jj]]$tcl$frame, text = .cdtEnv$tcl$lang$global[['button']][['4']])
        bt.Remove <- ttkbutton(.cdtData$GalParams$DATASETs[[jj]]$tcl$frame, text = lang.dlg[['button']][['2']])

        ####

        tkconfigure(bt.datatype, command = function(){
            .cdtData$GalParams$DATASETs[[jj]]$pars[["input"]] <- getInfoNetCDFData(.cdtEnv$tcl$main$win,
                                                                                   .cdtData$GalParams$DATASETs[[jj]]$pars[["input"]],
                                                                                   trimws(tclvalue(.cdtData$GalParams$DATASETs[[jj]]$tcl$input.file)))
        })

        ####

        tkconfigure(bt.datafile, command = function(){
            if(trimws(tclvalue(.cdtData$GalParams$DATASETs[[jj]]$tcl$data.type)) == CbdatatypeVAL[1]){
                dat.opfiles <- getOpenFiles(.cdtEnv$tcl$main$win)
                if(!is.null(dat.opfiles)){
                    update.OpenFiles('ascii', dat.opfiles)
                    listOpenFiles[[length(listOpenFiles) + 1]] <<- dat.opfiles[[1]]
                    tclvalue(.cdtData$GalParams$DATASETs[[jj]]$tcl$input.file) <- dat.opfiles[[1]]
                    tkconfigure(cb.en.datafile, values = unlist(openFile_ttkcomboList()))
                    .cdtData$GalParams$DATASETs[[jj]]$pars$input$dir <- trimws(tclvalue(.cdtData$GalParams$DATASETs[[jj]]$tcl$input.file))
                }
            }else{
                dirnc <- tk_choose.dir(getwd(), "")
                tclvalue(.cdtData$GalParams$DATASETs[[jj]]$tcl$input.file) <- if(dirnc %in% c("", "NA") | is.na(dirnc)) "" else dirnc
                .cdtData$GalParams$DATASETs[[jj]]$pars$input$dir <- trimws(tclvalue(.cdtData$GalParams$DATASETs[[jj]]$tcl$input.file))
            }
        })

        ####

        tkconfigure(bt.Options, command = function(){
            .cdtData$GalParams$DATASETs[[jj]]$pars <- MapGraph.MultiDatasets(.cdtData$GalParams$DATASETs[[jj]]$pars)
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
            tcl("update")
        })

        ####

        tkgrid(cb.datatype, row = 0, column = 0, sticky = 'we', rowspan = 1, columnspan = 8, padx = 1, pady = 1, ipadx = 1, ipady = 1)
        tkgrid(bt.datatype, row = 0, column = 8, sticky = 'we', rowspan = 1, columnspan = 2, padx = 1, pady = 1, ipadx = 1, ipady = 1)

        tkgrid(cb.en.datafile, row = 1, column = 0, sticky = 'we', rowspan = 1, columnspan = 9, padx = 0, pady = 1, ipadx = 1, ipady = 1)
        tkgrid(bt.datafile, row = 1, column = 9, sticky = 'we', rowspan = 1, columnspan = 1, padx = 0, pady = 1, ipadx = 1, ipady = 1)

        tkgrid(bt.Options, row = 2, column = 6, sticky = 'we', rowspan = 1, columnspan = 2, padx = 1, pady = 1, ipadx = 1, ipady = 1)
        tkgrid(bt.Remove, row = 2, column = 8, sticky = 'we', rowspan = 1, columnspan = 2, padx = 1, pady = 1, ipadx = 1, ipady = 1)


        tkgrid(.cdtData$GalParams$DATASETs[[jj]]$tcl$frame)

        ####

        helpWidget(cb.datatype, lang.dlg[['tooltip']][['1']], lang.dlg[['status']][['1']])
        helpWidget(bt.datatype, lang.dlg[['tooltip']][['2']], lang.dlg[['status']][['2']])

        if(trimws(tclvalue(.cdtData$GalParams$DATASETs[[jj]]$tcl$data.type)) == CbdatatypeVAL[1]){
            helpWidget(cb.en.datafile, lang.dlg[['tooltip']][['3']], lang.dlg[['status']][['3']])
        }else{
            helpWidget(cb.en.datafile, lang.dlg[['tooltip']][['4']], lang.dlg[['status']][['4']])
        }

        helpWidget(bt.datafile, lang.dlg[['tooltip']][['5']], lang.dlg[['status']][['5']])

        ####

        tkbind(cb.datatype, "<<ComboboxSelected>>", function(){
            tkdestroy(cb.en.datafile)
            tclvalue(.cdtData$GalParams$DATASETs[[jj]]$tcl$input.file) <- ''

            stateSetNC <- if(trimws(tclvalue(.cdtData$GalParams$DATASETs[[jj]]$tcl$data.type)) == CbdatatypeVAL[1]) "disabled" else "normal"
            tkconfigure(bt.datatype, state = stateSetNC)

            if(trimws(tclvalue(.cdtData$GalParams$DATASETs[[jj]]$tcl$data.type)) == CbdatatypeVAL[1]){
                cb.en.datafile <- ttkcombobox(.cdtData$GalParams$DATASETs[[jj]]$tcl$frame, values = unlist(listOpenFiles), textvariable = .cdtData$GalParams$DATASETs[[jj]]$tcl$input.file, width = largeur1)

                tkconfigure(bt.datafile, command = function(){
                    dat.opfiles <- getOpenFiles(.cdtEnv$tcl$main$win)
                    if(!is.null(dat.opfiles)){
                        update.OpenFiles('ascii', dat.opfiles)
                        listOpenFiles[[length(listOpenFiles) + 1]] <<- dat.opfiles[[1]]
                        tclvalue(.cdtData$GalParams$DATASETs[[jj]]$tcl$input.file) <- dat.opfiles[[1]]
                        tkconfigure(cb.en.datafile, values = unlist(openFile_ttkcomboList()))
                        .cdtData$GalParams$DATASETs[[jj]]$pars$input$dir <- trimws(tclvalue(.cdtData$GalParams$DATASETs[[jj]]$tcl$input.file))
                    }
                })

                helpWidget(cb.en.datafile, lang.dlg[['tooltip']][['3']], lang.dlg[['status']][['3']])

                .cdtData$GalParams$DATASETs[[jj]]$pars$data.type <- 'cdtstation'
                .cdtData$GalParams$DATASETs[[jj]]$pars$map.type <- "Points"
            }

            if(trimws(tclvalue(.cdtData$GalParams$DATASETs[[jj]]$tcl$data.type)) == CbdatatypeVAL[2]){
                cb.en.datafile <- tkentry(.cdtData$GalParams$DATASETs[[jj]]$tcl$frame, textvariable = .cdtData$GalParams$DATASETs[[jj]]$tcl$input.file, width = largeur2)

                tkconfigure(bt.datatype, command = function(){
                    .cdtData$GalParams$DATASETs[[jj]]$pars[["input"]] <- getInfoNetCDFData(.cdtEnv$tcl$main$win, .cdtData$GalParams$DATASETs[[jj]]$pars[["input"]],
                                                                                            trimws(tclvalue(.cdtData$GalParams$DATASETs[[jj]]$tcl$input.file)))
                })

                tkconfigure(bt.datafile, command = function(){
                    dirnc <- tk_choose.dir(getwd(), "")
                    tclvalue(.cdtData$GalParams$DATASETs[[jj]]$tcl$input.file) <- if(dirnc %in% c("", "NA") | is.na(dirnc)) "" else dirnc
                    .cdtData$GalParams$DATASETs[[jj]]$pars$input$dir <- trimws(tclvalue(.cdtData$GalParams$DATASETs[[jj]]$tcl$input.file))
                })

                helpWidget(cb.en.datafile, lang.dlg[['tooltip']][['4']], lang.dlg[['status']][['4']])

                .cdtData$GalParams$DATASETs[[jj]]$pars$data.type <- 'cdtnetcdf'
                .cdtData$GalParams$DATASETs[[jj]]$pars$map.type <- "Grid"
            }

            tkgrid(cb.en.datafile, row = 1, column = 0, sticky = 'we', rowspan = 1, columnspan = 9, padx = 0, pady = 1, ipadx = 1, ipady = 1)
        })
    }

    #######################################################################################################

    #Tab1
    subfr1 <- bwTabScrollableFrame(cmd.tab1, hscrlwin = data.h, wscrlwin = data.w)

    bt.AddData <- tkbutton(cmd.tab1, text = lang.dlg[['button']][['1']], bg = 'lightgreen')
    tkgrid(bt.AddData, sticky = 'swe', padx = 1, pady = 1, ipadx = 1, ipady = 1)

        ##############################################

        frameTS <- tkframe(subfr1)

        timeSteps <- tclVar()
        CbperiodVAL <- .cdtEnv$tcl$lang$global[['combobox']][['1']][1:6]
        periodVAL <- c('minute', 'hourly', 'daily', 'pentad', 'dekadal', 'monthly')
        tclvalue(timeSteps) <- CbperiodVAL[periodVAL %in% GeneralParameters$intstep]

        retminhr <- set.hour.minute(GeneralParameters$intstep, GeneralParameters$minhour)
        minhour.tclVar <- tclVar(retminhr$val)

        txt.tstep <- tklabel(frameTS, text = lang.dlg[['label']][['1']], anchor = 'w', justify = 'left')
        cb.tstep <- ttkcombobox(frameTS, values = CbperiodVAL, textvariable = timeSteps, width = largeur3)
        cb.minhour <- ttkcombobox(frameTS, values = retminhr$cb, textvariable = minhour.tclVar, state = retminhr$state, width = 2)

        ########

        tkgrid(txt.tstep, row = 0, column = 0, sticky = 'we', rowspan = 1, columnspan = 2, padx = 1, pady = 1, ipadx = 1, ipady = 1)
        tkgrid(cb.tstep, row = 0, column = 2, sticky = 'we', rowspan = 1, columnspan = 6, padx = 1, pady = 1, ipadx = 1, ipady = 1)
        tkgrid(cb.minhour, row = 0, column = 8, sticky = 'we', rowspan = 1, columnspan = 1, padx = 0, pady = 1, ipadx = 1, ipady = 1)

        tkgrid(frameTS)

        ########

        tkbind(cb.tstep, "<<ComboboxSelected>>", function(){
            tkdestroy(frTS0)
            frTS0 <<- tkframe(framePlotMap)

            intstep <- periodVAL[CbperiodVAL %in% trimws(tclvalue(timeSteps))]
            date.time.selection(intstep, frTS0)

            tkgrid(frTS0, row = 0, column = 0, sticky = '', padx = 1, pady = 1, columnspan = 3)

            ##############
            minhour <- as.numeric(trimws(tclvalue(minhour.tclVar)))
            retminhr <- set.hour.minute(intstep, minhour)
            tkconfigure(cb.minhour, values = retminhr$cb, state = retminhr$state)
            tclvalue(minhour.tclVar) <- retminhr$val
        })

        #######################

        CbdatatypeVAL <- .cdtEnv$tcl$lang$global[['combobox']][['2']][c(1, 3)]
        datatypeVAL <- c('cdtstation', 'cdtnetcdf')

        #######################

        tkconfigure(bt.AddData, command = function(){
            jj <- length(.cdtData$GalParams$DATASETs) + 1

            ids <- isaDataSet()
            .cdtData$GalParams$DATASETs[[jj]] <- list()
            .cdtData$GalParams$DATASETs[[jj]]$pars$data.type <- "cdtstation"
            .cdtData$GalParams$DATASETs[[jj]]$pars$map.type <- "Points"
            .cdtData$GalParams$DATASETs[[jj]]$pars$plot.type <- "Points"
            .cdtData$GalParams$DATASETs[[jj]]$pars$point.size <- 1.0
            .cdtData$GalParams$DATASETs[[jj]]$pars$title <- paste("Data set", ids)
            .cdtData$GalParams$DATASETs[[jj]]$pars$input$dir <- ""
            .cdtData$GalParams$DATASETs[[jj]]$pars$input$sample <- ""
            .cdtData$GalParams$DATASETs[[jj]]$pars$input$format <- "rfe_%s%s%s.nc"

            add.new.datasets(jj, ids)
        })

    #######################################################################################################

    #Tab2
    subfr2 <- bwTabScrollableFrame(cmd.tab2)

        ##############################################

        framePlotMap <- ttklabelframe(subfr2, text = lang.dlg[['label']][['3']], relief = 'groove')

        date.year <- tclVar(GeneralParameters$date$year)
        date.mon <- tclVar(GeneralParameters$date$mon)
        date.day <- tclVar(GeneralParameters$date$day)
        date.hour <- tclVar(GeneralParameters$date$hour)
        date.min <- tclVar(GeneralParameters$date$min)

        frTS0 <- tkframe(framePlotMap)
        date.time.selection(GeneralParameters$intstep, frTS0)

        bt.date.prev <- ttkbutton(framePlotMap, text = "<<", width = largeur4)
        bt.Map.plot <- ttkbutton(framePlotMap, text = .cdtEnv$tcl$lang$global[['button']][['3']], width = largeur4)
        bt.date.next <- ttkbutton(framePlotMap, text = ">>", width = largeur4)
        bt.Map.Opt <- ttkbutton(framePlotMap, text = .cdtEnv$tcl$lang$global[['button']][['4']], width = largeur4)

        ##############
        tkgrid(frTS0, row = 0, column = 0, sticky = '', padx = 1, pady = 1, columnspan = 3)
        tkgrid(bt.date.prev, row = 1, column = 0, sticky = 'we', padx = 1, pady = 1, columnspan = 1)
        tkgrid(bt.Map.plot, row = 1, column = 1, sticky = 'we', padx = 1, pady = 1, columnspan = 1)
        tkgrid(bt.date.next, row = 1, column = 2, sticky = 'we', padx = 1, pady = 1, columnspan = 1)
        tkgrid(bt.Map.Opt, row = 2, column = 1, sticky = 'we', padx = 1, pady = 1, columnspan = 1)

        #######################

        .cdtData$EnvData$tab$multidataMap <- NULL

        tkconfigure(bt.Map.plot, command = function(){
            ret <- try(getData2Plot(), silent = TRUE)
            if(inherits(ret, "try-error") | is.null(ret)) return(NULL)

            imgContainer <- CDT.Display.Graph(MultipleData.Plot.Map, .cdtData$EnvData$tab$multidataMap, "Multiple_Datasets")
            .cdtData$EnvData$tab$multidataMap <- imageNotebookTab_unik(imgContainer, .cdtData$EnvData$tab$multidataMap)
        })

        tkconfigure(bt.date.prev, command = function(){
            if(!is.null(.cdtData$GalParams$donnees$dates)){
                nl <- length(.cdtData$GalParams$donnees$dates)
                date2plot <- .cdtData$GalParams$donnees$date2plot
                ix <- which(.cdtData$GalParams$donnees$dates == date2plot)
                ix <- ix - 1
                if(ix < 1) ix <- nl

                date2plot <- .cdtData$GalParams$donnees$dates[ix]
                intstep <- periodVAL[CbperiodVAL %in% trimws(tclvalue(timeSteps))]

                tclvalue(date.year) <- substr(date2plot, 1, 4)
                tclvalue(date.mon) <- as.numeric(substr(date2plot, 5, 6))
                if(intstep != "monthly")
                    tclvalue(date.day) <- as.numeric(substr(date2plot, 7, 8))
                if(intstep %in% c("minute", "hourly"))
                    tclvalue(date.hour) <- as.numeric(substr(date2plot, 9, 10))
                if(intstep == "minute")
                    tclvalue(date.min) <- as.numeric(substr(date2plot, 11, 12))
            }

            ret <- try(getData2Plot(), silent = TRUE)
            if(inherits(ret, "try-error") | is.null(ret)) return(NULL)

            imgContainer <- CDT.Display.Graph(MultipleData.Plot.Map, .cdtData$EnvData$tab$multidataMap, "Multiple_Datasets")
            .cdtData$EnvData$tab$multidataMap <- imageNotebookTab_unik(imgContainer, .cdtData$EnvData$tab$multidataMap)
        })

        tkconfigure(bt.date.next, command = function(){
            if(!is.null(.cdtData$GalParams$donnees$dates)){
                nl <- length(.cdtData$GalParams$donnees$dates)
                date2plot <- .cdtData$GalParams$donnees$date2plot
                ix <- which(.cdtData$GalParams$donnees$dates == date2plot)
                ix <- ix + 1
                if(ix > nl) ix <- 1

                date2plot <- .cdtData$GalParams$donnees$dates[ix]
                intstep <- periodVAL[CbperiodVAL %in% trimws(tclvalue(timeSteps))]

                tclvalue(date.year) <- substr(date2plot, 1, 4)
                tclvalue(date.mon) <- as.numeric(substr(date2plot, 5, 6))
                if(intstep != "monthly")
                    tclvalue(date.day) <- as.numeric(substr(date2plot, 7, 8))
                if(intstep %in% c("minute", "hourly"))
                    tclvalue(date.hour) <- as.numeric(substr(date2plot, 9, 10))
                if(intstep == "minute")
                    tclvalue(date.min) <- as.numeric(substr(date2plot, 11, 12))
            }


            ret <- try(getData2Plot(), silent = TRUE)
            if(inherits(ret, "try-error") | is.null(ret)) return(NULL)

            imgContainer <- CDT.Display.Graph(MultipleData.Plot.Map, .cdtData$EnvData$tab$multidataMap, "Multiple_Datasets")
            .cdtData$EnvData$tab$multidataMap <- imageNotebookTab_unik(imgContainer, .cdtData$EnvData$tab$multidataMap)
        })

        ##############

        tkconfigure(bt.Map.Opt, command = function(){
            if(!is.null(.cdtData$EnvData$data.range)){
                atlevel <- pretty(.cdtData$EnvData$data.range[, 3], n = 10, min.n = 7)
                if(is.null(.cdtData$EnvData$dataMapOp$userLvl$levels)){
                    .cdtData$EnvData$dataMapOp$userLvl$levels <- atlevel
                }else{
                    if(!.cdtData$EnvData$dataMapOp$userLvl$custom)
                        .cdtData$EnvData$dataMapOp$userLvl$levels <- atlevel
                }
            }
            .cdtData$EnvData$dataMapOp <- MapGraph.MapOptions(.cdtData$EnvData$dataMapOp)
        })

        ##############################################

        bt.Save <- ttkbutton(subfr2, text = lang.dlg[['button']][['3']])
        bt.Load <- ttkbutton(subfr2, text = lang.dlg[['button']][['4']])

        ############

        tkconfigure(bt.Save, command = function(){
            filename <- tk_get_SaveFile(filetypes = .cdtEnv$tcl$data$filetypes6)

            if(filename == "" | filename == 'NA' | is.na(filename)){
                Insert.Messages.Out(lang.dlg[['message']][['7']], TRUE, "e")
                return(NULL)
            }

            tkconfigure(.cdtEnv$tcl$main$win, cursor = 'watch')
            tcl('update')
            on.exit({
                tkconfigure(.cdtEnv$tcl$main$win, cursor = '')
                tcl('update')
            })

            ####

            intstep <- periodVAL[CbperiodVAL %in% trimws(tclvalue(timeSteps))]

            datasets <- lapply(seq_along(.cdtData$GalParams$DATASETs), function(j){
                if(trimws(tclvalue(.cdtData$GalParams$DATASETs[[j]]$tcl$data.type)) == CbdatatypeVAL[1]){
                    filename <- trimws(tclvalue(.cdtData$GalParams$DATASETs[[j]]$tcl$input.file))
                }else{
                    filename <- .cdtData$GalParams$DATASETs[[j]]$pars$input$sample
                }

                jfile <- getIndex.AllOpenFiles(filename)
                ## test if exist
                Type <- .cdtData$OpenFiles$Type[[jfile]]
                Data <- .cdtData$OpenFiles$Data[[jfile]]
                nom <- .cdtData$OpenFiles$Data[[jfile]][[1]]
                PARS <- .cdtData$GalParams$DATASETs[[j]]$pars

                list(names = nom, PARS = PARS, Data = Data, Type = Type)
            })

            shp.data <- NULL
            if(tclvalue(.cdtData$EnvData$shp$add.shp) == "1"){
                nom <- trimws(tclvalue(file.plotShp))
                if(nom != "" & !is.null(.cdtData$EnvData$shp$ocrds)){
                    jfile <- getIndex.AllOpenFiles(nom)
                    Type <- .cdtData$OpenFiles$Type[[jfile]]
                    Data <- .cdtData$OpenFiles$Data[[jfile]]
                    PARS <- .cdtData$EnvData$SHPOp
                    ocrds <- .cdtData$EnvData$shp$ocrds
                    shp.data <- list(names = nom, PARS = PARS, Data = Data, Type = Type, ocrds = ocrds)
                }
            }

            minhour <- as.numeric(trimws(tclvalue(minhour.tclVar)))
            dates <- list(
                            year = as.numeric(trimws(tclvalue(date.year))),
                            mon = as.numeric(trimws(tclvalue(date.mon))),
                            day = as.numeric(trimws(tclvalue(date.day))),
                            hour = as.numeric(trimws(tclvalue(date.hour))),
                            min = as.numeric(trimws(tclvalue(date.min)))
                        )

            tosave <- list(type = "multiplot", intstep = intstep, minhour = minhour,
                           data = datasets, shp.data = shp.data,
                           dates = dates, Options = .cdtData$EnvData$dataMapOp)
            saveRDS(tosave, filename)

            Insert.Messages.Out(lang.dlg[['message']][['8']], TRUE, "s")
        })

        ############

        tkconfigure(bt.Load, command = function(){
            fileopen <- tclvalue(tkgetOpenFile(initialdir = getwd(), initialfile = "", filetypes = .cdtEnv$tcl$data$filetypes6))
            if(fileopen == "" | fileopen == 'NA' | is.na(fileopen)) return(NULL)

            tkconfigure(.cdtEnv$tcl$main$win, cursor = 'watch')
            tcl('update')
            on.exit({
                tkconfigure(.cdtEnv$tcl$main$win, cursor = '')
                tcl('update')
            })

            don <- readRDS(fileopen)

            returnNULL <- FALSE
            if(is.null(don$type)){
                returnNULL <- TRUE
            }else{
                if(don$type != "multiplot")
                    returnNULL <- TRUE
            }

            if(returnNULL){
                Insert.Messages.Out(lang.dlg[['message']][['9']], TRUE, 'e')
                return(NULL)
            }

            ###
            tclvalue(timeSteps) <- CbperiodVAL[periodVAL %in% don$intstep]
            retminhr <- set.hour.minute(don$intstep, don$minhour)

            tkconfigure(cb.minhour, values = retminhr$cb, state = retminhr$state)
            tclvalue(minhour.tclVar) <- retminhr$val

            ###

            lapply(don$data, function(x){
                listOpFiles <- sapply(.cdtData$OpenFiles$Data, "[[", 1)
                if(x$name %in% listOpFiles){
                    x$name <- paste0(x$name, ".1")
                    x$Data[[1]] <- paste0(x$Data[[1]], ".1")
                }

                jj <- length(.cdtData$GalParams$DATASETs) + 1
                ids <- isaDataSet()
                .cdtData$GalParams$DATASETs[[jj]] <- list()
                .cdtData$GalParams$DATASETs[[jj]]$pars <- x$PARS

                if(.cdtData$GalParams$DATASETs[[jj]]$pars$data.type == "cdtstation")
                    .cdtData$GalParams$DATASETs[[jj]]$pars$input$dir <- x$Data[[1]]

                add.new.datasets(jj, ids)

                jfile <- length(.cdtData$OpenFiles$Type) + 1
                .cdtData$OpenFiles$Type[[jfile]] <- x$Type
                .cdtData$OpenFiles$Data[[jfile]] <- x$Data
                tkinsert(.cdtEnv$tcl$main$Openfiles, "end", x$name)
            })

            ###
            if(!is.null(don$shp.data)){
                listOpFiles <- sapply(.cdtData$OpenFiles$Data, "[[", 1)
                if(don$shp.data$names %in% listOpFiles){
                    don$shp.data$names <- paste0(don$shp.data$names, ".1")
                    don$shp.data$Data[[1]] <- paste0(don$shp.data$Data[[1]], ".1")
                }
                .cdtData$EnvData$SHPOp <- don$shp.data$PARS
                .cdtData$EnvData$shp$ocrds <- don$shp.data$ocrds

                jfile <- length(.cdtData$OpenFiles$Type) + 1
                .cdtData$OpenFiles$Type[[jfile]] <- don$shp.data$Type
                .cdtData$OpenFiles$Data[[jfile]] <- don$shp.data$Data
                tkinsert(.cdtEnv$tcl$main$Openfiles, "end", don$shp.data$names)

                tclvalue(.cdtData$EnvData$shp$add.shp) <- TRUE
                lapply(list(bt.addshpOpt, cb.addshp, bt.addshp), tkconfigure, state = "normal")
                tkconfigure(cb.addshp, values = unlist(openFile_ttkcomboList()))
                tclvalue(file.plotShp) <- don$shp.data$names
            }

            ###
            .cdtData$EnvData$dataMapOp <- don$Options

            ###

            date.time.selection(don$intstep, frTS0)

            tclvalue(date.year) <- don$dates$year
            tclvalue(date.mon) <- don$dates$mon
            tclvalue(date.day) <- don$dates$day
            tclvalue(date.hour) <- don$dates$hour
            tclvalue(date.min) <- don$dates$min
        })

        ##############################################

        tkgrid(framePlotMap, row = 0, column = 0, sticky = 'we', rowspan = 1, columnspan = 2, padx = 1, pady = 1, ipadx = 1, ipady = 1)
        tkgrid(bt.Save, row = 1, column = 0, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 3, ipadx = 1, ipady = 1)
        tkgrid(bt.Load, row = 1, column = 1, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 3, ipadx = 1, ipady = 1)

    #######################################################################################################

    #Tab3
    subfr3 <- bwTabScrollableFrame(cmd.tab3)

        ##############################################

        frameSHP <- ttklabelframe(subfr3, text = lang.dlg[['label']][['13']], relief = 'groove')

        .cdtData$EnvData$shp$add.shp <- tclVar(FALSE)
        file.plotShp <- tclVar()
        stateSHP <- "disabled"

        chk.addshp <- tkcheckbutton(frameSHP, variable = .cdtData$EnvData$shp$add.shp, text = lang.dlg[['checkbutton']][['1']], anchor = 'w', justify = 'left')
        bt.addshpOpt <- ttkbutton(frameSHP, text = .cdtEnv$tcl$lang$global[['button']][['4']], state = stateSHP)
        cb.addshp <- ttkcombobox(frameSHP, values = unlist(listOpenFiles), textvariable = file.plotShp, width = largeur1, state = stateSHP)
        bt.addshp <- tkbutton(frameSHP, text = "...", state = stateSHP)

        ########
        tkgrid(chk.addshp, row = 0, column = 0, sticky = 'we', rowspan = 1, columnspan = 6, padx = 1, pady = 1)
        tkgrid(bt.addshpOpt, row = 0, column = 6, sticky = 'we', rowspan = 1, columnspan = 2, padx = 1, pady = 1)
        tkgrid(cb.addshp, row = 1, column = 0, sticky = 'we', rowspan = 1, columnspan = 7, padx = 1, pady = 1)
        tkgrid(bt.addshp, row = 1, column = 7, sticky = 'we', rowspan = 1, columnspan = 1, padx = 0, pady = 1)

        ########
        tkconfigure(bt.addshp, command = function(){
            shp.opfiles <- getOpenShp(.cdtEnv$tcl$main$win)
            if(!is.null(shp.opfiles)){
                update.OpenFiles('shp', shp.opfiles)
                tclvalue(file.plotShp) <- shp.opfiles[[1]]
                listOpenFiles[[length(listOpenFiles) + 1]] <<- shp.opfiles[[1]]

                tkconfigure(cb.addshp, values = unlist(listOpenFiles))

                shpofile <- getShpOpenData(file.plotShp)
                if(is.null(shpofile))
                    .cdtData$EnvData$shp$ocrds <- NULL
                else
                    .cdtData$EnvData$shp$ocrds <- getBoundaries(shpofile[[2]])
            }
        })

        ########

        tkconfigure(bt.addshpOpt, command = function(){
            .cdtData$EnvData$SHPOp <- MapGraph.GraphOptions.LineSHP(.cdtData$EnvData$SHPOp)
        })

        #################
        tkbind(cb.addshp, "<<ComboboxSelected>>", function(){
            shpofile <- getShpOpenData(file.plotShp)
            if(is.null(shpofile))
                .cdtData$EnvData$shp$ocrds <- NULL
            else
                .cdtData$EnvData$shp$ocrds <- getBoundaries(shpofile[[2]])
        })

        tkbind(chk.addshp, "<Button-1>", function(){
            stateSHP <- if(tclvalue(.cdtData$EnvData$shp$add.shp) == "1") "disabled" else "normal"
            tkconfigure(cb.addshp, state = stateSHP)
            tkconfigure(bt.addshp, state = stateSHP)
            tkconfigure(bt.addshpOpt, state = stateSHP)
        })

        ##############################################

        tkgrid(frameSHP, row = 0, column = 0, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)

    #######################################################################################################

    getDatasets <- function(){
        Infodata <- lapply(.cdtData$GalParams$DATASETs, function(don){
            dir <- trimws(tclvalue(don$tcl$input.file))
            format <- don$pars$input$format
            list(dir = dir, format = format)
        })

        getdatasets <- TRUE
        if(!is.null(.cdtData$GalParams$Infodata))
            if(isTRUE(all.equal(.cdtData$GalParams$Infodata, Infodata))) getdatasets <- FALSE

        if(getdatasets){
            intstep <- periodVAL[CbperiodVAL %in% trimws(tclvalue(timeSteps))]

            tmp.don <- lapply(.cdtData$GalParams$DATASETs, function(don){
                input.file <- trimws(tclvalue(don$tcl$input.file))
                if(don$pars$data.type == "cdtstation"){
                    ret <- list(data = NULL, dates = NULL, msg = paste(lang.dlg[['message']][['10']], input.file))

                    dat <- getStnOpenData(input.file)
                    if(is.null(dat)) return(ret)

                    dat <- splitCDTData0(dat)
                    if(is.null(dat)) return(ret)

                    ret <- list(data = dat, dates = dat$dates, msg = NULL)
                }else{
                    ret <- list(data = NULL, dates = NULL, msg = NULL)
                    ncdf <- list(dir = input.file, format = don$pars$input$format)

                    ret1 <- ncInfo.no.date.range(ncdf, intstep)
                    if(is.null(ret1)){
                        ret$msg <- paste(lang.dlg[['message']][['11']], input.file)
                        return(ret)
                    }

                    daty <- ret1$dates[ret1$exist]
                    nc.path <- ret1$ncfiles[ret1$exist]

                    #####
                    ncInfo <- getNCDFSampleData(don$pars$input$sample)
                    if(is.null(ncInfo)){
                        ret$msg <- lang.dlg[['message']][['12']]
                        return(ret)
                    }

                    dat <- c(ncInfo, list(path = nc.path))
                    ret <- list(data = dat, dates = daty, msg = NULL)
                }

                return(ret)
            })

            no.data <- sapply(lapply(tmp.don, "[[", "data"), is.null)
            if(any(no.data)){
                sapply(lapply(tmp.don[no.data], "[[", "msg"), Insert.Messages.Out, TRUE, 'e')
                return(NULL)
            }

            daty <- Reduce(intersect, lapply(tmp.don, "[[", "dates"))
            if(length(daty) == 0){
                Insert.Messages.Out(lang.dlg[['message']][['13']], TRUE, 'e')
                return(NULL)
            }

            ####
            end.dates <- daty[length(daty)]
            tclvalue(date.year) <- substr(end.dates, 1, 4)
            tclvalue(date.mon) <- as.numeric(substr(end.dates, 5, 6))
            if(intstep != "monthly")
                tclvalue(date.day) <- as.numeric(substr(end.dates, 7, 8))
            if(intstep %in% c("minute", "hourly"))
                tclvalue(date.hour) <- as.numeric(substr(end.dates, 9, 10))
            if(intstep == "minute")
                tclvalue(date.min) <- as.numeric(substr(end.dates, 11, 12))

            ####
            .cdtData$GalParams$donnees <- list(dates = daty, donnees = tmp.don)
            .cdtData$GalParams$Infodata <- Infodata
        }

        return(0)
    }

    #########################

    getData2Plot <- function(){
        tkconfigure(.cdtEnv$tcl$main$win, cursor = 'watch')
        tcl('update')
        on.exit({
            tkconfigure(.cdtEnv$tcl$main$win, cursor = '')
            tcl('update')
        })

        ret <- try(getDatasets(), silent = TRUE)
        if(inherits(ret, "try-error") | is.null(ret)) return(NULL)

        year <- trimws(tclvalue(date.year))
        mon <- stringr::str_pad(trimws(tclvalue(date.mon)), 2, pad = "0")
        day <- stringr::str_pad(trimws(tclvalue(date.day)), 2, pad = "0")
        hour <- stringr::str_pad(trimws(tclvalue(date.hour)), 2, pad = "0")
        min <- stringr::str_pad(trimws(tclvalue(date.min)), 2, pad = "0")

        intstep <- periodVAL[CbperiodVAL %in% trimws(tclvalue(timeSteps))]

        if(intstep == "minute")
            daty2plot <- paste0(year, mon, day, hour, min)
        if(intstep == "hourly")
            daty2plot <- paste0(year, mon, day, hour)
        if(intstep == "daily")
            daty2plot <- paste0(year, mon, day)
        if(intstep %in% c("pentad", "dekadal"))
            daty2plot <- paste0(year, mon, as.numeric(day))
        if(intstep == "monthly")
            daty2plot <- paste0(year, mon)

        data.Obj <- lapply(seq_along(.cdtData$GalParams$DATASETs), function(jj){
            params <- .cdtData$GalParams$DATASETs[[jj]]
            don <- .cdtData$GalParams$donnees$donnees[[jj]]
            pars.plot <- params$pars[c('map.type', 'plot.type', 'title', 'point.size')]
            if(pars.plot$map.type == "Grid" & pars.plot$plot.type == "Points")
                pars.plot$plot.type <- "Pixels"

            if(params$pars$data.type == "cdtstation"){
                lon <- don$data$lon
                lat <- don$data$lat
                zval <- don$data$data[don$dates == daty2plot, , drop = FALSE]
                if(nrow(zval) == 0){
                    msg <- paste(daty2plot, lang.dlg[['message']][['6']], params$pars$input$dir)
                    return(list(obj = NULL, msg = msg))
                }
                zval <- as.numeric(zval[1, ])

                if(params$pars$plot.type == "Pixels"){
                    nx <- nx_ny_as.image(diff(range(lon)))
                    ny <- nx_ny_as.image(diff(range(lat)))
                    tmp <- cdt.as.image(zval, nx = nx, ny = ny, pts.xy = cbind(lon, lat))
                }else tmp <- list(x = lon, y = lat, z = zval)
            }else{
                ncinfo <- don$data
                ncfile <- ncinfo$path[don$dates == daty2plot]
                if(length(ncfile) == 0){
                    msg <- paste("No file:", ncfile)
                    return(list(obj = NULL, msg = msg))
                }

                nc <- try(ncdf4::nc_open(ncfile), silent = TRUE)
                if(inherits(nc, "try-error")){
                    msg <- paste(lang.dlg[['message']][['14']], ncfile)
                    return(list(obj = NULL, msg = msg))
                }
                zval <- ncdf4::ncvar_get(nc, varid = ncinfo$varid)
                ncdf4::nc_close(nc)
                zval <- if(ncinfo$ilon < ncinfo$ilat) zval[ncinfo$xo, ncinfo$yo] else t(zval)[ncinfo$xo, ncinfo$yo]
                tmp <- list(x = ncinfo$lon, y = ncinfo$lat, z = zval)
            }

            return(list(obj = c(tmp, pars.plot), msg = NULL))
        })

        Obj <- lapply(data.Obj, "[[", "obj")
        no.data <- sapply(Obj, is.null)
        if(all(no.data)){
            Insert.Messages.Out(lang.dlg[['message']][['15']], TRUE, 'e')
            return(NULL)
        }
        if(any(no.data)) sapply(lapply(data.Obj[no.data], "[[", "msg"), Insert.Messages.Out, TRUE, 'e')

        .cdtData$GalParams$donnees$date2plot <- daty2plot
        .cdtData$EnvData$data.Obj <- Obj
        .cdtData$EnvData$data.range <- do.call(rbind, lapply(Obj, function(x) sapply(x[c('x', 'y', 'z')], range, na.rm = TRUE)))

        return(0)
    }

    #######################################################################################################

    tkgrid(tknote.cmd, sticky = 'nwes')
    tkgrid.columnconfigure(tknote.cmd, 0, weight = 1)
    tkgrid.rowconfigure(tknote.cmd, 0, weight = 1)

    tcl('update')
    tkgrid(.cdtEnv$tcl$main$cmd.frame, sticky = 'nwes', pady = 1)
    tkgrid.columnconfigure(.cdtEnv$tcl$main$cmd.frame, 0, weight = 1)
    tkgrid.rowconfigure(.cdtEnv$tcl$main$cmd.frame, 0, weight = 1)

    tkbind(.cdtEnv$tcl$main$cmd.frame, "<Destroy>", function(){
        .cdtData$GalParams$DATASETs <- NULL
    })

    invisible()
}

