
ExtractDataPanelCmd <- function(){
    listOpenFiles <- openFile_ttkcomboList()
    if(WindowsOS()){
        largeur0 <- 30
        largeur1 <- 34
        largeur2 <- 36
        largeur3 <- 27
        largeur4 <- 31
        largeur5 <- 12
    }else{
        largeur0 <- 30
        largeur1 <- 33
        largeur2 <- 34
        largeur3 <- 27
        largeur4 <- 31
        largeur5 <- 13
    }

    ##############

    MOIS <- format(ISOdate(2014, 1:12, 1), "%B")
    
    GeneralParameters <- list(intstep = "daily", minhour = 0, season.len = 3,
                              season.start = 1, type.extract = "point",
                              data.type = 'cdtnetcdf', cdtdataset = list(index = ""),
                              cdtnetcdf = list(dir = "", sample = "", format = "rr_mrg_%s%s%s.nc"),
                              shp.file = list(shp = "", attr = ""),
                              out.data = list(format = "cdt", sp.avrg = FALSE, outdir = ""),
                              months = list(start = 1, end = 12))

    GeneralParameters$date.range <- list(start.year = 2021, start.mon = 1,
                                         start.dek = 1, start.pen = 1, start.day = 1,
                                         start.hour = 0, start.min = 0,
                                         end.year = 2022, end.mon = 12,
                                         end.dek = 3, end.pen = 6, end.day = 31,
                                         end.hour = 23, end.min = 55)

    GeneralParameters$Geom <- list(minlon = '', maxlon = '', minlat = '', maxlat = '',
                                   padlon = 0, padlat = 0, namePoly = '', multiObj = NULL)

    .cdtData$EnvData$multiptspoly <- NULL

    ##############

    xml.dlg <- file.path(.cdtDir$dirLocal, "languages", "cdtExtractData_leftCmd.xml")
    lang.dlg <- cdtLanguageParse(xml.dlg, .cdtData$Config$lang.iso)
    .cdtData$EnvData$message <- lang.dlg[['message']]

    ##############
    .cdtData$EnvData$zoom$xx1 <- tclVar()
    .cdtData$EnvData$zoom$xx2 <- tclVar()
    .cdtData$EnvData$zoom$yy1 <- tclVar()
    .cdtData$EnvData$zoom$yy2 <- tclVar()

    .cdtData$EnvData$zoom$pressButP <- tclVar(0)
    .cdtData$EnvData$zoom$pressButM <- tclVar(0)
    .cdtData$EnvData$zoom$pressButRect <- tclVar(0)
    .cdtData$EnvData$zoom$pressButDrag <- tclVar(0)

    .cdtData$EnvData$pressGetCoords <- tclVar(0)

    ZoomXYval0 <- NULL

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

    set.data.type.vars <- function(data.type){
        if(data.type == 'cdtnetcdf'){
            txt <- lang.dlg[['label']][['3']]
            state <- "normal"
            help1 <- lang.dlg[['tooltip']][['3']]
            help2 <- lang.dlg[['status']][['3']]
        }else{
            txt <- lang.dlg[['label']][['4']]
            state <- "disabled"
            help1 <- lang.dlg[['tooltip']][['4']]
            help2 <- lang.dlg[['status']][['4']]
        }

        list(txt = txt, state = state, tooltip = help1, status = help2)
    }

    initXYval0 <- NA
    initializeButZoom <- function(){
        initXYval0 <<- trimws(c(tclvalue(.cdtData$EnvData$zoom$xx1),
                                  tclvalue(.cdtData$EnvData$zoom$xx2),
                                  tclvalue(.cdtData$EnvData$zoom$yy1),
                                  tclvalue(.cdtData$EnvData$zoom$yy2)))

        tclvalue(.cdtData$EnvData$zoom$pressButP) <- 0
        tclvalue(.cdtData$EnvData$zoom$pressButM) <- 0
        tclvalue(.cdtData$EnvData$zoom$pressButRect) <- 0
        tclvalue(.cdtData$EnvData$zoom$pressButDrag) <- 0

        tclvalue(.cdtData$EnvData$pressGetCoords) <- 0

        tkconfigure(.cdtData$EnvData$zoom$btZoomP, relief = 'raised', bg = 'lightblue', state = 'normal')
        tkconfigure(.cdtData$EnvData$zoom$btZoomM, relief = 'raised', bg = 'lightblue', state = 'normal')
        tkconfigure(.cdtData$EnvData$zoom$btZoomRect, relief = 'raised', bg = 'lightblue', state = 'normal')
        tkconfigure(.cdtData$EnvData$zoom$btPanImg, relief = 'raised', bg = 'lightblue', state = 'normal')

        tkconfigure(.cdtData$EnvData$bt.GETArea, relief = 'raised', bg = 'lightblue', state = 'normal')
        stateADD <- if(.cdtData$EnvData$type.extract %in% c('mpoint', 'mpoly')) "normal" else "disabled"
        tkconfigure(.cdtData$EnvData$bt.ADDObj, relief = 'raised', bg = 'lightblue', state = stateADD)
    }

    activateButRedraw <- function(){
        initXYval1 <- trimws(c(tclvalue(.cdtData$EnvData$zoom$xx1),
                                 tclvalue(.cdtData$EnvData$zoom$xx2),
                                 tclvalue(.cdtData$EnvData$zoom$yy1),
                                 tclvalue(.cdtData$EnvData$zoom$yy2)))
        if(!all(initXYval0 == initXYval1))
            tkconfigure(.cdtData$EnvData$zoom$btRedraw, relief = 'raised', bg = 'red')
    }

    #######################################################################################################

    #Tab1
    subfr1 <- bwTabScrollableFrame(cmd.tab1)

        ##########################################

        frameTimeS <- ttklabelframe(subfr1, text = lang.dlg[['label']][['1']], relief = 'groove')

        timeSteps <- tclVar()
        CbperiodVAL <- .cdtEnv$tcl$lang$global[['combobox']][['1']][1:8]
        periodVAL <- c('minute', 'hourly', 'daily', 'pentad', 'dekadal', 'monthly', 'annual', 'seasonal')
        tclvalue(timeSteps) <- CbperiodVAL[periodVAL %in% GeneralParameters$intstep]

        retminhr <- set.hour.minute(GeneralParameters$intstep, GeneralParameters$minhour)
        minhour.tclVar <- tclVar(retminhr$val)

        cb.fperiod <- ttkcombobox(frameTimeS, values = CbperiodVAL, textvariable = timeSteps, width = largeur4)
        cb.minhour <- ttkcombobox(frameTimeS, values = retminhr$cb, textvariable = minhour.tclVar, state = retminhr$state, width = 2)

        tkgrid(cb.fperiod, row = 0, column = 0, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
        tkgrid(cb.minhour, row = 0, column = 1, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)

        helpWidget(cb.fperiod, lang.dlg[['tooltip']][['1']], lang.dlg[['status']][['1']])

        #################

        frameSeas <- tkframe(frameTimeS)

        seas_mon <- GeneralParameters$season.start
        seas_len <- GeneralParameters$season.len
        seas_mon1 <- (seas_mon + seas_len - 1) %% 12
        seas_mon1[seas_mon1 == 0] <- 12

        seasStart.tclVar <- tclVar(MOIS[seas_mon])
        seasLen.tclVar <- tclVar(seas_len)

        #################

        tkbind(cb.fperiod, "<<ComboboxSelected>>", function(){
            intstep <- periodVAL[CbperiodVAL %in% trimws(tclvalue(timeSteps))]

            minhour <- as.numeric(trimws(tclvalue(minhour.tclVar)))
            retminhr <- set.hour.minute(intstep, minhour)
            tkconfigure(cb.minhour, values = retminhr$cb, state = retminhr$state)
            tclvalue(minhour.tclVar) <- retminhr$val

            tkdestroy(frameSeas)

            if(intstep == 'seasonal'){
                frameSeas <<- tkframe(frameTimeS)

                frSeasDef <- tkframe(frameSeas)

                txt.seasS <- tklabel(frSeasDef, text = lang.dlg[['label']][['21']], anchor = 'e', justify = 'right')
                cb.seasS <- ttkcombobox(frSeasDef, values = MOIS, textvariable = seasStart.tclVar, width = 13)
                txt.seasL <- tklabel(frSeasDef, text = lang.dlg[['label']][['22']])
                cb.seasL <- ttkcombobox(frSeasDef, values = 2:12, textvariable = seasLen.tclVar, width = 3)

                sepL.seasS <- tklabel(frSeasDef, text = '', width = 3)

                tkgrid(txt.seasS, row = 0, column = 0, sticky = 'we')
                tkgrid(cb.seasS, row = 0, column = 1, sticky = 'we')
                tkgrid(sepL.seasS, row = 0, column = 2)
                tkgrid(txt.seasL, row = 0, column = 3, sticky = 'we')
                tkgrid(cb.seasL, row = 0, column = 4, sticky = 'we')

                ######

                frSeasDisp <- tkframe(frameSeas)

                seasdef <- paste(MOIS[seas_mon], "->", MOIS[seas_mon1])
                seasDef.tclVar <- tclVar(seasdef)

                inMonthSeas <- tklabel(frSeasDisp, text = tclvalue(seasDef.tclVar), textvariable = seasDef.tclVar)

                tkgrid(inMonthSeas, row = 0, column = 0, sticky = 'we')

                ######
                tkgrid(frSeasDef)
                tkgrid(frSeasDisp)

                tkgrid(frameSeas, row = 1, column = 0, sticky = 'we', rowspan = 1, columnspan = 2, padx = 1, pady = 2, ipadx = 1, ipady = 1)

                ######

                tkbind(cb.seasS, "<<ComboboxSelected>>", function(){
                    mon <-  which(MOIS %in% trimws(tclvalue(seasStart.tclVar)))
                    len <- as.numeric(trimws(tclvalue(seasLen.tclVar)))
                    mon1 <- (mon + len - 1) %% 12
                    mon1[mon1 == 0] <- 12
                    seasdef <- paste(MOIS[mon], "->", MOIS[mon1])
                    tclvalue(seasDef.tclVar) <- seasdef
                })

                tkbind(cb.seasL, "<<ComboboxSelected>>", function(){
                    mon <-  which(MOIS %in% trimws(tclvalue(seasStart.tclVar)))
                    len <- as.numeric(trimws(tclvalue(seasLen.tclVar)))
                    mon1 <- (mon + len - 1) %% 12
                    mon1[mon1 == 0] <- 12
                    seasdef <- paste(MOIS[mon], "->", MOIS[mon1])
                    tclvalue(seasDef.tclVar) <- seasdef
                })
            }

            if(intstep %in% c('seasonal', 'annual')){
                tkconfigure(cb.startMonth, state = "disabled")
                tkconfigure(cb.endMonth, state = "disabled")
            }else{
                tkconfigure(cb.startMonth, state = "normal")
                tkconfigure(cb.endMonth, state = "normal")
            }
        })

        #############################

        frameInData <- ttklabelframe(subfr1, text = lang.dlg[['label']][['2']], relief = 'groove')

        DataType <- tclVar()
        # CbdatatypeVAL <- .cdtEnv$tcl$lang$global[['combobox']][['2']][2:3]
        # datatypeVAL <- c('cdtdataset', 'cdtnetcdf')
        CbdatatypeVAL <- .cdtEnv$tcl$lang$global[['combobox']][['2']][3]
        CbdatatypeVAL <- c(CbdatatypeVAL, "")
        datatypeVAL <- c('cdtnetcdf', "")
        tclvalue(DataType) <- CbdatatypeVAL[datatypeVAL %in% GeneralParameters$data.type]

        setDT <- set.data.type.vars(GeneralParameters$data.type)
        txt.INData.var <- tclVar(setDT$txt)

        dirInput <- if(GeneralParameters$data.type == 'cdtnetcdf')
                        GeneralParameters$cdtnetcdf$dir
                    else
                        GeneralParameters$cdtdataset$index
        input.file <- tclVar(dirInput)

        txt.datatype <- tklabel(frameInData, text = lang.dlg[['label']][['5']], anchor = 'w', justify = 'left')
        cb.datatype <- ttkcombobox(frameInData, values = CbdatatypeVAL, textvariable = DataType, width = largeur0)
        txt.infile <- tklabel(frameInData, text = tclvalue(txt.INData.var), textvariable = txt.INData.var, anchor = 'w', justify = 'left')
        set.infile <- tkbutton(frameInData, text = .cdtEnv$tcl$lang$global[['button']][['5']], width = 8, state = setDT$state)
        cb.infile <- tkentry(frameInData, textvariable = input.file, width = largeur2)
        bt.infile <- tkbutton(frameInData, text = "...")

        #################

        tkgrid(txt.datatype, row = 0, column = 0, sticky = 'we', rowspan = 1, columnspan = 2, padx = 1, pady = 1, ipadx = 1, ipady = 1)
        tkgrid(cb.datatype, row = 0, column = 2, sticky = 'we', rowspan = 1, columnspan = 8, padx = 1, pady = 1, ipadx = 1, ipady = 1)
        tkgrid(txt.infile, row = 1, column = 0, sticky = 'we', rowspan = 1, columnspan = 8, padx = 1, pady = 1, ipadx = 1, ipady = 1)
        tkgrid(set.infile, row = 1, column = 8, sticky = 'we', rowspan = 1, columnspan = 2, padx = 1, pady = 1, ipadx = 1, ipady = 1)
        tkgrid(cb.infile, row = 2, column = 0, sticky = 'we', rowspan = 1, columnspan = 9, padx = 0, pady = 1, ipadx = 1, ipady = 1)
        tkgrid(bt.infile, row = 2, column = 9, sticky = 'w', rowspan = 1, columnspan = 1, padx = 0, pady = 1, ipadx = 1, ipady = 1)

        #################

        helpWidget(cb.datatype, lang.dlg[['tooltip']][['2']], lang.dlg[['status']][['2']])
        helpWidget(cb.infile, setDT$tooltip, setDT$status)
        helpWidget(bt.infile, lang.dlg[['tooltip']][['5']], lang.dlg[['status']][['5']])

        ############
        settingINData <- NULL
        tkconfigure(set.infile, command = function(){
            GeneralParameters[['cdtnetcdf']] <<- getInfoNetCDFData(.cdtEnv$tcl$main$win,
                                                                   GeneralParameters[['cdtnetcdf']],
                                                                   trimws(tclvalue(input.file)))
            settingINData <<- 1
        })

        tkconfigure(bt.infile, command = function(){
            data.type <- datatypeVAL[CbdatatypeVAL %in% trimws(tclvalue(DataType))]
            if(data.type == 'cdtnetcdf'){
                dirnc <- tk_choose.dir(getwd(), "")
                tclvalue(input.file) <- if(dirnc %in% c("", "NA") | is.na(dirnc)) "" else dirnc
            }else{
                path.rds <- tclvalue(tkgetOpenFile(initialdir = getwd(), filetypes = .cdtEnv$tcl$data$filetypes6))
                tclvalue(input.file) <- if(path.rds %in% c("", "NA") | is.na(path.rds)) "" else path.rds
            }
        })

        #################

        tkbind(cb.datatype, "<<ComboboxSelected>>", function(){
            tclvalue(input.file) <- ''
            data.type <- datatypeVAL[CbdatatypeVAL %in% trimws(tclvalue(DataType))]

            setDT <- set.data.type.vars(data.type)
            tkconfigure(set.infile, state = setDT$state)
            helpWidget(cb.infile, setDT$tooltip, setDT$status)
       })

        #############################

        frameSHP <- tkframe(subfr1, relief = 'groove', borderwidth = 2)

        shpFile <- tclVar(GeneralParameters$shp.file$shp)
        shpAttr <- tclVar(GeneralParameters$shp.file$attr)

        txt.shpF <- tklabel(frameSHP, text = lang.dlg[['label']][['9']], anchor = 'w', justify = 'left')
        cb.shpF <- ttkcombobox(frameSHP, values = unlist(listOpenFiles), textvariable = shpFile, width = largeur1)
        bt.shpF <- tkbutton(frameSHP, text = "...")
        txt.shpAttr <- tklabel(frameSHP, text = lang.dlg[['label']][['10']], anchor = 'w', justify = 'left')
        .cdtData$EnvData$cb.shpAttr <- ttkcombobox(frameSHP, values = '', textvariable = shpAttr)

        tkgrid(txt.shpF, row = 0, column = 0, sticky = 'we', rowspan = 1, columnspan = 6, padx = 1, pady = 0, ipadx = 1, ipady = 1)
        tkgrid(cb.shpF, row = 1, column = 0, sticky = 'we', rowspan = 1, columnspan = 5, padx = 0, pady = 0, ipadx = 1, ipady = 1)
        tkgrid(bt.shpF, row = 1, column = 5, sticky = 'we', rowspan = 1, columnspan = 1, padx = 0, pady = 0, ipadx = 1, ipady = 1)
        tkgrid(txt.shpAttr, row = 2, column = 0, sticky = 'we', rowspan = 1, columnspan = 6, padx = 1, pady = 1, ipadx = 1, ipady = 1)
        tkgrid(.cdtData$EnvData$cb.shpAttr, row = 3, column = 0, sticky = 'we', rowspan = 1, columnspan = 5, padx = 1, pady = 1, ipadx = 1, ipady = 1)

        #################
        tkconfigure(bt.shpF, command = function(){
            shp.opfiles <- getOpenShp(.cdtEnv$tcl$main$win)
            if(!is.null(shp.opfiles)){
                update.OpenFiles('shp', shp.opfiles)
                tclvalue(shpFile) <- shp.opfiles[[1]]
                listOpenFiles[[length(listOpenFiles) + 1]] <<- shp.opfiles[[1]]
                tkconfigure(cb.shpF, values = unlist(listOpenFiles))

                shpf <- getShpOpenData(shpFile)
                dat <- shpf[[2]]@data
                AttrTable <- names(dat)
                tkconfigure(.cdtData$EnvData$cb.shpAttr, values = AttrTable)
                tclvalue(shpAttr) <- AttrTable[1]

                idx <- as.integer(tclvalue(tcl(.cdtData$EnvData$cb.shpAttr, 'current'))) + 1
                adminN <- as.character(dat[, idx])
                cbAttrTable <- levels(as.factor(adminN))
                tkconfigure(cb.Polygon, values = cbAttrTable)
                tclvalue(.cdtData$EnvData$namePoly) <- cbAttrTable[1]
            }
        })

        #######################
        tkbind(cb.shpF, "<<ComboboxSelected>>", function(){
            shpf <- getShpOpenData(shpFile)
            dat <- shpf[[2]]@data
            AttrTable <- names(dat)
            tkconfigure(.cdtData$EnvData$cb.shpAttr, values = AttrTable)
            tclvalue(shpAttr) <- AttrTable[1]

            idx <- as.integer(tclvalue(tcl(.cdtData$EnvData$cb.shpAttr, 'current'))) + 1
            adminN <- as.character(dat[, idx])
            cbAttrTable <- levels(as.factor(adminN))
            tkconfigure(cb.Polygon, values = cbAttrTable)
            tclvalue(.cdtData$EnvData$namePoly) <- cbAttrTable[1]
        })

        tkbind(.cdtData$EnvData$cb.shpAttr, "<<ComboboxSelected>>", function(){
            shpf <- getShpOpenData(shpFile)
            dat <- shpf[[2]]@data

            idx <- as.integer(tclvalue(tcl(.cdtData$EnvData$cb.shpAttr, 'current'))) + 1
            adminN <- as.character(dat[, idx])
            cbAttrTable <- levels(as.factor(adminN))
            tkconfigure(cb.Polygon, values = cbAttrTable)
            tclvalue(.cdtData$EnvData$namePoly) <- cbAttrTable[1]
        })

        #############################

        openAttrSHP <- ttkbutton(subfr1, text = lang.dlg[['button']][['2']])
        displayMapSHP <- ttkbutton(subfr1, text = lang.dlg[['button']][['3']])

        #################

        .cdtData$EnvData$tab$TableAttr <- NULL

        tkconfigure(openAttrSHP, command = function(){
            shpf <- getShpOpenData(shpFile)
            if(!is.null(shpf))
                .cdtData$EnvData$tab$TableAttr <- 
                    tableNotebookTab_unik(shpf[[2]]@data,
                                          .cdtData$EnvData$tab$TableAttr,
                                          shpf[[1]], 10)
        })

        #################

        .cdtData$EnvData$tab$MapSelect <- NULL

        tkconfigure(displayMapSHP, command = function(){
            shpofile <- getShpOpenData(shpFile)
            if(!is.null(shpofile))
            {
                shpf <- shpofile[[2]]
                .cdtData$EnvData$shpf <- shpf
                .cdtData$EnvData$ocrds <- getBoundaries(shpf)

                lo1 <- round(sp::bbox(shpf)[1, 1], 4)
                lo2 <- round(sp::bbox(shpf)[1, 2], 4)
                la1 <- round(sp::bbox(shpf)[2, 1], 4)
                la2 <- round(sp::bbox(shpf)[2, 2], 4)
                ZoomXYval0 <<- c(lo1, lo2, la1, la2)

                tclvalue(.cdtData$EnvData$zoom$xx1) <- lo1
                tclvalue(.cdtData$EnvData$zoom$xx2) <- lo2
                tclvalue(.cdtData$EnvData$zoom$yy1) <- la1
                tclvalue(.cdtData$EnvData$zoom$yy2) <- la2

                .cdtData$EnvData$ZoomXYval <- ZoomXYval0

                imgContainer <- displayMap4Extraction(.cdtData$EnvData$tab$MapSelect)
                .cdtData$EnvData$tab$MapSelect <- imageNotebookTab_unik(imgContainer, .cdtData$EnvData$tab$MapSelect)
            }
            else Insert.Messages.Out(lang.dlg[['message']][['1']], TRUE, 'e')
        })

        #############################
        tkgrid(frameTimeS, row = 0, column = 0, columnspan = 2, sticky = 'we', pady = 1, padx = 1)
        tkgrid(frameInData, row = 1, column = 0, columnspan = 2, sticky = 'we', pady = 2, padx = 1)
        tkgrid(frameSHP, row = 2, column = 0, columnspan = 2, sticky = 'we', pady = 2, padx = 1)
        tkgrid(openAttrSHP, row = 3, column = 0, columnspan = 1, sticky = 'we', pady = 1)
        tkgrid(displayMapSHP, row = 3, column = 1, columnspan = 1, sticky = 'we', pady = 1)

    ######################################################################################################

    #Tab2
    subfr2 <- bwTabScrollableFrame(cmd.tab2)

    ##########################################

        frameZoom <- ttklabelframe(subfr2, text = lang.dlg[['label']][['11']], relief = 'groove')

        xentr1.zoom <- tkentry(frameZoom, width = 8, justify = "left", textvariable = .cdtData$EnvData$zoom$xx1)
        xentr2.zoom <- tkentry(frameZoom, width = 8, justify = "left", textvariable = .cdtData$EnvData$zoom$xx2)
        yentr1.zoom <- tkentry(frameZoom, width = 8, justify = "left", textvariable = .cdtData$EnvData$zoom$yy1)
        yentr2.zoom <- tkentry(frameZoom, width = 8, justify = "left", textvariable = .cdtData$EnvData$zoom$yy2)
        bt.centre.zoom <- tklabel(frameZoom, image = .cdtEnv$tcl$zoom$img$centre)

        .cdtData$EnvData$zoom$btZoomP <- tkbutton(frameZoom, image = .cdtEnv$tcl$zoom$img$plus, relief = 'raised', bg = 'lightblue', state = 'normal')
        .cdtData$EnvData$zoom$btZoomM <- tkbutton(frameZoom, image = .cdtEnv$tcl$zoom$img$moins, relief = 'raised', bg = 'lightblue', state = 'normal')
        .cdtData$EnvData$zoom$btZoomRect <- tkbutton(frameZoom, image = .cdtEnv$tcl$zoom$img$rect, relief = 'raised', bg = 'lightblue', state = 'normal')
        .cdtData$EnvData$zoom$btPanImg <- tkbutton(frameZoom, image = .cdtEnv$tcl$zoom$img$pan, relief = 'raised', bg = 'lightblue', state = 'normal')
        .cdtData$EnvData$zoom$btRedraw <- tkbutton(frameZoom, image = .cdtEnv$tcl$zoom$img$redraw, relief = 'raised', bg = 'lightblue')
        .cdtData$EnvData$zoom$btReset <- tkbutton(frameZoom, image = .cdtEnv$tcl$zoom$img$reset, relief = 'raised')

        #################
        tkgrid(xentr1.zoom, row = 1, column = 0, sticky = 'we', rowspan = 1, columnspan = 1)
        tkgrid(xentr2.zoom, row = 1, column = 2, sticky = 'we', rowspan = 1, columnspan = 1)
        tkgrid(yentr1.zoom, row = 2, column = 1, sticky = 'we', rowspan = 1, columnspan = 1)
        tkgrid(yentr2.zoom, row = 0, column = 1, sticky = 'we', rowspan = 1, columnspan = 1)
        tkgrid(bt.centre.zoom, row = 1, column = 1, sticky = 'nswe', rowspan = 1, columnspan = 1)

        tkgrid(.cdtData$EnvData$zoom$btReset, row = 0, column = 3, sticky = 'nswe', rowspan = 1, columnspan = 1)
        tkgrid(.cdtData$EnvData$zoom$btRedraw, row = 1, column = 3, sticky = 'nswe', rowspan = 1, columnspan = 1)
        tkgrid(.cdtData$EnvData$zoom$btPanImg, row = 2, column = 3, sticky = 'nswe', rowspan = 1, columnspan = 1)
        tkgrid(.cdtData$EnvData$zoom$btZoomP, row = 0, column = 4, sticky = 'nswe', rowspan = 1, columnspan = 1)
        tkgrid(.cdtData$EnvData$zoom$btZoomM, row = 1, column = 4, sticky = 'nswe', rowspan = 1, columnspan = 1)
        tkgrid(.cdtData$EnvData$zoom$btZoomRect, row = 2, column = 4, sticky = 'nswe', rowspan = 1, columnspan = 1)

        helpWidget(.cdtData$EnvData$zoom$btZoomP, lang.dlg[['tooltip']][['7']], lang.dlg[['status']][['7']])
        helpWidget(.cdtData$EnvData$zoom$btZoomM, lang.dlg[['tooltip']][['8']], lang.dlg[['status']][['8']])
        helpWidget(.cdtData$EnvData$zoom$btZoomRect, lang.dlg[['tooltip']][['9']], lang.dlg[['status']][['9']])
        helpWidget(.cdtData$EnvData$zoom$btPanImg, lang.dlg[['tooltip']][['10']], lang.dlg[['status']][['10']])
        helpWidget(.cdtData$EnvData$zoom$btRedraw, lang.dlg[['tooltip']][['11']], lang.dlg[['status']][['11']])
        helpWidget(.cdtData$EnvData$zoom$btReset, lang.dlg[['tooltip']][['12']], lang.dlg[['status']][['12']])

        ##########################################

        tkconfigure(.cdtData$EnvData$zoom$btRedraw, command = function(){
            .cdtData$EnvData$ZoomXYval <- as.numeric(c(tclvalue(.cdtData$EnvData$zoom$xx1),
                                                       tclvalue(.cdtData$EnvData$zoom$xx2),
                                                       tclvalue(.cdtData$EnvData$zoom$yy1),
                                                       tclvalue(.cdtData$EnvData$zoom$yy2)))

            # ZoomXYval
            tabid <- as.numeric(tclvalue(tkindex(.cdtEnv$tcl$main$tknotes, 'current'))) + 1
            if(length(.cdtData$OpenTab$Type) > 0){
                if(.cdtData$OpenTab$Type[[tabid]] == "img" & !is.null(.cdtData$EnvData$tab$MapSelect))
                {
                    if(.cdtData$OpenTab$Data[[tabid]][[1]][[1]]$ID == .cdtData$EnvData$tab$MapSelect[[2]])
                    {
                        refreshPlot(W = .cdtData$OpenTab$Data[[tabid]][[2]][[1]],
                                    img = .cdtData$OpenTab$Data[[tabid]][[2]][[2]],
                                    hscale = as.numeric(tclvalue(tkget(.cdtEnv$tcl$toolbar$spinH))),
                                    vscale = as.numeric(tclvalue(tkget(.cdtEnv$tcl$toolbar$spinV))))
                        tkconfigure(.cdtData$EnvData$zoom$btRedraw, relief = 'raised', bg = 'lightblue')
                    }
                }
            }
        })

        tkconfigure(.cdtData$EnvData$zoom$btReset, command = function(){
            .cdtData$EnvData$ZoomXYval <- ZoomXYval0
            tclvalue(.cdtData$EnvData$zoom$xx1) <- ZoomXYval0[1]
            tclvalue(.cdtData$EnvData$zoom$xx2) <- ZoomXYval0[2]
            tclvalue(.cdtData$EnvData$zoom$yy1) <- ZoomXYval0[3]
            tclvalue(.cdtData$EnvData$zoom$yy2) <- ZoomXYval0[4]

            # ZoomXYval
            tabid <- as.numeric(tclvalue(tkindex(.cdtEnv$tcl$main$tknotes, 'current'))) + 1
            if(length(.cdtData$OpenTab$Type) > 0){
                if(.cdtData$OpenTab$Type[[tabid]] == "img" & !is.null(.cdtData$EnvData$tab$MapSelect))
                {
                    if(.cdtData$OpenTab$Data[[tabid]][[1]][[1]]$ID == .cdtData$EnvData$tab$MapSelect[[2]])
                    {
                        refreshPlot(W = .cdtData$OpenTab$Data[[tabid]][[2]][[1]],
                                    img = .cdtData$OpenTab$Data[[tabid]][[2]][[2]],
                                    hscale = as.numeric(tclvalue(tkget(.cdtEnv$tcl$toolbar$spinH))),
                                    vscale = as.numeric(tclvalue(tkget(.cdtEnv$tcl$toolbar$spinV))))
                        tkconfigure(.cdtData$EnvData$zoom$btRedraw, relief = 'raised', bg = 'lightblue')
                    }
                }
            }
        })

        ##########################################

        tkbind(xentr1.zoom, "<FocusIn>", initializeButZoom)
        tkbind(xentr1.zoom, "<FocusOut>", activateButRedraw)

        tkbind(xentr2.zoom, "<FocusIn>", initializeButZoom)
        tkbind(xentr2.zoom, "<FocusOut>", activateButRedraw)

        tkbind(yentr1.zoom, "<FocusIn>", initializeButZoom)
        tkbind(yentr1.zoom, "<FocusOut>", activateButRedraw)

        tkbind(yentr2.zoom, "<FocusIn>", initializeButZoom)
        tkbind(yentr2.zoom, "<FocusOut>", activateButRedraw)

        ##########################################
        tkbind(.cdtData$EnvData$zoom$btRedraw, "<Button-1>", function(){
            tclvalue(.cdtData$EnvData$zoom$pressButP) <- 0
            tclvalue(.cdtData$EnvData$zoom$pressButM) <- 0
            tclvalue(.cdtData$EnvData$zoom$pressButRect) <- 0
            tclvalue(.cdtData$EnvData$zoom$pressButDrag) <- 0

            tclvalue(.cdtData$EnvData$pressGetCoords) <- 0

            tkconfigure(.cdtData$EnvData$zoom$btRedraw, relief = 'raised', bg = 'lightblue')
            tkconfigure(.cdtData$EnvData$zoom$btZoomP, relief = 'raised', bg = 'lightblue', state = 'normal')
            tkconfigure(.cdtData$EnvData$zoom$btZoomM, relief = 'raised', bg = 'lightblue', state = 'normal')
            tkconfigure(.cdtData$EnvData$zoom$btZoomRect, relief = 'raised', bg = 'lightblue', state = 'normal')
            tkconfigure(.cdtData$EnvData$zoom$btPanImg, relief = 'raised', bg = 'lightblue', state = 'normal')

            tkconfigure(.cdtData$EnvData$bt.GETArea, relief = 'raised', bg = 'lightblue', state = 'normal')
            stateADD <- if(.cdtData$EnvData$type.extract %in% c('mpoint', 'mpoly')) "normal" else "disabled"
            tkconfigure(.cdtData$EnvData$bt.ADDObj, relief = 'raised', bg = 'lightblue', state = stateADD)
        })

        tkbind(.cdtData$EnvData$zoom$btReset, "<Button-1>", function(){
            tclvalue(.cdtData$EnvData$zoom$pressButP) <- 0
            tclvalue(.cdtData$EnvData$zoom$pressButM) <- 0
            tclvalue(.cdtData$EnvData$zoom$pressButRect) <- 0
            tclvalue(.cdtData$EnvData$zoom$pressButDrag) <- 0

            tclvalue(.cdtData$EnvData$pressGetCoords) <- 0

            tkconfigure(.cdtData$EnvData$zoom$btRedraw, relief = 'raised', bg = 'lightblue')
            tkconfigure(.cdtData$EnvData$zoom$btZoomP, relief = 'raised', bg = 'lightblue', state = 'normal')
            tkconfigure(.cdtData$EnvData$zoom$btZoomM, relief = 'raised', bg = 'lightblue', state = 'normal')
            tkconfigure(.cdtData$EnvData$zoom$btZoomRect, relief = 'raised', bg = 'lightblue', state = 'normal')
            tkconfigure(.cdtData$EnvData$zoom$btPanImg, relief = 'raised', bg = 'lightblue', state = 'normal')

            tkconfigure(.cdtData$EnvData$bt.GETArea, relief = 'raised', bg = 'lightblue', state = 'normal')
            stateADD <- if(.cdtData$EnvData$type.extract %in% c('mpoint', 'mpoly')) "normal" else "disabled"
            tkconfigure(.cdtData$EnvData$bt.ADDObj, relief = 'raised', bg = 'lightblue', state = stateADD)
        })

        tkbind(.cdtData$EnvData$zoom$btZoomP, "<Button-1>", function(){
            tclvalue(.cdtData$EnvData$zoom$pressButP) <- 1
            tclvalue(.cdtData$EnvData$zoom$pressButM) <- 0
            tclvalue(.cdtData$EnvData$zoom$pressButRect) <- 0
            tclvalue(.cdtData$EnvData$zoom$pressButDrag) <- 0

            tclvalue(.cdtData$EnvData$pressGetCoords) <- 0

            tkconfigure(.cdtData$EnvData$zoom$btRedraw, relief = 'raised', bg = 'lightblue')
            tkconfigure(.cdtData$EnvData$zoom$btZoomP, relief = 'raised', bg = 'red', state = 'disabled')
            tkconfigure(.cdtData$EnvData$zoom$btZoomM, relief = 'raised', bg = 'lightblue', state = 'normal')
            tkconfigure(.cdtData$EnvData$zoom$btZoomRect, relief = 'raised', bg = 'lightblue', state = 'normal')
            tkconfigure(.cdtData$EnvData$zoom$btPanImg, relief = 'raised', bg = 'lightblue', state = 'normal')

            tkconfigure(.cdtData$EnvData$bt.GETArea, relief = 'raised', bg = 'lightblue', state = 'normal')
            stateADD <- if(.cdtData$EnvData$type.extract %in% c('mpoint', 'mpoly')) "normal" else "disabled"
            tkconfigure(.cdtData$EnvData$bt.ADDObj, relief = 'raised', bg = 'lightblue', state = stateADD)
        })

        tkbind(.cdtData$EnvData$zoom$btZoomM, "<Button-1>", function(){
            tclvalue(.cdtData$EnvData$zoom$pressButP) <- 0
            tclvalue(.cdtData$EnvData$zoom$pressButM) <- 1
            tclvalue(.cdtData$EnvData$zoom$pressButRect) <- 0
            tclvalue(.cdtData$EnvData$zoom$pressButDrag) <- 0

            tclvalue(.cdtData$EnvData$pressGetCoords) <- 0

            tkconfigure(.cdtData$EnvData$zoom$btRedraw, relief = 'raised', bg = 'lightblue')
            tkconfigure(.cdtData$EnvData$zoom$btZoomP, relief = 'raised', bg = 'lightblue', state = 'normal')
            tkconfigure(.cdtData$EnvData$zoom$btZoomM, relief = 'raised', bg = 'red', state = 'disabled')
            tkconfigure(.cdtData$EnvData$zoom$btZoomRect, relief = 'raised', bg = 'lightblue', state = 'normal')
            tkconfigure(.cdtData$EnvData$zoom$btPanImg, relief = 'raised', bg = 'lightblue', state = 'normal')

            tkconfigure(.cdtData$EnvData$bt.GETArea, relief = 'raised', bg = 'lightblue', state = 'normal')
            stateADD <- if(.cdtData$EnvData$type.extract %in% c('mpoint', 'mpoly')) "normal" else "disabled"
            tkconfigure(.cdtData$EnvData$bt.ADDObj, relief = 'raised', bg = 'lightblue', state = stateADD)
        })

        tkbind(.cdtData$EnvData$zoom$btZoomRect, "<Button-1>", function(){
            tclvalue(.cdtData$EnvData$zoom$pressButP) <- 0
            tclvalue(.cdtData$EnvData$zoom$pressButM) <- 0
            tclvalue(.cdtData$EnvData$zoom$pressButRect) <- 1
            tclvalue(.cdtData$EnvData$zoom$pressButDrag) <- 0

            tclvalue(.cdtData$EnvData$pressGetCoords) <- 0

            tkconfigure(.cdtData$EnvData$zoom$btRedraw, relief = 'raised', bg = 'lightblue')
            tkconfigure(.cdtData$EnvData$zoom$btZoomP, relief = 'raised', bg = 'lightblue', state = 'normal')
            tkconfigure(.cdtData$EnvData$zoom$btZoomM, relief = 'raised', bg = 'lightblue', state = 'normal')
            tkconfigure(.cdtData$EnvData$zoom$btZoomRect, relief = 'raised', bg = 'red', state = 'disabled')
            tkconfigure(.cdtData$EnvData$zoom$btPanImg, relief = 'raised', bg = 'lightblue', state = 'normal')

            tkconfigure(.cdtData$EnvData$bt.GETArea, relief = 'raised', bg = 'lightblue', state = 'normal')
            stateADD <- if(.cdtData$EnvData$type.extract %in% c('mpoint', 'mpoly')) "normal" else "disabled"
            tkconfigure(.cdtData$EnvData$bt.ADDObj, relief = 'raised', bg = 'lightblue', state = stateADD)
        })

        tkbind(.cdtData$EnvData$zoom$btPanImg, "<Button-1>", function(){
            tclvalue(.cdtData$EnvData$zoom$pressButP) <- 0
            tclvalue(.cdtData$EnvData$zoom$pressButM) <- 0
            tclvalue(.cdtData$EnvData$zoom$pressButRect) <- 0
            tclvalue(.cdtData$EnvData$zoom$pressButDrag) <- 1

            tclvalue(.cdtData$EnvData$pressGetCoords) <- 0

            tkconfigure(.cdtData$EnvData$zoom$btRedraw, relief = 'raised', bg = 'lightblue')
            tkconfigure(.cdtData$EnvData$zoom$btZoomP, relief = 'raised', bg = 'lightblue', state = 'normal')
            tkconfigure(.cdtData$EnvData$zoom$btZoomM, relief = 'raised', bg = 'lightblue', state = 'normal')
            tkconfigure(.cdtData$EnvData$zoom$btZoomRect, relief = 'raised', bg = 'lightblue', state = 'normal')
            tkconfigure(.cdtData$EnvData$zoom$btPanImg, relief = 'raised', bg = 'red', state = 'disabled')

            tkconfigure(.cdtData$EnvData$bt.GETArea, relief = 'raised', bg = 'lightblue', state = 'normal')
            stateADD <- if(.cdtData$EnvData$type.extract %in% c('mpoint', 'mpoly')) "normal" else "disabled"
            tkconfigure(.cdtData$EnvData$bt.ADDObj, relief = 'raised', bg = 'lightblue', state = stateADD)
        })

        ##########################################

        frameExtract <- ttklabelframe(subfr2, text = lang.dlg[['label']][['12']], relief = 'groove')

        .cdtData$EnvData$type.extract <- GeneralParameters$type.extract
        type.extract <- tclVar()
        typeEXTRACT <- lang.dlg[['combobox']][['1']]
        typeEXTRACTV <- c('point', 'mpoint', 'rect', 'poly', 'mpoly')
        tclvalue(type.extract) <- typeEXTRACT[typeEXTRACTV %in% GeneralParameters$type.extract]

        .cdtData$EnvData$minlonRect <- tclVar(GeneralParameters$Geom$minlon)
        .cdtData$EnvData$maxlonRect <- tclVar(GeneralParameters$Geom$maxlon)
        .cdtData$EnvData$minlatRect <- tclVar(GeneralParameters$Geom$minlat)
        .cdtData$EnvData$maxlatRect <- tclVar(GeneralParameters$Geom$maxlat)
        .cdtData$EnvData$namePoly <- tclVar(GeneralParameters$Geom$namePoly)

        padLon <- tclVar(GeneralParameters$Geom$padlon)
        padLat <- tclVar(GeneralParameters$Geom$padlat)

        pointrect <- tclVar("Point")
        minrect <- tclVar()
        maxrect <- tclVar()

        statePts <- if(GeneralParameters$type.extract %in% c('point', 'mpoint')) "normal" else "disabled"
        stateRct <- if(GeneralParameters$type.extract == 'rect') "normal" else "disabled"
        statePad <- "normal"
        statePol <- if(GeneralParameters$type.extract %in% c('poly', 'mpoly')) "normal" else "disabled"
        stateADD <- if(GeneralParameters$type.extract %in% c('mpoint', 'mpoly')) "normal" else "disabled"

        ################

        txt.Type <- tklabel(frameExtract, text = lang.dlg[['label']][['13']], anchor = 'e', justify = 'right')
        cb.TypeExtr <- ttkcombobox(frameExtract, values = typeEXTRACT, textvariable = type.extract, width = largeur3)

        txt.PointRect <- tklabel(frameExtract, text = tclvalue(pointrect), textvariable = pointrect, anchor = 'e', justify = 'right', bg = 'green')
        txt.MIN <- tklabel(frameExtract, text = tclvalue(minrect), textvariable = minrect)
        txt.MAX <- tklabel(frameExtract, text = tclvalue(maxrect), textvariable = maxrect)
        txt.PAD <- tklabel(frameExtract, text = lang.dlg[['label']][['14']])

        txt.LON <- tklabel(frameExtract, text = lang.dlg[['label']][['15']], anchor = 'e', justify = 'right')
        en.minlon <- tkentry(frameExtract, width = 7, textvariable = .cdtData$EnvData$minlonRect, justify = "left", state = statePts)
        en.maxlon <- tkentry(frameExtract, width = 7, textvariable = .cdtData$EnvData$maxlonRect, justify = "left", state = stateRct)
        txt.PlusM1 <- tklabel(frameExtract, text = '\u00B1')
        en.PadLon <- tkentry(frameExtract, width = 4, textvariable = padLon, justify = "left", state = statePad)

        txt.LAT <- tklabel(frameExtract, text = lang.dlg[['label']][['16']], anchor = 'e', justify = 'right')
        en.minlat <- tkentry(frameExtract, width = 7, textvariable = .cdtData$EnvData$minlatRect, justify = "left", state = statePts)
        en.maxlat <- tkentry(frameExtract, width = 7, textvariable = .cdtData$EnvData$maxlatRect, justify = "left", state = stateRct)
        txt.PlusM2 <- tklabel(frameExtract, text = '\u00B1')
        en.PadLat <- tkentry(frameExtract, width = 4, textvariable = padLat, justify = "left", state = statePad)

        txt.Polygon <- tklabel(frameExtract, text = lang.dlg[['label']][['17']], anchor = 'e', justify = 'right', bg = 'green')
        cb.Polygon <- ttkcombobox(frameExtract, values = '', textvariable = .cdtData$EnvData$namePoly, state = statePol, width = largeur3)

        .cdtData$EnvData$bt.ADDObj <- tkbutton(frameExtract, text = lang.dlg[['button']][['4']], relief = 'raised', bg = 'lightblue', state = stateADD)
        .cdtData$EnvData$bt.GETArea <- tkbutton(frameExtract, text = lang.dlg[['button']][['5']], relief = 'raised', bg = 'lightblue', state = 'normal')

       #############

        tkgrid(txt.Type, row = 0, column = 0, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 2, ipadx = 1, ipady = 1)
        tkgrid(cb.TypeExtr, row = 0, column = 1, sticky = 'we', rowspan = 1, columnspan = 4, padx = 1, pady = 2, ipadx = 1, ipady = 1)

        tkgrid(txt.PointRect, row = 1, column = 0, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
        tkgrid(txt.MIN, row = 1, column = 1, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
        tkgrid(txt.MAX, row = 1, column = 2, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
        tkgrid(txt.PAD, row = 1, column = 4, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)

        tkgrid(txt.LON, row = 2, column = 0, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
        tkgrid(en.minlon, row = 2, column = 1, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
        tkgrid(en.maxlon, row = 2, column = 2, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
        tkgrid(txt.PlusM1, row = 2, column = 3, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
        tkgrid(en.PadLon, row = 2, column = 4, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)

        tkgrid(txt.LAT, row = 3, column = 0, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
        tkgrid(en.minlat, row = 3, column = 1, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
        tkgrid(en.maxlat, row = 3, column = 2, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
        tkgrid(txt.PlusM2, row = 3, column = 3, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
        tkgrid(en.PadLat, row = 3, column = 4, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)

        tkgrid(txt.Polygon, row = 4, column = 0, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 2, ipadx = 1, ipady = 1)
        tkgrid(cb.Polygon, row = 4, column = 1, sticky = 'we', rowspan = 1, columnspan = 4, padx = 1, pady = 2, ipadx = 1, ipady = 1)

        tkgrid(.cdtData$EnvData$bt.ADDObj, row = 5, column = 0, sticky = 'we', rowspan = 1, columnspan = 2, padx = 1, pady = 1, ipadx = 1, ipady = 1)
        tkgrid(.cdtData$EnvData$bt.GETArea, row = 5, column = 2, sticky = 'we', rowspan = 1, columnspan = 3, padx = 1, pady = 1, ipadx = 1, ipady = 1)

        ################
        helpWidget(en.PadLon, lang.dlg[['tooltip']][['13']], lang.dlg[['status']][['13']])
        helpWidget(en.PadLat, lang.dlg[['tooltip']][['14']], lang.dlg[['status']][['14']])
        helpWidget(.cdtData$EnvData$bt.GETArea, lang.dlg[['tooltip']][['15']], lang.dlg[['status']][['15']])
        helpWidget(.cdtData$EnvData$bt.ADDObj, lang.dlg[['tooltip']][['16']], lang.dlg[['status']][['16']])

        ##########################################
        nbpts <- 1
        retMultiP <- NULL

        tkconfigure(.cdtData$EnvData$bt.ADDObj, command = function(){
            if(.cdtData$EnvData$dlgBoxOpen){
                if(.cdtData$EnvData$type.extract == 'mpoint')
                {
                    tkinsert(retMultiP$textObj, "end", paste(paste('Pts', nbpts, sep = ''),
                                                        tclvalue(.cdtData$EnvData$minlonRect),
                                                        tclvalue(.cdtData$EnvData$minlatRect), "\n"))
                    nbpts <<- nbpts + 1
                }
                if(.cdtData$EnvData$type.extract == 'mpoly')
                {
                    tkinsert(retMultiP$textObj, "end", paste(tclvalue(.cdtData$EnvData$namePoly), "\n"))
                }
            }
            stateADD <- if(.cdtData$EnvData$type.extract %in% c('mpoint', 'mpoly')) "normal" else "disabled"
            tkconfigure(.cdtData$EnvData$bt.ADDObj, relief = 'raised', bg = 'lightblue', state = stateADD)
        })

        #################
        tkbind(cb.Polygon, "<<ComboboxSelected>>", function(){
            .cdtData$EnvData$selectedPolygon <- NULL
            if(tclvalue(.cdtData$EnvData$namePoly) != ''){
                shpfopen <- getShpOpenData(shpFile)
                shpf <- shpfopen[[2]]
                ids <- as.numeric(tclvalue(tcl(.cdtData$EnvData$cb.shpAttr, 'current'))) + 1
                .cdtData$EnvData$selectedPolygon <- getBoundaries(shpf[shpf@data[, ids] == tclvalue(.cdtData$EnvData$namePoly), ])
            }

            # selectedPolygon
            tabid <- as.numeric(tclvalue(tkindex(.cdtEnv$tcl$main$tknotes, 'current'))) + 1
            if(length(.cdtData$OpenTab$Type) > 0){
                if(.cdtData$OpenTab$Type[[tabid]] == "img" & !is.null(.cdtData$EnvData$tab$MapSelect))
                {
                    if(.cdtData$OpenTab$Data[[tabid]][[1]][[1]]$ID == .cdtData$EnvData$tab$MapSelect[[2]])
                    {
                        refreshPlot(W = .cdtData$OpenTab$Data[[tabid]][[2]][[1]],
                                    img = .cdtData$OpenTab$Data[[tabid]][[2]][[2]],
                                    hscale = as.numeric(tclvalue(tkget(.cdtEnv$tcl$toolbar$spinH))),
                                    vscale = as.numeric(tclvalue(tkget(.cdtEnv$tcl$toolbar$spinV))))
                    }
                }
            }
        })

        ##################
        tkbind(.cdtData$EnvData$bt.GETArea, "<Button-1>", function(){
            tclvalue(.cdtData$EnvData$zoom$pressButP) <- 0
            tclvalue(.cdtData$EnvData$zoom$pressButM) <- 0
            tclvalue(.cdtData$EnvData$zoom$pressButRect) <- 0
            tclvalue(.cdtData$EnvData$zoom$pressButDrag) <- 0

            tclvalue(.cdtData$EnvData$pressGetCoords) <- 1

            tkconfigure(.cdtData$EnvData$zoom$btRedraw, relief = 'raised', bg = 'lightblue')

            tkconfigure(.cdtData$EnvData$zoom$btZoomP, relief = 'raised', bg = 'lightblue', state = 'normal')
            tkconfigure(.cdtData$EnvData$zoom$btZoomM, relief = 'raised', bg = 'lightblue', state = 'normal')
            tkconfigure(.cdtData$EnvData$zoom$btZoomRect, relief = 'raised', bg = 'lightblue', state = 'normal')
            tkconfigure(.cdtData$EnvData$zoom$btPanImg, relief = 'raised', bg = 'lightblue', state = 'normal')

            tkconfigure(.cdtData$EnvData$bt.GETArea, relief = 'raised', bg = 'red', state = 'disabled')
            stateADD <- if(.cdtData$EnvData$type.extract %in% c('mpoint', 'mpoly')) "normal" else "disabled"
            tkconfigure(.cdtData$EnvData$bt.ADDObj, relief = 'raised', bg = 'lightblue', state = stateADD)
        })

        ##################

        tkbind(cb.TypeExtr, "<<ComboboxSelected>>", function(){
            .cdtData$EnvData$selectedPolygon <- NULL
            OutFileFormat <- CbOutFileFormat

            .cdtData$EnvData$type.extract <- typeEXTRACTV[typeEXTRACT %in% trimws(tclvalue(type.extract))]
            outfrmt <- trimws(tclvalue(out.format))

            if(.cdtData$EnvData$type.extract == "point")
            {
                if(!is.null(retMultiP$win)) tkdestroy(retMultiP$win)

                statePts <- 'normal'
                stateRct <- 'disabled'
                statePad <- 'normal'
                statePol <- 'disabled'
                stateADD <- 'disabled'

                pointrectVal <- 'Point'
                minrectVal <- ''
                maxrectVal <- ''

                stateSpAv <- 'disabled'
                spatAverg <- '0'

                txtfileORdir <- lang.dlg[['label']][['19']]
                colfileORdir <- 'lightblue'
                isFile <- TRUE
 
                OutFileFormat <- OutFileFormat[1:2]
                out.formatVAL <- if(outfrmt %in% CbOutFileFormat[3:4]) OutFileFormat[1] else tclvalue(out.format)
            }

            ##
            if(.cdtData$EnvData$type.extract == "rect")
            {
                if(!is.null(retMultiP$win)) tkdestroy(retMultiP$win)

                statePts <- 'normal'
                stateRct <- 'normal'
                statePad <- 'disabled'
                statePol <- 'disabled'
                stateADD <- 'disabled'

                pointrectVal <- 'Rectangle'
                minrectVal <- 'Minimum'
                maxrectVal <- 'Maximum'

                stateSpAv <- if(outfrmt == CbOutFileFormat[2]) 'normal' else 'disabled'

                if(outfrmt %in% CbOutFileFormat[3:4]){
                    spatAverg <- "0"
                }else if(outfrmt == CbOutFileFormat[1]){
                    spatAverg <- "1"
                }else spatAverg <- tclvalue(spatAverage)

                if(outfrmt == CbOutFileFormat[3]){
                    txtfileORdir <- lang.dlg[['label']][['20']]
                    colfileORdir <- 'lightgreen'
                    isFile <- FALSE
                }else{
                    txtfileORdir <- lang.dlg[['label']][['19']]
                    colfileORdir <- 'lightblue'
                    isFile <- TRUE
                }

                out.formatVAL <- tclvalue(out.format)
            }

            ##
            if(.cdtData$EnvData$type.extract == "poly")
            {
                if(!is.null(retMultiP$win)) tkdestroy(retMultiP$win)

                statePts <- 'disabled'
                stateRct <- 'disabled'
                statePad <- 'disabled'
                statePol <- 'normal'
                stateADD <- 'disabled'

                pointrectVal <- tclvalue(pointrect)
                minrectVal <- ''
                maxrectVal <- ''

                stateSpAv <- if(outfrmt == CbOutFileFormat[2]) 'normal' else 'disabled'

                if(outfrmt %in% CbOutFileFormat[3:4]){
                    spatAverg <- "0"
                }else if(outfrmt == CbOutFileFormat[1]){
                    spatAverg <- "1"
                }else spatAverg <- tclvalue(spatAverage)

                if(outfrmt == CbOutFileFormat[3]){
                    txtfileORdir <- lang.dlg[['label']][['20']]
                    colfileORdir <- 'lightgreen'
                    isFile <- FALSE
                }else{
                    txtfileORdir <- lang.dlg[['label']][['19']]
                    colfileORdir <- 'lightblue'
                    isFile <- TRUE
                }

                out.formatVAL <- tclvalue(out.format)

                if(tclvalue(.cdtData$EnvData$namePoly) != '')
                {
                    shpfopen <- getShpOpenData(shpFile)
                    shpf <- shpfopen[[2]]
                    ids <- as.integer(tclvalue(tcl(.cdtData$EnvData$cb.shpAttr, 'current'))) + 1
                    nompoly <- trimws(tclvalue(.cdtData$EnvData$namePoly))
                    .cdtData$EnvData$selectedPolygon <- getBoundaries(shpf[shpf@data[, ids] == nompoly, ])
                }
            }

            ##
            if(.cdtData$EnvData$type.extract == "mpoint")
            {
                if(!is.null(retMultiP$win)) tkdestroy(retMultiP$win)
                retMultiP <<- extractTS.previewWin(c('normal', 'disabled'),
                                                    list(.cdtData$EnvData$cb.shpAttr, shpFile),
                                                    .cdtData$EnvData$type.extract)
                statePts <- 'normal'
                stateRct <- 'disabled'
                statePad <- 'normal'
                statePol <- 'disabled'
                stateADD <- 'normal'

                pointrectVal <- 'Point'
                minrectVal <- ''
                maxrectVal <- ''

                stateSpAv <- 'disabled'
                spatAverg <- '0'

                txtfileORdir <- lang.dlg[['label']][['19']]
                colfileORdir <- 'lightblue'
                isFile <- TRUE

                OutFileFormat <- OutFileFormat[1:2]
                out.formatVAL <- if(outfrmt %in% CbOutFileFormat[3:4]) OutFileFormat[1] else tclvalue(out.format)
            }

            ##
            if(.cdtData$EnvData$type.extract == "mpoly")
            {
                if(!is.null(retMultiP$win)) tkdestroy(retMultiP$win)
                retMultiP <<- extractTS.previewWin(c('disabled', 'normal'),
                                                    list(.cdtData$EnvData$cb.shpAttr, shpFile),
                                                    .cdtData$EnvData$type.extract)
                statePts <- 'disabled'
                stateRct <- 'disabled'
                statePad <- 'disabled'
                statePol <- 'normal'
                stateADD <- 'normal'

                pointrectVal <- tclvalue(pointrect)
                minrectVal <- ''
                maxrectVal <- ''

                #################

                stateSpAv <- 'disabled'
                spatAverg <- if(outfrmt %in% CbOutFileFormat[3:4]) "0" else "1"

                if(outfrmt %in% CbOutFileFormat[3:4]){
                    txtfileORdir <- lang.dlg[['label']][['20']]
                    colfileORdir <- 'lightgreen'
                    isFile <- FALSE
                }else{
                    txtfileORdir <- lang.dlg[['label']][['19']]
                    colfileORdir <- 'lightblue'
                    isFile <- TRUE
                }

                out.formatVAL <- tclvalue(out.format)

                #################

                if(tclvalue(.cdtData$EnvData$namePoly) != ''){
                    shpfopen <- getShpOpenData(shpFile)
                    shpf <- shpfopen[[2]]
                    ids <- as.integer(tclvalue(tcl(.cdtData$EnvData$cb.shpAttr, 'current'))) + 1
                    nompoly <- trimws(tclvalue(.cdtData$EnvData$namePoly))
                    .cdtData$EnvData$selectedPolygon <- getBoundaries(shpf[shpf@data[, ids] == nompoly, ])
                }
            }

            tkconfigure(en.minlon, state = statePts)
            tkconfigure(en.minlat, state = statePts)
            tkconfigure(en.maxlon, state = stateRct)
            tkconfigure(en.maxlat, state = stateRct)
            tkconfigure(en.PadLon, state = statePad)
            tkconfigure(en.PadLat, state = statePad)

            tkconfigure(cb.Polygon, state = statePol)
            tkconfigure(.cdtData$EnvData$bt.ADDObj, state = stateADD)

            tclvalue(pointrect) <- pointrectVal
            tclvalue(minrect) <- minrectVal
            tclvalue(maxrect) <- maxrectVal

            ##
            tkconfigure(chk.SpAvrg, state = stateSpAv)
            tclvalue(spatAverage) <- spatAverg
            tkconfigure(txt.saveData, bg = colfileORdir)
            tclvalue(fileORdir) <- txtfileORdir
            tclvalue(outputFD) <- isFile
            tkconfigure(bt.saveData, command = function() fileORdir2Save(file2save, isFile = isFile))

            tkconfigure(cb.outFormat, values = OutFileFormat)
            tclvalue(out.format) <- out.formatVAL

            ##
            tclvalue(.cdtData$EnvData$minlonRect) <- ''
            tclvalue(.cdtData$EnvData$maxlonRect) <- ''
            tclvalue(.cdtData$EnvData$minlatRect) <- ''
            tclvalue(.cdtData$EnvData$maxlatRect) <- ''
            tkconfigure(.cdtData$EnvData$bt.GETArea, relief = 'raised', bg = 'lightblue', state = 'normal')

            # selectedPolygon
            tabid <- as.numeric(tclvalue(tkindex(.cdtEnv$tcl$main$tknotes, 'current'))) + 1
            if(length(.cdtData$OpenTab$Type) > 0){
                if(.cdtData$OpenTab$Type[[tabid]] == "img" & !is.null(.cdtData$EnvData$tab$MapSelect))
                {
                    if(.cdtData$OpenTab$Data[[tabid]][[1]][[1]]$ID == .cdtData$EnvData$tab$MapSelect[[2]])
                    {
                        refreshPlot(W = .cdtData$OpenTab$Data[[tabid]][[2]][[1]],
                                    img = .cdtData$OpenTab$Data[[tabid]][[2]][[2]],
                                    hscale = as.numeric(tclvalue(tkget(.cdtEnv$tcl$toolbar$spinH))),
                                    vscale = as.numeric(tclvalue(tkget(.cdtEnv$tcl$toolbar$spinV))))
                        tkdelete(tkwinfo('children', .cdtData$OpenTab$Data[[tabid]][[1]][[2]]), 'rect')
                    }
                }
            }
        })

        #############################
        tkgrid(frameZoom, row = 0, column = 0, sticky = '')
        tkgrid(frameExtract, row = 1, column = 0, sticky = 'we', pady = 3)

    ######################################################################################################

    #Tab3
    subfr3 <- bwTabScrollableFrame(cmd.tab3)

    ##########################################

        frameDate <- tkframe(subfr3, relief = 'groove', borderwidth = 2)

        startMonth <- tclVar(MOIS[GeneralParameters$months$start])
        endMonth <- tclVar(MOIS[GeneralParameters$months$end])

        bt.daterange <- ttkbutton(frameDate, text = lang.dlg[['button']][['1']])
        txt.month <- tklabel(frameDate, text = lang.dlg[['label']][['6']], anchor = 'w', justify = 'left')
        fr.Month <- tkframe(frameDate)

        txt.startMonth <- tklabel(fr.Month, text = lang.dlg[['label']][['7']], anchor = 'e', justify = 'right')
        cb.startMonth <- ttkcombobox(fr.Month, values = MOIS, textvariable = startMonth, width = largeur5)
        txt.endMonth <- tklabel(fr.Month, text = lang.dlg[['label']][['8']])
        cb.endMonth <- ttkcombobox(fr.Month, values = MOIS, textvariable = endMonth, width = largeur5)

        tkgrid(txt.startMonth, row = 0, column = 0, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
        tkgrid(cb.startMonth, row = 0, column = 1, sticky = 'we', rowspan = 1, columnspan = 3, padx = 1, pady = 1, ipadx = 1, ipady = 1)
        tkgrid(txt.endMonth, row = 0, column = 4, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
        tkgrid(cb.endMonth, row = 0, column = 5, sticky = 'we', rowspan = 1, columnspan = 3, padx = 1, pady = 1, ipadx = 1, ipady = 1)

        tkgrid(bt.daterange, row = 0, column = 0, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
        tkgrid(txt.month, row = 1, column = 0, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
        tkgrid(fr.Month, row = 2, column = 0, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)

        helpWidget(bt.daterange, lang.dlg[['tooltip']][['6']], lang.dlg[['status']][['6']])

        #################

        tkconfigure(bt.daterange, command = function(){
            intstep <- periodVAL[CbperiodVAL %in% trimws(tclvalue(timeSteps))]
            GeneralParameters[["date.range"]] <<- getInfoDateRange(.cdtEnv$tcl$main$win,
                                                                    GeneralParameters[["date.range"]],
                                                                    intstep)
        })

        #############################

        frameOutFormat <- ttklabelframe(subfr3, text = lang.dlg[['label']][['18']], relief = 'groove')

        CbOutFileFormat <- c('CDT Stations Data Format', 'CPT File Format',
                             'NetCDF Files', 'Time|Lat|Lon|Value Format')
        OutFileFormatV <- c('cdt', 'cpt', 'ncdf', 'tyxz')

        OutFileFormat <- if(GeneralParameters$type.extract %in% c("point", "mpoint")) CbOutFileFormat[1:2] else CbOutFileFormat
        out.format <- tclVar()
        tclvalue(out.format) <- CbOutFileFormat[OutFileFormatV %in% GeneralParameters$out.data$format]

        spatAverage <- tclVar(GeneralParameters$out.data$sp.avrg)
        stateSpAv <- if(GeneralParameters$out.data$format == "cpt" &
                        GeneralParameters$type.extract != "mpoly") 'normal' else 'disabled'

        #################

        cb.outFormat <- ttkcombobox(frameOutFormat, values = OutFileFormat, textvariable = out.format, width = largeur1)
        fr.SpAvrg <- tkframe(frameOutFormat)

        txt.SpAvrg <- tklabel(fr.SpAvrg, text = lang.dlg[['checkbutton']][['1']], anchor = 'e', justify = 'right')
        chk.SpAvrg <- tkcheckbutton(fr.SpAvrg, variable = spatAverage, state = stateSpAv)

        #################

        tkgrid(txt.SpAvrg, row = 0, column = 0, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 3, ipadx = 1, ipady = 1)
        tkgrid(chk.SpAvrg, row = 0, column = 1, sticky = 'w', rowspan = 1, columnspan = 1, padx = 1, pady = 3, ipadx = 1, ipady = 1)

        tkgrid(cb.outFormat, row = 0, column = 0, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
        tkgrid(fr.SpAvrg, row = 1, column = 0, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)

        helpWidget(cb.outFormat, lang.dlg[['tooltip']][['17']], lang.dlg[['status']][['17']])

        #################

        tkbind(cb.outFormat, "<<ComboboxSelected>>", function(){
            outfrmt <- trimws(tclvalue(out.format))

            if(.cdtData$EnvData$type.extract %in% c('point', 'mpoint'))
            {
                txtfileORdir <- lang.dlg[['label']][['19']]
                colfileORdir <- 'lightblue'
                isFile <- TRUE

                spatAverg <- "0"
                stateSpAv <- 'disabled'
            }else{
                if(outfrmt == CbOutFileFormat[3]){
                    txtfileORdir <- lang.dlg[['label']][['20']]
                    colfileORdir <- 'lightgreen'
                    isFile <- FALSE
                }else if(outfrmt == CbOutFileFormat[4] &
                         .cdtData$EnvData$type.extract == 'mpoly')
                {
                    txtfileORdir <- lang.dlg[['label']][['20']]
                    colfileORdir <- 'lightgreen'
                    isFile <- FALSE
                }else{
                    txtfileORdir <- lang.dlg[['label']][['19']]
                    colfileORdir <- 'lightblue'
                    isFile <- TRUE
                }

                if(outfrmt %in% CbOutFileFormat[3:4]){
                    spatAverg <- "0"
                }else if(outfrmt == CbOutFileFormat[1]){
                    spatAverg <- "1"
                }else{
                    spatAverg <- if(.cdtData$EnvData$type.extract == 'mpoly') "1" else "0"
                }

                stateSpAv <- if(outfrmt == CbOutFileFormat[2] &
                                .cdtData$EnvData$type.extract != 'mpoly') 'normal' else 'disabled'
            }

            tkconfigure(chk.SpAvrg, state = stateSpAv)
            tclvalue(spatAverage) <- spatAverg

            tkconfigure(txt.saveData, bg = colfileORdir)
            tclvalue(fileORdir) <- txtfileORdir
            tclvalue(outputFD) <- isFile
            tkconfigure(bt.saveData, command = function() fileORdir2Save(file2save, isFile = isFile))
        })

        #############################

        frameSave <- tkframe(subfr3, relief = 'groove', borderwidth = 2)

        if(GeneralParameters$out.data$sp.avrg)
        {
            txtfileORdir <- lang.dlg[['label']][['19']]
            colfileORdir <- 'lightblue'
            isFile <- TRUE
        }else{
            if(GeneralParameters$type.extract %in% c("rect", "poly", "mpoly"))
            {
                if(GeneralParameters$out.data$format == "ncdf" |
                  (GeneralParameters$type.extract == "mpoly" &
                   GeneralParameters$out.data$format == "tyxz"))
                {
                    txtfileORdir <- lang.dlg[['label']][['20']]
                    colfileORdir <- 'lightgreen'
                    isFile <- FALSE
                }else{
                    txtfileORdir <- lang.dlg[['label']][['19']]
                    colfileORdir <- 'lightblue'
                    isFile <- TRUE
                }
            }else{
                txtfileORdir <- lang.dlg[['label']][['19']]
                colfileORdir <- 'lightblue'
                isFile <- TRUE
            }
        }

        outputFD <- tclVar(isFile)
        fileORdir <- tclVar(txtfileORdir)
        file2save <- tclVar(GeneralParameters$out.data$outdir)

        txt.saveData <- tklabel(frameSave, text = tclvalue(fileORdir), textvariable = fileORdir, anchor = 'w', justify = 'left', bg = colfileORdir)
        en.saveData <- tkentry(frameSave, textvariable = file2save, width = largeur2)
        bt.saveData <- tkbutton(frameSave, text = "...")

        tkgrid(txt.saveData, row = 0, column = 0, sticky = 'we', rowspan = 1, columnspan = 5, padx = 1, pady = 0, ipadx = 1, ipady = 1)
        tkgrid(en.saveData, row = 1, column = 0, sticky = 'we', rowspan = 1, columnspan = 4, padx = 0, pady = 0, ipadx = 1, ipady = 1)
        tkgrid(bt.saveData, row = 1, column = 4, sticky = 'we', rowspan = 1, columnspan = 1, padx = 0, pady = 0, ipadx = 1, ipady = 1)

        helpWidget(en.saveData, lang.dlg[['tooltip']][['18']], lang.dlg[['status']][['18']])
        helpWidget(bt.saveData, lang.dlg[['tooltip']][['5']], lang.dlg[['status']][['5']])

        tkconfigure(bt.saveData, command = function() fileORdir2Save(file2save, isFile = isFile))

        #############################

        bt.Extract.Data <- ttkbutton(subfr3, text = lang.dlg[['button']][['6']])

        tkconfigure(bt.Extract.Data, command = function(){
            GeneralParameters$intstep <- periodVAL[CbperiodVAL %in% trimws(tclvalue(timeSteps))]
            GeneralParameters$minhour <- as.numeric(trimws(tclvalue(minhour.tclVar)))

            if(GeneralParameters$intstep == 'seasonal'){
                GeneralParameters$season.start <- which(MOIS %in% trimws(tclvalue(seasStart.tclVar)))
                GeneralParameters$season.len <- as.numeric(trimws(tclvalue(seasLen.tclVar)))
            }

            GeneralParameters$data.type <- datatypeVAL[CbdatatypeVAL %in% trimws(tclvalue(DataType))]

            if(GeneralParameters$data.type == "") return(NULL)

            if(GeneralParameters$data.type == 'cdtnetcdf'){
                GeneralParameters$cdtnetcdf$dir <- trimws(tclvalue(input.file))
                if(is.null(settingINData)){
                    Insert.Messages.Out(lang.dlg[['message']][['5']], TRUE, "w")
                    return(NULL)
                }
            }else GeneralParameters$cdtdataset$index <- trimws(tclvalue(input.file))

            GeneralParameters$shp.file$shp <- trimws(tclvalue(shpFile))
            GeneralParameters$shp.file$attr <- trimws(tclvalue(shpAttr))

            GeneralParameters$type.extract <- typeEXTRACTV[typeEXTRACT %in% trimws(tclvalue(type.extract))]

            GeneralParameters$Geom$minlon <- as.numeric(trimws(tclvalue(.cdtData$EnvData$minlonRect)))
            GeneralParameters$Geom$maxlon <- as.numeric(trimws(tclvalue(.cdtData$EnvData$maxlonRect)))
            GeneralParameters$Geom$minlat <- as.numeric(trimws(tclvalue(.cdtData$EnvData$minlatRect)))
            GeneralParameters$Geom$maxlat <- as.numeric(trimws(tclvalue(.cdtData$EnvData$maxlatRect)))
            GeneralParameters$Geom$padlon <- as.numeric(trimws(tclvalue(padLon)))
            GeneralParameters$Geom$padlat <- as.numeric(trimws(tclvalue(padLat)))
            GeneralParameters$Geom$namePoly <- trimws(tclvalue(.cdtData$EnvData$namePoly))

            if(GeneralParameters$type.extract %in% c('mpoint', 'mpoly')){
                multiptspoly <- gsub("[\r]", "", .cdtData$EnvData$multiptspoly)
                multiptspoly <- trimws(strsplit(multiptspoly, "[\n]")[[1]])
                multiptspoly <- multiptspoly[multiptspoly != ""]

                if(GeneralParameters$type.extract == 'mpoint'){
                    multiptspoly <- t(sapply(multiptspoly, function(x) strsplit(x, " ")[[1]]))
                    multiptspoly <- data.frame(multiptspoly, stringsAsFactors = FALSE)
                    rownames(multiptspoly) <- NULL
                    names(multiptspoly) <- c('id', 'x', 'y')
                    multiptspoly$x <- as.numeric(multiptspoly$x)
                    multiptspoly$y <- as.numeric(multiptspoly$y)
                }
            
                GeneralParameters$Geom$multiObj <- multiptspoly
            }

            GeneralParameters$months$start <- which(MOIS %in% trimws(tclvalue(startMonth)))
            GeneralParameters$months$end <- which(MOIS %in% trimws(tclvalue(endMonth)))

            GeneralParameters$out.data$format <- OutFileFormatV[CbOutFileFormat %in% trimws(tclvalue(out.format))]
            GeneralParameters$out.data$sp.avrg <- switch(tclvalue(spatAverage), '0' = FALSE, '1' = TRUE)
            GeneralParameters$out.data$outdir <- trimws(tclvalue(file2save))
            GeneralParameters$out.data$isFile <- switch(tclvalue(outputFD), '0' = FALSE, '1' = TRUE)

            # assign('GeneralParameters', GeneralParameters, envir = .GlobalEnv)

            Insert.Messages.Out(lang.dlg[['message']][['6']], TRUE, "i")

            tkconfigure(.cdtEnv$tcl$main$win, cursor = 'watch')
            tcl('update')
            ret <- tryCatch(
                {
                    ExtractDataProcs(GeneralParameters)
                },
                warning = function(w){
                    warningFun(w)
                    return(0)
                },
                error = function(e) errorFun(e),
                finally = {
                    tkconfigure(.cdtEnv$tcl$main$win, cursor = '')
                    tcl('update')
                }
            )

            if(!is.null(ret)){
                if(ret == 0)
                    Insert.Messages.Out(lang.dlg[['message']][['7']], TRUE, "s")
                else if(ret == -1)
                    Insert.Messages.Out(lang.dlg[['message']][['9']], TRUE, "w")
                else
                    Insert.Messages.Out(lang.dlg[['message']][['8']], TRUE, "e")
            }else Insert.Messages.Out(lang.dlg[['message']][['8']], TRUE, "e")
        })

        #############################
        tkgrid(frameDate, row = 0, column = 0, sticky = '', padx = 1, pady = 1, ipadx = 1, ipady = 1)
        tkgrid(frameOutFormat, row = 1, column = 0, sticky = '', padx = 1, pady = 3, ipadx = 1, ipady = 1)
        tkgrid(frameSave, row = 2, column = 0, sticky = 'we', padx = 1, pady = 3, ipadx = 1, ipady = 1)
        tkgrid(bt.Extract.Data, row = 3, column = 0, sticky = 'we', padx = 1, pady = 3, ipadx = 1, ipady = 1)

    #######################################################################################################

    tkgrid(tknote.cmd, sticky = 'nwes')
    tkgrid.columnconfigure(tknote.cmd, 0, weight = 1)
    tkgrid.rowconfigure(tknote.cmd, 0, weight = 1)

    tcl('update')
    tkgrid(.cdtEnv$tcl$main$cmd.frame, sticky = 'nwes', pady = 1)
    tkgrid.columnconfigure(.cdtEnv$tcl$main$cmd.frame, 0, weight = 1)
    tkgrid.rowconfigure(.cdtEnv$tcl$main$cmd.frame, 0, weight = 1)

    invisible()
}
