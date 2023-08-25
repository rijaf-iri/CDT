
StnChkCoordsPanelCmd <- function(){
    listOpenFiles <- openFile_ttkcomboList()
    if(WindowsOS()){
        largeur0 <- 21
        largeur1 <- 34
        largeur2 <- 36
        largeur3 <- 32
        largeur4 <- 39
    }else{
        largeur0 <- 21
        largeur1 <- 33
        largeur2 <- 34
        largeur3 <- 37
        largeur4 <- 39
    }

    GeneralParameters <- list(data.type = 'cdtcoords', infile = "", shpfile = "", output = "", buffer = 1)

    .cdtData$EnvData$MapOp <- list(blue = list(col = "blue", pch = 20, cex = 0.8),
                                   orange = list(col = "orange", pch = 20, cex = 0.8),
                                   red = list(col = "red", pch = 20, cex = 0.8),
                                   shp = list(col = "black", lwd = 1.5)
                                )

    xml.dlg <- file.path(.cdtDir$dirLocal, "languages", "cdtStnCoords_leftCmd.xml")
    lang.dlg <- cdtLanguageParse(xml.dlg, .cdtData$Config$lang.iso)
    .cdtData$EnvData$message <- lang.dlg[['message']]

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

    #Tab1
    subfr1 <- bwTabScrollableFrame(cmd.tab1)

        ############################################

        frameDataType <- tkframe(subfr1, relief = 'groove', borderwidth = 2)

        CbdatatypeVAL <- lang.dlg[['combobox']][['1']]
        datatypeVAL <- c('cdtcoords', 'cdtstation')
        DataType <- tclVar()
        tclvalue(DataType) <- CbdatatypeVAL[datatypeVAL %in% GeneralParameters$data.type]

        txt.datatype <- tklabel(frameDataType, text = lang.dlg[['label']][['1']], anchor = 'w', justify = 'left')
        cb.datatype <- ttkcombobox(frameDataType, values = CbdatatypeVAL, textvariable = DataType, width = largeur0)

        tkgrid(txt.datatype, row = 0, column = 0, sticky = 'we', rowspan = 1, columnspan = 2, padx = 1, pady = 1, ipadx = 1, ipady = 1)
        tkgrid(cb.datatype, row = 0, column = 2, sticky = 'we', rowspan = 1, columnspan = 8, padx = 1, pady = 1, ipadx = 1, ipady = 1)

        tkbind(cb.datatype, "<<ComboboxSelected>>", function(){
            if(trimws(tclvalue(DataType)) == CbdatatypeVAL[1])
                tclvalue(txt.INData.var) <- lang.dlg[['label']][['2']]
            if(trimws(tclvalue(DataType)) == CbdatatypeVAL[2])
                tclvalue(txt.INData.var) <- lang.dlg[['label']][['3']]
        })

        #######################

        frameInData <- tkframe(subfr1, relief = 'groove', borderwidth = 2)

        input.file <- tclVar(GeneralParameters$infile)
        shp.file <- tclVar(GeneralParameters$shpfile)

        if(GeneralParameters$data.type == 'cdtcoords')
            txt.INData <- lang.dlg[['label']][['2']]
        if(GeneralParameters$data.type == 'cdtstation')
            txt.INData <- lang.dlg[['label']][['3']]
        txt.INData.var <- tclVar(txt.INData)

        txt.infile <- tklabel(frameInData, text = tclvalue(txt.INData.var), textvariable = txt.INData.var, anchor = 'w', justify = 'left')
        cb.infile <- ttkcombobox(frameInData, values = unlist(listOpenFiles), textvariable = input.file, width = largeur1)
        bt.infile <- tkbutton(frameInData, text = "...")
        txt.blkshp <- tklabel(frameInData, text = lang.dlg[['label']][['4']], anchor = 'w', justify = 'left')
        cb.blkshp <- ttkcombobox(frameInData, values = unlist(listOpenFiles), textvariable = shp.file, width = largeur1)
        bt.blkshp <- tkbutton(frameInData, text = "...")

        tkgrid(txt.infile, row = 0, column = 0, sticky = 'we', rowspan = 1, columnspan = 8, padx = 1, pady = 1, ipadx = 1, ipady = 1)
        tkgrid(cb.infile, row = 1, column = 0, sticky = 'we', rowspan = 1, columnspan = 9, padx = 0, pady = 1, ipadx = 1, ipady = 1)
        tkgrid(bt.infile, row = 1, column = 9, sticky = 'w', rowspan = 1, columnspan = 1, padx = 0, pady = 1, ipadx = 1, ipady = 1)
        tkgrid(txt.blkshp, row = 2, column = 0, sticky = 'we', rowspan = 1, columnspan = 8, padx = 1, pady = 1, ipadx = 1, ipady = 1)
        tkgrid(cb.blkshp, row = 3, column = 0, sticky = 'we', rowspan = 1, columnspan = 9, padx = 0, pady = 1, ipadx = 1, ipady = 1)
        tkgrid(bt.blkshp, row = 3, column = 9, sticky = 'w', rowspan = 1, columnspan = 1, padx = 0, pady = 1, ipadx = 1, ipady = 1)

        ####
        tkconfigure(bt.infile, command = function(){
            dat.opfiles <- getOpenFiles(.cdtEnv$tcl$main$win)
            if(!is.null(dat.opfiles)){
                update.OpenFiles('ascii', dat.opfiles)
                listOpenFiles[[length(listOpenFiles) + 1]] <<- dat.opfiles[[1]]
                tclvalue(input.file) <- dat.opfiles[[1]]
                tkconfigure(cb.infile, values = unlist(listOpenFiles))
            }
        })

        tkconfigure(bt.blkshp, command = function(){
            shp.opfiles <- getOpenShp(.cdtEnv$tcl$main$win)
            if(!is.null(shp.opfiles)){
                update.OpenFiles('shp', shp.opfiles)
                tclvalue(shp.file) <- shp.opfiles[[1]]
                listOpenFiles[[length(listOpenFiles) + 1]] <<- shp.opfiles[[1]]
                tkconfigure(cb.blkshp, values = unlist(listOpenFiles))
            }
        })

        #######################

        frameBuff <- tkframe(subfr1)

        buffer <- tclVar(GeneralParameters$buffer)

        txt.buffer <- tklabel(frameBuff, text = lang.dlg[['label']][['5']])
        en.buffer <- tkentry(frameBuff, textvariable = buffer, width = 6)

        tkgrid(txt.buffer, row = 0, column = 0, sticky = 'we', padx = 1, pady = 1, ipadx = 1, ipady = 1)
        tkgrid(en.buffer, row = 0, column = 1, sticky = 'we', padx = 1, pady = 1, ipadx = 1, ipady = 1)

        helpWidget(frameBuff, lang.dlg[['tooltip']][['1']], lang.dlg[['status']][['1']])

        #######################

        frameDirSav <- tkframe(subfr1, relief = 'groove', borderwidth = 2)

        outdir <- tclVar(GeneralParameters$output)

        txt.outdir <- tklabel(frameDirSav, text = lang.dlg[['label']][['6']], anchor = 'w', justify = 'left')
        en.outdir <- tkentry(frameDirSav, textvariable = outdir, width = largeur2)
        bt.outdir <- tkbutton(frameDirSav, text = "...")

        tkgrid(txt.outdir, row = 0, column = 0, sticky = 'we', rowspan = 1, columnspan = 5, padx = 1, pady = 0, ipadx = 1, ipady = 1)
        tkgrid(en.outdir, row = 1, column = 0, sticky = 'we', rowspan = 1, columnspan = 4, padx = 0, pady = 0, ipadx = 1, ipady = 1)
        tkgrid(bt.outdir, row = 1, column = 4, sticky = 'w', rowspan = 1, columnspan = 1, padx = 0, pady = 0, ipadx = 1, ipady = 1)

        ####
        tkconfigure(bt.outdir, command = function(){
            dirCdrs <- tk_choose.dir(getwd(), "")
            tclvalue(outdir) <- if(dirCdrs %in% c("", "NA") | is.na(dirCdrs)) "" else dirCdrs
        })

        #######################

        bt.checkCoords <- ttkbutton(subfr1, text = lang.dlg[['button']][['1']])

        tkconfigure(bt.checkCoords, command = function(){
            GeneralParameters$data.type <- datatypeVAL[CbdatatypeVAL %in% trimws(tclvalue(DataType))]
            GeneralParameters$infile <- trimws(tclvalue(input.file))
            GeneralParameters$shpfile <- trimws(tclvalue(shp.file))
            GeneralParameters$output <- trimws(tclvalue(outdir))
            GeneralParameters$buffer <- as.numeric(trimws(tclvalue(buffer)))

            # assign('GeneralParameters', GeneralParameters, envir = .GlobalEnv)

            Insert.Messages.Out(lang.dlg[['message']][['1']], TRUE, "i")

            tkconfigure(.cdtEnv$tcl$main$win, cursor = 'watch')
            tcl('update')
            ret <- tryCatch(
                {
                    StnChkCoordsProcs(GeneralParameters)
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

            msg0 <- lang.dlg[['message']][['2']]
            msg1 <- lang.dlg[['message']][['3']]

            .cdtData$EnvData$okCorrect <- FALSE
            if(!is.null(ret)){
                if(ret == 0){
                    Insert.Messages.Out(msg0, TRUE, "s")
                    tkconfigure(chk.crdsdisp, state = "disabled")
                    .cdtData$EnvData$okCorrect <- TRUE

                    .cdtData$EnvData$plot.maps$data.type <- "cdtstation"
                    coords <- .cdtData$EnvData$output$coords[, c('lon', 'lat', 'id')]
                    .cdtData$EnvData$plot.maps[c('lon', 'lat', 'id')] <- coords
                    .cdtData$EnvData$plot.maps$id <- as.character(.cdtData$EnvData$plot.maps$id)
                }else Insert.Messages.Out(msg1, TRUE, "e")
            }else Insert.Messages.Out(msg1, TRUE, "e")
        })

        ############################################

        tkgrid(frameDataType, row = 0, column = 0, sticky = '', padx = 1, pady = 1, ipadx = 1, ipady = 1)
        tkgrid(frameInData, row = 1, column = 0, sticky = 'we', padx = 1, pady = 3, ipadx = 1, ipady = 1)
        tkgrid(frameBuff, row = 2, column = 0, sticky = 'we', padx = 1, pady = 3, ipadx = 1, ipady = 1)
        tkgrid(frameDirSav, row = 3, column = 0, sticky = 'we', padx = 1, pady = 1, ipadx = 1, ipady = 1)
        tkgrid(bt.checkCoords, row = 4, column = 0, sticky = 'we', padx = 1, pady = 3, ipadx = 1, ipady = 1)

    #######################################################################################################

    #Tab2
    subfr2 <- bwTabScrollableFrame(cmd.tab2)

        ##############################################

        frameCrdData <- ttklabelframe(subfr2, text = lang.dlg[['label']][['7']], relief = 'groove')

        .cdtData$EnvData$DirExist <- tclVar(0)
        file.Index <- tclVar()

        stateIndexDat <- if(tclvalue(.cdtData$EnvData$DirExist) == "1") "normal" else "disabled"

        chk.IdxDat <- tkcheckbutton(frameCrdData, variable = .cdtData$EnvData$DirExist, text = lang.dlg[['checkbutton']][['1']], anchor = 'w', justify = 'left')
        en.IdxDat <- tkentry(frameCrdData, textvariable = file.Index, width = largeur4, state = stateIndexDat)
        bt.IdxDat <- ttkbutton(frameCrdData, text = .cdtEnv$tcl$lang$global[['button']][['6']], state = stateIndexDat)

        tkgrid(chk.IdxDat, row = 0, column = 0, sticky = 'we', rowspan = 1, columnspan = 4, padx = 1, pady = 1, ipadx = 1, ipady = 1)
        tkgrid(bt.IdxDat, row = 0, column = 4, sticky = 'e', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
        tkgrid(en.IdxDat, row = 1, column = 0, sticky = 'we', rowspan = 1, columnspan = 5, padx = 1, pady = 1, ipadx = 1, ipady = 1)

        ###############

        tkconfigure(bt.IdxDat, command = function(){
            path.Idx <- tclvalue(tkgetOpenFile(initialdir = getwd(), filetypes = .cdtEnv$tcl$data$filetypes6))
            if(path.Idx %in% c("", "NA") | is.na(path.Idx)) return(NULL)
            tclvalue(file.Index) <- path.Idx

            if(file.exists(trimws(tclvalue(file.Index)))){
                OutChkCrds <- try(readRDS(trimws(tclvalue(file.Index))), silent = TRUE)
                if(inherits(OutChkCrds, "try-error")){
                    Insert.Messages.Out(gsub('[\r\n]', '', OutChkCrds[1]), format = TRUE)
                    Insert.Messages.Out(lang.dlg[['message']][['7']], TRUE, "e")

                    .cdtData$EnvData$okCorrect <- FALSE
                    .cdtData$EnvData$output <- NULL
                    .cdtData$EnvData$Table.Disp <- NULL
                    .cdtData$EnvData$Maps.Disp <- NULL
                    return(NULL)
                }

                .cdtData$EnvData$okCorrect <- TRUE
                .cdtData$EnvData$output <- OutChkCrds
                .cdtData$EnvData$PathData <- dirname(trimws(tclvalue(file.Index)))

                dataOUT <- file.path(.cdtData$EnvData$PathData, 'CDTDATASET')
                file.table.rds <- file.path(dataOUT, 'Table.rds')
                file.display <- file.path(dataOUT, 'Display.rds')
                .cdtData$EnvData$Table.Disp <- readRDS(file.table.rds)
                .cdtData$EnvData$Maps.Disp <- readRDS(file.display)

                .cdtData$EnvData$plot.maps$data.type <- "cdtstation"
                coords <- .cdtData$EnvData$output$coords[, c('lon', 'lat', 'id')]
                .cdtData$EnvData$plot.maps[c('lon', 'lat', 'id')] <- coords
                .cdtData$EnvData$plot.maps$id <- as.character(.cdtData$EnvData$plot.maps$id)
            }
        })

        ###############

        tkbind(chk.IdxDat, "<Button-1>", function(){
            stateIndexDat <- if(tclvalue(.cdtData$EnvData$DirExist) == '1') 'disabled' else 'normal'
            tkconfigure(en.IdxDat, state = stateIndexDat)
            tkconfigure(bt.IdxDat, state = stateIndexDat)

            stateChkCrds <- 'disabled'
            if(tclvalue(.cdtData$EnvData$DirExist) == '1')
                stateChkCrds <- if(tclvalue(.cdtData$EnvData$DispCrd) == '0') 'normal' else 'disabled'
            tkconfigure(bt.checkCoords, state = stateChkCrds)
            tkconfigure(en.buffer, state = stateChkCrds)
            tkconfigure(en.outdir, state = stateChkCrds)
            tkconfigure(bt.outdir, state = stateChkCrds)

            if(tclvalue(.cdtData$EnvData$DispCrd) == '0'){
                stateChkCrds1 <- if(tclvalue(.cdtData$EnvData$DirExist) == '0') 'disabled' else 'normal'
            }else{
                stateChkCrds1 <- if(tclvalue(.cdtData$EnvData$DirExist) == '0') 'disabled' else 'normal'
            }
            tkconfigure(cb.datatype, state = stateChkCrds1)
            tkconfigure(cb.infile, state = stateChkCrds1)
            tkconfigure(bt.infile, state = stateChkCrds1)

            stateEdit <- 'normal'
            if(tclvalue(.cdtData$EnvData$DirExist) == '1')
                stateEdit <- if(tclvalue(.cdtData$EnvData$DispCrd) == '0') 'normal' else 'disabled'
            tkconfigure(bt.editCoords, state = stateEdit)
            tkconfigure(bt.correctCoords, state = stateEdit)

            stateCrdDisp <- if(tclvalue(.cdtData$EnvData$DirExist) == '1') 'normal' else 'disabled'
            tkconfigure(chk.crdsdisp, state = stateCrdDisp)
        })

        ##############################################

        bt.editCoords <- ttkbutton(subfr2, text = lang.dlg[['button']][['2']])

        .cdtData$EnvData$okCorrect <- FALSE
        .cdtData$EnvData$tab$Table <- NULL
        .cdtData$EnvData$Table.Disp0 <- NULL

        tkconfigure(bt.editCoords, command = function(){
            if(.cdtData$EnvData$okCorrect){
                if(!is.null(.cdtData$EnvData$Table.Disp)){
                    .cdtData$EnvData$Table.Disp0 <- .cdtData$EnvData$Table.Disp
                    editstn.df <- .cdtData$EnvData$Table.Disp
                    editstn.df[is.na(editstn.df)] <- ""
                    .cdtData$EnvData$tab$Table <- tableNotebookTab_unik(editstn.df, .cdtData$EnvData$tab$Table,
                                                                        "Edit-Coordinates", 10, 'chkcrds')
                    menuRowHandleCopyPaste.OpenTable()
                }else{
                    Insert.Messages.Out(lang.dlg[['message']][['4']], TRUE, "s")
                    return(NULL)
                }
            }
        })

        ##############################################

        bt.correctCoords <- ttkbutton(subfr2, text = lang.dlg[['button']][['3']])

        tkconfigure(bt.correctCoords, command = function(){
            if(.cdtData$EnvData$okCorrect){
                ret <- try(StnChkCoordsCorrect(), silent = TRUE)
                if(is.null(ret)) return(NULL)
                if(inherits(ret, "try-error")){
                    Insert.Messages.Out(lang.dlg[['message']][['6']], TRUE, "e")
                    Insert.Messages.Out(gsub('[\r\n]', '', ret[1]), format = TRUE)
                }else{
                    Insert.Messages.Out(lang.dlg[['message']][['5']], TRUE, "s")
                }
            }
        })

        ##############################################

        frameDisplay <- tkframe(subfr2, relief = 'groove', borderwidth = 2)

        .cdtData$EnvData$DispCrd <- tclVar(0)

        bt.dispCDT <- ttkbutton(frameDisplay, text = lang.dlg[['button']][['4']])
        bt.dispCDTOpt <- ttkbutton(frameDisplay, text = .cdtEnv$tcl$lang$global[['button']][['4']])
        bt.dispGoogle <- ttkbutton(frameDisplay, text = lang.dlg[['button']][['5']])
        chk.crdsdisp <- tkcheckbutton(frameDisplay, variable = .cdtData$EnvData$DispCrd, text = lang.dlg[['checkbutton']][['2']], anchor = 'w', justify = 'left', width = largeur3)

        tkgrid(bt.dispCDT, row = 0, column = 0, sticky = 'we', rowspan = 1, columnspan = 1, padx = 0, pady = 1, ipadx = 1, ipady = 1)
        tkgrid(bt.dispCDTOpt, row = 0, column = 1, sticky = 'we', rowspan = 1, columnspan = 1, padx = 0, pady = 3, ipadx = 1, ipady = 1)
        tkgrid(bt.dispGoogle, row = 1, column = 0, sticky = 'we', rowspan = 1, columnspan = 2, padx = 1, pady = 3, ipadx = 1, ipady = 1)
        tkgrid(chk.crdsdisp, row = 2, column = 0, sticky = 'we', rowspan = 1, columnspan = 2, padx = 1, pady = 1, ipadx = 1, ipady = 1)

        ###########################

        tkconfigure(bt.dispCDTOpt, command = function(){
            .cdtData$EnvData$MapOp <- MapGraph.ChkCoordsOptions(.cdtData$EnvData$MapOp)
        })

        ###############

        .cdtData$EnvData$tab$MapSelect <- NULL

        tkconfigure(bt.dispCDT, command = function(){
            if(tclvalue(.cdtData$EnvData$DispCrd) == '1' &
                trimws(tclvalue(input.file)) != "")
            {
                GeneralParameters$data.type <- datatypeVAL[CbdatatypeVAL %in% trimws(tclvalue(DataType))]
                GeneralParameters$infile <- trimws(tclvalue(input.file))

                tkconfigure(.cdtEnv$tcl$main$win, cursor = 'watch')
                tcl('update')
                ret <- tryCatch(
                    {
                        StnChkCoordsDataStn(GeneralParameters)
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

                ok.crds <- FALSE
                if(!is.null(ret))
                    if(ret == 0){
                        ok.crds <- TRUE
                        .cdtData$EnvData$plot.maps$data.type <- "cdtstation"
                        coords <- .cdtData$EnvData$output$coords[, c('lon', 'lat', 'id')]
                        .cdtData$EnvData$plot.maps[c('lon', 'lat', 'id')] <- coords
                        .cdtData$EnvData$plot.maps$id <- as.character(.cdtData$EnvData$plot.maps$id)
                    }
                if(!ok.crds){
                    .cdtData$EnvData$Maps.Disp <- NULL
                    return(NULL)
                }
            }

            if(trimws(tclvalue(shp.file)) != "")
            {
                shpofile <- getShpOpenData(trimws(tclvalue(shp.file)))
                if(is.null(shpofile))
                    .cdtData$EnvData$shp$ocrds <- NULL
                else
                    .cdtData$EnvData$shp$ocrds <- getBoundaries(shpofile[[2]])
            }else .cdtData$EnvData$shp$ocrds <- NULL

            if(!is.null(.cdtData$EnvData$Maps.Disp))
            {
                set.initialize.Zoom()

                imgContainer <- CDT.Display.Points.Zoom(StnChkCoordsPlotMap, .cdtData$EnvData$tab$MapSelect, 'Coordinates-Map')
                .cdtData$EnvData$tab$MapSelect <- imageNotebookTab_unik(imgContainer, .cdtData$EnvData$tab$MapSelect)
            }
        })

        ###############

        tkconfigure(bt.dispGoogle, command = function(){
            if(!testConnection())
            {
                Insert.Messages.Out(lang.dlg[['message']][['8']], TRUE, "e")
                return(NULL)
            }
            if(tclvalue(.cdtData$EnvData$DispCrd) == '1' &
                trimws(tclvalue(input.file)) != "")
            {
                GeneralParameters$data.type <- datatypeVAL[CbdatatypeVAL %in% trimws(tclvalue(DataType))]
                GeneralParameters$infile <- trimws(tclvalue(input.file))

                tkconfigure(.cdtEnv$tcl$main$win, cursor = 'watch')
                tcl('update')
                ret <- tryCatch(
                    {
                        StnChkCoordsDataStn(GeneralParameters)
                    },
                    warning = function(w) {
                        warningFun(w)
                        return(0)
                    },
                    error = function(e) errorFun(e),
                    finally = {
                        tkconfigure(.cdtEnv$tcl$main$win, cursor = '')
                        tcl('update')
                    }
                )

                ok.crds <- FALSE
                if(!is.null(ret))
                    if(ret == 0) ok.crds <- TRUE
                if(!ok.crds){
                    .cdtData$EnvData$Maps.Disp <- NULL
                    return(NULL)
                }
            }

            if(!is.null(.cdtData$EnvData$Maps.Disp))
            {
                html.page <- StnChkCoordsFormatHtml()
                StnChkCoordsBrowse(html.page)
            }
        })

        ###############

        tkbind(chk.crdsdisp, "<Button-1>", function(){
            if(tclvalue(.cdtData$EnvData$DirExist) == '0'){
                stateEdit <- if(tclvalue(.cdtData$EnvData$DispCrd) == '1') 'normal' else 'disabled'
                tkconfigure(bt.checkCoords, state = stateEdit)
                tkconfigure(bt.editCoords, state = stateEdit)
                tkconfigure(bt.correctCoords, state = stateEdit)
                tkconfigure(en.buffer, state = stateEdit)
                tkconfigure(en.outdir, state = stateEdit)
                tkconfigure(bt.outdir, state = stateEdit)
            }
        })

        ##############################################

        tkgrid(frameCrdData, row = 0, column = 0, sticky = 'we', padx = 1, pady = 1, ipadx = 1, ipady = 1)
        tkgrid(bt.editCoords, row = 1, column = 0, sticky = 'we', padx = 1, pady = 3, ipadx = 1, ipady = 1)
        tkgrid(bt.correctCoords, row = 2, column = 0, sticky = 'we', padx = 1, pady = 3, ipadx = 1, ipady = 1)
        tkgrid(frameDisplay, row = 3, column = 0, sticky = 'we', padx = 0, pady = 1, ipadx = 0, ipady = 1)

    #######################################################################################################

    #Tab3
    subfr3 <- bwTabScrollableFrame(cmd.tab3)

        ##############################################

        .cdtData$EnvData$zoom$xx1 <- tclVar()
        .cdtData$EnvData$zoom$xx2 <- tclVar()
        .cdtData$EnvData$zoom$yy1 <- tclVar()
        .cdtData$EnvData$zoom$yy2 <- tclVar()

        .cdtData$EnvData$zoom$pressButP <- tclVar(0)
        .cdtData$EnvData$zoom$pressButM <- tclVar(0)
        .cdtData$EnvData$zoom$pressButRect <- tclVar(0)
        .cdtData$EnvData$zoom$pressButDrag <- tclVar(0)

        ZoomXYval0 <- NULL

        ##############################################

        frameZoom <- ttklabelframe(subfr3, text = lang.dlg[['label']][['8']], relief = 'groove')

        xentr1.zoom <- tkentry(frameZoom, width = 7, justify = "left", textvariable = .cdtData$EnvData$zoom$xx1)
        xentr2.zoom <- tkentry(frameZoom, width = 7, justify = "left", textvariable = .cdtData$EnvData$zoom$xx2)
        yentr1.zoom <- tkentry(frameZoom, width = 7, justify = "left", textvariable = .cdtData$EnvData$zoom$yy1)
        yentr2.zoom <- tkentry(frameZoom, width = 7, justify = "left", textvariable = .cdtData$EnvData$zoom$yy2)
        bt.centre.zoom <- tklabel(frameZoom, image = .cdtEnv$tcl$zoom$img$centre)

        .cdtData$EnvData$zoom$btZoomP <- tkbutton(frameZoom, image = .cdtEnv$tcl$zoom$img$plus, relief = 'raised', bg = 'lightblue', state = 'normal')
        .cdtData$EnvData$zoom$btZoomM <- tkbutton(frameZoom, image = .cdtEnv$tcl$zoom$img$moins, relief = 'raised', bg = 'lightblue', state = 'normal')
        .cdtData$EnvData$zoom$btZoomRect <- tkbutton(frameZoom, image = .cdtEnv$tcl$zoom$img$rect, relief = 'raised', bg = 'lightblue', state = 'normal')
        .cdtData$EnvData$zoom$btPanImg <- tkbutton(frameZoom, image = .cdtEnv$tcl$zoom$img$pan, relief = 'raised', bg = 'lightblue', state = 'normal')
        .cdtData$EnvData$zoom$btRedraw <- tkbutton(frameZoom, image = .cdtEnv$tcl$zoom$img$redraw, relief = 'raised', bg = 'lightblue')
        .cdtData$EnvData$zoom$btReset <- tkbutton(frameZoom, image = .cdtEnv$tcl$zoom$img$reset, relief = 'raised')

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

        helpWidget(.cdtData$EnvData$zoom$btZoomP, lang.dlg[['tooltip']][['2']], lang.dlg[['status']][['2']])
        helpWidget(.cdtData$EnvData$zoom$btZoomM, lang.dlg[['tooltip']][['3']], lang.dlg[['status']][['3']])
        helpWidget(.cdtData$EnvData$zoom$btZoomRect, lang.dlg[['tooltip']][['4']], lang.dlg[['status']][['4']])
        helpWidget(.cdtData$EnvData$zoom$btPanImg, lang.dlg[['tooltip']][['5']], lang.dlg[['status']][['5']])
        helpWidget(.cdtData$EnvData$zoom$btRedraw, lang.dlg[['tooltip']][['6']], lang.dlg[['status']][['6']])
        helpWidget(.cdtData$EnvData$zoom$btReset, lang.dlg[['tooltip']][['7']], lang.dlg[['status']][['7']])

        #################
        tkconfigure(.cdtData$EnvData$zoom$btRedraw, command = function(){
            .cdtData$EnvData$ZoomXYval <- as.numeric(c(tclvalue(.cdtData$EnvData$zoom$xx1), tclvalue(.cdtData$EnvData$zoom$xx2),
                                                    tclvalue(.cdtData$EnvData$zoom$yy1), tclvalue(.cdtData$EnvData$zoom$yy2)))

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

        initXYval0 <- NA
        initializeButZoom <- function(){
            initXYval0 <<- trimws(c(tclvalue(.cdtData$EnvData$zoom$xx1), tclvalue(.cdtData$EnvData$zoom$xx2),
                                    tclvalue(.cdtData$EnvData$zoom$yy1), tclvalue(.cdtData$EnvData$zoom$yy2)))

            tclvalue(.cdtData$EnvData$zoom$pressButP) <- 0
            tclvalue(.cdtData$EnvData$zoom$pressButM) <- 0
            tclvalue(.cdtData$EnvData$zoom$pressButRect) <- 0
            tclvalue(.cdtData$EnvData$zoom$pressButDrag) <- 0

            tkconfigure(.cdtData$EnvData$zoom$btZoomP, relief = 'raised', bg = 'lightblue', state = 'normal')
            tkconfigure(.cdtData$EnvData$zoom$btZoomM, relief = 'raised', bg = 'lightblue', state = 'normal')
            tkconfigure(.cdtData$EnvData$zoom$btZoomRect, relief = 'raised', bg = 'lightblue', state = 'normal')
            tkconfigure(.cdtData$EnvData$zoom$btPanImg, relief = 'raised', bg = 'lightblue', state = 'normal')
        }

        activateButRedraw <- function(){
            initXYval1 <- trimws(c(tclvalue(.cdtData$EnvData$zoom$xx1), tclvalue(.cdtData$EnvData$zoom$xx2),
                                    tclvalue(.cdtData$EnvData$zoom$yy1), tclvalue(.cdtData$EnvData$zoom$yy2)))
            if(!all(initXYval0 == initXYval1)) tkconfigure(.cdtData$EnvData$zoom$btRedraw, relief = 'raised', bg = 'red')
        }

        #################
        tkbind(xentr1.zoom, "<FocusIn>", initializeButZoom)
        tkbind(xentr1.zoom, "<FocusOut>", activateButRedraw)

        tkbind(xentr2.zoom, "<FocusIn>", initializeButZoom)
        tkbind(xentr2.zoom, "<FocusOut>", activateButRedraw)

        tkbind(yentr1.zoom, "<FocusIn>", initializeButZoom)
        tkbind(yentr1.zoom, "<FocusOut>", activateButRedraw)

        tkbind(yentr2.zoom, "<FocusIn>", initializeButZoom)
        tkbind(yentr2.zoom, "<FocusOut>", activateButRedraw)

        ####
        tkbind(.cdtData$EnvData$zoom$btRedraw, "<Button-1>", function(){
            tclvalue(.cdtData$EnvData$zoom$pressButP) <- 0
            tclvalue(.cdtData$EnvData$zoom$pressButM) <- 0
            tclvalue(.cdtData$EnvData$zoom$pressButRect) <- 0
            tclvalue(.cdtData$EnvData$zoom$pressButDrag) <- 0

            tkconfigure(.cdtData$EnvData$zoom$btRedraw, relief = 'raised', bg = 'lightblue')
            tkconfigure(.cdtData$EnvData$zoom$btZoomP, relief = 'raised', bg = 'lightblue', state = 'normal')
            tkconfigure(.cdtData$EnvData$zoom$btZoomM, relief = 'raised', bg = 'lightblue', state = 'normal')
            tkconfigure(.cdtData$EnvData$zoom$btZoomRect, relief = 'raised', bg = 'lightblue', state = 'normal')
            tkconfigure(.cdtData$EnvData$zoom$btPanImg, relief = 'raised', bg = 'lightblue', state = 'normal')
        })

        tkbind(.cdtData$EnvData$zoom$btReset, "<Button-1>", function(){
            tclvalue(.cdtData$EnvData$zoom$pressButP) <- 0
            tclvalue(.cdtData$EnvData$zoom$pressButM) <- 0
            tclvalue(.cdtData$EnvData$zoom$pressButRect) <- 0
            tclvalue(.cdtData$EnvData$zoom$pressButDrag) <- 0

            tkconfigure(.cdtData$EnvData$zoom$btRedraw, relief = 'raised', bg = 'lightblue')
            tkconfigure(.cdtData$EnvData$zoom$btZoomP, relief = 'raised', bg = 'lightblue', state = 'normal')
            tkconfigure(.cdtData$EnvData$zoom$btZoomM, relief = 'raised', bg = 'lightblue', state = 'normal')
            tkconfigure(.cdtData$EnvData$zoom$btZoomRect, relief = 'raised', bg = 'lightblue', state = 'normal')
            tkconfigure(.cdtData$EnvData$zoom$btPanImg, relief = 'raised', bg = 'lightblue', state = 'normal')
        })

        tkbind(.cdtData$EnvData$zoom$btZoomP, "<Button-1>", function(){
            tclvalue(.cdtData$EnvData$zoom$pressButP) <- 1
            tclvalue(.cdtData$EnvData$zoom$pressButM) <- 0
            tclvalue(.cdtData$EnvData$zoom$pressButRect) <- 0
            tclvalue(.cdtData$EnvData$zoom$pressButDrag) <- 0

            tkconfigure(.cdtData$EnvData$zoom$btRedraw, relief = 'raised', bg = 'lightblue')
            tkconfigure(.cdtData$EnvData$zoom$btZoomP, relief = 'raised', bg = 'red', state = 'disabled')
            tkconfigure(.cdtData$EnvData$zoom$btZoomM, relief = 'raised', bg = 'lightblue', state = 'normal')
            tkconfigure(.cdtData$EnvData$zoom$btZoomRect, relief = 'raised', bg = 'lightblue', state = 'normal')
            tkconfigure(.cdtData$EnvData$zoom$btPanImg, relief = 'raised', bg = 'lightblue', state = 'normal')
        })

        tkbind(.cdtData$EnvData$zoom$btZoomM, "<Button-1>", function(){
            tclvalue(.cdtData$EnvData$zoom$pressButP) <- 0
            tclvalue(.cdtData$EnvData$zoom$pressButM) <- 1
            tclvalue(.cdtData$EnvData$zoom$pressButRect) <- 0
            tclvalue(.cdtData$EnvData$zoom$pressButDrag) <- 0

            tkconfigure(.cdtData$EnvData$zoom$btRedraw, relief = 'raised', bg = 'lightblue')
            tkconfigure(.cdtData$EnvData$zoom$btZoomP, relief = 'raised', bg = 'lightblue', state = 'normal')
            tkconfigure(.cdtData$EnvData$zoom$btZoomM, relief = 'raised', bg = 'red', state = 'disabled')
            tkconfigure(.cdtData$EnvData$zoom$btZoomRect, relief = 'raised', bg = 'lightblue', state = 'normal')
            tkconfigure(.cdtData$EnvData$zoom$btPanImg, relief = 'raised', bg = 'lightblue', state = 'normal')
        })

        tkbind(.cdtData$EnvData$zoom$btZoomRect, "<Button-1>", function(){
            tclvalue(.cdtData$EnvData$zoom$pressButP) <- 0
            tclvalue(.cdtData$EnvData$zoom$pressButM) <- 0
            tclvalue(.cdtData$EnvData$zoom$pressButRect) <- 1
            tclvalue(.cdtData$EnvData$zoom$pressButDrag) <- 0

            tkconfigure(.cdtData$EnvData$zoom$btRedraw, relief = 'raised', bg = 'lightblue')
            tkconfigure(.cdtData$EnvData$zoom$btZoomP, relief = 'raised', bg = 'lightblue', state = 'normal')
            tkconfigure(.cdtData$EnvData$zoom$btZoomM, relief = 'raised', bg = 'lightblue', state = 'normal')
            tkconfigure(.cdtData$EnvData$zoom$btZoomRect, relief = 'raised', bg = 'red', state = 'disabled')
            tkconfigure(.cdtData$EnvData$zoom$btPanImg, relief = 'raised', bg = 'lightblue', state = 'normal')
        })

        tkbind(.cdtData$EnvData$zoom$btPanImg, "<Button-1>", function(){
            tclvalue(.cdtData$EnvData$zoom$pressButP) <- 0
            tclvalue(.cdtData$EnvData$zoom$pressButM) <- 0
            tclvalue(.cdtData$EnvData$zoom$pressButRect) <- 0
            tclvalue(.cdtData$EnvData$zoom$pressButDrag) <- 1

            tkconfigure(.cdtData$EnvData$zoom$btRedraw, relief = 'raised', bg = 'lightblue')
            tkconfigure(.cdtData$EnvData$zoom$btZoomP, relief = 'raised', bg = 'lightblue', state = 'normal')
            tkconfigure(.cdtData$EnvData$zoom$btZoomM, relief = 'raised', bg = 'lightblue', state = 'normal')
            tkconfigure(.cdtData$EnvData$zoom$btZoomRect, relief = 'raised', bg = 'lightblue', state = 'normal')
            tkconfigure(.cdtData$EnvData$zoom$btPanImg, relief = 'raised', bg = 'red', state = 'disabled')
        })

        ##########################################

        tkgrid(frameZoom, row = 0, column = 0, sticky = '')

    #######################################################################################################

    .cdtData$EnvData$StnChkCoords$SaveEdit <- function(dat2sav){
        if(all(is.na(dat2sav)) | is.null(dat2sav)) dat2sav <- NULL
        .cdtData$EnvData$Table.Disp <- dat2sav
        file.table.rds <- file.path(.cdtData$EnvData$PathData, 'CDTDATASET', 'Table.rds')
        saveRDS(dat2sav, file.table.rds)

        if(is.null(dat2sav)) dat2sav <- ""
        file.table.csv <- file.path(.cdtData$EnvData$PathData, 'Stations_to_Check.csv')
        writeFiles(dat2sav, file.table.csv, col.names = TRUE)
    }

    set.initialize.Zoom <- function(){
        donX <- range(.cdtData$EnvData$Maps.Disp$LonX, na.rm = TRUE)
        donY <- range(.cdtData$EnvData$Maps.Disp$LatX, na.rm = TRUE)
        lo1 <- round(donX[1], 4)
        lo2 <- round(donX[2], 4)
        la1 <- round(donY[1], 4)
        la2 <- round(donY[2], 4)
        ZoomXYval0 <<- c(lo1, lo2, la1, la2)

        tclvalue(.cdtData$EnvData$zoom$xx1) <- lo1
        tclvalue(.cdtData$EnvData$zoom$xx2) <- lo2
        tclvalue(.cdtData$EnvData$zoom$yy1) <- la1
        tclvalue(.cdtData$EnvData$zoom$yy2) <- la2

        .cdtData$EnvData$ZoomXYval <- as.numeric(c(tclvalue(.cdtData$EnvData$zoom$xx1),
                                                tclvalue(.cdtData$EnvData$zoom$xx2),
                                                tclvalue(.cdtData$EnvData$zoom$yy1),
                                                tclvalue(.cdtData$EnvData$zoom$yy2)))
    }

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
