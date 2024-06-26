
qcRRZeroCheckPanelCmd <- function(){
    if(WindowsOS()){
        largeur1 <- 31
        largeur2 <- 33
        largeur3 <- 21
        largeur4 <- 11
        largeur5 <- 5
    }else{
        largeur1 <- 32
        largeur2 <- 33
        largeur3 <- 22
        largeur4 <- 11
        largeur5 <- 5
    }

    ###################

    GeneralParameters <- list(infile = "", outdir = "",
                              params = list(min.nbrs = 4, max.nbrs = 20,
                                            min.days = 22, max.dist = 120,
                                            min.thrs = 1.8))

    .cdtData$EnvData$STN$Opts <- list(
                                    stn = list(col = "blue", pch = 23, cex = 1.2, txt.col = 'red', txt.cex = 1.0),
                                    fz = list(col = 'darkred', pch = 20, cex = 1.0, txt.col = 'red', txt.cex = 0.85),
                                    all = list(col = 'darkred', pch = 20, cex = 1.0, txt.col = 'blue', txt.cex = 0.7)
                                )

    ###################

    xml.dlg <- file.path(.cdtDir$dirLocal, "languages", "cdtQC_RR.ZeroCheck_leftCmd.xml")
    lang.dlg <- cdtLanguageParse(xml.dlg, .cdtData$Config$lang.iso)

    ###################

    .cdtEnv$tcl$main$cmd.frame <- tkframe(.cdtEnv$tcl$main$panel.left)

    tknote.cmd <- bwNoteBook(.cdtEnv$tcl$main$cmd.frame)
    cmd.tab1 <- bwAddTab(tknote.cmd, text = lang.dlg[['tab_title']][['1']])
    cmd.tab2 <- bwAddTab(tknote.cmd, text = lang.dlg[['tab_title']][['2']])
    cmd.tab3 <- bwAddTab(tknote.cmd, text = lang.dlg[['tab_title']][['3']])
    cmd.tab4 <- bwAddTab(tknote.cmd, text = lang.dlg[['tab_title']][['4']])

    bwRaiseTab(tknote.cmd, cmd.tab1)

    tkgrid.columnconfigure(cmd.tab1, 0, weight = 1)
    tkgrid.columnconfigure(cmd.tab2, 0, weight = 1)
    tkgrid.columnconfigure(cmd.tab3, 0, weight = 1)
    tkgrid.columnconfigure(cmd.tab4, 0, weight = 1)

    tkgrid.rowconfigure(cmd.tab1, 0, weight = 1)
    tkgrid.rowconfigure(cmd.tab2, 0, weight = 1)
    tkgrid.rowconfigure(cmd.tab3, 0, weight = 1)
    tkgrid.rowconfigure(cmd.tab4, 0, weight = 1)

    #######################################################################################################

    #Tab1
    subfr1 <- bwTabScrollableFrame(cmd.tab1)

        #######################

        frameInData <- ttklabelframe(subfr1, text = lang.dlg[['label']][['1']], relief = 'groove')

        input.file <- tclVar(GeneralParameters$infile)

        txt.infile <- tklabel(frameInData, text = lang.dlg[['label']][['2']], anchor = 'w', justify = 'left')
        cb.infile <- ttkcombobox(frameInData, values = unlist(openFile_ttkcomboList()), textvariable = input.file, width = largeur1)
        bt.infile <- tkbutton(frameInData, text = "...")

        tkgrid(txt.infile, row = 0, column = 0, sticky = 'we', rowspan = 1, columnspan = 5, padx = 1, pady = 1, ipadx = 1, ipady = 1)
        tkgrid(cb.infile, row = 1, column = 0, sticky = 'we', rowspan = 1, columnspan = 4, padx = 0, pady = 1, ipadx = 1, ipady = 1)
        tkgrid(bt.infile, row = 1, column = 4, sticky = 'we', rowspan = 1, columnspan = 1, padx = 0, pady = 1, ipadx = 1, ipady = 1)

        helpWidget(cb.infile, lang.dlg[['tooltip']][['1']], lang.dlg[['status']][['1']])
        helpWidget(bt.infile, lang.dlg[['tooltip']][['2']], lang.dlg[['status']][['2']])

        ########

        tkconfigure(bt.infile, command = function(){
            dat.opfiles <- getOpenFiles(.cdtEnv$tcl$main$win)
            if(!is.null(dat.opfiles)){
                update.OpenFiles('ascii', dat.opfiles)
                tclvalue(input.file) <- dat.opfiles[[1]]
                tkconfigure(cb.infile, values = unlist(openFile_ttkcomboList()))
            }
        })

        tkbind(cb.infile, "<Button-1>", function(){
            tkconfigure(cb.infile, values = unlist(openFile_ttkcomboList()))
        })

        #######################

        bt.params <- ttkbutton(subfr1, text = lang.dlg[['button']][['1']])

        helpWidget(bt.params, lang.dlg[['tooltip']][['3']], lang.dlg[['status']][['3']])

        ########

        tkconfigure(bt.params, command = function(){
            Params <- GeneralParameters[["params"]]
            GeneralParameters[["params"]] <<- getParams.RR.ZerosCheck(Params)
        })

        #######################

        frameDirSav <- tkframe(subfr1, relief = 'groove', borderwidth = 2)

        dir.save <- tclVar(GeneralParameters$outdir)

        txt.dir.save <- tklabel(frameDirSav, text = lang.dlg[['label']][['3']], anchor = 'w', justify = 'left')
        en.dir.save <- tkentry(frameDirSav, textvariable = dir.save, width = largeur2)
        bt.dir.save <- tkbutton(frameDirSav, text = "...")

        ######
        tkgrid(txt.dir.save, row = 0, column = 0, sticky = 'we', rowspan = 1, columnspan = 5, padx = 1, pady = 0, ipadx = 1, ipady = 1)
        tkgrid(en.dir.save, row = 1, column = 0, sticky = 'we', rowspan = 1, columnspan = 4, padx = 0, pady = 0, ipadx = 1, ipady = 1)
        tkgrid(bt.dir.save, row = 1, column = 4, sticky = 'we', rowspan = 1, columnspan = 1, padx = 0, pady = 0, ipadx = 1, ipady = 1)

        helpWidget(en.dir.save, lang.dlg[['tooltip']][['4']], lang.dlg[['status']][['4']])
        helpWidget(bt.dir.save, lang.dlg[['tooltip']][['5']], lang.dlg[['status']][['5']])

        ######
        tkconfigure(bt.dir.save, command = function() fileORdir2Save(dir.save, isFile = FALSE))

        #############################

        bt.checkZeros <- ttkbutton(subfr1, text = lang.dlg[['button']][['2']])

        tkconfigure(bt.checkZeros, command = function(){
            GeneralParameters$infile <- trimws(tclvalue(input.file))
            GeneralParameters$outdir <- trimws(tclvalue(dir.save))

            # assign('GeneralParameters', GeneralParameters, envir = .GlobalEnv)

            Insert.Messages.Out(lang.dlg[['message']][['1']], TRUE, "i")

            if(!dir.exists(GeneralParameters$outdir)){
                dirmsg <- paste(GeneralParameters$outdir, lang.dlg[['message']][['6']])
                Insert.Messages.Out(dirmsg, TRUE, "e")
                return(NULL)
            }

            ###########

            tkconfigure(.cdtEnv$tcl$main$win, cursor = 'watch')
            tcl('update')
            ret <- tryCatch(
                {
                    qcRRZeroCheckProcs(GeneralParameters)
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

            if(!is.null(ret)){
                if(ret == 0){
                    Insert.Messages.Out(msg0, TRUE, "s")

                    if(is.null(.cdtData$EnvData$outzeros)){
                        Insert.Messages.Out(lang.dlg[['message']][['4']], TRUE, "s")
                        return(NULL)
                    }

                    ###############
                    set.station.id()
                    ret <- try(set.date.false.zeros(), silent = TRUE)
                    if(inherits(ret, "try-error") | is.null(ret)) return(NULL)

                    ###############
                    coords <- .cdtData$EnvData$stn.data[c('lon', 'lat', 'id')]
                    .cdtData$EnvData$plot.maps[c('lon', 'lat', 'id')] <- coords

                    xlim <- range(coords$lon, na.rm = TRUE)
                    ylim <- range(coords$lat, na.rm = TRUE)
                    initialize_zoom_frame(xlim, ylim)
                }else Insert.Messages.Out(msg1, TRUE, "e")
            }else Insert.Messages.Out(msg1, TRUE, "e")
        })

        #########################################

        tkgrid(frameInData, row = 0, column = 0, sticky = 'we', padx = 1, pady = 1, ipadx = 1, ipady = 1)
        tkgrid(bt.params, row = 1, column = 0, sticky = 'we', padx = 1, pady = 5, ipadx = 1, ipady = 1)
        tkgrid(frameDirSav, row = 2, column = 0, sticky = 'we', padx = 1, pady = 1, ipadx = 1, ipady = 1)
        tkgrid(bt.checkZeros, row = 3, column = 0, sticky = 'we', padx = 1, pady = 3, ipadx = 1, ipady = 1)

    #######################################################################################################

    #Tab2
    subfr2 <- bwTabScrollableFrame(cmd.tab2)

        #######################

        frameZeros <- ttklabelframe(subfr2, text = lang.dlg[['label']][['4']], relief = 'groove')

        zeroExist <- tclVar(0)
        file.dataIndex <- tclVar()

        stateExistData <- if(tclvalue(zeroExist) == "1") "normal" else "disabled"

        chk.dataIdx <- tkcheckbutton(frameZeros, variable = zeroExist, text = lang.dlg[['checkbutton']][['1']], anchor = 'w', justify = 'left')
        en.dataIdx <- tkentry(frameZeros, textvariable = file.dataIndex, width = largeur2 + 5, state = stateExistData)
        bt.dataIdx <- ttkbutton(frameZeros, text = .cdtEnv$tcl$lang$global[['button']][['6']], state = stateExistData)

        tkgrid(chk.dataIdx, row = 0, column = 0, sticky = 'we', rowspan = 1, columnspan = 4, padx = 1, pady = 1, ipadx = 1, ipady = 1)
        tkgrid(bt.dataIdx, row = 0, column = 4, sticky = 'e', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
        tkgrid(en.dataIdx, row = 1, column = 0, sticky = 'we', rowspan = 1, columnspan = 5, padx = 1, pady = 1, ipadx = 1, ipady = 1)

        ########

        tkconfigure(bt.dataIdx, command = function(){
            path.dataIdx <- tclvalue(tkgetOpenFile(initialdir = getwd(), filetypes = .cdtEnv$tcl$data$filetypes6))
            if(path.dataIdx %in% c("", "NA") | is.na(path.dataIdx)) return(NULL)
            tclvalue(file.dataIndex) <- path.dataIdx

            if(file.exists(trimws(tclvalue(file.dataIndex)))){
                OutFZ <- try(readRDS(trimws(tclvalue(file.dataIndex))), silent = TRUE)
                if(inherits(OutFZ, "try-error")){
                    Insert.Messages.Out(gsub('[\r\n]', '', OutFZ[1]), TRUE, "e")
                    Insert.Messages.Out(lang.dlg[['message']][['5']], TRUE, "e")
                    tkconfigure(cb.stnID.disp, values = "")
                    tclvalue(.cdtData$EnvData$STN$stnID) <- ""
                    tkconfigure(cb.stnFZTable, values = "")
                    tclvalue(.cdtData$EnvData$STN$dateFZTable) <- ""
                    tkconfigure(cb.stnFZDateMap, values = "")
                    tclvalue(.cdtData$EnvData$STN$dateFZMap) <- ""
                    return(NULL)
                }

                .cdtData$EnvData$output <- OutFZ
                .cdtData$EnvData$PathData <- dirname(trimws(tclvalue(file.dataIndex)))

                ###############
                file.checkd <- file.path(.cdtData$EnvData$PathData, 'CDTDATASET', "FalseZerosResults.rds")
                if(!file.exists(file.checkd)){
                    Insert.Messages.Out(paste(file.checkd, lang.dlg[['message']][['6']]), TRUE, "e")
                    return(NULL)
                }
                .cdtData$EnvData$outzeros <- readRDS(file.checkd)
                if(is.null(.cdtData$EnvData$outzeros)){
                    Insert.Messages.Out(lang.dlg[['message']][['4']], TRUE, "s")
                    return(NULL)
                }

                ###############
                file.don <- file.path(.cdtData$EnvData$PathData, 'CDTDATASET', "StationData.rds")
                if(!file.exists(file.don)){
                    Insert.Messages.Out(paste(file.don, lang.dlg[['message']][['6']]), TRUE, "e")
                    return(NULL)
                }
                .cdtData$EnvData$stn.data <- readRDS(file.don)

                ###############
                set.station.id()
                ret <- try(set.date.false.zeros(), silent = TRUE)
                if(inherits(ret, "try-error") | is.null(ret)) return(NULL)

                ###############
                coords <- .cdtData$EnvData$stn.data[c('lon', 'lat', 'id')]
                .cdtData$EnvData$plot.maps[c('lon', 'lat', 'id')] <- coords

                xlim <- range(coords$lon, na.rm = TRUE)
                ylim <- range(coords$lat, na.rm = TRUE)
                initialize_zoom_frame(xlim, ylim)
            }
        })

        ###############
        tkbind(chk.dataIdx, "<Button-1>", function(){
            stateExistData <- if(tclvalue(zeroExist) == '1') 'disabled' else 'normal'
            tkconfigure(en.dataIdx, state = stateExistData)
            tkconfigure(bt.dataIdx, state = stateExistData)

            stateZero <- if(tclvalue(zeroExist) == '1') 'normal' else 'disabled'
            tcl(tknote.cmd, 'itemconfigure', cmd.tab1$IDtab, state = stateZero)
        })

        #######################

        frameStnId <- ttklabelframe(subfr2, text = lang.dlg[['label']][['5']], relief = 'groove')

        .cdtData$EnvData$STN$stnID <- tclVar()

        bt.stnID.prev <- ttkbutton(frameStnId, text = "<<", width = largeur5)
        bt.stnID.next <- ttkbutton(frameStnId, text = ">>", width = largeur5)
        cb.stnID.disp <- ttkcombobox(frameStnId, values = "", textvariable = .cdtData$EnvData$STN$stnID, width = largeur3, justify = 'center')
        bt.display.FZ <- ttkbutton(frameStnId, text = lang.dlg[['button']][['3']])

        tkgrid(bt.stnID.prev, row = 0, column = 0, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 2, ipadx = 1, ipady = 1)
        tkgrid(cb.stnID.disp, row = 0, column = 1, sticky = 'we', rowspan = 1, columnspan = 1, padx = 3, pady = 2, ipadx = 1, ipady = 1)
        tkgrid(bt.stnID.next, row = 0, column = 2, sticky = 'we', rowspan = 1, columnspan = 1, padx = 2, pady = 2, ipadx = 1, ipady = 1)
        tkgrid(bt.display.FZ, row = 1, column = 0, sticky = 'we', rowspan = 1, columnspan = 3, padx = 2, pady = 2, ipadx = 1, ipady = 1)

        #######

        tkconfigure(bt.stnID.prev, command = function(){
            if(!is.null(.cdtData$EnvData$outzeros)){
                STNID <- .cdtData$EnvData$outzeros$stn
                istn <- which(STNID == trimws(tclvalue(.cdtData$EnvData$STN$stnID)))
                istn <- istn - 1
                if(istn < 1) istn <- length(STNID)
                tclvalue(.cdtData$EnvData$STN$stnID) <- STNID[istn]

                ret <- try(set.date.false.zeros(), silent = TRUE)
                if(inherits(ret, "try-error") | is.null(ret)) return(NULL)
            }
        })

        tkconfigure(bt.stnID.next, command = function(){
            if(!is.null(.cdtData$EnvData$outzeros)){
                STNID <- .cdtData$EnvData$outzeros$stn
                istn <- which(STNID == trimws(tclvalue(.cdtData$EnvData$STN$stnID)))
                istn <- istn + 1
                if(istn > length(STNID)) istn <- 1
                tclvalue(.cdtData$EnvData$STN$stnID) <- STNID[istn]

                ret <- try(set.date.false.zeros(), silent = TRUE)
                if(inherits(ret, "try-error") | is.null(ret)) return(NULL)
            }
        })

        tkbind(cb.stnID.disp, "<<ComboboxSelected>>", function(){
            if(!is.null(.cdtData$EnvData$outzeros)){
                ret <- try(set.date.false.zeros(), silent = TRUE)
                if(inherits(ret, "try-error") | is.null(ret)) return(NULL)
            }
        })

        #######################

        .cdtData$EnvData$tab$TableStat <- NULL

        tkconfigure(bt.display.FZ, command = function(){
            stnid <- trimws(tclvalue(.cdtData$EnvData$STN$stnID))
            if(stnid == "") return(NULL)
            donFZstat <- .cdtData$EnvData$outzeros$res[[stnid]]$stat
            tab.title <- paste0(stnid, "-FZ-Outputs-Table")

            .cdtData$EnvData$tab$TableStat <- tableNotebookTab_unik(donFZstat,
                                                                .cdtData$EnvData$tab$TableStat,
                                                                tab.title, 12, 'falsezero')
            menuRowHandleCopyPaste.OpenTable()
            .cdtData$EnvData$STN$toreplace <- stnid
        })

        #######################

        frameFZTable <- ttklabelframe(subfr2, text = lang.dlg[['label']][['6']], relief = 'groove')

        .cdtData$EnvData$STN$dateFZTable <- tclVar()

        frFZDateTable <- tkframe(frameFZTable)
        txt.stnFZTable <- tklabel(frFZDateTable, text = lang.dlg[['label']][['7']], anchor = 'e', justify = 'right')
        cb.stnFZTable <- ttkcombobox(frFZDateTable, values = "", textvariable = .cdtData$EnvData$STN$dateFZTable, width = largeur3, justify = 'center')

        bt.stnFZTable.prev <- ttkbutton(frameFZTable, text = "<<", width = largeur4)
        bt.stnFZTable.disp <- ttkbutton(frameFZTable, text = lang.dlg[['button']][['4']], width = largeur4)
        bt.stnFZTable.next <- ttkbutton(frameFZTable, text = ">>", width = largeur4)

        ########  
        tkgrid(txt.stnFZTable, row = 0, column = 0, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
        tkgrid(cb.stnFZTable, row = 0, column = 1, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)

        tkgrid(frFZDateTable, row = 0, column = 0, sticky = '', rowspan = 1, columnspan = 3, padx = 1, pady = 2, ipadx = 1, ipady = 1)
        tkgrid(bt.stnFZTable.prev, row = 1, column = 0, sticky = 'we', rowspan = 1, columnspan = 1, padx = 2, pady = 2, ipadx = 1, ipady = 1)
        tkgrid(bt.stnFZTable.disp, row = 1, column = 1, sticky = 'we', rowspan = 1, columnspan = 1, padx = 2, pady = 2, ipadx = 1, ipady = 1)
        tkgrid(bt.stnFZTable.next, row = 1, column = 2, sticky = 'we', rowspan = 1, columnspan = 1, padx = 2, pady = 2, ipadx = 1, ipady = 1)

        helpWidget(frameFZTable, lang.dlg[['tooltip']][['6']], lang.dlg[['status']][['6']])

        #########

        .cdtData$EnvData$tab$TableNbrs <- NULL

        tkconfigure(bt.stnFZTable.prev, command = function(){
            stnid <- trimws(tclvalue(.cdtData$EnvData$STN$stnID))
            if(stnid == "") return(NULL)
            dateFZ <- .cdtData$EnvData$outzeros$res[[stnid]]$date
            idaty <- which(dateFZ == trimws(tclvalue(.cdtData$EnvData$STN$dateFZTable)))
            idaty <- idaty - 1
            if(idaty < 1) idaty <- length(dateFZ)
            tclvalue(.cdtData$EnvData$STN$dateFZTable) <- dateFZ[idaty]

            display.table.neighbors()
        })

        tkconfigure(bt.stnFZTable.next, command = function(){
            stnid <- trimws(tclvalue(.cdtData$EnvData$STN$stnID))
            if(stnid == "") return(NULL)
            dateFZ <- .cdtData$EnvData$outzeros$res[[stnid]]$date
            idaty <- which(dateFZ == trimws(tclvalue(.cdtData$EnvData$STN$dateFZTable)))
            idaty <- idaty + 1
            if(idaty > length(dateFZ)) idaty <- 1
            tclvalue(.cdtData$EnvData$STN$dateFZTable) <- dateFZ[idaty]

            display.table.neighbors()
        })

        tkconfigure(bt.stnFZTable.disp, command = function(){
            stnid <- trimws(tclvalue(.cdtData$EnvData$STN$stnID))
            if(stnid == "") return(NULL)

            display.table.neighbors()
        })

        #######################

        frameFZMap <- ttklabelframe(subfr2, text = lang.dlg[['label']][['8']], relief = 'groove')

        .cdtData$EnvData$STN$dateFZMap <- tclVar()
        .cdtData$EnvData$STN$statFZMap <- tclVar('mon')

        stats.FZMAP <- tclVar()
        CbstatFZMapVAL <- lang.dlg[['combobox']][['1']]
        statFZMapVAL <- c('mon', 'nzero', 'nbday')
        tclvalue(stats.FZMAP) <- CbstatFZMapVAL[statFZMapVAL %in% tclvalue(.cdtData$EnvData$STN$statFZMap)]

        frFZDateMap <- tkframe(frameFZMap)
        txt.stnFZDateMap <- tklabel(frFZDateMap, text = lang.dlg[['label']][['9']], anchor = 'e', justify = 'right')
        cb.stnFZDateMap <- ttkcombobox(frFZDateMap, values = "", textvariable = .cdtData$EnvData$STN$dateFZMap, width = largeur3, justify = 'center')

        bt.stnFZMap.prev <- ttkbutton(frameFZMap, text = "<<", width = largeur4)
        bt.stnFZMap.disp <- ttkbutton(frameFZMap, text = .cdtEnv$tcl$lang$global[['button']][['3']], width = largeur4)
        bt.stnFZMap.next <- ttkbutton(frameFZMap, text = ">>", width = largeur4)

        frFZStatMap <- tkframe(frameFZMap)
        txt.stnFZStatMap <- tklabel(frFZStatMap, text = lang.dlg[['label']][['10']], anchor = 'e', justify = 'right')
        cb.stnFZStatMap <- ttkcombobox(frFZStatMap, values = CbstatFZMapVAL, textvariable = stats.FZMAP, width = largeur3, justify = 'center')

        ########  
        tkgrid(txt.stnFZDateMap, row = 0, column = 0, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
        tkgrid(cb.stnFZDateMap, row = 0, column = 1, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)

        tkgrid(txt.stnFZStatMap, row = 0, column = 0, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
        tkgrid(cb.stnFZStatMap, row = 0, column = 1, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)

        tkgrid(frFZDateMap, row = 0, column = 0, sticky = '', rowspan = 1, columnspan = 3, padx = 1, pady = 2, ipadx = 1, ipady = 1)
        tkgrid(bt.stnFZMap.prev, row = 1, column = 0, sticky = 'we', rowspan = 1, columnspan = 1, padx = 2, pady = 2, ipadx = 1, ipady = 1)
        tkgrid(bt.stnFZMap.disp, row = 1, column = 1, sticky = 'we', rowspan = 1, columnspan = 1, padx = 2, pady = 2, ipadx = 1, ipady = 1)
        tkgrid(bt.stnFZMap.next, row = 1, column = 2, sticky = 'we', rowspan = 1, columnspan = 1, padx = 2, pady = 2, ipadx = 1, ipady = 1)
        tkgrid(frFZStatMap, row = 2, column = 0, sticky = '', rowspan = 1, columnspan = 3, padx = 1, pady = 2, ipadx = 1, ipady = 1)

        #########

        .cdtData$EnvData$tab$dataFZMap <- NULL

        tkconfigure(bt.stnFZMap.disp, command = function(){
            stnid <- trimws(tclvalue(.cdtData$EnvData$STN$stnID))
            if(stnid == "") return(NULL)

            display.map.neighbors()
        })

        tkconfigure(bt.stnFZMap.prev, command = function(){
            stnid <- trimws(tclvalue(.cdtData$EnvData$STN$stnID))
            if(stnid == "") return(NULL)
            dateFZ <- .cdtData$EnvData$outzeros$res[[stnid]]$date
            idaty <- which(dateFZ == trimws(tclvalue(.cdtData$EnvData$STN$dateFZMap)))
            idaty <- idaty - 1
            if(idaty < 1) idaty <- length(dateFZ)
            tclvalue(.cdtData$EnvData$STN$dateFZMap) <- dateFZ[idaty]

            display.map.neighbors()
        })

        tkconfigure(bt.stnFZMap.next, command = function(){
            stnid <- trimws(tclvalue(.cdtData$EnvData$STN$stnID))
            if(stnid == "") return(NULL)
            dateFZ <- .cdtData$EnvData$outzeros$res[[stnid]]$date
            idaty <- which(dateFZ == trimws(tclvalue(.cdtData$EnvData$STN$dateFZMap)))
            idaty <- idaty + 1
            if(idaty > length(dateFZ)) idaty <- 1
            tclvalue(.cdtData$EnvData$STN$dateFZMap) <- dateFZ[idaty]

            display.map.neighbors()
        })

        tkbind(cb.stnFZStatMap, "<<ComboboxSelected>>", function(){
            stat_fzmap <- statFZMapVAL[CbstatFZMapVAL %in% trimws(tclvalue(stats.FZMAP))]
            tclvalue(.cdtData$EnvData$STN$statFZMap) <- stat_fzmap
        })

        #######################

        bt.replace.FZ <- ttkbutton(subfr2, text = lang.dlg[['button']][['5']])

        helpWidget(bt.replace.FZ, lang.dlg[['tooltip']][['7']], lang.dlg[['status']][['7']])

        ######

        tkconfigure(bt.replace.FZ, command = function(){
            if(is.null(.cdtData$EnvData$outzeros)) return(NULL)

            stnid <- trimws(tclvalue(.cdtData$EnvData$STN$stnID))

            replace <- TRUE
            if(!is.null(.cdtData$EnvData$STN$toreplace))
                if(.cdtData$EnvData$STN$toreplace != stnid) replace <- FALSE

            if(replace){
                info <- .cdtData$EnvData$output$info[[3]]
                file.stn <- file.path(.cdtData$EnvData$PathData, 'CDTSTATIONS', .cdtData$EnvData$output$info[[1]])
                tmp <- utils::read.table(file.stn, header = FALSE, sep = info$sepr, stringsAsFactors = FALSE, colClasses = "character")

                daty <- .cdtData$EnvData$outzeros$res[[stnid]]$date
                index.mon <- unlist(.cdtData$EnvData$output$index[daty])
                is.elv <- if(is.null(.cdtData$EnvData$stn.data$elv)) 3 else 4
                index.mon <- index.mon + is.elv
                istn <- which(.cdtData$EnvData$stn.data$id == stnid) + 1
                replace0 <- tmp[index.mon, istn]
                replace0[!is.na(replace0) & replace0 == 0] <- info$miss.val
                tmp[index.mon, istn] <- replace0

                utils::write.table(tmp, file.stn, sep = info$sepr, na = info$miss.val, row.names = FALSE, col.names = FALSE, quote = FALSE)
                rm(tmp); gc()
                Insert.Messages.Out(paste(stnid, ':', lang.dlg[['message']][['7']]), TRUE, "s")
            }else{
                Insert.Messages.Out(lang.dlg[['message']][['8']], TRUE, "i")
            }
        })

        #########################################

        tkgrid(frameZeros, row = 0, column = 0, sticky = 'we', padx = 1, pady = 1, ipadx = 1, ipady = 1)
        tkgrid(frameStnId, row = 1, column = 0, sticky = 'we', padx = 1, pady = 1, ipadx = 1, ipady = 1)
        tkgrid(frameFZTable, row = 3, column = 0, sticky = 'we', padx = 1, pady = 1, ipadx = 0, ipady = 1)
        tkgrid(frameFZMap, row = 4, column = 0, sticky = 'we', padx = 1, pady = 1, ipadx = 0, ipady = 1)
        tkgrid(bt.replace.FZ, row = 5, column = 0, sticky = 'we', padx = 1, pady = 5, ipadx = 1, ipady = 1)

    #######################################################################################################

    #Tab3
    subfr3 <- bwTabScrollableFrame(cmd.tab3)

        #######################

        frameZoom <- create_zoom_frame(subfr3, .cdtData$EnvData$tab$dataFZMap)
        tkgrid(frameZoom, row = 0, column = 0, sticky = '')

    #######################################################################################################

    #Tab4
    subfr4 <- bwTabScrollableFrame(cmd.tab4)

        #######################

        frameSHP <- create_shpLayer_frame(subfr4)
        tkgrid(frameSHP, row = 0, column = 0, sticky = 'we', pady = 1)

    #######################################################################################################

    set.station.id <- function(){
        STNID <- .cdtData$EnvData$outzeros$stn
        tkconfigure(cb.stnID.disp, values = STNID)
        tclvalue(.cdtData$EnvData$STN$stnID) <- STNID[1]
    }

    set.date.false.zeros <- function(){
        stnid <- trimws(tclvalue(.cdtData$EnvData$STN$stnID))
        if(stnid == "") return(NULL)
        dateFZ <- .cdtData$EnvData$outzeros$res[[stnid]]$date
        tkconfigure(cb.stnFZTable, values = dateFZ)
        tclvalue(.cdtData$EnvData$STN$dateFZTable) <- dateFZ[1]
        tkconfigure(cb.stnFZDateMap, values = dateFZ)
        tclvalue(.cdtData$EnvData$STN$dateFZMap) <- dateFZ[1]

        return(0)
    }

    display.table.neighbors <- function(){
        stnid <- trimws(tclvalue(.cdtData$EnvData$STN$stnID))
        daty <- trimws(tclvalue(.cdtData$EnvData$STN$dateFZTable))
        index.mon <- .cdtData$EnvData$output$index[[daty]]
        dist <- c(0, .cdtData$EnvData$outzeros$res[[stnid]]$dist[[daty]])
        istn <- which(.cdtData$EnvData$stn.data$id == stnid)
        nstn <- .cdtData$EnvData$outzeros$res[[stnid]]$stn[[daty]]
        STN <- .cdtData$EnvData$stn.data$id[c(istn, nstn)]
        don.nbrs <- .cdtData$EnvData$stn.data$data[index.mon, c(istn, nstn), drop = FALSE]
        don.date <- .cdtData$EnvData$stn.data$dates[index.mon]
        don.disp <- rbind(cbind(c("STN.ID", "Distance (km)"), rbind(STN, dist)), cbind(don.date, don.nbrs))
        dimnames(don.disp) <- NULL

        .cdtData$EnvData$tab$TableNbrs <- tableNotebookTab_unik(don.disp,
                                                            .cdtData$EnvData$tab$TableNbrs,
                                                            paste0(stnid, "-FZ-Neighbors-Table"))
        tabid <- as.integer(tclvalue(tkindex(.cdtEnv$tcl$main$tknotes, 'current'))) + 1
        table1 <- .cdtData$OpenTab$Data[[tabid]][[2]][[1]]

        .Tcl(paste(table1, 'tag', 'celltag', 'ZchkStn',
            paste(1:as.integer(tclvalue(tkindex(table1, 'end', 'row'))), 2, sep = ',', collapse = ' ')))
        tcl(table1, "tag", "configure", "ZchkStn", bg = "lightgoldenrod1")
        .Tcl(paste(table1, 'tag', 'celltag', 'idZchkStn1',
            paste(1, 1:as.integer(tclvalue(tkindex(table1, 'end', 'col'))), sep = ',', collapse = ' ')))
        tcl(table1, "tag", "configure", "idZchkStn1", bg = "lightcyan1")
        .Tcl(paste(table1, 'tag', 'celltag', 'idZchkStn2',
            paste(2, 1:as.integer(tclvalue(tkindex(table1, 'end', 'col'))), sep = ',', collapse = ' ')))
        tcl(table1, "tag", "configure", "idZchkStn2", bg = "lightcyan1")
    }

    display.map.neighbors <- function(){
        if(!is.null(.cdtData$EnvData$outzeros) &
           is.null(.cdtData$EnvData$outzeros$mon))
        {
            Insert.Messages.Out(lang.dlg[['message']][['9']], TRUE, "e")
            return(NULL)
        }

        stnid <- trimws(tclvalue(.cdtData$EnvData$STN$stnID))
        tab.title <- paste0(stnid, "-FZ-Neighbors-Map")
        imgContainer <- CDT.Display.Points.Zoom(qcPlot_FalseZero.Check, .cdtData$EnvData$tab$dataFZMap, tab.title)
        .cdtData$EnvData$tab$dataFZMap <- imageNotebookTab_unik(imgContainer, .cdtData$EnvData$tab$dataFZMap)
    }

    #######################################################################################################

    .cdtData$EnvData$qcRRZeroCheck$SaveEdit <- function(dat2sav){
        stnid <- trimws(tclvalue(.cdtData$EnvData$STN$stnID))
        .cdtData$EnvData$STN$toreplace <- stnid

        tout.null <- TRUE
        if(!is.null(dat2sav)){
            daty <- trimws(as.character(dat2sav$YYYYMM))
            ix <- !is.na(daty) & daty != ""

            if(any(ix)){
                dat2sav <- dat2sav[ix, , drop = FALSE]
                daty <- daty[ix]
                idaty <- .cdtData$EnvData$outzeros$res[[stnid]]$date %in% daty
                .cdtData$EnvData$outzeros$res[[stnid]]$stat <- dat2sav
                .cdtData$EnvData$outzeros$res[[stnid]]$date <- daty
                .cdtData$EnvData$outzeros$res[[stnid]]$dist <- .cdtData$EnvData$outzeros$res[[stnid]]$dist[idaty]
                .cdtData$EnvData$outzeros$res[[stnid]]$stn <- .cdtData$EnvData$outzeros$res[[stnid]]$stn[idaty]
                tout.null <- FALSE
            }
        }

        if(tout.null){
            STN.ID <- .cdtData$EnvData$outzeros$stn
            STN.ID <- STN.ID[!STN.ID %in% stnid]
            if(length(STN.ID)){
                .cdtData$EnvData$outzeros$stn <- STN.ID
                .cdtData$EnvData$outzeros$res[[stnid]] <- NULL
            }else .cdtData$EnvData$outzeros <- NULL
        }

        file.checkd <- file.path(.cdtData$EnvData$PathData, 'CDTDATASET', "FalseZerosResults.rds")
        saveRDS(.cdtData$EnvData$outzeros, file.checkd)

        if(is.null(.cdtData$EnvData$outzeros)){
            tkconfigure(cb.stnID.disp, values = "")
            tclvalue(.cdtData$EnvData$STN$stnID) <- ""
            tkconfigure(cb.stnFZTable, values = "")
            tclvalue(.cdtData$EnvData$STN$dateFZTable) <- ""
        }else{
            STNID <- .cdtData$EnvData$outzeros$stn
            tkconfigure(cb.stnID.disp, values = STNID)
            stnid <- trimws(tclvalue(.cdtData$EnvData$STN$stnID))
            STN <- .cdtData$EnvData$stn.data$id
            while(!stnid %in% STNID){
                is <- which(STN == stnid) + 1
                if(is > length(STN)) is <- 1
                stnid <- STN[is]
            }
            tclvalue(.cdtData$EnvData$STN$stnID) <- stnid

            dateFZ <- .cdtData$EnvData$outzeros$res[[stnid]]$date
            tkconfigure(cb.stnFZTable, values = dateFZ)
            idaty <- trimws(tclvalue(.cdtData$EnvData$STN$dateFZTable))
            if(!idaty %in% dateFZ) idaty <- dateFZ[1]
            tclvalue(.cdtData$EnvData$STN$dateFZTable) <- idaty
        }
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
