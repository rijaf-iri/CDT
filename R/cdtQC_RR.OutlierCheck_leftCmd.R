
qcRROutlierCheckPanelCmd <- function(){
    listOpenFiles <- openFile_ttkcomboList()
    if(WindowsOS()){
        largeur0 <- 23
        largeur1 <- 31
        largeur2 <- 33
        largeur3 <- 12
        largeur4 <- 18
        largeur5 <- 11
        largeur6 <- 19
        largeur7 <- 7
    }else{
        largeur0 <- 23
        largeur1 <- 32
        largeur2 <- 33
        largeur3 <- 12
        largeur4 <- 19
        largeur5 <- 11
        largeur6 <- 19
        largeur7 <- 7
    }

    ###################

    GeneralParameters <- list(intstep = "daily", infile = "", outdir = "",
                              params = list(precip.max = 300, sigma = 3.5,
                                            voisin = list(min = 4, max = 15, dist = 120, elv = 800),
                                            elv = list(use = FALSE, dem = TRUE, file = "")
                                           )
                              )

    MOIS <- format(ISOdate(2014, 1:12, 1), "%B")

    .cdtData$EnvData$tab$ylabMon <- "Precipitation [mm]"

    .cdtData$EnvData$STN$Opt <- list(
                                    stn = list(col = "blue", pch = 23, cex = 1.2, txt.col = 'red', txt.cex = 1.0),
                                    use = list(col = 'darkgreen', pch = 20, cex = 1.0, txt.col = 'blue', txt.cex = 0.9),
                                    sel = list(col = 'orange', pch = 20, cex = 1.0, txt.col = 'blue', txt.cex = 0.85),
                                    vois = list(col = 'red', pch = 20, cex = 1.0, txt.col = 'blue', txt.cex = 0.8),
                                    all = list(col = 'darkred', pch = 20, cex = 1.0, txt.col = 'blue', txt.cex = 0.7),
                                    circle = list(draw = TRUE, lwd = 1.5, col = 'red')
                                )

    .cdtData$EnvData$SHPOp <- list(col = "black", lwd = 1.5)

    .cdtData$EnvData$dem$Opt <- list(
                                    user.colors = list(custom = FALSE, color = NULL),
                                    user.levels = list(custom = FALSE, levels = NULL, equidist = FALSE),
                                    preset.colors = list(color = 'qc.gray.colors', reverse = FALSE)
                                )

    .cdtData$EnvData$sat$dir <- ""
    .cdtData$EnvData$sat$sample <- ""
    .cdtData$EnvData$sat$format <- "rfe_%s%s%s.nc"
    .cdtData$EnvData$sat$sat.data <- NULL

    .cdtData$EnvData$sat$Opt <- list(
                                    user.colors = list(custom = FALSE, color = NULL),
                                    user.levels = list(custom = FALSE, levels = NULL, equidist = FALSE),
                                    preset.colors = list(color = 'qcrr.grid.colors', reverse = FALSE)
                                )

    ###################

    xml.dlg <- file.path(.cdtDir$dirLocal, "languages", "cdtQC_RR.OutlierCheck_leftCmd.xml")
    lang.dlg <- cdtLanguageParse(xml.dlg, .cdtData$Config$lang.iso)
    .cdtData$EnvData$message <- lang.dlg[['message']]

    ###################

    .cdtEnv$tcl$main$cmd.frame <- tkframe(.cdtEnv$tcl$main$panel.left)

    tknote.cmd <- bwNoteBook(.cdtEnv$tcl$main$cmd.frame)
    cmd.tab1 <- bwAddTab(tknote.cmd, text = lang.dlg[['tab_title']][['1']])
    cmd.tab2 <- bwAddTab(tknote.cmd, text = lang.dlg[['tab_title']][['2']])
    cmd.tab2a <- bwAddTab(tknote.cmd, text = lang.dlg[['tab_title']][['2-1']])
    cmd.tab3 <- bwAddTab(tknote.cmd, text = lang.dlg[['tab_title']][['3']])
    cmd.tab4 <- bwAddTab(tknote.cmd, text = lang.dlg[['tab_title']][['4']])

    bwRaiseTab(tknote.cmd, cmd.tab1)

    tkgrid.columnconfigure(cmd.tab1, 0, weight = 1)
    tkgrid.columnconfigure(cmd.tab2, 0, weight = 1)
    tkgrid.columnconfigure(cmd.tab2a, 0, weight = 1)
    tkgrid.columnconfigure(cmd.tab3, 0, weight = 1)
    tkgrid.columnconfigure(cmd.tab4, 0, weight = 1)

    tkgrid.rowconfigure(cmd.tab1, 0, weight = 1)
    tkgrid.rowconfigure(cmd.tab2, 0, weight = 1)
    tkgrid.rowconfigure(cmd.tab2a, 0, weight = 1)
    tkgrid.rowconfigure(cmd.tab3, 0, weight = 1)
    tkgrid.rowconfigure(cmd.tab4, 0, weight = 1)

    #######################################################################################################

    #Tab1
    subfr1 <- bwTabScrollableFrame(cmd.tab1)

        ##############################################

        frameTimeS <- ttklabelframe(subfr1, text = lang.dlg[['label']][['1']], relief = 'groove')

        timeSteps <- tclVar()
        CbperiodVAL <- .cdtEnv$tcl$lang$global[['combobox']][['1']][3:6]
        periodVAL <- c('daily', 'pentad', 'dekadal', 'monthly')
        tclvalue(timeSteps) <- CbperiodVAL[periodVAL %in% GeneralParameters$intstep]

        cb.fperiod <- ttkcombobox(frameTimeS, values = CbperiodVAL, textvariable = timeSteps, width = largeur0)

        tkgrid(cb.fperiod, row = 0, column = 0, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)

        helpWidget(cb.fperiod, lang.dlg[['tooltip']][['1']], lang.dlg[['status']][['1']])

        #######################

        frameInData <- ttklabelframe(subfr1, text = lang.dlg[['label']][['2']], relief = 'groove')

        input.file <- tclVar(GeneralParameters$infile)

        txt.infile <- tklabel(frameInData, text = lang.dlg[['label']][['3']], anchor = 'w', justify = 'left')
        cb.infile <- ttkcombobox(frameInData, values = unlist(listOpenFiles), textvariable = input.file, width = largeur1)
        bt.infile <- tkbutton(frameInData, text = "...")

        tkgrid(txt.infile, row = 0, column = 0, sticky = 'we', rowspan = 1, columnspan = 5, padx = 1, pady = 1, ipadx = 1, ipady = 1)
        tkgrid(cb.infile, row = 1, column = 0, sticky = 'we', rowspan = 1, columnspan = 4, padx = 0, pady = 1, ipadx = 1, ipady = 1)
        tkgrid(bt.infile, row = 1, column = 4, sticky = 'we', rowspan = 1, columnspan = 1, padx = 0, pady = 1, ipadx = 1, ipady = 1)

        helpWidget(cb.infile, lang.dlg[['tooltip']][['2']], lang.dlg[['status']][['2']])
        helpWidget(bt.infile, lang.dlg[['tooltip']][['3']], lang.dlg[['status']][['3']])

        #####
 
        tkconfigure(bt.infile, command = function(){
            dat.opfiles <- getOpenFiles(.cdtEnv$tcl$main$win)
            if(!is.null(dat.opfiles)){
                update.OpenFiles('ascii', dat.opfiles)
                listOpenFiles[[length(listOpenFiles) + 1]] <<- dat.opfiles[[1]]
                tclvalue(input.file) <- dat.opfiles[[1]]
                tkconfigure(cb.infile, values = unlist(listOpenFiles))
            }
        })

        #######################

        bt.params <- ttkbutton(subfr1, text = lang.dlg[['button']][['1']])

        helpWidget(bt.params, lang.dlg[['tooltip']][['4']], lang.dlg[['status']][['4']])

        tkconfigure(bt.params, command = function(){
            Params <- GeneralParameters[["params"]]
            GeneralParameters[["params"]] <<- getParams.RR.OutlierCheck(Params)
        })

        #######################

        frameDirSav <- tkframe(subfr1, relief = 'groove', borderwidth = 2)

        dir.save <- tclVar(GeneralParameters$outdir)

        txt.dir.save <- tklabel(frameDirSav, text = lang.dlg[['label']][['4']], anchor = 'w', justify = 'left')
        en.dir.save <- tkentry(frameDirSav, textvariable = dir.save, width = largeur2)
        bt.dir.save <- tkbutton(frameDirSav, text = "...")

        ######
        tkgrid(txt.dir.save, row = 0, column = 0, sticky = 'we', rowspan = 1, columnspan = 5, padx = 1, pady = 0, ipadx = 1, ipady = 1)
        tkgrid(en.dir.save, row = 1, column = 0, sticky = 'we', rowspan = 1, columnspan = 4, padx = 0, pady = 0, ipadx = 1, ipady = 1)
        tkgrid(bt.dir.save, row = 1, column = 4, sticky = 'we', rowspan = 1, columnspan = 1, padx = 0, pady = 0, ipadx = 1, ipady = 1)

        helpWidget(en.dir.save, lang.dlg[['tooltip']][['5']], lang.dlg[['status']][['5']])
        helpWidget(bt.dir.save, lang.dlg[['tooltip']][['6']], lang.dlg[['status']][['6']])

        ######
        tkconfigure(bt.dir.save, command = function() fileORdir2Save(dir.save, isFile = FALSE))

        #############################

        bt.doQC <- ttkbutton(subfr1, text = lang.dlg[['button']][['2']])

        tkconfigure(bt.doQC, command = function(){
            GeneralParameters$intstep <- periodVAL[CbperiodVAL %in% str_trim(tclvalue(timeSteps))]
            GeneralParameters$infile <- str_trim(tclvalue(input.file))
            GeneralParameters$outdir <- str_trim(tclvalue(dir.save))

            # assign('GeneralParameters', GeneralParameters, envir = .GlobalEnv)

            Insert.Messages.Out(lang.dlg[['message']][['1']], TRUE, "i")

            if(!dir.exists(GeneralParameters$outdir)){
                msgdir <- paste(GeneralParameters$outdir, lang.dlg[['message']][['6']])
                Insert.Messages.Out(msgdir, TRUE, "e")
                return(NULL)
            }

            tkconfigure(.cdtEnv$tcl$main$win, cursor = 'watch')
            tcl('update')
            ret <- tryCatch(
                {
                    qcRROutliersCheckProcs(GeneralParameters)
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
                if(ret == 0){
                    Insert.Messages.Out(lang.dlg[['message']][['2']], TRUE, "s")

                    if(is.null(.cdtData$EnvData$outqc)){
                        Insert.Messages.Out(lang.dlg[['message']][['4']], TRUE, "s")
                        return(NULL)
                    }

                    coords <- .cdtData$EnvData$stn.data[c('lon', 'lat', 'id')]
                    .cdtData$EnvData$plot.maps[c('lon', 'lat', 'id')] <- coords

                    ###############
                    set.station.id()
                    ret <- try(set.date.outliers(), silent = TRUE)
                    if(inherits(ret, "try-error") | is.null(ret)) return(NULL)
                    set.initialize.Zoom()
                }else Insert.Messages.Out(lang.dlg[['message']][['3']], TRUE, 'e')
            }else Insert.Messages.Out(lang.dlg[['message']][['3']], TRUE, 'e')
        })

        #########################################

        tkgrid(frameTimeS, row = 0, column = 0, sticky = '', padx = 1, pady = 1, ipadx = 1, ipady = 1)
        tkgrid(frameInData, row = 1, column = 0, sticky = 'we', padx = 1, pady = 1, ipadx = 1, ipady = 1)
        tkgrid(bt.params, row = 2, column = 0, sticky = 'we', padx = 1, pady = 5, ipadx = 1, ipady = 1)
        tkgrid(frameDirSav, row = 3, column = 0, sticky = 'we', padx = 1, pady = 1, ipadx = 1, ipady = 1)
        tkgrid(bt.doQC, row = 4, column = 0, sticky = 'we', padx = 1, pady = 3, ipadx = 1, ipady = 1)

    #######################################################################################################

    #Tab2
    subfr2 <- bwTabScrollableFrame(cmd.tab2)

        ##############################################

        frameOutQC <- ttklabelframe(subfr2, text = lang.dlg[['label']][['5']], relief = 'groove')

        QCExist <- tclVar(0)
        file.dataIndex <- tclVar()

        stateExistData <- if(tclvalue(QCExist) == '1') 'normal' else 'disabled'

        chk.dataIdx <- tkcheckbutton(frameOutQC, variable = QCExist, text = lang.dlg[['checkbutton']][['1']], anchor = 'w', justify = 'left')
        en.dataIdx <- tkentry(frameOutQC, textvariable = file.dataIndex, width = largeur2 + 5, state = stateExistData)
        bt.dataIdx <- ttkbutton(frameOutQC, text = .cdtEnv$tcl$lang$global[['button']][['6']], state = stateExistData)

        tkgrid(chk.dataIdx, row = 0, column = 0, sticky = 'we', rowspan = 1, columnspan = 4, padx = 1, pady = 1, ipadx = 1, ipady = 1)
        tkgrid(bt.dataIdx, row = 0, column = 4, sticky = 'e', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
        tkgrid(en.dataIdx, row = 1, column = 0, sticky = 'we', rowspan = 1, columnspan = 5, padx = 1, pady = 1, ipadx = 1, ipady = 1)

        ###############
        tkconfigure(bt.dataIdx, command = function(){
            path.dataIdx <- tclvalue(tkgetOpenFile(initialdir = getwd(), filetypes = .cdtEnv$tcl$data$filetypes6))
            if(path.dataIdx %in% c("", "NA") | is.na(path.dataIdx)) return(NULL)
            tclvalue(file.dataIndex) <- path.dataIdx

            if(file.exists(str_trim(tclvalue(file.dataIndex)))){
                OutQC <- try(readRDS(str_trim(tclvalue(file.dataIndex))), silent = TRUE)
                if(inherits(OutQC, "try-error")){
                    Insert.Messages.Out(gsub('[\r\n]', '', OutQC[1]), TRUE, "e")
                    Insert.Messages.Out(lang.dlg[['message']][['5']], TRUE, "e")
                    tkconfigure(.cdtData$EnvData$STN$cb.stnID, values = "")
                    tclvalue(.cdtData$EnvData$STN$stnID) <- ""
                    tkconfigure(.cdtData$EnvData$STN$cb.QCSP, values = "")
                    tclvalue(.cdtData$EnvData$STN$dateSP) <- ""
                    return(NULL)
                }

                .cdtData$EnvData$output <- OutQC
                .cdtData$EnvData$PathData <- dirname(str_trim(tclvalue(file.dataIndex)))

                ###############
                file.checkd <- file.path(.cdtData$EnvData$PathData, 'CDTDATASET', "QCResults.rds")
                if(!file.exists(file.checkd)){
                    Insert.Messages.Out(paste(file.checkd, lang.dlg[['message']][['6']]), TRUE, "e")
                    return(NULL)
                }
                .cdtData$EnvData$outqc <- readRDS(file.checkd)
                if(is.null(.cdtData$EnvData$outqc)){
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

                coords <- .cdtData$EnvData$stn.data[c('lon', 'lat', 'id')]
                .cdtData$EnvData$plot.maps[c('lon', 'lat', 'id')] <- coords

                ###############
                set.station.id()
                ret <- try(set.date.outliers(), silent = TRUE)
                if(inherits(ret, "try-error") | is.null(ret)) return(NULL)
                set.initialize.Zoom()
            }
        })

        ###############
        tkbind(chk.dataIdx, "<Button-1>", function(){
            stateExistData <- if(tclvalue(QCExist) == '1') 'disabled' else 'normal'
            tkconfigure(en.dataIdx, state = stateExistData)
            tkconfigure(bt.dataIdx, state = stateExistData)

            stateQC <- if(tclvalue(QCExist) == '1') 'normal' else 'disabled'
            tcl(tknote.cmd, 'itemconfigure', cmd.tab1$IDtab, state = stateQC)
        })

        #######################

        frameStnId <- ttklabelframe(subfr2, text = lang.dlg[['label']][['6']], relief = 'groove')

        .cdtData$EnvData$STN$stnID <- tclVar()

        bt.stnID.prev <- ttkbutton(frameStnId, text = "<<", width = largeur7)
        bt.stnID.next <- ttkbutton(frameStnId, text = ">>", width = largeur7)
        .cdtData$EnvData$STN$cb.stnID <- ttkcombobox(frameStnId, values = "", textvariable = .cdtData$EnvData$STN$stnID, width = largeur4, justify = 'center')
        bt.display.QC <- ttkbutton(frameStnId, text = lang.dlg[['button']][['3']])

        tkgrid(bt.stnID.prev, row = 0, column = 0, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 2, ipadx = 1, ipady = 1)
        tkgrid(.cdtData$EnvData$STN$cb.stnID, row = 0, column = 1, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 2, ipadx = 1, ipady = 1)
        tkgrid(bt.stnID.next, row = 0, column = 2, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 2, ipadx = 1, ipady = 1)
        tkgrid(bt.display.QC, row = 1, column = 0, sticky = 'we', rowspan = 1, columnspan = 3, padx = 1, pady = 2, ipadx = 1, ipady = 1)

        #######################

        tkconfigure(bt.stnID.prev, command = function(){
            if(!is.null(.cdtData$EnvData$outqc)){
                STNID <- .cdtData$EnvData$outqc$stn
                istn <- which(STNID == str_trim(tclvalue(.cdtData$EnvData$STN$stnID)))
                istn <- istn - 1
                if(istn < 1) istn <- length(STNID)
                tclvalue(.cdtData$EnvData$STN$stnID) <- STNID[istn]

                ret <- try(set.date.outliers(), silent = TRUE)
                if(inherits(ret, "try-error") | is.null(ret)) return(NULL)
            }
        })

        tkconfigure(bt.stnID.next, command = function(){
            if(!is.null(.cdtData$EnvData$outqc)){
                STNID <- .cdtData$EnvData$outqc$stn
                istn <- which(STNID == str_trim(tclvalue(.cdtData$EnvData$STN$stnID)))
                istn <- istn + 1
                if(istn > length(STNID)) istn <- 1
                tclvalue(.cdtData$EnvData$STN$stnID) <- STNID[istn]

                ret <- try(set.date.outliers(), silent = TRUE)
                if(inherits(ret, "try-error") | is.null(ret)) return(NULL)
            }
        })


        tkbind(.cdtData$EnvData$STN$cb.stnID, "<<ComboboxSelected>>", function(){
            if(!is.null(.cdtData$EnvData$outqc)){
                ret <- try(set.date.outliers(), silent = TRUE)
                if(inherits(ret, "try-error") | is.null(ret)) return(NULL)
            }
        })

        #######################

        .cdtData$EnvData$tab$TableStat <- NULL

        tkconfigure(bt.display.QC, command = function(){
            stnid <- str_trim(tclvalue(.cdtData$EnvData$STN$stnID))
            if(stnid == "") return(NULL)
            donQCstat <- .cdtData$EnvData$outqc$res[[stnid]]$outliers
            tab.title <- paste0(stnid, "-QC-Output")

            .cdtData$EnvData$tab$TableStat <- tableNotebookTab_unik(donQCstat, .cdtData$EnvData$tab$TableStat, tab.title, 12, 'outqc')

            tabid <- as.integer(tclvalue(tkindex(.cdtEnv$tcl$main$tknotes, 'current'))) + 1
            .cdtData$OpenTab$Data.Type[[tabid]] <- 'qc.outliers.data'
            table1 <- .cdtData$OpenTab$Data[[tabid]][[2]][[1]]

            .Tcl(paste(table1, 'tag', 'celltag', 'ttnreplace',
                paste(1:as.integer(tclvalue(tkindex(table1, 'end', 'row'))), 8, sep = ',', collapse = ' ')))
            tcl(table1, "tag", "configure", "ttnreplace", bg = "lightgoldenrod1", fg = "blue", anchor = "c")

            .Tcl(paste(table1, 'tag', 'celltag', 'ttchgval',
                paste(1:as.integer(tclvalue(tkindex(table1, 'end', 'row'))), 9, sep = ',', collapse = ' ')))
            tcl(table1, "tag", "configure", "ttchgval", bg = "darkolivegreen1", fg = "red", anchor = "c")

            menuCopyPaste.OpenTable()
        })

        #######################

        frameStnMon <- ttklabelframe(subfr2, text = lang.dlg[['label']][['7']], relief = 'groove')

        .cdtData$EnvData$STN$month <- tclVar(MOIS[1])

        frTSMON <- tkframe(frameStnMon)
        cb.Mon <- ttkcombobox(frTSMON, values = MOIS, textvariable = .cdtData$EnvData$STN$month, width = largeur3, justify = 'center')

        bt.Mon.prev <- ttkbutton(frameStnMon, text = "<<", width = largeur5)
        bt.Mon <- ttkbutton(frameStnMon, text = .cdtEnv$tcl$lang$global[['button']][['3']], width = largeur5)
        bt.Mon.next <- ttkbutton(frameStnMon, text = ">>", width = largeur5)

        ########
        tkgrid(cb.Mon, row = 0, column = 0, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)

        tkgrid(frTSMON, row = 0, column = 0, sticky = '', rowspan = 1, columnspan = 3, padx = 1, pady = 1, ipadx = 1, ipady = 1)
        tkgrid(bt.Mon.prev, row = 1, column = 0, sticky = 'we', rowspan = 1, columnspan = 1, padx = 2, pady = 1, ipadx = 1, ipady = 1)
        tkgrid(bt.Mon, row = 1, column = 1, sticky = 'we', rowspan = 1, columnspan = 1, padx = 2, pady = 1, ipadx = 1, ipady = 1)
        tkgrid(bt.Mon.next, row = 1, column = 2, sticky = 'we', rowspan = 1, columnspan = 1, padx = 2, pady = 1, ipadx = 1, ipady = 1)

        helpWidget(frameStnMon, lang.dlg[['tooltip']][['7']], lang.dlg[['status']][['7']])

        ########

        .cdtData$EnvData$tab$PlotMon <- NULL

        tkconfigure(bt.Mon.prev, command = function(){
            imois <- which(MOIS == str_trim(tclvalue(.cdtData$EnvData$STN$month)))
            imois <- imois - 1
            if(imois < 1) imois <- 12
            tclvalue(.cdtData$EnvData$STN$month) <- MOIS[imois]

            stnid <- str_trim(tclvalue(.cdtData$EnvData$STN$stnID))
            if(stnid == "") return(NULL)
            tab.title <- paste0(stnid, "-Outliers")

            imgContainer <- qcDislpay_Outliers.Mon(.cdtData$EnvData$tab$PlotMon, tab.title)
            .cdtData$EnvData$tab$PlotMon <- imageNotebookTab_unik(imgContainer, .cdtData$EnvData$tab$PlotMon)
        })

        tkconfigure(bt.Mon.next, command = function(){
            imois <- which(MOIS == str_trim(tclvalue(.cdtData$EnvData$STN$month)))
            imois <- imois + 1
            if(imois > 12) imois <- 1
            tclvalue(.cdtData$EnvData$STN$month) <- MOIS[imois]

            stnid <- str_trim(tclvalue(.cdtData$EnvData$STN$stnID))
            if(stnid == "") return(NULL)
            tab.title <- paste0(stnid, "-Outliers")

            imgContainer <- qcDislpay_Outliers.Mon(.cdtData$EnvData$tab$PlotMon, tab.title)
            .cdtData$EnvData$tab$PlotMon <- imageNotebookTab_unik(imgContainer, .cdtData$EnvData$tab$PlotMon)
        })

        tkconfigure(bt.Mon, command = function(){
            stnid <- str_trim(tclvalue(.cdtData$EnvData$STN$stnID))
            if(stnid == "") return(NULL)
            tab.title <- paste0(stnid, "-Outliers")

            imgContainer <- qcDislpay_Outliers.Mon(.cdtData$EnvData$tab$PlotMon, tab.title)
            .cdtData$EnvData$tab$PlotMon <- imageNotebookTab_unik(imgContainer, .cdtData$EnvData$tab$PlotMon)
        })

        #######################

        frameQCSP <- ttklabelframe(subfr2, text = lang.dlg[['label']][['8']], relief = 'groove')

        .cdtData$EnvData$STN$dateSP <- tclVar()

        frQCSP <- tkframe(frameQCSP)
        .cdtData$EnvData$STN$cb.QCSP <- ttkcombobox(frQCSP, values = "", textvariable = .cdtData$EnvData$STN$dateSP, width = largeur6, justify = 'center')
        bt.QCSP.opt <- ttkbutton(frQCSP, text = .cdtEnv$tcl$lang$global[['button']][['4']], width = largeur5)

        bt.QCSP.prev <- ttkbutton(frameQCSP, text = "<<", width = largeur5)
        bt.QCSP.plot <- ttkbutton(frameQCSP, text = .cdtEnv$tcl$lang$global[['button']][['3']], width = largeur5)
        bt.QCSP.next <- ttkbutton(frameQCSP, text = ">>", width = largeur5)

        ######
        tkgrid(.cdtData$EnvData$STN$cb.QCSP, row = 0, column = 0, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
        tkgrid(bt.QCSP.opt, row = 0, column = 1, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)

        tkgrid(frQCSP, row = 0, column = 0, sticky = '', rowspan = 1, columnspan = 3, padx = 1, pady = 1, ipadx = 1, ipady = 1)
        tkgrid(bt.QCSP.prev, row = 1, column = 0, sticky = 'we', rowspan = 1, columnspan = 1, padx = 2, pady = 1, ipadx = 1, ipady = 1)
        tkgrid(bt.QCSP.plot, row = 1, column = 1, sticky = 'we', rowspan = 1, columnspan = 1, padx = 2, pady = 1, ipadx = 1, ipady = 1)
        tkgrid(bt.QCSP.next, row = 1, column = 2, sticky = 'we', rowspan = 1, columnspan = 1, padx = 2, pady = 1, ipadx = 1, ipady = 1)

        helpWidget(frameQCSP, lang.dlg[['tooltip']][['8']], lang.dlg[['status']][['8']])

        ##########

        tkconfigure(bt.QCSP.opt, command = function(){
            .cdtData$EnvData$STN$Opt <- MapGraph.QCoutliersSP(.cdtData$EnvData$STN$Opt)
        })

        ##########

        .cdtData$EnvData$tab$spoutliers <- NULL

        tkconfigure(bt.QCSP.prev, command = function(){
            idaty <- str_trim(tclvalue(.cdtData$EnvData$STN$dateSP))
            if(idaty == "") return(NULL)
            stnid <- str_trim(tclvalue(.cdtData$EnvData$STN$stnID))
            if(stnid == "") return(NULL)

            dateSP <- .cdtData$EnvData$outqc$res[[stnid]]$date
            idaty <- which(dateSP == idaty)
            idaty <- idaty - 1
            if(idaty < 1) idaty <- length(dateSP)
            tclvalue(.cdtData$EnvData$STN$dateSP) <- dateSP[idaty]

            tab.title <- paste0(stnid, "-Spatial.Check")
            imgContainer <- CDT.Display.Points.Zoom(qcPlot_Spatial.Check, .cdtData$EnvData$tab$spoutliers, tab.title)
            .cdtData$EnvData$tab$spoutliers <- imageNotebookTab_unik(imgContainer, .cdtData$EnvData$tab$spoutliers)
        })

        tkconfigure(bt.QCSP.next, command = function(){
            idaty <- str_trim(tclvalue(.cdtData$EnvData$STN$dateSP))
            if(idaty == "") return(NULL)
            stnid <- str_trim(tclvalue(.cdtData$EnvData$STN$stnID))
            if(stnid == "") return(NULL)

            dateSP <- .cdtData$EnvData$outqc$res[[stnid]]$date
            idaty <- which(dateSP == idaty)
            idaty <- idaty + 1
            if(idaty > length(dateSP)) idaty <- 1
            tclvalue(.cdtData$EnvData$STN$dateSP) <- dateSP[idaty]

            tab.title <- paste0(stnid, "-Spatial.Check")
            imgContainer <- CDT.Display.Points.Zoom(qcPlot_Spatial.Check, .cdtData$EnvData$tab$spoutliers, tab.title)
            .cdtData$EnvData$tab$spoutliers <- imageNotebookTab_unik(imgContainer, .cdtData$EnvData$tab$spoutliers)
        })

        tkconfigure(bt.QCSP.plot, command = function(){
            idaty <- str_trim(tclvalue(.cdtData$EnvData$STN$dateSP))
            if(idaty == "") return(NULL)
            stnid <- str_trim(tclvalue(.cdtData$EnvData$STN$stnID))
            if(stnid == "") return(NULL)

            tab.title <- paste0(stnid, "-Spatial.Check")
            imgContainer <- CDT.Display.Points.Zoom(qcPlot_Spatial.Check, .cdtData$EnvData$tab$spoutliers, tab.title)
            .cdtData$EnvData$tab$spoutliers <- imageNotebookTab_unik(imgContainer, .cdtData$EnvData$tab$spoutliers)
        })

        #######################

        bt.replace.QC <- ttkbutton(subfr2, text = lang.dlg[['button']][['4']])
        helpWidget(bt.replace.QC, lang.dlg[['tooltip']][['9']], lang.dlg[['status']][['9']])

        tkconfigure(bt.replace.QC, command = function(){
            if(is.null(.cdtData$EnvData$outqc)) return(NULL)

            is.elv <- if(is.null(.cdtData$EnvData$stn.data$elv)) 3 else 4
            info <- .cdtData$EnvData$output$info[[3]]

            file.stn <- file.path(.cdtData$EnvData$PathData, 'CDTSTATIONS', .cdtData$EnvData$output$info[[1]])
            tmp <- read.table(file.stn, header = FALSE, sep = info$sepr, stringsAsFactors = FALSE, colClasses = "character")

            stnid <- str_trim(tclvalue(.cdtData$EnvData$STN$stnID))
            istn <- which(.cdtData$EnvData$stn.data$id == stnid) + 1
            outliers <- .cdtData$EnvData$outqc$res[[stnid]]$outliers

            daty <- str_trim(as.character(outliers$DATE))
            nonNA <- !is.na(daty) & daty != ""
            outliers <- outliers[nonNA, , drop = FALSE]
            daty <- daty[nonNA]
            idaty <- which(.cdtData$EnvData$stn.data$dates %in% daty) + is.elv

            stn.val <- as.numeric(as.character(outliers$STN.VAL))
            not.replace <- as.numeric(as.character(outliers$NOT.REPLACE))
            to.replace <- as.numeric(as.character(outliers$REPLACE.VAL))
            ina <- is.na(not.replace)
            stn.val[ina] <- info$miss.val
            nna <- !is.na(to.replace)
            stn.val[nna] <- to.replace[nna]
            tmp[idaty, istn] <- stn.val

            write.table(tmp, file.stn, sep = info$sepr, na = info$miss.val, row.names = FALSE, col.names = FALSE, quote = FALSE)
            rm(tmp); gc()
            Insert.Messages.Out(paste(stnid, ':',  lang.dlg[['message']][['10']]), TRUE, "s")
        })

        #########################################

        tkgrid(frameOutQC, row = 0, column = 0, sticky = 'we', padx = 1, pady = 1, ipadx = 1, ipady = 1)
        tkgrid(frameStnId, row = 1, column = 0, sticky = 'we', padx = 1, pady = 1, ipadx = 1, ipady = 1)
        tkgrid(frameStnMon, row = 2, column = 0, sticky = 'we', padx = 1, pady = 3, ipadx = 1, ipady = 1)
        tkgrid(frameQCSP, row = 3, column = 0, sticky = 'we', padx = 1, pady = 3, ipadx = 1, ipady = 1)
        tkgrid(bt.replace.QC, row = 4, column = 0, sticky = 'we', padx = 1, pady = 5, ipadx = 1, ipady = 1)

    #######################################################################################################

    #Tab2a
    subfr2a <- bwTabScrollableFrame(cmd.tab2a)

        ##############################################

        frameStnId.MXD <- ttklabelframe(subfr2a, text = lang.dlg[['label']][['11']], relief = 'groove')

        .cdtData$EnvData$MXD$STN$stnID <- tclVar()

        bt.MxstnID.prev <- ttkbutton(frameStnId.MXD, text = "<<", width = largeur7)
        bt.MxstnID.next <- ttkbutton(frameStnId.MXD, text = ">>", width = largeur7)
        .cdtData$EnvData$MXD$STN$cb.stnID <- ttkcombobox(frameStnId.MXD, values = "", textvariable = .cdtData$EnvData$MXD$STN$stnID, width = largeur4, justify = 'center')
        bt.display.MXD <- ttkbutton(frameStnId.MXD, text = lang.dlg[['button']][['5']])
        bt.replace.MXD <- ttkbutton(frameStnId.MXD, text = lang.dlg[['button']][['6']])

        tkgrid(bt.MxstnID.prev, row = 0, column = 0, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 2, ipadx = 1, ipady = 1)
        tkgrid(.cdtData$EnvData$MXD$STN$cb.stnID, row = 0, column = 1, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 2, ipadx = 1, ipady = 1)
        tkgrid(bt.MxstnID.next, row = 0, column = 2, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 2, ipadx = 1, ipady = 1)
        tkgrid(bt.display.MXD, row = 1, column = 0, sticky = 'we', rowspan = 1, columnspan = 3, padx = 1, pady = 1, ipadx = 1, ipady = 1)
        tkgrid(bt.replace.MXD, row = 2, column = 0, sticky = 'we', rowspan = 1, columnspan = 3, padx = 1, pady = 1, ipadx = 1, ipady = 1)

        #######################

        tkconfigure(bt.MxstnID.prev, command = function(){
            if(!is.null(.cdtData$EnvData$outqc$mixed)){
                STNID <- .cdtData$EnvData$outqc$mixed$stn
                istn <- which(STNID == str_trim(tclvalue(.cdtData$EnvData$MXD$STN$stnID)))
                istn <- istn - 1
                if(istn < 1) istn <- length(STNID)
                tclvalue(.cdtData$EnvData$MXD$STN$stnID) <- STNID[istn]
            }
        })

        tkconfigure(bt.MxstnID.next, command = function(){
            if(!is.null(.cdtData$EnvData$outqc$mixed)){
                STNID <- .cdtData$EnvData$outqc$mixed$stn
                istn <- which(STNID == str_trim(tclvalue(.cdtData$EnvData$MXD$STN$stnID)))
                istn <- istn + 1
                if(istn > length(STNID)) istn <- 1
                tclvalue(.cdtData$EnvData$MXD$STN$stnID) <- STNID[istn]
            }
        })

        #######################
        .cdtData$EnvData$tab$TableMXD <- NULL

        tkconfigure(bt.display.MXD, command = function(){
            stnid <- str_trim(tclvalue(.cdtData$EnvData$MXD$STN$stnID))
            if(stnid == "") return(NULL)
            donMXD <- .cdtData$EnvData$outqc$mixed$res[[stnid]]$tab
            tab.title <- paste0(stnid, "-Data-Mixed")

            .cdtData$EnvData$tab$TableMXD <- tableNotebookTab_unik(donMXD, .cdtData$EnvData$tab$TableMXD, tab.title, 10, 'outqc')

            tabid <- as.integer(tclvalue(tkindex(.cdtEnv$tcl$main$tknotes, 'current'))) + 1
            .cdtData$OpenTab$Data.Type[[tabid]] <- 'qc.mixed.data'
            table1 <- .cdtData$OpenTab$Data[[tabid]][[2]][[1]]

            .Tcl(paste(table1, 'tag', 'celltag', 'idxreplace1',
                paste(1:as.integer(tclvalue(tkindex(table1, 'end', 'row'))), 4, sep = ',', collapse = ' ')))
            tcl(table1, "tag", "configure", "idxreplace1", bg = "lightgoldenrod1", fg = "blue", anchor = "c")

            .Tcl(paste(table1, 'tag', 'celltag', 'idxchgvalues1',
                paste(1:as.integer(tclvalue(tkindex(table1, 'end', 'row'))), 5, sep = ',', collapse = ' ')))
            tcl(table1, "tag", "configure", "idxchgvalues1", bg = "darkolivegreen1", fg = "red", anchor = "c")

            menuCopyPaste.OpenTable()
        })

        #####
        tkconfigure(bt.replace.MXD, command = function(){
            if(is.null(.cdtData$EnvData$outqc)) return(NULL)
            stnid <- str_trim(tclvalue(.cdtData$EnvData$MXD$STN$stnID))
            if(stnid == "") return(NULL)
            tmpqc <- .cdtData$EnvData$outqc$mixed$res[[stnid]]$tab

            istn <- which(.cdtData$EnvData$stn.data$id == stnid) + 1

            is.elv <- if(is.null(.cdtData$EnvData$stn.data$elv)) 3 else 4
            info <- .cdtData$EnvData$output$info[[3]]

            file.stn <- file.path(.cdtData$EnvData$PathData, 'CDTSTATIONS', .cdtData$EnvData$output$info[[1]])
            tmpstn <- read.table(file.stn, header = FALSE, sep = info$sepr, stringsAsFactors = FALSE, colClasses = "character")

            daty <- str_trim(as.character(tmpqc$DATE))
            nonNA <- !is.na(daty) & daty != ""
            tmpqc <- tmpqc[nonNA, , drop = FALSE]
            daty <- daty[nonNA]
            idaty <- which(.cdtData$EnvData$stn.data$dates %in% daty) + is.elv

            stn.val <- as.numeric(as.character(tmpqc$STN.VAL))
            not.replace <- as.numeric(as.character(tmpqc$NOT.REPLACE))
            to.replace <- as.numeric(as.character(tmpqc$REPLACE.VAL))
            ina <- is.na(not.replace)
            stn.val[ina] <- info$miss.val
            nna <- !is.na(to.replace)
            stn.val[nna] <- to.replace[nna]
            tmpstn[idaty, istn] <- stn.val

            write.table(tmpstn, file.stn, sep = info$sepr, na = info$miss.val, row.names = FALSE, col.names = FALSE, quote = FALSE)
            rm(tmpstn); gc()
            Insert.Messages.Out(paste(stnid, ':',  lang.dlg[['message']][['10']]), TRUE, "s")
        })

        #########################################

        frameStnId.SEQ <- ttklabelframe(subfr2a, text = lang.dlg[['label']][['12']], relief = 'groove')

        .cdtData$EnvData$SEQ$STN$stnID <- tclVar()

        bt.SqstnID.prev <- ttkbutton(frameStnId.SEQ, text = "<<", width = largeur7)
        bt.SqstnID.next <- ttkbutton(frameStnId.SEQ, text = ">>", width = largeur7)
        .cdtData$EnvData$SEQ$STN$cb.stnID <- ttkcombobox(frameStnId.SEQ, values = "", textvariable = .cdtData$EnvData$SEQ$STN$stnID, width = largeur4, justify = 'center')
        bt.display.SEQ <- ttkbutton(frameStnId.SEQ, text = lang.dlg[['button']][['5']])
        bt.replace.SEQ <- ttkbutton(frameStnId.SEQ, text = lang.dlg[['button']][['7']])

        tkgrid(bt.SqstnID.prev, row = 0, column = 0, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 2, ipadx = 1, ipady = 1)
        tkgrid(.cdtData$EnvData$SEQ$STN$cb.stnID, row = 0, column = 1, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 2, ipadx = 1, ipady = 1)
        tkgrid(bt.SqstnID.next, row = 0, column = 2, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 2, ipadx = 1, ipady = 1)
        tkgrid(bt.display.SEQ, row = 1, column = 0, sticky = 'we', rowspan = 1, columnspan = 3, padx = 1, pady = 1, ipadx = 1, ipady = 1)
        tkgrid(bt.replace.SEQ, row = 2, column = 0, sticky = 'we', rowspan = 1, columnspan = 3, padx = 1, pady = 1, ipadx = 1, ipady = 1)

        #######################

        tkconfigure(bt.SqstnID.prev, command = function(){
            if(!is.null(.cdtData$EnvData$outqc$sequence)){
                STNID <- .cdtData$EnvData$outqc$sequence$stn
                istn <- which(STNID == str_trim(tclvalue(.cdtData$EnvData$SEQ$STN$stnID)))
                istn <- istn - 1
                if(istn < 1) istn <- length(STNID)
                tclvalue(.cdtData$EnvData$SEQ$STN$stnID) <- STNID[istn]
            }
        })

        tkconfigure(bt.SqstnID.next, command = function(){
            if(!is.null(.cdtData$EnvData$outqc$sequence)){
                STNID <- .cdtData$EnvData$outqc$sequence$stn
                istn <- which(STNID == str_trim(tclvalue(.cdtData$EnvData$SEQ$STN$stnID)))
                istn <- istn + 1
                if(istn > length(STNID)) istn <- 1
                tclvalue(.cdtData$EnvData$SEQ$STN$stnID) <- STNID[istn]
            }
        })

        #######################
        .cdtData$EnvData$tab$TableSEQ <- NULL

        tkconfigure(bt.display.SEQ, command = function(){
            stnid <- str_trim(tclvalue(.cdtData$EnvData$SEQ$STN$stnID))
            if(stnid == "") return(NULL)
            donSEQ <- .cdtData$EnvData$outqc$sequence$res[[stnid]]$tab
            tab.title <- paste0(stnid, "-Data-Invalid")

            .cdtData$EnvData$tab$TableSEQ <- tableNotebookTab_unik(donSEQ, .cdtData$EnvData$tab$TableSEQ, tab.title, 10, 'outqc')

            tabid <- as.integer(tclvalue(tkindex(.cdtEnv$tcl$main$tknotes, 'current'))) + 1
            .cdtData$OpenTab$Data.Type[[tabid]] <- 'qc.sequence.data'
            table1 <- .cdtData$OpenTab$Data[[tabid]][[2]][[1]]

            .Tcl(paste(table1, 'tag', 'celltag', 'idxreplace2',
                paste(1:as.integer(tclvalue(tkindex(table1, 'end', 'row'))), 4, sep = ',', collapse = ' ')))
            tcl(table1, "tag", "configure", "idxreplace2", bg = "lightgoldenrod1", fg = "blue", anchor = "c")

            .Tcl(paste(table1, 'tag', 'celltag', 'idxchgvalues2',
                paste(1:as.integer(tclvalue(tkindex(table1, 'end', 'row'))), 5, sep = ',', collapse = ' ')))
            tcl(table1, "tag", "configure", "idxchgvalues2", bg = "darkolivegreen1", fg = "red", anchor = "c")

            menuCopyPaste.OpenTable()
        })

        #####
        tkconfigure(bt.replace.SEQ, command = function(){
            if(is.null(.cdtData$EnvData$outqc)) return(NULL)
            stnid <- str_trim(tclvalue(.cdtData$EnvData$SEQ$STN$stnID))
            if(stnid == "") return(NULL)
            tmpqc <- .cdtData$EnvData$outqc$sequence$res[[stnid]]$tab

            istn <- which(.cdtData$EnvData$stn.data$id == stnid) + 1

            is.elv <- if(is.null(.cdtData$EnvData$stn.data$elv)) 3 else 4
            info <- .cdtData$EnvData$output$info[[3]]

            file.stn <- file.path(.cdtData$EnvData$PathData, 'CDTSTATIONS', .cdtData$EnvData$output$info[[1]])
            tmpstn <- read.table(file.stn, header = FALSE, sep = info$sepr, stringsAsFactors = FALSE, colClasses = "character")

            daty <- str_trim(as.character(tmpqc$DATE))
            nonNA <- !is.na(daty) & daty != ""
            tmpqc <- tmpqc[nonNA, , drop = FALSE]
            daty <- daty[nonNA]
            idaty <- which(.cdtData$EnvData$stn.data$dates %in% daty) + is.elv

            stn.val <- as.numeric(as.character(tmpqc$STN.VAL))
            not.replace <- as.numeric(as.character(tmpqc$NOT.REPLACE))
            to.replace <- as.numeric(as.character(tmpqc$REPLACE.VAL))
            ina <- is.na(not.replace)
            stn.val[ina] <- info$miss.val
            nna <- !is.na(to.replace)
            stn.val[nna] <- to.replace[nna]
            tmpstn[idaty, istn] <- stn.val

            write.table(tmpstn, file.stn, sep = info$sepr, na = info$miss.val, row.names = FALSE, col.names = FALSE, quote = FALSE)
            rm(tmpstn); gc()
            Insert.Messages.Out(paste(stnid, ':',  lang.dlg[['message']][['10']]), TRUE, "s")
        })

        #########################################

        tkgrid(frameStnId.MXD, row = 0, column = 0, sticky = 'we', padx = 1, pady = 1, ipadx = 1, ipady = 1)
        tkgrid(frameStnId.SEQ, row = 1, column = 0, sticky = 'we', padx = 1, pady = 2, ipadx = 1, ipady = 1)

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

        frameZoom <- ttklabelframe(subfr3, text = lang.dlg[['label']][['9']], relief = 'groove')

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

        #####
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

        helpWidget(.cdtData$EnvData$zoom$btZoomP, lang.dlg[['tooltip']][['10']], lang.dlg[['status']][['10']])
        helpWidget(.cdtData$EnvData$zoom$btZoomM, lang.dlg[['tooltip']][['11']], lang.dlg[['status']][['11']])
        helpWidget(.cdtData$EnvData$zoom$btZoomRect, lang.dlg[['tooltip']][['12']], lang.dlg[['status']][['12']])
        helpWidget(.cdtData$EnvData$zoom$btPanImg, lang.dlg[['tooltip']][['13']], lang.dlg[['status']][['13']])
        helpWidget(.cdtData$EnvData$zoom$btRedraw, lang.dlg[['tooltip']][['14']], lang.dlg[['status']][['14']])
        helpWidget(.cdtData$EnvData$zoom$btReset, lang.dlg[['tooltip']][['15']], lang.dlg[['status']][['15']])

        #################
        tkconfigure(.cdtData$EnvData$zoom$btRedraw, command = function(){
            .cdtData$EnvData$ZoomXYval <- as.numeric(c(tclvalue(.cdtData$EnvData$zoom$xx1), tclvalue(.cdtData$EnvData$zoom$xx2),
                                                    tclvalue(.cdtData$EnvData$zoom$yy1), tclvalue(.cdtData$EnvData$zoom$yy2)))

            # ZoomXYval
            tabid <- as.numeric(tclvalue(tkindex(.cdtEnv$tcl$main$tknotes, 'current'))) + 1
            if(length(.cdtData$OpenTab$Type) > 0){
                if(.cdtData$OpenTab$Type[[tabid]] == "img" & !is.null(.cdtData$EnvData$tab$spoutliers))
                {
                    if(.cdtData$OpenTab$Data[[tabid]][[1]][[1]]$ID == .cdtData$EnvData$tab$spoutliers[[2]])
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
                if(.cdtData$OpenTab$Type[[tabid]] == "img" & !is.null(.cdtData$EnvData$tab$spoutliers))
                {
                    if(.cdtData$OpenTab$Data[[tabid]][[1]][[1]]$ID == .cdtData$EnvData$tab$spoutliers[[2]])
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
            initXYval0 <<- str_trim(c(tclvalue(.cdtData$EnvData$zoom$xx1), tclvalue(.cdtData$EnvData$zoom$xx2),
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
            initXYval1 <- str_trim(c(tclvalue(.cdtData$EnvData$zoom$xx1), tclvalue(.cdtData$EnvData$zoom$xx2),
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

    #Tab4
    subfr4 <- bwTabScrollableFrame(cmd.tab4)

        ##############################################

        frameSHP <- tkframe(subfr4, relief = 'groove', borderwidth = 2)

        .cdtData$EnvData$shp$add.shp <- tclVar(FALSE)
        file.plotShp <- tclVar()
        stateSHP <- "disabled"

        chk.addshp <- tkcheckbutton(frameSHP, variable = .cdtData$EnvData$shp$add.shp, text = lang.dlg[['checkbutton']][['2']], anchor = 'w', justify = 'left')
        bt.addshpOpt <- ttkbutton(frameSHP, text = .cdtEnv$tcl$lang$global[['button']][['4']], state = stateSHP)
        cb.addshp <- ttkcombobox(frameSHP, values = unlist(listOpenFiles), textvariable = file.plotShp, width = largeur1, state = stateSHP)
        bt.addshp <- tkbutton(frameSHP, text = "...", state = stateSHP)

        ########
        tkgrid(chk.addshp, row = 0, column = 0, sticky = 'we', rowspan = 1, columnspan = 6, padx = 1, pady = 1)
        tkgrid(bt.addshpOpt, row = 0, column = 6, sticky = 'we', rowspan = 1, columnspan = 2, padx = 1, pady = 1)
        tkgrid(cb.addshp, row = 1, column = 0, sticky = 'we', rowspan = 1, columnspan = 7, padx = 1, pady = 1)
        tkgrid(bt.addshp, row = 1, column = 7, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1)

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

        #######################

        frameDEM <- tkframe(subfr4, relief = 'groove', borderwidth = 2)

        .cdtData$EnvData$dem$add.dem <- tclVar(FALSE)
        file.plotDem <- tclVar()
        stateDEM <- "disabled"

        chk.adddem <- tkcheckbutton(frameDEM, variable = .cdtData$EnvData$dem$add.dem, text = lang.dlg[['checkbutton']][['3']], anchor = 'w', justify = 'left')
        bt.adddemOpt <- ttkbutton(frameDEM, text = .cdtEnv$tcl$lang$global[['button']][['4']], state = stateDEM)
        cb.adddem <- ttkcombobox(frameDEM, values = unlist(listOpenFiles), textvariable = file.plotDem, width = largeur1, state = stateDEM)
        bt.adddem <- tkbutton(frameDEM, text = "...", state = stateDEM)

        ########

        tkgrid(chk.adddem, row = 0, column = 0, sticky = 'we', rowspan = 1, columnspan = 6, padx = 1, pady = 1)
        tkgrid(bt.adddemOpt, row = 0, column = 6, sticky = 'we', rowspan = 1, columnspan = 2, padx = 1, pady = 1)
        tkgrid(cb.adddem, row = 1, column = 0, sticky = 'we', rowspan = 1, columnspan = 7, padx = 1, pady = 1)
        tkgrid(bt.adddem, row = 1, column = 7, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1)

        ########

        tkconfigure(bt.adddem, command = function(){
            nc.opfiles <- getOpenNetcdf(.cdtEnv$tcl$main$win, initialdir = getwd())
            if(!is.null(nc.opfiles)){
                update.OpenFiles('netcdf', nc.opfiles)
                listOpenFiles[[length(listOpenFiles) + 1]] <<- nc.opfiles[[1]]
                tclvalue(file.plotDem) <- nc.opfiles[[1]]
                tkconfigure(cb.adddem, values = unlist(listOpenFiles))

                demInfo <- getNCDFSampleData(str_trim(tclvalue(file.plotDem)))
                if(is.null(demInfo)){
                    Insert.Messages.Out(lang.dlg[['message']][['7']], TRUE, "e")
                    tclvalue(file.plotDem) <- ""
                    .cdtData$EnvData$dem$dem <- NULL
                }else{
                    jfile <- getIndex.AllOpenFiles(str_trim(tclvalue(file.plotDem)))
                    ncdata <- .cdtData$OpenFiles$Data[[jfile]][[2]]
                    .cdtData$EnvData$dem$dem <- ncdata[c('x', 'y', 'z')]
                }
            }
        })

        tkconfigure(bt.adddemOpt, command = function(){
            if(!is.null(.cdtData$EnvData$dem$dem)){
                atlevel <- pretty(.cdtData$EnvData$dem$dem$z, n = 10, min.n = 7)
                if(is.null(.cdtData$EnvData$dem$Opt$user.levels$levels)){
                    .cdtData$EnvData$dem$Opt$user.levels$levels <- atlevel
                }else{
                    if(!.cdtData$EnvData$dem$Opt$user.levels$custom)
                        .cdtData$EnvData$dem$Opt$user.levels$levels <- atlevel
                }
            }

            .cdtData$EnvData$dem$Opt <- MapGraph.gridDataLayer(.cdtData$EnvData$dem$Opt)
        })

        ########
        tkbind(cb.adddem, "<<ComboboxSelected>>", function(){
            demInfo <- getNCDFSampleData(str_trim(tclvalue(file.plotDem)))
            if(is.null(demInfo)){
                Insert.Messages.Out(lang.dlg[['message']][['7']], TRUE, "e")
                .cdtData$EnvData$dem$dem <- NULL
            }else{
                jfile <- getIndex.AllOpenFiles(str_trim(tclvalue(file.plotDem)))
                ncdata <- .cdtData$OpenFiles$Data[[jfile]][[2]]
                .cdtData$EnvData$dem$dem <- ncdata[c('x', 'y', 'z')]
            }
        })

        tkbind(chk.adddem, "<Button-1>", function(){
            stateDEM <- if(tclvalue(.cdtData$EnvData$dem$add.dem) == "1") "disabled" else "normal"
            tkconfigure(cb.adddem, state = stateDEM)
            tkconfigure(bt.adddem, state = stateDEM)
            tkconfigure(bt.adddemOpt, state = stateDEM)
        })

        #######################

        frameSAT <- tkframe(subfr4, relief = 'groove', borderwidth = 2)

        .cdtData$EnvData$sat$add.sat <- tclVar(FALSE)
        dir.plotSat <- tclVar()
        stateSAT <- "disabled"

        chk.addSat <- tkcheckbutton(frameSAT, variable = .cdtData$EnvData$sat$add.sat, text = lang.dlg[['checkbutton']][['4']], anchor = 'w', justify = 'left')
        bt.addSatOpt <- ttkbutton(frameSAT, text = .cdtEnv$tcl$lang$global[['button']][['4']], state = stateSAT)
        txt.addSat <- tklabel(frameSAT, text = lang.dlg[['label']][['13']], anchor = 'w', justify = 'left')
        bt.addSatSet <- ttkbutton(frameSAT, text = .cdtEnv$tcl$lang$global[['button']][['5']], state = stateSAT)
        en.addSat <- tkentry(frameSAT, textvariable = dir.plotSat, width = largeur2, state = stateSAT)
        bt.addSat <- tkbutton(frameSAT, text = "...", state = stateSAT)

        ########
        tkgrid(chk.addSat, row = 0, column = 0, sticky = 'we', rowspan = 1, columnspan = 6, padx = 1, pady = 1)
        tkgrid(bt.addSatOpt, row = 0, column = 6, sticky = 'we', rowspan = 1, columnspan = 2, padx = 1, pady = 1)
        tkgrid(txt.addSat, row = 1, column = 0, sticky = 'we', rowspan = 1, columnspan = 6, padx = 1, pady = 1)
        tkgrid(bt.addSatSet, row = 1, column = 6, sticky = 'we', rowspan = 1, columnspan = 2, padx = 1, pady = 1)
        tkgrid(en.addSat, row = 2, column = 0, sticky = 'we', rowspan = 1, columnspan = 7, padx = 1, pady = 1)
        tkgrid(bt.addSat, row = 2, column = 7, sticky = 'we', rowspan = 1, columnspan = 1, padx = 0, pady = 1)

        ########

        tkconfigure(bt.addSat, command = function(){
            dirnc <- tk_choose.dir(getwd(), "")
            tclvalue(dir.plotSat) <- if(dirnc %in% c("", "NA") | is.na(dirnc)) "" else dirnc

            .cdtData$EnvData$sat$dir <- str_trim(tclvalue(dir.plotSat))
        })

        tkconfigure(bt.addSatSet, command = function(){
            tstep <- .cdtData$EnvData$output$params$intstep
            TSTEPVAL0 <- .cdtEnv$tcl$lang$global[['combobox']][['1']][3:6]
            periodVAL <- c('daily', 'pentad', 'dekadal', 'monthly')
            timeSteps <- if(is.null(tstep)) TSTEPVAL0[1] else TSTEPVAL0[periodVAL %in% tstep]

            .cdtData$EnvData$sat <- getInfoNetCDFData(.cdtEnv$tcl$main$win,
                                                      .cdtData$EnvData$sat,
                                                      str_trim(tclvalue(dir.plotSat)))

            sdon <- getNCDFSampleData(.cdtData$EnvData$sat$sample)
            if(is.null(sdon)){
                Insert.Messages.Out(lang.dlg[['message']][['8']], TRUE, "e")
                .cdtData$EnvData$sat$sat.data <- NULL
            }else .cdtData$EnvData$sat$sat.data <- sdon
        })

        tkconfigure(bt.addSatOpt, command = function(){
            if(!is.null(.cdtData$EnvData$sat$don.sat)){
                atlevel <- pretty(.cdtData$EnvData$sat$don.sat$z, n = 10, min.n = 7)
                if(is.null(.cdtData$EnvData$sat$Opt$user.levels$levels)){
                    .cdtData$EnvData$sat$Opt$user.levels$levels <- atlevel
                }else{
                    if(!.cdtData$EnvData$sat$Opt$user.levels$custom)
                        .cdtData$EnvData$sat$Opt$user.levels$levels <- atlevel
                }
            }

            .cdtData$EnvData$sat$Opt <- MapGraph.gridDataLayer(.cdtData$EnvData$sat$Opt)
        })

        ########
        tkbind(chk.addSat, "<Button-1>", function(){
            stateSAT <- if(tclvalue(.cdtData$EnvData$sat$add.sat) == "1") "disabled" else "normal"
            tkconfigure(en.addSat, state = stateSAT)
            tkconfigure(bt.addSat, state = stateSAT)
            tkconfigure(bt.addSatSet, state = stateSAT)
            tkconfigure(bt.addSatOpt, state = stateSAT)
        })

        #######################

        framePlotType <- tkframe(subfr4)

        plot.type <- c("Pixels", "FilledContour")
        .cdtData$EnvData$plot.maps$plot.type <- tclVar("Pixels")

        txt.plotType <- tklabel(framePlotType, text = lang.dlg[['label']][['10']], anchor = 'e', justify = 'right')
        cb.plotType <- ttkcombobox(framePlotType, values = plot.type, textvariable = .cdtData$EnvData$plot.maps$plot.type, width = largeur3)

        tkgrid(txt.plotType, row = 0, column = 0, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
        tkgrid(cb.plotType, row = 0, column = 1, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)

        ##############################################

        tkgrid(frameSHP, row = 0, column = 0, sticky = 'we', padx = 1, pady = 1, ipadx = 1, ipady = 1)
        tkgrid(frameDEM, row = 1, column = 0, sticky = 'we', padx = 1, pady = 3, ipadx = 1, ipady = 1)
        tkgrid(frameSAT, row = 2, column = 0, sticky = 'we', padx = 1, pady = 3, ipadx = 1, ipady = 1)
        tkgrid(framePlotType, row = 3, column = 0, sticky = '', padx = 1, pady = 3, ipadx = 1, ipady = 1)

    #######################################################################################################

    set.station.id <- function(){
        STNID <- .cdtData$EnvData$outqc$stn
        tkconfigure(.cdtData$EnvData$STN$cb.stnID, values = STNID)
        tclvalue(.cdtData$EnvData$STN$stnID) <- STNID[1]

        STNID1 <- .cdtData$EnvData$outqc$mixed$stn
        if(length(STNID1) > 0){
            tkconfigure(.cdtData$EnvData$MXD$STN$cb.stnID, values = STNID1)
            tclvalue(.cdtData$EnvData$MXD$STN$stnID) <- STNID1[1]
        }

        STNID2 <- .cdtData$EnvData$outqc$sequence$stn
        if(length(STNID2) > 0){
            tkconfigure(.cdtData$EnvData$SEQ$STN$cb.stnID, values = STNID2)
            tclvalue(.cdtData$EnvData$SEQ$STN$stnID) <- STNID2[1]
        }
    }

    set.date.outliers <- function(){
        stnid <- str_trim(tclvalue(.cdtData$EnvData$STN$stnID))
        if(stnid == "") return(NULL)
        dateOUT <- .cdtData$EnvData$outqc$res[[stnid]]$date
        tkconfigure(.cdtData$EnvData$STN$cb.QCSP, values = dateOUT)
        tclvalue(.cdtData$EnvData$STN$dateSP) <- dateOUT[1]
        return(0)
    }

    set.initialize.Zoom <- function(){
        donX <- range(.cdtData$EnvData$stn.data$lon, na.rm = TRUE)
        donY <- range(.cdtData$EnvData$stn.data$lat, na.rm = TRUE)
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

    ##########
    .cdtData$EnvData$QC$SaveEdit <- function(dat2sav){
        saved <- FALSE
        if(!is.null(dat2sav)){
            if(!is.null(dat2sav$REPLACE.VAL)){
                writeData <- TRUE
                tabid <- as.integer(tclvalue(tkindex(.cdtEnv$tcl$main$tknotes, 'current'))) + 1
                if(.cdtData$OpenTab$Data.Type[[tabid]] == 'qc.outliers.data'){
                    stnid <- str_trim(tclvalue(.cdtData$EnvData$STN$stnID))
                    .cdtData$EnvData$outqc$res[[stnid]]$outliers <- dat2sav
                }
                else if(.cdtData$OpenTab$Data.Type[[tabid]] == 'qc.mixed.data'){
                    stnid <- str_trim(tclvalue(.cdtData$EnvData$MXD$STN$stnID))
                    .cdtData$EnvData$outqc$mixed$res[[stnid]]$tab <- dat2sav
                }
                else if(.cdtData$OpenTab$Data.Type[[tabid]] == 'qc.sequence.data'){
                    stnid <- str_trim(tclvalue(.cdtData$EnvData$SEQ$STN$stnID))
                    .cdtData$EnvData$outqc$sequence$res[[stnid]]$tab <- dat2sav
                }
                else if(.cdtData$OpenTab$Data.Type[[tabid]] == 'qc.equal.data'){
                    stnid <- str_trim(tclvalue(.cdtData$EnvData$EQL$STN$stnID))
                    .cdtData$EnvData$outqc$equal$res[[stnid]]$tab <- dat2sav
                }
                else writeData <- FALSE

                if(writeData){
                    file.outqc <- file.path(.cdtData$EnvData$PathData, 'CDTDATASET', "QCResults.rds")
                    saveRDS(.cdtData$EnvData$outqc, file.outqc)
                    saved <- TRUE
                }
            }
        }

        if(!saved) Insert.Messages.Out(lang.dlg[['message']][['9']], TRUE, "e")
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
