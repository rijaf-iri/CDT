
PlotCDTDatasetCmd <- function(){
    listOpenFiles <- openFile_ttkcomboList()
    if(WindowsOS()){
        largeur0 <- .cdtEnv$tcl$fun$w.widgets(33)
        largeur1 <- .cdtEnv$tcl$fun$w.widgets(31)
        largeur2 <- 23
        largeur3 <- 12
        largeur4 <- 10
    }else{
        largeur0 <- .cdtEnv$tcl$fun$w.widgets(23)
        largeur1 <- .cdtEnv$tcl$fun$w.widgets(22)
        largeur2 <- 14
        largeur3 <- 8
        largeur4 <- 6
    }

    ###################

    .cdtData$EnvData$TSGraphOp <- list(
                bar = list(
                        xlim = list(is.min = FALSE, min = "1981-1-1", is.max = FALSE, max = "2017-12-3"),
                        ylim = list(is.min = FALSE, min = 0, is.max = FALSE, max = 200),
                        axislabs = list(is.xlab = FALSE, xlab = '', is.ylab = FALSE, ylab = ''),
                        title = list(is.title = FALSE, title = '', position = 'top'),
                        colors = list(col = "darkblue")
                    ),
                line = list(
                    xlim = list(is.min = FALSE, min = "1981-1-1", is.max = FALSE, max = "2017-12-3"),
                    ylim = list(is.min = FALSE, min = 0, is.max = FALSE, max = 200),
                    axislabs = list(is.xlab = FALSE, xlab = '', is.ylab = FALSE, ylab = ''),
                    title = list(is.title = FALSE, title = '', position = 'top'),
                    plot = list(type = 'both',
                        col = list(line = "red", points = "blue"),
                        lwd = 2, cex = 1.4),
                    legend = NULL)
                )

    .cdtData$EnvData$plot.maps$data.type <- 'cdtdataset'

    ###################

    xml.dlg <- file.path(.cdtDir$dirLocal, "languages", "cdtPlot_CDTDataset_leftCmd.xml")
    lang.dlg <- cdtLanguageParse(xml.dlg, .cdtData$Config$lang.iso)
    .cdtData$EnvData$message <- lang.dlg[['message']]

    ###################

    .cdtEnv$tcl$main$cmd.frame <- tkframe(.cdtEnv$tcl$main$panel.left)

    tknote.cmd <- bwNoteBook(.cdtEnv$tcl$main$cmd.frame)

    cmd.tab1 <- bwAddTab(tknote.cmd, text = lang.dlg[['tab_title']][['1']])
    cmd.tab2 <- bwAddTab(tknote.cmd, text = lang.dlg[['tab_title']][['2']])

    bwRaiseTab(tknote.cmd, cmd.tab1)

    tkgrid.columnconfigure(cmd.tab1, 0, weight = 1)
    tkgrid.columnconfigure(cmd.tab2, 0, weight = 1)

    tkgrid.rowconfigure(cmd.tab1, 0, weight = 1)
    tkgrid.rowconfigure(cmd.tab2, 0, weight = 1)

    #######################################################################################################

    #Tab1
    subfr1 <- bwTabScrollableFrame(cmd.tab1)

        #######################

        frameData <- ttklabelframe(subfr1, text = lang.dlg[['label']][['1']], relief = 'groove')

        file.index.data <- tclVar()

        txt.cdtdata <- tklabel(frameData, text = lang.dlg[['label']][['2']], anchor = 'w', justify = 'left')
        en.cdtdata <- tkentry(frameData, textvariable = file.index.data, width = largeur0)
        bt.cdtdata <- tkbutton(frameData, text = "...")

        tkconfigure(bt.cdtdata, command = function(){
            path.rds <- tclvalue(tkgetOpenFile(initialdir = getwd(), filetypes = .cdtEnv$tcl$data$filetypes6))
            tclvalue(file.index.data) <- if(path.rds %in% c("", "NA") | is.na(path.rds)) "" else path.rds

            ret <- try(get.CDT.dataset.Idx(), silent = TRUE)
            if(inherits(ret, "try-error") | is.null(ret)) return(NULL)
        })

        tkgrid(txt.cdtdata, row = 0, column = 0, sticky = 'we', rowspan = 1, columnspan = 6, padx = 1, pady = 0, ipadx = 1, ipady = 1)
        tkgrid(en.cdtdata, row = 1, column = 0, sticky = 'we', rowspan = 1, columnspan = 5, padx = 0, pady = 0, ipadx = 1, ipady = 1)
        tkgrid(bt.cdtdata, row = 1, column = 5, sticky = 'we', rowspan = 1, columnspan = 1, padx = 0, pady = 0, ipadx = 1, ipady = 1)

        ##############################################

        frameSHP <- tkframe(subfr1, relief = 'groove', borderwidth = 2)

        shpFile <- tclVar()
        shpAttr <- tclVar()

        txt.addshp <- tklabel(frameSHP, text = lang.dlg[['label']][['3']], anchor = 'w', justify = 'left')
        cb.addshp <- ttkcombobox(frameSHP, values = unlist(listOpenFiles), textvariable = shpFile, width = largeur1)
        bt.addshp <- tkbutton(frameSHP, text = "...")

        txt.attrshp <- tklabel(frameSHP, text = lang.dlg[['label']][['4']], anchor = 'w', justify = 'left')
        cb.attrshp <- ttkcombobox(frameSHP, values = "", textvariable = shpAttr, width = largeur1)

        bt.TableAttr <- ttkbutton(frameSHP, text = lang.dlg[['button']][['1']])
        bt.MapPixel <- ttkbutton(frameSHP, text = lang.dlg[['button']][['2']])

        ########

        tkgrid(txt.addshp, row = 0, column = 0, sticky = 'we', rowspan = 1, columnspan = 8, padx = 1, pady = 1)
        tkgrid(cb.addshp, row = 1, column = 0, sticky = 'we', rowspan = 1, columnspan = 7, padx = 1, pady = 1)
        tkgrid(bt.addshp, row = 1, column = 7, sticky = 'we', rowspan = 1, columnspan = 1, padx = 0, pady = 1)

        tkgrid(txt.attrshp, row = 2, column = 0, sticky = 'we', rowspan = 1, columnspan = 8, padx = 1, pady = 1)
        tkgrid(cb.attrshp, row = 3, column = 0, sticky = 'we', rowspan = 1, columnspan = 7, padx = 1, pady = 1)

        tkgrid(bt.TableAttr, row = 4, column = 0, sticky = 'we', rowspan = 1, columnspan = 4, padx = 1, pady = 1)
        tkgrid(bt.MapPixel, row = 4, column = 4, sticky = 'we', rowspan = 1, columnspan = 8, padx = 1, pady = 1)

        ########
        tkconfigure(bt.addshp, command = function(){
            shp.opfiles <- getOpenShp(.cdtEnv$tcl$main$win)
            if(!is.null(shp.opfiles)){
                update.OpenFiles('shp', shp.opfiles)
                tclvalue(shpFile) <- shp.opfiles[[1]]
                listOpenFiles[[length(listOpenFiles) + 1]] <<- shp.opfiles[[1]]
                tkconfigure(cb.addshp, values = unlist(listOpenFiles))

                shpf <- getShpOpenData(shpFile)
                if(is.null(shpf)){
                    .cdtData$EnvData$shp$data <- NULL
                    .cdtData$EnvData$shp$ocrds <- NULL
                    return(NULL)
                }

                AttrTable <- names(shpf[[2]]@data)
                tkconfigure(cb.attrshp, values = AttrTable)
                tclvalue(shpAttr) <- AttrTable[1]

                .cdtData$EnvData$shp$data <- shpf
                .cdtData$EnvData$shp$ocrds <- getBoundaries(shpf[[2]])

                .cdtData$EnvData$plot.maps$shp$display <- TRUE
                .cdtData$EnvData$plot.maps$shp$shp <- shpf[[2]]
                .cdtData$EnvData$plot.maps$shp$field <- cb.attrshp
            }else return(NULL)

            ret <- try(get.CDT.dataset.Idx(), silent = TRUE)
            if(inherits(ret, "try-error") | is.null(ret)) return(NULL)
        })

        ########

        .cdtData$EnvData$tab$TableAttr <- NULL

        tkconfigure(bt.TableAttr, command = function(){
            shpf <- .cdtData$EnvData$shp$data
            if(!is.null(shpf))
                .cdtData$EnvData$tab$TableAttr <- tableNotebookTab_unik(shpf[[2]]@data, .cdtData$EnvData$tab$TableAttr, shpf[[1]], 10)
        })

        ########
        .cdtData$EnvData$tab$MapSelect <- NULL

        tkconfigure(bt.MapPixel, command = function(){
            if(!is.null(.cdtData$EnvData$map) |
                !is.null(.cdtData$EnvData$shp$ocrds))
                    CDTdataset.Display.Map()
        })

        ########

        tkbind(cb.addshp, "<<ComboboxSelected>>", function(){
            shpf <- getShpOpenData(shpFile)
            if(is.null(shpf)){
                .cdtData$EnvData$shp$data <- NULL
                .cdtData$EnvData$shp$ocrds <- NULL
                return(NULL)
            }

            AttrTable <- names(shpf[[2]]@data)
            tkconfigure(cb.attrshp, values = AttrTable)
            tclvalue(shpAttr) <- AttrTable[1]

            .cdtData$EnvData$shp$data <- shpf
            .cdtData$EnvData$shp$ocrds <- getBoundaries(shpf[[2]])

            .cdtData$EnvData$plot.maps$shp$display <- TRUE
            .cdtData$EnvData$plot.maps$shp$shp <- shpf[[2]]
            .cdtData$EnvData$plot.maps$shp$field <- cb.attrshp

            ret <- try(get.CDT.dataset.Idx(), silent = TRUE)
            if(inherits(ret, "try-error") | is.null(ret)) return(NULL)
        })

        ############################################

        tkgrid(frameData, row = 0, column = 0, sticky = 'we', padx = 1, pady = 1, ipadx = 1, ipady = 1)
        tkgrid(frameSHP, row = 1, column = 0, sticky = 'we', padx = 1, pady = 1, ipadx = 1, ipady = 1)

    #######################################################################################################

    #Tab2
    subfr2 <- bwTabScrollableFrame(cmd.tab2)

        ##############################################

        frameGraph <- ttklabelframe(subfr2, text = lang.dlg[['label']][['5']], relief = 'groove')

        #################

        frGph1 <- tkframe(frameGraph)

        typeTSPLOT <- c("Line", "Barplot")
        .cdtData$EnvData$plot.maps$typeTSp <- tclVar("Line")

        cb.typeTSp <- ttkcombobox(frGph1, values = typeTSPLOT, textvariable = .cdtData$EnvData$plot.maps$typeTSp, width = largeur2)
        bt.TsGraph.plot <- ttkbutton(frGph1, text = .cdtEnv$tcl$lang$global[['button']][['3']], width = 7)
        bt.TSGraphOpt <- ttkbutton(frGph1, text = .cdtEnv$tcl$lang$global[['button']][['4']], width = 8)

        tkgrid(cb.typeTSp, row = 0, column = 0, sticky = 'we', pady = 1, columnspan = 1)
        tkgrid(bt.TSGraphOpt, row = 0, column = 1, sticky = 'we', padx = 4, pady = 1, columnspan = 1)
        tkgrid(bt.TsGraph.plot, row = 0, column = 2, sticky = 'we', pady = 1, columnspan = 1)

        #########

        tkconfigure(bt.TSGraphOpt, command = function(){
            suffix.fun <- switch(str_trim(tclvalue(.cdtData$EnvData$plot.maps$typeTSp)),
                                    "Barplot" = "Bar",
                                    "Line" = "Line")
            plot.fun <- get(paste0("MapGraph.GraphOptions.", suffix.fun), mode = "function")
            .cdtData$EnvData$TSGraphOp <- plot.fun(.cdtData$EnvData$TSGraphOp)
        })

        #########

        .cdtData$EnvData$tab$dataGraph <- NULL

        tkconfigure(bt.TsGraph.plot, command = function(){
            if(!is.null(.cdtData$EnvData$cdtdataset)){
                imgContainer <- CDT.Display.Graph(CDTdataset.Plot.Graph, .cdtData$EnvData$tab$dataGraph, "CDT Dataset - TS")
                .cdtData$EnvData$tab$dataGraph <- imageNotebookTab_unik(imgContainer, .cdtData$EnvData$tab$dataGraph)
            }
        })

        #################

        frGph2 <- tkframe(frameGraph)

        .cdtData$EnvData$plot.maps$lonLOC <- tclVar()
        .cdtData$EnvData$plot.maps$latLOC <- tclVar()

        txt.crdSel <- tklabel(frGph2, text = lang.dlg[['label']][['6']], anchor = 'w', justify = 'left')
        txt.lonLoc <- tklabel(frGph2, text = lang.dlg[['label']][['7']], anchor = 'e', justify = 'right')
        en.lonLoc <- tkentry(frGph2, textvariable = .cdtData$EnvData$plot.maps$lonLOC, width = largeur3)
        txt.latLoc <- tklabel(frGph2, text = lang.dlg[['label']][['8']], anchor = 'e', justify = 'right')
        en.latLoc <- tkentry(frGph2, textvariable = .cdtData$EnvData$plot.maps$latLOC, width = largeur3)

        tkgrid(txt.crdSel, row = 0, column = 0, sticky = 'we', rowspan = 1, columnspan = 4, padx = 1, pady = 1, ipadx = 1, ipady = 1)
        tkgrid(txt.lonLoc, row = 1, column = 0, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
        tkgrid(en.lonLoc, row = 1, column = 1, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
        tkgrid(txt.latLoc, row = 1, column = 2, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
        tkgrid(en.latLoc, row = 1, column = 3, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)

        #################

        frGph3 <- tkframe(frameGraph)

        .cdtData$EnvData$plot.maps$lonPAD <- tclVar('0.0')
        .cdtData$EnvData$plot.maps$latPAD <- tclVar('0.0')

        txt.spPAD <- tklabel(frGph3, text = lang.dlg[['label']][['9']], anchor = 'w', justify = 'left')
        txt.lonPAD <- tklabel(frGph3, text = paste(lang.dlg[['label']][['7']], "\u00B1"), anchor = 'e', justify = 'right')
        en.lonPAD <- tkentry(frGph3, textvariable = .cdtData$EnvData$plot.maps$lonPAD, width = largeur4)
        txt.latPAD <- tklabel(frGph3, text = paste(lang.dlg[['label']][['8']], "\u00B1"), anchor = 'e', justify = 'right')
        en.latPAD <- tkentry(frGph3, textvariable = .cdtData$EnvData$plot.maps$latPAD, width = largeur4)

        tkgrid(txt.spPAD, row = 0, column = 0, sticky = 'we', rowspan = 1, columnspan = 4, padx = 1, pady = 1, ipadx = 1, ipady = 1)
        tkgrid(txt.lonPAD, row = 1, column = 0, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
        tkgrid(en.lonPAD, row = 1, column = 1, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
        tkgrid(txt.latPAD, row = 1, column = 2, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
        tkgrid(en.latPAD, row = 1, column = 3, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)

        helpWidget(en.lonPAD, lang.dlg[['tooltip']][['1']], lang.dlg[['status']][['1']])
        helpWidget(en.latPAD, lang.dlg[['tooltip']][['2']], lang.dlg[['status']][['2']])

        #################
        tkgrid(frGph1, row = 0, column = 0, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
        tkgrid(frGph2, row = 1, column = 0, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
        tkgrid(frGph3, row = 2, column = 0, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)

        ############################################

        tkgrid(frameGraph, row = 0, column = 0, sticky = 'we', padx = 1, pady = 1, ipadx = 1, ipady = 1)

    #######################################################################################################

    get.CDT.dataset.Idx <- function(){
        tkconfigure(.cdtEnv$tcl$main$win, cursor = 'watch')
        tcl('update')
        on.exit({
            tkconfigure(.cdtEnv$tcl$main$win, cursor = '')
            tcl('update')
        })

        file.CDT.Idx <- str_trim(tclvalue(file.index.data))
        if(file.CDT.Idx == "") return(NULL)

        read.cdt.dataIdx <- TRUE
        if(!is.null(.cdtData$EnvData$cdtdataset))
            if(!is.null(.cdtData$EnvData$file.CDT.Idx))
                if(.cdtData$EnvData$file.CDT.Idx == file.CDT.Idx) read.cdt.dataIdx <- FALSE

        if(read.cdt.dataIdx){
            if(file.exists(file.CDT.Idx)){
                OutIndexdata <- try(readRDS(file.CDT.Idx), silent = TRUE)
                if(inherits(OutIndexdata, "try-error")){
                    Insert.Messages.Out(lang.dlg[['message']][['1']], format = TRUE)
                    Insert.Messages.Out(gsub('[\r\n]', '', OutIndexdata[1]), format = TRUE)
                    .cdtData$EnvData$cdtdataset <- NULL
                    return(NULL)
                }
                .cdtData$EnvData$cdtdataset <- OutIndexdata
                .cdtData$EnvData$cdtdataset$fileInfo <- file.CDT.Idx
                .cdtData$EnvData$file.CDT.Idx <- file.CDT.Idx
                ####

                .cdtData$EnvData$map <- readCdtDatasetChunk.multi.dates.order(file.CDT.Idx, OutIndexdata$dateInfo$date[1], onedate = TRUE)
            }
        }

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

    invisible()
}
