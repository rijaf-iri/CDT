
PlotSeqNetCDFFilesCmd <- function(){
    if(WindowsOS()){
        largeur0 <- 36
        largeur1 <- 33
        largeur2 <- 24
        largeur3 <- 19
        largeur4 <- 7
        largeur5 <- 18
        largeur6 <- 14
    }else{
        largeur0 <- 33
        largeur1 <- 32
        largeur2 <- 24
        largeur3 <- 19
        largeur4 <- 7
        largeur5 <- 18
        largeur6 <- 14
    }

    ###################

    GeneralParameters <- list(dir = "", sample = "", format = "rfe_%S.nc")

    .cdtData$EnvData$ncMapOp <- list(presetCol = list(color = 'tim.colors', reverse = FALSE),
                                     userCol = list(custom = FALSE, color = NULL),
                                     userLvl = list(custom = FALSE, levels = NULL, equidist = FALSE),
                                     title = list(user = FALSE, title = ''),
                                     colkeyLab = list(user = FALSE, label = ''),
                                     scalebar = list(add = FALSE, pos = 'bottomleft'),
                                     plotType = list(values = c("Pixels", "FilledContour"), var = "Pixels"),
                                     bbox = .cdtData$Config$region)

    .cdtData$EnvData$plot.maps$data.type <- "cdtnetcdf"

    ###################

    xml.dlg <- file.path(.cdtDir$dirLocal, "languages", "cdtPlot_SeqNetCDF_leftCmd.xml")
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

        frameNC <- tkframe(subfr1, relief = 'groove', borderwidth = 2)

        ncDIR <- tclVar(GeneralParameters$dir)
        ncSample <- tclVar(GeneralParameters$sample)
        ncFormat <- tclVar(GeneralParameters$format)

        txt.ncdr <- tklabel(frameNC, text = lang.dlg[['label']][['1']], anchor = 'w', justify = 'left')
        en.ncdr <- tkentry(frameNC, textvariable = ncDIR, width = largeur0)
        bt.ncdr <- tkbutton(frameNC, text = "...")
        txt.ncfl <- tklabel(frameNC, text = lang.dlg[['label']][['2']], anchor = 'w', justify = 'left')
        cb.ncfl <- ttkcombobox(frameNC, values = unlist(openFile_ttkcomboList()), textvariable = ncSample, width = largeur1)
        addTo_all_Combobox_List(cb.ncfl)
        bt.ncfl <- tkbutton(frameNC, text = "...")
        txt.ncff <- tklabel(frameNC, text = lang.dlg[['label']][['3']], anchor = 'e', justify = 'right')
        en.ncff <- tkentry(frameNC, textvariable = ncFormat, width = largeur2)

        tkgrid(txt.ncdr, row = 0, column = 0, sticky = 'we', rowspan = 1, columnspan = 6, padx = 1, pady = 0, ipadx = 1, ipady = 1)
        tkgrid(en.ncdr, row = 1, column = 0, sticky = 'we', rowspan = 1, columnspan = 5, padx = 0, pady = 0, ipadx = 1, ipady = 1)
        tkgrid(bt.ncdr, row = 1, column = 5, sticky = 'we', rowspan = 1, columnspan = 1, padx = 0, pady = 0, ipadx = 1, ipady = 1)
        tkgrid(txt.ncfl, row = 2, column = 0, sticky = 'we', rowspan = 1, columnspan = 6, padx = 1, pady = 0, ipadx = 1, ipady = 1)
        tkgrid(cb.ncfl, row = 3, column = 0, sticky = 'we', rowspan = 1, columnspan = 5, padx = 0, pady = 0, ipadx = 1, ipady = 1)
        tkgrid(bt.ncfl, row = 3, column = 5, sticky = 'we', rowspan = 1, columnspan = 1, padx = 0, pady = 0, ipadx = 1, ipady = 1)
        tkgrid(txt.ncff, row = 4, column = 0, sticky = 'e', rowspan = 1, columnspan = 2, padx = 1, pady = 1, ipadx = 1, ipady = 1)
        tkgrid(en.ncff, row = 4, column = 2, sticky = 'we', rowspan = 1, columnspan = 4, padx = 1, pady = 1, ipadx = 1, ipady = 1)

        helpWidget(en.ncdr, lang.dlg[['tooltip']][['1']], lang.dlg[['status']][['1']])
        helpWidget(cb.ncfl, lang.dlg[['tooltip']][['2']], lang.dlg[['status']][['2']])
        helpWidget(bt.ncfl, lang.dlg[['tooltip']][['3']], lang.dlg[['status']][['3']])
        helpWidget(en.ncff, lang.dlg[['tooltip']][['4']], lang.dlg[['status']][['4']])

        #################
        tkconfigure(bt.ncdr, command = function(){
            dir4cdf <- tk_choose.dir(getwd(), "")
            tclvalue(ncDIR) <- if(dir4cdf %in% c("", "NA") | is.na(dir4cdf)) "" else dir4cdf
        })

        tkconfigure(bt.ncfl, command = function(){
            initialdir <- if(file.exists(trimws(tclvalue(ncDIR)))) trimws(tclvalue(ncDIR)) else getwd()
            nc.opfiles <- getOpenNetcdf(.cdtEnv$tcl$main$win, initialdir = initialdir)
            if(!is.null(nc.opfiles)){
                update.OpenFiles('netcdf', nc.opfiles)
                tclvalue(ncSample) <- nc.opfiles[[1]]
            }
        })

        ##############################################

        frameMap <- ttklabelframe(subfr1, text = lang.dlg[['label']][['4']], relief = 'groove')

        ncdf.date.file <- tclVar()

        cb.nc.maps <- ttkcombobox(frameMap, values = "", textvariable = ncdf.date.file, width = largeur3, justify = 'center')
        bt.nc.Date.prev <- ttkbutton(frameMap, text = "<<", width = largeur4)
        bt.nc.Date.next <- ttkbutton(frameMap, text = ">>", width = largeur4)
        bt.nc.MapOpt <- ttkbutton(frameMap, text = .cdtEnv$tcl$lang$global[['button']][['4']], width = largeur5)
        bt.nc.maps <- ttkbutton(frameMap, text = .cdtEnv$tcl$lang$global[['button']][['3']], width = largeur5)

        ###################

        tkgrid(bt.nc.Date.prev, row = 0, column = 0, sticky = 'we', rowspan = 1, columnspan = 2, padx = 1, pady = 1, ipadx = 1, ipady = 1)
        tkgrid(cb.nc.maps, row = 0, column = 2, sticky = 'we', rowspan = 1, columnspan = 6, padx = 1, pady = 1, ipadx = 1, ipady = 1)
        tkgrid(bt.nc.Date.next, row = 0, column = 8, sticky = 'we', rowspan = 1, columnspan = 2, padx = 1, pady = 1, ipadx = 1, ipady = 1)
        tkgrid(bt.nc.MapOpt, row = 1, column = 0, sticky = 'we', rowspan = 1, columnspan = 5, ipadx = 1, ipady = 1)
        tkgrid(bt.nc.maps, row = 1, column = 5, sticky = 'we', rowspan = 1, columnspan = 5, ipadx = 1, ipady = 1)

        ###################

        tkconfigure(bt.nc.MapOpt, command = function(){
            if(!is.null(.cdtData$EnvData$ncData$map)){
                atlevel <- pretty(.cdtData$EnvData$ncData$map$z, n = 10, min.n = 7)
                if(is.null(.cdtData$EnvData$ncMapOp$userLvl$levels)){
                    .cdtData$EnvData$ncMapOp$userLvl$levels <- atlevel
                }else{
                    if(!.cdtData$EnvData$ncMapOp$userLvl$custom)
                        .cdtData$EnvData$ncMapOp$userLvl$levels <- atlevel
                }
            }
            .cdtData$EnvData$ncMapOp <- MapGraph.MapOptions(.cdtData$EnvData$ncMapOp)
        })

        ###################

        .cdtData$EnvData$tab$dataNCMap <- NULL

        tkconfigure(bt.nc.maps, command = function(){
            ret <- try(get.All.NCDF.Files(), silent = TRUE)
            if(inherits(ret, "try-error") | is.null(ret)) return(NULL)

            if(trimws(tclvalue(ncdf.date.file)) != ""){
                ret <- try(get.NCDF.DATA(), silent = TRUE)
                if(inherits(ret, "try-error") | is.null(ret)) return(NULL)

                tab.title <- paste('Map -', .cdtData$EnvData$ncData$file2plot)
                imgContainer <- CDT.Display.Graph(PlotNetCDFdataMaps, .cdtData$EnvData$tab$dataNCMap, tab.title)
                .cdtData$EnvData$tab$dataNCMap <- imageNotebookTab_unik(imgContainer, .cdtData$EnvData$tab$dataNCMap)
            }
        })

        tkconfigure(bt.nc.Date.prev, command = function(){
            ret <- try(get.All.NCDF.Files(), silent = TRUE)
            if(inherits(ret, "try-error") | is.null(ret)) return(NULL)

            if(trimws(tclvalue(ncdf.date.file)) != ""){
                donDates <- .cdtData$EnvData$NcFiles2Plot
                idaty <- which(donDates == trimws(tclvalue(ncdf.date.file)))
                idaty <- idaty - 1
                if(idaty < 1) idaty <- length(donDates)
                tclvalue(ncdf.date.file) <- donDates[idaty]

                ret <- try(get.NCDF.DATA(), silent = TRUE)
                if(inherits(ret, "try-error") | is.null(ret)) return(NULL)

                tab.title <- paste('Map -', .cdtData$EnvData$ncData$file2plot)
                imgContainer <- CDT.Display.Graph(PlotNetCDFdataMaps, .cdtData$EnvData$tab$dataNCMap, tab.title)
                .cdtData$EnvData$tab$dataNCMap <- imageNotebookTab_unik(imgContainer, .cdtData$EnvData$tab$dataNCMap)
            }
        })

        tkconfigure(bt.nc.Date.next, command = function(){
            ret <- try(get.All.NCDF.Files(), silent = TRUE)
            if(inherits(ret, "try-error") | is.null(ret)) return(NULL)

            if(trimws(tclvalue(ncdf.date.file)) != ""){
                donDates <- .cdtData$EnvData$NcFiles2Plot
                idaty <- which(donDates == trimws(tclvalue(ncdf.date.file)))
                idaty <- idaty + 1
                if(idaty > length(donDates)) idaty <- 1
                tclvalue(ncdf.date.file) <- donDates[idaty]

                ret <- try(get.NCDF.DATA(), silent = TRUE)
                if(inherits(ret, "try-error") | is.null(ret)) return(NULL)

                tab.title <- paste('Map -', .cdtData$EnvData$ncData$file2plot)
                imgContainer <- CDT.Display.Graph(PlotNetCDFdataMaps, .cdtData$EnvData$tab$dataNCMap, tab.title)
                .cdtData$EnvData$tab$dataNCMap <- imageNotebookTab_unik(imgContainer, .cdtData$EnvData$tab$dataNCMap)
            }
        })

        ############################################

        tkgrid(frameNC, row = 0, column = 0, sticky = 'we', padx = 1, pady = 1, ipadx = 1, ipady = 1)
        tkgrid(frameMap, row = 1, column = 0, sticky = 'we', padx = 1, pady = 1, ipadx = 1, ipady = 1)

    #######################################################################################################

    #Tab2
    subfr2 <- bwTabScrollableFrame(cmd.tab2)

        ##############################################

        frameSHP <- create_shpLayer_frame(subfr2)
        tkgrid(frameSHP, row = 0, column = 0, sticky = 'we', pady = 1)

    #######################################################################################################

    get.All.NCDF.Files <- function(){
        tkconfigure(.cdtEnv$tcl$main$win, cursor = 'watch')
        tcl('update')
        on.exit({
            tkconfigure(.cdtEnv$tcl$main$win, cursor = '')
            tcl('update')
        })

        nc.dir <- trimws(tclvalue(ncDIR))
        nc.format <- trimws(tclvalue(ncFormat))

        if(nc.dir == "" | nc.format == ""){
            tkconfigure(cb.nc.maps, values = "")
            tclvalue(ncdf.date.file) <- ""
            .cdtData$EnvData$loaded.nc <- NULL
            return(NULL)
        }

        loaded.nc <- list(nc.dir, nc.format)

        getNCFiles <- TRUE
        if(!is.null(.cdtData$EnvData$loaded.nc))
            getNCFiles <- if(!isTRUE(all.equal(.cdtData$EnvData$loaded.nc, loaded.nc))) TRUE else FALSE

        if(getNCFiles){
            error.msg <- lang.dlg[['message']][['1']]
            nc.files <- ncFilesInfoSeq(nc.dir, nc.format, error.msg)

            if(is.null(nc.files)){
                tkconfigure(cb.nc.maps, values = "")
                tclvalue(ncdf.date.file) <- ""
                .cdtData$EnvData$loaded.nc <- NULL
                return(NULL)
            }

            rfeDataInfo <- getNCDFSampleData(trimws(tclvalue(ncSample)))
            if(is.null(rfeDataInfo)){
                Insert.Messages.Out(lang.dlg[['message']][['2']], TRUE, 'e')
                return(NULL)
            }

            .cdtData$EnvData$varinfo <- rfeDataInfo$varinfo
            ncinfo <- list(xo = rfeDataInfo$ilon, yo = rfeDataInfo$ilat, varid = rfeDataInfo$varid)

            ncfileInit <- file.path(nc.dir, nc.files[1])
            nc <- ncdf4::nc_open(ncfileInit)
            xlon <- nc$var[[ncinfo$varid]]$dim[[ncinfo$xo]]$vals
            xlat <- nc$var[[ncinfo$varid]]$dim[[ncinfo$yo]]$vals
            ncdf4::nc_close(nc)
            xo <- order(xlon)
            xlon <- xlon[xo]
            yo <- order(xlat)
            xlat <- xlat[yo]

            .cdtData$EnvData$ncInfo$ncinfo <- ncinfo
            .cdtData$EnvData$ncInfo$coords <- list(lon = xlon, lat = xlat, ix = xo, iy = yo)

            tkconfigure(cb.nc.maps, values = nc.files)
            tclvalue(ncdf.date.file) <- nc.files[1]
            .cdtData$EnvData$NcFiles2Plot <- nc.files

            .cdtData$EnvData$loaded.nc <- loaded.nc
        }
        return(0)
    }

    get.NCDF.DATA <- function(){
        tkconfigure(.cdtEnv$tcl$main$win, cursor = 'watch')
        tcl('update')
        on.exit({
            tkconfigure(.cdtEnv$tcl$main$win, cursor = '')
            tcl('update')
        })

        nc.dir <- trimws(tclvalue(ncDIR))
        nc.file <- trimws(tclvalue(ncdf.date.file))
        ncfile.path <- file.path(nc.dir, nc.file)

        readNCFILE <- TRUE
        if(!is.null(.cdtData$EnvData$ncData))
            if(!is.null(.cdtData$EnvData$ncData$ncfile))
                if(.cdtData$EnvData$ncData$ncfile == ncfile.path) readNCFILE <- FALSE

        if(readNCFILE){
            .cdtData$EnvData$ncData$map$x <- .cdtData$EnvData$ncInfo$coords$lon
            .cdtData$EnvData$ncData$map$y <- .cdtData$EnvData$ncInfo$coords$lat

            ncinfo <- .cdtData$EnvData$ncInfo$ncinfo
            ix <- .cdtData$EnvData$ncInfo$coords$ix
            iy <- .cdtData$EnvData$ncInfo$coords$iy

            nc <- try(ncdf4::nc_open(ncfile.path), silent = TRUE)
            if(inherits(nc, "try-error")) return(NULL)
            ncdon <- ncdf4::ncvar_get(nc, varid = ncinfo$varid)
            ncdf4::nc_close(nc)
            ncdon <- if(ncinfo$xo < ncinfo$yo) ncdon[ix, iy] else t(ncdon)[ix, iy]
            .cdtData$EnvData$ncData$map$z <- ncdon

            .cdtData$EnvData$ncData$file2plot <- nc.file
            .cdtData$EnvData$ncData$ncfile <- ncfile.path
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
