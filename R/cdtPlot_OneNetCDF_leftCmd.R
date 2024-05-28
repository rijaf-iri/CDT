PlotOneNetCDFFileCmd <- function(){
    if(WindowsOS()){
        largeur0 <- 32
        largeur1 <- 18
        largeur2 <- 14
    }else{
        largeur0 <- 32
        largeur1 <- 18
        largeur2 <- 14
    }

    ###################

    .cdtData$EnvData$ncMapOp <- list(presetCol = list(color = 'tim.colors', reverse = FALSE),
                                     userCol = list(custom = FALSE, color = NULL),
                                     userLvl = list(custom = FALSE, levels = NULL, equidist = FALSE),
                                     title = list(user = FALSE, title = ''),
                                     colkeyLab = list(user = FALSE, label = ''),
                                     scalebar = list(add = FALSE, pos = 'bottomleft'),
                                     plotType = list(values = c("Pixels", "FilledContour"), var = "Pixels"),
                                     bbox = .cdtData$Config$region)
    .cdtData$EnvData$plot.maps$data.type <- "cdtnetcdf"

    .cdtData$EnvData$shapefile$options <- list(col = "black", lwd = 1.5)

    ###################

    xml.dlg <- file.path(.cdtDir$dirLocal, "languages", "cdtPlot_OneNetCDF_leftCmd.xml")
    lang.dlg <- cdtLanguageParse(xml.dlg, .cdtData$Config$lang.iso)
    # .cdtData$EnvData$message <- lang.dlg[['message']]

    ###################

    .cdtEnv$tcl$main$cmd.frame <- tkframe(.cdtEnv$tcl$main$panel.left)

    tknote.cmd <- bwNoteBook(.cdtEnv$tcl$main$cmd.frame)
    cmd.tab1 <- bwAddTab(tknote.cmd, text = lang.dlg[['tab_title']][['1']])

    bwRaiseTab(tknote.cmd, cmd.tab1)

    tkgrid.columnconfigure(cmd.tab1, 0, weight = 1)
    tkgrid.rowconfigure(cmd.tab1, 0, weight = 1)

    #######################################################################################################

    #Tab1
    subfr1 <- bwTabScrollableFrame(cmd.tab1)

        #######################

        frameNC <- tkframe(subfr1, relief = 'groove', borderwidth = 2)

        ncdf.file <- tclVar()

        txt.ncfl <- tklabel(frameNC, text = lang.dlg[['label']][['1']], anchor = 'w', justify = 'left')
        cb.ncfl <- ttkcombobox(frameNC, values = unlist(openFile_ttkcomboList()), textvariable = ncdf.file, width = largeur0)
        addTo_all_Combobox_List(cb.ncfl)
        bt.ncfl <- tkbutton(frameNC, text = "...")

        tkgrid(txt.ncfl, row = 0, column = 0, sticky = 'we', rowspan = 1, columnspan = 6, padx = 1, pady = 0, ipadx = 1, ipady = 1)
        tkgrid(cb.ncfl, row = 1, column = 0, sticky = 'we', rowspan = 1, columnspan = 5, padx = 0, pady = 0, ipadx = 1, ipady = 1)
        tkgrid(bt.ncfl, row = 1, column = 5, sticky = 'w', rowspan = 1, columnspan = 1, padx = 0, pady = 0, ipadx = 1, ipady = 1)

        ###################

        tkconfigure(bt.ncfl, command = function(){
            tkconfigure(.cdtEnv$tcl$main$win, cursor = 'watch')
            tcl('update')
            on.exit({
                tkconfigure(.cdtEnv$tcl$main$win, cursor = '')
                tcl('update')
            })

            nc.opfiles <- getOpenNetcdf(.cdtEnv$tcl$main$win)
            if(!is.null(nc.opfiles)){
                update.OpenFiles('netcdf', nc.opfiles)
                tclvalue(ncdf.file) <- nc.opfiles[[1]]
            }
        })

        ##############################################

        frameMap <- tkframe(subfr1)

        bt.nc.MapOpt <- ttkbutton(frameMap, text = .cdtEnv$tcl$lang$global[['button']][['4']], width = largeur1)
        bt.nc.maps <- ttkbutton(frameMap, text = .cdtEnv$tcl$lang$global[['button']][['3']], width = largeur1)

        tkgrid(bt.nc.MapOpt, row = 0, column = 0, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1)
        tkgrid(bt.nc.maps, row = 0, column = 1, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1)

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
            if(trimws(tclvalue(ncdf.file)) != ""){
                ret <- try(get.NCDF.DATA(), silent = TRUE)
                if(inherits(ret, "try-error") | is.null(ret)) return(NULL)

                tab.title <- paste('Map -', .cdtData$EnvData$ncData$file2plot)
                imgContainer <- CDT.Display.Graph(PlotNetCDFdataMaps, .cdtData$EnvData$tab$dataNCMap, tab.title)
                .cdtData$EnvData$tab$dataNCMap <- imageNotebookTab_unik(imgContainer, .cdtData$EnvData$tab$dataNCMap)
            }
        })

        ##############################################

        frameBlank <- tkframe(subfr1)

        blankGrid <- tclVar(0)

        chk.grid <- tkcheckbutton(frameBlank, variable = blankGrid, text = lang.dlg[['checkbutton']][['1']], anchor = 'w', justify = 'left')
        tkgrid(chk.grid)

        tkbind(chk.grid, "<Button-1>", function(){
            if(tclvalue(blankGrid) == "1"){
                stateSHP <- if(tclvalue(.cdtData$EnvData$shapefile$addshp) == "0") "disabled" else "normal"
            }else stateSHP <- "normal"
            tkconfigure(cb.addshp, state = stateSHP)
            tkconfigure(bt.addshp, state = stateSHP)
        })

        ##############################################

        frameSHP <- ttklabelframe(subfr1, text = lang.dlg[['label']][['2']], relief = 'groove')

        .cdtData$EnvData$shapefile$addshp <- tclVar(0)
        file.plotShp <- tclVar()
        stateSHP <- "disabled"

        chk.addshp <- tkcheckbutton(frameSHP, variable = .cdtData$EnvData$shapefile$addshp, text = lang.dlg[['checkbutton']][['2']], anchor = 'w', justify = 'left')
        bt.addshpOpt <- ttkbutton(frameSHP, text = .cdtEnv$tcl$lang$global[['button']][['4']], state = stateSHP)
        cb.addshp <- ttkcombobox(frameSHP, values = unlist(openFile_ttkcomboList()), textvariable = file.plotShp, width = largeur0, state = stateSHP)
        addTo_all_Combobox_List(cb.addshp)
        bt.addshp <- tkbutton(frameSHP, text = "...", state = stateSHP)

        ########
        tkgrid(chk.addshp, row = 0, column = 0, sticky = 'we', rowspan = 1, columnspan = 6, padx = 1, pady = 1)
        tkgrid(bt.addshpOpt, row = 0, column = 6, sticky = 'we', rowspan = 1, columnspan = 2, padx = 1, pady = 1)
        tkgrid(cb.addshp, row = 1, column = 0, sticky = 'we', rowspan = 1, columnspan = 7, padx = 1, pady = 1)
        tkgrid(bt.addshp, row = 1, column = 7, sticky = 'w', rowspan = 1, columnspan = 1, padx = 0, pady = 1)

        ########

        helpWidget(cb.addshp, lang.dlg[['tooltip']][['1']], lang.dlg[['status']][['1']])
        helpWidget(bt.addshp, lang.dlg[['tooltip']][['2']], lang.dlg[['status']][['2']])

        ########
        tkconfigure(bt.addshp, command = function(){
            shp.opfiles <- getOpenShp(.cdtEnv$tcl$main$win)
            if(!is.null(shp.opfiles)){
                update.OpenFiles('shp', shp.opfiles)
                tclvalue(file.plotShp) <- shp.opfiles[[1]]

                shpofile <- getShpOpenData(file.plotShp)
                if(is.null(shpofile))
                    .cdtData$EnvData$shapefile$ocrds <- NULL
                else
                    .cdtData$EnvData$shapefile$ocrds <- getBoundaries(shpofile[[2]])
            }
        })

        ########

        tkconfigure(bt.addshpOpt, command = function(){
            .cdtData$EnvData$shapefile$options <- MapGraph.GraphOptions.LineSHP(.cdtData$EnvData$shapefile$options)
        })

        #################
        tkbind(cb.addshp, "<<ComboboxSelected>>", function(){
            shpofile <- getShpOpenData(file.plotShp)
            if(is.null(shpofile))
                .cdtData$EnvData$shapefile$ocrds <- NULL
            else
                .cdtData$EnvData$shapefile$ocrds <- getBoundaries(shpofile[[2]])
        })

        tkbind(chk.addshp, "<Button-1>", function(){
            if(tclvalue(.cdtData$EnvData$shapefile$addshp) == "1"){
                stateSHP <- if(tclvalue(blankGrid) == "0") "disabled" else "normal"
            }else stateSHP <- "normal"
            tkconfigure(cb.addshp, state = stateSHP)
            tkconfigure(bt.addshp, state = stateSHP)

            stateSHP1 <- if(tclvalue(.cdtData$EnvData$shapefile$addshp) == "1") "disabled" else "normal"
            tkconfigure(bt.addshpOpt, state = stateSHP1)
        })

        ############################################

        tkgrid(frameNC, row = 0, column = 0, sticky = 'we', padx = 1, pady = 1, ipadx = 1, ipady = 1)
        tkgrid(frameMap, row = 1, column = 0, sticky = '', padx = 1, pady = 5, ipadx = 1, ipady = 1)
        tkgrid(frameBlank, row = 2, column = 0, sticky = 'we', padx = 1, pady = 1, ipadx = 1, ipady = 1)
        tkgrid(frameSHP, row = 3, column = 0, sticky = 'we', padx = 1, pady = 5, ipadx = 1, ipady = 1)

    #######################################################################################################

    get.NCDF.DATA <- function(){
        tkconfigure(.cdtEnv$tcl$main$win, cursor = 'watch')
        tcl('update')
        on.exit({
            tkconfigure(.cdtEnv$tcl$main$win, cursor = '')
            tcl('update')
        })

        loaded.nc <- list(trimws(tclvalue(ncdf.file)), tclvalue(blankGrid))

        getNCFiles <- TRUE
        if(!is.null(.cdtData$EnvData$loaded.nc))
            getNCFiles <- if(!isTRUE(all.equal(.cdtData$EnvData$loaded.nc, loaded.nc))) TRUE else FALSE

        if(getNCFiles){
            ncdata <- getNcdfOpenData(trimws(tclvalue(ncdf.file)))
            .cdtData$EnvData$ncData$map$x <- ncdata[[2]]$x
            .cdtData$EnvData$ncData$map$y <- ncdata[[2]]$y

            if(tclvalue(blankGrid) == "1"){
                shpdata <- getShpOpenData(file.plotShp)[[2]]
                if(is.null(shpdata)){
                    Insert.Messages.Out(lang.dlg[['message']][['1']], TRUE, "e")
                    return(NULL)
                }
                nc.grid <- list(lon = ncdata[[2]]$x, lat = ncdata[[2]]$y)
                mask <- create.mask.grid(shpdata, nc.grid)
                .cdtData$EnvData$ncData$map$z <- ncdata[[2]]$z * mask
            }else .cdtData$EnvData$ncData$map$z <- ncdata[[2]]$z

            .cdtData$EnvData$ncData$file2plot <- ncdata[[1]]
            .cdtData$EnvData$loaded.nc <- loaded.nc
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
