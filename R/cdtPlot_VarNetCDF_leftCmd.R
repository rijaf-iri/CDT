
PlotVarNetCDFFilesCmd <- function(){
    if(WindowsOS()){
        largeur0 <- 36
        largeur1 <- 33
        largeur2 <- 22
        largeur3 <- 14
        largeur4 <- 11
        largeur5 <- 12
        largeur6 <- 20
        largeur7 <- 19
        vars.w <- 280
        vars.h <- 140
    }else{
        largeur0 <- 33
        largeur1 <- 32
        largeur2 <- 21
        largeur3 <- 14
        largeur4 <- 11
        largeur5 <- 15
        largeur6 <- 25
        largeur7 <- 18
        vars.w <- 310
        vars.h <- 160
    }

    ###################

    GeneralParameters <- list(dir = "", sample = "", format = "rad_avg_%S.nc")

    ncMapOp <- list(presetCol = list(color = 'tim.colors', reverse = FALSE),
                    userCol = list(custom = FALSE, color = NULL),
                    userLvl = list(custom = FALSE, levels = NULL, equidist = FALSE),
                    title = list(user = FALSE, title = ''),
                    colkeyLab = list(user = FALSE, label = ''),
                    plotType = list(values = c("Pixels", "FilledContour"), var = "Pixels"))

    .cdtData$EnvData$tmpMapOp <- list(draw.box = FALSE, bbox = .cdtData$Config$region)

    .cdtData$EnvData$plot.maps$data.type <- "cdtnetcdf"

    ###################

    xml.dlg <- file.path(.cdtDir$dirLocal, "languages", "cdtPlot_VarNetCDF_leftCmd.xml")
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

        selectVarsFun <- function(fileopen){
            tkdestroy(frSelVars)
            frSelVars <<- tkframe(sel.vars)

            ##################################

            if(fileopen == "" | is.na(fileopen)){
                Insert.Messages.Out(lang.dlg[['message']][['8']], TRUE, 'e')
                return(NULL)
            }
            if(!file.exists(fileopen)){
                Insert.Messages.Out(paste(fileopen, lang.dlg[['message']][['3']]), TRUE, 'e')
                .cdtData$EnvData$ncvar <- NULL
                return(NULL)
            }

            nc <- ncdf4::nc_open(fileopen)
            ncdims <- sapply(nc$dim, '[[', 'name')
            var.name <- sapply(nc$var, '[[', 'name')
            var.lname <- sapply(nc$var, '[[', 'longname')
            ncdf4::nc_close(nc)

            ncvars <- paste0(var.name, '::', var.lname)

            if(length(ncdims) < 2){
                Insert.Messages.Out(lang.dlg[['message']][['1']], TRUE, 'e')
                .cdtData$EnvData$ncvar <- NULL
                return(NULL)
            }

            if(length(ncvars) < 1){
                Insert.Messages.Out(lang.dlg[['message']][['2']], TRUE, 'e')
                .cdtData$EnvData$ncvar <- NULL
                return(NULL)
            }

            tkconfigure(cb.lon, values = ncdims)
            tkconfigure(cb.lat, values = ncdims)
            tclvalue(ncLON) <- ncdims[1]
            tclvalue(ncLAT) <- ncdims[2]

            ##################################

            nl <- length(.cdtData$EnvData$ncvar)
            if(nl > 0){
                for(j in seq(nl)){
                    tkdestroy(.cdtData$EnvData$ncvar[[j]]$chk.wdg)
                    tkdestroy(.cdtData$EnvData$ncvar[[j]]$bt.wdg)
                    .cdtData$EnvData$ncvar <- NULL
                }
                tcl("update")
            }
            .cdtData$EnvData$ncvar <- vector(mode = 'list', length = length(ncvars))

            tcl("update", "idletasks")
            for(j in seq_along(ncvars)){
                .cdtData$EnvData$ncvar[[j]]$ncops <- ncMapOp
                .cdtData$EnvData$ncvar[[j]]$var.name <- var.name[j]
                .cdtData$EnvData$ncvar[[j]]$chk.var <- tclVar(1)
                .cdtData$EnvData$ncvar[[j]]$chk.wdg <- tkcheckbutton(frSelVars, variable = .cdtData$EnvData$ncvar[[j]]$chk.var,
                                                                     text = ncvars[j], anchor = 'w', justify = 'left', width = largeur6)
                .cdtData$EnvData$ncvar[[j]]$bt.wdg <- ttkbutton(frSelVars, text = .cdtEnv$tcl$lang$global[['button']][['4']])

                tkgrid(.cdtData$EnvData$ncvar[[j]]$chk.wdg, row = j - 1, column = 0, sticky = 'w')
                tkgrid(.cdtData$EnvData$ncvar[[j]]$bt.wdg, row = j - 1, column = 1, sticky = 'e')

                infobulle(.cdtData$EnvData$ncvar[[j]]$chk.wdg, ncvars[j])
            }

            #################

            res <- lapply(seq_along(ncvars), function(j){
                tkconfigure(.cdtData$EnvData$ncvar[[j]]$bt.wdg, command = function(){
                    if(!is.null(.cdtData$EnvData$ncData$map)){
                        atlevel <- pretty(.cdtData$EnvData$ncData$map[[j]]$z, n = 10, min.n = 7)
                        if(is.null(.cdtData$EnvData$ncvar[[j]]$ncops$userLvl$levels)){
                            .cdtData$EnvData$ncvar[[j]]$ncops$userLvl$levels <- atlevel
                        }else{
                            if(!.cdtData$EnvData$ncvar[[j]]$ncops$userLvl$custom)
                                .cdtData$EnvData$ncvar[[j]]$ncops$userLvl$levels <- atlevel
                        }
                    }
                    .cdtData$EnvData$ncvar[[j]]$ncops <- MapGraph.MapOptions.VarNetCDF(.cdtData$EnvData$ncvar[[j]]$ncops)
                })
            })

            #################

            res <- lapply(seq_along(ncvars), function(j){
                tkbind(.cdtData$EnvData$ncvar[[j]]$chk.wdg, "<Button-1>", function(){
                    stateOP <- if(tclvalue(.cdtData$EnvData$ncvar[[j]]$chk.var) == "1") "disabled" else "normal"
                    tkconfigure(.cdtData$EnvData$ncvar[[j]]$bt.wdg, state = stateOP)
                })
            })

            ##################################

            tkgrid(frSelVars)

            return(list(varinfo = var.name, diminfo = ncdims))
        }

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

        txt.ncff <- tklabel(frameNC, text = lang.dlg[['label']][['3']], anchor = 'w', justify = 'left')
        en.ncff <- tkentry(frameNC, textvariable = ncFormat, width = largeur0)

        #################

        tkgrid(txt.ncdr, row = 0, column = 0, sticky = 'we', rowspan = 1, columnspan = 6, padx = 1, pady = 0, ipadx = 1, ipady = 1)
        tkgrid(en.ncdr, row = 1, column = 0, sticky = 'we', rowspan = 1, columnspan = 5, padx = 0, pady = 0, ipadx = 1, ipady = 1)
        tkgrid(bt.ncdr, row = 1, column = 5, sticky = 'we', rowspan = 1, columnspan = 1, padx = 0, pady = 0, ipadx = 1, ipady = 1)
        tkgrid(txt.ncfl, row = 2, column = 0, sticky = 'we', rowspan = 1, columnspan = 6, padx = 1, pady = 0, ipadx = 1, ipady = 1)
        tkgrid(cb.ncfl, row = 3, column = 0, sticky = 'we', rowspan = 1, columnspan = 5, padx = 0, pady = 0, ipadx = 1, ipady = 1)
        tkgrid(bt.ncfl, row = 3, column = 5, sticky = 'we', rowspan = 1, columnspan = 1, padx = 0, pady = 0, ipadx = 1, ipady = 1)

        tkgrid(txt.ncff, row = 4, column = 0, sticky = 'we', rowspan = 1, columnspan = 6, padx = 1, pady = 0, ipadx = 1, ipady = 1)
        tkgrid(en.ncff, row = 5, column = 0, sticky = 'we', rowspan = 1, columnspan = 5, padx = 1, pady = 0, ipadx = 1, ipady = 1)

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
            fileopen <- tclvalue(tkgetOpenFile(initialdir = initialdir, filetypes = .cdtEnv$tcl$data$filetypes3))

            ret <- selectVarsFun(fileopen)

            if(!is.null(ret)){
                nc.opfiles <- list(basename(fileopen), ret, fileopen)
                tkinsert(.cdtEnv$tcl$main$Openfiles, "end", basename(fileopen))

                update.OpenFiles('netcdf', nc.opfiles)
                tclvalue(ncSample) <- nc.opfiles[[1]]
            }
        })

        tkbind(cb.ncfl, "<<ComboboxSelected>>", function(){
            jfile <- getIndex.AllOpenFiles(trimws(tclvalue(ncSample)))
            fileopen <- ""
            if(length(jfile) > 0){
                if(.cdtData$OpenFiles$Type[[jfile]] == "netcdf")
                    fileopen <- .cdtData$OpenFiles$Data[[jfile]][[3]]
            }

            ret <- selectVarsFun(fileopen)
        })

        #######################

        frameDIM <- ttklabelframe(subfr1, text = lang.dlg[['label']][['4']], relief = 'groove')

        ncLON <- tclVar()
        ncLAT <- tclVar()

        txt.lon <- tklabel(frameDIM, text = lang.dlg[['label']][['5']], anchor = 'e', justify = 'right', width = largeur5)
        cb.lon <- ttkcombobox(frameDIM, values = "", textvariable = ncLON, width = largeur2)
        txt.lat <- tklabel(frameDIM, text = lang.dlg[['label']][['6']], anchor = 'e', justify = 'right')
        cb.lat <- ttkcombobox(frameDIM, values = "", textvariable = ncLAT, width = largeur2)

        tkgrid(txt.lon, row = 0, column = 0, sticky = 'we', rowspan = 1, columnspan = 2, padx = 1, pady = 1, ipadx = 1, ipady = 1)
        tkgrid(cb.lon, row = 0, column = 2, sticky = 'we', rowspan = 1, columnspan = 4, padx = 1, pady = 1, ipadx = 1, ipady = 1)
        tkgrid(txt.lat, row = 1, column = 0, sticky = 'we', rowspan = 1, columnspan = 2, padx = 1, pady = 1, ipadx = 1, ipady = 1)
        tkgrid(cb.lat, row = 1, column = 2, sticky = 'we', rowspan = 1, columnspan = 4, padx = 1, pady = 1, ipadx = 1, ipady = 1)

        #######################

        frameVARS <- ttklabelframe(subfr1, text = lang.dlg[['label']][['7']], relief = 'groove')

        sel.vars <- bwTabScrollableFrame(frameVARS, wscrlwin = vars.w, hscrlwin = vars.h)

        frSelVars <- tkframe(sel.vars)

        ##############################################

        tkgrid(frameNC, row = 0, column = 0, sticky = 'we', padx = 1, pady = 1, ipadx = 1, ipady = 1)
        tkgrid(frameDIM, row = 1, column = 0, sticky = 'we', padx = 1, pady = 1, ipadx = 1, ipady = 1)
        tkgrid(frameVARS, row = 2, column = 0, sticky = 'nwe', padx = 1, pady = 1, ipadx = 1, ipady = 1)

    #######################################################################################################

    #Tab2
    subfr2 <- bwTabScrollableFrame(cmd.tab2)

        ##############################################

        frameMap <- ttklabelframe(subfr2, text = lang.dlg[['label']][['8']], relief = 'groove')

        ncdf.date.file <- tclVar()

        cb.nc.maps <- ttkcombobox(frameMap, values = "", textvariable = ncdf.date.file, width = largeur1, justify = 'center')
        bt.nc.Date.prev <- ttkbutton(frameMap, text = "<<", width = largeur4)
        bt.nc.maps <- ttkbutton(frameMap, text = .cdtEnv$tcl$lang$global[['button']][['3']], width = largeur4)
        bt.nc.Date.next <- ttkbutton(frameMap, text = ">>", width = largeur4)

        ###################

        tkgrid(cb.nc.maps, row = 0, column = 1, sticky = 'we', rowspan = 1, columnspan = 4, padx = 1, pady = 1, ipadx = 1, ipady = 1)
        tkgrid(bt.nc.Date.prev, row = 1, column = 0, sticky = 'we', rowspan = 1, columnspan = 2, padx = 1, pady = 1, ipadx = 1, ipady = 1)
        tkgrid(bt.nc.maps, row = 1, column = 2, sticky = 'we', rowspan = 1, columnspan = 2, padx = 1, pady = 1, ipadx = 1, ipady = 1)
        tkgrid(bt.nc.Date.next, row = 1, column = 4, sticky = 'we', rowspan = 1, columnspan = 2, padx = 1, pady = 1, ipadx = 1, ipady = 1)

        ###################

        .cdtData$EnvData$tab$dataNCMap <- NULL

        tkconfigure(bt.nc.maps, command = function(){
            ret <- try(get.All.NCDF.Files(), silent = TRUE)
            if(inherits(ret, "try-error") | is.null(ret)) return(NULL)

            if(trimws(tclvalue(ncdf.date.file)) != ""){
                ret <- try(get.NCDF.DATA(), silent = TRUE)
                if(inherits(ret, "try-error") | is.null(ret)) return(NULL)

                get.tmpMapOP.values()
                tab.title <- paste('Map -', .cdtData$EnvData$ncData$file2plot)
                imgContainer <- CDT.Display.Graph(PlotNetCDFVarsMaps, .cdtData$EnvData$tab$dataNCMap, tab.title)
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

                get.tmpMapOP.values()
                tab.title <- paste('Map -', .cdtData$EnvData$ncData$file2plot)
                imgContainer <- CDT.Display.Graph(PlotNetCDFVarsMaps, .cdtData$EnvData$tab$dataNCMap, tab.title)
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

                get.tmpMapOP.values()
                tab.title <- paste('Map -', .cdtData$EnvData$ncData$file2plot)
                imgContainer <- CDT.Display.Graph(PlotNetCDFVarsMaps, .cdtData$EnvData$tab$dataNCMap, tab.title)
                .cdtData$EnvData$tab$dataNCMap <- imageNotebookTab_unik(imgContainer, .cdtData$EnvData$tab$dataNCMap)
            }
        })

        ##############################################

        frameBbox <- ttklabelframe(subfr2, text = lang.dlg[['label']][['9']], relief = 'groove')

        minlonV <- tclVar(.cdtData$EnvData$tmpMapOp$bbox$minlon)
        maxlonV <- tclVar(.cdtData$EnvData$tmpMapOp$bbox$maxlon)
        minlatV <- tclVar(.cdtData$EnvData$tmpMapOp$bbox$minlat)
        maxlatV <- tclVar(.cdtData$EnvData$tmpMapOp$bbox$maxlat)

        txt.lon <- tklabel(frameBbox, text = "Longitude", anchor = 'e', justify = 'right', width = largeur7)
        txt.lat <- tklabel(frameBbox, text = "Latitude", anchor = 'e', justify = 'right')
        txt.min <- tklabel(frameBbox, text = "Minimum")
        txt.max <- tklabel(frameBbox, text = "Maximum")
        en.nlon <- tkentry(frameBbox, textvariable = minlonV, width = 8, justify = "right")
        en.xlon <- tkentry(frameBbox, textvariable = maxlonV, width = 8, justify = "right")
        en.nlat <- tkentry(frameBbox, textvariable = minlatV, width = 8, justify = "right")
        en.xlat <- tkentry(frameBbox, textvariable = maxlatV, width = 8, justify = "right")

        tkgrid(txt.min, row = 0, column = 1, sticky = "ew", rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
        tkgrid(txt.max, row = 0, column = 2, sticky = "ew", rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
        tkgrid(txt.lon, row = 1, column = 0, sticky = "ew", rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
        tkgrid(en.nlon, row = 1, column = 1, sticky = "ew", rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
        tkgrid(en.xlon, row = 1, column = 2, sticky = "ew", rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
        tkgrid(txt.lat, row = 2, column = 0, sticky = "ew", rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
        tkgrid(en.nlat, row = 2, column = 1, sticky = "ew", rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
        tkgrid(en.xlat, row = 2, column = 2, sticky = "ew", rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)

        ##############################################

        frameBox <- tkframe(subfr2)

        draw.box <- tclVar(.cdtData$EnvData$tmpMapOp$draw.box)

        chk.addbox <- tkcheckbutton(frameBox, variable = draw.box, text = lang.dlg[['checkbutton']][['1']], anchor = 'w', justify = 'left')

        tkgrid(chk.addbox, row = 0, column = 0, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1)

        ############################################

        tkgrid(frameMap, row = 0, column = 0, sticky = 'we', padx = 1, pady = 1, ipadx = 1, ipady = 1)
        tkgrid(frameBbox, row = 1, column = 0, sticky = 'we', padx = 1, pady = 3, ipadx = 1, ipady = 1)
        tkgrid(frameBox, row = 2, column = 0, sticky = 'we', padx = 1, pady = 1, ipadx = 1, ipady = 1)

    #######################################################################################################

    #Tab3
    subfr3 <- bwTabScrollableFrame(cmd.tab3)

        ##############################################

        frameSHP <- create_shpLayer_frame(subfr3)
        tkgrid(frameSHP, row = 0, column = 0, sticky = 'we', pady = 1)

    #######################################################################################################

    get.tmpMapOP.values <- function(){
        .cdtData$EnvData$tmpMapOp$bbox$minlon <- as.numeric(trimws(tclvalue(minlonV)))
        .cdtData$EnvData$tmpMapOp$bbox$maxlon <- as.numeric(trimws(tclvalue(maxlonV)))
        .cdtData$EnvData$tmpMapOp$bbox$minlat <- as.numeric(trimws(tclvalue(minlatV)))
        .cdtData$EnvData$tmpMapOp$bbox$maxlat <- as.numeric(trimws(tclvalue(maxlatV)))
        .cdtData$EnvData$tmpMapOp$draw.box <- switch(tclvalue(draw.box), '0' = FALSE, '1' = TRUE)
    }

    get.All.NCDF.Files <- function(){
        tkconfigure(.cdtEnv$tcl$main$win, cursor = 'watch')
        tcl('update')
        on.exit({
            tkconfigure(.cdtEnv$tcl$main$win, cursor = '')
            tcl('update')
        })

        nc.dir <- trimws(tclvalue(ncDIR))
        nc.format <- trimws(tclvalue(ncFormat))
        nc.sample <- trimws(tclvalue(ncSample))

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
            error.msg <- lang.dlg[['message']][['4']]
            nc.files <- ncFilesInfoSeq(nc.dir, nc.format, error.msg)

            if(is.null(nc.files)){
                tkconfigure(cb.nc.maps, values = "")
                tclvalue(ncdf.date.file) <- ""
                .cdtData$EnvData$loaded.nc <- NULL
                return(NULL)
            }

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

        nlon <- trimws(tclvalue(ncLON))
        nlat <- trimws(tclvalue(ncLAT))

        nl <- length(.cdtData$EnvData$ncvar)
        .cdtData$EnvData$ncData$MapOp <- vector(mode = 'list', length = nl)
        for(j in 1:nl) .cdtData$EnvData$ncData$MapOp[[j]] <- .cdtData$EnvData$ncvar[[j]]$ncops

        pvar <- sapply(.cdtData$EnvData$ncvar, function(x) ifelse(tclvalue(x$chk.var) == "1", TRUE, FALSE))
        if(!any(pvar)){
            Insert.Messages.Out(lang.dlg[['message']][['5']], TRUE, 'e')
            .cdtData$EnvData$ncData$map <- NULL
            return(NULL)
        }

        readNCFILE <- TRUE
        if(!is.null(.cdtData$EnvData$ncData))
            if(!is.null(.cdtData$EnvData$ncData$ncfile))
                if(.cdtData$EnvData$ncData$ncfile == ncfile.path &
                    all(.cdtData$EnvData$ncData$pvar == pvar)) readNCFILE <- FALSE

        if(readNCFILE){
            .cdtData$EnvData$ncData$map <- vector(mode = 'list', length = nl)

            nc <- try(ncdf4::nc_open(ncfile.path), silent = TRUE)
            if(inherits(nc, "try-error")){
                Insert.Messages.Out(paste(lang.dlg[['message']][['6']], ncfile.path), TRUE, 'e')
                .cdtData$EnvData$ncData$map <- NULL
                return(NULL)
            }
            for(j in which(pvar)){
                varid <- .cdtData$EnvData$ncvar[[j]]$var.name
                idim <- match(sapply(nc$var[[varid]]$dim, "[[", "name"), c(nlon, nlat))
                idim <- idim[!is.na(idim)]
                if(length(idim) != 2){
                    Insert.Messages.Out(paste(varid, lang.dlg[['message']][['7']]), TRUE, 'e')
                    .cdtData$EnvData$ncData$map <- NULL
                    return(NULL)
                }
                xlon <- nc$var[[varid]]$dim[[idim[1]]]$vals
                xlat <- nc$var[[varid]]$dim[[idim[2]]]$vals
                ncdon <- ncdf4::ncvar_get(nc, varid = varid)
                xo <- order(xlon)
                xlon <- xlon[xo]
                yo <- order(xlat)
                xlat <- xlat[yo]
                ncdon <- if(idim[1] < idim[2]) ncdon[xo, yo] else t(ncdon)[xo, yo]

                .cdtData$EnvData$ncData$map[[j]]$x <- xlon
                .cdtData$EnvData$ncData$map[[j]]$y <- xlat
                .cdtData$EnvData$ncData$map[[j]]$z <- ncdon
                .cdtData$EnvData$ncData$map[[j]]$title <- varid
            }
            ncdf4::nc_close(nc)

            .cdtData$EnvData$ncData$file2plot <- nc.file
            .cdtData$EnvData$ncData$ncfile <- ncfile.path
            .cdtData$EnvData$ncData$pvar <- pvar
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
