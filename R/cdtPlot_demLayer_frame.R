
get_demLayer_data <- function(){
    dem_data <- .cdtData$EnvData$dem
    don_dem <- NULL
    if(tclvalue(dem_data$adddem) == "1"){
        if(!is.null(dem_data$dem)){
            don_dem <- dem_data$dem
        }
    }

    return(don_dem)
}

create_demLayer_frame <- function(parent_frame){
    if(WindowsOS()){
        largeur1 <- 31
    }else{
        largeur1 <- 32
    }

    .cdtData$EnvData$dem$Opt <- list(
                                    user.colors = list(custom = FALSE, color = NULL),
                                    user.levels = list(custom = FALSE, levels = NULL, equidist = FALSE),
                                    preset.colors = list(color = 'qc.gray.colors', reverse = FALSE)
                                )

    xml.dlg <- file.path(.cdtDir$dirLocal, "languages", "cdtPlot_demLayer_frame.xml")
    lang.dlg <- cdtLanguageParse(xml.dlg, .cdtData$Config$lang.iso)

    ########

    frameDEM <- ttklabelframe(parent_frame, text = lang.dlg[['label']][['1']], relief = 'groove')

    .cdtData$EnvData$dem$adddem <- tclVar(FALSE)
    file.plotDem <- tclVar()
    stateDEM <- "disabled"

    chk.adddem <- tkcheckbutton(frameDEM, variable = .cdtData$EnvData$dem$adddem, text = lang.dlg[['checkbutton']][['1']], anchor = 'w', justify = 'left')
    bt.adddemOpt <- ttkbutton(frameDEM, text = .cdtEnv$tcl$lang$global[['button']][['4']], state = stateDEM)
    cb.adddem <- ttkcombobox(frameDEM, values = unlist(openFile_ttkcomboList()), textvariable = file.plotDem, width = largeur1, state = stateDEM)
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
            tclvalue(file.plotDem) <- nc.opfiles[[1]]
            tkconfigure(cb.adddem, values = unlist(openFile_ttkcomboList()))

            demInfo <- getNCDFSampleData(trimws(tclvalue(file.plotDem)))
            if(is.null(demInfo)){
                Insert.Messages.Out(lang.dlg[['message']][['1']], TRUE, "e")
                tclvalue(file.plotDem) <- ""
                .cdtData$EnvData$dem$dem <- NULL
            }else{
                jfile <- getIndex.AllOpenFiles(trimws(tclvalue(file.plotDem)))
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
        demInfo <- getNCDFSampleData(trimws(tclvalue(file.plotDem)))
        if(is.null(demInfo)){
            Insert.Messages.Out(lang.dlg[['message']][['1']], TRUE, "e")
            .cdtData$EnvData$dem$dem <- NULL
        }else{
            jfile <- getIndex.AllOpenFiles(trimws(tclvalue(file.plotDem)))
            ncdata <- .cdtData$OpenFiles$Data[[jfile]][[2]]
            .cdtData$EnvData$dem$dem <- ncdata[c('x', 'y', 'z')]
        }
    })

    tkbind(cb.adddem, "<Button-1>", function(){
        tkconfigure(cb.adddem, values = unlist(openFile_ttkcomboList()))
    })

    tkbind(chk.adddem, "<Button-1>", function(){
        stateDEM <- if(tclvalue(.cdtData$EnvData$dem$adddem) == "1") "disabled" else "normal"
        tkconfigure(cb.adddem, state = stateDEM)
        tkconfigure(bt.adddem, state = stateDEM)
        tkconfigure(bt.adddemOpt, state = stateDEM)
    })

    return(frameDEM)
}
