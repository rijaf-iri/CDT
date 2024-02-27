
create_shpLayer_frame <- function(parent_frame){
    if(WindowsOS()){
        largeur1 <- 31
    }else{
        largeur1 <- 32
    }

    xml.dlg <- file.path(.cdtDir$dirLocal, "languages", "cdtPlot_Shapefile_frame.xml")
    lang.dlg <- cdtLanguageParse(xml.dlg, .cdtData$Config$lang.iso)

    .cdtData$EnvData$shapefile$options <- list(col = "black", lwd = 1.5)

    #######################

    frameSHP <- ttklabelframe(parent_frame, text = lang.dlg[['label']][['1']], relief = 'groove')

    .cdtData$EnvData$shapefile$addshp <- tclVar(FALSE)
    file.plotShp <- tclVar()
    stateSHP <- "disabled"

    chk.addshp <- tkcheckbutton(frameSHP, variable = .cdtData$EnvData$shapefile$addshp, text = lang.dlg[['checkbutton']][['1']], anchor = 'w', justify = 'left')
    bt.addshpOpt <- ttkbutton(frameSHP, text = .cdtEnv$tcl$lang$global[['button']][['4']], state = stateSHP)
    cb.addshp <- ttkcombobox(frameSHP, values = unlist(openFile_ttkcomboList()), textvariable = file.plotShp, width = largeur1, state = stateSHP)
    bt.addshp <- tkbutton(frameSHP, text = "...", state = stateSHP)

    ########
    tkgrid(chk.addshp, row = 0, column = 0, sticky = 'we', rowspan = 1, columnspan = 6, padx = 1, pady = 1)
    tkgrid(bt.addshpOpt, row = 0, column = 6, sticky = 'we', rowspan = 1, columnspan = 2, padx = 1, pady = 1)
    tkgrid(cb.addshp, row = 1, column = 0, sticky = 'we', rowspan = 1, columnspan = 7, padx = 1, pady = 1)
    tkgrid(bt.addshp, row = 1, column = 7, sticky = 'w', rowspan = 1, columnspan = 1, padx = 0, pady = 1)

    ########

    tkconfigure(bt.addshp, command = function(){
        listOpenFiles <- openFile_ttkcomboList()
        shp.opfiles <- getOpenShp(.cdtEnv$tcl$main$win)
        if(!is.null(shp.opfiles)){
            update.OpenFiles('shp', shp.opfiles)
            tclvalue(file.plotShp) <- shp.opfiles[[1]]
            listOpenFiles[[length(listOpenFiles) + 1]] <- shp.opfiles[[1]]
            tkconfigure(cb.addshp, values = unlist(listOpenFiles))

            shpofile <- getShpOpenData(file.plotShp)
            if(is.null(shpofile)){
                .cdtData$EnvData$shapefile$ocrds <- NULL
            }else{
                .cdtData$EnvData$shapefile$ocrds <- getBoundaries(shpofile[[2]])
            }
        }
    })

    tkconfigure(bt.addshpOpt, command = function(){
        .cdtData$EnvData$shapefile$options <- MapGraph.GraphOptions.LineSHP(.cdtData$EnvData$shapefile$options)
    })

    tkbind(cb.addshp, "<<ComboboxSelected>>", function(){
        shpofile <- getShpOpenData(file.plotShp)
        if(is.null(shpofile)){
            .cdtData$EnvData$shapefile$ocrds <- NULL
        }else{
            .cdtData$EnvData$shapefile$ocrds <- getBoundaries(shpofile[[2]])
        }
    })

    tkbind(cb.addshp, "<Button-1>", function(){
        tkconfigure(cb.addshp, values = unlist(openFile_ttkcomboList()))
    })

    tkbind(chk.addshp, "<Button-1>", function(){
        stateSHP <- if(tclvalue(.cdtData$EnvData$shapefile$addshp) == "1") "disabled" else "normal"
        tkconfigure(cb.addshp, state = stateSHP)
        tkconfigure(bt.addshp, state = stateSHP)
        tkconfigure(bt.addshpOpt, state = stateSHP)
    })

    return(frameSHP)
}
