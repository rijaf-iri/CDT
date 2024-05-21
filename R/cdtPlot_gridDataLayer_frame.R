
get_gridDataLayer_data <- function(date){
    ######
    tstep <- .cdtData$EnvData$output$params$intstep
    ######

    year <- substr(date, 1, 4)
    mon <- substr(date, 5, 6)
    pendek <- substr(date, 7, 7)
    day <- substr(date, 7, 8)

    ######
    grid_data <- .cdtData$EnvData$grd_data
    don_grd <- NULL
    if(tclvalue(grid_data$addgrid) == "1"){
        if(!is.null(grid_data$data)){
            ncfile <- switch(tstep,
                'daily' = sprintf(grid_data$format, year, mon, day),
                'pentad' = sprintf(grid_data$format, year, mon, pendek),
                'dekadal' = sprintf(grid_data$format, year, mon, pendek),
                'monthly' = sprintf(grid_data$format, year, mon)
            )

            ncfile <- file.path(grid_data$dir, ncfile)
            if(file.exists(ncfile)){
                ncinfo <- grid_data$data
                nc <- ncdf4::nc_open(ncfile)
                don_grd <- ncdf4::ncvar_get(nc, varid = ncinfo$varid)
                ncdf4::nc_close(nc)
                don_grd <- transposeNCDFData(don_grd, ncinfo)
                don_grd <- list(x = ncinfo$lon, y = ncinfo$lat, z = don_grd)
                .cdtData$EnvData$grd_data$don <- don_grd
            }else{
            	msg <- paste(ncfile, grid_data$msg[['2']])
                Insert.Messages.Out(msg, TRUE, 'e')
            }
        }
    }

    return(don_grd)
}

create_gridDataLayer_frame <- function(parent_frame){
    if(WindowsOS()){
        largeur2 <- 33
    }else{
        largeur2 <- 33
    }

    .cdtData$EnvData$grd_data$dir <- ""
    .cdtData$EnvData$grd_data$sample <- ""
    .cdtData$EnvData$grd_data$format <- "grd_%s%s%s.nc"
    .cdtData$EnvData$grd_data$data <- NULL
    .cdtData$EnvData$grd_data$Opt <- list(
                                    user.colors = list(custom = FALSE, color = NULL),
                                    user.levels = list(custom = FALSE, levels = NULL, equidist = FALSE),
                                    preset.colors = list(color = 'qcrr.grid.colors', reverse = FALSE)
                                )

    xml.dlg <- file.path(.cdtDir$dirLocal, "languages", "cdtPlot_gridDataLayer_frame.xml")
    lang.dlg <- cdtLanguageParse(xml.dlg, .cdtData$Config$lang.iso)

    .cdtData$EnvData$grd_data$msg <- lang.dlg[['message']]

    ########

    frameGRID <- ttklabelframe(parent_frame, text = lang.dlg[['label']][['1']], relief = 'groove')

    .cdtData$EnvData$grd_data$addgrid <- tclVar(FALSE)
    dir.plotSat <- tclVar()
    stateSAT <- "disabled"

    chk.addSat <- tkcheckbutton(frameGRID, variable = .cdtData$EnvData$grd_data$addgrid, text = lang.dlg[['checkbutton']][['1']], anchor = 'w', justify = 'left')
    bt.addSatOpt <- ttkbutton(frameGRID, text = .cdtEnv$tcl$lang$global[['button']][['4']], state = stateSAT)
    txt.addSat <- tklabel(frameGRID, text = lang.dlg[['label']][['2']], anchor = 'w', justify = 'left')
    bt.addSatSet <- ttkbutton(frameGRID, text = .cdtEnv$tcl$lang$global[['button']][['5']], state = stateSAT)
    en.addSat <- tkentry(frameGRID, textvariable = dir.plotSat, width = largeur2, state = stateSAT)
    bt.addSat <- tkbutton(frameGRID, text = "...", state = stateSAT)

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

        .cdtData$EnvData$grd_data$dir <- trimws(tclvalue(dir.plotSat))
    })

    tkconfigure(bt.addSatSet, command = function(){
        ######
        tstep <- .cdtData$EnvData$output$params$intstep
        ######

        TSTEPVAL0 <- .cdtEnv$tcl$lang$global[['combobox']][['1']][3:6]
        periodVAL <- c('daily', 'pentad', 'dekadal', 'monthly')
        timeSteps <- if(is.null(tstep)) TSTEPVAL0[1] else TSTEPVAL0[periodVAL %in% tstep]

        .cdtData$EnvData$grd_data <- getInfoNetCDFData(.cdtEnv$tcl$main$win, .cdtData$EnvData$grd_data,
                                                       trimws(tclvalue(dir.plotSat)))

        sdon <- getNCDFSampleData(.cdtData$EnvData$grd_data$sample)
        if(is.null(sdon)){
            Insert.Messages.Out(lang.dlg[['message']][['1']], TRUE, "e")
            .cdtData$EnvData$grd_data$data <- NULL
        }else .cdtData$EnvData$grd_data$data <- sdon
    })

    tkconfigure(bt.addSatOpt, command = function(){
        if(!is.null(.cdtData$EnvData$grd_data$don)){
            atlevel <- pretty(.cdtData$EnvData$grd_data$don$z, n = 10, min.n = 7)
            if(is.null(.cdtData$EnvData$grd_data$Opt$user.levels$levels)){
                .cdtData$EnvData$grd_data$Opt$user.levels$levels <- atlevel
            }else{
                if(!.cdtData$EnvData$grd_data$Opt$user.levels$custom)
                    .cdtData$EnvData$grd_data$Opt$user.levels$levels <- atlevel
            }
        }

        .cdtData$EnvData$grd_data$Opt <- MapGraph.gridDataLayer(.cdtData$EnvData$grd_data$Opt)
    })

    ########
    tkbind(chk.addSat, "<Button-1>", function(){
        stateSAT <- if(tclvalue(.cdtData$EnvData$grd_data$addgrid) == "1") "disabled" else "normal"
        tkconfigure(en.addSat, state = stateSAT)
        tkconfigure(bt.addSat, state = stateSAT)
        tkconfigure(bt.addSatSet, state = stateSAT)
        tkconfigure(bt.addSatOpt, state = stateSAT)
    })

    return(frameGRID)
}
