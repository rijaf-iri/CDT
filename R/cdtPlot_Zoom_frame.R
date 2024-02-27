
initialize_zoom_frame <- function(xlim, ylim){
    lo1 <- round(xlim[1], 4)
    lo2 <- round(xlim[2], 4)
    la1 <- round(ylim[1], 4)
    la2 <- round(ylim[2], 4)
    .cdtData$EnvData$ZoomXYval0 <- c(lo1, lo2, la1, la2)

    tclvalue(.cdtData$EnvData$zoom$xx1) <- lo1
    tclvalue(.cdtData$EnvData$zoom$xx2) <- lo2
    tclvalue(.cdtData$EnvData$zoom$yy1) <- la1
    tclvalue(.cdtData$EnvData$zoom$yy2) <- la2

    .cdtData$EnvData$ZoomXYval <- as.numeric(c(tclvalue(.cdtData$EnvData$zoom$xx1),
                                               tclvalue(.cdtData$EnvData$zoom$xx2),
                                               tclvalue(.cdtData$EnvData$zoom$yy1),
                                               tclvalue(.cdtData$EnvData$zoom$yy2)))
}

get_zoom_xylimits <- function(){
    xmin <- .cdtData$EnvData$ZoomXYval[1]
    xmax <- .cdtData$EnvData$ZoomXYval[2]
    ymin <- .cdtData$EnvData$ZoomXYval[3]
    ymax <- .cdtData$EnvData$ZoomXYval[4]

    if(is.na(xmin) | is.null(xmin) | is.infinite(xmin)){
        Insert.Messages.Out(.cdtData$EnvData$ZoomMsg[['1']], TRUE, 'e')
        return(NULL)
    }
    if(is.na(xmax) | is.null(xmax) | is.infinite(xmax)){
        Insert.Messages.Out(.cdtData$EnvData$ZoomMsg[['2']], TRUE, 'e')
        return(NULL)
    }
    if(is.na(ymin) | is.null(ymin) | is.infinite(ymin)){
        Insert.Messages.Out(.cdtData$EnvData$ZoomMsg[['3']], TRUE, 'e')
        return(NULL)
    }
    if(is.na(ymax) | is.null(ymax) | is.infinite(ymax)){
        Insert.Messages.Out(.cdtData$EnvData$ZoomMsg[['4']], TRUE, 'e')
        return(NULL)
    }

    return(list(xlim = c(xmin, xmax), ylim = c(ymin, ymax)))
}

create_zoom_frame <- function(parent_frame, notebookTab){
    xml.dlg <- file.path(.cdtDir$dirLocal, "languages", "cdtPlot_Zoom_frame.xml")
    lang.dlg <- cdtLanguageParse(xml.dlg, .cdtData$Config$lang.iso)

    .cdtData$EnvData$ZoomMsg <- lang.dlg[['message']]

    #######################

    .cdtData$EnvData$zoom$xx1 <- tclVar()
    .cdtData$EnvData$zoom$xx2 <- tclVar()
    .cdtData$EnvData$zoom$yy1 <- tclVar()
    .cdtData$EnvData$zoom$yy2 <- tclVar()

    .cdtData$EnvData$zoom$pressButP <- tclVar(0)
    .cdtData$EnvData$zoom$pressButM <- tclVar(0)
    .cdtData$EnvData$zoom$pressButRect <- tclVar(0)
    .cdtData$EnvData$zoom$pressButDrag <- tclVar(0)

    .cdtData$EnvData$ZoomXYval0 <- NULL

    #######################

    frameZoom <- ttklabelframe(parent_frame, text = lang.dlg[['label']][['1']], relief = 'groove')

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

    helpWidget(.cdtData$EnvData$zoom$btZoomP, lang.dlg[['tooltip']][['1']], lang.dlg[['status']][['1']])
    helpWidget(.cdtData$EnvData$zoom$btZoomM, lang.dlg[['tooltip']][['2']], lang.dlg[['status']][['2']])
    helpWidget(.cdtData$EnvData$zoom$btZoomRect, lang.dlg[['tooltip']][['3']], lang.dlg[['status']][['3']])
    helpWidget(.cdtData$EnvData$zoom$btPanImg, lang.dlg[['tooltip']][['4']], lang.dlg[['status']][['4']])
    helpWidget(.cdtData$EnvData$zoom$btRedraw, lang.dlg[['tooltip']][['5']], lang.dlg[['status']][['5']])
    helpWidget(.cdtData$EnvData$zoom$btReset, lang.dlg[['tooltip']][['6']], lang.dlg[['status']][['6']])

    #######################

    tkconfigure(.cdtData$EnvData$zoom$btRedraw, command = function(){
        .cdtData$EnvData$ZoomXYval <- as.numeric(c(tclvalue(.cdtData$EnvData$zoom$xx1), tclvalue(.cdtData$EnvData$zoom$xx2),
                                                tclvalue(.cdtData$EnvData$zoom$yy1), tclvalue(.cdtData$EnvData$zoom$yy2)))

        # ZoomXYval
        tabid <- as.numeric(tclvalue(tkindex(.cdtEnv$tcl$main$tknotes, 'current'))) + 1
        if(length(.cdtData$OpenTab$Type) > 0){
            if(.cdtData$OpenTab$Type[[tabid]] == "img" & !is.null(notebookTab))
            {
                if(.cdtData$OpenTab$Data[[tabid]][[1]][[1]]$ID == notebookTab[[2]])
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
        ZoomXYval0 <- .cdtData$EnvData$ZoomXYval0
        .cdtData$EnvData$ZoomXYval <- ZoomXYval0
        tclvalue(.cdtData$EnvData$zoom$xx1) <- ZoomXYval0[1]
        tclvalue(.cdtData$EnvData$zoom$xx2) <- ZoomXYval0[2]
        tclvalue(.cdtData$EnvData$zoom$yy1) <- ZoomXYval0[3]
        tclvalue(.cdtData$EnvData$zoom$yy2) <- ZoomXYval0[4]

        # ZoomXYval
        tabid <- as.numeric(tclvalue(tkindex(.cdtEnv$tcl$main$tknotes, 'current'))) + 1
        if(length(.cdtData$OpenTab$Type) > 0){
            if(.cdtData$OpenTab$Type[[tabid]] == "img" & !is.null(notebookTab))
            {
                if(.cdtData$OpenTab$Data[[tabid]][[1]][[1]]$ID == notebookTab[[2]])
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

    #######################

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

    #######################

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

    #######################

    return(frameZoom)
}
