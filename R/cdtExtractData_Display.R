
plotMap4Extraction <- function(){
    xmin <- .cdtData$EnvData$ZoomXYval[1]
    xmax <- .cdtData$EnvData$ZoomXYval[2]
    ymin <- .cdtData$EnvData$ZoomXYval[3]
    ymax <- .cdtData$EnvData$ZoomXYval[4]

    if(is.na(xmin) | is.null(xmin) | is.infinite(xmin)){
        Insert.Messages.Out(.cdtData$EnvData[['message']][['1']], TRUE, 'e')
        return(NULL)
    }
    if(is.na(xmax) | is.null(xmax) | is.infinite(xmax)){
        Insert.Messages.Out(.cdtData$EnvData[['message']][['2']], TRUE, 'e')
        return(NULL)
    }
    if(is.na(ymin) | is.null(ymin) | is.infinite(ymin)){
        Insert.Messages.Out(.cdtData$EnvData[['message']][['3']], TRUE, 'e')
        return(NULL)
    }
    if(is.na(ymax) | is.null(ymax) | is.infinite(ymax)){
        Insert.Messages.Out(.cdtData$EnvData[['message']][['4']], TRUE, 'e')
        return(NULL)
    }

    #######
    opar <- graphics::par(mar = c(4, 4, 2, 2))
    graphics::plot(1, xlim = c(xmin, xmax), ylim = c(ymin, ymax), xlab = "", ylab = "", type = "n", xaxt = 'n', yaxt = 'n')
    graphics::lines(.cdtData$EnvData$ocrds)
    if(!is.null(.cdtData$EnvData$selectedPolygon))
        graphics::lines(.cdtData$EnvData$selectedPolygon, col = 'red', lwd = 2)

    graphics::abline(h = graphics::axTicks(2), v = graphics::axTicks(1) , col = "lightgray", lty = 3, lwd = 1.3)
    axlabs <- LatLonAxisLabels(graphics::axTicks(1), graphics::axTicks(2))
    graphics::axis(side = 1, at = graphics::axTicks(1), labels = axlabs$xaxl, tck = -0.01, cex.axis = 1.0)
    graphics::axis(side = 2, at = graphics::axTicks(2), labels = axlabs$yaxl, tck = -0.01, las = 1, cex.axis = 1.0)
    plt <- graphics::par("plt")
    usr <- graphics::par("usr")
    graphics::par(opar)

    return(list(par = c(plt, usr)))
}

#################################################################

displayMap4Extraction <- function(notebookTab){
    varplot <- c("parPlotSize1", "parPlotSize2", "parPlotSize3", "parPlotSize4",
                 "usrCoords1", "usrCoords2", "usrCoords3", "usrCoords4")
    parPltCrd <- stats::setNames(lapply(varplot, function(x) assign(x, tclVar(), envir = parent.frame())), varplot)

    plotIt <- function(){
        tkconfigure(.cdtEnv$tcl$main$win, cursor = 'watch')
        tcl('update')
        on.exit({
            tkconfigure(.cdtEnv$tcl$main$win, cursor = '')
            tcl('update')
        })

        op <- graphics::par(bg = "white")
        pltusr <- plotMap4Extraction()
        graphics::par(op)
        for(j in seq_along(varplot))
            tclvalue(parPltCrd[[varplot[j]]]) <- pltusr$par[j]
        return(0)
    }

    ###################################################################

    onglet <- imageNotebookTab_open(notebookTab, 'Extraction Map')
    hscale <- as.numeric(tclvalue(tkget(.cdtEnv$tcl$toolbar$spinH)))
    vscale <- as.numeric(tclvalue(tkget(.cdtEnv$tcl$toolbar$spinV)))

    canvas <- tkcanvas(onglet[[2]])
    tkgrid(canvas)

    img <- DisplayPlot(canvas, fun = plotIt, hscale = hscale, vscale = vscale)
    img_w <- as.double(tcl('image', 'width', img$image))
    img_h <- as.double(tcl('image', 'height', img$image))
    tkconfigure(canvas, width = img_w, height = img_h)
    tkcreate(canvas, "image", 0, 0, anchor = 'nw', image = img$image)
    tcl('raise', canvas)
    tcl('update')

    tkbind(canvas, "<Enter>", function(){
        if(tclvalue(.cdtData$EnvData$zoom$pressButP) == "1")
            tkconfigure(canvas, cursor = 'sizing')
        else if(tclvalue(.cdtData$EnvData$zoom$pressButM) == "1")
            tkconfigure(canvas, cursor = 'sizing')
        else if(tclvalue(.cdtData$EnvData$zoom$pressButRect) == "1")
            tkconfigure(canvas, cursor = 'sizing')
        else if(tclvalue(.cdtData$EnvData$zoom$pressButDrag) == "1")
            tkconfigure(canvas, cursor = 'hand1')
        else if(tclvalue(.cdtData$EnvData$pressGetCoords) == "1")
            tkconfigure(canvas, cursor = 'draped_box')
        else
            tkconfigure(canvas, cursor = 'crosshair')
    })

    tkbind(canvas, "<Leave>", function() tkconfigure(canvas, cursor = ''))

    ##########
    shpf <- .cdtData$EnvData$shpf
    .cdtData$EnvData$selectedPolygon <- NULL

    ## draw rectangle initial value
    .cdtEnv$tcl$lastX <- 0
    .cdtEnv$tcl$lastY <- 0

    ## zoom factor
    factZoom <- 0.2

    ##zoom rectangle
    rectZoomInit <- .cdtData$EnvData$ZoomXYval

    ## Pan Image
    panZoomInit <- c(0, 0, 0, 0, 0, 0)
    factPan <- 0.7

    ##########
    ## first click on map
    tkbind(canvas, "<Button-1>", function(W, x, y){
        ret <- getXYCoords(W, x, y, parPltCrd)
        tkdelete(W, 'rect')

        ##get coordinates or polygon id
        if(tclvalue(.cdtData$EnvData$pressGetCoords) == "1" & !ret$oin){
            .cdtData$EnvData$selectedPolygon <- NULL

            if(.cdtData$EnvData$type.extract == "point"){
                tclvalue(.cdtData$EnvData$minlonRect) <- round(ret$xc, 4)
                tclvalue(.cdtData$EnvData$maxlonRect) <- ''
                tclvalue(.cdtData$EnvData$minlatRect) <- round(ret$yc, 4)
                tclvalue(.cdtData$EnvData$maxlatRect) <- ''
                stateADD <- 'disabled'
                colorADD <- 'lightblue'
            }

            ##
            if(.cdtData$EnvData$type.extract == "rect"){
                pPressRect(W, x, y, width = 1, outline = "red")
                tclvalue(.cdtData$EnvData$minlonRect) <- round(ret$xc, 4)
                tclvalue(.cdtData$EnvData$minlatRect) <- round(ret$yc, 4)
                stateADD <- 'disabled'
                colorADD <- 'lightblue'
            }

            ##
            if(.cdtData$EnvData$type.extract == "poly"){
                xypts <- data.frame(x = ret$xc, y = ret$yc)
                xypts <- sf::st_as_sf(xypts, coords = c("x", "y"))
                idp <- sf::st_intersects(xypts, shpf)
                admin_name <- sf::st_drop_geometry(shpf)[idp[[1]], ]
                if(nrow(admin_name) == 0){
                    admin_name <- rep(NA, ncol(admin_name))
                }else{
                    admin_name <- c(t(admin_name[1, ]))
                }

                ids <- as.integer(tclvalue(tcl(.cdtData$EnvData$cb.shpAttr, 'current'))) + 1
                admin_name <- admin_name[ids]
                if(!is.na(admin_name)){
                    tclvalue(.cdtData$EnvData$namePoly) <- as.character(admin_name)
                    dat <- sf::st_drop_geometry(shpf)
                    .cdtData$EnvData$selectedPolygon <- getBoundaries(shpf[dat[, ids] == tclvalue(.cdtData$EnvData$namePoly), ])
                }
                stateADD <- 'disabled'
                colorADD <- 'lightblue'
            }

            ##
            if(.cdtData$EnvData$type.extract == 'mpoint'){
                tclvalue(.cdtData$EnvData$minlonRect) <- round(ret$xc, 4)
                tclvalue(.cdtData$EnvData$maxlonRect) <- ''
                tclvalue(.cdtData$EnvData$minlatRect) <- round(ret$yc, 4)
                tclvalue(.cdtData$EnvData$maxlatRect) <- ''
                stateADD <- 'normal'
                colorADD <- 'red'
            }

            ##
            if(.cdtData$EnvData$type.extract == 'mpoly'){
                xypts <- data.frame(x = ret$xc, y = ret$yc)
                xypts <- sf::st_as_sf(xypts, coords = c("x", "y"))
                idp <- sf::st_intersects(xypts, shpf)
                admin_name <- sf::st_drop_geometry(shpf)[idp[[1]], ]
                if(nrow(admin_name) == 0){
                    admin_name <- rep(NA, ncol(admin_name))
                }else{
                    admin_name <- c(t(admin_name[1, ]))
                }

                ids <- as.numeric(tclvalue(tcl(.cdtData$EnvData$cb.shpAttr, 'current'))) + 1
                admin_name <- admin_name[ids]
                if(!is.na(admin_name)){
                    tclvalue(.cdtData$EnvData$namePoly) <- as.character(admin_name)
                    dat <- sf::st_drop_geometry(shpf)
                    .cdtData$EnvData$selectedPolygon <- getBoundaries(shpf[dat[, ids] == tclvalue(.cdtData$EnvData$namePoly), ])
                }
                stateADD <- 'normal'
                colorADD <- 'red'
            }

            tkconfigure(.cdtData$EnvData$bt.ADDObj, relief = 'raised', bg = colorADD, state = stateADD)

            refreshPlot(W, img,
                        hscale = as.numeric(tclvalue(tkget(.cdtEnv$tcl$toolbar$spinH))),
                        vscale = as.numeric(tclvalue(tkget(.cdtEnv$tcl$toolbar$spinV)))
                        )
        }

        #Zoom plus
        if(tclvalue(.cdtData$EnvData$zoom$pressButP) == "1" & !ret$oin){
            rgX <- as.numeric(tclvalue(parPltCrd$usrCoords2)) - as.numeric(tclvalue(parPltCrd$usrCoords1))
            rgY <- as.numeric(tclvalue(parPltCrd$usrCoords4)) - as.numeric(tclvalue(parPltCrd$usrCoords3))
            shiftX <- rgX * (1 - factZoom)/2
            shiftY <- rgY * (1 - factZoom)/2
            xmin1 <- ret$xc - shiftX
            xmax1 <- ret$xc + shiftX
            ymin1 <- ret$yc - shiftY
            ymax1 <- ret$yc + shiftY

            .cdtData$EnvData$ZoomXYval <- c(xmin1, xmax1, ymin1, ymax1)

            tclvalue(.cdtData$EnvData$zoom$xx1) <- round(xmin1, 4)
            tclvalue(.cdtData$EnvData$zoom$xx2) <- round(xmax1, 4)
            tclvalue(.cdtData$EnvData$zoom$yy1) <- round(ymin1, 4)
            tclvalue(.cdtData$EnvData$zoom$yy2) <- round(ymax1, 4)

            refreshPlot(W, img,
                        hscale = as.numeric(tclvalue(tkget(.cdtEnv$tcl$toolbar$spinH))),
                        vscale = as.numeric(tclvalue(tkget(.cdtEnv$tcl$toolbar$spinV)))
                        )
        }

        #Zoom Moins
        if(tclvalue(.cdtData$EnvData$zoom$pressButM) == "1" & !ret$oin){
            rgX <- as.numeric(tclvalue(parPltCrd$usrCoords2)) - as.numeric(tclvalue(parPltCrd$usrCoords1))
            rgY <- as.numeric(tclvalue(parPltCrd$usrCoords4)) - as.numeric(tclvalue(parPltCrd$usrCoords3))
            shiftX <- rgX * (1 + factZoom)/2
            shiftY <- rgY * (1 + factZoom)/2
            xmin1 <- ret$xc - shiftX
            xmax1 <- ret$xc + shiftX
            ymin1 <- ret$yc - shiftY
            ymax1 <- ret$yc + shiftY

            if(xmin1< -180 | xmax1 > 180 | ymin1< -90 | ymax1 > 90){
                tclvalue(.cdtData$EnvData$zoom$pressButP) <- 0
                tclvalue(.cdtData$EnvData$zoom$pressButM) <- 0
                tclvalue(.cdtData$EnvData$zoom$pressButRect) <- 0
                tclvalue(.cdtData$EnvData$zoom$pressButDrag) <- 0
                
                tkconfigure(.cdtData$EnvData$zoom$btZoomP, relief = 'raised', bg = 'lightblue', state = 'normal')
                tkconfigure(.cdtData$EnvData$zoom$btZoomM, relief = 'raised', bg = 'lightblue', state = 'normal')
                tkconfigure(.cdtData$EnvData$zoom$btZoomRect, relief = 'raised', bg = 'lightblue', state = 'normal')
                tkconfigure(.cdtData$EnvData$zoom$btPanImg, relief = 'raised', bg = 'lightblue', state = 'normal')

                tkconfigure(W, cursor = 'crosshair')
            }else{
                .cdtData$EnvData$ZoomXYval <- c(xmin1, xmax1, ymin1, ymax1)

                tclvalue(.cdtData$EnvData$zoom$xx1) <- round(xmin1, 4)
                tclvalue(.cdtData$EnvData$zoom$xx2) <- round(xmax1, 4)
                tclvalue(.cdtData$EnvData$zoom$yy1) <- round(ymin1, 4)
                tclvalue(.cdtData$EnvData$zoom$yy2) <- round(ymax1, 4)

                refreshPlot(W, img,
                            hscale = as.numeric(tclvalue(tkget(.cdtEnv$tcl$toolbar$spinH))),
                            vscale = as.numeric(tclvalue(tkget(.cdtEnv$tcl$toolbar$spinV)))
                            )
            }
        }

        ##Zoom rectangle
        if(tclvalue(.cdtData$EnvData$zoom$pressButRect) == "1" & !ret$oin){
            pPressRect(W, x, y, width = 1, outline = "red")
            rectZoomInit[1] <<- ret$xc
            rectZoomInit[3] <<- ret$yc
        }

        ##Pan image
        if(tclvalue(.cdtData$EnvData$zoom$pressButDrag) == "1" & !ret$oin){
            panZoomInit[1] <<- ret$xc
            panZoomInit[2] <<- ret$yc

            panZoomInit[3] <<- as.numeric(tclvalue(.cdtData$EnvData$zoom$xx1))
            panZoomInit[4] <<- as.numeric(tclvalue(.cdtData$EnvData$zoom$xx2))
            panZoomInit[5] <<- as.numeric(tclvalue(.cdtData$EnvData$zoom$yy1))
            panZoomInit[6] <<- as.numeric(tclvalue(.cdtData$EnvData$zoom$yy2))

            tkconfigure(canvas, cursor = 'hand2')
        }
    })

    ##########
    ## cursor movement
    tkbind(canvas, "<Motion>", function(W, x, y){
        displayCursorPosition3Var(W, x, y, parPltCrd, getAdminLabel, shp = shpf,
                                    idField = .cdtData$EnvData$cb.shpAttr)
    })

    #########
    ## cursor movement with button-1 pressed
    tkbind(canvas, "<B1-Motion>", function(W, x, y){
        ret <- getXYCoords(W, x, y, parPltCrd)

        ##get coordinates rect
        if(tclvalue(.cdtData$EnvData$pressGetCoords) == "1" &
            .cdtData$EnvData$type.extract == "rect")
        {
            pMoveRect(W, x, y)
            tclvalue(.cdtData$EnvData$maxlonRect) <- round(ret$xc, 4)
            tclvalue(.cdtData$EnvData$maxlatRect) <- round(ret$yc, 4)
        }

        ##Zoom rectangle
        if(tclvalue(.cdtData$EnvData$zoom$pressButRect) == "1"){
            pMoveRect(W, x, y)
        }

        ##Pan image
        if(tclvalue(.cdtData$EnvData$zoom$pressButDrag) == "1"){
            transX <- ret$xc - panZoomInit[1]
            transY <- ret$yc - panZoomInit[2]

            tclvalue(.cdtData$EnvData$zoom$xx1) <- round(panZoomInit[3] + factPan * transX, 4)
            tclvalue(.cdtData$EnvData$zoom$xx2) <- round(panZoomInit[4] + factPan * transX, 4)
            tclvalue(.cdtData$EnvData$zoom$yy1) <- round(panZoomInit[5] + factPan * transY, 4)
            tclvalue(.cdtData$EnvData$zoom$yy2) <- round(panZoomInit[6] + factPan * transY, 4)

            .cdtData$EnvData$ZoomXYval <- as.numeric(c(
                                                        tclvalue(.cdtData$EnvData$zoom$xx1),
                                                        tclvalue(.cdtData$EnvData$zoom$xx2),
                                                        tclvalue(.cdtData$EnvData$zoom$yy1),
                                                        tclvalue(.cdtData$EnvData$zoom$yy2)
                                                    ))
            # refreshPlot(W, img,
            #             hscale = as.numeric(tclvalue(tkget(.cdtEnv$tcl$toolbar$spinH))),
            #             vscale = as.numeric(tclvalue(tkget(.cdtEnv$tcl$toolbar$spinV)))
            #             )
        }
    })

    #########
    ## release button1
    tkbind(canvas, "<ButtonRelease>", function(W, x, y){
        ret <- getXYCoords(W, x, y, parPltCrd)

        ##get coordinates rect
        if(tclvalue(.cdtData$EnvData$pressGetCoords) == "1"){
            if(.cdtData$EnvData$type.extract == "rect")
            {
                xpr <- c(as.numeric(tclvalue(.cdtData$EnvData$minlonRect)), round(ret$xc, 4),
                        as.numeric(tclvalue(.cdtData$EnvData$minlatRect)), round(ret$yc, 4))
                if(xpr[1] > xpr[2]) xpr <- xpr[c(2, 1, 3, 4)]
                if(xpr[3] > xpr[4]) xpr <- xpr[c(1, 2, 4, 3)]

                tclvalue(.cdtData$EnvData$minlonRect) <- xpr[1]
                tclvalue(.cdtData$EnvData$maxlonRect) <- xpr[2]
                tclvalue(.cdtData$EnvData$minlatRect) <- xpr[3]
                tclvalue(.cdtData$EnvData$maxlatRect) <- xpr[4]
            }

            tclvalue(.cdtData$EnvData$pressGetCoords) <- 0
            tkconfigure(.cdtData$EnvData$bt.GETArea, relief = 'raised', bg = 'lightblue', state = 'normal')

            tkconfigure(W, cursor = 'crosshair')
        }

        ##Zoom rectangle
        if(tclvalue(.cdtData$EnvData$zoom$pressButRect) == "1"){
            rectZoomInit[2] <<- ret$xc
            rectZoomInit[4] <<- ret$yc
            if(rectZoomInit[1] > rectZoomInit[2]) rectZoomInit <- rectZoomInit[c(2, 1, 3, 4)]
            if(rectZoomInit[3] > rectZoomInit[4]) rectZoomInit <- rectZoomInit[c(1, 2, 4, 3)]
            .cdtData$EnvData$ZoomXYval <- rectZoomInit

            tclvalue(.cdtData$EnvData$zoom$xx1) <- round(rectZoomInit[1], 4)
            tclvalue(.cdtData$EnvData$zoom$xx2) <- round(rectZoomInit[2], 4)
            tclvalue(.cdtData$EnvData$zoom$yy1) <- round(rectZoomInit[3], 4)
            tclvalue(.cdtData$EnvData$zoom$yy2) <- round(rectZoomInit[4], 4)

            refreshPlot(W, img,
                        hscale = as.numeric(tclvalue(tkget(.cdtEnv$tcl$toolbar$spinH))),
                        vscale = as.numeric(tclvalue(tkget(.cdtEnv$tcl$toolbar$spinV)))
                        )
            tkdelete(W, 'rect')
        }

        ##Pan image
        if(tclvalue(.cdtData$EnvData$zoom$pressButDrag) == "1"){
            refreshPlot(W, img,
                        hscale = as.numeric(tclvalue(tkget(.cdtEnv$tcl$toolbar$spinH))),
                        vscale = as.numeric(tclvalue(tkget(.cdtEnv$tcl$toolbar$spinV)))
                        )

            tkconfigure(canvas, cursor = 'hand1')
        }

        tcl('update')
    })

    ###############################################
    ## deactivate zoom (right button)
    tkbind(canvas, "<Button-3>", function(W){
        tclvalue(.cdtData$EnvData$zoom$pressButP) <- 0
        tclvalue(.cdtData$EnvData$zoom$pressButM) <- 0
        tclvalue(.cdtData$EnvData$zoom$pressButRect) <- 0
        tclvalue(.cdtData$EnvData$zoom$pressButDrag) <- 0

        tkconfigure(.cdtData$EnvData$zoom$btZoomP, relief = 'raised', bg = 'lightblue', state = 'normal')
        tkconfigure(.cdtData$EnvData$zoom$btZoomM, relief = 'raised', bg = 'lightblue', state = 'normal')
        tkconfigure(.cdtData$EnvData$zoom$btZoomRect, relief = 'raised', bg = 'lightblue', state = 'normal')
        tkconfigure(.cdtData$EnvData$zoom$btPanImg, relief = 'raised', bg = 'lightblue', state = 'normal')

        tkconfigure(.cdtData$EnvData$bt.GETArea, relief = 'raised', bg = 'lightblue', state = 'normal')
        stateADD <- if(.cdtData$EnvData$type.extract %in% c('mpoint', 'mpoly')) "normal" else "disabled"
        tkconfigure(.cdtData$EnvData$bt.ADDObj, relief = 'raised', bg = 'lightblue', state = stateADD)

        tkconfigure(canvas, cursor = 'crosshair')

        tkdelete(W, 'rect')

        .cdtData$EnvData$selectedPolygon <- NULL
        refreshPlot(W, img,
                    hscale = as.numeric(tclvalue(tkget(.cdtEnv$tcl$toolbar$spinH))),
                    vscale = as.numeric(tclvalue(tkget(.cdtEnv$tcl$toolbar$spinV)))
                    )
    })

    ###
    return(list(onglet, list(canvas, img)))
}
