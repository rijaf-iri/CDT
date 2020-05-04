
CDTdataset.Plot.Map <- function(){
    don <- .cdtData$EnvData$map
    ocrds <- .cdtData$EnvData$shp$ocrds

    if(is.null(don) & !is.null(ocrds)){
        xlim <- range(ocrds[, 1], na.rm = TRUE)
        ylim <- range(ocrds[, 2], na.rm = TRUE)
    }
    if(!is.null(don) & is.null(ocrds)){
        xlim <- range(don$x, na.rm = TRUE)
        ylim <- range(don$y, na.rm = TRUE)
    }
    if(!is.null(don) & !is.null(ocrds)){
        xlim <- range(range(don$x, na.rm = TRUE), range(ocrds[, 1], na.rm = TRUE))
        ylim <- range(range(don$y, na.rm = TRUE), range(ocrds[, 2], na.rm = TRUE))
    }

    opar <- par(mar = c(4, 4, 2.5, 2.5))
    plot(1, xlim = xlim, ylim = ylim, xlab = "", ylab = "", type = "n", xaxt = 'n', yaxt = 'n')
    axlabs <- LatLonAxisLabels(axTicks(1), axTicks(2))
    axis(side = 1, at = axTicks(1), labels = axlabs$xaxl, tcl = -0.2, cex.axis = 0.8)
    axis(side = 2, at = axTicks(2), labels = axlabs$yaxl, tcl = -0.2, las = 1, cex.axis = 0.8)

    if(!is.null(don)){
        breaks <- pretty(don$z, n = 10, min.n = 5)
        breaks <- if(length(breaks) > 0) breaks else c(0, 1) 
        kolor <- tim.colors(length(breaks) - 1)
        # image(don, breaks = breaks, col = kolor, xaxt = 'n', yaxt = 'n', add = TRUE)
        .filled.contour(don$x, don$y, don$z, levels = breaks, col = kolor)
    }

    abline(h = axTicks(2), v = axTicks(1), col = "lightgray", lty = 3)
    if(!is.null(ocrds)) lines(ocrds[, 1], ocrds[, 2], lwd = 1, col = "black")

    plt <- par("plt")
    usr <- par("usr")
    par(opar)
    return(list(par = c(plt, usr)))
}

#######################################

CDTdataset.Plot.Graph <- function(){
    cdtdataset <- .cdtData$EnvData$cdtdataset
    fileInfo <- cdtdataset$fileInfo

    xloc <- as.numeric(str_trim(tclvalue(.cdtData$EnvData$plot.maps$lonLOC)))
    yloc <- as.numeric(str_trim(tclvalue(.cdtData$EnvData$plot.maps$latLOC)))
    padx <- as.numeric(str_trim(tclvalue(.cdtData$EnvData$plot.maps$lonPAD)))
    pady <- as.numeric(str_trim(tclvalue(.cdtData$EnvData$plot.maps$latPAD)))

    xyloc <- cdtdataset.extarct.TS(cdtdataset, fileInfo, xloc, yloc, padx, pady)
    if(is.null(xyloc)) return(NULL)
    don <- as.numeric(xyloc$data)
    daty <- xyloc$date

    nchar.daty <- nchar(daty[1])
    known.data.tstep <- switch(cdtdataset$TimeStep,
                            "minute" = nchar.daty == 12,
                            "hourly" = nchar.daty == 10,
                            "daily" = nchar.daty == 8,
                            "pentad" = nchar.daty == 7,
                            "dekadal" = nchar.daty == 7,
                            "monthly" = nchar.daty == 6,
                            FALSE)
    known.data.tstep <- known.data.tstep && !grepl("[^[:digit:]]", daty[1])

    if(known.data.tstep){
        if(cdtdataset$TimeStep == "minute") daty <- as.POSIXct(daty, format = "%Y%m%d%H%M")
        if(cdtdataset$TimeStep == "hourly") daty <- as.POSIXct(daty, "%Y%m%d%H")
        if(cdtdataset$TimeStep == "daily") daty <- as.Date(daty, "%Y%m%d")
        if(cdtdataset$TimeStep == "pentad"){
            pen <- c(1, 6, 11, 16, 21, 26)[as.numeric(substr(daty, 7, 7))]
            daty <- as.Date(paste0(substr(daty, 1, 6), pen), "%Y%m%d")
        }
        if(cdtdataset$TimeStep == "dekadal"){
            dek <- c(1, 11, 21)[as.numeric(substr(daty, 7, 7))]
            daty <- as.Date(paste0(substr(daty, 1, 6), dek), "%Y%m%d")
        }
        if(cdtdataset$TimeStep == "monthly") daty <- as.Date(paste0(daty, 1), "%Y%m%d")
    }else daty <- seq_along(daty)

    timestep <- if(known.data.tstep) cdtdataset$TimeStep else "others"
    titre <- cdtdataset$varInfo$longname
    location <- paste0("Longitude: ", round(xloc, 5), ", Latitude: ", round(yloc, 5))

    #######
    TSGraphOp <- .cdtData$EnvData$TSGraphOp

    GRAPHTYPE <- str_trim(tclvalue(.cdtData$EnvData$plot.maps$typeTSp))
    if(GRAPHTYPE == "Line") optsgph <- TSGraphOp$line
    if(GRAPHTYPE == "Barplot") optsgph <- TSGraphOp$bar

    xlim <- range(daty, na.rm = TRUE)
    if(timestep != "others"){
        if(optsgph$xlim$is.min){
            xx <- format.xlim.date.range(optsgph$xlim$min, timestep, .cdtData$EnvData$message)
            if(is.null(xx)) return(NULL)
            xlim[1] <- xx
        }

        if(optsgph$xlim$is.max){
            xx <- format.xlim.date.range(optsgph$xlim$max, timestep, .cdtData$EnvData$message)
            if(is.null(xx)) return(NULL)
            xlim[2] <- xx
        }
    }else{
        if(optsgph$xlim$is.min){
            xx <- str_trim(optsgph$xlim$min)
            if(grepl("[^[:digit:]]", xx)){
                Insert.Messages.Out(.cdtData$EnvData$message[['7']], TRUE, "e")
                return(NULL)
            }
            xlim[1] <- as.numeric(xx)
        }
        if(optsgph$xlim$is.max){
            xx <- str_trim(optsgph$xlim$max)
            if(grepl("[^[:digit:]]", xx)){
                Insert.Messages.Out(.cdtData$EnvData$message[['7']], TRUE, "e")
                return(NULL)
            }
            xlim[2] <- as.numeric(xx)
        }
    }

    idt <- daty >= xlim[1] & daty <= xlim[2]
    daty <- daty[idt]
    don <- don[idt]
    ylim <- range(pretty(don))
    if(optsgph$ylim$is.min) ylim[1] <- optsgph$ylim$min
    if(optsgph$ylim$is.max) ylim[2] <- optsgph$ylim$max

    xlab <- if(optsgph$axislabs$is.xlab) optsgph$axislabs$xlab else ''
    ylab <- if(optsgph$axislabs$is.ylab) optsgph$axislabs$ylab else ''

    if(optsgph$title$is.title){
        titre <- optsgph$title$title
        titre.pos <- optsgph$title$position
    }else{
        titre <- titre
        titre.pos <- "top"
    }

    #######

    if(GRAPHTYPE == "Line"){
        graphs.plot.line(daty, don, xlim = xlim, ylim = ylim,
                        xlab = xlab, ylab = ylab, ylab.sub = NULL,
                        title = titre, title.position = titre.pos, axis.font = 1,
                        plotl = optsgph$plot, legends = NULL,
                        location = location)
    }

    if(GRAPHTYPE == "Barplot"){
        graphs.plot.bar(daty, don, xlim = xlim, ylim = ylim, origindate = NULL,
                        xlab = xlab, ylab = ylab, ylab.sub = NULL,
                        title = titre, title.position = titre.pos, axis.font = 1,
                        barcol = optsgph$colors$col,
                        location = location)
    }

    return(0)
}

##############################################################################

CDTdataset.Display.Map <- function(){
    imgContainer <- CDT.Display.Map.inter(CDTdataset.Plot.Map, .cdtData$EnvData$tab$MapSelect, 'CDT Dataset - Mean')
    .cdtData$EnvData$tab$MapSelect <- imageNotebookTab_unik(imgContainer, .cdtData$EnvData$tab$MapSelect)

    ###############
    tkbind(imgContainer[[2]], "<Button-1>", function(W, x, y){
        xyid <- getIDLatLonCoords(W, x, y, imgContainer[[3]], getPixelLatlon)

        if(xyid$plotTS){
            tclvalue(.cdtData$EnvData$plot.maps$lonLOC) <- xyid$crd$x
            tclvalue(.cdtData$EnvData$plot.maps$latLOC) <- xyid$crd$y

            if(!is.null(.cdtData$EnvData$cdtdataset)){
                imgContainer1 <- CDT.Display.Graph(CDTdataset.Plot.Graph, .cdtData$EnvData$tab$dataGraph, "CDT Dataset - TS")
                .cdtData$EnvData$tab$dataGraph <- imageNotebookTab_unik(imgContainer1, .cdtData$EnvData$tab$dataGraph)
            }
        }
    })
}
