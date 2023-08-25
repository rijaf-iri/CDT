
CessationCalc.plotCessationMaps <- function(){
    don <- .cdtData$EnvData$varData$map
    dataMapOp <- .cdtData$EnvData$dataMapOp

    ## titre
    if(!dataMapOp$title$user){
        .titre <- paste("Ending dates of the rainy season:", trimws(tclvalue(.cdtData$EnvData$donDate)))
    }else .titre <- dataMapOp$title$title

    #################

    .data.type <- .cdtData$EnvData$plot.maps$.data.type
    .plot.type <- trimws(tclvalue(.cdtData$EnvData$plot.maps$plot.type))
    map.args <- cdt.plotmap.args(don, dataMapOp, .cdtData$EnvData$shp,
                                 label.fun = legendLabel.plotOnsetMaps,
                                 donDate = trimws(tclvalue(.cdtData$EnvData$donDate)),
                                 start.date = .cdtData$EnvData$output$start.date
                            )

    opar <- graphics::par(mar = map.args$mar)
    map.args.add <- list(titre = .titre,
                         SHPOp = .cdtData$EnvData$SHPOp,
                         MapOp = dataMapOp,
                         data.type = .data.type,
                         plot.type = .plot.type)
    map.args <- map.args[!(names(map.args) %in% "mar")]
    map.args <- c(map.args, map.args.add)
    par.plot <- do.call(cdt.plotmap.fun, map.args)

    ## scale bar
    cdt.plotmap.scalebar(dataMapOp$scalebar)

    graphics::par(opar)

    return(par.plot)
}

#######################################

CessationCalc.plotCessationGraph <- function(){
    TSGraphOp <- .cdtData$EnvData$TSGraphOp

    if(.cdtData$EnvData$output$params$data.type == "cdtstation"){
        ixy <- which(.cdtData$EnvData$output$data$id == trimws(tclvalue(.cdtData$EnvData$plot.maps$stnIDTSp)))
        if(length(ixy) == 0){
            Insert.Messages.Out(.cdtData$EnvData[['message']][['13']], TRUE, 'e')
            return(NULL)
        }
        don <- .cdtData$EnvData$varData$data[, ixy]
        .cdtData$EnvData$location <- paste0("Station: ", .cdtData$EnvData$output$data$id[ixy])
        titre <- paste0("(", .cdtData$EnvData$output$data$id[ixy], ")")
    }else{
        cdtdataset <- .cdtData$EnvData$cdtdataset
        xloc <- as.numeric(trimws(tclvalue(.cdtData$EnvData$plot.maps$lonLOC)))
        yloc <- as.numeric(trimws(tclvalue(.cdtData$EnvData$plot.maps$latLOC)))
        xyloc <- cdtdataset.extarct.TS(cdtdataset, cdtdataset$fileInfo, xloc, yloc)
        if(is.null(xyloc)) return(NULL)
        don <- as.Date(xyloc$data, origin = "1970-1-1")
        .cdtData$EnvData$location <- paste0("Longitude: ", round(xloc, 5), ", Latitude: ", round(yloc, 5))
        titre <- ""
    }

    don <- as.numeric(don - .cdtData$EnvData$output$start.date)
    daty <- as.numeric(format(.cdtData$EnvData$output$start.date, "%Y"))
    origindate <- as.character(.cdtData$EnvData$output$start.date[1])

    titre <- paste("Ending dates of the rainy season", titre)

    #########

    GRAPHTYPE <- trimws(tclvalue(.cdtData$EnvData$plot.maps$typeTSp))
    if(GRAPHTYPE == "Line") optsgph <- TSGraphOp$line
    if(GRAPHTYPE == "Barplot") optsgph <- TSGraphOp$bar

    xlim <- range(daty, na.rm = TRUE)
    if(optsgph$xlim$is.min) xlim[1] <- as.numeric(optsgph$xlim$min)
    if(optsgph$xlim$is.max) xlim[2] <- as.numeric(optsgph$xlim$max)
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

    #########

    if(GRAPHTYPE == "Line"){
        ret <- graphs.plot.line(daty, don, xlim = xlim, ylim = ylim, origindate = origindate,
                        xlab = xlab, ylab = ylab, ylab.sub = NULL,
                        title = titre, title.position = titre.pos, axis.font = 1,
                        plotl = optsgph$plot, legends = NULL,
                        location = .cdtData$EnvData$location)
    }

    if(GRAPHTYPE == "Barplot"){
        ret <- graphs.plot.bar(daty, don, xlim = xlim, ylim = ylim, origindate = origindate,
                        xlab = xlab, ylab = ylab, ylab.sub = NULL,
                        title = titre, title.position = titre.pos, axis.font = 1,
                        barcol = optsgph$colors$col,
                        location = .cdtData$EnvData$location)
    }

    return(ret)
}

##############################################################################

CessationCalc.Display.Maps <- function(){
    if(is.null(.cdtData$EnvData)) return(NULL)
    if(is.null(.cdtData$EnvData$output)) return(NULL)

    imgContainer <- CDT.Display.Map.inter(CessationCalc.plotCessationMaps, .cdtData$EnvData$tab$dataMap, 'Cessation-Map')
    .cdtData$EnvData$tab$dataMap <- imageNotebookTab_unik(imgContainer, .cdtData$EnvData$tab$dataMap)

    ###############
    tkbind(imgContainer[[2]], "<Button-1>", function(W, x, y){
        if(is.null(.cdtData$EnvData$plot.maps$data.type)) return(NULL)
        if(.cdtData$EnvData$plot.maps$data.type == "cdtstation"){
            xyid <- getIDLatLonCoords(W, x, y, imgContainer[[3]], getStnIDLabel,
                            stn.coords = .cdtData$EnvData$plot.maps[c('lon', 'lat', 'id')])
            if(xyid$plotTS)
                tclvalue(.cdtData$EnvData$plot.maps$stnIDTSp) <- xyid$crd
        }else{
            xyid <- getIDLatLonCoords(W, x, y, imgContainer[[3]], getPixelLatlon)
            if(xyid$plotTS){
                tclvalue(.cdtData$EnvData$plot.maps$lonLOC) <- xyid$crd$x
                tclvalue(.cdtData$EnvData$plot.maps$latLOC) <- xyid$crd$y
            }
        }

        if(xyid$plotTS){
            imgContainer1 <- CDT.Display.Graph(CessationCalc.plotCessationGraph, .cdtData$EnvData$tab$dataGraph, 'Cessation-Graph')
            .cdtData$EnvData$tab$dataGraph <- imageNotebookTab_unik(imgContainer1, .cdtData$EnvData$tab$dataGraph)
        }
    })
}
