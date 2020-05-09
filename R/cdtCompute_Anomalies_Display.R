
anomaliesCalc.plotAnomMaps <- function(){
    don <- .cdtData$EnvData$anomdata$map
    anomMapOp <- .cdtData$EnvData$anomMapOp

    ## titre
    if(!anomMapOp$title$user){
        titre1 <- switch(.cdtData$EnvData$output$params$anomaly,
                         "Difference" = "Anomaly:",
                         "Percentage" = "Anomaly (% of Mean):",
                         "Standardized" = "Standardized Anomaly:")
        .titre <- paste(titre1, str_trim(tclvalue(.cdtData$EnvData$anomDate)))
    }else .titre <- anomMapOp$title$title

    #################

    .data.type <- .cdtData$EnvData$plot.maps$.data.type
    .plot.type <- str_trim(tclvalue(.cdtData$EnvData$plot.maps$plot.type))
    map.args <- cdt.plotmap.args(don, anomMapOp, .cdtData$EnvData$shp)

    opar <- par(mar = map.args$mar)
    map.args.add <- list(titre = .titre,
                         SHPOp = .cdtData$EnvData$SHPOp,
                         MapOp = anomMapOp,
                         data.type = .data.type,
                         plot.type = .plot.type)
    map.args <- map.args[!(names(map.args) %in% "mar")]
    map.args <- c(map.args, map.args.add)
    par.plot <- do.call(cdt.plotmap.fun, map.args)

    ## scale bar
    cdt.plotmap.scalebar(anomMapOp$scalebar)

    par(opar)

    return(par.plot)
}

#######################################

anomaliesCalc.plotAnomGraph <- function(){
    TSGraphOp <- .cdtData$EnvData$TSGraphOp

    if(.cdtData$EnvData$output$params$data.type == "cdtstation"){
        ixy <- which(.cdtData$EnvData$output$data$id == str_trim(tclvalue(.cdtData$EnvData$plot.maps$stnIDTSp)))
        if(length(ixy) == 0){
            Insert.Messages.Out(.cdtData$EnvData[['message']][['27']], TRUE, "e")
            return(NULL)
        }
        don <- .cdtData$EnvData$anomdata$data[, ixy]
        daty <- .cdtData$EnvData$output$data$dates
        .cdtData$EnvData$location <- paste0("Station: ", .cdtData$EnvData$output$data$id[ixy])
        titre3 <- paste0("(", .cdtData$EnvData$output$data$id[ixy], ")")
    }else{
        cdtdataset <- .cdtData$EnvData$cdtdataset
        xloc <- as.numeric(str_trim(tclvalue(.cdtData$EnvData$plot.maps$lonLOC)))
        yloc <- as.numeric(str_trim(tclvalue(.cdtData$EnvData$plot.maps$latLOC)))
        xyloc <- cdtdataset.extarct.TS(cdtdataset, cdtdataset$fileInfo, xloc, yloc)
        if(is.null(xyloc)) return(NULL)
        don <- as.numeric(xyloc$data)
        daty <- xyloc$date

        .cdtData$EnvData$location <- paste0("Longitude: ", round(xloc, 5), ", Latitude: ", round(yloc, 5))
        titre3 <- ""
    }

    #########

    if(.cdtData$EnvData$output$params$outstep == "daily"){
        titre1 <- "Daily"
        daty <- as.Date(daty, "%Y%m%d")
    }
    if(.cdtData$EnvData$output$params$outstep == "pentad"){
        titre1 <- "Pentad"
        seqtime <- as.Date(daty, "%Y%m%d")
        daty <- as.Date(paste0(format(seqtime, "%Y-%m-"), c(1, 6, 11, 16, 21, 26)[as.numeric(format(seqtime, "%d"))]))
    }
    if(.cdtData$EnvData$output$params$outstep == "dekadal"){
        titre1 <- "Dekadal"
        seqtime <- as.Date(daty, "%Y%m%d")
        daty <- as.Date(paste0(format(seqtime, "%Y-%m-"), c(1, 11, 21)[as.numeric(format(seqtime, "%d"))]))
    }
    if(.cdtData$EnvData$output$params$outstep == "monthly"){
        titre1 <- "Monthly"
        daty <- as.Date(paste0(daty, "01"), "%Y%m%d")
    }
    if(.cdtData$EnvData$output$params$outstep == "seasonal"){
        mois <- format(ISOdate(2014, 1:12, 1), "%b")
        mon <- .cdtData$EnvData$output$params$seasonal$start.mon
        len <- .cdtData$EnvData$output$params$seasonal$length.mon
        mon1 <- (mon + len - 1) %% 12
        mon1[mon1 == 0] <- 12
        seasdef <- paste0(mois[mon], "->", mois[mon1])
        titre1 <- paste(paste0("[", seasdef, "]"), "Seasonal")
        daty <- do.call(c, lapply(strsplit(daty, "_"), '[[', 1))
        daty <- as.Date(paste0(daty, '-01'))
    }
    if(.cdtData$EnvData$output$params$outstep == "annual"){
        titre1 <- "Annual"
        daty <- as.Date(paste0(daty, "0101"), "%Y%m%d")
    }

    titre2 <- switch(.cdtData$EnvData$output$params$anomaly,
                     "Difference" = "Anomaly",
                     "Percentage" = "Anomaly (% of Mean)",
                     "Standardized" = "Standardized Anomaly")

    titre <- paste(titre1, titre2, titre3)

    #########

    GRAPHTYPE <- str_trim(tclvalue(.cdtData$EnvData$plot.maps$typeTSp))
    if(GRAPHTYPE == "Line") optsgph <- TSGraphOp$line
    if(GRAPHTYPE == "Bar") optsgph <- TSGraphOp$anomaly

    xlim <- range(daty, na.rm = TRUE)
    if(optsgph$xlim$is.min){
        xx <- strsplit(optsgph$xlim$min, "-")[[1]]
        x3 <- as.numeric(xx[3])
        if(.cdtData$EnvData$output$params$intstep == "pentad"){
            if(is.na(x3) | x3 < 1 | x3 > 6){
                Insert.Messages.Out(.cdtData$EnvData[['message']][['28']], TRUE, 'e')
                return(NULL)
            }
            x3 <- c(1, 6, 11, 16, 21, 26)[x3]
        }
        if(.cdtData$EnvData$output$params$intstep == "dekadal"){
            if(is.na(x3) | x3 < 1 | x3 > 3){
                Insert.Messages.Out(.cdtData$EnvData[['message']][['29']], TRUE, 'e')
                return(NULL)
            }
            x3 <- c(1, 11, 21)[x3]
        }
        if(.cdtData$EnvData$output$params$intstep == "monthly") x3 <- 1
        x1 <- as.numeric(xx[1])
        x2 <- str_pad(as.numeric(xx[2]), 2, pad = "0")
        x3 <- str_pad(x3, 2, pad = "0")
        xx <- as.Date(paste0(x1, x2, x3), "%Y%m%d")
        if(is.na(xx)){
            Insert.Messages.Out(.cdtData$EnvData[['message']][['30']], TRUE, 'e')
            return(NULL)
        }
        xlim[1] <- xx
    }
    if(optsgph$xlim$is.max){
        xx <- strsplit(optsgph$xlim$max, "-")[[1]]
        x3 <- as.numeric(xx[3])
        if(.cdtData$EnvData$output$params$intstep == "pentad"){
            if(is.na(x3) | x3 < 1 | x3 > 6){
                Insert.Messages.Out(.cdtData$EnvData[['message']][['28']], TRUE, 'e')
                return(NULL)
            }
            x3 <- c(1, 6, 11, 16, 21, 26)[x3]
        }
        if(.cdtData$EnvData$output$params$intstep == "dekadal"){
            if(is.na(x3) | x3 < 1 | x3 > 3){
                Insert.Messages.Out(.cdtData$EnvData[['message']][['29']], TRUE, 'e')
                return(NULL)
            }
            x3 <- c(1, 11, 21)[x3]
        }
        if(.cdtData$EnvData$output$params$intstep == "monthly") x3 <- 1
        x1 <- as.numeric(xx[1])
        x2 <- str_pad(as.numeric(xx[2]), 2, pad = "0")
        x3 <- str_pad(x3, 2, pad = "0")
        xx <- as.Date(paste0(x1, x2, x3), "%Y%m%d")
        if(is.na(xx)){
            Insert.Messages.Out(.cdtData$EnvData[['message']][['30']], TRUE, 'e')
            return(NULL)
        }
        xlim[2] <- xx
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

    #########

    if(GRAPHTYPE == "Line"){
        ret <- graphs.plot.line(daty, don, xlim = xlim, ylim = ylim,
                                xlab = xlab, ylab = ylab, ylab.sub = NULL,
                                title = titre, title.position = titre.pos, axis.font = 1,
                                plotl = optsgph$plot, legends = NULL,
                                location = .cdtData$EnvData$location)
    }

    if(GRAPHTYPE == "Bar"){
        loko <- c(optsgph$colors$negative, optsgph$colors$positive)

        ret <- graphs.plot.bar.Anomaly(daty, don, period = NULL, percent = FALSE,
                                       xlim = xlim, ylim = ylim, xlab = xlab, ylab = ylab, ylab.sub = NULL,
                                       title = titre, title.position = titre.pos, axis.font = 1,
                                       barcol = loko, location = .cdtData$EnvData$location)
    }
    return(ret)
}

##############################################################################

anomaliesCalc.Display.Maps <- function(){
    if(is.null(.cdtData$EnvData)) return(NULL)
    if(is.null(.cdtData$EnvData$output)) return(NULL)

    imgContainer <- CDT.Display.Map.inter(anomaliesCalc.plotAnomMaps, .cdtData$EnvData$tab$AnomMap, 'Anomaly-Map')
    .cdtData$EnvData$tab$AnomMap <- imageNotebookTab_unik(imgContainer, .cdtData$EnvData$tab$AnomMap)

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
            imgContainer1 <- CDT.Display.Graph(anomaliesCalc.plotAnomGraph, .cdtData$EnvData$tab$AnomGraph, 'Anomaly-Graph')
            .cdtData$EnvData$tab$AnomGraph <- imageNotebookTab_unik(imgContainer1, .cdtData$EnvData$tab$AnomGraph)
        }
    })
}
