
climatologiesCalc.plotClimMaps <- function(){
    don <- .cdtData$EnvData$climdata$map
    climMapOp <- .cdtData$EnvData$climMapOp

    ## titre
    if(!climMapOp$title$user){
        titre2 <- if(.cdtData$EnvData$climdata$Var == "CDTMEAN") "mean" else "standard deviation"
        year1 <- .cdtData$EnvData$output$params$climato$start.year
        year2 <- .cdtData$EnvData$output$params$climato$end.year
        titre3 <- paste0("(", year1, "-", year2, ")")

        if(.cdtData$EnvData$output$params$outstep %in% c('annual', 'seasonal')){
            if(.cdtData$EnvData$output$params$outstep == 'annual'){
                titre1 <- "Annuall"
            }
            if(.cdtData$EnvData$output$params$outstep == 'seasonal'){
                titre1 <- "Seasonal"
            }
                titre4 <- ""
                seqtime <- ""
        }else{
            if(.cdtData$EnvData$output$params$outstep == "daily"){
                titre1 <- "Daily"
                titre4 <- "for"
                seqtime <- format(seq(as.Date('2015-1-1'), as.Date('2015-12-31'), 'day'), "%B %d")
            }
            if(.cdtData$EnvData$output$params$outstep == "pentad"){
                titre1 <- "Pentadal"
                titre4 <- "for the"
                seqtime <- seq(as.Date('2015-1-1'), as.Date('2015-12-31'), 'day')
                pen <- findInterval(as.numeric(format(seqtime, "%d")), c(1, 5, 10, 15, 20, 25, 31), rightmost.closed = TRUE, left.open = TRUE)
                seqtime <- as.Date(names(split(seq_along(seqtime), paste0(format(seqtime, "%Y-%m-"), pen))))
                ordNum <- c('1st', '2nd', '3rd', '4th', '5th', '6th')
                seqtime <- paste(ordNum[as.numeric(format(seqtime, "%d"))], "pentad of", format(seqtime, "%B"))
            }
            if(.cdtData$EnvData$output$params$outstep == "dekadal"){
                titre1 <- "Dekadal"
                titre4 <- "for the"
                seqtime <- seq(as.Date('2015-1-1'), as.Date('2015-12-31'), 'day')
                dek <- findInterval(as.numeric(format(seqtime, "%d")), c(1, 10, 20, 31), rightmost.closed = TRUE, left.open = TRUE)
                seqtime <- as.Date(names(split(seq_along(seqtime), paste0(format(seqtime, "%Y-%m-"), dek))))
                ordNum <- c('1st', '2nd', '3rd')
                seqtime <- paste(ordNum[as.numeric(format(seqtime, "%d"))], "dekad of", format(seqtime, "%B"))
            }
            if(.cdtData$EnvData$output$params$outstep == "monthly"){
                titre1 <- "Monthly"
                titre4 <- "for"
                seqtime <- format(seq(as.Date('2015-1-1'), as.Date('2015-12-31'), 'month'), "%B")
            }
        }

        ipos <- as.numeric(trimws(tclvalue(.cdtData$EnvData$climDate)))
        titre5 <- seqtime[ipos]
        .titre <- paste(titre1, titre2, titre3, titre4, titre5)
    }else .titre <- climMapOp$title$title

    #################

    .data.type <- .cdtData$EnvData$plot.maps$.data.type
    map.args <- cdt.plotmap.args(don, climMapOp, .cdtData$EnvData$shapefile)

    opar <- graphics::par(mar = map.args$mar)
    map.args.add <- list(titre = .titre, data.type = .data.type)

    map.args <- map.args[!(names(map.args) %in% "mar")]
    map.args <- c(map.args, map.args.add)
    par.plot <- do.call(cdt.plotmap.fun, map.args)

    ## scale bar
    cdt.plotmap.scalebar(climMapOp$scalebar)

    graphics::par(opar)

    return(par.plot)
}

#######################################

climatologiesCalc.plotClimGraph <- function(){
    TSGraphOp <- .cdtData$EnvData$TSGraphOp

    if(.cdtData$EnvData$output$params$data.type == "cdtstation"){
        ixy <- which(.cdtData$EnvData$output$data$id == trimws(tclvalue(.cdtData$EnvData$plot.maps$stnIDTSp)))
        if(length(ixy) == 0){
            Insert.Messages.Out(.cdtData$EnvData[['message']][['12']], TRUE, 'e')
            return(NULL)
        }
        don <- .cdtData$EnvData$climdata$data[, ixy]
        idaty <- .cdtData$EnvData$output$index
        .cdtData$EnvData$location <- paste0("Station: ", .cdtData$EnvData$output$data$id[ixy])
    }else{
        cdtdataset <- .cdtData$EnvData$cdtdataset
        xloc <- as.numeric(trimws(tclvalue(.cdtData$EnvData$plot.maps$lonLOC)))
        yloc <- as.numeric(trimws(tclvalue(.cdtData$EnvData$plot.maps$latLOC)))
        xyloc <- cdtdataset.extarct.TS(cdtdataset, cdtdataset$fileInfo, xloc, yloc)
        if(is.null(xyloc)) return(NULL)
        don <- as.numeric(xyloc$data)
        idaty <- xyloc$date

        .cdtData$EnvData$location <- paste0("Longitude: ", round(xloc, 5), ", Latitude: ", round(yloc, 5))
    }

    #########

    if(.cdtData$EnvData$output$params$outstep == "daily"){
        titre1 <- "Daily"
        seqtime <- seq(as.Date('2015-1-1'), as.Date('2015-12-31'), 'day')
    }
    if(.cdtData$EnvData$output$params$outstep == "pentad"){
        titre1 <- "Pentad"
        seqtime <- seq(as.Date('2015-1-1'), as.Date('2015-12-31'), 'day')
        pen <- findInterval(as.numeric(format(seqtime, "%d")), c(1, 5, 10, 15, 20, 26, 31), rightmost.closed = TRUE, left.open = TRUE)
        seqtime <- as.Date(names(split(seq_along(seqtime), paste0(format(seqtime, "%Y-%m-"), pen))))
        seqtime <- as.Date(paste0(format(seqtime, "%Y-%m-"), c(1, 6, 11, 16, 21, 26)[as.numeric(format(seqtime, "%d"))]))
    }
    if(.cdtData$EnvData$output$params$outstep == "dekadal"){
        titre1 <- "Dekadal"
        seqtime <- seq(as.Date('2015-1-1'), as.Date('2015-12-31'), 'day')
        dek <- findInterval(as.numeric(format(seqtime, "%d")), c(1, 10, 20, 31), rightmost.closed = TRUE, left.open = TRUE)
        seqtime <- as.Date(names(split(seq_along(seqtime), paste0(format(seqtime, "%Y-%m-"), dek))))
        seqtime <- as.Date(paste0(format(seqtime, "%Y-%m-"), c(1, 11, 21)[as.numeric(format(seqtime, "%d"))]))
    }
    if(.cdtData$EnvData$output$params$outstep == "monthly"){
        titre1 <- "Monthly"
        seqtime <- seq(as.Date('2015-1-1'), as.Date('2015-12-31'), 'month')
    }
    daty <- seqtime[idaty]

    titre2 <- if(.cdtData$EnvData$climdata$Var == "CDTMEAN") "mean" else "standard deviation"
    titre <- paste(titre1, titre2)

    #########

    GRAPHTYPE <- trimws(tclvalue(.cdtData$EnvData$plot.maps$typeTSp))
    if(GRAPHTYPE == "Line") optsgph <- TSGraphOp$line
    if(GRAPHTYPE == "Barplot") optsgph <- TSGraphOp$bar

    xlim <- range(daty, na.rm = TRUE)
    if(optsgph$xlim$is.min){
        xx <- strsplit(optsgph$xlim$min, "-")[[1]]
        if(length(xx) == 1) xx[2] <- 1
        xx <- try(as.Date(paste(2015, xx[1], xx[2], sep = '-')), silent = TRUE)

        if(inherits(xx, "try-error") | is.na(xx)){
            Insert.Messages.Out(.cdtData$EnvData[['message']][['17']], TRUE, 'e')
            return(NULL)
        }
        xlim[1] <- xx
    }
    if(optsgph$xlim$is.max){
        xx <- strsplit(optsgph$xlim$max, "-")[[1]]
        if(length(xx) == 1) xx[2] <- 1
        xx <- try(as.Date(paste(2015, xx[1], xx[2], sep = '-')), silent = TRUE)

        if(inherits(xx, "try-error") | is.na(xx)){
            Insert.Messages.Out(.cdtData$EnvData[['message']][['17']], TRUE, 'e')
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

    if(GRAPHTYPE == "Barplot"){
        ret <- graphs.plot.bar(daty, don, xlim = xlim, ylim = ylim,
                               xlab = xlab, ylab = ylab, ylab.sub = NULL,
                               title = titre, title.position = titre.pos, axis.font = 1,
                               barcol = optsgph$colors$col,
                               location = .cdtData$EnvData$location)
    }

    return(ret)
}

##############################################################################

climatologiesCalc.Display.Maps <- function(){
    if(is.null(.cdtData$EnvData)) return(NULL)
    if(is.null(.cdtData$EnvData$output)) return(NULL)

    imgContainer <- CDT.Display.Map.inter(climatologiesCalc.plotClimMaps, .cdtData$EnvData$tab$ClimMap, 'Climatology-Map')
    .cdtData$EnvData$tab$ClimMap <- imageNotebookTab_unik(imgContainer, .cdtData$EnvData$tab$ClimMap)

    ###############
    if(!.cdtData$EnvData$output$params$outstep %in% c('annual', 'seasonal')){
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
                imgContainer1 <- CDT.Display.Graph(climatologiesCalc.plotClimGraph, .cdtData$EnvData$tab$ClimGraph, 'Climatology-Graph')
                .cdtData$EnvData$tab$ClimGraph <- imageNotebookTab_unik(imgContainer1, .cdtData$EnvData$tab$ClimGraph)
            }
        })
    }
}

