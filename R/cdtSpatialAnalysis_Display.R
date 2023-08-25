
spatialAnalysis.plotStatMaps <- function(){
    don <- .cdtData$EnvData$don
    climMapOp <- .cdtData$EnvData$climMapOp

    ## titre
    if(!climMapOp$title$user){
        params <- .cdtData$EnvData$statpars$params
        titre1 <- stringr::str_to_title(params$out.series$tstep)
        titre2 <- tclvalue(.cdtData$EnvData$climStat)
        titre3 <- switch(params$analysis$method,
                         "percentile" = paste0("(", params$analysis$percentile, "th", ")"),
                         "frequency" = paste0("(", params$analysis$frequency$low, " < X < ",
                                                   params$analysis$frequency$up, ")"),
                          "trend" = {
                                if(params$analysis$trend$unit == 1) "per year"
                                if(params$analysis$trend$unit == 2) "over"
                                if(params$analysis$trend$unit == 3) "/ average (in %)"
                          },
                        NULL)
        titre4 <- tclvalue(.cdtData$EnvData$climDate)
        .titre <- paste(titre1, titre2, titre3, titre4)
    }else .titre <- climMapOp$title$title

    #################

    .data.type <- .cdtData$EnvData$plot.maps$.data.type
    .plot.type <- trimws(tclvalue(.cdtData$EnvData$plot.maps$plot.type))
    map.args <- cdt.plotmap.args(don, climMapOp, .cdtData$EnvData$shp)

    opar <- graphics::par(mar = map.args$mar)
    map.args.add <- list(titre = .titre,
                         SHPOp = .cdtData$EnvData$SHPOp,
                         MapOp = climMapOp,
                         data.type = .data.type,
                         plot.type = .plot.type)
    map.args <- map.args[!(names(map.args) %in% "mar")]
    map.args <- c(map.args, map.args.add)
    par.plot <- do.call(cdt.plotmap.fun, map.args)

    ### Trend
    if(trimws(tclvalue(.cdtData$EnvData$climStat)) == "Trend"){
        if(.cdtData$EnvData$statpars$params$data.type == "cdtstation"){
            ipvl <- !is.na(don$p.value) & don$p.value < 0.05
            if(any(ipvl)){
                # points(don$x0[ipvl], don$y0[ipvl], col = adjustcolor('gray40', alpha.f = 0.8))
                graphics::points(don$x0[ipvl], don$y0[ipvl], pch = 'x', cex = 0.8)
            }
        }else{
            ipvl <- c(don$pval)
            ipvl <- !is.na(ipvl) & ipvl < 0.05
            if(any(ipvl)){
                grd <- don$x[2] - don$x[1]
                dd <- expand.grid(x = don$x, y = don$y)
                sp::coordinates(dd) <- ~x+y
                dd <- dd[ipvl, ]
                buffer <- rgeos::gBuffer(dd, width = grd * 1.02)

                dd <- sp::disaggregate(buffer)
                centr <- sp::coordinates(dd)
                bbx <- lapply(seq_along(dd), function(i) sp::bbox(dd[i]))
                esp <- if(grd > 0.25) 0.25 else grd * 5
                esp <- if(esp > 0.25) 0.25 else esp
                dd <- lapply(seq_along(bbx), function(i){   
                    xpt <- c(rev(seq(centr[i, 1], bbx[[i]][1, 1], -esp)[-1]), seq(centr[i, 1], bbx[[i]][1, 2], esp))
                    ypt <- c(rev(seq(centr[i, 2], bbx[[i]][2, 1], -esp)[-1]), seq(centr[i, 2], bbx[[i]][2, 2], esp))
                    xy <- expand.grid(x = xpt, y = ypt)
                    sp::coordinates(xy) <- ~x+y
                    ij <- as.logical(sp::over(xy, dd[i]))
                    ij[is.na(ij)] <- FALSE
                    sp::coordinates(xy[ij, ])
                })
                dd <- do.call(rbind, dd)
                # points(dd[, 1], dd[, 2], pch = 15, cex = 0.3, col = adjustcolor('gray20', alpha.f = 0.9))
                graphics::points(dd[, 1], dd[, 2], pch = 15, cex = 0.3)
            }
        }
    }

    ## scale bar
    cdt.plotmap.scalebar(climMapOp$scalebar)

    graphics::par(opar)

    return(par.plot)
}

#######################################

spatialAnalysis.plotTSMaps <- function(){
    TSMapOp <- .cdtData$EnvData$TSMapOp

    if(tclvalue(.cdtData$EnvData$TSData) == "Data")
        don <- .cdtData$EnvData$tsdata
    if(tclvalue(.cdtData$EnvData$TSData) == "Anomaly")
        don <- .cdtData$EnvData$anomData

    if(!TSMapOp$title$user){
        if(trimws(tclvalue(.cdtData$EnvData$TSData)) == "Data"){
            params <- .cdtData$EnvData$statpars$params
            titre1 <- stringr::str_to_title(params$out.series$tstep)
            # c("sum", "mean", "median", "max", "min", "count")
            titre2 <- switch(params$aggr.series$aggr.fun,
                             "sum" = "total", "mean" = "average",
                             "median" = "median", "max" = "maximum",
                             "min" = "minimum", "count" = "number")
            titre3 <- if(params$aggr.series$aggr.fun == "count")
                            paste("(", params$aggr.series$opr.fun, params$aggr.series$opr.thres, ")") else NULL
            titre4 <- tclvalue(.cdtData$EnvData$TSDate)
            .titre <- paste(titre1, titre2, titre3, titre4)
        }

        if(trimws(tclvalue(.cdtData$EnvData$TSData)) == "Anomaly"){
            params <- don$params
            titre1 <- stringr::str_to_title(params$out.series$tstep)
            titre2 <- "anomaly"
            titre3 <- if(params$analysis$anomaly$perc) "% of mean" else NULL
            titre4 <- tclvalue(.cdtData$EnvData$TSDate)
            .titre <- paste(titre1, titre2, titre3, titre4)
        }
    }else .titre <- TSMapOp$title$title

    #################

    .data.type <- .cdtData$EnvData$plot.maps$.data.type
    .plot.type <- trimws(tclvalue(.cdtData$EnvData$plot.maps$plot.type))
    map.args <- cdt.plotmap.args(don, TSMapOp, .cdtData$EnvData$shp)

    opar <- graphics::par(mar = map.args$mar)
    map.args.add <- list(titre = .titre,
                         SHPOp = .cdtData$EnvData$SHPOp,
                         MapOp = TSMapOp,
                         data.type = .data.type,
                         plot.type = .plot.type)
    map.args <- map.args[!(names(map.args) %in% "mar")]
    map.args <- c(map.args, map.args.add)
    par.plot <- do.call(cdt.plotmap.fun, map.args)

    ## scale bar
    cdt.plotmap.scalebar(TSMapOp$scalebar)

    graphics::par(opar)

    return(par.plot)
}

#######################################

spatialAnalysis.plotTSGraph <- function(){
    TSGraphOp <- .cdtData$EnvData$TSGraphOp

    if(.cdtData$EnvData$statpars$params$data.type == "cdtstation"){
        ixy <- which(.cdtData$EnvData$tsdata$id == trimws(tclvalue(.cdtData$EnvData$plot.maps$stnIDTSp)))
        if(length(ixy) == 0){
            Insert.Messages.Out(.cdtData$EnvData$message[['18']], TRUE, 'e')
            return(NULL)
        }
        don <- .cdtData$EnvData$tsdata$data[, ixy]
        dates <- .cdtData$EnvData$tsdata$date
        daty <- as.numeric(substr(dates, 1, 4))
        .cdtData$EnvData$location <- paste0("Station: ", .cdtData$EnvData$tsdata$id[ixy])
    }else{
        cdtdataset <- .cdtData$EnvData$cdtdataset
        xloc <- as.numeric(trimws(tclvalue(.cdtData$EnvData$plot.maps$lonLOC)))
        yloc <- as.numeric(trimws(tclvalue(.cdtData$EnvData$plot.maps$latLOC)))
        xyloc <- cdtdataset.extarct.TS(cdtdataset, cdtdataset$fileInfo, xloc, yloc)
        if(is.null(xyloc)) return(NULL)
        don <- as.numeric(xyloc$data)
        dates <- xyloc$date

        ######
        year1 <- substr(dates, 1, 4) 
        mon1 <- substr(dates, 6, 7)
        year2 <- substr(dates, 9, 12)
        mon2 <- substr(dates, 14, 15)
        if(all(year1 == year2)){
            if(all(mon1 == mon2)) dateTS <- paste0(year1, mon1)
            else{
                dateTS <- if(all(mon1 == "01") && all(mon2 == "12")) year1 else dates
            }
        }else dateTS <- dates
        ipos <- which(.cdtData$EnvData$statpars$stats == trimws(tclvalue(.cdtData$EnvData$climDate)))
        idaty <- dateTS %in% .cdtData$EnvData$statpars$timeseries[[ipos]][[2]]
        dates <- dateTS[idaty]
        don <- don[idaty]

        daty <- as.numeric(substr(dates, 1, 4))
        .cdtData$EnvData$location <- paste0("Longitude: ", round(xloc, 5), ", Latitude: ", round(yloc, 5))
    }

    #########
    GRAPHTYPE <- .cdtData$EnvData$plot.maps$typeTSp

    #### ENSO
    if(GRAPHTYPE %in% c('eline', 'ebar', 'eproba')){
        if(nchar(dates[1]) == 4){
            start.mon <- paste0(dates, "0115")
            end.mon <- paste0(dates, "1215")
        }
        if(nchar(dates[1]) == 6){
            start.mon <- paste0(dates, "15")
            end.mon <- paste0(dates, "15")
        }
        if(nchar(dates[1]) == 15){
            dates <- lapply(strsplit(dates, '_'), function(x) format(as.Date(paste0(x, "-15")), "%Y%m%d"))
            start.mon <- sapply(dates, '[[', 1)
            end.mon <- sapply(dates, '[[', 2)
        }

        ijoni <- cdt.index.flexseason(start.mon, end.mon, .cdtData$EnvData$ONI$date, "monthly")
        oni <- sapply(ijoni$index, function(x) mean(.cdtData$EnvData$ONI$data[x], na.rm = TRUE))
        oni[length(ijoni$nba) == 0] <- NA
        oni[is.nan(oni)] <- NA
        oni <- ifelse(oni >= 0.5, 3, ifelse(oni <= -0.5, 1, 2))
    }

    ########

    xlab0 <- ""
    ylab0 <- ""

    #########

    optsgph <- switch(GRAPHTYPE,
                    'line' = TSGraphOp$line,
                    'bar' = TSGraphOp$bar,
                    'eline' = TSGraphOp$line.enso,
                    'ebar' = TSGraphOp$bar.enso,
                    'anom' = TSGraphOp$anomaly,
                    'proba' = TSGraphOp$proba,
                    'eproba' = TSGraphOp$proba.enso)

    ## xlim, ylim, xlab, ylab
    if(GRAPHTYPE %in% c('proba', 'eproba')){
        xlim <- range(don, na.rm = TRUE)
        if(optsgph$xlim$is.min) xlim[1] <- as.numeric(optsgph$xlim$min)
        if(optsgph$xlim$is.max) xlim[2] <- as.numeric(optsgph$xlim$max)
        ylim <- c(0, 100)
        ylab0 <- "Probability of Exceeding"
    }else{
        xlim <- range(daty, na.rm = TRUE)
        if(optsgph$xlim$is.min) xlim[1] <- as.numeric(optsgph$xlim$min)
        if(optsgph$xlim$is.max) xlim[2] <- as.numeric(optsgph$xlim$max)
        idt <- daty >= xlim[1] & daty <= xlim[2]
        daty <- daty[idt]
        don <- don[idt]
        ylim <- range(pretty(don))
        if(GRAPHTYPE == 'anom')
            if(optsgph$anom$perc.anom) ylab0 <- "Anomaly (% of Mean)"
    }

    if(optsgph$ylim$is.min) ylim[1] <- optsgph$ylim$min
    if(optsgph$ylim$is.max) ylim[2] <- optsgph$ylim$max

    xlab <- if(optsgph$axislabs$is.xlab) optsgph$axislabs$xlab else xlab0
    ylab <- if(optsgph$axislabs$is.ylab) optsgph$axislabs$ylab else ylab0

    if(optsgph$title$is.title){
        titre <- optsgph$title$title
        titre.pos <- optsgph$title$position
    }else{
        titre <- ""
        titre.pos <- "top"
    }

    #########

    if(GRAPHTYPE == 'line'){
        legends <- NULL
        if(optsgph$legend$is$mean){
            legends$add$mean <- optsgph$legend$add$mean
            legends$col$mean <- optsgph$legend$col$mean
            legends$text$mean <- optsgph$legend$text$mean
            legends$lwd$mean <- optsgph$legend$lwd$mean
        }else{
            if(tclvalue(.cdtData$EnvData$plot.maps$averageTSp) == "1") legends$add$mean <- TRUE
        }
        if(optsgph$legend$is$linear){
            legends$add$linear <- optsgph$legend$add$linear
            legends$col$linear <- optsgph$legend$col$linear
            legends$text$linear <- optsgph$legend$text$linear
            legends$lwd$linear <- optsgph$legend$lwd$linear
        }else{
            if(tclvalue(.cdtData$EnvData$plot.maps$trendTSp) == "1") legends$add$linear <- TRUE
        }
        if(optsgph$legend$is$tercile){
            legends$add$tercile <- optsgph$legend$add$tercile
            legends$col$tercile1 <- optsgph$legend$col$tercile1
            legends$text$tercile1 <- optsgph$legend$text$tercile1
            legends$col$tercile2 <- optsgph$legend$col$tercile2
            legends$text$tercile2 <- optsgph$legend$text$tercile2
            legends$lwd$tercile <- optsgph$legend$lwd$tercile
        }else{
            if(tclvalue(.cdtData$EnvData$plot.maps$tercileTSp) == "1") legends$add$tercile <- TRUE
        }

        ret <- graphs.plot.line(daty, don, xlim = xlim, ylim = ylim,
                        xlab = xlab, ylab = ylab, ylab.sub = NULL,
                        title = titre, title.position = titre.pos, axis.font = 1,
                        plotl = optsgph$plot, legends = legends,
                        location = .cdtData$EnvData$location)
    }

    if(GRAPHTYPE == 'bar'){
        ret <- graphs.plot.bar(daty, don, xlim = xlim, ylim = ylim,
                        xlab = xlab, ylab = ylab, ylab.sub = NULL,
                        title = titre, title.position = titre.pos, axis.font = 1,
                        barcol = optsgph$colors$col,
                        location = .cdtData$EnvData$location)
    }

    if(GRAPHTYPE == 'eline'){
        oni <- oni[idt]

        legends <- NULL
        if(optsgph$legend$is$mean){
            legends$add$mean <- optsgph$legend$add$mean
            legends$col$mean <- optsgph$legend$col$mean
            legends$text$mean <- optsgph$legend$text$mean
            legends$lwd$mean <- optsgph$legend$lwd$mean
        }else{
            if(tclvalue(.cdtData$EnvData$plot.maps$averageTSp) == "1") legends$add$mean <- TRUE
        }
        if(optsgph$legend$is$linear){
            legends$add$linear <- optsgph$legend$add$linear
            legends$col$linear <- optsgph$legend$col$linear
            legends$text$linear <- optsgph$legend$text$linear
            legends$lwd$linear <- optsgph$legend$lwd$linear
        }else{
            if(tclvalue(.cdtData$EnvData$plot.maps$trendTSp) == "1") legends$add$linear <- TRUE
        }
        if(optsgph$legend$is$tercile){
            legends$add$tercile <- optsgph$legend$add$tercile
            legends$col$tercile1 <- optsgph$legend$col$tercile1
            legends$text$tercile1 <- optsgph$legend$text$tercile1
            legends$col$tercile2 <- optsgph$legend$col$tercile2
            legends$text$tercile2 <- optsgph$legend$text$tercile2
            legends$lwd$tercile <- optsgph$legend$lwd$tercile
        }else{
            if(tclvalue(.cdtData$EnvData$plot.maps$tercileTSp) == "1") legends$add$tercile <- TRUE
        }

        ret <- graphs.plot.line.ENSO(daty, don, oni, xlim = xlim, ylim = ylim,
                            xlab = xlab, ylab = ylab, ylab.sub = NULL,
                            title = titre, title.position = titre.pos, axis.font = 1,
                            plotl = optsgph$plot, legends = legends,
                            location = .cdtData$EnvData$location)
    }

    if(GRAPHTYPE == 'ebar'){
        oni <- oni[idt]
        ret <- graphs.plot.bar.ENSO(daty, don, oni, xlim = xlim, ylim = ylim,
                            xlab = xlab, ylab = ylab, ylab.sub = NULL,
                            title = titre, title.position = titre.pos, axis.font = 1,
                            barcol = optsgph$colors$col, location = .cdtData$EnvData$location)
    }

    if(GRAPHTYPE == 'anom'){
        if(!optsgph$ylim$is.min & !optsgph$ylim$is.max) ylim <- NULL
        loko <- c(optsgph$colors$negative, optsgph$colors$positive)

        period <- range(daty, na.rm = TRUE)
        if(optsgph$anom$basePeriod){
            startYr <- optsgph$anom$startYr.anom
            endYr <- optsgph$anom$endYr.anom
            period <- c(startYr, endYr)
        }

        ret <- graphs.plot.bar.Anomaly(daty, don, period = period, percent = optsgph$anom$perc.anom,
                                xlim = xlim, ylim = ylim, xlab = xlab, ylab = ylab, ylab.sub = NULL,
                                title = titre, title.position = titre.pos, axis.font = 1,
                                barcol = loko, location = .cdtData$EnvData$location)
    }

    if(GRAPHTYPE == 'proba'){
        ret <- graphs.plot.proba(don, xlim = xlim, ylim = ylim,
                        xlab = xlab, xlab.sub = NULL, ylab = ylab,
                        title = titre, title.position = titre.pos, axis.font = 1,
                        proba = list(theoretical = optsgph$proba$theoretical),
                        plotp = optsgph$proba, plotl = optsgph$plot,
                        location = .cdtData$EnvData$location)
    }

    if(GRAPHTYPE == 'eproba'){
        ret <- graphs.plot.proba.ENSO(don, oni, xlim = xlim, ylim = ylim,
                            xlab = xlab, xlab.sub = NULL, ylab = ylab,
                            title = titre, title.position = titre.pos, axis.font = 1,
                            plotl = optsgph$plot, location = .cdtData$EnvData$location)
    }

    return(ret)
}

##############################################################################

spatialAnalysis.DisplayStatMaps <- function(){
    if(is.null(.cdtData$EnvData)) return(NULL)
    if(is.null(.cdtData$EnvData$statpars)) return(NULL)

    .cdtData$EnvData$plot.maps[c('lon', 'lat', 'id')] <- .cdtData$EnvData$don[c('x0', 'y0', 'id')]

    imgContainer <- CDT.Display.Map.inter(spatialAnalysis.plotStatMaps, .cdtData$EnvData$tab$climMap, 'Clim-Analysis-Maps')
    .cdtData$EnvData$tab$climMap <- imageNotebookTab_unik(imgContainer, .cdtData$EnvData$tab$climMap)

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
            imgContainer1 <- CDT.Display.Graph(spatialAnalysis.plotTSGraph, .cdtData$EnvData$tab$TSplot, 'Time-Series-Plot')
            .cdtData$EnvData$tab$TSplot <- imageNotebookTab_unik(imgContainer1, .cdtData$EnvData$tab$TSplot)
        }
    })
}

#######################################

spatialAnalysis.DisplayTSMaps <- function(){
    if(is.null(.cdtData$EnvData)) return(NULL)
    if(is.null(.cdtData$EnvData$statpars)) return(NULL)

    .cdtData$EnvData$plot.maps[c('lon', 'lat', 'id')] <- .cdtData$EnvData$tsdata[c('x0', 'y0', 'id')]

    imgContainer <- CDT.Display.Map.inter(spatialAnalysis.plotTSMaps, .cdtData$EnvData$tab$TSMap, 'Aggregated-Data')
    .cdtData$EnvData$tab$TSMap <- imageNotebookTab_unik(imgContainer, .cdtData$EnvData$tab$TSMap)

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
            imgContainer1 <- CDT.Display.Graph(spatialAnalysis.plotTSGraph, .cdtData$EnvData$tab$TSplot, 'Time-Series-Plot')
            .cdtData$EnvData$tab$TSplot <- imageNotebookTab_unik(imgContainer1, .cdtData$EnvData$tab$TSplot)
        }
    })
}

