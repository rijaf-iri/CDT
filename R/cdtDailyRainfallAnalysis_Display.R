
dailyRainAnalysis.plotMapVarStats <- function(){
    don <- .cdtData$EnvData$statData$map
    dataMapOp <- .cdtData$EnvData$varstatMapOp

    ## titre
    if(!dataMapOp$title$user){
        varstats <- .cdtData$EnvData$output$exist.vars.dates
        this.var <- .cdtData$EnvData$now$this.vars
        this.stat <- .cdtData$EnvData$now$this.stats

        infos <- varstats[[this.var]][[this.stat]]

        titre1 <- switch(this.var,
                         "TOTALRAIN" = 'Total Rainfall',
                         "RAININT" = 'Rainfall Intensity',
                         "WETDAY" = 'Number of Wet Days',
                         "DRYDAY" = 'Number of Dry Days',
                         "WETSPELL" = 'Number of Wet Spells',
                         "DRYSPELL" = 'Number of Dry Spells')

        var.def <- switch(this.var, "TOTALRAIN" = '', "RAININT" = '',
                          "WETDAY" = paste0('(RR >= ', infos$pars[1], ' mm)'),
                          "DRYDAY" = paste0('(RR < ', infos$pars[1], ' mm)'),
                          "WETSPELL" = paste0('(spell: ', infos$pars[2], ' days)'),
                          "DRYSPELL" = paste0('(spell: ', infos$pars[2], ' days)'))

        titre2 <- switch(this.stat,
                         'mean' = 'Mean',
                         'stdev' = 'Standard deviation',
                         'coefvar' = 'Coefficient of variation',
                         'proba' = 'Probability of exceeding')

        units <- switch(this.var, "TOTALRAIN" = 'mm', "RAININT" = 'mm/day', "WETDAY" = 'days',
                        "DRYDAY" = 'days', "WETSPELL" = 'spells', "DRYSPELL" = 'spells')

        proba.def <- switch(this.stat, 'mean' = '', 'stdev' = '', 'coefvar' = '',
                            'proba' = paste0('(', infos$pars[3], " ", units, ')'))

        period.def <- paste0(infos$year[1, 1], '_', infos$year[2, 2], '/', 
                      paste0(format(as.Date(paste0("2000-", strsplit(infos$season, "_")[[1]])), "%b-%d"), collapse = "_"))
        period.def <- paste0("[", period.def, "]")

        .titre <- paste(titre1, var.def, ";", titre2, proba.def, period.def)
    }else .titre <- dataMapOp$title$title

    #################

    .data.type <- .cdtData$EnvData$plot.maps$.data.type
    .plot.type <- str_trim(tclvalue(.cdtData$EnvData$plot.maps$plot.type))
    map.args <- cdt.plotmap.args(don, dataMapOp, .cdtData$EnvData$shp)

    opar <- par(mar = map.args$mar)
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

    par(opar)

    return(par.plot)
}

#######################################

dailyRainAnalysis.plotMapVarTS <- function(){
    don <- .cdtData$EnvData$tsData$map
    dataMapOp <- .cdtData$EnvData$dataMapOp

    ## titre
    if(!dataMapOp$title$user){
        varstats <- .cdtData$EnvData$output$exist.vars.dates
        this.var <- .cdtData$EnvData$now$this.vars
        this.stat <- .cdtData$EnvData$now$this.stats
        this.daty <- str_trim(tclvalue(.cdtData$EnvData$donDate))

        infos <- varstats[[this.var]][[this.stat]]

        titre1 <- switch(this.var,
                         "TOTALRAIN" = 'Total Rainfall',
                         "RAININT" = 'Rainfall Intensity',
                         "WETDAY" = 'Number of Wet Days',
                         "DRYDAY" = 'Number of Dry Days',
                         "WETSPELL" = 'Number of Wet Spells',
                         "DRYSPELL" = 'Number of Dry Spells')

        var.def <- switch(this.var, "TOTALRAIN" = '', "RAININT" = '',
                          "WETDAY" = paste0('(RR >= ', infos$pars[1], ' mm)'),
                          "DRYDAY" = paste0('(RR < ', infos$pars[1], ' mm)'),
                          "WETSPELL" = paste0('(spell: ', infos$pars[2], ' days)'),
                          "DRYSPELL" = paste0('(spell: ', infos$pars[2], ' days)'))

        .titre <- paste(titre1, var.def, paste0("[", this.daty, "]"))
    }else .titre <- dataMapOp$title$title

    #################

    .data.type <- .cdtData$EnvData$plot.maps$.data.type
    .plot.type <- str_trim(tclvalue(.cdtData$EnvData$plot.maps$plot.type))
    map.args <- cdt.plotmap.args(don, dataMapOp, .cdtData$EnvData$shp)

    opar <- par(mar = map.args$mar)
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

    par(opar)

    return(par.plot)
}

#######################################

dailyRainAnalysis.plotVarGraph <- function(){
    TSGraphOp <- .cdtData$EnvData$TSGraphOp
    varstats <- .cdtData$EnvData$output$exist.vars.dates
    this.var <- .cdtData$EnvData$now$this.vars
    this.stat <- .cdtData$EnvData$now$this.stats
    infos <- varstats[[this.var]][[this.stat]]

    if(.cdtData$EnvData$output$params$data.type == "cdtstation"){
        ixy <- which(.cdtData$EnvData$output$data$id == str_trim(tclvalue(.cdtData$EnvData$plot.maps$stnIDTSp)))
        if(length(ixy) == 0){
            Insert.Messages.Out(.cdtData$EnvData$message[['14']], TRUE, 'e')
            return(NULL)
        }
        don <- as.numeric(.cdtData$EnvData$tsData$data[, ixy])
        .cdtData$EnvData$location <- paste0("Station: ", .cdtData$EnvData$output$data$id[ixy])
    }else{
        cdtdataset <- .cdtData$EnvData$cdtdataset
        xloc <- as.numeric(str_trim(tclvalue(.cdtData$EnvData$plot.maps$lonLOC)))
        yloc <- as.numeric(str_trim(tclvalue(.cdtData$EnvData$plot.maps$latLOC)))
        xyloc <- cdtdataset.extarct.TS(cdtdataset, cdtdataset$fileInfo, xloc, yloc)
        if(is.null(xyloc)) return(NULL)
        don <- as.numeric(xyloc$data)
        .cdtData$EnvData$location <- paste0("Longitude: ", round(xloc, 5), ", Latitude: ", round(yloc, 5))
    }

    daty <- as.numeric(substr(varstats[[this.var]]$date, 1, 4))

    ########

    titre1 <- switch(this.var,
                     "TOTALRAIN" = 'Total Rainfall',
                     "RAININT" = 'Rainfall Intensity',
                     "WETDAY" = 'Number of Wet Days',
                     "DRYDAY" = 'Number of Dry Days',
                     "WETSPELL" = 'Number of Wet Spells',
                     "DRYSPELL" = 'Number of Dry Spells')

    var.def <- switch(this.var, "TOTALRAIN" = '', "RAININT" = '',
                      "WETDAY" = paste0('(RR >= ', infos$pars[1], ' mm)'),
                      "DRYDAY" = paste0('(RR < ', infos$pars[1], ' mm)'),
                      "WETSPELL" = paste0('(spell: ', infos$pars[2], ' days)'),
                      "DRYSPELL" = paste0('(spell: ', infos$pars[2], ' days)'))

    titre <- paste(titre1, var.def)

    ########

    xlab0 <- ""
    ylab0 <- ""

    #########

    GRAPHTYPE <- .cdtData$EnvData$plot.maps$typeTSp
    optsgph <- switch(GRAPHTYPE,
                      "line" = TSGraphOp$line,
                      "bar" = TSGraphOp$bar,
                      "proba" = TSGraphOp$proba,
                      "anom" = TSGraphOp$anomaly)

    ## xlim, ylim, xlab, ylab
    if(GRAPHTYPE == "proba"){
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
        if(GRAPHTYPE == "anom")
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
        titre <- if(GRAPHTYPE == "anom") paste("Anomaly:", titre) else titre
        titre.pos <- "top"
    }

    #########

    if(GRAPHTYPE == "line"){
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

    if(GRAPHTYPE == "bar"){
        ret <- graphs.plot.bar(daty, don, xlim = xlim, ylim = ylim,
                        xlab = xlab, ylab = ylab, ylab.sub = NULL,
                        title = titre, title.position = titre.pos, axis.font = 1,
                        barcol = optsgph$colors$col,
                        location = .cdtData$EnvData$location)
    }

    if(GRAPHTYPE == "proba"){
        ret <- graphs.plot.proba(don, xlim = xlim, ylim = ylim,
                        xlab = xlab, xlab.sub = NULL, ylab = ylab,
                        title = titre, title.position = titre.pos, axis.font = 1,
                        proba = list(theoretical = optsgph$proba$theoretical),
                        plotp = optsgph$proba, plotl = optsgph$plot,
                        location = .cdtData$EnvData$location)
    }

    if(GRAPHTYPE == "anom"){
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

    return(ret)
}

##############################################################################

dailyRainAnalysis.Display.MapsVarStats <- function(){
    if(is.null(.cdtData$EnvData)) return(NULL)
    if(is.null(.cdtData$EnvData$output)) return(NULL)

    imgContainer <- CDT.Display.Map.inter(dailyRainAnalysis.plotMapVarStats, .cdtData$EnvData$tab$dataMapStat, 'Analysis-Stats-Map')
    .cdtData$EnvData$tab$dataMapStat <- imageNotebookTab_unik(imgContainer, .cdtData$EnvData$tab$dataMapStat)

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
            imgContainer1 <- CDT.Display.Graph(dailyRainAnalysis.plotVarGraph, .cdtData$EnvData$tab$dataGraph, 'Analysis-Graph')
            .cdtData$EnvData$tab$dataGraph <- imageNotebookTab_unik(imgContainer1, .cdtData$EnvData$tab$dataGraph)
        }
    })
}

#######################################

dailyRainAnalysis.Display.MapVarTS <- function(){
    if(is.null(.cdtData$EnvData)) return(NULL)
    if(is.null(.cdtData$EnvData$output)) return(NULL)
    imgContainer <- CDT.Display.Map.inter(dailyRainAnalysis.plotMapVarTS, .cdtData$EnvData$tab$dataMapTS, 'Analysis-Var-Map')
    .cdtData$EnvData$tab$dataMapTS <- imageNotebookTab_unik(imgContainer, .cdtData$EnvData$tab$dataMapTS)

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
            imgContainer1 <- CDT.Display.Graph(dailyRainAnalysis.plotVarGraph, .cdtData$EnvData$tab$dataGraph, 'Analysis-Graph')
            .cdtData$EnvData$tab$dataGraph <- imageNotebookTab_unik(imgContainer1, .cdtData$EnvData$tab$dataGraph)
        }
    })
}
