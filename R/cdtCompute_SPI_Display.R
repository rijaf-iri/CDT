
SPICalc.Plot.Map <- function(){
    don <- .cdtData$EnvData$varData$map
    dataMapOp <- .cdtData$EnvData$dataMapOp

    ## titre
    if(!dataMapOp$title$user){
        scales <- str_trim(tclvalue(.cdtData$EnvData$spi.tscale))
        this.daty <- .cdtData$EnvData$varData$spi$this.daty
        if(.cdtData$EnvData$output$params$Indices == "Decile"){
            yy <- substr(this.daty, 1, 4)
            mo <- as.numeric(substr(this.daty, 5, 6))
            MOIS <- format(ISOdate(2014, 1:12, 1), "%b")
            if(scales == "Decile-1-Dekad"){
                dek <- c('st', 'nd', 'rd')
                dk <- as.numeric(substr(this.daty, 7, 7))
                titre <- paste0(dk, dek[dk], " dekad of ", MOIS[mo], " ", yy)
            }else{
                tscale <- as.numeric(strsplit(scales, '-')[[1]][2])
                if(tscale > 1){
                    daty <- addMonths(as.Date(paste(yy, mo, 1, sep = '-')), -tscale + 1)
                    titre <- paste0(format(daty, "%b"), "-", MOIS[mo], " ", yy)
                }else titre <- paste(MOIS[mo], yy)
            }
        }else titre <- this.daty

        .titre <- paste(scales, ":", titre)
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

SPICalc.Plot.Graph <- function(){
    if(.cdtData$EnvData$output$params$data.type == "cdtstation"){
        ixy <- which(.cdtData$EnvData$output$data$id == str_trim(tclvalue(.cdtData$EnvData$plot.maps$stnIDTSp)))
        if(length(ixy) == 0){
            Insert.Messages.Out("Station not found", format = TRUE)
            return(NULL)
        }
        varid <- if(.cdtData$EnvData$output$params$Indices == "Decile") 'decile' else 'spi'
        don <- as.numeric(.cdtData$EnvData$cdtdataset[[varid]][, ixy])

        .cdtData$EnvData$location <- paste0("Station: ", .cdtData$EnvData$output$data$id[ixy])
        titre <- paste0("(", .cdtData$EnvData$output$data$id[ixy], ")")
    }else{
        cdtdataset <- .cdtData$EnvData$cdtdataset
        xloc <- as.numeric(str_trim(tclvalue(.cdtData$EnvData$plot.maps$lonLOC)))
        yloc <- as.numeric(str_trim(tclvalue(.cdtData$EnvData$plot.maps$latLOC)))
        xyloc <- cdtdataset.extarct.TS(cdtdataset, cdtdataset$fileInfo, xloc, yloc)
        if(is.null(xyloc)) return(NULL)
        don <- as.numeric(xyloc$data)
        .cdtData$EnvData$location <- paste0("Longitude: ", round(xloc, 5), ", Latitude: ", round(yloc, 5))
        titre <- ""
    }

    daty <- .cdtData$EnvData$varData$ts$dates
    if(.cdtData$EnvData$varData$ts$step == "Dekad"){
        seqtime <- as.Date(daty, "%Y%m%d")
        daty <- as.Date(paste0(format(seqtime, "%Y-%m-"), c(1, 11, 21)[as.numeric(format(seqtime, "%d"))]))
    }
    if(.cdtData$EnvData$varData$ts$step == "Month")
        daty <- as.Date(paste0(daty, "01"), "%Y%m%d")

    #########

    titre <- paste(str_trim(tclvalue(.cdtData$EnvData$spi.tscale)), titre)

    #########
    TSGraphOp <- .cdtData$EnvData$TSGraphOp

    GRAPHTYPE <- str_trim(tclvalue(.cdtData$EnvData$plot.maps$typeTSp))
    # if(GRAPHTYPE == "Bar-Line") optsgph <- TSGraphOp$bar.line
    # if(GRAPHTYPE == "Polygon") optsgph <- TSGraphOp$polygon
    optsgph <- TSGraphOp$bar.line

    xlim <- range(daty, na.rm = TRUE)
    if(optsgph$xlim$is.min){
        xx <- strsplit(optsgph$xlim$min, "-")[[1]]

        if(.cdtData$EnvData$varData$ts$step == "Dekad"){
            x3 <- as.numeric(xx[3])
            if(is.na(x3) | x3 < 1 | x3 > 3){
                Insert.Messages.Out("xlim: dekad must be 1, 2 or 3", format = TRUE)
                return(NULL)
            }
            x3 <- c(1, 11, 21)[x3]
        }
        if(.cdtData$EnvData$varData$ts$step == "Month") x3 <- 1
        x1 <- as.numeric(xx[1])
        x2 <- str_pad(as.numeric(xx[2]), 2, pad = "0")
        x3 <- str_pad(x3, 2, pad = "0")
        xx <- as.Date(paste0(x1, x2, x3), "%Y%m%d")
        if(is.na(xx)){
            Insert.Messages.Out("xlim: invalid date", format = TRUE)
            return(NULL)
        }
        xlim[1] <- xx
    }
    if(optsgph$xlim$is.max){
        xx <- strsplit(optsgph$xlim$max, "-")[[1]]
        if(.cdtData$EnvData$varData$ts$step == "Dekad"){
            x3 <- as.numeric(xx[3])
            if(is.na(x3) | x3 < 1 | x3 > 3){
                Insert.Messages.Out("xlim: dekad must be 1, 2 or 3", format = TRUE)
                return(NULL)
            }
            x3 <- c(1, 11, 21)[x3]
        }
        if(.cdtData$EnvData$varData$ts$step == "Month") x3 <- 1
        x1 <- as.numeric(xx[1])
        x2 <- str_pad(as.numeric(xx[2]), 2, pad = "0")
        x3 <- str_pad(x3, 2, pad = "0")
        xx <- as.Date(paste0(x1, x2, x3), "%Y%m%d")
        if(is.na(xx)){
            Insert.Messages.Out("xlim: invalid date", format = TRUE)
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

    yticks <- if(optsgph$userYTcks$custom) optsgph$userYTcks$ticks else NULL
    loko <- c(optsgph$colors$negative, optsgph$colors$positive)

    #########

    if(GRAPHTYPE == "Bar-Line"){
        ret <- graphs.plot.bar.line(daty, don, y0 = optsgph$colors$y0, yticks = yticks,
                        xlim = xlim, ylim = ylim, xlab = xlab, ylab = ylab, ylab.sub = NULL,
                        title = titre, title.position = titre.pos, axis.font = 1,
                        barcol = loko, plot.line = optsgph$line, location = .cdtData$EnvData$location)
    }

    if(GRAPHTYPE == "Polygon"){
        ret <- graphs.plot.polygon(daty, don, y0 = optsgph$colors$y0, yticks = yticks,
                        xlim = xlim, ylim = ylim, xlab = xlab, ylab = ylab, ylab.sub = NULL,
                        title = titre, title.position = titre.pos, axis.font = 1,
                        fillcol = loko, plot.line = optsgph$line, location = .cdtData$EnvData$location)
    }

    return(ret)
}

#######################################

SPICalc.Plot.VizTS <- function(){
    tkconfigure(.cdtEnv$tcl$main$win, cursor = 'watch')
    tcl('update')
    on.exit({
        tkconfigure(.cdtEnv$tcl$main$win, cursor = '')
        tcl('update')
    })

    cdtParallelCond <- .cdtData$Config[c('dopar', 'detect.cores', 'nb.cores')]

    op <- par(bg = "white")

    if(.cdtData$EnvData$output$params$data.type == "cdtstation"){
        ixy <- which(.cdtData$EnvData$output$data$id == str_trim(tclvalue(.cdtData$EnvData$plot.maps$stnIDTSp)))
        if(length(ixy) == 0){
            Insert.Messages.Out("Station not found", format = TRUE)
            return(NULL)
        }

        if(.cdtData$EnvData$output$params$Indices %in% c("SPI", "Decile")){
            don <- as.numeric(.cdtData$EnvData$spiViz$cdtdataset$data[, ixy])
            daty <- .cdtData$EnvData$spiViz$cdtdataset$dates
        }else{
            prec <- as.numeric(.cdtData$EnvData$spiViz$cdtdataset$prec$data[, ixy])
            etp <- as.numeric(.cdtData$EnvData$spiViz$cdtdataset$etp$data[, ixy])
            don <- prec - etp
            daty <- .cdtData$EnvData$spiViz$cdtdataset$prec$dates
        }

        .cdtData$EnvData$location <- paste0("Station: ", .cdtData$EnvData$output$data$id[ixy])
        titre <- paste0("(", .cdtData$EnvData$output$data$id[ixy], ")")
    }else{
        cdtdataset <- .cdtData$EnvData$spiViz$cdtdataset

        if(.cdtData$EnvData$output$params$Indices %in% c("SPI", "Decile")){
            xlon <- cdtdataset$coords$mat$x
            xlat <- cdtdataset$coords$mat$y
        }else{
            xlon <- cdtdataset$prec$coords$mat$x
            xlat <- cdtdataset$prec$coords$mat$y
        }

        ilon <- as.numeric(str_trim(tclvalue(.cdtData$EnvData$plot.maps$lonLOC)))
        ilat <- as.numeric(str_trim(tclvalue(.cdtData$EnvData$plot.maps$latLOC)))

        iclo <- findInterval(ilon, xlon)
        ilo <- iclo + (2 * ilon > xlon[iclo] + xlon[iclo + 1])
        icla <- findInterval(ilat, xlat)
        ila <- icla + (2 * ilat > xlat[icla] + xlat[icla + 1])

        if(is.na(ilo) | is.na(ila)){
            Insert.Messages.Out("Coordinates outside of data range", format = TRUE)
            return(NULL)
        }
        ixy <- ilo + length(xlon) * (ila - 1)

        if(.cdtData$EnvData$output$params$Indices %in% c("SPI", "Decile")){
            don <- readCdtDatasetChunk.locations(ixy, cdtdataset$fileInfo, cdtdataset, parllCond = cdtParallelCond, do.par = FALSE)
            don <- as.numeric(don$data[, 1])
            daty <- cdtdataset$dateInfo$date
        }else{
            prec <- readCdtDatasetChunk.locations(ixy, cdtdataset$fileInfo.prec, cdtdataset$prec, parllCond = cdtParallelCond, do.par = FALSE)
            etp <- readCdtDatasetChunk.locations(ixy, cdtdataset$fileInfo.etp, cdtdataset$etp, parllCond = cdtParallelCond, do.par = FALSE)
            don <- as.numeric(prec$data[, 1]) - as.numeric(etp$data[, 1])
            daty <- cdtdataset$prec$dateInfo$date
        }
        .cdtData$EnvData$location <- paste0("Longitude: ", round(ilon, 5), ", Latitude: ", round(ilat, 5))
        titre <- ""
    }

    #################

    spiVizOpt <- .cdtData$EnvData$spiVizOp
    tscales <- 1:as.numeric(str_trim(tclvalue(.cdtData$EnvData$spiViz$max.tscale)))

    #################

    calculSPI <- list(.cdtData$EnvData$spiViz$tstep, .cdtData$EnvData$location, tscales)
    if(is.null(.cdtData$EnvData$spiViz$calculSPI)){
        doCalcul <- TRUE
    }else{
        doCalcul <- if(!isTRUE(all.equal(.cdtData$EnvData$spiViz$calculSPI, calculSPI))) TRUE else FALSE
    }

    if(doCalcul){
        if(.cdtData$EnvData$spiViz$tstep == "dekadal"){
            don <- tapply(don, substr(daty, 1, 6), sum, na.rm = TRUE)
            daty <- names(don)
            don <- as.numeric(don)
        }
        don <- matrix(don, ncol = 1)

        spi.mat <- lapply(tscales, function(tsc){
            if(.cdtData$EnvData$output$params$Indices == "Decile")
                res <- Deciles_function(don, daty, .cdtData$EnvData$output$params$base.period, tscale = tsc, outfreq = "month")
            else
                res <- SPEI_function(don, tscale = tsc, distribution = .cdtData$EnvData$output$params$distr)
            return(res)
        })
        spi.mat <- do.call(cbind, spi.mat)

        daty <- as.Date(paste0(daty, "01"), "%Y%m%d")
        .cdtData$EnvData$spiViz$calculSPI <- calculSPI
        .cdtData$EnvData$spiViz$spi.mat <- spi.mat
        .cdtData$EnvData$spiViz$daty <- daty
    }else{
        spi.mat <- .cdtData$EnvData$spiViz$spi.mat
        daty <- .cdtData$EnvData$spiViz$daty
    }

    #################
    ## titre
    if(!spiVizOpt$title$user){
        titre <- paste("Time-scales visualization", titre)
    }else titre <- spiVizOpt$title$title

    #################

    xlab <- if(spiVizOpt$axislabs$is.xlab) spiVizOpt$axislabs$xlab else ''
    ylab <- if(spiVizOpt$axislabs$is.ylab) spiVizOpt$axislabs$ylab else ''

    #################
    ## colorscale title
    if(spiVizOpt$colkeyLab$user){
        legend.texta <- spiVizOpt$colkeyLab$label
    }else legend.texta <- NULL

    #################
    ## breaks
    brks <- image.plot_Legend_pars(spi.mat, spiVizOpt$userLvl, spiVizOpt$userCol, spiVizOpt$presetCol)
    spi.mat <- spi.mat + 1e-15
    breaks <- brks$breaks
    zlim <- brks$legend.breaks$zlim
    breaks2 <- brks$legend.breaks$breaks
    kolor <- brks$colors
    breaks1 <- brks$legend.axis$at
    lab.breaks <- brks$legend.axis$labels

    ## legend label
    legendLabel <- lab.breaks

    #################

    xlim <- range(daty, na.rm = TRUE)
    ylim <- range(tscales)

    #################

    horizontal <- FALSE
    legend.mar <- 5.2
    mar <- c(4, 4, 2.5, 5.5)
    legend.width <- 0.9
    line <- if(max(nchar(as.character(breaks))) > 4) 3 else 2
    legend.args <- if(!is.null(legend.texta)) list(text = legend.texta, cex = 0.8, side = 4, line = line) else NULL

    #################

    opar <- par(mar = mar)
    plot(1, xlim = xlim, ylim = ylim, xlab = "", ylab = "", type = "n", xaxt = 'n', yaxt = 'n', xaxs = "i")

    xTck <- axTicks.Date(daty, 1)
    yTck <- axTicks(2)

    if(as.numeric(diff(xlim)) > 1095){
        xminor <- seq(as.Date(paste0(format(xlim[1], "%Y"), "-01-01")),
                    as.Date(paste0(as.numeric(format(xlim[2], "%Y")) + 1, "-01-01")), "year")
        xminor <- xminor[!xminor %in% xTck]
    }else xminor <- NULL

    if(as.numeric(diff(ylim)) > 5){
        yminor <- seq(floor(ylim[1]), floor(ylim[2]), 1)
        yminor <- yminor[!yminor %in% yTck]
    }else yminor <- NULL

    axis.Date(1, at = xTck, cex.axis = 0.8)
    if(length(xminor) > 0) axis.Date(1, at = xminor, labels = NA, tcl = par("tcl") * 0.5)
    axis(2, at = yTck, las = 1, cex.axis = 0.8)
    if(length(yminor) > 0) axis(2, at = yminor, labels = NA, tcl = par("tcl") * 0.5)

    mtext(xlab, side = 1, line = 2.1)
    mtext(ylab, side = 2, line = 2.1)
    mtext(.cdtData$EnvData$location, side = 3, outer = FALSE, adj = 1, line = 0, cex = 0.6)
    title(main = titre, cex.main = 1, font.main = 2)

    # image(daty, tscales, spi.mat, breaks = breaks, col = kolor, xaxt = 'n', yaxt = 'n', add = TRUE)
    .filled.contour(daty, tscales, spi.mat, levels = breaks, col = kolor)
    fields::image.plot(zlim = zlim, breaks = breaks2, col = kolor, horizontal = horizontal,
                legend.only = TRUE, legend.mar = legend.mar, legend.width = legend.width,
                legend.args = legend.args, axis.args = list(at = breaks1, labels = legendLabel,
                cex.axis = 0.7, font = 2, tcl = -0.3, mgp = c(0, 0.5, 0)), legend.shrink = 0.8)

    abline(h = yTck, v = xTck, col = "lightgray", lty = 3)
    box()
    par(opar)

    par(op)
    return(0)
}

##############################################################################

SPICalc.Display.Maps <- function(title.map, title.graph){
    if(is.null(.cdtData$EnvData)) return(NULL)
    if(is.null(.cdtData$EnvData$output)) return(NULL)

    imgContainer <- CDT.Display.Map.inter(SPICalc.Plot.Map, .cdtData$EnvData$tab$dataMap, title.map)
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
            imgContainer1 <- CDT.Display.Graph(SPICalc.Plot.Graph, .cdtData$EnvData$tab$dataGraph, title.graph)
            .cdtData$EnvData$tab$dataGraph <- imageNotebookTab_unik(imgContainer1, .cdtData$EnvData$tab$dataGraph)
        }
    })
}
