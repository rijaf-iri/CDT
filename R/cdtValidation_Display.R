
Validation.plotStatMaps <- function(){
    mapstat <- .cdtData$EnvData$statVAR
    istat <- which(.cdtData$EnvData$Statistics$STN$statNames == mapstat)
    don <- .cdtData$EnvData$Statistics$STN$statistics[istat, ]

    dataMapOp <- .cdtData$EnvData$statMapOp
    typeMap <- str_trim(tclvalue(.cdtData$EnvData$typeMap))

    if(!dataMapOp$title$user){
        titre <- .cdtData$EnvData$Statistics$STN$description[istat]
    }else titre <- dataMapOp$title$title

    legend.texta <- NULL
    if(dataMapOp$colkeyLab$user){
        if(str_trim(dataMapOp$colkeyLab$label) != "")
            legend.texta <- dataMapOp$colkeyLab$label
    }

    #################

    xx <- .cdtData$EnvData$opDATA$lon
    yy <- .cdtData$EnvData$opDATA$lat
    if(typeMap == "Pixels"){
        nx <- nx_ny_as.image(diff(range(xx)))
        ny <- nx_ny_as.image(diff(range(yy)))
        don <- cdt.as.image(don, pts.xy = cbind(xx, yy), nx = nx, ny = ny)
    }else{
        don <- list(x = xx, y = yy, z = don)
    }

    #################

    map.args <- cdt.plotmap.args0(don, user.levels = dataMapOp$userLvl,
                                       user.colors = dataMapOp$userCol,
                                       preset.colors = dataMapOp$presetCol)
    legend.args <- NULL
    if(map.args$horizontal){
        if(!is.null(legend.texta))
            legend.args <- list(text = legend.texta, cex = 0.8, side = 1, line = 1.5)
        map.legend.mar <- map.args$legend.mar
    }else{
        line <- if(max(nchar(as.character(map.args$labels))) > 4) 3 else 2
        if(!is.null(legend.texta)) 
            legend.args <- list(text = legend.texta, cex = 0.8, side = 4, line = line)
        map.legend.mar <- map.args$legend.mar
    }

    #################

    dem <- .cdtData$EnvData$dem
    add.dem <- FALSE
    if(tclvalue(dem$add.dem) == "1" & !is.null(dem$elv)){
        Opts <- dem$Opt[names(dem$Opt) != "add.hill"]
        dem.pars <- do.call(cdt.plotmap.args0, c(list(don = dem$elv), Opts))

        if(map.args$horizontal){
            map.args$mar[1] <- map.args$mar[1] + 3
            map.legend.mar <- 6.5
            dem.legend.mar <- 3
        }else{
            map.args$mar[4] <- map.args$mar[4] + 3
            map.legend.mar <- 10.5
            dem.legend.mar <- 4.5
        }

        if(dem$Opt$add.hill){
            demKol <- paste0(dem.pars$kolor, 50)
            kolFonction <- grDevices::colorRampPalette(dem.pars$kolor)
            hillsKol <- kolFonction(100)
        }else demKol <- dem.pars$kolor

        add.dem <- TRUE
    }

    #################

    shpf <- .cdtData$EnvData$shp
    ocrds <- if(tclvalue(shpf$add.shp) == "1" & !is.null(shpf$ocrds)) shpf$ocrds else matrix(NA, 1, 2)
    SHPOp <- .cdtData$EnvData$SHPOp

    #################

    xlim <- range(c(don$x, ocrds[, 1]), na.rm = TRUE)
    ylim <- range(c(don$y, ocrds[, 2]), na.rm = TRUE)

    #################

    opar <- par(mar = map.args$mar)
    plot(1, xlim = xlim, ylim = ylim, xlab = "", ylab = "", type = "n", xaxt = 'n', yaxt = 'n')
    axlabs <- LatLonAxisLabels(axTicks(1), axTicks(2))
    axis(side = 1, at = axTicks(1), labels = axlabs$xaxl, tcl = -0.2, cex.axis = 1.0)
    axis(side = 2, at = axTicks(2), labels = axlabs$yaxl, tcl = -0.2, las = 1, cex.axis = 1.0)
    title(main = titre, cex.main = 1.5, font.main = 2)

    if(add.dem){
        if(dem$Opt$add.hill)
            image(dem$hill, col = hillsKol, add = TRUE)

        image(dem.pars$don, breaks = dem.pars$breaks, col = demKol, xaxt = 'n', yaxt = 'n', add = TRUE)

        fields::image.plot(zlim = dem.pars$zlim, breaks = dem.pars$breaks2, col = dem.pars$kolor, horizontal = map.args$horizontal,
                           legend.only = TRUE, legend.mar = dem.legend.mar, legend.width = map.args$legend.width,
                           legend.args = NULL, axis.args = list(at = dem.pars$breaks1, labels = dem.pars$labels,
                           cex.axis = 0.8, font = 2, tcl = -0.3, mgp = c(0, 0.5, 0)), legend.shrink = 0.8)
    }

    abline(h = axTicks(2), v = axTicks(1) , col = "lightgray", lty = 3, lwd = 1.3)
    lines(ocrds[, 1], ocrds[, 2], lwd = SHPOp$lwd, col = SHPOp$col)

    if(typeMap == "Points"){
        kolor.p <- map.args$kolor[findInterval(map.args$don$z, map.args$breaks, rightmost.closed = TRUE, left.open = TRUE)]
        points(map.args$don$x, map.args$don$y, col = kolor.p, cex = dataMapOp$pointSize, pch = 20)
    }else{
        image(map.args$don, breaks = map.args$breaks, col = map.args$kolor, xaxt = 'n', yaxt = 'n', add = TRUE)
    }

    fields::image.plot(zlim = map.args$zlim, breaks = map.args$breaks2, col = map.args$kolor, horizontal = map.args$horizontal,
                       legend.only = TRUE, legend.mar = map.legend.mar, legend.width = map.args$legend.width,
                       legend.args = legend.args, axis.args = list(at = map.args$breaks1, labels = map.args$labels,
                       cex.axis = 0.8, font = 2, tcl = -0.3, mgp = c(0, 0.5, 0)), legend.shrink = 0.8)

    box()
    plt <- par("plt")
    usr <- par("usr")
    par(opar)

    return(list(par = c(plt, usr)))
}

###############################

Validation.plotGraph <- function(){
    if(.cdtData$EnvData$GeneralParameters$stat.data == 'all'){
        x <- c(.cdtData$EnvData$opDATA$stnStatData)
        y <- c(.cdtData$EnvData$opDATA$ncStatData)
        title <- "All Data"
    }
    if(.cdtData$EnvData$GeneralParameters$stat.data == 'avg'){
        x <- rowMeans(.cdtData$EnvData$opDATA$stnStatData, na.rm = TRUE)
        y <- rowMeans(.cdtData$EnvData$opDATA$ncStatData, na.rm = TRUE)
        title <- "Spatial Average"
    }
    if(.cdtData$EnvData$GeneralParameters$stat.data == 'stn'){
        istn <- which(.cdtData$EnvData$opDATA$id == tclvalue(.cdtData$EnvData$stnIDGraph))
        x <- .cdtData$EnvData$opDATA$stnStatData[, istn]
        y <- .cdtData$EnvData$opDATA$ncStatData[, istn]
        title <- tclvalue(.cdtData$EnvData$stnIDGraph)
    }

    ##############
    AggrSeries <- .cdtData$EnvData$opDATA$AggrSeries
    if(AggrSeries$aggr.data & AggrSeries$aggr.fun == "count"){
        units <- paste0("(Number of day ", AggrSeries$opr.fun, " ", AggrSeries$opr.thres, ")")
    }else{
        units <- if(.cdtData$EnvData$GeneralParameters$clim.var == "RR") "(mm)" else NA
    }

    ##############

    GraphOp <- .cdtData$EnvData$GraphOp
    GraphType <- .cdtData$EnvData$type.graph

    xmin <- min(c(x, y), na.rm = TRUE)
    xmin <- ifelse(is.infinite(xmin), 0, xmin)
    xmax <- max(c(x, y), na.rm = TRUE)
    xmax <- ifelse(is.infinite(xmax), 0, xmax)

    if(GraphType == "Scatter"){
        optsgph <- GraphOp$scatter

        xlim <- c(xmin, xmax)
        ylim <- c(xmin, xmax)

        xlab <- if(is.na(units)) expression(paste("Station (" * degree, "C)")) else paste('Station', units)
        ylab <- if(is.na(units)) expression(paste("Estimate (" * degree, "C)")) else paste('Estimate', units)
    }
    if(GraphType == "CDF"){
        optsgph <- GraphOp$cdf

        xlim <- c(xmin, xmax)
        ylim <- c(0, 1)

        xlab <- if(.cdtData$EnvData$GeneralParameters$clim.var == "RR") "Rainfall" else "Temperature"
        xlab <- if(is.na(units)) expression(paste(xlab, "(" * degree, "C)")) else paste(xlab, units)
        ylab <- "Cumulative density"
    }
    if(GraphType == "Lines"){
        optsgph <- GraphOp$line

        xlim <- range(.cdtData$EnvData$opDATA$temps, na.rm = TRUE)
        ylim <- c(xmin, xmax)

        xlab <- ""
        ylab <- if(.cdtData$EnvData$GeneralParameters$clim.var == "RR") "Rainfall" else "Temperature"
        ylab <- if(is.na(units)) expression(paste(ylab, "(" * degree, "C)")) else paste(ylab, units)
    }

    ##############

    if(optsgph$xlim$is.min){
        xx <- optsgph$xlim$min
        xx <- if(GraphType == "Lines") as.Date(xx) else as.numeric(xx) 
        if(inherits(xx, "try-error") | is.na(xx))
            Insert.Messages.Out("Invalid xlim minimum value", TRUE, "e")
        else xlim[1] <- xx
    }
    if(optsgph$xlim$is.max){
        xx <- optsgph$xlim$max
        xx <- if(GraphType == "Lines") as.Date(xx) else as.numeric(xx)
        if(inherits(xx, "try-error") | is.na(xx))
            Insert.Messages.Out("Invalid xlim maximum value", TRUE, "e")
        else xlim[2] <- xx
    }

    if(optsgph$ylim$is.min){
        xx <- optsgph$ylim$min
        xx <- if(GraphType == "Lines") as.Date(xx) else as.numeric(xx)
        if(inherits(xx, "try-error") | is.na(xx))
            Insert.Messages.Out("Invalid ylim minimum value", TRUE, "e")
        else ylim[1] <- xx
    }
    if(optsgph$ylim$is.max){
        xx <- optsgph$ylim$max
        xx <- if(GraphType == "Lines") as.Date(xx) else as.numeric(xx)
        if(inherits(xx, "try-error") | is.na(xx))
            Insert.Messages.Out("Invalid ylim maximum value", TRUE, "e")
        else ylim[2] <- xx
    }

    if(optsgph$axislabs$is.xlab) xlab <- optsgph$axislabs$xlab
    if(optsgph$axislabs$is.ylab) ylab <- optsgph$axislabs$ylab

    ##############

    if(optsgph$title$is.title){
        titre <- optsgph$title$title
        titre.pos <- optsgph$title$position
    }else{
        titre <- title
        titre.pos <- "top"
    }

    ##############

    draw.title <- if(str_trim(titre) == "") FALSE else TRUE

    if(draw.title){
        if(GraphType == "Lines"){
            if(titre.pos == 'bottom'){
                plot.position <- matrix(1:3, ncol = 1)
                ht.leg <- if(optsgph$legend$add) 0.1 else 0.01
                plot.heights <- c(0.9, ht.leg, 0.1)
            }else{
                 plot.position <- matrix(c(3, 1, 2), ncol = 1)
                 ht.leg <- if(optsgph$legend$add) 0.1 else 0.01
                 plot.heights <- c(0.1, 0.9, ht.leg)
            }
        }else{
            if(titre.pos == 'bottom'){
                plot.position <- matrix(1:2, ncol = 1)
                plot.heights <- c(0.9, 0.1)
            }else{
                plot.position <- matrix(c(2, 1), ncol = 1)
                plot.heights <- c(0.1, 0.9)
            }
        }
    }else{
        if(GraphType == "Lines"){
            plot.position <- matrix(1:3, ncol = 1)
            ht.leg <- if(optsgph$legend$add) 0.1 else 0.01
            plot.heights <- c(0.9, ht.leg, 0.01)
        }else{
            plot.position <- matrix(1:2, ncol = 1)
            plot.heights <- c(0.9, 0.01)
        }
    }

    top.mar <- if(draw.title & titre.pos == 'top') 1.5 else 2.1
    par.plot <- c(4.0, 5.0, top.mar, 2.1)

    if(draw.title){
        tb.mar <- if(titre.pos == 'bottom') c(1, 0) else c(0, 1)
    }else tb.mar <- c(0, 0)
    par.title <- c(tb.mar[1], 5.0, tb.mar[2], 2.1)

    par.legend <- c(0, 5.0, 0, 2.1)

    ##############

    if(GraphType == "Scatter"){
        layout(plot.position, widths = 1, heights = plot.heights, respect = FALSE)

        op <- par(mar = par.plot)
        plot(1, type = 'n', xaxt = 'n', yaxt = 'n', xlim = xlim, ylim = ylim, xlab = '', ylab = '')

        xminTck <- axTicks(1)
        xminTck <- xminTck[-length(xminTck)] + diff(xminTck) / 2
        xminTck <- c(min(axTicks(1)) - diff(xminTck)[1] / 2, xminTck, max(axTicks(1)) + diff(xminTck)[1] / 2)
        yminTck <- axTicks(2)
        yminTck <- yminTck[-length(yminTck)] + diff(yminTck) / 2
        yminTck <- c(min(axTicks(2)) - diff(yminTck)[1] / 2, yminTck, max(axTicks(2)) + diff(yminTck)[1] / 2)

        abline(h = axTicks(2), col = "lightgray", lty = "solid", lwd = 0.8)
        abline(h = yminTck, col = "lightgray", lty = "dotted")
        abline(v = axTicks(1), col = "lightgray", lty = "solid", lwd = 0.8)
        abline(v = xminTck, col = "lightgray", lty = "dotted")

        axis(1, at = axTicks(1), font = 1)
        axis(1, at = xminTck, labels = NA, tcl = par("tcl") * 0.5)
        mtext(xlab, side = 1, line = 2.5, cex = 1.2)
        axis(2, at = axTicks(2), las = 2, font = 1)
        axis(2, at = yminTck, labels = NA, tcl = par("tcl") * 0.6)
        mtext(ylab, side = 2, line = 3, cex = 1.2)

        if(optsgph$line$draw) abline(a = 0, b = 1, lwd = optsgph$line$lwd, col = optsgph$line$col)
        points(x, y, pch = optsgph$point$pch, col = optsgph$point$col, cex = optsgph$point$cex)
        box()
        par(op)

        op <- par(mar = par.title)
        if(draw.title){
            plot(1, type = 'n', xaxt = 'n', yaxt = 'n', xlab = '', ylab = '')
            bbx <- par("usr")
            rect(bbx[1], bbx[3], bbx[2], bbx[4], col = "ghostwhite")
            text(1, 1, titre, cex = 1.5, font = 2)
        }else plot.new()
        par(op)
    }

    if(GraphType == "CDF"){
        layout(plot.position, widths = 1, heights = plot.heights, respect = FALSE)

        op <- par(mar = par.plot)
        plot(1, type = 'n', xaxt = 'n', yaxt = 'n', xlim = xlim, ylim = ylim, xlab = '', ylab = '')

        xminTck <- axTicks(1)
        xminTck <- xminTck[-length(xminTck)] + diff(xminTck) / 2
        xminTck <- c(min(axTicks(1)) - diff(xminTck)[1] / 2, xminTck, max(axTicks(1)) + diff(xminTck)[1] / 2)
        yminTck <- axTicks(2)
        yminTck <- yminTck[-length(yminTck)] + diff(yminTck) / 2
        yminTck <- c(min(axTicks(2)) - diff(yminTck)[1] / 2, yminTck, max(axTicks(2)) + diff(yminTck)[1] / 2)

        abline(h = axTicks(2), col = "lightgray", lty = "solid", lwd = 0.8)
        abline(h = yminTck, col = "lightgray", lty = "dotted")
        abline(v = axTicks(1), col = "lightgray", lty = "solid", lwd = 0.8)
        abline(v = xminTck, col = "lightgray", lty = "dotted")

        axis(1, at = axTicks(1), font = 1)
        axis(1, at = xminTck, labels = NA, tcl = par("tcl") * 0.5)
        mtext(xlab, side = 1, line = 2.5, cex = 1.2)
        axis(2, at = axTicks(2), las = 2, font = 1)
        axis(2, at = yminTck, labels = NA, tcl = par("tcl") * 0.6)
        mtext(ylab, side = 2, line = 3, cex = 1.2)

        if(any(!is.na(x)) & any(!is.na(y))){
            xax <- seq(min(c(x, y), na.rm = TRUE), max(c(x, y), na.rm = TRUE), length.out = 1000)
            fx <- ecdf(x)
            fy <- ecdf(y)

            if(optsgph$plot$obs$type == 'both'){
                lines(xax, fx(xax), type = 'o', col = optsgph$plot$obs$line, lwd = optsgph$plot$obs$lwd,
                      pch = optsgph$plot$obs$pch, bg = optsgph$plot$obs$points, cex = optsgph$plot$obs$cex)
            }else{
                lines(xax, fx(xax), type = 'l', col = optsgph$plot$obs$line, lwd = optsgph$plot$obs$lwd)
            }

            if(optsgph$plot$est$type == 'both'){
                lines(xax, fy(xax), type = 'o', col = optsgph$plot$est$line, lwd = optsgph$plot$est$lwd,
                      pch = optsgph$plot$est$pch, bg = optsgph$plot$est$points, cex = optsgph$plot$est$cex)
            }else{
                lines(xax, fy(xax), type = 'l', col = optsgph$plot$est$line, lwd = optsgph$plot$est$lwd)
            }

            obs.pch <- if(optsgph$plot$obs$type == 'both') optsgph$plot$obs$pch else NA
            est.pch <- if(optsgph$plot$est$type == 'both') optsgph$plot$est$pch else NA
            obs.bg <- if(optsgph$plot$obs$type == 'both') optsgph$plot$obs$points else NA
            est.bg <- if(optsgph$plot$est$type == 'both') optsgph$plot$est$points else NA

            if(optsgph$legend$add){
                legend('bottomright',
                       legend = c(optsgph$legend$obs, optsgph$legend$est),
                       col = c(optsgph$plot$obs$line, optsgph$plot$est$line),
                       pch = c(obs.pch, est.pch), pt.bg = c(obs.bg, est.bg),
                       pt.cex = 1, pt.lwd = 1, lwd = 3, cex = 1, bty = 'n')
            }
        }
        par(op)

        op <- par(mar = par.title)
        if(draw.title){
            plot(1, type = 'n', xaxt = 'n', yaxt = 'n', xlab = '', ylab = '')
            bbx <- par("usr")
            rect(bbx[1], bbx[3], bbx[2], bbx[4], col = "ghostwhite")
            text(1, 1, titre, cex = 1.5, font = 2)
        }else plot.new()
        par(op)
    }

    if(GraphType == "Lines"){
        daty <- .cdtData$EnvData$opDATA$temps
        layout(plot.position, widths = 1, heights = plot.heights, respect = FALSE)

        op <- par(mar = par.plot)
        plot(1, type = 'n', xaxt = 'n', yaxt = 'n', xlim = xlim, ylim = ylim, xlab = '', ylab = '')

        xTck <- axTicks.Date(daty, 1)
        if(as.numeric(diff(xlim)) > 1095){
            xminTck <- seq(as.Date(paste0(format(xlim[1], "%Y"), "-01-01")),
                        as.Date(paste0(as.numeric(format(xlim[2], "%Y")) + 1, "-01-01")), "year")
            xminTck <- xminTck[!xminTck %in% xTck]
        }else xminTck <- NULL

        yminTck <- axTicks(2)
        yminTck <- yminTck[-length(yminTck)] + diff(yminTck) / 2
        yminTck <- c(min(axTicks(2)) - diff(yminTck)[1] / 2, yminTck, max(axTicks(2)) + diff(yminTck)[1] / 2)

        abline(h = axTicks(2), col = "lightgray", lty = "solid", lwd = 0.8)
        abline(h = yminTck, col = "lightgray", lty = "dotted")
        abline(v = xTck, col = "lightgray", lty = "solid", lwd = 0.8)
        if(!is.null(xminTck))
            abline(v = xminTck, col = "lightgray", lty = "dotted")

        axis.Date(1, at = xTck, font = 1)
        if(!is.null(xminTck))
            axis.Date(1, at = xminTck, labels = NA, tcl = par("tcl") * 0.5)
        mtext(xlab, side = 1, line = 2.5, cex = 1)
        axis(2, at = axTicks(2), las = 2, font = 1)
        axis(2, at = yminTck, labels = NA, tcl = par("tcl") * 0.6)
        mtext(ylab, side = 2, line = 3, cex = 1)

        if(optsgph$plot$obs$type == 'both'){
            lines(daty, x, type = 'o', col = optsgph$plot$obs$line, lwd = optsgph$plot$obs$lwd,
                  pch = optsgph$plot$obs$pch, bg = optsgph$plot$obs$points, cex = optsgph$plot$obs$cex)
        }else{
            lines(daty, x, type = 'l', col = optsgph$plot$obs$line, lwd = optsgph$plot$obs$lwd)
        }

        if(optsgph$plot$est$type == 'both'){
            lines(daty, y, type = 'o', col = optsgph$plot$est$line, lwd = optsgph$plot$est$lwd,
                  pch = optsgph$plot$est$pch, bg = optsgph$plot$est$points, cex = optsgph$plot$est$cex)
        }else{
            lines(daty, y, type = 'l', col = optsgph$plot$est$line, lwd = optsgph$plot$est$lwd)
        }
        par(op)

        op <- par(mar = par.legend)
        plot.new()
        if(optsgph$legend$add){
            obs.pch <- if(optsgph$plot$obs$type == 'both') optsgph$plot$obs$pch else NA
            est.pch <- if(optsgph$plot$est$type == 'both') optsgph$plot$est$pch else NA
            obs.bg <- if(optsgph$plot$obs$type == 'both') optsgph$plot$obs$points else NA
            est.bg <- if(optsgph$plot$est$type == 'both') optsgph$plot$est$points else NA

            legend('top', 'groups', horiz = TRUE,
                   legend = c(optsgph$legend$obs, optsgph$legend$est),
                   col = c(optsgph$plot$obs$line, optsgph$plot$est$line),
                   pch = c(obs.pch, est.pch), pt.bg = c(obs.bg, est.bg),
                   pt.cex = 1, pt.lwd = 1, lwd = 3, cex = 1.5)
        }
        par(op)

        op <- par(mar = par.title)
        if(draw.title){
            plot(1, type = 'n', xaxt = 'n', yaxt = 'n', xlab = '', ylab = '')
            bbx <- par("usr")
            rect(bbx[1], bbx[3], bbx[2], bbx[4], col = "ghostwhite")
            text(1, 1, titre, cex = 2, font = 2)
        }else plot.new()
        par(op)
    }
    return(0)
}

###############################

Validation.DisplayStatMaps <- function(){
    if(is.null(.cdtData$EnvData)) return(NULL)
    if(is.null(.cdtData$EnvData$opDATA)) return(NULL)

    imgContainer <- CDT.Display.Map.inter(Validation.plotStatMaps, .cdtData$EnvData$tab$Maps, 'Statistics-Maps')
    .cdtData$EnvData$tab$Maps <- imageNotebookTab_unik(imgContainer, .cdtData$EnvData$tab$Maps)

    ###############
    tkbind(imgContainer[[2]], "<Button-1>", function(W, x, y){
        xyid <- getIDLatLonCoords(W, x, y, imgContainer[[3]], getStnIDLabel,
                            stn.coords = .cdtData$EnvData$plot.maps[c('lon', 'lat', 'id')])

        if(xyid$plotTS){
            imgContainer1 <- CDT.Display.Graph(Validation.plotGraph, .cdtData$EnvData$tab$Graph, 'Validation-Plot')
            .cdtData$EnvData$tab$Graph <- imageNotebookTab_unik(imgContainer1, .cdtData$EnvData$tab$Graph)
        }
    })
}
