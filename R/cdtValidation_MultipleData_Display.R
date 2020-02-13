
multiValidation.plotGraph <- function(){
    if(.cdtData$EnvData$GeneralParameters$stat.data == 'all'){
        x <- c(.cdtData$EnvData$opDATA$stnStatData)
        y <- lapply(.cdtData$EnvData$opDATA$ncStatData, function(x) c(x))
        title <- "All Data"
    }
    if(.cdtData$EnvData$GeneralParameters$stat.data == 'avg'){
        x <- rowMeans(.cdtData$EnvData$opDATA$stnStatData, na.rm = TRUE)
        y <- lapply(.cdtData$EnvData$opDATA$ncStatData, function(x) rowMeans(x, na.rm = TRUE))
        title <- "Spatial Average"
    }
    if(.cdtData$EnvData$GeneralParameters$stat.data == 'stn'){
        istn <- which(.cdtData$EnvData$opDATA$id == tclvalue(.cdtData$EnvData$stnIDGraph))
        x <- .cdtData$EnvData$opDATA$stnStatData[, istn]
        y <- lapply(.cdtData$EnvData$opDATA$ncStatData, function(x) x[, istn])
        title <- tclvalue(.cdtData$EnvData$stnIDGraph)
    }

    ##############
    AggrSeries <- .cdtData$EnvData$opDATA$AggrSeries
    if(AggrSeries$aggr.fun == "count"){
        units <- paste0("(Number of day ", AggrSeries$opr.fun, " ", AggrSeries$opr.thres, ")")
    }else{
        units <- if(.cdtData$EnvData$GeneralParameters$clim.var == "RR") "(mm)" else NA
    }

    data.name <- .cdtData$EnvData$VALID.names

    ##############
    GraphOp <- .cdtData$EnvData$GraphOp

    plotType <- tclvalue(.cdtData$EnvData$type.graph)

    if(plotType == "Scatter") optsgph <- GraphOp$scatter
    if(plotType == "CDF") optsgph <- GraphOp$cdf
    if(plotType == "Lines") optsgph <- GraphOp$lines

    ##############
    if(optsgph$title$is.title)
        title <- optsgph$title$title

    ##############

    xmin0 <- min(c(x, sapply(y, min, na.rm = TRUE)), na.rm = TRUE)
    xmin <- ifelse(is.infinite(xmin0), 0, xmin0)
    xmax0 <- max(c(x, sapply(y, max, na.rm = TRUE)), na.rm = TRUE)
    xmax <- ifelse(is.infinite(xmax0), 0, xmax0)

    if(plotType == "Scatter"){
        xlim <- c(xmin, xmax)
        ylim <- c(xmin, xmax)
        xlim <- xlim + diff(xlim) * c(-1, 1) * 0.001
        ylim <- ylim + diff(ylim) * c(-1, 1) * 0.001
        if(optsgph$xylim$is.min){
            xx <- str_trim(optsgph$xylim$min)
            ylim[1] <- xlim[1] <- as.numeric(xx)
        }
        if(optsgph$xylim$is.max){
            xx <- str_trim(optsgph$xylim$max)
            ylim[2] <- xlim[2] <- as.numeric(xx)
        }

        #####
        xlab <- if(is.na(units)) expression(paste("Station (" * degree, "C)")) else paste('Station', units)
        ylab <- if(is.na(units)) expression(paste("Estimate (" * degree, "C)")) else paste('Estimate', units)
        if(optsgph$axislabs$is.xlab)
            xlab <- optsgph$axislabs$xlab
        if(optsgph$axislabs$is.ylab)
            ylab <- optsgph$axislabs$ylab

        #####
        legendlab <- NA

        #####
        plotScatter <- optsgph$plot$type
        if(plotScatter == "points"){
            kol.points <- optsgph$plot$col.points
            cex.points <- optsgph$plot$cex.points
        }else{
            kol.hexbin <- colorRampPalette(optsgph$plot$col.hexbin)
        }

        #####
        kol.line <- optsgph$plot$col.line
        lwd.line <- optsgph$plot$wd.line
    }
    if(plotType == "CDF"){
        xlim <- c(xmin, xmax)
        ylim <- c(0, 1)
        if(optsgph$xlim$is.min){
            xx <- str_trim(optsgph$xlim$min)
            xlim[1] <- as.numeric(xx)
        }
        if(optsgph$xlim$is.max){
            xx <- str_trim(optsgph$xlim$max)
            xlim[2] <- as.numeric(xx)
        }
        if(optsgph$ylim$is.min){
            xx <- str_trim(optsgph$ylim$min)
            ylim[1] <- as.numeric(xx)
        }
        if(optsgph$ylim$is.max){
            xx <- str_trim(optsgph$ylim$max)
            ylim[2] <- as.numeric(xx)
        }

        #######
        xlab <- if(.cdtData$EnvData$GeneralParameters$clim.var == "RR") "Rainfall" else "Temperature"
        xlab <- if(is.na(units)) expression(paste(xlab, "(" * degree, "C)")) else paste(xlab, units)
        ylab <- "Cumulative density"
        if(optsgph$axislabs$is.xlab)
            xlab <- optsgph$axislabs$xlab
        if(optsgph$axislabs$is.ylab)
            ylab <- optsgph$axislabs$ylab

        #######
        kol.Obs <- optsgph$col.obs
        nlkol <- length(data.name)
        if(optsgph$col.est$preset){
            funkol <- get(optsgph$col.est$fun, mode = "function")
            kol.Est <- funkol(nlkol)
        }else{
            if(length(optsgph$col.est$col) == nlkol){
                kol.Est <- optsgph$col.est$col
            }else{
                kol.Est <- colorRampPalette(optsgph$col.est$col)(nlkol)
            }
        }

        #######
        legendlab <- c('Station', data.name)
    }
    if(plotType == "Lines"){
        xlim <- range(.cdtData$EnvData$opDATA$temps, na.rm = TRUE)
        ylim <- c(xmin, xmax)

        if(optsgph$xlim$is.min){
            xx <- as.Date(str_trim(optsgph$xlim$min))
            if(is.na(xx)){
                Insert.Messages.Out("Invalid xlim", format = TRUE)
                return(NULL)
            }
            xlim[1] <- as.numeric(xx)
        }
        if(optsgph$xlim$is.max){
            xx <- as.Date(str_trim(optsgph$xlim$max))
            if(is.na(xx)){
                Insert.Messages.Out("Invalid xlim", format = TRUE)
                return(NULL)
            }
            xlim[2] <- as.numeric(xx)
        }
        if(optsgph$ylim$is.min){
            xx <- str_trim(optsgph$ylim$min)
            ylim[1] <- as.numeric(xx)
        }
        if(optsgph$ylim$is.max){
            xx <- str_trim(optsgph$ylim$max)
            ylim[2] <- as.numeric(xx)
        }

        #######
        xlab <- ""
        ylab <- if(.cdtData$EnvData$GeneralParameters$clim.var == "RR") "Rainfall" else "Temperature"
        ylab <- if(is.na(units)) expression(paste(ylab, "(" * degree, "C)")) else paste(ylab, units)

        if(optsgph$axislabs$is.xlab)
            xlab <- optsgph$axislabs$xlab
        if(optsgph$axislabs$is.ylab)
            ylab <- optsgph$axislabs$ylab

        #######
        kol.Obs <- optsgph$col.obs
        nlkol <- length(data.name)
        if(optsgph$col.est$preset){
            funkol <- get(optsgph$col.est$fun, mode = "function")
            kol.Est <- funkol(nlkol)
        }else{
            if(length(optsgph$col.est$col) == nlkol){
                kol.Est <- optsgph$col.est$col
            }else{
                kol.Est <- colorRampPalette(optsgph$col.est$col)(nlkol)
            }
        }

        #######
        legendlab <- c('Station', data.name)
    }

    ##############

    if(plotType == "Scatter"){
        don <- lapply(seq_along(y), function(i) data.frame(x = x, y = y[[i]], name = data.name[i]))
        don <- do.call(rbind, don)

        xyax <- pretty(xlim)

        par.StripText <- list(cex = 1.0, col = 'black', font = 2)
        par.stripCust <- lattice::strip.custom(factor.levels = data.name, bg = 'lightblue')
        par.Settings <- list(background = list(alpha = 1, col = 'white'),
                            layout.widths = list(left.padding = 0.5, right.padding = 0),
                            layout.heights = list(top.padding = 0.5, bottom.padding = 0.5))
        # Xaxis <- list(relation = "same", draw = TRUE, at = xyax, labels = xyax, cex = 1.0, font = 1, alternating = c(1, 2), tck = c(1, 1))
        # Yaxis <- list(relation = "same", draw = TRUE, at = xyax, labels = xyax, cex = 1.0, alternating = c(1, 2), tck = c(1, 1))

        if(plotScatter == "points"){
            pp <- lattice::xyplot(y ~ x | name, don,
                            panel = function(x, y, ...){
                                lattice::panel.abline(h = xyax, v = xyax, col = "lightgray", lty = 3, lwd = 1.3)
                                # lattice::panel.xyplot(x, y, ...)
                                lattice::panel.points(x, y, pch = 20, col = kol.points, cex = cex.points)
                                lattice::panel.abline(a = 0, b = 1, lwd = lwd.line, col = kol.line)
                            }
                        )
        }else{
            pp <- hexbin::hexbinplot(y ~ x | name, don,
                            panel = function(x, y, ...){
                                lattice::panel.abline(h = xyax, v = xyax, col = "lightgray", lty = 3, lwd = 1.3)
                                hexbin::panel.hexbinplot(x, y, ...)
                                lattice::panel.abline(a = 0, b = 1, lwd = lwd.line, col = kol.line)
                            },
                            trans = log, inv = exp, colramp = kol.hexbin, colorkey = TRUE
                        )
        }

        pp <- update(pp, as.table = TRUE, par.settings = par.Settings,
                     par.strip.text = par.StripText, strip = par.stripCust,
                     # scales = list(x = Xaxis, y = Yaxis),
                     xlim = xlim, ylim = ylim, xlab = xlab, ylab = ylab, main = title
                    )

        print(pp)
    }

    if(plotType == "CDF"){
        plot(1, xlim = xlim, ylim = ylim, type = 'n', xlab = xlab, ylab = ylab, main = title)
        abline(h = axTicks(2), col = "lightgray", lty = "dotted")
        abline(v = axTicks(1), col = "lightgray", lty = "dotted")

        plotECDF <- any(!is.na(x)) & Reduce('&', lapply(y, function(x) any(!is.na(x))))
        if(plotECDF){
            fx <- ecdf(x)
            xax <- seq(xmin0, xmax0, length.out = 1000)
            lines(xax, fx(xax), lwd = 2, col = kol.Obs, type = 'l')
            for(j in seq_along(y)){
                fy <- ecdf(y[[j]])
                lines(xax, fy(xax), lwd = 2, col = kol.Est[j], type = 'l')
            }
        }
        legend('bottomright', legendlab, col = c(kol.Obs, kol.Est), lwd = 3, bg = 'lightgoldenrodyellow')
    }

    if(plotType == "Lines"){
        layout(matrix(1:2, ncol = 1), widths = 1, heights = c(0.9, 0.1), respect = FALSE)
        op <- par(mar = c(3, 4, 2, 2))
        plot(.cdtData$EnvData$opDATA$temps, x, xlim = xlim, ylim = ylim, type = 'n', xaxt = 'n', xlab = xlab, ylab = ylab, main = title)

        abline(h = axTicks(2), col = "lightgray", lty = "dotted")

        xTck <- axTicks.Date(.cdtData$EnvData$opDATA$temps, 1)
        if(as.numeric(diff(xlim)) > 1095){
            xminor <- seq(as.Date(paste0(format(xlim[1], "%Y"), "-01-01")),
                        as.Date(paste0(as.numeric(format(xlim[2], "%Y")) + 1, "-01-01")), "year")
            xminor <- xminor[!xminor %in% xTck]
        }else xminor <- NULL
        abline(v = xTck, col = "lightgray", lty = "dotted")
        axis.Date(1, at = xTck, font = 1, cex.axis = 1)
        if(length(xminor) > 0) axis.Date(1, at = xminor, labels = NA, tcl = par("tcl") * 0.5)

        lines(.cdtData$EnvData$opDATA$temps, x, lwd = 2, col = kol.Obs, type = 'l')
        for(j in seq_along(y))
            lines(.cdtData$EnvData$opDATA$temps, y[[j]], lwd = 2, col = kol.Est[j], type = 'l')
        par(op)

        op <- par(mar = c(0, 4, 0, 2))
        plot.new()
        legend('top', 'groups', legend = legendlab, col = c(kol.Obs, kol.Est), lwd = 3, lty = 1, horiz = TRUE)
        par(op)
    }
    return(0)
}

##################################################################################################

multiValidation.plotStatMaps <- function(){
    dataMapOp <- .cdtData$EnvData$statMapOp
    typeMap <- str_trim(tclvalue(.cdtData$EnvData$typeMap))

    #######
    mapstat <- str_trim(tclvalue(.cdtData$EnvData$statistics))
    istat <- sapply(lapply(.cdtData$EnvData$Statistics$STN, '[[', 1), function(x){
        ll <- which(x$description == mapstat)
        if(length(ll)) ll else 0
    })
    ix <- which(istat != 0)
    don <- lapply(.cdtData$EnvData$Statistics$STN[[ix]], function(x) x$statistics[istat[ix], ])

    #######
    xx <- .cdtData$EnvData$opDATA$lon
    yy <- .cdtData$EnvData$opDATA$lat
    if(typeMap == "Pixels"){
        nx <- nx_ny_as.image(diff(range(xx)))
        ny <- nx_ny_as.image(diff(range(yy)))
        don <- lapply(don, function(v) cdt.as.image(v, pts.xy = cbind(xx, yy), nx = nx, ny = ny))
        data.Obj <- lapply(don, '[[', 'z')
        data.Crd <- list(x = don[[1]]$x, y = don[[1]]$y)
    }else{
        data.Obj <- don
        data.Crd <- list(x = xx, y = yy)
    }

    #################
    SHPOp <- .cdtData$EnvData$SHPOp
    ocrds <- .cdtData$EnvData$shp
    ocrds <- if(tclvalue(.cdtData$EnvData$add.shp) == "1" & !is.null(ocrds)) ocrds else matrix(NA, 1, 2)

    #######
    titre <- if(dataMapOp$title$user) dataMapOp$title$title else mapstat
    colorkey.Title <- if(dataMapOp$colkeyLab$user) dataMapOp$colkeyLab$label else ""

    #######
    ## range ocrds
    pars.x <- parAxisPlotFun(range(data.Crd$x))
    pars.y <- parAxisPlotFun(range(data.Crd$y))
    data.Rg <- range(sapply(data.Obj, range, na.rm = TRUE), na.rm = TRUE)
    brks <- image.plot_Legend_pars(data.Rg, dataMapOp$userLvl, dataMapOp$userCol, dataMapOp$presetCol)

    #######
    xylabs <- LatLonAxisLabels(pars.x$axp, pars.y$axp)
    Xaxis <- list(relation = "same", draw = TRUE, at = pars.x$axp, labels = xylabs$xaxl, cex = 1.0, alternating = c(1, 2), tck = c(1, 1))
    Yaxis <- list(relation = "same", draw = TRUE, at = pars.y$axp, labels = xylabs$yaxl, cex = 1.0, alternating = c(1, 2), tck = c(1, 1))

    #######
    nb.plot <- length(data.Obj)
    layout.Obj <- manageLayout(nb.plot)
    place <- if(diff(pars.x$usr) * layout.Obj$dim[1] >= diff(pars.y$usr) * layout.Obj$dim[2]) 'bottom' else 'right'
    panel.Title <- .cdtData$EnvData$VALID.names

    #######
    Plot.Obj <- lapply(data.Obj, function(obj){
        z.val <- obj + 1e-15
        if(typeMap == "Points"){
            kolor.p <- brks$colors[findInterval(z.val, brks$breaks, rightmost.closed = TRUE, left.open = TRUE)]
            ret <- lattice::levelplot(z.val ~ data.Crd$x + data.Crd$y, at = brks$breaks,
                    prepanel = lattice::prepanel.default.xyplot,
                    panel = function(x, y, ...){
                        lattice::panel.lines(ocrds, col = SHPOp$col, lwd = SHPOp$lwd)
                        lattice::panel.abline(h = pars.y$axp, v = pars.x$axp, col = "lightgray", lty = 3, lwd = 1.3)
                        lattice::panel.points(x, y, pch = 20, col = kolor.p, cex = dataMapOp$pointSize)
                    },
                    colorkey = FALSE)
        }

        if(typeMap == "Pixels"){
            ret <- lattice::levelplot(z.val, row.values = data.Crd$x, column.values = data.Crd$y, at = brks$breaks,
                    prepanel = lattice::prepanel.default.levelplot,
                    panel = function(...){
                        lattice::panel.levelplot(...)
                        lattice::panel.lines(ocrds, col = SHPOp$col, lwd = SHPOp$lwd)
                        lattice::panel.abline(h = pars.y$axp, v = pars.x$axp, col = "lightgray", lty = 3)
                    },
                    colorkey = FALSE)
        }

        return(ret)
    })

    ################# ################################

    requireNamespace("latticeExtra", quietly = TRUE)
    Plot.Obj <- do.call(c, Plot.Obj)
    Plot.Obj <- c(Plot.Obj, layout = layout.Obj$dim)

    ######
    pars.key <- switch(place,
                    "bottom" = list(x = 0.5, y = 0, rot = 0, side = place, pad = c(1, 1, 1, 2)),
                    "right" = list(x = 1, y = 0.5, rot = 90, side = place, pad = c(1, 2, 1, 1))
                    )
    #######
    par.StripText <- list(cex = 1.0, col = 'black', font = 2)
    par.stripCust <- lattice::strip.custom(factor.levels = panel.Title, bg = 'lightgray')

    par.Settings <- list(background = list(alpha = 1, col = 'white'),
                        layout.widths = list(left.padding = pars.key$pad[1], right.padding = pars.key$pad[2]),
                        layout.heights = list(top.padding = pars.key$pad[3], bottom.padding = pars.key$pad[4]))

    #######
    colorkey <- list(space = place, col = brks$colors, at = brks$legend.breaks$breaks,
                    labels = list(labels = round(brks$legend.axis$labels, 6), at = brks$legend.axis$at, cex = 1.0, col = 'black', rot = 0),
                    axis.line = list(alpha = 0.5, lty = 1, lwd = 1, col = 'black'),
                    width = 1, height = 0.8)
    colorkey.Frame <- lattice::draw.colorkey(key = colorkey, draw = FALSE, vp = NULL)

    #######
    grob.Obj <- grid::textGrob(colorkey.Title, x = pars.key$x, y = pars.key$y, rot = pars.key$rot,
                        just = c("center", "center"),
                        gp = grid::gpar(fontsize = 12, fontface = 'plain', col = "black", cex = 1.0))

    lezandy <- NULL
    lezandy[[place]]$fun <- grid::packGrob(frame = colorkey.Frame, grob = grob.Obj, side = pars.key$side, dynamic = TRUE)

    #######
    print(update(Plot.Obj, col.regions = brks$colors,  as.table = TRUE,
                xlim = pars.x$usr, ylim = pars.y$usr, xlab = '', ylab = '', main = titre,
                par.settings = par.Settings, par.strip.text = par.StripText, strip = par.stripCust,
                scales = list(x = Xaxis, y = Yaxis), legend = lezandy))
    return(0)
}

##################################################################################################

image.foramtted.table <- function(X, rk, title = "", col.text = c("red", "orange", "black"),
                                  col.fill = rev(RColorBrewer::brewer.pal(9, "Blues")))
{
    stopifnot(is.matrix(X), is.matrix(rk))
    nc <- ncol(X)
    nr <- nrow(X)
    mc <- colnames(X)
    mr <- rownames(X)

    rg <- range(rk, na.rm = TRUE)
    breaks <- rg + 0.5 * c(-1, 1)
    breaks <- seq(breaks[1], breaks[2], 1)
    nbc <- length(breaks) - 1
    if(class(col.fill) == "function"){
        foo <- col.fill
    }else{
        if(length(col.fill) == 1 & is.character(col.fill)){
            foo <- get(col.fill, mode = "function")
        }else{
            foo <- colorRampPalette(col.fill)
        }
    }
    kolor <- foo(nbc)

    nbx <- length(col.text)
    if(nbx < nbc){
        if(nbx == 1){
            col.txt <- rep(col.text, nbc)
        }else{
            col.txt <- c(col.text[1:(nbx -1)], rep(col.text[nbx], nbc - nbx + 1))
        }
    }else col.txt <- col.text[1:nbc]

    text.kol <- col.txt[findInterval(c(rk), breaks, rightmost.closed = TRUE, left.open = TRUE)]

    x <- 1:nc
    y <- 1:nr
    centers <- expand.grid(y, x)

    op <- par(mar = c(4.5, 7, 5, 2))
    plot(1, xlim = range(x) + c(-0.5, 0.5), ylim = c(max(y) + 0.5, min(y) - 0.5),
         xlab = "", ylab = "",type = "n", xaxt = 'n', yaxt = 'n', xaxs = "i", yaxs = "i")

    title(title, line = 3.5)

    axis(side = 3, at = x, tcl = -0.2, labels = FALSE)
    text(x = x, y = par("usr")[4] - 0.6, srt = 45, adj = 0, labels = mc, cex = 1, xpd = TRUE)
    mtext(mr, at = y, side = 2, las = 1, adj = 1.2, cex = 1)
    image(x, y, t(rk), col = kolor, breaks = breaks, xaxt = 'n', yaxt = 'n', add = TRUE)
    text(centers[, 2], centers[, 1], round(c(X), 3), cex = 1, col = text.kol)
    abline(h = y + 0.5)
    abline(v = x + 0.5)

    fields::image.plot(zlim = rg, col = rev(kolor), horizontal = TRUE, legend.only = TRUE,
                       legend.mar = 3.5, legend.width = 0.7, legend.shrink = 0.5, 
                       axis.args = list(at = c(1, nbc), labels = c("Weakest", "Strongest"),
                                        cex.axis = 1, font = 2, tcl = 0, mgp = c(0, -0.2, 0)),
                       legend.args = list(text = "Performance", cex = 1, side = 1, line = 0.5, font = 2)
                    )
    par(op)
}

##################################

multiValidation.plotRank <- function(){
    dataset <- toupper(.cdtData$EnvData$GeneralParameters$stat.data)
    don <- .cdtData$EnvData$Statistics[[dataset]]

    descrip <- lapply(lapply(don, "[[", 1), '[[', 'description')
    pscore <- lapply(lapply(don, "[[", 1), '[[', 'perfect.score')
    nstats <- lapply(lapply(lapply(don, "[[", 1), '[[', 'statistics'), rownames)
    infos <- list(description = unname(do.call(c, descrip)),
                  perfect.score = unname(do.call(c, pscore)),
                  stats = unname(do.call(c, nstats)))
    if(dataset == "STN"){
        istn <- which(.cdtData$EnvData$opDATA$id == tclvalue(.cdtData$EnvData$stnIDRank))
        stats <- lapply(don, function(x){
            s <- lapply(x, '[[', 'statistics')
            do.call(cbind, lapply(s, function(y) y[, istn]))
        })
    }else{
        stats <- lapply(don, function(x){
            s <- lapply(x, '[[', 'statistics')
            do.call(cbind, s)
        })
    }

    stats <- do.call(rbind, stats)
    colnames(stats) <- .cdtData$EnvData$VALID.names

    rang <- abs(infos$perfect.score - stats)
    rang <- t(apply(rang, 1, rank, ties.method = "average"))
    rang[is.na(stats)] <- NA

    col.text <- c("red", "orange", "black")
    col.fill <- rev(RColorBrewer::brewer.pal(9, "Blues"))
    titre <- switch(dataset,
                    "ALL" = "All Data",
                    "AVG" = "Spatial Average", 
                    "STN" = tclvalue(.cdtData$EnvData$stnIDRank)
                  )

    image.foramtted.table(stats, rang, titre, col.text = col.text, col.fill = col.fill)

    return(0)
}
