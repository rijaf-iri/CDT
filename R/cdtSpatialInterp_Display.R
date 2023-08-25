
spatialInterp.plotMap <- function(){
    don <- .cdtData$EnvData$mapdata
    dataMapOp <- .cdtData$EnvData$dataMapOp

    ###########

    SHPOp <- .cdtData$EnvData$SHPOp
    ocrds <- .cdtData$EnvData$shp$ocrds
    ocrds <- if(tclvalue(.cdtData$EnvData$shp$add.shp) == "1" & !is.null(ocrds)) ocrds else matrix(NA, 1, 2)

    titre <- if(dataMapOp$title$user) dataMapOp$title$title else don$t
    legend.texta <- NULL
    if(dataMapOp$colkeyLab$user){
        legend.texta <- dataMapOp$colkeyLab$label
        if(trimws(legend.texta) == "") legend.texta <- NULL
    }


    pars.x <- parAxisPlotFun(range(c(don$mapstn$x, don$mapncdf$x, ocrds[, 1]), na.rm = TRUE))
    pars.y <- parAxisPlotFun(range(c(don$mapstn$y, don$mapncdf$y, ocrds[, 2]), na.rm = TRUE))
    xylabs <- LatLonAxisLabels(pars.x$axp, pars.y$axp)

    brks <- image.plot_Legend_pars(c(don$mapstn$z, don$mapncdf$z), dataMapOp$userLvl, dataMapOp$userCol, dataMapOp$presetCol)

    ###################################

    panel.number <- .cdtData$EnvData$map$panelMap

    if(panel.number == "one"){
        if(diff(pars.x$usr) > diff(pars.y$usr)){
            horizontal <- TRUE
            legend.mar <- 3.5
            legend.width <- 0.9
            mar <- c(7, 4, 2.5, 2.5)
            legend.args <- if(!is.null(legend.texta)) list(text = legend.texta, cex = 1.0, side = 1, line = 2) else NULL
        }else{
            horizontal <- FALSE
            legend.mar <- 6.2
            mar <- c(4, 4, 2.5, 6)
            legend.width <- 0.9
            line <- if(max(nchar(as.character(brks$legend.axis$labels))) > 4) 3 else 2
            legend.args <- if(!is.null(legend.texta)) list(text = legend.texta, cex = 1.0, side = 4, line = line) else NULL
        }

        opar <- graphics::par(mar = mar)

        plot(1, xlim = pars.x$usr, ylim = pars.y$usr, xlab = "", ylab = "", type = "n", xaxt = 'n', yaxt = 'n')
        graphics::axis(side = 1, at = pars.x$axp, labels = xylabs$xaxl, tcl = -0.2, cex.axis = 1.0)
        graphics::axis(side = 2, at = pars.y$axp, labels = xylabs$yaxl, tcl = -0.2, las = 1, cex.axis = 1.0)
        graphics::title(main = titre, cex.main = 1.5, font.main = 2)

        don$mapncdf$z <- don$mapncdf$z + 1e-15
        if(don$mapncdf$p == "Pixels")
            graphics::image(don$mapncdf, breaks = brks$breaks, col = brks$colors, xaxt = 'n', yaxt = 'n', add = TRUE)
        if(don$mapncdf$p == "FilledContour")
            graphics::.filled.contour(don$mapncdf$x, don$mapncdf$y, don$mapncdf$z, levels = brks$breaks, col = brks$colors)

        graphics::abline(h = pars.y$axp, v = pars.x$axp, col = "lightgray", lty = 3, lwd = 1.3)
        graphics::lines(ocrds[, 1], ocrds[, 2], lwd = SHPOp$lwd, col = SHPOp$col)

        kolor.p <- brks$colors[findInterval(don$mapstn$z + 1e-15, brks$breaks,
                                            rightmost.closed = TRUE, left.open = TRUE)]
        graphics::points(don$mapstn$x, don$mapstn$y, bg = kolor.p, col = dataMapOp$pointCol, cex = dataMapOp$pointSize, pch = 21)

        fields::image.plot(zlim = brks$legend.breaks$zlim, breaks = brks$legend.breaks$breaks,
                           col = brks$colors, horizontal = horizontal, legend.only = TRUE,
                           legend.mar = legend.mar, legend.width = legend.width, legend.args = legend.args,
                           axis.args = list(at = brks$legend.axis$at, labels = brks$legend.axis$labels,
                           cex.axis = 1.0, font = 2, tcl = -0.3, mgp = c(0, 0.5, 0)), legend.shrink = 0.8)
        graphics::par(opar)
    }

    ###################################

    if(panel.number == "two"){
        Xaxis <- list(relation = "same", draw = TRUE, at = pars.x$axp, labels = xylabs$xaxl, cex = 1.0, alternating = c(1, 2), tck = c(1, 1))
        Yaxis <- list(relation = "same", draw = TRUE, at = pars.y$axp, labels = xylabs$yaxl, cex = 1.0, alternating = c(1, 2), tck = c(1, 1))

        #######
        # nb.plot <- length(data.Obj)
        layout.Obj <- manageLayout(2)
        place <- if(diff(pars.x$usr) * layout.Obj$dim[1] >= diff(pars.y$usr) * layout.Obj$dim[2]) 'bottom' else 'right'
        panel.Title <- c("Stations", "Interpolated")

        #######

        Plot.Obj <- lapply(don[c('mapstn', 'mapncdf')], function(obj){
            if(obj$mp == "Grid"){
                if(obj$p == "Pixels") plot.type <- "grid"
                if(obj$p == "FilledContour") plot.type <- "filledcontour"
            }
            if(obj$mp == "Points"){
                if(obj$p == "Pixels") plot.type <- "grid"
                if(obj$p == "Points") plot.type <- "point"
            }

            z.val <- obj$z + 1e-15

            if(plot.type == "grid"){
                ret <- lattice::levelplot(z.val, row.values = obj$x, column.values = obj$y, at = brks$breaks,
                        prepanel = lattice::prepanel.default.levelplot,
                        panel = function(...){
                            lattice::panel.levelplot(...)
                            lattice::panel.lines(ocrds, col = SHPOp$col, lwd = SHPOp$lwd)
                            lattice::panel.abline(h = pars.y$axp, v = pars.x$axp, col = "lightgray", lty = 3)
                        },
                        colorkey = FALSE)
            }

            if(plot.type == "filledcontour"){
                graphics::plot.new()
                ret <- lattice::levelplot(z.val, row.values = obj$x, column.values = obj$y, at = brks$breaks,
                        prepanel = lattice::prepanel.default.levelplot,
                        panel = function(...){
                            panel.filledcontour(...)
                            lattice::panel.lines(ocrds, col = SHPOp$col, lwd = SHPOp$lwd)
                            lattice::panel.abline(h = pars.y$axp, v = pars.x$axp, col = "lightgray", lty = 3, lwd = 1.3)
                        },
                        colorkey = FALSE)
            }

            if(plot.type == "point"){
                kolor.p <- brks$colors[findInterval(z.val, brks$breaks, rightmost.closed = TRUE, left.open = TRUE)]
                ret <- lattice::levelplot(z.val ~ obj$x + obj$y, at = brks$breaks,
                        prepanel = lattice::prepanel.default.xyplot,
                        panel = function(x, y, ...){
                            lattice::panel.lines(ocrds, col = SHPOp$col, lwd = SHPOp$lwd)
                            lattice::panel.abline(h = pars.y$axp, v = pars.x$axp, col = "lightgray", lty = 3, lwd = 1.3)
                            lattice::panel.points(x, y, pch = 20, col = kolor.p, cex = dataMapOp$pointSize)
                        },
                        colorkey = FALSE)
            }

            return(ret)
        })

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
                        labels = list(labels = brks$legend.axis$labels, at = brks$legend.axis$at, cex = 1.0, col = 'black', rot = 0),
                        axis.line = list(alpha = 0.5, lty = 1, lwd = 1, col = 'black'),
                        width = 1, height = 0.8)
        colorkey.Frame <- lattice::draw.colorkey(key = colorkey, draw = FALSE, vp = NULL)

        #######
        grob.Obj <- grid::textGrob(legend.texta, x = pars.key$x, y = pars.key$y, rot = pars.key$rot,
                            just = c("center", "center"),
                            gp = grid::gpar(fontsize = 12, fontface = 'plain', col = "black", cex = 1.0))

        lezandy <- NULL
        lezandy[[place]]$fun <- grid::packGrob(frame = colorkey.Frame, grob = grob.Obj, side = pars.key$side, dynamic = TRUE)

        #######
        print(stats::update(Plot.Obj, col.regions = brks$colors, aspect = 'fill', as.table = TRUE,
                    xlim = pars.x$usr, ylim = pars.y$usr, xlab = '', ylab = '', main = titre,
                    par.settings = par.Settings, par.strip.text = par.StripText, strip = par.stripCust,
                    scales = list(x = Xaxis, y = Yaxis), legend = lezandy))
    }

    return(0)
}



