
MultipleData.Plot.Map <- function(){
    MapOp <- .cdtData$EnvData$dataMapOp
    data.Obj <- .cdtData$EnvData$data.Obj

    SHPOp <- .cdtData$EnvData$SHPOp
    ocrds <- .cdtData$EnvData$shp$ocrds
    ocrds <- if(tclvalue(.cdtData$EnvData$shp$add.shp) == "1" & !is.null(ocrds)) ocrds else matrix(NA, 1, 2)

    #######
    titre <- if(MapOp$title$user) MapOp$title$title else .cdtData$GalParams$donnees$date2plot
    colorkey.Title <- if(MapOp$colkeyLab$user) MapOp$colkeyLab$label else ""

    #######
    ## range ocrds
    pars.x <- parAxisPlotFun(.cdtData$EnvData$data.range[, 1])
    pars.y <- parAxisPlotFun(.cdtData$EnvData$data.range[, 2])
    brks <- image.plot_Legend_pars(.cdtData$EnvData$data.range[, 3], MapOp$userLvl, MapOp$userCol, MapOp$presetCol)

    #######
    xylabs <- LatLonAxisLabels(pars.x$axp, pars.y$axp)
    Xaxis <- list(relation = "same", draw = TRUE, at = pars.x$axp, labels = xylabs$xaxl, cex = 1.0, alternating = c(1, 2), tck = c(1, 1))
    Yaxis <- list(relation = "same", draw = TRUE, at = pars.y$axp, labels = xylabs$yaxl, cex = 1.0, alternating = c(1, 2), tck = c(1, 1))

    #######
    nb.plot <- length(data.Obj)
    layout.Obj <- manageLayout(nb.plot)
    place <- if(diff(pars.x$usr) * layout.Obj$dim[1] >= diff(pars.y$usr) * layout.Obj$dim[2]) 'bottom' else 'right'
    panel.Title <- sapply(data.Obj, "[[", "title")

    #######

    Plot.Obj <- lapply(data.Obj, function(obj){
        if(obj$map.type == "Grid"){
            if(obj$plot.type == "Pixels") plot.type <- "grid"
            if(obj$plot.type == "Raster") plot.type <- "raster"
        }
        if(obj$map.type == "Points"){
            if(obj$plot.type == "Pixels") plot.type <- "grid"
            if(obj$plot.type == "Points") plot.type <- "point"
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

        if(plot.type == "raster"){
            ret <- lattice::levelplot(z.val, row.values = obj$x, column.values = obj$y, at = brks$breaks,
                    prepanel = lattice::prepanel.default.levelplot,
                    interpolate = TRUE, region = TRUE,
                    panel = function(...){
                        lattice::panel.levelplot.raster(...)
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
                        lattice::panel.points(x, y, pch = 20, col = kolor.p, cex = obj$point.size)
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
    grob.Obj <- grid::textGrob(colorkey.Title, x = pars.key$x, y = pars.key$y, rot = pars.key$rot,
                        just = c("center", "center"),
                        gp = grid::gpar(fontsize = 12, fontface = 'plain', col = "black", cex = 1.0))

    lezandy <- NULL
    lezandy[[place]]$fun <- grid::packGrob(frame = colorkey.Frame, grob = grob.Obj, side = pars.key$side, dynamic = TRUE)

    #######
    print(stats::update(Plot.Obj, col.regions = brks$colors, aspect = 'fill', as.table = TRUE,
                xlim = pars.x$usr, ylim = pars.y$usr, xlab = '', ylab = '', main = titre,
                par.settings = par.Settings, par.strip.text = par.StripText, strip = par.stripCust,
                scales = list(x = Xaxis, y = Yaxis), legend = lezandy))
    return(0)
}
