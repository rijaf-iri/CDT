#' Display maps from multiple data in one plot.
#'
#' Display maps from multiple data in one plot.
#' 
#' @param dataObj list of data. Each data list(x, y, z, type). \cr
#' Type: "grid" or "points".\cr
#' "grid": x vector, y vector, z matrix of dim c(length(x), length(y))
#' "points": x, y, z vectors same length
#' @param shp sp object of class \code{SpatialPolygons} or \code{SpatialPolygonsDataFrame}
#' @param xlim,ylim vector of length 2.
#' @param dataTitle vector same length as dataObj, title for each data
#' @param plotTitle character, title of the plot
#' @param colorkeyTitle character, title of the colorkey
#' @param presetCol list, preset colors for colorkey
#' @param userCol list, customized colors for colorkey
#' @param userLvl list, customized level for colorkey
#' @param ptsOpt list, options for points data
#' @param shpOpt list, options for the shp layer
#' 
#' @export

plotMultipleData <- function(dataObj, shp = NULL, xlim = NULL, ylim = NULL,
                             dataTitle = NULL, plotTitle = NULL, colorkeyTitle = "",
                             presetCol = list(color = 'tim.colors', reverse = FALSE),
                             userCol = list(custom = FALSE, color = NULL),
                             userLvl = list(custom = FALSE, levels = NULL, equidist = FALSE),
                             ptsOpt = list(pch = 20, cex = 1),
                             shpOpt = list(col = "black", lwd = 1)
                            )
{
    if(!is.null(shp)){
        ocrds <- getBoundaries(shp)
    }else{
        ocrds <- matrix(NA, ncol = 2)
    }

    #######

    data.range <- lapply(dataObj, function(x){
        sapply(x[c('x', 'y', 'z')], range, na.rm = TRUE)
    })
    data.range <- do.call(rbind, data.range)

    #######

    if(is.null(xlim))
        xlim <- range(data.range[, 1])
    if(is.null(ylim))
        ylim <- range(data.range[, 2])

    pars.x <- parAxisPlotFun(xlim)
    pars.y <- parAxisPlotFun(ylim)
    brks <- image.plot_Legend_pars(data.range[, 3], userLvl, userCol, presetCol)
    ##
    brks$legend.axis$labels <- round(brks$legend.axis$labels, 6)

    #######
    xylabs <- LatLonAxisLabels(pars.x$axp, pars.y$axp)
    Xaxis <- list(relation = "same", draw = TRUE, at = pars.x$axp, labels = xylabs$xaxl, cex = 1.0, alternating = c(1, 2), tck = c(1, 1))
    Yaxis <- list(relation = "same", draw = TRUE, at = pars.y$axp, labels = xylabs$yaxl, cex = 1.0, alternating = c(1, 2), tck = c(1, 1))

    #######

    nb.plot <- length(dataObj)
    layout.Obj <- manageLayout(nb.plot)
    place <- if(diff(pars.x$usr) * layout.Obj$dim[1] >= diff(pars.y$usr) * layout.Obj$dim[2]) 'bottom' else 'right'

    if(!is.null(dataTitle)){
        panel.Title <- dataTitle
        if(length(dataTitle) < nb.plot){
            title1 <- paste("Data", seq(nb.plot - length(dataTitle)))
            panel.Title <- c(panel.Title, title1)
        }
    }else{
        panel.Title <- paste("Data", seq(nb.plot))
    }

    #######

    Plot.Obj <- lapply(dataObj, function(obj){
        z.val <- obj$z + 1e-15
        ptype <- substr(toupper(obj$type), 1, 1)

        if(ptype == "G"){
            ret <- lattice::levelplot(z.val, row.values = obj$x, column.values = obj$y, at = brks$breaks,
                    prepanel = lattice::prepanel.default.levelplot,
                    panel = function(...){
                        lattice::panel.levelplot(...)
                        lattice::panel.lines(ocrds, col = shpOpt$col, lwd = shpOpt$lwd)
                        lattice::panel.abline(h = pars.y$axp, v = pars.x$axp, col = "lightgray", lty = 3)
                    },
                    colorkey = FALSE)
        }

        if(ptype == "P"){
            kolor.p <- brks$colors[findInterval(z.val, brks$breaks, rightmost.closed = TRUE, left.open = TRUE)]
            ret <- lattice::levelplot(z.val ~ obj$x + obj$y, at = brks$breaks,
                    prepanel = lattice::prepanel.default.xyplot,
                    panel = function(x, y, ...){
                        lattice::panel.lines(ocrds, col = shpOpt$col, lwd = shpOpt$lwd)
                        lattice::panel.abline(h = pars.y$axp, v = pars.x$axp, col = "lightgray", lty = 3, lwd = 1.3)
                        lattice::panel.points(x, y, pch = ptsOpt$pch, col = kolor.p, cex = ptsOpt$cex)
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
    grob.Obj <- grid::textGrob(colorkeyTitle, x = pars.key$x, y = pars.key$y, rot = pars.key$rot,
                        just = c("center", "center"),
                        gp = grid::gpar(fontsize = 12, fontface = 'plain', col = "black", cex = 1.0))

    lezandy <- NULL
    lezandy[[place]]$fun <- grid::packGrob(frame = colorkey.Frame, grob = grob.Obj, side = pars.key$side, dynamic = TRUE)

    #######
    
    print(update(Plot.Obj, col.regions = brks$colors, aspect = 'fill', as.table = TRUE,
                xlim = pars.x$usr, ylim = pars.y$usr, xlab = '', ylab = '', main = plotTitle,
                par.settings = par.Settings, par.strip.text = par.StripText, strip = par.stripCust,
                scales = list(x = Xaxis, y = Yaxis), legend = lezandy))
    return(0)
}
