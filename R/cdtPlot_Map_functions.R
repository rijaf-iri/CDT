
cdt.plotmap.args0 <- function(don,
                            user.levels = list(custom = FALSE, levels = NULL, equidist = TRUE),
                            user.colors = list(custom = FALSE, color = grDevices::rainbow(20)),
                            preset.colors = list(color = "rainbow", reverse = FALSE)
                        )
{
    brks <- image.plot_Legend_pars(don$z, user.levels, user.colors, preset.colors)
    don$z <- don$z + 1e-15
    keyColLab <- round(brks$legend.axis$labels, 6)
    pars0 <- list(
                    don = don,
                    zlim = brks$legend.breaks$zlim,
                    breaks = brks$breaks,
                    breaks1 = brks$legend.axis$at,
                    breaks2 = brks$legend.breaks$breaks,
                    labels = keyColLab,
                    kolor = brks$colors
                )

    is.horiz <- diff(range(don$x)) > diff(range(don$y))
    if(is.horiz){
        pars <- list(
                    horizontal = TRUE,
                    legend.mar = 3.5,
                    legend.width = 0.7,
                    mar = c(7, 4, 2.5, 2.5)
                )
    }else{
        pars <- list(
                    horizontal = FALSE,
                    legend.mar = 7.2,
                    legend.width = 0.9,
                    mar = c(4, 4, 2.5, 7)
                )
    }

    pars <- c(pars0, pars)
    return(pars)
}

cdt.plotmap.args <- function(don, MapOp, shpf, 
                        mar.h = c(7, 4, 2.5, 2.5), mar.v = c(4, 4, 2.5, 7),
                        legend.text = NULL, label.fun = identity, ...)
{
    ## colorscale title
    if(MapOp$colkeyLab$user){
        legend.texta <- MapOp$colkeyLab$label
        if(trimws(legend.texta) == "") legend.texta <- NULL
    }else legend.texta <- legend.text

    #################
    ## breaks
    brks <- image.plot_Legend_pars(don$z, MapOp$userLvl, MapOp$userCol, MapOp$presetCol)
    don$z <- don$z + 1e-15
    breaks <- brks$breaks
    zlim <- brks$legend.breaks$zlim
    breaks2 <- brks$legend.breaks$breaks
    kolor <- brks$colors
    breaks1 <- brks$legend.axis$at
    lab.breaks <- round(brks$legend.axis$labels, 6)

    ## legend label
    legendLabel <- label.fun(lab.breaks, ...)

    #################
    ### shape files
    ocrds <- if(tclvalue(shpf$add.shp) == "1" & !is.null(shpf$ocrds)) shpf$ocrds else matrix(NA, 1, 2)

    #################

    if(all(is.na(ocrds[, 1])) | all(is.na(ocrds[, 2]))){
        xlim <- range(don$x, na.rm = TRUE)
        ylim <- range(don$y, na.rm = TRUE)
    }else{
        xlim <- range(range(don$x, na.rm = TRUE), range(ocrds[, 1], na.rm = TRUE))
        ylim <- range(range(don$y, na.rm = TRUE), range(ocrds[, 2], na.rm = TRUE))
    }

    #################

    if(diff(xlim) > diff(ylim)){
        horizontal <- TRUE
        legend.mar <- 3.5
        legend.width <- 0.9
        mar <- mar.h
        legend.args <- if(!is.null(legend.texta)) list(text = legend.texta, cex = 1.0, side = 1, line = 2) else NULL
    }else{
        horizontal <- FALSE
        legend.mar <- 7.2
        mar <- mar.v
        legend.width <- 0.9
        line <- if(max(nchar(as.character(legendLabel))) > 4) 4 else 3
        legend.args <- if(!is.null(legend.texta)) list(text = legend.texta, cex = 1.0, side = 4, line = line) else NULL
    }

    list(don = don, horizontal = horizontal, kolor = kolor,
        mar = mar, xlim = xlim, ylim = ylim, zlim = zlim, ocrds = ocrds,
        breaks = breaks, breaks1 = breaks1, breaks2 = breaks2,
        legend.mar = legend.mar, legend.width = legend.width,
        legend.args = legend.args, legendLabel = legendLabel)
}

cdt.plotmap.args.ncvar <- function(don, mapops, PlotType, shpf, SHPOp,
                                mar.h = c(5.5, 4, 2.5, 1), mar.v = c(3.5, 4, 2.5, 6),
                                legend.text = NULL, label.fun = identity, ...)
{
    if(!mapops$title$user){
        .titre <- don$title
    }else .titre <- mapops$title$title

    map.args <- cdt.plotmap.args(don, mapops, shpf, mar.h, mar.v, legend.text, label.fun, ...)
    mar <- map.args$mar
    map.args.add <- list(titre = .titre,
                        SHPOp = SHPOp,
                        data.type = "Grid",
                        plot.type = PlotType)
    map.args <- map.args[!(names(map.args) %in% "mar")]
    map.args <- c(map.args, map.args.add)
    list(mar = mar, map.args = map.args)
}

####################################################################################################

# data.type = c("Points", "Grid")
# data.type = "Points"; plot.type = c("Pixels", "Points")
# data.type = "Grid"; plot.type = c("Pixels", "FilledContour")

cdt.plotmap.fun <- function(don, horizontal, kolor,
                            xlim, ylim, zlim,
                            breaks, breaks1, breaks2,
                            legend.mar, legend.width,
                            legend.args, legendLabel,
                            titre, ocrds, SHPOp, MapOp = NULL,
                            data.type = "Points", plot.type = "Pixels")
{
    plot(1, xlim = xlim, ylim = ylim, xlab = "", ylab = "", type = "n", xaxt = 'n', yaxt = 'n')
    axlabs <- LatLonAxisLabels(graphics::axTicks(1), graphics::axTicks(2))
    graphics::axis(side = 1, at = graphics::axTicks(1), labels = axlabs$xaxl, tcl = -0.2, cex.axis = 1.0)
    graphics::axis(side = 2, at = graphics::axTicks(2), labels = axlabs$yaxl, tcl = -0.2, las = 1, cex.axis = 1.0)
    graphics::title(main = titre, cex.main = 1.3, font.main = 2)

    if(plot.type %in% c("Pixels", "FilledContour")){
        if(plot.type == "Pixels")
            graphics::image(don, breaks = breaks, col = kolor, xaxt = 'n', yaxt = 'n', add = TRUE)

        if(data.type == "Grid" & plot.type == "FilledContour")
            graphics::.filled.contour(don$x, don$y, don$z, levels = breaks, col = kolor)

        graphics::abline(h = graphics::axTicks(2), v = graphics::axTicks(1), col = "lightgray", lty = 3, lwd = 1.3)
        graphics::lines(ocrds[, 1], ocrds[, 2], lwd = SHPOp$lwd, col = SHPOp$col)
    }else{
        if(data.type == "Points" & plot.type == "Points"){
            graphics::abline(h = graphics::axTicks(2), v = graphics::axTicks(1), col = "lightgray", lty = 3, lwd = 1.3)
            graphics::lines(ocrds[, 1], ocrds[, 2], lwd = SHPOp$lwd, col = SHPOp$col)

            kolor.p <- kolor[findInterval(don$z, breaks, rightmost.closed = TRUE, left.open = TRUE)]
            graphics::points(don$x, don$y, col = kolor.p, cex = MapOp$pointSize, pch = 20)
        }
    }

    fields::image.plot(zlim = zlim, breaks = breaks2, col = kolor, horizontal = horizontal,
                       legend.only = TRUE, legend.mar = legend.mar, legend.width = legend.width,
                       legend.args = legend.args, axis.args = list(at = breaks1, labels = legendLabel,
                       cex.axis = 1.0, font = 2, tcl = -0.3, mgp = c(0, 0.5, 0)), legend.shrink = 0.8)

    plt <- graphics::par("plt")
    usr <- graphics::par("usr")
    return(list(par = c(plt, usr)))
}

####################################################################################################

cdt.plotmap.scalebar <- function(scalebar)
{
    if(scalebar$add){
        if(scalebar$pos == 'bottomleft') posx <- 0.05
        if(scalebar$pos == 'bottomcenter') posx <- 0.425
        if(scalebar$pos == 'bottomright') posx <- 0.75
        posy <- 0.08

        scalebarX <- graphics::grconvertX(posx, "npc")
        scalebarY <- graphics::grconvertY(posy, "npc")

        maps::map.scale(x = scalebarX, y = scalebarY, relwidth = 0.15, metric = TRUE, ratio = FALSE, cex = 0.7, font = 2)
    }
}

