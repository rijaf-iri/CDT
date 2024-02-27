
qcPlot_Outliers.Mon <- function(){
    MOIS <- format(ISOdate(2014, 1:12, 1), "%B")
    imois <- which(MOIS == trimws(tclvalue(.cdtData$EnvData$STN$month)))
    stnid <- trimws(tclvalue(.cdtData$EnvData$STN$stnID))
    istn <- which(.cdtData$EnvData$stn.data$id == stnid)
    don <- .cdtData$EnvData$stn.data$data[, istn]
    daty <- .cdtData$EnvData$stn.data$dates

    mo <- which(as.numeric(substr(daty, 5, 6)) == imois)
    don <- don[mo]
    daty <- daty[mo]

    ina <- which(!is.na(don))
    if(length(ina) > 0){
        don <- don[ina[1]:ina[length(ina)]]
        daty <- daty[ina[1]:ina[length(ina)]]
        xlim <- c(0, length(daty) + 1)
        ylim <- range(pretty(don))
        plotOK <- TRUE
    }else{
        daty <- "2000011"
        don <- NA
        xlim <- c(0, 2)
        ylim <- c(0, 1)
        plotOK <- FALSE
    }

    if(plotOK){
        outlier <- .cdtData$EnvData$outqc$res[[stnid]]$outliers
        iout <- as.character(outlier$OUT.TEMPORAL)
        iout <- as.character(outlier$DATE)[!is.na(iout) & iout != ""]
        if(length(iout) > 0){
            iout <- match(iout, daty)
            iout <- iout[!is.na(iout)]
            if(length(iout)) vout <- don[iout]
        }

        idx.year <- split(seq_along(daty), substr(daty, 1, 4))
        at.labx <- sapply(idx.year, mean)
        labx <- as.numeric(names(idx.year))
        if(length(idx.year) > 1){
            at.tickx <- c(1, sapply(idx.year[-1], '[[', 1) + 0.5, length(daty))
        }else{
            at.tickx <- 1
        }
    }else{
        iout <- integer(0)
        at.tickx <- 1
        at.labx <- 1
        labx <- 1
    }

    opar <- graphics::par(mar = c(3.5, 4, 3, 1.5))
    plot(1, xlim = xlim, ylim = ylim, type = 'n', xaxt = 'n', yaxt = 'n', xlab = '', ylab = '')

    minTck <- graphics::axTicks(2)
    minTck <- minTck[-length(minTck)] + diff(minTck) / 2
    minTck <- c(min(graphics::axTicks(2)) - diff(minTck)[1] / 2, minTck, max(graphics::axTicks(2)) + diff(minTck)[1] / 2)
    graphics::abline(h = graphics::axTicks(2), col = "lightgray", lty = "solid", lwd = 1.0)
    graphics::abline(h = minTck, col = "lightgray", lty = "dotted", lwd = 1.3)
    graphics::abline(v = at.tickx, col = "lightgray", lty = "dotted", lwd = 1.3)

    graphics::axis(side = 2, at = graphics::axTicks(2))
    graphics::axis(side = 1, at = at.tickx, labels = FALSE)
    graphics::axis(side = 1, at = at.labx , tick = FALSE, labels = labx)

    graphics::lines(don, type = "h", lwd = 1, lend = "butt")
    graphics::mtext(.cdtData$EnvData$tab$ylabMon, side = 2, line = 2.5, cex = 1.2)
    graphics::title(main = paste(stnid, "-", MOIS[imois]))

    if(length(iout) > 0){
        graphics::lines(iout, vout, type = "h", lwd = 2, col = 2, lend = "butt")
        graphics::points(iout, vout, col = 2, pch = 6, cex = 1)
    }

    plt <- graphics::par("plt")
    usr <- graphics::par("usr")
    graphics::par(opar)

    return(list(par = c(plt, usr), dates = daty))
}

#################################################################

qcPlot_Mixed.Temp <- function(){
    stnid <- trimws(tclvalue(.cdtData$EnvData$MXD$STN$stnID))
    istn <- which(.cdtData$EnvData$stn.data$id == stnid)
    don <- .cdtData$EnvData$stn.data$data[, istn]
    daty <- .cdtData$EnvData$stn.data$dates
    intstep <- .cdtData$EnvData$output$params$intstep
    daty <- format.plot.date(daty, intstep)

    index <- .cdtData$EnvData$outqc$mixed$res[[stnid]]$index[, 1:2, drop = FALSE]
    moy <- sapply(seq(nrow(index)), function(j) mean(don[index[j, 1]:index[j, 2]], na.rm = TRUE))

    xlim <- range(daty)
    ylim <- range(pretty(don))

    opar <- graphics::par(mar = c(3.5, 4, 3, 1.5))
    plot(daty, don, type = 'n', xlab = '', ylab = '', axes = FALSE, xlim = xlim, ylim = ylim)

    minTck <- graphics::axTicks(2)
    minTck <- minTck[-length(minTck)] + diff(minTck) / 2
    minTck <- c(min(graphics::axTicks(2)) - diff(minTck)[1] / 2, minTck, max(graphics::axTicks(2)) + diff(minTck)[1] / 2)

    xTck <- axTicks.Date(daty, 1)
    xminor <- axTicks.minor.Date(c(xTck[1], xlim[2]))
    if(!is.null(xminor)) xminor <- xminor[!xminor %in% xTck]
    bar.width <- as.numeric(diff(range(xlim))) / min(as.numeric(diff(daty)), na.rm = TRUE)

    graphics::abline(h = graphics::axTicks(2), col = "lightgray", lty = "solid", lwd = 1)
    graphics::abline(h = minTck, col = "lightgray", lty = "dotted", lwd = 1.3)
    graphics::abline(v = xTck, col = "lightgray", lty = "solid", lwd = 1)
    graphics::abline(v = xminor, col = "lightgray", lty = "dotted", lwd = 1.3)

    bar.width <- 80 * bar.width^(-0.508775)
    if(bar.width < 1) bar.width <- 1
    graphics::lines(daty, don, type = "h", lwd = bar.width, lend = "butt", col = "darkblue")

    for(j in seq(nrow(index))){
        graphics::segments(daty[index[j, 1]], moy[j], daty[index[j, 2]], moy[j], col = "red", lwd = 2)
        graphics::points(daty[index[j, 1]], moy[j], col = "red")
        graphics::points(daty[index[j, 2]], moy[j], col = "red")
    }

    graphics::axis.Date(1, at = xTck, font = 1)
    if(length(xminor) > 0)
        graphics::axis.Date(1, at = xminor, labels = NA, tcl = graphics::par("tcl") * 0.5)

    graphics::axis(2, at = graphics::axTicks(2), font = 1, las = 1)
    graphics::axis(2, at = minTck, labels = NA, tcl = graphics::par("tcl") * 0.5)

    graphics::mtext("Precipitation [mm]", side = 2, line = 2.5, cex = 1.2)
    graphics::title(main = stnid)

    graphics::box(bty = 'l', col = 'black')
    graphics::box(bty = '7', col = 'black')

    plt <- graphics::par("plt")
    usr <- graphics::par("usr")
    graphics::par(opar)

    return(list(par = c(plt, usr), dates = daty))
}

#################################################################

qcPlot_Spatial.Check <- function(){
    ptsOp <- .cdtData$EnvData$STN$Opt

    #######
    stnid <- trimws(tclvalue(.cdtData$EnvData$STN$stnID))
    daty <- trimws(tclvalue(.cdtData$EnvData$STN$dateSP))
    idaty <- which(.cdtData$EnvData$stn.data$dates == daty)

    don <- data.frame(id = .cdtData$EnvData$stn.data$id,
                      x = .cdtData$EnvData$stn.data$lon,
                      y = .cdtData$EnvData$stn.data$lat,
                      z = .cdtData$EnvData$stn.data$data[idaty, ])

    #######
    selvois <- .cdtData$EnvData$outqc$res[[stnid]]$stn
    allvois <- .cdtData$EnvData$outqc$res[[stnid]]$vois

    ix <- sapply(.cdtData$EnvData$outqc$spatial.vois, '[[', 'id') == stnid
    usevois <- NULL
    if(any(ix)){ 
        useidx <- .cdtData$EnvData$outqc$spatial.vois[[which(ix)]]
        it <- which(useidx$it == daty)
        usevois <- if(length(it)) useidx$is[[it]] else it
        # if(length(it) > 0){
        #     usevois <- useidx$is[[it]]
        # }
    }

    #######

    ix0 <- which(don$id == stnid)
    stn_target <- as.list(don[ix0, , drop = FALSE])

    ix1 <- usevois[which(!usevois %in% ix0)]
    stn_usevois <- NULL
    if(length(ix1) > 0){
        tmp <- don[ix1, , drop = FALSE]
        tmp <- tmp[!is.na(tmp$z), , drop = FALSE]
        if(nrow(tmp) > 0) stn_usevois <- as.list(tmp)
    }

    ix2 <- selvois[which(!selvois %in% c(ix0, ix1))]
    stn_selvois <- NULL
    if(length(ix2) > 0){
        tmp <- don[ix2, , drop = FALSE]
        tmp <- tmp[!is.na(tmp$z), , drop = FALSE]
        if(nrow(tmp) > 0) stn_selvois <- as.list(tmp)
    }

    ix3 <- allvois[which(!allvois %in% c(ix0, ix1, ix2))]
    stn_allvois <- NULL
    if(length(ix3) > 0){
        tmp <- don[ix3, , drop = FALSE]
        tmp <- tmp[!is.na(tmp$z), , drop = FALSE]
        if(nrow(tmp) > 0) stn_allvois <- as.list(tmp)
    }

    ix4 <- c(ix0, ix1, ix2, ix3)
    tmp <- don[-ix4, , drop = FALSE]
    tmp <- tmp[!is.na(tmp$z), , drop = FALSE]
    stn_all <- NULL
    if(nrow(tmp) > 0) stn_all <- as.list(tmp)

    isp <- lapply(.cdtData$EnvData$outqc$res, '[[', 'date')
    isp <- sapply(isp, function(x) daty %in% x)
    id_outliers <- NULL
    if(any(isp)){
        tmp <- names(which(isp))
        tmp <- tmp[tmp != stnid]
        if(length(tmp) > 0) id_outliers <- tmp
    }

    #######
    if(ptsOp$circle$draw){
        dst <- .cdtData$EnvData$output$params$params$voisin$dist
        radius <- km2deg(dst, stn_target$y)
        theta <- seq(0, 2 * pi, length = 200)

        xc <- stn_target$x + radius * cos(theta)
        yc <- stn_target$y + radius * sin(theta)
    }

    #######

    don_dem <- get_demLayer_data()
    Opts_dem <- .cdtData$EnvData$dem$Opt

    don_grd <- get_gridDataLayer_data(daty)
    Opts_grd <- .cdtData$EnvData$grd_data$Opt

    ## Gridded data at top
    if(!is.null(don_dem) & !is.null(don_grd)){
        plot.grid <- TRUE
        pars <- do.call(cdt.plotmap.args0, c(list(don = don_grd), Opts_grd))
        mar <- pars$mar
    }else if(is.null(don_dem) & !is.null(don_grd)){
        plot.grid <- TRUE
        pars <- do.call(cdt.plotmap.args0, c(list(don = don_grd), Opts_grd))
        mar <- pars$mar
    }else if(!is.null(don_dem) & is.null(don_grd)){
        plot.grid <- TRUE
        pars <- do.call(cdt.plotmap.args0, c(list(don = don_dem), Opts_dem))
        mar <- pars$mar
    }else{
        mar <- c(4, 4, 2, 2)
        plot.grid <- FALSE
    }

    #######
    shpf <- .cdtData$EnvData$shapefile
    ocrds <- if(tclvalue(shpf$addshp) == "1" & !is.null(shpf$ocrds)) shpf$ocrds else matrix(NA, 1, 2)

    #######
    xyzoom <- get_zoom_xylimits()
    if(is.null(xyzoom)) return(NULL)

    #######
    opar <- graphics::par(mar = mar)
    plot(1, xlim = xyzoom$xlim, ylim = xyzoom$ylim, xlab = "", ylab = "", type = "n", xaxt = 'n', yaxt = 'n')
    axlabs <- LatLonAxisLabels(graphics::axTicks(1), graphics::axTicks(2))
    graphics::axis(side = 1, at = graphics::axTicks(1), labels = axlabs$xaxl, tck = -0.01, cex.axis = 1.0)
    graphics::axis(side = 2, at = graphics::axTicks(2), labels = axlabs$yaxl, tck = -0.01, las = 1, cex.axis = 1.0)

    if(plot.grid){
        plot.type <- trimws(tclvalue(.cdtData$EnvData$plot.maps$plot.type))
        if(plot.type == "FilledContour")
            graphics::.filled.contour(pars$don$x, pars$don$y, pars$don$z, levels = pars$breaks, col = pars$kolor)
        if(plot.type == "Pixels")
            graphics::image(pars$don, breaks = pars$breaks, col = pars$kolor, xaxt = 'n', yaxt = 'n', add = TRUE)

        fields::image.plot(zlim = pars$zlim, breaks = pars$breaks2, col = pars$kolor, horizontal = pars$horizontal,
                           legend.only = TRUE, legend.mar = pars$legend.mar, legend.width = pars$legend.width,
                           legend.args = NULL, axis.args = list(at = pars$breaks1, labels = pars$labels,
                           cex.axis = 0.7, font = 2, tcl = -0.3, mgp = c(0, 0.5, 0)), legend.shrink = 0.8)
    }

    graphics::abline(h = graphics::axTicks(2), v = graphics::axTicks(1) , col = "lightgray", lty = 3, lwd = 1.3)

    graphics::lines(ocrds[, 1], ocrds[, 2], lwd = shpf$options$lwd, col = shpf$options$col)

    if(ptsOp$circle$draw) graphics::lines(xc, yc, lwd = ptsOp$circle$lwd, col = ptsOp$circle$col)

    #######

    if(!is.null(stn_all))
        graphics::points(stn_all$x, stn_all$y, col = ptsOp$all$col, pch = ptsOp$all$pch, cex = ptsOp$all$cex)
    if(!is.null(stn_allvois))
        graphics::points(stn_allvois$x, stn_allvois$y, col = ptsOp$vois$col, pch = ptsOp$vois$pch, cex = ptsOp$vois$cex)
    if(!is.null(stn_selvois))
        graphics::points(stn_selvois$x, stn_selvois$y, col = ptsOp$sel$col, pch = ptsOp$sel$pch, cex = ptsOp$sel$cex)
    if(!is.null(stn_usevois))
        graphics::points(stn_usevois$x, stn_usevois$y, col = ptsOp$use$col, pch = ptsOp$use$pch, cex = ptsOp$use$cex)
    graphics::points(stn_target$x, stn_target$y, col = ptsOp$stn$col, bg = ptsOp$stn$txt.col, pch = ptsOp$stn$pch, cex = ptsOp$stn$cex)

    if(!is.null(stn_all)){
        plot_txt <- TRUE
        if(!is.null(id_outliers)){
            ip <- stn_all$id %in% id_outliers
            if(any(ip)){
                tmp1 <- lapply(stn_all, function(x) x[ip])
                graphics::text(tmp1$x, tmp1$y, labels = tmp1$z, pos = 1, cex = ptsOp$all$txt.cex, col = ptsOp$stn$txt.col)
                tmp2 <- lapply(stn_all, function(x) x[!ip])
                graphics::text(tmp2$x, tmp2$y, labels = tmp2$z, pos = 1, cex = ptsOp$all$txt.cex, col = ptsOp$all$txt.col)
                plot_txt <- FALSE
            }
        }
        if(plot_txt){
            graphics::text(stn_all$x, stn_all$y, labels = stn_all$z, pos = 1, cex = ptsOp$all$txt.cex, col = ptsOp$all$txt.col)
        }
    }

    if(!is.null(stn_allvois)){
        plot_txt <- TRUE
        if(!is.null(id_outliers)){
            ip <- stn_allvois$id %in% id_outliers
            if(any(ip)){
                tmp1 <- lapply(stn_allvois, function(x) x[ip])
                graphics::text(tmp1$x, tmp1$y, labels = tmp1$z, pos = 1, cex = ptsOp$vois$txt.cex, col = ptsOp$stn$txt.col)
                tmp2 <- lapply(stn_allvois, function(x) x[!ip])
                graphics::text(tmp2$x, tmp2$y, labels = tmp2$z, pos = 1, cex = ptsOp$vois$txt.cex, col = ptsOp$vois$txt.col)
                plot_txt <- FALSE
            }
        }
        if(plot_txt){
            graphics::text(stn_allvois$x, stn_allvois$y, labels = stn_allvois$z, pos = 1, cex = ptsOp$vois$txt.cex, col = ptsOp$vois$txt.col)
        }
    }

    if(!is.null(stn_selvois)){
        plot_txt <- TRUE
        if(!is.null(id_outliers)){
            ip <- stn_selvois$id %in% id_outliers
            if(any(ip)){
                tmp1 <- lapply(stn_selvois, function(x) x[ip])
                graphics::text(tmp1$x, tmp1$y, labels = tmp1$z, pos = 1, cex = ptsOp$sel$txt.cex, col = ptsOp$stn$txt.col)
                tmp2 <- lapply(stn_selvois, function(x) x[!ip])
                graphics::text(tmp2$x, tmp2$y, labels = tmp2$z, pos = 1, cex = ptsOp$sel$txt.cex, col = ptsOp$sel$txt.col)
                plot_txt <- FALSE
            }
        }
        if(plot_txt){
            graphics::text(stn_selvois$x, stn_selvois$y, labels = stn_selvois$z, pos = 1, cex = ptsOp$sel$txt.cex, col = ptsOp$sel$txt.col)
        }
    }

    if(!is.null(stn_usevois)){
        plot_txt <- TRUE
        if(!is.null(id_outliers)){
            ip <- stn_usevois$id %in% id_outliers
            if(any(ip)){
                tmp1 <- lapply(stn_usevois, function(x) x[ip])
                graphics::text(tmp1$x, tmp1$y, labels = tmp1$z, pos = 1, cex = ptsOp$use$txt.cex, col = ptsOp$stn$txt.col)
                tmp2 <- lapply(stn_usevois, function(x) x[!ip])
                graphics::text(tmp2$x, tmp2$y, labels = tmp2$z, pos = 1, cex = ptsOp$use$txt.cex, col = ptsOp$use$txt.col)
                plot_txt <- FALSE
            }
        }
        if(plot_txt){
            graphics::text(stn_usevois$x, stn_usevois$y, labels = stn_usevois$z, pos = 1, cex = ptsOp$use$txt.cex, col = ptsOp$use$txt.col)
        }
    }

    graphics::text(stn_target$x, stn_target$y, labels = stn_target$z, pos = 1, cex = ptsOp$stn$txt.cex, col = ptsOp$stn$txt.col)

    #######

    graphics::title(main = paste('STN:', stnid, '- Date:', daty), cex.main = 1, font.main = 1)
    graphics::box()

    plt <- graphics::par("plt")
    usr <- graphics::par("usr")
    graphics::par(opar)

    return(list(par = c(plt, usr)))
}

######################################################################################################

qcDislpay_Outliers.Mon <- function(notebookTab, tab.title){
    varplot <- c("parPlotSize1", "parPlotSize2", "parPlotSize3", "parPlotSize4",
                 "usrCoords1", "usrCoords2", "usrCoords3", "usrCoords4")
    parPltCrd <- stats::setNames(lapply(varplot, function(x) assign(x, tclVar(), envir = parent.frame())), varplot)

    daty <- NULL
    plotIt <- function(){
        op <- graphics::par(bg = "white")
        pltusr <- qcPlot_Outliers.Mon()
        graphics::par(op)
        for(j in seq_along(varplot)) tclvalue(parPltCrd[[varplot[j]]]) <- pltusr$par[j]
        daty <<- pltusr$dates
        return(0)
    }

    #########
    onglet <- imageNotebookTab_open(notebookTab, tab.title)
    hscale <- as.numeric(tclvalue(tkget(.cdtEnv$tcl$toolbar$spinH)))
    vscale <- as.numeric(tclvalue(tkget(.cdtEnv$tcl$toolbar$spinV)))

    img <- DisplayPlot(onglet[[2]], fun = plotIt, hscale = hscale, vscale = vscale)
    tkgrid(img)
    tkgrid.rowconfigure(img, 0, weight = 1)
    tkgrid.columnconfigure(img, 0, weight = 1)
    tcl("update")

    #########
    intstep <- .cdtData$EnvData$output$params$intstep

    #########
    tkbind(img, "<Motion>", function(W, x, y){
        xyMouse <- mouseMouvment(W, x, y, parPltCrd)

        ipos <- round(xyMouse$x)
        datyRange <- ipos < 1 | ipos > length(daty) | xyMouse$inout

        frxcoord <- if(datyRange) '' else format.plot.date.label(daty[ipos], intstep)
        frycoord <- if(xyMouse$inout) '' else round(xyMouse$y, 1)

        tclvalue(.cdtEnv$tcl$status$xcrd) <- frxcoord
        tclvalue(.cdtEnv$tcl$status$ycrd) <- frycoord
    })

    tkbind(img, "<Enter>", function() tkconfigure(img, cursor = 'crosshair'))
    tkbind(img, "<Leave>", function() tkconfigure(img, cursor = ''))

    return(list(onglet, img))
}

#################################################################
