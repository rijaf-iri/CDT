
assessData_plotYearMonAvg <- function(){
    intstep <- .cdtData$EnvData$output$params$intstep
    mh_data <- intstep %in% c('minute', 'hourly')
    nts <- if(mh_data) '5' else '6'
    nts <- .cdtData$EnvData$plottext[[nts]]
    titre <- paste(.cdtData$EnvData$plottext[['12']], nts)
    ylab <- .cdtData$EnvData$plottext[['13']]

    yrmon <- .cdtData$EnvData$output$dates.grp
    stn <- .cdtData$EnvData$output$nb.stn.grp
    if(mh_data){
       yrmon <- as.character(yrmon)
       yrmon <- as.Date(paste0(yrmon, '15'), '%Y%m%d')
       xlim <- range(yrmon)
    }else{
        xlim <- range(yrmon)
    }
    ylim <- c(0, max(stn, na.rm = TRUE))

    #####
    opar <- graphics::par(mar = c(4.5, 4, 3, 1.5))
    plot(1, type = 'n', axes = FALSE, xlim = xlim, ylim = ylim, xlab = '', ylab = ylab, main = titre)

    minTck <- graphics::axTicks(2)
    minTck <- minTck[-length(minTck)] + diff(minTck) / 2
    minTck <- c(min(graphics::axTicks(2)) - diff(minTck)[1] / 2, minTck, max(graphics::axTicks(2)) + diff(minTck)[1] / 2)

    graphics::abline(h = graphics::axTicks(2), col = "lightgray", lty = "solid", lwd = 1.0)
    graphics::abline(h = minTck, col = "lightgray", lty = "dotted", lwd = 1.3)

    if(methods::is(yrmon, "Date")){
        xTck <- axTicks.Date(yrmon, 1)
        axis.foo <- graphics::axis.Date
        xminor <- axTicks.minor.Date(c(xTck[1], xlim[2]))
        if(!is.null(xminor)) xminor <- xminor[!xminor %in% xTck]
        bar.width <- as.numeric(diff(range(xlim))) / min(as.numeric(diff(yrmon)), na.rm = TRUE)
    }else{
        xTck <- graphics::axTicks(1)
        xTck <- xTck[sapply(xTck, function(e) min(abs(c(e%%1, e%%1 - 1))) < 1e-10)]
        axis.foo <- graphics::axis
        bar.width <- as.numeric(diff(range(xlim)))
        if(as.numeric(diff(xlim)) > 5){
            xminor <- seq(floor(xlim[1]), floor(xlim[2]), 1)
            xminor <- xminor[!xminor %in% xTck]
        }else xminor <- NULL
    }
    bar.width <- 80 * bar.width^(-0.508775)
    if(bar.width < 1) bar.width <- 1

    graphics::lines(yrmon, stn[, 1], type = "h", lwd = bar.width, lend = "butt", col = 4)
    graphics::points(yrmon, stn[, 1], pch = 20, col = 2, cex = 0.7)
    ix <- stn[, 3] - stn[, 2] > 0
    graphics::arrows(yrmon[ix], stn[ix, 2], yrmon[ix], stn[ix, 3], angle = 90, code = 3, length = 0.03, col = 2)

    if(methods::is(yrmon, "Date")){
        axis.foo(1, at = xTck, labels = FALSE)
        graphics::text(xTck, graphics::par("usr")[3], labels = format(xTck, "%b-%Y"), srt = 45, adj = c(1.2, 1.5), xpd = TRUE)
    }else{
        axis.foo(1, at = xTck)
    }
    if(length(xminor) > 0)
        axis.foo(1, at = xminor, labels = NA, tcl = graphics::par("tcl") * 0.5)

    graphics::axis(2, at = graphics::axTicks(2), las = 1)
    graphics::axis(2, at = minTck, labels = NA, tcl = graphics::par("tcl") * 0.5)

    graphics::box(bty = 'l')
    graphics::box(bty = '7', col = 'gray')
    graphics::par(opar)
}

##############################

assessData_plotDistCor <- function(){
    dstcor <- readRDS(file.path(.cdtData$EnvData$output$PathData, 'CDTDATASET', "Distance_Correlation.rds"))
    xlab <- "Distance (km)"
    ylab <- "Correlation"

    xys <- dstcor$summary
    xlim <- range(xys[, 1], na.rm = TRUE)
    ylim <- c(min(xys[, 2], na.rm = TRUE), max(xys[, 4], na.rm = TRUE))

    op <- graphics::par(mar = c(3.5, 4, 2, 2))
    plot(1, type = 'n', xlim = xlim, ylim = ylim, xlab = '', ylab = '')
    graphics::polygon(c(rev(xys[, 1]), xys[, 1]), c(rev(xys[, 4]), xys[, 2]), col = "#97FFFF", border = NA)
    graphics::lines(xys[, 1], xys[, 3], col = 'blue', lwd = 2)
    graphics::lines(dstcor$loess$x, dstcor$loess$y, lwd = 2, col = 'red')
    graphics::abline(v = graphics::axTicks(1), h = graphics::axTicks(2), col = 'lightgrey', lty = "dotted")
    graphics::mtext(xlab, side = 1, line = 2)
    graphics::mtext(ylab, side = 2, line = 2.5)

    graphics::legend(x = 'topright', legend = c("5/95th Percentile", "Median", 'Loess smooth'), bg = 'gray97',
            col = c("#97FFFF", 'blue', 'red'), lty = c(0, 1, 1), lwd = c(0, 2, 2),
            pch = c(22, NA, NA), pt.bg = c("#97FFFF", NA, NA), pt.cex = 2)
    graphics::par(op)
}

##############################

assessData_plotActivities <- function(YLayoutDiv){
    stn <- readRDS(file.path(.cdtData$EnvData$output$PathData, 'CDTDATASET', "Station_Activities.rds"))

    xlim <- range(stn$date)
    ylim <- c(0, max(pretty(stn$work)))
    ylab <- .cdtData$EnvData$plottext[['9']]
    text.legend <- c(.cdtData$EnvData$plottext[['10']], .cdtData$EnvData$plottext[['11']])
    col.avail <- 'pink'
    col.work <- 'cyan1'
    col.legend <- c(col.work, col.avail)

    graphics::layout(matrix(1:2, ncol = 1), widths = 1, heights = YLayoutDiv, respect = FALSE)
    op <- graphics::par(mar = c(0, 0, 0, 0))
    graphics::plot.new()
    graphics::legend("center", "groups", legend = text.legend, fill = col.legend, horiz = TRUE)
    graphics::par(op)
    op <- graphics::par(mar = c(3.5, 4, 0, 2))
    plot(stn$date, stn$work, type = 'n', xaxt = 'n', xlab = '', ylab = ylab, xlim = xlim, ylim = ylim)

    if(methods::is(stn$date, "Date")){
        xTck <- axTicks.Date(stn$date, 1)
        if(as.numeric(diff(xlim)) > 1095){
            xminor <- seq(as.Date(paste0(format(xlim[1], "%Y"), "-01-01")),
                        as.Date(paste0(as.numeric(format(xlim[2], "%Y")) + 1, "-01-01")), "year")
            xminor <- xminor[!xminor %in% xTck]
        }else xminor <- NULL
        graphics::axis.Date(1, at = xTck)
        if(length(xminor) > 0) graphics::axis.Date(1, at = xminor, labels = NA, tcl = graphics::par("tcl") * 0.5)
    }

    if(methods::is(stn$date, "POSIXct")){
        xTck <- axTicks.POSIXct(stn$date, 1)
        xminor <- axTicks.minor.POSIXct(c(xTck[1], xlim[2]))
        if(!is.null(xminor)) xminor <- xminor[!xminor %in% xTck]
        graphics::axis.POSIXct(1, at = xTck)
        if(length(xminor) > 0) graphics::axis.POSIXct(1, at = xminor, labels = NA, tcl = graphics::par("tcl") * 0.5)
    }

    graphics::polygon(c(rev(stn$date), stn$date), c(rev(stn$avai), rep(0, length(stn$date))), col = col.avail, border = NA)
    graphics::polygon(c(rev(stn$date), stn$date), c(rev(stn$work), stn$avai), col = col.work, border = NA)

    graphics::abline(h = graphics::axTicks(2), col = "lightgray", lty = "dotted")
    graphics::abline(v = xTck, col = "lightgray", lty = "dotted")

    graphics::box()
    plt <- graphics::par("plt")
    usr <- graphics::par("usr")
    graphics::par(op)
    return(list(par = c(plt, usr)))
}

##############################

assessData_plotAvailability <- function(){
    don <- .cdtData$EnvData$DataAvail$map
    dataMapOp <- .cdtData$EnvData$avaiMapOp

    ## titre
    if(!dataMapOp$title$user){
        dt_rg <- get.range.date.time(.cdtData$EnvData$availPeriod,
                                     .cdtData$EnvData$output$params$intstep,
                                     .cdtData$EnvData$output$params$minhour)
        period <- paste(dt_rg$start, '/', dt_rg$end)
        txt1 <- .cdtData$EnvData$plottext[['1']]
        txt2 <- .cdtData$EnvData$plottext[['2']]
        txt3 <- .cdtData$EnvData$plottext[['3']]
        .titre <- paste(paste0(txt1, " (", txt2, " %). " , txt3, ":"), period)
    }else .titre <- dataMapOp$title$title

    #################

    map.args <- cdt.plotmap.args(don, dataMapOp, .cdtData$EnvData$shapefile)
    opar <- graphics::par(mar = map.args$mar)
    map.args.add <- list(titre = .titre, data.type = "Points")
    map.args <- map.args[!(names(map.args) %in% "mar")]
    map.args <- c(map.args, map.args.add)
    par.plot <- do.call(cdt.plotmap.fun, map.args)
    ## scale bar
    cdt.plotmap.scalebar(dataMapOp$scalebar)
    graphics::par(opar)

    return(par.plot)
}

##############################

assessData_plotDataGroupMap <- function(){
    don <- .cdtData$EnvData$DataGroup$map
    dataMapOp <- .cdtData$EnvData$grpMapOp

    ## titre
    if(!dataMapOp$title$user){
        intstep <- .cdtData$EnvData$output$params$intstep
        nobs <- if(intstep %in% c('minute', 'hourly')) "5" else "6"
        nobs <- .cdtData$EnvData$plottext[[nobs]]
        nobs <- paste0("(", .cdtData$EnvData$plottext[["4"]], " ", nobs, ")")
        this.daty <- trimws(tclvalue(.cdtData$EnvData$availDate))
        .titre <- paste(.cdtData$EnvData$plottext[["7"]], ":", this.daty, nobs)
    }else .titre <- dataMapOp$title$title

    #################

    map.args <- cdt.plotmap.args(don, dataMapOp, .cdtData$EnvData$shapefile)
    opar <- graphics::par(mar = map.args$mar)
    map.args.add <- list(titre = .titre, data.type = "Points")
    map.args <- map.args[!(names(map.args) %in% "mar")]
    map.args <- c(map.args, map.args.add)
    par.plot <- do.call(cdt.plotmap.fun, map.args)
    ## scale bar
    cdt.plotmap.scalebar(dataMapOp$scalebar)
    graphics::par(opar)

    return(par.plot)
}

##############################

assessData_plotDataGroupTS <- function(){
    optsgph <- .cdtData$EnvData$TSGraphOp
    IDSTN <- .cdtData$EnvData$output$data$id
    ixy <- which(IDSTN == trimws(tclvalue(.cdtData$EnvData$plot.maps$stnIDTSp)))
    if(length(ixy) == 0){
        Insert.Messages.Out(.cdtData$EnvData$message[[7]], format = TRUE)
        return(NULL)
    }

    don <- as.numeric(.cdtData$EnvData$output$data_grp$data[, ixy])
    location <- paste0(.cdtData$EnvData$plottext[["8"]], ": ", IDSTN[ixy])

    intstep <- .cdtData$EnvData$output$params$intstep
    minhour <- .cdtData$EnvData$output$params$minhour

    xlab <- if(optsgph$axislabs$is.xlab) optsgph$axislabs$xlab else ''
    ylab_s <- if(intstep %in% c('minute', 'hourly')) "5" else "6"
    ylab_s <- .cdtData$EnvData$plottext[[ylab_s]]
    ylab_s <- paste(.cdtData$EnvData$plottext[["4"]], ylab_s)
    ylab <- if(optsgph$axislabs$is.ylab) optsgph$axislabs$ylab else ylab_s
    titre <- if(optsgph$title$is.title) optsgph$title$title else IDSTN[ixy]

    grp <- .cdtData$EnvData$output$data_grp$grp
    at.tick <- seq_len(length(grp) + 1)
    at.lab <- seq_along(grp) - 0.5

    max.ylim <- switch(intstep,
                       'minute' = (31 * 24 * 60 / minhour) + 60,
                       'hourly' = (31 * 24/minhour) + 24,
                       'daily' = 400, 'pentad' = 80,
                       'dekadal' = 40, 'monthly' = 14)

    opar <- graphics::par(mar = c(6, 4, 3, 1.5))
    graphics::barplot(don, space = 0, ylim = c(0, max.ylim), xaxt = 'n', yaxt = 'n',
                      col = optsgph$colors$col, main = titre)
    graphics::axis(side = 2, at = graphics::axTicks(2))
    graphics::axis(side = 1, at = at.tick - 1, labels = FALSE, tcl = graphics::par("tcl") * 0.7)
    graphics::axis(side = 1, at = at.lab , tick = FALSE, labels = grp, las = 3, mgp = c(3, 0.7, 0), cex.axis = 1)
    # graphics::text(x = at.lab, y = par("usr")[3] - 0.45, labels = grp, xpd = TRUE, srt = 45, adj = 1.5, cex = 0.8)
    graphics::abline(h = graphics::axTicks(2), col = "lightgray", lty = "dotted")
    graphics::mtext(xlab, side = 1, line = 4)
    graphics::mtext(ylab, side = 2, line = 2.5)

    graphics::mtext(location, side = 3, outer = FALSE, adj = 1, line = 0, cex = 0.6)
    graphics::box()
    plt <- graphics::par("plt")
    usr <- graphics::par("usr")
    graphics::par(opar)

    return(list(par = c(plt, usr)))
}

######################################################################################################

assessData_displayActivities <- function(notebookTab, title.graph){
    varplot <- c("parPlotSize1", "parPlotSize2", "parPlotSize3", "parPlotSize4",
                "usrCoords1", "usrCoords2", "usrCoords3", "usrCoords4")
    parPltCrd <- stats::setNames(lapply(varplot, function(x) assign(x, tclVar(), envir = parent.frame())), varplot)

    YLayoutDiv <- c(0.1, 1)

    plotIt <- function(){
        op <- graphics::par(bg = "white")
        pltusr <- assessData_plotActivities(YLayoutDiv)
        graphics::par(op)
        for(j in seq_along(varplot))
            tclvalue(parPltCrd[[varplot[j]]]) <- pltusr$par[j]
        return(0)
    }

    #########
    onglet <- imageNotebookTab_open(notebookTab, title.graph)
    hscale <- as.numeric(tclvalue(tkget(.cdtEnv$tcl$toolbar$spinH)))
    vscale <- as.numeric(tclvalue(tkget(.cdtEnv$tcl$toolbar$spinV)))

    img <- DisplayPlot(onglet[[2]], fun = plotIt, hscale = hscale, vscale = vscale)
    tkgrid(img)
    tkgrid.rowconfigure(img, 0, weight = 1)
    tkgrid.columnconfigure(img, 0, weight = 1)
    tcl("update")
    # unlink(img$file)

    #########
    intstep <- .cdtData$EnvData$output$params$intstep

    tkbind(img, "<Motion>", function(W, x, y){
        xyMouse <- mouseMouvment(W, x, y, parPltCrd, ydiv = c(0, 1 / sum(YLayoutDiv)))

        frxcoord <- if(xyMouse$inout) '' else format.plot.date.label(xyMouse$x, intstep)
        frycoord <- if(xyMouse$inout) '' else round(xyMouse$y)

        tclvalue(.cdtEnv$tcl$status$xcrd) <- frxcoord
        tclvalue(.cdtEnv$tcl$status$ycrd) <- frycoord
    })

    tkbind(img, "<Enter>", function() tkconfigure(img, cursor = 'crosshair'))
    tkbind(img, "<Leave>", function() tkconfigure(img, cursor = ''))

    return(list(onglet, img))
}

##############################

assessData_displayDataGroupTS <- function(notebookTab, title.graph){
    varplot <- c("parPlotSize1", "parPlotSize2", "parPlotSize3", "parPlotSize4",
                "usrCoords1", "usrCoords2", "usrCoords3", "usrCoords4")
    parPltCrd <- stats::setNames(lapply(varplot, function(x) assign(x, tclVar(), envir = parent.frame())), varplot)

    plotIt <- function(){
        op <- graphics::par(bg = "white")
        pltusr <- assessData_plotDataGroupTS()
        graphics::par(op)
        for(j in seq_along(varplot))
            tclvalue(parPltCrd[[varplot[j]]]) <- pltusr$par[j]
        return(0)
    }

    #########
    onglet <- imageNotebookTab_open(notebookTab, title.graph)
    hscale <- as.numeric(tclvalue(tkget(.cdtEnv$tcl$toolbar$spinH)))
    vscale <- as.numeric(tclvalue(tkget(.cdtEnv$tcl$toolbar$spinV)))

    img <- DisplayPlot(onglet[[2]], fun = plotIt, hscale = hscale, vscale = vscale)
    tkgrid(img)
    tkgrid.rowconfigure(img, 0, weight = 1)
    tkgrid.columnconfigure(img, 0, weight = 1)
    tcl("update")
    # unlink(img$file)

    #########
    grp <- .cdtData$EnvData$output$data_grp$grp

    tkbind(img, "<Motion>", function(W, x, y){
        xyMouse <- mouseMouvment(W, x, y, parPltCrd)

        ipos <- ceiling(xyMouse$x)
        yearAxisRange <- ipos < 1 | ipos > length(grp) | xyMouse$inout
        frxcoord <- ifelse(yearAxisRange, '', grp[ipos])
        frycoord <- ifelse(xyMouse$inout, '', round(xyMouse$y))

        tclvalue(.cdtEnv$tcl$status$xcrd) <- frxcoord
        tclvalue(.cdtEnv$tcl$status$ycrd) <- frycoord
    })

    tkbind(img, "<Enter>", function() tkconfigure(img, cursor = 'crosshair'))
    tkbind(img, "<Leave>", function() tkconfigure(img, cursor = ''))

    return(list(onglet, img))
}

##############################

assessData_displayAvailability <- function(title.map, title.graph){
    imgContainer <- CDT.Display.Map.inter(assessData_plotAvailability, .cdtData$EnvData$tab$availMap, title.map)
    .cdtData$EnvData$tab$availMap <- imageNotebookTab_unik(imgContainer, .cdtData$EnvData$tab$availMap)

    ###############
    tkbind(imgContainer[[2]], "<Button-1>", function(W, x, y){
        xyid <- getIDLatLonCoords(W, x, y, imgContainer[[3]], getStnIDLabel,
                        stn.coords = .cdtData$EnvData$plot.maps[c('lon', 'lat', 'id')])
        if(xyid$plotTS)
            tclvalue(.cdtData$EnvData$plot.maps$stnIDTSp) <- xyid$crd

        if(xyid$plotTS){
            if(trimws(tclvalue(.cdtData$EnvData$plot.maps$typeTSp)) == "Chart"){
                if(is.null(.cdtData$EnvData$output$data_grp)) return(NULL)
                imgContainer1 <- assessData_displayDataGroupTS(.cdtData$EnvData$tab$Chart, title.graph)
                .cdtData$EnvData$tab$Chart <- imageNotebookTab_unik(imgContainer1, .cdtData$EnvData$tab$Chart)
            }

            if(trimws(tclvalue(.cdtData$EnvData$plot.maps$typeTSp)) == "Table"){
                if(is.null(.cdtData$EnvData$output$data_ts)) return(NULL)
                table.ts <- assessData_dataTableTS()
                if(is.null(table.ts)) return(NULL)
                .cdtData$EnvData$tab$Table <- tableNotebookTab_unik(table.ts, .cdtData$EnvData$tab$Table, title.graph)
            }
        }
    })
}

##############################

assessData_displayDataGroupMap <- function(title.map, title.graph){
    imgContainer <- CDT.Display.Map.inter(assessData_plotDataGroupMap, .cdtData$EnvData$tab$grpMap, title.map)
    .cdtData$EnvData$tab$grpMap <- imageNotebookTab_unik(imgContainer, .cdtData$EnvData$tab$grpMap)

    ###############
    tkbind(imgContainer[[2]], "<Button-1>", function(W, x, y){
        xyid <- getIDLatLonCoords(W, x, y, imgContainer[[3]], getStnIDLabel,
                        stn.coords = .cdtData$EnvData$plot.maps[c('lon', 'lat', 'id')])
        if(xyid$plotTS)
            tclvalue(.cdtData$EnvData$plot.maps$stnIDTSp) <- xyid$crd

        if(xyid$plotTS){
            if(trimws(tclvalue(.cdtData$EnvData$plot.maps$typeTSp)) == "Chart"){
                if(is.null(.cdtData$EnvData$output$data_grp)) return(NULL)
                imgContainer1 <- assessData_displayDataGroupTS(.cdtData$EnvData$tab$Chart, title.graph)
                .cdtData$EnvData$tab$Chart <- imageNotebookTab_unik(imgContainer1, .cdtData$EnvData$tab$Chart)
            }

            if(trimws(tclvalue(.cdtData$EnvData$plot.maps$typeTSp)) == "Table"){
                if(is.null(.cdtData$EnvData$output$data_ts)) return(NULL)
                table.ts <- assessData_dataTableTS()
                if(is.null(table.ts)) return(NULL)
                .cdtData$EnvData$tab$Table <- tableNotebookTab_unik(table.ts, .cdtData$EnvData$tab$Table, title.graph)
            }
        }
    })
}

##############################

assessData_dataTableTS <- function(){
    IDSTN <- .cdtData$EnvData$output$data$id
    ixy <- which(IDSTN == trimws(tclvalue(.cdtData$EnvData$plot.maps$stnIDTSp)))
    if(length(ixy) == 0){
        Insert.Messages.Out(.cdtData$EnvData$message[[7]], format = TRUE)
        return(NULL)
    }

    data.ts <- .cdtData$EnvData$output$data_ts
    ts1 <- data.ts$ts1
    ts2 <- data.ts$ts2
    don <- as.numeric(data.ts$data[, ixy])
    intstep <- .cdtData$EnvData$output$params$intstep
    if(intstep %in% c('minute', 'hourly')){
        table_h <- paste0('Day-', sort(unique(ts2)))
    }else{
        table_h <- format(ISOdate(2014, 1:12, 1), "%b")
    }
    res <- reshapeXYZ2Matrix(cbind(ts1, ts2, don))
    head <- c(paste0('STN', '::', IDSTN[ixy]), table_h[as.integer(res$y)])
    res <- rbind(head, cbind(res$x, res$z))
    res <- data.frame(res, stringsAsFactors = FALSE)
    names(res) <- NULL
    row.names(res) <- NULL

    return(res)
}
