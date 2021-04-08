
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
    if(AggrSeries$aggr.data & AggrSeries$aggr.fun == "count"){
        units <- paste0("(Number of day ", AggrSeries$opr.fun, " ", AggrSeries$opr.thres, ")")
    }else{
        units <- if(.cdtData$EnvData$GeneralParameters$clim.var == "RR") "(mm)" else NA
    }

    ##############
    GraphOp <- .cdtData$EnvData$GraphOp
    plotType <- .cdtData$EnvData$type.graph

    gph <- switch(plotType, "Scatter" = 'scatter', "CDF" = 'cdf', "Lines" = 'line')
    optsgph <- GraphOp[[gph]]

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

        #####
        xlab <- if(is.na(units)) expression(paste("Station (" * degree, "C)")) else paste('Station', units)
        ylab <- if(is.na(units)) expression(paste("Estimate (" * degree, "C)")) else paste('Estimate', units)
    }
    if(plotType == "CDF"){
        xlim <- c(xmin, xmax)
        ylim <- c(0, 1)

        #######
        xlab <- if(.cdtData$EnvData$GeneralParameters$clim.var == "RR") "Rainfall" else "Temperature"
        xlab <- if(is.na(units)) expression(paste("Temperature ", "(" * degree, "C)")) else paste(xlab, units)
        ylab <- "Cumulative density"
    }
    if(plotType == "Lines"){
        xlim <- range(.cdtData$EnvData$opDATA$temps, na.rm = TRUE)
        ylim <- c(xmin, xmax)

        #######
        xlab <- ""
        ylab <- if(.cdtData$EnvData$GeneralParameters$clim.var == "RR") "Rainfall" else "Temperature"
        ylab <- if(is.na(units)) expression(paste("Temperature ", "(" * degree, "C)")) else paste(ylab, units)
    }

    ##############
    message <- .cdtData$EnvData$message

    if(optsgph$xlim$is.min){
        xx <- optsgph$xlim$min
        xx <- if(plotType == "Lines") as.Date(xx) else as.numeric(xx) 
        if(is.na(xx))
            Insert.Messages.Out(message[['18']], TRUE, "w")
        else xlim[1] <- xx
    }
    if(optsgph$xlim$is.max){
        xx <- optsgph$xlim$max
        xx <- if(plotType == "Lines") as.Date(xx) else as.numeric(xx)
        if(is.na(xx))
            Insert.Messages.Out(message[['19']], TRUE, "w")
        else xlim[2] <- xx
    }

    if(optsgph$ylim$is.min){
        xx <- optsgph$ylim$min
        xx <- if(plotType == "Lines") as.Date(xx) else as.numeric(xx)
        if(is.na(xx))
            Insert.Messages.Out(message[['20']], TRUE, "w")
        else ylim[1] <- xx
    }
    if(optsgph$ylim$is.max){
        xx <- optsgph$ylim$max
        xx <- if(plotType == "Lines") as.Date(xx) else as.numeric(xx)
        if(is.na(xx))
            Insert.Messages.Out(message[['21']], TRUE, "w")
        else ylim[2] <- xx
    }

    if(optsgph$axislabs$is.xlab) xlab <- optsgph$axislabs$xlab
    if(optsgph$axislabs$is.ylab) ylab <- optsgph$axislabs$ylab

    ##############

    data.name <- .cdtData$EnvData$VALID.names

    if(optsgph$validName$change){
        if(!c("") %in% optsgph$validName$name){
            if(length(data.name) == length(optsgph$validName$name)){
                data.name <- optsgph$validName$name
            }else{
                Insert.Messages.Out(message[['22']], TRUE, "w")
            }
        }else{
            Insert.Messages.Out(message[['23']], TRUE, "w")
        }
    }

    plot.order <- match(data.name, levels(as.factor(data.name)))

    ##############

    if(plotType == "Scatter"){
        don <- lapply(seq_along(y), function(i) data.frame(x = x, y = y[[i]], name = data.name[i]))
        don <- do.call(rbind, don)

        #######
        xyax <- pretty(xlim)
        xminTck <- xyax[-length(xyax)] + diff(xyax) / 2
        xminTck <- c(min(xyax) - diff(xminTck)[1] / 2, xminTck, max(xyax) + diff(xminTck)[1] / 2)

        #######
        par.StripText <- list(cex = 1.0, col = 'black', font = 2)
        par.stripCust <- lattice::strip.custom(bg = 'lightblue')
        par.Settings <- list(background = list(alpha = 1, col = 'white'),
                             layout.widths = list(left.padding = 1, right.padding = 0.5),
                             layout.heights = list(top.padding = 1, bottom.padding = 1),
                             par.main.text = list(cex = 1.5, col = "black"),
                             par.xlab.text = list(cex = 1.0, col = "black"),
                             par.ylab.text = list(cex = 1.0, col = "black")
                            )
        # Xaxis <- list(relation = "same", draw = TRUE, at = xyax, labels = xyax, cex = 1.0, font = 1, alternating = c(1, 2), tck = c(1, 1))
        # Yaxis <- list(relation = "same", draw = TRUE, at = xyax, labels = xyax, cex = 1.0, alternating = c(1, 2), tck = c(1, 1))

        if(optsgph$plot.type == "points"){
            pp <- lattice::xyplot(y ~ x | name, don,
                            panel = function(x, y, ...){
                                lattice::panel.abline(h = xyax, v = xyax, col = "lightgray", lty = "solid", lwd = 1.0)
                                lattice::panel.abline(h = xminTck, v = xminTck, col = "lightgray", lty = "dotted")
                                # lattice::panel.xyplot(x, y, ...)
                                if(optsgph$line$draw)
                                    lattice::panel.abline(a = 0, b = 1, lwd = optsgph$line$lwd, col = optsgph$line$col)
                                lattice::panel.points(x, y, pch = optsgph$point$pch, col = optsgph$point$col, cex = optsgph$point$cex)
                            }
                        )
        }else{
            kol.hexbin <- colorRampPalette(optsgph$hexbin$col)
            pp <- hexbin::hexbinplot(y ~ x | name, don,
                            panel = function(x, y, ...){
                                lattice::panel.abline(h = xyax, v = xyax, col = "lightgray", lty = "solid", lwd = 1.0)
                                lattice::panel.abline(h = xminTck, v = xminTck, col = "lightgray", lty = "dotted")
                                hexbin::panel.hexbinplot(x, y, ...)
                                if(optsgph$line$draw)
                                    lattice::panel.abline(a = 0, b = 1, lwd = optsgph$line$lwd, col = optsgph$line$col)
                            },
                            trans = log, inv = exp, colramp = kol.hexbin, colorkey = TRUE
                        )
        }

        if(str_trim(title) != "") pp <- update(pp, main = title)

        pp <- update(pp, as.table = TRUE, par.settings = par.Settings,
                     par.strip.text = par.StripText, strip = par.stripCust,
                     # scales = list(x = Xaxis, y = Yaxis),
                     index.cond = list(plot.order),
                     xlim = xlim, ylim = ylim, xlab = xlab, ylab = ylab
                    )

        print(pp)
    }

    if(plotType == "CDF"){
        if(optsgph$plot.type == "multi"){
            xax <- seq(xmin0, xmax0, length.out = 1000)
            fx <- ecdf(x)
            # define "grp" to avoid "no visible binding for global variable" in R CMD check
            grp <- NULL
            don <- lapply(seq_along(y), function(i){
                obs <- data.frame(x = xax, y = fx(xax), name = data.name[i], grp = 'obs')
                fy <- ecdf(y[[i]])
                est <- data.frame(x = xax, y = fy(xax), name = data.name[i], grp = 'est')
                rbind(obs, est)
            })
            don <- do.call(rbind, don)

            ######
            xax <- pretty(xlim)
            xminTck <- xax[-length(xax)] + diff(xax) / 2
            xminTck <- c(min(xax) - diff(xminTck)[1] / 2, xminTck, max(xax) + diff(xminTck)[1] / 2)

            yax <- pretty(ylim)
            yminTck <- yax[-length(yax)] + diff(yax) / 2
            yminTck <- c(min(yax) - diff(yminTck)[1] / 2, yminTck, max(yax) + diff(yminTck)[1] / 2)

            ######
            par.StripText <- list(cex = 1.0, col = 'black', font = 2)
            par.stripCust <- lattice::strip.custom(bg = 'lightblue')
            par.Settings <- list(background = list(alpha = 1, col = 'white'),
                                 layout.widths = list(left.padding = 1, right.padding = 0.5),
                                 layout.heights = list(top.padding = 1, bottom.padding = 1, key.top = 2),
                                 par.main.text = list(cex = 1.5, col = "black"),
                                 par.xlab.text = list(cex = 1.0, col = "black"),
                                 par.ylab.text = list(cex = 1.0, col = "black")
                             )

            ######
            obs <- switch(optsgph$plot$obs$type,
                        'both' = list(t = 'o', lc = optsgph$plot$obs$line, lw = optsgph$plot$obs$lwd,
                                      ph = optsgph$plot$obs$pch, px = optsgph$plot$obs$cex,
                                      bg = optsgph$plot$obs$points),
                        'line' = list(t = 'l', lc = optsgph$plot$obs$line, lw = optsgph$plot$obs$lwd,
                                      ph = NA, px = NA, bg = NA)
                        )
            est <- switch(optsgph$plot$est$type,
                        'both' = list(t = 'o', lc = optsgph$plot$est$line, lw = optsgph$plot$est$lwd,
                                      ph = optsgph$plot$est$pch, px = optsgph$plot$est$cex,
                                      bg = optsgph$plot$est$points),
                        'line' = list(t = 'l', lc = optsgph$plot$est$line, lw = optsgph$plot$est$lwd,
                                      ph = NA, px = NA, bg = NA)
                        )

            ######
            pp <- lattice::xyplot(y ~ x | name, groups = grp, data = don,
                            type = c(obs$t, est$t),
                            col = c(obs$lc, est$lc), lwd = c(obs$lw, est$lw),
                            pch = c(obs$ph, est$ph), cex = c(obs$px, est$px),
                            fill = c(obs$bg, est$bg),
                            panel = function(x, y, ...){
                                lattice::panel.abline(h = yax, v = xax, col = "lightgray", lty = "solid", lwd = 1.0)
                                lattice::panel.abline(h = yminTck, v = xminTck, col = "lightgray", lty = "dotted")
                                lattice::panel.xyplot(x, y, ...)
                            }
                        )

            if(optsgph$legend$add){
                key <- list(space = "bottom", columns = 2, border = TRUE,
                           lines = list(type = c(obs$t, est$t), col = c(obs$lc, est$lc),
                                        pch = c(obs$ph, est$ph), fill = c(obs$bg, est$bg),
                                        cex = 1, lwd = 3),
                           divide = 1, padding.text = 8, between.columns = 1,
                           text = list(lab = c(optsgph$legend$obs, optsgph$legend$est), cex = 1)
                          )
                pp <- update(pp, key = key)
            }

            if(str_trim(title) != "") pp <- update(pp, main = title)

            pp <- update(pp, as.table = TRUE, par.settings = par.Settings,
                         par.strip.text = par.StripText, strip = par.stripCust,
                         index.cond = list(plot.order),
                         xlim = xlim, ylim = ylim, xlab = xlab, ylab = ylab
                        )

            print(pp)
        }else{
            plot(1, type = 'n', xaxt = 'n', yaxt = 'n', xlim = xlim, ylim = ylim, xlab = '', ylab = '')

            xminTck <- axTicks(1)
            xminTck <- xminTck[-length(xminTck)] + diff(xminTck) / 2
            xminTck <- c(min(axTicks(1)) - diff(xminTck)[1] / 2, xminTck, max(axTicks(1)) + diff(xminTck)[1] / 2)
            yminTck <- axTicks(2)
            yminTck <- yminTck[-length(yminTck)] + diff(yminTck) / 2
            yminTck <- c(min(axTicks(2)) - diff(yminTck)[1] / 2, yminTck, max(axTicks(2)) + diff(yminTck)[1] / 2)

            abline(h = axTicks(2), col = "lightgray", lty = "solid", lwd = 1.0)
            abline(h = yminTck, col = "lightgray", lty = "dotted")
            abline(v = axTicks(1), col = "lightgray", lty = "solid", lwd = 1.0)
            abline(v = xminTck, col = "lightgray", lty = "dotted")

            axis(1, at = axTicks(1), font = 1)
            axis(1, at = xminTck, labels = NA, tcl = par("tcl") * 0.5)
            mtext(xlab, side = 1, line = 2.5, cex = 1)
            axis(2, at = axTicks(2), las = 2, font = 1)
            axis(2, at = yminTck, labels = NA, tcl = par("tcl") * 0.6)
            mtext(ylab, side = 2, line = 3, cex = 1)
            title(main = title, cex.main = 1.2)
            box()

            ########

            obs <- switch(optsgph$plot$obs$type,
                        'both' = list(t = 'o', lc = optsgph$plot$obs$line, lw = optsgph$plot$obs$lwd,
                                      ph = optsgph$plot$obs$pch, px = optsgph$plot$obs$cex,
                                      bg = optsgph$plot$obs$points),
                        'line' = list(t = 'l', lc = optsgph$plot$obs$line, lw = optsgph$plot$obs$lwd,
                                      ph = NA, px = NA, bg = NA)
                        )

            kol.Est <- colorRampPalette(optsgph$plot1$est)(length(data.name))
            legendlab <- c(optsgph$validName$obs, data.name)

            #########

            plotECDF <- any(!is.na(x)) & Reduce('&', lapply(y, function(x) any(!is.na(x))))
            if(plotECDF){
                fx <- ecdf(x)
                xax <- seq(xmin0, xmax0, length.out = 1000)
                lines(xax, fx(xax), type = obs$t, col = obs$lc, lwd = obs$lw, bg = obs$bg, pch = obs$ph, cex = obs$px)
                for(j in seq_along(y)){
                    fy <- ecdf(y[[j]])
                    lines(xax, fy(xax), type = 'l', lwd = obs$lw, col = kol.Est[j])
                }
            }

            if(optsgph$legend$add){
                estNA <- rep(NA, length(kol.Est))
                legend('bottomright', legendlab, col = c(obs$lc, kol.Est), pch = c(obs$ph, estNA), 
                       bg = c(obs$bg, estNA), pt.cex = 1, pt.lwd = 1, lwd = 3, cex = 1, bty = 'n')
            }
        }
    }

    if(plotType == "Lines"){
        daty <- .cdtData$EnvData$opDATA$temps

        if(optsgph$plot.type == "multi"){
            don <- lapply(seq_along(y), function(i){
                obs <- data.frame(x = daty, y = x, name = data.name[i], grp = 'obs')
                est <- data.frame(x = daty, y = y[[i]], name = data.name[i], grp = 'est')
                rbind(obs, est)
            })
            don <- do.call(rbind, don)

            ######

            xax <- pretty(xlim)
            if(as.numeric(diff(xlim)) > 1095){
                xminTck <- seq(as.Date(paste0(format(xlim[1], "%Y"), "-01-01")),
                            as.Date(paste0(as.numeric(format(xlim[2], "%Y")) + 1, "-01-01")), "year")
                xminTck <- xminTck[!xminTck %in% xax]
            }else xminTck <- NULL

            yax <- pretty(ylim)
            yminTck <- yax[-length(yax)] + diff(yax) / 2
            yminTck <- c(min(yax) - diff(yminTck)[1] / 2, yminTck, max(yax) + diff(yminTck)[1] / 2)

            ######
            par.StripText <- list(cex = 1.0, col = 'black', font = 2)
            par.stripCust <- lattice::strip.custom(bg = 'lightblue')
            par.Settings <- list(background = list(alpha = 1, col = 'white'),
                                 layout.widths = list(left.padding = 1, right.padding = 0.5),
                                 layout.heights = list(top.padding = 1, bottom.padding = 0.5, key.top = 2),
                                 par.main.text = list(cex = 1.5, col = "black"),
                                 par.xlab.text = list(cex = 1.0, col = "black"),
                                 par.ylab.text = list(cex = 1.0, col = "black")
                             )

            ######
            obs <- switch(optsgph$plot$obs$type,
                        'both' = list(t = 'o', lc = optsgph$plot$obs$line, lw = optsgph$plot$obs$lwd,
                                      ph = optsgph$plot$obs$pch, px = optsgph$plot$obs$cex,
                                      bg = optsgph$plot$obs$points),
                        'line' = list(t = 'l', lc = optsgph$plot$obs$line, lw = optsgph$plot$obs$lwd,
                                      ph = NA, px = NA, bg = NA)
                        )
            est <- switch(optsgph$plot$est$type,
                        'both' = list(t = 'o', lc = optsgph$plot$est$line, lw = optsgph$plot$est$lwd,
                                      ph = optsgph$plot$est$pch, px = optsgph$plot$est$cex,
                                      bg = optsgph$plot$est$points),
                        'line' = list(t = 'l', lc = optsgph$plot$est$line, lw = optsgph$plot$est$lwd,
                                      ph = NA, px = NA, bg = NA)
                        )

            ######
            pp <- lattice::xyplot(y ~ x | name, groups = grp, data = don,
                            type = c(obs$t, est$t),
                            col = c(obs$lc, est$lc), lwd = c(obs$lw, est$lw),
                            pch = c(obs$ph, est$ph), cex = c(obs$px, est$px),
                            fill = c(obs$bg, est$bg),
                            panel = function(x, y, ...){
                                lattice::panel.abline(h = yax, v = xax, col = "lightgray", lty = "solid", lwd = 1.0)
                                lattice::panel.abline(h = yminTck, v = xminTck, col = "lightgray", lty = "dotted")
                                lattice::panel.xyplot(x, y, ...)
                            }
                        )

            if(optsgph$legend$add){
                key <- list(space = "bottom", columns = 2, border = TRUE,
                            lines = list(type = c(obs$t, est$t), col = c(obs$lc, est$lc),
                                         pch = c(obs$ph, est$ph), fill = c(obs$bg, est$bg),
                                         cex = 1, lwd = 3),
                            divide = 1, padding.text = 8, between.columns = 1,
                            text = list(lab = c(optsgph$legend$obs, optsgph$legend$est), cex = 1.0)
                          )
                pp <- update(pp, key = key)
            }

            if(str_trim(title) != "") pp <- update(pp, main = title)

            pp <- update(pp, as.table = TRUE, par.settings = par.Settings,
                         par.strip.text = par.StripText, strip = par.stripCust,
                         index.cond = list(plot.order),
                         xlim = xlim, ylim = ylim, xlab = xlab, ylab = ylab
                        )

            print(pp)
        }else{
            legH <- if(optsgph$legend$add) 0.1 else 0.01

            layout(matrix(1:2, ncol = 1), widths = 1, heights = c(0.9, legH), respect = FALSE)
            op <- par(mar = c(4, 4.5, 2, 2))

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

            abline(h = axTicks(2), col = "lightgray", lty = "solid", lwd = 1.0)
            abline(h = yminTck, col = "lightgray", lty = "dotted")
            abline(v = xTck, col = "lightgray", lty = "solid", lwd = 1.0)
            if(!is.null(xminTck))
                abline(v = xminTck, col = "lightgray", lty = "dotted")

            axis.Date(1, at = xTck, font = 1)
            if(!is.null(xminTck))
                axis.Date(1, at = xminTck, labels = NA, tcl = par("tcl") * 0.5)
            mtext(xlab, side = 1, line = 2.5, cex = 1)
            axis(2, at = axTicks(2), las = 2, font = 1)
            axis(2, at = yminTck, labels = NA, tcl = par("tcl") * 0.6)
            mtext(ylab, side = 2, line = 3, cex = 1)
            title(main = title, cex.main = 1.2)
            box()

            ########

            obs <- switch(optsgph$plot$obs$type,
                        'both' = list(t = 'o', lc = optsgph$plot$obs$line, lw = optsgph$plot$obs$lwd,
                                      ph = optsgph$plot$obs$pch, px = optsgph$plot$obs$cex,
                                      bg = optsgph$plot$obs$points),
                        'line' = list(t = 'l', lc = optsgph$plot$obs$line, lw = optsgph$plot$obs$lwd,
                                      ph = NA, px = NA, bg = NA)
                        )

            kol.Est <- colorRampPalette(optsgph$plot1$est)(length(data.name))
            legendlab <- c(optsgph$validName$obs, data.name)

            #########

            lines(daty, x, type = obs$t, col = obs$lc, lwd = obs$lw, bg = obs$bg, pch = obs$ph, cex = obs$px)
            for(j in seq_along(y))
                lines(daty, y[[j]], type = 'l', col = kol.Est[j], lwd = obs$lw)
            par(op)

            op <- par(mar = c(0, 4, 0, 2))
            plot.new()
            if(optsgph$legend$add){
                estNA <- rep(NA, length(kol.Est))
                legend('top', 'groups', legend = legendlab, col = c(obs$lc, kol.Est), pch = c(obs$ph, estNA),
                       bg = c(obs$bg, estNA), lwd = 3, lty = 1, horiz = TRUE)
            }
            par(op)
        }
    }
    return(0)
}

##################################################################################################

multiValidation.plotStatMaps <- function(){
    dataMapOp <- .cdtData$EnvData$statMapOp
    typeMap <- str_trim(tclvalue(.cdtData$EnvData$typeMap))

    #######

    stat.STN <- .cdtData$EnvData$Statistics$STN[c("cont", "catg", "volume")]
    istat <- sapply(lapply(stat.STN, '[[', 1), function(x){
        ll <- which(rownames(x$statistics) == .cdtData$EnvData$statVAR)
        if(length(ll)) ll else 0
    })
    ix <- which(istat != 0)
    don <- lapply(stat.STN[[ix]], function(x) x$statistics[istat[ix], ])

    #######

    if(!dataMapOp$title$user){
        titre <- stat.STN[[ix]][[1]]$description[istat[ix]]
    }else titre <- dataMapOp$title$title

    colorkey.Title <- if(dataMapOp$colkeyLab$user) dataMapOp$colkeyLab$label else ""

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

    shpf <- .cdtData$EnvData$shp
    ocrds <- if(tclvalue(shpf$add.shp) == "1" & !is.null(shpf$ocrds)) shpf$ocrds else matrix(NA, 1, 2)
    SHPOp <- .cdtData$EnvData$SHPOp

    #######
    ## range ocrds
    pars.x <- parAxisPlotFun(range(data.Crd$x))
    pars.y <- parAxisPlotFun(range(data.Crd$y))
    data.Rg <- range(sapply(data.Obj, range, na.rm = TRUE), na.rm = TRUE)
    brks <- image.plot_Legend_pars(data.Rg, dataMapOp$userLvl, dataMapOp$userCol, dataMapOp$presetCol)

    xlim <- range(c(pars.x$usr, ocrds[, 1]), na.rm = TRUE)
    ylim <- range(c(pars.y$usr, ocrds[, 2]), na.rm = TRUE)

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
                    aspect = "fill",
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
    cKT <- if(colorkey.Title == "") 2 else 3
    pars.key <- switch(place,
                    "bottom" = list(x = 0.5, y = 0, rot = 0, side = place, pad = c(1, 1, 1, cKT)),
                    "right" = list(x = 1, y = 0.5, rot = 90, side = place, pad = c(1, cKT, 1, 1))
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
                xlim = xlim, ylim = ylim, xlab = '', ylab = '', main = titre,
                par.settings = par.Settings, par.strip.text = par.StripText, strip = par.stripCust,
                scales = list(x = Xaxis, y = Yaxis), legend = lezandy))
    return(0)
}

##################################################################################################

image.foramtted.table <- function(X, rk, title = "",
                                  pars = list(
                                        col = list(fill = rev(RColorBrewer::brewer.pal(9, "Blues")),
                                                   text = c("red", "orange", "black")),
                                        key = list(title = "Performance",
                                                   lab1 = "Weakest",
                                                   lab2 = "Strongest")
                                        )
                                )
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

    if(class(pars$col$fill) == "function"){
        foo <- pars$col$fill
    }else{
        if(length(pars$col$fill) == 1 & is.character(pars$col$fill)){
            foo <- get(pars$col$fill, mode = "function")
        }else{
            foo <- colorRampPalette(pars$col$fill)
        }
    }
    kolor <- foo(nbc)

    nbx <- length(pars$col$text)
    if(nbx < nbc){
        if(nbx == 1){
            col.txt <- rep(pars$col$text, nbc)
        }else{
            col.txt <- c(pars$col$text[1:(nbx -1)],
                         rep(pars$col$text[nbx], nbc - nbx + 1))
        }
    }else col.txt <- pars$col$text[1:nbc]

    text.kol <- col.txt[findInterval(c(rk), breaks, rightmost.closed = TRUE, left.open = TRUE)]

    x <- 1:nc
    y <- 1:nr
    centers <- expand.grid(y, x)

    xlim <- range(x) + c(-0.5, 0.5)
    ylim <- c(max(y) + 0.5, min(y) - 0.5)

    #########

    draw.title <- if(title == "") FALSE else TRUE
    mar.top <- if(draw.title) 6.0 else 4.5
    mar.bas <- max(nchar(mc)) * 0.4 + 1.1

    #########
    op <- par(mar = c(mar.bas, 5.0, mar.top, 2.0))
    plot(1, xlim = xlim, ylim = ylim, xlab = "", ylab = "", type = "n",
         xaxt = 'n', yaxt = 'n', xaxs = "i", yaxs = "i")

    if(draw.title) title(title, line = 4.5, cex.main = 1.5)

    axis(side = 1, at = x, tcl = -0.2, labels = FALSE)
    text(x = x, y = par("usr")[3] + 0.6, srt = 45, adj = 1, labels = mc, cex = 1, xpd = TRUE)
    mtext(mr, at = y, side = 2, las = 1, adj = 1.2, cex = 1)

    image(x, y, t(rk), col = kolor, breaks = breaks, xaxt = 'n', yaxt = 'n', add = TRUE)
    text(centers[, 2], centers[, 1], round(c(X), 3), cex = 1, col = text.kol)
    abline(h = y + 0.5)
    abline(v = x + 0.5)

    plt <- par("plt")
    smallplot <- c((3 * plt[1] + plt[2])/4,
                   (plt[1] + 3 * plt[2])/4,
                    plt[4] + 0.04, plt[4] + 0.06)

    fields::image.plot(zlim = rg, col = rev(kolor), horizontal = TRUE, legend.only = TRUE,
                       axis.args = list(at = c(1, nbc), labels = c(pars$key$lab1, pars$key$lab2),
                                        cex.axis = 1, font = 2, tcl = 0, mgp = c(0, 0, 0)),
                       legend.args = list(text = pars$key$title, side = 3,
                                          cex = 1.2, line = 0.2, font = 2),
                       smallplot = smallplot
                    )
    par(op)
}

##################################

multiValidation.plotRank <- function(){
    rankTabOp <- .cdtData$EnvData$RankOp
    dataset <- toupper(.cdtData$EnvData$GeneralParameters$stat.data)
    don <- .cdtData$EnvData$Statistics[[dataset]]

    descrip <- c(don$cont[[1]]$description, don$catg[[1]]$description,
                 don$volume[[1]]$description)
    pscore <- c(don$cont[[1]]$perfect.score, don$catg[[1]]$perfect.score,
                don$volume[[1]]$perfect.score)
    nstats <- don$statNames[1:(length(don$statNames) - 2)]

    infos <- list(description = descrip,
                  perfect.score = pscore,
                  stats = nstats)

    if(dataset == "STN"){
        istn <- which(.cdtData$EnvData$opDATA$id == tclvalue(.cdtData$EnvData$stnIDRank))
        stats <- lapply(don[c("cont", "catg", "volume")], function(x){
            s <- lapply(x, '[[', 'statistics')
            do.call(cbind, lapply(s, function(y) y[, istn]))
        })
    }else{
        stats <- lapply(don[c("cont", "catg", "volume")], function(x){
            s <- lapply(x, '[[', 'statistics')
            do.call(cbind, s)
        })
    }

    stats <- do.call(rbind, stats)

    validName <- .cdtData$EnvData$VALID.names
    if(rankTabOp$validName$change){
        if(!c("") %in% rankTabOp$validName$name){
            if(length(validName) == length(rankTabOp$validName$name)){
                validName <- rankTabOp$validName$name
            }else{
                Insert.Messages.Out(.cdtData$EnvData$message[['22']], TRUE, "w")
            }
        }else{
            Insert.Messages.Out(.cdtData$EnvData$message[['23']], TRUE, "w")
        }
    }
    colnames(stats) <- validName

    #######
    rang <- abs(infos$perfect.score - stats)
    rang <- t(apply(rang, 1, rank, ties.method = "min"))
    rang[is.na(stats)] <- NA

    #######
    pstats <- match(rankTabOp$stats$name[rankTabOp$stats$plot], dimnames(stats)[[1]])
    if(length(pstats) == 0){
        Insert.Messages.Out(.cdtData$EnvData$message[['24']], TRUE, "e")
        return(NULL)
    }
    stats <- stats[pstats, , drop = FALSE]
    rang <- rang[pstats, , drop = FALSE]

    #######
    if(rankTabOp$title$is.title){
        titre <- rankTabOp$title$title
    }else{
        titre <- switch(dataset,
                        "ALL" = "All Data",
                        "AVG" = "Spatial Average", 
                        "STN" = tclvalue(.cdtData$EnvData$stnIDRank)
                      )
    }

    image.foramtted.table(stats, rang, titre, rankTabOp)

    return(0)
}
