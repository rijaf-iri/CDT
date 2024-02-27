
qcPlot_FalseZero.Check <- function(){
    ptsOp <- .cdtData$EnvData$STN$Opts

    stnid <- trimws(tclvalue(.cdtData$EnvData$STN$stnID))
    daty <- trimws(tclvalue(.cdtData$EnvData$STN$dateFZMap))
    stats <- trimws(tclvalue(.cdtData$EnvData$STN$statFZMap))
    idaty <- which(names(.cdtData$EnvData$outzeros$mon) == daty)

    don <- .cdtData$EnvData$outzeros$mon[[idaty]][[stats]]
    don <- cbind(.cdtData$EnvData$stn.data$lon, .cdtData$EnvData$stn.data$lat, don)
    fzid <- .cdtData$EnvData$outzeros$id[[idaty]]

    #######
    istn <- which(.cdtData$EnvData$stn.data$id == stnid)
    stn_target <- list(x = don[istn, 1], y = don[istn, 2], z = don[istn, 3])

    istn1 <- fzid[which(!fzid %in% istn)]
    stn_fz <- NULL
    if(length(istn1) > 0){
        don_fz <- don[istn1, , drop = FALSE]
        don_fz <- don_fz[!is.na(don_fz[, 3]), , drop = FALSE]
        if(nrow(don_fz) > 0){
            stn_fz <- list(x = don_fz[, 1], y = don_fz[, 2], z = don_fz[, 3])
        }
    }

    don_all <- don[-c(istn, istn1), , drop = FALSE]
    don_all <- don_all[!is.na(don_all[, 3]), , drop = FALSE]
    stn_all <- list(x = don_all[, 1], y = don_all[, 2], z = don_all[, 3])

    #######
    shpf <- .cdtData$EnvData$shapefile
    ocrds <- if(tclvalue(shpf$addshp) == "1" & !is.null(shpf$ocrds)) shpf$ocrds else matrix(NA, 1, 2)

    #######
    xyzoom <- get_zoom_xylimits()
    if(is.null(xyzoom)) return(NULL)

    #######
    opar <- graphics::par(mar = c(4, 4, 2, 2))
    plot(1, xlim = xyzoom$xlim, ylim = xyzoom$ylim, xlab = "", ylab = "", type = "n", xaxt = 'n', yaxt = 'n')
    axlabs <- LatLonAxisLabels(graphics::axTicks(1), graphics::axTicks(2))
    graphics::axis(side = 1, at = graphics::axTicks(1), labels = axlabs$xaxl, tck = -0.01, cex.axis = 1.0)
    graphics::axis(side = 2, at = graphics::axTicks(2), labels = axlabs$yaxl, tck = -0.01, las = 1, cex.axis = 1.0)

    graphics::abline(h = graphics::axTicks(2), v = graphics::axTicks(1) , col = "lightgray", lty = 3, lwd = 1.3)

    graphics::lines(ocrds[, 1], ocrds[, 2], lwd = shpf$options$lwd, col = shpf$options$col)

    graphics::points(stn_all$x, stn_all$y, col = ptsOp$all$col, pch = ptsOp$all$pch, cex = ptsOp$all$cex)
    if(!is.null(stn_fz))
        graphics::points(stn_fz$x, stn_fz$y, col = ptsOp$fz$col, pch = ptsOp$fz$pch, cex = ptsOp$fz$cex)
    graphics::points(stn_target$x, stn_target$y, col = ptsOp$stn$col, bg = ptsOp$stn$txt.col, pch = ptsOp$stn$pch, cex = ptsOp$stn$cex)

    graphics::text(stn_all$x, stn_all$y, labels = stn_all$z, pos = 1, cex = ptsOp$all$txt.cex, col = ptsOp$all$txt.col)
    if(!is.null(stn_fz))
        graphics::text(stn_fz$x, stn_fz$y, labels = stn_fz$z, pos = 1, cex = ptsOp$fz$txt.cex, col = ptsOp$fz$txt.col)
    graphics::text(stn_target$x, stn_target$y, labels = stn_target$z, pos = 1, cex = ptsOp$stn$txt.cex, col = ptsOp$stn$txt.col)

    graphics::title(main = paste('STN:', stnid, '- Date:', daty), cex.main = 1, font.main = 1)
    graphics::box()

    plt <- graphics::par("plt")
    usr <- graphics::par("usr")
    graphics::par(opar)

    return(list(par = c(plt, usr)))
}
