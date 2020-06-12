
SummaryData.Initial.Map <- function(){
    don <- .cdtData$EnvData$output$map
    breaks <- pretty(don$z, n = 10, min.n = 5)
    breaks <- if(length(breaks) > 0) breaks else c(0, 1) 
    kolor <- fields::tim.colors(length(breaks) - 1)

    ### shape files
    shpf <- .cdtData$EnvData$shp
    ocrds <- if(tclvalue(shpf$add.shp) == "1" & !is.null(shpf$ocrds)) shpf$ocrds else matrix(NA, 1, 2)

    #################

    if(all(is.na(ocrds[, 1])) | all(is.na(ocrds[, 2]))){
        xlim <- range(don$x, na.rm = TRUE)
        ylim <- range(don$y, na.rm = TRUE)
    }else{
        xlim <- range(range(don$x, na.rm = TRUE), range(ocrds[, 1], na.rm = TRUE))
        ylim <- range(range(don$y, na.rm = TRUE), range(ocrds[, 2], na.rm = TRUE))
    }

    opar <- par(mar = c(4, 4, 2.5, 2.5))
    plot(1, xlim = xlim, ylim = ylim, xlab = "", ylab = "", type = "n", xaxt = 'n', yaxt = 'n')
    axlabs <- LatLonAxisLabels(axTicks(1), axTicks(2))
    axis(side = 1, at = axTicks(1), labels = axlabs$xaxl, tcl = -0.2, cex.axis = 1.0)
    axis(side = 2, at = axTicks(2), labels = axlabs$yaxl, tcl = -0.2, las = 1, cex.axis = 1.0)

    if(.cdtData$EnvData$plot.maps$data.type == "cdtstation")
        image(don, breaks = breaks, col = kolor, xaxt = 'n', yaxt = 'n', add = TRUE)
    else
        .filled.contour(don$x, don$y, don$z, levels = breaks, col = kolor)

    abline(h = axTicks(2), v = axTicks(1), col = "lightgray", lty = 3)
    lines(ocrds[, 1], ocrds[, 2], lwd = .cdtData$EnvData$SHPOp$lwd, col = .cdtData$EnvData$SHPOp$col)

    plt <- par("plt")
    usr <- par("usr")
    par(opar)
    return(list(par = c(plt, usr)))
}

################

SummaryData.Plot.Graph <- function(){
    if(.cdtData$EnvData$output$params$data.type == "cdtstation"){
        ixy <- which(.cdtData$EnvData$output$data$id == str_trim(tclvalue(.cdtData$EnvData$plot.maps$stnIDTSp)))
        if(length(ixy) == 0){
            Insert.Messages.Out(.cdtData$EnvData$message[['10']], TRUE, 'e')
            return(NULL)
        }
        stn <- .cdtData$EnvData$output$data$id[ixy]
        pts <- c(.cdtData$EnvData$output$data$lon[ixy], .cdtData$EnvData$output$data$lat[ixy])
        don <- .cdtData$EnvData$output$data$data[, ixy]
        .cdtData$EnvData$location <- paste0("Station: ", stn)
    }else{
        cdtdataset <- .cdtData$EnvData$output$data
        fileInfo <- .cdtData$EnvData$output$index.file
        xloc <- as.numeric(str_trim(tclvalue(.cdtData$EnvData$plot.maps$lonLOC)))
        yloc <- as.numeric(str_trim(tclvalue(.cdtData$EnvData$plot.maps$latLOC)))
        xyloc <- cdtdataset.extarct.TS(cdtdataset, fileInfo, xloc, yloc)
        if(is.null(xyloc)) return(NULL)
        stn <- "Pixel"
        pts <- xyloc$coords
        don <- as.numeric(xyloc$data[cdtdataset$dateInfo$index])
        .cdtData$EnvData$location <- paste0("Longitude: ", round(xloc, 5), ", Latitude: ", round(yloc, 5))
    }

    index <- .cdtData$EnvData$output$index
    mois <- format(ISOdate(2014, 1:12, 1), "%b")

    if(.cdtData$EnvData$plot.maps$plotType == "boxplot"){
        mdon <- lapply(seq_along(index), function(j){
            data.frame(mois[as.numeric(names(index[j]))], don[index[[j]]], stringsAsFactors = FALSE)
        })

        ylim <- range(pretty(don))
        mdon <- do.call(rbind, mdon)
        names(mdon) <- c("group", "value")

        don <- data.frame(group = "ALL", value = don, stringsAsFactors = FALSE)
        don <- rbind(mdon, don)
        don$group <- factor(don$group, levels = c(mois, "ALL"))

        #########
        optsgph <- .cdtData$EnvData$GraphOp$boxplot
        xlab <- if(optsgph$axislabs$is.xlab) optsgph$axislabs$xlab else ''
        ylab <- if(optsgph$axislabs$is.ylab) optsgph$axislabs$ylab else ''
        titre <- if(optsgph$title$is.title) optsgph$title$title else ''

        kol <- optsgph$col
        if(!optsgph$col$diff){
            kol <- optsgph$col
            kol$outbg <- kol$col
            kol$whiskcol <- kol$boxcol
            kol$staplecol <- kol$boxcol
            kol$outcol <- kol$boxcol
        }

        ret <- graphs.boxplot(value ~ group, data.df = don, xlim = c(1, 13), ylim = ylim,
                              xlab = xlab, ylab = ylab, title = titre, col = kol,
                              location = .cdtData$EnvData$location)
    }else{
        plotMois <- .cdtData$EnvData$plot.maps$plotMois
        if(plotMois != "all")
            don <- don[index[[which(mois == plotMois)]]]

        #########
        optsgph <- .cdtData$EnvData$GraphOp$histogram
        xlab <- if(optsgph$axislabs$is.xlab) optsgph$axislabs$xlab else ''
        ylab <- if(optsgph$axislabs$is.ylab) optsgph$axislabs$ylab else 'Density'
        titre <- if(optsgph$title$is.title) optsgph$title$title else ''

        ret <- graphs.histogram(don, xlab = xlab, ylab = ylab, title = titre,
                                bw.pars = optsgph$bw, hist.pars = optsgph$hist,
                                location = .cdtData$EnvData$location)
    }
    return(ret)
}

##############################################################################

SummaryData.Display.Map <- function(){
    imgContainer <- CDT.Display.Map.inter(SummaryData.Initial.Map, .cdtData$EnvData$tab$pMap, 'Summary-Mean-Map')
    .cdtData$EnvData$tab$pMap <- imageNotebookTab_unik(imgContainer, .cdtData$EnvData$tab$pMap)

    ###############
    tkbind(imgContainer[[2]], "<Button-1>", function(W, x, y){
        if(is.null(.cdtData$EnvData$plot.maps$data.type)) return(NULL)
        if(.cdtData$EnvData$plot.maps$data.type == "cdtstation"){
            xyid <- getIDLatLonCoords(W, x, y, imgContainer[[3]], getStnIDLabel,
                            stn.coords = .cdtData$EnvData$plot.maps[c('lon', 'lat', 'id')])
            if(xyid$plotTS)
                tclvalue(.cdtData$EnvData$plot.maps$stnIDTSp) <- xyid$crd
        }else{
            xyid <- getIDLatLonCoords(W, x, y, imgContainer[[3]], getPixelLatlon)
            if(xyid$plotTS){
                tclvalue(.cdtData$EnvData$plot.maps$lonLOC) <- xyid$crd$x
                tclvalue(.cdtData$EnvData$plot.maps$latLOC) <- xyid$crd$y
            }
        }

        if(xyid$plotTS){
            imgContainer1 <- CDT.Display.Graph(SummaryData.Plot.Graph, .cdtData$EnvData$tab$TGraph, 'Summary - Plot')
            .cdtData$EnvData$tab$TGraph <- imageNotebookTab_unik(imgContainer1, .cdtData$EnvData$tab$TGraph)
        }
    })
}

##############################################################################

SummaryData.Get.Table <- function(){
    if(.cdtData$EnvData$output$params$data.type == "cdtstation"){
        ixy <- which(.cdtData$EnvData$output$data$id == str_trim(tclvalue(.cdtData$EnvData$plot.maps$stnIDTSp)))
        if(length(ixy) == 0){
            Insert.Messages.Out(.cdtData$EnvData$message[['10']], TRUE, 'e')
            return(NULL)
        }
        stn <- .cdtData$EnvData$output$data$id[ixy]
        pts <- c(.cdtData$EnvData$output$data$lon[ixy], .cdtData$EnvData$output$data$lat[ixy])
        don <- .cdtData$EnvData$output$data$data[, ixy]
    }else{
        cdtdataset <- .cdtData$EnvData$output$data
        fileInfo <- .cdtData$EnvData$output$index.file
        xloc <- as.numeric(str_trim(tclvalue(.cdtData$EnvData$plot.maps$lonLOC)))
        yloc <- as.numeric(str_trim(tclvalue(.cdtData$EnvData$plot.maps$latLOC)))
        xyloc <- cdtdataset.extarct.TS(cdtdataset, fileInfo, xloc, yloc)
        if(is.null(xyloc)) return(NULL)
        stn <- "Pixel"
        pts <- xyloc$coords
        don <- as.numeric(xyloc$data[cdtdataset$dateInfo$index])
    }

    index <- .cdtData$EnvData$output$index

    summ <- lapply(index, function(ix){
        sm <- as.numeric(summary(don[ix]))
        sm[is.nan(sm) | is.infinite(sm)] <- NA
        sm <- if(length(sm) == 7) sm else if(length(sm) == 6) c(sm, NA) else c(rep(NA, 5), sm)
        sm
    })
    mdon <- do.call(cbind, summ)
    adon <- as.numeric(summary(don))
    adon <- if(length(adon) == 7) adon
            else if(length(adon) == 6) c(adon, NA)
            else c(rep(NA, 5), adon)
    mdon <- cbind(mdon, adon)
    mdon <- round(mdon, 4)

    std <- sapply(index, function(ix) sd(don[ix], na.rm = TRUE))
    std <- c(std, sd(don, na.rm = TRUE))
    std <- round(std, 4)

    mdon <- rbind(mdon[1:6, ], std, mdon[7, ])
    mdon <- rbind(mdon, c(stn, "Longitude", pts[1], "Latitude", pts[2], rep(NA, 8)))
    stats <- c("Minimum", "1st Quartile", "Median", "Mean", "3rd Quartile", "Maximum",
               "Standard Deviation", "Missing", "Station")
    mdon <- data.frame(stats, mdon, stringsAsFactors = FALSE)
    names(mdon) <- c("Statistics", format(ISOdate(2014, 1:12, 1), "%b"), "ALL")
    return(mdon)
}
