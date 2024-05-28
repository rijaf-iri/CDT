
## Format lat-lon axis label

LatLonAxisLabels <- function(axis.x, axis.y){
    axis.x <- ifelse(axis.x > 180, -360 + axis.x, axis.x)

    sym1 <- ifelse(axis.x < 0 & axis.x > -180, 'W', ifelse(axis.x > 0 & axis.x < 180, 'E', ''))
    sym1 <- paste("paste(", paste(abs(axis.x), "*degree"), ",", sym1, ")", collapse = ',')
    lon_lab <- eval(parse(text = paste0("expression(", sym1, ")")))

    sym2 <- ifelse(axis.y < 0, 'S', ifelse(axis.y > 0, 'N', ''))
    sym2 <- paste("paste(", paste(abs(axis.y), "*degree"), ",", sym2, ")", collapse = ',')
    lat_lab <- eval(parse(text = paste0("expression(", sym2, ")")))

    return(list(xaxl = lon_lab, yaxl = lat_lab))
}

########################################################################

manageLayout <- function(nbPlot, n = 5){
    nbseq <- rep(1:n, seq(1, n * 2, 2))
    nc.layout <- nbseq[nbPlot]
    seq.layout <- split(seq(nbPlot), ceiling(seq(nbPlot) / nc.layout))
    nr.layout <- length(seq.layout)
    mat <- rep(0, nc.layout * nr.layout)
    mat[seq(nbPlot)] <- seq(nbPlot)
    orderLayout <- c(matrix(mat, nr.layout, nc.layout, byrow = TRUE))
    dimLayout <- c(nr.layout, nc.layout)
    return(list(dim = dimLayout, order = orderLayout))
}


################
## Axis ticks for levelplot
parAxisPlotFun <- function(x, factor = 0.04){
    #factor = 0.04 par defaut pour R
    x <- x[!is.na(x)]
    if(length(x) > 1) xlim <- range(x) + c(-1, 1) * factor * diff(range(x))
    else if(length(x) == 1 & x != 0) xlim <- x + c(-1, 1) * factor * abs(x)
    else xlim <- factor * c(-1, 1)
    xtick <- pretty(x)
    bInf <- min(xtick[xtick >= min(xlim)])
    bSup <- max(xtick[xtick <= max(xlim)])
    intervl <- length(which(xtick >= bInf & xtick <= bSup)) - 1
    ticks <- graphics::axTicks(side = 1, usr = xlim, axp = c(bInf, bSup, intervl))
    return(list(usr = xlim, axp = ticks))
}

########################################################################

## axis Date ticks
# copy of axis.Date
axTicks.Date <- function(x, side = 1){
    x <- as.Date(x)
    range <- graphics::par("usr")[if(side %% 2) 1L:2L else 3:4L]
    range[1L] <- ceiling(range[1L])
    range[2L] <- floor(range[2L])
    d <- range[2L] - range[1L]
    z <- c(range, x[is.finite(x)])
    class(z) <- "Date"
    if(d < 7) format <- "%a"
    if(d < 100){
        z <- structure(pretty(z), class = "Date")
        format <- "%b %d"
    }else if(d < 1.1 * 365){
        zz <- as.POSIXlt(z)
        zz$mday <- 1
        zz$mon <- pretty(zz$mon)
        m <- length(zz$mon)
        m <- rep.int(zz$year[1L], m)
        zz$year <- c(m, m + 1)
        z <- as.Date(zz)
        format <- "%b"
    }else{
        zz <- as.POSIXlt(z)
        zz$mday <- 1
        zz$mon <- 0
        zz$year <- pretty(zz$year)
        z <- as.Date(zz)
        format <- "%Y"
    }
    keep <- z >= range[1L] & z <= range[2L]
    z <- z[keep]
    z <- sort(unique(z))
    class(z) <- "Date"
    z
}

########################################################################

## axis POSIXc ticks
# copy of axis.POSIXct
axTicks.POSIXct <- function(x, side = 1){
    x <- as.POSIXct(x)
    range <- graphics::par("usr")[if (side %% 2) 1L:2L else 3L:4L]
    d <- range[2L] - range[1L]
    z <- c(range, x[is.finite(x)])
    attr(z, "tzone") <- attr(x, "tzone")
    if(d < 1.1 * 60){
        sc <- 1
        format <- "%S"
    }else if(d < 1.1 * 60 * 60){
        sc <- 60
        format <- "%M:%S"
    }else if(d < 1.1 * 60 * 60 * 24){
        sc <- 60 * 60
        format <- "%H:%M"
    }else if(d < 2 * 60 * 60 * 24){
        sc <- 60 * 60
        format <- "%a %H:%M"
    }else if(d < 7 * 60 * 60 * 24){
        sc <- 60 * 60 * 24
        format <- "%a"
    }else{
        sc <- 60 * 60 * 24
    }

    if(d < 60 * 60 * 24 * 50){
        zz <- pretty(z/sc)
        z <- zz * sc
        z <- .POSIXct(z, attr(x, "tzone"))
        if(sc == 60 * 60 * 24) z <- as.POSIXct(round(z, "days"), tz = attr(x, "tzone"))
        format <- "%b %d"
    }else if(d < 1.1 * 60 * 60 * 24 * 365){
        z <- .POSIXct(z, attr(x, "tzone"))
        zz <- unclass(as.POSIXlt(z))
        zz$mday <- zz$wday <- zz$yday <- 1
        zz$isdst <- -1
        zz$hour <- zz$min <- zz$sec <- 0
        zz$mon <- pretty(zz$mon)
        m <- length(zz$mon)
        M <- 2 * m
        m <- rep.int(zz$year[1L], m)
        zz$year <- c(m, m + 1)
        zz <- lapply(zz, function(x) rep(x, length.out = M))
        zz <- .POSIXlt(zz, attr(x, "tzone"))
        z <- as.POSIXct(zz, tz = attr(x, "tzone"))
        format <- "%b"
    }else{
        z <- .POSIXct(z, attr(x, "tzone"))
        zz <- unclass(as.POSIXlt(z))
        zz$mday <- zz$wday <- zz$yday <- 1
        zz$isdst <- -1
        zz$mon <- zz$hour <- zz$min <- zz$sec <- 0
        zz$year <- pretty(zz$year)
        M <- length(zz$year)
        zz <- lapply(zz, function(x) rep(x, length.out = M))
        z <- as.POSIXct(.POSIXlt(zz), tz = attr(x, "tzone"))
        format <- "%Y"
    }

    keep <- z >= range[1L] & z <= range[2L]
    z <- z[keep]
    z
}

########################################################################

axTicks.minor.Date <- function(xlim){
    df <- as.numeric(diff(xlim))

    if(df < 7) return(NULL)
    if(df < 94){
        xminor <- seq(xlim[1], xlim[2] + 10, "day")
        return(xminor)
    }
    if(df < 372){
        start <- format(xlim[1], "%Y-%m-01")
        end <- format(xlim[2] + 31, "%Y-%m-01")
        xminor <- seq(as.Date(start), as.Date(end), "month")
        return(xminor)
    }
    if(df < 2457){
        start <- format(xlim[1], "%Y-%m-01")
        end <- format(xlim[2] + 3 * 31, "%Y-%m-01")
        xminor <- seq(as.Date(start), as.Date(end), "3 months")
        return(xminor)
    }
    if(df >= 2457){
        start <- format(xlim[1], "%Y-01-01")
        end <- format(xlim[2] + 366, "%Y-01-01")
        xminor <- seq(as.Date(start), as.Date(end), "year")
        return(xminor)
    }
}

########################################################################

axTicks.minor.POSIXct <- function(xlim){
    tz <- attr(xlim, "tzone")
    df <- diff(xlim)
    units <- attr(df, 'units')
    df <- as.numeric(df)

    if(units == "mins") return(NULL)
    else if(units == "hours"){
        if(df < 12) return(NULL)
        if(df < 25){
            # start <- format(xlim[1] - 3600, "%Y-%m-%d %H:00")
            start <- format(xlim[1], "%Y-%m-%d %H:00")
            end <- format(xlim[2] + 3600, "%Y-%m-%d %H:00")
            xminor <- seq(as.POSIXct(start, tz = tz), as.POSIXct(end, tz = tz), "hour")
            return(xminor)
        }
    }
    else if(units == "days"){
        if(df < 6){
            start <- format(xlim[1] - 6 * 3600, "%Y-%m-%d %H:00")
            end <- format(xlim[2] + 6 * 3600, "%Y-%m-%d %H:00")
            xminor <- seq(as.POSIXct(start, tz = tz), as.POSIXct(end, tz = tz), "6 hours")
            return(xminor)
        }
        if(df < 47){
            start <- format(xlim[1] - 24 * 3600, "%Y-%m-%d %H:00")
            end <- format(xlim[2] + 24 * 3600, "%Y-%m-%d %H:00")
            xminor <- seq(as.POSIXct(start, tz = tz), as.POSIXct(end, tz = tz), "day")
            return(xminor)
        }
        if(df < 372){
            start <- format(xlim[1], "%Y-%m-%d %H:00")
            end <- format(xlim[2] + 16 * 24 * 3600, "%Y-%m-%d %H:00")
            xminor <- seq(as.POSIXct(start, tz = tz), as.POSIXct(end, tz = tz), "month")
            return(xminor)
        }
        if(df < 2459){
            start <- format(xlim[1], "%Y-%m-%d %H:00")
            end <- format(xlim[2] + 3 * 31 * 24 * 3600, "%Y-%m-%d %H:00")
            xminor <- seq(as.POSIXct(start, tz = tz), as.POSIXct(end, tz = tz), "3 months")
            return(xminor)
        }
        if(df >= 2459){
            start <- format(xlim[1], "%Y-%m-%d %H:00")
            end <- format(xlim[2] + 366 * 24 * 3600, "%Y-%m-%d %H:00")
            xminor <- seq(as.POSIXct(start, tz = tz), as.POSIXct(end, tz = tz), "year")
            return(xminor)
        }
    }else return(NULL)
}

########################################################################

## format xlim date and time
# message; 1:pentad, 2:dekad, 3:hour, 4:minute, 5:invalid date
#' @exportS3Method NULL
format.xlim.date.range <- function(xlim, timestep, message)
{
    xx <- strsplit(xlim, "-")[[1]]
    xx3 <- strsplit(xx[3], " ")[[1]]
    xx3 <- xx3[xx3 != ""]
    x3 <- as.numeric(xx3[1])

    if(timestep == "minute"){
        xx4 <- strsplit(xx3[2], ":")[[1]]
        x4 <- as.numeric(xx4[1])
        x5 <- as.numeric(xx4[2])
        if(is.na(x4) | x4 < 0| x4 > 23){
            msg <- paste("xlim:", message[[3]])
            Insert.Messages.Out(msg, TRUE, "e")
            return(NULL)
        }
        if(is.na(x5) | x5 < 0| x5 > 59){
            msg <- paste("xlim:", message[[4]])
            Insert.Messages.Out(msg, TRUE, "e")
            return(NULL)
        }
    }
    if(timestep == "hourly"){
        xx4 <- strsplit(xx3[2], ":")[[1]]
        x4 <- as.numeric(xx4[1])
        x5 <- if(length(xx4) > 1) as.numeric(xx4[2]) else 0
        if(is.na(x4) | x4 < 0| x4 > 23){
            msg <- paste("xlim:", message[[3]])
            Insert.Messages.Out(msg, TRUE, "e")
            return(NULL)
        }
    }
    if(timestep == "pentad"){
        if(is.na(x3) | x3 < 1 | x3 > 6){
            msg <- paste("xlim:", message[[1]])
            Insert.Messages.Out(msg, TRUE, "e")
            return(NULL)
        }
        x3 <- c(1, 6, 11, 16, 21, 26)[x3]
    }
    if(timestep == "dekadal"){
        if(is.na(x3) | x3 < 1 | x3 > 3){
            msg <- paste("xlim:", message[[2]])
            Insert.Messages.Out(msg, TRUE, "e")
            return(NULL)
        }
        x3 <- c(1, 11, 21)[x3]
    }
    if(timestep == "monthly") x3 <- 1

    x1 <- as.numeric(xx[1])
    x2 <- stringr::str_pad(as.numeric(xx[2]), 2, pad = "0")
    x3 <- stringr::str_pad(x3, 2, pad = "0")
    if(timestep %in% c('minute', 'hourly'))
        xx <- as.POSIXct(paste(x1, x2, x3, x4, x5, sep = "-"), format = "%Y-%m-%d-%H-%M")
    else
        xx <- as.Date(paste0(x1, x2, x3), format = "%Y%m%d")

    if(is.na(xx)){
        msg <- paste("xlim:", message[[5]])
        Insert.Messages.Out(msg, TRUE, "e")
        return(NULL)
    }

    return(xx)
}

########################################################################

## image.plot colorkey parameters
#' @exportS3Method NULL
image.plot_Legend_pars <- function(Zmat, user.levels, user.colors, preset.colors)
{
    brks0 <- pretty(Zmat, n = 10, min.n = 5)
    brks0 <- if(length(brks0) > 0) brks0 else c(0, 1)
    breaks <- if(user.levels$custom) user.levels$levels else brks0
    breaks[length(breaks)] <- breaks[length(breaks)] + 1e-15

    ## legend label breaks
    legend.label <- breaks

    breaks1 <- if(user.levels$equidist) seq(0, 1, length.out = length(breaks)) else breaks

    Zmat <- Zmat + 1e-15
    Zrange <- if(all(is.na(Zmat))) c(0, 1) else range(Zmat, na.rm = TRUE)

    brks0 <- range(brks0)
    brks1 <- range(breaks)
    brn0 <- min(brks0[1], brks1[1])
    brn1 <- max(brks0[2], brks1[2])
    if(brn0 == breaks[1]) brn0 <- brn0 - 1
    if(brn1 == breaks[length(breaks)]) brn1 <- brn1 + 1
    if(brn0 > Zrange[1]) brn0 <- Zrange[1]
    if(brn1 < Zrange[2]) brn1 <- Zrange[2]
    breaks <- c(brn0, breaks, brn1)
    tbrks2 <- breaks1[c(1, length(breaks1))] + diff(range(breaks1)) * 0.02 * c(-1, 1)
    breaks2 <- c(tbrks2[1], breaks1, tbrks2[2])
    zlim <- range(breaks2)

    ## colors
    if(user.colors$custom){
        kolFonction <- grDevices::colorRampPalette(user.colors$color)
        kolor <- kolFonction(length(breaks) - 1)
    }else{
        kolFonction <- get(preset.colors$color, mode = "function")
        kolor <- kolFonction(length(breaks) - 1)
        if(preset.colors$reverse) kolor <- rev(kolor)
    }
    
    ## bin
    ## < x1; [x1, x2[; [x2, x3[; ...; [xn-1, xn]; > xn

    list(breaks = breaks, legend.breaks = list(zlim = zlim, breaks = breaks2),
        legend.axis = list(at = breaks1, labels = legend.label), colors = kolor)
}

########################################################################

## from
# https://r.789695.n4.nabble.com/Creating-smooth-color-regions-with-panel-contourplot-td866253.html

panel.filledcontour <- function(x, y, z, subscripts, at, col.regions, ...){
    z <- matrix(z[subscripts],
                nrow = length(unique(x[subscripts])),
                ncol = length(unique(y[subscripts])))
    if(!is.double(z)) storage.mode(z) <- "double"
    opar <- graphics::par(no.readonly = TRUE)
    on.exit(graphics::par(opar))
    if(lattice::panel.number() > 1) graphics::par(new = TRUE)
    graphics::par(fig = gridBase::gridFIG(), omi = c(0, 0, 0, 0), mai = c(0, 0, 0, 0))
    cpl <- lattice::current.panel.limits()
    graphics::plot.window(xlim = cpl$xlim, ylim = cpl$ylim, log = "", xaxs = "i", yaxs = "i")
    graphics::.filled.contour(as.double(lattice::do.breaks(cpl$xlim, nrow(z) - 1)),
                            as.double(lattice::do.breaks(cpl$ylim, ncol(z) - 1)),
                            z, as.double(at), col = col.regions)
}

