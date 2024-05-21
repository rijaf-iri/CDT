
curve.intersects.points <- function(y1, y2){
    seg1 <- which(!!diff(y1 > y2))
    if(length(seg1) == 0) return(NULL)
    slope1 <- y1[seg1 + 1] - y1[seg1]
    slope2 <- y2[seg1 + 1] - y2[seg1]
    x <- seg1 + ((y2[seg1] - y1[seg1]) / (slope1 - slope2))
    y <- y1[seg1] + slope1 * (x - seg1)
    list(x = x, y = y)
}

###

xaxis.intersection <- function(idx, x){
    ix <- findInterval(idx, seq_along(x))
    out <- rep(NA, length(idx))
    for(j in seq_along(idx))
        out[j] <- (idx[j] - ix[j]) * ((x[ix[j] + 1] - x[ix[j]]) / (ix[j] + 1 - ix[j])) + x[ix[j]]
    if(idx[length(idx)] == length(x)) out[length(idx)] <- x[length(x)]
    return(out)
}

###

remove.duplicated.edge <- function(x, y){
    isq <- rbind(NA, diff(cbind(x, y)))
    isq <- isq[, 1] == 0 & isq[, 2] == 0
    isq[is.na(isq)] <- FALSE
    ina <- is.na(x) | is.na(y)
    idup <- duplicated(cbind(x, y)) & !ina & isq
    x <- x[!idup]
    y <- y[!idup]
    list(x = x, y = y)
}

###

create.polygons <- function(x, y1, y2, xs, ys, xe, ye){
    ina <- is.na(y1) | is.na(y2)
    if(any(ina)){
        y1[ina] <- NA
        y2[ina] <- NA

        rna <- rle(!is.na(y1))
        ic2 <- cumsum(rna$lengths)
        ic1 <- c(1, utils::head(ic2 + 1, -1))
        ic1 <- ic1[rna$values]
        ic2 <- ic2[rna$values]

        out <- lapply(seq_along(ic1), function(l){
            ic <- ic1[l]:ic2[l]
            xc <- x[ic]
            y1c <- y1[ic]
            y2c <- y2[ic]

            if(l == 1){
                xc <- c(xs, xc, rev(xc))
                yc <- c(ys, y1c, rev(y2c))
            }else if(l == length(ic1)){
                xc <- c(xc, xe, rev(xc))
                yc <- c(y1c, ye, rev(y2c))
            }else{
                xc <- c(xc, rev(xc))
                yc <- c(y1c, rev(y2c))
            }
            remove.duplicated.edge(xc, yc)
        })
    }else{
        x <- c(xs, x, xe, rev(x))
        y <- c(ys, y1, ye, rev(y2))
        out <- list(remove.duplicated.edge(x, y))
    }
    return(out)
}

###
# x <- 1:100
# # y1 <- rep(0, length(x))
# y1 <- cos(x)
# y2 <- sin(x)
# # plot(x, y1, type = 'l')
# # lines(x, y2, col = 2)

# polys <- split.polygons.non_missing(x, y1, y2)
## y2[10:15] <- NA
# polys <- split.polygons.with_missing(x, y1, y2)
# plot(x, y2, type = 'n')
# for(j in seq_along(polys)){
#     P <- polys[[j]]
#     # polygon(P$x, P$y, col = 'blue', border = NA)
#     polygon(P$x, P$y, col = 'blue', border = 'red')
# }

#' @exportS3Method NULL
split.polygons.non_missing <- function(x, y1, y2){
    nl <- length(x)
    seqx <- seq(nl)

    ixy <- curve.intersects.points(y1, y2)
    if(is.null(ixy)){
        iz <- if(isTRUE(all(y1 >= y2))) 1 else 2
        polys <- list(list(x = c(x, rev(x)), y = c(y1, rev(y2)), z = iz))
        return(polys)
    }

    intv <- findInterval(1:nl, c(0, ixy$x))
    intu <- unique(intv)
    inte <- intu[length(intu)]

    if(y1[nl] == y2[nl]) intu <- intu[-inte]

    Xv <- xaxis.intersection(ixy$x, x)
    Yv <- ixy$y

    polys <- lapply(intu, function(i){
        xstart <- if(i == 1) x[1] else Xv[i - 1]
        ystart <- if(i == 1) y1[1] else Yv[i - 1]
        xend <- if(i == inte) x[nl] else Xv[i]
        yend <- if(i == inte) y1[nl] else Yv[i]

        ix <- seqx[intv == i]
        xp <- c(xstart, x[ix], xend, rev(x[ix]))
        yp <- c(ystart, y1[ix], yend, rev(y2[ix]))

        out <- remove.duplicated.edge(xp, yp)
        ic <- unique(match(out$x , x))
        iz <- y1[ic] >= y2[ic]
        iz <- iz[!is.na(iz)]
        iz <- if(isTRUE(all(iz))) 1 else 2
        out$z <- iz
        return(out)
    })
    return(polys)
}

####

#' @exportS3Method NULL
split.polygons.with_missing <- function(x, y1, y2){
    ixy <- curve.intersects.points(y1, y2)
    nl <- length(x)
    seqx <- seq(nl)

    if(!is.null(ixy)){
        intv <- findInterval(1:nl, c(0, ixy$x))
        intu <- unique(intv)
        inte <- intu[length(intu)]

        ###
        xna <- is.na(y1) | is.na(y2)
        xna <- rle(xna)
        xnl <- xna$lengths

        ###
        if(xna$values[1]){
            x0 <- utils::tail(x, -xnl[1])[1]
            y0 <- utils::tail(y1, -xnl[1])[1]
        }else{
            x0 <- x[1]
            y0 <- y1[1]
        }

        ###
        if(xna$values[length(xnl)]){
            y1l <- utils::head(y1, -xnl[length(xnl)])
            y2l <- utils::head(y2, -xnl[length(xnl)])
            ilu <- length(y1l)
            if(y1l[ilu] == y2l[ilu]) intu <- intu[-inte]
        }

        Xv <- xaxis.intersection(ixy$x, x)
        Yv <- ixy$y

        polys <- lapply(intu, function(i){
            xstart <- if(i == 1) x0 else Xv[i - 1]
            ystart <- if(i == 1) y0 else Yv[i - 1]
            xend <- if(i == inte) x[nl] else Xv[i]
            yend <- if(i == inte) y1[nl] else Yv[i]

            ix <- seqx[intv == i]
            out <- create.polygons(x[ix], y1[ix], y2[ix], xstart, ystart, xend, yend)
            out <- lapply(out, function(v){
                ic <- unique(match(v$x , x))
                iz <- y1[ic] >= y2[ic]
                iz <- iz[!is.na(iz)]
                iz <- if(isTRUE(all(iz))) 1 else 2
                v$z <- iz
                v
            })
            if(length(out) == 1) out <- out[[1]]
            return(out)
        })

        nlist <- sapply(sapply(polys, "[[", "x"), is.null)
        if(any(nlist)){
            ## a remplacer
            out <- list()
            for(j in seq_along(polys)) out <- if(nlist[j]) c(out, polys[[j]]) else c(out, polys[j])
            polys <- out
        }
    }else{
        iz <- y1 >= y2
        iz <- iz[!is.na(iz)]
        iz <- if(isTRUE(all(iz))) 1 else 2

        xna <- is.na(y1) | is.na(y2)
        xna <- rle(xna)
        xnl <- xna$lengths
        if(xna$values[1]){
            x <- utils::tail(x, -xnl[1])
            y1 <- utils::tail(y1, -xnl[1])
            y2 <- utils::tail(y2, -xnl[1])
        }
        if(xna$values[length(xnl)]){
            x <- utils::head(x, -xnl[length(xnl)])
            y1 <- utils::head(y1, -xnl[length(xnl)])
            y2 <- utils::head(y2, -xnl[length(xnl)])
        }

        polys <- create.polygons(x, y1, y2, x[1], y1[1], x[length(x)], y1[length(x)])
        polys <- lapply(polys, function(v){
            v$z <- iz
            return(v)
        })
    }
    return(polys)
}

####

# # polys <- list(
# #     list(x = c(seq(0, 4, 0.5), 0), y = c(0:4, 3:0, 0)), 
# #     list(x = c(seq(4, 8, 0.5), 4), y = -c(0:4, 3:0, 0)),
# #     list(x = c(seq(8, 12, 0.5), 8), y = c(0:4, 3:0, 0))
# # )
# # breaks <- c(-2, 0, 2)

# # plot(1, type = 'n', xlim = c(0, 12), ylim = c(-4, 4))
# # for(j in seq_along(polys)){
# #     # lines(polys[[j]]$x, polys[[j]]$y)
# #     polygon(polys[[j]]$x, polys[[j]]$y, col = 'red', border = NA)
# # }

####

# split.polygons.gradients.with_missing <- function(polys, breaks){
#     nom.pl <- paste0("p", seq_along(polys))
#     POLY <- lapply(seq_along(polys), function(j){
#         pl <- sp::Polygon(do.call(cbind, polys[[j]][c('x','y')]))
#         sp::Polygons(list(pl), nom.pl[j])
#     })
#     POLY <- sp::SpatialPolygons(POLY, seq_along(polys))

#     xc <- range(unlist(lapply(polys, '[[', 'x')))
#     xcrd <- xc + c(-1, 1) * diff(xc) * 0.01
#     nom.sl <- paste0("p", seq_along(breaks))
#     LINE <- lapply(seq_along(breaks), function(j){
#         sl <- sp::Line(cbind(xcrd, breaks[j]))
#         sp::Lines(list(sl), nom.sl[j])
#     })
#     LINE <- sp::SpatialLines(LINE)

#     gint <- rgeos::gIntersection(POLY, LINE)
#     gbf <- rgeos::gBuffer(gint, width = 0.000001)
#     gdif <- rgeos::gDifference(POLY, gbf)

#     brks <- c(-Inf, breaks, Inf)
#     out <- lapply(gdif@polygons[[1]]@Polygons, function(pl){
#         crd <- pl@coords
#         crd <- list(x = crd[, 'x'], y = crd[ ,'y'])
#         crd$z <- findInterval(crd$y[1], brks)
#         crd
#     })
#     return(out)
# }

####

#' @exportS3Method NULL
split.polygons.gradients.with_missing <- function(polys, breaks){
    POLY <- lapply(seq_along(polys), function(j){
        list(do.call(cbind, polys[[j]][c('x','y')]))
    })
    POLY <- sf::st_multipolygon(POLY)
    POLY <- sf::st_sfc(POLY)

    xc <- range(unlist(lapply(polys, '[[', 'x')))
    xcrd <- xc + c(-1, 1) * diff(xc) * 0.01
    LINE <- lapply(seq_along(breaks), function(j){
        cbind(xcrd, breaks[j])
    })
    LINE <- sf::st_multilinestring(LINE)
    LINE <- sf::st_sfc(LINE)

    gint <- sf::st_intersection(POLY, LINE)
    gbf <- sf::st_buffer(gint, dist = 0.000001)
    gdif <- sf::st_difference(POLY, gbf)
    gdif <- sf::st_cast(gdif, 'POLYGON')

    brks <- c(-Inf, breaks, Inf)
    out <- lapply(seq(length(gdif)), function(j){
        crd <- sf::st_coordinates(gdif[j])
        crd <- list(x = crd[, 'X'], y = crd[ ,'Y'])
        crd$z <- findInterval(crd$y[1], brks)
        crd
    })

    return(out)
}

#########
# polys <- list(
#     list(x = c(seq(0, 4, 0.5), 0), y = c(0:4, 3:0, 0)), 
#     list(x = c(seq(4, 8, 0.5), 4), y = -c(0:4, 3:0, 0)),
#     list(x = c(seq(8, 12, 0.5), 8), y = c(0:4, 3:0, 0))
# )
# breaks <- c(-2, 0, 2)
# fillcol <- rainbow(length(breaks) + 1)
# out <- split.polygons.gradients.with_missing(polys, breaks)

# plot(1, type = 'n', xlim = c(0, 12), ylim = c(-4, 4))
# for(j in seq_along(out)){
#     polygon(out[[j]]$x, out[[j]]$y, col = fillcol[out[[j]]$z], border = NA)
# }
