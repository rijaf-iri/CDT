
## same as fields::interp.surface.grid
cdt.interp.surface.grid <- function(obj, grid.list, edge = TRUE)
{
    nx0 <- length(grid.list$lon)
    ny0 <- length(grid.list$lat)
    loc <- do.call(expand.grid, grid.list)

    nx <- length(obj$lon)
    ny <- length(obj$lat)
    rule <- if(edge) 2 else 1
    lx <- stats::approx(obj$lon, 1:nx, loc$lon, rule = rule)$y
    ly <- stats::approx(obj$lat, 1:ny, loc$lat, rule = rule)$y
    lx1 <- floor(lx)
    ly1 <- floor(ly)
    ex <- lx - lx1
    ey <- ly - ly1
    ex[lx1 == nx] <- 1
    ey[ly1 == ny] <- 1
    lx1[lx1 == nx] <- nx - 1
    ly1[ly1 == ny] <- ny - 1

    z <- obj$z[cbind(lx1, ly1)] * (1 - ex) * (1 - ey) +
         obj$z[cbind(lx1 + 1, ly1)] * ex * (1 - ey) +
         obj$z[cbind(lx1, ly1 + 1)] * (1 - ex) * ey +
         obj$z[cbind(lx1 + 1, ly1 + 1)] * ex * ey

    names(grid.list) <- c('x', 'y')
    out <- c(grid.list, list(z = matrix(z, nx0, ny0)))
    return(out)
}

##############################################

## Aggregate spatial data
cdt.aggregate.grid <- function(obj, grid.list, FUN = mean, ...)
{
    old.grid <- defSpatialPixels(obj[c('lon', 'lat')])
    new.grid <- defSpatialPixels(grid.list)

    dim.grid <- new.grid@grid@cells.dim
    names(grid.list) <- c('x', 'y')
    out <- c(grid.list, list(z = matrix(NA, dim.grid[1], dim.grid[2])))

    ixy <- over(old.grid, new.grid)
    z.out <- tapply(c(obj$z), ixy, FUN, ...)
    z.out[is.nan(z.out) | is.infinite(z.out)] <- NA

    out$z[as.numeric(names(z.out))] <- z.out
    return(out)
}

##############################################

## nx and ny for as.image
# x: diff(range( lon or lat ))
nx_ny_as.image <- function(x) round(x / (0.0167323 * x^0.9602))

## copy of fields::as.image
cdt.as.image <- function(pts.val, pts.xy, grid = NULL, nx = 64, ny = 64, weighted = FALSE)
{
    if(is.null(grid)){
        xlim <- range(pts.xy[, 1], na.rm = TRUE)
        ylim <- range(pts.xy[, 2], na.rm = TRUE)
        xlim <- xlim + diff(xlim) * c(-1, 1) * 0.01
        ylim <- ylim + diff(ylim) * c(-1, 1) * 0.01
        grid <- list(lon = seq(xlim[1], xlim[2], length.out = nx),
                     lat = seq(ylim[1], ylim[2], length.out = ny))
    }
    xy <- do.call(expand.grid, grid)
    ijGrd <- grid2pointINDEX(list(lon = pts.xy[, 1], lat = pts.xy[, 2]), grid)
    out <- list(x = grid$lon, y = grid$lat, z = matrix(NA, length(grid$lon), length(grid$lat)))

    ij <- !is.na(pts.val)
    pts.val <- pts.val[ij]
    if(length(pts.val) == 0) return(out)
    pts.xy <- pts.xy[ij, , drop = FALSE]
    ijGrd <- ijGrd[ij]
    idx <- split(seq_along(ijGrd), ijGrd)

    if(any(sapply(idx, length) > 1)){
        w <- rep(1, length(ijGrd))
        if(weighted){
            idup <- duplicated(ijGrd) | duplicated(ijGrd, fromLast = TRUE)
            stn.grd <- xy[ijGrd, ]
            dist <- 1 / ((stn.grd[idup, 1] - pts.xy[idup, 1])^2 + (stn.grd[idup, 2] - pts.xy[idup, 2])^2)
            dist[is.infinite(dist)] <- 2 * max(dist[!is.infinite(dist)])
            w[idup] <- dist
        }
        val <- sapply(idx, function(j) sum(w[j] * pts.val[j]) / sum(w[j]))
    }else val <- pts.val[unlist(idx)]
    ij <- as.numeric(names(idx))
    out$z[ij] <- val
    return(out)
}
