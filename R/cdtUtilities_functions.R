
## check OS type
WindowsOS <- function() if(Sys.info()["sysname"] == "Windows") TRUE else FALSE
MacOSXP <- function() if(Sys.info()["sysname"] %in% c("darwin", "Darwin")) TRUE else FALSE
LinuxOS <- function() if(Sys.info()["sysname"] == "Linux") TRUE else FALSE

##############################################

## Test Internet connection
testConnection <- function(url = "https://cran.r-project.org") {
    if(!as.logical(capabilities(what = "http/ftp"))) return(FALSE)
    test <- try(suppressWarnings(readLines(url, n = 1)), silent = TRUE)
    ifelse(inherits(test, "try-error"), FALSE, TRUE)
}

##############################################

## Check the Dynamic HTML Help System
is.HelpServerRunning <- function(){
    ifelse(R.version['svn rev'] < 67550 | getRversion() < "3.2.0", 
            get("httpdPort", envir = environment(tools::startDynamicHelp)) > 0,
            tools::startDynamicHelp(NA) > 0)
}

##############################################

## Reshape data.frame XYZ to matrix list(x, y, z = matrix)
reshapeXYZ2Matrix <- function(df){
    df <- as.data.frame(df, stringsAsFactors = FALSE)
    names(df) <- c('x', 'y', 'z')
    x <- sort(unique(df$x))
    y <- sort(unique(df$y))
    z <- reshape2::acast(df, x~y, value.var = "z")
    dimnames(z) <- NULL
    return(list(x = x, y = y, z = z))
}

##############################################

## reshape cbind(i, j, k) indices to matrix
reshape.array <- function(dat){
    dat <- as.data.frame(dat)
    names(dat) <- c('x', 'y', 'z')
    z <- matrix(NA, nrow = max(dat$x), ncol = max(dat$y))
    z[cbind(dat$x, dat$y)] <- as.character(dat$z)
    return(as.data.frame(z))
}

##############################################

## transpose netcdf data from (y, x) to (x, y)
transposeNCDFData <- function(x, ncinfo){
    if(ncinfo$ilon < ncinfo$ilat)
        x[ncinfo$xo, ncinfo$yo]
    else
        t(x)[ncinfo$xo, ncinfo$yo]
}

transposeNCDFData.inv <- function(x, ncinfo){
    if(ncinfo$ilon < ncinfo$ilat)
        x[ncinfo$xo, ncinfo$yo]
    else
        t(x[ncinfo$xo, ncinfo$yo])
}

##############################################
## convert matrix to numeric, character, trim, ...
## by keeping the dimension
## mat <- apply(mat, 2, as.character)
convert_data_type <- function(mat, as.fun){
    dim.mat <- dim(mat)
    mat <- as.fun(mat)
    dim(mat) <- dim.mat
    return(mat)
}

##############################################

## Test if the elements of two vectors are equals
isEquals <- function(x, y){
    ix <- (x == y) | (is.na(x) & is.na(y))
    ix[is.na(ix)] <- FALSE
    ix
}

## Test if two vectors are equals
isEqual <- function(x, y) !any(!isEquals(x, y))

## Test if all elements of a vector are equals
elEqual <- function(x){
    x <- x[!is.na(x)]
    if(length(x)) all(x == x[1]) else TRUE
}

##############################################

## Test if all elements of a vector or list are equals
all.equal.elements <- function(v){
    is.eq <- sapply(as.list(v[-1]), function(z) isTRUE(all.equal(z, v[1])))
    all(is.eq)
}

all.equal.elements.num <- function(x, tol = .Machine$double.eps ^ 0.5){
    # isTRUE(all.equal(max(x, na.rm = TRUE), min(x, na.rm = TRUE), tolerance = tol))
    # abs(max(x, na.rm = TRUE) - min(x, na.rm = TRUE)) < tol
    diff(range(x, na.rm = TRUE)) < tol
}

##############################################

## Define spatialPixels
defSpatialPixels <- function(grd_Coords, projCRS = CRS(as.character(NA)), regrid = FALSE)
{
    if(regrid){
        x <- grd_Coords$lon
        xrg <- diff(range(diff(x)))
        if(xrg > 0.0001){
            xr <- range(x)
            x <- seq(xr[1], xr[2], length.out = length(x))
        }
        y <- grd_Coords$lat
        yrg <- diff(range(diff(y)))
        if(yrg > 0.0001){
            yr <- range(y)
            y <- seq(yr[1], yr[2], length.out = length(y))
        }

        grd0 <- expand.grid(lon = x, lat = y)
        coordinates(grd0) <- ~lon+lat
        grd <- SpatialPixels(points = grd0, tolerance = 0.0002, proj4string = projCRS)
    }else{
        grd0 <- expand.grid(lon = grd_Coords$lon, lat = grd_Coords$lat)
        coordinates(grd0) <- ~lon+lat

        foo <- function(tol) SpatialPixels(points = grd0, tolerance = tol, proj4string = projCRS)
        grd <- try(foo(sqrt(sqrt(.Machine$double.eps))), silent = TRUE)
        if(inherits(grd, "try-error")) grd <- foo(0.005)
    }

    return(grd)
}

##############################################

## Get index of points at grid
grid2pointINDEX <- function(pts_Coords, grd_Coords, projCRS = CRS(as.character(NA)), regrid = FALSE)
{
    newgrid <- defSpatialPixels(grd_Coords, projCRS, regrid)
    pts.loc <- data.frame(lon = pts_Coords$lon, lat = pts_Coords$lat)
    pts.loc <- SpatialPoints(pts.loc)
    ijGrd <- unname(over(pts.loc, geometry(newgrid)))
    return(ijGrd)
}

##############################################

## Compare if 2 SpatialPixelsObjects have the same resolution
is.diffSpatialPixelsObj <- function(SP1, SP2, tol = 1e-07)
{
    SP1CelldX <- SP1@grid@cellsize[1]
    SP1CelldY <- SP1@grid@cellsize[2]
    SP1CellSX <- SP1@grid@cells.dim[1]
    SP1CellSY <- SP1@grid@cells.dim[2]
    SP2CelldX <- SP2@grid@cellsize[1]
    SP2CelldY <- SP2@grid@cellsize[2]
    SP2CellSX <- SP2@grid@cells.dim[1]
    SP2CellSY <- SP2@grid@cells.dim[2]
    unname(
            abs(SP1CelldX - SP2CelldX) > tol |
            (SP1CellSX != SP2CellSX) |
            abs(SP1CelldY - SP2CelldY) > tol |
            (SP1CellSY != SP2CellSY)
        )
}

########################################

## Create parallel loop
cdt.doparallel <- function(condition, dopar = TRUE, detect.cores = TRUE, nb.cores = 2)
{
    okpar <- FALSE
    if(dopar){
        cores <- parallel::detectCores()
        if(detect.cores){
            nb.cores <- cores - 1
            okpar <- if(nb.cores >= 2) TRUE else FALSE
        }else{
            okpar <- if(cores >= 2 && nb.cores >= 2) TRUE else FALSE
        }
    }

    if(okpar & condition){
        klust <- parallel::makeCluster(nb.cores)
        # doParallel::registerDoParallel(klust)
        doSNOW::registerDoSNOW(klust)
        `%dofun%` <- foreach::`%dopar%`
        closeklust <- TRUE
    }else{
        klust <- NULL
        `%dofun%` <- foreach::`%do%`
        closeklust <- FALSE
    }

    list(dofun = `%dofun%`, cluster = klust, parLL = closeklust)
}

###########

doparallel.cond <- function(condition,
                            parll = .cdtData$Config[c('dopar', 'detect.cores', 'nb.cores')])
{
    c(condition = condition, parll)
}

###########

utils::globalVariables(c('jloop'))

cdt.foreach <- function(loopL, parsL, GUI = TRUE, progress = FALSE, ..., FUN)
{
    FUN <- match.fun(FUN)
    if(missing(parsL)) parsL <- list(condition = FALSE)
    is.parallel <- do.call(cdt.doparallel, parsL)

    tclvalue(.cdtEnv$tcl$status$pbnmax) <- length(loopL)

    if(progress){
        if(GUI){
            tclvalue(.cdtEnv$tcl$status$pbLab) <- "0 %"
            tclvalue(.cdtEnv$tcl$status$pbBar) <- 0
        }

        progress.fun <- if(GUI) updateProgressBar else updateProgressText
        opts <- list(progress = progress.fun)
    }else opts <- list(progress = NULL)

    if(is.parallel$parLL){
        on.exit(parallel::stopCluster(is.parallel$cluster))
        `%parLoop%` <- is.parallel$dofun
        ret.loop <- foreach::foreach(jloop = loopL, .options.snow = opts, ...) %parLoop% FUN(jloop)
    }else{
        opts <- list(...)
        ret.loop <- lapply(loopL, function(jloop){
            ret <- FUN(jloop)
            if(progress) progress.fun(jloop)
            return(ret)
        })
    }

    return(ret.loop)
}

##############################################

sort.filename.data <- function(X){
    xo <- lapply(seq(ncol(X)), function(i) sort(unique(X[, i])))
    xo <- do.call(expand.grid, xo)
    xo <- apply(xo, 1, paste, collapse = ".")
    X <- apply(X, 1, paste, collapse = ".")
    xo <- xo[xo %in% X]
    match(xo, X)
}

##############################################

## Get boundaries from shapefile
getBoundaries <- function(shpf){
    # shpf <- rgeos::gSimplify(shpf, 0.001, topologyPreserve = TRUE)
    ocrds <- matrix(NA, nrow = 1, ncol = 2)
    if(!is.null(shpf)){
        retPolygon <- lapply(slot(shpf, "polygons"),
                             function(i) slot(i, "Polygons"))
        polys <- lapply(retPolygon, function(x){
            ret <- NULL
            for(i in seq_along(x)){
                poly <- rbind(slot(x[[i]], "coords"), cbind(NA, NA))
                ret <- rbind(ret, poly)
            }
            ret
        })
        ocrds <- do.call(rbind, polys)
    }
    return(ocrds)
}

## create mask, blanking
# ncgrid: named list(lon, lat)
create.mask.grid <- function(shp, ncgrid){
    width <- mean(diff(sapply(ncgrid, range)) / (sapply(ncgrid, length) - 1))
    shp <- rgeos::gSimplify(shp, width/4, topologyPreserve = TRUE)
    shp <- rgeos::gBuffer(shp, width = width/2)
    slot.shp <- slot(shp, "polygons")
    shp.df <- data.frame(vtmp = rep(1, length(slot.shp)))
    row.names(shp.df) <- sapply(slot.shp, function(x) slot(x, "ID"))
    shp <- SpatialPolygonsDataFrame(shp, shp.df)
    mask <- over(defSpatialPixels(ncgrid), shp)[, 'vtmp']
    dim(mask) <- sapply(ncgrid, length)
    return(mask)
}

##############################################

## convert km to degree
km2deg <- function(km, lat){
    R <- sqrt((1/110.54)^2 + (1 / (111.32 * cos(lat * pi/180)))^2)
    return(km * R / sqrt(2))
}

##############################################

## append two lists by name, concatenate
append.list <- function (l1, l2)
{
    stopifnot(is.list(l1), is.list(l2))
    lnames <- names(l1)
    for(v in names(l2)){
        if(v %in% lnames && is.list(l1[[v]]) && is.list(l2[[v]]))
            l1[[v]] <- append.list(l1[[v]], l2[[v]])
        else
            l1[[v]] <- c(l1[[v]], l2[[v]])
    }
    return(l1)
}

##############################################

## number and fraction of non-missing values
## by considering stations reporting period (first and last non-missing)

available.data.fraction <- function(data.mat){
    INA <- lapply(seq(ncol(data.mat)), function(j){
        x <- data.mat[, j]
        ina <- !is.na(x)
        ix <- which(ina)
        if(length(ix) == 0)
            return(c(length = 0, frac = 0))
        ix <- ix[1]:ix[length(ix)]
        x <- x[ix]
        ina <- ina[ix]
        n <- length(x)
        fr <- sum(ina) / n
        c(length = n, frac = fr)
    })

    as.list(data.frame(do.call(rbind, INA)))
}

##############################################

## Transpose matrix
transPose <- function(X){
    if(is.null(dim(X))) matrix(X, ncol = 1) else t(X)
}

##############################################

## compute slope aspect
raster.slope.aspect <- function(dem){
    dem <- raster::raster(dem)
    slope <- raster::terrain(dem, opt = "slope", unit = 'degrees', neighbors = 8) 
    aspect <- raster::terrain(dem, opt = "aspect", unit = 'degrees', neighbors = 8) 
    slope <- raster::as.matrix(slope)
    slope <- t(slope)
    revCol <- rev(seq(ncol(slope)))
    slope <- slope[, revCol]
    aspect <- raster::as.matrix(aspect)
    aspect <- t(aspect)
    revCol <- rev(seq(ncol(aspect)))
    aspect <- aspect[, revCol]
    list(slope = slope, aspect = aspect)
}

###########################################

## gstat block size
createBlock <- function(cellsize, fac = 0.5, len = 4){
    sDX <- cellsize[1]*fac
    dBX <- seq(-sDX, sDX, length.out = len)
    sDY <- cellsize[2] * fac
    dBY <- seq(-sDY, sDY, length.out = len)
    bGrd <- expand.grid(x = dBX, y = dBY)
    return(bGrd)
}

###########################################

smooth.matrix <- function(mat, ns){
    res <- cdt.matrix.mw(mat, ns, ns, mean, na.rm = TRUE)
    res[is.nan(res)] <- NA
    return(res)
}

# Calculate moving window values for the neighborhood of a center grid
cdt.matrix.mw <- function(x, sr, sc, fun, ...){
    fun <- match.fun(fun)
    nr <- nrow(x)
    nc <- ncol(x)
    res <- x * NA
    for(j in 1:nc){
        for(i in 1:nr){
            ir <- i + (-sr:sr)
            ir <- ir[ir > 0 & ir <= nr]
            ic <- j + (-sc:sc)
            ic <- ic[ic > 0 & ic <= nc]
            res[i, j] <- fun(c(x[ir, ic]), ...)
        }
    }
    return(res)
}

cdt.2matrices.mv <- function(x, y, sr, sc, fun, ...){
    stopifnot(dim(x) == dim(y))
    fun <- match.fun(fun)
    nr <- nrow(x)
    nc <- ncol(x)
    res <- x * NA
    for(j in 1:nc){
        for(i in 1:nr){
            ir <- i + (-sr:sr)
            ir <- ir[ir > 0 & ir <= nr]
            ic <- j + (-sc:sc)
            ic <- ic[ic > 0 & ic <= nc]
            res[i, j] <- fun(c(x[ir, ic]), c(y[ir, ic]), ...)
        }
    }
    return(res)
}

########################################

split_path <- function(path){
    # https://stackoverflow.com/q/29214932
    if (dirname(path) %in% c(".", path)) return(basename(path))
    return(c(basename(path), split_path(dirname(path))))
}

########################################

set.hour.minute <- function(intstep, minhour){
    if(intstep %in% c("minute", "hourly")){
        minhourVAL <- switch(intstep,
                               "minute" = c(5, 10, 15, 30),
                               "hourly" = c(1, 3, 6, 12)
                             )
        if(is.na(minhour)){
            minhour <- minhourVAL[1]
        }else{
            if(!minhour %in% minhourVAL)
                minhour <- minhourVAL[1]
        }

        state <- "normal"
    }else{
        minhourVAL <- ""
        minhour <- minhour
        state <- "disabled"
    }

    list(cb = minhourVAL, val = minhour, state = state)
}
