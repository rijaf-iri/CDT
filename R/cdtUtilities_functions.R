
## check OS type
WindowsOS <- function() if(tolower(Sys.info()["sysname"]) == "windows") TRUE else FALSE
MacOSXP <- function() if(tolower(Sys.info()["sysname"]) == "darwin") TRUE else FALSE
LinuxOS <- function() if(tolower(Sys.info()["sysname"]) == "linux") TRUE else FALSE

##############################################

## Test Internet connection
testConnection <- function(url = "https://www.google.com") {
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

## Load local configuration file
cdtLocalConfigData <- function(){
    cdt.file.conf <- file.path(.cdtDir$dirLocal, "config", "cdt_config.json")
    Config <- jsonlite::fromJSON(cdt.file.conf)
    Config <- rapply(Config, trimws, classes = "character", how = "replace")
    .cdtData$Config <- Config
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
#' @exportS3Method NULL
all.equal.elements <- function(v){
    is.eq <- sapply(as.list(v[-1]), function(z) isTRUE(all.equal(z, v[1])))
    all(is.eq)
}

#' @exportS3Method NULL
all.equal.elements.num <- function(x, tol = .Machine$double.eps ^ 0.5){
    # isTRUE(all.equal(max(x, na.rm = TRUE), min(x, na.rm = TRUE), tolerance = tol))
    # abs(max(x, na.rm = TRUE) - min(x, na.rm = TRUE)) < tol
    diff(range(x, na.rm = TRUE)) < tol
}

##############################################

## Define spatialPixels
defSpatialPixels <- function(grd_Coords, projCRS = sp::CRS(as.character(NA)), regrid = FALSE)
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
        sp::coordinates(grd0) <- ~lon+lat
        grd <- sp::SpatialPixels(points = grd0, tolerance = 0.0002, proj4string = projCRS)
    }else{
        grd0 <- expand.grid(lon = grd_Coords$lon, lat = grd_Coords$lat)
        sp::coordinates(grd0) <- ~lon+lat

        foo <- function(tol) sp::SpatialPixels(points = grd0, tolerance = tol, proj4string = projCRS)
        grd <- try(foo(sqrt(sqrt(.Machine$double.eps))), silent = TRUE)
        if(inherits(grd, "try-error")) grd <- foo(0.005)
    }

    return(grd)
}

defRegularGrid <- function(grd_Coords, projCRS = sf::NA_crs_, regrid = FALSE)
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

        nxy <- c(length(x), length(y))
        grd0 <- expand.grid(lon = x, lat = y)
    }else{
        nxy <- sapply(grd_Coords, length)
        grd0 <- expand.grid(lon = grd_Coords$lon, lat = grd_Coords$lat)
    }

    grd <- sf::st_as_sf(grd0, coords = c("lon", "lat"), dim = "XYZ")
    grd <- sf::st_make_grid(grd, n = nxy, what = "centers")
    sf::st_crs(grd) <- projCRS

    return(grd)
}

##############################################

## Get index of points at grid
grid2pointINDEX <- function(pts_Coords, grd_Coords,
                            projCRS = sp::CRS(as.character(NA)),
                            regrid = FALSE)
{
    newgrid <- defSpatialPixels(grd_Coords, projCRS, regrid)
    pts.loc <- data.frame(lon = pts_Coords$lon, lat = pts_Coords$lat)
    pts.loc <- sp::SpatialPoints(pts.loc)
    ijGrd <- unname(sp::over(pts.loc, sp::geometry(newgrid)))
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

doparallel.cond <- function(condition, parll = .cdtData$Config$parallel)
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

    ###########
    ### use of .cdtEnv$tcl$status$pbnmax
    if(progress){
        tclvalue(.cdtEnv$tcl$status$pbnmax) <- length(loopL)

        if(GUI){
            tclvalue(.cdtEnv$tcl$status$pbLab) <- "0 %"
            tclvalue(.cdtEnv$tcl$status$pbBar) <- 0
            progress.fun <- updateProgressBar
        }else{
            progress.fun <- updateProgressText
        }

        opts <- list(progress = progress.fun)
    }else opts <- list(progress = NULL)

    ###########
    ### no need of .cdtEnv$tcl$status$pbnmax, it can be removed
    # if(progress){
    #     pbnmax <- length(loopL)

    #     if(GUI){
    #         tclvalue(.cdtEnv$tcl$status$pbLab) <- "0 %"
    #         tclvalue(.cdtEnv$tcl$status$pbBar) <- 0
    #         progress.fun <- function(n){
    #             newvalue <- 100 * n / pbnmax
    #             newlabel <- sprintf("%.f %%", round(newvalue))
    #             tclvalue(.cdtEnv$tcl$status$pbLab) <- newlabel
    #             tclvalue(.cdtEnv$tcl$status$pbBar) <- newvalue
    #             tcl("update", "idletask")
    #         }
    #     }else{
    #         progress.fun <- function(n){
    #             newvalue <- 100 * n / pbnmax
    #             cat(sprintf("Task %.f done; %.1f %% completed\n", n, newvalue))
    #         }
    #     }

    #     opts <- list(progress = progress.fun)
    # }else opts <- list(progress = NULL)

    ###########

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

#' @exportS3Method NULL
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
    coords <- matrix(NA, nrow = 1, ncol = 2)
    if(!is.null(shpf)){
        st_geom <- sf::st_geometry(shpf)
        coords <- lapply(st_geom, function(x){
            ret <- lapply(x, function(s){
                if(is.list(s)){
                    do.call(rbind, lapply(s, rbind, cbind(NA, NA)))
                }else{
                    rbind(s, cbind(NA, NA))
                }
            })
            do.call(rbind, ret)
        })
        coords <- do.call(rbind, coords)
    }
    return(coords)
}

# getBoundaries <- function(shpf){
#     # shpf <- rgeos::gSimplify(shpf, 0.001, topologyPreserve = TRUE)
#     ocrds <- matrix(NA, nrow = 1, ncol = 2)
#     if(!is.null(shpf)){
#         retPolygon <- lapply(methods::slot(shpf, "polygons"),
#                              function(i) methods::slot(i, "Polygons"))
#         polys <- lapply(retPolygon, function(x){
#             ret <- NULL
#             for(i in seq_along(x)){
#                 poly <- rbind(methods::slot(x[[i]], "coords"), cbind(NA, NA))
#                 ret <- rbind(ret, poly)
#             }
#             ret
#         })
#         ocrds <- do.call(rbind, polys)
#     }
#     return(ocrds)
# }

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
    slope[is.nan(slope)] <- NA
    aspect[is.nan(aspect)] <- NA
    list(slope = slope, aspect = aspect)
}

###########################################

## gstat block size
# createBlock <- function(blockSize = c(1, 0.5, 1, 0.5)){
#     if(length(blockSize) != 4)
#         stop("blockSize must be of length 4 in the form c(width_x, by_x, width_y, by_y)")

#     sX <- blockSize[1]/2
#     lX <- ceiling((blockSize[1]/blockSize[2]) + 1)
#     dBX <- seq(-sX, sX, length.out = lX)
#     sY <- blockSize[3]/2
#     lY <- ceiling((blockSize[3]/blockSize[4]) + 1)
#     dBY <- seq(-sY, sY, length.out = lY)
#     bGrd <- expand.grid(x = dBX, y = dBY)

#     return(bGrd)
# }

createBlock <- function(blockSize = c(1, 0.5, 1, 0.5)){
    if(length(blockSize) != 4)
        stop("blockSize must be of length 4 in the form c(width_x, by_x, width_y, by_y)")
    if(blockSize[1] < blockSize[2])
        stop("width_x must be greater than by_x")
    if(blockSize[3] < blockSize[4])
        stop("width_y must be greater than by_y")

    sX <- blockSize[1]/2
    lX <- ceiling((blockSize[1]/blockSize[2]) + 1)
    dBX <- seq(-sX, sX, length.out = lX)
    sY <- blockSize[3]/2
    lY <- ceiling((blockSize[3]/blockSize[4]) + 1)
    dBY <- seq(-sY, sY, length.out = lY)
    bGrd <- expand.grid(x = dBX, y = dBY)
    bGrd <- rbind(bGrd, c(0, 0))
    bGrd <- bGrd[!duplicated(bGrd), ]

    return(bGrd)
}

gaussBlock <- function(blockSize = c(1, 1)){
    gauss_n = c(-0.4305681558, -0.1699905218,
                 0.1699905218, 0.4305681558) 
    gauss_w = c(0.1739274226, 0.3260725774,
                0.3260725774, 0.1739274226)

    nblock <- length(blockSize)
    if(nblock < 2)
        stop("The length of blockSize must be 2 or 3")

    d <- sweep(replicate(nblock, gauss_n), 2, rev(blockSize), '*')
    d <- expand.grid(split(d, col(d)))
    if(nblock == 2) d <- cbind(0, d)
    names(d) <- c('z', 'y', 'x')

    w <- expand.grid(rep(list(gauss_w), nblock))
    w <- Reduce('*', w)

    list(d = d, w = w, n = nblock)
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
                               "minute" = c(5, 10, 15, 20, 30),
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

########################################

fraction <- function(x, d){
    paste0(round(x * d), "/", d)
}

########################################

init.default.list.args <- function(inupt_args, init_pars){
    pars_name <- names(init_pars)
    data_name <- names(inupt_args)
    inm <- data_name %in% pars_name
    if(any(inm)){
        for(n in data_name[inm])
            init_pars[[n]] <- inupt_args[[n]]
    }

    return(init_pars)
}

########################################

extract_filename_dates <- function(filenames, fileformat){
    expr <- gregexpr('%', fileformat)[[1]]
    # len <- attr(expr, 'match.length') + 1
    len <- rep(2, length(expr))
    ret <- NULL
    if(expr[1] != -1){
        re <- FALSE
        ss <- 1
        se <- nchar(fileformat)
        nl <- length(expr)
        for(i in 1:nl){
            re <- c(re, TRUE, FALSE)
            ss <- c(ss, expr[i], expr[i] + len[i])
            j <- nl - i + 1
            se <- c(expr[j] - 1, expr[j] + len[j] - 1, se)
        }

        res <- lapply(seq_along(re), function(i){
            v <- substr(fileformat, ss[i], se[i])
            if(v == "") v <- NULL
            if(re[i]) v <- NULL
            v
        })

        inul <- sapply(res, is.null)
        if(!all(inul)){
            res <- do.call(c, res[!inul])
            res <- res[!duplicated(res)]
            res <- double_backslash_non_alnum(res)
            pattern <- paste0(res, collapse = '|')
            ret <- gsub(pattern, '', filenames)
        }
    }

    return(ret)
}

extract_filename_dates.1 <- function(filenames, fileformat){
    txt <- strsplit(fileformat, "%s")[[1]]
    txt <- txt[txt != ""]
    ret <- gsub(txt[1], "", filenames)
    if(length(txt) > 1){
        for(j in 2:length(txt))
            ret <- gsub(txt[j], "", ret)
    }

    return(ret)
}

########################################

double_backslash_non_alnum <- function(strings){
    for(i in seq_along(strings)){
        expr <- gregexpr("[^[:alnum:]]", strings[i])
        ex <- expr[[1]]
        if(ex[1] == -1) next

        chr <- rep('', length(ex))
        for(j in seq_along(chr)){
            chr[j] <- substr(strings[i], ex[j], ex[j])
        }
        chr <- chr[!duplicated(chr)]
        for(v in chr){
            pt0 <- paste0('\\', v)
            pt1 <- paste0('\\', pt0)
            strings[i] <- gsub(pt0, pt1, strings[i])
        }
    }

    return(strings)
}

########################################

intersect_rectangles <- function(rect1, rect2, border = FALSE){
    # rect <- c(xmin, xmax, ymin, ymax)
    xmx <- max(rect1[1], rect2[1])
    ymx <- max(rect1[3], rect2[3])
    xmn <- min(rect1[2], rect2[2])
    ymn <- min(rect1[4], rect2[4])
    no_inter <- (xmx > xmn) || (ymx > ymn)
    inter <- !no_inter
    if(!border){
        area <- (xmn - xmx) * (ymn - ymx)
        inter <- inter && area > 0
    }
    return(inter)
}

########################################

# # (val - 32) * 5/9
# fun_def <- "function(x, a, b){(x - a) * b}"
# fun_args <- "32;5/9"
# val <- 65
# eval_function(fun_def, fun_args, val)

# # val - 273.15
# fun_def <- "-"
# fun_args <- "273.15"
# val <- 295.3
# eval_function(fun_def, fun_args, val)

eval_function <- function(fun_def, fun_args, val){
    if(grepl("function", fun_def)){
        eval(parse(text = paste("tmp_fun <- ", fun_def)))
    }else{
        tmp_fun <- get(fun_def, mode = 'function')
    }

    f_args <- strsplit(fun_args, ';')[[1]]
    f_args <- lapply(f_args, function(x) eval(parse(text = x)))
    f_args <- c(list(val), f_args)

    do.call(tmp_fun, f_args)
}

########################################

## tryCatch handling warning message
## do not stop when warning

tryCatch2 <- function(expr, ...){
    warning_msg <- NULL
    res <- tryCatch(
        expr = {
            withCallingHandlers(
                expr = expr,
                warning = function(w){
                    env_fun <- parent.env(environment())
                    env_fun$warning_msg <- w
                } 
            )
        },
        ...
    )
    if(!is.null(warning_msg)) warningFun(warning_msg)

    return(res)
}