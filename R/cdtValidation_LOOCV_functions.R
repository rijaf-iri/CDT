
select.Station.Validation <- function(df, perc = 20){
    dst <- stats::dist(df[, c('lon', 'lat')], method = "euclidean")
    dmat <- as.matrix(dst)
    hklust <- stats::hclust(dst, method = 'average')

    nl <- length(df$id)
    n0 <- round(nl * perc/100)
    minSize <- floor(nl / n0)
    repeat{
        df$k <- dynamicTreeCut::cutreeDynamic(hklust, minClusterSize = minSize, method = "hybrid",
                                                distM = dmat, deepSplit = 4, verbose = 0)
        nClust <- length(unique(df$k))
        minSize <- minSize - 1
       if(minSize < 3) break
       if(n0 <= nClust) break
    }

    ipt <- lapply(seq(nClust), function(j){
        ij <- df$k == j
        io <- order(df$k[ij], decreasing = TRUE)
        which(ij)[io[1:2]]
    })
    ipt <- sort.int(do.call(c, ipt))
    dp <- df[ipt, , drop = FALSE]

    dmat <- dmat[ipt, ipt]
    dimnames(dmat) <- list(seq(nrow(dmat)), seq(ncol(dmat)))
    ir <- sample.int(nrow(dmat), nClust)
    repeat{
        ir0 <- ir
        for (i in 1:nClust){
            mm <- dmat[ir[-i], -ir[-i], drop = FALSE]
            k <- which.max(mm[(1:ncol(mm) - 1) * nrow(mm) + max.col(t(-mm))])
            ir[i] <- as.numeric(dimnames(mm)[[2]][k])
        }
        if(identical(ir0, ir)) break
    }

    dp[ir, c("id", 'lon', 'lat'), drop = FALSE]
}

cdtMergingLOOCV <- function(stnData, stnVID, ncInfo, xy.grid, params,
                            variable, demData, outdir, GUI = TRUE)
{
    log.file <- file.path(outdir, "log_file.txt")
    ncinfo <- ncInfo$ncinfo
    params$MRG$negative <- switch(variable, "rain" = FALSE, "temp" = TRUE)

    ##################
    tmpdir <- file.path(outdir, "tmp")
    dir.create(tmpdir, showWarnings = FALSE, recursive = TRUE)

    ##################

    stnInfo <- lapply(stnData[c("id", "lon", "lat")], function(x) x[stnVID])
    stnInfo <- do.call(rbind, stnInfo)
    stnInfo <- cbind(c("ID", "LON", "DATE/LAT"), stnInfo)

    stndat <- stnData$data[which(stnData$date %in% ncInfo$dates), stnVID, drop = FALSE]
    stndat <- cbind(ncInfo$dates, stndat)
    stndat <- rbind(stnInfo, stndat)
    outstnf <- file.path(outdir, "STATIONS_DATA.csv")
    write.table(stndat, outstnf, sep = ",", na = "-99", col.names = FALSE, row.names = FALSE, quote = FALSE)

    ##################

    newgrid <- defSpatialPixels(xy.grid)

    nmin <- ceiling(params$MRG$pass * params$interp$nmin)
    nmax <- ceiling(params$MRG$pass * params$interp$nmax)
    nmax <- ifelse(nmax - nmin < 2, nmax + 2, nmax)
    params$interp$nmin <- nmin
    params$interp$nmax <- nmax

    if(!params$interp$vargrd){
        maxdist <- params$MRG$pass * params$interp$maxdist
        params$interp$maxdist <- maxdist
    }else{
        bx <- diff(sapply(stnData[c('lon', 'lat')], range))
        # dg <- sqrt(bx[1]^2 + bx[2]^2) / 4
        dg <- sqrt(bx[1]^2 + bx[2]^2)
        dg <- 0.08 * dg + 0.199
        params$interp$maxdist <- params$MRG$pass * dg
    }

    locations.stn <- as.data.frame(stnData[c('lon', 'lat')])
    coordinates(locations.stn) <- c('lon', 'lat')
    ijs <- over(locations.stn, newgrid)
    locations.stn$stn <- rep(NA,  length(locations.stn))

    xy.data <- defSpatialPixels(ncinfo[c('lon', 'lat')])
    # ijgs <- over(locations.stn, xy.data)

    ##################

    is.regridNCDF <- is.diffSpatialPixelsObj(newgrid, xy.data, tol = 1e-07)
    ijnc <- NULL
    if(is.regridNCDF) ijnc <- over(newgrid, xy.data)

    ##################

    is.auxvar <- rep(FALSE, 5)
    if(params$MRG$method == "RK"){
        auxvar <- c('dem', 'slp', 'asp', 'alon', 'alat')
        is.auxvar <- unlist(params$auxvar[1:5])
        if(any(is.auxvar)){
            formuleRK <- formula(paste0('stn', '~', 'grd', '+',
                                 paste(auxvar[is.auxvar], collapse = '+')))
        }else{
            formuleRK <- formula(paste0('stn', '~', 'grd'))
        }

        if(is.auxvar['dem']) newgrid$dem <- c(demData$z)
        if(is.auxvar['slope'] | is.auxvar['aspect']){
            slpasp <- raster.slope.aspect(demData)
            if(is.auxvar['slope']) newgrid$slp <- c(slpasp$slope)
            if(is.auxvar['aspect']) newgrid$asp <- c(slpasp$aspect)
        }
        if(is.auxvar['lon']) newgrid$alon <- newgrid@coords[, 'lon']
        if(is.auxvar['lat']) newgrid$alat <- newgrid@coords[, 'lat']
        if(any(is.auxvar))
            locations.stn@data <- newgrid@data[ijs, , drop = FALSE]
    }

    ##################

    args <- methods::formalArgs(cdtMergingLOOCV)
    for(v in args) assign(v, get(v), envir = environment())

    mrgOpts <- merging.options()

    parsL <- doparallel.cond(length(ncInfo$ncfiles) > 10)
    ret <- cdt.foreach(seq_along(ncInfo$ncfiles), parsL, GUI,
                       progress = TRUE, .packages = "sp",
                       FUN = function(jj)
    {
        if(ncInfo$exist[jj]){
            nc <- ncdf4::nc_open(ncInfo$ncfiles[jj])
            nc.val <- ncdf4::ncvar_get(nc, varid = ncinfo$varid)
            ncdf4::nc_close(nc)
            nc.val <- transposeNCDFData(nc.val, ncinfo)
        }else{
            cat(paste(ncInfo$dates[jj], ":", "no netcdf data", "|",
                "no cross-validation performed", "\n"), file = log.file, append = TRUE)
            return(NULL)
        }

        if(all(is.na(nc.val))){
            cat(paste(ncInfo$dates[jj], ":", "all netcdf data are missing", "|",
                "no cross-validation performed", "\n"), file = log.file, append = TRUE)
            return(-1)
        }

        ######

        # locations.stn$grd <- nc.val[ijgs]
        newgrid$grd <- if(is.null(ijnc)) c(nc.val) else nc.val[ijnc]

        donne.stn <- stnData$data[which(stnData$date == ncInfo$dates[jj]), , drop = FALSE]
        if(nrow(donne.stn) == 0){
            cat(paste(ncInfo$dates[jj], ":", "no station data", "|",
                "no cross-validation performed", "\n"), file = log.file, append = TRUE)
            return(NULL)
        }

        locations.stn$stn <- as.numeric(donne.stn[1, ])
        noNA <- !is.na(locations.stn$stn)
        locations.stn <- locations.stn[noNA, ]

        ijv <- which(which(noNA) %in% stnVID)
        ijout <- ijs[noNA][ijv]

        if(length(locations.stn) < mrgOpts$mrgMinNumberSTN){
            cat(paste(ncInfo$dates[jj], ":", "not enough station data", "|",
                "no cross-validation performed", "\n"), file = log.file, append = TRUE)
            return(NULL)
        }

        if(params$MRG$method == "RK" & any(is.auxvar)){
            loc.data <- !is.na(locations.stn@data)
            loc.data <- split(loc.data, col(loc.data))
            nna <- Reduce("&", loc.data)
            if(length(which(nna)) < mrgOpts$rkMinNumberSTN){
                cat(paste(ncInfo$dates[jj], ":", "not enough spatial points data", "|",
                    "no cross-validation performed", "\n"), file = log.file, append = TRUE)
                return(NULL)
            }
        }

        ######

        xstn <- lapply(seq_along(ijv), function(ii){
            loc.stn <- locations.stn[-ijv[ii], ]
            out.mrg <- merging.functions(loc.stn, newgrid, params,
                                         formuleRK, ncInfo$dates[jj],
                                         log.file, mrgOpts)

            out.mrg[ijout[ii]]
        })

        out <- rep(NA, length(stnVID))
        ix <- which(stnVID %in% as.integer(names(ijout)))
        out[ix] <- do.call(c, xstn)
        out <- round(out, 1)

        tmpf <- file.path(tmpdir, paste0(ncInfo$dates[jj], '.txt'))
        tmp <- matrix(c(ncInfo$dates[jj], out), nrow = 1)
        write.table(tmp, tmpf, sep = ',', na = "-99", col.names = FALSE, row.names = FALSE, quote = FALSE)

        return(out)
    })

    inull <- sapply(ret, is.null)
    ret[inull] <- NA
    ret <- do.call(rbind, ret)

    ret <- cbind(ncInfo$dates, ret)
    ret <- rbind(stnInfo, ret)
    outcvf <- file.path(outdir, "CROSS-VALIDATION_DATA.csv")
    write.table(ret, outcvf, sep = ",", na = "-99", col.names = FALSE, row.names = FALSE, quote = FALSE)

    unlink(tmpdir, recursive = TRUE)

    if(any(inull)) return(-1)
    return(0)
}

