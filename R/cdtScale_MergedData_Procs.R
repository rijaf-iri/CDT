
exec_ScalingUpData <- function(){
    message <- .cdtData$GalParams[['message']]
    outdir <- file.path(.cdtData$GalParams$outdir, "Merged_ScaledData")
    dir.create(outdir, showWarnings = FALSE, recursive = TRUE)

    #################
    datMrgInfo <- getNCDFSampleData(.cdtData$GalParams$mrg.data$sample)
    if(is.null(datMrgInfo)){
        Insert.Messages.Out(message[['6']], TRUE, 'e')
        return(NULL)
    }

    #################
    ## get nc to be scald up
    ncInfoMrg <- ncInfo.with.date.range(.cdtData$GalParams$mrg.data,
                                        .cdtData$GalParams$date.range,
                                        .cdtData$GalParams$mrg.data$tstep)
    if(is.null(ncInfoMrg)){
        Insert.Messages.Out(message[['7']], TRUE, "e")
        return(NULL)
    }
    ncInfoMrg$ncinfo <- datMrgInfo

    #################

    rfeScaleInfo <- getNCDFSampleData(.cdtData$GalParams$scale.data$sample)
    if(is.null(rfeScaleInfo)){
        Insert.Messages.Out(message[['8']], TRUE, 'e')
        return(NULL)
    }

    #################

    if(.cdtData$GalParams$mrg.data$tstep == 'daily'){
        if(.cdtData$GalParams$scale.data$tstep == 'pentad'){
            pen.s <- cut(.cdtData$GalParams$date.range$start.day,
                          c(1, 5, 10, 15, 20, 25, 31),
                          labels = FALSE, include.lowest = TRUE)
            pen.e <- cut(.cdtData$GalParams$date.range$end.day,
                          c(1, 5, 10, 15, 20, 25, 31),
                          labels = FALSE, include.lowest = TRUE)
            .cdtData$GalParams$date.range$start.pen <- pen.s
            .cdtData$GalParams$date.range$end.pen <- pen.e

        }
        if(.cdtData$GalParams$scale.data$tstep == 'dekadal'){
            dek.s <- cut(.cdtData$GalParams$date.range$start.day,
                          c(1, 10, 20, 31), labels = FALSE, include.lowest = TRUE)
            dek.e <- cut(.cdtData$GalParams$date.range$end.day,
                          c(1, 10, 20, 31), labels = FALSE, include.lowest = TRUE)
            .cdtData$GalParams$date.range$start.dek <- dek.s
            .cdtData$GalParams$date.range$end.dek <- dek.e
        }
    }

    if(.cdtData$GalParams$mrg.data$tstep == 'pentad'){
        if(.cdtData$GalParams$scale.data$tstep == 'dekadal'){
            dek.s <- cut(.cdtData$GalParams$date.range$start.pen,
                         c(1, 2, 4, 6), labels = FALSE, include.lowest = TRUE)
            dek.e <- cut(.cdtData$GalParams$date.range$end.pen,
                         c(1, 2, 4, 6), labels = FALSE, include.lowest = TRUE)

            .cdtData$GalParams$date.range$start.dek <- dek.s
            .cdtData$GalParams$date.range$end.dek <- dek.e
        }
    }

    #######

    ncInfoScale <- ncInfo.with.date.range(.cdtData$GalParams$scale.data,
                                          .cdtData$GalParams$date.range,
                                          .cdtData$GalParams$scale.data$tstep)
    if(is.null(ncInfoScale)){
        Insert.Messages.Out(message[['9']], TRUE, "e")
        return(NULL)
    }
    ncInfoScale$ncinfo <- rfeScaleInfo

    ret <- merged_ScalingUpData(ncInfoMrg = ncInfoMrg, ncInfoScale = ncInfoScale,
                                params = .cdtData$GalParams, outdir = outdir)

    if(!is.null(ret)){
        if(ret == 0) return(0)
        else return(ret)
    } else return(NULL)
}

merged_ScalingUpData <- function(ncInfoMrg, ncInfoScale, params, outdir, GUI = TRUE){
    message <- .cdtData$GalParams[['message']]
    Insert.Messages.Out(message[['10']], TRUE, "i", GUI)
    ## indexing
    if(params$scale.data$tstep == "monthly"){
        ## daily, pentad and dekadal to monthly
        index <- split(seq_along(ncInfoMrg$dates), substr(ncInfoMrg$dates, 1, 6))
        if(params$mrg.data$tstep == "daily"){
            nbd0 <- nb.Day.Of.Month(ncInfoMrg$dates[1])
            nbd1 <- nb.Day.Of.Month(ncInfoMrg$dates[length(ncInfoMrg$dates)])
        }
        if(params$mrg.data$tstep == "pentad"){
            nbd0 <- 6
            nbd1 <- 6
        }
        if(params$mrg.data$tstep == "dekadal"){
            nbd0 <- 3
            nbd1 <- 3
        }
    }else if(params$scale.data$tstep == "dekadal"){
        if(params$mrg.data$tstep == "daily"){
            ## daily to dekadal
            yymm <- substr(ncInfoMrg$dates, 1, 6)
            jour <- as.numeric(substr(ncInfoMrg$dates, 7, 8))
            jour <- cut(jour, c(1, 10, 20, 31), labels = FALSE, include.lowest = TRUE)
            index <- split(seq_along(ncInfoMrg$dates), paste0(yymm, jour))
            nbd0 <- nb.Day.Of.Dekad(ncInfoMrg$dates[1])
            nbd1 <- nb.Day.Of.Dekad(ncInfoMrg$dates[length(ncInfoMrg$dates)])
        }
        if(params$mrg.data$tstep == "pentad"){
            ## pentad to dekadal
            yymm <- substr(ncInfoMrg$dates, 1, 6)
            pen <- as.numeric(substr(ncInfoMrg$dates, 7, 8))
            pen <- cut(pen, c(1, 2, 4, 6), labels = FALSE, include.lowest = TRUE)
            index <- split(seq_along(ncInfoMrg$dates), paste0(yymm, pen))
            nbd0 <- 2
            nbd1 <- 2
        }
    }else{
        ## daily to pentad
        yymm <- substr(ncInfoMrg$dates, 1, 6)
        jour <- as.numeric(substr(ncInfoMrg$dates, 7, 8))
        jour <- cut(jour, c(1, 5, 10, 15, 20, 25, 31), labels = FALSE, include.lowest = TRUE)
        index <- split(seq_along(ncInfoMrg$dates), paste0(yymm, jour))
        nbd0 <- nb.Day.Of.Pentad(ncInfoMrg$dates[1])
        nbd1 <- nb.Day.Of.Pentad(ncInfoMrg$dates[length(ncInfoMrg$dates)])
    }

    #####
    dates.scale <- names(index)
    nbd.in <- nbd <- sapply(index, length)
    nbd[1] <- nbd0
    nbd[length(nbd)] <- nbd1
    ifull <- nbd.in == nbd
    notscaled.dates <- dates.scale[!ifull]
    notscaled.index <- index[!ifull]
    dates.scale <- dates.scale[ifull]
    index <- index[ifull]
    nbd.in <- nbd.in[ifull]

    if(length(notscaled.index) > 0){
        for(j in seq_along(notscaled.index)){
            nc.files0 <- ncInfoMrg$ncfiles[notscaled.index[[j]]]
            nc.exist0 <- ncInfoMrg$exist[notscaled.index[[j]]]
            nc.files0 <- nc.files0[nc.exist0]
            if(length(nc.files0) == 0) next
            file.copy(nc.files0, file.path(params$outdir, basename(nc.files0)))
        }
    }

    #######

    varinfo0 <- ncInfoMrg$ncinfo$varinfo
    ncdim0 <- unlist(ncInfoScale$ncinfo[c('nx', 'ny')])

    #######
    ## check if same grid
    is.diff.grid <- is.diffSpatialPixelsObj(defSpatialPixels(ncInfoMrg$ncinfo),
                                            defSpatialPixels(ncInfoScale$ncinfo),
                                            tol = 1e-07)
    if(is.diff.grid){
        Insert.Messages.Out(message[['11']], TRUE, "e", GUI)
        return(NULL)
    }

    #######
    ## Def ncdf
    dx <- ncdim_def("Lon", "degreeE", ncInfoMrg$ncinfo$lon)
    dy <- ncdim_def("Lat", "degreeN", ncInfoMrg$ncinfo$lat)

    grd.nc.out <- ncvar_def(varinfo0$name, varinfo0$units, list(dx, dy),
                            varinfo0$missval, varinfo0$longname,
                            varinfo0$prec, compression = 9)

    #######

    scale.fun <- switch(params$scale.data$fun, "sum" = colSums, "mean" = colMeans)
    scale.div <- switch(params$scale.data$fun, "sum" = 0.1, "mean" = 0.001)

    #######

    parsL <- doparallel.cond(length(index) >= 100)
    ret <- cdt.foreach(seq_along(index), parsL, GUI,
                       progress = TRUE, .packages = "sp",
                       FUN = function(jj)
    {
        ix <- which(ncInfoScale$dates == dates.scale[[jj]])

        #######
        nc.files <- ncInfoMrg$ncfiles[index[[jj]]]
        nc.exist <- ncInfoMrg$exist[index[[jj]]]
        nc.files <- nc.files[nc.exist]
        if(length(nc.files) == 0) return(NULL)
        if(length(nc.files) != nbd.in[jj] |
           length(ix) == 0 |
           !ncInfoScale$exist[ix])
        {
            file.copy(nc.files, file.path(outdir, basename(nc.files)))
            return(NULL)
        }

        #######

        nc <- nc_open(ncInfoScale$ncfiles[ix])
        scl.don <- ncvar_get(nc, varid = ncInfoScale$ncinfo$varid)
        nc_close(nc)
        scl.don <- transposeNCDFData(scl.don, ncInfoScale$ncinfo)

        #######

        mrg.don <- lapply(seq_along(nc.files), function(j){
            nc <- nc_open(nc.files[j])
            don <- ncvar_get(nc, varid = varinfo0$name)
            nc_close(nc)
            don <- transposeNCDFData(don, ncInfoMrg$ncinfo)
            don
        })

        mrg.aggr <- do.call(rbind, lapply(mrg.don, c))
        mrg.aggr <- scale.fun(mrg.aggr)
        dim(mrg.aggr) <- ncdim0

        scale.mat <- scl.don / (mrg.aggr + scale.div)
        scale.mat[is.na(scale.mat) | is.nan(scale.mat) | is.infinite(scale.mat)] <- 1

        for(j in seq_along(mrg.don)){
            out <- scale.mat * mrg.don[[j]]
            out[is.na(out)] <- varinfo0$missval

            outfl <- file.path(outdir, basename(nc.files[j]))
            nc <- ncdf4::nc_create(outfl, grd.nc.out)
            ncdf4::ncvar_put(nc, grd.nc.out, out)
            ncdf4::nc_close(nc)
        }

        return(0)
    })

    Insert.Messages.Out(message[['12']], TRUE, "s", GUI)

    return(0)
}
