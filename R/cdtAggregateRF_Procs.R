
AggregateMWin_Execute <- function(){
    GUI <- TRUE
    progress <- TRUE
    GalParams <- .cdtData$GalParams
    win <- GalParams$aggr.series$win

    Insert.Messages.Out(GalParams[['message']][['4']], TRUE, "i")

    ############################
    if(GalParams$data.type == 'cdtstation'){
        donne <- getStnOpenData(GalParams$cdtstation)
        if(is.null(donne)) return(NULL)
        capition <- donne[1:4, 1]
        donne <- splitCDTData0(donne)
        if(is.null(donne)) return(NULL)
        miss.val <- getStnOpenDataInfo(GalParams$cdtstation)[[3]]$miss.val

        date.range <- get.date.time.range.cdt.station(donne$dates, GalParams$tstep)
        daty <- get.format.seq.date.time(date.range, GalParams$tstep, GalParams$minhour)
        idaty <- match(daty, donne$dates)
        if(all(is.na(idaty))){
            Insert.Messages.Out(GalParams[['message']][['6']], TRUE, 'e')
            return(NULL) 
        }
        don <- donne$data[idaty, , drop = FALSE]
        don <- do.call(.rollfun.mat, c(list(x = don), GalParams$aggr.series))
        don <- round(don, 4)

        headers <- do.call(rbind, donne[c('id', 'lon', 'lat', 'elv')])
        if(is.null(donne$elv)) capition <- capition[1:3]

        entete <- cbind(capition, headers)
        don <- rbind(entete, cbind(daty, don))
        don[is.na(don)] <- miss.val
        writeFiles(don, GalParams$output)
    }

    ############################
    if(GalParams$data.type == 'cdtdataset'){
        donne <- try(readRDS(GalParams$cdtdataset), silent = TRUE)
        if(inherits(donne, "try-error")){
            Insert.Messages.Out(paste(GalParams[['message']][['7']], GalParams$cdtdataset), TRUE, 'e')
            return(NULL)
        }
        if(GalParams$tstep != donne$TimeStep){
            Insert.Messages.Out(paste(GalParams[['message']][['8']], GalParams$tstep), TRUE, 'e')
            return(NULL)
        }
        # dates <- donne$dateInfo$date

        date.range <- get.date.time.range.cdt.station(donne$dateInfo$date, GalParams$tstep)
        daty <- get.format.seq.date.time(date.range, GalParams$tstep, GalParams$minhour)
        idaty <- match(daty, donne$dateInfo$date)
        if(all(is.na(idaty))){
            Insert.Messages.Out(GalParams[['message']][['6']], TRUE, 'e')
            return(NULL) 
        }

        ######
        dataset.name <- paste0("Aggregated_Rolling_", GalParams$aggr.series$fun)
        outputDIR <- file.path(GalParams$output, dataset.name)
        dataDIR <- file.path(outputDIR, "DATA")
        dir.create(dataDIR, showWarnings = FALSE, recursive = TRUE)
        file.index <- file.path(outputDIR, paste0(dataset.name, ".rds"))

        ######
        index.agg <- donne
        index.agg$dateInfo$date <- daty
        index.agg$dateInfo$index <- seq_along(daty)

        con <- gzfile(file.index, compression = 7)
        open(con, "wb")
        saveRDS(index.agg, con)
        close(con)

        ##########
        chunkfile <- sort(unique(donne$colInfo$index))
        chunkcalc <- split(chunkfile, ceiling(chunkfile / donne$chunkfac))
        cdtParallelCond <- .cdtData$Config[c('dopar', 'detect.cores', 'nb.cores')]

        ##########
        do.parChunk <- if(donne$chunkfac > length(chunkcalc)) TRUE else FALSE
        do.parCALC <- if(do.parChunk) FALSE else TRUE
        parsL <- doparallel.cond(do.parCALC & (length(chunkcalc) > 10))

        ret <- cdt.foreach(seq_along(chunkcalc), parsL, GUI, progress, FUN = function(jj)
        {
            don.data <- readCdtDatasetChunk.sequence(chunkcalc[[jj]], GalParams$cdtdataset, cdtParallelCond, do.par = do.parChunk)
            don.data <- don.data[donne$dateInfo$index, , drop = FALSE]
            don.data <- don.data[idaty, , drop = FALSE]
            don.data <- do.call(.rollfun.mat, c(list(x = don.data), GalParams$aggr.series))
            writeCdtDatasetChunk.sequence(don.data, chunkcalc[[jj]], index.agg, dataDIR, cdtParallelCond, do.par = do.parChunk)
            rm(don.data); gc()
            return(0)
        })
    }

    ############################
    if(GalParams$data.type == 'cdtnetcdf'){
        ncinfo <- ncInfo.with.date.range(GalParams$cdtnetcdf, GalParams$Date.Range,
                                         GalParams$tstep, GalParams$minhour)
        if(!any(ncinfo$exist)){
            Insert.Messages.Out(GalParams[['message']][['9']], TRUE, 'e')
            return(NULL)
        }
        dates <- ncinfo$dates[ncinfo$exist]
        ncPATH <- ncinfo$ncfiles[ncinfo$exist]

        ######
        ncsample <- getNCDFSampleData(GalParams$cdtnetcdf$sample)
        if(is.null(ncsample)){
            Insert.Messages.Out(GalParams[['message']][['10']], TRUE, 'e')
            return(NULL)
        }
        ncINFO <- ncsample[c('ilon', 'ilat', 'varid')]

        ######
        date.range <- get.date.time.range.cdt.station(dates, GalParams$tstep)
        daty <- get.format.seq.date.time(date.range, GalParams$tstep, GalParams$minhour)
        idaty <- match(daty, dates)
        index <- lapply(seq(length(idaty) - win + 1), function(k) seq(k, k + win - 1, 1))
        indexF <- sapply(index, function(x) sum(!is.na(idaty[x])) < GalParams$aggr.series$min.data)

        aggr.fun <- switch(GalParams$aggr.series$fun,
                      "sum" = colSums,
                      "mean" = colMeans,
                      "median" = matrixStats::colMedians,
                      "max" = matrixStats::colMaxs,
                      "min" = matrixStats::colMins,
                      "sd" = matrixStats::colSds
                    )
        ########
        odaty <- sapply(index, function(x){
            xdt <- daty[x]
            switch(GalParams$aggr.series$align,
                   "left" = xdt[1],
                   "right" = xdt[win],
                   "center" = {
                                ii <- floor((win - 1) / 2)
                                xdt[ii + 1]
                              }
                  )
        })

        ######
        dataset.name <- paste0("Aggregated_Rolling_", GalParams$aggr.series$fun)
        outputDIR <- file.path(GalParams$output, dataset.name)
        dir.create(outputDIR, showWarnings = FALSE, recursive = TRUE)

        outnc <- paste0(strsplit(GalParams$cdtnetcdf$format, "%")[[1]][1], odaty, '.nc')
        out.ncfiles <- file.path(outputDIR, outnc)

        ######
        nc <- nc_open(ncPATH[1])
        varid0 <- ncINFO$varid
        xlon0 <- nc$var[[varid0]]$dim[[ncINFO$ilon]]$vals
        xlat0 <- nc$var[[varid0]]$dim[[ncINFO$ilat]]$vals
        units0 <- nc$var[[varid0]]$units
        prec0 <- nc$var[[varid0]]$prec
        missval0 <- nc$var[[varid0]]$missval
        longname0 <- nc$var[[varid0]]$longname
        nc_close(nc)

        ncINFO$xo <- order(xlon0)
        xlon0 <- xlon0[ncINFO$xo]
        ncINFO$yo <- order(xlat0)
        xlat0 <- xlat0[ncINFO$yo]
        xnlon0 <- length(xlon0)
        xnlat0 <- length(xlat0)

        #######
        dx <- ncdim_def("Lon", "degreeE", xlon0)
        dy <- ncdim_def("Lat", "degreeN", xlat0)
        grd.nc.out <- ncvar_def(varid0, units0, list(dx, dy), missval0, longname = longname0, prec = prec0)

        #######
        tmp.mat <- matrix(NA, win, xnlon0 * xnlat0)

        for(jj in seq(win - 1)){
            ix <- idaty[jj]
            if(is.na(ix)) next
            nc <- ncdf4::nc_open(ncPATH[ix])
            don <- ncdf4::ncvar_get(nc, varid = varid0)
            ncdf4::nc_close(nc)
            don <- transposeNCDFData(don, ncINFO)
            tmp.mat[jj, ] <- c(don)
        }

        # ### for loop
        # for(jj in seq_along(index)){
        #     ix <- index[[jj]][win]
        #     ix <- idaty[ix]
        #     don <- rep(NA, xnlon0 * xnlat0)
        #     if(!is.na(ix)){
        #         nc <- ncdf4::nc_open(ncPATH[ix])
        #         don <- ncdf4::ncvar_get(nc, varid = varid0)
        #         ncdf4::nc_close(nc)
        #         don <- transposeNCDFData(don, ncINFO)
        #         don <- c(don)
        #     }

        #     if(jj > 1) tmp.mat[1:(win - 1), ] <- tmp.mat[2:win, ]
        #     tmp.mat[win, ] <- don
        #     if(indexF[jj]) next

        #     don <- aggr.fun(tmp.mat, GalParams$aggr.series$na.rm)
        #     don[is.nan(don) | is.infinite(don)] <- NA
        #     xna <- colSums(!is.na(tmp.mat))
        #     don[xna < GalParams$aggr.series$min.data] <- NA
        #     don[is.na(don)] <- missval0
        #     don <- matrix(don, nrow = xnlon0, ncol = xnlat0)

        #     nc <- ncdf4::nc_create(out.ncfiles[jj], grd.nc.out)
        #     ncdf4::ncvar_put(nc, grd.nc.out, don)
        #     ncdf4::nc_close(nc)
        # }

        env.tmp <- new.env()
        env.tmp$mat <- tmp.mat
        parsL <- doparallel.cond(FALSE)
        ret <- cdt.foreach(seq_along(index), parsL, GUI, progress, FUN = function(jj)
        {
            ix <- index[[jj]][win]
            ix <- idaty[ix]
            don <- rep(NA, xnlon0 * xnlat0)
            if(!is.na(ix)){
                nc <- ncdf4::nc_open(ncPATH[ix])
                don <- ncdf4::ncvar_get(nc, varid = varid0)
                ncdf4::nc_close(nc)
                don <- transposeNCDFData(don, ncINFO)
                don <- c(don)
            }
            
            if(jj > 1) env.tmp$mat[1:(win - 1), ] <- env.tmp$mat[2:win, ]
            env.tmp$mat[win, ] <- don
            if(indexF[jj]) return(NULL)

            don <- aggr.fun(env.tmp$mat, GalParams$aggr.series$na.rm)
            don[is.nan(don) | is.infinite(don)] <- NA
            xna <- colSums(!is.na(env.tmp$mat))
            don[xna < GalParams$aggr.series$min.data] <- NA
            don[is.na(don)] <- missval0
            don <- matrix(don, nrow = xnlon0, ncol = xnlat0)

            nc <- ncdf4::nc_create(out.ncfiles[jj], grd.nc.out)
            ncdf4::ncvar_put(nc, grd.nc.out, don)
            ncdf4::nc_close(nc)
            return(0)
        })
    }

    Insert.Messages.Out(GalParams[['message']][['5']], TRUE, "i")
    return(0)
}
