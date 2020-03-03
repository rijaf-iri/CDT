
AggregateTS_Execute <- function(){
    GUI <- TRUE
    progress <- TRUE

    ##############

    Insert.Messages.Out(.cdtData$GalParams[['message']][['6']], TRUE, "i")

    period <- .cdtData$GalParams$in.tstep
    period1 <- .cdtData$GalParams$out.tstep
    minhr.in.step <- .cdtData$GalParams$HourMin$int
    minhr.out.step <- .cdtData$GalParams$HourMin$out
    obs.hour <- .cdtData$GalParams$HourMin$obs.hour
    startMonth <- .cdtData$GalParams$Seasonal$start.mon
    seasonLength <- .cdtData$GalParams$Seasonal$length.mon

    min.frac <- .cdtData$GalParams$aggr.series$min.frac
    datatype <- .cdtData$GalParams$data.type

    if(datatype == 'cdtstation'){
        donne <- getStnOpenData(.cdtData$GalParams$cdtstation)
        if(is.null(donne)) return(NULL)
        donne <- splitCDTData0(donne)
        miss.val <- getStnOpenDataInfo(.cdtData$GalParams$cdtstation)[[3]]$miss.val
        dates <- donne$dates
    }

    if(datatype == 'cdtdataset'){
        donne <- try(readRDS(.cdtData$GalParams$cdtdataset), silent = TRUE)
        if(inherits(donne, "try-error")){
            Insert.Messages.Out(paste(.cdtData$GalParams[['message']][['7']], .cdtData$GalParams$cdtdataset), format = TRUE)
            return(NULL)
        }
        if(period != donne$TimeStep){
            Insert.Messages.Out(paste(.cdtData$GalParams[['message']][['8']], period), format = TRUE)
            return(NULL)
        }
        dates <- donne$dateInfo$date
    }

    if(datatype == 'cdtnetcdf'){
        ncinfo <- ncInfo.with.date.range(.cdtData$GalParams$cdtnetcdf,
                                         .cdtData$GalParams$Date.Range,
                                         period, minhr.in.step)
        ncEXIST <- ncinfo$exist
        if(!any(ncEXIST)){
            Insert.Messages.Out(.cdtData$GalParams[['message']][['11']], format = TRUE)
            return(NULL)
        }
        dates <- ncinfo$dates[ncEXIST]
        ncPATH <- ncinfo$ncfiles[ncEXIST]

        ######
        ncsample <- getNCDFSampleData(.cdtData$GalParams$cdtnetcdf$sample)
        if(is.null(ncsample)){
            Insert.Messages.Out(.cdtData$GalParams[['message']][['12']], format = TRUE)
            return(NULL)
        }
        ncINFO <- ncsample[c('ilon', 'ilat', 'varid')]
    }

    #########################
    ## index dates

    agg.index <- cdt.index.aggregate(dates, period, period1,
                                    minhr.in.step, minhr.out.step, obs.hour,
                                    seasonLength, startMonth)

    ifull <- (agg.index$nba / agg.index$nb0) >= min.frac
    if(all(!ifull)){
        Insert.Messages.Out(.cdtData$GalParams[['message']][['14']], format = TRUE)
        return(NULL)
    }

    odaty <- agg.index$date[ifull]
    index <- agg.index$index[ifull]
    nbd.in <- agg.index$nb0[ifull]

    #########################

    if(datatype == 'cdtstation'){
        cdtdata <- cdt.data.aggregate(donne$data, index, pars = .cdtData$GalParams$aggr.series)
        cdtdata <- round(cdtdata, 5)

        if(is.null(donne$elv)){
            headers <- t(cbind(donne$id, donne$lon, donne$lat))
            capition <- c('Stations', 'LON', paste(toupper(period1), 'LAT', sep = '/'))
        }else{
            headers <- t(cbind(donne$id, donne$lon, donne$lat, donne$elv))
            capition <- c('Stations', 'LON', 'LAT', paste(toupper(period1), 'ELV', sep = '/'))
        }

        entete <- cbind(capition, headers)
        cdtdata <- rbind(entete, cbind(odaty, cdtdata))
        cdtdata[is.na(cdtdata)] <- miss.val
        writeFiles(cdtdata, .cdtData$GalParams$output)
        rm(cdtdata)
    }

    if(datatype == 'cdtdataset'){
        outputDIR <- file.path(.cdtData$GalParams$output, "Aggregated_Data")
        dataDIR <- file.path(outputDIR, "DATA")
        dir.create(dataDIR, showWarnings = FALSE, recursive = TRUE)
        file.index <- file.path(outputDIR, "Aggregated_Data.rds")

        index.agg <- donne
        index.agg$TimeStep <- period1
        index.agg$dateInfo$date <- odaty
        index.agg$dateInfo$index <- seq_along(odaty)

        con <- gzfile(file.index, compression = 7)
        open(con, "wb")
        saveRDS(index.agg, con)
        close(con)

        ##########

        chunkfile <- sort(unique(donne$colInfo$index))
        chunkcalc <- split(chunkfile, ceiling(chunkfile / donne$chunkfac))
 
        GalParams <- .cdtData$GalParams
        cdtParallelCond <- .cdtData$Config[c('dopar', 'detect.cores', 'nb.cores')]

        ##########

        do.parChunk <- if(donne$chunkfac > length(chunkcalc)) TRUE else FALSE
        do.parCALC <- if(do.parChunk) FALSE else TRUE
        parsL <- doparallel.cond(do.parCALC & (length(chunkcalc) > 10))
        ret <- cdt.foreach(seq_along(chunkcalc), parsL, GUI, progress, FUN = function(jj)
        {
            don.data <- readCdtDatasetChunk.sequence(chunkcalc[[jj]], GalParams$cdtdataset, cdtParallelCond, do.par = do.parChunk)
            don.data <- don.data[donne$dateInfo$index, , drop = FALSE]
            cdtdata <- cdt.data.aggregate(don.data, index, pars = GalParams$aggr.series)
            writeCdtDatasetChunk.sequence(cdtdata, chunkcalc[[jj]], index.agg, dataDIR, cdtParallelCond, do.par = do.parChunk)
            rm(don.data, cdtdata); gc()
            return(0)
        })

        rm(GalParams)
    }

    if(datatype == 'cdtnetcdf'){
        outputDIR <- file.path(.cdtData$GalParams$output, "Aggregated_Data")
        dir.create(outputDIR, showWarnings = FALSE, recursive = TRUE)

        nc <- nc_open(ncPATH[which(ncEXIST)[1]])
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

        ########
        outnc <- paste0(strsplit(.cdtData$GalParams$cdtnetcdf$format, "%")[[1]][1], odaty, '.nc')
        out.ncfiles <- file.path(outputDIR, outnc)

        #######
        dx <- ncdim_def("Lon", "degreeE", xlon0)
        dy <- ncdim_def("Lat", "degreeN", xlat0)
        grd.nc.out <- ncvar_def(varid0, units0, list(dx, dy), missval0, longname = longname0, prec = prec0)

        #######
        pars.aggr.series <- .cdtData$GalParams$aggr.series

        #######
        parsL <- doparallel.cond(length(index) >= 20)
        ret <- cdt.foreach(seq_along(index), parsL, GUI, progress, FUN = function(jj)
        {
            ix <- index[[jj]]
            nc.files <- ncPATH[ix]
            nc.exist <- ncEXIST[ix]
            nc.files <- nc.files[nc.exist]
            len.nc.files <- length(nc.files)

            if((len.nc.files == 0) | ((len.nc.files / nbd.in[jj]) < min.frac)){
                out <- matrix(missval0, nrow = xnlon0, ncol = xnlat0)
                nc2 <- ncdf4::nc_create(out.ncfiles[jj], grd.nc.out)
                ncdf4::ncvar_put(nc2, grd.nc.out, out)
                ncdf4::nc_close(nc2)
                return(NULL)
            }

            ncdon <- lapply(seq_along(nc.files), function(j){
                nc <- ncdf4::nc_open(nc.files[j])
                don <- ncdf4::ncvar_get(nc, varid = varid0)
                ncdf4::nc_close(nc)
                don <- transposeNCDFData(don, ncINFO)
                c(don)
            })

            ncdon <- do.call(rbind, ncdon)
            miss <- (colSums(is.na(ncdon)) / nrow(ncdon)) >= min.frac

            out <- cdt.aggregate(ncdon, pars = pars.aggr.series)

            out[miss] <- missval0
            out[is.na(out) | is.nan(out) | is.infinite(out)] <- missval0
            out <- matrix(out, nrow = xnlon0, ncol = xnlat0)

            nc2 <- ncdf4::nc_create(out.ncfiles[jj], grd.nc.out)
            ncdf4::ncvar_put(nc2, grd.nc.out, out)
            ncdf4::nc_close(nc2)
            rm(out, ncdon); gc()
            return(0)
        })
    }

    Insert.Messages.Out(.cdtData$GalParams[['message']][['13']], TRUE, "i")
    return(0)
}
