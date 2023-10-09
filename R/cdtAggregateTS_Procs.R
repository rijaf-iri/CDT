
AggregateTS_Execute <- function(){
    GUI <- TRUE
    progress <- TRUE
    GalParams <- .cdtData$GalParams

    ##############
    Insert.Messages.Out(GalParams[['message']][['6']], TRUE, "i")

    period <- GalParams$in.tstep
    period1 <- GalParams$out.tstep
    minhr.in.step <- GalParams$HourMin$int
    minhr.out.step <- GalParams$HourMin$out
    obs.hour <- GalParams$HourMin$obs.hour
    aggr.date <- .cdtData$GalParams$aggr.date
    aggr.period <- .cdtData$GalParams$aggr.period
    startMonth <- GalParams$Seasonal$start.mon
    seasonLength <- GalParams$Seasonal$length.mon
    datatype <- GalParams$data.type
    aggr.pars <- GalParams$aggr.series

    if(datatype == 'cdtstation'){
        donne <- getStnOpenData(GalParams$cdtstation)
        if(is.null(donne)) return(NULL)
        donne <- splitCDTData0(donne)
        if(is.null(donne)) return(NULL)
        miss.val <- getStnOpenDataInfo(GalParams$cdtstation)[[3]]$miss.val
        dates <- donne$dates
    }

    if(datatype == 'cdtdataset'){
        donne <- try(readRDS(GalParams$cdtdataset), silent = TRUE)
        if(inherits(donne, "try-error")){
            Insert.Messages.Out(paste(GalParams[['message']][['7']], GalParams$cdtdataset), TRUE, 'e')
            return(NULL)
        }
        if(period != donne$TimeStep){
            Insert.Messages.Out(paste(GalParams[['message']][['8']], period), TRUE, 'e')
            return(NULL)
        }
        dates <- donne$dateInfo$date
    }

    if(datatype == 'cdtnetcdf'){
        ncinfo <- ncInfo.with.date.range(GalParams$cdtnetcdf, GalParams$date.range, period, minhr.in.step)
        if(!any(ncinfo$exist)){
            Insert.Messages.Out(GalParams[['message']][['11']], TRUE, 'e')
            return(NULL)
        }
        dates <- ncinfo$dates[ncinfo$exist]
        ncPATH <- ncinfo$ncfiles[ncinfo$exist]

        ######
        ncsample <- getNCDFSampleData(GalParams$cdtnetcdf$sample)
        if(is.null(ncsample)){
            Insert.Messages.Out(GalParams[['message']][['12']], TRUE, 'e')
            return(NULL)
        }
        ncINFO <- ncsample[c('ilon', 'ilat', 'varid')]
    }

    #########################
    agg.index <- cdt.index.aggregate(dates, period, period1,
                                    minhr.in.step, minhr.out.step,
                                    seasonLength, startMonth,
                                    obs.hour, aggr.date, aggr.period)

    if(aggr.pars$min.frac$unique){
        ifull <- (agg.index$nba / agg.index$nb0) >= aggr.pars$min.frac$all
    }else{
        ifull <- sapply(agg.index$nb.mon, function(x){
            all(x$nba / x$nb0 >= aggr.pars$min.frac$month[x$mo])
        })
    }

    if(all(!ifull)){
        Insert.Messages.Out(GalParams[['message']][['14']], TRUE, 'e')
        return(NULL)
    }

    odaty <- agg.index$date

    #########################
    if(datatype == 'cdtstation'){
        cdtdata <- cdt.data.aggregateTS(donne$data, agg.index, aggr.pars)
        cdtdata <- round(cdtdata, 5)

        headers <- do.call(rbind, donne[c('id', 'lon', 'lat', 'elv')])
        if(is.null(donne$elv)){
            capition <- c('Stations', 'LON', paste(toupper(period1), 'LAT', sep = '/'))
        }else{
            capition <- c('Stations', 'LON', 'LAT', paste(toupper(period1), 'ELV', sep = '/'))
        }

        entete <- cbind(capition, headers)
        cdtdata <- rbind(entete, cbind(odaty, cdtdata))
        cdtdata[is.na(cdtdata)] <- miss.val
        writeFiles(cdtdata, GalParams$output)
        rm(cdtdata)
    }

    #########################

    if(datatype == 'cdtdataset'){
        aggrDIR <- paste0("Aggregated_Data_", aggr.pars$aggr.fun)
        outputDIR <- file.path(GalParams$output, aggrDIR, GalParams$dataset.name)
        dataDIR <- file.path(outputDIR, "DATA")
        dir.create(dataDIR, showWarnings = FALSE, recursive = TRUE)
        file.index <- file.path(outputDIR, paste0(GalParams$dataset.name, ".rds"))

        index.agg <- donne
        index.agg$TimeStep <- period1
        index.agg$dateInfo$date <- odaty
        index.agg$dateInfo$index <- seq_along(odaty)
        index.agg$varInfo <- GalParams$varinfo

        con <- gzfile(file.index, compression = 7)
        open(con, "wb")
        saveRDS(index.agg, con)
        close(con)

        ##########
        chunkfile <- sort(unique(donne$colInfo$index))
        chunkcalc <- split(chunkfile, ceiling(chunkfile / donne$chunkfac))
        cdtParallelCond <- .cdtData$Config$parallel

        ##########
        do.parChunk <- if(donne$chunkfac > length(chunkcalc)) TRUE else FALSE
        do.parCALC <- if(do.parChunk) FALSE else TRUE
        parsL <- doparallel.cond(do.parCALC & (length(chunkcalc) > 10))

        ret <- cdt.foreach(seq_along(chunkcalc), parsL, GUI, progress, FUN = function(jj)
        {
            don.data <- readCdtDatasetChunk.sequence(chunkcalc[[jj]], GalParams$cdtdataset, cdtParallelCond, do.par = do.parChunk)
            don.data <- don.data[donne$dateInfo$index, , drop = FALSE]
            cdtdata <- cdt.data.aggregateTS(don.data, agg.index, aggr.pars)
            writeCdtDatasetChunk.sequence(cdtdata, chunkcalc[[jj]], index.agg, dataDIR, cdtParallelCond, do.par = do.parChunk)
            rm(don.data, cdtdata); gc()
            return(0)
        })
    }

    #########################
    if(datatype == 'cdtnetcdf'){
        dataset.name <- paste0("Aggregated_Data_", aggr.pars$aggr.fun)
        outputDIR <- file.path(GalParams$output, dataset.name)
        dir.create(outputDIR, showWarnings = FALSE, recursive = TRUE)

        ncout.format <- gsub('%S', '%s', GalParams$ncout.format)
        outnc <- sprintf(ncout.format, odaty)
        out.ncfiles <- file.path(outputDIR, outnc)

        ########
        nc <- ncdf4::nc_open(ncPATH[1])
        varid0 <- ncINFO$varid
        xlon0 <- nc$var[[varid0]]$dim[[ncINFO$ilon]]$vals
        xlat0 <- nc$var[[varid0]]$dim[[ncINFO$ilat]]$vals
        ncdf4::nc_close(nc)

        ncINFO$xo <- order(xlon0)
        xlon0 <- xlon0[ncINFO$xo]
        ncINFO$yo <- order(xlat0)
        xlat0 <- xlat0[ncINFO$yo]
        xnlon0 <- length(xlon0)
        xnlat0 <- length(xlat0)

        #######
        dx <- ncdf4::ncdim_def("Lon", "degreeE", xlon0)
        dy <- ncdf4::ncdim_def("Lat", "degreeN", xlat0)
        grd.nc.out <- ncdf4::ncvar_def(GalParams$varinfo$name, GalParams$varinfo$units,
                                       list(dx, dy), GalParams$varinfo$missval,
                                       longname = GalParams$varinfo$longname,
                                       prec = GalParams$varinfo$prec, compression = 9)

        #######
        parsL <- doparallel.cond(length(agg.index$index) >= 20)

        ret <- cdt.foreach(seq_along(agg.index$index), parsL, GUI, progress, FUN = function(jj)
        {
            if(!ifull[jj]) return(NULL)
            ix <- agg.index$index[[jj]]
            nc.files <- ncPATH[agg.index$index[[jj]]]

            ncdon <- lapply(seq_along(nc.files), function(j){
                nc <- ncdf4::nc_open(nc.files[j])
                don <- ncdf4::ncvar_get(nc, varid = varid0)
                ncdf4::nc_close(nc)
                don <- transposeNCDFData(don, ncINFO)
                c(don)
            })
            ncdon <- do.call(rbind, ncdon)
            
            if(aggr.pars$min.frac$unique){
                miss <- (colSums(!is.na(ncdon)) / agg.index$nb0[jj]) < aggr.pars$min.frac$all
            }else{
                ix <- agg.index$nb.mon[[jj]]
                ii <- split(seq_along(ix$tsmo), ix$tsmo)
                miss <- lapply(seq_along(ii), function(i){
                    colSums(!is.na(ncdon[ii[[i]], , drop = FALSE]))/ix$nb0[i] < aggr.pars$min.frac$month[ix$mo[i]]
                })
                miss <- do.call(rbind, miss)
                miss <- apply(miss, 2, any)
            }
            if(all(miss)) return(NULL)

            out <- cdt.aggregate(ncdon, aggr.pars)
            out[miss] <- NA
            out[is.nan(out) | is.infinite(out)] <- NA
            out[is.na(out)] <- GalParams$varinfo$missval
            out <- matrix(out, nrow = xnlon0, ncol = xnlat0)

            nc <- ncdf4::nc_create(out.ncfiles[jj], grd.nc.out)
            ncdf4::ncvar_put(nc, grd.nc.out, out)
            ncdf4::nc_close(nc)
            rm(out, ncdon); gc()
            return(0)
        })
    }

    Insert.Messages.Out(GalParams[['message']][['13']], TRUE, "i")
    return(0)
}
