
AggregateMinMax_Execute <- function(){
    GalParams <- .cdtData$GalParams

    ##############
    Insert.Messages.Out(GalParams[['message']][['6']], TRUE, "i")

    period <- GalParams$in.tstep
    period1 <- GalParams$out.tstep
    minhr.in.step <- GalParams$HourMin$int
    minhr.out.step <- GalParams$HourMin$out
    obs.hour <- GalParams$HourMin$obs.hour
    startMonth <- GalParams$Seasonal$start.mon
    seasonLength <- GalParams$Seasonal$length.mon
    aggr.pars <- GalParams$aggr.series

    donne <- getStnOpenData(GalParams$cdtstation)
    if(is.null(donne)) return(NULL)
    donne <- splitCDTData0(donne)
    if(is.null(donne)) return(NULL)
    miss.val <- getStnOpenDataInfo(GalParams$cdtstation)[[3]]$miss.val
    dates <- donne$dates

    #########################
    agg.index <- cdt.index.aggregate(dates, period, period1,
                                    minhr.in.step, minhr.out.step, obs.hour,
                                    seasonLength, startMonth)

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
    cdtdata <- cdt.data.aggregateTS(donne$data, agg.index, aggr.pars)
    nc <- ncol(donne$data)

    ddaty <- lapply(seq_along(agg.index$index), function(j){
        x <- cdtdata[j, ]
        out <- rep(NA, nc)
        miss <- is.na(x)
        if(all(miss)) return(out)
        ix <- agg.index$index[[j]]
        don <- donne$data[ix, , drop = FALSE]
        x <- x[!miss]
        don <- don[, !miss, drop = FALSE]

        ii <- sweep(don, 2, x, FUN = "==")
        ii <- lapply(split(ii, col(ii)), which)
        out[!miss] <- sapply(ii, function(i)
                                paste0(donne$dates[ix[i]], collapse = "-")
                            )
        return(out)
    })

    ddaty <- do.call(rbind, ddaty)

    #########################
    headers <- do.call(rbind, donne[c('id', 'lon', 'lat', 'elv')])
    if(is.null(donne$elv)){
        capition <- c('Stations', 'LON', paste(toupper(period1), 'LAT', sep = '/'))
    }else{
        capition <- c('Stations', 'LON', 'LAT', paste(toupper(period1), 'ELV', sep = '/'))
    }
    entete <- cbind(capition, headers)

    #########################
    cdtdata <- round(cdtdata, 5)
    cdtdata <- rbind(entete, cbind(odaty, cdtdata))
    cdtdata[is.na(cdtdata)] <- miss.val
    writeFiles(cdtdata, GalParams$output)

    #########################
    ext <- tools::file_ext(GalParams$output)
    if(ext != "") ext <- paste0(".", ext)
    fpath <- tools::file_path_sans_ext(GalParams$output)
    outfile <- paste0(fpath, "_dates", ext)

    ddaty <- rbind(entete, cbind(odaty, ddaty))
    ddaty[is.na(ddaty)] <- miss.val
    writeFiles(ddaty, outfile)

    Insert.Messages.Out(GalParams[['message']][['13']], TRUE, "i")
    return(0)
}

