
filterCDTdata <- function(){
    GalParams <- .cdtData$GalParams
    if(GalParams$filein == ""){
        Insert.Messages.Out(GalParams[['message']][['4']], TRUE, "e")
        return(NULL)
    }

    Insert.Messages.Out(GalParams[['message']][['1']], TRUE, "i")

    donneInfo <- getStnOpenDataInfo(GalParams$filein)
    if(is.null(donneInfo)) return(NULL)
    donne <- getStnOpenData(GalParams$filein)
    capt <- donne[1:4, 1]
    if(is.null(donne)) return(NULL)

    donne <- splitCDTData0(donne)
    if(is.null(donne)) return(NULL)

    if(!GalParams$all.period){
        dateRange <- get.range.date.time(GalParams$date.range, GalParams$tstep, GalParams$minhour)
        dates <- get.datetime.cdtstation(donne$dates, GalParams$tstep)
        idate <- dates >= dateRange$start & dates <= dateRange$end
        idate[is.na(idate)] <- FALSE
        if(!any(idate)){
            Insert.Messages.Out(GalParams[['message']][['5']], format = TRUE)
            return(NULL)
        }
        donne$dates <- donne$dates[idate]
        donne$data <- donne$data[idate, , drop = FALSE]
    }

    len <- length(donne$dates)
    pnadonne <- colSums(!is.na(donne$data))/len

    opfilter <- get(GalParams$opfilter, mode = "function")
    istn <- as.logical(opfilter(pnadonne, GalParams$valfilter / 100))

    donne$id <- donne$id[istn]
    donne$lon <- donne$lon[istn]
    donne$lat <- donne$lat[istn]
    if(!is.null(donne$elv)) donne$elv <- donne$elv[istn]
    donne$data <- donne$data[, istn, drop = FALSE]

    if(is.null(donne$elv)) capt <- capt[1:3]
    xhead <- do.call(rbind, donne[c('id', 'lon', 'lat', 'elv')])
    xhead <- as.matrix(cbind(capt, xhead))

    donne <- rbind(xhead, cbind(donne$dates, donne$data))

    writeFiles(donne, GalParams$file2save, na = donneInfo[[3]]$miss.val)
    # write.table(donne, GalParams$file2save,
    #             sep = donneInfo[[3]]$sepr,
    #             na = donneInfo[[3]]$miss.val,
    #             col.names = FALSE, row.names = FALSE, quote = FALSE)

    return(0)
}
