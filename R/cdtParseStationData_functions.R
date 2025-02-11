
#' Read CDT station format file.
#'
#' Read CDT station format file.
#' 
#' @param file character, full path to the file containing the CDT data
#' @param sep character, the column's separator of the data
#' @param na.strings character, the missing values flag
#'  
#' @return A CDT stations data objects. It is a list object with elements
#' \itemize{
#'   \item \strong{id}: Vector of the points/stations id
#'   \item \strong{lon}: Vector of the points/stations longitude
#'   \item \strong{lat}: Vector of the points/stations latitude
#'   \item \strong{elv}: Vector of the points/stations elevation, NULL if there is no elevation data
#'   \item \strong{dates}: Vector of the dates or times of the data
#'   \item \strong{data}: Matrix of the data, row indicates the dates and column the stations
#' }
#' 
#' @export

readCDTStationData <- function(file, sep = ",", na.strings = "-99"){
    donne <- utils::read.table(file, sep = sep,
                               na.strings = na.strings,
                               colClasses = "character",
                               stringsAsFactors = FALSE,
                               quote = "\"")
    dat <- splitCDTData0(donne, GUI = FALSE)
    if(is.null(dat))
        stop("Station data is not in a standard CDT format")

    class(dat) <- append(class(dat), "cdtstationdata")
    return(dat)
}


#' Read CDT station format file.
#'
#' Read CDT station format file.
#' 
#' @param file character, full path to the file containing the CDT data
#' @param sep character, the column's separator of the data
#' @param na.strings character, the missing values flag
#'  
#' @return A CDT stations data objects. It is a list object with elements
#' \itemize{
#'   \item \strong{id}: Vector of the points/stations id
#'   \item \strong{lon}: Vector of the points/stations longitude
#'   \item \strong{lat}: Vector of the points/stations latitude
#'   \item \strong{elv}: Vector of the points/stations elevation, NULL if there is no elevation data
#'   \item \strong{dates}: Vector of the dates or times of the data
#'   \item\strong{data}: Matrix of the data, row indicates the dates and column the stations
#' }
#' 
#' @export

readCDTStationData_allFormat <- function(file, sep = ",", na.strings = "-99"){
    donne <- utils::read.table(file, sep = sep,
                               na.strings = na.strings,
                               colClasses = "character",
                               stringsAsFactors = FALSE,
                               quote = "\"")
    dat <- splitCDTData1(donne)
    if(is.null(dat))
        stop("Station data is not in a standard CDT format")

    class(dat) <- append(class(dat), "cdtstationdata")
    return(dat)
}

#' Write CDT station format to a file.
#'
#' Write CDT station format to a file.
#' 
#' @param data CDT stations data objects.
#' @param file character, full path to the file to save the CDT data
#' @param na.strings character, the missing values flag to write to the file
#' @param sep character, the column separator
#' 
#' @export

writeCDTStationData <- function(data, file, na.strings = "-99", sep = ","){
    if(!inherits(data, "cdtstationdata"))
        stop("data is not a CDT stations data object")

    extFl <- tolower(tools::file_ext(trimws(basename(file))))
    sep <- if(extFl == "csv") "," else sep
    if(sep == "") sep <- " "
    capt <- c('ID', 'LON', 'LAT')
    if(!is.null(data$elv)) capt <- c(capt, 'ELV')

    dat <- do.call(rbind, data[c('id', 'lon', 'lat', 'elv', 'data')])
    dat <- cbind(c(capt, data$dates), dat)

    utils::write.table(dat, file = file, sep = sep, na = na.strings,
                row.names = FALSE, col.names = FALSE, quote = FALSE)
}

#' Match CDT stations data.
#'
#' Filter CDT stations data to match the stations and dates.
#' 
#' @param ... CDT stations data objects, possibly named.
#' 
#' @return A list of CDT stations data objects, in same order as provided in the input arguments.
#' A named list if input arguments are named.
#' 
#' @export

matchCDTStationsData <- function(...){
    tmp <- matchCDTStationsIDs(...)
    do.call(matchCDTStationsDates, tmp)
}

#' Match CDT stations data IDs.
#'
#' Filter CDT stations data to match the stations.
#' 
#' @param ... CDT stations data objects, possibly named.
#' 
#' @return A list of CDT stations data objects, in same order as provided in the input arguments.
#' A named list if input arguments are named.
#' 
#' @export

matchCDTStationsIDs <- function(...){
    x <- list(...)
    if(nargs() == 1){
        cat("Nothing to do.\n")
        return(x)
    }

    is_cdt <- sapply(x, inherits, what = "cdtstationdata")
    if(any(!is_cdt)){
        nom <- names(x)
        if(is.null(nom)){
            msg <- paste("Elements number",
                   paste0(which(!is_cdt), collapse = ', '))
        }else{
            msg <- paste("The elements",
                   paste0(nom[!is_cdt], collapse = ', '))
        }
        stop(paste(msg, "are not a CDT stations data objects"))
    }

    id <- Reduce(intersect, lapply(x, "[[", "id"))
    if(length(id) == 0)
        stop("Stations do not overlap")

    lapply(x, function(l){
        ix <- match(id, l$id)
        l$id <- l$id[ix]
        l$lon <- l$lon[ix]
        l$lat <- l$lat[ix]
        l$elv <- l$elv[ix]
        l$data <- l$data[, ix, drop = FALSE]
        l
    })
}

#' Match CDT stations data dates.
#'
#' Filter CDT stations data to match the dates.
#' 
#' @param ... CDT stations data objects, possibly named.
#' 
#' @return A list of CDT stations data objects, in same order as provided in the input arguments.
#' A named list if input arguments are named.
#' 
#' @export

matchCDTStationsDates <- function(...){
    x <- list(...)
    if(nargs() == 1){
        cat("Nothing to do.\n")
        return(x)
    }

    is_cdt <- sapply(x, inherits, what = "cdtstationdata")
    if(any(!is_cdt)){
        nom <- names(x)
        if(is.null(nom)){
            msg <- paste("Elements number",
                   paste0(which(!is_cdt), collapse = ', '))
        }else{
            msg <- paste("The elements",
                   paste0(nom[!is_cdt], collapse = ', '))
        }
        stop(paste(msg, "are not a CDT stations data objects"))
    }

    it <- Reduce(intersect, lapply(x, "[[", "dates"))
    if(length(it) == 0)
        stop("Dates do not overlap")

    it <- it[order(it)]
    lapply(x, function(l){
        ix <- match(it, l$dates)
        l$dates <- l$dates[ix]
        l$data <- l$data[ix, , drop = FALSE]
        l
    })
}

#########################################################

## use with CDT stations format with elevation or not
## CDT stations format without text or special character like (-, _, /) in dates
## daily, pentad, dekad, month, year, climatologies index,
## CDT stations format output like (onset, cessation, ....)
## output are not ordered and missing dates are not filled
## no filter for duplicated dates

splitCDTData0 <- function(donne, GUI = TRUE){
    cdt.file.conf <- file.path(.cdtDir$dirLocal, "config", "cdt_config.json")
    Config <- jsonlite::fromJSON(cdt.file.conf)
    xml.dlg <- file.path(.cdtDir$dirLocal, "languages", "cdtParseStationData_functions.xml")
    lang.dlg <- cdtLanguageParse(xml.dlg, Config$lang.iso)

    ###############
    seph <- rle(!grepl('[^[:digit:]]', as.character(donne[, 1])))
    ipos <- which(!seph$values & seph$lengths >= 3 & seph$lengths <= 4)
    if(length(ipos) == 0 | ipos[1] != 1){
        Insert.Messages.Out(lang.dlg[['message']][['1']], TRUE, "e", GUI)
        return(NULL)
    }
    pos <- seph$lengths[ipos[1]]

    ###############
    dat <- list(id = as.character(donne[1, -1]),
                lon = as.numeric(donne[2, -1]),
                lat = as.numeric(donne[3, -1]),
                elv = if(pos == 4) as.numeric(donne[4, -1]) else NULL,
                dates = as.character(donne[-(1:pos), 1]),
                data = local({
                            tmp <- donne[-(1:pos), -1, drop = FALSE]
                            ntmp <- dim(tmp)
                            tmp <- as.numeric(unlist(tmp))
                            dim(tmp) <- ntmp
                            tmp
                    })
                )
    dimnames(dat$data)[[2]] <- NULL
    dat$data <- convert_data_type(dat$data, as.numeric)
    return(dat)
}

##########################################
## use with any CDT stations format without elevation included
## all CDT stations format output
## output are not ordered and missing dates are not filled
## no filter for duplicated dates

splitCDTData1 <- function(donne){
    dat <- list(id = as.character(donne[1, -1]),
                lon = as.numeric(donne[2, -1]),
                lat = as.numeric(donne[3, -1]),
                dates = as.character(donne[-(1:3), 1]),
                data = local({
                            tmp <- donne[-(1:3), -1, drop = FALSE]
                            ntmp <- dim(tmp)
                            tmp <- as.numeric(unlist(tmp))
                            dim(tmp) <- ntmp
                            tmp
                    })
                )
    dimnames(dat$data)[[2]] <- NULL
    dat$data <- convert_data_type(dat$data, as.numeric)
    return(dat)
}

##########################################
## use with CDT stations format with elevation or not
## minute, hourly, daily, pentad, dekad, month
## output are ordered and the missing dates are filled
## duplicated dates are removed
## check invalid and duplicated coordinates

splitCDTData <- function(donne, tstep, GUI = TRUE){
    cdt.file.conf <- file.path(.cdtDir$dirLocal, "config", "cdt_config.json")
    Config <- jsonlite::fromJSON(cdt.file.conf)
    xml.dlg <- file.path(.cdtDir$dirLocal, "languages", "cdtParseStationData_functions.xml")
    lang.dlg <- cdtLanguageParse(xml.dlg, Config$lang.iso)

    ###############

    first_col <- trimws(donne[, 1])
    i1st_col <- first_col != ""
    donne <- donne[i1st_col, , drop = FALSE]

    ideb <- nrow(donne)
    datylen <- nchar(as.character(donne[ideb, 1]))

    if(tstep == 'minute' & datylen != 12){
        Insert.Messages.Out(lang.dlg[['message']][['2']], TRUE, "e", GUI)
        return(NULL)
    }
    if(tstep == 'hourly' & datylen != 10){
        Insert.Messages.Out(lang.dlg[['message']][['3']], TRUE, "e", GUI)
        return(NULL)
    }
    if(tstep == 'daily' & datylen != 8){
        Insert.Messages.Out(lang.dlg[['message']][['4']], TRUE, "e", GUI)
        return(NULL)
    }
    if(tstep == 'pentad' & datylen != 7){
        Insert.Messages.Out(lang.dlg[['message']][['5']], TRUE, "e", GUI)
        return(NULL)
    }
    if(tstep == 'dekadal' & datylen != 7){
        Insert.Messages.Out(lang.dlg[['message']][['6']], TRUE, "e", GUI)
        return(NULL)
    }
    if(tstep == 'monthly' & datylen != 6){
        Insert.Messages.Out(lang.dlg[['message']][['7']], TRUE, "e", GUI)
        return(NULL)
    }

    ##############

    seph <- rle(!grepl('[^[:digit:]]', as.character(donne[, 1])))
    ipos <- which(!seph$values & seph$lengths >= 3 & seph$lengths <= 4)
    if(length(ipos) == 0 | ipos[1] != 1){
        Insert.Messages.Out(lang.dlg[['message']][['1']], TRUE, "e", GUI)
        return(NULL)
    }

    ihead <- 1:seph$lengths[ipos[1]]
    Info <- data.frame(t(donne[ihead, -1, drop = FALSE]))
    names(Info) <- if(length(ihead) == 4)
                        c('Stations', 'Lon', 'Lat', 'ELV')
                   else
                        c('Stations', 'Lon', 'Lat')

    daty.orig <- as.character(donne[-ihead, 1])

    if(tstep %in% c('pentad', 'dekadal')){
        dek <- as.numeric(substr(daty.orig, 7, 7))
        dek <- dek[!is.na(dek)]
        if(tstep == 'pentad') wrongdaty <- any(dek == 0 | dek > 6)
        if(tstep == 'dekadal') wrongdaty <- any(dek == 0 | dek > 3)
        if(wrongdaty){
            Insert.Messages.Out(lang.dlg[['message']][['8']], TRUE, "e", GUI)
            return(NULL)
        }
    }

    ##############

    if(tstep == 'minute'){
        dates.bak <- daty.orig
        dates <- as.POSIXct(dates.bak, format = '%Y%m%d%H%M')
    }
    if(tstep == 'hourly'){
        dates.bak <- daty.orig
        dates <- as.POSIXct(dates.bak, format = '%Y%m%d%H')
    }
    if(tstep == 'daily'){
        dates.bak <- daty.orig
        dates <- as.Date(dates.bak, format = '%Y%m%d')
    }
    if(tstep == 'pentad'){
        xan <- substr(daty.orig, 1, 4)
        xmo <- substr(daty.orig, 5, 6)
        xpt <- substr(daty.orig, 7, 7)
        notpen <- which(as.numeric(xpt) > 6)
        dates.bak <- paste(xan, xmo, xpt, sep = '-')
        dates <- as.Date(dates.bak)
        dates[notpen] <- NA
    }
    if(tstep == 'dekadal'){
        xan <- substr(daty.orig, 1, 4)
        xmo <- substr(daty.orig, 5, 6)
        xdk <- substr(daty.orig, 7, 7)
        notdek <- which(as.numeric(xdk) > 3)
        dates.bak <- paste(xan, xmo, xdk, sep = '-')
        dates <- as.Date(dates.bak)
        dates[notdek] <- NA
    }
    if(tstep == 'monthly'){
        xan <- substr(daty.orig, 1, 4)
        xmo <- substr(daty.orig, 5, 6)
        dates.bak <- paste(xan, xmo, '1', sep = '-')
        dates <- as.Date(dates.bak)
    }

    ##############
    donne <- as.matrix(donne[-ihead, -1, drop = FALSE])
    dimnames(donne) <- NULL
    donne <- convert_data_type(donne, as.numeric)

    ##############
    ## missing
    iNAdate <- is.na(dates)
    wrong.dates <- dates.bak[iNAdate]
    dwrong.dates <- donne[iNAdate, , drop = FALSE]
    donne <- donne[!iNAdate, , drop = FALSE]
    dates <- dates[!iNAdate]

    ## duplicated dates
    idates0 <- duplicated(dates) | duplicated(dates, fromLast = TRUE)
    dup.dates <- dates[idates0]
    ddup.dates <- donne[idates0, , drop = FALSE]
    donne <- donne[!duplicated(dates), , drop = FALSE]
    dates <- dates[!duplicated(dates)]

    ##############
    wrong.dates <- NULL
    duplicated.dates <- NULL
    missing.dates <- NULL

    if(!tstep %in% c('minute', 'hourly')){
        ## fill missing dates 
        if(tstep == 'daily'){
            alldates <- seq(min(dates), max(dates), 'day')
            ix <- match(alldates, dates)
            dates <- format(alldates, '%Y%m%d')
            miss.dates <- alldates[is.na(ix)]
            miss.dates <- format(miss.dates, '%Y%m%d')
            dup.dates <- format(dup.dates, '%Y%m%d')
        }
        if(tstep == 'pentad'){
            alldates <- seq(min(dates), max(dates), 'day')
            alldates <- alldates[as.numeric(format(alldates, '%d')) <= 6]
            ix <- match(alldates, dates)
            dates <- paste0(format(alldates, '%Y%m'), as.numeric(format(alldates, '%d')))
            miss.dates <- alldates[is.na(ix)]
            miss.dates <- paste0(format(miss.dates, '%Y%m'), as.numeric(format(miss.dates, '%d')))
            dup.dates <- paste0(format(dup.dates, '%Y%m'), as.numeric(format(dup.dates, '%d')))
        }
        if(tstep == 'dekadal'){
            alldates <- seq(min(dates), max(dates), 'day')
            alldates <- alldates[as.numeric(format(alldates, '%d')) <= 3]
            ix <- match(alldates, dates)
            dates <- paste0(format(alldates, '%Y%m'), as.numeric(format(alldates, '%d')))
            miss.dates <- alldates[is.na(ix)]
            miss.dates <- paste0(format(miss.dates, '%Y%m'), as.numeric(format(miss.dates, '%d')))
            dup.dates <- paste0(format(dup.dates, '%Y%m'), as.numeric(format(dup.dates, '%d')))
        }
        if(tstep == 'monthly'){
            alldates <- seq(min(dates), max(dates), 'month')
            ix <- match(alldates, dates)
            dates <- format(alldates, '%Y%m')
            miss.dates <- alldates[is.na(ix)]
            miss.dates <- format(miss.dates, '%Y%m')
            dup.dates <- format(dup.dates, '%Y%m')
        }
        donne <- donne[ix, , drop = FALSE]

        if(any(iNAdate))
            wrong.dates <- list(date = wrong.dates, data = dwrong.dates)
        if(any(idates0))
            duplicated.dates <- list(date = dup.dates, data = ddup.dates)
        if(any(is.na(ix)))
            missing.dates <- list(date = miss.dates)
    }else{
        if(tstep == 'minute') dates <- format(dates, '%Y%m%d%H%M')
        if(tstep == 'hourly') dates <- format(dates, '%Y%m%d%H')
    }

    ##############

    stn.id <- as.character(Info[, 1])
    stn.lon <- as.numeric(as.character(Info[, 2]))
    stn.lat <- as.numeric(as.character(Info[, 3]))
    stn.elv <- if(ncol(Info) == 4) as.numeric(as.character(Info[, 4])) else NULL

    ## missing & duplicated coordinates
    iddup <- duplicated(stn.id) | duplicated(stn.id, fromLast = TRUE)
    imiss <- (is.na(stn.lon) | stn.lon < -180 | stn.lon > 360 | is.na(stn.lat) | stn.lat < -90 | stn.lat > 90)
    idup <- (!is.na(stn.lon) | !is.na(stn.lat)) & (duplicated(Info[, 2:3, drop = FALSE]) | duplicated(Info[, 2:3, drop = FALSE], fromLast = TRUE))
    aretenir <- !imiss & !duplicated(Info[, 2:3, drop = FALSE]) & !duplicated(stn.id)

    stn.lon <- stn.lon[aretenir]
    stn.lat <- stn.lat[aretenir]
    stn.id <- stn.id[aretenir]
    stn.elv <- stn.elv[aretenir]
    donne <- donne[, aretenir, drop = FALSE]

    ##############

    stn.miss <- NULL
    if(any(imiss)){
        stn.miss <- Info[imiss, , drop = FALSE]
        rownames(stn.miss) <- NULL
    }

    stn.dup <- NULL
    if(any(idup)){
        stn.dup <- Info[idup, , drop = FALSE]
        o <- order(apply(stn.dup[, 2:3, drop = FALSE], 1, paste, collapse = ''))
        stn.dup <- stn.dup[o, , drop = FALSE]
        rownames(stn.dup) <- NULL
    }

    stn.id.dup <- NULL
    if(any(iddup)){
        stn.id.dup <- Info[iddup, , drop = FALSE]
        stn.id.dup <- stn.id.dup[order(stn.id.dup[, 1]), , drop = FALSE]
        rownames(stn.id.dup) <- NULL
    }

    ##############

    stnlist <- list(id = stn.id,
                    lon = stn.lon,
                    lat = stn.lat,
                    elv = stn.elv,
                    dates = dates,
                    data = donne,
                    wrong.dates = wrong.dates,
                    duplicated.dates = duplicated.dates,
                    missing.dates = missing.dates,
                    duplicated.coords = stn.dup,
                    missing.coords = stn.miss,
                    duplicated.stnID = stn.id.dup,
                    miss.coords.idx = imiss,
                    dup.coords.idx = idup,
                    dup.stnID.idx = iddup)

    return(stnlist)
}

##########################################

splitTsData <- function(donne, tstep, filefrmt, datefrmt, GUI = TRUE){
    cdt.file.conf <- file.path(.cdtDir$dirLocal, "config", "cdt_config.json")
    Config <- jsonlite::fromJSON(cdt.file.conf)
    xml.dlg <- file.path(.cdtDir$dirLocal, "languages", "cdtParseStationData_functions.xml")
    lang.dlg <- cdtLanguageParse(xml.dlg, Config$lang.iso)

    ###############
    ## get dates
    first_col <- trimws(donne[, 1])
    i1st_col <- first_col != ""
    donne <- donne[i1st_col, , drop = FALSE]

    ideb <- nrow(donne)
    datylen <- nchar(as.character(donne[ideb, 1]))

    if(tstep == 'minute'){
        if(datefrmt == "1"){
            if(datylen != 12){
                Insert.Messages.Out(lang.dlg[['message']][['2']], TRUE, "e", GUI)
                return(NULL)
            }
            dates.bak <- as.character(donne[, 1])
            dates <- as.POSIXct(dates.bak, format = '%Y%m%d%H%M')
        }else{
            dates.bak <- paste(as.character(donne[, 1]),
                               as.character(donne[, 2]),
                               as.character(donne[, 3]),
                               as.character(donne[, 4]),
                               as.character(donne[, 5]),
                               sep = '-')
            dates <- as.POSIXct(dates.bak, format = '%Y-%m-%d-%H-%M')
        }
    }
    if(tstep == 'hourly'){
        if(datefrmt == "1"){
            if(datylen != 10){
                Insert.Messages.Out(lang.dlg[['message']][['3']], TRUE, "e", GUI)
                return(NULL)
            }
            dates.bak <- as.character(donne[, 1])
            dates <- as.POSIXct(dates.bak, format = '%Y%m%d%H')
        }else{
            dates.bak <- paste(as.character(donne[, 1]),
                               as.character(donne[, 2]),
                               as.character(donne[, 3]),
                               as.character(donne[, 4]),
                               sep = '-')
            dates <- as.POSIXct(dates.bak, format = '%Y-%m-%d-%H')
        }
    }
    if(tstep == 'daily'){
        if(datefrmt == "1"){
            if(datylen != 8){
                Insert.Messages.Out(lang.dlg[['message']][['4']], TRUE, "e", GUI)
                return(NULL)
            }
            dates.bak <- as.character(donne[, 1])
            dates <- as.Date(dates.bak, format = '%Y%m%d')
        }else{
            dates.bak <- paste(as.character(donne[, 1]),
                               as.character(donne[, 2]),
                               as.character(donne[, 3]),
                               sep = '-')
            dates <- as.Date(dates.bak)
        }
    }
    if(tstep == 'pentad'){
        if(datefrmt == "1"){
            #1date
            if(datylen != 7){
                Insert.Messages.Out(lang.dlg[['message']][['5']], TRUE, "e", GUI)
                return(NULL)
            }
            xan <- substr(as.character(donne[, 1]), 1, 4)
            xmo <- substr(as.character(donne[, 1]), 5, 6)
            xdk <- substr(as.character(donne[, 1]), 7, 7)
            notpen<- which(as.numeric(xdk) > 6)
            dates.bak <- paste(xan, xmo, xdk, sep = '-')
            dates <- as.Date(dates.bak)
        }else{
            #3date
            dates.bak <- paste(as.character(donne[, 1]),
                               as.character(donne[, 2]),
                               as.character(donne[, 3]),
                               sep = '-')
            dates <- as.Date(dates.bak)
            notpen <- which(as.numeric(as.character(donne[, 3])) > 6)
        }
        dates[notpen] <- NA
    }
    if(tstep == 'dekadal'){
        if(datefrmt == "1"){
            #1date
            if(datylen != 7){
                Insert.Messages.Out(lang.dlg[['message']][['6']], TRUE, "e", GUI)
                return(NULL)
            }
            xan <- substr(as.character(donne[, 1]), 1, 4)
            xmo <- substr(as.character(donne[, 1]), 5, 6)
            xdk <- substr(as.character(donne[, 1]), 7, 7)
            notdek <- which(as.numeric(xdk) > 3)
            dates.bak <- paste(xan, xmo, xdk, sep = '-')
            dates <- as.Date(dates.bak)
        }else{
            #3date
            dates.bak <- paste(as.character(donne[, 1]),
                               as.character(donne[, 2]),
                               as.character(donne[, 3]),
                               sep = '-')
            dates <- as.Date(dates.bak)
            notdek <- which(as.numeric(as.character(donne[, 3])) > 3)
        }
        dates[notdek] <- NA
    }
    if(tstep == 'monthly'){
        if(datefrmt == "1"){
            #1date
            if(datylen != 6){
                Insert.Messages.Out(lang.dlg[['message']][['7']], TRUE, "e", GUI)
                return(NULL)
            }
            xan <- substr(as.character(donne[, 1]), 1, 4)
            xmo <- substr(as.character(donne[, 1]), 5, 6)
            dates.bak <- paste(xan, xmo, '1', sep = '-')
            dates <- as.Date(dates.bak)
        }else{
            #3date
            dates.bak <- paste(as.character(donne[, 1]),
                               as.character(donne[, 2]),
                               '1', sep = '-')
            dates <- as.Date(dates.bak)
        }
    }

    #################
    ## get vars
    if(filefrmt == "1"){
        #1var
        if(datefrmt == "1"){
            ix <- 2
        }else{
            if(tstep == 'monthly'){
                if(ncol(donne) == 3) ix <- 3
                if(ncol(donne) == 4) ix <- 4
            }else{
                if(tstep == 'minute') ix <- 6
                else if(tstep == 'hourly') ix <- 5
                else ix <- 4
            }
        }

        var <- as.numeric(donne[, ix])
    }else{
        #3var
        if(datefrmt == "1"){
            ix <- 2:4
        }else{
            if(tstep == 'monthly'){
                if(ncol(donne) == 5) ix <- 3:5
                if(ncol(donne) == 6) ix <- 4:6
            }else{
                if(tstep == 'minute') ix <- 6:8
                else if(tstep == 'hourly') ix <- 5:7
                else ix <- 4:6
            }
        }

        rr <- as.numeric(donne[, ix[1]])
        tx <- as.numeric(donne[, ix[2]])
        tn <- as.numeric(donne[, ix[3]])
    }

    #################
    ## remove NA dates
    iNAdate <- is.na(dates)
    wrong.dates <- dates.bak[iNAdate]

    if(filefrmt == "1"){
        dwrong.dates <- var[iNAdate]
        var <- var[!iNAdate]
        dates <- dates[!iNAdate]
    }else{
        dwrong.dates <- cbind(rr[iNAdate], tx[iNAdate], tn[iNAdate])
        rr <- rr[!iNAdate]
        tx <- tx[!iNAdate]
        tn <- tn[!iNAdate]
        dates <- dates[!iNAdate]
    }

    #################
    ## duplicated dates
    idates0 <- duplicated(dates) | duplicated(dates, fromLast = TRUE)
    dup.dates <- dates[idates0]

    idates <- duplicated(dates)
    dates <- dates[!idates]
    if(filefrmt == "1"){
        ddup.dates <- var[idates0]
        var <- var[!idates]
    }else{
        ddup.dates <- cbind(rr[idates0], tx[idates0], tn[idates0])
        rr <- rr[!idates]
        tx <- tx[!idates]
        tn <- tn[!idates]
    }

    if(length(dates) == 0){
        Insert.Messages.Out(lang.dlg[['message']][['8']], TRUE, "e", GUI)
        return(NULL)
    }

    #################
    wrong.dates <- NULL
    duplicated.dates <- NULL
    missing.dates <- NULL

    if(!tstep %in% c('minute', 'hourly')){
        if(tstep == 'daily'){
            alldates <- seq(min(dates), max(dates), 'day')
            ix <- match(alldates, dates)
            dates <- format(alldates, '%Y%m%d')
            miss.dates <- alldates[is.na(ix)]
            miss.dates <- format(miss.dates, '%Y%m%d')
            dup.dates <- format(dup.dates, '%Y%m%d')
        }
        if(tstep == 'pentad'){
            alldates <- seq(min(dates), max(dates), 'day')
            alldates <- alldates[as.numeric(format(alldates, '%d')) <= 6]
            ix <- match(alldates, dates)
            dates <- paste0(format(alldates, '%Y%m'), as.numeric(format(alldates, '%d')))
            miss.dates <- alldates[is.na(ix)]
            miss.dates <- paste0(format(miss.dates, '%Y%m'), as.numeric(format(miss.dates, '%d')))
            dup.dates <- paste0(format(dup.dates, '%Y%m'), as.numeric(format(dup.dates, '%d')))
        }
        if(tstep == 'dekadal'){
            alldates <- seq(min(dates), max(dates), 'day')
            alldates <- alldates[as.numeric(format(alldates, '%d')) <= 3]
            ix <- match(alldates, dates)
            dates <- paste0(format(alldates, '%Y%m'), as.numeric(format(alldates, '%d')))
            miss.dates <- alldates[is.na(ix)]
            miss.dates <- paste0(format(miss.dates, '%Y%m'), as.numeric(format(miss.dates, '%d')))
            dup.dates <- paste0(format(dup.dates, '%Y%m'), as.numeric(format(dup.dates, '%d')))
        }
        if(tstep == 'monthly'){
            alldates <- seq(min(dates), max(dates), 'month')
            ix <- match(alldates, dates)
            dates <- format(alldates, '%Y%m')
            miss.dates <- alldates[is.na(ix)]
            miss.dates <- format(miss.dates, '%Y%m')
            dup.dates <- format(dup.dates, '%Y%m')
        }

        var <- if(filefrmt == "1")
                    list(var = var[ix])
                else
                    list(rr = rr[ix], tx = tx[ix], tn = tn[ix])

        if(any(iNAdate))
            wrong.dates <- list(date = wrong.dates, data = dwrong.dates)
        if(any(idates0))
            duplicated.dates <- list(date = dup.dates, data = ddup.dates)
        if(any(is.na(ix)))
            missing.dates <- list(date = miss.dates)
    }else{
        if(tstep == 'minute') dates <- format(dates, '%Y%m%d%H%M')
        if(tstep == 'hourly') dates <- format(dates, '%Y%m%d%H')

        var <- if(filefrmt == "1")
                    list(var = var)
                else
                    list(rr = rr, tx = tx, tn = tn)
    }

    nbvar <- if(filefrmt == "1") 1 else 3

    ret <- list(tstep = tstep,  dates = dates,
                nbvar = nbvar, var = var,
                wrong.dates = wrong.dates,
                duplicated.dates = duplicated.dates,
                missing.dates = missing.dates)

    return(ret)
}
