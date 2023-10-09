
formatCDTDataMultiple.Files <- function(){
    Insert.Messages.Out(.cdtData$GalParams[['message']][['4']], TRUE, "i")
    tcl("update")

    tstep <- .cdtData$GalParams$tstep
    if(tstep == "daily"){
        is <- "start.day"
        ie <- 'end.day'
    }else{
        is <- "start.dek"
        ie <- 'end.dek'
    }
    istart.yrs <- .cdtData$GalParams$date.range$start.year
    istart.mon <- .cdtData$GalParams$date.range$start.mon
    istart.day <- .cdtData$GalParams$date.range[[is]]
    iend.yrs <- .cdtData$GalParams$date.range$end.year
    iend.mon <- .cdtData$GalParams$date.range$end.mon
    iend.day <- .cdtData$GalParams$date.range[[ie]]
    min.perc <- .cdtData$GalParams$min.perc / 100

    filefrmt <- .cdtData$GalParams$Multiple.File$file.format
    datefrmt <- .cdtData$GalParams$Multiple.File$date.format
    include.elev <- .cdtData$GalParams$Multiple.File$include.elev

    STN.dir <- .cdtData$GalParams$IO.files$STN.dir
    STN.sample <- .cdtData$GalParams$IO.files$STN.sample.file
    STN.info <- .cdtData$GalParams$IO.files$STN.coords.file
    File2Save <- .cdtData$GalParams$IO.files$File2Save

    xinfo <- getStnOpenData(STN.info)
    if(is.null(xinfo)) return(NULL)
    xinfo <- apply(xinfo, 2, trimws)
    xinfo[xinfo == ""] <- NA
    stn.id <- xinfo[, 1]
    stn.lon <- as.numeric(xinfo[, 3])
    stn.lat <- as.numeric(xinfo[, 4])
    stn.elv <- as.numeric(xinfo[, 5])
    xinfo <- cbind(stn.id, stn.lon, stn.lat, stn.elv)

    miss.stn <- list()
    miss.stn$dup.STN.ID <- xinfo[duplicated(xinfo[, 1]) | duplicated(xinfo[, 1], fromLast = TRUE), , drop = FALSE]
    xinfo <- xinfo[!duplicated(xinfo[, 1]), ]
    miss.stn$dup.coords <- xinfo[duplicated(xinfo[, 2:3]) | duplicated(xinfo[, 2:3], fromLast = TRUE), , drop = FALSE]
    miss.stn$miss.coords <- xinfo[is.na(xinfo[, 2]) | is.na(xinfo[, 3]), , drop = FALSE]

    infoheadI <- xinfo[, 1:3]
    capition <- c('Stations', 'LON', paste(toupper(tstep), 'LAT', sep = '/'))
    if(include.elev){
        infoheadI <- xinfo
        capition <- c('Stations', 'LON', 'LAT', paste(toupper(tstep), 'ELV', sep = '/'))
    }

    STN.ext <- tools::file_ext(STN.sample)
    STN.files <- if(STN.ext == "")
                    list.files(STN.dir)
                else
                    tools::file_path_sans_ext(tools::list_files_with_exts(STN.dir, STN.ext, full.names = FALSE))
    istn <- stn.id %in% STN.files
    if(!any(istn)){
        Insert.Messages.Out(.cdtData$GalParams[['message']][['5']], format = TRUE)
        return(NULL)
    }
    miss.stn$no.coords <- STN.files[!STN.files %in% stn.id]

    infoheadI1 <- infoheadI[istn, ]
    stn.id <- stn.id[istn]
    miss.stn$no.data <- infoheadI[!istn, , drop = FALSE]

    donneInfo <- getStnOpenDataInfo(STN.sample)
    if(is.null(donneInfo)) return(NULL)
    donne <- lapply(stn.id, function(x){
        filext <- if(STN.ext == "") x else paste0(x, '.', STN.ext)
        filein <- file.path(STN.dir, filext)
        donne <- try(utils::read.table(filein, header = donneInfo[[3]]$header,
                                        sep = donneInfo[[3]]$sepr,
                                        skip = donneInfo[[3]]$skip - 1, 
                                        na.strings = donneInfo[[3]]$miss.val,
                                        quote = "\"", strip.white = TRUE,
                                        stringsAsFactors = FALSE,
                                        colClasses = "character", comment.char = ""
                                        ),
                                silent = TRUE)
        if(inherits(donne, "try-error")) return(NULL)
        donne <- getCDTTSdataAndDisplayMsg(donne, tstep, filefrmt, datefrmt, STN.sample, display = FALSE)
        return(donne)
    })

    donne.null <- sapply(donne, is.null)
    donne <- donne[!donne.null]
    miss.stn$not.read <- if(any(donne.null)) infoheadI1[donne.null, , drop = FALSE] else NULL
    infoheadI1 <- infoheadI1[!donne.null, ]

    if(tstep == 'daily'){
        istart <- as.Date(paste(istart.yrs, istart.mon, istart.day, sep = '-'))
        iend <- as.Date(paste(iend.yrs, iend.mon, iend.day, sep = '-'))
        odates <- format(seq(istart, iend, 'day'), '%Y%m%d')
    }
    if(tstep == 'dekadal'){
        istart <- as.Date(paste(istart.yrs, istart.mon, istart.day, sep = '-'))
        iend <- as.Date(paste(iend.yrs, iend.mon, iend.day, sep = '-'))
        odates <- seq(istart, iend, 'day')
        odates <- paste0(format(odates[which(as.numeric(format(odates, '%d')) <= 3)], '%Y%m'),
                    as.numeric(format(odates[which(as.numeric(format(odates, '%d')) <= 3)], '%d')))
    }
    if(tstep == 'monthly'){
        istart <- as.Date(paste(istart.yrs, istart.mon, 1, sep = '-'))
        iend <- as.Date(paste(iend.yrs, iend.mon, 1, sep = '-'))
        odates <- format(seq(istart, iend, 'month'), '%Y%m')
    }

    if(filefrmt == "1"){
        donne <- lapply(donne, function(x){
                        xvar <- x$var$var
                        xdaty <- x$dates
                        xvar[match(odates, xdaty)]
                    })
        donne <- do.call(cbind, donne)
        per.var <- 1 - (colSums(is.na(donne)) / length(odates)) >= min.perc
        donne <- donne[, per.var]
        miss.stn$less.data <- infoheadI1[!per.var, , drop = FALSE]
        infoheadI1 <- infoheadI1[per.var, ]
        donne <- rbind(t(rbind(capition, infoheadI1)), cbind(odates, donne))
        donne[is.na(donne)] <- donneInfo[[3]]$miss.val
        writeFiles(donne, File2Save)
    }else{
        donne <- lapply(donne, function(x){
                        xrr <- x$var$rr
                        xtx <- x$var$tx
                        xtn <- x$var$tn
                        xdaty <- x$dates
                        ix <- match(odates, xdaty)
                        xrr <- xrr[ix]
                        xtx <- xtx[ix]
                        xtn <- xtn[ix]
                        list(xrr, xtx, xtn)
                    })
        xrr <- sapply(donne, '[[', 1)
        xtx <- sapply(donne, '[[', 2)
        xtn <- sapply(donne, '[[', 3)

        per.rr <- 1 - (colSums(is.na(xrr)) / length(odates)) >= min.perc
        per.tx <- 1 - (colSums(is.na(xtx)) / length(odates)) >= min.perc
        per.tn <- 1 - (colSums(is.na(xtn)) / length(odates)) >= min.perc

        xrr <- xrr[, per.rr]
        xtx <- xtx[, per.tx]
        xtn <- xtn[, per.tn]

        miss.stn$less.data.rr <- infoheadI1[!per.rr, , drop = FALSE]
        miss.stn$less.data.tx <- infoheadI1[!per.tx, , drop = FALSE]
        miss.stn$less.data.tn <- infoheadI1[!per.tn, , drop = FALSE]

        infoheadI1.rr <- infoheadI1[per.rr, ]
        infoheadI1.tx <- infoheadI1[per.tx, ]
        infoheadI1.tn <- infoheadI1[per.tn, ]

        xrr <- rbind(t(rbind(capition, infoheadI1.rr)), cbind(odates, xrr))
        xtx <- rbind(t(rbind(capition, infoheadI1.tx)), cbind(odates, xtx))
        xtn <- rbind(t(rbind(capition, infoheadI1.tn)), cbind(odates, xtn))

        xrr[is.na(xrr)] <- donneInfo[[3]]$miss.val
        xtx[is.na(xtx)] <- donneInfo[[3]]$miss.val
        xtn[is.na(xtn)] <- donneInfo[[3]]$miss.val

        File2Save.rr <- file.path(dirname(File2Save), paste0('PRECIP_', basename(File2Save)))
        File2Save.tx <- file.path(dirname(File2Save), paste0('TMAX_', basename(File2Save)))
        File2Save.tn <- file.path(dirname(File2Save), paste0('TMIN_', basename(File2Save)))

        writeFiles(xrr, File2Save.rr)
        writeFiles(xtx, File2Save.tx)
        writeFiles(xtn, File2Save.tn)
    }

    outlist <- list()
    if(nrow(miss.stn$dup.STN.ID) > 0){
        outlist <- c(outlist, list(.cdtData$GalParams[['message']][['6']], miss.stn$dup.STN.ID))
    }
    if(nrow(miss.stn$dup.coords) > 0){
        outlist <- c(outlist, list(.cdtData$GalParams[['message']][['7']], miss.stn$dup.coords))
    }
    if(nrow(miss.stn$miss.coords) > 0){
        outlist <- c(outlist, list(.cdtData$GalParams[['message']][['8']], miss.stn$miss.coords))
    }
    if(length(miss.stn$no.coords) > 0){
        outlist <- c(outlist, list(.cdtData$GalParams[['message']][['9']], miss.stn$no.coords))
    }
    if(nrow(miss.stn$no.data) > 0){
        outlist <- c(outlist, list(.cdtData$GalParams[['message']][['10']], miss.stn$no.data))
    }
    if(!is.null(miss.stn$not.read)){
        outlist <- c(outlist, list(.cdtData$GalParams[['message']][['11']], miss.stn$not.read))
    }
    if(filefrmt == "1"){
        if(nrow(miss.stn$less.data) > 0){
            outlist <- c(outlist, list(.cdtData$GalParams[['message']][['12']], miss.stn$less.data))
        }
    }else{
        if(nrow(miss.stn$less.data.rr) > 0){
            outlist <- c(outlist, list(.cdtData$GalParams[['message']][['13']], miss.stn$less.data.rr))
        }
        if(nrow(miss.stn$less.data.tx) > 0){
            outlist <- c(outlist, list(.cdtData$GalParams[['message']][['14']], miss.stn$less.data.tx))
        }
        if(nrow(miss.stn$less.data.tn) > 0){
            outlist <- c(outlist, list(.cdtData$GalParams[['message']][['15']], miss.stn$less.data.tn))
        }
    }

    if(length(outlist) > 0){
        containertab <- Display_Output_Console_Tab(outlist, title = .cdtData$GalParams[['message']][['16']])
        ntab <- update.OpenTabs('ctxt', containertab)
        tkselect(.cdtEnv$tcl$main$tknotes, ntab)
    }
    return(0)
}

###################################################################

formatCDTDataSingle.File <- function(GeneralParameters){
    Insert.Messages.Out(.cdtData$GalParams[['message']][['4']], TRUE, "i")
    tcl("update")

    tstep <- .cdtData$GalParams$tstep
    if(tstep == "daily"){
        is <- "start.day"
        ie <- 'end.day'
    }else{
        is <- "start.dek"
        ie <- 'end.dek'
    }

    istart.yrs <- .cdtData$GalParams$date.range$start.year
    istart.mon <- .cdtData$GalParams$date.range$start.mon
    istart.day <- .cdtData$GalParams$date.range[[is]]
    iend.yrs <- .cdtData$GalParams$date.range$end.year
    iend.mon <- .cdtData$GalParams$date.range$end.mon
    iend.day <- .cdtData$GalParams$date.range[[ie]]
    min.perc <- .cdtData$GalParams$min.perc/100

    include.elev <- .cdtData$GalParams$Single.File$include.elev
    coords.included <- .cdtData$GalParams$Single.File$coords.included

    col.id <- .cdtData$GalParams$Single.File$col.stn.id
    col.lon <- .cdtData$GalParams$Single.File$col.stn.lon
    col.lat <- .cdtData$GalParams$Single.File$col.stn.lat
    col.elv <- .cdtData$GalParams$Single.File$col.stn.elv
    col.yr <- .cdtData$GalParams$Single.File$col.year
    col.mo <- .cdtData$GalParams$Single.File$col.month
    col.dy <- .cdtData$GalParams$Single.File$col.day.dek
    col.dat <- .cdtData$GalParams$Single.File$col.start.data
    nb.column <- .cdtData$GalParams$Single.File$nb.column

    STN.file <- .cdtData$GalParams$IO.files$STN.single.file
    File2Save <- .cdtData$GalParams$IO.files$File2Save

    donne <- getStnOpenData(STN.file)
    if(is.null(donne)) return(NULL)

    donne <- apply(donne, 2, trimws)
    donne[donne == ""] <- NA
    donne <- donne[rowSums(!is.na(donne)) > 0, , drop = FALSE]

    if(tstep == 'daily'){
        if(nb.column == 1)
            ina <- is.na(donne[, col.id]) | is.na(donne[, col.yr]) | is.na(donne[, col.mo]) | is.na(donne[, col.dy])
        if(nb.column == 31)
            ina <- is.na(donne[, col.id]) | is.na(donne[, col.yr]) | is.na(donne[, col.mo])
    }
    if(tstep == 'dekadal'){
        if(nb.column == 1)
            ina <- is.na(donne[, col.id]) | is.na(donne[, col.yr]) | is.na(donne[, col.mo]) | is.na(donne[, col.dy])
        if(nb.column == 3)
            ina <- is.na(donne[, col.id]) | is.na(donne[, col.yr]) | is.na(donne[, col.mo])
        if(nb.column == 36)
            ina <- is.na(donne[, col.id]) | is.na(donne[, col.yr])
    }
    if(tstep == 'monthly'){
        if(nb.column == 1)
            ina <- is.na(donne[, col.id]) | is.na(donne[, col.yr]) | is.na(donne[, col.mo])
        if(nb.column == 12)
            ina <- is.na(donne[, col.id]) | is.na(donne[, col.yr])
    }

    miss.stn <- list()
    miss.stn$vague.info <- donne[ina, , drop = FALSE]
    donne <- donne[!ina, ]

    if(coords.included){
        stn.id <- donne[, col.id]
        stn.lon <- as.numeric(donne[, col.lon])
        stn.lat <- as.numeric(donne[, col.lat])
        stn.elv <- if(include.elev) as.numeric(donne[, col.elv]) else rep(NA, length(stn.lon))
        xinfo <- cbind(stn.id, stn.lon, stn.lat, stn.elv)
        xinfo <- xinfo[!duplicated(xinfo[, 1:3]), , drop = FALSE]

        dupstn <- duplicated(xinfo[, 1]) | duplicated(xinfo[, 1], fromLast = TRUE)
        if(any(dupstn)){
            stn1 <- lapply(unique(xinfo[dupstn, 1]), function(x){
                xx <- xinfo[xinfo[, 1] == x, , drop = FALSE]
                if(any(is.na(xx[, 2:3]))) xx <- xx[rowSums(!is.na(xx[, 2:3])) > 0, , drop = FALSE]
                xx
            })
            xinfo <- rbind(xinfo[!dupstn, , drop = FALSE], do.call(rbind, stn1))
        }
    }else{
        STN.info <- .cdtData$GalParams$IO.files$STN.coords.file
        xinfo <- getStnOpenData(STN.info)
        if(is.null(xinfo)) return(NULL)
        xinfo <- apply(xinfo, 2, trimws)
        if(is.null(dim(xinfo))) xinfo <- matrix(xinfo, nrow = 1)
        xinfo[xinfo == ""] <- NA
        stn.id <- xinfo[, 1]
        stn.lon <- as.numeric(xinfo[, 3])
        stn.lat <- as.numeric(xinfo[, 4])
        stn.elv <- as.numeric(xinfo[, 5])
        xinfo <- cbind(stn.id, stn.lon, stn.lat, stn.elv)
    }

    miss.stn$dup.STN.ID <- xinfo[duplicated(xinfo[, 1]) | duplicated(xinfo[, 1], fromLast = TRUE), , drop = FALSE]
    xinfo <- xinfo[!duplicated(xinfo[, 1]), , drop = FALSE]
    miss.stn$dup.coords <- xinfo[duplicated(xinfo[, 2:3, drop = FALSE]) | duplicated(xinfo[, 2:3, drop = FALSE], fromLast = TRUE), , drop = FALSE]
    miss.stn$miss.coords <- xinfo[is.na(xinfo[, 2]) | is.na(xinfo[, 3]), , drop = FALSE]

    infoheadI <- xinfo[, 1:3, drop = FALSE]
    capition <- c('Stations', 'LON', paste(toupper(tstep), 'LAT', sep = '/'))
    if(include.elev){
        infoheadI <- xinfo
        capition <- c('Stations', 'LON', 'LAT', paste(toupper(tstep), 'ELV', sep = '/'))
    }

    if(tstep == 'daily'){
        istart <- as.Date(paste(istart.yrs, istart.mon, istart.day, sep = '-'))
        iend <- as.Date(paste(iend.yrs, iend.mon, iend.day, sep = '-'))
        odates <- format(seq(istart, iend, 'day'), '%Y%m%d')
        if(nb.column == 1){
            xdonne <- donne[, c(col.id, col.yr, col.mo, col.dy)]
            donne <- donne[, col.dat, drop = FALSE]
        }
        if(nb.column == 31){
            xdonne <- donne[, c(col.id, col.yr, col.mo)]
            donne <- donne[, col.dat + (0:30)]
        }
    }
    if(tstep == 'dekadal'){
        istart <- as.Date(paste(istart.yrs, istart.mon, istart.day, sep = '-'))
        iend <- as.Date(paste(iend.yrs, iend.mon, iend.day, sep = '-'))
        odates <- seq(istart, iend, 'day')
        odates <- paste0(format(odates[which(as.numeric(format(odates, '%d')) <= 3)], '%Y%m'),
                    as.numeric(format(odates[which(as.numeric(format(odates, '%d')) <= 3)], '%d')))
        if(nb.column == 1){
            xdonne <- donne[, c(col.id, col.yr, col.mo, col.dy)]
            donne <- donne[, col.dat, drop = FALSE]
        }
        if(nb.column == 3){
            xdonne <- donne[, c(col.id, col.yr, col.mo)]
            donne <- donne[, col.dat + (0:2)]
        }
        if(nb.column == 36){
            xdonne <- donne[, c(col.id, col.yr)]
            donne <- donne[, col.dat + (0:35)]
        }
    }
    if(tstep == 'monthly'){
        istart <- as.Date(paste(istart.yrs, istart.mon, 1, sep = '-'))
        iend <- as.Date(paste(iend.yrs, iend.mon, 1, sep = '-'))
        odates <- format(seq(istart, iend, 'month'), '%Y%m')
        if(nb.column == 1){
            xdonne <- donne[, c(col.id, col.yr, col.mo)]
            donne <- donne[, col.dat, drop = FALSE]
        }
        if(nb.column == 12){
            xdonne <- donne[, c(col.id, col.yr)]
            donne <- donne[, col.dat + (0:11)]
        }
    }

    donne1 <- convert_data_type(donne, as.numeric)
    miss.stn$non.numeric <- donne[is.na(convert_data_type(donne1, as.character))!= is.na(donne)]
    if(length(miss.stn$non.numeric) > 0)
        miss.stn$non.numeric <- miss.stn$non.numeric[!duplicated(miss.stn$non.numeric)]

    miss.stn$with.data.no.coords <- NULL
    if(!coords.included){
        id.stn.data <- unique(xdonne[, 1])
        isdata <- !id.stn.data %in% infoheadI[, 1]
        if(any(isdata)){
            stn.with.data <- id.stn.data[isdata]
            miss.stn$with.data.no.coords <- stn.with.data
            insert.stn <- matrix(NA, nrow = length(stn.with.data), ncol = ncol(infoheadI))
            insert.stn[, 1] <- stn.with.data
            infoheadI <- rbind(infoheadI, insert.stn)
        }
    }

    donne <- lapply(infoheadI[, 1], function(id){
        ix <- xdonne[, 1] == id
        if(!any(ix)) return(NULL)
        don <- donne1[ix, , drop = FALSE]
        don <- c(t(don))
        xdaty <- xdonne[ix, , drop = FALSE]

        if(tstep == 'daily'){
            if(nb.column == 1)
                daty <- as.Date(paste(xdaty[, 2], xdaty[, 3], xdaty[, 4], sep = '-'))
            if(nb.column == 31)
                daty <- as.Date(paste(paste(rep(paste(xdaty[, 2], xdaty[, 3], sep = '-'), each = 31),
                                        rep(1:31, nrow(xdaty)), sep = '-')))
        }
        if(tstep == 'dekadal'){
            if(nb.column == 1)
                daty <- as.Date(paste(xdaty[, 2], xdaty[, 3], xdaty[, 4], sep = '-'))
            if(nb.column == 3)
                daty <- as.Date(paste(paste(rep(paste(xdaty[, 2], xdaty[, 3], sep = '-'), each = 3),
                                        rep(1:3, nrow(xdaty)), sep = '-')))
            if(nb.column == 36)
                daty <- as.Date(paste(rep(xdaty[, 2], each = 36), rep(paste(rep(1:12, each = 3),
                                    rep(1:3, 12), sep = '-'), nrow(xdaty)), sep = '-'))

            daty[as.numeric(format(daty, '%d')) > 3] <- NA
        }
        if(tstep == 'monthly'){
            if(nb.column == 1)
                daty <- as.Date(paste(xdaty[, 2], xdaty[, 3], 1, sep = '-'))
            if(nb.column == 12)
                daty <- as.Date(paste(rep(xdaty[, 2], each = 12), rep(1:12, nrow(xdaty)), 1, sep = '-'))
        }

        don <- don[!is.na(daty)]
        daty <- daty[!is.na(daty)]
        don <- don[order(daty)]
        daty <- daty[order(daty)]

        if(tstep == 'daily') daty <- format(daty, '%Y%m%d')
        if(tstep == 'dekadal') daty <- paste0(format(daty, '%Y%m'), as.numeric(format(daty, '%d')))
        if(tstep == 'monthly') daty <- format(daty, '%Y%m')

        don <- don[match(odates, daty)]
        don
    })

    donne.null <- sapply(donne, is.null)
    donne <- donne[!donne.null]
    miss.stn$no.data <- if(any(donne.null)) infoheadI[donne.null, , drop = FALSE] else NULL
    infoheadI <- infoheadI[!donne.null, , drop = FALSE]
    donne <- do.call(cbind, donne)

    per.donne <- 1 - (colSums(is.na(donne)) / length(odates)) >= min.perc
    donne <- donne[, per.donne, drop = FALSE]
    miss.stn$less.data <- infoheadI[!per.donne, , drop = FALSE]
    infoheadI <- infoheadI[per.donne, , drop = FALSE]

    infoheadI[, 1] <- substr(gsub("[^[:alnum:]]", "", infoheadI[, 1]), 1, 15)

    donne <- rbind(t(rbind(capition, infoheadI)), cbind(odates, donne))
    donne[is.na(donne)] <- -99
    writeFiles(donne, File2Save)

    outlist <- list()
    if(nrow(miss.stn$vague.info) > 0){
        outlist <- c(outlist, list(.cdtData$GalParams[['message']][['17']], miss.stn$vague.info))
    }
    if(nrow(miss.stn$dup.STN.ID) > 0){
        outlist <- c(outlist, list(.cdtData$GalParams[['message']][['6']], miss.stn$dup.STN.ID))
    }
    if(nrow(miss.stn$dup.coords) > 0){
        outlist <- c(outlist, list(.cdtData$GalParams[['message']][['7']], miss.stn$dup.coords))
    }
    if(nrow(miss.stn$miss.coords) > 0){
        outlist <- c(outlist, list(.cdtData$GalParams[['message']][['8']], miss.stn$miss.coords))
    }
    if(!is.null(miss.stn$with.data.no.coords)){
        outlist <- c(outlist, list(.cdtData$GalParams[['message']][['18']], miss.stn$with.data.no.coords))
    }
    if(length(miss.stn$non.numeric) > 0){
        outlist <- c(outlist, list(.cdtData$GalParams[['message']][['19']], miss.stn$non.numeric))
    }
    if(!is.null(miss.stn$no.data)){
        outlist <- c(outlist, list(.cdtData$GalParams[['message']][['10']], miss.stn$no.data))
    }
    if(nrow(miss.stn$less.data) > 0){
        outlist <- c(outlist, list(.cdtData$GalParams[['message']][['12']], miss.stn$less.data))
    }

    if(length(outlist) > 0){
        containertab <- Display_Output_Console_Tab(outlist, title = .cdtData$GalParams[['message']][['16']])
        ntab <- update.OpenTabs('ctxt', containertab)
        tkselect(.cdtEnv$tcl$main$tknotes, ntab)
    }
    return(0)
}
