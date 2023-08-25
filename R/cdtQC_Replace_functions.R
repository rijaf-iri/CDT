
FalseZerosCheck_replaceAll <- function(file.index, action = c("all", "percentage", "ratio"), threshold = NULL)
{
    if(!file.exists(file.index)) stop(paste(file.index, 'not found'))
    PathData <- dirname(file.index)

    file.checkd <- file.path(PathData, 'CDTDATASET', "FalseZerosResults.rds")
    if(!file.exists(file.checkd))
        stop(paste(file.checkd, 'not found'))

    outzeros <- readRDS(file.checkd)
    if(is.null(outzeros)){
        cat('No suspicious False-Zeros found\n')
        return(NULL)
    }

    file.don <- file.path(PathData, 'CDTDATASET', "StationData.rds")
    if(!file.exists(file.don))
        stop(paste(file.don, 'not found'))

    stn.data <- readRDS(file.don)
    is.elv <- if(is.null(stn.data$elv)) 3 else 4

    output <- readRDS(file.index)
    info <- output$info[[3]]
    file.stn <- file.path(PathData, 'CDTSTATIONS', output$info[[1]])
    tmp <- utils::read.table(file.stn, header = FALSE, sep = info$sepr,
                      stringsAsFactors = FALSE, colClasses = "character")

    action <- action[1]
    if(action != "all"){
        if(missing(threshold))
            stop('Threshold is missing')
        if(is.null(threshold))
            stop('Provide the threshold')
    }

    for(stnid in outzeros$stn){
        daty <- outzeros$res[[stnid]]$date
        if(action != "all"){
            if(action == "percentage")
                thres <- as.character(outzeros$res[[stnid]]$stat$STN.VAL)
            if(action == "ratio")
                thres <- as.character(outzeros$res[[stnid]]$stat$RATIO.NGHBR.AVRG)
            thres <- as.numeric(thres)
            daty <- daty[thres >= threshold]
            if(length(daty) == 0) next
        }

        index.mon <- unlist(output$index[daty])
        index.mon <- index.mon + is.elv
        istn <- which(stn.data$id == stnid) + 1
        replace0 <- tmp[index.mon, istn, drop = FALSE]
        replace0[!is.na(replace0) & replace0 == 0] <- info$miss.val
        tmp[index.mon, istn] <- replace0
    }

    utils::write.table(tmp, file.stn, sep = info$sepr, na = info$miss.val,
                row.names = FALSE, col.names = FALSE, quote = FALSE)
    cat('Replacement done!\n')
}

#################################################################

QC.RR_replaceAll <- function(file.index, action = c("all", "partial"))
{
    if(!file.exists(file.index)) stop(paste(file.index, 'not found'))
    PathData <- dirname(file.index)

    file.qcout <- file.path(PathData, 'CDTDATASET', "QCResults.rds")
    if(!file.exists(file.qcout))
        stop(paste(file.qcout, 'not found'))

    outqc <- readRDS(file.qcout)
    if(is.null(outqc)){
        cat('No suspicious values found\n')
        return(NULL)
    }

    file.don <- file.path(PathData, 'CDTDATASET', "StationData.rds")
    if(!file.exists(file.don))
        stop(paste(file.don, 'not found'))

    stn.data <- readRDS(file.don)
    is.elv <- if(is.null(stn.data$elv)) 3 else 4

    output <- readRDS(file.index)
    info <- output$info[[3]]
    file.stn <- file.path(PathData, 'CDTSTATIONS', output$info[[1]])
    tmp <- utils::read.table(file.stn, header = FALSE, sep = info$sepr,
                      stringsAsFactors = FALSE, colClasses = "character")

    action <- action[1]

    for(stnid in outqc$stn){
        istn <- which(stn.data$id == stnid) + 1
        outliers <- outqc$res[[stnid]]$outliers

        daty <- trimws(as.character(outliers$DATE))
        nonNA <- !is.na(daty) & daty != ""
        outliers <- outliers[nonNA, , drop = FALSE]
        daty <- daty[nonNA]
        idaty <- which(stn.data$dates %in% daty) + is.elv

        stn.val <- as.numeric(as.character(outliers$STN.VAL))
        not.replace <- as.numeric(as.character(outliers$NOT.REPLACE))
        to.replace <- as.numeric(as.character(outliers$REPLACE.VAL))

        if(action == "partial"){
            ina <- is.na(not.replace)
            stn.val[ina] <- info$miss.val
            nna <- !is.na(to.replace)
            stn.val[nna] <- to.replace[nna]
        }
        if(action == "all")
            stn.val[] <- info$miss.val

        tmp[idaty, istn] <- stn.val
    }

    utils::write.table(tmp, file.stn, sep = info$sepr, na = info$miss.val,
                row.names = FALSE, col.names = FALSE, quote = FALSE)
    cat('Replacement done!\n')
}

#################################################################

QC.TT_replaceAll <- function(file.index, action = c("all", "partial", "estimated"))
{
    if(!file.exists(file.index)) stop(paste(file.index, 'not found'))
    PathData <- dirname(file.index)

    file.qcout <- file.path(PathData, 'CDTDATASET', "QCResults.rds")
    if(!file.exists(file.qcout))
        stop(paste(file.qcout, 'not found'))

    outqc <- readRDS(file.qcout)
    if(is.null(outqc)){
        cat('No suspicious values found\n')
        return(NULL)
    }

    file.don <- file.path(PathData, 'CDTDATASET', "StationData.rds")
    if(!file.exists(file.don))
        stop(paste(file.don, 'not found'))

    stn.data <- readRDS(file.don)
    is.elv <- if(is.null(stn.data$elv)) 3 else 4

    output <- readRDS(file.index)
    info <- output$info[[3]]
    file.stn <- file.path(PathData, 'CDTSTATIONS', output$info[[1]])
    tmp <- utils::read.table(file.stn, header = FALSE, sep = info$sepr,
                      stringsAsFactors = FALSE, colClasses = "character")

    action <- action[1]

    for(stnid in outqc$stn){
        istn <- which(stn.data$id == stnid) + 1
        outliers <- outqc$res[[stnid]]$outliers

        daty <- trimws(as.character(outliers$DATE))
        nonNA <- !is.na(daty) & daty != ""
        outliers <- outliers[nonNA, , drop = FALSE]
        daty <- daty[nonNA]
        idaty <- which(stn.data$dates %in% daty) + is.elv

        stn.val <- as.numeric(as.character(outliers$STN.VAL))
        est.val <- as.numeric(as.character(outliers$ESTIMATED.VAL))
        not.replace <- as.numeric(as.character(outliers$NOT.REPLACE))
        to.replace <- as.numeric(as.character(outliers$REPLACE.VAL))

        if(action == "partial"){
            ina <- is.na(not.replace)
            stn.val[ina] <- info$miss.val
            stn.val[!ina & not.replace == 2] <- est.val[!ina & not.replace == 2]
            nna <- !is.na(to.replace)
            stn.val[nna] <- to.replace[nna]
        }
        if(action == "estimated"){
            stn.val[] <- info$miss.val
            # ina <- !is.na(not.replace) & not.replace == 2
            # stn.val[ina] <- est.val[ina]
            # nna <- !is.na(to.replace)
            # stn.val[nna] <- to.replace[nna]

            ina <- !is.na(est.val)
            stn.val[ina] <- est.val[ina]
        }
        if(action == "all")
            stn.val[] <- info$miss.val

        tmp[idaty, istn] <- stn.val
    }

    utils::write.table(tmp, file.stn, sep = info$sepr, na = info$miss.val,
                row.names = FALSE, col.names = FALSE, quote = FALSE)
    cat('Replacement done!\n')
}
