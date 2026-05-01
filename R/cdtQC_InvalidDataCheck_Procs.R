
qcInvalidDataCheckProcs <- function(GeneralParameters){
    message <- .cdtData$EnvData[['message']]
    don <- getStnOpenData(GeneralParameters$infile)
    if(is.null(don)) return(NULL)
    head <- don[1:4, 1]
    don <- getCDTdataAndDisplayMsg(don, GeneralParameters$intstep, GeneralParameters$infile)
    if(is.null(don)) return(NULL)

    ###################
    if(!dir.exists(GeneralParameters$outdir)){
        outdir <- getwd()
    }else{
        outdir <- GeneralParameters$outdir
    }
    outdir <- file.path(outdir, "INVALID.CHECK_data")
    dataCDTdir <- file.path(outdir, 'CDTDATASET')
    dir.create(dataCDTdir, showWarnings = FALSE, recursive = TRUE)
    dataSTNdir <- file.path(outdir, 'CDTSTATIONS')
    dir.create(dataSTNdir, showWarnings = FALSE, recursive = TRUE)
    file.stn <- file.path(dataSTNdir, GeneralParameters$infile)

    ###################
    don.info <- getStnOpenDataInfo(GeneralParameters$infile)
    if(don.info[[3]]$sepr == "") don.info[[3]]$sepr <- " "
    don.info[[3]]$header <- FALSE
    don.info[[3]]$skip <- 0
    if(is.null(don$elv)) head <- head[1:3]
    don0 <- rbind(cbind(head, do.call(rbind, don[c('id', 'lon', 'lat', 'elv')])),
                  cbind(don$dates, don$data))
    utils::write.table(don0, file = file.stn,
                sep = don.info[[3]]$sepr, na = don.info[[3]]$miss.val,
                col.names = FALSE, row.names = FALSE, quote = FALSE)

    ###################
    params <- GeneralParameters$params
    don.qc <- don$data
    don.len <- nrow(don.qc)
    outqc <- NULL

    ###################
    ## Check consecutive values

    min.len <- 5
    outqc.seq <- NULL

    if(don.len >= min.len){
        tmp <- don.qc
        tmp[is.na(tmp)] <- 0
        tmp <- rbind(diff(tmp), 0)
        tmp <- tmp == 1
        # tmp <- Reduce(`|`, lapply(1:10,`==`, tmp))

        iseq <- lapply(seq(ncol(tmp)), function(j){
            x <- tmp[, j]
            x <- rle(x)
            if(!any(x$lengths[x$values] >= min.len)) return(NULL)
            ie <- cumsum(x$lengths)
            is <- c(1, ie[-length(ie)] + 1)
            ie <- ie[x$values & x$lengths >= min.len]
            is <- is[x$values & x$lengths >= min.len]
            ie <- ie + 1
            ie[ie > don.len] <- don.len
            cbind(is, ie, 0)
        })

        inull <- sapply(iseq, is.null)
        iseq <- iseq[!inull]

        if(length(iseq) > 0){
            istn <- which(!inull)
            xtmp <- lapply(seq_along(iseq), function(j){
                ix <- iseq[[j]]
                xx <- lapply(seq(nrow(ix)), function(i){
                  is <- ix[i, 1]:ix[i, 2]
                  cbind(don$dates[is], don.qc[is, istn[j]])
                })
                xx <- do.call(rbind, xx)
                id <- don$id[istn[j]]
                tab <- data.frame(id, xx)
                norep <- rep(NA, nrow(tab))
                repval <- rep(NA, nrow(tab))
                tab <- cbind.data.frame(tab, norep = norep, repval = repval)
                names(tab) <- c("STN.ID", "DATE", "STN.VAL",
                                "NOT.REPLACE", "REPLACE.VAL")
                list(tab = tab, index = ix)
            })
            names(xtmp) <- don$id[istn]
            outqc.seq <- list(res = xtmp, stn = don$id[istn])
        }
    }


    return(0)
}
