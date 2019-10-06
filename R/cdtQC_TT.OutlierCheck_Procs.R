
qcTTOutliersCheckProcs <- function(GeneralParameters){
    if(GeneralParameters$infile1 == ""){
        Insert.Messages.Out(.cdtData$EnvData[['message']][['15']], TRUE, "e")
        return(NULL)
    }

    don <- getStnOpenData(GeneralParameters$infile1)
    if(is.null(don)) return(NULL)
    head <- don[1:4, 1]
    don <- getCDTdataAndDisplayMsg(don, GeneralParameters$intstep, GeneralParameters$infile1)
    if(is.null(don)) return(NULL)

    if(GeneralParameters$infile2 != ""){
        don2 <- getStnOpenData(GeneralParameters$infile2)
        if(!is.null(don2))
            don2 <- getCDTdataAndDisplayMsg(don2, GeneralParameters$intstep, GeneralParameters$infile2)
    }else don2 <- NULL

    noCompare <- FALSE
    if(is.null(don2)){
        ms <- if(GeneralParameters$qc.tmax) "TMIN" else "TMAX"
        Insert.Messages.Out(paste(ms, .cdtData$EnvData[['message']][['19']]), TRUE, "e")
        noCompare <- TRUE
    }else{
        if(!any(don$id %in% don2$id)){
            Insert.Messages.Out(.cdtData$EnvData[['message']][['16']], TRUE, "e")
            noCompare <- TRUE
        }
        if(!any(don$dates %in% don2$dates)){
            Insert.Messages.Out(.cdtData$EnvData[['message']][['17']], TRUE, "e")
            noCompare <- TRUE
        }
    }
    if(noCompare)
        Insert.Messages.Out(.cdtData$EnvData[['message']][['18']], TRUE, "w")

    ###################
    sfdir <- if(GeneralParameters$qc.tmax) "TMAX" else "TMIN"
    outdir <- file.path(GeneralParameters$outdir, paste0(sfdir, ".OUTLIERS.CHECK_data"))
    dataCDTdir <- file.path(outdir, 'CDTDATASET')
    dir.create(dataCDTdir, showWarnings = FALSE, recursive = TRUE)
    dataSTNdir <- file.path(outdir, 'CDTSTATIONS')
    dir.create(dataSTNdir, showWarnings = FALSE, recursive = TRUE)
    file.stn <- file.path(dataSTNdir, GeneralParameters$infile1)

    ###################
    don.info <- getStnOpenDataInfo(GeneralParameters$infile1)
    if(don.info[[3]]$sepr == "") don.info[[3]]$sepr <- " "
    don.info[[3]]$header <- FALSE
    don.info[[3]]$skip <- 0
    if(is.null(don$elv)) head <- head[1:3]
    don0 <- rbind(cbind(head, do.call(rbind, don[c('id', 'lon', 'lat', 'elv')])),
                  cbind(don$dates, don$data))
    write.table(don0, file = file.stn,
                sep = don.info[[3]]$sepr, na = don.info[[3]]$miss.val,
                col.names = FALSE, row.names = FALSE, quote = FALSE)

    ###################

    elv.data <- NULL
    if(GeneralParameters$params$elv$use)
    {
        if(GeneralParameters$params$elv$dem)
        {
            readDEM <- TRUE
            if(GeneralParameters$params$elv$file == ""){
                Insert.Messages.Out(.cdtData$EnvData[['message']][['11']], TRUE, "e")
                readDEM <- FALSE
            }

            demInfo <- getNCDFSampleData(GeneralParameters$params$elv$file)
            if(is.null(demInfo)){
                Insert.Messages.Out(.cdtData$EnvData[['message']][['12']], TRUE, "e")
                readDEM <- FALSE
            }

            if(readDEM){
                jfile <- getIndex.AllOpenFiles(GeneralParameters$params$elv$file)
                ncdata <- .cdtData$OpenFiles$Data[[jfile]][[2]]
                elv.data <- ncdata$z

                ijx <- grid2pointINDEX(don[c('lon', 'lat')], list(lon = ncdata$x, lat = ncdata$y))
                elv.data <- elv.data[ijx]
                rm(ncdata, ijx)
            }
        }else{
            if(is.null(don$elv))
                Insert.Messages.Out(.cdtData$EnvData[['message']][['13']], TRUE, "e")
            else
                elv.data <- don$elv
        }

        if(is.null(elv.data))
            Insert.Messages.Out(.cdtData$EnvData[['message']][['14']], TRUE, "w")
    }

    ###################

    params <- GeneralParameters$params

    ###################

    voisin  <- lapply(seq_along(don$id), function(stn){
        istn <- seq_along(don$lon)
        crd0 <- cbind(don$lon[stn], don$lat[stn])
        crds <- do.call(cbind, don[c('lon', 'lat')])
        dist <- as.numeric(fields::rdist.earth(crd0, crds, miles = FALSE))
        io <- order(dist)
        istn <- istn[io]
        dist <- dist[io]
        idst <- dist <= params$voisin$dist
        idst[istn == stn] <- FALSE
        istn <- istn[idst]
        dist <- dist[idst]
        if(length(istn) < params$voisin$min) return(NULL)
        if(!is.null(elv.data)){
            if(!is.na(elv.data[stn])){
                elv <- elv.data[istn]
                elv.h <- elv.data[stn] + c(-1, 1) * (params$voisin$elv / 2)
                ielv <- elv >= elv.h[1] & elv <= elv.h[2]
                if(any(ielv)){
                    istn <- istn[ielv]
                    dist <- dist[ielv]
                }
            }
        }
        list(id = don$id[stn], stn = c(stn, istn), dist = c(0, dist))
    })

    inull <- sapply(voisin, is.null)
    voisin <- voisin[!inull]

    ###################

    don.qc <- don$data
    index.date <- seq_along(don$dates)
    index.mon <- split(seq_along(don$dates), substr(don$dates, 5, 6))

    ###################
    # Tmax & Tmin comparison

    outqc.txtn <- NULL
    if(!noCompare){
        tmax <- if(GeneralParameters$qc.tmax) don else don2
        tmin <- if(GeneralParameters$qc.tmax) don2 else don
        idaty <- match(tmin$dates, tmax$dates)
        idaty <- idaty[!is.na(idaty)]
        daty <- tmax$dates[idaty]
        tmax$data <- tmax$data[idaty, , drop = FALSE]
        tmin$data <- tmin$data[tmin$dates %in% tmax$dates, , drop = FALSE]

        id <- match(tmin$id, tmax$id)
        id <- id[!is.na(id)]
        stn.id <- tmax$id[id]
        tmax$data <- tmax$data[, id, drop = FALSE]
        tmin$data <- tmin$data[, tmin$id %in% tmax$id, drop = FALSE]

        invtxtn <- which(!is.na(tmax$data) & !is.na(tmin$data) & tmax$data <= tmin$data, arr.ind = TRUE)
        if(nrow(invtxtn) > 0){
            outqc.txtn$status.tmp <- "TMIN >= TMAX"
            outqc.txtn$status.sp <- NA
            outqc.txtn$stn.id <- stn.id[invtxtn[, 2]]
            outqc.txtn$dates <- daty[invtxtn[, 1]]
            if(GeneralParameters$qc.tmax){
                outqc.txtn$stn.val <- tmax$data[invtxtn]
                tvar <- tmin$data[invtxtn]
            }else{
                outqc.txtn$stn.val <- tmin$data[invtxtn]
                tvar <- tmax$data[invtxtn]
            }
            outqc.txtn$stats.tmp <- NA
            outqc.txtn$stats.sp <- NA
            outqc.txtn$est.sp <- NA
            outqc.txtn$minmax <- tvar

            id <- match(outqc.txtn$stn.id, don$id)
            it <- match(outqc.txtn$dates, don$dates)
            don.qc[cbind(it, id)] <- NA

            outqc.txtn <- data.frame(do.call(cbind, outqc.txtn), stringsAsFactors = FALSE)
        }
    }

    ###################
    # out bounds check

    imin <- which(!is.na(don.qc) & don.qc < params$temp.min, arr.ind = TRUE)
    outqc.min <- NULL
    if(nrow(imin)){
        outqc.min$status.tmp <- "less.than.minimum.values"
        outqc.min$status.sp <- NA
        outqc.min$stn.id <- don$id[imin[, 2]]
        outqc.min$dates <- don$dates[imin[, 1]]
        outqc.min$stn.val <- don.qc[imin]
        outqc.min$stats.tmp <- NA
        outqc.min$stats.sp <- NA
        outqc.min$est.sp <- NA
        outqc.min$minmax <- NA

        don.qc[imin] <- NA

        outqc.min <- data.frame(do.call(cbind, outqc.min), stringsAsFactors = FALSE)
    }

    imax <- which(!is.na(don.qc) & don.qc > params$temp.max, arr.ind = TRUE)
    outqc.max <- NULL
    if(nrow(imax)){
        outqc.max$status.tmp <- "greater.than.maximum.values"
        outqc.max$status.sp <- NA
        outqc.max$stn.id <- don$id[imax[, 2]]
        outqc.max$dates <- don$dates[imax[, 1]]
        outqc.max$stn.val <- don.qc[imax]
        outqc.max$stats.tmp <- NA
        outqc.max$stats.sp <- NA
        outqc.max$est.sp <- NA
        outqc.max$minmax <- NA

        don.qc[imax] <- NA

        outqc.max <- data.frame(do.call(cbind, outqc.max), stringsAsFactors = FALSE)
    }

    ###################
    # temporal check
    ## check only data greater than the 97% percentile
    # thres.perc <- 0.97
    
    ## convert to fraction (68–95–99.7 rule)
    alpha <- pnorm(params$sigma) - (1 - pnorm(params$sigma))

    mfactor <- qnorm(1 - ((1 - alpha) / 2))
    xwin <- floor(params$window / 2)
    ndate <- length(don$dates)

    ## min non-missing
    min.length <- switch(GeneralParameters$intstep,
                            'daily' = 300,
                            'pentad' = 60,
                            'dekadal' = 30,
                            'monthly' = 10
                        )

    outliers <- lapply(index.mon, function(it){
        x <- don.qc[it, , drop = FALSE]

        istn <- which(colSums(!is.na(x)) >= min.length)
        if(length(istn) == 0) return(NULL)
        x <- x[, istn, drop = FALSE]

        is <- which(matrixStats::colVars(x, na.rm = TRUE) > 0)
        if(length(is) == 0) return(NULL)
        istn <- istn[is]
        x <- x[, is, drop = FALSE]

        q50 <- matrixStats::colMedians(x, na.rm = TRUE)
        iqr <- matrixStats::colIQRs(x, na.rm = TRUE, type = 8)

        iq <- which(iqr > 0)
        if(length(iq) == 0) return(NULL)
        istn <- istn[iq]
        q50 <- q50[iq]
        iqr <- iqr[iq]
        x <- x[, iq, drop = FALSE]

        qdif <- sweep(x, 2, q50, FUN = '-')
        stats <- sweep(qdif, 2, mfactor * iqr, FUN = '/')

        iup <- which(!is.na(stats) & stats > 1, arr.ind = TRUE)
        ilow <- which(!is.na(stats) & stats < -1, arr.ind = TRUE)
        if(nrow(iup) == 0 & nrow(ilow) == 0) return(NULL)

        out.up <- NULL
        if(nrow(iup)){
            out.up <- list(val = x[iup], stat = round(stats[iup], 4),
                           istn = istn[iup[, 2]], idate = it[iup[, 1]])
        }

        out.low <- NULL
        if(nrow(ilow)){
            out.low <- list(val = x[ilow], stat = round(abs(stats[ilow]), 4),
                            istn = istn[ilow[, 2]], idate = it[ilow[, 1]])
        }

        list(up = out.up, low = out.low)
    })

    inull <- sapply(outliers, is.null)
    outliers <- outliers[!inull]

    outliers.up <- lapply(outliers, "[[", "up")
    outliers.low <- lapply(outliers, "[[", "low")

    ###################
    inull <- sapply(outliers.up, is.null)
    outliers.up <- outliers.up[!inull]
    tmp.date.up <- NULL
    outqc.outlier.up <- NULL
    if(length(outliers.up)){
        outqc.outlier.up$status.tmp <- "upper.outliers"
        outqc.outlier.up$status.sp <- NA
        outqc.outlier.up$stn.id <- don$id[do.call(c, lapply(outliers.up, '[[', 'istn'))]
        outqc.outlier.up$dates <- don$dates[do.call(c, lapply(outliers.up, '[[', 'idate'))]
        outqc.outlier.up$stn.val <- unname(do.call(c, lapply(outliers.up, '[[', 'val')))
        outqc.outlier.up$stats.tmp <- unname(do.call(c, lapply(outliers.up, '[[', 'stat')))
        outqc.outlier.up$stats.sp <- NA
        outqc.outlier.up$est.sp <- NA
        outqc.outlier.up$minmax <- NA

        tmp.date.up <- split(outqc.outlier.up$dates, outqc.outlier.up$stn.id)
        tmp.date.up <- lapply(tmp.date.up, function(x) which(don$dates %in% x))

        outqc.outlier.up <- data.frame(do.call(cbind, outqc.outlier.up), stringsAsFactors = FALSE)
    }

    ###################
    inull <- sapply(outliers.low, is.null)
    outliers.low <- outliers.low[!inull]
    tmp.date.low <- NULL
    outqc.outlier.low <- NULL
    if(length(outliers.low)){
        outqc.outlier.low$status.tmp <- "lower.outliers"
        outqc.outlier.low$status.sp <- NA
        outqc.outlier.low$stn.id <- don$id[do.call(c, lapply(outliers.low, '[[', 'istn'))]
        outqc.outlier.low$dates <- don$dates[do.call(c, lapply(outliers.low, '[[', 'idate'))]
        outqc.outlier.low$stn.val <- unname(do.call(c, lapply(outliers.low, '[[', 'val')))
        outqc.outlier.low$stats.tmp <- unname(do.call(c, lapply(outliers.low, '[[', 'stat')))
        outqc.outlier.low$stats.sp <- NA
        outqc.outlier.low$est.sp <- NA
        outqc.outlier.low$minmax <- NA

        tmp.date.low <- split(outqc.outlier.low$dates, outqc.outlier.low$stn.id)
        tmp.date.low <- lapply(tmp.date.low, function(x) which(don$dates %in% x))

        outqc.outlier.low <- data.frame(do.call(cbind, outqc.outlier.low), stringsAsFactors = FALSE)
    }

    ###################
    if(!is.null(tmp.date.up) & !is.null(tmp.date.low))
        tmp.date <- append.list(tmp.date.up, tmp.date.low)
    else if(!is.null(tmp.date.up) & is.null(tmp.date.low))
        tmp.date <- tmp.date.up
    else if(is.null(tmp.date.up) & !is.null(tmp.date.low))
        tmp.date <- tmp.date.low
    else
        tmp.date <- NULL

    ###################
    # spatial check

    outqc.spatial <- NULL
    if(length(voisin)){
        Insert.Messages.Out("Start spatial check ....", TRUE, "i")

        STNid <- lapply(voisin, "[[", "id")
        STNsp <- lapply(voisin, "[[", "stn")

        ## include temporal outliers
        if(!is.null(tmp.date))
            tmp.date <- tmp.date[match(unlist(STNid), names(tmp.date))]
        else
            tmp.date <- vector(mode = "list", length = length(STNid))

        parsL <- doparallel.cond(length(STNsp) >= 50)
        ret <- cdt.foreach(seq_along(STNsp), parsL, GUI = TRUE,
                           progress = TRUE, FUN = function(j)
        {
            x <- don.qc[, STNsp[[j]], drop = FALSE]
            idaty <- NULL
            ## remove missing
            ina <- which(!is.na(x[, 1]))
            if(length(ina))
            {
                x <- x[ina, , drop = FALSE]
                idaty <- index.date[ina]

                ## get minimum number of neighbors
                idt <- which(rowSums(!is.na(x[, -1, drop = FALSE])) >= params$voisin$min)
                if(length(idt))
                {
                    x <- x[idt, , drop = FALSE]
                    idaty <- idaty[idt]

                    # exclude variance null
                    is <- which(matrixStats::rowVars(x, na.rm = TRUE) > 0)
                    if(length(is))
                    {
                        x <- x[is, , drop = FALSE]
                        idaty <- idaty[is]

                        Q <- matrixStats::rowQuantiles(x[, -1, drop = FALSE], probs = c(0.25, 0.75),
                                                        na.rm = TRUE, type = 8, drop = FALSE)
                        iqr <- Q[, 2] - Q[, 1]
                        xlo <- Q[, 1] - 1.5 * iqr
                        xup <- Q[, 2] + 1.5 * iqr

                        iq <- which(x[, 1] < xlo | x[, 1] > xup)
                        if(length(iq)) idaty <- idaty[iq]
                    }
                }
            }

            idaty <- c(idaty, tmp.date[[j]])
            if(is.null(idaty)) return(NULL)
            idaty <- sort(unique(idaty))

            XE <- STATs <- rep(NA, length(idaty))
            for(i in seq_along(idaty)){
                if(idaty[i] <= xwin){
                    ix <- seq(2 * xwin + 1)
                    ipos <- idaty[i]
                }else if(idaty[i] > (ndate - xwin)){
                    ix <- (ndate - (2 * xwin)):ndate
                    ipos <- idaty[i] - ndate + 2 * xwin + 1
                }else{
                    ix <- (-xwin:xwin) + idaty[i]
                    ipos <- xwin + 1
                }

                nl <- length(ix)
                min.len <- round(nl * 0.7)

                xreg <- don.qc[ix, STNsp[[j]], drop = FALSE]
                xval <- xreg[ipos, 1]
                nx <- colSums(!is.na(xreg)) / nl > 0.7

                if(!nx[1]) next
                if(length(which(nx[-1])) < params$voisin$min) next

                xreg <- xreg[, nx, drop = FALSE]
                x0 <- matrix(xreg[, 1], nl, ncol(xreg[, -1]))
                xreg <- xreg[, -1]
                reg <- regression.Matrix(xreg, x0, min.len)
                xpr <- sweep(xreg, 2, reg[1, ], FUN = '*')
                xpr <- sweep(xpr, 2, reg[5, ], FUN = '+')
                res <- colMeans((x0 - xpr)^2, na.rm = TRUE)
                prd <- xpr[ipos, ]
                npr <- is.na(prd)
                if(any(npr))
                    prd[npr] <- matrixStats::colMedians(xpr, na.rm = TRUE)[npr]

                ir <- which(round(res, 5) != 0)
                if(length(ir) == 0) next
                res <- res[ir]
                prd <- prd[ir]
                if(length(res) < params$voisin$min) next
                xe <- sum(prd/res) / sum(1/res)
                se <- sqrt(length(res) / sum(1/res))

                STATs[i] <- (xval - xe) / (mfactor * se)
                XE[i] <- xe
            }

            ina <- which(!is.na(STATs))
            if(length(ina) == 0) return(NULL)
            XE <- XE[ina]
            STATs <- STATs[ina]
            idaty <- idaty[ina]

            tmp.chk <- rep(FALSE, length(idaty))
            if(!is.null(tmp.date[[j]])) tmp.chk <- idaty %in% tmp.date[[j]]
            isp <- STATs >= -1 & STATs <= 1 & tmp.chk
            spatial.ok <- NULL
            if(any(isp)){
                spatial.ok$val <- don.qc[idaty[isp], STNsp[[j]][1]]
                spatial.ok$it <- idaty[isp]
                spatial.ok$stats <- abs(STATs[isp])
                spatial.ok$est <- XE[isp]
            }

            ilow <- STATs < -1
            spatial.low <- NULL
            if(any(ilow)){
                spatial.low$val <- don.qc[idaty[ilow], STNsp[[j]][1]]
                spatial.low$it <- idaty[ilow]
                spatial.low$stats <- abs(STATs[ilow])
                spatial.low$est <- XE[ilow]
            }

            iup <- STATs > 1
            spatial.high <- NULL
            if(any(iup)){
                spatial.high$val <- don.qc[idaty[iup], STNsp[[j]][1]]
                spatial.high$it <- idaty[iup]
                spatial.high$stats <- STATs[iup]
                spatial.high$est <- XE[iup]
            }

            list(spatial.low = spatial.low, spatial.high = spatial.high, spatial.ok = spatial.ok)
        })

        inull <- sapply(ret, is.null)
        ret <- ret[!inull]
        STNid <- STNid[!inull]
        if(length(ret)){
            spatial.low <- lapply(ret, '[[', 'spatial.low')
            spatial.low <- qcTT.format.spatial(spatial.low, STNid, don$dates, "spatial.low.value")
            spatial.high <- lapply(ret, '[[', 'spatial.high')
            spatial.high <- qcTT.format.spatial(spatial.high, STNid, don$dates, "spatial.high.value")
            spatial.ok <- lapply(ret, '[[', 'spatial.ok')
            spatial.ok <- qcTT.format.spatial(spatial.ok, STNid, don$dates, "spatial.not.outliers")

            outqc.spatial <- rbind(spatial.low, spatial.high, spatial.ok)

            outqc.spatial$minmax <- NA
        }

        Insert.Messages.Out("Spatial check done", TRUE, "s")
    }

    ###################

    outqc <- rbind(outqc.txtn, outqc.min, outqc.max, outqc.outlier.up, outqc.outlier.low, outqc.spatial)

    if(!is.null(outqc)){
        index.stn <- split(seq(nrow(outqc)), outqc$stn.id)
        nom.stn <- names(index.stn)
        inom <- c("stn.id", "dates", "stn.val", "stats.tmp",
                  "stats.sp", "status.tmp", "status.sp", "est.sp", "minmax")
        nvar <- if(GeneralParameters$qc.tmax) "TMIN" else "TMAX"
        nom <- c("STN.ID", "DATE", "STN.VAL", "STAT.TEMPORAL",
                 "STAT.SPATIAL", "OUT.TEMPORAL", "OUT.SPATIAL",
                 "ESTIMATED.VAL", "NOT.REPLACE", "REPLACE.VAL", nvar)
        replace0 <- c("TMIN >= TMAX", "less.than.minimum.values", "greater.than.maximum.values")

        if(length(voisin)){
            STNid <- sapply(voisin, "[[", "id")
            voisin.qc <- voisin[match(nom.stn, STNid)]
        }else voisin.qc <- NULL

        outqc <- lapply(seq_along(index.stn), function(jj){
            x <- outqc[index.stn[[jj]], , drop = FALSE]
            id <- duplicated(x$dates) | duplicated(x$dates, fromLast = TRUE)
            nodup <- x[!id, , drop = FALSE]
            dup <- x[id, , drop = FALSE]
            if(nrow(dup)){
                idx <- split(seq_along(dup$dates), dup$dates)
                dup <- lapply(idx, function(jd){
                    y <- dup[jd, ]
                    y[1, is.na(y[1, ])] <- y[2, is.na(y[1, ])]
                    y[1, , drop = FALSE]
                })
                dup <- do.call(rbind.data.frame, dup)
                x <- if(nrow(nodup)) rbind.data.frame(nodup, dup) else dup
            }else x <- nodup

            x <- x[order(x$dates), inom, drop = FALSE]
            rownames(x) <- NULL
            norep <- rep(1, nrow(x))
            norep[x$status.tmp %in% replace0] <- NA
            repval <- rep(NA, nrow(x))
            x <- cbind.data.frame(x[, 1:8, drop = FALSE],
                                  norep = norep, repval = repval,
                                  x[, 9, drop = FALSE])
            names(x) <- nom
            res <- list(date = x$DATE, outliers = x,
                        stn = voisin.qc[[jj]]$stn[-1],
                        dist = voisin.qc[[jj]]$dist[-1])
            return(res)
        })
        names(outqc) <- nom.stn
        outqc <- list(res = outqc, stn = nom.stn)
    }else outqc <- NULL

    ###################

    .cdtData$EnvData$outqc <- outqc
    ix <- c('id', 'lon', 'lat', 'dates', 'data')
    if(!is.null(don$elv)) ix <- c(ix, 'elv')
    .cdtData$EnvData$stn.data <- don[ix]

    output <- list(params = GeneralParameters, info = don.info)
    .cdtData$EnvData$output <- output
    .cdtData$EnvData$PathData <- outdir

    ###################
    file.index <- file.path(outdir, "OutliersCheck.rds")
    file.qc <- file.path(dataCDTdir, "QCResults.rds")
    file.don <- file.path(dataCDTdir, "StationData.rds")

    saveRDS(output, file.index)
    saveRDS(outqc, file.qc)
    saveRDS(don[ix], file.don)

    return(0)
}

#####################################################################

qcTT.format.spatial <- function(sp.out, stn.id, dates, outlier.name)
{
    inull <- sapply(sp.out, is.null)
    sp.out <- sp.out[!inull]
    id0 <- stn.id[!inull]
    res <- NULL
    if(length(id0)){
        res$status.tmp <- NA
        res$status.sp <- outlier.name
        tmp <- do.call(rbind, lapply(seq_along(id0), function(j){
                x <- sp.out[[j]]
                cbind(id0[[j]], x$val, dates[x$it], x$stats, x$est)
            })
        )
        res$stn.id <- tmp[, 1]
        res$dates <- tmp[, 3]
        res$stn.val <- as.numeric(tmp[, 2])
        res$stats.tmp <- NA
        res$stats.sp <- round(as.numeric(tmp[, 4]), 4)
        res$est.sp <- round(as.numeric(tmp[, 5]), 1)
        res <- data.frame(do.call(cbind, res), stringsAsFactors = FALSE)
    }
    return(res)
}

