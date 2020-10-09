
qcRROutliersCheckProcs <- function(GeneralParameters){
    message <- .cdtData$EnvData[['message']]
    don <- getStnOpenData(GeneralParameters$infile)
    if(is.null(don)) return(NULL)
    head <- don[1:4, 1]
    don <- getCDTdataAndDisplayMsg(don, GeneralParameters$intstep, GeneralParameters$infile)
    if(is.null(don)) return(NULL)

    ###################
    outdir <- file.path(GeneralParameters$outdir, "RR.OUTLIERS.CHECK_data")
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
                Insert.Messages.Out(message[['11']], TRUE, "e")
                readDEM <- FALSE
            }

            demInfo <- getNCDFSampleData(GeneralParameters$params$elv$file)
            if(is.null(demInfo)){
                Insert.Messages.Out(message[['12']], TRUE, "e")
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
                Insert.Messages.Out(message[['13']], TRUE, "e")
            else
                elv.data <- don$elv
        }

        if(is.null(elv.data))
            Insert.Messages.Out(message[['14']], TRUE, "w")
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
        istn0 <- istn
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
        list(id = don$id[stn], stn = c(stn, istn), dist = c(0, dist), ivois = istn0)
    })

    inull <- sapply(voisin, is.null)
    voisin <- voisin[!inull]

    ###################

    don.qc <- don$data
    index.date <- seq_along(don$dates)
    index.mon <- split(seq_along(don$dates), substr(don$dates, 5, 6))

    don.len <- nrow(don.qc)

    ###################

    ## min non-missing
    min.length <- switch(GeneralParameters$intstep,
                            'daily' = 300,
                            'pentad' = 60,
                            'dekadal' = 30,
                            'monthly' = 10
                        )

    Insert.Messages.Out(message[['15']], TRUE, "i")

    ###################
    # out bounds check

    ineg <- which(!is.na(don.qc) & don.qc < 0, arr.ind = TRUE)
    outqc.neg <- NULL
    if(nrow(ineg)){
        outqc.neg$status.tmp <- "negative.values"
        outqc.neg$status.sp <- NA
        outqc.neg$stn.id <- don$id[ineg[, 2]]
        outqc.neg$dates <- don$dates[ineg[, 1]]
        outqc.neg$stn.val <- don.qc[ineg]
        outqc.neg$stats.tmp <- NA
        outqc.neg$stats.sp <- NA
        don.qc[ineg] <- NA

        outqc.neg <- data.frame(do.call(cbind, outqc.neg), stringsAsFactors = FALSE)
    }

    imax <- which(!is.na(don.qc) & don.qc > params$precip.max, arr.ind = TRUE)
    outqc.max <- NULL
    if(nrow(imax)){
        outqc.max$status.tmp <- "greater.than.maximum.values"
        outqc.max$status.sp <- NA
        outqc.max$stn.id <- don$id[imax[, 2]]
        outqc.max$dates <- don$dates[imax[, 1]]
        outqc.max$stn.val <- don.qc[imax]
        outqc.max$stats.tmp <- NA
        outqc.max$stats.sp <- NA
        # don.qc[imax] <- NA

        outqc.max <- data.frame(do.call(cbind, outqc.max), stringsAsFactors = FALSE)
    }

    ###################
    # temporal check
    ## check only data greater than the 97% percentile
    thres.perc <- 0.97

    ## convert to fraction (68–95–99.7 rule)
    alpha <- pnorm(params$sigma) - (1 - pnorm(params$sigma))

    tmp <- don.qc
    tmp[tmp < 0.0001] <- NA
    tmp <- tmp^(1/3)

    outliers <- lapply(index.mon, function(it){
        x <- tmp[it, , drop = FALSE]
        nl <- colSums(!is.na(x))

        istn <- which(nl >= min.length)
        if(length(istn) == 0) return(NULL)
        x <- x[, istn, drop = FALSE]
        nl <- nl[istn]

        sds <- matrixStats::colSds(x, na.rm = TRUE)
        is <- which(sds > 0)
        if(length(is) == 0) return(NULL)
        istn <- istn[is]
        nl <- nl[is]
        sds <- sds[is]
        x <- x[, is, drop = FALSE]

        moy <- colMeans(x, na.rm = TRUE)
        xs <- sds * sqrt(1 + (1/nl))
        me <- stats::qt(alpha, df = nl - 1) * xs

        xdon <- don.qc[it, istn, drop = FALSE]
        xq <- matrixStats::colQuantiles(xdon, probs = thres.perc, na.rm = TRUE, type = 8)
        xq <- sweep(xdon, 2, xq, FUN = '>')
        xq[is.na(xq)] <- FALSE
        stats <- sweep(xdon, 2, (moy + me)^3, FUN = '/')

        istat <- which(!is.na(stats) & stats > 1 & xq, arr.ind = TRUE)
        if(nrow(istat) == 0) return(NULL)
        list(val = xdon[istat], stat = round(stats[istat], 4),
             istn = istn[istat[, 2]], idate = it[istat[, 1]])
    })

    inull <- sapply(outliers, is.null)
    outliers <- outliers[!inull]
    tmp.date <- NULL
    outqc.outlier <- NULL
    if(length(outliers)){
        outqc.outlier$status.tmp <- "upper.outliers"
        outqc.outlier$status.sp <- NA
        outqc.outlier$stn.id <- don$id[do.call(c, lapply(outliers, '[[', 'istn'))]
        outqc.outlier$dates <- don$dates[do.call(c, lapply(outliers, '[[', 'idate'))]
        outqc.outlier$stn.val <- unname(do.call(c, lapply(outliers, '[[', 'val')))
        outqc.outlier$stats.tmp <- unname(do.call(c, lapply(outliers, '[[', 'stat')))
        outqc.outlier$stats.sp <- NA

        tmp.date <- split(outqc.outlier$dates, outqc.outlier$stn.id)
        tmp.date <- lapply(tmp.date, function(x) which(don$dates %in% x))

        outqc.outlier <- data.frame(do.call(cbind, outqc.outlier), stringsAsFactors = FALSE)
    }

    Insert.Messages.Out(message[['16']], TRUE, "s")

    ###################
    # spatial check

    params$spatial <- list(ispmax = 1, ispobs = 10, isdmin = 3,
                           isdobs = 1, isdq1 = 10, iqrf = 3.0, minDevA = 5)

    outqc.spatial <- NULL
    spatial.vois <- NULL
    if(length(voisin) > 0){
        Insert.Messages.Out(message[['17']], TRUE, "i")

        STNid <- lapply(voisin, "[[", "id")
        STNsp <- lapply(voisin, "[[", "stn")

        ## include temporal outliers
        if(!is.null(tmp.date))
            tmp.date <- tmp.date[match(unlist(STNid), names(tmp.date))]
        else
            tmp.date <- vector(mode = "list", length = length(STNid))

        # parsL <- doparallel.cond(FALSE)
        parsL <- doparallel.cond(length(STNsp) >= 100)
        ret <- cdt.foreach(seq_along(STNsp), parsL, GUI = TRUE,
                           progress = TRUE, FUN = function(j)
        {
            x <- don.qc[, STNsp[[j]], drop = FALSE]

            ## remove missing
            ina <- which(!is.na(x[, 1]))
            if(length(ina) == 0) return(NULL)
            x <- x[ina, , drop = FALSE]
            idaty <- index.date[ina]

            ## exclude variance less than 1
            is <- which(matrixStats::rowVars(x, na.rm = TRUE) > 1)
            if(length(is) == 0) return(NULL)
            x <- x[is, , drop = FALSE]
            idaty <- idaty[is]

            ## get minimum number of neighbors
            nonNA <- rowSums(!is.na(x[, -1, drop = FALSE]))
            idt <- which(nonNA >= params$voisin$min)
            if(length(idt) == 0) return(NULL)
            x <- x[idt, , drop = FALSE]
            idaty <- idaty[idt]
            nonNA <- nonNA[idt]

            ## get maximum number of neighbors to use
            imx <- nonNA > params$voisin$max
            nrw <- params$voisin$max + 1
            im <- 1:nrw
            v0 <- voisin[[j]]$stn
            if(any(imx)){
                xxm <- lapply(seq(nrow(x)), function(i){
                    xm <- x[i, ]
                    ina <- !is.na(xm)
                    xm <- xm[ina]
                    v1 <- v0[ina]
                    if(imx[i]){
                      xm <- xm[im]
                      v1 <- v1[im]
                    }else{
                        xm <- c(xm, rep(NA, params$voisin$max - nonNA[i]))
                    }
                    list(x = xm, v = v1)
                })
                x <- do.call(rbind, lapply(xxm, '[[', 'x'))
                VOIS <- lapply(xxm, '[[', 'v')
                rm(xxm)
            }else{
               VOIS <- lapply(seq(nrow(x)), function(i){
                   ina <- !is.na(x[i, ])
                   v0[ina]
               })
            }

            ## exclude variance less than 1
            is <- which(matrixStats::rowVars(x, na.rm = TRUE) > 1)
            if(length(is) == 0) return(NULL)
            x <- x[is, , drop = FALSE]
            idaty <- idaty[is]
            VOIS <- VOIS[is]

            ## check less than 25% percentile and greater than 75% 
            iqr0 <- matrixStats::rowQuantiles(x, probs = c(0.25, 0.75), na.rm = TRUE, type = 8, drop = FALSE)
            iqr0 <- (iqr0[, 2] - iqr0[, 1]) + 1e-6
            med <- matrixStats::rowMedians(x, na.rm = TRUE)
            stat0 <- (x[, 1] - med) / (params$spatial$iqrf * iqr0)
            # stat0[is.nan(stat0) | is.infinite(stat0)] <- 0

            ## include temporal outliers
            tmp.chk <- rep(FALSE, length(stat0))
            if(!is.null(tmp.date[[j]])) tmp.chk <- idaty %in% tmp.date[[j]]

            ## Filter
            iq <- which(stat0 < -0.9 | stat0 > 0.9 | tmp.chk)

            if(length(iq) == 0){
                spatial.ok <- NULL
                if(!is.null(tmp.date[[j]])){
                    if(any(tmp.chk)){
                        spatial.ok$val <- x[tmp.chk, 1]
                        spatial.ok$it <- idaty[tmp.chk]
                        spatial.ok$stats <- abs(stat0[tmp.chk])
                    }
                }
                sp.chk <- list(isolated.precip = NULL, isolated.dry = NULL,
                               largedev.above = NULL, largedev.below = NULL,
                               spatial.ok = spatial.ok, spatial.vois = NULL)
                return(sp.chk)
            }

            x <- x[iq, , drop = FALSE]
            idaty <- idaty[iq]
            stat0 <- stat0[iq]
            tmp.chk <- tmp.chk[iq]
            VOIS <- VOIS[iq]
            ########

            # ## remove target station
            min.nbrs <- matrixStats::rowMins(x[, -1, drop = FALSE], na.rm = TRUE)
            max.nbrs <- matrixStats::rowMaxs(x[, -1, drop = FALSE], na.rm = TRUE)
            Q <- matrixStats::rowQuantiles(x[, -1, drop = FALSE], probs = c(0.05, 0.25, 0.75, 0.98), na.rm = TRUE, type = 8, drop = FALSE)
            iqr <- (Q[, 3] - Q[, 2]) + 1e-6

            ########
            ## isolated precipitation
            isp <- stat0 >= 1e+6
            isol.pre <- rep(FALSE, length(isp))
            if(any(isp)){
                Qispmax <- rowMeans(x[isp, , drop = FALSE], na.rm = TRUE)
                ispr1 <- Qispmax - max.nbrs[isp]
                ispr2 <- x[isp, 1] - Q[isp, 4]
                isol.pre[isp] <- ispr1 > 0 & ispr2 > 0
            }else{
                ispr1 <- params$spatial$ispmax - max.nbrs[!isp]
                ispr2 <- x[!isp, 1] - params$spatial$ispobs
                isol.pre[!isp] <- ispr1 > 0 & ispr2 > 0 & stat0[!isp] > 1
            }

            isolated.precip <- NULL
            if(any(isol.pre)){
                isolated.precip$val <- x[isol.pre, 1]
                isolated.precip$it <- idaty[isol.pre]
                isolated.precip$stats <- stat0[isol.pre]
            }

            ## isolated dryness
            isdr1 <- min.nbrs - params$spatial$isdmin
            isdr2 <- params$spatial$isdobs - 2 * x[, 1]
            isdr3 <- Q[, 2] - params$spatial$isdq1
            isol.dry <- isdr1 > 0 & isdr2 > 0 & isdr3 > 0 & stat0 < -1

            isolated.dry <- NULL
            if(any(isol.dry)){
                isolated.dry$val <- x[isol.dry, 1]
                isolated.dry$it <- idaty[isol.dry]
                isolated.dry$stats <- abs(stat0[isol.dry])
            }

            ################################################

            # DIST <- lapply(VOIS, function(x){
            #     voisin[[j]]$dist[match(x, voisin[[j]]$stn)]
            # })

            # EST <- sapply(seq_along(VOIS), function(i){
            #     inv.dst <- 1/(DIST[[i]][-1] + 1e-6)^2
            #     v <- x[i, -1]
            #     inv.dst <- inv.dst[!is.na(v)]
            #     v <- v[!is.na(v)]
            #     sum(inv.dst * v) / sum(inv.dst)
            # })
            # MOY <- rowMeans(x[, -1, drop = FALSE], na.rm = TRUE)
            # SDS <- matrixStats::rowSds(x[, -1, drop = FALSE], na.rm = TRUE)

            ########

            ## Too large deviations above
            atldv <- (x[, 1] - Q[, 3]) / (2 * params$spatial$iqrf * iqr)
            extraC1 <- x[, 1] > Q[, 4] & x[, 1] > params$spatial$minDevA
            large.above <- atldv > 1 & !isol.pre & extraC1

            largedev.above <- NULL
            if(any(large.above)){
                largedev.above$val <- x[large.above, 1]
                largedev.above$it <- idaty[large.above]
                largedev.above$stats <- stat0[large.above]
            }

            ## Too large deviations below
            btldv <- (x[, 1] - Q[, 2]) / (params$spatial$iqrf * iqr)
            extraC2 <- x[, 1] < Q[, 1]
            large.below <- btldv < -1 & isdr2 > 0 & !isol.dry & extraC2

            largedev.below <- NULL
            if(any(large.below)){
                largedev.below$val <- x[large.below, 1]
                largedev.below$it <- idaty[large.below]
                largedev.below$stats <- abs(stat0[large.below])
            }

            ## include temporal check
            spatial.ok <- NULL
            if(!is.null(tmp.date[[j]])){
                if(any(tmp.chk)){
                    tmp.chk <- tmp.chk & !isol.pre & !isol.dry & !large.above & !large.below
                    if(any(tmp.chk)){
                        spatial.ok$val <- x[tmp.chk, 1]
                        spatial.ok$it <- idaty[tmp.chk]
                        spatial.ok$stats <- abs(stat0[tmp.chk])
                    }
                }
            }

            spatial.vois <- NULL
            ivois <- tmp.chk | isol.pre | isol.dry | large.above | large.below
            if(any(ivois)){
                spatial.vois$is <- VOIS[ivois]
                spatial.vois$it <- idaty[ivois]
            }
            
            list(isolated.precip = isolated.precip, isolated.dry = isolated.dry,
                 largedev.above = largedev.above, largedev.below = largedev.below,
                 spatial.ok = spatial.ok, spatial.vois = spatial.vois)
        })

        inull <- sapply(ret, is.null)
        ret <- ret[!inull]
        STNid <- STNid[!inull]
        if(length(ret) > 0){
            isolated.precip <- lapply(ret, '[[', 'isolated.precip')
            isolated.precip <- qcRR.format.spatial(isolated.precip, STNid, don$dates, "isolated.precipitation")
            isolated.dry <- lapply(ret, '[[', 'isolated.dry')
            isolated.dry <- qcRR.format.spatial(isolated.dry, STNid, don$dates, "isolated.dryness")
            largedev.above <- lapply(ret, '[[', 'largedev.above')
            largedev.above <- qcRR.format.spatial(largedev.above, STNid, don$dates, "too.large.deviation.above")
            largedev.below <- lapply(ret, '[[', 'largedev.below')
            largedev.below <- qcRR.format.spatial(largedev.below, STNid, don$dates, "too.large.deviation.below")
            spatial.ok <- lapply(ret, '[[', 'spatial.ok')
            spatial.ok <- qcRR.format.spatial(spatial.ok, STNid, don$dates, "spatial.not.outliers")

            outqc.spatial <- rbind(isolated.precip, isolated.dry, largedev.above, largedev.below, spatial.ok)

            spatial.vois <- lapply(ret, '[[', 'spatial.vois')
            names(spatial.vois) <- STNid
            inull <- sapply(spatial.vois, is.null)
            spatial.vois <- spatial.vois[!inull]
            if(length(spatial.vois)){
                spatial.vois <- lapply(seq_along(spatial.vois), function(i){
                    x <- spatial.vois[[i]]
                    x$it <- don$dates[x$it]
                    x$id <- names(spatial.vois[i])
                    x
                })
            }else spatial.vois <- NULL
        }

        Insert.Messages.Out(message[['18']], TRUE, "s")
    }

    ###################

    outqc <- rbind(outqc.neg, outqc.max, outqc.outlier, outqc.spatial)

    if(!is.null(outqc)){
        index.stn <- split(seq(nrow(outqc)), outqc$stn.id)
        nom.stn <- names(index.stn)
        inom <- c("stn.id", "dates", "stn.val", "stats.tmp",
                  "stats.sp", "status.tmp", "status.sp")
        nom <- c("STN.ID", "DATE", "STN.VAL", "STAT.TEMPORAL",
                 "STAT.SPATIAL", "OUT.TEMPORAL", "OUT.SPATIAL",
                 "NOT.REPLACE", "REPLACE.VAL")
        replace0 <- c("negative.values", "greater.than.maximum.values")

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
                    y <- dup[jd, , drop = FALSE]
                    if(nrow(y) == 3){
                        y[1, is.na(y[1, ])] <- y[3, is.na(y[1, ])]
                        y[1, 'stats.tmp'] <- y[2, 'stats.tmp']
                    }else{
                        y[1, is.na(y[1, ])] <- y[2, is.na(y[1, ])]
                    }
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
            x <- cbind.data.frame(x, norep = norep, repval = repval)
            names(x) <- nom
            res <- list(date = x$DATE, outliers = x,
                        stn = voisin.qc[[jj]]$stn[-1],
                        dist = voisin.qc[[jj]]$dist[-1],
                        vois = voisin.qc[[jj]]$ivois)
            return(res)
        })
        names(outqc) <- nom.stn
        outqc <- list(res = outqc, stn = nom.stn, spatial.vois = spatial.vois)
    }else outqc <- NULL

    ###################
    ## check if mixed with temperature data

    ## minimum sequence to check
    min.seq <- 10
    ## minimum temperature value
    min.temp <- 5
    ## maximum threshold variance of diff
    max.var.diff <- 1.5

    tmp <- don.qc
    itmp <- !is.na(tmp) & tmp >= min.temp
    tmp[!itmp] <- NA
    tmp <- cdt.roll.fun(tmp, 5, "mean", na.rm = TRUE, min.data = 3, align = "right")
    tmp <- rbind(diff(tmp), NA)
    itmp <- itmp & !is.na(tmp) & tmp >= -10 & tmp <= 10

    itmp <- lapply(seq(ncol(itmp)), function(j){
        x <- itmp[, j]
        x <- rle(x)
        if(!any(x$lengths[x$values] >= min.seq)) return(NULL)
        ie <- cumsum(x$lengths)
        is <- c(1, ie[-length(ie)] + 1)
        ie <- ie[x$values & x$lengths >= min.seq]
        is <- is[x$values & x$lengths >= min.seq]

        vx <- sapply(seq_along(is), function(i) var(diff(tmp[is[i]:ie[i], j])))
        ix <- vx < max.var.diff
        if(!any(ix)) return(NULL)

        cbind(is[ix] - 2, ie[ix], vx[ix])
    })

    inull <- sapply(itmp, is.null)
    itmp <- itmp[!inull]
    outqc.temp <- NULL
    if(length(itmp) > 0){
        istn <- which(!inull)
        xtmp <- lapply(seq_along(itmp), function(j){
            ix <- itmp[[j]]
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
        outqc.temp <- list(res = xtmp, stn = don$id[istn])
    }

    ###################
    ## Check consecutive values

    min.len <- 5

    tmp <- don.qc
    tmp[is.na(tmp)] <- 0
    tmp <- rbind(diff(tmp), 0)
    tmp <- tmp == 1

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
    outqc.seq <- NULL
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

    ###################

    outqc$mixed <- outqc.temp
    outqc$sequence <- outqc.seq

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

qcRR.format.spatial <- function(sp.out, stn.id, dates, outlier.name)
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
                cbind(id0[[j]], x$val, dates[x$it], x$stats)
            })
        )
        res$stn.id <- tmp[, 1]
        res$dates <- tmp[, 3]
        res$stn.val <- as.numeric(tmp[, 2])
        res$stats.tmp <- NA
        res$stats.sp <- round(as.numeric(tmp[, 4]), 4)
        res <- data.frame(do.call(cbind, res), stringsAsFactors = FALSE)
    }
    return(res)
}

