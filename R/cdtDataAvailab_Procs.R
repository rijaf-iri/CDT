
data_Avail_percentage <- function(cdtstn_dir, don, date.range, tstep, minhour){
    dt_rg <- get.range.date.time(date.range, tstep, minhour)
    dates <- get.datetime.cdtstation(don$dates, tstep)
    it <- dates >= dt_rg$start & dates <= dt_rg$end
    dat <- don$data[it, , drop = FALSE]
    dates <- don$dates[it]
    ndate <- length(dates)
    col_no_na <- colSums(!is.na(dat))
    avai.prec <- 100 * col_no_na / ndate

    out_avail <- list(x = don$lon, y = don$lat, z = avai.prec)

    date_range0 <- dates[c(1, ndate)]
    date_range0 <- paste0(date_range0, collapse = "-")

    dim(avai.prec) <- c(1, length(don$id))
    xdata <- don
    xdata$data <- xdata$data[1, , drop = FALSE]
    xdata$data[1, ] <- avai.prec
    xdata$dates <- date_range0
    xdata_file <- paste0("DataAvailability_percentage_", date_range0, ".csv")
    xdata_file <- file.path(cdtstn_dir, xdata_file)
    writeCDTStationData(xdata, xdata_file)

    return(out_avail)
}

AssessDataAvailProcs <- function(GeneralParameters){
    if(!dir.exists(GeneralParameters$outdir)){
        msg <- paste(GeneralParameters$outdir, .cdtData$EnvData$message[[6]])
        Insert.Messages.Out(msg, TRUE, "e")
        return(NULL)
    }

    don <- getCDTStationData(GeneralParameters$infile, GeneralParameters$intstep)
    if(is.null(don)) return(NULL)

    output <- list(params = GeneralParameters, data = don)
    output$availPeriod <- get.range.datetime.cdtstation(don$dates, GeneralParameters$intstep)

    ###################
    outdir <- file.path(GeneralParameters$outdir, "ASSESS.DATA.AVAILABILITY_data")
    if(dir.exists(outdir)){
        tmp <- list.files(GeneralParameters$outdir, "ASSESS\\.DATA\\.AVAILABILITY\\_data\\_[0-9]+", include.dirs = TRUE)
        if(length(tmp) == 0){
            tmp <- 1
        }else{
            tmp <- gsub("ASSESS.DATA.AVAILABILITY_data_", "", tmp)
            tmp <- max(as.numeric(tmp)) + 1
        }
        outdir <- file.path(GeneralParameters$outdir, paste0("ASSESS.DATA.AVAILABILITY_data_", tmp))
    }
    dataCDTdir <- file.path(outdir, 'CDTDATASET')
    dir.create(dataCDTdir, showWarnings = FALSE, recursive = TRUE)
    dataSTNdir <- file.path(outdir, 'CDTSTATIONS')
    dir.create(dataSTNdir, showWarnings = FALSE, recursive = TRUE)

    ###################
    ndate <- length(don$dates)
    row_no_na <- rowSums(!is.na(don$data))

    avail <- data_Avail_percentage(dataSTNdir, don, output$availPeriod,
                    GeneralParameters$intstep, GeneralParameters$minhour)

    ###################
    if(GeneralParameters$intstep %in% c('minute', 'hourly')){
        sub1 <- c(1, 8)
        sub2 <- c(1, 6)
        sub3 <- c(7, 8)
        ts_file_csv <- "Daily_non-missing.csv"
        grp_file_csv <- "Monthly_non-missing.csv"
        avg_ts <- "Month"
        avg_file <- "Monthly_Average_number-stations.csv"
    }else{
        sub1 <- c(1, 6)
        sub2 <- c(1, 4)
        sub3 <- c(5, 6)
        ts_file_csv <- "Monthly_non-missing.csv"
        grp_file_csv <- "Yearly_non-missing.csv"
        avg_ts <- "Year"
        avg_file <- "Annual_Average_number-stations.csv"
    }

    #######
    index.ts <- split(seq_along(don$dates), substr(don$dates, sub1[1], sub1[2]))
    don.ts <- lapply(index.ts, function(ix) colSums(!is.na(don$data[ix, , drop = FALSE])))
    don.ts <- do.call(rbind, don.ts)
    dates.ts <- names(index.ts)
    dates_ts1 <- substr(dates.ts, sub2[1], sub2[2])
    dates_ts2 <- substr(dates.ts, sub3[1], sub3[2])

    xdata <- don
    xdata$dates <- dates.ts
    xdata$data <- don.ts
    writeCDTStationData(xdata, file.path(dataSTNdir, ts_file_csv))

    xdata <- list(ts1 = dates_ts1, ts2 = dates_ts2, data = don.ts)
    output$data_ts <- xdata

    #######
    index.grp <- split(seq_along(don$dates), substr(don$dates, sub2[1], sub2[2]))
    dates.grp <- as.numeric(names(index.grp))
    don.grp <- lapply(index.grp, function(ix) colSums(!is.na(don$data[ix, , drop = FALSE])))
    don.grp <- do.call(rbind, don.grp)

    xdata <- don
    xdata$dates <- dates.grp
    xdata$data <- don.grp
    writeCDTStationData(xdata, file.path(dataSTNdir, grp_file_csv))

    xdata <- list(grp = dates.grp, data = don.grp)
    output$data_grp <- xdata

    #######
    nb.stn.grp <- lapply(index.grp, function(ix)
                    do.call(function(x) c(mean(x), min(x), max(x)), list(row_no_na[ix])))
    nb.stn.grp <- do.call(rbind, nb.stn.grp)

    xdata <- data.frame(cbind(dates.grp, round(nb.stn.grp, 1)))
    names(xdata) <- c(avg_ts, "Average", "Minimum", "Maximum")
    writeFiles(xdata, file.path(dataSTNdir, avg_file), col.names = TRUE)

    output$dates.grp <- dates.grp
    output$nb.stn.grp <- nb.stn.grp

    ###################
    jstn <- lapply(seq_along(don$id), function(j){
        x <- which(stats::complete.cases(don$data[, j]))
        if(length(x)) c(min(x), max(x)) else integer(0)
    })

    sworking <- lapply(jstn, function(ix){
        ret <- rep(0, ndate)
        if(length(ix)) ret[ix[1]:ix[2]] <- 1
        ret
    })
    sworking <- rowSums(do.call(cbind, sworking))

    daty <- format.datetime.cdtstation(don$dates, GeneralParameters$intstep)
    stn.working <- list(date = daty, avai = row_no_na, work = sworking)

    saveRDS(stn.working, file.path(dataCDTdir, "Station_Activities.rds"))

    ###################
    id0 <- apply(don$data, 2, elEqual)
    if(any(id0)){
        val <- apply(don$data[, id0, drop = FALSE], 2,
                    function(x){
                        x <- x[!is.na(x)]
                        if(length(x)) x[1] else NA
                    })
        infoIDen <- cbind(don$lon[id0], don$lat[id0], val)
        lon <- don$lon[!id0]
        lat <- don$lat[!id0]
        donne <- don$data[, !id0, drop = FALSE]
    }else{
        lon <- don$lon
        lat <- don$lat
        donne <- don$data
        infoIDen <- NULL
    }

    xdist <- Distance.Correlation(donne, cbind(lon, lat))
    xsummary <- summary.Distance.Correlation(xdist[, 1], xdist[, 2])
    loess <- stats::loess.smooth(xdist[, 1], xdist[, 2])

    dist.cor <- list(dst.cor = xdist, summary = xsummary, loess = loess)
    saveRDS(dist.cor, file.path(dataCDTdir, "Distance_Correlation.rds"))

    ###################
    file.index <- file.path(outdir, 'AssessDataAvailability.rds')
    saveRDS(output, file.index)

    output$PathData <- outdir
    output$status <- "ok"

    return(output)
}

Distance.Correlation <- function(data.mat, lonlat){
    corm <- suppressWarnings(stats::cor(data.mat, use = 'pairwise.complete.obs'))
    corm[lower.tri(corm, diag = TRUE)] <- NA

    dist <- fields::rdist.earth(lonlat, miles = FALSE)
    dist[lower.tri(dist, diag = TRUE)] <- NA

    idn <- which(!is.na(corm))
    xcor <- corm[idn]
    xdst <- dist[idn]
    return(cbind(xdst, xcor))
}

#' @exportS3Method NULL
summary.Distance.Correlation <- function(x, y){
    o <- order(x)
    x <- x[o]
    y <- y[o]

    win <- 100
    min.data <- 10
    probs <- c(0.05, 0.5, 0.95)

    nl <- length(x)
    nseq <- nl - min.data + 1
    xquant <- rep(list(rep(NA, 3)), nseq)
    for(k in seq(nseq)){
        ys <- y[seq(k, k + win - 1, 1)]
        ys <- ys[!is.na(ys)]
        if(length(ys) < min.data) next
        xquant[[k]] <- stats::quantile(ys,  probs = probs)
    }
    xquant <- do.call(rbind, xquant)
    return(unname(cbind(x[seq(nseq)], xquant)))
}

