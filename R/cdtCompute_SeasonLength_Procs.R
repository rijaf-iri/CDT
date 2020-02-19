
compute_SeasonLength_Procs <- function(GeneralParameters){
    if(!dir.exists(GeneralParameters$output)){
        Insert.Messages.Out(paste(GeneralParameters$output, "did not find"), format = TRUE)
        return(NULL)
    }

    onset <- try(readRDS(GeneralParameters$onset), silent = TRUE)
    if(inherits(onset, "try-error")){
        Insert.Messages.Out(paste("Unable to read", GeneralParameters$onset), format = TRUE)
        return(NULL)
    }

    cessation <- try(readRDS(GeneralParameters$cessation), silent = TRUE)
    if(inherits(cessation, "try-error")){
        Insert.Messages.Out(paste("Unable to read", GeneralParameters$cessation), format = TRUE)
        return(NULL)
    }

    if(onset$params$data.type != cessation$params$data.type){
        Insert.Messages.Out("Onset and Cessation data are of different types", format = TRUE)
        return(NULL)
    }

    ##########################################

    start.mon <- as.numeric(format(onset$start.date[1], "%m"))
    start.day <- as.numeric(format(onset$start.date[1], "%d"))
    index <- cdt.index.DailyYears(onset$ts.date, start.mon, start.day)

    idx.ons <- seq(nrow(index$range.date))
    idx.cess <- sapply(idx.ons, function(j){
        pos <- which(as.Date(index$range.date[j, 1], "%Y%m%d") <= cessation$start.date &
                    as.Date(index$range.date[j, 2], "%Y%m%d") >= cessation$start.date)
        if(length(pos) == 0) pos <- NA
        pos
    })

    if(all(is.na(idx.cess))){
        Insert.Messages.Out("Onset and Cessation do not overlap", format = TRUE)
        return(NULL)
    }

    idx.ons <- idx.ons[!is.na(idx.cess)]
    idx.cess <- idx.cess[!is.na(idx.cess)]
    range.date <- index$range.date[idx.ons, , drop = FALSE]

    ##########################################

    if(onset$params$data.type == "cdtstation"){
        if(!any(onset$data$id %in% cessation$data$id)){
            Insert.Messages.Out("Onset & Cessation stations do not match", format = TRUE)
            return(NULL)
        }

        onset.file <- file.path(dirname(GeneralParameters$onset), 'CDTDATASET', "ONSET.rds")
        cessa.file <- file.path(dirname(GeneralParameters$cessation), 'CDTDATASET', "CESSATION.rds")

        onset$onset <- readRDS(onset.file)
        cessation$cessation <- readRDS(cessa.file)

        ##################
        jnx <- match(onset$data$id, cessation$data$id)
        jnx <- jnx[!is.na(jnx)]
        cessation$data$id <- cessation$data$id[jnx]

        stn.id <- cessation$data$id
        stn.lon <- cessation$data$lon[jnx]
        stn.lat <- cessation$data$lat[jnx]
        cessation$cessation <- cessation$cessation[, jnx, drop = FALSE]

        inx <- onset$data$id %in% cessation$data$id
        onset$onset <- onset$onset[, inx, drop = FALSE]

        ##################

        onset$start.date <- onset$start.date[idx.ons]
        onset$onset <- onset$onset[idx.ons, , drop = FALSE]
        cessation$cessation <- cessation$cessation[idx.cess, , drop = FALSE]

        seasonL <- cessation$cessation - onset$onset
        seasonL[seasonL < 0] <- NA

        ##################

        outDIR <- file.path(GeneralParameters$output, "SEASON.LENGTH_data")
        dir.create(outDIR, showWarnings = FALSE, recursive = TRUE)

        datadir <- file.path(outDIR, 'CDTSTATIONS')
        dir.create(datadir, showWarnings = FALSE, recursive = TRUE)

        dataOUT <- file.path(outDIR, 'CDTDATASET')
        dir.create(dataOUT, showWarnings = FALSE, recursive = TRUE)

        file.seasonL <- file.path(datadir, "SeasonLength.txt")
        file.index <- file.path(outDIR, "SeasonLength.rds")
        file.cdt.length <- file.path(dataOUT, "SEASONLENGTH.rds")

        ##################

        stn.data <- list(id = stn.id, lon = stn.lon, lat = stn.lat, date = onset$data$date)
        output <- list(params = c(onset$params[!names(onset$params) %in% 'output'], GeneralParameters),
                        data = stn.data, start.date = onset$start.date, range.date = range.date)

        .cdtData$EnvData$output <- output
        .cdtData$EnvData$PathData <- outDIR

        ##################
        con <- gzfile(file.index, compression = 7)
        open(con, "wb")
        saveRDS(output, con)
        close(con)

        ##################
        con <- gzfile(file.cdt.length, compression = 7)
        open(con, "wb")
        saveRDS(seasonL, con)
        close(con)

        ##################

        seasonL[is.na(seasonL)] <- .cdtData$Config$missval
        daty <- format(onset$start.date, "%Y%m%d")
        xhead <- rbind(stn.id, stn.lon, stn.lat)
        chead <- c('ID.STN', 'LON', 'START.DATE/LAT')
        infohead <- cbind(chead, xhead)

        seasonL <- rbind(infohead, cbind(daty, seasonL))

        writeFiles(seasonL, file.seasonL)

        rm(stn.data, output, seasonL)
    }

    ##########################################

    if(onset$params$data.type == "cdtdataset"){
        onset.file <- file.path(dirname(GeneralParameters$onset), "CDTDATASET", "CDTDATASET.rds")
        cessa.file <- file.path(dirname(GeneralParameters$cessation), "CDTDATASET", "CDTDATASET.rds")

        onset.index <- try(readRDS(onset.file), silent = TRUE)
        if(inherits(onset.index, "try-error")){
            Insert.Messages.Out(paste("Unable to read", onset.file), format = TRUE)
            return(NULL)
        }

        cessa.index <- try(readRDS(cessa.file), silent = TRUE)
        if(inherits(cessa.index, "try-error")){
            Insert.Messages.Out(paste("Unable to read", cessa.file), format = TRUE)
            return(NULL)
        }

        ##################
        SP1 <- defSpatialPixels(list(lon = onset.index$coords$mat$x, lat = onset.index$coords$mat$y))
        SP2 <- defSpatialPixels(list(lon = cessa.index$coords$mat$x, lat = cessa.index$coords$mat$y))
        if(is.diffSpatialPixelsObj(SP1, SP2, tol = 1e-04)){
            Insert.Messages.Out("Onset & Cessation have different resolution or bbox", format = TRUE)
            return(NULL)
        }
        rm(SP1, SP2)

        if(onset.index$chunksize != cessa.index$chunksize){
            Insert.Messages.Out("Onset & Cessation have different chunk size", format = TRUE)
            return(NULL)
        }

        ##################

        outDIR <- file.path(GeneralParameters$output, "SEASON.LENGTH_data")
        dir.create(outDIR, showWarnings = FALSE, recursive = TRUE)

        ncdfOUT <- file.path(outDIR, 'DATA_NetCDF')
        dir.create(ncdfOUT, showWarnings = FALSE, recursive = TRUE)

        dataOUT <- file.path(outDIR, 'CDTDATASET')
        dir.create(dataOUT, showWarnings = FALSE, recursive = TRUE)

        datadir <- file.path(dataOUT, 'DATA')
        dir.create(datadir, showWarnings = FALSE, recursive = TRUE)
        datafileIdx <- file.path(dataOUT, 'CDTDATASET.rds')
        file.index <- file.path(outDIR, "SeasonLength.rds")

        index.out <- onset.index
        index.out$varInfo$name <- "seasLen"
        index.out$varInfo$units <- "days"
        index.out$varInfo$longname <- "Length of rainy season"

        ##################

        chunkfile <- sort(unique(onset.index$colInfo$index))
        chunkcalc <- split(chunkfile, ceiling(chunkfile / onset.index$chunkfac))

        do.parChunk <- if(onset.index$chunkfac > length(chunkcalc)) TRUE else FALSE
        do.parCALC <- if(do.parChunk) FALSE else TRUE

        cdtParallelCond <- .cdtData$Config[c('dopar', 'detect.cores', 'nb.cores')]
        parsL <- doparallel.cond(do.parCALC & (length(chunkcalc) > 10))
        ret <- cdt.foreach(seq_along(chunkcalc), parsL, GUI = TRUE,
                           progress = TRUE, FUN = function(jj)
        {
            ons.data <- readCdtDatasetChunk.sequence(chunkcalc[[jj]], onset.file, cdtParallelCond, do.par = do.parChunk)
            ons.data <- ons.data[idx.ons, , drop = FALSE]

            cess.data <- readCdtDatasetChunk.sequence(chunkcalc[[jj]], cessa.file, cdtParallelCond, do.par = do.parChunk)
            cess.data <- cess.data[idx.cess, , drop = FALSE]

            seasonL <- cess.data - ons.data
            seasonL[seasonL < 0] <- NA

            writeCdtDatasetChunk.sequence(seasonL, chunkcalc[[jj]], index.out, datadir, cdtParallelCond, do.par = do.parChunk)

            rm(ons.data, cess.data, seasonL); gc()
            return(0)
        })

        ####################################

        start.date <- onset$start.date[idx.ons]
        output <- list(params = c(onset$params[!names(onset$params) %in% 'output'], GeneralParameters),
                        start.date = start.date, range.date = range.date)
        .cdtData$EnvData$output <- output
        .cdtData$EnvData$PathData <- outDIR

        ##################
        con <- gzfile(file.index, compression = 6)
        open(con, "wb")
        saveRDS(output, con)
        close(con)

        ##################

        start.date <- format(start.date, "%Y%m%d")
        index.out$dateInfo$date <- start.date
        index.out$dateInfo$index <- seq_along(start.date)

        con <- gzfile(datafileIdx, compression = 6)
        open(con, "wb")
        saveRDS(index.out, con)
        close(con)

        ####################################

        onset.ncdir <- file.path(dirname(GeneralParameters$onset), 'DATA_NetCDF')
        cessa.ncdir <- file.path(dirname(GeneralParameters$cessation), 'DATA_NetCDF')
        cessa.date <- format(cessation$start.date[idx.cess], "%Y%m%d")

        x <- index.out$coords$mat$x
        y <- index.out$coords$mat$y
        dx <- ncdim_def("Lon", "degreeE", x)
        dy <- ncdim_def("Lat", "degreeN", y)

        ret <- lapply(seq_along(start.date), function(j){
            nc <- nc_open(file.path(onset.ncdir, paste0("onset_", start.date[j], ".nc")))
            ons.data <- ncvar_get(nc, varid = "onset")
            nc_close(nc)

            nc <- nc_open(file.path(cessa.ncdir, paste0("cessation_", cessa.date[j], ".nc")))
            cess.data <- ncvar_get(nc, varid = "cessation")
            nc_close(nc)

            ons.data <- ons.data + as.Date(start.date[j], "%Y%m%d")
            cess.data <- cess.data + as.Date(cessa.date[j], "%Y%m%d")

            seasonL <- cess.data - ons.data
            seasonL[is.na(seasonL)] <- -99

            time0 <- as.integer(as.Date(start.date[j], "%Y%m%d"))
            time <- ncdim_def("start", "days since 1970-1-1", time0)
            xyt.dim <- list(dx, dy, time)
            grdNC <- ncvar_def("seasLen", "days", xyt.dim, -99,
                                longname = "Length of rainy season", prec = "short",
                                shuffle = TRUE, compression = 9)

            filenc <- file.path(ncdfOUT, paste0("seasLen_", start.date[j], ".nc"))
            nc <- nc_create(filenc, grdNC)
            ncvar_put(nc, grdNC, seasonL)
            nc_close(nc)

            return(0)
        })
    }

    rm(onset, cessation)
    return(0)
}
