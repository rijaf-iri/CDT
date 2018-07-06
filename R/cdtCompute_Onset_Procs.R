
compute_SeasonOnset_Procs <- function(GeneralParameters){
	if(!dir.exists(GeneralParameters$output)){
		Insert.Messages.Out(paste(GeneralParameters$output, .cdtData$EnvData[['message']][['16']]), format = TRUE)
		return(NULL)
	}

	shpf <- NULL
	shpdiv <- NULL
	if(GeneralParameters$onset.reg$region == "Multiple"){
		if(GeneralParameters$onset.reg$subdiv == "Shapefile"){
			shpf <- getShpOpenData(GeneralParameters$onset.reg$shp$file)[[2]]
			if(is.null(shpf)){
				Insert.Messages.Out(.cdtData$EnvData[['message']][['19']], format = TRUE)
				return(NULL)
			}
			shpattr <- GeneralParameters$onset.reg$shp$attr
			shpf <- shpf[, shpattr]
			shpdiv <- as.character(shpf@data[, shpattr])
		}
	}

	if(GeneralParameters$data.type == "cdtstation"){
		prec <- getStnOpenData(GeneralParameters$cdtstation$prec)
		if(is.null(prec)) return(NULL)
		prec <- getCDTdataAndDisplayMsg(prec, "daily", GeneralParameters$cdtstation$prec)
		if(is.null(prec)) return(NULL)

		omethods <- sapply(GeneralParameters$onset.criteria, "[[", "method")
		if(any(omethods == 2)){
			etp <- getStnOpenData(GeneralParameters$cdtstation$etp)
			if(is.null(etp)) return(NULL)
			etp <- getCDTdataAndDisplayMsg(etp, "daily", GeneralParameters$cdtstation$etp)
			if(is.null(etp)) return(NULL)

			if(!any(prec$id %in% etp$id)){
				Insert.Messages.Out(.cdtData$EnvData[['message']][['17']], format = TRUE)
				return(NULL)
			}
			if(!any(prec$dates %in% etp$dates)){
				Insert.Messages.Out(.cdtData$EnvData[['message']][['18']], format = TRUE)
				return(NULL)
			}

			##################
			inx <- match(prec$dates, etp$dates)
			inx <- inx[!is.na(inx)]
			etp$dates <- etp$dates[inx]
			etp$data <- etp$data[inx, , drop = FALSE]
			prec$data <- prec$data[prec$dates %in% etp$dates, , drop = FALSE]
			daty <- etp$dates

			##################
			jnx <- match(prec$id, etp$id)
			jnx <- jnx[!is.na(jnx)]
			etp$id <- etp$id[jnx]

			stn.id <- etp$id
			stn.lon <- etp$lon[jnx]
			stn.lat <- etp$lat[jnx]
			etp$data <- etp$data[, jnx, drop = FALSE]
			prec$data <- prec$data[, prec$id %in% etp$id, drop = FALSE]
		}else{
			daty <- prec$dates
			stn.id <- prec$id
			stn.lon <- prec$lon
			stn.lat <- prec$lat
			etp <- NULL
		}

		##################

		criteria <- GeneralParameters$onset.criteria

		subdiv <- cdt.index.Region.Subdiv(stn.lat, stn.lon, shpf, shpdiv, GeneralParameters$onset.reg)
		divexist <- sapply(subdiv, length) > 0
		subdiv <- subdiv[divexist]
		criteria <- criteria[divexist]

		##################

		onset <- cdt.Season.Onset(daty, subdiv, criteria, prec$data, etp$data, GeneralParameters$min.frac)
		onset.num <- onset$onset.num
		start.date <- onset$start.date
		onset <- onset$onset.date

		##################

		outDIR <- file.path(GeneralParameters$output, "ONSET_data")
		dir.create(outDIR, showWarnings = FALSE, recursive = TRUE)

		datadir <- file.path(outDIR, 'CDTSTATIONS')
		dir.create(datadir, showWarnings = FALSE, recursive = TRUE)

		dataOUT <- file.path(outDIR, 'CDTDATASET')
		dir.create(dataOUT, showWarnings = FALSE, recursive = TRUE)

		file.onset <- file.path(datadir, "Onset_dates.txt")
		file.onset1 <- file.path(datadir, "Onset_days.txt")
		file.index <- file.path(outDIR, "Onset.rds")
		file.cdt.prec <- file.path(dataOUT, "PRECIP.rds")
		file.cdt.etp <- file.path(dataOUT, "PET.rds")
		file.cdt.onset <- file.path(dataOUT, "ONSET.rds")

		##################

		stn.data <- list(id = stn.id, lon = stn.lon, lat = stn.lat, date = daty)
		output <- list(params = GeneralParameters, data = stn.data, start.date = start.date, ts.date = daty)

		.cdtData$EnvData$output <- output
		.cdtData$EnvData$PathData <- outDIR

		##################
		con <- gzfile(file.index, compression = 7)
		open(con, "wb")
		saveRDS(output, con)
		close(con)

		##################
		con <- gzfile(file.cdt.prec, compression = 9)
		open(con, "wb")
		saveRDS(prec$data, con)
		close(con)

		if(!is.null(etp)){
			con <- gzfile(file.cdt.etp, compression = 9)
			open(con, "wb")
			saveRDS(etp$data, con)
			close(con)
		}

		##################
		con <- gzfile(file.cdt.onset, compression = 7)
		open(con, "wb")
		saveRDS(onset.num, con)
		close(con)

		##################

		onset.num <- onset.num - start.date
		onset[is.na(onset)] <- .cdtData$Config$missval
		onset.num[is.na(onset.num)] <- .cdtData$Config$missval
		start.date <- format(start.date, "%Y%m%d")

		xhead <- rbind(stn.id, stn.lon, stn.lat)
		chead <- c('ID.STN', 'LON', 'START.DATE/LAT')
		infohead <- cbind(chead, xhead)

		onset <- rbind(infohead, cbind(start.date, onset))
		onset.num <- rbind(infohead, cbind(start.date, onset.num))

		writeFiles(onset, file.onset)
		writeFiles(onset.num, file.onset1)

		rm(prec, etp, onset, onset.num, stn.data, output)
	}

	#######################################################

	if(GeneralParameters$data.type == "cdtdataset"){
		prec <- try(readRDS(GeneralParameters$cdtdataset$prec), silent = TRUE)
		if(inherits(prec, "try-error")){
			Insert.Messages.Out(paste(.cdtData$EnvData[['message']][['20']], GeneralParameters$cdtdataset$prec), format = TRUE)
			return(NULL)
		}
		if(prec$TimeStep != "daily"){
			Insert.Messages.Out(.cdtData$EnvData[['message']][['21']], format = TRUE)
			return(NULL)
		}

		omethods <- sapply(GeneralParameters$onset.criteria, "[[", "method")
		if(any(omethods == 2)){
			etp <- try(readRDS(GeneralParameters$cdtdataset$etp), silent = TRUE)
			if(inherits(etp, "try-error")){
				Insert.Messages.Out(paste(.cdtData$EnvData[['message']][['20']], GeneralParameters$cdtdataset$etp), format = TRUE)
				return(NULL)
			}
			if(etp$TimeStep != "daily"){
				Insert.Messages.Out(.cdtData$EnvData[['message']][['22']], format = TRUE)
				return(NULL)
			}

			##################
			SP1 <- defSpatialPixels(list(lon = prec$coords$mat$x, lat = prec$coords$mat$y))
			SP2 <- defSpatialPixels(list(lon = etp$coords$mat$x, lat = etp$coords$mat$y))
			if(is.diffSpatialPixelsObj(SP1, SP2, tol = 1e-04)){
				Insert.Messages.Out(.cdtData$EnvData[['message']][['23']], format = TRUE)
				return(NULL)
			}
			rm(SP1, SP2)

			##################
			if(prec$chunksize != etp$chunksize){
				Insert.Messages.Out(.cdtData$EnvData[['message']][['24']], format = TRUE)
				return(NULL)
			}

			##################
			if(!any(prec$dateInfo$date %in% etp$dateInfo$date)){
				Insert.Messages.Out(.cdtData$EnvData[['message']][['25']], format = TRUE)
				return(NULL)
			}

			##################
			inx <- match(prec$dateInfo$date, etp$dateInfo$date)
			inx <- inx[!is.na(inx)]
			etp$dateInfo$date <- etp$dateInfo$date[inx]
			etp$dateInfo$index <- etp$dateInfo$index[inx]

			pdaty <- prec$dateInfo$date %in% etp$dateInfo$date
			prec$dateInfo$date <- prec$dateInfo$date[pdaty]
			prec$dateInfo$index <- prec$dateInfo$index[pdaty]
		}

		##################

		outDIR <- file.path(GeneralParameters$output, "ONSET_data")
		dir.create(outDIR, showWarnings = FALSE, recursive = TRUE)

		ncdfOUT <- file.path(outDIR, 'DATA_NetCDF')
		dir.create(ncdfOUT, showWarnings = FALSE, recursive = TRUE)

		dataOUT <- file.path(outDIR, 'CDTDATASET')
		dir.create(dataOUT, showWarnings = FALSE, recursive = TRUE)

		datadir <- file.path(dataOUT, 'DATA')
		dir.create(datadir, showWarnings = FALSE, recursive = TRUE)
		datafileIdx <- file.path(dataOUT, 'CDTDATASET.rds')
		file.index <- file.path(outDIR, "Onset.rds")

		index.out <- prec
		index.out$varInfo$name <- "onset"
		index.out$varInfo$units <- "days since 1970-01-01"
		index.out$varInfo$longname <- "Onset date of rainy season"

		##################

		chunkfile <- sort(unique(prec$colInfo$index))
		chunkcalc <- split(chunkfile, ceiling(chunkfile / prec$chunkfac))

		do.parChunk <- if(prec$chunkfac > length(chunkcalc)) TRUE else FALSE
		do.parCALC <- if(do.parChunk) FALSE else TRUE

		is.parallel <- doparallel(do.parCALC & (length(chunkcalc) > 10))
		`%parLoop%` <- is.parallel$dofun
		ret <- foreach(jj = seq_along(chunkcalc), .packages = "doParallel") %parLoop% {
			rr.data <- readCdtDatasetChunk.sequence(chunkcalc[[jj]], GeneralParameters$cdtdataset$prec, do.par = do.parChunk)
			rr.data <- rr.data[prec$dateInfo$index, , drop = FALSE]

			if(any(omethods == 2)){
				et.data <- readCdtDatasetChunk.sequence(chunkcalc[[jj]], GeneralParameters$cdtdataset$etp, do.par = do.parChunk)
				et.data <- et.data[etp$dateInfo$index, , drop = FALSE]
			}else et.data <- NULL

			####################################

			lat <- prec$coords$df$y[prec$colInfo$index %in% chunkcalc[[jj]]]
			lon <- prec$coords$df$x[prec$colInfo$index %in% chunkcalc[[jj]]]
			criteria <- GeneralParameters$onset.criteria

			subdiv <- cdt.index.Region.Subdiv(lat, lon, shpf, shpdiv, GeneralParameters$onset.reg)
			divexist <- sapply(subdiv, length) > 0
			subdiv <- subdiv[divexist]
			criteria <- criteria[divexist]

			onset <- cdt.Season.Onset(prec$dateInfo$date, subdiv, criteria,
									rr.data, et.data, GeneralParameters$min.frac)
			start.date <- onset$start.date
			onset <- onset$onset.num

			####################################

			writeCdtDatasetChunk.sequence(onset, chunkcalc[[jj]], index.out, datadir, do.par = do.parChunk)

			rm(rr.data, et.data, onset); gc()
			return(start.date)
		}
		if(is.parallel$stop) stopCluster(is.parallel$cluster)

		####################################

		start.date <- as.Date(rowMins(do.call(cbind, ret)), origin = "1970-1-1")
		output <- list(params = GeneralParameters, start.date = start.date, ts.date = prec$dateInfo$date)
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
		chunkdate <- split(start.date, ceiling(seq_along(start.date) / 10))

		x <- index.out$coords$mat$x
		y <- index.out$coords$mat$y
		dx <- ncdim_def("Lon", "degreeE", x)
		dy <- ncdim_def("Lat", "degreeN", y)

		ret <- lapply(chunkdate, function(dates){
			dat <- readCdtDatasetChunk.multi.dates.order(datafileIdx, dates)
			for(j in seq_along(dates)){
				time0 <- as.integer(as.Date(dates[j], "%Y%m%d"))
				don <- dat[j, ] - time0
				don <- matrix(don, length(x), length(y))
				don[is.na(don)] <- -99
				time <- ncdim_def("start", "days since 1970-1-1", time0)
				xyt.dim <- list(dx, dy, time)
				grdNC <- ncvar_def("onset", paste("days since", dates[j]), xyt.dim, -99,
									longname = "Onset date of rainy season", prec = "short",
									shuffle = TRUE, compression = 9)

				filenc <- file.path(ncdfOUT, paste0("onset_", dates[j], ".nc"))
				nc <- nc_create(filenc, grdNC)
				ncvar_put(nc, grdNC, don)
				nc_close(nc)
			}
			return(0)
		})
	}

	return(0)
}
