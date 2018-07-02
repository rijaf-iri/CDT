
computeWBProcs <- function(){
	Insert.Messages.Out("Compute daily water balance ......")

	if(.cdtData$GalParams$data.type == "cdtstation"){
		prec <- getStnOpenData(.cdtData$GalParams$cdtstation$prec)
		if(is.null(prec)) return(NULL)
		prec <- getCDTdataAndDisplayMsg(prec, .cdtData$GalParams$Tstep, .cdtData$GalParams$cdtstation$prec)
		if(is.null(prec)) return(NULL)

		etp <- getStnOpenData(.cdtData$GalParams$cdtstation$etp)
		if(is.null(etp)) return(NULL)
		etp <- getCDTdataAndDisplayMsg(etp, .cdtData$GalParams$Tstep, .cdtData$GalParams$cdtstation$etp)
		if(is.null(etp)) return(NULL)

		if(!any(prec$id %in% etp$id)){
			Insert.Messages.Out("Precip & PET stations do not match", format = TRUE)
			return(NULL)
		}
		if(!any(prec$dates %in% etp$dates)){
			Insert.Messages.Out("Precip & PET dates do not match", format = TRUE)
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

		##################

		if(.cdtData$GalParams$swhc$multi){
			swhc <- getStnOpenData(.cdtData$GalParams$swhc$file)
			if(is.null(swhc)) return(NULL)

			swhc.id <- as.character(swhc[1, -1])
			# swhc.lon <- as.numeric(swhc[2, -1])
			# swhc.lat <- as.numeric(swhc[3, -1])
			swhc <- as.numeric(swhc[nrow(swhc), -1])

			if(!isTRUE(all.equal(stn.id, swhc.id))){
				Insert.Messages.Out("SWHC, Precip & PET stations do not match", format = TRUE)
				return(NULL)
			}
		}else swhc <- .cdtData$GalParams$swhc$cap.max

		if(.cdtData$GalParams$wb$multi){
			wb1 <- getStnOpenData(.cdtData$GalParams$wb$file)
			if(is.null(wb1)) return(NULL)

			wb1.id <- as.character(wb1[1, -1])
			# wb1.lon <- as.numeric(wb1[2, -1])
			# wb1.lat <- as.numeric(wb1[3, -1])
			wb1 <- as.numeric(wb1[nrow(wb1), -1])

			if(!isTRUE(all.equal(stn.id, wb1.id))){
				Insert.Messages.Out("Initial water balance, Precip & PET stations do not match", format = TRUE)
				return(NULL)
			}
		}else wb1 <- .cdtData$GalParams$wb$wb1

		##################

		index <- cdt.index.DailyYears(daty, .cdtData$GalParams$hdate$start.month, .cdtData$GalParams$hdate$start.day)
		index <- index$index

		##################
		miss1 <- is.na(index[[1]][1])
		ix <- do.call(c, index)
		startDaty <- min(which(!is.na(ix)))
		endDaty <- max(which(!is.na(ix)))

		if(!.cdtData$GalParams$hdate$separate.year){
			index <- if(miss1) list(index[[1]], unlist(index[-1])) else list(ix)
		}
		rg.date <- range(as.Date(daty, '%Y%m%d'), na.rm = TRUE)
		daty <- format(seq(rg.date[1], rg.date[2], 'day'), '%Y%m%d')

		##################

		WB <- lapply(index, function(idx){
			Water.Balance(prec$data[idx, , drop = FALSE], etp$data[idx, , drop = FALSE], swhc, wb1)
		})
		if(miss1) WB[[1]][] <- NA
		WB <- do.call(rbind, WB)
		WB <- WB[startDaty:endDaty, , drop = FALSE]
		WB <- round(WB, 1)
		WB[is.na(WB)] <- .cdtData$Config$missval

		##################

		xhead <- rbind(stn.id, stn.lon, stn.lat)
		chead <- c('ID.STN', 'LON', paste0(toupper(.cdtData$GalParams$Tstep), '/LAT'))
		infohead <- cbind(chead, xhead)

		WB <- rbind(infohead, cbind(daty, WB))
		writeFiles(WB, .cdtData$GalParams$output)
		rm(prec, etp, WB)
	}

	#######################################################

	if(.cdtData$GalParams$data.type == "cdtdataset"){
		prec <- try(readRDS(.cdtData$GalParams$cdtdataset$prec), silent = TRUE)
		if(inherits(prec, "try-error")){
			Insert.Messages.Out(paste("Unable to read", .cdtData$GalParams$cdtdataset$prec), format = TRUE)
			return(NULL)
		}
		if(.cdtData$GalParams$Tstep != prec$TimeStep){
			Insert.Messages.Out(paste("Precip dataset is not a", .cdtData$GalParams$Tstep, "data"), format = TRUE)
			return(NULL)
		}

		etp <- try(readRDS(.cdtData$GalParams$cdtdataset$etp), silent = TRUE)
		if(inherits(etp, "try-error")){
			Insert.Messages.Out(paste("Unable to read", .cdtData$GalParams$cdtdataset$etp), format = TRUE)
			return(NULL)
		}
		if(.cdtData$GalParams$Tstep != etp$TimeStep){
			Insert.Messages.Out(paste("PET dataset is not a", .cdtData$GalParams$Tstep, "data"), format = TRUE)
			return(NULL)
		}

		##################
		SP1 <- defSpatialPixels(list(lon = prec$coords$mat$x, lat = prec$coords$mat$y))
		SP2 <- defSpatialPixels(list(lon = etp$coords$mat$x, lat = etp$coords$mat$y))
		if(is.diffSpatialPixelsObj(SP1, SP2, tol = 1e-04)){
			Insert.Messages.Out("Precip & PET have different resolution or bbox", format = TRUE)
			return(NULL)
		}
		rm(SP1, SP2)

		##################
		if(prec$chunksize != etp$chunksize){
			Insert.Messages.Out("Precip & PET have different chunk size", format = TRUE)
			return(NULL)
		}

		##################
		if(!any(prec$dateInfo$date %in% etp$dateInfo$date)){
			Insert.Messages.Out("Precip & PET dates do not match", format = TRUE)
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

		daty <- prec$dateInfo$date

		##################

		if(.cdtData$GalParams$swhc$multi){
			swhc <- getNCDFSampleData(.cdtData$GalParams$swhc$file)
			if(is.null(swhc)){
				Insert.Messages.Out("No SWHC data found", format = TRUE)
				return(NULL)
			}

			SP1 <- defSpatialPixels(swhc[c('lon', 'lat')])
			SP2 <- defSpatialPixels(list(lon = etp$coords$mat$x, lat = etp$coords$mat$y))
			if(is.diffSpatialPixelsObj(SP1, SP2, tol = 1e-04)){
				Insert.Messages.Out("SWHC, Precip & PET have different resolution or bbox", format = TRUE)
				return(NULL)
			}
			rm(SP1, SP2)

			jfile <- getIndex.AllOpenFiles(.cdtData$GalParams$swhc$file)
			swhc <- .cdtData$OpenFiles$Data[[jfile]][[2]]$value
		}else swhc <- .cdtData$GalParams$swhc$cap.max

		if(.cdtData$GalParams$wb$multi){
			wb1 <- getNCDFSampleData(.cdtData$GalParams$wb$file)
			if(is.null(wb1)){
				Insert.Messages.Out("No initial water balance data found", format = TRUE)
				return(NULL)
			}

			SP1 <- defSpatialPixels(wb1[c('lon', 'lat')])
			SP2 <- defSpatialPixels(list(lon = etp$coords$mat$x, lat = etp$coords$mat$y))
			if(is.diffSpatialPixelsObj(SP1, SP2, tol = 1e-04)){
				Insert.Messages.Out("Initial water balance, Precip & PET have different resolution or bbox", format = TRUE)
				return(NULL)
			}
			rm(SP1, SP2)

			jfile <- getIndex.AllOpenFiles(.cdtData$GalParams$wb$file)
			wb1 <- .cdtData$OpenFiles$Data[[jfile]][[2]]$value
		}else wb1 <- .cdtData$GalParams$wb$wb1

		##################

		index <- cdt.index.DailyYears(daty, .cdtData$GalParams$hdate$start.month, .cdtData$GalParams$hdate$start.day)
		index <- index$index

		miss1 <- is.na(index[[1]][1])
		ix <- do.call(c, index)
		startDaty <- min(which(!is.na(ix)))
		endDaty <- max(which(!is.na(ix)))

		if(!.cdtData$GalParams$hdate$separate.year){
			index <- if(miss1) list(index[[1]], unlist(index[-1])) else list(ix)
		}

		rg.date <- range(as.Date(daty, '%Y%m%d'), na.rm = TRUE)
		daty <- format(seq(rg.date[1], rg.date[2], 'day'), '%Y%m%d')

		##################

		index.out <- prec
		index.out$varInfo$name <- "wb"
		index.out$varInfo$units <- "mm"
		index.out$varInfo$longname <- "Daily water balance"

		dataset <- paste0("WaterBalance_", .cdtData$GalParams$Tstep)
		outDIR <- file.path(.cdtData$GalParams$output, dataset)
		dir.create(outDIR, showWarnings = FALSE, recursive = TRUE)

		datadir <- file.path(outDIR, 'DATA')
		dir.create(datadir, showWarnings = FALSE, recursive = TRUE)
		datafileIdx <- file.path(outDIR, paste0(dataset, '.rds'))

		##################

		chunkfile <- sort(unique(prec$colInfo$index))
		chunkcalc <- split(chunkfile, ceiling(chunkfile/prec$chunkfac))

		do.parChunk <- if(prec$chunkfac > length(chunkcalc)) TRUE else FALSE
		do.parCALC <- if(do.parChunk) FALSE else TRUE

		GalParams <- .cdtData$GalParams

		is.parallel <- doparallel(do.parCALC & (length(chunkcalc) > 5))
		`%parLoop%` <- is.parallel$dofun
		ret <- foreach(j = seq_along(chunkcalc), .packages = "doParallel") %parLoop% {
			rr <- readCdtDatasetChunk.sequence(chunkcalc[[j]], GalParams$cdtdataset$prec, do.par = do.parChunk)
			rr <- rr[prec$dateInfo$index, , drop = FALSE]

			et <- readCdtDatasetChunk.sequence(chunkcalc[[j]], GalParams$cdtdataset$etp, do.par = do.parChunk)
			et <- et[etp$dateInfo$index, , drop = FALSE]

			if(GalParams$swhc$multi){
				ix <- prec$colInfo$index %in% chunkcalc[[j]]
				capmx <- swhc[prec$colInfo$id[ix]]
			}else capmx <- swhc

			if(GalParams$wb$multi){
				ix <- prec$colInfo$index %in% chunkcalc[[j]]
				wb0 <- wb1[prec$colInfo$id[ix]]
			}else wb0 <- wb1

			WB <- lapply(index, function(idx){
				Water.Balance(rr[idx, , drop = FALSE], et[idx, , drop = FALSE], capmx, wb0)
			})

			if(miss1) WB[[1]][] <- NA
			WB <- do.call(rbind, WB)
			WB <- WB[startDaty:endDaty, , drop = FALSE]

			writeCdtDatasetChunk.sequence(WB, chunkcalc[[j]], index.out, datadir, do.par = do.parChunk)

			rm(rr, et, WB); gc()
			return(0)
		}
		if(is.parallel$stop) stopCluster(is.parallel$cluster)

		##################

		index.out$dateInfo$date <- daty
		index.out$dateInfo$index <- seq_along(daty)

		con <- gzfile(datafileIdx, compression = 6)
		open(con, "wb")
		saveRDS(index.out, con)
		close(con)

		rm(etp, prec, index.out)
	}

	return(0)
}
