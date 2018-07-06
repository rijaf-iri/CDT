
compute_SeasonCessation_Procs <- function(GeneralParameters){
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

	wb <- NULL
	prec <- NULL
	etp <- NULL

	omethods <- sapply(GeneralParameters$onset.criteria, "[[", "method")
	USEWB0 <- NULL
	USEWB1 <- NULL
	CALCWB <- FALSE

	if(GeneralParameters$data.type == "cdtstation"){
		if(any(omethods == 1) & GeneralParameters$wb.data){
			if(GeneralParameters$cdtstation$wb == ""){
				Insert.Messages.Out(.cdtData$EnvData[['message']][['27']], format = TRUE)
				return(NULL)
			}

			wb <- getStnOpenData(GeneralParameters$cdtstation$wb)
			if(is.null(wb)) return(NULL)
			wb <- getCDTdataAndDisplayMsg(wb, "daily", GeneralParameters$cdtstation$wb)
			if(is.null(wb)) return(NULL)

			daty0 <- wb$dates
			stn.id0 <- wb$id
			stn.lon0 <- wb$lon
			stn.lat0 <- wb$lat

			USEWB0 <- TRUE
		}

		if(any(omethods != 1) | (any(omethods == 1) & !GeneralParameters$wb.data)){
			if(GeneralParameters$cdtstation$prec == ""){
				Insert.Messages.Out(.cdtData$EnvData[['message']][['28']], format = TRUE)
				return(NULL)
			}
			if(GeneralParameters$cdtstation$etp == ""){
				Insert.Messages.Out(.cdtData$EnvData[['message']][['29']], format = TRUE)
				return(NULL)
			}

			prec <- getStnOpenData(GeneralParameters$cdtstation$prec)
			if(is.null(prec)) return(NULL)
			prec <- getCDTdataAndDisplayMsg(prec, "daily", GeneralParameters$cdtstation$prec)
			if(is.null(prec)) return(NULL)

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
			daty1 <- etp$dates

			##################
			jnx <- match(prec$id, etp$id)
			jnx <- jnx[!is.na(jnx)]
			etp$id <- etp$id[jnx]

			stn.id1 <- etp$id
			stn.lon1 <- etp$lon[jnx]
			stn.lat1 <- etp$lat[jnx]
			etp$data <- etp$data[, jnx, drop = FALSE]
			prec$data <- prec$data[, prec$id %in% etp$id, drop = FALSE]

			USEWB1 <- if(all(omethods != 1)) FALSE else TRUE
		}

		##################

		if(!is.null(USEWB1)){
			if(!USEWB1){
				daty <- daty1
				stn.id <- stn.id1
				stn.lon <- stn.lon1
				stn.lat <- stn.lat1
			}else{
				if(!is.null(USEWB0)){
					if(!any(prec$id %in% wb$id)){
						Insert.Messages.Out(.cdtData$EnvData[['message']][['30']], format = TRUE)
						return(NULL)
					}
					if(!any(prec$dates %in% wb$dates)){
						Insert.Messages.Out(.cdtData$EnvData[['message']][['31']], format = TRUE)
						return(NULL)
					}

					inx <- match(prec$dates, wb$dates)
					inx <- inx[!is.na(inx)]
					wb$dates <- wb$dates[inx]
					wb$data <- wb$data[inx, , drop = FALSE]
					prec$data <- prec$data[prec$dates %in% wb$dates, , drop = FALSE]
					etp$data <- etp$data[prec$dates %in% wb$dates, , drop = FALSE]
					daty <- wb$dates

					########
					jnx <- match(prec$id, wb$id)
					jnx <- jnx[!is.na(jnx)]
					wb$id <- wb$id[jnx]

					stn.id <- wb$id
					stn.lon <- wb$lon[jnx]
					stn.lat <- wb$lat[jnx]
					wb$data <- wb$data[, jnx, drop = FALSE]
					prec$data <- prec$data[, prec$id %in% wb$id, drop = FALSE]
					etp$data <- etp$data[, prec$id %in% wb$id, drop = FALSE]					
				}else{
					daty <- daty1
					stn.id <- stn.id1
					stn.lon <- stn.lon1
					stn.lat <- stn.lat1

					CALCWB <- TRUE
				}
			}
		}else{
			daty <- daty0
			stn.id <- stn.id0
			stn.lon <- stn.lon0
			stn.lat <- stn.lat0
		}

		##################

		if(CALCWB){
			if(GeneralParameters$wb.pars$swhc$multi){
				swhc <- getStnOpenData(GeneralParameters$wb.pars$swhc$file)
				if(is.null(swhc)) return(NULL)

				swhc.id <- as.character(swhc[1, -1])
				swhc <- as.numeric(swhc[nrow(swhc), -1])

				if(!isTRUE(all.equal(stn.id, swhc.id))){
					Insert.Messages.Out(.cdtData$EnvData[['message']][['32']], format = TRUE)
					return(NULL)
				}
			}else swhc <- GeneralParameters$wb.pars$swhc$cap.max

			if(GeneralParameters$wb.pars$wb$multi){
				wb1 <- getStnOpenData(GeneralParameters$wb.pars$wb$file)
				if(is.null(wb1)) return(NULL)

				wb1.id <- as.character(wb1[1, -1])
				wb1 <- as.numeric(wb1[nrow(wb1), -1])

				if(!isTRUE(all.equal(stn.id, wb1.id))){
					Insert.Messages.Out(.cdtData$EnvData[['message']][['33']], format = TRUE)
					return(NULL)
				}
			}else wb1 <- GeneralParameters$wb.pars$wb$wb1

			##################
			Insert.Messages.Out(.cdtData$EnvData[['message']][['34']])

			index <- cdt.index.DailyYears(daty, GeneralParameters$wb.pars$hdate$start.month,
												GeneralParameters$wb.pars$hdate$start.day)
			index <- index$index

			##################
			miss1 <- is.na(index[[1]][1])
			ix <- do.call(c, index)
			startDaty <- min(which(!is.na(ix)))
			endDaty <- max(which(!is.na(ix)))

			if(!GeneralParameters$wb.pars$hdate$separate.year){
				index <- if(miss1) list(index[[1]], unlist(index[-1])) else list(ix)
			}

			##################

			wb.out <- lapply(index, function(idx){
				Water.Balance(prec$data[idx, , drop = FALSE], etp$data[idx, , drop = FALSE], swhc, wb1)
			})
			if(miss1) wb.out[[1]][] <- NA
			wb.out <- do.call(rbind, wb.out)
			wb.out <- wb.out[startDaty:endDaty, , drop = FALSE]
			wb <- NULL
			wb$data <- wb.out
			Insert.Messages.Out(.cdtData$EnvData[['message']][['34-a']])
		}

		##################

		criteria <- GeneralParameters$onset.criteria
		subdiv <- cdt.index.Region.Subdiv(stn.lat, stn.lon, shpf, shpdiv, GeneralParameters$onset.reg)
		divexist <- sapply(subdiv, length) > 0
		subdiv <- subdiv[divexist]
		criteria <- criteria[divexist]

		##################

		cessation <- cdt.Season.Cessation(daty, subdiv, criteria, wb$data,
								prec$data, etp$data, GeneralParameters$min.frac)
		cessation.num <- cessation$cessation.num
		start.date <- cessation$start.date
		cessation <- cessation$cessation.date

		##################

		outDIR <- file.path(GeneralParameters$output, "CESSATION_data")
		dir.create(outDIR, showWarnings = FALSE, recursive = TRUE)

		datadir <- file.path(outDIR, 'CDTSTATIONS')
		dir.create(datadir, showWarnings = FALSE, recursive = TRUE)

		dataOUT <- file.path(outDIR, 'CDTDATASET')
		dir.create(dataOUT, showWarnings = FALSE, recursive = TRUE)

		file.cessation <- file.path(datadir, "Cessation_dates.txt")
		file.cessation1 <- file.path(datadir, "Cessation_days.txt")
		file.index <- file.path(outDIR, "Cessation.rds")
		file.cdt.wb <- file.path(dataOUT, "WB.rds")
		file.cdt.cessation <- file.path(dataOUT, "CESSATION.rds")

		if(CALCWB) file.waterbalance <- file.path(datadir, "WaterBalance.txt")
		file.cdt.prec <- file.path(dataOUT, "PRECIP.rds")
		file.cdt.etp <- file.path(dataOUT, "PET.rds")

		##################

		stn.data <- list(id = stn.id, lon = stn.lon, lat = stn.lat, date = daty)
		output <- list(params = GeneralParameters, data = stn.data, start.date = start.date)

		.cdtData$EnvData$output <- output
		.cdtData$EnvData$PathData <- outDIR

		##################
		con <- gzfile(file.index, compression = 7)
		open(con, "wb")
		saveRDS(output, con)
		close(con)

		##################
		con <- gzfile(file.cdt.wb, compression = 9)
		open(con, "wb")
		saveRDS(wb$data, con)
		close(con)

		if(!is.null(prec)){
			con <- gzfile(file.cdt.prec, compression = 9)
			open(con, "wb")
			saveRDS(prec$data, con)
			close(con)
			rm(prec)
		}

		if(!is.null(etp)){
			con <- gzfile(file.cdt.etp, compression = 9)
			open(con, "wb")
			saveRDS(etp$data, con)
			close(con)
			rm(etp)
		}

		##################
		con <- gzfile(file.cdt.cessation, compression = 7)
		open(con, "wb")
		saveRDS(cessation.num, con)
		close(con)

		##################

		cessation.num <- cessation.num - start.date
		cessation[is.na(cessation)] <- .cdtData$Config$missval
		cessation.num[is.na(cessation.num)] <- .cdtData$Config$missval
		start.date <- format(start.date, "%Y%m%d")

		xhead <- rbind(stn.id, stn.lon, stn.lat)
		chead <- c('ID.STN', 'LON', 'START.DATE/LAT')
		infohead <- cbind(chead, xhead)

		cessation <- rbind(infohead, cbind(start.date, cessation))
		cessation.num <- rbind(infohead, cbind(start.date, cessation.num))

		writeFiles(cessation, file.cessation)
		writeFiles(cessation.num, file.cessation1)

		if(CALCWB){
			wb <- round(wb$data, 1)
			wb[is.na(wb)] <- .cdtData$Config$missval
			wb <- rbind(infohead, cbind(daty, wb))
			writeFiles(wb, file.waterbalance)
		}

		rm(wb, cessation, cessation.num, stn.data, output)
	}

	####################################################

	if(GeneralParameters$data.type == "cdtdataset"){
		if(any(omethods == 1) & GeneralParameters$wb.data){
			if(GeneralParameters$cdtdataset$wb == ""){
				Insert.Messages.Out(.cdtData$EnvData[['message']][['27']], format = TRUE)
				return(NULL)
			}

			wb <- try(readRDS(GeneralParameters$cdtdataset$wb), silent = TRUE)
			if(inherits(wb, "try-error")){
				Insert.Messages.Out(paste(.cdtData$EnvData[['message']][['20']], GeneralParameters$cdtdataset$wb), format = TRUE)
				return(NULL)
			}
			if(wb$TimeStep != "daily"){
				Insert.Messages.Out(.cdtData$EnvData[['message']][['21']], format = TRUE)
				return(NULL)
			}

			USEWB0 <- TRUE
		}

		##################

		if(any(omethods != 1) | (any(omethods == 1) & !GeneralParameters$wb.data)){
			if(GeneralParameters$cdtdataset$prec == ""){
				Insert.Messages.Out(.cdtData$EnvData[['message']][['28']], format = TRUE)
				return(NULL)
			}
			if(GeneralParameters$cdtdataset$etp == ""){
				Insert.Messages.Out(.cdtData$EnvData[['message']][['29']], format = TRUE)
				return(NULL)
			}

			prec <- try(readRDS(GeneralParameters$cdtdataset$prec), silent = TRUE)
			if(inherits(prec, "try-error")){
				Insert.Messages.Out(paste(.cdtData$EnvData[['message']][['20']], GeneralParameters$cdtdataset$prec), format = TRUE)
				return(NULL)
			}
			if(prec$TimeStep != "daily"){
				Insert.Messages.Out(.cdtData$EnvData[['message']][['22']], format = TRUE)
				return(NULL)
			}

			etp <- try(readRDS(GeneralParameters$cdtdataset$etp), silent = TRUE)
			if(inherits(etp, "try-error")){
				Insert.Messages.Out(paste(.cdtData$EnvData[['message']][['20']], GeneralParameters$cdtdataset$etp), format = TRUE)
				return(NULL)
			}
			if(etp$TimeStep != "daily"){
				Insert.Messages.Out(.cdtData$EnvData[['message']][['22-a']], format = TRUE)
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

			USEWB1 <- if(all(omethods != 1)) FALSE else TRUE
		}

		##################

		if(!is.null(USEWB1)){
			if(!USEWB1){
				index.out <- prec

				wbOK <- FALSE
				precOK <- TRUE
			}else{
				if(!is.null(USEWB0)){
					SP1 <- defSpatialPixels(list(lon = prec$coords$mat$x, lat = prec$coords$mat$y))
					SP2 <- defSpatialPixels(list(lon = wb$coords$mat$x, lat = wb$coords$mat$y))
					if(is.diffSpatialPixelsObj(SP1, SP2, tol = 1e-04)){
						Insert.Messages.Out(.cdtData$EnvData[['message']][['23-a']], format = TRUE)
						return(NULL)
					}
					rm(SP1, SP2)

					##################
					if(prec$chunksize != wb$chunksize){
						Insert.Messages.Out(.cdtData$EnvData[['message']][['24-a']], format = TRUE)
						return(NULL)
					}

					##################
					if(!any(prec$dateInfo$date %in% wb$dateInfo$date)){
						Insert.Messages.Out(.cdtData$EnvData[['message']][['25-a']], format = TRUE)
						return(NULL)
					}

					##################

					inx <- match(prec$dateInfo$date, wb$dateInfo$date)
					inx <- inx[!is.na(inx)]
					wb$dateInfo$date <- wb$dateInfo$date[inx]
					wb$dateInfo$index <- wb$dateInfo$index[inx]

					pdaty <- prec$dateInfo$date %in% wb$dateInfo$date
					prec$dateInfo$date <- prec$dateInfo$date[pdaty]
					prec$dateInfo$index <- prec$dateInfo$index[pdaty]
					etp$dateInfo$date <- etp$dateInfo$date[pdaty]
					etp$dateInfo$index <- etp$dateInfo$index[pdaty]

					index.out <- wb

					wbOK <- TRUE
					precOK <- TRUE
				}else{
					CALCWB <- TRUE
					index.out <- prec

					wbOK <- FALSE
					precOK <- TRUE
				}
			}
		}else{
			if(!is.null(USEWB0)){
				index.out <- wb

				wbOK <- TRUE
				precOK <- FALSE
			}else{
				Insert.Messages.Out(.cdtData$EnvData[['message']][['26']], format = TRUE)
				return(NULL)
			}
		}

		##################

		outDIR <- file.path(GeneralParameters$output, "CESSATION_data")
		dir.create(outDIR, showWarnings = FALSE, recursive = TRUE)

		ncdfOUT <- file.path(outDIR, 'DATA_NetCDF')
		dir.create(ncdfOUT, showWarnings = FALSE, recursive = TRUE)

		dataOUT <- file.path(outDIR, 'CDTDATASET')
		dir.create(dataOUT, showWarnings = FALSE, recursive = TRUE)

		datadir <- file.path(dataOUT, 'DATA')
		dir.create(datadir, showWarnings = FALSE, recursive = TRUE)
		datafileIdx <- file.path(dataOUT, 'CDTDATASET.rds')
		file.index <- file.path(outDIR, "Cessation.rds")

		index.out$varInfo$name <- "cessation"
		index.out$varInfo$units <- "days since 1970-01-01"
		index.out$varInfo$longname <- "Cessation date of rainy season"

		##################

		if(CALCWB){
			if(GeneralParameters$wb.pars$swhc$multi){
				swhc <- getNCDFSampleData(.GeneralParameters$wb.pars$swhc$file)
				if(is.null(swhc)){
					Insert.Messages.Out(.cdtData$EnvData[['message']][['35']], format = TRUE)
					return(NULL)
				}

				SP1 <- defSpatialPixels(swhc[c('lon', 'lat')])
				SP2 <- defSpatialPixels(list(lon = etp$coords$mat$x, lat = etp$coords$mat$y))
				if(is.diffSpatialPixelsObj(SP1, SP2, tol = 1e-04)){
					Insert.Messages.Out(.cdtData$EnvData[['message']][['35-a']], format = TRUE)
					return(NULL)
				}
				rm(SP1, SP2)

				jfile <- getIndex.AllOpenFiles(GeneralParameters$wb.pars$swhc$file)
				swhc <- .cdtData$OpenFiles$Data[[jfile]][[2]]$value
			}else swhc <- GeneralParameters$wb.pars$swhc$cap.max

			if(GeneralParameters$wb.pars$wb$multi){
				wb1 <- getNCDFSampleData(GeneralParameters$wb.pars$wb$file)
				if(is.null(wb1)){
					Insert.Messages.Out(.cdtData$EnvData[['message']][['36']], format = TRUE)
					return(NULL)
				}

				SP1 <- defSpatialPixels(wb1[c('lon', 'lat')])
				SP2 <- defSpatialPixels(list(lon = etp$coords$mat$x, lat = etp$coords$mat$y))
				if(is.diffSpatialPixelsObj(SP1, SP2, tol = 1e-04)){
					Insert.Messages.Out(.cdtData$EnvData[['message']][['36-a']], format = TRUE)
					return(NULL)
				}
				rm(SP1, SP2)

				jfile <- getIndex.AllOpenFiles(GeneralParameters$wb.pars$wb$file)
				wb1 <- .cdtData$OpenFiles$Data[[jfile]][[2]]$value
			}else wb1 <- GeneralParameters$wb.pars$wb$wb1

			index.wb <- cdt.index.DailyYears(index.out$dateInfo$date,
										GeneralParameters$wb.pars$hdate$start.month,
										GeneralParameters$wb.pars$hdate$start.day)
			index.wb <- index.wb$index

			miss1 <- is.na(index.wb[[1]][1])
			ix <- do.call(c, index.wb)
			startDaty <- min(which(!is.na(ix)))
			endDaty <- max(which(!is.na(ix)))

			if(!GeneralParameters$wb.pars$hdate$separate.year){
				index.wb <- if(miss1) list(index.wb[[1]], unlist(index.wb[-1])) else list(ix)
			}

			index.out.wb <- index.out
			index.out.wb$varInfo$name <- "wb"
			index.out.wb$varInfo$units <- "mm"
			index.out.wb$varInfo$longname <- "Daily water balance"

			wb.DIR <- file.path(outDIR, "WaterBalance")
			dir.create(wb.DIR, showWarnings = FALSE, recursive = TRUE)

			wb.datadir <- file.path(wb.DIR, 'DATA')
			dir.create(wb.datadir, showWarnings = FALSE, recursive = TRUE)
			wb.datafileIdx <- file.path(wb.DIR, "WaterBalance.rds")
		}

		##################

		chunkfile <- sort(unique(index.out$colInfo$index))
		chunkcalc <- split(chunkfile, ceiling(chunkfile / index.out$chunkfac))

		do.parChunk <- if(index.out$chunkfac > length(chunkcalc)) TRUE else FALSE
		do.parCALC <- if(do.parChunk) FALSE else TRUE

		is.parallel <- doparallel(do.parCALC & (length(chunkcalc) > 10))
		`%parLoop%` <- is.parallel$dofun
		ret <- foreach(jj = seq_along(chunkcalc), .packages = "doParallel") %parLoop% {
			rr.data <- NULL
			et.data <- NULL
			wb.data <- NULL

			if(wbOK){
				wb.data <- readCdtDatasetChunk.sequence(chunkcalc[[jj]], GeneralParameters$cdtdataset$wb, do.par = do.parChunk)
				wb.data <- wb.data[wb$dateInfo$index, , drop = FALSE]
			}

			if(precOK){
				rr.data <- readCdtDatasetChunk.sequence(chunkcalc[[jj]], GeneralParameters$cdtdataset$prec, do.par = do.parChunk)
				rr.data <- rr.data[prec$dateInfo$index, , drop = FALSE]

				et.data <- readCdtDatasetChunk.sequence(chunkcalc[[jj]], GeneralParameters$cdtdataset$etp, do.par = do.parChunk)
				et.data <- et.data[etp$dateInfo$index, , drop = FALSE]
			}

			####################################

			if(CALCWB){
				if(GeneralParameters$wb.pars$swhc$multi){
					ix <- prec$colInfo$index %in% chunkcalc[[jj]]
					capmx <- swhc[prec$colInfo$id[ix]]
				}else capmx <- swhc

				if(GeneralParameters$wb.pars$wb$multi){
					ix <- prec$colInfo$index %in% chunkcalc[[jj]]
					wb0 <- wb1[prec$colInfo$id[ix]]
				}else wb0 <- wb1

				wb.data <- lapply(index.wb, function(idx){
					Water.Balance(rr.data[idx, , drop = FALSE], et.data[idx, , drop = FALSE], capmx, wb0)
				})

				if(miss1) wb.data[[1]][] <- NA
				wb.data <- do.call(rbind, wb.data)
				wb.data <- wb.data[startDaty:endDaty, , drop = FALSE]

				writeCdtDatasetChunk.sequence(wb.data, chunkcalc[[jj]], index.out.wb, wb.datadir, do.par = do.parChunk)
			}

			####################################

			lat <- index.out$coords$df$y[index.out$colInfo$index %in% chunkcalc[[jj]]]
			lon <- index.out$coords$df$x[index.out$colInfo$index %in% chunkcalc[[jj]]]
			criteria <- GeneralParameters$onset.criteria

			####################################

			subdiv <- cdt.index.Region.Subdiv(lat, lon, shpf, shpdiv, GeneralParameters$onset.reg)
			divexist <- sapply(subdiv, length) > 0
			subdiv <- subdiv[divexist]
			criteria <- criteria[divexist]

			cessation <- cdt.Season.Cessation(index.out$dateInfo$date, subdiv, criteria,
											wb.data, rr.data, et.data,
											GeneralParameters$min.frac)
			start.date <- cessation$start.date
			cessation <- cessation$cessation.num

			####################################

			writeCdtDatasetChunk.sequence(cessation, chunkcalc[[jj]], index.out, datadir, do.par = do.parChunk)

			rm(wb.data, rr.data, et.data, cessation); gc()
			return(start.date)
		}
		if(is.parallel$stop) stopCluster(is.parallel$cluster)

		####################################

		start.date <- as.Date(rowMins(do.call(cbind, ret)), origin = "1970-1-1")
		output <- list(params = GeneralParameters, start.date = start.date, ts.date = index.out$dateInfo$date)
		.cdtData$EnvData$output <- output
		.cdtData$EnvData$PathData <- outDIR

		##################
		if(CALCWB){
			con <- gzfile(wb.datafileIdx, compression = 6)
			open(con, "wb")
			saveRDS(index.out.wb, con)
			close(con)
		}

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
				grdNC <- ncvar_def("cessation", paste("days since", dates[j]), xyt.dim, -99,
									longname = "Cessation date of rainy season", prec = "short",
									shuffle = TRUE, compression = 9)

				filenc <- file.path(ncdfOUT, paste0("cessation_", dates[j], ".nc"))
				nc <- nc_create(filenc, grdNC)
				ncvar_put(nc, grdNC, don)
				nc_close(nc)
			}
			return(0)
		})
	}

	return(0)
}
