
dailyRainAnalysisCalcProcs <- function(GeneralParameters){
	if(!dir.exists(GeneralParameters$output)){
		Insert.Messages.Out(paste(GeneralParameters$output, "did not find"), format = TRUE)
		return(NULL)
	}

	start.year <- GeneralParameters$seas$startYear
	end.year <- GeneralParameters$seas$endYear

	start.mon <- GeneralParameters$seas$startMon
	start.day <- GeneralParameters$seas$startDay
	end.mon <- GeneralParameters$seas$endMon
	end.day <- GeneralParameters$seas$endDay

	if(any(is.na(c(start.mon, start.day, end.mon, end.day)))){
		Insert.Messages.Out("Invalid season date", format = TRUE)
		return(NULL)
	}

	if(!GeneralParameters$seas$all.years & any(is.na(c(start.year, end.year)))){
		Insert.Messages.Out("Invalid year range", format = TRUE)
		return(NULL)
	}

	drywet.day <- GeneralParameters$def$drywet.day
	drywet.day <- if(drywet.day == 0) 0.0001 else drywet.day
	drywet.spell <- GeneralParameters$def$drywet.spell
	aggr.pars <- list(min.frac = GeneralParameters$min.frac,
						drywet.day = drywet.day,
						drywet.spell = drywet.spell)

	#######################

	stats.directory <- switch(GeneralParameters$stats$daily,
							'tot.rain' = "TOTALRAIN",
							'rain.int' = "RAININT",
							'nb.wet.day' = "WETDAY",
							'nb.dry.day' = "DRYDAY",
							'nb.wet.spell' = "WETSPELL",
							'nb.dry.spell' = "DRYSPELL")

	stats.season <- paste0(start.mon, "-", start.day, "_", end.mon, "-", end.day)

	################################################

	if(GeneralParameters$data.type == "cdtstation"){
		don <- getStnOpenData(GeneralParameters$cdtstation)
		if(is.null(don)) return(NULL)
		don <- getCDTdataAndDisplayMsg(don, "daily", GeneralParameters$cdtstation)
		if(is.null(don)) return(NULL)

		daty <- don$dates
	}

	if(GeneralParameters$data.type == "cdtdataset"){
		don <- try(readRDS(GeneralParameters$cdtdataset), silent = TRUE)
		if(inherits(don, "try-error")){
			Insert.Messages.Out(paste("Unable to read", GeneralParameters$cdtdataset), format = TRUE)
			return(NULL)
		}
		if(don$TimeStep != "daily"){
			Insert.Messages.Out(paste("The dataset is not a daily data"), format = TRUE)
			return(NULL)
		}

		daty <- don$dateInfo$date
	}

	year <- as.numeric(substr(daty, 1, 4))
	if(GeneralParameters$seas$all.years){
		start.year <- min(year, na.rm = TRUE)
		end.year <- max(year, na.rm = TRUE)
	}

	iyear <- year >= start.year & year <= end.year
	daty <- daty[iyear]
	if(GeneralParameters$data.type == "cdtstation") don$data <- don$data[iyear, , drop = FALSE]
	index <- cdt.index.DailySeason(daty, start.mon, start.day, end.mon, end.day)

	miss.day <- sapply(seq_along(index$index), function(jj){
		xdx <- index$index[[jj]]
		xdx <- xdx[!is.na(xdx)]
		length.day <- as.numeric(diff(as.Date(index$date[[jj]], '%Y%m%d')) + 1)
		length(xdx) / length.day < GeneralParameters$min.frac
	})

	out.daty <- sapply(index$date, paste0, collapse = '_')

	#########################################

	outDIR <- file.path(GeneralParameters$output, "DAILY.RAINFALL.ANALYSIS_data")
	dir.create(outDIR, showWarnings = FALSE, recursive = TRUE)

	################################################

	toAggr <- list(index, c(start.year, end.year), c(start.mon, start.day, end.mon, end.day),
				GeneralParameters$stats$daily, c(drywet.day, drywet.spell), GeneralParameters$min.frac)

	if(is.null(.cdtData$EnvData$toAggr)){
		aggregatData <- TRUE
	}else{
		aggregatData <- if(!isTRUE(all.equal(.cdtData$EnvData$toAggr, toAggr))) TRUE else FALSE
	}

	if(aggregatData){
		Insert.Messages.Out("Calculate seasonal statistics ......")
		if(GeneralParameters$data.type == "cdtstation"){
			dataOUT <- file.path(outDIR, stats.directory)
			dir.create(dataOUT, showWarnings = FALSE, recursive = TRUE)
			out.cdt.rds <- gzfile(file.path(dataOUT, paste0(stats.directory, '.rds')), compression = 7)
			out.cdt.csv <- file.path(dataOUT, paste0(stats.directory, '.csv'))

			dat.analys <- lapply(seq_along(index$index), function(jj){
				if(miss.day[[jj]]) return(rep(NA, length(don$id)))
				cdt.daily.statistics(don$data[index$index[[jj]], , drop = FALSE],
									STATS = GeneralParameters$stats$daily,
									pars = aggr.pars)
			})
			dat.analys <- do.call(rbind, dat.analys)
			index.out <- NULL

			##################

			xhead <- rbind(don$id, don$lon, don$lat)
			chead <- c('ID.STN', 'LON', 'SEASON/LAT')
			infohead <- cbind(chead, xhead)

			dat.tmp <- dat.analys
			dat.tmp[is.na(dat.tmp)] <- .cdtData$Config$missval
			dat.tmp <- rbind(infohead, cbind(out.daty, dat.tmp))
			writeFiles(dat.tmp, out.cdt.csv)
			saveRDS(dat.analys, out.cdt.rds)
			close(out.cdt.rds)
			rm(dat.tmp)
		}

		if(GeneralParameters$data.type == "cdtdataset"){
			datadir <- file.path(outDIR, stats.directory, 'DATA')
			dir.create(datadir, showWarnings = FALSE, recursive = TRUE)
			file.index <- file.path(outDIR, stats.directory, paste0(stats.directory, '.rds'))

			ncdfOUT.seas <- file.path(outDIR, stats.directory, 'DATA_NetCDF')
			dir.create(ncdfOUT.seas, showWarnings = FALSE, recursive = TRUE)

			##################

			index.out <- don

			CbDailyStatsVAL <- c('Total Rainfall', 'Rainfall Intensity', 'Number of Wet Days',
								'Number of Dry Days', 'Number of Wet Spells', 'Number of Dry Spells')
			stats.prefix <- switch(GeneralParameters$stats$daily,
									'tot.rain' = CbDailyStatsVAL[1],
									'rain.int' = CbDailyStatsVAL[2],
									'nb.wet.day' = CbDailyStatsVAL[3],
									'nb.dry.day' = CbDailyStatsVAL[4],
									'nb.wet.spell' = CbDailyStatsVAL[5],
									'nb.dry.spell' = CbDailyStatsVAL[6])

			stats.params <- switch(GeneralParameters$stats$daily,
									'tot.rain' = "",
									'rain.int' = "",
									'nb.wet.day' = paste0("wet day rr >= ", drywet.day, ' mm'),
									'nb.dry.day' = paste0("dry day rr < ", drywet.day, ' mm'),
									'nb.wet.spell' = paste0("wet day rr >= ", drywet.day, ' mm', ' & ', "wet spell >= ", drywet.spell, ' days'),
									'nb.dry.spell' = paste0("dry day rr < ", drywet.day, ' mm', ' & ', "dry spell >= ", drywet.spell, ' days'))

			index.out$varInfo$name <- GeneralParameters$stats$daily
			index.out$varInfo$prec <- 'float'
			index.out$varInfo$units <- switch(GeneralParameters$stats$daily,
												'tot.rain' = "mm",
												'rain.int' = "mm/day",
												'nb.wet.day' = "days",
												'nb.dry.day' = "days",
												'nb.wet.spell' = "spells",
												'nb.dry.spell' = "spells")
			index.out$varInfo$longname <- paste(stats.prefix, ":", stats.season, ";", stats.params)
			
			index.out$TimeStep <- "seasonal"
			index.out$dateInfo$date <- out.daty
			index.out$dateInfo$index <- seq_along(index$date)

			file.index.gz <- gzfile(file.index, compression = 7)
			saveRDS(index.out, file.index.gz)
			close(file.index.gz)
			
			#########################################

			chunkfile <- sort(unique(don$colInfo$index))
			chunkcalc <- split(chunkfile, ceiling(chunkfile/don$chunkfac))

			do.parChunk <- if(don$chunkfac > length(chunkcalc)) TRUE else FALSE
			do.parCALC <- if(do.parChunk) FALSE else TRUE

			is.parallel <- doparallel(do.parCALC & (length(chunkcalc) > 10))
			`%parLoop%` <- is.parallel$dofun
			ret <- foreach(chkj = seq_along(chunkcalc), .packages = "doParallel") %parLoop% {
				don.data <- readCdtDatasetChunk.sequence(chunkcalc[[chkj]], GeneralParameters$cdtdataset, do.par = do.parChunk)
				don.data <- don.data[don$dateInfo$index, , drop = FALSE]
				don.data <- don.data[iyear, , drop = FALSE]

				dat.analys <- lapply(seq_along(index$index), function(jj){
					if(miss.day[[jj]]) return(rep(NA, ncol(don.data)))
					cdt.daily.statistics(don.data[index$index[[jj]], , drop = FALSE],
										STATS = GeneralParameters$stats$daily,
										pars = aggr.pars)
				})
				dat.analys <- do.call(rbind, dat.analys)

				writeCdtDatasetChunk.sequence(dat.analys, chunkcalc[[chkj]], index.out, datadir, do.par = do.parChunk)
				rm(dat.analys, don.data); gc()
			}
			if(is.parallel$stop) stopCluster(is.parallel$cluster)

			######################
			x <- index.out$coords$mat$x
			y <- index.out$coords$mat$y
			dx <- ncdim_def("Lon", "degreeE", x)
			dy <- ncdim_def("Lat", "degreeN", y)
			xy.dim <- list(dx, dy)
			nc.grd <- ncvar_def(index.out$varInfo$name, index.out$varInfo$units, xy.dim, -99, index.out$varInfo$longname, "float", compression = 9)

			######################

			dat.analys <- readCdtDatasetChunk.multi.dates.order(file.index, out.daty)

			for(j in seq_along(out.daty)){
				ncdat.seas <- dat.analys[j, ]
				dim(ncdat.seas) <- c(length(x), length(y))
				ncdat.seas[is.na(ncdat.seas)] <- -99

				filenc <- file.path(ncdfOUT.seas, paste0('Seas_', out.daty[j], '.nc'))
				nc <- nc_create(filenc, nc.grd)
				ncvar_put(nc, nc.grd, ncdat.seas)
				nc_close(nc)
			}
		}

		.cdtData$EnvData$dat.analys <- dat.analys
		.cdtData$EnvData$index.out <- index.out
		.cdtData$EnvData$toAggr <- toAggr
		Insert.Messages.Out("Computing seasonal statistics done!")
	}else{
		dat.analys <- .cdtData$EnvData$dat.analys
		index.out <- .cdtData$EnvData$index.out
	}

	################################################

	if(GeneralParameters$stats$yearly == "mean")
		dat.yearly <- colMeans(dat.analys, na.rm = TRUE)
	if(GeneralParameters$stats$yearly == "stdev")
		dat.yearly <- colSds(dat.analys, na.rm = TRUE)
	if(GeneralParameters$stats$yearly == "coefvar")
		dat.yearly <- 100 * colSds(dat.analys, na.rm = TRUE) / colMeans(dat.analys, na.rm = TRUE)
	if(GeneralParameters$stats$yearly == "proba")
		dat.yearly <- 100 * colSums(!is.na(dat.analys) & dat.analys >= GeneralParameters$def$proba.thres)/colSums(!is.na(dat.analys))

	################################################

	out.index.file <- file.path(outDIR, "DailyRainfallAnalysis.rds")

	if(file.exists(out.index.file)){
		outIdx <- readRDS(out.index.file)
		calc.stats <- outIdx$exist.vars.dates
	}else calc.stats <- list()
	
	calc.stats[[stats.directory]]$date <- out.daty
	daty.range <- out.daty[c(1, length(out.daty))]
	daty.range <- cbind(substr(daty.range, 1, 4), substr(daty.range, 10, 13))
	vars.def <- c(GeneralParameters$def$drywet.day, GeneralParameters$def$drywet.spell, GeneralParameters$def$proba.thres)
	calc.stats[[stats.directory]][[GeneralParameters$stats$yearly]] <- list(year = daty.range, season = stats.season, pars = vars.def)

	last.vars <- c(stats.directory, GeneralParameters$stats$yearly)

	################################################

	if(GeneralParameters$data.type == "cdtstation"){
		stn.data <- list(id = don$id, lon = don$lon, lat = don$lat)
		output <- list(params = GeneralParameters, data = stn.data, exist.vars.dates = calc.stats, last = last.vars)

		##################
		datadir <- file.path(outDIR, 'CDTSTATIONS_STATS')
		dir.create(datadir, showWarnings = FALSE, recursive = TRUE)

		dataOUT <- file.path(outDIR, 'CDTDATASET')
		dir.create(dataOUT, showWarnings = FALSE, recursive = TRUE)

		out.cdt.stats.csv <- file.path(datadir, paste0(stats.directory, '_', GeneralParameters$stats$yearly, '.csv'))
		out.cdt.stats.rds <- file.path(dataOUT, paste0(stats.directory, '_', GeneralParameters$stats$yearly, '.rds'))

		##################

		con <- gzfile(out.cdt.stats.rds, compression = 7)
		open(con, "wb")
		saveRDS(dat.yearly, con)
		close(con)

		##################
		xhead <- rbind(don$id, don$lon, don$lat)
		chead <- c('ID.STN', 'LON', 'STAT/LAT')
		infohead <- cbind(chead, xhead)

		dat.yearly <- round(dat.yearly, 3)
		dat.yearly[is.na(dat.yearly)] <- -99
		dat.yearly <- rbind(infohead, c(paste0(GeneralParameters$stats$yearly, ':', stats.season), dat.yearly))
		writeFiles(dat.yearly, out.cdt.stats.csv)
	}

	if(GeneralParameters$data.type == "cdtdataset"){
		output <- list(params = GeneralParameters, exist.vars.dates = calc.stats, last = last.vars)

		######################
		ncdfOUT <- file.path(outDIR, 'DATA_NetCDF_STATS')
		dir.create(ncdfOUT, showWarnings = FALSE, recursive = TRUE)

		nom <- paste0(GeneralParameters$stats$yearly, ".", index.out$varInfo$name)
		nom_long <- paste(GeneralParameters$stats$yearly, index.out$varInfo$longname)

		if(GeneralParameters$stats$yearly == "mean") unite <- index.out$varInfo$units
		if(GeneralParameters$stats$yearly == "stdev") unite <- index.out$varInfo$units
		if(GeneralParameters$stats$yearly == "coefvar") unite <- "%"
		if(GeneralParameters$stats$yearly == "proba") unite <- "%"

		x <- index.out$coords$mat$x
		y <- index.out$coords$mat$y
		dx <- ncdim_def("Lon", "degreeE", x)
		dy <- ncdim_def("Lat", "degreeN", y)
		xy.dim <- list(dx, dy)
		nc.grd <- ncvar_def(nom, unite, xy.dim, -99, nom_long, "float", compression = 9)

		######################

		dim(dat.yearly) <- c(length(x), length(y))
		dat.yearly[is.na(dat.yearly)] <- -99

		filenc <- file.path(ncdfOUT, paste0(stats.directory, '_', GeneralParameters$stats$yearly, '.nc'))
		nc <- nc_create(filenc, nc.grd)
		ncvar_put(nc, nc.grd, dat.yearly)
		nc_close(nc)
	}

	##################
	con <- gzfile(out.index.file, compression = 9)
	open(con, "wb")
	saveRDS(output, con)
	close(con)

	##################
	.cdtData$EnvData$output <- output
	.cdtData$EnvData$PathData <- outDIR

	return(0)
}
