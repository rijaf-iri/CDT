
AggregateTS_Execute <- function(){
	Insert.Messages.Out(.cdtData$GalParams[['message']][['6']])

	period <- .cdtData$GalParams$in.tstep
	period1 <- .cdtData$GalParams$out.tstep
	min.frac <- .cdtData$GalParams$aggr.series$min.frac
	datatype <- .cdtData$GalParams$data.type

	if(datatype == 'cdtstation'){
		donne <- getStnOpenData(.cdtData$GalParams$cdtstation)
		if(is.null(donne)) return(NULL)
		donne <- getCDTdataAndDisplayMsg(donne, period, .cdtData$GalParams$cdtstation)
		if(is.null(donne)) return(NULL)
		miss.val <- getStnOpenDataInfo(.cdtData$GalParams$cdtstation)[[3]]$miss.val
		dates <- donne$dates
	}

	if(datatype == 'cdtdataset'){
		donne <- try(readRDS(.cdtData$GalParams$cdtdataset), silent = TRUE)
		if(inherits(donne, "try-error")){
			Insert.Messages.Out(paste(.cdtData$GalParams[['message']][['7']], .cdtData$GalParams$cdtdataset), format = TRUE)
			return(NULL)
		}
		if(period != donne$TimeStep){
			Insert.Messages.Out(paste(.cdtData$GalParams[['message']][['8']], period), format = TRUE)
			return(NULL)
		}
		dates <- donne$dateInfo$date
	}

	if(datatype == 'cdtnetcdf'){
		istart.yrs <- .cdtData$GalParams$Date.Range$start.year
		istart.mon <- .cdtData$GalParams$Date.Range$start.mon
		istart.day <- .cdtData$GalParams$Date.Range$start.day
		iend.yrs <- .cdtData$GalParams$Date.Range$end.year
		iend.mon <- .cdtData$GalParams$Date.Range$end.mon
		iend.day <- .cdtData$GalParams$Date.Range$end.day

		dstart <- as.Date(paste(istart.yrs, istart.mon, istart.day, sep = '-'))
		dend <- as.Date(paste(iend.yrs, iend.mon, iend.day, sep = '-'))
		if(is.na(dstart)){
			Insert.Messages.Out(.cdtData$GalParams[['message']][['9']], format = TRUE)
			return(NULL)
		}
		if(is.na(dend)){
			Insert.Messages.Out(.cdtData$GalParams[['message']][['10']], format = TRUE)
			return(NULL)
		}

		if(period == "daily"){
			dates <- format(seq(dstart, dend, 'day'), '%Y%m%d')
			ncfiles <- sprintf(.cdtData$GalParams$cdtnetcdf$format, substr(dates, 1, 4), substr(dates, 5, 6), substr(dates, 7, 8))
		}
		if(period == "pentad"){
			dates <- seq(dstart, dend, 'day')
			dates <- paste0(format(dates[which(as.numeric(format(dates, '%d')) <= 6)], '%Y%m'),
							as.numeric(format(dates[which(as.numeric(format(dates, '%d')) <= 6)], '%d')))
			ncfiles <- sprintf(.cdtData$GalParams$cdtnetcdf$format, substr(dates, 1, 4), substr(dates, 5, 6), substr(dates, 7, 7))
		}
		if(period == "dekadal"){
			dates <- seq(dstart, dend, 'day')
			dates <- paste0(format(dates[which(as.numeric(format(dates, '%d')) <= 3)], '%Y%m'),
							as.numeric(format(dates[which(as.numeric(format(dates, '%d')) <= 3)], '%d')))
			ncfiles <- sprintf(.cdtData$GalParams$cdtnetcdf$format, substr(dates, 1, 4), substr(dates, 5, 6), substr(dates, 7, 7))
		}
		if(period == "monthly"){
			dates <- format(seq(dstart, dend, 'month'), '%Y%m')
			ncfiles <- sprintf(.cdtData$GalParams$cdtnetcdf$format, substr(dates, 1, 4), substr(dates, 5, 6))
		}

		ncPATH <- file.path(.cdtData$GalParams$cdtnetcdf$dir, ncfiles)
		ncEXIST <- unlist(lapply(ncPATH, file.exists))
		if(!any(ncEXIST)){
			Insert.Messages.Out(.cdtData$GalParams[['message']][['11']], format = TRUE)
			return(NULL)
		}

		ncsample <- getNCDFSampleData(.cdtData$GalParams$cdtnetcdf$sample)
		if(is.null(ncsample)){
			Insert.Messages.Out(.cdtData$GalParams[['message']][['12']], format = TRUE)
			return(NULL)
		}
		ncINFO <- list(xo = ncsample$ilon, yo = ncsample$ilat, varid = ncsample$varid)
	}

	#########################
	## index dates

	startMonth <- .cdtData$GalParams$Seasonal$start.mon
	seasonLength <- .cdtData$GalParams$Seasonal$length.mon

	agg.index <- cdt.index.aggregate(dates, period, period1, seasonLength, startMonth)
	if(is.null(agg.index)) return(NULL)

	ifull <- (agg.index$nba / agg.index$nb0) >= min.frac

	odaty <- agg.index$date[ifull]
	index <- agg.index$index[ifull]
	nbd.in <- agg.index$nb0[ifull]

	#########################

	if(datatype == 'cdtstation'){
		cdtdata <- cdt.data.aggregate(donne$data, index, pars = .cdtData$GalParams$aggr.series)

		if(is.null(donne$elv)){
			headers <- t(cbind(donne$id, donne$lon, donne$lat))
			capition <- c('Stations', 'LON', paste(toupper(period1), 'LAT', sep = '/'))
		}else{
			headers <- t(cbind(donne$id, donne$lon, donne$lat, donne$elv))
			capition <- c('Stations', 'LON', 'LAT', paste(toupper(period1), 'ELV', sep = '/'))
		}

		entete <- cbind(capition, headers)
		cdtdata <- rbind(entete, cbind(odaty, cdtdata))
		cdtdata[is.na(cdtdata)] <- miss.val
		writeFiles(cdtdata, .cdtData$GalParams$output)
		rm(cdtdata)
	}

	if(datatype == 'cdtdataset'){
		outputDIR <- file.path(.cdtData$GalParams$output, "Aggregated_Data")
		dataDIR <- file.path(outputDIR, "DATA")
		dir.create(dataDIR, showWarnings = FALSE, recursive = TRUE)
		file.index <- file.path(outputDIR, "Aggregated_Data.rds")

		index.agg <- donne
		index.agg$TimeStep <- period1
		index.agg$dateInfo$date <- odaty
		index.agg$dateInfo$index <- seq_along(odaty)

		con <- gzfile(file.index, compression = 7)
		open(con, "wb")
		saveRDS(index.agg, con)
		close(con)

		##########

		chunkfile <- sort(unique(donne$colInfo$index))
		chunkcalc <- split(chunkfile, ceiling(chunkfile / donne$chunkfac))

		do.parChunk <- if(donne$chunkfac > length(chunkcalc)) TRUE else FALSE
		do.parCALC <- if(do.parChunk) FALSE else TRUE

		##########
		GalParams <- .cdtData$GalParams

		##########

		is.parallel <- doparallel(do.parCALC & (length(chunkcalc) > 10))
		`%parLoop%` <- is.parallel$dofun
		ret <- foreach(jj = seq_along(chunkcalc), .packages = "doParallel") %parLoop% {
			don.data <- readCdtDatasetChunk.sequence(chunkcalc[[jj]], GalParams$cdtdataset, do.par = do.parChunk)
			don.data <- don.data[donne$dateInfo$index, , drop = FALSE]

			cdtdata <- cdt.data.aggregate(don.data, index, pars = GalParams$aggr.series)

			writeCdtDatasetChunk.sequence(cdtdata, chunkcalc[[jj]], index.agg, dataDIR, do.par = do.parChunk)
			rm(don.data, cdtdata); gc()
		}
		if(is.parallel$stop) stopCluster(is.parallel$cluster)
		rm(GalParams)
	}

	if(datatype == 'cdtnetcdf'){
		outputDIR <- file.path(.cdtData$GalParams$output, "Aggregated_Data")
		dir.create(outputDIR, showWarnings = FALSE, recursive = TRUE)

		nc <- nc_open(ncPATH[which(ncEXIST)[1]])
		varid0 <- ncINFO$varid
		xlon0 <- nc$var[[varid0]]$dim[[ncINFO$xo]]$vals
		xlat0 <- nc$var[[varid0]]$dim[[ncINFO$yo]]$vals
		units0 <- nc$var[[varid0]]$units
		prec0 <- nc$var[[varid0]]$prec
		missval0 <- nc$var[[varid0]]$missval
		longname0 <- nc$var[[varid0]]$longname
		nc_close(nc)
		xo0 <- order(xlon0)
		xlon0 <- xlon0[xo0]
		yo0 <- order(xlat0)
		xlat0 <- xlat0[yo0]
		xnlon0 <- length(xlon0)
		xnlat0 <- length(xlat0)

		########
		outnc <- paste0(strsplit(.cdtData$GalParams$cdtnetcdf$format, "%")[[1]][1], odaty, '.nc')
		out.ncfiles <- file.path(outputDIR, outnc)

		#######
		dx <- ncdim_def("Lon", "degreeE", xlon0)
		dy <- ncdim_def("Lat", "degreeN", xlat0)
		grd.nc.out <- ncvar_def(varid0, units0, list(dx, dy), missval0, longname = longname0, prec = prec0)

		#######
		pars.aggr.series <- .cdtData$GalParams$aggr.series

		#######

		is.parallel <- doparallel(length(index) >= 20)
		`%parLoop%` <- is.parallel$dofun
		ret <- foreach(jj = seq_along(index), .packages = "ncdf4") %parLoop% {
			ix <- index[[jj]]
			nc.files <- ncPATH[ix]
			nc.exist <- ncEXIST[ix]
			nc.files <- nc.files[nc.exist]
			len.nc.files <- length(nc.files)

			if((len.nc.files == 0) | (len.nc.files / nbd.in[jj] < min.frac)){
				out <- matrix(missval0, nrow = xnlon0, ncol = xnlat0)
				nc2 <- nc_create(out.ncfiles[jj], grd.nc.out)
				ncvar_put(nc2, grd.nc.out, out)
				nc_close(nc2)
				return(NULL)
			}

			ncdon <- lapply(seq_along(nc.files), function(j){
				nc <- nc_open(nc.files[j])
				don <- ncvar_get(nc, varid = varid0)
				nc_close(nc)
				don <- if(ncINFO$xo < ncINFO$yo) don[xo0, yo0] else t(don)[xo0, yo0]
				c(don)
			})
			ncdon <- do.call(rbind, ncdon)
			miss <- (colSums(is.na(ncdon)) / nrow(ncdon)) >= min.frac

			out <- cdt.aggregate(ncdon, pars = pars.aggr.series)

			out[miss] <- missval0
			out[is.na(out) | is.nan(out) | is.infinite(out)] <- missval0
			out <- matrix(out, nrow = xnlon0, ncol = xnlat0)

			nc2 <- nc_create(out.ncfiles[jj], grd.nc.out)
			ncvar_put(nc2, grd.nc.out, out)
			nc_close(nc2)
			rm(out, ncdon); gc()
		}
		if(is.parallel$stop) stopCluster(is.parallel$cluster)
	}

	Insert.Messages.Out(.cdtData$GalParams[['message']][['13']])
	return(0)
}
