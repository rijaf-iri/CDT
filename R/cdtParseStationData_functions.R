
## use with CDT stations format with elevation or not
## CDT stations format without text or special character like (-, _, /) in dates
## daily, pentad, dekad, month, year, climatologies index,
## CDT stations format output like (onset, cessation, picsa, ....)
## output are not ordered and missing dates are not filled
## no filter for duplicated dates

splitCDTData0 <- function(donne){
	seph <- rle(grepl('[[:digit:]]', as.character(donne[, 1])))
	ipos <- which(!seph$values & seph$lengths >= 3 & seph$lengths <= 4)
	if(length(ipos) == 0 | ipos[1] != 1){
		Insert.Messages.Out('Station data is not in a standard unambiguous CDT format', format = TRUE)
		return(NULL)
	}
	pos <- seph$lengths[ipos[1]]

	dat <- list(id = as.character(donne[1, -1]),
				lon = as.numeric(donne[2, -1]),
				lat = as.numeric(donne[3, -1]),
				dates = as.character(donne[-(1:pos), 1]),
				data = as.matrix(donne[-(1:pos), -1]))
	dimnames(dat$data)[[2]] <- NULL
	dat$data <- convert_data_type(dat$data, as.numeric)
	return(dat)
}

##########################################
## use with any CDT stations format without elevation included
## all CDT stations format output
## output are not ordered and missing dates are not filled
## no filter for duplicated dates

splitCDTData1 <- function(donne){
	dat <- list(id = as.character(donne[1, -1]),
				lon = as.numeric(donne[2, -1]),
				lat = as.numeric(donne[3, -1]),
				dates = as.character(donne[-(1:3), 1]),
				data = as.matrix(donne[-(1:3), -1]))
	dimnames(dat$data)[[2]] <- NULL
	dat$data <- convert_data_type(dat$data, as.numeric)
	return(dat)
}

##########################################
## use with CDT stations format with elevation or not
## daily, pentad, dekad, month
## output are ordered and the missing dates are filled
## duplicated dates are removed
## check invalid and duplicated coordinates

splitCDTData <- function(donne, tstep){
	ideb <- nrow(donne)
	datylen <- nchar(as.character(donne[ideb, 1]))
	if(tstep == 'daily' & datylen != 8){
		Insert.Messages.Out('Station data: not a daily data', format = TRUE)
		return(NULL)
	} 
	if(tstep == 'pentad' & datylen != 7){
		Insert.Messages.Out('Station data: not a pentad data', format = TRUE)
		return(NULL)
	}
	if(tstep == 'dekadal' & datylen != 7){
		Insert.Messages.Out('Station data: not a dekadal data', format = TRUE)
		return(NULL)
	}
	if(tstep == 'monthly' & datylen != 6){
		Insert.Messages.Out('Station data: not a monthly data', format = TRUE)
		return(NULL)
	}

	seph <- rle(grepl('[[:digit:]]', as.character(donne[, 1])))
	ipos <- which(!seph$values & seph$lengths >= 3 & seph$lengths <= 4)
	if(length(ipos) == 0 | ipos[1] != 1){
		Insert.Messages.Out('Station data is not in a standard unambiguous CDT format', format = TRUE)
		return(NULL)
	}

	ihead <- 1:seph$lengths[ipos[1]]
	Info <- data.frame(t(donne[ihead, -1, drop = FALSE]))
	names(Info) <- if(length(ihead) == 4) c('Stations', 'Lon', 'Lat', 'ELV') else c('Stations', 'Lon', 'Lat')
	daty.orig <- as.character(donne[-ihead, 1])

	if(tstep%in%c('pentad', 'dekadal')){
		dek <- as.numeric(substr(daty.orig, 7, 7))
		dek <- dek[!is.na(dek)]
		if(tstep == 'pentad') wrongdaty <- any(dek == 0 | dek > 6)
		if(tstep == 'dekadal') wrongdaty <- any(dek == 0 | dek > 3)
		if(wrongdaty){
			Insert.Messages.Out('Station date is in wrong format', format = TRUE)
			return(NULL)
		}
	}

	if(tstep == 'daily'){
		dates.bak <- daty.orig
		dates <- as.Date(dates.bak, format = '%Y%m%d')
	} 
	if(tstep == 'pentad'){
		xan <- substr(daty.orig, 1, 4)
		xmo <- substr(daty.orig, 5, 6)
		xpt <- substr(daty.orig, 7, 7)
		notpen <- which(as.numeric(xpt) > 6)
		dates.bak <- paste(xan, xmo, xpt, sep = '-')
		dates <- as.Date(dates.bak)
		dates[notpen] <- NA
	}
	if(tstep == 'dekadal'){
		xan <- substr(daty.orig, 1, 4)
		xmo <- substr(daty.orig, 5, 6)
		xdk <- substr(daty.orig, 7, 7)
		notdek <- which(as.numeric(xdk) > 3)
		dates.bak <- paste(xan, xmo, xdk, sep = '-')
		dates <- as.Date(dates.bak)
		dates[notdek] <- NA
	}
	if(tstep == 'monthly'){
		xan <- substr(daty.orig, 1, 4)
		xmo <- substr(daty.orig, 5, 6)
		dates.bak <- paste(xan, xmo, '1', sep = '-')
		dates <- as.Date(dates.bak)
	}
	donne <- as.matrix(donne[-ihead, -1, drop = FALSE])
	dimnames(donne) <- NULL
	donne <- convert_data_type(donne, as.numeric)

	iNAdate <- is.na(dates)
	wrong.dates <- dates.bak[iNAdate]
	dwrong.dates <- donne[iNAdate, , drop = FALSE]
	donne <- donne[!iNAdate, , drop = FALSE]
	dates <- dates[!iNAdate]

	## duplicated dates
	idates0 <- duplicated(dates) | duplicated(dates, fromLast = TRUE)
	dup.dates <- dates[idates0]
	ddup.dates <- donne[idates0, , drop = FALSE]
	donne <- donne[!duplicated(dates), , drop = FALSE]
	dates <- dates[!duplicated(dates)]

	## fill missing dates
	if(tstep == 'daily'){
		alldates <- seq(min(dates), max(dates), 'day')
		ix <- match(alldates, dates)
		dates <- format(alldates, '%Y%m%d')
		miss.dates <- alldates[is.na(ix)]
		miss.dates <- format(miss.dates, '%Y%m%d')
		dup.dates <- format(dup.dates, '%Y%m%d')
	}
	if(tstep == 'pentad'){
		alldates <- seq(min(dates), max(dates), 'day')
		alldates <- alldates[as.numeric(format(alldates, '%d')) <= 6]
		ix <- match(alldates, dates)
		dates <- paste0(format(alldates, '%Y%m'), as.numeric(format(alldates, '%d')))
		miss.dates <- alldates[is.na(ix)]
		miss.dates <- paste0(format(miss.dates, '%Y%m'), as.numeric(format(miss.dates, '%d')))
		dup.dates <- paste0(format(dup.dates, '%Y%m'), as.numeric(format(dup.dates, '%d')))
	}
	if(tstep == 'dekadal'){
		alldates <- seq(min(dates), max(dates), 'day')
		alldates <- alldates[as.numeric(format(alldates, '%d')) <= 3]
		ix <- match(alldates, dates)
		dates <- paste0(format(alldates, '%Y%m'), as.numeric(format(alldates, '%d')))
		miss.dates <- alldates[is.na(ix)]
		miss.dates <- paste0(format(miss.dates, '%Y%m'), as.numeric(format(miss.dates, '%d')))
		dup.dates <- paste0(format(dup.dates, '%Y%m'), as.numeric(format(dup.dates, '%d')))
	}
	if(tstep == 'monthly'){
		alldates <- seq(min(dates), max(dates), 'month')
		ix <- match(alldates, dates)
		dates <- format(alldates, '%Y%m')
		miss.dates <- alldates[is.na(ix)]
		miss.dates <- format(miss.dates, '%Y%m')
		dup.dates <- format(dup.dates, '%Y%m')
	}
	donne <- donne[ix, , drop = FALSE]

	wrong.dates <- if(any(iNAdate)) list(date = wrong.dates, data = dwrong.dates) else NULL
	duplicated.dates <- if(any(idates0)) list(date = dup.dates, data = ddup.dates) else NULL
	missing.dates <- if(any(is.na(ix))) list(date = miss.dates) else NULL

	## missing & duplicated coordinates
	stn.id <- as.character(Info[, 1])
	stn.lon <- as.numeric(as.character(Info[, 2]))
	stn.lat <- as.numeric(as.character(Info[, 3]))
	stn.elv <- if(ncol(Info) == 4) as.numeric(as.character(Info[, 4])) else NULL

	iddup <- duplicated(stn.id) | duplicated(stn.id, fromLast = TRUE)
	imiss <- (is.na(stn.lon) | stn.lon < -180 | stn.lon > 360 | is.na(stn.lat) | stn.lat < -90 | stn.lat > 90)
	idup <- duplicated(Info[, 2:3, drop = FALSE]) | duplicated(Info[, 2:3, drop = FALSE], fromLast = TRUE)
	aretenir <- !imiss & !duplicated(Info[, 2:3, drop = FALSE]) & !duplicated(stn.id)

	stn.lon <- stn.lon[aretenir]
	stn.lat <- stn.lat[aretenir]
	stn.id <- stn.id[aretenir]
	stn.elv <- stn.elv[aretenir]
	donne <- donne[, aretenir, drop = FALSE]

	stn.miss <- if(any(imiss)) Info[imiss, , drop = FALSE] else NULL
	stn.dup <- NULL
	if(any(idup)){
		stn.dup <- Info[idup, , drop = FALSE]
		o <- order(apply(stn.dup[, 2:3, drop = FALSE], 1, paste, collapse = ''))
		stn.dup <- stn.dup[o, , drop = FALSE]
	}

	stn.id.dup <- NULL
	if(any(iddup)){
		stn.id.dup <- Info[iddup, , drop = FALSE]
		stn.id.dup <- stn.id.dup[order(stn.id.dup[, 1]), , drop = FALSE]
	}

	stnlist <- list(id = stn.id, lon = stn.lon, lat = stn.lat, elv = stn.elv, dates = dates, data = donne,
					wrong.dates = wrong.dates, duplicated.dates = duplicated.dates, missing.dates = missing.dates,
					duplicated.coords = stn.dup, missing.coords = stn.miss, duplicated.stnID = stn.id.dup,
					miss.coords.idx = imiss, dup.coords.idx = idup, dup.stnID.idx = iddup)
	return(stnlist)
}

##########################################

splitTsData <- function(donne, tstep, filefrmt, datefrmt){
	## get dates
	if(tstep == 'daily'){
		if(datefrmt == "1"){
			if(nchar(as.character(donne[5, 1])) != 8){
				Insert.Messages.Out('Station data: not a daily data', format = TRUE)
				return(NULL)
			}
			dates.bak <- as.character(donne[, 1])
			dates <- as.Date(dates.bak, format = '%Y%m%d')
		}else{
			dates.bak <- paste(as.character(donne[, 1]),
							   as.character(donne[, 2]),
							   as.character(donne[, 3]), sep = '-')
			dates <- as.Date(dates.bak)
		}
	}
	if(tstep == 'pentad'){
		if(datefrmt == "1"){
			#1date
			if(nchar(as.character(donne[5, 1])) != 7){
				Insert.Messages.Out('Station data: not a pentad data', format = TRUE)
				return(NULL)
			}
			xan <- substr(as.character(donne[, 1]), 1, 4)
			xmo <- substr(as.character(donne[, 1]), 5, 6)
			xdk <- substr(as.character(donne[, 1]), 7, 7)
			notpen<- which(as.numeric(xdk) > 6)
			dates.bak <- paste(xan, xmo, xdk, sep = '-')
			dates <- as.Date(dates.bak)
		}else{
			#3date
			dates.bak <- paste(as.character(donne[, 1]),
							   as.character(donne[, 2]),
							   as.character(donne[, 3]), sep = '-')
			dates <- as.Date(dates.bak)
			notpen <- which(as.numeric(as.character(donne[, 3])) > 6)
		}
		dates[notpen] <- NA
	}
	if(tstep == 'dekadal'){
		if(datefrmt == "1"){
			#1date
			if(nchar(as.character(donne[5, 1])) != 7){
				Insert.Messages.Out('Station data: not a dekadal data', format = TRUE)
				return(NULL)
			}
			xan <- substr(as.character(donne[, 1]), 1, 4)
			xmo <- substr(as.character(donne[, 1]), 5, 6)
			xdk <- substr(as.character(donne[, 1]), 7, 7)
			notdek <- which(as.numeric(xdk) > 3)
			dates.bak <- paste(xan, xmo, xdk, sep = '-')
			dates <- as.Date(dates.bak)
		}else{
			#3date
			dates.bak <- paste(as.character(donne[, 1]),
							   as.character(donne[, 2]),
							   as.character(donne[, 3]), sep = '-')
			dates <- as.Date(dates.bak)
			notdek <- which(as.numeric(as.character(donne[, 3])) > 3)
		}
		dates[notdek] <- NA
	}
	if(tstep == 'monthly'){
		if(datefrmt == "1"){
			#1date
			if(nchar(as.character(donne[5, 1])) != 6){
				Insert.Messages.Out('Station data: not a monthly data', format = TRUE)
				return(NULL)
			}
			xan <- substr(as.character(donne[, 1]), 1, 4)
			xmo <- substr(as.character(donne[, 1]), 5, 6)
			dates.bak <- paste(xan, xmo, '1', sep = '-')
			dates <- as.Date(dates.bak)
		}else{
			#3date
			dates.bak <- paste(as.character(donne[, 1]),
							   as.character(donne[, 2]), '1', sep = '-')
			dates <- as.Date(dates.bak)
		}
	}

	## get vars
	if(filefrmt == "1"){
		#1var
		if(datefrmt == "1"){
			var <- as.numeric(donne[, 2])
		}else{
			if(tstep == 'monthly'){
				if(ncol(donne) == 3) var <- as.numeric(donne[, 3])
				if(ncol(donne) == 4) var <- as.numeric(donne[, 4])
			}else{
				var <- as.numeric(donne[, 4])
			}
		}
	}else{
		#3var
		if(datefrmt == "1"){
			rr <- as.numeric(donne[, 2])
			tx <- as.numeric(donne[, 3])
			tn <- as.numeric(donne[, 4])
		}else{
			if(tstep == 'monthly'){
				if(ncol(donne) == 5){
					#rr = 3, tx = 4, tn = 5
					rr <- as.numeric(donne[, 3])
					tx <- as.numeric(donne[, 4])
					tn <- as.numeric(donne[, 5])
				}
				if(ncol(donne) == 6){
					#rr = 4, tx = 5, tn = 6
					rr <- as.numeric(donne[, 4])
					tx <- as.numeric(donne[, 5])
					tn <- as.numeric(donne[, 6])
				}
			}else{
				#rr = 4, tx = 5, tn = 6
				rr <- as.numeric(donne[, 4])
				tx <- as.numeric(donne[, 5])
				tn <- as.numeric(donne[, 6])
			}
		}
	}

	iNAdate <- is.na(dates)
	wrong.dates <- dates.bak[iNAdate]

	## remove NA dates and order
	if(filefrmt == "1"){
		dwrong.dates <- var[iNAdate]
		var <- var[!iNAdate]
		dates <- dates[!iNAdate]
	}else{
		dwrong.dates <- cbind(rr[iNAdate], tx[iNAdate], tn[iNAdate])
		rr <- rr[!iNAdate]
		tx <- tx[!iNAdate]
		tn <- tn[!iNAdate]
		dates <- dates[!iNAdate]
	}

	## duplicated dates
	idates0 <- duplicated(dates) | duplicated(dates, fromLast = TRUE)
	dup.dates <- dates[idates0]

	idates <- duplicated(dates)
	dates <- dates[!idates]
	if(filefrmt == "1"){
		ddup.dates <- var[idates0]
		var <- var[!idates]
	}else{
		ddup.dates <- cbind(rr[idates0], tx[idates0], tn[idates0])
		rr <- rr[!idates]
		tx <- tx[!idates]
		tn <- tn[!idates]
	}

	if(length(dates) == 0){
		Insert.Messages.Out('Wrong date format', format = TRUE)
		return(NULL)
	}

	if(tstep == 'daily'){
		alldates <- seq(min(dates), max(dates), 'day')
		ix <- match(alldates, dates)
		dates <- format(alldates, '%Y%m%d')
		miss.dates <- alldates[is.na(ix)]
		miss.dates <- format(miss.dates, '%Y%m%d')
		dup.dates <- format(dup.dates, '%Y%m%d')
	}
	if(tstep == 'pentad'){
		alldates <- seq(min(dates), max(dates), 'day')
		alldates <- alldates[as.numeric(format(alldates, '%d')) <= 6]
		ix <- match(alldates, dates)
		dates <- paste0(format(alldates, '%Y%m'), as.numeric(format(alldates, '%d')))
		miss.dates <- alldates[is.na(ix)]
		miss.dates <- paste0(format(miss.dates, '%Y%m'), as.numeric(format(miss.dates, '%d')))
		dup.dates <- paste0(format(dup.dates, '%Y%m'), as.numeric(format(dup.dates, '%d')))
	}
	if(tstep == 'dekadal'){
		alldates <- seq(min(dates), max(dates), 'day')
		alldates <- alldates[as.numeric(format(alldates, '%d')) <= 3]
		ix <- match(alldates, dates)
		dates <- paste0(format(alldates, '%Y%m'), as.numeric(format(alldates, '%d')))
		miss.dates <- alldates[is.na(ix)]
		miss.dates <- paste0(format(miss.dates, '%Y%m'), as.numeric(format(miss.dates, '%d')))
		dup.dates <- paste0(format(dup.dates, '%Y%m'), as.numeric(format(dup.dates, '%d')))
	}
	if(tstep == 'monthly'){
		alldates <- seq(min(dates), max(dates), 'month')
		ix <- match(alldates, dates)
		dates <- format(alldates, '%Y%m')
		miss.dates <- alldates[is.na(ix)]
		miss.dates <- format(miss.dates, '%Y%m')
		dup.dates <- format(dup.dates, '%Y%m')
	}

	if(filefrmt == "1"){
		var <- list(var = var[ix])
		nbvar <- 1
	}else{
		var <- list(rr = rr[ix], tx = tx[ix], tn = tn[ix])
		nbvar <- 3
	}

	wrong.dates <- if(any(iNAdate)) list(date = wrong.dates, data = dwrong.dates) else NULL
	duplicated.dates <-  if(any(idates0)) list(date = dup.dates, data = ddup.dates) else NULL
	missing.dates <-  if(any(is.na(ix))) list(date = miss.dates) else NULL

	ret <- list(tstep = tstep, nbvar = nbvar, var = var, dates = dates,
				wrong.dates = wrong.dates, duplicated.dates = duplicated.dates, missing.dates = missing.dates)
	return(ret)
}
