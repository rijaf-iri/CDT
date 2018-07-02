
download_RFE_day <- function(){
	deb <- try(as.Date(.cdtData$GalParams$istart), silent = TRUE)
	if(inherits(deb, "try-error")| is.na(deb)){
		Insert.Messages.Out(.cdtData$GalParams[['message']][['7']], format = TRUE)
		return(NULL)
	}

	fin <- try(as.Date(.cdtData$GalParams$iend), silent = TRUE)
	if(inherits(fin, "try-error")| is.na(fin)){
		Insert.Messages.Out(.cdtData$GalParams[['message']][['8']], format = TRUE)
		return(NULL)
	}

	dates <- format(seq(deb, fin, 'day'), '%Y-%m-%d')
	do.call(rbind, strsplit(dates, "-"))
}

download_RFE_dekad <- function(){
	deb <- try(as.Date(.cdtData$GalParams$istart), silent = TRUE)
	if(inherits(deb, "try-error")| is.na(deb)){
		Insert.Messages.Out(.cdtData$GalParams[['message']][['7']], format = TRUE)
		return(NULL)
	}

	year1 <- format(deb, "%Y")
	mon1 <- format(deb, "%m")
	dek1 <- as.numeric(format(deb, "%d"))
	if(dek1 > 3){
		Insert.Messages.Out(.cdtData$GalParams[['message']][['9']], format = TRUE)
		return(NULL)
	}

	fin <- try(as.Date(.cdtData$GalParams$iend), silent = TRUE)
	if(inherits(fin, "try-error")| is.na(fin)){
		Insert.Messages.Out(.cdtData$GalParams[['message']][['8']], format = TRUE)
		return(NULL)
	}

	year2 <- format(fin, "%Y")
	mon2 <- format(fin, "%m")
	dek2 <- as.numeric(format(fin, "%d"))
	if(dek2 > 3){
		Insert.Messages.Out(.cdtData$GalParams[['message']][['9']], format = TRUE)
		return(NULL)
	}

	dates <- seq(as.Date(paste(year1, mon1, dek1, sep = '/')), as.Date(paste(year2, mon2, dek2, sep = '/')), 'day')
	dates <- format(dates[as.numeric(format(dates, '%d')) <= 3], '%Y-%m-%d')
	do.call(rbind, strsplit(dates, "-"))
}

download_RFE_createURL <- function(){
	mois <- c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec")
	if(.cdtData$GalParams$rfe.data %in% .cdtData$GalParams$dekadRFE){
		dates <- download_RFE_dekad()
		if(is.null(dates)) return(NULL)
		dates <- dates[, c(1:3, 2:3), drop = FALSE]
		dates[, 4] <- mois[as.numeric(dates[, 2])]
		dates[, 5] <- ifelse(dates[, 5] == '01', '1-10', ifelse(dates[, 5] == '02', '11-20', '03'))
		if(length(which(dates[, 5] == '03')) > 0){
			endmois <- apply(matrix(dates[dates[, 5] == '03', 1:2], ncol = 2), 1,
						function(x) rev((28:31)[which(!is.na(as.Date(paste(x[1], x[2], 28:31, sep = '-'))))])[1])
			dates[dates[, 5] == '03', 5] <- paste(21, endmois, sep = '-')
		}
		tstep <- "dekadal"
	}else if(.cdtData$GalParams$rfe.data %in% .cdtData$GalParams$dailyRFE){
		dates <- download_RFE_day()
		if(is.null(dates)) return(NULL)
		dates <- dates[, c(1:3, 2), drop = FALSE]
		dates[, 4] <- mois[as.numeric(dates[, 2])]
		tstep <- "daily"
	}else return(NULL)

	datasrc <- switch(.cdtData$GalParams$rfe.data,
			'10-DAYS TAMSATv3' = { list(
						url = if(.cdtData$GalParams$IRI.DL)
								'http://iridl.ldeo.columbia.edu/SOURCES/.Reading/.Meteorology/.TAMSAT/.TARCAT/.v3p0/.dekadal/.rfe'
							else
								'https://www.tamsat.org.uk/public_data/TAMSAT3',
						fileout = paste0('rfe', dates[, 1], '_', dates[, 2], '-dk', as.numeric(dates[, 3]), '.v3.nc'),
						dim = c('lon', 'lat', 'T'),
						name = c('TAMSATv3', 'Dekadal')
					)
				},
			'10-DAYS TAMSATv2' = { list(
						url = if(.cdtData$GalParams$IRI.DL)
								'http://iridl.ldeo.columbia.edu/SOURCES/.Reading/.Meteorology/.TAMSAT/.TARCAT/.v2p0/.dekadal/.rfe'
							else
								'https://www.tamsat.org.uk/public_data',
						fileout = paste0('rfe', dates[, 1], '_', dates[, 2], '-dk', as.numeric(dates[, 3]), '.nc'),
						dim = c('lon', 'lat', 'T'),
						name = c('TAMSATv2', 'Dekadal')
					)
				},
			'DAILY TAMSATv3' = { list(
						url = if(.cdtData$GalParams$IRI.DL)
								'http://iridl.ldeo.columbia.edu/SOURCES/.Reading/.Meteorology/.TAMSAT/.TARCAT/.v3p0/.daily/.rfe'
							else
								'https://www.tamsat.org.uk/public_data/TAMSAT3',
						fileout = paste0('rfe', dates[, 1], '_', dates[, 2], '_', dates[, 3], '.v3.nc'),
						dim = c('lon', 'lat', 'time'),
						name = c('TAMSATv3', 'Daily')
					)
				},
			'DAILY TAMSATv2' = { list(
						url = if(.cdtData$GalParams$IRI.DL)
								'http://iridl.ldeo.columbia.edu/SOURCES/.Reading/.Meteorology/.TAMSAT/.TARCAT/.v2p0/.daily/.rfe'
							else
								'https://www.tamsat.org.uk/public_data',
						fileout = paste0('rfe', dates[, 1], '_', dates[, 2], '_', dates[, 3], '.nc'),
						dim = c('lon', 'lat', 'time'),
						name = c('TAMSATv2', 'Daily')
					)
				},
			'10-DAYS CHIRPv1.0' = { list(
						url = 'http://iridl.ldeo.columbia.edu/SOURCES/.UCSB/.CHIRP/.v1p0/.dekad/.prcp',
						fileout = paste0('chirpV1.0_', dates[, 1], dates[, 2], as.numeric(dates[, 3]), '.nc'),
						dim = c('X', 'Y', 'T'),
						name = c('CHIRPv1.0', 'Dekadal')
					)
				},
			'10-DAYS CHIRPSv2.0' = { list(
						url = 'http://iridl.ldeo.columbia.edu/SOURCES/.UCSB/.CHIRPS/.v2p0/.dekad/.prcp',
						fileout = paste0('chirpsV2.0_', dates[, 1], dates[, 2], as.numeric(dates[, 3]), '.nc'),
						dim = c('X', 'Y', 'T'),
						name = c('CHIRPSv2.0', 'Dekadal')
					)
				},
			'DAILY CHIRPSv2.0' = { list(
						url = 'http://iridl.ldeo.columbia.edu/SOURCES/.UCSB/.CHIRPS/.v2p0/.daily-improved/.global/.0p05/.prcp',
						fileout = paste0('chirpsV2.0_', dates[, 1], dates[, 2], dates[, 3], '.nc'),
						dim = c('X', 'Y', 'T'),
						name = c('CHIRPSv2.0', 'Daily')
					)
				}
		)

	if(.cdtData$GalParams$IRI.DL){
		if(tstep == "dekadal")
			time <- paste0(datasrc$dim[3], '/%28', dates[, 5], '%20', dates[, 4], '%20', dates[, 1], '%29/VALUE')
		if(tstep == "daily")
			time <- paste0(datasrc$dim[3], '/%28', as.numeric(dates[, 3]), '%20', dates[, 4], '%20', dates[, 1], '%29/VALUE')

		area <- paste(datasrc$dim[1], .cdtData$GalParams$bbox$minlon, .cdtData$GalParams$bbox$maxlon, 'RANGE',
					datasrc$dim[2], .cdtData$GalParams$bbox$minlat, .cdtData$GalParams$bbox$maxlat, 'RANGE', sep = '/')

		link <- paste(datasrc$url, area, time, 'data.nc', sep = '/')
	}

	TAMSAT <- c('10-DAYS TAMSATv3', '10-DAYS TAMSATv2', 'DAILY TAMSATv3', 'DAILY TAMSATv2')
	.cdtData$GalParams$TAMSAT.src <- !.cdtData$GalParams$IRI.DL & .cdtData$GalParams$rfe.data %in% TAMSAT
	if(.cdtData$GalParams$TAMSAT.src) link <- paste(datasrc$url, dates[, 1], dates[, 2], datasrc$fileout, sep = '/')
	## .cdtData$GalParams$CHIRP.src

	return(list(link = link, files = datasrc$fileout, name = datasrc$name))
}

ExecDownload_RFEData <- function(){
	on.exit({
		tkconfigure(.cdtEnv$tcl$main$win, cursor = '')
		tcl('update')
	})
	tkconfigure(.cdtEnv$tcl$main$win, cursor = 'watch')
	tcl('update')

	datasrc <- download_RFE_createURL()
	if(is.null(datasrc)){
		Insert.Messages.Out(.cdtData$GalParams[['message']][['5']], format = TRUE)
		return(NULL)
	}

	outdir_extr <- file.path(.cdtData$GalParams$dir2save, paste0(datasrc$name[2], '_', datasrc$name[1], '_Extracted'))
	if(!file.exists(outdir_extr)) dir.create(outdir_extr, showWarnings = FALSE, recursive = TRUE)
	destfile <- file.path(outdir_extr, datasrc$files)

	if(.cdtData$GalParams$TAMSAT.src){
		outdir_raw <- file.path(.cdtData$GalParams$dir2save, paste0(datasrc$name[2], '_', datasrc$name[1], '_Africa'))
		if(!file.exists(outdir_raw)) dir.create(outdir_raw, showWarnings = FALSE, recursive = TRUE)
		destfile_ext <- destfile
		destfile <- file.path(outdir_raw, datasrc$files)
	}

	bboxx <- do.call(paste, c(.cdtData$GalParams$bbox[c("minlon", "minlat", "maxlon", "maxlat")], sep = ':'))

	tcl("update", "idletasks")
	for(j in seq_along(datasrc$link)){
		outRet <- 0
		link.exist <- RCurl::url.exists(datasrc$link[j])
		if(!link.exist){
			Insert.Messages.Out(paste(.cdtData$GalParams[['message']][['10']], datasrc$files[j]), format = TRUE)
			outRet <- -1
			next
		}

		ret <- try(download.file(datasrc$link[j], destfile[j], mode = "wb", quiet = TRUE), silent = TRUE)
		if(ret != 0){
			Insert.Messages.Out(paste(.cdtData$GalParams[['message']][['5']], datasrc$files[j]), format = TRUE)
			outRet <- -1
			next
		}

		if(.cdtData$GalParams$TAMSAT.src){
			if(.cdtData$GalParams$rfe.data %in% c('DAILY TAMSATv2', '10-DAYS TAMSATv2')){
				dim.xo <- 2
				dim.yo <- 1
			}
			if(.cdtData$GalParams$rfe.data %in% c('DAILY TAMSATv3', '10-DAYS TAMSATv3')){
				dim.xo <- 1
				dim.yo <- 2
			}

			nc <- nc_open(destfile[j])
			xm <- nc$dim[[dim.xo]]$vals
			ym <- nc$dim[[dim.yo]]$vals
			xdat <- ncvar_get(nc, varid = nc$var[[1]]$name)
			nc_close(nc)
			xo <- order(xm)
			xm <- xm[xo]
			yo <- order(ym)
			ym <- ym[yo]
			xdat <- xdat[xo, yo]
			idx <- which(xm >= .cdtData$GalParams$bbox$minlon & xm <= .cdtData$GalParams$bbox$maxlon)
			idy <- which(ym >= .cdtData$GalParams$bbox$minlat & ym <= .cdtData$GalParams$bbox$maxlat)
			xm <- xm[idx]
			ym <- ym[idy]
			xdat <- xdat[idx, idy]
			xdat[is.na(xdat)] <- -99
			dx <- ncdim_def("Lon", "degreeE", xm)
			dy <- ncdim_def("Lat", "degreeN", ym)

			if(.cdtData$GalParams$rfe.data %in% c('10-DAYS TAMSATv3', '10-DAYS TAMSATv2'))
				rfeout <- ncvar_def('rfe', "mm", list(dx, dy), -99, longname = "TAMSAT 10-days rainfall estimate",
									prec = "short", shuffle = TRUE, compression = 9)
			if(.cdtData$GalParams$rfe.data %in% c('DAILY TAMSATv3', 'DAILY TAMSATv2'))
				rfeout <- ncvar_def('rfe', "mm", list(dx, dy), -99, longname = "TAMSAT daily rainfall estimate",
									prec = "short", shuffle = TRUE, compression = 9)

			nc2 <- nc_create(destfile_ext[j], rfeout)
			ncvar_put(nc2, rfeout, xdat)
			nc_close(nc2)
		}
		# if(.cdtData$GalParams$CHIRP.src)

		Insert.Messages.Out(paste('Extraction :', datasrc$files[j], 'over', paste0('bbox:', bboxx), 'done!'))
		tcl("update")
	}

	if(outRet == -1) Insert.Messages.Out(.cdtData$GalParams[['message']][['6']], format = TRUE)
	Insert.Messages.Out(.cdtData$GalParams[['message']][['4']])

	return(0)
}

