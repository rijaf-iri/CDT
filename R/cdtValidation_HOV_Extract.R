
HOV_DataExtraction <- function(GeneralParameters){
	if(GeneralParameters$outdir %in% c("", "NA")){
		Insert.Messages.Out("Directory to save results doesn't exist", format = TRUE)
		return(NULL)
	}

	outValidation <- file.path(GeneralParameters$outdir, paste0('HOVALIDATION_',
								file_path_sans_ext(GeneralParameters$STN.file)))
	dir.create(outValidation, showWarnings = FALSE, recursive = TRUE)

	freqData <- GeneralParameters$Tstep

	##################
	## Get data

	stnInfo <- getStnOpenDataInfo(GeneralParameters$STN.file)
	if(!is.null(.cdtData$EnvData$stnData)){
		if(!isTRUE(all.equal(.cdtData$EnvData$stnData$stnInfo, stnInfo))){
			readstnData <- TRUE
			.cdtData$EnvData$stnData <- NULL
		}else readstnData <- FALSE
	}else readstnData <- TRUE

	if(readstnData){
		cdtTmpVar <- getStnOpenData(GeneralParameters$STN.file)
		cdtTmpVar <- getCDTdataAndDisplayMsg(cdtTmpVar, freqData, GeneralParameters$STN.file)
		if(is.null(cdtTmpVar)) return(NULL)
		cdtTmpVar <- cdtTmpVar[c('id', 'lon', 'lat', 'dates', 'data')]
		cdtTmpVar$index <- seq_along(cdtTmpVar$dates)
		.cdtData$EnvData$stnData <- cdtTmpVar
		.cdtData$EnvData$stnData$stnInfo <- stnInfo
		rm(cdtTmpVar)
	}

	###################
	## define geometry
	if(GeneralParameters$type.select != "all"){
		if(GeneralParameters$type.select == "rect"){
			ilon <- .cdtData$EnvData$stnData$lon >= GeneralParameters$Geom$minlon &
					.cdtData$EnvData$stnData$lon <= GeneralParameters$Geom$maxlon
			ilat <- .cdtData$EnvData$stnData$lat >= GeneralParameters$Geom$minlat &
					.cdtData$EnvData$stnData$lat <= GeneralParameters$Geom$maxlat
			ixy <- ilon & ilat
		}

		if(GeneralParameters$type.select == "poly"){
			shp.dat <- getShpOpenData(GeneralParameters$shp.file$shp)[[2]]
			shp <- shp.dat[as.character(shp.dat@data[, GeneralParameters$shp.file$attr]) == GeneralParameters$Geom$namePoly, ]
			pts.dat <- data.frame(x = .cdtData$EnvData$stnData$lon, y = .cdtData$EnvData$stnData$lat)
			coordinates(pts.dat)<- ~x+y
			ixy <- unname(!is.na(over(pts.dat, geometry(shp))))
		}
		if(!any(ixy)){
			Insert.Messages.Out('The selection did not contain any stations', format = TRUE)
			return(NULL)
		}
	}else ixy <- rep(TRUE, length(.cdtData$EnvData$stnData$lon))

	##################
	## ncdf data sample file
	ncDataInfo <- getNCDFSampleData(GeneralParameters$ncdf.file$sample)
	if(is.null(ncDataInfo)){
		Insert.Messages.Out("No netcdf data sample found", format = TRUE)
		return(NULL)
	}

	##################
	## Get NCDF data info
	start.year <- GeneralParameters$Extract.Date$start.year
	start.mon <- GeneralParameters$Extract.Date$start.mon
	start.dek <- GeneralParameters$Extract.Date$start.day
	end.year <- GeneralParameters$Extract.Date$end.year
	end.mon <- GeneralParameters$Extract.Date$end.mon
	end.dek <- GeneralParameters$Extract.Date$end.day
	months <- GeneralParameters$Extract.Date$Months
	start.date <- as.Date(paste(start.year, start.mon, start.dek, sep = '/'), format = '%Y/%m/%d')
	end.date <- as.Date(paste(end.year, end.mon, end.dek, sep = '/'), format = '%Y/%m/%d')

	NCDF.DIR <- GeneralParameters$ncdf.file$dir
	NCDF.Format <- GeneralParameters$ncdf.file$format

	errmsg <- "NCDF data not found"
	ncInfo <- ncFilesInfo(freqData, start.date, end.date, months, NCDF.DIR, NCDF.Format, errmsg)
	if(is.null(ncInfo)) return(NULL)

	##################

	if(length(intersect(.cdtData$EnvData$stnData$dates, ncInfo$dates[ncInfo$exist])) == 0){
		Insert.Messages.Out("Station and netcdf dates did not overlap", format = TRUE)
		return(NULL)
	}

	##################

	dates <- ncInfo$date[ncInfo$exist]
	ncPATH <- ncInfo$nc.files[ncInfo$exist]

	ijx <- grid2pointINDEX(.cdtData$EnvData$stnData, ncDataInfo)

	##################

	ncdataInfo <- c(GeneralParameters$ncdf.file$dir, GeneralParameters$ncdf.file$format)
	bindncdfData <- FALSE
	if(!is.null(.cdtData$EnvData$ncdfData)){
		iexist <- dates %in% .cdtData$EnvData$ncdfData$dates
		if(all(iexist)){
			if(!isTRUE(all.equal(.cdtData$EnvData$ncdfData$ncdataInfo, ncdataInfo))){
				readNcdfData <- TRUE
				.cdtData$EnvData$ncdfData <- NULL
			}else readNcdfData <- FALSE
		}else{
			if(isTRUE(all.equal(.cdtData$EnvData$ncdfData$ncdataInfo, ncdataInfo))){
				bindncdfData <- TRUE
				if(any(iexist)){
					dates <- dates[!iexist]
					ncPATH <- ncPATH[!iexist]
				}
			}else .cdtData$EnvData$ncdfData <- NULL
			readNcdfData <- TRUE
		}
	}else readNcdfData <- TRUE

	if(readNcdfData){
		Insert.Messages.Out('Read and extract netcdf data ...')
		is.parallel <- doparallel(length(ncPATH) >= 180)
		`%parLoop%` <- is.parallel$dofun

		ncData <- foreach(jj = seq_along(ncPATH), .packages = "ncdf4") %parLoop% {
			nc <- nc_open(ncPATH[jj])
			vars <- ncvar_get(nc, varid = ncDataInfo$varid)
			nc_close(nc)
			vars <- transposeNCDFData(vars, ncDataInfo)
			vars[ijx]
		}
		if(is.parallel$stop) stopCluster(is.parallel$cluster)
		Insert.Messages.Out('Reading netcdf data finished')

		ncData <- do.call(rbind, ncData)

		cdtTmpVar <- NULL
		idx <- seq(length(dates))
		if(bindncdfData){
			cdtTmpVar$data <- rbind(.cdtData$EnvData$ncdfData$data, ncData)
			cdtTmpVar$dates <- c(.cdtData$EnvData$ncdfData$dates, dates)
			cdtTmpVar$index <- c(.cdtData$EnvData$ncdfData$index, max(.cdtData$EnvData$ncdfData$index) + idx)
		}else{
			cdtTmpVar$data <- ncData
			cdtTmpVar$dates <- dates
			cdtTmpVar$index <- idx
		}
		odaty <- order(cdtTmpVar$dates)
		cdtTmpVar$dates <- cdtTmpVar$dates[odaty]
		cdtTmpVar$index <- cdtTmpVar$index[odaty]

		cdtTmpVar$ncdataInfo <- ncdataInfo
		.cdtData$EnvData$ncdfData <- cdtTmpVar
		rm(cdtTmpVar, ncData, odaty, idx)
	}

	idx.stn <- match(.cdtData$EnvData$ncdfData$dates, .cdtData$EnvData$stnData$dates)
	idx.stn <- .cdtData$EnvData$stnData$index[na.omit(idx.stn)]
	idx.ncdf <- match(.cdtData$EnvData$stnData$dates, .cdtData$EnvData$ncdfData$dates)
	idx.ncdf <- .cdtData$EnvData$ncdfData$index[na.omit(idx.ncdf)]

	.cdtData$EnvData$cdtData$info$id <- .cdtData$EnvData$stnData$id[ixy]
	.cdtData$EnvData$cdtData$info$lon <- .cdtData$EnvData$stnData$lon[ixy]
	.cdtData$EnvData$cdtData$info$lat <- .cdtData$EnvData$stnData$lat[ixy]
	.cdtData$EnvData$cdtData$dates <- .cdtData$EnvData$stnData$dates[idx.stn]
	.cdtData$EnvData$cdtData$obs <- .cdtData$EnvData$stnData$data[idx.stn, ixy, drop = FALSE]
	.cdtData$EnvData$cdtData$fcst <- .cdtData$EnvData$ncdfData$data[idx.ncdf, ixy, drop = FALSE]
	.cdtData$EnvData$GeneralParameters <- GeneralParameters

	xhead <- cbind(c("STN", "LON", "DATE/LAT"), rbind(.cdtData$EnvData$cdtData$info$id,
					.cdtData$EnvData$cdtData$info$lon, .cdtData$EnvData$cdtData$info$lat))
	obs2file <- rbind(xhead, cbind(.cdtData$EnvData$cdtData$dates, .cdtData$EnvData$cdtData$obs))
	fcst2file <- rbind(xhead, cbind(.cdtData$EnvData$cdtData$dates, .cdtData$EnvData$cdtData$fcst))
	obs2file[is.na(obs2file)] <- -99
	fcst2file[is.na(fcst2file)] <- -99

	dirCDTdata <- file.path(outValidation, "OBS_GRD_DATA")
	dir.create(dirCDTdata, showWarnings = FALSE, recursive = TRUE)
	writeFiles(obs2file, file.path(dirCDTdata, "Observations.csv"))
	writeFiles(fcst2file, file.path(dirCDTdata, "Gridded_at_Obs_Loc.csv"))

	fileValidOut <- file.path(outValidation, "VALIDATION_DATA_OUT.rds")

	hovd.data <- .cdtData$EnvData[c('GeneralParameters', 'cdtData', 'stnData', 'ncdfData')]
	saveRDS(hovd.data, file = fileValidOut)

	Insert.Messages.Out('Data extraction finished')
	return(0)
}
