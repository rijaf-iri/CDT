
ExtractDataProcs <- function(GeneralParameters){
	tstep.In <- GeneralParameters$in.series
	tstep.Out <- GeneralParameters$out.series$out.series

	start.year <- GeneralParameters$date.range$start.year
	start.mon <- GeneralParameters$date.range$start.mon
	start.day <- GeneralParameters$date.range$start.day
	start.hour <- GeneralParameters$date.range$start.hour

	end.year <- GeneralParameters$date.range$end.year
	end.mon <- GeneralParameters$date.range$end.mon
	end.day <- GeneralParameters$date.range$end.day
	end.hour <- GeneralParameters$date.range$end.hour

	startMonth <- GeneralParameters$date.range$start.month
	endMonth <- GeneralParameters$date.range$end.month

	xminLon <- GeneralParameters$Geom$minlon
	xmaxLon <- GeneralParameters$Geom$maxlon
	xminLat <- GeneralParameters$Geom$minlat
	xmaxLat <- GeneralParameters$Geom$maxlat
	xpadLon <- GeneralParameters$Geom$padlon
	xpadLat <- GeneralParameters$Geom$padlat

	####
	outputDIR <- GeneralParameters$out.data$outdir
	if(outputDIR == "" | outputDIR == "NA" | is.na(outputDIR)){
		msg <- if(GeneralParameters$out.data$format == 'ncdf') "No directory to save the extracted data" else "No File to save the extracted data"
		Insert.Messages.Out(msg, format = TRUE)
		return(NULL)
	}

	if(tstep.In == "hourly"){
		if(any(sapply(GeneralParameters$date.range, is.na)))
		{
			Insert.Messages.Out("Invalid date for time series extraction", format = TRUE)
			return(NULL)
		}
	}else if(tstep.In %in% c("daily", "pentad", "dekadal")){
		if(any(sapply(c(start.year, start.mon, start.day, end.year, end.mon, end.day), is.na)))
		{
			Insert.Messages.Out("Invalid date for time series extraction", format = TRUE)
			return(NULL)
		}
	}else{
		if(any(sapply(c(start.year, start.mon, end.year, end.mon), is.na)))
		{
			Insert.Messages.Out("Invalid date for time series extraction", format = TRUE)
			return(NULL)
		}
	}

	if(tstep.In == "pentad")
	{
		if(GeneralParameters$date.range$start.day > 6 | GeneralParameters$date.range$end.day > 6)
		{
			Insert.Messages.Out("Invalid pentad date", format = TRUE)
			return(NULL)
		}
	}

	if(tstep.In == "dekadal")
	{
		if(GeneralParameters$date.range$start.day > 3 | GeneralParameters$date.range$end.day > 3)
		{
			Insert.Messages.Out("Invalid dekad date", format = TRUE)
			return(NULL)
		}
	}

	####
	if(GeneralParameters$type.series != 'rawts')
	{
		if(!GeneralParameters$climato$all.years)
		{
			if(is.na(GeneralParameters$climato$start.year) | is.na(GeneralParameters$climato$end.year))
			{
				Insert.Messages.Out("Invalid start and end of years for climatology calculation", format = TRUE)
				return(NULL)
			}
		}
		if(is.na(GeneralParameters$climato$min.year))
		{
			Insert.Messages.Out("Invalid minimum number of year for climatology calculation", format = TRUE)
			return(NULL)
		}
		if(tstep.In == "daily" & is.na(GeneralParameters$climato$winsize))
		{
			Insert.Messages.Out("Invalid window size for climatology calculation", format = TRUE)
			return(NULL)
		}
	}

	####
	if(GeneralParameters$type.extract == 'point')
	{
		if(is.na(xminLon) | is.na(xminLat))
		{
			Insert.Messages.Out("Invalid coordinates to extract", format = TRUE)
			return(NULL)
		}
	}

	if(GeneralParameters$type.extract == 'rect')
	{
		if(is.na(xminLon) | is.na(xmaxLon) | is.na(xminLat) | is.na(xmaxLat))
		{
			Insert.Messages.Out("Invalid coordinates for the extraction", format = TRUE)
			return(NULL)
		}
	}

	if(GeneralParameters$type.extract %in% c('mpoint', 'mpoly'))
	{
		if(is.null(GeneralParameters$Geom$multiObj))
		{
			if(GeneralParameters$type.extract == 'mpoint')
				Insert.Messages.Out("No selected points", format = TRUE)
			if(GeneralParameters$type.extract == 'mpoly')
				Insert.Messages.Out("No selected polygons", format = TRUE)
			return(NULL)
		}

		multiptspoly <- gsub("[\r]", "", GeneralParameters$Geom$multiObj)
		multiptspoly <- str_trim(strsplit(multiptspoly, "[\n]")[[1]])
		multiptspoly <- multiptspoly[multiptspoly != ""]
		if(length(multiptspoly) == 0)
		{
			Insert.Messages.Out("No coordinates  or polygons found", format = TRUE)
			return(NULL)
		}
	}

	if(GeneralParameters$type.extract %in% c('poly', 'mpoly'))
	{
		shpf <- getShpOpenData(GeneralParameters$shp.file$shp)[[2]]
		if(!is.null(shpf))
		{
			shpf.union <- maptools::unionSpatialPolygons(shpf, as.character(shpf@data[, GeneralParameters$shp.file$attr]))
			shpf.df <- aggregate(as(shpf, "data.frame")[, 1], list(as.character(shpf@data[, GeneralParameters$shp.file$attr])), identity)
			shpf.df$x <- seq(shpf.union)
			row.names(shpf.df) <- sapply(slot(shpf.union, "polygons"), function(x) slot(x, "ID"))
			shpf.union <- SpatialPolygonsDataFrame(shpf.union, shpf.df)
		}else{
			Insert.Messages.Out("No polygons found", format = TRUE)
			return(NULL)
		}
	}

	####
	if(tstep.In == 'hourly')
	{
		daty1 <- try(strptime(paste(start.year, start.mon, start.day, start.hour, sep = '-'), "%Y-%m-%d-%H"), silent = TRUE)
		daty2 <- try(strptime(paste(end.year, end.mon, end.day, end.hour, sep = '-'), "%Y-%m-%d-%H"), silent = TRUE)
	}

	if(tstep.In %in% c('daily', 'pentad', 'dekadal'))
	{
		daty1 <- try(as.Date(paste(start.year, start.mon, start.day, sep = '-')), silent = TRUE)
		daty2 <- try(as.Date(paste(end.year, end.mon, end.day, sep = '-')), silent = TRUE)
	}

	if(tstep.In == 'monthly')
	{
		daty1 <- try(as.Date(paste(start.year, start.mon, '01', sep = '-')), silent = TRUE)
		daty2 <- try(as.Date(paste(end.year, end.mon, '01', sep = '-')), silent = TRUE)
	}

	if(inherits(daty1, "try-error") | inherits(daty2, "try-error"))
	{
		Insert.Messages.Out("Invalid date for time series extraction", format = TRUE)
		return(NULL)
	}

	####
	if(tstep.In == 'hourly')
	{
		dates <- format(seq(daty1, daty2, 'hour'), '%Y%m%d%H')
		ncfiles <- sprintf(GeneralParameters$ncdf.file$format, substr(dates, 1, 4), substr(dates, 5, 6),
															substr(dates, 7, 8), substr(dates, 9, 10))
	}

	if(tstep.In == 'daily')
	{
		dates <- format(seq(daty1, daty2, 'day'), '%Y%m%d')
		ncfiles <- sprintf(GeneralParameters$ncdf.file$format, substr(dates, 1, 4), substr(dates, 5, 6), substr(dates, 7, 8))
	}

	if(tstep.In == 'pentad')
	{
		dates <- seq(daty1, daty2, 'day')
		dates <- paste0(format(dates[which(as.numeric(format(dates, '%d')) <= 6)], '%Y%m'),
					as.numeric(format(dates[which(as.numeric(format(dates, '%d')) <= 6)], '%d')))
		ncfiles <- sprintf(GeneralParameters$ncdf.file$format, substr(dates, 1, 4), substr(dates, 5, 6), substr(dates, 7, 7))
	}

	if(tstep.In == 'dekadal')
	{
		dates <- seq(daty1, daty2, 'day')
		dates <- paste0(format(dates[which(as.numeric(format(dates, '%d')) <= 3)], '%Y%m'),
					as.numeric(format(dates[which(as.numeric(format(dates, '%d')) <= 3)], '%d')))
		ncfiles <- sprintf(GeneralParameters$ncdf.file$format, substr(dates, 1, 4), substr(dates, 5, 6), substr(dates, 7, 7))
	}

	if(tstep.In == 'monthly')
	{
		dates <- format(seq(daty1, daty2, 'month'), '%Y%m')
		ncfiles <- sprintf(GeneralParameters$ncdf.file$format, substr(dates, 1, 4), substr(dates, 5, 6))
	}

	#####################################
	seasonLength <- (endMonth - startMonth + 1) %% 12
	seasonLength[seasonLength == 0] <- 12
	monthtoExtr <- (startMonth:(startMonth + (seasonLength - 1))) %% 12
	monthtoExtr[monthtoExtr == 0] <- 12

	imois <- as.numeric(substr(dates, 5, 6)) %in% monthtoExtr
	dates <- dates[imois]
	ncfiles <- ncfiles[imois]

	ncpath <- file.path(GeneralParameters$ncdf.file$dir, ncfiles)
	existFl <- unlist(lapply(ncpath, file.exists))
	if(!any(existFl)){
		Insert.Messages.Out("Unable to locate netcdf files", format = TRUE)
		return(NULL)
	}

	dates <- dates[existFl]
	ncpath <- ncpath[existFl]

	####
	ncInfo <- getNCDFSampleData(GeneralParameters$ncdf.file$sample)
	if(is.null(ncInfo)){
		Insert.Messages.Out("No netcdf data sample file found", format = TRUE)
		return(NULL)
	}

	#####################################
	nc <- nc_open(ncpath[1])
	nc.name <- ncInfo$varid
	lon <- nc$var[[nc.name]]$dim[[ncInfo$ilon]]$vals
	lat <- nc$var[[nc.name]]$dim[[ncInfo$ilat]]$vals
	nc.longname <- nc$var[[nc.name]]$longname
	nc.units <- nc$var[[nc.name]]$units
	nc.prec <- nc$var[[nc.name]]$prec
	nc_close(nc)

	xo <- order(lon)
	lon <- lon[xo]
	yo <- order(lat)
	lat <- lat[yo]

	spxycrd <- expand.grid(x = lon, y = lat)
	coordinates(spxycrd) <- ~x+y
	spxycrd <- SpatialPixels(points = spxycrd,
							tolerance = sqrt(sqrt(.Machine$double.eps)),
							proj4string = CRS(as.character(NA)))

	#####################################
	## Extraction Geometry

	parsextr <- list(xminLon, xminLat, xmaxLon, xmaxLat, xpadLon, xpadLat,
					GeneralParameters$Geom$namePoly, GeneralParameters$Geom$multiObj,
					GeneralParameters$shp.file$shp, GeneralParameters$shp.file$attr,
					GeneralParameters$type.extract)
	extractGeom <- TRUE
	if(!is.null(.cdtData$EnvData$procs$parsextr))
		extractGeom <- if(!isTRUE(all.equal(.cdtData$EnvData$procs$parsextr, parsextr))) TRUE else FALSE

	if(extractGeom)
	{
		Insert.Messages.Out('Define extraction geometry ...')

		if(GeneralParameters$type.extract == 'point')
		{
			if(is.na(xpadLon)) xpadLon <- 0
			if(is.na(xpadLat)) xpadLat <- 0

			headinfo <- cbind('Point', xminLon, xminLat)
			nxy <- spxycrd@grid@cellsize
			padx <- round(xpadLon/nxy[1])
			pady <- round(xpadLat/nxy[2])
			voisin <- expand.grid(x = xminLon + nxy[1] * (-padx:padx), y = xminLat + nxy[2] * (-pady:pady))
			coordinates(voisin) <- ~x+y
			if(length(voisin) > 1)
				voisin <- SpatialPixels(points = voisin, tolerance = sqrt(sqrt(.Machine$double.eps)))
			ij2xtr <- over(voisin, spxycrd)
			if(!any(!is.na(ij2xtr))){
				Insert.Messages.Out("No data to extract: Object outside data range", format = TRUE)
				return(NULL)
			}
		}

		if(GeneralParameters$type.extract == 'mpoint')
		{
			if(is.na(xpadLon)){
				Insert.Messages.Out("Pad lon is missing, no padding will be applied belong the longitude", format = TRUE)
				xpadLon <- 0
			}
			if(is.na(xpadLat)){
				Insert.Messages.Out("Pad lat is missing, no padding will be applied belong the latitude", format = TRUE)
				xpadLat <- 0
			}

			multiptspoly <- t(sapply(multiptspoly, function(x) strsplit(x, " ")[[1]]))
			multiptspoly <- data.frame(multiptspoly, stringsAsFactors = FALSE)
			rownames(multiptspoly) <- NULL
			names(multiptspoly) <- c('id', 'x', 'y')
			multiptspoly[, 2:3] <- apply(multiptspoly[, 2:3, drop = FALSE], 2, as.numeric)
			headinfo <- multiptspoly

			nxy <- spxycrd@grid@cellsize
			padx <- round(xpadLon/nxy[1])
			pady <- round(xpadLat/nxy[2])
			voisin <- lapply(seq(nrow(multiptspoly)), function(j){
							xy <- expand.grid(x = multiptspoly[j, 'x'] + nxy[1] * (-padx:padx),
											y = multiptspoly[j, 'y'] + nxy[2] * (-pady:pady))
							coordinates(xy) <- ~x+y
							if(length(xy) > 1)
								xy <- SpatialPixels(points = xy, tolerance = sqrt(sqrt(.Machine$double.eps)))
							return(xy)
						})
			ij2xtr <- lapply(voisin, over, y = spxycrd)
			if(all(sapply(ij2xtr, function(x) !any(!is.na(x))))){
				Insert.Messages.Out("No data to extract: Object outside data range", format = TRUE)
				return(NULL)
			}
		}

		if(GeneralParameters$type.extract == 'rect'){
			rectPoly <- Polygon(cbind(c(xminLon, xmaxLon, xmaxLon, xminLon, xminLon),
									c(xminLat, xminLat, xmaxLat, xmaxLat, xminLat)))
			rectPoly <- Polygons(list(rectPoly), "p1")
			rectPoly <- SpatialPolygons(list(rectPoly), 1:1)
			headinfo <- NULL

			polyRas <- spxycrd
			polyRas$z <- seq_along(spxycrd)
			polyRas <- raster(polyRas)
			ij2xtr <- extract(polyRas, rectPoly, weights = TRUE, normalizeWeights = TRUE, cellnumbers = TRUE)
			if(is.null(ij2xtr[[1]])){
				Insert.Messages.Out("No data to extract: Object outside data range", format = TRUE)
				return(NULL)
			}
		}

		if(GeneralParameters$type.extract == 'poly'){
			shpf.regOI <- shpf.union[str_trim(shpf.union@data$Group.1) == GeneralParameters$Geom$namePoly, ]
			headinfo <- NULL

			polyRas <- spxycrd
			polyRas$z <- seq_along(spxycrd)
			polyRas <- raster(polyRas)
			ij2xtr <- extract(polyRas, shpf.regOI, weights = TRUE, normalizeWeights = TRUE, cellnumbers = TRUE)
			if(is.null(ij2xtr[[1]])){
				Insert.Messages.Out("No data to extract: Object outside data range", format = TRUE)
				return(NULL)
			}
		}

		if(GeneralParameters$type.extract == 'mpoly'){
			shpf.regOI <- shpf.union[str_trim(shpf.union@data$Group.1) %in% multiptspoly, ]
			headinfo <- cbind(as.character(shpf.regOI@data$Group.1), round(coordinates(shpf.regOI), 5))
			headinfo[, 1] <- substr(str_replace_all(headinfo[, 1], "[^[:alnum:]]", ""), 1, 15)

			polyRas <- spxycrd
			polyRas$z <- seq_along(spxycrd)
			polyRas <- raster(polyRas)
			ij2xtr <- extract(polyRas, shpf.regOI, weights = TRUE, normalizeWeights = TRUE, cellnumbers = TRUE)
			if(all(sapply(ij2xtr, is.null))){
				Insert.Messages.Out("No data to extract: Object outside data range", format = TRUE)
				return(NULL)
			}
		}

		.cdtData$EnvData$procs$parsextr <- parsextr
		.cdtData$EnvData$procs$ij2xtr <- ij2xtr
		.cdtData$EnvData$procs$headinfo <- headinfo
		Insert.Messages.Out('Extraction geometry definition done!')
	}else{
		ij2xtr <- .cdtData$EnvData$procs$ij2xtr
		headinfo <- .cdtData$EnvData$procs$headinfo
	}

	#####################################

	if(GeneralParameters$type.extract == 'point')
		ij2xtr <- ij2xtr[!is.na(ij2xtr)]

	if(GeneralParameters$type.extract == 'mpoint')
	{
		ij2xtr <- lapply(ij2xtr, function(x) x[!is.na(x)])
		nonZero <- sapply(ij2xtr, length) > 0
		ij2xtr <- ij2xtr[nonZero]
	}

	if(GeneralParameters$type.extract == 'rect')
	{
		ij2xtr <- lapply(ij2xtr, function(x) x[order(x[, "value"]), , drop = FALSE])
		headinfo <- lapply(ij2xtr, function(x) reshapeXYZ2Matrix(cbind(spxycrd@coords[x[, 'value'], , drop = FALSE], seq(nrow(x)))))
		headinfo <- headinfo[[1]]
		if(GeneralParameters$out.data$sp.avrg)
			headinfo <- cbind('Rectangle', mean(headinfo$x), mean(headinfo$y))
	}

	if(GeneralParameters$type.extract == 'poly')
	{
		ij2xtr <- lapply(ij2xtr, function(x) x[order(x[, "value"]), , drop = FALSE])
		headinfo <- lapply(ij2xtr, function(x) reshapeXYZ2Matrix(cbind(spxycrd@coords[x[, 'value'], , drop = FALSE], seq(nrow(x)))))
		headinfo <- headinfo[[1]]
		if(GeneralParameters$out.data$sp.avrg){
			namepoly <- substr(str_replace_all(GeneralParameters$Geom$namePoly, "[^[:alnum:]]", ""), 1, 15)
			headinfo <- cbind(namepoly, coordinates(shpf.regOI))
		}
	}

	if(GeneralParameters$type.extract == 'mpoly')
	{
		nonNull <- !sapply(ij2xtr, is.null)
		ij2xtr <- ij2xtr[nonNull]
		ij2xtr <- lapply(ij2xtr, function(x) x[order(x[, "value"]), , drop = FALSE])
	}

	#####################################
	## read ncdf data

	infoFiles <- c(GeneralParameters$ncdf.file$dir, GeneralParameters$ncdf.file$format)
	bindData <- FALSE
	readNCDFdata <- TRUE
	if(!is.null(.cdtData$EnvData$procs$cdtdata))
	{
		if(isTRUE(all.equal(.cdtData$EnvData$procs$ij2xtr1, ij2xtr)))
		{
			iexist <- dates %in% .cdtData$EnvData$procs$cdtdata$dates
			if(all(iexist)){
				if(!isTRUE(all.equal(.cdtData$EnvData$procs$infoFiles, infoFiles))){
					readNCDFdata <- TRUE
					.cdtData$EnvData$procs$cdtdata <- NULL
				}else readNCDFdata <- FALSE
			}else{
				if(isTRUE(all.equal(.cdtData$EnvData$procs$infoFiles, infoFiles))){
					bindData <- TRUE
					if(any(iexist)){
						dates <- dates[!iexist]
						ncpath <- ncpath[!iexist]
					}
				}else .cdtData$EnvData$procs$cdtdata <- NULL
				readNCDFdata <- TRUE
			}
		}else{
			readNCDFdata <- TRUE
			.cdtData$EnvData$procs$cdtdata <- NULL
		}
	}

	if(readNCDFdata){
		Insert.Messages.Out('Read and extract netcdf data ...')

		is.parallel <- doparallel(length(ncpath) >= 180)
		`%parLoop%` <- is.parallel$dofun
		ncData <- foreach(jj = seq_along(ncpath), .packages = "ncdf4") %parLoop% {
			nc <- nc_open(ncpath[jj])
			vars <- ncvar_get(nc, varid = ncInfo$varid)
			nc_close(nc)
			vars <- if(ncInfo$ilon < ncInfo$ilat) vars[xo, yo] else t(vars)[xo, yo]
			vars <- round(c(vars), 1)

			if(GeneralParameters$type.extract == 'point')
			{
				DATAext <- vars[ij2xtr]
				if(length(ij2xtr) > 1)
					DATAext <- mean(DATAext, na.rm = TRUE)
			}

			if(GeneralParameters$type.extract == 'mpoint')
			{
				DATAext <- sapply(ij2xtr, function(ij){
					MAT <- vars[ij]
					if(length(ij) > 1)
						MAT <- mean(MAT, na.rm = TRUE)
					MAT
				})
				if(!all(nonZero)){
					DATAtmp <- rep(NA, length(nonZero))
					DATAtmp[nonZero] <- DATAext
					DATAext <- DATAtmp
					rm(DATAtmp)
				}
			}

			if(GeneralParameters$type.extract == 'mpoly')
			{
				DATAext <- sapply(ij2xtr, function(ij){
					MAT <- vars[ij[, "value"]]
					nijt <- nrow(ij)
					if(nijt > 1){
						MAT <- MAT * ij[, "weight"]
						MAT <- sum(MAT, na.rm = TRUE)
					}
					MAT
				})

				if(!all(nonNull)){
					DATAtmp <- rep(NA, ncol = length(nonNull))
					DATAtmp[nonNull] <- DATAext
					DATAext <- DATAtmp
					rm(DATAtmp)
				}
			}

			if(GeneralParameters$type.extract %in% c('rect', 'poly'))
				DATAext <- vars[ij2xtr[[1]][, "value"]]

			rm(vars); gc()
			return(DATAext)
		}
		if(is.parallel$stop) stopCluster(is.parallel$cluster)
		Insert.Messages.Out('Reading and extracting netcdf data finished')

		ncData <- do.call(rbind, ncData)
		ncData[is.nan(ncData)] <- NA
		cdtdata <- list(dates = dates, data = ncData)
		rm(ncData); gc()

		if(bindData){
			cdtdata$dates <- c(.cdtData$EnvData$procs$cdtdata$dates, cdtdata$dates)
			cdtdata$data <- rbind(.cdtData$EnvData$procs$cdtdata$data, cdtdata$data)
			odaty <- order(cdtdata$dates)
			cdtdata$dates <- cdtdata$dates[odaty]
			cdtdata$data <- cdtdata$data[odaty, , drop = FALSE]
		}
		.cdtData$EnvData$procs$cdtdata <- cdtdata
		.cdtData$EnvData$procs$infoFiles <- infoFiles
		.cdtData$EnvData$procs$ij2xtr1 <- ij2xtr
	}else cdtdata <- .cdtData$EnvData$procs$cdtdata

	#####################################

	if(GeneralParameters$type.extract %in% c('rect', 'poly'))
	{
		if(GeneralParameters$out.data$sp.avrg)
		{
			nDAT <- nrow(cdtdata$data)
			nijt <- nrow(ij2xtr[[1]])
			cdtdata$data <- cdtdata$data * rep(ij2xtr[[1]][, "weight"], rep(nDAT, nijt))
			cdtdata$data <- matrix(rowSums(cdtdata$data, na.rm = TRUE), ncol = 1)
		}
	}

	#####################################
	## Aggregating data

	if(tstep.In != tstep.Out)
	{
		toAggr <- list(cdtdata$dates, tstep.In, tstep.Out, GeneralParameters$aggr.series, headinfo)

		aggregatData <- TRUE
		if(!is.null(.cdtData$EnvData$procs$toAggr))
			aggregatData <- if(!isTRUE(all.equal(.cdtData$EnvData$procs$toAggr, toAggr))) TRUE else FALSE

		aggregatData <- aggregatData | extractGeom | readNCDFdata

		if(aggregatData)
		{
			Insert.Messages.Out('Aggregate data ...')

			outfreq <- if(tstep.Out %in% c('seasonal3', 'seasonal6', 'annual')) "seasonal" else tstep.Out
			agg.index <- cdt.index.aggregate(cdtdata$dates, tstep.In, outfreq, seasonLength, startMonth)
			if(is.null(agg.index)){
				Insert.Messages.Out("Incorrect Season length", format = TRUE)
				return(NULL)
			}

			ifull <- (agg.index$nba / agg.index$nb0) >= GeneralParameters$aggr.series$min.frac
			if(all(!ifull)){
				Insert.Messages.Out("No aggregation, too much missing values", format = TRUE)
				return(NULL)
			}

			odaty <- agg.index$date[ifull]
			index <- agg.index$index[ifull]

			AggrData <- cdt.data.aggregate(cdtdata$data, index, pars = GeneralParameters$aggr.series)

			.cdtData$EnvData$procs$AggrData <- AggrData
			.cdtData$EnvData$procs$AggrDate <- odaty
			.cdtData$EnvData$procs$toAggr <- toAggr

			Insert.Messages.Out('Aggregating data finished')
		}else{
			AggrData <- .cdtData$EnvData$procs$AggrData
			odaty <- .cdtData$EnvData$procs$AggrDate
		}
	}else{
		aggregatData <- FALSE
		AggrData <- cdtdata$data
		odaty <- cdtdata$dates
	}

	#####################################
	## Climatologies & anomalies

	if(GeneralParameters$type.series != "rawts")
	{
		calClim <- GeneralParameters$climato

		computeClimato <- TRUE
		if(!is.null(.cdtData$EnvData$procs$calClim))
			if(isTRUE(all.equal(.cdtData$EnvData$procs$calClim, calClim))) computeClimato <- FALSE

		computeClimato <- computeClimato | aggregatData

		if(computeClimato)
		{
			Insert.Messages.Out('Calculate climatologies ...')

			climato <- GeneralParameters$climato
			years <- as.numeric(substr(odaty, 1, 4))
			iyear <- rep(TRUE, length(years))
			if(!climato$all.years)
				iyear <- (years >= climato$start.year & years <= climato$end.year)
			years <- years[iyear]
			rangeYear <- paste0(range(years), collapse = "-")
			miss <- colSums(!is.na(AggrData[iyear, ])) < climato$min.year

			if(tstep.Out %in% c('seasonal3', 'seasonal6', 'annual'))
			{
				climOUT <- sdOUT <- matrix(NA, nrow = 1, ncol = length(miss))
				if(!all(miss)){
					climOUT[1, !miss] <- colMeans(AggrData[iyear, !miss, drop = FALSE], na.rm = TRUE)
					sdOUT[1, !miss] <- colSds(AggrData[iyear, !miss, drop = FALSE], na.rm = TRUE)
				}
				clim.mon <- paste0(substr(odaty[1], 6, 7), "-", substr(odaty[1], 14, 15))
				climdates <- paste(rangeYear, clim.mon, sep = "_")
				index <- NULL
			}

			if(tstep.Out %in% c('daily', 'pentad', 'dekadal', 'monthly'))
			{
				index <- cdt.index.Climatologies(odaty[iyear], tstep.Out, climato$winsize)
				climOUT <- sdOUT <- matrix(NA, nrow = length(index$id), ncol = length(miss))

				if(!all(miss)){
					dat.clim <- .cdt.Climatologies(index, AggrData[iyear, !miss, drop = FALSE],
													climato$min.year, tstep.Out, climato$winsize)
					climOUT[, !miss] <- dat.clim$mean
					sdOUT[, !miss] <- dat.clim$sd
				}

				if(tstep.Out == 'daily'){
					vtimes <- table.annuel()
					vtimes <- vtimes[index$id, , drop = FALSE]
					clim.date <- paste0(str_pad(vtimes[, 2], 2, pad = "0"), str_pad(vtimes[, 1], 2, pad = "0"))
				}
				if(tstep.Out == 'pentad'){
					vtimes <- expand.grid(1:6, 1:12)
					vtimes <- vtimes[index$id, , drop = FALSE]
					clim.date <- paste0(str_pad(vtimes[, 2], 2, pad = "0"), vtimes[, 1])
				}
				if(tstep.Out == 'dekadal'){
					vtimes <- expand.grid(1:3, 1:12)
					vtimes <- vtimes[index$id, , drop = FALSE]
					clim.date <- paste0(str_pad(vtimes[, 2], 2, pad = "0"), vtimes[, 1])
				}
				if(tstep.Out == 'monthly')
					clim.date <- str_pad(index$id, 2, pad = "0")

				climdates <- paste(rangeYear, clim.date, sep = "_")
			}

			climOUT[is.nan(climOUT) | is.infinite(climOUT)] <- NA
			sdOUT[is.nan(sdOUT) | is.infinite(sdOUT)] <- NA

			.cdtData$EnvData$procs$climdates <- climdates
			.cdtData$EnvData$procs$climOUT <- climOUT
			.cdtData$EnvData$procs$sdOUT <- sdOUT
			.cdtData$EnvData$procs$index <- index
			.cdtData$EnvData$procs$calClim <- calClim

			Insert.Messages.Out('Calculating climatologies done!')
		}else{
			climdates <- .cdtData$EnvData$procs$climdates
			climOUT <- .cdtData$EnvData$procs$climOUT
			sdOUT <- .cdtData$EnvData$procs$sdOUT
			index <- .cdtData$EnvData$procs$index
		}

		##################
		if(GeneralParameters$type.series %in% c('anom', 'stanom'))
		{
			Insert.Messages.Out('Calculate anomalies ...')

			if(tstep.Out %in% c('seasonal3', 'seasonal6', 'annual'))
			{
				anom.date <- odaty
				if(GeneralParameters$type.series == 'anom')
					outMAT <- sweep(AggrData, 2, climOUT, FUN = "-")
				if(GeneralParameters$type.series == 'stanom')
					outMAT <- sweep(sweep(AggrData, 2, climOUT, FUN = "-"), 2, sdOUT, FUN = "/")
			}

			if(tstep.Out %in% c('daily', 'pentad', 'dekadal', 'monthly'))
			{
				index1 <- cdt.index.Anomalies(odaty, index, tstep.Out)
				anom.date <- index1$date
				if(GeneralParameters$type.series == 'anom')
					outMAT <- .cdt.Anomalies(index1$index, AggrData, climOUT, sdOUT, "Difference")
				if(GeneralParameters$type.series == 'stanom')
					outMAT <- .cdt.Anomalies(index1$index, AggrData, climOUT, sdOUT, "Standardized")
			}
			outMAT[is.nan(outMAT) | is.infinite(outMAT)] <- 0

			Insert.Messages.Out('Calculating anomalies done!')
		}
	}

	#####################################
	## Write data
	Insert.Messages.Out('Writing data ...')

	#####################################

	aggr.series <- GeneralParameters$aggr.series

	if(tstep.Out == "daily") outFormat <- "Daily"
	if(tstep.Out == "pentad") outFormat <- "Pentad"
	if(tstep.Out == "dekadal") outFormat <- "Dekadal"
	if(tstep.Out == "monthly") outFormat <- "Monthly"
	if(tstep.Out %in% c("seasonal3", "seasonal6", "annual"))
		seas.lab <- paste0(substr(odaty, 6, 7), "-", substr(odaty, 14, 15))
	if(tstep.Out == "seasonal3") outFormat <- paste("Seasonal", seas.lab)
	if(tstep.Out == "seasonal6") outFormat <- paste("Seasonal", seas.lab)
	if(tstep.Out == "annual") outFormat <- paste("Annual", seas.lab)

	ex_data <- "Extracted_DATA"
	ex.clm_mn <- "Extracted_CLM_MEAN"
	ex.clm_sd <- "Extracted_CLM_SD"
	if(GeneralParameters$type.series == 'anom') odir <- "Extracted_Anomaly"
	if(GeneralParameters$type.series == 'stanom') odir <- "Extracted_Standardized_Anomaly"

	#####################################

	if(GeneralParameters$out.data$format == "ncdf")
	{
		if(GeneralParameters$type.series == 'rawts')
		{
			if(tstep.In == tstep.Out)
			{
				longname <- nc.longname
				name <- nc.name
				units <- nc.units
				prec <- nc.prec
			}else{
				if(tstep.In %in% c("hourly", "daily") & aggr.series$aggr.fun == "count")
				{
					longname <- paste(nc.name, aggr.series$opr.fun, aggr.series$opr.thres)
					longname <- paste0("Number of (", longname, ") from ", nc.longname)
					name <- 'number'
					units <- 'count'
					prec <- "short"
				}else{
					longname <- paste(outFormat, "from", nc.longname)
					name <- nc.name
					units <- nc.units
					prec <- nc.prec
				}
			}
		}

		if(GeneralParameters$type.series %in% c('climato', 'anom', 'stanom'))
		{
			longname.clim <- paste(outFormat, "climatology using", substr(climdates[1], 1, 9), "base period")

			if(GeneralParameters$type.series == 'anom')
			{
				longname <- paste(outFormat, "anomalies, based on", substr(climdates[1], 1, 9), "climatology")
				name <- "anom"
				if(tstep.In == "daily" & GeneralParameters$aggr.series$aggr.fun == "count")
				{
					units <- "count"
					prec <- "short"
				}else{
					units <- nc.units
					prec <- nc.prec
				}
			}
			if(GeneralParameters$type.series == 'stanom')
			{
				longname <- paste(outFormat, "standardized anomalies, based on", substr(climdates[1], 1, 9), "climatology")
				name <- "stanom"
				units <- ""
				prec <- "float"
			}
		}

		#########
		prefix <- strsplit(GeneralParameters$ncdf.file$format, "%")[[1]][1]
		missval <- -9999

		dx <- ncdim_def("Lon", "degreeE", headinfo$x)
		dy <- ncdim_def("Lat", "degreeN", headinfo$y)
		xy.dim <- list(dx, dy)

		if(GeneralParameters$type.series %in% c('climato', 'anom', 'stanom'))
		{
			units1 <- if(tstep.In == "daily" & aggr.series$aggr.fun == "count") "count" else nc.units
			grd.Mean <- ncvar_def("mean", units1, xy.dim, missval, longname.clim, "float", compression = 9)
			grd.SD <- ncvar_def("sd", units1, xy.dim, missval, longname.clim, "float", compression = 9)
		}

		if(GeneralParameters$type.series %in% c('rawts', 'anom', 'stanom'))
			grd.out <- ncvar_def(name, units, xy.dim, missval, longname, prec, compression = 9)

		#########
		if(GeneralParameters$type.series == 'rawts')
		{
			outputDIR <- file.path(GeneralParameters$out.data$outdir, ex_data)
			dir.create(outputDIR, showWarnings = FALSE, recursive = TRUE)
			exract_write_NCDF(AggrData, odaty, headinfo, grd.out, outputDIR, prefix, missval)
		}else{
			if(GeneralParameters$type.series %in% c('climato', 'anom', 'stanom'))
			{
				outputDIR <- file.path(GeneralParameters$out.data$outdir, ex.clm_mn)
				dir.create(outputDIR, showWarnings = FALSE, recursive = TRUE)
				exract_write_NCDF(climOUT, climdates, headinfo, grd.Mean, outputDIR, prefix, missval)
				rm(outputDIR)

				outputDIR <- file.path(GeneralParameters$out.data$outdir, ex.clm_sd)
				dir.create(outputDIR, showWarnings = FALSE, recursive = TRUE)
				exract_write_NCDF(sdOUT, climdates, headinfo, grd.SD, outputDIR, prefix, missval)
				rm(outputDIR)
			}

			if(GeneralParameters$type.series %in% c('anom', 'stanom'))
			{
				outputDIR <- file.path(GeneralParameters$out.data$outdir, odir)
				dir.create(outputDIR, showWarnings = FALSE, recursive = TRUE)
				exract_write_NCDF(outMAT, anom.date, headinfo, grd.out, outputDIR, prefix, missval)
			}
		}
	}

	#####################################

	if(GeneralParameters$out.data$format == "tyxz")
	{
		outputFile <- GeneralParameters$out.data$outdir

		if(GeneralParameters$type.series == 'rawts')
		{
			outfile <- file.path(dirname(outputFile), paste0(ex_data, "_", basename(outputFile)))
			exract_write_TYXZ(AggrData, odaty, headinfo, outfile, .cdtData$Config$missval)
		}else{
			if(GeneralParameters$type.series %in% c('climato', 'anom', 'stanom'))
			{
				outfile <- file.path(dirname(outputFile), paste0(ex.clm_mn, "_", basename(outputFile)))
				exract_write_TYXZ(climOUT, climdates, headinfo, outfile, .cdtData$Config$missval)
				rm(outfile)

				outfile <- file.path(dirname(outputFile), paste0(ex.clm_sd, "_", basename(outputFile)))
				exract_write_TYXZ(sdOUT, climdates, headinfo, outfile, .cdtData$Config$missval)
				rm(outfile)
			}

			if(GeneralParameters$type.series %in% c('anom', 'stanom'))
			{
				outfile <- file.path(dirname(outputFile), paste0(odir, "_", basename(outputFile)))
				exract_write_TYXZ(outMAT, anom.date, headinfo, outfile, .cdtData$Config$missval.anom)
			}
		}
	}

	#####################################

	if(GeneralParameters$out.data$format == "cdt")
	{
		outputFile <- GeneralParameters$out.data$outdir

		if(GeneralParameters$type.series == 'rawts')
		{
			outfile <- file.path(dirname(outputFile), paste0(ex_data, "_", basename(outputFile)))
			exract_write_CDT(AggrData, odaty, headinfo, outfile, .cdtData$Config$missval)
		}else{
			if(GeneralParameters$type.series %in% c('climato', 'anom', 'stanom'))
			{
				outfile <- file.path(dirname(outputFile), paste0(ex.clm_mn, "_", basename(outputFile)))
				exract_write_CDT(climOUT, climdates, headinfo, outfile, .cdtData$Config$missval)
				rm(outfile)

				outfile <- file.path(dirname(outputFile), paste0(ex.clm_sd, "_", basename(outputFile)))
				exract_write_CDT(sdOUT, climdates, headinfo, outfile, .cdtData$Config$missval)
				rm(outfile)
			}

			if(GeneralParameters$type.series %in% c('anom', 'stanom'))
			{
				outfile <- file.path(dirname(outputFile), paste0(odir, "_", basename(outputFile)))
				exract_write_CDT(outMAT, anom.date, headinfo, outfile, .cdtData$Config$missval.anom)
			}
		}
	}

	#####################################

	if(GeneralParameters$out.data$format == "cpt")
	{
		outputFile <- GeneralParameters$out.data$outdir
		name <- nc.name
		units <- if(tstep.In == "daily" & GeneralParameters$aggr.series$aggr.fun == "count") 'count' else nc.units
		cptgrid <- GeneralParameters$type.extract %in% c('rect', 'poly') & !GeneralParameters$out.data$sp.avrg

		if(GeneralParameters$type.series == 'rawts')
		{
			outfile <- file.path(dirname(outputFile), paste0(ex_data, "_", basename(outputFile)))
			exract_write_CPT(AggrData, odaty, headinfo, outfile, cptgrid, name, units)
		}else{
			if(GeneralParameters$type.series %in% c('climato', 'anom', 'stanom'))
			{
				outfile <- file.path(dirname(outputFile), paste0(ex.clm_mn, "_", basename(outputFile)))
				exract_write_CPT(climOUT, climdates, headinfo, outfile, cptgrid, name, units)
				rm(outfile)

				outfile <- file.path(dirname(outputFile), paste0(ex.clm_sd, "_", basename(outputFile)))
				exract_write_CPT(sdOUT, climdates, headinfo, outfile, cptgrid, name, units)
				rm(outfile)
			}

			if(GeneralParameters$type.series %in% c('anom', 'stanom'))
			{
				outfile <- file.path(dirname(outputFile), paste0(odir, "_", basename(outputFile)))
				exract_write_CPT(outMAT, anom.date, headinfo, outfile, cptgrid, name, units)
			}
		}
	}

	return(0)
}

##########################################################################

exract_write_CPT <- function(data.mat, dates, headinfo,
							outfile, cptgrid, name, units)
{
	if(cptgrid){
		xtmp <- lapply(seq_along(dates), function(j){
			x <- round(data.mat[j, ], 3)
			x <- x[headinfo$z]
			dim(x) <- dim(headinfo$z)
			x[is.na(x)] <- -9999
			x
		})
		gridinfo <- headinfo[c('x', 'y')]
		cptIn <- list(data = xtmp, date = dates, gridinfo = gridinfo,
					varid = name, units = units, missval = -9999)
		cptOut <- do.call(CPT.convertGridData, cptIn)
	}else{
		xtmp <- round(data.mat, 3)
		xtmp[is.na(xtmp)] <- -9999
		cptIn <- list(data = xtmp, date = dates, stninfo = headinfo,
						varid = name, units = units, missval = -9999)
		cptOut <- do.call(CPT.convertStationData, cptIn)
	}
	cat(cptOut, file = outfile)
}

exract_write_CDT <- function(data.mat, dates, headinfo,
								outfile, missval)
{
	caption <- c('ID', 'LON', 'DATE/LAT')
	xhead <- t(headinfo)
	xtmp <- round(data.mat, 3)
	xtmp[is.na(xtmp)] <- missval
	xtmp <- rbind(xhead, xtmp)
	xtmp <- cbind(c(caption, dates), xtmp)
	writeFiles(xtmp, outfile)
}

exract_write_TYXZ <- function(data.mat, dates, headinfo,
								outfile, missval)
{
	tyxz.crds <- expand.grid(round(headinfo$x, 6), round(headinfo$y, 6))
	tyxz.crds <- tyxz.crds[, 2:1]
	ret <- lapply(seq_along(dates), function(j){
		xtmp <- data.mat[j, ]
		xtmp <- round(xtmp[headinfo$z], 3)
		xtmp[is.na(xtmp)] <- missval
		xtmp <- data.frame(dates[j], tyxz.crds, xtmp)
		xtmp
	})
	ret <- do.call(rbind, ret)
	writeFiles(ret, outfile)
}

exract_write_NCDF <- function(data.mat, dates, headinfo,
							grd.nc, outdir, prefix, missval)
{
	for(j in seq_along(dates)){
		xtmp <- data.mat[j, ]
		xtmp <- xtmp[headinfo$z]
		dim(xtmp) <- dim(headinfo$z)
		xtmp[is.na(xtmp)] <- missval

		ncoutfile <- file.path(outdir, paste0(prefix, dates[j], ".nc"))
		nc <- nc_create(ncoutfile, grd.nc)
		ncvar_put(nc, grd.nc, xtmp)
		nc_close(nc)
	}
}

