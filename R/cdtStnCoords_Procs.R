
StnChkCoordsProcs <- function(GeneralParameters){
	if(!dir.exists(GeneralParameters$output)){
		Insert.Messages.Out(paste(GeneralParameters$output, "did not find"), format = TRUE)
		return(NULL)
	}

	if(GeneralParameters$data.type == "cdtcoords")
	{
		don0 <- getStnOpenData(GeneralParameters$infile)
		if(is.null(don0)) return(NULL)
		
		nom.col <- names(don0)
		don.disp <- don0
		coords <- list(id = as.character(don0[, 1]),
						lon = as.numeric(don0[, 3]),
						lat = as.numeric(don0[, 4]))
		coords$idx <- seq_along(coords$lon)
	}

	if(GeneralParameters$data.type == "cdtstation")
	{
		don0 <- getStnOpenData(GeneralParameters$infile)
		if(is.null(don0)) return(NULL)
		don <- splitCDTData0(don0)
		if(is.null(don)) return(NULL)
		don <- don[c('id', 'lon', 'lat', 'elv')]
		nom.col <- c("ID", "Longitude", "Latitude", "Elevation")
		if(is.null(don$elv)){
			don <- don[c('id', 'lon', 'lat')]
			nom.col <- nom.col[1:3]
		}
		don.disp <- as.data.frame(don)
		names(don.disp) <- nom.col
		coords <- don[c('id', 'lon', 'lat')]
		coords$idx <- seq_along(coords$lon)
		rm(don)
	}

	############

	outdir <- file.path(GeneralParameters$output, "CHECK.COORDS_data")
	dir.create(outdir, showWarnings = FALSE, recursive = TRUE)
	fileout <- file.path(outdir, paste0('Checked_Coords_', GeneralParameters$infile))
	don.info <- getStnOpenDataInfo(GeneralParameters$infile)
	sep <- don.info[[3]]$sepr
	if(sep == "") sep <- " "
	write.table(don0, file = fileout, sep = sep, na = don.info[[3]]$miss.val,
				col.names = don.info[[3]]$header, row.names = FALSE, quote = FALSE)
	rm(don0)

	############

	if(GeneralParameters$shpfile == "")
	{
		Insert.Messages.Out("No ESRI shapefile found", format = TRUE)
		Insert.Messages.Out("The stations outside the boundaries will not be checked", format = TRUE)
		shpd <- NULL
	}else{
		shpd <- getShpOpenData(GeneralParameters$shpfile)
		if(is.null(shpd)){
			Insert.Messages.Out(paste('Unable to open', GeneralParameters$shpfile, 'or it is not an ESRI shapefile'), format = TRUE)
			Insert.Messages.Out("The stations outside the boundaries will not be checked", format = TRUE)
			shpd <- NULL
		}else{
			shpd <- as(shpd[[2]], "SpatialPolygons")
			shpd <- gUnaryUnion(shpd)
			shpd <- gSimplify(shpd, tol = 0.05, topologyPreserve = TRUE)
			shpd <- gBuffer(shpd, width = GeneralParameters$buffer/111)
		}
	}

	############

	output <- list(params = GeneralParameters, info = don.info, id = coords$idx)

	coords <- as.data.frame(coords)
	coords$id <- as.character(coords$id)
	don.disp$idx <- coords$idx
	don.disp$LonX <- coords$lon
	don.disp$LatX <- coords$lat
	don.disp$StatusX <- rep("blue", length(coords$lon))
	don.table <- NULL

	############

	## Missing coords
	imiss <- is.na(coords$lon) | is.na(coords$lat)
	if(any(imiss)){
		don.table$miss <- data.frame(State = 'Missing Coordinates', don.disp[imiss, , drop = FALSE])
		don.disp <- don.disp[!imiss, , drop = FALSE]
		coords <- coords[!imiss, , drop = FALSE]
	}

	## Wrong coords
	iwrong <- coords$lon < -180 | coords$lon > 360 | coords$lat < -90 | coords$lat > 90
	if(any(iwrong)){
		don.table$wrong <- data.frame(State = 'Invalid Coordinates', don.disp[iwrong, , drop = FALSE])
		don.disp <- don.disp[!iwrong, , drop = FALSE]
		coords <- coords[!iwrong, , drop = FALSE]
	}

	## Duplicated ID
	iddup <- duplicated(coords$id) | duplicated(coords$id, fromLast = TRUE)
	if(any(iddup)){
		don.table$iddup <- data.frame(State = 'Duplicate ID', don.disp[iddup, , drop = FALSE])
		don.table$iddup <- don.table$iddup[order(coords$id[iddup]), , drop = FALSE]
		don.disp$StatusX[iddup] <- "orange"
	}

	## Duplicated coordinates
	crddup <- duplicated(coords[, c('lon', 'lat'), drop = FALSE]) |
			duplicated(coords[, c('lon', 'lat'), drop = FALSE], fromLast = TRUE)
	if(any(crddup)){
		don.table$crddup <- data.frame(State = 'Duplicate Coordinates', don.disp[crddup, , drop = FALSE])
		don.table$crddup <- don.table$crddup[order(paste0(coords$lon[crddup], coords$lat[crddup])), , drop = FALSE]
		don.disp$StatusX[crddup] <- "orange"
	}

	## Coordinates outside boundaries
	if(!is.null(shpd)){
		spcoords <- coords
		coordinates(spcoords) <- ~lon+lat
		iout <- is.na(over(spcoords, geometry(shpd)))
		if(any(iout)){
			don.table$out <- data.frame(State = 'Coordinates Outside', don.disp[iout, , drop = FALSE])
			don.table$out <- don.table$out[order(coords$id[iout]), , drop = FALSE]
			don.disp$StatusX[iout] <- "red"
		}
		rm(spcoords, shpd)
	}

	############

	if(!is.null(don.table)){
		don.table <- do.call(rbind, don.table)
		don.table <- don.table[, !names(don.table) %in% c('LonX', 'LatX', 'StatusX'), drop = FALSE]
		rownames(don.table) <- NULL
	}

	output$coords <- coords

	############

	file.index <- file.path(outdir, 'CoordinatesCheck.rds')
	dataOUT <- file.path(outdir, 'CDTDATASET')
	dir.create(dataOUT, showWarnings = FALSE, recursive = TRUE)
	file.table.csv <- file.path(outdir, 'Stations_to_Check.csv')
	file.table.rds <- file.path(dataOUT, 'Table.rds')
	file.display <- file.path(dataOUT, 'Display.rds')

	saveRDS(output, file.index)
	saveRDS(don.disp, file.display)

	saveRDS(don.table, file.table.rds)
	if(!is.null(don.table)) writeFiles(don.table, file.table.csv, col.names = TRUE)

	############

	.cdtData$EnvData$output <- output
	.cdtData$EnvData$PathData <- outdir
	.cdtData$EnvData$Table.Disp <- don.table
	.cdtData$EnvData$Maps.Disp <- don.disp

	return(0)
}

##########################################################################

StnChkCoordsDataStn <- function(GeneralParameters){
	if(GeneralParameters$data.type == "cdtcoords")
	{
		don0 <- getStnOpenData(GeneralParameters$infile)
		if(is.null(don0)) return(NULL)
		
		nom.col <- names(don0)
		don.orig <- don0
		coords <- list(id = as.character(don0[, 1]),
						lon = as.numeric(don0[, 3]),
						lat = as.numeric(don0[, 4]))
	}

	if(GeneralParameters$data.type == "cdtstation")
	{
		don0 <- getStnOpenData(GeneralParameters$infile)
		if(is.null(don0)) return(NULL)
		don <- splitCDTData0(don0)
		if(is.null(don)) return(NULL)
		don <- don[c('id', 'lon', 'lat', 'elv')]
		nom.col <- c("ID", "Longitude", "Latitude", "Elevation")
		if(is.null(don$elv)){
			don <- don[c('id', 'lon', 'lat')]
			nom.col <- nom.col[1:3]
		}
		don.orig <- as.data.frame(don)
		names(don.orig) <- nom.col
		coords <- don[c('id', 'lon', 'lat')]
		rm(don)
	}

	############
	rm(don0)
	coords <- as.data.frame(coords)
	don.orig$LonX <- coords$lon
	don.orig$LatX <- coords$lat
	don.orig$StatusX <- rep("blue", length(coords$lon))

	############

	## Missing coords
	imiss <- is.na(coords$lon) | is.na(coords$lat)
	if(any(imiss)){
		don.orig <- don.orig[!imiss, , drop = FALSE]
		coords <- coords[!imiss, , drop = FALSE]
	}

	## Wrong coords
	iwrong <- coords$lon < -180 | coords$lon > 360 | coords$lat < -90 | coords$lat > 90
	if(any(iwrong)){
		don.orig <- don.orig[!iwrong, , drop = FALSE]
		coords <- coords[!iwrong, , drop = FALSE]
	}

	.cdtData$EnvData$output$coords <- coords
	.cdtData$EnvData$Maps.Disp <- don.orig

	return(0)
}

##########################################################################

StnChkCoordsCorrect <- function(){
	if(is.null(.cdtData$EnvData$Table.Disp0)){
		Insert.Messages.Out("No stations to be corrected")
		return(NULL)
	}

	idx0 <- as.numeric(as.character(.cdtData$EnvData$Table.Disp0$idx))
	if(!is.null(.cdtData$EnvData$Table.Disp)){
		idx <- as.numeric(as.character(.cdtData$EnvData$Table.Disp$idx))
		nom <- names(.cdtData$EnvData$Table.Disp)
		nom <- nom != "idx"
		nom[1] <- FALSE
		id.del0 <- idx0[!idx0 %in% idx]
		change <- .cdtData$EnvData$Table.Disp[, nom, drop = FALSE]
		change <- as.matrix(change)
	}else{
		id.del0 <- idx0
		change <- NULL
	}

	######
	info <- .cdtData$EnvData$output$info
	fileout <- file.path(.cdtData$EnvData$PathData,
						paste0('Checked_Coords_', .cdtData$EnvData$output$params$infile))
	don0 <- read.table(fileout, header = info[[3]]$header,
						sep = info[[3]]$sepr, na.strings = info[[3]]$miss.val,
						stringsAsFactors = FALSE, colClasses = "character")
	filemap <- file.path(.cdtData$EnvData$PathData, 'CDTDATASET', 'Display.rds')
	map.disp <- readRDS(filemap)
	nom1 <- names(map.disp)
	nom1 <- which(!nom1 %in% c('idx', 'LonX', 'LatX', 'StatusX'))

	######
	if(.cdtData$EnvData$output$params$data.type == "cdtcoords"){
		if(nrow(change) > 0){
			ix <- match(idx, .cdtData$EnvData$output$id)
			don0[ix, ] <- change

			pos.lon <- 3
			pos.lat <- 4
		}
		if(length(id.del0)){
			ix1 <- match(id.del0, .cdtData$EnvData$output$id)
			don0 <- don0[-ix1, , drop = FALSE]
		}
	}

	if(.cdtData$EnvData$output$params$data.type == "cdtstation"){
		if(nrow(change) > 0){
			ix <- match(idx, .cdtData$EnvData$output$id)
			don0[1:ncol(change), ix + 1] <- change

			pos.lon <- 2
			pos.lat <- 3
		}
		if(length(id.del0)){
			ix1 <- match(id.del0, .cdtData$EnvData$output$id)
			don0 <- don0[, -(ix1 + 1), drop = FALSE]
		}
	}

	if(length(id.del0)) .cdtData$EnvData$output$id <- .cdtData$EnvData$output$id[-ix1]

	######

	idx1 <- .cdtData$EnvData$output$coords$idx
	if(length(id.del0)) id.del1 <- idx1[idx1 %in% id.del0]

	if(nrow(change) > 0){
		ix0 <- match(idx, idx1)
		ina <- is.na(ix0)
		if(any(ina)){
			ix0 <- ix0[!ina]
			change0 <- change[!ina, , drop = FALSE]
			change1 <- change[ina, , drop = FALSE]
			idx2 <- idx[ina]
		}else change0 <- change

		.cdtData$EnvData$output$coords$id[ix0] <- as.character(change0[, 1])
		.cdtData$EnvData$output$coords$lon[ix0] <- as.numeric(change0[, pos.lon])
		.cdtData$EnvData$output$coords$lat[ix0] <- as.numeric(change0[, pos.lat])

		map.disp[ix0, nom1] <- change0
		map.disp$LonX[ix0] <- as.numeric(change0[, pos.lon])
		map.disp$LatX[ix0] <- as.numeric(change0[, pos.lat])

		.cdtData$EnvData$Maps.Disp[ix0, nom1] <- change0
		.cdtData$EnvData$Maps.Disp$LonX[ix0] <- as.numeric(change0[, pos.lon])
		.cdtData$EnvData$Maps.Disp$LatX[ix0] <- as.numeric(change0[, pos.lat])
		if(any(ina)){
			idx1 <- c(idx1, idx2)
			## check invalid coordinates
			tmp <- data.frame(id = as.character(change1[, 1]),
							lon = as.numeric(change1[, pos.lon]),
							lat = as.numeric(change1[, pos.lat]),
							idx = idx2, stringsAsFactors = FALSE)
			.cdtData$EnvData$output$coords <- rbind(.cdtData$EnvData$output$coords, tmp)

			tmp1 <- data.frame(change1,
							idx = idx2,
							LonX = as.numeric(change1[, pos.lon]),
							LatX = as.numeric(change1[, pos.lat]),
							StatusX = "red", stringsAsFactors = FALSE)

			map.disp <- rbind(map.disp, tmp1)
			.cdtData$EnvData$Maps.Disp <- rbind(.cdtData$EnvData$Maps.Disp, tmp1)
		}
	}

	if(length(id.del1)){
		ix <- match(id.del1, idx1)
		.cdtData$EnvData$output$coords <- .cdtData$EnvData$output$coords[-ix, , drop = FALSE]
		map.disp <- map.disp[-ix, , drop = FALSE]
		.cdtData$EnvData$Maps.Disp <- .cdtData$EnvData$Maps.Disp[-ix, , drop = FALSE]
	}

	######
	sep <- info[[3]]$sepr
	if(sep == "") sep <- " "
	write.table(don0, file = fileout, sep = sep, na = info[[3]]$miss.val,
				col.names = info[[3]]$header, row.names = FALSE, quote = FALSE)
	saveRDS(map.disp, filemap)

	return(0)
}
