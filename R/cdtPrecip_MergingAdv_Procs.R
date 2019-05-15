
execMergeRain <- function(){
	daty <- .cdtData$GalParams$Merging.Date
	xdeb <- as.Date(paste(daty$start.year, daty$start.mon, daty$start.day, sep = '-'))
	xfin <- as.Date(paste(daty$end.year, daty$end.mon, daty$end.day, sep = '-'))
	if(.cdtData$GalParams$period == 'daily') daty <- seq(xdeb, xfin, 'day')
	if(.cdtData$GalParams$period == 'pentad'){
		daty <- seq(xdeb, xfin, 'day')
		daty <- daty[as.numeric(format(daty, '%d')) <= 6]
	}
	if(.cdtData$GalParams$period == 'dekadal'){
		daty <- seq(xdeb, xfin, 'day')
		daty <- daty[as.numeric(format(daty, '%d')) <= 3]
	}
	if(.cdtData$GalParams$period == 'monthly') daty <- seq(xdeb, xfin, 'month')

	daty <- daty[as.numeric(format(daty, '%m')) %in% .cdtData$GalParams$Merging.Date$Months]
	if(.cdtData$GalParams$period == 'daily'){
		xdeb <- format(daty[1], '%Y%m%d')
		xfin <- format(daty[length(daty)], '%Y%m%d')
	}

	if(.cdtData$GalParams$period %in% c('pentad', 'dekadal')){
		xdeb <- paste0(format(daty[1], '%Y%m'), as.numeric(format(daty[1], '%d')))
		xfin <- paste0(format(daty[length(daty)], '%Y%m'), as.numeric(format(daty[length(daty)], '%d')))
	}

	if(.cdtData$GalParams$period == 'monthly'){
		xdeb <- format(daty[1], '%Y%m')
		xfin <- format(daty[length(daty)], '%Y%m')
	}
	origdir <- file.path(.cdtData$GalParams$output$dir, paste("Merged_Precip_Data", xdeb, xfin, sep = '_'))

	dir.create(origdir, showWarnings = FALSE, recursive = TRUE)
	Insert.Messages.Out('Merging data ...')
	Insert.Messages.Out('Data preparation ...')

	freqData <- .cdtData$GalParams$period

	##################
	## Get data
	stnData <- getStnOpenData(.cdtData$GalParams$STN.file)
	stnData <- getCDTdataAndDisplayMsg(stnData, freqData, .cdtData$GalParams$STN.file)
	if(is.null(stnData)) return(NULL)

	##################
	## RFE sample file
	rfeDataInfo <- getNCDFSampleData(.cdtData$GalParams$RFE$sample)
	if(is.null(rfeDataInfo)){
		Insert.Messages.Out("No RFE or Adjusted RFE data sample found", format = TRUE)
		return(NULL)
	}

	##################
	## DEM data
	demData <- NULL
	if(.cdtData$GalParams$blank$blank == "2" |
		.cdtData$GalParams$Grid.Creation$grid == "2" |
		.cdtData$GalParams$auxvar$dem |
		.cdtData$GalParams$auxvar$slope |
		.cdtData$GalParams$auxvar$aspect)
	{
		demData <- getNCDFSampleData(.cdtData$GalParams$DEM.file)
		if(is.null(demData)){
			Insert.Messages.Out("No elevation data found", format = TRUE)
			return(NULL)
		}
		jfile <- getIndex.AllOpenFiles(.cdtData$GalParams$DEM.file)
		demData <- .cdtData$OpenFiles$Data[[jfile]][[2]]
		demData$lon <- demData$x
		demData$lat <- demData$y
	}

	##################
	##Create grid for interpolation
	if(.cdtData$GalParams$Grid.Creation$grid == '1'){
		grd.lon <- rfeDataInfo$lon
		grd.lat <- rfeDataInfo$lat
	}else if(.cdtData$GalParams$Grid.Creation$grid == '2'){
		grd.lon <- demData$lon
		grd.lat <- demData$lat
	}else if(.cdtData$GalParams$Grid.Creation$grid == '3'){
		X0 <- .cdtData$GalParams$Grid.Creation$minlon
		X1 <- .cdtData$GalParams$Grid.Creation$maxlon
		pX <- .cdtData$GalParams$Grid.Creation$reslon
		Y0 <- .cdtData$GalParams$Grid.Creation$minlat
		Y1 <- .cdtData$GalParams$Grid.Creation$maxlat
		pY <- .cdtData$GalParams$Grid.Creation$reslat
		grd.lon <- seq(X0, X1, pX)
		grd.lat <- seq(Y0, Y1, pY)
	}
	nlon0 <- length(grd.lon)
	nlat0 <- length(grd.lat)
	xy.grid <- list(lon = grd.lon, lat = grd.lat)

	##################
	## regrid DEM data
	if(!is.null(demData)){
		is.regridDEM <- is.diffSpatialPixelsObj(defSpatialPixels(xy.grid), defSpatialPixels(demData), tol = 1e-07)
		if(is.regridDEM)
			demData <- cdt.interp.surface.grid(demData, xy.grid)
		demData$z[demData$z < 0] <- 0
	}

	##################
	## Get RFE data info
	start.year <- .cdtData$GalParams$Merging.Date$start.year
	start.mon <- .cdtData$GalParams$Merging.Date$start.mon
	start.dek <- .cdtData$GalParams$Merging.Date$start.day
	end.year <- .cdtData$GalParams$Merging.Date$end.year
	end.mon <- .cdtData$GalParams$Merging.Date$end.mon
	end.dek <- .cdtData$GalParams$Merging.Date$end.day
	months <- .cdtData$GalParams$Merging.Date$Months
	start.date <- as.Date(paste(start.year, start.mon, start.dek, sep = '/'), format = '%Y/%m/%d')
	end.date <- as.Date(paste(end.year, end.mon, end.dek, sep = '/'), format = '%Y/%m/%d')

	RFE.DIR <- .cdtData$GalParams$RFE$dir
	RFE.Format <- .cdtData$GalParams$RFE$format

	##################
	errmsg <- "RFE or bias corrected RFE data not found"
	ncInfo <- ncFilesInfo(freqData, start.date, end.date, months, RFE.DIR, RFE.Format, errmsg)
	if(is.null(ncInfo)) return(NULL)
	ncInfo$ncinfo <- rfeDataInfo
	ncInfo$xy.rfe <- rfeDataInfo[c('lon', 'lat')]

	##################
	## blanking
	outMask <- switch(.cdtData$GalParams$blank$blank,
					"2" = {
							mask <- demData$z
							mask[mask <= 0] <- NA
							mask[!is.na(mask)] <- 1
							mask
						},
					"3" = {
							shpd <- getShpOpenData(.cdtData$GalParams$blank$SHP.file)[[2]]
							shpd[['vtmp']] <- 1
							mask <- over(defSpatialPixels(xy.grid), shpd)[, 'vtmp']
							dim(mask) <- c(nlon0, nlat0)
							mask
						}, NULL)

	Insert.Messages.Out('Data preparation finished')

	##################

	.cdtData$GalParams$mrgParms <- list(months = months, ncInfo = ncInfo,
					stnData = stnData, demData = demData, merge.DIR = origdir,
					interp.grid = list(grid = xy.grid, nlon = nlon0, nlat = nlat0), outMask = outMask)

	ret <- Precip_MergingFunctions()

	rm(ncInfo)
	gc()
	if(!is.null(ret)){
		if(ret != 0) return(ret) 
	}else return(NULL)

	return(0)
}

########################################################################################################

Precip_MergingFunctions <- function(){
	Insert.Messages.Out('Merge data ...')

	mrgParms <- .cdtData$GalParams$mrgParms
	freqData <- .cdtData$GalParams$period

	log.file <- file.path(mrgParms$merge.DIR, "log_file.txt")

	#############
	xy.grid <- mrgParms$interp.grid$grid
	# grdSp <- defSpatialPixels(xy.grid)
	newgrid <- defSpatialPixels(xy.grid)
	# nlon0 <- mrgParms$interp.grid$nlon
	# nlat0 <- mrgParms$interp.grid$nlat

	#############
	## Def ncdf
	dx <- ncdim_def("Lon", "degreeE", xy.grid$lon)
	dy <- ncdim_def("Lat", "degreeN", xy.grid$lat)
	grd.nc.out <- ncvar_def("precip", "mm", list(dx, dy), -99,
							longname = "Merged Station-Satellite Rainfall",
							prec = "short", shuffle = TRUE, compression = 9)

	#############
	mrg.method <- .cdtData$GalParams$Merging$mrg.method
	interp.method <- .cdtData$GalParams$Merging$interp.method
	# local.interpolation <- .cdtData$GalParams$Merging$local.interpolation
	pixelize.station <- .cdtData$GalParams$Merging$pixelize.station

	nmin <- .cdtData$GalParams$Merging$nmin
	nmax <- .cdtData$GalParams$Merging$nmax
	maxdist <- .cdtData$GalParams$Merging$maxdist
	vgm.model <- .cdtData$GalParams$Merging$vgm.model
	min.stn <- .cdtData$GalParams$Merging$min.stn
	# min.non.zero <- .cdtData$GalParams$Merging$min.non.zero

	# use.RnoR <- .cdtData$GalParams$RnoR$use.RnoR
	# maxdist.RnoR <- .cdtData$GalParams$RnoR$maxdist.RnoR
	# smooth.RnoR <- .cdtData$GalParams$RnoR$smooth.RnoR
	# wet.day <- .cdtData$GalParams$RnoR$wet.day
	pars.RnoR <- .cdtData$GalParams$RnoR

	#############
	lon.stn <- mrgParms$stnData$lon
	lat.stn <- mrgParms$stnData$lat
	date.stn <- mrgParms$stnData$dates
	data.stn <- mrgParms$stnData$data
	# nstn <- length(lon.stn)
	demData <- mrgParms$demData

	#############

	# res.lon <- diff(range(xy.grid$lon)) / (nlon0 - 1)
	# res.lat <- diff(range(xy.grid$lat)) / (nlat0 - 1)
	# res.latlon <- max(res.lon, res.lat)

	# if(local.interpolation){
	# 	if(maxdist < res.latlon) maxdist <- res.latlon * sqrt(2)
	# 	res.coarse <- maxdist / sqrt(2)
	# 	if(res.coarse < res.latlon) res.coarse <- maxdist
	# }else{
	# 	maxdist <- res.latlon * 10
	# 	res.coarse <- maxdist / sqrt(2)
	# }

	# if(maxdist < maxdist.RnoR) maxdist.RnoR <- maxdist

	# if(!is.null(demData)){
	# 	slpasp <- raster.slope.aspect(demData)
	# 	demData$slp <- slpasp$slope
	# 	demData$asp <- slpasp$aspect
	# }else{
	# 	demData <- list(x = xy.grid$lon, y = xy.grid$lat,
	# 					z = matrix(1, nlon0, nlat0),
	# 					slp = matrix(0, nlon0, nlat0),
	# 					asp = matrix(0, nlon0, nlat0))
	# }

	# interp.grid <- createGrid.merging(demData, res.coarse = res.coarse)
	# cells <- SpatialPixels(points = interp.grid$newgrid, tolerance = sqrt(sqrt(.Machine$double.eps)))@grid
	# bGrd <- if(.cdtData$GalParams$Merging$use.block) createBlock(cells@cellsize, 1, 5) else NULL

	#############
	auxvar <- c('dem', 'slp', 'asp', 'alon', 'alat')
	is.auxvar <- unlist(.cdtData$GalParams$auxvar)

	formuleRK <- formula(paste0('stn', '~', 'grd'))
	formule <- formula(paste0('res', '~', 1))
	if(any(is.auxvar)){
		formule <- formula(paste0('res', '~', paste(auxvar[is.auxvar], collapse = '+')))
		if(mrg.method == "Regression Kriging" & .cdtData$GalParams$Merging$sp.trend.aux)
			formuleRK <- formula(paste0('stn', '~', 'grd', '+', paste(auxvar[is.auxvar], collapse = '+')))
	}

	# #############
	# auxvar <- c('dem', 'slp', 'asp', 'alon', 'alat')
	# is.auxvar <- unlist(.cdtData$GalParams$auxvar)
	# if(any(is.auxvar)){
	# 	formule <- formula(paste0('res', '~', paste(auxvar[is.auxvar], collapse = '+')))
	# 	if(mrg.method == "Regression Kriging"){
	# 		sp.trend.aux <- .cdtData$GalParams$Merging$sp.trend.aux
	# 		if(sp.trend.aux)
	# 			formuleRK <- formula(paste0('stn', '~', 'rfe', '+', paste(auxvar[is.auxvar], collapse = '+')))
	# 		else
	# 			formuleRK <- formula(paste0('stn', '~', 'rfe'))
	# 	}
	# }else{
	# 	formule <- formula(paste0('res', '~', 1))
	# 	if(mrg.method == "Regression Kriging")
	# 		formuleRK <- formula(paste0('stn', '~', 'rfe'))
	# }

	#############

	if(is.auxvar['dem']){
		if(!is.null(demData))
			newgrid$dem <- c(demData$z)
		else
			Insert.Messages.Out("Elevation data doesn't find", format = TRUE)
	}

	if(is.auxvar['slope'] | is.auxvar['aspect']){
		if(!is.null(demData)){
			slpasp <- raster.slope.aspect(demData)
			if(is.auxvar['slope'])
				newgrid$slp <- c(slpasp$slope)
			if(is.auxvar['aspect'])
				newgrid$asp <- c(slpasp$aspect)
		}else{
			Insert.Messages.Out("Elevation data doesn't find", format = TRUE)
		}
	}

	if(is.auxvar['lon']) newgrid$alon <- newgrid@coords[, 'lon']

	if(is.auxvar['lat']) newgrid$alat <- newgrid@coords[, 'lat']

	#############
	# if(is.auxvar['lon']){
	# 	interp.grid$coords.coarse$alon <- interp.grid$coords.coarse@coords[, 'lon']
	# 	if(!is.null(interp.grid$coords.var)) interp.grid$coords.var$alon <- interp.grid$coords.var@coords[, 'lon']
	# 	interp.grid$newgrid$alon <- interp.grid$newgrid@coords[, 'lon']
	# }

	# if(is.auxvar['lat']){
	# 	interp.grid$coords.coarse$alat <- interp.grid$coords.coarse@coords[, 'lat']
	# 	if(!is.null(interp.grid$coords.var)) interp.grid$coords.var$alat <- interp.grid$coords.var@coords[, 'lat']
	# 	interp.grid$newgrid$alat <- interp.grid$newgrid@coords[, 'lat']
	# }

	#############

	months <- mrgParms$months
	MODEL.COEF <- NULL
	if(mrg.method == "Spatio-Temporal LM"){
		coefFiles <- file.path(.cdtData$GalParams$LMCOEF$dir.LMCoef, sprintf(.cdtData$GalParams$LMCOEF$format, months))
		existLMCfl <- file.exists(coefFiles)
		if(any(!existLMCfl)){
			for(i in which(!existLMCfl)) Insert.Messages.Out(paste(coefFiles[i], "doesn't exist"), format = TRUE)
			return(NULL)
		}
		MODEL.COEF <- vector(mode = 'list', length = 12)
		for(fl in seq(coefFiles)){
			nc <- nc_open(coefFiles[fl])
			coef1 <- ncvar_get(nc, varid = 'slope')
			coef2 <- ncvar_get(nc, varid = 'intercept')
			nc_close(nc)
			MODEL.COEF[[months[fl]]] <- list(slope = coef1, intercept = coef2)
		}
	}

	############# 
	GalParams <- .cdtData$GalParams
	ncInfo <- mrgParms$ncInfo

	#############
	rfeSp <- defSpatialPixels(ncInfo$xy.rfe)
	is.regridRFE <- is.diffSpatialPixelsObj(newgrid, rfeSp, tol = 1e-07)

	#############
	xlon <- ncInfo$ncinfo$lon
	xlat <- ncInfo$ncinfo$lat
	ijGrd <- grid2pointINDEX(list(lon = lon.stn, lat = lat.stn), list(lon = xlon, lat = xlat))

	#############

	ret <- cdt.foreach(seq_along(ncInfo$nc.files),
					parsL = length(which(ncInfo$exist)) >= 20,
					.packages = c('sp', 'ncdf4'), FUN = function(jj)
	{
		if(ncInfo$exist[jj]){
			nc <- nc_open(ncInfo$nc.files[jj])
			xtmp <- ncvar_get(nc, varid = ncInfo$ncinfo$varid)
			nc_close(nc)
			xtmp <- transposeNCDFData(xtmp, ncInfo$ncinfo)
		}else{
			cat(paste(ncInfo$dates[jj], ":", "no RFE data", "|", "no file generated", "\n"),
				file = log.file, append = TRUE)
			return(NULL)
		}

		if(all(is.na(xtmp))){
			cat(paste(ncInfo$dates[jj], ":", "all RFE data are missing", "|", "no file generated", "\n"),
				file = log.file, append = TRUE)
			return(NULL)
		}

		############
		if(is.regridRFE){
			rfeGrid <- cdt.interp.surface.grid(list(lon = xlon, lat = xlat, z = xtmp), xy.grid)
			xtmp <- rfeGrid$z
		}

		############

		# donne.stn <- data.stn[date.stn == ncInfo$dates[jj], , drop = FALSE]
		# if(nrow(donne.stn) == 0){
		# 	writeNC.merging(xrfe, ncInfo$dates[jj], freqData, grd.nc.out,
		# 			mrgParms$merge.DIR, GalParams$output$format)
		# 	cat(paste(ncInfo$dates[jj], ":", "no station data", "|", "RFE data", "\n"), file = log.file, append = TRUE)
		# 	return(NULL)
		# }

		# donne.stn <- data.frame(lon = lon.stn, lat = lat.stn, stn = c(donne.stn))
		# if(pixelize.station){
		# 	stng <- createGrid.StnData(donne.stn, ijGrd, interp.grid$newgrid, min.stn, weighted = TRUE)
		# }else{
		# 	stng <- donne.stn$stn
		# 	if(length(stng[!is.na(stng)]) < min.stn) stng <- NULL
		# }

		# if(is.null(stng)){
		# 	writeNC.merging(xrfe, ncInfo$dates[jj], freqData, grd.nc.out,
		# 			mrgParms$merge.DIR, GalParams$output$format)
		# 	cat(paste(ncInfo$dates[jj], ":", "not enough station data", "|", "RFE data", "\n"),
		# 		file = log.file, append = TRUE)
		# 	return(NULL)
		# }


		newgrid$grd <- c(xtmp)

		donne.stn <- data.stn[date.stn == ncInfo$dates[jj], , drop = FALSE]
		if(nrow(donne.stn) == 0){
			if(!is.null(mrgParms$outMask)) xtmp[is.na(mrgParms$outMask)] <- NA
			writeNC.merging(xtmp, ncInfo$dates[jj], freqData, grd.nc.out, mrgParms$merge.DIR, paramsOutFmt)
			cat(paste(ncInfo$dates[jj], ":", "no station data", "|", "Input RFE data", "\n"), file = log.file, append = TRUE)
			return(NULL)
		}

		donne.stn <- data.frame(lon = lon.stn, lat = lat.stn, stn = c(donne.stn[1, ]))
		if(pixelize.station){
			stng <- createGrid.StnData(donne.stn, ijGrd, newgrid, min.stn, weighted = TRUE)
		}else{
			stng <- donne.stn$stn
			if(length(stng[!is.na(stng)]) < min.stn) stng <- NULL
		}

		if(is.null(stng)){
			if(!is.null(mrgParms$outMask)) xtmp[is.na(mrgParms$outMask)] <- NA
			writeNC.merging(xtmp, ncInfo$dates[jj], freqData, grd.nc.out, mrgParms$merge.DIR, paramsOutFmt)
			cat(paste(ncInfo$dates[jj], ":", "not enough station data", "|",
				"Input RFE data", "\n"), file = log.file, append = TRUE)
			return(NULL)
		}

		############
		# if(pixelize.station){
		# 	locations.stn <- interp.grid$newgrid
		# 	locations.stn$stn <- stng
		# 	locations.stn$rfe <- c(xrfe)
		# }else{
		# 	locations.stn <- interp.grid$newgrid[ijGrd, ]
		# 	locations.stn@coords <- as.matrix(donne.stn[, c("lon", "lat")])
		# 	locations.stn$stn <- stng
		# 	locations.stn$rfe <- xrfe[ijGrd]
		# 	if(is.auxvar['lon']) locations.stn$alon <- donne.stn$lon
		# 	if(is.auxvar['lat']) locations.stn$alat <- donne.stn$lat
		# }

		if(pixelize.station){
			locations.stn <- newgrid
			locations.stn$stn <- stng
		}else{
			locations.stn <- newgrid[ijGrd, ]
			locations.stn@coords <- as.matrix(donne.stn[, c("lon", "lat")])
			locations.stn$stn <- stng
		}


		############
		noNA <- !is.na(locations.stn$stn)
		locations.stn <- locations.stn[noNA, ]

		locations.df <- as.data.frame(!is.na(locations.stn@data))
		locations.stn <- locations.stn[Reduce("&", locations.df), ]

		############

		if(length(locations.stn) < min.stn){
			if(!is.null(mrgParms$outMask)) xtmp[is.na(mrgParms$outMask)] <- NA
			writeNC.merging(xtmp, ncInfo$dates[jj], freqData, grd.nc.out, mrgParms$merge.DIR, paramsOutFmt)
			cat(paste(ncInfo$dates[jj], ":", "not enough data", "|", "Input temperature data", "\n"), file = log.file, append = TRUE)
			return(NULL)
		}

		############

		out.mrg <- cdt.merging.functions(locations.stn, newgrid, 
										mrg.method, interp.method,
										formule, formuleRK,
										maxdist, nmin, nmax, vgm.model,
										FALSE, ncInfo$dates[jj], MODEL.COEF, ijGrd)
		if(pars.RnoR$use.RnoR){
			rnr <- rain_no_rain.mask(locations.stn, newgrid, pars.RnoR)
			out.mrg <- out.mrg * rnr
		}

		############
		#Apply mask for area of interest
		if(!is.null(mrgParms$outMask)) out.mrg[is.na(mrgParms$outMask)] <- NA

		writeNC.merging(out.mrg, ncInfo$dates[jj], freqData, grd.nc.out,
				mrgParms$merge.DIR, GalParams$output$format)

		return(0)
	})

	Insert.Messages.Out('Merging finished')
	return(0)
}


