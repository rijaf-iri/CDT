
execMergeTemp <- function(){
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

	daty <- daty[as.numeric(format(daty, '%m'))%in%.cdtData$GalParams$Merging.Date$Months]
	if(.cdtData$GalParams$period == 'daily'){
		xdeb <- format(daty[1], '%Y%m%d')
		xfin <- format(daty[length(daty)], '%Y%m%d')
	}

	if(.cdtData$GalParams$period%in%c('pentad', 'dekadal')){
		xdeb <- paste0(format(daty[1], '%Y%m'), as.numeric(format(daty[1], '%d')))
		xfin <- paste0(format(daty[length(daty)], '%Y%m'), as.numeric(format(daty[length(daty)], '%d')))
	}

	if(.cdtData$GalParams$period == 'monthly'){
		xdeb <- format(daty[1], '%Y%m')
		xfin <- format(daty[length(daty)], '%Y%m')
	}
	origdir <- file.path(.cdtData$GalParams$output$dir, paste("Merged_Temp_Data", xdeb, xfin, sep = '_'))

	dir.create(origdir, showWarnings = FALSE, recursive = TRUE)
	Insert.Messages.Out('Merging data ...')
	Insert.Messages.Out('Data preparation ...')

	freqData <- .cdtData$GalParams$period

	#######get data
	stnData <- getStnOpenData(.cdtData$GalParams$STN.file)
	stnData <- getCDTdataAndDisplayMsg(stnData, freqData, .cdtData$GalParams$STN.file)
	if(is.null(stnData)) return(NULL)

	##################
	## TEMP sample file
	tmpDataInfo <- getNCDFSampleData(.cdtData$GalParams$TEMP$sample)
	if(is.null(tmpDataInfo)){
		Insert.Messages.Out("No downscaled or Adjusted reanalysis data sample found", format = TRUE)
		return(NULL)
	}

	##################
	## Grid for interpolation
	xy.grid <- tmpDataInfo[c('lon', 'lat')]
	nlon0 <- length(tmpDataInfo$lon)
	nlat0 <- length(tmpDataInfo$lat)

	##################
	## DEM data
	demData <- NULL
	if(.cdtData$GalParams$blank$blank == "2" |
	   .cdtData$GalParams$auxvar$dem | .cdtData$GalParams$auxvar$slope |
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

	TMP.DIR <- .cdtData$GalParams$TEMP$dir
	TMP.Format <- .cdtData$GalParams$TEMP$format

	##################
	errmsg <- "No downscaled or Adjusted reanalysis data sample found"
	ncInfo <- ncFilesInfo(freqData, start.date, end.date, months, TMP.DIR, TMP.Format, errmsg)
	if(is.null(ncInfo)) return(NULL)
	ncInfo$ncinfo <- tmpDataInfo

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

	.cdtData$GalParams$mrgParms <- list(months = months, ncInfo = ncInfo,
					stnData = stnData, demData = demData, merge.DIR = origdir,
					interp.grid = list(grid = xy.grid, nlon = nlon0, nlat = nlat0), outMask = outMask)

	ret <- Temp_MergingFunctions()

	rm(stnData, outMask, ncInfo, demData, tmpDataInfo)
	gc()
	if(!is.null(ret)){
		if(ret == 0) return(0)
		else return(ret)
	} else return(NULL)
}

########################################################################################################

Temp_MergingFunctions <- function(){
	Insert.Messages.Out('Merge data ...')

	mrgParms <- .cdtData$GalParams$mrgParms
	freqData <- .cdtData$GalParams$period

	log.file <- file.path(mrgParms$merge.DIR, "log_file.txt")

	#############
	xy.grid <- mrgParms$interp.grid$grid
	newgrid <- defSpatialPixels(xy.grid)

	#############
	## Def ncdf
	dx <- ncdim_def("Lon", "degreeE", xy.grid$lon)
	dy <- ncdim_def("Lat", "degreeN", xy.grid$lat)
	grd.nc.out <- ncvar_def('temp', "DegC", list(dx, dy), -99,
							longname = 'Downscaled Reanalysis merged with station',
							prec = "float", compression = 9)

	#############
	mrg.method <- .cdtData$GalParams$Merging$mrg.method
	interp.method <- .cdtData$GalParams$Merging$interp.method
	pixelize.station <- .cdtData$GalParams$Merging$pixelize.station

	nmin <- .cdtData$GalParams$Merging$nmin
	nmax <- .cdtData$GalParams$Merging$nmax
	maxdist <- .cdtData$GalParams$Merging$maxdist
	vgm.model <- .cdtData$GalParams$Merging$vgm.model
	min.stn <- .cdtData$GalParams$Merging$min.stn

	#############
	lon.stn <- mrgParms$stnData$lon
	lat.stn <- mrgParms$stnData$lat
	date.stn <- mrgParms$stnData$dates
	data.stn <- mrgParms$stnData$data
	demData <- mrgParms$demData

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
	paramsOutFmt <- .cdtData$GalParams$output$format
	ncInfo <- mrgParms$ncInfo

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
			cat(paste(ncInfo$dates[jj], ":", "no temperature data", "|", "no file generated", "\n"),
				file = log.file, append = TRUE)
			return(NULL)
		}

		if(all(is.na(xtmp))){
			cat(paste(ncInfo$dates[jj], ":", "all data are missing", "|", "no file generated", "\n"),
				file = log.file, append = TRUE)
			return(NULL)
		}

		############

		newgrid$grd <- c(xtmp)

		donne.stn <- data.stn[date.stn == ncInfo$dates[jj], , drop = FALSE]
		if(nrow(donne.stn) == 0){
			if(!is.null(mrgParms$outMask)) xtmp[is.na(mrgParms$outMask)] <- NA
			writeNC.merging(xtmp, ncInfo$dates[jj], freqData, grd.nc.out, mrgParms$merge.DIR, paramsOutFmt)
			cat(paste(ncInfo$dates[jj], ":", "no station data", "|", "Input temperature data", "\n"), file = log.file, append = TRUE)
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
				"Input temperature data", "\n"), file = log.file, append = TRUE)
			return(NULL)
		}

		############

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
										TRUE, ncInfo$dates[jj], MODEL.COEF,
										ijGrd, log.file)
		###########
		#Apply mask for area of interest
		if(!is.null(mrgParms$outMask)) out.mrg[is.na(mrgParms$outMask)] <- NA

		writeNC.merging(out.mrg, ncInfo$dates[jj], freqData, grd.nc.out, mrgParms$merge.DIR, paramsOutFmt)

		return(0)
	})

	Insert.Messages.Out('Merging finished')
	return(0)
}
