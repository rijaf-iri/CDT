
Precip_Merging_ALL <- function(){
	daty <- .cdtData$GalParams$Merging.Date
	xdeb <- as.Date(paste(daty$start.year, daty$start.mon, daty$start.day, sep = '-'))
	xfin <- as.Date(paste(daty$end.year, daty$end.mon, daty$end.day, sep = '-'))
	if(.cdtData$GalParams$period == 'daily') daty <- seq(xdeb, xfin, 'day')
	if(.cdtData$GalParams$period == 'monthly') daty <- seq(xdeb, xfin, 'month')
	if(.cdtData$GalParams$period == 'pentad'){
		daty <- seq(xdeb, xfin, 'day')
		daty <- daty[as.numeric(format(daty, '%d')) <= 6]
	}
	if(.cdtData$GalParams$period == 'dekadal'){
		daty <- seq(xdeb, xfin, 'day')
		daty <- daty[as.numeric(format(daty, '%d')) <= 3]
	}
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
	origdir <- file.path(.cdtData$GalParams$output$dir, paste('Merging_Precip_Data', xdeb, xfin, sep = '_'))

	dir.create(origdir, showWarnings = FALSE, recursive = TRUE)
	Insert.Messages.Out('Start merging ...')

	freqData <- .cdtData$GalParams$period
	.cdtData$GalParams$auxvar <- list(dem = FALSE, slope = FALSE, aspect = FALSE, lon = FALSE, lat = FALSE)
	.cdtData$GalParams$biasFilenames <- .cdtData$GalParams$BIAS$format
	.cdtData$GalParams$lmCoefFilenames <- .cdtData$GalParams$LMCOEF$format

	##################
	## Get data
	stnData <- getStnOpenData(.cdtData$GalParams$STN.file)
	stnData <- getCDTdataAndDisplayMsg(stnData, freqData, .cdtData$GalParams$STN.file)
	if(is.null(stnData)) return(NULL)

	##################
	## RFE sample file
	rfeDataInfo <- getNCDFSampleData(.cdtData$GalParams$RFE$sample)
	if(is.null(rfeDataInfo)){
		Insert.Messages.Out("No RFE data sample found", format = TRUE)
		return(NULL)
	}

	##################
	## Grid for interpolation
	.cdtData$GalParams$Grid.Creation$grid <- "1"
	xy.grid <- rfeDataInfo[c('lon', 'lat')]
	nlon0 <- length(rfeDataInfo$lon)
	nlat0 <- length(rfeDataInfo$lat)

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
	errmsg <- "RFE data not found"
	ncInfo <- ncFilesInfo(freqData, start.date, end.date, months, RFE.DIR, RFE.Format, errmsg)
	if(is.null(ncInfo)) return(NULL)
	ncInfo$ncinfo <- rfeDataInfo
	ncInfo$xy.rfe <- rfeDataInfo[c('lon', 'lat')]

	##################
	## DEM data
	demData <- NULL
	if(((!.cdtData$GalParams$BIAS$deja.calc &
		.cdtData$GalParams$BIAS$interp.method == "NN") |
		(.cdtData$GalParams$LMCOEF$interp.method == "NN" &
		!.cdtData$GalParams$LMCOEF$deja.calc &
		.cdtData$GalParams$Merging$mrg.method == "Spatio-Temporal LM") |
		.cdtData$GalParams$blank$blank == "2"))
	{
		demInfo <- getNCDFSampleData(.cdtData$GalParams$DEM.file)
		if(is.null(demInfo)){
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
	## Compute BIAS
	if(!.cdtData$GalParams$BIAS$deja.calc)
	{
		allyears <- .cdtData$GalParams$BIAS$all.years
		year1 <- .cdtData$GalParams$BIAS$start.year
		year2 <- .cdtData$GalParams$BIAS$end.year
		minyear <- .cdtData$GalParams$BIAS$min.year
		years <- as.numeric(substr(stnData$dates, 1, 4))
		iyrUse <- if(allyears) rep(TRUE, length(years)) else years >= year1 & years <= year2
		years <- years[iyrUse]

		if(length(unique(years)) < minyear){
			Insert.Messages.Out("Data too short", format = TRUE)
			return(NULL)
		}

		start.date1 <- as.Date(paste0(years[1], '0101'), format = '%Y%m%d')
		end.date1 <- as.Date(paste0(years[length(years)], '1231'), format = '%Y%m%d')

		ncInfoBias <- ncFilesInfo(freqData, start.date1, end.date1, months, RFE.DIR, RFE.Format, errmsg)
		if(is.null(ncInfoBias)) return(NULL)
		ncInfoBias$ncinfo <- rfeDataInfo
		ncInfoBias$xy.rfe <- rfeDataInfo[c('lon', 'lat')]

		# calculate bias factors
		bias.DIR <- file.path(origdir, "BIAS_Data")
		dir.create(bias.DIR, showWarnings = FALSE, recursive = TRUE)
		.cdtData$GalParams$biasParms <- list(stnData = stnData, ncInfo = ncInfoBias, bias.DIR = bias.DIR,
						months = months, interp.grid = list(grid = xy.grid, nlon = nlon0, nlat = nlat0))
		bias.pars <- Precip_ComputeBias()
		if(is.null(bias.pars)) return(NULL)
		#########
		.cdtData$GalParams$biasParms <- list(bias.pars = bias.pars, months = months,
						stnData = stnData[c('lon', 'lat')], demData = demData, ncInfo = ncInfoBias, bias.DIR = bias.DIR,
						interp.grid = list(grid = xy.grid, nlon = nlon0, nlat = nlat0))
		ret <- Precip_InterpolateBias()
		rm(bias.pars, ncInfoBias)
		gc()
		if(!is.null(ret)){
			if(ret != 0) return(ret) 
		}else return(NULL)
	}else bias.DIR <- .cdtData$GalParams$BIAS$dir.Bias

	##################
	## APPLY BIAS correction
	adj.DIR <- file.path(origdir, "ADJUSTED_Data")
	dir.create(adj.DIR, showWarnings = FALSE, recursive = TRUE)

	if(!.cdtData$GalParams$LMCOEF$deja.calc &
		.cdtData$GalParams$Merging$mrg.method == "Spatio-Temporal LM")
	{
		allyears <- .cdtData$GalParams$LMCOEF$all.years
		year1 <- .cdtData$GalParams$LMCOEF$start.year
		year2 <- .cdtData$GalParams$LMCOEF$end.year
		minyear <- .cdtData$GalParams$LMCOEF$min.year
		years <- as.numeric(substr(stnData$dates, 1, 4))
		iyrUse <- if(allyears) rep(TRUE, length(years)) else years >= year1 & years <= year2
		years <- years[iyrUse]

		if(length(unique(years)) < minyear){
			Insert.Messages.Out("Data too short", format = TRUE)
			return(NULL)
		}

		start.date1 <- as.Date(paste0(years[1], '0101'), format = '%Y%m%d')
		end.date1 <- as.Date(paste0(years[length(years)], '1231'), format = '%Y%m%d')

		start.date1 <- min(start.date1, start.date)
		end.date1 <- max(end.date1, end.date)

		ncInfoAdj <- ncFilesInfo(freqData, start.date1, end.date1, months, RFE.DIR, RFE.Format, errmsg)
		if(is.null(ncInfoAdj)) return(NULL)
		ncInfoAdj$ncinfo <- rfeDataInfo
		ncInfoAdj$xy.rfe <- rfeDataInfo[c('lon', 'lat')]
		AdjDate <- ncInfoAdj$dates
		extractADJ <- TRUE
	}else{
		ncInfoAdj <- ncInfo
		AdjDate <- ncInfo$dates
		extractADJ <- FALSE
	}

	##################
	## READ BIAS FILSES
	.cdtData$GalParams$biasParms <- list(bias.DIR = bias.DIR, dates = AdjDate, months = months)
	BIAS <- Precip_ReadBiasFiles()
	if(is.null(BIAS)) return(NULL)

	##################

	.cdtData$GalParams$biasParms <- list(adj.DIR = adj.DIR, extractADJ = extractADJ, BIAS = BIAS,
										ncInfo = ncInfoAdj, stnData = stnData[c('lon', 'lat')])

	data.adj.stn <- Precip_ApplyBiasCorrection()

	rm(BIAS, ncInfoAdj)
	if(!extractADJ){
		if(!is.null(data.adj.stn)){
			if(data.adj.stn != 0) return(data.adj.stn) 
		}else return(NULL)
	}

	##################
	if(!.cdtData$GalParams$LMCOEF$deja.calc &
		.cdtData$GalParams$Merging$mrg.method == "Spatio-Temporal LM")
	{
		ncInfoLMCoef <- NULL
		ncInfoLMCoef$data <- data.adj.stn

		LMCoef.DIR <- file.path(origdir, "LMCOEF_Data")
		dir.create(LMCoef.DIR, showWarnings = FALSE, recursive = TRUE)
		.cdtData$GalParams$lmCoefParms <- list(stnData = stnData,
							ncInfo = ncInfoLMCoef, LMCoef.DIR = LMCoef.DIR, months = months,
							interp.grid = list(grid = xy.grid, nlon = nlon0, nlat = nlat0))
		model.coef <- Precip_ComputeLMCoef()
		if(is.null(model.coef)) return(NULL)
		#########
		.cdtData$GalParams$lmCoefParms <- list(model.coef = model.coef, months = months,
						stnData = stnData[c('lon', 'lat')], demData = demData,
						ncInfo = ncInfo, LMCoef.DIR = LMCoef.DIR,
						interp.grid = list(grid = xy.grid, nlon = nlon0, nlat = nlat0))
		ret <- Precip_InterpolateLMCoef()

		.cdtData$GalParams$LMCOEF$dir.LMCoef <- LMCoef.DIR
		rm(model.coef)
		gc()
		if(!is.null(ret)){
			if(ret != 0) return(ret) 
		}else return(NULL)
	}

	##################
	if(freqData %in% c('daily', 'pentad', 'dekadal')) adj.Format <- "rr_adj_%s%s%s.nc"
	if(freqData == 'monthly') adj.Format <- "rr_adj_%s%s.nc"

	errmsg <- "Adjusted data not found"
	ncInfo <- ncFilesInfo(freqData, start.date, end.date, months, adj.DIR, adj.Format, errmsg)
	if(is.null(ncInfo)) return(NULL)
	ncInfo$ncinfo <- rfeDataInfo
	ncInfo$ncinfo$ilon <- 1
	ncInfo$ncinfo$ilat <- 2
	ncInfo$ncinfo$varid <- "precip"
	ncInfo$ncinfo$xo <- seq_along(rfeDataInfo$lon)
	ncInfo$ncinfo$yo <- seq_along(rfeDataInfo$lat)
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

	merge.DIR <- file.path(origdir, "Merged_Data")
	dir.create(merge.DIR, showWarnings = FALSE, recursive = TRUE)

	.cdtData$GalParams$mrgParms <- list(months = months, ncInfo = ncInfo,
					stnData = stnData, demData = demData, merge.DIR = merge.DIR,
					interp.grid = list(grid = xy.grid, nlon = nlon0, nlat = nlat0), outMask = outMask)

	ret <- Precip_MergingFunctions()

	rm(ncInfo)
	gc()
	if(!is.null(ret)){
		if(ret != 0) return(ret) 
	}else return(NULL)

	return(0)
}
