
execAjdBiasDownTemp <- function(){
	daty <- .cdtData$GalParams$Adjust.Date
	if(.cdtData$GalParams$period == 'monthly'){
		xdeb <- paste0(format(ISOdate(2014, daty$start.mon, 1), "%b"), daty$start.year)
		xfin <- paste0(format(ISOdate(2014, daty$end.mon, 1), "%b"), daty$end.year)
	}else{
		xdeb <- paste0(daty$start.day, format(ISOdate(2014, daty$start.mon, 1), "%b"), daty$start.year)
		xfin <- paste0(daty$end.day, format(ISOdate(2014, daty$end.mon, 1), "%b"), daty$end.year)
	}
	origdir <- file.path(.cdtData$GalParams$output$dir, paste('ADJUSTED_Temp_Data', xdeb, xfin, sep = '_'))

	dir.create(origdir, showWarnings = FALSE, recursive = TRUE)
	Insert.Messages.Out('Adjustment of downscaled data ...')

	##################
	## TEMP sample file
	tmpDataInfo <- getNCDFSampleData(.cdtData$GalParams$TEMP$sample)
	if(is.null(tmpDataInfo)){
		Insert.Messages.Out("No downscaled data sample found", format = TRUE)
		return(NULL)
	}

	################
	freqData <- .cdtData$GalParams$period
	start.year <- .cdtData$GalParams$Adjust.Date$start.year
	start.mon <- .cdtData$GalParams$Adjust.Date$start.mon
	start.dek <- .cdtData$GalParams$Adjust.Date$start.day
	end.year <- .cdtData$GalParams$Adjust.Date$end.year
	end.mon <- .cdtData$GalParams$Adjust.Date$end.mon
	end.dek <- .cdtData$GalParams$Adjust.Date$end.day
	months <- .cdtData$GalParams$Adjust.Date$Months
	start.date <- as.Date(paste(start.year, start.mon, start.dek, sep = '/'), format = '%Y/%m/%d')
	end.date <- as.Date(paste(end.year, end.mon, end.dek, sep = '/'), format = '%Y/%m/%d')

	TMP.DIR <- .cdtData$GalParams$TEMP$dir
	TMP.Format <- .cdtData$GalParams$TEMP$format

	##################
	errmsg <- "Downscaled data not found"
	ncInfo <- ncFilesInfo(freqData, start.date, end.date, months, TMP.DIR, TMP.Format, errmsg)
	if(is.null(ncInfo)) return(NULL)
	ncInfo$ncinfo <- tmpDataInfo

	##################
	## READ BIAS FILSES
	BIAS.DIR <- .cdtData$GalParams$BIAS$dir.Bias
	.cdtData$GalParams$biasFilenames <- .cdtData$GalParams$BIAS$format

	.cdtData$GalParams$biasParms <- list(bias.DIR = BIAS.DIR, dates = ncInfo$dates, months = months)
	BIAS <- Temp_ReadBiasFiles()
	if(is.null(BIAS)) return(NULL)

	##################
	.cdtData$GalParams$biasParms <- list(adj.DIR = origdir, extractADJ = FALSE, BIAS = BIAS,
									ncInfo = ncInfo, stnData = list(lon = NULL, lat = NULL))

	ret <- Temp_ApplyBiasCorrection()
	rm(BIAS, tmpDataInfo, ncInfo)
	gc()
	if(!is.null(ret)){
		if(ret == 0) return(0)
		else return(ret)
	} else return(NULL)
}

#######################################################################################

Temp_ReadBiasFiles <- function(){
	Insert.Messages.Out('Read bias data ...')
	biasParms <- .cdtData$GalParams$biasParms
	months <- biasParms$months

	if(.cdtData$GalParams$BIAS$bias.method == "Multiplicative.Bias.Mon"){
		biasFilename <- sprintf(.cdtData$GalParams$biasFilenames, months)
		biasFile <- file.path(biasParms$bias.DIR, biasFilename)
		exist.bias <- unlist(lapply(biasFile, file.exists))
		if(any(!exist.bias)){
			miss.bias <- months[!exist.bias]
			for(j in seq_along(miss.bias)){
				msg <- biasFilename[miss.bias[j]]
				Insert.Messages.Out(paste(msg, "doesn't exist"), format = TRUE)
			}
			return(NULL)
		}
		nc <- nc_open(biasFile[which(exist.bias)[1]])
		lon <- nc$dim[[1]]$vals
		lat <- nc$dim[[2]]$vals
		nc_close(nc)
		BIAS <- vector(mode = 'list', length = 12)
		BIAS[months] <- lapply(seq_along(months), function(m){
			nc <- nc_open(biasFile[m])
			xvar <- ncvar_get(nc, varid = nc$var[[1]]$name)
			nc_close(nc)
			xvar
		})
		bias <- list(lon = lon, lat = lat, bias = BIAS)
	}

	if(.cdtData$GalParams$BIAS$bias.method == "Multiplicative.Bias.Var"){
		freqData <- .cdtData$GalParams$period
		if(freqData == 'daily'){
			endmon <- c(31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31)
			vtimes <- cbind(unlist(sapply(endmon, function(j) 1:j)), rep(1:12, endmon), 1:365)
			vtimes <- vtimes[vtimes[, 2] %in% months, , drop = FALSE]

			daty <- biasParms$dates
			daty <- daty[as.numeric(substr(daty, 5, 6)) %in% vtimes[, 2]]
			xdaty <- paste(as.numeric(substr(daty, 7, 8)), as.numeric(substr(daty, 5, 6)), sep = '_')
			xvtm <- paste(vtimes[, 1], vtimes[, 2], sep = '_')
			times.stn <- vtimes[match(xdaty, xvtm), 3]
			times.stn[is.na(times.stn)] <- 59
			times.stn <- sort(unique(times.stn))
			BIAS <- vector(mode = 'list', length = 365) 
		}
		if(freqData == 'pentad'){
			vtimes <- cbind(expand.grid(1:6, 1:12), 1:72)
			vtimes <- vtimes[vtimes[, 2] %in% months, , drop = FALSE]

			daty <- biasParms$dates
			daty <- daty[as.numeric(substr(daty, 5, 6)) %in% vtimes[, 2]]
			xdaty <- paste(as.numeric(substr(daty, 7, 7)), as.numeric(substr(daty, 5, 6)), sep = '_')
			xvtm <- paste(vtimes[, 1], vtimes[, 2], sep = '_')
			times.stn <- vtimes[match(xdaty, xvtm), 3]
			times.stn <- sort(unique(times.stn))
			BIAS <- vector(mode = 'list', length = 72)
		}
		if(freqData == 'dekadal'){
			vtimes <- cbind(expand.grid(1:3, 1:12), 1:36)
			vtimes <- vtimes[vtimes[, 2] %in% months, , drop = FALSE]

			daty <- biasParms$dates
			daty <- daty[as.numeric(substr(daty, 5, 6)) %in% vtimes[, 2]]
			xdaty <- paste(as.numeric(substr(daty, 7, 7)), as.numeric(substr(daty, 5, 6)), sep = '_')
			xvtm <- paste(vtimes[, 1], vtimes[, 2], sep = '_')
			times.stn <- vtimes[match(xdaty, xvtm), 3]
			times.stn <- sort(unique(times.stn))
			BIAS <- vector(mode = 'list', length = 36)
		}
		if(freqData == 'monthly'){
			times.stn <- months
			BIAS <- vector(mode = 'list', length = 12)
		}

		biasFilename <- sprintf(.cdtData$GalParams$biasFilenames, times.stn)
		biasFile <- file.path(biasParms$bias.DIR, biasFilename)
		exist.bias <- unlist(lapply(biasFile, file.exists))

		if(any(!exist.bias)){
			miss.bias <- times.stn[!exist.bias]
			for(j in seq_along(miss.bias)){
				msg <- biasFilename[miss.bias[j]]
				Insert.Messages.Out(paste(msg, "doesn't exist"), format = TRUE)
			}
			return(NULL)
		}
		nc <- nc_open(biasFile[which(exist.bias)[1]])
		lon <- nc$dim[[1]]$vals
		lat <- nc$dim[[2]]$vals
		nc_close(nc)
		BIAS[times.stn] <- lapply(seq_along(times.stn), function(m){
			nc <- nc_open(biasFile[m])
			xvar <- ncvar_get(nc, varid = "bias")
			nc_close(nc)
			xvar
		})
		bias <- list(lon = lon, lat = lat, bias = BIAS)
	}

	if(.cdtData$GalParams$BIAS$bias.method == "Quantile.Mapping"){
		pars.stnFile <- file.path(biasParms$bias.DIR, paste0('Gaussian_Pars.STN_', months, '.nc'))
		exist.pars.stn <- unlist(lapply(pars.stnFile, file.exists))
		if(any(!exist.pars.stn)){
			miss.pars.stn <- months[!exist.pars.stn]
			for(j in seq_along(miss.pars.stn)){
				msg <- paste0('Gaussian_Pars.STN_', miss.pars.stn[j], '.nc')
				Insert.Messages.Out(paste(msg, "doesn't exist"), format = TRUE)
			}
			return(NULL)
		}

		pars.downFile <- file.path(biasParms$bias.DIR, paste0('Gaussian_Pars.REANAL_', months, '.nc'))
		exist.pars.down <- unlist(lapply(pars.downFile, file.exists))
		if(any(!exist.pars.down)){
			miss.pars.down <- months[!exist.pars.down]
			for(j in seq_along(miss.pars.down)){
				msg <- paste0('Gaussian_Pars.REANAL_', miss.pars.down[j], '.nc')
				Insert.Messages.Out(paste(msg, "doesn't exist"), format = TRUE)
			}
			return(NULL)
		}
		nc <- nc_open(pars.stnFile[which(exist.pars.stn)[1]])
		lon <- nc$dim[[1]]$vals
		lat <- nc$dim[[2]]$vals
		nc_close(nc)
		PARS.stn <- vector(mode = 'list', length = 12)
		PARS.down <- vector(mode = 'list', length = 12)
		PARS.stn[months] <- lapply(seq_along(months), function(m){
			nc <- nc_open(pars.stnFile[m])
			moy <- ncvar_get(nc, varid = "mean")
			ect <- ncvar_get(nc, varid = "sd")
			nc_close(nc)
			list(mean = moy, sd = ect)
		})

		PARS.down[months] <- lapply(seq_along(months), function(m){
			nc <- nc_open(pars.downFile[m])
			moy <- ncvar_get(nc, varid = "mean")
			ect <- ncvar_get(nc, varid = "sd")
			nc_close(nc)
			list(mean = moy, sd = ect)
		})
		bias <- list(lon = lon, lat = lat, bias.stn = PARS.stn, bias.down = PARS.down)
	}
	Insert.Messages.Out('Reading bias data finished')
	return(bias)
}

#######################################################################################

Temp_ApplyBiasCorrection <- function(){
	Insert.Messages.Out('Apply bias correction ...')
	biasParms <- .cdtData$GalParams$biasParms
	extractADJ <- biasParms$extractADJ
	bias.method <- .cdtData$GalParams$BIAS$bias.method
	freqData <- .cdtData$GalParams$period

	################
	dx <- ncdim_def("Lon", "degreeE", biasParms$BIAS$lon)
	dy <- ncdim_def("Lat", "degreeN", biasParms$BIAS$lat)
	xy.dim <- list(dx, dy)
	grd.bsadj <- ncvar_def("temp", "DegC", list(dx, dy), -99, longname= "Bias Corrected Reanalysis", prec = "float", compression = 9)

	################

	biasGrd <- biasParms$BIAS[c('lon', 'lat')]
	if(extractADJ) ijSTN <- grid2pointINDEX(biasParms$stnData, biasGrd)

	################
	nc.dates <- biasParms$ncInfo$dates[biasParms$ncInfo$exist]
	nc.files <- biasParms$ncInfo$nc.files[biasParms$ncInfo$exist]
	ncinfo <- biasParms$ncInfo$ncinfo

	##################
	xlon <- ncinfo$lon
	xlat <- ncinfo$lat
	filenameFormat <- .cdtData$GalParams$output$format

	ret <- cdt.foreach(seq_along(nc.files),
					parsL = length(nc.files) >= 30,
					.packages = 'ncdf4', FUN = function(jfl)
	{
		nc <- nc_open(nc.files[jfl])
		xtmp <- ncvar_get(nc, varid = ncinfo$varid)
		nc_close(nc)
		xtmp <- transposeNCDFData(xtmp, ncinfo)
		tmp.date <- nc.dates[jfl]

		############
		if(bias.method == "Multiplicative.Bias.Var"){
			if(freqData == 'daily'){
				ann <- as.numeric(substr(tmp.date, 1, 4))
				iday <- as.numeric(strftime(as.Date(tmp.date, format = '%Y%m%d'), format = '%j'))
				ijt <- ifelse(is.leapyear(ann) & iday > 59, iday - 1, iday)
			}
			if(freqData == 'pentad'){
				mon <- as.numeric(substr(tmp.date, 5, 6))
				pen <- as.numeric(substr(tmp.date, 7, 7))
				annual.pen <- expand.grid(pen = 1:6, mon = 1:12)
				ijt <- which(annual.pen$pen == pen & annual.pen$mon == mon)
			}
			if(freqData == 'dekadal'){
				mon <- as.numeric(substr(tmp.date, 5, 6))
				dek <- as.numeric(substr(tmp.date, 7, 7))
				annual.dek <- expand.grid(dek = 1:3, mon = 1:12)
				ijt <- which(annual.dek$dek == dek & annual.dek$mon == mon)
			}
			if(freqData == 'monthly'){
				ijt <- as.numeric(substr(tmp.date, 5, 6))
			}
		}else{
			ijt <- as.numeric(substr(tmp.date, 5, 6))
		}

		############
		if(bias.method == "Quantile.Mapping"){
			xadj <- quantile.mapping.Gau(xtmp, biasParms$BIAS$bias.stn[[ijt]],
											biasParms$BIAS$bias.down[[ijt]])
		}else xadj <- xtmp * biasParms$BIAS$bias[[ijt]]
		xadj[xadj < 0] <- 0
		adjSTN <- if(extractADJ) xadj[ijSTN] else NULL
		xadj[is.na(xadj)] <- -99

		############
		year <- substr(tmp.date, 1, 4)
		month <- substr(tmp.date, 5, 6)

		if(freqData == 'daily'){
			adjfrmt <- sprintf(filenameFormat, year, month, substr(tmp.date, 7, 8))
		}else if(freqData %in% c('pentad', 'dekadal')){
			adjfrmt <- sprintf(filenameFormat, year, month, substr(tmp.date, 7, 7))
		}else  adjfrmt <- sprintf(filenameFormat, year, month)

		outfl <- file.path(biasParms$adj.DIR, adjfrmt)

		nc2 <- nc_create(outfl, grd.bsadj)
		ncvar_put(nc2, grd.bsadj, xadj)
		nc_close(nc2)

		return(adjSTN)
	})

	if(extractADJ){
		adjSTN <- list(dates = nc.dates, data = do.call(rbind, adj.stn))
	}else adjSTN <- 0

	rm(biasGrd, adj.stn)
	gc()
	Insert.Messages.Out('Bias Correction done')
	return(adjSTN)
}
