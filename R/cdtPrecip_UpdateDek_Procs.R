
mergeOneDekadRain <- function(){
	dir2save <- .cdtData$GalParams$output$dir
	dyear <- .cdtData$GalParams$Merging.Date$year
	dmon <- .cdtData$GalParams$Merging.Date$month
	ddek <- as.numeric(.cdtData$GalParams$Merging.Date$dekad)
	if(ddek > 3 | ddek < 1){
		Insert.Messages.Out('Dekad must be 1, 2 or 3', format = TRUE)
		tcl("update")
		return(NULL)
	}

	daty <- paste0(format(as.Date(paste(dyear, dmon, ddek, sep = '-')), '%Y%m'), ddek)
	dmon <- substr(daty, 5, 6)
	yeardekad <- expand.grid(1:3, 1:12)
	dir2save <- file.path(dir2save, paste('DEKAD', daty, sep = '_'))
	dir.create(dir2save, showWarnings = FALSE, recursive = TRUE)

	################
	## get rfe data
	if(.cdtData$GalParams$RFE$downloaded){
		rfeInfo <- getNCDFSampleData(.cdtData$GalParams$RFE$file)
		if(is.null(rfeInfo)){
			Insert.Messages.Out("No RFE data found", format = TRUE)
			return(NULL)
		}
		rfeData <- getNcdfOpenData(.cdtData$GalParams$RFE$file)[[2]]
		ncInfo <- list(dates = daty, nc.files = rfeData$file, exist = TRUE)
		ncInfo$ncinfo <- rfeInfo
		ncInfo$xy.rfe <- rfeInfo[c('lon', 'lat')]
		rfeData <- rfeData[1:3]
		names(rfeData) <- c('lon', 'lat', 'z')
	}else{
		Insert.Messages.Out("Download RFE data .....")

		if(!testConnection()){
			Insert.Messages.Out('No Internet connection', format = TRUE)
			return(NULL)
		}

		xml.dlg <- file.path(.cdtDir$dirLocal, "languages", "cdtDownloadRFE_dlgBox.xml")
		lang.dlg <- cdtLanguageParse(xml.dlg, .cdtData$Config$lang.iso)
		.cdtData$GalParams$dekadRFE <- c('10-DAYS TAMSATv3', '10-DAYS TAMSATv2', '10-DAYS CHIRPv1.0', '10-DAYS CHIRPSv2.0')
		.cdtData$GalParams$message <- lang.dlg[['message']]

		.cdtData$GalParams$rfe.data <- .cdtData$GalParams$RFE$source
		.cdtData$GalParams$IRI.DL <- TRUE

		.cdtData$GalParams$date$year1 <- dyear
		.cdtData$GalParams$date$mon1 <- dmon
		.cdtData$GalParams$date$day1 <- ddek
		.cdtData$GalParams$date$year2 <- dyear
		.cdtData$GalParams$date$mon2 <- dmon
		.cdtData$GalParams$date$day2 <- ddek
		.cdtData$GalParams$istart <- do.call(paste, c(.cdtData$GalParams$date[c("year1", "mon1", "day1")], sep = '-'))
		.cdtData$GalParams$iend <- do.call(paste, c(.cdtData$GalParams$date[c("year2", "mon2", "day2")], sep = '-'))
		.cdtData$GalParams$dir2save <- dir2save

		ExecDownload_RFEData(watch.cursor = FALSE)
		datasrc <- download_RFE_createURL()
		file.rfe <- file.path(dir2save, paste0(datasrc$name[2], '_', datasrc$name[1], '_Extracted'), datasrc$files)
		nc <- nc_open(file.rfe)
		lon <- nc$dim[[1]]$vals
		lat <- nc$dim[[2]]$vals
		varid <- nc$var[[1]]$name
		z <- ncvar_get(nc, varid = varid)
		nc_close(nc)
		rfeData <- list(lon = lon, lat = lat, z = z)
		ncInfo <- list(dates = daty, nc.files = file.rfe, exist = TRUE)
		ncInfo$ncinfo <- list(lon = lon, lat = lat, varid = varid, ilon = 1, ilat = 2,
							xo = seq_along(lon), yo = seq_along(lat),
							nx = length(lon), ny = length(lat))
		ncInfo$xy.rfe <- list(lon = lon, lat = lat)
	}

	################
	## correct bias
	if(.cdtData$GalParams$BIAS$Adjust){
		bias.method <- .cdtData$GalParams$BIAS$method
		biasDir <- .cdtData$GalParams$BIAS$Dir

		if(bias.method == "Quantile.Mapping"){
			parsstnf <- paste0('Bernoulli-Gamma_Pars.STN', '_', as.numeric(dmon), '.nc')
			pars.stnFile <- file.path(biasDir, parsstnf)
			if(!file.exists(pars.stnFile)){
				Insert.Messages.Out(paste(parsstnf, "doesn't exist"), format = TRUE)
				return(NULL)
			}
			parsrfef <- paste0('Bernoulli-Gamma_Pars.RFE', '_', as.numeric(dmon), '.nc')
			pars.rfeFile <- file.path(biasDir, parsrfef)
			if(!file.exists(pars.rfeFile)){
				Insert.Messages.Out(paste(parsrfef, "doesn't exist"), format = TRUE)
				return(NULL)
			}

			nc <- nc_open(pars.stnFile)
			lon.bias <- nc$dim[[1]]$vals
			lat.bias <- nc$dim[[2]]$vals
			prob.stn <- ncvar_get(nc, varid = "prob")
			scale.stn <- ncvar_get(nc, varid = "scale")
			shape.stn <- ncvar_get(nc, varid = "shape")
			nc_close(nc)
			pars.stn <- list(prob = prob.stn, scale = scale.stn, shape = shape.stn)

			nc <- nc_open(pars.rfeFile)
			prob.rfe <- ncvar_get(nc, varid = "prob")
			scale.rfe <- ncvar_get(nc, varid = "scale")
			shape.rfe <- ncvar_get(nc, varid = "shape")
			nc_close(nc)
			pars.rfe <- list(prob = prob.rfe, scale = scale.rfe, shape = shape.rfe)
		}else{
			idek <- if(bias.method == "Multiplicative.Bias.Mon") as.numeric(dmon) else which(yeardekad[, 2] == as.numeric(dmon) & yeardekad[, 1] == ddek)
			biasf <- paste0("STN_GRID_MeanBias_", idek, '.nc')
			biasFile <- file.path(biasDir, biasf)
			if(!file.exists(biasFile)){
				Insert.Messages.Out(paste(biasf, "doesn't exist"), format = TRUE)
				tcl("update")
				return(NULL)
			}
			nc <- nc_open(biasFile)
			lon.bias <- nc$dim[[1]]$vals
			lat.bias <- nc$dim[[2]]$vals
			data.bias <- ncvar_get(nc, varid = "bias")
			nc_close(nc)
		}

		biasCoords <- list(lon = lon.bias, lat = lat.bias)
		biasSp <- defSpatialPixels(biasCoords)
		rfeSp <- defSpatialPixels(rfeData)
		is.regridRFE <- is.diffSpatialPixelsObj(biasSp, rfeSp, tol = 1e-07)
		if(is.regridRFE){
			rfeData <- cdt.interp.surface.grid(rfeData, biasCoords)
		}

		if(bias.method == "Quantile.Mapping"){
			xadj <- quantile.mapping.BGamma(rfeData$z, pars.stn, pars.rfe, TRUE)
		}else xadj <- rfeData$z * data.bias
		xadj[xadj < 0] <- 0

		xadj[is.na(xadj)] <- -99
		dx <- ncdim_def("Lon", "degreeE", lon.bias)
		dy <- ncdim_def("Lat", "degreeN", lat.bias)

		grd.bsadj <- ncvar_def("precip", "mm", list(dx, dy), -99, longname = "Mean Bias Adjusted RFE",
								prec = "short", shuffle = TRUE, compression = 9)
		bias.outfl <- file.path(dir2save, paste0('rr_adj', '_', daty,'.nc'))
		nc2 <- nc_create(bias.outfl, grd.bsadj)
		ncvar_put(nc2, grd.bsadj, xadj)
		nc_close(nc2)

		#########
		ncInfo <- list(dates = daty, nc.files = bias.outfl, exist = TRUE)
		ncInfo$ncinfo <- list(lon = lon.bias, lat = lat.bias, varid = 'precip', ilon = 1, ilat = 2,
							xo = seq_along(lon.bias), yo = seq_along(lat.bias),
							nx = length(lon.bias), ny = length(lat.bias))
		ncInfo$xy.rfe <- list(lon = lon.bias, lat = lat.bias)
	}

	################
	## merging
	if(!.cdtData$GalParams$STN$No.Stn.Data){
		stnData <- getStnOpenData(.cdtData$GalParams$STN$file)
		stnData <- getCDTdataAndDisplayMsg(stnData, 'dekadal', .cdtData$GalParams$STN$file)
		if(is.null(stnData)) return(NULL)

		################

		xy.grid <- ncInfo$xy.rfe
		nlon0 <- length(ncInfo$xy.rfe$lon)
		nlat0 <- length(ncInfo$xy.rfe$lat)

		##################
		## DEM data
		demData <- NULL
		if(.cdtData$GalParams$blank$blank == "2"){
			demData <- getNCDFSampleData(.cdtData$GalParams$blank$DEM.file)
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

		################

		.cdtData$GalParams$mrgParms <- list(months = as.numeric(dmon), ncInfo = ncInfo,
						stnData = stnData, demData = demData, merge.DIR = dir2save,
						interp.grid = list(grid = xy.grid, nlon = nlon0, nlat = nlat0), outMask = outMask)

		ret <- Precip_MergingFunctions()

		if(!is.null(ret)){
			if(ret != 0) return(ret) 
		}else return(NULL)
	}

	return(0)
}
