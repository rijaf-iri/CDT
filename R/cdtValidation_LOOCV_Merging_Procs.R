
LOOCV_MergingDataExec <- function(GeneralParameters){
	if(GeneralParameters$outdir %in% c("", "NA")){
		Insert.Messages.Out("Directory to save results doesn't exist", format = TRUE)
		return(NULL)
	}

	outValidation <- file.path(GeneralParameters$outdir, paste0('LOOCVALIDATION_',
								file_path_sans_ext(GeneralParameters$STN.file)))
	dir.create(outValidation, showWarnings = FALSE, recursive = TRUE)

	freqData <- GeneralParameters$Tstep

	#######get data
	stnData <- getStnOpenData(GeneralParameters$STN.file)
	stnData <- getCDTdataAndDisplayMsg(stnData, freqData, GeneralParameters$STN.file)
	if(is.null(stnData)) return(NULL)

	##################

	ncDataInfo <- getNCDFSampleData(GeneralParameters$NCDF$sample)
	if(is.null(ncDataInfo)){
		if(GeneralParameters$clim.var == "RR")
			msg <- "No RFE or Adjusted RFE data sample found"
		else
			msg <- "No downscaled or Adjusted reanalysis data sample found"
		Insert.Messages.Out(msg, format = TRUE)
		return(NULL)
	}

	##################
	## Grid for interpolation
	xy.grid <- ncDataInfo[c('lon', 'lat')]
	nlon0 <- length(ncDataInfo$lon)
	nlat0 <- length(ncDataInfo$lat)

	##################
	## DEM data
	demData <- NULL
	if(GeneralParameters$auxvar$dem |
		GeneralParameters$auxvar$slope |
		GeneralParameters$auxvar$aspect)
	{
		demData <- getNCDFSampleData(GeneralParameters$DEM.file)
		if(is.null(demData)){
			Insert.Messages.Out("No elevation data found", format = TRUE)
			return(NULL)
		}
		jfile <- getIndex.AllOpenFiles(GeneralParameters$DEM.file)
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
	start.year <- GeneralParameters$Merging.Date$start.year
	start.mon <- GeneralParameters$Merging.Date$start.mon
	start.dek <- GeneralParameters$Merging.Date$start.day
	end.year <- GeneralParameters$Merging.Date$end.year
	end.mon <- GeneralParameters$Merging.Date$end.mon
	end.dek <- GeneralParameters$Merging.Date$end.day
	months <- GeneralParameters$Merging.Date$Months
	start.date <- as.Date(paste(start.year, start.mon, start.dek, sep = '/'), format = '%Y/%m/%d')
	end.date <- as.Date(paste(end.year, end.mon, end.dek, sep = '/'), format = '%Y/%m/%d')

	NCDF.DIR <- GeneralParameters$NCDF$dir
	NCDF.Format <- GeneralParameters$NCDF$format

	##################
	if(GeneralParameters$clim.var == "RR")
		errmsg <- "No RFE or Adjusted RFE data sample found"
	else
		errmsg <- "No downscaled or Adjusted reanalysis data sample found"
	ncInfo <- ncFilesInfo(freqData, start.date, end.date, months, NCDF.DIR, NCDF.Format, errmsg)
	if(is.null(ncInfo)) return(NULL)
	ncInfo$ncinfo <- ncDataInfo
	if(GeneralParameters$clim.var == "RR")
		ncInfo$xy.rfe <- ncDataInfo[c('lon', 'lat')]

	mrgParms <- list(months = months, ncInfo = ncInfo, GeneralParameters = GeneralParameters,
				stnData = stnData, demData = demData, outdir = outValidation,
				interp.grid = list(grid = xy.grid, nlon = nlon0, nlat = nlat0))

	ret <- LOOCV_MergingDataProcs(mrgParms)

	rm(stnData, ncInfo, demData, ncDataInfo)

	if(!is.null(ret)){
		if(ret == 0) return(0)
		else return(ret)
	} else return(NULL)
}

########################################################################################################

LOOCV_MergingDataProcs <- function(mrgParms){
	Insert.Messages.Out('Leave-one-out cross-validation ...')

	GeneralParameters <- mrgParms$GeneralParameters
	freqData <- GeneralParameters$Tstep

	log.file <- file.path(mrgParms$outdir, "log_file.txt")

	#############
	xy.grid <- mrgParms$interp.grid$grid
	newgrid <- defSpatialPixels(xy.grid)

	#############
	mrg.method <- GeneralParameters$Merging$mrg.method
	interp.method <- GeneralParameters$Merging$interp.method
	pixelize.station <- GeneralParameters$Merging$pixelize.station

	nmin <- GeneralParameters$Merging$nmin
	nmax <- GeneralParameters$Merging$nmax
	maxdist <- GeneralParameters$Merging$maxdist
	vgm.model <- GeneralParameters$Merging$vgm.model
	min.stn <- GeneralParameters$Merging$min.stn

	if(GeneralParameters$clim.var == "RR") pars.RnoR <- GeneralParameters$RnoR

	#############
	lon.stn <- mrgParms$stnData$lon
	lat.stn <- mrgParms$stnData$lat
	date.stn <- mrgParms$stnData$dates
	data.stn <- mrgParms$stnData$data
	nstn <- length(lon.stn)
	demData <- mrgParms$demData

	#############

	auxvar <- c('dem', 'slp', 'asp', 'alon', 'alat')
	is.auxvar <- unlist(GeneralParameters$auxvar)

	formuleRK <- formula(paste0('stn', '~', 'grd'))
	formule <- formula(paste0('res', '~', 1))
	if(any(is.auxvar)){
		formule <- formula(paste0('res', '~', paste(auxvar[is.auxvar], collapse = '+')))
		if(mrg.method == "Regression Kriging" & GeneralParameters$Merging$sp.trend.aux)
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
		coefFiles <- file.path(GeneralParameters$LMCOEF$dir.LMCoef, sprintf(GeneralParameters$LMCOEF$format, months))
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

	ncInfo <- mrgParms$ncInfo

	if(GeneralParameters$clim.var == "RR"){
		rfeSp <- defSpatialPixels(ncInfo$xy.rfe)
		is.regridRFE <- is.diffSpatialPixelsObj(newgrid, rfeSp, tol = 1e-07)
	}

	xlon <- ncInfo$ncinfo$lon
	xlat <- ncInfo$ncinfo$lat
	ijGrd <- grid2pointINDEX(list(lon = lon.stn, lat = lat.stn), list(lon = xlon, lat = xlat))
	ijStn <- seq(nstn)

	#############

	ret.out.mrg <- cdt.foreach(seq_along(ncInfo$nc.files),
						parsL = length(which(ncInfo$exist)) >= 20,
						.packages = c('sp', 'ncdf4'), FUN = function(jj)
	{
		valid.out <- rep(NA, nstn)

		############

		if(ncInfo$exist[jj]){
			nc <- nc_open(ncInfo$nc.files[jj])
			xtmp <- ncvar_get(nc, varid = ncInfo$ncinfo$varid)
			nc_close(nc)
			xtmp <- transposeNCDFData(xtmp, ncInfo$ncinfo)
		}else{
			xmsg <- if(GeneralParameters$clim.var == "RR") "no RFE data" else "no temperature data"
			xmsg <- paste(ncInfo$dates[jj], ":", xmsg, "|", "no data generated", "\n")
			cat(xmsg, file = log.file, append = TRUE)
			return(NULL)
		}

		if(all(is.na(xtmp))){
			xmsg <- if(GeneralParameters$clim.var == "RR") "all RFE data are missing" else "all gridded data are missing"
			xmsg <- paste(ncInfo$dates[jj], ":", xmsg, "|", "no data generated", "\n")
			cat(xmsg, file = log.file, append = TRUE)
			return(NULL)
		}

		############
		if(GeneralParameters$clim.var == "RR" & is.regridRFE){
			rfeGrid <- cdt.interp.surface.grid(list(lon = xlon, lat = xlat, z = xtmp), xy.grid)
			xtmp <- rfeGrid$z
		}

		############

		newgrid$grd <- c(xtmp)

		donne.stn <- data.stn[date.stn == ncInfo$dates[jj], , drop = FALSE]
		if(nrow(donne.stn) == 0){
			xmsg <- paste(ncInfo$dates[jj], ":", "no station data", "|", "no data generated", "\n")
			cat(xmsg, file = log.file, append = TRUE)
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
			xmsg <- paste(ncInfo$dates[jj], ":", "not enough station data", "|", "no data generated", "\n")
			cat(xmsg, file = log.file, append = TRUE)
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
		ijn <- if(pixelize.station) ijStn[noNA[ijGrd]] else ijStn[noNA]

		locations.df <- as.data.frame(!is.na(locations.stn@data))
		loc.df <- Reduce("&", locations.df)
		locations.stn <- locations.stn[loc.df, ]
		ijn <- ijn[loc.df]

		############

		if(length(locations.stn) < min.stn){
			xmsg <- paste(ncInfo$dates[jj], ":", "not enough data", "|", "no data generated", "\n")
			cat(xmsg, file = log.file, append = TRUE)
			return(NULL)
		}

		############

		for(istn in seq_along(locations.stn)){
			loc.stn <- locations.stn[-istn, ]
			out.mrg <- cdt.merging.functions(loc.stn, newgrid, 
											mrg.method, interp.method,
											formule, formuleRK,
											maxdist, nmin, nmax, vgm.model,
											FALSE, ncInfo$dates[jj], MODEL.COEF,
											ijGrd, log.file)
			if(GeneralParameters$clim.var == "RR" & pars.RnoR$use.RnoR){
				rnr <- rain_no_rain.mask(loc.stn, newgrid, pars.RnoR)
				out.mrg <- out.mrg * rnr
			}

			valid.out[ijn[istn]] <- out.mrg[ijGrd[ijn[istn]]]
		}

		return(valid.out)
	})

	#############

	idaty <- date.stn %in% ncInfo$dates

	.cdtData$EnvData$cdtData$info$id <- mrgParms$stnData$id
	.cdtData$EnvData$cdtData$info$lon <- mrgParms$stnData$lon
	.cdtData$EnvData$cdtData$info$lat <- mrgParms$stnData$lat
	.cdtData$EnvData$cdtData$dates <- date.stn[idaty]
	.cdtData$EnvData$cdtData$obs <- data.stn[idaty, , drop = FALSE]
	.cdtData$EnvData$cdtData$fcst <- round(do.call(rbind, ret.out.mrg), 1)
	.cdtData$EnvData$GeneralParameters <- GeneralParameters

	xhead <- cbind(c("STN", "LON", "DATE/LAT"), rbind(.cdtData$EnvData$cdtData$info$id,
					.cdtData$EnvData$cdtData$info$lon, .cdtData$EnvData$cdtData$info$lat))
	obs2file <- rbind(xhead, cbind(.cdtData$EnvData$cdtData$dates, .cdtData$EnvData$cdtData$obs))
	fcst2file <- rbind(xhead, cbind(.cdtData$EnvData$cdtData$dates, .cdtData$EnvData$cdtData$fcst))
	obs2file[is.na(obs2file)] <- -99
	fcst2file[is.na(fcst2file)] <- -99

	dirCDTdata <- file.path(mrgParms$outdir, "OBS_GRD_DATA")
	dir.create(dirCDTdata, showWarnings = FALSE, recursive = TRUE)
	writeFiles(obs2file, file.path(dirCDTdata, "Observations.csv"))
	writeFiles(fcst2file, file.path(dirCDTdata, "Gridded_at_Obs_Loc.csv"))

	fileValidOut <- file.path(mrgParms$outdir, "VALIDATION_DATA_OUT.rds")

	loocv.data <- .cdtData$EnvData[c('GeneralParameters', 'cdtData')]
	saveRDS(loocv.data, file = fileValidOut)

	Insert.Messages.Out('Data extraction finished')
	return(0)
}
