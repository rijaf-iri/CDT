
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

	#############
	xy.grid <- mrgParms$interp.grid$grid
	grdSp <- defSpatialPixels(xy.grid)
	nlon0 <- mrgParms$interp.grid$nlon
	nlat0 <- mrgParms$interp.grid$nlat

	#############
	mrg.method <- GeneralParameters$Merging$mrg.method
	interp.method <- GeneralParameters$Merging$interp.method
	nmin <- GeneralParameters$Merging$nmin
	nmax <- GeneralParameters$Merging$nmax
	maxdist <- GeneralParameters$Merging$maxdist
	vgm.model <- GeneralParameters$Merging$vgm.model
	min.stn <- GeneralParameters$Merging$min.stn

	if(GeneralParameters$clim.var == "RR"){
		min.non.zero <- GeneralParameters$Merging$min.non.zero
		use.RnoR <- GeneralParameters$RnoR$use.RnoR
		maxdist.RnoR <- GeneralParameters$RnoR$maxdist.RnoR
		smooth.RnoR <- GeneralParameters$RnoR$smooth.RnoR
		wet.day <- GeneralParameters$RnoR$wet.day
	}

	#############
	lon.stn <- mrgParms$stnData$lon
	lat.stn <- mrgParms$stnData$lat
	date.stn <- mrgParms$stnData$dates
	data.stn <- mrgParms$stnData$data
	nstn <- length(lon.stn)
	demData <- mrgParms$demData

	#############

	res.lon <- diff(range(xy.grid$lon)) / (nlon0 - 1)
	res.lat <- diff(range(xy.grid$lat)) / (nlat0 - 1)
	res.latlon <- max(res.lon, res.lat)

	if(maxdist < res.latlon) maxdist <- res.latlon * sqrt(2)
	res.coarse <- maxdist / sqrt(2)
	if(res.coarse < res.latlon) res.coarse <- maxdist

	if(!is.null(demData)){
		slpasp <- raster.slope.aspect(demData)
		demData$slp <- slpasp$slope
		demData$asp <- slpasp$aspect
	}else{
		demData <- list(x = xy.grid$lon, y = xy.grid$lat,
						z = matrix(1, nlon0, nlat0),
						slp = matrix(0, nlon0, nlat0),
						asp = matrix(0, nlon0, nlat0))
	}

	interp.grid <- createGrid.merging(demData, res.coarse = res.coarse)
	cells <- SpatialPixels(points = interp.grid$newgrid, tolerance = sqrt(sqrt(.Machine$double.eps)))@grid
	bGrd <- if(GeneralParameters$Merging$use.block) createBlock(cells@cellsize, 1, 5) else NULL

	#############
	auxvar <- c('dem', 'slp', 'asp', 'alon', 'alat')
	is.auxvar <- unlist(GeneralParameters$auxvar)
	if(any(is.auxvar)){
		formule <- formula(paste0('res', '~', paste(auxvar[is.auxvar], collapse = '+')))
		if(mrg.method == "Regression Kriging"){
			sp.trend.aux <- GeneralParameters$Merging$sp.trend.aux
			if(sp.trend.aux)
				formuleRK <- formula(paste0('stn', '~', 'tmp', '+', paste(auxvar[is.auxvar], collapse = '+')))
			else
				formuleRK <- formula(paste0('stn', '~', 'tmp'))
		}
	}else{
		formule <- formula(paste0('res', '~', 1))
		if(mrg.method == "Regression Kriging")
			formuleRK <- formula(paste0('stn', '~', 'tmp'))
	}

	#############
	if(is.auxvar['lon']){
		interp.grid$coords.coarse$alon <- interp.grid$coords.coarse@coords[, 'lon']
		if(!is.null(interp.grid$coords.var)) interp.grid$coords.var$alon <- interp.grid$coords.var@coords[, 'lon']
		interp.grid$newgrid$alon <- interp.grid$newgrid@coords[, 'lon']
	}

	if(is.auxvar['lat']){
		interp.grid$coords.coarse$alat <- interp.grid$coords.coarse@coords[, 'lat']
		if(!is.null(interp.grid$coords.var)) interp.grid$coords.var$alat <- interp.grid$coords.var@coords[, 'lat']
		interp.grid$newgrid$alat <- interp.grid$newgrid@coords[, 'lat']
	}

	#############

	months <- mrgParms$months
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
		is.regridRFE <- is.diffSpatialPixelsObj(grdSp, rfeSp, tol = 1e-07)
	}

	xlon <- ncInfo$ncinfo$lon
	xlat <- ncInfo$ncinfo$lat
	ijGrd <- grid2pointINDEX(list(lon = lon.stn, lat = lat.stn), list(lon = xlon, lat = xlat))

	#############

	ret.out.mrg <- cdt.foreach(seq_along(ncInfo$nc.files),
						parsL = length(which(ncInfo$exist)) >= 20,
						.packages = c('sp', 'ncdf4'), FUN = function(jj)
	{
		valid.out <- rep(NA, ncol(data.stn))

		if(ncInfo$exist[jj]){
			nc <- nc_open(ncInfo$nc.files[jj])
			xtmp <- ncvar_get(nc, varid = ncInfo$ncinfo$varid)
			nc_close(nc)
			xtmp <- transposeNCDFData(xtmp, ncInfo$ncinfo)
		}else return(valid.out)

		if(all(is.na(xtmp))) return(valid.out)

		############
		if(GeneralParameters$clim.var == "RR" & is.regridRFE){
			rfeGrid <- cdt.interp.surface.grid(list(lon = xlon, lat = xlat, z = xtmp), xy.grid)
			xtmp <- rfeGrid$z
		}

		############

		donne.stn <- data.stn[date.stn == ncInfo$dates[jj], , drop = FALSE]
		if(nrow(donne.stn) == 0){
			valid.out <- xtmp[ijGrd]
			return(valid.out)
		}
		donne.stn <- data.frame(lon = lon.stn, lat = lat.stn, stn = c(donne.stn))
		ina.stn <- which(!is.na(donne.stn$stn))
		donne.stn <- donne.stn[ina.stn, , drop = FALSE]
		if(nrow(donne.stn) < min.stn){
			valid.out <- xtmp[ijGrd]
			return(valid.out)
		}

		############

		ret.out.stn <- lapply(seq_along(ina.stn), function(istn){
			locations.stn <- interp.grid$newgrid[ijGrd[ina.stn], ]
			locations.stn@coords <- as.matrix(donne.stn[, c("lon", "lat")])
			locations.stn$stn <- donne.stn$stn
			locations.stn$tmp <- xtmp[ijGrd[ina.stn]]

			############
			xadd <- interp.grid$coords.coarse
			xadd$stn <- xadd$tmp <- c(xtmp[interp.grid$id.coarse$ix, interp.grid$id.coarse$iy])
			xadd$res <- 0

			############
			interp.grid$newgrid$tmp <- c(xtmp)
			newdata <- locations.stn[istn, ]
			newdata <- newdata[, !names(newdata) %in% "stn"]
			locations.stn <- locations.stn[-istn, ]

			############
			if(all(is.na(locations.stn$tmp))) return(NA)

			############
			is.do.RK <- TRUE
			if(GeneralParameters$clim.var == "RR"){
				nb.stn.nonZero <- length(which(locations.stn$stn > 0))
				if(nb.stn.nonZero < min.non.zero) is.do.RK <- FALSE
			}

			############
			# spatial trend
			sp.trend <- xtmp
			locations.stn$res <- locations.stn$stn - locations.stn$tmp

			if(is.do.RK){
				if(mrg.method == "Spatio-Temporal LM"){
					mo <- as(substr(ncInfo$dates[jj], 5, 6), 'numeric')
					sp.trend <- xtmp * MODEL.COEF[[mo]]$slope + MODEL.COEF[[mo]]$intercept
					locations.stn$res <- locations.stn$stn - sp.trend[ijGrd[ina.stn]][-istn]
				}
				if(mrg.method == "Regression Kriging"){
					simplediff <- if(var(locations.stn$stn) < 1e-07 | var(locations.stn$tmp, na.rm = TRUE) < 1e-07) TRUE else FALSE
					glm.stn <- glm(formuleRK, data = locations.stn, family = gaussian)
					if(is.na(glm.stn$coefficients[2]) | glm.stn$coefficients[2] < 0) simplediff <- TRUE

					if(!simplediff){
						sp.trend <- predict(glm.stn, newdata = interp.grid$newgrid)
						sp.trend <- matrix(sp.trend, ncol = nlat0, nrow = nlon0)
						ina.trend <- is.na(sp.trend)
						sp.trend[ina.trend] <- xtmp[ina.trend]
						locations.stn$res <- NA
						if(length(glm.stn$na.action) > 0)
							locations.stn$res[-glm.stn$na.action] <- glm.stn$residuals
						else
							locations.stn$res <- glm.stn$residuals
					}
				}
			}

			sp.trend <- sp.trend[ijGrd[ina.stn]][istn]

			############

			locations.stn <- locations.stn[!is.na(locations.stn$res), ]
			if(length(locations.stn) < min.stn){
				v.out <- xtmp[ijGrd[ina.stn]][istn]
				return(v.out)
			}

			############
			if(any(is.auxvar)){
				locations.df <- as.data.frame(!is.na(locations.stn@data[, auxvar[is.auxvar]]))
				locations.stn <- locations.stn[Reduce("&", locations.df), ]

				if(length(locations.stn) < min.stn){
					v.out <- xtmp[ijGrd[ina.stn]][istn]
					return(v.out)
				}
			}

			############
			if(interp.method == 'Kriging'){
				if(length(locations.stn$res) > 7){
					vgm <- try(automap::autofitVariogram(formule, input_data = locations.stn, model = vgm.model, cressie = TRUE), silent = TRUE)
					vgm <- if(!inherits(vgm, "try-error")) vgm$var_model else NULL
				}else vgm <- NULL
			}else vgm <- NULL

			############
			# create buffer for stations
			buffer.xaddin <- rgeos::gBuffer(locations.stn, width = maxdist / 2)

			############
			# get coarse grid to add to location.stn
			xadd.in <- !as.logical(over(xadd, buffer.xaddin))
			xadd.in[is.na(xadd.in)] <- TRUE
			xadd <- xadd[xadd.in, ]

			############
			## add coarse grid
			row.names(locations.stn) <- 1:length(locations.stn)
			row.names(xadd) <- length(locations.stn) + (1:length(xadd))
			locations.stn <- maptools::spRbind(locations.stn, xadd)

			###########
			if(any(is.auxvar)){
				locations.df <- as.data.frame(!is.na(locations.stn@data[, auxvar[is.auxvar]]))
				locations.stn <- locations.stn[Reduce("&", locations.df), ]
				block <- NULL
			}else block <- bGrd

			###########
			# interpolate residual
			res.grd <- gstat::krige(formule, locations = locations.stn, newdata = newdata, model = vgm,
							block = block, nmin = nmin, nmax = nmax, maxdist = maxdist, debug.level = 0)

			out.mrg <- sp.trend + res.grd$var1.pred
			if(GeneralParameters$clim.var == "RR")
				if(out.mrg < 0) out.mrg <- 0

			###########
			# Rain-no-Rain
			if(GeneralParameters$clim.var == "RR" & use.RnoR){
				locations.stn <- interp.grid$newgrid[ijGrd[ina.stn], ]
				locations.stn@coords <- as.matrix(donne.stn[, c("lon", "lat")])
				locations.stn$rnr.stn <- ifelse(donne.stn$stn < wet.day + 0.001, 0, 1)
				locations.stn$rnr.rfe <- ifelse(xtmp[ijGrd[ina.stn]] < wet.day + 0.001, 0, 1)
				locations.stn <- locations.stn[, c('rnr.stn', 'rnr.rfe')]
				locations.stn <- locations.stn[!is.na(locations.stn$rnr.stn) & !is.na(locations.stn$rnr.rfe), ]

				if(length(locations.stn) < min.stn) return(out.mrg)

				###########
				rnr.rfe <- ifelse(xtmp < wet.day + 0.001, 0, 1)
				xadd <- interp.grid$coords.coarse
				xadd$rnr.stn <- xadd$rnr.rfe <- c(rnr.rfe[interp.grid$id.coarse$ix, interp.grid$id.coarse$iy])
				xadd <- xadd[, c('rnr.stn', 'rnr.rfe')]
				xadd$rnr.res <- 0

				###########
				newdata.glm <- locations.stn[istn, ]
				newdata.glm <- newdata.glm[, !names(newdata.glm) %in% "rnr.stn"]
				locations.stn <- locations.stn[-istn, ]

				###########
				# binomial logistic regression
				glm.binom <- glm(rnr.stn ~ rnr.rfe, data = locations.stn, family = binomial(link = "logit"))
				locations.stn$rnr.res <- residuals(glm.binom)
				rnr <- predict(glm.binom, newdata = newdata.glm, type = 'link')

				###########
				buffer.rnor.in <- rgeos::gBuffer(locations.stn, width = maxdist.RnoR / 2)

				############
				# get coarse grid to add to location.stn
				xadd.in <- !as.logical(over(xadd, buffer.rnor.in))
				xadd.in[is.na(xadd.in)] <- TRUE
				xadd <- xadd[xadd.in, ]

				###########
				row.names(locations.stn) <- 1:length(locations.stn)
				row.names(xadd) <- length(locations.stn) + (1:length(xadd))
				locations.stn <- maptools::spRbind(locations.stn, xadd)

				###########
				rnr.res.grd <- gstat::krige(rnr.res~1, locations = locations.stn, newdata = newdata.glm, maxdist = maxdist.RnoR, debug.level = 0)

				rnr0 <- rnr.res.grd$var1.pred
				rnr0[is.na(rnr0)] <- 0

				rnr <- rnr + rnr0
				rnr <- exp(rnr) / (1 + exp(rnr))
				### decision boundary 0.5
				rnr[rnr >= 0.5] <- 1
				rnr[rnr < 0.5] <- 0

				out.mrg <- out.mrg * rnr
			}

			return(out.mrg)
		})

		ret.out.stn <- do.call(c, ret.out.stn)
		valid.out[ina.stn] <- ret.out.stn
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
