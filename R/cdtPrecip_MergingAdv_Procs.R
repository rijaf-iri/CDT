
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
	grdSp <- defSpatialPixels(xy.grid)
	nlon0 <- mrgParms$interp.grid$nlon
	nlat0 <- mrgParms$interp.grid$nlat

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
	local.interpolation <- .cdtData$GalParams$Merging$local.interpolation
	pixelize.station <- .cdtData$GalParams$Merging$pixelize.station

	nmin <- .cdtData$GalParams$Merging$nmin
	nmax <- .cdtData$GalParams$Merging$nmax
	maxdist <- .cdtData$GalParams$Merging$maxdist
	vgm.model <- .cdtData$GalParams$Merging$vgm.model
	min.stn <- .cdtData$GalParams$Merging$min.stn
	min.non.zero <- .cdtData$GalParams$Merging$min.non.zero

	use.RnoR <- .cdtData$GalParams$RnoR$use.RnoR
	maxdist.RnoR <- .cdtData$GalParams$RnoR$maxdist.RnoR
	smooth.RnoR <- .cdtData$GalParams$RnoR$smooth.RnoR
	wet.day <- .cdtData$GalParams$RnoR$wet.day

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

	if(local.interpolation){
		if(maxdist < res.latlon) maxdist <- res.latlon * sqrt(2)
		res.coarse <- maxdist / sqrt(2)
		if(res.coarse < res.latlon) res.coarse <- maxdist
	}else{
		maxdist <- res.latlon * 10
		res.coarse <- maxdist / sqrt(2)
	}

	if(maxdist < maxdist.RnoR) maxdist.RnoR <- maxdist

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
	bGrd <- if(.cdtData$GalParams$Merging$use.block) createBlock(cells@cellsize, 1, 5) else NULL

	#############
	auxvar <- c('dem', 'slp', 'asp', 'alon', 'alat')
	is.auxvar <- unlist(.cdtData$GalParams$auxvar)
	if(any(is.auxvar)){
		formule <- formula(paste0('res', '~', paste(auxvar[is.auxvar], collapse = '+')))
		if(mrg.method == "Regression Kriging"){
			sp.trend.aux <- .cdtData$GalParams$Merging$sp.trend.aux
			if(sp.trend.aux)
				formuleRK <- formula(paste0('stn', '~', 'rfe', '+', paste(auxvar[is.auxvar], collapse = '+')))
			else
				formuleRK <- formula(paste0('stn', '~', 'rfe'))
		}
	}else{
		formule <- formula(paste0('res', '~', 1))
		if(mrg.method == "Regression Kriging")
			formuleRK <- formula(paste0('stn', '~', 'rfe'))
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
	is.regridRFE <- is.diffSpatialPixelsObj(grdSp, rfeSp, tol = 1e-07)

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
			xrfe <- ncvar_get(nc, varid = ncInfo$ncinfo$varid)
			nc_close(nc)
			xrfe <- transposeNCDFData(xrfe, ncInfo$ncinfo)
		}else{
			cat(paste(ncInfo$dates[jj], ":", "no RFE data", "|", "no file generated", "\n"),
				file = log.file, append = TRUE)
			return(NULL)
		}

		if(all(is.na(xrfe))){
			cat(paste(ncInfo$dates[jj], ":", "all RFE data are missing", "|", "no file generated", "\n"),
				file = log.file, append = TRUE)
			return(NULL)
		}

		############
		if(is.regridRFE){
			rfeGrid <- cdt.interp.surface.grid(list(lon = xlon, lat = xlat, z = xrfe), xy.grid)
			xrfe <- rfeGrid$z
		}

		############

		donne.stn <- data.stn[date.stn == ncInfo$dates[jj], , drop = FALSE]
		if(nrow(donne.stn) == 0){
			writeNC.merging(xrfe, ncInfo$dates[jj], freqData, grd.nc.out,
					mrgParms$merge.DIR, GalParams$output$format)
			cat(paste(ncInfo$dates[jj], ":", "no station data", "|", "RFE data", "\n"), file = log.file, append = TRUE)
			return(NULL)
		}

		donne.stn <- data.frame(lon = lon.stn, lat = lat.stn, stn = c(donne.stn))
		if(pixelize.station){
			stng <- createGrid.StnData(donne.stn, ijGrd, interp.grid$newgrid, min.stn, weighted = TRUE)
		}else{
			stng <- donne.stn$stn
			if(length(stng[!is.na(stng)]) < min.stn) stng <- NULL
		}

		if(is.null(stng)){
			writeNC.merging(xrfe, ncInfo$dates[jj], freqData, grd.nc.out,
					mrgParms$merge.DIR, GalParams$output$format)
			cat(paste(ncInfo$dates[jj], ":", "not enough station data", "|", "RFE data", "\n"),
				file = log.file, append = TRUE)
			return(NULL)
		}

		############
		if(pixelize.station){
			locations.stn <- interp.grid$newgrid
			locations.stn$stn <- stng
			locations.stn$rfe <- c(xrfe)
		}else{
			locations.stn <- interp.grid$newgrid[ijGrd, ]
			locations.stn@coords <- as.matrix(donne.stn[, c("lon", "lat")])
			locations.stn$stn <- stng
			locations.stn$rfe <- xrfe[ijGrd]
			if(is.auxvar['lon']) locations.stn$alon <- donne.stn$lon
			if(is.auxvar['lat']) locations.stn$alat <- donne.stn$lat
		}

		############
		xadd <- interp.grid$coords.coarse
		xadd$rfe <- xadd$stn <- c(xrfe[interp.grid$id.coarse$ix, interp.grid$id.coarse$iy])
		xadd$res <- 0

		############
		interp.grid$newgrid$rfe <- c(xrfe)
		newdata <- interp.grid$newgrid

		############
		noNA <- !is.na(locations.stn$stn)
		nb.stn.nonZero <- length(which(noNA & locations.stn$stn > 0))
		locations.stn <- locations.stn[noNA, ]

		############
		if(all(is.na(locations.stn$rfe))){
			writeNC.merging(xrfe, ncInfo$dates[jj], freqData, grd.nc.out,
					mrgParms$merge.DIR, GalParams$output$format)
			cat(paste(ncInfo$dates[jj], ":", "all RFE data @ station location are missing", "|", "RFE data", "\n"),
				file = log.file, append = TRUE)
			return(NULL)
		}

		############
		# spatial trend
		sp.trend <- xrfe
		locations.stn$res <- locations.stn$stn - locations.stn$rfe
		rfe.outside <- FALSE

		if(nb.stn.nonZero >= min.non.zero){
			if(mrg.method == "Spatio-Temporal LM"){
				mo <- as(substr(ncInfo$dates[jj], 5, 6), 'numeric')
				sp.trend <- xrfe * MODEL.COEF[[mo]]$slope + MODEL.COEF[[mo]]$intercept
				sp.trend[sp.trend < 0] <- 0
				locations.stn$res <- locations.stn$stn - sp.trend[noNA]
			}
			if(mrg.method == "Regression Kriging"){
				simplediff <- if(var(locations.stn$stn) < 1e-07 | var(locations.stn$rfe, na.rm = TRUE) < 1e-07) TRUE else FALSE
				if(simplediff){
					cat(paste(ncInfo$dates[jj], ":", "Zero variance", "|", "Simple Bias Adjustment", "\n"),
						file = log.file, append = TRUE)
				}

				glm.stn <- glm(formuleRK, data = locations.stn, family = gaussian)
				if(is.na(glm.stn$coefficients[2]) | glm.stn$coefficients[2] < 0){
					simplediff <- TRUE
					cat(paste(ncInfo$dates[jj], ":", "Invalid GLM coeffs", "|", "Simple Bias Adjustment", "\n"),
						file = log.file, append = TRUE)
				}
				if(!simplediff){
					sp.trend <- predict(glm.stn, newdata = interp.grid$newgrid)
					sp.trend <- matrix(sp.trend, ncol = nlat0, nrow = nlon0)
					ina.trend <- is.na(sp.trend)
					sp.trend[ina.trend] <- xrfe[ina.trend]
					sp.trend[sp.trend < 0] <- 0
					locations.stn$res <- NA
					if(length(glm.stn$na.action) > 0)
						locations.stn$res[-glm.stn$na.action] <- glm.stn$residuals
					else
						locations.stn$res <- glm.stn$residuals
					rfe.outside <- TRUE
				}
			}
		}else{
			cat(paste(ncInfo$dates[jj], ":", paste("Too much zero >", min.non.zero), "|", "Simple Bias Adjustment", "\n"),
				file = log.file, append = TRUE)
		}

		############
		locations.stn <- locations.stn[!is.na(locations.stn$res), ]

		if(length(locations.stn) < min.stn){
			writeNC.merging(xrfe, ncInfo$dates[jj], freqData, grd.nc.out,
					mrgParms$merge.DIR, GalParams$output$format)
			cat(paste(ncInfo$dates[jj], ":", "not enough station data", "|", "RFE data", "\n"),
				file = log.file, append = TRUE)
			return(NULL)
		}

		############
		if(any(is.auxvar)){
			locations.df <- as.data.frame(!is.na(locations.stn@data[, auxvar[is.auxvar]]))
			locations.stn <- locations.stn[Reduce("&", locations.df), ]

			if(length(locations.stn) < min.stn){
				writeNC.merging(xrfe, ncInfo$dates[jj], freqData, grd.nc.out,
						mrgParms$merge.DIR, GalParams$output$format)
				cat(paste(ncInfo$dates[jj], ":", "not enough station data combined with auxiliary var", "|",
						"RFE data", "\n"), file = log.file, append = TRUE)
				return(NULL)
			}
		}

		############
		if(interp.method == 'Kriging'){
			if(length(locations.stn$res) > 7){
				vgm <- try(automap::autofitVariogram(formule, input_data = locations.stn, model = vgm.model, cressie = TRUE), silent = TRUE)
				vgm <- if(!inherits(vgm, "try-error")) vgm$var_model else NULL
			}else{
				cat(paste(ncInfo$dates[jj], ":", "Unable to compute variogram", "|", "Interpolation using IDW", "\n"),
					file = log.file, append = TRUE)
				vgm <- NULL
			}
		}else vgm <- NULL

		############
		# create buffer for stations
		buffer.ina <- rgeos::gBuffer(locations.stn, width = maxdist)
		buffer.grid <- rgeos::gBuffer(locations.stn, width = maxdist * 1.5)
		buffer.xaddin <- rgeos::gBuffer(locations.stn, width = maxdist * 0.75)
		buffer.xaddout <- rgeos::gBuffer(locations.stn, width = maxdist * 2.5)

		############
		## inner interpolation grid
		igrid <- as.logical(over(newdata, buffer.grid))
		igrid[is.na(igrid)] <- FALSE
		newdata0 <- newdata[igrid, ]

		############
		# get coarse grid to add to location.stn
		xadd.in <- !as.logical(over(xadd, buffer.xaddin))
		xadd.in[is.na(xadd.in)] <- TRUE
		xadd.out <- as.logical(over(xadd, buffer.xaddout))
		xadd.out[is.na(xadd.out)] <- FALSE
		iadd <- xadd.in & xadd.out
		xadd <- xadd[iadd, ]

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
		if(local.interpolation){
			res.grd <- gstat::krige(formule, locations = locations.stn, newdata = newdata0, model = vgm,
							block = block, nmin = nmin, nmax = nmax, maxdist = maxdist, debug.level = 0)
			# fill missing inside interpolation buffer.ina
			inside <- as.logical(over(res.grd, buffer.ina))
			inside[is.na(inside)] <- FALSE
			ina <- is.na(res.grd$var1.pred) & inside
			if(any(ina)){
				tmp.res.grd <- res.grd[!ina, ]
				tmp.res.grd <- tmp.res.grd[!is.na(tmp.res.grd$var1.pred), ]
				res.grd.na <- gstat::krige(var1.pred~1, locations = tmp.res.grd, newdata = newdata0[ina, ], model = vgm,
										block = bGrd, nmin = nmin, nmax = nmax, maxdist = maxdist, debug.level = 0)
				res.grd$var1.pred[ina] <- res.grd.na$var1.pred
				rm(tmp.res.grd, res.grd.na)
			}
		}else{
			res.grd <- gstat::krige(formule, locations = locations.stn, newdata = newdata0, model = vgm,
									block = block, debug.level = 0)
		}

		###########
		# remove extreme residuals outside station range
		xtrm <- range(locations.stn$res, na.rm = TRUE)
		extrm1 <- xtrm[1] - diff(xtrm) * 0.05
		extrm2 <- xtrm[2] + diff(xtrm) * 0.05
		res.grd$var1.pred[!is.na(res.grd$var1.pred) & res.grd$var1.pred < extrm1] <- extrm1
		res.grd$var1.pred[!is.na(res.grd$var1.pred) & res.grd$var1.pred > extrm2] <- extrm2

		###########
		resid <- rep(0, length(newdata))
		resid[igrid] <- res.grd$var1.pred
		resid[is.na(resid)] <- 0
		resid <- matrix(resid, ncol = nlat0, nrow = nlon0)

		###########
		## "Regression Kriging"
		if(rfe.outside){
			smth.out <- as.logical(over(newdata, buffer.ina))
			smth.out[is.na(smth.out)] <- FALSE
			smth.in <- !as.logical(over(newdata, buffer.xaddin))
			smth.in[is.na(smth.in)] <- TRUE
			imout <- smth.in & igrid

			out.tmp <- xrfe
			out.tmp[smth.out] <- sp.trend[smth.out]
			out.tmp <- smooth.matrix(out.tmp, 2)

			sp.trend[!igrid] <- xrfe[!igrid]
			sp.trend[imout] <- out.tmp[imout]
			rm(smth.out, smth.in, imout, out.tmp)
		}

		###########
		out.mrg <- sp.trend + resid
		out.mrg[out.mrg < 0] <- 0

		###########

		rm(resid, sp.trend, res.grd, inside, newdata0,
			igrid, locations.stn, xadd, xadd.in, xadd.out)

		###########
		# Rain-no-Rain
		rnr <- matrix(1, ncol = nlat0, nrow = nlon0)
		if(use.RnoR){
			rnr.stn <- ifelse(stng < wet.day + 0.001, 0, 1)
			rnr.rfe <- ifelse(xrfe < wet.day + 0.001, 0, 1)

			if(pixelize.station){
				locations.stn <- interp.grid$newgrid
				locations.stn$rnr.stn <- rnr.stn
				locations.stn$rnr.rfe <- c(rnr.rfe)
				ix.stn <- !is.na(rnr.stn)
				stnRnR <- rnr.stn[ix.stn]
			}else{
				locations.stn <- interp.grid$newgrid[ijGrd, ]
				locations.stn@coords <- as.matrix(donne.stn[, c("lon", "lat")])
				locations.stn$rnr.stn <- rnr.stn
				locations.stn$rnr.rfe <- rnr.rfe[ijGrd]
				if(is.auxvar['lon']) locations.stn$alon <- donne.stn$lon
				if(is.auxvar['lat']) locations.stn$alat <- donne.stn$lat

				ix0 <- tapply(rnr.stn, ijGrd, mean, na.rm = TRUE)
				ix0[is.nan(ix0)] <- NA
				ix.stn <- as.numeric(names(ix0))
				ix.stn <- ix.stn[!is.na(ix0)]
				stnRnR <- as.numeric(ix0[!is.na(ix0)])
				stnRnR <- ifelse(stnRnR > 0, 1, 0)
			}

			locations.stn <- locations.stn[, c('rnr.stn', 'rnr.rfe')]
			locations.stn <- locations.stn[!is.na(locations.stn$rnr.stn) & !is.na(locations.stn$rnr.rfe), ]

			if(length(locations.stn) < min.stn){
				writeNC.merging(out.mrg, ncInfo$dates[jj], freqData, grd.nc.out,
						mrgParms$merge.DIR, GalParams$output$format)
				cat(paste(ncInfo$dates[jj], ":", "No rain-no-rain mask performed", "|", "Merged data", "\n"),
					file = log.file, append = TRUE)
				return(NULL)
			}

			###########
			# binomial logistic regression
			newdata.glm <- interp.grid$newgrid
			newdata.glm$rnr.rfe <- c(rnr.rfe)
			glm.binom <- glm(rnr.stn ~ rnr.rfe, data = locations.stn, family = binomial(link = "logit"))
			locations.stn$rnr.res <- residuals(glm.binom)
			rnr.trend <- predict(glm.binom, newdata = newdata.glm, type = 'link')
			rm(newdata.glm)

			###########
			## buffer
			buffer.grid <- buffer.ina
			buffer.ina <- rgeos::gBuffer(locations.stn, width = maxdist.RnoR)

			###########
			xadd <- interp.grid$coords.coarse
			xadd$rnr.rfe <- xadd$rnr.stn <- c(rnr.rfe[interp.grid$id.coarse$ix, interp.grid$id.coarse$iy])
			xadd <- xadd[, c('rnr.stn', 'rnr.rfe')]
			xadd$rnr.res <- 0
			xadd <- xadd[iadd, ]

			###########
			row.names(locations.stn) <- 1:length(locations.stn)
			row.names(xadd) <- length(locations.stn) + (1:length(xadd))
			locations.stn <- maptools::spRbind(locations.stn, xadd)

			###########
			## rnr interpolation grid
			igrid.rnr <- as.logical(over(newdata, buffer.grid))
			igrid.rnr[is.na(igrid.rnr)] <- FALSE
			newdata0.rnr <- newdata[igrid.rnr, ]

			###########
			## rnr.rfe outside maxdist.RnoR
			imsk0 <- !as.logical(over(newdata, buffer.ina))
			imsk0[is.na(imsk0)] <- TRUE
			imsk0 <- matrix(imsk0, nrow = nlon0, ncol = nlat0)

			###########
			rnr.res.grd <- gstat::krige(rnr.res~1, locations = locations.stn, newdata = newdata0.rnr, maxdist = maxdist.RnoR, debug.level = 0)
			ina <- is.na(rnr.res.grd$var1.pred)
			if(any(ina)){
				rnr.res.grd.na <- gstat::krige(var1.pred~1, locations = rnr.res.grd[!ina, ],
										newdata = newdata0.rnr[ina, ], maxdist = maxdist.RnoR, debug.level = 0)
				rnr.res.grd$var1.pred[ina] <- rnr.res.grd.na$var1.pred
			}

			rnr0 <- rep(0, length(newdata))
			rnr0[igrid.rnr] <- rnr.res.grd$var1.pred
			rnr0[is.na(rnr0)] <- 0

			rnr <- rnr.trend + rnr0
			rnr <- exp(rnr) / (1 + exp(rnr))
			### decision boundary 0.5
			rnr[rnr >= 0.5] <- 1
			rnr[rnr < 0.5] <- 0

			rnr[ix.stn] <- stnRnR
			rnr <- matrix(rnr, nrow = nlon0, ncol = nlat0)

			if(smooth.RnoR){
				rnr[imsk0] <- rnr.rfe[imsk0]
				rnr[is.na(rnr)] <- 1
				rnr <- smooth.matrix(rnr, 2)
				rnr[imsk0] <- rnr.rfe[imsk0]
			}else{
				rnr[imsk0] <- rnr.rfe[imsk0]
				rnr[is.na(rnr)] <- 1
			}

			rm(rnr0, imsk0, rnr.res.grd, newdata0.rnr, igrid.rnr, rnr.rfe,
				iadd, xadd, xadd.out, xadd.in, locations.stn, rnr.stn)
		}

		###########

		out.mrg <- out.mrg * rnr

		#Apply mask for area of interest
		if(!is.null(mrgParms$outMask)) out.mrg[is.na(mrgParms$outMask)] <- NA

		writeNC.merging(out.mrg, ncInfo$dates[jj], freqData, grd.nc.out,
				mrgParms$merge.DIR, GalParams$output$format)

		rm(out.mrg, rnr, newdata, xrfe)
		gc()
		return(0)
	})

	Insert.Messages.Out('Merging finished')
	return(0)
}


