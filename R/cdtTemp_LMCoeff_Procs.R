
execLMCoefTemp <- function(){
	origdir <- file.path(.cdtData$GalParams$output$dir, paste0('LMCOEF_Data_',
						file_path_sans_ext(.cdtData$GalParams$STN.file)))

	dir.create(origdir, showWarnings = FALSE, recursive = TRUE)
	Insert.Messages.Out('Computing LM Coefficients ...')

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
	if(.cdtData$GalParams$LMCOEF$interp.method == "NN" |
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
	## regrid DEM data
	if(!is.null(demData)){
		is.regridDEM <- is.diffSpatialPixelsObj(defSpatialPixels(xy.grid), defSpatialPixels(demData), tol = 1e-07)
		if(is.regridDEM)
			demData <- cdt.interp.surface.grid(demData, xy.grid)
		demData$z[demData$z < 0] <- 0
	}

	##################

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

	start.date <- as.Date(paste0(years[1], '0101'), format = '%Y%m%d')
	end.date <- as.Date(paste0(years[length(years)], '1231'), format = '%Y%m%d')
	months <- .cdtData$GalParams$LMCOEF$Months

	TMP.DIR <- .cdtData$GalParams$TEMP$dir
	TMP.Format <- .cdtData$GalParams$TEMP$format
	.cdtData$GalParams$lmCoefFilenames <- .cdtData$GalParams$output$format

	##################
	errmsg <- "No downscaled or Adjusted reanalysis data sample found"
	ncInfo <- ncFilesInfo(freqData, start.date, end.date, months, TMP.DIR, TMP.Format, errmsg)
	if(is.null(ncInfo)) return(NULL)
	ncInfo$ncinfo <- tmpDataInfo

	.cdtData$GalParams$lmCoefParms <- list(stnData = stnData,
						ncInfo = ncInfo, LMCoef.DIR = origdir, months = months,
						interp.grid = list(grid = xy.grid, nlon = nlon0, nlat = nlat0))
	model.coef <- Temp_ComputeLMCoef()
	if(is.null(model.coef)) return(NULL)

	##################
	.cdtData$GalParams$lmCoefParms <- list(model.coef = model.coef, months = months,
					stnData = stnData[c('lon', 'lat')], demData = demData, ncInfo = ncInfo, LMCoef.DIR = origdir,
					interp.grid = list(grid = xy.grid, nlon = nlon0, nlat = nlat0))

	ret <- Temp_InterpolateLMCoef()

	rm(model.coef, stnData, demData, ncInfo, tmpDataInfo)
	gc()
	if(!is.null(ret)){
		if(ret == 0) return(0)
		else return(ret)
	} else return(NULL)
}

#######################################################################################

Temp_ComputeLMCoef <- function(){
	Insert.Messages.Out('Compute LM Coefficients ...')

	lmCoefParms <- .cdtData$GalParams$lmCoefParms
	freqData <- .cdtData$GalParams$period
	months <- lmCoefParms$months
	date.stn <- lmCoefParms$stnData$dates
	data.stn <- lmCoefParms$stnData$data
	nstn <- length(lmCoefParms$stnData$lon)

	#############
	ncInfo <- lmCoefParms$ncInfo
	if(!is.null(ncInfo$data)){
		downData <- ncInfo$data
		date.down <- downData$dates
		data.down.stn <- downData$data
	}else{
		ptsData <- lmCoefParms$stnData[c('lon', 'lat')]
		msg <- list(start = 'Read Downscaled data ...', end = 'Reading Downscaled data finished')
		downData <- readNetCDFData2Points(ncInfo, ptsData, msg)
		date.down <- downData$dates
		data.down.stn <- downData$data
	}

	#############

	dtdown <- match(date.stn, date.down)
	dtdown <- dtdown[!is.na(dtdown)]
	date.down <- date.down[dtdown]
	dtstn <- date.stn %in% date.down

	if(length(dtdown) == 0){
		Insert.Messages.Out("Date out of range", format = TRUE)
		return(NULL)
	}

	data.stn <- data.stn[dtstn, , drop = FALSE]
	date.stn <- date.stn[dtstn]
	data.down.stn <- data.down.stn[dtdown, , drop = FALSE]

	#############
	year1 <- .cdtData$GalParams$LMCOEF$start.year
	year2 <- .cdtData$GalParams$LMCOEF$end.year
	min.len <- .cdtData$GalParams$LMCOEF$min.length
	useQuantile <- .cdtData$GalParams$LMCOEF$useQuantile

	month.stn <- as(substr(date.stn, 5, 6), 'numeric')
	year.stn <- as(substr(date.stn, 1, 4), 'numeric')
	iyear0 <- (year.stn >= year1 & year.stn <= year2) & (month.stn %in% months)
	data.stn.reg <- data.stn[iyear0, , drop = FALSE]
	mon.stn.reg <- month.stn[iyear0]

	#############
	index <- split(seq(length(mon.stn.reg)), mon.stn.reg)
	model.coef <- lapply(index, function(x){
		Y <- data.stn.reg[x, , drop = FALSE]
		X <- data.down.stn[x, , drop = FALSE]

		if(useQuantile){
			noNAY <- colSums(!is.na(Y))
			noNAX <- colSums(!is.na(X))
			iy <- noNAY >= min.len
			ix <- noNAX >= min.len
			if(!any(ix) | !any(iy)) return(NULL)
			Y <- Y[, ix & iy, drop = FALSE]
			X <- X[, ix & iy, drop = FALSE]
			ix <- ix & iy
			Y <- do.call(cbind, lapply(seq(ncol(Y)), function(j) quantile(ecdf(Y[, j]), seq(0, 1, 0.001))))
			X <- do.call(cbind, lapply(seq(ncol(X)), function(j) quantile(ecdf(X[, j]), seq(0, 1, 0.001))))
			nbY <- nrow(Y)
		}else{
			ina <- is.na(X) | is.na(Y)
			X[ina] <- NA
			Y[ina] <- NA
			nbY <- colSums(!is.na(Y))
			ix <- nbY >= min.len
			if(!any(ix)) return(NULL)
			Y <- Y[, ix, drop = FALSE]
			X <- X[, ix, drop = FALSE]
			nbY <- nbY[ix]
		}

		ncolY <- ncol(Y)
		nrowY <- nrow(Y)

		mX <- colMeans(X, na.rm = TRUE)
		mY <- colMeans(Y, na.rm = TRUE)
		vX <- matrixStats::colVars(X, na.rm = TRUE)
		vY <- matrixStats::colVars(Y, na.rm = TRUE)

		X1 <- X - matrix(mX, nrowY, ncolY, byrow = TRUE)
		Y1 <- Y - matrix(mY, nrowY, ncolY, byrow = TRUE)
		COV <- colSums(X1 * Y1, na.rm = TRUE) / (nbY - 1)
		alpha <- COV / vX
		beta <- mY - alpha * mX

		hatY <- matrix(alpha, nrowY, ncolY, byrow = TRUE) * X + matrix(beta, nrowY, ncolY, byrow = TRUE)
		SSE <- colSums((hatY - Y)^2, na.rm = TRUE)
		MSE <- SSE / (nbY - 2)
		sigma <- sqrt(MSE)
		std.alpha <- sigma / (sqrt(nbY - 1) * sqrt(vX))
		# std.beta <- sigma * sqrt((1/nbY) + (mX^2 / ((nbY - 1)*vX)))
		SXX <- (nbY - 1) * vX
		tvalue.alpha <- alpha / sqrt(MSE/SXX)
		# tvalue.beta <- beta / sqrt(MSE * ((1/nbY) + (mX^2/SXX)))
		pvalue.alpha <- 2 * pt(-abs(tvalue.alpha), nbY - 2)
		# pvalue.beta <- 2 * pt(-abs(tvalue.beta), nbY - 2)
		R2 <- COV^2 / (vX * vY)

		out <- matrix(NA, 4, length(ix))
		out[, ix] <- rbind(alpha, beta, pvalue.alpha, R2)
		return(out)
	})

	model.coef <- list(slope = do.call(rbind, lapply(model.coef, function(x) if(is.null(x)) rep(NA, nstn) else x[1, ])),
					   intercept = do.call(rbind, lapply(model.coef, function(x) if(is.null(x)) rep(NA, nstn) else x[2, ])),
					   pvalue = do.call(rbind, lapply(model.coef, function(x) if(is.null(x)) rep(NA, nstn) else x[3, ])),
					   rsquared = do.call(rbind, lapply(model.coef, function(x)if(is.null(x)) rep(NA, nstn) else x[4, ])))
	nommodcoef <- names(model.coef)
	coef0 <- model.coef

	islp <- !is.na(model.coef$slope) & model.coef$slope > 0
	model.coef$slope[!islp] <- NA
	extrm <- t(apply(model.coef$slope, 1, quantile, prob = c(0.001, 0.999), na.rm = TRUE))
	islp <- !is.na(model.coef$slope) & model.coef$slope > extrm[, 1] & model.coef$slope < extrm[, 2]
	intrcp <- !is.na(model.coef$intercept)
	ipval <- !is.na(model.coef$pvalue) & !is.nan(model.coef$pvalue) & model.coef$pvalue < 0.05
	irsq <- !is.na(model.coef$rsquared) & model.coef$rsquared > 0.2

	model.coef <- lapply(model.coef, function(x){
		x[!(islp & intrcp & ipval & irsq)] <- NA
		x
	})
	names(model.coef) <- nommodcoef

	##########
	model.params <- list(coef0 = coef0, coef = model.coef, id.stn = lmCoefParms$stnData$id,
						lon.stn = lmCoefParms$stnData$lon, lat.stn = lmCoefParms$stnData$lat,
						date.stn = date.stn[iyear0], data.stn = data.stn.reg, data.tt = data.down.stn)
	saveRDS(model.params, file = file.path(lmCoefParms$LMCoef.DIR, "LM_MODEL_PARS.rds"))

	rm(model.params, coef0, data.stn, data.down.stn, downData)
	##########
	Insert.Messages.Out('Computing LM Coefficients finished')

	return(model.coef)
}

#######################################################################################

Temp_InterpolateLMCoef <- function(){
	Insert.Messages.Out('Interpolate LM Coefficients ...')

	lmCoefParms <- .cdtData$GalParams$lmCoefParms

	#############
	auxvar <- c('dem', 'slp', 'asp', 'alon', 'alat')
	is.auxvar <- unlist(.cdtData$GalParams$auxvar)
	if(any(is.auxvar)){
		formule <- formula(paste0('pars', '~', paste(auxvar[is.auxvar], collapse = '+')))
	}else formule <- formula(paste0('pars', '~', 1))

	#############
	xy.grid <- lmCoefParms$interp.grid$grid
	grdSp <- defSpatialPixels(xy.grid)
	nlon0 <- lmCoefParms$interp.grid$nlon
	nlat0 <- lmCoefParms$interp.grid$nlat

	#############
	#Defines netcdf output dims
	dx <- ncdim_def("Lon", "degreeE", xy.grid$lon)
	dy <- ncdim_def("Lat", "degreeN", xy.grid$lat)
	xy.dim <- list(dx, dy)

	#############
	demGrid <- lmCoefParms$demData
	if(!is.null(demGrid)){
		slpasp <- raster.slope.aspect(demGrid)
		demGrid$slp <- slpasp$slope
		demGrid$asp <- slpasp$aspect
	}else{
		demGrid <- list(x = xy.grid$lon, y = xy.grid$lat, z = matrix(1, nlon0, nlat0))
		demGrid$slp <- matrix(0, nlon0, nlat0)
		demGrid$asp <- matrix(0, nlon0, nlat0)
	}

	ijGrd <- grid2pointINDEX(lmCoefParms$stnData, xy.grid)
	ObjStn <- list(x = lmCoefParms$stnData$lon, y = lmCoefParms$stnData$lat,
					z = demGrid$z[ijGrd], slp = demGrid$slp[ijGrd], asp = demGrid$asp[ijGrd])

	#############
	## interpolation grid
	interp.method <- .cdtData$GalParams$LMCOEF$interp.method
	nmin <- .cdtData$GalParams$LMCOEF$nmin
	nmax <- .cdtData$GalParams$LMCOEF$nmax
	maxdist <- .cdtData$GalParams$LMCOEF$maxdist
	vgm.model <- .cdtData$GalParams$LMCOEF$vgm.model
	rad.lon <- .cdtData$GalParams$LMCOEF$rad.lon
	rad.lat <- .cdtData$GalParams$LMCOEF$rad.lat
	rad.elv <- .cdtData$GalParams$LMCOEF$rad.elv

	min.stn <- .cdtData$GalParams$LMCOEF$min.stn

	if(interp.method == 'NN'){
		grd.lon <- xy.grid$lon
		grd.lat <- xy.grid$lat
		res.coarse <- sqrt((rad.lon * mean(grd.lon[-1] - grd.lon[-nlon0]))^2 + (rad.lat * mean(grd.lat[-1] - grd.lat[-nlat0]))^2)/2
	}else res.coarse <- maxdist/2
	res.coarse <- if(res.coarse >= 0.25) res.coarse else 0.25

	#############
	## create grid to interp
	bGrd <- NULL
	if(interp.method == 'NN'){
		interp.grid <- createGrid(ObjStn, demGrid,
					as.dim.elv = .cdtData$GalParams$LMCOEF$elev.3rd.dim,
					latlong = .cdtData$GalParams$LMCOEF$latlon.unit,
					normalize = .cdtData$GalParams$LMCOEF$normalize,
					coarse.grid = TRUE, res.coarse = res.coarse)
		xy.maxdist <- sqrt(sum((c(rad.lon, rad.lat) * (apply(coordinates(interp.grid$newgrid)[, c('lon', 'lat')], 2,
						function(x) diff(range(x))) / (c(nlon0, nlat0) - 1)))^2))
		elv.grd <- range(demGrid$z, na.rm = TRUE)
		nelv <- length(seq(elv.grd[1], elv.grd[2], 100))
		nelv <- if(nelv > 1) nelv else 2
		z.maxdist <- rad.elv * (diff(range(coordinates(interp.grid$newgrid)[, 'elv'])) / (nelv - 1))
		xy.maxdist <- if(xy.maxdist < res.coarse) res.coarse else xy.maxdist
		maxdist <- sqrt(xy.maxdist^2 + z.maxdist^2)
	}else{
		interp.grid <- createGrid(ObjStn, demGrid, as.dim.elv = FALSE, res.coarse = res.coarse)
		maxdist <- if(maxdist < res.coarse) res.coarse else maxdist
		cells <- SpatialPixels(points = interp.grid$newgrid, tolerance = sqrt(sqrt(.Machine$double.eps)))@grid
		bGrd <- if(.cdtData$GalParams$LMCOEF$use.block) createBlock(cells@cellsize, 2, 5) else NULL
	}

	if(is.auxvar['lon']){
		interp.grid$coords.stn$alon <- interp.grid$coords.stn@coords[, 'lon']
		interp.grid$coords.grd$alon <- interp.grid$coords.grd@coords[, 'lon']
		if(!is.null(interp.grid$coords.rfe)) interp.grid$coords.rfe$alon <- interp.grid$coords.rfe@coords[, 'lon']
		interp.grid$newgrid$alon <- interp.grid$newgrid@coords[, 'lon']
	}

	if(is.auxvar['lat']){
		interp.grid$coords.stn$alat <- interp.grid$coords.stn@coords[, 'lat']
		interp.grid$coords.grd$alat <- interp.grid$coords.grd@coords[, 'lat']
		if(!is.null(interp.grid$coords.rfe)) interp.grid$coords.rfe$alat <- interp.grid$coords.rfe@coords[, 'lat']
		interp.grid$newgrid$alat <- interp.grid$newgrid@coords[, 'lat']
	}

	##################

	months <- lmCoefParms$months
	model.coef <- lmCoefParms$model.coef

	is.parallel <- doparallel(length(months) >= 3)
	`%parLoop%` <- is.parallel$dofun
	MODEL.COEF <- vector(mode = 'list', length = 12)
	MODEL.COEF[months] <- foreach(m = months, .packages = 'sp') %parLoop% {
		pars.mon <- lapply(1:2, function(jc){
			locations.stn <- interp.grid$coords.stn
			xcoef <- model.coef[[jc]]
			locations.stn$pars <- xcoef[as.numeric(rownames(xcoef)) == m, ]
			locations.stn <- locations.stn[!is.na(locations.stn$pars), ]

			if(any(is.auxvar) & interp.method != 'NN'){
				locations.df <- as.data.frame(!is.na(locations.stn@data[, auxvar[is.auxvar]]))
				locations.stn <- locations.stn[Reduce("&", locations.df), ]
			}
			if(length(locations.stn$pars) < min.stn) return(NULL)

			if(interp.method == 'Kriging'){
				if(length(locations.stn$pars) > 7){
					vgm <- try(automap::autofitVariogram(formule, input_data = locations.stn, model = vgm.model, cressie = TRUE), silent = TRUE)
					vgm <- if(!inherits(vgm, "try-error")) vgm$var_model else NULL
				}else vgm <- NULL
			}else vgm <- NULL

			xstn <- as.data.frame(locations.stn)
			xadd <- as.data.frame(interp.grid$coords.grd)
			xadd$pars <- if(jc == 1) 1 else 0
			iadd <- rep(TRUE, nrow(xadd))

			for(k in 1:nrow(xstn)){
				if(interp.method == 'NN'){
					xy.dst <- sqrt((xstn$lon[k] - xadd$lon)^2 + (xstn$lat[k] - xadd$lat)^2) * sqrt(2)
					z.dst <- abs(xstn$elv[k] - xadd$elv)
					z.iadd <- (z.dst < z.maxdist) & (xy.dst == min(xy.dst))
					iadd <- iadd & (xy.dst >= xy.maxdist) & !z.iadd
				}else{
					dst <- sqrt((xstn$lon[k] - xadd$lon)^2 + (xstn$lat[k] - xadd$lat)^2) * sqrt(2)
					iadd <- iadd & (dst >= maxdist)
				}
			}
			xadd <- xadd[iadd, ]
			locations.stn <- rbind(xstn, xadd)

			if(interp.method == 'NN'){
				coordinates(locations.stn) <- ~lon + lat + elv
				pars.grd <- gstat::krige(pars~1, locations = locations.stn, newdata = interp.grid$newgrid,
									nmax = 1, maxdist = maxdist, debug.level = 0)
			}else{
				coordinates(locations.stn) <- ~lon + lat
				if(any(is.auxvar)){
					locations.df <- as.data.frame(!is.na(locations.stn@data[, auxvar[is.auxvar]]))
					locations.stn <- locations.stn[Reduce("&", locations.df), ]
					block <- NULL 
				}else block <- bGrd

				pars.grd <- gstat::krige(formule, locations = locations.stn, newdata = interp.grid$newgrid, model = vgm,
									block = block, nmin = nmin, nmax = nmax, maxdist = maxdist, debug.level = 0)
				extrm1 <- min(locations.stn$pars, na.rm = TRUE)
				pars.grd$var1.pred[pars.grd$var1.pred <= extrm1] <- extrm1
				extrm2  <- max(locations.stn$pars, na.rm = TRUE)
				pars.grd$var1.pred[pars.grd$var1.pred >= extrm2] <- extrm2
			}
			matrix(pars.grd$var1.pred, ncol = nlat0, nrow = nlon0)
		})
		names(pars.mon) <- c('slope', 'intercept')
		pars.mon
	}
	if(is.parallel$stop) stopCluster(is.parallel$cluster)

	###########
	grd.slope <- ncvar_def("slope", "", xy.dim, NA, longname= "Linear model Coef: Slope", prec = "float", compression = 9)
	grd.intercept <- ncvar_def("intercept", "", xy.dim, NA, longname= "Linear model Coef: Intercept", prec = "float", compression = 9)

	for(jfl in months){
		if(is.null(MODEL.COEF[[jfl]]$slope) | is.null(MODEL.COEF[[jfl]]$intercept)){
			MODEL.COEF[[jfl]]$slope <- matrix(1, ncol = nlat0, nrow = nlon0)
			MODEL.COEF[[jfl]]$intercept <- matrix(0, ncol = nlat0, nrow = nlon0)
		}
		ina <- is.na(MODEL.COEF[[jfl]]$slope) | is.na(MODEL.COEF[[jfl]]$intercept)
		MODEL.COEF[[jfl]]$slope[ina] <- 1
		MODEL.COEF[[jfl]]$intercept[ina] <- 0

		xneg <- MODEL.COEF[[jfl]]$slope <= 0
		MODEL.COEF[[jfl]]$slope[xneg] <- 1
		MODEL.COEF[[jfl]]$intercept[xneg] <- 0

		outnc1 <- file.path(lmCoefParms$LMCoef.DIR, sprintf(.cdtData$GalParams$lmCoefFilenames, jfl))
		nc1 <- nc_create(outnc1, list(grd.slope, grd.intercept))
		ncvar_put(nc1, grd.slope, MODEL.COEF[[jfl]]$slope)
		ncvar_put(nc1, grd.intercept, MODEL.COEF[[jfl]]$intercept)
		nc_close(nc1)
	}

	Insert.Messages.Out('Interpolating LM Coefficients finished')
	return(0)
}
