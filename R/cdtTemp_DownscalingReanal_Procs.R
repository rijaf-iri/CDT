
Temp_execDownscaling <- function(){
	daty <- .cdtData$GalParams$Down.Date.Range
	if(.cdtData$GalParams$period == 'monthly'){
		xdeb <- paste0(format(ISOdate(2014, daty$start.mon, 1), "%b"), daty$start.year)
		xfin <- paste0(format(ISOdate(2014, daty$end.mon, 1), "%b"), daty$end.year)
	}else{
		xdeb <- paste0(daty$start.day, format(ISOdate(2014, daty$start.mon, 1), "%b"), daty$start.year)
		xfin <- paste0(daty$end.day, format(ISOdate(2014, daty$end.mon, 1), "%b"), daty$end.year)
	}
	origdir <- file.path(.cdtData$GalParams$output$dir, paste('Downscaled_Reanalysis', xdeb, xfin, sep = '_'))

	dir.create(origdir, showWarnings = FALSE, recursive = TRUE)
	Insert.Messages.Out('Downscaling ...')

	################
	CoefFile <- str_trim(.cdtData$GalParams$DownCoef.file)
	if(!file.exists(CoefFile)){
		Insert.Messages.Out(paste(CoefFile, "not found"), format = TRUE)
		return(NULL)
	}

	downCoef <- try(readRDS(CoefFile), silent = TRUE)
	if(inherits(downCoef, "try-error")){
		Insert.Messages.Out('Error reading downscaling coefficients', format = TRUE)
		return(NULL)
	}

	################
	## Reanalysis sample file
	reanalInfo <- getNCDFSampleData(.cdtData$GalParams$REANAL$sample)
	if(is.null(reanalInfo)) return(NULL)

	################
	## get elevation data
	demInfo <- getNCDFSampleData(.cdtData$GalParams$DEM.file)
	if(is.null(demInfo)){
		Insert.Messages.Out("Unable to read DEM data", format = TRUE)
		return(NULL)
	}

	jfile <- getIndex.AllOpenFiles(.cdtData$GalParams$DEM.file)
	demData <- .cdtData$OpenFiles$Data[[jfile]][[2]]
	demData$lon <- demData$x
	demData$lat <- demData$y

	## Create grid for interpolation
	create.grd <- .cdtData$GalParams$Grid.Creation$grid
	if(create.grd == '1'){
		grd.lon <- demData$lon
		grd.lat <- demData$lat
	}else if(create.grd == '2'){
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

	## DEM data  at new grid
	demGrid <- demData$z
	if(create.grd == '2'){
		is.regridDEM <- is.diffSpatialPixelsObj(defSpatialPixels(xy.grid), defSpatialPixels(demData), tol = 1e-07)
		if(is.regridDEM){
			demGrid <- cdt.interp.surface.grid(demData, xy.grid)
			demGrid <- demGrid$z
		}
	}
	demGrid[demGrid < 0] <- 0

	################
	start.year <- .cdtData$GalParams$Down.Date.Range$start.year
	start.mon <- .cdtData$GalParams$Down.Date.Range$start.mon
	start.dek <- .cdtData$GalParams$Down.Date.Range$start.day
	end.year <- .cdtData$GalParams$Down.Date.Range$end.year
	end.mon <- .cdtData$GalParams$Down.Date.Range$end.mon
	end.dek <- .cdtData$GalParams$Down.Date.Range$end.day
	months <- .cdtData$GalParams$Down.Date.Range$Months[[1]]

	reanalDir <- .cdtData$GalParams$REANAL$dir
	reanalfilefrmt <- .cdtData$GalParams$REANAL$format

	################
	start.date <- as.Date(paste(start.year, start.mon, start.dek, sep = '/'), format = '%Y/%m/%d')
	end.date <- as.Date(paste(end.year, end.mon, end.dek, sep = '/'), format = '%Y/%m/%d')

	errmsg <- "Reanalysis data not found"
	ncInfo <- ncFilesInfo(.cdtData$GalParams$period, start.date, end.date, months, reanalDir, reanalfilefrmt, errmsg)
	if(is.null(ncInfo)) return(NULL)
	ncInfo$ncinfo <- reanalInfo

	################
	.cdtData$GalParams$paramsDownscl <- list(demGrid = demGrid, downCoef = downCoef, reanalData = ncInfo,
											xy.grid = xy.grid, origdir = origdir)
	ret <- Temp_ReanalysisDownscaling()
	rm(demData, demGrid)
	gc()
	if(!is.null(ret)){
		if(ret == 0) return(0)
		else return(ret)
	}else return(NULL)
}

#################################################################################################

Temp_ReanalysisDownscaling <- function(){
	paramsDownscl <- .cdtData$GalParams$paramsDownscl
	interp.method <- .cdtData$GalParams$Interpolation$interp.method
	nmin <- .cdtData$GalParams$Interpolation$nmin
	nmax <- .cdtData$GalParams$Interpolation$nmax
	maxdist <- .cdtData$GalParams$Interpolation$maxdist
	vgm.model <- str_trim(.cdtData$GalParams$Interpolation$vgm.model[[1]])
	use.block <- .cdtData$GalParams$Interpolation$use.block

	freqData <- .cdtData$GalParams$period
	Down.File.Format <- .cdtData$GalParams$output$format
	origdir <- paramsDownscl$origdir
	ncInfos <- paramsDownscl$reanalData
	ncinfo <- ncInfos$ncinfo

	###############

	lon.reanl <- ncinfo$lon
	lat.reanl <- ncinfo$lat
	nlon.r <- ncinfo$nx
	nlat.r <- ncinfo$ny
	xrnl <- lon.reanl[2] - lon.reanl[1]
	yrnl <- lat.reanl[2] - lat.reanl[1]

	res.max <- max(xrnl, yrnl)
	maxdist <- if(maxdist < res.max) sqrt(2) * res.max else maxdist

	###############

	demGrid <- paramsDownscl$demGrid

	xy.grid <- paramsDownscl$xy.grid
	grdSp <- defSpatialPixels(xy.grid)
	nlon0 <- length(xy.grid$lon)
	nlat0 <- length(xy.grid$lat)

	###############
	## coeff
	downCoef <- paramsDownscl$downCoef

	## elevation
	if(downCoef$fitting == "elevation") data.grid <- data.frame(z = c(demGrid))

	if(downCoef$standardize){
		data.grid0 <- as.matrix(data.grid)
		moy <- colMeans(data.grid0, na.rm = TRUE)
		sds <- matrixStats::colSds(data.grid0, na.rm = TRUE)
		data.grid <- sweep(sweep(data.grid, 2, moy, FUN = "-"), 2, sds, FUN = "/")
		rm(data.grid0, moy, sds)
	}

	###############
	#Defines netcdf output dims
	dx <- ncdim_def("Lon", "degreeE", xy.grid$lon)
	dy <- ncdim_def("Lat", "degreeN", xy.grid$lat)
	out.tt <- ncvar_def("temp", "DegC", list(dx, dy), -99, longname = "Dwonscaled temperature from reanalysis data", prec = "float", compression = 9)

	###############
	## DEM at reanalysis grid

	dem.reanal <- list(lon = lon.reanl, lat = lat.reanl)
	dem.reanal$lon <- c(lon.reanl[1] - xrnl, lon.reanl, lon.reanl[nlon.r] + xrnl)
	dem.reanal$lat <- c(lat.reanl[1] - yrnl, lat.reanl, lat.reanl[nlat.r] + yrnl)
	dem.reanal <- cdt.aggregate.grid(c(xy.grid, list(z = demGrid)), grid.list = dem.reanal, FUN = mean, na.rm = TRUE)
	# slpasp.reanal <- raster.slope.aspect(dem.reanal)
	dem.reanal <- dem.reanal$z[-c(1, nlon.r + 2), -c(1, nlat.r + 2)]

	## elevation
	if(downCoef$fitting == "elevation") data.reanal <- data.frame(z = c(dem.reanal))

	if(downCoef$standardize){
		data.reanal0 <- as.matrix(data.reanal)
		moy <- colMeans(data.reanal0, na.rm = TRUE)
		sds <- matrixStats::colSds(data.reanal0, na.rm = TRUE)
		data.reanal <- sweep(sweep(data.reanal, 2, moy, FUN = "-"), 2, sds, FUN = "/")
		rm(data.reanal0, moy, sds)
	}

	locations.reanl <- expand.grid(lon = lon.reanl, lat = lat.reanl)
	coordinates(locations.reanl) <- ~lon+lat

	##############
	## create grid

	interp.grid <- list(x = xy.grid$lon, y = xy.grid$lat,
					z = matrix(1, nlon0, nlat0),
					slp = matrix(0, nlon0, nlat0),
					asp = matrix(0, nlon0, nlat0))
	interp.grid <- createGrid.merging(interp.grid, coarse.grid = FALSE)

	cells <- SpatialPixels(points = interp.grid$newgrid, tolerance = sqrt(sqrt(.Machine$double.eps)))@grid
	bGrd <- if(use.block) createBlock(cells@cellsize, 1, 5) else NULL

	###############
	Insert.Messages.Out("Downscale  Reanalysis ...")

	is.parallel <- doparallel(length(which(ncInfos$exist)) >= 30)
	`%parLoop%` <- is.parallel$dofun

	ret <- foreach(jj = seq_along(ncInfos$nc.files), .packages = 'ncdf4') %parLoop% {
		if(ncInfos$exist[jj]){
			nc <- nc_open(ncInfos$nc.files[jj])
			tt.reanl <- ncvar_get(nc, varid = ncinfo$varid)
			nc_close(nc)
			tt.reanl <- transposeNCDFData(tt.reanl, ncinfo)
		}else return(NULL)

		date.reanl <- ncInfos$dates[[jj]]
		mon <- as.numeric(substr(date.reanl, 5, 6))

		if(downCoef$standardize){
			tt.mean <- downCoef$model[[mon]]$std.pars$mean['v']
			tt.sd <- downCoef$model[[mon]]$std.pars$sd['v']
			tt.reanl <- (tt.reanl - tt.mean) / tt.sd
		}

		resid <- tt.reanl - predict(downCoef$model[[mon]], newdata = data.reanal)

		############
		if(interp.method == 'FBL'){
			resid[is.na(resid)] <- 0
			residObj <- list(lon = lon.reanl, lat = lat.reanl, z = resid)
			residInterp <- cdt.interp.surface.grid(residObj, xy.grid)
			residInterp <- residInterp$z
		}else{
			locations.reanl$res <- c(resid)
			locations.reanl0 <- locations.reanl[!is.na(locations.reanl$res), ]
			if(length(locations.reanl0$res) < 2) return(NULL)

			if(interp.method == 'Kriging'){
				if(length(locations.reanl0$res) > 7){
					vgm <- try(automap::autofitVariogram(res~1, input_data = locations.reanl0, model = vgm.model, cressie = TRUE), silent = TRUE)
					vgm <- if(!inherits(vgm, "try-error")) vgm$var_model else NULL
				}else vgm <- NULL
			}else vgm <- NULL

			grd.temp <- gstat::krige(res~1, locations = locations.reanl0, newdata = interp.grid$newgrid, model = vgm,
									block = bGrd, nmin = nmin, nmax = nmax, maxdist = maxdist, debug.level = 0)
			residInterp <- matrix(grd.temp$var1.pred, ncol = nlat0, nrow = nlon0)
		}

 		############
		residInterp[is.na(residInterp)] <- 0
		downTT <- predict(downCoef$model[[mon]], newdata = data.grid) + residInterp
		if(downCoef$standardize) downTT <- downTT * tt.sd + tt.mean
		downTT[is.na(downTT)] <- -99

		############
		year <- substr(date.reanl, 1, 4)
		month <- substr(date.reanl, 5, 6)
		if(freqData == 'daily')
			outncfrmt <- sprintf(Down.File.Format, year, month, substr(date.reanl, 7, 8))
		else if(freqData %in% c('pentad', 'dekadal'))
			outncfrmt <- sprintf(Down.File.Format, year, month, substr(date.reanl, 7, 7))
		else
			outncfrmt <- sprintf(Down.File.Format, year, month)

		outfl <- file.path(origdir, outncfrmt)

		nc <- nc_create(outfl, out.tt)
		ncvar_put(nc, out.tt, downTT)
		nc_close(nc)
		rm(residInterp, downTT, resid, tt.reanl)
	}
	if(is.parallel$stop) stopCluster(is.parallel$cluster)

	Insert.Messages.Out('Downscaling  Reanalysis finished')
	rm(demGrid, interp.grid)
	gc()
	return(0)
}
