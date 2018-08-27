
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
	grdSp <- defSpatialPixels(xy.grid)
	nlon0 <- mrgParms$interp.grid$nlon
	nlat0 <- mrgParms$interp.grid$nlat

	#############
	## Def ncdf
	dx <- ncdim_def("Lon", "degreeE", xy.grid$lon)
	dy <- ncdim_def("Lat", "degreeN", xy.grid$lat)
	grd.nc.out <- ncvar_def('temp', "DegC", list(dx, dy), -99,
							longname = 'Reanalysis merged with station',
							prec = "float", compression = 9)

	#############
	mrg.method <- .cdtData$GalParams$Merging$mrg.method
	interp.method <- .cdtData$GalParams$Merging$interp.method
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
	bGrd <- if(.cdtData$GalParams$Merging$use.block) createBlock(cells@cellsize, 1, 5) else NULL

	#############
	auxvar <- c('dem', 'slp', 'asp', 'alon', 'alat')
	is.auxvar <- unlist(.cdtData$GalParams$auxvar)
	if(any(is.auxvar)){
		formule <- formula(paste0('res', '~', paste(auxvar[is.auxvar], collapse = '+')))
		if(mrg.method == "Regression Kriging"){
			sp.trend.aux <- .cdtData$GalParams$Merging$sp.trend.aux
			if(sp.trend.aux) formuleRK <- formula(paste0('stn', '~', 'tmp', '+', paste(auxvar[is.auxvar], collapse = '+')))
			else formuleRK <- formula(paste0('stn', '~', 'tmp'))
		}
	}else{
		formule <- formula(paste0('res', '~', 1))
		if(mrg.method == "Regression Kriging") formuleRK <- formula(paste0('stn', '~', 'tmp'))
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
	ncInfo <- mrgParms$ncInfo
	xlon <- ncInfo$ncinfo$lon
	xlat <- ncInfo$ncinfo$lat
	ijGrd <- grid2pointINDEX(list(lon = lon.stn, lat = lat.stn), list(lon = xlon, lat = xlat))

	#############

	is.parallel <- doparallel(length(which(ncInfo$exist)) >= 10)
	`%parLoop%` <- is.parallel$dofun
	ret <- foreach(jj = seq_along(ncInfo$nc.files), .packages = c('sp', 'ncdf4')) %parLoop% {
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

		donne.stn <- data.stn[date.stn == ncInfo$dates[jj], , drop = FALSE]
		if(nrow(donne.stn) == 0){
			writeNC.merging(xtmp, ncInfo$dates[jj], freqData, grd.nc.out,
					mrgParms$merge.DIR, .cdtData$GalParams$output$format)
			cat(paste(ncInfo$dates[jj], ":", "no station data", "|", "Input temperature data", "\n"), file = log.file, append = TRUE)
			return(NULL)
		}
		donne.stn <- data.frame(lon = lon.stn, lat = lat.stn, stn = c(donne.stn))
		stng <- createGrid.StnData(donne.stn, ijGrd, interp.grid$newgrid, min.stn, weighted = FALSE)
		if(is.null(stng)){
			writeNC.merging(xtmp, ncInfo$dates[jj], freqData, grd.nc.out,
					mrgParms$merge.DIR, .cdtData$GalParams$output$format)
			cat(paste(ncInfo$dates[jj], ":", "not enough station data", "|", "Input temperature data", "\n"),
				file = log.file, append = TRUE)
			return(NULL)
		}

		############
		locations.stn <- interp.grid$newgrid
		locations.stn$stn <- stng
		locations.stn$tmp <- c(xtmp)

		############
		xadd <- interp.grid$coords.coarse
		xadd$tmp <- xadd$stn <- c(xtmp[interp.grid$id.coarse$ix, interp.grid$id.coarse$iy])
		xadd$res <- 0

		############
		interp.grid$newgrid$tmp <- c(xtmp)
		newdata <- interp.grid$newgrid

		############
		noNA <- !is.na(locations.stn$stn)
		locations.stn <- locations.stn[noNA, ]

		if(all(is.na(locations.stn$tmp))){
			writeNC.merging(xtmp, ncInfo$dates[jj], freqData, grd.nc.out,
					mrgParms$merge.DIR, .cdtData$GalParams$output$format)
			cat(paste(ncInfo$dates[jj], ":", "all temperature data @ station location are missing", "|", "Input temperature data", "\n"),
				file = log.file, append = TRUE)
			return(NULL)
		}

		############
		# spatial trend
		sp.trend <- xtmp
		locations.stn$res <- locations.stn$stn - locations.stn$tmp

		if(mrg.method == "Spatio-Temporal LM"){
			mo <- as(substr(ncInfo$dates[jj], 5, 6), 'numeric')
			sp.trend <- xtmp * MODEL.COEF[[mo]]$slope + MODEL.COEF[[mo]]$intercept
			locations.stn$res <- locations.stn$stn - sp.trend[noNA]
		}
		if(mrg.method == "Regression Kriging"){
			simplediff <- if(var(locations.stn$stn) < 1e-07 | var(locations.stn$tmp, na.rm = TRUE) < 1e-07) TRUE else FALSE
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
				sp.trend[ina.trend] <- xtmp[ina.trend]
				locations.stn$res <- NA
				if(length(glm.stn$na.action) > 0) locations.stn$res[-glm.stn$na.action] <- glm.stn$residuals
				else locations.stn$res <- glm.stn$residuals
			}
		}

		############
		locations.stn <- locations.stn[!is.na(locations.stn$res), ]

		if(length(locations.stn) < min.stn){
			writeNC.merging(xtmp, ncInfo$dates[jj], freqData, grd.nc.out,
					mrgParms$merge.DIR, .cdtData$GalParams$output$format)
			cat(paste(ncInfo$dates[jj], ":", "not enough station data", "|", "Input temperature data", "\n"),
				file = log.file, append = TRUE)
			return(NULL)
		}

		############
		if(any(is.auxvar)){
			locations.df <- as.data.frame(!is.na(locations.stn@data[, auxvar[is.auxvar]]))
			locations.stn <- locations.stn[Reduce("&", locations.df), ]

			if(length(locations.stn) < min.stn){
				writeNC.merging(xtmp, ncInfo$dates[jj], freqData, grd.nc.out,
						mrgParms$merge.DIR, .cdtData$GalParams$output$format)
				cat(paste(ncInfo$dates[jj], ":", "not enough station data combined with auxiliary var", "|",
						"Input temperature data", "\n"), file = log.file, append = TRUE)
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
		buffer.xaddin <- rgeos::gBuffer(locations.stn, width = maxdist / sqrt(2))
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
		## plot ici

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
		res.grd <- gstat::krige(formule, locations = locations.stn, newdata = newdata0, model = vgm,
						block = block, nmin = nmin, nmax = nmax, maxdist = maxdist, debug.level = 0)

		###########
		# remove extreme residuals outside station range
		xtrm <- range(locations.stn$res, na.rm = TRUE)
		extrm1 <- xtrm[1] - diff(xtrm) * 0.05
		extrm2 <- xtrm[2] + diff(xtrm) * 0.05
		res.grd$var1.pred[!is.na(res.grd$var1.pred) & res.grd$var1.pred < extrm1] <- extrm1
		res.grd$var1.pred[!is.na(res.grd$var1.pred) & res.grd$var1.pred > extrm2] <- extrm2

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

		###########
		resid <- rep(0, length(newdata))
		resid[igrid] <- res.grd$var1.pred
		resid[is.na(resid)] <- 0
		resid <- matrix(resid, ncol = nlat0, nrow = nlon0)
		out.mrg <- sp.trend + resid

		###########

		if(mrg.method == "Regression Kriging"){
			bsmoo <- as.logical(over(newdata, buffer.ina))
			bsmoo[is.na(bsmoo)] <- FALSE
			mout.in <- !as.logical(over(newdata, buffer.xaddin))
			mout.in[is.na(mout.in)] <- TRUE
			imout <- mout.in & igrid

			out.tmp <- xtmp
			out.tmp[bsmoo] <- out.mrg[bsmoo]
			out.tmp <- smooth.matrix(out.tmp, 1)
			out.mrg[!igrid] <- xtmp[!igrid]
			out.mrg[imout] <- out.tmp[imout]
			rm(bsmoo, mout.in, imout, out.tmp)
		}

		rm(resid, sp.trend, res.grd, inside, ina, newdata0,
			igrid, locations.stn, xadd, xadd.in, xadd.out,
			buffer.ina, buffer.grid, buffer.xaddin, buffer.xaddout)

		###########
		#Apply mask for area of interest
		if(!is.null(mrgParms$outMask)) out.mrg[is.na(mrgParms$outMask)] <- NA

		writeNC.merging(out.mrg, ncInfo$dates[jj], freqData, grd.nc.out,
				mrgParms$merge.DIR, .cdtData$GalParams$output$format)

		rm(out.mrg, xtmp, newdata)
		gc()
		return(0)
	}
	if(is.parallel$stop) stopCluster(is.parallel$cluster)

	Insert.Messages.Out('Merging finished')
	return(0)
}


