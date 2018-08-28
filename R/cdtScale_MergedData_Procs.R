
exec_ScalingUpData <- function(){
	origdir <- file.path(.cdtData$GalParams$outdir, "Merged_ScaledData")
	dir.create(origdir, showWarnings = FALSE, recursive = TRUE)
	.cdtData$GalParams$outdir <- origdir
	ret <- merged_ScalingUpData()

	if(!is.null(ret)){
		if(ret == 0) return(0)
		else return(ret)
	} else return(NULL)
}

merged_ScalingUpData <- function(){
	Insert.Messages.Out('Scale data ...')

	ncParms <- .cdtData$GalParams
	start.year <- ncParms$Scaling.Date$start.year
	start.mon <- ncParms$Scaling.Date$start.mon
	start.dek <- ncParms$Scaling.Date$start.day
	end.year <- ncParms$Scaling.Date$end.year
	end.mon <- ncParms$Scaling.Date$end.mon
	end.dek <- ncParms$Scaling.Date$end.day
	months <- ncParms$Scaling.Date$Months
	start.date <- as.Date(paste(start.year, start.mon, start.dek, sep = '/'), format = '%Y/%m/%d')
	end.date <- as.Date(paste(end.year, end.mon, end.dek, sep = '/'), format = '%Y/%m/%d')

	#######
	errmsg <- "Data to be scaled up not found"
	ncInfoMrg <- ncFilesInfo(ncParms$mrg.data$tstep, start.date, end.date, 1:12,
							ncParms$mrg.data$dir, ncParms$mrg.data$format, errmsg)
	if(is.null(ncInfoMrg)) return(NULL)

	#######
	datMrgInfo <- getNCDFSampleData(ncParms$mrg.data$sample)
	if(is.null(datMrgInfo)){
		Insert.Messages.Out("Netcdf data sample to be scaled up not found", format = TRUE)
		return(NULL)
	}
	ncInfoMrg$ncinfo <- datMrgInfo

	#######
	rfeScaleInfo <- getNCDFSampleData(ncParms$scale.data$sample)
	if(is.null(rfeScaleInfo)){
		Insert.Messages.Out("Netcdf data sample to be used to scale up not found", format = TRUE)
		return(NULL)
	}

	#######
	if(ncParms$mrg.data$tstep == 'daily'){
		enddayM <- Day.Of.Month(format(end.date, '%Y'), format(end.date, '%m'))
		if(ncParms$scale.data$tstep == 'pentad'){
			jour.s <- cut(as.numeric(format(start.date, '%d')), c(1, 5, 10, 15, 20, 25, 31), labels = FALSE, include.lowest = TRUE)
			jour.e <- cut(as.numeric(format(end.date, '%d')), c(1, 5, 10, 15, 20, 25, 31), labels = FALSE, include.lowest = TRUE)
			start.date <- as.Date(paste0(format(start.date, '%Y-%m'), '-', jour.s))
			end.date <- as.Date(paste0(format(end.date, '%Y-%m'), '-', jour.e))
		}
		if(ncParms$scale.data$tstep == 'dekadal'){
			jour.s <- cut(as.numeric(format(start.date, '%d')), c(1, 10, 20, 31), labels = FALSE, include.lowest = TRUE)
			jour.e <- cut(as.numeric(format(end.date, '%d')), c(1, 10, 20, 31), labels = FALSE, include.lowest = TRUE)
			start.date <- as.Date(paste0(format(start.date, '%Y-%m'), '-', jour.s))
			end.date <- as.Date(paste0(format(end.date, '%Y-%m'), '-', jour.e))
		}
	}

	if(ncParms$mrg.data$tstep == 'pentad'){
		if(ncParms$scale.data$tstep == 'dekadal'){
			jour.s <- cut(as.numeric(format(start.date, '%d')), c(1, 2, 4, 6), labels = FALSE, include.lowest = TRUE)
			jour.e <- cut(as.numeric(format(end.date, '%d')), c(1, 2, 4, 6), labels = FALSE, include.lowest = TRUE)
			start.date <- as.Date(paste0(format(start.date, '%Y-%m'), '-', jour.s))
			end.date <- as.Date(paste0(format(end.date, '%Y-%m'), '-', jour.e))
		}
	}

	#######
	errmsg <- "Data to be used to scale up not found"
	ncInfoScale <- ncFilesInfo(ncParms$scale.data$tstep, start.date, end.date, 1:12,
							ncParms$scale.data$dir, ncParms$scale.data$format, errmsg)
	if(is.null(ncInfoScale)) return(NULL)
	ncInfoScale$ncinfo <- rfeScaleInfo

	#######
	## indexing
	if(ncParms$scale.data$tstep == "monthly"){
		## daily, pentad and dekadal to monthly
		index <- split(seq_along(ncInfoMrg$dates), substr(ncInfoMrg$dates, 1, 6))
		if(ncParms$mrg.data$tstep == "daily"){
			nbd0 <- nb.Day.Of.Month(ncInfoMrg$dates[1])
			nbd1 <- nb.Day.Of.Month(ncInfoMrg$dates[length(ncInfoMrg$dates)])
		}
		if(ncParms$mrg.data$tstep == "pentad"){
			nbd0 <- 6
			nbd1 <- 6
		}
		if(ncParms$mrg.data$tstep == "dekadal"){
			nbd0 <- 3
			nbd1 <- 3
		}
	}else if(ncParms$scale.data$tstep == "dekadal"){
		if(ncParms$mrg.data$tstep == "daily"){
			## daily to dekadal
			yymm <- substr(ncInfoMrg$dates, 1, 6)
			jour <- as.numeric(substr(ncInfoMrg$dates, 7, 8))
			jour <- cut(jour, c(1, 10, 20, 31), labels = FALSE, include.lowest = TRUE)
			index <- split(seq_along(ncInfoMrg$dates), paste0(yymm, jour))
			nbd0 <- nb.Day.Of.Dekad(ncInfoMrg$dates[1])
			nbd1 <- nb.Day.Of.Dekad(ncInfoMrg$dates[length(ncInfoMrg$dates)])
		}
		if(ncParms$mrg.data$tstep == "pentad"){
			## pentad to dekadal
			yymm <- substr(ncInfoMrg$dates, 1, 6)
			pen <- as.numeric(substr(ncInfoMrg$dates, 7, 8))
			pen <- cut(pen, c(1, 2, 4, 6), labels = FALSE, include.lowest = TRUE)
			index <- split(seq_along(ncInfoMrg$dates), paste0(yymm, pen))
			nbd0 <- 2
			nbd1 <- 2
		}
	}else{
		## daily to pentad
		yymm <- substr(ncInfoMrg$dates, 1, 6)
		jour <- as.numeric(substr(ncInfoMrg$dates, 7, 8))
		jour <- cut(jour, c(1, 5, 10, 15, 20, 25, 31), labels = FALSE, include.lowest = TRUE)
		index <- split(seq_along(ncInfoMrg$dates), paste0(yymm, jour))
		nbd0 <- nb.Day.Of.Pentad(ncInfoMrg$dates[1])
		nbd1 <- nb.Day.Of.Pentad(ncInfoMrg$dates[length(ncInfoMrg$dates)])
	}

	#####
	dates.scale <- names(index)
	nbd.in <- nbd <- sapply(index, length)
	nbd[1] <- nbd0
	nbd[length(nbd)] <- nbd1
	ifull <- nbd.in == nbd
	notscaled.dates <- dates.scale[!ifull]
	notscaled.index <- index[!ifull]
	dates.scale <- dates.scale[ifull]
	index <- index[ifull]
	nbd.in <- nbd.in[ifull]

	if(length(notscaled.index) > 0){
		for(j in seq_along(notscaled.index)){
			nc.files0 <- ncInfoMrg$nc.files[notscaled.index[[j]]]
			nc.exist0 <- ncInfoMrg$exist[notscaled.index[[j]]]
			nc.files0 <- nc.files0[nc.exist0]
			if(length(nc.files0) == 0) next
			file.copy(nc.files0, file.path(ncParms$outdir, basename(nc.files0)))
		}
	}

	#######
	varid0 <- ncInfoMrg$ncinfo$varinfo$name
	units0 <- ncInfoMrg$ncinfo$varinfo$units
	prec0 <- ncInfoMrg$ncinfo$varinfo$prec
	missval0 <- ncInfoMrg$ncinfo$varinfo$missval
	longname0 <- ncInfoMrg$ncinfo$varinfo$longname
	xlon0 <- ncInfoMrg$ncinfo$lon
	xlat0 <- ncInfoMrg$ncinfo$lat
	xnlon0 <- ncInfoMrg$ncinfo$nx
	xnlat0 <- ncInfoMrg$ncinfo$ny

	#######
	## check if same grid
	is.diff.grid <- is.diffSpatialPixelsObj(defSpatialPixels(ncInfoMrg$ncinfo),
								defSpatialPixels(ncInfoScale$ncinfo), tol = 1e-07)
	if(is.diff.grid){
		Insert.Messages.Out("Grid from the two datasets did not match", format = TRUE)
		return(NULL)
	}

	#######
	## Def ncdf
	dx <- ncdim_def("Lon", "degreeE", xlon0)
	dy <- ncdim_def("Lat", "degreeN", xlat0)
	grd.nc.out <- ncvar_def(varid0, units0, list(dx, dy), missval0, longname = longname0, prec = prec0)

	#######
	if(ncParms$scale.data$fun == "sum") scale.fun <- colSums
	if(ncParms$scale.data$fun == "mean") scale.fun <- colMeans

	#######
	is.parallel <- doparallel(length(index) >= 100)
	`%parLoop%` <- is.parallel$dofun
	ret <- foreach(jj = seq_along(index), .packages = "ncdf4") %parLoop% {
		ix <- which(ncInfoScale$dates == dates.scale[[jj]])

		#######
		nc.files <- ncInfoMrg$nc.files[index[[jj]]]
		nc.exist <- ncInfoMrg$exist[index[[jj]]]
		nc.files <- nc.files[nc.exist]
		if(length(nc.files) == 0) return(NULL)
		if(length(nc.files) != nbd.in[jj] | length(ix) == 0 | !ncInfoScale$exist[ix]){
			file.copy(nc.files, file.path(ncParms$outdir, basename(nc.files)))
			return(NULL)
		}

		#######
		nc <- nc_open(ncInfoScale$nc.files[ix])
		scl.don <- ncvar_get(nc, varid = ncInfoScale$ncinfo$varid)
		nc_close(nc)
		scl.don <- transposeNCDFData(scl.don, ncInfoScale$ncinfo)

		#######
		mrg.don <- lapply(seq_along(nc.files), function(j){
			nc <- nc_open(nc.files[j])
			don <- ncvar_get(nc, varid = varid0)
			nc_close(nc)
			don <- transposeNCDFData(don, ncInfoMrg$ncinfo)
		})
		mrg.aggr <- matrix(scale.fun(do.call(rbind, lapply(mrg.don, c))), xnlon0, xnlat0)

		scale.mat <- scl.don / (mrg.aggr + 0.1)
		scale.mat[is.na(scale.mat) | is.nan(scale.mat) | is.infinite(scale.mat)] <- 1

		for(j in seq_along(mrg.don)){
			out <- scale.mat * mrg.don[[j]]
			out[is.na(out)] <- missval0
			outfl <- file.path(ncParms$outdir, basename(nc.files[j]))
			nc2 <- nc_create(outfl, grd.nc.out)
			ncvar_put(nc2, grd.nc.out, out)
			nc_close(nc2)
		}
		rm(out, scale.mat, mrg.aggr, mrg.don, scl.don)
		gc()
		return(0)
	}
	if(is.parallel$stop) stopCluster(is.parallel$cluster)
	Insert.Messages.Out('Scale data finished')

	return(0)
}
