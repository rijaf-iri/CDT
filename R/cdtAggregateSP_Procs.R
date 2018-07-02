
AggregateSpNc_Execute <- function(){
	Insert.Messages.Out(.cdtData$GalParams[['message']][['3']])

	outputdir <- .cdtData$GalParams$output
	if(is.na(outputdir) | outputdir %in% c("", "NA")){
		Insert.Messages.Out(.cdtData$GalParams[['message']][['4']], format = TRUE)
		return(NULL)
	}
	outputNC <- file.path(outputdir, "Regrided_NetCDF")
	dir.create(outputNC, showWarnings = FALSE, recursive = TRUE)

	if(.cdtData$GalParams$nb.ncfile == "one"){
		ncDataInfo <- getNCDFSampleData(.cdtData$GalParams$ncdf$fileordir)
		if(is.null(ncDataInfo)){
			Insert.Messages.Out(.cdtData$GalParams[['message']][['5']], format = TRUE)
			return(NULL)
		}
	}

	if(.cdtData$GalParams$nb.ncfile == "several"){
		ncDataInfo <- getNCDFSampleData(.cdtData$GalParams$ncdf$sample)
		if(is.null(ncDataInfo)){
			Insert.Messages.Out(.cdtData$GalParams[['message']][['6']], format = TRUE)
			return(NULL)
		}
	}
	ncinfo <- list(xo = ncDataInfo$ilon, yo = ncDataInfo$ilat, varid = ncDataInfo$varid)
	old.grid <- defSpatialPixels(ncDataInfo[c('lon', 'lat')])

	if(.cdtData$GalParams$ncdf.grid$use.ncgrid){
		ncDataNew <- getNCDFSampleData(.cdtData$GalParams$ncdf.grid$file)
		if(is.null(ncDataNew)){
			Insert.Messages.Out(.cdtData$GalParams[['message']][['7']], format = TRUE)
			return(NULL)
		}
		grd.lon <- ncDataNew$lon
		grd.lat <- ncDataNew$lat
	}else{
		X0 <- .cdtData$GalParams$res$minlon
		X1 <- .cdtData$GalParams$res$maxlon
		Y0 <- .cdtData$GalParams$res$minlat
		Y1 <- .cdtData$GalParams$res$maxlat
		pX <- .cdtData$GalParams$res$reslon
		pY <- .cdtData$GalParams$res$reslat
		grd.lon <- seq(X0, X1, pX)
		grd.lat <- seq(Y0, Y1, pY)

		if(.cdtData$GalParams$method != "bilinear"){
			if(grd.lon[length(grd.lon)] + pX/2 < X1) grd.lon <- c(grd.lon, grd.lon[length(grd.lon)] + pX)
			if(grd.lat[length(grd.lat)] + pY/2 < Y1) grd.lat <- c(grd.lat, grd.lat[length(grd.lat)] + pY)
		}
	}
	new.grid <- defSpatialPixels(list(lon = grd.lon, lat = grd.lat))

	is.regridNC <- is.diffSpatialPixelsObj(old.grid, new.grid, tol = 1e-07)
	if(!is.regridNC){
		Insert.Messages.Out(.cdtData$GalParams[['message']][['8']])
		return(0)
	}

	if(!.cdtData$GalParams$ncdf.grid$use.ncgrid){
		if(.cdtData$GalParams$but == "Aggregate"){
			if(all(new.grid@grid@cellsize < old.grid@grid@cellsize)){
				Insert.Messages.Out(.cdtData$GalParams[['message']][['9']])
				return(0)
			}
		}else{
			if(all(old.grid@grid@cellsize < new.grid@grid@cellsize)){
				Insert.Messages.Out(.cdtData$GalParams[['message']][['10']])
				return(0)
			}
		}
	}

	if(.cdtData$GalParams$method == "bilinear"){
		gridInterp <- list(lon = grd.lon, lat = grd.lat)
		grid.dim <- c(length(grd.lon), length(grd.lat))
	}else{
		## maybe use raster::extract
		ixy <- over(old.grid, new.grid)
		grid.dim <- new.grid@grid@cells.dim
	}

	if(.cdtData$GalParams$nb.ncfile == "one"){
		jfile <- getIndex.AllOpenFiles(.cdtData$GalParams$ncdf$fileordir)
		ncfile <- .cdtData$OpenFiles$Data[[jfile]][[3]]

		nc <- nc_open(ncfile)
		xlon <- nc$var[[ncinfo$varid]]$dim[[ncinfo$xo]]$vals
		xlat <- nc$var[[ncinfo$varid]]$dim[[ncinfo$yo]]$vals
		zval <- ncvar_get(nc, varid = ncinfo$varid)

		nc.name <- ncinfo$varid
		nc.longname <- nc$var[[nc.name]]$longname
		nc.units <- nc$var[[nc.name]]$units
		nc.missval <- nc$var[[nc.name]]$missval
		nc.prec <- nc$var[[nc.name]]$prec
		nc_close(nc)

		xo <- order(xlon)
		xlon <- xlon[xo]
		yo <- order(xlat)
		xlat <- xlat[yo]
		zval <- if(ncinfo$xo < ncinfo$yo) zval[xo, yo] else t(zval)[xo, yo]

		dx <- ncdim_def("Lon", "degreeE", grd.lon)
		dy <- ncdim_def("Lat", "degreeN", grd.lat)
		grd.nc.out <- ncvar_def(nc.name, nc.units, list(dx, dy), nc.missval, longname = nc.longname, prec = nc.prec)

		if(.cdtData$GalParams$method == "bilinear"){
			z.out <- cdt.interp.surface.grid(list(lon = xlon, lat = xlat, z = zval), gridInterp)
			z.out <- z.out$z
			z.out[is.na(z.out)] <- nc.missval
		}else{
			z.out <- matrix(NA, grid.dim[1], grid.dim[2])
			out <- tapply(c(zval), ixy, mean, na.rm = TRUE)
			z.out[as.numeric(names(out))] <- out
			z.out[is.na(z.out) | is.nan(z.out) | is.infinite(z.out)] <- nc.missval
		}

		outfl <- file.path(outputNC, basename(ncfile))
		nc2 <- nc_create(outfl, grd.nc.out)
		ncvar_put(nc2, grd.nc.out, z.out)
		nc_close(nc2)
	}

	if(.cdtData$GalParams$nb.ncfile == "several"){
		allncfiles <- list.files(.cdtData$GalParams$ncdf$fileordir, ".nc", full.names = TRUE)
		fexist <- sapply(allncfiles, file.exists)
		if(length(fexist) == 0){
			Insert.Messages.Out(.cdtData$GalParams[['message']][['11']], format = TRUE)
			return(NULL)
		}
		
		allncfiles <- allncfiles[fexist]
		if(length(allncfiles) == 0){
			Insert.Messages.Out(.cdtData$GalParams[['message']][['11']], format = TRUE)
			return(NULL)
		}

		nc <- nc_open(allncfiles[1])
		xlon <- nc$var[[ncinfo$varid]]$dim[[ncinfo$xo]]$vals
		xlat <- nc$var[[ncinfo$varid]]$dim[[ncinfo$yo]]$vals
		nc.name <- ncinfo$varid
		nc.longname <- nc$var[[nc.name]]$longname
		nc.units <- nc$var[[nc.name]]$units
		nc.missval <- nc$var[[nc.name]]$missval
		nc.prec <- nc$var[[nc.name]]$prec
		nc_close(nc)

		xo <- order(xlon)
		xlon <- xlon[xo]
		yo <- order(xlat)
		xlat <- xlat[yo]
		xnlon <- length(xlon)
		xnlat <- length(xlat)

		dx <- ncdim_def("Lon", "degreeE", grd.lon)
		dy <- ncdim_def("Lat", "degreeN", grd.lat)
		grd.nc.out <- ncvar_def(nc.name, nc.units, list(dx, dy), nc.missval, longname = nc.longname, prec = nc.prec, compression = 9)

		for(jj in seq_along(allncfiles)){
			nc <- nc_open(allncfiles[jj])
			zval <- ncvar_get(nc, varid = ncinfo$varid)
			nc_close(nc)
			if(any(dim(zval) != c(xnlon, xnlat))){
				Insert.Messages.Out(paste(basename(allncfiles[jj]), "and", .cdtData$GalParams$ncdf$sample, "have different dimension"), format = TRUE)
				next
			}
			zval <- if(ncinfo$xo < ncinfo$yo) zval[xo, yo] else t(zval)[xo, yo]

			if(.cdtData$GalParams$method == "bilinear"){
				z.out <- cdt.interp.surface.grid(list(lon = xlon, lat = xlat, z = zval), gridInterp)
				z.out <- z.out$z
				z.out[is.na(z.out)] <- nc.missval
			}else{
				z.out <- matrix(NA, grid.dim[1], grid.dim[2])
				out <- tapply(c(zval), ixy, mean, na.rm = TRUE)
				z.out[as.numeric(names(out))] <- out
				z.out[is.na(z.out) | is.nan(z.out) | is.infinite(z.out)] <- nc.missval
			}

			outfl <- file.path(outputNC, basename(allncfiles[jj]))
			nc2 <- nc_create(outfl, grd.nc.out)
			ncvar_put(nc2, grd.nc.out, z.out)
			nc_close(nc2)
		}
	}

	return(0)
}

