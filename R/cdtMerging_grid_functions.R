
## gstat block size
createBlock <- function(cellsize, fac = 0.5, len = 4){
	sDX <- cellsize[1]*fac
	dBX <- seq(-sDX, sDX, length.out = len)
	sDY <- cellsize[2] * fac
	dBY <- seq(-sDY, sDY, length.out = len)
	bGrd <- expand.grid(x = dBX, y = dBY)
	return(bGrd)
}

## create grid for interpolation 
createGrid.merging <- function(grdData, ObjVar = NULL, coarse.grid = TRUE, res.coarse = 0.25){
	gridS <- data.frame(expand.grid(lon = grdData$x,
									lat = grdData$y),
						dem = c(grdData$z),
						slp = c(grdData$slp),
						asp = c(grdData$asp))
	coordinates(gridS) <- ~lon+lat

	gridS1 <- NULL
	gridS2 <- NULL
	idcoarse <- NULL
	idcoarse1 <- NULL

	if(coarse.grid){
		idcoarse <- indexCoarseGrid(grdData$x, grdData$y, res.coarse)
		gridS1 <- data.frame(expand.grid(lon = grdData$x[idcoarse$ix],
										lat = grdData$y[idcoarse$iy]),
							dem = c(grdData$z[idcoarse$ix, idcoarse$iy]),
							slp = c(grdData$slp[idcoarse$ix, idcoarse$iy]),
							asp = c(grdData$asp[idcoarse$ix, idcoarse$iy]))
		coordinates(gridS1) <- ~lon+lat
		if(!is.null(ObjVar)){
			idcoarse1 <- indexCoarseGrid(ObjVar$x, ObjVar$y, res.coarse)
			gridS2 <- data.frame(expand.grid(lon = ObjVar$x[idcoarse1$ix],
											lat = ObjVar$y[idcoarse1$iy]),
								dem = c(ObjVar$z[idcoarse1$ix, idcoarse1$iy]),
								slp = c(ObjVar$slp[idcoarse1$ix, idcoarse1$iy]),
								asp = c(ObjVar$asp[idcoarse1$ix, idcoarse1$iy]))
			coordinates(gridS2) <- ~lon+lat
		}
	}

	max.slope <- max(gridS$slp, na.rm = TRUE)
	max.aspect <- max(gridS$asp, na.rm = TRUE)
	min.dem <- min(gridS$dem, na.rm = TRUE)
	max.dem <- max(gridS$dem, na.rm = TRUE)

	gridS$dem <- (gridS$dem - min.dem) / (max.dem - min.dem)
	gridS$slp <- gridS$slp / max.slope
	gridS$asp <- gridS$asp / max.aspect

	if(!is.null(gridS1)){
		gridS1$dem <- (gridS1$dem - min.dem) / (max.dem - min.dem)
		gridS1$slp <- gridS1$slp / max.slope
		gridS1$asp <- gridS1$asp / max.aspect
	}
	if(!is.null(gridS2)){
		gridS2$dem <- (gridS2$dem - min.dem) / (max.dem - min.dem)
		gridS2$slp <- gridS2$slp / max.slope
		gridS2$asp <- gridS2$asp / max.aspect
	}

	return(list(newgrid = gridS, coords.coarse = gridS1, coords.var = gridS2,
				id.coarse = idcoarse, id.var = idcoarse1))
}

createGrid <- function(ObjStn, ObjGrd, ObjRfe = NULL, as.dim.elv = TRUE,
						latlong = 'km', normalize = FALSE, coarse.grid = TRUE,
						res.coarse = 0.25)
{
	lon2UTM <- function(lon) (floor((lon + 180)/6) %% 60) + 1
	lonO <- ObjStn$x
	latO <- ObjStn$y
	lonS <- ObjGrd$x
	latS <- ObjGrd$y
	gridO <- data.frame(lon = lonO, lat = latO, elv = ObjStn$z, dem = ObjStn$z, slp = ObjStn$slp, asp = ObjStn$asp)
	gridS <- data.frame(expand.grid(lon = lonS, lat = latS), elv = c(ObjGrd$z), dem = c(ObjGrd$z), slp = c(ObjGrd$slp), asp = c(ObjGrd$asp))

	ixOa <- is.na(gridO$elv)
	if(any(ixOa)){
		gridOa <- gridO[ixOa, c('lon', 'lat'), drop = FALSE]
		gridOa <- gstat::krige(formula = elv~1, locations = ~lon + lat, data = na.omit(gridS),
						newdata = gridOa, nmin = 3, nmax = 10, debug.level = 0)
		gridO$elv[ixOa] <- round(gridOa$var1.pred)
	}

	ixSa <- is.na(gridS$elv)
	if(any(ixSa)){
		gridSa <- gridS[ixSa, c('lon', 'lat'), drop = FALSE]
		gridSa <- gstat::krige(formula = elv~1, locations = ~lon + lat, data = na.omit(gridS),
						newdata = gridSa, nmin = 3, nmax = 10, debug.level = 0)
		gridS$elv[ixSa] <- round(gridSa$var1.pred)
		ObjGrd$z[ixSa] <- round(gridSa$var1.pred)
	}

	coordinates(gridO) <- ~lon + lat
	coordinates(gridS) <- ~lon + lat

	gridS1 <- NULL
	gridS2 <- NULL
	idcoarse <- NULL
	idcoarse1 <- NULL
	if(coarse.grid){
		idcoarse <- indexCoarseGrid(ObjGrd$x, ObjGrd$y, res.coarse)
		lonS1 <- ObjGrd$x[idcoarse$ix]
		latS1 <- ObjGrd$y[idcoarse$iy]
		demS1 <- c(ObjGrd$z[idcoarse$ix, idcoarse$iy])
		slpS1 <- c(ObjGrd$slp[idcoarse$ix, idcoarse$iy])
		aspS1 <- c(ObjGrd$asp[idcoarse$ix, idcoarse$iy])
		gridS1 <- data.frame(expand.grid(lon = lonS1, lat = latS1), elv = demS1, dem = demS1, slp = slpS1, asp = aspS1)
		coordinates(gridS1) <- ~lon+lat
		if(!is.null(ObjRfe)){
			idcoarse1 <- indexCoarseGrid(ObjRfe$x, ObjRfe$y, res.coarse)
			lonS2 <- ObjRfe$x[idcoarse1$ix]
			latS2 <- ObjRfe$y[idcoarse1$iy]
			demS2 <- c(ObjRfe$z[idcoarse1$ix, idcoarse1$iy])
			slpS2 <- c(ObjRfe$slp[idcoarse1$ix, idcoarse1$iy])
			aspS2 <- c(ObjRfe$asp[idcoarse1$ix, idcoarse1$iy])
			gridS2 <- data.frame(expand.grid(lon = lonS2, lat = latS2), elv = demS2, dem = demS2, slp = slpS2, asp = aspS2)
			coordinates(gridS2) <- ~lon + lat
		}
	}

	if(as.dim.elv){
		proj4string(gridO) <- CRS("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0")
		proj4string(gridS) <- CRS("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0")

		zone <- lon2UTM(sum(range(lonS)) / 2)
		orient <- if((sum(range(latS)) / 2) >= 0) 'north' else 'south'
		proj <- paste("+proj=utm +zone=", zone, " +", orient, " +datum=WGS84 +units=m +no_defs +ellps=WGS84 +towgs84=0,0,0", sep = "")

		gridO.utm <- spTransform(gridO, CRS(proj))
		gridS.utm <- spTransform(gridS, CRS(proj))

		gridO <- as(gridO.utm, 'data.frame')
		gridS <- as(gridS.utm, 'data.frame')

		origin <- rbind(apply(gridS[, c('lon', 'lat')],2,min), apply(gridO[, c('lon', 'lat')],2,min))
		origin <- apply(origin, 2, min)
		gridO[, c('lon', 'lat')] <- t(t(gridO[, c('lon', 'lat')]) - origin)
		gridS[, c('lon', 'lat')] <- t(t(gridS[, c('lon', 'lat')]) - origin)
		if(tolower(latlong) == 'km'){
			gridO[, c('lon', 'lat')] <- gridO[, c('lon', 'lat')] / 1000
			gridS[, c('lon', 'lat')] <- gridS[, c('lon', 'lat')] / 1000
		}
		if(normalize){
			mgrd <- apply(rbind(gridO[, c('lon', 'lat', 'elv')], gridS[, c('lon', 'lat', 'elv')]), 2, mean)
			sgrd <- apply(rbind(gridO[, c('lon', 'lat', 'elv')], gridS[, c('lon', 'lat', 'elv')]), 2, sd)
			gridO[, c('lon', 'lat', 'elv')] <- as.data.frame(t((t(gridO[, c('lon', 'lat', 'elv')]) - mgrd) / sgrd))
			gridS[, c('lon', 'lat', 'elv')] <- as.data.frame(t((t(gridS[, c('lon', 'lat', 'elv')]) - mgrd) / sgrd))
		}
		coordinates(gridO) <- ~lon + lat + elv
		coordinates(gridS) <- ~lon + lat + elv

		if(coarse.grid){
			proj4string(gridS1) <- CRS("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0")
			gridS1.utm <- spTransform(gridS1, CRS(proj))
			gridS1 <- as(gridS1.utm, 'data.frame')
			gridS1[, c('lon', 'lat')] <- t(t(gridS1[, c('lon', 'lat')]) - origin)
			if(tolower(latlong) == 'km') gridS1[, c('lon', 'lat')] <- gridS1[, c('lon', 'lat')] / 1000
			if(normalize){
				# mgrd1 <- apply(gridS1[, c('lon', 'lat', 'elv')], 2, mean)
				# sgrd1 <- apply(gridS1[, c('lon', 'lat', 'elv')], 2, sd)
				gridS1[, c('lon', 'lat', 'elv')] <- as.data.frame(t((t(gridS1[, c('lon', 'lat', 'elv')]) - mgrd) / sgrd))
			}
			coordinates(gridS1) <- ~lon + lat + elv
			
			if(!is.null(ObjRfe)){
				proj4string(gridS2) <- CRS("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0")
				gridS2.utm <- spTransform(gridS2, CRS(proj))
				gridS2 <- as(gridS2.utm, 'data.frame')
				gridS2[, c('lon', 'lat')] <- t(t(gridS2[, c('lon', 'lat')]) - origin)
				if(tolower(latlong) == 'km') gridS2[, c('lon', 'lat')] <- gridS2[, c('lon', 'lat')] / 1000
				if(normalize){
					# mgrd2 <- apply(gridS2[, c('lon', 'lat', 'elv')], 2, mean)
					# sgrd2 <- apply(gridS2[, c('lon', 'lat', 'elv')], 2, sd)
					gridS2[, c('lon', 'lat', 'elv')] <- as.data.frame(t((t(gridS2[, c('lon', 'lat', 'elv')]) - mgrd) / sgrd))
				}
				coordinates(gridS2) <- ~lon + lat + elv
			}
		}
	}
	max.slope <- 1.570796 #change 90 if degree
	max.aspect <- 360
	min.dem <- min(gridS$dem, na.rm = TRUE)
	max.dem <- max(gridS$dem, na.rm = TRUE)

	gridO$dem <- (gridO$dem - min.dem) / (max.dem - min.dem)
	gridO$slp <- gridO$slp / max.slope
	gridO$asp <- gridO$asp / max.aspect

	gridS$dem <- (gridS$dem - min.dem) / (max.dem - min.dem)
	gridS$slp <- gridS$slp / max.slope
	gridS$asp <- gridS$asp / max.aspect

	if(!is.null(gridS1)){
		gridS1$dem <- (gridS1$dem - min.dem) / (max.dem - min.dem)
		gridS1$slp <- gridS1$slp / max.slope
		gridS1$asp <- gridS1$asp / max.aspect
	}
	if(!is.null(gridS2)){
		gridS2$dem <- (gridS2$dem - min.dem) / (max.dem - min.dem)
		gridS2$slp <- gridS2$slp / max.slope
		gridS2$asp <- gridS2$asp / max.aspect
	}

	return(list(coords.stn = gridO, coords.grd = gridS1, coords.rfe = gridS2,
				newgrid = gridS, idxy = idcoarse, idxy.rfe = idcoarse1))
}


## get Coarse grid from matrix data with dim lon/lat
indexCoarseGrid <- function(lon, lat, res = 0.25){
	res <- if(length(res) > 1) res[1:2] else c(res, res)
	nlon <- length(lon)
	nlat <- length(lat)
	ilon <- diff(range(lon)) / (nlon - 1)
	ilat <- diff(range(lat)) / (nlat - 1)
	ix <- round(res[1] / ilon) - 1
	iy <- round(res[2] / ilat) - 1
	if(ix < 1) ix <- 1
	if(iy < 1) iy <- 1
	ix <- seq(1, nlon, ix)
	iy <- seq(1, nlat, iy)
	nx <- length(ix)
	ny <- length(iy)
	if(ix[nx] < nlon)
		ix <- if((lon[nlon] - lon[ix[nx]]) < res[1]/2) c(ix[-nx], nlon) else c(ix, nlon)
	if(iy[ny] < nlat)
		iy <- if((lat[nlat] - lat[iy[ny]]) < res[2]/2) c(iy[-ny], nlat) else c(iy, nlat)
	return(list(ix = ix, iy = iy))
}

## create grid for stations data 
createGrid.StnData <- function(donne.stn, ijGrd, newgrid, min.stn, weighted = FALSE){
	ij <- !is.na(donne.stn$stn)
	donne.stn <- donne.stn[ij, , drop = FALSE]
	if(nrow(donne.stn) < min.stn) return(NULL)
	ijGrd <- ijGrd[ij]
	idx <- split(seq_along(ijGrd), ijGrd)

	if(any(sapply(idx, length) > 1)){
		w <- rep(1, length(ijGrd))
		if(weighted){
			idup <- duplicated(ijGrd) | duplicated(ijGrd, fromLast = TRUE)
			stn.grd <- newgrid@coords[ijGrd, ]
			dist <- 1 / ((stn.grd[idup, 1] - donne.stn$lon[idup])^2 + (stn.grd[idup, 2] - donne.stn$lat[idup])^2)
			dist[is.infinite(dist)] <- 2 * max(dist[!is.infinite(dist)])
			w[idup] <- dist
		}
		val <- sapply(idx, function(j) sum(w[j] * donne.stn$stn[j]) / sum(w[j]))
	}else val <- donne.stn$stn[unlist(idx)]

	ij <- as.numeric(names(idx))
	stng <- rep(NA, length(newgrid))
	stng[ij] <- val
	return(stng)
}
