
outputADTest <- function(X, months = 1:12, distr = "berngamma"){
	H0.test <- vector(mode = 'list', length = 12)
	H0.test[months] <- lapply(X[months], function(mon){
		sapply(mon, function(stn) if(!is.null(stn)) stn[[distr]]$h0 else 'null')
	})
	return(H0.test)
}

extractDistrParams <- function(X, months = 1:12, distr = "berngamma"){
	pars <- vector(mode = 'list', length = 12)
	pars[months] <- lapply(X[months], function(mon){
		parstn <- lapply(mon, function(stn){
			if(!is.null(stn)){
				fitdist <- stn[[distr]]$fitted.distr
				if(!is.null(fitdist)) fitdist$estimate else NA
			}else NA
		})
		nom <- na.omit(do.call('rbind', lapply(parstn, function(x) if(length(x) > 1) names(x) else NA)))[1, ]
		parstn <- do.call('rbind', parstn)
		dimnames(parstn)[[2]] <- nom
		parstn
	})
	pars[months] <- rapply(pars[months], f = function(x) ifelse(is.nan(x) | is.infinite(x), NA, x), how = "replace")
	return(pars)
}

outputSWNTest <- function(X, months = 1:12){
	H0.test <- vector(mode = 'list', length = 12)
	H0.test[months] <- lapply(X[months], function(mon){
		sapply(mon, function(stn) if(!is.null(stn)) stn$h0 else 'null')
	})
	return(H0.test)
}

extractNormDistrParams <- function(X, months = 1:12){
	pars <- vector(mode = 'list', length = 12)
	pars[months] <- lapply(X[months], function(mon){
		parstn <- lapply(mon, function(stn){
			if(!is.null(stn)){
				fitdist <- stn$fitted.distr
				if(!is.null(fitdist)) fitdist$estimate else NA
			}else NA
		})
		nom <- na.omit(do.call('rbind', lapply(parstn, function(x) if(length(x) > 1) names(x) else NA)))[1, ]
		parstn <- do.call('rbind', parstn)
		dimnames(parstn)[[2]] <- nom
		parstn
	})
	pars[months] <- rapply(pars[months], f = function(x) ifelse(is.nan(x) | is.infinite(x), NA, x), how = "replace")
	return(pars)
}

raster.slope.aspect <- function(dem){
	dem <- raster::raster(dem)
	slope <- raster::terrain(dem, opt = "slope", unit = 'degrees', neighbors = 8) 
	aspect <- raster::terrain(dem, opt = "aspect", unit = 'degrees', neighbors = 8) 
	slope <- raster::as.matrix(slope)
	slope <- t(slope)
	revCol <- rev(seq(ncol(slope)))
	slope <- slope[, revCol]
	aspect <- raster::as.matrix(aspect)
	aspect <- t(aspect)
	revCol <- rev(seq(ncol(aspect)))
	aspect <- aspect[, revCol]
	list(slope = slope, aspect = aspect)
}

quantile.mapping.BGamma <- function(x, pars.stn, pars.rfe, rfe.zero){
	res <- x
	p.rfe <- 1 - pars.rfe$prob
	ix <- !is.na(x) & (x > 0)
	pgam <- pgamma(x[ix], scale = pars.rfe$scale[ix], shape = pars.rfe$shape[ix])
	p.rfe[ix] <- p.rfe[ix] + pars.rfe$prob[ix] * pgam
	p.rfe[p.rfe > 0.999] <- 0.99
	ip <- p.rfe > (1 - pars.stn$prob)
	pp <- (pars.stn$prob[ip] + p.rfe[ip] - 1) / pars.stn$prob[ip]
	pp[pp > 0.999] <- 0.99
	res[ip] <- qgamma(pp, scale = pars.stn$scale[ip], shape = pars.stn$shape[ip])
	miss <- is.na(res) | is.nan(res) | is.infinite(res)
	res[miss] <- x[miss]
	res[is.na(x)] <- NA
	if(rfe.zero) res[x == 0] <- 0
	moy <- pars.rfe$shape*pars.rfe$scale
	ssd <- sqrt(pars.rfe$shape * pars.rfe$scale^2)
	ix <- (res - moy) / ssd > 3
	ix[is.na(ix)] <- FALSE
	res[ix] <- x[ix]
	return(res)
}

quantile.mapping.Gau <- function(x, pars.stn, pars.reanal){
	p.reanal <- x
	ix <- !is.na(x)
	p.reanal[ix] <- pnorm(x[ix], mean = pars.reanal$mean[ix], sd = pars.reanal$sd[ix])
	p.reanal[ix][p.reanal[ix] < 0.001] <- 0.01
	p.reanal[ix][p.reanal[ix] > 0.999] <- 0.99
	res <- qnorm(p.reanal, mean = pars.stn$mean, sd = pars.stn$sd)
	miss <- is.na(res) | is.nan(res) | is.infinite(res)
	res[miss] <- x[miss]
	ix <- (res - pars.reanal$mean) / pars.reanal$sd > 3
	ix[is.na(ix)] <- FALSE
	res[ix] <- x[ix]
	return(res)
}

writeNC.merging <- function(mat, daty, tstep, grid.nc, dir2save, file.format, missval = -99)
{
	year <- substr(daty, 1, 4)
	month <- substr(daty, 5, 6)
	if(tstep == 'daily'){
		mrgfrmt <- sprintf(file.format, year, month, substr(daty, 7, 8))
	}else if(tstep %in% c('pentad', 'dekadal')){
		mrgfrmt <- sprintf(file.format, year, month, substr(daty, 7, 7))
	}else mrgfrmt <- sprintf(file.format, year, month)

	mat[is.na(mat)] <- missval

	outfl <- file.path(dir2save, mrgfrmt)
	nc <- nc_create(outfl, grid.nc)
	ncvar_put(nc, grid.nc, mat)
	nc_close(nc)
}

cdt.merging.functions <- function(locations.stn, newgrid, 
								merging.method, interp.method,
								formule, formuleRK,
								maxdist, nmin, nmax, vgm.model,
								neg.value, nc.date, MODEL.COEF, ijGrd)
{

	nx <- newgrid@grid@cells.dim[1]
	ny <- newgrid@grid@cells.dim[2]
	xy.grid <- create_grid_buffer(locations.stn, newgrid, maxdist, FALSE)

	newdata0 <- xy.grid$grid.buff

	igrid <- xy.grid$ij
	coarsegrid <- xy.grid$coarse
	coarse.df <- as.data.frame(!is.na(coarsegrid@data))
	coarsegrid <- coarsegrid[Reduce("&", coarse.df), ]
	coarsegrid$stn <- coarsegrid$grd
	coarsegrid$res <- rep(0, length(coarsegrid))

	######### sp.trend & residuals

	sp.trend <- newdata0@data$grd
	xres <- locations.stn$stn - locations.stn$grd

	## RK
	if(merging.method == "Regression Kriging"){
		if(var(locations.stn$stn) < 1e-07 | var(locations.stn$grd, na.rm = TRUE) < 1e-07){
			cat(paste(nc.date, ":", "Zero variance", "|", "Simple Bias Adjustment", "\n"),
				file = log.file, append = TRUE)
		}else{
			glm.stn <- glm(formuleRK, data = locations.stn, family = gaussian)
			if(is.na(glm.stn$coefficients[2]) | glm.stn$coefficients[2] < 0){
				cat(paste(nc.date, ":", "Invalid GLM coeffs", "|", "Simple Bias Adjustment", "\n"),
					file = log.file, append = TRUE)
			}else{
				sp.trend <- predict(glm.stn, newdata = newdata0)
				ina.out <- is.na(sp.trend)
				sp.trend[ina.out] <- newdata0@data$grd[ina.out]
				xres <- rep(NA, length(locations.stn))
				if(length(glm.stn$na.action) > 0)
					xres[-glm.stn$na.action] <- glm.stn$residuals
				else
					xres <- glm.stn$residuals
			}
		}
	}

	if(merging.method == "Spatio-Temporal LM"){
		mo <- as(substr(nc.date, 5, 6), 'numeric')
		sp.trend <- matrix(newgrid$grd, nx, ny) * MODEL.COEF[[mo]]$slope + MODEL.COEF[[mo]]$intercept
		sp.trend <- c(sp.trend)
		iloc <- over(as(locations.stn, "SpatialPoints"), as(newgrid, "SpatialPixels"))
		xres <- locations.stn$stn - sp.trend[iloc]
		sp.trend <- sp.trend[igrid]
	}

	locations.stn$res <- xres

	#########

	vgm <- NULL
	if(interp.method == 'Kriging'){
		loc.stn <- as(locations.stn, "SpatialPointsDataFrame")
		calc.vgm <- if(length(loc.stn$res) > 7 & var(loc.stn$res) > 1e-15) TRUE else FALSE
		if(calc.vgm){
			vgm <- try(automap::autofitVariogram(formule, input_data = loc.stn, model = vgm.model, cressie = TRUE), silent = TRUE)
			if(!inherits(vgm, "try-error")){
				vgm <- vgm$var_model
			}else{
				cat(paste(nc.date, ":", "Unable to compute variogram", "|", "Interpolation using IDW", "\n"),
					file = log.file, append = TRUE)
				vgm <- NULL
			}
		}else{
			cat(paste(nc.date, ":", "Unable to compute variogram", "|", "Interpolation using IDW", "\n"),
				file = log.file, append = TRUE)
			vgm <- NULL
		}
	}

	#########

	row.names(locations.stn) <- 1:length(locations.stn)
	row.names(coarsegrid) <- length(locations.stn) + (1:length(coarsegrid))
	locations.stn <- maptools::spRbind(locations.stn, coarsegrid)

	#########

	res.grd <- gstat::krige(formule, locations = locations.stn, newdata = newdata0, model = vgm, nmin = nmin, nmax = nmax, debug.level = 0)
	xtrm <- range(locations.stn$res, na.rm = TRUE)
	extrm1 <- xtrm[1] - diff(xtrm) * 0.05
	extrm2 <- xtrm[2] + diff(xtrm) * 0.05
	res.grd$var1.pred[!is.na(res.grd$var1.pred) & res.grd$var1.pred < extrm1] <- extrm1
	res.grd$var1.pred[!is.na(res.grd$var1.pred) & res.grd$var1.pred > extrm2] <- extrm2

	resid <- rep(0, length(newgrid))
	resid[igrid] <- res.grd$var1.pred
	resid[is.na(resid)] <- 0
	resid <- matrix(resid, ncol = ny, nrow = nx)
	res0 <- resid[ijGrd]
	resid <- smooth.matrix(resid, 3)
	resid[ijGrd] <- res0

	out.mrg <- newgrid@data$grd
	out.mrg[igrid] <- sp.trend + resid[igrid]
	if(!neg.value) out.mrg[out.mrg < 0] <- 0
	ina <- is.na(out.mrg)
	out.mrg[ina] <- newgrid@data$grd[ina]
	out.mrg <- matrix(out.mrg, ncol = ny, nrow = nx)

	return(out.mrg)
}

rain_no_rain.mask <- function(locations.stn, newgrid, pars.RnoR)
{
	wet.day <- pars.RnoR$wet.day
	if(wet.day <= 0) wet.day <- wet.day + 1e-13
	rnr.grd <- ifelse(newgrid@data$grd < wet.day, 0, 1)
	ij <- over(locations.stn, as(newgrid, "SpatialPixels"))
	locations.stn$rnr.stn <- ifelse(locations.stn$stn < wet.day, 0, 1)
	locations.stn$rnr.grd <- ifelse(rnr.grd[ij] < wet.day, 0, 1)

	glm.binom <- glm(rnr.stn ~ rnr.grd, data = locations.stn, family = binomial(link = "logit"))

	nlon <- newgrid@grid@cells.dim[1]
	nlat <- newgrid@grid@cells.dim[2]
	rnr <- matrix(1, ncol = nlat, nrow = nlon)
	if(!is.na(glm.binom$coef[2])){
		locations.stn$rnr.res <- residuals(glm.binom)
		rnr.trend <- predict(glm.binom, newdata = newgrid, type = 'link')

		rnr.res.grd <- gstat::krige(rnr.res~1, locations = locations.stn, newdata = newgrid, debug.level = 0)
		rnr <- rnr.trend + rnr.res.grd$var1.pred

		rnr <- exp(rnr) / (1 + exp(rnr))
		### decision boundary 0.5
		rnr[rnr >= 0.5] <- 1
		rnr[rnr < 0.5] <- 0
		rnr <- matrix(rnr, ncol = nlat, nrow = nlon)
		rnr[is.na(rnr)] <- 1
		if(pars.RnoR$smooth.RnoR) rnr <- smooth.matrix(rnr, 2)
	}

	return(rnr)
}

