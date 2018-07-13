
## check OS type
WindowsOS <- function() if(Sys.info()["sysname"] == "Windows") TRUE else FALSE
MacOSXP <- function() if(Sys.info()["sysname"] %in% c("darwin", "Darwin")) TRUE else FALSE
LinuxOS <- function() if(Sys.info()["sysname"] == "Linux") TRUE else FALSE

##############################################
## Test Internet connection
testConnection <- function(url = "https://cloud.r-project.org") {
    if(!as.logical(capabilities(what = "http/ftp"))) return(FALSE)
    test <- try(suppressWarnings(readLines(url, n = 1)), silent = TRUE)
    ifelse(inherits(test, "try-error"), FALSE, TRUE)
}

##############################################
## Create parallel loop
doparallel <- function(condition, nb.cores = detectCores() - 1,
						okpar = if(nb.cores < 3) FALSE else TRUE)
{
	if(okpar & condition){
		klust <- makeCluster(nb.cores)
		registerDoParallel(klust)
		`%dofun%` <- `%dopar%`
		closeklust <- TRUE
	}else{
		klust <- NULL
		`%dofun%` <- `%do%`
		closeklust <- FALSE
	}
	list(dofun = `%dofun%`, cluster = klust, stop = closeklust)
}

##############################################
## convert matrix or data.frame, to numeric, character, trim, ...
## by keeping the dimension
## mat <- apply(mat, 2, as.character)
convert_data_type <- function(mat, as.fun){
	dim.mat <- dim(mat)
	mat <- as.fun(mat)
	dim(mat) <- dim.mat
	return(mat)
}

##############################################

## Transpose matrix
transPose <- function(X){
	if(is.null(dim(X))) matrix(X, ncol = 1) else t(X)
}

##############################################

## Test leap year

is.leapyear <- function(year){
	leap <- year %% c(4, 100, 400)
	((leap[1] == 0) & (leap[2] != 0)) | (leap[3] == 0)
}

is.leapyears <- function(years){
	leap <- sapply(years, function(x) x %% c(4, 100, 400))
	((leap[1, ] == 0) & (leap[2, ] != 0)) | (leap[3, ] == 0)
}

##############################################

## Number of days
# daty: character representing the date

nb.Day.Of.Year <- function(daty){
	year <- as.numeric(substr(daty, 1, 4))
	ifelse(is.leapyears(year), 366, 365)
}

Day.Of.Month <- function(year, mon){
	rev((28:31)[!is.na(as.Date(paste(year, mon, 28:31, sep = '-')))])[1]
}

nb.Day.Of.Month <- function(daty){
	nbm <- mapply(Day.Of.Month, substr(daty, 1, 4), substr(daty, 5, 6))
	as.numeric(unname(nbm))
}

nb.Day.Of.Pentad <- function(daty){
	day <- as.numeric(substr(daty, 7, 7))
	nbp <- rep(5, length(daty))
	nbp[day >= 6] <- nb.Day.Of.Month(daty[day == 6]) - 25
	return(nbp)
}

nb.Day.Of.Dekad <- function(daty){
	day <- as.numeric(substr(daty, 7, 7))
	nbd <- rep(10, length(daty))
	nbd[day == 3] <- nb.Day.Of.Month(daty[day == 3]) - 20
	return(nbd)
}

# nbDayOfPentad <- function(daty){
# 	year <- substr(daty, 1, 4)
# 	mon <- substr(daty, 5, 6)
# 	day <- as.numeric(substr(daty, 7, 8))
# 	ifelse(day >= 25, rev((28:31)[!is.na(as.Date(paste(year, mon, 28:31, sep = '-')))])[1] - 25, 5)
# }

# nbDayOfDekad <- function(daty){
# 	year <- substr(daty, 1, 4)
# 	mon <- substr(daty, 5, 6)
# 	day <- as.numeric(substr(daty, 7, 8))
# 	ifelse(day >= 20, rev((28:31)[!is.na(as.Date(paste(year, mon, 28:31, sep = '-')))])[1] - 20, 10)
# }

# nbDayOfMonth <- function(daty){
# 	year <- substr(daty, 1, 4)
# 	mon <- substr(daty, 5, 6)
# 	rev((28:31)[!is.na(as.Date(paste(year, mon, 28:31, sep = '-')))])[1]
# }

# nbDayOfYear <- function(daty){
# 	year <- as.numeric(substr(daty, 1, 4))
# 	if(is.leapyear(year)) 366 else 365
# }

##############################################

## Cycle month,
# start: month, n: number of month (DJF, start = 'December', n = 3)
# full = TRUE (July),  FALSE (Jul)
# return end month

cycleMonth <- function(start, n, full = FALSE){
	frmt <- if(full) "%B" else "%b"
	mois <- format(ISOdate(2014, 1:12, 1), frmt)
	ix <- which(mois == start)
	im <- (ix + (n-1)) %% 12
	if(im == 0) im <- 12
	return(mois[im])
}

##############################################

## Add or subtract months
# daty: object of class "Date"

addMonths <- function(daty, n = 1){
	foo <- function(daty, n){
		date0 <- seq(daty, by = paste(n, "months"), length = 2)[2]
		date1 <- seq(as.Date(paste(format(daty, '%Y-%m'), '01', sep = '-')),
					by = paste(n + 1, "months"), length = 2)[2] - 1
		if(date0 > date1) date1 else date0
	}
	daty <- if(length(daty) == 1) foo(daty, n) else do.call(c, lapply(daty, foo, n))
	return(daty)
}
# addMonthsVec <- function(daty, n = 1) do.call(c, lapply(daty, addMonths, n))

##############################################

## Add or subtract dekads
# daty: object of class "Date"

addDekads <- function(daty, n = 1){
	foo <- function(daty, n){
		idek <- as.numeric(format(daty, '%d')) + n
		dek <- idek %% 3
		if(dek == 0) dek <- 3
		daty <- format(addMonths(daty, floor((idek - 1) / 3)), '%Y-%m')
		as.Date(paste(daty, dek, sep = '-'))
	}
	daty <- if(length(daty) == 1) foo(daty, n) else do.call(c, lapply(daty, foo, n))
	return(daty)
}

# addDekadsVec <- function(daty, n = 1) do.call(c, lapply(daty, addDekads, n))

##############################################

## Add or subtract pentads
# daty: object of class "Date"

addPentads <- function(daty, n = 1){
	foo <- function(daty, n){
		ipen <- as.numeric(format(daty, '%d')) + n
		pen <- ipen %% 6
		if(pen == 0) pen <- 6
		daty <- format(addMonths(daty, floor((ipen - 1) / 6)), '%Y-%m')
		as.Date(paste(daty, pen, sep = '-'))
	}
	daty <- if(length(daty) == 1) foo(daty, n) else do.call(c, lapply(daty, foo, n))
	return(daty)
}

# addPentadsVec <- function(daty, n = 1) do.call(c, lapply(daty, addPentads, n))

##############################################

table.annuel <- function(){
	uneAnne <- seq(as.Date('2014-1-1'), by = 'day', length.out = 365)
	day <- as.numeric(format(uneAnne, "%d"))
	mon <- as.numeric(format(uneAnne, "%m"))
	vtimes <- cbind(day, mon, 1:365)
	vtimes
}

##############################################
## File name or path without extension
# getf.no.ext <- function(flname){
# 	ig <- grep('\\.', flname)
# 	if(length(ig) == 0){
# 		fret <- flname
# 	}else{
# 		fsplit <- unlist(strsplit(flname, '\\.'))
# 		fret <- sub(paste0('.', fsplit[length(fsplit)]), '', flname)
# 	}
# 	return(fret)
# }
file.sans.ext <- function(x) file_path_sans_ext(x)

##############################################
## Parameters Initialization 
initialize.parameters <- function(action, tstep = 'dekadal'){
	initpars <- cdt.init.params(action, tstep)
	if(!is.null(.cdtData$GalParams))
		if(!is.null(.cdtData$GalParams$action))
			if(.cdtData$GalParams$action == action)
				initpars <- .cdtData$GalParams
	.cdtData$GalParams <- initpars
}

# initialize.parameters <- function(action, tstep = 'dekadal', previous = FALSE){
# 	initpars <- cdt.init.params(action, tstep)
# 	if(!is.null(.cdtData$GalParams))
# 		if(!is.null(.cdtData$GalParams$action))
# 			if(.cdtData$GalParams$action == action){
# 				if(previous)
# 					initpars <- cdt.init.params(action, .cdtData$GalParams$tstep)
# 				else
# 					initpars <- .cdtData$GalParams
# 			}
# 	.cdtData$GalParams <- initpars
# }

##############################################
## Get index of selected file from .cdtData$OpenFiles$Data
getIndex.AllOpenFiles <- function(nomfile){
	if(inherits(nomfile, "tclVar")){
		fileio <- tclvalue(nomfile)
	}else if(is.character(nomfile)){
		fileio <- nomfile
	}else return(NULL)

	if(fileio != ""){
		all.open.file <- do.call(c, lapply(.cdtData$OpenFiles$Data, "[[", 1))
		jfile <- which(all.open.file == fileio)
		return(jfile)
	}

	return(NULL)
}

##############################################
## Get stn data in the list (all open files)
## return CDT data format
getStnOpenData <- function(file.stnfl){
	jfile <- getIndex.AllOpenFiles(file.stnfl)
	donne <- NULL
	if(length(jfile) > 0){
		if(.cdtData$OpenFiles$Type[[jfile]] == "ascii")
			donne <- .cdtData$OpenFiles$Data[[jfile]][[2]]
	}
	return(donne)
}

getStnOpenDataInfo <- function(file.stnfl){
	jfile <- getIndex.AllOpenFiles(file.stnfl)
	info <- NULL
	if(length(jfile) > 0){
		if(.cdtData$OpenFiles$Type[[jfile]] == "ascii")
			info <- .cdtData$OpenFiles$Data[[jfile]][c(1, 3:4)]
	}
	return(info)
}

getCDTdataAndDisplayMsg <- function(donne, tstep, filename){
	if(is.null(donne)) return(NULL)
	donne <- splitCDTData(donne, tstep)
	if(is.null(donne)) return(NULL)

	outlist <- list()
	if(!is.null(donne$duplicated.stnID))
		outlist <- c(outlist, list('Duplicated Station IDs', as.matrix(donne$duplicated.stnID)))
	if(!is.null(donne$duplicated.coords))
		outlist <- c(outlist, list('Duplicated coordinates', as.matrix(donne$duplicated.coords)))
	if(!is.null(donne$missing.coords))
		outlist <- c(outlist, list('Missing coordinates', as.matrix(donne$missing.coords)))
	if(!is.null(donne$duplicated.dates)){
		tmp0 <- donne$duplicated.dates$date
		tmp0 <- matrix(c(tmp0, rep("", 10 - (length(tmp0) %% 10))), ncol = 10, byrow = TRUE)
		outlist <- c(outlist, list('Duplicated dates', tmp0))
	}
	if(!is.null(donne$wrong.dates)){
		tmp0 <- donne$wrong.dates$date
		tmp0 <- matrix(c(tmp0, rep("", 10 - (length(tmp0) %% 10))), ncol = 10, byrow = TRUE)
		outlist <- c(outlist, list('Wrong dates format', tmp0))
	}
	if(!is.null(donne$missing.dates)){
		tmp0 <- donne$missing.dates$date
		tmp0 <- matrix(c(tmp0, rep("", 10 - (length(tmp0) %% 10))), ncol = 10, byrow = TRUE)
		outlist <- c(outlist, list('Missing dates', tmp0))
	}

	if(length(outlist) > 0){
		containertab <- Display_Output_Console_Tab(outlist, title = filename)
		ntab <- update.OpenTabs('ctxt', containertab)
		tkselect(.cdtEnv$tcl$main$tknotes, ntab)
	}

	return(donne)
}

getCDTTSdataAndDisplayMsg <- function(donne, period, filefrmt, datefrmt, filename){
	if(is.null(donne)) return(NULL)
	donne <- splitTsData(donne, period, filefrmt, datefrmt)
	if(is.null(donne)) return(NULL)

	outlist <- list()
	if(!is.null(donne$duplicated.dates)){
		tmp0 <- donne$duplicated.dates$date
		tmp0 <- matrix(c(tmp0, rep("", 10 - (length(tmp0) %% 10))), ncol = 10, byrow = TRUE)
		outlist <- c(outlist, list('Duplicated dates', tmp0))
	}
	if(!is.null(donne$wrong.dates)){
		tmp0 <- donne$wrong.dates$date
		tmp0 <- matrix(c(tmp0, rep("", 10 - (length(tmp0) %% 10))), ncol = 10, byrow = TRUE)
		outlist <- c(outlist, list('Wrong dates format', tmp0))
	}
	if(!is.null(donne$missing.dates)){
		tmp0 <- donne$missing.dates$date
		tmp0 <- matrix(c(tmp0, rep("", 10 - (length(tmp0) %% 10))), ncol = 10, byrow = TRUE)
		outlist <- c(outlist, list('Missing dates', tmp0))
	}

	if(length(outlist) > 0){
		containertab <- Display_Output_Console_Tab(outlist, title = filename)
		ntab <- update.OpenTabs('ctxt', containertab)
		tkselect(.cdtEnv$tcl$main$tknotes, ntab)
	}

	return(donne)
}

##############################################

## Get NetCDF sample data  in the list (all open files)
## old name:  getRFESampleData
getNCDFSampleData <- function(file.netcdf){
	jfile <- getIndex.AllOpenFiles(file.netcdf)
	rfelist <- NULL
	if(length(jfile) > 0){
		if(.cdtData$OpenFiles$Type[[jfile]] == "netcdf"){
			ncdata <- .cdtData$OpenFiles$Data[[jfile]][[2]]
			nclist <- list(lon = ncdata$x, lat = ncdata$y, varid = ncdata$varid,
						ilon = ncdata$ilon, ilat = ncdata$ilat, irevlat = ncdata$irevlat)
		}
	}
	return(nclist)
}

##############################################

## Get shp file in the list (all open files)
## return [[1]] name [[2]] shp [[3]] path
getShpOpenData <- function(shp){
	jfile <- getIndex.AllOpenFiles(shp)
	shpf <- NULL
	if(length(jfile) > 0){
		if(.cdtData$OpenFiles$Type[[jfile]] == "shp")
			shpf <- .cdtData$OpenFiles$Data[[jfile]]
	}
	return(shpf)
}

##############################################

## Test if the elements of two vectors are equals
isEquals <- function(x, y){
	ix <- (x == y) | (is.na(x) & is.na(y))
	ix[is.na(ix)] <- FALSE
	ix
}

## Test if two vectors are equals
isEqual <- function(x, y) !any(!isEquals(x, y))

##############################################

## List of available NetCDF files
ncFilesInfo <- function(Tstep, start.date, end.date, months,
						ncDir, ncFileFormat, error.msg)
{
	if(Tstep == 'daily'){
		dates <- format(seq(start.date, end.date, 'day'), '%Y%m%d')
		ncDataFiles <- file.path(ncDir, sprintf(ncFileFormat, substr(dates, 1, 4),
										substr(dates, 5, 6), substr(dates, 7, 8)))
	}
	if(Tstep == 'pentad'){
		dates <- seq(start.date,  end.date, 'day')
		dates <- paste0(format(dates[which(as.numeric(format(dates, '%d')) <= 6)], '%Y%m'),
					as.numeric(format(dates[which(as.numeric(format(dates, '%d')) <= 6)], '%d')))
		ncDataFiles <- file.path(ncDir, sprintf(ncFileFormat, substr(dates, 1, 4),
										substr(dates, 5, 6), substr(dates, 7, 7)))
	}
	if(Tstep == 'dekadal'){
		dates <- seq(start.date,  end.date, 'day')
		dates <- paste0(format(dates[which(as.numeric(format(dates, '%d')) <= 3)], '%Y%m'),
					as.numeric(format(dates[which(as.numeric(format(dates, '%d')) <= 3)], '%d')))
		ncDataFiles <- file.path(ncDir, sprintf(ncFileFormat, substr(dates, 1, 4),
										substr(dates, 5, 6), substr(dates, 7, 7)))
	}
	if(Tstep == 'monthly'){
		dates <- format(seq(start.date, end.date, 'month'), '%Y%m')
		ncDataFiles <- file.path(ncDir, sprintf(ncFileFormat, substr(dates, 1, 4),
												substr(dates, 5, 6)))
	}
	months.dates <- as(substr(dates, 5, 6), 'numeric')
	imo <- months.dates %in% months
	dates <- dates[imo]
	ncDataFiles <- ncDataFiles[imo]

	existFl <- unlist(lapply(ncDataFiles, file.exists))
	if(!any(existFl)){
		Insert.Messages.Out(error.msg, format = TRUE)
		return(NULL)
	}
	return(list(dates = dates, nc.files = ncDataFiles, exist = existFl))
}

##############################################

## Read ncdf files, extract at a set points
read.NetCDF.Data2Points <- function(read.ncdf.parms, list.lonlat.pts){
	Insert.Messages.Out(read.ncdf.parms$msg$start)

	ncInfo <- do.call(ncFilesInfo, c(read.ncdf.parms$ncfiles, error.msg = read.ncdf.parms$errmsg))
	if(is.null(ncInfo)) return(NULL)

	nc <- nc_open(ncInfo$nc.files[which(ncInfo$exist)[1]])
	lon <- nc$var[[read.ncdf.parms$ncinfo$varid]]$dim[[read.ncdf.parms$ncinfo$xo]]$vals
	lat <- nc$var[[read.ncdf.parms$ncinfo$varid]]$dim[[read.ncdf.parms$ncinfo$yo]]$vals
	nc_close(nc)

	xo <- order(lon)
	lon <- lon[xo]
	yo <- order(lat)
	lat <- lat[yo]
	nlon <- length(lon)
	nlat <- length(lat)
	ijx <- grid2pointINDEX(list.lonlat.pts, list(lon = lon, lat = lat))

	is.parallel <- doparallel(length(which(ncInfo$exist)) >= 100)
	`%parLoop%` <- is.parallel$dofun

	ncdata <- foreach(jj = seq_along(ncInfo$nc.files), .packages = 'ncdf4') %parLoop% {
		if(ncInfo$exist[jj]){
			nc <- try(nc_open(ncInfo$nc.files[jj]), silent = TRUE)
			if(inherits(nc, "try-error")) return(NULL)
			xvar <- ncvar_get(nc, varid = read.ncdf.parms$ncinfo$varid)
			nc_close(nc)
			if(nlon != nrow(xvar) | nlat != ncol(xvar)) return(NULL)
			xvar <- if(read.ncdf.parms$ncinfo$xo < read.ncdf.parms$ncinfo$yo) xvar[xo, yo] else t(xvar)[xo, yo]
			xvar <- xvar[ijx]
		}else xvar <- NULL
		xvar
	}
	if(is.parallel$stop) stopCluster(is.parallel$cluster)
	ret <- list(dates = ncInfo$dates, data = ncdata, lon = lon, lat = lat,
				lon.pts = list.lonlat.pts$lon, lat.pts = list.lonlat.pts$lat)
	Insert.Messages.Out(read.ncdf.parms$msg$end)
	return(ret)
}

##############################################

## Get index of points at grid
grid2pointINDEX <- function(pts_Coords, grd_Coords){
	newgrid <- expand.grid(lon = grd_Coords$lon, lat = grd_Coords$lat)
	coordinates(newgrid) <- ~lon+lat
	newgrid <- SpatialPixels(points = newgrid,
							tolerance = sqrt(sqrt(.Machine$double.eps)),
							proj4string = CRS(as.character(NA)))
	pts.loc <- data.frame(lon = pts_Coords$lon, lat = pts_Coords$lat)
	pts.loc <- SpatialPoints(pts.loc)
	ijGrd <- unname(over(pts.loc, geometry(newgrid)))
	return(ijGrd)
}

##############################################

## Define spatialPixels
defSpatialPixels <- function(grd_Coords){
	newgrid <- expand.grid(lon = grd_Coords$lon, lat = grd_Coords$lat)
	coordinates(newgrid) <- ~lon+lat
	newgrid <- SpatialPixels(points = newgrid,
							tolerance = sqrt(sqrt(.Machine$double.eps)),
							proj4string = CRS(as.character(NA)))
	return(newgrid)
}

##############################################

## Compare if 2 SpatialPixelsObjects have the same resolution
is.diffSpatialPixelsObj <- function(SP1, SP2, tol = 1e-07){
	SP1CelldX <- SP1@grid@cellsize[1]
	SP1CelldY <- SP1@grid@cellsize[2]
	SP1CellSX <- SP1@grid@cells.dim[1]
	SP1CellSY <- SP1@grid@cells.dim[2]
	SP2CelldX <- SP2@grid@cellsize[1]
	SP2CelldY <- SP2@grid@cellsize[2]
	SP2CellSX <- SP2@grid@cells.dim[1]
	SP2CellSY <- SP2@grid@cells.dim[2]
	unname(
			abs(SP1CelldX - SP2CelldX) > tol |
			(SP1CellSX != SP2CellSX) |
			abs(SP1CelldY - SP2CelldY) > tol |
			(SP1CellSY != SP2CellSY)
		)
}

##############################################

## same as fields::interp.surface.grid
cdt.interp.surface.grid <- function(obj, grid.list, edge = TRUE)
{
	nx0 <- length(grid.list$lon)
	ny0 <- length(grid.list$lat)
	loc <- do.call(expand.grid, grid.list)

	nx <- length(obj$lon)
	ny <- length(obj$lat)
	rule <- if(edge) 2 else 1
	lx <- approx(obj$lon, 1:nx, loc$lon, rule = rule)$y
	ly <- approx(obj$lat, 1:ny, loc$lat, rule = rule)$y
	lx1 <- floor(lx)
	ly1 <- floor(ly)
	ex <- lx - lx1
	ey <- ly - ly1
	ex[lx1 == nx] <- 1
	ey[ly1 == ny] <- 1
	lx1[lx1 == nx] <- nx - 1
	ly1[ly1 == ny] <- ny - 1

	z <- obj$z[cbind(lx1, ly1)] * (1 - ex) * (1 - ey) +
		 obj$z[cbind(lx1 + 1, ly1)] * ex * (1 - ey) +
		 obj$z[cbind(lx1, ly1 + 1)] * (1 - ex) * ey +
		 obj$z[cbind(lx1 + 1, ly1 + 1)] * ex * ey

	names(grid.list) <- c('x', 'y')
	out <- c(grid.list, list(z = matrix(z, nx0, ny0)))
	return(out)
}

##############################################

## nx and ny for as.image
# x: diff(range( lon or lat ))
nx_ny_as.image <- function(x) round(x / (0.0167323 * x^0.9602))

## same as fields::as.image
cdt.as.image <- function(pts.val, pts.xy, grid = NULL, nx = 64, ny = 64, weighted = FALSE)
{
	if(is.null(grid)){
		xlim <- range(pts.xy[, 1], na.rm = TRUE)
		ylim <- range(pts.xy[, 2], na.rm = TRUE)
		xlim <- xlim + diff(xlim) * c(-1, 1) * 0.01
		ylim <- ylim + diff(ylim) * c(-1, 1) * 0.01
		grid <- list(lon = seq(xlim[1], xlim[2], length.out = nx),
					 lat = seq(ylim[1], ylim[2], length.out = ny))
	}
	xy <- do.call(expand.grid, grid)
	ijGrd <- grid2pointINDEX(list(lon = pts.xy[, 1], lat = pts.xy[, 2]), grid)
	out <- list(x = grid$lon, y = grid$lat, z = matrix(NA, length(grid$lon), length(grid$lat)))

	ij <- !is.na(pts.val)
	pts.val <- pts.val[ij]
	if(length(pts.val) == 0) return(out)
	pts.xy <- pts.xy[ij, , drop = FALSE]
	ijGrd <- ijGrd[ij]
	idx <- split(seq_along(ijGrd), ijGrd)

	if(any(sapply(idx, length) > 1)){
		w <- rep(1, length(ijGrd))
		if(weighted){
			idup <- duplicated(ijGrd) | duplicated(ijGrd, fromLast = TRUE)
			stn.grd <- xy[ijGrd, ]
			dist <- 1 / ((stn.grd[idup, 1] - pts.xy[idup, 1])^2 + (stn.grd[idup, 2] - pts.xy[idup, 2])^2)
			dist[is.infinite(dist)] <- 2 * max(dist[!is.infinite(dist)])
			w[idup] <- dist
		}
		val <- sapply(idx, function(j) sum(w[j] * pts.val[j]) / sum(w[j]))
	}else val <- pts.val[unlist(idx)]
	ij <- as.numeric(names(idx))
	out$z[ij] <- val
	return(out)
}

##############################################

## Get boundaries from shapefile
getBoundaries <- function(shpf){
	ocrds <- matrix(NA, nrow = 1, ncol = 2)
	if(!is.null(shpf)){
		retPolygon <- lapply(slot(shpf, "polygons"), function(i) slot(i, "Polygons"))
		polys <- lapply(retPolygon, function(x){
			ret <- NULL
			for(i in seq_along(x)){
				poly <- rbind(slot(x[[i]], "coords"), cbind(NA, NA))
				ret <- rbind(ret, poly)
			}
			ret
		})
		ocrds <- do.call(rbind, polys)
	}
	return(ocrds)
}

