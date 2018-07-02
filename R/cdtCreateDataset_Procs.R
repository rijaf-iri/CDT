
cdtDataset_readData <- function(){
	stt <- Sys.time()

	datarepo <- file.path(.cdtData$GalParams$output$dir, .cdtData$GalParams$output$data.name)
	datadir <- file.path(datarepo, 'DATA')
	datafileIdx <- file.path(datarepo, paste0(.cdtData$GalParams$output$data.name, '.rds'))

	#######
	if(.cdtData$GalParams$Update){
		Insert.Messages.Out(.cdtData$GalParams[['message']][['6']])
		if(!dir.exists(datarepo)){
			Insert.Messages.Out(.cdtData$GalParams[['message']][['7']], format = TRUE)
			return(NULL)
		}
		if(!dir.exists(datadir)){
			Insert.Messages.Out(.cdtData$GalParams[['message']][['8']], format = TRUE)
			return(NULL)
		}
		if(!file.exists(datafileIdx)){
			Insert.Messages.Out(paste(paste0(.cdtData$GalParams$output$data.name, '.rds'),
								.cdtData$GalParams[['message']][['9']]), format = TRUE)
			return(NULL)
		}
		cdtTmpVar <- try(readRDS(datafileIdx), silent = TRUE)
		if(inherits(cdtTmpVar, "try-error")){
			Insert.Messages.Out(paste(.cdtData$GalParams[['message']][['10']], datafileIdx), format = TRUE)
			return(NULL)
		}
		chunksize <- cdtTmpVar$chunksize
	}else{
		Insert.Messages.Out(.cdtData$GalParams[['message']][['11']])
		if(!dir.exists(datarepo)) dir.create(datarepo, showWarnings = FALSE, recursive = TRUE)
		if(!dir.exists(datadir)) dir.create(datadir, showWarnings = FALSE, recursive = TRUE)
		cdtTmpVar <- NULL
		cdtTmpVar$TimeStep <- .cdtData$GalParams$Tstep
		chunksize <- .cdtData$GalParams$chunk$chunksize
		cdtTmpVar$chunkfac <- .cdtData$GalParams$chunk$chunkfac
	}

	##################
	ncDataInfo <- getNCDFSampleData(.cdtData$GalParams$NCDF$sample)
	if(is.null(ncDataInfo)){
		Insert.Messages.Out(.cdtData$GalParams[['message']][['12']], format = TRUE)
		return(NULL)
	}

	##################
	tstep <- .cdtData$GalParams$Tstep
	start.year <- .cdtData$GalParams$date.range$start.year
	start.mon <- .cdtData$GalParams$date.range$start.mon
	start.dek <- .cdtData$GalParams$date.range$start.dek
	end.year <- .cdtData$GalParams$date.range$end.year
	end.mon <- .cdtData$GalParams$date.range$end.mon
	end.dek <- .cdtData$GalParams$date.range$end.dek
	months <- .cdtData$GalParams$date.range$Months
	start.date <- as.Date(paste(start.year, start.mon, start.dek, sep = '/'), format = '%Y/%m/%d')
	end.date <- as.Date(paste(end.year, end.mon, end.dek, sep = '/'), format = '%Y/%m/%d')

	NCDF.DIR <- .cdtData$GalParams$NCDF$dir
	NCDF.Format <- .cdtData$GalParams$NCDF$format

	errmsg <- .cdtData$GalParams[['message']][['13']]
	ncInfo <- ncFilesInfo(tstep, start.date, end.date, months, NCDF.DIR, NCDF.Format, errmsg)
	if(is.null(ncInfo)) return(NULL)
	ncInfo$ncinfo <- list(xo = ncDataInfo$ilon, yo = ncDataInfo$ilat, varid = ncDataInfo$varid)

	ncInfo$dates <- ncInfo$dates[ncInfo$exist]
	ncInfo$nc.files <- ncInfo$nc.files[ncInfo$exist]
	ncInfo$exist <- ncInfo$exist[ncInfo$exist]

	###################
	if(.cdtData$GalParams$Update){
		readDate <- !ncInfo$dates %in% cdtTmpVar$dateInfo$date
		if(!any(readDate)){
			Insert.Messages.Out(.cdtData$GalParams[['message']][['14']])
			return(NULL)
		}
		ncInfo$dates <- ncInfo$dates[readDate]
		ncInfo$nc.files <- ncInfo$nc.files[readDate]
		ncInfo$exist <- ncInfo$exist[readDate]
	}

	##################
	nc <- nc_open(ncInfo$nc.files[1])
	nc.lon <- nc$var[[ncInfo$ncinfo$varid]]$dim[[ncInfo$ncinfo$xo]]$vals
	nc.lat <- nc$var[[ncInfo$ncinfo$varid]]$dim[[ncInfo$ncinfo$yo]]$vals
	varInfo <- nc$var[[ncInfo$ncinfo$varid]][c('name', 'prec', 'units', 'longname')]
	nc_close(nc)

	xo <- order(nc.lon)
	nc.lon <- nc.lon[xo]
	yo <- order(nc.lat)
	nc.lat <- nc.lat[yo]
	len.lon <- length(nc.lon)
	len.lat <- length(nc.lat)

	##################

	if(.cdtData$GalParams$Update){
		SP1 <- cdtTmpVar$coords$mat
		SP1 <- defSpatialPixels(list(lon = SP1$x, lat = SP1$y))
		SP2 <- defSpatialPixels(list(lon = nc.lon, lat = nc.lat))
		if(is.diffSpatialPixelsObj(SP1, SP2, tol = 1e-04)){
			Insert.Messages.Out(.cdtData$GalParams[['message']][['15']], format = TRUE)
			return(NULL)
		}
		rm(SP1, SP2)
	}

	###################

	## square chunk
	nxy.chunksize <- round(sqrt(chunksize))
	seqlon <- seq_along(nc.lon)
	seqlat <- seq_along(nc.lat)
	seqcol <- cbind(id = seq(len.lon * len.lat), expand.grid(x = seqlon, y = seqlat))

	split.lon <- split(seqlon, ceiling(seqlon / nxy.chunksize))
	split.lat <- split(seqlat, ceiling(seqlat / nxy.chunksize))
	xgrid <- expand.grid(x = seq_along(split.lon), y = seq_along(split.lat))

	xarrg <- lapply(seq(nrow(xgrid)), function(j){
		crd <- expand.grid(x = nc.lon[split.lon[[xgrid$x[j]]]], y = nc.lat[split.lat[[xgrid$y[j]]]])
		id <- seqcol$id[(seqcol$x %in% split.lon[[xgrid$x[j]]]) & (seqcol$y %in% split.lat[[xgrid$y[j]]])]
		list(coords = crd, id = id, grp = rep(j, length(id)))
	})

	col.idx <- lapply(xarrg, function(x) x$id)
	col.id <- do.call(c, col.idx)
	col.grp <- do.call(c, lapply(xarrg, function(x) x$grp))
	xy.exp <- do.call(rbind, lapply(xarrg, function(x) x$coords))
	col.order <- order(col.id)

	###################

	if(!.cdtData$GalParams$Update){
		cdtTmpVar$chunksize <- nxy.chunksize * nxy.chunksize
		cdtTmpVar$coords$mat <- list(x = nc.lon, y = nc.lat)
		cdtTmpVar$coords$df <- xy.exp
		attr(cdtTmpVar$coords$df, "out.attrs") <- NULL
		cdtTmpVar$colInfo <- list(id = col.id, index = col.grp, order = col.order)
		cdtTmpVar$varInfo <- varInfo
	}

	#########################################################

	chunkdate <- split(seq_along(ncInfo$dates), ceiling(seq_along(ncInfo$dates)/30))

	is.parallel <- doparallel(length(chunkdate) >= 10)
	`%parLoop%` <- is.parallel$dofun
	ret <- foreach(jj = seq_along(chunkdate), .packages = "ncdf4") %parLoop% {
		retdaty <- lapply(chunkdate[[jj]], function(j){
			nc <- try(nc_open(ncInfo$nc.files[j]), silent = TRUE)
			if(inherits(nc, "try-error")) return(NULL)
			vars <- ncvar_get(nc, varid = ncInfo$ncinfo$varid)
			nc_close(nc)
			vars <- if(ncInfo$ncinfo$xo < ncInfo$ncinfo$yo) vars[xo, yo] else t(vars)[xo, yo]
			vars <- round(c(vars), 4)

			file.tmp <- file.path(datadir, paste0(jj, ".gz"))
			con <- gzfile(file.tmp, open = "a", compression = 6)
			cat(c(vars, "\n"), file = con)
			close(con)
			return(ncInfo$dates[j])
		})

		retdaty <- do.call(c, retdaty)
		saveRDS(retdaty, file = file.path(datadir, paste0(jj, "_d.rds")))
		return(0)
	}
	if(is.parallel$stop) stopCluster(is.parallel$cluster)

	###################

	ncDaty <- lapply(seq_along(chunkdate), function(jj){
		file.tmp <- file.path(datadir, paste0(jj, "_d.rds"))
		dd <- readRDS(file.tmp)
		unlink(file.tmp)
		return(dd)
	})
	ncDaty <- do.call(c, ncDaty)

	###################

	toExports <- c("col.idx", "datadir")
	is.parallel <- doparallel(length(col.idx) >= 50)
	`%parLoop%` <- is.parallel$dofun

	ret <- lapply(seq_along(chunkdate), function(jj){
		file.gz <- file.path(datadir, paste0(jj, ".gz"))
		gunzip(file.gz)
		file.tmp <- file_path_sans_ext(file.gz)
		tmp <- fread(file.tmp, header = FALSE, sep = " ", stringsAsFactors = FALSE, colClasses = "numeric")
		unlink(file.tmp)
		tmp <- as.matrix(tmp)
		dimnames(tmp) <- NULL

		ret <- foreach(j = seq_along(col.idx), .export = toExports) %parLoop% {
			chk <- tmp[, col.idx[[j]], drop = FALSE]
			file.rds <- file.path(datadir, paste0(j, ".rds"))
			if(file.exists(file.rds)){
				y <- readRDS(file.rds)
				chk <- rbind(y, chk)
			}

			con <- gzfile(file.rds, compression = 7)
			open(con, "wb")
			saveRDS(chk, con)
			close(con)

			return(0)
		}

		stt0 <- Sys.time()-stt
		vdaty <- ncInfo$dates[chunkdate[[jj]]]
		Insert.Messages.Out(paste("Date:", vdaty[1], "-", vdaty[length(vdaty)],
							"| Elapsed time:", as.character(stt0), attr(stt0, "units")))

		rm(tmp); gc()
		return(0)
	})

	if(is.parallel$stop) stopCluster(is.parallel$cluster)

	#########################################################

	idx <- seq(length(ncDaty))
	if(.cdtData$GalParams$Update){
		Adates <- c(cdtTmpVar$dateInfo$date, ncDaty)
		Aindex <- c(cdtTmpVar$dateInfo$index, max(cdtTmpVar$dateInfo$index) + idx)
	}else{
		Adates <- ncDaty
		Aindex <- idx
	}
	odaty <- order(Adates)
	cdtTmpVar$dateInfo <- list(date = Adates[odaty], index = Aindex[odaty])

	con <- gzfile(datafileIdx, compression = 6)
	open(con, "wb")
	saveRDS(cdtTmpVar, con)
	close(con)

	stt <- Sys.time()-stt
	Insert.Messages.Out(paste("Elapsed time:", as.character(stt), attr(stt, "units")))

	rm(ncDaty, idx, odaty, Adates, Aindex, cdtTmpVar, ncDataInfo, ncInfo)
	gc()
	return(0)
}

###########################################################

##### read chunk files (sequential)
# chunk files
# fileInfo <- "~/PRECIP/PRECIP.rds"
# cdtData <- readRDS(fileInfo) OR separate cdtdataset info files
# don <- readCdtDatasetChunk.sequence(loc, fileInfo, cdtData, do.par = TRUE)
# don <- readCdtDatasetChunk.sequence(loc, fileInfo, do.par = TRUE)
# return matrix,  row: all dates, col: sum of chunk col number

readCdtDatasetChunk.sequence <- function(chunk, fileInfo, cdtData = NULL, do.par = TRUE)
{
	if(is.null(cdtData)) cdtData <- readRDS(fileInfo)
	datadir <- file.path(dirname(fileInfo), "DATA")

	is.parallel <- doparallel(do.par & (length(chunk) >= 20))
	`%parLoop%` <- is.parallel$dofun
	don <- foreach(j = chunk) %parLoop% {
		file.rds <- file.path(datadir, paste0(j, ".rds"))
		x <- readRDS(file.rds)
		x
	}
	if(is.parallel$stop) stopCluster(is.parallel$cluster)
	do.call(cbind, don)
}

####################

writeCdtDatasetChunk.sequence <- function(mat, chunk, cdtData, datadir, do.par = TRUE)
{
	col.grp <- cdtData$colInfo$index[cdtData$colInfo$index %in% chunk]
	col.grp <- split(seq(ncol(mat)), col.grp)

	is.parallel <- doparallel(do.par & (length(chunk) >= 20))
	`%parLoop%` <- is.parallel$dofun
	don <- foreach(j = seq_along(chunk)) %parLoop% {
		tmp <- mat[, col.grp[[j]], drop = FALSE]
		file.rds <- file.path(datadir, paste0(chunk[j], ".rds"))
		con <- gzfile(file.rds, compression = 5)
		open(con, "wb")
		saveRDS(tmp, con)
		close(con)
		rm(tmp); gc()
	}
	if(is.parallel$stop) stopCluster(is.parallel$cluster)
	return(0)
}

####################
##### read several dates, fileInfo and datadir are located in a separated directories
# dates: day, pentad, dekad, month 
# dates <- c('2006021', ...., '2006061')
# fileInfo <- "~/PRECIP/PRECIP.rds"
# datadir <- "~/ClimatoAnalysis_monthly_PRECIP_dek/Aggregated_PRECIP_dek/DATA"
# don <- readCdtDatasetChunk.sepdir.dates.order(fileInfo, dates, do.par = TRUE)
# return matrix,  row: date, col: expand x y coords reorder

readCdtDatasetChunk.sepdir.dates.order <- function(fileInfo, datadir, dates, do.par = TRUE, coords = FALSE, onedate = FALSE)
{
	cdtdata <- readRDS(fileInfo)
	chunk <- seq(max(cdtdata$colInfo$index))
	idaty <- cdtdata$dateInfo$index[match(dates, cdtdata$dateInfo$date)]
	# dates <- dates[!is.na(idaty)]
	idaty <- idaty[!is.na(idaty)]
	if(length(idaty) == 0) return(NULL)
	if(onedate) idaty <- idaty[1]

	if(do.par){
		is.parallel <- doparallel(length(chunk) >= 50)
		`%parLoop%` <- is.parallel$dofun
		don <- foreach(j = chunk) %parLoop% {
			file.rds <- file.path(datadir, paste0(j, ".rds"))
			x <- readRDS(file.rds)
			x[idaty, , drop = FALSE]
		}
		if(is.parallel$stop) stopCluster(is.parallel$cluster)
	}else{
		don <- lapply(chunk, function(j){
			file.rds <- file.path(datadir, paste0(j, ".rds"))
			x <- readRDS(file.rds)
			x[idaty, , drop = FALSE]
		})
	}

	don <- do.call(cbind, don)
	don <- don[, cdtdata$colInfo$order, drop = FALSE]
	if(onedate){
		dim(don) <- sapply(cdtdata$coords$mat, length)
		return(c(cdtdata$coords$mat, list(z = don)))
	}
	if(coords) return(c(cdtdata$coords$mat, list(z = don)))
	return(don)
}

####################
##### read several dates
# dates: day, pentad, dekad, month 
# dates <- c('2006021', ...., '2006061')
# fileInfo <- "~/PRECIP/PRECIP.rds"
# don <- readCdtDatasetChunk.multi.dates.order(fileInfo, dates)
# return matrix,  row: date (same dates length), col: expand x y coords
# coords = TRUE; list(x = xcoord, y = ycoord, z = matrix{row: date (same dates length), col: expand x y coords})
# onedate = TRUE; list(x = xcoord, y = ycoord, z = matrix), used by image

readCdtDatasetChunk.multi.dates.order <- function(fileInfo, dates, do.par = TRUE, coords = FALSE, onedate = FALSE)
{
	datadir <- file.path(dirname(fileInfo), "DATA")
	cdtdata <- readRDS(fileInfo)
	chunk <- seq(max(cdtdata$colInfo$index))
	idaty <- cdtdata$dateInfo$index[match(dates, cdtdata$dateInfo$date)]
	# dates <- dates[!is.na(idaty)]
	idaty <- idaty[!is.na(idaty)]
	if(onedate) idaty <- idaty[1]

	if(do.par){
		is.parallel <- doparallel(length(chunk) >= 50)
		`%parLoop%` <- is.parallel$dofun
		don <- foreach(j = chunk) %parLoop% {
			file.rds <- file.path(datadir, paste0(j, ".rds"))
			x <- readRDS(file.rds)
			x[idaty, , drop = FALSE]
		}
		if(is.parallel$stop) stopCluster(is.parallel$cluster)
	}else{
		don <- lapply(chunk, function(j){
			file.rds <- file.path(datadir, paste0(j, ".rds"))
			x <- readRDS(file.rds)
			x[idaty, , drop = FALSE]
		})
	}

	don <- do.call(cbind, don)
	don <- don[, cdtdata$colInfo$order, drop = FALSE]
	if(onedate){
		dim(don) <- sapply(cdtdata$coords$mat, length)
		return(c(cdtdata$coords$mat, list(z = don)))
	}
	if(coords) return(c(cdtdata$coords$mat, list(z = don)))
	return(don)
}

########################
##### read pixels
# loc: index of column from expand x y coords, (from sp::over OR cdt.which | findInterval)
# fileInfo <- "~/PRECIP/PRECIP.rds"
# cdtData <- readRDS(fileInfo) OR separate cdtdataset info files
# don <- readCdtDatasetChunk.locations(loc, fileInfo, cdtData, do.par = TRUE)
# don <- readCdtDatasetChunk.locations(loc, fileInfo, do.par = TRUE)
# return matrix,  row: all dates, col: correspond to loc (same length as loc)


readCdtDatasetChunk.locations <- function(loc, fileInfo, cdtData = NULL, chunkDir = "DATA", do.par = TRUE)
{
	if(is.null(cdtData)) cdtData <- readRDS(fileInfo)
	datadir <- file.path(dirname(fileInfo), chunkDir)

	id <- match(loc, cdtData$colInfo$id)
	col.id <- split(cdtData$colInfo$id[id], cdtData$colInfo$index[id])
	chunk <- as.numeric(names(col.id))
	grp <- cdtData$colInfo$index%in%chunk
	col.grp <- split(cdtData$colInfo$id[grp], cdtData$colInfo$index[grp])
	# idx <- lapply(seq_along(chunk), function(j) which(col.grp[[j]]%in%col.id[[j]]))
	idx <- lapply(seq_along(chunk), function(j) match(col.id[[j]], col.grp[[j]]))
	xcrd <- do.call(c, lapply(seq_along(chunk), function(j) col.grp[[j]][idx[[j]]]))
	coords <- cdtData$coords$df[match(xcrd, cdtData$colInfo$id), , drop = FALSE]
	rownames(coords) <- NULL

	if(do.par){
		is.parallel <- doparallel(length(chunk) >= 50)
		`%parLoop%` <- is.parallel$dofun
		don <- foreach(j = seq_along(chunk)) %parLoop% {
			file.rds <- file.path(datadir, paste0(chunk[j], ".rds"))
			x <- readRDS(file.rds)
			x[, idx[[j]], drop = FALSE]
		}
		if(is.parallel$stop) stopCluster(is.parallel$cluster)
	}else{
		don <- lapply(seq_along(chunk), function(j){
			file.rds <- file.path(datadir, paste0(chunk[j], ".rds"))
			x <- readRDS(file.rds)
			x[, idx[[j]], drop = FALSE]
		})
	}
	don <- do.call(cbind, don)
	list(coords = coords, data = don)
}

###########################################################

# readCdtDatasetChunk for PICSA (a changer)

readCdtDatasetChunk.picsa <- function(col, colInfo, indir, chunksize = 100, chunk.par = TRUE)
{
	col.id <- match(col, colInfo$id)
	col.idx <- colInfo$index[col.id]
	col.grp <- split(col.id, col.idx)
	col.grp <- lapply(col.grp, function(l){
		ix <- (l-chunksize)%%chunksize
		ifelse(ix == 0, chunksize, ix)
	})
	chunk <- unique(col.idx)

	is.parallel <- doparallel(chunk.par & (length(chunk) >= 10))
	`%parLoop%` <- is.parallel$dofun
	don <- foreach(j = seq_along(chunk)) %parLoop% {
		file.rds <- file.path(indir, paste0(chunk[j], ".rds"))
		# con <- file(file.rds)
		# open(con, "rb")
		# x <- readRDS(con)
		# close(con)
		x <- readRDS(file.rds)
		x[, col.grp[[j]], drop = FALSE]
	}
	if(is.parallel$stop) stopCluster(is.parallel$cluster)
	do.call(cbind, don)
}

## in picsa

writeCdtDatasetChunk.create <- function(x, outdir, chunksize = 100, chunk.par = TRUE)
{
	col.id <- seq(ncol(x))
	col.grp <- split(col.id, ceiling(col.id/chunksize))
	col.idx <- rep(seq_along(col.grp), sapply(col.grp, length))

	is.parallel <- doparallel(chunk.par & (length(col.grp) >= 10))
	`%parLoop%` <- is.parallel$dofun
	ret <- foreach(j = seq_along(col.grp)) %parLoop% {
		tmp <- x[, col.grp[[j]], drop = FALSE]
		file.rds <- file.path(outdir, paste0(j, ".rds"))
		con <- gzfile(file.rds, compression = 5)
		open(con, "wb")
		saveRDS(tmp, con)
		close(con)
		rm(tmp); gc()
	}
	if(is.parallel$stop) stopCluster(is.parallel$cluster)
	return(list(id = col.id, index = col.idx))
}


writeCdtDatasetChunk.rbind <- function(x, outdir, chunksize = 100, chunk.par = TRUE)
{
	col.id <- seq(ncol(x))
	col.grp <- split(col.id, ceiling(col.id/chunksize))
	col.idx <- rep(seq_along(col.grp), sapply(col.grp, length))

	is.parallel <- doparallel(chunk.par & (length(col.grp) >= 10))
	`%parLoop%` <- is.parallel$dofun
	ret <- foreach(j = seq_along(col.grp)) %parLoop% {
		file.rds <- file.path(outdir, paste0(j, ".rds"))
		y <- readRDS(file.rds)
		z <- x[, col.grp[[j]], drop = FALSE]
		tmp <- rbind(y, z)
		con <- gzfile(file.rds, compression = 5)
		open(con, "wb")
		saveRDS(tmp, con)
		close(con)
		rm(y, z, tmp); gc()
	}
	if(is.parallel$stop) stopCluster(is.parallel$cluster)
	return(list(id = col.id, index = col.idx))
}
