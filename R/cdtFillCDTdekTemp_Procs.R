
fill_DekTemp_MissVal <- function(){
	tstep <- .cdtData$GalParams$tstep
	nlen <- .cdtData$GalParams$Fill.Params$dek.windows
	min.len <- .cdtData$GalParams$Fill.Params$min.length

	#######
	stnData <- getStnOpenData(.cdtData$GalParams$STN.file)
	if(is.null(stnData)) return(NULL)
	infoCap <- stnData[1:4, 1]
	stnData <- getCDTdataAndDisplayMsg(stnData, tstep, .cdtData$GalParams$STN.file)
	if(is.null(stnData)) return(NULL)
	missval <- getStnOpenDataInfo(.cdtData$GalParams$STN.file)[[3]]$miss.val

	date.stn <- stnData$dates
	data.stn0 <- data.stn <- stnData$data

	#######
	ncDataInfo <- getNCDFSampleData(.cdtData$GalParams$NCDF$sample)
	if(is.null(ncDataInfo)){
		Insert.Messages.Out(.cdtData$GalParams[['message']][['9a']], format = TRUE)
		return(NULL)
	}

	################
	start.year <- .cdtData$GalParams$Fill.Date.Range$start.year
	start.mon <- .cdtData$GalParams$Fill.Date.Range$start.mon
	start.dek <- .cdtData$GalParams$Fill.Date.Range$start.dek
	end.year <- .cdtData$GalParams$Fill.Date.Range$end.year
	end.mon <- .cdtData$GalParams$Fill.Date.Range$end.mon
	end.dek <- .cdtData$GalParams$Fill.Date.Range$end.dek
	months <- .cdtData$GalParams$Fill.Months

	if(!(start.dek %in% 1:3) | !(end.dek %in% 1:3))
	{
		Insert.Messages.Out(.cdtData$GalParams[['message']][['4']], format = TRUE)
		return(NULL)
	}

	tempDir <- .cdtData$GalParams$NCDF$dir
	tempfilefrmt <- .cdtData$GalParams$NCDF$format

	start.date <- as.Date(paste(start.year, start.mon, start.dek, sep = '/'), format = '%Y/%m/%d')
	end.date <- as.Date(paste(end.year, end.mon, end.dek, sep = '/'), format = '%Y/%m/%d')

	msg <- list(start = .cdtData$GalParams[['message']][['7']], end = .cdtData$GalParams[['message']][['8']])
	errmsg <- .cdtData$GalParams[['message']][['9']]
	ncfiles <- list(Tstep = tstep, start.date = start.date, end.date = end.date,
					months = months, ncDir = tempDir, ncFileFormat = tempfilefrmt)
	ncinfo <- list(xo = ncDataInfo$ilon, yo = ncDataInfo$ilat, varid = ncDataInfo$varid)
	read.ncdf.parms <- list(ncfiles = ncfiles, ncinfo = ncinfo, msg = msg, errmsg = errmsg)

	TempData <- read.NetCDF.Data2Points(read.ncdf.parms, stnData[c('lon', 'lat')])
	if(is.null(TempData)) return(NULL)
	inull <- sapply(TempData$data, is.null)
	if(all(inull)){
		Insert.Messages.Out(.cdtData$GalParams[['message']][['10']], format = TRUE)
		return(NULL)
	}

	TempData$data[inull] <- rep(NA, length(stnData$lon))
	data.tmp <- do.call(rbind, TempData$data)

	###############
	Insert.Messages.Out(.cdtData$GalParams[['message']][['11']])

	data.tmp <- data.tmp[TempData$dates %in% date.stn, , drop = FALSE]
	data.stn <- data.stn[date.stn %in% TempData$dates, , drop = FALSE]
	daty <- TempData$dates[TempData$dates %in% date.stn]
	if(nrow(data.tmp) == 0 | nrow(data.stn) == 0){
		Insert.Messages.Out(.cdtData$GalParams[['message']][['12']], format = TRUE)
		return(NULL)
	}

	na.data.stn <- is.na(data.stn)
	nbna <- colSums(na.data.stn)
	to.exclude <- nbna == nrow(data.stn) | nbna == 0
	if(all(to.exclude)){
		Insert.Messages.Out(.cdtData$GalParams[['message']][['14']], format = TRUE)
		return(NULL)
	}

	na.data.stn <- na.data.stn[, !to.exclude, drop = FALSE]
	data.stn.tmp <- data.stn[, !to.exclude, drop = FALSE]
	data.adj.tmp <- data.tmp[, !to.exclude, drop = FALSE]
	modk <- substr(daty, 5, 7)

	tofill <- lapply(seq(ncol(data.stn.tmp)), function(j){
		x <- data.stn.tmp[, j]
		ina <- na.data.stn[, j]
		lapply(which(ina), function(i){
			ix <- which(modk == substr(daty[i], 5, 7))
			ix1 <- ix[ix <= i][which(!is.na(x[ix][ix <= i]))]
			ix2 <- ix[ix > i][which(!is.na(x[ix][ix > i]))]
			ix1 <- rev(rev(ix1)[1:nlen])
			ix2 <- ix2[1:nlen]
			ix <- c(ix1, ix2)
			ix <- ix[!is.na(ix)]
			if(length(ix) >= min.len) list(id = c(i, j), irow = ix) else NULL
		})
	})
	tofill <- do.call(c, tofill)
	tofill <- tofill[!sapply(tofill, is.null)]
	# idx <- t(sapply(tofill, '[[', 1))
	idx <- do.call(rbind, lapply(tofill, '[[', 'id'))
	if(nrow(idx) == 0){
		Insert.Messages.Out(.cdtData$GalParams[['message']][['13']], format = TRUE)
		return(NULL)
	}

	is.parallel <- doparallel(nrow(idx) >= 500)
	`%parLoop%` <- is.parallel$dofun
	data.estim <- foreach(j = seq(nrow(idx))) %parLoop% {
		Obj <- tofill[[j]]
		y <- data.stn.tmp[Obj$irow, Obj$id[2]]
		x <- data.adj.tmp[Obj$irow, Obj$id[2]]
		m.glm <- glm(y~x)
		round(m.glm$coef[2] * data.adj.tmp[Obj$id[1], Obj$id[2]] + m.glm$coef[1], 1)
	}
	if(is.parallel$stop) stopCluster(is.parallel$cluster)

	data.stn.tmp[idx] <- unlist(data.estim)
	data.stn[, !to.exclude] <- data.stn.tmp
	data.stn0[date.stn %in% daty, ] <- data.stn
	data.stn0[is.na(data.stn0)] <- missval
	data.stn0 <- cbind(date.stn, data.stn0)

	headInfo <- do.call(rbind, stnData[c('id', 'lon', 'lat', 'elv')])
	headInfo <- cbind(infoCap[1:nrow(headInfo)], headInfo)
	stnData <- rbind(headInfo, data.stn0)
	writeFiles(stnData, .cdtData$GalParams$out.file)

	rm(stnData, headInfo, data.stn0, data.stn, tofill, data.tmp,
		data.stn.tmp, data.adj.tmp, TempData)
	return(0)
}
