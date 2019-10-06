
AssessDataAvailProcs <- function(GeneralParameters){
	if(!dir.exists(GeneralParameters$outdir)){
		msg <- paste(GeneralParameters$outdir, .cdtData$EnvData$message[[6]])
		Insert.Messages.Out(msg, TRUE, "e")
		return(NULL)
	}

	don <- getStnOpenData(GeneralParameters$infile)
	if(is.null(don)) return(NULL)
	don <- getCDTdataAndDisplayMsg(don, GeneralParameters$intstep, GeneralParameters$infile)
	if(is.null(don)) return(NULL)

	###################
	outdir <- file.path(GeneralParameters$outdir, "ASSESS.DATA.AVAILABILITY_data")
	dataCDTdir <- file.path(outdir, 'CDTDATASET')
	dir.create(dataCDTdir, showWarnings = FALSE, recursive = TRUE)
	dataSTNdir <- file.path(outdir, 'CDTSTATIONS')
	dir.create(dataSTNdir, showWarnings = FALSE, recursive = TRUE)

	###################
	output <- list(params = GeneralParameters, data = don[c('lon', 'lat', 'id')])

	xhead <- rbind(don$id, don$lon, don$lat)
	chead <- c('ID.STN', 'LON', 'DATE/LAT')
	infohead <- cbind(chead, xhead)

	###################
	nbstn <- ncol(don$data)
	ndate <- nrow(don$data)

	avai.prec <- 100 * colSums(!is.na(don$data)) / ndate
	output$Avai <- avai.prec

	xdata <- paste0(substr(don$dates[c(1, ndate)], 1, 4), collapse = "-")
	xdata <- rbind(infohead, cbind(xdata, matrix(round(avai.prec, 4), nrow = 1)))

	writeFiles(xdata, file.path(dataSTNdir, "DataAvailability_percentage.csv"))

	###################

	index.mon <- split(seq_along(don$dates), substr(don$dates, 1, 6))
	don.month <- lapply(index.mon, function(ix) colSums(!is.na(don$data[ix, , drop = FALSE])))
	don.month <- do.call(rbind, don.month)
	months <- names(index.mon)
	yr.mon <- substr(months, 1, 4) 
	mo.mon <- substr(months, 5, 6) 

	xdata <- rbind(infohead, cbind(months, don.month))
	writeFiles(xdata, file.path(dataSTNdir, "Monthly_non-missing.csv"))

	xdata <- list(year = substr(months, 1, 4), mon = substr(months, 5, 6), data = don.month)
	saveRDS(xdata, file.path(dataCDTdir, "Monthly_non-missing.rds"))

	.cdtData$EnvData$monthly <- xdata

	###################

	index.year <- split(seq_along(don$dates), substr(don$dates, 1, 4))
	year <- as.numeric(names(index.year))
	don.year <- lapply(index.year, function(ix) colSums(!is.na(don$data[ix, , drop = FALSE])))
	don.year <- do.call(rbind, don.year)

	xdata <- rbind(infohead, cbind(year, don.year))
	writeFiles(xdata, file.path(dataSTNdir, "Yearly_non-missing.csv"))

	xdata <- list(year = year, data = don.year)
	saveRDS(xdata, file.path(dataCDTdir, "Yearly_non-missing.rds"))

	.cdtData$EnvData$yearly <- xdata

	###################
	nonna <- rowSums(!is.na(don$data))

	nb.stn.year <- lapply(index.year, function(ix)
					do.call(function(x) c(mean(x), min(x), max(x)), list(nonna[ix])))
	nb.stn.year <- do.call(rbind, nb.stn.year)

	xdata <- data.frame(cbind(year, round(nb.stn.year, 1)))
	names(xdata) <- c("Year", "Average", "Minimum", "Maximum")
	writeFiles(xdata, file.path(dataSTNdir, "Annual_Average_number-stations.csv"), col.names = TRUE)

	output$year <- year
	output$nb.stn.year <- nb.stn.year

	###################

	jstn <- lapply(seq(nbstn), function(j){
		x <- which(stats::complete.cases(don$data[, j]))
		if(length(x)) c(min(x), max(x)) else integer(0)
	})

	sworking <- lapply(jstn, function(ix){
		ret <- rep(0, ndate)
		if(length(ix)) ret[ix[1]:ix[2]] <- 1
		ret
	})
	sworking <- rowSums(do.call(cbind, sworking))

	########
	if(GeneralParameters$intstep == "daily")
		daty <- as.Date(don$dates, "%Y%m%d")

	if(GeneralParameters$intstep == "pentad"){
		seqtime <- as.Date(don$dates, "%Y%m%d")
		pen <- c(1, 6, 11, 16, 21, 26)[as.numeric(format(seqtime, "%d"))]
		daty <- as.Date(paste0(format(seqtime, "%Y-%m-"), pen))
	}
	if(GeneralParameters$intstep == "dekadal"){
		seqtime <- as.Date(don$dates, "%Y%m%d")
		dek <- c(1, 11, 21)[as.numeric(format(seqtime, "%d"))]
		daty <- as.Date(paste0(format(seqtime, "%Y-%m-"), dek))
	}
	if(GeneralParameters$intstep == "monthly")
		daty <- as.Date(paste0(don$dates, "01"), "%Y%m%d")

	stn.working <- list(date = daty, avai = nonna, work = sworking)

	saveRDS(stn.working, file.path(dataCDTdir, "Station_Activities.rds"))

	###################

	id0 <- apply(don$data, 2, elEqual)
	if(any(id0)){
		val <- apply(don$data[, id0, drop = FALSE], 2,
					function(x){
						x <- x[!is.na(x)]
						if(length(x)) x[1] else NA
					})
		infoIDen <- cbind(don$lon[id0], don$lat[id0], val)
		lon <- don$lon[!id0]
		lat <- don$lat[!id0]
		donne <- don$data[, !id0, drop = FALSE]
	}else{
		lon <- don$lon
		lat <- don$lat
		donne <- don$data
		infoIDen <- NULL
	}

	xdist <- Distance.Correlation(donne, cbind(lon, lat))
	xsummary <- summary.Distance.Correlation(xdist[, 1], xdist[, 2])
	loess <- stats::loess.smooth(xdist[, 1], xdist[, 2])

	dist.cor <- list(dst.cor = xdist, summary = xsummary, loess = loess)
	saveRDS(dist.cor, file.path(dataCDTdir, "Distance_Correlation.rds"))

	###################

	file.index <- file.path(outdir, 'AssessDataAvailability.rds')
	saveRDS(output, file.index)

	.cdtData$EnvData$output <- output
	.cdtData$EnvData$PathData <- outdir

	return(0)
}

##############################################################################

Distance.Correlation <- function(data.mat, lonlat){
	corm <- suppressWarnings(cor(data.mat, use = 'pairwise.complete.obs'))
	corm[lower.tri(corm, diag = TRUE)] <- NA

	dist <- fields::rdist.earth(lonlat, miles = FALSE)
	dist[lower.tri(dist, diag = TRUE)] <- NA

	idn <- which(!is.na(corm))
	xcor <- corm[idn]
	xdst <- dist[idn]
	return(cbind(xdst, xcor))
}

##############################################################################

summary.Distance.Correlation <- function(x, y){
	o <- order(x)
	x <- x[o]
	y <- y[o]

	win <- 100
	min.data <- 10
	probs <- c(0.05, 0.5, 0.95)

	nl <- length(x)
	nseq <- nl - min.data + 1
	xquant <- rep(list(rep(NA, 3)), nseq)
	for(k in seq(nseq)){
		ys <- y[seq(k, k + win - 1, 1)]
		ys <- ys[!is.na(ys)]
		if(length(ys) < min.data) next
		xquant[[k]] <- quantile(ys,  probs = probs)
	}
	xquant <- do.call(rbind, xquant)
	return(unname(cbind(x[seq(nseq)], xquant)))
}
