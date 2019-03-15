
spatialAnalysisProcs <- function(GeneralParameters){
	if(GeneralParameters$in.file %in% c("", "NA")){
		Insert.Messages.Out('No input data found', format = TRUE)
		return(NULL)
	}

	if(!dir.exists(GeneralParameters$out.dir)){
		Insert.Messages.Out('Directory to save results not found', format = TRUE)
		Insert.Messages.Out(paste('The outputs will be put in', getwd()))
		GeneralParameters$out.dir <- getwd()
	}

	#############
	outputDIR <- file.path(GeneralParameters$out.dir, paste0("SPATIAL.ANALYSIS_",
							toupper(GeneralParameters$time.series$out.series), "_",
							file.sans.ext(basename(GeneralParameters$in.file))))
	dir.create(outputDIR, showWarnings = FALSE, recursive = TRUE)

	#############

	if(GeneralParameters$data.type == 'cdtstation'){
		cdtdata <- getStnOpenData(GeneralParameters$in.file)
		if(is.null(cdtdata)) return(NULL)
		cdtdata <- getCDTdataAndDisplayMsg(cdtdata, GeneralParameters$in.tstep, GeneralParameters$in.file)
		if(is.null(cdtdata)) return(NULL)
		cdtdata <- cdtdata[c('id', 'lon', 'lat', 'dates', 'data')]

		daty <- cdtdata$dates
		idrow <- seq_along(cdtdata$dates)
	}

	if(GeneralParameters$data.type == 'cdtdataset'){
		cdtdata <- try(readRDS(GeneralParameters$in.file), silent = TRUE)
		if(inherits(cdtdata, "try-error")){
			Insert.Messages.Out(paste("Unable to read", GeneralParameters$in.file), format = TRUE)
			return(NULL)
		}
		if(GeneralParameters$in.tstep != cdtdata$TimeStep){
			Insert.Messages.Out(paste("The dataset is not a", GeneralParameters$in.tstep, "data"), format = TRUE)
			return(NULL)
		}

		daty <- cdtdata$dateInfo$date
		idrow <- seq_along(cdtdata$dateInfo$date)
	}

	####################################################

	if(!GeneralParameters$time.series$all.years){
		if(GeneralParameters$time.series$nseq.years)
			yeartoAna <- GeneralParameters$time.series$custom.years
		else
			yeartoAna <- GeneralParameters$time.series$start.year:GeneralParameters$time.series$end.year
	}else yeartoAna <- as.numeric(substr(daty, 1, 4))

	startMonth <- GeneralParameters$time.series$start.month
	endMonth <- GeneralParameters$time.series$end.month
	seasonLength <- (endMonth - startMonth + 1) %% 12
	seasonLength[seasonLength == 0] <- 12
	monthtoAna <- (startMonth:(startMonth + (seasonLength - 1))) %% 12
	monthtoAna[monthtoAna == 0] <- 12

	if(GeneralParameters$time.series$out.series == "monthly"){
		if(GeneralParameters$time.series$nseq.months)
			monthtoAna <- GeneralParameters$time.series$custom.months
		seasonLength <- 1
	}

	itmp <- (as.numeric(substr(daty, 1, 4)) %in% yeartoAna) & (as.numeric(substr(daty, 5, 6)) %in% monthtoAna)
	if(length(daty[itmp]) == 0) return(NULL)

	agg.index <- cdt.index.aggregate(daty[itmp], GeneralParameters$in.tstep,
									GeneralParameters$time.series$out.series,
									seasonLength, startMonth)
	if(is.null(agg.index)) return(NULL)

	idrow <- idrow[itmp]
	indx <- lapply(agg.index$index, function(j) idrow[j])
	odaty <- agg.index$date
	len.data <- agg.index$nba
	len.data0 <- agg.index$nb0

	if((GeneralParameters$time.series$out.series == "monthly") |
	   (GeneralParameters$time.series$out.series == "seasonal" &
	   	seasonLength == 1))
	{
		yyyymm <- paste0(substr(odaty, 1, 4), '-', substr(odaty, 5, 6))
		odaty <- paste0(yyyymm, '_', yyyymm)
	}

	################################################

	toAggr <- list(indx, GeneralParameters$aggr.series)
	if(is.null(.cdtData$EnvTmp$toAggr)){
		aggregatData <- TRUE
	}else{
		aggregatData <- if(!isTRUE(all.equal(.cdtData$EnvTmp$toAggr, toAggr))) TRUE else FALSE
	}

	if(aggregatData){
		Insert.Messages.Out("Aggregate data......")

		if(GeneralParameters$data.type == 'cdtstation')
			AggrData <- cdt.data.aggregate(cdtdata$data, indx, pars = GeneralParameters$aggr.series)

		if(GeneralParameters$data.type == 'cdtdataset'){
			outDatasetDIR <- file.path(outputDIR, paste0("Aggregated_",
									file.sans.ext(basename(GeneralParameters$in.file))))
			outChunkDIR <- file.path(outDatasetDIR, "DATA")
			dir.create(outChunkDIR, showWarnings = FALSE, recursive = TRUE)

			chunkfile <- sort(unique(cdtdata$colInfo$index))
			chunkcalc <- split(chunkfile, ceiling(chunkfile / cdtdata$chunkfac))

			do.parChunk <- if(cdtdata$chunkfac > length(chunkcalc)) TRUE else FALSE
			do.parCALC <- if(do.parChunk) FALSE else TRUE

			is.parallel <- doparallel(do.parCALC & (length(chunkcalc) > 5))
			`%parLoop%` <- is.parallel$dofun
			ret <- foreach(ll = seq_along(chunkcalc), .packages = "doParallel") %parLoop% {
				don <- readCdtDatasetChunk.sequence(chunkcalc[[ll]], GeneralParameters$in.file, do.par = do.parChunk)
				don <- don[cdtdata$dateInfo$index, , drop = FALSE]

				AggrData <- cdt.data.aggregate(don, indx, pars = GeneralParameters$aggr.series)
				
				writeCdtDatasetChunk.sequence(AggrData, chunkcalc[[ll]], cdtdata, outChunkDIR, do.par = do.parChunk)

				rm(AggrData, don)
				return(0)
			}
			if(is.parallel$stop) stopCluster(is.parallel$cluster)

			#### write index data
			AggrData <- cdtdata

			AggrData$TimeStep <- GeneralParameters$time.series$out.series
			AggrData$dateInfo$date <- odaty
			AggrData$dateInfo$index <- seq_along(odaty)

			datafileIdx <- file.path(outDatasetDIR, basename(GeneralParameters$in.file))
			con <- gzfile(datafileIdx, compression = 5)
			open(con, "wb")
			saveRDS(AggrData, con)
			close(con)
			AggrData$file <- datafileIdx
		}

		.cdtData$EnvTmp$AggrData <- AggrData
		.cdtData$EnvTmp$toAggr <- toAggr
		Insert.Messages.Out("Aggregation done!")
	}else AggrData <- .cdtData$EnvTmp$AggrData

	################################################

	if(GeneralParameters$time.series$out.series == "monthly"){
		ixm <- substr(odaty, 6, 7)
		ixm <- tapply(seq(length(odaty)), ixm, identity)
		yyear <- as.numeric(substr(odaty[ixm[[1]]], 1, 4))
		odaty <- lapply(ixm, function(j) odaty[j])
	}else{
		ixm <- if(GeneralParameters$data.type == 'cdtstation') nrow(AggrData) else length(AggrData$dateInfo$index)
		ixm <- list(seq(ixm))
		yyear <- as.numeric(substr(odaty, 1, 4))
		odaty <- list(odaty)
	}

	####
	funAnalysis <- GeneralParameters$analysis.method$mth.fun

	####
	if(GeneralParameters$data.type == 'cdtstation'){
		if(funAnalysis != "anomaly"){
			if(GeneralParameters$time.series$out.series == "monthly"){
				AggrNA <- lapply(ixm, function(x){
					MAT <- AggrData[x, , drop = FALSE]
					matrix(1 - (colSums(is.na(MAT)) / nrow(MAT)), nrow = 1)
				})
			}else{
				AggrNA <- list(matrix(1 - (colSums(is.na(AggrData)) / nrow(AggrData)), nrow = 1))
			}
		}

		if(funAnalysis == "anomaly"){
			iyr <- yyear >= GeneralParameters$analysis.method$startYr.anom & yyear <= GeneralParameters$analysis.method$endYr.anom
			ixm1 <- lapply(ixm, function(ix) ix[iyr])
			climatoMean <- lapply(ixm1, function(ix){
				MAT <- AggrData[ix, , drop = FALSE]
				cdt.data.analysis(MAT, 'mean')
			})

			AnalysData <- lapply(seq_along(ixm), function(jj){
				don <- AggrData[ixm[[jj]], , drop = FALSE]
				clim <- climatoMean[[jj]]
				anom <- sweep(don, 2, clim, FUN = "-")
				if(GeneralParameters$analysis.method$perc.anom) anom <- 100 * sweep(anom, 2, clim + 0.001, FUN = "/")
				anom
			})
			AnalysData <- do.call(rbind, AnalysData)
			AnalysData <- AnalysData[order(unlist(ixm)), , drop = FALSE]
		}else if(funAnalysis == "trend"){
			AnalysData <- lapply(ixm, function(ix){
				MAT <- AggrData[ix, , drop = FALSE]
				cdt.data.analysis(MAT, 'trend', trend = list(year = yyear,
					min.year = GeneralParameters$analysis.method$trend.min.year,
					unit = GeneralParameters$analysis.method$trend.unit))
			})
		}else{
			AnalysData <- lapply(ixm, function(ix){
				MAT <- AggrData[ix, , drop = FALSE]
				cdt.data.analysis(MAT, funAnalysis,
					percentile = GeneralParameters$analysis.method$mth.perc,
					freq.thres = list(low = GeneralParameters$analysis.method$low.thres,
									up = GeneralParameters$analysis.method$up.thres))
			})
			AnalysData <- do.call(rbind, AnalysData)
		}
	}

	######
	if(GeneralParameters$data.type == 'cdtdataset'){
		if(funAnalysis == "anomaly"){
			outDatasetDIR <- file.path(outputDIR, paste0("Aggregated_",
									file.sans.ext(basename(GeneralParameters$in.file))))
			outChunkAnom <- file.path(outDatasetDIR, "TMP_ANOMALY")
			dir.create(outChunkAnom, showWarnings = FALSE, recursive = TRUE)
		}

		chunkfile <- sort(unique(AggrData$colInfo$index))
		chunkcalc <- split(chunkfile, ceiling(chunkfile / cdtdata$chunkfac))

		do.parChunk <- if(cdtdata$chunkfac > length(chunkcalc)) TRUE else FALSE
		do.parCALC <- if(do.parChunk) FALSE else TRUE

		is.parallel <- doparallel(do.parCALC & (length(chunkcalc) > 5))
		`%parLoop%` <- is.parallel$dofun
		AnalysData <- foreach(ll = seq_along(chunkcalc), .packages = "doParallel") %parLoop% {
			don <- readCdtDatasetChunk.sequence(chunkcalc[[ll]], AggrData$file, do.par = do.parChunk)

			if(funAnalysis != "anomaly"){
				if(GeneralParameters$time.series$out.series == "monthly"){
					AggrNA <- lapply(ixm, function(x){
						MAT <- don[x, , drop = FALSE]
						matrix(1 - (colSums(is.na(MAT)) / nrow(MAT)), nrow = 1)
					})
				}else{
					AggrNA <- list(matrix(1 - (colSums(is.na(don)) / nrow(don)), nrow = 1))
				}
			}

			if(funAnalysis == "anomaly"){
				iyr <- yyear >= GeneralParameters$analysis.method$startYr.anom & yyear <= GeneralParameters$analysis.method$endYr.anom
				ixm1 <- lapply(ixm, function(ix) ix[iyr])
				climatoMean <- lapply(ixm1, function(ix){
					MAT <- don[ix, , drop = FALSE]
					cdt.data.analysis(MAT, 'mean')
				})

				AnalysData <- lapply(seq_along(ixm), function(jj){
					don <- don[ixm[[jj]], , drop = FALSE]
					clim <- climatoMean[[jj]]
					anom <- sweep(don, 2, clim, FUN = "-")
					if(GeneralParameters$analysis.method$perc.anom) anom <- 100 * sweep(anom, 2, clim + 0.001, FUN = "/")
					anom
				})
				AnalysData <- do.call(rbind, AnalysData)
				AnalysData <- AnalysData[order(unlist(ixm)), , drop = FALSE]
				writeCdtDatasetChunk.sequence(AnalysData, chunkcalc[[ll]], AggrData, outChunkAnom, do.par = do.parChunk)
				return(NULL)
			}else if(funAnalysis == "trend"){
				AnalysData <- lapply(ixm, function(ix){
					MAT <- don[ix, , drop = FALSE]
					cdt.data.analysis(MAT, 'trend', trend = list(year = yyear,
						min.year = GeneralParameters$analysis.method$trend.min.year,
						unit = GeneralParameters$analysis.method$trend.unit))
				})
				return(list(Data = AnalysData, NonMiss = AggrNA))
			}else{
				AnalysData <- lapply(ixm, function(ix){
					MAT <- don[ix, , drop = FALSE]
					cdt.data.analysis(MAT, funAnalysis,
								percentile = GeneralParameters$analysis.method$mth.perc,
								freq.thres = list(low = GeneralParameters$analysis.method$low.thres,
												up = GeneralParameters$analysis.method$up.thres))
				})
				AnalysData <- do.call(rbind, AnalysData)
				return(list(Data = AnalysData, NonMiss = AggrNA))
			}
		}
		if(is.parallel$stop) stopCluster(is.parallel$cluster)

		if(funAnalysis != "anomaly"){
			AggrNA <- lapply(AnalysData, function(x) x$NonMiss)
			AggrNA <- do.call(mapply, c(cbind, AggrNA, SIMPLIFY = FALSE))
			AnalysData <- lapply(AnalysData, function(x) x$Data)
			if(funAnalysis == "trend") AnalysData <- do.call(mapply, c(cbind, AnalysData, SIMPLIFY = FALSE))
			else AnalysData <- do.call(cbind, AnalysData)
		}
	}

	################################################
	ANALYSIS <- c('Mean', 'Median', 'Standard_deviation', 'Trend',
				'Coefficient_of_variation', 'Percentiles', 'Frequency', 'Anomaly')
	analysis.dir <- switch(GeneralParameters$analysis.method$mth.fun, 
									'mean' = ANALYSIS[1],
									'median' = ANALYSIS[2], 
									'std' = ANALYSIS[3],
									'trend' = ANALYSIS[4], 
									'cv' = ANALYSIS[5],
									'percentile' = ANALYSIS[6],
									'frequency' = ANALYSIS[7], 
									'anomaly' = ANALYSIS[8])

	outAnaDIR <- file.path(outputDIR, analysis.dir)
	dir.create(outAnaDIR, showWarnings = FALSE, recursive = TRUE)

	outTSDIR <- file.path(outputDIR, "Aggregated_TimeSeries")
	dir.create(outTSDIR, showWarnings = FALSE, recursive = TRUE)

	################################################

	outAna.dates <- rep(NA, length(ixm))
	outTS.dates <- vector(mode = "list", length = length(ixm))

	################################################

	if(GeneralParameters$data.type == 'cdtdataset'){
		lon <- AggrData$coords$mat$x
		lat <- AggrData$coords$mat$y
		iorder <- AggrData$colInfo$order
		nx <- length(lon)
		ny <- length(lat)
		dx <- ncdim_def("Lon", "degreeE", lon)
		dy <- ncdim_def("Lat", "degreeN", lat)
		xy.dim <- list(dx, dy)

		if(funAnalysis != "anomaly"){
			grdNA <- ncvar_def("nonNA", "", xy.dim, NA, longname = "Fraction of the available data", prec = "float", compression = 9)
			if(funAnalysis == "trend"){
				if(GeneralParameters$analysis.method$trend.unit == 1) trend.longname <- "change or trend/year"
				if(GeneralParameters$analysis.method$trend.unit == 2) trend.longname <- "change or trend over the period"
				if(GeneralParameters$analysis.method$trend.unit == 3) trend.longname <- "change or trend/average (in %)"
				grd.slp <- ncvar_def("trend", "", xy.dim, NA, longname = trend.longname, prec = "float", compression = 9)
				grd.std.slp <- ncvar_def("std.slope", "", xy.dim, NA, longname = "Slope error", prec = "float", compression = 9)
				grd.pvalue <- ncvar_def("pvalue", "", xy.dim, NA, longname = "P-value", prec = "float", compression = 9)
				grd.r2 <- ncvar_def("r2", "", xy.dim, NA, longname = "Coefficient of determination R2", prec = "float", compression = 9)
				out.vars <- list(grd.slp, grd.std.slp, grd.pvalue, grd.r2, grdNA)
			}else{
				if(funAnalysis == "mean"){
					nc.var <- "mean"
					longname.mon <- "Mean"
				}
				if(funAnalysis == "median"){
					nc.var <- "med"
					longname.mon <- "Median"
				}
				if(funAnalysis == "std"){
					nc.var <- "std"
					longname.mon <- "Standard deviation"
				}
				if(funAnalysis == "cv"){
					nc.var <- "cv"
					longname.mon <- "Coefficient of variation"
				}
				if(funAnalysis == "percentile"){
					nc.var <- "perc"
					longname.mon <- paste0(GeneralParameters$analysis.method$mth.perc, "th Percentile")
				}
				if(funAnalysis == "frequency"){
					nc.var <- "ferq"
					longname.mon <- "Frequency, number of event every 10 years"
				}
				grdOut <- ncvar_def(nc.var, "", xy.dim, NA, longname = longname.mon, prec = "float", compression = 6)
				out.vars <- list(grdOut, grdNA)
			}

			## Analysis
			for(jj in seq_along(ixm)){
				yrsAna <- paste0(range(yeartoAna), collapse = "-")
				if(seasonLength <= 12){
					year1 <- substr(odaty[[jj]][1], 1, 4) 
					mon1 <- substr(odaty[[jj]][1], 6, 7)
					year2 <- substr(odaty[[jj]][1], 9, 12)
					mon2 <- substr(odaty[[jj]][1], 14, 15)
					if(year1 == year2){
						if(mon1 != mon2){
							dateAna <- if(mon1 == "01" & mon2 == "12") yrsAna else paste0(yrsAna, "_", paste0(mon1, "-", mon2))
						}else dateAna <- paste0(yrsAna, "_", mon1)
					}else dateAna <- paste0(yrsAna, "_", paste0(mon1, "-", mon2))
				}else dateAna <- paste0(yrsAna, "_", seasonLength)

				outAna.dates[jj] <- dateAna
				outfile <- file.path(outAnaDIR, paste0(funAnalysis, "_", dateAna, ".nc"))
				nc <- nc_create(outfile, out.vars)
				if(funAnalysis == "trend"){
					mat.slp <- as.numeric(AnalysData[[jj]][1, ])
					mat.slp <- matrix(mat.slp[iorder], ncol = ny, nrow = nx)
					mat.std.slp <- as.numeric(AnalysData[[jj]][2, ])
					mat.std.slp <- matrix(mat.std.slp[iorder], ncol = ny, nrow = nx)
					mat.pval <- as.numeric(AnalysData[[jj]][3, ])
					mat.pval <- matrix(mat.pval[iorder], ncol = ny, nrow = nx)
					mat.r2 <- as.numeric(AnalysData[[jj]][4, ])
					mat.r2 <- matrix(mat.r2[iorder], ncol = ny, nrow = nx)
					mat.na <- as.numeric(AggrNA[[jj]])
					mat.na <- matrix(mat.na[iorder], ncol = ny, nrow = nx)
					
					ncvar_put(nc, out.vars[[1]], mat.slp)
					ncvar_put(nc, out.vars[[2]], mat.std.slp)
					ncvar_put(nc, out.vars[[3]], mat.pval)
					ncvar_put(nc, out.vars[[4]], mat.r2)
					ncvar_put(nc, out.vars[[5]], mat.na)
				}else{
					mat.out <- as.numeric(AnalysData[jj, ])
					mat.out <- matrix(mat.out[iorder], ncol = ny, nrow = nx)
					mat.na <- as.numeric(AggrNA[[jj]])
					mat.na <- matrix(mat.na[iorder], ncol = ny, nrow = nx)
					ncvar_put(nc, out.vars[[1]], mat.out)
					ncvar_put(nc, out.vars[[2]], mat.na)
				}
				nc_close(nc)
			}
		}

		if(funAnalysis == "anomaly"){
			longname <- paste("Anomaly", if(GeneralParameters$analysis.method$perc.anom) "percentage of mean" else NULL)
			grdAnom <- ncvar_def("anom", "", xy.dim, NA, longname = longname, prec = "float", compression = 6)
			for(jj in seq_along(ixm)){
				tsdaty <- odaty[[jj]]
				AnalysData <- readCdtDatasetChunk.sepdir.dates.order(AggrData$file, outChunkAnom, tsdaty)
				outTSdaty <- vector(mode = "character", length = length(tsdaty))

				for(ii in seq_along(tsdaty)){
					year1 <- substr(tsdaty[ii], 1, 4) 
					mon1 <- substr(tsdaty[ii], 6, 7)
					year2 <- substr(tsdaty[ii], 9, 12)
					mon2 <- substr(tsdaty[ii], 14, 15)
					if(year1 == year2){
						if(mon1 == mon2) dateTS <- paste0(year1, mon1)
						else{
							dateTS <- if(mon1 == "01" & mon2 == "12") year1 else tsdaty[ii]
						}
					}else dateTS <- tsdaty[ii]

					outTSdaty[ii] <- dateTS
					outfile <- file.path(outAnaDIR, paste0(funAnalysis, "_", dateTS, ".nc"))
					nc <- nc_create(outfile, grdAnom)
					ncvar_put(nc, grdAnom, matrix(as.numeric(AnalysData[ii, ]), ncol = ny, nrow = nx))
					nc_close(nc)
				}

				#####
				yrsAna <- paste0(range(yeartoAna), collapse = "-")
				if(seasonLength <= 12){
					year1 <- substr(tsdaty[1], 1, 4) 
					mon1 <- substr(tsdaty[1], 6, 7)
					year2 <- substr(tsdaty[1], 9, 12)
					mon2 <- substr(tsdaty[1], 14, 15)
					if(year1 == year2){
						if(mon1 != mon2){
							dateAna <- if(mon1 == "01" & mon2 == "12") yrsAna else paste0(yrsAna, "_", paste0(mon1, "-", mon2))
						}else dateAna <- paste0(yrsAna, "_", mon1)
					}else dateAna <- paste0(yrsAna, "_", paste0(mon1, "-", mon2))
				}else dateAna <- paste0(yrsAna, "_", seasonLength)

				outAna.dates[jj] <- dateAna
			}
			unlink(outChunkAnom, recursive = TRUE)
		}

		## Time series
		if(aggregatData){
			grdTS <- ncvar_def("ts", "", xy.dim, NA, longname = "Time series", prec = "float", compression = 6)
			for(jj in seq_along(ixm)){
				tsdaty <- odaty[[jj]]
				aggrdatTS <- readCdtDatasetChunk.multi.dates.order(AggrData$file, tsdaty)
				outTSdaty <- vector(mode = "character", length = length(tsdaty))

				for(ii in seq_along(tsdaty)){
					year1 <- substr(tsdaty[ii], 1, 4) 
					mon1 <- substr(tsdaty[ii], 6, 7)
					year2 <- substr(tsdaty[ii], 9, 12)
					mon2 <- substr(tsdaty[ii], 14, 15)
					if(year1 == year2){
						if(mon1 == mon2) dateTS <- paste0(year1, mon1)
						else{
							dateTS <- if(mon1 == "01" & mon2 == "12") year1 else tsdaty[ii]
						}
					}else dateTS <- tsdaty[ii]

					outTSdaty[ii] <- dateTS
					outfile <- file.path(outTSDIR, paste0("outTS", "_", dateTS, ".nc"))
					nc <- nc_create(outfile, grdTS)
					ncvar_put(nc, grdTS, matrix(as.numeric(aggrdatTS[ii, ]), ncol = ny, nrow = nx))
					nc_close(nc)
				}
				outTS.dates[[jj]] <- list(outAna.dates[jj], outTSdaty)
			}
			.cdtData$EnvTmp$outTS.dates <- outTS.dates
		}else outTS.dates <- .cdtData$EnvTmp$outTS.dates
	}else{
		xhead <- rbind(cdtdata$id, cdtdata$lon, cdtdata$lat)
		infohead <- cbind(c('ID.STN', 'LON', 'VARS/LAT'), xhead)

		## Analysis
		if(funAnalysis != "anomaly"){
			for(jj in seq_along(ixm)){
				yrsAna <- paste0(range(yeartoAna), collapse = "-")
				if(seasonLength <= 12){
					year1 <- substr(odaty[[jj]][1], 1, 4) 
					mon1 <- substr(odaty[[jj]][1], 6, 7)
					year2 <- substr(odaty[[jj]][1], 9, 12)
					mon2 <- substr(odaty[[jj]][1], 14, 15)
					if(year1 == year2){
						if(mon1 != mon2){
							dateAna <- if(mon1 == "01" & mon2 == "12") yrsAna else paste0(yrsAna, "_", paste0(mon1, "-", mon2))
						}else dateAna <- paste0(yrsAna, "_", mon1)
					}else dateAna <- paste0(yrsAna, "_", paste0(mon1, "-", mon2))
				}else dateAna <- paste0(yrsAna, "_", seasonLength)

				outAna.dates[jj] <- dateAna
				outfile <- file.path(outAnaDIR, paste0(funAnalysis, "_", dateAna, ".csv"))
				nonmiss <- cbind("Non-missing-data", round(AggrNA[[jj]], 3))

				if(funAnalysis == "trend"){
					if(GeneralParameters$analysis.method$trend.unit == 1) trend.longname <- "change or trend/year"
					if(GeneralParameters$analysis.method$trend.unit == 2) trend.longname <- "change or trend over the period"
					if(GeneralParameters$analysis.method$trend.unit == 3) trend.longname <- "change or trend/average (in %)"
					outdata <- cbind(c(trend.longname, rownames(AnalysData[[jj]])[-1]), AnalysData[[jj]])
				}else{
					outdata <- cbind(paste0(funAnalysis, "_", dateAna), round(AnalysData[jj, , drop = FALSE], 3))
				}

				outdata <- rbind(infohead, outdata, nonmiss)
				outdata[is.na(outdata)] <- .cdtData$Config$missval
				writeFiles(outdata, outfile)
			}
		}

		if(funAnalysis == "anomaly"){
			headInfo <- cbind(c('ID.STN', 'LON', 'DATE/LAT'), xhead)
			for(jj in seq_along(ixm)){
				yrsAna <- paste0(range(yeartoAna), collapse = "-")
				year1 <- substr(odaty[[jj]], 1, 4) 
				mon1 <- substr(odaty[[jj]], 6, 7)
				year2 <- substr(odaty[[jj]], 9, 12)
				mon2 <- substr(odaty[[jj]], 14, 15)
				if(all(year1 == year2)){
					if(all(mon1 == mon2)){
						dateTS <- paste0(year1, mon1)
						outDFile <- paste0(yrsAna, "_", mon1[1])
					}else{
						if(all(mon1 == "01") & all(mon2 == "12")){
							dateTS <- year1
							outDFile <- yrsAna
						}else{
							dateTS <- odaty[[jj]]
							outDFile <- paste0(yrsAna, "_", paste0(mon1[1], "-", mon2[1]))
						}
					}
				}else{
					dateTS <- odaty[[jj]]
					outDFile <- paste0(yrsAna, "_", paste0(mon1[1], "-", mon2[1]))
				}
				outDFile <- if(seasonLength <= 12) outDFile else paste0(yrsAna, "_", seasonLength)

				outfile <- file.path(outAnaDIR, paste0(funAnalysis, "_", outDFile, ".csv"))
				outdata <- cbind(dateTS, round(AnalysData[ixm[[jj]], , drop = FALSE], 2))
				outdata <- rbind(headInfo, outdata)
				outdata[is.na(outdata)] <- .cdtData$Config$missval.anom
				writeFiles(outdata, outfile)

				####
				if(seasonLength <= 12){
					year1 <- substr(odaty[[jj]][1], 1, 4) 
					mon1 <- substr(odaty[[jj]][1], 6, 7)
					year2 <- substr(odaty[[jj]][1], 9, 12)
					mon2 <- substr(odaty[[jj]][1], 14, 15)
					if(year1 == year2){
						if(mon1 != mon2){
							dateAna <- if(mon1 == "01" & mon2 == "12") yrsAna else paste0(yrsAna, "_", paste0(mon1, "-", mon2))
						}else dateAna <- paste0(yrsAna, "_", mon1)
					}else dateAna <- paste0(yrsAna, "_", paste0(mon1, "-", mon2))
				}else dateAna <- paste0(yrsAna, "_", seasonLength)

				outAna.dates[jj] <- dateAna
			}
		}

		## Time series
		if(aggregatData){
			headInfo <- cbind(c('ID.STN', 'LON', 'DATE/LAT'), xhead)
			for(jj in seq_along(ixm)){
				yrsAna <- paste0(range(yeartoAna), collapse = "-")
				year1 <- substr(odaty[[jj]], 1, 4) 
				mon1 <- substr(odaty[[jj]], 6, 7)
				year2 <- substr(odaty[[jj]], 9, 12)
				mon2 <- substr(odaty[[jj]], 14, 15)
				if(all(year1 == year2)){
					if(all(mon1 == mon2)){
						dateTS <- paste0(year1, mon1)
						outDFile <- paste0(yrsAna, "_", mon1[1])
					}else{
						if(all(mon1 == "01") & all(mon2 == "12")){
							dateTS <- year1
							outDFile <- yrsAna
						}else{
							dateTS <- odaty[[jj]]
							outDFile <- paste0(yrsAna, "_", paste0(mon1[1], "-", mon2[1]))
						}
					}
				}else{
					dateTS <- odaty[[jj]]
					outDFile <- paste0(yrsAna, "_", paste0(mon1[1], "-", mon2[1]))
				}
				outDFile <- if(seasonLength <= 12) outDFile else paste0(yrsAna, "_", seasonLength)

				outTS.dates[[jj]] <- list(outDFile, dateTS)
				outfile <- file.path(outTSDIR, paste0("outTS", "_", outDFile, ".csv"))
				outdata <- cbind(dateTS, round(AggrData[ixm[[jj]], , drop = FALSE], 3))
				outdata <- rbind(headInfo, outdata)
				outdata[is.na(outdata)] <- .cdtData$Config$missval
				writeFiles(outdata, outfile)
			}
			.cdtData$EnvTmp$outTS.dates <- outTS.dates
		}else outTS.dates <- .cdtData$EnvTmp$outTS.dates
	}
	rm(cdtdata, AggrData, AnalysData); gc()
	out <- NULL
	out$params <- GeneralParameters
	out$monthtoAna <- monthtoAna
	out$stats <- outAna.dates
	out$timeseries <- outTS.dates
	saveRDS(out, file = file.path(outAnaDIR, "params.rds"))

	dirAnalysis <- list.dirs(outputDIR, full.names = FALSE, recursive = FALSE)
	dirAnalysis <- ANALYSIS[ANALYSIS %in% dirAnalysis]
	outStat <- list(Stats = dirAnalysis, last = analysis.dir)
	saveRDS(outStat, file = file.path(outputDIR, "SpatialAnalysis.rds"))
	.cdtData$EnvData$DirStat <- outStat
	.cdtData$EnvData$PathStat <- outputDIR
	return(0)
}
