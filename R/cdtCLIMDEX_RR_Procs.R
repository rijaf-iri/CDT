
climdexCalc.RR <- function(GeneralParameters){
	if(!dir.exists(GeneralParameters$output))
	{
		Insert.Messages.Out(paste(GeneralParameters$output, "did not find"), format = TRUE)
		return(NULL)
	}

	if(!GeneralParameters$baseYear$all.years &
		any(is.na(GeneralParameters$baseYear[c('start.year', 'end.year')])))
	{
		Insert.Messages.Out("Invalid year range", format = TRUE)
		return(NULL)
	}

	################################################

	is.Rx1day <- GeneralParameters$Indices$Rx1day
	is.Rx5day <- GeneralParameters$Indices$Rx5day
	is.SDII <- GeneralParameters$Indices$SDII
	is.R10mm <- GeneralParameters$Indices$R10mm
	is.R20mm <- GeneralParameters$Indices$R20mm
	is.Rnnmm <- GeneralParameters$Indices$Rnnmm
	is.CDD <- GeneralParameters$Indices$CDD
	is.CWD <- GeneralParameters$Indices$CWD
	is.R95pTOT <- GeneralParameters$Indices$R95pTOT
	is.R99pTOT <- GeneralParameters$Indices$R99pTOT
	is.PRCPTOT <- GeneralParameters$Indices$PRCPTOT
	
	indxlst <- c("Rx1day", "Rx5day", "SDII", "R10mm", "R20mm", "Rnnmm",
					"CDD", "CWD", "R95pTOT", "R99pTOT", "PRCPTOT")
	is.indxlst <- unlist(GeneralParameters$Indices[indxlst])

	if(!any(is.indxlst))
	{
		InsertMessagesTxt(main.txt.out, 'No indices selected.', format = TRUE)
		return(0)
	}

	################################################

	if(GeneralParameters$data.type == "cdtstation")
	{
		don <- getStnOpenData(GeneralParameters$cdtstation)
		if(is.null(don)) return(NULL)
		don <- getCDTdataAndDisplayMsg(don, "daily", GeneralParameters$cdtstation)
		if(is.null(don)) return(NULL)

		daty <- don$dates
	}

	if(GeneralParameters$data.type == "cdtdataset")
	{
		don <- try(readRDS(GeneralParameters$cdtdataset), silent = TRUE)
		if(inherits(don, "try-error")){
			Insert.Messages.Out(paste("Unable to read", GeneralParameters$cdtdataset), format = TRUE)
			return(NULL)
		}
		if(don$TimeStep != "daily"){
			Insert.Messages.Out(paste("The dataset is not a daily data"), format = TRUE)
			return(NULL)
		}

		daty <- don$dateInfo$date
	}

	#########################################

	# index.mon <- cdt.index.aggregate(daty, "daily", "monthly")
	# ifull.mon <- (index.mon$nba / index.mon$nb0) >= 0.9
	# m.nrow <- length(index.mon$date)
	# index.M <- list(index = index.mon$index, full = ifull.mon)

	index.year.N <- cdt.index.aggregate(daty, "daily", "seasonal", 12, 1)

	index.year.S <- NULL
	if(GeneralParameters$start.july)
		index.year.S <- cdt.index.aggregate(daty, "daily", "seasonal", 12, 7)

	#########################################

	outDIR <- file.path(GeneralParameters$output, "CLIMDEX_PRECIP_data")
	dir.create(outDIR, showWarnings = FALSE, recursive = TRUE)
	file.index <- file.path(outDIR, "climdex.rds")

	#########################################

	if(GeneralParameters$data.type == "cdtstation")
	{
		xhead <- do.call(rbind, don[c('id', 'lon', 'lat')])

		################################
		index.NS <- climdex.north.south(index.year.N, index.year.S,
								GeneralParameters$start.july, don$lat, 0.9)
		y.nrow <- length(index.NS$year)
		d.ncol <- ncol(don$data)

		# ndim <- list(ncol = d.ncol, y.nrow = y.nrow, m.nrow = m.nrow)
		ndim <- list(ncol = d.ncol, y.nrow = y.nrow, m.nrow = NA)
		pars.trend <- list(year = as.numeric(index.NS$year), min.year = GeneralParameters$baseYear$min.year)

		################################
		# Rx1day: Monthly maximum 1-day precipitation
		if(is.Rx1day){
			pars.Rx1day <- list(min.frac = 0.95, aggr.fun = "max")
			Rx1day <- climdex_aggr.fun(don$data, GeneralParameters$start.july,
									ndim, pars.Rx1day, pars.trend, index.NS,
									month.data = FALSE, index.M = NULL)

			climdex.write.cdtstation(Rx1day, index.NS$year, outDIR, "Rx1day", xhead)
			rm(Rx1day)
		}

		################################
		# Rx5day: Monthly maximum consecutive 5-day precipitation
		if(is.Rx5day){
			pars.Rx5day <- list(min.frac = 0.95, aggr.fun = "max")
			rr5day <- cdt.roll.fun(don$data, win = 5, align = "right")
			Rx5day <- climdex_aggr.fun(rr5day, GeneralParameters$start.july,
									ndim, pars.Rx5day, pars.trend, index.NS,
									month.data = FALSE, index.M = NULL)
			rm(rr5day)
			climdex.write.cdtstation(Rx5day, index.NS$year, outDIR, "Rx5day", xhead)
			rm(Rx5day)
		}

		################################
		# SDII: Simple pricipitation intensity index
		if(is.SDII){
			pars.SDII <- list(min.frac = 0.95, aggr.fun = "sum")
			ix.sdii <- climdex_aggr.fun(don$data, GeneralParameters$start.july,
										ndim, pars.SDII, pars.trend, index.NS)
			don.sdii <- don$data
			don.sdii[don.sdii < 1] <- NA
			pars.SDII <- list(min.frac = 0.01, aggr.fun = "mean")
			SDII <- climdex_aggr.fun(don.sdii, GeneralParameters$start.july,
									ndim, pars.SDII, pars.trend, index.NS)
			SDII$year[is.na(ix.sdii$year)] <- NA
			SDII$trend[is.na(ix.sdii$trend)] <- NA
			rm(don.sdii, ix.sdii)
			climdex.write.cdtstation(SDII, index.NS$year, outDIR, "SDII", xhead)
			rm(SDII)
		}

		################################
		# R10mm: Annual count of days when PRCP ≥ 10mm
		if(is.R10mm){
			pars.R10mm <- list(min.frac = 0.95, aggr.fun = "count",
								opr.fun = ">=", opr.thres = 10)
			R10mm <- climdex_aggr.fun(don$data, GeneralParameters$start.july,
									ndim, pars.R10mm, pars.trend, index.NS)
			climdex.write.cdtstation(R10mm, index.NS$year, outDIR, "R10mm", xhead)
			rm(R10mm)
		}

		################################
		# R20mm: Annual count of days when PRCP ≥ 20mm
		if(is.R20mm){
			pars.R20mm <- list(min.frac = 0.95, aggr.fun = "count",
								opr.fun = ">=", opr.thres = 20)
			R20mm <- climdex_aggr.fun(don$data, GeneralParameters$start.july,
									ndim, pars.R20mm, pars.trend, index.NS)
			climdex.write.cdtstation(R20mm, index.NS$year, outDIR, "R20mm", xhead)
			rm(R20mm)
		}

		################################
		# Rnnmm: Annual count of days when PRCP ≥ nnmm
		if(is.Rnnmm){
			pars.Rnnmm <- list(min.frac = 0.95, aggr.fun = "count", opr.fun = ">=",
								opr.thres = GeneralParameters$Indices$thres.Rnnmm)
			Rnnmm <- climdex_aggr.fun(don$data, GeneralParameters$start.july,
									ndim, pars.Rnnmm, pars.trend, index.NS)
			climdex.write.cdtstation(Rnnmm, index.NS$year, outDIR, "Rnnmm", xhead)
			rm(Rnnmm)
		}

		################################
		# CDD: Maximum length of dry spell, maximum number of consecutive days with RR < 1mm
		if(is.CDD){
			pars.CDD <- list(min.frac = 0.95, aggr.fun = "count.rle.max",
								opr.fun = "<", opr.thres = 1)
			CDD <- climdex_aggr.fun(don$data, GeneralParameters$start.july,
									ndim, pars.CDD, pars.trend, index.NS)
			climdex.write.cdtstation(CDD, index.NS$year, outDIR, "CDD", xhead)
			rm(CDD)
		}

		################################
		# CWD: Maximum length of wet spell, maximum number of consecutive days with RR ≥ 1mm
		if(is.CWD){
			pars.CWD <- list(min.frac = 0.95, aggr.fun = "count.rle.max",
								opr.fun = ">=", opr.thres = 1)
			CWD <- climdex_aggr.fun(don$data, GeneralParameters$start.july,
									ndim, pars.CWD, pars.trend, index.NS)
			climdex.write.cdtstation(CWD, index.NS$year, outDIR, "CWD", xhead)
			rm(CWD)
		}

		################################
		if(is.R95pTOT | is.R99pTOT){
			year <- as.numeric(substr(daty, 1, 4))
			if(!GeneralParameters$baseYear$all.years){
				iyear <- year >= GeneralParameters$baseYear$start.year &
						year <= GeneralParameters$baseYear$end.year
			}else iyear <- rep(TRUE, length(year))

			precentiles <- don$data[iyear, , drop = FALSE]
			miss.data <- colSums(!is.na(precentiles)) < 365 * GeneralParameters$baseYear$min.year
			precentiles[precentiles < 1] <- NA
			precentiles <- matrixStats::colQuantiles(precentiles, probs = c(0.95, 0.99), na.rm = TRUE, type = 8)
			precentiles <- precentiles[!miss.data, , drop = FALSE]
			pTOT <- TRUE
			if(nrow(precentiles) == 0){
				pTOT <- FALSE
				Insert.Messages.Out("No enough data, R95pTOT and R99pTOT will not be computed", format = TRUE)
			}
		}

		################################
		# R95pTOT: Annual total PRCP when RR > 95p
		if(is.R95pTOT & pTOT){
			don.r95 <- don$data[, !miss.data, drop = FALSE]
			ix95 <- sweep(don.r95, 2, precentiles[, 1], FUN = '<')
			don.r95[ix95 & !is.na(don.r95)] <- 0

			pars.R95pTOT <- list(min.frac = 0.95, aggr.fun = "sum")
			R95pTOT <- climdex_aggr.fun(don.r95, GeneralParameters$start.july,
										ndim, pars.R95pTOT, pars.trend, index.NS)
			rm(ix95, don.r95)

			year.don <- matrix(NA, y.nrow, d.ncol)
			year.don[, !miss.data] <- R95pTOT$year
			R95pTOT$year <- year.don
			rm(year.don)
			trend.don <- matrix(NA, 10, d.ncol)
			trend.don[, !miss.data] <- R95pTOT$trend
			R95pTOT$trend <- trend.don
			rm(trend.don)

			climdex.write.cdtstation(R95pTOT, index.NS$year, outDIR, "R95pTOT", xhead)
			rm(R95pTOT)
		}

		################################
		# R99pTOT: Annual total PRCP when RR > 99p
		if(is.R99pTOT & pTOT){
			don.r99 <- don$data[, !miss.data, drop = FALSE]
			ix99 <- sweep(don.r99, 2, precentiles[, 2], FUN = '<')
			don.r99[ix99 & !is.na(don.r99)] <- 0

			pars.R99pTOT <- list(min.frac = 0.95, aggr.fun = "sum")
			R99pTOT <- climdex_aggr.fun(don.r99, GeneralParameters$start.july,
										ndim, pars.R99pTOT, pars.trend, index.NS)
			rm(ix99, don.r99)

			year.don <- matrix(NA, y.nrow, d.ncol)
			year.don[, !miss.data] <- R99pTOT$year
			R99pTOT$year <- year.don
			rm(year.don)
			trend.don <- matrix(NA, 10, d.ncol)
			trend.don[, !miss.data] <- R99pTOT$trend
			R99pTOT$trend <- trend.don
			rm(trend.don)

			climdex.write.cdtstation(R99pTOT, index.NS$year, outDIR, "R99pTOT", xhead)
			rm(R99pTOT)
		}

		################################
		# PRCPTOT: Annual total precipitation in wet days
		if(is.PRCPTOT){
			don.wet <- don$data
			don.wet[don.wet < 1 & is.na(don.wet)] <- 0

			pars.PRCPTOT <- list(min.frac = 0.95, aggr.fun = "sum")
			PRCPTOT <- climdex_aggr.fun(don.wet, GeneralParameters$start.july,
										ndim, pars.PRCPTOT, pars.trend, index.NS)
			rm(don.wet)
			climdex.write.cdtstation(PRCPTOT, index.NS$year, outDIR, "PRCPTOT", xhead)
			rm(PRCPTOT)
		}

		######################

		output <- list(params = GeneralParameters, year = index.NS$year, data = don[c('id', 'lon', 'lat')])
	}

	#########################################

	if(GeneralParameters$data.type == "cdtdataset")
	{
		dir.cdtdata <- file.path(outDIR, 'CDTDATASET')
		dir.create(dir.cdtdata, showWarnings = FALSE, recursive = TRUE)
		dir.netcdf <- file.path(outDIR, 'DATA_NetCDF')
		dir.create(dir.netcdf, showWarnings = FALSE, recursive = TRUE)

		################################

		latitude <- don$coord$df$y
		index.NS <- climdex.north.south(index.year.N, index.year.S,
								GeneralParameters$start.july, latitude, 0.9)
		y.nrow <- length(index.NS$year)
		pars.trend <- list(year = as.numeric(index.NS$year), min.year = GeneralParameters$baseYear$min.year)

		################################

		index.data.year <- don
		index.data.year$dateInfo$date <- index.NS$year
		index.data.year$dateInfo$index <- seq_along(index.NS$year)
		index.data.year$TimeStep <- "annual"

		trend.vars <- c("slope", "std.slope", "t-value.slope", "p-value.slope",
						"intercept", "std.intercept", "t-value.intercept",
						"p-value.intercept", "R2", "sigma")
		index.data.trend <- don
		index.data.trend$dateInfo$date <- trend.vars
		index.data.trend$dateInfo$index <- 1:10
		index.data.trend$TimeStep <- "others"

		varInfo <- lapply(indxlst[is.indxlst], function(x){
			if(x == "Rx1day"){
				varinfo <- list(name = x, prec = "float", units = "mm",
								longname = "Monthly maximum 1-day precipitation")
			}
			if(x == "Rx5day"){
				varinfo <- list(name = x, prec = "float", units = "mm",
								longname = "Monthly maximum consecutive 5-day precipitation")
			}
			if(x == "SDII"){
				varinfo <- list(name = x, prec = "float", units = "mm",
								longname = "Simple pricipitation intensity index")
			}
			if(x == "R10mm"){
				varinfo <- list(name = x, prec = "short", units = "days",
								longname = "Annual count of days when PRCP ≥ 10mm")
			}
			if(x == "R20mm"){
				varinfo <- list(name = x, prec = "short", units = "days",
								longname = "Annual count of days when PRCP ≥ 20mm")
			}
			if(x == "Rnnmm"){
				varinfo <- list(name = x, prec = "short", units = "days",
								longname = paste0("Annual count of days when PRCP ≥ ", GeneralParameters$Indices$thres.Rnnmm, "mm"))
			}
			if(x == "CDD"){
				varinfo <- list(name = x, prec = "short", units = "days",
								longname = "Maximum length of dry spell, maximum number of consecutive days with RR < 1mm")
			}
			if(x == "CWD"){
				varinfo <- list(name = x, prec = "short", units = "days",
								longname = "Maximum length of wet spell, maximum number of consecutive days with RR ≥ 1mm")
			}
			if(x == "R95pTOT"){
				varinfo <- list(name = x, prec = "float", units = "mm",
								longname = "Annual total PRCP when RR > 95 percentile")
			}
			if(x == "R99pTOT"){
				varinfo <- list(name = x, prec = "float", units = "mm",
								longname = "Annual total PRCP when RR > 99 percentile")
			}
			if(x == "PRCPTOT"){
				varinfo <- list(name = x, prec = "float", units = "mm",
								longname = "Annual total precipitation in wet days")
			}

			return(varinfo)
		})
		names(varInfo) <- indxlst[is.indxlst]

		################################

		paths.index <- lapply(indxlst[is.indxlst], function(x){
			dir.var <- file.path(dir.cdtdata, x)
			dir.year <- file.path(dir.var, 'Yearly')
			dir.trend <- file.path(dir.var, 'Trend')
			dir.year.data <- file.path(dir.year, "DATA")
			dir.trend.data <- file.path(dir.trend, "DATA")
			dir.create(dir.year.data, showWarnings = FALSE, recursive = TRUE)
			dir.create(dir.trend.data, showWarnings = FALSE, recursive = TRUE)

			file.index.year <- file.path(dir.year, paste0(x, '.rds'))
			file.index.year.gz <- gzfile(file.index.year, compression = 7)
			index.data.year$varInfo <- varInfo[[x]]
			saveRDS(index.data.year, file.index.year.gz)
			close(file.index.year.gz)

			file.index.trend <- file.path(dir.trend, paste0(x, '.rds'))
			file.index.trend.gz <- gzfile(file.index.trend, compression = 7)
			index.data.trend$varInfo <- varInfo[[x]]
			saveRDS(index.data.trend, file.index.trend.gz)
			close(file.index.trend.gz)

			paths <- list(dir.year.data = dir.year.data,
						dir.trend.data = dir.trend.data,
						file.index.year = file.index.year,
						file.index.trend = file.index.trend)
			return(paths)
		})
		names(paths.index) <- indxlst[is.indxlst]

		################################
		chunkfile <- sort(unique(don$colInfo$index))
		chunkcalc <- split(chunkfile, ceiling(chunkfile / don$chunkfac))

		do.parChunk <- if(don$chunkfac > length(chunkcalc)) TRUE else FALSE
		do.parCALC <- if(do.parChunk) FALSE else TRUE

		is.parallel <- doparallel(do.parCALC & (length(chunkcalc) > 10))
		`%parLoop%` <- is.parallel$dofun
		ret <- foreach(chkj = seq_along(chunkcalc), .packages = "doParallel") %parLoop% {
			don.data <- readCdtDatasetChunk.sequence(chunkcalc[[chkj]], GeneralParameters$cdtdataset, do.par = do.parChunk)
			don.data <- don.data[don$dateInfo$index, , drop = FALSE]

			d.ncol <- ncol(don.data)
			ndim <- list(ncol = d.ncol, y.nrow = y.nrow, m.nrow = NA)

			latitude <- don$coord$df$y[don$colInfo$index %in% chunkcalc[[chkj]]]
			index.NS0 <- climdex.north.south(index.year.N, index.year.S,
						GeneralParameters$start.july, latitude, 0.9)
			index.NS1 <- index.NS
			index.NS1$index.S$idx <- index.NS0$index.S$idx
			index.NS1$index.S$ncol <- index.NS0$index.S$ncol
			index.NS1$index.N$idx <- index.NS0$index.N$idx
			index.NS1$index.N$ncol <- index.NS0$index.N$ncol
			rm(index.NS0)

			################################
			# Rx1day: Monthly maximum 1-day precipitation
			if(is.Rx1day){
				pars.Rx1day <- list(min.frac = 0.95, aggr.fun = "max")
				Rx1day <- climdex_aggr.fun(don.data, GeneralParameters$start.july,
										ndim, pars.Rx1day, pars.trend, index.NS1)
				writeCdtDatasetChunk.sequence(Rx1day$year, chunkcalc[[chkj]], index.data.year,
											paths.index[['Rx1day']]$dir.year.data, do.par = do.parChunk)
				writeCdtDatasetChunk.sequence(Rx1day$trend, chunkcalc[[chkj]], index.data.trend,
											paths.index[['Rx1day']]$dir.trend.data, do.par = do.parChunk)
				rm(Rx1day)
			}

			################################
			# Rx5day: Monthly maximum consecutive 5-day precipitation
			if(is.Rx5day){
				pars.Rx5day <- list(min.frac = 0.95, aggr.fun = "max")
				rr5day <- cdt.roll.fun(don.data, win = 5, align = "right")
				Rx5day <- climdex_aggr.fun(rr5day, GeneralParameters$start.july,
										ndim, pars.Rx5day, pars.trend, index.NS1)
				rm(rr5day)
				writeCdtDatasetChunk.sequence(Rx5day$year, chunkcalc[[chkj]], index.data.year,
											paths.index[["Rx5day"]]$dir.year.data, do.par = do.parChunk)
				writeCdtDatasetChunk.sequence(Rx5day$trend, chunkcalc[[chkj]], index.data.trend,
											paths.index[["Rx5day"]]$dir.trend.data, do.par = do.parChunk)
				rm(Rx5day)
			}

			################################
			# SDII: Simple pricipitation intensity index
			if(is.SDII){
				pars.SDII <- list(min.frac = 0.95, aggr.fun = "sum")
				ix.sdii <- climdex_aggr.fun(don.data, GeneralParameters$start.july,
											ndim, pars.SDII, pars.trend, index.NS1)
				don.sdii <- don.data
				don.sdii[don.sdii < 1] <- NA
				pars.SDII <- list(min.frac = 0.01, aggr.fun = "mean")
				SDII <- climdex_aggr.fun(don.sdii, GeneralParameters$start.july,
										ndim, pars.SDII, pars.trend, index.NS1)
				SDII$year[is.na(ix.sdii$year)] <- NA
				SDII$trend[is.na(ix.sdii$trend)] <- NA
				rm(don.sdii, ix.sdii)
				writeCdtDatasetChunk.sequence(SDII$year, chunkcalc[[chkj]], index.data.year,
											paths.index[["SDII"]]$dir.year.data, do.par = do.parChunk)
				writeCdtDatasetChunk.sequence(SDII$trend, chunkcalc[[chkj]], index.data.trend,
											paths.index[["SDII"]]$dir.trend.data, do.par = do.parChunk)
				rm(SDII)
			}

			################################
			# R10mm: Annual count of days when PRCP ≥ 10mm
			if(is.R10mm){
				pars.R10mm <- list(min.frac = 0.95, aggr.fun = "count",
									opr.fun = ">=", opr.thres = 10)
				R10mm <- climdex_aggr.fun(don.data, GeneralParameters$start.july,
										ndim, pars.R10mm, pars.trend, index.NS1)
				writeCdtDatasetChunk.sequence(R10mm$year, chunkcalc[[chkj]], index.data.year,
											paths.index[["R10mm"]]$dir.year.data, do.par = do.parChunk)
				writeCdtDatasetChunk.sequence(R10mm$trend, chunkcalc[[chkj]], index.data.trend,
											paths.index[["R10mm"]]$dir.trend.data, do.par = do.parChunk)
				rm(R10mm)
			}

			################################
			# R20mm: Annual count of days when PRCP ≥ 20mm
			if(is.R20mm){
				pars.R20mm <- list(min.frac = 0.95, aggr.fun = "count",
									opr.fun = ">=", opr.thres = 20)
				R20mm <- climdex_aggr.fun(don.data, GeneralParameters$start.july,
										ndim, pars.R20mm, pars.trend, index.NS1)
				writeCdtDatasetChunk.sequence(R20mm$year, chunkcalc[[chkj]], index.data.year,
											paths.index[["R20mm"]]$dir.year.data, do.par = do.parChunk)
				writeCdtDatasetChunk.sequence(R20mm$trend, chunkcalc[[chkj]], index.data.trend,
											paths.index[["R20mm"]]$dir.trend.data, do.par = do.parChunk)
				rm(R20mm)
			}

			################################
			# Rnnmm: Annual count of days when PRCP ≥ nnmm
			if(is.Rnnmm){
				pars.Rnnmm <- list(min.frac = 0.95, aggr.fun = "count", opr.fun = ">=",
									opr.thres = GeneralParameters$Indices$thres.Rnnmm)
				Rnnmm <- climdex_aggr.fun(don.data, GeneralParameters$start.july,
										ndim, pars.Rnnmm, pars.trend, index.NS1)
				writeCdtDatasetChunk.sequence(Rnnmm$year, chunkcalc[[chkj]], index.data.year,
											paths.index[["Rnnmm"]]$dir.year.data, do.par = do.parChunk)
				writeCdtDatasetChunk.sequence(Rnnmm$trend, chunkcalc[[chkj]], index.data.trend,
											paths.index[["Rnnmm"]]$dir.trend.data, do.par = do.parChunk)
				rm(Rnnmm)
			}

			################################
			# CDD: Maximum length of dry spell, maximum number of consecutive days with RR < 1mm
			if(is.CDD){
				pars.CDD <- list(min.frac = 0.95, aggr.fun = "count.rle.max",
									opr.fun = "<", opr.thres = 1)
				CDD <- climdex_aggr.fun(don.data, GeneralParameters$start.july,
										ndim, pars.CDD, pars.trend, index.NS1)
				writeCdtDatasetChunk.sequence(CDD$year, chunkcalc[[chkj]], index.data.year,
											paths.index[["CDD"]]$dir.year.data, do.par = do.parChunk)
				writeCdtDatasetChunk.sequence(CDD$trend, chunkcalc[[chkj]], index.data.trend,
											paths.index[["CDD"]]$dir.trend.data, do.par = do.parChunk)
				rm(CDD)
			}

			################################
			# CWD: Maximum length of wet spell, maximum number of consecutive days with RR ≥ 1mm
			if(is.CWD){
				pars.CWD <- list(min.frac = 0.95, aggr.fun = "count.rle.max",
									opr.fun = ">=", opr.thres = 1)
				CWD <- climdex_aggr.fun(don.data, GeneralParameters$start.july,
										ndim, pars.CWD, pars.trend, index.NS1)
				writeCdtDatasetChunk.sequence(CWD$year, chunkcalc[[chkj]], index.data.year,
											paths.index[["CWD"]]$dir.year.data, do.par = do.parChunk)
				writeCdtDatasetChunk.sequence(CWD$trend, chunkcalc[[chkj]], index.data.trend,
											paths.index[["CWD"]]$dir.trend.data, do.par = do.parChunk)
				rm(CWD)
			}

			################################
			if(is.R95pTOT | is.R99pTOT){
				year <- as.numeric(substr(daty, 1, 4))
				if(!GeneralParameters$baseYear$all.years){
					iyear <- year >= GeneralParameters$baseYear$start.year &
							year <= GeneralParameters$baseYear$end.year
				}else iyear <- rep(TRUE, length(year))

				precentiles <- don.data[iyear, , drop = FALSE]
				miss.data <- colSums(!is.na(precentiles)) < 365 * GeneralParameters$baseYear$min.year
				precentiles[precentiles < 1] <- NA
				precentiles <- matrixStats::colQuantiles(precentiles, probs = c(0.95, 0.99), na.rm = TRUE, type = 8)
				precentiles <- precentiles[!miss.data, , drop = FALSE]
				pTOT <- if(nrow(precentiles) == 0) FALSE else TRUE
			}

			################################
			# R95pTOT: Annual total PRCP when RR > 95p
			if(is.R95pTOT & pTOT){
				don.r95 <- don.data[, !miss.data, drop = FALSE]
				ix95 <- sweep(don.r95, 2, precentiles[, 1], FUN = '<')
				don.r95[ix95 & !is.na(don.r95)] <- 0

				pars.R95pTOT <- list(min.frac = 0.95, aggr.fun = "sum")
				R95pTOT <- climdex_aggr.fun(don.r95, GeneralParameters$start.july,
											ndim, pars.R95pTOT, pars.trend, index.NS1)
				rm(ix95, don.r95)

				year.don <- matrix(NA, y.nrow, d.ncol)
				year.don[, !miss.data] <- R95pTOT$year
				R95pTOT$year <- year.don
				rm(year.don)
				trend.don <- matrix(NA, 10, d.ncol)
				trend.don[, !miss.data] <- R95pTOT$trend
				R95pTOT$trend <- trend.don
				rm(trend.don)

				writeCdtDatasetChunk.sequence(R95pTOT$year, chunkcalc[[chkj]], index.data.year,
											paths.index[["R95pTOT"]]$dir.year.data, do.par = do.parChunk)
				writeCdtDatasetChunk.sequence(R95pTOT$trend, chunkcalc[[chkj]], index.data.trend,
											paths.index[["R95pTOT"]]$dir.trend.data, do.par = do.parChunk)
				rm(R95pTOT)
			}

			################################
			# R99pTOT: Annual total PRCP when RR > 99p
			if(is.R99pTOT & pTOT){
				don.r99 <- don.data[, !miss.data, drop = FALSE]
				ix99 <- sweep(don.r99, 2, precentiles[, 2], FUN = '<')
				don.r99[ix99 & !is.na(don.r99)] <- 0

				pars.R99pTOT <- list(min.frac = 0.95, aggr.fun = "sum")
				R99pTOT <- climdex_aggr.fun(don.r99, GeneralParameters$start.july,
											ndim, pars.R99pTOT, pars.trend, index.NS1)
				rm(ix99, don.r99)

				year.don <- matrix(NA, y.nrow, d.ncol)
				year.don[, !miss.data] <- R99pTOT$year
				R99pTOT$year <- year.don
				rm(year.don)
				trend.don <- matrix(NA, 10, d.ncol)
				trend.don[, !miss.data] <- R99pTOT$trend
				R99pTOT$trend <- trend.don
				rm(trend.don)

				writeCdtDatasetChunk.sequence(R99pTOT$year, chunkcalc[[chkj]], index.data.year,
											paths.index[["R99pTOT"]]$dir.year.data, do.par = do.parChunk)
				writeCdtDatasetChunk.sequence(R99pTOT$trend, chunkcalc[[chkj]], index.data.trend,
											paths.index[["R99pTOT"]]$dir.trend.data, do.par = do.parChunk)
				rm(R99pTOT)
			}

			################################
			# PRCPTOT: Annual total precipitation in wet days
			if(is.PRCPTOT){
				don.wet <- don.data
				don.wet[don.wet < 1 & is.na(don.wet)] <- 0

				pars.PRCPTOT <- list(min.frac = 0.95, aggr.fun = "sum")
				PRCPTOT <- climdex_aggr.fun(don.wet, GeneralParameters$start.july,
											ndim, pars.PRCPTOT, pars.trend, index.NS1)
				rm(don.wet)
				writeCdtDatasetChunk.sequence(PRCPTOT$year, chunkcalc[[chkj]], index.data.year,
											paths.index[["PRCPTOT"]]$dir.year.data, do.par = do.parChunk)
				writeCdtDatasetChunk.sequence(PRCPTOT$trend, chunkcalc[[chkj]], index.data.trend,
											paths.index[["PRCPTOT"]]$dir.trend.data, do.par = do.parChunk)
				rm(PRCPTOT)
			}

			rm(don.data, index.NS1); gc()
		}
		if(is.parallel$stop) stopCluster(is.parallel$cluster)

		######################

		x <- don$coords$mat$x
		y <- don$coords$mat$y
		nx <- length(x)
		ny <- length(y)
		dx <- ncdim_def("Lon", "degreeE", x)
		dy <- ncdim_def("Lat", "degreeN", y)
		xy.dim <- list(dx, dy)

		######################
		trend.vars.name <- c("slope", "std.slope", "t.value.slope", "p.value.slope",
							"intercept", "std.intercept", "t.value.intercept",
							"p.value.intercept", "R2", "sigma")
		trend.vars.longname <- c(
								"Slope - Estimate", "Slope - Standard Error", "Slope t-value", "Slope p-value Pr(>t)",
								"Intercept - Estimate", "Intercept - Standard Error", "Intercept t-value", "Intercept p-value Pr(>t)",
								"Multiple R-squared", "Residual Standard Error"
								)
		trend.vars.grd <- lapply(seq_along(trend.vars.name), function(j){
			grd <- ncvar_def(trend.vars.name[j], "", xy.dim, NA, longname = trend.vars.longname[j], prec = "float", compression = 9)
			grd
		})

		######################

		ret <- lapply(indxlst[is.indxlst], function(nom.idx){
			dir.year <- file.path(dir.netcdf, nom.idx, 'Yearly')
			dir.trend <- file.path(dir.netcdf, nom.idx, 'Trend')
			dir.create(dir.year, showWarnings = FALSE, recursive = TRUE)
			dir.create(dir.trend, showWarnings = FALSE, recursive = TRUE)

			vars <- varInfo[[nom.idx]]
			nc.grd <- ncvar_def(vars$name, vars$units, xy.dim, -99, vars$longname, vars$prec, compression = 9)
			data.year <- readCdtDatasetChunk.multi.dates.order(paths.index[[nom.idx]]$file.index.year, index.NS$year)
			for(j in seq_along(index.NS$year)){
				don.year <- data.year[j, ]
				dim(don.year) <- c(nx, ny)
				don.year[is.na(don.year)] <- -99

				filenc <- file.path(dir.year, paste0(nom.idx, "_", index.NS$year[j], ".nc"))
				nc <- nc_create(filenc, nc.grd)
				ncvar_put(nc, nc.grd, don.year)
				nc_close(nc)
			}
			rm(don.year, data.year)

			data.trend <- readCdtDatasetChunk.multi.dates.order(paths.index[[nom.idx]]$file.index.trend, trend.vars)
			outfile <- file.path(dir.trend, paste0(nom.idx, ".nc"))
			nc <- nc_create(outfile, trend.vars.grd)
			for(j in seq_along(trend.vars.grd)){
				trend.tmp <- data.trend[j, ]
				dim(trend.tmp) <- c(nx, ny)
				ncvar_put(nc, trend.vars.grd[[j]], trend.tmp)
			}
			nc_close(nc)
			rm(trend.tmp, data.trend)
			return(0)
		})

		######################

		output <- list(params = GeneralParameters, year = index.NS$year, data = don$coords$mat)
	}

	#########################################

	.cdtData$EnvData$output <- output
	.cdtData$EnvData$PathData <- outDIR

	con <- gzfile(file.index, compression = 7)
	open(con, "wb")
	saveRDS(output, con)
	close(con)

	return(0)
}
