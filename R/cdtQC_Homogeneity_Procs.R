
homogeneityTestProcs <- function(GeneralParameters){
	if(!dir.exists(GeneralParameters$outdir)){
		Insert.Messages.Out(paste(GeneralParameters$outdir, "did not find"), format = TRUE)
		return(NULL)
	}

	don <- getStnOpenData(GeneralParameters$infile)
	if(is.null(don)) return(NULL)
	don <- getCDTdataAndDisplayMsg(don, GeneralParameters$intstep, GeneralParameters$infile)
	if(is.null(don)) return(NULL)

	###################
	outdir <- file.path(GeneralParameters$outdir, "HOMOGENIZATION_data")
	dataCDTdir <- file.path(outdir, 'CDTDATASET')
	dir.create(dataCDTdir, showWarnings = FALSE, recursive = TRUE)
	dataSTNdir <- file.path(outdir, 'CDTSTATIONS')
	dir.create(dataSTNdir, showWarnings = FALSE, recursive = TRUE)

	###################

	don.info <- getStnOpenDataInfo(GeneralParameters$infile)
	if(don.info[[3]]$sepr == "") don.info[[3]]$sepr <- " "
	don.info[[3]]$header <- FALSE
	don.info[[3]]$skip <- 0
	if(is.null(don$elv))
		head <- c("STN", "LON", "DATE/LAT")
	else
		head <- c("STN", "LON", "LAT", "DATE/ELEV")
	
	idim <- c('id', 'lon', 'lat', 'elv')
	xhead <- cbind(head, do.call(rbind, don[idim]))

	###################

	candS <- homog.AggregateSeries(don, GeneralParameters$intstep, GeneralParameters$aggr)

	###################

	ret <- lapply(candS, function(dat){
		if(is.null(dat)) return(NULL)
		file.stn <- file.path(dataSTNdir, paste0(toupper(dat$tstep), '_', GeneralParameters$infile))
		don0 <- rbind(xhead, cbind(dat$date, round(dat$data, 1)))
		write.table(don0, file = file.stn,
					sep = don.info[[3]]$sepr, na = don.info[[3]]$miss.val,
					col.names = FALSE, row.names = FALSE, quote = FALSE)
		rm(don0)
		return(NULL)
	})

	###################

	parStats <- GeneralParameters$stats

	frac.data.mon <- available.data.fraction(candS$don3$data)

	id.ret <- (frac.data.mon$length / 12) >= parStats$min.year & frac.data.mon$frac >= parStats$min.frac
	if(!any(id.ret)){
		Insert.Messages.Out("No enough data to perform test for all stations", format = TRUE)
		return(NULL)
	}

	MSG <- vector("list", length(don$id))
	names(MSG) <- don$id
	don0.id <- don$id

	don[idim] <- lapply(don[idim], function(x) x[id.ret])
	don$data <- don$data[, id.ret, drop = FALSE]
	don <- don[c(idim, 'dates', 'data')]

	candS <- lapply(candS, function(dat){
		if(is.null(dat)) return(NULL)
		dat$data <- dat$data[, id.ret, drop = FALSE]
		dat
	})

	MSG[!id.ret] <- "No Test: no.enough.data"

	outREFs <- list(stn.id = don$id, voisin = NULL, voisin1 = NULL,
					msg = MSG, exclude = don0.id[!id.ret])

	###################

	parSeries <- GeneralParameters$series

	if(parSeries$use)
	{
		if(parSeries$user$refs)
		{
			don1 <- getStnOpenData(parSeries$user$file)
			if(is.null(don1)) return(NULL)
			don1 <- getCDTdataAndDisplayMsg(don1, GeneralParameters$intstep, parSeries$user$file)
			if(is.null(don1)) return(NULL)

			if(!isTRUE(all.equal(don0.id, don1$id))){
				Insert.Messages.Out("Stations from candidates and references series do not match", format = TRUE)
				return(NULL)
			}
			if(!isTRUE(all.equal(don$dates, don1$dates))){
				Insert.Messages.Out("Dates from candidates and references series do not match", format = TRUE)
				return(NULL)
			}

			# don1[idim] <- lapply(don1[idim], function(x) x[id.ret])
			don1$data <- don1$data[, id.ret, drop = FALSE]

			refrS <- homog.AggregateSeries(don1, GeneralParameters$intstep, GeneralParameters$aggr)
			testS <- homog.RefSeriesUser(candS, refrS, parSeries)

			outREFs$msg[id.ret] <- "Test with reference series: ref.series.by.user"

			rm(don1, don0.id)
		}else{
			elv.data <- NULL
			if(parSeries$elv$use)
			{
				if(parSeries$elv$dem)
				{
					readDEM <- TRUE
					if(parSeries$elv$file == ""){
						Insert.Messages.Out("No DEM file found", format = TRUE)
						readDEM <- FALSE
					}

					demInfo <- getNCDFSampleData(parSeries$elv$file)
					if(is.null(demInfo)){
						Insert.Messages.Out("Unable to read DEM data", format = TRUE)
						readDEM <- FALSE
					}

					if(readDEM){
						jfile <- getIndex.AllOpenFiles(parSeries$elv$file)
						ncdata <- .cdtData$OpenFiles$Data[[jfile]][[2]]
						elv.data <- ncdata$z

						ijx <- grid2pointINDEX(don[c('lon', 'lat')], list(lon = ncdata$x, lat = ncdata$y))
						elv.data <- elv.data[ijx]
						rm(ncdata, ijx)
					}
				}else{
					if(is.null(don$elv))
						Insert.Messages.Out("No elevation data in station data", format = TRUE)
					else
						elv.data <- don$elv
				}

				if(is.null(elv.data))
					Insert.Messages.Out("Neighbor stations will be selected without considering elevation data", format = TRUE)
			}

			###################

			voisin <- lapply(seq_along(don$id), function(stn){
				istn <- seq_along(don$lon)
				crd0 <- cbind(don$lon[stn], don$lat[stn])
				crds <- do.call(cbind, don[c('lon', 'lat')])
				dist <- as.numeric(fields::rdist.earth(crd0, crds, miles = FALSE))
				io <- order(dist)
				istn <- istn[io]
				dist <- dist[io]
				idst <- dist <= parSeries$voisin$dist
				idst[istn == stn] <- FALSE
				istn <- istn[idst]
				dist <- dist[idst]
				if(length(istn) < parSeries$voisin$min) return(NULL)

				if(!is.null(elv.data)){
					elv <- elv.data[istn]
					elv.h <- elv.data[stn] + c(-1, 1) * (parSeries$voisin$elv / 2)
					ielv <- elv >= elv.h[1] & elv <= elv.h[2]
					if(!any(ielv)) return(NULL)
					istn <- istn[ielv]
					dist <- dist[ielv]
				}

				list(id = don$id[stn], stn = c(stn, istn), dist = c(0, dist))
			})

			inull <- sapply(voisin, is.null)
			if(all(inull)){
				Insert.Messages.Out("Unable to create references series, no neighbors stations", format = TRUE)
				return(NULL)
			}

			outREFs$voisin <- voisin

			###################

			voisin1 <- lapply(voisin, function(stn){
				if(is.null(stn))
					return(list(refs = NULL, msg = "Test without reference series: no.voisin"))
				if(length(stn$stn[-1]) < parSeries$voisin$min)
					return(list(refs = NULL, msg = "Test without reference series: no.voisin"))

				x0 <- candS$don3$data[, stn$stn[1]]
				Y <- candS$don3$data[, stn$stn[-1], drop = FALSE]

				ix <- rowSums(!is.na(Y)) >= parSeries$voisin$min
				if(!any(ix))
					return(list(refs = NULL, msg = "Test without reference series: no.voisin"))

				if(sum(ix)/12 < parStats$min.year)
					return(list(refs = NULL, msg = "Test without reference series: few.data.refs"))

				if(parSeries$weight == 1){
					rho <- as.vector(cor(x0, Y, use = "pairwise.complete.obs"))
					if(all(is.na(rho)))
						return(list(refs = NULL, msg = "Test without reference series: no.voisin.miss.rho"))
					irh <- !is.na(rho) & rho >= parSeries$voisin$rho
					if(length(which(irh)) < parSeries$voisin$min)
						return(list(refs = NULL, msg = "Test without reference series: few.data.refs.rho"))

					rho <- rho[irh]
					Y <- Y[, irh, drop = FALSE]
					stn$stn <- c(stn$stn[1], stn$stn[-1][irh])
					stn$dist <- c(stn$dist[1], stn$dist[-1][irh])

					rho2 <- rho * rho
					lambda0 <- rho2
					lambda <- rho2 / sum(rho2)
				}

				if(parSeries$weight == 2){
					inv.dist2 <- 1 / (stn$dist[-1] * stn$dist[-1])
					lambda0 <- inv.dist2
					lambda <- inv.dist2 / sum(inv.dist2)
				}

				if(parSeries$weight == 3){
					vcov <- t(cov(x0, Y, use = "pairwise.complete.obs"))
					icv <- as.vector(vcov)
					if(all(is.na(icv)))
						return(list(refs = NULL, msg = "Test without reference series: no.voisin.miss.cov"))
					icv <- !is.na(icv)
					if(length(which(icv)) < parSeries$voisin$min)
						return(list(refs = NULL, msg = "Test without reference series: few.data.refs.cov"))

					vcov <- vcov[icv, , drop = FALSE]
					Y <- Y[, icv, drop = FALSE]
					stn$stn <- c(stn$stn[1], stn$stn[-1][icv])
					stn$dist <- c(stn$dist[1], stn$dist[-1][icv])

					ones <- matrix(1, ncol = 1, nrow = ncol(Y))
					covmat <- cov(Y, use = "pairwise.complete.obs")
					detcov <- det(covmat)
					if(is.na(detcov))
						return(list(refs = NULL, msg = "Test without reference series: few.data.refs.cov"))

					if(round(detcov, 10) == 0){
						rho <- as.vector(cor(x0, Y, use = "pairwise.complete.obs"))
						rho2 <- rho * rho
						lambda0 <- rho2
						lambda <- rho2 / sum(rho2)
					}else{
						invcov <- solve(covmat)
						X <- t(ones) %*% (invcov %*% vcov)
						X <- vcov + ones %*% ((1 - X) / X)
						lamb <- as.vector(invcov %*% X)
						lamb2 <- lamb * lamb
						lambda0 <- lamb2
						lambda <- lamb2 / sum(lamb2)
					}
				}

				list(refs = c(stn, list(lambda = lambda, lambda0 = lambda0)), msg = NULL)
			})

			refr.vois <- lapply(voisin1, '[[', "refs")
			msg <- lapply(voisin1, '[[', "msg")
			ref.null <- sapply(refr.vois, is.null)
			msg[!ref.null] <- "Test with reference series: with.reference.series"
			outREFs$msg[id.ret] <- msg
			outREFs$voisin1 <- refr.vois
			rm(voisin, voisin1)

			###################

			if(all(ref.null)){
				testS <- lapply(candS, homog.TestSeries.noRef, pars = parSeries)
			}else{
				if(!any(ref.null)){
					testS <- lapply(candS, homog.TestSeries.withRef, pars = parSeries, voisin = refr.vois)
				}else{
					testS0 <- lapply(candS, homog.TestSeries.noRef, pars = parSeries, id = ref.null)
					testS1 <- lapply(candS, homog.TestSeries.withRef, pars = parSeries, voisin = refr.vois, id = !ref.null)
					testS <- lapply(seq_along(candS), function(j){
						x <- candS[[j]]
						if(is.null(x)) return(x)
						x$data[, ref.null] <- testS0[[j]]$data[, ref.null, drop = FALSE]
						x$data[, !ref.null] <- testS1[[j]]$data[, !ref.null, drop = FALSE]
						return(x)
					})
					names(testS) <- names(candS)
					rm(testS0, testS1); gc()
				}
			}
		}
	}else{
		testS <- lapply(candS, homog.TestSeries.noRef, pars = parSeries)
		outREFs$msg[id.ret] <- "Test without reference series: no.ref.series"
	}

	###################

	frac.data.mon <- available.data.fraction(testS$don3$data)
	id.ret1 <- (frac.data.mon$length / 12) >= parStats$min.year & frac.data.mon$frac >= parStats$min.frac
	if(!any(id.ret1)){
		Insert.Messages.Out("No enough data on test series to perform test for all stations", format = TRUE)
		return(NULL)
	}

	if(!all(id.ret1)){
		don0.id <- don$id
		don[idim] <- lapply(don[idim], function(x) x[id.ret1])
		don$data <- don$data[, id.ret1, drop = FALSE]
		don <- don[c(idim, 'dates', 'data')]

		candS <- lapply(candS, function(dat){
			if(is.null(dat)) return(NULL)
			dat$data <- dat$data[, id.ret1, drop = FALSE]
			dat
		})

		testS <- lapply(testS, function(dat){
			if(is.null(dat)) return(NULL)
			dat$data <- dat$data[, id.ret1, drop = FALSE]
			dat
		})

		outREFs$stn.id <- don$id
		outREFs$msg[id.ret][!id.ret1] <- "No Test: no.enough.data.test.series"
		outREFs$voisin[!id.ret1] <- NULL
		outREFs$voisin1[!id.ret1] <- NULL
		outREFs$exclude <- c(outREFs$exclude, don0.id[!id.ret1])
	}

	###################

	cost.func <- switch(parStats$mthd,
				"SNHT" = homog.SNHT.Test,
				"Pettitt" = homog.PettittRank.Test,
				"CUSUM" = homog.CUSUM.Simple,
				"CUSUMtr" = homog.CUSUM.Trend)
	h <- if(parStats$crop) parStats$h else NULL

	out.hom <- lapply(testS, function(dat){
		if(is.null(dat)) return(NULL)
		factm <- switch(dat$tstep, "daily" = 30, "pentad" = 6, "dekadal" = 3, "monthly" = 1)
		min.int <- factm * parStats$min.len
		nl <- seq(ncol(dat$data))
		break_points <- lapply(nl, function(j) cdt.multiBreaks.Detection(dat$data[, j], cost.func, min.int, h))
		change_points <- lapply(nl, function(j){
			obj <- break_points[[j]]
			if(is.null(obj)) return(NULL)
			brks <- data.frame(Breakpoints.Date = dat$date[obj$cpt.index],
								Max.Conf.Lev = obj$max.conf.lev,
								Statistics.Test = round(obj$statistics, 4),
								Signif.Test = "",
								Breakpoints.Index = obj$cpt.index,
								stringsAsFactors = FALSE)
			brks <- brks[order(brks$Breakpoints.Index), , drop = FALSE]

			cpt <- cdt.changepoints(dat$data[, j], obj, parStats)
			if(!is.null(cpt)){
				isignf <- brks$Breakpoints.Index %in% cpt$res$cpt.index
				brks$Signif.Test[isignf] <- "****"
				isup <- brks$Max.Conf.Lev >= parStats$conf.lev
				if(any(isup))
					brks <- brks[isup, , drop = FALSE]
				else
					brks <- NULL
			}

			list(out = brks, cpt = cpt)
		})

		out.cpt <- lapply(change_points, "[[", "out")

		cpt <- lapply(change_points, "[[", "cpt")
		cpt <- lapply(seq_along(break_points), function(j){
			brks <- break_points[[j]]
			if(is.null(brks)) return(NULL)
			list(breaks = brks, cpt = cpt[[j]]$cpt)
		})

		names(out.cpt) <- don$id
		names(cpt) <- don$id
		list(cpt = out.cpt, stat.test = cpt)
	})

	cpt.table <- lapply(out.hom, function(x) x$cpt)
	cpt.stats <- lapply(out.hom, function(x) x$stat.test)

	# result of Homog
	###################

	output <- list(params = GeneralParameters, info = don.info, series = outREFs, data = don[idim])

	.cdtData$EnvData$candS <- candS
	.cdtData$EnvData$testS <- testS
	.cdtData$EnvData$output <- output
	.cdtData$EnvData$PathData <- outdir
	.cdtData$EnvData$cpt.table <- cpt.table
	.cdtData$EnvData$cpt.table0 <- cpt.table
	.cdtData$EnvData$cpt.stats <- cpt.stats

	###################
	file.index <- file.path(outdir, "HomogeneityTest.rds")
	file.candS <- file.path(dataCDTdir, "CandidateSeries.rds")
	file.testS <- file.path(dataCDTdir, "TestSeries.rds")
	file.don <- file.path(dataCDTdir, "StationData.rds")
	file.stats <- file.path(dataCDTdir, "BreaksPointsStats.rds")
	file.table0 <- file.path(dataCDTdir, "BreaksPointsTable0.rds")
	file.table <- file.path(dataCDTdir, "BreaksPointsTable.rds")

	saveRDS(output, file.index)
	saveRDS(candS, file.candS)
	saveRDS(testS, file.testS)
	saveRDS(don, file.don)
	saveRDS(cpt.table, file.table)
	saveRDS(cpt.stats, file.stats)
	file.copy(file.table, file.table0, overwrite = TRUE)

	return(0)
}
