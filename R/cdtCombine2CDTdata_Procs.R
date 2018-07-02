
merge2CDTdata <- function(GalParams){
	compareDonneCDT <- function(idc, idate, cdon1, cdon2){
		ix <- sapply(seq_along(idc), function(j) isEqual(cdon1[, j], cdon2[, j]))
		if(any(!ix)){
			idif <- rep(FALSE, nrow(cdon1))
			for(j in which(!ix)) idif <- idif | !isEquals(cdon1[, j], cdon2[, j])
			dif.don1 <- cbind(idate[idif],cdon1[idif, !ix, drop = FALSE])
			dif.don2 <- cbind(idate[idif],cdon2[idif, !ix, drop = FALSE])
			names(dif.don1) <- names(dif.don2) <- c('Date', idc[!ix])
			stn.diff.values <- list(data1 = dif.don1,  data2 = dif.don2)
			for(j in which(!ix))
				cdon1[is.na(cdon1[, j]) & !is.na(cdon2[, j]), j] <- cdon2[is.na(cdon1[, j]) & !is.na(cdon2[, j]), j]
		}else{
			cdon1 <- cdon1
			stn.diff.values <- NULL
		}
		return(list(cdon1, stn.diff.values))
	}

	compareCoordsCDT <- function(idc, chead1, chead2){
		ix <- sapply(seq_along(idc), function(j) isEqual(chead1[, j], chead2[, j]))
		if(any(!ix)){
			capt0 <- if(nrow(chead1) == 2) c('STN.ID', 'LON', 'LAT') else c('STN.ID', 'LON', 'LAT', 'ELV')
			crd1 <- cbind(capt0, rbind(idc[!ix], chead1[, !ix, drop = FALSE]))
			crd2 <- cbind(capt0, rbind(idc[!ix], chead2[, !ix, drop = FALSE]))
			names(crd1) <- names(crd2) <- NULL
			stn.crd.diff <- list(Coord1 = crd1, Coord2 = crd2)
			for(j in which(!ix))
				chead1[is.na(chead1[, j]) & !is.na(chead2[, j]), j] <- chead2[is.na(chead1[, j]) & !is.na(chead2[, j]), j]
		}else{
			chead1 <- chead1
			stn.crd.diff <- NULL
		}
		return(list(chead1, stn.crd.diff))
	}

	##########################

	file1 <- GalParams$file1
	file2 <- GalParams$file2
	File2Save <- GalParams$file2save

	donne1 <- getStnOpenData(file1)
	if(is.null(donne1)) return(NULL)
	donneInfo <- getStnOpenDataInfo(file1)
	if(is.null(donneInfo)) return(NULL)
	donne2 <- getStnOpenData(file2)
	if(is.null(donne2)) return(NULL)

	donne <- lapply(list(donne1, donne2), function(x){
		ix <- grepl('[[:digit:]]', x[, 1])
		seph <- rle(ix)
		ipos <- which(!seph$values & seph$lengths >= 3 & seph$lengths <= 4)
		if(length(ipos) == 0){
			Insert.Messages.Out(GalParams[['message']][['4']], format = TRUE)
			return(NULL)
		}
		if(ipos[1] != 1){
			Insert.Messages.Out(GalParams[['message']][['4']], format = TRUE)
			return(NULL)
		}
		header <- x[1:seph$lengths[ipos[1]], , drop = FALSE]
		x <- x[ix, , drop = FALSE]
		capt <- as.character(header[, 1])
		id.stn <- as.character(header[1, -1])
		coords <- header[-1, -1]
		daty <- x[, 1]
		don <- x[, -1, drop = FALSE]
		list(capt = capt, id = id.stn, crd = coords, date = daty, data = don)
	})

	ndate1 <- nchar(donne[[1]]$date[1])
	ndate2 <- nchar(donne[[2]]$date[1])
	if(ndate1 != ndate2){
		Insert.Messages.Out(GalParams[['message']][['5']], format = TRUE)
		return(NULL)
	}

	ldate1 <- donne[[1]]$date %in% donne[[2]]$date
	ldate2 <- donne[[2]]$date %in% donne[[1]]$date
	if(any(ldate1)){
		date1 <- donne[[1]]$date[!ldate1]
		date2 <- donne[[2]]$date[!ldate2]
		donne1 <- donne[[1]]$data[!ldate1, , drop = FALSE]
		donne2 <- donne[[2]]$data[!ldate2, , drop = FALSE]

		idate <- donne[[1]]$date[ldate1]
		idon1 <- donne[[1]]$data[ldate1, , drop = FALSE]
		idon2 <- donne[[2]]$data[ldate2, , drop = FALSE]

		same.id1 <- donne[[1]]$id %in% donne[[2]]$id
		same.id2 <- donne[[2]]$id %in% donne[[1]]$id
		if(any(same.id1)){
			if(all(same.id1) & all(same.id2)){
				retComp <- compareDonneCDT(donne[[1]]$id, idate, idon1,
							idon2[, match(donne[[1]]$id, donne[[2]]$id), drop = FALSE])
				id.com <- donne[[1]]$id
				don.com <- retComp[[1]]
			}else if(all(same.id1) & !all(same.id2)){
				retComp <- compareDonneCDT(donne[[1]]$id, idate, idon1,
							idon2[, match(donne[[1]]$id, donne[[2]]$id), drop = FALSE])
				idd <- donne[[2]]$id[!same.id2]
				ddon <- idon2[, !same.id2, drop = FALSE]
				id.com <- c(donne[[1]]$id, idd)
				don.com <- cbind(retComp[[1]], ddon)
			}else if(!all(same.id1) & all(same.id2)){
				retComp <- compareDonneCDT(donne[[2]]$id, idate, idon2,
							idon1[, match(donne[[2]]$id, donne[[1]]$id), drop = FALSE])
				idd <- donne[[1]]$id[!same.id1]
				ddon <- idon1[, !same.id1, drop = FALSE]
				id.com <- c(donne[[2]]$id, idd)
				don.com <- cbind(retComp[[1]], ddon)
			}else{
				retComp <- compareDonneCDT(donne[[1]]$id[same.id1], idate,
							idon1[, match(donne[[1]]$id[same.id1], donne[[1]]$id), drop = FALSE],
							idon2[, match(donne[[1]]$id[same.id1], donne[[2]]$id), drop = FALSE])
				idd <- c(donne[[1]]$id[!same.id1], donne[[2]]$id[!same.id2])
				ddon <- cbind(idon1[, !same.id1, drop = FALSE], idon2[, !same.id2, drop = FALSE])
				id.com <- c(donne[[1]]$id[same.id1], idd)
				don.com <- cbind(retComp[[1]], ddon)
			}
			stn.diff.values <- retComp[[2]]
		}else{
			id.com <- c(donne[[1]]$id, donne[[2]]$id)
			don.com <- cbind(idon1, idon2)
			stn.diff.values <- NULL
		}

		XX1 <- data.frame(date1, donne1)
		names(XX1) <- c('Daty', donne[[1]]$id)
		XX2 <- data.frame(idate, don.com)
		names(XX2) <- c('Daty', id.com)
		XX3 <- data.frame(date2, donne2)
		names(XX3) <- c('Daty', donne[[2]]$id)
		XX <- merge(merge(XX1, XX2, all = TRUE), XX3, all = TRUE)
		rm(date1, date2, idon1, idon2, XX1, XX2, XX3)
	}else{
		stn.diff.values <- NULL
		XX1 <- data.frame(donne[[1]]$date, donne[[1]]$data)
		names(XX1) <- c('Daty', donne[[1]]$id)
		XX2 <- data.frame(donne[[2]]$date, donne[[2]]$data)
		names(XX2) <- c('Daty', donne[[2]]$id)
		XX <- merge(XX1, XX2, all = TRUE)
		rm(XX1, XX2)
	}
	xxo <- order(as.character(XX$Daty))
	XX <- XX[xxo, , drop = FALSE]
	xxdaty <- as.character(XX$Daty)

	######################

	id1 <- donne[[1]]$id
	id2 <- donne[[2]]$id
	im <- min(c(nrow(donne[[1]]$crd), nrow(donne[[2]]$crd)))
	head1 <- donne[[1]]$crd[1:im, ]
	head2 <- donne[[2]]$crd[1:im, ]

	head1.id <- id1 %in% id2
	head2.id <- id2 %in% id1
	if(any(head1.id)){
		if(all(head1.id) & all(head2.id)){
			retComp <- compareCoordsCDT(id1, head1, head2[, match(id1, id2), drop = FALSE])
			id.com <- id1
			head.com <- retComp[[1]]
		}else if(all(head1.id) & !all(head2.id)){
			retComp <- compareCoordsCDT(id1, head1, head2[, match(id1, id2), drop = FALSE])
			idd <- id2[!head2.id]
			dhead <- head2[, !head2.id, drop = FALSE]
			id.com <- c(id1, idd)
			head.com <- cbind(retComp[[1]], dhead)
		}else if(!all(head1.id) & all(head2.id)){
			retComp <- compareCoordsCDT(id2, head2, head1[, match(id2, id1), drop = FALSE])
			idd <- id1[!head1.id]
			dhead <- head1[, !head1.id, drop = FALSE]
			id.com <- c(id2, idd)
			head.com <- cbind(retComp[[1]], dhead)
		}else{
			retComp <- compareCoordsCDT(id1[head1.id], 
						head1[, match(id1[head1.id], id1), drop = FALSE],
						head2[, match(id1[head1.id], id2), drop = FALSE])
			idd <- c(id1[!head1.id], id2[!head2.id])
			dhead <- cbind(head1[, !head1.id, drop = FALSE], head2[, !head2.id, drop = FALSE])
			id.com <- c(id1[head1.id], idd)
			head.com <- cbind(retComp[[1]], dhead)
		}
		stn.crd.diff <- retComp[[2]]
	}else{
		id.com <- c(id1, id2)
		head.com <- cbind(head1, head2)
		stn.crd.diff <- NULL
	}

	xmx <- max(c(nrow(donne[[1]]$crd), nrow(donne[[2]]$crd)))
	if(xmx == nrow(head.com)){
		headF <- rbind(id.com, head.com)
		headF <- data.frame(donne[[1]]$capt, headF, row.names = NULL)
	}else{
		imx <- which.max(c(nrow(donne[[1]]$crd), nrow(donne[[2]]$crd)))
		xvar <- donne[[imx]]$crd[xmx, , drop = FALSE]
		head.com[xmx, match(donne[[imx]]$id, id.com)] <- donne[[imx]]$crd[xmx, , drop = FALSE]
		headF <- rbind(id.com, head.com)
		headF <- data.frame(donne[[imx]]$capt, headF, row.names = NULL)
	}
	names(headF) <- c('Daty', id.com)

	donne <- merge(headF, XX, all = TRUE, sort = FALSE)
	donne[is.na(donne)] <- donneInfo[[3]]$miss.val
	writeFiles(donne, File2Save)
	rm(donne1, donne2, ldate1, ldate2, donne, XX, headF, head1, head2)

	########################

	outlist <- list()
	if(!is.null(stn.diff.values)){
		outlist <- c(outlist, list(GalParams[['message']][['6']], stn.diff.values))
	}
	if(!is.null(stn.crd.diff)){
		outlist <- c(outlist, list(GalParams[['message']][['7']], stn.crd.diff))
	}

	if(length(outlist) > 0){
		containertab <- Display_Output_Console_Tab(outlist, title = GalParams[['message']][['6']])
		ntab <- update.OpenTabs('ctxt', containertab)
		tkselect(.cdtEnv$tcl$main$tknotes, ntab)
	}
	return(0)
}
