
filterCDTdata <- function(GalParams){
	if(GalParams$filein == ""){
		Insert.Messages.Out(GalParams[['message']][['4']], format = TRUE)
		return(NULL)
	}

	donne <- getStnOpenData(GalParams$filein)
	if(is.null(donne)) return(NULL)
	donneInfo <- getStnOpenDataInfo(GalParams$filein)
	if(is.null(donneInfo)) return(NULL)

	ix <- grepl('[[:digit:]]', donne[, 1])
	seph <- rle(ix)
	ipos <- which(!seph$values & seph$lengths >= 3 & seph$lengths <= 4)
	if(length(ipos) == 0){
		Insert.Messages.Out(GalParams[['message']][['5']], format = TRUE)
		return(NULL)
	}
	if(ipos[1] != 1){
		Insert.Messages.Out(GalParams[['message']][['5']], format = TRUE)
		return(NULL)
	}
	header <- as.matrix(donne[1:seph$lengths[ipos[1]], , drop = FALSE])
	daty <- donne[ix, 1]
	donne <- as.matrix(donne[ix, -1, drop = FALSE])
	len <- nrow(donne)
	pnadonne <- (len - colSums(is.na(donne)))/len

	# opfilter <- match.fun(GalParams$opfilter)
	opfilter <- get(GalParams$opfilter, mode = "function")
	istn <- as.logical(opfilter(pnadonne, GalParams$valfilter / 100))

	donne <- donne[, istn, drop = FALSE]
	header <- header[, c(TRUE, istn), drop = FALSE]
	donne <- rbind(header, cbind(daty, donne))
	donne[is.na(donne)] <- donneInfo[[3]]$miss.val

	writeFiles(donne, GalParams$file2save)

	return(0)
}


