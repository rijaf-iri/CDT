
selectCDTdata <- function(){
    GalParams <- .cdtData$GalParams
    if(GalParams$filein == ""){
        Insert.Messages.Out(GalParams[['message']][['4']], format = TRUE)
        return(NULL)
    }
    if(GalParams$filein1 == ""){
        Insert.Messages.Out(GalParams[['message']][['4']], format = TRUE)
        return(NULL)
    }

    Insert.Messages.Out(GalParams[['message']][['1']])

    ####
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
    ID <- as.character(header[1, -1])

    ####
    donne1 <- getStnOpenData(GalParams$filein1)
    if(is.null(donne1)) return(NULL)
    donneInfo1 <- getStnOpenDataInfo(GalParams$filein1)
    if(is.null(donneInfo1)) return(NULL)
    ix <- grepl('[[:digit:]]', donne1[, 1])
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
    header1 <- as.matrix(donne1[1:seph$lengths[ipos[1]], , drop = FALSE])
    daty1 <- donne1[ix, 1]
    donne1 <- as.matrix(donne1[ix, -1, drop = FALSE])
    ID1 <- as.character(header1[1, -1])

    idaty <- daty %in% daty1
    if(!any(idaty)){
        Insert.Messages.Out(GalParams[['message']][['9']], format = TRUE)
        return(NULL)
    }

    istn <- ID %in% ID1
    if(!any(istn)){
        Insert.Messages.Out(GalParams[['message']][['10']], format = TRUE)
        return(NULL)
    }

    header <- header[, c(TRUE, !istn), drop = FALSE]
    donne <- donne[, !istn, drop = FALSE]

    len <- nrow(donne)
    pnadonne <- (len - colSums(is.na(donne)))/len

    opfilter <- get(GalParams$opfilter, mode = "function")
    istn <- as.logical(opfilter(pnadonne, GalParams$valfilter / 100))

    if(!any(istn)){
        msg <- paste(GalParams[['message']][['11']], GalParams$opfilter, GalParams$valfilter, "%")
        Insert.Messages.Out(msg, format = TRUE)
        return(NULL)
    }

    donne <- donne[, istn, drop = FALSE]
    header <- header[, c(TRUE, istn), drop = FALSE]
    donne <- rbind(header, cbind(daty, donne))
    donne[is.na(donne)] <- donneInfo[[3]]$miss.val

    writeFiles(donne, GalParams$file2save)

    return(0)
}
