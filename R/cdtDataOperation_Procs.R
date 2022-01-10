
dataOperation_Execute <- function(){
    GalParams <- .cdtData$GalParams
    Insert.Messages.Out(GalParams[['message']][['1']], TRUE, "i")

    posVar <- gregexpr('X[0-9]+', GalParams$formula)
    formulaVar <- regmatches(GalParams$formula, posVar)
    formulaVar <- sort(unique(formulaVar[[1]]))

    if(GalParams$datatype == "cdtstation"){
        fileSTNs <- lapply(seq_along(GalParams$inputs), function(jj){
            GalParams$inputs[[paste0('file', jj)]]$dir
        })

        donne <- lapply(fileSTNs, getStnOpenData)

        inull <- sapply(donne, is.null)
        if(any(inull)){
            msg <- paste0(unlist(fileSTNs[inull]), collapse = ", ")
            msg <- paste(GalParams[['message']][['9-1']], ":", msg)
            Insert.Messages.Out(msg, TRUE, "e")
            return(NULL)
        }

        donne <- lapply(donne, splitCDTData0)
        inull <- sapply(donne, is.null)
        if(any(inull)){
            msg <- paste0(unlist(fileSTNs[inull]), collapse = ", ")
            msg <- paste(GalParams[['message']][['9-2']], ":", msg)
            Insert.Messages.Out(msg, TRUE, "e")
            return(NULL)
        }

        donInfo <- getStnOpenDataInfo(fileSTNs[[1]])

        donID <- lapply(donne, "[[", "id")
        if(length(donID) > 1){
            nl <- length(donID[[1]])
            eqls <- sapply(donID[-1], function(x){
                if(length(x) != nl) return(1)
                if(!all(donID[[1]] == x)) return(1)
                return(0)
            })

            if(any(eqls == 1)){
                Insert.Messages.Out(GalParams[['message']][['10']], TRUE, "e")
                return(NULL)
            }
        }

        donDates <- lapply(donne, "[[", "dates")
        daty <- sort(unique(do.call(c, donDates)))
        intD <- lapply(donDates, function(x){
            it <- match(daty, x)
            if(all(is.na(it))) return(NULL)
            it
        })

        inull <- sapply(intD, is.null)
        if(any(inull)){
            Insert.Messages.Out(GalParams[['message']][['11']], TRUE, "e")
            return(NULL)
        }

        donne <- lapply(seq_along(donne), function(j){
            x <- donne[[j]]
            x$dates <- daty
            x$data <- x$data[intD[[j]], , drop = FALSE]
            x
        })

        outSTN <- donne[[1]]
        class(outSTN) <- append(class(outSTN), "cdtstationdata")

        donne <- lapply(donne, '[[', 'data')
        names(donne) <- formulaVar

        txtFormula <- GalParams$formula
        for(j in seq_along(formulaVar))
            txtFormula <- gsub(paste0("X", j), paste0("donne[['X", j, "']]"), txtFormula)

        evalOut <- NULL
        evalFormula <- paste("evalOut =", txtFormula)
        ret <- try(eval(parse(text = evalFormula)), silent = TRUE)

        if(inherits(ret, "try-error")){
            Insert.Messages.Out(GalParams[['message']][['12']], TRUE, "e")
            return(NULL)
        }
        if(is.null(evalOut)){
            Insert.Messages.Out(GalParams[['message']][['12']], TRUE, "e")
            return(NULL)
        }

        outSTN$data <- evalOut
        writeCDTStationData(outSTN, GalParams$output, donInfo[[3]]$miss.val)
    }

    if(GalParams$datatype == "cdtnetcdf"){
        ncFiles <- lapply(seq_along(GalParams$inputs), function(jj){
            GalParams$inputs[[paste0('file', jj)]]$sample
        })

        ncsample <- lapply(ncFiles, getNCDFSampleData)
        inull <- sapply(ncsample, is.null)
        if(any(inull)){
            msg <- paste0(unlist(ncFiles[inull]), collapse = ", ")
            msg <- paste(GalParams[['message']][['15']], ":", msg)
            Insert.Messages.Out(msg, TRUE, "e")
            return(NULL)
        }

        ## check grid match
        ## check dates overlap
        ## create new folder
    }

    if(GalParams$datatype == "cdtdataset"){

    }

    return(0)
}
