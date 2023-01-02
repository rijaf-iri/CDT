
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
        if(length(ncsample) > 1){
            SP <- lapply(ncsample, function(x) defSpatialPixels(x[c('lon', 'lat')]))
            lSP <- sapply(SP[-1], function(x) is.diffSpatialPixelsObj(SP[[1]], x, tol = 1e-04))
            if(any(lSP)){
                Insert.Messages.Out(GalParams[['message']][['16']], TRUE, "e")
                return(NULL)
            }
        }

        ## check dates overlap
        ncInfo <- lapply(seq_along(GalParams$inputs), function(i){
            ncIn <- GalParams$inputs[[paste0('file', i)]]
            ncFiles <- list.files(ncIn$dir, gsub("%s", "[[:digit:]]+", ncIn$format))
            if(length(ncFiles) == 0) return(NULL)

            txt <- strsplit(ncIn$format, "%s")[[1]]
            txt <- txt[txt != ""]
            daty <- gsub(txt[1], "", ncFiles)
            if(length(txt) > 1){
                for(j in 2:length(txt))
                    daty <- gsub(txt[j], "", daty)
            }

            list(dates = daty, files = ncFiles)
        })

        inull <- sapply(ncInfo, is.null)
        if(any(inull)){
            msg <- paste0('X', which(inull))
            msg <- paste(GalParams[['message']][['17']], "; variables :", msg)
            Insert.Messages.Out(msg, TRUE, "e")
            return(NULL)
        }

        ncDates <- lapply(ncInfo, '[[', 'dates')
        ncFiles <- lapply(ncInfo, '[[', 'files')

        if(length(ncDates) > 1){
            daty <- Reduce(intersect, ncDates)
            if(length(daty) == 0){
                Insert.Messages.Out(GalParams[['message']][['11']], TRUE, "e")
                return(NULL)
            }
            ncFiles <- lapply(seq_along(ncFiles), function(i){
                ix <- match(daty, ncDates[[i]])
                ncFiles[[i]][ix]
            })
        }

        #########
        outNCDIR <- file.path(GalParams$output, "dataOperation_Output")
        dir.create(outNCDIR, showWarnings = FALSE, recursive = TRUE)

        dx <- ncdf4::ncdim_def("Lon", "degreeE", ncsample[[1]]$lon)
        dy <- ncdf4::ncdim_def("Lat", "degreeN", ncsample[[1]]$lat)
        xy.dim <- list(dx, dy)

        grdNC <- ncdf4::ncvar_def(GalParams$varinfo$name, GalParams$varinfo$units,
                                  xy.dim, GalParams$varinfo$missval,
                                  longname = GalParams$varinfo$longname,
                                  prec = GalParams$varinfo$prec, compression = 6)

        #########

        parsL <- doparallel.cond(length(ncFiles[[1]]) >= 180)

        ret <- cdt.foreach(seq_along(ncFiles[[1]]), parsL, GUI = TRUE,
                           progress = TRUE, FUN = function(i)
        {
            donne <- lapply(seq_along(ncFiles), function(j){
                ncfile <- file.path(GalParams$inputs[[paste0('file', j)]]$dir, ncFiles[[j]][i])
                nc <- try(ncdf4::nc_open(ncfile), silent = TRUE)
                if(inherits(nc, "try-error")) return(NULL)
                val <- ncdf4::ncvar_get(nc, varid = ncsample[[j]]$varinfo$name)
                ncdf4::nc_close(nc)
                val <- transposeNCDFData(val, ncsample[[j]])
                val
            })

            inull <- sapply(donne, is.null)
            if(any(inull)) return(-1)

            names(donne) <- formulaVar

            txtFormula <- GalParams$formula
            for(j in seq_along(formulaVar))
                txtFormula <- gsub(paste0("X", j), paste0("donne[['X", j, "']]"), txtFormula)

            evalOut <- NULL
            evalFormula <- paste("evalOut =", txtFormula)
            res <- try(eval(parse(text = evalFormula)), silent = TRUE)
            if(inherits(res, "try-error")) return(-2)
            if(is.null(evalOut)) return(-2)

            evalOut[is.na(evalOut) | is.nan(evalOut) | is.infinite(evalOut)] <- GalParams$varinfo$missval

            outfile <- file.path(outNCDIR, ncFiles[[1]][i])
            nc <- ncdf4::nc_create(outfile, grdNC)
            ncdf4::ncvar_put(nc, grdNC, evalOut)
            ncdf4::nc_close(nc)

            return(0)
        })

        noNC <- sapply(ret, function(x) x == -1)
        out1 <- NULL
        if(any(noNC)){
            out <- do.call(cbind, lapply(ncFiles, function(x) x[noNC]))
            out <- apply(out, 1, paste, collapse = " - ")
            out <- paste0(out, collapse = '\n')
            out1 <- paste0(GalParams[['message']][['18']], '\n\n', out)
        }

        noEval <- sapply(ret, function(x) x == -2)
        out2 <- NULL
        if(any(noNC)){
            out <- do.call(cbind, lapply(ncFiles, function(x) x[noEval]))
            out <- apply(out, 1, paste, collapse = " - ")
            out <- paste0(out, collapse = '\n')
            out2 <- paste0(GalParams[['message']][['12']], '\n\n', out)
        }

        if(!is.null(out1) | !is.null(out2)){
            out <- paste(out1, '\n\n', out2)
            tab_title <- "Unprocessed-Data"
            containertab <- Display_Output_Console_Tab(out, tab_title, cat)
            ntab <- update.OpenTabs('ctxt', containertab)
            tkselect(.cdtEnv$tcl$main$tknotes, ntab)
        }
    }

    if(GalParams$datatype == "cdtdataset"){
        ## to do
    }

    return(0)
}
