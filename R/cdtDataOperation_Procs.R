
dataOperation_Execute <- function(){
    GalParams <- .cdtData$GalParams
    Insert.Messages.Out(GalParams[['message']][['1']], TRUE, "i")

    posVar <- gregexpr('X[0-9]+', GalParams$formula)
    formulaVar <- regmatches(GalParams$formula, posVar)
    formulaVar <- sort(unique(formulaVar[[1]]))

    const <- do.call(c, GalParams$constant)

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

        donne <- lapply(seq_along(donne), function(j){
            if(const[j])
                splitCDTData1(donne[[j]])
            else
                splitCDTData0(donne[[j]])
        })

        inull <- sapply(donne, is.null)
        if(any(inull)){
            msg <- paste0(unlist(fileSTNs[inull]), collapse = ", ")
            msg <- paste(GalParams[['message']][['9-2']], ":", msg)
            Insert.Messages.Out(msg, TRUE, "e")
            return(NULL)
        }

        izeros <- sapply(donne[const], function(x) nrow(x$data) == 0)
        if(any(izeros)){
            Insert.Messages.Out(GalParams[['message']][['24']], TRUE, "e")
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

        donDates <- lapply(donne[!const], "[[", "dates")
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

        donne[!const] <- lapply(seq_along(donne[!const]), function(j){
            x <- donne[!const][[j]]
            x$dates <- daty
            x$data <- x$data[intD[[j]], , drop = FALSE]
            x
        })

        ndim <- dim(donne[[1]]$data)
        donne[const] <- lapply(donne[const], function(x){
            tmp <- matrix(x$data[1, , drop = FALSE],
                          nrow = ndim[1], ncol = ndim[2],
                          byrow = TRUE)
            x$data <- tmp
            x
        })

        outSTN <- donne[[1]]
        class(outSTN) <- append(class(outSTN), "cdtstationdata")

        donne <- lapply(donne, '[[', 'data')
        names(donne) <- formulaVar

        ######
        txtFormula <- GalParams$formula

        is_latlon <- grepl('LAT|LON', txtFormula)
        if(is_latlon){
            ndim <- dim(donne[[1]])
            if(grepl('LON', txtFormula)){
                donne[['LON']] <- matrix(outSTN$lon, nrow = ndim[1], ncol = ndim[2], byrow = TRUE)
                txtFormula <- gsub("LON", "donne[['LON']]", txtFormula)
            }
            if(grepl('LAT', txtFormula)){
                donne[['LAT']] <- matrix(outSTN$lat, nrow = ndim[1], ncol = ndim[2], byrow = TRUE)
                txtFormula <- gsub("LAT", "donne[['LAT']]", txtFormula)
            }
        }

        for(j in seq_along(formulaVar))
            txtFormula <- gsub(paste0("X", j), paste0("donne[['X", j, "']]"), txtFormula)

        ######
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
            if(const[jj])
                GalParams$inputs$file3$dir
            else
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
        ncInfo <- lapply(names(GalParams$inputs[!const]), function(n){
            ncIn <- GalParams$inputs[[n]]
            ncFiles <- list.files(ncIn$dir, gsub("%s", "[[:digit:]]+", ncIn$format))
            if(length(ncFiles) == 0) return(NULL)

            daty <- extract_filename_dates.1(ncFiles, ncIn$format)
            # daty <- extract_filename_dates(ncFiles, ncIn$format)

            list(dates = daty, files = ncFiles)
        })

        inull <- sapply(ncInfo, is.null)
        if(any(inull)){
            msg <- paste0('X', which(!const)[which(inull)], collapse = ' - ')
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
            ncDates <- daty
        }else ncDates <- ncDates[[1]]

        #########
        constData <- NULL
        if(any(const)){
            constData <- lapply(names(GalParams$inputs[const]), function(n){
                tmp <- getNcdfOpenData(GalParams$inputs[[n]]$dir)
                tmp[[2]]$z
            })
            names(constData) <- formulaVar[const]
        }

        ######
        txtFormula <- GalParams$formula

        is_latlon <- grepl('LAT|LON', txtFormula)
        if(is_latlon){
            ndim <- sapply(ncsample[[1]][c('lon', 'lat')], length)
            if(grepl('LON', txtFormula)){
                constData[['LON']] <- matrix(ncsample[[1]]$lon, nrow = ndim[1], ncol = ndim[2])
                txtFormula <- gsub("LON", "donne[['LON']]", txtFormula)
            }
            if(grepl('LAT', txtFormula)){
                constData[['LAT']] <- matrix(ncsample[[1]]$lat, nrow = ndim[1], ncol = ndim[2], byrow = TRUE)
                txtFormula <- gsub("LAT", "donne[['LAT']]", txtFormula)
            }
        }

        for(j in seq_along(formulaVar))
            txtFormula <- gsub(paste0("X", j), paste0("donne[['X", j, "']]"), txtFormula)

        #########
        outNCDIR <- file.path(GalParams$output, "dataOperation_Output")
        if(dir.exists(outNCDIR)){
            tmp <- list.files(GalParams$output, "dataOperation\\_Output\\_[0-9]+", include.dirs = TRUE)
            if(length(tmp) == 0){
                tmp <- 1
            }else{
                tmp <- gsub("dataOperation_Output_", "", tmp)
                tmp <- max(as.numeric(tmp)) + 1
            }
            outNCDIR <- file.path(GalParams$output, paste0("dataOperation_Output_", tmp))
        }
        dir.create(outNCDIR, showWarnings = FALSE, recursive = TRUE)
        outNCFRMT <- GalParams$ncoutformat

        dx <- ncdf4::ncdim_def("Lon", "degreeE", ncsample[[1]]$lon)
        dy <- ncdf4::ncdim_def("Lat", "degreeN", ncsample[[1]]$lat)
        xy.dim <- list(dx, dy)

        grdNC <- ncdf4::ncvar_def(GalParams$varinfo$name, GalParams$varinfo$units,
                                  xy.dim, GalParams$varinfo$missval,
                                  longname = GalParams$varinfo$longname,
                                  prec = GalParams$varinfo$prec, compression = 9)

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

            names(donne) <- formulaVar[!const]
            if(!is.null(constData)) donne <- c(donne, constData)

            evalOut <- NULL
            evalFormula <- paste("evalOut =", txtFormula)
            res <- try(eval(parse(text = evalFormula)), silent = TRUE)
            if(inherits(res, "try-error")) return(-2)
            if(is.null(evalOut)) return(-2)

            evalOut[is.na(evalOut) | is.nan(evalOut) | is.infinite(evalOut)] <- GalParams$varinfo$missval

            outfile <- gsub('%S', ncDates[i], outNCFRMT)
            outfile <- file.path(outNCDIR, outfile)
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
        if(any(noEval)){
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
        if(any(const)){
            ncFiles <- lapply(names(GalParams$inputs[const]), function(n){
                    GalParams$inputs[[n]]$dir
            })

            ncsample <- lapply(ncFiles, getNCDFSampleData)
            inull <- sapply(ncsample, is.null)
            if(any(inull)){
                msg <- paste0(unlist(ncFiles[inull]), collapse = ", ")
                msg <- paste(GalParams[['message']][['15']], ":", msg)
                Insert.Messages.Out(msg, TRUE, "e")
                return(NULL)
            }
        }

        fileRDS <- lapply(names(GalParams$inputs[!const]), function(n){
            GalParams$inputs[[n]]$dir
        })

        infoIndex <- lapply(fileRDS, function(ff){
            idx <- try(readRDS(ff), silent = TRUE)
            if(inherits(idx, "try-error")) return(NULL)
            idx
        })
        inull <- sapply(infoIndex, is.null)
        if(any(inull)){
            msg <- paste0(unlist(fileRDS[inull]), collapse = '\n')
            Insert.Messages.Out(GalParams[['message']][['23']], TRUE, "e")
            Insert.Messages.Out(msg, TRUE, "e")
            return(NULL)
        }

        ## check time step
        if(length(infoIndex) > 1){
            tsteps <- sapply(infoIndex, '[[', 'TimeStep')
            if(any(tsteps[1] != tsteps)){
                Insert.Messages.Out(GalParams[['message']][['20']], TRUE, "e")
                return(NULL)
            }
        }

        ## check chunk size
        if(length(infoIndex) > 1){
            chunksize <- sapply(infoIndex, '[[', 'chunksize')
            if(any(chunksize[1] != chunksize)){
                Insert.Messages.Out(GalParams[['message']][['21']], TRUE, "e")
                return(NULL)
            }
        }

        ## check grid match
        if(length(infoIndex) > 1){
            SP <- lapply(infoIndex, function(x) x$coords$mat[c('x', 'y')])
            SP <- lapply(SP, function(x){names(x) <- c('lon', 'lat'); x})
            SP <- lapply(SP, defSpatialPixels)
            lSP <- sapply(SP[-1], function(x) is.diffSpatialPixelsObj(SP[[1]], x, tol = 1e-04))
            if(any(lSP)){
                Insert.Messages.Out(GalParams[['message']][['22']], TRUE, "e")
                return(NULL)
            }
        }

        if(any(const)){
            if(length(infoIndex) == 1){
                SP0 <- infoIndex[[1]]$coords$mat[c('x', 'y')]
                names(SP0) <- c('lon', 'lat')
                SP0 <- defSpatialPixels(SP0)
            }else{
                SP0 <- SP[[1]]
            }
            SP1 <- lapply(ncsample, function(x) defSpatialPixels(x[c('lon', 'lat')]))
            lSP <- sapply(SP1, function(x) is.diffSpatialPixelsObj(SP0, x, tol = 1e-04))
            if(any(lSP)){
                Insert.Messages.Out(GalParams[['message']][['16']], TRUE, "e")
                return(NULL)
            }
        }

        ## check dates overlap
        infoDates <- lapply(infoIndex, function(x) x$dateInfo$date)
        if(length(infoIndex) > 1){
            daty <- Reduce(intersect, infoDates)
            if(length(daty) == 0){
                Insert.Messages.Out(GalParams[['message']][['11']], TRUE, "e")
                return(NULL)
            }
        }else daty <- infoDates[[1]]

        infoIndex <- lapply(seq_along(infoIndex), function(j){
            x <- infoIndex[[j]]
            it <- match(daty, x$dateInfo$date)
            x$dateInfo$date <- x$dateInfo$date[it]
            x$dateInfo$index <- x$dateInfo$index[it]
            x
        })

        #########
        outDSDIR <- file.path(GalParams$output, GalParams$dataset.name)
        dataDIR <- file.path(outDSDIR, 'DATA')
        dir.create(dataDIR, showWarnings = FALSE, recursive = TRUE)

        outIndex <- infoIndex[[1]]
        outIndex$varInfo <- GalParams$varinfo
        outIndex$dateInfo$date <- daty
        outIndex$dateInfo$index <- seq_along(daty)

        outIdxFile <- file.path(outDSDIR, paste0(GalParams$dataset.name, '.rds'))
        conn <- gzfile(outIdxFile, compression = 6)
        open(conn, "wb")
        saveRDS(outIndex, conn)
        close(conn)

        #########
        constData <- NULL
        if(any(const)){
            constData <- lapply(names(GalParams$inputs[const]), function(n){
                tmp <- getNcdfOpenData(GalParams$inputs[[n]]$dir)
                tmp[[2]]$z
            })
            names(constData) <- formulaVar[const]
        }

        ######
        txtFormula <- GalParams$formula

        is_latlon <- grepl('LAT|LON', txtFormula)
        if(is_latlon){
            ndim <- sapply(ncsample[[1]][c('lon', 'lat')], length)
            if(grepl('LON', txtFormula)){
                constData[['LON']] <- matrix(ncsample[[1]]$lon, nrow = ndim[1], ncol = ndim[2])
                txtFormula <- gsub("LON", "donne[['LON']]", txtFormula)
            }
            if(grepl('LAT', txtFormula)){
                constData[['LAT']] <- matrix(ncsample[[1]]$lat, nrow = ndim[1], ncol = ndim[2], byrow = TRUE)
                txtFormula <- gsub("LAT", "donne[['LAT']]", txtFormula)
            }
        }

        for(j in seq_along(formulaVar))
            txtFormula <- gsub(paste0("X", j), paste0("donne[['X", j, "']]"), txtFormula)

        #########
        chunkfile <- sort(unique(outIndex$colInfo$index))
        chunkcalc <- split(chunkfile, ceiling(chunkfile/outIndex$chunkfac))

        do.parChunk <- if(outIndex$chunkfac > length(chunkcalc)) TRUE else FALSE
        do.parCALC <- if(do.parChunk) FALSE else TRUE

        cdtParallelCond <- .cdtData$Config$parallel

        parsL <- doparallel.cond(do.parCALC & (length(chunkcalc) > 5))
        ret <- cdt.foreach(seq_along(chunkcalc), parsL, GUI = TRUE,
                           progress = TRUE, FUN = function(j)
        {
            donne <- lapply(seq_along(infoIndex), function(i){
                x <- readCdtDatasetChunk.sequence(chunkcalc[[j]], fileRDS[[i]], cdtParallelCond, do.par = do.parChunk)
                x <- x[infoIndex[[i]]$dateInfo$index, , drop = FALSE]
                x
            })
            names(donne) <- formulaVar[!const]

            if(!is.null(constData)){
                ix <- infoIndex[[1]]$colInfo$index %in% chunkcalc[[j]]
                id <- infoIndex[[1]]$colInfo$id[ix]
                nr <- length(infoIndex[[1]]$dateInfo$index)
                nc <- length(id)
                tmp <- lapply(constData, function(x){
                    matrix(x[id], nrow = nr, ncol = nc, byrow = TRUE)
                })
                donne <- c(donne, tmp)
            }

            evalOut <- NULL
            evalFormula <- paste("evalOut =", txtFormula)
            res <- try(eval(parse(text = evalFormula)), silent = TRUE)
            if(inherits(res, "try-error")) return(-2)
            if(is.null(evalOut)) return(-2)

            evalOut[is.nan(evalOut)] <- NA
            writeCdtDatasetChunk.sequence(evalOut, chunkcalc[[j]], outIndex, dataDIR, cdtParallelCond, do.par = do.parChunk)

            return(0)
        })

        noEval <- sapply(ret, function(x) x == -2)
        if(any(noEval)){
            out <- do.call(c, chunkcalc[noEval])
            out <- paste0(out, collapse = ',')
            out1 <- "chunk numbers:"
            out <- paste(GalParams[['message']][['12']], out1, out, sep = '\n')

            Insert.Messages.Out(out, TRUE, "e")
            return(NULL)
        }
    }

    return(0)
}
