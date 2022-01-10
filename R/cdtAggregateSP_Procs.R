
AggregateSpNc_Execute <- function(){
    Insert.Messages.Out(.cdtData$GalParams[['message']][['3']], TRUE, "i")

    outputdir <- .cdtData$GalParams$output
    if(is.na(outputdir) | outputdir %in% c("", "NA")){
        Insert.Messages.Out(.cdtData$GalParams[['message']][['4']], format = TRUE)
        return(NULL)
    }
    outputNC <- file.path(outputdir, "Regrided_Data")
    dir.create(outputNC, showWarnings = FALSE, recursive = TRUE)

    if(.cdtData$GalParams$nb.ncfile == "cdtnetcdf1"){
        ncDataInfo <- getNCDFSampleData(.cdtData$GalParams$ncdf$fileordir)
        if(is.null(ncDataInfo)){
            Insert.Messages.Out(.cdtData$GalParams[['message']][['5']], format = TRUE)
            return(NULL)
        }
    }

    if(.cdtData$GalParams$nb.ncfile == "cdtnetcdfs"){
        ncDataInfo <- getNCDFSampleData(.cdtData$GalParams$ncdf$sample)
        if(is.null(ncDataInfo)){
            Insert.Messages.Out(.cdtData$GalParams[['message']][['6']], format = TRUE)
            return(NULL)
        }
    }

    if(.cdtData$GalParams$nb.ncfile == "cdtdataset"){
        if(!file.exists(.cdtData$GalParams$ncdf$fileordir)){
            Insert.Messages.Out(.cdtData$GalParams[['message']][['6-1']], format = TRUE)
            return(NULL)
        }
        oldIndex <- readRDS(.cdtData$GalParams$ncdf$fileordir)
        ncDataInfo <- list(lon = oldIndex$coords$mat$x,
                           lat = oldIndex$coords$mat$y)

        dirOldCDTData <- file.path(dirname(.cdtData$GalParams$ncdf$fileordir), "DATA")
        ## create new cdt dataset
        indexOldCDTData <- basename(.cdtData$GalParams$ncdf$fileordir)
        dirNewCDTData <- file.path(outputNC, tools::file_path_sans_ext(indexOldCDTData))
        fileIndxNewCDTData <- file.path(dirNewCDTData, indexOldCDTData)
        dirDataNewCDTData <- file.path(dirNewCDTData, "DATA")
        dir.create(dirDataNewCDTData, showWarnings = FALSE, recursive = TRUE)
    }

    ###########################################

    match.var <- FALSE
    if(.cdtData$GalParams$ncdf.grid$use.ncgrid){
        if(.cdtData$GalParams$ncdf.grid$type == "cdtnetcdf"){
            ncDataNew <- getNCDFSampleData(.cdtData$GalParams$ncdf.grid$file)
            if(is.null(ncDataNew)){
                Insert.Messages.Out(.cdtData$GalParams[['message']][['7']], format = TRUE)
                return(NULL)
            }

            grd.lon <- ncDataNew$lon
            grd.lat <- ncDataNew$lat
        }else{
            if(!file.exists(.cdtData$GalParams$ncdf.grid$file)){
                Insert.Messages.Out(.cdtData$GalParams[['message']][['7-1']], format = TRUE)
                return(NULL)
            }

            ncDataNew <- readRDS(.cdtData$GalParams$ncdf.grid$file)
            grd.lon <- ncDataNew$coords$mat$x
            grd.lat <- ncDataNew$coords$mat$y
        }

        match.var <- .cdtData$GalParams$ncdf.grid$match.var
    }else{
        X0 <- .cdtData$GalParams$res$minlon
        X1 <- .cdtData$GalParams$res$maxlon
        Y0 <- .cdtData$GalParams$res$minlat
        Y1 <- .cdtData$GalParams$res$maxlat
        pX <- .cdtData$GalParams$res$reslon
        pY <- .cdtData$GalParams$res$reslat
        grd.lon <- seq(X0, X1, pX)
        grd.lat <- seq(Y0, Y1, pY)

        if(.cdtData$GalParams$method != "bilinear"){
            if(grd.lon[length(grd.lon)] + pX/2 < X1)
                grd.lon <- c(grd.lon, grd.lon[length(grd.lon)] + pX)
            if(grd.lat[length(grd.lat)] + pY/2 < Y1)
                grd.lat <- c(grd.lat, grd.lat[length(grd.lat)] + pY)
        }
    }

    ###########################################

    if(match.var){
        if(.cdtData$GalParams$ncdf.grid$type == "cdtnetcdf"){
            if(.cdtData$GalParams$nb.ncfile != "cdtdataset"){
                ncinfo0 <- ncDataNew[c('ilon', 'ilat', 'xo', 'yo')]
                dx <- ncdf4::ncdim_def(ncDataNew$diminfo[1, 1], ncDataNew$diminfo[1, 3], grd.lon[ncDataNew$xo])
                dy <- ncdf4::ncdim_def(ncDataNew$diminfo[2, 1], ncDataNew$diminfo[2, 3], grd.lat[ncDataNew$yo])
                xydim <- if(ncDataNew$ilon < ncDataNew$ilat) list(dx, dy) else list(dy, dx)
            }else{
               newIndex <- cdtdataset.new.index(oldIndex, list(lon = grd.lon, lat = grd.lat))
            }
        }else{
            if(.cdtData$GalParams$nb.ncfile != "cdtdataset"){
                ncinfo0 <- list(ilon = 1, ilat = 2,
                                xo = seq_along(grd.lon),
                                yo = seq_along(grd.lat))
                dx <- ncdf4::ncdim_def("Lon", "degreeE", grd.lon)
                dy <- ncdf4::ncdim_def("Lat", "degreeN", grd.lat)
                xydim <- list(dx, dy)
            }else{
                newIndex <- ncDataNew 
            }
        }
    }else{
        if(.cdtData$GalParams$nb.ncfile != "cdtdataset"){
            dx <- ncdf4::ncdim_def("Lon", "degreeE", grd.lon)
            dy <- ncdf4::ncdim_def("Lat", "degreeN", grd.lat)
            xydim <- list(dx, dy)
        }else{
            newIndex <- cdtdataset.new.index(oldIndex, list(lon = grd.lon, lat = grd.lat))
        }
    }

    ###########################################

    if(.cdtData$GalParams$nb.ncfile != "cdtdataset"){
        ncinfo <- ncDataInfo[c('ilon', 'ilat', 'varid')]
    }

    old.grid <- defSpatialPixels(ncDataInfo[c('lon', 'lat')], regrid = TRUE)

    ###########################################

    new.grid <- defSpatialPixels(list(lon = grd.lon, lat = grd.lat), regrid = TRUE)
    is.regridNC <- is.diffSpatialPixelsObj(old.grid, new.grid, tol = 1e-07)
    if(!is.regridNC){
        Insert.Messages.Out(.cdtData$GalParams[['message']][['8']], TRUE, "i")
        return(0)
    }

    if(!.cdtData$GalParams$ncdf.grid$use.ncgrid){
        if(.cdtData$GalParams$but == "Aggregate"){
            if(all(new.grid@grid@cellsize < old.grid@grid@cellsize)){
                Insert.Messages.Out(.cdtData$GalParams[['message']][['9']], TRUE, "i")
                return(0)
            }
        }else{
            if(all(old.grid@grid@cellsize < new.grid@grid@cellsize)){
                Insert.Messages.Out(.cdtData$GalParams[['message']][['10']], TRUE, "i")
                return(0)
            }
        }
    }

    if(.cdtData$GalParams$method == "bilinear"){
        gridInterp <- list(lon = grd.lon, lat = grd.lat)
        grid.dim <- c(length(grd.lon), length(grd.lat))
    }else{
        ## maybe use raster::extract
        ixy <- over(old.grid, new.grid)
        grid.dim <- new.grid@grid@cells.dim
    }

    ###########################################

    if(.cdtData$GalParams$nb.ncfile == "cdtnetcdf1"){
        jfile <- getIndex.AllOpenFiles(.cdtData$GalParams$ncdf$fileordir)
        ncfile <- .cdtData$OpenFiles$Data[[jfile]][[3]]

        #####################

        nc <- ncdf4::nc_open(ncfile)
        xlon <- nc$var[[ncinfo$varid]]$dim[[ncinfo$ilon]]$vals
        xlat <- nc$var[[ncinfo$varid]]$dim[[ncinfo$ilat]]$vals
        zval <- ncdf4::ncvar_get(nc, varid = ncinfo$varid)

        nc.name <- ncinfo$varid
        nc.longname <- nc$var[[nc.name]]$longname
        nc.units <- nc$var[[nc.name]]$units
        nc.missval <- nc$var[[nc.name]]$missval
        nc.prec <- nc$var[[nc.name]]$prec
        ncdf4::nc_close(nc)

        #####################

        if(match.var){
            nomVar <- if(.cdtData$GalParams$ncdf.grid$type == "cdtnetcdf") "varinfo" else "varInfo"
            nc.name <- ncDataNew[[nomVar]]$name
            nc.longname <- ncDataNew[[nomVar]]$longname
            nc.units <- ncDataNew[[nomVar]]$units
            nc.prec <- ncDataNew[[nomVar]]$prec
            nc.missval <- ncDataNew[[nomVar]]$missval
        }

        grd.nc.out <- ncdf4::ncvar_def(nc.name, nc.units, xydim, nc.missval, prec = nc.prec,
                                       longname = nc.longname, compression = 9)

        #####################

        ncinfo$xo <- order(xlon)
        xlon <- xlon[ncinfo$xo]
        ncinfo$yo <- order(xlat)
        xlat <- xlat[ncinfo$yo]
        zval <- transposeNCDFData(zval, ncinfo)

        #####################

        if(.cdtData$GalParams$method == "bilinear"){
            z.out <- cdt.interp.surface.grid(list(lon = xlon, lat = xlat, z = zval), gridInterp)
            z.out <- z.out$z
            z.out[is.na(z.out)] <- nc.missval
        }else{
            z.out <- matrix(NA, grid.dim[1], grid.dim[2])
            out <- tapply(c(zval), ixy, mean, na.rm = TRUE)
            z.out[as.numeric(names(out))] <- out
            z.out[is.na(z.out) | is.nan(z.out) | is.infinite(z.out)] <- nc.missval
        }

        if(match.var) z.out <- transposeNCDFData.inv(z.out, ncinfo0)

        outfl <- file.path(outputNC, basename(ncfile))
        nc2 <- ncdf4::nc_create(outfl, grd.nc.out)
        ncdf4::ncvar_put(nc2, grd.nc.out, z.out)
        ncdf4::nc_close(nc2)
    }

    ###########################################

    if(.cdtData$GalParams$nb.ncfile == "cdtnetcdfs"){
        allncfiles <- list.files(.cdtData$GalParams$ncdf$fileordir, ".+\\.nc$", full.names = TRUE)
        if(length(allncfiles) == 0){
            Insert.Messages.Out(.cdtData$GalParams[['message']][['11']], format = TRUE)
            return(NULL)
        }

        #####################

        nc <- ncdf4::nc_open(allncfiles[1])
        xlon <- nc$var[[ncinfo$varid]]$dim[[ncinfo$ilon]]$vals
        xlat <- nc$var[[ncinfo$varid]]$dim[[ncinfo$ilat]]$vals

        nc.name <- ncinfo$varid
        nc.longname <- nc$var[[nc.name]]$longname
        nc.units <- nc$var[[nc.name]]$units
        nc.missval <- nc$var[[nc.name]]$missval
        nc.prec <- nc$var[[nc.name]]$prec
        ncdf4::nc_close(nc)

        #####################

        if(match.var){
            nomVar <- if(.cdtData$GalParams$ncdf.grid$type == "cdtnetcdf") "varinfo" else "varInfo"
            nc.name <- ncDataNew[[nomVar]]$name
            nc.longname <- ncDataNew[[nomVar]]$longname
            nc.units <- ncDataNew[[nomVar]]$units
            nc.prec <- ncDataNew[[nomVar]]$prec
            nc.missval <- ncDataNew[[nomVar]]$missval
        }

        grd.nc.out <- ncdf4::ncvar_def(nc.name, nc.units, xydim, nc.missval, prec = nc.prec,
                                       longname = nc.longname, compression = 9)

        #####################

        ncinfo$xo <- order(xlon)
        xlon <- xlon[ncinfo$xo]
        ncinfo$yo <- order(xlat)
        xlat <- xlat[ncinfo$yo]
        xnlon <- length(xlon)
        xnlat <- length(xlat)

        #####################

        ncsample <- .cdtData$GalParams$ncdf$sample
        aggrmethod <- .cdtData$GalParams$method

        #####################

        GUI <- TRUE
        progress <- TRUE
        parsL <- doparallel.cond(length(allncfiles) >= 500)

        ret <- cdt.foreach(seq_along(allncfiles), parsL, GUI, progress, FUN = function(jj)
        {
            nc <- ncdf4::nc_open(allncfiles[jj])
            zval <- ncdf4::ncvar_get(nc, varid = ncinfo$varid)
            ncdf4::nc_close(nc)

            if(any(dim(zval) != c(xnlon, xnlat))) return(basename(allncfiles[jj]))

            zval <- transposeNCDFData(zval, ncinfo)

            if(aggrmethod == "bilinear"){
                z.out <- cdt.interp.surface.grid(list(lon = xlon, lat = xlat, z = zval), gridInterp)
                z.out <- z.out$z
                z.out[is.na(z.out)] <- nc.missval
            }else{
                z.out <- matrix(NA, grid.dim[1], grid.dim[2])
                out <- tapply(c(zval), ixy, mean, na.rm = TRUE)
                z.out[as.numeric(names(out))] <- out
                z.out[is.na(z.out) | is.nan(z.out) | is.infinite(z.out)] <- nc.missval
            }

            if(match.var) z.out <- transposeNCDFData.inv(z.out, ncinfo0)

            outfl <- file.path(outputNC, basename(allncfiles[jj]))
            nc2 <- ncdf4::nc_create(outfl, grd.nc.out)
            ncdf4::ncvar_put(nc2, grd.nc.out, z.out)
            ncdf4::nc_close(nc2)

            return(NULL)
        })

        #####################

        inull <- !sapply(ret, is.null)
        if(any(inull)){
            msg <- paste(unlist(ret[inull]), "and", ncsample, "have different dimension")
            msg <- paste("Unprocessed Files", paste(msg, collapse = "\n"), sep = "\n")
            containertab <- Display_Output_Console_Tab(msg, "Wrong-Files", cat)
            ntab <- update.OpenTabs('ctxt', containertab)
            tkselect(.cdtEnv$tcl$main$tknotes, ntab)
        }
    }

    ###########################################

    if(.cdtData$GalParams$nb.ncfile == "cdtdataset"){
        if(.cdtData$GalParams$method == "bilinear"){
            gridOld <- oldIndex$coords$mat
            names(gridOld) <- c('lon', 'lat')
            ixy <- interp.bilinear.pars(gridOld, gridInterp)

            index_old <- split(ixy$index, row(ixy$index))
            fac_old <- split(ixy$fac, row(ixy$fac))

            grp_old <- oldIndex$colInfo$index[oldIndex$colInfo$order]
            chunk_order <- split(seq_along(oldIndex$colInfo$index), oldIndex$colInfo$index)
            chunk_order <- do.call(c, lapply(chunk_order, seq_along))
            chunk_order <- chunk_order[oldIndex$colInfo$order]

            Group <- split(seq_along(newIndex$colInfo$index), newIndex$colInfo$index)

            for(ii in seq_along(Group)){
                id <- as.character(newIndex$colInfo$id[Group[[ii]]])
                index_old_g <- index_old[id]
                fac_old_g <- fac_old[id]

                z.out <- lapply(seq_along(index_old_g), function(j){
                    id_ordered <- index_old_g[[j]]
                    chk_fac <- fac_old_g[[j]]
                    grp <- grp_old[id_ordered]
                    chk <- chunk_order[id_ordered]
                    xgrp <- split(seq_along(grp), grp)
                    dat <- lapply(xgrp, function(i){
                        ig <- grp[i][1]
                        chunk_file <- file.path(dirOldCDTData, paste0(ig, '.rds'))
                        chunk_data <- readRDS(chunk_file)
                        chunk_data[, chk[i], drop = FALSE]
                    })

                    dat <- do.call(cbind, dat)
                    dat <- sweep(dat, 2, chk_fac, '*')
                    ina <- rowSums(!is.na(dat)) == 0
                    dat <- rowSums(dat)
                    dat[ina] <- NA

                    dat
                })

                chunk_data <- do.call(cbind, z.out)
                dimnames(chunk_data) <- NULL

                ig <- newIndex$colInfo$index[Group[[ii]]][1]
                file.rds <- file.path(dirDataNewCDTData, paste0(ig, '.rds'))
                con <- gzfile(file.rds, compression = 7)
                open(con, "wb")
                saveRDS(chunk_data, con)
                close(con)
            }
        }else{
            id_ordered <- as.numeric(names(ixy))
            grp_old <- oldIndex$colInfo$index[oldIndex$colInfo$order][id_ordered]
            chunk_order <- split(seq_along(oldIndex$colInfo$index), oldIndex$colInfo$index)
            chunk_order <- do.call(c, lapply(chunk_order, seq_along))
            chunk_order <- chunk_order[oldIndex$colInfo$order][id_ordered]
            ix_new <- split(seq_along(ixy), ixy)
            Group <- split(seq_along(newIndex$colInfo$index), newIndex$colInfo$index)

            for(ii in seq_along(Group)){
                id <- as.character(newIndex$colInfo$id[Group[[ii]]])
                ix_new_g <- ix_new[id]

                z.out <- lapply(ix_new_g, function(ix){
                    grp <- grp_old[ix]
                    chko <- chunk_order[ix]
                    xgrp <- split(seq_along(grp), grp)
                    dat <- lapply(xgrp, function(i){
                        ig <- grp[i][1]
                        chunk_file <- file.path(dirOldCDTData, paste0(ig, '.rds'))
                        chunk_data <- readRDS(chunk_file)
                        chunk_data[, chko[i], drop = FALSE]
                    })

                    dat <- do.call(cbind, dat)
                    dat <- rowMeans(dat, na.rm = TRUE)
                    dat[is.nan(dat)] <- NA

                    dat
                })

                chunk_data <- do.call(cbind, z.out)
                dimnames(chunk_data) <- NULL

                ig <- newIndex$colInfo$index[Group[[ii]]][1]
                file.rds <- file.path(dirDataNewCDTData, paste0(ig, '.rds'))
                con <- gzfile(file.rds, compression = 7)
                open(con, "wb")
                saveRDS(chunk_data, con)
                close(con)
            }
        }

        con <- gzfile(fileIndxNewCDTData, compression = 6)
        open(con, "wb")
        saveRDS(newIndex, con)
        close(con)
    }

    return(0)
}
