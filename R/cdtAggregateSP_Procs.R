
AggregateSpNc_Execute <- function(){
    Insert.Messages.Out(.cdtData$GalParams[['message']][['3']], TRUE, "i")

    outputdir <- .cdtData$GalParams$output
    if(is.na(outputdir) | outputdir %in% c("", "NA")){
        Insert.Messages.Out(.cdtData$GalParams[['message']][['4']], format = TRUE)
        return(NULL)
    }
    outputNC <- file.path(outputdir, "Regrided_NetCDF")
    dir.create(outputNC, showWarnings = FALSE, recursive = TRUE)

    if(.cdtData$GalParams$nb.ncfile == "one"){
        ncDataInfo <- getNCDFSampleData(.cdtData$GalParams$ncdf$fileordir)
        if(is.null(ncDataInfo)){
            Insert.Messages.Out(.cdtData$GalParams[['message']][['5']], format = TRUE)
            return(NULL)
        }
    }

    if(.cdtData$GalParams$nb.ncfile == "several"){
        ncDataInfo <- getNCDFSampleData(.cdtData$GalParams$ncdf$sample)
        if(is.null(ncDataInfo)){
            Insert.Messages.Out(.cdtData$GalParams[['message']][['6']], format = TRUE)
            return(NULL)
        }
    }

    ###########################################

    match.var <- FALSE
    if(.cdtData$GalParams$ncdf.grid$use.ncgrid){
        ncDataNew <- getNCDFSampleData(.cdtData$GalParams$ncdf.grid$file)
        if(is.null(ncDataNew)){
            Insert.Messages.Out(.cdtData$GalParams[['message']][['7']], format = TRUE)
            return(NULL)
        }
        grd.lon <- ncDataNew$lon
        grd.lat <- ncDataNew$lat
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
        ncinfo0 <- ncDataNew[c('ilon', 'ilat', 'xo', 'yo')]

        dx <- ncdim_def(ncDataNew$diminfo[1, 1], ncDataNew$diminfo[1, 3], grd.lon[ncDataNew$xo])
        dy <- ncdim_def(ncDataNew$diminfo[2, 1], ncDataNew$diminfo[2, 3], grd.lat[ncDataNew$yo])
        xydim <- if(ncDataNew$ilon < ncDataNew$ilat) list(dx, dy) else list(dy, dx)
    }else{
        dx <- ncdim_def("Lon", "degreeE", grd.lon)
        dy <- ncdim_def("Lat", "degreeN", grd.lat)
        xydim <- list(dx, dy)
    }

    ###########################################

    ncinfo <- ncDataInfo[c('ilon', 'ilat', 'varid')]
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

    if(.cdtData$GalParams$nb.ncfile == "one"){
        jfile <- getIndex.AllOpenFiles(.cdtData$GalParams$ncdf$fileordir)
        ncfile <- .cdtData$OpenFiles$Data[[jfile]][[3]]

        #####################

        nc <- nc_open(ncfile)
        xlon <- nc$var[[ncinfo$varid]]$dim[[ncinfo$ilon]]$vals
        xlat <- nc$var[[ncinfo$varid]]$dim[[ncinfo$ilat]]$vals
        zval <- ncvar_get(nc, varid = ncinfo$varid)

        nc.name <- ncinfo$varid
        nc.longname <- nc$var[[nc.name]]$longname
        nc.units <- nc$var[[nc.name]]$units
        nc.missval <- nc$var[[nc.name]]$missval
        nc.prec <- nc$var[[nc.name]]$prec
        nc_close(nc)

        #####################

        if(match.var){
            nc.name <- ncDataNew$varinfo$name
            nc.longname <- ncDataNew$varinfo$longname
            nc.units <- ncDataNew$varinfo$units
            nc.prec <- ncDataNew$varinfo$prec
            nc.missval <- ncDataNew$varinfo$missval
        }

        grd.nc.out <- ncvar_def(nc.name, nc.units, xydim, nc.missval, prec = nc.prec,
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
        nc2 <- nc_create(outfl, grd.nc.out)
        ncvar_put(nc2, grd.nc.out, z.out)
        nc_close(nc2)
    }

    ###########################################

    if(.cdtData$GalParams$nb.ncfile == "several"){
        allncfiles <- list.files(.cdtData$GalParams$ncdf$fileordir, ".+\\.nc$", full.names = TRUE)
        if(length(allncfiles) == 0){
            Insert.Messages.Out(.cdtData$GalParams[['message']][['11']], format = TRUE)
            return(NULL)
        }

        #####################

        nc <- nc_open(allncfiles[1])
        xlon <- nc$var[[ncinfo$varid]]$dim[[ncinfo$ilon]]$vals
        xlat <- nc$var[[ncinfo$varid]]$dim[[ncinfo$ilat]]$vals

        nc.name <- ncinfo$varid
        nc.longname <- nc$var[[nc.name]]$longname
        nc.units <- nc$var[[nc.name]]$units
        nc.missval <- nc$var[[nc.name]]$missval
        nc.prec <- nc$var[[nc.name]]$prec
        nc_close(nc)

        #####################

        if(match.var){
            nc.name <- ncDataNew$varinfo$name
            nc.longname <- ncDataNew$varinfo$longname
            nc.units <- ncDataNew$varinfo$units
            nc.prec <- ncDataNew$varinfo$prec
            nc.missval <- ncDataNew$varinfo$missval
        }

        grd.nc.out <- ncvar_def(nc.name, nc.units, xydim, nc.missval, prec = nc.prec,
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

    return(0)
}
