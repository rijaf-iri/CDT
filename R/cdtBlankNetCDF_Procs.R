
blankNcdf_Execute <- function(){
    Insert.Messages.Out("Blank NetCDF data ... ", TRUE, "i")

    outputdir <- .cdtData$GalParams$output
    if(is.na(outputdir) | outputdir %in% c("", "NA")){
        Insert.Messages.Out("Directory to save results is missing", TRUE, "e")
        return(NULL)
    }
    outputNC <- file.path(outputdir, "Blanked_NetCDF")
    dir.create(outputNC, showWarnings = FALSE, recursive = TRUE)

    if(.cdtData$GalParams$nbnc == "one"){
        ncDataInfo <- getNCDFSampleData(.cdtData$GalParams$dirnc)
        if(is.null(ncDataInfo)){
            Insert.Messages.Out("Unable to read NetCDF file", TRUE, "e")
            return(NULL)
        }
    }

    if(.cdtData$GalParams$nbnc == "several"){
        ncDataInfo <- getNCDFSampleData(.cdtData$GalParams$sample)
        if(is.null(ncDataInfo)){
            Insert.Messages.Out("Unable to read the sample NetCDF file", TRUE, "e")
            return(NULL)
        }
    }

    ###########################################

    xy.grid <- ncDataInfo[c('lon', 'lat')]
    ncinfo <- ncDataInfo[c('ilon', 'ilat', 'varid')]

    dx <- ncdf4::ncdim_def("Lon", "degreeE", xy.grid$lon)
    dy <- ncdf4::ncdim_def("Lat", "degreeN", xy.grid$lat)
    xydim <- list(dx, dy)

    ###########################################

    if(.cdtData$GalParams$shpf == ""){
        Insert.Messages.Out("No shapefiles provided ", TRUE, "e")
        return(NULL)
    }

    shpd <- getShpOpenData(.cdtData$GalParams$shpf)[[2]]
    if(is.null(shpd)){
        Insert.Messages.Out("Unable to open the shapefiles", TRUE, "e")
        return(NULL)
    }
    outMask <- create.mask.grid(shpd, xy.grid)

    ###########################################

    if(.cdtData$GalParams$nbnc == "one"){
        jfile <- getIndex.AllOpenFiles(.cdtData$GalParams$dirnc)
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

        ncinfo$xo <- order(xlon)
        ncinfo$yo <- order(xlat)

        grd.ncout <- ncdf4::ncvar_def(nc.name, nc.units, xydim, nc.missval, prec = nc.prec,
                                      longname = nc.longname, compression = 9)
        #####################

        zval <- transposeNCDFData(zval, ncinfo)
        zval <- zval * outMask
        zval[is.na(zval) | is.nan(zval) | is.infinite(zval)] <- nc.missval

        outfl <- file.path(outputNC, basename(ncfile))
        nc <- ncdf4::nc_create(outfl, grd.ncout)
        ncdf4::ncvar_put(nc, grd.ncout, zval)
        ncdf4::nc_close(nc)
    }

    ###########################################

    if(.cdtData$GalParams$nbnc == "several"){
        allncfiles <- list.files(.cdtData$GalParams$dirnc, ".+\\.nc$", full.names = TRUE)
        if(length(allncfiles) == 0){
            Insert.Messages.Out("No NetCDF files found", format = TRUE)
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

        ncinfo$xo <- order(xlon)
        ncinfo$yo <- order(xlat)

        grd.ncout <- ncdf4::ncvar_def(nc.name, nc.units, xydim, nc.missval, prec = nc.prec,
                                      longname = nc.longname, compression = 9)
        #####################

        GUI <- TRUE
        progress <- TRUE
        parsL <- doparallel.cond(length(allncfiles) >= 30)

        ret <- cdt.foreach(seq_along(allncfiles), parsL, GUI, progress, FUN = function(jj)
        {
            nc <- ncdf4::nc_open(allncfiles[jj])
            zval <- ncdf4::ncvar_get(nc, varid = ncinfo$varid)
            ncdf4::nc_close(nc)
            zval <- transposeNCDFData(zval, ncinfo)
            zval <- zval * outMask
            zval[is.na(zval) | is.nan(zval) | is.infinite(zval)] <- nc.missval

            outfl <- file.path(outputNC, basename(allncfiles[jj]))
            nc <- ncdf4::nc_create(outfl, grd.ncout)
            ncdf4::ncvar_put(nc, grd.ncout, zval)
            ncdf4::nc_close(nc)
            return(0)
        })
    }

    return(0)
}
