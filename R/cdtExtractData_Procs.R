
ExtractDataProcs <- function(GeneralParameters, GUI = TRUE, progress = TRUE){
    xml.dlg <- file.path(.cdtDir$dirLocal, "languages", "cdtExtractData_Procs.xml")
    lang.dlg <- cdtLanguageParse(xml.dlg, .cdtData$Config$lang.iso)

    if(GUI){
        missval <- as.numeric(.cdtData$Config$missval)
    }else missval <- -9999

    ###############################################

    extraction.geom <- function(){
        Insert.Messages.Out(lang.dlg[['message']][['17']], TRUE, "i", GUI)

        if(GeneralParameters$type.extract == 'point')
        {
            if(is.na(xpadLon)) xpadLon <- 0
            if(is.na(xpadLat)) xpadLat <- 0

            headinfo <- cbind('Point', xminLon, xminLat)
            nxy <- spxycrd@grid@cellsize
            padx <- round(xpadLon/nxy[1])
            pady <- round(xpadLat/nxy[2])

            xx <- xminLon + nxy[1] * (-padx:padx)
            yy <- xminLat + nxy[2] * (-pady:pady)
            if(length(xx) > 1 | length(yy) > 1){
                voisin <- defSpatialPixels(list(lon = xx, lat = yy))
            }else{
                voisin <- data.frame(lon = xx, lat = yy)
                sp::coordinates(voisin) <- ~lon+lat
            }

            ij2xtr <- sp::over(voisin, spxycrd)
            if(!any(!is.na(ij2xtr))){
                Insert.Messages.Out(lang.dlg[['message']][['19']], TRUE, "e", GUI)
                return(NULL)
            }
        }

        if(GeneralParameters$type.extract == 'mpoint')
        {
            if(is.na(xpadLon)) xpadLon <- 0
            if(is.na(xpadLat)) xpadLat <- 0

            pts <- GeneralParameters$Geom$multiObj
            headinfo <- pts

            nxy <- spxycrd@grid@cellsize
            padx <- round(xpadLon/nxy[1])
            pady <- round(xpadLat/nxy[2])
            voisin <- lapply(seq(nrow(pts)), function(j){
                            xx <- pts[j, 2] + nxy[1] * (-padx:padx)
                            yy <- pts[j, 3] + nxy[2] * (-pady:pady)
                            if(length(xx) > 1 | length(yy) > 1){
                                xy <- defSpatialPixels(list(lon = xx, lat = yy))
                            }else{
                                xy <- data.frame(lon = xx, lat = yy)
                                sp::coordinates(xy) <- ~lon+lat
                            }
                            return(xy)
                        })
            ij2xtr <- lapply(voisin, sp::over, y = spxycrd)
            if(all(sapply(ij2xtr, function(x) !any(!is.na(x))))){
                Insert.Messages.Out(lang.dlg[['message']][['19']], TRUE, "e", GUI)
                return(NULL)
            }
        }

        if(GeneralParameters$type.extract %in% c('rect', 'poly')){
            if(GeneralParameters$type.extract == 'rect'){
                rectPoly <- sp::Polygon(cbind(c(xminLon, xmaxLon, xmaxLon, xminLon, xminLon),
                                        c(xminLat, xminLat, xmaxLat, xmaxLat, xminLat)))
                rectPoly <- sp::Polygons(list(rectPoly), "p1")
                geomPoly <- sp::SpatialPolygons(list(rectPoly), 1:1)
                headinfo <- matrix(c('Rectangle', rowMeans(sp::bbox(geomPoly))), nrow = 1)
            }

            if(GeneralParameters$type.extract == 'poly'){
                idPoly <- trimws(shpf.union@data$Group.1) == GeneralParameters$Geom$namePoly
                geomPoly <- shpf.union[idPoly, ]
                namepoly <- gsub("[^[:alnum:]]", "", GeneralParameters$Geom$namePoly)
                namepoly <- substr(namepoly, 1, 25)
                headinfo <- cbind(namepoly, sp::coordinates(geomPoly))
            }

            polyRas <- spxycrd
            polyRas$z <- seq_along(spxycrd)
            polyRas <- raster::raster(polyRas)
            ij2xtr <- raster::extract(polyRas, geomPoly, weights = TRUE,
                                      normalizeWeights = TRUE, cellnumbers = TRUE)
            if(is.null(ij2xtr[[1]])){
                Insert.Messages.Out(lang.dlg[['message']][['19']], TRUE, "e", GUI)
                return(NULL)
            }
        }

        if(GeneralParameters$type.extract == 'mpoly'){
            idPoly <- trimws(shpf.union@data$Group.1) %in% GeneralParameters$Geom$multiObj
            shpf.regOI <- shpf.union[idPoly, ]
            headinfo <- cbind(as.character(shpf.regOI@data$Group.1), round(sp::coordinates(shpf.regOI), 5))
            headinfo[, 1] <- substr(gsub("[^[:alnum:]]", "", headinfo[, 1]), 1, 25)

            polyRas <- spxycrd
            polyRas$z <- seq_along(spxycrd)
            polyRas <- raster::raster(polyRas)
            ij2xtr <- raster::extract(polyRas, shpf.regOI, weights = TRUE,
                                      normalizeWeights = TRUE, cellnumbers = TRUE)
            if(all(sapply(ij2xtr, is.null))){
                Insert.Messages.Out(lang.dlg[['message']][['19']], TRUE, "e", GUI)
                return(NULL)
            }
        }

        Insert.Messages.Out(lang.dlg[['message']][['18']], TRUE, "s", GUI)

        list(headinfo = headinfo, ij2xtr = ij2xtr)
    }

    ###############################################

    tstep <- GeneralParameters$intstep
    date.range <- GeneralParameters$date.range
    minhour <- GeneralParameters$minhour

    xminLon <- GeneralParameters$Geom$minlon
    xmaxLon <- GeneralParameters$Geom$maxlon
    xminLat <- GeneralParameters$Geom$minlat
    xmaxLat <- GeneralParameters$Geom$maxlat
    xpadLon <- GeneralParameters$Geom$padlon
    xpadLat <- GeneralParameters$Geom$padlat

    ####
    outputDIR <- GeneralParameters$out.data$outdir
    if(outputDIR == "" | outputDIR == "NA" | is.na(outputDIR)){
        msg <- if(GeneralParameters$out.data$format == 'ncdf')
                    lang.dlg[['message']][['1']]
                else
                    lang.dlg[['message']][['2']]
        Insert.Messages.Out(msg, TRUE, "e", GUI)
        return(NULL)
    }

    if(!GeneralParameters$out.data$isFile){
        outputDIR <- file.path(outputDIR, "Extracted_DATA")
        dir.create(outputDIR, showWarnings = FALSE, recursive = TRUE)
    }else{
        if(file.exists(outputDIR)) unlink(outputDIR)
    }

    #####################################

    if(!(tstep %in% c('onefile', 'others'))){
        rgtime <- check.start.end.InfoDate(tstep, date.range)
        if(any(sapply(rgtime, is.null))){
            Insert.Messages.Out(lang.dlg[['message']][['3']], TRUE, "e", GUI)
            return(NULL)
        }
    }

    #####################################

    if(GeneralParameters$type.extract == 'point')
    {
        if(is.na(xminLon) | is.na(xminLat))
        {
            Insert.Messages.Out(lang.dlg[['message']][['4']], TRUE, "e", GUI)
            return(NULL)
        }
    }

    if(GeneralParameters$type.extract == 'rect')
    {
        if(is.na(xminLon) | is.na(xmaxLon) | is.na(xminLat) | is.na(xmaxLat))
        {
            Insert.Messages.Out(lang.dlg[['message']][['5']], TRUE, "e", GUI)
            return(NULL)
        }
    }

    if(GeneralParameters$type.extract == 'mpoint'){
        if(is.null(GeneralParameters$Geom$multiObj))
        {
            Insert.Messages.Out(lang.dlg[['message']][['6']], TRUE, "e", GUI)
            return(NULL)
        }
        if(is.null(dim(GeneralParameters$Geom$multiObj)))
        {
            Insert.Messages.Out(lang.dlg[['message']][['7']], TRUE, "e", GUI)
            return(NULL)
        }
        if(nrow(GeneralParameters$Geom$multiObj) == 0)
        {
            Insert.Messages.Out(lang.dlg[['message']][['6']], TRUE, "e", GUI)
            return(NULL)
        }
    }

    if(GeneralParameters$type.extract == 'poly'){
        if(is.null(GeneralParameters$Geom$namePoly)){
            Insert.Messages.Out(lang.dlg[['message']][['8']], TRUE, "e", GUI)
            return(NULL)
        }
    }

    if(GeneralParameters$type.extract == 'mpoly'){
        if(is.null(GeneralParameters$Geom$multiObj))
        {
            Insert.Messages.Out(lang.dlg[['message']][['8']], TRUE, "e", GUI)
            return(NULL)
        }
        if(length(GeneralParameters$Geom$multiObj) == 0)
        {
            Insert.Messages.Out(lang.dlg[['message']][['8']], TRUE, "e", GUI)
            return(NULL)
        }
    }

    if(GeneralParameters$type.extract %in% c('poly', 'mpoly'))
    {
        if(!GUI){
            fileopen <- GeneralParameters$shp.file$shp
            shpf <- try(sf::st_read(fileopen, quiet = TRUE), silent = TRUE)
            if(inherits(shpf, "try-error")){
                Insert.Messages.Out(paste(lang.dlg[['message']][['9']], ":", fileopen), TRUE, "e", GUI)
                return(NULL)
            }
            sf::st_crs(shpf) <- sf::NA_crs_
        }else{
            shpf <- getShpOpenData(GeneralParameters$shp.file$shp)[[2]]
            if(is.null(shpf)){
                Insert.Messages.Out(lang.dlg[['message']][['10']], TRUE, "e", GUI)
                return(NULL)
            }
        }

        if(is.null(GeneralParameters$shp.file$attr)){
            Insert.Messages.Out(lang.dlg[['message']][['11']], TRUE, "e", GUI)
            return(NULL)
        }
        if(GeneralParameters$shp.file$attr == "" |
           is.na(GeneralParameters$shp.file$attr))
        {
            Insert.Messages.Out(lang.dlg[['message']][['11']], TRUE, "e", GUI)
            return(NULL)
        }

        iattr <- as.character(sf::st_drop_geometry(shpf[, GeneralParameters$shp.file$attr])[, 1])
        shpf.union <- stats::aggregate(iattr, list(iattr), identity)
        shpf.union$x <- seq_along(unique(iattr))
        shpf.geom <- lapply(shpf.union$x, function(j){
            y <- shpf[iattr == shpf.union$Group.1[j], ]
            sf::st_union(sf::st_geometry(y))
        })
        shpf.geom <- do.call(c, shpf.geom)
        sf::st_geometry(shpf.union) <- sf::st_sfc(shpf.geom)
        shpf.union <- sf::as_Spatial(shpf.union)
    }

    #####################################

    if(!(tstep %in% c('onefile', 'others'))){
        if(tstep == "seasonal"){
            startMonth <- GeneralParameters$season.start
            seasonLength <- GeneralParameters$season.len
            dates <- table.format.date.seasonal(date.range, startMonth, seasonLength)
        }else{
            dates <- table.format.date.time(tstep, date.range, minhour)
        }

        if(!tstep %in% c("annual", "seasonal")){
            startMonth <- GeneralParameters$months$start
            endMonth <- GeneralParameters$months$end

            seasonLength <- (endMonth - startMonth + 1) %% 12
            seasonLength[seasonLength == 0] <- 12
            monthtoExtr <- (startMonth:(startMonth + (seasonLength - 1))) %% 12
            monthtoExtr[monthtoExtr == 0] <- 12

            imois <- as.numeric(dates[, 2]) %in% monthtoExtr
            dates <- dates[imois, , drop = FALSE]
        }else{
            imois <- rep(TRUE, nrow(dates))
        }

        xdaty <- dates[, -ncol(dates), drop = FALSE]
        xdaty <- split(xdaty, col(xdaty))

        if(tstep == "seasonal"){
            dates <- paste0(xdaty[[1]], '-', xdaty[[2]], '_',
                            xdaty[[3]], '-', xdaty[[4]])
        }else{
            dates <- do.call(paste0, xdaty)
        }
    }

    #####################################

    if(GeneralParameters$data.type == 'cdtnetcdf'){
        if(tstep == "onefile"){
            inputdat <- GeneralParameters$cdtnetcdf
            ncdata <- getNcdfOpenData(inputdat$dir)
            lon <- ncdata[[2]]$x
            lat <- ncdata[[2]]$y
            varInfo <- ncdata[[2]]$varinfo
        }else if(tstep == "others"){
            inputdat <- GeneralParameters$cdtnetcdf
            ## 
            ##
            ##
            Insert.Messages.Out("Not implemented yet", TRUE, "e", GUI)
            return(NULL)
        }else{
            inputdat <- GeneralParameters$cdtnetcdf
            ncfiles <- do.call(sprintf, c(list(fmt = inputdat$format), xdaty))
            ncfiles <- ncfiles[imois]
            ncpath <- file.path(inputdat$dir, ncfiles)
            existFl <- unlist(lapply(ncpath, file.exists))

            if(!any(existFl)){
                Insert.Messages.Out(lang.dlg[['message']][['12']], TRUE, "e", GUI)
                return(NULL)
            }

            dates <- dates[existFl]
            ncpath <- ncpath[existFl]

            ####
            if(GUI){
                ncInfo <- getNCDFSampleData(inputdat$sample)
                if(is.null(ncInfo)){
                    Insert.Messages.Out(lang.dlg[['message']][['13']], TRUE, "e", GUI)
                    return(NULL)
                }
                ncInfo <- ncInfo[c('varid', 'ilon', 'ilat')]
            }else{
                ncInfo <- inputdat$sample
                if(is.null(ncInfo$varid) | is.null(ncInfo$ilon) | is.null(ncInfo$ilat)){
                    Insert.Messages.Out(lang.dlg[['message']][['14']], TRUE, "e", GUI)
                    return(NULL)
                }
            }

            varInfo <- NULL
            varInfo$name <- ncInfo$varid
            nc <- ncdf4::nc_open(ncpath[1])
            lon <- nc$var[[ncInfo$varid]]$dim[[ncInfo$ilon]]$vals
            lat <- nc$var[[ncInfo$varid]]$dim[[ncInfo$ilat]]$vals
            varInfo$longname <- nc$var[[ncInfo$varid]]$longname
            varInfo$units <- nc$var[[ncInfo$varid]]$units
            varInfo$prec <- nc$var[[ncInfo$varid]]$prec
            ncdf4::nc_close(nc)

            ncInfo$xo <- order(lon)
            lon <- lon[ncInfo$xo]
            ncInfo$yo <- order(lat)
            lat <- lat[ncInfo$yo]
        }
    }else{
        inputdat <- GeneralParameters$cdtdataset
        if(!file.exists(inputdat$index)){
            msg <- paste(inputdat$index, lang.dlg[['message']][['15']])
            Insert.Messages.Out(msg, TRUE, "e", GUI)
            return(NULL)
        }

        indexfile <- readRDS(inputdat$index)
        varInfo <- indexfile$varInfo
        dateInfo <- indexfile$dateInfo

        ix <- which(dateInfo$date %in% dates)
        if(length(ix) == 0){
            Insert.Messages.Out(lang.dlg[['message']][['16']], TRUE, "e", GUI)
            return(NULL)
        }

        dateInfo$date <- dateInfo$date[ix]
        dateInfo$index <- dateInfo$index[ix]
        lon <- indexfile$coords$mat$x
        lat <- indexfile$coords$mat$y
    }

    spxycrd <- defSpatialPixels(list(lon = lon, lat = lat), regrid = TRUE)

    #####################################
    ## Extraction Geometry
    if(GUI){
        parsextr <- list(xminLon, xminLat, xmaxLon, xmaxLat, xpadLon, xpadLat,
                         GeneralParameters$Geom$namePoly, GeneralParameters$Geom$multiObj,
                         GeneralParameters$shp.file$shp, GeneralParameters$shp.file$attr,
                         GeneralParameters$type.extract, inputdat)
        extractGeom <- TRUE
        if(!is.null(.cdtData$EnvData$procs$parsextr))
            extractGeom <- if(!isTRUE(all.equal(.cdtData$EnvData$procs$parsextr, parsextr))) TRUE else FALSE

        if(extractGeom)
        {
            ret <- extraction.geom()
            headinfo <- ret$headinfo
            ij2xtr <- ret$ij2xtr
            .cdtData$EnvData$procs$parsextr <- parsextr
            .cdtData$EnvData$procs$ij2xtr <- ij2xtr
            .cdtData$EnvData$procs$headinfo <- headinfo
        }else{
            ij2xtr <- .cdtData$EnvData$procs$ij2xtr
            headinfo <- .cdtData$EnvData$procs$headinfo
        }
    }else{
        ret <- extraction.geom()
        headinfo <- ret$headinfo
        ij2xtr <- ret$ij2xtr
    }

    #####################################

    if(GeneralParameters$type.extract == 'point')
        ij2xtr <- ij2xtr[!is.na(ij2xtr)]

    if(GeneralParameters$type.extract == 'mpoint')
    {
        ij2xtr <- lapply(ij2xtr, function(x) x[!is.na(x)])
        nonZero <- sapply(ij2xtr, length) > 0
        ij2xtr <- ij2xtr[nonZero]
    }

    if(GeneralParameters$type.extract %in% c('rect', 'poly'))
    {
        ij2xtr <- lapply(ij2xtr, function(x) x[order(x[, "value"]), , drop = FALSE])

        if(!GeneralParameters$out.data$sp.avrg){
            headinfo <- lapply(ij2xtr, function(x){
                xx <- spxycrd@coords[x[, 'value'], , drop = FALSE]
                nl <- seq(nrow(x))
                reshapeXYZ2Matrix(cbind(xx, nl))
            })
        }
    }

    if(GeneralParameters$type.extract == 'mpoly')
    {
        nonNull <- !sapply(ij2xtr, is.null)
        ij2xtr <- ij2xtr[nonNull]
        ij2xtr <- lapply(ij2xtr, function(x) x[order(x[, "value"]), , drop = FALSE])

        if(GeneralParameters$out.data$format %in% c("ncdf", "tyxz")){
            if(GeneralParameters$out.data$format == "ncdf"){
                outputDIR <- file.path(outputDIR, headinfo[, 1])
                outputDIR <- outputDIR[nonNull]
                for(dr in outputDIR)
                    dir.create(dr, showWarnings = FALSE, recursive = TRUE)
            }else{
                outputDIR <- file.path(outputDIR, paste0(headinfo[, 1], ".txt"))
                outputDIR <- outputDIR[nonNull]
                for(fl in outputDIR)
                    if(file.exists(fl)) unlink(fl)
            }
            headinfo <- lapply(ij2xtr, function(x){
                xx <- spxycrd@coords[x[, 'value'], , drop = FALSE]
                nl <- seq(nrow(x))
                reshapeXYZ2Matrix(cbind(xx, nl))
            })
        }
    }

    #####################################
    Insert.Messages.Out(lang.dlg[['message']][['20']], TRUE, "i", GUI)

    params <- GeneralParameters

    if(params$data.type == 'cdtnetcdf'){
        if(tstep == "onefile"){
            val <- ncdata[[2]]$z

            if(params$type.extract == 'point')
            {
                extdat <- val[ij2xtr]
                if(length(ij2xtr) > 1)
                    extdat <- mean(extdat, na.rm = TRUE)

                caption <- c('ID', 'LON', 'DATE/LAT')
                xhead <- t(headinfo)
                ret <- round(extdat, 3)
                ret[is.na(ret)] <- missval
                ret <- rbind(xhead, ret)
                ret <- cbind(c(caption, "Values"), ret)
                writeFiles(ret, outputDIR)
            }

            if(params$type.extract == 'mpoint')
            {
                extdat <- sapply(ij2xtr, function(ij){
                    mat <- val[ij]
                    if(length(ij) > 1)
                        mat <- mean(mat, na.rm = TRUE)
                    return(mat)
                })
                if(!all(nonZero)){
                    tmp <- rep(NA, length(nonZero))
                    tmp[nonZero] <- extdat
                    extdat <- tmp
                }

                caption <- c('ID', 'LON', 'DATE/LAT')
                xhead <- t(headinfo)
                ret <- round(extdat, 3)
                ret[is.na(ret)] <- missval
                ret <- rbind(xhead, ret)
                ret <- cbind(c(caption, "Values"), ret)
                writeFiles(ret, outputDIR)
            }

            if(params$type.extract %in% c('rect', 'poly'))
            {
                # extdat <- lapply(seq_along(ij2xtr), function(ii){
                # ij <- ij2xtr[[ii]]
                ij <- ij2xtr[[1]]
                mat <- val[ij[, "value"]]

                if(params$out.data$sp.avrg)
                {
                    if(nrow(ij) > 1){
                        mat <- mat * ij[, "weight"]
                        mat <- mat[!is.na(mat)]
                        mat <- if(length(mat) > 0) sum(mat) else NA
                    }
                    
                    caption <- c('ID', 'LON', 'DATE/LAT')
                    xhead <- t(headinfo)
                    ret <- round(mat, 3)
                    ret[is.na(ret)] <- missval
                    ret <- rbind(xhead, ret)
                    ret <- cbind(c(caption, 'Value'), ret)
                    writeFiles(ret, outputDIR)
                }else{
                    if(params$out.data$format == "ncdf"){
                        if(params$type.extract == 'rect'){
                            outfile <- paste0('extracted_rectangle_', ncdata[[1]])
                        }else{
                            namepoly <- gsub("[^[:alnum:]]", "", GeneralParameters$Geom$namePoly)
                            namepoly <- substr(namepoly, 1, 25)
                            outfile <- paste0('extracted_', namepoly, '_', ncdata[[1]])
                        }

                        outfile <- file.path(outputDIR, outfile)
                        dx <- ncdf4::ncdim_def("Lon", "degreeE", headinfo[[1]]$x)
                        dy <- ncdf4::ncdim_def("Lat", "degreeN", headinfo[[1]]$y)
                        xydim <- list(dx, dy)
                        ncout <- do.call(ncdf4::ncvar_def, c(varInfo[c('name', 'prec', 'units', 'longname')], list(dim = xydim, missval = -9999, compression = 9)))
                        mat <- mat[headinfo[[1]]$z]
                        dim(mat) <- dim(headinfo[[1]]$z)
                        mat[is.na(mat)] <- missval
                        nc <- ncdf4::nc_create(outfile, ncout)
                        ncdf4::ncvar_put(nc, ncout, mat)
                        ncdf4::nc_close(nc)
                    }

                    if(params$out.data$format == "tyxz"){
                        crds <- expand.grid(headinfo[[1]]$x, headinfo[[1]]$y)
                        crds <- round(crds[, 2:1], 6)
                        ina <- c(is.na(headinfo[[1]]$z))
                        mat <- round(mat[headinfo[[1]]$z], 3)
                        mat <- cbind(crds, mat)
                        mat <- mat[!ina, , drop = FALSE]
                        names(mat) <- c('Lat', 'Lon', 'Values')
                        writeFiles(mat, outputDIR, col.names = TRUE)
                    }
                }
                # })
            }

            if(params$type.extract == 'mpoly')
            {
                if(params$out.data$format %in% c("ncdf", "tyxz")){
                    for(ii in seq_along(ij2xtr)){
                        ij <- ij2xtr[[ii]]
                        mat <- val[ij[, "value"]]

                        if(params$out.data$format == "ncdf"){
                            outfile <- paste0('extracted_', basename(outputDIR[ii]), '_', ncdata[[1]])
                            outfile <- file.path(outputDIR[ii], outfile)
                            dx <- ncdf4::ncdim_def("Lon", "degreeE", headinfo[[ii]]$x)
                            dy <- ncdf4::ncdim_def("Lat", "degreeN", headinfo[[ii]]$y)
                            xydim <- list(dx, dy)
                            ncout <- do.call(ncdf4::ncvar_def, c(varInfo[c('name', 'prec', 'units', 'longname')], list(dim = xydim, missval = -9999, compression = 9)))
                            mat <- mat[headinfo[[ii]]$z]
                            dim(mat) <- dim(headinfo[[ii]]$z)
                            mat[is.na(mat)] <- missval
                            nc <- ncdf4::nc_create(outfile, ncout)
                            ncdf4::ncvar_put(nc, ncout, mat)
                            ncdf4::nc_close(nc)
                        }

                        if(params$out.data$format == "tyxz"){
                            crds <- expand.grid(headinfo[[ii]]$x, headinfo[[ii]]$y)
                            crds <- round(crds[, 2:1], 6)
                            ina <- c(is.na(headinfo[[ii]]$z))
                            mat <- round(mat[headinfo[[ii]]$z], 3)
                            mat <- cbind(crds, mat)
                            mat <- mat[!ina, , drop = FALSE]
                            names(mat) <- c('Lat', 'Lon', 'Values')
                            writeFiles(mat, outputDIR[ii], col.names = TRUE)
                        }
                    }
                }

                if(params$out.data$format == 'cdt'){
                    extdat <- sapply(seq_along(ij2xtr), function(ii){
                        ij <- ij2xtr[[ii]]
                        mat <- val[ij[, "value"]]

                        if(nrow(ij) > 1){
                            mat <- mat * ij[, "weight"]
                            mat <- mat[!is.na(mat)]
                            mat <- if(length(mat) > 0) sum(mat) else NA
                        }

                        return(mat)
                    })

                    if(!all(nonNull)){
                        tmp <- rep(NA, length(nonNull))
                        tmp[nonNull] <- extdat
                        extdat <- tmp
                    }

                    caption <- c('ID', 'LON', 'DATE/LAT')
                    xhead <- t(headinfo)
                    ret <- round(extdat, 3)
                    ret[is.na(ret)] <- missval
                    ret <- rbind(xhead, ret)
                    ret <- cbind(c(caption, "Values"), ret)
                    writeFiles(ret, outputDIR)
                }
            }
        }else if(tstep == "others"){
            ##
            ##
            ##
        }else{
            parsL <- doparallel.cond(length(ncpath) >= 100)
            ret <- cdt.foreach(seq_along(ncpath), parsL, GUI, progress, FUN = function(jj)
            {
                exract_write_NCDF <- function(ii, mat){
                    outfile <- file.path(outputDIR[ii], basename(ncpath[jj]))
                    dx <- ncdf4::ncdim_def("Lon", "degreeE", headinfo[[ii]]$x)
                    dy <- ncdf4::ncdim_def("Lat", "degreeN", headinfo[[ii]]$y)
                    xydim <- list(dx, dy)
                    ncout <- do.call(ncdf4::ncvar_def, c(varInfo, list(dim = xydim, missval = missval, compression = 9)))
                    mat <- mat[headinfo[[ii]]$z]
                    dim(mat) <- dim(headinfo[[ii]]$z)
                    mat[is.na(mat)] <- missval
                    nc <- ncdf4::nc_create(outfile, ncout)
                    ncdf4::ncvar_put(nc, ncout, mat)
                    ncdf4::nc_close(nc)
                }

                exract_write_TYXZ <- function(ii, mat){
                    outfile <- outputDIR[ii]
                    crds <- expand.grid(headinfo[[ii]]$x, headinfo[[ii]]$y)
                    crds <- round(crds[, 2:1], 6)
                    ina <- c(is.na(headinfo[[ii]]$z))
                    mat <- round(mat[headinfo[[ii]]$z], 3)
                    mat <- cbind(dates[jj], crds, mat)
                    mat <- mat[!ina, , drop = FALSE]
                    writeFiles(mat, outfile, append = TRUE)
                }

                ###############

                nc <- try(ncdf4::nc_open(ncpath[jj]), silent = TRUE)
                if(inherits(nc, "try-error")) return(NA)
                val <- ncdf4::ncvar_get(nc, varid = ncInfo$varid)
                ncdf4::nc_close(nc)
                val <- transposeNCDFData(val, ncInfo)

                if(params$type.extract == 'point')
                {
                    extdat <- val[ij2xtr]
                    if(length(ij2xtr) > 1)
                        extdat <- mean(extdat, na.rm = TRUE)
                }

                if(params$type.extract == 'mpoint')
                {
                    extdat <- sapply(ij2xtr, function(ij){
                        mat <- val[ij]
                        if(length(ij) > 1)
                            mat <- mean(mat, na.rm = TRUE)
                        return(mat)
                    })
                    if(!all(nonZero)){
                        tmp <- rep(NA, length(nonZero))
                        tmp[nonZero] <- extdat
                        extdat <- tmp
                        rm(tmp)
                    }
                }

                if(params$type.extract %in% c('rect', 'poly'))
                {
                    extdat <- lapply(seq_along(ij2xtr), function(ii){
                        ij <- ij2xtr[[ii]]
                        mat <- val[ij[, "value"]]

                        if(params$out.data$sp.avrg)
                        {
                            if(nrow(ij) > 1){
                                mat <- mat * ij[, "weight"]
                                mat <- mat[!is.na(mat)]
                                mat <- if(length(mat) > 0) sum(mat) else NA
                            }
                            return(mat)
                        }else{
                            if(params$out.data$format == "ncdf"){
                                exract_write_NCDF(ii, mat)
                                return(NULL)
                            }

                            if(params$out.data$format == "tyxz"){
                                exract_write_TYXZ(ii, mat)
                                return(NULL)
                            }

                            if(params$out.data$format == "cpt"){
                                ### mthd1
                                mat <- mat[headinfo[[ii]]$z]
                                dim(mat) <- dim(headinfo[[ii]]$z)
                                mat[is.na(mat)] <- missval
                                return(mat)

                                ### mthd2
                                # mat <- mat[headinfo[[ii]]$z]
                                # mat <- matrix(mat, nrow = 1)
                                # writeFiles(mat, outputDIR[ii], append = TRUE)
                                # return(jj)
                            }
                        }
                    })
                    extdat <- extdat[[1]]
                }

                if(params$type.extract == 'mpoly')
                {
                    extdat <- sapply(seq_along(ij2xtr), function(ii){
                        ij <- ij2xtr[[ii]]
                        mat <- val[ij[, "value"]]

                        if(params$out.data$format %in% c('cdt', 'cpt'))
                        {
                            if(nrow(ij) > 1){
                                mat <- mat * ij[, "weight"]
                                mat <- mat[!is.na(mat)]
                                mat <- if(length(mat) > 0) sum(mat) else NA
                            }
                            return(mat)
                        }else{
                            if(params$out.data$format == "ncdf"){
                                exract_write_NCDF(ii, mat)
                                return(NULL)
                            }

                            if(params$out.data$format == "tyxz"){
                                exract_write_TYXZ(ii, mat)
                                return(NULL)
                            }
                        }
                    })
                    
                    if(params$out.data$format %in% c('cdt', 'cpt'))
                    {
                        if(!all(nonNull)){
                            tmp <- rep(NA, length(nonNull))
                            tmp[nonNull] <- extdat
                            extdat <- tmp
                            rm(tmp)
                        }
                    }
                }

                rm(val)
                return(extdat)
            })

            #############

            cond1 <- params$out.data$sp.avrg
            cond2 <- params$out.data$format == "cdt"
            cond3 <- params$out.data$format == "cpt"
            cond4 <- params$type.extract %in% c('rect', 'poly')
            cond5 <- params$type.extract %in% c('point', 'mpoint', 'mpoly')
            cond6 <- (cond3 & cond5) | (cond3 & cond1)
            cond7 <- cond3 & !cond1 & cond4

            if(cond1 | cond2 | cond6) ret <- do.call(rbind, ret)

            if(cond2){
                caption <- c('ID', 'LON', 'DATE/LAT')
                xhead <- t(headinfo)
                ret <- round(ret, 3)
                ret[is.na(ret)] <- missval
                ret <- rbind(xhead, ret)
                ret <- cbind(c(caption, dates), ret)
                writeFiles(ret, outputDIR)
            }

            if(cond6){
                ret <- round(ret, 3)
                ret[is.na(ret)] <- missval
                cptIn <- list(data = ret, date = dates, stninfo = headinfo,
                              varid = varInfo$name, units = varInfo$units,
                              missval = missval)
                cptOut <- do.call(CPT.convertStationData, cptIn)
                cat(cptOut, file = outputDIR)
            }

            if(cond7){
                ### mthd1
                ret <- lapply(ret, function(x){
                    if(is.null(dim(x)) & is.na(x[1]))
                        x <- array(missval, dim(headinfo[[1]]$z))
                    return(x)
                })
                gridinfo <- headinfo[[1]][c('x', 'y')]
                cptIn <- list(data = ret, date = dates, gridinfo = gridinfo,
                              varid = varInfo$name, units = varInfo$units,
                              missval = missval)
                cptOut <- do.call(CPT.convertGridData, cptIn)
                cat(cptOut, file = outputDIR)

                ### mthd2
                # ix <- do.call(c, ret)
                # ix <- ix[!is.na(x)]
                # ix <- order(ix)
                # dates <- dates[ix]
                # ret <- read.table(outputDIR)
                # ret <- ret[ix, , drop = FALSE]
                # ret <- lapply(seq(nrow(ret)), function(j){
                #     x <- as.numeric(ret[j, ])
                #     dim(x) <- dim(headinfo[[1]]$z)
                #     x[is.na(x)] <- missval
                #     x
                # })
                # gridinfo <- headinfo[[1]][c('x', 'y')]
                # cptIn <- list(data = ret, date = dates, gridinfo = gridinfo,
                #               varid = varInfo$name, units = varInfo$units,
                #               missval = missval)
                # cptOut <- do.call(CPT.convertGridData, cptIn)
                # cat(cptOut, file = outputDIR)
            }
        }
        Insert.Messages.Out(lang.dlg[['message']][['21']], TRUE, "s", GUI)
    }else{
        ## cdtdataset
        Insert.Messages.Out("Not implemented yet", TRUE, "e", GUI)
    }

    return(0)
}
