
cdtMerging <- function(stnData, ncInfo, xy.grid, params, variable,
                       demData, outdir, mask = NULL, GUI = TRUE)
{
    log.file <- file.path(outdir, "log_file.txt")
    ncinfo <- ncInfo$ncinfo
    mrgOpts <- merging.options()

    if(variable == "rain"){
        if(params$prec$from.data){
            dataprec <- ncinfo$varinfo$prec
        }else{
            dataprec <- params$prec$prec
        }
    }else{
        dataprec <- ncinfo$varinfo$prec
    }

    if(variable %in% c("ugrd", "vgrd")){
        missval <- -9999
    }else{
        missval <- -99
    }

    datainfo <- mrgOpts$netCDFDataDef[[variable]]
    params$MRG$limits <- c(datainfo$min, datainfo$max)

    ##################

    dx <- ncdf4::ncdim_def("Lon", "degree_east", xy.grid$lon)
    dy <- ncdf4::ncdim_def("Lat", "degree_north", xy.grid$lat)
    shuffle <- if(dataprec %in% c("integer", "short")) TRUE else FALSE
    grd.nc.out <- ncdf4::ncvar_def(datainfo$name, datainfo$units, list(dx, dy), missval,
                                   longname = datainfo$longname, prec = dataprec,
                                   shuffle = shuffle, compression = 9)

    ##################

    newgrid <- defSpatialPixels(xy.grid)

    nmin <- ceiling(params$MRG$pass * params$interp$nmin)
    nmax <- ceiling(params$MRG$pass * params$interp$nmax)
    nmax <- ifelse(nmax - nmin < 2, nmax + 2, nmax)
    params$interp$nmin <- nmin
    params$interp$nmax <- nmax

    if(!params$interp$vargrd){
        maxdist <- params$MRG$pass * params$interp$maxdist
        params$interp$maxdist <- maxdist
    }else{
        bx <- diff(sapply(stnData[c('lon', 'lat')], range))
        dg <- sqrt(bx[1]^2 + bx[2]^2)
        dg <- 0.1 * dg + 2
        params$interp$maxdist <- params$MRG$pass * dg
    }

    locations.stn <- as.data.frame(stnData[c('lon', 'lat')])
    sp::coordinates(locations.stn) <- c('lon', 'lat')
    ijs <- sp::over(locations.stn, newgrid)
    locations.stn$stn <- rep(NA, length(locations.stn))

    xy.data <- defSpatialPixels(ncinfo[c('lon', 'lat')])
    is.regridNCDF <- is.diffSpatialPixelsObj(newgrid, xy.data, tol = 1e-07)
    gridBuffer <- list(igrid = rep(TRUE, length(newgrid)), icoarse = NULL, coarsegrid = NULL)

    ##################

    is.auxvar <- rep(FALSE, 5)
    formuleRK <- NULL
    if(params$MRG$method == "RK"){
        auxvar <- c('dem', 'slp', 'asp', 'alon', 'alat')
        is.auxvar <- unlist(params$auxvar[1:5])
        if(any(is.auxvar)){
            formuleRK <- stats::formula(paste0('stn', '~', 'grd', '+',
                                 paste(auxvar[is.auxvar], collapse = '+')))
        }else{
            formuleRK <- stats::formula(paste0('stn', '~', 'grd'))
        }

        if(is.auxvar['dem']) newgrid$dem <- c(demData$z)
        if(is.auxvar['slope'] | is.auxvar['aspect']){
            slpasp <- raster.slope.aspect(demData)
            if(is.auxvar['slope']) newgrid$slp <- c(slpasp$slope)
            if(is.auxvar['aspect']) newgrid$asp <- c(slpasp$aspect)
        }
        if(is.auxvar['lon']) newgrid$alon <- newgrid@coords[, 'lon']
        if(is.auxvar['lat']) newgrid$alat <- newgrid@coords[, 'lat']
        if(any(is.auxvar))
            locations.stn@data <- newgrid@data[ijs, , drop = FALSE]
    }

    ##################

    args <- methods::formalArgs(cdtMerging)
    for(v in args) assign(v, get(v), envir = environment())

    parsL <- doparallel.cond(length(ncInfo$ncfiles) > 20)
    ret <- cdt.foreach(seq_along(ncInfo$ncfiles), parsL, GUI,
                       progress = TRUE, FUN = function(jj)
    {
        if(ncInfo$exist[jj]){
            nc <- ncdf4::nc_open(ncInfo$ncfiles[jj])
            nc.val <- ncdf4::ncvar_get(nc, varid = ncinfo$varid)
            ncdf4::nc_close(nc)
            nc.val <- transposeNCDFData(nc.val, ncinfo)
        }else{
            msg <- paste(ncInfo$dates[jj], ":", "no NetCDF data",
                         "|", "no file generated", "\n")
            cat(msg, file = log.file, append = TRUE)
            return(-1)
        }

        if(all(is.na(nc.val))){
            msg <- paste(ncInfo$dates[jj], ":", "all NetCDF data are missing",
                         "|", "no file generated", "\n")
            cat(msg, file = log.file, append = TRUE)
            return(-1)
        }

        ######
        if(is.regridNCDF){
            nc.val <- c(ncinfo[c('lon', 'lat')], list(z = nc.val))
            nc.val <- cdt.interp.surface.grid(nc.val, xy.grid)
            nc.val <- nc.val$z
        }
        newgrid$grd <- c(nc.val)

        donne.stn <- stnData$data[which(stnData$date == ncInfo$dates[jj]), , drop = FALSE]
        if(nrow(donne.stn) == 0){
            msg <- paste(ncInfo$dates[jj], ":", "no station data", "|",
                         "No merging performed, output equals to the input NetCDF data", "\n")
            cat(msg, file = log.file, append = TRUE)

            write.merging.output(nc.val, grd.nc.out, outdir, ncInfo$dates[jj],
                                 params$period, params$output$format, missval, mask)
            return(0)
        }

        locations.stn$stn <- as.numeric(donne.stn[1, ])
        noNA <- !is.na(locations.stn$stn)
        locations.stn <- locations.stn[noNA, ]
        donne.len <- length(locations.stn)

        if(donne.len == 0){
            msg <- paste(ncInfo$dates[jj], ":", "no station data", "|",
                         "No merging performed, output equals to the input NetCDF data", "\n")
            cat(msg, file = log.file, append = TRUE)

            write.merging.output(nc.val, grd.nc.out, outdir, ncInfo$dates[jj],
                                 params$period, params$output$format, missval, mask)
            return(0)
        }

        if(donne.len > 0 & donne.len < mrgOpts$mrgMinNumberSTN){
            msg <- paste(ncInfo$dates[jj], ":", "not enough station data", "|",
                         "No merging performed, output equals to the input NetCDF data", "\n")
            cat(msg, file = log.file, append = TRUE)

            write.merging.output(nc.val, grd.nc.out, outdir, ncInfo$dates[jj],
                                 params$period, params$output$format, missval, mask)
            return(0)
        }

        if(params$MRG$method == "RK" & any(is.auxvar)){
            loc.data <- !is.na(locations.stn@data)
            loc.data <- split(loc.data, col(loc.data))
            nna <- Reduce("&", loc.data)
            if(length(which(nna)) < mrgOpts$rkMinNumberSTN){
                msg <- paste(ncInfo$dates[jj], ":", "not enough spatial points data", "|",
                             "No merging performed, output equals to the input NetCDF data", "\n")
                cat(msg, file = log.file, append = TRUE)

                write.merging.output(nc.val, grd.nc.out, outdir, ncInfo$dates[jj],
                                     params$period, params$output$format, missval, mask)
                return(0)
            }
        }

        ######
        if(mrgOpts$addCoarseGrid){
            fileGridBuffer <- ""
            if(mrgOpts$saveGridBuffer){
                fileGridBuffer <- file.path(outdir, "GRID_BUFFER", paste0("grid_buffer_", ncInfo$dates[jj], ".rds"))
            }
            gridBuffer <- create_grid_buffer(locations.stn, newgrid, mrgOpts$saveGridBuffer, fileGridBuffer)
        }

        out.mrg <- merging.functions(locations.stn, newgrid, params, formuleRK, 
                                     ncInfo$dates[jj], outdir, mrgOpts, gridBuffer)

        write.merging.output(out.mrg, grd.nc.out, outdir, ncInfo$dates[jj],
                             params$period, params$output$format, missval, mask)
        return(0)
    })

    ret <- do.call(c, ret)
    if(any(ret == -1)) return(-1)
    return(0)
}

###############################

write.merging.output <- function(out.mrg, grd.nc.out, outdir, nc.date,
                                 timestep, nc.format, missval, mask)
{
    if(!is.null(mask)) out.mrg[is.na(mask)] <- missval

    out.mrg[is.na(out.mrg)] <- missval

    year <- substr(nc.date, 1, 4)
    month <- substr(nc.date, 5, 6)
    if(timestep == 'daily'){
        day <- substr(nc.date, 7, 8)
        ncfile <- sprintf(nc.format, year, month, day)
    }else if(timestep %in% c('pentad', 'dekadal')){
        pdk <- substr(nc.date, 7, 7)
        ncfile <- sprintf(nc.format, year, month, pdk)
    }else if(timestep %in% c('minute', 'hourly')){
        day <- substr(nc.date, 7, 8)
        hour <- substr(nc.date, 9, 10)
        if(timestep == 'minute'){
            mn <- substr(nc.date, 11, 12)
            ncfile <- sprintf(nc.format, year, month, day, hour, mn)
        }else{
            ncfile <- sprintf(nc.format, year, month, day, hour)
        }
    }else{
        ncfile <- sprintf(nc.format, year, month)
    }

    out.nc.file <- file.path(outdir, 'DATA', ncfile)
    nc <- ncdf4::nc_create(out.nc.file, grd.nc.out)
    ncdf4::ncvar_put(nc, grd.nc.out, out.mrg)
    ncdf4::nc_close(nc)

    return(0)
}
