
cdtMergingWind <- function(stnData, ncInfo, xy.grid, params,
                           demData, outdir, mask = NULL, GUI = TRUE)
{
    log.file <- file.path(outdir, "log_file.txt")
    ncinfo <- ncInfo$ncinfo
    mrgOpts <- merging.options()

    datainfo <- mrgOpts$netCDFDataDef[c('ugrd', 'vgrd')]
    params$MRG$limits <- c(datainfo$ugrd$min, datainfo$ugrd$max)

    dx <- ncdf4::ncdim_def("Lon", "degree_east", xy.grid$lon)
    dy <- ncdf4::ncdim_def("Lat", "degree_north", xy.grid$lat)

    missval <- -9999
    grd.nc.out <- lapply(datainfo, function(x){
            ncdf4::ncvar_def(x$name, x$units, list(dx, dy), missval,
                             x$longname, ncinfo$U$varinfo$prec, compression = 9)
    })

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

    if(params$one.ncdf){
        nc_dates <- ncInfo$UV$dates
        nc_formats <- params$output$format.UV
    }else{
        nc_dates <- ncInfo$U$dates
        nc_formats <- c(params$output$format.U, params$output$format.V)
    }

    ##################

    args <- methods::formalArgs(cdtMergingWind)
    for(v in args) assign(v, get(v), envir = environment())

    parsL <- doparallel.cond(length(nc_dates) > 20)
    ret <- cdt.foreach(seq_along(nc_dates), parsL, GUI,
                       progress = TRUE, FUN = function(jj)
    {
        if(params$one.ncdf){
            if(ncInfo$UV$exist[jj]){
                nc <- ncdf4::nc_open(ncInfo$UV$ncfiles[jj])
                nc.valU <- ncdf4::ncvar_get(nc, varid = ncinfo$varidU)
                nc.valV <- ncdf4::ncvar_get(nc, varid = ncinfo$varidV)
                ncdf4::nc_close(nc)
                nc.valU <- transposeNCDFData(nc.valU, ncinfo$UV)
                nc.valV <- transposeNCDFData(nc.valV, ncinfo$UV)
            }else{
                msg <- paste(nc_dates[jj], ":", "no NetCDF data",
                             "|", "no file generated", "\n")
                cat(msg, file = log.file, append = TRUE)
                return(-1)
            }
        }else{
            nc <- ncdf4::nc_open(ncInfo$U$ncfiles[jj])
            nc.valU <- ncdf4::ncvar_get(nc, varid = ncinfo$varidU)
            ncdf4::nc_close(nc)
            nc.valU <- transposeNCDFData(nc.valU, ncinfo$U)

            nc <- ncdf4::nc_open(ncInfo$V$ncfiles[jj])
            nc.valV <- ncdf4::ncvar_get(nc, varid = ncinfo$varidV)
            ncdf4::nc_close(nc)
            nc.valV <- transposeNCDFData(nc.valV, ncinfo$V)
        }

        if(all(is.na(nc.valU))){
            msg <- paste(nc_dates[jj], ":", "all NetCDF U-component data are missing",
                         "|", "no file generated", "\n")
            cat(msg, file = log.file, append = TRUE)
            return(-1)
        }

        if(all(is.na(nc.valV))){
            msg <- paste(nc_dates[jj], ":", "all NetCDF V-component data are missing",
                         "|", "no file generated", "\n")
            cat(msg, file = log.file, append = TRUE)
            return(-1)
        }

        ######
        if(is.regridNCDF){
            nc.valU <- c(ncinfo[c('lon', 'lat')], list(z = nc.valU))
            nc.valU <- cdt.interp.surface.grid(nc.valU, xy.grid)
            nc.valU <- nc.valU$z

            nc.valV <- c(ncinfo[c('lon', 'lat')], list(z = nc.valV))
            nc.valV <- cdt.interp.surface.grid(nc.valV, xy.grid)
            nc.valV <- nc.valV$z
        }

        ########################
        newgridU <- newgrid
        locations.U <- locations.stn
        newgridU$grd <- c(nc.valU)
        donne.stn <- stnData$U[which(stnData$date == nc_dates[jj]), , drop = FALSE]

        if(nrow(donne.stn) == 0){
            msg <- paste(nc_dates[jj], ":", "no station U-component data", "|",
                         "No merging performed, output equals to the input NetCDF data", "\n")
            cat(msg, file = log.file, append = TRUE)

            write.mergingwind.output(nc.valU, nc.valV, grd.nc.out, outdir, nc_dates[jj],
                                     params$period, nc_formats, missval, mask)
            return(0)
        }

        locations.U$stn <- as.numeric(donne.stn[1, ])
        noNA <- !is.na(locations.U$stn)
        locations.U <- locations.U[noNA, ]
        donne.len <- length(locations.U)

        if(donne.len == 0){
            msg <- paste(nc_dates[jj], ":", "no station U-component data", "|",
                         "No merging performed, output equals to the input NetCDF data", "\n")
            cat(msg, file = log.file, append = TRUE)

            write.mergingwind.output(nc.valU, nc.valV, grd.nc.out, outdir, nc_dates[jj],
                                     params$period, nc_formats, missval, mask)
            return(0)
        }

        if(donne.len > 0 & donne.len < mrgOpts$mrgMinNumberSTN){
            msg <- paste(nc_dates[jj], ":", "not enough station U-component data", "|",
                         "No merging performed, output equals to the input NetCDF data", "\n")
            cat(msg, file = log.file, append = TRUE)

            write.mergingwind.output(nc.valU, nc.valV, grd.nc.out, outdir, nc_dates[jj],
                                     params$period, nc_formats, missval, mask)
            return(0)
        }

        if(params$MRG$method == "RK" & any(is.auxvar)){
            loc.data <- !is.na(locations.U@data)
            loc.data <- split(loc.data, col(loc.data))
            nna <- Reduce("&", loc.data)
            if(length(which(nna)) < mrgOpts$rkMinNumberSTN){
                msg <- paste(nc_dates[jj], ":", "not enough spatial points data for U-component", "|",
                             "No merging performed, output equals to the input NetCDF data", "\n")
                cat(msg, file = log.file, append = TRUE)

                write.mergingwind.output(nc.valU, nc.valV, grd.nc.out, outdir, nc_dates[jj],
                                         params$period, nc_formats, missval, mask)
                return(0)
            }
        }

        ######
        if(mrgOpts$addCoarseGrid){
            fileGridBuffer <- ""
            if(mrgOpts$saveGridBuffer){
                fileGridBuffer <- file.path(outdir, "GRID_BUFFER", paste0("grid_buffer_", nc_dates[jj], ".rds"))
            }
            gridBuffer <- create_grid_buffer(locations.U, newgridU, mrgOpts$saveGridBuffer, fileGridBuffer)
        }

        if(params$interp$method == "okr") params$interp$vgm_dir <- "VARIOGRAM_U"

        out.U <- merging.functions(locations.U, newgridU, params, formuleRK,
                                   nc_dates[jj], outdir, mrgOpts, gridBuffer)
        ########################
        newgridV <- newgrid
        locations.V <- locations.stn
        newgridV$grd <- c(nc.valV)
        donne.stn <- stnData$V[which(stnData$date == nc_dates[jj]), , drop = FALSE]

        if(nrow(donne.stn) == 0){
            msg <- paste(nc_dates[jj], ":", "no station V-component data", "|",
                         "No merging performed, output equals to the input NetCDF data", "\n")
            cat(msg, file = log.file, append = TRUE)

            write.mergingwind.output(nc.valU, nc.valV, grd.nc.out, outdir, nc_dates[jj],
                                     params$period, nc_formats, missval, mask)
            return(0)
        }

        locations.V$stn <- as.numeric(donne.stn[1, ])
        noNA <- !is.na(locations.V$stn)
        locations.V <- locations.V[noNA, ]
        donne.len <- length(locations.V)

        if(donne.len == 0){
            msg <- paste(nc_dates[jj], ":", "no station V-component data", "|",
                         "No merging performed, output equals to the input NetCDF data", "\n")
            cat(msg, file = log.file, append = TRUE)

            write.mergingwind.output(nc.valU, nc.valV, grd.nc.out, outdir, nc_dates[jj],
                                     params$period, nc_formats, missval, mask)
            return(0)
        }

        if(donne.len > 0 & donne.len < mrgOpts$mrgMinNumberSTN){
            msg <- paste(nc_dates[jj], ":", "not enough station U-component data", "|",
                         "No merging performed, output equals to the input NetCDF data", "\n")
            cat(msg, file = log.file, append = TRUE)

            write.mergingwind.output(nc.valU, nc.valV, grd.nc.out, outdir, nc_dates[jj],
                                     params$period, nc_formats, missval, mask)
            return(0)
        }

        if(params$MRG$method == "RK" & any(is.auxvar)){
            loc.data <- !is.na(locations.V@data)
            loc.data <- split(loc.data, col(loc.data))
            nna <- Reduce("&", loc.data)
            if(length(which(nna)) < mrgOpts$rkMinNumberSTN){
                msg <- paste(nc_dates[jj], ":", "not enough spatial points data for V-component", "|",
                             "No merging performed, output equals to the input NetCDF data", "\n")
                cat(msg, file = log.file, append = TRUE)

                write.mergingwind.output(nc.valU, nc.valV, grd.nc.out, outdir, nc_dates[jj],
                                         params$period, nc_formats, missval, mask)
                return(0)
            }
        }

        if(params$interp$method == "okr") params$interp$vgm_dir <- "VARIOGRAM_V"

        out.V <- merging.functions(locations.V, newgridV, params, formuleRK,
                                   nc_dates[jj], outdir, mrgOpts, gridBuffer)
        ########

        write.mergingwind.output(out.U, out.V, grd.nc.out, outdir, nc_dates[jj],
                                 params$period, nc_formats, missval, mask)
        return(0)
    })

    ret <- do.call(c, ret)
    if(any(ret == -1)) return(-1)
    return(0)
}

###############################

write.mergingwind.output <- function(out.U, out.V, grd.nc.out, outdir, nc.date,
                                     timestep, nc_formats, missval, mask)
{
    one_ncdf <- length(nc_formats) == 1

    if(!is.null(mask)){
        out.U[is.na(mask)] <- missval
        out.V[is.na(mask)] <- missval
    }
    out.U[is.na(out.U)] <- missval
    out.V[is.na(out.V)] <- missval

    year <- substr(nc.date, 1, 4)
    month <- substr(nc.date, 5, 6)
    if(timestep == 'daily'){
        day <- substr(nc.date, 7, 8)
        if(one_ncdf){
            ncfile <- sprintf(nc_formats, year, month, day)
        }else{
            ncfileU <- sprintf(nc_formats[1], year, month, day)
            ncfileV <- sprintf(nc_formats[2], year, month, day)
        }
    }else if(timestep %in% c('pentad', 'dekadal')){
        pdk <- substr(nc.date, 7, 7)
        if(one_ncdf){
            ncfile <- sprintf(nc_formats, year, month, pdk)
        }else{
            ncfileU <- sprintf(nc_formats[1], year, month, pdk)
            ncfileV <- sprintf(nc_formats[2], year, month, pdk)
        }
    }else if(timestep %in% c('minute', 'hourly')){
        day <- substr(nc.date, 7, 8)
        hour <- substr(nc.date, 9, 10)
        if(timestep == 'minute'){
            mn <- substr(nc.date, 11, 12)
            if(one_ncdf){
                ncfile <- sprintf(nc_formats, year, month, day, hour, mn)
            }else{
                ncfileU <- sprintf(nc_formats[1], year, month, day, hour, mn)
                ncfileV <- sprintf(nc_formats[2], year, month, day, hour, mn)
            }
        }else{
            if(one_ncdf){
                ncfile <- sprintf(nc_formats, year, month, day, hour)
            }else{
                ncfileU <- sprintf(nc_formats[1], year, month, day, hour)
                ncfileV <- sprintf(nc_formats[2], year, month, day, hour)
            }
        }
    }else{
        if(one_ncdf){
            ncfile <- sprintf(nc_formats, year, month)
        }else{
            ncfileU <- sprintf(nc_formats[1], year, month)
            ncfileV <- sprintf(nc_formats[2], year, month)
        }
    }

    if(one_ncdf){
        fileUV <- file.path(outdir, 'DATA', ncfile)
        nc <- ncdf4::nc_create(fileUV, grd.nc.out)
        ncdf4::ncvar_put(nc, grd.nc.out[[1]], out.U)
        ncdf4::ncvar_put(nc, grd.nc.out[[2]], out.V)
        ncdf4::nc_close(nc)
    }else{
        fileU <- file.path(outdir, 'UGRD', ncfileU)
        nc <- ncdf4::nc_create(fileU, grd.nc.out[[1]])
        ncdf4::ncvar_put(nc, grd.nc.out[[1]], out.U)
        ncdf4::nc_close(nc)

        fileV <- file.path(outdir, 'VGRD', ncfileV)
        nc <- ncdf4::nc_create(fileV, grd.nc.out[[2]])
        ncdf4::ncvar_put(nc, grd.nc.out[[2]], out.V)
        ncdf4::nc_close(nc)
    }

    return(0)
}
