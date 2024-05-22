
extract.jra_nrt.grib.data <- function(GUI = TRUE){
    Insert.Messages.Out(.cdtData$GalParams[['message']][['1']], TRUE, "i")
    jra_prod <- .cdtData$GalParams$jra.prod
    if(jra_prod == "jra3q"){
        wgrib2 <- if(WindowsOS()) 'wgrib2.exe' else 'wgrib2'
        wgrib_exe <- file.path(.cdtDir$dirLocal, 'wgrib2', wgrib2)
        wgrib_msg <- "5-2"
        coords_file <- "JRA3Q_Coords.rds"
        opts_file <- "jra3q_nrt_options.csv"
        minhour <- 1
        data.name <- "JRA-3Q NRT Hourly"
    }else{
        wgrib <- if(WindowsOS()) 'wgrib.exe' else 'wgrib'
        wgrib_exe <- file.path(.cdtDir$dirLocal, 'wgrib', 'wgrib')
        wgrib_msg <- "5-1"
        coords_file <- "JRA55_Coords.rds"
        opts_file <- "jra55_nrt_options.csv"
        minhour <- 3
        data.name <- "JRA-55 NRT 3 Hourly"
    }

    if(!file.exists(wgrib_exe)){
        Insert.Messages.Out(.cdtData$GalParams[['message']][[wgrib_msg]], TRUE, "e", GUI)
        return(-2)
    }

    ###########
    crd_file <- file.path(.cdtDir$Root, "data", coords_file)
    crd_gauss <- readRDS(crd_file)
    crd_gauss$lon <- ((crd_gauss$lon + 180) %% 360) - 180
    xo <- order(crd_gauss$lon)
    yo <- order(crd_gauss$lat)
    xlon <- crd_gauss$lon[xo]
    xlat <- crd_gauss$lat[yo]

    ix <- xlon >= .cdtData$GalParams$bbox$minlon & xlon <= .cdtData$GalParams$bbox$maxlon
    iy <- xlat >= .cdtData$GalParams$bbox$minlat & xlat <= .cdtData$GalParams$bbox$maxlat

    if(!any(ix) | !any(iy)){
        Insert.Messages.Out(.cdtData$GalParams[['message']][['4']], TRUE, "e", GUI)
        return(-2)
    }

    xycoords <- list(xlon = xlon[ix], xlat = xlat[iy])
    bbox <- c(.cdtData$GalParams$bbox, xycoords)

    ###########
    rtimes <- seq.format.date.time('hourly', .cdtData$GalParams$date.range, minhour)
    if(is.null(rtimes)) return(-2)
    yrmo <- format(rtimes, '%Y%m')
    ymdh <- format(rtimes, '%Y%m%d%H')

    ############
    opts <- get_reanalysis.variables(opts_file)
    opts <- opts[[.cdtData$GalParams$var]]

    filenames <- paste0(opts$grib_path, ".", ymdh)
    gribfiles <- file.path(.cdtData$GalParams$dir.grib, filenames)
    gribexist <- file.exists(gribfiles)

    if(!any(gribexist)){
        Insert.Messages.Out(.cdtData$GalParams[['message']][['6']], TRUE, "e", GUI)
        msg <- paste(.cdtData$GalParams[['message']][['7']], opts$grib_path)
        Insert.Messages.Out(msg, TRUE, "e", GUI)
        return(-2)
    }

    gribfiles <- gribfiles[gribexist]
    ymdh <- ymdh[gribexist]
    ncfiles <- sprintf(paste0(.cdtData$GalParams$var, "_%s.nc"), ymdh)

    pars <- list(wgrib = wgrib_exe, GUI = GUI)
    convert_units <- if(is.null(opts$units_fun)) NULL else list(fun = opts$units_fun, args = opts$units_args)
    ncpars <- list(name = opts$nc_name, units = opts$nc_units, longname = opts$nc_longname,
                   prec = "float", missval = -9999, convert = convert_units)
    gribpars <- list(var = opts$grib_name, hgt = opts$grib_height, srch = opts$grib_search)

    ############

    extrdir <- paste0('Extracted_', .cdtData$GalParams$var)
    extrdir <- file.path(.cdtData$GalParams$dir2save, extrdir)
    dir.create(extrdir, showWarnings = FALSE, recursive = TRUE)
    ncfiles <- file.path(extrdir, ncfiles)

    parsL <- doparallel.cond(length(gribfiles) > 30)
    ret <- cdt.foreach(seq_along(gribfiles), parsL, GUI, progress = TRUE, FUN = function(g){
        dest <- gribfiles[g]
        ncfl <- ncfiles[g]
        xx <- basename(dest)

        if(length(gribpars$var) == 1){
            if(jra_prod == "jra3q"){
                don <- jra3q_nrt.extract.grib2(dest, gribpars$var, gribpars$hgt,
                                               gribpars$srch, bbox, pars$wgrib)
            }else{
                don <- jra55_nrt.extract.grib(dest, gribpars$var, gribpars$hgt, pars$wgrib)
            }
            dat <- don$data
            msg <- don$msg

            if(is.null(dat)){
                Insert.Messages.Out(msg, TRUE, "e", pars$GUI)
                return(xx)
            }
        }else{
            don <- lapply(seq_along(gribpars$var), function(j){
                if(jra_prod == "jra3q"){
                    jra3q_nrt.extract.grib2(dest, gribpars$var[j], gribpars$hgt[j],
                                            gribpars$srch[j], bbox, pars$wgrib)
                }else{
                    jra55_nrt.extract.grib(dest, gribpars$var[j], gribpars$hgt[j], pars$wgrib)
                }
            })
            dat <- lapply(don, '[[', 'data')
            msg <- lapply(don, '[[', 'msg')

            inull <- sapply(dat, is.null)
            if(any(inull)){
                msg <- msg[inull]
                for(j in seq_along(msg))
                    Insert.Messages.Out(msg[[j]], TRUE, "e", pars$GUI)

                return(xx)
            }

            dat <- list(x = dat[[1]]$x, y = dat[[1]]$y,
                        z = lapply(dat, '[[', 'z'))
        }

        if(jra_prod == "jra3q"){
            ret <- jra3q_nrt.format.grib2(dat, ncpars, ncfl)
        }else{
            ret <- jra55_nrt.format.grib(dat, bbox, ncpars, ncfl)
        }
        if(ret == 0) xx <- NULL

        return(xx)
    })
    inull <- !sapply(ret, is.null)

    if(any(inull)){
        data.files <- paste(.cdtData$GalParams[['message']][['8']], unlist(ret[inull]))
        xx <- paste(data.name, paste(data.files, collapse = "\n"), sep = "\n")

        if(GUI){
            containertab <- Display_Output_Console_Tab(xx, data.name, cat)
            ntab <- update.OpenTabs('ctxt', containertab)
            tkselect(.cdtEnv$tcl$main$tknotes, ntab)
        }else{
            cat(xx, "\n")
        }
    }

    return(0)
}
