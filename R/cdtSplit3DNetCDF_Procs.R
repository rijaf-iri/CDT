
get.netcdf.time <- function(time.dim){
    temps <- time.dim$units
    if(grepl("since", temps, ignore.case = TRUE)){
        tmp <- trimws(strsplit(temps, " ")[[1]])
        tmp <- tmp[(grep("since", tmp) + 1):length(tmp)]
        tmp <- paste0(tmp, collapse = " ")
        tmp <- trimws(gsub("[^0-9]", " ", tmp))
        tmp <- trimws(strsplit(tmp, " ")[[1]])
        tmp <- tmp[tmp != ""]
        if(length(tmp) > 6)
            tmp <- c(tmp[1:5], paste0(tmp[6], ".", tmp[7]))
        tmp <- as.numeric(tmp)
        tmp <- as.list(tmp)
        nmt <- c('year', 'month', 'day', 'hour', 'min', 'sec')
        names(tmp) <- nmt[seq(length(tmp))]
        origin <- do.call(ISOdate, as.list(tmp))

        if(grepl("second", temps, ignore.case = TRUE)){
            units <- "seconds"
            daty <- as.POSIXct(time.dim$vals, origin = origin, tz = "GMT")
        }else if(grepl("minute", temps, ignore.case = TRUE)){
            units <- "minutes"
            daty <- as.POSIXct(time.dim$vals * 60, origin = origin, tz = "GMT")
        }else if(grepl("hour", temps, ignore.case = TRUE)){
            units <- "hours"
            daty <- as.POSIXct(time.dim$vals * 3600, origin = origin, tz = "GMT")
        }else if(grepl("day", temps, ignore.case = TRUE)){
            units <- "days"
            daty <- as.Date(time.dim$vals, origin = origin)
        }else if(grepl("month", temps, ignore.case = TRUE)){
            units <- "months"
            foo <- function(n) seq(as.Date(origin), by = paste(n, "months"), length = 2)[2]
            daty <- do.call(c, lapply(time.dim$vals, foo))
        }else{
            origin <- NULL
            units <- temps
            daty <- time.dim$vals
            warning(paste("Unknown time units:", temps), "\n")
        }
    }else{
        if(grepl("julian", temps, ignore.case = TRUE)){
            units <- "julian"
            # Unix Time
            daty <- as.POSIXct((time.dim$vals - 2440587.5) * 86400, origin = "1970-01-01", tz = "GMT")
        }else{
            origin <- NULL
            units <- temps
            daty <- time.dim$vals
            warning(paste("Unknown time units:", temps), "\n")
        }
    }
    c(time.dim[c('name', 'len')], list(origin = origin, units = units, vals = daty))
}

###################################################################

split_3d.netcdf_writeNC <- function(){
    Insert.Messages.Out(.cdtData$GalParams[['message']][['4']], TRUE, "i")

    ncpars <- .cdtData$GalParams$ncpars
    outdir <- .cdtData$GalParams$output
    outdir <- file.path(outdir, "Splitted_NetCDF")
    dir.create(outdir, showWarnings = FALSE, recursive = TRUE)

    nc.dir <- dirname(.cdtData$GalParams$ncdf$file)
    if(.cdtData$GalParams$nbfile == "several"){
        error.msg <- .cdtData$GalParams[['message']][['2']]
        nc.format <- .cdtData$GalParams$ncdf$format
        ncfiles <- ncFilesInfoSeq(nc.dir, nc.format, error.msg)
    }else{
        ncfiles <- basename(.cdtData$GalParams$ncdf$file)
    }

    ret <- lapply(seq_along(ncfiles), function(jj){
        nc <- nc_open(file.path(nc.dir, ncfiles[jj]))
        time.dim <- nc$dim[[ncpars$dim[[3]]$name]]
        var.val <- ncvar_get(nc, varid = ncpars$var$name, collapse_degen = FALSE)
        nc_close(nc)

        if((ncpars$var$ndims - 3) > 0){
            indices <- lapply(1:(ncpars$var$ndims - 3), function(i){
                vdim <- ncpars$dim[[i + 3]]
                list(name = which(ncpars$var.dim == vdim$name), ix = vdim$sel)
            })
            noms <- sapply(indices, "[[", "name")
            indices <- lapply(indices, "[[", "ix")
            names(indices) <- noms
            var.val <- R.utils::extract(var.val, indices = indices, drop = TRUE)
            ncpars$var.dim <- ncpars$var.dim[-noms]
        }

        time.name <- which(ncpars$var.dim == ncpars$dim[[3]]$name)
        ret <- get.netcdf.time(time.dim)
        daty <- ret$vals
        if(!is.null(ret$origin)){
            rdaty <- lapply(c("%Y", "%m", "%d", "%H", "%M", "%S"), function(v) format(daty, v))
            for(it in 6:1){
                if(all.equal.elements(rdaty[[it]]))
                   rdaty[[it]] <- NULL
               else
                    break
            }
            daty <- do.call(paste0, rdaty)
        }

        dx <- do.call(ncdim_def, c(ncpars$dim[[1]][c('name', 'units', 'vals')],
                                   list(longname = "Longitude")))
        dy <- do.call(ncdim_def, c(ncpars$dim[[2]][c('name', 'units', 'vals')],
                                   list(longname = "Latitude")))
        outnc <- ncvar_def(ncpars$var$name, ncpars$var$units, list(dx, dy), -9999,
                           ncpars$var$longname, ncpars$var$prec, compression = 9)

        for(i in seq_along(daty)){
            indices <- list(i)
            names(indices) <- time.name
            tmp <- R.utils::extract(var.val, indices = indices, drop = TRUE)
            if(ncpars$rev) tmp <- t(tmp)
            tmp <- tmp[ncpars$dim[[1]]$order, ncpars$dim[[2]]$order]
            tmp[is.na(tmp)] <- -9999

            ncfile <- paste0(ncpars$var$name, "_", daty[i], ".nc")
            ncpath <- file.path(outdir, ncfile)
            nc <- nc_create(ncpath, outnc)
            ncvar_put(nc, outnc, tmp)
            nc_close(nc)
        }

        return(0)
    })

    return(0)
}
