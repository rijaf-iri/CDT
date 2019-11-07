
jra55.download.jra.kishou <- function(GalParams, nbfile = 1, GUI = TRUE, verbose = TRUE){
    on.exit(curl::handle_reset(handle))

    start <- GalParams$date.range[paste0('start.', c('year', 'mon', 'day', 'hour'))]
    start <- jra55.start.end.time(start)
    end <- GalParams$date.range[paste0('end.', c('year', 'mon', 'day', 'hour'))]
    end <- jra55.start.end.time(end)

    down.3hrs <- seq(start, end, "3 hours")
    dir.month <- format(down.3hrs, "%Y%m")
    hours <- format(down.3hrs, "%Y%m%d%H")
    ftpfiles <- paste0("minmax_surf.", hours)

    ######################

    ftpserver <- "ds.data.jma.go.jp"

    if(GalParams$var %in% c("tmax", "tmin")){
        ftpdir <- "JRA-55/Hist/Daily/minmax_surf"
        ftplink <- paste0("ftp://", ftpserver, "/", ftpdir, "/")

        varid <- c("tmax", "tmin")
        longname <- c("JRA55 3 Hourly Maximum temperature at 2 m above ground",
                      "JRA55 3 Hourly Minimum temperature at 2 m above ground")
    }

    ####
    grads.bin <- GalParams$path.exe
    if(WindowsOS()){
        if(!file.exists(grads.bin)) return(2)
    }else{
        pthgs <- split_path(grads.bin)
        if(length(pthgs) == 1){
            ret <- system(paste('which', grads.bin), intern = TRUE)
            if(length(ret) == 0) return(2)
            grads.bin <- ret
        }else{
            if(!file.exists(grads.bin)) return(2)
        }
    }

    ####
    pars <- list(grads.bin = grads.bin, varid = varid, longname = longname, bbox = GalParams$bbox)

    ######################

    data.name <- "JRA-55 3 Hourly"

    outdir <- file.path(GalParams$dir2save, "JRA55_GRIB_3Hr")
    if(GalParams$var %in% c("tmax", "tmin"))
        dirGRIB <- file.path(outdir, "Temperature", "GRIB")

    dir.create(dirGRIB, showWarnings = FALSE, recursive = TRUE)

    dirExNC <- file.path(outdir, "Temperature", "Extracted")
    if(GalParams$var %in% c("tmax", "tmin")){
        dir.create(file.path(dirExNC, varid[1]), showWarnings = FALSE, recursive = TRUE)
        dir.create(file.path(dirExNC, varid[2]), showWarnings = FALSE, recursive = TRUE)
    }

    gradsF <- c("minmax_surf.ctl", "minmax_surf.idx", "TL319.pdef")

    data.month <- unique(dir.month)
    data.down <- lapply(data.month, function(mo){
        ftpmon <- paste0(ftplink, mo, "/")
        ftpgrads <- paste0(ftpmon, gradsF)
        gribgrads <- file.path(dirGRIB, mo, gradsF)

        ftpdata <- paste0(ftpmon, ftpfiles[dir.month == mo])
        gribdata <- file.path(dirGRIB, mo, ftpfiles[dir.month == mo])

        ncff <- file.path(dirExNC, hours[dir.month == mo])

        list(grib = c(gribgrads, gribdata), ftp = c(ftpgrads, ftpdata), nc = ncff)
    })
    names(data.down) <- data.month

    urls <- lapply(data.down, "[[", "ftp")
    destfiles <- lapply(data.down, "[[", "grib")
    ncfiles <- lapply(data.down, "[[", "nc")

    ######################

    handle <- curl::new_handle()
    curl::handle_setopt(handle, username = GalParams$login$usr, password = GalParams$login$pwd)

    if(GalParams$var %in% c("tmax", "tmin"))
        download.fun <- jra55.download.minmax_surf

    ret <- cdt.download.data(urls, destfiles, ncfiles, nbfile, GUI,
                             verbose, data.name, download.fun,
                             handle = handle, pars = pars)

    return(ret)
}

#################################################################################

jra55.download.minmax_surf <- function(lnk, dest, ncfl, handle, pars){
    mon <- names(lnk)
    lnk <- lnk[[1]]
    dest <- dest[[1]]
    ncfl <- ncfl[[1]]

    dir.month <- dirname(dest[1])
    tmp <- file.path(dirname(dir.month), basename(dest[1]))
    ret <- try(curl::curl_download(lnk[1], tmp, handle = handle), silent = TRUE)

    xx <- paste("Unable to download minmax_surf.ctl for", mon)

    if(!inherits(ret, "try-error")){
        if(file.exists(tmp)){
            dir.create(dir.month, showWarnings = FALSE, recursive = TRUE)
            file.copy(tmp, dest[1], overwrite = TRUE)
            unlink(tmp)

            dc <- try(curl::curl_download(lnk[2], dest[2], handle = handle), silent = TRUE)
            if(inherits(dc, "try-error"))
                return(paste("Unable to download minmax_surf.idx for", mon))
            rm(dc)

            if(!file.exists(dest[3])){
                dc <- try(curl::curl_download(lnk[3], dest[3], handle = handle), silent = TRUE)
                if(inherits(dc, "try-error"))
                    return(paste("Unable to download TL319.pdef for", mon))
                rm(dc)
            }

            lnk <- lnk[-(1:3)]
            dest <- dest[-(1:3)]

            res <- lapply(seq_along(lnk), function(j){
                dc <- dest[j]
                if(!file.exists(dest[j]))
                    dc <- try(curl::curl_download(lnk[j], dest[j], handle = handle), silent = TRUE)
                out <- 1
                if(!inherits(dc, "try-error")){
                    grds <- try(jra55.grads.wraper.minmax_surf(dest[j], pars), silent = TRUE)
                    if(inherits(grds, "try-error")) return(out)
                    out <- jra55.create.nc.minmax_surf(dest[j], ncfl[j], pars)
                }
                return(out)
            })

            if(!all(unlist(res) == 0)){
                ix <- unlist(res) != 0
                xx <- paste0(paste0(basename(dest[ix]), collapse = "\n"), "\n")
            }else xx <- NULL
        }
    }

    return(xx)
}

jra55.grads.wraper.minmax_surf <- function(dest, pars){
    dir.month <- dirname(dest)
    file.ctl <- file.path(dir.month, "minmax_surf.ctl")

    dctl <- readLines(file.ctl)
    it <- grep("tdef", dctl, ignore.case = TRUE)
    dctl <- strsplit(dctl[it], " ")[[1]]
    dctl <- dctl[dctl != ""]
    nl <- as.numeric(dctl[2])
    orig <- as.POSIXct(dctl[4], tz = "GMT", format = "%HZ%d%b%Y")

    hrs.d <- strsplit(basename(dest), "\\.")[[1]][2]
    hrs.g <- format(seq(orig, length.out = nl, by = "3 hours"), "%Y%m%d%H")
    hr.t <- which(hrs.g == hrs.d)

    file.tx.nc <- file.path(dir.month, paste0(pars$varid[1], ".nc"))
    file.tn.nc <- file.path(dir.month, paste0(pars$varid[2], ".nc"))
    file.grads <- file.path(dir.month, "script.gs")

    file.gs <- c(
            paste0("'", "open ", file.ctl, "'"),
            paste0("'", "set t ", hr.t, "'"),
            paste0("'", "set sdfwrite -4d ", file.tx.nc, "'"),
            "'define var = tmax2m'",
            "'sdfwrite var'",
            paste0("'", "set sdfwrite -4d ", file.tn.nc, "'"),
            "'define var = tmin2m'",
            "'sdfwrite var'"
        )

    file.gs <- paste0(file.gs, collapse = "\n")
    cat(file.gs, file = file.grads, sep = "\n")
    log.file <- file.path(dir.month, "log.txt")
    system2(pars$grads.bin, paste("-lbcx", file.grads),
            stdout = log.file, stderr = log.file)

    invisible()
}

jra55.create.nc.minmax_surf <- function(dest, ncfl, pars){
    dir.month <- dirname(dest)

    file.tx.nc <- file.path(dir.month, paste0(pars$varid[1], ".nc"))
    if(!file.exists(file.tx.nc)){
        Insert.Messages.Out(paste(file.tx.nc, "not found!"), TRUE, "e")
        ret <- 1
    }else{
        ret <- jra55.write.nc.minmax_surf(file.tx.nc, ncfl, pars$bbox,
                                          pars$varid[1], pars$longname[1])
        unlink(file.tx.nc)
    }

    file.tn.nc <- file.path(dir.month, paste0(pars$varid[2], ".nc"))
    if(!file.exists(file.tn.nc)){
        Insert.Messages.Out(paste(file.tn.nc, "not found!"), TRUE, "e")
        ret <- 1
    }else{
        ret <- jra55.write.nc.minmax_surf(file.tn.nc, ncfl, pars$bbox,
                                          pars$varid[2], pars$longname[2])
        unlink(file.tn.nc)
    }

    return(ret)
}

jra55.write.nc.minmax_surf <- function(incfile, oncfile, bbox, varid, longname){
    temp <- try(jra55.read.nc.minmax_surf(incfile, bbox), silent = TRUE)
    if(inherits(temp, "try-error")) return(1)

    dx <- ncdf4::ncdim_def("Longitude", "degrees_east", temp$x)
    dy <- ncdf4::ncdim_def("Latitude", "degrees_north", temp$y)
    outnc <- ncdf4::ncvar_def(varid, "degC", list(dx, dy), -9999,
                              longname, "float", compression = 9)

    dirTT <- file.path(dirname(oncfile), varid)
    # temps <- basename(oncfile)
    temps <- format(temp$t, "%Y%m%d%H")
    fileout <- file.path(dirTT, paste0(varid, "_", temps, ".nc"))

    temp$z[is.na(temp$z) | is.nan(temp$z)] <- -9999
    nc <- ncdf4::nc_create(fileout, outnc)
    ncdf4::ncvar_put(nc, outnc, temp$z)
    ncdf4::nc_close(nc)

    return(0)
}

jra55.read.nc.minmax_surf <- function(ncfile, bbox){
    nc <- ncdf4::nc_open(ncfile)
    x <- nc$dim[[1]]$vals
    y <- nc$dim[[2]]$vals
    tm <- nc$dim[[4]]$vals
    units <- nc$dim[[4]]$units
    z <- ncdf4::ncvar_get(nc, varid = "var")
    ncdf4::nc_close(nc)
    x <- ((x + 180) %% 360) - 180
    orig <- paste0(strsplit(units, " ")[[1]][3:4], collapse = " ")
    orig <- as.POSIXct(orig, tz = "GMT", format = "%Y-%m-%d %H:%M")
    tm <- orig + tm * 60

    ix <- x >= bbox$minlon & x <= bbox$maxlon
    iy <- y >= bbox$minlat & y <= bbox$maxlat
    x <- x[ix]
    y <- y[iy]
    z <- z[ix, iy] - 273.15
    ox <- order(x)
    x <- x[ox]
    z <- z[ox, ]
    i2 <- duplicated(x)
    if(any(i2)){
        x <- x[!i2]
        z <- z[!i2, ]
    }

    list(t = tm, x = x, y = y, z = z)
}
