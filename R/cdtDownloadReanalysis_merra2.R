
merra2.download.iridl <- function(GalParams, nbfile = 3, GUI = TRUE, verbose = TRUE){
    dlpath <- "https://iridl.ldeo.columbia.edu/SOURCES/.NASA/.GSFC/.MERRA2/.Anl_MonoLev"
    varid <- switch(GalParams$var,
                      "tmax" = ".t2mmax",
                      "tmin" = ".t2mmin"
                    )
    rlon <- unlist(GalParams$bbox[c('minlon', 'maxlon')])
    rlon <- paste(c('X', rlon, 'RANGE'), collapse = "/")
    rlat <- unlist(GalParams$bbox[c('minlat', 'maxlat')])
    rlat <- paste(c('Y', rlat, 'RANGE'), collapse = "/")
    units <- "273.15/sub//units/%28Celcius_scale%29/def"

    rdate <- iridl.format.date("daily", GalParams$date.range)
    urls <- urltools::url_encode(paste0("(", rdate$dates, ")"))
    urls <- paste0("T", "/", urls, "/", "VALUE")

    urls <- paste(dlpath, varid, rlon, rlat, urls, units, 'data.nc', sep = "/")

    #########
    data.name <- paste0("MERRA2_daily_", GalParams$var)
    outdir <- file.path(GalParams$dir2save, data.name)
    dir.create(outdir, showWarnings = FALSE, recursive = TRUE)
    destfiles <- file.path(outdir, paste0(GalParams$var, "_", rdate$out, ".nc"))

    ret <- cdt.download.data(urls, destfiles, destfiles, nbfile, GUI,
                             verbose, data.name, iridl.download.data)

    return(ret)
}
