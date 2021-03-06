
cdt.download.data <- function(urls, destfiles, ncfiles, nbfile,
                              GUI, verbose, data.name, down.fun, ...)
{
    parsL <- list(condition = if(length(urls) < 50 | nbfile == 1) FALSE else TRUE,
                  dopar = TRUE, detect.cores = FALSE, nb.cores = nbfile)
    f.args <- list(...)

    ret <- cdt.foreach(seq_along(urls), parsL, GUI, progress = TRUE, FUN = function(j)
    {
        f.args <- c(list(lnk = urls[j], dest = destfiles[j], ncfl = ncfiles[j]), f.args)
        dwn <- do.call(down.fun, f.args)
        return(dwn)
    })

    inull <- !sapply(ret, is.null)
    if(any(inull)){
        data.files <- paste("Unable to download data for", unlist(ret[inull]))
        xx <- paste(data.name, paste(data.files, collapse = "\n"), sep = "\n")

        if(GUI){
            containertab <- Display_Output_Console_Tab(xx, data.name, cat)
            ntab <- update.OpenTabs('ctxt', containertab)
            tkselect(.cdtEnv$tcl$main$tknotes, ntab)
        }else{
            cat(xx, "\n")
        }
        return(1)
    }
    return(0)
}

##############################################

iridl.download.data <- function(lnk, dest, ncfl = NULL)
{
    ncfl <- ncfl
    xx <- basename(dest)

    dc <- try(curl::curl_download(lnk, dest), silent = TRUE)
    if(!inherits(dc, "try-error")){
        nc <- try(ncdf4::nc_open(dest), silent = TRUE)
        if(!inherits(nc, "try-error")){
            v <- ncdf4::ncvar_get(nc, nc$var[[1]]$name)
            ncdf4::nc_close(nc)
            if(!all(is.na(v))){
                ## TAMSAT v3.1
                if(grepl("\\.TAMSAT", lnk) && grepl("\\.v3p1", lnk)){
                    unlink(dest)
                    lnk <- sub("\\.rfe", "\\.rfe_filled", lnk)
                    dc <- try(curl::curl_download(lnk, dest), silent = TRUE)
                    if(!inherits(dc, "try-error")){
                        nc <- try(ncdf4::nc_open(dest, write = TRUE), silent = TRUE)
                        if(!inherits(nc, "try-error")){
                            ncdf4::ncvar_rename(nc, "rfe_filled", "rfe")
                            ncdf4::nc_close(nc)
                            xx <- NULL
                        }else unlink(dest)
                    }
                }else{
                    ## other dataset
                    xx <- NULL
                }
            }else unlink(dest)
        }else unlink(dest)
    }
    return(xx)
}
