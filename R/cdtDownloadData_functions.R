
cdt.download.data <- function(urls, destfiles, ncfiles, nbfile,
                              GUI, verbose, data.name, down.fun, ...)
{
    parsL <- list(condition = if(length(urls) < 50 | nbfile == 1) FALSE else TRUE,
                  dopar = TRUE, detect.cores = FALSE, nb.cores = nbfile)
    f.args <- list(...)
    name.args <- names(as.list(args(down.fun)))
    if("GUI" %in% name.args) f.args$GUI <- GUI

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

        tmpdir <- dirname(destfiles[[1]][[1]][1])
        miss_files <- paste0(basename(tmpdir), '_missing_files.txt')
        miss_files <- file.path(dirname(tmpdir), miss_files)
        miss_data <- do.call(c, ret)
        cat(miss_data, file = miss_files, sep = '\n')

        return(1)
    }
    return(0)
}

##############################################

iridl.download.data <- function(lnk, dest, ncfl = NULL, GUI = TRUE)
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
                xx <- NULL
            }else unlink(dest)
        }else unlink(dest)
    }else{
        msg <- gsub('[\r\n]', '', dc[1])
        Insert.Messages.Out(msg, TRUE, "w", GUI)
    }
    return(xx)
}
