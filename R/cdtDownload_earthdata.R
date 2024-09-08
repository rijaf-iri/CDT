
earthdata.curlopts <- function(work_dir, login){
    cfile <- file.path(work_dir, '.urs_cookies')
    nfile <- file.path(work_dir, '.netrc')
    login_url <- "urs.earthdata.nasa.gov"
    netrc <- paste("machine", login_url,
                   "login", login$usr,
                   "password", login$pwd)
    cat(netrc, file = nfile, sep = '\n')
    cat('', file = cfile)

    if(tolower(Sys.info()["sysname"]) == "windows"){
        isrtools <- pkgbuild::find_rtools()
        if(!isrtools){
            stop('Unable to find Rtools.')
        }

        r_version <- R.Version()
        r_major <- r_version$major
        r_minor <- strsplit(r_version$minor, '\\.')[[1]][1]
        r_version <- as.numeric(paste0(r_major, '.', r_minor))
        if(r_version < 4.0){
            stop('CDT requires R version 4.0.0 or higher.')
        }
        rt_v <- if(r_version < 4.2) '40' else paste0(r_major, r_minor)
        env_rtools <- grepl(paste0('rtools', rt_v), Sys.getenv('PATH'))
        if(!env_rtools){
            stop("Rtools is not in your PATH environment variable.")
        }
        curl_cmd <- 'curl'
        error_msg <- "Unable to find curl."
    }else{
        curl_cmd <- 'curl'
        error_msg <- "curl does not appear to be installed, or it is not on the PATH variable."
    }

    curl_ver <- try(system(paste(curl_cmd, '--version'), intern = TRUE, ignore.stderr = TRUE), silent = TRUE)
    if(inherits(curl_ver, "try-error")) stop(error_msg)

    opendap <- "https://gpm1.gesdisc.eosdis.nasa.gov/opendap"
    datapath <- "GPM_L3/GPM_3IMERGHH.07/2023/001"
    datahdf5 <- "3B-HHR.MS.MRG.3IMERG.20230101-S000000-E002959.0000.V07B.HDF5"
    outformat <- ".ascii"
    query <- "lon%5B2265:1:2275%5D"
    dataset <- file.path(opendap, datapath, datahdf5)
    url <- paste0(dataset, outformat, '?', query)

    current_wd <- getwd()
    setwd(work_dir)
    cmd <- paste(curl_cmd, '-LJO', '--netrc-file', nfile,
                 '--cookie-jar', cfile, '--cookie', cfile,
                 '--url', url)
    ret <- try(system(cmd, intern = TRUE, ignore.stderr = TRUE), silent = TRUE)
    if(inherits(ret, "try-error")){
        stop('Unable to create cookies file')
    }

    tmpf <- list.files(work_dir, paste0('^', datahdf5), full.names = TRUE)
    unlink(tmpf)
    setwd(current_wd)

    list(cookie = cfile, netrc = nfile)
}

#########################################################

opendap.gesdisc.table <- function(url){
    ret <- httr::GET(url)
    if(httr::status_code(ret) != 200){
        Insert.Messages.httr(ret)
        return(NULL)
    }
    ret <- httr::content(ret)

    tmp <- rvest::html_table(ret, fill = TRUE)[[1]]
    tmp <- as.data.frame(tmp[-1, ])
    tmp <- tmp[tmp[, 1] != "", 1]
    tmp <- tmp[tmp != "-"]
    tmp <- tmp[tmp != "ddx"]
    tmp <- gsub('/', '', tmp)
    tmp <- tmp[!grepl('\\.xml$', tmp)]
    return(tmp)
}

opendap.gesdisc.dates <- function(url, type, fileformat = NA, datetype = NA, diryear = NA){
    tmp <- opendap.gesdisc.table(url)
    if(length(tmp) == 0) return(NULL)

    if(type == 'file'){
        ret <- extract_filename_dates(tmp, fileformat)
        if(is.null(ret)) return(NULL)
        ret <- gsub('[^[:digit:]]', '', ret)
    }else if(type == 'directory'){
        if(datetype == 'year'){
            tmp <- as.Date(paste0(tmp, '-01-01'))
            frmt <- '%Y'
        }else if(datetype == 'month'){
            tmp <- as.Date(paste0(diryear, '-', tmp, '-01'))
            frmt <- '%m'
        }else if(datetype == 'doy'){
            tmp <- as.Date(paste0(diryear, tmp), '%Y%j')
            frmt <- '%j'
        }else return(NULL)

        ret <- tmp[!is.na(tmp)]
        if(length(ret) == 0) return(NULL)
        ret <- format(ret, frmt)
    }else return(NULL)

    ret <- sort(ret)

    return(ret)
}
