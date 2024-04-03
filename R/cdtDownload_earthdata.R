
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
        isrtools <- devtools::find_rtools()
        if(!isrtools){
            stop('Unable to find Rtools.')
        }
        rtools_pth <- pkgbuild::rtools_path()
        rtools_pth <- rev(split_path(rtools_pth[1]))
        rtools_pth <- file.path(rtools_pth[1], rtools_pth[2])
        curl_cmd <- file.path(rtools_pth, 'usr', 'bin', 'curl.exe')
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
