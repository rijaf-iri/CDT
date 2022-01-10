#' Install wgrib.
#' 
#' Function to download and install wgrib into CDT local folder.
#' 
#' @references \url{https://www.cpc.ncep.noaa.gov/products/wesley/wgrib.html}
#' 
#' @export

install_wgrib <- function(){
    wgrib_dir <- file.path(.cdtDir$dirLocal, 'wgrib')
    dir.create(wgrib_dir, showWarnings = FALSE, recursive = TRUE)

    wgrib_lnk <- "ftp://ftp.cpc.ncep.noaa.gov/wd51we/wgrib/wgrib.c"
    wgrib_dest <- file.path(wgrib_dir, 'wgrib.c')
    ret <- try(utils::download.file(wgrib_lnk, wgrib_dest, method = "auto", quiet = TRUE, mode = "wb"), silent = TRUE)

    if(inherits(ret, "try-error")){
        stop('Unable to download wgrib.')
    }else{
        if(ret != 0){
            stop('An error occurred when downloading wgrib.')
        }
    }

    if(tolower(Sys.info()["sysname"]) == "windows"){
        isrtools <- devtools::find_rtools()
        if(!isrtools){
            stop('Unable to find Rtools.')
        }

        rtools_pth <- pkgbuild::rtools_path()
        rtools_pth <- rev(split_path(rtools_pth))
        rtools_pth <- file.path(rtools_pth[1], rtools_pth[2])

        r_arch <- switch(Sys.getenv("R_ARCH"), "/x64" = 64, "/i386" = 32)
        gcc_comp <- file.path(rtools_pth, paste0('mingw', r_arch), 'bin', 'gcc.exe')
        error_msg <- "Unable to find gcc."
    }else{
        gcc_comp <- 'gcc'
        error_msg <- "gcc does not appear to be installed, or it is not on the PATH variable."
    }

    gcc_ver <- try(system(paste(gcc_comp, '--version'), intern = TRUE, ignore.stderr = TRUE), silent = TRUE)
    if(inherits(gcc_ver, "try-error")) stop(error_msg)

    wgrib_exe <- file.path(wgrib_dir, 'wgrib')
    wgrib_comp <- paste(gcc_comp, "-o", wgrib_exe, wgrib_dest)

    ret <- try(system(wgrib_comp, intern = TRUE, ignore.stderr = TRUE), silent = TRUE)
    if(inherits(ret, "try-error")){
        stop('Unable to compile wgrib.')
    }

    cat('wgrib compiled successfully!\n')
}
