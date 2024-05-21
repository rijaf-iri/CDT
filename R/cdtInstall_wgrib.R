#' Install wgrib.
#' 
#' Function to download and compile wgrib into CDT local folder.
#'
#' @param cc the \code{C} compiler to be used or the full path to the \code{C} compiler.
#' 
#' @references \url{https://www.cpc.ncep.noaa.gov/products/wesley/wgrib.html}
#' 
#' @examples
#'
#' \dontrun{
#' install_wgrib()
#' }
#' 
#' @export

install_wgrib <- function(cc = "gcc"){
    is_windows <- tolower(Sys.info()["sysname"]) == "windows"
    wgrib_dir <- file.path(.cdtDir$dirLocal, 'wgrib')
    dir.create(wgrib_dir, showWarnings = FALSE, recursive = TRUE)

    ####
    if(is_windows){
        isrtools <- devtools::find_rtools()
        if(!isrtools){
            stop('Unable to find Rtools.')
        }

        r_version <- R.Version()
        r_major <- r_version$major
        r_minor <- gsub('\\.', '', r_version$minor)
        r_version <- as.numeric(paste0(r_major, '.', r_minor))

        if(r_version < 4.2){
            rtools_pth <- pkgbuild::rtools_path()
            rtools_pth <- rev(split_path(rtools_pth[1]))
            rtools_pth <- file.path(rtools_pth[1], rtools_pth[2])
            r_arch <- switch(Sys.getenv("R_ARCH"), "/x64" = 64, "/i386" = 32)
            mingw_arch <- file.path(rtools_pth, paste0('mingw', r_arch))
            mingw_bin <- file.path(mingw_arch, 'bin')
            mingw_pth <- paste0(mingw_bin, ';', Sys.getenv('PATH'))
            Sys.setenv(PATH = mingw_pth)
        }
    }

    ####
    if(Sys.which(cc) == "") stop(paste("Unable to find", cc))
    gcc_ver <- try(system(paste(cc, '--version'), intern = TRUE, ignore.stderr = TRUE), silent = TRUE)
    cc_msg <- paste(cc, "does not appear to be installed, or it is not on the PATH variable.")
    if(inherits(gcc_ver, "try-error")) stop(cc_msg)

    ####
    wgrib_lnk <- "https://ftp.cpc.ncep.noaa.gov/wd51we/wgrib/wgrib.c"
    wgrib_dest <- file.path(wgrib_dir, 'wgrib.c')
    if(!file.exists(wgrib_dest)){
        ret <- try(utils::download.file(wgrib_lnk, wgrib_dest, method = "auto", quiet = TRUE, mode = "wb"), silent = TRUE)
        if(inherits(ret, "try-error")){
            stop('Unable to download wgrib')
        }else{
            if(ret != 0){
                stop('An error occurred when downloading wgrib')
            }
        }
    }

    ####
    out_msg <- file.path(wgrib_dir, 'install.out')
    unlink(out_msg)
    wgrib_exe <- if(is_windows) 'wgrib.exe' else 'wgrib'
    wgrib_exe <- file.path(wgrib_dir, wgrib_exe)
    unlink(wgrib_exe)
    wgrib_comp <- paste(cc, "-o", wgrib_exe, wgrib_dest, '-lm')
    err_msg <- 'Unable to compile wgrib'

    if(is_windows){
        ret <- try(system(wgrib_comp, intern = TRUE), silent = TRUE)
        cat(ret, file = out_msg, sep = '\n')
        if(inherits(ret, "try-error")) stop(err_msg)
        if(!file.exists(wgrib_exe)) stop(err_msg)
    }else{
        cmd <- paste(wgrib_comp, '>', out_msg, '2>&1')
        ret <- try(system(cmd, intern = TRUE), silent = TRUE)
        if(inherits(ret, "try-error")) stop(err_msg)
    }

    cat('*******************************************\n')
    cat('wgrib compiled successfully!\n')
    return(0)
}

#' Install wgrib2.
#' 
#' Function to download and build wgrib2 into CDT local folder.
#'
#' @param make the make command or full path to make command. 
#' @param cc the \code{C} compiler to be used or the full path to the \code{C} compiler.
#' @param fc the \code{Fortran} compiler to be used or the full path to the \code{Fortran} compiler.
#' 
#' @references \url{https://www.cpc.ncep.noaa.gov/products/wesley/wgrib2/index.html}
#'
#' @examples
#'
#' \dontrun{
#' ## Install wgrib2 on Windows
#' install_wgrib2()
#' 
#' ## Install wgrib2 on MacOS
#' install_wgrib2(make = '/usr/local/bin/gmake', cc = '/usr/local/bin/gcc-13', fc = '/usr/local/bin/gfortran')
#' 
#' ## Install wgrib2 on Linux with the default make, CC and FC
#' install_wgrib2()
#' }
#' 
#' @export

install_wgrib2 <- function(make = "make", cc = "gcc", fc = "gfortran"){
    is_windows <- tolower(Sys.info()["sysname"]) == "windows"
    wgrib_dir <- file.path(.cdtDir$dirLocal, 'wgrib2')
    dir.create(wgrib_dir, showWarnings = FALSE, recursive = TRUE)
    config_dir <- file.path(wgrib_dir, 'config')
    dir.create(config_dir, showWarnings = FALSE, recursive = TRUE)

    ####
    if(is_windows){
        isrtools <- devtools::find_rtools()
        if(!isrtools){
            stop('Unable to find Rtools.')
        }

        r_version <- R.Version()
        r_major <- r_version$major
        r_minor <- gsub('\\.', '', r_version$minor)
        r_version <- as.numeric(paste0(r_major, '.', r_minor))

        if(r_version < 4.2){
            rtools_pth <- pkgbuild::rtools_path()
            rtools_pth <- rev(split_path(rtools_pth[1]))
            rtools_pth <- file.path(rtools_pth[1], rtools_pth[2])
            r_arch <- switch(Sys.getenv("R_ARCH"), "/x64" = 64, "/i386" = 32)
            mingw_arch <- file.path(rtools_pth, paste0('mingw', r_arch))
            mingw_bin <- file.path(mingw_arch, 'bin')
            mingw_pth <- paste0(mingw_bin, ';', Sys.getenv('PATH'))
            Sys.setenv(PATH = mingw_pth)
        }
        # Sys.setenv(COMP_SYS = "gnu_linux")
        Sys.setenv(COMP_SYS = "cygwin_win")
    }

    ####
    if(Sys.which(make) == "") stop(paste('Unable to find', make))
    if(Sys.which(cc) == "") stop(paste('Unable to find', cc))
    gcc_ver <- try(system(paste(cc, '--version'), intern = TRUE, ignore.stderr = TRUE), silent = TRUE)
    cc_msg <- paste(cc, "does not appear to be installed, or it is not on the PATH variable.")
    if(inherits(gcc_ver, "try-error")) stop(cc_msg)
    if(Sys.which(fc) == "") stop(paste('Unable to find', fc))
    gfortran_ver <- try(system(paste(fc, '--version'), intern = TRUE, ignore.stderr = TRUE), silent = TRUE)
    fc_msg <- paste(fc, "does not appear to be installed, or it is not on the PATH variable.")
    if(inherits(gfortran_ver, "try-error")) stop(fc_msg)

    Sys.setenv(CC = cc)
    Sys.setenv(FC = fc)

    ####
    conf_g_u <- 'https://git.savannah.gnu.org/gitweb/?p=config.git;a=blob_plain;f=config.guess;hb=HEAD'
    conf_g_o <- file.path(config_dir, 'config.guess') 
    if(!file.exists(conf_g_o)){
        ret <- try(utils::download.file(conf_g_u, conf_g_o, method = "auto", quiet = TRUE, mode = "wb"), silent = TRUE)
        if(inherits(ret, "try-error")){
            stop('Unable to download config.guess')
        }else{
            if(ret != 0){
                stop('An error occurred when downloading config.guess')
            }
        }
    }

    conf_s_u <- 'https://git.savannah.gnu.org/gitweb/?p=config.git;a=blob_plain;f=config.sub;hb=HEAD'
    conf_s_o <- file.path(config_dir, 'config.sub') 
    if(!file.exists(conf_s_o)){
        ret <- try(utils::download.file(conf_s_u, conf_s_o, method = "auto", quiet = TRUE, mode = "wb"), silent = TRUE)
        if(inherits(ret, "try-error")){
            stop('Unable to download config.sub')
        }else{
            if(ret != 0){
                stop('An error occurred when downloading config.sub')
            }
        }
    }

    wgrib_lnk <- "https://www.ftp.cpc.ncep.noaa.gov/wd51we/wgrib2/wgrib2.tgz"
    wgrib_dest <- file.path(wgrib_dir, 'wgrib2.tgz')
    if(!file.exists(wgrib_dest)){
        ret <- try(utils::download.file(wgrib_lnk, wgrib_dest, method = "auto", quiet = TRUE, mode = "wb"), silent = TRUE)
        if(inherits(ret, "try-error")){
            stop('Unable to download wgrib2')
        }else{
            if(ret != 0){
                stop('An error occurred when downloading wgrib2')
            }
        }
    }

    ####
    grib2_dir <- file.path(wgrib_dir, 'grib2')
    if(dir.exists(grib2_dir)) unlink(grib2_dir, recursive = TRUE)
    utils::untar(wgrib_dest, exdir = wgrib_dir)

    ####
    makefile <- file.path(grib2_dir, 'makefile')
    if(!file.exists(makefile)){
        stop('Could not find grib2/makefile')
    }

    makedit <- readLines(makefile)
    i_jpg <- grep("^USE\\_OPENJPEG\\=1$", makedit)
    if(length(i_jpg) > 0){
       makedit[i_jpg[1]] <- "USE_OPENJPEG=0"
    }
    i_aec <- grep("^USE\\_AEC\\=1$", makedit)
    if(length(i_aec) > 0){
        makedit[i_aec[1]] <- "USE_AEC=0"
    }

    if(is_windows){
        i_slib <- grep("^MAKE\\_SHARED\\_LIB\\=1$", makedit)
        if(length(i_slib) > 0){
           makedit[i_slib[1]] <- "MAKE_SHARED_LIB=0"
        }

        # i_png <- grep("^USE\\_PNG\\=1$", makedit)
        # if(length(i_png) > 0){
        #    makedit[i_png[1]] <- "USE_PNG=0"
        # }
    }

    d_png <- grep("rm tmpp.tar", makedit)
    if(length(d_png) > 0){
        i1 <- d_png[1]
        v1 <- gsub("rm tmpp.tar", "", makedit[i1])
        cp1 <- paste0(v1, "cp ../config/config.* ${pngdir}/")
        makedit <- c(makedit[1:i1], cp1, makedit[(i1 + 1):length(makedit)])
    }

    d_proj4 <- grep("rm tmpproj4.tar", makedit)
    if(length(d_proj4) > 0){
        i2 <- d_proj4[1]
        v2 <- gsub("rm tmpproj4.tar", "", makedit[i2])
        cp2 <- paste0(v2, "cp ../config/config.* ${proj4dir}/")
        makedit <- c(makedit[1:i2], cp2, makedit[(i2 + 1):length(makedit)])
    }

    d_ncdf3 <- grep("rm tmpn.tar", makedit)
    if(length(d_ncdf3) > 0){
        i3 <- d_ncdf3[1]
        v3 <- gsub("rm tmpn.tar", "", makedit[i3])
        cp3 <- paste0(v3, "cp ../config/config.* ${netcdf3dir}/")
        makedit <- c(makedit[1:i3], cp3, makedit[(i3 + 1):length(makedit)])
    }

    if(is_windows){
        ld_math <- grep("^wLDFLAGS\\+\\=\\-lm$", makedit)
        if(length(ld_math) > 0){
            makedit[ld_math[1]] <- paste(makedit[ld_math[1]], "-lquadmath")
        }

        ###
        if(r_version < 4.2){
            i_reg <- grep("^USE\\_REGEX\\=1$", makedit)
            if(length(i_reg) > 0){
               makedit[i_reg[1]] <- "USE_REGEX=0"
            }
        }else{
            u_reg <- grep("^ifeq \\(\\$\\(USE_REGEX\\)\\,1\\)$", makedit)
            if(length(u_reg) > 0){
                i1 <- u_reg[1]
                v1 <- makedit[i1 + 1]
                v1 <- strsplit(v1, 'a:=')[[1]][1]
                cp1 <- paste0(v1, "wLDFLAGS+=-lregex")
                makedit <- c(makedit[1:i1], cp1, makedit[(i1 + 1):length(makedit)])
            }
        }
    }

    cat(makedit, file = makefile, sep = '\n')

    ####
    wd <- getwd()
    on.exit(setwd(wd))
    setwd(file.path(wgrib_dir, 'grib2'))

    out_msg <- file.path(wgrib_dir, 'install.out')
    unlink(out_msg)
    err_msg <- 'Unable to build wgrib2'

    if(is_windows){
        ret <- try(system(make, intern = TRUE), silent = TRUE)
        cat(ret, file = out_msg, sep = '\n')
        if(inherits(ret, "try-error")) stop(err_msg)
        wgrib2_exe <- 'wgrib2.exe'
    }else{
        cmd <- paste(make, '>', out_msg, '2>&1')
        ret <- try(system(cmd, intern = TRUE), silent = TRUE)
        if(inherits(ret, "try-error")) stop(err_msg)
        wgrib2_exe <- 'wgrib2'
    }
    wgrib2_exe1 <- file.path(wgrib_dir, 'grib2', 'wgrib2', wgrib2_exe)
    if(!file.exists(wgrib2_exe1)) stop(err_msg)
    wgrib2_exe2 <- file.path(wgrib_dir, wgrib2_exe)
    file.copy(wgrib2_exe1, wgrib2_exe2, overwrite = TRUE)

    cat('*******************************************\n')
    cat('************** wgrib2 config **************\n')
    system(paste(wgrib2_exe2, '-config'))
    cat('*******************************************\n')
    cat('wgrib2 build successfully!\n')

    return(0)
}
