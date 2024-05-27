build_wgrib_gui <- function(){
    on.exit({
        tkconfigure(.cdtEnv$tcl$main$win, cursor = '')
        tcl('update')
    })
    tkconfigure(.cdtEnv$tcl$main$win, cursor = 'watch')
    tcl('update')

    wgrib_dir <- file.path(.cdtDir$dirLocal, .cdtData$GalParams$wgrib)
    out_file <- file.path(wgrib_dir, 'install.out')
    out_msg <- paste(.cdtData$GalParams$message[['3-1']], out_file,
                     .cdtData$GalParams$message[['3-2']])

    if(.cdtData$GalParams$wgrib == "wgrib2"){
        Insert.Messages.Out(.cdtData$GalParams$message[['1']], TRUE, "i")
        ret <- tryCatch2({
                        build_wgrib2(make = .cdtData$GalParams$make,
                                     cc = .cdtData$GalParams$cc,
                                     fc = .cdtData$GalParams$fc,
                                     messages = .cdtData$GalParams$message,
                                     GUI = TRUE)
                        },
                         error = function(e) errorFun(e),
                         finally = {
                            Insert.Messages.Out(out_msg, TRUE, "i")
                        })
    }else{
        Insert.Messages.Out(.cdtData$GalParams$message[['2']], TRUE, "i")
        ret <- tryCatch2({
                        compile_wgrib(cc = .cdtData$GalParams$cc,
                                      messages = .cdtData$GalParams$message,
                                      GUI = TRUE)
                        },
                         error = function(e) errorFun(e),
                         finally = {
                            Insert.Messages.Out(out_msg, TRUE, "i")
                        })
    }

    return(ret)
}

build_wgrib2_cmd <- function(make, cc, fc){
    xml.dlg <- file.path(.cdtDir$dirLocal, "languages", "cdtInstall_wgrib_dlgBox.xml")
    lang.dlg <- cdtLanguageParse(xml.dlg, .cdtData$Config$lang.iso)
    wgrib_dir <- file.path(.cdtDir$dirLocal, 'wgrib2')
    out_file <- file.path(wgrib_dir, 'install.out')
    out_msg <- paste(lang.dlg[['message']][['3-1']], out_file,
                     lang.dlg[['message']][['3-2']], '\n')
    on.exit(cat(out_msg))

    cat(paste0(lang.dlg[['message']][['1']], '\n'))
    ret <- build_wgrib2(make, cc, fc, lang.dlg[['message']], FALSE)

    return(ret)
}

compile_wgrib_cmd <- function(cc){
    xml.dlg <- file.path(.cdtDir$dirLocal, "languages", "cdtInstall_wgrib_dlgBox.xml")
    lang.dlg <- cdtLanguageParse(xml.dlg, .cdtData$Config$lang.iso)
    wgrib_dir <- file.path(.cdtDir$dirLocal, 'wgrib')
    out_file <- file.path(wgrib_dir, 'install.out')
    out_msg <- paste(lang.dlg[['message']][['3-1']], out_file,
                     lang.dlg[['message']][['3-2']], '\n')
    on.exit(cat(out_msg))

    cat(paste0(lang.dlg[['message']][['2']], '\n'))
    ret <- compile_wgrib(cc, lang.dlg[['message']], FALSE)

    return(ret)
}

build_wgrib2 <- function(make, cc, fc, messages, GUI){
    is_windows <- tolower(Sys.info()["sysname"]) == "windows"
    wgrib_dir <- file.path(.cdtDir$dirLocal, 'wgrib2')
    dir.create(wgrib_dir, showWarnings = FALSE, recursive = TRUE)
    config_dir <- file.path(wgrib_dir, 'config')
    dir.create(config_dir, showWarnings = FALSE, recursive = TRUE)

    ####
    if(is_windows){
        isrtools <- devtools::find_rtools()
        if(!isrtools) stop(messages[['4']])

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

    if(Sys.which(make) == "") stop(paste(messages[['5']], make))
    if(Sys.which(cc) == "") stop(paste(messages[['5']], cc))
    gcc_ver <- try(system(paste(cc, '--version'), intern = TRUE, ignore.stderr = TRUE), silent = TRUE)
    if(inherits(gcc_ver, "try-error")) stop(paste(cc, messages[['6']]))
    if(Sys.which(fc) == "") stop(paste(messages[['5']], fc))
    gfortran_ver <- try(system(paste(fc, '--version'), intern = TRUE, ignore.stderr = TRUE), silent = TRUE)
    if(inherits(gfortran_ver, "try-error")) stop(paste(fc, messages[['6']]))

    Sys.setenv(CC = cc)
    Sys.setenv(FC = fc)

    ####

    conf_g_u <- 'https://git.savannah.gnu.org/gitweb/?p=config.git;a=blob_plain;f=config.guess;hb=HEAD'
    conf_g_o <- file.path(config_dir, 'config.guess') 
    if(!file.exists(conf_g_o)){
        ret <- try(utils::download.file(conf_g_u, conf_g_o, method = "auto", quiet = TRUE, mode = "wb"), silent = TRUE)
        if(inherits(ret, "try-error")){
            stop(paste(messages[['7']], 'config.guess'))
        }else{
            if(ret != 0){
                stop(paste(messages[['8']], 'config.guess'))
            }
        }
    }

    conf_s_u <- 'https://git.savannah.gnu.org/gitweb/?p=config.git;a=blob_plain;f=config.sub;hb=HEAD'
    conf_s_o <- file.path(config_dir, 'config.sub') 
    if(!file.exists(conf_s_o)){
        ret <- try(utils::download.file(conf_s_u, conf_s_o, method = "auto", quiet = TRUE, mode = "wb"), silent = TRUE)
        if(inherits(ret, "try-error")){
            stop(paste(messages[['7']], 'config.sub'))
        }else{
            if(ret != 0){
                stop(paste(messages[['8']], 'config.sub'))
            }
        }
    }

    wgrib_lnk <- "https://www.ftp.cpc.ncep.noaa.gov/wd51we/wgrib2/wgrib2.tgz"
    wgrib_dest <- file.path(wgrib_dir, 'wgrib2.tgz')
    if(!file.exists(wgrib_dest)){
        ret <- try(utils::download.file(wgrib_lnk, wgrib_dest, method = "auto", quiet = TRUE, mode = "wb"), silent = TRUE)
        if(inherits(ret, "try-error")){
            stop(paste(messages[['7']], 'wgrib2'))
        }else{
            if(ret != 0){
                stop(paste(messages[['8']], 'wgrib2'))
            }
        }
    }

    ####
    grib2_dir <- file.path(wgrib_dir, 'grib2')
    if(dir.exists(grib2_dir)) unlink(grib2_dir, recursive = TRUE)
    res <- utils::untar(wgrib_dest, exdir = wgrib_dir)
    if(res != 0){
        res <- utils::untar(wgrib_dest, exdir = wgrib_dir,
                            tar = Sys.getenv("R_INSTALL_TAR", "internal"))
    }

    ####
    makefile <- file.path(grib2_dir, 'makefile')
    if(!file.exists(makefile)){
        stop(paste(messages[['9']], 'grib2/makefile'))
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

    if(is_windows){
        ret <- try(system(make, intern = TRUE), silent = TRUE)
        cat(ret, file = out_msg, sep = '\n')
        if(inherits(ret, "try-error")) stop(paste(messages[['10']], 'wgrib2'))
        wgrib2_exe <- 'wgrib2.exe'
    }else{
        cmd <- paste(make, '>', out_msg, '2>&1')
        ret <- try(system(cmd, intern = TRUE), silent = TRUE)
        if(inherits(ret, "try-error")) stop(paste(messages[['10']], 'wgrib2'))
        wgrib2_exe <- 'wgrib2'
    }
    wgrib2_exe1 <- file.path(wgrib_dir, 'grib2', 'wgrib2', wgrib2_exe)
    if(!file.exists(wgrib2_exe1)) stop(paste(messages[['10']], 'wgrib2'))
    wgrib2_exe2 <- file.path(wgrib_dir, wgrib2_exe)
    file.copy(wgrib2_exe1, wgrib2_exe2, overwrite = TRUE)

    wgrib2_config <- suppressWarnings(system2(wgrib2_exe2, '-config', stdout = TRUE))
    wgrib2_config <- c('*******************************************\n',
                       '************** wgrib2 config **************\n',
                       paste0(wgrib2_config, '\n'),
                       '*******************************************\n')
    msg_out <- paste0('wgrib2 ', messages[['11']], '!')
    if(GUI){
        containertab <- Display_Output_Console_Tab(wgrib2_config, "wgrib2 config", cat)
        ntab <- update.OpenTabs('ctxt', containertab)
        tkselect(.cdtEnv$tcl$main$tknotes, ntab)
        Insert.Messages.Out(msg_out, TRUE, "s")
    }else{
        cat(wgrib2_config)
        cat(paste(msg_out, '\n'))
    }

    return(0)
}

compile_wgrib <- function(cc, messages, GUI){
    is_windows <- tolower(Sys.info()["sysname"]) == "windows"
    wgrib_dir <- file.path(.cdtDir$dirLocal, 'wgrib')
    dir.create(wgrib_dir, showWarnings = FALSE, recursive = TRUE)

    ####
    if(is_windows){
        isrtools <- devtools::find_rtools()
        if(!isrtools) stop(messages[['4']])

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
    if(Sys.which(cc) == "") stop(paste(messages[['5']], cc))
    gcc_ver <- try(system(paste(cc, '--version'), intern = TRUE, ignore.stderr = TRUE), silent = TRUE)
    if(inherits(gcc_ver, "try-error")) stop(paste(cc, messages[['6']]))

    ####
    wgrib_lnk <- "https://ftp.cpc.ncep.noaa.gov/wd51we/wgrib/wgrib.c"
    wgrib_dest <- file.path(wgrib_dir, 'wgrib.c')
    if(!file.exists(wgrib_dest)){
        ret <- try(utils::download.file(wgrib_lnk, wgrib_dest, method = "auto", quiet = TRUE, mode = "wb"), silent = TRUE)
        if(inherits(ret, "try-error")){
            stop(paste(messages[['7']], 'wgrib'))
        }else{
            if(ret != 0){
                stop(paste(messages[['8']], 'wgrib'))
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

    if(is_windows){
        ret <- try(system(wgrib_comp, intern = TRUE), silent = TRUE)
        cat(ret, file = out_msg, sep = '\n')
        if(inherits(ret, "try-error")) stop(paste(messages[['10']], 'wgrib'))
        if(!file.exists(wgrib_exe)) stop(paste(messages[['10']], 'wgrib'))
    }else{
        cmd <- paste(wgrib_comp, '>', out_msg, '2>&1')
        ret <- try(system(cmd, intern = TRUE), silent = TRUE)
        if(inherits(ret, "try-error")) stop(paste(messages[['10']], 'wgrib'))
    }

    msg_out <- paste0('wgrib ', messages[['11']], '!')
    if(GUI){
        Insert.Messages.Out(msg_out, TRUE, "s")
    }else{
        cat(paste(msg_out, '\n'))
    }

    return(0)
}