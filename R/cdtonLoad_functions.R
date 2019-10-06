
## check OS type
WindowsOS <- function() if(Sys.info()["sysname"] == "Windows") TRUE else FALSE
MacOSXP <- function() if(Sys.info()["sysname"] %in% c("darwin", "Darwin")) TRUE else FALSE
LinuxOS <- function() if(Sys.info()["sysname"] == "Linux") TRUE else FALSE

.cdtEnv <- new.env()
.cdtData <- new.env()
.cdtDir <- new.env()

.cdtEnv$pkg$version <- packageVersion("CDT")
.cdtDir$Root <- system.file("cdt", package = "CDT")
local_dir <- if(WindowsOS()) '~/CDT_Local_Config' else '~/Documents/CDT_Local_Config'
.cdtDir$dirLocal <- path.expand(local_dir)

#############################

.onLoad <- function(...){
    if(WindowsOS()) ostype <- "Windows"
    if(MacOSXP()) ostype <- "MacOS"
    if(LinuxOS()) ostype <- "Linux"

    ## Copy config directories to local dir
    if(!dir.exists(.cdtDir$dirLocal)){
        dir.create(.cdtDir$dirLocal, recursive = TRUE, showWarnings = FALSE)
        cdtDirRoot <- file.path(.cdtDir$Root, c("config", "languages"))
        ret <- file.copy(cdtDirRoot, .cdtDir$dirLocal, recursive = TRUE, copy.mode = TRUE)
        if(any(!ret)){
            warning("Unable to copy")
            for(i in which(!ret))
                warning(paste(cdtDirRoot[i], "to", .cdtDir$dirLocal, "\n"))
            warning("Please copy manually.")
            stop("Copy of configurations files to local directory failed.")
        }
        orig.files <- c("Tcl_config.json", "cdt_config.json")
        bak.files <- c("Tcl_config.json_orig.bak", "cdt_config.json_orig.bak")
        config.dir <- file.path(.cdtDir$dirLocal, "config")
        file.copy(file.path(config.dir, orig.files), file.path(config.dir, bak.files), overwrite = TRUE)
    }
    # else{
    #     dirs <- c("config", "languages")
    #     md5.cdt.f <- file.path(.cdtDir$Root, "config", "cdt_md5_files")
    #     md5.loc.f <- file.path(.cdtDir$dirLocal, "config", "cdt_md5_files")
    #     md5.cdt <- readRDS(md5.cdt.f)
    #     md5.loc <- readRDS(md5.loc.f)

    #     if(!isTRUE(all.equal(md5.cdt, md5.loc))){
    #         for(j in seq_along(dirs)){
    #             cdtDirRoot <- file.path(.cdtDir$Root, dirs[j])
    #             cdtDirLocal <- file.path(.cdtDir$dirLocal, dirs[j])
    #             cdt <- md5.cdt[[dirs[j]]]
    #             loc <- md5.loc[[dirs[j]]]
    #             if(!isTRUE(all.equal(cdt, loc))){
    #                 ## new
    #                 new <- !cdt$files %in% loc$files
    #                 if(any(new)){
    #                     tocp <- file.path(cdtDirRoot, cdt$files[new])
    #                     vers <- file.path(cdtDirLocal, cdt$files[new])
    #                     ret <- file.copy(tocp, vers, overwrite = TRUE)
    #                     if(any(!ret)){
    #                         warning("Unable to copy")
    #                         for(i in which(!ret))
    #                             warning(paste(tocp[i], "to", dirname(vers[i])))
    #                          warning("Please copy manually")
    #                         warning("Copy of configurations files to local directory failed.")
    #                     }
    #                 }
    #                 ## change
    #                 ixfl1 <- match(loc$files, cdt$files)
    #                 ixfl1 <- ixfl1[!is.na(ixfl1)]
    #                 cdt.files <- cdt$files[ixfl1]
    #                 cdt.md5 <- cdt$md5[ixfl1]
    #                 ixfl2 <- loc$files %in% cdt.files
    #                 loc.files <- loc$files[ixfl2]
    #                 loc.md5 <- loc$md5[ixfl2]

    #                 change <- cdt.md5 != loc.md5
    #                 if(any(change)){
    #                     tocp <- file.path(cdtDirRoot, cdt.files[change])
    #                     vers <- file.path(cdtDirLocal, cdt.files[change])
    #                     bak <- paste0(vers, "_bak-", format(Sys.time(), "%Y%m%d%H%M"))
    #                     file.copy(vers, bak, overwrite = TRUE)
    #                     ret <- file.copy(tocp, vers, overwrite = TRUE)
    #                     if(any(!ret)){
    #                         warning("Unable to copy")
    #                         for(i in which(!ret))
    #                             warning(paste(tocp[i], "to", dirname(vers[i])))
    #                         warning("Please copy manually")
    #                         warning("Copy of configurations files to local directory failed.")
    #                     }
    #                 }
    #             }
    #         }
    #         ret <- file.copy(md5.cdt.f, md5.loc.f, overwrite = TRUE)
    #         if(!ret){
    #             msg1 <- "Unable to copy\n"
    #             msg2 <- paste(md5.cdt.f, "to", file.path(.cdtDir$dirLocal, "config"))
    #             warning(paste(msg1, msg2))
    #             warning("Unable to update local CDT configuration.")
    #         }
    #     }
    # }

    #############################

    editcfg.msg <- function(X)
        paste("\nInstall", X, "or edit the configuration file:\n", tcl.file.conf.json)

    #############################

    tcl.file.conf.json <- file.path(.cdtDir$dirLocal, "config", "Tcl_config.json")
    tcl.conf <- jsonlite::fromJSON(tcl.file.conf.json)

    if(WindowsOS()){
        if(tcl.conf[[ostype]]$UseOtherTclTk){
            tclbin <- stringr::str_trim(tcl.conf[[ostype]]$Tclbin)
            if(!dir.exists(tclbin))
                stop(paste0(tclbin, " does not exist", editcfg.msg("Tcl/Tk")))
            ### 
            # tclsh <- file.path(tclbin, "tclsh.exe")
            # tcl_lib <- file.path(.cdtDir$dirLocal, "library.tcl")
            # cat("puts $tcl_library\n", file = tcl_lib)
            # # writeLines("puts $tcl_library", tcl_lib)
            # tcllib <- system(paste(tclsh, tcl_lib), intern = TRUE)
            # if(length(tcllib) == 0){
            #     unlink(tcl_lib)
            #     stop("Unable to locate Tcl library")
            # }
            # unlink(tcl_lib)

            Sys.setenv(MY_TCLTK = tclbin)
            Sys.setenv(TCL_LIBRARY = tcllib)
            library.dynam("tcltk", "tcltk", .libPaths(), DLLpath = tclbin)
        }
    }else{
        tclsh <- suppressWarnings(system("which tclsh", intern = TRUE))
        if(length(tclsh) == 0) stop("No Tcl/Tk installation found")
        tcllib <- suppressWarnings(system("echo 'puts $tcl_library;exit 0' | tclsh", intern = TRUE))
        if(!dir.exists(tcllib)) stop("Tcl library installation not found")
    }

    if(!tcl.conf[[ostype]]$Tktable.auto){
        Tktable.path <- stringr::str_trim(tcl.conf[[ostype]]$Tktable.path)
        if(!dir.exists(Tktable.path)) stop(paste(Tktable.path, "does not found"))
        if(!WindowsOS()){
            Sys.setenv(TK_TABLE_LIBRARY = Tktable.path)
            Sys.setenv(TK_TABLE_LIBRARY_FILE = "tkTable.tcl")
        }
    }else Tktable.path <- NULL

    if(!tcl.conf[[ostype]]$Bwidget.auto){
        Bwidget.path <- stringr::str_trim(tcl.conf[[ostype]]$Bwidget.path)
        if(!dir.exists(Bwidget.path)) warning(paste(Bwidget.path, "does not found"))
    }else Bwidget.path <- NULL

    if(!isNamespaceLoaded("tcltk")){
        loadNamespace("tcltk")
        attachNamespace("tcltk")
    }

    if(!is.null(Tktable.path)) addTclPath(path = Tktable.path)
    is.notkt <- tclRequire("Tktable")
    if(is.logical(is.notkt)) warning(paste0("Tcl package 'Tktable' not found", editcfg.msg('Tktable')))
    if(!is.null(Bwidget.path)) addTclPath(path = Bwidget.path)
    is.nobw <- tclRequire("BWidget")
    if(is.logical(is.nobw)) warning(paste0("Tcl package 'BWidget' not found", editcfg.msg('BWidget')))

    invisible()
}

.onAttach <- function(libname, pkgname){
    packageStartupMessage("
    -----------------------------------------------------------
    CDT version ", .cdtEnv$pkg$version, " -- Copyright (C) 2014-", format(Sys.Date(), "%Y"), "
    Climate Data Tools
    International Research Institute for Climate and Society
    CDT can be found at https://github.com/rijaf/CDT
    If you have issues installing CDT, you can report it to
    https://github.com/rijaf-iri/CDT/issues/1

    To start CDT use startCDT()
    -----------------------------------------------------------
    ")

    ### progress bar
    .cdtEnv$tcl$status$pbnmax <- tclVar(1)

    invisible()
}
