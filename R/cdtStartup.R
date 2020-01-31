
#' Start CDT GUI
#'
#' Starting CDT GUI from R console.
#' 
#' @param wd full path to the working directory.
#' @param lang the language to be used.
#' 
#' @examples
#' 
#' \dontrun{
#' library(CDT)
#' 
#' # Default working directory and English language
#' startCDT()
#' 
#' # Default working directory and Français language
#' startCDT(lang = "fr")
#' 
#' # Setting both working directory and language
#' startCDT("C:/Users/rijaf/Documents/AGHRYMET_ENACTS", "fr")
#' }
#' 
#' @export

startCDT <- function(wd = NA, lang = NA){
    cdt.file.conf <- file.path(.cdtDir$dirLocal, "config", "cdt_config.json")
    Config <- fromJSON(cdt.file.conf)
    Config <- rapply(Config, str_trim, classes = "character", how = "replace")

    if(!is.na(wd)){
        wd <- str_trim(wd)
        if(!dir.exists(wd)){
            warning(paste(wd, "does not found"), immediate. = TRUE)
            wd <- getwd()
        }
    }else{
        wd <- getwd()
        if(Config$wd != "" & dir.exists(wd)) wd <- Config$wd
    }
    setwd(wd)

    Config$wd <- wd
    if(!is.na(lang)) Config$lang.iso <- lang
    .cdtData$Config <- Config

    .cdtEnv$tcl$dir <- tempdir()

    ##################

    xml.global <- file.path(.cdtDir$dirLocal, "languages", "cdtGlobal_widgets.xml")
    .cdtEnv$tcl$lang$global <- cdtLanguageParse(xml.global, .cdtData$Config$lang.iso)

    xml.menu <- file.path(.cdtDir$dirLocal, "languages", "cdt_menu_bar.xml")
    lang.menu <- cdtLanguageParse.menu(xml.menu, .cdtData$Config$lang.iso)

    # #################################################################################

    options(warn = -1)
    # options(warn = 0)

    .cdtEnv$tcl$main$win <- tktoplevel()
    tkwm.resizable(.cdtEnv$tcl$main$win, TRUE, TRUE)
    tkgrid.columnconfigure(.cdtEnv$tcl$main$win, 0, weight = 1)
    tkgrid.rowconfigure(.cdtEnv$tcl$main$win, 1, weight = 1)

    ## Window geometry
    .cdtEnv$tcl$data$width.scr <- as.integer(tkwinfo("screenwidth", .cdtEnv$tcl$main$win))
    .cdtEnv$tcl$data$height.scr <- as.integer(tkwinfo("screenheight", .cdtEnv$tcl$main$win))

    ## Function to scale dialog in %
    .cdtEnv$tcl$fun$w.scale <- function(percent) as.integer(percent * .cdtEnv$tcl$data$width.scr / 100)
    .cdtEnv$tcl$fun$h.scale <- function(percent) as.integer(percent * .cdtEnv$tcl$data$height.scr / 100)

    ## Left panel dimension
    .cdtEnv$tcl$data$wpanel.left <- .cdtEnv$tcl$fun$w.scale(30)
    .cdtEnv$tcl$data$hpanel.left <- .cdtEnv$tcl$fun$h.scale(70)

    ## Left panel ScrollableFrame
    ## wscrlwin (min:23 max:25)
    .cdtEnv$tcl$data$wscrlwin <- .cdtEnv$tcl$fun$w.scale(24)
    .cdtEnv$tcl$data$hscrlwin <- .cdtEnv$tcl$fun$h.scale(50)

    ## Font width
    .cdtEnv$tcl$data$sfont0 <- as.numeric(
                            tclvalue(
                                tkfont.measure(.cdtEnv$tcl$main$win,
                                    paste0("0123456789", paste0(letters[1:26], LETTERS[1:26], collapse = ''))
                                )
                            )
                        ) / (10 + 2 * 26)

    ## Widgets width function in %
    .cdtEnv$tcl$fun$w.widgets <- function(percent) 
            as.integer(.cdtEnv$tcl$fun$w.scale(percent) / .cdtEnv$tcl$data$sfont0)
    .cdtEnv$tcl$fun$h.widgets <- function(percent) 
            as.integer(.cdtEnv$tcl$fun$h.scale(percent) / .cdtEnv$tcl$data$sfont0)

    if(WindowsOS()){
        ## Output message, tktext height
        txtHeight <- 6
        w.opfiles.perc <- 39
    }else{
        txtHeight <- 7
        w.opfiles.perc <- 30
    }
    ## List open files width
    .cdtEnv$tcl$data$w.opfiles <- .cdtEnv$tcl$fun$w.widgets(w.opfiles.perc)

    ## Files extension
    .cdtEnv$tcl$data$filetypes1 <- "{{Text Files} {.txt .TXT}} {{CSV Files} {.csv .CSV}} {{All files} *}"
    .cdtEnv$tcl$data$filetypes2 <- "{{CSV Files} {.csv .CSV}} {{Text Files} {.txt .TXT}} {{All files} *}"
    .cdtEnv$tcl$data$filetypes3 <- "{{NetCDF Files} {.nc .NC .cdf .CDF}} {{All files} *}"
    .cdtEnv$tcl$data$filetypes4 <- "{{ESRI Shapefile} {.shp}} {{All files} *}"
    .cdtEnv$tcl$data$filetypes5 <- "{{JPEG format} {.jpeg .JPEG}} {{PNG format} {.png .PNG}} {{All files} *}"
    .cdtEnv$tcl$data$filetypes6 <- "{{R Objects} {.rds .RDS .RData}} {{All files} *}"

    ## Initialization
    .cdtData$OpenFiles$Type <- list()
    .cdtData$OpenFiles$Data <- list()
    .cdtData$OpenTab$Type <- list()
    .cdtData$OpenTab$Data <- list()

    #################################################################################

    top.menu <- tkmenu(.cdtEnv$tcl$main$win, tearoff = FALSE)
    tkconfigure(.cdtEnv$tcl$main$win, menu = top.menu)

        ####################################

        menu.file <- tkmenu(top.menu, tearoff = FALSE, relief = "flat")
        tkadd(top.menu, "cascade", label = lang.menu[["file"]][["0"]], menu = menu.file, activebackground = 'lightblue')

            ##########

            tkadd(menu.file, "command", label = lang.menu[["file"]][["1"]],
                  command = function()
            {
                on.exit({
                    tkconfigure(.cdtEnv$tcl$main$win, cursor = '')
                    tcl('update')
                })
                tkconfigure(.cdtEnv$tcl$main$win, cursor = 'watch')
                tcl('update')

                dat.opfiles <- getOpenFiles(.cdtEnv$tcl$main$win)
                if(!is.null(dat.opfiles)) update.OpenFiles('ascii', dat.opfiles)
            })

            ##########
            tkadd(menu.file, "command", label = lang.menu[["file"]][["2"]],
                  command = function()
            {
                on.exit({
                    tkconfigure(.cdtEnv$tcl$main$win, cursor = '')
                    tcl('update')
                })
                tkconfigure(.cdtEnv$tcl$main$win, cursor = 'watch')
                tcl('update')

                nc.opfiles <- getOpenNetcdf(.cdtEnv$tcl$main$win)
                if(!is.null(nc.opfiles)) update.OpenFiles('netcdf', nc.opfiles)
            })

            ##########
            tkadd(menu.file, "command", label = lang.menu[["file"]][["3"]],
                  command = function()
            {
                on.exit({
                    tkconfigure(.cdtEnv$tcl$main$win, cursor = '')
                    tcl('update')
                })
                tkconfigure(.cdtEnv$tcl$main$win, cursor = 'watch')
                tcl('update')

                shp.opfiles <- getOpenShp(.cdtEnv$tcl$main$win)
                if(!is.null(shp.opfiles)) update.OpenFiles('shp', shp.opfiles)
            })

            ##########
            tkadd(menu.file, "separator")

            ##########

            tkadd(menu.file, "command", label = lang.menu[["file"]][["4"]],
                  command = function()
            {
                if(length(.cdtData$OpenTab$Type) == 0) return(NULL)
                tabid <- as.integer(tclvalue(tkindex(.cdtEnv$tcl$main$tknotes, 'current'))) + 1
                arrTypes <- c("arr", "chkcrds", "falsezero", "outqc", "outhom")
                if(!.cdtData$OpenTab$Type[[tabid]] %in% arrTypes) return(NULL)

                tab2sav <- try(Save_Notebook_Tab_Array(), silent = TRUE)
                if(!inherits(tab2sav, "try-error")){
                    if(!is.null(tab2sav))
                        Insert.Messages.Out(.cdtEnv$tcl$lang$global[['message']][['3']], TRUE, "s")
                }else{
                    Insert.Messages.Out(.cdtEnv$tcl$lang$global[['message']][['2']], format = TRUE)
                    Insert.Messages.Out(gsub('[\r\n]', '', tab2sav[1]), format = TRUE)
                }
            })

            ##########
            tkadd(menu.file, "command", label = lang.menu[["file"]][["5"]],
                  command = function()
            {
                Save_Table_As()
            })

            ##########
            tkadd(menu.file, "command", label = lang.menu[["file"]][["6"]],
                  command = function()
            {
                if(length(.cdtData$OpenTab$Type)){
                    tabid <- as.integer(tclvalue(tkindex(.cdtEnv$tcl$main$tknotes, 'current'))) + 1
                    if(.cdtData$OpenTab$Type[[tabid]] == "img") SavePlot()
                    tkfocus(.cdtEnv$tcl$main$win)
                }
            })

            ##########
            tkadd(menu.file, "separator")

            ##########
            tkadd(menu.file, "command", label = lang.menu[["file"]][["7"]],
                  command = function()
            {
                cdtConfiguration(.cdtEnv$tcl$main$win)
            })

            ##########
            tkadd(menu.file, "separator")

            ##########
            tkadd(menu.file, "command", label = lang.menu[["file"]][["8"]],
                  command = function()
            {
                on.exit({
                    rm(list = ls(envir = .cdtData), envir = .cdtData)
                    .cdtEnv$tcl <- NULL
                    options(warn = 0)
                })
                refreshCDT()
                tkdestroy(.cdtEnv$tcl$main$win)
            })

        ####################################

        menu.dataprep <- tkmenu(top.menu, tearoff = FALSE, relief = "flat")
        tkadd(top.menu, "cascade", label = lang.menu[["data.preparation"]][["0"]], menu = menu.dataprep, activebackground = 'lightblue')

            ##########
            tkadd(menu.dataprep, "command", label = lang.menu[["data.preparation"]][["1"]],
                  command = function()
            {
                refreshCDT(staterun = "normal")
                initialize.parameters('cdtInput.stn', 'daily')
                Format_CDT_Input_Station_Data()
            })

            ##########
            tkadd(menu.dataprep, "separator")

            ##########
            menu.cdt.data <- tkmenu(top.menu, tearoff = FALSE)
            tkadd(menu.dataprep, "cascade", label = lang.menu[["data.preparation"]][["2"]], menu = menu.cdt.data)

                ##########
                tkadd(menu.cdt.data, "command", label = lang.menu[["data.preparation"]][["2-1"]],
                      command = function()
                {
                    refreshCDT(staterun = "normal")
                    initialize.parameters('merge2CDT.stn', 'daily')
                    merge2CDTdata_getParams()
                })

                ########
                tkadd(menu.cdt.data, "separator")

                ##########
                tkadd(menu.cdt.data, "command", label = lang.menu[["data.preparation"]][["2-2"]],
                      command = function()
                {
                    refreshCDT(staterun = "normal")
                    initialize.parameters('filter.data', 'daily')
                    filterCDTData_getParams()
                })

                ########
                tkadd(menu.cdt.data, "separator")

                ##########
                tkadd(menu.cdt.data, "command", label = lang.menu[["data.preparation"]][["2-3"]],
                      command = function()
                {
                    refreshCDT(staterun = "normal")
                    initialize.parameters('selectCDT.data', 'daily')
                    selectCDTData_getParams()
                })

            ##########
            tkadd(menu.dataprep, "separator")

            ##########
            tkadd(menu.dataprep, "command", label = lang.menu[["data.preparation"]][["3"]],
                  command = function()
            {
                refreshCDT(staterun = "disabled")
                spinbox.state(state = 'normal')
                if(is.null(.cdtEnv$tcl$data$lcmd.frame)){
                    AssessDataPanelCmd()
                    .cdtEnv$tcl$data$lcmd.frame <- 1
                }
            })

            ##########
            tkadd(menu.dataprep, "separator")

            ##########
            menu.dataDown <- tkmenu(top.menu, tearoff = FALSE)
            tkadd(menu.dataprep, "cascade", label = lang.menu[["data.preparation"]][["6"]], menu = menu.dataDown)

                ##########
                tkadd(menu.dataDown, "command", label = lang.menu[["data.preparation"]][["6-1"]],
                      command = function()
                {
                    refreshCDT(staterun = "disabled")
                    initialize.parameters('down.DEM')
                    download_DEM()
                })

                ########
                tkadd(menu.dataDown, "separator")

                ##########
                tkadd(menu.dataDown, "command", label = lang.menu[["data.preparation"]][["6-2"]],
                      command = function()
                {
                    refreshCDT(staterun = "disabled")
                    initialize.parameters('down.SHP')
                    download_CountryShapefile()
                })

                ########
                tkadd(menu.dataDown, "separator")

                ##########
                tkadd(menu.dataDown, "command", label = lang.menu[["data.preparation"]][["6-3"]],
                      command = function()
                {
                    refreshCDT(staterun = "disabled")
                    initialize.parameters('down.RFE')
                    download_RFE()
                })

                ########
                tkadd(menu.dataDown, "separator")

                ##########
                tkadd(menu.dataDown, "command", label = lang.menu[["data.preparation"]][["6-4"]],
                      command = function()
                {
                    refreshCDT(staterun = "disabled")
                    initialize.parameters('down.Reanal')
                    download_Reanalysis()
                })

            ##########
            tkadd(menu.dataprep, "separator")

            ########
            menu.AggrData <- tkmenu(top.menu, tearoff = FALSE)
            tkadd(menu.dataprep, "cascade", label = lang.menu[["data.preparation"]][["7"]], menu = menu.AggrData)

                ##########
                tkadd(menu.AggrData, "command", label = lang.menu[["data.preparation"]][["7-1"]],
                      command = function()
                {
                    refreshCDT(staterun = "normal")
                    initialize.parameters('aggregate.ts', 'daily')
                    AggregateTS_GetInfo()
                })

                ###########
                tkadd(menu.AggrData, "separator")

                ##########
                tkadd(menu.AggrData, "command", label = lang.menu[["data.preparation"]][["7-2"]],
                      command = function()
                {
                    refreshCDT(staterun = "normal")
                    initialize.parameters('aggregate.nc')
                    AggregateNcdf_GetInfo()
                })

            ##########
            tkadd(menu.dataprep, "separator")

            ##########
            tkadd(menu.dataprep, "command", label = lang.menu[["data.preparation"]][["4"]],
                  command = function()
            {
                refreshCDT(staterun = "normal")
                initialize.parameters('create.CdtDataset', 'dekadal')
                cdtDataset_getParams()
            })

            ##########
            tkadd(menu.dataprep, "separator")

            #########
            tkadd(menu.dataprep, "command", label = lang.menu[["data.preparation"]][["10"]],
                command = function()
            {
                refreshCDT(staterun = "disabled")
                spinbox.state(state = 'normal')
                if(is.null(.cdtEnv$tcl$data$lcmd.frame)){
                    ExtractDataPanelCmd()
                    .cdtEnv$tcl$data$lcmd.frame <- 1
                }
            })

            ##########
            tkadd(menu.dataprep, "separator")

            ##########
            tkadd(menu.dataprep, "command", label = lang.menu[["data.preparation"]][["5"]],
                  command = function()
            {
                refreshCDT(staterun = "normal")
                initialize.parameters('split.NetCDF')
                split_3d.netcdf_getParams()
            })

            ##########
            tkadd(menu.dataprep, "separator")

            ##########
            menu.dataConv <- tkmenu(top.menu, tearoff = FALSE)
            tkadd(menu.dataprep, "cascade", label = lang.menu[["data.preparation"]][["9"]], menu = menu.dataConv)

                ########
                tkadd(menu.dataConv, "command", label = lang.menu[["data.preparation"]][["9-1"]],
                      command = function()
                {
                    refreshCDT(staterun = "normal")
                    initialize.parameters('convert.CPTdata', 'daily')
                    CPT.convert_getParams()
                })

                ########
                tkadd(menu.dataConv, "separator")

                ########
                tkadd(menu.dataConv, "command", label = lang.menu[["data.preparation"]][["9-2"]],
                      command = function()
                {
                    refreshCDT(staterun = "normal")
                    initialize.parameters('convert.nc.tif.bil', 'daily')
                    rasterData.convert_getParams()
                })

                ########
                tkadd(menu.dataConv, "separator")

                ########
                tkadd(menu.dataConv, "command", label = lang.menu[["data.preparation"]][["9-3"]],
                      command = function()
                {
                    refreshCDT(staterun = "normal")
                    initialize.parameters('grads.ctl', 'daily')
                    grads_create.ctl_getParams()
                })

            ##########
            tkadd(menu.dataprep, "separator")

            ##########
            tkadd(menu.dataprep, "command", label = lang.menu[["data.preparation"]][["8"]],
                  command = function()
            {
                refreshCDT(staterun = "normal")
                initialize.parameters('fill.temp', 'dekadal')
                fill_Miss_DekTemp()
            })

        ####################################

        menu.qchom <- tkmenu(top.menu, tearoff = FALSE, relief = "flat")
        tkadd(top.menu, "cascade", label = lang.menu[["quality.control"]][["0"]], menu = menu.qchom, activebackground = 'lightblue')

            ##########
            tkadd(menu.qchom, "command", label = lang.menu[["quality.control"]][["1"]],
                  command = function()
            {
                refreshCDT(staterun = "disabled")
                spinbox.state(state = 'normal')
                if(is.null(.cdtEnv$tcl$data$lcmd.frame)){
                    StnChkCoordsPanelCmd()
                    .cdtEnv$tcl$data$lcmd.frame <- 1
                }
            })

            ##########
            tkadd(menu.qchom, "separator")

            ##########
            tkadd(menu.qchom, "command", label = lang.menu[["quality.control"]][["2"]],
                  command = function()
            {
                refreshCDT(staterun = "disabled")
                spinbox.state(state = 'normal')
                if(is.null(.cdtEnv$tcl$data$lcmd.frame)){
                    qcRRZeroCheckPanelCmd()
                    .cdtEnv$tcl$data$lcmd.frame <- 1
                }
            })

            ##########
            tkadd(menu.qchom, "separator")

            ##########
            tkadd(menu.qchom, "command", label = lang.menu[["quality.control"]][["3"]],
                  command = function()
            {
                refreshCDT(staterun = "disabled")
                spinbox.state(state = 'normal')
                if(is.null(.cdtEnv$tcl$data$lcmd.frame)){
                    qcRROutlierCheckPanelCmd()
                    .cdtEnv$tcl$data$lcmd.frame <- 1
                }
            })

            ##########
            tkadd(menu.qchom, "separator")

            ##########
            tkadd(menu.qchom, "command", label = lang.menu[["quality.control"]][["4"]],
                  command = function()
            {
                refreshCDT(staterun = "disabled")
                spinbox.state(state = 'normal')
                if(is.null(.cdtEnv$tcl$data$lcmd.frame)){
                    qcTTOutlierCheckPanelCmd()
                    .cdtEnv$tcl$data$lcmd.frame <- 1
                }
            })

            ##########
            tkadd(menu.qchom, "separator")

            ##########
            tkadd(menu.qchom, "command", label = lang.menu[["quality.control"]][["5"]],
                  command = function()
            {
                refreshCDT(staterun = "disabled")
                spinbox.state(state = 'normal')
                if(is.null(.cdtEnv$tcl$data$lcmd.frame)){
                    testHomogeneityPanelCmd()
                    .cdtEnv$tcl$data$lcmd.frame <- 1
                }
            })

        ####################################

        menu.mrg <- tkmenu(top.menu, tearoff = FALSE, relief = "flat")
        tkadd(top.menu, "cascade", label = lang.menu[["merging.data"]][["0"]], menu = menu.mrg, activebackground = 'lightblue')

            ##########
            tkadd(menu.mrg, "command", label = lang.menu[["merging.data"]][["1"]],
                  command = function()
            {
                refreshCDT(staterun = "disabled")
                spinbox.state(state = 'normal')
                if(is.null(.cdtEnv$tcl$data$lcmd.frame)){
                    SpatialInterpPanelCmd()
                    .cdtEnv$tcl$data$lcmd.frame <- 1
                }
            })

            ##########
            tkadd(menu.mrg, "separator")

            ###########
            menu.mrg.rain <- tkmenu(top.menu, tearoff = FALSE)
            tkadd(menu.mrg, "cascade", label = lang.menu[["merging.data"]][["2"]], menu = menu.mrg.rain)

                ##########
                tkadd(menu.mrg.rain, "command", label = lang.menu[["merging.data"]][["2-1"]], background = 'lightblue',
                      command = function()
                {
                    refreshCDT(staterun = "normal")
                    initialize.parameters('coefbias.rain', 'dekadal')
                    coefBiasGetInfoRain()
                })

                ########
                tkadd(menu.mrg.rain, "separator")

                ##########
                tkadd(menu.mrg.rain, "command", label = lang.menu[["merging.data"]][["2-2"]],
                      command = function()
                {
                        refreshCDT(staterun = "normal")
                        initialize.parameters('rmbias.rain', 'dekadal')
                        rmvBiasGetInfoRain()
                })

                 ########
                tkadd(menu.mrg.rain, "separator")

                ##########
                tkadd(menu.mrg.rain, "command", label = lang.menu[["merging.data"]][["2-3"]],
                      command = function()
                {
                        refreshCDT(staterun = "normal")
                        initialize.parameters('merge.rain', 'dekadal')
                        mergeGetInfoRain()
                })

            ##########
            tkadd(menu.mrg, "separator")

            ###########
            menu.mrg.temp <- tkmenu(top.menu, tearoff = FALSE)
            tkadd(menu.mrg, "cascade", label = lang.menu[["merging.data"]][["3"]], menu = menu.mrg.temp)

                ##########
                tkadd(menu.mrg.temp, "command", label = lang.menu[["merging.data"]][["3-1"]], background = 'lightblue',
                      command = function()
                {
                    refreshCDT(staterun = "normal")
                    initialize.parameters('coefdown.temp', 'dekadal')
                    Temp_coefDownGetInfo()
                })

                ########
                tkadd(menu.mrg.temp, "separator")

                ##########
                tkadd(menu.mrg.temp, "command", label = lang.menu[["merging.data"]][["3-2"]],
                      command = function()
                {
                    refreshCDT(staterun = "normal")
                    initialize.parameters('down.temp', 'dekadal')
                    Temp_reanalDownGetInfo()
                })

                 ########
                tkadd(menu.mrg.temp, "separator")

                ##########
                tkadd(menu.mrg.temp, "command", label = lang.menu[["merging.data"]][["3-3"]], background = 'lightblue',
                      command = function()
                {
                    refreshCDT(staterun = "normal")
                    initialize.parameters('coefbias.temp', 'dekadal')
                    biasGetInfoTempDown()
                })

                ########
                tkadd(menu.mrg.temp, "separator")

                ##########
                tkadd(menu.mrg.temp, "command", label = lang.menu[["merging.data"]][["3-4"]],
                      command = function()
                {
                    refreshCDT(staterun = "normal")
                    initialize.parameters('adjust.temp', 'dekadal')
                    adjGetInfoTempDownReanal()
                })

                 ########
                tkadd(menu.mrg.temp, "separator")

                ##########
                tkadd(menu.mrg.temp, "command", label = lang.menu[["merging.data"]][["3-5"]],
                      command = function()
                {
                    refreshCDT(staterun = "normal")
                    initialize.parameters('merge.temp', 'dekadal')
                    mrgGetInfoTemp()
                })

            # ##########
            # tkadd(menu.mrg, "separator")

            # ###########
            # tkadd(menu.mrg, "command", label = lang.menu[["merging.data"]][["4"]],
            #       command = function()
            # {
            #     refreshCDT(staterun = "normal")
            #     initialize.parameters('merge.dekrain', 'dekadal')
            #     # mergeDekadInfoRain()
            # })

            ##########
            tkadd(menu.mrg, "separator")

            ##########
            tkadd(menu.mrg, "command", label = lang.menu[["merging.data"]][["5"]],
                  command = function()
            {
                refreshCDT(staterun = "normal")
                initialize.parameters('scale.merged', 'daily')
                Merging_ScaleDataInfo()
            })

            ##########
            tkadd(menu.mrg, "separator")

            ###########
            menu.mrg.loocv <- tkmenu(top.menu, tearoff = FALSE)
            tkadd(menu.mrg, "cascade", label = lang.menu[["merging.data"]][["6"]], menu = menu.mrg.loocv)

                ########
                tkadd(menu.mrg.loocv, "command", label = lang.menu[["merging.data"]][["6-1"]],
                      command = function()
                {
                    refreshCDT(staterun = "normal")
                    initialize.parameters('crossv.rain', 'dekadal')
                    crossValidationInfoRain()
                })

                ########
                tkadd( menu.mrg.loocv, "separator")

                ########
                tkadd(menu.mrg.loocv, "command", label = lang.menu[["merging.data"]][["6-2"]],
                    command = function()
                {
                    refreshCDT(staterun = "normal")
                    initialize.parameters('crossv.temp', 'dekadal')
                    crossValidationInfoTemp()
                })

        ####################################

        menu.valid <- tkmenu(top.menu, tearoff = FALSE, relief = "flat")
        tkadd(top.menu, "cascade", label = lang.menu[["validation"]][["0"]], menu = menu.valid, activebackground = 'lightblue')

            ##########
            menu.valid.spts <- tkmenu(top.menu, tearoff = FALSE)
            tkadd(menu.valid, "cascade", label = lang.menu[["validation"]][["1"]], menu = menu.valid.spts)

                ########
                tkadd(menu.valid.spts, "command", label = lang.menu[["validation"]][["1-1"]],
                      command = function()
                {
                    refreshCDT(staterun = "disabled")
                    spinbox.state(state = 'normal')
                    if(is.null(.cdtEnv$tcl$data$lcmd.frame)){
                        Validation.STAT.PanelCmd("RR")
                        .cdtEnv$tcl$data$lcmd.frame <- 1
                    }
                })

                ########
                tkadd(menu.valid.spts, "separator")

                ########
                tkadd(menu.valid.spts, "command", label = lang.menu[["validation"]][["1-2"]],
                      command = function()
                {
                    refreshCDT(staterun = "disabled")
                    spinbox.state(state = 'normal')
                    if(is.null(.cdtEnv$tcl$data$lcmd.frame)){
                        Validation.STAT.PanelCmd("TT")
                        .cdtEnv$tcl$data$lcmd.frame <- 1
                    }
                })

            ##########
            tkadd(menu.valid, "separator")

            ##########
            menu.valid.grd <- tkmenu(top.menu, tearoff = FALSE)
            tkadd(menu.valid, "cascade", label = lang.menu[["validation"]][["2"]], menu = menu.valid.grd)

                ########
                tkadd(menu.valid.grd, "command", label = lang.menu[["validation"]][["2-1"]],
                      command = function()
                {
                    refreshCDT(staterun = "disabled")
                    spinbox.state(state = 'normal')
                    if(is.null(.cdtEnv$tcl$data$lcmd.frame)){
                        Validation.HOV.PanelCmd('RR')
                        .cdtEnv$tcl$data$lcmd.frame <- 1
                    }
                })

                ########
                tkadd(menu.valid.grd, "separator")

                ########
                tkadd(menu.valid.grd, "command", label = lang.menu[["validation"]][["2-2"]],
                      command = function()
                {
                    refreshCDT(staterun = "disabled")
                    spinbox.state(state = 'normal')
                    if(is.null(.cdtEnv$tcl$data$lcmd.frame)){
                        Validation.HOV.PanelCmd('TT')
                        .cdtEnv$tcl$data$lcmd.frame <- 1
                    }
                })

            ##########
            tkadd(menu.valid, "separator")

            ##########
            menu.valid.stats <- tkmenu(top.menu, tearoff = FALSE)
            tkadd(menu.valid, "cascade", label = lang.menu[["validation"]][["3"]], menu = menu.valid.stats)

                ########
                tkadd(menu.valid.stats, "command", label = lang.menu[["validation"]][["3-1"]],
                      command = function()
                {
                    refreshCDT(staterun = "disabled")
                    spinbox.state(state = 'normal')
                    if(is.null(.cdtEnv$tcl$data$lcmd.frame)){
                        Validation.MultiData.PanelCmd('RR')
                        .cdtEnv$tcl$data$lcmd.frame <- 1
                    }
                })

                ########
                tkadd(menu.valid.stats, "separator")

                ########
                tkadd(menu.valid.stats, "command", label = lang.menu[["validation"]][["3-2"]],
                      command = function()
                {
                    refreshCDT(staterun = "disabled")
                    spinbox.state(state = 'normal')
                    if(is.null(.cdtEnv$tcl$data$lcmd.frame)){
                        Validation.MultiData.PanelCmd('TT')
                        .cdtEnv$tcl$data$lcmd.frame <- 1
                    }
                })

        ####################################

        menu.dataproc <- tkmenu(top.menu, tearoff = FALSE, relief = "flat")
        tkadd(top.menu, "cascade", label = lang.menu[["data.analysis"]][["0"]], menu = menu.dataproc, activebackground = 'lightblue')

            ##########
            tkadd(menu.dataproc, "command", label = lang.menu[["data.analysis"]][["1"]],
                command = function()
            {
                refreshCDT(staterun = "disabled")
                spinbox.state(state = 'normal')
                if(is.null(.cdtEnv$tcl$data$lcmd.frame)){
                    summariesDataPanelCmd()
                    .cdtEnv$tcl$data$lcmd.frame <- 1
                }
            })

            ##########
            tkadd(menu.dataproc, "separator")

            ##########
            menu.ClimVars <- tkmenu(top.menu, tearoff = FALSE)
            tkadd(menu.dataproc, "cascade", label = lang.menu[["data.analysis"]][["2"]], menu = menu.ClimVars)

                ########
                tkadd(menu.ClimVars, "command", label = lang.menu[["data.analysis"]][["2-1"]],
                      command = function()
                {
                    refreshCDT(staterun = "normal")
                    initialize.parameters('compute.dervTemp', 'daily')
                    computeTvars_getParams()
                })

                ########
                tkadd(menu.ClimVars, "separator")

                ########
                tkadd(menu.ClimVars, "command", label = lang.menu[["data.analysis"]][["2-2"]],
                      command = function()
                {
                    refreshCDT(staterun = "normal")
                    initialize.parameters('compute.PET', 'daily')
                    computePET_getParams()
                })

                ########
                tkadd(menu.ClimVars, "separator")

                ########
                tkadd(menu.ClimVars, "command", label = lang.menu[["data.analysis"]][["2-3"]],
                      command = function()
                {
                    refreshCDT(staterun = "normal")
                    initialize.parameters('compute.WB', 'daily')
                    computeWB_getParams()
                })

            ##########
            tkadd(menu.dataproc, "separator")

            ##########
            menu.ClimCalc <- tkmenu(top.menu, tearoff = FALSE)
            tkadd(menu.dataproc, "cascade", label = lang.menu[["data.analysis"]][["3"]], menu = menu.ClimCalc)

                ########
                tkadd(menu.ClimCalc, "command", label = lang.menu[["data.analysis"]][["3-1"]],
                      command = function()
                {
                    refreshCDT(staterun = "disabled")
                    spinbox.state(state = 'normal')
                    if(is.null(.cdtEnv$tcl$data$lcmd.frame)){
                        climatologiesCalcPanelCmd()
                        .cdtEnv$tcl$data$lcmd.frame <- 1
                    }
                })

                ########
                tkadd(menu.ClimCalc, "separator")

                ########
                tkadd(menu.ClimCalc, "command", label = lang.menu[["data.analysis"]][["3-2"]],
                      command = function()
                {
                    refreshCDT(staterun = "disabled")
                    spinbox.state(state = 'normal')
                    if(is.null(.cdtEnv$tcl$data$lcmd.frame)){
                        anomaliesCalcPanelCmd()
                        .cdtEnv$tcl$data$lcmd.frame <- 1
                    }
                })

            ##########
            tkadd(menu.dataproc, "separator")

            ##########
            tkadd(menu.dataproc, "command", label = lang.menu[["data.analysis"]][["4"]],
                  command = function()
            {
                refreshCDT(staterun = "disabled")
                spinbox.state(state = 'normal')
                if(is.null(.cdtEnv$tcl$data$lcmd.frame)){
                    spatialAnalysisPanelCmd()
                    .cdtEnv$tcl$data$lcmd.frame <- 1
                }
            })

            ##########
            tkadd(menu.dataproc, "separator")

            ##########
            tkadd(menu.dataproc, "command", label = lang.menu[["data.analysis"]][["5"]],
                  command = function()
            {
                refreshCDT(staterun = "disabled")
                spinbox.state(state = 'normal')
                if(is.null(.cdtEnv$tcl$data$lcmd.frame)){
                    dailyRainAnalysisPanelCmd()
                    .cdtEnv$tcl$data$lcmd.frame <- 1
                }
            })

            ##########
            tkadd(menu.dataproc, "separator")

            ##########
            menu.rainySeas <- tkmenu(top.menu, tearoff = FALSE)
            tkadd(menu.dataproc, "cascade", label = lang.menu[["data.analysis"]][["6"]], menu = menu.rainySeas)

                ########
                tkadd(menu.rainySeas, "command", label = lang.menu[["data.analysis"]][["6-1"]],
                      command = function()
                {
                    refreshCDT(staterun = "disabled")
                    spinbox.state(state = 'normal')
                    if(is.null(.cdtEnv$tcl$data$lcmd.frame)){
                        OnsetCalcPanelCmd()
                        .cdtEnv$tcl$data$lcmd.frame <- 1
                    }
                })

                ########
                tkadd(menu.rainySeas, "separator")

                ########
                tkadd(menu.rainySeas, "command", label = lang.menu[["data.analysis"]][["6-2"]],
                      command = function()
                {
                    refreshCDT(staterun = "disabled")
                    spinbox.state(state = 'normal')
                    if(is.null(.cdtEnv$tcl$data$lcmd.frame)){
                        CessationCalcPanelCmd()
                        .cdtEnv$tcl$data$lcmd.frame <- 1
                    }
                })

                ########
                tkadd(menu.rainySeas, "separator")

                ########
                tkadd(menu.rainySeas, "command", label = lang.menu[["data.analysis"]][["6-3"]],
                      command = function()
                {
                    refreshCDT(staterun = "disabled")
                    spinbox.state(state = 'normal')
                    if(is.null(.cdtEnv$tcl$data$lcmd.frame)){
                        SeasonLengthCalcPanelCmd()
                        .cdtEnv$tcl$data$lcmd.frame <- 1
                    }
                })

            ##########
            tkadd(menu.dataproc, "command", label = lang.menu[["data.analysis"]][["7"]],
                  command = function()
            {
                refreshCDT(staterun = "disabled")
                spinbox.state(state = 'normal')
                if(is.null(.cdtEnv$tcl$data$lcmd.frame)){
                    SeasonAnalysisPanelCmd()
                    .cdtEnv$tcl$data$lcmd.frame <- 1
                }
            })

            ##########
            tkadd(menu.dataproc, "separator")

            ##########
            menu.climdex <- tkmenu(top.menu, tearoff = FALSE)
            tkadd(menu.dataproc, "cascade", label = lang.menu[["data.analysis"]][["8"]], menu = menu.climdex)

                ########
                tkadd(menu.climdex, "command", label = lang.menu[["data.analysis"]][["8-1"]],
                      command = function()
                {
                    refreshCDT(staterun = "disabled")
                    spinbox.state(state = 'normal')
                    if(is.null(.cdtEnv$tcl$data$lcmd.frame)){
                        climdexPanelCmd.RR()
                        .cdtEnv$tcl$data$lcmd.frame <- 1
                    }
                })

                ########
                tkadd(menu.climdex, "separator")

                ########
                tkadd(menu.climdex, "command", label = lang.menu[["data.analysis"]][["8-2"]],
                      command = function()
                {
                    refreshCDT(staterun = "disabled")
                    spinbox.state(state = 'normal')
                    if(is.null(.cdtEnv$tcl$data$lcmd.frame)){
                        climdexPanelCmd.TT()
                        .cdtEnv$tcl$data$lcmd.frame <- 1
                    }
                })

            ##########
            tkadd(menu.dataproc, "separator")

            ##########
            menu.drought <- tkmenu(top.menu, tearoff = FALSE)
            tkadd(menu.dataproc, "cascade", label = lang.menu[["data.analysis"]][["9"]], menu = menu.drought)

                ########
                ## Standardized Precipitation Index (SPI)
                ## https://climatedataguide.ucar.edu/climate-data/standardized-precipitation-index-spi
                tkadd(menu.drought, "command", label = lang.menu[["data.analysis"]][["9-1"]],
                      command = function()
                {
                    refreshCDT(staterun = "disabled")
                    spinbox.state(state = 'normal')
                    if(is.null(.cdtEnv$tcl$data$lcmd.frame)){
                        SPICalcPanelCmd()
                        .cdtEnv$tcl$data$lcmd.frame <- 1
                    }
                })

                ##########
                tkadd(menu.drought, "separator")

                #########
                ## Standardized Precipitation Evapotranspiration Index (SPEI)
                ## https://climatedataguide.ucar.edu/climate-data/standardized-precipitation-evapotranspiration-index-spei
                tkadd(menu.drought, "command", label = lang.menu[["data.analysis"]][["9-2"]],
                      command = function()
                {
                    refreshCDT(staterun = "disabled")
                    spinbox.state(state = 'normal')
                    if(is.null(.cdtEnv$tcl$data$lcmd.frame)){
                        SPEICalcPanelCmd()
                        .cdtEnv$tcl$data$lcmd.frame <- 1
                    }
                })

                ##########
                tkadd(menu.drought, "separator")

                #########
                tkadd(menu.drought, "command", label = lang.menu[["data.analysis"]][["9-3"]],
                      command = function()
                {
                    refreshCDT(staterun = "disabled")
                    spinbox.state(state = 'normal')
                    if(is.null(.cdtEnv$tcl$data$lcmd.frame)){
                        DecilesCalcPanelCmd()
                        .cdtEnv$tcl$data$lcmd.frame <- 1
                    }
                })

        ###################################

        menu.plot <- tkmenu(top.menu, tearoff = FALSE, relief = "flat")
        tkadd(top.menu, "cascade", label = lang.menu[["plot.data"]][["0"]], menu = menu.plot, activebackground = 'lightblue')

            ##########
            tkadd(menu.plot, "command", label = lang.menu[["plot.data"]][["1"]],
                command = function()
            {
                refreshCDT(staterun = "disabled")
                spinbox.state(state = 'normal')
                if(is.null(.cdtEnv$tcl$data$lcmd.frame)){
                    PlotCDTStationCmd()
                    .cdtEnv$tcl$data$lcmd.frame <- 1
                }
            })

            ##########
            tkadd(menu.plot, "separator")

            ##########
            tkadd(menu.plot, "command", label = lang.menu[["plot.data"]][["2"]],
                command = function()
            {
                refreshCDT()
                spinbox.state(state = 'normal')
                if(is.null(.cdtEnv$tcl$data$lcmd.frame)){
                    PlotCDTDatasetCmd()
                    .cdtEnv$tcl$data$lcmd.frame <- 1
                }
            })

            ##########
            tkadd(menu.plot, "separator")

            ##########
            tkadd(menu.plot, "command", label = lang.menu[["plot.data"]][["3"]],
                command = function()
            {
                refreshCDT()
                spinbox.state(state = 'normal')
                if(is.null(.cdtEnv$tcl$data$lcmd.frame)){
                    PlotOneNetCDFFileCmd()
                    .cdtEnv$tcl$data$lcmd.frame <- 1
                }
            })

            ##########
            tkadd(menu.plot, "separator")

            ##########
            tkadd(menu.plot, "command", label = lang.menu[["plot.data"]][["4"]],
                command = function()
            {
                refreshCDT()
                spinbox.state(state = 'normal')
                if(is.null(.cdtEnv$tcl$data$lcmd.frame)){
                    PlotSeqNetCDFFilesCmd()
                    .cdtEnv$tcl$data$lcmd.frame <- 1
                }
            })

            ##########
            tkadd(menu.plot, "separator")

            ##########
            tkadd(menu.plot, "command", label = lang.menu[["plot.data"]][["5"]],
                command = function()
            {
                refreshCDT()
                spinbox.state(state = 'normal')
                if(is.null(.cdtEnv$tcl$data$lcmd.frame)){
                    PlotVarNetCDFFilesCmd()
                    .cdtEnv$tcl$data$lcmd.frame <- 1
                }
            })

            ##########
            tkadd(menu.plot, "separator")

            ##########
            tkadd(menu.plot, "command", label = lang.menu[["plot.data"]][["6"]],
                command = function()
            {
                refreshCDT()
                spinbox.state(state = 'normal')
                if(is.null(.cdtEnv$tcl$data$lcmd.frame)){
                    PlotMulitpleDataCmd()
                    .cdtEnv$tcl$data$lcmd.frame <- 1
                }
            })

        ###################################

        menu.aide <- tkmenu(top.menu, tearoff = FALSE, relief = "flat")
        tkadd(top.menu, "cascade", label = lang.menu[["help"]][["0"]], menu = menu.aide, activebackground = 'lightblue')

            ##########
            tkadd(menu.aide, "command", label = lang.menu[["help"]][["1"]], command = function(){
                # start help server ifnot
                # utils::browseURL(paste0('file://',file.path(.cdtDir$Root, 'help', 'index.html')))
                msg <- "You will be able to download from <https://iri.columbia.edu/our-expertise/climate/tools/cdt> CDT user guide and technical documentation, hopefully soon."
                Insert.Messages.Out(msg, TRUE, 'i')
                utils::browseURL("https://iri.columbia.edu/our-expertise/climate/tools/cdt/")
            })

            ##########
            tkadd(menu.aide, "separator")

            ##########
            tkadd(menu.aide, "command", label = lang.menu[["help"]][["2"]], command = function(){
                Insert.Messages.Out(.cdtEnv$tcl$lang$global[['message']][['11']], TRUE, 'i')
                tkconfigure(.cdtEnv$tcl$main$win, cursor = 'watch')
                tcl('update')
                on.exit({
                    tkconfigure(.cdtEnv$tcl$main$win, cursor = '')
                    tcl('update')
                })

                if(testConnection()){
                    if(Sys.getenv("TAR")[1] == "") Sys.setenv("TAR" = "internal")
                    devtools::install_github("rijaf-iri/CDT", dependencies = FALSE, upgrade_dependencies = FALSE, force = TRUE)
                    Insert.Messages.Out(.cdtEnv$tcl$lang$global[['message']][['12']], TRUE, 's')
                    Insert.Messages.Out(.cdtEnv$tcl$lang$global[['message']][['13']], TRUE, 'i')
                }else{
                    Insert.Messages.Out(.cdtEnv$tcl$lang$global[['message']][['14']], TRUE, 'e')
                    return(NULL)
                }
            })

            ##########
            tkadd(menu.aide, "separator")

            ##########
            tkadd(menu.aide, "command", label = lang.menu[["help"]][["3"]], command = function() aboutCDT())

    #################################################################################

    tkbind(top.menu, "<ButtonPress-1>", function(){
        tclvalue(.cdtEnv$tcl$status$pbLab) <- ""
        tclvalue(.cdtEnv$tcl$status$pbBar) <- 0
    })
 
    #################################################################################

    # Toolbars
    tools.frame <- tkframe(.cdtEnv$tcl$main$win, bd = 2, relief = 'ridge')

        ####################################

        if(WindowsOS()) {
            horiz <- .cdtEnv$tcl$fun$w.scale(70) / 385
            verti <- .cdtEnv$tcl$fun$h.scale(60) / 385
        }else{
            horiz <- .cdtEnv$tcl$fun$w.scale(70) / 480
            verti <- .cdtEnv$tcl$fun$h.scale(60) / 480
        }
        horizS <- round(horiz, 1)
        vertiS <- round(verti, 1)

        xml.toolbar <- file.path(.cdtDir$dirLocal, "languages", "cdt_toolbars_help.xml")
        lang.toolbar <- cdtLanguageParse(xml.toolbar, .cdtData$Config$lang.iso)

        ##################

        tb.open.file <- tkbutton.toolbar(tools.frame, "open_file24.gif", lang.toolbar[['tooltip']][['1']], lang.toolbar[['status']][['1']])
        tb.save.image <- tkbutton.toolbar(tools.frame, "save_img24.gif", lang.toolbar[['tooltip']][['2']], lang.toolbar[['status']][['2']])
        tb.open.table <- tkbutton.toolbar(tools.frame, "open_table24.gif", lang.toolbar[['tooltip']][['3']], lang.toolbar[['status']][['3']])
        tb.save.table <- tkbutton.toolbar(tools.frame, "save_table24.gif", lang.toolbar[['tooltip']][['4']], lang.toolbar[['status']][['4']])

        ###
        .cdtEnv$tcl$toolbar$run <- tkbutton.toolbar(tools.frame, "execute24.gif", lang.toolbar[['tooltip']][['5']], lang.toolbar[['status']][['5']])
        tkconfigure(.cdtEnv$tcl$toolbar$run, state = "disabled")

        ###
        lspinH <- tklabel(tools.frame, text = lang.toolbar[['label']][['1']])
        .cdtEnv$tcl$toolbar$spinH <- ttkspinbox(tools.frame, from = 0.5, to = 5.0, increment = 0.1, justify = 'center', width = 6, state = 'disabled')
        tkset(.cdtEnv$tcl$toolbar$spinH, horizS)

        helpWidget(.cdtEnv$tcl$toolbar$spinH, lang.toolbar[['tooltip']][['6']], lang.toolbar[['status']][['6']])

        ###
        lspinV <- tklabel(tools.frame, text = lang.toolbar[['label']][['2']])
        .cdtEnv$tcl$toolbar$spinV <- ttkspinbox(tools.frame, from = 0.5, to = 5.0, increment = 0.1, justify = 'center', width = 6, state = 'disabled')
        tkset(.cdtEnv$tcl$toolbar$spinV, vertiS)

        helpWidget(.cdtEnv$tcl$toolbar$spinV, lang.toolbar[['tooltip']][['7']], lang.toolbar[['status']][['7']])

        ###
        .cdtEnv$tcl$toolbar$plotRedraw <- tkbutton.toolbar(tools.frame, "RedrawButton24.gif", lang.toolbar[['tooltip']][['8']], lang.toolbar[['status']][['8']])
        helpWidget(.cdtEnv$tcl$toolbar$plotRedraw, lang.toolbar[['tooltip']][['11']], lang.toolbar[['status']][['11']])

        ###
        tb.close.tab <- tkbutton.toolbar(tools.frame, "close_tab24.gif", lang.toolbar[['tooltip']][['9']], lang.toolbar[['status']][['9']])
        tb.exit.win <- tkbutton.toolbar(tools.frame, "quit_cdt24.gif", lang.toolbar[['tooltip']][['10']], lang.toolbar[['status']][['10']])

        ###
        tb.separator0 <- ttkseparator(tools.frame, orient = 'vertical')
        tb.separator1 <- ttkseparator(tools.frame, orient = 'vertical')
        tb.separator2 <- ttkseparator(tools.frame, orient = 'vertical')
        tb.separator3 <- ttkseparator(tools.frame, orient = 'vertical')

        ##################

        tkgrid(
                tb.open.file, tb.save.image,
                tb.separator0,
                tb.open.table, tb.save.table,
                tb.separator1,
                .cdtEnv$tcl$toolbar$run,
                tb.separator2,
                lspinH, .cdtEnv$tcl$toolbar$spinH,
                lspinV, .cdtEnv$tcl$toolbar$spinV,
                .cdtEnv$tcl$toolbar$plotRedraw,
                tb.separator3,
                tb.close.tab, tb.exit.win
            )

        ###
        tkgrid.configure(tb.separator0, sticky = 'ns')
        tkgrid.configure(tb.separator1, sticky = 'ns')
        tkgrid.configure(tb.separator2, sticky = 'ns', padx = 20)
        tkgrid.configure(tb.separator3, sticky = 'ns', padx = 20)

        tkgrid.configure(tb.open.file, padx = 5)
        tkgrid.configure(tb.save.image, padx = 5)

        tkgrid.configure(tb.open.table, padx = 5)
        tkgrid.configure(tb.save.table, padx = 5)

        ###
        tkgrid.configure(.cdtEnv$tcl$toolbar$run, padx = 20, ipadx = 5)

        ###
        tkgrid.configure(.cdtEnv$tcl$toolbar$plotRedraw, padx = 5)

        ###
        tkgrid.configure(tb.close.tab, padx = 5)
        tkgrid.configure(tb.exit.win, padx = 30, sticky = 'e')

        ##################

        hRedraw <- tkimage.create('photo', '-file', file.path(.cdtDir$Root, "images", 'RedrawButton24.gif'))
        hRedraw1 <- tkimage.create('photo', '-file', file.path(.cdtDir$Root, "images", 'RedrawButton-Change24.gif'))

        tkbind(.cdtEnv$tcl$toolbar$plotRedraw, "<ButtonRelease>", function(){
            tkconfigure(.cdtEnv$tcl$toolbar$plotRedraw, image = hRedraw)
        })

        tkbind(.cdtEnv$tcl$toolbar$spinH, "<<Increment>>", function(){
            tkconfigure(.cdtEnv$tcl$toolbar$plotRedraw, image = hRedraw1)
        })
        tkbind(.cdtEnv$tcl$toolbar$spinH, "<<Decrement>>", function(){
            tkconfigure(.cdtEnv$tcl$toolbar$plotRedraw, image = hRedraw1)
        })
        tkbind(.cdtEnv$tcl$toolbar$spinV, "<<Increment>>", function(){
            tkconfigure(.cdtEnv$tcl$toolbar$plotRedraw, image = hRedraw1)
        })
        tkbind(.cdtEnv$tcl$toolbar$spinV, "<<Decrement>>", function(){
            tkconfigure(.cdtEnv$tcl$toolbar$plotRedraw, image = hRedraw1)
        })

        ##################

        tkconfigure(tb.exit.win, command = function(){
            on.exit({
                rm(list = ls(envir = .cdtData), envir = .cdtData)
                .cdtEnv$tcl <- NULL
                options(warn = 0)
            })
            refreshCDT()
            tkdestroy(.cdtEnv$tcl$main$win)
        })

        ##################

        tkconfigure(tb.close.tab, command = function(){
            tabid <- as.integer(tclvalue(tkindex(.cdtEnv$tcl$main$tknotes, 'current')))
            Close_Notebook_Tab(tabid)
        })

        ##################

        tkconfigure(tb.open.file, command = function(){
            on.exit({
                tkconfigure(.cdtEnv$tcl$main$win, cursor = '')
                tcl('update')
            })
            tkconfigure(.cdtEnv$tcl$main$win, cursor = 'watch')
            tcl('update')

            dat.opfiles <- getOpenFiles(.cdtEnv$tcl$main$win)
            if(!is.null(dat.opfiles)) update.OpenFiles('ascii', dat.opfiles)
        })

        ##################

        tkconfigure(tb.save.image, command = function(){
            if(length(.cdtData$OpenTab$Type)){
                tabid <- as.integer(tclvalue(tkindex(.cdtEnv$tcl$main$tknotes, 'current'))) + 1
                if(.cdtData$OpenTab$Type[[tabid]] == "img") SavePlot()
                tkfocus(.cdtEnv$tcl$main$win)
            }
        })

        ##################

        tkconfigure(tb.open.table, command = function() {
            tab.array <- Display_Array_Tab(.cdtEnv$tcl$main$win)
            if(!is.null(tab.array)){
                ntab <- update.OpenTabs('arr', tab.array)
                tkselect(.cdtEnv$tcl$main$tknotes, ntab)
            }
        })

        ##################

        tkconfigure(tb.save.table, command = function(){
            if(length(.cdtData$OpenTab$Type) == 0) return(NULL)
            tabid <- as.integer(tclvalue(tkindex(.cdtEnv$tcl$main$tknotes, 'current'))) + 1
            arrTypes <- c("arr", "chkcrds", "falsezero", "outqc", "outhom")
            if(!.cdtData$OpenTab$Type[[tabid]] %in% arrTypes) return(NULL)

            tab2sav <- try(Save_Notebook_Tab_Array(), silent = TRUE)
            if(inherits(tab2sav, "try-error")){
                Insert.Messages.Out(gsub('[\r\n]', '', tab2sav[1]), format = TRUE)
                Insert.Messages.Out(lang.toolbar[['message']][['2']], format = TRUE)
            }else{
                if(!is.null(tab2sav))
                    Insert.Messages.Out(lang.toolbar[['message']][['1']], TRUE, "s")
            }
        })

        ##################

        tkconfigure(.cdtEnv$tcl$toolbar$run, command = function(){
            on.exit({
                tkconfigure(.cdtEnv$tcl$main$win, cursor = '')
                tcl('update')
            })
            tkconfigure(.cdtEnv$tcl$main$win, cursor = 'watch')
            tcl('update')

            if(!is.null(.cdtData$GalParams)) Execute_Function()
        })

        ##################

        tkconfigure(.cdtEnv$tcl$toolbar$plotRedraw, relief = 'raised', state = "disabled",
            command = function()
        {
            on.exit({
                tkconfigure(.cdtEnv$tcl$main$win, cursor = '')
                tcl('update')
            })
            tkconfigure(.cdtEnv$tcl$main$win, cursor = 'watch')
            tcl('update')

            tabid <- as.integer(tclvalue(tkindex(.cdtEnv$tcl$main$tknotes, 'current'))) + 1
            if(length(.cdtData$OpenTab$Type)){
                if(.cdtData$OpenTab$Type[[tabid]] == "img"){

                    if(class(.cdtData$OpenTab$Data[[tabid]][[2]]) == "tkwin"){
                        W <- .cdtData$OpenTab$Data[[tabid]][[2]]
                        img <- .cdtData$OpenTab$Data[[tabid]][[2]]
                        refreshPlot(W = W, img = img,
                            hscale = as.numeric(tclvalue(tkget(.cdtEnv$tcl$toolbar$spinH))),
                            vscale = as.numeric(tclvalue(tkget(.cdtEnv$tcl$toolbar$spinV))))
                    }
                    if(class(.cdtData$OpenTab$Data[[tabid]][[2]]) == "list"){
                        W <- .cdtData$OpenTab$Data[[tabid]][[2]][[1]]
                        img <- .cdtData$OpenTab$Data[[tabid]][[2]][[2]]
                        refreshPlot(W = W, img = img,
                            hscale = as.numeric(tclvalue(tkget(.cdtEnv$tcl$toolbar$spinH))),
                            vscale = as.numeric(tclvalue(tkget(.cdtEnv$tcl$toolbar$spinV))))
                        win.child.class <- tclvalue(tkwinfo('class', tkwinfo('children', .cdtData$OpenTab$Data[[tabid]][[1]][[2]])))
                        if(win.child.class == "Frame"){
                            w <- as.double(tkwinfo("width", .cdtEnv$tcl$main$panel.right))
                            h <- as.double(tkwinfo("height", .cdtEnv$tcl$main$panel.right))
                            setScrollCanvas(W, w, h)
                        }
                    }
                    tkconfigure(.cdtEnv$tcl$toolbar$plotRedraw, image = hRedraw)
                }
            }
        })

    #################################################################################

    main.frame0 <- tkframe(.cdtEnv$tcl$main$win, bd = 2, relief = 'ridge')

    # panedwindow vertical
    main.pane0 <- ttkpanedwindow (main.frame0, orient = 'vertical')
    # panedwindow horizontal
    main.frame <- ttkpanedwindow (main.pane0, orient = 'horizontal', height = .cdtEnv$tcl$data$hpanel.left)
    # left panel
    .cdtEnv$tcl$main$panel.left <- tkframe(main.frame, relief = 'raised', bd = 2, width = .cdtEnv$tcl$data$wpanel.left)  
    # right panel
    .cdtEnv$tcl$main$panel.right <- tkframe(main.frame)

        ####################################
        ### Left panel

        frame.opfiles <- ttklabelframe(.cdtEnv$tcl$main$panel.left, text = .cdtEnv$tcl$lang$global[['label']][['1']], relief = 'groove', width = .cdtEnv$tcl$data$wpanel.left)
        .cdtEnv$tcl$main$cmd.frame <- tkframe(.cdtEnv$tcl$main$panel.left, relief = 'groove', bd = 2)

        ### List open files 
        scr.opfiles <- tkscrollbar(frame.opfiles, repeatinterval = 5,
                        command = function(...) tkyview(.cdtEnv$tcl$main$Openfiles, ...))
        .cdtEnv$tcl$main$Openfiles <- tklistbox(frame.opfiles, selectmode = "single",
                                height = 5, width = .cdtEnv$tcl$data$w.opfiles,
                                selectbackground = "yellow", selectforeground = "blue", background = "white",
                                yscrollcommand = function(...) tkset(scr.opfiles, ...))
        tkgrid(.cdtEnv$tcl$main$Openfiles, row = 0, column = 0, sticky = "nwe")
        tkgrid(scr.opfiles, row = 0, column = 1, rowspan = 4, sticky = "ns")

        ###
        tkgrid(frame.opfiles, sticky = 'nwe')
        tkgrid.columnconfigure(frame.opfiles, 0, weight = 1)

        tkgrid(.cdtEnv$tcl$main$cmd.frame, sticky = 'nwe')

        ####################################

        .cdtEnv$tcl$main$menu.opfiles <- tkmenu(.cdtEnv$tcl$main$Openfiles, tearoff = FALSE)
        tkadd(.cdtEnv$tcl$main$menu.opfiles, "command", label = .cdtEnv$tcl$lang$global[['label']][['2']], command = function(){
            id.active <- as.integer(tclvalue(tkcurselection(.cdtEnv$tcl$main$Openfiles))) + 1
            tkdelete(.cdtEnv$tcl$main$Openfiles, id.active - 1)
            .cdtData$OpenFiles$Type[id.active] <- NULL
            .cdtData$OpenFiles$Data[id.active] <- NULL
        })
        tkadd(.cdtEnv$tcl$main$menu.opfiles, "separator")
        tkadd(.cdtEnv$tcl$main$menu.opfiles, "command", label = .cdtEnv$tcl$lang$global[['label']][['3']], command = function(){
            on.exit({
                tkconfigure(.cdtEnv$tcl$main$win, cursor = '')
                tcl('update')
            })
            tkconfigure(.cdtEnv$tcl$main$win, cursor = 'watch')
            tcl('update')

            nopf <- as.integer(tclvalue(tkcurselection(.cdtEnv$tcl$main$Openfiles))) + 1
            type.file <- .cdtData$OpenFiles$Type[[nopf]]
            if(length(type.file) > 0){
                if(type.file == 'ascii'){
                    title <- .cdtData$OpenFiles$Data[[nopf]][[1]]
                    data.df <- .cdtData$OpenFiles$Data[[nopf]][[2]]
                    tab.array <- Display_data.frame_Table(data.df, title, colwidth = 5)
                    tab.array <- c(tab.array, .cdtData$OpenFiles$Data[[nopf]][[3]])

                    ntab <- update.OpenTabs('arr', tab.array)
                    tkselect(.cdtEnv$tcl$main$tknotes, ntab)
                }else{
                    Insert.Messages.Out(.cdtEnv$tcl$lang$global[['message']][['5']], format = TRUE)
                }
            }
        })

        tkbind(.cdtEnv$tcl$main$Openfiles, "<Button-3>", function(x, y){
            nopf <- as.integer(tclvalue(tkcurselection(.cdtEnv$tcl$main$Openfiles)))
            if(!is.na(nopf)) defile.menu.OpenFiles(x, y)
        })

        ####################################
        ### Right panel

        area.frame <- tkframe(.cdtEnv$tcl$main$panel.right)
        .cdtEnv$tcl$main$tknotes <- ttknotebook(area.frame)

        tkgrid(.cdtEnv$tcl$main$tknotes, row = 0, column = 0, sticky = 'nswe')
        tkgrid(area.frame, row = 0, column = 0, sticky = 'nswe')

        for(i in 0:3) tkgrid.columnconfigure(.cdtEnv$tcl$main$tknotes, i, weight = 1)
        for(i in 0:3) tkgrid.rowconfigure(.cdtEnv$tcl$main$tknotes, i, weight = 1)

        tkgrid.columnconfigure(area.frame, 0, weight = 1)
        tkgrid.rowconfigure(area.frame, 0, weight = 1)

        ####################################

        .cdtEnv$tcl$main$pressed_index <- tclVar('')
        tkbind(.cdtEnv$tcl$main$tknotes, "<ButtonPress-1>", function(x, y, W) btn_press(x, y, W))
        tkbind(.cdtEnv$tcl$main$tknotes, "<ButtonRelease-1>", function(x, y, W) btn_releases(x, y, W))

    #################################################################################

    frameAcc <- tkframe(.cdtEnv$tcl$main$tknotes)

        cdtfr.irilogo0 <- tkframe(frameAcc)
        cdtfr.cdtname <- tkframe(frameAcc)

        cdtfr.tmp2 <- tkframe(frameAcc)

        cdtfr.tmp3a <- tkframe(frameAcc)

        cdtfr.tmp4a <- tkframe(frameAcc)
        cdtfr.tmp4b <- tkframe(frameAcc)
        cdtfr.tmp4c <- tkframe(frameAcc)

        cdtfr.irilogo1 <- tkframe(frameAcc)

        #######
        cdtfont0 <- tkfont.create(family = "times", size = 48, weight = "bold")
        cdtfont1 <- tkfont.create(family = "times", size = 18, weight = "bold")
        cdtfont2 <- tkfont.create(family = "times", size = 12)

        imgAcc.cdt <- tkimage.create('photo', file = file.path(.cdtDir$Root, "images", "cdttext.gif"))
        imgAcc.iri0 <- resizeTclImage(file.path(.cdtDir$Root, "images", "iriLogo.gif"), factor = 3, zoom = FALSE)
        imgAcc.iri1 <- resizeTclImage(file.path(.cdtDir$Root, "images", "iri_logo_no_icon.gif"), factor = 4, zoom = FALSE)

        #######
        imglab.iri0 <- tklabel(cdtfr.irilogo0, image = imgAcc.iri0)
        tkgrid(imglab.iri0)

        #######
        imglab.cdt <- tklabel(cdtfr.cdtname, image = imgAcc.cdt)
        txtlab.cdt <- tklabel(cdtfr.cdtname, text = 'Climate Data Tools', font = cdtfont0, foreground = '#00227C')
        txtlab.ver <- tklabel(cdtfr.cdtname, text = paste('Version', .cdtEnv$pkg$version), font = cdtfont1)
        txtlab.auth <- tklabel(cdtfr.cdtname, text = 'Rija Faniriantsoa, Tufa Dinku', font = cdtfont2)

        tkgrid(imglab.cdt)
        tkgrid(txtlab.cdt)
        tkgrid(txtlab.ver)
        tkgrid(txtlab.auth)

        #######
        # hauteur_sep <- if(WindowsOS()) 17 else 14
        # largeur_sep <- if(WindowsOS()) 21 else 17

        hauteur_sep <- if(WindowsOS()) 8 else 14
        largeur_sep <- if(WindowsOS()) 17 else 17

        hauteur_sep <- .cdtEnv$tcl$fun$h.widgets(hauteur_sep)
        largeur_sep <- .cdtEnv$tcl$fun$w.widgets(largeur_sep)

        txtlab.tmp2 <- tklabel(cdtfr.tmp2, text = '', height = hauteur_sep)
        tkgrid(txtlab.tmp2)

        #######

        txtlab.tmp3a <- tklabel(cdtfr.tmp3a, text = '', width = largeur_sep)
        tkgrid(txtlab.tmp3a)

        #######

        txtlab.tmp4a <- tklabel(cdtfr.tmp4a, text = '', width = largeur_sep)
        txtlab.tmp4b <- tklabel(cdtfr.tmp4b, text = '', width = largeur_sep)
        txtlab.tmp4c <- tklabel(cdtfr.tmp4c, text = '', width = largeur_sep)
        tkgrid(txtlab.tmp4a)
        tkgrid(txtlab.tmp4b)
        tkgrid(txtlab.tmp4c)

        #######
        imglab.iri1 <- tklabel(cdtfr.irilogo1, image = imgAcc.iri1)
        tkgrid(imglab.iri1, row = 0, column = 0, sticky = 'se')

        #######
        tkgrid(cdtfr.irilogo0, row = 0, column = 0, sticky = 'nw', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
        tkgrid(cdtfr.cdtname, row = 0, column = 1, sticky = 'nwe', rowspan = 2, columnspan = 5, padx = 1, pady = 1, ipadx = 3, ipady = 1)

        tkgrid(cdtfr.tmp2, row = 2, column = 0, sticky = 'we', rowspan = 1, columnspan = 6, padx = 1, pady = 1, ipadx = 1, ipady = 1)

        tkgrid(cdtfr.tmp3a, row = 3, column = 0, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)

        tkgrid(cdtfr.tmp4a, row = 4, column = 0, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
        tkgrid(cdtfr.tmp4b, row = 4, column = 1, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
        tkgrid(cdtfr.tmp4c, row = 4, column = 2, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)

        tkgrid(cdtfr.irilogo1, row = 4, column = 3, sticky = 'se', rowspan = 1, columnspan = 3, padx = 1, pady = 1, ipadx = 1, ipady = 1)

        #######
        tkgrid(frameAcc, sticky = 'nsew', pady = 25, padx = 1, ipadx = 1, ipady = 1)

    #################################################################################

    ### Tabs manipulation (close)
    .Tcl(paste0("image create photo img_close -file ", '"', file.path(.cdtDir$Root, "images", "closeTabButton0.gif"), '"'))
    .Tcl(paste0("image create photo img_closeactive  -file ", '"', file.path(.cdtDir$Root, "images", "closeTabButton1.gif"), '"'))
    .Tcl(paste0("image create photo img_closepressed -file ", '"', file.path(.cdtDir$Root, "images", "closeTabButton2.gif"), '"'))

    try(.Tcl('ttk::style element create Fermer image [list img_close {active pressed !disabled} img_closepressed {active  !disabled} img_closeactive ] -border 4 -sticky e'), silent = TRUE)

    .Tcl('ttk::style layout TNotebook {TNotebook.client -sticky nswe}')

    .Tcl("ttk::style layout TNotebook.Tab {
            TNotebook.tab -sticky nswe -children {
                TNotebook.padding  -side top -sticky nswe -children {
                    TNotebook.focus -side top -sticky nswe -children {
                        TNotebook.label -side left -sticky {}
                        TNotebook.Fermer  -side right -sticky e
                    }
                }
            }
        }", sep = '\n')

    #################################################################################

    out.frame <- tkframe(main.pane0, bd = 2, relief = 'groove')

        ####################################

        .cdtEnv$tcl$main$out.xscr <- tkscrollbar(out.frame, repeatinterval = 5, orient = "horizontal",
                                        command = function(...) tkxview(.cdtEnv$tcl$main$out.text, ...))
        .cdtEnv$tcl$main$out.yscr <- tkscrollbar(out.frame, repeatinterval = 5,
                                        command = function(...) tkyview(.cdtEnv$tcl$main$out.text, ...))
        .cdtEnv$tcl$main$out.text <- tktext(out.frame, bg = "white",
                                        # selectbackground = 'blue',
                                        #font = tkfont.create(family = "courier", size = 11),
                                        xscrollcommand = function(...) tkset(.cdtEnv$tcl$main$out.xscr, ...),
                                        yscrollcommand = function(...) tkset(.cdtEnv$tcl$main$out.yscr, ...),
                                        wrap = "none", height = txtHeight)

        tkgrid(.cdtEnv$tcl$main$out.text, .cdtEnv$tcl$main$out.yscr)
        tkgrid(.cdtEnv$tcl$main$out.xscr)
        tkgrid.configure(.cdtEnv$tcl$main$out.yscr, sticky = "ns")
        tkgrid.configure(.cdtEnv$tcl$main$out.xscr, sticky = "ew")
        tkgrid.configure(.cdtEnv$tcl$main$out.text, sticky = 'nswe')
        tkgrid.columnconfigure(.cdtEnv$tcl$main$out.text, 0, weight = 1)

        tktag.configure(.cdtEnv$tcl$main$out.text, "sel", foreground = "black", background = "yellow")

        menuCopyPaste(.cdtEnv$tcl$main$out.text, scopy = 'normal', scut = 'normal', spaste = 'disabled')


    #################################################################################

    frstatusbar <- tkframe(.cdtEnv$tcl$main$win)

        ####################################

        ## Status bar width (from left)
        wbstatus1 <- .cdtEnv$tcl$fun$w.widgets(58)
        wbstatus2a <- .cdtEnv$tcl$fun$w.widgets(11)
        wbstatus2b <- .cdtEnv$tcl$fun$w.widgets(11)
        wbstatus3 <- .cdtEnv$tcl$fun$w.widgets(13)
        wbpbar <- .cdtEnv$tcl$fun$w.scale(11)

        ######

        .cdtEnv$tcl$status$help <- tclVar()
        .cdtEnv$tcl$status$xcrd <- tclVar()
        .cdtEnv$tcl$status$ycrd <- tclVar()
        .cdtEnv$tcl$status$zval <- tclVar()

        .cdtEnv$tcl$status$pbLab <- tclVar()
        .cdtEnv$tcl$status$pbBar <- tclVar(0)

        ######

        status.frame <- tkframe(frstatusbar)

        ######
        bstatus1 <- tkframe(status.frame, relief = 'sunken', bd = 2)

        lhelp <- tklabel(bstatus1, textvariable = .cdtEnv$tcl$status$help, anchor = 'w', width = wbstatus1)

        tkgrid(lhelp, row = 0, column = 0, sticky = "we")

        ######
        bstatus2 <- tkframe(status.frame, relief = 'sunken', bd = 2)

        ######
        bstatus2a <- tkframe(bstatus2)
        lxcoords <- tklabel(bstatus2a, textvariable = .cdtEnv$tcl$status$xcrd, width = wbstatus2a)
        lxcoords0 <- tklabel(bstatus2a, text = 'X:',justify = 'left', anchor = 'w')

        tkgrid(lxcoords0, row = 0, column = 0, sticky = "we")
        tkgrid(lxcoords, row = 0, column = 1, sticky = "we")

        ######
        xy.separator <- ttkseparator(bstatus2, orient = 'vertical')

        ######
        bstatus2b <- tkframe(bstatus2)
        lycoords <- tklabel(bstatus2b, textvariable = .cdtEnv$tcl$status$ycrd, width = wbstatus2b)
        lycoords0 <- tklabel(bstatus2b, text = 'Y:',justify = 'left', anchor = 'w')

        tkgrid(lycoords0, row = 0, column = 0, sticky = "we")
        tkgrid(lycoords, row = 0, column = 1, sticky = "we")

        tkgrid(bstatus2a, row = 0, column = 0, sticky = "we")
        tkgrid(xy.separator, row = 0, column = 1, sticky = 'ns', padx = 2)
        tkgrid(bstatus2b, row = 0, column = 2, sticky = "we")

        tkgrid.columnconfigure(bstatus2a, 1, weight = 1)
        tkgrid.columnconfigure(bstatus2b, 1, weight = 1)

        ######
        bstatus3 <- tkframe(status.frame, relief = 'sunken', bd = 2)

        lzcoords <- tklabel(bstatus3, textvariable = .cdtEnv$tcl$status$zval, width = wbstatus3)
        lzcoords0 <- tklabel(bstatus3, text = 'Z:',justify = 'left', anchor = 'w')

        tkgrid(lzcoords0, row = 0, column = 0, sticky = "we")
        tkgrid(lzcoords, row = 0, column = 1, sticky = "we")

        ######
        bstatus4 <- tkframe(status.frame)

        tb.pb.lab <- tklabel(bstatus4, width = 5, text = tclvalue(.cdtEnv$tcl$status$pbLab), textvariable = .cdtEnv$tcl$status$pbLab)
        tb.pb.bar <- ttkprogressbar(bstatus4, length = wbpbar, variable = .cdtEnv$tcl$status$pbBar)

        tkgrid(tb.pb.lab, tb.pb.bar)

        ####

        tkgrid(bstatus1, row = 0, column = 0, padx = 2, pady = 0, sticky = "we")
        tkgrid(bstatus2, row = 0, column = 1, padx = 1, pady = 0, sticky = "we")
        tkgrid(bstatus3, row = 0, column = 2, padx = 1, pady = 0, sticky = "we")
        tkgrid(bstatus4, row = 0, column = 3, sticky = "e")
        tkgrid.columnconfigure(bstatus1, 0, weight = 1)
        tkgrid.columnconfigure(bstatus2, 0, weight = 1)
        tkgrid.columnconfigure(bstatus2, 2, weight = 1)
        tkgrid.columnconfigure(bstatus3, 1, weight = 1)
        tkgrid.columnconfigure(bstatus4, 1, weight = 1)

        ####

        tkgrid(status.frame, row = 0, column = 0, sticky = "ew")
        tkgrid.columnconfigure(status.frame, 0, weight = 2)
        tkgrid.columnconfigure(status.frame, 1, weight = 2)
        tkgrid.columnconfigure(status.frame, 2, weight = 1)
        tkgrid.columnconfigure(status.frame, 3, weight = 1)

    #################################################################################
    ## Manage grid

    tkadd(main.frame, .cdtEnv$tcl$main$panel.left)
    tkadd(main.frame, .cdtEnv$tcl$main$panel.right)

    #left panel
    tkgrid.columnconfigure(.cdtEnv$tcl$main$panel.left, 0, weight = 1)
    tkgrid.rowconfigure(.cdtEnv$tcl$main$panel.left, 1, weight = 1)

    #right panel
    tkgrid.columnconfigure(.cdtEnv$tcl$main$panel.right, 0, weight = 1)
    tkgrid.rowconfigure(.cdtEnv$tcl$main$panel.right, 0, weight = 1)

    ###
    tkadd(main.pane0, main.frame)
    tkadd(main.pane0, out.frame)

    tkgrid.columnconfigure(main.frame, 0, weight = 1)
    tkgrid.columnconfigure(main.frame, 1, weight = 1)
    tkgrid.rowconfigure(main.frame, 0, weight = 1)
    tkgrid.columnconfigure(out.frame, 0, weight = 1)
    tkgrid.rowconfigure(out.frame, 0, weight = 1)

    ## panned frame
    tkgrid(main.pane0, row = 0, column = 0, rowspan = 1, columnspan = 2, sticky = "snew", padx = 1) 
    tkgrid.rowconfigure(main.pane0, 0, weight = 1)
    tkgrid.rowconfigure(main.pane0, 1, weight = 1)
    tkgrid.columnconfigure(main.pane0, 0, weight = 1)

    ####################################

    ## toolbars
    tkgrid(tools.frame, row = 0, column = 0, rowspan = 1, columnspan = 2, sticky = "new", padx = 5)

    ## main frame
    tkgrid(main.frame0, row = 1, column = 0, rowspan = 1, columnspan = 2, sticky = "snew", padx = 5) 
    tkgrid.rowconfigure(main.frame0, 0, weight = 1)
    tkgrid.columnconfigure(main.frame0, 0, weight = 1)

    ## statusbar
    tkgrid(frstatusbar, row = 2, column = 0, rowspan = 1, columnspan = 1, sticky = "snew")
    tkgrid.columnconfigure(frstatusbar, 0, weight = 1)

    ## grip
    grip.right <- ttksizegrip(.cdtEnv$tcl$main$win)
    tkgrid(grip.right, row = 2, column = 1, sticky = "se")

    #################################################################################
    ## Manage geometry
    tkwm.withdraw(.cdtEnv$tcl$main$win)
    tcl('update')
    tkwm.geometry(.cdtEnv$tcl$main$win, paste0(.cdtEnv$tcl$data$width.scr, 'x', .cdtEnv$tcl$data$height.scr, '+', 0, '+', 0))
    tkwm.transient(.cdtEnv$tcl$main$win)
    tkwm.title(.cdtEnv$tcl$main$win, paste0("Climate Data Tools, v", .cdtEnv$pkg$version))
    tkwm.deiconify(.cdtEnv$tcl$main$win)

    if(WindowsOS()){
        tcl('wm', 'attributes', .cdtEnv$tcl$main$win, fullscreen = FALSE)
        tcl('wm', 'state', .cdtEnv$tcl$main$win, 'zoomed')
    }else{
        tcl('wm', 'attributes', .cdtEnv$tcl$main$win, fullscreen = FALSE, zoomed = TRUE)
    }

    ## Close CDT
    tcl("wm", "protocol", .cdtEnv$tcl$main$win, "WM_DELETE_WINDOW", function(){
        on.exit({
            rm(list = ls(envir = .cdtData), envir = .cdtData)
            .cdtEnv$tcl <- NULL
            options(warn = 0)
        })
        refreshCDT()
        tkdestroy(.cdtEnv$tcl$main$win)
    })

    #################################################################################

    .cdtEnv$tcl$zoom$img$plus <- resizeTclImage(file.path(.cdtDir$Root, "images", 'ZoomIn128.gif'), factor = 4, zoom = FALSE)
    .cdtEnv$tcl$zoom$img$moins <- resizeTclImage(file.path(.cdtDir$Root, "images", 'ZoomOut128.gif'), factor = 4, zoom = FALSE)
    .cdtEnv$tcl$zoom$img$rect <- resizeTclImage(file.path(.cdtDir$Root, "images", 'ZoomRect128.gif'), factor = 4, zoom = FALSE)
    .cdtEnv$tcl$zoom$img$centre <- tkimage.create('photo', file = file.path(.cdtDir$Root, "images", 'imgCentre24.gif'))
    .cdtEnv$tcl$zoom$img$redraw <- resizeTclImage(file.path(.cdtDir$Root, "images", 'redraw128.gif'), factor = 4, zoom = FALSE)
    .cdtEnv$tcl$zoom$img$pan <- tkimage.create('photo', file = file.path(.cdtDir$Root, "images", 'PanImage32.gif'))
    .cdtEnv$tcl$zoom$img$reset <- resizeTclImage(file.path(.cdtDir$Root, "images", 'reset128.gif'), factor = 4, zoom = FALSE)

    #################################################################################

    invisible()
}
