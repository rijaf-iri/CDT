
cdtMainContextMenu <- function(){
    xml.menu <- file.path(.cdtDir$dirLocal, "languages", "cdt_menu_bar.xml")
    lang.menu <- cdtLanguageParse.menu(xml.menu, .cdtData$Config$lang.iso)

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

            ## refresh to avoid the errors
            ## Error in try(<environment>()) : attempt to apply non-function
            ## Error in (function () : 'rho' must be an environment not language: detected in C-level eval
            ## Error in try(NULL()) : attempt to apply non-function
            refreshCDT(staterun = "disabled")

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
            refreshCDT(staterun = "disabled")

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
            refreshCDT(staterun = "disabled")

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
                Insert.Messages.Out(.cdtEnv$tcl$lang$global[['message']][['2']], TRUE, 'e')
                Insert.Messages.Out(gsub('[\r\n]', '', tab2sav[1]), TRUE, 'e')
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
            refreshCDT(staterun = "disabled")
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

            ## stop openFiles listener
            tcl("after", "cancel", .cdtEnv$tcl$task_openFiles$id)

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
                initialize.parameters('combineCDT.stn', 'daily')
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

            ########
            tkadd(menu.dataDown, "separator")

            ##########
            tkadd(menu.dataDown, "command", label = lang.menu[["data.preparation"]][["6-5"]],
                  command = function()
            {
                refreshCDT(staterun = "normal")
                initialize.parameters('exGRIB.JRA.NRT')
                extractGRIB_JRA_NRT()
            })

            ########
            tkadd(menu.dataDown, "separator")

            ##########
            tkadd(menu.dataDown, "command", label = lang.menu[["data.preparation"]][["6-6"]],
                  command = function()
            {
                refreshCDT(staterun = "disabled")
                initialize.parameters('downElv.Reanal')
                download_Elevation_Reanalysis()
            })

            ########
            tkadd(menu.dataDown, "separator")

            ##########
            tkadd(menu.dataDown, "command", label = lang.menu[["data.preparation"]][["6-7"]],
                  command = function()
            {
                refreshCDT(staterun = "disabled")
                initialize.parameters('build.wgrib')
                build_wgrib_info()
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
            tkadd(menu.AggrData, "command", label = lang.menu[["data.preparation"]][["7-4"]],
                  command = function()
            {
                refreshCDT(staterun = "normal")
                initialize.parameters('aggregate.minmax', 'daily')
                AggregateTS_minmax_GetInfo()
            })

            ###########
            tkadd(menu.AggrData, "separator")

            ##########
            tkadd(menu.AggrData, "command", label = lang.menu[["data.preparation"]][["7-3"]],
                  command = function()
            {
                refreshCDT(staterun = "normal")
                initialize.parameters('aggregate.rf', 'daily')
                AggregateMWin_GetInfo()
            })

        ##########
        tkadd(menu.dataprep, "separator")

        ##########
        tkadd(menu.dataprep, "command", label = lang.menu[["data.preparation"]][["7-2"]],
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
        tkadd(menu.dataprep, "command", label = lang.menu[["data.preparation"]][["13"]],
              command = function()
        {
            refreshCDT(staterun = "normal")
            initialize.parameters('data.Operation')
            dataOperation_GetInfo()
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
        tkadd(menu.dataprep, "command", label = lang.menu[["data.preparation"]][["12"]],
              command = function()
        {
            refreshCDT(staterun = "normal")
            initialize.parameters('combine.NetCDF')
            combine.netcdf_getParams()
        })

        ##########
        tkadd(menu.dataprep, "separator")

        ##########
        tkadd(menu.dataprep, "command", label = lang.menu[["data.preparation"]][["11"]],
              command = function()
        {
            refreshCDT(staterun = "normal")
            initialize.parameters('blank.NetCDF')
            blankNcdf_GetInfo()
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
        menu.mrg.var <- tkmenu(top.menu, tearoff = FALSE)
        tkadd(menu.mrg, "cascade", label = lang.menu[["merging.data"]][["2"]], menu = menu.mrg.var)

            ###########
            menu.mrg.rain <- tkmenu(menu.mrg, tearoff = FALSE)
            tkadd(menu.mrg.var, "cascade", label = lang.menu[["merging.data"]][["2-1"]], menu = menu.mrg.rain)

                ##########
                tkadd(menu.mrg.rain, "command", label = lang.menu[["merging.data"]][["3-2"]], background = 'lightblue',
                      command = function()
                {
                    refreshCDT(staterun = "normal")
                    initialize.parameters('coefbias.rain', 'dekadal')
                    biasCoeffGetInfoClimData()
                })

                ########
                tkadd(menu.mrg.rain, "separator")

                ##########
                tkadd(menu.mrg.rain, "command", label = lang.menu[["merging.data"]][["3-3"]],
                      command = function()
                {
                    refreshCDT(staterun = "normal")
                    initialize.parameters('rmbias.rain', 'dekadal')
                    removeBiasGetInfoClimData()
                })

                 ########
                tkadd(menu.mrg.rain, "separator")

                ##########
                tkadd(menu.mrg.rain, "command", label = lang.menu[["merging.data"]][["3-4"]],
                      command = function()
                {
                    refreshCDT(staterun = "normal")
                    initialize.parameters('merge.rain', 'dekadal')
                    mergeGetInfoClimData()
                })

            ##########
            tkadd(menu.mrg.var, "separator")

            ###########
            menu.mrg.temp <- tkmenu(menu.mrg, tearoff = FALSE)
            tkadd(menu.mrg.var, "cascade", label = lang.menu[["merging.data"]][["2-2"]], menu = menu.mrg.temp)

                ##########
                tkadd(menu.mrg.temp, "command", label = lang.menu[["merging.data"]][["2-2-1"]], background = 'lightblue',
                      command = function()
                {
                    refreshCDT(staterun = "normal")
                    initialize.parameters('coefdown.temp', 'dekadal')
                    Temp_coefDownGetInfo()
                })

                ########
                tkadd(menu.mrg.temp, "separator")

                ##########
                tkadd(menu.mrg.temp, "command", label = lang.menu[["merging.data"]][["3-1"]],
                      command = function()
                {
                    refreshCDT(staterun = "normal")
                    initialize.parameters('down.temp', 'dekadal')
                    Temp_reanalDownGetInfo()
                })

                 ########
                tkadd(menu.mrg.temp, "separator")

                ##########
                tkadd(menu.mrg.temp, "command", label = lang.menu[["merging.data"]][["3-2"]], background = 'lightblue',
                      command = function()
                {
                    refreshCDT(staterun = "normal")
                    initialize.parameters('coefbias.temp', 'dekadal')
                    biasCoeffGetInfoClimData()
                })

                ########
                tkadd(menu.mrg.temp, "separator")

                ##########
                tkadd(menu.mrg.temp, "command", label = lang.menu[["merging.data"]][["3-3"]],
                      command = function()
                {
                    refreshCDT(staterun = "normal")
                    initialize.parameters('rmbias.temp', 'dekadal')
                    removeBiasGetInfoClimData()
                })

                 ########
                tkadd(menu.mrg.temp, "separator")

                ##########
                tkadd(menu.mrg.temp, "command", label = lang.menu[["merging.data"]][["3-4"]],
                      command = function()
                {
                    refreshCDT(staterun = "normal")
                    initialize.parameters('merge.temp', 'dekadal')
                    mergeGetInfoClimData()
                })

            ##########
            tkadd(menu.mrg.var, "separator")

            ###########
            menu.mrg.rh <- tkmenu(menu.mrg, tearoff = FALSE)
            tkadd(menu.mrg.var, "cascade", label = lang.menu[["merging.data"]][["2-3"]], menu = menu.mrg.rh)

                ##########
                tkadd(menu.mrg.rh, "command", label = lang.menu[["merging.data"]][["3-1"]],
                      command = function()
                {
                    refreshCDT(staterun = "normal")
                    # initialize.parameters('down.rh', 'dekadal')
                    # down()
                })

                ########
                tkadd(menu.mrg.rh, "separator")

                ##########
                tkadd(menu.mrg.rh, "command", label = lang.menu[["merging.data"]][["3-2"]], background = 'lightblue',
                      command = function()
                {
                    refreshCDT(staterun = "normal")
                    initialize.parameters('coefbias.rh', 'daily')
                    biasCoeffGetInfoClimData()
                })

                ########
                tkadd(menu.mrg.rh, "separator")

                ##########
                tkadd(menu.mrg.rh, "command", label = lang.menu[["merging.data"]][["3-3"]],
                      command = function()
                {
                    refreshCDT(staterun = "normal")
                    initialize.parameters('rmbias.rh', 'daily')
                    removeBiasGetInfoClimData()
                })

                 ########
                tkadd(menu.mrg.rh, "separator")

                ##########
                tkadd(menu.mrg.rh, "command", label = lang.menu[["merging.data"]][["3-4"]],
                      command = function()
                {
                        refreshCDT(staterun = "normal")
                        initialize.parameters('merge.rh', 'daily')
                        mergeGetInfoClimData()
                })

            ##########
            tkadd(menu.mrg.var, "separator")

            ###########
            menu.mrg.pres <- tkmenu(menu.mrg, tearoff = FALSE)
            tkadd(menu.mrg.var, "cascade", label = lang.menu[["merging.data"]][["2-4"]], menu = menu.mrg.pres)

                ##########
                tkadd(menu.mrg.pres, "command", label = lang.menu[["merging.data"]][["3-1"]],
                      command = function()
                {
                    refreshCDT(staterun = "normal")
                    # initialize.parameters('down.pres', 'daily')
                    # down()
                })

                ########
                tkadd(menu.mrg.pres, "separator")

                ##########
                tkadd(menu.mrg.pres, "command", label = lang.menu[["merging.data"]][["3-2"]], background = 'lightblue',
                      command = function()
                {
                    refreshCDT(staterun = "normal")
                    initialize.parameters('coefbias.pres', 'daily')
                    biasCoeffGetInfoClimData()
                })

                ########
                tkadd(menu.mrg.pres, "separator")

                ##########
                tkadd(menu.mrg.pres, "command", label = lang.menu[["merging.data"]][["3-3"]],
                      command = function()
                {
                        refreshCDT(staterun = "normal")
                        initialize.parameters('rmbias.pres', 'daily')
                        removeBiasGetInfoClimData()
                })

                 ########
                tkadd(menu.mrg.pres, "separator")

                ##########
                tkadd(menu.mrg.pres, "command", label = lang.menu[["merging.data"]][["3-4"]],
                      command = function()
                {
                        refreshCDT(staterun = "normal")
                        initialize.parameters('merge.pres', 'daily')
                        mergeGetInfoClimData()
                })

            ##########
            tkadd(menu.mrg.var, "separator")

            ###########
            menu.mrg.rad <- tkmenu(menu.mrg, tearoff = FALSE)
            tkadd(menu.mrg.var, "cascade", label = lang.menu[["merging.data"]][["2-6"]], menu = menu.mrg.rad)

                ##########
                tkadd(menu.mrg.rad, "command", label = lang.menu[["merging.data"]][["3-1"]],
                      command = function()
                {
                    refreshCDT(staterun = "normal")
                    # initialize.parameters('down.rad', 'dekadal')
                    # down()
                })

                ########
                tkadd(menu.mrg.rad, "separator")

                ##########
                tkadd(menu.mrg.rad, "command", label = lang.menu[["merging.data"]][["3-2"]], background = 'lightblue',
                      command = function()
                {
                    refreshCDT(staterun = "normal")
                    initialize.parameters('coefbias.rad', 'daily')
                    biasCoeffGetInfoClimData()
                })

                ########
                tkadd(menu.mrg.rad, "separator")

                ##########
                tkadd(menu.mrg.rad, "command", label = lang.menu[["merging.data"]][["3-3"]],
                      command = function()
                {
                        refreshCDT(staterun = "normal")
                        initialize.parameters('rmbias.rad', 'daily')
                        removeBiasGetInfoClimData()
                })

                 ########
                tkadd(menu.mrg.rad, "separator")

                ##########
                tkadd(menu.mrg.rad, "command", label = lang.menu[["merging.data"]][["3-4"]],
                      command = function()
                {
                        refreshCDT(staterun = "normal")
                        initialize.parameters('merge.rad', 'daily')
                        mergeGetInfoClimData()
                })

            ##########
            tkadd(menu.mrg.var, "separator")

            ###########
            menu.mrg.wind <- tkmenu(menu.mrg, tearoff = FALSE)
            tkadd(menu.mrg.var, "cascade", label = lang.menu[["merging.data"]][["2-5"]], menu = menu.mrg.wind)

                ##########
                tkadd(menu.mrg.wind, "command", label = lang.menu[["merging.data"]][["3-1"]],
                      command = function()
                {
                    refreshCDT(staterun = "normal")
                    # initialize.parameters('down.wind', 'dekadal')
                    # down()
                })

                ########
                tkadd(menu.mrg.wind, "separator")

                ##########
                tkadd(menu.mrg.wind, "command", label = lang.menu[["merging.data"]][["3-2"]], background = 'lightblue',
                      command = function()
                {
                    refreshCDT(staterun = "normal")
                    initialize.parameters('coefbias.wind', 'daily')
                    biasCoeffGetInfoWind()
                })

                ########
                tkadd(menu.mrg.wind, "separator")

                ##########
                tkadd(menu.mrg.wind, "command", label = lang.menu[["merging.data"]][["3-3"]],
                      command = function()
                {
                    refreshCDT(staterun = "normal")
                    initialize.parameters('rmbias.wind', 'daily')
                    removeBiasGetInfoWind()
                })

                 ########
                tkadd(menu.mrg.wind, "separator")

                ##########
                tkadd(menu.mrg.wind, "command", label = lang.menu[["merging.data"]][["3-4"]],
                      command = function()
                {
                    refreshCDT(staterun = "normal")
                    initialize.parameters('merge.wind', 'daily')
                    mergeGetInfoWind()
                })

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
            tkadd(menu.mrg.loocv, "command", label = lang.menu[["merging.data"]][["2-1"]],
                  command = function()
            {
                refreshCDT(staterun = "normal")
                initialize.parameters('crossv.rain', 'dekadal')
                crossValidationInfoClimData()
            })

            ########
            tkadd( menu.mrg.loocv, "separator")

            ########
            tkadd(menu.mrg.loocv, "command", label = lang.menu[["merging.data"]][["2-2"]],
                command = function()
            {
                refreshCDT(staterun = "normal")
                initialize.parameters('crossv.temp', 'dekadal')
                crossValidationInfoClimData()
            })

            ########
            tkadd( menu.mrg.loocv, "separator")

            ########
            tkadd(menu.mrg.loocv, "command", label = lang.menu[["merging.data"]][["2-3"]],
                command = function()
            {
                refreshCDT(staterun = "normal")
                initialize.parameters('crossv.rh', 'dekadal')
                crossValidationInfoClimData()
            })

            ########
            tkadd( menu.mrg.loocv, "separator")

            ########
            tkadd(menu.mrg.loocv, "command", label = lang.menu[["merging.data"]][["2-4"]],
                command = function()
            {
                refreshCDT(staterun = "normal")
                initialize.parameters('crossv.pres', 'dekadal')
                crossValidationInfoClimData()
            })

            ########
            tkadd( menu.mrg.loocv, "separator")

            ########
            tkadd(menu.mrg.loocv, "command", label = lang.menu[["merging.data"]][["2-6"]],
                command = function()
            {
                refreshCDT(staterun = "normal")
                initialize.parameters('crossv.rad', 'dekadal')
                crossValidationInfoClimData()
            })

            ########
            tkadd( menu.mrg.loocv, "separator")

            ########
            tkadd(menu.mrg.loocv, "command", label = lang.menu[["merging.data"]][["2-5"]],
                command = function()
            {
                refreshCDT(staterun = "normal")
                initialize.parameters('crossv.wind', 'dekadal')
                crossValidationInfoWind()
            })

        # ##########
        # tkadd(menu.mrg, "separator")

        # ##########
        # tkadd(menu.mrg, "command", label = lang.menu[["merging.data"]][["7"]],
        #       command = function()
        # {
        #     refreshCDT()
        #     spinbox.state(state = 'normal')
        #     if(is.null(.cdtEnv$tcl$data$lcmd.frame)){
        #         # PlotSeqNetCDFFilesCmd()
        #         .cdtEnv$tcl$data$lcmd.frame <- 1
        #     }
        # })

        # ##########
        # tkadd(menu.mrg, "separator")

        # tkadd(menu.mrg, "command", label = lang.menu[["merging.data"]][["8"]],
        #       command = function()
        # {
        #     refreshCDT()
        #     spinbox.state(state = 'normal')
        #     if(is.null(.cdtEnv$tcl$data$lcmd.frame)){
        #         # PlotSeqNetCDFFilesCmd()
        #         .cdtEnv$tcl$data$lcmd.frame <- 1
        #     }
        # })

        # ##########
        # tkadd(menu.mrg, "separator")

        # tkadd(menu.mrg, "command", label = lang.menu[["merging.data"]][["9"]],
        #       command = function()
        # {
        #     refreshCDT()
        #     spinbox.state(state = 'normal')
        #     if(is.null(.cdtEnv$tcl$data$lcmd.frame)){
        #         # PlotSeqNetCDFFilesCmd()
        #         .cdtEnv$tcl$data$lcmd.frame <- 1
        #     }
        # })

    ####################################

    menu.valid <- tkmenu(top.menu, tearoff = FALSE, relief = "flat")
    tkadd(top.menu, "cascade", label = lang.menu[["validation"]][["0"]], menu = menu.valid, activebackground = 'lightblue')

        ##########
        menu.valid.spts <- tkmenu(top.menu, tearoff = FALSE)
        tkadd(menu.valid, "cascade", label = lang.menu[["validation"]][["1"]], menu = menu.valid.spts)

            ########
            tkadd(menu.valid.spts, "command", label = lang.menu[["validation"]][["4-1"]],
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
            tkadd(menu.valid.spts, "command", label = lang.menu[["validation"]][["4-2"]],
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
            tkadd(menu.valid.grd, "command", label = lang.menu[["validation"]][["4-1"]],
                  command = function()
            {
                refreshCDT(staterun = "disabled")
                spinbox.state(state = 'normal')
                if(is.null(.cdtEnv$tcl$data$lcmd.frame)){
                    Validation.GriddedData.PanelCmd('RR')
                    .cdtEnv$tcl$data$lcmd.frame <- 1
                }
            })

            ########
            tkadd(menu.valid.grd, "separator")

            ########
            tkadd(menu.valid.grd, "command", label = lang.menu[["validation"]][["4-2"]],
                  command = function()
            {
                refreshCDT(staterun = "disabled")
                spinbox.state(state = 'normal')
                if(is.null(.cdtEnv$tcl$data$lcmd.frame)){
                    Validation.GriddedData.PanelCmd('TT')
                    .cdtEnv$tcl$data$lcmd.frame <- 1
                }
            })

        # ##########
        # tkadd(menu.valid, "separator")

        # ##########
        # menu.valid.grd <- tkmenu(top.menu, tearoff = FALSE)
        # tkadd(menu.valid, "cascade", label = lang.menu[["validation"]][["2-old"]], menu = menu.valid.grd)

        #     ########
        #     tkadd(menu.valid.grd, "command", label = lang.menu[["validation"]][["4-1"]],
        #           command = function()
        #     {
        #         refreshCDT(staterun = "disabled")
        #         spinbox.state(state = 'normal')
        #         if(is.null(.cdtEnv$tcl$data$lcmd.frame)){
        #             Validation.HOV.PanelCmd('RR')
        #             .cdtEnv$tcl$data$lcmd.frame <- 1
        #         }
        #     })

        #     ########
        #     tkadd(menu.valid.grd, "separator")

        #     ########
        #     tkadd(menu.valid.grd, "command", label = lang.menu[["validation"]][["4-2"]],
        #           command = function()
        #     {
        #         refreshCDT(staterun = "disabled")
        #         spinbox.state(state = 'normal')
        #         if(is.null(.cdtEnv$tcl$data$lcmd.frame)){
        #             Validation.HOV.PanelCmd('TT')
        #             .cdtEnv$tcl$data$lcmd.frame <- 1
        #         }
        #     })

        ##########
        tkadd(menu.valid, "separator")

        ##########
        menu.valid.stats <- tkmenu(top.menu, tearoff = FALSE)
        tkadd(menu.valid, "cascade", label = lang.menu[["validation"]][["3"]], menu = menu.valid.stats)

            ########
            tkadd(menu.valid.stats, "command", label = lang.menu[["validation"]][["4-1"]],
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
            tkadd(menu.valid.stats, "command", label = lang.menu[["validation"]][["4-2"]],
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
                # initialize.parameters('compute.Wind', 'daily')
                # computeWind_getParams()
            })

            ########
            tkadd(menu.ClimVars, "separator")

            ########
            tkadd(menu.ClimVars, "command", label = lang.menu[["data.analysis"]][["2-3"]],
                  command = function()
            {
                refreshCDT(staterun = "normal")
                initialize.parameters('compute.PET', 'daily')
                computePET_getParams()
            })

            ########
            tkadd(menu.ClimVars, "separator")

            ########
            tkadd(menu.ClimVars, "command", label = lang.menu[["data.analysis"]][["2-4"]],
                  command = function()
            {
                refreshCDT(staterun = "normal")
                initialize.parameters('compute.WB', 'daily')
                computeWB_getParams()
            })

            ########
            tkadd(menu.ClimVars, "separator")

            ########
            tkadd(menu.ClimVars, "command", label = lang.menu[["data.analysis"]][["2-5"]],
                  command = function()
            {
                refreshCDT(staterun = "normal")
                # initialize.parameters('compute.WRSI', 'daily')
                # computeWRSI_getParams()
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

        tkadd(menu.dataproc, "command", label = lang.menu[["data.analysis"]][["5-1"]],
              command = function()
        {
            refreshCDT(staterun = "disabled")
            spinbox.state(state = 'normal')
            # if(is.null(.cdtEnv$tcl$data$lcmd.frame)){
            #     seasonalRainAnalysisPanelCmd()
            #     .cdtEnv$tcl$data$lcmd.frame <- 1
            # }
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

        # ##########
        # tkadd(menu.dataproc, "separator")

        # ##########
        # tkadd(menu.dataproc, "command", label = "Crop Probabilities Tables",
        #       command = function()
        # {
        #     refreshCDT(staterun = "disabled")
        #     spinbox.state(state = 'normal')
        #     # if(is.null(.cdtEnv$tcl$data$lcmd.frame)){
        #     #     CropProbaPanelCmd()
        #     #     .cdtEnv$tcl$data$lcmd.frame <- 1
        #     # }
        # })

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
                if(utils::packageVersion("devtools") >= "2.0.0"){
                    devtools::install_github("rijaf-iri/CDT", dependencies = FALSE,
                                             upgrade = FALSE, force = TRUE)
                }else{
                    devtools::install_github("rijaf-iri/CDT", dependencies = FALSE, 
                                              upgrade_dependencies = FALSE, force = TRUE)
                }
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
        tclvalue(.cdtEnv$tcl$status$pbnmax) <- 1
    })

    invisible()
}
