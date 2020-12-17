
## Parameters Initialization 

initialize.parameters <- function(action, tstep = 'dekadal'){
    initpars <- cdt.init.params(action, tstep)
    if(!is.null(.cdtData$GalParams))
        if(!is.null(.cdtData$GalParams$action))
            if(.cdtData$GalParams$action == action)
                initpars <- .cdtData$GalParams
    .cdtData$GalParams <- initpars
}

##############################################

cdt.init.params <- function(action, tstep){
    ## Disabled toolbars spinbox
    spinbox.state()

    date.range <- list(start.year = 2018, start.mon = 1, start.dek = 1,
                       start.pen = 1, start.day = 1,
                       start.hour = 0, start.min = 0,
                       end.year = 2019, end.mon = 12, end.dek = 3,
                       end.pen = 6, end.day = 31,
                       end.hour = 23, end.min = 55)

    ## Format CDTs Input station Data
    if(action == 'cdtInput.stn'){
    IO.files <- list(STN.sample.file = "", STN.dir = "",
                     STN.single.file = "", STN.coords.file = "",
                     File2Save = "")
    Multiple.File <- list(file.format = 1, date.format = 1, include.elev = FALSE)
    Single.File <- list(coords.included = TRUE, nb.column = 31, col.stn.id = 1,
                        col.stn.lon = 2, col.stn.lat = 3, col.stn.elv = 4,
                        col.year = 5, col.month = 6, col.day.dek = 7,
                        col.start.data = 8, include.elev = FALSE)
    ret.params <- list(action = action, tstep = tstep,
                       IO.files = IO.files, Date.Range = date.range,
                       min.perc = 1, data.type = "Multiple",
                       Multiple.File = Multiple.File,
                       Single.File = Single.File)
    }

    ## Merge 2 CDTs station Data
    if(action == 'merge2CDT.stn'){
        ret.params <- list(action = action, file1 = "", file2 = "", file2save = "")
    }

    ## Filter CDT station Data
    if(action == 'filter.data'){
        ret.params <- list(action = action, filein = "",
                           all.period = TRUE,
                           tstep = 'daily', minhour = 0,
                           date.range = date.range,
                           opfilter = ">=", valfilter = 20,
                           file2save = "")
    }

    ## Select CDT station Data
    if(action == 'selectCDT.data'){
        ret.params <- list(action = action, filein = "", filein1 = "",
                           opfilter = ">=", valfilter = 20, file2save = "")
    }

    ## Create cdt dataset from ncdf files
    if(action == 'create.CdtDataset'){
        NCDF <- list(dir = "", format = "rr_mrg_%s%s%s_ALL.nc", sample = "")
        output <- list(dir = getwd(), data.name = "PRECIP")
        ret.params <- list(action = action, tstep = tstep, date.range = date.range,
                           NCDF = NCDF, output = output, Update = FALSE, cdtDataSet = "",
                           chunk = .cdtData$Config$cdtDataset.chunk
                          )
    }

    ## split NetCDF multiple dim files
    if(action == 'split.NetCDF'){
        ret.params <- list(action = action,
                           nbfile = "one",
                           ncdf = list(file = "", format = "chirp.%S.days_p05.nc"),
                           output = "")
    }

    ## Blank NetCDF files
    if(action == 'blank.NetCDF'){
        ret.params <- list(action = action,
                           nbnc = "one", dirnc = "",
                           sample = "", shpf = "",
                           output = "")
    }

    #################################################################

    ## Download DEM
    if(action == 'down.DEM'){
        ret.params <- list(action = action,
                           bbox = .cdtData$Config$region,
                           dir2save = getwd())
    }

    ## Download SHP
    if(action == 'down.SHP'){
        ret.params <- list(action = action,
                           region = "Africa",
                           country = "Madagascar",
                           level = 0,
                           dir2save = getwd())
    }

    ## Download RFE
    if(action == 'down.RFE'){
        ret.params <- list(action = action,
                           tstep = "dekadal",
                           rfe.src = "tamsatv3.1-af",
                           iridl.src = FALSE,
                           minhour = 1,
                           date.range = date.range,
                           bbox = .cdtData$Config$region,
                           login = list(usr = "", pwd = ""),
                           dir2save = getwd()
                        )
    }

    ## Download Reanalysis
    if(action == 'down.Reanal'){
        ret.params <- list(action = action,
                           src = "rda.ucar.edu",
                           prod = "jra55",
                           var = "tmax",
                           date.range = date.range,
                           bbox = .cdtData$Config$region,
                           dir2save = getwd(),
                           login = list(usr = "", pwd = "")
                        )
    }

    #################################################################

    ## Filling missing dekadal temperature values
    if(action == 'fill.temp'){
        ret.params <- list(action = action, tstep = tstep,
                           NCDF = list(dir = "", format = "tmax_adj_%s%s%s.nc", sample = ""),
                           STN.file = "", out.file = "",
                           Fill.Date.Range = date.range,
                           Fill.Months = 1:12,
                           Fill.Params = list(min.length = 15, dek.windows = 10)
                        )
    }

    #################################################################

    ## Conversion to CPT data format
    if(action == 'convert.CPTdata'){
        ret.params <- list(action = action,
                           data.type = "cdtstation",
                           cdtstation = "",
                           cdtnetcdf = list(dir = "", sample = "", format = "onset_%Y%M%D.nc"),
                           cptinfo = list(name = "onset", units = "days since", missval = '-9999'),
                           output = "")
    }

    ## Conversion between ncdf, geotiff, esri .hrd labeled
    if(action == 'convert.nc.tif.bil'){
        ret.params <- list(action = action,
                           dir.in = "", dir.out = "",
                           type.in = "nc", type.out = "tif",
                           nc.opts = list(varname = "precip", varunit = "mm", missval = -9999,
                                          longname = "Merged station-satellite precipitation")
                           )
    }

    ## Create a GrADS Data Descriptor File
    if(action == 'grads.ctl'){
        ret.params <- list(action = action, tstep = tstep,
                           nc = list(dir = "", sample = "", format = "rr_mrg_%Y%M%D.nc"),
                           date = date.range, out.ctl = "")
    }

    #################################################################

    ## Time series aggregation
    if(action == 'aggregate.ts'){
        ret.params <- list(action = action, in.tstep = tstep, out.tstep = "dekadal",
                           data.type = "cdtstation", cdtstation = "", cdtdataset = "",
                           cdtnetcdf = list(dir = "", sample = "", format = "rfe_%s%s%s.nc"),
                           output = "",
                           Date.Range = date.range,
                           Seasonal = list(start.mon = 1, length.mon = 3),
                           HourMin = list(int = 1, out = 1, obs.hour = 9),
                           aggr.series = list(aggr.fun = "sum", opr.fun = ">=", opr.thres = 0,
                                              min.frac = list(unique = TRUE, all = 0.95,
                                                              month = rep(0.95, 12)))
                         )
    }

    ## Time series aggregation minimum and maximum with date
    if(action == 'aggregate.minmax'){
        ret.params <- list(action = action, in.tstep = tstep, out.tstep = "dekadal",
                           cdtstation = "",  output = "",
                           Seasonal = list(start.mon = 1, length.mon = 3),
                           HourMin = list(int = 1, out = 1, obs.hour = 9),
                           aggr.series = list(aggr.fun = "max", opr.fun = ">=", opr.thres = 0,
                                              min.frac = list(unique = TRUE, all = 0.95,
                                                              month = rep(0.95, 12)))
                         )
    }

    ## Time series rolling aggregation
    if(action == 'aggregate.rf'){
        ret.params <- list(action = action, tstep = tstep, minhour = 0, Date.Range = date.range,
                           data.type = "cdtstation", cdtstation = "", cdtdataset = "",
                           cdtnetcdf = list(dir = "", sample = "", format = "rfe_%s%s%s.nc"),
                           output = "",
                           aggr.series = list(fun = "sum", win = 5, min.data = 5, align = "right",
                                              na.rm = TRUE, na.pad = TRUE, fill = FALSE)
                          )
    }

    ## Aggregate/Disaggregate Spatial NetCDF data
    if(action == 'aggregate.nc'){
        ret.params <- list(action = action, nb.ncfile = "one",
                           ncdf = list(fileordir = "", sample = ""),
                           ncdf.grid = list(use.ncgrid = FALSE, file = "", match.var = FALSE),
                           but = "Aggregate", method = "mean",
                           res = list(minlon = "", maxlon = "", reslon = 0.25, minlat = "", maxlat = "", reslat = 0.25),
                           output = "")
    }

    #################################################################

    ## Compute derived temperature variables
    ## TODO: remove range use ncInfo.no.date.range
    if(action == 'compute.dervTemp'){
        ret.params <- list(action = action, Tstep = tstep,
                           variable = "Mean", data.type = "cdtstation",
                           cdtstation = list(tmin = "", tmax = ""),
                           cdtdataset = list(tmin = "", tmax = ""),
                           cdtnetcdf = list(tmin = list(dir = "", sample = "", format = "tmin_%s%s%s.nc"),
                                            tmax = list(dir = "", sample = "", format = "tmax_%s%s%s.nc"),
                                            range = c("1900-1-1", "2100-12-31")),
                           output = "")
    }

    ## Compute Hargreaves potential evapotranspiration
    # Hargreaves (HAR), Modified-Hargreaves (MHAR)
    ## TODO: remove range use ncInfo.no.date.range
    if(action == 'compute.PET'){
        ret.params <- list(action = action, Tstep = tstep,
                           method = "HAR", data.type = "cdtstation",
                           cdtstation = list(tmin = "", tmax = "", prec = ""),
                           cdtdataset = list(tmin = "", tmax = "", prec = ""),
                           cdtnetcdf = list(tmin = list(dir = "", sample = "", format = "tmin_%s%s%s.nc"),
                                            tmax = list(dir = "", sample = "", format = "tmax_%s%s%s.nc"),
                                            prec = list(dir = "", sample = "", format = "precip_%s%s%s.nc"),
                                            range = c("1900-1-1", "2100-12-31")),
                           output = "")
    }

    ## Compute water balance
    if(action == 'compute.WB'){
        ret.params <- list(action = action, Tstep = tstep, data.type = "cdtstation",
                           cdtstation = list(etp = "", prec = ""),
                           cdtdataset = list(etp = "", prec = ""),
                           hdate = list(start.month = 1, start.day = 1, separate.year = FALSE),
                           wb = list(wb1 = 0, multi = FALSE, file = ""),
                           swhc = list(cap.max = 100, multi = FALSE, file = ""),
                           output = "")
    }

    #################################################################

    ## Mean bias
    if(action == 'coefbias.rain'){
        ret.params <- list(action = action, period = tstep, STN.file = '',
                           base.period = list(all.years = TRUE, start.year = 1981, end.year = 2010, min.year = 15),
                           RFE = list(dir = "", sample = "", format = "rfe_%s%s%s.nc"),
                           BIAS = list(method = "mbvar", AD.test = FALSE, min.length = 7,
                                       blon = 1, blat = 1),
                           interp = list(method = "idw", nmin = 3, nmax = 9, maxdist = 2.5,
                                         minstn = 10, use.block = TRUE, demfile = "",
                                         vgm.model = c("Sph", "Exp", "Gau", "Pen")),
                           grid = list(from = 'data', ncfile = "",
                                       bbox = c(.cdtData$Config$region, reslon = 0.1, reslat = 0.1)),
                           output = list(dir = getwd(), format = "STN_GRID_Bias_%s")
                          )
    }

    ## Remove bias
    if(action == 'rmbias.rain'){
        ret.params <- list(action = action, period = tstep, date.range = date.range,
                           RFE = list(dir = "", sample = "", format = "rfe_%s%s%s.nc"),
                           BIAS = list(method = "mbvar", dir = "", Adjusted.to.Zero = TRUE,
                                       format = "STN_GRID_Bias_%s"),
                           output = list(dir = getwd(), format = "rr_adj_%s%s%s.nc")
                          )
    }

    if(action == 'merge.rain'){
        ret.params <- list(action = action, period = tstep,
                           date.range = date.range, STN.file = '',
                           RFE = list(dir = "", sample = "", format = "rr_adj_%s%s%s.nc"),
                           MRG = list(method = "SBA", nrun = 3, pass = c(1, 0.75, 0.5)),
                           interp = list(vargrd = FALSE, method = "idw", nmin = 8, nmax = 16,
                                         maxdist = 2.5, minstn = 10, use.block = TRUE,
                                         vgm.model = c("Sph", "Exp", "Gau", "Pen")),
                           grid = list(from = 'data', ncfile = "",
                                       bbox = c(.cdtData$Config$region, reslon = 0.1, reslat = 0.1)),
                           output = list(dir = getwd(), format = "rr_mrg_%s%s%s.nc"),
                           blank = list(data = FALSE, shpf = ""),
                           auxvar = list(dem = FALSE, slope = FALSE, aspect = FALSE,
                                         lon = FALSE, lat = FALSE, demfile = ""),
                           RnoR = list(use = FALSE, wet = 1.0, smooth = FALSE),
                           prec = .cdtData$Config$prec.precip
                          )
    }

    if(action == 'crossv.rain'){
        ret.params <- list(action = action, period = tstep,
                           date.range = date.range, STN.file = '', outdir = "",
                           RFE = list(dir = "", sample = "", format = "rr_adj_%s%s%s.nc"),
                           MRG = list(method = "SBA", nrun = 3, pass = c(1, 0.75, 0.5)),
                           interp = list(vargrd = FALSE, method = "idw", nmin = 8, nmax = 16,
                                         maxdist = 2.5, minstn = 10, use.block = TRUE,
                                         vgm.model = c("Sph", "Exp", "Gau", "Pen")),
                           auxvar = list(dem = FALSE, slope = FALSE, aspect = FALSE,
                                         lon = FALSE, lat = FALSE, demfile = ""),
                           RnoR = list(use = FALSE, wet = 1.0, smooth = FALSE),
                           selstn = list(from = 'all', min.perc = 40, file.type = 'cdtstation', file.stn = '')
                          )
    }

    #################################################################

    ## Compute regression parameters for downscaling
    if(action == 'coefdown.temp'){
        ret.params <- list(action = action, period = tstep,
                           IO.files = list(STN.file = "", DEM.file = "", dir2save = getwd()),
                           base.period = list(all.years = TRUE, start.year = 1981, end.year = 2010, min.year = 15)
                          )
    }

    ## Downscaling reanalysis
    if(action == 'down.temp'){
        ret.params <- list(action = action, period = tstep, date.range = date.range,
                           DownCoef.file = "", DEM.file = "",
                           REANAL = list(dir = "", sample = "", format = "tmax_jra55_%s%s%s.nc"),
                           interp = list(method = "blin", nmin = 3, nmax = 9, maxdist = 2.5,
                                         minstn = 10, use.block = TRUE, demfile = "",
                                         vgm.model = c("Sph", "Exp", "Gau", "Pen")),
                           grid = list(from = 'ncdf', ncfile = "",
                                       bbox = c(.cdtData$Config$region, reslon = 0.1, reslat = 0.1)),
                           output = list(dir = getwd(), format = "tmax_down_%s%s%s.nc")
                          )
    }

    ## Bias coeff
    if(action == 'coefbias.temp'){
        ret.params <- list(action = action, period = tstep, STN.file = '',
                           base.period = list(all.years = TRUE, start.year = 1981, end.year = 2010, min.year = 15),
                           TEMP = list(dir = "", sample = "", format = "tmax_down_%s%s%s.nc"),
                           BIAS = list(method = "mbvar", SWnorm.test = FALSE, min.length = 7,
                                       blon = 1, blat = 1),
                           interp = list(method = "idw", nmin = 3, nmax = 9, maxdist = 2.5,
                                         minstn = 10, use.block = TRUE, demfile = "",
                                         vgm.model = c("Sph", "Exp", "Gau", "Pen")),
                           grid = list(from = 'data', ncfile = "",
                                       bbox = c(.cdtData$Config$region, reslon = 0.1, reslat = 0.1)),
                           output = list(dir = getwd(), format = "STN_GRID_Bias_%s")
                          )
    }

    ## Adjustment
    if(action == 'adjust.temp'){
        ret.params <- list(action = action, period = tstep, date.range = date.range,
                           TEMP = list(dir = "", sample = "", format = "tmax_down_%s%s%s.nc"),
                           BIAS = list(method = "mbvar", dir = "",
                                       format = "STN_GRID_Bias_%s"),
                           output = list(dir = getwd(), format = "tmax_adj_%s%s%s.nc")
                          )
    }

    ## Merging
    if(action == 'merge.temp'){
        ret.params <- list(action = action, period = tstep,
                           date.range = date.range, STN.file = '',
                           TEMP = list(dir = "", sample = "", format = "tmax_adj_%s%s%s.nc"),
                           MRG = list(method = "SBA", nrun = 3, pass = c(1, 0.75, 0.5)),
                           interp = list(vargrd = FALSE, method = "idw", nmin = 8, nmax = 16,
                                         maxdist = 3.5, minstn = 10, use.block = TRUE,
                                         vgm.model = c("Sph", "Exp", "Gau", "Pen")),
                           grid = list(from = 'data', ncfile = "",
                                       bbox = c(.cdtData$Config$region, reslon = 0.1, reslat = 0.1)),
                           output = list(dir = getwd(), format = "tmax_mrg_%s%s%s.nc"),
                           blank = list(data = FALSE, shpf = ""),
                           auxvar = list(dem = FALSE, slope = FALSE, aspect = FALSE,
                                         lon = FALSE, lat = FALSE, demfile = "")
                          )
    }

    ## Merging
    if(action == 'crossv.temp'){
        ret.params <- list(action = action, period = tstep,
                           date.range = date.range, STN.file = '',  outdir = "",
                           TEMP = list(dir = "", sample = "", format = "tmax_adj_%s%s%s.nc"),
                           MRG = list(method = "SBA", nrun = 3, pass = c(1, 0.75, 0.5)),
                           interp = list(vargrd = FALSE, method = "idw", nmin = 8, nmax = 16,
                                         maxdist = 3.5, minstn = 10, use.block = TRUE,
                                         vgm.model = c("Sph", "Exp", "Gau", "Pen")),
                           auxvar = list(dem = FALSE, slope = FALSE, aspect = FALSE,
                                         lon = FALSE, lat = FALSE, demfile = ""),
                           selstn = list(from = 'all', min.perc = 40, file.type = 'cdtstation', file.stn = '')
                          )
    }

    #################################################################

    ## Scale merged data
    if(action == 'scale.merged'){
        ret.params <- list(action = action, date.range = date.range, outdir = getwd(),
                           mrg.data = list(tstep = "daily", dir = "", sample = "", format = "rr_mrg_%s%s%s_ALL.nc"),
                           scale.data = list(tstep = "dekadal", fun = "sum",
                                             dir = "", sample = "", format = "rr_mrg_%s%s%s_ALL.nc")
                          )
    }

    # #################################################################

    # ## Dekadal update
    # if(action == 'merge.dekrain'){
    #     ret.params <- fromJSON(file.path(.cdtDir$dirLocal, 'init_params', 'Precip_Update_dekadal.json'))
    #     ret.params <- c(list(action = action, period = tstep), ret.params)
    #     if(str_trim(ret.params$output$dir) == "") ret.params$output$dir <- getwd()
    # }

    return(ret.params)
}
