
## Parameters initialization

cdt.init.params <- function(action, tstep){
	## Disabled toolbars spinbox
	spinbox.state()

	## Format CDTs Input station Data
	if(action == 'cdtInput.stn'){
		ret.params <- fromJSON(file.path(.cdtDir$dirLocal, 'init_params', 'Format_CDT_Input_Station_Data.json'))
		ret.params <- c(list(action = action, tstep = tstep), ret.params)
	}

	## Create cdt dataset from ncdf files
	if(action == 'create.CdtDataset'){
		ret.params <- fromJSON(file.path(.cdtDir$dirLocal, 'init_params', 'Create_CDT_Dataset.json'))
		ret.params <- c(list(action = action, Tstep = tstep), ret.params)
		if(str_trim(ret.params$output$dir) == "") ret.params$output$dir <- getwd()
	}

	## Download DEM
	if(action == 'down.DEM'){
		ret.params <- fromJSON(file.path(.cdtDir$dirLocal, 'init_params', 'Download_DEM.json'))
		ret.params <- c(list(action = action), ret.params)
		if(str_trim(ret.params$dir2save) == "") ret.params$dir2save <- getwd()
	}

	## Download SHP
	if(action == 'down.SHP'){
		ret.params <- fromJSON(file.path(.cdtDir$dirLocal, 'init_params', 'Download_Shapefile.json'))
		ret.params <- c(list(action = action), ret.params)
		if(str_trim(ret.params$dir2save) == "") ret.params$dir2save <- getwd()
	}

	## Download RFE
	if(action == 'down.RFE'){
		ret.params <- fromJSON(file.path(.cdtDir$dirLocal, 'init_params', 'Download_RFE.json'))
		ret.params <- c(list(action = action), ret.params)
		if(str_trim(ret.params$dir2save) == "") ret.params$dir2save <- getwd()
	}

	## Filling missing dekadal temperature values
	if(action == 'fill.temp'){
		ret.params <- fromJSON(file.path(.cdtDir$dirLocal, 'init_params', 'Filling_CDT_Temperature.json'))
		ret.params <- c(list(action = action, tstep = tstep), ret.params)
	}

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
						longname = "Merged station-satellite precipitation"))
	}

	## Create a GrADS Data Descriptor File
	if(action == 'grads.ctl'){
		ret.params <- list(action = action, tstep = tstep,
							nc = list(dir = "", sample = "", format = "rr_mrg_%Y%M%D.nc"),
							date = list(year1 = 1981, mon1 = 1, day1 = 1,
										year2 = 2017, mon2 = 12, day2 = 31),
							out.ctl = "")
	}

	## Time series aggregation
	if(action == 'aggregate.ts'){
		ret.params <- fromJSON(file.path(.cdtDir$dirLocal, 'init_params', 'Aggregate_Time_Series.json'))
		ret.params <- c(list(action = action, in.tstep = tstep), ret.params)
	}

	## Aggregate/Disaggregate Spatial NetCDF data
	if(action == 'aggregate.nc'){
		ret.params <- fromJSON(file.path(.cdtDir$dirLocal, 'init_params', 'Aggregate_Spatial_NetCDF.json'))
		ret.params <- c(list(action = action), ret.params)
	}

	## Compute derived temperature variables
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

	return(ret.params)
}
