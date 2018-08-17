
## Output message
Execute_end_msg <- function(outret, msgOK, msgFail){
	if(!inherits(outret, "try-error")){
		if(!is.null(outret)){
			if(outret == 0) Insert.Messages.Out(msgOK)
			else Insert.Messages.Out(msgFail, format = TRUE)
		}else Insert.Messages.Out(msgFail, format = TRUE)
	}else{
		Insert.Messages.Out(msgFail, format = TRUE)
		Insert.Messages.Out(gsub('[\r\n]', '', outret[1]), format = TRUE)
	}
}

## Toolbars execute button 
Execute_Function <- function(){
	## Format CDTs Input station Data
	if(.cdtData$GalParams$action == "cdtInput.stn"){		
		if(.cdtData$GalParams$data.type == "Multiple") cdtInput.stn.Fun <- formatCDTDataMultiple.Files
		if(.cdtData$GalParams$data.type == "Single") cdtInput.stn.Fun <- formatCDTDataSingle.File
		ret <- try(cdtInput.stn.Fun(), silent = TRUE)

		msg0 <- .cdtData$GalParams[['message']][['2']]
		msg1 <- .cdtData$GalParams[['message']][['3']]
		Execute_end_msg(ret, msg0, msg1)
	}

	## Create cdt dataset from ncdf files
	if(.cdtData$GalParams$action == "create.CdtDataset"){
		ret <- try(cdtDataset_readData(), silent = TRUE)

		msg0 <- .cdtData$GalParams[['message']][['4']]
		msg1 <- .cdtData$GalParams[['message']][['5']]
		Execute_end_msg(ret, msg0, msg1)
	}

	## Conversion to CPT data format
	if(.cdtData$GalParams$action == "convert.CPTdata"){
		ret <- try(CPT.convertProcs(), silent = TRUE)

		msg0 <- .cdtData$GalParams[['message']][['4']]
		msg1 <- .cdtData$GalParams[['message']][['5']]
		Execute_end_msg(ret, msg0, msg1)
	}

	## Conversion between ncdf, geotiff, esri .hrd labeled
	if(.cdtData$GalParams$action == "convert.nc.tif.bil"){
		ret <- try(rasterData.convert_Proc(), silent = TRUE)

		msg0 <- .cdtData$GalParams[['message']][['3']]
		msg1 <- .cdtData$GalParams[['message']][['4']]
		Execute_end_msg(ret, msg0, msg1)
	}

	## Create a GrADS Data Descriptor File
	if(.cdtData$GalParams$action == "grads.ctl"){
		ret <- try(grads_create.ctl_Procs(), silent = TRUE)

		msg0 <- .cdtData$GalParams[['message']][['4']]
		msg1 <- .cdtData$GalParams[['message']][['5']]
		Execute_end_msg(ret, msg0, msg1)
	}

	## Filling missing dekadal temperature values
	if(.cdtData$GalParams$action == "fill.temp"){
		ret <- try(fill_DekTemp_MissVal(), silent = TRUE)

		msg0 <- .cdtData$GalParams[['message']][['5']]
		msg1 <- .cdtData$GalParams[['message']][['6']]
		Execute_end_msg(ret, msg0, msg1)
	}

	## Time series aggregation
	if(.cdtData$GalParams$action == "aggregate.ts"){
		ret <- try(AggregateTS_Execute(), silent = TRUE)

		msg0 <- .cdtData$GalParams[['message']][['4']]
		msg1 <- .cdtData$GalParams[['message']][['5']]
		Execute_end_msg(ret, msg0, msg1)
	}

	## Aggregate/Disaggregate Spatial NetCDF data
	if(.cdtData$GalParams$action == "aggregate.nc"){
		ret <- try(AggregateSpNc_Execute(), silent = TRUE)

		msg0 <- .cdtData$GalParams[['message']][['1']]
		msg1 <- .cdtData$GalParams[['message']][['2']]
		Execute_end_msg(ret, msg0, msg1)
	}

	## Compute derived temperature variables
	if(.cdtData$GalParams$action == "compute.dervTemp"){
		ret <- try(computeTvarsProcs(), silent = TRUE)

		msg <- paste("Computing", .cdtData$GalParams$Tstep, tolower(.cdtData$GalParams$variable), "temperature")
		msg0 <- paste(msg, "finished successfully")
		msg1 <- paste(msg, "failed")
		Execute_end_msg(ret, msg0, msg1)
	}

	## Compute Hargreaves potential evapotranspiration
	if(.cdtData$GalParams$action == "compute.PET"){
		ret <- try(computePETProcs(), silent = TRUE)

		msg <- paste("Computing", .cdtData$GalParams$Tstep, "potential evapotranspiration")
		msg0 <- paste(msg, "finished successfully")
		msg1 <- paste(msg, "failed")
		Execute_end_msg(ret, msg0, msg1)
	}

	## Compute water balance
	if(.cdtData$GalParams$action == "compute.WB"){
		ret <- try(computeWBProcs(), silent = TRUE)

		msg <- paste("Computing", .cdtData$GalParams$Tstep, "water balance")
		msg0 <- paste(msg, "finished successfully")
		msg1 <- paste(msg, "failed")
		Execute_end_msg(ret, msg0, msg1)
	}

	## Compute downscaling coefficients
	if(.cdtData$GalParams$action == 'coefdown.temp'){
		ret <- try(Temp_execCoefDown(), silent = TRUE)

		msg0 <- "Computing regression parameters finished successfully"
		msg1 <- "Computing regression parameters failed"
		Execute_end_msg(ret, msg0, msg1)
	}

	## Downscale reanalysis data
	if(.cdtData$GalParams$action == 'down.temp'){
		ret <- try(Temp_execDownscaling(), silent = TRUE)

		msg0 <- "Downscaling finished successfully"
		msg1 <- "Downscaling failed"
		Execute_end_msg(ret, msg0, msg1)
	}

	###

}
