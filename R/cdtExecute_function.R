
## Output message
Execute_end_msg <- function(outret, msgOK, msgFail){
    if(!inherits(outret, "try-error")){
        if(!is.null(outret)){
            if(outret == 0)
                Insert.Messages.Out(msgOK, TRUE, "s")
            else
                Insert.Messages.Out(msgFail, TRUE, "e")
        }else Insert.Messages.Out(msgFail, TRUE, "e")
    }else{
        Insert.Messages.Out(gsub('[\r\n]', '', outret[1]), TRUE, "e")
        Insert.Messages.Out(msgFail, TRUE, "e")
    }
}

## Toolbars execute button 
Execute_Function <- function(){
    ## Format CDTs Input station Data
    if(.cdtData$GalParams$action == "cdtInput.stn"){
        if(.cdtData$GalParams$data.type == "Multiple")
            cdtInput.stn.Fun <- formatCDTDataMultiple.Files
        if(.cdtData$GalParams$data.type == "Single")
            cdtInput.stn.Fun <- formatCDTDataSingle.File
        ret <- try(cdtInput.stn.Fun(), silent = TRUE)

        msg0 <- .cdtData$GalParams[['message']][['2']]
        msg1 <- .cdtData$GalParams[['message']][['3']]
        Execute_end_msg(ret, msg0, msg1)
    }

    ## Merge 2 CDTs station Data
    if(.cdtData$GalParams$action == "merge2CDT.stn"){
        ret <- try(merge2CDTdata(), silent = TRUE)

        msg0 <- .cdtData$GalParams[['message']][['2']]
        msg1 <- .cdtData$GalParams[['message']][['3']]
        Execute_end_msg(ret, msg0, msg1)
    }

    ## Filtering CDT station Data
    if(.cdtData$GalParams$action == "filter.data"){
        ret <- try(filterCDTdata(), silent = TRUE)

        msg0 <- .cdtData$GalParams[['message']][['2']]
        msg1 <- .cdtData$GalParams[['message']][['3']]
        Execute_end_msg(ret, msg0, msg1)
    }

    ## selecting CDT station Data
    if(.cdtData$GalParams$action == "selectCDT.data"){
        ret <- try(selectCDTdata(), silent = TRUE)

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

    ## Split multiple dimension NetCDF
    if(.cdtData$GalParams$action == 'split.NetCDF'){
        ret <- try(split_3d.netcdf_writeNC(), silent = TRUE)

        msg0 <- .cdtData$GalParams[['message']][['5']]
        msg1 <- .cdtData$GalParams[['message']][['6']]
        Execute_end_msg(ret, msg0, msg1)
    }

    ## Blanking NetCDF grid
    if(.cdtData$GalParams$action == 'blank.NetCDF'){
        ret <- try(blankNcdf_Execute(), silent = TRUE)

        msg0 <- .cdtData$GalParams[['message']][['8']]
        msg1 <- .cdtData$GalParams[['message']][['9']]
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

    ## Time series aggregation - Min and Max with Obs dates
    if(.cdtData$GalParams$action == "aggregate.minmax"){
        ret <- try(AggregateMinMax_Execute(), silent = TRUE)

        msg0 <- .cdtData$GalParams[['message']][['4']]
        msg1 <- .cdtData$GalParams[['message']][['5']]
        Execute_end_msg(ret, msg0, msg1)
    }

    ## Time series aggregation - Rolling window
    if(.cdtData$GalParams$action == "aggregate.rf"){
        ret <- try(AggregateMWin_Execute(), silent = TRUE)

        msg0 <- .cdtData$GalParams[['message']][['11']]
        msg1 <- .cdtData$GalParams[['message']][['12']]
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

    ## Compute bias coefficients RR
    if(.cdtData$GalParams$action == 'coefbias.rain'){
        ret <- try(execBiasRain(), silent = TRUE)

        msg0 <- .cdtData$GalParams[['message']][['5']]
        msg1 <- .cdtData$GalParams[['message']][['6']]
        Execute_end_msg(ret, msg0, msg1)
    }

    ## Adjust bias RFE
    if(.cdtData$GalParams$action == 'rmbias.rain'){
        ret <- try(execAdjBiasRain(), silent = TRUE)

        msg0 <- .cdtData$GalParams[['message']][['5']]
        msg1 <- .cdtData$GalParams[['message']][['6']]
        Execute_end_msg(ret, msg0, msg1)
    }

    ## Merging
    if(.cdtData$GalParams$action == 'merge.rain'){
        ret <- try(execMergeRain(), silent = TRUE)
        msg0 <- .cdtData$GalParams[['message']][['8']]
        msg1 <- .cdtData$GalParams[['message']][['9']]
        Execute_end_msg(ret, msg0, msg1)
    }

    ## Compute downscaling coefficients
    if(.cdtData$GalParams$action == 'coefdown.temp'){
        ret <- try(Temp_execCoefDown(), silent = TRUE)

        msg0 <- .cdtData$GalParams[['message']][['4']]
        msg1 <- .cdtData$GalParams[['message']][['5']]
        Execute_end_msg(ret, msg0, msg1)
    }

    ## Downscale reanalysis data
    if(.cdtData$GalParams$action == 'down.temp'){
        ret <- try(Temp_execDownscaling(), silent = TRUE)

        msg0 <- .cdtData$GalParams[['message']][['6']]
        msg1 <- .cdtData$GalParams[['message']][['7']]
        Execute_end_msg(ret, msg0, msg1)
    }

    ## Compute mean bias coef
    if(.cdtData$GalParams$action == 'coefbias.temp'){
        ret <- try(execBiasTemp(), silent = TRUE)

        msg0 <- .cdtData$GalParams[['message']][['5']]
        msg1 <- .cdtData$GalParams[['message']][['6']]
        Execute_end_msg(ret, msg0, msg1)
    }

    ## bias correction
    if(.cdtData$GalParams$action == 'adjust.temp'){
        ret <- try(execAjdBiasDownTemp(), silent = TRUE)

        msg0 <- .cdtData$GalParams[['message']][['5']]
        msg1 <- .cdtData$GalParams[['message']][['6']]
        Execute_end_msg(ret, msg0, msg1)
    }

    ## Merge temp data
    if(.cdtData$GalParams$action == 'merge.temp'){
        ret <- try(execMergeTemp(), silent = TRUE)
        msg0 <- .cdtData$GalParams[['message']][['8']]
        msg1 <- .cdtData$GalParams[['message']][['9']]
        Execute_end_msg(ret, msg0, msg1)
    }

    ## Scaling merged data
    if(.cdtData$GalParams$action == 'scale.merged'){
        ret <- try(exec_ScalingUpData(), silent = TRUE)
        msg0 <- .cdtData$GalParams[['message']][['4']]
        msg1 <- .cdtData$GalParams[['message']][['5']]
        Execute_end_msg(ret, msg0, msg1)
    }

    ## Merging
    if(.cdtData$GalParams$action == 'crossv.rain'){
        ret <- try(execCrossValidRain(), silent = TRUE)
        msg0 <- "Rainfall merging cross-validation finished successfully"
        msg1 <- "Rainfall merging cross-validation failed"
        Execute_end_msg(ret, msg0, msg1)
    }

    ## Merge temp data
    if(.cdtData$GalParams$action == 'crossv.temp'){
        ret <- try(execCrossValidTEMP(), silent = TRUE)
        msg0 <- "Temperature merging cross-validation finished successfully"
        msg1 <- "Temperature merging cross-validation failed"
        Execute_end_msg(ret, msg0, msg1)
    }

    ###
}
