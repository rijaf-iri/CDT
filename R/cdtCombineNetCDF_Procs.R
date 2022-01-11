
combine.netcdf_writeNC <- function(){
    GalParams <- .cdtData$GalParams
    Insert.Messages.Out(GalParams[['message']][['1']], TRUE, "i")

    rdate <- table.format.date.time(GalParams$tstep, GalParams$date.range, GalParams$minhour)
    rdate0 <- rdate
    if(GalParams$tstep == 'pentad'){
        rdate0[, 3] <- c(1, 6, 11, 16, 21, 26)[as.numeric(rdate0[, 3])]
    }
    if(GalParams$tstep == 'dekadal'){
        rdate0[, 3] <- c(1, 11, 21)[as.numeric(rdate0[, 3])]
    }

    dateOpts <- switch(GalParams$tstep,
                       'minute' = list(nc = 5, fun = as.POSIXct, format = "%Y-%m-%d-%H-%M"),
                       'hourly' = list(nc = 4, fun = as.POSIXct, format = "%Y-%m-%d-%H"),
                       'daily' = list(nc = 3, fun = as.Date, format = "%Y-%m-%d"),
                       'pentad' = list(nc = 3, fun = as.Date, format = "%Y-%m-%d"),
                       'dekadal' = list(nc = 3, fun = as.Date, format = "%Y-%m-%d"),
                       'monthly' = list(nc = 3, fun = as.Date, format = "%Y-%m-%d"),
                      )

    datetimes <- apply(rdate0[, 1:dateOpts$nc], 1, paste, collapse = "-")
    datetimes <- dateOpts$fun(datetimes, format = dateOpts$format)


    ##################

    return(0)
}
