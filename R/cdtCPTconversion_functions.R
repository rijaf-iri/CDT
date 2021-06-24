
CPT.convertCDTdates <- function(dates){
    dates <- as.character(dates)
    ndate <- nchar(dates[1])
    cpt <- NULL

    # clim month, dekad, pentad
    if(ndate == 2){
        irang <- as.numeric(dates)
        if(any(irang > 36)){
            mon <- rep(str_pad(1:12, 2, pad = "0"), each = 6)
            pen <- rep(1:6, 12)
            eom <- sapply(seq_along(mon), function(i){
                            daty <- as.Date(paste("2014", mon[i], 28:31, sep = '-'))
                            rev((28:31)[which(!is.na(daty))])[1]
                        })
            pendt <- pen
            pendt[pen == 1] <- '01/05'
            pendt[pen == 2] <- '06/10'
            pendt[pen == 3] <- '11/15'
            pendt[pen == 4] <- '16/20'
            pendt[pen == 5] <- '21/15'
            pendt[pen == 6] <- paste(26, eom[pen == 6], sep = '/')
            pendt <- paste(mon, pendt, sep = "-")
            cpt <- pendt[irang]
        }else if(any(irang > 12)){
            mon <- rep(str_pad(1:12, 2, pad = "0"), each = 3)
            dek <- rep(1:3, 12)
            eom <- sapply(seq_along(mon), function(i){
                            daty <- as.Date(paste("2014", mon[i], 28:31, sep = '-'))
                            rev((28:31)[which(!is.na(daty))])[1]
                        })
            dekdt <- dek
            dekdt[dek == 1] <- '01/10'
            dekdt[dek == 2] <- '11/20'
            dekdt[dek == 3] <- paste(21, eom[dek == 3], sep = '/')
            dekdt <- paste(mon, dekdt, sep = "-")
            cpt <- dekdt[irang]
        }else{
            mon <- str_pad(1:12, 2, pad = "0")
            cpt <- mon[irang]
        }
    }
    # clim day
    if(ndate == 3){
        mondd <- format(seq(as.Date("2014-1-1"), as.Date("2014-12-31"), "day"), "%m-%d")
        cpt <- mondd[as.numeric(dates)]
    }
    # annual, clim pentad-dekad
    if(ndate == 4){
        ## clim pentad-dekad
        if(substr(dates[1], 3, 3) == "-"){
            mon <- substr(dates, 1, 2)
            ptdk <- as.numeric(substr(dates, 4, 4))
            eom <- sapply(seq_along(mon), function(i){
                            daty <- as.Date(paste("2014", mon[i], 28:31, sep = '-'))
                            rev((28:31)[which(!is.na(daty))])[1]
                        })
            pendek <- ptdk
            if(any(unique(ptdk) > 3)){
                pendek[ptdk == 1] <- '01/05'
                pendek[ptdk == 2] <- '06/10'
                pendek[ptdk == 3] <- '11/15'
                pendek[ptdk == 4] <- '16/20'
                pendek[ptdk == 5] <- '21/15'
                pendek[ptdk == 6] <- paste(26, eom[ptdk == 3], sep = '/')
            }else{
                pendek[ptdk == 1] <- '01/10'
                pendek[ptdk == 2] <- '11/20'
                pendek[ptdk == 3] <- paste(21, eom[ptdk == 3], sep = '/')
            }
            cpt <- paste(mon, pendek, sep = '-')
        }else cpt <- dates # annual
    }
    # clim day
    if(ndate == 5 & substr(dates[1], 3, 3) == "-") cpt <- dates
    # month
    if(ndate == 6)
        cpt <- paste(substr(dates, 1, 4), substr(dates, 5, 6), sep = '-')
    # pentad, dekad
    if(ndate == 7){
        year <- substr(dates, 1, 4)
        mon <- substr(dates, 5, 6)
        ptdk <- as.numeric(substr(dates, 7, 7))
        eom <- sapply(seq_along(year), function(i){
                        daty <- as.Date(paste(year[i], mon[i], 28:31, sep = '-'))
                        rev((28:31)[which(!is.na(daty))])[1]
                    })
        pendek <- ptdk
        if(any(unique(ptdk) > 3)){
            pendek[ptdk == 1] <- '01/05'
            pendek[ptdk == 2] <- '06/10'
            pendek[ptdk == 3] <- '11/15'
            pendek[ptdk == 4] <- '16/20'
            pendek[ptdk == 5] <- '21/15'
            pendek[ptdk == 6] <- paste(26, eom[ptdk == 6], sep = '/')
        }else{
            pendek[ptdk == 1] <- '01/10'
            pendek[ptdk == 2] <- '11/20'
            pendek[ptdk == 3] <- paste(21, eom[ptdk == 3], sep = '/')
        }
        cpt <- paste(year, mon, pendek, sep = '-')
    }
    # day
    if(ndate == 8)
        cpt <- paste(substr(dates, 1, 4), substr(dates, 5, 6), substr(dates, 7, 8), sep = '-')
    # clim annual
    if(ndate == 9 & substr(dates[1], 5, 5) == "-")
        cpt <- paste(substr(dates, 1, 4), substr(dates, 6, 9), sep = '/')
    # clim month
    if(ndate == 12 & substr(dates[1], 10, 10) == "_") cpt <- substr(dates, 11, 12)
    # clim pentad, dekad
    if(ndate == 13 & substr(dates[1], 10, 10) == "_"){
        mon <- substr(dates, 11, 12)
        ptdk <- as.numeric(substr(dates, 13, 13))
        eom <- sapply(seq_along(mon), function(i){
                        daty <- as.Date(paste("2014", mon[i], 28:31, sep = '-'))
                        rev((28:31)[which(!is.na(daty))])[1]
                    })
        pendek <- ptdk
        if(any(unique(ptdk) > 3)){
            pendek[ptdk == 1] <- '01/05'
            pendek[ptdk == 2] <- '06/10'
            pendek[ptdk == 3] <- '11/15'
            pendek[ptdk == 4] <- '16/20'
            pendek[ptdk == 5] <- '21/15'
            pendek[ptdk == 6] <- paste(26, eom[ptdk == 6], sep = '/')
        }else{
            pendek[ptdk == 1] <- '01/10'
            pendek[ptdk == 2] <- '11/20'
            pendek[ptdk == 3] <- paste(21, eom[ptdk == 3], sep = '/')
        }
        cpt <- paste(mon, pendek, sep = '-')
    }
    # clim day
    if(ndate == 14 & substr(dates[1], 10, 10) == "_")
        cpt <- paste(substr(dates, 11, 12), substr(dates, 13, 14), sep = '-')
    if(ndate == 15){
        # season
        if(substr(dates[1], 8, 8) == "_"){
            year1 <- substr(dates, 1, 4)
            mon1 <- substr(dates, 6, 7)
            year2 <- substr(dates, 9, 12)
            mon2 <- substr(dates, 14, 15)
            if(all(year1 == year2)){
                if(all(mon1 == mon2)) cpt <- paste0(year1, "-", mon1)
                else{
                    cpt <- if(all((as.numeric(mon2)-as.numeric(mon1)+1) %in% c(0, 12))) year1 else paste0(year1, "-", mon1, "/", mon2)
                }
            }else cpt <- paste0(year1, "-", mon1, "/", year2, "-", mon2)
        }
        # clim season
        if(substr(dates[1], 10, 10) == "_"){
            year1 <- substr(dates, 1, 4)
            year2 <- substr(dates, 6, 9)
            mon1 <- substr(dates, 11, 12)
            mon2 <- substr(dates, 14, 15)
            if(all(mon1 == "01") & all(mon2 == "12")){
                cpt <- paste0(year1, "/", year2)
            }else{
                cpt <- if(all((as.numeric(mon2)-as.numeric(mon1)+1) %in% c(0, 12))) paste0(year1, "/", year2) else paste0(mon1, "/", mon2)
            }
        }
    }
    if(ndate == 17 & substr(dates[1], 9, 9) == "_"){
        year1 <- substr(dates, 1, 4)
        mon1 <- substr(dates, 5, 6)
        day1 <- substr(dates, 7, 8)
        year2 <- substr(dates, 10, 13)
        mon2 <- substr(dates, 14, 15)
        day2 <- substr(dates, 16, 17)
        eom2 <- sapply(seq_along(year2), function(i){
                        daty <- as.Date(paste(year2[i], mon2[i], 28:31, sep = '-'))
                        daty <- rev((28:31)[which(!is.na(daty))])[1]
                        as.character(daty)
                    })

        if(all(year1 == year2)){
            if(all(mon1 == "01") & all(day1 == "01") & all(mon2 == "12") & all(day2 == "31")) cpt <- year1
            else if(all(mon1 == mon2) & all(day1 == "01") & all(day2 == eom2)) cpt <- paste(year1, mon1, sep = "-")
            else if(all(mon1 == mon2) & all(mon1 == "02") & all(day1 == "01") & any(day2 == eom2)){
                iend <- day2 != eom2
                cpt <- paste(year1, mon1, sep = "-")
                cpt[iend] <- paste(year1, mon1, paste(day1, day2, sep = "/"), sep = "-")[iend]
            }
            else if(all(mon1 != mon2) & all(day1 == "01") & all(day2 == eom2)) cpt <- paste(year1, paste(mon1, mon2, sep = "/"), sep = "-")
            else if(all(mon1 != mon2) & all(mon2 == "02") & all(day1 == "01") & any(day2 == eom2)){
                iend <- day2 != eom2
                cpt <- paste(year1, paste(mon1, mon2, sep = "/"), sep = "-")
                cpt[iend] <- paste(paste(year1, mon1, day1, sep = "-"), paste(year1, mon2, day2, sep = "-"), sep = "/")[iend]
            }
            else cpt <- paste(paste(year1, mon1, day1, sep = "-"), paste(year1, mon2, day2, sep = "-"), sep = "/")
        }else{
            if(all(day1 == "01") & all(day2 == eom2)) cpt <- paste(paste(year1, mon1, sep = "-"), paste(year2, mon2, sep = "-"), sep = "/")
            else if(all(mon2 == "02") & all(day1 == "01") & any(day2 == eom2)){
                iend <- day2 != eom2
                cpt <- paste(paste(year1, mon1, sep = "-"), paste(year2, mon2, sep = "-"), sep = "/")
                cpt[iend] <- paste(paste(year1, mon1, day1, sep = "-"), paste(year2, mon2, day2, sep = "-"), sep = "/")[iend]
            }
            else cpt <- paste(paste(year1, mon1, day1, sep = "-"), paste(year2, mon2, day2, sep = "-"), sep = "/")
        }
    }

    return(cpt)
}

CPT.getTAG.line <- function(cpt.tags){
    cpt.tags <- paste(paste('cpt', names(cpt.tags), sep = ':'), do.call(c, cpt.tags), sep = '=')
    tags <- paste0(paste(cpt.tags, collapse = ', '), '\n')
    return(tags)
}

########

CPT.formatStationData <- function(date, z, width = 13, side = "both"){
    xout <- formatC(c(z))
    xout <- str_pad(xout, width = width, side = side)
    xout <- paste0("\t", xout)
    dim(xout) <- dim(z)
    xout <- cbind(date, xout)
    xout <- paste0(apply(xout, 1, paste, collapse = ""), '\n')
    return(xout)
}

CPT.convertStationData <- function(data, date, stninfo, varid, units, missval){
    ncmax <- max(nchar(formatC(c(data))))
    daty <- CPT.convertCDTdates(date)
    id <- as.character(stninfo[, 1])
    lon <- as.character(stninfo[, 2])
    lat <- as.character(stninfo[, 3])
    xmlns <- "xmlns:cpt=http://iri.columbia.edu/CPT/v10/\n"
    cpt.tags <- list(field = varid, nrow = length(date), ncol = length(lon),
                     row = 'T', col = 'station', units = units, missing = missval)
    cpt.tags <- CPT.getTAG.line(cpt.tags)
    cpt.stn <- paste0('\t', paste(id, collapse = '\t'), '\n')
    cpt.lon <- paste0('cpt:X', '\t', paste(lon, collapse = '\t'), '\n')
    cpt.lat <- paste0('cpt:Y', '\t', paste(lat, collapse = '\t'), '\n')
    cpt.data <- CPT.formatStationData(daty, data, width = ncmax)
    cpt.out <- c(xmlns, cpt.tags, cpt.stn, cpt.lon, cpt.lat, cpt.data)
    return(cpt.out)
}

CPT.convertStationData.Files <- function(cdtdata, output.file, cptInfo){
    data <- splitCDTData1(cdtdata)
    if(is.null(data)) return(NULL)

    ncmax <- max(nchar(formatC(c(data$data))))
    daty <- str_pad(data$dates, max(nchar(data$dates)), pad = "0")
    daty <- CPT.convertCDTdates(daty)

    xmlns <- "xmlns:cpt=http://iri.columbia.edu/CPT/v10/\n"
    cpt.tags <- list(field = cptInfo$name, nrow = length(data$dates), ncol = length(data$lon),
                     row = 'T', col = 'station', units = cptInfo$units, missing = cptInfo$missval)
    cpt.tags <- CPT.getTAG.line(cpt.tags)
    cpt.stn <- paste0('\t', paste(data$id, collapse = '\t'), '\n')
    cpt.lon <- paste0('cpt:X', '\t', paste(data$lon, collapse = '\t'), '\n')
    cpt.lat <- paste0('cpt:Y', '\t', paste(data$lat, collapse = '\t'), '\n')
    data$data[is.na(data$data)] <- cptInfo$missval
    cpt.data <- CPT.formatStationData(daty, data$data, width = ncmax)
    cpt.out <- c(xmlns, cpt.tags, cpt.stn, cpt.lon, cpt.lat, cpt.data)
    cat(cpt.out, file = output.file)
    return(0)
}

########

CPT.arrangeDate <- function(date){
    cptT <- paste(c("cpt:T", date), collapse = "\t")
    paste0(cptT, '\n')
}

CPT.formatGridData <- function(x, y, z, width = 13, side = "both"){
    z <- t(z)
    xout <- formatC(c(z))
    xout <- str_pad(xout, width = width, side = side)
    xout <- paste0("\t", xout)
    dim(xout) <- dim(z)
    xout <- cbind(round(y, 6), xout)
    xout <- xout[nrow(xout):1, ]
    xout <- paste0(apply(xout, 1, paste, collapse = ""), '\n')
    xout <- c(paste0("\t", paste(round(x, 6), collapse = "\t"), "\n"), xout)
    return(xout)
}

CPT.convertGridData <- function(data, date, gridinfo, varid, units, missval){
    ncmax <- max(nchar(formatC(do.call(c, data))))
    daty <- CPT.convertCDTdates(date)
    xmlns <- "xmlns:cpt=http://iri.columbia.edu/CPT/v10/\n"
    nfields <- paste0("cpt:nfields=", 1, "\n")
    cpt.date <- CPT.arrangeDate(daty)
    lon <- gridinfo$x
    lat <- gridinfo$y
    cpt.data <- lapply(seq_along(daty), function(j){
                    cpt.tags <- list(field = varid, T = daty[j], nrow = length(lat), ncol = length(lon),
                                     row = 'Y', col = 'X', units = units, missing = missval)
                    cpt.tags <- CPT.getTAG.line(cpt.tags)
                    cpt.data <- CPT.formatGridData(lon, lat, data[[j]], width = ncmax, side = "both")
                    c(cpt.tags, cpt.data)
                })
    cpt.data <- do.call(c, cpt.data)
    cpt.out <- c(xmlns, nfields, cpt.date, cpt.data)
    return(cpt.out)
}

CPT.convertGridData.One <- function(data, gridinfo, varid, units, missval){
    ncmax <- max(nchar(formatC(data)))
    xmlns <- "xmlns:cpt=http://iri.columbia.edu/CPT/v10/\n"
    nfields <- paste0("cpt:nfields=", 1, "\n")
    lon <- gridinfo$x
    lat <- gridinfo$y
    cpt.tags <- list(field = varid, nrow = length(lat), ncol = length(lon),
                     row = 'Y', col = 'X', units = units, missing = missval)
    cpt.tags <- CPT.getTAG.line(cpt.tags)
    cpt.data <- CPT.formatGridData(lon, lat, data, width = ncmax, side = "both")
    cpt.data <- list(cpt.tags, cpt.data)
    cpt.data <- do.call(c, cpt.data)
    cpt.out <- c(xmlns, nfields, cpt.data)
    return(cpt.out)
}

CPT.parse.Date.Filename <- function(dirNC, filename){
    flpos <- gregexpr('%', filename)[[1]]
    temps <- sapply(flpos, function(i) substr(filename, i+1, i+1))
    all.files <- list.files(dirNC, paste0("*.", tools::file_ext(filename), "$"))
    ymdpt <- c("Y", "M", "D", "P", "T")

    #####
    ## yearly, clim daily-pendat-dekad-month
    # %Y (4), %D (3), %P (2), %T (2), %M (2)
    if(length(flpos) == 1){
        if(temps == "Y"){
            flpos1 <- gregexpr('%', gsub("%Y", "%YYYY", filename))[[1]]
            daty <- as.numeric(substr(all.files, flpos1, flpos1+3))
            daty <- daty[!is.na(daty)]
            daty0 <- daty
        }

        if(temps %in% c("M", "T", "P", "D")){
            firstD <- substr(all.files, flpos, flpos)
            it <- switch(temps, "M" = list(1:12, 2), "T" = list(1:36, 2), "P" = list(1:72, 2), "D" = list(1:365, 3))
            daty <- if(any(firstD == "0")) str_pad(it[[1]], it[[2]], pad = "0") else it[[1]]
            daty0 <- str_pad(as.numeric(daty), it[[2]], pad = "0")
        }
        ffrmt <- gsub("%Y|%M|%D|%P|%T", "%s", filename)
        ncfiles <- sprintf(ffrmt, daty)
    }

    #####
    ## monthly, clim daily-pentad-dekad
    # %Y%M (6), %M-%D (5), %M-%P (4), %M-%T (4)
    if(length(flpos) == 2 & (temps[1] %in% ymdpt & temps[2] %in% ymdpt)){
        filename1 <- gsub("%T", "%T", gsub("%P", "%P", gsub("%D", "%DD", gsub("%M", "%MM", gsub("%Y", "%YYYY", filename)))))
        flpos1 <- gregexpr('%', filename1)[[1]]
        flpos1 <- flpos1 - 0:1

        if(isTRUE(all(temps %in% c("Y", "M")))){
            it <- lapply(temps, function(x) switch(x, "Y" = c(3, 1), "M" = c(1, 2)))
        }else{
            it <- lapply(temps, function(x) switch(x, "M" = c(1, 1), "D" = c(1, 2), "P" = c(0, 2), "T" = c(0, 2)))
        }

        odaty <- sapply(it, '[[', 2)
        daty <- lapply(seq_along(it), function(j) substr(all.files, flpos1[j], flpos1[j]+it[[j]][1]))
        daty <- do.call(cbind, daty)
        daty0 <- daty[, odaty]

        if(isTRUE(all(temps %in% c("Y", "M")))){
            daty0 <- as.Date(paste(daty0[, 1], daty0[, 2], "01", sep = "-"))
        }else{
            daty0 <- as.Date(paste("2001", daty0[, 1], daty0[, 2], sep = "-"))
        }
        ina <- !is.na(daty0)
        daty0 <- daty0[ina]
        daty <- daty[ina, ]
        idaty <- order(daty0)
        daty <- daty[idaty, ]
        daty0 <- daty0[idaty]
        if(isTRUE(all(temps %in% c("Y", "M")))){
            daty0 <- format(daty0, "%Y%m")
        }else if(isTRUE(all(temps %in% c("M", "D")))){
            daty0 <- format(daty0, "%m-%d")
        }else{
            daty0 <- paste0(format(daty0, "%m"), "-", as.numeric(format(daty0, "%d")))
        }
        ffrmt <- gsub("%Y|%M|%D|%P|%T", "%s", filename)
        ncfiles <- sprintf(ffrmt, daty[, 1], daty[, 2])
    }

    #####
    # daily, pentad, dekad
    # %Y%M%D (8), %Y%M%P (7), %Y%M%T (7)
    if(length(flpos) == 3 & (temps[1] %in% ymdpt & temps[2] %in% ymdpt & temps[3] %in% ymdpt)){
        filename1 <- gsub("%T", "%T", gsub("%P", "%P", gsub("%D", "%DD", gsub("%M", "%MM", gsub("%Y", "%YYYY", filename)))))
        flpos1 <- gregexpr('%', filename1)[[1]]
        flpos1 <- flpos1 - 0:2
        it <- lapply(temps, function(x) switch(x, "Y" = c(3, 1), "M" = c(1, 2), "D" = c(1, 3), "P" = c(0, 3), "T" = c(0, 3)))

        odaty <- sapply(it, '[[', 2)
        daty <- lapply(seq_along(it), function(j) substr(all.files, flpos1[j], flpos1[j]+it[[j]][1]))
        daty <- do.call(cbind, daty)

        daty0 <- daty[, odaty]
        daty0 <- as.Date(paste(daty0[, 1], daty0[, 2], daty0[, 3], sep = "-"))
        ina <- !is.na(daty0)
        daty0 <- daty0[ina]
        daty <- daty[ina, ]
        idaty <- order(daty0)
        daty <- daty[idaty, ]
        daty0 <- daty0[idaty]
        daty0 <- if("D" %in% temps) format(daty0, "%Y%m%d") else paste0(format(daty0, "%Y%m"), as.numeric(format(daty0, "%d")))
        ffrmt <- gsub("%Y|%M|%D|%P|%T", "%s", filename)
        ncfiles <- sprintf(ffrmt, daty[, 1], daty[, 2], daty[, 3])
    }

    #####
    # seasonal, annual
    # %Y-%M_%Y-%M (15)
    if(length(flpos) == 4 & (temps[1] == "Y" & temps[2] == "M" & temps[3] == "Y" & temps[4] == "M")){
        flpos1 <- gregexpr('%', gsub("%M", "%MM", gsub("%Y", "%YYYY", filename)))[[1]]
        flpos1 <- flpos1 - 0:3
        it <- lapply(temps, function(x) switch(x, "Y" = c(3, 1), "M" = c(1, 2)))

        daty <- lapply(seq_along(it), function(j) substr(all.files, flpos1[j], flpos1[j]+it[[j]][1]))
        daty <- do.call(cbind, daty)

        daty0 <- as.Date(paste(daty[, 3], daty[, 4], "01", sep = "-"))
        ina <- !is.na(daty0)
        daty0 <- daty0[ina]
        daty <- daty[ina, ]
        idaty <- order(daty0)
        daty <- daty[idaty, ]
        daty0 <- paste0(daty[, 1], "-", daty[, 2], "_", daty[, 3], "-", daty[, 4])
        ffrmt <- gsub("%Y|%M", "%s", filename)
        ncfiles <- sprintf(ffrmt, daty[, 1], daty[, 2], daty[, 3], daty[, 4])
    }

    #####
    # Daily_analysis
    # %Y%M%D_%Y%M%D (17)
    if(length(flpos) == 6 &
        (temps[1] == "Y" & temps[2] == "M" & temps[3] == "D") &
        (temps[4] == "Y" & temps[5] == "M" & temps[6] == "D")){
        filename1 <- gsub("%D", "%DD", gsub("%M", "%MM", gsub("%Y", "%YYYY", filename)))
        flpos1 <- gregexpr('%', filename1)[[1]]
        flpos1 <- flpos1 - 0:5
        it <- lapply(temps, function(x) switch(x, "Y" = c(3, 1), "M" = c(1, 2), "D" = c(1, 3)))

        daty <- lapply(seq_along(it), function(j) substr(all.files, flpos1[j], flpos1[j]+it[[j]][1]))
        daty <- do.call(cbind, daty)

        daty0 <- as.Date(paste(daty[, 4], daty[, 5], daty[, 6], sep = "-"))
        ina <- !is.na(daty0)
        daty0 <- daty0[ina]
        daty <- daty[ina, ]
        idaty <- order(daty0)
        daty <- daty[idaty, ]
        daty0 <- paste0(daty[, 1], daty[, 2], daty[, 3], "_", daty[, 4], daty[, 5], daty[, 6])
        ffrmt <- gsub("%Y|%M|%D", "%s", filename)
        ncfiles <- sprintf(ffrmt, daty[, 1], daty[, 2], daty[, 3], daty[, 4], daty[, 5], daty[, 6])
    }

    daty <- daty0
    ncpath <- file.path(dirNC, ncfiles)
    nc.exist <- file.exists(ncpath)
    if(!any(nc.exist)) return(NULL)
    ncpath <- ncpath[nc.exist]
    daty <- daty[nc.exist]

    out <- list(ncpath = ncpath, dates = daty)
    return(out)
}

CPT.convertGridData.Files <- function(ncInfo, output.file, cptInfo = NULL){
    cnpth.daty <- CPT.parse.Date.Filename(ncInfo$dir, ncInfo$format)
    if(is.null(cnpth.daty)){
        Insert.Messages.Out(.cdtData$GalParams[['message']][['8']], format = TRUE)
        return(NULL)
    }
    cat("xmlns:cpt=http://iri.columbia.edu/CPT/v10/\n", file = output.file, append = TRUE)
    cat("cpt:nfields=1\n", file = output.file, append = TRUE)

    daty <- CPT.convertCDTdates(cnpth.daty$dates)
    cpt.date <- CPT.arrangeDate(daty)
    cat(cpt.date, file = output.file, append = TRUE)

    nc <- nc_open(cnpth.daty$ncpath[1])
    lon <- nc$var[[ncInfo$varid]]$dim[[ncInfo$ilon]]$vals
    lat <- nc$var[[ncInfo$varid]]$dim[[ncInfo$ilat]]$vals
    nc.units <- nc$var[[ncInfo$varid]]$units
    nc.missval <- nc$var[[ncInfo$varid]]$missval
    nc_close(nc)

    ncInfo$xo <- order(lon)
    lon <- lon[ncInfo$xo]
    ncInfo$yo <- order(lat)
    lat <- lat[ncInfo$yo]
    nlon <- length(lon)
    nlat <- length(lat)

    if(is.null(cptInfo)){
        cptInfo <- NULL
        cptInfo$name <- ncInfo$varid
        cptInfo$units <- nc.units
        cptInfo$missval <- if(is.nan(nc.missval) | is.na(nc.missval)) -9999 else nc.missval
    }

    for(jj in seq_along(daty)){
        cpt.tags <- list(field = cptInfo$name, T = daty[jj], nrow = nlat, ncol = nlon,
                         row = 'Y', col = 'X', units = cptInfo$units, missing = cptInfo$missval)
        cpt.tags <- CPT.getTAG.line(cpt.tags)
        cat(cpt.tags, file = output.file, append = TRUE)

        nc <- nc_open(cnpth.daty$ncpath[jj])
        xdon <- ncvar_get(nc, varid = ncInfo$varid)
        nc_close(nc)
        xdon <- transposeNCDFData(xdon, ncInfo)
        xdon[is.na(xdon)] <- cptInfo$missval
        ncmax <- max(nchar(formatC(xdon)))

        cpt.data <- CPT.formatGridData(lon, lat, xdon, width = ncmax, side = "both")
        cat(cpt.data, file = output.file, append = TRUE)
    }
    return(0)
}
