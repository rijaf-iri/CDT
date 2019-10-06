
legendLabel.SeasonAnalysis <- function(lab.breaks, varSeas, donDate, start.date)
{
    if(varSeas %in% c("Onset", "Cessation")){
        .start.date <- format(start.date, '%Y%m%d')
        start.dateYear <- format(start.date, '%Y')
        odaty <- .start.date[start.dateYear == donDate]
        odaty <- as.character(as.Date(odaty, '%Y%m%d'))
        legendLabel <- format(as.Date(lab.breaks, origin = odaty), '%d-%b')
    }else legendLabel <- lab.breaks
    return(legendLabel)
}

legendLabel.SeasonAnalysis1 <- function(lab.breaks, varSeas, statClim, start.date)
{
    if(varSeas %in% c("Onset", "Cessation") &
        statClim %in% c("Average", "Median", "Percentiles"))
    {
        odaty <- format(start.date[1], '%Y-%m-%d')
        legendLabel <- format(as.Date(lab.breaks, origin = odaty), '%d-%b')
    }else legendLabel <- lab.breaks
    return(legendLabel)
}

#######################################

SeasonAnalysis.plot.TSMaps <- function(){
    TSMapOp <- .cdtData$EnvData$TSMapOp
    don <- .cdtData$EnvData$tsdata

    if(!TSMapOp$title$user){
        .titre <- switch(str_trim(tclvalue(.cdtData$EnvData$varPICSA)),
                        "Onset" = "Starting dates of the rainy season",
                        "Cessation" = "Ending dates of the rainy season",
                        "Season Length" = "Length of the rainy season",
                        "Seasonal Rainfall Amounts" = "Seasonal rainfall amounts",
                        "Longest Dry Spell" = "Longest dry spell",
                        "Number of rain day" = "Seasonal number of rainy days",
                        "Maximum daily rain" = 'Seasonal maximum of daily rainfall',
                        "Total rain when RR>95thPerc" = 'Seasonal total of precipitation when RR > 95th percentile',
                        "Nb of day when RR>95thPerc" = 'Seasonal count of days when RR > 95th percentile',
                        "Dry Spells" = {
                            drydef <- str_trim(tclvalue(tkget(.cdtData$EnvData$spin.TsMap.dryspell)))
                            paste0("Dry spells - ", drydef, " or more consecutive days")
                            })
    }else .titre <- TSMapOp$title$title

    #################

    legend.texta <- switch(str_trim(tclvalue(.cdtData$EnvData$varPICSA)),
                        "Onset" = NULL,
                        "Cessation" = NULL,
                        "Season Length" = 'Number of Days',
                        "Seasonal Rainfall Amounts" = 'Rainfall Amount (mm)',
                        "Longest Dry Spell" = 'Number of Days',
                        "Number of rain day" = 'Number of Days',
                        "Maximum daily rain" = 'Rainfall Depth (mm)',
                        "Total rain when RR>95thPerc" = 'Rainfall Amount (mm)',
                        "Nb of day when RR>95thPerc" = 'Number of Days',
                        "Dry Spells" = 'Number of Dry Spells')

    #################

    .data.type <- .cdtData$EnvData$plot.maps$.data.type
    .plot.type <- str_trim(tclvalue(.cdtData$EnvData$plot.maps$plot.type))
    map.args <- cdt.plotmap.args(don, TSMapOp, .cdtData$EnvData$shp,
                                legend.text = legend.texta,
                                label.fun = legendLabel.SeasonAnalysis,
                                varSeas = str_trim(tclvalue(.cdtData$EnvData$varPICSA)),
                                donDate = str_trim(tclvalue(tkget(.cdtData$EnvData$spin.TsMap.year))),
                                start.date = .cdtData$EnvData$output$start.date
                            )

    opar <- par(mar = map.args$mar)
    map.args.add <- list(titre = .titre,
                        SHPOp = .cdtData$EnvData$SHPOp,
                        MapOp = TSMapOp,
                        data.type = .data.type,
                        plot.type = .plot.type)
    map.args <- map.args[!(names(map.args) %in% "mar")]
    map.args <- c(map.args, map.args.add)
    par.plot <- do.call(cdt.plotmap.fun, map.args)

    ## scale bar
    cdt.plotmap.scalebar(TSMapOp$scalebar)

    par(opar)

    return(par.plot)
}

#######################################

SeasonAnalysis.plot.ClimMaps <- function(){
    don <- .cdtData$EnvData$climdata
    climMapOp <- .cdtData$EnvData$climMapOp
    StatOp <- str_trim(tclvalue(.cdtData$EnvData$analysis.method))

    ## titre
    if(!climMapOp$title$user){
        .titre <- switch(str_trim(tclvalue(.cdtData$EnvData$varPICSA)),
                        "Onset" = "Starting dates of the rainy season",
                        "Cessation" = "Ending dates of the rainy season",
                        "Season Length" = "Length of the rainy season",
                        "Seasonal Rainfall Amounts" = "Seasonal rainfall amounts",
                        "Longest Dry Spell" = "Longest dry spell",
                        "Number of rain day" = "Seasonal number of rainy days",
                        "Maximum daily rain" = 'Seasonal maximum of daily rainfall',
                        "Total rain when RR>95thPerc" = 'Seasonal total of precipitation when RR > 95th percentile',
                        "Nb of day when RR>95thPerc" = 'Seasonal count of days when RR > 95th percentile',
                        "Dry Spells" = {
                                drydef <- str_trim(tclvalue(tkget(.cdtData$EnvData$spin.TsMap.dryspell)))
                                paste0("Dry spells - ", drydef, " or more consecutive days")
                            })
    }else .titre <- climMapOp$title$title

    #################

    if(!climMapOp$colkeyLab$user){
        start.dateYear <- as.numeric(format(.cdtData$EnvData$output$start.date, '%Y'))
        utrnd <- (diff(range(start.dateYear, na.rm = TRUE))+1)
        uu <- TRUE
        if(str_trim(tclvalue(.cdtData$EnvData$trend)) == "Change (trend) / year") utrnd <- "/ year"
        if(str_trim(tclvalue(.cdtData$EnvData$trend)) == "Change (trend) over the period") utrnd <- paste("over", utrnd, "years")
        if(str_trim(tclvalue(.cdtData$EnvData$trend)) == "Change (trend) / average (in %)"){
             utrnd <- "change / average (in %)"
             uu <- FALSE
        }
        dryUn <- "Number of Dry Spells"

        legUnit <- switch(str_trim(tclvalue(.cdtData$EnvData$varPICSA)),
                    "Onset" = list(NULL, NULL, "days", NULL, "count", if(uu) paste("days", utrnd) else utrnd),
                    "Cessation" = list(NULL, NULL, "days", NULL, "count", if(uu) paste("days", utrnd) else utrnd),
                    "Season Length" = list("days", "days", "days", "days", "count", if(uu) paste("days", utrnd) else utrnd),
                    "Seasonal Rainfall Amounts" = list("mm", "mm", "mm", "mm", "count", if(uu) paste("mm", utrnd) else utrnd),
                    "Longest Dry Spell" = list("days", "days", "days", "days", "count", if(uu) paste("days", utrnd) else utrnd),
                    "Number of rain day" = list("days", "days", "days", "days", "count", if(uu) paste("days", utrnd) else utrnd),
                    "Maximum daily rain" = list("mm", "mm", "mm", "mm", "count", if(uu) paste("mm", utrnd) else utrnd),
                    "Total rain when RR>95thPerc" = list("mm", "mm", "mm", "mm", "count", if(uu) paste("mm", utrnd) else utrnd),
                    "Nb of day when RR>95thPerc" = list("days", "days", "days", "days", "count", if(uu) paste("days", utrnd) else utrnd),
                    "Dry Spells" = list(dryUn, dryUn, dryUn, dryUn, "count", if(uu) paste(dryUn, utrnd) else utrnd))

        StatVal <- c("Average", "Median", "Standard deviation", "Percentiles", "Frequency", "Trend")
        units <- legUnit[[which(StatVal == StatOp)]]
        units <- if(!is.null(units)) paste0("(Units: ", units, ")") else ""
        legend.texta <- paste(StatOp, units)
    }else legend.texta <- climMapOp$colkeyLab$label


    #################

    .data.type <- .cdtData$EnvData$plot.maps$.data.type
    .plot.type <- str_trim(tclvalue(.cdtData$EnvData$plot.maps$plot.type))
    map.args <- cdt.plotmap.args(don, climMapOp, .cdtData$EnvData$shp,
                                legend.text = legend.texta,
                                label.fun = legendLabel.SeasonAnalysis1,
                                varSeas = str_trim(tclvalue(.cdtData$EnvData$varPICSA)),
                                statClim = StatOp,
                                start.date = .cdtData$EnvData$output$start.date
                            )

    opar <- par(mar = map.args$mar)
    map.args.add <- list(titre = .titre,
                        SHPOp = .cdtData$EnvData$SHPOp,
                        MapOp = climMapOp,
                        data.type = .data.type,
                        plot.type = .plot.type)
    map.args <- map.args[!(names(map.args) %in% "mar")]
    map.args <- c(map.args, map.args.add)
    par.plot <- do.call(cdt.plotmap.fun, map.args)

    ## scale bar
    cdt.plotmap.scalebar(climMapOp$scalebar)

    par(opar)

    return(par.plot)
}

#######################################

SeasonAnalysis.plot.TSGraph <- function(){
    TSGraphOp <- .cdtData$EnvData$TSGraphOp
    dryspl <- as.numeric(str_trim(tclvalue(tkget(.cdtData$EnvData$spin.TsMap.dryspell))))
    varPICSA <- str_trim(tclvalue(.cdtData$EnvData$varPICSA))

    if(.cdtData$EnvData$output$data.type == "cdtstation"){
        ixy <- which(.cdtData$EnvData$output$data$id == str_trim(tclvalue(.cdtData$EnvData$plot.maps$stnIDTSp)))
        if(length(ixy) == 0){
            Insert.Messages.Out("Station not found", format = TRUE)
            return(NULL)
        }

        if(str_trim(tclvalue(.cdtData$EnvData$plot.maps$varTSp)) == "From Maps"){
            don <- .cdtData$EnvData$tsdata$data[, ixy]
            if(varPICSA == "Dry Spells"){
                nval <- sapply(don, function(x) (length(x) == 1) & is.na(x[1]))
                don <- sapply(don, function(x) sum(!is.na(x) & x >= dryspl))
                don[nval] <- NA
            }else don <- as.numeric(don)
            # if(str_trim(tclvalue(.cdtData$EnvData$varPICSA)) == "Longest Dry Spell"){
            #   don <- sapply(don, max, na.rm = TRUE)
            #   don[is.infinite(don)] <- NA
            # }
            dates <- .cdtData$EnvData$tsdata$date
            daty <- as.numeric(substr(dates, 1, 4))
        }else{
            don <- .cdtData$EnvData$daily.precip[, ixy]
            dates <- .cdtData$EnvData$output$data$date
        }

        .cdtData$EnvData$location <- paste0("Station: ", .cdtData$EnvData$output$data$id[ixy])
    }else{
        tsdata.dir <- switch(varPICSA,
                            "Onset" = "Onset_days",
                            "Cessation" = "Cessation_days",
                            "Season Length" = "Season_length",
                            "Seasonal Rainfall Amounts" = "Seasonal_rain_amount",
                            "Number of rain day" = "Number_rainy_day",
                            "Maximum daily rain" = "Maximum_rain_daily",
                            "Total rain when RR>95thPerc" = "Total_rain_above_Perc95th",
                            "Nb of day when RR>95thPerc" = "Number_day_above_Perc95th",
                            "Longest Dry Spell" = "Dry_Spells",
                            "Dry Spells" = "Dry_Spells")

        cdtdataset <- .cdtData$EnvData$cdtdataset
        xlon <- cdtdataset$coords$mat$x
        xlat <- cdtdataset$coords$mat$y
        ilon <- as.numeric(str_trim(tclvalue(.cdtData$EnvData$plot.maps$lonLOC)))
        ilat <- as.numeric(str_trim(tclvalue(.cdtData$EnvData$plot.maps$latLOC)))

        iclo <- findInterval(ilon, xlon)
        ilo <- iclo + (2 * ilon > xlon[iclo] + xlon[iclo + 1])
        icla <- findInterval(ilat, xlat)
        ila <- icla + (2 * ilat > xlat[icla] + xlat[icla + 1])

        if(is.na(ilo) | is.na(ila)){
            Insert.Messages.Out("Coordinates outside of data range", format = TRUE)
            return(NULL)
        }
        ixy <- ilo + length(xlon) * (ila - 1)

        if(str_trim(tclvalue(.cdtData$EnvData$plot.maps$varTSp)) == "From Maps"){
            don <- readCdtDatasetChunk.locations(ixy, cdtdataset$fileInfo, cdtdataset, chunkDir = tsdata.dir, do.par = FALSE)

            if(varPICSA == "Dry Spells"){
                nval <- sapply(don, function(x) (length(x) == 1) & is.na(x[1]))
                don <- sapply(don, function(x) sum(!is.na(x) & x >= dryspl))
                don[nval] <- NA
            }
            if(varPICSA == "Longest Dry Spell"){
                don <- sapply(don, max, na.rm = TRUE)
                don[is.infinite(don)] <- NA
            }

            don <- as.numeric(don$data[, 1])
            dates <- cdtdataset$dateInfo$date
            daty <- as.numeric(substr(dates, 1, 4))
        }else{
            don <- readCdtDatasetChunk.locations(ixy, .cdtData$EnvData$output$daily.precip, .cdtData$EnvData$daily.precip, do.par = FALSE)
            don <- as.numeric(don$data[.cdtData$EnvData$daily.precip$dateInfo$index, 1])
            dates <- .cdtData$EnvData$daily.precip$dateInfo$date
        }

        .cdtData$EnvData$location <- paste0("Longitude: ", round(ilon, 5), ", Latitude: ", round(ilat, 5))
    }

    if(str_trim(tclvalue(.cdtData$EnvData$plot.maps$varTSp)) == "Daily Rainfall"){
        ret <- picsa.plot.daily(dates, don, .cdtData$EnvData$location, .cdtData$EnvData$output$params$dryday)
        return(ret)
    }

    #########

    if(varPICSA == "Onset"){
        xlab0 <- ''
        ylab0 <- ''
        sub <- NULL
        theoretical <- FALSE
        title <- "Starting dates of the rainy season"
    }
    if(varPICSA == "Cessation"){
        xlab0 <- ''
        ylab0 <- ''
        sub <- NULL
        theoretical <- FALSE
        title <- "Ending dates of the rainy season"
    }
    if(varPICSA == "Season Length"){
        xlab0 <- 'Year'
        ylab0 <- 'Number of Days'
        sub <- NULL
        theoretical <- TRUE
        title <- "Length of the rainy season"
    }
    if(varPICSA == "Seasonal Rainfall Amounts"){
        xlab0 <- 'Year'
        ylab0 <- 'Rainfall Amount (mm)'
        sub <- NULL
        theoretical <- TRUE
        title <- "Seasonal rainfall amounts"
    }
    if(varPICSA == "Dry Spells"){
        xlab0 <- 'Year'
        ylab0 <- 'Number of Dry Spells'
        sub <- paste("Dry spells -", dryspl, "or more consecutive days")
        theoretical <- FALSE
        title <- "Dry Spells"
    }
    if(varPICSA == "Longest Dry Spell"){
        xlab0 <- 'Year'
        ylab0 <- 'Number of Days'
        sub <- NULL
        theoretical <- FALSE
        title <- "Longest dry spell"
    }
    if(varPICSA == "Number of rain day"){
        xlab0 <- 'Year'
        ylab0 <- 'Number of Days'
        sub <- NULL
        theoretical <- TRUE
        title <- "Seasonal number of rainy days"
    }
    if(varPICSA == "Maximum daily rain"){
        xlab0 <- 'Year'
        ylab0 <- 'Rainfall Depth (mm)'
        sub <- NULL
        theoretical <- FALSE
        title <- 'Seasonal maximum of daily rainfall'
    }
    if(varPICSA == "Total rain when RR>95thPerc"){
        xlab0 <- 'Year'
        ylab0 <- 'Rainfall Amount (mm)'
        sub <- NULL
        theoretical <- FALSE
        title <- 'Seasonal total of precipitation when RR > 95th percentile'
    }
    if(varPICSA == "Nb of day when RR>95thPerc"){
        xlab0 <- 'Year'
        ylab0 <- 'Number of Days'
        sub <- NULL
        theoretical <- FALSE
        title <- 'Seasonal count of days when RR > 95th percentile'
    }

    #########
    GRAPHTYPE <- str_trim(tclvalue(.cdtData$EnvData$plot.maps$typeTSp))
    origindate <- if(varPICSA %in% c("Onset", "Cessation")) as.character(.cdtData$EnvData$output$start.date[1]) else NULL

    #### ENSO
    if(GRAPHTYPE %in% c("ENSO-Line", "ENSO-Barplot", "ENSO-Proba")){
        if(.cdtData$EnvData$output$data.type == "cdtstation"){
            onset <- readRDS(file.path(.cdtData$EnvData$PathPicsa, "CDTDATASET", "Onset_days.rds"))
            onset <- as.numeric(onset[, ixy])
            cessat <- readRDS(file.path(.cdtData$EnvData$PathPicsa, "CDTDATASET", "Cessation_days.rds"))
            cessat <- as.numeric(cessat[, ixy])
        }else{
            onset <- readCdtDatasetChunk.locations(ixy, cdtdataset$fileInfo, cdtdataset, chunkDir = "Onset_days", do.par = FALSE)
            onset <- as.numeric(onset$data[.cdtData$EnvData$daily.precip$dateInfo$index, 1])
            cessat <- readCdtDatasetChunk.locations(ixy, cdtdataset$fileInfo, cdtdataset, chunkDir = "Cessation_days", do.par = FALSE)
            cessat <- as.numeric(cessat$data[.cdtData$EnvData$daily.precip$dateInfo$index, 1])
        }

        onset <- format(onset + .cdtData$EnvData$output$start.date, "%Y%m%d")
        cessat <- format(cessat + .cdtData$EnvData$output$start.date, "%Y%m%d")

        ijoni <- cdt.index.flexseason(onset, cessat, .cdtData$EnvData$ONI$date, "monthly")
        oni <- sapply(ijoni$index, function(x) mean(.cdtData$EnvData$ONI$data[x], na.rm = TRUE))
        oni[length(ijoni$nba) == 0] <- NA
        oni[is.nan(oni)] <- NA
        oni <- ifelse(oni >= 0.5, 3, ifelse(oni <= -0.5, 1, 2))
    }

    #########

    optsgph <- switch(GRAPHTYPE,
                "Line" = TSGraphOp$line,
                "Barplot" = TSGraphOp$bar,
                "ENSO-Line" = TSGraphOp$line.enso,
                "ENSO-Barplot" = TSGraphOp$bar.enso,
                "Anomaly" = TSGraphOp$anomaly,
                "Probability" = TSGraphOp$proba,
                "ENSO-Proba" = TSGraphOp$proba.enso)

    ## xlim, ylim, xlab, ylab
    if(GRAPHTYPE %in% c("Probability", "ENSO-Proba")){
        xlim <- range(don, na.rm = TRUE)
        if(optsgph$xlim$is.min) xlim[1] <- as.numeric(optsgph$xlim$min)
        if(optsgph$xlim$is.max) xlim[2] <- as.numeric(optsgph$xlim$max)
        ylim <- c(0, 100)
        xlab0 <- ""
        ylab0 <- "Probability of Exceeding"
    }else{
        xlim <- range(daty, na.rm = TRUE)
        if(optsgph$xlim$is.min) xlim[1] <- as.numeric(optsgph$xlim$min)
        if(optsgph$xlim$is.max) xlim[2] <- as.numeric(optsgph$xlim$max)
        idt <- daty >= xlim[1] & daty <= xlim[2]
        daty <- daty[idt]
        don <- don[idt]
        ylim <- range(pretty(don))
        if(GRAPHTYPE == "Anomaly")
            if(optsgph$anom$perc.anom) ylab0 <- "Anomaly (% of Mean)"
    }

    if(optsgph$ylim$is.min) ylim[1] <- optsgph$ylim$min
    if(optsgph$ylim$is.max) ylim[2] <- optsgph$ylim$max

    xlab <- if(optsgph$axislabs$is.xlab) optsgph$axislabs$xlab else xlab0
    if(optsgph$axislabs$is.ylab){
        ylab <- optsgph$axislabs$ylab
        sub <- NULL
    }else ylab <- ylab0

    ## title
    if(optsgph$title$is.title){
        titre <- optsgph$title$title
        titre.pos <- optsgph$title$position
    }else{
        titre <- title
        titre.pos <- "top"
    }

    #########

    if(GRAPHTYPE == "Line"){
        legends <- NULL
        if(optsgph$legend$is$mean){
            legends$add$mean <- optsgph$legend$add$mean
            legends$col$mean <- optsgph$legend$col$mean
            legends$text$mean <- optsgph$legend$text$mean
            legends$lwd$mean <- optsgph$legend$lwd$mean
        }else{
            if(tclvalue(.cdtData$EnvData$plot.maps$averageTSp) == "1") legends$add$mean <- TRUE
        }
        if(optsgph$legend$is$linear){
            legends$add$linear <- optsgph$legend$add$linear
            legends$col$linear <- optsgph$legend$col$linear
            legends$text$linear <- optsgph$legend$text$linear
            legends$lwd$linear <- optsgph$legend$lwd$linear
        }else{
            if(tclvalue(.cdtData$EnvData$plot.maps$trendTSp) == "1") legends$add$linear <- TRUE
        }
        if(optsgph$legend$is$tercile){
            legends$add$tercile <- optsgph$legend$add$tercile
            legends$col$tercile1 <- optsgph$legend$col$tercile1
            legends$text$tercile1 <- optsgph$legend$text$tercile1
            legends$col$tercile2 <- optsgph$legend$col$tercile2
            legends$text$tercile2 <- optsgph$legend$text$tercile2
            legends$lwd$tercile <- optsgph$legend$lwd$tercile
        }else{
            if(tclvalue(.cdtData$EnvData$plot.maps$tercileTSp) == "1") legends$add$tercile <- TRUE
        }

        ret <- graphs.plot.line(daty, don, xlim = xlim, ylim = ylim, origindate = origindate,
                        xlab = xlab, ylab = ylab, ylab.sub = sub,
                        title = titre, title.position = titre.pos, axis.font = 1,
                        plotl = optsgph$plot, legends = legends,
                        location = .cdtData$EnvData$location)
    }

    if(GRAPHTYPE == "Barplot"){
        ret <- graphs.plot.bar(daty, don, xlim = xlim, ylim = ylim, origindate = origindate,
                        xlab = xlab, ylab = ylab, ylab.sub = sub,
                        title = titre, title.position = titre.pos, axis.font = 1,
                        barcol = optsgph$colors$col,
                        location = .cdtData$EnvData$location)
    }

    if(GRAPHTYPE == "ENSO-Line"){
        oni <- oni[idt]

        legends <- NULL
        if(optsgph$legend$is$mean){
            legends$add$mean <- optsgph$legend$add$mean
            legends$col$mean <- optsgph$legend$col$mean
            legends$text$mean <- optsgph$legend$text$mean
            legends$lwd$mean <- optsgph$legend$lwd$mean
        }else{
            if(tclvalue(.cdtData$EnvData$plot.maps$averageTSp) == "1") legends$add$mean <- TRUE
        }
        if(optsgph$legend$is$linear){
            legends$add$linear <- optsgph$legend$add$linear
            legends$col$linear <- optsgph$legend$col$linear
            legends$text$linear <- optsgph$legend$text$linear
            legends$lwd$linear <- optsgph$legend$lwd$linear
        }else{
            if(tclvalue(.cdtData$EnvData$plot.maps$trendTSp) == "1") legends$add$linear <- TRUE
        }
        if(optsgph$legend$is$tercile){
            legends$add$tercile <- optsgph$legend$add$tercile
            legends$col$tercile1 <- optsgph$legend$col$tercile1
            legends$text$tercile1 <- optsgph$legend$text$tercile1
            legends$col$tercile2 <- optsgph$legend$col$tercile2
            legends$text$tercile2 <- optsgph$legend$text$tercile2
            legends$lwd$tercile <- optsgph$legend$lwd$tercile
        }else{
            if(tclvalue(.cdtData$EnvData$plot.maps$tercileTSp) == "1") legends$add$tercile <- TRUE
        }

        ret <- graphs.plot.line.ENSO(daty, don, oni, xlim = xlim, ylim = ylim, origindate = origindate,
                            xlab = xlab, ylab = ylab, ylab.sub = sub,
                            title = titre, title.position = titre.pos, axis.font = 1,
                            plotl = optsgph$plot, legends = legends,
                            location = .cdtData$EnvData$location)
    }

    if(GRAPHTYPE == "ENSO-Barplot"){
        oni <- oni[idt]

        ret <- graphs.plot.bar.ENSO(daty, don, oni, xlim = xlim, ylim = ylim, origindate = origindate,
                            xlab = xlab, ylab = ylab, ylab.sub = sub,
                            title = titre, title.position = titre.pos, axis.font = 1,
                            barcol = optsgph$colors$col, location = .cdtData$EnvData$location)
    }

    if(GRAPHTYPE == "Anomaly"){
        if(!optsgph$ylim$is.min & !optsgph$ylim$is.max) ylim <- NULL
        loko <- c(optsgph$colors$negative, optsgph$colors$positive)

        period <- range(daty, na.rm = TRUE)
        if(optsgph$anom$basePeriod){
            startYr <- optsgph$anom$startYr.anom
            endYr <- optsgph$anom$endYr.anom
            period <- c(startYr, endYr)
        }

        ret <- graphs.plot.bar.Anomaly(daty, don, period = period, percent = optsgph$anom$perc.anom,
                                xlim = xlim, ylim = ylim, xlab = xlab, ylab = ylab, ylab.sub = sub,
                                title = titre, title.position = titre.pos, axis.font = 1,
                                barcol = loko, location = .cdtData$EnvData$location)
    }

    if(GRAPHTYPE == "Probability"){
        if(theoretical) theoretical <- optsgph$proba$theoretical

        ret <- graphs.plot.proba(don, xlim = xlim, ylim = ylim, origindate = origindate,
                        xlab = xlab, xlab.sub = NULL, ylab = ylab,
                        title = titre, title.position = titre.pos, axis.font = 1,
                        proba = list(theoretical = theoretical),
                        plotp = optsgph$proba, plotl = optsgph$plot,
                        location = .cdtData$EnvData$location)
    }

    if(GRAPHTYPE == "ENSO-Proba"){
        ret <- graphs.plot.proba.ENSO(don, oni, xlim = xlim, ylim = ylim, origindate = origindate,
                                xlab = xlab, xlab.sub = NULL, ylab = ylab,
                                title = titre, title.position = titre.pos, axis.font = 1,
                                plotl = optsgph$plot, location = .cdtData$EnvData$location)
    }

    return(ret)
}

##############################################################################

SeasonAnalysis.Display.TSMaps <- function(){
    if(is.null(.cdtData$EnvData)) return(NULL)
    if(is.null(.cdtData$EnvData$output)) return(NULL)

    imgContainer <- CDT.Display.Map.inter(SeasonAnalysis.plot.TSMaps, .cdtData$EnvData$tab$TSMap, 'Aggregated-Data')
    .cdtData$EnvData$tab$TSMap <- imageNotebookTab_unik(imgContainer, .cdtData$EnvData$tab$TSMap)

    ###############
    tkbind(imgContainer[[2]], "<Button-1>", function(W, x, y){
        if(is.null(.cdtData$EnvData$plot.maps$data.type)) return(NULL)
        if(.cdtData$EnvData$plot.maps$data.type == "cdtstation"){
            xyid <- getIDLatLonCoords(W, x, y, imgContainer[[3]], getStnIDLabel,
                            stn.coords = .cdtData$EnvData$plot.maps[c('lon', 'lat', 'id')])
            if(xyid$plotTS)
                tclvalue(.cdtData$EnvData$plot.maps$stnIDTSp) <- xyid$crd
        }else{
            xyid <- getIDLatLonCoords(W, x, y, imgContainer[[3]], getPixelLatlon)
            if(xyid$plotTS){
                tclvalue(.cdtData$EnvData$plot.maps$lonLOC) <- xyid$crd$x
                tclvalue(.cdtData$EnvData$plot.maps$latLOC) <- xyid$crd$y
            }
        }

        if(xyid$plotTS){
            imgContainer1 <- CDT.Display.Graph(SeasonAnalysis.plot.TSGraph, .cdtData$EnvData$tab$Tsplot, 'Time-Series-Plot')
            .cdtData$EnvData$tab$Tsplot <- imageNotebookTab_unik(imgContainer1, .cdtData$EnvData$tab$Tsplot)
        }
    })
}

#######################################

SeasonAnalysis.Display.ClimMap <- function(){
    if(is.null(.cdtData$EnvData)) return(NULL)
    if(is.null(.cdtData$EnvData$output)) return(NULL)

    imgContainer <- CDT.Display.Map.inter(SeasonAnalysis.plot.ClimMaps, .cdtData$EnvData$tab$ClimMap, 'Clim-Analysis-Maps')
    .cdtData$EnvData$tab$ClimMap <- imageNotebookTab_unik(imgContainer, .cdtData$EnvData$tab$ClimMap)

    ###############
    tkbind(imgContainer[[2]], "<Button-1>", function(W, x, y){
        if(is.null(.cdtData$EnvData$plot.maps$data.type)) return(NULL)
        if(.cdtData$EnvData$plot.maps$data.type == "cdtstation"){
            xyid <- getIDLatLonCoords(W, x, y, imgContainer[[3]], getStnIDLabel,
                            stn.coords = .cdtData$EnvData$plot.maps[c('lon', 'lat', 'id')])
            if(xyid$plotTS)
                tclvalue(.cdtData$EnvData$plot.maps$stnIDTSp) <- xyid$crd
        }else{
            xyid <- getIDLatLonCoords(W, x, y, imgContainer[[3]], getPixelLatlon)
            if(xyid$plotTS){
                tclvalue(.cdtData$EnvData$plot.maps$lonLOC) <- xyid$crd$x
                tclvalue(.cdtData$EnvData$plot.maps$latLOC) <- xyid$crd$y
            }
        }

        if(xyid$plotTS){
            imgContainer1 <- CDT.Display.Graph(SeasonAnalysis.plot.TSGraph, .cdtData$EnvData$tab$Tsplot, 'Time-Series-Plot')
            .cdtData$EnvData$tab$Tsplot <- imageNotebookTab_unik(imgContainer1, .cdtData$EnvData$tab$Tsplot)
        }
    })
}
