
legendLabel.SeasonAnalysis <- function(lab.breaks, varSeas, donDate, start.date)
{
    legendLabel <- lab.breaks

    if(varSeas %in% c("onset", "cessation")){
        .start.date <- format(start.date, '%Y%m%d')
        start.dateYear <- format(start.date, '%Y')
        odaty <- .start.date[start.dateYear == donDate]
        odaty <- as.character(as.Date(odaty, '%Y%m%d'))
        legendLabel <- format(as.Date(lab.breaks, origin = odaty), '%d-%b')
    }

    return(legendLabel)
}

legendLabel.SeasonAnalysis1 <- function(lab.breaks, varSeas, statClim, start.date)
{
    legendLabel <- lab.breaks

    if(varSeas %in% c("onset", "cessation") &
       statClim %in% c("mean", "med", "perc"))
    {
        odaty <- format(start.date[1], '%Y-%m-%d')
        legendLabel <- format(as.Date(lab.breaks, origin = odaty), '%d-%b')
    }

    return(legendLabel)
}

#######################################

SeasonAnalysis.plot.TSMaps <- function(){
    TSMapOp <- .cdtData$EnvData$TSMapOp
    don <- .cdtData$EnvData$tsMap

    if(!TSMapOp$title$user){
        .titre <- switch(.cdtData$EnvData$plotVar$varPICSA,
                         "onset" = "Starting dates of the rainy season",
                         "cessation" = "Ending dates of the rainy season",
                         "lengthSeas" = "Length of the rainy season",
                         "totrainSeas" = "Seasonal rainfall amounts",
                         "longdryspell" = "Longest dry spell",
                         "nbrainSeas" = "Seasonal number of rainy days",
                         "max24hrain" = 'Seasonal maximum of daily rainfall',
                         "totrain95P" = 'Seasonal total of precipitation when RR > 95th percentile',
                         "nbrain95P" = 'Seasonal count of days when RR > 95th percentile',
                         "dryspell" = {
                                         drydef <- .cdtData$EnvData$plotVar$dryspell
                                         paste0("Dry spells - ", drydef, " or more consecutive days")
                                      }
                        )
        .titre <- paste(.titre, '[', .cdtData$EnvData$plotVar$yearseas, ']')
    }else .titre <- TSMapOp$title$title

    #################

    legend.texta <- switch(.cdtData$EnvData$plotVar$varPICSA,
                        "onset" = NULL,
                        "cessation" = NULL,
                        "lengthSeas" = 'Number of Days',
                        "totrainSeas" = 'Rainfall Amount (mm)',
                        "longdryspell" = 'Number of Days',
                        "nbrainSeas" = 'Number of Days',
                        "max24hrain" = 'Rainfall Depth (mm)',
                        "totrain95P" = 'Rainfall Amount (mm)',
                        "nbrain95P" = 'Number of Days',
                        "dryspell" = 'Number of Dry Spells')

    #################

    .data.type <- .cdtData$EnvData$plot.maps$.data.type
    map.args <- cdt.plotmap.args(don, TSMapOp, .cdtData$EnvData$shapefile,
                                 legend.text = legend.texta,
                                 label.fun = legendLabel.SeasonAnalysis,
                                 varSeas = .cdtData$EnvData$plotVar$varPICSA,
                                 donDate = .cdtData$EnvData$plotVar$yearseas,
                                 start.date = .cdtData$EnvData$output$start.date
                                )

    opar <- graphics::par(mar = map.args$mar)
    map.args.add <- list(titre = .titre, data.type = .data.type)

    map.args <- map.args[!(names(map.args) %in% "mar")]
    map.args <- c(map.args, map.args.add)
    par.plot <- do.call(cdt.plotmap.fun, map.args)

    ## scale bar
    cdt.plotmap.scalebar(TSMapOp$scalebar)

    graphics::par(opar)

    return(par.plot)
}

#######################################

SeasonAnalysis.plot.ClimMaps <- function(){
    don <- .cdtData$EnvData$climdata
    climMapOp <- .cdtData$EnvData$climMapOp
    StatOp <- .cdtData$EnvData$analysis$method

    ## titre
    if(!climMapOp$title$user){
        .titre <- switch(.cdtData$EnvData$plotVar$varPICSA,
                        "onset" = "Starting dates of the rainy season",
                        "cessation" = "Ending dates of the rainy season",
                        "lengthSeas" = "Length of the rainy season",
                        "totrainSeas" = "Seasonal rainfall amounts",
                        "longdryspell" = "Longest dry spell",
                        "nbrainSeas" = "Seasonal number of rainy days",
                        "max24hrain" = 'Seasonal maximum of daily rainfall',
                        "totrain95P" = 'Seasonal total of precipitation when RR > 95th percentile',
                        "nbrain95P" = 'Seasonal count of days when RR > 95th percentile',
                        "dryspell" = {
                                drydef <- .cdtData$EnvData$plotVar$dryspell
                                paste0("Dry spells - ", drydef, " or more consecutive days")
                            })
    }else .titre <- climMapOp$title$title

    #################

    if(!climMapOp$colkeyLab$user){
        start.dateYear <- as.numeric(format(.cdtData$EnvData$output$start.date, '%Y'))
        utrnd <- (diff(range(start.dateYear, na.rm = TRUE))+1)
        uu <- TRUE
        if(.cdtData$EnvData$analysis$trend == 'trendEY') utrnd <- "/ year"
        if(.cdtData$EnvData$analysis$trend == 'trendOP') utrnd <- paste("over", utrnd, "years")
        if(.cdtData$EnvData$analysis$trend == 'trendAP'){
             utrnd <- "change / average (in %)"
             uu <- FALSE
        }
        dryUn <- "Number of Dry Spells"

        legUnit <- switch(.cdtData$EnvData$plotVar$varPICSA,
                        "onset" = list(NULL, NULL, "days", NULL, "count",
                                       if(uu) paste("days", utrnd) else utrnd),
                        "cessation" = list(NULL, NULL, "days", NULL, "count",
                                           if(uu) paste("days", utrnd) else utrnd),
                        "lengthSeas" = list("days", "days", "days", "days", "count",
                                            if(uu) paste("days", utrnd) else utrnd),
                        "totrainSeas" = list("mm", "mm", "mm", "mm", "count",
                                             if(uu) paste("mm", utrnd) else utrnd),
                        "longdryspell" = list("days", "days", "days", "days", "count",
                                              if(uu) paste("days", utrnd) else utrnd),
                        "nbrainSeas" = list("days", "days", "days", "days", "count",
                                            if(uu) paste("days", utrnd) else utrnd),
                        "max24hrain" = list("mm", "mm", "mm", "mm", "count",
                                            if(uu) paste("mm", utrnd) else utrnd),
                        "totrain95P" = list("mm", "mm", "mm", "mm", "count",
                                            if(uu) paste("mm", utrnd) else utrnd),
                        "nbrain95P" = list("days", "days", "days", "days", "count",
                                           if(uu) paste("days", utrnd) else utrnd),
                        "dryspell" = list(dryUn, dryUn, dryUn, dryUn, "count",
                                          if(uu) paste(dryUn, utrnd) else utrnd)
                    )

        StatVal <- c('mean', 'med', 'std', 'perc', 'freq', 'trend')
        units <- legUnit[[which(StatVal == StatOp)]]
        units <- if(!is.null(units)) paste0("(Units: ", units, ")") else ""
        legend.texta <- paste(StatOp, units)
    }else legend.texta <- climMapOp$colkeyLab$label


    #################

    .data.type <- .cdtData$EnvData$plot.maps$.data.type
    map.args <- cdt.plotmap.args(don, climMapOp, .cdtData$EnvData$shapefile,
                                 legend.text = legend.texta,
                                 label.fun = legendLabel.SeasonAnalysis1,
                                 varSeas = .cdtData$EnvData$plotVar$varPICSA,
                                 statClim = StatOp,
                                 start.date = .cdtData$EnvData$output$start.date
                            )

    opar <- graphics::par(mar = map.args$mar)
    map.args.add <- list(titre = .titre, data.type = .data.type)

    map.args <- map.args[!(names(map.args) %in% "mar")]
    map.args <- c(map.args, map.args.add)
    par.plot <- do.call(cdt.plotmap.fun, map.args)

    ## scale bar
    cdt.plotmap.scalebar(climMapOp$scalebar)

    graphics::par(opar)

    return(par.plot)
}

#######################################

SeasonAnalysis.plot.TSGraph <- function(){
    TSGraphOp <- .cdtData$EnvData$TSGraphOp
    dryspl <- .cdtData$EnvData$plot.maps$dryspell
    varPICSA <- .cdtData$EnvData$plot.maps$varTSp
    data_type <- .cdtData$EnvData$output$data.type
    cdtParallelCond <- .cdtData$Config$parallel

    long_dryspell <- if(data_type == "cdtstation") "Longest_dry_spell" else "Dry_Spells"
    tsdata.dir <- switch(varPICSA,
                         'onset' = "Onset_days",
                         'cessation' = "Cessation_days",
                         'lengthSeas' = "Season_length",
                         'totrainSeas' = "Seasonal_rain_amount",
                         'nbrainSeas' = "Number_rainy_day",
                         'max24hrain' = "Maximum_rain_daily",
                         'totrain95P' = "Total_rain_above_Perc95th",
                         'nbrain95P' = "Number_day_above_Perc95th",
                         'longdryspell' = long_dryspell,
                         'dryspell' = "Dry_Spells")

    if(data_type == "cdtstation"){
        stnID <- trimws(tclvalue(.cdtData$EnvData$plot.maps$stnIDTSp))
        ixy <- which(.cdtData$EnvData$output$data$id == stnID)
        if(length(ixy) == 0){
            Insert.Messages.Out(.cdtData$EnvData$message[['23']], TRUE, 'e')
            return(NULL)
        }

        if(varPICSA != "raints"){
            tsdata.path <- file.path(.cdtData$EnvData$PathPicsa, "CDTDATASET")
            filetsdata <- file.path(tsdata.path, paste0(tsdata.dir, ".rds"))
            if(!file.exists(filetsdata)){
                Insert.Messages.Out(paste(filetsdata, .cdtData$EnvData$message[['5']]), TRUE, 'e')
                return(NULL)
            }
            don <- readRDS(filetsdata)
            don <- don[, ixy]
            if(varPICSA == "dryspell"){
                nval <- sapply(don, function(x) (length(x) == 1) & is.na(x[1]))
                don <- sapply(don, count_dryspell_number, ds = dryspl)
                don[nval] <- NA
            }else don <- as.numeric(don)

            dates <- format(.cdtData$EnvData$output$start.date, '%Y%m%d')
            daty <- as.numeric(substr(dates, 1, 4))
        }else{
            don <- .cdtData$EnvData$daily.precip[, ixy]
            dates <- .cdtData$EnvData$output$data$date
        }

        .cdtData$EnvData$location <- paste0("Station: ", .cdtData$EnvData$output$data$id[ixy])
    }else{
        cdtdataset <- .cdtData$EnvData$cdtdataset
        xlon <- cdtdataset$coords$mat$x
        xlat <- cdtdataset$coords$mat$y
        ilon <- as.numeric(trimws(tclvalue(.cdtData$EnvData$plot.maps$lonLOC)))
        ilat <- as.numeric(trimws(tclvalue(.cdtData$EnvData$plot.maps$latLOC)))

        iclo <- findInterval(ilon, xlon)
        ilo <- iclo + (2 * ilon > xlon[iclo] + xlon[iclo + 1])
        icla <- findInterval(ilat, xlat)
        ila <- icla + (2 * ilat > xlat[icla] + xlat[icla + 1])

        if(is.na(ilo) | is.na(ila)){
            Insert.Messages.Out(.cdtData$EnvData$message[['24']], TRUE, 'e')
            return(NULL)
        }
        ixy <- ilo + length(xlon) * (ila - 1)

        if(varPICSA != "raints"){
            don <- readCdtDatasetChunk.locations(ixy, cdtdataset$fileInfo, cdtdataset, chunkDir = tsdata.dir, parllCond = cdtParallelCond, do.par = FALSE)

            if(varPICSA == "dryspell"){
                nval <- sapply(don$data[, 1], function(x) (length(x) == 1) & is.na(x[1]))
                don <- sapply(don$data[, 1], count_dryspell_number, ds = dryspl)
                don[nval] <- NA
            }else if(varPICSA == "longdryspell"){
                don <- sapply(don$data[, 1], max, na.rm = TRUE)
                don[is.infinite(don)] <- NA
            }else don <- as.numeric(don$data[, 1])

            dates <- cdtdataset$dateInfo$date
            daty <- as.numeric(substr(dates, 1, 4))
        }else{
            don <- readCdtDatasetChunk.locations(ixy, .cdtData$EnvData$output$daily.precip, .cdtData$EnvData$daily.precip, parllCond = cdtParallelCond, do.par = FALSE)
            don <- as.numeric(don$data[.cdtData$EnvData$daily.precip$dateInfo$index, 1])
            dates <- .cdtData$EnvData$daily.precip$dateInfo$date
        }

        .cdtData$EnvData$location <- paste0("Longitude: ", round(ilon, 5), ", Latitude: ", round(ilat, 5))
    }

    ######### plot yearly rain

    if(varPICSA == "raints"){
        ret <- picsa.plot.daily(dates, don, .cdtData$EnvData$location, .cdtData$EnvData$output$params$dryday)
        return(ret)
    }

    ######### plot other params

    if(varPICSA == "onset"){
        xlab0 <- ''
        ylab0 <- ''
        sub <- NULL
        theoretical <- TRUE
        title <- "Starting dates of the rainy season"
    }
    if(varPICSA == "cessation"){
        xlab0 <- ''
        ylab0 <- ''
        sub <- NULL
        theoretical <- TRUE
        title <- "Ending dates of the rainy season"
    }
    if(varPICSA == 'lengthSeas'){
        xlab0 <- 'Year'
        ylab0 <- 'Number of Days'
        sub <- NULL
        theoretical <- TRUE
        title <- "Length of the rainy season"
    }
    if(varPICSA == 'totrainSeas'){
        xlab0 <- 'Year'
        ylab0 <- 'Rainfall Amount (mm)'
        sub <- NULL
        theoretical <- TRUE
        title <- "Seasonal rainfall amounts"
    }
    if(varPICSA == 'dryspell'){
        xlab0 <- 'Year'
        ylab0 <- 'Number of Dry Spells'
        sub <- paste("Dry spells -", dryspl, "or more consecutive days")
        theoretical <- FALSE
        title <- "Dry Spells"
    }
    if(varPICSA == 'longdryspell'){
        xlab0 <- 'Year'
        ylab0 <- 'Number of Days'
        sub <- NULL
        theoretical <- FALSE
        title <- "Longest dry spell"
    }
    if(varPICSA == 'nbrainSeas'){
        xlab0 <- 'Year'
        ylab0 <- 'Number of Days'
        sub <- NULL
        theoretical <- TRUE
        title <- "Seasonal number of rainy days"
    }
    if(varPICSA == 'max24hrain'){
        xlab0 <- 'Year'
        ylab0 <- 'Rainfall Depth (mm)'
        sub <- NULL
        theoretical <- FALSE
        title <- 'Seasonal maximum of daily rainfall'
    }
    if(varPICSA == 'totrain95P'){
        xlab0 <- 'Year'
        ylab0 <- 'Rainfall Amount (mm)'
        sub <- NULL
        theoretical <- FALSE
        title <- 'Seasonal total of precipitation when RR > 95th percentile'
    }
    if(varPICSA == 'nbrain95P'){
        xlab0 <- 'Year'
        ylab0 <- 'Number of Days'
        sub <- NULL
        theoretical <- FALSE
        title <- 'Seasonal count of days when RR > 95th percentile'
    }

    #########
    GRAPHTYPE <- .cdtData$EnvData$plot.maps$typeTSp
    origindate <- if(varPICSA %in% c("onset", "cessation")) as.character(.cdtData$EnvData$output$start.date[1]) else NULL

    #### ENSO
    if(GRAPHTYPE %in% c("eline", "ebar", "eproba")){
        if(data_type == "cdtstation"){
            onset <- readRDS(file.path(.cdtData$EnvData$PathPicsa, "CDTDATASET", "Onset_days.rds"))
            onset <- as.numeric(onset[, ixy])
            cessat <- readRDS(file.path(.cdtData$EnvData$PathPicsa, "CDTDATASET", "Cessation_days.rds"))
            cessat <- as.numeric(cessat[, ixy])
        }else{
            onset <- readCdtDatasetChunk.locations(ixy, cdtdataset$fileInfo, cdtdataset, chunkDir = "Onset_days", parllCond = cdtParallelCond, do.par = FALSE)
            onset <- as.numeric(onset$data[.cdtData$EnvData$cdtdataset$dateInfo$index, 1])
            cessat <- readCdtDatasetChunk.locations(ixy, cdtdataset$fileInfo, cdtdataset, chunkDir = "Cessation_days", parllCond = cdtParallelCond, do.par = FALSE)
            cessat <- as.numeric(cessat$data[.cdtData$EnvData$cdtdataset$dateInfo$index, 1])
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
                      "line" = TSGraphOp$line,
                      "bar" = TSGraphOp$bar,
                      "eline" = TSGraphOp$line.enso,
                      "ebar" = TSGraphOp$bar.enso,
                      "anom" = TSGraphOp$anomaly,
                      "proba" = TSGraphOp$proba,
                      "eproba" = TSGraphOp$proba.enso
                  )

    ## xlim, ylim, xlab, ylab
    if(GRAPHTYPE %in% c("proba", "eproba")){
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
        if(GRAPHTYPE == "anom")
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

    if(GRAPHTYPE %in% c("line", "eline")){
        legends <- NULL
        nmp <- c("add", "lwd", "col", "text")
        if(optsgph$legend$is$mean){
            legends <- append.list(legends, lapply(optsgph$legend[nmp], '[', 'mean'))
        }else{
            if(tclvalue(.cdtData$EnvData$plot.maps$averageTSp) == "1") legends$add$mean <- TRUE
        }
        if(optsgph$legend$is$linear){
            legends <- append.list(legends, lapply(optsgph$legend[nmp], '[', 'linear'))
        }else{
            if(tclvalue(.cdtData$EnvData$plot.maps$trendTSp) == "1") legends$add$linear <- TRUE
        }
        if(optsgph$legend$is$tercile){
            legends <- append.list(legends, lapply(optsgph$legend[nmp[1:2]], '[', 'tercile'))
            legends <- append.list(legends, lapply(optsgph$legend[nmp[3:4]], '[', 'tercile1'))
            legends <- append.list(legends, lapply(optsgph$legend[nmp[3:4]], '[', 'tercile2'))
        }else{
            if(tclvalue(.cdtData$EnvData$plot.maps$tercileTSp) == "1") legends$add$tercile <- TRUE
        }
    }

    #########

    if(GRAPHTYPE == "line"){
        ret <- graphs.plot.line(daty, don, xlim = xlim, ylim = ylim, origindate = origindate,
                        xlab = xlab, ylab = ylab, ylab.sub = sub,
                        title = titre, title.position = titre.pos, axis.font = 1,
                        plotl = optsgph$plot, legends = legends,
                        location = .cdtData$EnvData$location)
    }

    if(GRAPHTYPE == "bar"){
        ret <- graphs.plot.bar(daty, don, xlim = xlim, ylim = ylim, origindate = origindate,
                        xlab = xlab, ylab = ylab, ylab.sub = sub,
                        title = titre, title.position = titre.pos, axis.font = 1,
                        barcol = optsgph$colors$col,
                        location = .cdtData$EnvData$location)
    }

    if(GRAPHTYPE == "eline"){
        oni <- oni[idt]
        ret <- graphs.plot.line.ENSO(daty, don, oni, xlim = xlim, ylim = ylim, origindate = origindate,
                            xlab = xlab, ylab = ylab, ylab.sub = sub,
                            title = titre, title.position = titre.pos, axis.font = 1,
                            plotl = optsgph$plot, legends = legends,
                            location = .cdtData$EnvData$location)
    }

    if(GRAPHTYPE == "ebar"){
        oni <- oni[idt]
        ret <- graphs.plot.bar.ENSO(daty, don, oni, xlim = xlim, ylim = ylim, origindate = origindate,
                            xlab = xlab, ylab = ylab, ylab.sub = sub,
                            title = titre, title.position = titre.pos, axis.font = 1,
                            barcol = optsgph$colors$col, location = .cdtData$EnvData$location)
    }

    if(GRAPHTYPE == "anom"){
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

    if(GRAPHTYPE == "proba"){
        if(!theoretical){
            if(optsgph$proba$theoretical){
                msg <- paste0('Theoretical probability function for "', title, '" will not be fitted')
                Insert.Messages.Out(msg, TRUE, "w")
            }
            optsgph$proba$theoretical <- FALSE
        }

        if(varPICSA %in% c("onset", "cessation")){
            don[don <= 0] <- 0.01
        }

        ret <- graphs.plot.proba(don, xlim = xlim, ylim = ylim, origindate = origindate,
                        xlab = xlab, xlab.sub = NULL, ylab = ylab,
                        title = titre, title.position = titre.pos, axis.font = 1,
                        proba = optsgph$proba, plotl = optsgph$plot,
                        location = .cdtData$EnvData$location)
    }

    if(GRAPHTYPE == "eproba"){
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
