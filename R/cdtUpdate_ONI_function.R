
#' Update Oceanic Niño Index (ONI)
#'
#' Updating CDT's ONI data from cpc.ncep.noaa.gov.
#' 
#' @param GUI logical, if TRUE, display message on CDT (if CDT GUI is open), otherwise the message is displayed on R console.
#' 
#' @references \url{https://origin.cpc.ncep.noaa.gov/products/analysis_monitoring/ensostuff/ONI_v5.php}
#' 
#' @export

download.oni.cpc.ncep.noaa <- function(GUI = FALSE){
    cdtLocalConfigData()
    xml.dlg <- file.path(.cdtDir$dirLocal, "languages", "cdtUpdate_ONI_function.xml")
    lang.dlg <- cdtLanguageParse(xml.dlg, .cdtData$Config$lang.iso)

    if(!testConnection()){
        Insert.Messages.Out(lang.dlg[['message']][['1']], TRUE, 'e', GUI)
        return(NULL)
    }
    Insert.Messages.Out(lang.dlg[['message']][['2']], TRUE, 'i', GUI)

    url.oni <- "https://origin.cpc.ncep.noaa.gov/products/analysis_monitoring/ensostuff/ONI_v5.php"
    page.oni <- xml2::read_html(url.oni)
    tbl.oni <- rvest::html_nodes(page.oni, "table")
    tbl.oni <- rvest::html_table(tbl.oni[9], fill = TRUE)
    tbl.oni <- as.data.frame(tbl.oni[[1]])
    xhead <- as.character(tbl.oni[1, ])
    tbl.oni <- tbl.oni[!grepl('[^[:digit:]]', tbl.oni[, 1]), , drop = FALSE]
    names(tbl.oni) <- xhead
    tbl.oni <- apply(tbl.oni, 2, as.numeric)

    start <- paste0(tbl.oni[1, 1] - 1, "-", 12)
    end <- paste0(tbl.oni[nrow(tbl.oni), 1] + 1, "-", "01")
    tsmat <- expand.grid(sprintf("%02d", 1:12), tbl.oni[, 1])
    tsmat <- paste0(tsmat[, 2], "-", tsmat[, 1])
    ts1 <- c(start, tsmat[-length(tsmat)])
    ts2 <- c(tsmat[-1], end)
    oni <- c(t(tbl.oni[, -1]))
    ts.oni <- data.frame(start.month = ts1, end.month = ts2, ONI = oni, stringsAsFactors = FALSE)

    oni <- list(table = tbl.oni, ts = ts.oni)
    file.oni <- file.path(.cdtDir$Root, 'data', 'ONI_1950-present.rds')
    saveRDS(oni, file = file.oni)
    Insert.Messages.Out(lang.dlg[['message']][['3']], TRUE, 's', GUI)
    return(0)
}

#' Get seasonal Oceanic Niño Index (ONI)
#'
#' Get seasonal Oceanic Niño Index from ONI data already downloaded in CDT.
#' 
#' @param startMonth integer, start month of the season.
#' @param seasonLength integer, length of the season.
#' 
#' @references \url{https://origin.cpc.ncep.noaa.gov/products/analysis_monitoring/ensostuff/ONI_v5.php}
#' 
#' @return A named list
#' 
#' @export

oni.cpc.ncep.noaa <- function(startMonth, seasonLength){
    ONI <- readRDS(file.path(.cdtDir$Root, 'data', 'ONI_1950-present.rds'))
    oni_date <- as.Date(paste0(ONI$ts$start.month, "-01"))
    oni_date <- addMonths(oni_date, 1)

    if(seasonLength == 1){
        daty <- format(oni_date, "%Y-%m")
        daty <- paste0(daty, "_", daty)
        oni <- ONI$ts$ONI
    }else if(seasonLength == 2){
        daty <- format(oni_date, "%Y-%m")
        daty <- paste0(daty, "_", ONI$ts$end.month)
        oni <- ONI$ts$ONI
    }else if(seasonLength == 3){
        daty <- paste0(ONI$ts$start.month, "_", ONI$ts$end.month)
        oni <- ONI$ts$ONI
    }else{
        oni_date <- format(oni_date, "%Y%m")
        oni_index <- cdt.index.aggregate(oni_date, "monthly", "seasonal",
                                         seasonLength = seasonLength,
                                         startMonth = startMonth)
        oni <- sapply(oni_index$index, function(x) mean(ONI$ts$ONI[x], na.rm = TRUE))
        oni[is.nan(oni)] <- NA
        daty <- oni_index$date
        oni <- as.numeric(oni)
    }

    list(date = daty, oni = oni)
}

#' Download seasonal Oceanic Niño Index (ONI)
#'
#' The seasonal Oceanic Niño Index is computed then downloaded from IRI Data Library.
#' 
#' @param startYear integer, start year to download.
#' @param seasonLength integer, length of the season.
#' 
#' @references \url{http://iridl.ldeo.columbia.edu/SOURCES/.NOAA/.NCDC/.ERSST/.version5}
#'
#' @return A named list
#'  
#' @export

oni.iridl.ldeo <- function(startYear, seasonLength){
    url_sst <- "http://iridl.ldeo.columbia.edu/SOURCES/.NOAA/.NCDC/.ERSST/.version5/.sst"
    url_idx <- "zlev/removeGRID/X/-170/-120/RANGE/Y/-5/5/RANGEEDGES/dup/T/12.0/splitstreamgrid/dup/T2/(1856)/last/RANGE/T2/30.0/12.0/mul/runningAverage/T2/12.0/5.0/mul/STEP/%5BT2%5DregridLB/nip/T2/12/pad1/T/unsplitstreamgrid/sub/%7BY/cosd%7D%5BX/Y%5Dweighted-average"
    url_seas <- paste("T", seasonLength, "1.0/runningAverage", sep = "/")
    url_year <- paste0("T/(", startYear, ")/last/RANGE")
    url_json <- paste(url_sst, url_idx, url_seas, url_year, "info.json", sep = "/")

    enso_index <- jsonlite::fromJSON(url_json)
    enso_index <- enso_index[["iridl:values"]]
    seas_dates <- iridl.seasonal.dates(enso_index$T)
    seas_dates <- paste(seas_dates$start, seas_dates$end, sep = "_")
    list(date = seas_dates, oni = as.numeric(enso_index$sst))
}
