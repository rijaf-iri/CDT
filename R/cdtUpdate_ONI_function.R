
#' Update Oceanic Niño Index (ONI)
#'
#' Updating ONI data
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
    tbl.oni <- tbl.oni[[1]]
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
