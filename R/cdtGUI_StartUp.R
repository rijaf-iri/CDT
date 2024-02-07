
#' Start CDT GUI
#'
#' Starting CDT GUI from R console.
#' 
#' @param wd full path to the working directory.
#' @param lang the language to be used.
#' 
#' @examples
#' 
#' \dontrun{
#' library(CDT)
#' 
#' # Default working directory and English language
#' startCDT()
#' 
#' # Default working directory and Français language
#' startCDT(lang = "fr")
#' 
#' # Setting both working directory and language
#' startCDT("C:/Users/rijaf/Documents/AGHRYMET_ENACTS", "fr")
#' }
#' 
#' @export

startCDT <- function(wd = NA, lang = NA){
    cdt.file.conf <- file.path(.cdtDir$dirLocal, "config", "cdt_config.json")
    Config <- jsonlite::fromJSON(cdt.file.conf)
    Config <- rapply(Config, trimws, classes = "character", how = "replace")

    if(!is.na(wd)){
        wd <- trimws(wd)
        if(!dir.exists(wd)){
            warning(paste(wd, "does not found"), immediate. = TRUE)
            wd <- getwd()
        }
    }else{
        wd <- getwd()
        if(Config$wd != "" & dir.exists(wd)) wd <- Config$wd
    }
    setwd(wd)

    Config$wd <- wd
    if(!is.na(lang)) Config$lang.iso <- lang
    .cdtData$Config <- Config

    .cdtEnv$tcl$dir <- tempdir()
    .cdtEnv$tcl$GUI <- TRUE

    xml.global <- file.path(.cdtDir$dirLocal, "languages", "cdtGlobal_widgets.xml")
    .cdtEnv$tcl$lang$global <- cdtLanguageParse(xml.global, .cdtData$Config$lang.iso)

    ##################
    options(warn = -1)
    # options(warn = 0)

    cdtTclTk_styles()
    cdtMainWindow()
    cdtImagesZoom()
    cdtUserInformation()

    invisible()
}
