
# http://www.fao.org/3/x0490e/x0490e07.htm
# http://www.fao.org/3/x0490e/x0490e08.htm

# Extraterrestrial.Radiation

#######

#' Compute the extraterrestrial radiation.
#'
#' Compute the extraterrestrial radiation throughout the year for different latitudes.
#' 
#' @param lat numeric, a vector of the latitude of the points where the extraterrestrial radiation will be calculated.
#' @param tstep character, the time step of the computed extraterrestrial radiation. Available options: \code{"daily"}, \code{"pentad"}, \code{"dekadal"}, \code{"monthly"}.
#' 
#' @references 
#' Allen, R.G., Pereira, L.S., Raes, D., and Smith, M. 1998. Crop evapotranspiration: Guidelines for computing crop requirements. Irrigation and Drainage PaperNo. 56, FAO, Rome, Italy 300 p.
#' 
#' @return Returns a matrix, where the column represent the latitude and the row the time of the year. The number of row for \code{"daily"} is 365, 72 for \code{"pentad"}, 36 for \code{"dekadal"} and 12 \code{"monthly"}.
#' 
#' @export

extraterrestrial_radiation_annual <- function(lat, tstep = "daily"){
    phi <- pi * (lat) / 180
    dates <- seq(as.Date("2001-1-1"), as.Date("2001-12-31"), 'day')

    J <- as.numeric(strftime(dates, format = "%j"))
    fJ <- 2 * pi * J / 365
    dr <- 1 + 0.033 * cos(fJ)
    delta <- 0.409 * sin(fJ - 1.39)

    ws <- matrix(NA, nrow = 365, ncol = length(lat))
    sin2 <- cos2 <- ws
    for(j in seq_along(lat)){
        ws[, j] <- acos(-tan(phi[j]) * tan(delta))
        sin2[, j] <- sin(phi[j]) * sin(delta)
        cos2[, j] <- cos(phi[j]) * cos(delta)
    } 
    Ra <- (37.58603 * dr) * (ws * sin2 + cos2 * sin(ws))
    Ra[Ra < 0] <- 0
    if(tstep == "pentad"){
        irow <- as.numeric(format(dates, "%d")) %in% c(3, 8, 13, 18, 23, 28)
        Ra <- Ra[irow, , drop = FALSE]
    }
    if(tstep == "dekadal"){
        irow <- as.numeric(format(dates, "%d")) %in% c(5, 15, 25)
        Ra <- Ra[irow, , drop = FALSE]
    }
    if(tstep == "monthly"){
        irow <- as.numeric(format(dates, "%d")) == 15
        Ra <- Ra[irow, , drop = FALSE]
    }
    # Ra in MJ m-2 d-1
    return(Ra)
}

#' Compute the extraterrestrial radiation for daily data.
#'
#' Compute the extraterrestrial radiation of daily data for different latitudes.
#' 
#' @param lat numeric, a vector of the latitude of the points where the extraterrestrial radiation will be calculated.
#' @param dates character, a vector of dates in the form of \code{YYYYMMDD}.
#' 
#' @references 
#' Allen, R.G., Pereira, L.S., Raes, D., and Smith, M. 1998. Crop evapotranspiration: Guidelines for computing crop requirements. Irrigation and Drainage PaperNo. 56, FAO, Rome, Italy 300 p.
#' 
#' @return Returns a matrix, where the column represent the latitude and the row the date.
#' 
#' @export

extraterrestrial_radiation_daily <- function(lat, dates){
    phi <- pi * (lat) / 180
    dates <- as.Date(dates, '%Y%m%d')
    J <- as.numeric(strftime(dates, format = "%j"))
    fJ <- 2 * pi * J / 365
    dr <- 1 + 0.033 * cos(fJ)
    delta <- 0.409 * sin(fJ - 1.39)

    ws <- matrix(NA, nrow = length(dates), ncol = length(lat))
    sin2 <- cos2 <- ws
    for(j in seq_along(lat)){
        ws[, j] <- acos(-tan(phi[j]) * tan(delta))
        sin2[, j] <- sin(phi[j]) * sin(delta)
        cos2[, j] <- cos(phi[j]) * cos(delta)
    } 

    Ra <- (37.58603 * dr) * (ws * sin2 + cos2 * sin(ws))
    Ra[Ra < 0] <- 0

    # Ra in MJ m-2 d-1
    # convert to W m-2
    Ra <- 11.6 * Ra

    return(Ra)
}

#############################

#' Compute the daylight hours.
#'
#' Compute the daylight hours for different latitudes.
#' 
#' @param lat numeric, a vector of the latitude of the points where the daylight hours will be calculated.
#' @param dates character, a vector of dates in the form of \code{YYYYMMDD}.
#' 
#' @references 
#' Allen, R.G., Pereira, L.S., Raes, D., and Smith, M. 1998. Crop evapotranspiration: Guidelines for computing crop requirements. Irrigation and Drainage PaperNo. 56, FAO, Rome, Italy 300 p.
#' 
#' @return Returns a matrix, where the column represent the latitude and the row the date.
#' 
#' @export

daylight_hours <- function(lat, dates){
    phi <- pi * (lat) / 180
    dates <- as.Date(dates, '%Y%m%d')
    J <- as.numeric(strftime(dates, format = "%j"))
    fJ <- 2 * pi * J / 365
    dr <- 1 + 0.033 * cos(fJ)
    delta <- 0.409 * sin(fJ - 1.39)

    ws <- matrix(NA, nrow = length(dates), ncol = length(lat))

    for(j in seq_along(lat)){
        ws[, j] <- acos(-tan(phi[j]) * tan(delta))
    } 

    N <- 24 * ws / pi

    ## N in hours
    return(N)
}
