
#' Saturation vapour pressure.
#'
#' Compute the saturation vapour pressure.
#' 
#' @param tmp numeric, a vector or matrix of the temperature data in degree Celsius.
#' 
#' @return Returns a vector or matrix, the units are in hPa.
#' 
#' @export

saturation_vapour_pressure <- function(tmp){
    # tmp: temperature in degC
    # es in mb or hPa
    6.112 * exp((17.67 * tmp)/(tmp + 243.5))
}

#' Relative humidity.
#'
#' Compute the relative humidity from the air temperature and dewpoint temperature.
#' 
#' @param tm numeric, a vector or matrix of the air temperature in degree Celsius.
#' @param td numeric, a vector or matrix of the dewpoint temperature in degree Celsius.
#' 
#' @return Returns a vector or matrix, the units are in percentage.
#' 
#' @export

relative_humidity <- function(tm, td){
    # tm: temperature in degC
    # td: dewpoint in degC
    # relative humidity in %
    es <- saturation_vapour_pressure(tm)
    ea <- saturation_vapour_pressure(td)
    100 * ea/es
}

#' Specific humidity.
#'
#' Compute the specific humidity from the dewpoint temperature and surface pressure.
#' 
#' @param td numeric, a vector or matrix of the dewpoint temperature in degree Celsius.
#' @param pr numeric, a vector or matrix of the surface pressure in hPa.
#' 
#' @return Returns a vector or matrix, the units are in kg/kg.
#' 
#' @export

specific_humidity <- function(td, pr){
    # td: dewpoint in degC
    # pr: surface pressure in mb or hPa
    # specific humidity in kg/kg
    ea <- saturation_vapour_pressure(td)
    (0.622 * ea)/(pr - (0.378 * ea))
}

#' Dewpoint temperature.
#'
#' Compute the dewpoint temperature from the air temperature and relative humidity.
#' 
#' @param tm numeric, a vector or matrix of the air temperature in degree Celsius.
#' @param rh numeric, a vector or matrix of the relative humidity in percentage.
#' 
#' @return Returns a vector or matrix, the units are in degree Celsius.
#' 
#' @export

dewpoint_temperature <- function(tm, rh){
    # tm: temperature in degC
    # rh: relative humidity in %
    # dewpoint in degC
    es <- saturation_vapour_pressure(tm)
    ea <- es * (rh/100)
    (log(ea/6.112) * 243.5)/(17.67 - log(ea/6.112))
}
