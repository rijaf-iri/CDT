
#' Extract data from one netCDF file.
#'
#' Function to extract data over a set of points from one netCDF file.

#' @param ncdf_data named list, providing the input netCDF file to extract.
#' \itemize{
#' \item{\code{file}: }{character, full path to the netCDF file.}
#' \item{\code{varid}: }{character, name of the variable to read from the netCDF data}
#' \item{\code{ilon}: }{integer, order for the longitude dimension of the variable. 
#' Example: if the variable "temp" has the dimension order [Lat, Lon] then \code{ilon} must be 2}
#' \item{\code{ilat}: }{integer, order for the latitude dimension of the variable.}
#' }
#' @param crd_points named list, providing the coordinates over which the data will be extracted.
#' \itemize{
#' \item{\code{type}: }{character, the type of file containing the coordinates of the points. Valid options: \code{"cdtStnData"}, \code{"cdtCrdFile"}.
#'  \itemize{
#'   \item{\strong{"cdtStnData"}: }{the coordinates are from a CDT station data file}
#'   \item{\strong{"cdtCrdFile"}: }{the coordinates are from a CDT coordinates file}
#'  }
#' }
#' \item{\code{file}: }{character, full path to the file containing the coordinates}
#' \item{\code{sep}: }{character, column separator of the data}
#' \item{\code{na.strings}: }{character, missing values flag}
#' \item{\code{header}: }{logical, in the case of \code{"cdtCrdFile"}, \code{TRUE} if the data has a header}
#' }
#' 
#' @return Returns a data.frame, with the columns \code{id, lon, lat, value}.
#' 
#' @export

extract_netcdf_file <- function(ncdf_data = list(file = "", varid = "z", ilon = 1, ilat = 2),
                                crd_points = list(type = 'cdtStnData', file = '', sep = ",",
                                              na.strings = "-99", header = TRUE)
                                )
{
    nc_pars <- list(file = "", varid = "z", ilon = 1, ilat = 2)
    ncdf_data <- init.default.list.args(ncdf_data, nc_pars)

    ncData <- NULL
    nc <- ncdf4::nc_open(ncdf_data$file)
    ncData$lon <- nc$var[[ncdf_data$varid]]$dim[[ncdf_data$ilon]]$vals
    ncData$lat <- nc$var[[ncdf_data$varid]]$dim[[ncdf_data$ilat]]$vals
    ncData$z <- ncdf4::ncvar_get(nc, ncdf_data$varid)
    ncdf4::nc_close(nc)

    xo <- order(ncData$lon)
    ncData$lon <- ncData$lon[xo]
    yo <- order(ncData$lat)
    ncData$lat <- ncData$lat[yo]
    ncData$z <- if(ncdf_data$ilon < ncdf_data$ilat) ncData$z[xo, yo] else t(ncData$z)[xo, yo]

    crd_type <- crd_points$type
    crd_pars <- list(file = "", sep = ",", na.strings = "-99", header = TRUE)
    crd_points <- init.default.list.args(crd_points, crd_pars)
    pars_read <- list(stringsAsFactors = FALSE, colClasses = "character")

    crds <- switch(crd_type,
                    'cdtStnData' = local({
                                crd_points$header <- FALSE
                                crdData <- do.call(read.table, c(crd_points, pars_read))
                                crdData <- splitCDTData0(crdData, FALSE)
                                data.frame(id = crdData$id, lon = crdData$lon, lat = crdData$lat)
                            }),
                    'cdtCrdFile' = local({
                                crdData <- do.call(read.table, c(crd_points, pars_read))
                                data.frame(id = crdData[, 1], 
                                           lon = as.numeric(crdData[, 3]),
                                           lat = as.numeric(crdData[, 4]))
                            })
                   )

    ncgrid <- defSpatialPixels(ncData)
    crd_sp <- crds
    sp::coordinates(crd_sp) <- c('lon', 'lat')
    ijs <- over(crd_sp, ncgrid)
    crds$value <- ncData$z[ijs]

    return(crds)
}

