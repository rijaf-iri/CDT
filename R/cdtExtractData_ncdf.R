
#' Extract data from one netCDF file.
#'
#' Function to extract data over a set of points from one netCDF file.

#' @param ncdf_data named list, providing the input netCDF file to extract.
#' \itemize{
#' \item \code{file}: character, full path to the netCDF file.
#' \item \code{varid}: character, name of the variable to read from the netCDF data
#' \item \code{ilon}: integer, order for the longitude dimension of the variable. 
#' Example: if the variable "temp" has the dimension order [Lat, Lon] then \code{ilon} must be 2
#' \item \code{ilat}: integer, order for the latitude dimension of the variable.
#' }
#' @param crd_points named list, providing the coordinates over which the data will be extracted.
#' \itemize{
#' \item \code{type}: character, the type of file containing the coordinates of the points. Valid options: \code{"cdtStnData"}, \code{"cdtCrdFile"}.
#'  \itemize{
#'   \item \strong{"cdtStnData"}: the coordinates are from a CDT station data file
#'   \item \strong{"cdtCrdFile"}: the coordinates are from a CDT coordinates file
#'  }
#' \item \code{file}: character, full path to the file containing the coordinates
#' \item \code{sep}: character, column separator of the data
#' \item \code{na.strings}: character, missing values flag
#' }
#' 
#' @param out_file character, full path to the file in which the extracted data will be saved.
#' Default \code{NULL}, the extracted data will be returned and not saved to a file.
#' 
#' @return If out_file is \code{NULL}, returns a data.frame with columns \code{id}, \code{lon}, \code{lat} and \code{value}.
#' 
#' @export

extract_netcdf_file <- function(ncdf_data = list(file = "", varid = "z", ilon = 1, ilat = 2),
                                crd_points = list(type = 'cdtStnData', file = '', sep = ",", na.strings = "-99"),
                                out_file = NULL
                               )
{
    nc_pars <- list(file = "", varid = "z", ilon = 1, ilat = 2)
    ncdf_data <- init.default.list.args(ncdf_data, nc_pars)
    ncData <- get.ncData.value(ncdf_data)

    crd_type <- crd_points$type
    crd_pars <- list(file = "", sep = ",", na.strings = "-99", header = FALSE)
    crd_points <- init.default.list.args(crd_points, crd_pars)
    pars_read <- list(stringsAsFactors = FALSE, colClasses = "character")

    crds <- switch(crd_type,
                    'cdtStnData' = local({
                                crd_points$header <- FALSE
                                crdData <- do.call(utils::read.table, c(crd_points, pars_read))
                                crdData <- splitCDTData0(crdData, FALSE)
                                data.frame(id = crdData$id, lon = crdData$lon, lat = crdData$lat)
                            }),
                    'cdtCrdFile' = local({
                                crd_points$header <- TRUE
                                crdData <- do.call(utils::read.table, c(crd_points, pars_read))
                                data.frame(id = crdData[, 1], 
                                           lon = as.numeric(crdData[, 3]),
                                           lat = as.numeric(crdData[, 4]))
                            })
                   )

    ncgrid <- defSpatialPixels(ncData)
    crd_sp <- crds
    sp::coordinates(crd_sp) <- c('lon', 'lat')
    ijs <- sp::over(crd_sp, ncgrid)
    crds$value <- ncData$z[ijs]

    if(!is.null(out_file)){
        utils::write.table(crds, out_file, sep = ',', na = '',
                           col.names = TRUE, row.names = FALSE,
                           quote = FALSE)
        return(0)
    }else{
        return(crds)
    }
}

