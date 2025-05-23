% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/cdtClimate_ET0_FAO_functions.R
\name{ET0_Penman_Monteith_FAO_netcdf}
\alias{ET0_Penman_Monteith_FAO_netcdf}
\title{Compute daily reference evapotranspiration for a netCDF dataset.}
\usage{
ET0_Penman_Monteith_FAO_netcdf(
  start.date = "1991-01-01",
  end.date = "2020-12-31",
  tmax.data = list(dir = "", format = "tmax_\%s\%s\%s.nc", varid = "tmax", ilon = 1, ilat
    = 2),
  tmin.data = list(dir = "", format = "tmin_\%s\%s\%s.nc", varid = "tmin", ilon = 1, ilat
    = 2),
  rh.from = "minmax",
  rhmax.data = list(dir = "", format = "rhmax_\%s\%s\%s.nc", varid = "rhmax", ilon = 1,
    ilat = 2),
  rhmin.data = list(dir = "", format = "rhmin_\%s\%s\%s.nc", varid = "rhmin", ilon = 1,
    ilat = 2),
  rhmean.data = list(dir = "", format = "rhmean_\%s\%s\%s.nc", varid = "rhmean", ilon =
    1, ilat = 2),
  wff10.data = list(dir = "", format = "wff_\%s\%s\%s.nc", varid = "wsp", ilon = 1, ilat
    = 2),
  pres.data = list(dir = "", format = "pres_\%s\%s\%s.nc", varid = "pres", ilon = 1, ilat
    = 2),
  rad.data = list(dir = "", format = "rad_\%s\%s\%s.nc", varid = "rad", ilon = 1, ilat =
    2),
  elev.data = list(file = "", varid = "z", ilon = 1, ilat = 2),
  output = list(dir = "", format = "et0_\%S.nc")
)
}
\arguments{
\item{start.date}{character, the start date of the data to be calculated in the form \code{YYYY-MM-DD}.}

\item{end.date}{character, the end date of the data to be calculated in the form \code{YYYY-MM-DD}.}

\item{tmax.data}{named list, providing the maximum temperature netCDF dataset. Units: degree Celsius.
\itemize{
\item \code{dir}: character, full path to the directory containing the netCDF files.
\item \code{format}: character, format of the netCDF file names
\item \code{varid}: character, name of the variable to read from the netCDF data
\item \code{ilon}: integer, order for the longitude dimension of the variable. 
Example: if the variable "pres" has the dimension order [Lat, Lon] then \code{ilon} must be 2
\item \code{ilat}: integer, order for the latitude dimension of the variable.
}}

\item{tmin.data}{named list, providing the minimum temperature netCDF dataset. Units: degree Celsius.
See \code{tmax.data} for the elements of the list.}

\item{rh.from}{character, source of relative humidity data. Valid options: \code{"minmax"} and \code{"mean"}.
\itemize{
\item \code{"minmax"}: the humidity data are from the minimum and maximum relative humidity.
\item \code{"mean"}: the humidity data is from a mean relative humidity data.
}}

\item{rhmax.data}{named list, if \code{rh.from} is equal to \code{"minmax"},
providing the maximum relative humidity netCDF dataset. Units: percentage.
See \code{tmax.data} for the elements of the list.}

\item{rhmin.data}{named list, if \code{rh.from} is equal to \code{"minmax"},
providing the minimum relative humidity netCDF dataset. Units: percentage.
See \code{tmax.data} for the elements of the list.}

\item{rhmean.data}{named list, if \code{rh.from} is equal to \code{"mean"},
providing the mean relative humidity netCDF dataset. Units: percentage.
See \code{tmax.data} for the elements of the list.}

\item{wff10.data}{named list, providing the wind speed at 10 meters netCDF dataset. Units: m/s.
See \code{tmax.data} for the elements of the list.}

\item{pres.data}{named list, providing the surface pressure netCDF dataset. Units: hPa.
See \code{tmax.data} for the elements of the list.}

\item{rad.data}{named list, providing the solar radiation flux netCDF dataset. Units: W/m^2.
See \code{tmax.data} for the elements of the list.}

\item{elev.data}{named list, providing the Digital Elevation Model in netCDF format. Units: meters.
\itemize{
\item \code{file}: character, full path to the netCDF file containing the elevation data.
\item \code{varid}: character, name of the variable to read from the netCDF data.
\item \code{ilon}: integer, order for the longitude dimension of the variable.
\item \code{ilat}: integer, order for the latitude dimension of the variable.
}}

\item{output}{named list, the elements of the list are
\itemize{
\item \code{dir}: character, full path to the directory to save the calculated reference evapotranspiration
\item \code{format}: character, format of the output netCDF file names, the dates are represented by \code{\%S}
}}
}
\description{
Function to compute daily reference evapotranspiration using the FAO Penman-Monteith equation.
}
\references{
Allen, R.G., Pereira, L.S., Raes, D., and Smith, M. 1998. Crop evapotranspiration: Guidelines for computing crop requirements. Irrigation and Drainage PaperNo. 56, FAO, Rome, Italy 300 p.
\url{https://www.fao.org/3/X0490E/x0490e00.htm}
}
