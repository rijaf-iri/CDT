#' Install wgrib.
#' 
#' Function to download and compile wgrib into CDT local folder.
#'
#' @param cc the \code{C} compiler to be used or the full path to the \code{C} compiler.
#' 
#' @references \url{https://www.cpc.ncep.noaa.gov/products/wesley/wgrib.html}
#' 
#' @examples
#'
#' \dontrun{
#' install_wgrib()
#' }
#' 
#' @export

install_wgrib <- function(cc = "gcc"){
    compile_wgrib_cmd(cc)
}

#' Install wgrib2.
#' 
#' Function to download and build wgrib2 into CDT local folder.
#'
#' @param make the make command or full path to make command. 
#' @param cc the \code{C} compiler to be used or the full path to the \code{C} compiler.
#' @param fc the \code{Fortran} compiler to be used or the full path to the \code{Fortran} compiler.
#' 
#' @references \url{https://www.cpc.ncep.noaa.gov/products/wesley/wgrib2/index.html}
#'
#' @examples
#'
#' \dontrun{
#' ## Install wgrib2 on Windows
#' install_wgrib2()
#' 
#' ## Install wgrib2 on MacOS
#' install_wgrib2(make = '/usr/local/bin/gmake', cc = '/usr/local/bin/gcc-13', fc = '/usr/local/bin/gfortran')
#' 
#' ## Install wgrib2 on Linux with the default make, CC and FC
#' install_wgrib2()
#' }
#' 
#' @export

install_wgrib2 <- function(make = "make", cc = "gcc", fc = "gfortran"){
    build_wgrib2_cmd(make, cc, fc)
}
