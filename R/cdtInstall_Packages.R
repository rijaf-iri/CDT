
#' Check and install CDT dependencies packages
#'
#' Check CDT dependencies packages and install them if not installed yet
#' 
#' @param interactive if \code{TRUE} the path to the packages directory (Windows)
#'    or configure.args for \code{rgeos} and \code{rgdal} (MacOS and Linux)
#'    will be entered from the console.
#' @param config A list containing the full path of the packages directory or
#'    the path to set the configure.args for \code{rgeos} and \code{rgdal}.
#'    Leave \code{NULL} if the \code{interactive} is \code{TRUE}.
#'    (See example below.)
#' 
#' @examples
#' library(CDT)
#' 
#' # Interactive mode
#' cdtInstall.Packages()
#' 
#' # Windows
#' cdtInstall.Packages(FALSE, config = list(
#' 		local.dir = TRUE,
#' 		pkg.path = "C:/Users/rijaf/Desktop/R3.5-packages")
#'  )
#' 
#' # MacOS and Linux
#' cdtInstall.Packages(FALSE, config = list(
#' 		geos_config = "/Library/Frameworks/GEOS.framework/unix/bin/geos-config",
#' 		gdal_config = "/Library/Frameworks/GDAL.framework/unix/bin/gdal-config",
#' 		proj_include = "/Library/Frameworks/PROJ.framework/unix/include",
#' 		proj_lib = "/Library/Frameworks/PROJ.framework/unix/lib")
#'  )
#'
#' @export

cdtInstall.Packages <- function(interactive = TRUE, config = NULL){
	required.packages <- c( 'tkrplot', 'latticeExtra', 'ncdf4',
							'R.utils', 'jsonlite', 'RCurl', 'stringr', 'reshape2',
							'foreach', 'doParallel',
							'gstat', 'automap', 'fields', 'gmt',
							'fitdistrplus', 'lmomco', 'qmap', 'ADGofTest',
							'sp', 'maptools', 'raster',
							'matrixStats', 'data.table',
							'rgeos', 'rgdal'
						)
	new.packages <- required.packages[!(required.packages %in% installed.packages()[, "Package"])]

	if(length(new.packages) > 0){
		if(WindowsOS()){
			online <- TRUE
			if(interactive){
				rep <- menu(c("Yes", "No"), title = "Do you want to install Packages from local directory?")
				if(rep == 1){
					cat("Full path to the directory containing the packages (Change the '\\' to '/')\n")
					dirpath <- readline(prompt = "Path: ")
					path2pkgs <- gsub("^\\s+|\\s+$", "", dirpath)
					online <- FALSE
				}
			}else{
				if(config$local.dir){
					path2pkgs <- config$pkg.path
					online <- FALSE
				}
			}

			if(!online){
				if(file.exists(path2pkgs)){
					tools::write_PACKAGES(path2pkgs)
					install.packages(new.packages, contriburl = file.path("file://", path2pkgs))
				}else stop(paste(path2pkgs, 'does not exist.'))
			}else install.packages(new.packages)
		}else{
			if(!any(c('rgdal', 'rgeos') %in% new.packages)){
				install.packages(new.packages)
			}else{
				new.packages1 <- new.packages[!new.packages %in% c('rgdal', 'rgeos')]
				if(length(new.packages1) > 0) install.packages(new.packages1)
				if('rgeos' %in% new.packages){
					if(interactive){
						geos_config <- suppressWarnings(system("which geos-config", intern = TRUE))
						if(length(geos_config) == 0){
							cat("geos-config not found\n")
							cat("Enter the full path to geos-config\n")
							geos_config <- readline(prompt = "Path: ")
							geos_config <- gsub("^\\s+|\\s+$", "", geos_config)
						}
					}else geos_config <- config$geos_config

					if(!file.exists(geos_config)) stop('geos-config not found')
					install.packages('rgeos', type = "source", configure.args = paste0('--with-geos-config=', geos_config))
				}
				if('rgdal' %in% new.packages){
					if(interactive){
						gdal_config <- suppressWarnings(system("which gdal-config", intern = TRUE))
						if(length(gdal_config) == 0){
							cat("gdal-config not found\n")
							cat("Enter the full path to gdal-config\n")
							gdal_config <- readline(prompt = "Path: ")
							gdal_config <- gsub("^\\s+|\\s+$", "", gdal_config)
						}
					}else gdal_config <- config$gdal_config
					if(!file.exists(gdal_config)) stop('gdal-config not found')

					if(interactive){
						proj_include <- file.path(dirname(dirname(gdal_config)), "include")
						if(!file.exists(file.path(proj_include, "proj_api.h"))){
							cat("could not find proj_api.h\n")
							cat("Enter the full path to PROJ.4 include directory\n")
							proj_include <- readline(prompt = "Path: ")
							proj_include <- gsub("^\\s+|\\s+$", "", proj_include)
						}
						proj_lib <- file.path(dirname(proj_include), "lib")
					}else{
						proj_include <- config$proj_include
						proj_lib <- config$proj_lib
					}
					if(!file.exists(file.path(proj_include, "proj_api.h"))) stop('PROJ.4 installation not found')

					install.packages('rgdal', type = "source", 
							 configure.args = c(paste0('--with-proj-include=', proj_include),
												paste0('--with-proj-lib=', proj_lib),
												paste0('--with-gdal-config=', gdal_config)))
				}
			}
		}
	}else{
		cat("All packages were installed\n")
	}

	invisible()
}
