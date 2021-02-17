#' Merging stations observation and satellite rainfall estimates data.
#'
#' Function to merge stations observation and satellite rainfall estimates data.
#' 
#' @param time.step the time step of the data. Available options: \code{"daily"}, \code{"pentad"}, \code{"dekadal"}, \code{"monthly"}.
#' @param dates a named list providing the dates to merge.
#' The list includes an element \code{from} with available options \code{"range"}, \code{"file"} or \code{"dates"}, and 
#'  an element \code{pars} which is a named list specifying the parameters related to \code{from}:
#' \itemize{
#' \item{\strong{"range"}: }{\code{pars} specifies the start and end dates to merge. \cr
#' Example: \code{pars = list(start = "2018011", end = "2018123")}}
#' \item{\strong{"file"}: }{\code{pars} specifies the full path to the file containing the dates to merge.
#'  Example: \code{pars = list(file = "/home/data/files/dates.txt")}\cr
#' The contents of the file are as follows:\cr
#' ## cat /home/data/files/dates.txt \cr
#' 2020011\cr
#' 2020012\cr
#' 2020013\cr
#' ......
#' }
#' \item{\strong{"dates"}: }{\code{pars} specifies a vector containing the dates to merge. \cr
#' Example: \code{pars = list(dates = c("2020011", "2020012", 2020091, 2020113))}}
#' } 
#' @param station.data a named list providing the station data to be used in CDT format.
#' \itemize{
#' \item{\code{file}: }{full path to the file containing the stations data}
#' \item{\code{sep}: }{column separator of the data}
#' \item{\code{na.strings}: }{missing values flag}
#' }
#' @param netcdf.data a named list providing the input netCDF dataset to be used.
#' \itemize{
#' \item{\code{dir}: }{full path to the directory containing the netCDF files.}
#' \item{\code{format}: }{format of the netCDF file names}
#' \item{\code{varid}: }{name of the variable to read from the netCDF data}
#' \item{\code{ilon}: }{order for the longitude dimension of the variable. 
#' Example: if the variable "precip" has the dimension order [Lat, Lon] then \code{ilon} must be 2}
#' \item{\code{ilat}: }{order for the latitude dimension of the variable.}
#' }
#' @param merge.method a named list indicating the merging method.
#' \itemize{
#' \item{\code{"method"}: }{the merging method. Valid options: \code{"CSc"}, \code{"BSc"}, \code{"SBA"} or \code{"RK"}.
#'  \itemize{
#'   \item{\strong{"CSc"}: }{Cressman Scheme}
#'   \item{\strong{"BSc"}: }{Barnes Scheme}
#'   \item{\strong{"SBA"}: }{Simple Bias Adjustment}
#'   \item{\strong{"RK"}: }{Regression Kriging}
#'  }
#' }
#' \item{\code{"nrun"}: }{number of the nested run to be performed}
#' \item{\code{"pass"}: }{vector giving the fraction of \code{nmin}, \code{nmax} and \code{maxdist} to be used for each pass.}
#' }
#' @param interp.method a named list indicating the interpolation method and parameters to be used for \code{"SBA"} and \code{"RK"}. 
#' \itemize{
#' \item{\code{method}: }{the interpolation method to be used.
#' Valid options: \code{"idw"}, \code{"shepard"}, \code{"sphere"} or \code{"okr"}.
#' \itemize{
#'  \item{\strong{"idw"}: }{Inverse distance weighted}
#'  \item{\strong{"shepard"}: }{Modified Shepard interpolation}
#'  \item{\strong{"sphere"}: }{Spheremap interpolation method}
#'  \item{\strong{"okr"}: }{Ordiranry kriging}
#' }
#' }
#' \item{\code{nmin}: }{minimum number of stations to be used to interpolate a grid point}
#' \item{\code{nmax}: }{maximum number of stations to be used to interpolate a grid point}
#' \item{\code{maxdist}: }{maximum radius of influence in decimal degree}
#' \item{\code{use.block}: }{logical, use block mean values to interpolate a grid point}
#' \item{\code{vargrd}: }{logical, use a variable radius of influence}
#' \item{\code{minstn}: }{minimum number of non missing values from station to perform the interpolation}
#' \item{\code{vgm.model}: }{vector of variogram model to be used if \code{method} is \code{"okr"}. Default is \code{c("Exp", "Gau", "Sph", "Pen")}}
#' }
#' @param auxvar a named list specifying the auxiliary variables to use when the merging method is \code{"RK"}.
#' \itemize{
#' \item{\code{dem}: }{logical, include elevation data as auxiliary variable}
#' \item{\code{slope}: }{logical, include slope as auxiliary variable}
#' \item{\code{aspect}: }{logical, include aspect as auxiliary variable}
#' \item{\code{lon}: }{logical, include longitude as auxiliary variable}
#' \item{\code{lat}: }{logical, include latitude as auxiliary variable}
#' }
#' @param dem.data a named list providing the Digital Elevation Model (in netCDF format) when using regression kriging method with elevation related data as auxiliary variable.
#' \itemize{
#' \item{\code{file}: }{full path to the netCDF file containing the elevation data.}
#' \item{\code{varid}: }{name of the variable to read from the netCDF data}
#' \item{\code{ilon}: }{order for the longitude dimension of the variable.}
#' \item{\code{ilat}: }{order for the latitude dimension of the variable.}
#' }
#' @param grid a named list providing the grid to use to interpolate the data.
#' The list includes an element \code{from} with available options \code{"data"}, \code{"ncdf"} or \code{"new"}, and 
#'  an element \code{pars} which is a named list specifying the parameters related to \code{from}:
#' \itemize{
#' \item{\strong{"data"}: }{the grid to interpolate will be taken from the input netCDF data, \code{pars} can be omitted} 
#' \item{\strong{"ncdf"}: }{the grid to interpolate will be taken from a provided netCDF file, 
#' \code{pars} specifies the full path to the netCDF file containing the grid to be extracted, 
#' the name of the variable to read, the order for the longitude and latitude dimension.\cr
#' Example: \code{pars = list(file = "/home/data/files/rfe_2020121.nc", varid = "precip", ilon = 1, ilat = 2)}
#' }
#' \item{\strong{"new"}: }{the grid to interpolate will be created from the information provided by user, 
#' \code{pars} specifies the minimum/maximum of the longitude and latitude of the domain and the resolution to be used. \cr
#' Example: \code{pars = list(minlon = 42, maxlon = 52, minlat = -26, maxlat = -11, reslon = 0.1, reslat = 0.1)}
#' }
#' }
#' @param RnoR a named list specifying the rain-no-rain mask parameters.
#' \itemize{
#' \item{\code{use}: }{logical, apply rain-no-rain mask}
#' \item{\code{wet}: }{threshold to be use to define the wet/dry event}
#' \item{\code{smooth}: }{logical, smooth the rain-no-rain mask after interpolation}
#' }
#' @param blank a named list indicating if the data outside a provided shapefile will be removed.
#' \itemize{
#' \item{\code{data}: }{logical, blank the grid outside the provided shapefile}
#' \item{\code{shapefile}: }{full path to the shapefile to be used for blanking with the extension ".shp"}
#' }
#' @param output a named list indicating the directory to save the merged data and the format of the merged netCDF file name.
#' @param precision a named list indicating the precision of the merged data.
#' \itemize{
#' \item{\code{from.data}: }{if \code{TRUE} the precision from the input netCDF data will be used.}
#' \item{\code{prec}: }{if \code{from.data} is \code{FALSE} specify here the precision to be used}
#' }
#' @param GUI a logical indicating whether or not the output message should be displayed on CDT GUI. If \code{TRUE}, CDT GUI must be open.
#' 
#' @export

cdtMergingPrecipCMD <- function(time.step = "dekadal",
                          dates = list(from = "range", pars = list(start = "2018011", end = "2018123")),
                          station.data = list(file = "", sep = ",", na.strings = "-99"),
                          netcdf.data = list(dir = "", format = "rr_adj_%s%s%s.nc",
                                             varid = "precip", ilon = 1, ilat = 2),
                          merge.method = list(method = "SBA", nrun = 3, pass = c(1, 0.75, 0.5)),
                          interp.method = list(method = "idw", nmin = 8, nmax = 16, maxdist = 2.5,
                                               use.block = TRUE, vargrd = FALSE, minstn = 10,
                                               vgm.model = c("Sph", "Exp", "Gau", "Pen")),
                          auxvar = list(dem = FALSE, slope = FALSE, aspect = FALSE, lon = FALSE, lat = FALSE),
                          dem.data = list(file = "", varid = "dem", ilon = 1, ilat = 2),
                          grid = list(from = "data", pars = NULL),
                          RnoR = list(use = FALSE, wet = 1.0, smooth = FALSE),
                          blank = list(data = FALSE, shapefile = ""),
                          output = list(dir = "", format = "rr_mrg_%s%s%s.nc"),
                          precision = list(from.data = TRUE, prec = "short"),
                          GUI = FALSE)
{
    cdtLocalConfigData()
    xml.dlg <- file.path(.cdtDir$dirLocal, "languages", "cdtPrecip_Merging_dlgBox.xml")
    lang.dlg <- cdtLanguageParse(xml.dlg, .cdtData$Config$lang.iso)
    message <- lang.dlg[['message']]

    Insert.Messages.Out(message[['10']], TRUE, "i", GUI)

    ##################

    if(!is.null(netcdf.data$dir)){
        if(!dir.exists(netcdf.data$dir)){
            msg <- paste("Folder containing the netCDF data does not exist", ":", netcdf.data$dir)
            Insert.Messages.Out(msg, TRUE, "e", GUI)
            return(NULL)
        }else{
            ncdata_pars <- list(dir = "", format = "rr_adj_%s%s%s.nc",
                               varid = "precip", ilon = 1, ilat = 2)
            netcdf.data <- init.default.list.args(netcdf.data, ncdata_pars)
        }
    }else{
        Insert.Messages.Out("No folder containing the netCDF data provided", TRUE, "e", GUI)
        return(NULL)
    }

    #######
    if(!is.null(station.data$file)){
        if(!file.exists(station.data$file)){
            msg <- paste("File containing the station data does not exist", ":", station.data$file)
            Insert.Messages.Out(msg, TRUE, "e", GUI)
            return(NULL)
        }else{
            stndata_pars <- list(file = "", sep = ",", na.strings = "-99")
            station.data <- init.default.list.args(station.data, stndata_pars)
        }
    }else{
        Insert.Messages.Out("No station data file provided", TRUE, "e", GUI)
        return(NULL)
    }

    #######
    mrgmthd_pars <- list(method = "SBA", nrun = 3, pass = c(1, 0.75, 0.5))
    merge.method <- init.default.list.args(merge.method, mrgmthd_pars)

    #######
    intmthd_pars <- list(method = "idw", vargrd = FALSE, minstn = 10,
                         nmin = 8, nmax = 16, maxdist = 2.5, use.block = TRUE,
                         vgm.model = c("Sph", "Exp", "Gau", "Pen"))
    interp.method <- init.default.list.args(interp.method, intmthd_pars)

    #######
    axvar_pars <- list(dem = FALSE, slope = FALSE, aspect = FALSE, lon = FALSE, lat = FALSE)
    auxvar <- init.default.list.args(auxvar, axvar_pars)

    #######
    dem_pars <- list(file = "", varid = "dem", ilon = 1, ilat = 2)
    dem.data <- init.default.list.args(dem.data, dem_pars)

    #######
    if(grid$from %in% c("data", "ncdf", "new")){
        if(grid$from != "data"){
            if(grid$from == "ncdf"){
                grid_pars <- list(file = "", varid = "precip",
                                  ilon = 1, ilat = 2)
            }
            if(grid$from == "new"){
                grid_pars <- list(minlon = 42, maxlon = 52,
                                  minlat = -26, maxlat = -11,
                                  reslon = 0.1, reslat = 0.1)
            }
            grid$pars <- init.default.list.args(grid$pars, grid_pars)
        }
    }else{
       Insert.Messages.Out("Unknown grid for interpolation", TRUE, "e", GUI)
       return(NULL)
    }

    #######
    rnr_pars <- list(use = FALSE, wet = 1.0, smooth = FALSE)
    RnoR <- init.default.list.args(RnoR, rnr_pars)

    #######
    blank_pars <- list(data = FALSE, shapefile = "")
    blank <- init.default.list.args(blank, blank_pars)

    #######
    output_pars <- list(dir = "", format = "rr_mrg_%s%s%s.nc")
    output <- init.default.list.args(output, output_pars)

    #######
    prec_pars = list(from.data = TRUE, prec = "short")
    precision <- init.default.list.args(precision, prec_pars)

    ##################

    if(is.null(dates$from)) dates$from <- "range"

    if(dates$from %in% c("file", "dates")){
        if(dates$from == "file"){
            if(!file.exists(dates$pars$file)){
                msg <- paste("File containing the date does not exist", ":", dates$pars$file)
                Insert.Messages.Out(msg, TRUE, "e", GUI)
                return(NULL)
            }
            daty <- read.table(dates$pars$file, stringsAsFactors = FALSE, colClasses = "character")
            daty <- daty[, 1]

            dirMrg <- paste0('Merged_Precip_Data_',
                             tools::file_path_sans_ext(basename(dates$pars$file)))
        }else{
            daty <- dates$pars$dates
            if(is.null(daty)){
                Insert.Messages.Out("No vector dates", TRUE, "e", GUI)
                return(NULL)
            }

            dirMrg <- paste('Merged_Precip_Data', daty[1], daty[length(daty)], sep = '_')
        }

        ncInfo <- ncInfo.from.date.vector(netcdf.data, daty, time.step)
    }else{
        date.range <- split_date.range(time.step, dates$pars)
        # date.range <- rename_date.range(dates$pars)
        daty <- get.range.date.time(date.range, time.step)
        if(time.step == 'monthly'){
            xdeb <- format(daty$start, "%b%Y")
            xfin <- format(daty$end, "%b%Y")
        }else{
            xdeb <- paste0(as.numeric(format(daty$start, "%d")), format(daty$start, "%b%Y"))
            xfin <- paste0(as.numeric(format(daty$end, "%d")), format(daty$end, "%b%Y"))
        }

        dirMrg <- paste('Merged_Precip_Data', xdeb, xfin, sep = '_')
        ncInfo <- ncInfo.with.date.range(netcdf.data, date.range, time.step)
    }

    if(is.null(ncInfo)){
        Insert.Messages.Out(message[['15']], TRUE, "e", GUI)
        return(NULL)
    }

    outdir <- file.path(output$dir, dirMrg)
    dir.create(outdir, showWarnings = FALSE, recursive = TRUE)

    Insert.Messages.Out(message[['11']], TRUE, "i", GUI)

    ##################
    ## Station data

    stnpars_read <- list(stringsAsFactors = FALSE, colClasses = "character")
    stnData <- do.call(read.table, c(station.data, stnpars_read))
    stnData <- splitCDTData0(stnData, GUI)
    if(is.null(stnData)) return(NULL)

    ##################
    ## Get RFE data info

    varid <- netcdf.data$varid
    nc <- nc_open(ncInfo$ncfiles[ncInfo$exist][1])
    lon <- nc$var[[varid]]$dim[[netcdf.data$ilon]]$vals
    lat <- nc$var[[varid]]$dim[[netcdf.data$ilat]]$vals
    varinfo <- nc$var[[varid]][c('name', 'prec', 'units', 'longname', 'missval')]
    nc_close(nc)

    xo <- order(lon)
    lon <- lon[xo]
    yo <- order(lat)
    lat <- lat[yo]
    ncInfo$ncinfo <- list(varid = varid, lon = lon, lat = lat,
                          ilon = netcdf.data$ilon, ilat = netcdf.data$ilat,
                          xo = xo, yo = yo, varinfo = varinfo)
    ##################
    ## DEM data

    demData <- NULL
    if(merge.method$method == "RK" &
       (auxvar$dem | auxvar$slope | auxvar$aspect)
      )
    {
        if(!file.exists(dem.data$file)){
            Insert.Messages.Out(message[['13']], TRUE, "e", GUI)
            return(NULL)
        }
        nc <- nc_open(dem.data$file)
        demData$x <- nc$var[[dem.data$varid]]$dim[[dem.data$ilon]]$vals
        demData$y <- nc$var[[dem.data$varid]]$dim[[dem.data$ilat]]$vals
        demData$z <- ncvar_get(nc, dem.data$varid)
        nc_close(nc)

        xo <- order(demData$x)
        demData$x <- demData$x[xo]
        yo <- order(demData$y)
        demData$y <- demData$y[yo]
        demData$z <- if(dem.data$ilon < dem.data$ilat) demData$z[xo, yo] else t(demData$z)[xo, yo]
    }

    ##################
    ##Create grid for interpolation

    if(grid$from == "data"){
        grd.lon <- ncInfo$ncinfo$lon
        grd.lat <- ncInfo$ncinfo$lat
    }

    if(grid$from == "new"){
        grd.lon <- seq(grid$pars$minlon, grid$pars$maxlon, grid$pars$reslon)
        grd.lat <- seq(grid$pars$minlat, grid$pars$maxlat, grid$pars$reslat)
    }

    if(grid$from == "ncdf"){
        if(!file.exists(grid$pars$file)){
            Insert.Messages.Out(message[['14']], TRUE, "e", GUI)
            return(NULL)
        }
        nc <- nc_open(grid$pars$file)
        lon <- nc$var[[grid$pars$varid]]$dim[[grid$pars$ilon]]$vals
        lat <- nc$var[[grid$pars$varid]]$dim[[grid$pars$ilat]]$vals
        nc_close(nc)

        grd.lon <- lon[order(lon)]
        grd.lat <- lat[order(lat)]
    }

    xy.grid <- list(lon = grd.lon, lat = grd.lat)

    ##################
    ## regrid DEM data

    if(!is.null(demData)){
        demData$lon <- demData$x
        demData$lat <- demData$y
        is.regridDEM <- is.diffSpatialPixelsObj(defSpatialPixels(xy.grid),
                                                defSpatialPixels(demData),
                                                tol = 1e-07)
        if(is.regridDEM){
            demData <- cdt.interp.surface.grid(demData, xy.grid)
        }else demData <- demData[c('x', 'y', 'z')]
        demData$z[demData$z < 0] <- 0
    }

    ##################
    ## blanking

    outMask <- NULL

    if(blank$data){
        if(!file.exists(blank$shapefile)){
            msg <- paste("The shapefile not found", ":", blank$shapefile)
            Insert.Messages.Out(msg, TRUE, "e", GUI)
            return(NULL)
        }
        dsn <- dirname(blank$shapefile)
        layer <- tools::file_path_sans_ext(basename(blank$shapefile))
        shpd <- rgdal::readOGR(dsn, layer)
        proj4string(shpd) <- CRS(as.character(NA))
        outMask <- create.mask.grid(shpd, xy.grid)
    }

    Insert.Messages.Out(message[['16']], TRUE, "s", GUI)

    ##################

    Insert.Messages.Out(message[['17']], TRUE, "i", GUI)

    params <- list(period = time.step, MRG = merge.method,
                   interp = interp.method, output = output,
                   auxvar = auxvar, RnoR = RnoR, prec = precision)

    ret <- cdtMerging(stnData = stnData, ncInfo = ncInfo,
                      xy.grid = xy.grid, params = params,
                      variable = "rain", demData = demData,
                      outdir = outdir, mask = outMask, GUI = GUI)

    if(!is.null(ret)){
        if(ret != 0){
          Insert.Messages.Out(paste(message[['18']],
                              file.path(outdir, "log_file.txt")), TRUE, "w", GUI)
        }
    }else return(NULL)

    Insert.Messages.Out(message[['19']], TRUE, "s", GUI)

    return(0)
}

# dates = list(from = "range", pars = list(start = "2018011", end = "2018123"))
# dates = list(from = "dates", pars = list(dates = c("2020011", "2020012")))
# dates = list(from = "file", pars = list(file = "/file/dates.txt"))
# ## cat /file/dates.txt
# ## 2020011
# ## 2020012
# ## .......

# grid = list(from = "data", pars = NULL)
# grid = list(from = "ncdf", pars = list(file = "", varid = "precip", ilon = 1, ilat = 2))
# grid = list(from = "new", pars = list(minlon = 42, maxlon = 52, minlat = -26, maxlat = -11, reslon = 0.1, reslat = 0.1))

