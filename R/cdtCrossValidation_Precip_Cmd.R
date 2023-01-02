#' Cross-validation, merging stations observation and satellite rainfall estimates data.
#'
#' Function to perform a Leave-One-Out Cross-Validation for rainfall merging.
#' 
#' @param time.step character, the time step of the data. Available options: \code{"daily"}, \code{"pentad"}, \code{"dekadal"}, \code{"monthly"}.
#' @param dates named list, providing the dates to merge.
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
#' @param station.data named list, providing the station data to be used in CDT format.
#' \itemize{
#' \item{\code{file}: }{character, full path to the file containing the stations data}
#' \item{\code{sep}: }{character, column separator of the data}
#' \item{\code{na.strings}: }{character, missing values flag}
#' }
#' @param netcdf.data named list, providing the input netCDF dataset to be used.
#' \itemize{
#' \item{\code{dir}: }{character, full path to the directory containing the netCDF files.}
#' \item{\code{format}: }{character, format of the netCDF file names}
#' \item{\code{varid}: }{character, name of the variable to read from the netCDF data}
#' \item{\code{ilon}: }{integer, order for the longitude dimension of the variable. 
#' Example: if the variable "precip" has the dimension order [Lat, Lon] then \code{ilon} must be 2}
#' \item{\code{ilat}: }{integer, order for the latitude dimension of the variable.}
#' }
#' @param merge.method named list, indicating the merging method.
#' \itemize{
#' \item{\code{"method"}: }{character, the merging method. Valid options: \code{"CSc"}, \code{"BSc"}, \code{"SBA"} or \code{"RK"}.
#'  \itemize{
#'   \item{\strong{"CSc"}: }{Cressman Scheme}
#'   \item{\strong{"BSc"}: }{Barnes Scheme}
#'   \item{\strong{"SBA"}: }{Simple Bias Adjustment}
#'   \item{\strong{"RK"}: }{Regression Kriging}
#'  }
#' }
#' \item{\code{"nrun"}: }{integer, number of the nested run to be performed}
#' \item{\code{"pass"}: }{numeric vector giving the fraction of \code{nmin}, \code{nmax} and \code{maxdist} to be used for each pass.}
#' }
#' @param interp.method named list, indicating the interpolation method and parameters to be used for \code{"SBA"} and \code{"RK"}. 
#' \itemize{
#' \item{\code{method}: }{character, the interpolation method to be used.
#' Valid options: \code{"idw"}, \code{"shepard"}, \code{"sphere"} or \code{"okr"}.
#' \itemize{
#'  \item{\strong{"idw"}: }{Inverse distance weighted}
#'  \item{\strong{"shepard"}: }{Modified Shepard interpolation}
#'  \item{\strong{"sphere"}: }{Spheremap interpolation method}
#'  \item{\strong{"okr"}: }{Ordiranry kriging}
#' }
#' }
#' \item{\code{nmin}: }{integer, minimum number of stations to be used to interpolate a grid point}
#' \item{\code{nmax}: }{integer, maximum number of stations to be used to interpolate a grid point}
#' \item{\code{maxdist}: }{numeric, maximum radius of influence in decimal degree}
#' \item{\code{use.block}: }{logical, use block mean values to interpolate a grid point}
#' \item{\code{vargrd}: }{logical, use a variable radius of influence}
#' \item{\code{vgm.model}: }{character vector of variogram model to be used if \code{method} is \code{"okr"}. Default is \code{c("Exp", "Gau", "Sph", "Pen")}}
#' }
#' @param crossv.station named list, selecting the stations to use for the cross-validation.
#' The list includes an element \code{from} with available options \code{"all"}, \code{"file"} or \code{"cdt"}, and 
#' an element \code{pars} which is a named list specifying the parameters related to \code{from}:
#' \itemize{
#' \item{\strong{"all"}: }{all the stations from \code{station.data} will be used for cross-validation, \code{pars} can be omitted}
#' \item{\strong{"file"}: }{the list of stations to be used for the cross-validation comes from a file.
#'  \itemize{
#'    \item{\code{type}: }{character, the type of the data, valid options are:\cr
#'       \strong{"cdtstation"}: the stations come from a CDT stations data, \cr
#'       \strong{"cdtcoords"}: the stations come from a CDT coordinates file}
#'    \item{\code{file}: }{character, full path to the file containing the stations}
#'    \item{\code{sep}: }{character, column separator of the data}
#'    \item{\code{na.strings}: }{character, missing values flag}
#'    \item{\code{header}: }{logical, in case of \strong{"cdtcoords"}, set \code{TRUE} if the data has a header}
#'  }
#' }
#' \item{\strong{"cdt"}: }{the list of stations to be used for the cross-validation will be selected from \code{station.data} 
#'   by providing the percent of minimum available data for each station.\cr
#'   Example: \code{pars = list(min.perc = 40)}}
#' }
#' @param auxvar named list, specifying the auxiliary variables to use when the merging method is \code{"RK"}.
#' \itemize{
#' \item{\code{dem}: }{logical, include elevation data as auxiliary variable}
#' \item{\code{slope}: }{logical, include slope as auxiliary variable}
#' \item{\code{aspect}: }{logical, include aspect as auxiliary variable}
#' \item{\code{lon}: }{logical, include longitude as auxiliary variable}
#' \item{\code{lat}: }{logical, include latitude as auxiliary variable}
#' }
#' @param dem.data named list, providing the Digital Elevation Model (in netCDF format) when using regression kriging method with elevation related data as auxiliary variable.
#' \itemize{
#' \item{\code{file}: }{character, full path to the netCDF file containing the elevation data.}
#' \item{\code{varid}: }{character, name of the variable to read from the netCDF data}
#' \item{\code{ilon}: }{integer, order for the longitude dimension of the variable.}
#' \item{\code{ilat}: }{integer, order for the latitude dimension of the variable.}
#' }
#' @param RnoR named list, specifying the rain-no-rain mask parameters.
#' \itemize{
#' \item{\code{use}: }{logical, apply rain-no-rain mask}
#' \item{\code{wet}: }{numeric, threshold to be use to define the wet/dry event}
#' \item{\code{smooth}: }{logical, smooth the rain-no-rain mask after interpolation}
#' }
#' @param output.dir character, full path to the directory to save the output.
#' @param GUI logical, indicating whether or not the output message should be displayed on CDT GUI. If \code{TRUE}, CDT GUI must be open.
#' 
#' @export

cdtCrossValidationPrecipCMD <- function(time.step = "dekadal",
                                        dates = list(from = "range", pars = list(start = "2018011", end = "2018123")),
                                        station.data = list(file = "", sep = ",", na.strings = "-99"),
                                        netcdf.data = list(dir = "", format = "rr_adj_%s%s%s.nc",
                                                           varid = "precip", ilon = 1, ilat = 2),
                                        merge.method = list(method = "SBA", nrun = 3, pass = c(1, 0.75, 0.5)),
                                        interp.method = list(method = "idw", nmin = 8, nmax = 16, maxdist = 2.5,
                                                             use.block = TRUE, vargrd = FALSE,
                                                             vgm.model = c("Sph", "Exp", "Gau", "Pen")),
                                        crossv.station = list(from = "all", pars = NULL),
                                        auxvar = list(dem = FALSE, slope = FALSE, aspect = FALSE, lon = FALSE, lat = FALSE),
                                        dem.data = list(file = "", varid = "dem", ilon = 1, ilat = 2),
                                        RnoR = list(use = FALSE, wet = 1.0, smooth = FALSE),
                                        output.dir = "",
                                        GUI = FALSE)
{
    cdtLocalConfigData()
    xml.dlg <- file.path(.cdtDir$dirLocal, "languages", "cdtCrossValidation_ClimData_dlgBox.xml")
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

    if(crossv.station$from == "file"){
        if(!file.exists(crossv.station$pars$file)){
            msg <- paste("File containing the stations, to be used for the cross-validation, does not exist",
                         ":", crossv.station$pars$file)
            Insert.Messages.Out(msg, TRUE, "e", GUI)
            return(NULL)
        }

        cv_pars <- list(type = "cdtstation", file = "", sep = ",", na.strings = "-99", header = FALSE)
        crossv.station$pars <- init.default.list.args(crossv.station$pars, cv_pars)
    }

    if(crossv.station$from == "cdt"){
        cv_pars <- list(min.perc = 40)
        crossv.station$pars <- init.default.list.args(crossv.station$pars, cv_pars)
    }

    #######
    mrgmthd_pars <- list(method = "SBA", nrun = 3, pass = c(1, 0.75, 0.5))
    merge.method <- init.default.list.args(merge.method, mrgmthd_pars)

    #######
    intmthd_pars <- list(method = "idw", vargrd = FALSE,
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
    rnr_pars <- list(use = FALSE, wet = 1.0, smooth = FALSE)
    RnoR <- init.default.list.args(RnoR, rnr_pars)

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

            dirMrg <- paste0('CrossValidation_Precip_Data_',
                             tools::file_path_sans_ext(basename(dates$pars$file)))
        }else{
            daty <- dates$pars$dates
            if(is.null(daty)){
                Insert.Messages.Out("No vector dates", TRUE, "e", GUI)
                return(NULL)
            }

            dirMrg <- paste('CrossValidation_Precip_Data', daty[1], daty[length(daty)], sep = '_')
        }

        ncInfo <- ncInfo.from.date.vector(netcdf.data, daty, time.step)
    }else{
        date.range <- split_date.range(time.step, dates$pars)
        daty <- get.range.date.time(date.range, time.step)
        if(time.step == 'monthly'){
            xdeb <- format(daty$start, "%b%Y")
            xfin <- format(daty$end, "%b%Y")
        }else{
            xdeb <- paste0(as.numeric(format(daty$start, "%d")), format(daty$start, "%b%Y"))
            xfin <- paste0(as.numeric(format(daty$end, "%d")), format(daty$end, "%b%Y"))
        }

        dirMrg <- paste('CrossValidation_Precip_Data', xdeb, xfin, sep = '_')
        ncInfo <- ncInfo.with.date.range(netcdf.data, date.range, time.step)
    }

    if(is.null(ncInfo)){
        Insert.Messages.Out(message[['14']], TRUE, "e", GUI)
        return(NULL)
    }

    outdir <- file.path(output.dir, dirMrg)
    dir.create(outdir, showWarnings = FALSE, recursive = TRUE)

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

    grd.lon <- ncInfo$ncinfo$lon
    grd.lat <- ncInfo$ncinfo$lat
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
    ## select station for cross-validation

    nbNA <- colSums(!is.na(stnData$data[stnData$dates %in% ncInfo$dates, , drop = FALSE]))

    if(!any(nbNA > 0)){
        Insert.Messages.Out(message[['15']], TRUE, "e", GUI)
        return(NULL)
    }

    if(crossv.station$from == "file"){
        cv_pars <- crossv.station$pars[!names(crossv.station$pars) %in% "type"]
        cvpars_read <- list(stringsAsFactors = FALSE, colClasses = "character")
        df <- do.call(read.table, c(cv_pars, cvpars_read))

        if(crossv.station$pars$type == 'cdtstation'){
            df <- splitCDTData0(df, GUI)
            if(is.null(df)) return(NULL)
            df <- as.data.frame(df[c("id", 'lon', 'lat')])
        }

        istn <- as.character(df[, 1]) %in% stnData$id
        if(!any(istn)){
            Insert.Messages.Out(message[['20']], TRUE, "e", GUI)
            return(NULL)
        }

        if(any(!istn)){
            outlist <- list(message[['21']], df[!istn, , drop = FALSE])
            if(GUI){
                containertab <- Display_Output_Console_Tab(outlist, title = basename(crossv.station$pars$file))
                ntab <- update.OpenTabs('ctxt', containertab)
                tkselect(.cdtEnv$tcl$main$tknotes, ntab)
            }else{
                print(outlist)
            }
        }

        stn.valid <- as.character(df[istn, 1])
        ix <- match(stn.valid, stnData$id)
        stn.valid <- stn.valid[nbNA[ix] > 0]
        if(length(stn.valid) == 0){
            Insert.Messages.Out(message[['15']], TRUE, "e", GUI)
            return(NULL)
        }
    }

    if(crossv.station$from == "cdt"){
        istn <- nbNA / length(ncInfo$dates) >= (crossv.station$pars$min.perc / 100)
        if(!any(istn)){
            Insert.Messages.Out(message[['16']], TRUE, "e", GUI)
            return(NULL)
        }
        df <- as.data.frame(stnData[c("id", 'lon', 'lat')])
        df <- df[istn, , drop = FALSE]

        stn.valid <- select.Station.Validation(df, perc = 80)
        stn.valid <- as.character(stn.valid$id)
    }

    if(crossv.station$from == "all"){
        stn.valid <- stnData$id[nbNA > 0]
        if(length(stn.valid) == 0){
            Insert.Messages.Out(message[['15']], TRUE, "e", GUI)
            return(NULL)
        }
    }

    stn.valid <- which(stnData$id %in% stn.valid)

    ##################

    params <- list(period = time.step, MRG = merge.method,
                   interp = interp.method, auxvar = auxvar, RnoR = RnoR)

    ret <- cdtMergingLOOCV(stnData = stnData, stnVID = stn.valid,
                           ncInfo = ncInfo, xy.grid = xy.grid, 
                           params = params, variable = "rain",
                           demData = demData, outdir = outdir, GUI = GUI)

    if(!is.null(ret)){
        if(ret != 0){
          Insert.Messages.Out(paste(message[['17']],
                              file.path(outdir, "log_file.txt")), TRUE, "w", GUI)
        }
    }else return(NULL)

    Insert.Messages.Out(message[['18']], TRUE, "s", GUI)
    return(0)
}
