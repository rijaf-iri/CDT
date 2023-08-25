#' Temperature Bias Correction.
#'
#' Function to correct bias from reanalysis data.
#' 
#' @param time.step character, the time step of the data. Available options: \code{"daily"}, \code{"pentad"}, \code{"dekadal"}, \code{"monthly"}.
#' @param dates named list, providing the dates to correct the bias.
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
#' @param netcdf.data named list, providing the input netCDF dataset to be corrected.
#' \itemize{
#' \item{\code{dir}: }{character, full path to the directory containing the netCDF files.}
#' \item{\code{format}: }{character, format of the netCDF file names}
#' \item{\code{varid}: }{character, name of the variable to read from the netCDF data}
#' \item{\code{ilon}: }{integer, order for the longitude dimension of the variable. 
#' Example: if the variable "precip" has the dimension order [Lat, Lon] then \code{ilon} must be 2}
#' \item{\code{ilat}: }{integer, order for the latitude dimension of the variable.}
#' }
#' @param bias.method named list, indicating the bias parameters.
#' \itemize{
#' \item{\code{"method"}: }{character, the bias method. Valid options: \code{"mbvar"}, \code{"mbmon"}, \code{"qmdist"} or \code{"qmecdf"}.
#'  \itemize{
#'   \item{\strong{"mbvar"}: }{Multiplicative Bias Time Step Variable}
#'   \item{\strong{"mbmon"}: }{Multiplicative Bias for Each Month}
#'   \item{\strong{"qmdist"}: }{Quantile Mapping with Empirical Distribution}
#'   \item{\strong{"qmecdf"}: }{Quantile Mapping with Fitted Distribution}
#'  }
#' }
#' \item{\code{"dir"}: }{character, full path to the directory containing the bias coefficients in netCDF format}
#' \item{\code{format}: }{character, format of the bias netCDF file names}
#' }
#' @param output named list, indicating the directory to save the bias corrected data and the format of the bias corrected netCDF file name.
#' @param GUI logical, indicating whether or not the output message should be displayed on CDT GUI. If \code{TRUE}, CDT GUI must be open.
#' 
#' @export

cdtBiasCorrectTempCMD <- function(time.step = "dekadal",
                                  dates = list(from = "range", pars = list(start = "2018011", end = "2018123")),
                                  netcdf.data = list(dir = "", format = "tmax_down_%s%s%s.nc",
                                                     varid = "temp", ilon = 1, ilat = 2),
                                    bias.method = list(method = "mbvar", dir = "", format = "STN_GRID_Bias_%s.nc"),
                                    output = list(dir = "", format = "tmax_adj_%s%s%s.nc"),
                                    GUI = FALSE)
{
    cdtLocalConfigData()
    xml.dlg <- file.path(.cdtDir$dirLocal, "languages", "cdtTemp_BiasCorrect_dlgBox.xml")
    lang.dlg <- cdtLanguageParse(xml.dlg, .cdtData$Config$lang.iso)
    message <- lang.dlg[['message']]

    Insert.Messages.Out(message[['7']], TRUE, "i", GUI)

    ##################

    if(!is.null(netcdf.data$dir)){
        if(!dir.exists(netcdf.data$dir)){
            msg <- paste("Folder containing the netCDF data does not exist", ":", netcdf.data$dir)
            Insert.Messages.Out(msg, TRUE, "e", GUI)
            return(NULL)
        }else{
            ncdata_pars <- list(dir = "", format = "tmax_down_%s%s%s.nc",
                                varid = "temp", ilon = 1, ilat = 2)
            netcdf.data <- init.default.list.args(netcdf.data, ncdata_pars)
        }
    }else{
        Insert.Messages.Out("No folder containing the netCDF data provided", TRUE, "e", GUI)
        return(NULL)
    }

    #######
    biasmthd_pars <- list(method = "mbvar", dir = "")
    bias.method <- init.default.list.args(bias.method, biasmthd_pars)

    #######
    output_pars <- list(dir = "", format = "tmax_adj_%s%s%s.nc")
    output <- init.default.list.args(output, output_pars)

    ##################

    if(is.null(dates$from)) dates$from <- "range"

    if(dates$from %in% c("file", "dates")){
        if(dates$from == "file"){
            if(!file.exists(dates$pars$file)){
                msg <- paste("File containing the date does not exist", ":", dates$pars$file)
                Insert.Messages.Out(msg, TRUE, "e", GUI)
                return(NULL)
            }
            daty <- utils::read.table(dates$pars$file, stringsAsFactors = FALSE, colClasses = "character")
            daty <- daty[, 1]

            dirBias <- paste0('ADJUSTED_Temp_Data_',
                             tools::file_path_sans_ext(basename(dates$pars$file)))
        }else{
            daty <- dates$pars$dates
            if(is.null(daty)){
                Insert.Messages.Out("No vector dates", TRUE, "e", GUI)
                return(NULL)
            }

            dirBias <- paste('ADJUSTED_Temp_Data', daty[1], daty[length(daty)], sep = '_')
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

        dirBias <- paste('ADJUSTED_Temp_Data', xdeb, xfin, sep = '_')
        ncInfo <- ncInfo.with.date.range(netcdf.data, date.range, time.step)
    }

    if(is.null(ncInfo)){
        Insert.Messages.Out(message[['9']], TRUE, "e", GUI)
        return(NULL)
    }

    outdir <- file.path(output$dir, dirBias)
    dir.create(outdir, showWarnings = FALSE, recursive = TRUE)

    ##################
    ## Get RFE data info

    varid <- netcdf.data$varid
    nc <- ncdf4::nc_open(ncInfo$ncfiles[ncInfo$exist][1])
    lon <- nc$var[[varid]]$dim[[netcdf.data$ilon]]$vals
    lat <- nc$var[[varid]]$dim[[netcdf.data$ilat]]$vals
    varinfo <- nc$var[[varid]][c('name', 'prec', 'units', 'longname', 'missval')]
    ncdf4::nc_close(nc)

    xo <- order(lon)
    lon <- lon[xo]
    yo <- order(lat)
    lat <- lat[yo]
    ncInfo$ncinfo <- list(varid = varid, lon = lon, lat = lat,
                          ilon = netcdf.data$ilon, ilat = netcdf.data$ilat,
                          xo = xo, yo = yo, varinfo = varinfo)

    ##################
    ## READ BIAS FILSES

    params <- list(period = time.step, BIAS = bias.method)
    BIAS <- readBiasFiles(params = params, variable = "temp", GUI)
    if(is.null(BIAS)) return(NULL)

    bbox1 <- sapply(ncInfo$ncinfo[c("lon", "lat")], range)
    bbox2 <- sapply(BIAS[c("lon", "lat")], range)
    btx <- max(bbox1[1, 1], bbox2[1, 1])
    bty <- max(bbox1[1, 2], bbox2[1, 2])
    upx <- min(bbox1[2, 1], bbox2[2, 1])
    upy <- min(bbox1[2, 2], bbox2[2, 2])

    if(btx >= upx | bty >= upy){
        Insert.Messages.Out(message[['10']], TRUE, "e")
        return(NULL)
    }

    ##################

    params <- list(period = time.step, BIAS = bias.method, output = output)
    ret <- applyBiasCorrection(BIAS, ncInfo, outdir, params = params, variable = "temp", GUI = GUI)

    if(!is.null(ret)){
        if(ret != 0){
          Insert.Messages.Out(message[['6']], TRUE, "e", GUI)
        }
    }else{
        Insert.Messages.Out(message[['6']], TRUE, "e", GUI)
        return(NULL)
    }

    Insert.Messages.Out(message[['5']], TRUE, "s", GUI)

    return(0)
}
