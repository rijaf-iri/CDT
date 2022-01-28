
CPT.convertProcs <- function(){
    Insert.Messages.Out(.cdtData$GalParams[['message']][['6']], TRUE, "i")

    cptInfo <- list(name = .cdtData$GalParams$cptinfo$name,
                    units = .cdtData$GalParams$cptinfo$units,
                    missval = .cdtData$GalParams$cptinfo$missval)

    if(.cdtData$GalParams$data.type == "cdtstation"){
        cdtdata <- getStnOpenData(.cdtData$GalParams$cdtstation)
        if(is.null(cdtdata)) return(NULL)

        ret <- CPT.convertStationData.Files(cdtdata, .cdtData$GalParams$output, cptInfo)
    }

    if(.cdtData$GalParams$data.type == "cdtnetcdf"){
        ncDataInfo <- getNCDFSampleData(.cdtData$GalParams$cdtnetcdf$sample)
        if(is.null(ncDataInfo)){
            Insert.Messages.Out(.cdtData$GalParams[['message']][['7']], TRUE, "e")
            return(NULL)
        }

        ncInfo <- c(.cdtData$GalParams$cdtnetcdf[c('dir', 'format')],
                    ncDataInfo[c('ilon', 'ilat', 'varid')])
        ret <- CPT.convertGridData.Files(ncInfo, .cdtData$GalParams$output, cptInfo)
        if(is.null(ret)){
            Insert.Messages.Out(.cdtData$GalParams[['message']][['8']], TRUE, "e")
        }
    }

    return(ret)
}


#' Conversion to CPT data format.
#'
#' Function to correct station data or netCDF files to CPT data format.
#' 
#' @param data.type character, the type of the data. Available options: \code{"cdtstation"} or \code{"cdtnetcdf"}.
#' @param station.data named list, providing the station data to convert in CDT format.
#' \itemize{
#' \item{\code{file}: }{character, full path to the file containing the stations data}
#' \item{\code{sep}: }{character, column separator of the data}
#' \item{\code{na.strings}: }{character, missing values flag}
#' }
#' @param netcdf.data named list, providing the input netCDF dataset to convert.
#' \itemize{
#' \item{\code{dir}: }{character, full path to the directory containing the netCDF files.}
#' \item{\code{format}: }{character, format of the netCDF file names.\cr
#' Replace the year, month, dekad, pentad or day by Year: \code{%Y}; Month: \code{%M}; Dekad: \code{%T}; Pentad: \code{%P}; Day: \code{%D}.\cr
#' Example: \cr
#' \code{outTS_1981-07_1982-06.nc => outTS_%Y-%M_%Y-%M.nc} \cr
#' \code{onset_19830901.nc  => onset_%Y%M%D.nc}
#' }
#' \item{\code{varid}: }{character, name of the variable to read from the netCDF data}
#' \item{\code{ilon}: }{integer, order for the longitude dimension of the variable. 
#' Example: if the variable "precip" has the dimension order [Lat, Lon] then \code{ilon} must be 2}
#' \item{\code{ilat}: }{integer, order for the latitude dimension of the variable.}
#' }
#' @param cpt.varinfo named list, information about the field of the output CPT file.
#' \itemize{
#' \item{\code{name}: }{character, the field name}
#' \item{\code{units}: }{character, the units}
#' \item{\code{missval}: }{character, missing values flag}
#' }
#' @param output.file character, full path to the file to save the CPT data format. 
#' @param GUI logical, indicating whether or not the output message should be displayed on CDT GUI. If \code{TRUE}, CDT GUI must be open.
#' 
#' @export

convert2CPTFormat <- function(data.type = "cdtstation",
                            station.data = list(file = "", sep = ",", na.strings = "-99"),
                            netcdf.data = list(dir = "", format = "onset_%Y%M%D.nc",
                                               varid = "onset", ilon = 1, ilat = 2),
                            cpt.varinfo =  list(name = "onset", units = "days since",
                                                missval = "-9999"),
                            output.file = "", GUI = FALSE)
{
    cdtLocalConfigData()
    xml.dlg <- file.path(.cdtDir$dirLocal, "languages", "cdtCPTconversion_dlgBox.xml")
    lang.dlg <- cdtLanguageParse(xml.dlg, .cdtData$Config$lang.iso)
    message <- lang.dlg[['message']]

    Insert.Messages.Out(message [['6']], TRUE, "i", GUI)

    cptinfo <- list(name = "onset", units = "days since", missval = "-9999")
    cpt.varinfo <- init.default.list.args(cpt.varinfo, cptinfo)

    if(data.type == "cdtstation"){
        if(!is.null(station.data$file)){
            if(!file.exists(station.data$file)){
                msg <- paste(message [['9']], ":", station.data$file)
                Insert.Messages.Out(msg, TRUE, "e", GUI)
                return(NULL)
            }else{
                stndata_pars <- list(file = "", sep = ",", na.strings = "-99")
                station.data <- init.default.list.args(station.data, stndata_pars)
            }
        }else{
            Insert.Messages.Out(message [['1']], TRUE, "e", GUI)
            return(NULL)
        }

        stnpars_read <- list(stringsAsFactors = FALSE, colClasses = "character")
        cdtdata <- do.call(read.table, c(station.data, stnpars_read))
        ret <- CPT.convertStationData.Files(cdtdata, output.file, cpt.varinfo)
    }

    if(data.type == "cdtnetcdf"){
        if(!is.null(netcdf.data$dir)){
            if(!dir.exists(netcdf.data$dir)){
                msg <- paste(message [['10']], ":", netcdf.data$dir)
                Insert.Messages.Out(msg, TRUE, "e", GUI)
                return(NULL)
            }else{
                ncdata_pars <- list(dir = "", format = "onset_%Y%M%D.nc",
                                    varid = "onset", ilon = 1, ilat = 2)
                netcdf.data <- init.default.list.args(netcdf.data, ncdata_pars)
            }
        }else{
            Insert.Messages.Out(message [['10']], TRUE, "e", GUI)
            return(NULL)
        }

        ret <- CPT.convertGridData.Files(netcdf.data, output.file, cpt.varinfo)
        if(is.null(ret))
            Insert.Messages.Out(message[['8']], TRUE, "e", GUI)
    }

    if(!is.null(ret)){
        if(ret != 0)
          Insert.Messages.Out(message[['5']], TRUE, "e", GUI)
    }else{
        Insert.Messages.Out(message[['5']], TRUE, "e", GUI)
        return(-1)
    }

    Insert.Messages.Out(message[['4']], TRUE, "s", GUI)

    return(0)
}
