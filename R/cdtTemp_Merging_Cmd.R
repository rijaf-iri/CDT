
#' Merging stations observation and reanalysis data.
#'
#' Function to merge stations observation and reanalysis data.
#' @param time.step the time step of the data
#' @param dates a named list providing the dates to merge.
#' A component "from" with available option "range", "file" or "dates".
#' (to be completed) 
#' @param station.data a named list (to be completed)
#' @param netcdf.data a named list (to be completed)
#' @param merge.method a named list (to be completed)
#' @param interp.method a named list (to be completed)
#' @param auxvar a named list (to be completed)
#' @param dem.data a named list (to be completed)
#' @param grid a named list (to be completed)
#' @param blank a named list (to be completed)
#' @param output a named list (to be completed)
#' @param GUI a logical indicating whether or not the output message should be displayed on CDT GUI.
#' 
#' @export

cdtMergingTempCMD <- function(time.step = "dekadal",
                              dates = list(from = "range", 
                                           range = list(start = list(year = 2018, month = 1, dekad = 1),
                                                        end = list(year = 2018, month = 12, dekad = 3)),
                                           file = "",
                                           dates = NULL),
                              station.data = list(file = "", sep = ",", na.strings = "-99"),
                              netcdf.data = list(dir = "", format = "tmax_adj_%s%s%s.nc",
                                                 varid = "temp", ilon = 1, ilat = 2),
                              merge.method = list(method = "SBA", nrun = 3, pass = c(1, 0.75, 0.5)),
                              interp.method = list(method = "idw", vargrd = FALSE, minstn = 10,
                                                   nmin = 8, nmax = 16, maxdist = 3.5, use.block = TRUE,
                                                   vgm.model = c("Sph", "Exp", "Gau", "Pen")),
                              auxvar = list(dem = FALSE, slope = FALSE, aspect = FALSE, lon = FALSE, lat = FALSE),
                              dem.data = list(file = "", varid = "dem", ilon = 1, ilat = 2),
                              grid = list(from = "data",
                                          netcdf = list(file = "", varid = "precip", ilon = 1, ilat = 2),
                                          bbox = list(minlon = 42, maxlon = 52, minlat = -26, maxlat = -11,
                                                      reslon = 0.1, reslat = 0.1)),
                              blank = list(data = FALSE, shapefile = ""),
                              output = list(dir = "", format = "tmax_mrg_%s%s%s.nc"),
                              GUI = FALSE)
{
    cdtLocalConfigData()
    xml.dlg <- file.path(.cdtDir$dirLocal, "languages", "cdtTemp_Merging_dlgBox.xml")
    lang.dlg <- cdtLanguageParse(xml.dlg, .cdtData$Config$lang.iso)
    message <- lang.dlg[['message']]

    Insert.Messages.Out(message[['10']], TRUE, "i", GUI)

    ##################

    if(is.null(dates$from)) dates$from <- "range"

    if(dates$from %in% c("file", "dates")){
        if(dates$from == "file"){
            if(!file.exists(dates$file)){
                msg <- paste("File containing the date does not exist", ":", dates$file)
                Insert.Messages.Out(msg, TRUE, "e", GUI)
                return(NULL)
            }
            daty <- read.table(dates$file, stringsAsFactors = FALSE, colClasses = "character")
            daty <- daty[, 1]

            dirMrg <- paste0('Merged_Temp_Data_',
                             tools::file_path_sans_ext(basename(dates$file)))
        }else{
            daty <- dates$dates
            if(is.null(daty)){
                Insert.Messages.Out("No vector dates", TRUE, "e", GUI)
                return(NULL)
            }
 
            dirMrg <- paste('Merged_Temp_Data', daty[1], daty[length(daty)], sep = '_')
        }

        ncInfo <- ncInfo.from.date.vector(netcdf.data, daty, time.step)
    }else{
        date.range <- rename_date.range(dates$range)
        daty <- get.range.date.time(date.range, time.step)
        if(time.step == 'monthly'){
            xdeb <- format(daty$start, "%b%Y")
            xfin <- format(daty$end, "%b%Y")
        }else{
            xdeb <- paste0(as.numeric(format(daty$start, "%d")), format(daty$start, "%b%Y"))
            xfin <- paste0(as.numeric(format(daty$end, "%d")), format(daty$end, "%b%Y"))
        }

        dirMrg <- paste('Merged_Temp_Data', xdeb, xfin, sep = '_')
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

    stnData <- do.call(read.table, c(station.data, list(stringsAsFactors = FALSE, colClasses = "character")))
    stnData <- splitCDTData0(stnData, GUI)
    if(is.null(stnData)) return(NULL)

    ##################
    ## Get NetCDF data info

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
        grdInfo <- grid$bbox
        grd.lon <- seq(grdInfo$minlon, grdInfo$maxlon, grdInfo$reslon)
        grd.lat <- seq(grdInfo$minlat, grdInfo$maxlat, grdInfo$reslat)
    }

    if(grid$from == "ncdf"){
        if(!file.exists(grid$netcdf$file)){
            Insert.Messages.Out(message[['14']], TRUE, "e", GUI)
            return(NULL)
        }
        nc <- nc_open(grid$netcdf$file)
        lon <- nc$var[[grid$netcdf$varid]]$dim[[grid$netcdf$ilon]]$vals
        lat <- nc$var[[grid$netcdf$varid]]$dim[[grid$netcdf$ilat]]$vals
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
        dsn <- dirname(blank$shapefile)
        layer <- tools::file_path_sans_ext(basename(blank$shapefile))
        shpd <- rgdal::readOGR(dsn, layer)
        proj4string(shpd) <- CRS(as.character(NA))
        outMask <- create.mask.grid(shpd, xy.grid)
    }

    Insert.Messages.Out(message[['16']], TRUE, "s", GUI)

    ##################

    Insert.Messages.Out(message[['17']], TRUE, "i", GUI)

    RnoR <- list(use = FALSE, wet = 1.0, smooth = FALSE)
    params <- list(period = time.step, MRG = merge.method,
                   interp = interp.method, output = output,
                   auxvar = auxvar, RnoR = RnoR)

    ret <- cdtMerging(stnData = stnData, ncInfo = ncInfo,
                      xy.grid = xy.grid, params = params,
                      variable = "temp", demData = demData,
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