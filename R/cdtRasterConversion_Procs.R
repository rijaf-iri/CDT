
rasterData.convert_Proc <- function(){
    Insert.Messages.Out(.cdtData$GalParams[['message']][['5']], TRUE, "i")

    regex.in <- switch(.cdtData$GalParams$type.in,
                       "nc" = c("*\\.nc$", "\\.nc$"),
                       "tif" = c("*\\.tif$|*\\.tiff$", "\\.tif$|\\.tiff$"),
                       "bil" = c("*\\.bil$", "\\.bil$"))
    sub.out <- switch(.cdtData$GalParams$type.out, "nc" = ".nc", "tif" = ".tif", "bil" = ".bil")
    format.out <- switch(.cdtData$GalParams$type.out, "nc" = "CDF", "tif" = "GTiff", "bil" = "EHdr")

    files.in <- list.files(.cdtData$GalParams$dir.in, regex.in[1])
    if(length(files.in) == 0){
        Insert.Messages.Out(paste(.cdtData$GalParams[['message']][['6']], .cdtData$GalParams$type.in), format = TRUE)
        return(NULL)
    }
    files.out <- gsub(regex.in[2], sub.out, files.in)

    ret <- lapply(seq_along(files.in), function(jj){
        path.in <- file.path(.cdtData$GalParams$dir.in, files.in[jj])
        if(.cdtData$GalParams$type.in == "bil"){
            path.hdr <- gsub("\\.bil$", ".hdr", path.in)
            if(!file.exists(path.hdr)) return(NULL)
        }

        don.raster <- try(raster::raster(path.in), silent = TRUE)
        if(inherits(don.raster, "try-error")) return(NULL)

        raster::crs(don.raster) <- NA
        raster::crs(don.raster) <- sp::CRS("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0")

        don.raster[is.nan(don.raster) | is.infinite(don.raster)] <- NA
        if(.cdtData$GalParams$type.out == "nc")
            don.raster[don.raster == .cdtData$GalParams$nc.opts$missval] <- NA

        path.out <- file.path(.cdtData$GalParams$dir.out, files.out[jj])
        write.args <- list(x = don.raster, filename = path.out, format = format.out, overwrite = TRUE)
        if(.cdtData$GalParams$type.out == "nc")
            write.args <- c(write.args, c(.cdtData$GalParams$nc.opts[names(.cdtData$GalParams$nc.opts) != "missval"], list(compression = 9)))

        res <- do.call(raster::writeRaster, write.args)
        return(0)
    })

    return(0)
}
