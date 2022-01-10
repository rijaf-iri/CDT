
summarizeDataProcs <- function(GeneralParameters){
    intstep <- GeneralParameters$intstep
    message <- .cdtData$EnvData$message
    if(GeneralParameters$data.type == 'cdtstation')
        input.file <- GeneralParameters$cdtstation$file
    else
        input.file <- GeneralParameters$cdtdataset$index

    if(input.file %in% c("", "NA")){
        Insert.Messages.Out(message[['5']], TRUE, 'e')
        return(NULL)
    }

    if(!dir.exists(GeneralParameters$outdir)){
        Insert.Messages.Out(message[['6']], TRUE, 'e')
        Insert.Messages.Out(paste(message[['7']], getwd()))
        GeneralParameters$outdir <- getwd()
    }

    #####################
    outDIR <- file.path(GeneralParameters$outdir, "SUMMARY_Statistics")
    dir.create(outDIR, showWarnings = FALSE, recursive = TRUE)
    out.dat.index <- gzfile(file.path(outDIR, "Summary.rds"), compression = 7)

    #####################
    if(GeneralParameters$data.type == "cdtstation"){
        don <- getStnOpenData(input.file)
        if(is.null(don)) return(NULL)
        don <- getCDTdataAndDisplayMsg(don, intstep, input.file)
        if(is.null(don)) return(NULL)

        moy <- colMeans(don$data, na.rm = TRUE)
        moy[is.nan(moy) | is.infinite(moy)] <- NA
        nx <- nx_ny_as.image(diff(range(don$lon)))
        ny <- nx_ny_as.image(diff(range(don$lat)))
        moy <- cdt.as.image(moy, nx = nx, ny = ny, pts.xy = cbind(don$lon, don$lat))

        index <- split(seq_along(don$dates), substr(don$dates, 5, 6))

        output <- NULL
        output <- list(params = GeneralParameters,
                       data = don[c('id', 'lon', 'lat', 'dates', 'data')],
                       map = moy[c('x', 'y', 'z')],
                       index = index)
    }

    if(GeneralParameters$data.type == "cdtdataset"){
        don <- try(readRDS(input.file), silent = TRUE)
        if(inherits(don, "try-error")){
            Insert.Messages.Out(paste(message[['8']], input.file), TRUE, 'e')
            return(NULL)
        }
        if(intstep != don$TimeStep){
            Insert.Messages.Out(paste(message[['9']], intstep), TRUE, 'e')
            return(NULL)
        }

        ##########
        chunkfile <- sort(unique(don$colInfo$index))
        chunkcalc <- split(chunkfile, ceiling(chunkfile / don$chunkfac))
        cdtParallelCond <- .cdtData$Config$parallel

        ##########
        do.parChunk <- if(don$chunkfac > length(chunkcalc)) TRUE else FALSE
        do.parCALC <- if(do.parChunk) FALSE else TRUE
        parsL <- doparallel.cond(do.parCALC & (length(chunkcalc) > 10))

        moy <- cdt.foreach(seq_along(chunkcalc), parsL, GUI = TRUE, FUN = function(jj)
        {
            don.data <- readCdtDatasetChunk.sequence(chunkcalc[[jj]], input.file, cdtParallelCond, do.par = do.parChunk)
            don.data <- don.data[don$dateInfo$index, , drop = FALSE]
            moy <- colMeans(don.data, na.rm = TRUE)
            moy[is.nan(moy) | is.infinite(moy)] <- NA
            return(moy)
        })

        moy <- do.call(c, moy)
        moy <- moy[don$colInfo$order]
        dim(moy) <- sapply(don$coords$mat, length)
        moy <- c(don$coords$mat, list(z = moy))

        index <- split(seq_along(don$dateInfo$date), substr(don$dateInfo$date, 5, 6))

        output <- NULL
        output <- list(params = GeneralParameters, data = don,
                       index.file = input.file, map = moy,
                       index = index)
    }

    saveRDS(output, out.dat.index)
    close(out.dat.index)
    .cdtData$EnvData$output <- output
    .cdtData$EnvData$PathSum <- outDIR

    rm(moy, output, don)
    gc()

    return(0)
}
