
write_distrParms_QMECDF <- function(coefData, outdir, lang.msg, GUI){
    Insert.Messages.Out(lang.msg[['8']], TRUE, "i", GUI)

    coords <- coefData$data[c('lon_box', 'lat_box')]
    nlon <- length(coords$lon_box)
    nlat <- length(coords$lat_box)

    biasdir <- file.path(outdir, "BIAS_DATA")
    dir.create(biasdir, showWarnings = FALSE, recursive = TRUE)

    for(jj in 1:12){
        stn_pars <- coefData$ecdf_fun$stn[[jj]]
        grd_pars <- coefData$ecdf_fun$grd[[jj]]
        if(is.null(stn_pars) | is.null(grd_pars)) next

        stn_miss <- sapply(stn_pars, function(x) is.null(x))
        grd_miss <- sapply(grd_pars, function(x) is.null(x))
        if(all(stn_miss) | all(grd_miss)) next

        dim(stn_pars) <- c(nlon, nlat)
        stn_out <- list(lon = coords$lon_box, lat = coords$lat_box, data = stn_pars)
        stn_file <- file.path(biasdir, paste0("STN_ecdf_", jj, ".rds"))
        saveRDS(stn_out, stn_file)

        dim(grd_pars) <- c(nlon, nlat)
        grd_out <- list(lon = coords$lon_box, lat = coords$lat_box, data = grd_pars)

        grd_file <- file.path(biasdir, paste0("GRD_ecdf_", jj, ".rds"))
        saveRDS(grd_out, grd_file)
    }

    ##########

    voisin <- biascoeff.getOption("maxBoxNeighbor")
    boxGrid <- as.matrix(expand.grid(coords))
    gcoords <- coefData$data[c('lon', 'lat')]
    newGrid <- as.matrix(expand.grid(gcoords))
    dst <- fields::rdist(boxGrid, newGrid)
    ord <- apply(dst, 2, order)
    dst <- apply(dst, 2, sort)
    ord <- ord[1:voisin, , drop = FALSE]
    dst <- dst[1:voisin, , drop = FALSE] / (2 * coefData$data$diag)
    wk <- exp(-(dst / 0.33)^2)

    out_pars <- coefData["params"]
    out_pars$interp <- list(lon = gcoords$lon, lat = gcoords$lat, id = ord, wk = wk)

    parsFile <- file.path(biasdir, 'input_parameters.rds')
    saveRDS(out_pars, parsFile)

    Insert.Messages.Out(lang.msg[['9']], TRUE, "s", GUI)

    return(0)
}
