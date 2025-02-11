
cdtBiasCoefficients <- function(stnData, ncInfo, demData, params, variable, outdir, GUI = TRUE){
    xml.dlg <- file.path(.cdtDir$dirLocal, "languages", "cdtBias_Coefficient_functions1.xml")
    lang.msg <- cdtLanguageParse(xml.dlg, .cdtData$Config$lang.iso)
    lang.msg <- lang.msg[['message']]

    if(params$BIAS$method %in% c("mbvar", "mbmon")){
        coefData <- biasCoefficients_MBIAS(stnData, ncInfo, params, variable, lang.msg, GUI)
        interp_biasCoeff_MBIAS(coefData, demData, outdir, variable, lang.msg, GUI)
    }

    if(params$BIAS$method == "qmdist"){
        if(params$BIAS$ts.support == "points"){
            coefData <- biasCoefficients_QMDIST_PTS(stnData, ncInfo, params, outdir, lang.msg, GUI)
            if(is.null(coefData)) return(NULL)
            interp_distrParms_QMDIST(coefData, demData, outdir, lang.msg, GUI)
        }else{
            coefData <- biasCoefficients_QMDIST_BOX(stnData, ncInfo, params, lang.msg, GUI)
            write_distrParms_QMDIST(coefData, outdir, lang.msg, GUI)
        }
    }

    if(params$BIAS$method == "qmecdf"){
        coefData <- biasCoefficients_QMECDF(stnData, ncInfo, params, lang.msg, GUI)
        write_distrParms_QMECDF(coefData, outdir, lang.msg, GUI)
    }

    coefData$variable <- variable
    saveRDS(coefData, file = file.path(outdir, "BIAS_PARAMS_DATA.rds"))

    return(0)
}

cdtBiasCoefficientsWind <- function(stnData, ncInfo, demData, params, outdir, GUI = TRUE){
    xml.dlg <- file.path(.cdtDir$dirLocal, "languages", "cdtBias_Coefficient_functions1.xml")
    lang.msg <- cdtLanguageParse(xml.dlg, .cdtData$Config$lang.iso)
    lang.msg <- lang.msg[['message']]

    if(params$BIAS$method %in% c("mbvar", "mbmon")){
        coefData <- biasCoefficients_MBIAS_wind(stnData, ncInfo, params, lang.msg, GUI)
        interp_biasCoeff_MBIAS_wind(coefData, demData, outdir, lang.msg, GUI)
    }

    if(params$BIAS$method == "qmdist"){
        if(params$BIAS$ts.support == "points"){
            coefData <- biasCoefficients_QMDIST_PTS_wind(stnData, ncInfo, params, outdir, lang.msg, GUI)
            if(is.null(coefData)) return(NULL)
            interp_distrParms_QMDIST_wind(coefData, demData, outdir, lang.msg, GUI)
        }else{
            coefData <- biasCoefficients_QMDIST_BOX_wind(stnData, ncInfo, params, lang.msg, GUI)
            write_distrParms_QMDIST_wind(coefData, outdir, lang.msg, GUI)
        }
    }

    if(params$BIAS$method == "qmecdf"){
        coefData <- biasCoefficients_QMECDF_wind(stnData, ncInfo, params, lang.msg, GUI)
        write_distrParms_QMECDF_wind(coefData, outdir, lang.msg, GUI)
    }

    coefData$variable <- "wind"
    saveRDS(coefData, file = file.path(outdir, "BIAS_PARAMS_DATA.rds"))

    return(0)
}

get_biasInterp_params <- function(stnCoords, grdCoords, demData, parsInterp){
    ixs <- NULL
    elvNA <- NULL
    bGrd <- NULL
    nlon <- length(grdCoords$lon)
    nlat <- length(grdCoords$lat)

    if(parsInterp$method %in% c("idw", "okr", "nns")){
        interp.grid <- defSpatialPixels(grdCoords)
        locations.pts <- as.data.frame(stnCoords)
        sp::coordinates(locations.pts) <- ~lon+lat
    }

    if(parsInterp$method == "nn3d"){
        interp.grid <- expand.grid(grdCoords)
        interp.grid <- data.frame(interp.grid, elv = c(demData$z))
        locations.pts <- as.data.frame(stnCoords)

        dem1 <- demData[c('x', 'y')]
        names(dem1) <- c('lon', 'lat')
        ijx <- grid2pointINDEX(stnCoords, dem1)
        locations.pts$elv <- demData$z[ijx]

        elvNA <- is.na(locations.pts$elv)
        locations.pts <- locations.pts[!elvNA, ]

        ixs <- cdt.nn.index.all(interp.grid[, 1:2, drop = FALSE],
                                locations.pts[, 1:2, drop = FALSE],
                                parsInterp$maxdist)
        mngrd <- sapply(interp.grid, function(x){
            c(min(x, na.rm = TRUE), max(x, na.rm = TRUE))
        })
        interp.grid <- scale(interp.grid, center = mngrd[1, ], scale = mngrd[2, ] - mngrd[1, ])
        # interp.grid <- scale(interp.grid)
        interp.grid <- as.data.frame(interp.grid)
        mnpts <- sapply(locations.pts, function(x){
            c(min(x, na.rm = TRUE), max(x, na.rm = TRUE))
        })
        locations.pts <- scale(locations.pts, center = mnpts[1, ], scale = mnpts[2, ] - mnpts[1, ])
        # locations.pts <- scale(locations.pts)
        locations.pts <- as.data.frame(locations.pts)
    }

    if(parsInterp$method %in% c("idw", "okr")){
        if(parsInterp$use.block){
            bGrd <- switch(biascoeff.getOption("blockType"),
                           "userdefined" = createBlock(biascoeff.getOption("blockSize")),
                           "gaussian" = biascoeff.getOption("blockSize")
                          )
        }
    }

    list(grid = interp.grid, points = locations.pts, nlon = nlon,
         nlat = nlat, block = bGrd, index = ixs, elvNA = elvNA, 
         pars = parsInterp, addcoarse = biascoeff.getOption("addCoarseGrid"),
         savecoarse = biascoeff.getOption("saveCoarseGrid"))
}
