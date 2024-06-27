
rain_no_rain.mask_log <- function(locations.stn, newgrid, nmax)
{
    glm.binom <- tryCatch(
            stats::glm(rnr.stn ~ grd, data = locations.stn, family = stats::binomial(link = "logit")),
            error=function(e) e, warning=function(w) w
        )
    if(inherits(glm.binom, "warning") | inherits(glm.binom, "error")) return(NULL)

    rnr <- NULL
    if(!is.na(glm.binom$coef[2])){
        locations.stn$rnr.res <- stats::residuals(glm.binom)
        rnr.trend <- stats::predict(glm.binom, newdata = newgrid, type = 'link')

        rnr.res.grd <- gstat::krige(rnr.res~1, locations = locations.stn, newdata = newgrid,
                                    nmax = nmax, set = list(idp = 4.0), debug.level = 0)

        rnr <- rnr.trend + rnr.res.grd$var1.pred
        rnr <- exp(rnr) / (1 + exp(rnr))

        rnr[is.na(rnr)] <- 1
    }

    return(rnr)
}

rain_no_rain.mask_add <- function(locations.stn, newgrid, nmax)
{
    locations.stn$rnr.res <- locations.stn$rnr.stn - locations.stn$rnr.grd
    rnr.trend <- newgrid$rnr.grd

    rnr.res.grd <- gstat::krige(rnr.res~1, locations = locations.stn, newdata = newgrid,
                                nmax = nmax, set = list(idp = 4.0), debug.level = 0)

    rnr <- rnr.trend + rnr.res.grd$var1.pred
    rnr[rnr < 0] <- 0
    rnr[rnr > 1] <- 1

    rnr[is.na(rnr)] <- 1

    return(rnr)
}

rain_no_rain.cut_off <- function(rnr, RnoRCutOff){
    if(RnoRCutOff == 1){
        rnr[rnr >= 0.5] <- 1
        rnr[rnr < 0.5] <- 0
    }else if(RnoRCutOff == 2){
        rnr[rnr < 0.1] <- 0
    }else{
        rnr[rnr < 0.25] <- 0
        rnr[rnr > 0.75] <- 1
    }
  
    return(rnr)
}
