
## Hargreaves evapotranspiration
Ref.ET.Hargreaves <- function(Tmax, Tmin, Ra, tstep = "daily", Precip = NULL){
    TM <- (Tmax + Tmin) / 2
    TR <- Tmax - Tmin
    TR[TR < 0] <- 0
    if(is.null(Precip)){
        # Original Hargreaves equation
        coefs <- if(tstep == "daily") c(0.0028, 19.1869) else c(0.0023, 17.8)
        ETP <- coefs[1] * (0.408 * Ra) * (TM + coefs[2]) * TR^0.5
    }else{
        # Hargreaves modified method
        coefs <- if(tstep == "daily") c(0.0019, 21.0584, 0.0874, 0.6278) else c(0.0013, 17, 0.0123, 0.76)
        PRE <- (TR - coefs[3] * Precip)^coefs[4]
        PRE[is.nan(PRE)] <- 0
        ETP <- coefs[1] * (0.408 * Ra) * (TM + coefs[2]) * PRE
    }
    # mm/day
    return(ETP)
}

Ref.ET.Hargreaves.FAO <- function(Tmax, Tmin, Ra){
    TM <- (Tmax + Tmin) / 2
    TR <- Tmax - Tmin
    TR[TR < 0] <- 0

    0.0023 * (0.408 * Ra) * (TM + 17.8) * TR^0.5
}
