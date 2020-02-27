
## Climatology index
cdt.index.Climatologies <- function(dates, tstep = "daily", xwin = 0){
    monval <- as.numeric(substr(dates, 5, 6))
    if(tstep == 'daily'){
        endmon <- c(31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31)
        vtimes <- cbind(unlist(sapply(endmon, function(j) 1:j)), rep(1:12, endmon), 1:365)
        xdaty <- paste(as.numeric(substr(dates, 7, 8)), monval, sep = '_')
        xvtm <- paste(vtimes[, 1], vtimes[, 2], sep = '_')
        times.stn <- vtimes[match(xdaty, xvtm), 3]
        times.stn[is.na(times.stn)] <- 59

        ## sliding windows xwin
        ixmovwin <- lapply(unique(times.stn), function(nt){
            ix1 <- which(times.stn == nt)
            ix1 <- c(sapply(ix1, function(x) x + (-xwin:xwin)))
            cbind(nt, ix1[ix1 > 0 & ix1 <= length(dates)])
        })
        ixmovwin <- do.call(rbind, ixmovwin)
        times.stn <- ixmovwin[, 1]
        idx.stn <- ixmovwin[, 2]
    }

    if(tstep == 'pentad'){
        vtimes <- cbind(expand.grid(1:6, 1:12), 1:72)
        xdaty <- paste(as.numeric(substr(dates, 7, 7)), monval, sep = '_')
        xvtm <- paste(vtimes[, 1], vtimes[, 2], sep = '_')
        times.stn <- vtimes[match(xdaty, xvtm), 3]
        idx.stn <- seq_along(dates)
    }

    if(tstep == 'dekadal'){
        vtimes <- cbind(expand.grid(1:3, 1:12), 1:36)
        xdaty <- paste(as.numeric(substr(dates, 7, 7)), monval, sep = '_')
        xvtm <- paste(vtimes[, 1], vtimes[, 2], sep = '_')
        times.stn <- vtimes[match(xdaty, xvtm), 3]
        idx.stn <- seq_along(dates)
    }

    if(tstep == 'monthly'){
        times.stn <- monval
        idx.stn <- seq_along(dates)
    }

    if(tstep %in% c('seasonal', 'annual')){
        times.stn <- rep(1, length(dates))
        idx.stn <- seq_along(dates)
    }

    index <- split(idx.stn, times.stn)
    return(list(id = as.numeric(names(index)), index = index))
}

#############################################

## Anomaly index
cdt.index.Anomalies <- function(dates, index.clim, tstep = "daily")
{
    index <- cdt.index.Climatologies(dates, tstep, 0)
    ixx <- match(index$id, index.clim$id)
    ixx <- ixx[!is.na(ixx)]

    if(length(ixx) >= 12){
        index$id <- index$id[ixx]
        index$index <- index$index[ixx]
    }

    index1 <- lapply(seq_along(index$id), function(j){
        cbind(index$id[j], index$index[[j]], ixx[j])
    })

    index1 <- do.call(rbind, index1)
    index1 <- index1[order(index1[, 2]), , drop = FALSE]
    dates <- dates[index1[, 2]]
    return(list(date = dates, index = index1))
}

#############################################

## Flexible seasonal index
## startSeas: YYYYMMDD
## endSeas: YYYYMMDD
## Ex: 
## startSeas <- c("20030201", "20040201", "20050201", "20060201")
## endSeas <- c("20030430", "20040630", "20050530", "20061030")

cdt.index.flexseason <- function(startSeas, endSeas, dates, inTimestep)
{
    len.start <- length(startSeas)
    odaty <- paste0(startSeas, "_", endSeas)

    idx <- vector(mode = "list", length = len.start)
    ina <- is.na(startSeas) | is.na(endSeas)
    idx[ina] <- NA
    startSeas <- startSeas[!ina]
    endSeas <- endSeas[!ina]
    odaty[ina] <- NA
    if(length(startSeas) == 0){
        idx <- lapply(idx, function(x) x[!is.na(x)])
        return(list(index = idx,
                    date = odaty,
                    nb0 = rep(1, len.start),
                    nba = rep(0, len.start)
                )
            )
    }

    if(inTimestep == "daily"){
        start.seas <- as.numeric(as.character(startSeas))
        end.seas <- as.numeric(as.character(endSeas))
    }
    if(inTimestep == "pentad"){
        start.seas1 <- as.numeric(substr(startSeas, 1, 6))
        end.seas1 <- as.numeric(substr(endSeas, 1, 6))
        start.seas2 <- as.numeric(substr(startSeas, 7, 8))
        end.seas2 <- as.numeric(substr(endSeas, 7, 8))
        start.seas2 <- findInterval(start.seas2, c(1, 5, 10, 15, 20, 25, 31), rightmost.closed = TRUE, left.open = TRUE)
        end.seas2 <- findInterval(end.seas2, c(1, 5, 10, 15, 20, 25, 31), rightmost.closed = TRUE, left.open = TRUE)
        start.seas <- as.numeric(paste0(start.seas1, start.seas2))
        end.seas <- as.numeric(paste0(end.seas1, end.seas2))
    }
    if(inTimestep == "dekadal"){
        start.seas1 <- as.numeric(substr(startSeas, 1, 6))
        end.seas1 <- as.numeric(substr(endSeas, 1, 6))
        start.seas2 <- as.numeric(substr(startSeas, 7, 8))
        end.seas2 <- as.numeric(substr(endSeas, 7, 8))
        start.seas2 <- findInterval(start.seas2, c(1, 10, 20, 31), rightmost.closed = TRUE, left.open = TRUE)
        end.seas2 <- findInterval(end.seas2, c(1, 10, 20, 31), rightmost.closed = TRUE, left.open = TRUE)
        start.seas <- as.numeric(paste0(start.seas1, start.seas2))
        end.seas <- as.numeric(paste0(end.seas1, end.seas2))
    }
    if(inTimestep == "monthly"){
        start.seas <- as.numeric(substr(startSeas, 1, 6))
        end.seas <- as.numeric(substr(endSeas, 1, 6))
    }

    ##################

    if(inTimestep == 'monthly'){
        dtmp <- range(as.Date(paste0(dates, '01'), '%Y%m%d'), na.rm = TRUE)
        pastemps <- 'month'
    }else{
        dtmp <- range(as.Date(dates, '%Y%m%d'), na.rm = TRUE)
        pastemps <- 'day'
    }
    cdates <- seq(dtmp[1], dtmp[2], pastemps)
    ystart <- seq(as.Date(paste0(format(cdates[1], '%Y'), '-1-1')), cdates[1], pastemps)
    ystart <- ystart[-length(ystart)]
    yend <- seq(cdates[length(cdates)], as.Date(paste0(format(cdates[length(cdates)], '%Y'), '-12-31')), pastemps)
    yend <- yend[-1]
    if(length(ystart) > 0) cdates <- c(ystart, cdates)
    if(length(yend) > 0) cdates <- c(cdates, yend)
    if(inTimestep == 'daily') cdates <- format(cdates, "%Y%m%d")
    if(inTimestep == 'pentad'){
        pen <- as.numeric(format(cdates, '%d'))
        cdates <- paste0(format(cdates, "%Y%m")[pen <= 6], pen[pen <= 6])
    }
    if(inTimestep == 'dekadal'){
        dek <- as.numeric(format(cdates, '%d'))
        cdates <- paste0(format(cdates, "%Y%m")[dek <= 3], dek[dek <= 3])
    }
    if(inTimestep == 'monthly') cdates <- format(cdates, "%Y%m")

    idrow <- seq_along(dates)
    idrow <- idrow[match(cdates, dates)]

    ##################

    cdates <- as.numeric(as.character(cdates))
    istart <- match(start.seas, cdates)
    iend <- match(end.seas, cdates)

    idrow <- lapply(seq_along(istart), function(j) {
        if(!is.na(iend[j])) idrow[istart[j]:iend[j]] else NA
    })
    idx[!ina] <- idrow
    nbd0 <- unname(sapply(idx, length))
    idx <- lapply(idx, function(x) x[!is.na(x)])
    nbda <- unname(sapply(idx, length))
    list(index = idx, date = odaty, nb0 = nbd0, nba = nbda)
}

#############################################

## Time at the beginning
## 2018-01-01 00:00 will represent the average or total from
## 2018-01-01 00:00 to (2018-01-01 00:00)+out.step

cdt.index.min2min <- function(times, out.step){
    ttn <- as.numeric(substr(times, 11, 12))
    ttn <- floor(ttn / out.step) * out.step
    ttn <- str_pad(ttn, 2, pad = "0")
    index <- split(seq_along(ttn), paste0(substr(times, 1, 10), ttn))

    return(index)
}

## Time at the end
## 2018-01-01 00:00 will represent the average or total from
## (2018-01-01 00:00)-out.step to 2018-01-01 00:00
cdt.index.min2min.end <- function(times, out.step){
    ttn <- as.numeric(substr(times, 11, 12))
    ttn <- ttn - 1/60
    ttn <- floor(ttn / out.step) * out.step
    ttn <- (ttn + out.step) %% 60
    ttn <- str_pad(ttn, 2, pad = "0")
    trle <- rle(ttn)
    ie <- cumsum(trle$lengths)
    is <- c(1, (ie + 1)[-length(ie)])
    index <- lapply(seq_along(is), function(j) is[j]:ie[j])

    tth <- times[ie]
    step <- !substr(tth, 11, 12) %in% unique(trle$values)
    if(any(step)){
        td <- as.POSIXct(tth[step], tz = "UTC", format = "%Y%m%d%H%M")
        tth[step] <- paste0(format(td + out.step * 60, "%Y%m%d%H"), trle$values[step])
    }
    names(index) <- tth

    return(index)
}

##############

cdt.index.hour2hour <- function(times, out.step){
    ttn <- as.numeric(substr(times, 9, 10))
    ttn <- floor(ttn / out.step) * out.step
    ttn <- str_pad(ttn, 2, pad = "0")
    index <- split(seq_along(ttn), paste0(substr(times, 1, 8), ttn))

    return(index)
}

cdt.index.hour2hour.end <- function(times, out.step){
    ttn <- as.numeric(substr(times, 9, 10))
    ttn <- ttn - 1/60
    ttn <- floor(ttn / out.step) * out.step
    ttn <- (ttn + out.step) %% 24
    ttn <- str_pad(ttn, 2, pad = "0")
    trle <- rle(ttn)
    ie <- cumsum(trle$lengths)
    is <- c(1, (ie + 1)[-length(ie)])
    index <- lapply(seq_along(is), function(j) is[j]:ie[j])

    tth <- times[ie]
    step <- !substr(tth, 9, 10) %in% unique(trle$values)
    if(any(step)){
        td <- as.POSIXct(tth[step], tz = "UTC", format = "%Y%m%d%H")
        tth[step] <- paste0(format(td + out.step * 3600, "%Y%m%d"), trle$values[step])
    }
    names(index) <- tth

    return(index)
}

##############

cdt.index.min2hour <- function(times, out.step){
    index <- split(seq_along(times), substr(times, 1, 10))
    if(out.step > 1){
        idx <- cdt.index.hour2hour(names(index), out.step)
        index <- lapply(idx, function(j) unlist(index[j], use.names = FALSE))
    }

    return(index)
}

cdt.index.min2hour.end <- function(times, out.step){
    ttn <- as.POSIXct(times, tz = "UTC", format = "%Y%m%d%H%M")
    ttn <- ttn - 1
    ttn <- format(ttn, "%Y%m%d%H%M")
    index <- split(seq_along(ttn), substr(ttn, 1, 10))

    tth <- names(index)
    tth <- as.POSIXct(tth, tz = "UTC", format = "%Y%m%d%H")
    tth <- format(tth + 3600, "%Y%m%d%H")
    names(index) <- tth

    if(out.step > 1){
        idx <- cdt.index.hour2hour.end(tth, out.step)
        index <- lapply(idx, function(j) unlist(index[j], use.names = FALSE))
    }

    return(index)
}

##############

cdt.index.minhr2daily <- function(times, instep, obs.hour){
    format <- switch(instep, 'minute' = "%Y%m%d%H%M", 'hourly' = "%Y%m%d%H")
    ttn <- as.POSIXct(times, tz = "UTC", format = format)
    ttn <- ttn  - 3600 * obs.hour
    ttn <- format(ttn, format)
    index <- split(seq_along(ttn), substr(ttn, 1, 8))

    return(index)
}

cdt.index.minhr2daily.end <- function(times, instep, obs.hour){
    format <- switch(instep, 'minute' = "%Y%m%d%H%M", 'hourly' = "%Y%m%d%H")
    ttn <- as.POSIXct(times, tz = "UTC", format = format)
    ttn <- (ttn - 1) - 3600 * obs.hour
    ttn <- format(ttn, format)
    index <- split(seq_along(ttn), substr(ttn, 1, 8))

    return(index)
}

##############

cdt.index.daily2pentad <- function(dates){
    yymm <- substr(dates, 1, 6)
    jour <- as.numeric(substr(dates, 7, 8))
    jour <- cut(jour, c(1, 5, 10, 15, 20, 25, 31), labels = FALSE, include.lowest = TRUE)
    index <- split(seq_along(dates), paste0(yymm, jour))

    return(index)
}

##############

cdt.index.daypen2dekad <- function(dates, instep){
    yymm <- substr(dates, 1, 6)
    pars <- switch(instep,
                   "daily" = list(8, c(1, 10, 20, 31)),
                   "pentad" = list(7, c(1, 2, 4, 6))
                  )
    x <- as.numeric(substr(dates, 7, pars[[1]]))
    x <- cut(x, pars[[2]], labels = FALSE, include.lowest = TRUE)
    index <- split(seq_along(dates), paste0(yymm, x))

    return(index)
}

##############

cdt.index.rollSeasonal <- function(dates, seas.len){
    index <- split(seq_along(dates), substr(dates, 1, 6))
    odaty <- names(index)
    dtmp <- range(as.Date(paste0(odaty, "01"), '%Y%m%d'), na.rm = TRUE)
    cdates <- seq(dtmp[1], dtmp[2], "month")
    ystart <- seq(as.Date(paste0(format(cdates[1], '%Y'), '-1-1')), cdates[1], "month")
    ystart <- ystart[-length(ystart)]
    last <- as.Date(paste0(format(cdates[length(cdates)], '%Y'), '-12-31'))
    yend <- seq(cdates[length(cdates)], last, "month")
    yend <- yend[-1]
    if(length(ystart) > 0) cdates <- c(ystart, cdates)
    if(length(yend) > 0) cdates <- c(cdates, yend)
    cdates <- format(cdates, "%Y%m")
    idrow <- seq_along(odaty)
    idrow <- idrow[match(cdates, odaty)]

    roll.mon <- rle(cdates)
    ij <- seq_along(roll.mon$values)
    ij <- ij[ij <= (length(ij) - seas.len + 1)]
    ij <- lapply(ij, function(i) i:(i + seas.len - 1))
    ij <- lapply(ij, function(j) which(cdates %in% roll.mon$values[j]))
    idx <- utils::relist(idrow[unlist(ij)], ij)
    idx <- lapply(idx, function(x) x[!is.na(x)])

    cdates <- do.call(rbind, lapply(ij, function(j) cdates[j][c(1, seas.len)]))
    ix <- sapply(idx, length) > 0
    cdates <- cdates[ix, , drop = FALSE]
    idx <- idx[ix]
    index <- lapply(idx, function(j) unlist(index[j], use.names = FALSE))

    year1 <- substr(cdates[, 1], 1, 4)
    mon1 <- substr(cdates[, 1], 5, 6)
    year2 <- substr(cdates[, 2], 1, 4)
    mon2 <- substr(cdates[, 2], 5, 6)
    odaty <- paste0(year1, "-", mon1, "_", year2, "-", mon2)
    names(index) <- odaty
    return(index)
}

#############################################

## Time series aggregation
cdt.index.aggregate <- function(dates, tstep.in = c("minute", "hourly", "daily", "pentad", "dekadal", "monthly"),
                                tstep.out = c("minute", "hourly", "daily", "pentad", "dekadal", "monthly", "annual", "seasonal", "roll.seas"),
                                inMinHour = 5, outMinHour = 30, obs.hour = 0, seasonLength = 3, startMonth = 1)
{
    tstep.in <- tstep.in[1]
    tstep.out <- tstep.out[1]

    if(tstep.out == "minute"){
        index <- cdt.index.min2min(dates, outMinHour)
        nbd0 <- rep(outMinHour/inMinHour, length(index))
    }

    if(tstep.out == "hourly"){
        fun <- switch(tstep.in, 'minute' = cdt.index.min2hour, 'hourly' = cdt.index.hour2hour)
        fac <- switch(tstep.in, 'minute' = 60, 'hourly' = 1)
        index <- fun(dates, outMinHour)
        nbd0 <- fac * outMinHour/inMinHour
    }

    if(tstep.out == "daily"){
        index <- cdt.index.minhr2daily(dates, tstep.in, obs.hour)
        fac <- switch(tstep.in, 'minute' = 60, 'hourly' = 1)
        nbd0 <- rep(fac * 24/inMinHour, length(index))
    }

    if(tstep.out == "pentad"){
        index <- cdt.index.daily2pentad(dates)
        nbd0 <- nb.Day.Of.Pentad(names(index))
    }

    if(tstep.out == "dekadal"){
        index <- cdt.index.daypen2dekad(dates, tstep.in)
        nbd0 <- switch(tstep.in,
                        "daily" = nb.Day.Of.Dekad(names(index)),
                        "pentad" = rep(2, length(index))
                      )
    }

    if(tstep.out == "monthly"){
        index <- split(seq_along(dates), substr(dates, 1, 6))
        nbd0 <- switch(tstep.in,
                        "daily" = nb.Day.Of.Month(names(index)),
                        "pentad" = rep(6, length(index)),
                        "dekadal" = rep(3, length(index))
                      )
    }

    if(tstep.out == "annual"){
        index <- split(seq_along(dates), substr(dates, 1, 4))
        nbd0 <- switch(tstep.in,
                        "daily" = nb.Day.Of.Year(names(index)),
                        "pentad" = rep(72, length(index)),
                        "dekadal" = rep(36, length(index)),
                        "monthly" = rep(12, length(index))
                      )
    }

   if(tstep.out %in% c("seasonal", "roll.seas")){
        index <- cdt.index.rollSeasonal(dates, seasonLength)
        nbd0 <- switch(tstep.in,
                        "daily" = sapply(names(index), function(x){
                                        mon <- as.Date(paste(strsplit(x, "_")[[1]], 1, sep = '-'))
                                        mon <- format(seq(mon[1], mon[2], 'month'), "%Y%m")
                                        sum(nb.Day.Of.Month(mon))
                                    }, USE.NAMES = FALSE),
                        "pentad" = rep(6 * seasonLength, length(index)),
                        "dekadal" = rep(3 * seasonLength, length(index)),
                        "monthly" = rep(seasonLength, length(index))
                      )
        if(tstep.out == "seasonal"){
            seasMonth <- (startMonth:(startMonth + (seasonLength - 1))) %% 12
            seasMonth[seasMonth == 0] <- 12
            mon <- str_pad(seasMonth[c(1, seasonLength)], width = 2, pad = "0")
            monseas <- lapply(strsplit(names(index), "_"), strsplit, split = '-')
            mon1 <- sapply(lapply(monseas, '[[', 1), '[[', 2)
            mon2 <- sapply(lapply(monseas, '[[', 2), '[[', 2)
            ix <- mon1 == mon[1] & mon2 == mon[2]
            index <- index[ix]
            nbd0 <- nbd0[ix]
        }
    }

    odaty <- names(index)
    nbda <- unname(sapply(index, length))
    list(index = index, date = odaty, nb0 = nbd0, nba = nbda)
}

#############################################

## Extraterrestrial Radiation index and PET multipliers
cdt.index.RA.PET <- function(dates, tstep)
{
    year <- as.numeric(substr(dates, 1, 4))
    mon <- as.numeric(substr(dates, 5, 6))

    if(tstep == "daily"){
        day <- as.numeric(strftime(as.Date(dates, "%Y%m%d"), format = "%j"))
        leap <- is.leapyears(year) & day >= 60
        day[leap] <- day[leap] - 1
        idxRa <- day
        multip <- rep(1, length(day))
    }
    if(tstep == "pentad"){
        pen <- as.numeric(substr(dates, 7, 7))
        vpen <- cbind(expand.grid(1:6, 1:12), 1:72)
        vpen <- vpen[match(paste0(pen, ".", mon), paste0(vpen[, 1], ".", vpen[, 2])), 3]
        idxRa <- vpen
        end.day.mon <- as.numeric(format(as.Date(paste(year, (mon %% 12) + 1, 1, sep = '-')) - 1, "%d"))
        multip <- ifelse(pen != 6, 5, end.day.mon - 25)
    }
    if(tstep == "dekadal"){
        dek <- as.numeric(substr(dates, 7, 7))
        vdek <- cbind(expand.grid(1:3, 1:12), 1:36)
        vdek <- vdek[match(paste0(dek, ".", mon), paste0(vdek[, 1], ".", vdek[, 2])), 3]
        idxRa <- vdek
        end.day.mon <- as.numeric(format(as.Date(paste(year, (mon %% 12) + 1, 1, sep = '-')) - 1, "%d"))
        multip <- ifelse(dek != 3, 10, end.day.mon - 20)
    }
    if(tstep == "monthly"){
        idxRa <- mon
        multip <- as.numeric(format(as.Date(paste(year, (mon %% 12) + 1, 1, sep = '-')) - 1, "%d"))
    }
    list(index = idxRa, multi = multip)
}

#############################################

## Yearly index from daily data (Water balance, Onset,  Cessation, PICSA)

cdt.index.DailyYears <- function(dates, start.month, start.day){
    start.daty <- paste0(stringr::str_pad(start.month, 2, pad = '0'),
                         stringr::str_pad(start.day, 2, pad = '0'))

    dtmp <- range(as.Date(dates, '%Y%m%d'), na.rm = TRUE)
    dmonday <- format(seq(dtmp[1], dtmp[2], "day"), "%m%d")
    nmod <- length(dmonday)

    rleday1 <- rle(dmonday == start.daty)
    cumidx <- cumsum(rleday1$length)
    debidx <- cumidx[rleday1$values]
    if(debidx[1] != 1) debidx <- c(1, debidx)
    if(debidx[length(debidx)] != nmod) debidx <- c(debidx, nmod + 1)
    nbsplit <- diff(debidx)

    if(!nbsplit[1] %in% c(365, 366)){
        deb <- paste0(format(dtmp[1] - (366 - nbsplit[1]), "%Y"), start.daty)
        deb <- as.Date(deb, '%Y%m%d')
    }else deb <- dtmp[1]

    if(!nbsplit[length(nbsplit)] %in% c(365, 366)){
        fin <- format(dtmp[2] + (366 - nbsplit[length(nbsplit)]), "%Y")
        fin <- as.Date(paste0(fin, start.daty), '%Y%m%d') - 1
    }else fin <- dtmp[2]

    cdates <- format(seq(deb, fin, "day"), "%Y%m%d")
    idrow <- seq(length(dates))
    idrow <- idrow[match(cdates, dates)]

    ix <- c(which(substr(cdates, 5, 8) == start.daty), length(cdates) + 1)
    idx <- rep(seq(length(ix) - 1), ix[-1] - ix[-length(ix)])
    index <- split(idrow, idx)
    dates <- do.call(rbind, lapply(split(cdates, idx), function(x) c(x[1], x[length(x)])))
    return(list(index = index, range.date = dates))
}

#############################################

## Daily to seasonal index
cdt.index.DailySeason <- function(dates, start.mon, start.day, end.mon, end.day){
    dtmp <- range(as.Date(dates, '%Y%m%d'), na.rm = TRUE)

    cdates <- seq(dtmp[1], dtmp[2], 'day')
    ystart <- as.Date(paste0(as.numeric(format(cdates[1], '%Y')) - 1, '-', start.mon, '-', start.day))
    ystart <- seq(ystart, cdates[1], 'day')
    ystart <- ystart[-length(ystart)]
    yend <- as.Date(paste0(as.numeric(format(cdates[length(cdates)], '%Y')) + 1, '-', end.mon, '-', end.day))
    yend <- seq(cdates[length(cdates)], yend, 'day')
    yend <- yend[-1]
    if(length(ystart) > 0) cdates <- c(ystart, cdates)
    if(length(yend) > 0) cdates <- c(cdates, yend)
    cdates <- format(cdates, "%Y%m%d")

    idrow <- seq(length(dates))
    idrow <- idrow[match(cdates, dates)]

    mon <- as.numeric(substr(cdates, 5, 6))
    day <- as.numeric(substr(cdates, 7, 8))

    sstart <- which(mon == start.mon & day == start.day)
    send <- which(mon == end.mon & day == end.day)

    idx <- lapply(seq_along(sstart), function(j) idrow[sstart[j]:send[j]])
    outdates <- lapply(seq_along(sstart), function(j) c(cdates[sstart[j]], cdates[send[j]]))

    ina <- sapply(idx, function(x) isTRUE(all(is.na(x))))
    idx <- idx[!ina]
    outdates <- outdates[!ina]
    return(list(index = idx, date = outdates))
}

#############################################

## Region subdivision index (onset, cessation)
cdt.index.Region.Subdiv <- function(lat, lon = NULL, shpf = NULL, shpdiv = NULL,
                                    region.pars = list(
                                            region = "One", subdiv = "Latitude",
                                            lat = list(nb = 2, div = list(8))
                                        )
                                    )
{
    subdiv <- list(seq_along(lat))
    if(region.pars$region == "Multiple"){
        if(region.pars$subdiv == "Latitude"){
            nbreg <- region.pars$lat$nb
            latdiv <- region.pars$lat$div
            subdiv <- list()
            if(nbreg > 2){
                subdiv[[1]] <- which(lat <= latdiv[[1]])
                for(i in 1:(length(latdiv) - 1))
                    subdiv[[i + 1]] <- which(latdiv[[i + 1]] >= lat & lat > latdiv[[i]])
                subdiv[[nbreg]] <- which(lat > latdiv[[length(latdiv)]])
            }else{
                div <- lat <= latdiv[[1]]
                subdiv[[1]] <- which(div)
                subdiv[[2]] <- which(!div)
            }
        }

        if(region.pars$subdiv == "Shapefile"){
            coords <- data.frame(x = lon, y = lat)
            sp::coordinates(coords) <- ~x+y
            subdiv <- lapply(shpdiv, function(x){
                xov <- sp::over(coords, shpf[shpdiv == x, ])
                which(!is.na(xov[, 1]))
            })
        }
    }
    return(subdiv)
}

#############################################

## all nearest neighbor index: grid and points
cdt.nn.index.all <- function(grd, pts, maxdist){
    # dst <- distance.Matrix(pts, grd)
    dst <- fields::rdist(pts, grd)
    dst <- dst <= maxdist
    # dst <- split(dst, col(dst))
    dst <- split(dst, row(dst))
    Reduce("|", dst)
}

