
## Test leap year

is.leapyear <- function(year){
    leap <- year %% c(4, 100, 400)
    ((leap[1] == 0) & (leap[2] != 0)) | (leap[3] == 0)
}

is.leapyears <- function(years){
    leap <- sapply(years, function(x) x %% c(4, 100, 400))
    ((leap[1, ] == 0) & (leap[2, ] != 0)) | (leap[3, ] == 0)
}

##############################################

## Check class Date
is.date <- function(x) inherits(x, "Date")

##############################################

## Number of days
# daty: character representing the date

nb.Day.Of.Year <- function(daty){
    year <- as.numeric(substr(daty, 1, 4))
    ifelse(is.leapyears(year), 366, 365)
}

Day.Of.Month <- function(year, mon){
    rev((28:31)[!is.na(as.Date(paste(year, mon, 28:31, sep = '-')))])[1]
}

nb.Day.Of.Month <- function(daty){
    nbm <- mapply(Day.Of.Month, substr(daty, 1, 4), substr(daty, 5, 6), USE.NAMES = FALSE)
    as.numeric(nbm)
}

nb.Day.Of.Pentad <- function(daty){
    day <- as.numeric(substr(daty, 7, 7))
    nbp <- rep(5, length(daty))
    nbp[day >= 6] <- nb.Day.Of.Month(daty[day == 6]) - 25
    return(nbp)
}

nb.Day.Of.Dekad <- function(daty){
    day <- as.numeric(substr(daty, 7, 7))
    nbd <- rep(10, length(daty))
    nbd[day == 3] <- nb.Day.Of.Month(daty[day == 3]) - 20
    return(nbd)
}

##############################################

## Cycle month,
# start: month, n: number of month (DJF, start = 'December', n = 3)
# full = TRUE (July),  FALSE (Jul)
# return end month

cycleMonth <- function(start, n, full = FALSE){
    frmt <- if(full) "%B" else "%b"
    mois <- format(ISOdate(2014, 1:12, 1), frmt)
    ix <- which(mois == start)
    im <- (ix + (n - 1)) %% 12
    if(im == 0) im <- 12
    return(mois[im])
}

##############################################

## Add or subtract months
# daty: object of class "Date"

addMonths <- function(daty, n = 1){
    foo <- function(daty, n){
        date0 <- seq(daty, by = paste(n, "months"), length = 2)[2]
        date1 <- seq(as.Date(paste(format(daty, '%Y-%m'), '01', sep = '-')),
                    by = paste(n + 1, "months"), length = 2)[2] - 1
        if(date0 > date1) date1 else date0
    }
    daty <- if(length(daty) == 1) foo(daty, n) else do.call(c, lapply(daty, foo, n))
    return(daty)
}

##############################################

## Add or subtract dekads
# daty: object of class "Date"

addDekads <- function(daty, n = 1){
    foo <- function(daty, n){
        idek <- as.numeric(format(daty, '%d')) + n
        dek <- idek %% 3
        if(dek == 0) dek <- 3
        daty <- format(addMonths(daty, floor((idek - 1) / 3)), '%Y-%m')
        as.Date(paste(daty, dek, sep = '-'))
    }
    daty <- if(length(daty) == 1) foo(daty, n) else do.call(c, lapply(daty, foo, n))
    return(daty)
}

##############################################

## Add or subtract pentads
# daty: object of class "Date"

addPentads <- function(daty, n = 1){
    foo <- function(daty, n){
        ipen <- as.numeric(format(daty, '%d')) + n
        pen <- ipen %% 6
        if(pen == 0) pen <- 6
        daty <- format(addMonths(daty, floor((ipen - 1) / 6)), '%Y-%m')
        as.Date(paste(daty, pen, sep = '-'))
    }
    daty <- if(length(daty) == 1) foo(daty, n) else do.call(c, lapply(daty, foo, n))
    return(daty)
}

##############################################

format.plot.date <- function(dates, tstep){
    if(tstep == "daily")
        daty <- as.Date(dates, "%Y%m%d")
    if(tstep == "pentad"){
        pen <- c(1, 6, 11, 16, 21, 26)[as.numeric(substr(dates, 7, 7))]
        daty <- as.Date(paste0(substr(dates, 1, 6), pen), "%Y%m%d")
    }
    if(tstep == "dekadal"){
        dek <- c(1, 11, 21)[as.numeric(substr(dates, 7, 7))]
        daty <- as.Date(paste0(substr(dates, 1, 6), dek), "%Y%m%d")
    }
    if(tstep == "monthly")
        daty <- as.Date(paste0(dates, 1), "%Y%m%d")
    return(daty)
}

format.plot.date.label <- function(x, tstep){
    if(is.character(x)){
        daty <- switch(tstep,
                    "daily" = as.Date(x, "%Y%m%d"),
                    "pentad" = local({
                        pen <- c(1, 6, 11, 16, 21, 26)[as.numeric(substr(x, 7, 7))]
                        as.Date(paste0(substr(x, 1, 6), pen), "%Y%m%d")
                    }) ,
                    "dekadal" = local({
                        dek <- c(1, 11, 21)[as.numeric(substr(x, 7, 7))]
                        as.Date(paste0(substr(x, 1, 6), dek), "%Y%m%d")
                    }),
                    "monthly" = as.Date(paste0(x, 1), "%Y%m%d"))
    }
    if(is.numeric(x))
        daty <- as.Date(x, origin = '1970-1-1')
    if(is.date(x)) daty <- x

    #######
    if(tstep == 'daily')
        labdates <- format(daty, '%d-%b-%Y')

    if(tstep %in% c('pentad', 'dekadal')){
        brks <- if(tstep == 'dekadal') c(1, 10, 20, 31) else c(1, 5, 10, 15, 20, 25, 31)
        day <- as.numeric(format(daty, '%d'))
        pdk <- findInterval(day, brks, rightmost.closed = TRUE, left.open = TRUE)
        labdates <- paste0(pdk, '-', format(daty, '%b-%Y'))
    }

    if(tstep == 'monthly')
        labdates <- format(daty,'%b-%Y')

    return(labdates)
}

##############################################

get.date.time.cdt.station <- function(dates, tstep){
    format <- switch(tstep,
                     "minute" = "%Y%m%d%H%M",
                     "hourly" = "%Y%m%d%H",
                                "%Y%m%d"
                    )
    if(tstep == "monthly") dates <- paste0(dates, "15")
    dates <- as.POSIXct(dates, tz = "UTC", format = format)
    if(tstep %in% c("daily", "pentad", "dekadal", "monthly"))
        dates <- as.Date(dates)
    return(dates)
}

##############################################

get.date.time.range.cdt.station <- function(dates, tstep){
    format <- switch(tstep,
                     "minute" = "%Y-%m-%d-%H-%M",
                     "hourly" = "%Y-%m-%d-%H",
                                "%Y-%m-%d")
    tmp <- c('year', 'mon', 'dek', 'pen', 'day', 'hour', 'min')
    dates <- get.date.time.cdt.station(dates, tstep)
    rgDate <- format(range(dates, na.rm = TRUE), format)
    date.range <- as.numeric(unlist(strsplit(rgDate, "-")))
    date.range <- as.list(date.range)
    idx <- switch(tstep,
                  "minute" = c(1:2, 5:7),
                  "hourly" = c(1:2, 5:6),
                  "daily" = c(1:2, 5),
                  "pentad" = c(1:2, 4),
                  "dekadal" = 1:3,
                  "monthly" = c(1:2, 5))
    names(date.range) <- c(paste0('start.', tmp[idx]), paste0('end.', tmp[idx]))
    return(date.range)
}

##############################################

rename_date.range <- function(date.range){
    rename <- function(x){
        nm <- names(x)
        ix <- !nm %in% 'year'
        nm[ix] <- substr(nm[ix], 1, 3)
        return(nm)
    }
    names(date.range$start) <- rename(date.range$start)
    names(date.range$end) <- rename(date.range$end)

    as.list(unlist(date.range))
}

split_date.range <- function(tstep, date.range){
    year <- lapply(date.range, substr, start = 1, stop = 4)
    names(year) <- paste0(names(year), ".year")
    mon <- lapply(date.range, substr, start = 5, stop = 6)
    names(mon) <- paste0(names(mon), ".mon")
    out <- c(year, mon)
    if(tstep != "monthly"){
        dd <- lapply(date.range, substr, start = 7, stop = 8)
        nm <- if(tstep == "daily") "day" else substr(tstep, 1, 3)
        names(dd) <- paste0(names(dd), ".", nm)
        out <- c(out, dd)
    }

    lapply(out, as.numeric)
}

##############################################

get.range.date.time <- function(date.range, tstep, minhour = NA){
    if(tstep %in% c("daily", "pentad", "dekadal", "monthly")){
        dekpenday <- switch(tstep,
                            "daily" = c("start.day", "end.day"),
                            "pentad" = c("start.pen", "end.pen"),
                            "dekadal" = c("start.dek", "end.dek"),
                            "monthly" = c("start.day", "end.day"))

        start <- date.range[c('start.year', 'start.mon', dekpenday[1])]
        if(tstep == "monthly") start$start.day <- 1
        start <- paste(unlist(start), collapse = "-")
        start <- as.Date(start)

        end <- date.range[c('end.year', 'end.mon', dekpenday[2])]
        if(tstep == "monthly") end$end.day <- 1
        end <- paste(unlist(end), collapse = "-")
        end <- as.Date(end)

        pas <- if(tstep == "monthly") "month" else "day"
    }

    if(tstep == "annual"){
        start <- paste0(date.range$start.year, '-1-1')
        start <- as.Date(start)
        end <- paste0(date.range$end.year, '-12-31')
        end <- as.Date(end)

        pas <- "year"
    }

    if(tstep == "hourly"){
        start <- date.range[c('start.year', 'start.mon', 'start.day', 'start.hour')]
        if(minhour > 1){
            divh <- start$start.hour %% minhour
            if(divh != 0) start$start.hour <- start$start.hour - divh
        }
        start <- paste(unlist(start), collapse = "-")
        start <- as.POSIXct(start, tz = "UTC", format = "%Y-%m-%d-%H")

        end <- date.range[c('end.year', 'end.mon', 'end.day', 'end.hour')]
        end <- paste(unlist(end), collapse = "-")
        end <- as.POSIXct(end, tz = "UTC", format = "%Y-%m-%d-%H")

        pas <- paste(minhour, "hour")
    }

    if(tstep == "minute"){
        start <- date.range[c('start.year', 'start.mon', 'start.day', 'start.hour', 'start.min')]
        divm <- start$start.min %% minhour
        if(divm != 0) start$start.min <- start$start.min - divm
        start <- paste(unlist(start), collapse = "-")
        start <- as.POSIXct(start, tz = "UTC", format = "%Y-%m-%d-%H-%M")

        end <- date.range[c('end.year', 'end.mon', 'end.day', 'end.hour', 'end.min')]
        end <- paste(unlist(end), collapse = "-")
        end <- as.POSIXct(end, tz = "UTC", format = "%Y-%m-%d-%H-%M")

        pas <- paste(minhour, "min")
    }

    list(start = start, end = end, step = pas)
}

##############################################

get.seq.date.time <- function(date.range, tstep, minhour = NA){
    daty <- get.range.date.time(date.range, tstep, minhour)
    seq(daty$start, daty$end, daty$step)
}

get.format.seq.date.time <- function(date.range, tstep, minhour = NA){
    daty <- get.seq.date.time(date.range, tstep, minhour)
    if(tstep == "minute") daty <- format(daty, "%Y%m%d%H%M")
    if(tstep == "hourly") daty <- format(daty, "%Y%m%d%H")
    if(tstep == "daily") daty <- format(daty, "%Y%m%d")
    if(tstep == "pentad"){
        pen <- as.numeric(format(daty, "%d"))
        daty <- paste0(format(daty, "%Y%m"), pen)[pen <= 6]
    }
    if(tstep == "dekadal"){
        dek <- as.numeric(format(daty, "%d"))
        daty <- paste0(format(daty, "%Y%m"), dek)[dek <= 3]
    }
    if(tstep == "monthly") daty <- format(daty, "%Y%m")
    if(tstep == "annual") daty <- format(daty, "%Y")

    return(daty)
}

##############################################

table.format.date.time <- function(tstep, date.range, minhour = NA){
    dates <- get.seq.date.time(date.range, tstep, minhour)

    if(tstep %in% c("daily", "hourly", "minute")){
        doy <- strftime(dates, format = "%j", tz = "UTC")
        format <- switch(tstep,
                         "minute" = "%Y-%m-%d-%H-%M",
                         "hourly" = "%Y-%m-%d-%H",
                         "daily" = "%Y-%m-%d"
                        )
        dates <- format(dates, format)
        dates <- do.call(rbind, strsplit(dates, "-"))
        dates <- cbind(dates, doy)
    }

    if(tstep %in% c("pentad", "dekadal")){
        dates <- format(dates, '%Y-%m-%d')
        dates <- do.call(rbind, strsplit(dates, "-"))

        n <- switch(tstep, "pentad" = 6, "dekadal" = 3)
        xx <- as.numeric(dates[, 3])
        dates <- dates[xx <= n, , drop = FALSE]
        xx <- cbind(str_pad(rep(1:12, each = n), 2, pad = "0"),
                    str_pad(rep(1:n, 12), 2, pad = "0"))
        p1 <- paste(dates[, 2], dates[, 3], sep = "-")
        p2 <- paste(xx[, 1], xx[, 2], sep = "-")
        xx <- str_pad(match(p1, p2), 2, pad = "0")
        dates <- cbind(dates[, 1:2, drop = FALSE], as.numeric(dates[, 3]), xx)
    }

    if(tstep == "monthly"){
        dates <- format(dates, '%Y-%m-%d')
        dates <- do.call(rbind, strsplit(dates, "-"))
    }

    if(tstep == "annual"){
        dates <- format(dates, '%Y-%m-%d')
        dates <- do.call(rbind, strsplit(dates, "-"))
        dates <- dates[, 1:2, drop = FALSE]
    }

    return(dates)
}

table.format.date.time1 <- function(tstep, dates){
    if(tstep %in% c("daily", "hourly", "minute")){
        format <- switch(tstep,
                         "minute" = c("%Y%m%d%H%M", "%Y-%m-%d-%H-%M"),
                         "hourly" = c("%Y%m%d%H", "%Y-%m-%d-%H"),
                         "daily" = c("%Y%m%d", "%Y-%m-%d")
                        )
        dates <- as.POSIXct(dates, tz = "UTC", format = format[1])
        dates <- dates[!is.na(dates)]
        doy <- strftime(dates, format = "%j", tz = "UTC")
        dates <- format(dates, format[2])
        dates <- do.call(rbind, strsplit(dates, "-"))
        dates <- cbind(dates, doy)
    }

    if(tstep %in% c("pentad", "dekadal")){
        dates <- as.Date(dates, '%Y%m%d')
        dates <- dates[!is.na(dates)]
        dates <- format(dates, '%Y-%m-%d')
        dates <- do.call(rbind, strsplit(dates, "-"))
        n <- switch(tstep, "pentad" = 6, "dekadal" = 3)
        xx <- as.numeric(dates[, 3])
        dates <- dates[xx <= n, , drop = FALSE]
        xx <- cbind(str_pad(rep(1:12, each = n), 2, pad = "0"),
                    str_pad(rep(1:n, 12), 2, pad = "0"))
        p1 <- paste(dates[, 2], dates[, 3], sep = "-")
        p2 <- paste(xx[, 1], xx[, 2], sep = "-")
        xx <- str_pad(match(p1, p2), 2, pad = "0")
        dates <- cbind(dates[, 1:2, drop = FALSE], as.numeric(dates[, 3]), xx)
    }

    if(tstep %in% c("monthly", "annual")){
        mmdd <- if(tstep == "monthly") '15' else '0101'
        dates <- as.Date(paste0(dates, mmdd), '%Y%m%d')
        dates <- dates[!is.na(dates)]
        dates <- format(dates, '%Y-%m-%d')
        dates <- do.call(rbind, strsplit(dates, "-"))
    }

    return(dates)
}

table.format.date.seasonal <- function(date.range, season.start, season.len){
    start <- date.range[['start.year']]
    end <- date.range[['end.year']]
    years <- start:end

    seasMonth <- (season.start:(season.start + (season.len - 1))) %% 12
    seasMonth[seasMonth == 0] <- 12

    dates <- lapply(years, function(yr){
        daty <- as.Date(paste0(yr, '-01-01'))

        seas1 <- addMonths(daty, seasMonth[1] - 1)
        seas2 <- addMonths(daty, seasMonth[season.len] - 1)
        if(seas2 < seas1){
            seas2 <- addMonths(seas2, 12)
        }

        seas1 <- format(seas1, '%Y-%m')
        seas1 <- strsplit(seas1, "-")[[1]]
        seas2 <- format(seas2, '%Y-%m')
        seas2 <- strsplit(seas2, "-")[[1]]

        c(seas1, seas2)
    })

    dates <- do.call(rbind, dates)
    dates <- cbind(dates, NA)

    return(dates)
}

##############################################

iridl.format.date <- function(tstep, date.range)
{
    mois <- c("Jan", "Feb", "Mar", "Apr", "May", "Jun",
              "Jul", "Aug", "Sep", "Oct", "Nov", "Dec")
    dates <- table.format.date.time(tstep, date.range)
    dates0 <- dates

    if(tstep == "dekadal"){
        dates[dates[, 3] == '1', 3] <- '1-10'
        dates[dates[, 3] == '2', 3] <- '11-20'
        ix3 <- dates[, 3] == '3'
        endm <- nb.Day.Of.Month(paste0(dates[ix3, 1], dates[ix3, 2]))
        dates[ix3, 3] <- paste0(21, "-", endm)
    }

    dates[, 2] <- mois[as.numeric(dates[, 2])]
    if(tstep == "monthly"){
        indate <- paste(dates[, 2], dates[, 1])
        outdate <- paste0(dates0[, 1], dates0[, 2])
    }else{
        indate <- paste(dates[, 3], dates[, 2], dates[, 1])
        outdate <- paste0(dates0[, 1], dates0[, 2], dates0[, 3])
    }

    list(dates = indate, out = outdate)
}

##############################################

iridl.seasonal.dates <- function(seasonal_dates){
    months <- c("Jan", "Feb", "Mar", "Apr", "May", "Jun",
                "Jul", "Aug", "Sep", "Oct", "Nov", "Dec")
    seas <- strsplit(seasonal_dates, "-")
    seas <- lapply(seas, trimws)
    lenS <- sapply(seas, length)
    if(all(lenS == 1)){
        seas <- do.call(c, seas)
        seas <- strsplit(seas, " ")
        seas_mon <- sapply(seas, "[[", 1)
        start_mon <- match(seas_mon, months)
        end_mon <- start_mon
        start_year <- sapply(seas, "[[", 2)
        end_year <- start_year
    }else{
        end <- sapply(seas, "[[", 2)
        end <- strsplit(end, " ")
        end_mon <- sapply(end, "[[", 1)
        end_mon <- match(end_mon, months)
        end_year <- sapply(end, "[[", 2)
        start <- sapply(seas, "[[", 1)
        start <- strsplit(start, " ")
        start <- lapply(start, function(x){
            if(length(x) == 1) c(x, NA) else x
        })
        start_mon <- sapply(start, "[[", 1)
        start_mon <- match(start_mon, months)
        start_year <- sapply(start, "[[", 2)
        ina <- is.na(start_year)
        start_year[ina] <- end_year[ina]
    }

    start <- as.Date(paste(start_year, start_mon, 1, sep = "-"))
    end <- as.Date(paste(end_year, end_mon, 1, sep = "-"))
    list(start = format(start, "%Y-%m"),
         end = format(end, "%Y-%m"))
}

##############################################

table.annuel <- function(){
    uneAnne <- seq(as.Date('2014-1-1'), by = 'day', length.out = 365)
    day <- as.numeric(format(uneAnne, "%d"))
    mon <- as.numeric(format(uneAnne, "%m"))
    vtimes <- cbind(day, mon, 1:365)
    vtimes
}

