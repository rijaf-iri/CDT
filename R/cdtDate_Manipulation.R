
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

#' @exportS3Method NULL
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

#' @exportS3Method NULL
format.plot.date.label <- function(x, tstep){
    if(is.character(x)){
        daty <- switch(tstep,
                    "minute" = as.POSIXct(x, format = "%Y%m%d%H%M", tz = "UTC"),
                    "hourly" = as.POSIXct(x, format = "%Y%m%d%H", tz = "UTC"),
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
    }else if(methods::is(x, "Date") | methods::is(x, "POSIXct")){
        daty <- x
    }else if(is.numeric(x)){
        if(tstep %in% c('minute', 'hourly')){
            daty <- as.POSIXct(x, origin = '1970-1-1', tz = 'UTC')
        }else{
            daty <- as.Date(x, origin = '1970-1-1')
        }
    }

    #######
    if(tstep %in% c('minute', 'hourly'))
        labdates <- format(daty, '%Y-%m-%d %H:%M:%S')

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

#' @exportS3Method NULL
format.datetime.cdtstation <- function(dates, tstep){
    switch(tstep,
           "minute" = as.POSIXct(dates, format = "%Y%m%d%H%M", tz = "UTC"),
           "hourly" = as.POSIXct(dates, format = "%Y%m%d%H", tz = "UTC"),
           "daily" = as.Date(dates, "%Y%m%d"),
           "pentad" = local({
                        seqtime <- as.Date(dates, "%Y%m%d")
                        pen <- c(1, 6, 11, 16, 21, 26)[as.numeric(format(seqtime, "%d"))]
                        as.Date(paste0(format(seqtime, "%Y-%m-"), pen))
                    }),
           "dekadal" = local({
                        seqtime <- as.Date(dates, "%Y%m%d")
                        dek <- c(1, 11, 21)[as.numeric(format(seqtime, "%d"))]
                        as.Date(paste0(format(seqtime, "%Y-%m-"), dek))
                    }),
           "monthly" = as.Date(paste0(dates, "01"), "%Y%m%d"),
           "yearly" = as.Date(paste0(dates, "0101"), "%Y%m%d"),
           "annual" = as.Date(paste0(dates, "0101"), "%Y%m%d")
        )
}

get.datetime.cdtstation <- function(dates, tstep){
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

get.range.datetime.cdtstation <- function(dates, tstep){
    dates <- get.datetime.cdtstation(dates, tstep)
    get.range.datetime(dates, tstep)
}

get.range.datetime <- function(dates, tstep){
    format <- switch(tstep,
                     "minute" = "%Y-%m-%d-%H-%M",
                     "hourly" = "%Y-%m-%d-%H",
                                "%Y-%m-%d")
    tmp <- c('year', 'mon', 'dek', 'pen', 'day', 'hour', 'min')
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

#' @exportS3Method NULL
format.datetime.2character <- function(dates, tstep){
    switch(tstep,
           "minute" = format(dates, "%Y%m%d%H%M"),
           "hourly" = format(dates, "%Y%m%d%H"),
           "daily" = format(dates, "%Y%m%d"),
           "pentad" = paste0(format(dates, "%Y%m"), as.integer(format(dates, '%d'))),
           "dekadal" = paste0(format(dates, "%Y%m"), as.integer(format(dates, '%d'))),
           "monthly" = format(dates, "%Y%m"),
           "yearly" = format(dates, "%Y"),
           "annual" = format(dates, "%Y")
        )
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

# date.range = list(start = "201801010000", end = "201812312355")
split_date.range <- function(tstep, date.range){
    year <- lapply(date.range, substr, start = 1, stop = 4)
    names(year) <- paste0(names(year), ".year")
    mon <- lapply(date.range, substr, start = 5, stop = 6)
    names(mon) <- paste0(names(mon), ".mon")
    out <- c(year, mon)
    if(tstep != "monthly"){
        dd <- lapply(date.range, substr, start = 7, stop = 8)
        nm <- if(tstep %in% c("pentad", "dekadal")) substr(tstep, 1, 3) else "day"
        names(dd) <- paste0(names(dd), ".", nm)
        out <- c(out, dd)
        if(tstep %in% c("minute", "hourly")){
            hh <- lapply(date.range, substr, start = 9, stop = 10)
            names(hh) <- paste0(names(hh), ".hour")
            out <- c(out, hh)
            if(tstep == "minute"){
                mm <- lapply(date.range, substr, start = 11, stop = 12)
                names(mm) <- paste0(names(mm), ".min")
                out <- c(out, mm)
            }
        }
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

get.file.date.time <- function(date.range, tstep, minhour = NA, GUI = TRUE){
    dates0 <- readLines(date.range$path.file, warn = FALSE, skipNul = TRUE)
    format_file_datetime(dates0, tstep, minhour, GUI)
}

format_file_datetime <- function(dates0, tstep, minhour, GUI){
    dates <- trimws(dates0)
    dates <- gsub('\"', '', dates)
    id <- dates != ""
    dates <- dates[id]
    dates0 <- dates0[id]
    dates <- gsub('[^[:digit:]]', '', dates)
    dates <- trimws(dates)
    id <- dates == ""
    if(all(id)){
        msg <- "Invalid list of dates"
        Insert.Messages.Out(msg, TRUE, "e", GUI)
        return(NULL)
    }
    if(any(id)){
        msg <- paste0(dates0[id], collapse = ", ")
        msg <- paste0("Invalid dates:\n", msg)
        Insert.Messages.Out(msg, TRUE, "w", GUI)
        dates <- dates[!id]
        dates0 <- dates0[!id]
    }

    year <- substr(dates, 1, 4)

    if(tstep == "daily"){
        mon <- substr(dates, 5, 6)
        day <- substr(dates, 7, 8)
        daty <- paste(year, mon, day, sep = '-')
        daty <- as.Date(daty)
    }

    if(tstep %in% c("pentad", "dekadal")){
        mon <- substr(dates, 5, 6)
        pd <- substr(dates, 7, 8)
        if(any(nchar(pd) == 2)){
            msg <- paste("Invalid", tstep, "dates")
            Insert.Messages.Out(msg, TRUE, "e", GUI)
            return(NULL)
        }

        k <- if(tstep == "pentad") 6 else 3
        if(any(!as.integer(pd) %in% 1:k)){
            msg <- paste("Invalid", tstep, "dates")
            Insert.Messages.Out(msg, TRUE, "e", GUI)
            return(NULL)
        }
        daty <- paste(year, mon, pd, sep = '-')
        daty <- as.Date(daty)
    }

    if(tstep == "monthly"){
        mon <- substr(dates, 5, 6)
        daty <- paste(year, mon, '01', sep = '-')
        daty <- as.Date(daty)
    }

    if(tstep == "annual"){
        daty <- paste0(year, '-01-01')
        daty <- as.Date(daty)
    }

    if(tstep == "hourly"){
        mon <- substr(dates, 5, 6)
        day <- substr(dates, 7, 8)
        hour <- substr(dates, 9, 10)

        if(minhour > 1){
            divh <- as.numeric(hour) %% minhour
            if(any(divh) > 0){
                hour[divh > 0] <- NA
            }
        }

        daty <- paste(year, mon, day, hour, sep = '-')
        daty <- as.POSIXct(daty, tz = "UTC", format = "%Y-%m-%d-%H")
    }

    if(tstep == "minute"){
        mon <- substr(dates, 5, 6)
        day <- substr(dates, 7, 8)
        hour <- substr(dates, 9, 10)
        min <- substr(dates, 11, 12)

        divm <- as.numeric(min) %% minhour
        if(any(divm > 0)){
            min[divm > 0] <- NA
        }

        daty <- paste(year, mon, day, hour, min, sep = '-')
        daty <- as.POSIXct(daty, tz = "UTC", format = "%Y-%m-%d-%H-%M")
    }

    if(all(is.na(daty))){
        msg <- "Invalid list of dates"
        Insert.Messages.Out(msg, TRUE, "e", GUI)
        return(NULL)
    }

    if(any(is.na(daty))){
        msg <- paste0(dates0[is.na(daty)], collapse = ", ")
        msg <- paste0("Invalid dates:\n", msg)
        Insert.Messages.Out(msg, TRUE, "w", GUI)
        daty <- daty[!is.na(daty)]
    }

    return(daty)
}

##############################################

divisible.start.end.time <- function(x, minhour){
    div <- x %% minhour
    if(div != 0) x <- x - div

    return(x)
}

hourly.start.end.time <- function(date.range, minhour = NA){
    if(is.na(minhour)){
        date.range$start.hour <- divisible.start.end.time(date.range$start.hour, minhour)
        date.range$end.hour <- divisible.start.end.time(date.range$end.hour, minhour)
    }

    start <- date.range[paste0('start.', c('year', 'mon', 'day', 'hour'))]
    start <- paste(unlist(start), collapse = "-")
    start <- as.POSIXct(start, tz = "UTC", format = "%Y-%m-%d-%H")
    end <- date.range[paste0('end.', c('year', 'mon', 'day', 'hour'))]
    end <- paste(unlist(end), collapse = "-")
    as.POSIXct(end, tz = "UTC", format = "%Y-%m-%d-%H")

    list(start = start, end = end)
}

#' @exportS3Method NULL
seq.format.date.time <- function(tstep, date.range, minhour = NA){
    if(is.null(date.range$from.file)){
        dates <- get.seq.date.time(date.range, tstep, minhour)
    }else{
        if(date.range$from.file){
            dates <- get.file.date.time(date.range, tstep, minhour)
        }else{
            dates <- get.seq.date.time(date.range, tstep, minhour)
        }
    }

    return(dates)
}

table.format.date.time <- function(tstep, date.range, minhour = NA){
    dates <- seq.format.date.time(tstep, date.range, minhour)
    if(is.null(dates)) return(NULL)

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
        xx <- cbind(sprintf('%02d', rep(1:12, each = n)),
                    sprintf('%02d', rep(1:n, 12)))
        p1 <- paste(dates[, 2], dates[, 3], sep = "-")
        p2 <- paste(xx[, 1], xx[, 2], sep = "-")
        xx <- sprintf('%02d', match(p1, p2))
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
        xx <- cbind(sprintf('%02d', rep(1:12, each = n)),
                    sprintf('%02d', rep(1:n, 12)))
        p1 <- paste(dates[, 2], dates[, 3], sep = "-")
        p2 <- paste(xx[, 1], xx[, 2], sep = "-")
        xx <- sprintf('%02d', match(p1, p2))
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

iridl.format.date <- function(tstep, date.range){
    months <- c("Jan", "Feb", "Mar", "Apr", "May", "Jun",
                "Jul", "Aug", "Sep", "Oct", "Nov", "Dec")
    dates <- table.format.date.time(tstep, date.range)
    if(is.null(dates)) return(NULL)
    dates0 <- dates

    if(tstep == "dekadal"){
        dates[dates[, 3] == '1', 3] <- '1-10'
        dates[dates[, 3] == '2', 3] <- '11-20'
        ix3 <- dates[, 3] == '3'
        endm <- nb.Day.Of.Month(paste0(dates[ix3, 1], dates[ix3, 2]))
        dates[ix3, 3] <- paste0(21, "-", endm)
    }

    dates[, 2] <- months[as.numeric(dates[, 2])]
    if(tstep == "monthly"){
        indate <- paste(dates[, 2], dates[, 1])
        outdate <- paste0(dates0[, 1], dates0[, 2])
    }else{
        indate <- paste(dates[, 3], dates[, 2], dates[, 1])
        outdate <- paste0(dates0[, 1], dates0[, 2], dates0[, 3])
    }

    list(dates = indate, out = outdate)
}

iridl.get.end_date <- function(url, timestep){
    months <- c('Jan', 'Feb', 'Mar', 'Apr', 'May', 'Jun',
                'Jul', 'Aug', 'Sep', 'Oct', 'Nov', 'Dec')
    tmp <- rvest::read_html(url)
    tmp <- rvest::html_nodes(tmp, ".infodivinfo")
    val <- rvest::html_nodes(tmp, "dl dd")
    val <- rvest::html_text(val)
    txt <- rvest::html_nodes(tmp, "dl dt")
    txt <- sapply(txt, function(x){
        y <- rvest::html_node(x, 'em')
        rvest::html_text(y)
    })
    ix <- !is.na(txt) & (txt == "Time")
    end_d <- sub(".*\\((.+)\\).*", "\\1", val[ix])
    end_d <- trimws(strsplit(end_d, " ")[[1]])
    end_date <- switch(timestep,
        "hourly" = local({
            yy <- end_d[4]
            mo <- which(months == end_d[3])
            mo <- sprintf('%02d', mo)
            dy <- as.numeric(end_d[2])
            dy <- sprintf('%02d', dy)
            hr <- trimws(strsplit(end_d[1], '-')[[1]])
            hr <- substr(hr[1], 1, 2)
            paste0(yy, mo, dy, hr)
        }),
        "daily" = local({
            yy <- end_d[3]
            mo <- which(months == end_d[2])
            mo <- sprintf('%02d', mo)
            dy <- as.numeric(end_d[1])
            dy <- sprintf('%02d', dy)
            paste0(yy, mo, dy)
        }),
        "dekadal" = local({
            yy <- end_d[3]
            mo <- which(months == end_d[2])
            mo <- sprintf('%02d', mo)
            dk <- trimws(strsplit(end_d[1], '-')[[1]])
            dk <- switch(dk[1], '1' = 1, '11' = 2, '21' = 3)
            paste0(yy, mo, dk)
        }),
        "monthly" = local({
            yy <- end_d[2]
            mo <- which(months == end_d[1])
            dd <- as.Date(paste(yy, mo, 1, sep = '-'))
            # dd <- addMonths(dd, -1)
            format(dd, '%Y%m')
        })
      )

    return(end_date)
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

