
format_wcoords_msg <- function(list_data, input_file,
                               title_msg, head_data)
{
    msg <- lapply(list_data, function(x){
        n <- nrow(x)
        xx <- rep(NA, n)
        for(i in seq(n)){
            xx[i] <- paste(input_file[[x[i, 1]]],
                     paste0(x[i, -1], collapse = '\t'),
                     sep = '\t')
        }
        paste0(xx, collapse = '\n')
    })
    msg <- paste0(unlist(msg), collapse = "\n\n")
    dhd <- paste0(head_data, collapse = '\t')
    msg <- paste(title_msg, '\n', dhd, msg, sep = "\n")

    return(msg)
}

mergeCDTStationData <- function(){
    GalParams <- .cdtData$GalParams
    Insert.Messages.Out(GalParams[['message']][['1']], TRUE, "i")

    don <- lapply(GalParams$inputs, getStnOpenData)
    inull <- sapply(don, is.null)
    if(any(inull)){
        msg <- paste0(unlist(GalParams$inputs[inull]), collapse = ", ")
        msg <- paste(GalParams[['message']][['4-1']], ":", msg)
        Insert.Messages.Out(msg, TRUE, "e")
        return(NULL)
    }

    don <- lapply(don, splitCDTData0)
    inull <- sapply(don, is.null)
    if(any(inull)){
        msg <- paste0(unlist(GalParams$inputs[inull]), collapse = ", ")
        msg <- paste(GalParams[['message']][['4-2']], ":", msg)
        Insert.Messages.Out(msg, TRUE, "e")
        return(NULL)
    }

    donInfo <- getStnOpenDataInfo(GalParams$inputs[[1]])

    lenDates <- lapply(don, function(x){
        n <- nchar(x$dates)
        unique(n)
    })

    wdate <- sapply(lenDates, length) > 1
    if(any(wdate)){
        msg <- paste0(unlist(GalParams$inputs[wdate]), collapse = ", ")
        msg <- paste(GalParams[['message']][['12']], ":", msg)
        Insert.Messages.Out(msg, TRUE, "e")
        return(NULL)
    }

    ldate <- length(unique(unlist(lenDates)))
    if(ldate > 1){
        Insert.Messages.Out(GalParams[['message']][['13']], TRUE, "e")
        return(NULL)
    }

    ##########

    crds <- lapply(seq_along(don), function(j){
        n <- length(don[[j]]$id)
        elv <- don[[j]]$elv
        if(is.null(elv)) elv <- rep(NA, n)
        data.frame(file = rep(j, n),
                   id = don[[j]]$id,
                   lon = don[[j]]$lon,
                   lat = don[[j]]$lat,
                   elv = elv)
    })
    crds <- do.call(rbind, crds)

    ### missing coords
    inaC <- is.na(crds$lon) | is.na(crds$lat)
    miss_cords_msg <- NULL
    miss_cords <- NULL
    if(any(inaC)){
        miss_cords1 <- crds[inaC, , drop = FALSE]
        crds <- crds[!inaC, , drop = FALSE]

        ims <- crds$id %in% miss_cords1$id
        miss_cords2 <- crds[ims, , drop = FALSE]
        crds <- crds[!ims, , drop = FALSE]

        miss_cords <- rbind(miss_cords1, miss_cords2)
        im <- split(seq(nrow(miss_cords)), miss_cords$id)
        miss_cords <- lapply(im, function(i) miss_cords[i, , drop = FALSE])

        thd <- "Stations with missing coordinates"
        miss_cords_msg <- format_wcoords_msg(miss_cords, GalParams$inputs,
                                             thd, names(crds))
    }

    ### same coordinates, diff id
    crdC <- crds[, c('lon', 'lat'), drop = FALSE]
    idC <- duplicated(crdC) | duplicated(crdC, fromLast = TRUE)
    crdC <- crds[idC, , drop = FALSE]
    ic <- split(seq(nrow(crdC)), paste0(crdC$lon, '/', crdC$lat))
    diff_id <- lapply(ic, function(i){
        x <- crdC[i, , drop = FALSE]
        if(length(unique(x$id)) == 1) return(NULL)
        return(x)
    })

    idf <- !sapply(diff_id, is.null)
    diff_id_msg <- NULL
    if(any(idf)){
        diff_id <- diff_id[idf]
        thd <- "Stations with different ID but same coordinates"
        diff_id_msg <- format_wcoords_msg(diff_id, GalParams$inputs,
                                          thd, names(crds))
    }

    ### same id, diff coords
    sameId <- split(seq(nrow(crds)), crds$id)

    same_id <- lapply(sameId, function(i){
        x <- crds[i, , drop = FALSE]
        xc <- paste0(x$lon, '/', x$lat)
        if(length(unique(xc)) == 1){
            return(list(data = x, s = NULL))
        }else{
            return(list(data = x, s = 'yes'))
        }
    })

    ic <- lapply(same_id, '[[', 's')
    lcrds <- lapply(same_id, '[[', 'data')

    icf <- !sapply(ic, is.null)
    diff_crd_msg <- NULL
    diff_crd <- NULL
    if(any(icf)){
        diff_crd <- lcrds[icf]
        lcrds <- lcrds[!icf]

        thd <- "Stations with same ID but different coordinates"
        diff_crd_msg <- format_wcoords_msg(diff_crd, GalParams$inputs,
                                           thd, names(crds))
    }

    ##########

    stn_crds <- c(miss_cords, diff_crd, lcrds)
    stn_crds <- lapply(stn_crds, function(x){
        o <- order(x$file)
        x[o, , drop = FALSE]
    })

    out_crds <- lapply(stn_crds, function(x){
        o <- order(x$file)
        x <- x[o, , drop = FALSE]
        ix <- !is.na(x$lon) & !is.na(x$lat)
        ie <- !is.na(x$elv)
        if(!any(ix)){
            out <- x[1, c('id', 'lon', 'lat', 'elv'), drop = FALSE]
        }else{
            out <- x[ix, c('id', 'lon', 'lat', 'elv'), drop = FALSE]
            out <- out[1, , drop = FALSE]
        }

        if(any(ie)) out$elv <- x$elv[ie][1]
        return(out)
    })
    out_crds <- do.call(rbind, out_crds)

    ##########

    out_dates <- lapply(don, '[[', 'dates')
    out_dates <- do.call(c, out_dates)
    out_dates <- sort(unique(out_dates))

    ##########
    ### duplicated dates

    out_dup_dates <- lapply(don, function(x){
        idup <- duplicated(x$dates) | duplicated(x$dates, fromLast = TRUE)
        if(any(idup)){
            daty <- x$dates[idup]
            daty0 <- x$dates[!idup]
            y <- x$data[idup, , drop = FALSE]
            y0 <- x$data[!idup, , drop = FALSE]

            o <- order(daty)
            daty <- daty[o]
            y <- y[o, , drop = FALSE]

            ix <- split(seq_along(daty), daty)

            out <- lapply(ix, function(i){
                v <- y[i, ]
                d <- daty[i]
                o <- lapply(seq(ncol(v)), function(j){
                    n <- v[, j]
                    ia <- !is.na(n)
                    if(!any(ia)) return(list(v = NA, m = NULL))
                    z <- n[ia]
                    id <- d[ia]
                    s <- NULL
                    if(length(unique(z)) > 1)
                        s <- data.frame(date = id, values = z)
                    return(list(v = z[1], m = s))
                })

                v <- do.call(cbind, lapply(o, '[[', 'v'))
                m <- lapply(o, '[[', 'm')
                n <- !sapply(m, is.null)
                m <- if(any(n)) list(m[n], which(n)) else NULL
                list(v = v, m = m)
            })

            y <- do.call(rbind, lapply(out, '[[', 'v'))
            daty <- names(ix)

            daty <- c(daty, daty0)
            y <- rbind(y, y0)
            o <- order(daty)
            x$dates <- daty[o]
            x$data <- y[o, , drop = FALSE]

            odup <- lapply(out, '[[', 'm')
            inull <- !sapply(odup, is.null)
            xd <- NULL
            if(any(inull)){
                odup <- odup[inull]
                xd <- lapply(odup, function(v){
                    z <- v[[1]]
                    names(z) <- x$id[v[[2]]]
                    z
                })

            }

            return(list(data = x, dup = xd))
        }

        return(list(data = x, dup = NULL))
    })

    don <- lapply(out_dup_dates, '[[', 'data')
    dup <- lapply(out_dup_dates, '[[', 'dup')

    inull <- !sapply(dup, is.null)
    out_dup_dates <- NULL

    if(any(inull)){
        dup <- dup[inull]
        file_dup <- GalParams$inputs[inull]

        out_dup_dates <- lapply(seq_along(dup), function(j){
            l1 <- paste("File :", file_dup[j])
            l2 <- lapply(seq_along(dup[[j]]), function(i){
                s1 <- paste("\tDate :", names(dup[[j]][i]))
                s2 <- lapply(seq_along(dup[[j]]), function(l){
                    c1 <- paste("\t\tStation ID :", names(dup[[j]][[i]][l]))
                    c2 <- paste0("\t\t", paste0(names(dup[[j]][[i]][[l]]), collapse = '\t'))
                    N <- nrow(dup[[j]][[i]][[l]])
                    c3 <- rep(NA, N)
                    for(n in seq(N)){
                        c3[n] <- paste0("\t\t", paste0(dup[[j]][[i]][[l]][n, ], collapse = '\t'))
                    }
                    paste0(c(c1, c2, c3), collapse = '\n')
                })
                
                s2 <- paste0(unlist(s2), collapse = "\n\n")
                paste(s1, '\n\n', s2)
            })

            l2 <- paste0(unlist(l2), collapse = "\n\n")
            paste(l1, '\n\n', l2)
        })

        out_dup_dates <- paste0(unlist(out_dup_dates), collapse = "\n\n")

        m1 <- "Duplicated dates"
        m2 <- "Stations with different values"
        out_dup_dates <- paste0(m1, '\n', m2, '\n\n', out_dup_dates)
    }

    ##########

    ilen <- sapply(stn_crds, nrow) == 1
    dat1_crd <- NULL
    dat1 <- NULL
    if(any(ilen)){
        dat1_crd <- out_crds[ilen, , drop = FALSE]
        dat1 <- lapply(stn_crds[ilen], function(x){
            dat <- don[[x$file]]
            m <- dat$data[, dat$id == x$id]
            m[match(out_dates, dat$dates)]
        })
        dat1 <- do.call(cbind, dat1)
    }

    ilen <- sapply(stn_crds, nrow) > 1
    dats_crd <- NULL
    dats <- NULL
    diff_vals_msg <- NULL
    if(any(ilen)){
        dats_crd <- out_crds[ilen, , drop = FALSE]
        dats <- lapply(stn_crds[ilen], function(x){
            z <- lapply(seq_along(x$file), function(i){
                dat <- don[[x$file[i]]]
                m <- dat$data[, dat$id == x$id[i]]
                m[match(out_dates, dat$dates)]
            })
            z <- do.call(cbind, z)

            zo <- lapply(seq(nrow(z)), function(i){
                v <- z[i, ]
                ina <- !is.na(v)
                v <- v[ina]
                if(length(v) == 0) return(list(v = NA, d = NULL))
                if(all(v == v[1])) return(list(v = v[1], d = NULL))
                ff <- x$file[ina]

                return(list(v = v[1], d = cbind(ff, v)))
            })

            z <- do.call(c, lapply(zo, '[[', 'v'))
            zo <- lapply(zo, '[[', 'd')
            inull <- !sapply(zo, is.null)
            xd <- NULL
            if(any(inull)){
                zo <- zo[inull]
                do <- out_dates[inull]
                xd <- list(id = x$id[1], date = do, val = zo)
            }

            list(z = z, d = xd)
        })

        diff_vals <- lapply(dats, '[[', 'd')
        dats <- lapply(dats, '[[', 'z')
        dats <- do.call(cbind, dats)

        inull <- !sapply(diff_vals, is.null)
        if(any(inull)){
            diff_vals <- diff_vals[inull]

            diff_vals_msg <- lapply(diff_vals, function(x){
                l1 <- paste("\nStation ID :", x$id)
                l2 <- lapply(seq_along(x$val), function(j){
                    s1 <- paste("\n\tDate :", x$date[j], '\n\n')
                    z <- x$val[[j]]
                    n <- nrow(z)
                    s2 <- rep(NA, n)
                    for(i in 1:n){
                        ff <- GalParams$inputs[[z[i, 1]]]
                        s2[i] <- paste('\t', ff, z[i, 2], sep = '\t')
                    }
                    s2 <- paste0(c("\t\tFile\tValues", s2), collapse = '\n')
                   
                   paste0(s1, s2)
                })
                l2 <- paste0(unlist(l2), collapse = '\n')
                paste0(l1, '\n', l2)
            })

            diff_vals_msg <- paste0(unlist(diff_vals_msg), collapse = '\n')

            t1 <- "Stations with different values"
            diff_vals_msg <- paste0(t1, '\n', diff_vals_msg)
        }
    }


    ##########

    out_message_crds <- c(miss_cords_msg, diff_id_msg, diff_crd_msg)
    if(length(out_message_crds) > 0){
        out_message_crds <- paste0(out_message_crds, collapse = '\n\n')
        tab_title <- "Coordinates-issues"
        containertab <- Display_Output_Console_Tab(out_message_crds, tab_title, cat)
        ntab <- update.OpenTabs('ctxt', containertab)
        tkselect(.cdtEnv$tcl$main$tknotes, ntab)
    }

    if(!is.null(out_dup_dates)){
        tab_title <- "Duplicated-dates"
        containertab <- Display_Output_Console_Tab(out_dup_dates, tab_title, cat)
        ntab <- update.OpenTabs('ctxt', containertab)
        tkselect(.cdtEnv$tcl$main$tknotes, ntab)
    }

    if(!is.null(diff_vals_msg)){
        tab_title <- "Different-values"
        containertab <- Display_Output_Console_Tab(diff_vals_msg, tab_title, cat)
        ntab <- update.OpenTabs('ctxt', containertab)
        tkselect(.cdtEnv$tcl$main$tknotes, ntab)
    }

    ##########

    if(length(out_message_crds) > 0 |
       !is.null(out_dup_dates) |
       !is.null(diff_vals_msg))
    {
        sepl <- "\n\n*********************************\n\n"
        out_log <- paste0(out_message_crds, sepl, out_dup_dates, sepl, diff_vals_msg)
        file_log <- paste0(tools::file_path_sans_ext(basename(GalParams$file2save)), "_LOG.txt")
        file_log <- file.path(dirname(GalParams$file2save), file_log)

        cat(out_log, file = file_log, sep = '\n')
    }

    ##########

    out_crds <- rbind(dats_crd, dat1_crd)
    out_data <- cbind(dats, dat1)
    colnames(out_data) <- NULL
    don <- don[[1]]

    don$id <- out_crds$id
    don$lon <- out_crds$lon
    don$lat <- out_crds$lat
    don$elv <- if(all(is.na(out_crds$elv))) NULL else out_crds$elv
    don$dates <- out_dates
    don$data <- out_data
    class(don) <- append(class(don), "cdtstationdata")

    writeCDTStationData(don, GalParams$file2save, na.strings = donInfo[[3]]$miss.val)

    return(0)
}

