
rfe.product.list <- function(tstep = "dekadal", minhour = NULL){
    if(tstep %in% c("minute", "hourly")){
        nom <- paste0(minhour, '-', tstep)
    }else{
        nom <- tstep
    }
    rfe_cbname <- .cdtData$EnvData$rfeProds[[nom]]$longname
    rfe_name <- .cdtData$EnvData$rfeProds[[nom]]$name

    list(cbname = rfe_cbname, name = rfe_name)
}

rfe.product.source <- function(src, tstep, minhour = NULL){
    if(tstep %in% c("minute", "hourly")){
        nom <- paste0(minhour, '-', tstep)
    }else{
        nom <- tstep
    }
    rfe_src <- .cdtData$EnvData$rfeProds[[nom]]$source
    rfe_name <- .cdtData$EnvData$rfeProds[[nom]]$name
    urls <- rfe_src[[which(rfe_name == src)]]

    tmpf <- tempfile(fileext = ".html")
    cat("<html>\n<body>\n", file = tmpf)
    cat("<h3>Data source and documentation</h3>\n", file = tmpf, append = TRUE)
    cat("<p><i> Click on the links:</i></p>\n", file = tmpf, append = TRUE)
    cat("<ul>\n", file = tmpf, append = TRUE)
    for(lk in urls)
        cat(paste0("<li><a href=",  lk, ">",  lk, "</a></li>\n"), file = tmpf, append = TRUE)
    cat("</ul>\n", file = tmpf, append = TRUE)
    cat("</body>\n</html>\n", file = tmpf, append = TRUE)

    return(tmpf)
}

rfe.iridl.ulrs <- function(src, tstep, minhour){
    if(tstep %in% c("minute", "hourly")){
        nom <- paste0(minhour, '-', tstep)
    }else{
        nom <- tstep
    }

    rfe_name <- .cdtData$EnvData$rfeProds[[nom]]$name
    iridl <- .cdtData$EnvData$rfeProds[[nom]]$iridl
    ret <- iridl[which(rfe_name == src)]

    return(ret)
}

rfe.need.usrpwd <- function(src, tstep, minhour){
    if(tstep %in% c("minute", "hourly")){
        nom <- paste0(minhour, '-', tstep)
    }else{
        nom <- tstep
    }

    rfe_name <- .cdtData$EnvData$rfeProds[[nom]]$name
    regist <- .cdtData$EnvData$rfeProds[[nom]]$registration
    urllog <- regist[which(rfe_name == src)]

    usrpwd <- if(urllog == "") FALSE else TRUE

    statepwd <- if(usrpwd) 'normal' else 'disabled'

    if(grepl("^mswep\\.", src)){
        statepwd <- 'disabled'
    }

    list(usrpwd = usrpwd, statepwd = statepwd, urllog = urllog)
}

get_all.rfe.products <- function(){
    rfeFiles <- file.path(.cdtDir$Root, "rfeproducts")
    rfeList <- list.files(rfeFiles, "^rfe.+\\.csv$")
    rfeProds <- gsub('\\.csv', '', rfeList)
    rfeProds <- gsub('rfe_', '', rfeProds)
    rfetmp <- lapply(rfeList, function(csv){
        ff <- file.path(rfeFiles, csv)
        dat <- utils::read.table(ff, sep = ',', header = TRUE, na.strings = '',
                                 colClasses = 'character', stringsAsFactors = FALSE)
        ix <- which(!is.na(dat$name))
        ie <- c(ix[-1] - 1, nrow(dat))
        xx <- lapply(seq_along(ix), function(i){
            x <- dat[ix[i]:ie[i], ]
            x1 <- trimws(x$name)
            x1[x1 == ""] <- NA
            x1 <- x1[!is.na(x1)]
            x2 <- trimws(x$longname)
            x2[x2 == ""] <- NA
            x2 <- x2[!is.na(x2)]
            x3 <- trimws(x$source)
            x3[x3 == ""] <- NA
            x3 <- x3[!is.na(x3)]
            x4 <- trimws(x$iridl)
            x4[x4 == ""] <- NA
            x4 <- x4[!is.na(x4)]
            x4 <- as.logical(x4)
            if(length(x4) == 0) x4 <- FALSE
            x5 <- trimws(x$registration)
            x5[x5 == ""] <- NA
            x5 <- x5[!is.na(x5)]
            if(length(x5) == 0) x5 <- ""

            list(x1 = x1, x2 = x2, x3 = x3,
                x4 = x4, x5 = x5)
        })
        x1 <- sapply(xx, '[[', 'x1')
        x2 <- sapply(xx, '[[', 'x2')
        x3 <- lapply(xx, '[[', 'x3')
        x4 <- sapply(xx, '[[', 'x4')
        x5 <- sapply(xx, '[[', 'x5')

        list(name = x1, longname = x2,
             source = x3, iridl = x4,
             registration = x5)
    })
    names(rfetmp) <- rfeProds

    .cdtData$EnvData$rfeProds <- rfetmp
}

################################################

download_RFE <- function(){
    if(WindowsOS()){
        largeur0 <- 55
        largeur1 <- 27
        largeur2 <- 18
        largeur3 <- 33
        largeur4 <- 22
        largeur5 <- 16
    }else{
        largeur0 <- 52
        largeur1 <- 27
        largeur2 <- 18
        largeur3 <- 33
        largeur4 <- 22
        largeur5 <- 15
    }

    xml.dlg <- file.path(.cdtDir$dirLocal, "languages", "cdtDownloadRFE_dlgBox.xml")
    lang.dlg <- cdtLanguageParse(xml.dlg, .cdtData$Config$lang.iso)

    ## load rfe products
    get_all.rfe.products()

    #########
    tt <- tktoplevel()
    tkgrab.set(tt)
    tkfocus(tt)

    frGrd0 <- tkframe(tt, relief = 'raised', borderwidth = 2)
    frGrd1 <- tkframe(tt)

    ###################################################

    frRFE <- tkframe(frGrd0, relief = 'sunken', bd = 2)

    timeStep <- tclVar()
    CbperiodVAL <- .cdtEnv$tcl$lang$global[['combobox']][['1']][1:6]
    periodVAL <- c('minute', 'hourly', 'daily', 'pentad', 'dekadal', 'monthly')
    tclvalue(timeStep) <- CbperiodVAL[periodVAL %in% .cdtData$GalParams$tstep]

    if(.cdtData$GalParams$tstep %in% c("minute", "hourly")){
        if(.cdtData$GalParams$tstep == "minute"){
            # CbminhourVAL <- c(15, 30)
            CbminhourVAL <- 30
            minhour.txt <- lang.dlg[['label']][['1']]
        }
        if(.cdtData$GalParams$tstep == "hourly"){
            CbminhourVAL <- c(1, 3, 6)
            minhour.txt <- lang.dlg[['label']][['2']]
        }

        minhour.val <- .cdtData$GalParams$minhour

        if(is.na(minhour.val)){
            minhour.val <- CbminhourVAL[1]
        }else{
            if(!minhour.val %in% CbminhourVAL)
                minhour.val <- CbminhourVAL[1]
        }

        minhour.state <- "normal"
    }else{
        CbminhourVAL <- ""
        minhour.val <- ""
        minhour.state <- "disabled"
        minhour.txt <- ""
    }

    minhour.tclVar <- tclVar(minhour.val)
    minhour.txtVar <- tclVar(minhour.txt)

    #################

    RFESrc <- tclVar()
    rfedata <- rfe.product.list(.cdtData$GalParams$tstep,
                                .cdtData$GalParams$minhour)
    CbRFEVAL <- rfedata$cbname
    RFEVAL <- rfedata$name
    tclvalue(RFESrc) <- CbRFEVAL[RFEVAL %in% .cdtData$GalParams$rfe.src]

    #################

    need.pwd <- rfe.need.usrpwd(.cdtData$GalParams$rfe.src,
                                .cdtData$GalParams$tstep,
                                .cdtData$GalParams$minhour)
    stateusr <- if(need.pwd$usrpwd) "normal" else "disabled"
    statepwd <- need.pwd$statepwd

    url.log <- tclVar(need.pwd$urllog)
    username <- tclVar(.cdtData$GalParams$login$usr)
    password <- tclVar(.cdtData$GalParams$login$pwd)

    #################

    is_iridl <- rfe.iridl.ulrs(.cdtData$GalParams$rfe.src,
                               .cdtData$GalParams$tstep,
                               .cdtData$GalParams$minhour)
    if(is_iridl){
        stateiridl <- "normal"
        valiridl <- .cdtData$GalParams$iridl.src
    }else{
        stateiridl <- "disabled"
        valiridl <- FALSE
    }
    iridl.src <- tclVar(valiridl)

    #################

    chirps_data <- .cdtData$GalParams$rfe.src %in% c("chirp-gb", "chirpsv2-gb") &&
                   !.cdtData$GalParams$iridl.src
    chirps.global <- tclVar(.cdtData$GalParams$chirps.global)
    ##
    fews_iridl <- .cdtData$GalParams$rfe.src %in% c("arc2-af", "rfev2-af", "rfev2-sa") &&
                  .cdtData$GalParams$tstep %in% c("dekadal", "monthly")
    if(fews_iridl){
        stateiridl <- "disabled"
        tclvalue(iridl.src) <- TRUE
    }
    ##
    fewsdaily_cpc <- .cdtData$GalParams$rfe.src %in% c("arc2-af", "rfev2-af") &&
                     .cdtData$GalParams$tstep == "daily" &&
                     !.cdtData$GalParams$iridl.src
    fewsDailyCB <- c('Binary', 'GeoTIFF')
    fewsDailyVAL <- c('bin', 'tif')
    fewsAFdaily.type <- tclVar()
    tclvalue(fewsAFdaily.type) <- fewsDailyCB[fewsDailyVAL %in% .cdtData$GalParams$fewsAFdaily.type]

    fewsdaily_fun <- function(frame){
        txt.fews_type <- tklabel(frame, text = lang.dlg[['label']][['16']], anchor = 'w', justify = 'left')
        cb.fews_type <- ttkcombobox(frame, values = fewsDailyCB, textvariable = fewsAFdaily.type, justify = 'center', width = 9)
        tkgrid(txt.fews_type, cb.fews_type)
    }

    #################

    txt.tres <- tklabel(frRFE, text = lang.dlg[['label']][['3']], anchor = 'w', justify = 'left')
    cb.tres <- ttkcombobox(frRFE, values = CbperiodVAL, textvariable = timeStep, width = largeur2)
    txt.sat <- tklabel(frRFE, text = lang.dlg[['label']][['4']], anchor = 'w', justify = 'left')
    cb.sat <- ttkcombobox(frRFE, values = CbRFEVAL, textvariable = RFESrc, width = largeur3)

    cb.mhI <- ttkcombobox(frRFE, values = CbminhourVAL, textvariable = minhour.tclVar, width = 3, state = minhour.state)
    txt.mhI <- tklabel(frRFE, text = tclvalue(minhour.txtVar), textvariable = minhour.txtVar, anchor = 'w', justify = 'left')
    bt.range <- ttkbutton(frRFE, text = lang.dlg[['button']][['3']])

    chk.iridl <- tkcheckbutton(frRFE, variable = iridl.src, text = lang.dlg[['label']][['14']], anchor = 'w', justify = 'left', state = stateiridl)

    ####
    chk.chirps_global <- tkcheckbutton(frRFE, variable = chirps.global, text = lang.dlg[['label']][['15']], anchor = 'w', justify = 'left')

    frFEWSdaily <- tkframe(frRFE)
    fewsdaily_fun(frFEWSdaily)

    ####
    txt.log1 <- tklabel(frRFE, text = lang.dlg[['label']][['5']], anchor = 'e', justify = 'right')
    txt.log2 <- tklabel(frRFE, text = tclvalue(url.log), textvariable = url.log, anchor = 'w', justify = 'left')

    ####
    fgKolRegistr <- as.character(tkcget(txt.log2, '-foreground'))
    bgKolRegistr <- as.character(tkcget(txt.log2, '-background'))
    if(need.pwd$usrpwd){
        fgKol <- "blue"
        bgKol <- "white"
    }else{
        fgKol <- fgKolRegistr
        bgKol <- bgKolRegistr
    }
    tkconfigure(txt.log2, foreground = fgKol, background = bgKol)

    ####
    frUSRPWD <- tkframe(frRFE)
    txt.usr <- tklabel(frUSRPWD, text = lang.dlg[['label']][['6']], anchor = 'e', justify = 'right')
    en.usr <- tkentry(frUSRPWD, textvariable = username, state = stateusr, width = largeur4, justify = "left")
    txt.pwd <- tklabel(frUSRPWD, text = lang.dlg[['label']][['7']], anchor = 'e', justify = 'right')
    en.pwd <- tkentry(frUSRPWD, textvariable = password, show = "*", state = statepwd, width = largeur5, justify = "left")

    bt.info <- ttkbutton(frRFE, text = lang.dlg[['button']][['4']])
    bt.tcover <- ttkbutton(frRFE, text = lang.dlg[['button']][['5']])

    #################

    tkconfigure(bt.range, command = function(){
        tstep <- periodVAL[CbperiodVAL %in% trimws(tclvalue(timeStep))]
        tcl('wm', 'attributes', tt, topmost = FALSE)
        .cdtData$GalParams[["date.range"]] <- getInfoDateRange(tt, .cdtData$GalParams[["date.range"]], tstep, TRUE)
        tcl('wm', 'attributes', tt, topmost = TRUE)
    })

    tkconfigure(bt.info, command = function(){
        tstep <- periodVAL[CbperiodVAL %in% trimws(tclvalue(timeStep))]
        minhour <- as.numeric(trimws(tclvalue(minhour.tclVar)))
        src <- RFEVAL[CbRFEVAL %in% trimws(tclvalue(RFESrc))]
        urls <- rfe.product.source(src, tstep, minhour)
        utils::browseURL(paste0('file://', urls))
    })

    tkconfigure(bt.tcover, command = function(){
        tkgrab.release(tt)
        tkdestroy(tt)
        tkfocus(.cdtEnv$tcl$main$win)
        tcl('update')

        .cdtData$GalParams$tstep <- periodVAL[CbperiodVAL %in% trimws(tclvalue(timeStep))]
        .cdtData$GalParams$rfe.src <- RFEVAL[CbRFEVAL %in% trimws(tclvalue(RFESrc))]
        .cdtData$GalParams$minhour <- as.numeric(trimws(tclvalue(minhour.tclVar)))
        .cdtData$GalParams$iridl.src <- switch(tclvalue(iridl.src), '0' = FALSE, '1' = TRUE)

        .cdtData$GalParams$chirps.global <- switch(tclvalue(chirps.global), '0' = FALSE, '1' = TRUE)
        .cdtData$GalParams$fewsAFdaily.type <- fewsDailyVAL[fewsDailyCB %in% trimws(tclvalue(fewsAFdaily.type))]

        .cdtData$GalParams$login$usr <- trimws(tclvalue(username))
        .cdtData$GalParams$login$pwd <- trimws(tclvalue(password))

        if(testConnection()){
            Insert.Messages.Out(lang.dlg[['message']][['9']], TRUE, "i")
            ret <- try(exec.check_coverage_RFE(), silent = TRUE)
            if(!inherits(ret, "try-error")){
                if(is.null(ret$end)){
                    msg <- paste(ret$name, ret$timestep)
                    Insert.Messages.Out(msg, TRUE, "e")
                    Insert.Messages.Out(lang.dlg[['message']][['10']], TRUE, "e")
                }else{
                    Insert.Messages.Out(lang.dlg[['message']][['11']], TRUE, "s")
                }
            }else{
                Insert.Messages.Out(gsub('[\r\n]', '', ret[1]), TRUE, "e")
                Insert.Messages.Out(lang.dlg[['message']][['10']], TRUE, "e")
            }
        }else{
            Insert.Messages.Out(lang.dlg[['message']][['2']], format = TRUE)
            return(NULL)
        }
    })

    #################

    tkgrid(txt.usr, row = 0, column = 0, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
    tkgrid(en.usr, row = 0, column = 1, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
    tkgrid(txt.pwd, row = 0, column = 2, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
    tkgrid(en.pwd, row = 0, column = 3, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)

    #################

    tkgrid(txt.tres, row = 0, column = 0, sticky = 'we', rowspan = 1, columnspan = 4, padx = 1, pady = 1, ipadx = 1, ipady = 1)
    tkgrid(cb.tres, row = 1, column = 0, sticky = 'we', rowspan = 1, columnspan = 4, padx = 1, pady = 1, ipadx = 1, ipady = 1)
    tkgrid(txt.sat, row = 0, column = 4, sticky = 'we', rowspan = 1, columnspan = 4, padx = 1, pady = 1, ipadx = 1, ipady = 1)
    tkgrid(cb.sat, row = 1, column = 4, sticky = 'we', rowspan = 1, columnspan = 4, padx = 1, pady = 1, ipadx = 1, ipady = 1)

    tkgrid(cb.mhI, row = 2, column = 2, sticky = 'e', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
    tkgrid(txt.mhI, row = 2, column = 3, sticky = 'w', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)

    tkgrid(bt.range, row = 2, column = 4, sticky = 'we', rowspan = 1, columnspan = 4, padx = 1, pady = 1, ipadx = 1, ipady = 1)

    tkgrid(chk.iridl, row = 3, column = 0, sticky = 'we', rowspan = 1, columnspan = 8, padx = 2, pady = 2, ipadx = 1, ipady = 1)

    ####
    if(chirps_data) tkgrid(chk.chirps_global, row = 4, column = 0, sticky = 'we', rowspan = 1, columnspan = 8, padx = 2, pady = 2, ipadx = 1, ipady = 1)
    if(fewsdaily_cpc) tkgrid(frFEWSdaily, row = 4, column = 0, sticky = 'we', rowspan = 1, columnspan = 8, padx = 2, pady = 2, ipadx = 1, ipady = 1)

    ####
    tkgrid(txt.log1, row = 5, column = 0, sticky = 'we', rowspan = 1, columnspan = 2, padx = 1, pady = 3, ipadx = 1, ipady = 1)
    tkgrid(txt.log2, row = 5, column = 2, sticky = 'we', rowspan = 1, columnspan = 6, padx = 1, pady = 3, ipadx = 1, ipady = 1)
    tkgrid(frUSRPWD, row = 6, column = 0, sticky = 'we', rowspan = 1, columnspan = 8, padx = 1, pady = 1, ipadx = 1, ipady = 1)

    tkgrid(bt.info, row = 7, column = 0, sticky = 'we', rowspan = 1, columnspan = 5, padx = 1, pady = 5, ipadx = 1, ipady = 1)
    tkgrid(bt.tcover, row = 7, column = 5, sticky = 'we', rowspan = 1, columnspan = 3, padx = 1, pady = 5, ipadx = 1, ipady = 1)

    helpWidget(cb.tres, lang.dlg[['tooltip']][['1']], lang.dlg[['status']][['1']])
    helpWidget(cb.mhI, lang.dlg[['tooltip']][['2']], lang.dlg[['status']][['2']])
    helpWidget(cb.sat, lang.dlg[['tooltip']][['2a']], lang.dlg[['status']][['2a']])
    helpWidget(bt.range, lang.dlg[['tooltip']][['2b']], lang.dlg[['status']][['2b']])

    ######################

    tkbind(cb.tres, "<<ComboboxSelected>>", function(){
        tstep <- periodVAL[CbperiodVAL %in% trimws(tclvalue(timeStep))]
        minhour.val <- as.numeric(trimws(tclvalue(minhour.tclVar)))

        ########
        ## minute & hourly
        if(tstep %in% c("minute", "hourly")){
            if(tstep == "minute"){
                # CbminhourVAL <- c(15, 30)
                CbminhourVAL <- 30
                minhour.txt <- lang.dlg[['label']][['1']]
            }
            if(tstep == "hourly"){
                CbminhourVAL <- c(1, 3, 6)
                minhour.txt <- lang.dlg[['label']][['2']]
            }

            if(is.na(minhour.val)){
                minhour.val <- CbminhourVAL[1]
            }else{
                if(!minhour.val %in% CbminhourVAL)
                    minhour.val <- CbminhourVAL[1]
            }

            minhour.state <- "normal"
        }else{
            CbminhourVAL <- ""
            minhour.val <- ""
            minhour.state <- "disabled"
            minhour.txt <- ""
        }

        tkconfigure(cb.mhI, values = CbminhourVAL, state = minhour.state)
        tclvalue(minhour.tclVar) <- minhour.val
        tclvalue(minhour.txtVar) <- minhour.txt

        ########
        rfedata <- rfe.product.list(tstep, minhour.val)
        CbRFEVAL <<- rfedata$cbname
        RFEVAL <<- rfedata$name
 
        tkconfigure(cb.sat, values = CbRFEVAL)
        if(!trimws(tclvalue(RFESrc)) %in% CbRFEVAL) tclvalue(RFESrc) <- CbRFEVAL[1]

        rfe.src <- RFEVAL[CbRFEVAL %in% trimws(tclvalue(RFESrc))]

        ########
        need.pwd <- rfe.need.usrpwd(rfe.src, tstep, minhour.val)
        stateusr <- if(need.pwd$usrpwd) "normal" else "disabled"
        statepwd <- need.pwd$statepwd

        tclvalue(url.log) <- need.pwd$urllog
        tkconfigure(en.usr, state = stateusr)
        tkconfigure(en.pwd, state = statepwd)

        if(need.pwd$usrpwd){
            fgKol <- "blue"
            bgKol <- "white"
        }else{
            fgKol <- fgKolRegistr
            bgKol <- bgKolRegistr
        }
        tkconfigure(txt.log2, foreground = fgKol, background = bgKol)

        ########
        if(rfe.iridl.ulrs(rfe.src, tstep, minhour.val)){
            stateiridl <- "normal"
            valiridl <- tclvalue(iridl.src)
        }else{
            stateiridl <- "disabled"
            valiridl <- FALSE
        }
        tclvalue(iridl.src) <- valiridl
        tkconfigure(chk.iridl, state = stateiridl)

        ########
        chirps_data <- rfe.src %in% c("chirp-gb", "chirpsv2-gb") &&
                       tclvalue(iridl.src) == "0"

        tkdestroy(chk.chirps_global)
        if(chirps_data){
            chk.chirps_global <<- tkcheckbutton(frRFE, variable = chirps.global, text = lang.dlg[['label']][['15']], anchor = 'w', justify = 'left')
            tkgrid(chk.chirps_global, row = 4, column = 0, sticky = 'we', rowspan = 1, columnspan = 8, padx = 2, pady = 2, ipadx = 1, ipady = 1)
        }
        ####
        fews_iridl <- rfe.src %in% c("arc2-af", "rfev2-af", "rfev2-sa") &&
                      tstep %in% c("dekadal", "monthly")
        if(fews_iridl){
            tclvalue(iridl.src) <- TRUE
            tkconfigure(chk.iridl, state = 'disabled')
        }
        ####
        fewsdaily_cpc <- rfe.src %in% c("arc2-af", "rfev2-af") &&
                         tstep == "daily" && tclvalue(iridl.src) == "0"
        tkdestroy(frFEWSdaily)
        if(fewsdaily_cpc){
            frFEWSdaily <<- tkframe(frRFE)
            fewsdaily_fun(frFEWSdaily)
            tkgrid(frFEWSdaily, row = 4, column = 0, sticky = 'we', rowspan = 1, columnspan = 8, padx = 2, pady = 2, ipadx = 1, ipady = 1)
        }
    })

    ####
    tkbind(cb.mhI, "<<ComboboxSelected>>", function(){
        tstep <- periodVAL[CbperiodVAL %in% trimws(tclvalue(timeStep))]
        minhour.val <- as.numeric(trimws(tclvalue(minhour.tclVar)))

        ########
        rfedata <- rfe.product.list(tstep, minhour.val)
        CbRFEVAL <<- rfedata$cbname
        RFEVAL <<- rfedata$name

        tkconfigure(cb.sat, values = CbRFEVAL)
        if(!trimws(tclvalue(RFESrc)) %in% CbRFEVAL) tclvalue(RFESrc) <- CbRFEVAL[1]

        rfe.src <- RFEVAL[CbRFEVAL %in% trimws(tclvalue(RFESrc))]

        ########
        need.pwd <- rfe.need.usrpwd(rfe.src, tstep, minhour.val)
        stateusr <- if(need.pwd$usrpwd) "normal" else "disabled"
        statepwd <- need.pwd$statepwd

        tclvalue(url.log) <- need.pwd$urllog
        tkconfigure(en.usr, state = stateusr)
        tkconfigure(en.pwd, state = statepwd)

        if(need.pwd$usrpwd){
            fgKol <- "blue"
            bgKol <- "white"
        }else{
            fgKol <- fgKolRegistr
            bgKol <- bgKolRegistr
        }
        tkconfigure(txt.log2, foreground = fgKol, background = bgKol)
    })

    ####
    tkbind(cb.sat, "<<ComboboxSelected>>", function(){
        rfe.src <- RFEVAL[CbRFEVAL %in% trimws(tclvalue(RFESrc))]
        tstep <- periodVAL[CbperiodVAL %in% trimws(tclvalue(timeStep))]
        minhour <- as.numeric(trimws(tclvalue(minhour.tclVar)))

        ########
        need.pwd <- rfe.need.usrpwd(rfe.src, tstep, minhour)
        stateusr <- if(need.pwd$usrpwd) "normal" else "disabled"
        statepwd <- need.pwd$statepwd

        tclvalue(url.log) <- need.pwd$urllog
        tkconfigure(en.usr, state = stateusr)
        tkconfigure(en.pwd, state = statepwd)

        if(need.pwd$usrpwd){
            fgKol <- "blue"
            bgKol <- "white"
        }else{
            fgKol <- fgKolRegistr
            bgKol <- bgKolRegistr
        }
        tkconfigure(txt.log2, foreground = fgKol, background = bgKol)

        ########
        if(rfe.iridl.ulrs(rfe.src, tstep, minhour)){
            stateiridl <- "normal"
            valiridl <- tclvalue(iridl.src)
        }else{
            stateiridl <- "disabled"
            valiridl <- FALSE
        }
        tclvalue(iridl.src) <- valiridl
        tkconfigure(chk.iridl, state = stateiridl)

        ########
        chirps_data <- rfe.src %in% c("chirp-gb", "chirpsv2-gb") &&
                       tclvalue(iridl.src) == "0"

        tkdestroy(chk.chirps_global)
        if(chirps_data){
            chk.chirps_global <<- tkcheckbutton(frRFE, variable = chirps.global, text = lang.dlg[['label']][['15']], anchor = 'w', justify = 'left')
            tkgrid(chk.chirps_global, row = 4, column = 0, sticky = 'we', rowspan = 1, columnspan = 8, padx = 2, pady = 2, ipadx = 1, ipady = 1)
        }
        ####
        fews_iridl <- rfe.src %in% c("arc2-af", "rfev2-af", "rfev2-sa") &&
                      tstep %in% c("dekadal", "monthly")
        if(fews_iridl){
            tclvalue(iridl.src) <- TRUE
            tkconfigure(chk.iridl, state = 'disabled')
        }
        ####
        fewsdaily_cpc <- rfe.src %in% c("arc2-af", "rfev2-af") &&
                         tstep == "daily" && tclvalue(iridl.src) == "0"
        tkdestroy(frFEWSdaily)
        if(fewsdaily_cpc){
            frFEWSdaily <<- tkframe(frRFE)
            fewsdaily_fun(frFEWSdaily)
            tkgrid(frFEWSdaily, row = 4, column = 0, sticky = 'we', rowspan = 1, columnspan = 8, padx = 2, pady = 2, ipadx = 1, ipady = 1)
        }
    })

    tkbind(chk.iridl, "<Button-1>", function(){
        rfe.src <- RFEVAL[CbRFEVAL %in% trimws(tclvalue(RFESrc))]
        tstep <- periodVAL[CbperiodVAL %in% trimws(tclvalue(timeStep))]
        chirps_data <- rfe.src %in% c("chirp-gb", "chirpsv2-gb")

        if(chirps_data){
            if(tstep != 'pentad'){
                tkdestroy(chk.chirps_global)
            }
            if(tclvalue(iridl.src) == "1"){
                chk.chirps_global <<- tkcheckbutton(frRFE, variable = chirps.global, text = lang.dlg[['label']][['15']], anchor = 'w', justify = 'left')
                tkgrid(chk.chirps_global, row = 4, column = 0, sticky = 'we', rowspan = 1, columnspan = 8, padx = 2, pady = 2, ipadx = 1, ipady = 1)
            }
        }
        #####
        fewsdaily_cpc <- rfe.src %in% c("arc2-af", "rfev2-af") && tstep == "daily"
        if(fewsdaily_cpc){
            tkdestroy(frFEWSdaily)
            if(tclvalue(iridl.src) == "1"){
                frFEWSdaily <<- tkframe(frRFE)
                fewsdaily_fun(frFEWSdaily)
                tkgrid(frFEWSdaily, row = 4, column = 0, sticky = 'we', rowspan = 1, columnspan = 8, padx = 2, pady = 2, ipadx = 1, ipady = 1)
            }
        }
    })

    ####
    tkbind(txt.log2, "<Button-1>", function(){
        rfe.src <- RFEVAL[CbRFEVAL %in% trimws(tclvalue(RFESrc))]
        tstep <- periodVAL[CbperiodVAL %in% trimws(tclvalue(timeStep))]
        minhour.val <- as.numeric(trimws(tclvalue(minhour.tclVar)))
        need.pwd <- rfe.need.usrpwd(rfe.src, tstep, minhour.val)
        if(need.pwd$usrpwd){
            utils::browseURL(need.pwd$urllog)
        }
    })

    tkbind(txt.log2, "<Enter>", function(){
        rfe.src <- RFEVAL[CbRFEVAL %in% trimws(tclvalue(RFESrc))]
        tstep <- periodVAL[CbperiodVAL %in% trimws(tclvalue(timeStep))]
        minhour.val <- as.numeric(trimws(tclvalue(minhour.tclVar)))
        need.pwd <- rfe.need.usrpwd(rfe.src, tstep, minhour.val)
        if(need.pwd$usrpwd){
            tkconfigure(txt.log2, relief = 'groove', borderwidth = 2, cursor = 'hand2')
        }
    })

    tkbind(txt.log2, "<Leave>", function(){
        tkconfigure(txt.log2, relief = 'flat', borderwidth = 0, cursor = '')
    })

    ###################################################

    frRegion <- tkframe(frGrd0, relief = 'sunken', bd = 2)

    minLon <- tclVar(.cdtData$GalParams$bbox$minlon)
    maxLon <- tclVar(.cdtData$GalParams$bbox$maxlon)
    minLat <- tclVar(.cdtData$GalParams$bbox$minlat)
    maxLat <- tclVar(.cdtData$GalParams$bbox$maxlat)

    fr_grd <- ttklabelframe(frRegion, text = lang.dlg[['label']][['8']], relief = "groove", borderwidth = 2)

    grd_llon <- tklabel(fr_grd, text = lang.dlg[['label']][['9']], anchor = 'e', justify = 'right', width = largeur1)
    grd_llat <- tklabel(fr_grd, text = lang.dlg[['label']][['10']], anchor = 'e', justify = 'right')
    grd_lb1 <- tklabel(fr_grd, text = lang.dlg[['label']][['11']])
    grd_lb2 <- tklabel(fr_grd, text = lang.dlg[['label']][['12']])
    grd_vlon1 <- tkentry(fr_grd, width = 8, justify = "right", textvariable = minLon)
    grd_vlon2 <- tkentry(fr_grd, width = 8, justify = "right", textvariable = maxLon)
    grd_vlat1 <- tkentry(fr_grd, width = 8, justify = "right", textvariable = minLat)
    grd_vlat2 <- tkentry(fr_grd, width = 8, justify = "right", textvariable = maxLat)

    tkgrid(grd_lb1, row = 0, column = 1, sticky = "ew")
    tkgrid(grd_lb2, row = 0, column = 2, sticky = "ew")
    tkgrid(grd_llon, row = 1, column = 0, sticky = "ew")
    tkgrid(grd_vlon1, row = 1, column = 1, sticky = "ew")
    tkgrid(grd_vlon2, row = 1, column = 2, sticky = "ew")
    tkgrid(grd_llat, row = 2, column = 0, sticky = "ew")
    tkgrid(grd_vlat1, row = 2, column = 1, sticky = "ew")
    tkgrid(grd_vlat2, row = 2, column = 2, sticky = "ew")

    tkgrid(fr_grd, row = 0, column = 0, sticky = "ew", padx = 5, pady = 5)

    helpWidget(grd_vlon1, lang.dlg[['tooltip']][['3']], lang.dlg[['status']][['3']])
    helpWidget(grd_vlon2, lang.dlg[['tooltip']][['4']], lang.dlg[['status']][['4']])
    helpWidget(grd_vlat1, lang.dlg[['tooltip']][['5']], lang.dlg[['status']][['5']])
    helpWidget(grd_vlat2, lang.dlg[['tooltip']][['6']], lang.dlg[['status']][['6']])

    ###################################################

    frDirsave <- tkframe(frGrd0, relief = 'sunken', bd = 2)

    dir2save <- tclVar(.cdtData$GalParams$dir2save)

    txt.dir.save <- tklabel(frDirsave, text = lang.dlg[['label']][['13']], anchor = 'w', justify = 'left')
    en.dir.save <- tkentry(frDirsave, textvariable = dir2save, width = largeur0)
    bt.dir.save <- tkbutton(frDirsave, text = "...")

    ###
    tkconfigure(bt.dir.save, command = function(){
        tcl('wm', 'attributes', tt, topmost = FALSE)
        dir2savepth <- tk_choose.dir(.cdtData$GalParams$dir2save, "")
        tcl('wm', 'attributes', tt, topmost = TRUE)
        if(is.na(dir2savepth)) tclvalue(dir2save) <- .cdtData$GalParams$dir2save
        else{
            dir.create(dir2savepth, showWarnings = FALSE, recursive = TRUE)
            tclvalue(dir2save) <- dir2savepth
        }
    })

    ###
    tkgrid(txt.dir.save, row = 0, column = 0, sticky = 'we', rowspan = 1, columnspan = 2, padx = 1, pady = 1, ipadx = 1, ipady = 1)
    tkgrid(en.dir.save, row = 1, column = 0, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
    tkgrid(bt.dir.save, row = 1, column = 1, sticky = 'w', rowspan = 1, columnspan = 1, padx = 0, pady = 1, ipadx = 1, ipady = 1)

    helpWidget(en.dir.save, lang.dlg[['tooltip']][['7']], lang.dlg[['status']][['7']])
    helpWidget(bt.dir.save, lang.dlg[['tooltip']][['8']], lang.dlg[['status']][['8']])

    ######
    tkgrid(frRFE, row = 0, column = 0, sticky = 'we', rowspan = 1, columnspan = 1, padx = 5, pady = 5, ipadx = 1, ipady = 1)
    tkgrid(frRegion, row = 1, column = 0, sticky = '', rowspan = 1, columnspan = 1, padx = 5, pady = 5, ipadx = 1, ipady = 1)
    tkgrid(frDirsave, row = 2, column = 0, sticky = 'we', rowspan = 1, columnspan = 1, padx = 5, pady = 5, ipadx = 1, ipady = 1)

    ###################################################

    btOK <- ttkbutton(frGrd1, text = lang.dlg[['button']][['1']])
    btCA <- ttkbutton(frGrd1, text = lang.dlg[['button']][['2']])

    tkconfigure(btOK, command = function(){
        if(trimws(tclvalue(dir2save)) %in% c("", "NA")){
            cdt.tkmessageBox(tt, message = lang.dlg[['message']][['1']], icon = "warning", type = "ok")
            tkwait.window(tt)
        }else{
            tkgrab.release(tt)
            tkdestroy(tt)
            tkfocus(.cdtEnv$tcl$main$win)
            tcl('update')

            .cdtData$GalParams$tstep <- periodVAL[CbperiodVAL %in% trimws(tclvalue(timeStep))]
            .cdtData$GalParams$rfe.src <- RFEVAL[CbRFEVAL %in% trimws(tclvalue(RFESrc))]
            .cdtData$GalParams$minhour <- as.numeric(trimws(tclvalue(minhour.tclVar)))
            .cdtData$GalParams$iridl.src <- switch(tclvalue(iridl.src), '0' = FALSE, '1' = TRUE)

            .cdtData$GalParams$chirps.global <- switch(tclvalue(chirps.global), '0' = FALSE, '1' = TRUE)
            .cdtData$GalParams$fewsAFdaily.type <- fewsDailyVAL[fewsDailyCB %in% trimws(tclvalue(fewsAFdaily.type))]

            .cdtData$GalParams$login$usr <- trimws(tclvalue(username))
            .cdtData$GalParams$login$pwd <- trimws(tclvalue(password))

            .cdtData$GalParams$dir2save <- trimws(tclvalue(dir2save))

            .cdtData$GalParams$bbox$minlon <- as.numeric(tclvalue(minLon))
            .cdtData$GalParams$bbox$maxlon <- as.numeric(tclvalue(maxLon))
            .cdtData$GalParams$bbox$minlat <- as.numeric(tclvalue(minLat))
            .cdtData$GalParams$bbox$maxlat <- as.numeric(tclvalue(maxLat))

            .cdtData$GalParams$message <- lang.dlg[['message']]

            if(testConnection()){
                Insert.Messages.Out(lang.dlg[['message']][['3']], TRUE, "i")
                ret <- try(exec.download_RFE(), silent = TRUE)
                # ret <- 0
                if(!inherits(ret, "try-error")){
                    if(ret == 0)
                        Insert.Messages.Out(lang.dlg[['message']][['4']], TRUE, "s")
                    if(ret == 1)
                        Insert.Messages.Out(lang.dlg[['message']][['6']], TRUE, "w")
                    if(ret == -1)
                        Insert.Messages.Out(lang.dlg[['message']][['7']], TRUE, "w")
                    if(ret == -2)
                        Insert.Messages.Out(lang.dlg[['message']][['8']], TRUE, "w")
                    if(ret == -3)
                        Insert.Messages.Out(lang.dlg[['message']][['5']], TRUE, "e")
                }else{
                    Insert.Messages.Out(gsub('[\r\n]', '', ret[1]), TRUE, "e")
                    Insert.Messages.Out(lang.dlg[['message']][['5']], TRUE, "e")
                }
            }else{
                Insert.Messages.Out(lang.dlg[['message']][['2']], format = TRUE)
                return(NULL)
            }
        }
    })

    tkconfigure(btCA, command = function(){
        tkgrab.release(tt)
        tkdestroy(tt)
        tkfocus(.cdtEnv$tcl$main$win)
    })

    tkgrid(btCA, row = 0, column = 0, sticky = 'w', padx = 5, pady = 1, ipadx = 1, ipady = 1)
    tkgrid(btOK, row = 0, column = 1, sticky = 'e', padx = 5, pady = 1, ipadx = 1, ipady = 1)

    #####
    tkgrid(frGrd0, row = 0, column = 0, sticky = 'nswe', rowspan = 1, columnspan = 2, padx = 1, pady = 1, ipadx = 1, ipady = 1)
    tkgrid(frGrd1, row = 1, column = 1, sticky = 'se', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)

    ###################################################

    tkwm.withdraw(tt)
    tcl('update')
    tt.w <- as.integer(tkwinfo("reqwidth", tt))
    tt.h <- as.integer(tkwinfo("reqheight", tt))
    tt.x <- as.integer(.cdtEnv$tcl$data$width.scr*0.5 - tt.w*0.5)
    tt.y <- as.integer(.cdtEnv$tcl$data$height.scr*0.5 - tt.h*0.5)
    tkwm.geometry(tt, paste0('+', tt.x, '+', tt.y))
    tkwm.transient(tt)
    tkwm.title(tt, lang.dlg[['title']])
    tkwm.deiconify(tt)
    tcl('wm', 'attributes', tt, topmost = TRUE)

    tkfocus(tt)
    tkbind(tt, "<Destroy>", function(){
        tkgrab.release(tt)
        tkfocus(.cdtEnv$tcl$main$win)
    })
    tkwait.window(tt)
}
