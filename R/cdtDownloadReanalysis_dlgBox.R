
reanal.product.info <- function(prod, src){
    urls <- .cdtData$EnvData$rnlProd[[prod]]$pars[[src]]$source

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

reanal.need.usrpwd <- function(prod, src){
    src_opts <- .cdtData$EnvData$rnlProd[[prod]]$pars[[src]]

    usrpwd <- FALSE
    if(length(src_opts$registration) > 0){
        usrpwd <- if(src_opts$registration == "") FALSE else TRUE
    }
    urllog <- src_opts$registration

    list(usrpwd = usrpwd, urllog = urllog)
}

get_reanalysis.products <- function(){
    optFile <- file.path(.cdtDir$Root, "reanalysis", "reanalysis_options.csv")
    opts <- utils::read.table(optFile, sep = ',', header = TRUE, na.strings = '',
                              colClasses = 'character', stringsAsFactors = FALSE,
                              comment.char = ";")
    ix <- which(!is.na(opts$name))
    ie <- c(ix[-1] - 1, nrow(opts))
    xx <- lapply(seq_along(ix), function(i){
        x <- opts[ix[i]:ie[i], ]
        x1 <- trimws(x$name)
        x1[x1 == ""] <- NA
        x1 <- x1[!is.na(x1)]
        x2 <- trimws(x$longname)
        x2[x2 == ""] <- NA
        x2 <- x2[!is.na(x2)]

        xs <- which(!is.na(x$opt_name))
        xe <- c(xs[-1] - 1, length(x$opt_name))
        yy <- lapply(seq_along(xs), function(j){
            y <- x[xs[j]:xe[j], ]
            y1 <- trimws(y$opt_name)
            y1[y1 == ""] <- NA
            y1 <- y1[!is.na(y1)]
            y2 <- trimws(y$par_name)
            y2[y2 == ""] <- NA
            y2 <- y2[!is.na(y2)]
            y3 <- trimws(y$var_name)
            y3[y3 == ""] <- NA
            y3 <- y3[!is.na(y3)]
            y4 <- trimws(y$source)
            y4[y4 == ""] <- NA
            y4 <- y4[!is.na(y4)]
            y5 <- trimws(y$registration)
            y5[y5 == ""] <- NA
            y5 <- y5[!is.na(y5)]
            if(length(y5) == 0) y5 <- ""
            y6 <- trimws(y$timestep)
            y6[y6 == ""] <- NA
            y6 <- y6[!is.na(y6)]

            list(opt_name = y1, par_name = y2, var_name = y3,
                 source = y4, registration = y5, timestep = y6)
        })

        opt_name <- sapply(yy, '[[', 'opt_name')
        yy <- lapply(yy, function(y) y[!names(y) %in% 'opt_name'])
        names(yy) <- opt_name

        if(length(opt_name) == 1){
            opt_name <- c(opt_name, '')
        }

        list(name = x1, longname = x2, opt_name = opt_name, pars = yy)
    })
    names(xx) <- sapply(xx, '[[', 'name')

    .cdtData$EnvData$rnlProd <- xx
}

#######################################

download_Reanalysis <- function(){
    if(WindowsOS()){
        largeur0 <- 65
        largeur1 <- 27
        largeur2 <- 37
        largeur3 <- 20
        largeur5 <- 42
        largeur6 <- 30
        largeur7 <- 18
    }else{
        largeur0 <- 61
        largeur1 <- 27
        largeur2 <- 37
        largeur3 <- 20
        largeur5 <- 42
        largeur6 <- 30
        largeur7 <- 15
    }

    xml.dlg <- file.path(.cdtDir$dirLocal, "languages", "cdtDownloadReanalysis_dlgBox.xml")
    lang.dlg <- cdtLanguageParse(xml.dlg, .cdtData$Config$lang.iso)

    ## load reanalysis products
    get_reanalysis.products()

    #########
    tt <- tktoplevel()
    tkgrab.set(tt)
    tkfocus(tt)

    frGrd0 <- tkframe(tt, relief = 'raised', borderwidth = 2)
    frGrd1 <- tkframe(tt)

    ###################################################

    frRNLPROD <- tkframe(frGrd0, relief = 'sunken', bd = 2)

    #############
    reanalProd <- tclVar()
    CbprodVAL <- sapply(.cdtData$EnvData$rnlProd, '[[', 'longname')
    prodVAL <- sapply(.cdtData$EnvData$rnlProd, '[[', 'name')
    tclvalue(reanalProd) <- CbprodVAL[prodVAL %in% .cdtData$GalParams$prod]

    #######
    reanal_opts <- .cdtData$EnvData$rnlProd[[.cdtData$GalParams$prod]]

    CbsrcVAL <- reanal_opts$opt_name
    reanalSrc <- tclVar(.cdtData$GalParams$src)

    #######
    downVar <- tclVar()
    CbvarsVAL <- reanal_opts$pars[[.cdtData$GalParams$src]]$var_name
    varsVAL <- reanal_opts$pars[[.cdtData$GalParams$src]]$par_name
    tclvalue(downVar) <- CbvarsVAL[varsVAL %in% .cdtData$GalParams$var]

    #######
    need.pwd <- reanal.need.usrpwd(.cdtData$GalParams$prod, .cdtData$GalParams$src)
    statepwd <- if(need.pwd$usrpwd) "normal" else "disabled"
    url.log <- tclVar(need.pwd$urllog)
    username <- tclVar(.cdtData$GalParams$login$usr)
    password <- tclVar(.cdtData$GalParams$login$pwd)

    ###########################

    frREAN <- tkframe(frRNLPROD)
    txt.prod <- tklabel(frREAN, text = lang.dlg[['label']][['2']], anchor = 'w', justify = 'left')
    cb.prod <- ttkcombobox(frREAN, values = CbprodVAL, textvariable = reanalProd, width = largeur3)
    txt.src <- tklabel(frREAN, text = lang.dlg[['label']][['3']], anchor = 'w', justify = 'right')
    cb.src <- ttkcombobox(frREAN, values = CbsrcVAL, textvariable = reanalSrc, width = largeur5)

    frVARDATE <- tkframe(frRNLPROD)
    txt.vars <- tklabel(frVARDATE, text = lang.dlg[['label']][['1']], anchor = 'w', justify = 'left')
    cb.vars <- ttkcombobox(frVARDATE, values = CbvarsVAL, textvariable = downVar, width = largeur2)

    bt.range <- ttkbutton(frRNLPROD, text = lang.dlg[['button']][['3']])

    ####
    frLOGIN <- tkframe(frRNLPROD)
    txt.log1 <- tklabel(frLOGIN, text = lang.dlg[['label']][['4']], anchor = 'e', justify = 'right')
    txt.log2 <- tklabel(frLOGIN, text = tclvalue(url.log), textvariable = url.log, anchor = 'w', justify = 'left')

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
    frUSER <- tkframe(frRNLPROD)
    txt.usr <- tklabel(frUSER, text = lang.dlg[['label']][['5']], anchor = 'e', justify = 'right')
    en.usr <- tkentry(frUSER, textvariable = username, state = statepwd, width = largeur6, justify = "left")
    txt.pwd <- tklabel(frUSER, text = lang.dlg[['label']][['6']], anchor = 'e', justify = 'right')
    en.pwd <- tkentry(frUSER, textvariable = password, show = "*", state = statepwd, width = largeur7, justify = "left")

    ####
    bt.info <- ttkbutton(frRNLPROD, text = lang.dlg[['button']][['4']])
    bt.tcover <- ttkbutton(frRNLPROD, text = lang.dlg[['button']][['5']])

    ###########################

    tkconfigure(bt.range, command = function(){
        prod <- prodVAL[CbprodVAL %in% trimws(tclvalue(reanalProd))]
        src <- trimws(tclvalue(reanalSrc))
        tstep <- .cdtData$EnvData$rnlProd[[prod]]$pars[[src]]$timestep

        tcl('wm', 'attributes', tt, topmost = FALSE)
        .cdtData$GalParams[["date.range"]] <- getInfoDateRange(tt, .cdtData$GalParams[["date.range"]], tstep, TRUE)
        tcl('wm', 'attributes', tt, topmost = TRUE)
    })

    tkconfigure(bt.info, command = function(){
        prod <- prodVAL[CbprodVAL %in% trimws(tclvalue(reanalProd))]
        src <- trimws(tclvalue(reanalSrc))
        urls <- reanal.product.info(prod, src)
        utils::browseURL(paste0('file://', urls))
    })

    tkconfigure(bt.tcover, command = function(){
        tkgrab.release(tt)
        tkdestroy(tt)
        tkfocus(.cdtEnv$tcl$main$win)
        tcl('update')

        .cdtData$GalParams$var <- varsVAL[CbvarsVAL %in% trimws(tclvalue(downVar))]
        .cdtData$GalParams$prod <- prodVAL[CbprodVAL %in% trimws(tclvalue(reanalProd))]
        .cdtData$GalParams$src <- trimws(tclvalue(reanalSrc))
        .cdtData$GalParams$reanalysis <- trimws(tclvalue(reanalProd))

        .cdtData$GalParams$login$usr <- trimws(tclvalue(username))
        .cdtData$GalParams$login$pwd <- trimws(tclvalue(password))

        if(testConnection()){
            Insert.Messages.Out(lang.dlg[['message']][['11']], TRUE, "i")
            ret <- try(check.coverage_Reanalysis(), silent = TRUE)
            # ret <- NULL
            # ret$end <- 0
            if(!inherits(ret, "try-error")){
                if(is.null(ret$end)){
                    msg <- paste(ret$name, ret$timestep)
                    Insert.Messages.Out(msg, TRUE, "e")
                    Insert.Messages.Out(lang.dlg[['message']][['12']], TRUE, "e")
                }else{
                    Insert.Messages.Out(lang.dlg[['message']][['13']], TRUE, "s")
                }
            }else{
                Insert.Messages.Out(gsub('[\r\n]', '', ret[1]), TRUE, "e")
                Insert.Messages.Out(lang.dlg[['message']][['12']], TRUE, "e")
            }
        }else{
            Insert.Messages.Out(lang.dlg[['message']][['2']], format = TRUE)
            return(NULL)
        }
    })

    ###########################

    tkgrid(txt.prod, row = 0, column = 0, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
    tkgrid(cb.prod, row = 1, column = 0, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
    tkgrid(txt.src, row = 0, column = 1, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
    tkgrid(cb.src, row = 1, column = 1, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)

    tkgrid(txt.vars, row = 0, column = 0, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
    tkgrid(cb.vars, row = 1, column = 0, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)

    tkgrid(txt.log1, row = 0, column = 0, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
    tkgrid(txt.log2, row = 0, column = 1, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)

    tkgrid(txt.usr, row = 0, column = 0, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
    tkgrid(en.usr, row = 0, column = 1, sticky = 'we', rowspan = 1, columnspan = 4, padx = 1, pady = 1, ipadx = 1, ipady = 1)
    tkgrid(txt.pwd, row = 0, column = 5, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
    tkgrid(en.pwd, row = 0, column = 6, sticky = 'we', rowspan = 1, columnspan = 2, padx = 1, pady = 1, ipadx = 1, ipady = 1)

    tkgrid(frREAN, row = 0, column = 0, sticky = 'we', rowspan = 1, columnspan = 8, padx = 1, pady = 1, ipadx = 1, ipady = 1)
    tkgrid(frVARDATE, row = 1, column = 0, sticky = 'we', rowspan = 1, columnspan = 5, padx = 1, pady = 1, ipadx = 1, ipady = 1)
    tkgrid(bt.range, row = 1, column = 5, sticky = 'we', rowspan = 1, columnspan = 3, padx = 1, pady = 1, ipadx = 1, ipady = 1)

    tkgrid(frLOGIN, row = 2, column = 0, sticky = 'we', rowspan = 1, columnspan = 8, padx = 1, pady = 1, ipadx = 1, ipady = 1)
    tkgrid(frUSER, row = 3, column = 0, sticky = 'we', rowspan = 1, columnspan = 8, padx = 1, pady = 1, ipadx = 1, ipady = 1)

    tkgrid(bt.info, row = 4, column = 0, sticky = 'we', rowspan = 1, columnspan = 4, padx = 1, pady = 5, ipadx = 1, ipady = 1)
    tkgrid(bt.tcover, row = 4, column = 4, sticky = 'we', rowspan = 1, columnspan = 4, padx = 1, pady = 5, ipadx = 1, ipady = 1)

    helpWidget(cb.vars, lang.dlg[['tooltip']][['1']], lang.dlg[['status']][['1']])
    helpWidget(cb.prod, lang.dlg[['tooltip']][['2']], lang.dlg[['status']][['2']])
    helpWidget(cb.src, lang.dlg[['tooltip']][['2a']], lang.dlg[['status']][['2a']])
    helpWidget(bt.range, lang.dlg[['tooltip']][['2b']], lang.dlg[['status']][['2b']])

    ###########################

    tkbind(cb.prod, "<<ComboboxSelected>>", function(){
        prod <- prodVAL[CbprodVAL %in% trimws(tclvalue(reanalProd))]
        reanal_opts <- .cdtData$EnvData$rnlProd[[prod]]
        CbsrcVAL <- reanal_opts$opt_name

        tkconfigure(cb.src, values = CbsrcVAL)
        tclvalue(reanalSrc) <- CbsrcVAL[1]

        ########
        src <- trimws(tclvalue(reanalSrc))

        need.pwd <- reanal.need.usrpwd(prod, src)
        statepwd <- if(need.pwd$usrpwd) "normal" else "disabled"

        tclvalue(url.log) <- need.pwd$urllog
        tkconfigure(en.usr, state = statepwd)
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
        CbvarsVAL <<- reanal_opts$pars[[src]]$var_name
        varsVAL <<- reanal_opts$pars[[src]]$par_name

        tkconfigure(cb.vars, values = CbvarsVAL)
        if(!trimws(tclvalue(downVar)) %in% CbvarsVAL)
            tclvalue(downVar) <- CbvarsVAL[1]
    })

    tkbind(cb.src, "<<ComboboxSelected>>", function(){
        prod <- prodVAL[CbprodVAL %in% trimws(tclvalue(reanalProd))]
        src <- trimws(tclvalue(reanalSrc))
        reanal_opts <- .cdtData$EnvData$rnlProd[[prod]]

        CbvarsVAL <<- reanal_opts$pars[[src]]$var_name
        varsVAL <<- reanal_opts$pars[[src]]$par_name

        tkconfigure(cb.vars, values = CbvarsVAL)
        if(!trimws(tclvalue(downVar)) %in% CbvarsVAL)
            tclvalue(downVar) <- CbvarsVAL[1]

        ########
        need.pwd <- reanal.need.usrpwd(prod, src)
        statepwd <- if(need.pwd$usrpwd) "normal" else "disabled"

        tclvalue(url.log) <- need.pwd$urllog
        tkconfigure(en.usr, state = statepwd)
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
    tkbind(txt.log2, "<Button-1>", function(){
        prod <- prodVAL[CbprodVAL %in% trimws(tclvalue(reanalProd))]
        src <- trimws(tclvalue(reanalSrc))
        need.pwd <- reanal.need.usrpwd(prod, src)
        if(need.pwd$usrpwd){
            utils::browseURL(need.pwd$urllog)
        }
    })

    tkbind(txt.log2, "<Enter>", function(){
        prod <- prodVAL[CbprodVAL %in% trimws(tclvalue(reanalProd))]
        src <- trimws(tclvalue(reanalSrc))
        need.pwd <- reanal.need.usrpwd(prod, src)
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

    ###########################

    tkgrid(frRNLPROD, row = 0, column = 0, sticky = 'we', rowspan = 1, columnspan = 1, padx = 5, pady = 5, ipadx = 1, ipady = 1)
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

            .cdtData$GalParams$var <- varsVAL[CbvarsVAL %in% trimws(tclvalue(downVar))]
            .cdtData$GalParams$prod <- prodVAL[CbprodVAL %in% trimws(tclvalue(reanalProd))]
            .cdtData$GalParams$src <- trimws(tclvalue(reanalSrc))

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
                ret <- try(exec.download_Reanalysis(), silent = TRUE)
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
                        Insert.Messages.Out(lang.dlg[['message']][['9']], TRUE, "e")
                    if(ret == -4)
                        Insert.Messages.Out(lang.dlg[['message']][['10']], TRUE, "e")
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
