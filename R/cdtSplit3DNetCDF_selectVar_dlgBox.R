
split_3d.netcdf_selectVariable <- function(parent.win, ncfile){
    if(WindowsOS()){
        txta.w <- 38
        txta.h <- 7
        largeur0 <- 43
        largeur1 <- 17
    }else{
        txta.w <- 40
        txta.h <- 9
        largeur0 <- 26
        largeur1 <- 13
    }

    xml.dlg <- file.path(.cdtDir$dirLocal, "languages", "cdtSplit3DNetCDF_selectVar_dlgBox.xml")
    lang.dlg <- cdtLanguageParse(xml.dlg, .cdtData$Config$lang.iso)

    ############################################

    tt <- tktoplevel()
    tkgrab.set(tt)
    tkfocus(tt)

    frMRG0 <- tkframe(tt, relief = 'raised', borderwidth = 2)
    frMRG1 <- tkframe(tt)
    frInn <- tkframe(frMRG0, relief = "sunken", borderwidth = 2)

    ##################################################################

    nc <- try(nc_open(ncfile), silent = TRUE)
    if(inherits(nc, "try-error")){
        Insert.Messages.Out(paste(lang.dlg[['message']][['1']], ncfile), format = TRUE)
        tkgrab.release(tt)
        tkdestroy(tt)
        tkfocus(parent.win)
        return(NULL)
    }

    ncvar <- nc$nvars
    ncdim <- nc$ndims
    var.info <- lapply(nc$var, function(v) v[c('name', 'ndims', 'size', 'dimids', 'prec', 'units', 'longname')])
    dim.info <- lapply(nc$dim, function(v) v[c('name', 'len', 'id', 'units', 'vals')])
    att.dim <- lapply(seq(ncdim), function(i){
        att <- ncatt_get(nc, nc$dim[[i]]$name)
        nom <- names(att)
        nameL <- grepl('name', nom, ignore.case = TRUE)
        ix0 <- which(grepl('long', nom, ignore.case = TRUE) & nameL)
        ix1 <- which(grepl('standard', nom, ignore.case = TRUE) & nameL)
        ix2 <- which(nameL)[1]
        if(length(ix0) > 0){
            paste0(nom[ix0], ': ', att[[ix0]], collapse = "")
        }else{
            if(length(ix1) > 0){
                paste0(nom[ix1], ': ', att[[ix1]], collapse = "")
            }else{
                if(length(ix2) > 0){
                    paste0(nom[ix2], ': ', att[[ix2]], collapse = "")
                }else{
                    NULL
                }
            }
        }
    })
    names(att.dim) <- sapply(dim.info, '[[', 'name')
    calendar <- lapply(seq(ncdim), function(i) ncatt_get(nc, nc$dim[[i]]$name)$calendar)
    names(calendar) <- sapply(dim.info, '[[', 'name')
    calendar <- calendar[!sapply(calendar, is.null)]
    nc_close(nc)

    ##################################################################

    if(ncdim < 3){
        Insert.Messages.Out(lang.dlg[['message']][['2']], format = TRUE)
        tkgrab.release(tt)
        tkdestroy(tt)
        tkfocus(parent.win)
        return(NULL)
    }

    ##################################################################

    disp0 <- paste("File", basename(ncfile), "has")
    disp1 <- paste(ncvar, "variables:")
    disp2 <- sapply(var.info, function(v){
        paste0('\t', v$prec, '\t',
        paste0(v$name, '[',
        paste0(sapply(dim.info, '[[', 'name')[v$dimids + 1], collapse = ','),
        ']'), '\t', 'Longname: ' , v$longname, '\t', 'Units: ', v$units)
    })
    disp3 <- paste(ncdim, "dimensions:")
    disp4 <- sapply(dim.info, function(v){
        paste0('\t', v$name, '\t',
        'Size: ', v$len, '\t',
        'Units: ', v$units, '\t',
        att.dim[[v$name]])
    })

    displayInfovras <- paste0(c(disp0, disp1, disp2, disp3, disp4, "\n"), collapse = "\n")

    ##################################################################

    frWidg <- ttklabelframe(frInn, text = lang.dlg[['label']][['1']], labelanchor = "nw", relief = "groove", borderwidth = 2)

    var.choix <- c(lang.dlg[['label']][['3']], sapply(var.info, function(v) paste(v$name, '::', v$longname)))
    dim.choix <- c("", sapply(dim.info, function(v) paste(v$name, '::', att.dim[[v$name]])))

    var.Sel <- tclVar(var.choix[1])
    dim.Sel <- lapply(seq_along(dim.info), function(i) tclVar(dim.choix[1]))

    txt.var <- tklabel(frWidg, text = "Variable:", anchor = 'e', justify = 'right')
    cb.var <- ttkcombobox(frWidg, values = var.choix, textvariable = var.Sel, state = "readonly", width = largeur0)

    lab.LLT <- c("Longitude:", "Latitude:", "Time:")
    txt.LTT <- lapply(1:3, function(i)
        tklabel(frWidg, text = lab.LLT[i], anchor = 'e', justify = 'right'))
    cb.LLT <- lapply(1:3, function(i) 
        ttkcombobox(frWidg, values = dim.choix, textvariable = dim.Sel[[i]], state = 'disabled', width = largeur0))

    nD <- ncdim - 3
    if(nD > 0){
        dim.Val <- lapply(seq(nD), function(i) tclVar(""))
        txt.DIM <- lapply(seq(nD), function(i){
            tklabel(frWidg, text = paste0("Dim_", i), anchor = 'e', justify = 'right')
        })

        cb.Sel <- lapply(seq(nD), function(i){
            ttkcombobox(frWidg, values = dim.choix, textvariable = dim.Sel[[i + 3]], state = 'disabled', width = largeur0)
        })

        cb.Val <- lapply(seq(nD), function(i){
            ttkcombobox(frWidg, values = "", textvariable = dim.Val[[i]], state = 'disabled', width = largeur1)
        })
    }

    tkgrid(txt.var, row = 0, column = 0, sticky = 'we', padx = 1, pady = 5)
    tkgrid(cb.var, row = 0, column = 1, sticky = 'we', padx = 1, pady = 5)
    for(i in 1:3){
        tkgrid(txt.LTT[[i]], row = i, column = 0, sticky = 'we', padx = 1, pady = 5)
        tkgrid(cb.LLT[[i]], row = i, column = 1, sticky = 'we', padx = 1, pady = 5)
    }
    if(nD > 0){
        for(i in 1:nD){
            tkgrid(txt.DIM[[i]], row = i + 3, column = 0, sticky = 'we', padx = 1, pady = 5)
            tkgrid(cb.Sel[[i]], row = i + 3, column = 1, sticky = 'we', padx = 1, pady = 5)
            tkgrid(cb.Val[[i]], row = i + 3, column = 2, sticky = 'we', padx = 1, pady = 5)
        }
    }

    ############################################

    ivar <- 0
    tkbind(cb.var, "<<ComboboxSelected>>", function(){
        ivar <<- as.integer(tclvalue(tcl(cb.var, 'current')))
        if(ivar > 0){
            if(var.info[[ivar]]$ndims < 3){
                cdt.tkmessageBox(tt, message = lang.dlg[['message']][['3']], icon = "warning", type = "ok")

                for(i in 1:3)
                    tkconfigure(cb.LLT[[i]], state = 'disabled')
                for(i in seq_along(dim.Sel))
                    tclvalue(dim.Sel[[i]]) <- ""

                if(nD > 0){
                    for(i in 1:nD){
                        tkconfigure(cb.Sel[[i]], state = 'disabled')
                        tclvalue(dim.Val[[i]]) <- ""
                        tkconfigure(cb.Val[[i]], state = 'disabled', values = "")
                    }
                }
            }else{
                for(i in 1:3) tkconfigure(cb.LLT[[i]], state = 'normal')
                if(nD > 0){
                    nV <- var.info[[ivar]]$ndims
                    if(nV > 3){
                        nA <- ncdim - nV
                        nL <- nD - nA
                        for(i in 1:nL)
                            tkconfigure(cb.Sel[[i]], state = 'normal')
                        if(nA > 0){
                            for(i in (nL + 1):nD){
                                tclvalue(dim.Sel[[i + 3]]) <- ""
                                tkconfigure(cb.Sel[[i]], state = 'disabled')
                                tclvalue(dim.Val[[i]]) <- ""
                                tkconfigure(cb.Val[[i]], state = 'disabled', values = "")
                            }
                        }
                    }else{
                        for(i in 1:nD){
                            tclvalue(dim.Sel[[i + 3]]) <- ""
                            tkconfigure(cb.Sel[[i]], state = 'disabled')
                            tclvalue(dim.Val[[i]]) <- ""
                            tkconfigure(cb.Val[[i]], state = 'disabled', values = "")
                        }
                    }
                }
            }
        }else{
            for(i in 1:3)
                tkconfigure(cb.LLT[[i]], state = 'disabled')
            for(i in seq_along(dim.Sel))
                tclvalue(dim.Sel[[i]]) <- ""

            if(nD > 0){
                for(i in 1:nD){
                    tkconfigure(cb.Sel[[i]], state = 'disabled')
                    tclvalue(dim.Val[[i]]) <- ""
                    tkconfigure(cb.Val[[i]], state = 'disabled', values = "")
                }
            }
        }
    })

    if(nD > 0){
        ret <-lapply(1:nD, function(i){
            tkbind(cb.Sel[[i]], "<<ComboboxSelected>>", function(){
                id <- as.integer(tclvalue(tcl(cb.Sel[[i]], 'current')))
                if(id > 0){
                    values <- dim.info[[id]]$vals
                    tclvalue(dim.Val[[i]]) <- values[1]
                    tkconfigure(cb.Val[[i]], state = "normal", values = values)
                }else{
                    tclvalue(dim.Val[[i]]) <- ""
                    tkconfigure(cb.Val[[i]], state = "disabled", values = "")
                }
            })
        })
    }

    ############################################

    frText <- ttklabelframe(frInn, text = lang.dlg[['label']][['2']], labelanchor = "nw", relief = "groove", borderwidth = 2)

    xscr <- tkscrollbar(frText, repeatinterval = 5, orient = "horizontal",
                        command = function(...) tkxview(txta, ...))
    yscr <- tkscrollbar(frText, repeatinterval = 5,
                        command = function(...) tkyview(txta, ...))
    txta <- tktext(frText, bg = "white", font = "courier", wrap = "none",
                    xscrollcommand = function(...) tkset(xscr, ...),
                    yscrollcommand = function(...) tkset(yscr, ...),
                    width = txta.w, height = txta.h)

    tkgrid(txta, yscr)
    tkgrid(xscr)
    tkgrid.configure(txta, row = 0, column = 0, sticky = "nsew")
    tkgrid.configure(yscr, sticky = "ns")
    tkgrid.configure(xscr, sticky = "ew")

    font <- tkfont.create(family = "times", weight = "normal", slant = "roman", size = 10)
    tktag.configure(txta, "splitnc", font = font)

    tkinsert(txta, "end", displayInfovras, "splitnc")

    ############################################

    tkgrid(frWidg, row = 0, column = 0, sticky = 'we', padx = 1, pady = 1, ipadx = 1, ipady = 1)
    tkgrid(frText, row = 1, column = 0, sticky = 'we', padx = 1, pady = 1, ipadx = 1, ipady = 1)

    ############################################

    tkgrid(frInn, row = 0, column = 0, sticky = 'news', padx = 5, pady = 1, ipadx = 1, ipady = 1)

    ############################################

    bt.prm.OK <- ttkbutton(frMRG1, text = .cdtEnv$tcl$lang$global[['button']][['1']])
    bt.prm.CA <- ttkbutton(frMRG1, text = .cdtEnv$tcl$lang$global[['button']][['2']])

    ####
    retval <- NULL
    tkconfigure(bt.prm.OK, command = function(){
        cbB <- if(var.info[[ivar]]$ndims > 3) c(cb.LLT, cb.Sel) else cb.LLT
        idx <- sapply(cbB, function(v) as.integer(tclvalue(tcl(v, 'current'))))
        if(ivar > 0)
            idx <- idx[seq(var.info[[ivar]]$ndims)]
        if(ivar == 0){
            cdt.tkmessageBox(tt, message = lang.dlg[['message']][['4']], icon = "warning", type = "ok")
            tkwait.window(tt)
        }else if(any(idx == 0)){
            cdt.tkmessageBox(tt, message = lang.dlg[['message']][['5']], icon = "warning", type = "ok")
            tkwait.window(tt)
        }else{
            vars <- var.info[[ivar]]
            ov.dim <- sapply(dim.info, '[[', 'name')[vars$dimids + 1]
            ilon <- which(ov.dim == dim.info[[idx[1]]]$name)
            ilat <- which(ov.dim == dim.info[[idx[2]]]$name)
            rev <- if(ilat < ilon) TRUE else FALSE

            dims <- lapply(seq_along(idx), function(i){
                x <- dim.info[[idx[i]]]
                if(i %in% 1:2){
                    crd <- x$vals
                    ord <- order(crd)
                    crd <- crd[ord]
                    c(x[c('name', 'len', 'units')], list(vals = crd, order = ord))
                }else if(i == 3){
                    x[c('name', 'units')]
                }else{
                    isel <- as.integer(tclvalue(tcl(cb.Val[[i - 3]], 'current'))) + 1
                    c(x, list(sel = isel))
                }
            })

            retval <<- list(var = vars, var.dim = ov.dim, rev = rev, dim = dims)

            tkgrab.release(tt)
            tkdestroy(tt)
            tkfocus(parent.win)
        }
    })

    tkconfigure(bt.prm.CA, command = function(){
        retval <<- NULL
        tkgrab.release(tt)
        tkdestroy(tt)
        tkfocus(parent.win)
    })

    ####
    tkgrid(bt.prm.OK, row = 0, column = 0, sticky = 'w', padx = 5, pady = 1, ipadx = 1, ipady = 1)
    tkgrid(bt.prm.CA, row = 0, column = 1, sticky = 'e', padx = 5, pady = 1, ipadx = 1, ipady = 1)

    ############################################
    
    tkgrid(frMRG0, row = 0, column = 0, sticky = 'nswe', rowspan = 1, columnspan = 2, padx = 1, pady = 1, ipadx = 1, ipady = 1)
    tkgrid(frMRG1, row = 1, column = 1, sticky = 'se', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)

    ###########################
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
    tkbind(tt, "<Destroy>", function() {
        tkgrab.release(tt)
        tkfocus(parent.win)
    })
    tkwait.window(tt)
    return(retval)
}
