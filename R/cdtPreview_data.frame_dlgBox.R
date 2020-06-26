
## Preview data.frame
preview.data <- function(parent.win, fileopen){
    if(WindowsOS()){
        txta.w <- 41
        txta.h <- 8
        labprvw <- 50
    }else{
        txta.w <- 45
        txta.h <- 9
        labprvw <- 56
    }

    xml.dlg <- file.path(.cdtDir$dirLocal, "languages", "cdtPreview_data.frame_dlgBox.xml")
    lang.dlg <- cdtLanguageParse(xml.dlg, .cdtData$Config$lang.iso)

    ##############

    tt <- tktoplevel()
    tkgrab.set(tt)
    tkfocus(tt)

    fr0 <- tkframe(tt)
    fr1 <- tkframe(tt, relief = "groove", borderwidth = 2)
    fr2 <- tkframe(tt)

    ##############

    fr.delim <- ttklabelframe(fr0, text = lang.dlg[['label']][['1']], labelanchor = "nw", relief = "sunken", borderwidth = 2)

    etrval <- tclVar()
    rbval <- tclVar("sp")

    delim1 <- tkradiobutton(fr.delim, text = lang.dlg[['radiobutton']][['1']][1], variable = rbval, value = "tb", anchor = 'w', justify = 'left')
    delim2 <- tkradiobutton(fr.delim, text = lang.dlg[['radiobutton']][['1']][2], variable = rbval, value = "sp", anchor = 'w', justify = 'left')
    delim3 <- tkradiobutton(fr.delim, text = lang.dlg[['radiobutton']][['1']][3], variable = rbval, value = "sc", anchor = 'w', justify = 'left')
    delim4 <- tkradiobutton(fr.delim, text = lang.dlg[['radiobutton']][['1']][4], variable = rbval, value = "cm", anchor = 'w', justify = 'left')
    delim5 <- tkradiobutton(fr.delim, text = lang.dlg[['radiobutton']][['1']][5], variable = rbval, value = "ot", anchor = 'w', justify = 'left')
    vdelim5 <- tkentry(fr.delim, width = 6, textvariable = etrval)

    tkgrid(delim1, row = 0, column = 0, sticky = "we", padx = 1, pady = 1, ipadx = 1, ipady = 1)
    tkgrid(delim3, row = 0, column = 1, sticky = "we", padx = 1, pady = 1, ipadx = 1, ipady = 1)
    tkgrid(delim5, row = 0, column = 2, sticky = "we", padx = 1, pady = 1, ipadx = 1, ipady = 1)
    tkgrid(delim2, row = 1, column = 0, sticky = "we", padx = 1, pady = 1, ipadx = 1, ipady = 1)
    tkgrid(delim4, row = 1, column = 1, sticky = "we", padx = 1, pady = 1, ipadx = 1, ipady = 1)
    tkgrid(vdelim5, row = 1, column = 2, sticky = "we", padx = 1, pady = 1, ipadx = 1, ipady = 1)

    ##############

    fr.head <- ttklabelframe(fr0, text = lang.dlg[['label']][['2']], labelanchor = "nw", relief = "sunken", borderwidth = 2)

    etrval1 <- tclVar(1)
    vhead <- tclVar("FALSE")
    ishead <- c("TRUE", "FALSE")

    lb1 <- tklabel(fr.head, text = lang.dlg[['label']][['3']], anchor = 'w', justify = 'left')
    lb2 <- tklabel(fr.head, text = lang.dlg[['label']][['4']], anchor = 'w', justify = 'left')
    skp <- tkentry(fr.head, width = 6, textvariable = etrval1, justify = "center")
    head <- ttkcombobox(fr.head, values = ishead, textvariable = vhead, state = "readonly", width = 6, justify = "center")

    tkgrid(lb1, row = 0, column = 0, sticky = "we", padx = 1, pady = 1, ipadx = 1, ipady = 1)
    tkgrid(skp, row = 0, column = 1, sticky = "we", padx = 1, pady = 1, ipadx = 1, ipady = 1)
    tkgrid(lb2, row = 1, column = 0, sticky = "we", padx = 1, pady = 1, ipadx = 1, ipady = 1)
    tkgrid(head, row = 1, column = 1, sticky = "we", padx = 1, pady = 1, ipadx = 1, ipady = 1)

    ##############
    tkgrid(fr.delim, row = 0, column = 0, padx = 1, pady = 2, ipadx = 2, ipady = 2, sticky = 'nswe')
    tkgrid(fr.head, row = 0, column = 1, padx = 1, pady = 2, ipadx = 2, ipady = 2, sticky = 'nswe')

    ##############
 
    labtxt <- tklabel(fr1, text = paste(lang.dlg[['label']][['5']], fileopen), width = labprvw, anchor = 'w', justify = 'left')
    infobulle(labtxt, fileopen)

    xscr <- tkscrollbar(fr1, repeatinterval = 5, orient = "horizontal",
                        command = function(...) tkxview(txta, ...))
    yscr <- tkscrollbar(fr1, repeatinterval = 5,
                        command = function(...) tkyview(txta, ...))
    txta <- tktext(fr1, bg = "white", font = "courier", width = txta.w, height = txta.h,
                    xscrollcommand = function(...) tkset(xscr, ...),
                    yscrollcommand = function(...) tkset(yscr, ...), wrap = "none")

    tkgrid(labtxt, row = 0, column = 0, sticky = 'we')
    tkgrid(txta, yscr)
    tkgrid(xscr)
    tkgrid.configure(txta, row = 1, column = 0, sticky = "nsew")
    tkgrid.configure(yscr, sticky = "ns")
    tkgrid.configure(xscr, sticky = "ew")

    font <- tkfont.create(family = "times", weight = "normal", slant = "roman", size = 10)
    tktag.configure(txta, "dfpreview", font = font)

    rdL <- try(readLines(fileopen, n = 10, warn = FALSE), silent = TRUE)
    if(inherits(rdL, "try-error")){
        Insert.Messages.Out(paste(lang.dlg[['message']][['1']], fileopen), format = TRUE)
        tkgrab.release(tt)
        tkdestroy(tt)
        tkfocus(parent.win)
        return(NULL)
    }else{
        for(i in 1:length(rdL))
            tkinsert(txta, "end", paste(rdL[i], "\n"), "dfpreview")
        tcl("update")
    }

    ##############

    fr.miss <- tkframe(fr2, relief = "groove", borderwidth = 2)

    etrmiss <- tclVar(.cdtData$Config$missval)

    lbmiss <- tklabel(fr.miss, text = lang.dlg[['label']][['6']], anchor = 'w', justify = 'left')
    missvl <- tkentry(fr.miss, width = 8, textvariable = etrmiss, justify = "center")

    tkgrid(lbmiss, row = 0, column = 0, sticky = 'w', padx = 5, pady = 1, ipadx = 1, ipady = 1)
    tkgrid(missvl, row = 0, column = 1, sticky = 'e', padx = 5, pady = 1, ipadx = 1, ipady = 1)

    ##############

    fr.but <- tkframe(fr2)

    OK.but <- ttkbutton(fr.but, text = .cdtEnv$tcl$lang$global[['button']][['1']])
    CA.but <- ttkbutton(fr.but, text = .cdtEnv$tcl$lang$global[['button']][['2']])

    separator <- NULL

    tkconfigure(OK.but, command = function() {
        delim <- as.character(tclvalue(rbval))
        if(delim == 'tb') sepr <- ""
        if(delim == 'sp') sepr <- ""
        # if(delim == 'tb') sepr <- "\t"
        # if(delim == 'sp') sepr <- " "
        if(delim == 'sc') sepr <- ";"
        if(delim == 'cm') sepr <- ","
        if(delim == 'ot'){
            tclvalue(etrval) <- tclvalue(tkget(vdelim5))
            sepr <- tclvalue(etrval)
        }
        
        header <- as.logical(tclvalue(vhead))
        tclvalue(etrval1) <- tclvalue(tkget(skp))
        skip <- as.integer(tclvalue(etrval1))
        missval <- as.character(tclvalue(etrmiss))
        separator <<- list(sepr = sepr, header = header, skip = skip, miss.val = missval)
        
        tkgrab.release(tt)
        tkdestroy(tt)
        tkfocus(parent.win)
    })

    tkconfigure(CA.but, command = function(){
        separator <<- NULL
        tkgrab.release(tt)
        tkdestroy(tt)
        tkfocus(parent.win)
    })

    tkgrid(OK.but, row = 0, column = 0, sticky = 'we', padx = 5, pady = 1, ipadx = 1, ipady = 1)
    tkgrid(CA.but, row = 0, column = 1, sticky = 'we', padx = 5, pady = 1, ipadx = 1, ipady = 1)

    ##############
    tkgrid(fr.miss, row = 0, column = 0, padx = 1, ipadx = 1, sticky = 'we')
    tkgrid(fr.but, row = 0, column = 1, padx = 5, ipadx = 5, sticky = 'we')

    #####################
    tkgrid(fr0, row = 0, column = 0, sticky = 'snwe', padx = 5, pady = 5)
    tkgrid(fr1, row = 1, column = 0, sticky = 'we', padx = 5, pady = 5)
    tkgrid(fr2, row = 2, column = 0, padx = 5, pady = 5)

    #####################

    tkwm.withdraw(tt)
    tcl('update')
    tt.w <- as.integer(tkwinfo("reqwidth", tt))
    tt.h <- as.integer(tkwinfo("reqheight", tt))
    tt.x <- as.integer(.cdtEnv$tcl$data$width.scr*0.5 - tt.w*0.5)
    tt.y <- as.integer(.cdtEnv$tcl$data$height.scr*0.5 - tt.h*0.5)
    tkwm.geometry(tt, paste0('+', tt.x, '+', tt.y))
    tkwm.transient(tt)
    tkwm.title(tt, paste(lang.dlg[['title']], "-", basename(fileopen)))
    tkwm.deiconify(tt)
    tcl('wm', 'attributes', tt, topmost = TRUE)

    tkfocus(tt)
    tkbind(tt, "<Destroy>", function(){
        tkgrab.release(tt)
        tkfocus(parent.win)
    })
    tkwait.window(tt)
    return(separator)
}
