
minimum_Fraction_Info <- function(parent.win, min.frac){
    largeur0 <- if(WindowsOS()) 20 else 20

    ################################

    tt1 <- tktoplevel()
    tkgrab.set(tt1)
    tkfocus(tt1)

    frMRG0 <- tkframe(tt1, relief = 'raised', borderwidth = 2)
    frMRG1 <- tkframe(tt1)

    ################################

    xml.dlg <- file.path(.cdtDir$dirLocal, "languages", "cdtInfoMinFraction_dlgBox.xml")
    lang.dlg <- cdtLanguageParse(xml.dlg, .cdtData$Config$lang.iso)

    ################################

    frMinFr <- tkframe(frMRG0, relief = "groove", borderwidth = 2)

    uniqueMinFr <- tclVar(min.frac$unique)
    allMinFr <- tclVar(min.frac$all)

    stateMinFr0 <- if(min.frac$unique) "normal" else "disabled"

    chk.OneMinFr <- tkcheckbutton(frMinFr, variable = uniqueMinFr, text = lang.dlg[['label']][['1']], anchor = 'w', justify = 'left')
    txt.OneMinFr <- tklabel(frMinFr, text = "", justify = "right", anchor = 'e', width = largeur0)
    txt.allMinFr <- tklabel(frMinFr, text = lang.dlg[['label']][['2']], anchor = 'e', justify = 'right')
    en.allMinFr <- tkentry(frMinFr, textvariable = allMinFr, width = 6, state = stateMinFr0)

    tkgrid(chk.OneMinFr, row = 0, column = 0, sticky = 'we', rowspan = 1, columnspan = 10, padx = 1, pady = 0, ipadx = 1, ipady = 1)
    tkgrid(txt.OneMinFr, row = 1, column = 0, sticky = 'we', rowspan = 1, columnspan = 4, padx = 0, pady = 0, ipadx = 1, ipady = 1)
    tkgrid(txt.allMinFr, row = 1, column = 4, sticky = 'e', rowspan = 1, columnspan = 5, padx = 0, pady = 0, ipadx = 1, ipady = 1)
    tkgrid(en.allMinFr, row = 1, column = 9, sticky = 'we', rowspan = 1, columnspan = 1, padx = 0, pady = 0, ipadx = 1, ipady = 1)

    tkbind(chk.OneMinFr, "<Button-1>", function(){
        stateMinFr0 <- if(tclvalue(uniqueMinFr) == '0') "normal" else "disabled"
        tkconfigure(en.allMinFr, state = stateMinFr0)

        stateMinFr1 <- if(tclvalue(uniqueMinFr) == '1') "normal" else "disabled"
        for(j in 1:12) tkconfigure(en.monMinFr[[j]], state = stateMinFr1)
    })

    ################################

    frMinFrMon <- ttklabelframe(frMRG0, text = lang.dlg[['label']][['3']], labelanchor = "nw", relief = "groove", borderwidth = 2)

    monthMinFr <- lapply(min.frac$month, tclVar)

    stateMinFr1 <- if(min.frac$unique) "disabled" else "normal"

    txt.monMinFr <- vector(12, mode = 'list')
    en.monMinFr <- vector(12, mode = 'list')
    MOIS <- format(ISOdate(2014, 1:12, 1), "%B")

    txtCol <- c(0, 2, 4)
    enCol <- c(1, 3, 5)

    for(j in 1:12){
        txt.monMinFr[[j]] <- tklabel(frMinFrMon, text = MOIS[j], anchor = 'e', justify = 'right')
        en.monMinFr[[j]] <- tkentry(frMinFrMon, textvariable = monthMinFr[[j]], width = 6, state = stateMinFr1)

        j1 <- j %% 3
        j1[j1 == 0] <- 3

        tkgrid(txt.monMinFr[[j]], row = ceiling(j/3) - 1, column = txtCol[j1], sticky = 'e', padx = 1, pady = 1, ipadx = 1, ipady = 1)
        tkgrid(en.monMinFr[[j]], row = ceiling(j/3) - 1, column = enCol[j1], sticky = 'we', padx = 1, pady = 1, ipadx = 1, ipady = 1)
    }

    ################################

    tkgrid(frMinFr, row = 0, column = 0, sticky = 'we', padx = 3, pady = 3, ipadx = 1, ipady = 1)
    tkgrid(frMinFrMon, row = 1, column = 0, sticky = 'we', padx = 3, pady = 3, ipadx = 1, ipady = 1)

    ################################

    bt.prm.OK <- ttkbutton(frMRG1, text = .cdtEnv$tcl$lang$global[['button']][['1']])
    bt.prm.CA <- ttkbutton(frMRG1, text = .cdtEnv$tcl$lang$global[['button']][['2']])

    tkconfigure(bt.prm.OK, command = function(){
        min.frac$unique <<- switch(tclvalue(uniqueMinFr), '0' = FALSE, '1' = TRUE)
        if(min.frac$unique)
            min.frac$all <<- as.numeric(str_trim(tclvalue(allMinFr)))
        else
            min.frac$month <<- as.numeric(str_trim(sapply(monthMinFr, tclvalue)))

        tkgrab.release(tt1)
        tkdestroy(tt1)
        tkfocus(parent.win)
    })

    tkconfigure(bt.prm.CA, command = function(){
        tkgrab.release(tt1)
        tkdestroy(tt1)
        tkfocus(parent.win)
    })

    tkgrid(bt.prm.OK, row = 0, column = 0, sticky = 'w', padx = 5, pady = 1, ipadx = 1, ipady = 1)
    tkgrid(bt.prm.CA, row = 0, column = 1, sticky = 'e', padx = 5, pady = 1, ipadx = 1, ipady = 1)

    ################################
    tkgrid(frMRG0, row = 0, column = 0, sticky = 'nswe', rowspan = 1, columnspan = 2, padx = 1, pady = 1, ipadx = 1, ipady = 1)
    tkgrid(frMRG1, row = 1, column = 1, sticky = 'se', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)

    tkwm.withdraw(tt1)
    tcl('update')
    tt.w <- as.integer(tkwinfo("reqwidth", tt1))
    tt.h <- as.integer(tkwinfo("reqheight", tt1))
    tt.x <- as.integer(.cdtEnv$tcl$data$width.scr*0.5 - tt.w*0.5)
    tt.y <- as.integer(.cdtEnv$tcl$data$height.scr*0.5 - tt.h*0.5)
    tkwm.geometry(tt1, paste0('+', tt.x, '+', tt.y))
    tkwm.transient(tt1)
    tkwm.title(tt1, lang.dlg[['title']])
    tkwm.deiconify(tt1)
    tcl('wm', 'attributes', tt1, topmost = TRUE)

    tkfocus(tt1)
    tkbind(tt1, "<Destroy>", function(){
        tkgrab.release(tt1)
        tkfocus(parent.win)
    })
    tkwait.window(tt1)
    return(min.frac)
}

