
getParams.TT.OutlierCheck <- function(Parameters)
{
    if(WindowsOS()){
        largeur0 <- 2
        largeur1 <- 11
        largeur2 <- 13
    }else{
        largeur0 <- 3
        largeur1 <- 12
        largeur2 <- 14
    }

    ###################

    xml.dlg <- file.path(.cdtDir$dirLocal, "languages", "cdtQC_TT.OutlierCheck_dlgBox.xml")
    lang.dlg <- cdtLanguageParse(xml.dlg, .cdtData$Config$lang.iso)

    ###################

    tt <- tktoplevel()
    tkgrab.set(tt)
    tkfocus(tt)

    frMRG0 <- tkframe(tt, relief = 'raised', borderwidth = 2)
    frMRG1 <- tkframe(tt)

    ###################

    framePars <- tkframe(frMRG0, relief = 'groove', borderwidth = 2)

    temp.max <- tclVar(Parameters$temp.max)
    temp.min <- tclVar(Parameters$temp.min)
    mul.sigma <- tclVar(Parameters$sigma)
    window <- tclVar(Parameters$window)

    txt.tmin <- tklabel(framePars, text = lang.dlg[['label']][['1-1']], anchor = 'e', justify = 'right')
    en.tmin <- tkentry(framePars, width = 4, textvariable = temp.min, justify = 'left')
    txt.tmax <- tklabel(framePars, text = lang.dlg[['label']][['1-2']], anchor = 'e', justify = 'right')
    en.tmax <- tkentry(framePars, width = 4, textvariable = temp.max, justify = 'left')
    txt.win <- tklabel(framePars, text = lang.dlg[['label']][['2-1']], anchor = 'e', justify = 'right')
    en.win <- tkentry(framePars, width = 4, textvariable = window, justify = 'left')
    txt.confL <- tklabel(framePars, text = lang.dlg[['label']][['2-2']], anchor = 'e', justify = 'right')
    en.confL <- tkentry(framePars, width = 4, textvariable = mul.sigma, justify = 'left')
    txt.frpr <- tklabel(framePars, text = "", width = largeur0)

    tkgrid(txt.tmin, row = 0, column = 0, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
    tkgrid(en.tmin, row = 0, column = 1, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
    tkgrid(txt.tmax, row = 1, column = 0, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
    tkgrid(en.tmax, row = 1, column = 1, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
    tkgrid(txt.frpr, row = 0, column = 2, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
    tkgrid(txt.win, row = 0, column = 3, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
    tkgrid(en.win, row = 0, column = 4, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
    tkgrid(txt.confL, row = 1, column = 3, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
    tkgrid(en.confL, row = 1, column = 4, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)

    helpWidget(en.tmin, lang.dlg[['tooltip']][['1-1']], lang.dlg[['status']][['1-1']])
    helpWidget(en.tmax, lang.dlg[['tooltip']][['1-2']], lang.dlg[['status']][['1-2']])
    helpWidget(en.win, lang.dlg[['tooltip']][['2-1']], lang.dlg[['status']][['2-1']])
    helpWidget(en.confL, lang.dlg[['tooltip']][['2-2']], lang.dlg[['status']][['2-2']])

    ###################

    frameELV <- ttklabelframe(frMRG0, text = lang.dlg[['label']][['3']], relief = 'groove')

    use.elev <- tclVar(Parameters$elv$use)

    chk.elev <- tkcheckbutton(frameELV, variable = use.elev, text = lang.dlg[['checkbutton']][['1']], anchor = 'w', justify = 'left')
    txt.elev <- tklabel(frameELV, text = "", width = largeur1)
    bt.elev <- ttkbutton(frameELV, text = .cdtEnv$tcl$lang$global[['button']][['5']], state = "disabled", width = largeur2)

    tkgrid(chk.elev, row = 0, column = 0, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
    tkgrid(txt.elev, row = 0, column = 1, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
    tkgrid(bt.elev, row = 0, column = 2, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)

    helpWidget(chk.elev, lang.dlg[['tooltip']][['3']], lang.dlg[['status']][['3']])
    helpWidget(bt.elev, lang.dlg[['tooltip']][['4']], lang.dlg[['status']][['4']])

    ######
    tkconfigure(bt.elev, command = function(){
        tcl('wm', 'attributes', tt, topmost = FALSE)
        Parameters$elv <<- getParams.QC.Elevation(Parameters$elv, tt)
        tcl('wm', 'attributes', tt, topmost = TRUE)
    })

    tkbind(chk.elev, "<Button-1>", function(){
        stateElev <- if(tclvalue(use.elev) == '1') 'disabled' else 'normal'
        tkconfigure(bt.elev, state = stateElev)
    })

    ###################

    frameVois <- ttklabelframe(frMRG0, text = lang.dlg[['label']][['4']], relief = 'groove')

    min.nbrs <- tclVar(Parameters$voisin$min)
    max.nbrs <- tclVar(Parameters$voisin$max)
    max.dst <- tclVar(Parameters$voisin$dist)
    elv.diff <- tclVar(Parameters$voisin$elv)

    txt.min.nb <- tklabel(frameVois, text = lang.dlg[['label']][['5']], anchor = 'e', justify = 'right')
    en.min.nb <- tkentry(frameVois, width = 3, textvariable = min.nbrs, justify = 'left')
    txt.max.nb <- tklabel(frameVois, text = lang.dlg[['label']][['6']], anchor = 'e', justify = 'right')
    en.max.nb <- tkentry(frameVois, width = 3, textvariable = max.nbrs, justify = 'left')
    txt.max.dt <- tklabel(frameVois, text = lang.dlg[['label']][['7']], anchor = 'e', justify = 'right')
    en.max.dt <- tkentry(frameVois, width = 3, textvariable = max.dst, justify = 'left')
    txt.elv.dif <- tklabel(frameVois, text = lang.dlg[['label']][['8']], anchor = 'e', justify = 'right')
    en.elv.dif <- tkentry(frameVois, width = 4, textvariable = elv.diff, justify = 'left')

    tkgrid(txt.min.nb, row = 0, column = 0, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
    tkgrid(en.min.nb, row = 0, column = 1, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
    tkgrid(txt.max.nb, row = 0, column = 2, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
    tkgrid(en.max.nb, row = 0, column = 3, sticky = 'w', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
    tkgrid(txt.max.dt, row = 1, column = 0, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
    tkgrid(en.max.dt, row = 1, column = 1, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
    tkgrid(txt.elv.dif, row = 1, column = 2, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
    tkgrid(en.elv.dif, row = 1, column = 3, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)

    helpWidget(en.min.nb, lang.dlg[['tooltip']][['5']], lang.dlg[['status']][['5']])
    helpWidget(en.max.nb, lang.dlg[['tooltip']][['6']], lang.dlg[['status']][['6']])
    helpWidget(en.max.dt, lang.dlg[['tooltip']][['7']], lang.dlg[['status']][['7']])
    helpWidget(en.elv.dif, lang.dlg[['tooltip']][['8']], lang.dlg[['status']][['8']])

    ###################

    tkgrid(framePars, row = 0, column = 0, sticky = 'we', padx = 1, ipadx = 1, pady = 3, ipady = 1)
    tkgrid(frameELV, row = 1, column = 0, sticky = 'we', padx = 1, ipadx = 1, pady = 3, ipady = 1)
    tkgrid(frameVois, row = 2, column = 0, sticky = 'we', padx = 1, ipadx = 1, pady = 1, ipady = 1)

    ################################

    bt.prm.OK <- ttkbutton(frMRG1, text = .cdtEnv$tcl$lang$global[['button']][['1']])
    bt.prm.CA <- ttkbutton(frMRG1, text = .cdtEnv$tcl$lang$global[['button']][['2']])

    tkconfigure(bt.prm.OK, command = function(){
        Parameters$temp.max <<- as.numeric(str_trim(tclvalue(temp.max)))
        Parameters$temp.min <<- as.numeric(str_trim(tclvalue(temp.min)))
        Parameters$window <<- as.numeric(str_trim(tclvalue(window)))
        Parameters$sigma <<- as.numeric(str_trim(tclvalue(mul.sigma)))

        Parameters$elv$use <<- switch(tclvalue(use.elev), '0' = FALSE, '1' = TRUE)

        Parameters$voisin$min <<- as.numeric(str_trim(tclvalue(min.nbrs)))
        Parameters$voisin$max <<- as.numeric(str_trim(tclvalue(max.nbrs)))
        Parameters$voisin$dist <<- as.numeric(str_trim(tclvalue(max.dst)))
        Parameters$voisin$elv <<- as.numeric(str_trim(tclvalue(elv.diff)))

        tkgrab.release(tt)
        tkdestroy(tt)
        tkfocus(.cdtEnv$tcl$main$win)
    })

    tkconfigure(bt.prm.CA, command = function(){
        tkgrab.release(tt)
        tkdestroy(tt)
        tkfocus(.cdtEnv$tcl$main$win)
    })

    tkgrid(bt.prm.OK, row = 0, column = 0, sticky = 'w', padx = 5, pady = 1, ipadx = 1, ipady = 1)
    tkgrid(bt.prm.CA, row = 0, column = 1, sticky = 'e', padx = 5, pady = 1, ipadx = 1, ipady = 1)

    ################################
    tkgrid(frMRG0, row = 0, column = 0, sticky = 'nswe', rowspan = 1, columnspan = 2, padx = 1, pady = 1, ipadx = 1, ipady = 1)
    tkgrid(frMRG1, row = 1, column = 1, sticky = 'se', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)

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
    return(Parameters)
}
