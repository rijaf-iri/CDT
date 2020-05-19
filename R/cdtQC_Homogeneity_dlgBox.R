
getParams.HomogMethod <- function(Parameters, CONF.LEV,
                                  parent.win = .cdtEnv$tcl$main$win)
{
    xml.dlg <- file.path(.cdtDir$dirLocal, "languages", "cdtQC_Homogeneity_dlgBox1.xml")
    lang.dlg <- cdtLanguageParse(xml.dlg, .cdtData$Config$lang.iso)

    ###################

    tt <- tktoplevel()
    tkgrab.set(tt)
    tkfocus(tt)

    frMRG0 <- tkframe(tt, relief = 'raised', borderwidth = 2)
    frMRG1 <- tkframe(tt)

    ###################

    framePars <- tkframe(frMRG0, relief = 'sunken', borderwidth = 2)

    crop.perc.list <- c('0.010', '0.025', '0.050', '0.100')
    crop.bound <- tclVar(Parameters$crop)
    crop.perc <- tclVar(sprintf("%.3f", Parameters$h))
    CL <- sprintf("%.1f", Parameters$conf.lev)
    if(!CL %in% CONF.LEV){
        CL <- findInterval(as.numeric(Parameters$conf.lev), as.numeric(CONF.LEV))
        CL <- CONF.LEV[CL]
    }
    conf.lev <- tclVar(CL)
    Kmax <- tclVar(Parameters$kmax)
    minSeg <- tclVar(Parameters$min.len)
    minYear <- tclVar(Parameters$min.year)
    minFrac <- tclVar(Parameters$min.frac)

    fr.crop <- tkframe(framePars)
    txt.conf <- tklabel(framePars, text = lang.dlg[['label']][['1']], anchor = 'e', justify = 'right')
    cb.conf <- ttkcombobox(framePars, values = CONF.LEV, textvariable = conf.lev, width = 6)
    txt.brks <- tklabel(framePars, text = lang.dlg[['label']][['2']], anchor = 'e', justify = 'right')
    en.brks <- tkentry(framePars, textvariable = Kmax, width = 6)
    txt.minS <- tklabel(framePars, text = lang.dlg[['label']][['3']], anchor = 'e', justify = 'right')
    en.minS <- tkentry(framePars, textvariable = minSeg, width = 6)
    txt.year <- tklabel(framePars, text = lang.dlg[['label']][['4']], anchor = 'e', justify = 'right')
    en.year <- tkentry(framePars, textvariable = minYear, width = 6)
    txt.frac <- tklabel(framePars, text = lang.dlg[['label']][['5']], anchor = 'e', justify = 'right')
    en.frac <- tkentry(framePars, textvariable = minFrac, width = 6)

    ###################
    chk.crop <- tkcheckbutton(fr.crop, variable = crop.bound, text = lang.dlg[['checkbutton']][['1']], anchor = 'w', justify = 'left')
    txt.crop <- tklabel(fr.crop, text = 'h:', anchor = 'e', justify = 'right')
    cb.crop <- ttkcombobox(fr.crop, values = crop.perc.list, textvariable = crop.perc, width = 6)

    tkgrid(chk.crop, row = 0, column = 0, sticky = 'we', rowspan = 1, columnspan = 1, padx = 3, pady = 1, ipadx = 1, ipady = 1)
    tkgrid(txt.crop, row = 0, column = 1, sticky = 'e', rowspan = 1, columnspan = 1, padx = 0, pady = 1, ipadx = 1, ipady = 1)
    tkgrid(cb.crop, row = 0, column = 3, sticky = 'we', rowspan = 1, columnspan = 1, padx = 0, pady = 1, ipadx = 1, ipady = 1)

    ###################

    tkgrid(fr.crop, row = 0, column = 0, sticky = 'we', rowspan = 1, columnspan = 2, padx = 1, pady = 1, ipadx = 1, ipady = 1)
    tkgrid(txt.conf, row = 0, column = 2, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
    tkgrid(cb.conf, row = 0, column = 3, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
    tkgrid(txt.brks, row = 1, column = 0, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
    tkgrid(en.brks, row = 1, column = 1, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
    tkgrid(txt.minS, row = 1, column = 2, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
    tkgrid(en.minS, row = 1, column = 3, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
    tkgrid(txt.year, row = 2, column = 0, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
    tkgrid(en.year, row = 2, column = 1, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
    tkgrid(txt.frac, row = 2, column = 2, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
    tkgrid(en.frac, row = 2, column = 3, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)

    helpWidget(fr.crop, lang.dlg[['tooltip']][['1']], lang.dlg[['status']][['1']])
    helpWidget(en.brks, lang.dlg[['tooltip']][['2']], lang.dlg[['status']][['2']])
    helpWidget(en.minS, lang.dlg[['tooltip']][['3']], lang.dlg[['status']][['3']])
    helpWidget(cb.conf, lang.dlg[['tooltip']][['4']], lang.dlg[['status']][['4']])
    helpWidget(en.year, lang.dlg[['tooltip']][['5']], lang.dlg[['status']][['5']])
    helpWidget(en.frac, lang.dlg[['tooltip']][['6']], lang.dlg[['status']][['6']])

    ###################

    tkgrid(framePars, row = 0, column = 0, sticky = 'we', padx = 1, ipadx = 1, pady = 3, ipady = 5)

    ################################

    bt.prm.OK <- ttkbutton(frMRG1, text = .cdtEnv$tcl$lang$global[['button']][['1']])
    bt.prm.CA <- ttkbutton(frMRG1, text = .cdtEnv$tcl$lang$global[['button']][['2']])

    tkconfigure(bt.prm.OK, command = function(){
        Parameters$crop <<- switch(tclvalue(crop.bound), '0' = FALSE, '1' = TRUE)
        Parameters$h <<- as.numeric(str_trim(tclvalue(crop.perc)))
        Parameters$kmax <<- as.numeric(str_trim(tclvalue(Kmax)))
        Parameters$min.len <<- as.numeric(str_trim(tclvalue(minSeg)))
        Parameters$conf.lev <<- as.numeric(str_trim(tclvalue(conf.lev)))
        Parameters$min.year <<- as.numeric(str_trim(tclvalue(minYear)))
        Parameters$min.frac <<- as.numeric(str_trim(tclvalue(minFrac)))

        tkgrab.release(tt)
        tkdestroy(tt)
        tkfocus(parent.win)
    })

    tkconfigure(bt.prm.CA, command = function(){
        tkgrab.release(tt)
        tkdestroy(tt)
        tkfocus(parent.win)
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
        tkfocus(parent.win)
    })
    tkwait.window(tt)
    return(Parameters)
}

############################################################################################

getParams.HomogAdjust <- function(Parameters, label = "Day",
                                  state.dyp = 'normal',
                                  state.dek = 'normal',
                                  parent.win = .cdtEnv$tcl$main$win)
{
    xml.dlg <- file.path(.cdtDir$dirLocal, "languages", "cdtQC_Homogeneity_dlgBox2.xml")
    lang.dlg <- cdtLanguageParse(xml.dlg, .cdtData$Config$lang.iso)

    ###################

    tt <- tktoplevel()
    tkgrab.set(tt)
    tkfocus(tt)

    frMRG0 <- tkframe(tt, relief = 'raised', borderwidth = 2)
    frMRG1 <- tkframe(tt)

    ###################

    framePars <- tkframe(frMRG0, relief = 'sunken', borderwidth = 2)

    min.mon <- tclVar(Parameters$min.mon)
    min.dek <- tclVar(Parameters$min.dek)
    min.dyp <- tclVar(Parameters$min.dyp)

    seg.mon <- tclVar(Parameters$seg.mon)
    seg.dek <- tclVar(Parameters$seg.dek)
    seg.dyp <- tclVar(Parameters$seg.dyp)

    ilab <- if(label == 'Pentad') '5' else '6'

    txt.min <- tklabel(framePars, text = lang.dlg[['label']][['1']], anchor = 'e', justify = 'right')
    txt.seg <- tklabel(framePars, text = lang.dlg[['label']][['2']], anchor = 'e', justify = 'right')
    txt.mon <- tklabel(framePars, text = lang.dlg[['label']][['3']])
    txt.dek <- tklabel(framePars, text = lang.dlg[['label']][['4']])
    txt.dyp <- tklabel(framePars, text = lang.dlg[['label']][[ilab]])

    en.min.mon <- tkentry(framePars, width = 4, textvariable = min.mon, justify = "center")
    en.min.dek <- tkentry(framePars, width = 4, textvariable = min.dek, justify = "center", state = state.dek)
    en.min.dyp <- tkentry(framePars, width = 4, textvariable = min.dyp, justify = "center", state = state.dyp)

    en.seg.mon <- tkentry(framePars, width = 4, textvariable = seg.mon, justify = "center")
    en.seg.dek <- tkentry(framePars, width = 4, textvariable = seg.dek, justify = "center", state = state.dek)
    en.seg.dyp <- tkentry(framePars, width = 4, textvariable = seg.dyp, justify = "center", state = state.dyp)

    tkgrid(txt.min, row = 1, column = 0, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
    tkgrid(txt.seg, row = 2, column = 0, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
    tkgrid(txt.mon, row = 0, column = 1, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
    tkgrid(txt.dek, row = 0, column = 2, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
    tkgrid(txt.dyp, row = 0, column = 3, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)

    tkgrid(en.min.mon, row = 1, column = 1, sticky = '', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
    tkgrid(en.min.dek, row = 1, column = 2, sticky = '', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
    tkgrid(en.min.dyp, row = 1, column = 3, sticky = '', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)

    tkgrid(en.seg.mon, row = 2, column = 1, sticky = '', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
    tkgrid(en.seg.dek, row = 2, column = 2, sticky = '', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
    tkgrid(en.seg.dyp, row = 2, column = 3, sticky = '', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)

    sapply(list(en.min.mon, en.min.dek, en.min.dyp), helpWidget,
                text_balloon = lang.dlg[['tooltip']][['1']],
                text_statusbar = lang.dlg[['status']][['2']])

    sapply(list(en.seg.mon, en.seg.dek, en.seg.dyp), helpWidget,
                text_balloon = lang.dlg[['tooltip']][['2']],
                text_statusbar = lang.dlg[['status']][['2']])

    ###################

    tkgrid(framePars, row = 0, column = 0, sticky = 'we', padx = 1, ipadx = 1, pady = 3, ipady = 5)

    ################################

    bt.prm.OK <- ttkbutton(frMRG1, text = .cdtEnv$tcl$lang$global[['button']][['1']])
    bt.prm.CA <- ttkbutton(frMRG1, text = .cdtEnv$tcl$lang$global[['button']][['2']])

    tkconfigure(bt.prm.OK, command = function(){
        Parameters$min.mon <<- as.numeric(str_trim(tclvalue(min.mon)))
        Parameters$min.dek <<- as.numeric(str_trim(tclvalue(min.dek)))
        Parameters$min.dyp <<- as.numeric(str_trim(tclvalue(min.dyp)))

        Parameters$seg.mon <<- as.numeric(str_trim(tclvalue(seg.mon)))
        Parameters$seg.dek <<- as.numeric(str_trim(tclvalue(seg.dek)))
        Parameters$seg.dyp <<- as.numeric(str_trim(tclvalue(seg.dyp)))

        tkgrab.release(tt)
        tkdestroy(tt)
        tkfocus(parent.win)
    })

    tkconfigure(bt.prm.CA, command = function(){
        tkgrab.release(tt)
        tkdestroy(tt)
        tkfocus(parent.win)
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
        tkfocus(parent.win)
    })
    tkwait.window(tt)
    return(Parameters)
}

############################################################################################

getParams.HomoRefSeries <- function(Parameters, parent.win = .cdtEnv$tcl$main$win)
{
    listOpenFiles <- openFile_ttkcomboList()
    if(WindowsOS()){
        largeur1 <- 59
        largeur2 <- 6
        largeur3 <- 7
        largeur4 <- 41
    }else{
        largeur1 <- 42
        largeur2 <- 2
        largeur3 <- 5
        largeur4 <- 35
    }

    xml.dlg <- file.path(.cdtDir$dirLocal, "languages", "cdtQC_Homogeneity_dlgBox3.xml")
    lang.dlg <- cdtLanguageParse(xml.dlg, .cdtData$Config$lang.iso)

    ###################

    tt <- tktoplevel()
    tkgrab.set(tt)
    tkfocus(tt)

    frMRG0 <- tkframe(tt, relief = 'raised', borderwidth = 2)
    frMRG1 <- tkframe(tt)

    ###################

    stateQRefS <- if(Parameters$user$refs) 'disabled' else 'normal'

    ###################

    frTestS <- ttklabelframe(frMRG0, text = lang.dlg[['label']][['1']], relief = "groove")

    diff.ratio <- tclVar(Parameters$diff.ratio)

    dif.rat1 <- tkradiobutton(frTestS, variable = diff.ratio, value = "1", text = lang.dlg[['radiobutton']][['1']][1], anchor = 'w', justify = 'left', state = stateQRefS)
    dif.rat2 <- tkradiobutton(frTestS, variable = diff.ratio, value = "2", text = lang.dlg[['radiobutton']][['1']][2], anchor = 'w', justify = 'left', state = stateQRefS)
    dif.rat3 <- tkradiobutton(frTestS, variable = diff.ratio, value = "3", text = lang.dlg[['radiobutton']][['1']][3], anchor = 'w', justify = 'left', state = stateQRefS)

    tkgrid(dif.rat1, row = 0, column = 0, sticky = 'ew', padx = 1, pady = 1, ipadx = 1, ipady = 1)
    tkgrid(dif.rat2, row = 1, column = 0, sticky = 'ew', padx = 1, pady = 1, ipadx = 1, ipady = 1)
    tkgrid(dif.rat3, row = 2, column = 0, sticky = 'ew', padx = 1, pady = 1, ipadx = 1, ipady = 1)

    helpWidget(dif.rat1, lang.dlg[['tooltip']][['1']], lang.dlg[['status']][['1']])
    helpWidget(dif.rat2, lang.dlg[['tooltip']][['2']], lang.dlg[['status']][['2']])
    helpWidget(dif.rat3, lang.dlg[['tooltip']][['3']], lang.dlg[['status']][['3']])

    ###################

    frWeig <- ttklabelframe(frMRG0, text = lang.dlg[['label']][['2']], relief = "groove")

    weight.fac <- tclVar(Parameters$weight)

    wmean1 <- tkradiobutton(frWeig, variable = weight.fac, value = "1", text = lang.dlg[['radiobutton']][['2']][1], anchor = 'w', justify = 'left', state = stateQRefS)
    wmean2 <- tkradiobutton(frWeig, variable = weight.fac, value = "2", text = lang.dlg[['radiobutton']][['2']][2], anchor = 'w', justify = 'left', state = stateQRefS)
    wmean3 <- tkradiobutton(frWeig, variable = weight.fac, value = "3", text = lang.dlg[['radiobutton']][['2']][3], anchor = 'w', justify = 'left', state = stateQRefS)

    tkgrid(wmean1, row = 0, column = 0, sticky = 'ew', padx = 1, pady = 1, ipadx = 1, ipady = 1)
    tkgrid(wmean2, row = 1, column = 0, sticky = 'ew', padx = 1, pady = 1, ipadx = 1, ipady = 1)
    tkgrid(wmean3, row = 2, column = 0, sticky = 'ew', padx = 1, pady = 1, ipadx = 1, ipady = 1)

    helpWidget(wmean1, lang.dlg[['tooltip']][['4']], lang.dlg[['status']][['4']])
    helpWidget(wmean2, lang.dlg[['tooltip']][['5']], lang.dlg[['status']][['5']])
    helpWidget(wmean3, lang.dlg[['tooltip']][['6']], lang.dlg[['status']][['6']])

    ###################

    frameClimato <- tkframe(frMRG0, relief = 'groove', borderwidth = 2)

    use.climato <- tclVar(Parameters$use.climato)

    chk.clim <- tkcheckbutton(frameClimato, variable = use.climato, text = lang.dlg[['checkbutton']][['3']], anchor = 'w', justify = 'left')
    tkgrid(chk.clim)

    helpWidget(chk.clim, lang.dlg[['tooltip']][['17']], lang.dlg[['status']][['17']])

    ###################

    frameVois <- ttklabelframe(frMRG0, text = lang.dlg[['label']][['3']], relief = 'groove')

    fr.separ <- tklabel(frameVois, text = "", width = largeur2)

    ########
    fr.nbrs <- tkframe(frameVois)

    min.nbrs <- tclVar(Parameters$voisin$min)
    max.nbrs <- tclVar(Parameters$voisin$max)

    txt.min.nb <- tklabel(fr.nbrs, text = lang.dlg[['label']][['4']], anchor = 'e', justify = 'right')
    en.min.nb <- tkentry(fr.nbrs, width = largeur3, textvariable = min.nbrs, justify = 'left', state = stateQRefS)
    txt.max.nb <- tklabel(fr.nbrs, text = lang.dlg[['label']][['5']], anchor = 'e', justify = 'right')
    en.max.nb <- tkentry(fr.nbrs, width = largeur3, textvariable = max.nbrs, justify = 'left', state = stateQRefS)

    tkgrid(txt.min.nb, row = 0, column = 0, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
    tkgrid(en.min.nb, row = 0, column = 1, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
    tkgrid(txt.max.nb, row = 1, column = 0, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
    tkgrid(en.max.nb, row = 1, column = 1, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)

    helpWidget(en.min.nb, lang.dlg[['tooltip']][['7']], lang.dlg[['status']][['7']])
    helpWidget(en.max.nb, lang.dlg[['tooltip']][['8']], lang.dlg[['status']][['8']])

    ########
    fr.pars <- tkframe(frameVois)

    max.dst <- tclVar(Parameters$voisin$dist)
    elv.diff <- tclVar(Parameters$voisin$elv)

    txt.max.dt <- tklabel(fr.pars, text = lang.dlg[['label']][['6']], anchor = 'e', justify = 'right')
    en.max.dt <- tkentry(fr.pars, width = largeur3, textvariable = max.dst, justify = 'left', state = stateQRefS)
    txt.elv.dif <- tklabel(fr.pars, text = lang.dlg[['label']][['7']], anchor = 'e', justify = 'right')
    en.elv.dif <- tkentry(fr.pars, width = largeur3, textvariable = elv.diff, justify = 'left', state = stateQRefS)

    tkgrid(txt.max.dt, row = 0, column = 0, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
    tkgrid(en.max.dt, row = 0, column = 1, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
    tkgrid(txt.elv.dif, row = 1, column = 0, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
    tkgrid(en.elv.dif, row = 1, column = 1, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)

    helpWidget(en.max.dt, lang.dlg[['tooltip']][['9']], lang.dlg[['status']][['9']])
    helpWidget(en.elv.dif, lang.dlg[['tooltip']][['10']], lang.dlg[['status']][['10']])

    ########
    fr.rho <- tkframe(frameVois)

    min.rho <- tclVar(Parameters$voisin$rho)

    txt.min.rho <- tklabel(fr.rho, text = lang.dlg[['label']][['8']], anchor = 'e', justify = 'right')
    en.min.rho <- tkentry(fr.rho, width = largeur3, textvariable = min.rho, justify = 'left', state = stateQRefS)

    tkgrid(txt.min.rho, en.min.rho)

    helpWidget(en.min.rho, lang.dlg[['tooltip']][['11']], lang.dlg[['status']][['11']])

    ########

    tkgrid(fr.nbrs, row = 0, column = 0, sticky = 'we', rowspan = 1, columnspan = 1)
    tkgrid(fr.separ, row = 0, column = 1, sticky = 'we', rowspan = 1, columnspan = 1)
    tkgrid(fr.pars, row = 0, column = 2, sticky = 'we', rowspan = 1, columnspan = 1)
    tkgrid(fr.rho, row = 1, column = 0, sticky = '', rowspan = 1, columnspan = 3)

    ###################

    frameELV <- tkframe(frMRG0, relief = 'groove', borderwidth = 2)

    use.elev <- tclVar(Parameters$elv$use)

    stateElev <- if(Parameters$elv$use) 'normal' else 'disabled'

    chk.elev <- tkcheckbutton(frameELV, variable = use.elev, text = lang.dlg[['checkbutton']][['1']], anchor = 'w', justify = 'left', state = stateQRefS, width = largeur4)
    bt.elev <- ttkbutton(frameELV, text = .cdtEnv$tcl$lang$global[['button']][['5']], state = stateElev)

    tkgrid(chk.elev, row = 0, column = 0, sticky = 'we', rowspan = 1, columnspan = 2, padx = 1, pady = 1, ipadx = 1, ipady = 1)
    tkgrid(bt.elev, row = 0, column = 2, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)

    helpWidget(chk.elev, lang.dlg[['tooltip']][['12']], lang.dlg[['status']][['12']])
    helpWidget(bt.elev, lang.dlg[['tooltip']][['13']], lang.dlg[['status']][['13']])

    ###############
    tkconfigure(bt.elev, command = function(){
        Params <- Parameters[["elv"]]
        tcl('wm', 'attributes', tt, topmost = FALSE)
        Parameters[["elv"]] <<- getParams.QC.Elevation(Params, tt)
        tcl('wm', 'attributes', tt, topmost = TRUE)
    })

    tkbind(chk.elev, "<Button-1>", function(){
        if(tclvalue(user.ref) == '0'){
            stateElev <- if(tclvalue(use.elev) == '1') 'disabled' else 'normal'
            tkconfigure(bt.elev, state = stateElev)
        }
    })

    ###################

    frameUSER <- tkframe(frMRG0, relief = 'groove', borderwidth = 2)

    user.ref <- tclVar(Parameters$user$refs)
    user.file <- tclVar(Parameters$user$file)

    stateUser <- if(Parameters$user$refs) 'normal' else 'disabled'

    chk.user <- tkcheckbutton(frameUSER, variable = user.ref, text = lang.dlg[['checkbutton']][['2']], anchor = 'w', justify = 'left')
    txt.user <- tklabel(frameUSER, text = lang.dlg[['label']][['9']], anchor = 'w', justify = 'left')
    cb.user <- ttkcombobox(frameUSER, values = unlist(listOpenFiles), textvariable = user.file, width = largeur1, state = stateUser)
    bt.user <- tkbutton(frameUSER, text = "...", state = stateUser)

    tkgrid(chk.user, row = 0, column = 0, sticky = 'we', rowspan = 1, columnspan = 5, padx = 1, pady = 1, ipadx = 1, ipady = 1)
    tkgrid(txt.user, row = 1, column = 0, sticky = 'we', rowspan = 1, columnspan = 5, padx = 1, pady = 1, ipadx = 1, ipady = 1)
    tkgrid(cb.user, row = 2, column = 0, sticky = 'we', rowspan = 1, columnspan = 4, padx = 0, pady = 1, ipadx = 1, ipady = 1)
    tkgrid(bt.user, row = 2, column = 4, sticky = 'w', rowspan = 1, columnspan = 1, padx = 0, pady = 1, ipadx = 1, ipady = 1)

    helpWidget(chk.user, lang.dlg[['tooltip']][['14']], lang.dlg[['status']][['14']])
    helpWidget(cb.user, lang.dlg[['tooltip']][['15']], lang.dlg[['status']][['15']])
    helpWidget(bt.user, lang.dlg[['tooltip']][['16']], lang.dlg[['status']][['16']])

    ###################
    tkconfigure(bt.user, command = function(){
        tcl('wm', 'attributes', tt, topmost = FALSE)
        dat.opfiles <- getOpenFiles(tt)
        tcl('wm', 'attributes', tt, topmost = TRUE)
        if(!is.null(dat.opfiles)){
            update.OpenFiles('ascii', dat.opfiles)
            listOpenFiles[[length(listOpenFiles) + 1]] <<- dat.opfiles[[1]]
            tclvalue(user.file) <- dat.opfiles[[1]]
            tkconfigure(cb.user, values = unlist(listOpenFiles))
        }
    })

    tkbind(chk.user, "<Button-1>", function(){
        stateUser <- if(tclvalue(user.ref) == '1') 'disabled' else 'normal'
        tkconfigure(cb.user, state = stateUser)
        tkconfigure(bt.user, state = stateUser)

        stateQRefS <- if(tclvalue(user.ref) == '1') 'normal' else 'disabled'
        tkconfigure(dif.rat1, state = stateQRefS)
        tkconfigure(dif.rat2, state = stateQRefS)
        tkconfigure(dif.rat3, state = stateQRefS)
        tkconfigure(wmean1, state = stateQRefS)
        tkconfigure(wmean2, state = stateQRefS)
        tkconfigure(wmean3, state = stateQRefS)
        tkconfigure(en.min.nb, state = stateQRefS)
        tkconfigure(en.max.nb, state = stateQRefS)
        tkconfigure(en.max.dt, state = stateQRefS)
        tkconfigure(en.elv.dif, state = stateQRefS)
        tkconfigure(en.min.rho, state = stateQRefS)

        tkconfigure(chk.elev, state = stateQRefS)
        if(tclvalue(user.ref) == '1'){
            stateElev <- if(tclvalue(use.elev) == '0') 'disabled' else 'normal'
        }else stateElev <- 'disabled'
        tkconfigure(bt.elev, state = stateElev)
    })

    ###################

    tkgrid(frTestS, row = 0, column = 0, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, ipadx = 1, pady = 1, ipady = 1)
    tkgrid(frWeig, row = 0, column = 1, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, ipadx = 1, pady = 1, ipady = 1)
    tkgrid(frameClimato, row = 1, column = 0, sticky = 'we', rowspan = 1, columnspan = 2, padx = 1, ipadx = 1, pady = 3, ipady = 1)
    tkgrid(frameVois, row = 2, column = 0, sticky = 'we', rowspan = 1, columnspan = 2, padx = 1, ipadx = 1, pady = 1, ipady = 1)
    tkgrid(frameELV, row = 3, column = 0, sticky = 'we', rowspan = 1, columnspan = 2, padx = 1, ipadx = 1, pady = 3, ipady = 1)
    tkgrid(frameUSER, row = 4, column = 0, sticky = 'we', rowspan = 1, columnspan = 2, padx = 1, ipadx = 1, pady = 1, ipady = 1)

    ################################

    bt.prm.OK <- ttkbutton(frMRG1, text = .cdtEnv$tcl$lang$global[['button']][['1']])
    bt.prm.CA <- ttkbutton(frMRG1, text = .cdtEnv$tcl$lang$global[['button']][['2']])

    tkconfigure(bt.prm.OK, command = function(){
        Parameters$diff.ratio <<- as.numeric(str_trim(tclvalue(diff.ratio)))
        Parameters$weight <<- as.numeric(str_trim(tclvalue(weight.fac)))
        Parameters$use.climato <<- switch(tclvalue(use.climato), '0' = FALSE, '1' = TRUE)

        Parameters$voisin$min <<- as.numeric(str_trim(tclvalue(min.nbrs)))
        Parameters$voisin$max <<- as.numeric(str_trim(tclvalue(max.nbrs)))
        Parameters$voisin$dist <<- as.numeric(str_trim(tclvalue(max.dst)))
        Parameters$voisin$elv <<- as.numeric(str_trim(tclvalue(elv.diff)))
        Parameters$voisin$rho <<- as.numeric(str_trim(tclvalue(min.rho)))

        Parameters$elv$use <<- switch(tclvalue(use.elev), '0' = FALSE, '1' = TRUE)
        Parameters$user$refs <<- switch(tclvalue(user.ref), '0' = FALSE, '1' = TRUE)
        Parameters$user$file <<- str_trim(tclvalue(user.file))

        tkgrab.release(tt)
        tkdestroy(tt)
        tkfocus(parent.win)
    })

    tkconfigure(bt.prm.CA, command = function(){
        tkgrab.release(tt)
        tkdestroy(tt)
        tkfocus(parent.win)
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
        tkfocus(parent.win)
    })
    tkwait.window(tt)
    return(Parameters)
}
