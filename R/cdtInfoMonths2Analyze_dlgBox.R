
getInfoMonths2Process <- function(parent.win, Parameters)
{
    largeur0 <- if(WindowsOS()) 35 else 39

    xml.dlg <- file.path(.cdtDir$dirLocal, "languages", "cdtInfoMonths2Analyze_dlgBox.xml")
    lang.dlg <- cdtLanguageParse(xml.dlg, .cdtData$Config$lang.iso)

    ###################

    tt <- tktoplevel()
    tkgrab.set(tt)
    tkfocus(tt)

    frMRG0 <- tkframe(tt, relief = 'raised', borderwidth = 2)
    frMRG1 <- tkframe(tt)

    ###################

    frBaseP <- tkframe(frMRG0, relief = "sunken", borderwidth = 2)

    MOIS <- format(ISOdate(2014, 1:12, 1), "%B")

    startMon <- tclVar(MOIS[Parameters$start.month])
    endMon <- tclVar(MOIS[Parameters$end.month])
    nseqMon <- tclVar(Parameters$nseq.months)

    state0 <- if(Parameters$nseq.months) 'disabled' else 'normal'
    state1 <- if(Parameters$nseq.months) 'normal' else 'disabled'

    txt.Month <- tklabel(frBaseP, text = lang.dlg[['label']][['1']], anchor = 'w', justify = 'left', width = largeur0)
    txt.startMon <- tklabel(frBaseP, text = lang.dlg[['label']][['2']], anchor = 'e', justify = 'right')
    cb.startMon <- ttkcombobox(frBaseP, values = MOIS, textvariable = startMon, width = 11, state = state0)
    txt.endMon <- tklabel(frBaseP, text = lang.dlg[['label']][['3']], anchor = 'e', justify = 'right')
    cb.endMon <- ttkcombobox(frBaseP, values = MOIS, textvariable = endMon, width = 11, state = state0)
    chk.customMon <- tkcheckbutton(frBaseP, variable = nseqMon, text = lang.dlg[['checkbutton']][['1']], anchor = 'w', justify = 'left')
    bt.customMon <- ttkbutton(frBaseP, text = lang.dlg[['button']][['1']], state = state1)

    tkgrid(txt.Month, row = 0, column = 0, sticky = 'we', rowspan = 1, columnspan = 8, padx = 1, pady = 1, ipadx = 1, ipady = 1)
    tkgrid(txt.startMon, row = 1, column = 0, sticky = 'e', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
    tkgrid(cb.startMon, row = 1, column = 1, sticky = 'w', rowspan = 1, columnspan = 3, padx = 1, pady = 1, ipadx = 1, ipady = 1)
    tkgrid(txt.endMon, row = 1, column = 4, sticky = 'e', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
    tkgrid(cb.endMon, row = 1, column = 5, sticky = 'w', rowspan = 1, columnspan = 3, padx = 1, pady = 1, ipadx = 1, ipady = 1)
    tkgrid(chk.customMon, row = 2, column = 0, sticky = 'we', rowspan = 1, columnspan = 6, padx = 1, pady = 1, ipadx = 1, ipady = 1)
    tkgrid(bt.customMon, row = 2, column = 6, sticky = 'e', rowspan = 1, columnspan = 2, padx = 1, pady = 1, ipadx = 1, ipady = 1)

    ###################

    helpWidget(cb.startMon, lang.dlg[['tooltip']][['1']], lang.dlg[['status']][['1']])
    helpWidget(cb.endMon, lang.dlg[['tooltip']][['2']], lang.dlg[['status']][['2']])
    helpWidget(chk.customMon, lang.dlg[['tooltip']][['3']], lang.dlg[['status']][['3']])
    helpWidget(bt.customMon, lang.dlg[['tooltip']][['4']], lang.dlg[['status']][['4']])

    ###################

    tkconfigure(bt.customMon, command = function(){
        tcl('wm', 'attributes', tt, topmost = FALSE)
        Parameters$custom.months <<- spatialAnalysisEditYrsMon(tt, Parameters$custom.months, FALSE)
        tcl('wm', 'attributes', tt, topmost = TRUE)
    })

    ############

    tkbind(chk.customMon, "<Button-1>", function(){
        state0 <- if(tclvalue(nseqMon) == '1') 'normal' else 'disabled'
        tkconfigure(cb.startMon, state = state0)
        tkconfigure(cb.endMon, state = state0)

        state1 <- if(tclvalue(nseqMon) == '1') 'disabled' else 'normal'
        tkconfigure(bt.customMon, state = state1)
    })

    ###################

    tkgrid(frBaseP, row = 0, column = 0, sticky = 'ewns', padx = 3, pady = 1, ipady = 5)

    ################################

    bt.prm.OK <- ttkbutton(frMRG1, text = .cdtEnv$tcl$lang$global[['button']][['1']])
    bt.prm.CA <- ttkbutton(frMRG1, text = .cdtEnv$tcl$lang$global[['button']][['2']])

    tkconfigure(bt.prm.OK, command = function(){
        Parameters$start.month <<- which(MOIS %in% trimws(tclvalue(startMon)))
        Parameters$end.month <<- which(MOIS %in% trimws(tclvalue(endMon)))
        Parameters$nseq.months <<- switch(tclvalue(nseqMon), '0' = FALSE, '1' = TRUE)

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
