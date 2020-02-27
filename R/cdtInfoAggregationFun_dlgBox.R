getInfoAggregateFun <- function(parent.win, Parameters, timeStep)
{
    # xml.dlg <- file.path(.cdtDir$dirLocal, "languages", "cdtInfoBasePeriod_dlgBox.xml")
    # lang.dlg <- cdtLanguageParse(xml.dlg, .cdtData$Config$lang.iso)

    ###################

    tt <- tktoplevel()
    tkgrab.set(tt)
    tkfocus(tt)

    frMRG0 <- tkframe(tt, relief = 'raised', borderwidth = 2)
    frMRG1 <- tkframe(tt)

    ###################

    frameAggr <- tkframe(frMRG0, relief = "sunken", borderwidth = 2)

    aggr.fun <- tclVar(Parameters$aggr.fun)
    min.frac <- tclVar(Parameters$min.frac)
    opr.fun <- tclVar(Parameters$opr.fun)
    opr.thres <- tclVar(Parameters$opr.thres)

    AGGRFUN <- c("mean", "sum", "max", "min")
    if(timeStep == "daily") AGGRFUN <- c(AGGRFUN, "count")
    stateCount <- if(Parameters$aggr.fun == "count") 'normal' else 'disabled'

    txt.aggfun <- tklabel(frameAggr, text = "Function", anchor = 'e', justify = 'right')
    cb.aggfun <- ttkcombobox(frameAggr, values = AGGRFUN, textvariable = aggr.fun, width = 6)
    txt.minfrac <- tklabel(frameAggr, text = "Min.Frac", anchor = 'e', justify = 'right')
    en.minfrac <- tkentry(frameAggr, textvariable = min.frac, width = 6)
    txt.opfun <- tklabel(frameAggr, text = "Operator", anchor = 'e', justify = 'right')
    cb.opfun <- ttkcombobox(frameAggr, values = c(">=", ">", "<=", "<"), textvariable = opr.fun, width = 6, state = stateCount)
    txt.opthres <- tklabel(frameAggr, text = "Threshold", anchor = 'e', justify = 'right')
    en.opthres <- tkentry(frameAggr, textvariable = opr.thres, width = 6, width = 6, state = stateCount)

    blank1 <- tklabel(frameAggr, text = "", width = 5)
    blank2 <- tklabel(frameAggr, text = "", width = 5)

    tkgrid(txt.aggfun, row = 0, column = 0, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
    tkgrid(cb.aggfun, row = 0, column = 1, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
    tkgrid(blank1, row = 0, column = 2, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
    tkgrid(txt.minfrac, row = 0, column = 3, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
    tkgrid(en.minfrac, row = 0, column = 4, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)

    tkgrid(txt.opfun, row = 1, column = 0, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
    tkgrid(cb.opfun, row = 1, column = 1, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
    tkgrid(blank2, row = 1, column = 2, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
    tkgrid(txt.opthres, row = 1, column = 3, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
    tkgrid(en.opthres, row = 1, column = 4, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)

    # helpWidget(cb.aggfun, lang.dlg[['tooltip']][['12']], lang.dlg[['status']][['12']])
    # helpWidget(en.minfrac, lang.dlg[['tooltip']][['13']], lang.dlg[['status']][['13']])
    # helpWidget(cb.opfun, lang.dlg[['tooltip']][['14']], lang.dlg[['status']][['14']])
    # helpWidget(en.opthres, lang.dlg[['tooltip']][['15']], lang.dlg[['status']][['15']])

    helpWidget(cb.aggfun, "Function that have to be applied for aggregating from daily/dekadal/monthly into\n a higher time step (e.g., for precipitation FUN=sum and for temperature FUN=mean)",
                          "Function that have to be applied for aggregating from daily/dekadal/monthly into\n a higher time step (e.g., for precipitation FUN=sum and for temperature FUN=mean)")
    helpWidget(en.minfrac, "Minimum fraction of available data that must be present within each output time step",
                           "Minimum fraction of available data that must be present within each output time step")
    helpWidget(cb.opfun, "Select the comparison operator to be used to match event",
                         "Select the comparison operator to be used to match event")
    helpWidget(en.opthres, "User defined threshold applied to count event",
                           "User defined threshold applied to count event")

    ##############

    tkbind(cb.aggfun, "<<ComboboxSelected>>", function(){
        stateCount <- if(tclvalue(aggr.fun) == "count") "normal" else "disabled"
        tkconfigure(cb.opfun, state = stateCount)
        tkconfigure(en.opthres, state = stateCount)
    })

    ##############

    tkgrid(frameAggr, row = 0, column = 0, sticky = 'ewns', padx = 3, pady = 1, ipady = 5)

    ################################

    bt.prm.OK <- ttkbutton(frMRG1, text = .cdtEnv$tcl$lang$global[['button']][['1']])
    bt.prm.CA <- ttkbutton(frMRG1, text = .cdtEnv$tcl$lang$global[['button']][['2']])

    tkconfigure(bt.prm.OK, command = function(){
        Parameters$aggr.fun <<- str_trim(tclvalue(aggr.fun))
        Parameters$min.frac <<- as.numeric(str_trim(tclvalue(min.frac)))
        Parameters$opr.fun <<- str_trim(tclvalue(opr.fun))
        Parameters$opr.thres <<- as.numeric(str_trim(tclvalue(opr.thres)))

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
    tkwm.title(tt, "Aggregation Function")
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
