
getInfo_AggregateFun <- function(parent.win, Parameters,
								 FUN = c("sum", "mean", "median",
								 		 "var", "sd",
								 		 "max", "min", "count")
								)
{
    if(WindowsOS()){
        largeur0 <- 23
        largeur1 <- 8
    }else{
        largeur0 <- 20
        largeur1 <- 6
    }

    xml.dlg <- file.path(.cdtDir$dirLocal, "languages", "cdtInfoAggregationFun_dlgBox.xml")
    lang.dlg <- cdtLanguageParse(xml.dlg, .cdtData$Config$lang.iso)

    ###################

    tt <- tktoplevel()
    tkgrab.set(tt)
    tkfocus(tt)

    frMRG0 <- tkframe(tt, relief = 'raised', borderwidth = 2)
    frMRG1 <- tkframe(tt)

    ###################

    frameAggr <- tkframe(frMRG0, relief = "groove", borderwidth = 2)

    aggr.fun <- tclVar(Parameters$aggr.fun)
    opr.fun <- tclVar(Parameters$opr.fun)
    opr.thres <- tclVar(Parameters$opr.thres)

    stateCount <- if(str_trim(Parameters$aggr.fun) == "count") 'normal' else 'disabled'

    txt.aggfun <- tklabel(frameAggr, text = lang.dlg[['label']][['1']], anchor = 'w', justify = 'left')
    cb.aggfun <- ttkcombobox(frameAggr, values = FUN, textvariable = aggr.fun, width = largeur1)
    txt.opfun <- tklabel(frameAggr, text = lang.dlg[['label']][['2']], anchor = 'w', justify = 'left')
    cb.opfun <- ttkcombobox(frameAggr, values = c(">=", ">", "<=", "<"), textvariable = opr.fun, width = 4, state = stateCount)
    txt.opthres <- tklabel(frameAggr, text = lang.dlg[['label']][['3']], anchor = 'w', justify = 'left')
    en.opthres <- tkentry(frameAggr, textvariable = opr.thres, width = largeur1, state = stateCount)

    tkgrid(txt.aggfun, row = 0, column = 0, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
    tkgrid(cb.aggfun, row = 0, column = 1, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
    tkgrid(txt.opfun, row = 0, column = 2, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
    tkgrid(cb.opfun, row = 0, column = 3, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
    tkgrid(txt.opthres, row = 0, column = 4, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
    tkgrid(en.opthres, row = 0, column = 5, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)

    helpWidget(cb.aggfun, lang.dlg[['tooltip']][['1']], lang.dlg[['status']][['1']])
    helpWidget(cb.opfun, lang.dlg[['tooltip']][['2']], lang.dlg[['status']][['2']])
    helpWidget(en.opthres, lang.dlg[['tooltip']][['3']], lang.dlg[['status']][['3']])

    ##############

    tkbind(cb.aggfun, "<<ComboboxSelected>>", function(){
        stateCount <- if(tclvalue(aggr.fun) == "count") "normal" else "disabled"
        tkconfigure(cb.opfun, state = stateCount)
        tkconfigure(en.opthres, state = stateCount)
    })

    ###################

    frMinFr <- tkframe(frMRG0, relief = "groove", borderwidth = 2)

    uniqueMinFr <- tclVar(Parameters$min.frac$unique)
    allMinFr <- tclVar(Parameters$min.frac$all)

    stateMinFr0 <- if(Parameters$min.frac$unique) "normal" else "disabled"

    chk.OneMinFr <- tkcheckbutton(frMinFr, variable = uniqueMinFr, text = lang.dlg[['label']][['4']], anchor = 'w', justify = 'left')
    txt.OneMinFr <- tklabel(frMinFr, text = "", justify = "right", anchor = 'e', width = largeur0)
    txt.allMinFr <- tklabel(frMinFr, text = lang.dlg[['label']][['5']], anchor = 'e', justify = 'right')
    en.allMinFr <- tkentry(frMinFr, textvariable = allMinFr, width = largeur1, state = stateMinFr0)

    tkgrid(chk.OneMinFr, row = 0, column = 0, sticky = 'we', rowspan = 1, columnspan = 10, padx = 1, pady = 0, ipadx = 1, ipady = 1)
    tkgrid(txt.OneMinFr, row = 1, column = 0, sticky = 'we', rowspan = 1, columnspan = 4, padx = 0, pady = 0, ipadx = 1, ipady = 1)
    tkgrid(txt.allMinFr, row = 1, column = 4, sticky = 'e', rowspan = 1, columnspan = 5, padx = 0, pady = 0, ipadx = 1, ipady = 1)
    tkgrid(en.allMinFr, row = 1, column = 9, sticky = 'we', rowspan = 1, columnspan = 1, padx = 0, pady = 0, ipadx = 1, ipady = 1)

    helpWidget(en.allMinFr, lang.dlg[['tooltip']][['4']], lang.dlg[['status']][['4']])

    ##############

    tkbind(chk.OneMinFr, "<Button-1>", function(){
        stateMinFr0 <- if(tclvalue(uniqueMinFr) == '0') "normal" else "disabled"
        tkconfigure(en.allMinFr, state = stateMinFr0)

        stateMinFr1 <- if(tclvalue(uniqueMinFr) == '1') "normal" else "disabled"
        for(j in 1:12) tkconfigure(en.monMinFr[[j]], state = stateMinFr1)
    })

    ###################

    frMinFrMon <- ttklabelframe(frMRG0, text = lang.dlg[['label']][['6']], labelanchor = "nw", relief = "groove", borderwidth = 2)

    monthMinFr <- lapply(Parameters$min.frac$month, tclVar)

    stateMinFr1 <- if(Parameters$min.frac$unique) "disabled" else "normal"

    txt.monMinFr <- vector(12, mode = 'list')
    en.monMinFr <- vector(12, mode = 'list')
    MOIS <- format(ISOdate(2014, 1:12, 1), "%B")

    txtCol <- c(0, 2, 4)
    enCol <- c(1, 3, 5)

    for(j in 1:12){
        txt.monMinFr[[j]] <- tklabel(frMinFrMon, text = MOIS[j], anchor = 'e', justify = 'right')
        en.monMinFr[[j]] <- tkentry(frMinFrMon, textvariable = monthMinFr[[j]], width = largeur1, state = stateMinFr1)

        j1 <- j %% 3
        j1[j1 == 0] <- 3

        tkgrid(txt.monMinFr[[j]], row = ceiling(j/3) - 1, column = txtCol[j1], sticky = 'e', padx = 1, pady = 1, ipadx = 1, ipady = 1)
        tkgrid(en.monMinFr[[j]], row = ceiling(j/3) - 1, column = enCol[j1], sticky = 'we', padx = 1, pady = 1, ipadx = 1, ipady = 1)
    }

    ################################

    tkgrid(frameAggr, row = 0, column = 0, sticky = 'we', padx = 3, pady = 5, ipadx = 1, ipady = 1)
    tkgrid(frMinFr, row = 1, column = 0, sticky = 'we', padx = 3, pady = 3, ipadx = 1, ipady = 1)
    tkgrid(frMinFrMon, row = 2, column = 0, sticky = 'we', padx = 3, pady = 3, ipadx = 1, ipady = 1)

    ################################

    bt.prm.OK <- ttkbutton(frMRG1, text = .cdtEnv$tcl$lang$global[['button']][['1']])
    bt.prm.CA <- ttkbutton(frMRG1, text = .cdtEnv$tcl$lang$global[['button']][['2']])

    tkconfigure(bt.prm.OK, command = function(){
        Parameters$aggr.fun <<- str_trim(tclvalue(aggr.fun))
        Parameters$opr.fun <<- str_trim(tclvalue(opr.fun))
        Parameters$opr.thres <<- as.numeric(str_trim(tclvalue(opr.thres)))

        Parameters$min.frac$unique <<- switch(tclvalue(uniqueMinFr), '0' = FALSE, '1' = TRUE)
        if(Parameters$min.frac$unique)
            Parameters$min.frac$all <<- as.numeric(str_trim(tclvalue(allMinFr)))
        else
            Parameters$min.frac$month <<- as.numeric(str_trim(sapply(monthMinFr, tclvalue)))

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
    tkgrid(frMRG0, row = 0, column = 0, sticky = 'nswe', rowspan = 1, columnspan = 2, padx = 1, pady = 3, ipadx = 1, ipady = 1)
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
