
Summary.GraphOptions.Boxplot <- function(GraphOpt, parent.win = .cdtEnv$tcl$main$win){
    if(WindowsOS()){
        largeur1 <- 54
        largeur2 <- 58
        width.col <- 3
        width.spin <- 4
    }else{
        largeur1 <- 54
        largeur2 <- 58
        width.col <- 1
        width.spin <- 4
    }

    #####################
    xml.dlg <- file.path(.cdtDir$dirLocal, "languages", "cdtGraphOptions.Boxplot_dlgBox.xml")
    lang.dlg <- cdtLanguageParse(xml.dlg, .cdtData$Config$lang.iso)

    #####################
    tt <- tktoplevel()
    tkgrab.set(tt)
    tkfocus(tt)

    #####################
    frDialog <- tkframe(tt, relief = 'raised', borderwidth = 2)
    frButt <- tkframe(tt)

    #####################

    frameGraphAxLabs <- ttklabelframe(frDialog, text = lang.dlg[['label']][['1']], relief = 'groove')

    is.xaxis.lab <- tclVar(GraphOpt$boxplot$axislabs$is.xlab)
    is.yaxis.lab <- tclVar(GraphOpt$boxplot$axislabs$is.ylab)
    xaxis.lab <- tclVar(GraphOpt$boxplot$axislabs$xlab)
    yaxis.lab <- tclVar(GraphOpt$boxplot$axislabs$ylab)

    stateXLab <- if(GraphOpt$boxplot$axislabs$is.xlab) 'normal' else 'disabled'
    stateYLab <- if(GraphOpt$boxplot$axislabs$is.ylab) 'normal' else 'disabled'

    chk.Xlab <- tkcheckbutton(frameGraphAxLabs, variable = is.xaxis.lab, text = 'Xlab', anchor = 'w', justify = 'left')
    en.Xlab <- tkentry(frameGraphAxLabs, textvariable = xaxis.lab, width = largeur1, state = stateXLab)
    chk.Ylab <- tkcheckbutton(frameGraphAxLabs, variable = is.yaxis.lab, text = 'Ylab', anchor = 'w', justify = 'left')
    en.Ylab <- tkentry(frameGraphAxLabs, textvariable = yaxis.lab, width = largeur1, state = stateYLab)

    tkgrid(chk.Xlab, row = 0, column = 0, sticky = 'we')
    tkgrid(en.Xlab, row = 0, column = 1, sticky = 'we')
    tkgrid(chk.Ylab, row = 1, column = 0, sticky = 'we')
    tkgrid(en.Ylab, row = 1, column = 1, sticky = 'we')

    #########

    tkbind(chk.Xlab, "<Button-1>", function(){
        stateXLab <- if(tclvalue(is.xaxis.lab) == '0') 'normal' else 'disabled'
        tkconfigure(en.Xlab, state = stateXLab)
    })

    tkbind(chk.Ylab, "<Button-1>", function(){
        stateYLab <- if(tclvalue(is.yaxis.lab) == '0') 'normal' else 'disabled'
        tkconfigure(en.Ylab, state = stateYLab)
    })

    #####################

    frameGraphTitle <- ttklabelframe(frDialog, text = lang.dlg[['label']][['2']], relief = 'groove')

    is.title <- tclVar(GraphOpt$boxplot$title$is.title)
    text.title <- tclVar(GraphOpt$boxplot$title$title)

    stateGpTlt <- if(GraphOpt$boxplot$title$is.title) 'normal' else 'disabled'

    chk.GpTlt <- tkcheckbutton(frameGraphTitle, variable = is.title, anchor = 'e', justify = 'right')
    en.GpTlt <- tkentry(frameGraphTitle, textvariable = text.title, width = largeur2, state = stateGpTlt)

    tkgrid(chk.GpTlt, row = 0, column = 0, sticky = 'e', rowspan = 1, columnspan = 1)
    tkgrid(en.GpTlt, row = 0, column = 1, sticky = 'we', rowspan = 1, columnspan = 1)

    #########

    tkbind(chk.GpTlt, "<Button-1>", function(){
        stateGpTlt <- if(tclvalue(is.title) == '0') 'normal' else 'disabled'
        tkconfigure(en.GpTlt, state = stateGpTlt)
    })

    #####################

    frameBox <- ttklabelframe(frDialog, text = lang.dlg[['label']][['3']], relief = 'groove')

    box.Kol <- tclVar(GraphOpt$boxplot$col$col)
    med.Kol <- tclVar(GraphOpt$boxplot$col$medcol)
    out.Kol <- tclVar(GraphOpt$boxplot$col$boxcol)

    txt.boxC <- tklabel(frameBox, text = lang.dlg[['label']][['4']], anchor = 'e', justify = 'right')
    bt.boxC <- tkbutton(frameBox, bg = tclvalue(box.Kol), width = width.col)
    txt.medC <- tklabel(frameBox, text = lang.dlg[['label']][['5']], anchor = 'e', justify = 'right')
    bt.medC <- tkbutton(frameBox, bg = tclvalue(med.Kol), width = width.col)
    txt.outC <- tklabel(frameBox, text = lang.dlg[['label']][['6']], anchor = 'e', justify = 'right')
    bt.outC <- tkbutton(frameBox, bg = tclvalue(out.Kol), width = width.col)

    tkgrid(txt.boxC, bt.boxC, txt.medC, bt.medC, txt.outC, bt.outC)

    ######
    tkconfigure(bt.boxC, command = function(){
        tcl('wm', 'attributes', tt, topmost = FALSE)
        loko <- str_trim(tclvalue(tcl("tk_chooseColor", initialcolor = tclvalue(box.Kol), title = lang.dlg[['label']][['7']])))
        tcl('wm', 'attributes', tt, topmost = TRUE)
        if(nchar(loko) > 0){
            tkconfigure(bt.boxC, bg = loko)
            tclvalue(box.Kol) <- loko
        }
    })

    tkconfigure(bt.medC, command = function(){
        tcl('wm', 'attributes', tt, topmost = FALSE)
        loko <- str_trim(tclvalue(tcl("tk_chooseColor", initialcolor = tclvalue(med.Kol), title = lang.dlg[['label']][['7']])))
        tcl('wm', 'attributes', tt, topmost = TRUE)
        if(nchar(loko) > 0){
            tkconfigure(bt.medC, bg = loko)
            tclvalue(med.Kol) <- loko
        }
    })

    tkconfigure(bt.outC, command = function(){
        tcl('wm', 'attributes', tt, topmost = FALSE)
        loko <- str_trim(tclvalue(tcl("tk_chooseColor", initialcolor = tclvalue(out.Kol), title = lang.dlg[['label']][['7']])))
        tcl('wm', 'attributes', tt, topmost = TRUE)
        if(nchar(loko) > 0){
            tkconfigure(bt.outC, bg = loko)
            tclvalue(out.Kol) <- loko
        }
    })

    #####################

    tkgrid(frameGraphAxLabs, row = 0, column = 0, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 2, ipadx = 1, ipady = 1)
    tkgrid(frameGraphTitle, row = 1, column = 0, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 2, ipadx = 1, ipady = 1)
    tkgrid(frameBox, row = 2, column = 0, sticky = '', rowspan = 1, columnspan = 1, padx = 1, pady = 2, ipadx = 1, ipady = 1)

    #####################
    bt.opt.OK <- ttkbutton(frButt, text = .cdtEnv$tcl$lang$global[['button']][['1']])
    bt.opt.CA <- ttkbutton(frButt, text = .cdtEnv$tcl$lang$global[['button']][['2']])

    tkconfigure(bt.opt.OK, command = function(){
        GraphOpt$boxplot$axislabs$is.xlab <<- switch(tclvalue(is.xaxis.lab), '0' = FALSE, '1' = TRUE)
        GraphOpt$boxplot$axislabs$is.ylab <<- switch(tclvalue(is.yaxis.lab), '0' = FALSE, '1' = TRUE)
        GraphOpt$boxplot$axislabs$xlab <<- gsub('\\\\n', '\n', str_trim(tclvalue(xaxis.lab)))
        GraphOpt$boxplot$axislabs$ylab <<- gsub('\\\\n', '\n', str_trim(tclvalue(yaxis.lab)))

        GraphOpt$boxplot$title$is.title <<- switch(tclvalue(is.title), '0' = FALSE, '1' = TRUE)
        GraphOpt$boxplot$title$title <<- gsub('\\\\n', '\n', str_trim(tclvalue(text.title)))

        GraphOpt$boxplot$col$col <<- tclvalue(box.Kol)
        GraphOpt$boxplot$col$medcol <<- tclvalue(med.Kol)
        GraphOpt$boxplot$col$boxcol <<- tclvalue(out.Kol)

        tkgrab.release(tt)
        tkdestroy(tt)
        tkfocus(parent.win)
    })

    tkconfigure(bt.opt.CA, command = function(){
        tkgrab.release(tt)
        tkdestroy(tt)
        tkfocus(parent.win)
    })

    tkgrid(bt.opt.OK, row = 0, column = 0, padx = 5, pady = 1, ipadx = 1, sticky = 'w')
    tkgrid(bt.opt.CA, row = 0, column = 1, padx = 5, pady = 1, ipadx = 1, sticky = 'e')

    ###############################################################

    tkgrid(frDialog, row = 0, column = 0, sticky = 'nswe', rowspan = 1, columnspan = 2, padx = 2, pady = 1, ipadx = 1, ipady = 1)
    tkgrid(frButt, row = 1, column = 1, sticky = 'se', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)

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

    ##################################################################
    tkfocus(tt)
    tkbind(tt, "<Destroy>", function(){
        tkgrab.release(tt)
        tkfocus(parent.win)
    })
    tkwait.window(tt)
    return(GraphOpt)
}

####################################################################################################

Summary.GraphOptions.Histogram <- function(GraphOpt, parent.win = .cdtEnv$tcl$main$win){
    if(WindowsOS()){
        largeur1 <- 59
        largeur2 <- 63
        largeur3 <- 67
        width.col <- 3
        width.spin <- 4
    }else{
        largeur1 <- 57
        largeur2 <- 60
        largeur3 <- 64
        width.col <- 1
        width.spin <- 4
    }

    #####################
    xml.dlg <- file.path(.cdtDir$dirLocal, "languages", "cdtGraphOptions.Histogram_dlgBox.xml")
    lang.dlg <- cdtLanguageParse(xml.dlg, .cdtData$Config$lang.iso)

    #####################
    tt <- tktoplevel()
    tkgrab.set(tt)
    tkfocus(tt)

    #####################
    frDialog <- tkframe(tt, relief = 'raised', borderwidth = 2)
    frButt <- tkframe(tt)

    #####################

    frameGraphAxLabs <- ttklabelframe(frDialog, text = lang.dlg[['label']][['1']], relief = 'groove')

    is.xaxis.lab <- tclVar(GraphOpt$histogram$axislabs$is.xlab)
    is.yaxis.lab <- tclVar(GraphOpt$histogram$axislabs$is.ylab)
    xaxis.lab <- tclVar(GraphOpt$histogram$axislabs$xlab)
    yaxis.lab <- tclVar(GraphOpt$histogram$axislabs$ylab)

    stateXLab <- if(GraphOpt$histogram$axislabs$is.xlab) 'normal' else 'disabled'
    stateYLab <- if(GraphOpt$histogram$axislabs$is.ylab) 'normal' else 'disabled'

    chk.Xlab <- tkcheckbutton(frameGraphAxLabs, variable = is.xaxis.lab, text = 'Xlab', anchor = 'w', justify = 'left')
    en.Xlab <- tkentry(frameGraphAxLabs, textvariable = xaxis.lab, width = largeur1, state = stateXLab)
    chk.Ylab <- tkcheckbutton(frameGraphAxLabs, variable = is.yaxis.lab, text = 'Ylab', anchor = 'w', justify = 'left')
    en.Ylab <- tkentry(frameGraphAxLabs, textvariable = yaxis.lab, width = largeur1, state = stateYLab)

    tkgrid(chk.Xlab, row = 0, column = 0, sticky = 'we')
    tkgrid(en.Xlab, row = 0, column = 1, sticky = 'we')
    tkgrid(chk.Ylab, row = 1, column = 0, sticky = 'we')
    tkgrid(en.Ylab, row = 1, column = 1, sticky = 'we')

    #########

    tkbind(chk.Xlab, "<Button-1>", function(){
        stateXLab <- if(tclvalue(is.xaxis.lab) == '0') 'normal' else 'disabled'
        tkconfigure(en.Xlab, state = stateXLab)
    })

    tkbind(chk.Ylab, "<Button-1>", function(){
        stateYLab <- if(tclvalue(is.yaxis.lab) == '0') 'normal' else 'disabled'
        tkconfigure(en.Ylab, state = stateYLab)
    })

    #####################

    frameGraphTitle <- ttklabelframe(frDialog, text = lang.dlg[['label']][['2']], relief = 'groove')

    is.title <- tclVar(GraphOpt$histogram$title$is.title)
    text.title <- tclVar(GraphOpt$histogram$title$title)

    stateGpTlt <- if(GraphOpt$histogram$title$is.title) 'normal' else 'disabled'

    chk.GpTlt <- tkcheckbutton(frameGraphTitle, variable = is.title, anchor = 'e', justify = 'right')
    en.GpTlt <- tkentry(frameGraphTitle, textvariable = text.title, width = largeur2, state = stateGpTlt)

    tkgrid(chk.GpTlt, row = 0, column = 0, sticky = 'e', rowspan = 1, columnspan = 1)
    tkgrid(en.GpTlt, row = 0, column = 1, sticky = 'we', rowspan = 1, columnspan = 1)

    #########

    tkbind(chk.GpTlt, "<Button-1>", function(){
        stateGpTlt <- if(tclvalue(is.title) == '0') 'normal' else 'disabled'
        tkconfigure(en.GpTlt, state = stateGpTlt)
    })

    #####################

    frameHist <- ttklabelframe(frDialog, text = lang.dlg[['label']][['3']], relief = 'groove')

    edit.breaks <- tclVar(GraphOpt$histogram$hist$user.break)
    inner.Kol <- tclVar(GraphOpt$histogram$hist$col)
    border.Kol <- tclVar(GraphOpt$histogram$hist$border)

    chk.breaks <- tkcheckbutton(frameHist, variable = edit.breaks, text = lang.dlg[['checkbutton']][['2']], anchor = 'w', justify = 'left')

    txt.innerC <- tklabel(frameHist, text = lang.dlg[['label']][['4']], anchor = 'e', justify = 'right')
    bt.innerC <- tkbutton(frameHist, bg = tclvalue(inner.Kol), width = width.col)
    txt.borderC <- tklabel(frameHist, text = lang.dlg[['label']][['5']], anchor = 'e', justify = 'right')
    bt.borderC <- tkbutton(frameHist, bg = tclvalue(border.Kol), width = width.col)

    tkgrid(chk.breaks, row = 0, columnspan = 4, sticky = 'w')
    tkgrid(txt.innerC, bt.innerC, txt.borderC, bt.borderC)

    ######
    tkconfigure(bt.innerC, command = function(){
        tcl('wm', 'attributes', tt, topmost = FALSE)
        loko <- str_trim(tclvalue(tcl("tk_chooseColor", initialcolor = tclvalue(inner.Kol), title = lang.dlg[['label']][['7']])))
        tcl('wm', 'attributes', tt, topmost = TRUE)
        if(nchar(loko) > 0){
            tkconfigure(bt.innerC, bg = loko)
            tclvalue(inner.Kol) <- loko
        }
    })

    tkconfigure(bt.borderC, command = function(){
        tcl('wm', 'attributes', tt, topmost = FALSE)
        loko <- str_trim(tclvalue(tcl("tk_chooseColor", initialcolor = tclvalue(border.Kol), title = lang.dlg[['label']][['7']])))
        tcl('wm', 'attributes', tt, topmost = TRUE)
        if(nchar(loko) > 0){
            tkconfigure(bt.borderC, bg = loko)
            tclvalue(border.Kol) <- loko
        }
    })

    tkbind(chk.breaks, "<Button-1>", function(){
        tkdestroy(frameBreak)
        frameBreak <<- ttklabelframe(frDialog, text = lang.dlg[['label']][['11']], relief = 'groove')

        if(tclvalue(edit.breaks) == '0'){
            en.breaksV <- tkentry(frameBreak, textvariable = breaks.Vect, width = largeur3)
            tkgrid(en.breaksV, sticky = 'we')
            helpWidget(en.breaksV, lang.dlg[['tooltip']][['1']], lang.dlg[['status']][['1']])

            tkgrid(frameBreak, row = 3, column = 0, sticky = 'we', rowspan = 1, columnspan = 2, padx = 1, pady = 2, ipadx = 1, ipady = 2)
        }
    })

    #####################

    frameBw <- ttklabelframe(frDialog, text = lang.dlg[['label']][['6']], relief = 'groove')

    bwd.Add <- tclVar(GraphOpt$histogram$bw$add)
    bwd.Val <- tclVar(GraphOpt$histogram$bw$bw)
    bwd.Kol <- tclVar(GraphOpt$histogram$bw$col)

    stateBwd <- if(GraphOpt$histogram$bw$add) 'normal' else 'disabled'

    chk.Bwd <- tkcheckbutton(frameBw, variable = bwd.Add, text = lang.dlg[['checkbutton']][['1']], anchor = 'w', justify = 'left')

    txt.bwB <- tklabel(frameBw, text = lang.dlg[['label']][['9']], anchor = 'e', justify = 'right')
    en.bwB <- tkentry(frameBw, textvariable = bwd.Val, width = 3, state = stateBwd)
    txt.bwC <- tklabel(frameBw, text = lang.dlg[['label']][['8']], anchor = 'e', justify = 'right')
    bt.bwC <- tkbutton(frameBw, bg = tclvalue(bwd.Kol), width = width.col, state = stateBwd)
    txt.bwL <- tklabel(frameBw, text = lang.dlg[['label']][['10']], anchor = 'e', justify = 'right')
    spin.bwL <- ttkspinbox(frameBw, from = 0.5, to = 4, increment = 0.1, justify = 'center', width = width.spin, state = stateBwd)
    tkset(spin.bwL, GraphOpt$histogram$bw$lwd)

    tkgrid(chk.Bwd, row = 0, column = 0, sticky = 'we', rowspan = 1, columnspan = 6)
    tkgrid(txt.bwB, en.bwB, txt.bwC, bt.bwC, txt.bwL, spin.bwL)

    ######

    tkconfigure(bt.bwC, command = function(){
        tcl('wm', 'attributes', tt, topmost = FALSE)
        loko <- str_trim(tclvalue(tcl("tk_chooseColor", initialcolor = tclvalue(bwd.Kol), title = lang.dlg[['label']][['7']])))
        tcl('wm', 'attributes', tt, topmost = TRUE)
        if(nchar(loko) > 0){
            tkconfigure(bt.bwC, bg = loko)
            tclvalue(bwd.Kol) <- loko
        }
    })

    tkbind(chk.Bwd, "<Button-1>", function(){
        stateBwd <- if(tclvalue(bwd.Add) == '0') 'normal' else 'disabled'
        tkconfigure(en.bwB, state = stateBwd)
        tkconfigure(bt.bwC, state = stateBwd)
        tkconfigure(spin.bwL, state = stateBwd)
    })

    #####################

    tkgrid(frameGraphAxLabs, row = 0, column = 0, sticky = 'we', rowspan = 1, columnspan = 2, padx = 1, pady = 2, ipadx = 1, ipady = 1)
    tkgrid(frameGraphTitle, row = 1, column = 0, sticky = 'we', rowspan = 1, columnspan = 2, padx = 1, pady = 2, ipadx = 1, ipady = 1)
    tkgrid(frameHist, row = 2, column = 0, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 2, ipadx = 1, ipady = 1)
    tkgrid(frameBw, row = 2, column = 1, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 2, ipadx = 1, ipady = 1)

    #####################

    frameBreak <- ttklabelframe(frDialog, text = lang.dlg[['label']][['11']], relief = 'groove')

    breaksV <- ''
    if(!is.null(GraphOpt$histogram$hist$breaks)){
        breaksV <- paste0(GraphOpt$histogram$hist$breaks, collapse = ", ")
    }

    breaks.Vect <- tclVar(breaksV)

    if(tclvalue(edit.breaks) == '1'){
        en.breaksV <- tkentry(frameBreak, textvariable = breaks.Vect, width = largeur3)
        tkgrid(en.breaksV, sticky = 'we')
        helpWidget(en.breaksV, lang.dlg[['tooltip']][['1']], lang.dlg[['status']][['1']])

        tkgrid(frameBreak, row = 3, column = 0, sticky = 'we', rowspan = 1, columnspan = 2, padx = 1, pady = 2, ipadx = 1, ipady = 2)
    }

    #####################
    bt.opt.OK <- ttkbutton(frButt, text = .cdtEnv$tcl$lang$global[['button']][['1']])
    bt.opt.CA <- ttkbutton(frButt, text = .cdtEnv$tcl$lang$global[['button']][['2']])

    tkconfigure(bt.opt.OK, command = function(){
        GraphOpt$histogram$axislabs$is.xlab <<- switch(tclvalue(is.xaxis.lab), '0' = FALSE, '1' = TRUE)
        GraphOpt$histogram$axislabs$is.ylab <<- switch(tclvalue(is.yaxis.lab), '0' = FALSE, '1' = TRUE)
        GraphOpt$histogram$axislabs$xlab <<- gsub('\\\\n', '\n', str_trim(tclvalue(xaxis.lab)))
        GraphOpt$histogram$axislabs$ylab <<- gsub('\\\\n', '\n', str_trim(tclvalue(yaxis.lab)))

        GraphOpt$histogram$title$is.title <<- switch(tclvalue(is.title), '0' = FALSE, '1' = TRUE)
        GraphOpt$histogram$title$title <<- gsub('\\\\n', '\n', str_trim(tclvalue(text.title)))

        GraphOpt$histogram$hist$col <<- tclvalue(inner.Kol)
        GraphOpt$histogram$hist$border <<- tclvalue(border.Kol)

        GraphOpt$histogram$bw$add <<- switch(tclvalue(bwd.Add), '0' = FALSE, '1' = TRUE)
        GraphOpt$histogram$bw$bw <<- as.numeric(str_trim(tclvalue(bwd.Val)))
        GraphOpt$histogram$bw$col <<- tclvalue(bwd.Kol)
        GraphOpt$histogram$bw$lwd <<- as.numeric(str_trim(tclvalue(tkget(spin.bwL))))

        GraphOpt$histogram$hist$user.break <<- switch(tclvalue(edit.breaks), '0' = FALSE, '1' = TRUE)

        if(GraphOpt$histogram$hist$user.break){
            breaksV <- str_trim(tclvalue(breaks.Vect))
            breaksV <- strsplit(breaksV, ",")[[1]]
            breaksV <- str_trim(breaksV)
            breaksV <- breaksV[breaksV != ""]
            breaksV <- if(length(breaksV) > 0) as.numeric(breaksV) else NULL
            if(length(breaksV) == 0){
                cdt.tkmessageBox(tt, message = lang.dlg[['message']][['1']], icon = "warning", type = "ok")
                tkwait.window(tt)
            }

            breaksV <- as.numeric(breaksV)

            if(any(is.na(breaksV))){
                cdt.tkmessageBox(tt, message = lang.dlg[['message']][['2']], icon = "warning", type = "ok")
                tkwait.window(tt)
            }
            if(length(breaksV) < 2){
                cdt.tkmessageBox(tt, message = lang.dlg[['message']][['3']], icon = "warning", type = "ok")
                tkwait.window(tt)
            }
            GraphOpt$histogram$hist$breaks <<- breaksV
        }

        tkgrab.release(tt)
        tkdestroy(tt)
        tkfocus(parent.win)
    })

    tkconfigure(bt.opt.CA, command = function(){
        tkgrab.release(tt)
        tkdestroy(tt)
        tkfocus(parent.win)
    })

    tkgrid(bt.opt.OK, row = 0, column = 0, padx = 5, pady = 1, ipadx = 1, sticky = 'w')
    tkgrid(bt.opt.CA, row = 0, column = 1, padx = 5, pady = 1, ipadx = 1, sticky = 'e')

    ###############################################################

    tkgrid(frDialog, row = 0, column = 0, sticky = 'nswe', rowspan = 1, columnspan = 2, padx = 2, pady = 1, ipadx = 1, ipady = 1)
    tkgrid(frButt, row = 1, column = 1, sticky = 'se', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)

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

    ##################################################################
    tkfocus(tt)
    tkbind(tt, "<Destroy>", function(){
        tkgrab.release(tt)
        tkfocus(parent.win)
    })
    tkwait.window(tt)
    return(GraphOpt)
}
