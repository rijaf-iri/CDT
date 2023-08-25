
Validation.GraphOptions.Scatter <- function(GraphOpt, parent.win = .cdtEnv$tcl$main$win){
    if(WindowsOS()){
        largeur1 <- 55
        largeur2 <- 59
        largeur3 <- 5
        width.col <- 3
        width.spin <- 4
    }else{
        largeur1 <- 54
        largeur2 <- 58
        largeur3 <- 7
        width.col <- 1
        width.spin <- 4
    }

    #####################
    xml.dlg <- file.path(.cdtDir$dirLocal, "languages", "cdtGraphOptions.Scatter_Valid_dlgBox.xml")
    lang.dlg <- cdtLanguageParse(xml.dlg, .cdtData$Config$lang.iso)

    #####################
    tt <- tktoplevel()
    tkgrab.set(tt)
    tkfocus(tt)

    #####################
    frDialog <- tkframe(tt, relief = 'raised', borderwidth = 2)
    frButt <- tkframe(tt)

    #####################

    frameGraphXYlim <- tkframe(frDialog)

    ###########

    frameGraphXlim <- ttklabelframe(frameGraphXYlim, text = lang.dlg[['label']][['1']], relief = 'groove')

    is.min.xlim <- tclVar(GraphOpt$scatter$xlim$is.min)
    is.max.xlim <- tclVar(GraphOpt$scatter$xlim$is.max)
    min.xlim <- tclVar(GraphOpt$scatter$xlim$min)
    max.xlim <- tclVar(GraphOpt$scatter$xlim$max)

    stateMinXlim <- if(GraphOpt$scatter$xlim$is.min) 'normal' else 'disabled'
    stateMaxXlim <- if(GraphOpt$scatter$xlim$is.max) 'normal' else 'disabled'

    chk.min.Xlim <- tkcheckbutton(frameGraphXlim, variable = is.min.xlim, text = "Min", anchor = 'w', justify = 'left')
    en.min.Xlim <- tkentry(frameGraphXlim, textvariable = min.xlim, width = 5, state = stateMinXlim)
    chk.max.Xlim <- tkcheckbutton(frameGraphXlim, variable = is.max.xlim, text = "Max", anchor = 'w', justify = 'left')
    en.max.Xlim <- tkentry(frameGraphXlim, textvariable = max.xlim, width = 5, state = stateMaxXlim)

    tkgrid(chk.min.Xlim, row = 0, column = 0, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
    tkgrid(en.min.Xlim, row = 0, column = 1, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
    tkgrid(chk.max.Xlim, row = 0, column = 2, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
    tkgrid(en.max.Xlim, row = 0, column = 3, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)

    #########

    tkbind(chk.min.Xlim, "<Button-1>", function(){
        stateMinXlim <- if(tclvalue(is.min.xlim) == '0') 'normal' else 'disabled'
        tkconfigure(en.min.Xlim, state = stateMinXlim)
    })

    tkbind(chk.max.Xlim, "<Button-1>", function(){
        stateMaxXlim <- if(tclvalue(is.max.xlim) == '0') 'normal' else 'disabled'
        tkconfigure(en.max.Xlim, state = stateMaxXlim)
    })

    ###########

    frameGraphYlim <- ttklabelframe(frameGraphXYlim, text = lang.dlg[['label']][['2']], relief = 'groove')

    is.min.ylim <- tclVar(GraphOpt$scatter$ylim$is.min)
    is.max.ylim <- tclVar(GraphOpt$scatter$ylim$is.max)
    min.ylim <- tclVar(GraphOpt$scatter$ylim$min)
    max.ylim <- tclVar(GraphOpt$scatter$ylim$max)

    stateMinYlim <- if(GraphOpt$scatter$ylim$is.min) 'normal' else 'disabled'
    stateMaxYlim <- if(GraphOpt$scatter$ylim$is.max) 'normal' else 'disabled'

    chk.min.Ylim <- tkcheckbutton(frameGraphYlim, variable = is.min.ylim, text = "Min", anchor = 'w', justify = 'left')
    en.min.Ylim <- tkentry(frameGraphYlim, textvariable = min.ylim, width = 5, state = stateMinYlim)
    chk.max.Ylim <- tkcheckbutton(frameGraphYlim, variable = is.max.ylim, text = "Max", anchor = 'w', justify = 'left')
    en.max.Ylim <- tkentry(frameGraphYlim, textvariable = max.ylim, width = 5, state = stateMaxYlim)

    tkgrid(chk.min.Ylim, row = 0, column = 0, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
    tkgrid(en.min.Ylim, row = 0, column = 1, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
    tkgrid(chk.max.Ylim, row = 0, column = 2, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
    tkgrid(en.max.Ylim, row = 0, column = 3, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)

    #########

    tkbind(chk.min.Ylim, "<Button-1>", function(){
        stateMinYlim <- if(tclvalue(is.min.ylim) == '0') 'normal' else 'disabled'
        tkconfigure(en.min.Ylim, state = stateMinYlim)
    })

    tkbind(chk.max.Ylim, "<Button-1>", function(){
        stateMaxYlim <- if(tclvalue(is.max.ylim) == '0') 'normal' else 'disabled'
        tkconfigure(en.max.Ylim, state = stateMaxYlim)
    })

    #####################

    sepXYlim <- tklabel(frameGraphXYlim, text = "", width = largeur3)

    tkgrid(frameGraphXlim, row = 0, column = 0, sticky = 'w')
    tkgrid(sepXYlim, row = 0, column = 1, sticky = 'we')
    tkgrid(frameGraphYlim, row = 0, column = 2, sticky = 'e')

    #####################

    frameGraphAxLabs <- ttklabelframe(frDialog, text = lang.dlg[['label']][['3']], relief = 'groove')

    is.xaxis.lab <- tclVar(GraphOpt$scatter$axislabs$is.xlab)
    is.yaxis.lab <- tclVar(GraphOpt$scatter$axislabs$is.ylab)
    xaxis.lab <- tclVar(GraphOpt$scatter$axislabs$xlab)
    yaxis.lab <- tclVar(GraphOpt$scatter$axislabs$ylab)

    stateXLab <- if(GraphOpt$scatter$axislabs$is.xlab) 'normal' else 'disabled'
    stateYLab <- if(GraphOpt$scatter$axislabs$is.ylab) 'normal' else 'disabled'

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

    frameGraphTitle <- ttklabelframe(frDialog, text = lang.dlg[['label']][['4']], relief = 'groove')

    is.title <- tclVar(GraphOpt$scatter$title$is.title)
    text.title <- tclVar(GraphOpt$scatter$title$title)

    pos.title <- tclVar()
    CbposTitleVAL <- lang.dlg[['combobox']][['1']]
    posTitleVAL <- c("top", "bottom")
    tclvalue(pos.title) <- CbposTitleVAL[posTitleVAL %in% GraphOpt$scatter$title$position]

    stateGpTlt <- if(GraphOpt$scatter$title$is.title) 'normal' else 'disabled'

    chk.GpTlt <- tkcheckbutton(frameGraphTitle, variable = is.title, anchor = 'e', justify = 'right')
    en.GpTlt <- tkentry(frameGraphTitle, textvariable = text.title, width = largeur2, state = stateGpTlt)
    txt.GpTlt <- tklabel(frameGraphTitle, text = lang.dlg[['label']][['5']], anchor = 'e', justify = 'right')
    cb.GpTlt <- ttkcombobox(frameGraphTitle, values = CbposTitleVAL, textvariable = pos.title, width = 7, state = stateGpTlt)

    tkgrid(chk.GpTlt, row = 0, column = 0, sticky = 'e', rowspan = 1, columnspan = 1)
    tkgrid(en.GpTlt, row = 0, column = 1, sticky = 'we', rowspan = 1, columnspan = 7)
    tkgrid(txt.GpTlt, row = 1, column = 1, sticky = 'e', rowspan = 1, columnspan = 6)
    tkgrid(cb.GpTlt, row = 1, column = 7, sticky = 'we', rowspan = 1, columnspan = 1)

    #########

    tkbind(chk.GpTlt, "<Button-1>", function(){
        stateGpTlt <- if(tclvalue(is.title) == '0') 'normal' else 'disabled'
        tkconfigure(en.GpTlt, state = stateGpTlt)
        tkconfigure(cb.GpTlt, state = stateGpTlt)
    })

    #####################

    frameScatter <- tkframe(frDialog)

    ############

    framePoint <- ttklabelframe(frameScatter, text = lang.dlg[['label']][['6']], relief = 'groove')

    point.Kol <- tclVar(GraphOpt$scatter$point$col)

    txt.PtC <- tklabel(framePoint, text = lang.dlg[['label']][['7']], anchor = 'e', justify = 'right')
    bt.PtC <- tkbutton(framePoint, bg = tclvalue(point.Kol), width = width.col)
    txt.PtT <- tklabel(framePoint, text = lang.dlg[['label']][['8']], anchor = 'e', justify = 'right')
    spin.PtT <- ttkspinbox(framePoint, from = 15, to = 20, increment = 1, justify = 'center', width = width.spin)
    tkset(spin.PtT, GraphOpt$scatter$point$pch)
    txt.PtS <- tklabel(framePoint, text = lang.dlg[['label']][['9']], anchor = 'e', justify = 'right')
    spin.PtS <- ttkspinbox(framePoint, from = 0.5, to = 4, increment = 0.1, justify = 'center', width = width.spin)
    tkset(spin.PtS, GraphOpt$scatter$point$cex)

    tkgrid(txt.PtC, row = 0, column = 0, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
    tkgrid(bt.PtC, row = 0, column = 1, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
    tkgrid(txt.PtT, row = 0, column = 2, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
    tkgrid(spin.PtT, row = 0, column = 3, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
    tkgrid(txt.PtS, row = 0, column = 4, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
    tkgrid(spin.PtS, row = 0, column = 5, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)

    ######
    tkconfigure(bt.PtC, command = function(){
        tcl('wm', 'attributes', tt, topmost = FALSE)
        loko <- trimws(tclvalue(tcl("tk_chooseColor", initialcolor = tclvalue(point.Kol), title = lang.dlg[['label']][['10']])))
        tcl('wm', 'attributes', tt, topmost = TRUE)
        if(nchar(loko) > 0){
            tkconfigure(bt.PtC, bg = loko)
            tclvalue(point.Kol) <- loko
        }
    })

    ##########

    frameXY <- tkframe(frameScatter, relief = 'groove', borderwidth = 2)

    draw.XY <- tclVar(GraphOpt$scatter$line$draw)
    col.XY <- tclVar(GraphOpt$scatter$line$col)

    stateXY <- if(tclvalue(draw.XY) == '1') 'normal' else 'disabled'

    chk.pltLine <- tkcheckbutton(frameXY, variable = draw.XY, text = lang.dlg[['checkbutton']][['1']], anchor = 'w', justify = 'left')
    txt.pltLineC <- tklabel(frameXY, text = lang.dlg[['label']][['7']], anchor = 'e', justify = 'right')
    bt.pltLineC <- tkbutton(frameXY, bg = tclvalue(col.XY), width = width.col, state = stateXY)
    txt.pltLineW <- tklabel(frameXY, text = lang.dlg[['label']][['11']], anchor = 'e', justify = 'right')
    spin.pltLineW <- ttkspinbox(frameXY, from = 0.5, to = 4, increment = 0.1, justify = 'center', width = width.spin, state = stateXY)
    tkset(spin.pltLineW, GraphOpt$scatter$line$lwd)

    tkgrid(chk.pltLine, row = 0, column = 0, sticky = 'we', rowspan = 1, columnspan = 4, padx = 1, pady = 1, ipadx = 1, ipady = 1)
    tkgrid(txt.pltLineC, row = 1, column = 0, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
    tkgrid(bt.pltLineC, row = 1, column = 1, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
    tkgrid(txt.pltLineW, row = 1, column = 2, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
    tkgrid(spin.pltLineW, row = 1, column = 3, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)

    ######
    tkconfigure(bt.pltLineC, command = function(){
        tcl('wm', 'attributes', tt, topmost = FALSE)
        loko <- trimws(tclvalue(tcl("tk_chooseColor", initialcolor = tclvalue(col.XY), title = lang.dlg[['label']][['10']])))
        tcl('wm', 'attributes', tt, topmost = TRUE)
        if(nchar(loko) > 0){
            tkconfigure(bt.pltLineC, bg = loko)
            tclvalue(col.XY) <- loko
        }
    })

    tkbind(chk.pltLine, "<Button-1>", function(){
        stateXY <- if(tclvalue(draw.XY) == '1') 'disabled' else 'normal'
        tkconfigure(bt.pltLineC, state = stateXY)
        tkconfigure(spin.pltLineW, state = stateXY)
    })

    ###########

    tkgrid(framePoint, row = 0, column = 0, padx = 1)
    tkgrid(frameXY, row = 0, column = 1, padx = 1)

    #####################

    tkgrid(frameGraphXYlim, row = 0, column = 0, sticky = '', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
    tkgrid(frameGraphAxLabs, row = 1, column = 0, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 2, ipadx = 1, ipady = 1)
    tkgrid(frameGraphTitle, row = 2, column = 0, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 2, ipadx = 1, ipady = 1)
    tkgrid(frameScatter, row = 3, column = 0, sticky = '', rowspan = 1, columnspan = 1, padx = 1, pady = 2, ipadx = 1, ipady = 1)

    #####################
    bt.opt.OK <- ttkbutton(frButt, text = .cdtEnv$tcl$lang$global[['button']][['1']])
    bt.opt.CA <- ttkbutton(frButt, text = .cdtEnv$tcl$lang$global[['button']][['2']])

    tkconfigure(bt.opt.OK, command = function(){
        GraphOpt$scatter$xlim$is.min <<- switch(tclvalue(is.min.xlim), '0' = FALSE, '1' = TRUE)
        GraphOpt$scatter$xlim$is.max <<- switch(tclvalue(is.max.xlim), '0' = FALSE, '1' = TRUE)
        GraphOpt$scatter$xlim$min <<- as.numeric(trimws(tclvalue(min.xlim)))
        GraphOpt$scatter$xlim$max <<- as.numeric(trimws(tclvalue(max.xlim)))

        GraphOpt$scatter$ylim$is.min <<- switch(tclvalue(is.min.ylim), '0' = FALSE, '1' = TRUE)
        GraphOpt$scatter$ylim$is.max <<- switch(tclvalue(is.max.ylim), '0' = FALSE, '1' = TRUE)
        GraphOpt$scatter$ylim$min <<- as.numeric(trimws(tclvalue(min.ylim)))
        GraphOpt$scatter$ylim$max <<- as.numeric(trimws(tclvalue(max.ylim)))

        GraphOpt$scatter$axislabs$is.xlab <<- switch(tclvalue(is.xaxis.lab), '0' = FALSE, '1' = TRUE)
        GraphOpt$scatter$axislabs$is.ylab <<- switch(tclvalue(is.yaxis.lab), '0' = FALSE, '1' = TRUE)
        GraphOpt$scatter$axislabs$xlab <<- gsub('\\\\n', '\n', trimws(tclvalue(xaxis.lab)))
        GraphOpt$scatter$axislabs$ylab <<- gsub('\\\\n', '\n', trimws(tclvalue(yaxis.lab)))

        GraphOpt$scatter$title$is.title <<- switch(tclvalue(is.title), '0' = FALSE, '1' = TRUE)
        GraphOpt$scatter$title$title <<- gsub('\\\\n', '\n', trimws(tclvalue(text.title)))
        GraphOpt$scatter$title$position <<- posTitleVAL[CbposTitleVAL %in% trimws(tclvalue(pos.title))]

        GraphOpt$scatter$point$col <<- tclvalue(point.Kol)
        GraphOpt$scatter$point$pch <<- as.numeric(trimws(tclvalue(tkget(spin.PtT))))
        GraphOpt$scatter$point$cex <<- as.numeric(trimws(tclvalue(tkget(spin.PtS))))

        GraphOpt$scatter$line$draw <<- switch(tclvalue(draw.XY), '0' = FALSE, '1' = TRUE)
        GraphOpt$scatter$line$col <<- tclvalue(col.XY)
        GraphOpt$scatter$line$lwd <<- as.numeric(trimws(tclvalue(tkget(spin.pltLineW))))

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

#######################################################################################################

Validation.GraphOptions.CDF <- function(GraphOpt, parent.win = .cdtEnv$tcl$main$win){
    if(WindowsOS()){
        largeur1 <- 63
        largeur2 <- 67
        largeur3 <- 12
        largeur4 <- 35
        width.col <- 3
        width.spin <- 4
    }else{
        largeur1 <- 60
        largeur2 <- 64
        largeur3 <- 12
        largeur4 <- 33
        width.col <- 1
        width.spin <- 4
    }

    #####################
    xml.dlg <- file.path(.cdtDir$dirLocal, "languages", "cdtGraphOptions.CDF_Valid_dlgBox.xml")
    lang.dlg <- cdtLanguageParse(xml.dlg, .cdtData$Config$lang.iso)

    #####################
    tt <- tktoplevel()
    tkgrab.set(tt)
    tkfocus(tt)

    #####################
    frDialog <- tkframe(tt, relief = 'raised', borderwidth = 2)
    frButt <- tkframe(tt)

    #####################

    frameGraphXYlim <- tkframe(frDialog)

    ########

    frameGraphXlim <- ttklabelframe(frameGraphXYlim, text = lang.dlg[['label']][['1']], relief = 'groove')

    is.min.xlim <- tclVar(GraphOpt$cdf$xlim$is.min)
    is.max.xlim <- tclVar(GraphOpt$cdf$xlim$is.max)
    min.xlim <- tclVar(GraphOpt$cdf$xlim$min)
    max.xlim <- tclVar(GraphOpt$cdf$xlim$max)

    stateMinXlim <- if(GraphOpt$cdf$xlim$is.min) 'normal' else 'disabled'
    stateMaxXlim <- if(GraphOpt$cdf$xlim$is.max) 'normal' else 'disabled'

    chk.min.Xlim <- tkcheckbutton(frameGraphXlim, variable = is.min.xlim, text = "Min", anchor = 'w', justify = 'left')
    en.min.Xlim <- tkentry(frameGraphXlim, textvariable = min.xlim, width = 5, state = stateMinXlim)
    chk.max.Xlim <- tkcheckbutton(frameGraphXlim, variable = is.max.xlim, text = "Max", anchor = 'w', justify = 'left')
    en.max.Xlim <- tkentry(frameGraphXlim, textvariable = max.xlim, width = 5, state = stateMaxXlim)

    tkgrid(chk.min.Xlim, row = 0, column = 0, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
    tkgrid(en.min.Xlim, row = 0, column = 1, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
    tkgrid(chk.max.Xlim, row = 0, column = 2, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
    tkgrid(en.max.Xlim, row = 0, column = 3, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)

    #########

    tkbind(chk.min.Xlim, "<Button-1>", function(){
        stateMinXlim <- if(tclvalue(is.min.xlim) == '0') 'normal' else 'disabled'
        tkconfigure(en.min.Xlim, state = stateMinXlim)
    })

    tkbind(chk.max.Xlim, "<Button-1>", function(){
        stateMaxXlim <- if(tclvalue(is.max.xlim) == '0') 'normal' else 'disabled'
        tkconfigure(en.max.Xlim, state = stateMaxXlim)
    })

    ###########

    frameGraphYlim <- ttklabelframe(frameGraphXYlim, text = lang.dlg[['label']][['2']], relief = 'groove')

    is.min.ylim <- tclVar(GraphOpt$cdf$ylim$is.min)
    is.max.ylim <- tclVar(GraphOpt$cdf$ylim$is.max)
    min.ylim <- tclVar(GraphOpt$cdf$ylim$min)
    max.ylim <- tclVar(GraphOpt$cdf$ylim$max)

    stateMinYlim <- if(GraphOpt$cdf$ylim$is.min) 'normal' else 'disabled'
    stateMaxYlim <- if(GraphOpt$cdf$ylim$is.max) 'normal' else 'disabled'

    chk.min.Ylim <- tkcheckbutton(frameGraphYlim, variable = is.min.ylim, text = "Min", anchor = 'w', justify = 'left')
    en.min.Ylim <- tkentry(frameGraphYlim, textvariable = min.ylim, width = 5, state = stateMinYlim)
    chk.max.Ylim <- tkcheckbutton(frameGraphYlim, variable = is.max.ylim, text = "Max", anchor = 'w', justify = 'left')
    en.max.Ylim <- tkentry(frameGraphYlim, textvariable = max.ylim, width = 5, state = stateMaxYlim)

    tkgrid(chk.min.Ylim, row = 0, column = 0, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
    tkgrid(en.min.Ylim, row = 0, column = 1, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
    tkgrid(chk.max.Ylim, row = 0, column = 2, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
    tkgrid(en.max.Ylim, row = 0, column = 3, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)

    #########

    tkbind(chk.min.Ylim, "<Button-1>", function(){
        stateMinYlim <- if(tclvalue(is.min.ylim) == '0') 'normal' else 'disabled'
        tkconfigure(en.min.Ylim, state = stateMinYlim)
    })

    tkbind(chk.max.Ylim, "<Button-1>", function(){
        stateMaxYlim <- if(tclvalue(is.max.ylim) == '0') 'normal' else 'disabled'
        tkconfigure(en.max.Ylim, state = stateMaxYlim)
    })

    #####################

    sepXYlim <- tklabel(frameGraphXYlim, text = "", width = largeur3)

    tkgrid(frameGraphXlim, row = 0, column = 0, sticky = 'w')
    tkgrid(sepXYlim, row = 0, column = 1, sticky = 'we')
    tkgrid(frameGraphYlim, row = 0, column = 2, sticky = 'e')

    #####################

    frameGraphAxLabs <- ttklabelframe(frDialog, text = lang.dlg[['label']][['3']], relief = 'groove')

    is.xaxis.lab <- tclVar(GraphOpt$cdf$axislabs$is.xlab)
    is.yaxis.lab <- tclVar(GraphOpt$cdf$axislabs$is.ylab)
    xaxis.lab <- tclVar(GraphOpt$cdf$axislabs$xlab)
    yaxis.lab <- tclVar(GraphOpt$cdf$axislabs$ylab)

    stateXLab <- if(GraphOpt$cdf$axislabs$is.xlab) 'normal' else 'disabled'
    stateYLab <- if(GraphOpt$cdf$axislabs$is.ylab) 'normal' else 'disabled'

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

    frameGraphTitle <- ttklabelframe(frDialog, text = lang.dlg[['label']][['4']], relief = 'groove')

    is.title <- tclVar(GraphOpt$cdf$title$is.title)
    text.title <- tclVar(GraphOpt$cdf$title$title)

    pos.title <- tclVar()
    CbposTitleVAL <- lang.dlg[['combobox']][['1']]
    posTitleVAL <- c("top", "bottom")
    tclvalue(pos.title) <- CbposTitleVAL[posTitleVAL %in% GraphOpt$cdf$title$position]

    stateGpTlt <- if(GraphOpt$cdf$title$is.title) 'normal' else 'disabled'

    chk.GpTlt <- tkcheckbutton(frameGraphTitle, variable = is.title, anchor = 'e', justify = 'right')
    en.GpTlt <- tkentry(frameGraphTitle, textvariable = text.title, width = largeur2, state = stateGpTlt)
    txt.GpTlt <- tklabel(frameGraphTitle, text = lang.dlg[['label']][['5']], anchor = 'e', justify = 'right')
    cb.GpTlt <- ttkcombobox(frameGraphTitle, values = CbposTitleVAL, textvariable = pos.title, width = 7, state = stateGpTlt)

    tkgrid(chk.GpTlt, row = 0, column = 0, sticky = 'e', rowspan = 1, columnspan = 1)
    tkgrid(en.GpTlt, row = 0, column = 1, sticky = 'we', rowspan = 1, columnspan = 7)
    tkgrid(txt.GpTlt, row = 1, column = 1, sticky = 'e', rowspan = 1, columnspan = 6)
    tkgrid(cb.GpTlt, row = 1, column = 7, sticky = 'we', rowspan = 1, columnspan = 1)

    #########

    tkbind(chk.GpTlt, "<Button-1>", function(){
        stateGpTlt <- if(tclvalue(is.title) == '0') 'normal' else 'disabled'
        tkconfigure(en.GpTlt, state = stateGpTlt)
        tkconfigure(cb.GpTlt, state = stateGpTlt)
    })

    #####################

    frameLegend <- ttklabelframe(frDialog, text = lang.dlg[['label']][['16']], relief = 'groove')

    add.legend <- tclVar(GraphOpt$cdf$legend$add)
    obs.legend <- tclVar(GraphOpt$cdf$legend$obs)
    est.legend <- tclVar(GraphOpt$cdf$legend$est)

    stateLeg <- if(tclvalue(add.legend) == '1') 'normal' else 'disabled'

    chk.Leg <- tkcheckbutton(frameLegend, variable = add.legend, text = lang.dlg[['checkbutton']][['1']], anchor = 'w', justify = 'left')
    txt.Leg1 <- tklabel(frameLegend, text = lang.dlg[['label']][['14']], anchor = 'w', justify = 'left')
    en.Leg1 <- tkentry(frameLegend, textvariable = obs.legend, width = largeur4, state = stateLeg)
    txt.Leg2 <- tklabel(frameLegend, text = lang.dlg[['label']][['15']], anchor = 'w', justify = 'left')
    en.Leg2 <- tkentry(frameLegend, textvariable = est.legend, width = largeur4, state = stateLeg)

    tkgrid(chk.Leg, row = 0, column = 0, sticky = 'we', columnspan = 4)
    tkgrid(txt.Leg1, row = 1, column = 0, sticky = 'w', columnspan = 1)
    tkgrid(en.Leg1, row = 2, column = 0, sticky = 'we', columnspan = 1)
    tkgrid(txt.Leg2, row = 1, column = 1, sticky = 'w', columnspan = 1)
    tkgrid(en.Leg2, row = 2, column = 1, sticky = 'we', columnspan = 1)

    #########

    tkbind(chk.Leg, "<Button-1>", function(){
        stateLeg <- if(tclvalue(add.legend) == '0') 'normal' else 'disabled'
        tkconfigure(en.Leg1, state = stateLeg)
        tkconfigure(en.Leg2, state = stateLeg)
    })

    #####################

    frameGraphObs <- ttklabelframe(frDialog, text = lang.dlg[['label']][['14']], relief = 'groove')

    plot.type1 <- tclVar(GraphOpt$cdf$plot$obs$type)

    txt.pltType1 <- tklabel(frameGraphObs, text = lang.dlg[['label']][['6']], anchor = 'e', justify = 'right')
    cb.pltType1 <- ttkcombobox(frameGraphObs, values = c('both', 'line'), textvariable = plot.type1, width = 4)

    #########
    tkbind(cb.pltType1, "<<ComboboxSelected>>", function(){
        statePlotPoints1 <- if(tclvalue(plot.type1) == 'both') 'normal' else 'disabled'
        tkconfigure(bt.pltPointC1, state = statePlotPoints1)
        tkconfigure(spin.pltPointS1, state = statePlotPoints1)
        tkconfigure(spin.pltPointT1, state = statePlotPoints1)
    })

    ########
    framepltLine1 <- ttklabelframe(frameGraphObs, text = lang.dlg[['label']][['7']], relief = 'groove')

    plot.col.line1 <- tclVar(GraphOpt$cdf$plot$obs$line)

    txt.pltLineC1 <- tklabel(framepltLine1, text = lang.dlg[['label']][['9']], anchor = 'e', justify = 'right')
    bt.pltLineC1 <- tkbutton(framepltLine1, bg = tclvalue(plot.col.line1), width = width.col)
    txt.pltLineW1 <- tklabel(framepltLine1, text = lang.dlg[['label']][['10']], anchor = 'e', justify = 'right')
    spin.pltLineW1 <- ttkspinbox(framepltLine1, from = 0.5, to = 4, increment = 0.1, justify = 'center', width = width.spin)
    tkset(spin.pltLineW1, GraphOpt$cdf$plot$obs$lwd)

    tkgrid(txt.pltLineC1, bt.pltLineC1, txt.pltLineW1, spin.pltLineW1)

    ########
    tkconfigure(bt.pltLineC1, command = function(){
        tcl('wm', 'attributes', tt, topmost = FALSE)
        loko <- trimws(tclvalue(tcl("tk_chooseColor", initialcolor = tclvalue(plot.col.line1), title = lang.dlg[['label']][['13']])))
        tcl('wm', 'attributes', tt, topmost = TRUE)
        if(nchar(loko) > 0){
            tkconfigure(bt.pltLineC1, bg = loko)
            tclvalue(plot.col.line1) <- loko
        }
    })

    ########
    framepltPoints1 <- ttklabelframe(frameGraphObs, text = lang.dlg[['label']][['8']], relief = 'groove')

    plot.col.points1 <- tclVar(GraphOpt$cdf$plot$obs$points)
    statePlotPoints1 <- if(GraphOpt$cdf$plot$obs$type == 'both') 'normal' else 'disabled'

    txt.pltPointC1 <- tklabel(framepltPoints1, text = lang.dlg[['label']][['9']], anchor = 'e', justify = 'right')
    bt.pltPointC1 <- tkbutton(framepltPoints1, bg = tclvalue(plot.col.points1), width = width.col, state = statePlotPoints1)
    txt.pltPointT1 <- tklabel(framepltPoints1, text = lang.dlg[['label']][['11']], anchor = 'e', justify = 'right')
    spin.pltPointT1 <- ttkspinbox(framepltPoints1, from = 21, to = 25, increment = 1, justify = 'center', width = width.spin, state = statePlotPoints1)
    tkset(spin.pltPointT1, GraphOpt$cdf$plot$obs$pch)
    txt.pltPointS1 <- tklabel(framepltPoints1, text = lang.dlg[['label']][['12']], anchor = 'e', justify = 'right')
    spin.pltPointS1 <- ttkspinbox(framepltPoints1, from = 0.5, to = 2.5, increment = 0.1, justify = 'center', width = width.spin, state = statePlotPoints1)
    tkset(spin.pltPointS1, GraphOpt$cdf$plot$obs$cex)

    tkgrid(txt.pltPointC1, bt.pltPointC1, txt.pltPointT1, spin.pltPointT1, txt.pltPointS1, spin.pltPointS1)

    ########

    tkconfigure(bt.pltPointC1, command = function(){
        tcl('wm', 'attributes', tt, topmost = FALSE)
        loko <- trimws(tclvalue(tcl("tk_chooseColor", initialcolor = tclvalue(plot.col.points1), title = lang.dlg[['label']][['13']])))
        tcl('wm', 'attributes', tt, topmost = TRUE)
        if(nchar(loko) > 0){
            tkconfigure(bt.pltPointC1, bg = loko)
            tclvalue(plot.col.points1) <- loko
        }
    })

    ##########

    tkgrid(txt.pltType1, row = 0, column = 0)
    tkgrid(cb.pltType1, row = 0, column = 1)
    tkgrid(framepltLine1, row = 0, column = 2, padx = 2)
    tkgrid(framepltPoints1, row = 0, column = 3)

    #####################

    frameGraphEst <- ttklabelframe(frDialog, text = lang.dlg[['label']][['15']], relief = 'groove')

    plot.type2 <- tclVar(GraphOpt$cdf$plot$est$type)

    txt.pltType2 <- tklabel(frameGraphEst, text = lang.dlg[['label']][['6']], anchor = 'e', justify = 'right')
    cb.pltType2 <- ttkcombobox(frameGraphEst, values = c('both', 'line'), textvariable = plot.type2, width = 4)

    #########
    tkbind(cb.pltType2, "<<ComboboxSelected>>", function(){
        statePlotPoints2 <- if(tclvalue(plot.type2) == 'both') 'normal' else 'disabled'
        tkconfigure(bt.pltPointC2, state = statePlotPoints2)
        tkconfigure(spin.pltPointS2, state = statePlotPoints2)
        tkconfigure(spin.pltPointT2, state = statePlotPoints2)
    })

    ########
    framepltLine2 <- ttklabelframe(frameGraphEst, text = lang.dlg[['label']][['7']], relief = 'groove')

    plot.col.line2 <- tclVar(GraphOpt$cdf$plot$est$line)

    txt.pltLineC2 <- tklabel(framepltLine2, text = lang.dlg[['label']][['9']], anchor = 'e', justify = 'right')
    bt.pltLineC2 <- tkbutton(framepltLine2, bg = tclvalue(plot.col.line2), width = width.col)
    txt.pltLineW2 <- tklabel(framepltLine2, text = lang.dlg[['label']][['10']], anchor = 'e', justify = 'right')
    spin.pltLineW2 <- ttkspinbox(framepltLine2, from = 0.5, to = 4, increment = 0.1, justify = 'center', width = width.spin)
    tkset(spin.pltLineW2, GraphOpt$cdf$plot$est$lwd)

    tkgrid(txt.pltLineC2, bt.pltLineC2, txt.pltLineW2, spin.pltLineW2)

    ########
    tkconfigure(bt.pltLineC2, command = function(){
        tcl('wm', 'attributes', tt, topmost = FALSE)
        loko <- trimws(tclvalue(tcl("tk_chooseColor", initialcolor = tclvalue(plot.col.line2), title = lang.dlg[['label']][['13']])))
        tcl('wm', 'attributes', tt, topmost = TRUE)
        if(nchar(loko) > 0){
            tkconfigure(bt.pltLineC2, bg = loko)
            tclvalue(plot.col.line2) <- loko
        }
    })

    ########
    framepltPoints2 <- ttklabelframe(frameGraphEst, text = lang.dlg[['label']][['8']], relief = 'groove')

    plot.col.points2 <- tclVar(GraphOpt$cdf$plot$est$points)
    statePlotPoints2 <- if(GraphOpt$cdf$plot$est$type == 'both') 'normal' else 'disabled'

    txt.pltPointC2 <- tklabel(framepltPoints2, text = lang.dlg[['label']][['9']], anchor = 'e', justify = 'right')
    bt.pltPointC2 <- tkbutton(framepltPoints2, bg = tclvalue(plot.col.points2), width = width.col, state = statePlotPoints2)
    txt.pltPointT2 <- tklabel(framepltPoints2, text = lang.dlg[['label']][['11']], anchor = 'e', justify = 'right')
    spin.pltPointT2 <- ttkspinbox(framepltPoints2, from = 21, to = 25, increment = 1, justify = 'center', width = width.spin, state = statePlotPoints2)
    tkset(spin.pltPointT2, GraphOpt$cdf$plot$est$pch)
    txt.pltPointS2 <- tklabel(framepltPoints2, text = lang.dlg[['label']][['12']], anchor = 'e', justify = 'right')
    spin.pltPointS2 <- ttkspinbox(framepltPoints2, from = 0.5, to = 2.5, increment = 0.1, justify = 'center', width = width.spin, state = statePlotPoints2)
    tkset(spin.pltPointS2, GraphOpt$cdf$plot$est$cex)

    tkgrid(txt.pltPointC2, bt.pltPointC2, txt.pltPointT2, spin.pltPointT2, txt.pltPointS2, spin.pltPointS2)

    ########

    tkconfigure(bt.pltPointC2, command = function(){
        tcl('wm', 'attributes', tt, topmost = FALSE)
        loko <- trimws(tclvalue(tcl("tk_chooseColor", initialcolor = tclvalue(plot.col.points2), title = lang.dlg[['label']][['13']])))
        tcl('wm', 'attributes', tt, topmost = TRUE)
        if(nchar(loko) > 0){
            tkconfigure(bt.pltPointC2, bg = loko)
            tclvalue(plot.col.points2) <- loko
        }
    })

    ###########

    tkgrid(txt.pltType2, row = 0, column = 0)
    tkgrid(cb.pltType2, row = 0, column = 1)
    tkgrid(framepltLine2, row = 0, column = 2, padx = 2)
    tkgrid(framepltPoints2, row = 0, column = 3)

    #####################

    tkgrid(frameGraphXYlim, row = 0, column = 0, sticky = '', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
    tkgrid(frameGraphAxLabs, row = 1, column = 0, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 2, ipadx = 1, ipady = 1)
    tkgrid(frameGraphTitle, row = 2, column = 0, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 2, ipadx = 1, ipady = 1)
    tkgrid(frameLegend, row = 3, column = 0, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 2, ipadx = 1, ipady = 1)
    tkgrid(frameGraphObs, row = 4, column = 0, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 2, ipadx = 1, ipady = 1)
    tkgrid(frameGraphEst, row = 5, column = 0, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 2, ipadx = 1, ipady = 1)

    #####################
    bt.opt.OK <- ttkbutton(frButt, text = .cdtEnv$tcl$lang$global[['button']][['1']])
    bt.opt.CA <- ttkbutton(frButt, text = .cdtEnv$tcl$lang$global[['button']][['2']])

    tkconfigure(bt.opt.OK, command = function(){
        GraphOpt$cdf$xlim$is.min <<- switch(tclvalue(is.min.xlim), '0' = FALSE, '1' = TRUE)
        GraphOpt$cdf$xlim$is.max <<- switch(tclvalue(is.max.xlim), '0' = FALSE, '1' = TRUE)
        GraphOpt$cdf$xlim$min <<- as.numeric(trimws(tclvalue(min.xlim)))
        GraphOpt$cdf$xlim$max <<- as.numeric(trimws(tclvalue(max.xlim)))

        GraphOpt$cdf$ylim$is.min <<- switch(tclvalue(is.min.ylim), '0' = FALSE, '1' = TRUE)
        GraphOpt$cdf$ylim$is.max <<- switch(tclvalue(is.max.ylim), '0' = FALSE, '1' = TRUE)
        GraphOpt$cdf$ylim$min <<- as.numeric(trimws(tclvalue(min.ylim)))
        GraphOpt$cdf$ylim$max <<- as.numeric(trimws(tclvalue(max.ylim)))

        GraphOpt$cdf$axislabs$is.xlab <<- switch(tclvalue(is.xaxis.lab), '0' = FALSE, '1' = TRUE)
        GraphOpt$cdf$axislabs$is.ylab <<- switch(tclvalue(is.yaxis.lab), '0' = FALSE, '1' = TRUE)
        GraphOpt$cdf$axislabs$xlab <<- gsub('\\\\n', '\n', trimws(tclvalue(xaxis.lab)))
        GraphOpt$cdf$axislabs$ylab <<- gsub('\\\\n', '\n', trimws(tclvalue(yaxis.lab)))

        GraphOpt$cdf$title$is.title <<- switch(tclvalue(is.title), '0' = FALSE, '1' = TRUE)
        GraphOpt$cdf$title$title <<- gsub('\\\\n', '\n', trimws(tclvalue(text.title)))
        GraphOpt$cdf$title$position <<- posTitleVAL[CbposTitleVAL %in% trimws(tclvalue(pos.title))]

        GraphOpt$cdf$plot$obs$type <<- trimws(tclvalue(plot.type1))
        GraphOpt$cdf$plot$obs$line <<- tclvalue(plot.col.line1)
        GraphOpt$cdf$plot$obs$lwd <<- as.numeric(trimws(tclvalue(tkget(spin.pltLineW1))))
        GraphOpt$cdf$plot$obs$points <<- tclvalue(plot.col.points1)
        GraphOpt$cdf$plot$obs$cex <<- as.numeric(trimws(tclvalue(tkget(spin.pltPointS1))))
        GraphOpt$cdf$plot$obs$pch <<- as.numeric(trimws(tclvalue(tkget(spin.pltPointT1))))

        GraphOpt$cdf$plot$est$type <<- trimws(tclvalue(plot.type2))
        GraphOpt$cdf$plot$est$line <<- tclvalue(plot.col.line2)
        GraphOpt$cdf$plot$est$lwd <<- as.numeric(trimws(tclvalue(tkget(spin.pltLineW2))))
        GraphOpt$cdf$plot$est$points <<- tclvalue(plot.col.points2)
        GraphOpt$cdf$plot$est$cex <<- as.numeric(trimws(tclvalue(tkget(spin.pltPointS2))))
        GraphOpt$cdf$plot$est$pch <<- as.numeric(trimws(tclvalue(tkget(spin.pltPointT2))))

        GraphOpt$cdf$legend$add <<- switch(tclvalue(add.legend), '0' = FALSE, '1' = TRUE)
        GraphOpt$cdf$legend$obs <<- trimws(tclvalue(obs.legend))
        GraphOpt$cdf$legend$est <<- trimws(tclvalue(est.legend))

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

#######################################################################################################

Validation.GraphOptions.Lines <- function(GraphOpt, parent.win = .cdtEnv$tcl$main$win){
    if(WindowsOS()){
        largeur1 <- 63
        largeur2 <- 67
        largeur3 <- 1
        largeur4 <- 35
        width.col <- 3
        width.spin <- 4
    }else{
        largeur1 <- 60
        largeur2 <- 64
        largeur4 <- 33
        largeur3 <- 1
        width.col <- 1
        width.spin <- 4
    }

    #####################
    xml.dlg <- file.path(.cdtDir$dirLocal, "languages", "cdtGraphOptions.Lines_Valid_dlgBox.xml")
    lang.dlg <- cdtLanguageParse(xml.dlg, .cdtData$Config$lang.iso)

    #####################
    tt <- tktoplevel()
    tkgrab.set(tt)
    tkfocus(tt)

    #####################
    frDialog <- tkframe(tt, relief = 'raised', borderwidth = 2)
    frButt <- tkframe(tt)

    #####################

    frameGraphXYlim <- tkframe(frDialog)

    ########

    frameGraphXlim <- ttklabelframe(frameGraphXYlim, text = lang.dlg[['label']][['1']], relief = 'groove')

    is.min.xlim <- tclVar(GraphOpt$line$xlim$is.min)
    is.max.xlim <- tclVar(GraphOpt$line$xlim$is.max)
    min.xlim <- tclVar(GraphOpt$line$xlim$min)
    max.xlim <- tclVar(GraphOpt$line$xlim$max)

    stateMinXlim <- if(GraphOpt$line$xlim$is.min) 'normal' else 'disabled'
    stateMaxXlim <- if(GraphOpt$line$xlim$is.max) 'normal' else 'disabled'

    chk.min.Xlim <- tkcheckbutton(frameGraphXlim, variable = is.min.xlim, text = "Min", anchor = 'w', justify = 'left')
    en.min.Xlim <- tkentry(frameGraphXlim, textvariable = min.xlim, width = 11, state = stateMinXlim)
    chk.max.Xlim <- tkcheckbutton(frameGraphXlim, variable = is.max.xlim, text = "Max", anchor = 'w', justify = 'left')
    en.max.Xlim <- tkentry(frameGraphXlim, textvariable = max.xlim, width = 11, state = stateMaxXlim)

    tkgrid(chk.min.Xlim, row = 0, column = 0, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
    tkgrid(en.min.Xlim, row = 0, column = 1, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
    tkgrid(chk.max.Xlim, row = 0, column = 2, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
    tkgrid(en.max.Xlim, row = 0, column = 3, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)

    helpWidget(en.min.Xlim, lang.dlg[['tooltip']][['1']], lang.dlg[['status']][['1']])
    helpWidget(en.max.Xlim, lang.dlg[['tooltip']][['1']], lang.dlg[['status']][['1']])

    #########

    tkbind(chk.min.Xlim, "<Button-1>", function(){
        stateMinXlim <- if(tclvalue(is.min.xlim) == '0') 'normal' else 'disabled'
        tkconfigure(en.min.Xlim, state = stateMinXlim)
    })

    tkbind(chk.max.Xlim, "<Button-1>", function(){
        stateMaxXlim <- if(tclvalue(is.max.xlim) == '0') 'normal' else 'disabled'
        tkconfigure(en.max.Xlim, state = stateMaxXlim)
    })

    #########

    frameGraphYlim <- ttklabelframe(frameGraphXYlim, text = lang.dlg[['label']][['2']], relief = 'groove')

    is.min.ylim <- tclVar(GraphOpt$line$ylim$is.min)
    is.max.ylim <- tclVar(GraphOpt$line$ylim$is.max)
    min.ylim <- tclVar(GraphOpt$line$ylim$min)
    max.ylim <- tclVar(GraphOpt$line$ylim$max)

    stateMinYlim <- if(GraphOpt$line$ylim$is.min) 'normal' else 'disabled'
    stateMaxYlim <- if(GraphOpt$line$ylim$is.max) 'normal' else 'disabled'

    chk.min.Ylim <- tkcheckbutton(frameGraphYlim, variable = is.min.ylim, text = "Min", anchor = 'w', justify = 'left')
    en.min.Ylim <- tkentry(frameGraphYlim, textvariable = min.ylim, width = 5, state = stateMinYlim)
    chk.max.Ylim <- tkcheckbutton(frameGraphYlim, variable = is.max.ylim, text = "Max", anchor = 'w', justify = 'left')
    en.max.Ylim <- tkentry(frameGraphYlim, textvariable = max.ylim, width = 5, state = stateMaxYlim)

    tkgrid(chk.min.Ylim, row = 0, column = 0, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
    tkgrid(en.min.Ylim, row = 0, column = 1, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
    tkgrid(chk.max.Ylim, row = 0, column = 2, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
    tkgrid(en.max.Ylim, row = 0, column = 3, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)

    #########

    tkbind(chk.min.Ylim, "<Button-1>", function(){
        stateMinYlim <- if(tclvalue(is.min.ylim) == '0') 'normal' else 'disabled'
        tkconfigure(en.min.Ylim, state = stateMinYlim)
    })

    tkbind(chk.max.Ylim, "<Button-1>", function(){
        stateMaxYlim <- if(tclvalue(is.max.ylim) == '0') 'normal' else 'disabled'
        tkconfigure(en.max.Ylim, state = stateMaxYlim)
    })

    #####################

    sepXYlim <- tklabel(frameGraphXYlim, text = "", width = largeur3)

    tkgrid(frameGraphXlim, row = 0, column = 0, sticky = 'w')
    tkgrid(sepXYlim, row = 0, column = 1, sticky = 'we')
    tkgrid(frameGraphYlim, row = 0, column = 2, sticky = 'e')

    #####################

    frameGraphAxLabs <- ttklabelframe(frDialog, text = lang.dlg[['label']][['3']], relief = 'groove')

    is.xaxis.lab <- tclVar(GraphOpt$line$axislabs$is.xlab)
    is.yaxis.lab <- tclVar(GraphOpt$line$axislabs$is.ylab)
    xaxis.lab <- tclVar(GraphOpt$line$axislabs$xlab)
    yaxis.lab <- tclVar(GraphOpt$line$axislabs$ylab)

    stateXLab <- if(GraphOpt$line$axislabs$is.xlab) 'normal' else 'disabled'
    stateYLab <- if(GraphOpt$line$axislabs$is.ylab) 'normal' else 'disabled'

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

    frameGraphTitle <- ttklabelframe(frDialog, text = lang.dlg[['label']][['4']], relief = 'groove')

    is.title <- tclVar(GraphOpt$line$title$is.title)
    text.title <- tclVar(GraphOpt$line$title$title)

    pos.title <- tclVar()
    CbposTitleVAL <- lang.dlg[['combobox']][['1']]
    posTitleVAL <- c("top", "bottom")
    tclvalue(pos.title) <- CbposTitleVAL[posTitleVAL %in% GraphOpt$line$title$position]

    stateGpTlt <- if(GraphOpt$line$title$is.title) 'normal' else 'disabled'

    chk.GpTlt <- tkcheckbutton(frameGraphTitle, variable = is.title, anchor = 'e', justify = 'right')
    en.GpTlt <- tkentry(frameGraphTitle, textvariable = text.title, width = largeur2, state = stateGpTlt)
    txt.GpTlt <- tklabel(frameGraphTitle, text = lang.dlg[['label']][['5']], anchor = 'e', justify = 'right')
    cb.GpTlt <- ttkcombobox(frameGraphTitle, values = CbposTitleVAL, textvariable = pos.title, width = 7, state = stateGpTlt)

    tkgrid(chk.GpTlt, row = 0, column = 0, sticky = 'e', rowspan = 1, columnspan = 1)
    tkgrid(en.GpTlt, row = 0, column = 1, sticky = 'we', rowspan = 1, columnspan = 7)
    tkgrid(txt.GpTlt, row = 1, column = 1, sticky = 'e', rowspan = 1, columnspan = 6)
    tkgrid(cb.GpTlt, row = 1, column = 7, sticky = 'we', rowspan = 1, columnspan = 1)

    #########

    tkbind(chk.GpTlt, "<Button-1>", function(){
        stateGpTlt <- if(tclvalue(is.title) == '0') 'normal' else 'disabled'
        tkconfigure(en.GpTlt, state = stateGpTlt)
        tkconfigure(cb.GpTlt, state = stateGpTlt)
    })

    #####################

    frameLegend <- ttklabelframe(frDialog, text = lang.dlg[['label']][['16']], relief = 'groove')

    add.legend <- tclVar(GraphOpt$line$legend$add)
    obs.legend <- tclVar(GraphOpt$line$legend$obs)
    est.legend <- tclVar(GraphOpt$line$legend$est)

    stateLeg <- if(tclvalue(add.legend) == '1') 'normal' else 'disabled'

    chk.Leg <- tkcheckbutton(frameLegend, variable = add.legend, text = lang.dlg[['checkbutton']][['1']], anchor = 'w', justify = 'left')
    txt.Leg1 <- tklabel(frameLegend, text = lang.dlg[['label']][['14']], anchor = 'w', justify = 'left')
    en.Leg1 <- tkentry(frameLegend, textvariable = obs.legend, width = largeur4, state = stateLeg)
    txt.Leg2 <- tklabel(frameLegend, text = lang.dlg[['label']][['15']], anchor = 'w', justify = 'left')
    en.Leg2 <- tkentry(frameLegend, textvariable = est.legend, width = largeur4, state = stateLeg)

    tkgrid(chk.Leg, row = 0, column = 0, sticky = 'we', columnspan = 4)
    tkgrid(txt.Leg1, row = 1, column = 0, sticky = 'w', columnspan = 1)
    tkgrid(en.Leg1, row = 2, column = 0, sticky = 'we', columnspan = 1)
    tkgrid(txt.Leg2, row = 1, column = 1, sticky = 'w', columnspan = 1)
    tkgrid(en.Leg2, row = 2, column = 1, sticky = 'we', columnspan = 1)

    #########

    tkbind(chk.Leg, "<Button-1>", function(){
        stateLeg <- if(tclvalue(add.legend) == '0') 'normal' else 'disabled'
        tkconfigure(en.Leg1, state = stateLeg)
        tkconfigure(en.Leg2, state = stateLeg)
    })

    #####################

    frameGraphObs <- ttklabelframe(frDialog, text = lang.dlg[['label']][['14']], relief = 'groove')

    plot.type1 <- tclVar(GraphOpt$line$plot$obs$type)

    txt.pltType1 <- tklabel(frameGraphObs, text = lang.dlg[['label']][['6']], anchor = 'e', justify = 'right')
    cb.pltType1 <- ttkcombobox(frameGraphObs, values = c('both', 'line'), textvariable = plot.type1, width = 4)

    #########
    tkbind(cb.pltType1, "<<ComboboxSelected>>", function(){
        statePlotPoints1 <- if(tclvalue(plot.type1) == 'both') 'normal' else 'disabled'
        tkconfigure(bt.pltPointC1, state = statePlotPoints1)
        tkconfigure(spin.pltPointS1, state = statePlotPoints1)
        tkconfigure(spin.pltPointT1, state = statePlotPoints1)
    })

    ########
    framepltLine1 <- ttklabelframe(frameGraphObs, text = lang.dlg[['label']][['7']], relief = 'groove')

    plot.col.line1 <- tclVar(GraphOpt$line$plot$obs$line)

    txt.pltLineC1 <- tklabel(framepltLine1, text = lang.dlg[['label']][['9']], anchor = 'e', justify = 'right')
    bt.pltLineC1 <- tkbutton(framepltLine1, bg = tclvalue(plot.col.line1), width = width.col)
    txt.pltLineW1 <- tklabel(framepltLine1, text = lang.dlg[['label']][['10']], anchor = 'e', justify = 'right')
    spin.pltLineW1 <- ttkspinbox(framepltLine1, from = 0.5, to = 4, increment = 0.1, justify = 'center', width = width.spin)
    tkset(spin.pltLineW1, GraphOpt$line$plot$obs$lwd)

    tkgrid(txt.pltLineC1, bt.pltLineC1, txt.pltLineW1, spin.pltLineW1)

    ########
    tkconfigure(bt.pltLineC1, command = function(){
        tcl('wm', 'attributes', tt, topmost = FALSE)
        loko <- trimws(tclvalue(tcl("tk_chooseColor", initialcolor = tclvalue(plot.col.line1), title = lang.dlg[['label']][['13']])))
        tcl('wm', 'attributes', tt, topmost = TRUE)
        if(nchar(loko) > 0){
            tkconfigure(bt.pltLineC1, bg = loko)
            tclvalue(plot.col.line1) <- loko
        }
    })

    ########
    framepltPoints1 <- ttklabelframe(frameGraphObs, text = lang.dlg[['label']][['8']], relief = 'groove')

    plot.col.points1 <- tclVar(GraphOpt$line$plot$obs$points)
    statePlotPoints1 <- if(GraphOpt$line$plot$obs$type == 'both') 'normal' else 'disabled'

    txt.pltPointC1 <- tklabel(framepltPoints1, text = lang.dlg[['label']][['9']], anchor = 'e', justify = 'right')
    bt.pltPointC1 <- tkbutton(framepltPoints1, bg = tclvalue(plot.col.points1), width = width.col, state = statePlotPoints1)
    txt.pltPointT1 <- tklabel(framepltPoints1, text = lang.dlg[['label']][['11']], anchor = 'e', justify = 'right')
    spin.pltPointT1 <- ttkspinbox(framepltPoints1, from = 21, to = 25, increment = 1, justify = 'center', width = width.spin, state = statePlotPoints1)
    tkset(spin.pltPointT1, GraphOpt$line$plot$obs$pch)
    txt.pltPointS1 <- tklabel(framepltPoints1, text = lang.dlg[['label']][['12']], anchor = 'e', justify = 'right')
    spin.pltPointS1 <- ttkspinbox(framepltPoints1, from = 0.5, to = 2.5, increment = 0.1, justify = 'center', width = width.spin, state = statePlotPoints1)
    tkset(spin.pltPointS1, GraphOpt$line$plot$obs$cex)

    tkgrid(txt.pltPointC1, bt.pltPointC1, txt.pltPointT1, spin.pltPointT1, txt.pltPointS1, spin.pltPointS1)

    ########

    tkconfigure(bt.pltPointC1, command = function(){
        tcl('wm', 'attributes', tt, topmost = FALSE)
        loko <- trimws(tclvalue(tcl("tk_chooseColor", initialcolor = tclvalue(plot.col.points1), title = lang.dlg[['label']][['13']])))
        tcl('wm', 'attributes', tt, topmost = TRUE)
        if(nchar(loko) > 0){
            tkconfigure(bt.pltPointC1, bg = loko)
            tclvalue(plot.col.points1) <- loko
        }
    })

    ##########

    tkgrid(txt.pltType1, row = 0, column = 0)
    tkgrid(cb.pltType1, row = 0, column = 1)
    tkgrid(framepltLine1, row = 0, column = 2, padx = 2)
    tkgrid(framepltPoints1, row = 0, column = 3)

    #####################

    frameGraphEst <- ttklabelframe(frDialog, text = lang.dlg[['label']][['15']], relief = 'groove')

    plot.type2 <- tclVar(GraphOpt$line$plot$est$type)

    txt.pltType2 <- tklabel(frameGraphEst, text = lang.dlg[['label']][['6']], anchor = 'e', justify = 'right')
    cb.pltType2 <- ttkcombobox(frameGraphEst, values = c('both', 'line'), textvariable = plot.type2, width = 4)

    #########
    tkbind(cb.pltType2, "<<ComboboxSelected>>", function(){
        statePlotPoints2 <- if(tclvalue(plot.type2) == 'both') 'normal' else 'disabled'
        tkconfigure(bt.pltPointC2, state = statePlotPoints2)
        tkconfigure(spin.pltPointS2, state = statePlotPoints2)
        tkconfigure(spin.pltPointT2, state = statePlotPoints2)
    })

    ########
    framepltLine2 <- ttklabelframe(frameGraphEst, text = lang.dlg[['label']][['7']], relief = 'groove')

    plot.col.line2 <- tclVar(GraphOpt$line$plot$est$line)

    txt.pltLineC2 <- tklabel(framepltLine2, text = lang.dlg[['label']][['9']], anchor = 'e', justify = 'right')
    bt.pltLineC2 <- tkbutton(framepltLine2, bg = tclvalue(plot.col.line2), width = width.col)
    txt.pltLineW2 <- tklabel(framepltLine2, text = lang.dlg[['label']][['10']], anchor = 'e', justify = 'right')
    spin.pltLineW2 <- ttkspinbox(framepltLine2, from = 0.5, to = 4, increment = 0.1, justify = 'center', width = width.spin)
    tkset(spin.pltLineW2, GraphOpt$line$plot$est$lwd)

    tkgrid(txt.pltLineC2, bt.pltLineC2, txt.pltLineW2, spin.pltLineW2)

    ########
    tkconfigure(bt.pltLineC2, command = function(){
        tcl('wm', 'attributes', tt, topmost = FALSE)
        loko <- trimws(tclvalue(tcl("tk_chooseColor", initialcolor = tclvalue(plot.col.line2), title = lang.dlg[['label']][['13']])))
        tcl('wm', 'attributes', tt, topmost = TRUE)
        if(nchar(loko) > 0){
            tkconfigure(bt.pltLineC2, bg = loko)
            tclvalue(plot.col.line2) <- loko
        }
    })

    ########
    framepltPoints2 <- ttklabelframe(frameGraphEst, text = lang.dlg[['label']][['8']], relief = 'groove')

    plot.col.points2 <- tclVar(GraphOpt$line$plot$est$points)
    statePlotPoints2 <- if(GraphOpt$line$plot$est$type == 'both') 'normal' else 'disabled'

    txt.pltPointC2 <- tklabel(framepltPoints2, text = lang.dlg[['label']][['9']], anchor = 'e', justify = 'right')
    bt.pltPointC2 <- tkbutton(framepltPoints2, bg = tclvalue(plot.col.points2), width = width.col, state = statePlotPoints2)
    txt.pltPointT2 <- tklabel(framepltPoints2, text = lang.dlg[['label']][['11']], anchor = 'e', justify = 'right')
    spin.pltPointT2 <- ttkspinbox(framepltPoints2, from = 21, to = 25, increment = 1, justify = 'center', width = width.spin, state = statePlotPoints2)
    tkset(spin.pltPointT2, GraphOpt$line$plot$est$pch)
    txt.pltPointS2 <- tklabel(framepltPoints2, text = lang.dlg[['label']][['12']], anchor = 'e', justify = 'right')
    spin.pltPointS2 <- ttkspinbox(framepltPoints2, from = 0.5, to = 2.5, increment = 0.1, justify = 'center', width = width.spin, state = statePlotPoints2)
    tkset(spin.pltPointS2, GraphOpt$line$plot$est$cex)

    tkgrid(txt.pltPointC2, bt.pltPointC2, txt.pltPointT2, spin.pltPointT2, txt.pltPointS2, spin.pltPointS2)

    ########

    tkconfigure(bt.pltPointC2, command = function(){
        tcl('wm', 'attributes', tt, topmost = FALSE)
        loko <- trimws(tclvalue(tcl("tk_chooseColor", initialcolor = tclvalue(plot.col.points2), title = lang.dlg[['label']][['13']])))
        tcl('wm', 'attributes', tt, topmost = TRUE)
        if(nchar(loko) > 0){
            tkconfigure(bt.pltPointC2, bg = loko)
            tclvalue(plot.col.points2) <- loko
        }
    })

    ###########

    tkgrid(txt.pltType2, row = 0, column = 0)
    tkgrid(cb.pltType2, row = 0, column = 1)
    tkgrid(framepltLine2, row = 0, column = 2, padx = 2)
    tkgrid(framepltPoints2, row = 0, column = 3)

    #####################

    tkgrid(frameGraphXYlim, row = 0, column = 0, sticky = '', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
    tkgrid(frameGraphAxLabs, row = 1, column = 0, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 2, ipadx = 1, ipady = 1)
    tkgrid(frameGraphTitle, row = 2, column = 0, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 2, ipadx = 1, ipady = 1)
    tkgrid(frameLegend, row = 3, column = 0, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 2, ipadx = 1, ipady = 1)
    tkgrid(frameGraphObs, row = 4, column = 0, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 2, ipadx = 1, ipady = 1)
    tkgrid(frameGraphEst, row = 5, column = 0, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 2, ipadx = 1, ipady = 1)

    #####################
    bt.opt.OK <- ttkbutton(frButt, text = .cdtEnv$tcl$lang$global[['button']][['1']])
    bt.opt.CA <- ttkbutton(frButt, text = .cdtEnv$tcl$lang$global[['button']][['2']])

    tkconfigure(bt.opt.OK, command = function(){
        GraphOpt$line$xlim$is.min <<- switch(tclvalue(is.min.xlim), '0' = FALSE, '1' = TRUE)
        GraphOpt$line$xlim$is.max <<- switch(tclvalue(is.max.xlim), '0' = FALSE, '1' = TRUE)
        GraphOpt$line$xlim$min <<- trimws(tclvalue(min.xlim))
        GraphOpt$line$xlim$max <<- trimws(tclvalue(max.xlim))

        GraphOpt$line$ylim$is.min <<- switch(tclvalue(is.min.ylim), '0' = FALSE, '1' = TRUE)
        GraphOpt$line$ylim$is.max <<- switch(tclvalue(is.max.ylim), '0' = FALSE, '1' = TRUE)
        GraphOpt$line$ylim$min <<- as.numeric(trimws(tclvalue(min.ylim)))
        GraphOpt$line$ylim$max <<- as.numeric(trimws(tclvalue(max.ylim)))

        GraphOpt$line$axislabs$is.xlab <<- switch(tclvalue(is.xaxis.lab), '0' = FALSE, '1' = TRUE)
        GraphOpt$line$axislabs$is.ylab <<- switch(tclvalue(is.yaxis.lab), '0' = FALSE, '1' = TRUE)
        GraphOpt$line$axislabs$xlab <<- gsub('\\\\n', '\n', trimws(tclvalue(xaxis.lab)))
        GraphOpt$line$axislabs$ylab <<- gsub('\\\\n', '\n', trimws(tclvalue(yaxis.lab)))

        GraphOpt$line$title$is.title <<- switch(tclvalue(is.title), '0' = FALSE, '1' = TRUE)
        GraphOpt$line$title$title <<- gsub('\\\\n', '\n', trimws(tclvalue(text.title)))
        GraphOpt$line$title$position <<- posTitleVAL[CbposTitleVAL %in% trimws(tclvalue(pos.title))]

        GraphOpt$line$plot$obs$type <<- trimws(tclvalue(plot.type1))
        GraphOpt$line$plot$obs$line <<- tclvalue(plot.col.line1)
        GraphOpt$line$plot$obs$lwd <<- as.numeric(trimws(tclvalue(tkget(spin.pltLineW1))))
        GraphOpt$line$plot$obs$points <<- tclvalue(plot.col.points1)
        GraphOpt$line$plot$obs$cex <<- as.numeric(trimws(tclvalue(tkget(spin.pltPointS1))))
        GraphOpt$line$plot$obs$pch <<- as.numeric(trimws(tclvalue(tkget(spin.pltPointT1))))

        GraphOpt$line$plot$est$type <<- trimws(tclvalue(plot.type2))
        GraphOpt$line$plot$est$line <<- tclvalue(plot.col.line2)
        GraphOpt$line$plot$est$lwd <<- as.numeric(trimws(tclvalue(tkget(spin.pltLineW2))))
        GraphOpt$line$plot$est$points <<- tclvalue(plot.col.points2)
        GraphOpt$line$plot$est$cex <<- as.numeric(trimws(tclvalue(tkget(spin.pltPointS2))))
        GraphOpt$line$plot$est$pch <<- as.numeric(trimws(tclvalue(tkget(spin.pltPointT2))))

        GraphOpt$line$legend$add <<- switch(tclvalue(add.legend), '0' = FALSE, '1' = TRUE)
        GraphOpt$line$legend$obs <<- trimws(tclvalue(obs.legend))
        GraphOpt$line$legend$est <<- trimws(tclvalue(est.legend))

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

#######################################################################################################

Validation.GraphOptions.Rank <- function(GraphOpt, description, parent.win = .cdtEnv$tcl$main$win){
    if(WindowsOS()){
        largeur0 <- 390
        largeur1 <- 4
        largeur2 <- 52
        largeur3 <- 13
        largeur4 <- 55
        width.col <- 3
    }else{
        largeur0 <- 426
        largeur1 <- 4
        largeur2 <- 50
        largeur3 <- 12
        largeur4 <- 53
        width.col <- 1
    }

    #####################

    xml.dlg <- file.path(.cdtDir$dirLocal, "languages", "cdtGraphOptions.Rank_Valid_dlgBox.xml")
    lang.dlg <- cdtLanguageParse(xml.dlg, .cdtData$Config$lang.iso)

    #####################

    default.Kol <- rev(RColorBrewer::brewer.pal(9, "Blues"))

    preview.canvasf <- function(kolKey, cond){
        if(cond){
            kolor <- getGradientColor(kolKey, 0:largeur0)
            tkdelete(canvas.preview, 'gradlines0')
            for(i in 0:largeur0)
                tkcreate(canvas.preview, "line", i, 0, i, 20, fill = kolor[i], tags = 'gradlines0')
        }else{
            tkdelete(canvas.preview, 'gradlines0')
        }
    }

    #####################

    tt <- tktoplevel()
    tkgrab.set(tt)
    tkfocus(tt)

    #####################
    frDialog <- tkframe(tt, relief = 'raised', borderwidth = 2)
    frButt <- tkframe(tt)

    #####################

    frameColkey <- ttklabelframe(frDialog, text = lang.dlg[['label']][['1']], relief = 'groove')

    custom.color <- tclVar(GraphOpt$col$custom)

    stateKol2 <- if(GraphOpt$col$custom) "normal" else "disabled"

    chk.userKol <- tkcheckbutton(frameColkey, variable = custom.color, text = lang.dlg[['checkbutton']][['1']], anchor = 'w', justify = 'left')
    bt.userKol <- ttkbutton(frameColkey, text = lang.dlg[['button']][['1']], state = stateKol2)
    canvas.preview <- tkcanvas(frameColkey, width = largeur0, height = 20, bg = 'white')

    ## Preview Color
    kolKey <- if(GraphOpt$col$custom) GraphOpt$col$fill else default.Kol
    preview.canvasf(kolKey, !is.null(kolKey) & length(kolKey) > 0)

    tkgrid(chk.userKol, row = 0, column = 0, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
    tkgrid(bt.userKol, row = 0, column = 1, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
    tkgrid(canvas.preview, row = 1, column = 0, sticky = 'we', rowspan = 1, columnspan = 2, padx = 1, pady = 1, ipadx = 1, ipady = 1)

    #########

    tkconfigure(bt.userKol, command = function(){
        tcl('wm', 'attributes', tt, topmost = FALSE)
        GraphOpt$col$fill <<- createColorkey(tt, GraphOpt$col$fill)
        tcl('wm', 'attributes', tt, topmost = TRUE)
        preview.canvasf(GraphOpt$col$fill, !is.null(GraphOpt$col$fill))
    })

    tkbind(chk.userKol, "<Button-1>", function(){
        stateKol2 <- if(tclvalue(custom.color) == '0') 'normal' else 'disabled'
        tkconfigure(bt.userKol, state = stateKol2)

        kolKey <- if(tclvalue(custom.color) == '0') GraphOpt$col$fill else default.Kol
        preview.canvasf(kolKey, !is.null(kolKey))
    })

    #####################

    frameTxtCol <- ttklabelframe(frDialog, text = lang.dlg[['label']][['2']], relief = 'groove')

    text1.Kol <- tclVar(GraphOpt$col$text[1])
    text2.Kol <- tclVar(GraphOpt$col$text[2])
    text3.Kol <- tclVar(GraphOpt$col$text[3])

    txt.TxtC1 <- tklabel(frameTxtCol, text = lang.dlg[['label']][['3']], anchor = 'e', justify = 'right')
    bt.TxtC1 <- tkbutton(frameTxtCol, bg = tclvalue(text1.Kol), width = width.col)
    txt.TxtC2 <- tklabel(frameTxtCol, text = lang.dlg[['label']][['4']], anchor = 'e', justify = 'right')
    bt.TxtC2 <- tkbutton(frameTxtCol, bg = tclvalue(text2.Kol), width = width.col)
    txt.TxtC3 <- tklabel(frameTxtCol, text = lang.dlg[['label']][['5']], anchor = 'e', justify = 'right')
    bt.TxtC3 <- tkbutton(frameTxtCol, bg = tclvalue(text3.Kol), width = width.col)

    txt.sep1 <- tklabel(frameTxtCol, text = "", width = largeur1)
    txt.sep2 <- tklabel(frameTxtCol, text = "", width = largeur1)

    tkgrid(bt.TxtC1, row = 0, column = 0, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
    tkgrid(txt.TxtC1, row = 0, column = 1, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
    tkgrid(txt.sep1, row = 0, column = 2, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
    tkgrid(bt.TxtC2, row = 0, column = 3, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
    tkgrid(txt.TxtC2, row = 0, column = 4, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
    tkgrid(txt.sep2, row = 0, column = 5, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
    tkgrid(bt.TxtC3, row = 0, column = 6, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
    tkgrid(txt.TxtC3, row = 0, column = 7, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)

    ###########
    tkconfigure(bt.TxtC1, command = function(){
        tcl('wm', 'attributes', tt, topmost = FALSE)
        loko <- trimws(tclvalue(tcl("tk_chooseColor", initialcolor = tclvalue(text1.Kol), title = lang.dlg[['label']][['6']])))
        tcl('wm', 'attributes', tt, topmost = TRUE)
        if(nchar(loko) > 0){
            tkconfigure(bt.TxtC1, bg = loko)
            tclvalue(text1.Kol) <- loko
        }
    })

    tkconfigure(bt.TxtC2, command = function(){
        tcl('wm', 'attributes', tt, topmost = FALSE)
        loko <- trimws(tclvalue(tcl("tk_chooseColor", initialcolor = tclvalue(text2.Kol), title = lang.dlg[['label']][['6']])))
        tcl('wm', 'attributes', tt, topmost = TRUE)
        if(nchar(loko) > 0){
            tkconfigure(bt.TxtC2, bg = loko)
            tclvalue(text2.Kol) <- loko
        }
    })

    tkconfigure(bt.TxtC3, command = function(){
        tcl('wm', 'attributes', tt, topmost = FALSE)
        loko <- trimws(tclvalue(tcl("tk_chooseColor", initialcolor = tclvalue(text3.Kol), title = lang.dlg[['label']][['6']])))
        tcl('wm', 'attributes', tt, topmost = TRUE)
        if(nchar(loko) > 0){
            tkconfigure(bt.TxtC3, bg = loko)
            tclvalue(text3.Kol) <- loko
        }
    })

    #####################

    frameGraphTitle <- ttklabelframe(frDialog, text = lang.dlg[['label']][['7']], relief = 'groove')

    is.title <- tclVar(GraphOpt$title$is.title)
    text.title <- tclVar(GraphOpt$title$title)

    stateGpTlt <- if(GraphOpt$title$is.title) 'normal' else 'disabled'

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

    bt.StatsDisp <- ttkbutton(frDialog, text = lang.dlg[['button']][['2']])

    tkconfigure(bt.StatsDisp, command = function(){
        tcl('wm', 'attributes', tt, topmost = FALSE)
        GraphOpt$stats <<- SelectStats2Display.Valid.Rank(tt, GraphOpt$stats, description, lang.dlg[['button']][['2']])
        tcl('wm', 'attributes', tt, topmost = TRUE)
    })

    #####################

    frameKeyTxt <- ttklabelframe(frDialog, text = lang.dlg[['label']][['8']], relief = 'groove')

    key.title <- tclVar(GraphOpt$key$title)
    key.label1 <- tclVar(GraphOpt$key$lab1)
    key.label2 <- tclVar(GraphOpt$key$lab2)

    txt.KeyT <- tklabel(frameKeyTxt, text = lang.dlg[['label']][['7']], anchor = 'e', justify = 'right')
    en.KeyT <- tkentry(frameKeyTxt, textvariable = key.title, width = largeur3)
    txt.KeyL1 <- tklabel(frameKeyTxt, text = lang.dlg[['label']][['9']], anchor = 'e', justify = 'right')
    en.KeyL1 <- tkentry(frameKeyTxt, textvariable = key.label1, width = largeur3)
    txt.KeyL2 <- tklabel(frameKeyTxt, text = lang.dlg[['label']][['10']], anchor = 'e', justify = 'right')
    en.KeyL2 <- tkentry(frameKeyTxt, textvariable = key.label2, width = largeur3)

    tkgrid(txt.KeyT, row = 0, column = 0, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
    tkgrid(en.KeyT, row = 0, column = 1, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
    tkgrid(txt.KeyL1, row = 0, column = 2, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
    tkgrid(en.KeyL1, row = 0, column = 3, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
    tkgrid(txt.KeyL2, row = 0, column = 4, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
    tkgrid(txt.sep2, row = 0, column = 5, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
    tkgrid(en.KeyL2, row = 0, column = 6, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)

    #####################

    frameNames <- tkframe(frDialog, relief = 'groove', borderwidth = 2)

    validName <- ""
    if(length(GraphOpt$validName$name) > 1){
        validNameOpt <- GraphOpt$validName$name
        validNameOpt <- validNameOpt[validNameOpt != ""]
        if(length(validNameOpt) > 0){
           validName <- paste0(validNameOpt, collapse = ", ")
        }
    }
    valid.name <- tclVar(validName)

    valid.change <- tclVar(GraphOpt$validName$change)

    chk.changeN <- tkcheckbutton(frameNames, variable = valid.change, text = lang.dlg[['checkbutton']][['2']], anchor = 'w', justify = 'left')
    en.changeN <- tkentry(frameNames, textvariable = valid.name, width = largeur4)

    tkgrid(chk.changeN, row = 0, column = 0, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
    if(GraphOpt$validName$change){
        tkgrid(en.changeN, row = 1, column = 0, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
        helpWidget(en.changeN, lang.dlg[['tooltip']][['1']], lang.dlg[['status']][['1']])
    }

    ##########
    tkbind(chk.changeN, "<Button-1>", function(){
        tkdestroy(en.changeN)
        if(tclvalue(valid.change) == '0'){
            en.changeN <<- tkentry(frameNames, textvariable = valid.name, width = largeur4)
            tkgrid(en.changeN, row = 1, column = 0, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
            helpWidget(en.changeN, lang.dlg[['tooltip']][['1']], lang.dlg[['status']][['1']])
        }
    })

    #####################
    tkgrid(frameColkey, row = 0, column = 0, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
    tkgrid(frameTxtCol, row = 1, column = 0, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
    tkgrid(frameGraphTitle, row = 2, column = 0, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
    tkgrid(bt.StatsDisp, row = 3, column = 0, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 5, ipadx = 1, ipady = 1)
    tkgrid(frameKeyTxt, row = 4, column = 0, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
    tkgrid(frameNames, row = 5, column = 0, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)

    #####################
    bt.opt.OK <- ttkbutton(frButt, text = .cdtEnv$tcl$lang$global[['button']][['1']])
    bt.opt.CA <- ttkbutton(frButt, text = .cdtEnv$tcl$lang$global[['button']][['2']])

    tkconfigure(bt.opt.OK, command = function(){
        GraphOpt$col$custom <<- switch(tclvalue(custom.color), '0' = FALSE, '1' = TRUE)
        if(!GraphOpt$col$custom)
            GraphOpt$col$fill <<- default.Kol

        GraphOpt$col$text[1] <<- tclvalue(text1.Kol)
        GraphOpt$col$text[2] <<- tclvalue(text2.Kol)
        GraphOpt$col$text[3] <<- tclvalue(text3.Kol)

        GraphOpt$title$is.title <<- switch(tclvalue(is.title), '0' = FALSE, '1' = TRUE)
        GraphOpt$title$title <<- gsub('\\\\n', '\n', trimws(tclvalue(text.title)))

        GraphOpt$key$title <<- trimws(tclvalue(key.title))
        GraphOpt$key$lab1 <<- trimws(tclvalue(key.label1))
        GraphOpt$key$lab2 <<- trimws(tclvalue(key.label2))

        GraphOpt$validName$change <<- switch(tclvalue(valid.change), '0' = FALSE, '1' = TRUE)
        if(GraphOpt$validName$change){
            validName <- trimws(tclvalue(valid.name))
            validName <- strsplit(validName, ",")[[1]]
            validName <- trimws(validName)
            validName <- validName[validName != ""]
            if(length(validName) == 0) validName <- ""
            GraphOpt$validName$name <<- validName
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

    tkgrid(frDialog, row = 0, column = 0, sticky = 'nswe', rowspan = 1, columnspan = 2, padx = 1, pady = 1, ipadx = 1, ipady = 1)
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

#######################################################################################################

SelectStats2Display.Valid.Rank <- function(parent, stats, description, title){
    if(WindowsOS()){
        fr.height <- 350
        fr.width <- 400
    }else{
        fr.height <- 350
        fr.width <- 400
    }

    #####################

    tt <- tktoplevel()
    tkgrab.set(tt)
    tkfocus(tt)

    #####################
    frDialog <- tkframe(tt, relief = 'raised', borderwidth = 2)
    frButt <- tkframe(tt)

    #####################

    frameOut <- tkframe(frDialog, relief = 'sunken', borderwidth = 2)

    frStatName <- bwTabScrollableFrame(frameOut, wscrlwin = fr.width, hscrlwin = fr.height)

    stats2Plot <- lapply(stats$plot, tclVar)
    chkTexts <- paste(stats$name, ":", description)

    chk.stats <- lapply(seq_along(stats2Plot), function(j){
        tkcheckbutton(frStatName, variable = stats2Plot[[j]], text = chkTexts[j], anchor = 'w', justify = 'left')
    })

    for(j in seq_along(chk.stats))
        tkgrid(chk.stats[[j]], row = j - 1, column = 0, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)

    #####################
    tkgrid(frameOut, sticky = 'we', padx = 5, pady = 5, ipadx = 2, ipady = 2)

    #####################
    bt.opt.OK <- ttkbutton(frButt, text = .cdtEnv$tcl$lang$global[['button']][['1']])
    bt.opt.CA <- ttkbutton(frButt, text = .cdtEnv$tcl$lang$global[['button']][['2']])

    tkconfigure(bt.opt.OK, command = function(){
        stats$plot <<- sapply(seq_along(stats2Plot), function(j){
            switch(tclvalue(stats2Plot[[j]]), '0' = FALSE, '1' = TRUE)
        })

        tkgrab.release(tt)
        tkdestroy(tt)
        tkfocus(parent)
    })

    tkconfigure(bt.opt.CA, command = function(){
        tkgrab.release(tt)
        tkdestroy(tt)
        tkfocus(parent)
    })

    tkgrid(bt.opt.OK, row = 0, column = 0, padx = 5, pady = 1, ipadx = 1, sticky = 'w')
    tkgrid(bt.opt.CA, row = 0, column = 1, padx = 5, pady = 1, ipadx = 1, sticky = 'e')

    ###############################################################

    tkgrid(frDialog, row = 0, column = 0, sticky = 'nswe', rowspan = 1, columnspan = 2, padx = 1, pady = 1, ipadx = 1, ipady = 1)
    tkgrid(frButt, row = 1, column = 1, sticky = 'se', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)

    tkwm.withdraw(tt)
    tcl('update')
    tt.w <- as.integer(tkwinfo("reqwidth", tt))
    tt.h <- as.integer(tkwinfo("reqheight", tt))
    tt.x <- as.integer(.cdtEnv$tcl$data$width.scr*0.5 - tt.w*0.5)
    tt.y <- as.integer(.cdtEnv$tcl$data$height.scr*0.5 - tt.h*0.5)
    tkwm.geometry(tt, paste0('+', tt.x, '+', tt.y))
    tkwm.transient(tt)
    tkwm.title(tt, title)
    tkwm.deiconify(tt)
    tcl('wm', 'attributes', tt, topmost = TRUE)

    ##################################################################
    tkfocus(tt)
    tkbind(tt, "<Destroy>", function(){
        tkgrab.release(tt)
        tkfocus(parent)
    })
    tkwait.window(tt)
    return(stats)
}

#######################################################################################################

Validation.GraphOptions1.Scatter <- function(GraphOpt, parent.win = .cdtEnv$tcl$main$win){
    if(WindowsOS()){
        largeur0 <- 270
        largeur1 <- 57
        largeur2 <- 61
        largeur3 <- 6
        largeur4 <- 64
        width.col <- 3
        width.spin <- 4
    }else{
        largeur0 <- 292
        largeur1 <- 54
        largeur2 <- 58
        largeur3 <- 7
        largeur4 <- 60
        width.col <- 1
        width.spin <- 4
    }

    #####################
    xml.dlg <- file.path(.cdtDir$dirLocal, "languages", "cdtGraphOptions.Scatter_Valid1_dlgBox.xml")
    lang.dlg <- cdtLanguageParse(xml.dlg, .cdtData$Config$lang.iso)

    #####################
    tt <- tktoplevel()
    tkgrab.set(tt)
    tkfocus(tt)

    #####################
    frDialog <- tkframe(tt, relief = 'raised', borderwidth = 2)
    frButt <- tkframe(tt)

    #####################

    plotPoints <- function(){
        tkdestroy(frameScatIn)
        frameScatIn <<- tkframe(frameScatter)

        #####
        framePoint <- ttklabelframe(frameScatIn, text = lang.dlg[['label']][['6']], relief = 'groove')

        txt.PtC <- tklabel(framePoint, text = lang.dlg[['label']][['7']], anchor = 'e', justify = 'right')
        bt.PtC <- tkbutton(framePoint, bg = tclvalue(point.Kol), width = width.col)
        txt.PtT <- tklabel(framePoint, text = lang.dlg[['label']][['8']], anchor = 'e', justify = 'right')
        spin.PtT <<- ttkspinbox(framePoint, from = 15, to = 20, increment = 1, justify = 'center', width = width.spin)
        tkset(spin.PtT, GraphOpt$scatter$point$pch)
        txt.PtS <- tklabel(framePoint, text = lang.dlg[['label']][['9']], anchor = 'e', justify = 'right')
        spin.PtS <<- ttkspinbox(framePoint, from = 0.5, to = 4, increment = 0.1, justify = 'center', width = width.spin)
        tkset(spin.PtS, GraphOpt$scatter$point$cex)

        tkgrid(txt.PtC, row = 0, column = 0, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
        tkgrid(bt.PtC, row = 0, column = 1, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
        tkgrid(txt.PtT, row = 0, column = 2, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
        tkgrid(spin.PtT, row = 0, column = 3, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
        tkgrid(txt.PtS, row = 0, column = 4, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
        tkgrid(spin.PtS, row = 0, column = 5, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)

        ######
        tkgrid(framePoint)

        ######
        tkconfigure(bt.PtC, command = function(){
            tcl('wm', 'attributes', tt, topmost = FALSE)
            loko <- trimws(tclvalue(tcl("tk_chooseColor", initialcolor = tclvalue(point.Kol), title = lang.dlg[['label']][['10']])))
            tcl('wm', 'attributes', tt, topmost = TRUE)
            if(nchar(loko) > 0){
                tkconfigure(bt.PtC, bg = loko)
                tclvalue(point.Kol) <- loko
            }
        })

        ######
        tkgrid(frameScatIn, row = 0, column = 0, padx = 1)
    }

    #####################

    plotHexbin <- function(){
        tkdestroy(frameScatIn)
        frameScatIn <<- tkframe(frameScatter)

        #########
        default.Kol <- rev(RColorBrewer::brewer.pal(11, 'Spectral'))

        preview.canvasf <- function(kolKey, cond){
            if(cond){
                kolor <- getGradientColor(kolKey, 0:largeur0)
                tkdelete(canvas.preview, 'gradlines0')
                for(i in 0:largeur0)
                    tkcreate(canvas.preview, "line", i, 0, i, 20, fill = kolor[i], tags = 'gradlines0')
            }else{
                tkdelete(canvas.preview, 'gradlines0')
            }
        }

        #########
        frameColkey <- ttklabelframe(frameScatIn, text = lang.dlg[['label']][['12']], relief = 'groove')

        chk.userKol <- tkcheckbutton(frameColkey, variable = custom.color, text = lang.dlg[['checkbutton']][['2']], anchor = 'w', justify = 'left')
        bt.userKol <- ttkbutton(frameColkey, text = lang.dlg[['button']][['1']], state = stateKol2)
        canvas.preview <- tkcanvas(frameColkey, width = largeur0, height = 20, bg = 'white')

        ## Preview Color
        kolKey <- if(GraphOpt$scatter$hexbin$custom) GraphOpt$scatter$hexbin$col else default.Kol
        preview.canvasf(kolKey, !is.null(kolKey) & length(kolKey) > 0)

        tkgrid(chk.userKol, row = 0, column = 0, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
        tkgrid(bt.userKol, row = 0, column = 1, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
        tkgrid(canvas.preview, row = 1, column = 0, sticky = 'we', rowspan = 1, columnspan = 2, padx = 1, pady = 1, ipadx = 1, ipady = 1)

        #########
        tkgrid(frameColkey)

        #########

        tkconfigure(bt.userKol, command = function(){
            tcl('wm', 'attributes', tt, topmost = FALSE)
            GraphOpt$scatter$hexbin$col <<- createColorkey(tt, GraphOpt$scatter$hexbin$col)
            tcl('wm', 'attributes', tt, topmost = TRUE)
            preview.canvasf(GraphOpt$scatter$hexbin$col, !is.null(GraphOpt$scatter$hexbin$col))
        })

        tkbind(chk.userKol, "<Button-1>", function(){
            stateKol2 <- if(tclvalue(custom.color) == '0') 'normal' else 'disabled'
            tkconfigure(bt.userKol, state = stateKol2)

            kolKey <- if(tclvalue(custom.color) == '0') GraphOpt$scatter$hexbin$col else default.Kol
            preview.canvasf(kolKey, !is.null(kolKey))
        })

        ######
        tkgrid(frameScatIn, row = 0, column = 0, padx = 1)
    }

    #####################

    frameGraphXYlim <- tkframe(frDialog)

    ###########
 
    frameGraphXlim <- ttklabelframe(frameGraphXYlim, text = lang.dlg[['label']][['1']], relief = 'groove')

    is.min.xlim <- tclVar(GraphOpt$scatter$xlim$is.min)
    is.max.xlim <- tclVar(GraphOpt$scatter$xlim$is.max)
    min.xlim <- tclVar(GraphOpt$scatter$xlim$min)
    max.xlim <- tclVar(GraphOpt$scatter$xlim$max)

    stateMinXlim <- if(GraphOpt$scatter$xlim$is.min) 'normal' else 'disabled'
    stateMaxXlim <- if(GraphOpt$scatter$xlim$is.max) 'normal' else 'disabled'

    chk.min.Xlim <- tkcheckbutton(frameGraphXlim, variable = is.min.xlim, text = "Min", anchor = 'w', justify = 'left')
    en.min.Xlim <- tkentry(frameGraphXlim, textvariable = min.xlim, width = 5, state = stateMinXlim)
    chk.max.Xlim <- tkcheckbutton(frameGraphXlim, variable = is.max.xlim, text = "Max", anchor = 'w', justify = 'left')
    en.max.Xlim <- tkentry(frameGraphXlim, textvariable = max.xlim, width = 5, state = stateMaxXlim)

    tkgrid(chk.min.Xlim, row = 0, column = 0, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
    tkgrid(en.min.Xlim, row = 0, column = 1, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
    tkgrid(chk.max.Xlim, row = 0, column = 2, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
    tkgrid(en.max.Xlim, row = 0, column = 3, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)

    #########

    tkbind(chk.min.Xlim, "<Button-1>", function(){
        stateMinXlim <- if(tclvalue(is.min.xlim) == '0') 'normal' else 'disabled'
        tkconfigure(en.min.Xlim, state = stateMinXlim)
    })

    tkbind(chk.max.Xlim, "<Button-1>", function(){
        stateMaxXlim <- if(tclvalue(is.max.xlim) == '0') 'normal' else 'disabled'
        tkconfigure(en.max.Xlim, state = stateMaxXlim)
    })

    ###########

    frameGraphYlim <- ttklabelframe(frameGraphXYlim, text = lang.dlg[['label']][['2']], relief = 'groove')

    is.min.ylim <- tclVar(GraphOpt$scatter$ylim$is.min)
    is.max.ylim <- tclVar(GraphOpt$scatter$ylim$is.max)
    min.ylim <- tclVar(GraphOpt$scatter$ylim$min)
    max.ylim <- tclVar(GraphOpt$scatter$ylim$max)

    stateMinYlim <- if(GraphOpt$scatter$ylim$is.min) 'normal' else 'disabled'
    stateMaxYlim <- if(GraphOpt$scatter$ylim$is.max) 'normal' else 'disabled'

    chk.min.Ylim <- tkcheckbutton(frameGraphYlim, variable = is.min.ylim, text = "Min", anchor = 'w', justify = 'left')
    en.min.Ylim <- tkentry(frameGraphYlim, textvariable = min.ylim, width = 5, state = stateMinYlim)
    chk.max.Ylim <- tkcheckbutton(frameGraphYlim, variable = is.max.ylim, text = "Max", anchor = 'w', justify = 'left')
    en.max.Ylim <- tkentry(frameGraphYlim, textvariable = max.ylim, width = 5, state = stateMaxYlim)

    tkgrid(chk.min.Ylim, row = 0, column = 0, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
    tkgrid(en.min.Ylim, row = 0, column = 1, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
    tkgrid(chk.max.Ylim, row = 0, column = 2, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
    tkgrid(en.max.Ylim, row = 0, column = 3, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)

    #########

    tkbind(chk.min.Ylim, "<Button-1>", function(){
        stateMinYlim <- if(tclvalue(is.min.ylim) == '0') 'normal' else 'disabled'
        tkconfigure(en.min.Ylim, state = stateMinYlim)
    })

    tkbind(chk.max.Ylim, "<Button-1>", function(){
        stateMaxYlim <- if(tclvalue(is.max.ylim) == '0') 'normal' else 'disabled'
        tkconfigure(en.max.Ylim, state = stateMaxYlim)
    })

    #####################

    sepXYlim <- tklabel(frameGraphXYlim, text = "", width = largeur3)

    tkgrid(frameGraphXlim, row = 0, column = 0, sticky = 'w')
    tkgrid(sepXYlim, row = 0, column = 1, sticky = 'we')
    tkgrid(frameGraphYlim, row = 0, column = 2, sticky = 'e')

    #####################

    frameGraphAxLabs <- ttklabelframe(frDialog, text = lang.dlg[['label']][['3']], relief = 'groove')

    is.xaxis.lab <- tclVar(GraphOpt$scatter$axislabs$is.xlab)
    is.yaxis.lab <- tclVar(GraphOpt$scatter$axislabs$is.ylab)
    xaxis.lab <- tclVar(GraphOpt$scatter$axislabs$xlab)
    yaxis.lab <- tclVar(GraphOpt$scatter$axislabs$ylab)

    stateXLab <- if(GraphOpt$scatter$axislabs$is.xlab) 'normal' else 'disabled'
    stateYLab <- if(GraphOpt$scatter$axislabs$is.ylab) 'normal' else 'disabled'

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

    frameGraphTitle <- ttklabelframe(frDialog, text = lang.dlg[['label']][['4']], relief = 'groove')

    is.title <- tclVar(GraphOpt$scatter$title$is.title)
    text.title <- tclVar(GraphOpt$scatter$title$title)

    stateGpTlt <- if(GraphOpt$scatter$title$is.title) 'normal' else 'disabled'

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

    framePlotT <- tkframe(frDialog, relief = 'sunken', borderwidth = 2)

    plot.type <- tclVar()
    CbplotTypeVAL <- lang.dlg[['combobox']][['1']]
    plotTypeVAL <- c("points", "hexbin")
    tclvalue(plot.type) <- CbplotTypeVAL[plotTypeVAL %in% GraphOpt$scatter$plot.type]

    txt.plotT <- tklabel(framePlotT, text = lang.dlg[['label']][['5']], anchor = 'e', justify = 'right')
    cb.plotT <- ttkcombobox(framePlotT, values = CbplotTypeVAL, textvariable = plot.type, width = 15)
    txt.plotT1 <- tklabel(framePlotT, text = '')

    tkgrid(txt.plotT1, row = 0, column = 0, sticky = 'we', rowspan = 1, columnspan = 2)
    tkgrid(txt.plotT, row = 1, column = 0, sticky = 'we', rowspan = 1, columnspan = 1)
    tkgrid(cb.plotT, row = 1, column = 1, sticky = 'we', rowspan = 1, columnspan = 1)

    ############

    tkbind(cb.plotT, "<<ComboboxSelected>>", function(){
        pltType <- plotTypeVAL[CbplotTypeVAL %in% trimws(tclvalue(plot.type))]
        if(pltType == 'points') plotPoints() else plotHexbin()
    })

    #####################

    frameScatter <- tkframe(frDialog)

    ############

    frameScatIn <- tkframe(frameScatter)

    point.Kol <- tclVar(GraphOpt$scatter$point$col)
    custom.color <- tclVar(GraphOpt$scatter$hexbin$custom)

    stateKol2 <- if(GraphOpt$scatter$hexbin$custom) "normal" else "disabled"

    spin.PtT <- NULL
    spin.PtS <- NULL

    if(GraphOpt$scatter$plot.type == 'points') plotPoints() else plotHexbin()

    ##########

    frameXY <- tkframe(frameScatter, relief = 'groove', borderwidth = 2)

    draw.XY <- tclVar(GraphOpt$scatter$line$draw)
    col.XY <- tclVar(GraphOpt$scatter$line$col)

    stateXY <- if(tclvalue(draw.XY) == '1') 'normal' else 'disabled'

    chk.pltLine <- tkcheckbutton(frameXY, variable = draw.XY, text = lang.dlg[['checkbutton']][['1']], anchor = 'w', justify = 'left')
    txt.pltLineC <- tklabel(frameXY, text = lang.dlg[['label']][['7']], anchor = 'e', justify = 'right')
    bt.pltLineC <- tkbutton(frameXY, bg = tclvalue(col.XY), width = width.col, state = stateXY)
    txt.pltLineW <- tklabel(frameXY, text = lang.dlg[['label']][['11']], anchor = 'e', justify = 'right')
    spin.pltLineW <- ttkspinbox(frameXY, from = 0.5, to = 4, increment = 0.1, justify = 'center', width = width.spin, state = stateXY)
    tkset(spin.pltLineW, GraphOpt$scatter$line$lwd)

    tkgrid(chk.pltLine, row = 0, column = 0, sticky = 'we', rowspan = 1, columnspan = 4, padx = 1, pady = 1, ipadx = 1, ipady = 1)
    tkgrid(txt.pltLineC, row = 1, column = 0, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
    tkgrid(bt.pltLineC, row = 1, column = 1, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
    tkgrid(txt.pltLineW, row = 1, column = 2, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
    tkgrid(spin.pltLineW, row = 1, column = 3, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)

    ######
    tkconfigure(bt.pltLineC, command = function(){
        tcl('wm', 'attributes', tt, topmost = FALSE)
        loko <- trimws(tclvalue(tcl("tk_chooseColor", initialcolor = tclvalue(col.XY), title = lang.dlg[['label']][['10']])))
        tcl('wm', 'attributes', tt, topmost = TRUE)
        if(nchar(loko) > 0){
            tkconfigure(bt.pltLineC, bg = loko)
            tclvalue(col.XY) <- loko
        }
    })

    tkbind(chk.pltLine, "<Button-1>", function(){
        stateXY <- if(tclvalue(draw.XY) == '1') 'disabled' else 'normal'
        tkconfigure(bt.pltLineC, state = stateXY)
        tkconfigure(spin.pltLineW, state = stateXY)
    })

    ###########

    tkgrid(frameXY, row = 0, column = 1, padx = 1)

    #####################

    frameNames <- tkframe(frDialog, relief = 'groove', borderwidth = 2)

    validName <- ""
    if(length(GraphOpt$scatter$validName$name) > 1){
        validNameOpt <- GraphOpt$scatter$validName$name
        validNameOpt <- validNameOpt[validNameOpt != ""]
        if(length(validNameOpt) > 0){
           validName <- paste0(validNameOpt, collapse = ", ")
        }
    }
    valid.name <- tclVar(validName)

    valid.change <- tclVar(GraphOpt$scatter$validName$change)

    chk.changeN <- tkcheckbutton(frameNames, variable = valid.change, text = lang.dlg[['checkbutton']][['3']], anchor = 'w', justify = 'left')
    en.changeN <- tkentry(frameNames, textvariable = valid.name, width = largeur4)

    tkgrid(chk.changeN, row = 0, column = 0, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
    if(GraphOpt$scatter$validName$change){
        tkgrid(en.changeN, row = 1, column = 0, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
        helpWidget(en.changeN, lang.dlg[['tooltip']][['1']], lang.dlg[['status']][['1']])
    }

    ##########
    tkbind(chk.changeN, "<Button-1>", function(){
        tkdestroy(en.changeN)
        if(tclvalue(valid.change) == '0'){
            en.changeN <<- tkentry(frameNames, textvariable = valid.name, width = largeur4)
            tkgrid(en.changeN, row = 1, column = 0, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
            helpWidget(en.changeN, lang.dlg[['tooltip']][['1']], lang.dlg[['status']][['1']])
        }
    })

    #####################

    tkgrid(frameGraphXYlim, row = 0, column = 0, sticky = '', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
    tkgrid(frameGraphAxLabs, row = 1, column = 0, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 2, ipadx = 1, ipady = 1)
    tkgrid(frameGraphTitle, row = 2, column = 0, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 2, ipadx = 1, ipady = 1)
    tkgrid(framePlotT, row = 3, column = 0, sticky = '', rowspan = 1, columnspan = 1, padx = 1, pady = 3, ipadx = 2, ipady = 2)
    tkgrid(frameScatter, row = 4, column = 0, sticky = '', rowspan = 1, columnspan = 1, padx = 1, pady = 2, ipadx = 1, ipady = 1)
    tkgrid(frameNames, row = 5, column = 0, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 2, ipadx = 1, ipady = 1)

    #####################
    bt.opt.OK <- ttkbutton(frButt, text = .cdtEnv$tcl$lang$global[['button']][['1']])
    bt.opt.CA <- ttkbutton(frButt, text = .cdtEnv$tcl$lang$global[['button']][['2']])

    tkconfigure(bt.opt.OK, command = function(){
        GraphOpt$scatter$xlim$is.min <<- switch(tclvalue(is.min.xlim), '0' = FALSE, '1' = TRUE)
        GraphOpt$scatter$xlim$is.max <<- switch(tclvalue(is.max.xlim), '0' = FALSE, '1' = TRUE)
        GraphOpt$scatter$xlim$min <<- as.numeric(trimws(tclvalue(min.xlim)))
        GraphOpt$scatter$xlim$max <<- as.numeric(trimws(tclvalue(max.xlim)))

        GraphOpt$scatter$ylim$is.min <<- switch(tclvalue(is.min.ylim), '0' = FALSE, '1' = TRUE)
        GraphOpt$scatter$ylim$is.max <<- switch(tclvalue(is.max.ylim), '0' = FALSE, '1' = TRUE)
        GraphOpt$scatter$ylim$min <<- as.numeric(trimws(tclvalue(min.ylim)))
        GraphOpt$scatter$ylim$max <<- as.numeric(trimws(tclvalue(max.ylim)))

        GraphOpt$scatter$axislabs$is.xlab <<- switch(tclvalue(is.xaxis.lab), '0' = FALSE, '1' = TRUE)
        GraphOpt$scatter$axislabs$is.ylab <<- switch(tclvalue(is.yaxis.lab), '0' = FALSE, '1' = TRUE)
        GraphOpt$scatter$axislabs$xlab <<- gsub('\\\\n', '\n', trimws(tclvalue(xaxis.lab)))
        GraphOpt$scatter$axislabs$ylab <<- gsub('\\\\n', '\n', trimws(tclvalue(yaxis.lab)))

        GraphOpt$scatter$title$is.title <<- switch(tclvalue(is.title), '0' = FALSE, '1' = TRUE)
        GraphOpt$scatter$title$title <<- gsub('\\\\n', '\n', trimws(tclvalue(text.title)))

        GraphOpt$scatter$plot.type <<- plotTypeVAL[CbplotTypeVAL %in% trimws(tclvalue(plot.type))]

        if(GraphOpt$scatter$plot.type == 'points'){
            GraphOpt$scatter$point$col <<- tclvalue(point.Kol)
            GraphOpt$scatter$point$pch <<- as.numeric(trimws(tclvalue(tkget(spin.PtT))))
            GraphOpt$scatter$point$cex <<- as.numeric(trimws(tclvalue(tkget(spin.PtS))))
        }else{
            GraphOpt$scatter$hexbin$custom <<- switch(tclvalue(custom.color), '0' = FALSE, '1' = TRUE)
        }

        GraphOpt$scatter$line$draw <<- switch(tclvalue(draw.XY), '0' = FALSE, '1' = TRUE)
        GraphOpt$scatter$line$col <<- tclvalue(col.XY)
        GraphOpt$scatter$line$lwd <<- as.numeric(trimws(tclvalue(tkget(spin.pltLineW))))

        GraphOpt$scatter$validName$change <<- switch(tclvalue(valid.change), '0' = FALSE, '1' = TRUE)
        if(GraphOpt$scatter$validName$change){
            validName <- trimws(tclvalue(valid.name))
            validName <- strsplit(validName, ",")[[1]]
            validName <- trimws(validName)
            validName <- validName[validName != ""]
            if(length(validName) == 0) validName <- ""
            GraphOpt$scatter$validName$name <<- validName
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

#######################################################################################################

Validation.GraphOptions1.CDF <- function(GraphOpt, parent.win = .cdtEnv$tcl$main$win){
    if(WindowsOS()){
        largeur0 <- 350
        largeur1 <- 63
        largeur2 <- 67
        largeur3 <- 11
        largeur4 <- 35
        largeur5 <- 70
        largeur6 <- 56
        width.col <- 3
        width.spin <- 4
    }else{
        largeur0 <- 370
        largeur1 <- 60
        largeur2 <- 64
        largeur3 <- 12
        largeur4 <- 33
        largeur5 <- 66
        largeur6 <- 53
        width.col <- 1
        width.spin <- 4
    }

    #####################
    xml.dlg <- file.path(.cdtDir$dirLocal, "languages", "cdtGraphOptions.CDF_Valid1_dlgBox.xml")
    lang.dlg <- cdtLanguageParse(xml.dlg, .cdtData$Config$lang.iso)

    #####################
    tt <- tktoplevel()
    tkgrab.set(tt)
    tkfocus(tt)

    #####################
    frDialog <- tkframe(tt, relief = 'raised', borderwidth = 2)
    frButt <- tkframe(tt)

    #####################

    plotMultiplePanel <- function(){
        tkdestroy(frameOptIn)
        frameOptIn <<- tkframe(framePlotOpt)

        #########
        frameGraphObs <- ttklabelframe(frameOptIn, text = lang.dlg[['label']][['14']], relief = 'groove')

        txt.pltType1 <- tklabel(frameGraphObs, text = lang.dlg[['label']][['6']], anchor = 'e', justify = 'right')
        cb.pltType1 <- ttkcombobox(frameGraphObs, values = c('both', 'line'), textvariable = plot.type1, width = 4)

        #########
        tkbind(cb.pltType1, "<<ComboboxSelected>>", function(){
            statePlotPoints1 <- if(tclvalue(plot.type1) == 'both') 'normal' else 'disabled'
            tkconfigure(bt.pltPointC1, state = statePlotPoints1)
            tkconfigure(spin.pltPointS1, state = statePlotPoints1)
            tkconfigure(spin.pltPointT1, state = statePlotPoints1)
        })

        ########
        framepltLine1 <- ttklabelframe(frameGraphObs, text = lang.dlg[['label']][['7']], relief = 'groove')

        txt.pltLineC1 <- tklabel(framepltLine1, text = lang.dlg[['label']][['9']], anchor = 'e', justify = 'right')
        bt.pltLineC1 <- tkbutton(framepltLine1, bg = tclvalue(plot.col.line1), width = width.col)
        txt.pltLineW1 <- tklabel(framepltLine1, text = lang.dlg[['label']][['10']], anchor = 'e', justify = 'right')
        spin.pltLineW1 <<- ttkspinbox(framepltLine1, from = 0.5, to = 4, increment = 0.1, justify = 'center', width = width.spin)
        tkset(spin.pltLineW1, GraphOpt$cdf$plot$obs$lwd)

        tkgrid(txt.pltLineC1, bt.pltLineC1, txt.pltLineW1, spin.pltLineW1)

        ########
        tkconfigure(bt.pltLineC1, command = function(){
            tcl('wm', 'attributes', tt, topmost = FALSE)
            loko <- trimws(tclvalue(tcl("tk_chooseColor", initialcolor = tclvalue(plot.col.line1), title = lang.dlg[['label']][['13']])))
            tcl('wm', 'attributes', tt, topmost = TRUE)
            if(nchar(loko) > 0){
                tkconfigure(bt.pltLineC1, bg = loko)
                tclvalue(plot.col.line1) <- loko
            }
        })

        ########
        framepltPoints1 <- ttklabelframe(frameGraphObs, text = lang.dlg[['label']][['8']], relief = 'groove')

        txt.pltPointC1 <- tklabel(framepltPoints1, text = lang.dlg[['label']][['9']], anchor = 'e', justify = 'right')
        bt.pltPointC1 <- tkbutton(framepltPoints1, bg = tclvalue(plot.col.points1), width = width.col, state = statePlotPoints1)
        txt.pltPointT1 <- tklabel(framepltPoints1, text = lang.dlg[['label']][['11']], anchor = 'e', justify = 'right')
        spin.pltPointT1 <<- ttkspinbox(framepltPoints1, from = 21, to = 25, increment = 1, justify = 'center', width = width.spin, state = statePlotPoints1)
        tkset(spin.pltPointT1, GraphOpt$cdf$plot$obs$pch)
        txt.pltPointS1 <- tklabel(framepltPoints1, text = lang.dlg[['label']][['12']], anchor = 'e', justify = 'right')
        spin.pltPointS1 <<- ttkspinbox(framepltPoints1, from = 0.5, to = 2.5, increment = 0.1, justify = 'center', width = width.spin, state = statePlotPoints1)
        tkset(spin.pltPointS1, GraphOpt$cdf$plot$obs$cex)

        tkgrid(txt.pltPointC1, bt.pltPointC1, txt.pltPointT1, spin.pltPointT1, txt.pltPointS1, spin.pltPointS1)

        ########

        tkconfigure(bt.pltPointC1, command = function(){
            tcl('wm', 'attributes', tt, topmost = FALSE)
            loko <- trimws(tclvalue(tcl("tk_chooseColor", initialcolor = tclvalue(plot.col.points1), title = lang.dlg[['label']][['13']])))
            tcl('wm', 'attributes', tt, topmost = TRUE)
            if(nchar(loko) > 0){
                tkconfigure(bt.pltPointC1, bg = loko)
                tclvalue(plot.col.points1) <- loko
            }
        })

        ##########

        tkgrid(txt.pltType1, row = 0, column = 0)
        tkgrid(cb.pltType1, row = 0, column = 1)
        tkgrid(framepltLine1, row = 0, column = 2, padx = 2)
        tkgrid(framepltPoints1, row = 0, column = 3)

        #####################

        frameGraphEst <- ttklabelframe(frameOptIn, text = lang.dlg[['label']][['15']], relief = 'groove')

        txt.pltType2 <- tklabel(frameGraphEst, text = lang.dlg[['label']][['6']], anchor = 'e', justify = 'right')
        cb.pltType2 <- ttkcombobox(frameGraphEst, values = c('both', 'line'), textvariable = plot.type2, width = 4)

        #########
        tkbind(cb.pltType2, "<<ComboboxSelected>>", function(){
            statePlotPoints2 <- if(tclvalue(plot.type2) == 'both') 'normal' else 'disabled'
            tkconfigure(bt.pltPointC2, state = statePlotPoints2)
            tkconfigure(spin.pltPointS2, state = statePlotPoints2)
            tkconfigure(spin.pltPointT2, state = statePlotPoints2)
        })

        ########
        framepltLine2 <- ttklabelframe(frameGraphEst, text = lang.dlg[['label']][['7']], relief = 'groove')

        txt.pltLineC2 <- tklabel(framepltLine2, text = lang.dlg[['label']][['9']], anchor = 'e', justify = 'right')
        bt.pltLineC2 <- tkbutton(framepltLine2, bg = tclvalue(plot.col.line2), width = width.col)
        txt.pltLineW2 <- tklabel(framepltLine2, text = lang.dlg[['label']][['10']], anchor = 'e', justify = 'right')
        spin.pltLineW2 <<- ttkspinbox(framepltLine2, from = 0.5, to = 4, increment = 0.1, justify = 'center', width = width.spin)
        tkset(spin.pltLineW2, GraphOpt$cdf$plot$est$lwd)

        tkgrid(txt.pltLineC2, bt.pltLineC2, txt.pltLineW2, spin.pltLineW2)

        ########
        tkconfigure(bt.pltLineC2, command = function(){
            tcl('wm', 'attributes', tt, topmost = FALSE)
            loko <- trimws(tclvalue(tcl("tk_chooseColor", initialcolor = tclvalue(plot.col.line2), title = lang.dlg[['label']][['13']])))
            tcl('wm', 'attributes', tt, topmost = TRUE)
            if(nchar(loko) > 0){
                tkconfigure(bt.pltLineC2, bg = loko)
                tclvalue(plot.col.line2) <- loko
            }
        })

        ########
        framepltPoints2 <- ttklabelframe(frameGraphEst, text = lang.dlg[['label']][['8']], relief = 'groove')

        txt.pltPointC2 <- tklabel(framepltPoints2, text = lang.dlg[['label']][['9']], anchor = 'e', justify = 'right')
        bt.pltPointC2 <- tkbutton(framepltPoints2, bg = tclvalue(plot.col.points2), width = width.col, state = statePlotPoints2)
        txt.pltPointT2 <- tklabel(framepltPoints2, text = lang.dlg[['label']][['11']], anchor = 'e', justify = 'right')
        spin.pltPointT2 <<- ttkspinbox(framepltPoints2, from = 21, to = 25, increment = 1, justify = 'center', width = width.spin, state = statePlotPoints2)
        tkset(spin.pltPointT2, GraphOpt$cdf$plot$est$pch)
        txt.pltPointS2 <- tklabel(framepltPoints2, text = lang.dlg[['label']][['12']], anchor = 'e', justify = 'right')
        spin.pltPointS2 <<- ttkspinbox(framepltPoints2, from = 0.5, to = 2.5, increment = 0.1, justify = 'center', width = width.spin, state = statePlotPoints2)
        tkset(spin.pltPointS2, GraphOpt$cdf$plot$est$cex)

        tkgrid(txt.pltPointC2, bt.pltPointC2, txt.pltPointT2, spin.pltPointT2, txt.pltPointS2, spin.pltPointS2)

        ########

        tkconfigure(bt.pltPointC2, command = function(){
            tcl('wm', 'attributes', tt, topmost = FALSE)
            loko <- trimws(tclvalue(tcl("tk_chooseColor", initialcolor = tclvalue(plot.col.points2), title = lang.dlg[['label']][['13']])))
            tcl('wm', 'attributes', tt, topmost = TRUE)
            if(nchar(loko) > 0){
                tkconfigure(bt.pltPointC2, bg = loko)
                tclvalue(plot.col.points2) <- loko
            }
        })

        ###########

        tkgrid(txt.pltType2, row = 0, column = 0)
        tkgrid(cb.pltType2, row = 0, column = 1)
        tkgrid(framepltLine2, row = 0, column = 2, padx = 2)
        tkgrid(framepltPoints2, row = 0, column = 3)

        #####################

        frameLegend <- ttklabelframe(frameOptIn, text = lang.dlg[['label']][['16']], relief = 'groove')

        stateLeg <- if(tclvalue(add.legend) == '1') 'normal' else 'disabled'

        chk.Leg <- tkcheckbutton(frameLegend, variable = add.legend, text = lang.dlg[['checkbutton']][['1']], anchor = 'w', justify = 'left')
        txt.Leg1 <- tklabel(frameLegend, text = lang.dlg[['label']][['14']], anchor = 'w', justify = 'left')
        en.Leg1 <- tkentry(frameLegend, textvariable = obs.legend, width = largeur4, state = stateLeg)
        txt.Leg2 <- tklabel(frameLegend, text = lang.dlg[['label']][['15']], anchor = 'w', justify = 'left')
        en.Leg2 <- tkentry(frameLegend, textvariable = est.legend, width = largeur4, state = stateLeg)

        tkgrid(chk.Leg, row = 0, column = 0, sticky = 'we', columnspan = 4)
        tkgrid(txt.Leg1, row = 1, column = 0, sticky = 'w', columnspan = 1)
        tkgrid(en.Leg1, row = 2, column = 0, sticky = 'we', columnspan = 1)
        tkgrid(txt.Leg2, row = 1, column = 1, sticky = 'w', columnspan = 1)
        tkgrid(en.Leg2, row = 2, column = 1, sticky = 'we', columnspan = 1)

        #########

        tkbind(chk.Leg, "<Button-1>", function(){
            stateLeg <- if(tclvalue(add.legend) == '0') 'normal' else 'disabled'
            tkconfigure(en.Leg1, state = stateLeg)
            tkconfigure(en.Leg2, state = stateLeg)
        })

        #####################

        frameNames <- tkframe(frameOptIn, relief = 'groove', borderwidth = 2)

        chk.changeN <- tkcheckbutton(frameNames, variable = valid.change, text = lang.dlg[['checkbutton']][['3']], anchor = 'w', justify = 'left')
        en.changeN <- tkentry(frameNames, textvariable = valid.name, width = largeur5)

        tkgrid(chk.changeN, row = 0, column = 0, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
        if(tclvalue(valid.change) == '1'){
            tkgrid(en.changeN, row = 1, column = 0, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
            helpWidget(en.changeN, lang.dlg[['tooltip']][['1']], lang.dlg[['status']][['1']])
        }

        ##########
        tkbind(chk.changeN, "<Button-1>", function(){
            tkdestroy(en.changeN)
            if(tclvalue(valid.change) == '0'){
                en.changeN <<- tkentry(frameNames, textvariable = valid.name, width = largeur5)
                tkgrid(en.changeN, row = 1, column = 0, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
                helpWidget(en.changeN, lang.dlg[['tooltip']][['1']], lang.dlg[['status']][['1']])
            }
        })

        #####################

        tkgrid(frameGraphObs, row = 0, column = 0, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 2, ipadx = 1, ipady = 1)
        tkgrid(frameGraphEst, row = 1, column = 0, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 2, ipadx = 1, ipady = 1)
        tkgrid(frameLegend, row = 2, column = 0, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 2, ipadx = 1, ipady = 1)
        tkgrid(frameNames, row = 3, column = 0, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 2, ipadx = 1, ipady = 1)

        #####################

        tkgrid(frameOptIn)
    }

    ########

    plotSinglePanel <- function(){
        tkdestroy(frameOptIn)
        frameOptIn <<- tkframe(framePlotOpt)

        #########
        frameGraphObs <- ttklabelframe(frameOptIn, text = lang.dlg[['label']][['14']], relief = 'groove')

        txt.pltType1 <- tklabel(frameGraphObs, text = lang.dlg[['label']][['6']], anchor = 'e', justify = 'right')
        cb.pltType1 <- ttkcombobox(frameGraphObs, values = c('both', 'line'), textvariable = plot.type1, width = 4)

        #########
        tkbind(cb.pltType1, "<<ComboboxSelected>>", function(){
            statePlotPoints1 <- if(tclvalue(plot.type1) == 'both') 'normal' else 'disabled'
            tkconfigure(bt.pltPointC1, state = statePlotPoints1)
            tkconfigure(spin.pltPointS1, state = statePlotPoints1)
            tkconfigure(spin.pltPointT1, state = statePlotPoints1)
        })

        ########
        framepltLine1 <- ttklabelframe(frameGraphObs, text = lang.dlg[['label']][['7']], relief = 'groove')

        txt.pltLineC1 <- tklabel(framepltLine1, text = lang.dlg[['label']][['9']], anchor = 'e', justify = 'right')
        bt.pltLineC1 <- tkbutton(framepltLine1, bg = tclvalue(plot.col.line1), width = width.col)
        txt.pltLineW1 <- tklabel(framepltLine1, text = lang.dlg[['label']][['10']], anchor = 'e', justify = 'right')
        spin.pltLineW1 <<- ttkspinbox(framepltLine1, from = 0.5, to = 4, increment = 0.1, justify = 'center', width = width.spin)
        tkset(spin.pltLineW1, GraphOpt$cdf$plot$obs$lwd)

        tkgrid(txt.pltLineC1, bt.pltLineC1, txt.pltLineW1, spin.pltLineW1)

        ########
        tkconfigure(bt.pltLineC1, command = function(){
            tcl('wm', 'attributes', tt, topmost = FALSE)
            loko <- trimws(tclvalue(tcl("tk_chooseColor", initialcolor = tclvalue(plot.col.line1), title = lang.dlg[['label']][['13']])))
            tcl('wm', 'attributes', tt, topmost = TRUE)
            if(nchar(loko) > 0){
                tkconfigure(bt.pltLineC1, bg = loko)
                tclvalue(plot.col.line1) <- loko
            }
        })

        ########
        framepltPoints1 <- ttklabelframe(frameGraphObs, text = lang.dlg[['label']][['8']], relief = 'groove')

        txt.pltPointC1 <- tklabel(framepltPoints1, text = lang.dlg[['label']][['9']], anchor = 'e', justify = 'right')
        bt.pltPointC1 <- tkbutton(framepltPoints1, bg = tclvalue(plot.col.points1), width = width.col, state = statePlotPoints1)
        txt.pltPointT1 <- tklabel(framepltPoints1, text = lang.dlg[['label']][['11']], anchor = 'e', justify = 'right')
        spin.pltPointT1 <<- ttkspinbox(framepltPoints1, from = 21, to = 25, increment = 1, justify = 'center', width = width.spin, state = statePlotPoints1)
        tkset(spin.pltPointT1, GraphOpt$cdf$plot$obs$pch)
        txt.pltPointS1 <- tklabel(framepltPoints1, text = lang.dlg[['label']][['12']], anchor = 'e', justify = 'right')
        spin.pltPointS1 <<- ttkspinbox(framepltPoints1, from = 0.5, to = 2.5, increment = 0.1, justify = 'center', width = width.spin, state = statePlotPoints1)
        tkset(spin.pltPointS1, GraphOpt$cdf$plot$obs$cex)

        tkgrid(txt.pltPointC1, bt.pltPointC1, txt.pltPointT1, spin.pltPointT1, txt.pltPointS1, spin.pltPointS1)

        ########

        tkconfigure(bt.pltPointC1, command = function(){
            tcl('wm', 'attributes', tt, topmost = FALSE)
            loko <- trimws(tclvalue(tcl("tk_chooseColor", initialcolor = tclvalue(plot.col.points1), title = lang.dlg[['label']][['13']])))
            tcl('wm', 'attributes', tt, topmost = TRUE)
            if(nchar(loko) > 0){
                tkconfigure(bt.pltPointC1, bg = loko)
                tclvalue(plot.col.points1) <- loko
            }
        })

        ##########

        tkgrid(txt.pltType1, row = 0, column = 0)
        tkgrid(cb.pltType1, row = 0, column = 1)
        tkgrid(framepltLine1, row = 0, column = 2, padx = 2)
        tkgrid(framepltPoints1, row = 0, column = 3)

        #####################

        frameGraphEst <- ttklabelframe(frameOptIn, text = lang.dlg[['label']][['15']], relief = 'groove')

        default.Kol <- grDevices::rainbow(4)

        preview.canvasf <- function(kolKey, cond){
            if(cond){
                kolor <- getGradientColor(kolKey, 0:largeur0)
                tkdelete(canvas.preview, 'gradlines0')
                for(i in 0:largeur0)
                    tkcreate(canvas.preview, "line", i, 0, i, 20, fill = kolor[i], tags = 'gradlines0')
            }else{
                tkdelete(canvas.preview, 'gradlines0')
            }
        }

        #########

        chk.userKol <- tkcheckbutton(frameGraphEst, variable = custom.color, text = lang.dlg[['checkbutton']][['2']], anchor = 'w', justify = 'left')
        bt.userKol <- ttkbutton(frameGraphEst, text = lang.dlg[['button']][['1']], state = stateKol2)
        canvas.preview <- tkcanvas(frameGraphEst, width = largeur0, height = 20, bg = 'white')

        ## Preview Color
        kolKey <- if(tclvalue(custom.color) == '1') GraphOpt$cdf$plot1$est else default.Kol
        preview.canvasf(kolKey, !is.null(kolKey) & length(kolKey) > 0)

        tkgrid(chk.userKol, row = 0, column = 0, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
        tkgrid(bt.userKol, row = 0, column = 1, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
        tkgrid(canvas.preview, row = 1, column = 0, sticky = 'we', rowspan = 1, columnspan = 2, padx = 1, pady = 1, ipadx = 1, ipady = 1)

        #########

        tkconfigure(bt.userKol, command = function(){
            tcl('wm', 'attributes', tt, topmost = FALSE)
            GraphOpt$cdf$plot1$est <<- createColorkey(tt, GraphOpt$cdf$plot1$estl)
            tcl('wm', 'attributes', tt, topmost = TRUE)
            preview.canvasf(GraphOpt$cdf$plot1$est, !is.null(GraphOpt$cdf$plot1$est))
        })

        tkbind(chk.userKol, "<Button-1>", function(){
            stateKol2 <- if(tclvalue(custom.color) == '0') 'normal' else 'disabled'
            tkconfigure(bt.userKol, state = stateKol2)

            kolKey <- if(tclvalue(custom.color) == '0') GraphOpt$cdf$plot1$est else default.Kol
            preview.canvasf(kolKey, !is.null(kolKey))
        })

        #####################

        frameLegend <- ttklabelframe(frameOptIn, text = lang.dlg[['label']][['16']], relief = 'groove')

        txt.Leg <- tklabel(frameLegend, text = '')
        chk.Leg <- tkcheckbutton(frameLegend, variable = add.legend, text = lang.dlg[['checkbutton']][['1']], anchor = 'w', justify = 'left')

        tkgrid(txt.Leg)
        tkgrid(chk.Leg)

        #####################

        frameNames <- tkframe(frameOptIn, relief = 'groove', borderwidth = 2)

        chk.changeN <- tkcheckbutton(frameNames, variable = valid.change, text = lang.dlg[['checkbutton']][['3']], anchor = 'w', justify = 'left')
        txt.changeO <- tklabel(frameNames, text = lang.dlg[['label']][['14']], anchor = 'e', justify = 'right')
        en.changeO <- tkentry(frameNames, textvariable = valid.obsN, width = largeur4)
        txt.changeN <- tklabel(frameNames, text = lang.dlg[['label']][['17']], anchor = 'e', justify = 'right')
        en.changeN <- tkentry(frameNames, textvariable = valid.name, width = largeur6)

        tkgrid(chk.changeN, row = 0, column = 0, sticky = 'we', rowspan = 1, columnspan = 2, padx = 1, pady = 1, ipadx = 1, ipady = 1)
        if(tclvalue(valid.change) == '1'){
            tkgrid(txt.changeO, row = 1, column = 0, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
            tkgrid(en.changeO, row = 1, column = 1, sticky = 'w', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
            tkgrid(txt.changeN, row = 2, column = 0, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
            tkgrid(en.changeN, row = 2, column = 1, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
            helpWidget(en.changeN, lang.dlg[['tooltip']][['1']], lang.dlg[['status']][['1']])
        }

        ##########
        tkbind(chk.changeN, "<Button-1>", function(){
            tkdestroy(txt.changeO)
            tkdestroy(en.changeO)
            tkdestroy(txt.changeN)
            tkdestroy(en.changeN)
            if(tclvalue(valid.change) == '0'){
                txt.changeO <<- tklabel(frameNames, text = lang.dlg[['label']][['14']], anchor = 'e', justify = 'right')
                en.changeO <<- tkentry(frameNames, textvariable = valid.obsN, width = largeur4)
                txt.changeN <<- tklabel(frameNames, text = lang.dlg[['label']][['17']], anchor = 'e', justify = 'right')
                en.changeN <<- tkentry(frameNames, textvariable = valid.name, width = largeur6)
                tkgrid(txt.changeO, row = 1, column = 0, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
                tkgrid(en.changeO, row = 1, column = 1, sticky = 'w', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
                tkgrid(txt.changeN, row = 2, column = 0, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
                tkgrid(en.changeN, row = 2, column = 1, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
                helpWidget(en.changeN, lang.dlg[['tooltip']][['1']], lang.dlg[['status']][['1']])
            }
        })

        #####################

        tkgrid(frameGraphObs, row = 0, column = 0, sticky = 'we', rowspan = 1, columnspan = 2, padx = 1, pady = 2, ipadx = 1, ipady = 1)
        tkgrid(frameGraphEst, row = 1, column = 0, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 2, ipadx = 1, ipady = 1)
        tkgrid(frameLegend, row = 1, column = 1, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 2, ipadx = 1, ipady = 1)
        tkgrid(frameNames, row = 2, column = 0, sticky = 'we', rowspan = 1, columnspan = 2, padx = 1, pady = 2, ipadx = 1, ipady = 1)

        #####################

        tkgrid(frameOptIn)
    }

    #####################

    frameGraphXYlim <- tkframe(frDialog)

    ########

    frameGraphXlim <- ttklabelframe(frameGraphXYlim, text = lang.dlg[['label']][['1']], relief = 'groove')

    is.min.xlim <- tclVar(GraphOpt$cdf$xlim$is.min)
    is.max.xlim <- tclVar(GraphOpt$cdf$xlim$is.max)
    min.xlim <- tclVar(GraphOpt$cdf$xlim$min)
    max.xlim <- tclVar(GraphOpt$cdf$xlim$max)

    stateMinXlim <- if(GraphOpt$cdf$xlim$is.min) 'normal' else 'disabled'
    stateMaxXlim <- if(GraphOpt$cdf$xlim$is.max) 'normal' else 'disabled'

    chk.min.Xlim <- tkcheckbutton(frameGraphXlim, variable = is.min.xlim, text = "Min", anchor = 'w', justify = 'left')
    en.min.Xlim <- tkentry(frameGraphXlim, textvariable = min.xlim, width = 5, state = stateMinXlim)
    chk.max.Xlim <- tkcheckbutton(frameGraphXlim, variable = is.max.xlim, text = "Max", anchor = 'w', justify = 'left')
    en.max.Xlim <- tkentry(frameGraphXlim, textvariable = max.xlim, width = 5, state = stateMaxXlim)

    tkgrid(chk.min.Xlim, row = 0, column = 0, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
    tkgrid(en.min.Xlim, row = 0, column = 1, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
    tkgrid(chk.max.Xlim, row = 0, column = 2, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
    tkgrid(en.max.Xlim, row = 0, column = 3, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)

    #########

    tkbind(chk.min.Xlim, "<Button-1>", function(){
        stateMinXlim <- if(tclvalue(is.min.xlim) == '0') 'normal' else 'disabled'
        tkconfigure(en.min.Xlim, state = stateMinXlim)
    })

    tkbind(chk.max.Xlim, "<Button-1>", function(){
        stateMaxXlim <- if(tclvalue(is.max.xlim) == '0') 'normal' else 'disabled'
        tkconfigure(en.max.Xlim, state = stateMaxXlim)
    })

    ###########

    frameGraphYlim <- ttklabelframe(frameGraphXYlim, text = lang.dlg[['label']][['2']], relief = 'groove')

    is.min.ylim <- tclVar(GraphOpt$cdf$ylim$is.min)
    is.max.ylim <- tclVar(GraphOpt$cdf$ylim$is.max)
    min.ylim <- tclVar(GraphOpt$cdf$ylim$min)
    max.ylim <- tclVar(GraphOpt$cdf$ylim$max)

    stateMinYlim <- if(GraphOpt$cdf$ylim$is.min) 'normal' else 'disabled'
    stateMaxYlim <- if(GraphOpt$cdf$ylim$is.max) 'normal' else 'disabled'

    chk.min.Ylim <- tkcheckbutton(frameGraphYlim, variable = is.min.ylim, text = "Min", anchor = 'w', justify = 'left')
    en.min.Ylim <- tkentry(frameGraphYlim, textvariable = min.ylim, width = 5, state = stateMinYlim)
    chk.max.Ylim <- tkcheckbutton(frameGraphYlim, variable = is.max.ylim, text = "Max", anchor = 'w', justify = 'left')
    en.max.Ylim <- tkentry(frameGraphYlim, textvariable = max.ylim, width = 5, state = stateMaxYlim)

    tkgrid(chk.min.Ylim, row = 0, column = 0, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
    tkgrid(en.min.Ylim, row = 0, column = 1, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
    tkgrid(chk.max.Ylim, row = 0, column = 2, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
    tkgrid(en.max.Ylim, row = 0, column = 3, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)

    #########

    tkbind(chk.min.Ylim, "<Button-1>", function(){
        stateMinYlim <- if(tclvalue(is.min.ylim) == '0') 'normal' else 'disabled'
        tkconfigure(en.min.Ylim, state = stateMinYlim)
    })

    tkbind(chk.max.Ylim, "<Button-1>", function(){
        stateMaxYlim <- if(tclvalue(is.max.ylim) == '0') 'normal' else 'disabled'
        tkconfigure(en.max.Ylim, state = stateMaxYlim)
    })

    #####################

    sepXYlim <- tklabel(frameGraphXYlim, text = "", width = largeur3)

    tkgrid(frameGraphXlim, row = 0, column = 0, sticky = 'w')
    tkgrid(sepXYlim, row = 0, column = 1, sticky = 'we')
    tkgrid(frameGraphYlim, row = 0, column = 2, sticky = 'e')

    #####################

    frameGraphAxLabs <- ttklabelframe(frDialog, text = lang.dlg[['label']][['3']], relief = 'groove')

    is.xaxis.lab <- tclVar(GraphOpt$cdf$axislabs$is.xlab)
    is.yaxis.lab <- tclVar(GraphOpt$cdf$axislabs$is.ylab)
    xaxis.lab <- tclVar(GraphOpt$cdf$axislabs$xlab)
    yaxis.lab <- tclVar(GraphOpt$cdf$axislabs$ylab)

    stateXLab <- if(GraphOpt$cdf$axislabs$is.xlab) 'normal' else 'disabled'
    stateYLab <- if(GraphOpt$cdf$axislabs$is.ylab) 'normal' else 'disabled'

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

    frameGraphTitle <- ttklabelframe(frDialog, text = lang.dlg[['label']][['4']], relief = 'groove')

    is.title <- tclVar(GraphOpt$cdf$title$is.title)
    text.title <- tclVar(GraphOpt$cdf$title$title)

    stateGpTlt <- if(GraphOpt$cdf$title$is.title) 'normal' else 'disabled'

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

    framePlotT <- tkframe(frDialog, relief = 'sunken', borderwidth = 2)

    plot.type <- tclVar()
    CbplotTypeVAL <- lang.dlg[['combobox']][['1']]
    plotTypeVAL <- c("multi", "single")
    tclvalue(plot.type) <- CbplotTypeVAL[plotTypeVAL %in% GraphOpt$cdf$plot.type]

    txt.plotT <- tklabel(framePlotT, text = lang.dlg[['label']][['5']], anchor = 'e', justify = 'right')
    cb.plotT <- ttkcombobox(framePlotT, values = CbplotTypeVAL, textvariable = plot.type, width = 15)
    txt.plotT1 <- tklabel(framePlotT, text = '')

    tkgrid(txt.plotT1, row = 0, column = 0, sticky = 'we', rowspan = 1, columnspan = 2)
    tkgrid(txt.plotT, row = 1, column = 0, sticky = 'we', rowspan = 1, columnspan = 1)
    tkgrid(cb.plotT, row = 1, column = 1, sticky = 'we', rowspan = 1, columnspan = 1)

    ############

    tkbind(cb.plotT, "<<ComboboxSelected>>", function(){
        pltType <- plotTypeVAL[CbplotTypeVAL %in% trimws(tclvalue(plot.type))]
        if(pltType == 'multi') plotMultiplePanel() else plotSinglePanel()
    })

    #####################

    framePlotOpt <- tkframe(frDialog)

    ############

    frameOptIn <- tkframe(framePlotOpt)

    plot.type1 <- tclVar(GraphOpt$cdf$plot$obs$type)

    plot.col.line1 <- tclVar(GraphOpt$cdf$plot$obs$line)
    plot.col.points1 <- tclVar(GraphOpt$cdf$plot$obs$points)
    statePlotPoints1 <- if(GraphOpt$cdf$plot$obs$type == 'both') 'normal' else 'disabled'

    add.legend <- tclVar(GraphOpt$cdf$legend$add)

    validName <- ""
    if(length(GraphOpt$cdf$validName$name)){
        validNameOpt <- GraphOpt$cdf$validName$name
        validNameOpt <- validNameOpt[validNameOpt != ""]
        if(length(validNameOpt) > 0){
            validName <- paste0(validNameOpt, collapse = ", ")
        }
    }
    valid.name <- tclVar(validName)

    valid.change <- tclVar(GraphOpt$cdf$validName$change)

    # multi
    plot.type2 <- tclVar(GraphOpt$cdf$plot$est$type)
    plot.col.line2 <- tclVar(GraphOpt$cdf$plot$est$line)
    plot.col.points2 <- tclVar(GraphOpt$cdf$plot$est$points)
    statePlotPoints2 <- if(GraphOpt$cdf$plot$est$type == 'both') 'normal' else 'disabled'
    obs.legend <- tclVar(GraphOpt$cdf$legend$obs)
    est.legend <- tclVar(GraphOpt$cdf$legend$est)
    # single
    custom.color <- tclVar(GraphOpt$cdf$plot1$custom)
    stateKol2 <- if(GraphOpt$cdf$plot1$custom) "normal" else "disabled"
    valid.obsN <- tclVar(GraphOpt$cdf$validName$obs)

    spin.pltPointS1 <- NULL
    spin.pltPointS2 <- NULL
    spin.pltPointT1 <- NULL
    spin.pltPointT2 <- NULL
    spin.pltLineW1 <- NULL
    spin.pltLineW2 <- NULL

    if(GraphOpt$cdf$plot.type == 'multi') plotMultiplePanel() else plotSinglePanel()

    #####################

    tkgrid(frameGraphXYlim, row = 0, column = 0, sticky = '', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
    tkgrid(frameGraphAxLabs, row = 1, column = 0, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 2, ipadx = 1, ipady = 1)
    tkgrid(frameGraphTitle, row = 2, column = 0, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 2, ipadx = 1, ipady = 1)
    tkgrid(framePlotT, row = 3, column = 0, sticky = '', rowspan = 1, columnspan = 1, padx = 1, pady = 2, ipadx = 1, ipady = 1)
    tkgrid(framePlotOpt, row = 4, column = 0, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 2, ipadx = 1, ipady = 1)

    #####################
    bt.opt.OK <- ttkbutton(frButt, text = .cdtEnv$tcl$lang$global[['button']][['1']])
    bt.opt.CA <- ttkbutton(frButt, text = .cdtEnv$tcl$lang$global[['button']][['2']])

    tkconfigure(bt.opt.OK, command = function(){
        GraphOpt$cdf$xlim$is.min <<- switch(tclvalue(is.min.xlim), '0' = FALSE, '1' = TRUE)
        GraphOpt$cdf$xlim$is.max <<- switch(tclvalue(is.max.xlim), '0' = FALSE, '1' = TRUE)
        GraphOpt$cdf$xlim$min <<- as.numeric(trimws(tclvalue(min.xlim)))
        GraphOpt$cdf$xlim$max <<- as.numeric(trimws(tclvalue(max.xlim)))

        GraphOpt$cdf$ylim$is.min <<- switch(tclvalue(is.min.ylim), '0' = FALSE, '1' = TRUE)
        GraphOpt$cdf$ylim$is.max <<- switch(tclvalue(is.max.ylim), '0' = FALSE, '1' = TRUE)
        GraphOpt$cdf$ylim$min <<- as.numeric(trimws(tclvalue(min.ylim)))
        GraphOpt$cdf$ylim$max <<- as.numeric(trimws(tclvalue(max.ylim)))

        GraphOpt$cdf$axislabs$is.xlab <<- switch(tclvalue(is.xaxis.lab), '0' = FALSE, '1' = TRUE)
        GraphOpt$cdf$axislabs$is.ylab <<- switch(tclvalue(is.yaxis.lab), '0' = FALSE, '1' = TRUE)
        GraphOpt$cdf$axislabs$xlab <<- gsub('\\\\n', '\n', trimws(tclvalue(xaxis.lab)))
        GraphOpt$cdf$axislabs$ylab <<- gsub('\\\\n', '\n', trimws(tclvalue(yaxis.lab)))

        GraphOpt$cdf$title$is.title <<- switch(tclvalue(is.title), '0' = FALSE, '1' = TRUE)
        GraphOpt$cdf$title$title <<- gsub('\\\\n', '\n', trimws(tclvalue(text.title)))

        GraphOpt$cdf$plot$obs$type <<- trimws(tclvalue(plot.type1))
        GraphOpt$cdf$plot$obs$line <<- tclvalue(plot.col.line1)
        GraphOpt$cdf$plot$obs$lwd <<- as.numeric(trimws(tclvalue(tkget(spin.pltLineW1))))
        GraphOpt$cdf$plot$obs$points <<- tclvalue(plot.col.points1)
        GraphOpt$cdf$plot$obs$cex <<- as.numeric(trimws(tclvalue(tkget(spin.pltPointS1))))
        GraphOpt$cdf$plot$obs$pch <<- as.numeric(trimws(tclvalue(tkget(spin.pltPointT1))))

        GraphOpt$cdf$legend$add <<- switch(tclvalue(add.legend), '0' = FALSE, '1' = TRUE)
        GraphOpt$cdf$plot.type <<- plotTypeVAL[CbplotTypeVAL %in% trimws(tclvalue(plot.type))]

        GraphOpt$cdf$validName$change <<- switch(tclvalue(valid.change), '0' = FALSE, '1' = TRUE)

        if(GraphOpt$cdf$plot.type == 'multi'){
            GraphOpt$cdf$plot$est$type <<- trimws(tclvalue(plot.type2))
            GraphOpt$cdf$plot$est$line <<- tclvalue(plot.col.line2)
            GraphOpt$cdf$plot$est$lwd <<- as.numeric(trimws(tclvalue(tkget(spin.pltLineW2))))
            GraphOpt$cdf$plot$est$points <<- tclvalue(plot.col.points2)
            GraphOpt$cdf$plot$est$cex <<- as.numeric(trimws(tclvalue(tkget(spin.pltPointS2))))
            GraphOpt$cdf$plot$est$pch <<- as.numeric(trimws(tclvalue(tkget(spin.pltPointT2))))

            GraphOpt$cdf$legend$obs <<- trimws(tclvalue(obs.legend))
            GraphOpt$cdf$legend$est <<- trimws(tclvalue(est.legend))
        }else{
            GraphOpt$cdf$plot1$custom <<- switch(tclvalue(custom.color), '0' = FALSE, '1' = TRUE)
            if(GraphOpt$cdf$validName$change)
                GraphOpt$cdf$validName$obs <<- trimws(tclvalue(valid.obsN))
        }

        if(GraphOpt$cdf$validName$change){
            validName <- trimws(tclvalue(valid.name))
            validName <- strsplit(validName, ",")[[1]]
            validName <- trimws(validName)
            validName <- validName[validName != ""]
            if(length(validName) == 0) validName <- ""
            GraphOpt$cdf$validName$name <<- validName
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

#######################################################################################################

Validation.GraphOptions1.Lines <- function(GraphOpt, parent.win = .cdtEnv$tcl$main$win){
    if(WindowsOS()){
        largeur0 <- 350
        largeur1 <- 63
        largeur2 <- 67
        largeur3 <- 1
        largeur4 <- 35
        largeur5 <- 70
        largeur6 <- 56
        width.col <- 3
        width.spin <- 4
    }else{
        largeur0 <- 370
        largeur1 <- 60
        largeur2 <- 64
        largeur3 <- 1
        largeur4 <- 33
        largeur5 <- 66
        largeur6 <- 53
        width.col <- 1
        width.spin <- 4
    }

    #####################
    xml.dlg <- file.path(.cdtDir$dirLocal, "languages", "cdtGraphOptions.Lines_Valid1_dlgBox.xml")
    lang.dlg <- cdtLanguageParse(xml.dlg, .cdtData$Config$lang.iso)

    #####################
    tt <- tktoplevel()
    tkgrab.set(tt)
    tkfocus(tt)

    #####################
    frDialog <- tkframe(tt, relief = 'raised', borderwidth = 2)
    frButt <- tkframe(tt)

    #####################

    plotMultiplePanel <- function(){
        tkdestroy(frameOptIn)
        frameOptIn <<- tkframe(framePlotOpt)

        #########
        frameGraphObs <- ttklabelframe(frameOptIn, text = lang.dlg[['label']][['14']], relief = 'groove')

        txt.pltType1 <- tklabel(frameGraphObs, text = lang.dlg[['label']][['6']], anchor = 'e', justify = 'right')
        cb.pltType1 <- ttkcombobox(frameGraphObs, values = c('both', 'line'), textvariable = plot.type1, width = 4)

        #########
        tkbind(cb.pltType1, "<<ComboboxSelected>>", function(){
            statePlotPoints1 <- if(tclvalue(plot.type1) == 'both') 'normal' else 'disabled'
            tkconfigure(bt.pltPointC1, state = statePlotPoints1)
            tkconfigure(spin.pltPointS1, state = statePlotPoints1)
            tkconfigure(spin.pltPointT1, state = statePlotPoints1)
        })

        ########
        framepltLine1 <- ttklabelframe(frameGraphObs, text = lang.dlg[['label']][['7']], relief = 'groove')

        txt.pltLineC1 <- tklabel(framepltLine1, text = lang.dlg[['label']][['9']], anchor = 'e', justify = 'right')
        bt.pltLineC1 <- tkbutton(framepltLine1, bg = tclvalue(plot.col.line1), width = width.col)
        txt.pltLineW1 <- tklabel(framepltLine1, text = lang.dlg[['label']][['10']], anchor = 'e', justify = 'right')
        spin.pltLineW1 <<- ttkspinbox(framepltLine1, from = 0.5, to = 4, increment = 0.1, justify = 'center', width = width.spin)
        tkset(spin.pltLineW1, GraphOpt$line$plot$obs$lwd)

        tkgrid(txt.pltLineC1, bt.pltLineC1, txt.pltLineW1, spin.pltLineW1)

        ########
        tkconfigure(bt.pltLineC1, command = function(){
            tcl('wm', 'attributes', tt, topmost = FALSE)
            loko <- trimws(tclvalue(tcl("tk_chooseColor", initialcolor = tclvalue(plot.col.line1), title = lang.dlg[['label']][['13']])))
            tcl('wm', 'attributes', tt, topmost = TRUE)
            if(nchar(loko) > 0){
                tkconfigure(bt.pltLineC1, bg = loko)
                tclvalue(plot.col.line1) <- loko
            }
        })

        ########
        framepltPoints1 <- ttklabelframe(frameGraphObs, text = lang.dlg[['label']][['8']], relief = 'groove')

        txt.pltPointC1 <- tklabel(framepltPoints1, text = lang.dlg[['label']][['9']], anchor = 'e', justify = 'right')
        bt.pltPointC1 <- tkbutton(framepltPoints1, bg = tclvalue(plot.col.points1), width = width.col, state = statePlotPoints1)
        txt.pltPointT1 <- tklabel(framepltPoints1, text = lang.dlg[['label']][['11']], anchor = 'e', justify = 'right')
        spin.pltPointT1 <<- ttkspinbox(framepltPoints1, from = 21, to = 25, increment = 1, justify = 'center', width = width.spin, state = statePlotPoints1)
        tkset(spin.pltPointT1, GraphOpt$line$plot$obs$pch)
        txt.pltPointS1 <- tklabel(framepltPoints1, text = lang.dlg[['label']][['12']], anchor = 'e', justify = 'right')
        spin.pltPointS1 <<- ttkspinbox(framepltPoints1, from = 0.5, to = 2.5, increment = 0.1, justify = 'center', width = width.spin, state = statePlotPoints1)
        tkset(spin.pltPointS1, GraphOpt$line$plot$obs$cex)

        tkgrid(txt.pltPointC1, bt.pltPointC1, txt.pltPointT1, spin.pltPointT1, txt.pltPointS1, spin.pltPointS1)

        ########

        tkconfigure(bt.pltPointC1, command = function(){
            tcl('wm', 'attributes', tt, topmost = FALSE)
            loko <- trimws(tclvalue(tcl("tk_chooseColor", initialcolor = tclvalue(plot.col.points1), title = lang.dlg[['label']][['13']])))
            tcl('wm', 'attributes', tt, topmost = TRUE)
            if(nchar(loko) > 0){
                tkconfigure(bt.pltPointC1, bg = loko)
                tclvalue(plot.col.points1) <- loko
            }
        })

        ##########

        tkgrid(txt.pltType1, row = 0, column = 0)
        tkgrid(cb.pltType1, row = 0, column = 1)
        tkgrid(framepltLine1, row = 0, column = 2, padx = 2)
        tkgrid(framepltPoints1, row = 0, column = 3)

        #####################

        frameGraphEst <- ttklabelframe(frameOptIn, text = lang.dlg[['label']][['15']], relief = 'groove')

        txt.pltType2 <- tklabel(frameGraphEst, text = lang.dlg[['label']][['6']], anchor = 'e', justify = 'right')
        cb.pltType2 <- ttkcombobox(frameGraphEst, values = c('both', 'line'), textvariable = plot.type2, width = 4)

        #########
        tkbind(cb.pltType2, "<<ComboboxSelected>>", function(){
            statePlotPoints2 <- if(tclvalue(plot.type2) == 'both') 'normal' else 'disabled'
            tkconfigure(bt.pltPointC2, state = statePlotPoints2)
            tkconfigure(spin.pltPointS2, state = statePlotPoints2)
            tkconfigure(spin.pltPointT2, state = statePlotPoints2)
        })

        ########
        framepltLine2 <- ttklabelframe(frameGraphEst, text = lang.dlg[['label']][['7']], relief = 'groove')

        txt.pltLineC2 <- tklabel(framepltLine2, text = lang.dlg[['label']][['9']], anchor = 'e', justify = 'right')
        bt.pltLineC2 <- tkbutton(framepltLine2, bg = tclvalue(plot.col.line2), width = width.col)
        txt.pltLineW2 <- tklabel(framepltLine2, text = lang.dlg[['label']][['10']], anchor = 'e', justify = 'right')
        spin.pltLineW2 <<- ttkspinbox(framepltLine2, from = 0.5, to = 4, increment = 0.1, justify = 'center', width = width.spin)
        tkset(spin.pltLineW2, GraphOpt$line$plot$est$lwd)

        tkgrid(txt.pltLineC2, bt.pltLineC2, txt.pltLineW2, spin.pltLineW2)

        ########
        tkconfigure(bt.pltLineC2, command = function(){
            tcl('wm', 'attributes', tt, topmost = FALSE)
            loko <- trimws(tclvalue(tcl("tk_chooseColor", initialcolor = tclvalue(plot.col.line2), title = lang.dlg[['label']][['13']])))
            tcl('wm', 'attributes', tt, topmost = TRUE)
            if(nchar(loko) > 0){
                tkconfigure(bt.pltLineC2, bg = loko)
                tclvalue(plot.col.line2) <- loko
            }
        })

        ########
        framepltPoints2 <- ttklabelframe(frameGraphEst, text = lang.dlg[['label']][['8']], relief = 'groove')

        txt.pltPointC2 <- tklabel(framepltPoints2, text = lang.dlg[['label']][['9']], anchor = 'e', justify = 'right')
        bt.pltPointC2 <- tkbutton(framepltPoints2, bg = tclvalue(plot.col.points2), width = width.col, state = statePlotPoints2)
        txt.pltPointT2 <- tklabel(framepltPoints2, text = lang.dlg[['label']][['11']], anchor = 'e', justify = 'right')
        spin.pltPointT2 <<- ttkspinbox(framepltPoints2, from = 21, to = 25, increment = 1, justify = 'center', width = width.spin, state = statePlotPoints2)
        tkset(spin.pltPointT2, GraphOpt$line$plot$est$pch)
        txt.pltPointS2 <- tklabel(framepltPoints2, text = lang.dlg[['label']][['12']], anchor = 'e', justify = 'right')
        spin.pltPointS2 <<- ttkspinbox(framepltPoints2, from = 0.5, to = 2.5, increment = 0.1, justify = 'center', width = width.spin, state = statePlotPoints2)
        tkset(spin.pltPointS2, GraphOpt$line$plot$est$cex)

        tkgrid(txt.pltPointC2, bt.pltPointC2, txt.pltPointT2, spin.pltPointT2, txt.pltPointS2, spin.pltPointS2)

        ########

        tkconfigure(bt.pltPointC2, command = function(){
            tcl('wm', 'attributes', tt, topmost = FALSE)
            loko <- trimws(tclvalue(tcl("tk_chooseColor", initialcolor = tclvalue(plot.col.points2), title = lang.dlg[['label']][['13']])))
            tcl('wm', 'attributes', tt, topmost = TRUE)
            if(nchar(loko) > 0){
                tkconfigure(bt.pltPointC2, bg = loko)
                tclvalue(plot.col.points2) <- loko
            }
        })

        ###########

        tkgrid(txt.pltType2, row = 0, column = 0)
        tkgrid(cb.pltType2, row = 0, column = 1)
        tkgrid(framepltLine2, row = 0, column = 2, padx = 2)
        tkgrid(framepltPoints2, row = 0, column = 3)

        #####################

        frameLegend <- ttklabelframe(frameOptIn, text = lang.dlg[['label']][['16']], relief = 'groove')

        stateLeg <- if(tclvalue(add.legend) == '1') 'normal' else 'disabled'

        chk.Leg <- tkcheckbutton(frameLegend, variable = add.legend, text = lang.dlg[['checkbutton']][['1']], anchor = 'w', justify = 'left')
        txt.Leg1 <- tklabel(frameLegend, text = lang.dlg[['label']][['14']], anchor = 'w', justify = 'left')
        en.Leg1 <- tkentry(frameLegend, textvariable = obs.legend, width = largeur4, state = stateLeg)
        txt.Leg2 <- tklabel(frameLegend, text = lang.dlg[['label']][['15']], anchor = 'w', justify = 'left')
        en.Leg2 <- tkentry(frameLegend, textvariable = est.legend, width = largeur4, state = stateLeg)

        tkgrid(chk.Leg, row = 0, column = 0, sticky = 'we', columnspan = 4)
        tkgrid(txt.Leg1, row = 1, column = 0, sticky = 'w', columnspan = 1)
        tkgrid(en.Leg1, row = 2, column = 0, sticky = 'we', columnspan = 1)
        tkgrid(txt.Leg2, row = 1, column = 1, sticky = 'w', columnspan = 1)
        tkgrid(en.Leg2, row = 2, column = 1, sticky = 'we', columnspan = 1)

        #########

        tkbind(chk.Leg, "<Button-1>", function(){
            stateLeg <- if(tclvalue(add.legend) == '0') 'normal' else 'disabled'
            tkconfigure(en.Leg1, state = stateLeg)
            tkconfigure(en.Leg2, state = stateLeg)
        })

        #####################

        frameNames <- tkframe(frameOptIn, relief = 'groove', borderwidth = 2)

        chk.changeN <- tkcheckbutton(frameNames, variable = valid.change, text = lang.dlg[['checkbutton']][['3']], anchor = 'w', justify = 'left')
        en.changeN <- tkentry(frameNames, textvariable = valid.name, width = largeur5)

        tkgrid(chk.changeN, row = 0, column = 0, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
        if(tclvalue(valid.change) == '1'){
            tkgrid(en.changeN, row = 1, column = 0, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
            helpWidget(en.changeN, lang.dlg[['tooltip']][['2']], lang.dlg[['status']][['2']])
        }

        ##########
        tkbind(chk.changeN, "<Button-1>", function(){
            tkdestroy(en.changeN)
            if(tclvalue(valid.change) == '0'){
                en.changeN <<- tkentry(frameNames, textvariable = valid.name, width = largeur5)
                tkgrid(en.changeN, row = 1, column = 0, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
                helpWidget(en.changeN, lang.dlg[['tooltip']][['2']], lang.dlg[['status']][['2']])
            }
        })

        #####################

        tkgrid(frameGraphObs, row = 0, column = 0, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 2, ipadx = 1, ipady = 1)
        tkgrid(frameGraphEst, row = 1, column = 0, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 2, ipadx = 1, ipady = 1)
        tkgrid(frameLegend, row = 2, column = 0, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 2, ipadx = 1, ipady = 1)
        tkgrid(frameNames, row = 3, column = 0, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 2, ipadx = 1, ipady = 1)

        #####################

        tkgrid(frameOptIn)
    }

    ########

    plotSinglePanel <- function(){
        tkdestroy(frameOptIn)
        frameOptIn <<- tkframe(framePlotOpt)

        #########
        frameGraphObs <- ttklabelframe(frameOptIn, text = lang.dlg[['label']][['14']], relief = 'groove')

        txt.pltType1 <- tklabel(frameGraphObs, text = lang.dlg[['label']][['6']], anchor = 'e', justify = 'right')
        cb.pltType1 <- ttkcombobox(frameGraphObs, values = c('both', 'line'), textvariable = plot.type1, width = 4)

        #########
        tkbind(cb.pltType1, "<<ComboboxSelected>>", function(){
            statePlotPoints1 <- if(tclvalue(plot.type1) == 'both') 'normal' else 'disabled'
            tkconfigure(bt.pltPointC1, state = statePlotPoints1)
            tkconfigure(spin.pltPointS1, state = statePlotPoints1)
            tkconfigure(spin.pltPointT1, state = statePlotPoints1)
        })

        ########
        framepltLine1 <- ttklabelframe(frameGraphObs, text = lang.dlg[['label']][['7']], relief = 'groove')

        txt.pltLineC1 <- tklabel(framepltLine1, text = lang.dlg[['label']][['9']], anchor = 'e', justify = 'right')
        bt.pltLineC1 <- tkbutton(framepltLine1, bg = tclvalue(plot.col.line1), width = width.col)
        txt.pltLineW1 <- tklabel(framepltLine1, text = lang.dlg[['label']][['10']], anchor = 'e', justify = 'right')
        spin.pltLineW1 <<- ttkspinbox(framepltLine1, from = 0.5, to = 4, increment = 0.1, justify = 'center', width = width.spin)
        tkset(spin.pltLineW1, GraphOpt$line$plot$obs$lwd)

        tkgrid(txt.pltLineC1, bt.pltLineC1, txt.pltLineW1, spin.pltLineW1)

        ########
        tkconfigure(bt.pltLineC1, command = function(){
            tcl('wm', 'attributes', tt, topmost = FALSE)
            loko <- trimws(tclvalue(tcl("tk_chooseColor", initialcolor = tclvalue(plot.col.line1), title = lang.dlg[['label']][['13']])))
            tcl('wm', 'attributes', tt, topmost = TRUE)
            if(nchar(loko) > 0){
                tkconfigure(bt.pltLineC1, bg = loko)
                tclvalue(plot.col.line1) <- loko
            }
        })

        ########
        framepltPoints1 <- ttklabelframe(frameGraphObs, text = lang.dlg[['label']][['8']], relief = 'groove')

        txt.pltPointC1 <- tklabel(framepltPoints1, text = lang.dlg[['label']][['9']], anchor = 'e', justify = 'right')
        bt.pltPointC1 <- tkbutton(framepltPoints1, bg = tclvalue(plot.col.points1), width = width.col, state = statePlotPoints1)
        txt.pltPointT1 <- tklabel(framepltPoints1, text = lang.dlg[['label']][['11']], anchor = 'e', justify = 'right')
        spin.pltPointT1 <<- ttkspinbox(framepltPoints1, from = 21, to = 25, increment = 1, justify = 'center', width = width.spin, state = statePlotPoints1)
        tkset(spin.pltPointT1, GraphOpt$line$plot$obs$pch)
        txt.pltPointS1 <- tklabel(framepltPoints1, text = lang.dlg[['label']][['12']], anchor = 'e', justify = 'right')
        spin.pltPointS1 <<- ttkspinbox(framepltPoints1, from = 0.5, to = 2.5, increment = 0.1, justify = 'center', width = width.spin, state = statePlotPoints1)
        tkset(spin.pltPointS1, GraphOpt$line$plot$obs$cex)

        tkgrid(txt.pltPointC1, bt.pltPointC1, txt.pltPointT1, spin.pltPointT1, txt.pltPointS1, spin.pltPointS1)

        ########

        tkconfigure(bt.pltPointC1, command = function(){
            tcl('wm', 'attributes', tt, topmost = FALSE)
            loko <- trimws(tclvalue(tcl("tk_chooseColor", initialcolor = tclvalue(plot.col.points1), title = lang.dlg[['label']][['13']])))
            tcl('wm', 'attributes', tt, topmost = TRUE)
            if(nchar(loko) > 0){
                tkconfigure(bt.pltPointC1, bg = loko)
                tclvalue(plot.col.points1) <- loko
            }
        })

        ##########

        tkgrid(txt.pltType1, row = 0, column = 0)
        tkgrid(cb.pltType1, row = 0, column = 1)
        tkgrid(framepltLine1, row = 0, column = 2, padx = 2)
        tkgrid(framepltPoints1, row = 0, column = 3)

        #####################

        frameGraphEst <- ttklabelframe(frameOptIn, text = lang.dlg[['label']][['15']], relief = 'groove')

        default.Kol <- grDevices::rainbow(4)

        preview.canvasf <- function(kolKey, cond){
            if(cond){
                kolor <- getGradientColor(kolKey, 0:largeur0)
                tkdelete(canvas.preview, 'gradlines0')
                for(i in 0:largeur0)
                    tkcreate(canvas.preview, "line", i, 0, i, 20, fill = kolor[i], tags = 'gradlines0')
            }else{
                tkdelete(canvas.preview, 'gradlines0')
            }
        }

        #########

        chk.userKol <- tkcheckbutton(frameGraphEst, variable = custom.color, text = lang.dlg[['checkbutton']][['2']], anchor = 'w', justify = 'left')
        bt.userKol <- ttkbutton(frameGraphEst, text = lang.dlg[['button']][['1']], state = stateKol2)
        canvas.preview <- tkcanvas(frameGraphEst, width = largeur0, height = 20, bg = 'white')

        ## Preview Color
        kolKey <- if(tclvalue(custom.color) == '1') GraphOpt$line$plot1$est else default.Kol
        preview.canvasf(kolKey, !is.null(kolKey) & length(kolKey) > 0)

        tkgrid(chk.userKol, row = 0, column = 0, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
        tkgrid(bt.userKol, row = 0, column = 1, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
        tkgrid(canvas.preview, row = 1, column = 0, sticky = 'we', rowspan = 1, columnspan = 2, padx = 1, pady = 1, ipadx = 1, ipady = 1)

        #########

        tkconfigure(bt.userKol, command = function(){
            tcl('wm', 'attributes', tt, topmost = FALSE)
            GraphOpt$line$plot1$est <<- createColorkey(tt, GraphOpt$line$plot1$estl)
            tcl('wm', 'attributes', tt, topmost = TRUE)
            preview.canvasf(GraphOpt$line$plot1$est, !is.null(GraphOpt$line$plot1$est))
        })

        tkbind(chk.userKol, "<Button-1>", function(){
            stateKol2 <- if(tclvalue(custom.color) == '0') 'normal' else 'disabled'
            tkconfigure(bt.userKol, state = stateKol2)

            kolKey <- if(tclvalue(custom.color) == '0') GraphOpt$line$plot1$est else default.Kol
            preview.canvasf(kolKey, !is.null(kolKey))
        })

        #####################

        frameLegend <- ttklabelframe(frameOptIn, text = lang.dlg[['label']][['16']], relief = 'groove')

        txt.Leg <- tklabel(frameLegend, text = '')
        chk.Leg <- tkcheckbutton(frameLegend, variable = add.legend, text = lang.dlg[['checkbutton']][['1']], anchor = 'w', justify = 'left')

        tkgrid(txt.Leg)
        tkgrid(chk.Leg)

        #####################

        frameNames <- tkframe(frameOptIn, relief = 'groove', borderwidth = 2)

        chk.changeN <- tkcheckbutton(frameNames, variable = valid.change, text = lang.dlg[['checkbutton']][['3']], anchor = 'w', justify = 'left')
        txt.changeO <- tklabel(frameNames, text = lang.dlg[['label']][['14']], anchor = 'e', justify = 'right')
        en.changeO <- tkentry(frameNames, textvariable = valid.obsN, width = largeur4)
        txt.changeN <- tklabel(frameNames, text = lang.dlg[['label']][['17']], anchor = 'e', justify = 'right')
        en.changeN <- tkentry(frameNames, textvariable = valid.name, width = largeur6)

        tkgrid(chk.changeN, row = 0, column = 0, sticky = 'we', rowspan = 1, columnspan = 2, padx = 1, pady = 1, ipadx = 1, ipady = 1)
        if(tclvalue(valid.change) == '1'){
            tkgrid(txt.changeO, row = 1, column = 0, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
            tkgrid(en.changeO, row = 1, column = 1, sticky = 'w', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
            tkgrid(txt.changeN, row = 2, column = 0, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
            tkgrid(en.changeN, row = 2, column = 1, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
            helpWidget(en.changeN, lang.dlg[['tooltip']][['2']], lang.dlg[['status']][['2']])
        }

        ##########
        tkbind(chk.changeN, "<Button-1>", function(){
            tkdestroy(txt.changeO)
            tkdestroy(en.changeO)
            tkdestroy(txt.changeN)
            tkdestroy(en.changeN)
            if(tclvalue(valid.change) == '0'){
                txt.changeO <<- tklabel(frameNames, text = lang.dlg[['label']][['14']], anchor = 'e', justify = 'right')
                en.changeO <<- tkentry(frameNames, textvariable = valid.obsN, width = largeur4)
                txt.changeN <<- tklabel(frameNames, text = lang.dlg[['label']][['17']], anchor = 'e', justify = 'right')
                en.changeN <<- tkentry(frameNames, textvariable = valid.name, width = largeur6)
                tkgrid(txt.changeO, row = 1, column = 0, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
                tkgrid(en.changeO, row = 1, column = 1, sticky = 'w', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
                tkgrid(txt.changeN, row = 2, column = 0, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
                tkgrid(en.changeN, row = 2, column = 1, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
                helpWidget(en.changeN, lang.dlg[['tooltip']][['2']], lang.dlg[['status']][['2']])
            }
        })

        #####################

        tkgrid(frameGraphObs, row = 0, column = 0, sticky = 'we', rowspan = 1, columnspan = 2, padx = 1, pady = 2, ipadx = 1, ipady = 1)
        tkgrid(frameGraphEst, row = 1, column = 0, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 2, ipadx = 1, ipady = 1)
        tkgrid(frameLegend, row = 1, column = 1, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 2, ipadx = 1, ipady = 1)
        tkgrid(frameNames, row = 2, column = 0, sticky = 'we', rowspan = 1, columnspan = 2, padx = 1, pady = 2, ipadx = 1, ipady = 1)

        #####################

        tkgrid(frameOptIn)
    }

    #####################

    frameGraphXYlim <- tkframe(frDialog)

    ########

    frameGraphXlim <- ttklabelframe(frameGraphXYlim, text = lang.dlg[['label']][['1']], relief = 'groove')

    is.min.xlim <- tclVar(GraphOpt$line$xlim$is.min)
    is.max.xlim <- tclVar(GraphOpt$line$xlim$is.max)
    min.xlim <- tclVar(GraphOpt$line$xlim$min)
    max.xlim <- tclVar(GraphOpt$line$xlim$max)

    stateMinXlim <- if(GraphOpt$line$xlim$is.min) 'normal' else 'disabled'
    stateMaxXlim <- if(GraphOpt$line$xlim$is.max) 'normal' else 'disabled'

    chk.min.Xlim <- tkcheckbutton(frameGraphXlim, variable = is.min.xlim, text = "Min", anchor = 'w', justify = 'left')
    en.min.Xlim <- tkentry(frameGraphXlim, textvariable = min.xlim, width = 11, state = stateMinXlim)
    chk.max.Xlim <- tkcheckbutton(frameGraphXlim, variable = is.max.xlim, text = "Max", anchor = 'w', justify = 'left')
    en.max.Xlim <- tkentry(frameGraphXlim, textvariable = max.xlim, width = 11, state = stateMaxXlim)

    tkgrid(chk.min.Xlim, row = 0, column = 0, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
    tkgrid(en.min.Xlim, row = 0, column = 1, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
    tkgrid(chk.max.Xlim, row = 0, column = 2, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
    tkgrid(en.max.Xlim, row = 0, column = 3, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)

    helpWidget(en.min.Xlim, lang.dlg[['tooltip']][['1']], lang.dlg[['status']][['1']])
    helpWidget(en.max.Xlim, lang.dlg[['tooltip']][['1']], lang.dlg[['status']][['1']])

    #########

    tkbind(chk.min.Xlim, "<Button-1>", function(){
        stateMinXlim <- if(tclvalue(is.min.xlim) == '0') 'normal' else 'disabled'
        tkconfigure(en.min.Xlim, state = stateMinXlim)
    })

    tkbind(chk.max.Xlim, "<Button-1>", function(){
        stateMaxXlim <- if(tclvalue(is.max.xlim) == '0') 'normal' else 'disabled'
        tkconfigure(en.max.Xlim, state = stateMaxXlim)
    })

    #########

    frameGraphYlim <- ttklabelframe(frameGraphXYlim, text = lang.dlg[['label']][['2']], relief = 'groove')

    is.min.ylim <- tclVar(GraphOpt$line$ylim$is.min)
    is.max.ylim <- tclVar(GraphOpt$line$ylim$is.max)
    min.ylim <- tclVar(GraphOpt$line$ylim$min)
    max.ylim <- tclVar(GraphOpt$line$ylim$max)

    stateMinYlim <- if(GraphOpt$line$ylim$is.min) 'normal' else 'disabled'
    stateMaxYlim <- if(GraphOpt$line$ylim$is.max) 'normal' else 'disabled'

    chk.min.Ylim <- tkcheckbutton(frameGraphYlim, variable = is.min.ylim, text = "Min", anchor = 'w', justify = 'left')
    en.min.Ylim <- tkentry(frameGraphYlim, textvariable = min.ylim, width = 5, state = stateMinYlim)
    chk.max.Ylim <- tkcheckbutton(frameGraphYlim, variable = is.max.ylim, text = "Max", anchor = 'w', justify = 'left')
    en.max.Ylim <- tkentry(frameGraphYlim, textvariable = max.ylim, width = 5, state = stateMaxYlim)

    tkgrid(chk.min.Ylim, row = 0, column = 0, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
    tkgrid(en.min.Ylim, row = 0, column = 1, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
    tkgrid(chk.max.Ylim, row = 0, column = 2, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
    tkgrid(en.max.Ylim, row = 0, column = 3, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)

    #########

    tkbind(chk.min.Ylim, "<Button-1>", function(){
        stateMinYlim <- if(tclvalue(is.min.ylim) == '0') 'normal' else 'disabled'
        tkconfigure(en.min.Ylim, state = stateMinYlim)
    })

    tkbind(chk.max.Ylim, "<Button-1>", function(){
        stateMaxYlim <- if(tclvalue(is.max.ylim) == '0') 'normal' else 'disabled'
        tkconfigure(en.max.Ylim, state = stateMaxYlim)
    })

    #####################

    sepXYlim <- tklabel(frameGraphXYlim, text = "", width = largeur3)

    tkgrid(frameGraphXlim, row = 0, column = 0, sticky = 'w')
    tkgrid(sepXYlim, row = 0, column = 1, sticky = 'we')
    tkgrid(frameGraphYlim, row = 0, column = 2, sticky = 'e')

    #####################

    frameGraphAxLabs <- ttklabelframe(frDialog, text = lang.dlg[['label']][['3']], relief = 'groove')

    is.xaxis.lab <- tclVar(GraphOpt$line$axislabs$is.xlab)
    is.yaxis.lab <- tclVar(GraphOpt$line$axislabs$is.ylab)
    xaxis.lab <- tclVar(GraphOpt$line$axislabs$xlab)
    yaxis.lab <- tclVar(GraphOpt$line$axislabs$ylab)

    stateXLab <- if(GraphOpt$line$axislabs$is.xlab) 'normal' else 'disabled'
    stateYLab <- if(GraphOpt$line$axislabs$is.ylab) 'normal' else 'disabled'

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

    frameGraphTitle <- ttklabelframe(frDialog, text = lang.dlg[['label']][['4']], relief = 'groove')

    is.title <- tclVar(GraphOpt$line$title$is.title)
    text.title <- tclVar(GraphOpt$line$title$title)

    stateGpTlt <- if(GraphOpt$line$title$is.title) 'normal' else 'disabled'

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

    framePlotT <- tkframe(frDialog, relief = 'sunken', borderwidth = 2)

    plot.type <- tclVar()
    CbplotTypeVAL <- lang.dlg[['combobox']][['1']]
    plotTypeVAL <- c("multi", "single")
    tclvalue(plot.type) <- CbplotTypeVAL[plotTypeVAL %in% GraphOpt$line$plot.type]

    txt.plotT <- tklabel(framePlotT, text = lang.dlg[['label']][['5']], anchor = 'e', justify = 'right')
    cb.plotT <- ttkcombobox(framePlotT, values = CbplotTypeVAL, textvariable = plot.type, width = 15)
    txt.plotT1 <- tklabel(framePlotT, text = '')

    tkgrid(txt.plotT1, row = 0, column = 0, sticky = 'we', rowspan = 1, columnspan = 2)
    tkgrid(txt.plotT, row = 1, column = 0, sticky = 'we', rowspan = 1, columnspan = 1)
    tkgrid(cb.plotT, row = 1, column = 1, sticky = 'we', rowspan = 1, columnspan = 1)

    ############

    tkbind(cb.plotT, "<<ComboboxSelected>>", function(){
        pltType <- plotTypeVAL[CbplotTypeVAL %in% trimws(tclvalue(plot.type))]
        if(pltType == 'multi') plotMultiplePanel() else plotSinglePanel()
    })

    #####################

    framePlotOpt <- tkframe(frDialog)

    ############

    frameOptIn <- tkframe(framePlotOpt)

    plot.type1 <- tclVar(GraphOpt$line$plot$obs$type)

    plot.col.line1 <- tclVar(GraphOpt$line$plot$obs$line)
    plot.col.points1 <- tclVar(GraphOpt$line$plot$obs$points)
    statePlotPoints1 <- if(GraphOpt$line$plot$obs$type == 'both') 'normal' else 'disabled'

    add.legend <- tclVar(GraphOpt$line$legend$add)

    validName <- ""
    if(length(GraphOpt$line$validName$name) > 1){
        validNameOpt <- GraphOpt$line$validName$name
        validNameOpt <- validNameOpt[validNameOpt != ""]
        if(length(validNameOpt) > 0){
           validName <- paste0(validNameOpt, collapse = ", ")
        }
    }
    valid.name <- tclVar(validName)

    valid.change <- tclVar(GraphOpt$line$validName$change)

    # multi
    plot.type2 <- tclVar(GraphOpt$line$plot$est$type)
    plot.col.line2 <- tclVar(GraphOpt$line$plot$est$line)
    plot.col.points2 <- tclVar(GraphOpt$line$plot$est$points)
    statePlotPoints2 <- if(GraphOpt$line$plot$est$type == 'both') 'normal' else 'disabled'
    obs.legend <- tclVar(GraphOpt$line$legend$obs)
    est.legend <- tclVar(GraphOpt$line$legend$est)
    # single
    custom.color <- tclVar(GraphOpt$line$plot1$custom)
    stateKol2 <- if(GraphOpt$line$plot1$custom) "normal" else "disabled"
    valid.obsN <- tclVar(GraphOpt$line$validName$obs)

    spin.pltPointS1 <- NULL
    spin.pltPointS2 <- NULL
    spin.pltPointT1 <- NULL
    spin.pltPointT2 <- NULL
    spin.pltLineW1 <- NULL
    spin.pltLineW2 <- NULL

    if(GraphOpt$line$plot.type == 'multi') plotMultiplePanel() else plotSinglePanel()

    #####################

    tkgrid(frameGraphXYlim, row = 0, column = 0, sticky = '', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
    tkgrid(frameGraphAxLabs, row = 1, column = 0, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 2, ipadx = 1, ipady = 1)
    tkgrid(frameGraphTitle, row = 2, column = 0, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 2, ipadx = 1, ipady = 1)
    tkgrid(framePlotT, row = 3, column = 0, sticky = '', rowspan = 1, columnspan = 1, padx = 1, pady = 2, ipadx = 1, ipady = 1)
    tkgrid(framePlotOpt, row = 4, column = 0, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 2, ipadx = 1, ipady = 1)

    #####################
    bt.opt.OK <- ttkbutton(frButt, text = .cdtEnv$tcl$lang$global[['button']][['1']])
    bt.opt.CA <- ttkbutton(frButt, text = .cdtEnv$tcl$lang$global[['button']][['2']])

    tkconfigure(bt.opt.OK, command = function(){
        GraphOpt$line$xlim$is.min <<- switch(tclvalue(is.min.xlim), '0' = FALSE, '1' = TRUE)
        GraphOpt$line$xlim$is.max <<- switch(tclvalue(is.max.xlim), '0' = FALSE, '1' = TRUE)
        GraphOpt$line$xlim$min <<- trimws(tclvalue(min.xlim))
        GraphOpt$line$xlim$max <<- trimws(tclvalue(max.xlim))

        GraphOpt$line$ylim$is.min <<- switch(tclvalue(is.min.ylim), '0' = FALSE, '1' = TRUE)
        GraphOpt$line$ylim$is.max <<- switch(tclvalue(is.max.ylim), '0' = FALSE, '1' = TRUE)
        GraphOpt$line$ylim$min <<- as.numeric(trimws(tclvalue(min.ylim)))
        GraphOpt$line$ylim$max <<- as.numeric(trimws(tclvalue(max.ylim)))

        GraphOpt$line$axislabs$is.xlab <<- switch(tclvalue(is.xaxis.lab), '0' = FALSE, '1' = TRUE)
        GraphOpt$line$axislabs$is.ylab <<- switch(tclvalue(is.yaxis.lab), '0' = FALSE, '1' = TRUE)
        GraphOpt$line$axislabs$xlab <<- gsub('\\\\n', '\n', trimws(tclvalue(xaxis.lab)))
        GraphOpt$line$axislabs$ylab <<- gsub('\\\\n', '\n', trimws(tclvalue(yaxis.lab)))

        GraphOpt$line$title$is.title <<- switch(tclvalue(is.title), '0' = FALSE, '1' = TRUE)
        GraphOpt$line$title$title <<- gsub('\\\\n', '\n', trimws(tclvalue(text.title)))

        GraphOpt$line$plot$obs$type <<- trimws(tclvalue(plot.type1))
        GraphOpt$line$plot$obs$line <<- tclvalue(plot.col.line1)
        GraphOpt$line$plot$obs$lwd <<- as.numeric(trimws(tclvalue(tkget(spin.pltLineW1))))
        GraphOpt$line$plot$obs$points <<- tclvalue(plot.col.points1)
        GraphOpt$line$plot$obs$cex <<- as.numeric(trimws(tclvalue(tkget(spin.pltPointS1))))
        GraphOpt$line$plot$obs$pch <<- as.numeric(trimws(tclvalue(tkget(spin.pltPointT1))))

        GraphOpt$line$legend$add <<- switch(tclvalue(add.legend), '0' = FALSE, '1' = TRUE)
        GraphOpt$line$plot.type <<- plotTypeVAL[CbplotTypeVAL %in% trimws(tclvalue(plot.type))]

        GraphOpt$line$validName$change <<- switch(tclvalue(valid.change), '0' = FALSE, '1' = TRUE)

        if(GraphOpt$line$plot.type == 'multi'){
            GraphOpt$line$plot$est$type <<- trimws(tclvalue(plot.type2))
            GraphOpt$line$plot$est$line <<- tclvalue(plot.col.line2)
            GraphOpt$line$plot$est$lwd <<- as.numeric(trimws(tclvalue(tkget(spin.pltLineW2))))
            GraphOpt$line$plot$est$points <<- tclvalue(plot.col.points2)
            GraphOpt$line$plot$est$cex <<- as.numeric(trimws(tclvalue(tkget(spin.pltPointS2))))
            GraphOpt$line$plot$est$pch <<- as.numeric(trimws(tclvalue(tkget(spin.pltPointT2))))

            GraphOpt$line$legend$obs <<- trimws(tclvalue(obs.legend))
            GraphOpt$line$legend$est <<- trimws(tclvalue(est.legend))
        }else{
            GraphOpt$line$plot1$custom <<- switch(tclvalue(custom.color), '0' = FALSE, '1' = TRUE)
            if(GraphOpt$line$validName$change)
                GraphOpt$line$validName$obs <<- trimws(tclvalue(valid.obsN))
        }

        if(GraphOpt$line$validName$change){
            validName <- trimws(tclvalue(valid.name))
            validName <- strsplit(validName, ",")[[1]]
            validName <- trimws(validName)
            validName <- validName[validName != ""]
            if(length(validName) == 0) validName <- ""
            GraphOpt$line$validName$name <<- validName
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
