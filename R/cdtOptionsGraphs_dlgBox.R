
MapGraph.GraphOptions.Assess <- function(assessOp, parent.win = .cdtEnv$tcl$main$win){
    if(WindowsOS()){
        largeur1 <- 48
        largeur2 <- 52
        width.col <- 7
    }else{
        largeur1 <- 47
        largeur2 <- 50
        width.col <- 5
    }

    #####################
    xml.dlg <- file.path(.cdtDir$dirLocal, "languages", "cdtGraphOptions.Assess_dlgBox.xml")
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

    is.xaxis.lab <- tclVar(assessOp$axislabs$is.xlab)
    is.yaxis.lab <- tclVar(assessOp$axislabs$is.ylab)
    xaxis.lab <- tclVar(assessOp$axislabs$xlab)
    yaxis.lab <- tclVar(assessOp$axislabs$ylab)

    stateXLab <- if(assessOp$axislabs$is.xlab) 'normal' else 'disabled'
    stateYLab <- if(assessOp$axislabs$is.ylab) 'normal' else 'disabled'

    chk.Xlab <- tkcheckbutton(frameGraphAxLabs, variable = is.xaxis.lab, text = 'Xlab', anchor = 'w', justify = 'left')
    en.Xlab <- tkentry(frameGraphAxLabs, textvariable = xaxis.lab, width = largeur1, state = stateXLab)
    chk.Ylab <- tkcheckbutton(frameGraphAxLabs, variable = is.yaxis.lab, text = 'Ylab', anchor = 'w', justify = 'left')
    en.Ylab <- tkentry(frameGraphAxLabs, textvariable = yaxis.lab, width = largeur1, state = stateYLab)

    tkgrid(chk.Xlab, row = 0, column = 0, sticky = 'e')
    tkgrid(en.Xlab, row = 0, column = 1, sticky = 'we')
    tkgrid(chk.Ylab, row = 1, column = 0, sticky = 'e')
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

    is.title <- tclVar(assessOp$title$is.title)
    text.title <- tclVar(assessOp$title$title)

    stateGpTlt <- if(assessOp$title$is.title) 'normal' else 'disabled'

    chk.GpTlt <- tkcheckbutton(frameGraphTitle, variable = is.title, anchor = 'e', justify = 'right')
    en.GpTlt <- tkentry(frameGraphTitle, textvariable = text.title, width = largeur2, state = stateGpTlt)

    tkgrid(chk.GpTlt, row = 0, column = 0, sticky = 'e', rowspan = 1, columnspan = 1, pady = 3)
    tkgrid(en.GpTlt, row = 0, column = 1, sticky = 'we', rowspan = 1, columnspan = 7, pady = 3)

    #########

    tkbind(chk.GpTlt, "<Button-1>", function(){
        stateGpTlt <- if(tclvalue(is.title) == '0') 'normal' else 'disabled'
        tkconfigure(en.GpTlt, state = stateGpTlt)
    })

    #####################

    frameBarCol <- tkframe(frDialog)

    color.bar <- tclVar(assessOp$colors$col)

    txt.colBar <- tklabel(frameBarCol, text = lang.dlg[['label']][['3']], anchor = 'e', justify = 'right')
    bt.colBar <- tkbutton(frameBarCol, bg = tclvalue(color.bar), width = width.col)

    tkgrid(txt.colBar, row = 0, column = 0, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
    tkgrid(bt.colBar, row = 0, column = 1, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)

    #########
    tkconfigure(bt.colBar, command = function(){
        tcl('wm', 'attributes', tt, topmost = FALSE)
        loko <- trimws(tclvalue(tcl("tk_chooseColor", initialcolor = tclvalue(color.bar), title = lang.dlg[['label']][['4']])))
        tcl('wm', 'attributes', tt, topmost = TRUE)
        if(nchar(loko) > 0){
            tkconfigure(bt.colBar, bg = loko)
            tclvalue(color.bar) <- loko
        }
    })

    #####################

    tkgrid(frameGraphAxLabs, row = 0, column = 0, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
    tkgrid(frameGraphTitle, row = 1, column = 0, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
    tkgrid(frameBarCol, row = 2, column = 0, sticky = '', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)

    #####################
    bt.opt.OK <- ttkbutton(frButt, text = .cdtEnv$tcl$lang$global[['button']][['1']])
    bt.opt.CA <- ttkbutton(frButt, text = .cdtEnv$tcl$lang$global[['button']][['2']])

    tkconfigure(bt.opt.OK, command = function(){
        assessOp$axislabs$is.xlab <<- switch(tclvalue(is.xaxis.lab), '0' = FALSE, '1' = TRUE)
        assessOp$axislabs$is.ylab <<- switch(tclvalue(is.yaxis.lab), '0' = FALSE, '1' = TRUE)
        assessOp$axislabs$xlab <<- gsub('\\\\n', '\n', trimws(tclvalue(xaxis.lab)))
        assessOp$axislabs$ylab <<- gsub('\\\\n', '\n', trimws(tclvalue(yaxis.lab)))

        assessOp$title$is.title <<- switch(tclvalue(is.title), '0' = FALSE, '1' = TRUE)
        assessOp$title$title <<- gsub('\\\\n', '\n', trimws(tclvalue(text.title)))

        assessOp$colors$col <<- tclvalue(color.bar)

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
    return(assessOp)
}

#######################################################################################################

MapGraph.GraphOptions.Bar <- function(climGraphOpt, parent.win = .cdtEnv$tcl$main$win){
    if(WindowsOS()){
        largeur1 <- 64
        largeur2 <- 68
        width.col <- 7
    }else{
        largeur1 <- 60
        largeur2 <- 64
        width.col <- 5
    }

    #####################
    xml.dlg <- file.path(.cdtDir$dirLocal, "languages", "cdtGraphOptions.Bar_dlgBox.xml")
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

    is.min.xlim <- tclVar(climGraphOpt$bar$xlim$is.min)
    is.max.xlim <- tclVar(climGraphOpt$bar$xlim$is.max)
    min.xlim <- tclVar(climGraphOpt$bar$xlim$min)
    max.xlim <- tclVar(climGraphOpt$bar$xlim$max)

    stateMinXlim <- if(climGraphOpt$bar$xlim$is.min) 'normal' else 'disabled'
    stateMaxXlim <- if(climGraphOpt$bar$xlim$is.max) 'normal' else 'disabled'

    chk.min.Xlim <- tkcheckbutton(frameGraphXlim, variable = is.min.xlim, text = "Min", anchor = 'w', justify = 'left')
    en.min.Xlim <- tkentry(frameGraphXlim, textvariable = min.xlim, width = 11, state = stateMinXlim)
    chk.max.Xlim <- tkcheckbutton(frameGraphXlim, variable = is.max.xlim, text = "Max", anchor = 'w', justify = 'left')
    en.max.Xlim <- tkentry(frameGraphXlim, textvariable = max.xlim, width = 11, state = stateMaxXlim)

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

    #####################

    frameGraphYlim <- ttklabelframe(frameGraphXYlim, text = lang.dlg[['label']][['2']], relief = 'groove')

    is.min.ylim <- tclVar(climGraphOpt$bar$ylim$is.min)
    is.max.ylim <- tclVar(climGraphOpt$bar$ylim$is.max)
    min.ylim <- tclVar(climGraphOpt$bar$ylim$min)
    max.ylim <- tclVar(climGraphOpt$bar$ylim$max)

    stateMinYlim <- if(climGraphOpt$bar$ylim$is.min) 'normal' else 'disabled'
    stateMaxYlim <- if(climGraphOpt$bar$ylim$is.max) 'normal' else 'disabled'

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

    sepXYlim <- tklabel(frameGraphXYlim, text = "", width = 2)

    tkgrid(frameGraphXlim, row = 0, column = 0, sticky = 'w')
    tkgrid(sepXYlim, row = 0, column = 1, sticky = 'we')
    tkgrid(frameGraphYlim, row = 0, column = 2, sticky = 'e')

    #####################

    frameGraphAxLabs <- ttklabelframe(frDialog, text = lang.dlg[['label']][['3']], relief = 'groove')

    is.xaxis.lab <- tclVar(climGraphOpt$bar$axislabs$is.xlab)
    is.yaxis.lab <- tclVar(climGraphOpt$bar$axislabs$is.ylab)
    xaxis.lab <- tclVar(climGraphOpt$bar$axislabs$xlab)
    yaxis.lab <- tclVar(climGraphOpt$bar$axislabs$ylab)

    stateXLab <- if(climGraphOpt$bar$axislabs$is.xlab) 'normal' else 'disabled'
    stateYLab <- if(climGraphOpt$bar$axislabs$is.ylab) 'normal' else 'disabled'

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

    is.title <- tclVar(climGraphOpt$bar$title$is.title)
    text.title <- tclVar(climGraphOpt$bar$title$title)

    pos.title <- tclVar()
    CbposTitleVAL <- lang.dlg[['combobox']][['1']]
    posTitleVAL <- c("top", "bottom")
    tclvalue(pos.title) <- CbposTitleVAL[posTitleVAL %in% climGraphOpt$bar$title$position]

    stateGpTlt <- if(climGraphOpt$bar$title$is.title) 'normal' else 'disabled'

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

    frameBarCol <- tkframe(frDialog)

    color.bar <- tclVar(climGraphOpt$bar$colors$col)

    txt.colBar <- tklabel(frameBarCol, text = lang.dlg[['label']][['6']], anchor = 'e', justify = 'right')
    bt.colBar <- tkbutton(frameBarCol, bg = tclvalue(color.bar), width = width.col)

    tkgrid(txt.colBar, row = 0, column = 0, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
    tkgrid(bt.colBar, row = 0, column = 1, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)

    #########
    tkconfigure(bt.colBar, command = function(){
        tcl('wm', 'attributes', tt, topmost = FALSE)
        loko <- trimws(tclvalue(tcl("tk_chooseColor", initialcolor = tclvalue(color.bar), title = lang.dlg[['label']][['7']])))
        tcl('wm', 'attributes', tt, topmost = TRUE)
        if(nchar(loko) > 0){
            tkconfigure(bt.colBar, bg = loko)
            tclvalue(color.bar) <- loko
        }
    })

    #####################

    tkgrid(frameGraphXYlim, row = 0, column = 0, sticky = '', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
    tkgrid(frameGraphAxLabs, row = 1, column = 0, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
    tkgrid(frameGraphTitle, row = 2, column = 0, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
    tkgrid(frameBarCol, row = 3, column = 0, sticky = '', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)

    #####################
    bt.opt.OK <- ttkbutton(frButt, text = .cdtEnv$tcl$lang$global[['button']][['1']])
    bt.opt.CA <- ttkbutton(frButt, text = .cdtEnv$tcl$lang$global[['button']][['2']])

    tkconfigure(bt.opt.OK, command = function(){
        climGraphOpt$bar$xlim$is.min <<- switch(tclvalue(is.min.xlim), '0' = FALSE, '1' = TRUE)
        climGraphOpt$bar$xlim$is.max <<- switch(tclvalue(is.max.xlim), '0' = FALSE, '1' = TRUE)
        climGraphOpt$bar$xlim$min <<- trimws(tclvalue(min.xlim))
        climGraphOpt$bar$xlim$max <<- trimws(tclvalue(max.xlim))

        climGraphOpt$bar$ylim$is.min <<- switch(tclvalue(is.min.ylim), '0' = FALSE, '1' = TRUE)
        climGraphOpt$bar$ylim$is.max <<- switch(tclvalue(is.max.ylim), '0' = FALSE, '1' = TRUE)
        climGraphOpt$bar$ylim$min <<- as.numeric(trimws(tclvalue(min.ylim)))
        climGraphOpt$bar$ylim$max <<- as.numeric(trimws(tclvalue(max.ylim)))

        climGraphOpt$bar$axislabs$is.xlab <<- switch(tclvalue(is.xaxis.lab), '0' = FALSE, '1' = TRUE)
        climGraphOpt$bar$axislabs$is.ylab <<- switch(tclvalue(is.yaxis.lab), '0' = FALSE, '1' = TRUE)
        climGraphOpt$bar$axislabs$xlab <<- gsub('\\\\n', '\n', trimws(tclvalue(xaxis.lab)))
        climGraphOpt$bar$axislabs$ylab <<- gsub('\\\\n', '\n', trimws(tclvalue(yaxis.lab)))

        climGraphOpt$bar$title$is.title <<- switch(tclvalue(is.title), '0' = FALSE, '1' = TRUE)
        climGraphOpt$bar$title$title <<- gsub('\\\\n', '\n', trimws(tclvalue(text.title)))
        climGraphOpt$bar$title$position <<- posTitleVAL[CbposTitleVAL %in% trimws(tclvalue(pos.title))]

        climGraphOpt$bar$colors$col <<- tclvalue(color.bar)

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
    return(climGraphOpt)
}

#######################################################################################################

MapGraph.GraphOptions.Anomaly <- function(climGraphOpt, parent.win = .cdtEnv$tcl$main$win){
    if(WindowsOS()){
        largeur1 <- 64
        largeur2 <- 68
        largeur3 <- 6
        width.col <- 7
    }else{
        largeur1 <- 60
        largeur2 <- 64
        largeur3 <- 7
        width.col <- 5
    }

    #####################
    xml.dlg <- file.path(.cdtDir$dirLocal, "languages", "cdtGraphOptions.Anomaly_dlgBox.xml")
    lang.dlg <- cdtLanguageParse(xml.dlg, .cdtData$Config$lang.iso)

    #####################
    tt <- tktoplevel()
    tkgrab.set(tt)
    tkfocus(tt)

    #####################
    frDialog <- tkframe(tt, relief = 'raised', borderwidth = 2)
    frButt <- tkframe(tt)

    #####################

    if(!is.null(climGraphOpt$anomaly$anom)){
        frameAnomaly <- ttklabelframe(frDialog, text = lang.dlg[['label']][['1']], relief = 'groove')

        ####
        frameAnomCHK <- tkframe(frameAnomaly)

        perc.anom <- tclVar(climGraphOpt$anomaly$anom$perc.anom)
        basePeriod <- tclVar(climGraphOpt$anomaly$anom$basePeriod)

        chk.PercAnom <- tkcheckbutton(frameAnomCHK, variable = perc.anom, text = lang.dlg[['checkbutton']][['1']], anchor = 'w', justify = 'left')
        chk.BaseAnom <- tkcheckbutton(frameAnomCHK, variable = basePeriod, text = lang.dlg[['checkbutton']][['2']], anchor = 'w', justify = 'left')

        tkgrid(chk.PercAnom, row = 0, column = 0, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
        tkgrid(chk.BaseAnom, row = 1, column = 0, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)

        helpWidget(chk.PercAnom, lang.dlg[['tooltip']][['1']], lang.dlg[['status']][['1']])
        helpWidget(chk.BaseAnom, lang.dlg[['tooltip']][['2']], lang.dlg[['status']][['2']])

        ####

        tkbind(chk.BaseAnom, "<Button-1>", function(){
            statebasePeriod <- if(tclvalue(basePeriod) == '0') 'normal' else 'disabled'
            tkconfigure(en.strtYrAnom, state = statebasePeriod)
            tkconfigure(en.endYrAnom, state = statebasePeriod)
        })

        ####
        frameAnomEN <- tkframe(frameAnomaly)

        startYr.anom <- tclVar(climGraphOpt$anomaly$anom$startYr.anom)
        endYr.anom <- tclVar(climGraphOpt$anomaly$anom$endYr.anom)

        statebasePeriod <- if(climGraphOpt$anomaly$anom$basePeriod) 'normal' else 'disabled'
 
        txt.strtYrAnom <- tklabel(frameAnomEN, text = lang.dlg[['label']][['2']], anchor = 'e', justify = 'right')
        en.strtYrAnom <- tkentry(frameAnomEN, textvariable = startYr.anom, width = 5, state = statebasePeriod)
        txt.endYrAnom <- tklabel(frameAnomEN, text = lang.dlg[['label']][['3']], anchor = 'e', justify = 'right')
        en.endYrAnom <- tkentry(frameAnomEN, textvariable = endYr.anom, width = 5, state = statebasePeriod)

        tkgrid(txt.strtYrAnom, row = 0, column = 0, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
        tkgrid(en.strtYrAnom, row = 0, column = 1, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
        tkgrid(txt.endYrAnom, row = 0, column = 2, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
        tkgrid(en.endYrAnom, row = 0, column = 3, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)

        helpWidget(en.strtYrAnom, lang.dlg[['tooltip']][['3']], lang.dlg[['status']][['3']])
        helpWidget(en.endYrAnom, lang.dlg[['tooltip']][['4']], lang.dlg[['status']][['4']])

        #########
        sepAnom <- tklabel(frameAnomaly, text = "", width = largeur3)

        tkgrid(frameAnomCHK, row = 0, column = 0, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
        tkgrid(sepAnom, row = 0, column = 1, sticky = 'we')
        tkgrid(frameAnomEN, row = 0, column = 2, sticky = 'we', rowspan = 2, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
    }

    #####################

    frameGraphXYlim <- tkframe(frDialog)

    ###########

    frameGraphXlim <- ttklabelframe(frameGraphXYlim, text = lang.dlg[['label']][['4']], relief = 'groove')

    is.min.xlim <- tclVar(climGraphOpt$anomaly$xlim$is.min)
    is.max.xlim <- tclVar(climGraphOpt$anomaly$xlim$is.max)
    min.xlim <- tclVar(climGraphOpt$anomaly$xlim$min)
    max.xlim <- tclVar(climGraphOpt$anomaly$xlim$max)

    stateMinXlim <- if(climGraphOpt$anomaly$xlim$is.min) 'normal' else 'disabled'
    stateMaxXlim <- if(climGraphOpt$anomaly$xlim$is.max) 'normal' else 'disabled'

    chk.min.Xlim <- tkcheckbutton(frameGraphXlim, variable = is.min.xlim, text = "Min", anchor = 'w', justify = 'left')
    en.min.Xlim <- tkentry(frameGraphXlim, textvariable = min.xlim, width = 11, state = stateMinXlim)
    chk.max.Xlim <- tkcheckbutton(frameGraphXlim, variable = is.max.xlim, text = "Max", anchor = 'w', justify = 'left')
    en.max.Xlim <- tkentry(frameGraphXlim, textvariable = max.xlim, width = 11, state = stateMaxXlim)

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

    #####################

    frameGraphYlim <- ttklabelframe(frameGraphXYlim, text = lang.dlg[['label']][['5']], relief = 'groove')

    is.min.ylim <- tclVar(climGraphOpt$anomaly$ylim$is.min)
    is.max.ylim <- tclVar(climGraphOpt$anomaly$ylim$is.max)
    min.ylim <- tclVar(climGraphOpt$anomaly$ylim$min)
    max.ylim <- tclVar(climGraphOpt$anomaly$ylim$max)

    stateMinYlim <- if(climGraphOpt$anomaly$ylim$is.min) 'normal' else 'disabled'
    stateMaxYlim <- if(climGraphOpt$anomaly$ylim$is.max) 'normal' else 'disabled'

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

    sepXYlim <- tklabel(frameGraphXYlim, text = "", width = 2)

    tkgrid(frameGraphXlim, row = 0, column = 0, sticky = 'w')
    tkgrid(sepXYlim, row = 0, column = 1, sticky = 'we')
    tkgrid(frameGraphYlim, row = 0, column = 2, sticky = 'e')

    #####################

    frameGraphAxLabs <- ttklabelframe(frDialog, text = lang.dlg[['label']][['6']], relief = 'groove')

    is.xaxis.lab <- tclVar(climGraphOpt$anomaly$axislabs$is.xlab)
    is.yaxis.lab <- tclVar(climGraphOpt$anomaly$axislabs$is.ylab)
    xaxis.lab <- tclVar(climGraphOpt$anomaly$axislabs$xlab)
    yaxis.lab <- tclVar(climGraphOpt$anomaly$axislabs$ylab)

    stateXLab <- if(climGraphOpt$anomaly$axislabs$is.xlab) 'normal' else 'disabled'
    stateYLab <- if(climGraphOpt$anomaly$axislabs$is.ylab) 'normal' else 'disabled'

    chk.Xlab <- tkcheckbutton(frameGraphAxLabs, variable = is.xaxis.lab, text = 'Xlab', anchor = 'w', justify = 'left')
    en.Xlab <- tkentry(frameGraphAxLabs, textvariable = xaxis.lab, width = largeur1, state = stateXLab)
    chk.Ylab <- tkcheckbutton(frameGraphAxLabs, variable = is.yaxis.lab, text = 'Ylab', anchor = 'w', justify = 'left')
    en.Ylab <- tkentry(frameGraphAxLabs, textvariable = yaxis.lab, width = largeur1, state = stateYLab)

    tkgrid(chk.Xlab, row = 0, column = 0, sticky = 'e')
    tkgrid(en.Xlab, row = 0, column = 1, sticky = 'we')
    tkgrid(chk.Ylab, row = 1, column = 0, sticky = 'e')
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

    frameGraphTitle <- ttklabelframe(frDialog, text = lang.dlg[['label']][['7']], relief = 'groove')

    is.title <- tclVar(climGraphOpt$anomaly$title$is.title)
    text.title <- tclVar(climGraphOpt$anomaly$title$title)

    pos.title <- tclVar()
    CbposTitleVAL <- lang.dlg[['combobox']][['1']]
    posTitleVAL <- c("top", "bottom")
    tclvalue(pos.title) <- CbposTitleVAL[posTitleVAL %in% climGraphOpt$anomaly$title$position]

    stateGpTlt <- if(climGraphOpt$anomaly$title$is.title) 'normal' else 'disabled'

    chk.GpTlt <- tkcheckbutton(frameGraphTitle, variable = is.title, anchor = 'e', justify = 'right')
    en.GpTlt <- tkentry(frameGraphTitle, textvariable = text.title, width = largeur2, state = stateGpTlt)
    txt.GpTlt <- tklabel(frameGraphTitle, text = lang.dlg[['label']][['8']], anchor = 'e', justify = 'right')
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

    frameBarCol <- ttklabelframe(frDialog, text = lang.dlg[['label']][['9']], relief = 'groove')

    color.moins <- tclVar(climGraphOpt$anomaly$colors$negative)
    color.plus <- tclVar(climGraphOpt$anomaly$colors$positive)

    txt.colMoins <- tklabel(frameBarCol, text = lang.dlg[['label']][['10']], anchor = 'e', justify = 'right')
    bt.colMoins <- tkbutton(frameBarCol, bg = tclvalue(color.moins), width = width.col)
    txt.colPlus <- tklabel(frameBarCol, text = lang.dlg[['label']][['11']], anchor = 'e', justify = 'right')
    bt.colPlus <- tkbutton(frameBarCol, bg = tclvalue(color.plus), width = width.col)

    tkgrid(txt.colMoins, row = 0, column = 0, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
    tkgrid(bt.colMoins, row = 0, column = 1, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
    tkgrid(txt.colPlus, row = 0, column = 2, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
    tkgrid(bt.colPlus, row = 0, column = 3, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)

    #########
    tkconfigure(bt.colMoins, command = function(){
        tcl('wm', 'attributes', tt, topmost = FALSE)
        loko <- trimws(tclvalue(tcl("tk_chooseColor", initialcolor = tclvalue(color.moins), title = lang.dlg[['label']][['12']])))
        tcl('wm', 'attributes', tt, topmost = TRUE)
        if(nchar(loko) > 0){
            tkconfigure(bt.colMoins, bg = loko)
            tclvalue(color.moins) <- loko
        }
    })

    tkconfigure(bt.colPlus, command = function(){
        tcl('wm', 'attributes', tt, topmost = FALSE)
        loko <- trimws(tclvalue(tcl("tk_chooseColor", initialcolor = tclvalue(color.plus), title = lang.dlg[['label']][['12']])))
        tcl('wm', 'attributes', tt, topmost = TRUE)
        if(nchar(loko) > 0){
            tkconfigure(bt.colPlus, bg = loko)
            tclvalue(color.plus) <- loko
        }
    })

    #####################

    if(!is.null(climGraphOpt$anomaly$anom))
        tkgrid(frameAnomaly, row = 0, column = 0, sticky = '', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)

    tkgrid(frameGraphXYlim, row = 1, column = 0, sticky = '', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
    tkgrid(frameGraphAxLabs, row = 2, column = 0, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
    tkgrid(frameGraphTitle, row = 3, column = 0, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
    tkgrid(frameBarCol, row = 4, column = 0, sticky = '', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)

    #####################
    bt.opt.OK <- ttkbutton(frButt, text = .cdtEnv$tcl$lang$global[['button']][['1']])
    bt.opt.CA <- ttkbutton(frButt, text = .cdtEnv$tcl$lang$global[['button']][['2']])

    tkconfigure(bt.opt.OK, command = function(){
        if(!is.null(climGraphOpt$anomaly$anom)){
            climGraphOpt$anomaly$anom$perc.anom <<- switch(tclvalue(perc.anom), '0' = FALSE, '1' = TRUE)
            climGraphOpt$anomaly$anom$basePeriod <<- switch(tclvalue(basePeriod), '0' = FALSE, '1' = TRUE)
            climGraphOpt$anomaly$anom$startYr.anom <<- as.numeric(trimws(tclvalue(startYr.anom)))
            climGraphOpt$anomaly$anom$endYr.anom <<- as.numeric(trimws(tclvalue(endYr.anom)))
        }

        climGraphOpt$anomaly$xlim$is.min <<- switch(tclvalue(is.min.xlim), '0' = FALSE, '1' = TRUE)
        climGraphOpt$anomaly$xlim$is.max <<- switch(tclvalue(is.max.xlim), '0' = FALSE, '1' = TRUE)
        climGraphOpt$anomaly$xlim$min <<- trimws(tclvalue(min.xlim))
        climGraphOpt$anomaly$xlim$max <<- trimws(tclvalue(max.xlim))

        climGraphOpt$anomaly$ylim$is.min <<- switch(tclvalue(is.min.ylim), '0' = FALSE, '1' = TRUE)
        climGraphOpt$anomaly$ylim$is.max <<- switch(tclvalue(is.max.ylim), '0' = FALSE, '1' = TRUE)
        climGraphOpt$anomaly$ylim$min <<- as.numeric(trimws(tclvalue(min.ylim)))
        climGraphOpt$anomaly$ylim$max <<- as.numeric(trimws(tclvalue(max.ylim)))

        climGraphOpt$anomaly$axislabs$is.xlab <<- switch(tclvalue(is.xaxis.lab), '0' = FALSE, '1' = TRUE)
        climGraphOpt$anomaly$axislabs$is.ylab <<- switch(tclvalue(is.yaxis.lab), '0' = FALSE, '1' = TRUE)
        climGraphOpt$anomaly$axislabs$xlab <<- gsub('\\\\n', '\n', trimws(tclvalue(xaxis.lab)))
        climGraphOpt$anomaly$axislabs$ylab <<- gsub('\\\\n', '\n', trimws(tclvalue(yaxis.lab)))

        climGraphOpt$anomaly$title$is.title <<- switch(tclvalue(is.title), '0' = FALSE, '1' = TRUE)
        climGraphOpt$anomaly$title$title <<- gsub('\\\\n', '\n', trimws(tclvalue(text.title)))
        climGraphOpt$anomaly$title$position <<- posTitleVAL[CbposTitleVAL %in% trimws(tclvalue(pos.title))]

        climGraphOpt$anomaly$colors$negative <<- tclvalue(color.moins)
        climGraphOpt$anomaly$colors$positive <<- tclvalue(color.plus)

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
    return(climGraphOpt)
}

#######################################################################################################

MapGraph.GraphOptions.Line <- function(climGraphOpt, parent.win = .cdtEnv$tcl$main$win){
    if(WindowsOS()){
        largeur1 <- 64
        largeur2 <- 68
        largeur3 <- 40
        width.col <- 3
        width.spin <- 4
    }else{
        largeur1 <- 60
        largeur2 <- 64
        largeur3 <- 37
        width.col <- 1
        width.spin <- 4
    }

    #####################
    xml.dlg <- file.path(.cdtDir$dirLocal, "languages", "cdtGraphOptions.Line_dlgBox.xml")
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

    is.min.xlim <- tclVar(climGraphOpt$line$xlim$is.min)
    is.max.xlim <- tclVar(climGraphOpt$line$xlim$is.max)
    min.xlim <- tclVar(climGraphOpt$line$xlim$min)
    max.xlim <- tclVar(climGraphOpt$line$xlim$max)

    stateMinXlim <- if(climGraphOpt$line$xlim$is.min) 'normal' else 'disabled'
    stateMaxXlim <- if(climGraphOpt$line$xlim$is.max) 'normal' else 'disabled'

    chk.min.Xlim <- tkcheckbutton(frameGraphXlim, variable = is.min.xlim, text = "Min", anchor = 'w', justify = 'left')
    en.min.Xlim <- tkentry(frameGraphXlim, textvariable = min.xlim, width = 11, state = stateMinXlim)
    chk.max.Xlim <- tkcheckbutton(frameGraphXlim, variable = is.max.xlim, text = "Max", anchor = 'w', justify = 'left')
    en.max.Xlim <- tkentry(frameGraphXlim, textvariable = max.xlim, width = 11, state = stateMaxXlim)

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

    #####################

    frameGraphYlim <- ttklabelframe(frameGraphXYlim, text = lang.dlg[['label']][['2']], relief = 'groove')

    is.min.ylim <- tclVar(climGraphOpt$line$ylim$is.min)
    is.max.ylim <- tclVar(climGraphOpt$line$ylim$is.max)
    min.ylim <- tclVar(climGraphOpt$line$ylim$min)
    max.ylim <- tclVar(climGraphOpt$line$ylim$max)

    stateMinYlim <- if(climGraphOpt$line$ylim$is.min) 'normal' else 'disabled'
    stateMaxYlim <- if(climGraphOpt$line$ylim$is.max) 'normal' else 'disabled'

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

    sepXYlim <- tklabel(frameGraphXYlim, text = "", width = 2)

    tkgrid(frameGraphXlim, row = 0, column = 0, sticky = 'w')
    tkgrid(sepXYlim, row = 0, column = 1, sticky = 'we')
    tkgrid(frameGraphYlim, row = 0, column = 2, sticky = 'e')

    #####################

    frameGraphAxLabs <- ttklabelframe(frDialog, text = lang.dlg[['label']][['3']], relief = 'groove')

    is.xaxis.lab <- tclVar(climGraphOpt$line$axislabs$is.xlab)
    is.yaxis.lab <- tclVar(climGraphOpt$line$axislabs$is.ylab)
    xaxis.lab <- tclVar(climGraphOpt$line$axislabs$xlab)
    yaxis.lab <- tclVar(climGraphOpt$line$axislabs$ylab)

    stateXLab <- if(climGraphOpt$line$axislabs$is.xlab) 'normal' else 'disabled'
    stateYLab <- if(climGraphOpt$line$axislabs$is.ylab) 'normal' else 'disabled'

    chk.Xlab <- tkcheckbutton(frameGraphAxLabs, variable = is.xaxis.lab, text = 'Xlab', anchor = 'w', justify = 'left')
    en.Xlab <- tkentry(frameGraphAxLabs, textvariable = xaxis.lab, width = largeur1, state = stateXLab)
    chk.Ylab <- tkcheckbutton(frameGraphAxLabs, variable = is.yaxis.lab, text = 'Ylab', anchor = 'w', justify = 'left')
    en.Ylab <- tkentry(frameGraphAxLabs, textvariable = yaxis.lab, width = largeur1, state = stateYLab)

    tkgrid(chk.Xlab, row = 0, column = 0, sticky = 'e')
    tkgrid(en.Xlab, row = 0, column = 1, sticky = 'we')
    tkgrid(chk.Ylab, row = 1, column = 0, sticky = 'e')
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

    is.title <- tclVar(climGraphOpt$line$title$is.title)
    text.title <- tclVar(climGraphOpt$line$title$title)

    pos.title <- tclVar()
    CbposTitleVAL <- lang.dlg[['combobox']][['1']]
    posTitleVAL <- c("top", "bottom")
    tclvalue(pos.title) <- CbposTitleVAL[posTitleVAL %in% climGraphOpt$line$title$position]

    stateGpTlt <- if(climGraphOpt$line$title$is.title) 'normal' else 'disabled'

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
    frameGraphPlot <- ttklabelframe(frDialog, text = lang.dlg[['label']][['6']], relief = 'groove')

    plot.type <- tclVar(climGraphOpt$line$plot$type)

    txt.pltType <- tklabel(frameGraphPlot, text = lang.dlg[['label']][['7']], anchor = 'e', justify = 'right')
    cb.pltType <- ttkcombobox(frameGraphPlot, values = c('both', 'line'), textvariable = plot.type, width = 4)

    #########
    tkbind(cb.pltType, "<<ComboboxSelected>>", function(){
        statePlotPoints <- if(tclvalue(plot.type) == 'both') 'normal' else 'disabled'
        tkconfigure(bt.pltPointC, state = statePlotPoints)
        tkconfigure(spin.pltPointS, state = statePlotPoints)
    })

    ########
    framepltLine <- ttklabelframe(frameGraphPlot, text = lang.dlg[['label']][['8']], relief = 'groove')

    plot.col.line <- tclVar(climGraphOpt$line$plot$col$line)

    txt.pltLineC <- tklabel(framepltLine, text = lang.dlg[['label']][['9']], anchor = 'e', justify = 'right')
    bt.pltLineC <- tkbutton(framepltLine, bg = tclvalue(plot.col.line), width = width.col)
    txt.pltLineW <- tklabel(framepltLine, text = lang.dlg[['label']][['10']], anchor = 'e', justify = 'right')
    spin.pltLineW <- ttkspinbox(framepltLine, from = 0.5, to = 4, increment = 0.1, justify = 'center', width = width.spin)
    tkset(spin.pltLineW, climGraphOpt$line$plot$lwd)

    tkgrid(txt.pltLineC, bt.pltLineC, txt.pltLineW, spin.pltLineW)

    ########
    tkconfigure(bt.pltLineC, command = function(){
        tcl('wm', 'attributes', tt, topmost = FALSE)
        loko <- trimws(tclvalue(tcl("tk_chooseColor", initialcolor = tclvalue(plot.col.line), title = lang.dlg[['label']][['13']])))
        tcl('wm', 'attributes', tt, topmost = TRUE)
        if(nchar(loko) > 0){
            tkconfigure(bt.pltLineC, bg = loko)
            tclvalue(plot.col.line) <- loko
        }
    })

    ########
    framepltPoints <- ttklabelframe(frameGraphPlot, text = lang.dlg[['label']][['11']], relief = 'groove')

    plot.col.points <- tclVar(climGraphOpt$line$plot$col$points)
    statePlotPoints <- if(climGraphOpt$line$plot$type == 'both') 'normal' else 'disabled'

    txt.pltPointC <- tklabel(framepltPoints, text = lang.dlg[['label']][['9']], anchor = 'e', justify = 'right')
    bt.pltPointC <- tkbutton(framepltPoints, bg = tclvalue(plot.col.points), width = width.col, state = statePlotPoints)
    txt.pltPointS <- tklabel(framepltPoints, text = lang.dlg[['label']][['12']], anchor = 'e', justify = 'right')
    spin.pltPointS <- ttkspinbox(framepltPoints, from = 0.5, to = 2.5, increment = 0.1, justify = 'center', width = width.spin, state = statePlotPoints)
    tkset(spin.pltPointS, climGraphOpt$line$plot$cex)

    tkgrid(txt.pltPointC, bt.pltPointC, txt.pltPointS, spin.pltPointS)

    ########

    tkconfigure(bt.pltPointC, command = function(){
        tcl('wm', 'attributes', tt, topmost = FALSE)
        loko <- trimws(tclvalue(tcl("tk_chooseColor", initialcolor = tclvalue(plot.col.points), title = lang.dlg[['label']][['13']])))
        tcl('wm', 'attributes', tt, topmost = TRUE)
        if(nchar(loko) > 0){
            tkconfigure(bt.pltPointC, bg = loko)
            tclvalue(plot.col.points) <- loko
        }
    })

    #####################
    tkgrid(txt.pltType, row = 0, column = 0, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
    tkgrid(cb.pltType, row = 0, column = 1, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
    tkgrid(framepltLine, row = 0, column = 2, sticky = 'we', rowspan = 1, columnspan = 1, padx = 2, pady = 1, ipadx = 1, ipady = 1)
    tkgrid(framepltPoints, row = 0, column = 3, sticky = 'we', rowspan = 1, columnspan = 1, padx = 2, pady = 1, ipadx = 1, ipady = 1)

    #####################

    if(!is.null(climGraphOpt$line$legend)){
        frameGraphLegend <- ttklabelframe(frDialog, text = lang.dlg[['label']][['14']], relief = 'groove')

        is.lezMean <- tclVar(climGraphOpt$line$legend$is$mean)
        col.lezMean <- tclVar(climGraphOpt$line$legend$col$mean)
        text.lezMean <- tclVar(climGraphOpt$line$legend$text$mean)
        stateLezMean <- if(climGraphOpt$line$legend$is$mean & climGraphOpt$line$legend$add$mean) "normal" else "disabled"

        frameLezMean <- ttklabelframe(frameGraphLegend, text = lang.dlg[['label']][['15']], relief = 'groove')

        chk.lezMean <- tkcheckbutton(frameLezMean, variable = is.lezMean, anchor = 'e', justify = 'right')
        txt.lezMeanC <- tklabel(frameLezMean, text = lang.dlg[['label']][['9']], anchor = 'e', justify = 'right')
        bt.lezMeanC <- tkbutton(frameLezMean, bg = tclvalue(col.lezMean), width = width.col, state = stateLezMean)
        txt.lezMeanW <- tklabel(frameLezMean, text = lang.dlg[['label']][['10']], anchor = 'e', justify = 'right')
        spin.lezMeanW <- ttkspinbox(frameLezMean, from = 0.5, to = 4, increment = 0.1, justify = 'center', width = width.spin, state = stateLezMean)
        tkset(spin.lezMeanW, climGraphOpt$line$legend$lwd$mean)
        txt.lezMeanT <- tklabel(frameLezMean, text = lang.dlg[['label']][['18']], anchor = 'e', justify = 'right')
        en.lezMeanT <- tkentry(frameLezMean, textvariable = text.lezMean, width = largeur3, state = stateLezMean)

        tkconfigure(bt.lezMeanC, command = function(){
            tcl('wm', 'attributes', tt, topmost = FALSE)
            loko <- trimws(tclvalue(tcl("tk_chooseColor", initialcolor = tclvalue(col.lezMean), title = lang.dlg[['label']][['13']])))
            tcl('wm', 'attributes', tt, topmost = TRUE)
            if(nchar(loko) > 0){
                tkconfigure(bt.lezMeanC, bg = loko)
                tclvalue(col.lezMean) <- loko
            }
        })

        tkgrid(chk.lezMean, txt.lezMeanC, bt.lezMeanC, txt.lezMeanW, spin.lezMeanW, txt.lezMeanT, en.lezMeanT)

        #####
        tkbind(chk.lezMean, "<Button-1>", function(){
            stateLezMean <- if(climGraphOpt$line$legend$add$mean & tclvalue(is.lezMean) == '0') 'normal' else 'disabled'
            tkconfigure(bt.lezMeanC, state = stateLezMean)
            tkconfigure(spin.lezMeanW, state = stateLezMean)
            tkconfigure(en.lezMeanT, state = stateLezMean)
        })

        #########
        is.lezLin <- tclVar(climGraphOpt$line$legend$is$linear)
        col.lezLin <- tclVar(climGraphOpt$line$legend$col$linear)
        text.lezLin <- tclVar(climGraphOpt$line$legend$text$linear)
        stateLezLin <- if(climGraphOpt$line$legend$is$linear & climGraphOpt$line$legend$add$linear) "normal" else "disabled"

        frameLezLin <- ttklabelframe(frameGraphLegend, text = lang.dlg[['label']][['16']], relief = 'groove')

        chk.lezLin <- tkcheckbutton(frameLezLin, variable = is.lezLin, anchor = 'e', justify = 'right')
        txt.lezLinC <- tklabel(frameLezLin, text = lang.dlg[['label']][['9']], anchor = 'e', justify = 'right')
        bt.lezLinC <- tkbutton(frameLezLin, bg = tclvalue(col.lezLin), width = width.col, state = stateLezLin)
        txt.lezLinW <- tklabel(frameLezLin, text = lang.dlg[['label']][['10']], anchor = 'e', justify = 'right')
        spin.lezLinW <- ttkspinbox(frameLezLin, from = 0.5, to = 4, increment = 0.1, justify = 'center', width = width.spin, state = stateLezLin)
        tkset(spin.lezLinW, climGraphOpt$line$legend$lwd$linear)
        txt.lezLinT <- tklabel(frameLezLin, text = lang.dlg[['label']][['18']], anchor = 'e', justify = 'right')
        en.lezLinT <- tkentry(frameLezLin, textvariable = text.lezLin, width = largeur3, state = stateLezLin)

        tkconfigure(bt.lezLinC, command = function(){
            tcl('wm', 'attributes', tt, topmost = FALSE)
            loko <- trimws(tclvalue(tcl("tk_chooseColor", initialcolor = tclvalue(col.lezLin), title = "Colors")))
            tcl('wm', 'attributes', tt, topmost = TRUE)
            if(nchar(loko) > 0){
                tkconfigure(bt.lezLinC, bg = loko)
                tclvalue(col.lezLin) <- loko
            }
        })

        tkgrid(chk.lezLin, txt.lezLinC, bt.lezLinC, txt.lezLinW, spin.lezLinW, txt.lezLinT, en.lezLinT)

        #####
        tkbind(chk.lezLin, "<Button-1>", function(){
            stateLezLin <- if(climGraphOpt$line$legend$add$linear & tclvalue(is.lezLin) == '0') 'normal' else 'disabled'
            tkconfigure(bt.lezLinC, state = stateLezLin)
            tkconfigure(spin.lezLinW, state = stateLezLin)
            tkconfigure(en.lezLinT, state = stateLezLin)
        })

        #########
        is.lezTer <- tclVar(climGraphOpt$line$legend$is$tercile)
        col.lezTer1 <- tclVar(climGraphOpt$line$legend$col$tercile1)
        col.lezTer2 <- tclVar(climGraphOpt$line$legend$col$tercile2)
        text.lezTer1 <- tclVar(climGraphOpt$line$legend$text$tercile1)
        text.lezTer2 <- tclVar(climGraphOpt$line$legend$text$tercile2)
        stateLezTer <- if(climGraphOpt$line$legend$is$tercile & climGraphOpt$line$legend$add$tercile) "normal" else "disabled"

        frameLezTer <- ttklabelframe(frameGraphLegend, text = lang.dlg[['label']][['17']], relief = 'groove')

        chk.lezTer <- tkcheckbutton(frameLezTer, variable = is.lezTer, anchor = 'e', justify = 'right')
        txt.lezTerC1 <- tklabel(frameLezTer, text = lang.dlg[['label']][['9']], anchor = 'e', justify = 'right')
        bt.lezTerC1 <- tkbutton(frameLezTer, bg = tclvalue(col.lezTer1), width = width.col, state = stateLezTer)
        txt.lezTerW <- tklabel(frameLezTer, text = lang.dlg[['label']][['10']], anchor = 'e', justify = 'right')
        spin.lezTerW <- ttkspinbox(frameLezTer, from = 0.5, to = 4, increment = 0.1, justify = 'center', width = width.spin, state = stateLezTer)
        tkset(spin.lezTerW, climGraphOpt$line$legend$lwd$tercile)
        txt.lezTerT1 <- tklabel(frameLezTer, text = lang.dlg[['label']][['18']], anchor = 'e', justify = 'right')
        en.lezTerT1 <- tkentry(frameLezTer, textvariable = text.lezTer1, width = largeur3, state = stateLezTer)
        txt.lezTerC2 <- tklabel(frameLezTer, text = lang.dlg[['label']][['9']], anchor = 'e', justify = 'right')
        bt.lezTerC2 <- tkbutton(frameLezTer, bg = tclvalue(col.lezTer2), width = width.col, state = stateLezTer)
        txt.lezTerT2 <- tklabel(frameLezTer, text = lang.dlg[['label']][['18']], anchor = 'e', justify = 'right')
        en.lezTerT2 <- tkentry(frameLezTer, textvariable = text.lezTer2, width = largeur3, state = stateLezTer)

        tkconfigure(bt.lezTerC1, command = function(){
            tcl('wm', 'attributes', tt, topmost = FALSE)
            loko <- trimws(tclvalue(tcl("tk_chooseColor", initialcolor = tclvalue(col.lezTer1), title = lang.dlg[['label']][['13']])))
            tcl('wm', 'attributes', tt, topmost = TRUE)
            if(nchar(loko) > 0){
                tkconfigure(bt.lezTerC1, bg = loko)
                tclvalue(col.lezTer1) <- loko
            }
        })

        tkconfigure(bt.lezTerC2, command = function(){
            tcl('wm', 'attributes', tt, topmost = FALSE)
            loko <- trimws(tclvalue(tcl("tk_chooseColor", initialcolor = tclvalue(col.lezTer2), title = lang.dlg[['label']][['13']])))
            tcl('wm', 'attributes', tt, topmost = TRUE)
            if(nchar(loko) > 0){
                tkconfigure(bt.lezTerC2, bg = loko)
                tclvalue(col.lezTer2) <- loko
            }
        })

        tkgrid(chk.lezTer, txt.lezTerC1, bt.lezTerC1, txt.lezTerW, spin.lezTerW, txt.lezTerT1, en.lezTerT1)
        tkgrid(tklabel(frameLezTer), txt.lezTerC2, bt.lezTerC2, tklabel(frameLezTer), tklabel(frameLezTer), txt.lezTerT2, en.lezTerT2)

        #####
        tkbind(chk.lezTer, "<Button-1>", function(){
            stateLezTer <- if(climGraphOpt$line$legend$add$tercile & tclvalue(is.lezTer) == '0') 'normal' else 'disabled'
            tkconfigure(bt.lezTerC1, state = stateLezTer)
            tkconfigure(spin.lezTerW, state = stateLezTer)
            tkconfigure(en.lezTerT1, state = stateLezTer)
            tkconfigure(bt.lezTerC2, state = stateLezTer)
            tkconfigure(en.lezTerT2, state = stateLezTer)
        })

        #########

        tkgrid(frameLezMean, row = 0, column = 0, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
        tkgrid(frameLezLin, row = 1, column = 0, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
        tkgrid(frameLezTer, row = 2, column = 0, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
    }

    #####################

    tkgrid(frameGraphXYlim, row = 0, column = 0, sticky = '', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
    tkgrid(frameGraphAxLabs, row = 1, column = 0, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
    tkgrid(frameGraphTitle, row = 2, column = 0, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
    tkgrid(frameGraphPlot, row = 3, column = 0, sticky = '', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
    if(!is.null(climGraphOpt$line$legend))
        tkgrid(frameGraphLegend, row = 4, column = 0, sticky = '', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)

    #####################
    bt.opt.OK <- ttkbutton(frButt, text = .cdtEnv$tcl$lang$global[['button']][['1']])
    bt.opt.CA <- ttkbutton(frButt, text = .cdtEnv$tcl$lang$global[['button']][['2']])

    tkconfigure(bt.opt.OK, command = function(){
        climGraphOpt$line$xlim$is.min <<- switch(tclvalue(is.min.xlim), '0' = FALSE, '1' = TRUE)
        climGraphOpt$line$xlim$is.max <<- switch(tclvalue(is.max.xlim), '0' = FALSE, '1' = TRUE)
        climGraphOpt$line$xlim$min <<- trimws(tclvalue(min.xlim))
        climGraphOpt$line$xlim$max <<- trimws(tclvalue(max.xlim))

        climGraphOpt$line$ylim$is.min <<- switch(tclvalue(is.min.ylim), '0' = FALSE, '1' = TRUE)
        climGraphOpt$line$ylim$is.max <<- switch(tclvalue(is.max.ylim), '0' = FALSE, '1' = TRUE)
        climGraphOpt$line$ylim$min <<- as.numeric(trimws(tclvalue(min.ylim)))
        climGraphOpt$line$ylim$max <<- as.numeric(trimws(tclvalue(max.ylim)))

        climGraphOpt$line$axislabs$is.xlab <<- switch(tclvalue(is.xaxis.lab), '0' = FALSE, '1' = TRUE)
        climGraphOpt$line$axislabs$is.ylab <<- switch(tclvalue(is.yaxis.lab), '0' = FALSE, '1' = TRUE)
        climGraphOpt$line$axislabs$xlab <<- gsub('\\\\n', '\n', trimws(tclvalue(xaxis.lab)))
        climGraphOpt$line$axislabs$ylab <<- gsub('\\\\n', '\n', trimws(tclvalue(yaxis.lab)))

        climGraphOpt$line$title$is.title <<- switch(tclvalue(is.title), '0' = FALSE, '1' = TRUE)
        climGraphOpt$line$title$title <<- gsub('\\\\n', '\n', trimws(tclvalue(text.title)))
        climGraphOpt$line$title$position <<- posTitleVAL[CbposTitleVAL %in% trimws(tclvalue(pos.title))]

        climGraphOpt$line$plot$type <<- trimws(tclvalue(plot.type))
        climGraphOpt$line$plot$col$line <<- tclvalue(plot.col.line)
        climGraphOpt$line$plot$lwd <<- as.numeric(trimws(tclvalue(tkget(spin.pltLineW))))
        climGraphOpt$line$plot$col$points <<- tclvalue(plot.col.points)
        climGraphOpt$line$plot$cex <<- as.numeric(trimws(tclvalue(tkget(spin.pltPointS))))

        if(!is.null(climGraphOpt$line$legend)){
            climGraphOpt$line$legend$is$mean <<- switch(tclvalue(is.lezMean), '0' = FALSE, '1' = TRUE)
            climGraphOpt$line$legend$col$mean <<- tclvalue(col.lezMean)
            climGraphOpt$line$legend$lwd$mean <<- as.numeric(trimws(tclvalue(tkget(spin.lezMeanW))))
            climGraphOpt$line$legend$text$mean <<- trimws(tclvalue(text.lezMean))

            climGraphOpt$line$legend$is$linear <<- switch(tclvalue(is.lezLin), '0' = FALSE, '1' = TRUE)
            climGraphOpt$line$legend$col$linear <<- tclvalue(col.lezLin)
            climGraphOpt$line$legend$lwd$linear <<- as.numeric(trimws(tclvalue(tkget(spin.lezLinW))))
            climGraphOpt$line$legend$text$linear <<- trimws(tclvalue(text.lezLin))

            climGraphOpt$line$legend$is$tercile <<- switch(tclvalue(is.lezTer), '0' = FALSE, '1' = TRUE)
            climGraphOpt$line$legend$col$tercile1 <<- tclvalue(col.lezTer1)
            climGraphOpt$line$legend$col$tercile2 <<- tclvalue(col.lezTer2)
            climGraphOpt$line$legend$lwd$tercile <<- as.numeric(trimws(tclvalue(tkget(spin.lezTerW))))
            climGraphOpt$line$legend$text$tercile1 <<- trimws(tclvalue(text.lezTer1))
            climGraphOpt$line$legend$text$tercile2 <<- trimws(tclvalue(text.lezTer2))
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
    return(climGraphOpt)
}

#######################################################################################################

MapGraph.GraphOptions.Proba <- function(climGraphOpt, parent.win = .cdtEnv$tcl$main$win){
    if(WindowsOS()){
        largeur1 <- 51
        largeur2 <- 55
        largeur3 <- 3
        width.col <- 3
        width.spin <- 4
    }else{
        largeur1 <- 48
        largeur2 <- 52
        largeur3 <- 4
        width.col <- 1
        width.spin <- 4
    }

    #####################
    xml.dlg <- file.path(.cdtDir$dirLocal, "languages", "cdtGraphOptions.Proba_dlgBox.xml")
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

    is.min.xlim <- tclVar(climGraphOpt$proba$xlim$is.min)
    is.max.xlim <- tclVar(climGraphOpt$proba$xlim$is.max)
    min.xlim <- tclVar(climGraphOpt$proba$xlim$min)
    max.xlim <- tclVar(climGraphOpt$proba$xlim$max)

    stateMinXlim <- if(climGraphOpt$proba$xlim$is.min) 'normal' else 'disabled'
    stateMaxXlim <- if(climGraphOpt$proba$xlim$is.max) 'normal' else 'disabled'

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

    #####################

    frameGraphYlim <- ttklabelframe(frameGraphXYlim, text = lang.dlg[['label']][['2']], relief = 'groove')

    is.min.ylim <- tclVar(climGraphOpt$proba$ylim$is.min)
    is.max.ylim <- tclVar(climGraphOpt$proba$ylim$is.max)
    min.ylim <- tclVar(climGraphOpt$proba$ylim$min)
    max.ylim <- tclVar(climGraphOpt$proba$ylim$max)

    stateMinYlim <- if(climGraphOpt$proba$ylim$is.min) 'normal' else 'disabled'
    stateMaxYlim <- if(climGraphOpt$proba$ylim$is.max) 'normal' else 'disabled'

    chk.min.Ylim <- tkcheckbutton(frameGraphYlim, variable = is.min.ylim, text = "Min", anchor = 'w', justify = 'left')
    en.min.Ylim <- tkentry(frameGraphYlim, textvariable = min.ylim, width = 4, state = stateMinYlim)
    chk.max.Ylim <- tkcheckbutton(frameGraphYlim, variable = is.max.ylim, text = "Max", anchor = 'w', justify = 'left')
    en.max.Ylim <- tkentry(frameGraphYlim, textvariable = max.ylim, width = 4, state = stateMaxYlim)

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

    is.xaxis.lab <- tclVar(climGraphOpt$proba$axislabs$is.xlab)
    is.yaxis.lab <- tclVar(climGraphOpt$proba$axislabs$is.ylab)
    xaxis.lab <- tclVar(climGraphOpt$proba$axislabs$xlab)
    yaxis.lab <- tclVar(climGraphOpt$proba$axislabs$ylab)

    stateXLab <- if(climGraphOpt$proba$axislabs$is.xlab) 'normal' else 'disabled'
    stateYLab <- if(climGraphOpt$proba$axislabs$is.ylab) 'normal' else 'disabled'

    chk.Xlab <- tkcheckbutton(frameGraphAxLabs, variable = is.xaxis.lab, text = 'Xlab', anchor = 'w', justify = 'left')
    en.Xlab <- tkentry(frameGraphAxLabs, textvariable = xaxis.lab, width = largeur1, state = stateXLab)
    chk.Ylab <- tkcheckbutton(frameGraphAxLabs, variable = is.yaxis.lab, text = 'Ylab', anchor = 'w', justify = 'left')
    en.Ylab <- tkentry(frameGraphAxLabs, textvariable = yaxis.lab, width = largeur1, state = stateYLab)

    tkgrid(chk.Xlab, row = 0, column = 0, sticky = 'e')
    tkgrid(en.Xlab, row = 0, column = 1, sticky = 'we')
    tkgrid(chk.Ylab, row = 1, column = 0, sticky = 'e')
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

    is.title <- tclVar(climGraphOpt$proba$title$is.title)
    text.title <- tclVar(climGraphOpt$proba$title$title)

    pos.title <- tclVar()
    CbposTitleVAL <- lang.dlg[['combobox']][['1']]
    posTitleVAL <- c("top", "bottom")
    tclvalue(pos.title) <- CbposTitleVAL[posTitleVAL %in% climGraphOpt$proba$title$position]

    stateGpTlt <- if(climGraphOpt$proba$title$is.title) 'normal' else 'disabled'

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

    frameGraphPlot <- ttklabelframe(frDialog, text = lang.dlg[['label']][['6']], relief = 'groove')

    plot.type <- tclVar(climGraphOpt$proba$plot$type)

    txt.pltType <- tklabel(frameGraphPlot, text = lang.dlg[['label']][['7']], anchor = 'e', justify = 'right')
    cb.pltType <- ttkcombobox(frameGraphPlot, values = c('both', 'line'), textvariable = plot.type, width = 4)

    #########
    tkbind(cb.pltType, "<<ComboboxSelected>>", function(){
        statePlotPoints <- if(tclvalue(plot.type) == 'both') 'normal' else 'disabled'
        tkconfigure(bt.pltPointC, state = statePlotPoints)
        tkconfigure(spin.pltPointS, state = statePlotPoints)
    })

    #######
    framepltLine <- ttklabelframe(frameGraphPlot, text = lang.dlg[['label']][['8']], relief = 'groove')

    plot.col.line <- tclVar(climGraphOpt$proba$plot$col$line)

    txt.pltLineC <- tklabel(framepltLine, text = lang.dlg[['label']][['9']], anchor = 'e', justify = 'right')
    bt.pltLineC <- tkbutton(framepltLine, bg = tclvalue(plot.col.line), width = width.col)
    txt.pltLineW <- tklabel(framepltLine, text = lang.dlg[['label']][['10']], anchor = 'e', justify = 'right')
    spin.pltLineW <- ttkspinbox(framepltLine, from = 0.5, to = 4, increment = 0.1, justify = 'center', width = width.spin)
    tkset(spin.pltLineW, climGraphOpt$proba$plot$lwd)

    tkgrid(txt.pltLineC, bt.pltLineC, txt.pltLineW, spin.pltLineW)

    #######
    tkconfigure(bt.pltLineC, command = function(){
        loko <- trimws(tclvalue(tcl("tk_chooseColor", initialcolor = tclvalue(plot.col.line), title = lang.dlg[['label']][['13']])))
        if(nchar(loko) > 0){
            tkconfigure(bt.pltLineC, bg = loko)
            tclvalue(plot.col.line) <- loko
        }
    })

    #######
    framepltPoints <- ttklabelframe(frameGraphPlot, text = lang.dlg[['label']][['11']], relief = 'groove')

    plot.col.points <- tclVar(climGraphOpt$proba$plot$col$points)
    statePlotPoints <- if(climGraphOpt$proba$plot$type == 'both') 'normal' else 'disabled'

    txt.pltPointC <- tklabel(framepltPoints, text = lang.dlg[['label']][['9']], anchor = 'e', justify = 'right')
    bt.pltPointC <- tkbutton(framepltPoints, bg = tclvalue(plot.col.points), width = width.col, state = statePlotPoints)
    txt.pltPointS <- tklabel(framepltPoints, text = lang.dlg[['label']][['12']], anchor = 'e', justify = 'right')
    spin.pltPointS <- ttkspinbox(framepltPoints, from = 0.5, to = 2.5, increment = 0.1, justify = 'center', width = width.spin, state = statePlotPoints)
    tkset(spin.pltPointS, climGraphOpt$proba$plot$cex)

    tkgrid(txt.pltPointC, bt.pltPointC, txt.pltPointS, spin.pltPointS)

    #######
    tkconfigure(bt.pltPointC, command = function(){
        loko <- trimws(tclvalue(tcl("tk_chooseColor", initialcolor = tclvalue(plot.col.points), title = lang.dlg[['label']][['13']])))
        if(nchar(loko) > 0){
            tkconfigure(bt.pltPointC, bg = loko)
            tclvalue(plot.col.points) <- loko
        }
    })

    #######
    tkgrid(txt.pltType, row = 0, column = 0, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
    tkgrid(cb.pltType, row = 0, column = 1, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
    tkgrid(framepltLine, row = 0, column = 2, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
    tkgrid(framepltPoints, row = 0, column = 3, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)

    #####################

    frameGraphProba <- ttklabelframe(frDialog, text = lang.dlg[['label']][['14']], relief = 'groove')

    is.probaTheo <- tclVar(climGraphOpt$proba$proba$theoretical)
    col.probaTheo <- tclVar(climGraphOpt$proba$proba$col)

    chk.probaTheo <- tkcheckbutton(frameGraphProba, variable = is.probaTheo, text = lang.dlg[['checkbutton']][['1']], anchor = 'w', justify = 'left')
    txt.probaTheoC <- tklabel(frameGraphProba, text = lang.dlg[['label']][['9']], anchor = 'e', justify = 'right')
    bt.probaTheoC <- tkbutton(frameGraphProba, bg = tclvalue(col.probaTheo), width = width.col)
    txt.probaTheoW <- tklabel(frameGraphProba, text = lang.dlg[['label']][['10']], anchor = 'e', justify = 'right')
    spin.probaTheoW <- ttkspinbox(frameGraphProba, from = 0.5, to = 4, increment = 0.1, justify = 'center', width = width.spin)
    tkset(spin.probaTheoW, climGraphOpt$proba$proba$lwd)

    tkgrid(chk.probaTheo, tklabel(frameGraphProba, width = 5), txt.probaTheoC, bt.probaTheoC, txt.probaTheoW, spin.probaTheoW)

    #######
    tkconfigure(bt.probaTheoC, command = function(){
        loko <- trimws(tclvalue(tcl("tk_chooseColor", initialcolor = tclvalue(col.probaTheo), title = lang.dlg[['label']][['13']])))
        if(nchar(loko) > 0){
            tkconfigure(bt.probaTheoC, bg = loko)
            tclvalue(col.probaTheo) <- loko
        }
    })

    #####################
    tkgrid(frameGraphXYlim, row = 0, column = 0, sticky = '', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
    tkgrid(frameGraphAxLabs, row = 1, column = 0, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
    tkgrid(frameGraphTitle, row = 2, column = 0, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
    tkgrid(frameGraphPlot, row = 3, column = 0, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
    tkgrid(frameGraphProba, row = 4, column = 0, sticky = '', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)

    #####################
    bt.opt.OK <- ttkbutton(frButt, text = .cdtEnv$tcl$lang$global[['button']][['1']])
    bt.opt.CA <- ttkbutton(frButt, text = .cdtEnv$tcl$lang$global[['button']][['2']])

    tkconfigure(bt.opt.OK, command = function(){
        climGraphOpt$proba$xlim$is.min <<- switch(tclvalue(is.min.xlim), '0' = FALSE, '1' = TRUE)
        climGraphOpt$proba$xlim$is.max <<- switch(tclvalue(is.max.xlim), '0' = FALSE, '1' = TRUE)
        climGraphOpt$proba$xlim$min <<- trimws(tclvalue(min.xlim))
        climGraphOpt$proba$xlim$max <<- trimws(tclvalue(max.xlim))

        climGraphOpt$proba$ylim$is.min <<- switch(tclvalue(is.min.ylim), '0' = FALSE, '1' = TRUE)
        climGraphOpt$proba$ylim$is.max <<- switch(tclvalue(is.max.ylim), '0' = FALSE, '1' = TRUE)
        climGraphOpt$proba$ylim$min <<- as.numeric(trimws(tclvalue(min.ylim)))
        climGraphOpt$proba$ylim$max <<- as.numeric(trimws(tclvalue(max.ylim)))

        climGraphOpt$proba$axislabs$is.xlab <<- switch(tclvalue(is.xaxis.lab), '0' = FALSE, '1' = TRUE)
        climGraphOpt$proba$axislabs$is.ylab <<- switch(tclvalue(is.yaxis.lab), '0' = FALSE, '1' = TRUE)
        climGraphOpt$proba$axislabs$xlab <<- gsub('\\\\n', '\n', trimws(tclvalue(xaxis.lab)))
        climGraphOpt$proba$axislabs$ylab <<- gsub('\\\\n', '\n', trimws(tclvalue(yaxis.lab)))

        climGraphOpt$proba$title$is.title <<- switch(tclvalue(is.title), '0' = FALSE, '1' = TRUE)
        climGraphOpt$proba$title$title <<- gsub('\\\\n', '\n', trimws(tclvalue(text.title)))
        climGraphOpt$proba$title$position <<- posTitleVAL[CbposTitleVAL %in% trimws(tclvalue(pos.title))]

        climGraphOpt$proba$plot$type <<- trimws(tclvalue(plot.type))
        climGraphOpt$proba$plot$col$line <<- tclvalue(plot.col.line)
        climGraphOpt$proba$plot$lwd <<- as.numeric(trimws(tclvalue(tkget(spin.pltLineW))))
        climGraphOpt$proba$plot$col$points <<- tclvalue(plot.col.points)
        climGraphOpt$proba$plot$cex <<- as.numeric(trimws(tclvalue(tkget(spin.pltPointS))))

        climGraphOpt$proba$proba$theoretical <<- switch(tclvalue(is.probaTheo), '0' = FALSE, '1' = TRUE)
        climGraphOpt$proba$proba$col <<- tclvalue(col.probaTheo)
        climGraphOpt$proba$proba$lwd <<- as.numeric(trimws(tclvalue(tkget(spin.probaTheoW))))

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

    ##################################################################
    tkfocus(tt)
    tkbind(tt, "<Destroy>", function(){
        tkgrab.release(tt)
        tkfocus(parent.win)
    })
    tkwait.window(tt)
    return(climGraphOpt)
}

#######################################################################################################

MapGraph.GraphOptions.LineENSO <- function(climGraphOpt, parent.win = .cdtEnv$tcl$main$win){
    if(WindowsOS()){
        largeur1 <- 68
        largeur2 <- 72
        largeur3 <- 44
        width.xlim <- 11
        width.ylim <- 5
        width.col <- 3
        width.col1 <- 4
        width.spin <- 4
    }else{
        largeur1 <- 64
        largeur2 <- 68
        largeur3 <- 41
        width.xlim <- 11
        width.ylim <- 5
        width.col <- 1
        width.col1 <- 2
        width.spin <- 4
    }

    #####################
    xml.dlg <- file.path(.cdtDir$dirLocal, "languages", "cdtGraphOptions.LineENSO_dlgBox.xml")
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

    is.min.xlim <- tclVar(climGraphOpt$line.enso$xlim$is.min)
    is.max.xlim <- tclVar(climGraphOpt$line.enso$xlim$is.max)
    min.xlim <- tclVar(climGraphOpt$line.enso$xlim$min)
    max.xlim <- tclVar(climGraphOpt$line.enso$xlim$max)

    stateMinXlim <- if(climGraphOpt$line.enso$xlim$is.min) 'normal' else 'disabled'
    stateMaxXlim <- if(climGraphOpt$line.enso$xlim$is.max) 'normal' else 'disabled'

    chk.min.Xlim <- tkcheckbutton(frameGraphXlim, variable = is.min.xlim, text = "Min", anchor = 'w', justify = 'left')
    en.min.Xlim <- tkentry(frameGraphXlim, textvariable = min.xlim, width = width.xlim, state = stateMinXlim)
    chk.max.Xlim <- tkcheckbutton(frameGraphXlim, variable = is.max.xlim, text = "Max", anchor = 'w', justify = 'left')
    en.max.Xlim <- tkentry(frameGraphXlim, textvariable = max.xlim, width = width.xlim, state = stateMaxXlim)

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

    #####################

    frameGraphYlim <- ttklabelframe(frameGraphXYlim, text = lang.dlg[['label']][['2']], relief = 'groove')

    is.min.ylim <- tclVar(climGraphOpt$line.enso$ylim$is.min)
    is.max.ylim <- tclVar(climGraphOpt$line.enso$ylim$is.max)
    min.ylim <- tclVar(climGraphOpt$line.enso$ylim$min)
    max.ylim <- tclVar(climGraphOpt$line.enso$ylim$max)

    stateMinYlim <- if(climGraphOpt$line.enso$ylim$is.min) 'normal' else 'disabled'
    stateMaxYlim <- if(climGraphOpt$line.enso$ylim$is.max) 'normal' else 'disabled'

    chk.min.Ylim <- tkcheckbutton(frameGraphYlim, variable = is.min.ylim, text = "Min", anchor = 'w', justify = 'left')
    en.min.Ylim <- tkentry(frameGraphYlim, textvariable = min.ylim, width = width.ylim, state = stateMinYlim)
    chk.max.Ylim <- tkcheckbutton(frameGraphYlim, variable = is.max.ylim, text = "Max", anchor = 'w', justify = 'left')
    en.max.Ylim <- tkentry(frameGraphYlim, textvariable = max.ylim, width = width.ylim, state = stateMaxYlim)

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

    sepXYlim <- tklabel(frameGraphXYlim, text = "", width = 6)

    tkgrid(frameGraphXlim, row = 0, column = 0, sticky = 'w')
    tkgrid(sepXYlim, row = 0, column = 1, sticky = 'we')
    tkgrid(frameGraphYlim, row = 0, column = 2, sticky = 'e')

    #####################

    frameGraphAxLabs <- ttklabelframe(frDialog, text = lang.dlg[['label']][['3']], relief = 'groove')

    is.xaxis.lab <- tclVar(climGraphOpt$line.enso$axislabs$is.xlab)
    is.yaxis.lab <- tclVar(climGraphOpt$line.enso$axislabs$is.ylab)
    xaxis.lab <- tclVar(climGraphOpt$line.enso$axislabs$xlab)
    yaxis.lab <- tclVar(climGraphOpt$line.enso$axislabs$ylab)

    stateXLab <- if(climGraphOpt$line.enso$axislabs$is.xlab) 'normal' else 'disabled'
    stateYLab <- if(climGraphOpt$line.enso$axislabs$is.ylab) 'normal' else 'disabled'

    chk.Xlab <- tkcheckbutton(frameGraphAxLabs, variable = is.xaxis.lab, text = 'Xlab', anchor = 'w', justify = 'left')
    en.Xlab <- tkentry(frameGraphAxLabs, textvariable = xaxis.lab, width = largeur1, state = stateXLab)
    chk.Ylab <- tkcheckbutton(frameGraphAxLabs, variable = is.yaxis.lab, text = 'Ylab', anchor = 'w', justify = 'left')
    en.Ylab <- tkentry(frameGraphAxLabs, textvariable = yaxis.lab, width = largeur1, state = stateYLab)

    tkgrid(chk.Xlab, row = 0, column = 0, sticky = 'e')
    tkgrid(en.Xlab, row = 0, column = 1, sticky = 'we')
    tkgrid(chk.Ylab, row = 1, column = 0, sticky = 'e')
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

    is.title <- tclVar(climGraphOpt$line.enso$title$is.title)
    text.title <- tclVar(climGraphOpt$line.enso$title$title)

    pos.title <- tclVar()
    CbposTitleVAL <- lang.dlg[['combobox']][['1']]
    posTitleVAL <- c("top", "bottom")
    tclvalue(pos.title) <- CbposTitleVAL[posTitleVAL %in% climGraphOpt$line.enso$title$position]

    stateGpTlt <- if(climGraphOpt$line.enso$title$is.title) 'normal' else 'disabled'

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

    frameGraphPlot <- ttklabelframe(frDialog, text = lang.dlg[['label']][['6']], relief = 'groove')

    #########
    framepltLine <- ttklabelframe(frameGraphPlot, text = lang.dlg[['label']][['7']], relief = 'groove')

    plot.col.line <- tclVar(climGraphOpt$line.enso$plot$col$line)

    txt.pltLineC <- tklabel(framepltLine, text = lang.dlg[['label']][['8']], anchor = 'e', justify = 'right')
    bt.pltLineC <- tkbutton(framepltLine, bg = tclvalue(plot.col.line), width = width.col1)
    txt.pltLineW <- tklabel(framepltLine, text = lang.dlg[['label']][['9']], anchor = 'e', justify = 'right')
    spin.pltLineW <- ttkspinbox(framepltLine, from = 0.5, to = 4, increment = 0.1, justify = 'center', width = width.spin)
    tkset(spin.pltLineW, climGraphOpt$line.enso$plot$lwd)

    tkgrid(txt.pltLineC, bt.pltLineC, txt.pltLineW, spin.pltLineW)

    #########
    tkconfigure(bt.pltLineC, command = function(){
        loko <- trimws(tclvalue(tcl("tk_chooseColor", initialcolor = tclvalue(plot.col.line), title = lang.dlg[['label']][['20']])))
        if(nchar(loko) > 0){
            tkconfigure(bt.pltLineC, bg = loko)
            tclvalue(plot.col.line) <- loko
        }
    })

    #########
    framepltPoints <- ttklabelframe(frameGraphPlot, text = lang.dlg[['label']][['11']], relief = 'groove')

    plot.col.nina <- tclVar(climGraphOpt$line.enso$plot$col$points[1])
    plot.col.neutre <- tclVar(climGraphOpt$line.enso$plot$col$points[2])
    plot.col.nino <- tclVar(climGraphOpt$line.enso$plot$col$points[3])

    txt.pltPointNa <- tklabel(framepltPoints, text = lang.dlg[['label']][['12']], anchor = 'e', justify = 'right')
    bt.pltPointNa <- tkbutton(framepltPoints, bg = tclvalue(plot.col.nina), width = width.col1)
    txt.pltPointNe <- tklabel(framepltPoints, text = lang.dlg[['label']][['13']], anchor = 'e', justify = 'right')
    bt.pltPointNe <- tkbutton(framepltPoints, bg = tclvalue(plot.col.neutre), width = width.col1)
    txt.pltPointNo <- tklabel(framepltPoints, text = lang.dlg[['label']][['14']], anchor = 'e', justify = 'right')
    bt.pltPointNo <- tkbutton(framepltPoints, bg = tclvalue(plot.col.nino), width = width.col1)
    txt.pltPointS <- tklabel(framepltPoints, text = lang.dlg[['label']][['15']], anchor = 'e', justify = 'right')
    spin.pltPointS <- ttkspinbox(framepltPoints, from = 0.5, to = 2.5, increment = 0.1, justify = 'center', width = width.spin)
    tkset(spin.pltPointS, climGraphOpt$line.enso$plot$cex)

    tkgrid(txt.pltPointNa, bt.pltPointNa, txt.pltPointNe, bt.pltPointNe, txt.pltPointNo, bt.pltPointNo, txt.pltPointS, spin.pltPointS)

    #########
    tkconfigure(bt.pltPointNa, command = function(){
        loko <- trimws(tclvalue(tcl("tk_chooseColor", initialcolor = tclvalue(plot.col.nina), title = lang.dlg[['label']][['20']])))
        if(nchar(loko) > 0){
            tkconfigure(bt.pltPointNa, bg = loko)
            tclvalue(plot.col.nina) <- loko
        }
    })

    tkconfigure(bt.pltPointNe, command = function(){
        loko <- trimws(tclvalue(tcl("tk_chooseColor", initialcolor = tclvalue(plot.col.neutre), title = lang.dlg[['label']][['20']])))
        if(nchar(loko) > 0){
            tkconfigure(bt.pltPointNe, bg = loko)
            tclvalue(plot.col.neutre) <- loko
        }
    })

    tkconfigure(bt.pltPointNo, command = function(){
        loko <- trimws(tclvalue(tcl("tk_chooseColor", initialcolor = tclvalue(plot.col.nino), title = lang.dlg[['label']][['20']])))
        if(nchar(loko) > 0){
            tkconfigure(bt.pltPointNo, bg = loko)
            tclvalue(plot.col.nino) <- loko
        }
    })

    ###########
    tkgrid(framepltLine, row = 0, column = 0, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1)
    tkgrid(framepltPoints, row = 0, column = 1, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1)

    #####################

    frameGraphLegend <- ttklabelframe(frDialog, text = lang.dlg[['label']][['16']], relief = 'groove')

    frameLezMean <- ttklabelframe(frameGraphLegend, text = lang.dlg[['label']][['17']], relief = 'groove')

    is.lezMean <- tclVar(climGraphOpt$line.enso$legend$is$mean)
    col.lezMean <- tclVar(climGraphOpt$line.enso$legend$col$mean)
    text.lezMean <- tclVar(climGraphOpt$line.enso$legend$text$mean)
    stateLezMean <- if(climGraphOpt$line.enso$legend$is$mean & climGraphOpt$line.enso$legend$add$mean) "normal" else "disabled"

    chk.lezMean <- tkcheckbutton(frameLezMean, variable = is.lezMean, anchor = 'e', justify = 'right')
    txt.lezMeanC <- tklabel(frameLezMean, text = lang.dlg[['label']][['8']], anchor = 'e', justify = 'right')
    bt.lezMeanC <- tkbutton(frameLezMean, bg = tclvalue(col.lezMean), width = width.col, state = stateLezMean)
    txt.lezMeanW <- tklabel(frameLezMean, text = lang.dlg[['label']][['9']], anchor = 'e', justify = 'right')
    spin.lezMeanW <- ttkspinbox(frameLezMean, from = 0.5, to = 4, increment = 0.1, justify = 'center', width = width.spin, state = stateLezMean)
    tkset(spin.lezMeanW, climGraphOpt$line.enso$legend$lwd$mean)
    txt.lezMeanT <- tklabel(frameLezMean, text = lang.dlg[['label']][['10']], anchor = 'e', justify = 'right')
    en.lezMeanT <- tkentry(frameLezMean, textvariable = text.lezMean, width = largeur3, state = stateLezMean)

    tkgrid(chk.lezMean, txt.lezMeanC, bt.lezMeanC, txt.lezMeanW, spin.lezMeanW, txt.lezMeanT, en.lezMeanT)

    #####
    tkconfigure(bt.lezMeanC, command = function(){
        loko <- trimws(tclvalue(tcl("tk_chooseColor", initialcolor = tclvalue(col.lezMean), title = lang.dlg[['label']][['20']])))
        if(nchar(loko) > 0){
            tkconfigure(bt.lezMeanC, bg = loko)
            tclvalue(col.lezMean) <- loko
        }
    })

    tkbind(chk.lezMean, "<Button-1>", function(){
        stateLezMean <- if(climGraphOpt$line.enso$legend$add$mean & tclvalue(is.lezMean) == '0') 'normal' else 'disabled'
        tkconfigure(bt.lezMeanC, state = stateLezMean)
        tkconfigure(spin.lezMeanW, state = stateLezMean)
        tkconfigure(en.lezMeanT, state = stateLezMean)
    })

    #########
    frameLezLin <- ttklabelframe(frameGraphLegend, text = lang.dlg[['label']][['18']], relief = 'groove')

    is.lezLin <- tclVar(climGraphOpt$line.enso$legend$is$linear)
    col.lezLin <- tclVar(climGraphOpt$line.enso$legend$col$linear)
    text.lezLin <- tclVar(climGraphOpt$line.enso$legend$text$linear)
    stateLezLin <- if(climGraphOpt$line.enso$legend$is$linear & climGraphOpt$line.enso$legend$add$linear) "normal" else "disabled"

    chk.lezLin <- tkcheckbutton(frameLezLin, variable = is.lezLin, anchor = 'e', justify = 'right')
    txt.lezLinC <- tklabel(frameLezLin, text = lang.dlg[['label']][['8']], anchor = 'e', justify = 'right')
    bt.lezLinC <- tkbutton(frameLezLin, bg = tclvalue(col.lezLin), width = width.col, state = stateLezLin)
    txt.lezLinW <- tklabel(frameLezLin, text = lang.dlg[['label']][['9']], anchor = 'e', justify = 'right')
    spin.lezLinW <- ttkspinbox(frameLezLin, from = 0.5, to = 4, increment = 0.1, justify = 'center', width = width.spin, state = stateLezLin)
    tkset(spin.lezLinW, climGraphOpt$line.enso$legend$lwd$linear)
    txt.lezLinT <- tklabel(frameLezLin, text = lang.dlg[['label']][['10']], anchor = 'e', justify = 'right')
    en.lezLinT <- tkentry(frameLezLin, textvariable = text.lezLin, width = largeur3, state = stateLezLin)

    tkgrid(chk.lezLin, txt.lezLinC, bt.lezLinC, txt.lezLinW, spin.lezLinW, txt.lezLinT, en.lezLinT)

    #####
    tkconfigure(bt.lezLinC, command = function(){
        loko <- trimws(tclvalue(tcl("tk_chooseColor", initialcolor = tclvalue(col.lezLin), title = lang.dlg[['label']][['20']])))
        if(nchar(loko) > 0){
            tkconfigure(bt.lezLinC, bg = loko)
            tclvalue(col.lezLin) <- loko
        }
    })

    tkbind(chk.lezLin, "<Button-1>", function(){
        stateLezLin <- if(climGraphOpt$line.enso$legend$add$linear & tclvalue(is.lezLin) == '0') 'normal' else 'disabled'
        tkconfigure(bt.lezLinC, state = stateLezLin)
        tkconfigure(spin.lezLinW, state = stateLezLin)
        tkconfigure(en.lezLinT, state = stateLezLin)
    })

    #########

    frameLezTer <- ttklabelframe(frameGraphLegend, text = lang.dlg[['label']][['19']], relief = 'groove')

    is.lezTer <- tclVar(climGraphOpt$line.enso$legend$is$tercile)
    col.lezTer1 <- tclVar(climGraphOpt$line.enso$legend$col$tercile1)
    col.lezTer2 <- tclVar(climGraphOpt$line.enso$legend$col$tercile2)
    text.lezTer1 <- tclVar(climGraphOpt$line.enso$legend$text$tercile1)
    text.lezTer2 <- tclVar(climGraphOpt$line.enso$legend$text$tercile2)

    stateLezTer <- if(climGraphOpt$line.enso$legend$is$tercile & climGraphOpt$line.enso$legend$add$tercile) "normal" else "disabled"

    chk.lezTer <- tkcheckbutton(frameLezTer, variable = is.lezTer, anchor = 'e', justify = 'right')
    txt.lezTerC1 <- tklabel(frameLezTer, text = lang.dlg[['label']][['8']], anchor = 'e', justify = 'right')
    bt.lezTerC1 <- tkbutton(frameLezTer, bg = tclvalue(col.lezTer1), width = width.col, state = stateLezTer)
    txt.lezTerW <- tklabel(frameLezTer, text = lang.dlg[['label']][['9']], anchor = 'e', justify = 'right')
    spin.lezTerW <- ttkspinbox(frameLezTer, from = 0.5, to = 4, increment = 0.1, justify = 'center', width = width.spin, state = stateLezTer)
    tkset(spin.lezTerW, climGraphOpt$line.enso$legend$lwd$tercile)
    txt.lezTerT1 <- tklabel(frameLezTer, text = lang.dlg[['label']][['10']], anchor = 'e', justify = 'right')
    en.lezTerT1 <- tkentry(frameLezTer, textvariable = text.lezTer1, width = largeur3, state = stateLezTer)
    txt.lezTerC2 <- tklabel(frameLezTer, text = lang.dlg[['label']][['8']], anchor = 'e', justify = 'right')
    bt.lezTerC2 <- tkbutton(frameLezTer, bg = tclvalue(col.lezTer2), width = width.col, state = stateLezTer)
    txt.lezTerT2 <- tklabel(frameLezTer, text = lang.dlg[['label']][['10']], anchor = 'e', justify = 'right')
    en.lezTerT2 <- tkentry(frameLezTer, textvariable = text.lezTer2, width = largeur3, state = stateLezTer)

    tkgrid(chk.lezTer, txt.lezTerC1, bt.lezTerC1, txt.lezTerW, spin.lezTerW, txt.lezTerT1, en.lezTerT1)
    tkgrid(tklabel(frameLezTer), txt.lezTerC2, bt.lezTerC2, tklabel(frameLezTer), tklabel(frameLezTer), txt.lezTerT2, en.lezTerT2)

    #####
    tkconfigure(bt.lezTerC1, command = function(){
        loko <- trimws(tclvalue(tcl("tk_chooseColor", initialcolor = tclvalue(col.lezTer1), title = lang.dlg[['label']][['20']])))
        if(nchar(loko) > 0){
            tkconfigure(bt.lezTerC1, bg = loko)
            tclvalue(col.lezTer1) <- loko
        }
    })

    tkconfigure(bt.lezTerC2, command = function(){
        loko <- trimws(tclvalue(tcl("tk_chooseColor", initialcolor = tclvalue(col.lezTer2), title = lang.dlg[['label']][['20']])))
        if(nchar(loko) > 0){
            tkconfigure(bt.lezTerC2, bg = loko)
            tclvalue(col.lezTer2) <- loko
        }
    })

    #####
    tkbind(chk.lezTer, "<Button-1>", function(){
        stateLezTer <- if(climGraphOpt$line.enso$legend$add$tercile & tclvalue(is.lezTer) == '0') 'normal' else 'disabled'
        tkconfigure(bt.lezTerC1, state = stateLezTer)
        tkconfigure(spin.lezTerW, state = stateLezTer)
        tkconfigure(en.lezTerT1, state = stateLezTer)
        tkconfigure(bt.lezTerC2, state = stateLezTer)
        tkconfigure(en.lezTerT2, state = stateLezTer)
    })

    #########

    tkgrid(frameLezMean, row = 0, column = 0, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
    tkgrid(frameLezLin, row = 1, column = 0, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
    tkgrid(frameLezTer, row = 2, column = 0, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)

    #####################
    tkgrid(frameGraphXYlim, row = 0, column = 0, sticky = '', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
    tkgrid(frameGraphAxLabs, row = 1, column = 0, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
    tkgrid(frameGraphTitle, row = 2, column = 0, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
    tkgrid(frameGraphPlot, row = 3, column = 0, sticky = '', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
    tkgrid(frameGraphLegend, row = 4, column = 0, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)

    #####################
    bt.opt.OK <- ttkbutton(frButt, text = .cdtEnv$tcl$lang$global[['button']][['1']])
    bt.opt.CA <- ttkbutton(frButt, text = .cdtEnv$tcl$lang$global[['button']][['2']])

    tkconfigure(bt.opt.OK, command = function(){
        climGraphOpt$line.enso$xlim$is.min <<- switch(tclvalue(is.min.xlim), '0' = FALSE, '1' = TRUE)
        climGraphOpt$line.enso$xlim$is.max <<- switch(tclvalue(is.max.xlim), '0' = FALSE, '1' = TRUE)
        climGraphOpt$line.enso$xlim$min <<- trimws(tclvalue(min.xlim))
        climGraphOpt$line.enso$xlim$max <<- trimws(tclvalue(max.xlim))

        climGraphOpt$line.enso$ylim$is.min <<- switch(tclvalue(is.min.ylim), '0' = FALSE, '1' = TRUE)
        climGraphOpt$line.enso$ylim$is.max <<- switch(tclvalue(is.max.ylim), '0' = FALSE, '1' = TRUE)
        climGraphOpt$line.enso$ylim$min <<- as.numeric(trimws(tclvalue(min.ylim)))
        climGraphOpt$line.enso$ylim$max <<- as.numeric(trimws(tclvalue(max.ylim)))

        climGraphOpt$line.enso$axislabs$is.xlab <<- switch(tclvalue(is.xaxis.lab), '0' = FALSE, '1' = TRUE)
        climGraphOpt$line.enso$axislabs$is.ylab <<- switch(tclvalue(is.yaxis.lab), '0' = FALSE, '1' = TRUE)
        climGraphOpt$line.enso$axislabs$xlab <<- gsub('\\\\n', '\n', trimws(tclvalue(xaxis.lab)))
        climGraphOpt$line.enso$axislabs$ylab <<- gsub('\\\\n', '\n', trimws(tclvalue(yaxis.lab)))

        climGraphOpt$line.enso$title$is.title <<- switch(tclvalue(is.title), '0' = FALSE, '1' = TRUE)
        climGraphOpt$line.enso$title$title <<- gsub('\\\\n', '\n', trimws(tclvalue(text.title)))
        climGraphOpt$line.enso$title$position <<- posTitleVAL[CbposTitleVAL %in% trimws(tclvalue(pos.title))]

        climGraphOpt$line.enso$plot$col$line <<- tclvalue(plot.col.line)
        climGraphOpt$line.enso$plot$lwd <<- as.numeric(trimws(tclvalue(tkget(spin.pltLineW))))
        climGraphOpt$line.enso$plot$col$points <<- c(tclvalue(plot.col.nina), tclvalue(plot.col.neutre), tclvalue(plot.col.nino))
        climGraphOpt$line.enso$plot$cex <<- as.numeric(trimws(tclvalue(tkget(spin.pltPointS))))

        climGraphOpt$line.enso$legend$is$mean <<- switch(tclvalue(is.lezMean), '0' = FALSE, '1' = TRUE)
        climGraphOpt$line.enso$legend$col$mean <<- tclvalue(col.lezMean)
        climGraphOpt$line.enso$legend$lwd$mean <<- as.numeric(trimws(tclvalue(tkget(spin.lezMeanW))))
        climGraphOpt$line.enso$legend$text$mean <<- trimws(tclvalue(text.lezMean))

        climGraphOpt$line.enso$legend$is$linear <<- switch(tclvalue(is.lezLin), '0' = FALSE, '1' = TRUE)
        climGraphOpt$line.enso$legend$col$linear <<- tclvalue(col.lezLin)
        climGraphOpt$line.enso$legend$lwd$linear <<- as.numeric(trimws(tclvalue(tkget(spin.lezLinW))))
        climGraphOpt$line.enso$legend$text$linear <<- trimws(tclvalue(text.lezLin))

        climGraphOpt$line.enso$legend$is$tercile <<- switch(tclvalue(is.lezTer), '0' = FALSE, '1' = TRUE)
        climGraphOpt$line.enso$legend$col$tercile1 <<- tclvalue(col.lezTer1)
        climGraphOpt$line.enso$legend$col$tercile2 <<- tclvalue(col.lezTer2)
        climGraphOpt$line.enso$legend$lwd$tercile <<- as.numeric(trimws(tclvalue(tkget(spin.lezTerW))))
        climGraphOpt$line.enso$legend$text$tercile1 <<- trimws(tclvalue(text.lezTer1))
        climGraphOpt$line.enso$legend$text$tercile2 <<- trimws(tclvalue(text.lezTer2))

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
    tkwm.title(tt, "Options")
    tkwm.deiconify(tt)

    ##################################################################
    tkfocus(tt)
    tkbind(tt, "<Destroy>", function(){
        tkgrab.release(tt)
        tkfocus(parent.win)
    })
    tkwait.window(tt)
    return(climGraphOpt)
}

#######################################################################################################

MapGraph.GraphOptions.BarENSO <- function(climGraphOpt, parent.win = .cdtEnv$tcl$main$win){
    if(WindowsOS()){
        largeur1 <- 64
        largeur2 <- 68
        width.xlim <- 11
        width.ylim <- 5
        width.col <- 3
    }else{
        largeur1 <- 60
        largeur2 <- 64
        width.xlim <- 11
        width.ylim <- 5
        width.col <- 1
    }

    #####################
    xml.dlg <- file.path(.cdtDir$dirLocal, "languages", "cdtGraphOptions.BarENSO_dlgBox.xml")
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

    is.min.xlim <- tclVar(climGraphOpt$bar.enso$xlim$is.min)
    is.max.xlim <- tclVar(climGraphOpt$bar.enso$xlim$is.max)
    min.xlim <- tclVar(climGraphOpt$bar.enso$xlim$min)
    max.xlim <- tclVar(climGraphOpt$bar.enso$xlim$max)

    stateMinXlim <- if(climGraphOpt$bar.enso$xlim$is.min) 'normal' else 'disabled'
    stateMaxXlim <- if(climGraphOpt$bar.enso$xlim$is.max) 'normal' else 'disabled'

    chk.min.Xlim <- tkcheckbutton(frameGraphXlim, variable = is.min.xlim, text = "Min", anchor = 'w', justify = 'left')
    en.min.Xlim <- tkentry(frameGraphXlim, textvariable = min.xlim, width = width.xlim, state = stateMinXlim)
    chk.max.Xlim <- tkcheckbutton(frameGraphXlim, variable = is.max.xlim, text = "Max", anchor = 'w', justify = 'left')
    en.max.Xlim <- tkentry(frameGraphXlim, textvariable = max.xlim, width = width.xlim, state = stateMaxXlim)

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

    #####################

    frameGraphYlim <- ttklabelframe(frameGraphXYlim, text = lang.dlg[['label']][['2']], relief = 'groove')

    is.min.ylim <- tclVar(climGraphOpt$bar.enso$ylim$is.min)
    is.max.ylim <- tclVar(climGraphOpt$bar.enso$ylim$is.max)
    min.ylim <- tclVar(climGraphOpt$bar.enso$ylim$min)
    max.ylim <- tclVar(climGraphOpt$bar.enso$ylim$max)

    stateMinYlim <- if(climGraphOpt$bar.enso$ylim$is.min) 'normal' else 'disabled'
    stateMaxYlim <- if(climGraphOpt$bar.enso$ylim$is.max) 'normal' else 'disabled'

    chk.min.Ylim <- tkcheckbutton(frameGraphYlim, variable = is.min.ylim, text = "Min", anchor = 'w', justify = 'left')
    en.min.Ylim <- tkentry(frameGraphYlim, textvariable = min.ylim, width = width.ylim, state = stateMinYlim)
    chk.max.Ylim <- tkcheckbutton(frameGraphYlim, variable = is.max.ylim, text = "Max", anchor = 'w', justify = 'left')
    en.max.Ylim <- tkentry(frameGraphYlim, textvariable = max.ylim, width = width.ylim, state = stateMaxYlim)

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

    sepXYlim <- tklabel(frameGraphXYlim, text = "", width = 2)

    tkgrid(frameGraphXlim, row = 0, column = 0, sticky = 'w')
    tkgrid(sepXYlim, row = 0, column = 1, sticky = 'we')
    tkgrid(frameGraphYlim, row = 0, column = 2, sticky = 'e')

    #####################

    frameGraphAxLabs <- ttklabelframe(frDialog, text = lang.dlg[['label']][['3']], relief = 'groove')

    is.xaxis.lab <- tclVar(climGraphOpt$bar.enso$axislabs$is.xlab)
    is.yaxis.lab <- tclVar(climGraphOpt$bar.enso$axislabs$is.ylab)
    xaxis.lab <- tclVar(climGraphOpt$bar.enso$axislabs$xlab)
    yaxis.lab <- tclVar(climGraphOpt$bar.enso$axislabs$ylab)

    stateXLab <- if(climGraphOpt$bar.enso$axislabs$is.xlab) 'normal' else 'disabled'
    stateYLab <- if(climGraphOpt$bar.enso$axislabs$is.ylab) 'normal' else 'disabled'

    chk.Xlab <- tkcheckbutton(frameGraphAxLabs, variable = is.xaxis.lab, text = 'Xlab', anchor = 'w', justify = 'left')
    en.Xlab <- tkentry(frameGraphAxLabs, textvariable = xaxis.lab, width = largeur1, state = stateXLab)
    chk.Ylab <- tkcheckbutton(frameGraphAxLabs, variable = is.yaxis.lab, text = 'Ylab', anchor = 'w', justify = 'left')
    en.Ylab <- tkentry(frameGraphAxLabs, textvariable = yaxis.lab, width = largeur1, state = stateYLab)

    tkgrid(chk.Xlab, row = 0, column = 0, sticky = 'e')
    tkgrid(en.Xlab, row = 0, column = 1, sticky = 'we')
    tkgrid(chk.Ylab, row = 1, column = 0, sticky = 'e')
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

    is.title <- tclVar(climGraphOpt$bar.enso$title$is.title)
    text.title <- tclVar(climGraphOpt$bar.enso$title$title)

    pos.title <- tclVar()
    CbposTitleVAL <- lang.dlg[['combobox']][['1']]
    posTitleVAL <- c("top", "bottom")
    tclvalue(pos.title) <- CbposTitleVAL[posTitleVAL %in% climGraphOpt$bar.enso$title$position]

    stateGpTlt <- if(climGraphOpt$bar.enso$title$is.title) 'normal' else 'disabled'

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

    frameBarCol <- tkframe(frDialog)

    color.nina <- tclVar(climGraphOpt$bar.enso$colors$col[1])
    color.neutre <- tclVar(climGraphOpt$bar.enso$colors$col[2])
    color.nino <- tclVar(climGraphOpt$bar.enso$colors$col[3])

    txt.colBarNa <- tklabel(frameBarCol, text = lang.dlg[['label']][['6']], anchor = 'e', justify = 'right')
    bt.colBarNa <- tkbutton(frameBarCol, bg = tclvalue(color.nina), width = width.col)
    txt.colBarNe <- tklabel(frameBarCol, text = lang.dlg[['label']][['7']], anchor = 'e', justify = 'right')
    bt.colBarNe <- tkbutton(frameBarCol, bg = tclvalue(color.neutre), width = width.col)
    txt.colBarNo <- tklabel(frameBarCol, text = lang.dlg[['label']][['8']], anchor = 'e', justify = 'right')
    bt.colBarNo <- tkbutton(frameBarCol, bg = tclvalue(color.nino), width = width.col)

    tkgrid(txt.colBarNa, bt.colBarNa, txt.colBarNe, bt.colBarNe, txt.colBarNo, bt.colBarNo)

    #########
    tkconfigure(bt.colBarNa, command = function(){
        loko <- trimws(tclvalue(tcl("tk_chooseColor", initialcolor = tclvalue(color.nina), title = lang.dlg[['label']][['9']])))
        if(nchar(loko) > 0){
            tkconfigure(bt.colBarNa, bg = loko)
            tclvalue(color.nina) <- loko
        }
    })

    tkconfigure(bt.colBarNe, command = function(){
        loko <- trimws(tclvalue(tcl("tk_chooseColor", initialcolor = tclvalue(color.neutre), title = lang.dlg[['label']][['9']])))
        if(nchar(loko) > 0){
            tkconfigure(bt.colBarNe, bg = loko)
            tclvalue(color.neutre) <- loko
        }
    })

    tkconfigure(bt.colBarNo, command = function(){
        loko <- trimws(tclvalue(tcl("tk_chooseColor", initialcolor = tclvalue(color.nino), title = lang.dlg[['label']][['9']])))
        if(nchar(loko) > 0){
            tkconfigure(bt.colBarNo, bg = loko)
            tclvalue(color.nino) <- loko
        }
    })

    #####################

    tkgrid(frameGraphXYlim, row = 0, column = 0, sticky = '', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
    tkgrid(frameGraphAxLabs, row = 1, column = 0, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
    tkgrid(frameGraphTitle, row = 2, column = 0, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
    tkgrid(frameBarCol, row = 3, column = 0, sticky = '', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)

    #####################
    bt.opt.OK <- ttkbutton(frButt, text = .cdtEnv$tcl$lang$global[['button']][['1']])
    bt.opt.CA <- ttkbutton(frButt, text = .cdtEnv$tcl$lang$global[['button']][['2']])

    tkconfigure(bt.opt.OK, command = function(){
        climGraphOpt$bar.enso$xlim$is.min <<- switch(tclvalue(is.min.xlim), '0' = FALSE, '1' = TRUE)
        climGraphOpt$bar.enso$xlim$is.max <<- switch(tclvalue(is.max.xlim), '0' = FALSE, '1' = TRUE)
        climGraphOpt$bar.enso$xlim$min <<- trimws(tclvalue(min.xlim))
        climGraphOpt$bar.enso$xlim$max <<- trimws(tclvalue(max.xlim))

        climGraphOpt$bar.enso$ylim$is.min <<- switch(tclvalue(is.min.ylim), '0' = FALSE, '1' = TRUE)
        climGraphOpt$bar.enso$ylim$is.max <<- switch(tclvalue(is.max.ylim), '0' = FALSE, '1' = TRUE)
        climGraphOpt$bar.enso$ylim$min <<- as.numeric(trimws(tclvalue(min.ylim)))
        climGraphOpt$bar.enso$ylim$max <<- as.numeric(trimws(tclvalue(max.ylim)))

        climGraphOpt$bar.enso$axislabs$is.xlab <<- switch(tclvalue(is.xaxis.lab), '0' = FALSE, '1' = TRUE)
        climGraphOpt$bar.enso$axislabs$is.ylab <<- switch(tclvalue(is.yaxis.lab), '0' = FALSE, '1' = TRUE)
        climGraphOpt$bar.enso$axislabs$xlab <<- gsub('\\\\n', '\n', trimws(tclvalue(xaxis.lab)))
        climGraphOpt$bar.enso$axislabs$ylab <<- gsub('\\\\n', '\n', trimws(tclvalue(yaxis.lab)))

        climGraphOpt$bar.enso$title$is.title <<- switch(tclvalue(is.title), '0' = FALSE, '1' = TRUE)
        climGraphOpt$bar.enso$title$title <<- gsub('\\\\n', '\n', trimws(tclvalue(text.title)))
        climGraphOpt$bar.enso$title$position <<- posTitleVAL[CbposTitleVAL %in% trimws(tclvalue(pos.title))]

        climGraphOpt$bar.enso$colors$col <<- c(tclvalue(color.nina), tclvalue(color.neutre), tclvalue(color.nino))

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

    ##################################################################
    tkfocus(tt)
    tkbind(tt, "<Destroy>", function(){
        tkgrab.release(tt)
        tkfocus(parent.win)
    })
    tkwait.window(tt)
    return(climGraphOpt)
}

#######################################################################################################

MapGraph.GraphOptions.ProbaENSO <- function(climGraphOpt, parent.win = .cdtEnv$tcl$main$win){
    if(WindowsOS()){
        largeur1 <- 62
        largeur2 <- 66
        width.xlim <- 11
        width.ylim <- 5
        width.col <- 3
        width.spin <- 4
    }else{
        largeur1 <- 58
        largeur2 <- 62
        width.xlim <- 11
        width.ylim <- 5
        width.col <- 1
        width.spin <- 4
    }

    #####################
    xml.dlg <- file.path(.cdtDir$dirLocal, "languages", "cdtGraphOptions.ProbaENSO_dlgBox.xml")
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

    is.min.xlim <- tclVar(climGraphOpt$proba.enso$xlim$is.min)
    is.max.xlim <- tclVar(climGraphOpt$proba.enso$xlim$is.max)
    min.xlim <- tclVar(climGraphOpt$proba.enso$xlim$min)
    max.xlim <- tclVar(climGraphOpt$proba.enso$xlim$max)

    stateMinXlim <- if(climGraphOpt$proba.enso$xlim$is.min) 'normal' else 'disabled'
    stateMaxXlim <- if(climGraphOpt$proba.enso$xlim$is.max) 'normal' else 'disabled'

    chk.min.Xlim <- tkcheckbutton(frameGraphXlim, variable = is.min.xlim, text = "Min", anchor = 'w', justify = 'left')
    en.min.Xlim <- tkentry(frameGraphXlim, textvariable = min.xlim, width = width.xlim, state = stateMinXlim)
    chk.max.Xlim <- tkcheckbutton(frameGraphXlim, variable = is.max.xlim, text = "Max", anchor = 'w', justify = 'left')
    en.max.Xlim <- tkentry(frameGraphXlim, textvariable = max.xlim, width = width.xlim, state = stateMaxXlim)

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

    #####################

    frameGraphYlim <- ttklabelframe(frameGraphXYlim, text = lang.dlg[['label']][['2']], relief = 'groove')

    is.min.ylim <- tclVar(climGraphOpt$proba.enso$ylim$is.min)
    is.max.ylim <- tclVar(climGraphOpt$proba.enso$ylim$is.max)
    min.ylim <- tclVar(climGraphOpt$proba.enso$ylim$min)
    max.ylim <- tclVar(climGraphOpt$proba.enso$ylim$max)

    stateMinYlim <- if(climGraphOpt$proba.enso$ylim$is.min) 'normal' else 'disabled'
    stateMaxYlim <- if(climGraphOpt$proba.enso$ylim$is.max) 'normal' else 'disabled'

    chk.min.Ylim <- tkcheckbutton(frameGraphYlim, variable = is.min.ylim, text = "Min", anchor = 'w', justify = 'left')
    en.min.Ylim <- tkentry(frameGraphYlim, textvariable = min.ylim, width = width.ylim, state = stateMinYlim)
    chk.max.Ylim <- tkcheckbutton(frameGraphYlim, variable = is.max.ylim, text = "Max", anchor = 'w', justify = 'left')
    en.max.Ylim <- tkentry(frameGraphYlim, textvariable = max.ylim, width = width.ylim, state = stateMaxYlim)

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

    sepXYlim <- tklabel(frameGraphXYlim, text = "", width = 0)

    tkgrid(frameGraphXlim, row = 0, column = 0, sticky = 'w')
    tkgrid(sepXYlim, row = 0, column = 1, sticky = 'we')
    tkgrid(frameGraphYlim, row = 0, column = 2, sticky = 'e')

    #####################

    frameGraphAxLabs <- ttklabelframe(frDialog, text = lang.dlg[['label']][['3']], relief = 'groove')

    is.xaxis.lab <- tclVar(climGraphOpt$proba.enso$axislabs$is.xlab)
    is.yaxis.lab <- tclVar(climGraphOpt$proba.enso$axislabs$is.ylab)
    xaxis.lab <- tclVar(climGraphOpt$proba.enso$axislabs$xlab)
    yaxis.lab <- tclVar(climGraphOpt$proba.enso$axislabs$ylab)

    stateXLab <- if(climGraphOpt$proba.enso$axislabs$is.xlab) 'normal' else 'disabled'
    stateYLab <- if(climGraphOpt$proba.enso$axislabs$is.ylab) 'normal' else 'disabled'

    chk.Xlab <- tkcheckbutton(frameGraphAxLabs, variable = is.xaxis.lab, text = 'Xlab', anchor = 'w', justify = 'left')
    en.Xlab <- tkentry(frameGraphAxLabs, textvariable = xaxis.lab, width = largeur1, state = stateXLab)
    chk.Ylab <- tkcheckbutton(frameGraphAxLabs, variable = is.yaxis.lab, text = 'Ylab', anchor = 'w', justify = 'left')
    en.Ylab <- tkentry(frameGraphAxLabs, textvariable = yaxis.lab, width = largeur1, state = stateYLab)

    tkgrid(chk.Xlab, row = 0, column = 0, sticky = 'e')
    tkgrid(en.Xlab, row = 0, column = 1, sticky = 'we')
    tkgrid(chk.Ylab, row = 1, column = 0, sticky = 'e')
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

    is.title <- tclVar(climGraphOpt$proba.enso$title$is.title)
    text.title <- tclVar(climGraphOpt$proba.enso$title$title)

    pos.title <- tclVar()
    CbposTitleVAL <- lang.dlg[['combobox']][['1']]
    posTitleVAL <- c("top", "bottom")
    tclvalue(pos.title) <- CbposTitleVAL[posTitleVAL %in% climGraphOpt$proba.enso$title$position]

    stateGpTlt <- if(climGraphOpt$proba.enso$title$is.title) 'normal' else 'disabled'

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

    frameGraphPlot <- ttklabelframe(frDialog, text = lang.dlg[['label']][['6']], relief = 'groove')

    #####
    framepltType <- tkframe(frameGraphPlot)

    plot.type <- tclVar(climGraphOpt$proba.enso$plot$type)
    statePlotPoints <- if(climGraphOpt$proba.enso$plot$type == 'both') 'normal' else 'disabled'

    txt.pltType <- tklabel(framepltType, text = lang.dlg[['label']][['7']], anchor = 'e', justify = 'right')
    cb.pltType <- ttkcombobox(framepltType, values = c('both', 'line'), textvariable = plot.type, width = 5)
    txt.pltLineW <- tklabel(framepltType, text = lang.dlg[['label']][['8']], anchor = 'e', justify = 'right')
    spin.pltLineW <- ttkspinbox(framepltType, from = 0.5, to = 4, increment = 0.1, justify = 'center', width = width.spin)
    tkset(spin.pltLineW, climGraphOpt$proba.enso$plot$lwd)
    txt.pltPointS <- tklabel(framepltType, text = lang.dlg[['label']][['9']], anchor = 'e', justify = 'right')
    spin.pltPointS <- ttkspinbox(framepltType, from = 0.5, to = 2.5, increment = 0.1, justify = 'center', width = width.spin, state = statePlotPoints)
    tkset(spin.pltPointS, climGraphOpt$proba.enso$plot$cex)

    tkgrid(txt.pltType, cb.pltType, txt.pltLineW, spin.pltLineW, txt.pltPointS, spin.pltPointS)

    #########

    tkbind(cb.pltType, "<<ComboboxSelected>>", function(){
        statePlotPoints <- if(tclvalue(plot.type) == 'both') 'normal' else 'disabled'
        tkconfigure(spin.pltPointS, state = statePlotPoints)
        tkconfigure(bt.pltPointAl, state = statePlotPoints)
        tkconfigure(bt.pltPointNa, state = statePlotPoints)
        tkconfigure(bt.pltPointNe, state = statePlotPoints)
        tkconfigure(bt.pltPointNo, state = statePlotPoints)
    })

    #####
    framepltLine <- ttklabelframe(frameGraphPlot, text = lang.dlg[['label']][['10']], relief = 'groove')

    plot.col.lineAl <- tclVar(climGraphOpt$proba.enso$plot$all$line)
    plot.col.lineNa <- tclVar(climGraphOpt$proba.enso$plot$nina$line)
    plot.col.lineNe <- tclVar(climGraphOpt$proba.enso$plot$neutre$line)
    plot.col.lineNo <- tclVar(climGraphOpt$proba.enso$plot$nino$line)

    txt.pltAll <- tklabel(framepltLine, text = lang.dlg[['label']][['12']], anchor = 'e', justify = 'right')
    bt.pltLineAl <- tkbutton(framepltLine, bg = tclvalue(plot.col.lineAl), width = width.col)
    txt.pltNina <- tklabel(framepltLine, text = lang.dlg[['label']][['13']], anchor = 'e', justify = 'right')
    bt.pltLineNa <- tkbutton(framepltLine, bg = tclvalue(plot.col.lineNa), width = width.col)
    txt.pltNeutre <- tklabel(framepltLine, text = lang.dlg[['label']][['14']], anchor = 'e', justify = 'right')
    bt.pltLineNe <- tkbutton(framepltLine, bg = tclvalue(plot.col.lineNe), width = width.col)
    txt.pltNino <- tklabel(framepltLine, text = lang.dlg[['label']][['15']], anchor = 'e', justify = 'right')
    bt.pltLineNo <- tkbutton(framepltLine, bg = tclvalue(plot.col.lineNo), width = width.col)

    tkgrid(txt.pltAll, row = 0, column = 0, sticky = 'e')
    tkgrid(bt.pltLineAl, row = 0, column = 1)
    tkgrid(txt.pltNina, row = 1, column = 0, sticky = 'e')
    tkgrid(bt.pltLineNa, row = 1, column = 1)
    tkgrid(txt.pltNeutre, row = 2, column = 0, sticky = 'e')
    tkgrid(bt.pltLineNe, row = 2, column = 1)
    tkgrid(txt.pltNino, row = 3, column = 0, sticky = 'e')
    tkgrid(bt.pltLineNo, row = 3, column = 1)

    #####

    tkconfigure(bt.pltLineAl, command = function(){
        loko <- trimws(tclvalue(tcl("tk_chooseColor", initialcolor = tclvalue(plot.col.lineAl), title = lang.dlg[['label']][['16']])))
        if(nchar(loko) > 0){
            tkconfigure(bt.pltLineAl, bg = loko)
            tclvalue(plot.col.lineAl) <- loko
        }
    })

    tkconfigure(bt.pltLineNa, command = function(){
        loko <- trimws(tclvalue(tcl("tk_chooseColor", initialcolor = tclvalue(plot.col.lineNa), title = lang.dlg[['label']][['16']])))
        if(nchar(loko) > 0){
            tkconfigure(bt.pltLineNa, bg = loko)
            tclvalue(plot.col.lineNa) <- loko
        }
    })

    tkconfigure(bt.pltLineNe, command = function(){
        loko <- trimws(tclvalue(tcl("tk_chooseColor", initialcolor = tclvalue(plot.col.lineNe), title = lang.dlg[['label']][['16']])))
        if(nchar(loko) > 0){
            tkconfigure(bt.pltLineNe, bg = loko)
            tclvalue(plot.col.lineNe) <- loko
        }
    })

    tkconfigure(bt.pltLineNo, command = function(){
        loko <- trimws(tclvalue(tcl("tk_chooseColor", initialcolor = tclvalue(plot.col.lineNo), title = lang.dlg[['label']][['16']])))
        if(nchar(loko) > 0){
            tkconfigure(bt.pltLineNo, bg = loko)
            tclvalue(plot.col.lineNo) <- loko
        }
    })

    #####
    framepltPoints <- ttklabelframe(frameGraphPlot, text = lang.dlg[['label']][['11']], relief = 'groove')

    plot.col.pointsAl <- tclVar(climGraphOpt$proba.enso$plot$all$points)
    plot.col.pointsNa <- tclVar(climGraphOpt$proba.enso$plot$nina$points)
    plot.col.pointsNe <- tclVar(climGraphOpt$proba.enso$plot$neutre$points)
    plot.col.pointsNo <- tclVar(climGraphOpt$proba.enso$plot$nino$points)

    bt.pltPointAl <- tkbutton(framepltPoints, bg = tclvalue(plot.col.pointsAl), width = width.col, state = statePlotPoints)
    bt.pltPointNa <- tkbutton(framepltPoints, bg = tclvalue(plot.col.pointsNa), width = width.col, state = statePlotPoints)
    bt.pltPointNe <- tkbutton(framepltPoints, bg = tclvalue(plot.col.pointsNe), width = width.col, state = statePlotPoints)
    bt.pltPointNo <- tkbutton(framepltPoints, bg = tclvalue(plot.col.pointsNo), width = width.col, state = statePlotPoints)

    tkgrid(bt.pltPointAl, row = 0, column = 1)
    tkgrid(bt.pltPointNa, row = 1, column = 1)
    tkgrid(bt.pltPointNe, row = 2, column = 1)
    tkgrid(bt.pltPointNo, row = 3, column = 1)

    #####

    tkconfigure(bt.pltPointAl, command = function(){
        loko <- trimws(tclvalue(tcl("tk_chooseColor", initialcolor = tclvalue(plot.col.pointsAl), title = lang.dlg[['label']][['16']])))
        if(nchar(loko) > 0){
            tkconfigure(bt.pltPointAl, bg = loko)
            tclvalue(plot.col.pointsAl) <- loko
        }
    })

    tkconfigure(bt.pltPointNa, command = function(){
        loko <- trimws(tclvalue(tcl("tk_chooseColor", initialcolor = tclvalue(plot.col.pointsNa), title = lang.dlg[['label']][['16']])))
        if(nchar(loko) > 0){
            tkconfigure(bt.pltPointNa, bg = loko)
            tclvalue(plot.col.pointsNa) <- loko
        }
    })

    tkconfigure(bt.pltPointNe, command = function(){
        loko <- trimws(tclvalue(tcl("tk_chooseColor", initialcolor = tclvalue(plot.col.pointsNe), title = lang.dlg[['label']][['16']])))
        if(nchar(loko) > 0){
            tkconfigure(bt.pltPointNe, bg = loko)
            tclvalue(plot.col.pointsNe) <- loko
        }
    })

    tkconfigure(bt.pltPointNo, command = function(){
        loko <- trimws(tclvalue(tcl("tk_chooseColor", initialcolor = tclvalue(plot.col.pointsNo), title = lang.dlg[['label']][['16']])))
        if(nchar(loko) > 0){
            tkconfigure(bt.pltPointNo, bg = loko)
            tclvalue(plot.col.pointsNo) <- loko
        }
    })

    #########
    tkgrid(framepltType, row = 0, column = 0, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
    tkgrid(framepltLine, row = 0, column = 1, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
    tkgrid(framepltPoints, row = 0, column = 2, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)

    #####################

    tkgrid(frameGraphXYlim, row = 0, column = 0, sticky = '', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
    tkgrid(frameGraphAxLabs, row = 1, column = 0, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
    tkgrid(frameGraphTitle, row = 2, column = 0, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
    tkgrid(frameGraphPlot, row = 3, column = 0, sticky = '', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)

    #####################
    bt.opt.OK <- ttkbutton(frButt, text = .cdtEnv$tcl$lang$global[['button']][['1']])
    bt.opt.CA <- ttkbutton(frButt, text = .cdtEnv$tcl$lang$global[['button']][['2']])

    tkconfigure(bt.opt.OK, command = function(){
        climGraphOpt$proba.enso$xlim$is.min <<- switch(tclvalue(is.min.xlim), '0' = FALSE, '1' = TRUE)
        climGraphOpt$proba.enso$xlim$is.max <<- switch(tclvalue(is.max.xlim), '0' = FALSE, '1' = TRUE)
        climGraphOpt$proba.enso$xlim$min <<- trimws(tclvalue(min.xlim))
        climGraphOpt$proba.enso$xlim$max <<- trimws(tclvalue(max.xlim))

        climGraphOpt$proba.enso$ylim$is.min <<- switch(tclvalue(is.min.ylim), '0' = FALSE, '1' = TRUE)
        climGraphOpt$proba.enso$ylim$is.max <<- switch(tclvalue(is.max.ylim), '0' = FALSE, '1' = TRUE)
        climGraphOpt$proba.enso$ylim$min <<- as.numeric(trimws(tclvalue(min.ylim)))
        climGraphOpt$proba.enso$ylim$max <<- as.numeric(trimws(tclvalue(max.ylim)))

        climGraphOpt$proba.enso$axislabs$is.xlab <<- switch(tclvalue(is.xaxis.lab), '0' = FALSE, '1' = TRUE)
        climGraphOpt$proba.enso$axislabs$is.ylab <<- switch(tclvalue(is.yaxis.lab), '0' = FALSE, '1' = TRUE)
        climGraphOpt$proba.enso$axislabs$xlab <<- gsub('\\\\n', '\n', trimws(tclvalue(xaxis.lab)))
        climGraphOpt$proba.enso$axislabs$ylab <<- gsub('\\\\n', '\n', trimws(tclvalue(yaxis.lab)))

        climGraphOpt$proba.enso$title$is.title <<- switch(tclvalue(is.title), '0' = FALSE, '1' = TRUE)
        climGraphOpt$proba.enso$title$title <<- gsub('\\\\n', '\n', trimws(tclvalue(text.title)))
        climGraphOpt$proba.enso$title$position <<- posTitleVAL[CbposTitleVAL %in% trimws(tclvalue(pos.title))]

        climGraphOpt$proba.enso$plot$type <<- trimws(tclvalue(plot.type))
        climGraphOpt$proba.enso$plot$lwd <<- as.numeric(trimws(tclvalue(tkget(spin.pltLineW))))
        climGraphOpt$proba.enso$plot$cex <<- as.numeric(trimws(tclvalue(tkget(spin.pltPointS))))

        climGraphOpt$proba.enso$plot$all$line <<- tclvalue(plot.col.lineAl)
        climGraphOpt$proba.enso$plot$all$points <<- tclvalue(plot.col.pointsAl)
        climGraphOpt$proba.enso$plot$nina$line <<- tclvalue(plot.col.lineNa)
        climGraphOpt$proba.enso$plot$nina$points <<- tclvalue(plot.col.pointsNa)
        climGraphOpt$proba.enso$plot$nino$line <<- tclvalue(plot.col.lineNo)
        climGraphOpt$proba.enso$plot$nino$points <<- tclvalue(plot.col.pointsNo)
        climGraphOpt$proba.enso$plot$neutre$line <<- tclvalue(plot.col.lineNe)
        climGraphOpt$proba.enso$plot$neutre$points <<- tclvalue(plot.col.pointsNe)

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

    ##################################################################
    tkfocus(tt)
    tkbind(tt, "<Destroy>", function(){
        tkgrab.release(tt)
        tkfocus(parent.win)
    })
    tkwait.window(tt)
    return(climGraphOpt)
}

#######################################################################################################

MapGraph.GraphOptions.Bar.Line <- function(climGraphOpt, parent.win = .cdtEnv$tcl$main$win){
    if(WindowsOS()){
        largeur1 <- 64
        largeur2 <- 68
        largeur3 <- 54
        width.xlim <- 11
        width.ylim <- 5
        width.col <- 4
        width.spin <- 4
    }else{
        largeur1 <- 60
        largeur2 <- 64
        largeur3 <- 58
        width.xlim <- 11
        width.ylim <- 5
        width.col <- 2
        width.spin <- 4
    }

    #####################
    xml.dlg <- file.path(.cdtDir$dirLocal, "languages", "cdtGraphOptions.BarLine_dlgBox.xml")
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

    is.min.xlim <- tclVar(climGraphOpt$bar.line$xlim$is.min)
    is.max.xlim <- tclVar(climGraphOpt$bar.line$xlim$is.max)
    min.xlim <- tclVar(climGraphOpt$bar.line$xlim$min)
    max.xlim <- tclVar(climGraphOpt$bar.line$xlim$max)

    stateMinXlim <- if(climGraphOpt$bar.line$xlim$is.min) 'normal' else 'disabled'
    stateMaxXlim <- if(climGraphOpt$bar.line$xlim$is.max) 'normal' else 'disabled'

    chk.min.Xlim <- tkcheckbutton(frameGraphXlim, variable = is.min.xlim, text = "Min", anchor = 'w', justify = 'left')
    en.min.Xlim <- tkentry(frameGraphXlim, textvariable = min.xlim, width = width.xlim, state = stateMinXlim)
    chk.max.Xlim <- tkcheckbutton(frameGraphXlim, variable = is.max.xlim, text = "Max", anchor = 'w', justify = 'left')
    en.max.Xlim <- tkentry(frameGraphXlim, textvariable = max.xlim, width = width.xlim, state = stateMaxXlim)

    tkgrid(chk.min.Xlim, row = 0, column = 0, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
    tkgrid(en.min.Xlim, row = 0, column = 1, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
    tkgrid(chk.max.Xlim, row = 0, column = 3, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
    tkgrid(en.max.Xlim, row = 0, column = 4, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)

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

    #####################

    frameGraphYlim <- ttklabelframe(frameGraphXYlim, text = lang.dlg[['label']][['2']], relief = 'groove')

    is.min.ylim <- tclVar(climGraphOpt$bar.line$ylim$is.min)
    is.max.ylim <- tclVar(climGraphOpt$bar.line$ylim$is.max)
    min.ylim <- tclVar(climGraphOpt$bar.line$ylim$min)
    max.ylim <- tclVar(climGraphOpt$bar.line$ylim$max)

    stateMinYlim <- if(climGraphOpt$bar.line$ylim$is.min) 'normal' else 'disabled'
    stateMaxYlim <- if(climGraphOpt$bar.line$ylim$is.max) 'normal' else 'disabled'

    chk.min.Ylim <- tkcheckbutton(frameGraphYlim, variable = is.min.ylim, text = "Min", anchor = 'w', justify = 'left')
    en.min.Ylim <- tkentry(frameGraphYlim, textvariable = min.ylim, width = width.ylim, state = stateMinYlim)
    chk.max.Ylim <- tkcheckbutton(frameGraphYlim, variable = is.max.ylim, text = "Max", anchor = 'w', justify = 'left')
    en.max.Ylim <- tkentry(frameGraphYlim, textvariable = max.ylim, width = width.ylim, state = stateMaxYlim)

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

    sepXYlim <- tklabel(frameGraphXYlim, text = "", width = 2)

    tkgrid(frameGraphXlim, row = 0, column = 0, sticky = 'w')
    tkgrid(sepXYlim, row = 0, column = 1, sticky = 'we')
    tkgrid(frameGraphYlim, row = 0, column = 2, sticky = 'e')

    #####################

    frameLevel <- ttklabelframe(frDialog, text = lang.dlg[['label']][['3']], relief = 'groove')

    custom.ticks <- tclVar(climGraphOpt$bar.line$userYTcks$custom)
    stateEditLvl <- if(climGraphOpt$bar.line$userYTcks$custom) 'normal' else 'disabled'

    chk.Level <- tkcheckbutton(frameLevel, variable = custom.ticks, text = lang.dlg[['checkbutton']][['1']], anchor = 'w', justify = 'left')
    yscrLevel <- tkscrollbar(frameLevel, repeatinterval = 4, command = function(...) tkyview(textLevel, ...))
    textLevel <- tktext(frameLevel, bg = "white", wrap = "word", height = 2, width = largeur3,
                            yscrollcommand = function(...) tkset(yscrLevel, ...))

    tkgrid(chk.Level, sticky = "we")
    tkgrid(textLevel, yscrLevel)
    tkgrid.configure(yscrLevel, sticky = "ns")
    tkgrid.configure(textLevel, sticky = 'nswe') 

    if(length(climGraphOpt$bar.line$userYTcks$ticks) > 0)
        for(j in seq_along(climGraphOpt$bar.line$userYTcks$ticks))
            tkinsert(textLevel, "end", paste0(climGraphOpt$bar.line$userYTcks$ticks[j], ', '))
    tkconfigure(textLevel, state = stateEditLvl)

    #########

    tkbind(chk.Level, "<Button-1>", function(){
        stateEditLvl <- if(tclvalue(custom.ticks) == '0') 'normal' else 'disabled'
        tkconfigure(textLevel, state = stateEditLvl)
    })

    #####################

    frameGraphAxLabs <- ttklabelframe(frDialog, text = lang.dlg[['label']][['4']], relief = 'groove')

    is.xaxis.lab <- tclVar(climGraphOpt$bar.line$axislabs$is.xlab)
    is.yaxis.lab <- tclVar(climGraphOpt$bar.line$axislabs$is.ylab)
    xaxis.lab <- tclVar(climGraphOpt$bar.line$axislabs$xlab)
    yaxis.lab <- tclVar(climGraphOpt$bar.line$axislabs$ylab)

    stateXLab <- if(climGraphOpt$bar.line$axislabs$is.xlab) 'normal' else 'disabled'
    stateYLab <- if(climGraphOpt$bar.line$axislabs$is.ylab) 'normal' else 'disabled'

    chk.Xlab <- tkcheckbutton(frameGraphAxLabs, variable = is.xaxis.lab, text = 'Xlab', anchor = 'w', justify = 'left')
    en.Xlab <- tkentry(frameGraphAxLabs, textvariable = xaxis.lab, width = largeur1, state = stateXLab)
    chk.Ylab <- tkcheckbutton(frameGraphAxLabs, variable = is.yaxis.lab, text = 'Ylab', anchor = 'w', justify = 'left')
    en.Ylab <- tkentry(frameGraphAxLabs, textvariable = yaxis.lab, width = largeur1, state = stateYLab)

    tkgrid(chk.Xlab, row = 0, column = 0, sticky = 'e')
    tkgrid(en.Xlab, row = 0, column = 1, sticky = 'we')
    tkgrid(chk.Ylab, row = 1, column = 0, sticky = 'e')
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

    frameGraphTitle <- ttklabelframe(frDialog, text = lang.dlg[['label']][['5']], relief = 'groove')

    is.title <- tclVar(climGraphOpt$bar.line$title$is.title)
    text.title <- tclVar(climGraphOpt$bar.line$title$title)

    pos.title <- tclVar()
    CbposTitleVAL <- lang.dlg[['combobox']][['1']]
    posTitleVAL <- c("top", "bottom")
    tclvalue(pos.title) <- CbposTitleVAL[posTitleVAL %in% climGraphOpt$bar.line$title$position]

    stateGpTlt <- if(climGraphOpt$bar.line$title$is.title) 'normal' else 'disabled'

    chk.GpTlt <- tkcheckbutton(frameGraphTitle, variable = is.title, anchor = 'e', justify = 'right')
    en.GpTlt <- tkentry(frameGraphTitle, textvariable = text.title, width = largeur2, state = stateGpTlt)
    txt.GpTlt <- tklabel(frameGraphTitle, text = lang.dlg[['label']][['6']], anchor = 'e', justify = 'right')
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

    frameColG <- tkframe(frDialog)

    #########

    frameBarCol <- ttklabelframe(frameColG, text = lang.dlg[['label']][['7']], relief = 'groove')

    color.Y0 <- tclVar(climGraphOpt$bar.line$colors$y0)
    color.moins <- tclVar(climGraphOpt$bar.line$colors$negative)
    color.plus <- tclVar(climGraphOpt$bar.line$colors$positive)

    txt.colY0 <- tklabel(frameBarCol, text = "Y0", anchor = 'e', justify = 'right')
    en.colY0 <- tkentry(frameBarCol, textvariable = color.Y0, width = 3)
    txt.colMoins <- tklabel(frameBarCol, text = lang.dlg[['label']][['8']], anchor = 'e', justify = 'right')
    bt.colMoins <- tkbutton(frameBarCol, bg = tclvalue(color.moins), width = width.col)
    txt.colPlus <- tklabel(frameBarCol, text = lang.dlg[['label']][['9']], anchor = 'e', justify = 'right')
    bt.colPlus <- tkbutton(frameBarCol, bg = tclvalue(color.plus), width = width.col)

    tkgrid(txt.colY0, row = 0, column = 0, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
    tkgrid(en.colY0, row = 0, column = 1, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
    tkgrid(txt.colMoins, row = 0, column = 2, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
    tkgrid(bt.colMoins, row = 0, column = 3, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
    tkgrid(txt.colPlus, row = 0, column = 4, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
    tkgrid(bt.colPlus, row = 0, column = 5, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)

    #########
    tkconfigure(bt.colMoins, command = function(){
        loko <- trimws(tclvalue(tcl("tk_chooseColor", initialcolor = tclvalue(color.moins), title = lang.dlg[['label']][['13']])))
        if(nchar(loko) > 0){
            tkconfigure(bt.colMoins, bg = loko)
            tclvalue(color.moins) <- loko
        }
    })

    tkconfigure(bt.colPlus, command = function(){
        loko <- trimws(tclvalue(tcl("tk_chooseColor", initialcolor = tclvalue(color.plus), title = lang.dlg[['label']][['13']])))
        if(nchar(loko) > 0){
            tkconfigure(bt.colPlus, bg = loko)
            tclvalue(color.plus) <- loko
        }
    })

    #####################

    framePlotLine <- ttklabelframe(frameColG, text = lang.dlg[['label']][['10']], relief = 'groove')

    is.plotline <- tclVar(climGraphOpt$bar.line$line$plot)
    col.plotline <- tclVar(climGraphOpt$bar.line$line$col)
    statePlotLine <- if(climGraphOpt$bar.line$line$plot) "normal" else "disabled"

    chk.plotLine <- tkcheckbutton(framePlotLine, variable = is.plotline, anchor = 'e', justify = 'right')
    txt.plotLineC <- tklabel(framePlotLine, text = lang.dlg[['label']][['11']], anchor = 'e', justify = 'right')
    bt.plotLineC <- tkbutton(framePlotLine, bg = tclvalue(col.plotline), width = width.col, state = statePlotLine)
    txt.plotLineW <- tklabel(framePlotLine, text = lang.dlg[['label']][['12']], anchor = 'e', justify = 'right')
    spin.plotLineW <- ttkspinbox(framePlotLine, from = 0.5, to = 4, increment = 0.1, justify = 'center', width = width.spin, state = statePlotLine)
    tkset(spin.plotLineW, climGraphOpt$bar.line$line$lwd)

    tkgrid(chk.plotLine, txt.plotLineC, bt.plotLineC, txt.plotLineW, spin.plotLineW)

    #####
    tkconfigure(bt.plotLineC, command = function(){
        loko <- trimws(tclvalue(tcl("tk_chooseColor", initialcolor = tclvalue(col.plotline), title = lang.dlg[['label']][['13']])))
        if(nchar(loko) > 0){
            tkconfigure(bt.plotLineC, bg = loko)
            tclvalue(col.plotline) <- loko
        }
    })

    tkbind(chk.plotLine, "<Button-1>", function(){
        statePlotLine <- if(tclvalue(is.plotline) == '0') 'normal' else 'disabled'
        tkconfigure(bt.plotLineC, state = statePlotLine)
        tkconfigure(spin.plotLineW, state = statePlotLine)
    })

    #####################

    sepColG <- tklabel(frameColG, text = "", width = 4)

    tkgrid(frameBarCol, row = 0, column = 0, sticky = 'w')
    tkgrid(sepColG, row = 0, column = 1, sticky = 'we')
    tkgrid(framePlotLine, row = 0, column = 2, sticky = 'e')

    #####################

    tkgrid(frameGraphXYlim, row = 0, column = 0, sticky = '', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
    tkgrid(frameLevel, row = 1, column = 0, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
    tkgrid(frameGraphAxLabs, row = 2, column = 0, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
    tkgrid(frameGraphTitle, row = 3, column = 0, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
    tkgrid(frameColG, row = 4, column = 0, sticky = '', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)

    #####################
    bt.opt.OK <- ttkbutton(frButt, text = .cdtEnv$tcl$lang$global[['button']][['1']])
    bt.opt.CA <- ttkbutton(frButt, text = .cdtEnv$tcl$lang$global[['button']][['2']])

    tkconfigure(bt.opt.OK, command = function(){
        climGraphOpt$bar.line$xlim$is.min <<- switch(tclvalue(is.min.xlim), '0' = FALSE, '1' = TRUE)
        climGraphOpt$bar.line$xlim$is.max <<- switch(tclvalue(is.max.xlim), '0' = FALSE, '1' = TRUE)
        climGraphOpt$bar.line$xlim$min <<- trimws(tclvalue(min.xlim))
        climGraphOpt$bar.line$xlim$max <<- trimws(tclvalue(max.xlim))

        climGraphOpt$bar.line$ylim$is.min <<- switch(tclvalue(is.min.ylim), '0' = FALSE, '1' = TRUE)
        climGraphOpt$bar.line$ylim$is.max <<- switch(tclvalue(is.max.ylim), '0' = FALSE, '1' = TRUE)
        climGraphOpt$bar.line$ylim$min <<- as.numeric(trimws(tclvalue(min.ylim)))
        climGraphOpt$bar.line$ylim$max <<- as.numeric(trimws(tclvalue(max.ylim)))

        climGraphOpt$bar.line$userYTcks$custom <<- switch(tclvalue(custom.ticks), '0' = FALSE, '1' = TRUE)
        if(climGraphOpt$bar.line$userYTcks$custom){
            vlevel <- tclvalue(tkget(textLevel, "0.0", "end"))
            vlevel <- gsub("[\t\r\n]", "", vlevel)
            vlevel <- gsub('\\s+', '', vlevel)
            vlevel <- strsplit(vlevel, ",")[[1]]
            vlevel <- vlevel[!is.na(vlevel) | vlevel != '']
            if(length(vlevel) < 2){
                cdt.tkmessageBox(tt, message = lang.dlg[['message']][['1']], icon = "warning", type = "ok")
                tkwait.window(tt)
            }
            climGraphOpt$bar.line$userYTcks$ticks <<- as.numeric(vlevel)
        }

        climGraphOpt$bar.line$axislabs$is.xlab <<- switch(tclvalue(is.xaxis.lab), '0' = FALSE, '1' = TRUE)
        climGraphOpt$bar.line$axislabs$is.ylab <<- switch(tclvalue(is.yaxis.lab), '0' = FALSE, '1' = TRUE)
        climGraphOpt$bar.line$axislabs$xlab <<- gsub('\\\\n', '\n', trimws(tclvalue(xaxis.lab)))
        climGraphOpt$bar.line$axislabs$ylab <<- gsub('\\\\n', '\n', trimws(tclvalue(yaxis.lab)))

        climGraphOpt$bar.line$title$is.title <<- switch(tclvalue(is.title), '0' = FALSE, '1' = TRUE)
        climGraphOpt$bar.line$title$title <<- gsub('\\\\n', '\n', trimws(tclvalue(text.title)))
        climGraphOpt$bar.line$title$position <<- posTitleVAL[CbposTitleVAL %in% trimws(tclvalue(pos.title))]

        climGraphOpt$bar.line$colors$y0 <<- as.numeric(trimws(tclvalue(color.Y0)))
        climGraphOpt$bar.line$colors$negative <<- tclvalue(color.moins)
        climGraphOpt$bar.line$colors$positive <<- tclvalue(color.plus)

        climGraphOpt$bar.line$line$plot <<- switch(tclvalue(is.plotline), '0' = FALSE, '1' = TRUE)
        climGraphOpt$bar.line$line$col <<- tclvalue(col.plotline)
        climGraphOpt$bar.line$line$lwd <<- as.numeric(trimws(tclvalue(tkget(spin.plotLineW))))

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

    ##################################################################
    tkfocus(tt)
    tkbind(tt, "<Destroy>", function(){
        tkgrab.release(tt)
        tkfocus(parent.win)
    })
    tkwait.window(tt)
    return(climGraphOpt)
}

#######################################################################################################

MapGraph.GraphOptions.LineCLIMDEX <- function(climGraphOpt, parent.win = .cdtEnv$tcl$main$win){
    if(WindowsOS()){
        largeur1 <- 54
        largeur2 <- 58
        largeur3 <- 23
        largeur4 <- 4
        width.xlim <- 5
        width.ylim <- 5
        width.col <- 3
        width.spin <- 4
    }else{
        largeur1 <- 54
        largeur2 <- 58
        largeur3 <- 23
        largeur4 <- 8
        width.xlim <- 5
        width.ylim <- 5
        width.col <- 1
        width.spin <- 4
    }

    #####################
    xml.dlg <- file.path(.cdtDir$dirLocal, "languages", "cdtGraphOptions.LineCLIMDEX_dlgBox.xml")
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

    is.min.xlim <- tclVar(climGraphOpt$line$xlim$is.min)
    is.max.xlim <- tclVar(climGraphOpt$line$xlim$is.max)
    min.xlim <- tclVar(climGraphOpt$line$xlim$min)
    max.xlim <- tclVar(climGraphOpt$line$xlim$max)

    stateMinXlim <- if(climGraphOpt$line$xlim$is.min) 'normal' else 'disabled'
    stateMaxXlim <- if(climGraphOpt$line$xlim$is.max) 'normal' else 'disabled'

    chk.min.Xlim <- tkcheckbutton(frameGraphXlim, variable = is.min.xlim, text = "Min", anchor = 'w', justify = 'left')
    en.min.Xlim <- tkentry(frameGraphXlim, textvariable = min.xlim, width = width.xlim, state = stateMinXlim)
    chk.max.Xlim <- tkcheckbutton(frameGraphXlim, variable = is.max.xlim, text = "Max", anchor = 'w', justify = 'left')
    en.max.Xlim <- tkentry(frameGraphXlim, textvariable = max.xlim, width = width.xlim, state = stateMaxXlim)

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

    #####################

    frameGraphYlim <- ttklabelframe(frameGraphXYlim, text = lang.dlg[['label']][['2']], relief = 'groove')

    is.min.ylim <- tclVar(climGraphOpt$line$ylim$is.min)
    is.max.ylim <- tclVar(climGraphOpt$line$ylim$is.max)
    min.ylim <- tclVar(climGraphOpt$line$ylim$min)
    max.ylim <- tclVar(climGraphOpt$line$ylim$max)

    stateMinYlim <- if(climGraphOpt$line$ylim$is.min) 'normal' else 'disabled'
    stateMaxYlim <- if(climGraphOpt$line$ylim$is.max) 'normal' else 'disabled'

    chk.min.Ylim <- tkcheckbutton(frameGraphYlim, variable = is.min.ylim, text = "Min", anchor = 'w', justify = 'left')
    en.min.Ylim <- tkentry(frameGraphYlim, textvariable = min.ylim, width = width.ylim, state = stateMinYlim)
    chk.max.Ylim <- tkcheckbutton(frameGraphYlim, variable = is.max.ylim, text = "Max", anchor = 'w', justify = 'left')
    en.max.Ylim <- tkentry(frameGraphYlim, textvariable = max.ylim, width = width.ylim, state = stateMaxYlim)

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

    sepXYlim <- tklabel(frameGraphXYlim, text = "", width = largeur4)

    tkgrid(frameGraphXlim, row = 0, column = 0, sticky = 'w')
    tkgrid(sepXYlim, row = 0, column = 1, sticky = 'we')
    tkgrid(frameGraphYlim, row = 0, column = 2, sticky = 'e')

    #####################

    frameGraphAxLabs <- ttklabelframe(frDialog, text = lang.dlg[['label']][['3']], relief = 'groove')

    is.xaxis.lab <- tclVar(climGraphOpt$line$axislabs$is.xlab)
    is.yaxis.lab <- tclVar(climGraphOpt$line$axislabs$is.ylab)
    xaxis.lab <- tclVar(climGraphOpt$line$axislabs$xlab)
    yaxis.lab <- tclVar(climGraphOpt$line$axislabs$ylab)

    stateXLab <- if(climGraphOpt$line$axislabs$is.xlab) 'normal' else 'disabled'
    stateYLab <- if(climGraphOpt$line$axislabs$is.ylab) 'normal' else 'disabled'

    chk.Xlab <- tkcheckbutton(frameGraphAxLabs, variable = is.xaxis.lab, text = 'Xlab', anchor = 'w', justify = 'left')
    en.Xlab <- tkentry(frameGraphAxLabs, textvariable = xaxis.lab, width = largeur1, state = stateXLab)
    chk.Ylab <- tkcheckbutton(frameGraphAxLabs, variable = is.yaxis.lab, text = 'Ylab', anchor = 'w', justify = 'left')
    en.Ylab <- tkentry(frameGraphAxLabs, textvariable = yaxis.lab, width = largeur1, state = stateYLab)

    tkgrid(chk.Xlab, row = 0, column = 0, sticky = 'e')
    tkgrid(en.Xlab, row = 0, column = 1, sticky = 'we')
    tkgrid(chk.Ylab, row = 1, column = 0, sticky = 'e')
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

    is.title <- tclVar(climGraphOpt$line$title$is.title)
    text.title <- tclVar(climGraphOpt$line$title$title)

    pos.title <- tclVar()
    CbposTitleVAL <- lang.dlg[['combobox']][['1']]
    posTitleVAL <- c("top", "bottom")
    tclvalue(pos.title) <- CbposTitleVAL[posTitleVAL %in% climGraphOpt$line$title$position]

    stateGpTlt <- if(climGraphOpt$line$title$is.title) 'normal' else 'disabled'

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

    frameGraphPlot <- ttklabelframe(frDialog, text = lang.dlg[['label']][['6']], relief = 'groove')

    plot.type <- tclVar(climGraphOpt$line$plot$type)

    txt.pltType <- tklabel(frameGraphPlot, text = lang.dlg[['label']][['7']], anchor = 'e', justify = 'right')
    cb.pltType <- ttkcombobox(frameGraphPlot, values = c('both', 'line'), textvariable = plot.type, width = 4)

    #########
    tkbind(cb.pltType, "<<ComboboxSelected>>", function(){
        statePlotPoints <- if(tclvalue(plot.type) == 'both') 'normal' else 'disabled'
        tkconfigure(bt.pltPointC, state = statePlotPoints)
        tkconfigure(spin.pltPointS, state = statePlotPoints)
    })

    #########

    framepltLine <- ttklabelframe(frameGraphPlot, text = lang.dlg[['label']][['8']], relief = 'groove')

    plot.col.line <- tclVar(climGraphOpt$line$plot$col$line)

    txt.pltLineC <- tklabel(framepltLine, text = lang.dlg[['label']][['9']], anchor = 'e', justify = 'right')
    bt.pltLineC <- tkbutton(framepltLine, bg = tclvalue(plot.col.line), width = width.col)
    txt.pltLineW <- tklabel(framepltLine, text = lang.dlg[['label']][['10']], anchor = 'e', justify = 'right')
    spin.pltLineW <- ttkspinbox(framepltLine, from = 0.5, to = 4, increment = 0.1, justify = 'center', width = width.spin)
    tkset(spin.pltLineW, climGraphOpt$line$plot$lwd)

    tkgrid(txt.pltLineC, bt.pltLineC, txt.pltLineW, spin.pltLineW)

    #########
    tkconfigure(bt.pltLineC, command = function(){
        loko <- trimws(tclvalue(tcl("tk_chooseColor", initialcolor = tclvalue(plot.col.line), title = lang.dlg[['label']][['16']])))
        if(nchar(loko) > 0){
            tkconfigure(bt.pltLineC, bg = loko)
            tclvalue(plot.col.line) <- loko
        }
    })

    #########

    framepltPoints <- ttklabelframe(frameGraphPlot, text = lang.dlg[['label']][['11']], relief = 'groove')

    plot.col.points <- tclVar(climGraphOpt$line$plot$col$points)
    statePlotPoints <- if(climGraphOpt$line$plot$type == 'both') 'normal' else 'disabled'

    txt.pltPointC <- tklabel(framepltPoints, text = lang.dlg[['label']][['9']], anchor = 'e', justify = 'right')
    bt.pltPointC <- tkbutton(framepltPoints, bg = tclvalue(plot.col.points), width = width.col, state = statePlotPoints)
    txt.pltPointS <- tklabel(framepltPoints, text = lang.dlg[['label']][['12']], anchor = 'e', justify = 'right')
    spin.pltPointS <- ttkspinbox(framepltPoints, from = 0.5, to = 2.5, increment = 0.1, justify = 'center', width = width.spin, state = statePlotPoints)
    tkset(spin.pltPointS, climGraphOpt$line$plot$cex)

    tkgrid(txt.pltPointC, bt.pltPointC, txt.pltPointS, spin.pltPointS)

    #########
    tkconfigure(bt.pltPointC, command = function(){
        loko <- trimws(tclvalue(tcl("tk_chooseColor", initialcolor = tclvalue(plot.col.points), title = lang.dlg[['label']][['16']])))
        if(nchar(loko) > 0){
            tkconfigure(bt.pltPointC, bg = loko)
            tclvalue(plot.col.points) <- loko
        }
    })

    #####################
    tkgrid(txt.pltType, row = 0, column = 0, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
    tkgrid(cb.pltType, row = 0, column = 1, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
    tkgrid(framepltLine, row = 0, column = 2, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
    tkgrid(framepltPoints, row = 0, column = 3, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)

    #####################

    frameGraphLegend <- tkframe(frDialog)

    #########

    frameLezLin <- ttklabelframe(frameGraphLegend, text = lang.dlg[['label']][['13']], relief = 'groove')

    col.lezLin <- tclVar(climGraphOpt$line$legend$col$linear)
    text.lezLin <- tclVar(climGraphOpt$line$legend$text$linear)

    txt.lezLinC <- tklabel(frameLezLin, text = lang.dlg[['label']][['9']], anchor = 'e', justify = 'right')
    bt.lezLinC <- tkbutton(frameLezLin, bg = tclvalue(col.lezLin), width = width.col)
    txt.lezLinW <- tklabel(frameLezLin, text = lang.dlg[['label']][['10']], anchor = 'e', justify = 'right')
    spin.lezLinW <- ttkspinbox(frameLezLin, from = 0.5, to = 4, increment = 0.1, justify = 'center', width = width.spin)
    tkset(spin.lezLinW, climGraphOpt$line$legend$lwd$linear)
    txt.lezLinY <- tklabel(frameLezLin, text = lang.dlg[['label']][['7']], anchor = 'e', justify = 'right')
    spin.lezLinY <- ttkspinbox(frameLezLin, from = 1, to = 5, increment = 1, justify = 'center', width = width.spin)
    tkset(spin.lezLinY, climGraphOpt$line$legend$lty$linear)
    txt.lezLinT <- tklabel(frameLezLin, text = lang.dlg[['label']][['14']], anchor = 'e', justify = 'right')
    en.lezLinT <- tkentry(frameLezLin, textvariable = text.lezLin, width = largeur3)

    tkgrid(txt.lezLinC, bt.lezLinC, txt.lezLinW, spin.lezLinW, txt.lezLinY, spin.lezLinY, txt.lezLinT, en.lezLinT)

    #########
    tkconfigure(bt.lezLinC, command = function(){
        loko <- trimws(tclvalue(tcl("tk_chooseColor", initialcolor = tclvalue(col.lezLin), title = lang.dlg[['label']][['16']])))
        if(nchar(loko) > 0){
            tkconfigure(bt.lezLinC, bg = loko)
            tclvalue(col.lezLin) <- loko
        }
    })

    #########

    frameLezLowess <- ttklabelframe(frameGraphLegend, text = lang.dlg[['label']][['15']], relief = 'groove')

    col.lezLowess <- tclVar(climGraphOpt$line$legend$col$lowess)
    text.lezLowess <- tclVar(climGraphOpt$line$legend$text$lowess)

    txt.lezLowessC <- tklabel(frameLezLowess, text = lang.dlg[['label']][['9']], anchor = 'e', justify = 'right')
    bt.lezLowessC <- tkbutton(frameLezLowess, bg = tclvalue(col.lezLowess), width = width.col)
    txt.lezLowessW <- tklabel(frameLezLowess, text = lang.dlg[['label']][['10']], anchor = 'e', justify = 'right')
    spin.lezLowessW <- ttkspinbox(frameLezLowess, from = 0.5, to = 4, increment = 0.1, justify = 'center', width = width.spin)
    tkset(spin.lezLowessW, climGraphOpt$line$legend$lwd$lowess)
    txt.lezLowessY <- tklabel(frameLezLowess, text = lang.dlg[['label']][['7']], anchor = 'e', justify = 'right')
    spin.lezLowessY <- ttkspinbox(frameLezLowess, from = 1, to = 5, increment = 1, justify = 'center', width = width.spin)
    tkset(spin.lezLowessY, climGraphOpt$line$legend$lty$lowess)
    txt.lezLowessT <- tklabel(frameLezLowess, text = lang.dlg[['label']][['14']], anchor = 'e', justify = 'right')
    en.lezLowessT <- tkentry(frameLezLowess, textvariable = text.lezLowess, width = largeur3)

    tkgrid(txt.lezLowessC, bt.lezLowessC, txt.lezLowessW, spin.lezLowessW, txt.lezLowessY, spin.lezLowessY, txt.lezLowessT, en.lezLowessT)

    #########
    tkconfigure(bt.lezLowessC, command = function(){
        loko <- trimws(tclvalue(tcl("tk_chooseColor", initialcolor = tclvalue(col.lezLowess), title = lang.dlg[['label']][['16']])))
        if(nchar(loko) > 0){
            tkconfigure(bt.lezLowessC, bg = loko)
            tclvalue(col.lezLowess) <- loko
        }
    })

    #########

    tkgrid(frameLezLin, row = 0, column = 0, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
    tkgrid(frameLezLowess, row = 1, column = 0, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)

    #####################

    tkgrid(frameGraphXYlim, row = 0, column = 0, sticky = '', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
    tkgrid(frameGraphAxLabs, row = 1, column = 0, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
    tkgrid(frameGraphTitle, row = 2, column = 0, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
    tkgrid(frameGraphPlot, row = 3, column = 0, sticky = '', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
    tkgrid(frameGraphLegend, row = 4, column = 0, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)

    #####################
    bt.opt.OK <- ttkbutton(frButt, text = .cdtEnv$tcl$lang$global[['button']][['1']])
    bt.opt.CA <- ttkbutton(frButt, text = .cdtEnv$tcl$lang$global[['button']][['2']])

    tkconfigure(bt.opt.OK, command = function(){
        climGraphOpt$line$xlim$is.min <<- switch(tclvalue(is.min.xlim), '0' = FALSE, '1' = TRUE)
        climGraphOpt$line$xlim$is.max <<- switch(tclvalue(is.max.xlim), '0' = FALSE, '1' = TRUE)
        climGraphOpt$line$xlim$min <<- trimws(tclvalue(min.xlim))
        climGraphOpt$line$xlim$max <<- trimws(tclvalue(max.xlim))

        climGraphOpt$line$ylim$is.min <<- switch(tclvalue(is.min.ylim), '0' = FALSE, '1' = TRUE)
        climGraphOpt$line$ylim$is.max <<- switch(tclvalue(is.max.ylim), '0' = FALSE, '1' = TRUE)
        climGraphOpt$line$ylim$min <<- as.numeric(trimws(tclvalue(min.ylim)))
        climGraphOpt$line$ylim$max <<- as.numeric(trimws(tclvalue(max.ylim)))

        climGraphOpt$line$axislabs$is.xlab <<- switch(tclvalue(is.xaxis.lab), '0' = FALSE, '1' = TRUE)
        climGraphOpt$line$axislabs$is.ylab <<- switch(tclvalue(is.yaxis.lab), '0' = FALSE, '1' = TRUE)
        climGraphOpt$line$axislabs$xlab <<- gsub('\\\\n', '\n', trimws(tclvalue(xaxis.lab)))
        climGraphOpt$line$axislabs$ylab <<- gsub('\\\\n', '\n', trimws(tclvalue(yaxis.lab)))

        climGraphOpt$line$title$is.title <<- switch(tclvalue(is.title), '0' = FALSE, '1' = TRUE)
        climGraphOpt$line$title$title <<- gsub('\\\\n', '\n', trimws(tclvalue(text.title)))
        climGraphOpt$line$title$position <<- posTitleVAL[CbposTitleVAL %in% trimws(tclvalue(pos.title))]

        climGraphOpt$line$plot$type <<- trimws(tclvalue(plot.type))
        climGraphOpt$line$plot$col$line <<- tclvalue(plot.col.line)
        climGraphOpt$line$plot$lwd <<- as.numeric(trimws(tclvalue(tkget(spin.pltLineW))))
        climGraphOpt$line$plot$col$points <<- tclvalue(plot.col.points)
        climGraphOpt$line$plot$cex <<- as.numeric(trimws(tclvalue(tkget(spin.pltPointS))))

        climGraphOpt$line$legend$col$linear <<- tclvalue(col.lezLin)
        climGraphOpt$line$legend$lwd$linear <<- as.numeric(trimws(tclvalue(tkget(spin.lezLinW))))
        climGraphOpt$line$legend$lty$linear <<- as.numeric(trimws(tclvalue(tkget(spin.lezLinY))))
        climGraphOpt$line$legend$text$linear <<- trimws(tclvalue(text.lezLin))

        climGraphOpt$line$legend$col$lowess <<- tclvalue(col.lezLowess)
        climGraphOpt$line$legend$lwd$lowess <<- as.numeric(trimws(tclvalue(tkget(spin.lezLowessW))))
        climGraphOpt$line$legend$lty$lowess <<- as.numeric(trimws(tclvalue(tkget(spin.lezLowessY))))
        climGraphOpt$line$legend$text$lowess <<- trimws(tclvalue(text.lezLowess))

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

    ##################################################################
    tkfocus(tt)
    tkbind(tt, "<Destroy>", function(){
        tkgrab.release(tt)
        tkfocus(parent.win)
    })
    tkwait.window(tt)
    return(climGraphOpt)
}

