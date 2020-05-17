
MapGraph.MapOptions <- function(climMapOpt, parent.win = .cdtEnv$tcl$main$win){
    if(WindowsOS()){
        largeur1 <- 47
        largeur2 <- 60
        largeur3 <- .cdtEnv$tcl$fun$w.scale(30)
        largeur4 <- 78
        largeur5 <- 28
    }else{
        largeur1 <- 20
        largeur2 <- 59
        largeur3 <- .cdtEnv$tcl$fun$w.scale(29.6)
        largeur4 <- 50
        largeur5 <- 20
    }

    #####################

    xml.dlg <- file.path(.cdtDir$dirLocal, "languages", "cdtMapOptions_dlgBox.xml")
    lang.dlg <- cdtLanguageParse(xml.dlg, .cdtData$Config$lang.iso)

    #####################

    preview.canvasf1 <- function(rv){
        funkol <- str_trim(tclvalue(preset.color))
        funkol <- get(funkol, mode = "function")
        listCol <- funkol(10)
        if(tclvalue(reverse.color) == rv) listCol <- rev(listCol)
        kolor <- getGradientColor(listCol, 0:largeur3)
        tkdelete(canvas.preview, 'gradlines0')
        for(i in 0:largeur3)
            tkcreate(canvas.preview, "line", i, 0, i, 20, fill = kolor[i], tags = 'gradlines0')
    }

    preview.canvasf2 <- function(cond){
        if(cond){
            kolor <- getGradientColor(climMapOpt$userCol$color, 0:largeur3)
            tkdelete(canvas.preview, 'gradlines0')
            for(i in 0:largeur3)
                tkcreate(canvas.preview, "line", i, 0, i, 20, fill = kolor[i], tags = 'gradlines0')
        }
        else tkdelete(canvas.preview, 'gradlines0')
    }

    #####################

    tt <- tktoplevel()
    tkgrab.set(tt)
    tkfocus(tt)

    #####################
    frDialog <- tkframe(tt, relief = 'raised', borderwidth = 2)
    frButt <- tkframe(tt)

    #####################

    frameColkey <- ttklabelframe(frDialog, text = lang.dlg[['label']][['1a']], relief = 'groove')

    preset.colkey <- c('tim.colors', 'rainbow', 'heat.colors', 'cm.colors', 'topo.colors',
                       'terrain.colors', 'spi.colors', 'precip.colors', 'decile.colors')

    preset.color <- tclVar(climMapOpt$presetCol$color)
    reverse.color <- tclVar(climMapOpt$presetCol$reverse)
    custom.color <- tclVar(climMapOpt$userCol$custom)

    stateKol1 <- if(climMapOpt$userCol$custom) "disabled" else "normal"
    stateKol2 <- if(climMapOpt$userCol$custom) "normal" else "disabled"

    cb.colkey <- ttkcombobox(frameColkey, values = preset.colkey, textvariable = preset.color, width = largeur1, state = stateKol1)
    chk.colkey <- tkcheckbutton(frameColkey, variable = reverse.color, text = lang.dlg[['label']][['1']], anchor = 'w', justify = 'left', state = stateKol1)
    chk.userKol <- tkcheckbutton(frameColkey, variable = custom.color, text = lang.dlg[['label']][['2']], anchor = 'w', justify = 'left')
    bt.userKol <- ttkbutton(frameColkey, text = lang.dlg[['button']][['1']], state = stateKol2)
    canvas.preview <- tkcanvas(frameColkey, width = largeur3, height = 20, bg = 'white')

    if(!is.null(climMapOpt$pointSize)){
        framePtSz <- tkframe(frameColkey)

        txt.PointSz <- tklabel(framePtSz, text = lang.dlg[['label']][['3']], anchor = 'e', justify = 'right')
        spin.PointSz <- ttkspinbox(framePtSz, from = 0.3, to = 2.5, increment = 0.1, justify = 'center', width = 4)
        tkset(spin.PointSz, climMapOpt$pointSize)

        tkgrid(txt.PointSz, spin.PointSz)
    }

    ##Preview Color
    if(tclvalue(custom.color) == "0"){
        preview.canvasf1('1')
    }else{
        preview.canvasf2(!is.null(climMapOpt$userCol$color) &
                         length(climMapOpt$userCol$color) > 0)
    }

    tkgrid(cb.colkey, row = 0, column = 1, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
    tkgrid(chk.colkey, row = 0, column = 2, sticky = 'w', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
    if(!is.null(climMapOpt$pointSize))
        tkgrid(framePtSz, row = 1, column = 0, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
    tkgrid(chk.userKol, row = 1, column = 1, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
    tkgrid(bt.userKol, row = 1, column = 2, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
    tkgrid(canvas.preview, row = 2, column = 0, sticky = 'we', rowspan = 1, columnspan = 3, padx = 1, pady = 1, ipadx = 1, ipady = 1)

    #########

    tkconfigure(bt.userKol, command = function(){
        tcl('wm', 'attributes', tt, topmost = FALSE)
        climMapOpt$userCol$color <<- createColorkey(.cdtEnv$tcl$main$win,
                                                    climMapOpt$userCol$color)
        tcl('wm', 'attributes', tt, topmost = TRUE)
        preview.canvasf2(!is.null(climMapOpt$userCol$color))
    })

    tkbind(chk.userKol, "<Button-1>", function(){
        stateKol1 <- if(tclvalue(custom.color) == '1') 'normal' else 'disabled'
        tkconfigure(cb.colkey, state = stateKol1)
        tkconfigure(chk.colkey, state = stateKol1)
        stateKol2 <- if(tclvalue(custom.color) == '0') 'normal' else 'disabled'
        tkconfigure(bt.userKol, state = stateKol2)

        if(tclvalue(custom.color) == '0'){
            preview.canvasf2(!is.null(climMapOpt$userCol$color))
        }else{
            preview.canvasf1('1')
        }
    })

    tkbind(cb.colkey, "<<ComboboxSelected>>", function() preview.canvasf1('1'))
    tkbind(chk.colkey, "<Button-1>", function() preview.canvasf1('0'))

    #####################

    frameLevel <- ttklabelframe(frDialog, text = lang.dlg[['label']][['4']], relief = 'groove')

    equidist.level <- tclVar(climMapOpt$userLvl$equidist)
    custom.level <- tclVar(climMapOpt$userLvl$custom)
    stateEditLvl <- if(climMapOpt$userLvl$custom) 'normal' else 'disabled'

    chk.Level <- tkcheckbutton(frameLevel, variable = custom.level, text = lang.dlg[['label']][['5']], anchor = 'w', justify = 'left')
    yscrLevel <- tkscrollbar(frameLevel, repeatinterval = 4, command = function(...) tkyview(textLevel, ...))
    textLevel <- tktext(frameLevel, bg = "white", wrap = "word", height = 3, width = largeur2,
                            yscrollcommand = function(...) tkset(yscrLevel, ...))
    chk.Equidist <- tkcheckbutton(frameLevel, variable = equidist.level, text = lang.dlg[['label']][['6']], anchor = 'w', justify = 'left')

    tkgrid(chk.Level, sticky = "we")
    tkgrid(textLevel, yscrLevel)
    tkgrid.configure(yscrLevel, sticky = "ns")
    tkgrid.configure(textLevel, sticky = 'nswe')
    tkgrid(chk.Equidist, sticky = "we")

    if(length(climMapOpt$userLvl$levels) > 0)
        for(j in seq_along(climMapOpt$userLvl$levels))
            tkinsert(textLevel, "end", paste0(climMapOpt$userLvl$levels[j], ', '))
    tkconfigure(textLevel, state = stateEditLvl)

    #########

    tkbind(chk.Level, "<Button-1>", function(){
        stateEditLvl <- if(tclvalue(custom.level) == '0') 'normal' else 'disabled'
        tkconfigure(textLevel, state = stateEditLvl)
    })

    #####################

    frameMapTitle <- ttklabelframe(frDialog, text = lang.dlg[['label']][['7']], relief = 'groove')

    user.title <- tclVar(climMapOpt$title$user)
    text.title <- tclVar(climMapOpt$title$title)

    stateMpTlt <- if(climMapOpt$title$user) 'normal' else 'disabled'

    chk.MpTlt <- tkcheckbutton(frameMapTitle, variable = user.title, anchor = 'e', justify = 'right')
    en.MpTlt <- tkentry(frameMapTitle, textvariable = text.title, width = largeur4, state = stateMpTlt)

    tkgrid(chk.MpTlt, row = 0, column = 0, sticky = 'e')
    tkgrid(en.MpTlt, row = 0, column = 1, sticky = 'we')

    #########

    tkbind(chk.MpTlt, "<Button-1>", function(){
        stateMpTlt <- if(tclvalue(user.title) == '0') 'normal' else 'disabled'
        tkconfigure(en.MpTlt, state = stateMpTlt)
    })

    #####################

    frameColKeyLab <- ttklabelframe(frDialog, text = lang.dlg[['label']][['8']], relief = 'groove')

    user.clLab <- tclVar(climMapOpt$colkeyLab$user)
    text.clLab <- tclVar(climMapOpt$colkeyLab$label)

    stateColLab <- if(climMapOpt$colkeyLab$user) 'normal' else 'disabled'

    chk.clLab <- tkcheckbutton(frameColKeyLab, variable = user.clLab, anchor = 'e', justify = 'right')
    en.clLab <- tkentry(frameColKeyLab, textvariable = text.clLab, width = largeur4, state = stateColLab)

    tkgrid(chk.clLab, row = 0, column = 0, sticky = 'e')
    tkgrid(en.clLab, row = 0, column = 1, sticky = 'we')

    #########

    tkbind(chk.clLab, "<Button-1>", function(){
        stateColLab <- if(tclvalue(user.clLab) == '0') 'normal' else 'disabled'
        tkconfigure(en.clLab, state = stateColLab)
    })

    #####################

    if(!is.null(climMapOpt$scalebar)){
        frameMpScale <- ttklabelframe(frDialog, text = lang.dlg[['label']][['9']], relief = 'groove')

        place.scale <- c('bottomleft', 'bottomcenter', 'bottomright')
        add.scale <- tclVar(climMapOpt$scalebar$add)
        pos.scale <- tclVar(climMapOpt$scalebar$pos)

        stateSclBr <- if(climMapOpt$scalebar$add) 'normal' else 'disabled'

        chk.MpScl <- tkcheckbutton(frameMpScale, variable = add.scale, text = lang.dlg[['label']][['10']], anchor = 'w', justify = 'left')
        cb.MpScl <- ttkcombobox(frameMpScale, values = place.scale, textvariable = pos.scale, width = largeur5, state = stateSclBr)

        tkgrid(chk.MpScl, row = 0, column = 0, sticky = 'we', padx = 1)
        tkgrid(cb.MpScl, row = 0, column = 1, sticky = 'e', padx = 1)

        #########

        tkbind(chk.MpScl, "<Button-1>", function(){
            stateSclBr <- if(tclvalue(add.scale) == '0') 'normal' else 'disabled'
            tkconfigure(cb.MpScl, state = stateSclBr)
        })
    }

    #####################
    tkgrid(frameColkey, row = 0, column = 0, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
    tkgrid(frameLevel, row = 1, column = 0, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
    tkgrid(frameMapTitle, row = 2, column = 0, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
    tkgrid(frameColKeyLab, row = 3, column = 0, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
    if(!is.null(climMapOpt$scalebar))
        tkgrid(frameMpScale, row = 4, column = 0, sticky = '', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)

    #####################
    bt.opt.OK <- ttkbutton(frButt, text = .cdtEnv$tcl$lang$global[['button']][['1']])
    bt.opt.CA <- ttkbutton(frButt, text = .cdtEnv$tcl$lang$global[['button']][['2']])

    tkconfigure(bt.opt.OK, command = function(){
        climMapOpt$presetCol$color <<- str_trim(tclvalue(preset.color))
        climMapOpt$presetCol$reverse <<- switch(tclvalue(reverse.color), '0' = FALSE, '1' = TRUE)
        climMapOpt$userCol$custom <<- switch(tclvalue(custom.color), '0' = FALSE, '1' = TRUE)
        climMapOpt$userLvl$custom <<- switch(tclvalue(custom.level), '0' = FALSE, '1' = TRUE)
        climMapOpt$userLvl$equidist <<- switch(tclvalue(equidist.level), '0' = FALSE, '1' = TRUE)
        if(climMapOpt$userLvl$custom){
            vlevel <- tclvalue(tkget(textLevel, "0.0", "end"))
            vlevel <- gsub("[\t\r\n]", "", vlevel)
            vlevel <- gsub('\\s+', '', vlevel)
            vlevel <- strsplit(vlevel, ",")[[1]]
            vlevel <- vlevel[!is.na(vlevel) | vlevel != '']
            if(length(vlevel) < 2){
                cdt.tkmessageBox(tt, message = lang.dlg[['message']][['1']], icon = "warning", type = "ok")
                tkwait.window(tt)
            }
            climMapOpt$userLvl$levels <<- as.numeric(vlevel)
        }
        climMapOpt$title$user <<- switch(tclvalue(user.title), '0' = FALSE, '1' = TRUE)
        climMapOpt$title$title <<- str_trim(tclvalue(text.title))
        climMapOpt$colkeyLab$user <<- switch(tclvalue(user.clLab), '0' = FALSE, '1' = TRUE)
        climMapOpt$colkeyLab$label <<- str_trim(tclvalue(text.clLab))

        if(!is.null(climMapOpt$scalebar)){
            climMapOpt$scalebar$add <<- switch(tclvalue(add.scale), '0' = FALSE, '1' = TRUE)
            climMapOpt$scalebar$pos <<- str_trim(tclvalue(pos.scale))
        }

        if(!is.null(climMapOpt$pointSize))
            climMapOpt$pointSize <<- as.numeric(str_trim(tclvalue(tkget(spin.PointSz))))

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
    return(climMapOpt)
}

#######################################################################################################

MapGraph.MapOptions.VarNetCDF <- function(climMapOpt, parent.win = .cdtEnv$tcl$main$win){
    if(WindowsOS()){
        largeur1 <- 47
        largeur2 <- 60
        largeur3 <- .cdtEnv$tcl$fun$w.scale(30)
        largeur4 <- 78
    }else{
        largeur1 <- 30
        largeur2 <- 59
        largeur3 <- .cdtEnv$tcl$fun$w.scale(29.6)
        largeur4 <- 50
    }

    #####################

    xml.dlg <- file.path(.cdtDir$dirLocal, "languages", "cdtMapOptions.VarNetCDF_dlgBox.xml")
    lang.dlg <- cdtLanguageParse(xml.dlg, .cdtData$Config$lang.iso)

    #####################
    tt <- tktoplevel()
    tkgrab.set(tt)
    tkfocus(tt)

    #####################
    frDialog <- tkframe(tt, relief = 'raised', borderwidth = 2)
    frButt <- tkframe(tt)

    #####################

    frameColkey <- ttklabelframe(frDialog, text = "Colorkey", relief = 'groove')

    preset.colkey <- c('tim.colors', 'rainbow', 'heat.colors', 'cm.colors', 'topo.colors',
                       'terrain.colors', 'spi.colors', 'precip.colors', 'decile.colors')

    preset.color <- tclVar(climMapOpt$presetCol$color)
    reverse.color <- tclVar(climMapOpt$presetCol$reverse)
    custom.color <- tclVar(climMapOpt$userCol$custom)

    stateKol1 <- if(climMapOpt$userCol$custom) "disabled" else "normal"
    stateKol2 <- if(climMapOpt$userCol$custom) "normal" else "disabled"

    cb.colkey <- ttkcombobox(frameColkey, values = preset.colkey, textvariable = preset.color, width = largeur1, state = stateKol1)
    chk.colkey <- tkcheckbutton(frameColkey, variable = reverse.color, text = lang.dlg[['label']][['1']], anchor = 'w', justify = 'left', state = stateKol1)
    chk.userKol <- tkcheckbutton(frameColkey, variable = custom.color, text = lang.dlg[['label']][['2']], anchor = 'w', justify = 'left')
    bt.userKol <- ttkbutton(frameColkey, text = lang.dlg[['button']][['1']], state = stateKol2)
    canvas.preview <- tkcanvas(frameColkey, width = largeur3, height = 20, bg = 'white')

    tkgrid(cb.colkey, row = 0, column = 0, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
    tkgrid(chk.colkey, row = 0, column = 1, sticky = 'w', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
    tkgrid(chk.userKol, row = 1, column = 0, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
    tkgrid(bt.userKol, row = 1, column = 1, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
    tkgrid(canvas.preview, row = 2, column = 0, sticky = 'we', rowspan = 1, columnspan = 2, padx = 1, pady = 1, ipadx = 1, ipady = 1)

    #########
    ##Preview Color
    if(tclvalue(custom.color) == "0"){
        funkol <- get(tclvalue(preset.color), mode = "function")
        listCol <- funkol(10)
        if(tclvalue(reverse.color) == '1') listCol <- rev(listCol)
        kolor <- getGradientColor(listCol, 0:largeur3)
        tkdelete(canvas.preview, 'gradlines0')
        for(i in 0:largeur3)
            tkcreate(canvas.preview, "line", i, 0, i, 20, fill = kolor[i], tags = 'gradlines0')
    }else{
        if(!is.null(climMapOpt$userCol$color) & length(climMapOpt$userCol$color) > 0){
            kolor <- getGradientColor(climMapOpt$userCol$color, 0:largeur3)
            tkdelete(canvas.preview, 'gradlines0')
            for(i in 0:largeur3)
                tkcreate(canvas.preview, "line", i, 0, i, 20, fill = kolor[i], tags = 'gradlines0')
        }else tkdelete(canvas.preview, 'gradlines0')
    }

    tkconfigure(bt.userKol, command = function(){
        tcl('wm', 'attributes', tt, topmost = FALSE)
        climMapOpt$userCol$color <<- createColorkey(.cdtEnv$tcl$main$win,
                                                    climMapOpt$userCol$color)
        tcl('wm', 'attributes', tt, topmost = TRUE)

        if(!is.null(climMapOpt$userCol$color)){
            kolor <- getGradientColor(climMapOpt$userCol$color, 0:largeur3)
            tkdelete(canvas.preview, 'gradlines0')
            for(i in 0:largeur3)
                tkcreate(canvas.preview, "line", i, 0, i, 20, fill = kolor[i], tags = 'gradlines0')
        }else tkdelete(canvas.preview, 'gradlines0')
    })

    #########

    tkbind(chk.userKol, "<Button-1>", function(){
        stateKol1 <- if(tclvalue(custom.color) == '1') 'normal' else 'disabled'
        tkconfigure(cb.colkey, state = stateKol1)
        tkconfigure(chk.colkey, state = stateKol1)
        stateKol2 <- if(tclvalue(custom.color) == '0') 'normal' else 'disabled'
        tkconfigure(bt.userKol, state = stateKol2)

        if(tclvalue(custom.color) == '0'){
            if(!is.null(climMapOpt$userCol$color)){
                kolor <- getGradientColor(climMapOpt$userCol$color, 0:largeur3)
                tkdelete(canvas.preview, 'gradlines0')
                for(i in 0:largeur3)
                    tkcreate(canvas.preview, "line", i, 0, i, 20, fill = kolor[i], tags = 'gradlines0')
            }else tkdelete(canvas.preview, 'gradlines0')
        }else{
            funkol <- get(tclvalue(preset.color), mode = "function")
            listCol <- funkol(10)
            if(tclvalue(reverse.color) == '1') listCol <- rev(listCol)
            kolor <- getGradientColor(listCol, 0:largeur3)
            tkdelete(canvas.preview, 'gradlines0')
            for(i in 0:largeur3)
                tkcreate(canvas.preview, "line", i, 0, i, 20, fill = kolor[i], tags = 'gradlines0')
        }
    })

    tkbind(cb.colkey, "<<ComboboxSelected>>", function(){
        funkol <- get(tclvalue(preset.color), mode = "function")
        listCol <- funkol(10)
        if(tclvalue(reverse.color) == '1') listCol <- rev(listCol)
        kolor <- getGradientColor(listCol, 0:largeur3)
        tkdelete(canvas.preview, 'gradlines0')
        for(i in 0:largeur3)
            tkcreate(canvas.preview, "line", i, 0, i, 20, fill = kolor[i], tags = 'gradlines0')
    })

    tkbind(chk.colkey, "<Button-1>", function(){
        funkol <- get(tclvalue(preset.color), mode = "function")
        listCol <- funkol(10)
        if(tclvalue(reverse.color) == '0') listCol <- rev(listCol)
        kolor <- getGradientColor(listCol, 0:largeur3)
        tkdelete(canvas.preview, 'gradlines0')
        for(i in 0:largeur3)
            tkcreate(canvas.preview, "line", i, 0, i, 20, fill = kolor[i], tags = 'gradlines0')
    })

    #####################

    frameLevel <- ttklabelframe(frDialog, text = lang.dlg[['label']][['3']], relief = 'groove')

    equidist.level <- tclVar(climMapOpt$userLvl$equidist)
    custom.level <- tclVar(climMapOpt$userLvl$custom)
    stateEditLvl <- if(climMapOpt$userLvl$custom) 'normal' else 'disabled'

    chk.Level <- tkcheckbutton(frameLevel, variable = custom.level, text = lang.dlg[['label']][['4']], anchor = 'w', justify = 'left')
    yscrLevel <- tkscrollbar(frameLevel, repeatinterval = 4, command = function(...) tkyview(textLevel, ...))
    textLevel <- tktext(frameLevel, bg = "white", wrap = "word", height = 3, width = largeur2,
                            yscrollcommand = function(...) tkset(yscrLevel, ...))
    chk.Equidist <- tkcheckbutton(frameLevel, variable = equidist.level, text = lang.dlg[['label']][['5']], anchor = 'w', justify = 'left')

    tkgrid(chk.Level, sticky = "we")
    tkgrid(textLevel, yscrLevel)
    tkgrid.configure(yscrLevel, sticky = "ns")
    tkgrid.configure(textLevel, sticky = 'nswe')
    tkgrid(chk.Equidist, sticky = "we")

    if(length(climMapOpt$userLvl$levels) > 0)
        for(j in seq_along(climMapOpt$userLvl$levels))
            tkinsert(textLevel, "end", paste0(climMapOpt$userLvl$levels[j], ', '))
    tkconfigure(textLevel, state = stateEditLvl)

    #########

    tkbind(chk.Level, "<Button-1>", function(){
        stateEditLvl <- if(tclvalue(custom.level) == '0') 'normal' else 'disabled'
        tkconfigure(textLevel, state = stateEditLvl)
    })

    #####################

    frameMapTitle <- ttklabelframe(frDialog, text = lang.dlg[['label']][['6']], relief = 'groove')

    user.title <- tclVar(climMapOpt$title$user)
    text.title <- tclVar(climMapOpt$title$title)

    stateMpTlt <- if(climMapOpt$title$user) 'normal' else 'disabled'

    chk.MpTlt <- tkcheckbutton(frameMapTitle, variable = user.title, anchor = 'e', justify = 'right')
    en.MpTlt <- tkentry(frameMapTitle, textvariable = text.title, width = largeur4, state = stateMpTlt)

    tkgrid(chk.MpTlt, row = 0, column = 0, sticky = 'e')
    tkgrid(en.MpTlt, row = 0, column = 1, sticky = 'we')

    #########

    tkbind(chk.MpTlt, "<Button-1>", function(){
        stateMpTlt <- if(tclvalue(user.title) == '0') 'normal' else 'disabled'
        tkconfigure(en.MpTlt, state = stateMpTlt)
    })

    #####################
    tkgrid(frameColkey, row = 0, column = 0, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
    tkgrid(frameLevel, row = 1, column = 0, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
    tkgrid(frameMapTitle, row = 2, column = 0, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)

    #####################
    bt.opt.OK <- ttkbutton(frButt, text = .cdtEnv$tcl$lang$global[['button']][['1']])
    bt.opt.CA <- ttkbutton(frButt, text = .cdtEnv$tcl$lang$global[['button']][['2']])

    tkconfigure(bt.opt.OK, command = function(){
        climMapOpt$presetCol$color <<- str_trim(tclvalue(preset.color))
        climMapOpt$presetCol$reverse <<- switch(tclvalue(reverse.color), '0' = FALSE, '1' = TRUE)
        climMapOpt$userCol$custom <<- switch(tclvalue(custom.color), '0' = FALSE, '1' = TRUE)
        climMapOpt$userLvl$custom <<- switch(tclvalue(custom.level), '0' = FALSE, '1' = TRUE)
        climMapOpt$userLvl$equidist <<- switch(tclvalue(equidist.level), '0' = FALSE, '1' = TRUE)
        if(climMapOpt$userLvl$custom){
            vlevel <- tclvalue(tkget(textLevel, "0.0", "end"))
            vlevel <- gsub("[\t\r\n]", "", vlevel)
            vlevel <- gsub('\\s+', '', vlevel)
            vlevel <- strsplit(vlevel, ",")[[1]]
            vlevel <- vlevel[!is.na(vlevel) | vlevel != '']
            if(length(vlevel) < 2){
                cdt.tkmessageBox(tt, message = lang.dlg[['message']][['1']], icon = "warning", type = "ok")
                tkwait.window(tt)
            }
            climMapOpt$userLvl$levels <<- as.numeric(vlevel)
        }
        climMapOpt$title$user <<- switch(tclvalue(user.title), '0' = FALSE, '1' = TRUE)
        climMapOpt$title$title <<- str_trim(tclvalue(text.title))

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
    return(climMapOpt)
}

#######################################################################################################

MapGraph.MultiDatasets <- function(mapOpt){
    if(WindowsOS()){
        largeur1 <- 40
        largeur2 <- 21
    }else{
        largeur1 <- 29
        largeur2 <- 14
    }

    #####################

    xml.dlg <- file.path(.cdtDir$dirLocal, "languages", "cdtMapOptions.MultiDatasets_dlgBox.xml")
    lang.dlg <- cdtLanguageParse(xml.dlg, .cdtData$Config$lang.iso)

    #####################
    tt <- tktoplevel()
    tkgrab.set(tt)
    tkfocus(tt)

    #####################
    frDialog <- tkframe(tt, relief = 'raised', borderwidth = 2)
    frButt <- tkframe(tt)

    #####################

    frameTitle <- tkframe(frDialog)

    title.var <- tclVar(mapOpt$title)

    txt.title <- tklabel(frameTitle, text = lang.dlg[['label']][['1']], anchor = 'w', justify = 'left')
    en.title <- tkentry(frameTitle, textvariable = title.var, width = largeur1)

    tkgrid(txt.title, row = 0, column = 0, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
    tkgrid(en.title, row = 0, column = 1, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)

    #####################

    framePlotType <- tkframe(frDialog)

    cbPlotTypeVal <- if(mapOpt$map.type == "Points") c("Points", "Pixels") else c("Pixels", "Raster")
    plotType <- if(mapOpt$plot.type %in% cbPlotTypeVal) mapOpt$plot.type else cbPlotTypeVal[1]
    plot.type.var <- tclVar(plotType)

    txt.plotType <- tklabel(framePlotType, text = lang.dlg[['label']][['2']], anchor = 'w', justify = 'left')
    cb.plotType <- ttkcombobox(framePlotType, values = cbPlotTypeVal, textvariable = plot.type.var, width = largeur2)

    tkgrid(txt.plotType, row = 0, column = 0, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
    tkgrid(cb.plotType, row = 0, column = 1, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)

    #####################

    if(mapOpt$map.type == "Points"){
        framePtSz <- tkframe(frDialog)

        statePts <- if(str_trim(tclvalue(plot.type.var)) == "Points") 'normal' else 'disabled'

        txt.PointSz <- tklabel(framePtSz, text = lang.dlg[['label']][['3']], anchor = 'w', justify = 'left')
        spin.PointSz <- ttkspinbox(framePtSz, from = 0.3, to = 2.5, increment = 0.1, justify = 'center', width = 4, state = statePts)
        tkset(spin.PointSz, mapOpt$point.size)

        tkgrid(txt.PointSz, spin.PointSz)

        tkbind(cb.plotType, "<<ComboboxSelected>>", function(){
            statePts <- if(str_trim(tclvalue(plot.type.var)) == "Points") 'normal' else 'disabled'
            tkconfigure(spin.PointSz, state = statePts)
        })
    }

    #####################

    tkgrid(frameTitle, row = 0, column = 0, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
    tkgrid(framePlotType, row = 1, column = 0, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
    if(mapOpt$map.type == "Points")
        tkgrid(framePtSz, row = 2, column = 0, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)

    #####################

    bt.opt.OK <- ttkbutton(frButt, text = .cdtEnv$tcl$lang$global[['button']][['1']])
    bt.opt.CA <- ttkbutton(frButt, text = .cdtEnv$tcl$lang$global[['button']][['2']])

    tkconfigure(bt.opt.OK, command = function(){
        mapOpt$title <<- str_trim(tclvalue(title.var))
        mapOpt$plot.type <<- str_trim(tclvalue(plot.type.var))

        if(mapOpt$map.type == "Points")
            mapOpt$point.size <<- as.numeric(str_trim(tclvalue(tkget(spin.PointSz))))

        tkgrab.release(tt)
        tkdestroy(tt)
        tkfocus(.cdtEnv$tcl$main$win)
    })

    tkconfigure(bt.opt.CA, command = function(){
        tkgrab.release(tt)
        tkdestroy(tt)
        tkfocus(.cdtEnv$tcl$main$win)
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
        tkfocus(.cdtEnv$tcl$main$win)
    })
    tkwait.window(tt)
    return(mapOpt)
}

#######################################################################################################

MapGraph.ChkCoordsOptions <- function(CrdOpt, parent.win = .cdtEnv$tcl$main$win){
    if(WindowsOS()){
        width.col <- 4
        width.spin <- 5
    }else{
        width.col <- 1
        width.spin <- 3
    }

    #####################

    xml.dlg <- file.path(.cdtDir$dirLocal, "languages", "cdtMapOptions.ChkCoords_dlgBox.xml")
    lang.dlg <- cdtLanguageParse(xml.dlg, .cdtData$Config$lang.iso)

    #####################
    tt <- tktoplevel()
    tkgrab.set(tt)
    tkfocus(tt)

    #####################
    frDialog <- tkframe(tt, relief = 'raised', borderwidth = 2)
    frButt <- tkframe(tt)

    #####################

    frameBlue <- ttklabelframe(frDialog, text = lang.dlg[['label']][['2']], relief = 'groove')

    blue.col <- tclVar(CrdOpt$blue$col)

    txt.blueC <- tklabel(frameBlue, text = lang.dlg[['label']][['3']], anchor = 'e', justify = 'right')
    bt.blueC <- tkbutton(frameBlue, bg = tclvalue(blue.col), width = width.col)
    txt.blueT <- tklabel(frameBlue, text = lang.dlg[['label']][['4']], anchor = 'e', justify = 'right')
    spin.blueT <- ttkspinbox(frameBlue, from = 0, to = 25, increment = 1, justify = 'center', width = width.spin)
    tkset(spin.blueT, CrdOpt$blue$pch)
    txt.blueS <- tklabel(frameBlue, text = lang.dlg[['label']][['5']], anchor = 'e', justify = 'right')
    spin.blueS <- ttkspinbox(frameBlue, from = 0.5, to = 4, increment = 0.1, justify = 'center', width = width.spin)
    tkset(spin.blueS, CrdOpt$blue$cex)

    tkgrid(txt.blueC, row = 0, column = 0, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
    tkgrid(bt.blueC, row = 0, column = 1, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
    tkgrid(txt.blueT, row = 0, column = 2, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
    tkgrid(spin.blueT, row = 0, column = 3, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
    tkgrid(txt.blueS, row = 0, column = 4, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
    tkgrid(spin.blueS, row = 0, column = 5, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)

    ######
    tkconfigure(bt.blueC, command = function(){
        tcl('wm', 'attributes', tt, topmost = FALSE)
        loko <- str_trim(tclvalue(tcl("tk_chooseColor", initialcolor = tclvalue(blue.col), title = lang.dlg[['label']][['1']])))
        tcl('wm', 'attributes', tt, topmost = TRUE)
        if(nchar(loko) > 0){
            tkconfigure(bt.blueC, bg = loko)
            tclvalue(blue.col) <- loko
        }
    })

    #####################

    frameOrange <- ttklabelframe(frDialog, text = lang.dlg[['label']][['7']], relief = 'groove')

    orange.col <- tclVar(CrdOpt$orange$col)

    txt.orangeC <- tklabel(frameOrange, text = lang.dlg[['label']][['3']], anchor = 'e', justify = 'right')
    bt.orangeC <- tkbutton(frameOrange, bg = tclvalue(orange.col), width = width.col)
    txt.orangeT <- tklabel(frameOrange, text = lang.dlg[['label']][['4']], anchor = 'e', justify = 'right')
    spin.orangeT <- ttkspinbox(frameOrange, from = 0, to = 25, increment = 1, justify = 'center', width = width.spin)
    tkset(spin.orangeT, CrdOpt$orange$pch)
    txt.orangeS <- tklabel(frameOrange, text = lang.dlg[['label']][['5']], anchor = 'e', justify = 'right')
    spin.orangeS <- ttkspinbox(frameOrange, from = 0.5, to = 4, increment = 0.1, justify = 'center', width = width.spin)
    tkset(spin.orangeS, CrdOpt$orange$cex)

    tkgrid(txt.orangeC, row = 0, column = 0, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
    tkgrid(bt.orangeC, row = 0, column = 1, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
    tkgrid(txt.orangeT, row = 0, column = 2, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
    tkgrid(spin.orangeT, row = 0, column = 3, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
    tkgrid(txt.orangeS, row = 0, column = 4, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
    tkgrid(spin.orangeS, row = 0, column = 5, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)

    ######
    tkconfigure(bt.orangeC, command = function(){
        tcl('wm', 'attributes', tt, topmost = FALSE)
        loko <- str_trim(tclvalue(tcl("tk_chooseColor", initialcolor = tclvalue(orange.col), title = lang.dlg[['label']][['1']])))
        tcl('wm', 'attributes', tt, topmost = TRUE)
        if(nchar(loko) > 0){
            tkconfigure(bt.orangeC, bg = loko)
            tclvalue(orange.col) <- loko
        }
    })

    #####################

    frameRed <- ttklabelframe(frDialog, text = lang.dlg[['label']][['8']], relief = 'groove')

    red.col <- tclVar(CrdOpt$red$col)

    txt.redC <- tklabel(frameRed, text = lang.dlg[['label']][['3']], anchor = 'e', justify = 'right')
    bt.redC <- tkbutton(frameRed, bg = tclvalue(red.col), width = width.col)
    txt.redT <- tklabel(frameRed, text = lang.dlg[['label']][['4']], anchor = 'e', justify = 'right')
    spin.redT <- ttkspinbox(frameRed, from = 0, to = 25, increment = 1, justify = 'center', width = width.spin)
    tkset(spin.redT, CrdOpt$red$pch)
    txt.redS <- tklabel(frameRed, text = lang.dlg[['label']][['5']], anchor = 'e', justify = 'right')
    spin.redS <- ttkspinbox(frameRed, from = 0.5, to = 4, increment = 0.1, justify = 'center', width = width.spin)
    tkset(spin.redS, CrdOpt$red$cex)

    tkgrid(txt.redC, row = 0, column = 0, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
    tkgrid(bt.redC, row = 0, column = 1, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
    tkgrid(txt.redT, row = 0, column = 2, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
    tkgrid(spin.redT, row = 0, column = 3, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
    tkgrid(txt.redS, row = 0, column = 4, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
    tkgrid(spin.redS, row = 0, column = 5, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)

    ######
    tkconfigure(bt.redC, command = function(){
        tcl('wm', 'attributes', tt, topmost = FALSE)
        loko <- str_trim(tclvalue(tcl("tk_chooseColor", initialcolor = tclvalue(red.col), title = lang.dlg[['label']][['1']])))
        tcl('wm', 'attributes', tt, topmost = TRUE)
        if(nchar(loko) > 0){
            tkconfigure(bt.redC, bg = loko)
            tclvalue(red.col) <- loko
        }
    })

    #####################

    frLinesProp <- ttklabelframe(frDialog, text = lang.dlg[['label']][['9']], relief = 'groove')

    plot.col.line <- tclVar(CrdOpt$shp$col)

    txt.pltLineC <- tklabel(frLinesProp, text = lang.dlg[['label']][['3']], anchor = 'e', justify = 'right')
    bt.pltLineC <- tkbutton(frLinesProp, bg = tclvalue(plot.col.line), width = width.col)
    txt.pltLineW <- tklabel(frLinesProp, text = lang.dlg[['label']][['6']], anchor = 'e', justify = 'right')
    spin.pltLineW <- ttkspinbox(frLinesProp, from = 0.5, to = 4, increment = 0.1, justify = 'center', width = width.spin)
    tkset(spin.pltLineW, CrdOpt$shp$lwd)

    tkgrid(txt.pltLineC, row = 0, column = 0, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
    tkgrid(bt.pltLineC, row = 0, column = 1, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
    tkgrid(txt.pltLineW, row = 0, column = 2, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
    tkgrid(spin.pltLineW, row = 0, column = 3, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)

    ######
    tkconfigure(bt.pltLineC, command = function(){
        tcl('wm', 'attributes', tt, topmost = FALSE)
        loko <- str_trim(tclvalue(tcl("tk_chooseColor", initialcolor = tclvalue(plot.col.line), title = lang.dlg[['label']][['1']])))
        tcl('wm', 'attributes', tt, topmost = TRUE)
        if(nchar(loko) > 0){
            tkconfigure(bt.pltLineC, bg = loko)
            tclvalue(plot.col.line) <- loko
        }
    })

    #####################

    tkgrid(frameBlue, row = 0, column = 0, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 3, ipadx = 1, ipady = 1)
    tkgrid(frameOrange, row = 1, column = 0, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 3, ipadx = 1, ipady = 1)
    tkgrid(frameRed, row = 2, column = 0, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 3, ipadx = 1, ipady = 1)
    tkgrid(frLinesProp, row = 3, column = 0, sticky = '', rowspan = 1, columnspan = 1, padx = 1, pady = 3, ipadx = 1, ipady = 1)

    #####################

    bt.opt.OK <- ttkbutton(frButt, text = .cdtEnv$tcl$lang$global[['button']][['1']])
    bt.opt.CA <- ttkbutton(frButt, text = .cdtEnv$tcl$lang$global[['button']][['2']])

    tkconfigure(bt.opt.OK, command = function(){
        CrdOpt$blue$col <<- tclvalue(blue.col)
        CrdOpt$blue$pch <<- as.numeric(str_trim(tclvalue(tkget(spin.blueT))))
        CrdOpt$blue$cex <<- as.numeric(str_trim(tclvalue(tkget(spin.blueS))))

        CrdOpt$orange$col <<- tclvalue(orange.col)
        CrdOpt$orange$pch <<- as.numeric(str_trim(tclvalue(tkget(spin.orangeT))))
        CrdOpt$orange$cex <<- as.numeric(str_trim(tclvalue(tkget(spin.orangeS))))

        CrdOpt$red$col <<- tclvalue(red.col)
        CrdOpt$red$pch <<- as.numeric(str_trim(tclvalue(tkget(spin.redT))))
        CrdOpt$red$cex <<- as.numeric(str_trim(tclvalue(tkget(spin.redS))))

        CrdOpt$shp$col <<- tclvalue(plot.col.line)
        CrdOpt$shp$lwd <<- as.numeric(str_trim(tclvalue(tkget(spin.pltLineW))))

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

    tkgrid(frDialog, row = 0, column = 0, sticky = 'nswe', rowspan = 1, columnspan = 2, padx = 5, pady = 5, ipadx = 5, ipady = 5)
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
    return(CrdOpt)
}

#######################################################################################################

MapGraph.SpiVizOptions <- function(spiVizOpt, parent.win = .cdtEnv$tcl$main$win){
    if(WindowsOS()){
        largeur1 <- 47
        largeur2 <- 60
        largeur3 <- .cdtEnv$tcl$fun$w.scale(30)
        largeur4 <- 78
        largeur5 <- 74
    }else{
        largeur1 <- 30
        largeur2 <- 59
        largeur3 <- .cdtEnv$tcl$fun$w.scale(29.6)
        largeur4 <- 50
        largeur5 <- 47
    }

    #####################
    xml.dlg <- file.path(.cdtDir$dirLocal, "languages", "cdtMapOptions.SpiVizOptions_dlgBox.xml")
    lang.dlg <- cdtLanguageParse(xml.dlg, .cdtData$Config$lang.iso)

    #####################
    tt <- tktoplevel()
    tkgrab.set(tt)
    tkfocus(tt)

    #####################
    frDialog <- tkframe(tt, relief = 'raised', borderwidth = 2)
    frButt <- tkframe(tt)

    #####################

    frameColkey <- ttklabelframe(frDialog, text = lang.dlg[['label']][['1']], relief = 'groove')

    preset.colkey <- c('tim.colors', 'rainbow', 'heat.colors', 'cm.colors', 'topo.colors',
                       'terrain.colors', 'spi.colors', 'precip.colors', 'decile.colors')
    preset.color <- tclVar(spiVizOpt$presetCol$color)
    reverse.color <- tclVar(spiVizOpt$presetCol$reverse)
    custom.color <- tclVar(spiVizOpt$userCol$custom)

    stateKol1 <- if(spiVizOpt$userCol$custom) "disabled" else "normal"
    stateKol2 <- if(spiVizOpt$userCol$custom) "normal" else "disabled"

    cb.colkey <- ttkcombobox(frameColkey, values = preset.colkey, textvariable = preset.color, width = largeur1, state = stateKol1)
    chk.colkey <- tkcheckbutton(frameColkey, variable = reverse.color, text = lang.dlg[['checkbutton']][['1']], anchor = 'e', justify = 'right', state = stateKol1)
    chk.userKol <- tkcheckbutton(frameColkey, variable = custom.color, text = lang.dlg[['checkbutton']][['2']], anchor = 'w', justify = 'left')
    bt.userKol <- ttkbutton(frameColkey, text = lang.dlg[['button']][['1']], state = stateKol2)
    canvas.preview <- tkcanvas(frameColkey, width = largeur3, height = 20, bg = 'white')

    tkgrid(cb.colkey, row = 0, column = 0, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
    tkgrid(chk.colkey, row = 0, column = 1, sticky = 'w', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
    tkgrid(chk.userKol, row = 1, column = 0, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
    tkgrid(bt.userKol, row = 1, column = 1, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
    tkgrid(canvas.preview, row = 2, column = 0, sticky = 'we', rowspan = 1, columnspan = 2, padx = 1, pady = 1, ipadx = 1, ipady = 1)

    #########
    ##Preview Color
    if(tclvalue(custom.color) == "0"){
        funkol <- get(tclvalue(preset.color), mode = "function")
        listCol <- funkol(10)
        if(tclvalue(reverse.color) == '1') listCol <- rev(listCol)
        kolor <- getGradientColor(listCol, 0:largeur3)
        tkdelete(canvas.preview, 'gradlines0')
        for(i in 0:largeur3) tkcreate(canvas.preview, "line", i, 0, i, 20, fill = kolor[i], tags = 'gradlines0')
    }else{
        if(!is.null(spiVizOpt$userCol$color) & length(spiVizOpt$userCol$color) > 0){
            kolor <- getGradientColor(spiVizOpt$userCol$color, 0:largeur3)
            tkdelete(canvas.preview, 'gradlines0')
            for(i in 0:largeur3) tkcreate(canvas.preview, "line", i, 0, i, 20, fill = kolor[i], tags = 'gradlines0')
        }else tkdelete(canvas.preview, 'gradlines0')
    }

    tkconfigure(bt.userKol, command = function(){
        spiVizOpt$userCol$color <<- createColorkey(.cdtEnv$tcl$main$win, spiVizOpt$userCol$color)
        if(!is.null(spiVizOpt$userCol$color)){
            kolor <- getGradientColor(spiVizOpt$userCol$color, 0:largeur3)
            tkdelete(canvas.preview, 'gradlines0')
            for(i in 0:largeur3) tkcreate(canvas.preview, "line", i, 0, i, 20, fill = kolor[i], tags = 'gradlines0')
        }else tkdelete(canvas.preview, 'gradlines0')
    })

    #########

    tkbind(chk.userKol, "<Button-1>", function(){
        stateKol1 <- if(tclvalue(custom.color) == '1') 'normal' else 'disabled'
        tkconfigure(cb.colkey, state = stateKol1)
        tkconfigure(chk.colkey, state = stateKol1)
        stateKol2 <- if(tclvalue(custom.color) == '0') 'normal' else 'disabled'
        tkconfigure(bt.userKol, state = stateKol2)

        if(tclvalue(custom.color) == '0'){
            if(!is.null(spiVizOpt$userCol$color)){
                kolor <- getGradientColor(spiVizOpt$userCol$color, 0:largeur3)
                tkdelete(canvas.preview, 'gradlines0')
                for(i in 0:largeur3) tkcreate(canvas.preview, "line", i, 0, i, 20, fill = kolor[i], tags = 'gradlines0')
            }else tkdelete(canvas.preview, 'gradlines0')
        }else{
            funkol <- get(tclvalue(preset.color), mode = "function")
            listCol <- funkol(10)
            if(tclvalue(reverse.color) == '1') listCol <- rev(listCol)
            kolor <- getGradientColor(listCol, 0:largeur3)
            tkdelete(canvas.preview, 'gradlines0')
            for(i in 0:largeur3) tkcreate(canvas.preview, "line", i, 0, i, 20, fill = kolor[i], tags = 'gradlines0')
        }
    })

    tkbind(cb.colkey, "<<ComboboxSelected>>", function(){
        funkol <- get(tclvalue(preset.color), mode = "function")
        listCol <- funkol(10)
        if(tclvalue(reverse.color) == '1') listCol <- rev(listCol)
        kolor <- getGradientColor(listCol, 0:largeur3)
        tkdelete(canvas.preview, 'gradlines0')
        for(i in 0:largeur3) tkcreate(canvas.preview, "line", i, 0, i, 20, fill = kolor[i], tags = 'gradlines0')
    })

    tkbind(chk.colkey, "<Button-1>", function(){
        funkol <- get(tclvalue(preset.color), mode = "function")
        listCol <- funkol(10)
        if(tclvalue(reverse.color) == '0') listCol <- rev(listCol)
        kolor <- getGradientColor(listCol, 0:largeur3)
        tkdelete(canvas.preview, 'gradlines0')
        for(i in 0:largeur3) tkcreate(canvas.preview, "line", i, 0, i, 20, fill = kolor[i], tags = 'gradlines0')
    })

    #####################

    frameLevel <- ttklabelframe(frDialog, text = lang.dlg[['label']][['2']], relief = 'groove')

    equidist.level <- tclVar(spiVizOpt$userLvl$equidist)
    custom.level <- tclVar(spiVizOpt$userLvl$custom)
    stateEditLvl <- if(spiVizOpt$userLvl$custom) 'normal' else 'disabled'

    chk.Level <- tkcheckbutton(frameLevel, variable = custom.level, text = lang.dlg[['checkbutton']][['3']], anchor = 'w', justify = 'left')
    yscrLevel <- tkscrollbar(frameLevel, repeatinterval = 4, command = function(...) tkyview(textLevel, ...))
    textLevel <- tktext(frameLevel, bg = "white", wrap = "word", height = 2, width = largeur2,
                            yscrollcommand = function(...) tkset(yscrLevel, ...))
    chk.Equidist <- tkcheckbutton(frameLevel, variable = equidist.level, text = lang.dlg[['checkbutton']][['4']], anchor = 'w', justify = 'left')

    tkgrid(chk.Level, sticky = "we")
    tkgrid(textLevel, yscrLevel)
    tkgrid.configure(yscrLevel, sticky = "ns")
    tkgrid.configure(textLevel, sticky = 'nswe')
    tkgrid(chk.Equidist, sticky = "we")

    if(length(spiVizOpt$userLvl$levels) > 0)
        for(j in seq_along(spiVizOpt$userLvl$levels))
            tkinsert(textLevel, "end", paste0(spiVizOpt$userLvl$levels[j], ', '))
    tkconfigure(textLevel, state = stateEditLvl)

    #########

    tkbind(chk.Level, "<Button-1>", function(){
        stateEditLvl <- if(tclvalue(custom.level) == '0') 'normal' else 'disabled'
        tkconfigure(textLevel, state = stateEditLvl)
    })

    #####################

    frameMapTitle <- ttklabelframe(frDialog, text = lang.dlg[['label']][['3']], relief = 'groove')

    user.title <- tclVar(spiVizOpt$title$user)
    text.title <- tclVar(spiVizOpt$title$title)

    stateMpTlt <- if(spiVizOpt$title$user) 'normal' else 'disabled'

    chk.MpTlt <- tkcheckbutton(frameMapTitle, variable = user.title, anchor = 'e', justify = 'right')
    en.MpTlt <- tkentry(frameMapTitle, textvariable = text.title, width = largeur4, state = stateMpTlt)

    tkgrid(chk.MpTlt, row = 0, column = 0, sticky = 'e')
    tkgrid(en.MpTlt, row = 0, column = 1, sticky = 'we')

    #########

    tkbind(chk.MpTlt, "<Button-1>", function(){
        stateMpTlt <- if(tclvalue(user.title) == '0') 'normal' else 'disabled'
        tkconfigure(en.MpTlt, state = stateMpTlt)
    })

    #####################

    frameColKeyLab <- ttklabelframe(frDialog, text = lang.dlg[['label']][['4']], relief = 'groove')

    user.clLab <- tclVar(spiVizOpt$colkeyLab$user)
    text.clLab <- tclVar(spiVizOpt$colkeyLab$label)

    stateColLab <- if(spiVizOpt$colkeyLab$user) 'normal' else 'disabled'

    chk.clLab <- tkcheckbutton(frameColKeyLab, variable = user.clLab, anchor = 'e', justify = 'right')
    en.clLab <- tkentry(frameColKeyLab, textvariable = text.clLab, width = largeur4, state = stateColLab)

    tkgrid(chk.clLab, row = 0, column = 0, sticky = 'e')
    tkgrid(en.clLab, row = 0, column = 1, sticky = 'we')

    #########

    tkbind(chk.clLab, "<Button-1>", function(){
        stateColLab <- if(tclvalue(user.clLab) == '0') 'normal' else 'disabled'
        tkconfigure(en.clLab, state = stateColLab)
    })

    #####################

    frameGraphAxLabs <- ttklabelframe(frDialog, text = lang.dlg[['label']][['5']], relief = 'groove')

    is.xaxis.lab <- tclVar(spiVizOpt$axislabs$is.xlab)
    is.yaxis.lab <- tclVar(spiVizOpt$axislabs$is.ylab)
    xaxis.lab <- tclVar(spiVizOpt$axislabs$xlab)
    yaxis.lab <- tclVar(spiVizOpt$axislabs$ylab)

    stateXLab <- if(spiVizOpt$axislabs$is.xlab) 'normal' else 'disabled'
    stateYLab <- if(spiVizOpt$axislabs$is.ylab) 'normal' else 'disabled'

    chk.Xlab <- tkcheckbutton(frameGraphAxLabs, variable = is.xaxis.lab, text = 'Xlab', anchor = 'w', justify = 'left')
    en.Xlab <- tkentry(frameGraphAxLabs, textvariable = xaxis.lab, width = largeur5, state = stateXLab)
    chk.Ylab <- tkcheckbutton(frameGraphAxLabs, variable = is.yaxis.lab, text = 'Ylab', anchor = 'w', justify = 'left')
    en.Ylab <- tkentry(frameGraphAxLabs, textvariable = yaxis.lab, width = largeur5, state = stateYLab)

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
    tkgrid(frameColkey, row = 0, column = 0, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
    tkgrid(frameLevel, row = 1, column = 0, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
    tkgrid(frameMapTitle, row = 2, column = 0, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
    tkgrid(frameColKeyLab, row = 3, column = 0, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
    tkgrid(frameGraphAxLabs, row = 4, column = 0, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)

    #####################
    bt.opt.OK <- ttkbutton(frButt, text = .cdtEnv$tcl$lang$global[['button']][['1']])
    bt.opt.CA <- ttkbutton(frButt, text = .cdtEnv$tcl$lang$global[['button']][['2']])

    tkconfigure(bt.opt.OK, command = function(){
        spiVizOpt$presetCol$color <<- str_trim(tclvalue(preset.color))
        spiVizOpt$presetCol$reverse <<- switch(tclvalue(reverse.color), '0' = FALSE, '1' = TRUE)
        spiVizOpt$userCol$custom <<- switch(tclvalue(custom.color), '0' = FALSE, '1' = TRUE)
        spiVizOpt$userLvl$custom <<- switch(tclvalue(custom.level), '0' = FALSE, '1' = TRUE)
        spiVizOpt$userLvl$equidist <<- switch(tclvalue(equidist.level), '0' = FALSE, '1' = TRUE)
        if(spiVizOpt$userLvl$custom){
            vlevel <- tclvalue(tkget(textLevel, "0.0", "end"))
            vlevel <- gsub("[\t\r\n]", "", vlevel)
            vlevel <- gsub('\\s+', '', vlevel)
            vlevel <- strsplit(vlevel, ",")[[1]]
            vlevel <- vlevel[!is.na(vlevel) | vlevel != '']
            if(length(vlevel) < 2){
                cdt.tkmessageBox(tt, message = lang.dlg[['message']][['1']], icon = "warning", type = "ok")
                tkwait.window(tt)
            }
            spiVizOpt$userLvl$levels <<- as.numeric(vlevel)
        }
        spiVizOpt$title$user <<- switch(tclvalue(user.title), '0' = FALSE, '1' = TRUE)
        spiVizOpt$title$title <<- str_trim(tclvalue(text.title))
        spiVizOpt$colkeyLab$user <<- switch(tclvalue(user.clLab), '0' = FALSE, '1' = TRUE)
        spiVizOpt$colkeyLab$label <<- str_trim(tclvalue(text.clLab))

        spiVizOpt$axislabs$is.xlab <<- switch(tclvalue(is.xaxis.lab), '0' = FALSE, '1' = TRUE)
        spiVizOpt$axislabs$is.ylab <<- switch(tclvalue(is.yaxis.lab), '0' = FALSE, '1' = TRUE)
        spiVizOpt$axislabs$xlab <<- gsub('\\\\n', '\n', str_trim(tclvalue(xaxis.lab)))
        spiVizOpt$axislabs$ylab <<- gsub('\\\\n', '\n', str_trim(tclvalue(yaxis.lab)))

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
    return(spiVizOpt)
}

#######################################################################################################

MapGraph.QCoutliersSP <- function(qcOpt, parent.win = .cdtEnv$tcl$main$win){
    if(WindowsOS()){
        width.col <- 4
        width.spin <- 5
    }else{
        width.col <- 1
        width.spin <- 3
    }

    #####################

    xml.dlg <- file.path(.cdtDir$dirLocal, "languages", "cdtMapOptions.QCoutliers_dlgBox.xml")
    lang.dlg <- cdtLanguageParse(xml.dlg, .cdtData$Config$lang.iso)

    #####################
    tt <- tktoplevel()
    tkgrab.set(tt)
    tkfocus(tt)

    #####################
    frDialog <- tkframe(tt, relief = 'raised', borderwidth = 2)
    frButt <- tkframe(tt)

    #####################

    frameSTN <- ttklabelframe(frDialog, text = lang.dlg[['label']][['2']], relief = 'groove')

    #######
    frameSTN1 <- ttklabelframe(frameSTN, text = lang.dlg[['label']][['7']], relief = 'groove')

    stn.ptcol <- tclVar(qcOpt$stn$col)

    txt.STN1C <- tklabel(frameSTN1, text = lang.dlg[['label']][['9']], anchor = 'e', justify = 'right')
    bt.STN1C <- tkbutton(frameSTN1, bg = tclvalue(stn.ptcol), width = width.col)
    txt.STN1T <- tklabel(frameSTN1, text = lang.dlg[['label']][['10']], anchor = 'e', justify = 'right')
    spin.STN1T <- ttkspinbox(frameSTN1, from = 21, to = 25, increment = 1, justify = 'center', width = width.spin)
    tkset(spin.STN1T, qcOpt$stn$pch)
    txt.STN1S <- tklabel(frameSTN1, text = lang.dlg[['label']][['11']], anchor = 'e', justify = 'right')
    spin.STN1S <- ttkspinbox(frameSTN1, from = 0.5, to = 4, increment = 0.1, justify = 'center', width = width.spin)
    tkset(spin.STN1S, qcOpt$stn$cex)

    tkgrid(txt.STN1C, row = 0, column = 0, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
    tkgrid(bt.STN1C, row = 0, column = 1, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
    tkgrid(txt.STN1T, row = 0, column = 2, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
    tkgrid(spin.STN1T, row = 0, column = 3, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
    tkgrid(txt.STN1S, row = 0, column = 4, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
    tkgrid(spin.STN1S, row = 0, column = 5, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)

    ######
    tkconfigure(bt.STN1C, command = function(){
        tcl('wm', 'attributes', tt, topmost = FALSE)
        loko <- str_trim(tclvalue(tcl("tk_chooseColor", initialcolor = tclvalue(stn.ptcol), title = lang.dlg[['label']][['1']])))
        tcl('wm', 'attributes', tt, topmost = TRUE)
        if(nchar(loko) > 0){
            tkconfigure(bt.STN1C, bg = loko)
            tclvalue(stn.ptcol) <- loko
        }
    })

    #######
    frameSTN2 <- ttklabelframe(frameSTN, text = lang.dlg[['label']][['8']], relief = 'groove')

    stn.txtcol <- tclVar(qcOpt$stn$txt.col)

    txt.STN2C <- tklabel(frameSTN2, text = lang.dlg[['label']][['9']], anchor = 'e', justify = 'right')
    bt.STN2C <- tkbutton(frameSTN2, bg = tclvalue(stn.txtcol), width = width.col)
    txt.STN2S <- tklabel(frameSTN2, text = lang.dlg[['label']][['11']], anchor = 'e', justify = 'right')
    spin.STN2S <- ttkspinbox(frameSTN2, from = 0.5, to = 4, increment = 0.1, justify = 'center', width = width.spin)
    tkset(spin.STN2S, qcOpt$stn$txt.cex)

    tkgrid(txt.STN2C, row = 0, column = 0, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
    tkgrid(bt.STN2C, row = 0, column = 1, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
    tkgrid(txt.STN2S, row = 0, column = 2, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
    tkgrid(spin.STN2S, row = 0, column = 3, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)

    ######
    tkconfigure(bt.STN2C, command = function(){
        tcl('wm', 'attributes', tt, topmost = FALSE)
        loko <- str_trim(tclvalue(tcl("tk_chooseColor", initialcolor = tclvalue(stn.txtcol), title = lang.dlg[['label']][['1']])))
        tcl('wm', 'attributes', tt, topmost = TRUE)
        if(nchar(loko) > 0){
            tkconfigure(bt.STN2C, bg = loko)
            tclvalue(stn.txtcol) <- loko
        }
    })

    #######
    tkgrid(frameSTN1, row = 0, column = 0, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
    tkgrid(frameSTN2, row = 0, column = 1, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)

    #####################

    frameUSE <- ttklabelframe(frDialog, text = lang.dlg[['label']][['3']], relief = 'groove')

    #######
    frameUSE1 <- ttklabelframe(frameUSE, text = lang.dlg[['label']][['7']], relief = 'groove')

    use.ptcol <- tclVar(qcOpt$use$col)

    txt.USE1C <- tklabel(frameUSE1, text = lang.dlg[['label']][['9']], anchor = 'e', justify = 'right')
    bt.USE1C <- tkbutton(frameUSE1, bg = tclvalue(use.ptcol), width = width.col)
    txt.USE1T <- tklabel(frameUSE1, text = lang.dlg[['label']][['10']], anchor = 'e', justify = 'right')
    spin.USE1T <- ttkspinbox(frameUSE1, from = 15, to = 20, increment = 1, justify = 'center', width = width.spin)
    tkset(spin.USE1T, qcOpt$use$pch)
    txt.USE1S <- tklabel(frameUSE1, text = 'Size', anchor = 'e', justify = 'right')
    spin.USE1S <- ttkspinbox(frameUSE1, from = 0.5, to = 4, increment = 0.1, justify = 'center', width = width.spin)
    tkset(spin.USE1S, qcOpt$use$cex)

    tkgrid(txt.USE1C, row = 0, column = 0, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
    tkgrid(bt.USE1C, row = 0, column = 1, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
    tkgrid(txt.USE1T, row = 0, column = 2, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
    tkgrid(spin.USE1T, row = 0, column = 3, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
    tkgrid(txt.USE1S, row = 0, column = 4, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
    tkgrid(spin.USE1S, row = 0, column = 5, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)

    ######
    tkconfigure(bt.USE1C, command = function(){
        tcl('wm', 'attributes', tt, topmost = FALSE)
        loko <- str_trim(tclvalue(tcl("tk_chooseColor", initialcolor = tclvalue(use.ptcol), title = lang.dlg[['label']][['1']])))
        tcl('wm', 'attributes', tt, topmost = TRUE)
        if(nchar(loko) > 0){
            tkconfigure(bt.USE1C, bg = loko)
            tclvalue(use.ptcol) <- loko
        }
    })

    #######
    frameUSE2 <- ttklabelframe(frameUSE, text = lang.dlg[['label']][['8']], relief = 'groove')

    use.txtcol <- tclVar(qcOpt$use$txt.col)

    txt.USE2C <- tklabel(frameUSE2, text = lang.dlg[['label']][['9']], anchor = 'e', justify = 'right')
    bt.USE2C <- tkbutton(frameUSE2, bg = tclvalue(use.txtcol), width = width.col)
    txt.USE2S <- tklabel(frameUSE2, text = lang.dlg[['label']][['11']], anchor = 'e', justify = 'right')
    spin.USE2S <- ttkspinbox(frameUSE2, from = 0.5, to = 4, increment = 0.1, justify = 'center', width = width.spin)
    tkset(spin.USE2S, qcOpt$use$txt.cex)

    tkgrid(txt.USE2C, row = 0, column = 0, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
    tkgrid(bt.USE2C, row = 0, column = 1, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
    tkgrid(txt.USE2S, row = 0, column = 2, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
    tkgrid(spin.USE2S, row = 0, column = 3, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)

    ######
    tkconfigure(bt.USE2C, command = function(){
        tcl('wm', 'attributes', tt, topmost = FALSE)
        loko <- str_trim(tclvalue(tcl("tk_chooseColor", initialcolor = tclvalue(use.txtcol), title = lang.dlg[['label']][['1']])))
        tcl('wm', 'attributes', tt, topmost = TRUE)
        if(nchar(loko) > 0){
            tkconfigure(bt.USE2C, bg = loko)
            tclvalue(use.txtcol) <- loko
        }
    })

    #######
    tkgrid(frameUSE1, row = 0, column = 0, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
    tkgrid(frameUSE2, row = 0, column = 1, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)

    #####################

    frameSEL <- ttklabelframe(frDialog, text = lang.dlg[['label']][['4']], relief = 'groove')

    #######
    frameSEL1 <- ttklabelframe(frameSEL, text = lang.dlg[['label']][['7']], relief = 'groove')

    sel.ptcol <- tclVar(qcOpt$sel$col)

    txt.SEL1C <- tklabel(frameSEL1, text = lang.dlg[['label']][['9']], anchor = 'e', justify = 'right')
    bt.SEL1C <- tkbutton(frameSEL1, bg = tclvalue(sel.ptcol), width = width.col)
    txt.SEL1T <- tklabel(frameSEL1, text = lang.dlg[['label']][['10']], anchor = 'e', justify = 'right')
    spin.SEL1T <- ttkspinbox(frameSEL1, from = 15, to = 20, increment = 1, justify = 'center', width = width.spin)
    tkset(spin.SEL1T, qcOpt$sel$pch)
    txt.SEL1S <- tklabel(frameSEL1, text = lang.dlg[['label']][['11']], anchor = 'e', justify = 'right')
    spin.SEL1S <- ttkspinbox(frameSEL1, from = 0.5, to = 4, increment = 0.1, justify = 'center', width = width.spin)
    tkset(spin.SEL1S, qcOpt$sel$cex)

    tkgrid(txt.SEL1C, row = 0, column = 0, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
    tkgrid(bt.SEL1C, row = 0, column = 1, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
    tkgrid(txt.SEL1T, row = 0, column = 2, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
    tkgrid(spin.SEL1T, row = 0, column = 3, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
    tkgrid(txt.SEL1S, row = 0, column = 4, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
    tkgrid(spin.SEL1S, row = 0, column = 5, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)

    ######
    tkconfigure(bt.SEL1C, command = function(){
        tcl('wm', 'attributes', tt, topmost = FALSE)
        loko <- str_trim(tclvalue(tcl("tk_chooseColor", initialcolor = tclvalue(sel.ptcol), title = lang.dlg[['label']][['1']])))
        tcl('wm', 'attributes', tt, topmost = TRUE)
        if(nchar(loko) > 0){
            tkconfigure(bt.SEL1C, bg = loko)
            tclvalue(sel.ptcol) <- loko
        }
    })

    #######
    frameSEL2 <- ttklabelframe(frameSEL, text = lang.dlg[['label']][['8']], relief = 'groove')

    sel.txtcol <- tclVar(qcOpt$sel$txt.col)

    txt.SEL2C <- tklabel(frameSEL2, text = lang.dlg[['label']][['9']], anchor = 'e', justify = 'right')
    bt.SEL2C <- tkbutton(frameSEL2, bg = tclvalue(sel.txtcol), width = width.col)
    txt.SEL2S <- tklabel(frameSEL2, text = lang.dlg[['label']][['11']], anchor = 'e', justify = 'right')
    spin.SEL2S <- ttkspinbox(frameSEL2, from = 0.5, to = 4, increment = 0.1, justify = 'center', width = width.spin)
    tkset(spin.SEL2S, qcOpt$sel$txt.cex)

    tkgrid(txt.SEL2C, row = 0, column = 0, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
    tkgrid(bt.SEL2C, row = 0, column = 1, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
    tkgrid(txt.SEL2S, row = 0, column = 2, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
    tkgrid(spin.SEL2S, row = 0, column = 3, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)

    ######
    tkconfigure(bt.SEL2C, command = function(){
        tcl('wm', 'attributes', tt, topmost = FALSE)
        loko <- str_trim(tclvalue(tcl("tk_chooseColor", initialcolor = tclvalue(sel.txtcol), title = lang.dlg[['label']][['1']])))
        tcl('wm', 'attributes', tt, topmost = TRUE)
        if(nchar(loko) > 0){
            tkconfigure(bt.SEL2C, bg = loko)
            tclvalue(sel.txtcol) <- loko
        }
    })

    #######
    tkgrid(frameSEL1, row = 0, column = 0, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
    tkgrid(frameSEL2, row = 0, column = 1, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)

    #####################

    frameVOIS <- ttklabelframe(frDialog, text = lang.dlg[['label']][['5']], relief = 'groove')

    #######
    frameVOIS1 <- ttklabelframe(frameVOIS, text = lang.dlg[['label']][['7']], relief = 'groove')

    vois.ptcol <- tclVar(qcOpt$vois$col)

    txt.VOIS1C <- tklabel(frameVOIS1, text = lang.dlg[['label']][['9']], anchor = 'e', justify = 'right')
    bt.VOIS1C <- tkbutton(frameVOIS1, bg = tclvalue(vois.ptcol), width = width.col)
    txt.VOIS1T <- tklabel(frameVOIS1, text = lang.dlg[['label']][['10']], anchor = 'e', justify = 'right')
    spin.VOIS1T <- ttkspinbox(frameVOIS1, from = 15, to = 20, increment = 1, justify = 'center', width = width.spin)
    tkset(spin.VOIS1T, qcOpt$vois$pch)
    txt.VOIS1S <- tklabel(frameVOIS1, text = lang.dlg[['label']][['11']], anchor = 'e', justify = 'right')
    spin.VOIS1S <- ttkspinbox(frameVOIS1, from = 0.5, to = 4, increment = 0.1, justify = 'center', width = width.spin)
    tkset(spin.VOIS1S, qcOpt$vois$cex)

    tkgrid(txt.VOIS1C, row = 0, column = 0, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
    tkgrid(bt.VOIS1C, row = 0, column = 1, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
    tkgrid(txt.VOIS1T, row = 0, column = 2, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
    tkgrid(spin.VOIS1T, row = 0, column = 3, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
    tkgrid(txt.VOIS1S, row = 0, column = 4, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
    tkgrid(spin.VOIS1S, row = 0, column = 5, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)

    ######
    tkconfigure(bt.VOIS1C, command = function(){
        tcl('wm', 'attributes', tt, topmost = FALSE)
        loko <- str_trim(tclvalue(tcl("tk_chooseColor", initialcolor = tclvalue(vois.ptcol), title = lang.dlg[['label']][['1']])))
        tcl('wm', 'attributes', tt, topmost = TRUE)
        if(nchar(loko) > 0){
            tkconfigure(bt.VOIS1C, bg = loko)
            tclvalue(vois.ptcol) <- loko
        }
    })

    #######
    frameVOIS2 <- ttklabelframe(frameVOIS, text = lang.dlg[['label']][['8']], relief = 'groove')

    vois.txtcol <- tclVar(qcOpt$vois$txt.col)

    txt.VOIS2C <- tklabel(frameVOIS2, text = lang.dlg[['label']][['9']], anchor = 'e', justify = 'right')
    bt.VOIS2C <- tkbutton(frameVOIS2, bg = tclvalue(vois.txtcol), width = width.col)
    txt.VOIS2S <- tklabel(frameVOIS2, text = lang.dlg[['label']][['11']], anchor = 'e', justify = 'right')
    spin.VOIS2S <- ttkspinbox(frameVOIS2, from = 0.5, to = 4, increment = 0.1, justify = 'center', width = width.spin)
    tkset(spin.VOIS2S, qcOpt$vois$txt.cex)

    tkgrid(txt.VOIS2C, row = 0, column = 0, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
    tkgrid(bt.VOIS2C, row = 0, column = 1, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
    tkgrid(txt.VOIS2S, row = 0, column = 2, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
    tkgrid(spin.VOIS2S, row = 0, column = 3, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)

    ######
    tkconfigure(bt.VOIS2C, command = function(){
        tcl('wm', 'attributes', tt, topmost = FALSE)
        loko <- str_trim(tclvalue(tcl("tk_chooseColor", initialcolor = tclvalue(vois.txtcol), title = lang.dlg[['label']][['1']])))
        tcl('wm', 'attributes', tt, topmost = TRUE)
        if(nchar(loko) > 0){
            tkconfigure(bt.VOIS2C, bg = loko)
            tclvalue(vois.txtcol) <- loko
        }
    })

    #######
    tkgrid(frameVOIS1, row = 0, column = 0, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
    tkgrid(frameVOIS2, row = 0, column = 1, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)

    #####################
    frameALL <- ttklabelframe(frDialog, text = lang.dlg[['label']][['6']], relief = 'groove')

    #######
    frameALL1 <- ttklabelframe(frameALL, text = lang.dlg[['label']][['7']], relief = 'groove')

    all.ptcol <- tclVar(qcOpt$all$col)

    txt.ALL1C <- tklabel(frameALL1, text = lang.dlg[['label']][['9']], anchor = 'e', justify = 'right')
    bt.ALL1C <- tkbutton(frameALL1, bg = tclvalue(all.ptcol), width = width.col)
    txt.ALL1T <- tklabel(frameALL1, text = lang.dlg[['label']][['10']], anchor = 'e', justify = 'right')
    spin.ALL1T <- ttkspinbox(frameALL1, from = 15, to = 20, increment = 1, justify = 'center', width = width.spin)
    tkset(spin.ALL1T, qcOpt$all$pch)
    txt.ALL1S <- tklabel(frameALL1, text = lang.dlg[['label']][['11']], anchor = 'e', justify = 'right')
    spin.ALL1S <- ttkspinbox(frameALL1, from = 0.5, to = 4, increment = 0.1, justify = 'center', width = width.spin)
    tkset(spin.ALL1S, qcOpt$all$cex)

    tkgrid(txt.ALL1C, row = 0, column = 0, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
    tkgrid(bt.ALL1C, row = 0, column = 1, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
    tkgrid(txt.ALL1T, row = 0, column = 2, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
    tkgrid(spin.ALL1T, row = 0, column = 3, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
    tkgrid(txt.ALL1S, row = 0, column = 4, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
    tkgrid(spin.ALL1S, row = 0, column = 5, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)

    ######
    tkconfigure(bt.ALL1C, command = function(){
        tcl('wm', 'attributes', tt, topmost = FALSE)
        loko <- str_trim(tclvalue(tcl("tk_chooseColor", initialcolor = tclvalue(all.ptcol), title = lang.dlg[['label']][['1']])))
        tcl('wm', 'attributes', tt, topmost = TRUE)
        if(nchar(loko) > 0){
            tkconfigure(bt.ALL1C, bg = loko)
            tclvalue(all.ptcol) <- loko
        }
    })

    #######
    frameALL2 <- ttklabelframe(frameALL, text = lang.dlg[['label']][['8']], relief = 'groove')

    all.txtcol <- tclVar(qcOpt$all$txt.col)

    txt.ALL2C <- tklabel(frameALL2, text = lang.dlg[['label']][['9']], anchor = 'e', justify = 'right')
    bt.ALL2C <- tkbutton(frameALL2, bg = tclvalue(all.txtcol), width = width.col)
    txt.ALL2S <- tklabel(frameALL2, text = lang.dlg[['label']][['11']], anchor = 'e', justify = 'right')
    spin.ALL2S <- ttkspinbox(frameALL2, from = 0.5, to = 4, increment = 0.1, justify = 'center', width = width.spin)
    tkset(spin.ALL2S, qcOpt$all$txt.cex)

    tkgrid(txt.ALL2C, row = 0, column = 0, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
    tkgrid(bt.ALL2C, row = 0, column = 1, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
    tkgrid(txt.ALL2S, row = 0, column = 2, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
    tkgrid(spin.ALL2S, row = 0, column = 3, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)

    ######
    tkconfigure(bt.ALL2C, command = function(){
        tcl('wm', 'attributes', tt, topmost = FALSE)
        loko <- str_trim(tclvalue(tcl("tk_chooseColor", initialcolor = tclvalue(all.txtcol), title = lang.dlg[['label']][['1']])))
        tcl('wm', 'attributes', tt, topmost = TRUE)
        if(nchar(loko) > 0){
            tkconfigure(bt.ALL2C, bg = loko)
            tclvalue(all.txtcol) <- loko
        }
    })

    #######
    tkgrid(frameALL1, row = 0, column = 0, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
    tkgrid(frameALL2, row = 0, column = 1, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)

    #####################

    frameCircle <- tkframe(frDialog, relief = 'groove', borderwidth = 2)

    draw.circle <- tclVar(qcOpt$circle$draw)
    col.circle <- tclVar(qcOpt$circle$col)

    stateCircle <- if(tclvalue(draw.circle) == '1') 'normal' else 'disabled'

    chk.pltLine <- tkcheckbutton(frameCircle, variable = draw.circle, text = lang.dlg[['checkbutton']][['1']], anchor = 'w', justify = 'left')
    txt.pltLineC <- tklabel(frameCircle, text = lang.dlg[['label']][['9']], anchor = 'e', justify = 'right')
    bt.pltLineC <- tkbutton(frameCircle, bg = tclvalue(col.circle), width = width.col, state = stateCircle)
    txt.pltLineW <- tklabel(frameCircle, text = lang.dlg[['label']][['12']], anchor = 'e', justify = 'right')
    spin.pltLineW <- ttkspinbox(frameCircle, from = 0.5, to = 4, increment = 0.1, justify = 'center', width = width.spin, state = stateCircle)
    tkset(spin.pltLineW, qcOpt$circle$lwd)

    tkgrid(chk.pltLine, row = 0, column = 0, sticky = 'we', rowspan = 1, columnspan = 4, padx = 1, pady = 1, ipadx = 1, ipady = 1)
    tkgrid(txt.pltLineC, row = 1, column = 0, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
    tkgrid(bt.pltLineC, row = 1, column = 1, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
    tkgrid(txt.pltLineW, row = 1, column = 2, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
    tkgrid(spin.pltLineW, row = 1, column = 3, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)

    ######
    tkconfigure(bt.pltLineC, command = function(){
        tcl('wm', 'attributes', tt, topmost = FALSE)
        loko <- str_trim(tclvalue(tcl("tk_chooseColor", initialcolor = tclvalue(col.circle), title = lang.dlg[['label']][['1']])))
        tcl('wm', 'attributes', tt, topmost = TRUE)
        if(nchar(loko) > 0){
            tkconfigure(bt.pltLineC, bg = loko)
            tclvalue(col.circle) <- loko
        }
    })

    tkbind(chk.pltLine, "<Button-1>", function(){
        stateCircle <- if(tclvalue(draw.circle) == '1') 'disabled' else 'normal'
        tkconfigure(bt.pltLineC, state = stateCircle)
        tkconfigure(spin.pltLineW, state = stateCircle)
    })

    #####################

    tkgrid(frameSTN, row = 0, column = 0, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
    tkgrid(frameUSE, row = 1, column = 0, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 3, ipadx = 1, ipady = 1)
    tkgrid(frameSEL, row = 2, column = 0, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
    tkgrid(frameVOIS, row = 3, column = 0, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 3, ipadx = 1, ipady = 1)
    tkgrid(frameALL, row = 4, column = 0, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
    tkgrid(frameCircle, row = 5, column = 0, sticky = '', rowspan = 1, columnspan = 1, padx = 1, pady = 3, ipadx = 1, ipady = 1)

    #####################

    bt.opt.OK <- ttkbutton(frButt, text = .cdtEnv$tcl$lang$global[['button']][['1']])
    bt.opt.CA <- ttkbutton(frButt, text = .cdtEnv$tcl$lang$global[['button']][['2']])

    tkconfigure(bt.opt.OK, command = function(){
        qcOpt$stn$col <<- tclvalue(stn.ptcol)
        qcOpt$stn$pch <<- as.numeric(str_trim(tclvalue(tkget(spin.STN1T))))
        qcOpt$stn$cex <<- as.numeric(str_trim(tclvalue(tkget(spin.STN1S))))
        qcOpt$stn$txt.col <<- tclvalue(stn.txtcol)
        qcOpt$stn$txt.cex <<- as.numeric(str_trim(tclvalue(tkget(spin.STN2S))))

        qcOpt$use$col <<- tclvalue(use.ptcol)
        qcOpt$use$pch <<- as.numeric(str_trim(tclvalue(tkget(spin.USE1T))))
        qcOpt$use$cex <<- as.numeric(str_trim(tclvalue(tkget(spin.USE1S))))
        qcOpt$use$txt.col <<- tclvalue(use.txtcol)
        qcOpt$use$txt.cex <<- as.numeric(str_trim(tclvalue(tkget(spin.USE2S))))

        qcOpt$sel$col <<- tclvalue(sel.ptcol)
        qcOpt$sel$pch <<- as.numeric(str_trim(tclvalue(tkget(spin.SEL1T))))
        qcOpt$sel$cex <<- as.numeric(str_trim(tclvalue(tkget(spin.SEL1S))))
        qcOpt$sel$txt.col <<- tclvalue(sel.txtcol)
        qcOpt$sel$txt.cex <<- as.numeric(str_trim(tclvalue(tkget(spin.SEL2S))))

        qcOpt$vois$col <<- tclvalue(vois.ptcol)
        qcOpt$vois$pch <<- as.numeric(str_trim(tclvalue(tkget(spin.VOIS1T))))
        qcOpt$vois$cex <<- as.numeric(str_trim(tclvalue(tkget(spin.VOIS1S))))
        qcOpt$vois$txt.col <<- tclvalue(vois.txtcol)
        qcOpt$vois$txt.cex <<- as.numeric(str_trim(tclvalue(tkget(spin.VOIS2S))))

        qcOpt$all$col <<- tclvalue(all.ptcol)
        qcOpt$all$pch <<- as.numeric(str_trim(tclvalue(tkget(spin.ALL1T))))
        qcOpt$all$cex <<- as.numeric(str_trim(tclvalue(tkget(spin.ALL1S))))
        qcOpt$all$txt.col <<- tclvalue(all.txtcol)
        qcOpt$all$txt.cex <<- as.numeric(str_trim(tclvalue(tkget(spin.ALL2S))))

        qcOpt$circle$col <<- tclvalue(col.circle)
        qcOpt$circle$lwd <<- as.numeric(str_trim(tclvalue(tkget(spin.pltLineW))))
        qcOpt$circle$draw <<- switch(tclvalue(draw.circle), '0' = FALSE, '1' = TRUE)

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

    tkgrid(frDialog, row = 0, column = 0, sticky = 'nswe', rowspan = 1, columnspan = 2, padx = 5, pady = 5, ipadx = 5, ipady = 5)
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
    return(qcOpt)
}

#######################################################################################################

MapGraph.QCgridData <- function(mapOpt, parent.win = .cdtEnv$tcl$main$win){
    if(WindowsOS()){
        largeur2 <- 60
        largeur3 <- .cdtEnv$tcl$fun$w.scale(30)
    }else{
        largeur2 <- 59
        largeur3 <- .cdtEnv$tcl$fun$w.scale(29.6)
    }

    #####################

    xml.dlg <- file.path(.cdtDir$dirLocal, "languages", "cdtMapOptions.QCgridData_dlgBox.xml")
    lang.dlg <- cdtLanguageParse(xml.dlg, .cdtData$Config$lang.iso)

    #####################

    preview.canvasf1 <- function(){
        funkol <- get(mapOpt$preset.colors$color, mode = "function")
        listCol <- funkol(64)
        kolor <- getGradientColor(listCol, 0:largeur3)
        tkdelete(canvas.preview, 'gradlines0')
        for(i in 0:largeur3)
            tkcreate(canvas.preview, "line", i, 0, i, 20, fill = kolor[i], tags = 'gradlines0')
    }

    preview.canvasf2 <- function(cond){
        if(cond){
            kolor <- getGradientColor(mapOpt$user.colors$color, 0:largeur3)
            tkdelete(canvas.preview, 'gradlines0')
            for(i in 0:largeur3)
                tkcreate(canvas.preview, "line", i, 0, i, 20, fill = kolor[i], tags = 'gradlines0')
        }
        else tkdelete(canvas.preview, 'gradlines0')
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

    custom.color <- tclVar(mapOpt$user.colors$custom)

    stateKol2 <- if(mapOpt$user.colors$custom) "normal" else "disabled"

    chk.userKol <- tkcheckbutton(frameColkey, variable = custom.color, text = lang.dlg[['checkbutton']][['1']], anchor = 'w', justify = 'left')
    bt.userKol <- ttkbutton(frameColkey, text = lang.dlg[['button']][['1']], state = stateKol2)
    canvas.preview <- tkcanvas(frameColkey, width = largeur3, height = 20, bg = 'white')

    ## Preview Color
    if(tclvalue(custom.color) == "0"){
        preview.canvasf1()
    }else{
        preview.canvasf2(!is.null(mapOpt$user.colors$color) &
                         length(mapOpt$user.colors$color) > 0)
    }

    tkgrid(chk.userKol, row = 0, column = 0, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
    tkgrid(bt.userKol, row = 0, column = 1, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
    tkgrid(canvas.preview, row = 1, column = 0, sticky = 'we', rowspan = 1, columnspan = 2, padx = 1, pady = 1, ipadx = 1, ipady = 1)

    #########

    tkconfigure(bt.userKol, command = function(){
        tcl('wm', 'attributes', tt, topmost = FALSE)
        mapOpt$user.colors$color <<- createColorkey(.cdtEnv$tcl$main$win,
                                                    mapOpt$user.colors$color)
        tcl('wm', 'attributes', tt, topmost = TRUE)
        preview.canvasf2(!is.null(mapOpt$user.colors$color))
    })

    tkbind(chk.userKol, "<Button-1>", function(){
        stateKol2 <- if(tclvalue(custom.color) == '0') 'normal' else 'disabled'
        tkconfigure(bt.userKol, state = stateKol2)

        if(tclvalue(custom.color) == '0'){
            preview.canvasf2(!is.null(mapOpt$user.colors$color))
        }else{
            preview.canvasf1()
        }
    })

    #####################

    frameLevel <- ttklabelframe(frDialog, text = lang.dlg[['label']][['2']], relief = 'groove')

    equidist.level <- tclVar(mapOpt$user.levels$equidist)
    custom.level <- tclVar(mapOpt$user.levels$custom)
    stateEditLvl <- if(mapOpt$user.levels$custom) 'normal' else 'disabled'

    chk.Level <- tkcheckbutton(frameLevel, variable = custom.level, text = lang.dlg[['checkbutton']][['2']], anchor = 'w', justify = 'left')
    yscrLevel <- tkscrollbar(frameLevel, repeatinterval = 4, command = function(...) tkyview(textLevel, ...))
    textLevel <- tktext(frameLevel, bg = "white", wrap = "word", height = 3, width = largeur2,
                            yscrollcommand = function(...) tkset(yscrLevel, ...))
    chk.Equidist <- tkcheckbutton(frameLevel, variable = equidist.level, text = lang.dlg[['checkbutton']][['3']], anchor = 'w', justify = 'left')

    tkgrid(chk.Level, sticky = "we")
    tkgrid(textLevel, yscrLevel)
    tkgrid.configure(yscrLevel, sticky = "ns")
    tkgrid.configure(textLevel, sticky = 'nswe')
    tkgrid(chk.Equidist, sticky = "we")

    if(length(mapOpt$user.levels$levels) > 0)
        for(j in seq_along(mapOpt$user.levels$levels))
            tkinsert(textLevel, "end", paste0(mapOpt$user.levels$levels[j], ', '))
    tkconfigure(textLevel, state = stateEditLvl)

    #########

    tkbind(chk.Level, "<Button-1>", function(){
        stateEditLvl <- if(tclvalue(custom.level) == '0') 'normal' else 'disabled'
        tkconfigure(textLevel, state = stateEditLvl)
    })

    #####################
    tkgrid(frameColkey, row = 0, column = 0, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
    tkgrid(frameLevel, row = 1, column = 0, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)

    #####################
    bt.opt.OK <- ttkbutton(frButt, text = .cdtEnv$tcl$lang$global[['button']][['1']])
    bt.opt.CA <- ttkbutton(frButt, text = .cdtEnv$tcl$lang$global[['button']][['2']])

    tkconfigure(bt.opt.OK, command = function(){
        mapOpt$user.colors$custom <<- switch(tclvalue(custom.color), '0' = FALSE, '1' = TRUE)
        mapOpt$user.levels$custom <<- switch(tclvalue(custom.level), '0' = FALSE, '1' = TRUE)
        mapOpt$user.levels$equidist <<- switch(tclvalue(equidist.level), '0' = FALSE, '1' = TRUE)
        if(mapOpt$user.levels$custom){
            vlevel <- tclvalue(tkget(textLevel, "0.0", "end"))
            vlevel <- gsub("[\t\r\n]", "", vlevel)
            vlevel <- gsub('\\s+', '', vlevel)
            vlevel <- strsplit(vlevel, ",")[[1]]
            vlevel <- vlevel[!is.na(vlevel) | vlevel != '']
            if(length(vlevel) < 2){
                cdt.tkmessageBox(tt, message = lang.dlg[['message']][['1']], icon = "warning", type = "ok")
                tkwait.window(tt)
            }
            mapOpt$user.levels$levels <<- as.numeric(vlevel)
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
    return(mapOpt)
}
