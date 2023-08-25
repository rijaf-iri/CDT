
getNewGridParams <- function(parent.win, Parameters){
    xml.dlg <- file.path(.cdtDir$dirLocal, "languages", "cdtInfoNewGrid_dlgBox.xml")
    lang.dlg <- cdtLanguageParse(xml.dlg, .cdtData$Config$lang.iso)

    ################################

    tt <- tktoplevel()
    tkgrab.set(tt)
    tkfocus(tt)

    frGrd0 <- tkframe(tt, relief = 'raised', borderwidth = 2)
    frGrd1 <- tkframe(tt)

    ################################

    fr_grd <- ttklabelframe(frGrd0, text = lang.dlg[['label']][['1']], relief = "groove", borderwidth = 2)

    minLon <- tclVar(Parameters$minlon)
    maxLon <- tclVar(Parameters$maxlon)
    resLon <- tclVar(Parameters$reslon)
    minLat <- tclVar(Parameters$minlat)
    maxLat <- tclVar(Parameters$maxlat)
    resLat <- tclVar(Parameters$reslat)

    grd_llon <- tklabel(fr_grd, text = lang.dlg[['label']][['2']], anchor = 'e', justify = 'right')
    grd_llat <- tklabel(fr_grd, text = lang.dlg[['label']][['3']], anchor = 'e', justify = 'right')
    grd_lb1 <- tklabel(fr_grd, text = lang.dlg[['label']][['4']])
    grd_lb2 <- tklabel(fr_grd, text = lang.dlg[['label']][['5']])
    grd_lb3 <- tklabel(fr_grd, text = lang.dlg[['label']][['6']])

    grd_vlon1 <- tkentry(fr_grd, width = 6, justify = "center", textvariable = minLon)
    grd_vlon2 <- tkentry(fr_grd, width = 6, justify = "center", textvariable = maxLon)
    grd_vlon3 <- tkentry(fr_grd, width = 6, justify = "center", textvariable = resLon)
    grd_vlat1 <- tkentry(fr_grd, width = 6, justify = "center", textvariable = minLat)
    grd_vlat2 <- tkentry(fr_grd, width = 6, justify = "center", textvariable = maxLat)
    grd_vlat3 <- tkentry(fr_grd, width = 6, justify = "center", textvariable = resLat)

    tkgrid(grd_lb1, row = 0, column = 1, sticky = "ew", rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
    tkgrid(grd_lb2, row = 0, column = 2, sticky = "ew", rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
    tkgrid(grd_lb3, row = 0, column = 3, sticky = "ew", rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)

    tkgrid(grd_llon, row = 1, column = 0, sticky = "ew", rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
    tkgrid(grd_vlon1, row = 1, column = 1, sticky = "ew", rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
    tkgrid(grd_vlon2, row = 1, column = 2, sticky = "ew", rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
    tkgrid(grd_vlon3, row = 1, column = 3, sticky = "ew", rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)

    tkgrid(grd_llat, row = 2, column = 0, sticky = "ew", rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
    tkgrid(grd_vlat1, row = 2, column = 1, sticky = "ew", rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
    tkgrid(grd_vlat2, row = 2, column = 2, sticky = "ew", rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
    tkgrid(grd_vlat3, row = 2, column = 3, sticky = "ew", rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)

    helpWidget(grd_vlon1, lang.dlg[['tooltip']][['1']], lang.dlg[['status']][['1']])
    helpWidget(grd_vlon2, lang.dlg[['tooltip']][['2']], lang.dlg[['status']][['2']])
    helpWidget(grd_vlon3, lang.dlg[['tooltip']][['3']], lang.dlg[['status']][['3']])
    helpWidget(grd_vlat1, lang.dlg[['tooltip']][['4']], lang.dlg[['status']][['4']])
    helpWidget(grd_vlat2, lang.dlg[['tooltip']][['5']], lang.dlg[['status']][['5']])
    helpWidget(grd_vlat3, lang.dlg[['tooltip']][['6']], lang.dlg[['status']][['6']])

    ################################

    tkgrid(fr_grd, row = 0, column = 0, sticky = 'nswe', rowspan = 1, columnspan = 2, padx = 1, pady = 1, ipadx = 1, ipady = 1)

    ################################

    bt.prm.OK <- ttkbutton(frGrd1, text = .cdtEnv$tcl$lang$global[['button']][['1']])
    bt.prm.CA <- ttkbutton(frGrd1, text = .cdtEnv$tcl$lang$global[['button']][['2']])

    tkconfigure(bt.prm.OK, command = function(){
        Parameters$minlon <<- as.numeric(trimws(tclvalue(minLon)))
        Parameters$maxlon <<- as.numeric(trimws(tclvalue(maxLon)))
        Parameters$reslon <<- as.numeric(trimws(tclvalue(resLon)))
        Parameters$minlat <<- as.numeric(trimws(tclvalue(minLat)))
        Parameters$maxlat <<- as.numeric(trimws(tclvalue(maxLat)))
        Parameters$reslat <<- as.numeric(trimws(tclvalue(resLat)))

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

    tkgrid(frGrd0, row = 0, column = 0, sticky = 'nswe', rowspan = 1, columnspan = 2, padx = 1, pady = 1, ipadx = 1, ipady = 1)
    tkgrid(frGrd1, row = 1, column = 1, sticky = 'se', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)

    ############################
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
