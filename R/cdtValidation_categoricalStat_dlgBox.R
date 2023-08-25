
getInfo_categoricalValid <- function(parent.win, Parameters){
    xml.dlg <- file.path(.cdtDir$dirLocal, "languages", "cdtValidation_categoricalStat_dlgBox.xml")
    lang.dlg <- cdtLanguageParse(xml.dlg, .cdtData$Config$lang.iso)

    ###################

    tt <- tktoplevel()
    tkgrab.set(tt)
    tkfocus(tt)

    frMRG0 <- tkframe(tt, relief = 'raised', borderwidth = 2)
    frMRG1 <- tkframe(tt)

    ################################

    frameConTab <- ttklabelframe(frMRG0, text = lang.dlg[['label']][['1']], relief = 'groove')

    oprList <- c(">=", ">", "<=", "<")
    dicho.opr <- tclVar(Parameters$fun)
    dicho.thres <- tclVar(Parameters$thres)

    txt.opr <- tklabel(frameConTab, text = lang.dlg[['label']][['2']], anchor = "e", justify = "right")
    cb.opr <- ttkcombobox(frameConTab, values = oprList, textvariable = dicho.opr, width = 3)
    txt.thres <- tklabel(frameConTab, text = lang.dlg[['label']][['3']], anchor = "e", justify = "right")
    en.thres <- tkentry(frameConTab, textvariable = dicho.thres, width = 4)

    txt.sep <- tklabel(frameConTab, text = "", width = 10)

    tkgrid(txt.opr, row = 0, column = 0, sticky = 'e', padx = 1, pady = 3, ipadx = 1, ipady = 1)
    tkgrid(cb.opr, row = 0, column = 1, sticky = 'we', padx = 1, pady = 3, ipadx = 1, ipady = 1)
    tkgrid(txt.sep, row = 0, column = 2, sticky = 'we', padx = 1, pady = 3, ipadx = 1, ipady = 1)
    tkgrid(txt.thres, row = 0, column = 3, sticky = 'e', padx = 1, pady = 3, ipadx = 1, ipady = 1)
    tkgrid(en.thres, row = 0, column = 4, sticky = 'we', padx = 1, pady = 3, ipadx = 1, ipady = 1)

    helpWidget(cb.opr, lang.dlg[['tooltip']][['1']], lang.dlg[['status']][['1']])
    helpWidget(en.thres, lang.dlg[['tooltip']][['2']], lang.dlg[['status']][['2']])

    ###################

    tkgrid(frameConTab, row = 0, column = 0, sticky = 'we', padx = 3, pady = 10, ipadx = 1, ipady = 1)

    ################################

    bt.prm.OK <- ttkbutton(frMRG1, text = .cdtEnv$tcl$lang$global[['button']][['1']])
    bt.prm.CA <- ttkbutton(frMRG1, text = .cdtEnv$tcl$lang$global[['button']][['2']])

    tkconfigure(bt.prm.OK, command = function(){
        Parameters$fun <<- trimws(tclvalue(dicho.opr))
        Parameters$thres <<- as.numeric(trimws(tclvalue(dicho.thres)))

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
