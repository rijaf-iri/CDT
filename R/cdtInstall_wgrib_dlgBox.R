
build_wgrib_info <- function(){
    if(WindowsOS()){
        largeur0 <- 25
        largeur1 <- 42
    }else{
        largeur0 <- 21
        largeur1 <- 36
    }

    xml.dlg <- file.path(.cdtDir$dirLocal, "languages", "cdtInstall_wgrib_dlgBox.xml")
    lang.dlg <- cdtLanguageParse(xml.dlg, .cdtData$Config$lang.iso)

    #####
    tt <- tktoplevel()
    tkgrab.set(tt)
    tkfocus(tt)

    frGrd0 <- tkframe(tt, relief = 'raised', borderwidth = 2)
    frGrd1 <- tkframe(tt)

    #########
    frA1 <- tkframe(frGrd0, relief = 'sunken', bd = 2)

    wgribs <- tclVar(.cdtData$GalParams$wgrib)
    cb_wgribs <- c("wgrib2", "wgrib")

    txt.wgrib <- tklabel(frA1, text = lang.dlg[['label']][['1']], anchor = 'e', justify = 'right')
    cb.wgrib <- ttkcombobox(frA1, values = cb_wgribs, textvariable = wgribs, justify = 'center', width = largeur0)

    tkgrid(txt.wgrib, row = 0, column = 0, sticky = 'we', padx = 1, pady = 1, ipadx = 1, ipady = 1)
    tkgrid(cb.wgrib, row = 0, column = 1, sticky = 'we', padx = 1, pady = 1, ipadx = 1, ipady = 1)

    tkbind(cb.wgrib, "<<ComboboxSelected>>", function(){
        stateC <- if(trimws(tclvalue(wgribs)) == "wgrib2") "normal" else "disabled"
        tkconfigure(en.fc, state = stateC)
        tkconfigure(en.make, state = stateC)
    })

    #####
    frA2 <- tkframe(frGrd0, relief = 'sunken', bd = 2)

    ccompiler <- tclVar(.cdtData$GalParams$cc)
    fcompiler <- tclVar(.cdtData$GalParams$fc)
    makecmd <- tclVar(.cdtData$GalParams$make)

    stateC <- if(.cdtData$GalParams$wgrib == "wgrib2") "normal" else "disabled"

    txt.cc <- tklabel(frA2, text = lang.dlg[['label']][['2']], anchor = 'w', justify = 'left')
    en.cc <- tkentry(frA2, textvariable = ccompiler, justify = "left", width = largeur1)
    txt.fc <- tklabel(frA2, text = lang.dlg[['label']][['3']], anchor = 'w', justify = 'left', state = stateC)
    en.fc <- tkentry(frA2, textvariable = fcompiler, justify = "left", width = largeur1)
    txt.make <- tklabel(frA2, text = lang.dlg[['label']][['4']], anchor = 'w', justify = 'left', state = stateC)
    en.make <- tkentry(frA2, textvariable = makecmd, justify = "left", width = largeur1)

    tkgrid(txt.cc, row = 0, column = 0, sticky = 'we', padx = 1, pady = 1, ipadx = 1, ipady = 1)
    tkgrid(en.cc, row = 1, column = 0, sticky = 'we', padx = 1, pady = 1, ipadx = 1, ipady = 1)
    tkgrid(txt.fc, row = 2, column = 0, sticky = 'we', padx = 1, pady = 1, ipadx = 1, ipady = 1)
    tkgrid(en.fc, row = 3, column = 0, sticky = 'we', padx = 1, pady = 1, ipadx = 1, ipady = 1)
    tkgrid(txt.make, row = 4, column = 0, sticky = 'we', padx = 1, pady = 1, ipadx = 1, ipady = 1)
    tkgrid(en.make, row = 5, column = 0, sticky = 'we', padx = 1, pady = 1, ipadx = 1, ipady = 1)

    helpWidget(en.cc, lang.dlg[['tooltip']][['1']], lang.dlg[['status']][['1']])
    helpWidget(en.fc, lang.dlg[['tooltip']][['2']], lang.dlg[['status']][['2']])
    helpWidget(en.make, lang.dlg[['tooltip']][['3']], lang.dlg[['status']][['3']])

    ######
    tkgrid(frA1, row = 0, column = 0, sticky = 'we', rowspan = 1, columnspan = 1, padx = 5, pady = 5, ipadx = 1, ipady = 5)
    tkgrid(frA2, row = 1, column = 0, sticky = 'we', rowspan = 1, columnspan = 1, padx = 5, pady = 5, ipadx = 1, ipady = 1)

    ######
    btOK <- ttkbutton(frGrd1, text = lang.dlg[['button']][['1']])
    btCA <- ttkbutton(frGrd1, text = lang.dlg[['button']][['2']])

    tkconfigure(btOK, command = function(){
        tkgrab.release(tt)
        tkdestroy(tt)
        tkfocus(.cdtEnv$tcl$main$win)
        tcl('update')

        .cdtData$GalParams$wgrib <- trimws(tclvalue(wgribs))
        .cdtData$GalParams$cc <- trimws(tclvalue(ccompiler))
        .cdtData$GalParams$fc <- trimws(tclvalue(fcompiler))
        .cdtData$GalParams$make <- trimws(tclvalue(makecmd))
        .cdtData$GalParams$message <- lang.dlg[['message']]

        build_wgrib_gui()
    })

    tkconfigure(btCA, command = function(){
        tkgrab.release(tt)
        tkdestroy(tt)
        tkfocus(.cdtEnv$tcl$main$win)
    })

    tkgrid(btCA, row = 0, column = 0, sticky = 'w', padx = 5, pady = 1, ipadx = 1, ipady = 1)
    tkgrid(btOK, row = 0, column = 1, sticky = 'e', padx = 5, pady = 1, ipadx = 1, ipady = 1)

    #####
    tkgrid(frGrd0, row = 0, column = 0, sticky = 'nswe', rowspan = 1, columnspan = 2, padx = 1, pady = 1, ipadx = 1, ipady = 1)
    tkgrid(frGrd1, row = 1, column = 1, sticky = 'se', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)

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
        tkfocus(.cdtEnv$tcl$main$win)
    })
    tkwait.window(tt)
}
