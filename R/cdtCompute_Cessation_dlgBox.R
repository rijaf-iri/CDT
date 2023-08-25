
computeWB_Cessation <- function(Parameters, data.type){
    if(WindowsOS()){
        largeur0 <- 7
        largeur1 <- 2
    }else{
        largeur0 <- 7
        largeur1 <- 2
    }

    MOIS <- format(ISOdate(2014, 1:12, 1), "%B")

    xml.dlg <- file.path(.cdtDir$dirLocal, "languages", "cdtCompute_Cessation_dlgBox.xml")
    lang.dlg <- cdtLanguageParse(xml.dlg, .cdtData$Config$lang.iso)

    ############################################

    tt <- tktoplevel()
    tkgrab.set(tt)
    tkfocus(tt)

    frMRG0 <- tkframe(tt, relief = 'raised', borderwidth = 2)
    frMRG1 <- tkframe(tt)

    ############################################

    frWBalance <- tkframe(frMRG0, relief = 'sunken', borderwidth = 2)

    ###############

    frameDate <- tkframe(frWBalance)

    imon <- as.numeric(trimws(Parameters$hdate$start.month))
    start.month <- tclVar(MOIS[imon])
    start.day <- tclVar(Parameters$hdate$start.day)
    separate.year <- tclVar(Parameters$hdate$separate.year)

    chk.sep.year <- tkcheckbutton(frameDate, variable = separate.year, text = lang.dlg[['checkbutton']][['1']], anchor = 'w', justify = 'left')
    txt.1stdate0 <- tklabel(frameDate, text = lang.dlg[['label']][['1']], anchor = 'e', justify = 'right')
    txt.1stdate1 <- tklabel(frameDate, text = lang.dlg[['label']][['2']], anchor = 'e', justify = 'right')
    cb.1stdate1 <- ttkcombobox(frameDate, values = MOIS, textvariable = start.month, width = 11)
    txt.1stdate2 <- tklabel(frameDate, text = lang.dlg[['label']][['3']], anchor = 'e', justify = 'right')
    cb.1stdate2 <- ttkcombobox(frameDate, values = 1:31, textvariable = start.day, width = 3)

    txt.1stdatesep <- tklabel(frameDate, text = "", width = largeur0)

    ###############

    tkgrid(chk.sep.year, row = 0, column = 0, sticky = 'we', rowspan = 1, columnspan = 6, padx = 1, pady = 1, ipadx = 1, ipady = 1)

    tkgrid(txt.1stdatesep, row = 1, column = 0, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
    tkgrid(txt.1stdate0, row = 1, column = 1, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
    tkgrid(txt.1stdate1, row = 1, column = 2, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
    tkgrid(cb.1stdate1, row = 1, column = 3, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
    tkgrid(txt.1stdate2, row = 1, column = 4, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
    tkgrid(cb.1stdate2, row = 1, column = 5, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)

    ###############

    frameParWB <- tkframe(frWBalance, relief = 'groove', borderwidth = 2)

    start.wb <- tclVar(Parameters$wb$wb1)
    capacity.max <- tclVar(Parameters$swhc$cap.max)
    use.multi.wb <- tclVar(Parameters$wb$multi)
    use.multi.swhc <- tclVar(Parameters$swhc$multi)

    stateMWB <- if(Parameters$wb$multi) "normal" else "disabled"
    stateMSWHC <- if(Parameters$swhc$multi) "normal" else "disabled"

    txt.wb.1stday <- tklabel(frameParWB, text = lang.dlg[['label']][['4']], anchor = 'w', justify = 'left')
    en.wb.1stday <- tkentry(frameParWB, textvariable = start.wb, width = 4)
    chk.wb.1stday <- tkcheckbutton(frameParWB, variable = use.multi.wb, text = lang.dlg[['checkbutton']][['2']], anchor = 'w', justify = 'left')
    bt.wb.1stday <- ttkbutton(frameParWB, text = lang.dlg[['button']][['1']], state = stateMWB)
    txt.wb.swhc <- tklabel(frameParWB, text = lang.dlg[['label']][['5']], anchor = 'w', justify = 'left')
    en.wb.swhc <- tkentry(frameParWB, textvariable = capacity.max, width = 4)
    chk.wb.swhc <- tkcheckbutton(frameParWB, variable = use.multi.swhc, text = lang.dlg[['checkbutton']][['3']], anchor = 'w', justify = 'left')
    bt.wb.swhc <- ttkbutton(frameParWB, text = lang.dlg[['button']][['1']], state = stateMSWHC)

    txt.1stdaysep <- tklabel(frameParWB, text = "", width = largeur1)

    ###############

    tkgrid(txt.wb.1stday, row = 0, column = 0, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
    tkgrid(en.wb.1stday, row = 0, column = 1, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
    tkgrid(txt.1stdaysep, row = 0, column = 2, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
    tkgrid(chk.wb.1stday, row = 0, column = 3, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
    tkgrid(bt.wb.1stday, row = 0, column = 4, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)

    tkgrid(txt.wb.swhc, row = 1, column = 0, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
    tkgrid(en.wb.swhc, row = 1, column = 1, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
    tkgrid(chk.wb.swhc, row = 1, column = 3, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
    tkgrid(bt.wb.swhc, row = 1, column = 4, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)

    ###############
    tkconfigure(bt.wb.1stday, command = function(){
        tcl('wm', 'attributes', tt, topmost = FALSE)
        Parameters$wb[["file"]] <<- computeWB_get.WB.SWHC(tt, Parameters$wb[["file"]], data.type, "WB")
        tcl('wm', 'attributes', tt, topmost = TRUE)
    })

    tkconfigure(bt.wb.swhc, command = function(){
        tcl('wm', 'attributes', tt, topmost = FALSE)
        Parameters$swhc[["file"]] <<- computeWB_get.WB.SWHC(tt, Parameters$swhc[["file"]], data.type, "SWHC")
        tcl('wm', 'attributes', tt, topmost = TRUE)
    })

    ###############

    tkbind(chk.wb.1stday, "<Button-1>", function(){
        stateMWB <- if(tclvalue(use.multi.wb) == '0') 'normal' else 'disabled'
        tkconfigure(bt.wb.1stday, state = stateMWB)
    })

    tkbind(chk.wb.swhc, "<Button-1>", function(){
        stateMSWHC <- if(tclvalue(use.multi.swhc) == '0') 'normal' else 'disabled'
        tkconfigure(bt.wb.swhc, state = stateMSWHC)
    })

    ############################################

    tkgrid(frameDate, row = 0, column = 0, sticky = 'we', pady = 3)
    tkgrid(frameParWB, row = 1, column = 0, sticky = 'we')

    ############################################
    tkgrid(frWBalance, row = 0, column = 0, sticky = 'we', padx = 1, pady = 3, ipadx = 1, ipady = 1)

    ############################################

    bt.prm.OK <- ttkbutton(frMRG1, text = .cdtEnv$tcl$lang$global[['button']][['1']])
    bt.prm.CA <- ttkbutton(frMRG1, text = .cdtEnv$tcl$lang$global[['button']][['2']])

    tkconfigure(bt.prm.OK, command = function(){
            Parameters$hdate$start.month <<- which(MOIS %in% trimws(tclvalue(start.month)))
            Parameters$hdate$start.day <<- as.numeric(trimws(tclvalue(start.day)))
            Parameters$hdate$separate.year <<- switch(tclvalue(separate.year), '0' = FALSE, '1' = TRUE)

            Parameters$wb$multi <<- switch(tclvalue(use.multi.wb), '0' = FALSE, '1' = TRUE)
            Parameters$swhc$multi <<- switch(tclvalue(use.multi.swhc), '0' = FALSE, '1' = TRUE)

            Parameters$wb$wb1 <<- as.numeric(trimws(tclvalue(start.wb)))
            Parameters$swhc$cap.max <<- as.numeric(trimws(tclvalue(capacity.max)))

            tkgrab.release(tt)
            tkdestroy(tt)
            tkfocus(.cdtEnv$tcl$main$win)
    })

    tkconfigure(bt.prm.CA, command = function(){
        tkgrab.release(tt)
        tkdestroy(tt)
        tkfocus(.cdtEnv$tcl$main$win)
    })

    tkgrid(bt.prm.OK, row = 0, column = 0, sticky = 'w', padx = 5, pady = 1, ipadx = 1, ipady = 1)
    tkgrid(bt.prm.CA, row = 0, column = 1, sticky = 'e', padx = 5, pady = 1, ipadx = 1, ipady = 1)

    ############################################

    tkgrid(frMRG0, row = 0, column = 0, sticky = 'nswe', rowspan = 1, columnspan = 2, padx = 1, pady = 1, ipadx = 1, ipady = 1)
    tkgrid(frMRG1, row = 1, column = 1, sticky = 'se', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)

    ############################3
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
    return(Parameters)
}
