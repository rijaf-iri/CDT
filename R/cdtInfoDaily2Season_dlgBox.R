
getInfoDaily2Season <- function(parent.win, Parameters)
{
    # largeur0 <- 40

    xml.dlg <- file.path(.cdtDir$dirLocal, "languages", "cdtInfoDaily2Season_dlgBox.xml")
    lang.dlg <- cdtLanguageParse(xml.dlg, .cdtData$Config$lang.iso)

    ###################

    tt <- tktoplevel()
    tkgrab.set(tt)
    tkfocus(tt)

    frMRG0 <- tkframe(tt, relief = 'raised', borderwidth = 2)
    frMRG1 <- tkframe(tt)
    frMain <- tkframe(frMRG0, relief = 'groove', borderwidth = 2)

    ################################

    allYears <- tclVar(Parameters$all.years)

    chk.allYears <- tkcheckbutton(frMain, variable = allYears, text = lang.dlg[['checkbutton']][['1']], anchor = 'w', justify = 'left')

    tkbind(chk.allYears, "<Button-1>", function(){
        stateYear <- if(tclvalue(allYears) == '1') 'normal' else 'disabled'
        tkconfigure(en.startYear, state = stateYear)
        tkconfigure(en.endYear, state = stateYear)
    })

    ################################

    frameYear <- tkframe(frMain)

    startYear <- tclVar(Parameters$startYear)
    endYear <- tclVar(Parameters$endYear)

    stateYear <- if(Parameters$all.years) 'disabled' else 'normal'

    txt.startYear <- tklabel(frameYear, text = lang.dlg[['label']][['1']], anchor = 'e', justify = 'right')
    en.startYear <- tkentry(frameYear, textvariable = startYear, width = 6, state = stateYear)
    txt.endYear <- tklabel(frameYear, text = lang.dlg[['label']][['2']], anchor = 'e', justify = 'right')
    en.endYear <- tkentry(frameYear, textvariable = endYear, width = 6, state = stateYear)

    tkgrid(txt.startYear, row = 0, column = 0, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
    tkgrid(en.startYear, row = 0, column = 1, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
    tkgrid(txt.endYear, row = 0, column = 2, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
    tkgrid(en.endYear, row = 0, column = 3, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)

    ################################

    frameSeas <- tkframe(frMain)

    MOIS <- format(ISOdate(2014, 1:12, 1), "%b")
    mon1 <- as.numeric(str_trim(Parameters$startMon))
    startMon <- tclVar(MOIS[mon1])
    mon2 <- as.numeric(str_trim(Parameters$endMon))
    endMon <- tclVar(MOIS[mon2])

    txt.startMon <- tklabel(frameSeas, text = lang.dlg[['label']][['3']])
    cb.startMon <- ttkcombobox(frameSeas, values = MOIS, textvariable = startMon, width = 6)
    spin.startDay <- ttkspinbox(frameSeas, from = 1, to = 31, increment = 1, justify = 'center', width = 3)
    tkset(spin.startDay, Parameters$startDay)

    txt.endMon <- tklabel(frameSeas, text = lang.dlg[['label']][['4']])
    cb.endMon <- ttkcombobox(frameSeas, values = MOIS, textvariable = endMon, width = 6)
    spin.endDay <- ttkspinbox(frameSeas, from = 1, to = 31, increment = 1, justify = 'center', width = 3)
    tkset(spin.endDay, Parameters$endDay)

    tkgrid(txt.startMon, cb.startMon, spin.startDay, txt.endMon, cb.endMon, spin.endDay)
    tkgrid.configure(txt.endMon, sticky = "we", padx = 5)

    ################################

    frameMin <- tkframe(frMain)

    min.frac <- tclVar(Parameters$min.frac)

    txt.minfrac <- tklabel(frameMin, text = lang.dlg[['label']][['5']], anchor = 'e', justify = 'right')
    en.minfrac <- tkentry(frameMin, textvariable = min.frac, width = 7)

    tkgrid(txt.minfrac, en.minfrac)

    ################################

    tkgrid(chk.allYears, row = 0, column = 0, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 3, ipadx = 1, ipady = 1)
    tkgrid(frameYear, row = 1, column = 0, sticky = 'e', rowspan = 1, columnspan = 1, padx = 1, pady = 5, ipadx = 1, ipady = 1)
    tkgrid(frameSeas, row = 2, column = 0, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 5, ipadx = 1, ipady = 1)
    tkgrid(frameMin, row = 3, column = 0, sticky = 'e', rowspan = 1, columnspan = 1, padx = 1, pady = 3, ipadx = 1, ipady = 1)

    ################################

    tkgrid(frMain, sticky = 'we', padx = 5, pady = 5, ipadx = 1, ipady = 1)

    ################################

    bt.prm.OK <- ttkbutton(frMRG1, text = .cdtEnv$tcl$lang$global[['button']][['1']])
    bt.prm.CA <- ttkbutton(frMRG1, text = .cdtEnv$tcl$lang$global[['button']][['2']])

    tkconfigure(bt.prm.OK, command = function(){
        Parameters$all.years <<- switch(tclvalue(allYears), '0' = FALSE, '1' = TRUE)
        Parameters$startYear <<- as.numeric(str_trim(tclvalue(startYear)))
        Parameters$endYear <<- as.numeric(str_trim(tclvalue(endYear)))

        Parameters$startMon <<- which(MOIS %in% str_trim(tclvalue(startMon)))
        Parameters$endMon <<- which(MOIS %in% str_trim(tclvalue(endMon)))
        Parameters$startDay <<- as.numeric(str_trim(tclvalue(tkget(spin.startDay))))
        Parameters$endDay <<- as.numeric(str_trim(tclvalue(tkget(spin.endDay))))
        Parameters$min.frac <<- as.numeric(str_trim(tclvalue(min.frac)))

        daty1 <- try(as.Date(paste(2020, Parameters$startMon, Parameters$startDay, sep = '-')), silent = TRUE)
        if(inherits(daty1, "try-error") | is.na(daty1)){
            cdt.tkmessageBox(tt, message = lang.dlg[['message']][['1']], icon = "warning", type = "ok")
            tkwait.window(tt)
        }
 
        daty2 <- try(as.Date(paste(2020, Parameters$endMon, Parameters$endDay, sep = '-')), silent = TRUE)
        if(inherits(daty2, "try-error") | is.na(daty2)){
            cdt.tkmessageBox(tt, message = lang.dlg[['message']][['2']], icon = "warning", type = "ok")
            tkwait.window(tt)
        }

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
