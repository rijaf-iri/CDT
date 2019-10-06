
getInfoBasePeriod <- function(parent.win, Parameters)
{

    if(WindowsOS()){
        largeur0 <- 35
    }else{
        largeur0 <- 30
    }
    ###################

    # xml.dlg <- file.path(.cdtDir$dirLocal, "languages", "cdtInfoBasePeriod_dlgBox.xml")
    # lang.dlg <- cdtLanguageParse(xml.dlg, .cdtData$Config$lang.iso)

    ###################

    tt <- tktoplevel()
    tkgrab.set(tt)
    tkfocus(tt)

    frMRG0 <- tkframe(tt, relief = 'raised', borderwidth = 2)
    frMRG1 <- tkframe(tt)

    ###################

    frBaseP <- tkframe(frMRG0, relief = "sunken", borderwidth = 2)

    allYears <- tclVar(Parameters$all.years)
    startYear <- tclVar(Parameters$start.year)
    endYear <- tclVar(Parameters$end.year)
    minYear <- tclVar(Parameters$min.year)

    state0 <- if(Parameters$all.years) "disable" else "normal"

    chk.allYears <- tkcheckbutton(frBaseP, variable = allYears, text =  "Use all years from the input data", anchor = 'w', justify = 'left', width = largeur0)
    txt.startYear <- tklabel(frBaseP, text = "Start Year", anchor = 'e', justify = 'right')
    en.startYear <- tkentry(frBaseP, textvariable = startYear, width = 6, state = state0)
    txt.endYear <- tklabel(frBaseP, text = "End Year", anchor = 'e', justify = 'right')
    en.endYear <- tkentry(frBaseP, textvariable = endYear, width = 6, state = state0)
    txt.minYear <- tklabel(frBaseP, text = "Minimum number of years", anchor = 'e', justify = 'right')
    en.minYear <- tkentry(frBaseP, textvariable = minYear, width = 6)

    tkgrid(chk.allYears, row = 0, column = 0, sticky = 'we', rowspan = 1, columnspan = 8, padx = 1, pady = 1, ipadx = 1, ipady = 1)
    tkgrid(txt.startYear, row = 1, column = 0, sticky = 'e', rowspan = 1, columnspan = 3, padx = 1, pady = 1, ipadx = 1, ipady = 1)
    tkgrid(en.startYear, row = 1, column = 3, sticky = 'w', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
    tkgrid(txt.endYear, row = 1, column = 4, sticky = 'e', rowspan = 1, columnspan = 3, padx = 1, pady = 1, ipadx = 1, ipady = 1)
    tkgrid(en.endYear, row = 1, column = 7, sticky = 'w', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
    tkgrid(txt.minYear, row = 2, column = 0, sticky = 'e', rowspan = 1, columnspan = 7, padx = 1, pady = 1, ipadx = 1, ipady = 1)
    tkgrid(en.minYear, row = 2, column = 7, sticky = 'w', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)

    ###################

    helpWidget(chk.allYears, "Check this box to use all years from the input data", "Check this box to use all years from the input data")
    helpWidget(en.startYear, "Enter the start year of the period", "Enter the start year of the period")
    helpWidget(en.endYear, "Enter the end year of the period", "Enter the end year of the period")
    helpWidget(en.minYear, "Enter the minimum number of years without missing values required for computation", "Enter the minimum number of years without missing values required for computation")

    ###################

    tkbind(chk.allYears, "<Button-1>", function(){
        state0 <- if(tclvalue(allYears) == '0') 'disabled' else 'normal'
        tkconfigure(en.startYear, state = state0)
        tkconfigure(en.endYear, state = state0)
    })

    ###################

    # tkgrid(frBaseP, row = 0, column = 0, sticky = 'ew', padx = 1, pady = 1)
    tkgrid(frBaseP, row = 0, column = 0, sticky = 'ewns', padx = 3, pady = 1, ipady = 5)

    ################################

    bt.prm.OK <- ttkbutton(frMRG1, text = .cdtEnv$tcl$lang$global[['button']][['1']])
    bt.prm.CA <- ttkbutton(frMRG1, text = .cdtEnv$tcl$lang$global[['button']][['2']])

    tkconfigure(bt.prm.OK, command = function(){
        Parameters$all.years <<- switch(tclvalue(allYears), '0' = FALSE, '1' = TRUE)
        Parameters$start.year <<- as.numeric(str_trim(tclvalue(startYear)))
        Parameters$end.year <<- as.numeric(str_trim(tclvalue(endYear)))
        Parameters$min.year <<- as.numeric(str_trim(tclvalue(minYear)))

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
    tkwm.title(tt, "Base Period Settings")
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
