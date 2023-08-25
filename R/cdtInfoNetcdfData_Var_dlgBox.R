
getNetCDFvarInfo <- function(parent.win, Parameters){
    if(WindowsOS()){
        largeur1 <- 10
        largeur2 <- 40
    }else{
        largeur1 <- 10
        largeur2 <- 40
    }

    xml.dlg <- file.path(.cdtDir$dirLocal, "languages", "cdtInfoNetcdfData_Var_dlgBox.xml")
    lang.dlg <- cdtLanguageParse(xml.dlg, .cdtData$Config$lang.iso)

    ###################

    tt <- tktoplevel()
    tkgrab.set(tt)
    tkfocus(tt)

    frMRG0 <- tkframe(tt, relief = 'raised', borderwidth = 2)
    frMRG1 <- tkframe(tt)

    ############################################

    frInfo <- tkframe(frMRG0, relief = "sunken", borderwidth = 2)

    var.name <- tclVar(Parameters$name)
    var.unit <- tclVar(Parameters$units)
    var.prec <- tclVar(Parameters$prec)
    var.miss <- tclVar(Parameters$missval)
    var.long <- tclVar(Parameters$longname)

    txt.name <- tklabel(frInfo, text = lang.dlg[['label']][['1']], anchor = 'e', justify = 'right')
    en.name <- tkentry(frInfo, textvariable = var.name, width = largeur1)
    txt.unit <- tklabel(frInfo, text = lang.dlg[['label']][['2']], anchor = 'e', justify = 'right')
    en.unit <- tkentry(frInfo, textvariable = var.unit, width = largeur1)
    txt.prec <- tklabel(frInfo, text = lang.dlg[['label']][['3']], anchor = 'e', justify = 'right')
    en.prec <- tkentry(frInfo, textvariable = var.prec, width = largeur1)
    txt.miss <- tklabel(frInfo, text = lang.dlg[['label']][['4']], anchor = 'e', justify = 'right')
    en.miss <- tkentry(frInfo, textvariable = var.miss, width = largeur1)
    txt.long <- tklabel(frInfo, text = lang.dlg[['label']][['5']], anchor = 'e', justify = 'right')
    en.long <- tkentry(frInfo, textvariable = var.long, width = largeur2)


    tkgrid(txt.name, row = 0, column = 0, sticky = 'we', padx = 1, pady = 1, ipadx = 1, ipady = 1)
    tkgrid(en.name, row = 0, column = 1, sticky = 'we', padx = 1, pady = 1, ipadx = 1, ipady = 1)
    tkgrid(txt.unit, row = 0, column = 2, sticky = 'we', padx = 1, pady = 1, ipadx = 1, ipady = 1)
    tkgrid(en.unit, row = 0, column = 3, sticky = 'we', padx = 1, pady = 1, ipadx = 1, ipady = 1)
    tkgrid(txt.prec, row = 1, column = 0, sticky = 'we', padx = 1, pady = 1, ipadx = 1, ipady = 1)
    tkgrid(en.prec, row = 1, column = 1, sticky = 'we', padx = 1, pady = 1, ipadx = 1, ipady = 1)
    tkgrid(txt.miss, row = 1, column = 2, sticky = 'we', padx = 1, pady = 1, ipadx = 1, ipady = 1)
    tkgrid(en.miss, row = 1, column = 3, sticky = 'we', padx = 1, pady = 1, ipadx = 1, ipady = 1)
    tkgrid(txt.long, row = 2, column = 0, sticky = 'we', padx = 1, pady = 1, ipadx = 1, ipady = 1)
    tkgrid(en.long, row = 2, column = 1, columnspan = 3, sticky = 'we', padx = 1, pady = 1, ipadx = 1, ipady = 1)

    tkgrid(frInfo, row = 0, column = 0, sticky = '', padx = 1, pady = 5, ipadx = 1, ipady = 5)

    ############################################

    bt.prm.OK <- ttkbutton(frMRG1, text = .cdtEnv$tcl$lang$global[['button']][['1']])
    bt.prm.CA <- ttkbutton(frMRG1, text = .cdtEnv$tcl$lang$global[['button']][['2']])

    ####
    tkconfigure(bt.prm.OK, command = function(){
        Parameters$name <<- trimws(tclvalue(var.name))
        Parameters$units <<- trimws(tclvalue(var.unit))
        Parameters$prec <<- trimws(tclvalue(var.prec))
        Parameters$missval <<- as.numeric(trimws(tclvalue(var.miss)))
        Parameters$longname <<- trimws(tclvalue(var.long))

        tkgrab.release(tt)
        tkdestroy(tt)
        tkfocus(parent.win)
    })

    tkconfigure(bt.prm.CA, command = function(){
        tkgrab.release(tt)
        tkdestroy(tt)
        tkfocus(parent.win)
    })

    ####
    tkgrid(bt.prm.OK, row = 0, column = 0, sticky = 'w', padx = 5, pady = 1, ipadx = 1, ipady = 1)
    tkgrid(bt.prm.CA, row = 0, column = 1, sticky = 'e', padx = 5, pady = 1, ipadx = 1, ipady = 1)

    ############################################
    
    tkgrid(frMRG0, row = 0, column = 0, sticky = 'nswe', rowspan = 1, columnspan = 2, padx = 1, pady = 1, ipadx = 1, ipady = 1)
    tkgrid(frMRG1, row = 1, column = 1, sticky = 'se', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)

    ###########################
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
    tkbind(tt, "<Destroy>", function() {
        tkgrab.release(tt)
        tkfocus(parent.win)
    })
    tkwait.window(tt)

    return(Parameters)
}
