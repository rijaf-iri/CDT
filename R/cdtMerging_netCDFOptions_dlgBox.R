
mergingNCDF_Options <- function(parent.win, variable, wind = "speed"){
    if(WindowsOS()){
        largeur0 <- 15
        largeur1 <- 10
        largeur2 <- 55
    }else{
        largeur0 <- 15
        largeur1 <- 10
        largeur2 <- 55
    }

    xml.dlg <- file.path(.cdtDir$dirLocal, "languages", "cdtMerging_netCDFOptions_dlgBox.xml")
    lang.dlg <- cdtLanguageParse(xml.dlg, .cdtData$Config$lang.iso)

    mrgOpts <- merging.options()

    windUV <- FALSE
    if(variable == 'wind'){
        if(wind == 'uv-comp'){
            windUV <- TRUE
            ncdfOpts <- mrgOpts$netCDFDataDef[c('ugrd', 'vgrd')]
        }else{
            ncdfOpts <- mrgOpts$netCDFDataDef[['wspd']]
        }
    }else{
        ncdfOpts <- mrgOpts$netCDFDataDef[[variable]]
    }

    #####################
    tt <- tktoplevel()
    tkgrab.set(tt)
    tkfocus(tt)

    #####################
    frDialog <- tkframe(tt, relief = 'raised', borderwidth = 2)
    frButt <- tkframe(tt)

    #####################

    render_label_frame <- function(frname, vname, vunits, vlgname, vmin, vmax){
        frVar <- ttklabelframe(frDialog, text = frname, relief = 'groove')

        txt.nom <- tklabel(frVar, text = lang.dlg[['label']][['1']], anchor = 'e', justify = 'right') 
        en.nom <- tkentry(frVar, textvariable = vname, width = largeur0)
        txt.unit <- tklabel(frVar, text = lang.dlg[['label']][['2']], anchor = 'e', justify = 'right') 
        en.unit <- tkentry(frVar, textvariable = vunits, width = largeur1)
        txt.lname <- tklabel(frVar, text = lang.dlg[['label']][['3']], anchor = 'e', justify = 'right') 
        en.lname <- tkentry(frVar, textvariable = vlgname, width = largeur2)
        txt.vmin <- tklabel(frVar, text = lang.dlg[['label']][['4']], anchor = 'e', justify = 'right') 
        en.vmin <- tkentry(frVar, textvariable = vmin, width = 8, justify = 'right')
        txt.vmax <- tklabel(frVar, text = lang.dlg[['label']][['5']], anchor = 'e', justify = 'right') 
        en.vmax <- tkentry(frVar, textvariable = vmax, width = 8, justify = 'right')

        tkgrid(txt.nom, row = 0, column = 0, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
        tkgrid(en.nom, row = 0, column = 1, sticky = 'w', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
        tkgrid(txt.unit, row = 1, column = 0, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
        tkgrid(en.unit, row = 1, column = 1, sticky = 'w', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
        tkgrid(txt.lname, row = 2, column = 0, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
        tkgrid(en.lname, row = 2, column = 1, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
        tkgrid(txt.vmin, row = 3, column = 0, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
        tkgrid(en.vmin, row = 3, column = 1, sticky = 'w', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
        tkgrid(txt.vmax, row = 4, column = 0, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
        tkgrid(en.vmax, row = 4, column = 1, sticky = 'w', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)

        tkgrid(frVar, padx = 1, pady = 1, ipadx = 1, ipady = 1)
    }

    if(windUV){
        u_name <- tclVar(ncdfOpts$ugrd$name)
        u_units <- tclVar(ncdfOpts$ugrd$units)
        u_lname <- tclVar(ncdfOpts$ugrd$longname)
        u_min <- tclVar(ncdfOpts$ugrd$min)
        u_max <- tclVar(ncdfOpts$ugrd$max)
        render_label_frame(ncdfOpts$ugrd$standardname, u_name, u_units, u_lname, u_min, u_max)

        v_name <- tclVar(ncdfOpts$vgrd$name)
        v_units <- tclVar(ncdfOpts$vgrd$units)
        v_lname <- tclVar(ncdfOpts$vgrd$longname)
        v_min <- tclVar(ncdfOpts$vgrd$min)
        v_max <- tclVar(ncdfOpts$vgrd$max)
        render_label_frame(ncdfOpts$vgrd$standardname, v_name, v_units, v_lname, v_min, v_max)
    }else{
        var_name <- tclVar(ncdfOpts$name)
        var_units <- tclVar(ncdfOpts$units)
        var_lname <- tclVar(ncdfOpts$longname)
        var_min <- tclVar(ncdfOpts$min)
        var_max <- tclVar(ncdfOpts$max)
        render_label_frame(ncdfOpts$standardname, var_name, var_units, var_lname, var_min, var_max)
    }

    #####################
    bt.opt.OK <- ttkbutton(frButt, text = .cdtEnv$tcl$lang$global[['button']][['1']])
    bt.opt.CA <- ttkbutton(frButt, text = .cdtEnv$tcl$lang$global[['button']][['2']])

    tkconfigure(bt.opt.OK, command = function(){
        if(windUV){
            ncdfOpts$ugrd$name <- trimws(tclvalue(u_name))
            ncdfOpts$ugrd$units <- trimws(tclvalue(u_units))
            ncdfOpts$ugrd$longname <- trimws(tclvalue(u_lname))
            ncdfOpts$ugrd$min <- as.numeric(trimws(tclvalue(u_min)))
            ncdfOpts$ugrd$max <- as.numeric(trimws(tclvalue(u_max)))
            ncdfOpts$vgrd$name <- trimws(tclvalue(v_name))
            ncdfOpts$vgrd$units <- trimws(tclvalue(v_units))
            ncdfOpts$vgrd$longname <- trimws(tclvalue(v_lname))
            ncdfOpts$vgrd$min <- as.numeric(trimws(tclvalue(v_min)))
            ncdfOpts$vgrd$max <- as.numeric(trimws(tclvalue(v_max)))
            mrgOpts$netCDFDataDef[c('ugrd', 'vgrd')] <- ncdfOpts
        }else{
            ncdfOpts$name <- trimws(tclvalue(var_name))
            ncdfOpts$units <- trimws(tclvalue(var_units))
            ncdfOpts$longname <- trimws(tclvalue(var_lname))
            ncdfOpts$min <- as.numeric(trimws(tclvalue(var_min)))
            ncdfOpts$max <- as.numeric(trimws(tclvalue(var_max)))
            vname <- if(variable == 'wind') 'wspd' else variable
            mrgOpts$netCDFDataDef[[vname]] <- ncdfOpts
        }

        merging.options(netCDFDataDef = mrgOpts$netCDFDataDef)

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

    ##########################

    tkgrid(frDialog, row = 0, column = 0, sticky = 'nswe', rowspan = 1, columnspan = 2, padx = 1, pady = 1, ipadx = 1, ipady = 1)
    tkgrid(frButt, row = 1, column = 1, sticky = 'se', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)

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
}
