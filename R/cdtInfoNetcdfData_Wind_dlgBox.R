
getInfoNetCDFDataWind <- function(parent.win, Parameters, ncDIR)
{
    if(WindowsOS()){
        largeur1 <- 42
        largeur2 <- 10
    }else{
        largeur1 <- 40
        largeur2 <- 10
    }

    xml.dlg <- file.path(.cdtDir$dirLocal, "languages", "cdtInfoNetcdfData_Wind_dlgBox.xml")
    lang.dlg <- cdtLanguageParse(xml.dlg, .cdtData$Config$lang.iso)

    ###################

    tt1 <- tktoplevel()
    tkgrab.set(tt1)
    tkfocus(tt1)

    frMRG0 <- tkframe(tt1, relief = 'raised', borderwidth = 2)
    frMRG1 <- tkframe(tt1)

    ###################

    setncVarList <- function(fileopen){
        nc <- ncdf4::nc_open(fileopen)
        var.name <- sapply(nc$var, '[[', 'name')
        ncdf4::nc_close(nc)

        tkconfigure(cb.Ucomp, values = var.name)
        tclvalue(uWind) <- var.name[1]

        tkconfigure(cb.Vcomp, values = var.name)
        tclvalue(vWind) <- if(length(var.name) > 1) var.name[2] else ""


    }

    ###################

    frFF <- tkframe(frMRG0, relief = "sunken", borderwidth = 2)

    inrfeff <- tclVar(Parameters$format)
    ncSample <- tclVar(Parameters$sample)

    txt.ncsample <- tklabel(frFF, text = lang.dlg[['label']][['1']], anchor = 'w', justify = 'left')
    cb.ncsample <- ttkcombobox(frFF, values = unlist(openFile_ttkcomboList()), textvariable = ncSample, width = largeur1)
    bt.ncsample <- tkbutton(frFF, text = "...")
    txt.inrfeff <- tklabel(frFF, text = lang.dlg[['label']][['2']], anchor = 'w', justify = 'left')
    en.inrfeff <- tkentry(frFF, textvariable = inrfeff, width = largeur1)
    fr.UV <- tkframe(frFF)

    ###################

    tkconfigure(bt.ncsample, command = function(){
        initialdir <- if(file.exists(ncDIR)) ncDIR else getwd()
        tcl('wm', 'attributes', tt1, topmost = FALSE)
        nc.opfiles <- getOpenNetcdf(tt1, initialdir = initialdir)
        tcl('wm', 'attributes', tt1, topmost = TRUE)
        if(!is.null(nc.opfiles)){
            update.OpenFiles('netcdf', nc.opfiles)
            tclvalue(ncSample) <- nc.opfiles[[1]]
            tkconfigure(cb.ncsample, values = unlist(openFile_ttkcomboList()))

            setncVarList(nc.opfiles[[3]])
        }
    })

    tkbind(cb.ncsample, "<<ComboboxSelected>>", function(){
        jfile <- getIndex.AllOpenFiles(trimws(tclvalue(ncSample)))
        fileopen <- ""
        if(length(jfile) > 0){
            if(.cdtData$OpenFiles$Type[[jfile]] == "netcdf")
                fileopen <- .cdtData$OpenFiles$Data[[jfile]][[3]]
        }

        setncVarList(fileopen)
    })

    ###################

    tkgrid(txt.ncsample, row = 1, column = 0, sticky = 'we', rowspan = 1, columnspan = 5, padx = 1, pady = 1, ipadx = 1, ipady = 1)
    tkgrid(cb.ncsample, row = 2, column = 0, sticky = 'we', rowspan = 1, columnspan = 4, padx = 0, pady = 1, ipadx = 1, ipady = 1)
    tkgrid(bt.ncsample, row = 2, column = 4, sticky = 'w', rowspan = 1, columnspan = 1, padx = 0, pady = 1, ipadx = 1, ipady = 1)
    tkgrid(txt.inrfeff, row = 3, column = 0, sticky = 'we', rowspan = 1, columnspan = 5, padx = 1, pady = 1, ipadx = 1, ipady = 1)
    tkgrid(en.inrfeff, row = 4, column = 0, sticky = 'we', rowspan = 1, columnspan = 5, padx = 1, pady = 1, ipadx = 1, ipady = 1)
    tkgrid(fr.UV, row = 5, column = 0, sticky = 'we', rowspan = 1, columnspan = 5)

    helpWidget(cb.ncsample, lang.dlg[['tooltip']][['1']], lang.dlg[['status']][['1']])
    helpWidget(bt.ncsample, lang.dlg[['tooltip']][['2']], lang.dlg[['status']][['2']])
    helpWidget(en.inrfeff, lang.dlg[['tooltip']][['3']], lang.dlg[['status']][['3']])

    ###################

    uWind <- tclVar(Parameters$U)
    vWind <- tclVar(Parameters$V)

    txt.Ucomp <- tklabel(fr.UV, text = lang.dlg[['label']][['3']], anchor = 'e', justify = 'right')
    cb.Ucomp <- ttkcombobox(fr.UV, values = "", textvariable = uWind, width = largeur2)
    txt.Vcomp <- tklabel(fr.UV, text = lang.dlg[['label']][['4']], anchor = 'e', justify = 'right')
    cb.Vcomp <- ttkcombobox(fr.UV, values = "", textvariable = vWind, width = largeur2)

    tkgrid(txt.Ucomp, row = 0, column = 0, sticky = 'we', rowspan = 1, columnspan = 1, pady = 1, ipady = 1)
    tkgrid(cb.Ucomp, row = 0, column = 1, sticky = 'we', rowspan = 1, columnspan = 1, pady = 1, ipady = 1)
    tkgrid(txt.Vcomp, row = 0, column = 2, sticky = 'we', rowspan = 1, columnspan = 1, pady = 1, ipady = 1)
    tkgrid(cb.Vcomp, row = 0, column = 3, sticky = 'we', rowspan = 1, columnspan = 1, pady = 1, ipady = 1)

    helpWidget(cb.Ucomp, lang.dlg[['tooltip']][['4']], lang.dlg[['status']][['4']])
    helpWidget(cb.Vcomp, lang.dlg[['tooltip']][['5']], lang.dlg[['status']][['5']])

    if(!is.null(Parameters$varList)){
        if(length(Parameters$varList) >= 1){
            tkconfigure(cb.Ucomp, values = Parameters$varList)
            tkconfigure(cb.Vcomp, values = Parameters$varList)
        }
    }

    ###################

    tkgrid(frFF, row = 0, column = 0, sticky = 'ew', padx = 1, pady = 1)

    ################################

    bt.prm.OK <- ttkbutton(frMRG1, text = .cdtEnv$tcl$lang$global[['button']][['1']])
    bt.prm.CA <- ttkbutton(frMRG1, text = .cdtEnv$tcl$lang$global[['button']][['2']])

    tkconfigure(bt.prm.OK, command = function(){
        if(trimws(tclvalue(ncSample)) == ""){
            cdt.tkmessageBox(tt1, message = lang.dlg[['message']][['1']], icon = "warning", type = "ok")
            tkwait.window(tt1)
        }else{
            Parameters$format <<- trimws(tclvalue(inrfeff))
            Parameters$sample <<- trimws(tclvalue(ncSample))
            Parameters$U <<- trimws(tclvalue(uWind))
            Parameters$V <<- trimws(tclvalue(vWind))
            Parameters$varList <<- trimws(as.character(tkcget(cb.Ucomp, '-values')))

            tkgrab.release(tt1)
            tkdestroy(tt1)
            tkfocus(parent.win)
        }
    })

    tkconfigure(bt.prm.CA, command = function(){
        tkgrab.release(tt1)
        tkdestroy(tt1)
        tkfocus(parent.win)
    })

    tkgrid(bt.prm.OK, row = 0, column = 0, sticky = 'w', padx = 5, pady = 1, ipadx = 1, ipady = 1)
    tkgrid(bt.prm.CA, row = 0, column = 1, sticky = 'e', padx = 5, pady = 1, ipadx = 1, ipady = 1)

    ################################
    tkgrid(frMRG0, row = 0, column = 0, sticky = 'nswe', rowspan = 1, columnspan = 2, padx = 1, pady = 1, ipadx = 1, ipady = 1)
    tkgrid(frMRG1, row = 1, column = 1, sticky = 'se', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)

    tkwm.withdraw(tt1)
    tcl('update')
    tt.w <- as.integer(tkwinfo("reqwidth", tt1))
    tt.h <- as.integer(tkwinfo("reqheight", tt1))
    tt.x <- as.integer(.cdtEnv$tcl$data$width.scr*0.5 - tt.w*0.5)
    tt.y <- as.integer(.cdtEnv$tcl$data$height.scr*0.5 - tt.h*0.5)
    tkwm.geometry(tt1, paste0('+', tt.x, '+', tt.y))
    tkwm.transient(tt1)
    tkwm.title(tt1, lang.dlg[['title']])
    tkwm.deiconify(tt1)
    tcl('wm', 'attributes', tt1, topmost = TRUE)

    tkfocus(tt1)
    tkbind(tt1, "<Destroy>", function(){
        tkgrab.release(tt1)
        tkfocus(parent.win)
    })
    tkwait.window(tt1)
    return(Parameters)
}
