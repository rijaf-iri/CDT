
getInfoNetCDFScale <- function(parent.win, Parameters, ncDIR,
                               tstep = .cdtEnv$tcl$lang$global[['combobox']][['1']][5],
                               scale = FALSE)
{
    largeur1 <- if(WindowsOS()) 42 else 40

    xml.dlg <- file.path(.cdtDir$dirLocal, "languages", "cdtInfoNetcdfData_Scale_dlgBox.xml")
    lang.dlg <- cdtLanguageParse(xml.dlg, .cdtData$Config$lang.iso)

    ###################

    tt1 <- tktoplevel()
    tkgrab.set(tt1)
    tkfocus(tt1)

    frMRG0 <- tkframe(tt1, relief = 'raised', borderwidth = 2)
    frMRG1 <- tkframe(tt1)

    ###################

    frFF <- tkframe(frMRG0, relief = "sunken", borderwidth = 2)

    TSTEPVAL0 <- .cdtEnv$tcl$lang$global[['combobox']][['1']][4:6]

    if(scale){
        scale.tstep <- tclVar()

        TSTEPVAL1 <- c('pentad', 'dekadal', 'monthly')
        tclvalue(scale.tstep) <- TSTEPVAL0[TSTEPVAL1 %in% Parameters$tstep]

        TSTEPVAL <- TSTEPVAL0
        if(tstep == TSTEPVAL0[1]) TSTEPVAL <- TSTEPVAL0[-1]
        if(tstep == TSTEPVAL0[2]){
            TSTEPVAL <- c(TSTEPVAL0[3], '')
            tclvalue(scale.tstep) <- TSTEPVAL0[3]
        }
        cb.scale.tstep <- ttkcombobox(frFF, values = TSTEPVAL, textvariable = scale.tstep, width = largeur1)
    }

    inrfeff <- tclVar(Parameters$format)
    rfesample <- tclVar(Parameters$sample)

    txt.ncsample <- tklabel(frFF, text = lang.dlg[['label']][['1']], anchor = 'w', justify = 'left')
    cb.ncsample <- ttkcombobox(frFF, values = unlist(openFile_ttkcomboList()), textvariable = rfesample, width = largeur1)
    bt.ncsample <- tkbutton(frFF, text = "...")
    txt.inrfeff <- tklabel(frFF, text = lang.dlg[['label']][['2']], anchor = 'w', justify = 'left')
    en.inrfeff <- tkentry(frFF, textvariable = inrfeff, width = largeur1)

    ###################

    tkconfigure(bt.ncsample, command = function(){
        initialdir <- if(file.exists(ncDIR)) ncDIR else getwd()
        tcl('wm', 'attributes', tt1, topmost = FALSE)
        nc.opfiles <- getOpenNetcdf(tt1, initialdir = initialdir)
        tcl('wm', 'attributes', tt1, topmost = TRUE)
        if(!is.null(nc.opfiles)){
            update.OpenFiles('netcdf', nc.opfiles)
            tclvalue(rfesample) <- nc.opfiles[[1]]
            tkconfigure(cb.ncsample, values = unlist(openFile_ttkcomboList()))
        }
    })

    ###################

    if(scale) tkgrid(cb.scale.tstep, row = 0, column = 0, sticky = 'we', rowspan = 1, columnspan = 4, padx = 1, pady = 1, ipadx = 1, ipady = 1)

    tkgrid(txt.ncsample, row = 1, column = 0, sticky = 'we', rowspan = 1, columnspan = 5, padx = 1, pady = 1, ipadx = 1, ipady = 1)
    tkgrid(cb.ncsample, row = 2, column = 0, sticky = 'we', rowspan = 1, columnspan = 4, padx = 0, pady = 1, ipadx = 1, ipady = 1)
    tkgrid(bt.ncsample, row = 2, column = 4, sticky = 'w', rowspan = 1, columnspan = 1, padx = 0, pady = 1, ipadx = 1, ipady = 1)
    tkgrid(txt.inrfeff, row = 3, column = 0, sticky = 'we', rowspan = 1, columnspan = 5, padx = 1, pady = 1, ipadx = 1, ipady = 1)
    tkgrid(en.inrfeff, row = 4, column = 0, sticky = 'we', rowspan = 1, columnspan = 5, padx = 1, pady = 1, ipadx = 1, ipady = 1)

    ddk <- if(tstep %in% TSTEPVAL0[1:2]) 1 else '01'
    example <- do.call(sprintf, c(list(fmt = Parameters$format),
                as.list(c(1981, '01', ddk)[seq(length(gregexpr('%s', Parameters$format)[[1]]))])))

    helpWidget(cb.ncsample, lang.dlg[['tooltip']][['1']], lang.dlg[['status']][['1']])
    helpWidget(bt.ncsample, lang.dlg[['tooltip']][['2']], lang.dlg[['status']][['2']])
    helpWidget(en.inrfeff, paste(lang.dlg[['tooltip']][['3']], example),
                           paste(lang.dlg[['status']][['3']], example))

    ###################

    tkgrid(frFF, row = 0, column = 0, sticky = 'ew', padx = 1, pady = 1)

    ################################

    bt.prm.OK <- ttkbutton(frMRG1, text = .cdtEnv$tcl$lang$global[['button']][['1']])
    bt.prm.CA <- ttkbutton(frMRG1, text = .cdtEnv$tcl$lang$global[['button']][['2']])

    tkconfigure(bt.prm.OK, command = function(){
        if(str_trim(tclvalue(rfesample)) == ""){
            cdt.tkmessageBox(tt1, message = lang.dlg[['message']][['1']], icon = "warning", type = "ok")
            tkwait.window(tt1)
        }else{
            Parameters$format <<- str_trim(tclvalue(inrfeff))
            Parameters$sample <<- str_trim(tclvalue(rfesample))

            if(scale){
                Parameters$tstep <<- TSTEPVAL1[TSTEPVAL0 %in% str_trim(tclvalue(scale.tstep))]

                lenS <- length(gregexpr('%s', Parameters$format)[[1]])
                if((Parameters$tstep %in% c('pentad', 'dekadal') & lenS != 3) |
                    (Parameters$tstep == 'monthly' & lenS != 2))
                {
                    cdt.tkmessageBox(tt1, message = lang.dlg[['message']][['2']], icon = "warning", type = "ok")
                    tkwait.window(tt1)
                }
            }

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
