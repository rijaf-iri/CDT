
AggregateTS_ncdfData <- function(parent.win, ncDIR,
                                tstep = .cdtEnv$tcl$lang$global[['combobox']][['1']][3])
{
    listOpenFiles <- openFile_ttkcomboList()
    largeur1 <- if(WindowsOS()) 56 else 38

    xml.dlg <- file.path(.cdtDir$dirLocal, "languages", "cdtInfoNetcdfData_AggrTS_dlgBox.xml")
    lang.dlg <- cdtLanguageParse(xml.dlg, .cdtData$Config$lang.iso)

    ###################

    tt1 <- tktoplevel()
    tkgrab.set(tt1)
    tkfocus(tt1)

    frMRG0 <- tkframe(tt1, relief = 'raised', borderwidth = 2)
    frMRG1 <- tkframe(tt1)

    ###################

    frDate <- tkframe(frMRG0, relief = 'sunken', borderwidth = 2, pady = 5, padx = 5)

    TSTEPVAL0 <- .cdtEnv$tcl$lang$global[['combobox']][['1']][1:6]

    bt.daterange <- ttkbutton(frDate, text = lang.dlg[['button']][['1']], width = largeur1)

    tkconfigure(bt.daterange, command = function(){
        TSTEPVAL1 <- c('minute', 'hourly', 'daily', 'pentad', 'dekadal', 'monthly')
        tstep <- TSTEPVAL1[TSTEPVAL0 %in% tstep]
        .cdtData$GalParams[["Date.Range"]] <- getInfoDateRange(tt1, .cdtData$GalParams[["Date.Range"]], tstep)
    })

    tkgrid(bt.daterange, row = 0, column = 0, sticky = 'we', rowspan = 1, columnspan = 5, padx = 1, pady = 1, ipadx = 1, ipady = 1)

    helpWidget(bt.daterange, lang.dlg[['tooltip']][['1']], lang.dlg[['status']][['1']])

    ###################

    frFF <- tkframe(frMRG0, relief = "sunken", borderwidth = 2)

    inrfeff <- tclVar(.cdtData$GalParams$cdtnetcdf$format)
    rfesample <- tclVar(.cdtData$GalParams$cdtnetcdf$sample)

    txt.ncsample <- tklabel(frFF, text = lang.dlg[['label']][['4']], anchor = 'w', justify = 'left')
    cb.ncsample <- ttkcombobox(frFF, values = unlist(listOpenFiles), textvariable = rfesample, width = largeur1)
    bt.ncsample <- tkbutton(frFF, text = "...")
    txt.inrfeff <- tklabel(frFF, text = lang.dlg[['label']][['5']], anchor = 'w', justify = 'left')
    en.inrfeff <- tkentry(frFF, textvariable = inrfeff, width = largeur1)

    ###################

    tkconfigure(bt.ncsample, command = function(){
        initialdir <- if(file.exists(ncDIR)) ncDIR else getwd()
        tcl('wm', 'attributes', tt1, topmost = FALSE)
        nc.opfiles <- getOpenNetcdf(tt1, initialdir = initialdir)
        tcl('wm', 'attributes', tt1, topmost = TRUE)
        if(!is.null(nc.opfiles)){
            update.OpenFiles('netcdf', nc.opfiles)
            listOpenFiles[[length(listOpenFiles) + 1]] <<- nc.opfiles[[1]]
            tclvalue(rfesample) <- nc.opfiles[[1]]
            tkconfigure(cb.ncsample, values = unlist(listOpenFiles))
        }
    })

    ###################

    tkgrid(txt.ncsample, row = 0, column = 0, sticky = 'we', rowspan = 1, columnspan = 5, padx = 1, pady = 1, ipadx = 1, ipady = 1)
    tkgrid(cb.ncsample, row = 1, column = 0, sticky = 'we', rowspan = 1, columnspan = 4, padx = 0, pady = 1, ipadx = 1, ipady = 1)
    tkgrid(bt.ncsample, row = 1, column = 4, sticky = 'w', rowspan = 1, columnspan = 1, padx = 0, pady = 1, ipadx = 1, ipady = 1)
    tkgrid(txt.inrfeff, row = 2, column = 0, sticky = 'we', rowspan = 1, columnspan = 5, padx = 1, pady = 1, ipadx = 1, ipady = 1)
    tkgrid(en.inrfeff, row = 3, column = 0, sticky = 'we', rowspan = 1, columnspan = 5, padx = 1, pady = 1, ipadx = 1, ipady = 1)

    status.bar.display(cb.ncsample, lang.dlg[['status']][['2']])
    infobulle(bt.ncsample, lang.dlg[['tooltip']][['2']])
    helpWidget(en.inrfeff, lang.dlg[['tooltip']][['3']], lang.dlg[['status']][['3']])

    ###################

    tkgrid(frDate, row = 0, column = 0, sticky = '', padx = 1, pady = 3)
    tkgrid(frFF, row = 1, column = 0, sticky = 'we', padx = 1, pady = 1)

    ################################

    bt.prm.OK <- ttkbutton(frMRG1, text = .cdtEnv$tcl$lang$global[['button']][['1']])
    bt.prm.CA <- ttkbutton(frMRG1, text = .cdtEnv$tcl$lang$global[['button']][['2']])

    tkconfigure(bt.prm.OK, command = function(){
        if(trimws(tclvalue(rfesample)) == ""){
            cdt.tkmessageBox(tt1, message = lang.dlg[['message']][['1']], icon = "warning", type = "ok")
            tkwait.window(tt1)
        }else{
            .cdtData$GalParams$cdtnetcdf$format <- trimws(tclvalue(inrfeff))
            .cdtData$GalParams$cdtnetcdf$sample <- trimws(tclvalue(rfesample))

            lenS <- length(gregexpr('%s', .cdtData$GalParams$cdtnetcdf$format)[[1]])
            if( (tstep == TSTEPVAL0[1] & lenS != 5) |
                (tstep == TSTEPVAL0[2] & lenS != 4) |
                (tstep %in% TSTEPVAL0[3:5] & lenS != 3) |
                (tstep == TSTEPVAL0[6] & lenS != 2)
              )
            {
                cdt.tkmessageBox(tt1, message = lang.dlg[['message']][['2']], icon = "warning", type = "ok")
                tkwait.window(tt1)
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
}
