
getParams.QC.Elevation <- function(Parameters, parent.win = .cdtEnv$tcl$main$win)
{
    listOpenFiles <- openFile_ttkcomboList()
    largeur1 <- 42

    ###################

    xml.dlg <- file.path(.cdtDir$dirLocal, "languages", "cdtQC_ElevationData_dlgBox.xml")
    lang.dlg <- cdtLanguageParse(xml.dlg, .cdtData$Config$lang.iso)

    ###################

    tt <- tktoplevel()
    tkgrab.set(tt)
    tkfocus(tt)

    frMRG0 <- tkframe(tt, relief = 'raised', borderwidth = 2)
    frMRG1 <- tkframe(tt)

    ###################

    frameData <- tkframe(frMRG0)

    elevfrom <- tclVar(Parameters$dem)

    crdfrm1 <- tkradiobutton(frameData, variable = elevfrom, value = "1", text = lang.dlg[['checkbutton']][['1']], anchor = 'w', justify = 'left')
    crdfrm2 <- tkradiobutton(frameData, variable = elevfrom, value = "0", text = lang.dlg[['checkbutton']][['2']], anchor = 'w', justify = 'left')

    tkgrid(crdfrm1, row = 0, column = 0, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
    tkgrid(crdfrm2, row = 1, column = 0, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)

    tkbind(crdfrm1, "<Button-1>", function(){
        stateDEM <- 'normal'
        tkconfigure(cb.infile, state = stateDEM)
        tkconfigure(bt.infile, state = stateDEM)
    })

    tkbind(crdfrm2, "<Button-1>", function(){
        stateDEM <- 'disabled'
        tkconfigure(cb.infile, state = stateDEM)
        tkconfigure(bt.infile, state = stateDEM)
    })

    ###################

    frameNcdf <- tkframe(frMRG0, relief = 'sunken', borderwidth = 2)

    input.file <- tclVar(Parameters$file)

    stateDEM <- if(tclvalue(elevfrom) == '0') 'disabled' else 'normal'

    txt.infile <- tklabel(frameNcdf, text = lang.dlg[['label']][['1']], anchor = 'w', justify = 'left')
    cb.infile <- ttkcombobox(frameNcdf, values = unlist(listOpenFiles), textvariable = input.file, width = largeur1, state = stateDEM)
    bt.infile <- tkbutton(frameNcdf, text = "...", state = stateDEM)

    tkconfigure(bt.infile, command = function(){
        tcl('wm', 'attributes', tt, topmost = FALSE)
        nc.opfiles <- getOpenNetcdf(tt, initialdir = getwd())
        tcl('wm', 'attributes', tt, topmost = TRUE)
        if(!is.null(nc.opfiles)){
            update.OpenFiles('netcdf', nc.opfiles)
            listOpenFiles[[length(listOpenFiles) + 1]] <<- nc.opfiles[[1]]
            tclvalue(input.file) <- nc.opfiles[[1]]
            tkconfigure(cb.infile, values = unlist(listOpenFiles))
        }
    })

    tkgrid(txt.infile, row = 0, column = 0, sticky = 'we', rowspan = 1, columnspan = 5, padx = 1, pady = 1, ipadx = 1, ipady = 1)
    tkgrid(cb.infile, row = 1, column = 0, sticky = 'we', rowspan = 1, columnspan = 4, padx = 0, pady = 1, ipadx = 1, ipady = 1)
    tkgrid(bt.infile, row = 1, column = 4, sticky = 'w', rowspan = 1, columnspan = 1, padx = 0, pady = 1, ipadx = 1, ipady = 1)

    helpWidget(cb.infile, lang.dlg[['tooltip']][['1']], lang.dlg[['status']][['1']])
    helpWidget(bt.infile, lang.dlg[['tooltip']][['2']], lang.dlg[['status']][['2']])

    ###################

    tkgrid(frameData, row = 0, column = 0, sticky = 'we', padx = 1, ipadx = 1, pady = 3, ipady = 1)
    tkgrid(frameNcdf, row = 1, column = 0, sticky = 'we', padx = 1, ipadx = 1, pady = 3, ipady = 1)

    ################################

    bt.prm.OK <- ttkbutton(frMRG1, text = .cdtEnv$tcl$lang$global[['button']][['1']])
    bt.prm.CA <- ttkbutton(frMRG1, text = .cdtEnv$tcl$lang$global[['button']][['2']])

    tkconfigure(bt.prm.OK, command = function(){
        Parameters$file <<- str_trim(tclvalue(input.file))
        Parameters$dem <<- switch(tclvalue(elevfrom), '0' = FALSE, '1' = TRUE)

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
