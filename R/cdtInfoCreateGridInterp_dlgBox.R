
createGridInterpolation <- function(parent.win, Parameters, group = 1){
    listOpenFiles <- openFile_ttkcomboList()
    if(WindowsOS()){
        largeur0 <- 25
        largeur1 <- 44
    }else{
        largeur0 <- 26
        largeur1 <- 41
    }

    ################################

    xml.dlg <- file.path(.cdtDir$dirLocal, "languages", "cdtInfoCreateGridInterp_dlgBox.xml")
    lang.dlg <- cdtLanguageParse(xml.dlg, .cdtData$Config$lang.iso)

    ################################

    tt <- tktoplevel()
    tkgrab.set(tt)
    tkfocus(tt)

    frGrd0 <- tkframe(tt, relief = 'raised', borderwidth = 2)
    frGrd1 <- tkframe(tt)

    ################################

    cb.createGird <- lang.dlg[['combobox']][['1']]
    val.createGird <- c('data', 'ncdf', 'new')

    ################################

    frGrid <- tkframe(frGrd0, relief = 'sunken', borderwidth = 2)

    if(group == 1) igroup <- 1:3
    if(group == 2) igroup <- 2:3

    cb.createGird <- cb.createGird[igroup]
    val.createGird <- val.createGird[igroup]

    varCreateGrd <- tclVar()
    tclvalue(varCreateGrd) <- cb.createGird[val.createGird %in% Parameters$from]
    file.grdNC <- tclVar(Parameters$ncfile)

    stategrd <- if(Parameters$from == 'new') 'normal' else 'disabled'
    stategrNC <- if(Parameters$from == 'ncdf') 'normal' else 'disabled'

    txt.CreateGrd0 <- tklabel(frGrid, text = lang.dlg[['label']][['1']], anchor = 'w', justify = 'left')
    txt.CreateGrd1 <- tklabel(frGrid, text = lang.dlg[['label']][['2']], anchor = 'e', justify = 'right')
    cb.CreateGrd <- ttkcombobox(frGrid, values = cb.createGird, textvariable = varCreateGrd, width = largeur0)
    bt.CreateGrd <- ttkbutton(frGrid, text = lang.dlg[['button']][['1']], state = stategrd)
    frameGrdNC <- tkframe(frGrid)

    txt.grdNC <- tklabel(frameGrdNC, text = lang.dlg[['label']][['3']],  anchor = 'w', justify = 'left')
    cb.grdNC <- ttkcombobox(frameGrdNC, values = unlist(listOpenFiles), textvariable = file.grdNC, state = stategrNC, width = largeur1)
    bt.grdNC <- tkbutton(frameGrdNC, text = "...", state = stategrNC)

    ####

    tkgrid(txt.grdNC, row = 0, column = 0, sticky = 'we', rowspan = 1, columnspan = 2, padx = 1, pady = 1, ipadx = 1, ipady = 1)
    tkgrid(cb.grdNC, row = 1, column = 0, sticky = 'we', rowspan = 1, columnspan = 1, padx = 0, pady = 1, ipadx = 1, ipady = 1)
    tkgrid(bt.grdNC, row = 1, column = 1, sticky = 'we', rowspan = 1, columnspan = 1, padx = 0, pady = 1, ipadx = 1, ipady = 1)

    tkgrid(txt.CreateGrd0, row = 0, column = 0, sticky = 'we', rowspan = 1, columnspan = 6, padx = 1, pady = 1, ipadx = 1, ipady = 1)
    tkgrid(txt.CreateGrd1, row = 1, column = 0, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
    tkgrid(cb.CreateGrd, row = 1, column = 1, sticky = 'we', rowspan = 1, columnspan = 4, padx = 1, pady = 1, ipadx = 1, ipady = 1)
    tkgrid(bt.CreateGrd, row = 1, column = 5, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
    tkgrid(frameGrdNC, row = 2, column = 0, sticky = 'we', rowspan = 1, columnspan = 6, padx = 1, pady = 1, ipadx = 1, ipady = 1)

    helpWidget(bt.CreateGrd, lang.dlg[['tooltip']][['1']], lang.dlg[['status']][['1']])
    helpWidget(cb.CreateGrd, lang.dlg[['tooltip']][['2']], lang.dlg[['status']][['2']])
    helpWidget(cb.grdNC, lang.dlg[['tooltip']][['3']], lang.dlg[['status']][['3']])
    helpWidget(bt.grdNC, lang.dlg[['tooltip']][['4']], lang.dlg[['status']][['4']])

    ####

    tkconfigure(bt.CreateGrd, command = function(){
        tcl('wm', 'attributes', tt, topmost = FALSE)
        Parameters[["bbox"]] <<- getNewGridParams(tt, Parameters[["bbox"]])
        tcl('wm', 'attributes', tt, topmost = TRUE)
    })

    tkconfigure(bt.grdNC, command = function(){
        tcl('wm', 'attributes', tt, topmost = FALSE)
        nc.opfiles <- getOpenNetcdf(tt, initialdir = getwd())
        tcl('wm', 'attributes', tt, topmost = TRUE)
        if(!is.null(nc.opfiles)){
            update.OpenFiles('netcdf', nc.opfiles)
            listOpenFiles[[length(listOpenFiles) + 1]] <<- nc.opfiles[[1]]
            tclvalue(file.grdNC) <- nc.opfiles[[1]]
            tkconfigure(cb.grdNC, values = unlist(listOpenFiles))
        }
    })

    tkbind(cb.CreateGrd, "<<ComboboxSelected>>", function(){
        gridfrom <- val.createGird[cb.createGird %in% str_trim(tclvalue(varCreateGrd))]
        stategrd <- if(gridfrom == 'new') 'normal' else 'disabled'
        tkconfigure(bt.CreateGrd, state = stategrd)
        stategrNC <- if(gridfrom == 'ncdf') 'normal' else 'disabled'
        tkconfigure(cb.grdNC, state = stategrNC)
        tkconfigure(bt.grdNC, state = stategrNC)
    })

    ################################

    tkgrid(frGrid, row = 0, column = 0, sticky = 'we', padx = 1, pady = 1, ipadx = 1, ipady = 1)

    ################################

    bt.prm.OK <- ttkbutton(frGrd1, text = .cdtEnv$tcl$lang$global[['button']][['1']])
    bt.prm.CA <- ttkbutton(frGrd1, text = .cdtEnv$tcl$lang$global[['button']][['2']])

    tkconfigure(bt.prm.OK, command = function(){
        Parameters$from <<- val.createGird[cb.createGird %in% str_trim(tclvalue(varCreateGrd))]
        Parameters$ncfile <<- str_trim(tclvalue(file.grdNC))

        if((Parameters$from == "ncdf") &
           (Parameters$ncfile %in% c("", "NA")))
        {
            cdt.tkmessageBox(tt, message = lang.dlg[['message']][['1']], icon = "warning", type = "ok")
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

    tkgrid(frGrd0, row = 0, column = 0, sticky = 'nswe', rowspan = 1, columnspan = 2, padx = 1, pady = 1, ipadx = 1, ipady = 1)
    tkgrid(frGrd1, row = 1, column = 1, sticky = 'se', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)

    ############################
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
