
blankNcdf_Options <- function(parent.win){
    if(WindowsOS()){
        largeur0 <- 20
    }else{
        largeur0 <- 20
    }

    xml.dlg <- file.path(.cdtDir$dirLocal, "languages", "cdtBlanking_Options_dlgBox.xml")
    lang.dlg <- cdtLanguageParse(xml.dlg, .cdtData$Config$lang.iso)

    blank_opts <- blanking.options()

    #####################
    tt <- tktoplevel()
    tkgrab.set(tt)
    tkfocus(tt)

    #####################
    frDialog <- tkframe(tt, relief = 'raised', borderwidth = 2)
    frButt <- tkframe(tt)

    #####################

    frBlank <- tkframe(frDialog, relief = 'sunken', borderwidth = 2)

    cbBlankM <- lang.dlg[['combobox']][['1']]
    valBlankM <- c("default", "user")
    buffer_O <- tclVar()
    tclvalue(buffer_O) <- cbBlankM[valBlankM %in% blank_opts$bufferOption]

    buffer_W <- tclVar(blank_opts$bufferWidth)

    stateUser <- if(blank_opts$bufferOption == 'user') 'normal' else 'disabled'

    txt.bufferM <- tklabel(frBlank, text = lang.dlg[['label']][['1']], anchor = 'e', justify = 'right')
    cb.bufferM <- ttkcombobox(frBlank, values = cbBlankM, textvariable = buffer_O, width = largeur0, justify = 'center')
    txt.bufferW <- tklabel(frBlank, text = lang.dlg[['label']][['2']], anchor = 'e', justify = 'right')
    en.bufferW <- tkentry(frBlank, textvariable = buffer_W, width = 5, state = stateUser)

    tkgrid(txt.bufferM, row = 0, column = 0, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
    tkgrid(cb.bufferM, row = 0, column = 1, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
    tkgrid(txt.bufferW, row = 1, column = 0, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
    tkgrid(en.bufferW, row = 1, column = 1, sticky = 'w', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)

    helpWidget(cb.bufferM, lang.dlg[['tooltip']][['1']], lang.dlg[['status']][['1']])
    helpWidget(en.bufferW, lang.dlg[['tooltip']][['2']], lang.dlg[['status']][['2']])

    tkbind(cb.bufferM, "<<ComboboxSelected>>", function(){
        bufferOption <- valBlankM[cbBlankM %in% trimws(tclvalue(buffer_O))]
        stateUser <- if(bufferOption == 'user') 'normal' else 'disabled'
        tkconfigure(en.bufferW, state = stateUser)
    })

    #####################

    tkgrid(frBlank, row = 0, column = 0, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 10, ipadx = 1, ipady = 1)

    #####################
    bt.opt.OK <- ttkbutton(frButt, text = .cdtEnv$tcl$lang$global[['button']][['1']])
    bt.opt.CA <- ttkbutton(frButt, text = .cdtEnv$tcl$lang$global[['button']][['2']])

    tkconfigure(bt.opt.OK, command = function(){
        opt <- valBlankM[cbBlankM %in% trimws(tclvalue(buffer_O))]
        wd <- as.numeric(trimws(tclvalue(buffer_W)))
        blanking.options(bufferOption = opt, bufferWidth = wd)

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
