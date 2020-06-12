split_3d.netcdf_getParams <- function(){
    if(WindowsOS()){
        largeur0 <- 26
        largeur1 <- 44
        largeur3 <- 24
        largeur4 <- 43
    }else{
        largeur0 <- 26
        largeur1 <- 44
        largeur3 <- 24
        largeur4 <- 41
    }

    xml.dlg <- file.path(.cdtDir$dirLocal, "languages", "cdtSplit3DNetCDF_dlgBox.xml")
    lang.dlg <- cdtLanguageParse(xml.dlg, .cdtData$Config$lang.iso)

    ############################################

    tt <- tktoplevel()
    tkgrab.set(tt)
    tkfocus(tt)

    frMRG0 <- tkframe(tt, relief = 'raised', borderwidth = 2)
    frMRG1 <- tkframe(tt)
    frInner <- tkframe(frMRG0, relief = "groove", borderwidth = 2)

    ############################################

    frNbFile <- tkframe(frInner, relief = 'sunken', borderwidth = 2)

    nb.file <- tclVar()
    cb.nbfileVAL <- lang.dlg[['combobox']][['1']]
    nbfileVAL <- c('one', 'several')
    tclvalue(nb.file) <- cb.nbfileVAL[nbfileVAL %in% .cdtData$GalParams$nbfile]

    cb.nbfile <- ttkcombobox(frNbFile, values = cb.nbfileVAL, textvariable = nb.file, width = largeur0)

    tkgrid(cb.nbfile, row = 0, column = 0, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 3, ipadx = 1, ipady = 1)

    ###########
    tkbind(cb.nbfile, "<<ComboboxSelected>>", function(){
        nbfile <- str_trim(tclvalue(nb.file))
        if(nbfile == cb.nbfileVAL[1]){
            tclvalue(txt.NcFL.var) <- lang.dlg[['label']][['1']]
            stateFF <- 'disabled'
        }else{
            tclvalue(txt.NcFL.var) <- lang.dlg[['label']][['2']]
            stateFF <- 'normal'
        }
        tkconfigure(en.NcFF, state = stateFF)
    })

    ############################################

    frNCFile <- tkframe(frInner, relief = 'sunken', borderwidth = 2)

    nc.file <- tclVar(.cdtData$GalParams$ncdf$file)
    nc.format <- tclVar(.cdtData$GalParams$ncdf$format)

    if(.cdtData$GalParams$nbfile == "one"){
        txtNCFile <- lang.dlg[['label']][['1']]
        stateFF <- 'disabled'
    }else{
        txtNCFile <- lang.dlg[['label']][['2']]
        stateFF <- 'normal'
    }

    txt.NcFL.var <- tclVar(txtNCFile)

    txt.NcFL <- tklabel(frNCFile, text = tclvalue(txt.NcFL.var), textvariable = txt.NcFL.var, anchor = 'w', justify = 'left')
    bt.NcFL <- ttkbutton(frNCFile, text = lang.dlg[['button']][['1']])
    en.NcFL <- tkentry(frNCFile, textvariable = nc.file, width = largeur1)
    txt.NcFF <- tklabel(frNCFile, text = lang.dlg[['label']][['3']], anchor = 'w', justify = 'left')
    en.NcFF <- tkentry(frNCFile, textvariable = nc.format, width = largeur3, state = stateFF)

    nc.params <- .cdtData$GalParams$ncpars
    tkconfigure(bt.NcFL, command = function(){
        tcl('wm', 'attributes', tt, topmost = FALSE)
        ncfile <- tclvalue(tkgetOpenFile(initialdir = getwd(), filetypes = .cdtEnv$tcl$data$filetypes3))
        tcl('wm', 'attributes', tt, topmost = TRUE)
        if(ncfile == "" | is.na(ncfile)) return(NULL)
        if(!file.exists(ncfile)){
            Insert.Messages.Out(paste(ncfile, lang.dlg[['message']][['1']]), format = TRUE)
           return(NULL) 
        }

        nc.params <<- split_3d.netcdf_selectVariable(tt, ncfile)
        tclvalue(nc.file) <- if(!is.null(nc.params)) ncfile else ""
    })

    tkgrid(txt.NcFL, row = 0, column = 0, sticky = 'we', rowspan = 1, columnspan = 5, padx = 1, pady = 1, ipadx = 1, ipady = 1)
    tkgrid(bt.NcFL, row = 0, column = 5, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
    tkgrid(en.NcFL, row = 1, column = 0, sticky = 'we', rowspan = 1, columnspan = 6, padx = 1, pady = 1, ipadx = 1, ipady = 1)
    tkgrid(txt.NcFF, row = 2, column = 0, sticky = 'we', rowspan = 1, columnspan = 4, padx = 1, pady = 1, ipadx = 1, ipady = 1)
    tkgrid(en.NcFF, row = 2, column = 4, sticky = 'we', rowspan = 1, columnspan = 2, padx = 1, pady = 1, ipadx = 1, ipady = 1)

    ############################################

    frSave <- tkframe(frInner, relief = 'sunken', borderwidth = 2)

    dir.save <- tclVar(.cdtData$GalParams$output)

    txt.dirsave <- tklabel(frSave, text = lang.dlg[['label']][['4']], anchor = 'w', justify = 'left')
    en.dirsave <- tkentry(frSave, textvariable = dir.save, width = largeur4)
    bt.dirsave <- tkbutton(frSave, text = "...")

    tkconfigure(bt.dirsave, command = function(){
        tcl('wm', 'attributes', tt, topmost = FALSE)
        fileORdir2Save(dir.save, isFile = FALSE)
        tcl('wm', 'attributes', tt, topmost = TRUE)
    })

    #########
    tkgrid(txt.dirsave, row = 0, column = 0, sticky = 'we', rowspan = 1, columnspan = 5, padx = 1, pady = 0, ipadx = 1, ipady = 1)
    tkgrid(en.dirsave, row = 1, column = 0, sticky = 'we', rowspan = 1, columnspan = 4, padx = 0, pady = 0, ipadx = 1, ipady = 1)
    tkgrid(bt.dirsave, row = 1, column = 4, sticky = 'we', rowspan = 1, columnspan = 1, padx = 0, pady = 0, ipadx = 1, ipady = 1)

    ############################################

    tkgrid(frNbFile, row = 0, column = 0, sticky = '', padx = 1, pady = 3, ipadx = 1, ipady = 1)
    tkgrid(frNCFile, row = 1, column = 0, sticky = 'we', padx = 1, pady = 3, ipadx = 1, ipady = 1)
    tkgrid(frSave, row = 2, column = 0, sticky = 'we', padx = 1, pady = 3, ipadx = 1, ipady = 1)

    ############################################
    
    tkgrid(frInner, row = 0, column = 0, sticky = 'news', padx = 5, pady = 1, ipadx = 1, ipady = 1)

    ############################################

    bt.prm.OK <- ttkbutton(frMRG1, text = .cdtEnv$tcl$lang$global[['button']][['1']])
    bt.prm.CA <- ttkbutton(frMRG1, text = .cdtEnv$tcl$lang$global[['button']][['2']])

    tkconfigure(bt.prm.OK, command = function(){
        if(str_trim(tclvalue(nc.file)) %in% c("", "NA")){
          cdt.tkmessageBox(tt, message = lang.dlg[['message']][['2']], icon = "warning", type = "ok")
          tkwait.window(tt)
        }else if(str_trim(tclvalue(dir.save)) %in% c("", "NA")){
          cdt.tkmessageBox(tt, message = lang.dlg[['message']][['3']], icon = "warning", type = "ok")
          tkwait.window(tt)
        }else{
          .cdtData$GalParams$nbfile <- nbfileVAL[cb.nbfileVAL %in% str_trim(tclvalue(nb.file))]

          .cdtData$GalParams$ncdf$file <- str_trim(tclvalue(nc.file))
          .cdtData$GalParams$ncdf$format <- str_trim(tclvalue(nc.format))
          .cdtData$GalParams$output <- str_trim(tclvalue(dir.save))

          .cdtData$GalParams$ncpars <- nc.params
          .cdtData$GalParams$message <- lang.dlg[['message']]

          tkgrab.release(tt)
          tkdestroy(tt)
          tkfocus(.cdtEnv$tcl$main$win)
        }
    })

    tkconfigure(bt.prm.CA, command = function(){
        tkgrab.release(tt)
        tkdestroy(tt)
        tkfocus(.cdtEnv$tcl$main$win)
    })

    tkgrid(bt.prm.OK, row = 0, column = 0, sticky = 'w', padx = 5, pady = 1, ipadx = 1, ipady = 1)
    tkgrid(bt.prm.CA, row = 0, column = 1, sticky = 'e', padx = 5, pady = 1, ipadx = 1, ipady = 1)

    ############################################

    tkgrid(frMRG0, row = 0, column = 0, sticky = 'nswe', rowspan = 1, columnspan = 2, padx = 1, pady = 1, ipadx = 1, ipady = 1)
    tkgrid(frMRG1, row = 1, column = 1, sticky = 'se', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)

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
        tkfocus(.cdtEnv$tcl$main$win)
    })
    tkwait.window(tt)
}
