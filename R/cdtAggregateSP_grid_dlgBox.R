
getGridForRegriding <- function(parent.win, Parameters){
    listOpenFiles <- openFile_ttkcomboList()
    if(WindowsOS()){
        largeur0 <- 28
        largeur1 <- 40
        largeur2 <- 42
    }else{
        largeur0 <- 28
        largeur1 <- 32
        largeur2 <- 34
    }

    ################################

    xml.dlg <- file.path(.cdtDir$dirLocal, "languages", "cdtAggregateSP_grid_dlgBox.xml")
    lang.dlg <- cdtLanguageParse(xml.dlg, .cdtData$Config$lang.iso)

    ################################

    tt <- tktoplevel()
    tkgrab.set(tt)
    tkfocus(tt)

    frGrd0 <- tkframe(tt, relief = 'raised', borderwidth = 2)
    frGrd1 <- tkframe(tt)

    ################################

    frGrid <- tkframe(frGrd0, relief = 'sunken', borderwidth = 2)

    cbGridFormat <- lang.dlg[['combobox']][['1']]
    gridFormat <- c("cdtnetcdf", "cdtdataset")

    gridType <- tclVar()
    tclvalue(gridType) <- cbGridFormat[gridFormat %in% Parameters$type]

    txt.gridtype <- tklabel(frGrid, text = lang.dlg[['label']][['1']], anchor = 'w', justify = 'left')
    cb.gridtype <- ttkcombobox(frGrid, values = cbGridFormat, textvariable = gridType, justify = 'center', width = largeur0)

    tkgrid(txt.gridtype, row = 0, column = 0, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
    tkgrid(cb.gridtype, row = 0, column = 1, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)

    tkbind(cb.gridtype, "<<ComboboxSelected>>", function(){
        tkdestroy(cb.ncfldir)
        tclvalue(inputFile) <- ''
        srcfile <- gridFormat[cbGridFormat %in% str_trim(tclvalue(gridType))]

        if(srcfile == "cdtnetcdf"){
            tclvalue(fileINdir) <- lang.dlg[['label']][['2']]

            cb.ncfldir <- ttkcombobox(frInput, values = unlist(listOpenFiles), textvariable = inputFile, width = largeur1)

            #######
            tkconfigure(bt.ncfldir, command = function(){
                tcl('wm', 'attributes', tt, topmost = FALSE)
                nc.opfiles <- getOpenNetcdf(tt, initialdir = getwd())
                tcl('wm', 'attributes', tt, topmost = TRUE)
                if(!is.null(nc.opfiles)){
                    update.OpenFiles('netcdf', nc.opfiles)
                    listOpenFiles[[length(listOpenFiles) + 1]] <<- nc.opfiles[[1]]
                    tclvalue(inputFile) <- nc.opfiles[[1]]
                    tkconfigure(cb.ncfldir, values = unlist(listOpenFiles))
                }
            })
        }

        if(srcfile == "cdtdataset"){
            tclvalue(fileINdir) <- lang.dlg[['label']][['3']]

            cb.ncfldir <- tkentry(frInput, textvariable = inputFile, width = largeur2)

            #######
            tkconfigure(bt.ncfldir, command = function(){
                tcl('wm', 'attributes', tt, topmost = FALSE)
                path.rds <- tclvalue(tkgetOpenFile(initialdir = getwd(), filetypes = .cdtEnv$tcl$data$filetypes6))
                tcl('wm', 'attributes', tt, topmost = TRUE)
                tclvalue(inputFile) <- if(path.rds %in% c("", "NA") | is.na(path.rds)) "" else path.rds
            })
        }

        #######
        tkgrid(cb.ncfldir, row = 1, column = 0, sticky = 'we', rowspan = 1, columnspan = 4, padx = 0, pady = 1, ipadx = 1, ipady = 1)

        tkfocus(tt)
    })

    ################################

    frInput <- tkframe(frGrd0, relief = "sunken", borderwidth = 2)

    inputFile <- tclVar(Parameters$file)

    txtfiledir <- switch(Parameters$type,
                         "cdtnetcdf" = lang.dlg[['label']][['2']],
                         "cdtdataset" = lang.dlg[['label']][['3']])

    fileINdir <- tclVar(txtfiledir)

    txt.ncfldir <- tklabel(frInput, text = tclvalue(fileINdir), textvariable = fileINdir, anchor = 'w', justify = 'left')
    if(Parameters$type == "cdtnetcdf"){
        cb.ncfldir <- ttkcombobox(frInput, values = unlist(listOpenFiles), textvariable = inputFile, width = largeur1)
    }else{
        cb.ncfldir <- tkentry(frInput, textvariable = inputFile, width = largeur2)
    }
    bt.ncfldir <- tkbutton(frInput, text = "...")

    tkgrid(txt.ncfldir, row = 0, column = 0, sticky = 'we', rowspan = 1, columnspan = 5, padx = 1, pady = 1, ipadx = 1, ipady = 1)
    tkgrid(cb.ncfldir, row = 1, column = 0, sticky = 'we', rowspan = 1, columnspan = 4, padx = 0, pady = 1, ipadx = 1, ipady = 1)
    tkgrid(bt.ncfldir, row = 1, column = 4, sticky = 'we', rowspan = 1, columnspan = 1, padx = 0, pady = 1, ipadx = 1, ipady = 1)

    tkconfigure(bt.ncfldir, command = function(){
        srcfile <- gridFormat[cbGridFormat %in% str_trim(tclvalue(gridType))]
        if(srcfile == "cdtnetcdf"){
            tcl('wm', 'attributes', tt, topmost = FALSE)
            nc.opfiles <- getOpenNetcdf(tt, initialdir = getwd())
            tcl('wm', 'attributes', tt, topmost = TRUE)
            if(!is.null(nc.opfiles)){
                update.OpenFiles('netcdf', nc.opfiles)
                listOpenFiles[[length(listOpenFiles) + 1]] <<- nc.opfiles[[1]]
                tclvalue(inputFile) <- nc.opfiles[[1]]
                tkconfigure(cb.ncfldir, values = unlist(listOpenFiles))
            }
        }else{
            tcl('wm', 'attributes', tt, topmost = FALSE)
            path.rds <- tclvalue(tkgetOpenFile(initialdir = getwd(), filetypes = .cdtEnv$tcl$data$filetypes6))
            tcl('wm', 'attributes', tt, topmost = TRUE)
            tclvalue(inputFile) <- if(path.rds %in% c("", "NA") | is.na(path.rds)) "" else path.rds
        }
    })

    ################################

    tkgrid(frGrid, row = 0, column = 0, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
    tkgrid(frInput, row = 1, column = 0, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 3, ipadx = 1, ipady = 1)

    ################################

    bt.prm.OK <- ttkbutton(frGrd1, text = .cdtEnv$tcl$lang$global[['button']][['1']])
    bt.prm.CA <- ttkbutton(frGrd1, text = .cdtEnv$tcl$lang$global[['button']][['2']])

    tkconfigure(bt.prm.OK, command = function(){
        Parameters$type <<- gridFormat[cbGridFormat %in% str_trim(tclvalue(gridType))]
        Parameters$file <<- str_trim(tclvalue(inputFile))

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