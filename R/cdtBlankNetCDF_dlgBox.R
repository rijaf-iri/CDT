
blankNcdf_GetInfo <- function(){
    listOpenFiles <- openFile_ttkcomboList()
    if(WindowsOS()){
        largeur0 <- 44
        largeur1 <- 46
        largeur2 <- 23
    }else{
        largeur0 <- 44
        largeur1 <- 45
        largeur2 <- 23
    }

    xml.dlg <- file.path(.cdtDir$dirLocal, "languages", "cdtBlankNetCDF_dlgBox.xml")
    lang.dlg <- cdtLanguageParse(xml.dlg, .cdtData$Config$lang.iso)

    ###################

    tt <- tktoplevel()
    tkgrab.set(tt)
    tkfocus(tt)

    frMRG0 <- tkframe(tt, relief = 'raised', borderwidth = 2)
    frMRG1 <- tkframe(tt)

    ############################################

    frNCDF <- tkframe(frMRG0, relief = "sunken", borderwidth = 2)

    nbcnfile <- tclVar()
    NBNCF <- lang.dlg[['combobox']][['1']]
    NBNC <- c('one', 'several')
    tclvalue(nbcnfile) <- NBNCF[NBNC %in% .cdtData$GalParams$nbnc]

    ncfiledir <- tclVar(.cdtData$GalParams$dirnc)
    ncsample <- tclVar(.cdtData$GalParams$sample)

    statesample <- if(.cdtData$GalParams$nbnc == "one") "disabled" else "normal"

    idx <- if(.cdtData$GalParams$nbnc == "one") "1" else "2"
    txtfiledir <- lang.dlg[['label']][[idx]]
    fileINdir <- tclVar(txtfiledir)

    cb.nbncf <- ttkcombobox(frNCDF, values = NBNCF, textvariable = nbcnfile, width = largeur2)

    txt.ncfldir <- tklabel(frNCDF, text = tclvalue(fileINdir), textvariable = fileINdir, anchor = 'w', justify = 'left')
    if(.cdtData$GalParams$nbnc == "one"){
        cb.ncfldir <- ttkcombobox(frNCDF, values = unlist(listOpenFiles), textvariable = ncfiledir, width = largeur0)
    }else{
        cb.ncfldir <- tkentry(frNCDF, textvariable = ncfiledir, width = largeur1)
    }
    bt.ncfldir <- tkbutton(frNCDF, text = "...")

    txt.ncsample <- tklabel(frNCDF, text = lang.dlg[['label']][['3']], anchor = 'w', justify = 'left')
    cb.ncsample <- ttkcombobox(frNCDF, values = unlist(listOpenFiles), textvariable = ncsample, width = largeur0, state = statesample)
    bt.ncsample <- tkbutton(frNCDF, text = "...", state = statesample)

    tkgrid(cb.nbncf, row = 0, column = 0, sticky = '', rowspan = 1, columnspan = 5, padx = 1, pady = 5, ipadx = 1, ipady = 1)

    tkgrid(txt.ncfldir, row = 1, column = 0, sticky = 'we', rowspan = 1, columnspan = 5, padx = 1, pady = 1, ipadx = 1, ipady = 1)
    tkgrid(cb.ncfldir, row = 2, column = 0, sticky = 'we', rowspan = 1, columnspan = 4, padx = 0, pady = 1, ipadx = 1, ipady = 1)
    tkgrid(bt.ncfldir, row = 2, column = 4, sticky = 'we', rowspan = 1, columnspan = 1, padx = 0, pady = 1, ipadx = 1, ipady = 1)

    tkgrid(txt.ncsample, row = 3, column = 0, sticky = 'we', rowspan = 1, columnspan = 5, padx = 1, pady = 1, ipadx = 1, ipady = 1)
    tkgrid(cb.ncsample, row = 4, column = 0, sticky = 'we', rowspan = 1, columnspan = 4, padx = 0, pady = 1, ipadx = 1, ipady = 1)
    tkgrid(bt.ncsample, row = 4, column = 4, sticky = 'we', rowspan = 1, columnspan = 1, padx = 0, pady = 1, ipadx = 1, ipady = 1)

    ###################

    tkconfigure(bt.ncfldir, command = function(){
        nbnc <- NBNC[NBNCF %in% str_trim(tclvalue(nbcnfile))]
        if(nbnc == 'one'){
            tcl('wm', 'attributes', tt, topmost = FALSE)
            nc.opfiles <- getOpenNetcdf(tt, initialdir = getwd())
            tcl('wm', 'attributes', tt, topmost = TRUE)
            if(!is.null(nc.opfiles)){
                update.OpenFiles('netcdf', nc.opfiles)
                listOpenFiles[[length(listOpenFiles) + 1]] <<- nc.opfiles[[1]]
                tclvalue(ncfiledir) <- nc.opfiles[[1]]

                lapply(list(cb.ncfldir, cb.ncsample, cb.blank), tkconfigure, values = unlist(listOpenFiles))
            }
        }else{
            tcl('wm', 'attributes', tt, topmost = FALSE)
            file2convert <- tk_choose.dir(getwd(), "")
            tcl('wm', 'attributes', tt, topmost = TRUE)
            tclvalue(ncfiledir) <- if(!is.na(file2convert)) file2convert else ""
        }
    })

    tkconfigure(bt.ncsample, command = function(){
        initialdir <- if(file.exists(tclvalue(ncfiledir))) tclvalue(ncfiledir) else getwd()
        tcl('wm', 'attributes', tt, topmost = FALSE)
        nc.opfiles <- getOpenNetcdf(tt, initialdir = initialdir)
        tcl('wm', 'attributes', tt, topmost = TRUE)
        if(!is.null(nc.opfiles)){
            update.OpenFiles('netcdf', nc.opfiles)
            listOpenFiles[[length(listOpenFiles) + 1]] <<- nc.opfiles[[1]]
            tclvalue(ncsample) <- nc.opfiles[[1]]

            nbnc <- NBNC[NBNCF %in% str_trim(tclvalue(nbcnfile))]
            setCB <- list(cb.ncfldir, cb.ncsample, cb.blank)
            if(nbnc == 'several') setCB <- setCB[-1]
            lapply(setCB, tkconfigure, values = unlist(listOpenFiles))
        }
    })

    tkbind(cb.nbncf, "<<ComboboxSelected>>", function(){
        tkdestroy(cb.ncfldir)
        tclvalue(ncfiledir) <- ''
        nbnc <- NBNC[NBNCF %in% str_trim(tclvalue(nbcnfile))]

        if(nbnc == 'one'){
            tclvalue(fileINdir) <- lang.dlg[['label']][['1']]

            cb.ncfldir <- ttkcombobox(frNCDF, values = unlist(listOpenFiles), textvariable = ncfiledir, width = largeur0)

            #######
            tkconfigure(bt.ncfldir, command = function(){
                tcl('wm', 'attributes', tt, topmost = FALSE)
                nc.opfiles <- getOpenNetcdf(tt, initialdir = getwd())
                tcl('wm', 'attributes', tt, topmost = TRUE)
                if(!is.null(nc.opfiles)){
                    update.OpenFiles('netcdf', nc.opfiles)
                    listOpenFiles[[length(listOpenFiles) + 1]] <<- nc.opfiles[[1]]
                    tclvalue(ncfiledir) <- nc.opfiles[[1]]

                    lapply(list(cb.ncfldir, cb.ncsample, cb.blank), tkconfigure, values = unlist(listOpenFiles))
                }
            })

            tkconfigure(cb.ncsample, state = 'disabled')
            tkconfigure(bt.ncsample, state = 'disabled')
        }

        #######
        if(nbnc == 'several'){
            tclvalue(fileINdir) <- lang.dlg[['label']][['2']]

            cb.ncfldir <- tkentry(frNCDF, textvariable = ncfiledir, width = largeur1)

            #######
            tkconfigure(bt.ncfldir, command = function(){
                tcl('wm', 'attributes', tt, topmost = FALSE)
                file2convert <- tk_choose.dir(getwd(), "")
                tcl('wm', 'attributes', tt, topmost = TRUE)
                tclvalue(ncfiledir) <- if(!is.na(file2convert)) file2convert else ""
            })

            tkconfigure(cb.ncsample, state = 'normal')
            tkconfigure(bt.ncsample, state = 'normal')
        }

        #######
        tkgrid(cb.ncfldir, row = 2, column = 0, sticky = 'we', rowspan = 1, columnspan = 4, padx = 0, pady = 1, ipadx = 1, ipady = 1)

        tkfocus(tt)
    })

    ############################################

    frSHP <- tkframe(frMRG0, relief = "sunken", borderwidth = 2)

    blank.shpf <- tclVar(.cdtData$GalParams$shpf)

    txt.blank <- tklabel(frSHP, text = lang.dlg[['label']][['4']], anchor = 'w', justify = 'left')
    cb.blank <- ttkcombobox(frSHP, values = unlist(listOpenFiles), textvariable = blank.shpf, width = largeur0)
    bt.blank <- tkbutton(frSHP, text = "...")

    tkgrid(txt.blank, row = 0, column = 0, sticky = 'we', rowspan = 1, columnspan = 2, padx = 1, pady = 1, ipadx = 1, ipady = 1)
    tkgrid(cb.blank, row = 1, column = 0, sticky = 'we', rowspan = 1, columnspan = 1, padx = 0, pady = 1, ipadx = 1, ipady = 1)
    tkgrid(bt.blank, row = 1, column = 1, sticky = 'we', rowspan = 1, columnspan = 1, padx = 0, pady = 1, ipadx = 1, ipady = 1)

    helpWidget(cb.blank, lang.dlg[['tooltip']][['1']], lang.dlg[['status']][['1']])
    helpWidget(bt.blank, lang.dlg[['tooltip']][['2']], lang.dlg[['status']][['2']])

    ########

    tkconfigure(bt.blank, command = function(){
        tcl('wm', 'attributes', tt, topmost = FALSE)
        shp.opfiles <- getOpenShp(tt)
        tcl('wm', 'attributes', tt, topmost = TRUE)
        if(!is.null(shp.opfiles)){
            update.OpenFiles('shp', shp.opfiles)
            tclvalue(blank.shpf) <- shp.opfiles[[1]]
            listOpenFiles[[length(listOpenFiles) + 1]] <<- shp.opfiles[[1]]

            nbnc <- NBNC[NBNCF %in% str_trim(tclvalue(nbcnfile))]
            setCB <- list(cb.ncfldir, cb.ncsample, cb.blank)
            if(nbnc == 'several') setCB <- setCB[-1]
            lapply(setCB, tkconfigure, values = unlist(listOpenFiles))
        }
    })

    ############################################

    frSave <- tkframe(frMRG0, relief = "sunken", borderwidth = 2)

    dir2save <- tclVar(.cdtData$GalParams$output)

    txt.dir2save <- tklabel(frSave, text = lang.dlg[['label']][['5']], anchor = 'w', justify = 'left')
    en.dir2save <- tkentry(frSave, textvariable = dir2save, width = largeur1)
    bt.dir2save <- tkbutton(frSave, text = "...")

    tkgrid(txt.dir2save, row = 0, column = 0, sticky = 'we', rowspan = 1, columnspan = 2, padx = 1, pady = 2, ipadx = 1, ipady = 1)
    tkgrid(en.dir2save, row = 1, column = 0, sticky = 'we', rowspan = 1, columnspan = 1, padx = 0, pady = 1, ipadx = 1, ipady = 1)
    tkgrid(bt.dir2save, row = 1, column = 1, sticky = 'we', rowspan = 1, columnspan = 1, padx = 0, pady = 1, ipadx = 1, ipady = 1)

    helpWidget(en.dir2save, lang.dlg[['tooltip']][['3']], lang.dlg[['status']][['3']])
    helpWidget(bt.dir2save, lang.dlg[['tooltip']][['4']], lang.dlg[['status']][['4']])

    #####

    tkconfigure(bt.dir2save, command = function(){
        initialdir <- if(str_trim(.cdtData$GalParams$output) != "") .cdtData$GalParams$output else getwd()
        tcl('wm', 'attributes', tt, topmost = FALSE)
        dir2savepth <- tk_choose.dir(initialdir, "")
        tcl('wm', 'attributes', tt, topmost = TRUE)
        if(is.na(dir2savepth)) tclvalue(dir2save) <- initialdir
        else{
            dir.create(dir2savepth, showWarnings = FALSE, recursive = TRUE)
            tclvalue(dir2save) <- dir2savepth
        }
    })

    ############################################

    tkgrid(frNCDF, row = 0, column = 0, sticky = 'we', padx = 1, pady = 1, ipadx = 1, ipady = 1)
    tkgrid(frSHP, row = 1, column = 0, sticky = 'we', padx = 1, pady = 1, ipadx = 1, ipady = 1)
    tkgrid(frSave, row = 2, column = 0, sticky = 'we', padx = 1, pady = 1, ipadx = 1, ipady = 1)

    ############################################

    bt.prm.OK <- ttkbutton(frMRG1, text = .cdtEnv$tcl$lang$global[['button']][['1']])
    bt.prm.CA <- ttkbutton(frMRG1, text = .cdtEnv$tcl$lang$global[['button']][['2']])

    ####
    tkconfigure(bt.prm.OK, command = function(){
        .cdtData$GalParams$nbnc <- NBNC[NBNCF %in% str_trim(tclvalue(nbcnfile))]
        .cdtData$GalParams$dirnc <- str_trim(tclvalue(ncfiledir))
        .cdtData$GalParams$sample <- str_trim(tclvalue(ncsample))
        .cdtData$GalParams$shpf <- str_trim(tclvalue(blank.shpf))
        .cdtData$GalParams$output <- str_trim(tclvalue(dir2save))

        .cdtData$GalParams$message <- lang.dlg[['message']]

        tkgrab.release(tt)
        tkdestroy(tt)
        tkfocus(.cdtEnv$tcl$main$win)
    })

    tkconfigure(bt.prm.CA, command = function(){
        tkgrab.release(tt)
        tkdestroy(tt)
        tkfocus(.cdtEnv$tcl$main$win)
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
        tkfocus(.cdtEnv$tcl$main$win)
    })
    tkwait.window(tt)
}
