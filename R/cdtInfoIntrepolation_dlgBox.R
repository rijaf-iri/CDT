
getInterpolationPars <- function(parent.win, Parameters, group = 0){
    listOpenFiles <- openFile_ttkcomboList()
    if(WindowsOS()){
        largeur0 <- 35
        largeur1 <- 50
        largeur2 <- 35
    }else{
        largeur0 <- 28
        largeur1 <- 38
        largeur2 <- 25
    }

    ################################

    # xml.dlg <- file.path(.cdtDir$dirLocal, "languages", "cdtInfoIntrepolation_dlgBox.xml")
    # lang.dlg <- cdtLanguageParse(xml.dlg, .cdtData$Config$lang.iso)

    ################################

    tt <- tktoplevel()
    tkgrab.set(tt)
    tkfocus(tt)

    frGrd0 <- tkframe(tt, relief = 'raised', borderwidth = 2)
    frGrd1 <- tkframe(tt)

    ################################

    cb.interpMthd <- c('Inverse Distance Weighted', 'Ordinary Kriging', 'Nearest Neighbor',
                       'Nearest Neighbor with elevation - 3D', 'Bilinear interpolation')
    val.interpMthd <- c("idw", "okr", "nns", "nn3d", "blin")

    if(group == 0) idx <- 1:2
    if(group == 1) idx <- 1:4
    if(group == 2) idx <- c(1, 2, 5)
    if(group == 3) idx <- 1:5

    ################################

    interpolationParams <- function(intmthd){
        tkdestroy(frInterpP)

        frInterpP <<- tkframe(frInterpPars)

        ######
        if(intmthd == "idw"){
            txt.nmin <- tklabel(frInterpP, text = "nmin", anchor = 'e', justify = 'right')
            txt.nmax <- tklabel(frInterpP, text = "nmax", anchor = 'e', justify = 'right')
            txt.mdst <- tklabel(frInterpP, text = "maxdist", anchor = 'e', justify = 'right')
            en.nmin <- tkentry(frInterpP, width = 5, textvariable = nmin.var, justify = 'right')
            en.nmax <- tkentry(frInterpP, width = 5, textvariable = nmax.var, justify = 'right')
            en.mdst <- tkentry(frInterpP, width = 5, textvariable = maxdist.var, justify = 'right')
            chk.block <- tkcheckbutton(frInterpP, variable = use.block.var, text = "Use block mean values", anchor = 'w', justify = 'left')

            tkgrid(txt.nmin, row = 0, column = 0, sticky = 'e', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
            tkgrid(en.nmin, row = 0, column = 1, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
            tkgrid(txt.nmax, row = 0, column = 2, sticky = 'e', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
            tkgrid(en.nmax, row = 0, column = 3, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
            tkgrid(txt.mdst, row = 0, column = 4, sticky = 'e', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
            tkgrid(en.mdst, row = 0, column = 5, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
            tkgrid(chk.block, row = 1, column = 0, sticky = 'we', rowspan = 1, columnspan = 6, padx = 1, pady = 1, ipadx = 1, ipady = 1)

            helpWidget(en.nmin, 'Minimum number of neighbors to be used to interpolate data', 'Minimum number of neighbors to be used to interpolate data')
            helpWidget(en.nmax, 'Maximum number of neighbors to be used to interpolate data', 'Maximum number of neighbors to be used to interpolate data')
            helpWidget(en.mdst, 'Maximum distance (in decimal degree) to be used to interpolate data', 'Maximum distance (in decimal degree) to be used to interpolate data')
            helpWidget(chk.block, "Compute averaged estimates over blocks", "Compute averaged estimates over blocks")
        }

        if(intmthd == "okr"){
            txt.nmin <- tklabel(frInterpP, text = "nmin", anchor = 'e', justify = 'right')
            txt.nmax <- tklabel(frInterpP, text = "nmax", anchor = 'e', justify = 'right')
            txt.mdst <- tklabel(frInterpP, text = "maxdist", anchor = 'e', justify = 'right')
            en.nmin <- tkentry(frInterpP, width = 5, textvariable = nmin.var, justify = 'right')
            en.nmax <- tkentry(frInterpP, width = 5, textvariable = nmax.var, justify = 'right')
            en.mdst <- tkentry(frInterpP, width = 5, textvariable = maxdist.var, justify = 'right')
            chk.block <- tkcheckbutton(frInterpP, variable = use.block.var, text = "Use block mean values", anchor = 'w', justify = 'left')

            fr.vgm <- tkframe(frInterpP)
            txt.vgm <- tklabel(fr.vgm, text = "Variogram Model", anchor = 'e', justify = 'right')
            en.vgm <- tkentry(fr.vgm, textvariable = vgm.model.var, justify = 'left', width = largeur2)

            tkgrid(txt.vgm, row = 0, column = 0, sticky = 'e', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
            tkgrid(en.vgm, row = 0, column = 1, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)

            fr.minstn <- tkframe(frInterpP)
            txt.minstn <- tklabel(fr.minstn, text = "Minimum number of stations", anchor = 'e', justify = 'right')
            en.minstn <- tkentry(fr.minstn, width = 3, textvariable = minstn.var)

            tkgrid(txt.minstn, row = 0, column = 0, sticky = 'e', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
            tkgrid(en.minstn, row = 0, column = 1, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)

            tkgrid(txt.nmin, row = 0, column = 0, sticky = 'e', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
            tkgrid(en.nmin, row = 0, column = 1, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
            tkgrid(txt.nmax, row = 0, column = 2, sticky = 'e', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
            tkgrid(en.nmax, row = 0, column = 3, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
            tkgrid(txt.mdst, row = 0, column = 4, sticky = 'e', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
            tkgrid(en.mdst, row = 0, column = 5, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
            tkgrid(chk.block, row = 1, column = 0, sticky = 'we', rowspan = 1, columnspan = 6, padx = 1, pady = 1, ipadx = 1, ipady = 1)
            tkgrid(fr.vgm, row = 2, column = 0, sticky = 'we', rowspan = 1, columnspan = 6, padx = 1, pady = 1, ipadx = 1, ipady = 1)
            tkgrid(fr.minstn, row = 3, column = 0, sticky = 'e', rowspan = 1, columnspan = 6, padx = 1, pady = 1, ipadx = 1, ipady = 1)

            helpWidget(en.nmin, 'Minimum number of neighbors to be used to interpolate data', 'Minimum number of neighbors to be used to interpolate data')
            helpWidget(en.nmax, 'Maximum number of neighbors to be used to interpolate data', 'Maximum number of neighbors to be used to interpolate data')
            helpWidget(en.mdst, 'Maximum distance (in decimal degree) to be used to interpolate data', 'Maximum distance (in decimal degree) to be used to interpolate data')
            helpWidget(chk.block, "Compute averaged estimates over blocks", "Compute averaged estimates over blocks")
            helpWidget(en.vgm, 'Candidate variogram models to be selected, the best fitting will be used for the interpolation,\nsee gstat package for list of available model', 'Candidate variogram models to be selected, the best fitting will be used for the interpolation,\nsee gstat package for list of available model')
            helpWidget(en.minstn, 'Minimum number of points used to fit variogram models', 'Minimum number of points used to fit variogram models')
        }

        if(intmthd == "nns"){
            txt.mdst <- tklabel(frInterpP, text = "Radius of Influence", anchor = 'e', justify = 'right')
            en.mdst <- tkentry(frInterpP, width = 5, textvariable = maxdist.var, justify = 'right')

            tkgrid(txt.mdst, row = 0, column = 0, sticky = 'e', rowspan = 1, columnspan = 1, padx = 1, pady = 3, ipadx = 1, ipady = 1)
            tkgrid(en.mdst, row = 0, column = 1, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 3, ipadx = 1, ipady = 1)

            helpWidget(en.mdst, 'Maximum distance (in decimal degree) to be used to interpolate data', 'Maximum distance (in decimal degree) to be used to interpolate data')
        }

        if(intmthd == "nn3d"){
            frMDST <- tkframe(frInterpP)
            txt.mdst <- tklabel(frMDST, text = "Horizontal Radius of Influence", anchor = 'e', justify = 'right')
            en.mdst <- tkentry(frMDST, width = 5, textvariable = maxdist.var, justify = 'right')

            tkgrid(txt.mdst, row = 0, column = 0, sticky = 'e', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
            tkgrid(en.mdst, row = 0, column = 1, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)

            helpWidget(en.mdst, 'Horizontal maximum distance (in decimal degree) to be used to interpolate data', 'Horizontal maximum distance (in decimal degree) to be used to interpolate data')
            ######

            frDEM <- tkframe(frInterpP, relief = 'sunken', borderwidth = 2)
            txt.grddem <- tklabel(frDEM, text = "Elevation data (NetCDF)",  anchor = 'w', justify = 'left')
            cb.grddem <- ttkcombobox(frDEM, values = unlist(listOpenFiles), textvariable = demfile.var, width = largeur1)
            bt.grddem <- tkbutton(frDEM, text = "...")

            tkgrid(txt.grddem, row = 0, column = 0, sticky = 'we', rowspan = 1, columnspan = 2, padx = 1, pady = 1, ipadx = 1, ipady = 1)
            tkgrid(cb.grddem, row = 1, column = 0, sticky = 'we', rowspan = 1, columnspan = 1, padx = 0, pady = 1, ipadx = 1, ipady = 1)
            tkgrid(bt.grddem, row = 1, column = 1, sticky = 'we', rowspan = 1, columnspan = 1, padx = 0, pady = 1, ipadx = 1, ipady = 1)

            helpWidget(cb.grddem, 'Select the file in the list', 'File containing the elevation data in NetCDF')
            helpWidget(bt.grddem, 'Browse file if not listed', 'Browse file if not listed')

            ######

            tkconfigure(bt.grddem, command = function(){
                tcl('wm', 'attributes', tt, topmost = FALSE)
                nc.opfiles <- getOpenNetcdf(tt, initialdir = getwd())
                tcl('wm', 'attributes', tt, topmost = TRUE)
                if(!is.null(nc.opfiles)){
                    update.OpenFiles('netcdf', nc.opfiles)
                    listOpenFiles[[length(listOpenFiles) + 1]] <<- nc.opfiles[[1]]
                    tclvalue(demfile.var) <- nc.opfiles[[1]]
                    tkconfigure(cb.grddem, values = unlist(listOpenFiles))
                }
            })

            ######
            tkgrid(frMDST, row = 0, column = 0, sticky = 'e', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
            tkgrid(frDEM, row = 1, column = 0, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
        }

        if(intmthd == "blin"){
            tkgrid(tklabel(frInterpP, text = ""))
        }

        ######
        tkgrid(frInterpP)
    }

    ################################

    frInterpMthd <- tkframe(frGrd0, relief = 'sunken', borderwidth = 2)

    cb.interpMthd <- cb.interpMthd[idx]
    val.interpMthd <- val.interpMthd[idx]

    interp.method <- tclVar()
    tclvalue(interp.method) <- cb.interpMthd[val.interpMthd %in% Parameters$method]

    txt.mthd <- tklabel(frInterpMthd, text = 'Interpolation method', anchor = 'e', justify = 'right')
    cb.mthd <- ttkcombobox(frInterpMthd, values = cb.interpMthd, textvariable = interp.method, width = largeur0)

    tkgrid(txt.mthd, row = 0, column = 0, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
    tkgrid(cb.mthd, row = 0, column = 1, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)

    tkbind(cb.mthd, "<<ComboboxSelected>>", function(){
        tkconfigure(frInterpPars, text = tclvalue(interp.method))

        InterpP <- val.interpMthd[cb.interpMthd %in% str_trim(tclvalue(interp.method))]
        interpolationParams(InterpP)
    })

    ################################

    frInterpPars <- ttklabelframe(frGrd0, text = tclvalue(interp.method), relief = 'groove', borderwidth = 2)

    frInterpP <- tkframe(frInterpPars)

    nmin.var <- tclVar(Parameters$nmin)
    nmax.var <- tclVar(Parameters$nmax)
    maxdist.var <- tclVar(Parameters$maxdist)
    minstn.var <- tclVar(Parameters$minstn)
    use.block.var <- tclVar(Parameters$use.block)
    demfile.var <- tclVar(Parameters$demfile)
    vgm.model.var <- tclVar(paste0(Parameters$vgm.model, collapse = ", "))

    interpolationParams(Parameters$method)

    ################################

    tkgrid(frInterpMthd, row = 0, column = 0, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 3, ipadx = 1, ipady = 5)
    tkgrid(frInterpPars, row = 1, column = 0, sticky = '', rowspan = 1, columnspan = 1, padx = 1, pady = 3, ipadx = 1, ipady = 1)

    ################################

    bt.prm.OK <- ttkbutton(frGrd1, text = .cdtEnv$tcl$lang$global[['button']][['1']])
    bt.prm.CA <- ttkbutton(frGrd1, text = .cdtEnv$tcl$lang$global[['button']][['2']])

    tkconfigure(bt.prm.OK, command = function(){
        Parameters$method <<- val.interpMthd[cb.interpMthd %in% str_trim(tclvalue(interp.method))]
        Parameters$nmin <<- as.numeric(str_trim(tclvalue(nmin.var)))
        Parameters$nmax <<- as.numeric(str_trim(tclvalue(nmax.var)))
        Parameters$maxdist <<- as.numeric(str_trim(tclvalue(maxdist.var)))
        Parameters$minstn <<- as.numeric(str_trim(tclvalue(minstn.var)))
        Parameters$use.block <<- switch(tclvalue(use.block.var), '0' = FALSE, '1' = TRUE)
        Parameters$demfile <<- str_trim(tclvalue(demfile.var))

        vgms <- str_trim(strsplit(tclvalue(vgm.model.var), ",")[[1]])
        Parameters$vgm.model <<- vgms[vgms != ""]

        if((Parameters$method == "nn3d") &
           (Parameters$demfile %in% c("", "NA")))
        {
            cdt.tkmessageBox(tt, message = "You have to provide DEM data in NetCDF format", icon = "warning", type = "ok")
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

    tkgrid(bt.prm.CA, row = 0, column = 0, sticky = 'w', padx = 5, pady = 1, ipadx = 1, ipady = 1)
    tkgrid(bt.prm.OK, row = 0, column = 1, sticky = 'e', padx = 5, pady = 1, ipadx = 1, ipady = 1)

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
    tkwm.title(tt, 'Interpolation parameters')
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
