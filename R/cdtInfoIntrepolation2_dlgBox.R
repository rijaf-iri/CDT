
getInterpolationPars2 <- function(parent.win, Parameters){
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

    xml.dlg <- file.path(.cdtDir$dirLocal, "languages", "cdtInfoIntrepolation2_dlgBox.xml")
    lang.dlg <- cdtLanguageParse(xml.dlg, .cdtData$Config$lang.iso)

    ################################

    tt <- tktoplevel()
    tkgrab.set(tt)
    tkfocus(tt)

    frGrd0 <- tkframe(tt, relief = 'raised', borderwidth = 2)
    frGrd1 <- tkframe(tt)

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
            chk.block <- tkcheckbutton(frInterpP, variable = use.block.var, text = lang.dlg[['checkbutton']][['1']], anchor = 'w', justify = 'left')

            tkgrid(txt.nmin, row = 0, column = 0, sticky = 'e', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
            tkgrid(en.nmin, row = 0, column = 1, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
            tkgrid(txt.nmax, row = 0, column = 2, sticky = 'e', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
            tkgrid(en.nmax, row = 0, column = 3, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
            tkgrid(txt.mdst, row = 0, column = 4, sticky = 'e', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
            tkgrid(en.mdst, row = 0, column = 5, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
            tkgrid(chk.block, row = 1, column = 0, sticky = 'we', rowspan = 1, columnspan = 6, padx = 1, pady = 1, ipadx = 1, ipady = 1)
            
            helpWidget(en.nmin, lang.dlg[['tooltip']][['1']], lang.dlg[['status']][['1']])
            helpWidget(en.nmax, lang.dlg[['tooltip']][['2']], lang.dlg[['status']][['2']])
            helpWidget(en.mdst, lang.dlg[['tooltip']][['3']], lang.dlg[['status']][['3']])
            helpWidget(chk.block, lang.dlg[['tooltip']][['4']], lang.dlg[['status']][['4']])
        }

        if(intmthd == "okr"){
            txt.nmin <- tklabel(frInterpP, text = "nmin", anchor = 'e', justify = 'right')
            txt.nmax <- tklabel(frInterpP, text = "nmax", anchor = 'e', justify = 'right')
            txt.mdst <- tklabel(frInterpP, text = "maxdist", anchor = 'e', justify = 'right')
            en.nmin <- tkentry(frInterpP, width = 5, textvariable = nmin.var, justify = 'right')
            en.nmax <- tkentry(frInterpP, width = 5, textvariable = nmax.var, justify = 'right')
            en.mdst <- tkentry(frInterpP, width = 5, textvariable = maxdist.var, justify = 'right')
            chk.block <- tkcheckbutton(frInterpP, variable = use.block.var, text = lang.dlg[['checkbutton']][['1']], anchor = 'w', justify = 'left')

            fr.vgm <- tkframe(frInterpP)
            txt.vgm <- tklabel(fr.vgm, text = lang.dlg[['label']][['1']], anchor = 'e', justify = 'right')
            en.vgm <- tkentry(fr.vgm, textvariable = vgm.model.var, justify = 'left', width = largeur2)

            tkgrid(txt.vgm, row = 0, column = 0, sticky = 'e', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
            tkgrid(en.vgm, row = 0, column = 1, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)

            fr.minstn <- tkframe(frInterpP)
            txt.minstn <- tklabel(fr.minstn, text = lang.dlg[['label']][['2']], anchor = 'e', justify = 'right')
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

            helpWidget(en.nmin, lang.dlg[['tooltip']][['1']], lang.dlg[['status']][['1']])
            helpWidget(en.nmax, lang.dlg[['tooltip']][['2']], lang.dlg[['status']][['2']])
            helpWidget(en.mdst, lang.dlg[['tooltip']][['3']], lang.dlg[['status']][['3']])
            helpWidget(chk.block, lang.dlg[['tooltip']][['4']], lang.dlg[['status']][['4']])
            helpWidget(en.vgm, lang.dlg[['tooltip']][['5']], lang.dlg[['status']][['5']])
            helpWidget(en.minstn, lang.dlg[['tooltip']][['6']], lang.dlg[['status']][['6']])
        }

        if(intmthd == "ukr"){
            txt.nmin <- tklabel(frInterpP, text = "nmin", anchor = 'e', justify = 'right')
            txt.nmax <- tklabel(frInterpP, text = "nmax", anchor = 'e', justify = 'right')
            txt.mdst <- tklabel(frInterpP, text = "maxdist", anchor = 'e', justify = 'right')
            en.nmin <- tkentry(frInterpP, width = 5, textvariable = nmin.var, justify = 'right')
            en.nmax <- tkentry(frInterpP, width = 5, textvariable = nmax.var, justify = 'right')
            en.mdst <- tkentry(frInterpP, width = 5, textvariable = maxdist.var, justify = 'right', state = 'disabled')
            chk.block <- tkcheckbutton(frInterpP, variable = use.block.var, text = lang.dlg[['checkbutton']][['1']], anchor = 'w', justify = 'left')

            fr.vgm <- tkframe(frInterpP)
            txt.vgm <- tklabel(fr.vgm, text = lang.dlg[['label']][['1']], anchor = 'e', justify = 'right')
            en.vgm <- tkentry(fr.vgm, textvariable = vgm.model.var, justify = 'left', width = largeur2)

            tkgrid(txt.vgm, row = 0, column = 0, sticky = 'e', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
            tkgrid(en.vgm, row = 0, column = 1, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)

            helpWidget(en.vgm, lang.dlg[['tooltip']][['5']], lang.dlg[['status']][['5']])

            ###
            fr.minstn <- tkframe(frInterpP)
            txt.minstn <- tklabel(fr.minstn, text = lang.dlg[['label']][['2']], anchor = 'e', justify = 'right')
            en.minstn <- tkentry(fr.minstn, width = 3, textvariable = minstn.var)

            tkgrid(txt.minstn, row = 0, column = 0, sticky = 'e', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
            tkgrid(en.minstn, row = 0, column = 1, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)

            helpWidget(en.minstn, lang.dlg[['tooltip']][['6']], lang.dlg[['status']][['6']])

            ###
            frAUX <- tkframe(frInterpP)

            dem.chk.auxvar <- tkcheckbutton(frAUX, variable = dem.auxvar, text = lang.dlg[['checkbutton']][['2']], anchor = 'w', justify = 'left')
            slope.chk.auxvar <- tkcheckbutton(frAUX, variable = slope.auxvar, text = lang.dlg[['checkbutton']][['3']], anchor = 'w', justify = 'left')
            aspect.chk.auxvar <- tkcheckbutton(frAUX, variable = aspect.auxvar, text = lang.dlg[['checkbutton']][['4']], anchor = 'w', justify = 'left')
            lon.chk.auxvar <- tkcheckbutton(frAUX, variable = lon.auxvar, text = lang.dlg[['checkbutton']][['5']], anchor = 'w', justify = 'left')
            lat.chk.auxvar <- tkcheckbutton(frAUX, variable = lat.auxvar, text = lang.dlg[['checkbutton']][['6']], anchor = 'w', justify = 'left')

            tkgrid(dem.chk.auxvar, row = 0, column = 0, sticky = 'we', rowspan = 1, columnspan = 1, ipady = 1)
            tkgrid(slope.chk.auxvar, row = 0, column = 1, sticky = 'we', rowspan = 1, columnspan = 1, ipady = 1)
            tkgrid(aspect.chk.auxvar, row = 0, column = 2, sticky = 'we', rowspan = 1, columnspan = 1, ipady = 1)
            tkgrid(lon.chk.auxvar, row = 0, column = 3, sticky = 'we', rowspan = 1, columnspan = 1, ipady = 1)
            tkgrid(lat.chk.auxvar, row = 0, column = 4, sticky = 'we', rowspan = 1, columnspan = 1, ipady = 1)

            helpWidget(dem.chk.auxvar, lang.dlg[['tooltip']][['9']], lang.dlg[['status']][['9']])
            helpWidget(slope.chk.auxvar, lang.dlg[['tooltip']][['10']], lang.dlg[['status']][['10']])
            helpWidget(aspect.chk.auxvar, lang.dlg[['tooltip']][['11']], lang.dlg[['status']][['11']])
            helpWidget(lon.chk.auxvar, lang.dlg[['tooltip']][['12']], lang.dlg[['status']][['12']])
            helpWidget(lat.chk.auxvar, lang.dlg[['tooltip']][['13']], lang.dlg[['status']][['13']])

            ###
            frDEM <- tkframe(frInterpP, relief = 'sunken', borderwidth = 2)

            statedem <- if(tclvalue(dem.auxvar) == "1" |
                           tclvalue(slope.auxvar) == "1" |
                           tclvalue(aspect.auxvar) == "1") "normal" else "disabled"

            txt.grddem <- tklabel(frDEM, text = lang.dlg[['label']][['5']],  anchor = 'w', justify = 'left')
            cb.grddem <- ttkcombobox(frDEM, values = unlist(openFile_ttkcomboList()), textvariable = demfile.var, width = largeur1, state = statedem)
            addTo_all_Combobox_List(cb.grddem)
            bt.grddem <- tkbutton(frDEM, text = "...", state = statedem)

            tkgrid(txt.grddem, row = 0, column = 0, sticky = 'we', rowspan = 1, columnspan = 2, padx = 1, pady = 1, ipadx = 1, ipady = 1)
            tkgrid(cb.grddem, row = 1, column = 0, sticky = 'we', rowspan = 1, columnspan = 1, padx = 0, pady = 1, ipadx = 1, ipady = 1)
            tkgrid(bt.grddem, row = 1, column = 1, sticky = 'we', rowspan = 1, columnspan = 1, padx = 0, pady = 1, ipadx = 1, ipady = 1)

            helpWidget(cb.grddem, lang.dlg[['tooltip']][['7']], lang.dlg[['status']][['7']])
            helpWidget(bt.grddem, lang.dlg[['tooltip']][['8']], lang.dlg[['status']][['8']])

            ###

            tkgrid(txt.nmin, row = 0, column = 0, sticky = 'e', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
            tkgrid(en.nmin, row = 0, column = 1, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
            tkgrid(txt.nmax, row = 0, column = 2, sticky = 'e', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
            tkgrid(en.nmax, row = 0, column = 3, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
            tkgrid(txt.mdst, row = 0, column = 4, sticky = 'e', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
            tkgrid(en.mdst, row = 0, column = 5, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
            tkgrid(chk.block, row = 1, column = 0, sticky = 'we', rowspan = 1, columnspan = 6, padx = 1, pady = 1, ipadx = 1, ipady = 1)
            tkgrid(fr.vgm, row = 2, column = 0, sticky = 'we', rowspan = 1, columnspan = 6, padx = 1, pady = 1, ipadx = 1, ipady = 1)
            tkgrid(fr.minstn, row = 3, column = 0, sticky = 'e', rowspan = 1, columnspan = 6, padx = 1, pady = 1, ipadx = 1, ipady = 1)
            tkgrid(frAUX, row = 4, column = 0, sticky = 'we', rowspan = 1, columnspan = 6, padx = 1, pady = 1, ipadx = 1, ipady = 1)
            tkgrid(frDEM, row = 5, column = 0, sticky = 'we', rowspan = 1, columnspan = 6, padx = 1, pady = 1, ipadx = 1, ipady = 1)

            helpWidget(en.nmin, lang.dlg[['tooltip']][['1']], lang.dlg[['status']][['1']])
            helpWidget(en.nmax, lang.dlg[['tooltip']][['2']], lang.dlg[['status']][['2']])
            helpWidget(en.mdst, lang.dlg[['tooltip']][['3']], lang.dlg[['status']][['3']])
            helpWidget(chk.block, lang.dlg[['tooltip']][['4']], lang.dlg[['status']][['4']])

            ######

            tkconfigure(bt.grddem, command = function(){
                tcl('wm', 'attributes', tt, topmost = FALSE)
                nc.opfiles <- getOpenNetcdf(tt, initialdir = getwd())
                tcl('wm', 'attributes', tt, topmost = TRUE)
                if(!is.null(nc.opfiles)){
                    update.OpenFiles('netcdf', nc.opfiles)
                    tclvalue(demfile.var) <- nc.opfiles[[1]]
                }
            })

            tkbind(dem.chk.auxvar, "<Button-1>", function(){
                statedem <- if(tclvalue(dem.auxvar) == "0" |
                              (tclvalue(slope.auxvar) == "1" |
                               tclvalue(aspect.auxvar) == "1")) 'normal' else 'disabled'
                tkconfigure(cb.grddem, state = statedem)
                tkconfigure(bt.grddem, state = statedem)
            })

            tkbind(slope.chk.auxvar, "<Button-1>", function(){
                statedem <- if(tclvalue(slope.auxvar) == "0" |
                              (tclvalue(dem.auxvar) == "1" |
                               tclvalue(aspect.auxvar) == "1")) 'normal' else 'disabled'
                tkconfigure(cb.grddem, state = statedem)
                tkconfigure(bt.grddem, state = statedem)
            })

            tkbind(aspect.chk.auxvar, "<Button-1>", function(){
                statedem <- if(tclvalue(aspect.auxvar) == "0" |
                              (tclvalue(slope.auxvar) == "1" |
                               tclvalue(dem.auxvar) == "1")) 'normal' else 'disabled'
                tkconfigure(cb.grddem, state = statedem)
                tkconfigure(bt.grddem, state = statedem)
            })
        }

        if(intmthd %in% c("shepard", "sphere")){
            txt.nmin <- tklabel(frInterpP, text = "nmin", anchor = 'e', justify = 'right')
            txt.nmax <- tklabel(frInterpP, text = "nmax", anchor = 'e', justify = 'right')
            txt.mdst <- tklabel(frInterpP, text = "maxdist", anchor = 'e', justify = 'right')
            en.nmin <- tkentry(frInterpP, width = 5, textvariable = nmin.var, justify = 'right')
            en.nmax <- tkentry(frInterpP, width = 5, textvariable = nmax.var, justify = 'right')
            en.mdst <- tkentry(frInterpP, width = 5, textvariable = maxdist.var, justify = 'right', state = 'disabled')

            tkgrid(txt.nmin, row = 0, column = 0, sticky = 'e', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
            tkgrid(en.nmin, row = 0, column = 1, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
            tkgrid(txt.nmax, row = 0, column = 2, sticky = 'e', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
            tkgrid(en.nmax, row = 0, column = 3, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
            tkgrid(txt.mdst, row = 0, column = 4, sticky = 'e', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
            tkgrid(en.mdst, row = 0, column = 5, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
            
            helpWidget(en.nmin, lang.dlg[['tooltip']][['1']], lang.dlg[['status']][['1']])
            helpWidget(en.nmax, lang.dlg[['tooltip']][['2']], lang.dlg[['status']][['2']])
            helpWidget(en.mdst, lang.dlg[['tooltip']][['3']], lang.dlg[['status']][['3']])
        }

        if(intmthd == "nns"){
            txt.mdst <- tklabel(frInterpP, text = lang.dlg[['label']][['3']], anchor = 'e', justify = 'right')
            en.mdst <- tkentry(frInterpP, width = 5, textvariable = maxdist.var, justify = 'right')

            tkgrid(txt.mdst, row = 0, column = 0, sticky = 'e', rowspan = 1, columnspan = 1, padx = 1, pady = 3, ipadx = 1, ipady = 1)
            tkgrid(en.mdst, row = 0, column = 1, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 3, ipadx = 1, ipady = 1)

            helpWidget(en.mdst, lang.dlg[['tooltip']][['3']], lang.dlg[['status']][['3']])
        }

        if(intmthd == "nn3d"){
            frMDST <- tkframe(frInterpP)
            txt.mdst <- tklabel(frMDST, text = lang.dlg[['label']][['4']], anchor = 'e', justify = 'right')
            en.mdst <- tkentry(frMDST, width = 5, textvariable = maxdist.var, justify = 'right')

            tkgrid(txt.mdst, row = 0, column = 0, sticky = 'e', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
            tkgrid(en.mdst, row = 0, column = 1, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)

            helpWidget(en.mdst, lang.dlg[['tooltip']][['3a']], lang.dlg[['status']][['3a']])
            ######

            frDEM <- tkframe(frInterpP, relief = 'sunken', borderwidth = 2)
            txt.grddem <- tklabel(frDEM, text = lang.dlg[['label']][['5']],  anchor = 'w', justify = 'left')
            cb.grddem <- ttkcombobox(frDEM, values = unlist(openFile_ttkcomboList()), textvariable = demfile.var, width = largeur1)
            addTo_all_Combobox_List(cb.grddem)
            bt.grddem <- tkbutton(frDEM, text = "...")

            tkgrid(txt.grddem, row = 0, column = 0, sticky = 'we', rowspan = 1, columnspan = 2, padx = 1, pady = 1, ipadx = 1, ipady = 1)
            tkgrid(cb.grddem, row = 1, column = 0, sticky = 'we', rowspan = 1, columnspan = 1, padx = 0, pady = 1, ipadx = 1, ipady = 1)
            tkgrid(bt.grddem, row = 1, column = 1, sticky = 'we', rowspan = 1, columnspan = 1, padx = 0, pady = 1, ipadx = 1, ipady = 1)

            helpWidget(cb.grddem, lang.dlg[['tooltip']][['7']], lang.dlg[['status']][['7']])
            helpWidget(bt.grddem, lang.dlg[['tooltip']][['8']], lang.dlg[['status']][['8']])

            ######

            tkconfigure(bt.grddem, command = function(){
                tcl('wm', 'attributes', tt, topmost = FALSE)
                nc.opfiles <- getOpenNetcdf(tt, initialdir = getwd())
                tcl('wm', 'attributes', tt, topmost = TRUE)
                if(!is.null(nc.opfiles)){
                    update.OpenFiles('netcdf', nc.opfiles)
                    tclvalue(demfile.var) <- nc.opfiles[[1]]
                }
            })

            ######
            tkgrid(frMDST, row = 0, column = 0, sticky = 'e', rowspan = 1, columnspan = 1, pady = 1, ipadx = 1, ipady = 1)
            tkgrid(frDEM, row = 1, column = 0, sticky = 'we', rowspan = 1, columnspan = 1, pady = 1, ipadx = 1, ipady = 1)
        }

        ######
        tkgrid(frInterpP)
    }

    ################################

    frInterpMthd <- tkframe(frGrd0, relief = 'sunken', borderwidth = 2)

    cb.interpMthd <- lang.dlg[['combobox']][['1']]
    val.interpMthd <- c("idw", "okr", "ukr", "shepard", "sphere", "nns", "nn3d")

    interp.method <- tclVar()
    tclvalue(interp.method) <- cb.interpMthd[val.interpMthd %in% Parameters$method]

    txt.mthd <- tklabel(frInterpMthd, text = lang.dlg[['label']][['6']], anchor = 'e', justify = 'right')
    cb.mthd <- ttkcombobox(frInterpMthd, values = cb.interpMthd, textvariable = interp.method, width = largeur0)

    tkgrid(txt.mthd, row = 0, column = 0, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
    tkgrid(cb.mthd, row = 0, column = 1, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)

    tkbind(cb.mthd, "<<ComboboxSelected>>", function(){
        tkconfigure(frInterpPars, text = tclvalue(interp.method))

        InterpP <- val.interpMthd[cb.interpMthd %in% trimws(tclvalue(interp.method))]
        interpolationParams(InterpP)
    })

    ################################

    frInterpPars <- ttklabelframe(frGrd0, text = tclvalue(interp.method), relief = 'groove', borderwidth = 2)

    frInterpP <- tkframe(frInterpPars)

    nmin.var <- tclVar(Parameters$nmin)
    nmax.var <- tclVar(Parameters$nmax)
    maxdist.var <- tclVar(Parameters$maxdist)
    use.block.var <- tclVar(Parameters$use.block)
    vgm.model.var <- tclVar(paste0(Parameters$vgm.model, collapse = ", "))
    minstn.var <- tclVar(Parameters$minstn)
    demfile.var <- tclVar(Parameters$demfile)

    dem.auxvar <- tclVar(Parameters$auxvar$dem)
    slope.auxvar <- tclVar(Parameters$auxvar$slope)
    aspect.auxvar <- tclVar(Parameters$auxvar$aspect)
    lon.auxvar <- tclVar(Parameters$auxvar$lon)
    lat.auxvar <- tclVar(Parameters$auxvar$lat)

    interpolationParams(Parameters$method)

    ################################

    tkgrid(frInterpMthd, row = 0, column = 0, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 3, ipadx = 1, ipady = 5)
    tkgrid(frInterpPars, row = 1, column = 0, sticky = '', rowspan = 1, columnspan = 1, padx = 1, pady = 3, ipadx = 1, ipady = 1)

    ################################

    bt.prm.OK <- ttkbutton(frGrd1, text = .cdtEnv$tcl$lang$global[['button']][['1']])
    bt.prm.CA <- ttkbutton(frGrd1, text = .cdtEnv$tcl$lang$global[['button']][['2']])

    tkconfigure(bt.prm.OK, command = function(){
        Parameters$method <<- val.interpMthd[cb.interpMthd %in% trimws(tclvalue(interp.method))]
        Parameters$nmin <<- as.numeric(trimws(tclvalue(nmin.var)))
        Parameters$nmax <<- as.numeric(trimws(tclvalue(nmax.var)))
        Parameters$maxdist <<- as.numeric(trimws(tclvalue(maxdist.var)))
        Parameters$minstn <<- as.numeric(trimws(tclvalue(minstn.var)))
        Parameters$use.block <<- switch(tclvalue(use.block.var), '0' = FALSE, '1' = TRUE)
        Parameters$demfile <<- trimws(tclvalue(demfile.var))

        vgms <- trimws(strsplit(tclvalue(vgm.model.var), ",")[[1]])
        Parameters$vgm.model <<- vgms[vgms != ""]

        Parameters$auxvar$dem <<- switch(tclvalue(dem.auxvar), '0' = FALSE, '1' = TRUE)
        Parameters$auxvar$slope <<- switch(tclvalue(slope.auxvar), '0' = FALSE, '1' = TRUE)
        Parameters$auxvar$aspect <<- switch(tclvalue(aspect.auxvar), '0' = FALSE, '1' = TRUE)
        Parameters$auxvar$lon <<- switch(tclvalue(lon.auxvar), '0' = FALSE, '1' = TRUE)
        Parameters$auxvar$lat <<- switch(tclvalue(lat.auxvar), '0' = FALSE, '1' = TRUE)

        if(
           (
            (Parameters$method == "nn3d") |
            (Parameters$method == "ukr" &
             (Parameters$auxvar$dem |
              Parameters$auxvar$slope |
              Parameters$auxvar$aspect)
            )
           ) & Parameters$demfile %in% c("", "NA")
          )
        {
            cdt.tkmessageBox(tt, message = lang.dlg[['message']][['1']], icon = "warning", type = "ok")
            tkwait.window(tt)
        }

        if(!any(do.call(c, Parameters$auxvar))){
            cdt.tkmessageBox(tt, message = lang.dlg[['message']][['2']], icon = "warning", type = "ok")
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
