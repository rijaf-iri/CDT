
getInterpolationPars1 <- function(parent.win, Parameters, stateMethod = "normal"){
    listOpenFiles <- openFile_ttkcomboList()
    if(WindowsOS()){
        largeur0 <- 35
        largeur1 <- 35
    }else{
        largeur0 <- 28
        largeur1 <- 25
    }

    ################################

    # xml.dlg <- file.path(.cdtDir$dirLocal, "languages", "cdtInfoIntrepolation1_dlgBox.xml")
    # lang.dlg <- cdtLanguageParse(xml.dlg, .cdtData$Config$lang.iso)

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
            chk.block <- tkcheckbutton(frInterpP, variable = use.block.var, text = "Use block mean values", anchor = 'w', justify = 'left')

            tkgrid(chk.block, row = 0, column = 0, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
            
            helpWidget(chk.block, "Compute averaged estimates over blocks", "Compute averaged estimates over blocks")
        }

        if(intmthd == "okr"){
            chk.block <- tkcheckbutton(frInterpP, variable = use.block.var, text = "Use block mean values", anchor = 'w', justify = 'left')

            fr.vgm <- tkframe(frInterpP)
            txt.vgm <- tklabel(fr.vgm, text = "Variogram Model", anchor = 'e', justify = 'right')
            en.vgm <- tkentry(fr.vgm, textvariable = vgm.model.var, justify = 'left', width = largeur1)

            tkgrid(txt.vgm, row = 0, column = 0, sticky = 'e', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
            tkgrid(en.vgm, row = 0, column = 1, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)

            fr.minstn <- tkframe(frInterpP)
            txt.minstn <- tklabel(fr.minstn, text = "Minimum number of stations", anchor = 'e', justify = 'right')
            en.minstn <- tkentry(fr.minstn, width = 3, textvariable = minstn.var)

            tkgrid(txt.minstn, row = 0, column = 0, sticky = 'e', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
            tkgrid(en.minstn, row = 0, column = 1, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)

            tkgrid(chk.block, row = 0, column = 0, sticky = 'we', rowspan = 1, columnspan = 6, padx = 1, pady = 1, ipadx = 1, ipady = 1)
            tkgrid(fr.vgm, row = 1, column = 0, sticky = 'we', rowspan = 1, columnspan = 6, padx = 1, pady = 1, ipadx = 1, ipady = 1)
            tkgrid(fr.minstn, row = 2, column = 0, sticky = 'e', rowspan = 1, columnspan = 6, padx = 1, pady = 1, ipadx = 1, ipady = 1)

            helpWidget(chk.block, "Compute averaged estimates over blocks", "Compute averaged estimates over blocks")
            helpWidget(en.vgm, 'Candidate variogram models to be selected, the best fitting will be used for the interpolation,\nsee gstat package for list of available model', 'Candidate variogram models to be selected, the best fitting will be used for the interpolation,\nsee gstat package for list of available model')
            helpWidget(en.minstn, 'Minimum number of points used to fit variogram models', 'Minimum number of points used to fit variogram models')
        }

        if(intmthd %in% c("shepard", "sphere")){
            tkgrid(tklabel(frInterpP, text = ""))
        }

        ######
        tkgrid(frInterpP)
    }

    ################################

    frInterpMthd <- tkframe(frGrd0, relief = 'sunken', borderwidth = 2)

    cb.interpMthd <- c('Inverse Distance Weighted', 'Ordinary Kriging',
                       'Modified Shepard interpolation', 'Spheremap interpolation method')
    val.interpMthd <- c("idw", "okr", "shepard", "sphere")

    interp.method <- tclVar()
    tclvalue(interp.method) <- cb.interpMthd[val.interpMthd %in% Parameters$method]

    nmin.var <- tclVar(Parameters$nmin)
    nmax.var <- tclVar(Parameters$nmax)
    maxdist.var <- tclVar(Parameters$maxdist)
    grid.var <- tclVar(Parameters$vargrd)

    statemdst <- if(Parameters$vargrd) "disabled" else "normal"

    txt.mthd <- tklabel(frInterpMthd, text = 'Interpolation method', anchor = 'e', justify = 'right')
    cb.mthd <- ttkcombobox(frInterpMthd, values = cb.interpMthd, textvariable = interp.method, width = largeur0, state = stateMethod)
    chk.GRDVar <- tkcheckbutton(frInterpMthd, variable = grid.var, text = "Use variable radius of influence for each grid", anchor = 'w', justify = 'left')

    frNeigh <- tkframe(frInterpMthd, relief = 'groove', borderwidth = 2)

    txt.nmin <- tklabel(frNeigh, text = "nmin pass=1 :", anchor = 'e', justify = 'right')
    txt.nmax <- tklabel(frNeigh, text = "nmax pass=1 :", anchor = 'e', justify = 'right')
    txt.mdst <- tklabel(frNeigh, text = "maxdist pass=1 :", anchor = 'e', justify = 'right')
    en.nmin <- tkentry(frNeigh, width = 5, textvariable = nmin.var, justify = 'right')
    en.nmax <- tkentry(frNeigh, width = 5, textvariable = nmax.var, justify = 'right')
    en.mdst <- tkentry(frNeigh, width = 5, textvariable = maxdist.var, justify = 'right', state = statemdst)

    tkgrid(txt.nmin, row = 0, column = 0, sticky = 'e', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
    tkgrid(en.nmin, row = 0, column = 1, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
    tkgrid(txt.nmax, row = 0, column = 2, sticky = 'e', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
    tkgrid(en.nmax, row = 0, column = 3, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
    tkgrid(txt.mdst, row = 1, column = 2, sticky = 'e', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
    tkgrid(en.mdst, row = 1, column = 3, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)

    tkgrid(txt.mthd, row = 0, column = 0, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
    tkgrid(cb.mthd, row = 0, column = 1, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
    tkgrid(frNeigh, row = 1, column = 0, sticky = 'we', rowspan = 1, columnspan = 2, padx = 1, pady = 3, ipadx = 1, ipady = 1)
    tkgrid(chk.GRDVar, row = 2, column = 0, sticky = 'we', rowspan = 1, columnspan = 2, padx = 1, pady = 1, ipadx = 1, ipady = 1)

    helpWidget(en.nmin, 'Minimum number of neighbors to be used to interpolate data,\n this value is the value for pass ratio 1,\n it will decrease depending on the fraction of the pass ratio',
        'Minimum number of neighbors to be used to interpolate data,\n this value is the value for pass ratio 1,\n it will decrease depending on the fraction of the pass ratio')
    helpWidget(en.nmax, 'Maximum number of neighbors to be used to interpolate data,\n this value is the value for pass ratio 1,\n it will decrease depending on the fraction of the pass ratio',
        'Maximum number of neighbors to be used to interpolate data,\n this value is the value for pass ratio 1,\n it will decrease depending on the fraction of the pass ratio')
    helpWidget(en.mdst, 'Maximum distance (in decimal degree) to be used to interpolate data,\n this value is the value for pass ratio 1,\n it will decrease depending on the fraction of the pass ratio',
        'Maximum distance (in decimal degree) to be used to interpolate data,\n this value is the value for pass ratio 1,\n it will decrease depending on the fraction of the pass ratio')

    ########

    tkbind(cb.mthd, "<<ComboboxSelected>>", function(){
        if(stateMethod == "normal"){
            tkconfigure(frInterpPars, text = tclvalue(interp.method))
            InterpP <- val.interpMthd[cb.interpMthd %in% str_trim(tclvalue(interp.method))]
            interpolationParams(InterpP)
        }
    })

    tkbind(chk.GRDVar, "<Button-1>", function(){
        statemdst <- if(tclvalue(grid.var) == "0") "disabled" else "normal"
        tkconfigure(en.mdst, state = statemdst)
    })

    ################################

    frInterpPars <- ttklabelframe(frGrd0, text = tclvalue(interp.method), relief = 'groove', borderwidth = 2)

    frInterpP <- tkframe(frInterpPars)

    minstn.var <- tclVar(Parameters$minstn)
    use.block.var <- tclVar(Parameters$use.block)
    vgm.model.var <- tclVar(paste0(Parameters$vgm.model, collapse = ", "))

    if(stateMethod == "normal") interpolationParams(Parameters$method)

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
        Parameters$vargrd <<- switch(tclvalue(grid.var), '0' = FALSE, '1' = TRUE)

        vgms <- str_trim(strsplit(tclvalue(vgm.model.var), ",")[[1]])
        Parameters$vgm.model <<- vgms[vgms != ""]

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