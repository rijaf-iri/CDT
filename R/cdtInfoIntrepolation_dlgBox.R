
getInterpolationPars <- function(tt, Parameters, interpChoix = 0){
	largeur <- if(WindowsOS()) 38 else 36

	# xml.dlg <- file.path(.cdtDir$dirLocal, "languages", "cdtInfoIntrepolation_dlgBox.xml")
	# lang.dlg <- cdtLanguageParse(xml.dlg, .cdtData$Config$lang.iso)

	NNHelpFun <- function(){
		infobulle(en.pars1, 'Maximum distance belong longitude, n times of grid interpolation resolution')
		status.bar.display(en.pars1, 'Maximum distance belong longitude, n times of grid interpolation resolution')
		infobulle(en.pars2, 'Maximum distance belong latitude, n times of grid interpolation resolution')
		status.bar.display(en.pars2, 'Maximum distance belong latitude, n times of grid interpolation resolution')
		infobulle(en.pars3, 'Maximum height for elevation, n times of elevation resolution\n(elevation is discretized by 100 m)')
		status.bar.display(en.pars3, 'Maximum height for elevation, n times of elevation resolution\n(elevation is discretized by 100 m)')
	}
	GstatHelpFun <- function(){
		infobulle(en.pars1, 'Minimum number of neighbors to be used to interpolate data')
		status.bar.display(en.pars1, 'Minimum number of neighbors to be used to interpolate data')
		infobulle(en.pars2, 'Maximum number of neighbors to be used to interpolate data')
		status.bar.display(en.pars2, 'Maximum number of neighbors to be used to interpolate data')
		infobulle(en.pars3, 'Maximum distance (in decimal degree) to be used to interpolate data')
		status.bar.display(en.pars3, 'Maximum distance (in decimal degree) to be used to interpolate data')
	}

	###################

	tt1 <- tktoplevel()
	tkgrab.set(tt1)
	tkfocus(tt1)

	frMRG0 <- tkframe(tt1, relief = 'raised', borderwidth = 2)
	frMRG1 <- tkframe(tt1)

	###################

	frInterp <- tkframe(frMRG0, relief = 'sunken', borderwidth = 2)

	CBInterpVAL <- c('Inverse Distance Weighted', 'Kriging', 'Nearest Neighbor', 'Fast Bilinear Interpolator')
	rInterpVAL <- c('IDW', 'Kriging', 'NN', 'FBL')

	if(interpChoix == 0) idx <- 1:2
	if(interpChoix == 1) idx <- c(1:2, 3)
	if(interpChoix == 2) idx <- c(1:2, 4)

	cb.InterpVAL <- CBInterpVAL[idx]
	interpVAL <- rInterpVAL[idx]
	interp.method <- tclVar()
	tclvalue(interp.method) <- cb.InterpVAL[interpVAL %in% Parameters$interp.method]

	frInterpMthd <- ttklabelframe(frInterp, text = 'Interpolation method', relief = 'groove', borderwidth = 2)
	cb.Interp <- ttkcombobox(frInterpMthd, values = cb.InterpVAL, textvariable = interp.method, width = largeur)
	tkgrid(cb.Interp, row = 0, column = 0, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 3, ipadx = 1, ipady = 1)

	tkbind(cb.Interp, "<<ComboboxSelected>>", function(){
		tkconfigure(frInterpPars, text = tclvalue(interp.method))

		texts <- if(tclvalue(interp.method) == CBInterpVAL[3]) texts1 else texts2
		tclvalue(txtvar1) <- texts[1]
		tclvalue(txtvar2) <- texts[2]
		tclvalue(txtvar3) <- texts[3]

		tkconfigure(txt.Lab1, text = tclvalue(txtvar1))
		tkconfigure(txt.Lab2, text = tclvalue(txtvar2))
		tkconfigure(txt.Lab3, text = tclvalue(txtvar3))

		statePars <- if(interpChoix == 2 & tclvalue(interp.method) == CBInterpVAL[4]) 'disabled' else 'normal'
		tkconfigure(en.pars1, state = statePars)
		tkconfigure(en.pars2, state = statePars)
		tkconfigure(en.pars3, state = statePars)

		varpars <<- if(tclvalue(interp.method) == CBInterpVAL[3]) varpars1 else varpars2
		if((parsenv$previous[1] %in% CBInterpVAL[1:2] & tclvalue(interp.method) == CBInterpVAL[3]) |
		(parsenv$previous[1] == CBInterpVAL[3] & tclvalue(interp.method) %in% CBInterpVAL[1:2]))
		{
			tmp1 <- Parameters[[varpars[1]]]
			tmp2 <- Parameters[[varpars[2]]]
			tmp3 <- Parameters[[varpars[3]]]
		}else{
			tmp1 <- parsenv$previous[2]
			tmp2 <- parsenv$previous[3]
			tmp3 <- parsenv$previous[4]
		}
		tclvalue(parsenv$pars1) <- tmp1
		tclvalue(parsenv$pars2) <- tmp2
		tclvalue(parsenv$pars3) <- tmp3

		parsenv$previous <- c(tclvalue(interp.method), tclvalue(parsenv$pars1),
								tclvalue(parsenv$pars2), tclvalue(parsenv$pars3))
		if(tclvalue(interp.method) == CBInterpVAL[3]) NNHelpFun() else GstatHelpFun()
	})

	tkbind(cb.Interp, "<Button-1>", function(){
		parsenv$previous <- c(tclvalue(interp.method), tclvalue(parsenv$pars1),
							tclvalue(parsenv$pars2), tclvalue(parsenv$pars3))
	})

	#####
	frInterpPars <- ttklabelframe(frInterp, text = tclvalue(interp.method), relief = 'groove', borderwidth = 2)

	parsenv <- new.env()

	texts1 <- c('Multi.Lon', 'Multi.Lat', 'Multi.Elv')
	varpars1 <- c('rad.lon', 'rad.lat', 'rad.elv')
	varpars2 <- texts2 <- c('nmin', 'nmax', 'maxdist')

	texts <- if(Parameters$interp.method == 'NN') texts1 else texts2
	varpars <- if(Parameters$interp.method == 'NN') varpars1 else varpars2

	txtvar1 <- tclVar(texts[1])
	txtvar2 <- tclVar(texts[2])
	txtvar3 <- tclVar(texts[3])
	parsenv$pars1 <- tclVar(Parameters[[varpars[1]]])
	parsenv$pars2 <- tclVar(Parameters[[varpars[2]]])
	parsenv$pars3 <- tclVar(Parameters[[varpars[3]]])
	parsenv$previous <- c(tclvalue(interp.method), tclvalue(parsenv$pars1),
						tclvalue(parsenv$pars2), tclvalue(parsenv$pars3))

	statePars <- if(interpChoix == 2 & Parameters$interp.method == 'FBL') 'disabled' else 'normal'

	txt.Lab1 <- tklabel(frInterpPars, text = tclvalue(txtvar1), anchor = 'e', justify = 'right')
	txt.Lab2 <- tklabel(frInterpPars, text = tclvalue(txtvar2), anchor = 'e', justify = 'right')
	txt.Lab3 <- tklabel(frInterpPars, text = tclvalue(txtvar3), anchor = 'e', justify = 'right')
	en.pars1 <- tkentry(frInterpPars, width = 4, textvariable = parsenv$pars1, justify = 'right', state = statePars)
	en.pars2 <- tkentry(frInterpPars, width = 4, textvariable = parsenv$pars2, justify = 'right', state = statePars)
	en.pars3 <- tkentry(frInterpPars, width = 4, textvariable = parsenv$pars3, justify = 'right', state = statePars)

	tkgrid(txt.Lab1, row = 0, column = 0, sticky = 'e', rowspan = 1, columnspan = 1, padx = 1, pady = 3, ipadx = 1, ipady = 1)
	tkgrid(en.pars1, row = 0, column = 1, sticky = 'w', rowspan = 1, columnspan = 1, padx = 1, pady = 3, ipadx = 1, ipady = 1)
	tkgrid(txt.Lab2, row = 0, column = 2, sticky = 'e', rowspan = 1, columnspan = 1, padx = 1, pady = 3, ipadx = 1, ipady = 1)
	tkgrid(en.pars2, row = 0, column = 3, sticky = 'w', rowspan = 1, columnspan = 1, padx = 1, pady = 3, ipadx = 1, ipady = 1)
	tkgrid(txt.Lab3, row = 0, column = 4, sticky = 'e', rowspan = 1, columnspan = 1, padx = 1, pady = 3, ipadx = 1, ipady = 1)
	tkgrid(en.pars3, row = 0, column = 5, sticky = 'w', rowspan = 1, columnspan = 1, padx = 1, pady = 3, ipadx = 1, ipady = 1)

	if(Parameters$interp.method == 'NN') NNHelpFun() else GstatHelpFun()

	########
	tkgrid(frInterpMthd, row = 0, column = 0, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
	tkgrid(frInterpPars, row = 1, column = 0, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)

	########
	tkgrid(frInterp, row = 0, column = 0, sticky = 'snwe', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)

	################################

	bt.prm.OK <- ttkbutton(frMRG1, text = .cdtEnv$tcl$lang$global[['button']][['1']])
	bt.prm.CA <- ttkbutton(frMRG1, text = .cdtEnv$tcl$lang$global[['button']][['2']])

	tkconfigure(bt.prm.OK, command = function(){
		Parameters[[varpars[1]]] <<- as.numeric(str_trim(tclvalue(parsenv$pars1)))
		Parameters[[varpars[2]]] <<- as.numeric(str_trim(tclvalue(parsenv$pars2)))
		Parameters[[varpars[3]]] <<- as.numeric(str_trim(tclvalue(parsenv$pars3)))

		Parameters$interp.method <<- rInterpVAL[CBInterpVAL %in% str_trim(tclvalue(interp.method))]

		tkgrab.release(tt1)
		tkdestroy(tt1)
		tkfocus(tt)
	})

	tkconfigure(bt.prm.CA, command = function(){
		tkgrab.release(tt1)
		tkdestroy(tt1)
		tkfocus(tt)
	})

	tkgrid(bt.prm.OK, row = 0, column = 0, sticky = 'w', padx = 5, pady = 1, ipadx = 1, ipady = 1)
	tkgrid(bt.prm.CA, row = 0, column = 1, sticky = 'e', padx = 5, pady = 1, ipadx = 1, ipady = 1)

	################################
	tkgrid(frMRG0, row = 0, column = 0, sticky = 'nswe', rowspan = 1, columnspan = 2, padx = 1, pady = 1, ipadx = 1, ipady = 1)
	tkgrid(frMRG1, row = 1, column = 1, sticky = 'se', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)

	################################
	tkwm.withdraw(tt1)
	tcl('update')
	tt.w <- as.integer(tkwinfo("reqwidth", tt1))
	tt.h <- as.integer(tkwinfo("reqheight", tt1))
	tt.x <- as.integer(.cdtEnv$tcl$data$width.scr*0.5 - tt.w*0.5)
	tt.y <- as.integer(.cdtEnv$tcl$data$height.scr*0.5 - tt.h*0.5)
	tkwm.geometry(tt1, paste0('+', tt.x, '+', tt.y))
	tkwm.transient(tt1)
	tkwm.title(tt1, 'Interpolation parameters')
	tkwm.deiconify(tt1)

	tkfocus(tt1)
	tkbind(tt1, "<Destroy>", function(){
		tkgrab.release(tt1)
		tkfocus(tt)
	})
	tkwait.window(tt1)
	return(Parameters)
}
