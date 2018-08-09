
getParams.TT.OutlierCheck <- function(Parameters)
{
	# xml.dlg <- file.path(.cdtDir$dirLocal, "languages", "cdtQC_TT.OutlierCheck_dlgBox.xml")
	# lang.dlg <- cdtLanguageParse(xml.dlg, .cdtData$Config$lang.iso)

	###################

	tt <- tktoplevel()
	tkgrab.set(tt)
	tkfocus(tt)

	frMRG0 <- tkframe(tt, relief = 'raised', borderwidth = 2)
	frMRG1 <- tkframe(tt)

	###################

	framePars <- tkframe(frMRG0, relief = 'groove', borderwidth = 2)

	temp.max <- tclVar(Parameters$temp.max)
	temp.min <- tclVar(Parameters$temp.min)
	conf.lev <- tclVar(Parameters$conf.lev)
	window <- tclVar(Parameters$window)

	txt.tmin <- tklabel(framePars, text = 'Minimum Temperature', anchor = 'e', justify = 'right')
	en.tmin <- tkentry(framePars, width = 4, textvariable = temp.min, justify = 'left')
	txt.tmax <- tklabel(framePars, text = 'Maximum Temperature', anchor = 'e', justify = 'right')
	en.tmax <- tkentry(framePars, width = 4, textvariable = temp.max, justify = 'left')
	txt.win <- tklabel(framePars, text = 'Time window', anchor = 'e', justify = 'right')
	en.win <- tkentry(framePars, width = 6, textvariable = window, justify = 'left')
	txt.confL <- tklabel(framePars, text = 'Confidence level', anchor = 'e', justify = 'right')
	en.confL <- tkentry(framePars, width = 6, textvariable = conf.lev, justify = 'left')

	tkgrid(txt.tmin, row = 0, column = 0, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
	tkgrid(en.tmin, row = 0, column = 1, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
	tkgrid(txt.tmax, row = 1, column = 0, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
	tkgrid(en.tmax, row = 1, column = 1, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
	tkgrid(txt.win, row = 0, column = 2, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
	tkgrid(en.win, row = 0, column = 3, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
	tkgrid(txt.confL, row = 1, column = 2, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
	tkgrid(en.confL, row = 1, column = 3, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)

	helpWidget(en.tmin, 'Minimum value of temperature (degC) to be considered', 'Minimum value of temperature (degC) to be considered')
	helpWidget(en.tmax, 'Maximum value of temperature (degC) to be considered', 'Maximum value of temperature (degC) to be considered')
	helpWidget(en.win, 'Sliding time window to be used on regression', 'Sliding time window to be used on regression')
	helpWidget(en.confL, 'Confidence level (%)', 'Confidence level (%)')

	###################

	frameVois <- ttklabelframe(frMRG0, text = "Neighborhood Selection", relief = 'groove')

	min.nbrs <- tclVar(Parameters$voisin$min)
	max.nbrs <- tclVar(Parameters$voisin$max)
	max.dst <- tclVar(Parameters$voisin$dist)
	elv.diff <- tclVar(Parameters$voisin$elv)

	txt.min.nb <- tklabel(frameVois, text = 'Min number neighbors', anchor = 'e', justify = 'right')
	en.min.nb <- tkentry(frameVois, width = 3, textvariable = min.nbrs, justify = 'left')
	txt.max.nb <- tklabel(frameVois, text = 'Max number neighbors', anchor = 'e', justify = 'right')
	en.max.nb <- tkentry(frameVois, width = 3, textvariable = max.nbrs, justify = 'left')
	txt.max.dt <- tklabel(frameVois, text = 'Max search distance', anchor = 'e', justify = 'right')
	en.max.dt <- tkentry(frameVois, width = 3, textvariable = max.dst, justify = 'left')
	txt.elv.dif <- tklabel(frameVois, text = 'Elevation difference', anchor = 'e', justify = 'right')
	en.elv.dif <- tkentry(frameVois, width = 3, textvariable = elv.diff, justify = 'left')

	tkgrid(txt.min.nb, row = 0, column = 0, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
	tkgrid(en.min.nb, row = 0, column = 1, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
	tkgrid(txt.max.nb, row = 0, column = 2, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
	tkgrid(en.max.nb, row = 0, column = 3, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
	tkgrid(txt.max.dt, row = 1, column = 0, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
	tkgrid(en.max.dt, row = 1, column = 1, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
	tkgrid(txt.elv.dif, row = 1, column = 2, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
	tkgrid(en.elv.dif, row = 1, column = 3, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)

	helpWidget(en.min.nb, 'Minimum number of neighbors stations to be selected', 'Minimum number of neighbors stations to be selected')
	helpWidget(en.max.nb, 'Maximum number of neighbors stations to be selected', 'Maximum number of neighbors stations to be selected')
	helpWidget(en.max.dt, 'Maximum search distance (in km) for neighbors stations', 'Maximum search distance (in km) for neighbors stations')
	helpWidget(en.elv.dif, 'Maximum altitude difference (in m) for neighbors stations to be selected',
							'Maximum altitude difference (in m) for neighbors stations to be selected')

	###################

	tkgrid(framePars, row = 0, column = 0, sticky = '', padx = 1, ipadx = 1, pady = 3, ipady = 1)
	tkgrid(frameVois, row = 1, column = 0, sticky = 'we', padx = 1, ipadx = 1, pady = 1, ipady = 1)

	################################

	bt.prm.OK <- ttkbutton(frMRG1, text = .cdtEnv$tcl$lang$global[['button']][['1']])
	bt.prm.CA <- ttkbutton(frMRG1, text = .cdtEnv$tcl$lang$global[['button']][['2']])

	tkconfigure(bt.prm.OK, command = function(){
		Parameters$temp.max <<- as.numeric(str_trim(tclvalue(temp.max)))
		Parameters$temp.min <<- as.numeric(str_trim(tclvalue(temp.min)))
		Parameters$window <<- as.numeric(str_trim(tclvalue(window)))
		Parameters$conf.lev <<- as.numeric(str_trim(tclvalue(conf.lev)))

		Parameters$voisin$min <<- as.numeric(str_trim(tclvalue(min.nbrs)))
		Parameters$voisin$max <<- as.numeric(str_trim(tclvalue(max.nbrs)))
		Parameters$voisin$dist <<- as.numeric(str_trim(tclvalue(max.dst)))
		Parameters$voisin$elv <<- as.numeric(str_trim(tclvalue(elv.diff)))

		tkgrab.release(tt)
		tkdestroy(tt)
		tkfocus(.cdtEnv$tcl$main$win)
	})

	tkconfigure(bt.prm.CA, command = function(){
		tkgrab.release(tt)
		tkdestroy(tt)
		tkfocus(.cdtEnv$tcl$main$win)
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
	tkwm.title(tt, "Outliers Check Parameters")
	tkwm.deiconify(tt)

	tkfocus(tt)
	tkbind(tt, "<Destroy>", function(){
		tkgrab.release(tt)
		tkfocus(.cdtEnv$tcl$main$win)
	})
	tkwait.window(tt)
	return(Parameters)
}
