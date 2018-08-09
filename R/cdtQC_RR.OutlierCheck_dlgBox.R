
getParams.RR.OutlierCheck <- function(Parameters)
{
	# xml.dlg <- file.path(.cdtDir$dirLocal, "languages", "cdtQC_RR.OutlierCheck_dlgBox.xml")
	# lang.dlg <- cdtLanguageParse(xml.dlg, .cdtData$Config$lang.iso)

	###################

	tt <- tktoplevel()
	tkgrab.set(tt)
	tkfocus(tt)

	frMRG0 <- tkframe(tt, relief = 'raised', borderwidth = 2)
	frMRG1 <- tkframe(tt)

	###################

	framePars <- tkframe(frMRG0, relief = 'groove', borderwidth = 2)

	precip.max <- tclVar(Parameters$precip.max)
	conf.lev <- tclVar(Parameters$conf.lev)

	txt.rrmax <- tklabel(framePars, text = 'Maximum precipitation', anchor = 'e', justify = 'right')
	en.rrmax <- tkentry(framePars, width = 4, textvariable = precip.max, justify = 'left')
	txt.confL <- tklabel(framePars, text = 'Confidence level', anchor = 'e', justify = 'right')
	en.confL <- tkentry(framePars, width = 6, textvariable = conf.lev, justify = 'left')

	tkgrid(txt.rrmax, row = 0, column = 0, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
	tkgrid(en.rrmax, row = 0, column = 1, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
	tkgrid(txt.confL, row = 0, column = 2, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
	tkgrid(en.confL, row = 0, column = 3, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)

	helpWidget(en.rrmax, 'Maximum value of precipitation (mm) to be considered', 'Maximum value of precipitation (mm) to be considered')
	helpWidget(en.confL, 'Confidence level (%)', 'Confidence level (%)')

	###################

	frameSP <- ttklabelframe(frMRG0, text = "Spatial Check Parameters", relief = 'groove')

	ispmax <- tclVar(Parameters$spatial$ispmax)
	ispobs <- tclVar(Parameters$spatial$ispobs)
	iqrf <- tclVar(Parameters$spatial$iqrf)
	isdmin <- tclVar(Parameters$spatial$isdmin)
	isdobs <- tclVar(Parameters$spatial$isdobs)
	isdq1 <- tclVar(Parameters$spatial$isdq1)

	txt.ispmax <- tklabel(frameSP, text = 'ISPMAX', anchor = 'e', justify = 'right')
	en.ispmax <- tkentry(frameSP, width = 4, textvariable = ispmax, justify = 'left')
	txt.ispobs <- tklabel(frameSP, text = 'ISPOBS', anchor = 'e', justify = 'right')
	en.ispobs <- tkentry(frameSP, width = 4, textvariable = ispobs, justify = 'left')
	txt.iqrf <- tklabel(frameSP, text = 'IQRF', anchor = 'e', justify = 'right')
	en.iqrf <- tkentry(frameSP, width = 5, textvariable = iqrf, justify = 'left')
	txt.isdmin <- tklabel(frameSP, text = 'ISDMIN', anchor = 'e', justify = 'right')
	en.isdmin <- tkentry(frameSP, width = 4, textvariable = isdmin, justify = 'left')
	txt.isdobs <- tklabel(frameSP, text = 'ISDOBS', anchor = 'e', justify = 'right')
	en.isdobs <- tkentry(frameSP, width = 4, textvariable = isdobs, justify = 'left')
	txt.isdq1 <- tklabel(frameSP, text = 'ISDQ1', anchor = 'e', justify = 'right')
	en.isdq1 <- tkentry(frameSP, width = 5, textvariable = isdq1, justify = 'left')

	tkgrid(txt.ispmax, row = 0, column = 0, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
	tkgrid(en.ispmax, row = 0, column = 1, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
	tkgrid(txt.ispobs, row = 0, column = 2, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
	tkgrid(en.ispobs, row = 0, column = 3, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
	tkgrid(txt.iqrf, row = 0, column = 4, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
	tkgrid(en.iqrf, row = 0, column = 5, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
	tkgrid(txt.isdmin, row = 1, column = 0, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
	tkgrid(en.isdmin, row = 1, column = 1, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
	tkgrid(txt.isdobs, row = 1, column = 2, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
	tkgrid(en.isdobs, row = 1, column = 3, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
	tkgrid(txt.isdq1, row = 1, column = 4, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
	tkgrid(en.isdq1, row = 1, column = 5, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)

	helpWidget(en.ispmax, 'Isolated precipitation: maximum value (mm) of the neighbors stations is less than ISPMAX',
						'Isolated precipitation: maximum value (mm) of the neighbors stations is less than ISPMAX')
	helpWidget(en.ispobs, 'Isolated precipitation: the value (mm) of target station is greater ISPOBS',
						'Isolated precipitation: the value (mm) of target station is greater ISPOBS')
	helpWidget(en.iqrf, 'Outliers factor value between 2 and 4, multiplier of IQR',
						'Outliers factor value between 2 and 4, multiplier of IQR')
	helpWidget(en.isdmin, 'Isolated dryness: minimum value (mm) of the  neighbors stations is greater ISDMIN',
						'Isolated dryness: minimum value (mm) of the  neighbors stations is greater ISDMIN')
	helpWidget(en.isdobs, 'Isolated dryness: the value (mm) of target station is less than ISDOBS',
						'Isolated dryness: the value (mm) of target station is less than ISDOBS')
	helpWidget(en.isdq1, 'Isolated dryness: the first quartile value (mm) of the neighbors stations is greater than ISDQ1',
						'Isolated dryness: the first quartile value (mm) of the neighbors stations is greater than ISDQ1')

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
	tkgrid(frameSP, row = 2, column = 0, sticky = '', padx = 1, ipadx = 1, pady = 3, ipady = 1)

	################################

	bt.prm.OK <- ttkbutton(frMRG1, text = .cdtEnv$tcl$lang$global[['button']][['1']])
	bt.prm.CA <- ttkbutton(frMRG1, text = .cdtEnv$tcl$lang$global[['button']][['2']])

	tkconfigure(bt.prm.OK, command = function(){
		Parameters$precip.max <<- as.numeric(str_trim(tclvalue(precip.max)))
		Parameters$conf.lev <<- as.numeric(str_trim(tclvalue(conf.lev)))

		Parameters$voisin$min <<- as.numeric(str_trim(tclvalue(min.nbrs)))
		Parameters$voisin$max <<- as.numeric(str_trim(tclvalue(max.nbrs)))
		Parameters$voisin$dist <<- as.numeric(str_trim(tclvalue(max.dst)))
		Parameters$voisin$elv <<- as.numeric(str_trim(tclvalue(elv.diff)))

		Parameters$spatial$ispmax <<- as.numeric(str_trim(tclvalue(ispmax)))
		Parameters$spatial$ispobs <<- as.numeric(str_trim(tclvalue(ispobs)))
		Parameters$spatial$iqrf <<- as.numeric(str_trim(tclvalue(iqrf)))
		Parameters$spatial$isdmin <<- as.numeric(str_trim(tclvalue(isdmin)))
		Parameters$spatial$isdobs <<- as.numeric(str_trim(tclvalue(isdobs)))
		Parameters$spatial$isdq1 <<- as.numeric(str_trim(tclvalue(isdq1)))

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
