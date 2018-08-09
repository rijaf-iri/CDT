
getParams.RR.ZerosCheck <- function(Parameters)
{
	# xml.dlg <- file.path(.cdtDir$dirLocal, "languages", "cdtQC_RR.ZeroCheck_dlgBox.xml")
	# lang.dlg <- cdtLanguageParse(xml.dlg, .cdtData$Config$lang.iso)

	###################

	tt <- tktoplevel()
	tkgrab.set(tt)
	tkfocus(tt)

	frMRG0 <- tkframe(tt, relief = 'raised', borderwidth = 2)
	frMRG1 <- tkframe(tt)

	###################

	frParams <- tkframe(frMRG0, relief = "sunken", borderwidth = 2)

	min.nbrs <- tclVar(Parameters$min.nbrs)
	max.nbrs <- tclVar(Parameters$max.nbrs)
	min.days <- tclVar(Parameters$min.days)
	max.dst <- tclVar(Parameters$max.dist)
	pct.trsh <- tclVar(Parameters$min.thrs)

	txt.min.nb <- tklabel(frParams, text = 'Min number neighbors', anchor = 'e', justify = 'right')
	en.min.nb <- tkentry(frParams, width = 3, textvariable = min.nbrs, justify = 'left')
	txt.max.nb <- tklabel(frParams, text = 'Max number neighbors', anchor = 'e', justify = 'right')
	en.max.nb <- tkentry(frParams, width = 3, textvariable = max.nbrs, justify = 'left')
	txt.min.dy <- tklabel(frParams, text = 'Min number of days', anchor = 'e', justify = 'right')
	en.min.dy <- tkentry(frParams, width = 3, textvariable = min.days, justify = 'left')
	txt.max.dt <- tklabel(frParams, text = 'Max search distance', anchor = 'e', justify = 'right')
	en.max.dt <- tkentry(frParams, width = 3, textvariable = max.dst, justify = 'left')

	fr.thrs <- tkframe(frParams)
	txt.thrs <- tklabel(fr.thrs, text = 'Minimum threshold', anchor = 'e', justify = 'right')
	en.thrs <- tkentry(fr.thrs, width = 6, textvariable = pct.trsh, justify = 'left')

	tkgrid(txt.min.nb, row = 0, column = 0, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
	tkgrid(en.min.nb, row = 0, column = 1, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
	tkgrid(txt.max.nb, row = 0, column = 2, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
	tkgrid(en.max.nb, row = 0, column = 3, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
	tkgrid(txt.min.dy, row = 1, column = 0, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
	tkgrid(en.min.dy, row = 1, column = 1, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
	tkgrid(txt.max.dt, row = 1, column = 2, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
	tkgrid(en.max.dt, row = 1, column = 3, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)

	tkgrid(fr.thrs, row = 2, column = 0, sticky = '', rowspan = 1, columnspan = 4, padx = 1, pady = 1, ipadx = 1, ipady = 1)
	tkgrid(txt.thrs, row = 0, column = 0, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
	tkgrid(en.thrs, row = 0, column = 1, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)

	helpWidget(en.min.nb, 'Minimum number of neighbors stations to use', 'Minimum number of neighbors stations to use')
	helpWidget(en.max.nb, 'Maximum number of neighbors stations to use', 'Maximum number of neighbors stations to use')
	helpWidget(en.min.dy, 'Minimum number of days in a month with observation', 'Minimum number of days in a month with observation')
	helpWidget(en.max.dt, 'Maximum search distance (in km) for neighbors stations', 'Maximum search distance (in km) for neighbors stations')
	helpWidget(en.thrs, "Minimum threshold (% zero.station / % zero.neighbors)\nto flag that month's observation as problematic",
						"Minimum threshold (% zero.station / % zero.neighbors)\nto flag that month's observation as problematic")

	###################

	tkgrid(frParams, row = 0, column = 0, sticky = 'ewns', padx = 1, pady = 1, ipadx = 1, ipady = 1)

	################################

	bt.prm.OK <- ttkbutton(frMRG1, text = .cdtEnv$tcl$lang$global[['button']][['1']])
	bt.prm.CA <- ttkbutton(frMRG1, text = .cdtEnv$tcl$lang$global[['button']][['2']])

	tkconfigure(bt.prm.OK, command = function(){
		Parameters$min.nbrs <<- as.numeric(str_trim(tclvalue(min.nbrs)))
		Parameters$max.nbrs <<- as.numeric(str_trim(tclvalue(max.nbrs)))
		Parameters$min.days <<- as.numeric(str_trim(tclvalue(min.days)))
		Parameters$max.dist <<- as.numeric(str_trim(tclvalue(max.dst)))
		Parameters$min.thrs <<- as.numeric(str_trim(tclvalue(pct.trsh)))

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
	tkwm.title(tt, "False-Zeros Check Parameters")
	tkwm.deiconify(tt)

	tkfocus(tt)
	tkbind(tt, "<Destroy>", function(){
		tkgrab.release(tt)
		tkfocus(.cdtEnv$tcl$main$win)
	})
	tkwait.window(tt)
	return(Parameters)
}
