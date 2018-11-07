
getInfoDateRange <- function(parent.win, Parameters, daypendek.lab = "Day",
							state.dek = 'normal', state.hour = 'disabled',
							state.mon = 'normal')
{
	# xml.dlg <- file.path(.cdtDir$dirLocal, "languages", "cdtInfoDateRange_dlgBox.xml")
	# lang.dlg <- cdtLanguageParse(xml.dlg, .cdtData$Config$lang.iso)

	###################

	tt <- tktoplevel()
	tkgrab.set(tt)
	tkfocus(tt)

	frMRG0 <- tkframe(tt, relief = 'raised', borderwidth = 2)
	frMRG1 <- tkframe(tt)

	###################
	if(is.null(Parameters$start.day)) Parameters$start.day <- 1
	if(is.null(Parameters$end.day)) Parameters$end.day <- 3
	if(is.null(Parameters$start.hour)) Parameters$start.hour <- 0
	if(is.null(Parameters$end.hour)) Parameters$end.hour <- 23

	###################

	frDatyR <- tkframe(frMRG0, relief = "sunken", borderwidth = 2)

	istart.yrs <- tclVar(Parameters$start.year)
	istart.mon <- tclVar(Parameters$start.mon)
	istart.day <- tclVar(Parameters$start.day)
	istart.hour <- tclVar(Parameters$start.hour)

	iend.yrs <- tclVar(Parameters$end.year)
	iend.mon <- tclVar(Parameters$end.mon)
	iend.day <- tclVar(Parameters$end.day)
	iend.hour <- tclVar(Parameters$end.hour)

	deb.txt <- tklabel(frDatyR, text = 'Start date:', anchor = 'e', justify = 'right')
	fin.txt <- tklabel(frDatyR, text = 'End date:', anchor = 'e', justify = 'right')
	yrs.txt <- tklabel(frDatyR, text = 'Year')
	mon.txt <- tklabel(frDatyR, text = 'Month')
	day.txt <- tklabel(frDatyR, text = daypendek.lab)
	hour.txt <- tklabel(frDatyR, text = 'Hour')

	yrs1.v <- tkentry(frDatyR, width = 4, textvariable = istart.yrs, justify = "center")
	mon1.v <- tkentry(frDatyR, width = 4, textvariable = istart.mon, justify = "center", state = state.mon)
	day1.v <- tkentry(frDatyR, width = 4, textvariable = istart.day, justify = "center", state = state.dek)
	hour1.v <- tkentry(frDatyR, width = 4, textvariable = istart.hour, justify = "center", state = state.hour)

	yrs2.v <- tkentry(frDatyR, width = 4, textvariable = iend.yrs, justify = "center")
	mon2.v <- tkentry(frDatyR, width = 4, textvariable = iend.mon, justify = "center", state = state.mon)
	day2.v <- tkentry(frDatyR, width = 4, textvariable = iend.day, justify = "center", state = state.dek)
	hour2.v <- tkentry(frDatyR, width = 4, textvariable = iend.hour, justify = "center", state = state.hour)

	###################
	tkgrid(deb.txt, row = 1, column = 0, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
	tkgrid(fin.txt, row = 2, column = 0, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
	tkgrid(yrs.txt, row = 0, column = 1, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
	tkgrid(mon.txt, row = 0, column = 2, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
	tkgrid(day.txt, row = 0, column = 3, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
	tkgrid(hour.txt, row = 0, column = 4, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)

	tkgrid(yrs1.v, row = 1, column = 1, sticky = '', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
	tkgrid(mon1.v, row = 1, column = 2, sticky = '', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
	tkgrid(day1.v, row = 1, column = 3, sticky = '', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
	tkgrid(hour1.v, row = 1, column = 4, sticky = '', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)

	tkgrid(yrs2.v, row = 2, column = 1, sticky = '', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
	tkgrid(mon2.v, row = 2, column = 2, sticky = '', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
	tkgrid(day2.v, row = 2, column = 3, sticky = '', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
	tkgrid(hour2.v, row = 2, column = 4, sticky = '', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)

	###################

	tkgrid(frDatyR, row = 0, column = 0, sticky = 'ewns', padx = 3, pady = 1, ipady = 5)

	################################

	bt.prm.OK <- ttkbutton(frMRG1, text = .cdtEnv$tcl$lang$global[['button']][['1']])
	bt.prm.CA <- ttkbutton(frMRG1, text = .cdtEnv$tcl$lang$global[['button']][['2']])

	tkconfigure(bt.prm.OK, command = function(){
		Parameters$start.year <<- as.numeric(str_trim(tclvalue(istart.yrs)))
		Parameters$start.mon <<- as.numeric(str_trim(tclvalue(istart.mon)))
		Parameters$start.day <<- as.numeric(str_trim(tclvalue(istart.day)))
		Parameters$start.hour <<- as.numeric(str_trim(tclvalue(istart.hour)))

		Parameters$end.year <<- as.numeric(str_trim(tclvalue(iend.yrs)))
		Parameters$end.mon <<- as.numeric(str_trim(tclvalue(iend.mon)))
		Parameters$end.day <<- as.numeric(str_trim(tclvalue(iend.day)))
		Parameters$end.hour <<- as.numeric(str_trim(tclvalue(iend.hour)))

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
	tkwm.title(tt, "Date Range Settings")
	tkwm.deiconify(tt)

	tkfocus(tt)
	tkbind(tt, "<Destroy>", function(){
		tkgrab.release(tt)
		tkfocus(parent.win)
	})
	tkwait.window(tt)
	return(Parameters)
}
