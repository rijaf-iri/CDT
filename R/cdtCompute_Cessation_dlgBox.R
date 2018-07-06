
computeWB_Cessation <- function(Parameters, data.type){
	MOIS <- format(ISOdate(2014, 1:12, 1), "%B")

	# xml.dlg <- file.path(.cdtDir$dirLocal, "languages", "cdtCompute_Cessation_dlgBox.xml")
	# lang.dlg <- cdtLanguageParse(xml.dlg, .cdtData$Config$lang.iso)

	############################################

	tt <- tktoplevel()
	tkgrab.set(tt)
	tkfocus(tt)

	frMRG0 <- tkframe(tt, relief = 'raised', borderwidth = 2)
	frMRG1 <- tkframe(tt)

	############################################

	frWBalance <- tkframe(frMRG0, relief = 'sunken', borderwidth = 2)

	imon <- as.numeric(str_trim(Parameters$hdate$start.month))
	start.month <- tclVar(MOIS[imon])
	start.day <- tclVar(Parameters$hdate$start.day)
	separate.year <- tclVar(Parameters$hdate$separate.year)
	start.wb <- tclVar(Parameters$wb$wb1)
	capacity.max <- tclVar(Parameters$swhc$cap.max)

	use.multi.wb <- tclVar(Parameters$wb$multi)
	use.multi.swhc <- tclVar(Parameters$swhc$multi)

	stateMWB <- if(Parameters$wb$multi) "normal" else "disabled"
	stateMSWHC <- if(Parameters$swhc$multi) "normal" else "disabled"

	chk.sep.year <- tkcheckbutton(frWBalance, variable = separate.year, text = 'Compute each year separately', anchor = 'w', justify = 'left')

	txt.1stdate0 <- tklabel(frWBalance, text = "Start Water Balance from", anchor = 'e', justify = 'right')
	txt.1stdate1 <- tklabel(frWBalance, text = "Month", anchor = 'e', justify = 'right')
	cb.1stdate1 <- ttkcombobox(frWBalance, values = MOIS, textvariable = start.month, width = 9)
	txt.1stdate2 <- tklabel(frWBalance, text = "Day", anchor = 'e', justify = 'right')
	cb.1stdate2 <- ttkcombobox(frWBalance, values = 1:31, textvariable = start.day, width = 2)

	txt.wb.1stday <- tklabel(frWBalance, text = "First Day Water Balance", anchor = 'w', justify = 'left')
	en.wb.1stday <- tkentry(frWBalance, textvariable = start.wb, width = 4)

	chk.wb.1stday <- tkcheckbutton(frWBalance, variable = use.multi.wb, text = "Multiple WB", anchor = 'w', justify = 'left')
	bt.wb.1stday <- tkbutton(frWBalance, text = "Set", state = stateMWB)

	txt.wb.swhc <- tklabel(frWBalance, text = "Soil Water Holding Capacity", anchor = 'w', justify = 'left')
	en.wb.swhc <- tkentry(frWBalance, textvariable = capacity.max, width = 4)

	chk.wb.swhc <- tkcheckbutton(frWBalance, variable = use.multi.swhc, text = "Multiple SWHC", anchor = 'w', justify = 'left')
	bt.wb.swhc <- tkbutton(frWBalance, text = "Set", state = stateMSWHC)

	###############
	tkconfigure(bt.wb.1stday, command = function(){
		Parameters$wb[["file"]] <<- computeWB_get.WB.SWHC(tt, Parameters$wb[["file"]], data.type, "WB")
	})

	tkconfigure(bt.wb.swhc, command = function(){
		Parameters$swhc[["file"]] <<- computeWB_get.WB.SWHC(tt, Parameters$swhc[["file"]], data.type, "SWHC")
	})

	###############

	tkgrid(chk.sep.year, row = 0, column = 0, sticky = 'we', rowspan = 1, columnspan = 10, padx = 1, pady = 1, ipadx = 1, ipady = 1)

	tkgrid(txt.1stdate0, row = 1, column = 0, sticky = 'we', rowspan = 1, columnspan = 5, padx = 1, pady = 1, ipadx = 1, ipady = 1)
	tkgrid(txt.1stdate1, row = 1, column = 5, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
	tkgrid(cb.1stdate1, row = 1, column = 6, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
	tkgrid(txt.1stdate2, row = 1, column = 7, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
	tkgrid(cb.1stdate2, row = 1, column = 8, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)

	tkgrid(txt.wb.1stday, row = 2, column = 0, sticky = 'we', rowspan = 1, columnspan = 5, padx = 1, pady = 1, ipadx = 1, ipady = 1)
	tkgrid(en.wb.1stday, row = 2, column = 5, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
	tkgrid(chk.wb.1stday, row = 2, column = 6, sticky = 'we', rowspan = 1, columnspan = 2, padx = 1, pady = 1, ipadx = 1, ipady = 1)
	tkgrid(bt.wb.1stday, row = 2, column = 8, sticky = 'we', rowspan = 1, columnspan = 2, padx = 1, pady = 1, ipadx = 1, ipady = 1)

	tkgrid(txt.wb.swhc, row = 3, column = 0, sticky = 'we', rowspan = 1, columnspan = 5, padx = 1, pady = 1, ipadx = 1, ipady = 1)
	tkgrid(en.wb.swhc, row = 3, column = 5, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
	tkgrid(chk.wb.swhc, row = 3, column = 6, sticky = 'we', rowspan = 1, columnspan = 2, padx = 1, pady = 1, ipadx = 1, ipady = 1)
	tkgrid(bt.wb.swhc, row = 3, column = 8, sticky = 'we', rowspan = 1, columnspan = 2, padx = 1, pady = 1, ipadx = 1, ipady = 1)

	###############

	tkbind(chk.wb.1stday, "<Button-1>", function(){
		stateMWB <- if(tclvalue(use.multi.wb) == '0') 'normal' else 'disabled'
		tkconfigure(bt.wb.1stday, state = stateMWB)
	})

	tkbind(chk.wb.swhc, "<Button-1>", function(){
		stateMSWHC <- if(tclvalue(use.multi.swhc) == '0') 'normal' else 'disabled'
		tkconfigure(bt.wb.swhc, state = stateMSWHC)
	})

	############################################
	tkgrid(frWBalance, row = 0, column = 0, sticky = 'we', padx = 1, pady = 3, ipadx = 1, ipady = 1)

	############################################

	bt.prm.OK <- ttkbutton(frMRG1, text = .cdtEnv$tcl$lang$global[['button']][['1']])
	bt.prm.CA <- ttkbutton(frMRG1, text = .cdtEnv$tcl$lang$global[['button']][['2']])

	tkconfigure(bt.prm.OK, command = function(){
			Parameters$hdate$start.month <<- which(MOIS %in% str_trim(tclvalue(start.month)))
			Parameters$hdate$start.day <<- as.numeric(str_trim(tclvalue(start.day)))
			Parameters$hdate$separate.year <<- switch(tclvalue(separate.year), '0' = FALSE, '1' = TRUE)

			Parameters$wb$multi <<- switch(tclvalue(use.multi.wb), '0' = FALSE, '1' = TRUE)
			Parameters$swhc$multi <<- switch(tclvalue(use.multi.swhc), '0' = FALSE, '1' = TRUE)

			Parameters$wb$wb1 <<- as.numeric(str_trim(tclvalue(start.wb)))
			Parameters$swhc$cap.max <<- as.numeric(str_trim(tclvalue(capacity.max)))

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

	############################################

	tkgrid(frMRG0, row = 0, column = 0, sticky = 'nswe', rowspan = 1, columnspan = 2, padx = 1, pady = 1, ipadx = 1, ipady = 1)
	tkgrid(frMRG1, row = 1, column = 1, sticky = 'se', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)

	############################3
	tkwm.withdraw(tt)
	tcl('update')
	tt.w <- as.integer(tkwinfo("reqwidth", tt))
	tt.h <- as.integer(tkwinfo("reqheight", tt))
	tt.x <- as.integer(.cdtEnv$tcl$data$width.scr*0.5 - tt.w*0.5)
	tt.y <- as.integer(.cdtEnv$tcl$data$height.scr*0.5 - tt.h*0.5)
	tkwm.geometry(tt, paste0('+', tt.x, '+', tt.y))
	tkwm.transient(tt)
	tkwm.title(tt, 'Water Balance Setting')
	tkwm.deiconify(tt)

	tkfocus(tt)
	tkbind(tt, "<Destroy>", function(){
		tkgrab.release(tt)
		tkfocus(.cdtEnv$tcl$main$win)
	})
	tkwait.window(tt)
	return(Parameters)
}
