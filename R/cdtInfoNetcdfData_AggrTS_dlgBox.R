
AggregateTS_ncdfData <- function(parent.win, ncDIR,
								tstep = .cdtEnv$tcl$lang$global[['combobox']][['1']][1])
{
	listOpenFiles <- openFile_ttkcomboList()
	largeur1 <- if(WindowsOS()) 32 else 25

	xml.dlg <- file.path(.cdtDir$dirLocal, "languages", "cdtInfoNetcdfData_AggrTS_dlgBox.xml")
	lang.dlg <- cdtLanguageParse(xml.dlg, .cdtData$Config$lang.iso)

	###################

	tt1 <- tktoplevel()
	tkgrab.set(tt1)
	tkfocus(tt1)

	frMRG0 <- tkframe(tt1, relief = 'raised', borderwidth = 2)
	frMRG1 <- tkframe(tt1)

	###################

	frDate <- tkframe(frMRG0, relief = 'sunken', borderwidth = 2)

	istart.yrs <- tclVar(.cdtData$GalParams$Date.Range$start.year)
	istart.mon <- tclVar(.cdtData$GalParams$Date.Range$start.mon)
	istart.day <- tclVar(.cdtData$GalParams$Date.Range$start.day)
	iend.yrs <- tclVar(.cdtData$GalParams$Date.Range$end.year)
	iend.mon <- tclVar(.cdtData$GalParams$Date.Range$end.mon)
	iend.day <- tclVar(.cdtData$GalParams$Date.Range$end.day)

	TSTEPVAL0 <- .cdtEnv$tcl$lang$global[['combobox']][['1']][2:5]

	txtdek <- if(tstep == TSTEPVAL0[3]) 'Dek'
			  else if(tstep == TSTEPVAL0[2]) 'Pen'
			  else 'Day'
	day.txtVar <- tclVar(txtdek)
	stateDay <- if(tstep == TSTEPVAL0[4]) "disabled" else "normal"

	frtxtDate <- ttklabelframe(frDate, text = lang.dlg[['label']][['1']], relief = 'groove')

	txt.deb <- tklabel(frtxtDate, text = lang.dlg[['label']][['2']], anchor = 'e', justify = 'right')
	txt.fin <- tklabel(frtxtDate, text = lang.dlg[['label']][['3']], anchor = 'e', justify = 'right')
	txt.yrs <- tklabel(frtxtDate, text = lang.dlg[['label']][['4']])
	txt.mon <- tklabel(frtxtDate, text = lang.dlg[['label']][['5']])
	txt.day <- tklabel(frtxtDate, text = tclvalue(day.txtVar), textvariable = day.txtVar)
	en.yrs1 <- tkentry(frtxtDate, width = 4, textvariable = istart.yrs, justify = "right")
	en.mon1 <- tkentry(frtxtDate, width = 4, textvariable = istart.mon, justify = "right")
	en.day1 <- tkentry(frtxtDate, width = 4, textvariable = istart.day, justify = "right", state = stateDay)
	en.yrs2 <- tkentry(frtxtDate, width = 4, textvariable = iend.yrs, justify = "right")
	en.mon2 <- tkentry(frtxtDate, width = 4, textvariable = iend.mon, justify = "right")
	en.day2 <- tkentry(frtxtDate, width = 4, textvariable = iend.day, justify = "right", state = stateDay)

	tkgrid(txt.deb, row = 1, column = 0, sticky = 'ew', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
	tkgrid(txt.fin, row = 2, column = 0, sticky = 'ew', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
	tkgrid(txt.yrs, row = 0, column = 1, rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
	tkgrid(txt.mon, row = 0, column = 2, rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
	tkgrid(txt.day, row = 0, column = 3, rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
	tkgrid(en.yrs1, row = 1, column = 1, rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
	tkgrid(en.mon1, row = 1, column = 2, rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
	tkgrid(en.day1, row = 1, column = 3, rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
	tkgrid(en.yrs2, row = 2, column = 1, rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
	tkgrid(en.mon2, row = 2, column = 2, rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
	tkgrid(en.day2, row = 2, column = 3, rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)

	tkgrid(frtxtDate, row = 0, column = 0, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 3, ipadx = 1, ipady = 1)

	helpWidget(frtxtDate, lang.dlg[['tooltip']][['1']], lang.dlg[['status']][['1']])

	###################

	frFF <- tkframe(frMRG0, relief = "sunken", borderwidth = 2)

	inrfeff <- tclVar(.cdtData$GalParams$cdtnetcdf$format)
	rfesample <- tclVar(.cdtData$GalParams$cdtnetcdf$sample)

	txt.ncsample <- tklabel(frFF, text = lang.dlg[['label']][['6']], anchor = 'w', justify = 'left')
	cb.ncsample <- ttkcombobox(frFF, values = unlist(listOpenFiles), textvariable = rfesample, width = largeur1)
	bt.ncsample <- tkbutton(frFF, text = "...")
	txt.inrfeff <- tklabel(frFF, text = lang.dlg[['label']][['7']], anchor = 'w', justify = 'left')
	en.inrfeff <- tkentry(frFF, textvariable = inrfeff, width = largeur1)

	###################

	tkconfigure(bt.ncsample, command = function(){
		initialdir <- if(file.exists(ncDIR)) ncDIR else getwd()
		nc.opfiles <- getOpenNetcdf(parent.win, initialdir = initialdir)
		if(!is.null(nc.opfiles)){
			update.OpenFiles('netcdf', nc.opfiles)
			listOpenFiles[[length(listOpenFiles) + 1]] <<- nc.opfiles[[1]]
			tclvalue(rfesample) <- nc.opfiles[[1]]
			tkconfigure(cb.ncsample, values = unlist(listOpenFiles))
		}
	})

	###################

	tkgrid(txt.ncsample, row = 0, column = 0, sticky = 'we', rowspan = 1, columnspan = 5, padx = 1, pady = 1, ipadx = 1, ipady = 1)
	tkgrid(cb.ncsample, row = 1, column = 0, sticky = 'we', rowspan = 1, columnspan = 4, padx = 0, pady = 1, ipadx = 1, ipady = 1)
	tkgrid(bt.ncsample, row = 1, column = 4, sticky = 'w', rowspan = 1, columnspan = 1, padx = 0, pady = 1, ipadx = 1, ipady = 1)
	tkgrid(txt.inrfeff, row = 2, column = 0, sticky = 'we', rowspan = 1, columnspan = 5, padx = 1, pady = 1, ipadx = 1, ipady = 1)
	tkgrid(en.inrfeff, row = 3, column = 0, sticky = 'we', rowspan = 1, columnspan = 5, padx = 1, pady = 1, ipadx = 1, ipady = 1)

	ddk <- if(tstep %in% TSTEPVAL0[2:3]) 1 else '01'
	example <- do.call(sprintf, c(list(fmt = .cdtData$GalParams$cdtnetcdf$format),
				as.list(c(1981, '01', ddk)[seq(length(gregexpr('%s', .cdtData$GalParams$cdtnetcdf$format)[[1]]))])))

	status.bar.display(cb.ncsample, lang.dlg[['status']][['2']])
	infobulle(bt.ncsample, lang.dlg[['tooltip']][['2']])
	helpWidget(en.inrfeff, paste(lang.dlg[['tooltip']][['3']], example),
							paste(lang.dlg[['status']][['3']], example))

	###################

	tkgrid(frDate, row = 0, column = 0, sticky = '', padx = 1, pady = 1)
	tkgrid(frFF, row = 1, column = 0, sticky = 'ew', padx = 1, pady = 1)

	################################

	bt.prm.OK <- ttkbutton(frMRG1, text = .cdtEnv$tcl$lang$global[['button']][['1']])
	bt.prm.CA <- ttkbutton(frMRG1, text = .cdtEnv$tcl$lang$global[['button']][['2']])

	tkconfigure(bt.prm.OK, command = function(){
		if(str_trim(tclvalue(rfesample)) == ""){
			tkmessageBox(message = lang.dlg[['message']][['1']], icon = "warning", type = "ok")
			tkwait.window(tt1)
		}else{
			.cdtData$GalParams$cdtnetcdf$format <- str_trim(tclvalue(inrfeff))
			.cdtData$GalParams$cdtnetcdf$sample <- str_trim(tclvalue(rfesample))

			.cdtData$GalParams$Date.Range$start.year <- as.numeric(str_trim(tclvalue(istart.yrs)))
			.cdtData$GalParams$Date.Range$start.mon <- as.numeric(str_trim(tclvalue(istart.mon)))
			.cdtData$GalParams$Date.Range$start.day <- as.numeric(str_trim(tclvalue(istart.day)))
			.cdtData$GalParams$Date.Range$end.year <- as.numeric(str_trim(tclvalue(iend.yrs)))
			.cdtData$GalParams$Date.Range$end.mon <- as.numeric(str_trim(tclvalue(iend.mon)))
			.cdtData$GalParams$Date.Range$end.day <- as.numeric(str_trim(tclvalue(iend.day)))

			lenS <- length(gregexpr('%s', .cdtData$GalParams$cdtnetcdf$format)[[1]])
			if((tstep %in% TSTEPVAL0[1:3] & lenS != 3) |
				(tstep == TSTEPVAL0[4] & lenS != 2))
			{
				tkmessageBox(message = lang.dlg[['message']][['2']], icon = "warning", type = "ok")
				tkwait.window(tt1)
			}

			tkgrab.release(tt1)
			tkdestroy(tt1)
			tkfocus(parent.win)
		}
	})

	tkconfigure(bt.prm.CA, command = function(){
		tkgrab.release(tt1)
		tkdestroy(tt1)
		tkfocus(parent.win)
	})

	tkgrid(bt.prm.OK, row = 0, column = 0, sticky = 'w', padx = 5, pady = 1, ipadx = 1, ipady = 1)
	tkgrid(bt.prm.CA, row = 0, column = 1, sticky = 'e', padx = 5, pady = 1, ipadx = 1, ipady = 1)

	################################
	tkgrid(frMRG0, row = 0, column = 0, sticky = 'nswe', rowspan = 1, columnspan = 2, padx = 1, pady = 1, ipadx = 1, ipady = 1)
	tkgrid(frMRG1, row = 1, column = 1, sticky = 'se', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)

	tkwm.withdraw(tt1)
	tcl('update')
	tt.w <- as.integer(tkwinfo("reqwidth", tt1))
	tt.h <- as.integer(tkwinfo("reqheight", tt1))
	tt.x <- as.integer(.cdtEnv$tcl$data$width.scr*0.5 - tt.w*0.5)
	tt.y <- as.integer(.cdtEnv$tcl$data$height.scr*0.5 - tt.h*0.5)
	tkwm.geometry(tt1, paste0('+', tt.x, '+', tt.y))
	tkwm.transient(tt1)
	tkwm.title(tt1, lang.dlg[['title']])
	tkwm.deiconify(tt1)

	tkfocus(tt1)
	tkbind(tt1, "<Destroy>", function(){
		tkgrab.release(tt1)
		tkfocus(parent.win)
	})
	tkwait.window(tt1)
}
