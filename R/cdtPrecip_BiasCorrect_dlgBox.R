
rmvBiasGetInfoRain <- function(parent.win, GeneralParameters){
	listOpenFiles <- openFile_ttkcomboList()
	if(WindowsOS()){
		largeur0 <- 19
		largeur1 <- 42
		largeur2 <- 45
		largeur3 <- 27
	}else{
		largeur0 <- 16
		largeur1 <- 38
		largeur2 <- 39
		largeur3 <- 21
	}

	# xml.dlg <- file.path(.cdtDir$dirLocal, "languages", "cdtPrecip_BiasCorrect_dlgBox.xml")
	# lang.dlg <- cdtLanguageParse(xml.dlg, .cdtData$Config$lang.iso)

	####################################

	tt <- tktoplevel()
	tkgrab.set(tt)
	tkfocus(tt)

	frMRG0 <- tkframe(tt, relief = 'raised', borderwidth = 2)
	frMRG1 <- tkframe(tt)

	############################################

	frtimestep <- tkframe(frMRG0, relief = 'sunken', borderwidth = 2, padx = 3, pady = 3)

	file.period <- tclVar()
	CbperiodVAL <- .cdtEnv$tcl$lang$global[['combobox']][['1']][2:5]
	periodVAL <- c('daily', 'pentad', 'dekadal', 'monthly')
	tclvalue(file.period) <- CbperiodVAL[periodVAL %in% .cdtData$GalParams$period]

	txtdek <- switch(.cdtData$GalParams$period, 'dekadal' = 'Dekad', 'pentad' = 'Pentad', 'Day')
	day.txtVar <- tclVar(txtdek)
	statedate <- if(.cdtData$GalParams$period == 'monthly') 'disabled' else 'normal'

	cb.period <- ttkcombobox(frtimestep, values = CbperiodVAL, textvariable = file.period, width = largeur0)
	bt.DateRange <- ttkbutton(frtimestep, text = "Set Date Range")

	tkconfigure(bt.DateRange, command = function(){
		.cdtData$GalParams[["Adjust.Date"]] <- getInfoDateRange(.cdtEnv$tcl$main$win,
												.cdtData$GalParams[["Adjust.Date"]],
												daypendek.lab = tclvalue(day.txtVar),
												state.dek = statedate)
	})

	tkgrid(cb.period, row = 0, column = 0, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
	tkgrid(bt.DateRange, row = 0, column = 1, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)

	infobulle(cb.period, 'Select the time step of the data')
	status.bar.display(cb.period, 'Select the time step of the data')
	infobulle(bt.DateRange, 'Set the start and end date of data to correct')
	status.bar.display(bt.DateRange, 'Set the start and end date of data to correct')

	###########

	tkbind(cb.period, "<<ComboboxSelected>>", function(){
		tclvalue(day.txtVar) <- ifelse(str_trim(tclvalue(file.period)) == CbperiodVAL[3], 'Dekad',
								ifelse(str_trim(tclvalue(file.period)) == CbperiodVAL[2], 'Pentad', 'Day'))
		statedate <<- if(str_trim(tclvalue(file.period)) == CbperiodVAL[4]) 'disabled' else 'normal'
	})

	############################################

	frameBias <- tkframe(frMRG0, relief = 'sunken', borderwidth = 2, padx = 3, pady = 3)

	cb.biasMthd <- c("Quantile.Mapping", "Multiplicative.Bias.Var", "Multiplicative.Bias.Mon")
	bias.method <- tclVar(str_trim(.cdtData$GalParams$BIAS$bias.method))
	bias.dir <- tclVar(.cdtData$GalParams$BIAS$dir.Bias)

	txt.bias <- tklabel(frameBias, text = 'Bias method', anchor = 'e', justify = 'right')
	cb.bias <- ttkcombobox(frameBias, values = cb.biasMthd, textvariable = bias.method, width = largeur3)

	txt.bias.dir <- tklabel(frameBias, text = "Directory of bias files", anchor = 'w', justify = 'left')
	en.bias.dir <- tkentry(frameBias, textvariable = bias.dir, width = largeur2)
	bt.bias.dir <- tkbutton(frameBias, text = "...")

	tkconfigure(bt.bias.dir, command = function(){
		dirbias <- tk_choose.dir(getwd(), "")
		tclvalue(bias.dir) <- if(!is.na(dirbias)) dirbias else ""
	})

	tkgrid(txt.bias, row = 0, column = 0, sticky = 'e', rowspan = 1, columnspan = 2, padx = 1, pady = 5, ipadx = 1, ipady = 1)
	tkgrid(cb.bias, row = 0, column = 2, sticky = 'we', rowspan = 1, columnspan = 3, padx = 1, pady = 5, ipadx = 1, ipady = 1)

	tkgrid(txt.bias.dir, row = 1, column = 0, sticky = 'we', rowspan = 1, columnspan = 6, padx = 1, pady = 1, ipadx = 1, ipady = 1)
	tkgrid(en.bias.dir, row = 2, column = 0, sticky = 'we', rowspan = 1, columnspan = 5, padx = 0, pady = 1, ipadx = 1, ipady = 1)
	tkgrid(bt.bias.dir, row = 2, column = 5, sticky = 'w', rowspan = 1, columnspan = 1, padx = 0, pady = 1, ipadx = 1, ipady = 1)

	infobulle(cb.bias, 'Select the method used to calculate the Bias Factors or Parameters')
	status.bar.display(cb.bias, 'Select the method used to calculate the Bias Factors or Parameters')
	infobulle(en.bias.dir, 'Enter the full path to directory containing the bias files')
	status.bar.display(en.bias.dir, 'Enter the full path to directory containing the bias files')

	############################################

	frameRFE <- tkframe(frMRG0, relief = 'sunken', borderwidth = 2, padx = 3, pady = 3)

	dir.RFE <- tclVar(.cdtData$GalParams$RFE$dir)

	txt.RFE <- tklabel(frameRFE, text = 'Directory containing RFE data', anchor = 'w', justify = 'left')
	set.RFE <- ttkbutton(frameRFE, text = .cdtEnv$tcl$lang$global[['button']][['5']])
	en.RFE <- tkentry(frameRFE, textvariable = dir.RFE, width = largeur2)
	bt.RFE <- tkbutton(frameRFE, text = "...")

	######

	tkconfigure(set.RFE, command = function(){
		.cdtData$GalParams[["RFE"]] <- getInfoNetcdfData(tt, .cdtData$GalParams[["RFE"]],
														str_trim(tclvalue(dir.RFE)), str_trim(tclvalue(file.period)))
	})

	tkconfigure(bt.RFE, command = function(){
		dirrfe <- tk_choose.dir(getwd(), "")
		tclvalue(dir.RFE) <- if(!is.na(dirrfe)) dirrfe else ""
	})

	######
	tkgrid(txt.RFE, row = 0, column = 0, sticky = 'we', rowspan = 1, columnspan = 3, padx = 1, pady = 0, ipadx = 1, ipady = 1)
	tkgrid(set.RFE, row = 0, column = 3, sticky = 'we', rowspan = 1, columnspan = 2, padx = 1, pady = 0, ipadx = 1, ipady = 1)
	tkgrid(en.RFE, row = 1, column = 0, sticky = 'we', rowspan = 1, columnspan = 4, padx = 0, pady = 0, ipadx = 1, ipady = 1)
	tkgrid(bt.RFE, row = 1, column = 4, sticky = 'w', rowspan = 1, columnspan = 1, padx = 0, pady = 0, ipadx = 1, ipady = 1)

	infobulle(en.RFE, 'Enter the full path to the directory containing the RFE data')
	status.bar.display(en.RFE, 'Enter the full path to the directory containing the RFE data')
	infobulle(bt.RFE, 'Or browse here')
	status.bar.display(bt.RFE, 'Or browse here')
	infobulle(set.RFE, 'Setting netcdf data options')
	status.bar.display(set.RFE, 'Setting netcdf data options')

	############################################

	frSave <- tkframe(frMRG0, relief = 'sunken', borderwidth = 2, padx = 3, pady = 3)

	dir2save <- tclVar(.cdtData$GalParams$output$dir)
	outmrgff <- tclVar(.cdtData$GalParams$output$format)

	txt.dir2save <- tklabel(frSave, text = 'Directory to save result', anchor = 'w', justify = 'left')
	en.dir2save <- tkentry(frSave, textvariable = dir2save, width = largeur2)
	bt.dir2save <- tkbutton(frSave, text = "...")
	txt.outmrgff <- tklabel(frSave, text = 'Adjusted data filename format', anchor = 'w', justify = 'left')
	en.outmrgff <- tkentry(frSave, textvariable = outmrgff, width = largeur2)

	#####

	tkconfigure(bt.dir2save, command = function(){
		dir2savepth <- tk_choose.dir(.cdtData$GalParams$output$dir, "")
		if(is.na(dir2savepth)) tclvalue(dir2save) <- .cdtData$GalParams$output$dir
		else{
			dir.create(dir2savepth, showWarnings = FALSE, recursive = TRUE)
			tclvalue(dir2save) <- dir2savepth
		}
	})

	#####

	tkgrid(txt.dir2save, row = 0, column = 0, sticky = 'we', rowspan = 1, columnspan = 2, padx = 1, pady = 2, ipadx = 1, ipady = 1)
	tkgrid(en.dir2save, row = 1, column = 0, sticky = 'we', rowspan = 1, columnspan = 1, padx = 0, pady = 1, ipadx = 1, ipady = 1)
	tkgrid(bt.dir2save, row = 1, column = 1, sticky = 'w', rowspan = 1, columnspan = 1, padx = 0, pady = 1, ipadx = 1, ipady = 1)
	tkgrid(txt.outmrgff, row = 2, column = 0, sticky = 'we', rowspan = 1, columnspan = 2, padx = 1, pady = 1, ipadx = 1, ipady = 1)
	tkgrid(en.outmrgff, row = 3, column = 0, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)

	infobulle(en.dir2save, 'Enter the full path to directory to save result')
	status.bar.display(en.dir2save, 'Enter the full path to directory to save result')
	infobulle(bt.dir2save, 'or browse here')
	status.bar.display(bt.dir2save, 'or browse here')
	infobulle(en.outmrgff, 'Format of the adjusted RFE filenames in NetCDF, example: rr_adj_1983011.nc')
	status.bar.display(en.outmrgff, 'Format of the adjusted RFE filenames in NetCDF, example: rr_adj_1983011.nc')

	############################################
	tkgrid(frtimestep, row = 0, column = 0, sticky = '', padx = 1, pady = 1, ipadx = 1, ipady = 1)
	tkgrid(frameBias, row = 1, column = 0, sticky = 'we', padx = 1, pady = 1, ipadx = 1, ipady = 1)
	tkgrid(frameRFE, row = 2, column = 0, sticky = 'we', padx = 1, pady = 1, ipadx = 1, ipady = 1)
	tkgrid(frSave, row = 3, column = 0, sticky = 'we', padx = 1, pady = 1, ipadx = 1, ipady = 1)

	############################################

	bt.prm.OK <- ttkbutton(frMRG1, text = .cdtEnv$tcl$lang$global[['button']][['1']])
	bt.prm.CA <- ttkbutton(frMRG1, text = .cdtEnv$tcl$lang$global[['button']][['2']])

	#######
	tkconfigure(bt.prm.OK, command = function(){
		if(str_trim(tclvalue(dir.RFE)) %in% c("", "NA")){
			tkmessageBox(message = "Select or enter the  directory containing the RFE files", icon = "warning", type = "ok")
			tkwait.window(tt)
		}else if(str_trim(tclvalue(bias.dir)) %in% c("", "NA")){
			tkmessageBox(message = "Enter the path to directory containing the Bias factors", icon = "warning", type = "ok")
			tkwait.window(tt)
		}else if(str_trim(tclvalue(dir2save)) %in% c("", "NA")){
			tkmessageBox(message = "Browse or enter the path to directory to save results", icon = "warning", type = "ok")
			tkwait.window(tt)
		}else{
			.cdtData$GalParams$RFE$dir <- str_trim(tclvalue(dir.RFE))
			.cdtData$GalParams$BIAS$bias.method <- str_trim(tclvalue(bias.method))
			.cdtData$GalParams$BIAS$dir.Bias <- str_trim(tclvalue(bias.dir))

			.cdtData$GalParams$period <- periodVAL[CbperiodVAL %in% str_trim(tclvalue(file.period))]

			.cdtData$GalParams$output$dir <- str_trim(tclvalue(dir2save))
			.cdtData$GalParams$output$format <- str_trim(tclvalue(outmrgff))

			tkgrab.release(tt)
			tkdestroy(tt)
			tkfocus(.cdtEnv$tcl$main$win)
		}
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

	############################################

	tkwm.withdraw(tt)
	tcl('update')
	tt.w <- as.integer(tkwinfo("reqwidth", tt))
	tt.h <- as.integer(tkwinfo("reqheight", tt))
	tt.x <- as.integer(.cdtEnv$tcl$data$width.scr*0.5 - tt.w*0.5)
	tt.y <- as.integer(.cdtEnv$tcl$data$height.scr*0.5 - tt.h*0.5)
	tkwm.geometry(tt, paste0('+', tt.x, '+', tt.y))
	tkwm.transient(tt)
	tkwm.title(tt, 'Bias Correction - Settings')
	tkwm.deiconify(tt)

	tkfocus(tt)
	tkbind(tt, "<Destroy>", function(){
		tkgrab.release(tt)
		tkfocus(.cdtEnv$tcl$main$win)
	})
	tkwait.window(tt)
	invisible()
}
