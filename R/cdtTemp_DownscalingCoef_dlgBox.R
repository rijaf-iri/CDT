
Temp_coefDownGetInfo <- function(){
	listOpenFiles <- openFile_ttkcomboList()
	if(WindowsOS()){
		largeur <- 38
		largeur1 <- 35
		largeur2 <- 21
	}else{
		largeur <- 34
		largeur1 <- 33
		largeur2 <- 18
	}

	# xml.dlg <- file.path(.cdtDir$dirLocal, "languages", "cdtTemp_DownscalingCoef_dlgBox.xml")
	# lang.dlg <- cdtLanguageParse(xml.dlg, .cdtData$Config$lang.iso)

	############################################

	tt <- tktoplevel()
	tkgrab.set(tt)
	tkfocus(tt)

	frMRG0 <- tkframe(tt, relief = 'raised', borderwidth = 2)
	frMRG1 <- tkframe(tt)
	frLeft <- tkframe(frMRG0, relief = "groove", borderwidth = 2)

	############################################
	frSTN <- tkframe(frLeft, relief = 'sunken', borderwidth = 2)

	file.stnfl <- tclVar(.cdtData$GalParams$IO.files$STN.file)

	txt.stnfl <- tklabel(frSTN, text = 'Station data file', anchor = 'w', justify = 'left')
	cb.stnfl <- ttkcombobox(frSTN, values = unlist(listOpenFiles), textvariable = file.stnfl, width = largeur1)
	bt.stnfl <- tkbutton(frSTN, text = "...")

	######
	tkconfigure(bt.stnfl, command = function(){
		dat.opfiles <- getOpenFiles(tt)
		if(!is.null(dat.opfiles)){
			update.OpenFiles('ascii', dat.opfiles)
			listOpenFiles[[length(listOpenFiles) + 1]] <<- dat.opfiles[[1]]
			tclvalue(file.stnfl) <- dat.opfiles[[1]]
			lapply(list(cb.stnfl, cb.grddem), tkconfigure, values = unlist(listOpenFiles))
		}
	})

	tkgrid(txt.stnfl, row = 0, column = 0, sticky = 'we', rowspan = 1, columnspan = 2, padx = 1, pady = 0, ipadx = 1, ipady = 1)
	tkgrid(cb.stnfl, row = 1, column = 0, sticky = 'we', rowspan = 1, columnspan = 1, padx = 0, pady = 0, ipadx = 1, ipady = 1)
	tkgrid(bt.stnfl, row = 1, column = 1, sticky = 'w', rowspan = 1, columnspan = 1, padx = 0, pady = 0, ipadx = 1, ipady = 1)

	infobulle(cb.stnfl, 'Choose the file in the list')
	status.bar.display(cb.stnfl, 'Choose the file containing the gauge data')
	infobulle(bt.stnfl, 'Browse file if not listed')
	status.bar.display(bt.stnfl, 'Browse file if not listed')

	############################################

	frDEM <- tkframe(frLeft, relief = 'sunken', borderwidth = 2)

	file.grddem <- tclVar(.cdtData$GalParams$IO.files$DEM.file)

	txt.grddem <- tklabel(frDEM, text = "Elevation data (NetCDF)", anchor = 'w', justify = 'left')
	cb.grddem <- ttkcombobox(frDEM, values = unlist(listOpenFiles), textvariable = file.grddem, width = largeur1)
	bt.grddem <- tkbutton(frDEM, text = "...")

	####
	tkconfigure(bt.grddem, command = function(){
		nc.opfiles <- getOpenNetcdf(tt, initialdir = getwd())
		if(!is.null(nc.opfiles)){
			update.OpenFiles('netcdf', nc.opfiles)
			listOpenFiles[[length(listOpenFiles) + 1]] <<- nc.opfiles[[1]]
			tclvalue(file.grddem) <- nc.opfiles[[1]]
			lapply(list(cb.stnfl, cb.grddem), tkconfigure, values = unlist(listOpenFiles))
		}
	})

	#####
	tkgrid(txt.grddem, row = 0, column = 0, sticky = 'we', rowspan = 1, columnspan = 2, padx = 1, pady = 0, ipadx = 1, ipady = 1)
	tkgrid(cb.grddem, row = 1, column = 0, sticky = 'we', rowspan = 1, columnspan = 1, padx = 0, pady = 0, ipadx = 1, ipady = 1)
	tkgrid(bt.grddem, row = 1, column = 1, sticky = 'w', rowspan = 1, columnspan = 1, padx = 0, pady = 0, ipadx = 1, ipady = 1)

	infobulle(cb.grddem, 'Select the file from the list')
	status.bar.display(cb.grddem, 'File containing the elevation data in netcdf')
	infobulle(bt.grddem, 'Browse file if not listed')
	status.bar.display(bt.grddem, 'Browse file if not listed')

	############################################

	frDate <- tkframe(frLeft, relief = 'sunken', borderwidth = 2)

	file.period <- tclVar()
	CbperiodVAL <- .cdtEnv$tcl$lang$global[['combobox']][['1']][2:5]
	periodVAL <- c('daily', 'pentad', 'dekadal', 'monthly')
	tclvalue(file.period) <- CbperiodVAL[periodVAL %in% .cdtData$GalParams$period]

	cb.period <- ttkcombobox(frDate, values = CbperiodVAL, textvariable = file.period, width = largeur2)
	bt.BasePeriod <- ttkbutton(frDate, text = "Set Base Period")

	tkconfigure(bt.BasePeriod, command = function(){
		.cdtData$GalParams[["Down.Date.Range"]] <- getInfoBasePeriod(tt, .cdtData$GalParams[["Down.Date.Range"]])
	})

	tkgrid(cb.period, row = 0, column = 0, sticky = 'we', rowspan = 1, columnspan = 1, padx = 3, pady = 5, ipadx = 1, ipady = 1)
	tkgrid(bt.BasePeriod, row = 0, column = 1, sticky = 'we', rowspan = 1, columnspan = 1, padx = 3, pady = 5, ipadx = 5, ipady = 1)

	infobulle(cb.period, 'Select the time step of the data')
	status.bar.display(cb.period, 'Select the time step of the data')
	infobulle(bt.BasePeriod, 'Base period to be used to compute regression parameters between station temperature and elevation')
	status.bar.display(bt.BasePeriod, 'Base period to be used to compute regression parameters between station temperature and elevation')

	############################################

	frSave <- tkframe(frLeft, relief = 'sunken', borderwidth = 2)

	file.save1 <- tclVar(.cdtData$GalParams$IO.files$dir2save)

	txt.file.save <- tklabel(frSave, text = 'Directory to save result', anchor = 'w', justify = 'left')
	en.file.save <- tkentry(frSave, textvariable = file.save1, width = largeur)
	bt.file.save <- tkbutton(frSave, text = "...")

	#####

	tkconfigure(bt.file.save, command = function(){
		file2save1 <- tk_choose.dir(.cdtData$GalParams$IO.files$dir2save, "")
		if(is.na(file2save1)) tclvalue(file.save1) <- .cdtData$GalParams$IO.files$dir2save
		else{
			dir.create(file2save1, showWarnings = FALSE, recursive = TRUE)
			tclvalue(file.save1) <- file2save1
		}
	})

	#####

	tkgrid(txt.file.save, row = 0, column = 0, sticky = 'we', rowspan = 1, columnspan = 2, padx = 1, pady = 0, ipadx = 1, ipady = 1)
	tkgrid(en.file.save, row = 1, column = 0, sticky = 'we', rowspan = 1, columnspan = 1, padx = 0, pady = 0, ipadx = 1, ipady = 1)
	tkgrid(bt.file.save, row = 1, column = 1, sticky = 'w', rowspan = 1, columnspan = 1, padx = 0, pady = 0, ipadx = 1, ipady = 1)

	infobulle(en.file.save, 'Enter the full path to directory to save result')
	status.bar.display(en.file.save, 'Enter the full path to directory to save result')
	infobulle(bt.file.save, 'or browse here')
	status.bar.display(bt.file.save, 'or browse here')

	############################################
	tkgrid(frSTN, row = 0, column = 0, sticky = 'we', padx = 1, pady = 1, ipadx = 1, ipady = 1)
	tkgrid(frDEM, row = 1, column = 0, sticky = 'we', padx = 1, pady = 1, ipadx = 1, ipady = 1)
	tkgrid(frDate, row = 2, column = 0, sticky = '', padx = 1, pady = 1, ipadx = 1, ipady = 1)
	tkgrid(frSave, row = 3, column = 0, sticky = 'we', padx = 1, pady = 1, ipadx = 1, ipady = 1)

	############################################
	
	tkgrid(frLeft, row = 0, column = 0, sticky = 'news', padx = 5, pady = 1, ipadx = 1, ipady = 1)

	############################################

	bt.prm.OK <- ttkbutton(frMRG1, text = .cdtEnv$tcl$lang$global[['button']][['1']])
	bt.prm.CA <- ttkbutton(frMRG1, text = .cdtEnv$tcl$lang$global[['button']][['2']])

	tkconfigure(bt.prm.OK, command = function(){
		if(str_trim(tclvalue(file.stnfl)) == ""){
			tkmessageBox(message = "Select the file containing the gauge data", icon = "warning", type = "ok")
		}else if(str_trim(tclvalue(file.grddem)) == "" ){
			tkmessageBox(message = "You have to choose DEM data in NetCDF format", icon = "warning", type = "ok")
			tkwait.window(tt)
		}else if(str_trim(tclvalue(file.save1)) %in% c("", "NA")){
			tkmessageBox(message = "Select or enter the path to directory to save results", icon = "warning", type = "ok")
			tkwait.window(tt)
		}else{
			.cdtData$GalParams$period <- periodVAL[CbperiodVAL %in% str_trim(tclvalue(file.period))]

			.cdtData$GalParams$IO.files$STN.file <- str_trim(tclvalue(file.stnfl))
			.cdtData$GalParams$IO.files$DEM.file <- str_trim(tclvalue(file.grddem))
			.cdtData$GalParams$IO.files$dir2save <- str_trim(tclvalue(file.save1))

			# .cdtData$GalParams$message <- lang.dlg[['message']]

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

	############################3
	tkwm.withdraw(tt)
	tcl('update')
	tt.w <- as.integer(tkwinfo("reqwidth", tt))
	tt.h <- as.integer(tkwinfo("reqheight", tt))
	tt.x <- as.integer(.cdtEnv$tcl$data$width.scr*0.5 - tt.w*0.5)
	tt.y <- as.integer(.cdtEnv$tcl$data$height.scr*0.5 - tt.h*0.5)
	tkwm.geometry(tt, paste0('+', tt.x, '+', tt.y))
	tkwm.transient(tt)
	tkwm.title(tt, 'Coefficients Downscaling-Settings')
	tkwm.deiconify(tt)

	tkfocus(tt)
	tkbind(tt, "<Destroy>", function(){
		tkgrab.release(tt)
		tkfocus(.cdtEnv$tcl$main$win)
	})
	tkwait.window(tt)
}
