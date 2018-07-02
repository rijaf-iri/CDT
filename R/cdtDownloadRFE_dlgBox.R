
download_RFE <- function(){
	if(WindowsOS()){
		largeur <- 36
		largeur1 <- 24
	}else{
		largeur <- 30
		largeur1 <- 24
	}

	xml.dlg <- file.path(.cdtDir$dirLocal, "languages", "cdtDownloadRFE_dlgBox.xml")
	lang.dlg <- cdtLanguageParse(xml.dlg, .cdtData$Config$lang.iso)

	#####################
	tt <- tktoplevel()
	tkgrab.set(tt)
	tkfocus(tt)

	frA <- tkframe(tt)
	frB <- tkframe(tt)

	#####################
	frA1 <- tkframe(frA, relief = 'sunken', borderwidth = 2)

	dekadRFE <- c('10-DAYS TAMSATv3', '10-DAYS TAMSATv2', '10-DAYS CHIRPSv2.0', '10-DAYS CHIRPv1.0')
	dailyRFE <- c('DAILY TAMSATv3', 'DAILY TAMSATv2', 'DAILY CHIRPSv2.0')

	fileSource <- tclVar(str_trim(.cdtData$GalParams$rfe.data))
	rfeChoix <- c(dekadRFE, '------------------', dailyRFE)

	cb.period <- ttkcombobox(frA1, values = rfeChoix, textvariable = fileSource, width = largeur1)

	######
	tkgrid(cb.period, row = 0, column = 0, sticky = 'we', padx = 1, pady = 3, ipadx = 1, ipady = 1)

	helpWidget(cb.period, lang.dlg[['tooltip']][['1']], lang.dlg[['status']][['1']])

	######
	tkbind(cb.period, "<<ComboboxSelected>>", function(){
		if(tclvalue(fileSource) %in% dekadRFE){
			tclvalue(daytext) <- 'Dek'
			tclvalue(iend.day) <- '3'
		}else{
			tclvalue(daytext) <- 'Day'
			tclvalue(iend.day) <- '31'
		}
	})

	######
	frA2 <- tkframe(frA, relief = 'sunken', borderwidth = 2)

	istart.yrs <- tclVar(.cdtData$GalParams$date$year1)
	istart.mon <- tclVar(.cdtData$GalParams$date$mon1)
	istart.day <- tclVar(.cdtData$GalParams$date$day1)
	iend.yrs <- tclVar(.cdtData$GalParams$date$year2)
	iend.mon <- tclVar(.cdtData$GalParams$date$mon2)
	iend.day <- tclVar(.cdtData$GalParams$date$day2)
	daytext <- tclVar("Dek")

	deb.txt <- tklabel(frA2, text = lang.dlg[['label']][['1']], anchor = 'e', justify = 'right')
	fin.txt <- tklabel(frA2, text = lang.dlg[['label']][['2']], anchor = 'e', justify = 'right')
	yrs.txt <- tklabel(frA2, text = 'Year')
	mon.txt <- tklabel(frA2, text = 'Mon')
	day.txt <- tklabel(frA2, text = tclvalue(daytext), textvariable = daytext)

	yrs1.v <- tkentry(frA2, width = 4, textvariable = istart.yrs, justify = "right")
	mon1.v <- tkentry(frA2, width = 4, textvariable = istart.mon, justify = "right")
	day1.v <- tkentry(frA2, width = 4, textvariable = istart.day, justify = "right")
	yrs2.v <- tkentry(frA2, width = 4, textvariable = iend.yrs, justify = "right")
	mon2.v <- tkentry(frA2, width = 4, textvariable = iend.mon, justify = "right")
	day2.v <- tkentry(frA2, width = 4, textvariable = iend.day, justify = "right")

	tkgrid(deb.txt, row = 1, column = 0, sticky = 'ew', padx = 1, pady = 1)
	tkgrid(fin.txt, row = 2, column = 0, sticky = 'ew', padx = 1, pady = 1)
	tkgrid(yrs.txt, row = 0, column = 1, sticky = 'ew', padx = 1, pady = 1)
	tkgrid(mon.txt, row = 0, column = 2, sticky = 'ew', padx = 1, pady = 1)
	tkgrid(day.txt, row = 0, column = 3, sticky = 'ew', padx = 1, pady = 1)
	tkgrid(yrs1.v, row = 1, column = 1, sticky = 'ew', padx = 1, pady = 1)
	tkgrid(mon1.v, row = 1, column = 2, sticky = 'ew', padx = 1, pady = 1)
	tkgrid(day1.v, row = 1, column = 3, sticky = 'ew', padx = 1, pady = 1)
	tkgrid(yrs2.v, row = 2, column = 1, sticky = 'ew', padx = 1, pady = 1)
	tkgrid(mon2.v, row = 2, column = 2, sticky = 'ew', padx = 1, pady = 1)
	tkgrid(day2.v, row = 2, column = 3, sticky = 'ew', padx = 1, pady = 1)

	helpWidget(frA2, lang.dlg[['tooltip']][['2']], lang.dlg[['status']][['2']])

	######
	frA3 <- tkframe(frA)

	btOK <- ttkbutton(frA3, text = lang.dlg[['button']][['1']])
	btCA <- ttkbutton(frA3, text = lang.dlg[['button']][['2']])

	tkconfigure(btOK, command = function(){
		if(str_trim(tclvalue(file.save1)) %in% c("", "NA")){
			tkmessageBox(message = lang.dlg[['message']][['1']], icon = "warning", type = "ok")
			tkwait.window(tt)
		}else{
			tkgrab.release(tt)
			tkdestroy(tt)
			tkfocus(.cdtEnv$tcl$main$win)

			.cdtData$GalParams$date$year1 <- as.numeric(tclvalue(istart.yrs))
			.cdtData$GalParams$date$mon1 <- as.numeric(tclvalue(istart.mon))
			.cdtData$GalParams$date$day1 <- as.numeric(tclvalue(istart.day))
			.cdtData$GalParams$date$year2 <- as.numeric(tclvalue(iend.yrs))
			.cdtData$GalParams$date$mon2 <- as.numeric(tclvalue(iend.mon))
			.cdtData$GalParams$date$day2 <- as.numeric(tclvalue(iend.day))
			.cdtData$GalParams$bbox$minlon <- as.numeric(tclvalue(minLon))
			.cdtData$GalParams$bbox$maxlon <- as.numeric(tclvalue(maxLon))
			.cdtData$GalParams$bbox$minlat <- as.numeric(tclvalue(minLat))
			.cdtData$GalParams$bbox$maxlat <- as.numeric(tclvalue(maxLat))
			.cdtData$GalParams$dir2save <- str_trim(tclvalue(file.save1))
			.cdtData$GalParams$rfe.data <- str_trim(tclvalue(fileSource))

			.cdtData$GalParams$message <- lang.dlg[['message']]
			.cdtData$GalParams$dekadRFE <- dekadRFE
			.cdtData$GalParams$dailyRFE <- dailyRFE

			.cdtData$GalParams$istart <- do.call(paste, c(.cdtData$GalParams$date[c("year1", "mon1", "day1")], sep = '-'))
			.cdtData$GalParams$iend <- do.call(paste, c(.cdtData$GalParams$date[c("year2", "mon2", "day2")], sep = '-'))

			if(testConnection()){
				Insert.Messages.Out(lang.dlg[['message']][['3']])
				ExecDownload_RFEData()
			}else{
				Insert.Messages.Out(lang.dlg[['message']][['2']], format = TRUE)
				return(NULL)
			}
		}
	})

	tkconfigure(btCA, command = function(){
		tkgrab.release(tt)
		tkdestroy(tt)
		tkfocus(.cdtEnv$tcl$main$win)
	})

	tkgrid(btCA, row = 0, column = 0, sticky = 'w', padx = 5, pady = 1, ipadx = 1, ipady = 1)
	tkgrid(btOK, row = 0, column = 1, sticky = 'e', padx = 5, pady = 1, ipadx = 1, ipady = 1)

	##################
	tkgrid(frA1, row = 0, column = 0, sticky = 'we', padx = 5, pady = 5, ipadx = 1, ipady = 1)
	tkgrid(frA2, row = 1, column = 0, sticky = 'we', padx = 5, pady = 5, ipadx = 1, ipady = 1)
	tkgrid(frA3, padx = 5, pady = 5)

	##################

	frB1 <- tkframe(frB, relief = 'sunken', borderwidth = 2)

	minLon <- tclVar(.cdtData$GalParams$bbox$minlon)
	maxLon <- tclVar(.cdtData$GalParams$bbox$maxlon)
	minLat <- tclVar(.cdtData$GalParams$bbox$minlat)
	maxLat <- tclVar(.cdtData$GalParams$bbox$maxlat)

	fr_grd <- ttklabelframe(frB1, text = lang.dlg[['label']][['3']], relief = "groove", borderwidth = 2)

	grd_llon <- tklabel(fr_grd, text = lang.dlg[['label']][['4']], anchor = 'e', justify = 'right')
	grd_llat <- tklabel(fr_grd, text = lang.dlg[['label']][['5']], anchor = 'e', justify = 'right')
	grd_lb1 <- tklabel(fr_grd, text = lang.dlg[['label']][['6']])
	grd_lb2 <- tklabel(fr_grd, text = lang.dlg[['label']][['7']])
	grd_vlon1 <- tkentry(fr_grd, width = 8, justify = "right", textvariable = minLon)
	grd_vlon2 <- tkentry(fr_grd, width = 8, justify = "right", textvariable = maxLon)
	grd_vlat1 <- tkentry(fr_grd, width = 8, justify = "right", textvariable = minLat)
	grd_vlat2 <- tkentry(fr_grd, width = 8, justify = "right", textvariable = maxLat)

	######
	tkgrid(grd_lb1, row = 0, column = 1, sticky = "ew")
	tkgrid(grd_lb2, row = 0, column = 2, sticky = "ew")
	tkgrid(grd_llon, row = 1, column = 0, sticky = "ew")
	tkgrid(grd_vlon1, row = 1, column = 1, sticky = "ew")
	tkgrid(grd_vlon2, row = 1, column = 2, sticky = "ew")
	tkgrid(grd_llat, row = 2, column = 0, sticky = "ew")
	tkgrid(grd_vlat1, row = 2, column = 1, sticky = "ew")
	tkgrid(grd_vlat2, row = 2, column = 2, sticky = "ew")

	tkgrid(fr_grd, row = 0, column = 0, sticky = 'e', padx = 1, pady = 1, ipadx = 1, ipady = 1)

	helpWidget(grd_vlon1, lang.dlg[['tooltip']][['3']], lang.dlg[['status']][['3']])
	helpWidget(grd_vlon2, lang.dlg[['tooltip']][['4']], lang.dlg[['status']][['4']])
	helpWidget(grd_vlat1, lang.dlg[['tooltip']][['5']], lang.dlg[['status']][['5']])
	helpWidget(grd_vlat2, lang.dlg[['tooltip']][['6']], lang.dlg[['status']][['6']])

	########

	frB2 <- tkframe(frB, relief = 'sunken', borderwidth = 2)

	file.save1 <- tclVar(.cdtData$GalParams$dir2save)

	txt.file.save <- tklabel(frB2, text = lang.dlg[['label']][['8']], anchor = 'w', justify = 'left')
	en.file.save <- tkentry(frB2, textvariable = file.save1, width = largeur)
	bt.file.save <- tkbutton(frB2, text = "...")

	tkconfigure(bt.file.save, command = function(){
		file2save1 <- tk_choose.dir(.cdtData$GalParams$dir2save, "")
			if(is.na(file2save1)) tclvalue(file.save1) <- .cdtData$GalParams$dir2save
			else{
				dir.create(file2save1, showWarnings = FALSE, recursive = TRUE)
				tclvalue(file.save1) <- file2save1
			}
	})

	tkgrid(txt.file.save, row = 0, column = 0, sticky = 'we', rowspan = 1, columnspan = 2, padx = 1, pady = 1, ipadx = 1, ipady = 1)
	tkgrid(en.file.save, row = 1, column = 0, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
	tkgrid(bt.file.save, row = 1, column = 1, sticky = 'w', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)

	helpWidget(en.file.save, lang.dlg[['tooltip']][['7']], lang.dlg[['status']][['7']])
	helpWidget(bt.file.save, lang.dlg[['tooltip']][['8']], lang.dlg[['status']][['8']])

	##################
	tkgrid(frB1, row = 0, column = 0, sticky = 'we', padx = 5, pady = 5, ipadx = 1, ipady = 1)
	tkgrid(frB2, row = 1, column = 0, sticky = 'we', padx = 5, pady = 5, ipadx = 1, ipady = 1)

	##########
	tkgrid(frA, row = 0, column = 0, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
	tkgrid(frB, row = 0, column = 1, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)

	tkwm.withdraw(tt)
	tcl('update')
	tt.w <- as.integer(tkwinfo("reqwidth", tt))
	tt.h <- as.integer(tkwinfo("reqheight", tt))
	tt.x <- as.integer(.cdtEnv$tcl$data$width.scr*0.5 - tt.w*0.5)
	tt.y <- as.integer(.cdtEnv$tcl$data$height.scr*0.5 - tt.h*0.5)
	tkwm.geometry(tt, paste0('+', tt.x, '+', tt.y))
	tkwm.transient(tt)
	tkwm.title(tt, lang.dlg[['title']])
	tkwm.deiconify(tt)

	tkfocus(tt)
	tkbind(tt, "<Destroy>", function() {
		tkgrab.release(tt)
		tkfocus(.cdtEnv$tcl$main$win)
	})
	tkwait.window(tt)
}

