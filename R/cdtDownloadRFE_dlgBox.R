
download_RFE <- function(){
	if(WindowsOS()){
		largeur <- 35
		largeur1 <- 15
		largeur2 <- 20
	}else{
		largeur <- 30
		largeur1 <- 17
		largeur2 <- 20
	}

	xml.dlg <- file.path(.cdtDir$dirLocal, "languages", "cdtDownloadRFE_dlgBox.xml")
	lang.dlg <- cdtLanguageParse(xml.dlg, .cdtData$Config$lang.iso)

	#########
	tt <- tktoplevel()
	tkgrab.set(tt)
	tkfocus(tt)

	frGrd0 <- tkframe(tt, relief = 'raised', borderwidth = 2)
	frGrd1 <- tkframe(tt)

	#########
	frA0 <- tkframe(frGrd0, relief = 'sunken', bd = 2)

	dekadRFE <- c('10-DAYS TAMSATv3', '10-DAYS TAMSATv2', '10-DAYS CHIRPSv2.0', '10-DAYS CHIRPv1.0')
	dailyRFE <- c('DAILY TAMSATv3', 'DAILY TAMSATv2', 'DAILY CHIRPSv2.0')

	fileSource <- tclVar(str_trim(.cdtData$GalParams$rfe.data))
	rfeChoix <- c(dekadRFE, '------------------', dailyRFE)

	nomdek <- if(.cdtData$GalParams$rfe.data %in% dekadRFE) 'Dekad' else 'Day'

	cb.rfesource <- ttkcombobox(frA0, values = rfeChoix, textvariable = fileSource, width = largeur2)
	bt.rfesource <- ttkbutton(frA0, text = lang.dlg[['button']][['3']])

	tkconfigure(bt.rfesource, command = function(){
		Params <- .cdtData$GalParams[["date"]]
		names(Params) <- c("start.year", "start.mon", "start.day",
							"end.year", "end.mon", "end.day")
		Params <- getInfoDateRange(tt, Params, daypendek.lab = nomdek)
		.cdtData$GalParams$date$year1 <- Params$start.year
		.cdtData$GalParams$date$mon1 <- Params$start.mon
		.cdtData$GalParams$date$day1 <- Params$start.day
		.cdtData$GalParams$date$year2 <- Params$end.year
		.cdtData$GalParams$date$mon2 <- Params$end.mon
		.cdtData$GalParams$date$day2 <- Params$end.day
	})

	helpWidget(cb.rfesource, lang.dlg[['tooltip']][['1']], lang.dlg[['status']][['1']])
	helpWidget(bt.rfesource, lang.dlg[['tooltip']][['2']], lang.dlg[['status']][['2']])

	######
	tkgrid(cb.rfesource, row = 0, column = 0, sticky = 'we', padx = 1, pady = 3, ipadx = 1, ipady = 1)
	tkgrid(bt.rfesource, row = 0, column = 1, sticky = 'w', padx = 1, pady = 3, ipadx = 1, ipady = 1)

	######
	tkbind(cb.rfesource, "<<ComboboxSelected>>", function(){
		if(tclvalue(fileSource) %in% dekadRFE)
			nomdek <<- 'Dekad'
		else
			nomdek <<- 'Day'
	})

	#########
	frA1 <- tkframe(frGrd0, relief = 'sunken', bd = 2)

	minLon <- tclVar(.cdtData$GalParams$bbox$minlon)
	maxLon <- tclVar(.cdtData$GalParams$bbox$maxlon)
	minLat <- tclVar(.cdtData$GalParams$bbox$minlat)
	maxLat <- tclVar(.cdtData$GalParams$bbox$maxlat)

	fr_grd <- ttklabelframe(frA1, text = lang.dlg[['label']][['1']], relief = "groove", borderwidth = 2)

	grd_llon <- tklabel(fr_grd, text = lang.dlg[['label']][['2']], anchor = 'e', justify = 'right', width = largeur1)
	grd_llat <- tklabel(fr_grd, text = lang.dlg[['label']][['3']], anchor = 'e', justify = 'right')
	grd_lb1 <- tklabel(fr_grd, text = lang.dlg[['label']][['4']])
	grd_lb2 <- tklabel(fr_grd, text = lang.dlg[['label']][['5']])
	grd_vlon1 <- tkentry(fr_grd, width = 8, justify = "right", textvariable = minLon)
	grd_vlon2 <- tkentry(fr_grd, width = 8, justify = "right", textvariable = maxLon)
	grd_vlat1 <- tkentry(fr_grd, width = 8, justify = "right", textvariable = minLat)
	grd_vlat2 <- tkentry(fr_grd, width = 8, justify = "right", textvariable = maxLat)

	tkgrid(grd_lb1, row = 0, column = 1, sticky = "ew")
	tkgrid(grd_lb2, row = 0, column = 2, sticky = "ew")
	tkgrid(grd_llon, row = 1, column = 0, sticky = "ew")
	tkgrid(grd_vlon1, row = 1, column = 1, sticky = "ew")
	tkgrid(grd_vlon2, row = 1, column = 2, sticky = "ew")
	tkgrid(grd_llat, row = 2, column = 0, sticky = "ew")
	tkgrid(grd_vlat1, row = 2, column = 1, sticky = "ew")
	tkgrid(grd_vlat2, row = 2, column = 2, sticky = "ew")

	tkgrid(fr_grd, row = 0, column = 0, sticky = "ew", padx = 5, pady = 5)

	helpWidget(grd_vlon1, lang.dlg[['tooltip']][['3']], lang.dlg[['status']][['3']])
	helpWidget(grd_vlon2, lang.dlg[['tooltip']][['4']], lang.dlg[['status']][['4']])
	helpWidget(grd_vlat1, lang.dlg[['tooltip']][['5']], lang.dlg[['status']][['5']])
	helpWidget(grd_vlat2, lang.dlg[['tooltip']][['6']], lang.dlg[['status']][['6']])

	#########
	frA2 <- tkframe(frGrd0, relief = 'sunken', bd = 2)

	dir2save <- tclVar(.cdtData$GalParams$dir2save)

	txt.dir.save <- tklabel(frA2, text = lang.dlg[['label']][['6']], anchor = 'w', justify = 'left')
	en.dir.save <- tkentry(frA2, textvariable = dir2save, width = largeur)
	bt.dir.save <- tkbutton(frA2, text = "...")

	###
	tkconfigure(bt.dir.save, command = function(){
		dir2savepth <- tk_choose.dir(.cdtData$GalParams$dir2save, "")
		if(is.na(dir2savepth)) tclvalue(dir2save) <- .cdtData$GalParams$dir2save
		else{
			dir.create(dir2savepth, showWarnings = FALSE, recursive = TRUE)
			tclvalue(dir2save) <- dir2savepth
		}
	})

	###
	tkgrid(txt.dir.save, row = 0, column = 0, sticky = 'we', rowspan = 1, columnspan = 2, padx = 1, pady = 1, ipadx = 1, ipady = 1)
	tkgrid(en.dir.save, row = 1, column = 0, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
	tkgrid(bt.dir.save, row = 1, column = 1, sticky = 'w', rowspan = 1, columnspan = 1, padx = 0, pady = 1, ipadx = 1, ipady = 1)

	helpWidget(en.dir.save, lang.dlg[['tooltip']][['7']], lang.dlg[['status']][['7']])
	helpWidget(bt.dir.save, lang.dlg[['tooltip']][['8']], lang.dlg[['status']][['8']])

	######
	tkgrid(frA0, row = 0, column = 0, sticky = 'we', rowspan = 1, columnspan = 1, padx = 5, pady = 5, ipadx = 1, ipady = 1)
	tkgrid(frA1, row = 1, column = 0, sticky = 'we', rowspan = 1, columnspan = 1, padx = 5, pady = 5, ipadx = 1, ipady = 1)
	tkgrid(frA2, row = 2, column = 0, sticky = 'we', rowspan = 1, columnspan = 1, padx = 5, pady = 5, ipadx = 1, ipady = 1)

	######
	btOK <- ttkbutton(frGrd1, text = lang.dlg[['button']][['1']])
	btCA <- ttkbutton(frGrd1, text = lang.dlg[['button']][['2']])

	tkconfigure(btOK, command = function(){
		if(str_trim(tclvalue(dir2save)) %in% c("", "NA")){
			tkmessageBox(message = lang.dlg[['message']][['1']], icon = "warning", type = "ok")
			tkwait.window(tt)
		}else{
			tkgrab.release(tt)
			tkdestroy(tt)
			tkfocus(.cdtEnv$tcl$main$win)

			.cdtData$GalParams$rfe.data <- str_trim(tclvalue(fileSource))
			.cdtData$GalParams$dir2save <- str_trim(tclvalue(dir2save))

			.cdtData$GalParams$minlon <- as.numeric(tclvalue(minLon))
			.cdtData$GalParams$maxlon <- as.numeric(tclvalue(maxLon))
			.cdtData$GalParams$minlat <- as.numeric(tclvalue(minLat))
			.cdtData$GalParams$maxlat <- as.numeric(tclvalue(maxLat))


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

	#####
	tkgrid(frGrd0, row = 0, column = 0, sticky = 'nswe', rowspan = 1, columnspan = 2, padx = 1, pady = 1, ipadx = 1, ipady = 1)
	tkgrid(frGrd1, row = 1, column = 1, sticky = 'se', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)

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
	tkbind(tt, "<Destroy>", function(){
		tkgrab.release(tt)
		tkfocus(.cdtEnv$tcl$main$win)
	})
	tkwait.window(tt)
}
