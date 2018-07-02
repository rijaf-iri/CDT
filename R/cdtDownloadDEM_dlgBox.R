
download_DEM <- function(){
	if(WindowsOS()){
		largeur <- 35
		largeur1 <- 15
	}else{
		largeur <- 28
		largeur1 <- 15
	}

	xml.dlg <- file.path(.cdtDir$dirLocal, "languages", "cdtDownloadDEM_dlgBox.xml")
	lang.dlg <- cdtLanguageParse(xml.dlg, .cdtData$Config$lang.iso)

	#####
	tt <- tktoplevel()
	tkgrab.set(tt)
	tkfocus(tt)

	frGrd0 <- tkframe(tt, relief = 'raised', borderwidth = 2)
	frGrd1 <- tkframe(tt)

	#####
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

	helpWidget(grd_vlon1, lang.dlg[['tooltip']][['1']], lang.dlg[['status']][['1']])
	helpWidget(grd_vlon2, lang.dlg[['tooltip']][['2']], lang.dlg[['status']][['2']])
	helpWidget(grd_vlat1, lang.dlg[['tooltip']][['3']], lang.dlg[['status']][['3']])
	helpWidget(grd_vlat2, lang.dlg[['tooltip']][['4']], lang.dlg[['status']][['4']])

	#########
	frA2 <- tkframe(frGrd0, relief = 'sunken', bd = 2)

	dir2save <- tclVar(.cdtData$GalParams$dir2save)

	lab2 <- tklabel(frA2, text = lang.dlg[['label']][['6']], anchor = 'w', justify = 'left')
	enfile.save <- tkentry(frA2, textvariable = dir2save, width = largeur)
	btfile.save <- tkbutton(frA2, text = "...")

	###
	tkconfigure(btfile.save, command = function(){
		dir2savepth <- tk_choose.dir(.cdtData$GalParams$dir2save, "")
		if(is.na(dir2savepth)) tclvalue(dir2save) <- .cdtData$GalParams$dir2save
		else{
			dir.create(dir2savepth, showWarnings = FALSE, recursive = TRUE)
			tclvalue(dir2save) <- dir2savepth
		}
	})

	###
	tkgrid(lab2, row = 0, column = 0, sticky = 'we', rowspan = 1, columnspan = 2, padx = 1, pady = 1, ipadx = 1, ipady = 1)
	tkgrid(enfile.save, row = 1, column = 0, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
	tkgrid(btfile.save, row = 1, column = 1, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)

	helpWidget(enfile.save, lang.dlg[['tooltip']][['5']], lang.dlg[['status']][['5']])
	helpWidget(btfile.save, lang.dlg[['tooltip']][['6']], lang.dlg[['status']][['6']])

	######
	tkgrid(frA1, row = 0, column = 0, sticky = 'we', rowspan = 1, columnspan = 1, padx = 5, pady = 5, ipadx = 1, ipady = 1)
	tkgrid(frA2, row = 1, column = 0, sticky = 'we', rowspan = 1, columnspan = 1, padx = 5, pady = 5, ipadx = 1, ipady = 1)

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

			.cdtData$GalParams$dir2save <- str_trim(tclvalue(dir2save))
			.cdtData$GalParams$minlon <- as.numeric(tclvalue(minLon))
			.cdtData$GalParams$maxlon <- as.numeric(tclvalue(maxLon))
			.cdtData$GalParams$minlat <- as.numeric(tclvalue(minLat))
			.cdtData$GalParams$maxlat <- as.numeric(tclvalue(maxLat))
			.cdtData$GalParams$message <- lang.dlg[['message']]

			if(testConnection()){
				Insert.Messages.Out(lang.dlg[['message']][['3']])
				ExecDownload_DEM()
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
