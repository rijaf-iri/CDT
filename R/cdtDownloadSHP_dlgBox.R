
download_CountryShapefile <- function(){
	if(WindowsOS()){
		largeur <- 34
		largeur1 <- 35
	}else{
		largeur <- 31
		largeur1 <- 35
	}

	xml.dlg <- file.path(.cdtDir$dirLocal, "languages", "cdtDownloadSHP_dlgBox.xml")
	lang.dlg <- cdtLanguageParse(xml.dlg, .cdtData$Config$lang.iso)
	cntr <- readRDS(file.path(.cdtDir$dirLocal, 'data', 'Africa_Country.rds'))

	#####
	tt <- tktoplevel()
	tkgrab.set(tt)
	tkfocus(tt)

	frGrd0 <- tkframe(tt, relief = 'raised', borderwidth = 2)
	frGrd1 <- tkframe(tt)

	#####
	frA1 <- tkframe(frGrd0, relief = 'sunken', bd = 2)

	cbvalues <- cntr$NAME_ENGLISH
	country <- tclVar(.cdtData$GalParams$shp$country)
	level_sub <- tclVar(.cdtData$GalParams$shp$level)

	lab1 <- tklabel(frA1, text = lang.dlg[['label']][['1']], anchor = 'w', justify = 'left')
	cb.country <- ttkcombobox(frA1, values = cbvalues, textvariable = country, width = largeur1)
	separator1 <- ttkseparator(frA1)
	lab2 <- tklabel(frA1, text = lang.dlg[['label']][['2']], anchor = 'w', justify = 'left')
	cb.level_sub <- ttkcombobox(frA1, values = 0:4, textvariable = level_sub, width = largeur1)

	###
	tkgrid(lab1, row = 0, column = 0, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
	tkgrid(cb.country, row = 1, column = 0, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
	tkgrid(separator1, row = 2, column = 0, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
	tkgrid(lab2, row = 3, column = 0, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
	tkgrid(cb.level_sub, row = 4, column = 0, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)

	helpWidget(cb.country, lang.dlg[['tooltip']][['1']], lang.dlg[['status']][['1']])
	helpWidget(cb.level_sub, lang.dlg[['tooltip']][['2']], lang.dlg[['status']][['2']])

	###
	tkbind(cb.country, "<<ComboboxSelected>>", function(){
		cntr_id <- match(tclvalue(country), cbvalues)
		maxlev <- as.numeric(cntr[cntr_id, "max_lev"])
		level_val <- 0:maxlev
		tclvalue(level_sub) <- '0'
		tkconfigure(cb.level_sub, values = level_val)
	})

	#####
	frA2 <- tkframe(frGrd0, relief = 'sunken', bd = 2)

	dir2save <- tclVar(.cdtData$GalParams$dir2save)

	txt.file.save <- tklabel(frA2, text = lang.dlg[['label']][['3']], anchor = 'w', justify = 'left')
	en.file.save <- tkentry(frA2, textvariable = dir2save, width = largeur)
	bt.file.save <- tkbutton(frA2, text = "...")

	###
	tkconfigure(bt.file.save, command = function(){
		dir2savepth <- tk_choose.dir(.cdtData$GalParams$dir2save, "")
		if(is.na(dir2savepth)) tclvalue(dir2save) <- .cdtData$GalParams$dir2save
		else{
			dir.create(dir2savepth, showWarnings = FALSE, recursive = TRUE)
			tclvalue(dir2save) <- dir2savepth
		}
	})

	###
	tkgrid(txt.file.save, row = 0, column = 0, sticky = 'we', rowspan = 1, columnspan = 2, padx = 1, pady = 1, ipadx = 1, ipady = 1)
	tkgrid(en.file.save, row = 1, column = 0, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
	tkgrid(bt.file.save, row = 1, column = 1, sticky = 'w', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)

	helpWidget(en.file.save, lang.dlg[['tooltip']][['3']], lang.dlg[['status']][['3']])
	helpWidget(bt.file.save, lang.dlg[['tooltip']][['4']], lang.dlg[['status']][['4']])

	###
	tkgrid(frA1, row = 0, column = 0, sticky = 'we', rowspan = 1, columnspan = 1, padx = 5, pady = 5, ipadx = 1, ipady = 1)
	tkgrid(frA2, row = 1, column = 0, sticky = 'we', rowspan = 1, columnspan = 1, padx = 5, pady = 5, ipadx = 1, ipady = 1)

	###
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
			.cdtData$GalParams$shp$country <- str_trim(tclvalue(country))
			.cdtData$GalParams$shp$level <- str_trim(tclvalue(level_sub))
			cntr_id <- match(.cdtData$GalParams$shp$country, cbvalues)
			.cdtData$GalParams$cntr_iso3 <- cntr[cntr_id, "ISO3"]
			.cdtData$GalParams$message <- lang.dlg[['message']]

			if(testConnection()){
				Insert.Messages.Out(lang.dlg[['message']][['3']])
				ExecDownload_GADM()
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
