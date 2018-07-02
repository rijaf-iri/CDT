
fill_Miss_DekTemp <- function(){
	listOpenFiles <- openFile_ttkcomboList()
	if(WindowsOS()){
		largeur <- 48
		largeur1 <- 34
	}else{
		largeur <- 35
		largeur1 <- 33
	}

	xml.dlg <- file.path(.cdtDir$dirLocal, "languages", "cdtFillCDTdekTemp_dlgBox.xml")
	lang.dlg <- cdtLanguageParse(xml.dlg, .cdtData$Config$lang.iso)

	############################################

	tt <- tktoplevel()
	tkgrab.set(tt)
	tkfocus(tt)

	frMRG0 <- tkframe(tt, relief = 'raised', borderwidth = 2)
	frMRG1 <- tkframe(tt)
	frLeft <- tkframe(frMRG0, relief = "groove", borderwidth = 2)
	frRight <- tkframe(frMRG0, relief = "groove", borderwidth = 2)

	############################################

	frSTN <- tkframe(frLeft, relief = 'sunken', borderwidth = 2)

	file.stnfl <- tclVar(.cdtData$GalParams$STN.file)

	txt.stnfl <- tklabel(frSTN, text = lang.dlg[['label']][['1']], anchor = 'w', justify = 'left')
	cb.stnfl <- ttkcombobox(frSTN, values = unlist(listOpenFiles), textvariable = file.stnfl, width = largeur1)
	bt.stnfl <- tkbutton(frSTN, text = "...")

	######
	tkconfigure(bt.stnfl, command = function(){
		dat.opfiles <- getOpenFiles(tt)
		if(!is.null(dat.opfiles)){
			update.OpenFiles('ascii', dat.opfiles)
			listOpenFiles[[length(listOpenFiles)+1]] <<- dat.opfiles[[1]]
			tclvalue(file.stnfl) <- dat.opfiles[[1]]
			tkconfigure(cb.stnfl, values = unlist(listOpenFiles))
		}
	})

	######
	tkgrid(txt.stnfl, row = 0, column = 0, sticky = 'we', rowspan = 1, columnspan = 2, padx = 1, pady = 0, ipadx = 1, ipady = 1)
	tkgrid(cb.stnfl, row = 1, column = 0, sticky = 'we', rowspan = 1, columnspan = 1, padx = 0, pady = 0, ipadx = 1, ipady = 1)
	tkgrid(bt.stnfl, row = 1, column = 1, sticky = 'w', rowspan = 1, columnspan = 1, padx = 0, pady = 0, ipadx = 1, ipady = 1)

	status.bar.display(cb.stnfl, lang.dlg[['status']][['1']])
	infobulle(cb.stnfl, lang.dlg[['tooltip']][['1']])
	helpWidget(bt.stnfl, lang.dlg[['tooltip']][['2']], lang.dlg[['status']][['2']])

	############################################

	frRFE <- tkframe(frLeft, relief = 'sunken', borderwidth = 2)

	dir.rfe <- tclVar(.cdtData$GalParams$NCDF$dir)

	txt.dir.rfe <- tklabel(frRFE, text = lang.dlg[['label']][['2']], anchor = 'w', justify = 'left')
	set.dir.rfe <- tkbutton(frRFE, text = lang.dlg[['label']][['3']])
	en.dir.rfe <- tkentry(frRFE, textvariable = dir.rfe, width = largeur)
	bt.dir.rfe <- tkbutton(frRFE, text = "...")

	######
	tkconfigure(bt.dir.rfe, command = function(){
		dir4rfe <- tk_choose.dir(.cdtData$GalParams$NCDF$dir, "")
		tclvalue(dir.rfe) <- if(!is.na(dir4rfe)) dir4rfe else .cdtData$GalParams$NCDF$dir
	})

	tkconfigure(set.dir.rfe, command = function(){
		.cdtData$GalParams[["NCDF"]] <- getInfoNetcdfData(tt, .cdtData$GalParams[["NCDF"]],
														str_trim(tclvalue(dir.rfe)),
														.cdtEnv$tcl$lang$global[['combobox']][['1']][4])
	})

	######

	tkgrid(txt.dir.rfe, row = 0, column = 0, sticky = 'we', rowspan = 1, columnspan = 3, padx = 1, pady = 0, ipadx = 1, ipady = 1)
	tkgrid(set.dir.rfe, row = 0, column = 3, sticky = 'we', rowspan = 1, columnspan = 2, padx = 1, pady = 0, ipadx = 1, ipady = 1)
	tkgrid(en.dir.rfe, row = 1, column = 0, sticky = 'we', rowspan = 1, columnspan = 4, padx = 0, pady = 0, ipadx = 1, ipady = 1)
	tkgrid(bt.dir.rfe, row = 1, column = 4, sticky = 'w', rowspan = 1, columnspan = 1, padx = 0, pady = 0, ipadx = 1, ipady = 1)

	helpWidget(en.dir.rfe, lang.dlg[['tooltip']][['3']], lang.dlg[['status']][['3']])
	helpWidget(bt.dir.rfe, lang.dlg[['tooltip']][['4']], lang.dlg[['status']][['4']])
	helpWidget(set.dir.rfe, lang.dlg[['tooltip']][['5']], lang.dlg[['status']][['5']])

	############################################

	frSave <- tkframe(frLeft, relief = 'sunken', borderwidth = 2)

	file.save1 <- tclVar(.cdtData$GalParams$out.file)

	txt.file.save <- tklabel(frSave, text = lang.dlg[['label']][['4']], anchor = 'w', justify = 'left')
	en.file.save <- tkentry(frSave, textvariable = file.save1, width = largeur)
	bt.file.save <- tkbutton(frSave, text = "...")

	######
	tkconfigure(bt.file.save, command = function() fileORdir2Save(file.save1, isFile = TRUE))

	######
	tkgrid(txt.file.save, row = 0, column = 0, sticky = 'we', rowspan = 1, columnspan = 2, padx = 1, pady = 0, ipadx = 1, ipady = 1)
	tkgrid(en.file.save, row = 1, column = 0, sticky = 'we', rowspan = 1, columnspan = 1, padx = 0, pady = 0, ipadx = 1, ipady = 1)
	tkgrid(bt.file.save, row = 1, column = 1, sticky = 'w', rowspan = 1, columnspan = 1, padx = 0, pady = 0, ipadx = 1, ipady = 1)

	status.bar.display(en.file.save, lang.dlg[['status']][['6']])
	infobulle(en.file.save, lang.dlg[['tooltip']][['6']])
	status.bar.display(bt.file.save, lang.dlg[['status']][['7']])
	infobulle(bt.file.save, lang.dlg[['tooltip']][['7']])

	############################################
	tkgrid(frSTN, row = 0, column = 0, sticky = 'we', padx = 1, pady = 1, ipadx = 1, ipady = 1)
	tkgrid(frRFE, row = 1, column = 0, sticky = 'we', padx = 1, pady = 1, ipadx = 1, ipady = 1)
	tkgrid(frSave, row = 2, column = 0, sticky = 'we', padx = 1, pady = 1, ipadx = 1, ipady = 1)

	#######################  RIGHT   #####################

	frDate <- tkframe(frRight, relief = 'sunken', borderwidth = 2)

	istart.yrs <- tclVar(.cdtData$GalParams$Fill.Date.Range$start.year)
	istart.mon <- tclVar(.cdtData$GalParams$Fill.Date.Range$start.mon)
	istart.dek <- tclVar(.cdtData$GalParams$Fill.Date.Range$start.dek)
	iend.yrs <- tclVar(.cdtData$GalParams$Fill.Date.Range$end.year)
	iend.mon <- tclVar(.cdtData$GalParams$Fill.Date.Range$end.mon)
	iend.dek <- tclVar(.cdtData$GalParams$Fill.Date.Range$end.dek)

	frtxtDate <- ttklabelframe(frDate, text = lang.dlg[['label']][['5']], relief = 'groove')

	deb.txt <- tklabel(frtxtDate, text = lang.dlg[['label']][['6']], anchor = 'e', justify = 'right')
	fin.txt <- tklabel(frtxtDate, text = lang.dlg[['label']][['7']], anchor = 'e', justify = 'right')
	yrs.txt <- tklabel(frtxtDate, text = lang.dlg[['label']][['8']])
	mon.txt <- tklabel(frtxtDate, text = lang.dlg[['label']][['9']])
	dek.txt <- tklabel(frtxtDate, text = lang.dlg[['label']][['10']])
	yrs1.v <- tkentry(frtxtDate, width = 4, textvariable = istart.yrs, justify = "right")
	mon1.v <- tkentry(frtxtDate, width = 4, textvariable = istart.mon, justify = "right")
	dek1.v <- tkentry(frtxtDate, width = 4, textvariable = istart.dek, justify = "right")
	yrs2.v <- tkentry(frtxtDate, width = 4, textvariable = iend.yrs, justify = "right")
	mon2.v <- tkentry(frtxtDate, width = 4, textvariable = iend.mon, justify = "right")
	dek2.v <- tkentry(frtxtDate, width = 4, textvariable = iend.dek, justify = "right")

	tkgrid(deb.txt, row = 1, column = 0, sticky = 'ew', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
	tkgrid(fin.txt, row = 2, column = 0, sticky = 'ew', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
	tkgrid(yrs.txt, row = 0, column = 1, sticky = 'ew', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
	tkgrid(mon.txt, row = 0, column = 2, sticky = 'ew', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
	tkgrid(dek.txt, row = 0, column = 3, sticky = 'ew', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
	tkgrid(yrs1.v, row = 1, column = 1, sticky = 'ew', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
	tkgrid(mon1.v, row = 1, column = 2, sticky = 'ew', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
	tkgrid(dek1.v, row = 1, column = 3, sticky = 'ew', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
	tkgrid(yrs2.v, row = 2, column = 1, sticky = 'ew', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
	tkgrid(mon2.v, row = 2, column = 2, sticky = 'ew', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
	tkgrid(dek2.v, row = 2, column = 3, sticky = 'ew', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)

	tkgrid(frtxtDate, row = 1, column = 0, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 5, ipadx = 1, ipady = 1)

	helpWidget(frtxtDate, lang.dlg[['tooltip']][['8']], lang.dlg[['status']][['8']])

	############################################

	frParams <- tkframe(frRight, relief = 'sunken', borderwidth = 2)

	min.len <- tclVar(.cdtData$GalParams$Fill.Params$min.length)

	txt0.min.len <- tklabel(frParams, text = lang.dlg[['label']][['11']], anchor = 'w', justify = 'left')
	txt.min.len <- tklabel(frParams, text = lang.dlg[['label']][['12']], anchor = 'e', justify = 'right')
	en.min.len <- tkentry(frParams, textvariable = min.len, width = 8)

	tkgrid(txt0.min.len, row = 0, column = 0, sticky = 'we', rowspan = 1, columnspan = 2, padx = 1, pady = 1, ipadx = 1, ipady = 1)
	tkgrid(txt.min.len, row = 1, column = 0, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
	tkgrid(en.min.len, row = 1, column = 1, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)

	helpWidget(en.min.len, lang.dlg[['tooltip']][['9']], lang.dlg[['status']][['9']])

	############################################
	tkgrid(frDate, row = 0, column = 0, sticky = 'we', padx = 1, pady = 1, ipadx = 1, ipady = 1)
	tkgrid(frParams, row = 1, column = 0, sticky = 'we', padx = 1, pady = 5, ipadx = 1, ipady = 5)

	############################################
	
	tkgrid(frLeft, row = 0, column = 0, sticky = 'news', padx = 5, pady = 1, ipadx = 1, ipady = 1)
	tkgrid(frRight, row = 0, column = 1, sticky = '', padx = 5, pady = 1, ipadx = 1, ipady = 1)

	############################################

	bt.prm.OK <- ttkbutton(frMRG1, text = .cdtEnv$tcl$lang$global[['button']][['1']])
	bt.prm.CA <- ttkbutton(frMRG1, text = .cdtEnv$tcl$lang$global[['button']][['2']])

	tkconfigure(bt.prm.OK, command = function(){
		if(str_trim(tclvalue(file.stnfl)) == ""){
			tkmessageBox(message = lang.dlg[['message']][['1']], icon = "warning", type = "ok")
		}else if(str_trim(tclvalue(dir.rfe)) %in% c("", "NA")){
			tkmessageBox(message = lang.dlg[['message']][['2']], icon = "warning", type = "ok")
			tkwait.window(tt)
		}else if(str_trim(tclvalue(file.save1)) %in% c("", "NA")){
			tkmessageBox(message = lang.dlg[['message']][['3']], icon = "warning", type = "ok")
			tkwait.window(tt)
		}else if(!(as.numeric(str_trim(tclvalue(istart.dek))) %in% 1:3) |
					!(as.numeric(str_trim(tclvalue(iend.dek))) %in% 1:3))
		{
			tkmessageBox(message = lang.dlg[['message']][['4']], icon = "warning", type = "ok")
			tkwait.window(tt)
		}else{
			.cdtData$GalParams$STN.file <- str_trim(tclvalue(file.stnfl))
			.cdtData$GalParams$NCDF$dir <- str_trim(tclvalue(dir.rfe))
			.cdtData$GalParams$out.file <- str_trim(tclvalue(file.save1))

			.cdtData$GalParams$Fill.Date.Range$start.year <- as.numeric(str_trim(tclvalue(istart.yrs)))
			.cdtData$GalParams$Fill.Date.Range$start.mon <- as.numeric(str_trim(tclvalue(istart.mon)))
			.cdtData$GalParams$Fill.Date.Range$start.dek <- as.numeric(str_trim(tclvalue(istart.dek)))
			.cdtData$GalParams$Fill.Date.Range$end.year <- as.numeric(str_trim(tclvalue(iend.yrs)))
			.cdtData$GalParams$Fill.Date.Range$end.mon <- as.numeric(str_trim(tclvalue(iend.mon)))
			.cdtData$GalParams$Fill.Date.Range$end.dek <- as.numeric(str_trim(tclvalue(iend.dek)))

			.cdtData$GalParams$Fill.Params$min.length <- as.numeric(str_trim(tclvalue(min.len)))
			.cdtData$GalParams$message <- lang.dlg[['message']]

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
	tkwm.title(tt, lang.dlg[['title']])
	tkwm.deiconify(tt)

	tkfocus(tt)
	tkbind(tt, "<Destroy>", function(){
		tkgrab.release(tt)
		tkfocus(.cdtEnv$tcl$main$win)
	})
	tkwait.window(tt)
}

