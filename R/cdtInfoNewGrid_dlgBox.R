
getNewGridParams <- function(tt, Parameters){
	tt1 <- tktoplevel()
	tkgrab.set(tt1)
	tkfocus(tt1)

	frGrd0 <- tkframe(tt1, relief = 'raised', borderwidth = 2)
	frGrd1 <- tkframe(tt1)

	################################

	# xml.dlg <- file.path(.cdtDir$dirLocal, "languages", "cdtInfoNewGrid_dlgBox.xml")
	# lang.dlg <- cdtLanguageParse(xml.dlg, .cdtData$Config$lang.iso)

	################################

	fr_grd <- ttklabelframe(frGrd0, text = "Create new grid", relief = "groove", borderwidth = 2)

	minLon <- tclVar(Parameters$minlon)
	maxLon <- tclVar(Parameters$maxlon)
	resLon <- tclVar(Parameters$reslon)
	minLat <- tclVar(Parameters$minlat)
	maxLat <- tclVar(Parameters$maxlat)
	resLat <- tclVar(Parameters$reslat)

	grd_llon <- tklabel(fr_grd, text = "Longitude", anchor = 'e', justify = 'right')
	grd_llat <- tklabel(fr_grd, text = "Latitude", anchor = 'e', justify = 'right')
	grd_lb1 <- tklabel(fr_grd, text = "Min")
	grd_lb2 <- tklabel(fr_grd, text = "Max")
	grd_lb3 <- tklabel(fr_grd, text = "Res")

	grd_vlon1 <- tkentry(fr_grd, width = 5, justify = "right", textvariable = minLon)
	grd_vlon2 <- tkentry(fr_grd, width = 5, justify = "right", textvariable = maxLon)
	grd_vlon3 <- tkentry(fr_grd, width = 6, justify = "right", textvariable = resLon)
	grd_vlat1 <- tkentry(fr_grd, width = 5, justify = "right", textvariable = minLat)
	grd_vlat2 <- tkentry(fr_grd, width = 5, justify = "right", textvariable = maxLat)
	grd_vlat3 <- tkentry(fr_grd, width = 6, justify = "right", textvariable = resLat)

	tkgrid(grd_lb1, row = 0, column = 1, sticky = "ew", rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
	tkgrid(grd_lb2, row = 0, column = 2, sticky = "ew", rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
	tkgrid(grd_lb3, row = 0, column = 3, sticky = "ew", rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
	tkgrid(grd_llon, row = 1, column = 0, sticky = "ew", rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
	tkgrid(grd_vlon1, row = 1, column = 1, sticky = "ew", rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
	tkgrid(grd_vlon2, row = 1, column = 2, sticky = "ew", rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
	tkgrid(grd_vlon3, row = 1, column = 3, sticky = "ew", rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
	tkgrid(grd_llat, row = 2, column = 0, sticky = "ew", rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
	tkgrid(grd_vlat1, row = 2, column = 1, sticky = "ew", rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
	tkgrid(grd_vlat2, row = 2, column = 2, sticky = "ew", rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
	tkgrid(grd_vlat3, row = 2, column = 3, sticky = "ew", rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)

	infobulle(grd_vlon1, 'Minimum longitude in degree decimal')
	status.bar.display(grd_vlon1, 'Minimum longitude in degree decimal')
	infobulle(grd_vlon2, 'Maximum longitude in degree decimal')
	status.bar.display(grd_vlon2, 'Maximum longitude in degree decimal')
	infobulle(grd_vlon3, 'Resolution in degree decimal')
	status.bar.display(grd_vlon3, 'Resolution in degree decimal')
	infobulle(grd_vlat1, 'Minimum latitude in degree decimal')
	status.bar.display(grd_vlat1, 'Minimum latitude in degree decimal')
	infobulle(grd_vlat2, 'Maximum latitude in degree decimal')
	status.bar.display(grd_vlat2, 'Maximum latitude in degree decimal')
	infobulle(grd_vlat3, 'Resolution in degree decimal')
	status.bar.display(grd_vlat3, 'Resolution in degree decimal')

	################################

	tkgrid(fr_grd, row = 0, column = 0, sticky = 'nswe', rowspan = 1, columnspan = 2, padx = 1, pady = 1, ipadx = 1, ipady = 1)

	################################

	bt.prm.OK <- ttkbutton(frGrd1, text = .cdtEnv$tcl$lang$global[['button']][['1']])
	bt.prm.CA <- ttkbutton(frGrd1, text = .cdtEnv$tcl$lang$global[['button']][['2']])

	tkconfigure(bt.prm.OK, command = function(){
		Parameters$minlon <<- as.numeric(str_trim(tclvalue(minLon)))
		Parameters$maxlon <<- as.numeric(str_trim(tclvalue(maxLon)))
		Parameters$reslon <<- as.numeric(str_trim(tclvalue(resLon)))
		Parameters$minlat <<- as.numeric(str_trim(tclvalue(minLat)))
		Parameters$maxlat <<- as.numeric(str_trim(tclvalue(maxLat)))
		Parameters$reslat <<- as.numeric(str_trim(tclvalue(resLat)))

		tkgrab.release(tt1)
		tkdestroy(tt1)
		tkfocus(tt)
	})

	tkconfigure(bt.prm.CA, command = function(){
		tkgrab.release(tt1)
		tkdestroy(tt1)
		tkfocus(tt)
	})

	tkgrid(bt.prm.OK, row = 0, column = 0, sticky = 'w', padx = 5, pady = 1, ipadx = 1, ipady = 1)
	tkgrid(bt.prm.CA, row = 0, column = 1, sticky = 'e', padx = 5, pady = 1, ipadx = 1, ipady = 1)

	################################

	tkgrid(frGrd0, row = 0, column = 0, sticky = 'nswe', rowspan = 1, columnspan = 2, padx = 1, pady = 1, ipadx = 1, ipady = 1)
	tkgrid(frGrd1, row = 1, column = 1, sticky = 'se', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)

	############################
	tkwm.withdraw(tt1)
	tcl('update')
	tt.w <- as.integer(tkwinfo("reqwidth", tt1))
	tt.h <- as.integer(tkwinfo("reqheight", tt1))
	tt.x <- as.integer(.cdtEnv$tcl$data$width.scr*0.5 - tt.w*0.5)
	tt.y <- as.integer(.cdtEnv$tcl$data$height.scr*0.5 - tt.h*0.5)
	tkwm.geometry(tt1, paste0('+', tt.x, '+', tt.y))
	tkwm.transient(tt1)
	tkwm.title(tt1, 'Grid Parameters')
	tkwm.deiconify(tt1)

	tkfocus(tt1)
	tkbind(tt1, "<Destroy>", function(){
		tkgrab.release(tt1)
		tkfocus(tt)
	})
	tkwait.window(tt1)
	return(Parameters)
}
