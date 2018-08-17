
getParams.HomogMethod <- function(Parameters, CONF.LEV,
								parent.win = .cdtEnv$tcl$main$win)
{
	# xml.dlg <- file.path(.cdtDir$dirLocal, "languages", "cdtQC_Homogeneity_dlgBox1.xml")
	# lang.dlg <- cdtLanguageParse(xml.dlg, .cdtData$Config$lang.iso)

	###################

	tt <- tktoplevel()
	tkgrab.set(tt)
	tkfocus(tt)

	frMRG0 <- tkframe(tt, relief = 'raised', borderwidth = 2)
	frMRG1 <- tkframe(tt)

	###################

	framePars <- tkframe(frMRG0, relief = 'sunken', borderwidth = 2)


	crop.perc.list <- c('0.010', '0.025', '0.050', '0.100')
	crop.bound <- tclVar(Parameters$crop)
	crop.perc <- tclVar(sprintf("%.3f", Parameters$h))
	CL <- sprintf("%.1f", Parameters$conf.lev)
	if(!CL %in% CONF.LEV){
		CL <- findInterval(as.numeric(Parameters$conf.lev), as.numeric(CONF.LEV))
		CL <- CONF.LEV[CL]
	}
	conf.lev <- tclVar(CL)
	Kmax <- tclVar(Parameters$kmax)
	minSeg <- tclVar(Parameters$min.len)
	minYear <- tclVar(Parameters$min.year)
	minFrac <- tclVar(Parameters$min.frac)

	fr.crop <- tkframe(framePars)
	txt.conf <- tklabel(framePars, text = 'Confidence level', anchor = 'e', justify = 'right')
	cb.conf <- ttkcombobox(framePars, values = CONF.LEV, textvariable = conf.lev, width = 6)
	txt.brks <- tklabel(framePars, text = 'Max number breaks', anchor = 'e', justify = 'right')
	en.brks <- tkentry(framePars, textvariable = Kmax, width = 6)
	txt.minS <- tklabel(framePars, text = 'Min segment length', anchor = 'e', justify = 'right')
	en.minS <- tkentry(framePars, textvariable = minSeg, width = 6)
	txt.year <- tklabel(framePars, text = 'Min number year', anchor = 'e', justify = 'right')
	en.year <- tkentry(framePars, textvariable = minYear, width = 6)
	txt.frac <- tklabel(framePars, text = 'Min non-missing frac', anchor = 'e', justify = 'right')
	en.frac <- tkentry(framePars, textvariable = minFrac, width = 6)

	###################
	chk.crop <- tkcheckbutton(fr.crop, variable = crop.bound, text = 'Crop bounds', anchor = 'w', justify = 'left')
	txt.crop <- tklabel(fr.crop, text = 'h:', anchor = 'e', justify = 'right')
	cb.crop <- ttkcombobox(fr.crop, values = crop.perc.list, textvariable = crop.perc, width = 6)

	tkgrid(chk.crop, row = 0, column = 0, sticky = 'we', rowspan = 1, columnspan = 1, padx = 3, pady = 1, ipadx = 1, ipady = 1)
	tkgrid(txt.crop, row = 0, column = 1, sticky = 'e', rowspan = 1, columnspan = 1, padx = 0, pady = 1, ipadx = 1, ipady = 1)
	tkgrid(cb.crop, row = 0, column = 3, sticky = 'we', rowspan = 1, columnspan = 1, padx = 0, pady = 1, ipadx = 1, ipady = 1)

	###################

	tkgrid(fr.crop, row = 0, column = 0, sticky = 'we', rowspan = 1, columnspan = 2, padx = 1, pady = 1, ipadx = 1, ipady = 1)
	tkgrid(txt.conf, row = 0, column = 2, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
	tkgrid(cb.conf, row = 0, column = 3, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
	tkgrid(txt.brks, row = 1, column = 0, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
	tkgrid(en.brks, row = 1, column = 1, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
	tkgrid(txt.minS, row = 1, column = 2, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
	tkgrid(en.minS, row = 1, column = 3, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
	tkgrid(txt.year, row = 2, column = 0, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
	tkgrid(en.year, row = 2, column = 1, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
	tkgrid(txt.frac, row = 2, column = 2, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
	tkgrid(en.frac, row = 2, column = 3, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)

	helpWidget(fr.crop, 'Cropping the first and last [h x 100%] percent of the series',
							'Cropping the first and last [h x 100%] percent of the series')
	helpWidget(en.brks, 'Maximum number of change-points to be detected', 'Maximum number of change-points to be detected')
	helpWidget(en.minS, 'Minimum length segment (in months) to carry out the test', 'Minimum length segment (in months) to carry out the test')
	helpWidget(cb.conf, 'Confidence level (%)', 'Confidence level (%)')

	helpWidget(en.year, 'Minimum number of year to be considered,\nif the length of year (when the station is supposed to report data)\nis less than this threshold no test will be performed',
						'Minimum number of year to be considered,\nif the length of year (when the station is supposed to report data)\nis less than this threshold no test will be performed')
	helpWidget(en.frac, 'Minimum fraction of available data (non-missing) that must be present\nduring the operating period of the station',
						'Minimum fraction of available data (non-missing) that must be present\nduring the operating period of the station')

	###################

	tkgrid(framePars, row = 0, column = 0, sticky = 'we', padx = 1, ipadx = 1, pady = 3, ipady = 5)

	################################

	bt.prm.OK <- ttkbutton(frMRG1, text = .cdtEnv$tcl$lang$global[['button']][['1']])
	bt.prm.CA <- ttkbutton(frMRG1, text = .cdtEnv$tcl$lang$global[['button']][['2']])

	tkconfigure(bt.prm.OK, command = function(){
		Parameters$crop <<- switch(tclvalue(crop.bound), '0' = FALSE, '1' = TRUE)
		Parameters$h <<- as.numeric(str_trim(tclvalue(crop.perc)))
		Parameters$kmax <<- as.numeric(str_trim(tclvalue(Kmax)))
		Parameters$min.len <<- as.numeric(str_trim(tclvalue(minSeg)))
		Parameters$conf.lev <<- as.numeric(str_trim(tclvalue(conf.lev)))
		Parameters$min.year <<- as.numeric(str_trim(tclvalue(minYear)))
		Parameters$min.frac <<- as.numeric(str_trim(tclvalue(minFrac)))

		tkgrab.release(tt)
		tkdestroy(tt)
		tkfocus(parent.win)
	})

	tkconfigure(bt.prm.CA, command = function(){
		tkgrab.release(tt)
		tkdestroy(tt)
		tkfocus(parent.win)
	})

	tkgrid(bt.prm.OK, row = 0, column = 0, sticky = 'w', padx = 5, pady = 1, ipadx = 1, ipady = 1)
	tkgrid(bt.prm.CA, row = 0, column = 1, sticky = 'e', padx = 5, pady = 1, ipadx = 1, ipady = 1)

	################################
	tkgrid(frMRG0, row = 0, column = 0, sticky = 'nswe', rowspan = 1, columnspan = 2, padx = 1, pady = 1, ipadx = 1, ipady = 1)
	tkgrid(frMRG1, row = 1, column = 1, sticky = 'se', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)

	tkwm.withdraw(tt)
	tcl('update')
	tt.w <- as.integer(tkwinfo("reqwidth", tt))
	tt.h <- as.integer(tkwinfo("reqheight", tt))
	tt.x <- as.integer(.cdtEnv$tcl$data$width.scr*0.5 - tt.w*0.5)
	tt.y <- as.integer(.cdtEnv$tcl$data$height.scr*0.5 - tt.h*0.5)
	tkwm.geometry(tt, paste0('+', tt.x, '+', tt.y))
	tkwm.transient(tt)
	tkwm.title(tt, 'Statistics Test Parameters')
	tkwm.deiconify(tt)

	tkfocus(tt)
	tkbind(tt, "<Destroy>", function(){
		tkgrab.release(tt)
		tkfocus(parent.win)
	})
	tkwait.window(tt)
	return(Parameters)
}

############################################################################################

getParams.HomogAdjust <- function(Parameters, label = "Day",
									state.dyp = 'normal',
									state.dek = 'normal',
									parent.win = .cdtEnv$tcl$main$win)
{
	# xml.dlg <- file.path(.cdtDir$dirLocal, "languages", "cdtQC_Homogeneity_dlgBox2.xml")
	# lang.dlg <- cdtLanguageParse(xml.dlg, .cdtData$Config$lang.iso)

	###################

	tt <- tktoplevel()
	tkgrab.set(tt)
	tkfocus(tt)

	frMRG0 <- tkframe(tt, relief = 'raised', borderwidth = 2)
	frMRG1 <- tkframe(tt)

	###################

	framePars <- tkframe(frMRG0, relief = 'sunken', borderwidth = 2)

	min.mon <- tclVar(Parameters$min.mon)
	min.dek <- tclVar(Parameters$min.dek)
	min.dyp <- tclVar(Parameters$min.dyp)

	seg.mon <- tclVar(Parameters$seg.mon)
	seg.dek <- tclVar(Parameters$seg.dek)
	seg.dyp <- tclVar(Parameters$seg.dyp)

	txt.min <- tklabel(framePars, text = 'Min.Adj (in month)', anchor = 'e', justify = 'right')
	txt.seg <- tklabel(framePars, text = 'Segment to Adjust', anchor = 'e', justify = 'right')
	txt.mon <- tklabel(framePars, text = 'Month')
	txt.dek <- tklabel(framePars, text = 'Dekad')
	txt.dyp <- tklabel(framePars, text = label)

	en.min.mon <- tkentry(framePars, width = 4, textvariable = min.mon, justify = "center")
	en.min.dek <- tkentry(framePars, width = 4, textvariable = min.dek, justify = "center", state = state.dek)
	en.min.dyp <- tkentry(framePars, width = 4, textvariable = min.dyp, justify = "center", state = state.dyp)

	en.seg.mon <- tkentry(framePars, width = 4, textvariable = seg.mon, justify = "center")
	en.seg.dek <- tkentry(framePars, width = 4, textvariable = seg.dek, justify = "center", state = state.dek)
	en.seg.dyp <- tkentry(framePars, width = 4, textvariable = seg.dyp, justify = "center", state = state.dyp)

	tkgrid(txt.min, row = 1, column = 0, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
	tkgrid(txt.seg, row = 2, column = 0, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
	tkgrid(txt.mon, row = 0, column = 1, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
	tkgrid(txt.dek, row = 0, column = 2, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
	tkgrid(txt.dyp, row = 0, column = 3, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)

	tkgrid(en.min.mon, row = 1, column = 1, sticky = '', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
	tkgrid(en.min.dek, row = 1, column = 2, sticky = '', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
	tkgrid(en.min.dyp, row = 1, column = 3, sticky = '', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)

	tkgrid(en.seg.mon, row = 2, column = 1, sticky = '', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
	tkgrid(en.seg.dek, row = 2, column = 2, sticky = '', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
	tkgrid(en.seg.dyp, row = 2, column = 3, sticky = '', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)

	sapply(list(en.min.mon, en.min.dek, en.min.dyp), helpWidget,
			text_balloon = 'Minimum number of non-missing values to be used to adjust the series (in month)',
			text_statusbar = 'Minimum number of non-missing values to be used to adjust the series (in month)')

	sapply(list(en.seg.mon, en.seg.dek, en.seg.dyp), helpWidget,
			text_balloon = 'The segment to which the series will be adjusted (0: last segment)',
			text_statusbar = 'The segment to which the series will be adjusted (0: last segment)')

	###################

	tkgrid(framePars, row = 0, column = 0, sticky = 'we', padx = 1, ipadx = 1, pady = 3, ipady = 5)

	################################

	bt.prm.OK <- ttkbutton(frMRG1, text = .cdtEnv$tcl$lang$global[['button']][['1']])
	bt.prm.CA <- ttkbutton(frMRG1, text = .cdtEnv$tcl$lang$global[['button']][['2']])

	tkconfigure(bt.prm.OK, command = function(){
		Parameters$min.mon <<- as.numeric(str_trim(tclvalue(min.mon)))
		Parameters$min.dek <<- as.numeric(str_trim(tclvalue(min.dek)))
		Parameters$min.dyp <<- as.numeric(str_trim(tclvalue(min.dyp)))

		Parameters$seg.mon <<- as.numeric(str_trim(tclvalue(seg.mon)))
		Parameters$seg.dek <<- as.numeric(str_trim(tclvalue(seg.dek)))
		Parameters$seg.dyp <<- as.numeric(str_trim(tclvalue(seg.dyp)))

		tkgrab.release(tt)
		tkdestroy(tt)
		tkfocus(parent.win)
	})

	tkconfigure(bt.prm.CA, command = function(){
		tkgrab.release(tt)
		tkdestroy(tt)
		tkfocus(parent.win)
	})

	tkgrid(bt.prm.OK, row = 0, column = 0, sticky = 'w', padx = 5, pady = 1, ipadx = 1, ipady = 1)
	tkgrid(bt.prm.CA, row = 0, column = 1, sticky = 'e', padx = 5, pady = 1, ipadx = 1, ipady = 1)

	################################
	tkgrid(frMRG0, row = 0, column = 0, sticky = 'nswe', rowspan = 1, columnspan = 2, padx = 1, pady = 1, ipadx = 1, ipady = 1)
	tkgrid(frMRG1, row = 1, column = 1, sticky = 'se', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)

	tkwm.withdraw(tt)
	tcl('update')
	tt.w <- as.integer(tkwinfo("reqwidth", tt))
	tt.h <- as.integer(tkwinfo("reqheight", tt))
	tt.x <- as.integer(.cdtEnv$tcl$data$width.scr*0.5 - tt.w*0.5)
	tt.y <- as.integer(.cdtEnv$tcl$data$height.scr*0.5 - tt.h*0.5)
	tkwm.geometry(tt, paste0('+', tt.x, '+', tt.y))
	tkwm.transient(tt)
	tkwm.title(tt, 'Adjustment- Settings')
	tkwm.deiconify(tt)

	tkfocus(tt)
	tkbind(tt, "<Destroy>", function(){
		tkgrab.release(tt)
		tkfocus(parent.win)
	})
	tkwait.window(tt)
	return(Parameters)
}

############################################################################################

getParams.HomoRefSeries <- function(Parameters, parent.win = .cdtEnv$tcl$main$win)
{
	listOpenFiles <- openFile_ttkcomboList()
	if(WindowsOS())
		largeur1 <- .cdtEnv$tcl$fun$w.widgets(32)
	else
		largeur1 <- .cdtEnv$tcl$fun$w.widgets(26)

	# xml.dlg <- file.path(.cdtDir$dirLocal, "languages", "cdtQC_Homogeneity_dlgBox3.xml")
	# lang.dlg <- cdtLanguageParse(xml.dlg, .cdtData$Config$lang.iso)

	###################

	tt <- tktoplevel()
	tkgrab.set(tt)
	tkfocus(tt)

	frMRG0 <- tkframe(tt, relief = 'raised', borderwidth = 2)
	frMRG1 <- tkframe(tt)

	###################

	stateQRefS <- if(Parameters$user$refs) 'disabled' else 'normal'

	###################

	frTestS <- ttklabelframe(frMRG0, text = "Test series constitution", relief = "groove")

	diff.ratio <- tclVar(Parameters$diff.ratio)

	dif.rat1 <- tkradiobutton(frTestS, variable = diff.ratio, value = "1", text = "Difference", anchor = 'w', justify = 'left', state = stateQRefS)
	dif.rat2 <- tkradiobutton(frTestS, variable = diff.ratio, value = "2", text = "Ratio", anchor = 'w', justify = 'left', state = stateQRefS)
	dif.rat3 <- tkradiobutton(frTestS, variable = diff.ratio, value = "3", text = "LogRatio", anchor = 'w', justify = 'left', state = stateQRefS)

	tkgrid(dif.rat1, row = 0, column = 0, sticky = 'ew', padx = 1, pady = 1, ipadx = 1, ipady = 1)
	tkgrid(dif.rat2, row = 1, column = 0, sticky = 'ew', padx = 1, pady = 1, ipadx = 1, ipady = 1)
	tkgrid(dif.rat3, row = 2, column = 0, sticky = 'ew', padx = 1, pady = 1, ipadx = 1, ipady = 1)

	helpWidget(dif.rat1, 'Constitution of relative comparison series: [Candidate - Reference]',
						'Constitution of relative comparison series: [Candidate - Reference]')
	helpWidget(dif.rat2, 'Constitution of relative comparison series: [Candidate / Reference]',
						'Constitution of relative comparison series: [Candidate / Reference]')
	helpWidget(dif.rat3, 'Constitution of relative comparison series: [log(Candidate / Reference)]',
						'Constitution of relative comparison series: [log(Candidate / Reference)]')

	###################

	frWeig <- ttklabelframe(frMRG0, text = "Weighting factors", relief = "groove")

	weight.fac <- tclVar(Parameters$weight)

	wmean1 <- tkradiobutton(frWeig, variable = weight.fac, value = "1", text = "Correlation", anchor = 'w', justify = 'left', state = stateQRefS)
	wmean2 <- tkradiobutton(frWeig, variable = weight.fac, value = "2", text = "Distance", anchor = 'w', justify = 'left', state = stateQRefS)
	wmean3 <- tkradiobutton(frWeig, variable = weight.fac, value = "3", text = "Optimal", anchor = 'w', justify = 'left', state = stateQRefS)

	tkgrid(wmean1, row = 0, column = 0, sticky = 'ew', padx = 1, pady = 1, ipadx = 1, ipady = 1)
	tkgrid(wmean2, row = 1, column = 0, sticky = 'ew', padx = 1, pady = 1, ipadx = 1, ipady = 1)
	tkgrid(wmean3, row = 2, column = 0, sticky = 'ew', padx = 1, pady = 1, ipadx = 1, ipady = 1)

	helpWidget(wmean1, 'Use the square of the correlation coefficient as the weight factor',
						'Use the square of the correlation coefficient as the weight factor')
	helpWidget(wmean2, 'Use the square of the inverse of distance as the weight factor',
						'Use the square of the inverse of distance as the weight factor')
	helpWidget(wmean3, 'Optimal weighting using covariance matrix (ordinary kriging method)',
						'Optimal weighting using covariance matrix (ordinary kriging method)')

	###################

	frameVois <- ttklabelframe(frMRG0, text = "Neighborhood Selection", relief = 'groove')

	min.nbrs <- tclVar(Parameters$voisin$min)
	max.nbrs <- tclVar(Parameters$voisin$max)
	max.dst <- tclVar(Parameters$voisin$dist)
	elv.diff <- tclVar(Parameters$voisin$elv)
	min.rho <- tclVar(Parameters$voisin$rho)

	txt.min.nb <- tklabel(frameVois, text = 'Min number neighbors', anchor = 'e', justify = 'right')
	en.min.nb <- tkentry(frameVois, width = 3, textvariable = min.nbrs, justify = 'left', state = stateQRefS)
	txt.max.nb <- tklabel(frameVois, text = 'Max number neighbors', anchor = 'e', justify = 'right')
	en.max.nb <- tkentry(frameVois, width = 3, textvariable = max.nbrs, justify = 'left', state = stateQRefS)
	txt.max.dt <- tklabel(frameVois, text = 'Max search distance', anchor = 'e', justify = 'right')
	en.max.dt <- tkentry(frameVois, width = 5, textvariable = max.dst, justify = 'left', state = stateQRefS)
	txt.elv.dif <- tklabel(frameVois, text = 'Elevation difference', anchor = 'e', justify = 'right')
	en.elv.dif <- tkentry(frameVois, width = 5, textvariable = elv.diff, justify = 'left', state = stateQRefS)
	fr.min.rho <- tkframe(frameVois)

	txt.min.rho <- tklabel(fr.min.rho, text = 'Minimum correlation', anchor = 'e', justify = 'right')
	en.min.rho <- tkentry(fr.min.rho, width = 4, textvariable = min.rho, justify = 'left', state = stateQRefS)
	tkgrid(txt.min.rho, en.min.rho)

	tkgrid(txt.min.nb, row = 0, column = 0, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
	tkgrid(en.min.nb, row = 0, column = 1, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
	tkgrid(txt.max.nb, row = 1, column = 0, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
	tkgrid(en.max.nb, row = 1, column = 1, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
	tkgrid(txt.max.dt, row = 0, column = 2, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
	tkgrid(en.max.dt, row = 0, column = 3, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
	tkgrid(txt.elv.dif, row = 1, column = 2, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
	tkgrid(en.elv.dif, row = 1, column = 3, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
	tkgrid(fr.min.rho, row = 2, column = 0, sticky = '', rowspan = 1, columnspan = 4, padx = 1, pady = 1, ipadx = 1, ipady = 1)

	helpWidget(en.min.nb, 'Minimum number of neighbors stations to be selected', 'Minimum number of neighbors stations to be selected')
	helpWidget(en.max.nb, 'Maximum number of neighbors stations to be selected', 'Maximum number of neighbors stations to be selected')
	helpWidget(en.max.dt, 'Maximum search distance (in km) for neighbors stations', 'Maximum search distance (in km) for neighbors stations')
	helpWidget(en.elv.dif, 'Maximum altitude difference (in m) for neighbors stations to be selected',
							'Maximum altitude difference (in m) for neighbors stations to be selected')
	helpWidget(en.min.rho, 'Minimum correlation coefficient between candidate and neighbor series',
							'Minimum correlation coefficient between candidate and neighbor series')

	###################

	frameELV <- tkframe(frMRG0, relief = 'groove', borderwidth = 2)

	use.elev <- tclVar(Parameters$elv$use)

	stateElev <- if(Parameters$elv$use) 'normal' else 'disabled'

	chk.elev <- tkcheckbutton(frameELV, variable = use.elev, text = "Use Elevation to select neighbors", anchor = 'w', justify = 'left', state = stateQRefS)
	bt.elev <- ttkbutton(frameELV, text = .cdtEnv$tcl$lang$global[['button']][['5']], state = stateElev)

	tkconfigure(bt.elev, command = function(){
		Params <- Parameters[["elv"]]
		Parameters[["elv"]] <<- getParams.QC.Elevation(Params, tt)
	})

	tkgrid(chk.elev, row = 0, column = 0, sticky = 'we', rowspan = 1, columnspan = 2, padx = 1, pady = 1, ipadx = 1, ipady = 1)
	tkgrid(bt.elev, row = 0, column = 2, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)

	helpWidget(chk.elev, 'Check this box if you want to use elevation data when selecting neighbors stations',
						'Check this box if you want to use elevation data when selecting neighbors stations')
	helpWidget(bt.elev, 'Select the elevation data to be used', 'Select the elevation data to be used')

	###############
	tkbind(chk.elev, "<Button-1>", function(){
		if(tclvalue(user.ref) == '0'){
			stateElev <- if(tclvalue(use.elev) == '1') 'disabled' else 'normal'
			tkconfigure(bt.elev, state = stateElev)
		}
	})

	###################

	frameUSER <- tkframe(frMRG0, relief = 'groove', borderwidth = 2)

	user.ref <- tclVar(Parameters$user$refs)
	user.file <- tclVar(Parameters$user$file)

	stateUser <- if(Parameters$user$refs) 'normal' else 'disabled'

	chk.user <- tkcheckbutton(frameUSER, variable = user.ref, text = "Stations provided by User", anchor = 'w', justify = 'left')
	txt.user <- tklabel(frameUSER, text = 'File containing stations references series', anchor = 'w', justify = 'left')
	cb.user <- ttkcombobox(frameUSER, values = unlist(listOpenFiles), textvariable = user.file, width = largeur1, state = stateUser)
	bt.user <- tkbutton(frameUSER, text = "...", state = stateUser)

	tkconfigure(bt.user, command = function(){
		dat.opfiles <- getOpenFiles(tt)
		if(!is.null(dat.opfiles)){
			update.OpenFiles('ascii', dat.opfiles)
			listOpenFiles[[length(listOpenFiles) + 1]] <<- dat.opfiles[[1]]
			tclvalue(user.file) <- dat.opfiles[[1]]
			tkconfigure(cb.user, values = unlist(listOpenFiles))
		}
	})

	tkgrid(chk.user, row = 0, column = 0, sticky = 'we', rowspan = 1, columnspan = 5, padx = 1, pady = 1, ipadx = 1, ipady = 1)
	tkgrid(txt.user, row = 1, column = 0, sticky = 'we', rowspan = 1, columnspan = 5, padx = 1, pady = 1, ipadx = 1, ipady = 1)
	tkgrid(cb.user, row = 2, column = 0, sticky = 'we', rowspan = 1, columnspan = 4, padx = 0, pady = 1, ipadx = 1, ipady = 1)
	tkgrid(bt.user, row = 2, column = 4, sticky = 'w', rowspan = 1, columnspan = 1, padx = 0, pady = 1, ipadx = 1, ipady = 1)

	helpWidget(chk.user, 'The references series are preset by user,\nthe ID and order of the stations must be the same as the the candidates series',
						'The references series are preset by user,\nthe ID and order of the stations must be the same as the the candidates series')
	helpWidget(cb.user, 'Select the file containing the references series', 'Select the file containing the references series')
	helpWidget(bt.user, 'Browse file if not listed', 'Browse file if not listed')

	###################

	tkbind(chk.user, "<Button-1>", function(){
		stateUser <- if(tclvalue(user.ref) == '1') 'disabled' else 'normal'
		tkconfigure(cb.user, state = stateUser)
		tkconfigure(bt.user, state = stateUser)

		stateQRefS <- if(tclvalue(user.ref) == '1') 'normal' else 'disabled'
		tkconfigure(dif.rat1, state = stateQRefS)
		tkconfigure(dif.rat2, state = stateQRefS)
		tkconfigure(dif.rat3, state = stateQRefS)
		tkconfigure(wmean1, state = stateQRefS)
		tkconfigure(wmean2, state = stateQRefS)
		tkconfigure(wmean3, state = stateQRefS)
		tkconfigure(en.min.nb, state = stateQRefS)
		tkconfigure(en.max.nb, state = stateQRefS)
		tkconfigure(en.max.dt, state = stateQRefS)
		tkconfigure(en.elv.dif, state = stateQRefS)
		tkconfigure(en.min.rho, state = stateQRefS)

		tkconfigure(chk.elev, state = stateQRefS)
		if(tclvalue(user.ref) == '1'){
			stateElev <- if(tclvalue(use.elev) == '0') 'disabled' else 'normal'
		}else stateElev <- 'disabled'
		tkconfigure(bt.elev, state = stateElev)
	})

	###################

	tkgrid(frTestS, row = 0, column = 0, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, ipadx = 1, pady = 1, ipady = 1)
	tkgrid(frWeig, row = 0, column = 1, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, ipadx = 1, pady = 1, ipady = 1)
	tkgrid(frameVois, row = 1, column = 0, sticky = 'we', rowspan = 1, columnspan = 2, padx = 1, ipadx = 1, pady = 1, ipady = 1)
	tkgrid(frameELV, row = 2, column = 0, sticky = '', rowspan = 1, columnspan = 2, padx = 1, ipadx = 1, pady = 1, ipady = 1)
	tkgrid(frameUSER, row = 3, column = 0, sticky = '', rowspan = 1, columnspan = 2, padx = 1, ipadx = 1, pady = 1, ipady = 1)

	################################

	bt.prm.OK <- ttkbutton(frMRG1, text = .cdtEnv$tcl$lang$global[['button']][['1']])
	bt.prm.CA <- ttkbutton(frMRG1, text = .cdtEnv$tcl$lang$global[['button']][['2']])

	tkconfigure(bt.prm.OK, command = function(){
		Parameters$diff.ratio <<- as.numeric(str_trim(tclvalue(diff.ratio)))
		Parameters$weight <<- as.numeric(str_trim(tclvalue(weight.fac)))

		Parameters$voisin$min <<- as.numeric(str_trim(tclvalue(min.nbrs)))
		Parameters$voisin$max <<- as.numeric(str_trim(tclvalue(max.nbrs)))
		Parameters$voisin$dist <<- as.numeric(str_trim(tclvalue(max.dst)))
		Parameters$voisin$elv <<- as.numeric(str_trim(tclvalue(elv.diff)))
		Parameters$voisin$rho <<- as.numeric(str_trim(tclvalue(min.rho)))

		Parameters$elv$use <<- switch(tclvalue(use.elev), '0' = FALSE, '1' = TRUE)
		Parameters$user$refs <<- switch(tclvalue(user.ref), '0' = FALSE, '1' = TRUE)
		Parameters$user$file <<- str_trim(tclvalue(user.file))

		tkgrab.release(tt)
		tkdestroy(tt)
		tkfocus(parent.win)
	})

	tkconfigure(bt.prm.CA, command = function(){
		tkgrab.release(tt)
		tkdestroy(tt)
		tkfocus(parent.win)
	})

	tkgrid(bt.prm.OK, row = 0, column = 0, sticky = 'w', padx = 5, pady = 1, ipadx = 1, ipady = 1)
	tkgrid(bt.prm.CA, row = 0, column = 1, sticky = 'e', padx = 5, pady = 1, ipadx = 1, ipady = 1)

	################################
	tkgrid(frMRG0, row = 0, column = 0, sticky = 'nswe', rowspan = 1, columnspan = 2, padx = 1, pady = 1, ipadx = 1, ipady = 1)
	tkgrid(frMRG1, row = 1, column = 1, sticky = 'se', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)

	tkwm.withdraw(tt)
	tcl('update')
	tt.w <- as.integer(tkwinfo("reqwidth", tt))
	tt.h <- as.integer(tkwinfo("reqheight", tt))
	tt.x <- as.integer(.cdtEnv$tcl$data$width.scr*0.5 - tt.w*0.5)
	tt.y <- as.integer(.cdtEnv$tcl$data$height.scr*0.5 - tt.h*0.5)
	tkwm.geometry(tt, paste0('+', tt.x, '+', tt.y))
	tkwm.transient(tt)
	tkwm.title(tt, 'Reference series creation')
	tkwm.deiconify(tt)

	tkfocus(tt)
	tkbind(tt, "<Destroy>", function(){
		tkgrab.release(tt)
		tkfocus(parent.win)
	})
	tkwait.window(tt)
	return(Parameters)
}
