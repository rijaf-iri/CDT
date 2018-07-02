
grads.getInfoNetcdfData <- function(parent.win, Parameters, ncDIR){
	listOpenFiles <- openFile_ttkcomboList()
	if(WindowsOS()){
		largeur1 <- 43
		largeur2 <- 28
	}else{
		largeur1 <- 34
		largeur2 <- 31
	}

	xml.dlg <- file.path(.cdtDir$dirLocal, "languages", "cdtInfoNetcdfData_Grads_dlgBox.xml")
	lang.dlg <- cdtLanguageParse(xml.dlg, .cdtData$Config$lang.iso)

	###################

	tt1 <- tktoplevel()
	tkgrab.set(tt1)
	tkfocus(tt1)

	frMRG0 <- tkframe(tt1, relief = 'raised', borderwidth = 2)
	frMRG1 <- tkframe(tt1)

	###################

	frFF <- tkframe(frMRG0, relief = "sunken", borderwidth = 2)

	inrfeff <- tclVar(Parameters$format)
	rfesample <- tclVar(Parameters$sample)

	txt.ncsample <- tklabel(frFF, text = lang.dlg[['label']][['1']], anchor = 'w', justify = 'left')
	cb.ncsample <- ttkcombobox(frFF, values = unlist(listOpenFiles), textvariable = rfesample, width = largeur1)
	bt.ncsample <- tkbutton(frFF, text = "...")
	txt.inrfeff <- tklabel(frFF, text = lang.dlg[['label']][['2']], anchor = 'w', justify = 'left')
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

	status.bar.display(cb.ncsample, lang.dlg[['status']][['1']])
	infobulle(bt.ncsample, lang.dlg[['tooltip']][['1']])
	helpWidget(en.inrfeff, lang.dlg[['tooltip']][['2']], lang.dlg[['status']][['2']])

	###################

	frHH <- tkframe(frMRG0, relief = "sunken", borderwidth = 2)

	yscr.Help <- tkscrollbar(frHH, repeatinterval = 4, command = function(...) tkyview(txta.Help, ...))
	txta.Help <- tktext(frHH, bg = "white", font = "courier", cursor = "", wrap = "word",
							height = 7, width = largeur2,
							yscrollcommand = function(...) tkset(yscr.Help, ...))

	tkgrid(txta.Help, yscr.Help)
	tkgrid.configure(yscr.Help, sticky = "ns")
	tkgrid.configure(txta.Help, sticky = 'nswe')

	TXTA1 <- lang.dlg[['label']][['3']]
	TXTA2 <- lang.dlg[['label']][['4']]
	TXTA3 <- lang.dlg[['label']][['5']]
	TXTA4a <- "rr_mrg_19810731_ALL.nc"
	TXTA4b <- "rr_mrg_%Y%M%D_ALL.nc\n\n"
	TXTA5a <- "rfe_198309.nc"
	TXTA5b <- "rfe_%Y%M.nc"
	TXTAby <- " => "

	font1 <- tkfont.create(family = "times", size = 12, weight = "bold")
	font2 <- tkfont.create(family = "times", size = 12, weight = "bold", underline = TRUE)
	font3 <- tkfont.create(family = "courier", size = 11)
	font4 <- tkfont.create(family = "courier", size = 11, weight = "bold")

	tktag.configure(txta.Help, "font1.tag", font = font1, foreground = 'red')
	tktag.configure(txta.Help, "font2.tag", font = font2)
	tktag.configure(txta.Help, "font3.tag", font = font3)
	tktag.configure(txta.Help, "font4.tag", font = font4, foreground = 'red')

	tkinsert(txta.Help, "end", TXTA1, "font3.tag")
	tkinsert(txta.Help, "end", TXTA2, "font1.tag")
	tkinsert(txta.Help, "end", TXTA3, "font2.tag")
	tkinsert(txta.Help, "end", TXTA4a, "font3.tag")
	tkinsert(txta.Help, "end", TXTAby, "font4.tag")
	tkinsert(txta.Help, "end", TXTA4b, "font3.tag")
	tkinsert(txta.Help, "end", TXTA5a, "font3.tag")
	tkinsert(txta.Help, "end", TXTAby, "font4.tag")
	tkinsert(txta.Help, "end", TXTA5b, "font3.tag")

	tkconfigure(txta.Help, state = "disabled")

	###################

	tkgrid(frFF, row = 0, column = 0, sticky = 'ew', padx = 1, pady = 1)
	tkgrid(frHH, row = 1, column = 0, sticky = 'ew', padx = 1, pady = 1)

	################################

	bt.prm.OK <- ttkbutton(frMRG1, text = .cdtEnv$tcl$lang$global[['button']][['1']])
	bt.prm.CA <- ttkbutton(frMRG1, text = .cdtEnv$tcl$lang$global[['button']][['2']])

	tkconfigure(bt.prm.OK, command = function(){
		if(str_trim(tclvalue(rfesample)) == ""){
			tkmessageBox(message = lang.dlg[['message']][['1']], icon = "warning", type = "ok")
			tkwait.window(tt1)
		}else{
			Parameters$format <<- str_trim(tclvalue(inrfeff))
			Parameters$sample <<- str_trim(tclvalue(rfesample))

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
	return(Parameters)
}

