
extractTS.previewWin <- function(states, shpL, type){
	listOpenFiles <- openFile_ttkcomboList()
	if(WindowsOS()){
		largeur <- .cdtEnv$tcl$fun$w.widgets(32)
		largeur1 <- .cdtEnv$tcl$fun$w.widgets(34)
		wtext <- .cdtEnv$tcl$fun$w.widgets(27)
	}else{
		largeur <- .cdtEnv$tcl$fun$w.widgets(27)
		largeur1 <- .cdtEnv$tcl$fun$w.widgets(31)
		wtext <- .cdtEnv$tcl$fun$w.widgets(35)
	}

	# xml.dlg <- file.path(.cdtDir$dirLocal, "languages", "cdtExtractData_dlgBox.xml")
	# lang.dlg <- cdtLanguageParse(xml.dlg, .cdtData$Config$lang.iso)

	#############################

	tt <- tktoplevel()
	frA <- tkframe(tt, relief = "raised", borderwidth = 2)
	frB <- tkframe(tt)

	#############################

	if(type == 'mpoint'){
		titre <- 'Multiple Points'
		frameType <- ttklabelframe(frA, text = titre, relief = 'groove', borderwidth = 2)

		coordsfiles <- tclVar()
		coordsfrom <- tclVar('crd')

		crdfrm1 <- tkradiobutton(frameType, variable = coordsfrom, value = "crd", text = "From CDT coordinates file", anchor = 'w', justify = 'left', state = states[1])
		crdfrm2 <- tkradiobutton(frameType, variable = coordsfrom, value = "cdt", text = "From CDT stations data format", anchor = 'w', justify = 'left', state = states[1])
		cbmltpts <- ttkcombobox(frameType, values = unlist(listOpenFiles), textvariable = coordsfiles, state = states[1], width = largeur)
		btmltpts <- tkbutton(frameType, text = "...", state = states[1])

		#########
		tkconfigure(btmltpts, command = function(){
			tkdelete(textObj, "0.0", "end")
			dat.opfiles <- getOpenFiles(.cdtEnv$tcl$main$win)
			if(!is.null(dat.opfiles)){
				update.OpenFiles('ascii', dat.opfiles)
				listOpenFiles[[length(listOpenFiles) + 1]] <<- dat.opfiles[[1]]
				tclvalue(coordsfiles) <- dat.opfiles[[1]]
				tkconfigure(cbmltpts, values = unlist(listOpenFiles))
				
				crds <- dat.opfiles[[2]]
				if(tclvalue(coordsfrom) == 'crd') crds <- crds[, c(1, 3, 4), drop = FALSE]
				if(tclvalue(coordsfrom) == 'cdt') crds <- t(crds[1:3, -1, drop = FALSE])
				for(i in 1:nrow(crds)) tkinsert(textObj, "end", paste(crds[i, 1], crds[i, 2], crds[i, 3], "\n"))
			}
		})

		#########

		tkgrid(crdfrm1, row = 0, column = 0, sticky = 'we', rowspan = 1, columnspan = 5, padx = 1, pady = 1, ipadx = 1, ipady = 1)
		tkgrid(crdfrm2, row = 1, column = 0, sticky = 'we', rowspan = 1, columnspan = 5, padx = 1, pady = 1, ipadx = 1, ipady = 1)
		tkgrid(cbmltpts, row = 2, column = 0, sticky = 'we', rowspan = 1, columnspan = 4, padx = 0, pady = 1, ipadx = 1, ipady = 1)
		tkgrid(btmltpts, row = 2, column = 4, sticky = 'w', rowspan = 1, columnspan = 1, padx = 0, pady = 1, ipadx = 1, ipady = 1)

		infobulle(crdfrm1, 'The coordinates come from a coordinate file')
		status.bar.display(crdfrm1, 'The coordinates come from a coordinate file')
		infobulle(crdfrm2, 'The coordinates come from a CDT data file')
		status.bar.display(crdfrm2, 'The coordinates come from a CDT data file')
		infobulle(cbmltpts, 'Choose the file in the list')
		status.bar.display(cbmltpts, 'File containing the ids and coordinates of points to be extracted')
		infobulle(btmltpts, 'Browse file if not listed')
		status.bar.display(btmltpts, 'Browse file if not listed')

		#########
		tkbind(cbmltpts, "<<ComboboxSelected>>", function(){
			tkdelete(textObj, "0.0", "end")
			crds <- getStnOpenData(coordsfiles)
			if(tclvalue(coordsfrom) == 'crd') crds <- crds[, c(1, 3, 4), drop = FALSE]
			if(tclvalue(coordsfrom) == 'cdt') crds <- t(crds[1:3, -1, drop = FALSE])
			for(i in 1:nrow(crds)) tkinsert(textObj, "end", paste(crds[i, 1], crds[i, 2], crds[i, 3], "\n"))
		})
	}

	#############################

	if(type == 'mpoly'){ 
		titre <- 'Multiple Polygons'
		frameType <- ttklabelframe(frA, text = titre, relief = 'groove', borderwidth = 2)

		btmpoly <- ttkbutton(frameType, text = 'Get All Polygons', state = states[2], width = largeur1)

		tkconfigure(btmpoly, command = function(){
			tkdelete(textObj, "0.0", "end")
			shpf <- getShpOpenData(shpL[[2]])
			if(!is.null(shpf)){
				dat <- shpf[[2]]@data
				adminN <- as.character(dat[, as.numeric(tclvalue(tcl(shpL[[1]], 'current'))) + 1])
				shpAttr <- levels(as.factor(adminN))
				for(i in 1:length(shpAttr)) tkinsert(textObj, "end", paste0(shpAttr[i], "\n"))
			}
		})

		#########

		tkgrid(btmpoly, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 3, ipadx = 1, ipady = 1)

		infobulle(btmpoly, 'Get all the polygons from the shapefile')
		status.bar.display(btmpoly, 'Get all the polygons from the shapefile')
	}

	########################

	btClear <- ttkbutton(frA, text = "Clear all")

	tkconfigure(btClear, command = function(){
		tkdelete(textObj, "0.0", "end")
		.cdtData$EnvData$multiptspoly <- NULL
	})

	########################

	frameText <- tkframe(frA, relief = 'groove', borderwidth = 2)

	yscr <- tkscrollbar(frameText, repeatinterval = 4,
						command = function(...) tkyview(textObj, ...))
	textObj <- tktext(frameText, bg = "white", wrap = "none", height = 5, width = wtext,
						yscrollcommand = function(...) tkset(yscr, ...))

	tkgrid(textObj, yscr)
	tkgrid.configure(yscr, sticky = "ns")
	tkgrid.configure(textObj, sticky = 'nswe')

	#########

	if(!is.null(.cdtData$EnvData$multiptspoly))
		tkinsert(textObj, "end", .cdtData$EnvData$multiptspoly)

	#############################
	tkgrid(frameType, row = 0, column = 0, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
	tkgrid(btClear, row = 1, column = 0, sticky = '', rowspan = 1, columnspan = 1, padx = 1, pady = 5, ipadx = 1, ipady = 1)
	tkgrid(frameText, row = 2, column = 0, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)

	#############################

	btOK <- ttkbutton(frB, text = .cdtEnv$tcl$lang$global[['button']][['1']])
	btCA <- ttkbutton(frB, text = .cdtEnv$tcl$lang$global[['button']][['2']])

	ret.params <- list(win = tt, textObj = textObj)
	.cdtData$EnvData$dlgBoxOpen <- TRUE

	tkconfigure(btOK, command = function(){
		retvars <- tclvalue(tkget(textObj, "0.0", "end"))
		.cdtData$EnvData$multiptspoly <- retvars
		tkdestroy(tt)
		tkfocus(.cdtEnv$tcl$main$win)
		ret.params <<- NULL
	})

	tkconfigure(btCA, command = function(){
		tkdestroy(tt)
		tkfocus(.cdtEnv$tcl$main$win)
		ret.params <<- NULL
	})

	########################
	tkgrid(btOK, row = 0, column = 0, sticky = 'w', padx = 5, pady = 1, ipadx = 1, ipady = 1)
	tkgrid(btCA, row = 0, column = 1, sticky = 'e', padx = 5, pady = 1, ipadx = 1, ipady = 1)

	########################

	tkgrid(frA, row = 0, column = 0, sticky = 'nswe', rowspan = 1, columnspan = 2, padx = 1, pady = 1, ipadx = 1, ipady = 1)
	tkgrid(frB, row = 1, column = 1, sticky = 'se', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)

	#########################
	tkwm.withdraw(tt)
	tcl('update')
	tkwm.geometry(tt, '+5+15')
	tkwm.transient(tt, .cdtEnv$tcl$main$win)
	tkwm.title(tt, titre)
	tkwm.deiconify(tt)

	tkfocus(.cdtEnv$tcl$main$win)
	tkbind(tt, "<Destroy>", function(){
		tkfocus(.cdtEnv$tcl$main$win)
		ret.params <<- NULL
		.cdtData$EnvData$dlgBoxOpen <- FALSE
	})
	return(ret.params)
}
