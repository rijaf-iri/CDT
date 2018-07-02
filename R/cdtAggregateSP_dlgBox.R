
AggregateNcdf_GetInfo <- function(){
	listOpenFiles <- openFile_ttkcomboList()
	if(WindowsOS()){
		largeur <- 42
		largeur1 <- 46
		largeur2 <- 13
		largeur3 <- 29
	}else{
		largeur <- 32
		largeur1 <- 34
		largeur2 <- 11
		largeur3 <- 31
	}

	xml.dlg <- file.path(.cdtDir$dirLocal, "languages", "cdtAggregateSP_dlgBox.xml")
	lang.dlg <- cdtLanguageParse(xml.dlg, .cdtData$Config$lang.iso)

	###################

	infoOpenNC <- function(ncfichier){
		ncDataInfo <- getNCDFSampleData(ncfichier)
		coords <- expand.grid(ncDataInfo[c('lon', 'lat')])
		coordinates(coords) <- ~lon+lat
		ncDataInfo <- SpatialPixels(points = coords,
									tolerance = sqrt(sqrt(.Machine$double.eps)),
									proj4string = CRS(as.character(NA)))
		nxy <- ncDataInfo@grid
		minlon <- nxy@cellcentre.offset["lon"]
		reslon <- nxy@cellsize["lon"]
		maxlon <- minlon + reslon * (nxy@cells.dim["lon"] - 1)
		minlat <- nxy@cellcentre.offset["lat"]
		reslat <- nxy@cellsize["lat"]
		maxlat <- minlat + reslat * (nxy@cells.dim["lat"] - 1)

		if(tclvalue(use.ncgrid) == '0'){
			tclvalue(minLon) <- round(minlon, 6)
			tclvalue(maxLon) <- round(maxlon, 6)
			# tclvalue(resLon) <- round(reslon, 6)
			tclvalue(minLat) <- round(minlat, 6)
			tclvalue(maxLat) <- round(maxlat, 6)
			# tclvalue(resLat) <- round(reslat, 6)
		}

		tkconfigure(txta.ncinfo, state = "normal")
		tkdelete(txta.ncinfo, "0.0", "end")
		tkinsert(txta.ncinfo, "end", paste("min.lon:", round(minlon, 6), "/ max.lon:",
				round(maxlon, 6), "/ res.lon:", round(reslon, 6), "\n"), "txtfonttag")
		tkinsert(txta.ncinfo, "end", paste("min.lat:", round(minlat, 6), "/ max.lat:",
				round(maxlat, 6), "/ res.lat:", round(reslat, 6)), "txtfonttag")
		tkconfigure(txta.ncinfo, state = "disabled")
	}

	infoOpenNC1 <- function(ncfichier){
		ncDataInfo <- getNCDFSampleData(ncfichier)
		coords <- expand.grid(ncDataInfo[c('lon', 'lat')])
		coordinates(coords) <- ~lon+lat
		ncDataInfo <- SpatialPixels(points = coords,
									tolerance = sqrt(sqrt(.Machine$double.eps)),
									proj4string = CRS(as.character(NA)))
		nxy <- ncDataInfo@grid
		minlon <- nxy@cellcentre.offset["lon"]
		reslon <- nxy@cellsize["lon"]
		maxlon <- minlon + reslon * (nxy@cells.dim["lon"] - 1)
		minlat <- nxy@cellcentre.offset["lat"]
		reslat <- nxy@cellsize["lat"]
		maxlat <- minlat + reslat * (nxy@cells.dim["lat"] - 1)

		tclvalue(minLon) <- round(minlon, 6)
		tclvalue(maxLon) <- round(maxlon, 6)
		tclvalue(resLon) <- round(reslon, 6)
		tclvalue(minLat) <- round(minlat, 6)
		tclvalue(maxLat) <- round(maxlat, 6)
		tclvalue(resLat) <- round(reslat, 6)
	}

	############################################

	tt <- tktoplevel()
	tkgrab.set(tt)
	tkfocus(tt)

	frMRG0 <- tkframe(tt, relief = 'raised', borderwidth = 2)
	frMRG1 <- tkframe(tt)

	frLeft <- tkframe(frMRG0, relief = "groove", borderwidth = 2)
	frRight <- tkframe(frMRG0, relief = "groove", borderwidth = 2)

	############################################

	frNCDF <- tkframe(frLeft, relief = "sunken", borderwidth = 2)

	frNCDATA <- ttklabelframe(frNCDF, text = lang.dlg[['label']][['1']], relief = 'groove')

	nbcnfile <- tclVar()
	NBNCF <- lang.dlg[['combobox']][['1']]
	NBNC <- c('one', 'several')
	tclvalue(nbcnfile) <- NBNCF[NBNC %in% .cdtData$GalParams$nb.ncfile]

	ncfiledir <- tclVar(.cdtData$GalParams$ncdf$fileordir)
	ncsample <- tclVar(.cdtData$GalParams$ncdf$sample)

	statesample <- if(.cdtData$GalParams$nb.ncfile == "one") "disabled" else "normal"
	txtfiledir <- if(.cdtData$GalParams$nb.ncfile == "one") lang.dlg[['label']][['2']] else lang.dlg[['label']][['3']]
	fileINdir <- tclVar(txtfiledir)

	cb.nbncf <- ttkcombobox(frNCDATA, values = NBNCF, textvariable = nbcnfile, width = largeur)

	txt.ncfldir <- tklabel(frNCDATA, text = tclvalue(fileINdir), textvariable = fileINdir, anchor = 'w', justify = 'left')
	if(.cdtData$GalParams$nb.ncfile == "one"){
		cb.ncfldir <- ttkcombobox(frNCDATA, values = unlist(listOpenFiles), textvariable = ncfiledir, width = largeur)
	}else{
		cb.ncfldir <- tkentry(frNCDATA, textvariable = ncfiledir, width = largeur1)
	}
	bt.ncfldir <- tkbutton(frNCDATA, text = "...")

	txt.ncsample <- tklabel(frNCDATA, text = lang.dlg[['label']][['4']], anchor = 'w', justify = 'left')
	cb.ncsample <- ttkcombobox(frNCDATA, values = unlist(listOpenFiles), textvariable = ncsample, width = largeur, state = statesample)
	bt.ncsample <- tkbutton(frNCDATA, text = "...", state = statesample)

	####
	frNCINFO <- tkframe(frNCDF, relief = 'groove', borderwidth = 2)

	txta.ncinfo <- tktext(frNCINFO, bg = "white", font = "courier", cursor = "", wrap = "word", height = 2, width = largeur3)
	txtafont <- tkfont.create(family = "times", size = 9)
	tktag.configure(txta.ncinfo, "txtfonttag", font = txtafont)

	###################

	tkconfigure(bt.ncfldir, command = function(){
		if(tclvalue(nbcnfile) == NBNCF[1]){
			nc.opfiles <- getOpenNetcdf(tt, initialdir = getwd())
			if(!is.null(nc.opfiles)){
				update.OpenFiles('netcdf', nc.opfiles)
				listOpenFiles[[length(listOpenFiles) + 1]] <<- nc.opfiles[[1]]
				tclvalue(ncfiledir) <- nc.opfiles[[1]]

				lapply(list(cb.ncfldir, cb.ncsample, cb.ncgrid), tkconfigure, values = unlist(listOpenFiles))
				####
				infoOpenNC(tclvalue(ncfiledir))
			}
		}else{
			file2convert <- tk_choose.dir(getwd(), "")
			tclvalue(ncfiledir) <- if(!is.na(file2convert)) file2convert else ""
		}
	})

	tkconfigure(bt.ncsample, command = function(){
		initialdir <- if(file.exists(tclvalue(ncfiledir))) tclvalue(ncfiledir) else getwd()
		nc.opfiles <- getOpenNetcdf(tt, initialdir = initialdir)
		if(!is.null(nc.opfiles)){
			update.OpenFiles('netcdf', nc.opfiles)
			listOpenFiles[[length(listOpenFiles) + 1]] <<- nc.opfiles[[1]]
			tclvalue(ncsample) <- nc.opfiles[[1]]

			lapply(list(cb.ncsample, cb.ncgrid), tkconfigure, values = unlist(listOpenFiles))
			####
			infoOpenNC(tclvalue(ncsample))
		}
	})

	###################

	tkgrid(cb.nbncf, row = 0, column = 0, sticky = 'we', rowspan = 1, columnspan = 4, padx = 1, pady = 1, ipadx = 1, ipady = 1)

	tkgrid(txt.ncfldir, row = 1, column = 0, sticky = 'we', rowspan = 1, columnspan = 5, padx = 1, pady = 1, ipadx = 1, ipady = 1)
	tkgrid(cb.ncfldir, row = 2, column = 0, sticky = 'we', rowspan = 1, columnspan = 4, padx = 0, pady = 1, ipadx = 1, ipady = 1)
	tkgrid(bt.ncfldir, row = 2, column = 4, sticky = 'w', rowspan = 1, columnspan = 1, padx = 0, pady = 1, ipadx = 1, ipady = 1)

	tkgrid(txt.ncsample, row = 3, column = 0, sticky = 'we', rowspan = 1, columnspan = 5, padx = 1, pady = 1, ipadx = 1, ipady = 1)
	tkgrid(cb.ncsample, row = 4, column = 0, sticky = 'we', rowspan = 1, columnspan = 4, padx = 0, pady = 1, ipadx = 1, ipady = 1)
	tkgrid(bt.ncsample, row = 4, column = 4, sticky = 'w', rowspan = 1, columnspan = 1, padx = 0, pady = 1, ipadx = 1, ipady = 1)

	###################
	tkgrid(txta.ncinfo, row = 5, column = 0, sticky = "nsew", rowspan = 1, columnspan = 5, padx = 1, pady = 1, ipadx = 1, ipady = 1)

	###################

	tkgrid(frNCDATA, row = 0, column = 0, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
	tkgrid(frNCINFO, row = 1, column = 0, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)

	###################
	tkbind(cb.nbncf, "<<ComboboxSelected>>", function(){
		tkdestroy(cb.ncfldir)
		tclvalue(ncfiledir) <- ''

		if(tclvalue(nbcnfile) == NBNCF[1]){
			tclvalue(fileINdir) <- lang.dlg[['label']][['2']]

			cb.ncfldir <- ttkcombobox(frNCDATA, values = unlist(listOpenFiles), textvariable = ncfiledir, width = largeur)

			#######
			tkconfigure(bt.ncfldir, command = function(){
				nc.opfiles <- getOpenNetcdf(tt, initialdir = getwd())
				if(!is.null(nc.opfiles)){
					update.OpenFiles('netcdf', nc.opfiles)
					listOpenFiles[[length(listOpenFiles) + 1]] <<- nc.opfiles[[1]]
					tclvalue(ncfiledir) <- nc.opfiles[[1]]

					lapply(list(cb.ncfldir, cb.ncsample, cb.ncgrid), tkconfigure, values = unlist(listOpenFiles))
					####
					infoOpenNC(tclvalue(ncfiledir))
				}
			})

			tkconfigure(cb.ncsample, state = 'disabled')
			tkconfigure(bt.ncsample, state = 'disabled')
		}

		#######
		if(tclvalue(nbcnfile) == NBNCF[2]){
			tclvalue(fileINdir) <- lang.dlg[['label']][['3']]

			cb.ncfldir <- tkentry(frNCDATA, textvariable = ncfiledir, width = largeur1)

			#######
			tkconfigure(bt.ncfldir, command = function(){
				file2convert <- tk_choose.dir(getwd(), "")
				tclvalue(ncfiledir) <- if(!is.na(file2convert)) file2convert else ""
			})

			tkconfigure(cb.ncsample, state = 'normal')
			tkconfigure(bt.ncsample, state = 'normal')
		}

		#######
		tkgrid(cb.ncfldir, row = 2, column = 0, sticky = 'we', rowspan = 1, columnspan = 4, padx = 0, pady = 1, ipadx = 1, ipady = 1)

		if(tclvalue(nbcnfile) == NBNCF[1]){
			tkbind(cb.ncfldir, "<<ComboboxSelected>>", function(){
				infoOpenNC(tclvalue(ncfiledir))
			})
		}

		tkfocus(tt)
	})

	###################
	if(tclvalue(nbcnfile) == NBNCF[1]){
		tkbind(cb.ncfldir, "<<ComboboxSelected>>", function(){
			infoOpenNC(tclvalue(ncfiledir))
		})
	}

	###################
	tkbind(cb.ncsample, "<<ComboboxSelected>>", function(){
		infoOpenNC(tclvalue(ncsample))
	})

	############################################

	frNCGRID <- tkframe(frLeft, relief = "sunken", borderwidth = 2)

	use.ncgrid <- tclVar(.cdtData$GalParams$ncdf.grid$use.ncgrid)
	file.ncgrid <- tclVar(.cdtData$GalParams$ncdf.grid$file)

	statencgrid <- if(.cdtData$GalParams$ncdf.grid$use.ncgrid) 'normal' else 'disabled'

	chk.ncgrid <- tkcheckbutton(frNCGRID, variable = use.ncgrid, text = lang.dlg[['label']][['5']], anchor = 'w', justify = 'left')
	frncgrid <- tkframe(frNCGRID, relief = 'groove', borderwidth = 2)

	txt.ncgrid <- tklabel(frncgrid, text = lang.dlg[['label']][['6']], anchor = 'w', justify = 'left')
	cb.ncgrid <- ttkcombobox(frncgrid, values = unlist(listOpenFiles), textvariable = file.ncgrid, width = largeur, state = statencgrid)
	bt.ncgrid <- tkbutton(frncgrid, text = "...", state = statencgrid)

	########
	tkconfigure(bt.ncgrid, command = function(){
		nc.opfiles <- getOpenNetcdf(tt, initialdir = getwd())
		if(!is.null(nc.opfiles)){
			update.OpenFiles('netcdf', nc.opfiles)
			listOpenFiles[[length(listOpenFiles) + 1]] <<- nc.opfiles[[1]]
			tclvalue(file.ncgrid) <- nc.opfiles[[1]]

			if(tclvalue(nbcnfile) == NBNCF[1]) tkconfigure(cb.ncfldir, values = unlist(listOpenFiles))
			lapply(list(cb.ncsample, cb.ncgrid), tkconfigure, values = unlist(listOpenFiles))
			####
			infoOpenNC1(tclvalue(file.ncgrid))
		}
	})

	tkgrid(chk.ncgrid, row = 0, column = 0, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
	tkgrid(frncgrid, row = 1, column = 0, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)

	tkgrid(txt.ncgrid, row = 0, column = 0, sticky = 'we', rowspan = 1, columnspan = 5, padx = 1, pady = 1, ipadx = 1, ipady = 1)
	tkgrid(cb.ncgrid, row = 1, column = 0, sticky = 'we', rowspan = 1, columnspan = 4, padx = 0, pady = 1, ipadx = 1, ipady = 1)
	tkgrid(bt.ncgrid, row = 1, column = 4, sticky = 'w', rowspan = 1, columnspan = 1, padx = 0, pady = 1, ipadx = 1, ipady = 1)

	###################

	tkbind(chk.ncgrid, "<Button-1>", function(){
		statencgrid <- if(tclvalue(use.ncgrid) == '0') "normal" else "disabled"
		tkconfigure(cb.ncgrid, state = statencgrid)
		tkconfigure(bt.ncgrid, state = statencgrid)

		statemethod <- if(tclvalue(use.ncgrid) == '0') "disabled" else "normal"
		tkconfigure(cb.but, state = statemethod)

		METHODS <- if(tclvalue(but) == BUTVAL[1] & tclvalue(use.ncgrid) == '1') c('mean', 'bilinear') else 'bilinear'
		tkconfigure(cb.method, values = METHODS)
		tclvalue(method) <- if(tclvalue(but) == BUTVAL[1] & tclvalue(use.ncgrid) == '1') tclvalue(method) else 'bilinear'

		stateDefGrid <- if(tclvalue(use.ncgrid) == '0') "disabled" else "normal"
		tkconfigure(grd_vlon1, state = stateDefGrid)
		tkconfigure(grd_vlon2, state = stateDefGrid)
		tkconfigure(grd_vlon3, state = stateDefGrid)
		tkconfigure(grd_vlat1, state = stateDefGrid)
		tkconfigure(grd_vlat2, state = stateDefGrid)
		tkconfigure(grd_vlat3, state = stateDefGrid)
	})

	tkbind(cb.ncgrid, "<<ComboboxSelected>>", function() infoOpenNC1(tclvalue(file.ncgrid)))

	############################################
	tkgrid(frNCDF, row = 0, column = 0, sticky = 'we', padx = 1, pady = 1, ipadx = 1, ipady = 1)
	tkgrid(frNCGRID, row = 1, column = 0, sticky = 'we', padx = 1, pady = 3, ipadx = 1, ipady = 1)

	##############################################  RIGHT   ############################################

	frMTH <- tkframe(frRight, relief = "sunken", borderwidth = 2)

	choixMTHD <- c('mean', 'bilinear')
	method <- tclVar(.cdtData$GalParams$method)

	but <- tclVar()
	cbBUTVAL <- lang.dlg[['combobox']][['2']]
	BUTVAL <- c("Aggregate", "Disaggregate")
	tclvalue(but) <- cbBUTVAL[BUTVAL %in% .cdtData$GalParams$but]

	METHODS <- if(.cdtData$GalParams$but == cbBUTVAL[1] & tclvalue(use.ncgrid) == '0') choixMTHD else 'bilinear'
	tclvalue(method) <- if(.cdtData$GalParams$but == cbBUTVAL[1] & tclvalue(use.ncgrid) == '0') tclvalue(method) else 'bilinear'

	statemethod <- if(.cdtData$GalParams$ncdf.grid$use.ncgrid) 'disabled' else 'normal'

	txt.Aggreg <- tklabel(frMTH, text = lang.dlg[['label']][['7']], anchor = 'w', justify = 'left')
	txt.but <- tklabel(frMTH, text = lang.dlg[['label']][['8']], anchor = 'w', justify = 'left')
	cb.but <- ttkcombobox(frMTH, values = cbBUTVAL, textvariable = but, width = largeur2, state = statemethod)
	txt.method <- tklabel(frMTH, text = lang.dlg[['label']][['9']], anchor = 'w', justify = 'left')
	cb.method <- ttkcombobox(frMTH, values = METHODS, textvariable = method, width = largeur2)

	tkgrid(txt.Aggreg, row = 0, column = 0, sticky = 'we', rowspan = 1, columnspan = 4, padx = 1, pady = 2, ipadx = 1, ipady = 1)
	tkgrid(txt.but, row = 1, column = 0, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 2, ipadx = 1, ipady = 1)
	tkgrid(cb.but, row = 1, column = 1, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 2, ipadx = 1, ipady = 1)
	tkgrid(txt.method, row = 1, column = 2, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 2, ipadx = 1, ipady = 1)
	tkgrid(cb.method, row = 1, column = 3, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 2, ipadx = 1, ipady = 1)

	###################

	tkbind(cb.but, "<<ComboboxSelected>>", function(){
		METHODS <- if(tclvalue(but) == BUTVAL[1]) choixMTHD else 'bilinear'
		tkconfigure(cb.method, values = METHODS)
		tclvalue(method) <- if(tclvalue(but) == BUTVAL[1]) tclvalue(method) else 'bilinear'
	})

	############################################

	frRES <- tkframe(frRight, relief = "sunken", borderwidth = 2)

	minLon <- tclVar(.cdtData$GalParams$res$minlon)
	maxLon <- tclVar(.cdtData$GalParams$res$maxlon)
	resLon <- tclVar(.cdtData$GalParams$res$reslon)
	minLat <- tclVar(.cdtData$GalParams$res$minlat)
	maxLat <- tclVar(.cdtData$GalParams$res$maxlat)
	resLat <- tclVar(.cdtData$GalParams$res$reslat)

	stateDefGrid <- if(.cdtData$GalParams$ncdf.grid$use.ncgrid) 'disabled' else 'normal'

	txt.newgrid <- tklabel(frRES, text = lang.dlg[['label']][['10']], anchor = 'w', justify = 'left')

	grd_llon <- tklabel(frRES, text = lang.dlg[['label']][['11']], anchor = 'e', justify = 'right')
	grd_llat <- tklabel(frRES, text = lang.dlg[['label']][['12']], anchor = 'e', justify = 'right')
	grd_lb1 <- tklabel(frRES, text = "Min")
	grd_lb2 <- tklabel(frRES, text = "Max")
	grd_lb3 <- tklabel(frRES, text = "Res")

	grd_vlon1 <- tkentry(frRES, width = 8, justify = "right", textvariable = minLon, state = stateDefGrid)
	grd_vlon2 <- tkentry(frRES, width = 8, justify = "right", textvariable = maxLon, state = stateDefGrid)
	grd_vlon3 <- tkentry(frRES, width = 8, justify = "right", textvariable = resLon, state = stateDefGrid)
	grd_vlat1 <- tkentry(frRES, width = 8, justify = "right", textvariable = minLat, state = stateDefGrid)
	grd_vlat2 <- tkentry(frRES, width = 8, justify = "right", textvariable = maxLat, state = stateDefGrid)
	grd_vlat3 <- tkentry(frRES, width = 8, justify = "right", textvariable = resLat, state = stateDefGrid)

	tkgrid(txt.newgrid, row = 0, column = 0, sticky = "ew", rowspan = 1, columnspan = 4, padx = 1, pady = 1, ipadx = 1, ipady = 1)

	tkgrid(grd_lb1, row = 1, column = 1, sticky = "ew", rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
	tkgrid(grd_lb2, row = 1, column = 2, sticky = "ew", rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
	tkgrid(grd_lb3, row = 1, column = 3, sticky = "ew", rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
	tkgrid(grd_llon, row = 2, column = 0, sticky = "ew", rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
	tkgrid(grd_vlon1, row = 2, column = 1, sticky = "ew", rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
	tkgrid(grd_vlon2, row = 2, column = 2, sticky = "ew", rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
	tkgrid(grd_vlon3, row = 2, column = 3, sticky = "ew", rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
	tkgrid(grd_llat, row = 3, column = 0, sticky = "ew", rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
	tkgrid(grd_vlat1, row = 3, column = 1, sticky = "ew", rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
	tkgrid(grd_vlat2, row = 3, column = 2, sticky = "ew", rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
	tkgrid(grd_vlat3, row = 3, column = 3, sticky = "ew", rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)

	helpWidget(grd_vlon1, lang.dlg[['tooltip']][['1']], lang.dlg[['status']][['1']])
	helpWidget(grd_vlon2, lang.dlg[['tooltip']][['2']], lang.dlg[['status']][['2']])
	helpWidget(grd_vlon3, lang.dlg[['tooltip']][['3']], lang.dlg[['status']][['3']])
	helpWidget(grd_vlat1, lang.dlg[['tooltip']][['4']], lang.dlg[['status']][['4']])
	helpWidget(grd_vlat2, lang.dlg[['tooltip']][['5']], lang.dlg[['status']][['5']])
	helpWidget(grd_vlat3, lang.dlg[['tooltip']][['3']], lang.dlg[['status']][['3']])

	############################################

	frSave <- tkframe(frRight, relief = "sunken", borderwidth = 2)

	dir2save <- tclVar(.cdtData$GalParams$output)

	txt.dir2save <- tklabel(frSave, text = lang.dlg[['label']][['13']], anchor = 'w', justify = 'left')
	en.dir2save <- tkentry(frSave, textvariable = dir2save, width = largeur1)
	bt.dir2save <- tkbutton(frSave, text = "...")

	#####

	tkconfigure(bt.dir2save, command = function(){
		initialdir <- if(str_trim(.cdtData$GalParams$output) != "") .cdtData$GalParams$output else getwd()
		dir2savepth <- tk_choose.dir(initialdir, "")
		if(is.na(dir2savepth)) tclvalue(dir2save) <- initialdir
		else{
			dir.create(dir2savepth, showWarnings = FALSE, recursive = TRUE)
			tclvalue(dir2save) <- dir2savepth
		}
	})

	tkgrid(txt.dir2save, row = 0, column = 0, sticky = 'we', rowspan = 1, columnspan = 2, padx = 1, pady = 2, ipadx = 1, ipady = 1)
	tkgrid(en.dir2save, row = 1, column = 0, sticky = 'we', rowspan = 1, columnspan = 1, padx = 0, pady = 1, ipadx = 1, ipady = 1)
	tkgrid(bt.dir2save, row = 1, column = 1, sticky = 'w', rowspan = 1, columnspan = 1, padx = 0, pady = 1, ipadx = 1, ipady = 1)

	helpWidget(en.dir2save, lang.dlg[['tooltip']][['6']], lang.dlg[['status']][['6']])
	helpWidget(bt.dir2save, lang.dlg[['tooltip']][['7']], lang.dlg[['status']][['7']])

	############################################
	tkgrid(frMTH, row = 2, column = 0, sticky = 'we', padx = 1, pady = 3, ipadx = 1, ipady = 2)
	tkgrid(frRES, row = 3, column = 0, sticky = '', padx = 1, pady = 3, ipadx = 1, ipady = 1)
	tkgrid(frSave, row = 4, column = 0, sticky = 'we', padx = 1, pady = 3, ipadx = 1, ipady = 1)

	############################################
	tkgrid(frLeft, row = 0, column = 0, sticky = 'news', padx = 5, pady = 1, ipadx = 1, ipady = 1)
	tkgrid(frRight, row = 0, column = 1, sticky = '', padx = 5, pady = 1, ipadx = 1, ipady = 1)

	############################################

	bt.prm.OK <- ttkbutton(frMRG1, text = .cdtEnv$tcl$lang$global[['button']][['1']])
	bt.prm.CA <- ttkbutton(frMRG1, text = .cdtEnv$tcl$lang$global[['button']][['2']])

	tkconfigure(bt.prm.OK, command = function(){
		.cdtData$GalParams$nb.ncfile <- NBNC[NBNCF %in% str_trim(tclvalue(nbcnfile))]

		.cdtData$GalParams$ncdf$fileordir <- str_trim(tclvalue(ncfiledir))
		.cdtData$GalParams$ncdf$sample <- str_trim(tclvalue(ncsample))

		.cdtData$GalParams$ncdf.grid$use.ncgrid <- switch(tclvalue(use.ncgrid), '0' = FALSE, '1' = TRUE)
		.cdtData$GalParams$ncdf.grid$file <- str_trim(tclvalue(file.ncgrid))

		.cdtData$GalParams$but <- BUTVAL[cbBUTVAL %in% str_trim(tclvalue(but))]
		.cdtData$GalParams$method <- str_trim(tclvalue(method))

		.cdtData$GalParams$res$minlon <- as.numeric(str_trim(tclvalue(minLon)))
		.cdtData$GalParams$res$maxlon <- as.numeric(str_trim(tclvalue(maxLon)))
		.cdtData$GalParams$res$reslon <- as.numeric(str_trim(tclvalue(resLon)))
		.cdtData$GalParams$res$minlat <- as.numeric(str_trim(tclvalue(minLat)))
		.cdtData$GalParams$res$maxlat <- as.numeric(str_trim(tclvalue(maxLat)))
		.cdtData$GalParams$res$reslat <- as.numeric(str_trim(tclvalue(resLat)))

		.cdtData$GalParams$output <- str_trim(tclvalue(dir2save))
		.cdtData$GalParams$message <- lang.dlg[['message']]

		tkgrab.release(tt)
		tkdestroy(tt)
		tkfocus(.cdtEnv$tcl$main$win)
	})

	tkconfigure(bt.prm.CA, command = function(){
		tkgrab.release(tt)
		tkdestroy(tt)
		tkfocus(.cdtEnv$tcl$main$win)
	})

	####
	tkgrid(bt.prm.OK, row = 0, column = 0, sticky = 'w', padx = 5, pady = 1, ipadx = 1, ipady = 1)
	tkgrid(bt.prm.CA, row = 0, column = 1, sticky = 'e', padx = 5, pady = 1, ipadx = 1, ipady = 1)

	############################################
	
	tkgrid(frMRG0, row = 0, column = 0, sticky = 'nswe', rowspan = 1, columnspan = 2, padx = 1, pady = 1, ipadx = 1, ipady = 1)
	tkgrid(frMRG1, row = 1, column = 1, sticky = 'se', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)

	###########################
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

