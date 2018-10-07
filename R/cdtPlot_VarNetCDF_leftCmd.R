
PlotVarNetCDFFilesCmd <- function(){
	listOpenFiles <- openFile_ttkcomboList()
	if(WindowsOS()){
		largeur0 <- .cdtEnv$tcl$fun$w.widgets(29)
		largeur1 <- .cdtEnv$tcl$fun$w.widgets(28)
		largeur2 <- .cdtEnv$tcl$fun$w.widgets(16)
		largeur3 <- 29
		largeur4 <- 12
		largeur5 <- 15
		largeur6 <- .cdtEnv$tcl$fun$w.widgets(32)
		largeur7 <- .cdtEnv$tcl$fun$w.widgets(16)
		vars.w <- .cdtEnv$tcl$fun$w.scale(21)
		vars.h <- .cdtEnv$tcl$fun$h.scale(15)
	}else{
		largeur0 <- .cdtEnv$tcl$fun$w.widgets(23)
		largeur1 <- .cdtEnv$tcl$fun$w.widgets(22)
		largeur2 <- .cdtEnv$tcl$fun$w.widgets(15)
		largeur3 <- .cdtEnv$tcl$fun$w.widgets(16)
		largeur4 <- 8
		largeur5 <- 13
		largeur6 <- .cdtEnv$tcl$fun$w.widgets(26)
		largeur7 <- .cdtEnv$tcl$fun$w.widgets(16)
		vars.w <- .cdtEnv$tcl$fun$w.scale(22)
		vars.h <- .cdtEnv$tcl$fun$h.scale(15)
	}

	GeneralParameters <- list(dir = "", sample = "", format = "Bernoulli-Gamma_Pars.STN_%S.nc")

	# xml.dlg <- file.path(.cdtDir$dirLocal, "languages", "cdtPlot_VarNetCDF_leftCmd.xml")
	# lang.dlg <- cdtLanguageParse(xml.dlg, .cdtData$Config$lang.iso)

	.cdtData$EnvData$plot.maps$data.type <- "cdtnetcdf"

	###################

	.cdtEnv$tcl$main$cmd.frame <- tkframe(.cdtEnv$tcl$main$panel.left)

	tknote.cmd <- bwNoteBook(.cdtEnv$tcl$main$cmd.frame)
	cmd.tab1 <- bwAddTab(tknote.cmd, text = "NetCDF Data")
	cmd.tab2 <- bwAddTab(tknote.cmd, text = "Maps")

	bwRaiseTab(tknote.cmd, cmd.tab1)

	tkgrid.columnconfigure(cmd.tab1, 0, weight = 1)
	tkgrid.columnconfigure(cmd.tab2, 0, weight = 1)

	tkgrid.rowconfigure(cmd.tab1, 0, weight = 1)
	tkgrid.rowconfigure(cmd.tab2, 0, weight = 1)

	#######################################################################################################

	#Tab1
	subfr1 <- bwTabScrollableFrame(cmd.tab1)

		#######################

		frameNC <- tkframe(subfr1, relief = 'groove', borderwidth = 2)

		ncDIR <- tclVar(GeneralParameters$dir)
		ncSample <- tclVar(GeneralParameters$sample)
		ncFormat <- tclVar(GeneralParameters$format)

		txt.ncdr <- tklabel(frameNC, text = 'Directory containing the NetCDF data', anchor = 'w', justify = 'left')
		en.ncdr <- tkentry(frameNC, textvariable = ncDIR, width = largeur0)
		bt.ncdr <- tkbutton(frameNC, text = "...")
		txt.ncfl <- tklabel(frameNC, text = "NetCDF data sample file", anchor = 'w', justify = 'left')
		en.ncfl <- tkentry(frameNC, textvariable = ncSample, width = largeur0, state = 'disabled')
		bt.ncfl <- tkbutton(frameNC, text = "...", bg = 'lightblue')
		txt.ncff <- tklabel(frameNC, text = "Filename format", anchor = 'w', justify = 'left')
		en.ncff <- tkentry(frameNC, textvariable = ncFormat, width = largeur0)

		#################

		tkconfigure(bt.ncdr, command = function(){
			dir4cdf <- tk_choose.dir(getwd(), "")
			tclvalue(ncDIR) <- if(dir4cdf %in% c("", "NA") | is.na(dir4cdf)) "" else dir4cdf
		})

		tkconfigure(bt.ncfl, command = function(){
			initialdir <- if(file.exists(str_trim(tclvalue(ncDIR)))) str_trim(tclvalue(ncDIR)) else getwd()
			fileopen <- tclvalue(tkgetOpenFile(initialdir = initialdir, filetypes = .cdtEnv$tcl$data$filetypes3))
			if(fileopen == "" | is.na(fileopen)) return(NULL)
			if(!file.exists(fileopen)){
				Insert.Messages.Out(paste(fileopen, "does not exist"), format = TRUE)
				.cdtData$EnvData$ncvar <- NULL
				return(NULL)
			}
			tclvalue(ncSample) <- basename(fileopen)

			nc <- nc_open(fileopen)
			ncdims <- sapply(nc$dim, '[[', 'name')
			var.name <- sapply(nc$var, '[[', 'name')
			var.lname <- sapply(nc$var, '[[', 'longname')
			nc_close(nc)

			ncvars <- paste0(var.name, '::', var.lname)

			if(length(ncdims) < 2){
				Insert.Messages.Out("The NetCDF must be a spatial data", format = TRUE)
				.cdtData$EnvData$ncvar <- NULL
				return(NULL)
			}

			if(length(ncvars) < 1){
				Insert.Messages.Out("No variables found in the NetCDF file", format = TRUE)
				.cdtData$EnvData$ncvar <- NULL
				return(NULL)
			}

			tkconfigure(cb.lon, values = ncdims)
			tkconfigure(cb.lat, values = ncdims)
			tclvalue(ncLON) <- ncdims[1]
			tclvalue(ncLAT) <- ncdims[2]

			##################################

			nl <- length(.cdtData$EnvData$ncvar)
			if(nl > 0){
				for(j in seq(nl)){
					tkdestroy(.cdtData$EnvData$ncvar[[j]]$chk.wdg)
					tkdestroy(.cdtData$EnvData$ncvar[[j]]$bt.wdg)
					.cdtData$EnvData$ncvar <- NULL
				}
				tcl("update")
			}
			.cdtData$EnvData$ncvar <- vector(mode = 'list', length = length(ncvars))


			tcl("update", "idletasks")
			for(j in seq_along(ncvars)){
				.cdtData$EnvData$ncvar[[j]]$ncops <- ncMapOp
				.cdtData$EnvData$ncvar[[j]]$var.name <- var.name[j]
				.cdtData$EnvData$ncvar[[j]]$chk.var <- tclVar(1)
				.cdtData$EnvData$ncvar[[j]]$chk.wdg <- tkcheckbutton(sel.vars, variable = .cdtData$EnvData$ncvar[[j]]$chk.var, text = ncvars[j], anchor = 'w', justify = 'left', width = largeur7)
				.cdtData$EnvData$ncvar[[j]]$bt.wdg <- ttkbutton(sel.vars, text = .cdtEnv$tcl$lang$global[['button']][['4']])

				tkgrid(.cdtData$EnvData$ncvar[[j]]$chk.wdg, row = j - 1, column = 0)
				tkgrid(.cdtData$EnvData$ncvar[[j]]$bt.wdg, row = j - 1, column = 1, padx = 3)

				infobulle(.cdtData$EnvData$ncvar[[j]]$chk.wdg, ncvars[j])
			}

			#################

			lapply(seq_along(ncvars), function(j){
				tkconfigure(.cdtData$EnvData$ncvar[[j]]$bt.wdg, command = function(){
					if(!is.null(.cdtData$EnvData$ncData$map)){
						atlevel <- pretty(.cdtData$EnvData$ncData$map[[j]]$z, n = 10, min.n = 7)
						if(is.null(.cdtData$EnvData$ncvar[[j]]$ncops$userLvl$levels)){
							.cdtData$EnvData$ncvar[[j]]$ncops$userLvl$levels <- atlevel
						}else{
							if(!.cdtData$EnvData$ncvar[[j]]$ncops$userLvl$custom)
								.cdtData$EnvData$ncvar[[j]]$ncops$userLvl$levels <- atlevel
						}
					}
					.cdtData$EnvData$ncvar[[j]]$ncops <- MapGraph.MapOptions.VarNetCDF(.cdtData$EnvData$ncvar[[j]]$ncops)
				})
			})

			#################

			lapply(seq_along(ncvars), function(j){
				tkbind(.cdtData$EnvData$ncvar[[j]]$chk.wdg, "<Button-1>", function(){
					stateOP <- if(tclvalue(.cdtData$EnvData$ncvar[[j]]$chk.var) == "1") "disabled" else "normal"
					tkconfigure(.cdtData$EnvData$ncvar[[j]]$bt.wdg, state = stateOP)
				})
			})
		})

		tkgrid(txt.ncdr, row = 0, column = 0, sticky = 'we', rowspan = 1, columnspan = 6, padx = 1, pady = 0, ipadx = 1, ipady = 1)
		tkgrid(en.ncdr, row = 1, column = 0, sticky = 'we', rowspan = 1, columnspan = 5, padx = 0, pady = 0, ipadx = 1, ipady = 1)
		tkgrid(bt.ncdr, row = 1, column = 5, sticky = 'w', rowspan = 1, columnspan = 1, padx = 0, pady = 0, ipadx = 1, ipady = 1)
		tkgrid(txt.ncfl, row = 2, column = 0, sticky = 'we', rowspan = 1, columnspan = 6, padx = 1, pady = 0, ipadx = 1, ipady = 1)
		tkgrid(en.ncfl, row = 3, column = 0, sticky = 'we', rowspan = 1, columnspan = 5, padx = 0, pady = 0, ipadx = 1, ipady = 1)
		tkgrid(bt.ncfl, row = 3, column = 5, sticky = 'w', rowspan = 1, columnspan = 1, padx = 0, pady = 0, ipadx = 1, ipady = 1)

		tkgrid(txt.ncff, row = 4, column = 0, sticky = 'we', rowspan = 1, columnspan = 6, padx = 1, pady = 0, ipadx = 1, ipady = 1)
		tkgrid(en.ncff, row = 5, column = 0, sticky = 'we', rowspan = 1, columnspan = 5, padx = 1, pady = 0, ipadx = 1, ipady = 1)


		infobulle(en.ncdr, 'Enter the full path to directory containing the NetCDF data')
		status.bar.display(en.ncdr, 'Enter the full path to directory containing the NetCDF data')
		status.bar.display(en.ncfl, 'File containing a sample of the data in NetCDF, browse with (...)')
		infobulle(bt.ncfl, 'Browse here the sample file')

		infobulle(en.ncff, 'Enter the filename format of NetCDF data, example: Bernoulli-Gamma_Pars.STN_2.nc')
		status.bar.display(en.ncff, 'Enter the filename format of NetCDF data, example: Bernoulli-Gamma_Pars.STN_2.nc\nReplace the whole string representing the date or sequence by %S\nBernoulli-Gamma_Pars.STN_2.nc becomes Bernoulli-Gamma_Pars.STN_%S.nc')

		#######################

		frameDIM <- ttklabelframe(subfr1, text = "Dimensions", relief = 'groove')

		ncLON <- tclVar()
		ncLAT <- tclVar()

		txt.lon <- tklabel(frameDIM, text = "Longitude", anchor = 'e', justify = 'right', width = largeur5)
		cb.lon <- ttkcombobox(frameDIM, values = "", textvariable = ncLON, width = largeur2)
		txt.lat <- tklabel(frameDIM, text = "Latitude", anchor = 'e', justify = 'right')
		cb.lat <- ttkcombobox(frameDIM, values = "", textvariable = ncLAT, width = largeur2)

		tkgrid(txt.lon, row = 0, column = 0, sticky = 'we', rowspan = 1, columnspan = 2, padx = 1, pady = 1, ipadx = 1, ipady = 1)
		tkgrid(cb.lon, row = 0, column = 2, sticky = 'we', rowspan = 1, columnspan = 4, padx = 1, pady = 1, ipadx = 1, ipady = 1)
		tkgrid(txt.lat, row = 1, column = 0, sticky = 'we', rowspan = 1, columnspan = 2, padx = 1, pady = 1, ipadx = 1, ipady = 1)
		tkgrid(cb.lat, row = 1, column = 2, sticky = 'we', rowspan = 1, columnspan = 4, padx = 1, pady = 1, ipadx = 1, ipady = 1)

		#######################

		frameVARS <- ttklabelframe(subfr1, text = "Select variable to plot", relief = 'groove')

		sel.vars <- bwTabScrollableFrame(frameVARS, wscrlwin = vars.w, hscrlwin = vars.h, bg = "red")

		ncMapOp <- list(presetCol = list(color = 'tim.colors', reverse = FALSE),
						userCol = list(custom = FALSE, color = NULL),
						userLvl = list(custom = FALSE, levels = NULL, equidist = FALSE),
						title = list(user = FALSE, title = ''),
						colkeyLab = list(user = FALSE, label = ''))

		##############################################

		tkgrid(frameNC, row = 0, column = 0, sticky = 'we', padx = 1, pady = 1, ipadx = 1, ipady = 1)
		tkgrid(frameDIM, row = 1, column = 0, sticky = 'we', padx = 1, pady = 1, ipadx = 1, ipady = 1)
		tkgrid(frameVARS, row = 2, column = 0, sticky = 'nwe', padx = 1, pady = 1, ipadx = 1, ipady = 1)

	#######################################################################################################

	#Tab2
	subfr2 <- bwTabScrollableFrame(cmd.tab2)

		##############################################

		frameMap <- ttklabelframe(subfr2, text = "Map", relief = 'groove')

		ncdf.date.file <- tclVar()

		cb.nc.maps <- ttkcombobox(frameMap, values = "", textvariable = ncdf.date.file, width = largeur1)
		bt.nc.Date.prev <- ttkbutton(frameMap, text = "<<- Previous", width = 11)
		bt.nc.maps <- ttkbutton(frameMap, text = .cdtEnv$tcl$lang$global[['button']][['3']], width = largeur4)
		bt.nc.Date.next <- ttkbutton(frameMap, text = "Next ->>", width = 11)

		###################

		.cdtData$EnvData$tab$dataNCMap <- NULL

		tkconfigure(bt.nc.maps, command = function(){
			ret <- try(get.All.NCDF.Files(), silent = TRUE)
			if(inherits(ret, "try-error") | is.null(ret)) return(NULL)

			if(str_trim(tclvalue(ncdf.date.file)) != ""){
				ret <- try(get.NCDF.DATA(), silent = TRUE)
				if(inherits(ret, "try-error") | is.null(ret)) return(NULL)

				tab.title <- paste('Map -', .cdtData$EnvData$ncData$file2plot)
				imgContainer <- CDT.Display.Graph(PlotNetCDFVarsMaps, .cdtData$EnvData$tab$dataNCMap, tab.title)
				.cdtData$EnvData$tab$dataNCMap <- imageNotebookTab_unik(imgContainer, .cdtData$EnvData$tab$dataNCMap)
			}
		})

		tkconfigure(bt.nc.Date.prev, command = function(){
			ret <- try(get.All.NCDF.Files(), silent = TRUE)
			if(inherits(ret, "try-error") | is.null(ret)) return(NULL)

			if(str_trim(tclvalue(ncdf.date.file)) != ""){
				donDates <- .cdtData$EnvData$NcFiles2Plot
				idaty <- which(donDates == str_trim(tclvalue(ncdf.date.file)))
				idaty <- idaty - 1
				if(idaty < 1) idaty <- length(donDates)
				tclvalue(ncdf.date.file) <- donDates[idaty]

				ret <- try(get.NCDF.DATA(), silent = TRUE)
				if(inherits(ret, "try-error") | is.null(ret)) return(NULL)

				tab.title <- paste('Map -', .cdtData$EnvData$ncData$file2plot)
				imgContainer <- CDT.Display.Graph(PlotNetCDFVarsMaps, .cdtData$EnvData$tab$dataNCMap, tab.title)
				.cdtData$EnvData$tab$dataNCMap <- imageNotebookTab_unik(imgContainer, .cdtData$EnvData$tab$dataNCMap)
			}
		})

		tkconfigure(bt.nc.Date.next, command = function(){
			ret <- try(get.All.NCDF.Files(), silent = TRUE)
			if(inherits(ret, "try-error") | is.null(ret)) return(NULL)

			if(str_trim(tclvalue(ncdf.date.file)) != ""){
				donDates <- .cdtData$EnvData$NcFiles2Plot
				idaty <- which(donDates == str_trim(tclvalue(ncdf.date.file)))
				idaty <- idaty + 1
				if(idaty > length(donDates)) idaty <- 1
				tclvalue(ncdf.date.file) <- donDates[idaty]

				ret <- try(get.NCDF.DATA(), silent = TRUE)
				if(inherits(ret, "try-error") | is.null(ret)) return(NULL)

				tab.title <- paste('Map -', .cdtData$EnvData$ncData$file2plot)
				imgContainer <- CDT.Display.Graph(PlotNetCDFVarsMaps, .cdtData$EnvData$tab$dataNCMap, tab.title)
				.cdtData$EnvData$tab$dataNCMap <- imageNotebookTab_unik(imgContainer, .cdtData$EnvData$tab$dataNCMap)
			}
		})

		###################

		tkgrid(cb.nc.maps, row = 0, column = 0, sticky = 'we', rowspan = 1, columnspan = 3, padx = 1, pady = 1, ipadx = 1, ipady = 1)
		tkgrid(bt.nc.Date.prev, row = 1, column = 0, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
		tkgrid(bt.nc.maps, row = 1, column = 1, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
		tkgrid(bt.nc.Date.next, row = 1, column = 2, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)

		##############################################

		framePlotType <- tkframe(subfr2)

		.cdtData$EnvData$plot.maps$.data.type <- "Grid"
		plot.type <- c("Pixels", "FilledContour")
		.cdtData$EnvData$plot.maps$plot.type <- tclVar("Pixels")

		txt.plotType <- tklabel(framePlotType, text = "Plot Type", anchor = 'e', justify = 'right')
		cb.plotType <- ttkcombobox(framePlotType, values = plot.type, textvariable = .cdtData$EnvData$plot.maps$plot.type, width = largeur3)

		tkgrid(txt.plotType, row = 0, column = 0, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
		tkgrid(cb.plotType, row = 0, column = 1, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)

		##############################################

		frameSHP <- ttklabelframe(subfr2, text = "Boundaries", relief = 'groove')

		.cdtData$EnvData$shp$add.shp <- tclVar(0)
		file.plotShp <- tclVar()
		stateSHP <- "disabled"

		chk.addshp <- tkcheckbutton(frameSHP, variable = .cdtData$EnvData$shp$add.shp, text = "Add boundaries to Map", anchor = 'w', justify = 'left')
		bt.addshpOpt <- ttkbutton(frameSHP, text = "Options", state = stateSHP)
		cb.addshp <- ttkcombobox(frameSHP, values = unlist(listOpenFiles), textvariable = file.plotShp, width = largeur1, state = stateSHP)
		bt.addshp <- tkbutton(frameSHP, text = "...", state = stateSHP)

		########
		tkconfigure(bt.addshp, command = function(){
			shp.opfiles <- getOpenShp(.cdtEnv$tcl$main$win)
			if(!is.null(shp.opfiles)){
				update.OpenFiles('shp', shp.opfiles)
				tclvalue(file.plotShp) <- shp.opfiles[[1]]
				listOpenFiles[[length(listOpenFiles) + 1]] <<- shp.opfiles[[1]]
				tkconfigure(cb.addshp, values = unlist(listOpenFiles))

				shpofile <- getShpOpenData(file.plotShp)
				if(is.null(shpofile))
					.cdtData$EnvData$shp$ocrds <- NULL
				else
					.cdtData$EnvData$shp$ocrds <- getBoundaries(shpofile[[2]])
			}
		})

		########
		.cdtData$EnvData$SHPOp <- list(col = "black", lwd = 1.5)

		tkconfigure(bt.addshpOpt, command = function(){
			.cdtData$EnvData$SHPOp <- MapGraph.GraphOptions.LineSHP(.cdtData$EnvData$SHPOp)
		})

		########
		tkgrid(chk.addshp, row = 0, column = 0, sticky = 'we', rowspan = 1, columnspan = 6, padx = 1, pady = 1)
		tkgrid(bt.addshpOpt, row = 0, column = 6, sticky = 'we', rowspan = 1, columnspan = 2, padx = 1, pady = 1)
		tkgrid(cb.addshp, row = 1, column = 0, sticky = 'we', rowspan = 1, columnspan = 7, padx = 1, pady = 1)
		tkgrid(bt.addshp, row = 1, column = 7, sticky = 'w', rowspan = 1, columnspan = 1, padx = 0, pady = 1)

		#################
		tkbind(cb.addshp, "<<ComboboxSelected>>", function(){
			shpofile <- getShpOpenData(file.plotShp)
			if(is.null(shpofile))
				.cdtData$EnvData$shp$ocrds <- NULL
			else
				.cdtData$EnvData$shp$ocrds <- getBoundaries(shpofile[[2]])
		})

		tkbind(chk.addshp, "<Button-1>", function(){
			stateSHP <- if(tclvalue(.cdtData$EnvData$shp$add.shp) == "1") "disabled" else "normal"
			tkconfigure(cb.addshp, state = stateSHP)
			tkconfigure(bt.addshp, state = stateSHP)
			tkconfigure(bt.addshpOpt, state = stateSHP)
		})

		##############################################

		frameBox <- tkframe(subfr2)

		.cdtData$EnvData$plot.maps$draw.box <- tclVar(0)

		chk.addbox <- tkcheckbutton(frameBox, variable = .cdtData$EnvData$plot.maps$draw.box, text = "Draw a box around plots", anchor = 'w', justify = 'left')

		tkgrid(chk.addbox, row = 0, column = 0, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1)

		############################################

		tkgrid(frameMap, row = 0, column = 0, sticky = 'we', padx = 1, pady = 1, ipadx = 1, ipady = 1)
		tkgrid(framePlotType, row = 1, column = 0, sticky = '', padx = 1, pady = 3, ipadx = 1, ipady = 1)
		tkgrid(frameSHP, row = 2, column = 0, sticky = 'we', padx = 1, pady = 1, ipadx = 1, ipady = 1)
		tkgrid(frameBox, row = 3, column = 0, sticky = 'we', padx = 1, pady = 1, ipadx = 1, ipady = 1)

	#######################################################################################################

	get.All.NCDF.Files <- function(){
		tkconfigure(.cdtEnv$tcl$main$win, cursor = 'watch')
		tcl('update')
		on.exit({
			tkconfigure(.cdtEnv$tcl$main$win, cursor = '')
			tcl('update')
		})

		nc.dir <- str_trim(tclvalue(ncDIR))
		nc.format <- str_trim(tclvalue(ncFormat))
		nc.sample <- str_trim(tclvalue(ncSample))

		if(nc.dir == "" | nc.format == ""){
			tkconfigure(cb.nc.maps, values = "")
			tclvalue(ncdf.date.file) <- ""
			.cdtData$EnvData$loaded.nc <- NULL
			return(NULL)
		}

		loaded.nc <- list(nc.dir, nc.format)

		getNCFiles <- TRUE
		if(!is.null(.cdtData$EnvData$loaded.nc))
			getNCFiles <- if(!isTRUE(all.equal(.cdtData$EnvData$loaded.nc, loaded.nc))) TRUE else FALSE

		if(getNCFiles){
			nc.files <- list.files(nc.dir, sub("%S", ".+", nc.format))
			if(length(nc.files) == 0){
				Insert.Messages.Out("No ncdf files found", format = TRUE)
				tkconfigure(cb.nc.maps, values = "")
				tclvalue(ncdf.date.file) <- ""
				.cdtData$EnvData$loaded.nc <- NULL
				return(NULL)
			}
			frmt <- strsplit(nc.format, "%S")[[1]]

			seq_dat <- gsub(frmt[1], "", gsub(frmt[2], "", nc.files))
			nb_only <- grepl("^[0-9]+$", seq_dat)
			seq_order <- seq_along(seq_dat)
			if(all(nb_only)){
				nmax <- max(nchar(seq_dat))
				seq_dat <- str_pad(seq_dat, nmax, pad = "0")
				seq_order <- order(seq_dat)
			}else{
				nb_mixed <- gregexpr("[[:digit:]]+", seq_dat)
				ch_mixed <- gregexpr("[[:alpha:]]+", seq_dat)
				nb_cont <- sapply(nb_mixed, function(x) x[1] > 0)
				ch_cont <- sapply(ch_mixed, function(x) x[1] > 0)
				if(all(nb_cont) & !all(ch_cont)){
					nb_mixed <- regmatches(seq_dat, nb_mixed)
					nb_cont <- diff(range(sapply(nb_mixed, length)))
					if(nb_cont == 0){
						nb_mixed <- do.call(rbind, nb_mixed)
						nb_mixed <- apply(nb_mixed, 2, as.numeric)
						seq_order <- sort.filename.data(nb_mixed)
					}
				}
				if(!all(nb_cont) & all(ch_cont)){
					ch_mixed <- regmatches(seq_dat, ch_mixed)
					ch_cont <- diff(range(sapply(ch_mixed, length)))
					if(ch_cont == 0){
						ch_mixed <- do.call(rbind, ch_mixed)
						seq_order <- sort.filename.data(ch_mixed)
					}
				}
				if(all(nb_cont) & all(ch_cont)){
					nb_mixed <- regmatches(seq_dat, nb_mixed)
					nb_cont <- diff(range(sapply(nb_mixed, length)))
					ch_mixed <- regmatches(seq_dat, ch_mixed)
					ch_cont <- diff(range(sapply(ch_mixed, length)))
					if(nb_cont == 0 & ch_cont == 0){
						nb_mixed <- do.call(rbind, nb_mixed)
						nb_mixed <- apply(nb_mixed, 2, as.numeric)
						ch_mixed <- do.call(rbind, ch_mixed)
						seq_order <- sort.filename.data(data.frame(ch_mixed, nb_mixed))
					}
					if(nb_cont == 0 & ch_cont != 0){
						nb_mixed <- do.call(rbind, nb_mixed)
						nb_mixed <- apply(nb_mixed, 2, as.numeric)
						seq_order <- sort.filename.data(nb_mixed)
					}
					if(nb_cont != 0 & ch_cont == 0){
						ch_mixed <- do.call(rbind, ch_mixed)
						seq_order <- sort.filename.data(ch_mixed)
					}
				}
			}

			nc.files <- nc.files[seq_order]

			tkconfigure(cb.nc.maps, values = nc.files)
			tclvalue(ncdf.date.file) <- nc.files[1]
			.cdtData$EnvData$NcFiles2Plot <- nc.files

			.cdtData$EnvData$loaded.nc <- loaded.nc
		}
		return(0)
	}

	get.NCDF.DATA <- function(){
		tkconfigure(.cdtEnv$tcl$main$win, cursor = 'watch')
		tcl('update')
		on.exit({
			tkconfigure(.cdtEnv$tcl$main$win, cursor = '')
			tcl('update')
		})

		nc.dir <- str_trim(tclvalue(ncDIR))
		nc.file <- str_trim(tclvalue(ncdf.date.file))
		ncfile.path <- file.path(nc.dir, nc.file)

		nlon <- str_trim(tclvalue(ncLON))
		nlat <- str_trim(tclvalue(ncLAT))

		nl <- length(.cdtData$EnvData$ncvar)
		.cdtData$EnvData$ncData$MapOp <- vector(mode = 'list', length = nl)
		for(j in 1:nl) .cdtData$EnvData$ncData$MapOp[[j]] <- .cdtData$EnvData$ncvar[[j]]$ncops

		pvar <- sapply(.cdtData$EnvData$ncvar, function(x) ifelse(tclvalue(x$chk.var) == "1", TRUE, FALSE))
		if(!any(pvar)){
			Insert.Messages.Out("No selected variables to plot", format = TRUE)
			.cdtData$EnvData$ncData$map <- NULL
			return(NULL)
		}

		readNCFILE <- TRUE
		if(!is.null(.cdtData$EnvData$ncData))
			if(!is.null(.cdtData$EnvData$ncData$ncfile))
				if(.cdtData$EnvData$ncData$ncfile == ncfile.path &
					all(.cdtData$EnvData$ncData$pvar == pvar)) readNCFILE <- FALSE

		if(readNCFILE){
			.cdtData$EnvData$ncData$map <- vector(mode = 'list', length = nl)

			nc <- try(nc_open(ncfile.path), silent = TRUE)
			if(inherits(nc, "try-error")){
				Insert.Messages.Out(paste("Unable to open:", ncfile.path), format = TRUE)
				.cdtData$EnvData$ncData$map <- NULL
				return(NULL)
			}
			for(j in which(pvar)){
				varid <- .cdtData$EnvData$ncvar[[j]]$var.name
				idim <- match(sapply(nc$var[[varid]]$dim, "[[", "name"), c(nlon, nlat))
				idim <- idim[!is.na(idim)]
				if(length(idim) != 2){
					Insert.Messages.Out(paste(varid, "not a spatial data"), format = TRUE)
					.cdtData$EnvData$ncData$map <- NULL
					return(NULL)
				}
				xlon <- nc$var[[varid]]$dim[[idim[1]]]$vals
				xlat <- nc$var[[varid]]$dim[[idim[2]]]$vals
				ncdon <- ncvar_get(nc, varid = varid)
				xo <- order(xlon)
				xlon <- xlon[xo]
				yo <- order(xlat)
				xlat <- xlat[yo]
				ncdon <- if(idim[1] < idim[2]) ncdon[xo, yo] else t(ncdon)[xo, yo]

				.cdtData$EnvData$ncData$map[[j]]$x <- xlon
				.cdtData$EnvData$ncData$map[[j]]$y <- xlat
				.cdtData$EnvData$ncData$map[[j]]$z <- ncdon
				.cdtData$EnvData$ncData$map[[j]]$title <- varid
			}
			nc_close(nc)

			.cdtData$EnvData$ncData$file2plot <- nc.file
			.cdtData$EnvData$ncData$ncfile <- ncfile.path
			.cdtData$EnvData$ncData$pvar <- pvar
		}

		return(0)
	}

	#######################################################################################################

	tkgrid(tknote.cmd, sticky = 'nwes')
	tkgrid.columnconfigure(tknote.cmd, 0, weight = 1)
	tkgrid.rowconfigure(tknote.cmd, 0, weight = 1)

	tcl('update')
	tkgrid(.cdtEnv$tcl$main$cmd.frame, sticky = 'nwes', pady = 1)
	tkgrid.columnconfigure(.cdtEnv$tcl$main$cmd.frame, 0, weight = 1)
	tkgrid.rowconfigure(.cdtEnv$tcl$main$cmd.frame, 0, weight = 1)

	invisible()
}
