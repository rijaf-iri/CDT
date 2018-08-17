
qcRRZeroCheckPanelCmd <- function(){
	listOpenFiles <- openFile_ttkcomboList()
	if(WindowsOS()){
		largeur1 <- .cdtEnv$tcl$fun$w.widgets(27)
		largeur2 <- .cdtEnv$tcl$fun$w.widgets(29)
		largeur3 <- 19
		largeur4 <- 24
	}else{
		largeur1 <- .cdtEnv$tcl$fun$w.widgets(22)
		largeur2 <- .cdtEnv$tcl$fun$w.widgets(23)
		largeur3 <- 12
		largeur4 <- 19
	}

	GeneralParameters <- list(infile = "", outdir = "",
							params = list(min.nbrs = 4, max.nbrs = 8,
										min.days = 22, max.dist = 30,
										min.thrs = 1.5))

	# xml.dlg <- file.path(.cdtDir$dirLocal, "languages", "cdtQC_RR.ZeroCheck_leftCmd.xml")
	# lang.dlg <- cdtLanguageParse(xml.dlg, .cdtData$Config$lang.iso)
	# .cdtData$EnvData$message <- lang.dlg[['message']]

	###################

	.cdtEnv$tcl$main$cmd.frame <- tkframe(.cdtEnv$tcl$main$panel.left)

	tknote.cmd <- bwNoteBook(.cdtEnv$tcl$main$cmd.frame)
	cmd.tab1 <- bwAddTab(tknote.cmd, text = "Data")
	cmd.tab2 <- bwAddTab(tknote.cmd, text = "False-Zeros")

	bwRaiseTab(tknote.cmd, cmd.tab1)

	tkgrid.columnconfigure(cmd.tab1, 0, weight = 1)
	tkgrid.columnconfigure(cmd.tab2, 0, weight = 1)

	tkgrid.rowconfigure(cmd.tab1, 0, weight = 1)
	tkgrid.rowconfigure(cmd.tab2, 0, weight = 1)

	#######################################################################################################

	#Tab1
	subfr1 <- bwTabScrollableFrame(cmd.tab1)

		#######################

		frameInData <- ttklabelframe(subfr1, text = "Station Data", relief = 'groove')

		input.file <- tclVar(GeneralParameters$infile)

		txt.infile <- tklabel(frameInData, text = 'File containing daily Precip data', anchor = 'w', justify = 'left')
		cb.infile <- ttkcombobox(frameInData, values = unlist(listOpenFiles), textvariable = input.file, width = largeur1)
		bt.infile <- tkbutton(frameInData, text = "...")

		tkconfigure(bt.infile, command = function(){
			dat.opfiles <- getOpenFiles(.cdtEnv$tcl$main$win)
			if(!is.null(dat.opfiles)){
				update.OpenFiles('ascii', dat.opfiles)
				listOpenFiles[[length(listOpenFiles) + 1]] <<- dat.opfiles[[1]]
				tclvalue(input.file) <- dat.opfiles[[1]]
				tkconfigure(cb.infile, values = unlist(listOpenFiles))
			}
		})

		tkgrid(txt.infile, row = 0, column = 0, sticky = 'we', rowspan = 1, columnspan = 5, padx = 1, pady = 1, ipadx = 1, ipady = 1)
		tkgrid(cb.infile, row = 1, column = 0, sticky = 'we', rowspan = 1, columnspan = 4, padx = 0, pady = 1, ipadx = 1, ipady = 1)
		tkgrid(bt.infile, row = 1, column = 4, sticky = 'w', rowspan = 1, columnspan = 1, padx = 0, pady = 1, ipadx = 1, ipady = 1)

		helpWidget(cb.infile, 'Select the file containing stations daily precipitation data', 'Select the file containing stations daily precipitation data')
		helpWidget(bt.infile, 'Browse file if not listed', 'Browse file if not listed')

		#######################

		frameParams <- tkframe(subfr1, relief = 'sunken', borderwidth = 2, padx = 3, pady = 3)

		bt.params <- ttkbutton(frameParams, text = "False-Zeros Check Parameters")

		tkconfigure(bt.params, command = function(){
			Params <- GeneralParameters[["params"]]
			GeneralParameters[["params"]] <<- getParams.RR.ZerosCheck(Params)
		})

		tkgrid(bt.params, row = 0, column = 0, sticky = '', padx = 1, pady = 1, ipadx = 3, ipady = 1)

		helpWidget(bt.params, 'Set the parameters of the False-Zeros check', 'Set the parameters of the False-Zeros check')

		#######################

		frameDirSav <- tkframe(subfr1, relief = 'groove', borderwidth = 2)

		dir.save <- tclVar(GeneralParameters$outdir)

		txt.dir.save <- tklabel(frameDirSav, text = "Directory to save the outputs", anchor = 'w', justify = 'left')
		en.dir.save <- tkentry(frameDirSav, textvariable = dir.save, width = largeur2)
		bt.dir.save <- tkbutton(frameDirSav, text = "...")

		######
		tkconfigure(bt.dir.save, command = function() fileORdir2Save(dir.save, isFile = FALSE))

		######
		tkgrid(txt.dir.save, row = 0, column = 0, sticky = 'we', rowspan = 1, columnspan = 5, padx = 1, pady = 0, ipadx = 1, ipady = 1)
		tkgrid(en.dir.save, row = 1, column = 0, sticky = 'we', rowspan = 1, columnspan = 4, padx = 0, pady = 0, ipadx = 1, ipady = 1)
		tkgrid(bt.dir.save, row = 1, column = 4, sticky = 'w', rowspan = 1, columnspan = 1, padx = 0, pady = 0, ipadx = 1, ipady = 1)

		helpWidget(en.dir.save, 'Enter the full path to directory to save outputs',
								'Enter the full path to directory to save outputs')
		helpWidget(bt.dir.save, 'or browse here', 'or browse here')

		#############################

		bt.checkZeros <- ttkbutton(subfr1, text = "Check False-Zeros")

		tkconfigure(bt.checkZeros, command = function(){
			GeneralParameters$infile <- str_trim(tclvalue(input.file))
			GeneralParameters$outdir <- str_trim(tclvalue(dir.save))

			# assign('GeneralParameters', GeneralParameters, envir = .GlobalEnv)

			Insert.Messages.Out("Check False-Zeros ......")

			tkconfigure(.cdtEnv$tcl$main$win, cursor = 'watch')
			tcl('update')
			ret <- tryCatch(
				{
					qcRRZeroCheckProcs(GeneralParameters)
				},
				warning = function(w) warningFun(w),
				error = function(e) errorFun(e),
				finally = {
					tkconfigure(.cdtEnv$tcl$main$win, cursor = '')
					tcl('update')
				}
			)

			msg0 <- "False-zeros checking finished successfully"
			msg1 <- "False-zeros checking failed"

			if(!is.null(ret)){
				if(ret == 0){
					Insert.Messages.Out(msg0)

					if(is.null(.cdtData$EnvData$outzeros)){
						Insert.Messages.Out('No suspicious False-Zeros found')
						return(NULL)
					}

					###############
					set.station.id()
					ret <- try(set.date.false.zeros(), silent = TRUE)
					if(inherits(ret, "try-error") | is.null(ret)) return(NULL)
				}else Insert.Messages.Out(msg1, format = TRUE)
			}else Insert.Messages.Out(msg1, format = TRUE)
		})

		#########################################

		tkgrid(frameInData, row = 0, column = 0, sticky = 'we', padx = 1, pady = 1, ipadx = 1, ipady = 1)
		tkgrid(frameParams, row = 1, column = 0, sticky = '', padx = 1, pady = 1, ipadx = 1, ipady = 1)
		tkgrid(frameDirSav, row = 2, column = 0, sticky = 'we', padx = 1, pady = 1, ipadx = 1, ipady = 1)
		tkgrid(bt.checkZeros, row = 3, column = 0, sticky = '', padx = 1, pady = 3, ipadx = 1, ipady = 1)

	#######################################################################################################

	#Tab2
	subfr2 <- bwTabScrollableFrame(cmd.tab2)

		#######################

		frameZeros <- ttklabelframe(subfr2, text = "False-Zeros Data", relief = 'groove')

		zeroExist <- tclVar(0)
		file.dataIndex <- tclVar()

		chk.dataIdx <- tkcheckbutton(frameZeros, variable = zeroExist, text = "Load existing False-Zeros Check session", anchor = 'w', justify = 'left')
		en.dataIdx <- tkentry(frameZeros, textvariable = file.dataIndex, width = largeur2, state = "disabled")
		bt.dataIdx <- tkbutton(frameZeros, text = "...", state = "disabled")

		tkconfigure(bt.dataIdx, command = function(){
			path.dataIdx <- tclvalue(tkgetOpenFile(initialdir = getwd(), filetypes = .cdtEnv$tcl$data$filetypes6))
			if(path.dataIdx %in% c("", "NA") | is.na(path.dataIdx)) return(NULL)
			tclvalue(file.dataIndex) <- path.dataIdx

			if(file.exists(str_trim(tclvalue(file.dataIndex)))){
				OutFZ <- try(readRDS(str_trim(tclvalue(file.dataIndex))), silent = TRUE)
				if(inherits(OutFZ, "try-error")){
					Insert.Messages.Out('Unable to load False-Zeros data', format = TRUE)
					Insert.Messages.Out(gsub('[\r\n]', '', OutFZ[1]), format = TRUE)
					tkconfigure(.cdtData$EnvData$STN$cb.stnID, values = "")
					tclvalue(.cdtData$EnvData$STN$stnID) <- ""
					tkconfigure(.cdtData$EnvData$STN$cb.stnFZ, values = "")
					tclvalue(.cdtData$EnvData$STN$dateFZ) <- ""
					return(NULL)
				}

				.cdtData$EnvData$output <- OutFZ
				.cdtData$EnvData$PathData <- dirname(str_trim(tclvalue(file.dataIndex)))

				###############
				file.checkd <- file.path(.cdtData$EnvData$PathData, 'CDTDATASET', "FalseZerosResults.rds")
				if(!file.exists(file.checkd)){
					Insert.Messages.Out(paste(file.checkd, 'not found'), format = TRUE)
					return(NULL)
				}
				.cdtData$EnvData$outzeros <- readRDS(file.checkd)
				if(is.null(.cdtData$EnvData$outzeros)){
					Insert.Messages.Out('No suspicious False-Zeros found')
					return(NULL)
				}

				###############
				file.don <- file.path(.cdtData$EnvData$PathData, 'CDTDATASET', "StationData.rds")
				if(!file.exists(file.don)){
					Insert.Messages.Out(paste(file.don, 'not found'), format = TRUE)
					return(NULL)
				}
				.cdtData$EnvData$stn.data <- readRDS(file.don)

				###############
				set.station.id()
				ret <- try(set.date.false.zeros(), silent = TRUE)
				if(inherits(ret, "try-error") | is.null(ret)) return(NULL)
			}
		})

		tkgrid(chk.dataIdx, row = 0, column = 0, sticky = 'we', rowspan = 1, columnspan = 5, padx = 1, pady = 1, ipadx = 1, ipady = 1)
		tkgrid(en.dataIdx, row = 1, column = 0, sticky = 'we', rowspan = 1, columnspan = 4, padx = 1, pady = 1, ipadx = 1, ipady = 1)
		tkgrid(bt.dataIdx, row = 1, column = 4, sticky = 'w', rowspan = 1, columnspan = 1, padx = 0, pady = 1, ipadx = 1, ipady = 1)

		###############
		tkbind(chk.dataIdx, "<Button-1>", function(){
			stateExistData <- if(tclvalue(zeroExist) == '1') 'disabled' else 'normal'
			tkconfigure(en.dataIdx, state = stateExistData)
			tkconfigure(bt.dataIdx, state = stateExistData)

			stateZero <- if(tclvalue(zeroExist) == '1') 'normal' else 'disabled'
			tkconfigure(cb.infile, state = stateZero)
			tkconfigure(bt.infile, state = stateZero)
			tkconfigure(bt.params, state = stateZero)
			tkconfigure(en.dir.save, state = stateZero)
			tkconfigure(bt.dir.save, state = stateZero)
			tkconfigure(bt.checkZeros, state = stateZero)
		})

		#######################

		frameStnId <- ttklabelframe(subfr2, text = "Select station", relief = 'groove')

		.cdtData$EnvData$STN$stnID <- tclVar()

		bt.stnID.prev <- ttkbutton(frameStnId, text = "<<", width = 5)
		bt.stnID.next <- ttkbutton(frameStnId, text = ">>", width = 5)
		.cdtData$EnvData$STN$cb.stnID <- ttkcombobox(frameStnId, values = "", textvariable = .cdtData$EnvData$STN$stnID, width = largeur4, justify = 'center')

		tkconfigure(bt.stnID.prev, command = function(){
			if(!is.null(.cdtData$EnvData$outzeros)){
				STNID <- .cdtData$EnvData$outzeros$stn
				istn <- which(STNID == str_trim(tclvalue(.cdtData$EnvData$STN$stnID)))
				istn <- istn - 1
				if(istn < 1) istn <- length(STNID)
				tclvalue(.cdtData$EnvData$STN$stnID) <- STNID[istn]

				ret <- try(set.date.false.zeros(), silent = TRUE)
				if(inherits(ret, "try-error") | is.null(ret)) return(NULL)
			}
		})

		tkconfigure(bt.stnID.next, command = function(){
			if(!is.null(.cdtData$EnvData$outzeros)){
				STNID <- .cdtData$EnvData$outzeros$stn
				istn <- which(STNID == str_trim(tclvalue(.cdtData$EnvData$STN$stnID)))
				istn <- istn + 1
				if(istn > length(STNID)) istn <- 1
				tclvalue(.cdtData$EnvData$STN$stnID) <- STNID[istn]

				ret <- try(set.date.false.zeros(), silent = TRUE)
				if(inherits(ret, "try-error") | is.null(ret)) return(NULL)
			}
		})

		tkgrid(bt.stnID.prev, row = 0, column = 0, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 2, ipadx = 1, ipady = 1)
		tkgrid(.cdtData$EnvData$STN$cb.stnID, row = 0, column = 1, sticky = 'we', rowspan = 1, columnspan = 2, padx = 1, pady = 2, ipadx = 1, ipady = 1)
		tkgrid(bt.stnID.next, row = 0, column = 3, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 2, ipadx = 1, ipady = 1)

		tkbind(.cdtData$EnvData$STN$cb.stnID, "<<ComboboxSelected>>", function(){
			if(!is.null(.cdtData$EnvData$outzeros)){
				ret <- try(set.date.false.zeros(), silent = TRUE)
				if(inherits(ret, "try-error") | is.null(ret)) return(NULL)
			}
		})

		#######################

		bt.display.FZ <- ttkbutton(subfr2, text = "View & Edit Result")

		.cdtData$EnvData$tab$TableStat <- NULL

		tkconfigure(bt.display.FZ, command = function(){
			stnid <- str_trim(tclvalue(.cdtData$EnvData$STN$stnID))
			if(stnid == "") return(NULL)
			donFZstat <- .cdtData$EnvData$outzeros$res[[stnid]]$stat
			tab.title <- paste0(stnid, "-False-Zeros-Check")

			.cdtData$EnvData$tab$TableStat <- tableNotebookTab_unik(donFZstat,
																.cdtData$EnvData$tab$TableStat,
																tab.title, 12, 'falsezero')
			defile.menu.OpenTable()
			.cdtData$EnvData$STN$toreplace <- stnid
		})

		#######################

		frameStnFZ <- ttklabelframe(subfr2, text = 'Display neighbors stations', relief = 'groove')

		.cdtData$EnvData$STN$dateFZ <- tclVar()

		bt.stnFZ.prev <- ttkbutton(frameStnFZ, text = "<<", width = 4)
		bt.stnFZ.next <- ttkbutton(frameStnFZ, text = ">>", width = 4)
		bt.stnFZ <- ttkbutton(frameStnFZ, text = "DISPLAY", width = 7)
		.cdtData$EnvData$STN$cb.stnFZ <- ttkcombobox(frameStnFZ, values = "", textvariable = .cdtData$EnvData$STN$dateFZ, width = largeur3, justify = 'center')

		.cdtData$EnvData$tab$TableNbrs <- NULL

		tkconfigure(bt.stnFZ.prev, command = function(){
			stnid <- str_trim(tclvalue(.cdtData$EnvData$STN$stnID))
			if(stnid == "") return(NULL)
			dateFZ <- .cdtData$EnvData$outzeros$res[[stnid]]$date
			idaty <- which(dateFZ == str_trim(tclvalue(.cdtData$EnvData$STN$dateFZ)))
			idaty <- idaty - 1
			if(idaty < 1) idaty <- length(dateFZ)
			tclvalue(.cdtData$EnvData$STN$dateFZ) <- dateFZ[idaty]

			display.month.neighbors()
		})

		tkconfigure(bt.stnFZ.next, command = function(){
			stnid <- str_trim(tclvalue(.cdtData$EnvData$STN$stnID))
			if(stnid == "") return(NULL)
			dateFZ <- .cdtData$EnvData$outzeros$res[[stnid]]$date
			idaty <- which(dateFZ == str_trim(tclvalue(.cdtData$EnvData$STN$dateFZ)))
			idaty <- idaty + 1
			if(idaty > length(dateFZ)) idaty <- 1
			tclvalue(.cdtData$EnvData$STN$dateFZ) <- dateFZ[idaty]

			display.month.neighbors()
		})

		tkconfigure(bt.stnFZ, command = function(){
			stnid <- str_trim(tclvalue(.cdtData$EnvData$STN$stnID))
			if(stnid == "") return(NULL)

			display.month.neighbors()
		})

		tkgrid(bt.stnFZ.prev, row = 0, column = 0, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 2, ipadx = 1, ipady = 1)
		tkgrid(.cdtData$EnvData$STN$cb.stnFZ, row = 0, column = 1, sticky = 'we', rowspan = 1, columnspan = 2, padx = 1, pady = 2, ipadx = 1, ipady = 1)
		tkgrid(bt.stnFZ.next, row = 0, column = 3, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 2, ipadx = 1, ipady = 1)
		tkgrid(bt.stnFZ, row = 0, column = 4, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 2, ipadx = 1, ipady = 1)

		helpWidget(frameStnFZ, 'Display the neighbors values on a table for the same month',
								'Display the neighbors values on a table for the same month')

		#######################

		bt.replace.FZ <- ttkbutton(subfr2, text = "Replace False Zeros")

		tkconfigure(bt.replace.FZ, command = function(){
			if(is.null(.cdtData$EnvData$outzeros)) return(NULL)

			stnid <- str_trim(tclvalue(.cdtData$EnvData$STN$stnID))

			replace <- TRUE
			if(!is.null(.cdtData$EnvData$STN$toreplace))
				if(.cdtData$EnvData$STN$toreplace != stnid) replace <- FALSE

			if(replace){
				info <- .cdtData$EnvData$output$info[[3]]
				file.stn <- file.path(.cdtData$EnvData$PathData, 'CDTSTATIONS', .cdtData$EnvData$output$info[[1]])
				tmp <- fread(file.stn, header = FALSE, sep = info$sepr, stringsAsFactors = FALSE, colClasses = "character")

				daty <- .cdtData$EnvData$outzeros$res[[stnid]]$date
				index.mon <- unlist(.cdtData$EnvData$output$index[daty])
				is.elv <- if(is.null(.cdtData$EnvData$stn.data$elv)) 3 else 4
				index.mon <- index.mon + is.elv
				istn <- which(.cdtData$EnvData$stn.data$id == stnid) + 1
				replace0 <- tmp[index.mon, istn, with = FALSE]
				replace0[!is.na(replace0) & replace0 == 0] <- info$miss.val
				tmp[index.mon, istn] <- replace0

				fwrite(tmp, file = file.stn, quote = FALSE, sep = info$sepr, col.names = FALSE, na = info$miss.val)
				rm(tmp); gc()
				Insert.Messages.Out(paste(stnid, ': replacement done'))
			}else{
				Insert.Messages.Out("No replacement performed")
			}
		})

		helpWidget(bt.replace.FZ, 'Replaces false zeros with missing values', 'Replaces false zeros with missing values')

		#########################################

		tkgrid(frameZeros, row = 0, column = 0, sticky = 'we', padx = 1, pady = 1, ipadx = 1, ipady = 1)
		tkgrid(frameStnId, row = 1, column = 0, sticky = 'we', padx = 1, pady = 1, ipadx = 1, ipady = 1)
		tkgrid(bt.display.FZ, row = 2, column = 0, sticky = 'we', padx = 1, pady = 5, ipadx = 1, ipady = 1)
		tkgrid(frameStnFZ, row = 3, column = 0, sticky = '', padx = 1, pady = 1, ipadx = 1, ipady = 1)
		tkgrid(bt.replace.FZ, row = 4, column = 0, sticky = 'we', padx = 1, pady = 5, ipadx = 1, ipady = 1)

	#######################################################################################################

	set.station.id <- function(){
		STNID <- .cdtData$EnvData$outzeros$stn
		tkconfigure(.cdtData$EnvData$STN$cb.stnID, values = STNID)
		tclvalue(.cdtData$EnvData$STN$stnID) <- STNID[1]
	}

	set.date.false.zeros <- function(){
		stnid <- str_trim(tclvalue(.cdtData$EnvData$STN$stnID))
		if(stnid == "") return(NULL)
		dateFZ <- .cdtData$EnvData$outzeros$res[[stnid]]$date
		tkconfigure(.cdtData$EnvData$STN$cb.stnFZ, values = dateFZ)
		tclvalue(.cdtData$EnvData$STN$dateFZ) <- dateFZ[1]
		return(0)
	}

	display.month.neighbors <- function(){
		stnid <- str_trim(tclvalue(.cdtData$EnvData$STN$stnID))
		daty <- str_trim(tclvalue(.cdtData$EnvData$STN$dateFZ))
		index.mon <- .cdtData$EnvData$output$index[[daty]]
		dist <- c(0, .cdtData$EnvData$outzeros$res[[stnid]]$dist[[daty]])
		istn <- which(.cdtData$EnvData$stn.data$id == stnid)
		nstn <- .cdtData$EnvData$outzeros$res[[stnid]]$stn[[daty]]
		STN <- .cdtData$EnvData$stn.data$id[c(istn, nstn)]
		don.nbrs <- .cdtData$EnvData$stn.data$data[index.mon, c(istn, nstn), drop = FALSE]
		don.date <- .cdtData$EnvData$stn.data$dates[index.mon]
		don.disp <- rbind(cbind(c("STN.ID", "Distance (km)"), rbind(STN, dist)), cbind(don.date, don.nbrs))
		dimnames(don.disp) <- NULL

		.cdtData$EnvData$tab$TableNbrs <- tableNotebookTab_unik(don.disp,
															.cdtData$EnvData$tab$TableNbrs,
															paste0(stnid, "-neighbors"))
		tabid <- as.integer(tclvalue(tkindex(.cdtEnv$tcl$main$tknotes, 'current'))) + 1
		table1 <- .cdtData$OpenTab$Data[[tabid]][[2]][[1]]

		.Tcl(paste(table1, 'tag', 'celltag', 'ZchkStn',
			paste(1:as.integer(tclvalue(tkindex(table1, 'end', 'row'))), 2, sep = ',', collapse = ' ')))
		tcl(table1, "tag", "configure", "ZchkStn", bg = "lightgoldenrod1")
		.Tcl(paste(table1, 'tag', 'celltag', 'idZchkStn1',
			paste(1, 1:as.integer(tclvalue(tkindex(table1, 'end', 'col'))), sep = ',', collapse = ' ')))
		tcl(table1, "tag", "configure", "idZchkStn1", bg = "lightcyan1")
		.Tcl(paste(table1, 'tag', 'celltag', 'idZchkStn2',
			paste(2, 1:as.integer(tclvalue(tkindex(table1, 'end', 'col'))), sep = ',', collapse = ' ')))
		tcl(table1, "tag", "configure", "idZchkStn2", bg = "lightcyan1")
	}

	#######################################################################################################

	.cdtData$EnvData$qcRRZeroCheck$SaveEdit <- function(dat2sav){
		stnid <- str_trim(tclvalue(.cdtData$EnvData$STN$stnID))
		.cdtData$EnvData$STN$toreplace <- stnid

		tout.null <- TRUE
		if(!is.null(dat2sav)){
			daty <- str_trim(as.character(dat2sav$YYYYMM))
			ix <- !is.na(daty) & daty != ""

			if(any(ix)){
				dat2sav <- dat2sav[ix, , drop = FALSE]
				daty <- daty[ix]
				idaty <- .cdtData$EnvData$outzeros$res[[stnid]]$date %in% daty
				.cdtData$EnvData$outzeros$res[[stnid]]$stat <- dat2sav
				.cdtData$EnvData$outzeros$res[[stnid]]$date <- daty
				.cdtData$EnvData$outzeros$res[[stnid]]$dist <- .cdtData$EnvData$outzeros$res[[stnid]]$dist[idaty]
				.cdtData$EnvData$outzeros$res[[stnid]]$stn <- .cdtData$EnvData$outzeros$res[[stnid]]$stn[idaty]
				tout.null <- FALSE
			}
		}

		if(tout.null){
			STN.ID <- .cdtData$EnvData$outzeros$stn
			STN.ID <- STN.ID[!STN.ID %in% stnid]
			if(length(STN.ID)){
				.cdtData$EnvData$outzeros$stn <- STN.ID
				.cdtData$EnvData$outzeros$res[[stnid]] <- NULL
			}else .cdtData$EnvData$outzeros <- NULL
		}

		file.checkd <- file.path(.cdtData$EnvData$PathData, 'CDTDATASET', "FalseZerosResults.rds")
		saveRDS(.cdtData$EnvData$outzeros, file.checkd)

		if(is.null(.cdtData$EnvData$outzeros)){
			tkconfigure(.cdtData$EnvData$STN$cb.stnID, values = "")
			tclvalue(.cdtData$EnvData$STN$stnID) <- ""
			tkconfigure(.cdtData$EnvData$STN$cb.stnFZ, values = "")
			tclvalue(.cdtData$EnvData$STN$dateFZ) <- ""
		}else{
			STNID <- .cdtData$EnvData$outzeros$stn
			tkconfigure(.cdtData$EnvData$STN$cb.stnID, values = STNID)
			stnid <- str_trim(tclvalue(.cdtData$EnvData$STN$stnID))
			STN <- .cdtData$EnvData$stn.data$id
			while(!stnid %in% STNID){
				is <- which(STN == stnid) + 1
				if(is > length(STN)) is <- 1
				stnid <- STN[is]
			}
			tclvalue(.cdtData$EnvData$STN$stnID) <- stnid

			dateFZ <- .cdtData$EnvData$outzeros$res[[stnid]]$date
			tkconfigure(.cdtData$EnvData$STN$cb.stnFZ, values = dateFZ)
			idaty <- str_trim(tclvalue(.cdtData$EnvData$STN$dateFZ))
			if(!idaty %in% dateFZ) idaty <- dateFZ[1]
			tclvalue(.cdtData$EnvData$STN$dateFZ) <- idaty
		}
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
