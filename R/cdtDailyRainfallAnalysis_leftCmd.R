
dailyRainAnalysisPanelCmd <- function(){
	listOpenFiles <- openFile_ttkcomboList()
	if(WindowsOS()){
		largeur0 <- .cdtEnv$tcl$fun$w.widgets(22)
		largeur1 <- .cdtEnv$tcl$fun$w.widgets(27)
		largeur2 <- .cdtEnv$tcl$fun$w.widgets(29)
		largeur3 <- 24
		largeur4 <- 28
		largeur5 <- 21
		largeur6 <- 22
	}else{
		largeur0 <- .cdtEnv$tcl$fun$w.widgets(20)
		largeur1 <- .cdtEnv$tcl$fun$w.widgets(21)
		largeur2 <- .cdtEnv$tcl$fun$w.widgets(23)
		largeur3 <- 24
		largeur4 <- 22
		largeur5 <- 14
		largeur6 <- 14
	}

	MOIS <- format(ISOdate(2014, 1:12, 1), "%b")
	varsname <- list(name = c("TOTALRAIN", "RAININT", "WETDAY", "DRYDAY", "WETSPELL", "DRYSPELL"),
					longname = c('Total Rainfall', 'Rainfall Intensity', 'Number of Wet Days',
					'Number of Dry Days', 'Number of Wet Spells', 'Number of Dry Spells'))
	statsname <- list(name = c('mean', 'stdev', 'coefvar', 'proba'),
					longname = c('Mean', 'Standard deviation', 'Coefficient of variation',
								'Probability of exceeding'))

	GeneralParameters <- list(data.type = "cdtstation", cdtstation = "", cdtdataset = "",
							seas = list(all.years = TRUE,
										startYear = 1981, startMon = 9, startDay = 1,
										endYear = 2017, endMon = 12, endDay = 31),
							stats = list(daily = 'tot.rain', yearly = 'mean'),
							def = list(drywet.day = 0.85, drywet.spell = 7, proba.thres = 400),
							min.frac = 0.95, output = "")

	# xml.dlg <- file.path(.cdtDir$dirLocal, "languages", "cdtSpatialAnalysis_leftCmd.xml")
	# lang.dlg <- cdtLanguageParse(xml.dlg, .cdtData$Config$lang.iso)
	# .cdtData$EnvData$message <- lang.dlg[['message']]

	###################

	.cdtEnv$tcl$main$cmd.frame <- tkframe(.cdtEnv$tcl$main$panel.left)

	tknote.cmd <- bwNoteBook(.cdtEnv$tcl$main$cmd.frame)
	cmd.tab1 <- bwAddTab(tknote.cmd, text = "Input")
	cmd.tab2 <- bwAddTab(tknote.cmd, text = "Analysis")
	cmd.tab3 <- bwAddTab(tknote.cmd, text = "Maps")
	cmd.tab4 <- bwAddTab(tknote.cmd, text = "Graphs")
	cmd.tab5 <- bwAddTab(tknote.cmd, text = "Boundaries")

	bwRaiseTab(tknote.cmd, cmd.tab1)

	tkgrid.columnconfigure(cmd.tab1, 0, weight = 1)
	tkgrid.columnconfigure(cmd.tab2, 0, weight = 1)
	tkgrid.columnconfigure(cmd.tab3, 0, weight = 1)
	tkgrid.columnconfigure(cmd.tab4, 0, weight = 1)
	tkgrid.columnconfigure(cmd.tab5, 0, weight = 1)

	tkgrid.rowconfigure(cmd.tab1, 0, weight = 1)
	tkgrid.rowconfigure(cmd.tab2, 0, weight = 1)
	tkgrid.rowconfigure(cmd.tab3, 0, weight = 1)
	tkgrid.rowconfigure(cmd.tab4, 0, weight = 1)
	tkgrid.rowconfigure(cmd.tab5, 0, weight = 1)

	#######################################################################################################

	#Tab1
	subfr1 <- bwTabScrollableFrame(cmd.tab1)

		############################################

		frameInData <- ttklabelframe(subfr1, text = "Input Data", relief = 'groove')

		DataType <- tclVar()
		CbdatatypeVAL <- .cdtEnv$tcl$lang$global[['combobox']][['2']][1:2]
		datatypeVAL <- c('cdtstation', 'cdtdataset')
		tclvalue(DataType) <- CbdatatypeVAL[datatypeVAL %in% GeneralParameters$data.type]

		if(GeneralParameters$data.type == 'cdtstation'){
			input.Prec <- tclVar(GeneralParameters$cdtstation)
			txt.INPrec <- 'File containing stations daily Precip data'
		}else{
			input.Prec <- tclVar(GeneralParameters$cdtdataset)
			txt.INPrec <- 'Index file (*.rds) for daily Precip data'
		}
		txt.INPrec.var <- tclVar(txt.INPrec)

		txt.datatype <- tklabel(frameInData, text = "Format", anchor = 'w', justify = 'left')
		cb.datatype <- ttkcombobox(frameInData, values = CbdatatypeVAL, textvariable = DataType, width = largeur0)

		txt.INPrec <- tklabel(frameInData, text = tclvalue(txt.INPrec.var), textvariable = txt.INPrec.var, anchor = 'w', justify = 'left')
		if(GeneralParameters$data.type == 'cdtstation'){
			cb.en.INPrec <- ttkcombobox(frameInData, values = unlist(listOpenFiles), textvariable = input.Prec, width = largeur1)
		}else{
			cb.en.INPrec <- tkentry(frameInData, textvariable = input.Prec, width = largeur2)
		}
		bt.INPrec <- tkbutton(frameInData, text = "...")

		############

		tkconfigure(bt.INPrec, command = function(){
			if(GeneralParameters$data.type == 'cdtstation'){
				dat.opfiles <- getOpenFiles(.cdtEnv$tcl$main$win)
				if(!is.null(dat.opfiles)){
					update.OpenFiles('ascii', dat.opfiles)
					listOpenFiles[[length(listOpenFiles) + 1]] <<- dat.opfiles[[1]]
					tclvalue(input.Prec) <- dat.opfiles[[1]]
					tkconfigure(cb.en.INPrec, values = unlist(listOpenFiles))
				}
			}else{
				path.rds <- tclvalue(tkgetOpenFile(initialdir = getwd(), filetypes = .cdtEnv$tcl$data$filetypes6))
				tclvalue(input.Prec) <- if(path.rds %in% c("", "NA") | is.na(path.rds)) "" else path.rds
			}
		})

		############
		tkgrid(txt.datatype, row = 0, column = 0, sticky = 'we', rowspan = 1, columnspan = 2, padx = 1, pady = 1, ipadx = 1, ipady = 1)
		tkgrid(cb.datatype, row = 0, column = 2, sticky = 'we', rowspan = 1, columnspan = 8, padx = 1, pady = 1, ipadx = 1, ipady = 1)
		tkgrid(txt.INPrec, row = 1, column = 0, sticky = 'we', rowspan = 1, columnspan = 10, padx = 1, pady = 1, ipadx = 1, ipady = 1)
		tkgrid(cb.en.INPrec, row = 2, column = 0, sticky = 'we', rowspan = 1, columnspan = 9, padx = 0, pady = 1, ipadx = 1, ipady = 1)
		tkgrid(bt.INPrec, row = 2, column = 9, sticky = 'w', rowspan = 1, columnspan = 1, padx = 0, pady = 1, ipadx = 1, ipady = 1)

		############
		infobulle(cb.datatype, 'Select the format of the input data')
		status.bar.display(cb.datatype, 'Select the format of the input data')

		if(GeneralParameters$data.type == 'cdtstation'){
			infobulle(cb.en.INPrec, 'Select the file containing the daily precipitation')
			status.bar.display(cb.en.INPrec, 'Select the file containing the daily precipitation')

			infobulle(bt.INPrec, 'Browse file if not listed')
			status.bar.display(bt.INPrec, 'Browse file if not listed')
		}else{
			infobulle(cb.en.INPrec, 'Enter the full path to the file <daily precipitation dataset name>.rds')
			status.bar.display(cb.en.INPrec, 'Enter the full path to the file <daily precipitation dataset name>.rds')

			infobulle(bt.INPrec, 'or browse here')
			status.bar.display(bt.INPrec, 'or browse here')
		}

		############

		tkbind(cb.datatype, "<<ComboboxSelected>>", function(){
			tkdestroy(cb.en.INPrec)
			tclvalue(input.Prec) <- ''

			###
			if(str_trim(tclvalue(DataType)) == CbdatatypeVAL[1]){
				tclvalue(txt.INPrec.var) <- 'File containing stations daily Precip data'

				cb.en.INPrec <- ttkcombobox(frameInData, values = unlist(listOpenFiles), textvariable = input.Prec, width = largeur1)

				######
				tkconfigure(bt.INPrec, command = function(){
					dat.opfiles <- getOpenFiles(.cdtEnv$tcl$main$win)
					if(!is.null(dat.opfiles)){
						update.OpenFiles('ascii', dat.opfiles)
						listOpenFiles[[length(listOpenFiles) + 1]] <<- dat.opfiles[[1]]
						tclvalue(input.Prec) <- dat.opfiles[[1]]
						tkconfigure(cb.en.INPrec, values = unlist(listOpenFiles))
					}
				})

				######
				infobulle(cb.en.INPrec, 'Select the file containing the daily precipitation')
				status.bar.display(cb.en.INPrec, 'Select the file containing the daily precipitation')

				infobulle(bt.INPrec, 'Browse file if not listed')
				status.bar.display(bt.INPrec, 'Browse file if not listed')
			}

			###
			if(str_trim(tclvalue(DataType)) == CbdatatypeVAL[2]){
				tclvalue(txt.INPrec.var) <- 'Index file (*.rds) for daily Precip data'

				cb.en.INPrec <- tkentry(frameInData, textvariable = input.Prec, width = largeur2)

				######
				tkconfigure(bt.INPrec, command = function(){
					path.rds <- tclvalue(tkgetOpenFile(initialdir = getwd(), filetypes = .cdtEnv$tcl$data$filetypes6))
					tclvalue(input.Prec) <- if(path.rds %in% c("", "NA")) "" else path.rds
				})

				######
				infobulle(cb.en.INPrec, 'Enter the full path to the file <daily precipitation dataset name>.rds')
				status.bar.display(cb.en.INPrec, 'Enter the full path to the file <daily precipitation dataset name>.rds')

				infobulle(bt.INPrec, 'or browse here')
				status.bar.display(bt.INPrec, 'or browse here')
			}

			#######
			tkgrid(cb.en.INPrec, row = 2, column = 0, sticky = 'we', rowspan = 1, columnspan = 9, padx = 0, pady = 1, ipadx = 1, ipady = 1)
		})

		############################################

		frameSeas <- ttklabelframe(subfr1, text = "Season", relief = 'groove')

		allYears <- tclVar(GeneralParameters$seas$all.years)
		startYear <- tclVar(GeneralParameters$seas$startYear)
		endYear <- tclVar(GeneralParameters$seas$endYear)

		mon1 <- as.numeric(str_trim(GeneralParameters$seas$startMon))
		startMon <- tclVar(MOIS[mon1])
		mon2 <- as.numeric(str_trim(GeneralParameters$seas$endMon))
		endMon <- tclVar(MOIS[mon2])

		min.frac <- tclVar(GeneralParameters$min.frac)

		stateYear <- if(GeneralParameters$seas$all.years) 'disabled' else 'normal'

		chk.allYears <- tkcheckbutton(frameSeas, variable = allYears, text = "Use all years from the input data", anchor = 'w', justify = 'left')
		txt.startYear <- tklabel(frameSeas, text = "Start Year", anchor = 'e', justify = 'right')
		en.startYear <- tkentry(frameSeas, textvariable = startYear, width = 6, state = stateYear)
		txt.endYear <- tklabel(frameSeas, text = "End Year", anchor = 'e', justify = 'right')
		en.endYear <- tkentry(frameSeas, textvariable = endYear, width = 6, state = stateYear)

		frMonDay <- tkframe(frameSeas)
		txt.startMon <- tklabel(frMonDay, text = "From")
		cb.startMon <- ttkcombobox(frMonDay, values = MOIS, textvariable = startMon, width = 6)
		spin.startDay <- ttkspinbox(frMonDay, from = 1, to = 31, increment = 1, justify = 'center', width = 2)
		tkset(spin.startDay, GeneralParameters$seas$startDay)

		txt.endMon <- tklabel(frMonDay, text = "to")
		cb.endMon <- ttkcombobox(frMonDay, values = MOIS, textvariable = endMon, width = 6)
		spin.endDay <- ttkspinbox(frMonDay, from = 1, to = 31, increment = 1, justify = 'center', width = 2)
		tkset(spin.endDay, GeneralParameters$seas$endDay)

		tkgrid(txt.startMon, cb.startMon, spin.startDay, txt.endMon, cb.endMon, spin.endDay)
		tkgrid.configure(txt.endMon, sticky = "we", padx = 3)

		frMinFrac <- tkframe(frameSeas)
		txt.minfrac <- tklabel(frMinFrac, text = "Minimum fraction of available data", anchor = 'w', justify = 'left')
		en.minfrac <- tkentry(frMinFrac, textvariable = min.frac, width = 5)

		tkgrid(txt.minfrac, en.minfrac)

		tkgrid(chk.allYears, row = 0, column = 0, sticky = 'we', rowspan = 1, columnspan = 7, padx = 1, pady = 1, ipadx = 1, ipady = 1)
		tkgrid(txt.startYear, row = 1, column = 0, sticky = 'e', rowspan = 1, columnspan = 2, padx = 0, pady = 1, ipadx = 1, ipady = 1)
		tkgrid(en.startYear, row = 1, column = 2, sticky = 'w', rowspan = 1, columnspan = 1, padx = 0, pady = 1, ipadx = 1, ipady = 1)
		tkgrid(txt.endYear, row = 1, column = 3, sticky = 'e', rowspan = 1, columnspan = 3, padx = 0, pady = 1, ipadx = 1, ipady = 1)
		tkgrid(en.endYear, row = 1, column = 6, sticky = 'w', rowspan = 1, columnspan = 1, padx = 0, pady = 1, ipadx = 1, ipady = 1)
		tkgrid(frMonDay, row = 2, column = 0, sticky = 'we', rowspan = 1, columnspan = 7, padx = 1, pady = 3, ipadx = 1, ipady = 1)
		tkgrid(frMinFrac, row = 3, column = 0, sticky = 'we', rowspan = 1, columnspan = 7, padx = 1, pady = 3, ipadx = 1, ipady = 1)

		############
		tkbind(chk.allYears, "<Button-1>", function(){
			stateYear <- if(tclvalue(allYears) == '1') 'normal' else 'disabled'
			tkconfigure(en.startYear, state = stateYear)
			tkconfigure(en.endYear, state = stateYear)
		})

		############################################

		frameDirSav <- tkframe(subfr1, relief = 'groove', borderwidth = 2)

		dir.save <- tclVar(GeneralParameters$output)

		txt.dir.save <- tklabel(frameDirSav, text = "Directory to save results", anchor = 'w', justify = 'left')
		en.dir.save <- tkentry(frameDirSav, textvariable = dir.save, width = largeur2)
		bt.dir.save <- tkbutton(frameDirSav, text = "...")

		######
		tkconfigure(bt.dir.save, command = function() fileORdir2Save(dir.save, isFile = FALSE))

		######
		tkgrid(txt.dir.save, row = 0, column = 0, sticky = 'we', rowspan = 1, columnspan = 5, padx = 1, pady = 0, ipadx = 1, ipady = 1)
		tkgrid(en.dir.save, row = 1, column = 0, sticky = 'we', rowspan = 1, columnspan = 4, padx = 0, pady = 0, ipadx = 1, ipady = 1)
		tkgrid(bt.dir.save, row = 1, column = 4, sticky = 'w', rowspan = 1, columnspan = 1, padx = 0, pady = 0, ipadx = 1, ipady = 1)

		infobulle(en.dir.save, 'Enter the full path to directory to save outputs')
		status.bar.display(en.dir.save, 'Enter the full path to directory to save outputs')
		infobulle(bt.dir.save, 'or browse here')
		status.bar.display(bt.dir.save, 'or browse here')

		############################################

		tkgrid(frameInData, row = 0, column = 0, sticky = 'we', padx = 1, pady = 3, ipadx = 1, ipady = 1)
		tkgrid(frameSeas, row = 1, column = 0, sticky = '', padx = 1, pady = 3, ipadx = 1, ipady = 1)
		tkgrid(frameDirSav, row = 2, column = 0, sticky = 'we', padx = 1, pady = 3, ipadx = 1, ipady = 1)

	#######################################################################################################

	#Tab2
	subfr2 <- bwTabScrollableFrame(cmd.tab2)

		##############################################

		frameStats <- ttklabelframe(subfr2, text = "Statistics", relief = 'groove')

		daily.Stats <- tclVar()
		CbDailyStatsVAL <- c('Total Rainfall', 'Rainfall Intensity', 'Number of Wet Days',
							'Number of Dry Days', 'Number of Wet Spells', 'Number of Dry Spells')
		DailyStatsVAL <- c('tot.rain', 'rain.int', 'nb.wet.day', 'nb.dry.day', 'nb.wet.spell', 'nb.dry.spell')
		tclvalue(daily.Stats) <- CbDailyStatsVAL[DailyStatsVAL %in% GeneralParameters$stats$daily]

		yearly.Stats <- tclVar()
		CbYearlyStatsVAL <- c('Mean', 'Standard deviation', 'Coefficient of variation', 'Probability of exceeding')
		YearlyStatsVAL <- c('mean', 'stdev', 'coefvar', 'proba')
		tclvalue(yearly.Stats) <- CbYearlyStatsVAL[YearlyStatsVAL %in% GeneralParameters$stats$yearly]

		txt.StatDay <- tklabel(frameStats, text = 'Seasonal daily statistics', anchor = 'w', justify = 'left')
		cb.StatDay <- ttkcombobox(frameStats, values = CbDailyStatsVAL, textvariable = daily.Stats, width = largeur3)

		txt.StatYear <- tklabel(frameStats, text = 'Yearly seasonal statistics', anchor = 'w', justify = 'left')
		cb.StatYear <- ttkcombobox(frameStats, values = CbYearlyStatsVAL, textvariable = yearly.Stats, width = largeur3)

		########
		tkgrid(txt.StatDay, row = 0, column = 0, sticky = 'we', rowspan = 1, columnspan = 5, padx = 1, pady = 1, ipadx = 1, ipady = 1)
		tkgrid(cb.StatDay, row = 1, column = 3, sticky = 'we', rowspan = 1, columnspan = 5, padx = 1, pady = 1, ipadx = 1, ipady = 1)
		tkgrid(txt.StatYear, row = 2, column = 0, sticky = 'we', rowspan = 1, columnspan = 5, padx = 1, pady = 1, ipadx = 1, ipady = 1)
		tkgrid(tklabel(frameStats, width = 5), row = 3, column = 0, sticky = 'we', rowspan = 1, columnspan = 3, padx = 1, pady = 1, ipadx = 1, ipady = 1)
		tkgrid(cb.StatYear, row = 3, column = 3, sticky = 'we', rowspan = 1, columnspan = 5, padx = 1, pady = 1, ipadx = 1, ipady = 1)

		##############################################

		frameDryDay <- ttklabelframe(subfr2, text = "Wet/Dry Day definition", relief = 'groove')

		drywet.day <- tclVar(GeneralParameters$def$drywet.day)

		txt.DryDay1 <- tklabel(frameDryDay, text = 'Rainfall amount above/below', anchor = 'w', justify = 'left')
		en.DryDay <- tkentry(frameDryDay, textvariable = drywet.day, width = 5)
		txt.DryDay2 <- tklabel(frameDryDay, text = 'mm/day', anchor = 'w', justify = 'left')

		tkgrid(txt.DryDay1, en.DryDay, txt.DryDay2)

		##############################################

		frameDrySpell <- ttklabelframe(subfr2, text = "Wet/Dry Spell definition", relief = 'groove')

		drywet.spell <- tclVar(GeneralParameters$def$drywet.spell)

		txt.DrySpell1 <- tklabel(frameDrySpell, text = 'Defined as', anchor = 'w', justify = 'left')
		en.DrySpell <- tkentry(frameDrySpell, textvariable = drywet.spell, width = 2)
		txt.DrySpell2 <- tklabel(frameDrySpell, text = 'continuous wet/dry days', anchor = 'w', justify = 'left')

		tkgrid(txt.DrySpell1, en.DrySpell, txt.DrySpell2)

		##############################################

		frameProba <- tkframe(subfr2)

		INITIAL.VAL <- c(400, 10, 30, 30, 5, 5)
		UNIT.TXT <- c('mm', 'mm/day', 'days', 'days', 'spells', 'spells')

		txt.units.thres <- UNIT.TXT[DailyStatsVAL %in% GeneralParameters$stats$daily]

		proba.thres <- tclVar(GeneralParameters$def$proba.thres)
		units.thres <- tclVar(txt.units.thres)
		stateProba <- if(GeneralParameters$stats$yearly == 'proba') 'normal' else 'disabled'

		txt.Proba1 <- tklabel(frameProba, text = 'Probability of exceeding', anchor = 'w', justify = 'left')
		en.Proba <- tkentry(frameProba, textvariable = proba.thres, width = 4, state = stateProba)
		txt.Proba2 <- tklabel(frameProba, text = tclvalue(units.thres), textvariable = units.thres, anchor = 'w', justify = 'left')

		tkgrid(txt.Proba1, en.Proba, txt.Proba2)

		###################

		tkbind(cb.StatYear, "<<ComboboxSelected>>", function(){
			stateProba <- if(str_trim(tclvalue(yearly.Stats)) == CbYearlyStatsVAL[4]) 'normal' else 'disabled'
			tkconfigure(en.Proba, state = stateProba)

			if(str_trim(tclvalue(yearly.Stats)) == CbYearlyStatsVAL[4]){
				tclvalue(units.thres) <- UNIT.TXT[CbDailyStatsVAL %in% str_trim(tclvalue(daily.Stats))]
				tclvalue(proba.thres) <- INITIAL.VAL[CbDailyStatsVAL %in% str_trim(tclvalue(daily.Stats))]
			}
		})

		tkbind(cb.StatDay, "<<ComboboxSelected>>", function(){
			if(str_trim(tclvalue(yearly.Stats)) == CbYearlyStatsVAL[4]){
				tclvalue(units.thres) <- UNIT.TXT[CbDailyStatsVAL %in% str_trim(tclvalue(daily.Stats))]
				tclvalue(proba.thres) <- INITIAL.VAL[CbDailyStatsVAL %in% str_trim(tclvalue(daily.Stats))]
			}
		})

		##############################################

		frameCalc <- tkframe(subfr2)

		if(!is.null(.cdtData$EnvData$DirExist)){
			stateCaclBut <- if(tclvalue(.cdtData$EnvData$DirExist) == "1") "normal" else "disabled"
		}else stateCaclBut <- "normal"

		bt.CalcDaily <- ttkbutton(frameCalc, text = 'Calculate', state = stateCaclBut)

		tkconfigure(bt.CalcDaily, command = function(){
			GeneralParameters$data.type <- datatypeVAL[CbdatatypeVAL %in% str_trim(tclvalue(DataType))]

			if(str_trim(tclvalue(DataType)) == CbdatatypeVAL[1])
				GeneralParameters$cdtstation <- str_trim(tclvalue(input.Prec))

			if(str_trim(tclvalue(DataType)) == CbdatatypeVAL[2])
				GeneralParameters$cdtdataset <- str_trim(tclvalue(input.Prec))

			GeneralParameters$min.frac <- as.numeric(str_trim(tclvalue(min.frac)))
			GeneralParameters$output <- str_trim(tclvalue(dir.save))

			GeneralParameters$seas$all.years <- switch(tclvalue(allYears), '0' = FALSE, '1' = TRUE)
			GeneralParameters$seas$startYear <- as.numeric(str_trim(tclvalue(startYear)))
			GeneralParameters$seas$endYear <- as.numeric(str_trim(tclvalue(endYear)))
			GeneralParameters$seas$startMon <- which(MOIS %in% str_trim(tclvalue(startMon)))
			GeneralParameters$seas$startDay <- as.numeric(str_trim(tclvalue(tkget(spin.startDay))))
			GeneralParameters$seas$endMon <- which(MOIS %in% str_trim(tclvalue(endMon)))
			GeneralParameters$seas$endDay <- as.numeric(str_trim(tclvalue(tkget(spin.endDay))))

			GeneralParameters$stats$daily <- DailyStatsVAL[CbDailyStatsVAL %in% str_trim(tclvalue(daily.Stats))]
			GeneralParameters$stats$yearly <- YearlyStatsVAL[CbYearlyStatsVAL %in% str_trim(tclvalue(yearly.Stats))]

			GeneralParameters$def$drywet.day <- as.numeric(str_trim(tclvalue(drywet.day)))
			GeneralParameters$def$drywet.spell <- as.numeric(str_trim(tclvalue(drywet.spell)))
			GeneralParameters$def$proba.thres <- as.numeric(str_trim(tclvalue(proba.thres)))

			# assign('GeneralParameters', GeneralParameters, envir = .GlobalEnv)

			analysis.method <- paste(str_trim(tclvalue(daily.Stats)), ":", str_trim(tclvalue(yearly.Stats)))
			Insert.Messages.Out(paste("Calculating", analysis.method, "......."))

			tkconfigure(.cdtEnv$tcl$main$win, cursor = 'watch')
			tcl('update')
			ret <- tryCatch(
				{
					dailyRainAnalysisCalcProcs(GeneralParameters)
				},
				warning = function(w) warningFun(w),
				error = function(e) errorFun(e),
				finally = {
					tkconfigure(.cdtEnv$tcl$main$win, cursor = '')
					tcl('update')
				}
			)

			msg0 <- paste(analysis.method, "calculation finished successfully")
			msg1 <- paste(analysis.method, "calculation failed")

			if(!is.null(ret)){
				if(ret == 0){
					Insert.Messages.Out(msg0)

					.cdtData$EnvData$plot.maps$data.type <- .cdtData$EnvData$output$params$data.type
					.cdtData$EnvData$plot.maps[c('lon', 'lat', 'id')] <- .cdtData$EnvData$output$data[c('lon', 'lat', 'id')]
					###################
					set.Data.VarStat.Dates_1st()
					widgets.Station.Pixel()
					set.plot.type()
					res1 <- try(read.Data.MapVarStat(), silent = TRUE)
					if(inherits(res1, "try-error") | is.null(res1)) return(NULL)
					res2 <- try(read.Data.MapVarTS(), silent = TRUE)
					if(inherits(res2, "try-error") | is.null(res2)) return(NULL)
				}else Insert.Messages.Out(msg1, format = TRUE)
			}else Insert.Messages.Out(msg1, format = TRUE)
		})

		####################

		tkgrid(bt.CalcDaily, row = 0, column = 0, sticky = 'we', pady = 1)

		############################################

		tkgrid(frameStats, row = 0, column = 0, sticky = 'we', padx = 1, pady = 1, ipadx = 1, ipady = 1)
		tkgrid(frameDryDay, row = 1, column = 0, sticky = 'we', padx = 1, pady = 3, ipadx = 1, ipady = 2)
		tkgrid(frameDrySpell, row = 2, column = 0, sticky = 'we', padx = 1, pady = 3, ipadx = 1, ipady = 2)
		tkgrid(frameProba, row = 3, column = 0, sticky = 'we', padx = 1, pady = 3, ipadx = 1, ipady = 2)
		tkgrid(frameCalc, row = 4, column = 0, sticky = '', padx = 1, pady = 3, ipadx = 1, ipady = 1)

	#######################################################################################################

	#Tab3
	subfr3 <- bwTabScrollableFrame(cmd.tab3)

		##############################################

		frameDataExist <- ttklabelframe(subfr3, text = "Analysis data", relief = 'groove')

		.cdtData$EnvData$DirExist <- tclVar(0)
		file.dataIndex <- tclVar()

		stateExistData <- if(tclvalue(.cdtData$EnvData$DirExist) == "1") "normal" else "disabled"

		chk.dataIdx <- tkcheckbutton(frameDataExist, variable = .cdtData$EnvData$DirExist, text = "Analysis data already computed", anchor = 'w', justify = 'left')
		en.dataIdx <- tkentry(frameDataExist, textvariable = file.dataIndex, width = largeur2, state = stateExistData)
		bt.dataIdx <- tkbutton(frameDataExist, text = "...", state = stateExistData)

		tkconfigure(bt.dataIdx, command = function(){
			path.dataIdx <- tclvalue(tkgetOpenFile(initialdir = getwd(), filetypes = .cdtEnv$tcl$data$filetypes6))
			if(path.dataIdx %in% c("", "NA") | is.na(path.dataIdx)) return(NULL)
			tclvalue(file.dataIndex) <- path.dataIdx

			if(file.exists(str_trim(tclvalue(file.dataIndex)))){
				OutIndexdata <- try(readRDS(str_trim(tclvalue(file.dataIndex))), silent = TRUE)
				if(inherits(OutIndexdata, "try-error")){
					Insert.Messages.Out('Unable to load daily rainfall analysis data', format = TRUE)
					Insert.Messages.Out(gsub('[\r\n]', '', OutIndexdata[1]), format = TRUE)

					tkconfigure(cb.varstat.var, values = "")
					tclvalue(.cdtData$EnvData$anaVars) <- ""
					tkconfigure(cb.varstat.stat, values = "")
					tclvalue(.cdtData$EnvData$anaStat) <- ""
					tkconfigure(cb.data.Index, values = "")
					tclvalue(.cdtData$EnvData$donDate) <- ""
					return(NULL)
				}

				.cdtData$EnvData$output <- OutIndexdata
				.cdtData$EnvData$PathData <- dirname(str_trim(tclvalue(file.dataIndex)))
				.cdtData$EnvData$plot.maps$data.type <- .cdtData$EnvData$output$params$data.type
				.cdtData$EnvData$plot.maps[c('lon', 'lat', 'id')] <- .cdtData$EnvData$output$data[c('lon', 'lat', 'id')]
				###################
				set.Data.VarStat.Dates_1st()
				widgets.Station.Pixel()
				set.plot.type()
				ret1 <- try(read.Data.MapVarStat(), silent = TRUE)
				if(inherits(ret1, "try-error") | is.null(ret1)) return(NULL)
				ret2 <- try(read.Data.MapVarTS(), silent = TRUE)
				if(inherits(ret2, "try-error") | is.null(ret2)) return(NULL)
			}
		})

		tkgrid(chk.dataIdx, row = 0, column = 0, sticky = 'we', rowspan = 1, columnspan = 5, padx = 1, pady = 1, ipadx = 1, ipady = 1)
		tkgrid(en.dataIdx, row = 1, column = 0, sticky = 'we', rowspan = 1, columnspan = 4, padx = 1, pady = 1, ipadx = 1, ipady = 1)
		tkgrid(bt.dataIdx, row = 1, column = 4, sticky = 'w', rowspan = 1, columnspan = 1, padx = 0, pady = 1, ipadx = 1, ipady = 1)

		###############
		tkbind(chk.dataIdx, "<Button-1>", function(){
			stateExistData <- if(tclvalue(.cdtData$EnvData$DirExist) == '1') 'disabled' else 'normal'
			tkconfigure(en.dataIdx, state = stateExistData)
			tkconfigure(bt.dataIdx, state = stateExistData)
			stateCaclBut <- if(tclvalue(.cdtData$EnvData$DirExist) == '1') 'normal' else 'disabled'
			tkconfigure(bt.CalcDaily, state = stateCaclBut)
		})

		##############################################

		frameDataStatMap <- ttklabelframe(subfr3, text = "Statistics Maps", relief = 'groove')

		.cdtData$EnvData$anaVars <- tclVar()
		.cdtData$EnvData$anaStat <- tclVar()

		cb.varstat.var <- ttkcombobox(frameDataStatMap, values = "", textvariable = .cdtData$EnvData$anaVars, width = largeur4)
		bt.varstat.maps <- ttkbutton(frameDataStatMap, text = .cdtEnv$tcl$lang$global[['button']][['3']])
		cb.varstat.stat <- ttkcombobox(frameDataStatMap, values = "", textvariable = .cdtData$EnvData$anaStat, width = largeur4)
		bt.varstat.MapOpt <- ttkbutton(frameDataStatMap, text = .cdtEnv$tcl$lang$global[['button']][['4']])

		###################

		.cdtData$EnvData$tab$pointSize.MapStat <- NULL
		.cdtData$EnvData$varstatMapOp <- list(presetCol = list(color = 'tim.colors', reverse = FALSE),
											userCol = list(custom = FALSE, color = NULL),
											userLvl = list(custom = FALSE, levels = NULL, equidist = FALSE),
											title = list(user = FALSE, title = ''),
											colkeyLab = list(user = FALSE, label = ''),
											scalebar = list(add = FALSE, pos = 'bottomleft'),
											pointSize = .cdtData$EnvData$tab$pointSize.MapStat)

		tkconfigure(bt.varstat.MapOpt, command = function(){
			if(!is.null(.cdtData$EnvData$varData$map)){
				atlevel <- pretty(.cdtData$EnvData$varData$map$z, n = 10, min.n = 7)
				if(is.null(.cdtData$EnvData$varstatMapOp$userLvl$levels)){
					.cdtData$EnvData$varstatMapOp$userLvl$levels <- atlevel
				}else{
					if(!.cdtData$EnvData$varstatMapOp$userLvl$custom)
						.cdtData$EnvData$varstatMapOp$userLvl$levels <- atlevel
				}
			}
			.cdtData$EnvData$varstatMapOp <- MapGraph.MapOptions(.cdtData$EnvData$varstatMapOp)

			if(str_trim(tclvalue(.cdtData$EnvData$plot.maps$plot.type)) == "Points")
				.cdtData$EnvData$tab$pointSize.MapStat <- .cdtData$EnvData$varstatMapOp$pointSize
		})

		###################

		.cdtData$EnvData$tab$dataMapStat <- NULL

		tkconfigure(bt.varstat.maps, command = function(){
			if(str_trim(tclvalue(.cdtData$EnvData$anaVars)) != "" &
				str_trim(tclvalue(.cdtData$EnvData$anaStat)) != "")
			{
				ret <- try(read.Data.MapVarStat(), silent = TRUE)
				if(inherits(ret, "try-error") | is.null(ret)) return(NULL)
				dailyRainAnalysis.Display.MapsVarStats()
			}
		})

		###################

		tkgrid(cb.varstat.var, row = 0, column = 0, sticky = 'we', rowspan = 1, columnspan = 4, padx = 1, pady = 1, ipadx = 1, ipady = 1)
		tkgrid(bt.varstat.maps, row = 0, column = 4, sticky = '', rowspan = 1, columnspan = 2, padx = 1, pady = 1, ipadx = 1, ipady = 1)
		tkgrid(cb.varstat.stat, row = 1, column = 0, sticky = 'we', rowspan = 1, columnspan = 2, padx = 1, pady = 1, ipadx = 1, ipady = 1)
		tkgrid(bt.varstat.MapOpt, row = 1, column = 4, sticky = 'we', rowspan = 1, columnspan = 2, padx = 1, pady = 1, ipadx = 1, ipady = 1)

		###################

		tkbind(cb.varstat.var, "<<ComboboxSelected>>", function(){
			vars <- str_trim(tclvalue(.cdtData$EnvData$anaVars))
			if(vars == "") return(NULL)
			varstats <- .cdtData$EnvData$output$exist.vars.dates
			statsval <- varstats[[varsname$name[which(varsname$longname == vars)]]]

			STATSVAL <- statsname$longname[statsname$name %in% names(statsval)]
			if(length(STATSVAL) == 1) STATSVAL <- c(STATSVAL, "")
			tkconfigure(cb.varstat.stat, values = STATSVAL)
			tclvalue(.cdtData$EnvData$anaStat) <- STATSVAL[1]

			tkconfigure(cb.data.Index, values = statsval$date)
			tclvalue(.cdtData$EnvData$donDate) <- statsval$date[length(statsval$date)]
			return(0)
		})

		##############################################

		frameDataMap <- ttklabelframe(subfr3, text = "Seasonal Maps", relief = 'groove')

		.cdtData$EnvData$donDate <- tclVar()

		cb.data.Index <- ttkcombobox(frameDataMap, values = "", textvariable = .cdtData$EnvData$donDate, width = largeur5)
		bt.data.Index.prev <- ttkbutton(frameDataMap, text = "<<", width = 3)
		bt.data.Index.next <- ttkbutton(frameDataMap, text = ">>", width = 3)
		bt.data.maps <- ttkbutton(frameDataMap, text = .cdtEnv$tcl$lang$global[['button']][['3']], width = 7)
		bt.data.MapOpt <- ttkbutton(frameDataMap, text = .cdtEnv$tcl$lang$global[['button']][['4']], width = 7)

		###############

		.cdtData$EnvData$tab$pointSize.MapTS <- NULL
		.cdtData$EnvData$dataMapOp <- list(presetCol = list(color = 'tim.colors', reverse = FALSE),
										userCol = list(custom = FALSE, color = NULL),
										userLvl = list(custom = FALSE, levels = NULL, equidist = FALSE),
										title = list(user = FALSE, title = ''),
										colkeyLab = list(user = FALSE, label = ''),
										scalebar = list(add = FALSE, pos = 'bottomleft'),
										pointSize = .cdtData$EnvData$tab$pointSize.MapTS)

		tkconfigure(bt.data.MapOpt, command = function(){
			if(!is.null(.cdtData$EnvData$varData$map)){
				atlevel <- pretty(.cdtData$EnvData$varData$map$z, n = 10, min.n = 7)
				if(is.null(.cdtData$EnvData$dataMapOp$userLvl$levels)){
					.cdtData$EnvData$dataMapOp$userLvl$levels <- atlevel
				}else{
					if(!.cdtData$EnvData$dataMapOp$userLvl$custom)
						.cdtData$EnvData$dataMapOp$userLvl$levels <- atlevel
				}
			}
			.cdtData$EnvData$dataMapOp <- MapGraph.MapOptions(.cdtData$EnvData$dataMapOp)

			if(str_trim(tclvalue(.cdtData$EnvData$plot.maps$plot.type)) == "Points")
				.cdtData$EnvData$tab$pointSize.MapTS <- .cdtData$EnvData$dataMapOp$pointSize
		})

		###############

		.cdtData$EnvData$tab$dataMapTS <- NULL

		tkconfigure(bt.data.maps, command = function(){
			if(str_trim(tclvalue(.cdtData$EnvData$donDate)) != "" &
				!is.null(.cdtData$EnvData$tsData))
					dailyRainAnalysis.Display.MapVarTS()
		})

		tkconfigure(bt.data.Index.prev, command = function(){
			if(str_trim(tclvalue(.cdtData$EnvData$donDate)) != ""){
				vars <- str_trim(tclvalue(.cdtData$EnvData$anaVars))
				this.vars <- varsname$name[which(varsname$longname == vars)]
				donDates <- .cdtData$EnvData$output$exist.vars.dates[[this.vars]]$date
				idaty <- which(donDates == str_trim(tclvalue(.cdtData$EnvData$donDate)))
				idaty <- idaty - 1
				if(idaty < 1) idaty <- length(donDates)
				tclvalue(.cdtData$EnvData$donDate) <- donDates[idaty]

				ret <- try(read.Data.MapVarTS(), silent = TRUE)
				if(inherits(ret, "try-error") | is.null(ret)) return(NULL)

				dailyRainAnalysis.Display.MapVarTS()
			}
		})

		tkconfigure(bt.data.Index.next, command = function(){
			if(str_trim(tclvalue(.cdtData$EnvData$donDate)) != ""){
				vars <- str_trim(tclvalue(.cdtData$EnvData$anaVars))
				this.vars <- varsname$name[which(varsname$longname == vars)]
				donDates <- .cdtData$EnvData$output$exist.vars.dates[[this.vars]]$date
				idaty <- which(donDates == str_trim(tclvalue(.cdtData$EnvData$donDate)))
				idaty <- idaty + 1
				if(idaty > length(donDates)) idaty <- 1
				tclvalue(.cdtData$EnvData$donDate) <- donDates[idaty]

				ret <- try(read.Data.MapVarTS(), silent = TRUE)
				if(inherits(ret, "try-error") | is.null(ret)) return(NULL)

				dailyRainAnalysis.Display.MapVarTS()
			}
		})

		###############

		tkgrid(bt.data.Index.prev, row = 0, column = 0, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
		tkgrid(cb.data.Index, row = 0, column = 1, sticky = 'we', rowspan = 1, columnspan = 2, padx = 1, pady = 1, ipadx = 1, ipady = 1)
		tkgrid(bt.data.Index.next, row = 0, column = 3, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
		tkgrid(bt.data.maps, row = 0, column = 4, sticky = 'we', rowspan = 1, columnspan = 2, padx = 1, pady = 1, ipadx = 1, ipady = 1)
		tkgrid(bt.data.MapOpt, row = 1, column = 4, sticky = 'we', rowspan = 1, columnspan = 2, padx = 1, pady = 1, ipadx = 1, ipady = 1)

		###############

		tkbind(cb.data.Index, "<<ComboboxSelected>>", function(){
			if(!is.null(.cdtData$EnvData$tsData)){
				ret <- try(read.Data.MapVarTS(), silent = TRUE)
				if(inherits(ret, "try-error") | is.null(ret)) return(NULL)
			}
		})

		##############################################

		framePlotType <- tkframe(subfr3)

		.cdtData$EnvData$plot.maps$plot.type <- tclVar("Pixels")

		txt.plotType <- tklabel(framePlotType, text = "Plot Type", anchor = 'e', justify = 'right')
		cb.plotType <- ttkcombobox(framePlotType, values = "Pixels", textvariable = .cdtData$EnvData$plot.maps$plot.type, width = largeur5)

		tkgrid(txt.plotType, row = 0, column = 0, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
		tkgrid(cb.plotType, row = 0, column = 1, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)

		###############

		tkbind(cb.plotType, "<<ComboboxSelected>>", function(){
			if(str_trim(tclvalue(.cdtData$EnvData$anaVars)) != "" &
				str_trim(tclvalue(.cdtData$EnvData$anaStat)) != "")
			{
				ret1 <- try(read.Data.MapVarStat(), silent = TRUE)
				if(inherits(ret1, "try-error") | is.null(ret1)) return(NULL)
			}

			########
			if(!is.null(.cdtData$EnvData$tsData)){
				ret2 <- try(read.Data.MapVarTS(), silent = TRUE)
				if(inherits(ret2, "try-error") | is.null(ret2)) return(NULL)
			}
		})

		##############################################

		tkgrid(frameDataExist, row = 0, column = 0, sticky = 'we', padx = 1, pady = 1, ipadx = 1, ipady = 1)
		tkgrid(frameDataStatMap, row = 1, column = 0, sticky = 'we', padx = 1, pady = 3, ipadx = 1, ipady = 1)
		tkgrid(frameDataMap, row = 2, column = 0, sticky = 'we', padx = 1, pady = 1, ipadx = 1, ipady = 1)
		tkgrid(framePlotType, row = 3, column = 0, sticky = '', padx = 1, pady = 3, ipadx = 1, ipady = 1)

	#######################################################################################################

	#Tab4
	subfr4 <- bwTabScrollableFrame(cmd.tab4)

		##############################################

		frameDataTS <- ttklabelframe(subfr4, text = "Analysis Graph", relief = 'groove')

		typeTSPLOT <- c("Line", "Barplot", "Probability", "Anomaly")
		.cdtData$EnvData$plot.maps$typeTSp <- tclVar("Line")
		.cdtData$EnvData$plot.maps$averageTSp <- tclVar(FALSE)
		.cdtData$EnvData$plot.maps$tercileTSp <- tclVar(FALSE)
		.cdtData$EnvData$plot.maps$trendTSp <- tclVar(FALSE)

		stateType <- if(str_trim(tclvalue(.cdtData$EnvData$plot.maps$typeTSp)) == "Line") "normal" else "disabled"

		cb.typeTSp <- ttkcombobox(frameDataTS, values = typeTSPLOT, textvariable = .cdtData$EnvData$plot.maps$typeTSp, width = largeur5)
		bt.TsGraph.plot <- ttkbutton(frameDataTS, text = .cdtEnv$tcl$lang$global[['button']][['3']], width = 7)
		bt.TSGraphOpt <- ttkbutton(frameDataTS, text = .cdtEnv$tcl$lang$global[['button']][['4']], width = 8)

		frTS1 <- tkframe(frameDataTS)
		chk.meanTSp <- tkcheckbutton(frTS1, variable = .cdtData$EnvData$plot.maps$averageTSp, text = "Add Mean", anchor = 'w', justify = 'left', state = stateType)
		chk.tercTSp <- tkcheckbutton(frTS1, variable = .cdtData$EnvData$plot.maps$tercileTSp, text = "Add Terciles", anchor = 'w', justify = 'left', state = stateType)
		chk.trendTSp <- tkcheckbutton(frTS1, variable = .cdtData$EnvData$plot.maps$trendTSp, text = "Add Trend", anchor = 'w', justify = 'left', state = stateType)
		tkgrid(chk.meanTSp, chk.tercTSp, chk.trendTSp)

		#################

		.cdtData$EnvData$TSGraphOp <- list(
							anomaly = list(
									anom = list(perc.anom = FALSE, basePeriod = FALSE, startYr.anom = 1981, endYr.anom = 2010),
									xlim = list(is.min = FALSE, min = 1981, is.max = FALSE, max = 2017),
									ylim = list(is.min = FALSE, min = -100, is.max = FALSE, max = 100),
									axislabs = list(is.xlab = FALSE, xlab = '', is.ylab = FALSE, ylab = ''),
									title = list(is.title = FALSE, title = '', position = 'top'),
									colors = list(negative = "blue", positive = "red")
								),
							bar = list(
								xlim = list(is.min = FALSE, min = 1981, is.max = FALSE, max = 2017),
								ylim = list(is.min = FALSE, min = 0, is.max = FALSE, max = 100),
								axislabs = list(is.xlab = FALSE, xlab = '', is.ylab = FALSE, ylab = ''),
								title = list(is.title = FALSE, title = '', position = 'top'),
								colors = list(col = "darkblue")
							),
							line = list(
								xlim = list(is.min = FALSE, min = 1981, is.max = FALSE, max = 2017),
								ylim = list(is.min = FALSE, min = 0, is.max = FALSE, max = 100),
								axislabs = list(is.xlab = FALSE, xlab = '', is.ylab = FALSE, ylab = ''),
								title = list(is.title = FALSE, title = '', position = 'top'),
								plot = list(type = 'both',
									col = list(line = "red", points = "blue"),
									lwd = 2, cex = 1.4),
								legend = list(
									is = list(mean = FALSE, tercile = FALSE, linear = FALSE),
									add = list(mean = FALSE, tercile = FALSE, linear = FALSE),
									col = list(mean = "black", tercile1 = "green", tercile2 = "blue", linear = "purple3"),
									text = list(mean = "Average", tercile1 = "Tercile 0.33333", tercile2 = "Tercile 0.66666", linear = "Trend line"),
									lwd = list(mean = 2, tercile = 2, linear = 2))
							),
							proba = list(
								xlim = list(is.min = FALSE, min = 0, is.max = FALSE, max = 100),
								ylim = list(is.min = FALSE, min = 0, is.max = FALSE, max = 100),
								axislabs = list(is.xlab = FALSE, xlab = '', is.ylab = FALSE, ylab = ''),
								title = list(is.title = FALSE, title = '', position = 'top'),
								plot = list(type = 'both',
									col = list(line = "red", points = "blue"),
									lwd = 2, cex = 0.8),
								proba = list(theoretical = FALSE, col = 'black', lwd = 2)
							)
						)

		tkconfigure(bt.TSGraphOpt, command = function(){
			suffix.fun <- switch(tclvalue(.cdtData$EnvData$plot.maps$typeTSp),
									"Anomaly" = "Anomaly",
									"Barplot" = "Bar",
									"Line" = "Line",
									"Probability" = "Proba")
			plot.fun <- get(paste0("MapGraph.GraphOptions.", suffix.fun), mode = "function")
			.cdtData$EnvData$TSGraphOp <- plot.fun(.cdtData$EnvData$TSGraphOp)
		})

		#################

		.cdtData$EnvData$tab$dataGraph <- NULL

		tkconfigure(bt.TsGraph.plot, command = function(){
			if(!is.null(.cdtData$EnvData$tsData)){
				imgContainer <- CDT.Display.Graph(dailyRainAnalysis.plotVarGraph, .cdtData$EnvData$tab$dataGraph, 'Analysis-Graph')
				.cdtData$EnvData$tab$dataGraph <- imageNotebookTab_unik(imgContainer, .cdtData$EnvData$tab$dataGraph)
			}
		})

		#################

		tkgrid(cb.typeTSp, row = 0, column = 0, sticky = 'we', pady = 3, columnspan = 1)
		tkgrid(bt.TSGraphOpt, row = 0, column = 1, sticky = 'we', padx = 4, pady = 1, columnspan = 1)
		tkgrid(bt.TsGraph.plot, row = 0, column = 2, sticky = 'we', pady = 3, columnspan = 1)
		tkgrid(frTS1, row = 1, column = 0, sticky = 'we', pady = 3, columnspan = 3)

		#################

		tkbind(cb.typeTSp, "<<ComboboxSelected>>", function(){
			stateType <- if(str_trim(tclvalue(.cdtData$EnvData$plot.maps$typeTSp)) == "Line") "normal" else "disabled"
			tkconfigure(chk.meanTSp, state = stateType)
			tkconfigure(chk.tercTSp, state = stateType)
			tkconfigure(chk.trendTSp, state = stateType)
		})

		tkbind(chk.meanTSp, "<Button-1>", function(){
			.cdtData$EnvData$TSGraphOp$line$legend$add$mean <- 
						if(tclvalue(.cdtData$EnvData$plot.maps$averageTSp) == '0') TRUE else FALSE
			.cdtData$EnvData$TSGraphOp$line.enso$legend$add$mean <- 
						if(tclvalue(.cdtData$EnvData$plot.maps$averageTSp) == '0') TRUE else FALSE
		})

		tkbind(chk.tercTSp, "<Button-1>", function(){
			.cdtData$EnvData$TSGraphOp$line$legend$add$tercile <- 
						if(tclvalue(.cdtData$EnvData$plot.maps$tercileTSp) == '0') TRUE else FALSE
			.cdtData$EnvData$TSGraphOp$line.enso$legend$add$tercile <- 
						if(tclvalue(.cdtData$EnvData$plot.maps$tercileTSp) == '0') TRUE else FALSE
		})

		tkbind(chk.trendTSp, "<Button-1>", function(){
			.cdtData$EnvData$TSGraphOp$line$legend$add$linear <- 
						if(tclvalue(.cdtData$EnvData$plot.maps$trendTSp) == '0') TRUE else FALSE
			.cdtData$EnvData$TSGraphOp$line.enso$legend$add$linear <- 
						if(tclvalue(.cdtData$EnvData$plot.maps$trendTSp) == '0') TRUE else FALSE
		})

		##############################################

		frameSTNCrds <- ttklabelframe(subfr4, text = "Station/Coordinates", relief = 'groove')

		frTS2 <- tkframe(frameSTNCrds)
		.cdtData$EnvData$plot.maps$lonLOC <- tclVar()
		.cdtData$EnvData$plot.maps$latLOC <- tclVar()
		.cdtData$EnvData$plot.maps$stnIDTSp <- tclVar()

		tkgrid(frTS2, row = 0, column = 0, sticky = 'e', pady = 1)

		##############################################

		tkgrid(frameDataTS, row = 0, column = 0, sticky = 'we', pady = 1)
		tkgrid(frameSTNCrds, row = 1, column = 0, sticky = '', pady = 3)

	#######################################################################################################

	#Tab5
	subfr5 <- bwTabScrollableFrame(cmd.tab5)

		##############################################

		frameSHP <- ttklabelframe(subfr5, text = "Boundaries", relief = 'groove')

		.cdtData$EnvData$shp$add.shp <- tclVar(FALSE)
		file.plotShp <- tclVar()
		stateSHP <- "disabled"

		chk.addshp <- tkcheckbutton(frameSHP, variable = .cdtData$EnvData$shp$add.shp, text = "Add boundaries to Map", anchor = 'w', justify = 'left')
		bt.addshpOpt <- ttkbutton(frameSHP, text = .cdtEnv$tcl$lang$global[['button']][['4']], state = stateSHP)
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

		tkgrid(frameSHP, row = 0, column = 0, sticky = 'we', pady = 1)

	#######################################################################################################

	widgets.Station.Pixel <- function(){
		tkdestroy(frTS2)
		frTS2 <<- tkframe(frameSTNCrds)

		if(.cdtData$EnvData$output$params$data.type == "cdtstation"){
			stnIDTSPLOT <- .cdtData$EnvData$output$data$id
			txt.stnSel <- tklabel(frTS2, text = "Select a station to plot")
			bt.stnID.prev <- ttkbutton(frTS2, text = "<<", width = 6)
			bt.stnID.next <- ttkbutton(frTS2, text = ">>", width = 6)
			cb.stnID <- ttkcombobox(frTS2, values = stnIDTSPLOT, textvariable = .cdtData$EnvData$plot.maps$stnIDTSp, width = largeur6)
			tclvalue(.cdtData$EnvData$plot.maps$stnIDTSp) <- stnIDTSPLOT[1]

			tkconfigure(bt.stnID.prev, command = function(){
				if(!is.null(.cdtData$EnvData$tsData)){
					istn <- which(stnIDTSPLOT == str_trim(tclvalue(.cdtData$EnvData$plot.maps$stnIDTSp)))
					istn <- istn - 1
					if(istn < 1) istn <- length(stnIDTSPLOT)
					tclvalue(.cdtData$EnvData$plot.maps$stnIDTSp) <- stnIDTSPLOT[istn]

					imgContainer <- CDT.Display.Graph(dailyRainAnalysis.plotVarGraph, .cdtData$EnvData$tab$dataGraph, 'Analysis-Graph')
					.cdtData$EnvData$tab$dataGraph <- imageNotebookTab_unik(imgContainer, .cdtData$EnvData$tab$dataGraph)
				}
			})

			tkconfigure(bt.stnID.next, command = function(){
				if(!is.null(.cdtData$EnvData$tsData)){
					istn <- which(stnIDTSPLOT == str_trim(tclvalue(.cdtData$EnvData$plot.maps$stnIDTSp)))
					istn <- istn + 1
					if(istn > length(stnIDTSPLOT)) istn <- 1
					tclvalue(.cdtData$EnvData$plot.maps$stnIDTSp) <- stnIDTSPLOT[istn]

					imgContainer <- CDT.Display.Graph(dailyRainAnalysis.plotVarGraph, .cdtData$EnvData$tab$dataGraph, 'Analysis-Graph')
					.cdtData$EnvData$tab$dataGraph <- imageNotebookTab_unik(imgContainer, .cdtData$EnvData$tab$dataGraph)
				}
			})

			tkgrid(txt.stnSel, row = 0, column = 0, sticky = '', rowspan = 1, columnspan = 3, padx = 1, pady = 1, ipadx = 1, ipady = 1)
			tkgrid(bt.stnID.prev, row = 1, column = 0, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
			tkgrid(cb.stnID, row = 1, column = 1, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
			tkgrid(bt.stnID.next, row = 1, column = 2, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
		}else{
			txt.crdSel <- tklabel(frTS2, text = "Enter longitude and latitude to plot", anchor = 'w', justify = 'left')
			txt.lonLoc <- tklabel(frTS2, text = "Longitude", anchor = 'e', justify = 'right')
			en.lonLoc <- tkentry(frTS2, textvariable = .cdtData$EnvData$plot.maps$lonLOC, width = 8)
			txt.latLoc <- tklabel(frTS2, text = "Latitude", anchor = 'e', justify = 'right')
			en.latLoc <- tkentry(frTS2, textvariable = .cdtData$EnvData$plot.maps$latLOC, width = 8)
			stnIDTSPLOT <- ""
			tclvalue(.cdtData$EnvData$plot.maps$stnIDTSp) <- ""

			tkgrid(txt.crdSel, row = 0, column = 0, sticky = 'we', rowspan = 1, columnspan = 4, padx = 1, pady = 1, ipadx = 1, ipady = 1)
			tkgrid(txt.lonLoc, row = 1, column = 0, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
			tkgrid(en.lonLoc, row = 1, column = 1, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
			tkgrid(txt.latLoc, row = 1, column = 2, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
			tkgrid(en.latLoc, row = 1, column = 3, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
		}

		tkgrid(frTS2, row = 0, column = 0, sticky = 'e', pady = 1)
		return(0)
	}

	###################

	set.plot.type <- function(){
		if(.cdtData$EnvData$output$params$data.type == "cdtstation")
		{
			plot.type <- c("Pixels", "Points")
			.cdtData$EnvData$plot.maps$.data.type <- "Points"

			.cdtData$EnvData$varstatMapOp$pointSize <- 0.7
			.cdtData$EnvData$dataMapOp$pointSize <- 0.7
		}else{
			plot.type <- c("Pixels", "FilledContour")
			.cdtData$EnvData$plot.maps$.data.type <- "Grid"
		}
		tkconfigure(cb.plotType, values = plot.type)
	}

	###################

	set.Data.VarStat.Dates_1st <- function(){
		varstats <- .cdtData$EnvData$output$exist.vars.dates
		if(length(names(varstats)) == 0) return(NULL)

		VARSVAL <- varsname$longname[varsname$name %in% names(varstats)]
		if(length(VARSVAL) == 1) VARSVAL <- c(VARSVAL, "")
		tkconfigure(cb.varstat.var, values = VARSVAL)
		last.vars <- varsname$longname[which(varsname$name == .cdtData$EnvData$output$last[1])]
		tclvalue(.cdtData$EnvData$anaVars) <- last.vars

		statsval <- varstats[[.cdtData$EnvData$output$last[1]]]
		STATSVAL <- statsname$longname[statsname$name %in% names(statsval)]
		if(length(STATSVAL) == 1) STATSVAL <- c(STATSVAL, "")
		tkconfigure(cb.varstat.stat, values = STATSVAL)
		last.stats <- statsname$longname[which(statsname$name == .cdtData$EnvData$output$last[2])]
		tclvalue(.cdtData$EnvData$anaStat) <- last.stats

		tkconfigure(cb.data.Index, values = statsval$date)
		tclvalue(.cdtData$EnvData$donDate) <- statsval$date[length(statsval$date)]

		return(0)
	}

	#######################################################################################################

	read.Data.MapVarStat <- function(){
		tkconfigure(.cdtEnv$tcl$main$win, cursor = 'watch')
		tcl('update')
		on.exit({
			tkconfigure(.cdtEnv$tcl$main$win, cursor = '')
			tcl('update')
		})

		vars <- str_trim(tclvalue(.cdtData$EnvData$anaVars))
		stats <- str_trim(tclvalue(.cdtData$EnvData$anaStat))
		this.vars <- varsname$name[which(varsname$longname == vars)]
		this.stats <- statsname$name[which(statsname$longname == stats)]
		
		if(vars == "" | stats == "") return(NULL)

		if(.cdtData$EnvData$output$params$data.type == "cdtstation"){
			filePathData <- file.path(.cdtData$EnvData$PathData, "CDTDATASET", paste0(this.vars, "_", this.stats, ".rds"))
			if(!file.exists(filePathData)){
				Insert.Messages.Out(paste(filePathData, 'not found'), format = TRUE)
				return(NULL)
			}

			change.plot <- str_trim(tclvalue(.cdtData$EnvData$plot.maps$plot.type))

			readVarData <- TRUE
			if(!is.null(.cdtData$EnvData$statData))
				if(!is.null(.cdtData$EnvData$statData$filePathData))
					if(.cdtData$EnvData$statData$filePathData == filePathData) readVarData <- FALSE

			if(!readVarData)
				if(.cdtData$EnvData$change.plot.VarData != change.plot) readVarData <- TRUE

			if(readVarData){
				.cdtData$EnvData$statData$data <- readRDS(filePathData)

				X0 <- .cdtData$EnvData$output$data$lon
				Y0 <- .cdtData$EnvData$output$data$lat
				VAR0 <- .cdtData$EnvData$statData$data
				if(change.plot == "Pixels"){
					nx <- nx_ny_as.image(diff(range(X0)))
					ny <- nx_ny_as.image(diff(range(Y0)))
					tmp <- cdt.as.image(VAR0, nx = nx, ny = ny, pts.xy = cbind(X0, Y0))
					.cdtData$EnvData$statData$map$x <- tmp$x
					.cdtData$EnvData$statData$map$y <- tmp$y
					.cdtData$EnvData$statData$map$z <- tmp$z
					rm(tmp)
				}

				if(change.plot == "Points"){
					.cdtData$EnvData$statData$map$x <- X0
					.cdtData$EnvData$statData$map$y <- Y0
					.cdtData$EnvData$statData$map$z <- VAR0
				}

				.cdtData$EnvData$statData$filePathData <- filePathData
				.cdtData$EnvData$change.plot.VarData <- change.plot
			}
		}else{
			filePathData <- file.path(.cdtData$EnvData$PathData, "DATA_NetCDF_STATS", paste0(this.vars, "_", this.stats, ".nc"))
			if(!file.exists(filePathData)){
				Insert.Messages.Out(paste(filePathData, 'not found'), format = TRUE)
				return(NULL)
			}

			readVarData <- TRUE
			if(!is.null(.cdtData$EnvData$statData))
				if(!is.null(.cdtData$EnvData$statData$filePathData))
					if(.cdtData$EnvData$statData$filePathData == filePathData) readVarData <- FALSE

			if(readVarData){
				nc <- nc_open(filePathData)
				.cdtData$EnvData$statData$map$x <- nc$dim[[1]]$vals
				.cdtData$EnvData$statData$map$y <- nc$dim[[2]]$vals
				.cdtData$EnvData$statData$map$z <- ncvar_get(nc, varid = nc$var[[1]]$name)
				nc_close(nc)
				.cdtData$EnvData$statData$filePathData <- filePathData
			}
		}

		.cdtData$EnvData$now$this.vars <- this.vars
		.cdtData$EnvData$now$this.stats <- this.stats

		return(0)
	}

	###################

	read.Data.MapVarTS <- function(){
		tkconfigure(.cdtEnv$tcl$main$win, cursor = 'watch')
		tcl('update')
		on.exit({
			tkconfigure(.cdtEnv$tcl$main$win, cursor = '')
			tcl('update')
		})

		vars <- str_trim(tclvalue(.cdtData$EnvData$anaVars))
		this.vars <- varsname$name[which(varsname$longname == vars)]
		this.daty <- str_trim(tclvalue(.cdtData$EnvData$donDate))

		if(vars == "" | this.daty == "") return(NULL)

		if(.cdtData$EnvData$output$params$data.type == "cdtstation"){
			filePathData <- file.path(.cdtData$EnvData$PathData, this.vars, paste0(this.vars, ".rds"))
			if(!file.exists(filePathData)){
				Insert.Messages.Out(paste(filePathData, 'not found'), format = TRUE)
				return(NULL)
			}

			change.plot <- str_trim(tclvalue(.cdtData$EnvData$plot.maps$plot.type))

			########
			readVarData <- TRUE
			if(!is.null(.cdtData$EnvData$tsData))
				if(!is.null(.cdtData$EnvData$tsData$filePathData))
					if(.cdtData$EnvData$tsData$filePathData == filePathData) readVarData <- FALSE

			if(readVarData){
				.cdtData$EnvData$tsData$data <- readRDS(filePathData)
				.cdtData$EnvData$tsData$filePathData <- filePathData
			}

			########
			rasterVarData <- TRUE
			if(!rasterVarData)
				if(!is.null(.cdtData$EnvData$tsData$rasterDate))
					if(.cdtData$EnvData$tsData$filePathData == filePathData)
						if(.cdtData$EnvData$tsData$rasterDate == this.daty) rasterVarData <- FALSE

			if(!rasterVarData)
				if(.cdtData$EnvData$change.plot.rasterVarData != change.plot) rasterVarData <- TRUE

			if(rasterVarData){
				idt <- which(.cdtData$EnvData$output$exist.vars.dates[[this.vars]]$date == this.daty)

				X0 <- .cdtData$EnvData$output$data$lon
				Y0 <- .cdtData$EnvData$output$data$lat
				VAR0 <- as.numeric(.cdtData$EnvData$tsData$data[idt, ])
				if(change.plot == "Pixels"){
					nx <- nx_ny_as.image(diff(range(X0)))
					ny <- nx_ny_as.image(diff(range(Y0)))
					tmp <- cdt.as.image(VAR0, nx = nx, ny = ny, pts.xy = cbind(X0, Y0))
					.cdtData$EnvData$tsData$map$x <- tmp$x
					.cdtData$EnvData$tsData$map$y <- tmp$y
					.cdtData$EnvData$tsData$map$z <- tmp$z
					rm(tmp)
				}

				if(change.plot == "Points"){
					.cdtData$EnvData$tsData$map$x <- X0
					.cdtData$EnvData$tsData$map$y <- Y0
					.cdtData$EnvData$tsData$map$z <- VAR0
				}

				.cdtData$EnvData$tsData$rasterDate <- this.daty
				.cdtData$EnvData$change.plot.rasterVarData <- change.plot
			}
		}else{
			filePathData <- file.path(.cdtData$EnvData$PathData, this.vars, "DATA_NetCDF", paste0("Seas_", this.daty, ".nc"))
			if(!file.exists(filePathData)){
				Insert.Messages.Out(paste(filePathData, 'not found'), format = TRUE)
				return(NULL)
			}

			readVarData <- TRUE
			if(!is.null(.cdtData$EnvData$tsData))
				if(!is.null(.cdtData$EnvData$tsData$filePathData))
					if(.cdtData$EnvData$tsData$filePathData == filePathData) readVarData <- FALSE

			if(readVarData){
				nc <- nc_open(filePathData)
				.cdtData$EnvData$tsData$map$x <- nc$dim[[1]]$vals
				.cdtData$EnvData$tsData$map$y <- nc$dim[[2]]$vals
				.cdtData$EnvData$tsData$map$z <- ncvar_get(nc, varid = nc$var[[1]]$name)
				nc_close(nc)
				.cdtData$EnvData$tsData$filePathData <- filePathData
			}

			###################

			file.CDT.Idx <- file.path(.cdtData$EnvData$PathData, this.vars, paste0(this.vars, ".rds"))

			read.cdt.dataIdx<- TRUE
			if(!is.null(.cdtData$EnvData$cdtdataset))
				if(!is.null(.cdtData$EnvData$file.CDT.Idx))
					if(.cdtData$EnvData$file.CDT.Idx == file.CDT.Idx) read.cdt.dataIdx <- FALSE

			if(read.cdt.dataIdx){
				.cdtData$EnvData$cdtdataset <- readRDS(file.CDT.Idx)
				.cdtData$EnvData$cdtdataset$fileInfo <- file.CDT.Idx
				.cdtData$EnvData$file.CDT.Idx <- file.CDT.Idx
			}
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
