
AggregateTS_GetInfo <- function(){
	listOpenFiles <- openFile_ttkcomboList()
	if(WindowsOS()){
		largeur <- 47
		largeur1 <- 44
		largeur2 <- 34
		wtkcombo <- 20
	}else{
		largeur <- 33
		largeur1 <- 32
		largeur2 <- 24
		wtkcombo <- 16
	}

	xml.dlg <- file.path(.cdtDir$dirLocal, "languages", "cdtAggregateTS_dlgBox.xml")
	lang.dlg <- cdtLanguageParse(xml.dlg, .cdtData$Config$lang.iso)

	###################

	tt <- tktoplevel()
	tkgrab.set(tt)
	tkfocus(tt)

	frMRG0 <- tkframe(tt, relief = 'raised', borderwidth = 2)
	frMRG1 <- tkframe(tt)
	frAGGRTS <- tkframe(frMRG0, relief = "sunken", borderwidth = 2)

	############################################

	frConvTS <- ttklabelframe(frAGGRTS, text = lang.dlg[['label']][['1']], labelanchor = "nw", relief = "groove", borderwidth = 2)

	OriginData <- tclVar()
	Cbperiod0VAL <- .cdtEnv$tcl$lang$global[['combobox']][['1']][2:5]
	period0VAL <- c('daily', 'pentad', 'dekadal', 'monthly')
	tclvalue(OriginData) <- Cbperiod0VAL[period0VAL %in% .cdtData$GalParams$in.tstep]

	ConvertData <- tclVar()
	Cbperiod1VAL <- .cdtEnv$tcl$lang$global[['combobox']][['1']][3:8]
	period1VAL <- c('pentad', 'dekadal', 'monthly', 'annual', 'seasonal', 'roll.seas')
	tclvalue(ConvertData) <- Cbperiod1VAL[period1VAL %in% .cdtData$GalParams$out.tstep]

	start.mon <- tclVar(.cdtData$GalParams$Seasonal$start.mon)
	length.mon <- tclVar(.cdtData$GalParams$Seasonal$length.mon)

	state.enSeasS <- if(tclvalue(ConvertData) == Cbperiod1VAL[5]) "normal" else "disabled"
	state.enSeasL <- if(tclvalue(ConvertData) %in% Cbperiod1VAL[5:6]) "normal" else "disabled"

	cb.intstep <- ttkcombobox(frConvTS, values = Cbperiod0VAL, textvariable = OriginData, width = wtkcombo)
	cb.outstep <- ttkcombobox(frConvTS, values = Cbperiod1VAL, textvariable = ConvertData, width = wtkcombo)
	txt.convTs <- tklabel(frConvTS, text = lang.dlg[['label']][['2']])
	txt.seasS <- tklabel(frConvTS, text = lang.dlg[['label']][['3']], anchor = 'e', justify = 'right')
	en.seasS <- tkentry(frConvTS, textvariable = start.mon, width = 3, state = state.enSeasS)
	txt.seasL <- tklabel(frConvTS, text = lang.dlg[['label']][['4']])
	en.seasL <- tkentry(frConvTS, textvariable = length.mon, width = 3, state = state.enSeasL)

	tkgrid(cb.intstep, row = 0, column = 0, sticky = 'we', rowspan = 1, columnspan = 8, padx = 1, pady = 1)
	tkgrid(txt.convTs, row = 0, column = 8, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1)
	tkgrid(cb.outstep, row = 0, column = 9, sticky = 'we', rowspan = 1, columnspan = 8, padx = 1, pady = 1)
	tkgrid(txt.seasS, row = 1, column = 5, sticky = 'we', rowspan = 1, columnspan = 6, padx = 1, pady = 1)
	tkgrid(en.seasS, row = 1, column = 11, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1)
	tkgrid(txt.seasL, row = 1, column = 12, sticky = 'we', rowspan = 1, columnspan = 4, padx = 1, pady = 1)
	tkgrid(en.seasL, row = 1, column = 16, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1)

	helpWidget(cb.intstep, lang.dlg[['tooltip']][['1']], lang.dlg[['status']][['1']])
	helpWidget(cb.outstep, lang.dlg[['tooltip']][['2']], lang.dlg[['status']][['2']])
	helpWidget(en.seasS, lang.dlg[['tooltip']][['3']], lang.dlg[['status']][['3']])
	helpWidget(en.seasL, lang.dlg[['tooltip']][['4']], lang.dlg[['status']][['4']])

	##############
	tkbind(cb.intstep, "<<ComboboxSelected>>", function(){
		if(tclvalue(OriginData) == Cbperiod0VAL[1]){
			tkconfigure(cb.outstep, values = Cbperiod1VAL)
			if(tclvalue(ConvertData) == Cbperiod0VAL[1]) tclvalue(ConvertData) <- Cbperiod1VAL[1]
		}else if(tclvalue(OriginData) == Cbperiod0VAL[2]){
			tkconfigure(cb.outstep, values = Cbperiod1VAL[-1])
			if(tclvalue(ConvertData) %in% Cbperiod0VAL[1:2]) tclvalue(ConvertData) <- Cbperiod1VAL[2]
		}else if(tclvalue(OriginData) == Cbperiod0VAL[3]){
			tkconfigure(cb.outstep, values = Cbperiod1VAL[-(1:2)])
			if(tclvalue(ConvertData) %in% Cbperiod0VAL[1:3]) tclvalue(ConvertData) <- Cbperiod1VAL[3]
		}else if(tclvalue(OriginData) == Cbperiod0VAL[4]){
			tkconfigure(cb.outstep, values = Cbperiod1VAL[-(1:3)])
			if(tclvalue(ConvertData) %in% Cbperiod0VAL[1:4]) tclvalue(ConvertData) <- Cbperiod1VAL[4]
		}

		state.enSeasS <- if(tclvalue(ConvertData) == Cbperiod1VAL[5]) "normal" else "disabled"
		state.enSeasL <- if(tclvalue(ConvertData) %in% Cbperiod1VAL[5:6]) "normal" else "disabled"
		tkconfigure(en.seasS, state = state.enSeasS)
		tkconfigure(en.seasL, state = state.enSeasL)

		AGGRFUN <- c("mean", "sum", "max", "min")
		if(tclvalue(OriginData) == Cbperiod0VAL[1]) AGGRFUN <- c("mean", "sum", "max", "min", "count")
		tkconfigure(cb.aggfun, values = AGGRFUN)
		if(tclvalue(aggr.fun) == "count") tclvalue(aggr.fun) <- "sum"
		stateCount <- if(tclvalue(aggr.fun) == "count") "normal" else "disabled"
		tkconfigure(cb.opfun, state = stateCount)
		tkconfigure(en.opthres, state = stateCount)
	})

	tkbind(cb.outstep, "<<ComboboxSelected>>", function(){
		state.enSeasS <- if(tclvalue(ConvertData) == Cbperiod1VAL[5]) "normal" else "disabled"
		state.enSeasL <- if(tclvalue(ConvertData) %in% Cbperiod1VAL[5:6]) "normal" else "disabled"
		tkconfigure(en.seasS, state = state.enSeasS)
		tkconfigure(en.seasL, state = state.enSeasL)
	})

	############################################

	frDataType <- ttklabelframe(frAGGRTS, text = lang.dlg[['label']][['5']], labelanchor = "nw", relief = "groove", borderwidth = 2)

	DataType <- tclVar()
	CbdatatypeVAL <- .cdtEnv$tcl$lang$global[['combobox']][['2']][1:3]
	datatypeVAL <- c('cdtstation', 'cdtdataset', 'cdtnetcdf')
	tclvalue(DataType) <- CbdatatypeVAL[datatypeVAL %in% .cdtData$GalParams$data.type]

	if(.cdtData$GalParams$data.type == 'cdtstation'){
		file.stnfl <- tclVar(.cdtData$GalParams$cdtstation)
		txtFileDir <- lang.dlg[['label']][['6']]
		stateSetData <- "disabled"
	}else if(.cdtData$GalParams$data.type == 'cdtdataset'){
		file.stnfl <- tclVar(.cdtData$GalParams$cdtdataset)
		txtFileDir <- lang.dlg[['label']][['7']]
		stateSetData <- "disabled"
	}else{
		file.stnfl <- tclVar(.cdtData$GalParams$cdtnetcdf$dir)
		txtFileDir <- lang.dlg[['label']][['8']]
		stateSetData <- "normal"
	}
	fileINdir <- tclVar(txtFileDir)

	##############
	cb.datatype <- ttkcombobox(frDataType, values = CbdatatypeVAL, textvariable = DataType, width = largeur2)
	set.datatype <- ttkbutton(frDataType, text = lang.dlg[['button']][['1']], state = stateSetData)

	txt.stnfl <- tklabel(frDataType, text = tclvalue(fileINdir), textvariable = fileINdir, anchor = 'w', justify = 'left')
	if(.cdtData$GalParams$data.type == 'cdtstation'){
		cb.stnfl <- ttkcombobox(frDataType, values = unlist(listOpenFiles), textvariable = file.stnfl, width = largeur1)
	}else{
		cb.stnfl <- tkentry(frDataType, textvariable = file.stnfl, width = largeur)
	}
	bt.stnfl <- tkbutton(frDataType, text = "...")

	##############

	settingdone <- .cdtData$GalParams$settingdone
	tkconfigure(set.datatype, command = function(){
		AggregateTS_ncdfData(tt, str_trim(tclvalue(file.stnfl)), tclvalue(OriginData))
		settingdone <<- 1
	})

	tkconfigure(bt.stnfl, command = function(){
		if(.cdtData$GalParams$data.type == 'cdtstation'){
			dat.opfiles <- getOpenFiles(tt)
			if(!is.null(dat.opfiles)){
				update.OpenFiles('ascii', dat.opfiles)
				listOpenFiles[[length(listOpenFiles) + 1]] <<- dat.opfiles[[1]]
				tclvalue(file.stnfl) <- dat.opfiles[[1]]
				tkconfigure(cb.stnfl, values = unlist(listOpenFiles))
			}
		}else if(.cdtData$GalParams$data.type == 'cdtdataset'){
			path.rds <- tclvalue(tkgetOpenFile(filetypes = .cdtEnv$tcl$data$filetypes6))
			tclvalue(file.stnfl) <- if(path.rds %in% c("", "NA") | is.na(path.rds)) "" else path.rds
		}else{
			dirnc <- tk_choose.dir(getwd(), "")
			tclvalue(file.stnfl) <- if(dirnc %in% c("", "NA") | is.na(dirnc)) "" else dirnc
		}
	})

	##############
	tkgrid(cb.datatype, row = 0, column = 0, sticky = 'we', rowspan = 1, columnspan = 3, padx = 1, pady = 1, ipadx = 1, ipady = 1)
	tkgrid(set.datatype, row = 0, column = 3, sticky = 'we', rowspan = 1, columnspan = 2, padx = 1, pady = 1, ipadx = 1, ipady = 1)
	tkgrid(txt.stnfl, row = 1, column = 0, sticky = 'we', rowspan = 1, columnspan = 5, padx = 1, pady = 1, ipadx = 1, ipady = 1)
	tkgrid(cb.stnfl, row = 2, column = 0, sticky = 'we', rowspan = 1, columnspan = 4, padx = 0, pady = 1, ipadx = 1, ipady = 1)
	tkgrid(bt.stnfl, row = 2, column = 4, sticky = 'w', rowspan = 1, columnspan = 1, padx = 0, pady = 1, ipadx = 1, ipady = 1)

	##############
	if(.cdtData$GalParams$data.type == 'cdtstation'){
		helpWidget(cb.stnfl, lang.dlg[['tooltip']][['5']], lang.dlg[['status']][['5']])
		helpWidget(bt.stnfl, lang.dlg[['tooltip']][['8']], lang.dlg[['status']][['8']])
	}else if(.cdtData$GalParams$data.type == 'cdtdataset'){
		helpWidget(cb.stnfl, lang.dlg[['tooltip']][['6']], lang.dlg[['status']][['6']])
		helpWidget(bt.stnfl, lang.dlg[['tooltip']][['9']], lang.dlg[['status']][['9']])
	}else{
		helpWidget(cb.stnfl, lang.dlg[['tooltip']][['7']], lang.dlg[['status']][['7']])
		helpWidget(bt.stnfl, lang.dlg[['tooltip']][['9']], lang.dlg[['status']][['9']])
	}

	##############

	tkbind(cb.datatype, "<<ComboboxSelected>>", function(){
		tkdestroy(cb.stnfl)
		tclvalue(file.stnfl) <- ''

		####
		if(str_trim(tclvalue(DataType)) == CbdatatypeVAL[1]){
			tclvalue(fileINdir) <- lang.dlg[['label']][['6']]
			tclvalue(fileORdir) <- lang.dlg[['label']][['9']]

			cb.stnfl <- ttkcombobox(frDataType, values = unlist(listOpenFiles), textvariable = file.stnfl, width = largeur1)

			#######
			tkconfigure(bt.stnfl, command = function(){
				dat.opfiles <- getOpenFiles(tt)
				if(!is.null(dat.opfiles)){
					update.OpenFiles('ascii', dat.opfiles)
					listOpenFiles[[length(listOpenFiles) + 1]] <<- dat.opfiles[[1]]
					tclvalue(file.stnfl) <- dat.opfiles[[1]]
					tkconfigure(cb.stnfl, values = unlist(listOpenFiles))
				}
			})
			tkconfigure(bt.file.save, command = function() fileORdir2Save(file.save, isFile = TRUE))
			tkconfigure(set.datatype, state = 'disabled')

			#######
			helpWidget(cb.stnfl, lang.dlg[['tooltip']][['5']], lang.dlg[['status']][['5']])
			helpWidget(bt.stnfl, lang.dlg[['tooltip']][['8']], lang.dlg[['status']][['8']])
			helpWidget(en.file.save, lang.dlg[['tooltip']][['10']], lang.dlg[['status']][['10']])
		}

		####
		if(str_trim(tclvalue(DataType)) == CbdatatypeVAL[2]){
			tclvalue(fileINdir) <- lang.dlg[['label']][['7']]
			tclvalue(fileORdir) <- lang.dlg[['label']][['10']]

			cb.stnfl <- tkentry(frDataType, textvariable = file.stnfl, width = largeur)

			#######
			tkconfigure(bt.stnfl, command = function(){
				path.rds <- tclvalue(tkgetOpenFile(filetypes = .cdtEnv$tcl$data$filetypes6))
				tclvalue(file.stnfl) <- if(path.rds %in% c("", "NA") | is.na(path.rds)) "" else path.rds
			})

			tkconfigure(bt.file.save, command = function() fileORdir2Save(file.save, isFile = FALSE))
			tkconfigure(set.datatype, state = 'disabled')

			#######
			helpWidget(cb.stnfl, lang.dlg[['tooltip']][['6']], lang.dlg[['status']][['6']])
			helpWidget(bt.stnfl, lang.dlg[['tooltip']][['9']], lang.dlg[['status']][['9']])
			helpWidget(en.file.save, lang.dlg[['tooltip']][['11']], lang.dlg[['status']][['11']])
		}

		####
		if(str_trim(tclvalue(DataType)) == CbdatatypeVAL[3]){
			tclvalue(fileINdir) <- lang.dlg[['label']][['8']]
			tclvalue(fileORdir) <- lang.dlg[['label']][['10']]

			cb.stnfl <- tkentry(frDataType, textvariable = file.stnfl, width = largeur)

			#######
			tkconfigure(bt.stnfl, command = function(){
				dirnc <- tk_choose.dir(getwd(), "")
				tclvalue(file.stnfl) <- if(dirnc %in% c("", "NA") | is.na(dirnc)) "" else dirnc
			})

			tkconfigure(set.datatype, command = function(){
				AggregateTS_ncdfData(tt, str_trim(tclvalue(file.stnfl)), tclvalue(OriginData))
				settingdone <<- 1
			})

			tkconfigure(bt.file.save, command = function() fileORdir2Save(file.save, isFile = FALSE))
			tkconfigure(set.datatype, state = 'normal')

			#######
			helpWidget(cb.stnfl, lang.dlg[['tooltip']][['7']], lang.dlg[['status']][['7']])
			helpWidget(bt.stnfl, lang.dlg[['tooltip']][['9']], lang.dlg[['status']][['9']])
			helpWidget(en.file.save, lang.dlg[['tooltip']][['11']], lang.dlg[['status']][['11']])
		}
		
		#######
		tkgrid(cb.stnfl, row = 2, column = 0, sticky = 'we', rowspan = 1, columnspan = 4, padx = 0, pady = 1, ipadx = 1, ipady = 1)
		tkfocus(tt)
	})

	############################################
	frameAggr <- ttklabelframe(frAGGRTS, text = lang.dlg[['label']][['11']], labelanchor = "nw", relief = "groove", borderwidth = 2)

	aggr.fun <- tclVar(.cdtData$GalParams$aggr.series$aggr.fun)
	min.frac <- tclVar(.cdtData$GalParams$aggr.series$min.frac)
	opr.fun <- tclVar(.cdtData$GalParams$aggr.series$opr.fun)
	opr.thres <- tclVar(.cdtData$GalParams$aggr.series$opr.thres)

	AGGRFUN <- c("mean", "sum", "max", "min")
	if(str_trim(.cdtData$GalParams$in.tstep) == "daily") AGGRFUN <- c("mean", "sum", "max", "min", "count")
	stateCount <- if(str_trim(.cdtData$GalParams$aggr.series$aggr.fun) == "count") 'normal' else 'disabled'

	txt.aggfun <- tklabel(frameAggr, text = lang.dlg[['label']][['12']], anchor = 'w', justify = 'left')
	cb.aggfun <- ttkcombobox(frameAggr, values = AGGRFUN, textvariable = aggr.fun, width = 6)
	txt.minfrac <- tklabel(frameAggr, text = lang.dlg[['label']][['13']], anchor = 'w', justify = 'left')
	en.minfrac <- tkentry(frameAggr, textvariable = min.frac, width = 6)
	txt.opfun <- tklabel(frameAggr, text = lang.dlg[['label']][['14']], anchor = 'w', justify = 'left')
	cb.opfun <- ttkcombobox(frameAggr, values = c(">=", ">", "<=", "<"), textvariable = opr.fun, width = 6, state = stateCount)
	txt.opthres <- tklabel(frameAggr, text = lang.dlg[['label']][['15']], anchor = 'w', justify = 'left')
	en.opthres <- tkentry(frameAggr, textvariable = opr.thres, width = 6, width = 6, state = stateCount)

	tkgrid(txt.aggfun, row = 0, column = 0, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
	tkgrid(cb.aggfun, row = 0, column = 1, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
	tkgrid(txt.minfrac, row = 0, column = 2, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
	tkgrid(en.minfrac, row = 0, column = 3, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
	tkgrid(txt.opfun, row = 1, column = 0, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
	tkgrid(cb.opfun, row = 1, column = 1, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
	tkgrid(txt.opthres, row = 1, column = 2, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
	tkgrid(en.opthres, row = 1, column = 3, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)

	helpWidget(cb.aggfun, lang.dlg[['tooltip']][['12']], lang.dlg[['status']][['12']])
	helpWidget(en.minfrac, lang.dlg[['tooltip']][['13']], lang.dlg[['status']][['13']])
	helpWidget(cb.opfun, lang.dlg[['tooltip']][['14']], lang.dlg[['status']][['14']])
	helpWidget(en.opthres, lang.dlg[['tooltip']][['15']], lang.dlg[['status']][['15']])

	##############
	tkbind(cb.aggfun, "<<ComboboxSelected>>", function(){
		stateCount <- if(tclvalue(aggr.fun) == "count") "normal" else "disabled"
		tkconfigure(cb.opfun, state = stateCount)
		tkconfigure(en.opthres, state = stateCount)
	})

	############################################
	frSave <- tkframe(frAGGRTS, relief = 'groove', borderwidth = 2)

	file.save <- tclVar(.cdtData$GalParams$output)

	if(.cdtData$GalParams$data.type == 'cdtstation'){
		txtSaveDir <- lang.dlg[['label']][['9']]
		isFile <- TRUE
	}else{
		txtSaveDir <- lang.dlg[['label']][['10']]
		isFile <- FALSE
	}
	fileORdir <- tclVar(txtSaveDir)

	txt.file.save <- tklabel(frSave, text = tclvalue(fileORdir), textvariable = fileORdir, anchor = 'w', justify = 'left')
	en.file.save <- tkentry(frSave, textvariable = file.save, width = largeur)
	bt.file.save <- tkbutton(frSave, text = "...")

	tkconfigure(bt.file.save, command = function() fileORdir2Save(file.save, isFile = isFile))

	tkgrid(txt.file.save, row = 0, column = 0, sticky = 'we', rowspan = 1, columnspan = 5, padx = 1, pady = 0, ipadx = 1, ipady = 1)
	tkgrid(en.file.save, row = 1, column = 0, sticky = 'we', rowspan = 1, columnspan = 4, padx = 0, pady = 0, ipadx = 1, ipady = 1)
	tkgrid(bt.file.save, row = 1, column = 4, sticky = 'w', rowspan = 1, columnspan = 1, padx = 0, pady = 0, ipadx = 1, ipady = 1)

	###################
	if(.cdtData$GalParams$data.type == 'cdtstation'){
		txtStatus <- lang.dlg[['status']][['10']]
		txtTooltip <- lang.dlg[['tooltip']][['10']]
	}else{
		txtStatus <- lang.dlg[['status']][['11']]
		txtTooltip <- lang.dlg[['tooltip']][['11']]
	}

	helpWidget(en.file.save, txtTooltip, txtStatus)
	helpWidget(bt.file.save, lang.dlg[['tooltip']][['9']], lang.dlg[['status']][['9']])

	############################################
	tkgrid(frConvTS, row = 0, column = 0, sticky = 'we', padx = 1, pady = 1, ipadx = 1, ipady = 1)
	tkgrid(frDataType, row = 1, column = 0, sticky = 'we', padx = 1, pady = 1, ipadx = 1, ipady = 1)
	tkgrid(frameAggr, row = 3, column = 0, sticky = '', padx = 1, pady = 1, ipadx = 1, ipady = 1)
	tkgrid(frSave, row = 4, column = 0, sticky = 'we', padx = 1, pady = 1, ipadx = 1, ipady = 1)

	############################################

	tkgrid(frAGGRTS, row = 0, column = 0, sticky = 'news', padx = 5, pady = 1, ipadx = 1, ipady = 1)

	############################################

	bt.prm.OK <- ttkbutton(frMRG1, text = .cdtEnv$tcl$lang$global[['button']][['1']])
	bt.prm.CA <- ttkbutton(frMRG1, text = .cdtEnv$tcl$lang$global[['button']][['2']])

	####
	tkconfigure(bt.prm.OK, command = function(){
		if(tclvalue(file.stnfl) == ""){
			tkmessageBox(message = lang.dlg[['message']][['1']], icon = "warning", type = "ok")
		}else if(tclvalue(file.save) %in% c("", "NA")){
			tkmessageBox(message = lang.dlg[['message']][['2']], icon = "warning", type = "ok")
			tkwait.window(tt)
		}else if(str_trim(tclvalue(DataType)) == CbdatatypeVAL[3] & is.null(settingdone)){
				tkmessageBox(message = lang.dlg[['message']][['3']], icon = "warning", type = "ok")
				tkwait.window(tt)
		}else{
			.cdtData$GalParams$in.tstep <- period0VAL[Cbperiod0VAL %in% str_trim(tclvalue(OriginData))]
			.cdtData$GalParams$out.tstep <- period1VAL[Cbperiod1VAL %in% str_trim(tclvalue(ConvertData))]

			.cdtData$GalParams$Seasonal$start.mon <- as.numeric(str_trim(tclvalue(start.mon)))
			.cdtData$GalParams$Seasonal$length.mon <- as.numeric(str_trim(tclvalue(length.mon)))

			.cdtData$GalParams$data.type <- datatypeVAL[CbdatatypeVAL %in% str_trim(tclvalue(DataType))]

			if(str_trim(tclvalue(DataType)) == CbdatatypeVAL[1])
				.cdtData$GalParams$cdtstation <- str_trim(tclvalue(file.stnfl))
			if(str_trim(tclvalue(DataType)) == CbdatatypeVAL[2])
				.cdtData$GalParams$cdtdataset <- str_trim(tclvalue(file.stnfl))
			if(str_trim(tclvalue(DataType)) == CbdatatypeVAL[3])
				.cdtData$GalParams$cdtnetcdf$dir <- str_trim(tclvalue(file.stnfl))

			.cdtData$GalParams$output <- str_trim(tclvalue(file.save))

			.cdtData$GalParams$aggr.series$aggr.fun <- str_trim(tclvalue(aggr.fun))
			.cdtData$GalParams$aggr.series$min.frac <- as.numeric(str_trim(tclvalue(min.frac)))
			.cdtData$GalParams$aggr.series$opr.fun <- str_trim(tclvalue(opr.fun))
			.cdtData$GalParams$aggr.series$opr.thres <- as.numeric(str_trim(tclvalue(opr.thres)))

			.cdtData$GalParams$settingdone <- settingdone
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
	tkbind(tt, "<Destroy>", function() {
		tkgrab.release(tt)
		tkfocus(.cdtEnv$tcl$main$win)
	})
	tkwait.window(tt)
}
