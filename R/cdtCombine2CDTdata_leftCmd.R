
merge2CDTdata.PanelCmd <- function(){
	listOpenFiles <- openFile_ttkcomboList()

	xml.dlg <- file.path(.cdtDir$dirLocal, "languages", "cdtCombine2CDTdata_leftCmd.xml")
	lang.dlg <- cdtLanguageParse(xml.dlg, .cdtData$Config$lang.iso)

	#############

	.cdtEnv$tcl$main$cmd.frame <- tkframe(.cdtEnv$tcl$main$panel.left)

	input.cmd <- tkframe(.cdtEnv$tcl$main$cmd.frame, relief = 'groove', bd = 2)
	merge.cmd <- tkframe(.cdtEnv$tcl$main$cmd.frame)

	#############

	file.stnfl1 <- tclVar()
	file.stnfl2 <- tclVar()
	file.save1 <- tclVar()

	txtStnfl1 <- tklabel(input.cmd, text = lang.dlg[['label']][['1']], anchor = 'w', justify = 'left')
	cbStnfl1 <- ttkcombobox(input.cmd, values = unlist(listOpenFiles), textvariable = file.stnfl1)
	btStnfl1 <- tkbutton(input.cmd, text = "...")
	txtStnfl2 <- tklabel(input.cmd, text = lang.dlg[['label']][['2']], anchor = 'w', justify = 'left')
	cbStnfl2 <- ttkcombobox(input.cmd, values = unlist(listOpenFiles), textvariable = file.stnfl2)
	btStnfl2 <- tkbutton(input.cmd, text = "...")
	txtFileSave <- tklabel(input.cmd, text = lang.dlg[['label']][['3']], anchor = 'w', justify = 'left')
	enFileSave <- tkentry(input.cmd, textvariable = file.save1)
	btFileSave <- tkbutton(input.cmd, text = "...")

	#############

	tkconfigure(btStnfl1, command = function(){
		dat.opfiles <- getOpenFiles(.cdtEnv$tcl$main$win)
		if(!is.null(dat.opfiles)){
			update.OpenFiles('ascii', dat.opfiles)
			listOpenFiles[[length(listOpenFiles) + 1]] <<- dat.opfiles[[1]]
			tclvalue(file.stnfl1) <- dat.opfiles[[1]]

			lapply(list(cbStnfl1, cbStnfl2), tkconfigure, values = unlist(listOpenFiles))
		}
	})

	tkconfigure(btStnfl2, command = function(){
		dat.opfiles <- getOpenFiles(.cdtEnv$tcl$main$win)
		if(!is.null(dat.opfiles)){
			update.OpenFiles('ascii', dat.opfiles)
			listOpenFiles[[length(listOpenFiles) + 1]] <<- dat.opfiles[[1]]
			tclvalue(file.stnfl2) <- dat.opfiles[[1]]

			lapply(list(cbStnfl1, cbStnfl2), tkconfigure, values = unlist(listOpenFiles))
		}
	})

	tkconfigure(btFileSave, command = function(){
		file2save1 <- tk_get_SaveFile(filetypes = .cdtEnv$tcl$data$filetypes1)
		tclvalue(file.save1) <- if(is.na(file2save1)) "" else file2save1
	})

	#############

	tkgrid(txtStnfl1, row = 0, column = 0, sticky = 'we', rowspan = 1, columnspan = 2, padx = 1, pady = 0, ipadx = 1, ipady = 1)
	tkgrid(cbStnfl1, row = 1, column = 0, sticky = 'we', rowspan = 1, columnspan = 1, padx = 0, pady = 0, ipadx = 1, ipady = 1)
	tkgrid(btStnfl1, row = 1, column = 1, sticky = 'w', rowspan = 1, columnspan = 1, padx = 0, pady = 0, ipadx = 1, ipady = 1)

	tkgrid(txtStnfl2, row = 2, column = 0, sticky = 'we', rowspan = 1, columnspan = 2, padx = 1, pady = 0, ipadx = 1, ipady = 1)
	tkgrid(cbStnfl2, row = 3, column = 0, sticky = 'we', rowspan = 1, columnspan = 1, padx = 0, pady = 0, ipadx = 1, ipady = 1)
	tkgrid(btStnfl2, row = 3, column = 1, sticky = 'w', rowspan = 1, columnspan = 1, padx = 0, pady = 0, ipadx = 1, ipady = 1)

	tkgrid(txtFileSave, row = 4, column = 0, sticky = 'we', rowspan = 1, columnspan = 2, padx = 1, pady = 0, ipadx = 1, ipady = 1)
	tkgrid(enFileSave, row = 5, column = 0, sticky = 'we', rowspan = 1, columnspan = 1, padx = 0, pady = 0, ipadx = 1, ipady = 1)
	tkgrid(btFileSave, row = 5, column = 1, sticky = 'w', rowspan = 1, columnspan = 1, padx = 0, pady = 0, ipadx = 1, ipady = 1)

	#############

	mrgDataBut <- ttkbutton(merge.cmd, text = lang.dlg[['button']][['1']])

	tkconfigure(mrgDataBut, command = function(){
		GalParams <- list(file1 = str_trim(tclvalue(file.stnfl1)),
								file2 = str_trim(tclvalue(file.stnfl2)),
								file2save = str_trim(tclvalue(file.save1)))
		GalParams$message <- lang.dlg[['message']]


		tkconfigure(.cdtEnv$tcl$main$win, cursor = 'watch')
		Insert.Messages.Out(lang.dlg[['message']][['1']])
		ret <- tryCatch(
			merge2CDTdata(GalParams),
			warning = function(w) warningFun(w),
			error = function(e) errorFun(e),
			finally = {
				tkconfigure(.cdtEnv$tcl$main$win, cursor = '')
			}
		)

		if(!is.null(ret)){
			if(ret == 0) Insert.Messages.Out(lang.dlg[['message']][['2']])
			else Insert.Messages.Out(lang.dlg[['message']][['3']], format = TRUE)
		}else{
			Insert.Messages.Out(lang.dlg[['message']][['3']], format = TRUE)
		}
	})

	tkgrid(mrgDataBut, row = 0, column = 0, sticky = 'e', padx = 5, pady = 5)

	#############

	tkgrid(input.cmd, row = 0, column = 0, sticky = 'nswe', rowspan = 1, columnspan = 2)
	tkgrid(merge.cmd, row = 1, column = 1, sticky = 'se', rowspan = 1, columnspan = 1)
	tkgrid.columnconfigure(input.cmd, 0, weight = 1)
	tkgrid.columnconfigure(merge.cmd, 0, weight = 1)

	#############
	tcl('update')
	tkgrid(.cdtEnv$tcl$main$cmd.frame, sticky = 'nswe', pady = 10, padx = 3)
	tkgrid.columnconfigure(.cdtEnv$tcl$main$cmd.frame, 0, weight = 1)

	invisible()
}
