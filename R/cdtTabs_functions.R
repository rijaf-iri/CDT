
## Add new tab on the right panel
addNewTab <- function(tab.title = NULL){
	if(is.null(tab.title)) tab.title <- paste('Tab', infoTabs(), "  ")
	tab <- ttkframe(.cdtEnv$tcl$main$tknotes)
	tkadd(.cdtEnv$tcl$main$tknotes, tab, text = paste(tab.title, "  "))
	tkgrid.columnconfigure(tab, 0, weight = 1)
	tkgrid.rowconfigure(tab, 0, weight = 1)

	ftab <- tkframe(tab, bd = 2, relief = 'sunken', bg = 'white')
	tkgrid(ftab, row = 0, column = 0, sticky = 'nswe')
	tkgrid.columnconfigure(ftab, 0, weight = 1)
	tkgrid.rowconfigure(ftab, 0, weight = 1)

	return(list(tab, ftab))
}

## Count created tabs
infoTabs <- function(){
	open.tabs <- unlist(strsplit(tclvalue(tkwinfo("children", .cdtEnv$tcl$main$tknotes)), " "))
	end.tabs <- as.integer(unlist(strsplit(open.tabs[length(open.tabs)], "\\.")))
	id.tabs <- end.tabs[length(end.tabs)]
	if(length(id.tabs) == 0) id.tabs <- 0
	return(id.tabs + 1)
}

########################################################################

## Display opened data.frame on Tab in a table
Display_data.frame_Table <- function(data.df, title, colwidth = 8){
	onglet <- addNewTab(title)
	dtab <- try(tclArrayVar(data.df), silent = TRUE)
	if(inherits(dtab, "try-error")){
		Insert.Messages.Out("Unable to create tclArrayVar", format = TRUE)
		Insert.Messages.Out(gsub('[\r\n]', '', dtab[1]), format = TRUE)
		return(list(onglet, list(NULL, NULL)))
	}

	table1 <- try(displayTable(onglet[[2]], tclArray = dtab, colwidth = colwidth), silent = TRUE)
	if(inherits(table1, "try-error")){
		Insert.Messages.Out("Unable to display table", format = TRUE)
		Insert.Messages.Out(gsub('[\r\n]', '', table1[1]), format = TRUE)
		table1 <- list(NULL, dtab)
	}

	return(list(onglet, table1))
}

## Display homogenization output info
## ???replace by Display_data.frame_Table (colwidth = 24)
# DisplayHomInfo <- function(parent, homInfo, title, colwidth = '24'){
# 	onglet <- addNewTab(parent, tab.title = title)
# 	dtab <- tclArrayVar(homInfo)
# 	table1 <- displayTable(onglet[[2]], tclArray = dtab, colwidth = colwidth)
# 	return(list(onglet, table1))
# }

########
## Display output QC/HOMOGE in a table
## old name: DisplayQcHom(parent, outqchom, title)
## data: ReturnExecResults$action, GeneralParameters$action
Display_QcHom_Output <- function(out.qc.hom, title){
	onglet <- addNewTab(title)
	dtab <- tclArrayVar(out.qc.hom[[1]])
	col <- if(ReturnExecResults$action == 'homog' | GeneralParameters$action == "rhtests") '15' else '10'
	table1 <- displayTable(onglet[[2]], tclArray = dtab, colwidth = col)
	return(list(onglet, table1, out.qc.hom[-1]))
}

## ???replace by Display_data.frame_Table
# data.df <- out.qc.hom[[1]]
# col <- if(ReturnExecResults$action == 'homog' | GeneralParameters$action == "rhtests") '15' else '10'
# tab.array <- Display_data.frame_Table(data.df, title, col)
# tab.array <- c(tab.array, out.qc.hom[-1])

#######
## Display interpolation data in a table
## old name: DisplayInterpData(parent, data, title, colwidth = '15')
DisplayInterpData <- function(data, title, colwidth = '15'){
	onglet <- addNewTab(title)
	dtab <- tclArrayVar(data[[2]])
	table1 <- displayTable(onglet[[2]], tclArray = dtab, colwidth = colwidth)
	return(list(onglet, table1, data[-2]))
}

## ???replace by Display_data.frame_Table
# data.df <- data[[2]]
# tab.array <- Display_data.frame_Table(data.df, title, colwidth = 15)
# tab.array <- c(tab.array, data[-2])

########################################################################

## Deja remplacer
# ## Display data in a table from open files list
# displayInTable <- function(){
# 	id.active <- as.integer(tclvalue(tkcurselection(.cdtEnv$tcl$main$Openfiles))) + 1
# 	onglet <- addNewTab(.cdtData$OpenFiles$Data[[id.active]][[1]])
# 	dat.file <- .cdtData$OpenFiles$Data[[id.active]][[2]]
# 	dtab <- tclArrayVar(dat.file)
# 	table1 <- displayTable(onglet[[2]], tclArray = dtab)
# 	return(list(onglet, table1, .cdtData$OpenFiles$Data[[id.active]][[3]]))
# }

# id.active <- as.integer(tclvalue(tkcurselection(.cdtEnv$tcl$main$Openfiles))) + 1
# title <- .cdtData$OpenFiles$Data[[id.active]][[1]]
# data.df <- .cdtData$OpenFiles$Data[[id.active]][[2]]
# data.out <- Display_data.frame_Table(data.df, title)
# tab.array <- c(data.out, .cdtData$OpenFiles$Data[[id.active]][[3]])

########################################################################

## Open and display table on new tab
## old name: displayArrayTab(parent.win, parent)
Display_Array_Tab <- function(parent){
	on.exit({
		tkconfigure(.cdtEnv$tcl$main$win, cursor = '')
		tcl('update')
	})
	tkconfigure(.cdtEnv$tcl$main$win, cursor = 'watch')
	tcl('update')

	data.file <- getOpenFiles(parent)
	if(is.null(data.file)) return(NULL)
	update.OpenFiles('ascii', data.file)
	
	tab.array <- Display_data.frame_Table(data.file[[2]], data.file[[1]])
	tab.array <- c(tab.array, data.file[[3]])

	return(tab.array)
}

########################################################################

## Display output console inner frame
## old name: displayConsOutput(parent, out2disp, rhtests = FALSE)
Display_Output_Console <- function(parent, out2disp, rhtests = FALSE){
	xscr <- tkscrollbar(parent, repeatinterval = 5, orient = "horizontal",
						command = function(...) tkxview(txta, ...))
	yscr <- tkscrollbar(parent, repeatinterval = 5,
						command = function(...) tkyview(txta, ...))
	txta <- tktext(parent, bg = "white", font = "courier", wrap = "none", 
					xscrollcommand = function(...) tkset(xscr, ...),
					yscrollcommand = function(...) tkset(yscr, ...))
	tkgrid(txta, yscr)
	tkgrid(xscr)
	tkgrid.configure(yscr, sticky = "ns")
	tkgrid.configure(xscr, sticky = "ew")
	tkgrid.configure(txta, sticky = 'nswe')
	tkgrid.rowconfigure(txta, 0, weight = 1)
	tkgrid.columnconfigure(txta, 0, weight = 1)
	if(!rhtests){
		tempfile <- tempfile()
		sink(tempfile)
		op <- options()
		options(width = 160)
		print(out2disp)
		options(op)
		sink()
		rdL <- readLines(tempfile, warn = FALSE)
		for(i in 1:length(rdL)) tkinsert(txta, "end", paste(rdL[i], "\n"))
		unlink(tempfile)
	}else tkinsert(txta, "end", out2disp)
}

## Display output console on tab
## old name: displayConsOutputTabs(parent, out2disp, title, rhtests = FALSE)
Display_Output_Console_Tab <- function(out2disp, title, rhtests = FALSE){
	onglet <- addNewTab(title)
	Display_Output_Console(onglet[[2]], out2disp, rhtests)
	return(onglet)
}

########################################################################

## Update Open Tab data
update.OpenTabs <- function(type, data){
	ntab <- length(.cdtData$OpenTab$Type)
	.cdtData$OpenTab$Type[[ntab + 1]] <- type
	.cdtData$OpenTab$Data[[ntab + 1]] <- data
	return(ntab)
}

########################################################################

## Close tab

Close_Notebook_Tab <- function(index)
{
	tabid <- as.integer(index) + 1
	if(!is.na(tabid)){
		arrTypes <- c("arr", "chkcrds", "falsezero", "outqc",
					"arrhom", "arrRHtest", "arrInterp",
					"homInfo", "arrValid")
		if(.cdtData$OpenTab$Type[[tabid]] %in% arrTypes){
			tkdestroy(.cdtData$OpenTab$Data[[tabid]][[1]][[1]])
		}else if(.cdtData$OpenTab$Type[[tabid]] == "ctxt"){
			tkdestroy(.cdtData$OpenTab$Data[[tabid]][[1]])
		}else if(.cdtData$OpenTab$Type[[tabid]] == "img"){
			tkdestroy(.cdtData$OpenTab$Data[[tabid]][[1]][[2]])
			tkdestroy(.cdtData$OpenTab$Data[[tabid]][[1]][[1]])
		}else return(NULL)
		.cdtData$OpenTab$Data[tabid] <- NULL
		.cdtData$OpenTab$Type[tabid] <- NULL
	}else return(NULL)
}

########################################################################

## Save table As
Save_Table_As <- function(){
	on.exit({
		tkconfigure(.cdtEnv$tcl$main$win, cursor = '')
		tcl('update')
	})
	tkconfigure(.cdtEnv$tcl$main$win, cursor = 'watch')
	tcl('update')

	tabid <- as.integer(tclvalue(tkindex(.cdtEnv$tcl$main$tknotes, 'current'))) + 1
	if(!is.na(tabid)){
		Objarray <- .cdtData$OpenTab$Data[[tabid]][[2]]
		if(!inherits(Objarray[[2]], "tclArrayVar")){
			Insert.Messages.Out(.cdtEnv$tcl$lang$global[['message']][['6']], format = TRUE)
			return(NULL)
		}
		tryCatch(
			{
				file.to.save <- tk_get_SaveFile(filetypes = .cdtEnv$tcl$data$filetypes2)
				dat2sav <- tclArray2dataframe(Objarray)
				
				file.spec <- NULL
				if(length(.cdtData$OpenTab$Data[[tabid]]) > 2){
					file.disk <- .cdtData$OpenTab$Data[[tabid]][[3]]
					if(file.exists(file.disk)){
						nopfs <- length(.cdtData$OpenFiles$Type)
						if(nopfs > 0){
							listOpenFiles <- sapply(1:nopfs, function(j) .cdtData$OpenFiles$Data[[j]][[1]])
							if(basename(file.disk) %in% listOpenFiles){
								nopf <- which(listOpenFiles %in% basename(file.disk))
								file.spec <- .cdtData$OpenFiles$Data[[nopf]][[4]]
							}
						}
					}
				}

				if(!is.null(file.spec)){
					writeFiles(dat2sav, file.to.save, col.names = file.spec$header,
								na = file.spec$miss.val, sep = file.spec$sepr)
				}else{
					writeFiles(dat2sav, file.to.save, col.names = Objarray[[2]]$col.names)
				}
				Insert.Messages.Out(.cdtEnv$tcl$lang$global[['message']][['3']])
			},
			warning = function(w){
				Insert.Messages.Out(.cdtEnv$tcl$lang$global[['message']][['2']], format = TRUE)
				warningFun(w)
			},
			error = function(e){
				Insert.Messages.Out(.cdtEnv$tcl$lang$global[['message']][['2']], format = TRUE)
				errorFun(e)
			}
		)
	}

	invisible()
}

########################################################################

## data: ReturnExecResults, GeneralParameters
## fun: reHomOutFormat, SummaryData.Get.Table
## à changer "arrInterp" / saveRDS /(assign)
Save_Notebook_Tab_Array <- function(){
	on.exit({
		tkconfigure(.cdtEnv$tcl$main$win, cursor = '')
		tcl('update')
	})
	tkconfigure(.cdtEnv$tcl$main$win, cursor = 'watch')
	tcl('update')

	tabid <- as.integer(tclvalue(tkindex(.cdtEnv$tcl$main$tknotes, 'current'))) + 1
	if(length(.cdtData$OpenTab$Type) > 0){
		if(.cdtData$OpenTab$Type[[tabid]] == "arr"){
			Objarray <- .cdtData$OpenTab$Data[[tabid]][[2]]
			dat2sav <- tclArray2dataframe(Objarray)
			if(is.null(dat2sav)){
				Insert.Messages.Out("No data to save", format = TRUE)
				return(NULL)
			}
			if(length(.cdtData$OpenTab$Data[[tabid]]) == 2){
				filetosave <- tk_get_SaveFile(filetypes = .cdtEnv$tcl$data$filetypes2)
				writeFiles(dat2sav, filetosave, col.names = TRUE)
			}else{
				filetosave <- .cdtData$OpenTab$Data[[tabid]][[3]]
				file.spec <- NULL
				nopfs <- length(.cdtData$OpenFiles$Type)
				if(nopfs > 0){
					listOpenFiles <- sapply(1:nopfs, function(j) .cdtData$OpenFiles$Data[[j]][[1]])
					if(basename(filetosave) %in% listOpenFiles){
						nopf <- which(listOpenFiles %in% basename(filetosave))
						file.spec <- .cdtData$OpenFiles$Data[[nopf]][[4]]
					}
				}

				if(!is.null(file.spec)){
					writeFiles(dat2sav, filetosave, col.names = file.spec$header,
								na = file.spec$miss.val, sep = file.spec$sepr)
				}else{
					writeFiles(dat2sav, filetosave,
								col.names = Objarray[[2]]$col.names,
								row.names = Objarray[[2]]$row.names)
				}
			}
		}else if(.cdtData$OpenTab$Type[[tabid]] == "chkcrds"){
			Objarray <- .cdtData$OpenTab$Data[[tabid]][[2]]
			dat2sav <- tclArray2dataframe(Objarray)

			.cdtData$EnvData$StnChkCoords$SaveEdit(dat2sav)
		}else if(.cdtData$OpenTab$Type[[tabid]] == "falsezero"){
			Objarray <- .cdtData$OpenTab$Data[[tabid]][[2]]
			dat2sav <- tclArray2dataframe(Objarray)

			.cdtData$EnvData$qcRRZeroCheck$SaveEdit(dat2sav)
		}else if(.cdtData$OpenTab$Type[[tabid]] == "outqc"){
			Objarray <- .cdtData$OpenTab$Data[[tabid]][[2]]
			dat2sav <- tclArray2dataframe(Objarray)

			.cdtData$EnvData$QC$SaveEdit(dat2sav)
		}else if(.cdtData$OpenTab$Type[[tabid]] == "homog"){


		}else return(NULL)

		# }else if(.cdtData$OpenTab$Type[[tabid]] == "arrhom"){
		# 	if(ReturnExecResults$action == 'homog' & ReturnExecResults$period == 'daily'){
		# 		filetosave <- .cdtData$OpenTab$Data[[tabid]][[3]]
		# 		f2sdly <- filetosave[[1]]
		# 		f2sdek <- filetosave[[2]]
		# 		f2smon <- filetosave[[3]]
		# 		Objarray <- .cdtData$OpenTab$Data[[tabid]][[2]]
		# 		dat2format <- tclArray2dataframe(Objarray)
		# 		dat2sav <- reHomOutFormat(dat2format)
		# 		write.table(dat2sav[[1]], f2sdly, row.names = FALSE, col.names = TRUE)
		# 		write.table(dat2sav[[2]], f2sdek, row.names = FALSE, col.names = TRUE)
		# 		write.table(dat2sav[[3]], f2smon, row.names = FALSE, col.names = TRUE)
		# 	}else if(ReturnExecResults$action == 'homog' & ReturnExecResults$period == 'dekadal'){
		# 		filetosave <- .cdtData$OpenTab$Data[[tabid]][[3]]
		# 		f2sdek <- filetosave[[1]]
		# 		f2smon <- filetosave[[2]]
		# 		Objarray <- .cdtData$OpenTab$Data[[tabid]][[2]]
		# 		dat2format <- tclArray2dataframe(Objarray)
		# 		dat2sav <- reHomOutFormat(dat2format)
		# 		write.table(dat2sav[[1]], f2sdek, row.names = FALSE, col.names = TRUE)
		# 		write.table(dat2sav[[2]], f2smon, row.names = FALSE, col.names = TRUE)
		# 	}else if(ReturnExecResults$action == 'homog' & ReturnExecResults$period == 'monthly'){
		# 		filetosave <- .cdtData$OpenTab$Data[[tabid]][[3]]
		# 		f2smon <- filetosave[[1]]
		# 		Objarray <- .cdtData$OpenTab$Data[[tabid]][[2]]
		# 		dat2format <- tclArray2dataframe(Objarray)
		# 		dat2sav <- reHomOutFormat(dat2format)
		# 		write.table(dat2sav[[1]], f2smon, row.names = FALSE, col.names = TRUE)
		# 	}else{
		# 		Insert.Messages.Out(.cdtEnv$tcl$lang$global[['message']][['2']], format = TRUE)
		# 		return(NULL)
		# 	}
		# }else if(.cdtData$OpenTab$Type[[tabid]] == "arrRHtest"){
		# 	f2save <- .cdtData$OpenTab$Data[[tabid]][[3]][[1]]
		# 	Objarray <- .cdtData$OpenTab$Data[[tabid]][[2]]
		# 	dat2sav <- tclArray2dataframe(Objarray)
		# 	head <- readLines(f2save, n = 1)
		# 	cat(paste(head, '\n'), file = f2save)
		# 	if(!is.null(dat2sav)){
		# 		dat2sav <- dat2sav[!is.na(dat2sav[, 3]), , drop = FALSE]
		# 		nline <- nrow(dat2sav)
		# 		if(nline > 0){
		# 			tmp4 <- strsplit(str_trim(gsub("[()-]", " ", dat2sav[, 4])), ' ')
		# 			tmp4 <- lapply(tmp4, function(x) if(length(x) == 0) c(NA, NA) else x)
		# 			tmp7 <- strsplit(str_trim(gsub("[()-]", " ", dat2sav[, 7])), ' ')
		# 			tmp7 <- lapply(tmp7, function(x) if(length(x) == 0) c(NA, NA) else x)
		# 			tmp <- cbind(dat2sav[, 1:3], do.call('rbind', tmp4),
		# 						 dat2sav[, 5:6], do.call('rbind', tmp7))
		# 			tmp <- convert_data_type(tmp, as.character)
		# 			colClasses <- c('numeric', 'character', rep('numeric', 7))
		# 			tmp0 <- data.frame(tmp)
		# 			tmp0[] <- NA
		# 			for(j in 1:ncol(tmp)) tmp0[, j] <- as(tmp[, j], colClasses[j])
		# 			for(j in 1:nline){
		# 				cat(paste0(
		# 					ifelse(is.na(tmp0[j, 1]), sprintf("%1.0s", ''), sprintf("%1.0f", tmp0[j, 1])), " ",
		# 					sprintf("%-4.4s", ifelse(is.na(tmp0[j, 2]), '',tmp0[j, 2])),
		# 					ifelse(is.na(tmp0[j, 3]), sprintf("%10.0s", ''), sprintf("%10.0f", tmp0[j, 3])), " (",
		# 					ifelse(is.na(tmp0[j, 4]), sprintf("%6.4s", ''), sprintf("%6.4f", tmp0[j, 4])), "-",
		# 					ifelse(is.na(tmp0[j, 5]), sprintf("%6.4s", ''), sprintf("%6.4f", tmp0[j, 5])), ")",
		# 					ifelse(is.na(tmp0[j, 6]), sprintf("%6.3s", ''), sprintf("%6.3f", tmp0[j, 6])),
		# 					ifelse(is.na(tmp0[j, 7]), sprintf("%10.4s", ''), sprintf("%10.4f", tmp0[j, 7])), " (",
		# 					ifelse(is.na(tmp0[j, 8]), sprintf("%10.4s", ''), sprintf("%10.4f", tmp0[j, 8])), "-",
		# 					ifelse(is.na(tmp0[j, 9]), sprintf("%10.4s", ''), sprintf("%10.4f", tmp0[j, 9])), ")\n"
		# 				), file = f2save, append = TRUE)
		# 			}
		# 		}
		# 	}
		# }else if(.cdtData$OpenTab$Type[[tabid]] == "arrInterp"){
		# 	Objarray <- .cdtData$OpenTab$Data[[tabid]][[2]]
		# 	dat2sav <- tclArray2dataframe(Objarray)
		# 	elvd <- as.numeric(as.character(dat2sav$elv))
		# 	if(sum(!is.na(elvd)) == 0) elvd <- NULL
		# 	donnees <- list(date = .cdtData$OpenTab$Data[[tabid]][[3]][[1]],
		# 					lon = as.numeric(as.character(dat2sav$lon)),
		# 					lat = as.numeric(as.character(dat2sav$lat)),
		# 					id = as.character(dat2sav$id),
		# 					z = as.numeric(as.character(dat2sav$z)),
		# 					elv = elvd)
		# 	assign('donnees', donnees, envir = EnvInterpolation)
		# }else return(NULL)

	}else return(NULL)

	return(0)
}

