
## error and warning handling
warningFun <- function(w){
	txt <- as.character(w)
	retW <- gsub('[\r\n]', '', txt)
	Insert.Messages.Out(retW, format = TRUE)
	return(NULL)
}

errorFun <- function(e){
	txt <- as.character(e)
	retE <- gsub('[\r\n]', '', txt)
	Insert.Messages.Out(retE, format = TRUE)
	return(NULL)
}

########################################################################

## Insert text on output message
Insert.Messages.Out <- function(texta, format = FALSE, fgcolor = 'red', bgcolor = 'yellow'){
	font1 <- tkfont.create(family = "times", weight = "bold", slant = "roman", size = 11)
	font2 <- tkfont.create(family = "times", weight = "normal", slant = "italic", size = 11)
	tktag.configure(.cdtEnv$tcl$main$out.text, "formated1", foreground = fgcolor, background = bgcolor, font = font1)
	tktag.configure(.cdtEnv$tcl$main$out.text, "formated2", foreground = fgcolor, background = bgcolor, font = font2)
	txtformated <- if(fgcolor == 'red' & bgcolor == 'yellow') "formated1" else "formated2"

	# tktag.configure(.cdtEnv$tcl$main$out.text, "sel", foreground = fgcolor, background = 'blue', font = font2)
	# chn <- tclvalue(tkget(.cdtEnv$tcl$main$out.text, "0.0", "end"))
	# vectxt <- unlist(strsplit(chn, "\n"))
	# lnt <- length(vectxt)

	if(format)
		tkinsert(.cdtEnv$tcl$main$out.text, "end", paste(texta, "\n"), txtformated)
	else
		tkinsert(.cdtEnv$tcl$main$out.text, "end", paste(texta, "\n"))

	tcl(.cdtEnv$tcl$main$out.text, 'yview', 'moveto', '1.0')
	tcl("update")
}

## BWidget info-bulle help
infobulle <- function(widget, text){
	tcl("interp", "alias", "", "help", "", "DynamicHelp::register") 
	tcl('help', widget, 'balloon', text)
}

## Binding event in toolbar and display on status bar
status.bar.display <- function(widget, text){
	tkbind(widget, "<Enter>", function(){
		tclvalue(.cdtEnv$tcl$status$help) <- text
	})
	tkbind(widget, "<Leave>", function(){
		tclvalue(.cdtEnv$tcl$status$help) <- ""
	})
}

helpWidget <- function(widget, text_balloon, text_statusbar){
	status.bar.display(widget, text_statusbar)
	tcl("DynamicHelp::register", widget, 'balloon', text_balloon)
}

########################################################################

## Close Tabs buttons

btn_press <- function(x, y, W){
	elem <- tclvalue(tcl(W, 'identify', 'element', x, y))
	index <- tclvalue(tcl(W, 'identify', 'tab', x, y))
	if(elem == 'Fermer'){
		.Tcl(paste(W, 'state pressed'))
		tclvalue(.cdtEnv$tcl$main$pressed_index) <- index
	}
}

btn_releases <- function(x, y, W){
	if(as.logical(.Tcl(paste(W, 'instate pressed')))){
		elem <- tclvalue(tcl(W, 'identify', 'element', x, y))
		index <- tclvalue(tcl(W, 'identify', 'tab', x, y))
		if(elem == 'Fermer' &  tclvalue(.cdtEnv$tcl$main$pressed_index) == index){
			Close_Notebook_Tab(index)
		}
		.Tcl(paste(W, 'state !pressed'))
		tclvalue(.cdtEnv$tcl$main$pressed_index) <- ""
	}else return(NULL)
}

########################################################################

## copy/paste/cut output text message
defile.menu.out.CopyPaste <- function(x, y){
	rootx <- as.integer(tkwinfo("rootx", .cdtEnv$tcl$main$out.text))
	rooty <- as.integer(tkwinfo("rooty", .cdtEnv$tcl$main$out.text))
	xTxt <- as.integer(x) + rootx
	yTxt <- as.integer(y) + rooty
	.Tcl(paste("tk_popup", .Tcl.args(.cdtEnv$tcl$main$out.CopyPaste, xTxt, yTxt)))
}

########################################################################

## open files defile menu
defile.menu.OpenFiles <- function(x, y){
	rootx <- as.integer(tkwinfo("rootx", .cdtEnv$tcl$main$Openfiles))
	rooty <- as.integer(tkwinfo("rooty", .cdtEnv$tcl$main$Openfiles))
	xTxt <- as.integer(x) + rootx
	yTxt <- as.integer(y) + rooty
	tkselection.clear(.cdtEnv$tcl$main$Openfiles, "0", as.character(length(.cdtData$OpenFiles$Data) - 1))
	idsel <- tclvalue(tkindex(.cdtEnv$tcl$main$Openfiles, paste0("@", x, ",", y)))
	tkselection.set(.cdtEnv$tcl$main$Openfiles, idsel)
	.Tcl(paste("tk_popup", .Tcl.args(.cdtEnv$tcl$main$menu.opfiles, xTxt, yTxt)))
}

########################################################################

## table 1st column defile menu
defile.menu.OpenTable <- function(){
	tabid <- as.numeric(tclvalue(tkindex(.cdtEnv$tcl$main$tknotes, 'current'))) + 1

	table1 <- .cdtData$OpenTab$Data[[tabid]][[2]][[1]]
	data.arr <- .cdtData$OpenTab$Data[[tabid]][[2]][[2]]
	nl <- data.arr$nrow

	#########
	popup.EditTable <- tkmenu(table1, tearoff = FALSE)
	tkadd(popup.EditTable, "command", label = "Insert row above", command = function(){
		nl <<- table.insertRowAbove(table1, data.arr, nl)
	})
	tkadd(popup.EditTable, "command", label = "Insert row below", command = function(){
		nl <<- table.insertRowBelow(table1, data.arr, nl)
	})
	tkadd(popup.EditTable, "separator")
	tkadd(popup.EditTable, "command", label = "Delete selected row", command = function(){
		nl <<- table.deleteSelRow(table1, data.arr, nl)
	})

	#########
	defile.popup <- function(x, y) {
		rootx <- as.integer(tkwinfo("rootx", table1))
		rooty <- as.integer(tkwinfo("rooty", table1))
		xTxt <- as.integer(x) + rootx
		yTxt <- as.integer(y) + rooty
		if(tclvalue(tkindex(table1, paste("@", x, ",", y, sep = ""), "col")) == "0"){
			selrow <- tclvalue(tkindex(table1, paste("@", x, ",", y, sep = ""), "row"))
			tkselection.set(table1, paste(selrow, 0, sep = ','), paste(selrow, data.arr$ncol, sep = ','))
			.Tcl(paste("tk_popup", .Tcl.args(popup.EditTable, xTxt, yTxt)))
		}
	}

	#########
	tkbind(table1, "<Button-3>", function(x, y){
		defile.popup(x, y)
	})
}

table.insertRowAbove <- function(parent, data.arr, nl){
	row.nb <- unlist(strsplit(tclvalue(tcl(parent, "curselection")), ','))[1]
	tkinsert(parent, "rows", row.nb, "-1")
	nl <- nl + 1
	data.arr$nrow <- nl
	for(i in 1:nl) data.arr[i, 0] <- i
	nl
}

table.insertRowBelow <- function(parent, data.arr, nl){
	row.nb <- unlist(strsplit(tclvalue(tcl(parent, "curselection")), ','))[1]
	tkinsert(parent, "rows", row.nb, "1")
	nl <- nl + 1
	data.arr$nrow <- nl
	for(i in 1:nl) data.arr[i, 0] <- i
	nl
}

table.deleteSelRow <- function(parent, data.arr, nl){
	tmp <- unlist(strsplit(tclvalue(tcl(parent, "curselection")), ' '))
	tmp <- do.call(rbind, strsplit(tmp, ','))
	remrow <- sort(as.numeric(levels(as.factor(tmp[, 1]))))
	tkdelete(parent, "rows", remrow[1], length(remrow))
	nl <- nl - length(remrow)

	data.arr$nrow <- nl
	for(i in 1:nl) data.arr[i, 0] <- i
	nl
}

########################################################################

## Create button on toolbar
tkbutton.toolbar <- function(frame, img.file, txt.tooltip, txt.status)
{
	picture <- tkimage.create('photo', '-file', file.path(.cdtDir$Root, "images", img.file))
	button <- tkbutton(frame, image = picture, relief = 'flat')
	infobulle(button, txt.tooltip)
	status.bar.display(button, txt.status)
	return(button)
}

########################################################################

## spinbox
# ttkspinbox <- function(parent, ...) tkwidget(parent, "ttk::spinbox", ...)
ttkspinbox <- function(parent, ...){
	test <- try(tkwidget(parent, "ttk::spinbox"), silent = TRUE)
	if(!inherits(test, "try-error")){
		spinbox_widget <- "ttk::spinbox"
		tkdestroy(test)
	}else spinbox_widget <- "spinbox"
	tkwidget(parent, spinbox_widget, ...)
}

## deactivate spinbox
spinbox.state <- function(state = 'disabled'){
	tkconfigure(.cdtEnv$tcl$toolbar$spinH, state = state)
	tkconfigure(.cdtEnv$tcl$toolbar$spinV, state = state)
}

########################################################################

## Save dialog box
tk_get_SaveFile <- function(initialdir = getwd(), initialfile = "", filetypes = filetypes)
{
	if(WindowsOS()){
		f2save <- tclvalue(tkgetSaveFile(initialdir = initialdir,
										initialfile = initialfile,
										filetypes = filetypes,
										defaultextension = TRUE))
	}else{
		f2save <- tclvalue(tkgetSaveFile(initialdir = initialdir,
										initialfile = initialfile,
										filetypes = filetypes))
	}
	return(f2save)
}

## File or directory to save result
## filedirVar tclVar
fileORdir2Save <- function(filedirVar, initialdir = getwd(), isFile = TRUE){
	if(isFile){
		filetypes <- .cdtEnv$tcl$data$filetypes1
		if(WindowsOS()){
			file2save <- tclvalue(tkgetSaveFile(initialdir = initialdir,
												initialfile = "",
												filetypes = filetypes,
												defaultextension = TRUE))
		}else{
			file2save <- tclvalue(tkgetSaveFile(initialdir = initialdir,
												initialfile = "",
												filetypes = filetypes))
		}
		tclvalue(filedirVar) <- if(!is.na(file2save)) file2save else ""
	}else{
		dir2save <- tk_choose.dir(default = initialdir, caption = "")
		if(!is.na(dir2save)){
			dir.create(dir2save, showWarnings = FALSE, recursive = TRUE)
			tclvalue(filedirVar) <- dir2save
		}else tclvalue(filedirVar) <- ""
	}
}

########################################################################

## Refresh all environment, destroy left panel widgets
## old name: refreshCDT.lcmd.env(lcmdf = lcmd.frame, choixStn = lchoixStnFr)
refreshCDT <- function(){
	tkdestroy(.cdtEnv$tcl$main$cmd.frame)
	.cdtEnv$tcl$data$lcmd.frame <- NULL
	.cdtData$EnvData <- NULL
	.cdtData$EnvTmp <- NULL
}

########################################################################

## Get list of all open files

openFile_ttkcomboList <- function(){
	nopfs <- length(.cdtData$OpenFiles$Type)
	if(nopfs > 0){
		listOpenFiles <- lapply(1:nopfs, function(j) .cdtData$OpenFiles$Data[[j]][[1]])
		if(length(listOpenFiles) == 1) listOpenFiles <- c("", listOpenFiles)
	}else{
		listOpenFiles <- list()
		listOpenFiles[[1]] <- ""
	}
	return(listOpenFiles)
}

########################################################################

## BWidget NoteBook
isaTabBwNb <- local({
	k <- 0
	function() {
		k <<- k + 1
		return(k)
	}
})

bwNoteBook <- function(parent, side = 'top', ...) tkwidget(parent, "NoteBook", side = side, ...)

bwAddTab <- function(parent, text = "Tab", ...){
	IDtab <- paste0('_BwNb', isaTabBwNb())
	tab <- tkinsert(parent, 'end', IDtab, text = text, ...)
	win <- .Tk.newwin(tclvalue(tab))
	win$IDtab <- IDtab
	return(win)
}

bwRaiseTab <- function(parent, tab) tcl(parent, 'raise', tab$IDtab)

########################################################################

## BWidget ScrolledWindow
bwScrolledWindow <- function(parent, ...) tkwidget(parent, "ScrolledWindow", ...)

## BWidget ScrollableFrame
bwScrollableFrame <- function(parent, ...){
	scrfrm <- tkwidget(parent, "ScrollableFrame", ...)
	tcl(parent, "setwidget", scrfrm)
	# return(scrfrm)
	subfram <- tclvalue(tcl(scrfrm, "getframe"))
	win <- .Tk.newwin(subfram)
	return(win)
}

bwTabScrollableFrame <- function(parent, wscrlwin = .cdtEnv$tcl$data$wscrlwin,
								 hscrlwin = .cdtEnv$tcl$data$hscrlwin, ...)
{
	frTab <- tkframe(parent)
	tkgrid(frTab, padx = 1, pady = 1, ipadx = 1, ipady = 1)
	tkgrid.columnconfigure(frTab, 0, weight = 1)
	tkgrid.rowconfigure(frTab, 0, weight = 1)

	scrwin <- bwScrolledWindow(frTab)
	tkgrid(scrwin)
	tkgrid.columnconfigure(scrwin, 0, weight = 1)
	tkgrid.rowconfigure(scrwin, 0, weight = 1)

	scrfr <- bwScrollableFrame(scrwin, width = wscrlwin, height = hscrlwin, ...) #bg = "red",
	return(scrfr)
}

########################################################################

## ScrollableCanvas
ScrollCanvas <- function(parent, ...){
	canvas <- tkcanvas(parent, ...)
	tcl(parent, "setwidget", canvas)
	return(canvas)
}

setScrollCanvas <- function(parent, width, height){
	bbox <- tkbbox(parent, "all")
	tkconfigure(parent, width = width, height = height, scrollregion = bbox)
}

########################################################################

## Resize Tcl image type photo
resizeTclImage <- function(file, factor = 2, zoom = TRUE){
	imgtmp <- tkimage.create('photo', file = file)
	imgrsz <- tkimage.create("photo")
	if(zoom)
		tcl(imgrsz, "copy", imgtmp, zoom = factor)
	else
		tcl(imgrsz, "copy", imgtmp, subsample = factor)  
	tcl('image', 'delete', imgtmp)
	return(imgrsz)
}

