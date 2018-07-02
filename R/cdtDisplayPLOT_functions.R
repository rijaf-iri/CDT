
## Plot graph
CDT.Display.Graph <- function(plot.graph, notebookTab, tab.title){
	onglet <- imageNotebookTab_open(notebookTab, tab.title)

	hscale <- as.numeric(tclvalue(tkget(.cdtEnv$tcl$toolbar$spinH)))
	vscale <- as.numeric(tclvalue(tkget(.cdtEnv$tcl$toolbar$spinV)))
	hscrFrame <- as.integer(tclvalue(tkwinfo("height", .cdtEnv$tcl$main$panel.right)))
	wscrFrame <- as.integer(tclvalue(tkwinfo("width", .cdtEnv$tcl$main$panel.right)))

	scrollwin <- bwScrolledWindow(onglet[[2]])
	tkgrid(scrollwin)
	tkgrid.rowconfigure(scrollwin, 0, weight = 1)
	tkgrid.columnconfigure(scrollwin, 0, weight = 1)
	containerFrame <- bwScrollableFrame(scrollwin, width = wscrFrame, height = hscrFrame)

	img <- tkrplot(containerFrame, fun = plot.graph, hscale = hscale, vscale = vscale)
	tkgrid(img)
	tkgrid.rowconfigure(img, 0, weight = 1)
	tkgrid.columnconfigure(img, 0, weight = 1)
	tcl("update")

	return(list(onglet, img))
}

########################################################################

## Plot map
CDT.Display.Map.inter <- function(plot.map, notebookTab, tab.title){
	varplot <- c("parPlotSize1", "parPlotSize2", "parPlotSize3", "parPlotSize4",
				 "usrCoords1", "usrCoords2", "usrCoords3", "usrCoords4")
	parPltCrd <- setNames(lapply(varplot, function(x) assign(x, tclVar(), env = parent.frame())), varplot)

	plotIt <- function(){
		tkconfigure(.cdtEnv$tcl$main$win, cursor = 'watch')
		tcl('update')
		on.exit({
			tkconfigure(.cdtEnv$tcl$main$win, cursor = '')
			tcl('update')
		})

		op <- par(bg = "white")
		pltusr <- plot.map()
		par(op)
		for(j in seq_along(varplot)) tclvalue(parPltCrd[[varplot[j]]]) <- pltusr$par[j]
	}

	#########
	onglet <- imageNotebookTab_open(notebookTab, tab.title)
	hscale <- as.numeric(tclvalue(tkget(.cdtEnv$tcl$toolbar$spinH)))
	vscale <- as.numeric(tclvalue(tkget(.cdtEnv$tcl$toolbar$spinV)))

	img <- tkrplot(onglet[[2]], fun = plotIt, hscale = hscale, vscale = vscale)
	tkgrid(img)
	tkgrid.rowconfigure(img, 0, weight = 1)
	tkgrid.columnconfigure(img, 0, weight = 1)
	tcl("update")

	#########

	tkbind(img, "<Motion>", function(W, x, y){
		if(.cdtData$EnvData$plot.maps$data.type == "cdtstation")
			displayCursorPosition3Var(W, x, y, parPltCrd, getStnIDLabel,
									stn.coords = .cdtData$EnvData$plot.maps[c('lon', 'lat', 'id')])
		else
			displayCursorPosition3Var(W, x, y, parPltCrd, getEmptyChar)
	})

	tkbind(img, "<Enter>", function() tkconfigure(img, cursor = 'crosshair'))
	tkbind(img, "<Leave>", function() tkconfigure(img, cursor = ''))

	return(list(onglet, img, parPltCrd))
}
