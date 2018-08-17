
## Plot graph
CDT.Display.Graph <- function(plot.graph, notebookTab, tab.title){
	plotIt <- function(){
		tkconfigure(.cdtEnv$tcl$main$win, cursor = 'watch')
		tcl('update')
		on.exit({
			tkconfigure(.cdtEnv$tcl$main$win, cursor = '')
			tcl('update')
		})
		op <- par(bg = "white")
		plot.graph()
		par(op)
	}

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
	tcl("update")

	img <- tkrplot(containerFrame, fun = plotIt, hscale = hscale, vscale = vscale)
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
		{
			display.shp.attr <- FALSE
			if(!is.null(.cdtData$EnvData$plot.maps$shp))
				if(.cdtData$EnvData$plot.maps$shp$display) display.shp.attr <- TRUE

			if(display.shp.attr){
				displayCursorPosition3Var(W, x, y, parPltCrd, getAdminLabel,
										shp = .cdtData$EnvData$plot.maps$shp$shp,
										idField = .cdtData$EnvData$plot.maps$shp$field)
			}else displayCursorPosition3Var(W, x, y, parPltCrd, getEmptyChar)
		}
	})

	tkbind(img, "<Enter>", function() tkconfigure(img, cursor = 'crosshair'))
	tkbind(img, "<Leave>", function() tkconfigure(img, cursor = ''))

	return(list(onglet, img, parPltCrd))
}

########################################################################

## plot stations coordinates with zoom
CDT.Display.Points.Zoom <- function(plot.map, notebookTab, tab.title){
	if(is.null(.cdtData$EnvData)) return(NULL)
	if(is.null(.cdtData$EnvData$plot.maps$id)) return(NULL)
	stn.coords <- .cdtData$EnvData$plot.maps[c('lon', 'lat', 'id')]

	#########################

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

	###################################################################

	onglet <- imageNotebookTab_open(notebookTab, tab.title)
	hscale <- as.numeric(tclvalue(tkget(.cdtEnv$tcl$toolbar$spinH)))
	vscale <- as.numeric(tclvalue(tkget(.cdtEnv$tcl$toolbar$spinV)))

	canvas <- tkcanvas(onglet[[2]])
	tkgrid(canvas)

	img <- tkrplot(canvas, fun = plotIt, hscale = hscale, vscale = vscale)
	img_w <- as.double(tcl('image', 'width', img$image))
	img_h <- as.double(tcl('image', 'height', img$image))
	tkconfigure(canvas, width = img_w, height = img_h)
	tkcreate(canvas, "image", 0, 0, anchor = 'nw', image = img$image)
	tcl('raise', canvas)
	tcl('update')

	##########

	tkbind(canvas, "<Enter>", function(){
		if(is.null(.cdtData$EnvData$zoom)) return(NULL)

		if(tclvalue(.cdtData$EnvData$zoom$pressButP) == "1")
			tkconfigure(canvas, cursor = 'sizing')
		else if(tclvalue(.cdtData$EnvData$zoom$pressButM) == "1")
			tkconfigure(canvas, cursor = 'sizing')
		else if(tclvalue(.cdtData$EnvData$zoom$pressButRect) == "1")
			tkconfigure(canvas, cursor = 'sizing')
		else if(tclvalue(.cdtData$EnvData$zoom$pressButDrag) == "1")
			tkconfigure(canvas, cursor = 'hand1')
		else
			tkconfigure(canvas, cursor = 'crosshair')
	})

	tkbind(canvas, "<Leave>", function() tkconfigure(canvas, cursor = ''))

	##########
	## draw rectangle initial value
	.cdtEnv$tcl$lastX <- 0
	.cdtEnv$tcl$lastY <- 0

	## zoom factor
	factZoom <- 0.2

	##zoom rectangle
	rectZoomInit <- .cdtData$EnvData$ZoomXYval

	## Pan Image
	panZoomInit <- c(0, 0, 0, 0, 0, 0)
	factPan <- 0.2

	##########
	## first click on map
	tkbind(canvas, "<Button-1>", function(W, x, y){
		if(is.null(.cdtData$EnvData$zoom)) return(NULL)

		ret <- getXYCoords(W, x, y, parPltCrd)
		tkdelete(W, 'rect')

		#Zoom plus
		if(tclvalue(.cdtData$EnvData$zoom$pressButP) == "1" & !ret$oin){
			rgX <- as.numeric(tclvalue(parPltCrd$usrCoords2)) - as.numeric(tclvalue(parPltCrd$usrCoords1))
			rgY <- as.numeric(tclvalue(parPltCrd$usrCoords4)) - as.numeric(tclvalue(parPltCrd$usrCoords3))
			shiftX <- rgX * (1 - factZoom)/2
			shiftY <- rgY * (1 - factZoom)/2
			xmin1 <- ret$xc - shiftX
			xmax1 <- ret$xc + shiftX
			ymin1 <- ret$yc - shiftY
			ymax1 <- ret$yc + shiftY

			.cdtData$EnvData$ZoomXYval <- c(xmin1, xmax1, ymin1, ymax1)

			tclvalue(.cdtData$EnvData$zoom$xx1) <- round(xmin1, 4)
			tclvalue(.cdtData$EnvData$zoom$xx2) <- round(xmax1, 4)
			tclvalue(.cdtData$EnvData$zoom$yy1) <- round(ymin1, 4)
			tclvalue(.cdtData$EnvData$zoom$yy2) <- round(ymax1, 4)

			refreshPlot(W, img,
						hscale = as.numeric(tclvalue(tkget(.cdtEnv$tcl$toolbar$spinH))),
						vscale = as.numeric(tclvalue(tkget(.cdtEnv$tcl$toolbar$spinV)))
						)
		}

		#Zoom Moins
		if(tclvalue(.cdtData$EnvData$zoom$pressButM) == "1" & !ret$oin){
			rgX <- as.numeric(tclvalue(parPltCrd$usrCoords2)) - as.numeric(tclvalue(parPltCrd$usrCoords1))
			rgY <- as.numeric(tclvalue(parPltCrd$usrCoords4)) - as.numeric(tclvalue(parPltCrd$usrCoords3))
			shiftX <- rgX * (1 + factZoom)/2
			shiftY <- rgY * (1 + factZoom)/2
			xmin1 <- ret$xc - shiftX
			xmax1 <- ret$xc + shiftX
			ymin1 <- ret$yc - shiftY
			ymax1 <- ret$yc + shiftY

			if(xmin1< -180 | xmax1 > 180 | ymin1< -90 | ymax1 > 90){
				tclvalue(.cdtData$EnvData$zoom$pressButP) <- 0
				tclvalue(.cdtData$EnvData$zoom$pressButM) <- 0
				tclvalue(.cdtData$EnvData$zoom$pressButRect) <- 0
				tclvalue(.cdtData$EnvData$zoom$pressButDrag) <- 0
				
				tkconfigure(.cdtData$EnvData$zoom$btZoomP, relief = 'raised', bg = 'lightblue', state = 'normal')
				tkconfigure(.cdtData$EnvData$zoom$btZoomM, relief = 'raised', bg = 'lightblue', state = 'normal')
				tkconfigure(.cdtData$EnvData$zoom$btZoomRect, relief = 'raised', bg = 'lightblue', state = 'normal')
				tkconfigure(.cdtData$EnvData$zoom$btPanImg, relief = 'raised', bg = 'lightblue', state = 'normal')

				tkconfigure(W, cursor = 'crosshair')
			}else{
				.cdtData$EnvData$ZoomXYval <- c(xmin1, xmax1, ymin1, ymax1)

				tclvalue(.cdtData$EnvData$zoom$xx1) <- round(xmin1, 4)
				tclvalue(.cdtData$EnvData$zoom$xx2) <- round(xmax1, 4)
				tclvalue(.cdtData$EnvData$zoom$yy1) <- round(ymin1, 4)
				tclvalue(.cdtData$EnvData$zoom$yy2) <- round(ymax1, 4)

				refreshPlot(W, img,
							hscale = as.numeric(tclvalue(tkget(.cdtEnv$tcl$toolbar$spinH))),
							vscale = as.numeric(tclvalue(tkget(.cdtEnv$tcl$toolbar$spinV)))
							)
			}
		}

		##Zoom rectangle
		if(tclvalue(.cdtData$EnvData$zoom$pressButRect) == "1" & !ret$oin){
			pPressRect(W, x, y, width = 1, outline = "red")
			rectZoomInit[1] <<- ret$xc
			rectZoomInit[3] <<- ret$yc
		}

		##Pan image
		if(tclvalue(.cdtData$EnvData$zoom$pressButDrag) == "1" & !ret$oin){
			panZoomInit[1] <<- ret$xc
			panZoomInit[2] <<- ret$yc

			panZoomInit[3] <<- as.numeric(tclvalue(.cdtData$EnvData$zoom$xx1))
			panZoomInit[4] <<- as.numeric(tclvalue(.cdtData$EnvData$zoom$xx2))
			panZoomInit[5] <<- as.numeric(tclvalue(.cdtData$EnvData$zoom$yy1))
			panZoomInit[6] <<- as.numeric(tclvalue(.cdtData$EnvData$zoom$yy2))

			tkconfigure(canvas, cursor = 'hand2')
		}
	})

	##########
	## cursor movement
	tkbind(canvas, "<Motion>", function(W, x, y){
		if(is.null(.cdtData$EnvData$zoom)) return(NULL)

		displayCursorPosition3Var(W, x, y, parPltCrd, getStnIDLabel, stn.coords = stn.coords)
	})

	#########
	## cursor movement with button-1 pressed
	tkbind(canvas, "<B1-Motion>", function(W, x, y){
		if(is.null(.cdtData$EnvData$zoom)) return(NULL)

		ret <- getXYCoords(W, x, y, parPltCrd)

		##Zoom rectangle
		if(tclvalue(.cdtData$EnvData$zoom$pressButRect) == "1"){
			pMoveRect(W, x, y)
		}

		##Pan image
		if(tclvalue(.cdtData$EnvData$zoom$pressButDrag) == "1"){
			transX <- ret$xc - panZoomInit[1]
			transY <- ret$yc - panZoomInit[2]

			tclvalue(.cdtData$EnvData$zoom$xx1) <- round(panZoomInit[3] + factPan * transX, 4)
			tclvalue(.cdtData$EnvData$zoom$xx2) <- round(panZoomInit[4] + factPan * transX, 4)
			tclvalue(.cdtData$EnvData$zoom$yy1) <- round(panZoomInit[5] + factPan * transY, 4)
			tclvalue(.cdtData$EnvData$zoom$yy2) <- round(panZoomInit[6] + factPan * transY, 4)

			.cdtData$EnvData$ZoomXYval <- as.numeric(c(
														tclvalue(.cdtData$EnvData$zoom$xx1),
														tclvalue(.cdtData$EnvData$zoom$xx2),
														tclvalue(.cdtData$EnvData$zoom$yy1),
														tclvalue(.cdtData$EnvData$zoom$yy2)
													))
		}

		refreshPlot(W, img,
					hscale = as.numeric(tclvalue(tkget(.cdtEnv$tcl$toolbar$spinH))),
					vscale = as.numeric(tclvalue(tkget(.cdtEnv$tcl$toolbar$spinV)))
					)
	})

	#########
	## release button1
	tkbind(canvas, "<ButtonRelease>", function(W, x, y){
		if(is.null(.cdtData$EnvData$zoom)) return(NULL)

		ret <- getXYCoords(W, x, y, parPltCrd)

		##Zoom rectangle
		if(tclvalue(.cdtData$EnvData$zoom$pressButRect) == "1"){
			rectZoomInit[2] <<- ret$xc
			rectZoomInit[4] <<- ret$yc
			if(rectZoomInit[1] > rectZoomInit[2]) rectZoomInit <- rectZoomInit[c(2, 1, 3, 4)]
			if(rectZoomInit[3] > rectZoomInit[4]) rectZoomInit <- rectZoomInit[c(1, 2, 4, 3)]
			.cdtData$EnvData$ZoomXYval <- rectZoomInit

			tclvalue(.cdtData$EnvData$zoom$xx1) <- round(rectZoomInit[1], 4)
			tclvalue(.cdtData$EnvData$zoom$xx2) <- round(rectZoomInit[2], 4)
			tclvalue(.cdtData$EnvData$zoom$yy1) <- round(rectZoomInit[3], 4)
			tclvalue(.cdtData$EnvData$zoom$yy2) <- round(rectZoomInit[4], 4)

			refreshPlot(W, img,
						hscale = as.numeric(tclvalue(tkget(.cdtEnv$tcl$toolbar$spinH))),
						vscale = as.numeric(tclvalue(tkget(.cdtEnv$tcl$toolbar$spinV)))
						)
			tkdelete(W, 'rect')
		}

		##Pan image
		if(tclvalue(.cdtData$EnvData$zoom$pressButDrag) == "1"){
			tkconfigure(canvas, cursor = 'hand1')
		}

		tcl('update')
	})

	###############################################
	## deactivate zoom (right button)
	tkbind(canvas, "<Button-3>", function(W){
		if(is.null(.cdtData$EnvData$zoom)) return(NULL)

		tclvalue(.cdtData$EnvData$zoom$pressButP) <- 0
		tclvalue(.cdtData$EnvData$zoom$pressButM) <- 0
		tclvalue(.cdtData$EnvData$zoom$pressButRect) <- 0
		tclvalue(.cdtData$EnvData$zoom$pressButDrag) <- 0

		tkconfigure(.cdtData$EnvData$zoom$btZoomP, relief = 'raised', bg = 'lightblue', state = 'normal')
		tkconfigure(.cdtData$EnvData$zoom$btZoomM, relief = 'raised', bg = 'lightblue', state = 'normal')
		tkconfigure(.cdtData$EnvData$zoom$btZoomRect, relief = 'raised', bg = 'lightblue', state = 'normal')
		tkconfigure(.cdtData$EnvData$zoom$btPanImg, relief = 'raised', bg = 'lightblue', state = 'normal')

		tkconfigure(canvas, cursor = 'crosshair')

		tkdelete(W, 'rect')

		refreshPlot(W, img,
					hscale = as.numeric(tclvalue(tkget(.cdtEnv$tcl$toolbar$spinH))),
					vscale = as.numeric(tclvalue(tkget(.cdtEnv$tcl$toolbar$spinV)))
					)
	})

	###
	return(list(onglet, list(canvas, img)))
}


