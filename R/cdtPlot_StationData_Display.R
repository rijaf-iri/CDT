
plotCDTStation.Maps <- function(){
	don <- .cdtData$EnvData$stndata$map
	dataMapOp <- .cdtData$EnvData$dataMapOp

	## titre
	if(!dataMapOp$title$user){
		.titre <- paste("Observation:", don$t)
	}else .titre <- dataMapOp$title$title

	#################

	map.args <- cdt.plotmap.args(don, dataMapOp, .cdtData$EnvData$shp)
	opar <- par(mar = map.args$mar)

	map.args.add <- list(titre = .titre,
						SHPOp = .cdtData$EnvData$SHPOp,
						MapOp = dataMapOp,
						data.type = "Points",
						plot.type = don$p)
	map.args <- map.args[!(names(map.args) %in% "mar")]
	map.args <- c(map.args, map.args.add)
	par.plot <- do.call(cdt.plotmap.fun, map.args)

	## scale bar
	cdt.plotmap.scalebar(dataMapOp$scalebar)

	par(opar)

	return(par.plot)
}

#######################################

plotCDTStation.Graph <- function(){
	TSGraphOp <- .cdtData$EnvData$TSGraphOp
	daty <- .cdtData$EnvData$tsdates
	don <- .cdtData$EnvData$stndata$series$ts
	timestep <- .cdtData$EnvData$tstep

	titre <- paste("Station:", .cdtData$EnvData$stndata$series$id)
	location <- paste0("Station: ", .cdtData$EnvData$stndata$series$id)

	#######

	GRAPHTYPE <- str_trim(tclvalue(.cdtData$EnvData$plot.maps$typeTSp))
	if(GRAPHTYPE == "Line") optsgph <- TSGraphOp$line
	if(GRAPHTYPE == "Barplot") optsgph <- TSGraphOp$bar

	xlim <- range(daty, na.rm = TRUE)
	if(timestep != "others"){
		if(optsgph$xlim$is.min){
			xx <- strsplit(optsgph$xlim$min, "-")[[1]]
			x3 <- as.numeric(xx[3])
			if(timestep == "pentad"){
				if(is.na(x3) | x3 < 1 | x3 > 6){
					Insert.Messages.Out("xlim: pentad must be  between 1 and 6", format = TRUE)
					return(NULL)
				}
				x3 <- c(1, 6, 11, 16, 21, 26)[x3]
			}
			if(timestep == "dekadal"){
				if(is.na(x3) | x3 < 1 | x3 > 3){
					Insert.Messages.Out("xlim: dekad must be 1, 2 or 3", format = TRUE)
					return(NULL)
				}
				x3 <- c(1, 11, 21)[x3]
			}
			if(timestep == "monthly") x3 <- 1
			x1 <- as.numeric(xx[1])
			x2 <- str_pad(as.numeric(xx[2]), 2, pad = "0")
			x3 <- str_pad(x3, 2, pad = "0")
			xx <- as.Date(paste0(x1, x2, x3), "%Y%m%d")
			if(is.na(xx)){
				Insert.Messages.Out("xlim: invalid date", format = TRUE)
				return(NULL)
			}
			xlim[1] <- xx
		}
		if(optsgph$xlim$is.max){
			xx <- strsplit(optsgph$xlim$max, "-")[[1]]
			x3 <- as.numeric(xx[3])
			if(timestep == "pentad"){
				if(is.na(x3) | x3 < 1 | x3 > 6){
					Insert.Messages.Out("xlim: pentad must be  between 1 and 6", format = TRUE)
					return(NULL)
				}
				x3 <- c(1, 6, 11, 16, 21, 26)[x3]
			}
			if(timestep == "dekadal"){
				if(is.na(x3) | x3 < 1 | x3 > 3){
					Insert.Messages.Out("xlim: dekad must be 1, 2 or 3", format = TRUE)
					return(NULL)
				}
				x3 <- c(1, 11, 21)[x3]
			}
			if(timestep == "monthly") x3 <- 1
			x1 <- as.numeric(xx[1])
			x2 <- str_pad(as.numeric(xx[2]), 2, pad = "0")
			x3 <- str_pad(x3, 2, pad = "0")
			xx <- as.Date(paste0(x1, x2, x3), "%Y%m%d")
			if(is.na(xx)){
				Insert.Messages.Out("xlim: invalid date", format = TRUE)
				return(NULL)
			}
			xlim[2] <- xx
		}
	}else{
		if(optsgph$xlim$is.min) xlim[1] <- as.numeric(optsgph$xlim$min)
		if(optsgph$xlim$is.max) xlim[2] <- as.numeric(optsgph$xlim$max)
	}

	idt <- daty >= xlim[1] & daty <= xlim[2]
	daty <- daty[idt]
	don <- don[idt]
	ylim <- range(pretty(don))
	if(optsgph$ylim$is.min) ylim[1] <- optsgph$ylim$min
	if(optsgph$ylim$is.max) ylim[2] <- optsgph$ylim$max

	xlab <- if(optsgph$axislabs$is.xlab) optsgph$axislabs$xlab else ''
	ylab <- if(optsgph$axislabs$is.ylab) optsgph$axislabs$ylab else ''

	if(optsgph$title$is.title){
		titre <- optsgph$title$title
		titre.pos <- optsgph$title$position
	}else{
		titre <- titre
		titre.pos <- "top"
	}

	#######

	if(GRAPHTYPE == "Line"){
		graphs.plot.line(daty, don, xlim = xlim, ylim = ylim,
						xlab = xlab, ylab = ylab, ylab.sub = NULL,
						title = titre, title.position = titre.pos, axis.font = 1,
						plotl = optsgph$plot, legends = NULL,
						location = location)
	}

	if(GRAPHTYPE == "Barplot"){
		graphs.plot.bar(daty, don, xlim = xlim, ylim = ylim, origindate = NULL,
						xlab = xlab, ylab = ylab, ylab.sub = NULL,
						title = titre, title.position = titre.pos, axis.font = 1,
						barcol = optsgph$colors$col,
						location = location)
	}
}

####################################################################################

CDTdataStation.Display.Maps <- function(){
	if(is.null(.cdtData$EnvData)) return(NULL)
	if(is.null(.cdtData$EnvData$stndata)) return(NULL)

	titre <- paste('Map -', .cdtData$EnvData$stndata$map$t)
	imgContainer <- CDT.Display.Map.inter(plotCDTStation.Maps, .cdtData$EnvData$tab$dataMap, titre)
	.cdtData$EnvData$tab$dataMap <- imageNotebookTab_unik(imgContainer, .cdtData$EnvData$tab$dataMap)

	###############
	tkbind(imgContainer[[2]], "<Button-1>", function(W, x, y){
		xyid <- getIDLatLonCoords(W, x, y, imgContainer[[3]], getStnIDLabel,
						stn.coords = .cdtData$EnvData$plot.maps[c('lon', 'lat', 'id')])

		if(xyid$plotTS){
			tclvalue(.cdtData$EnvData$plot.maps$stnIDTSp) <- xyid$crd

			istn <- which(.cdtData$EnvData$don$id == str_trim(tclvalue(.cdtData$EnvData$plot.maps$stnIDTSp)))
			if(length(istn) == 0){
				.cdtData$EnvData$stndata$series <- NULL
				Insert.Messages.Out(paste(str_trim(tclvalue(.cdtData$EnvData$plot.maps$stnIDTSp)), "doesn't exist"), format = TRUE)
			}else{
				.cdtData$EnvData$stndata$series$ts <- .cdtData$EnvData$don$data[, istn]
				.cdtData$EnvData$stndata$series$id <- str_trim(tclvalue(.cdtData$EnvData$plot.maps$stnIDTSp))
			}

			titre1 <- paste('Station -', .cdtData$EnvData$stndata$series$id)
			imgContainer1 <- CDT.Display.Graph(plotCDTStation.Graph, .cdtData$EnvData$tab$dataGraph, titre1)
			.cdtData$EnvData$tab$dataGraph <- imageNotebookTab_unik(imgContainer1, .cdtData$EnvData$tab$dataGraph)
		}
	})
}
