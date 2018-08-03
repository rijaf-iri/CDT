
PlotNetCDFdataMaps <- function(){
	don <- .cdtData$EnvData$ncData$map
	dataMapOp <- .cdtData$EnvData$ncMapOp

	## titre
	if(!dataMapOp$title$user){
		.titre <- .cdtData$EnvData$ncData$file2plot
	}else .titre <- dataMapOp$title$title

	#################

	.data.type <- .cdtData$EnvData$plot.maps$.data.type
	.plot.type <- str_trim(tclvalue(.cdtData$EnvData$plot.maps$plot.type))
	map.args <- cdt.plotmap.args(don, dataMapOp, .cdtData$EnvData$shp)

	opar <- par(mar = map.args$mar)
	map.args.add <- list(titre = .titre,
						SHPOp = .cdtData$EnvData$SHPOp,
						# MapOp = dataMapOp,
						data.type = .data.type,
						plot.type = .plot.type)
	map.args <- map.args[!(names(map.args) %in% "mar")]
	map.args <- c(map.args, map.args.add)
	par.plot <- do.call(cdt.plotmap.fun, map.args)

	## scale bar
	cdt.plotmap.scalebar(dataMapOp$scalebar)

	par(opar)
	invisible()
}
