
ExecDownload_GADM <- function(){
	on.exit({
		tkconfigure(.cdtEnv$tcl$main$win, cursor = '')
		tcl('update')
	})
	tkconfigure(.cdtEnv$tcl$main$win, cursor = 'watch')
	tcl('update')

	baseURL <- 'http://biogeo.ucdavis.edu/data/gadm2.8/rds/'
	layer <- paste0(.cdtData$GalParams$cntr_iso3, '_adm', .cdtData$GalParams$shp$level)
	urlfl <- paste0(baseURL, layer, '.rds')
	destfl <- paste0(tempfile(), '.rds')
	ret <- try(download.file(urlfl, destfl, method = "auto", quiet = TRUE, mode = "wb", cacheOK = TRUE), silent = TRUE)
	if(inherits(ret, "try-error")){
		Insert.Messages.Out(.cdtData$GalParams[['message']][['5']], format = TRUE)
		Insert.Messages.Out(gsub('[\r\n]', '', ret[1]), format = TRUE)
		return(NULL)
	}else{
		if(ret != 0){
			Insert.Messages.Out(.cdtData$GalParams[['message']][['5']], format = TRUE)
			return(NULL)
		}
	}

	shp <- readRDS(destfl)
	varname <- switch(.cdtData$GalParams$shp$level,
					'0' = c("OBJECTID", "ID_0", "ISO", "NAME_ISO"),
					'1' = c("OBJECTID", "ID_0", "ID_1", "ISO", "NAME_0", "NAME_1", "ENGTYPE_1"),
					'2' = c("OBJECTID", "ID_0", "ID_1", "ID_2", "ISO", "NAME_0", "NAME_1", "NAME_2", "ENGTYPE_2"),
					'3' = c("OBJECTID", "ID_0", "ID_1", "ID_2", "ID_3", "ISO", "NAME_0", "NAME_1", "NAME_2", "NAME_3", "ENGTYPE_3"),
					'4' = c("OBJECTID", "ID_0", "ID_1", "ID_2", "ID_3", "ID_4", "ISO", "NAME_0", "NAME_1", "NAME_2", "NAME_3", "NAME_4", "ENGTYPE_4")
				)
	shp <- shp[, varname]

	writeOGR(shp, dsn = .cdtData$GalParams$dir2save, layer = layer, driver = "ESRI Shapefile")
	unlink(destfl)
	Insert.Messages.Out(.cdtData$GalParams[['message']][['4']])
	return(0)
}
