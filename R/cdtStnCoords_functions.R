
StnChkCoordsFormatHtml <- function(){
	jsonfile <- file.path(tempdir(), "station_coords.js")
	crds <- .cdtData$EnvData$Maps.Disp
	don <- toJSON(crds, pretty = TRUE)
	don <- c('stncrds_callback({\n', '"stncoord": ', don, '\n});')
	cat(don, file = jsonfile)

	lon.c <- mean(crds$LonX, na.rm = TRUE)
	lat.c <- mean(crds$LatX, na.rm = TRUE)
	lon.b <- range(crds$LonX, na.rm = TRUE)
	lat.b <- range(crds$LatX, na.rm = TRUE)
	nom <- names(crds)
	nom <- nom[!nom %in% c('LonX', 'LatX', 'StatusX', 'idx')]
	titre <- lapply(nom, function(x) paste0("'", x, " : ' + data.", x, " + '\\r'"))
	titre <- do.call(paste, c(titre, sep = " + "))
	contenu <- lapply(nom, function(x) paste0("'", x, " : ' + data.", x, " + '<br>'"))
	contenu <- do.call(paste, c(contenu, sep = " + "))

	html.f <- StnChkCoordsHtml()
	html.f[28] <- paste0("var latlngcentre = new google.maps.LatLng(", lat.c, ", ", lon.c, ");")
	html.f[38] <- paste0("new google.maps.LatLng(", lat.b[1], ", ", lon.b[1], "),")
	html.f[39] <- paste0("new google.maps.LatLng(", lat.b[2], ", ", lon.b[2], ")));")
	html.f[97] <- paste0("var titre = ", titre)
	html.f[98] <- paste0("var contenu = ", contenu)

	return(html.f)
}

StnChkCoordsProxyPass <- function(path, query, ...){
	path <- gsub("^/custom/CDT/", "", path)
	tmpfile <- sprintf("%s%s%s", tempdir(), .Platform$file.sep, path)
	list(file = tmpfile, "content-type" = "text/html", "status code" = 200L)
}

StnChkCoordsBrowse <- function(html.page){
	options(help_type = "html")
	if(!is.HelpServerRunning()) tools::startDynamicHelp()
	env <- get(".httpd.handlers.env", asNamespace("tools"))
	env[["CDT"]] <- StnChkCoordsProxyPass
	port <- ifelse(R.version['svn rev'] < 67550 | getRversion() < "3.2.0", 
					get("httpdPort", envir = environment(startDynamicHelp)),
					tools::startDynamicHelp(NA))
	html.page[42] <- sprintf("script.src = 'http://127.0.0.1:%s/custom/CDT/station_coords.js';", port)
	tmpfile <- file.path(tempdir(), "StationsCoordinates.html")
	cat(html.page, file = tmpfile, sep = "\n")

	html.url <- sprintf("http://127.0.0.1:%s/custom/CDT/%s", port, basename(tmpfile))
	if(interactive()) browseURL(html.url) else browseURL(html.url, browser = 'false')
	invisible()
}
