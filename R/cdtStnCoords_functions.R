
StnChkCoordsFormatHtml <- function(){
    jsonfile <- file.path(tempdir(), "station_coords.json")
    crds <- .cdtData$EnvData$Maps.Disp
    ###
    ii <- switch(.cdtData$EnvData$output$params$data.type, "cdtstation" = 2:3, 3:4)
    nom0 <- names(crds)
    nom1 <- c(nom0[ii], 'LonX', 'LatX', 'StatusX')
    inom <- !nom0 %in% nom1

    xcrd <- crds[, c('LonX', 'LatX'), drop = FALSE]
    xcrd <- paste(xcrd[, 1], xcrd[, 2], sep = "_")
    ix1 <- duplicated(xcrd) & !is.na(crds$LonX)
    ix2 <- duplicated(xcrd, fromLast = TRUE) & !is.na(crds$LonX)
    ix <- ix1 | ix2
    icrd <- unique(xcrd[ix])
    if(length(icrd) > 0){
        for(jj in icrd){
            ic <- xcrd == jj
            xx <- apply(crds[ic, inom, drop = FALSE], 2, paste0, collapse = " | ")
            xx <- matrix(xx, nrow = 1, dimnames = list(NULL, names(xx)))
            xx <- do.call(rbind, lapply(seq_along(which(ic)), function(i) xx))
            crds[ic, inom] <- xx
        }
    }
    ###
    don <- toJSON(crds, pretty = TRUE)
    cat(don, file = jsonfile)

    lon.c <- mean(crds$LonX, na.rm = TRUE)
    lat.c <- mean(crds$LatX, na.rm = TRUE)
    nom <- names(crds)
    nom <- nom[!nom %in% c('LonX', 'LatX', 'StatusX', 'idx')]
    contenu <- lapply(nom, function(x) paste0("'<b>", x, " : </b>' + this.", x, " + '<br>'"))
    contenu <- do.call(paste, c(contenu, sep = " + "))

    html.f <- StnChkCoordsHtml()
    html.f[44] <- paste0("center: [", lat.c, ", ", lon.c, "],")
    html.f[100] <- paste0("var contenu = ", contenu, ";")
    if(!is.null(.cdtData$Config$Google.Maps.API.key)){
        html.f[9] <- paste0('<script src="https://maps.googleapis.com/maps/api/js?key=',
                              .cdtData$Config$Google.Maps.API.key,
                              '" async defer></script>')
    }

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
                   get("httpdPort", envir = environment(tools::startDynamicHelp)),
                   tools::startDynamicHelp(NA))
    html.page[41] <- sprintf('var serverPath = "http://127.0.0.1:%s/custom/CDT/";', port)
    tmpfile <- file.path(tempdir(), "StationsCoordinates.html")
    cat(html.page, file = tmpfile, sep = "\n")

    markers <- c('marker-shadow.png', 'marker-icon-blue.png',
                 'marker-icon-orange.png', 'marker-icon-red.png',
                 'marker-icon-green.png')
    file.copy(file.path(.cdtDir$Root, 'images', markers),
              file.path(tempdir(), markers), overwrite = TRUE)

    html.url <- sprintf("http://127.0.0.1:%s/custom/CDT/%s", port, basename(tmpfile))
    if(interactive())
        utils::browseURL(html.url)
    else return(NULL)
    invisible()
}
