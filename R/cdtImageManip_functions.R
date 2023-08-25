
## Copied and adapted from tkrplot

CountImgIndex <- local({
    k <- 0
    function() {
        k <<- k + 1
        return(k)
    }
})

DisplayPlot <- function(parent, fun, hscale = 1, vscale = 1){
    # on.exit(unlink(tmpfl))

    image <- paste0("RImage", CountImgIndex())

    # tmpfl <- paste0(tempfile(), '.png')
    tmpfl <- file.path(.cdtEnv$tcl$dir, paste0(image, '.png'))

    mul <- if(WindowsOS()) 385 else 480
    opts <- list(filename = tmpfl, width = mul * hscale, height = mul * vscale, type = "cairo")
    if(WindowsOS()) opts <- c(opts, list(restoreConsole = FALSE))
    do.call(grDevices::png, opts)
    ret <- try(fun(), silent = TRUE)
    grDevices::dev.off()
    if(inherits(ret, "try-error")){
        Insert.Messages.Out(gsub('[\r\n]', '', ret[1]), format = TRUE)
        return(NULL)
    }
    if(is.null(ret)) return(NULL)

    tcl('image', 'create', 'photo', image, file = tmpfl)
    lab <- tklabel(parent, image = image)
    tkbind(lab, "<Destroy>", function() tcl('image', 'delete', image))
    lab$image <- image
    lab$fun <- fun
    lab$hscale <- hscale
    lab$vscale <- vscale
    lab$source <- 'cdt'
    return(lab)
}

reDisplayPlot <- function(lab, hscale = lab$hscale, vscale = lab$vscale){
    # on.exit(unlink(tmpfl))

    # tmpfl <- paste0(tempfile(), '.png')
    # image <- paste0("RImage", CountImgIndex())
    # tmpfl <- file.path(.cdtEnv$tcl$dir, paste0(image, '.png'))
    tmpfl <- file.path(.cdtEnv$tcl$dir, paste0(lab$image, '.png'))

    mul <- if(WindowsOS()) 385 else 480
    opts <- list(filename = tmpfl, width = mul * hscale, height = mul * vscale, type = "cairo")
    if(WindowsOS()) opts <- c(opts, list(restoreConsole = FALSE))
    do.call(grDevices::png, opts)
    ret <- try(lab$fun(), silent = TRUE)
    grDevices::dev.off()
    if(inherits(ret, "try-error")){
        Insert.Messages.Out(gsub('[\r\n]', '', ret[1]), format = TRUE)
        return(NULL)
    }
    tcl('image', 'create', 'photo', lab$image, file = tmpfl)
    return(0)
}

refreshPlot <- function(W, img, hscale, vscale){
    ret <- reDisplayPlot(img, hscale = hscale, vscale = vscale)
    if(is.null(ret)) return(NULL)
    img_w <- as.double(tcl('image', 'width', img$image))
    img_h <- as.double(tcl('image', 'height', img$image))
    tkconfigure(W, width = img_w, height = img_h)
    tcl('update')
    return(0)
}

########################################################################

## Draw rectangle above image
## 'rect' the tagged rectangle

pPressRect <- function(W, x, y, ...){
    x <- as.numeric(x)
    y <- as.numeric(y)
    tkdelete(W, 'rect')
    tkcreate(W, "rectangle", x, y, x, y, tag = 'rect', ...)
    .cdtEnv$tcl$lastX <- x
    .cdtEnv$tcl$lastY <- y
}

pMoveRect <- function(W, x, y){
    x <- as.numeric(x)
    y <- as.numeric(y)

    if(x < .cdtEnv$tcl$lastX){
        x1 <- x
        x2 <- .cdtEnv$tcl$lastX
    }else{
        x1 <- .cdtEnv$tcl$lastX
        x2 <- x
    }

    if(y < .cdtEnv$tcl$lastY){
        y1 <- y
        y2 <- .cdtEnv$tcl$lastY
    }else{
        y1 <- .cdtEnv$tcl$lastY
        y2 <- y
    }
    tkcoords(W, 'rect', x1, y1, x2, y2)
}

pReleaseRect <- function(W, rectCrds){
    tclvalue(rectCrds) <- tclvalue(tkcoords(W, 'rect'))
    #tkdelete(W, 'rect')
}

########################################################################

## Cursor movement 
mouseMouvment <- function(W, x, y, parPltCrd, xdiv = c(0, 1), ydiv = c(0, 1)){
    xmouse <- as.numeric(x) + 2
    ymouse <- as.numeric(y) + 2

    imgw <- as.numeric(tclvalue(tkwinfo("reqwidth", W)))
    imgh <- as.numeric(tclvalue(tkwinfo("reqheight", W)))
    imgmw <- as.numeric(tclvalue(tkwinfo("width", W)))
    imgmh <- as.numeric(tclvalue(tkwinfo("height", W)))
    posimgx <- round((imgmw - imgw) / 2)
    posimgy <- round((imgmh - imgh) / 2)
    orgx <- if(posimgx < 0) 0 else posimgx
    orgy <- if(posimgy < 0) 0 else posimgy

    xpos <- (xmouse - orgx) / imgw
    ypos <- 1 - (ymouse - orgy) / imgh

    xplt1 <- as.numeric(tclvalue(parPltCrd$parPlotSize1))
    xplt2 <- as.numeric(tclvalue(parPltCrd$parPlotSize2))
    yplt1 <- as.numeric(tclvalue(parPltCrd$parPlotSize3))
    yplt2 <- as.numeric(tclvalue(parPltCrd$parPlotSize4))
    usrcrd1 <- as.numeric(tclvalue(parPltCrd$usrCoords1))
    usrcrd2 <- as.numeric(tclvalue(parPltCrd$usrCoords2))
    usrcrd3 <- as.numeric(tclvalue(parPltCrd$usrCoords3))
    usrcrd4 <- as.numeric(tclvalue(parPltCrd$usrCoords4))

    minX <- usrcrd1
    rangeX <- usrcrd2 - usrcrd1
    minY <- usrcrd3
    rangeY <- usrcrd4 - usrcrd3

    xposD <- (xpos - xdiv[1]) / (xdiv[2] - xdiv[1])
    xcoord <- minX + (xposD - xplt1) * rangeX / (xplt2 - xplt1)
    yposD <- (ypos - ydiv[1]) / (ydiv[2] - ydiv[1])
    ycoord <- minY + (yposD - yplt1) * rangeY / (yplt2 - yplt1)
    outsideArea <- xcoord < usrcrd1 | xcoord > usrcrd2 | ycoord < usrcrd3 | ycoord > usrcrd4

    return(list(x = xcoord, y = ycoord, inout = outsideArea, xym = list(x = xpos, y = ypos)))
}

########################################################################

## Get coordinates
getXYCoords <- function(W, x, y, parPltCrd) {
    xyMouse <- mouseMouvment(W, x, y, parPltCrd)
    return(list(xc = xyMouse$x, yc = xyMouse$y, oin = xyMouse$inout))
}

########################################################################

## Display lat-lon on status bar
LatLonLabels <- function(xlon, xlat){
    frac <- function(x) abs(x - trunc(x))
    xlon <- if(xlon > 180) -360 + xlon else xlon
    degLo <- abs(trunc(xlon))
    degLa <- abs(trunc(xlat))
    xm <- frac(xlon) * 60
    ym <- frac(xlat) * 60
    minLo <- trunc(xm)
    minLa <- trunc(ym)
    secLo <- round(frac(xm) * 60, 2)
    secLa <- round(frac(ym) * 60, 2)
    sLon <- if(xlon > 0 & xlon < 180) 'E' else if(xlon > -180 & xlon < 0) 'W' else ''
    sLat <- if(xlat > 0) 'N' else if(xlat < 0) 'S' else ''
    degsym <- "\u00b0"
    lon_lab <- paste0(sprintf("%03d", degLo), degsym, sprintf("%02d", minLo), "' ",
               paste0(sprintf("%02d", trunc(secLo)), '.', substr(sprintf("%.2f",
                      frac(secLo)), 3, 4)), '" ', sLon)
    lat_lab <- paste0(sprintf("%03d", degLa), degsym, sprintf("%02d", minLa), "' ",
               paste0(sprintf("%02d", trunc(secLa)), '.', substr(sprintf("%.2f",
                      frac(secLa)), 3, 4)), '" ', sLat)
    return(list(xdisp = lon_lab, ydisp = lat_lab))
}

########################################################################

## Get name of polygon @x, y position
getAdminLabel <- function(xyMouse, parPltCrd = NULL, shp, idField){
    xypts <- data.frame(x = xyMouse$x, y = xyMouse$y)
    sp::coordinates(xypts) <- ~x+y
    admin_name <- sp::over(xypts, shp)
    admin_name <- c(t(admin_name[1, ]))

    if(tclvalue(tkwinfo('exists', idField$ID)) == "1"){
        idx <- as.integer(tclvalue(tcl(idField, 'current'))) + 1
        admin_name <- admin_name[idx]
        labAdmin_name <- if(!is.na(admin_name)) as.character(admin_name) else NULL
    }else labAdmin_name <- NULL
    return(labAdmin_name)
}

## Radius of search
fdispIdStn <- function(x){
    y <- if(x <= 2) 0.0006944444 * x else 0.002777778
    return(y)
}

## Get station name/id
getStnIDLabel <- function(xyMouse, parPltCrd, stn.coords){
    if(is.null(stn.coords)) return(NULL)
    sdist <- (xyMouse$x - stn.coords$lon)^2 + (xyMouse$y - stn.coords$lat)^2
    inear <- which.min(sdist)
    range.usr <- as.numeric(tclvalue(parPltCrd$usrCoords2)) - as.numeric(tclvalue(parPltCrd$usrCoords1))
    rayondisp <- sdist[inear] > fdispIdStn(range.usr)
    frzcoord <- if(xyMouse$inout | rayondisp) NULL else stn.coords$id[inear]
    return(frzcoord)
}

## Get latlon of a pixel
getPixelLatlon <- function(xyMouse, parPltCrd = NULL)
    if(!xyMouse$inout) lapply(xyMouse[c('x', 'y')], round, digits = 6) else NULL

## Return empty char
getEmptyChar <- function(xyMouse, parPltCrd = NULL) return(NULL)

## Display coordinates on status bar
displayCursorPosition3Var <- function(W, x, y, parPltCrd, FUN, ...){
    xyMouse <- mouseMouvment(W, x, y, parPltCrd)

    xydisp <- LatLonLabels(xyMouse$x, xyMouse$y)
    frxcoord <- if(xyMouse$inout) '' else xydisp$xdisp
    frycoord <- if(xyMouse$inout) '' else xydisp$ydisp
    frzcoord <- FUN(xyMouse, parPltCrd, ...)
    if(is.null(frzcoord)) frzcoord <- ''

    tclvalue(.cdtEnv$tcl$status$xcrd) <- frxcoord
    tclvalue(.cdtEnv$tcl$status$ycrd) <- frycoord
    tclvalue(.cdtEnv$tcl$status$zval) <- frzcoord
}

getIDLatLonCoords <- function(W, x, y, parPltCrd, FUN, ...){
    xyMouse <- mouseMouvment(W, x, y, parPltCrd)
    coord <- FUN(xyMouse, parPltCrd, ...)
    plotTS <- if(is.null(coord)) FALSE else TRUE
    if(is.null(coord)) coord <- ''
    return(list(crd = coord, plotTS = plotTS))
}
