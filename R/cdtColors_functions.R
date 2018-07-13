
## Gradient Color

reScaleC <- function(x, newrange){
	xrange <- range(x)
	if(xrange[1] == xrange[2]) return(x)
	mfac <- (newrange[2] - newrange[1]) / (xrange[2] - xrange[1])
	retScaled <- newrange[1] + (x - xrange[1]) * mfac
	return(retScaled)
}

getGradientColor <- function(listCol, cW){
	ncolors <- length(cW)
	xrange <- range(cW)
	rgbC <- col2rgb(listCol)
	rouge <- rgbC[1, ]
	verte <- rgbC[2, ]
	bleue <- rgbC[3, ]
	nCl <- length(rouge)
	if(nCl > 1){
		rEd <- rep(rouge[nCl], ncolors)
		gEd <- rep(verte[nCl], ncolors)
		bEd <- rep(bleue[nCl], ncolors)
		xstart <- xrange[1]
		xinc <- diff(xrange) / (nCl - 1)
		for(seg in seq(nCl - 1)){
		   segindex <- which((cW >= xstart) & (cW <= (xstart + xinc)))
		   rEd[segindex] <- reScaleC(cW[segindex], rouge[c(seg, seg + 1)])
		   gEd[segindex] <- reScaleC(cW[segindex], verte[c(seg, seg + 1)])
		   bEd[segindex] <- reScaleC(cW[segindex], bleue[c(seg, seg + 1)])
		   xstart <- xstart + xinc
		}
		rEd <- ifelse(rEd < 0, 0, rEd)
		rEd <- ifelse(rEd > 255, 255, rEd)
		rEd <- as.integer(rEd)
		gEd <- ifelse(gEd < 0, 0, gEd)
		gEd <- ifelse(gEd > 255, 255, gEd)
		gEd <- as.integer(gEd)
		bEd <- ifelse(bEd < 0, 0, bEd)
		bEd <- ifelse(bEd > 255, 255, bEd)
		bEd <- as.integer(bEd)
	}else{
		rEd <- rep(rouge, ncolors)
		gEd <- rep(verte, ncolors)
		bEd <- rep(bleue, ncolors)
	}
	gradientColor <- paste0('#', sprintf("%2.2x", rEd), sprintf("%2.2x", gEd), sprintf("%2.2x", bEd))
	return(gradientColor)
}

########################################################

## SPI Color
spi.colors <- function(n = 8){
	kolor <- c('#B12126', '#CF661C', '#F8DDB3', '#FEFFFF',
				'#FEFFFF', '#99F99C', '#3DB272', '#157040')
	if(n == 8) return(kolor)
	kolorFonction <- colorRampPalette(kolor)
	kolorFonction(n)
}

## IMERG precip colors
precip.colors <- function(n = 12){
	kolor <- c("#FFFFFF00", "#396892", "#358ADB", "#83E01D",
				"#D8D71D", "#C08A27", "#D55423", "#C3272B",
				"#953130", "#963295", "#8840CF", "#7E6AB1")
	if(n == 12) return(kolor)
	kolorFonction <- colorRampPalette(kolor)
	kolorFonction(n)
}

## Decile colors
decile.colors <- function(n = 8){
	kolor <- c('#8B2323', '#BD5B1C', '#F28E43', '#FCE9DA',
				'#EBF3DE', '#BACCE4', '#608CC4', '#3B638F')
	if(n == 8) return(kolor)
	kolorFonction <- colorRampPalette(kolor)
	kolorFonction(n)
}


