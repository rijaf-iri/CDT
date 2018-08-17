
## gstat block size
createBlock <- function(cellsize, fac = 0.5, len = 4){
	sDX <- cellsize[1]*fac
	dBX <- seq(-sDX, sDX, length.out = len)
	sDY <- cellsize[2] * fac
	dBY <- seq(-sDY, sDY, length.out = len)
	bGrd <- expand.grid(x = dBX, y = dBY)
	return(bGrd)
}

## create grid for interpolation 

createGrid.merging <- function(grdData, ObjVar = NULL, coarse.grid = TRUE, res.coarse = 0.25){
	gridS <- data.frame(expand.grid(lon = grdData$x,
									lat = grdData$y),
						dem = c(grdData$z),
						slp = c(grdData$slp),
						asp = c(grdData$asp))
	coordinates(gridS) <- ~lon+lat

	gridS1 <- NULL
	gridS2 <- NULL
	idcoarse <- NULL
	idcoarse1 <- NULL

	if(coarse.grid){
		idcoarse <- indexCoarseGrid(grdData$x, grdData$y, res.coarse)
		gridS1 <- data.frame(expand.grid(lon = grdData$x[idcoarse$ix],
										lat = grdData$y[idcoarse$iy]),
							dem = c(grdData$z[idcoarse$ix, idcoarse$iy]),
							slp = c(grdData$slp[idcoarse$ix, idcoarse$iy]),
							asp = c(grdData$asp[idcoarse$ix, idcoarse$iy]))
		coordinates(gridS1) <- ~lon+lat
		if(!is.null(ObjVar)){
			idcoarse1 <- indexCoarseGrid(ObjVar$x, ObjVar$y, res.coarse)
			gridS2 <- data.frame(expand.grid(lon = ObjVar$x[idcoarse1$ix],
											lat = ObjVar$y[idcoarse1$iy]),
								dem = c(ObjVar$z[idcoarse1$ix, idcoarse1$iy]),
								slp = c(ObjVar$slp[idcoarse1$ix, idcoarse1$iy]),
								asp = c(ObjVar$asp[idcoarse1$ix, idcoarse1$iy]))
			coordinates(gridS2) <- ~lon+lat
		}
	}

	max.slope <- max(gridS$slp, na.rm = TRUE)
	max.aspect <- max(gridS$asp, na.rm = TRUE)
	min.dem <- min(gridS$dem, na.rm = TRUE)
	max.dem <- max(gridS$dem, na.rm = TRUE)

	gridS$dem <- (gridS$dem - min.dem) / (max.dem - min.dem)
	gridS$slp <- gridS$slp / max.slope
	gridS$asp <- gridS$asp / max.aspect

	if(!is.null(gridS1)){
		gridS1$dem <- (gridS1$dem - min.dem) / (max.dem - min.dem)
		gridS1$slp <- gridS1$slp / max.slope
		gridS1$asp <- gridS1$asp / max.aspect
	}
	if(!is.null(gridS2)){
		gridS2$dem <- (gridS2$dem - min.dem) / (max.dem - min.dem)
		gridS2$slp <- gridS2$slp / max.slope
		gridS2$asp <- gridS2$asp / max.aspect
	}

	return(list(newgrid = gridS, coords.coarse = gridS1, coords.var = gridS2,
				id.coarse = idcoarse, id.var = idcoarse1))
}

