
grads_create.ctl_Procs <- function(){
	daty1 <- as.Date(paste(.cdtData$GalParams$date$year1,
							.cdtData$GalParams$date$mon1,
							.cdtData$GalParams$date$day1, sep = "-"))
	daty2 <- as.Date(paste(.cdtData$GalParams$date$year2,
							.cdtData$GalParams$date$mon2,
							.cdtData$GalParams$date$day2, sep = "-"))

	filename0 <- gsub("%Y|%M|%D|%T", "%s", .cdtData$GalParams$nc$format)
	filename <- gsub("%D", "%d2", gsub("%M", "%m2", gsub("%Y", "%y4", .cdtData$GalParams$nc$format)))
	DSET <- paste("DSET", file.path(.cdtData$GalParams$nc$dir, filename))

	if(.cdtData$GalParams$tstep == "daily"){
		daty <- seq(daty1, daty2, "day")
		year <- format(daty, "%Y")
		mon <- format(daty, "%m")
		day <- format(daty, "%d")
		ncfiles <- sprintf(filename0, year, mon, day)
		start <- daty[1]
		increment <- "1dy"
	}

	if(.cdtData$GalParams$tstep == "dekadal"){
		daty <- seq(daty1, daty2, "day")
		daty <- daty[as.numeric(format(daty, "%d")) <= 3]
		year <- format(daty, "%Y")
		mon <- format(daty, "%m")
		dek <- as.numeric(format(daty, "%d"))
		ncfiles <- sprintf(filename0, year, mon, dek)
		start <- switch(as.character(dek[1]), "1" = 1, "2" = 11, "3" = 21)
		start <- as.Date(paste0(format(daty[1], "%Y-%m-"), start))
		increment <- "10dy"
		DSET <- paste("DSET", file.path(.cdtData$GalParams$nc$dir, "%ch"))
		CHSUB <- paste("CHSUB", seq_along(ncfiles), seq_along(ncfiles), ncfiles, collapse = "\n")
		DSET <- paste(DSET, CHSUB, sep = "\n")
	}

	if(.cdtData$GalParams$tstep == "monthly"){
		daty <- seq(daty1, daty2, "month")
		year <- format(daty, "%Y")
		mon <- format(daty, "%m")
		ncfiles <- sprintf(filename0, year, mon)
		start <- daty[1]
		increment <- "1mo"
	}

	if(.cdtData$GalParams$tstep == "annual"){
		daty <- seq(daty1, daty2, "year")
		year <- format(daty, "%Y")
		ncfiles <- sprintf(filename0, year)
		start <- daty[1]
		increment <- "1yr"
	}

	ncpath <- file.path(.cdtData$GalParams$nc$dir, ncfiles)
	ncexist <- file.exists(ncpath)
	if(!any(ncexist)){
		Insert.Messages.Out(GalParams[['message']][['6']], format = TRUE)
		return(NULL)
	}

	start <- paste0(as.numeric(format(start, "%d")), tolower(format(start, "%b")), as.numeric(format(start, "%Y")))
	TDEF <- paste("TDEF", length(daty), "LINEAR", start, increment)

	ncdataInfo <- getNCDFSampleData(.cdtData$GalParams$nc$sample)
	if(is.null(ncdataInfo)){
		Insert.Messages.Out(GalParams[['message']][['7']], format = TRUE)
		return(NULL)
	}

	nc <- nc_open(ncpath[ncexist][1])
	nc.varid <- ncdataInfo$varid
	lon <- nc$var[[nc.varid]]$dim[[ncdataInfo$ilon]]$vals
	lat <- nc$var[[nc.varid]]$dim[[ncdataInfo$ilat]]$vals
	nc.units <- nc$var[[nc.varid]]$units
	# nc.prec <- nc$var[[nc.varid]]$prec
	nc.missval <- nc$var[[nc.varid]]$missval
	nc.longname <- nc$var[[nc.varid]]$longname
	nc_close(nc)

	TITLE <- paste("TITLE", nc.longname)
	UNDEF <- paste("UNDEF", nc.missval)
	XDEF <- paste("XDEF", length(lon), "LINEAR", lon[1], lon[2]-lon[1])
	YDEF <- paste("YDEF", length(lat), "LINEAR", lat[1], lat[2]-lat[1])
	VAR <- paste(paste0(nc.varid, "=>vars"), 0, "y,x", nc.longname)

	CTL <- paste(DSET, "OPTIONS template", TITLE, "DTYPE netcdf", UNDEF, XDEF, YDEF,
				"ZDEF 1 LINEAR 1 1", TDEF, "VARS 1", VAR, "ENDVARS", sep = "\n")
	cat(CTL, file = .cdtData$GalParams$out.ctl)

	return(0)
}
