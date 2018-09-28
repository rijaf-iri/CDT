
update.OpenFiles <- function(type, data){
	nopf <- length(.cdtData$OpenFiles$Type)
	.cdtData$OpenFiles$Type[[nopf+1]] <- type
	.cdtData$OpenFiles$Data[[nopf+1]] <- data
}

########################################################################

## Open ascii files (data.frame)

openFiles <- function(parent, fileopen){
	if(nchar(fileopen)){
		delimter <- preview.data(parent, fileopen)
		if(!is.null(delimter)){
			dat.file <- try(read.table(fileopen, header = delimter$header, sep = delimter$sepr,
									skip = delimter$skip-1, na.strings = delimter$miss.val, quote = "\"'",
									strip.white = TRUE, stringsAsFactors = FALSE,
									colClasses = "character", comment.char = ""), silent = TRUE)
			# dat.file <- try(data.table::fread(fileopen, header = delimter$header, sep = delimter$sepr,
			# 						skip = delimter$skip-1, na.strings = delimter$miss.val, quote = "\"",
			# 						strip.white = TRUE, stringsAsFactors = FALSE, colClasses = "character",
			# 						showProgress = FALSE, data.table = FALSE), silent = TRUE)

			if(!inherits(dat.file, "try-error")){
				tkinsert(.cdtEnv$tcl$main$Openfiles, "end", basename(fileopen))
				return(list(basename(fileopen), dat.file, fileopen, delimter))
			}else{
				Insert.Messages.Out(paste(.cdtEnv$tcl$lang$global[['message']][['1']], fileopen), format = TRUE)
				Insert.Messages.Out(gsub('[\r\n]', '', dat.file[1]), format = TRUE)
			}
		}
	}

	return(NULL)
}

getOpenFiles <- function(parent, filetype = 'txt', initialdir = getwd()){
	if(filetype == 'txt') filetypes <- .cdtEnv$tcl$data$filetypes1
	if(filetype == 'csv') filetypes <- .cdtEnv$tcl$data$filetypes2
	fileopen <- tclvalue(tkgetOpenFile(initialdir = initialdir, initialfile = "", filetypes = filetypes))
	if(fileopen == "") return(NULL)
	if(length(.cdtData$OpenFiles$Data) > 0){
		existff <- unlist(
						lapply(seq_along(.cdtData$OpenFiles$Data),
						function(j) .cdtData$OpenFiles$Data[[j]][[1]])
					)
		if(basename(fileopen) %in% existff){
			tkmessageBox(message = .cdtEnv$tcl$lang$global[['message']][['4']], icon = "warning", type = "ok")
			return(NULL)
		}
	}
	dat.opfiles <- openFiles(parent, fileopen)
	return(dat.opfiles)
}

########################################################################

## Write files (data.frame)
writeFiles <- function(data2save, file2save,
					row.names = FALSE, col.names = FALSE,
					quote = FALSE, append = FALSE, sep = " ", ...)
{
	extFl <- tolower(str_trim(file_ext(basename(file2save))))
	sep <- if(extFl == "csv") ',' else sep
	if(sep == "") sep <- " "
	write.table(data2save, file = file2save,
				row.names = row.names, col.names = col.names,
				quote = quote, sep = sep, append = append, ...)
}

########################################################################

## Open netcdf files
getOpenNetcdf <- function(parent, initialdir = getwd()){
	fileopen <- tclvalue(tkgetOpenFile(initialdir = initialdir, filetypes = .cdtEnv$tcl$data$filetypes3))
	if(fileopen == "" | is.na(fileopen)) return(NULL)
	if(length(.cdtData$OpenFiles$Data) > 0){
		existff <- unlist(
						lapply(seq_along(.cdtData$OpenFiles$Data),
						function(j) .cdtData$OpenFiles$Data[[j]][[1]])
					)
		if(basename(fileopen) %in% existff){
			tkmessageBox(message = .cdtEnv$tcl$lang$global[['message']][['4']], icon = "warning", type = "ok")
			return(NULL)
		}
	}

	nc.opfiles <- preview.data.nc(parent, fileopen)
	if(!is.null(nc.opfiles)){
		tkinsert(.cdtEnv$tcl$main$Openfiles, "end", basename(fileopen))
		return(list(basename(fileopen), nc.opfiles, fileopen))
	}else return(NULL)
}

########################################################################

## Open ESRI shapefile
getOpenShp <- function(parent){
	# parent doesn't use yet
	fileopen <- tclvalue(tkgetOpenFile(initialdir = getwd(), filetypes = .cdtEnv$tcl$data$filetypes4))
	if(fileopen == "") return(NULL)
	if(length(.cdtData$OpenFiles$Data) > 0){
		existff <- unlist(
						lapply(seq_along(.cdtData$OpenFiles$Data),
						function(j) .cdtData$OpenFiles$Data[[j]][[1]])
					)
		if(basename(fileopen) %in% existff){
			tkmessageBox(message = .cdtEnv$tcl$lang$global[['message']][['4']], icon = "warning", type = "ok")
			return(NULL)
		}
	}

	dsn <- dirname(fileopen)
	layer <- file.sans.ext(basename(fileopen))
	shp.opfiles <- try(readOGR(dsn, layer, verbose = FALSE), silent = TRUE)
	if(inherits(shp.opfiles, "try-error")){
		Insert.Messages.Out(paste(.cdtEnv$tcl$lang$global[['message']][['1']], fileopen), format = TRUE)
		return(NULL)
	}else{
		tkinsert(.cdtEnv$tcl$main$Openfiles, "end", basename(fileopen))
		proj4string(shp.opfiles) <- CRS(as.character(NA))
		return(list(basename(fileopen), shp.opfiles, fileopen))
	}
}

