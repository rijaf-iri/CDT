
## Get Tab containing image display

imageNotebookTab_unik <- function(imgContainer, notebookTab){
	ntab <- length(.cdtData$OpenTab$Type)
	newTab <- TRUE
	if(!is.null(notebookTab)){
		if(ntab > 0){
			AllNoteTab <- sapply(seq(ntab), function(j){
				if(!is.null(attributes(.cdtData$OpenTab$Data[[j]][[1]][[1]])))
					.cdtData$OpenTab$Data[[j]][[1]][[1]]$ID
				else
					.cdtData$OpenTab$Data[[j]][[1]][[1]]
			})
			idTabs <- which(AllNoteTab == notebookTab[[2]])
			if(length(idTabs) > 0) newTab <- FALSE
		}
	}

	if(newTab){
		tabID <- ntab + 1
		.cdtData$OpenTab$Type[[tabID]] <- 'img'
		.cdtData$OpenTab$Data[[tabID]] <- imgContainer
	}else{
		tabID <- idTabs
		.cdtData$OpenTab$Data[[tabID]][[2]] <- imgContainer[[2]]
	}

	ntbkIdTab <- .cdtData$OpenTab$Data[[tabID]][[1]][[1]]$ID
	tkselect(.cdtEnv$tcl$main$tknotes, tabID - 1)

	return(list(imgContainer[[1]], ntbkIdTab))
}

#########################################

## Open new tab if not exist

imageNotebookTab_open <- function(notebookTab, tabTitle){
	newTab <- TRUE
	if(!is.null(notebookTab)){
		ntab <- length(.cdtData$OpenTab$Type)
		if(ntab > 0){
			AllNoteTab <- sapply(1:ntab, function(j){
				if(!is.null(attributes(.cdtData$OpenTab$Data[[j]][[1]][[1]])))
					.cdtData$OpenTab$Data[[j]][[1]][[1]]$ID
				else
					.cdtData$OpenTab$Data[[j]][[1]][[1]]
			})
			idTabs <- which(AllNoteTab == notebookTab[[2]])
			if(length(idTabs) > 0) newTab <- FALSE
		}
	}

	if(!newTab){
		onglet <- notebookTab[[1]]
		tab.child <- tkwinfo('children', .cdtData$OpenTab$Data[[idTabs]][[1]][[2]])
		if(class(.cdtData$OpenTab$Data[[idTabs]][[2]]) == "tkwin"){
			if(tclvalue(tkwinfo('class', tab.child)) == "Label")
				tkdestroy(.cdtData$OpenTab$Data[[idTabs]][[2]])
			else
				tcl('destroy', tab.child)
		}else{
			if(tclvalue(tkwinfo('class', tab.child)) == "Canvas")
				tkdestroy(.cdtData$OpenTab$Data[[idTabs]][[2]][[1]])
			else
				tcl('destroy', tab.child)
		}
		tcl(.cdtEnv$tcl$main$tknotes, 'tab', .cdtData$OpenTab$Data[[idTabs]][[1]][[1]], '-text', tabTitle)
	}else onglet <- addNewTab(tabTitle)

	return(onglet)
}

#########################################

## Display data.frame on unique Tab

tableNotebookTab_unik <- function(data.df, notebookTab, title,
								colwidth = 8, table.type = 'arr')
{
	ntab <- length(.cdtData$OpenTab$Type)
	newTable <- TRUE
	if(!is.null(notebookTab)){
		if(ntab > 0){
			AllNoteTab <- sapply(seq(ntab), function(j){
				if(!is.null(attributes(.cdtData$OpenTab$Data[[j]][[1]][[1]])))
					.cdtData$OpenTab$Data[[j]][[1]][[1]]$ID
				else
					.cdtData$OpenTab$Data[[j]][[1]][[1]]
			})
			idTabs <- which(AllNoteTab == notebookTab)
			if(length(idTabs) > 0) newTable <- FALSE
		}
	}

	if(newTable){
		tabID <- ntab + 1
		tableDisp <- Display_data.frame_Table(data.df, title, colwidth)
		.cdtData$OpenTab$Type[[tabID]] <- table.type
		.cdtData$OpenTab$Data[[tabID]] <- tableDisp
	}else{
		tabID <- idTabs
		.Tcl(paste('destroy', tclvalue(tkwinfo("children", .cdtData$OpenTab$Data[[tabID]][[1]][[2]]))))
		dtab <- try(tclArrayVar(data.df), silent = TRUE)
		if(inherits(dtab, "try-error")){
			Insert.Messages.Out("Unable to create tclArrayVar", format = TRUE)
			Insert.Messages.Out(gsub('[\r\n]', '', dtab[1]), format = TRUE)
			return(notebookTab)
		}
		table1 <- try(displayTable(.cdtData$OpenTab$Data[[tabID]][[1]][[2]], dtab, colwidth), silent = TRUE)
		if(inherits(table1, "try-error")){
			Insert.Messages.Out("Unable to display table", format = TRUE)
			Insert.Messages.Out(gsub('[\r\n]', '', table1[1]), format = TRUE)
			return(notebookTab)
		}
		.cdtData$OpenTab$Data[[tabID]][[2]] <- table1
		tcl(.cdtEnv$tcl$main$tknotes, 'tab', .cdtData$OpenTab$Data[[tabID]][[1]][[1]], '-text', title)
	}

	ntbkIdTab <- .cdtData$OpenTab$Data[[tabID]][[1]][[1]]$ID
	tkselect(.cdtEnv$tcl$main$tknotes, tabID - 1)

	return(ntbkIdTab)
}

