
## Get Tab containing image display
imageNotebookTab_unik <- function(imgContainer, notebookTab){
	ntab <- length(.cdtData$OpenTab$Type)
	if(!is.null(notebookTab)){
		if(ntab > 0){
			AllNoteTab <- sapply(1:ntab, function(j){
				if(!is.null(attributes(.cdtData$OpenTab$Data[[j]][[1]][[1]])))
					.cdtData$OpenTab$Data[[j]][[1]][[1]]$ID
				else
					.cdtData$OpenTab$Data[[j]][[1]][[1]]
			})
			idTabs <- which(AllNoteTab == notebookTab[[2]])
			if(length(idTabs) > 0){
				ntbkIdTab <- .cdtData$OpenTab$Data[[idTabs]][[1]][[1]]$ID
				.cdtData$OpenTab$Data[[idTabs]][[2]] <- imgContainer[[2]]
				tkselect(.cdtEnv$tcl$main$tknotes, idTabs - 1)
			}else{
				.cdtData$OpenTab$Type[[ntab+1]] <- 'img'
				.cdtData$OpenTab$Data[[ntab+1]] <- imgContainer
				ntbkIdTab <- .cdtData$OpenTab$Data[[ntab+1]][[1]][[1]]$ID
				tkselect(.cdtEnv$tcl$main$tknotes, ntab)
			}
		}else{
			.cdtData$OpenTab$Type[[1]] <- 'img'
			.cdtData$OpenTab$Data[[1]] <- imgContainer
			ntbkIdTab <- .cdtData$OpenTab$Data[[1]][[1]][[1]]$ID
			tkselect(.cdtEnv$tcl$main$tknotes, 0)
		}
	}else{
		.cdtData$OpenTab$Type[[ntab+1]] <- 'img'
		.cdtData$OpenTab$Data[[ntab+1]] <- imgContainer
		ntbkIdTab <- .cdtData$OpenTab$Data[[ntab+1]][[1]][[1]]$ID
		tkselect(.cdtEnv$tcl$main$tknotes, ntab)
	}
	return(list(imgContainer[[1]], ntbkIdTab))
}

#########################################

## Open new tab if not exist

imageNotebookTab_open <- function(notebookTab, tabTitle){
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

			if(length(idTabs) > 0){
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
		}else onglet <- addNewTab(tabTitle)
	}else onglet <- addNewTab(tabTitle)
	return(onglet)
}
