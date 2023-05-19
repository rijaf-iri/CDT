
## Add new tab on the right panel
addNewTab <- function(tab.title = NULL){
    if(is.null(tab.title)) tab.title <- paste('Tab', infoTabs(), "  ")
    tab <- ttkframe(.cdtEnv$tcl$main$tknotes)
    tkadd(.cdtEnv$tcl$main$tknotes, tab, text = paste(tab.title, "  "))
    tkgrid.columnconfigure(tab, 0, weight = 1)
    tkgrid.rowconfigure(tab, 0, weight = 1)

    ftab <- tkframe(tab, bd = 2, relief = 'sunken', bg = 'white')
    tkgrid(ftab, row = 0, column = 0, sticky = 'nswe')
    tkgrid.columnconfigure(ftab, 0, weight = 1)
    tkgrid.rowconfigure(ftab, 0, weight = 1)

    return(list(tab, ftab))
}

## Count created tabs
infoTabs <- function(){
    open.tabs <- unlist(strsplit(tclvalue(tkwinfo("children", .cdtEnv$tcl$main$tknotes)), " "))
    end.tabs <- as.integer(unlist(strsplit(open.tabs[length(open.tabs)], "\\.")))
    id.tabs <- end.tabs[length(end.tabs)]
    if(length(id.tabs) == 0) id.tabs <- 0
    return(id.tabs + 1)
}

########################################################################

## Display opened data.frame on Tab in a table
Display_data.frame_Table <- function(data.df, title, colwidth = 8){
    onglet <- addNewTab(title)
    dtab <- try(tclArrayVar(data.df), silent = TRUE)
    if(inherits(dtab, "try-error")){
        Insert.Messages.Out(.cdtEnv$tcl$lang$global[['message']][['8']], TRUE, 'e')
        Insert.Messages.Out(gsub('[\r\n]', '', dtab[1]), TRUE, 'e')
        return(list(onglet, list(NULL, NULL)))
    }

    table1 <- try(displayTable(onglet[[2]], tclArray = dtab, colwidth = colwidth), silent = TRUE)
    if(inherits(table1, "try-error")){
        Insert.Messages.Out(.cdtEnv$tcl$lang$global[['message']][['9']], TRUE, 'e')
        Insert.Messages.Out(gsub('[\r\n]', '', table1[1]), TRUE, 'e')
        table1 <- list(NULL, dtab)
    }

    return(list(onglet, table1))
}

#######
## Display interpolation data in a table
DisplayInterpData <- function(data, title, colwidth = '15'){
    onglet <- addNewTab(title)
    dtab <- tclArrayVar(data[[2]])
    table1 <- displayTable(onglet[[2]], tclArray = dtab, colwidth = colwidth)
    return(list(onglet, table1, data[-2]))
}

########################################################################

## Open and display table on new tab
Display_Array_Tab <- function(parent){
    on.exit({
        tkconfigure(.cdtEnv$tcl$main$win, cursor = '')
        tcl('update')
    })
    tkconfigure(.cdtEnv$tcl$main$win, cursor = 'watch')
    tcl('update')

    data.file <- getOpenFiles(parent)
    if(is.null(data.file)) return(NULL)
    update.OpenFiles('ascii', data.file)
    
    tab.array <- Display_data.frame_Table(data.file[[2]], data.file[[1]])
    tab.array <- c(tab.array, data.file[[3]])

    return(tab.array)
}

########################################################################

## Display output console inner frame
Display_Output_Console <- function(parent, out2disp, FUN = print){
    xscr <- tkscrollbar(parent, repeatinterval = 5, orient = "horizontal",
                        command = function(...) tkxview(txta, ...))
    yscr <- tkscrollbar(parent, repeatinterval = 5,
                        command = function(...) tkyview(txta, ...))
    txta <- tktext(parent, bg = "white", font = "courier", wrap = "none", 
                    xscrollcommand = function(...) tkset(xscr, ...),
                    yscrollcommand = function(...) tkset(yscr, ...))
    tkgrid(txta, yscr)
    tkgrid(xscr)
    tkgrid.configure(yscr, sticky = "ns")
    tkgrid.configure(xscr, sticky = "ew")
    tkgrid.configure(txta, sticky = 'nswe')
    tkgrid.rowconfigure(txta, 0, weight = 1)
    tkgrid.columnconfigure(txta, 0, weight = 1)

    menuCopyPaste(txta, scopy = 'normal', scut = 'normal', spaste = 'disabled')

    tempfile <- tempfile()
    sink(tempfile)
    op <- options()
    options(width = 160)
    FUN(out2disp)
    options(op)
    sink()
    rdL <- readLines(tempfile, warn = FALSE)
    for(i in 1:length(rdL)) tkinsert(txta, "end", paste(rdL[i], "\n"))
    unlink(tempfile)
}

## Display output console on tab
Display_Output_Console_Tab <- function(out2disp, title, FUN = print){
    onglet <- addNewTab(title)
    Display_Output_Console(onglet[[2]], out2disp, FUN)
    return(onglet)
}

########################################################################

## Update Open Tab data
update.OpenTabs <- function(type, data){
    ntab <- length(.cdtData$OpenTab$Type)
    .cdtData$OpenTab$Type[[ntab + 1]] <- type
    .cdtData$OpenTab$Data[[ntab + 1]] <- data
    return(ntab)
}

########################################################################

## Close tab

Close_Notebook_Tab <- function(index)
{
    tabid <- as.integer(index) + 1
    if(!is.na(tabid)){
        arrTypes <- c("arr", "chkcrds", "falsezero", "outqc", "outhom")
        if(.cdtData$OpenTab$Type[[tabid]] %in% arrTypes){
            tkdestroy(.cdtData$OpenTab$Data[[tabid]][[1]][[1]])
        }else if(.cdtData$OpenTab$Type[[tabid]] == "ctxt"){
            tkdestroy(.cdtData$OpenTab$Data[[tabid]][[1]])
        }else if(.cdtData$OpenTab$Type[[tabid]] == "img"){
            tkdestroy(.cdtData$OpenTab$Data[[tabid]][[1]][[2]])
            tkdestroy(.cdtData$OpenTab$Data[[tabid]][[1]][[1]])
        }else return(NULL)
        .cdtData$OpenTab$Data[tabid] <- NULL
        .cdtData$OpenTab$Type[tabid] <- NULL
    }else return(NULL)
}

########################################################################

## Save table As
Save_Table_As <- function(){
    on.exit({
        tkconfigure(.cdtEnv$tcl$main$win, cursor = '')
        tcl('update')
    })
    tkconfigure(.cdtEnv$tcl$main$win, cursor = 'watch')
    tcl('update')

    tabid <- as.integer(tclvalue(tkindex(.cdtEnv$tcl$main$tknotes, 'current'))) + 1
    if(!is.na(tabid)){
        Objarray <- .cdtData$OpenTab$Data[[tabid]][[2]]
        if(!inherits(Objarray[[2]], "tclArrayVar")){
            Insert.Messages.Out(.cdtEnv$tcl$lang$global[['message']][['6']], TRUE, 'e')
            return(NULL)
        }
        tryCatch(
            {
                file.to.save <- tk_get_SaveFile(filetypes = .cdtEnv$tcl$data$filetypesA)
                dat2sav <- tclArray2dataframe(Objarray)
                
                file.spec <- NULL
                if(length(.cdtData$OpenTab$Data[[tabid]]) > 2){
                    file.disk <- .cdtData$OpenTab$Data[[tabid]][[3]]
                    if(file.exists(file.disk)){
                        nopfs <- length(.cdtData$OpenFiles$Type)
                        if(nopfs > 0){
                            listOpenFiles <- sapply(1:nopfs, function(j) .cdtData$OpenFiles$Data[[j]][[1]])
                            if(basename(file.disk) %in% listOpenFiles){
                                nopf <- which(listOpenFiles %in% basename(file.disk))
                                file.spec <- .cdtData$OpenFiles$Data[[nopf]][[4]]
                            }
                        }
                    }
                }

                if(!is.null(file.spec)){
                    writeFiles(dat2sav, file.to.save, col.names = file.spec$header,
                                na = file.spec$miss.val, sep = file.spec$sepr)
                }else{
                    writeFiles(dat2sav, file.to.save, col.names = Objarray[[2]]$col.names)
                }
                Insert.Messages.Out(.cdtEnv$tcl$lang$global[['message']][['3']], TRUE, 's')
            },
            warning = function(w){
                Insert.Messages.Out(.cdtEnv$tcl$lang$global[['message']][['2']], TRUE, 'e')
                warningFun(w)
            },
            error = function(e){
                Insert.Messages.Out(.cdtEnv$tcl$lang$global[['message']][['2']], TRUE, 'e')
                errorFun(e)
            }
        )
    }

    invisible()
}

########################################################################

## Save table type "arr"
saveTable.arr.type <- function(dat2sav, tabid, rowcolnames){
    if(is.null(dat2sav)){
        Insert.Messages.Out(.cdtEnv$tcl$lang$global[['message']][['10']], TRUE, 'e')
        return(NULL)
    }
    if(length(.cdtData$OpenTab$Data[[tabid]]) == 2){
        filetosave <- tk_get_SaveFile(filetypes = .cdtEnv$tcl$data$filetypesA)
        writeFiles(dat2sav, filetosave, col.names = TRUE)
    }else{
        filetosave <- .cdtData$OpenTab$Data[[tabid]][[3]]
        file.spec <- NULL
        nopfs <- length(.cdtData$OpenFiles$Type)
        if(nopfs > 0){
            listOpenFiles <- sapply(1:nopfs, function(j) .cdtData$OpenFiles$Data[[j]][[1]])
            if(basename(filetosave) %in% listOpenFiles){
                nopf <- which(listOpenFiles %in% basename(filetosave))
                file.spec <- .cdtData$OpenFiles$Data[[nopf]][[4]]
            }
        }

        if(!is.null(file.spec)){
            writeFiles(dat2sav, filetosave, col.names = file.spec$header,
                       na = file.spec$miss.val, sep = file.spec$sepr)
        }else{
            writeFiles(dat2sav, filetosave, 
                       col.names = rowcolnames$col.names,
                       row.names = rowcolnames$row.names)
        }
    }
}

########################################################################

Save_Notebook_Tab_Array <- function(){
    on.exit({
        tkconfigure(.cdtEnv$tcl$main$win, cursor = '')
        tcl('update')
    })
    tkconfigure(.cdtEnv$tcl$main$win, cursor = 'watch')
    tcl('update')

    tabid <- as.integer(tclvalue(tkindex(.cdtEnv$tcl$main$tknotes, 'current'))) + 1

    if(length(.cdtData$OpenTab$Type) > 0){
        Objarray <- .cdtData$OpenTab$Data[[tabid]][[2]]
        dat2sav <- tclArray2dataframe(Objarray)

        switch(.cdtData$OpenTab$Type[[tabid]],
               "arr" = saveTable.arr.type(dat2sav, tabid,
                            Objarray[[2]][c('row.names', 'col.names')]),
               "chkcrds" = .cdtData$EnvData$StnChkCoords$SaveEdit(dat2sav),
               "falsezero" = .cdtData$EnvData$qcRRZeroCheck$SaveEdit(dat2sav),
               "outqc" = .cdtData$EnvData$QC$SaveEdit(dat2sav),
               "outhom" = .cdtData$EnvData$HomTest$SaveEdit(dat2sav),
               NULL)
    }else return(NULL)

    return(0)
}

