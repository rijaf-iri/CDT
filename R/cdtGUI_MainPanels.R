
cdtLeftPanel <- function(){
    frame.opfiles <- ttklabelframe(.cdtEnv$tcl$main$panel.left, text = .cdtEnv$tcl$lang$global[['label']][['1']],
                                   relief = 'groove', width = .cdtEnv$tcl$data$wpanel.left)
    .cdtEnv$tcl$main$cmd.frame <- tkframe(.cdtEnv$tcl$main$panel.left, relief = 'groove', bd = 2)

    ### List open files 
    scr.opfiles <- tkscrollbar(frame.opfiles, repeatinterval = 5,
                    command = function(...) tkyview(.cdtEnv$tcl$main$Openfiles, ...))
    .cdtEnv$tcl$main$Openfiles <- tklistbox(frame.opfiles, selectmode = "single",
                            height = 5, width = .cdtEnv$tcl$data$w.opfiles,
                            selectbackground = "yellow", selectforeground = "blue", background = "white",
                            yscrollcommand = function(...) tkset(scr.opfiles, ...))
    tkgrid(.cdtEnv$tcl$main$Openfiles, row = 0, column = 0, sticky = "nwe")
    tkgrid(scr.opfiles, row = 0, column = 1, rowspan = 4, sticky = "ns")

    ###
    tkgrid(frame.opfiles, sticky = 'nwe')
    tkgrid.columnconfigure(frame.opfiles, 0, weight = 1)

    tkgrid(.cdtEnv$tcl$main$cmd.frame, sticky = 'nwe')

    ####################################

    .cdtEnv$tcl$main$menu.opfiles <- tkmenu(.cdtEnv$tcl$main$Openfiles, tearoff = FALSE)
    tkadd(.cdtEnv$tcl$main$menu.opfiles, "command", label = .cdtEnv$tcl$lang$global[['label']][['2']], command = function(){
        id.active <- as.integer(tclvalue(tkcurselection(.cdtEnv$tcl$main$Openfiles))) + 1
        tkdelete(.cdtEnv$tcl$main$Openfiles, id.active - 1)
        .cdtData$OpenFiles$Type[id.active] <- NULL
        .cdtData$OpenFiles$Data[id.active] <- NULL
    })
    tkadd(.cdtEnv$tcl$main$menu.opfiles, "separator")
    tkadd(.cdtEnv$tcl$main$menu.opfiles, "command", label = .cdtEnv$tcl$lang$global[['label']][['3']], command = function(){
        on.exit({
            tkconfigure(.cdtEnv$tcl$main$win, cursor = '')
            tcl('update')
        })
        tkconfigure(.cdtEnv$tcl$main$win, cursor = 'watch')
        tcl('update')

        nopf <- as.integer(tclvalue(tkcurselection(.cdtEnv$tcl$main$Openfiles))) + 1
        type.file <- .cdtData$OpenFiles$Type[[nopf]]
        if(length(type.file) > 0){
            if(type.file == 'ascii'){
                title <- .cdtData$OpenFiles$Data[[nopf]][[1]]
                data.df <- .cdtData$OpenFiles$Data[[nopf]][[2]]
                tab.array <- Display_data.frame_Table(data.df, title, colwidth = 5)
                tab.array <- c(tab.array, .cdtData$OpenFiles$Data[[nopf]][[3]])

                ntab <- update.OpenTabs('arr', tab.array)
                tkselect(.cdtEnv$tcl$main$tknotes, ntab)
            }else{
                Insert.Messages.Out(.cdtEnv$tcl$lang$global[['message']][['5']], format = TRUE)
            }
        }
    })

    tkbind(.cdtEnv$tcl$main$Openfiles, "<Button-3>", function(x, y){
        nopf <- as.integer(tclvalue(tkcurselection(.cdtEnv$tcl$main$Openfiles)))
        if(!is.na(nopf)) defile.menu.OpenFiles(x, y)
    })

    invisible()
}

cdtRightPanel <- function(){
    area.frame <- tkframe(.cdtEnv$tcl$main$panel.right)
    .cdtEnv$tcl$main$tknotes <- ttknotebook(area.frame)

    tkgrid(.cdtEnv$tcl$main$tknotes, row = 0, column = 0, sticky = 'nswe')
    tkgrid(area.frame, row = 0, column = 0, sticky = 'nswe')

    for(i in 0:3) tkgrid.columnconfigure(.cdtEnv$tcl$main$tknotes, i, weight = 1)
    for(i in 0:3) tkgrid.rowconfigure(.cdtEnv$tcl$main$tknotes, i, weight = 1)

    tkgrid.columnconfigure(area.frame, 0, weight = 1)
    tkgrid.rowconfigure(area.frame, 0, weight = 1)

    ########
    .cdtEnv$tcl$main$pressed_index <- tclVar('')
    tkbind(.cdtEnv$tcl$main$tknotes, "<ButtonPress-1>", function(x, y, W) btn_press(x, y, W))
    tkbind(.cdtEnv$tcl$main$tknotes, "<ButtonRelease-1>", function(x, y, W) btn_releases(x, y, W))

    invisible()
}
