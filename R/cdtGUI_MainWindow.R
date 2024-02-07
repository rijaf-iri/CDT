
cdtMainWindow <- function(){
    .cdtEnv$tcl$main$win <- tktoplevel()
    tkwm.resizable(.cdtEnv$tcl$main$win, TRUE, TRUE)
    tkgrid.columnconfigure(.cdtEnv$tcl$main$win, 0, weight = 1)
    tkgrid.rowconfigure(.cdtEnv$tcl$main$win, 1, weight = 1)

    ## Window geometry
    .cdtEnv$tcl$data$width.scr <- as.integer(tkwinfo("screenwidth", .cdtEnv$tcl$main$win))
    .cdtEnv$tcl$data$height.scr <- as.integer(tkwinfo("screenheight", .cdtEnv$tcl$main$win))

    ## Function to scale dialog in %
    .cdtEnv$tcl$fun$w.scale <- function(percent) as.integer(percent * .cdtEnv$tcl$data$width.scr / 100)
    .cdtEnv$tcl$fun$h.scale <- function(percent) as.integer(percent * .cdtEnv$tcl$data$height.scr / 100)

    ## Left panel dimension
    .cdtEnv$tcl$data$wpanel.left <- .cdtEnv$tcl$fun$w.scale(30)
    .cdtEnv$tcl$data$hpanel.left <- .cdtEnv$tcl$fun$h.scale(70)

    ## Left panel ScrollableFrame
    ## wscrlwin (min:23 max:25)
    .cdtEnv$tcl$data$wscrlwin <- .cdtEnv$tcl$fun$w.scale(24)
    .cdtEnv$tcl$data$hscrlwin <- .cdtEnv$tcl$fun$h.scale(50)

    ## Font width
    font_size <- paste0("0123456789", paste0(letters[1:26], LETTERS[1:26], collapse = ''))
    font_size <- tclvalue(tkfont.measure(.cdtEnv$tcl$main$win, font_size))
    font_size <- as.numeric(font_size) / (10 + 2 * 26)

    ## Widgets width function in %
    .cdtEnv$tcl$fun$w.widgets <- function(percent) 
            as.integer(.cdtEnv$tcl$fun$w.scale(percent) / font_size)
    .cdtEnv$tcl$fun$h.widgets <- function(percent) 
            as.integer(.cdtEnv$tcl$fun$h.scale(percent) / font_size)

    ## List open files width
    .cdtEnv$tcl$data$w.opfiles <- .cdtEnv$tcl$fun$w.widgets(30)

    ## Files extension
    .cdtEnv$tcl$data$filetypes1 <- "{{Text Files} {.txt .TXT}} {{CSV Files} {.csv .CSV}} {{All files} *}"
    .cdtEnv$tcl$data$filetypes2 <- "{{CSV Files} {.csv .CSV}} {{Text Files} {.txt .TXT}} {{All files} *}"
    .cdtEnv$tcl$data$filetypes3 <- "{{NetCDF Files} {.nc .NC .cdf .CDF}} {{All files} *}"
    .cdtEnv$tcl$data$filetypes4 <- "{{ESRI Shapefile} {.shp}} {{All files} *}"
    .cdtEnv$tcl$data$filetypes5 <- "{{JPEG format} {.jpeg .JPEG}} {{PNG format} {.png .PNG}} {{All files} *}"
    .cdtEnv$tcl$data$filetypes6 <- "{{R Objects} {.rds .RDS .RData}} {{All files} *}"

    if(.cdtData$Config$ascii.file.ext == "csv"){
        .cdtEnv$tcl$data$filetypesA <- .cdtEnv$tcl$data$filetypes2
    }else{
        .cdtEnv$tcl$data$filetypesA <- .cdtEnv$tcl$data$filetypes1
    }

    ## Initialization
    .cdtData$OpenFiles$Type <- list()
    .cdtData$OpenFiles$Data <- list()
    .cdtData$OpenTab$Type <- list()
    .cdtData$OpenTab$Data <- list()

    ## Menus
    cdtMainContextMenu()

    ## Toolbars
    tools.frame <- cdtDrawToolbars()

    ############
    ## Main frame
    main.frame0 <- tkframe(.cdtEnv$tcl$main$win, bd = 2, relief = 'ridge')

    # panedwindow vertical
    main.pane0 <- ttkpanedwindow (main.frame0, orient = 'vertical')
    # panedwindow horizontal
    main.frame <- ttkpanedwindow (main.pane0, orient = 'horizontal', height = .cdtEnv$tcl$data$hpanel.left)
    # left panel
    .cdtEnv$tcl$main$panel.left <- tkframe(main.frame, relief = 'raised', bd = 2, width = .cdtEnv$tcl$data$wpanel.left)  
    # right panel
    .cdtEnv$tcl$main$panel.right <- tkframe(main.frame)

    #####
    ### Left panel
    cdtLeftPanel()

    ### Right panel
    cdtRightPanel()

    ### Welcome page
    cdtWelcomePage()

    ### Output message frame
    out.frame <- cdtOutputMessages(main.pane0)

    ### Status bar
    frstatusbar <- cdtStatusBar()

    ############
    ## Manage grid

    tkadd(main.frame, .cdtEnv$tcl$main$panel.left)
    tkadd(main.frame, .cdtEnv$tcl$main$panel.right)

    #left panel
    tkgrid.columnconfigure(.cdtEnv$tcl$main$panel.left, 0, weight = 1)
    tkgrid.rowconfigure(.cdtEnv$tcl$main$panel.left, 1, weight = 1)

    #right panel
    tkgrid.columnconfigure(.cdtEnv$tcl$main$panel.right, 0, weight = 1)
    tkgrid.rowconfigure(.cdtEnv$tcl$main$panel.right, 0, weight = 1)

    ###
    tkadd(main.pane0, main.frame)
    tkadd(main.pane0, out.frame)

    tkgrid.columnconfigure(main.frame, 0, weight = 1)
    tkgrid.columnconfigure(main.frame, 1, weight = 1)
    tkgrid.rowconfigure(main.frame, 0, weight = 1)
    tkgrid.columnconfigure(out.frame, 0, weight = 1)
    tkgrid.rowconfigure(out.frame, 0, weight = 1)

    ## panned frame
    tkgrid(main.pane0, row = 0, column = 0, rowspan = 1, columnspan = 2, sticky = "snew", padx = 1) 
    tkgrid.rowconfigure(main.pane0, 0, weight = 1)
    tkgrid.rowconfigure(main.pane0, 1, weight = 1)
    tkgrid.columnconfigure(main.pane0, 0, weight = 1)

    ############

    ## toolbars
    tkgrid(tools.frame, row = 0, column = 0, rowspan = 1, columnspan = 2, sticky = "new", padx = 5)

    ## main frame
    tkgrid(main.frame0, row = 1, column = 0, rowspan = 1, columnspan = 2, sticky = "snew", padx = 5) 
    tkgrid.rowconfigure(main.frame0, 0, weight = 1)
    tkgrid.columnconfigure(main.frame0, 0, weight = 1)

    ## statusbar
    tkgrid(frstatusbar, row = 2, column = 0, rowspan = 1, columnspan = 1, sticky = "snew")
    tkgrid.columnconfigure(frstatusbar, 0, weight = 1)

    ## grip
    grip.right <- ttksizegrip(.cdtEnv$tcl$main$win)
    tkgrid(grip.right, row = 2, column = 1, sticky = "se")

    ############
    ## Manage geometry

    tkwm.withdraw(.cdtEnv$tcl$main$win)
    tcl('update')
    tkwm.geometry(.cdtEnv$tcl$main$win, paste0(.cdtEnv$tcl$data$width.scr, 'x', .cdtEnv$tcl$data$height.scr, '+', 0, '+', 0))
    tkwm.transient(.cdtEnv$tcl$main$win)
    tkwm.title(.cdtEnv$tcl$main$win, paste0("Climate Data Tools, v", .cdtEnv$pkg$version))
    tkwm.deiconify(.cdtEnv$tcl$main$win)

    if(WindowsOS()){
        tcl('wm', 'attributes', .cdtEnv$tcl$main$win, fullscreen = FALSE)
        tcl('wm', 'state', .cdtEnv$tcl$main$win, 'zoomed')
    }else{
        tcl('wm', 'attributes', .cdtEnv$tcl$main$win, fullscreen = FALSE, zoomed = TRUE)
    }

    ## Close CDT
    tcl("wm", "protocol", .cdtEnv$tcl$main$win, "WM_DELETE_WINDOW", function(){
        on.exit({
            rm(list = ls(envir = .cdtData), envir = .cdtData)
            .cdtEnv$tcl <- NULL
            options(warn = 0)
        })
        refreshCDT()
        tkdestroy(.cdtEnv$tcl$main$win)
    })

    invisible()
}