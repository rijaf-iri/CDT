
cdtTclTk_styles <- function(){
    existFont <- strsplit(tclvalue(.Tcl("font names")), " ")[[1]]
    if("cdtDefaultFont" %in% existFont)
        .Tcl("font delete cdtDefaultFont")

    tcl.file.conf <- file.path(.cdtDir$dirLocal, "config", "Tcl_config.json")
    TclConfig <- jsonlite::fromJSON(tcl.file.conf)
    TclConfig <- rapply(TclConfig, trimws, classes = "character", how = "replace")

    fontSize <- switch(tools::toTitleCase(Sys.info()["sysname"]),
                      "Windows" = TclConfig$Windows$font.size,
                      "Darwin" = TclConfig$MacOS$font.size,
                      "Linux" = TclConfig$Linux$font.size)
    if(is.null(fontSize) | is.na(fontSize)) fontSize <- 11

    .Tcl(paste("font create cdtDefaultFont -family Helvetica -size", fontSize))
    .Tcl("option add *font cdtDefaultFont")

    # .Tcl("option add *Menuentry.font cdtDefaultFont widgetDefault")
    # .Tcl("option add *Menu.font cdtDefaultFont widgetDefault")
    # .Tcl("option add *Menubutton.font cdtDefaultFont widgetDefault")

    .Tcl("option add *TCombobox*Listbox.font cdtDefaultFont")
    .Tcl("ttk::style configure TButton -font cdtDefaultFont")
    .Tcl("ttk::style configure TMenubutton -font cdtDefaultFont")
    .Tcl("ttk::style configure Toolbutton -font cdtDefaultFont")
    .Tcl("ttk::style configure TLabel -font cdtDefaultFont")
    .Tcl("ttk::style configure TLabelframe.Label -font cdtDefaultFont")
    .Tcl("ttk::style configure TLabelframe.Label -foreground black")

    fontOutTxt <- tkfont.create(family = "courier", size = 11)
    .Tcl(paste('option add *Text.font', fontOutTxt))

    bgnotebk <- tclvalue(.Tcl('::ttk::style lookup TFrame -background'))
    .Tcl(paste('ttk::style configure TPanedwindow -background', bgnotebk))
    .Tcl(paste('ttk::style configure Sash -background', bgnotebk))

    .Tcl("ttk::style configure TNotebook.Tab -font cdtDefaultFont")
    .Tcl('ttk::style layout TNotebook {Notebook.client -sticky nswe}')
    .Tcl(paste('ttk::style configure TNotebook -background', bgnotebk))
    # .Tcl('ttk::style configure TNotebook -background red')

    ### Tabs manipulation (close)
    closeTabgif0 <- file.path(.cdtDir$Root, "images", "closeTabButton0.gif")
    closeTabgif1 <- file.path(.cdtDir$Root, "images", "closeTabButton1.gif")
    closeTabgif2 <- file.path(.cdtDir$Root, "images", "closeTabButton2.gif")
    .Tcl(paste0("image create photo img_close -file ", '"', closeTabgif0, '"'))
    .Tcl(paste0("image create photo img_closeactive  -file ", '"', closeTabgif1, '"'))
    .Tcl(paste0("image create photo img_closepressed -file ", '"', closeTabgif2, '"'))

    try(.Tcl('ttk::style element create Fermer image [list img_close {active pressed !disabled} img_closepressed {active !disabled} img_closeactive ] -border 4 -sticky e'), silent = TRUE)

    .Tcl("ttk::style layout TNotebook.Tab {
            TNotebook.tab -sticky nswe -children {
                TNotebook.padding  -side top -sticky nswe -children {
                    TNotebook.focus -side top -sticky nswe -children {
                        TNotebook.label -side left -sticky {}
                        TNotebook.Fermer  -side right -sticky e
                    }
                }
            }
        }", sep = '\n')

    invisible()
}
