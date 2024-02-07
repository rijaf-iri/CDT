
cdtOutputMessages <- function(main.pane0){
    out.frame <- tkframe(main.pane0, bd = 2, relief = 'groove')

    .cdtEnv$tcl$main$out.xscr <- tkscrollbar(out.frame, repeatinterval = 5, orient = "horizontal",
                                    command = function(...) tkxview(.cdtEnv$tcl$main$out.text, ...))
    .cdtEnv$tcl$main$out.yscr <- tkscrollbar(out.frame, repeatinterval = 5,
                                    command = function(...) tkyview(.cdtEnv$tcl$main$out.text, ...))
    .cdtEnv$tcl$main$out.text <- tktext(out.frame, bg = "white",
                                    # selectbackground = 'blue',
                                    #font = tkfont.create(family = "courier", size = 11),
                                    xscrollcommand = function(...) tkset(.cdtEnv$tcl$main$out.xscr, ...),
                                    yscrollcommand = function(...) tkset(.cdtEnv$tcl$main$out.yscr, ...),
                                    wrap = "none", height = 6)

    tkgrid(.cdtEnv$tcl$main$out.text, .cdtEnv$tcl$main$out.yscr)
    tkgrid(.cdtEnv$tcl$main$out.xscr)
    tkgrid.configure(.cdtEnv$tcl$main$out.yscr, sticky = "ns")
    tkgrid.configure(.cdtEnv$tcl$main$out.xscr, sticky = "ew")
    tkgrid.configure(.cdtEnv$tcl$main$out.text, sticky = 'nswe')
    tkgrid.columnconfigure(.cdtEnv$tcl$main$out.text, 0, weight = 1)

    tktag.configure(.cdtEnv$tcl$main$out.text, "sel", foreground = "black", background = "yellow")

    ###################
    menuCopyPaste(.cdtEnv$tcl$main$out.text, scopy = 'normal', scut = 'normal', spaste = 'disabled')

    tkbind(.cdtEnv$tcl$main$out.text, "<B1-Motion>", function(W, x, y){
        idx <- paste0('@', x, ',', y)
        idx <- tclvalue(tcl(W, "index", idx))
        tn <- tclvalue(tcl(W, "tag", "names", idx))
        tn <- strsplit(tn, " ")[[1]]
        ix <- as.numeric(strsplit(idx, "\\.")[[1]][1])
        itag <- tn %in% c("errortag", "warningtag", "infotag", "successtag")
        if(any(itag)){
            tcl(W, "tag", "remove", tn[itag], paste0(ix, '.0'), paste0(ix + 1, '.0'))
            .cdtEnv$tcl$main$out.copy <<- c(.cdtEnv$tcl$main$out.copy, list(c(ix, tn)))
        }
    })

    ### re-format in menuCopyPaste, or click inside 
    tkbind(.cdtEnv$tcl$main$out.text, "<Button-1>", function(W){
        if(!is.null(.cdtEnv$tcl$main$out.copy)){
            ret <- lapply(.cdtEnv$tcl$main$out.copy, function(x){
                ix <- as.numeric(x[1])
                tktag.add(W, x[2], paste0(ix, '.0'), paste0(ix + 1, '.0'))
                # tcl(W, 'tag', 'add', x[2], paste0(ix, '.0'), paste0(ix + 1, '.0'))
            })
            .cdtEnv$tcl$main$out.copy <- NULL
        }
    })

    return(out.frame)
}
