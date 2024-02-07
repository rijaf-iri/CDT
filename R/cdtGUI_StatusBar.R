
cdtStatusBar <- function(){
    wbstatus1 <- .cdtEnv$tcl$fun$w.widgets(58)
    wbstatus2a <- .cdtEnv$tcl$fun$w.widgets(11)
    wbstatus2b <- .cdtEnv$tcl$fun$w.widgets(11)
    wbstatus3 <- .cdtEnv$tcl$fun$w.widgets(13)
    wbpbar <- .cdtEnv$tcl$fun$w.scale(11)

    ######
    ## Status bar width (from left)
    frstatusbar <- tkframe(.cdtEnv$tcl$main$win)

    ######
    .cdtEnv$tcl$status$help <- tclVar()
    .cdtEnv$tcl$status$xcrd <- tclVar()
    .cdtEnv$tcl$status$ycrd <- tclVar()
    .cdtEnv$tcl$status$zval <- tclVar()

    .cdtEnv$tcl$status$pbLab <- tclVar()
    .cdtEnv$tcl$status$pbBar <- tclVar(0)

    ######

    status.frame <- tkframe(frstatusbar)

    ######
    bstatus1 <- tkframe(status.frame, relief = 'sunken', bd = 2)

    lhelp <- tklabel(bstatus1, textvariable = .cdtEnv$tcl$status$help, anchor = 'w', width = wbstatus1)

    tkgrid(lhelp, row = 0, column = 0, sticky = "we")

    ######
    bstatus2 <- tkframe(status.frame, relief = 'sunken', bd = 2)

    ######
    bstatus2a <- tkframe(bstatus2)
    lxcoords <- tklabel(bstatus2a, textvariable = .cdtEnv$tcl$status$xcrd, width = wbstatus2a)
    lxcoords0 <- tklabel(bstatus2a, text = 'X:',justify = 'left', anchor = 'w')

    tkgrid(lxcoords0, row = 0, column = 0, sticky = "we")
    tkgrid(lxcoords, row = 0, column = 1, sticky = "we")

    ######
    xy.separator <- ttkseparator(bstatus2, orient = 'vertical')

    ######
    bstatus2b <- tkframe(bstatus2)
    lycoords <- tklabel(bstatus2b, textvariable = .cdtEnv$tcl$status$ycrd, width = wbstatus2b)
    lycoords0 <- tklabel(bstatus2b, text = 'Y:',justify = 'left', anchor = 'w')

    tkgrid(lycoords0, row = 0, column = 0, sticky = "we")
    tkgrid(lycoords, row = 0, column = 1, sticky = "we")

    tkgrid(bstatus2a, row = 0, column = 0, sticky = "we")
    tkgrid(xy.separator, row = 0, column = 1, sticky = 'ns', padx = 2)
    tkgrid(bstatus2b, row = 0, column = 2, sticky = "we")

    tkgrid.columnconfigure(bstatus2a, 1, weight = 1)
    tkgrid.columnconfigure(bstatus2b, 1, weight = 1)

    ######
    bstatus3 <- tkframe(status.frame, relief = 'sunken', bd = 2)

    lzcoords <- tklabel(bstatus3, textvariable = .cdtEnv$tcl$status$zval, width = wbstatus3)
    lzcoords0 <- tklabel(bstatus3, text = 'Z:',justify = 'left', anchor = 'w')

    tkgrid(lzcoords0, row = 0, column = 0, sticky = "we")
    tkgrid(lzcoords, row = 0, column = 1, sticky = "we")

    ######
    bstatus4 <- tkframe(status.frame)

    tb.pb.lab <- tklabel(bstatus4, width = 5, text = tclvalue(.cdtEnv$tcl$status$pbLab), textvariable = .cdtEnv$tcl$status$pbLab)
    tb.pb.bar <- ttkprogressbar(bstatus4, length = wbpbar, variable = .cdtEnv$tcl$status$pbBar)

    tkgrid(tb.pb.lab, tb.pb.bar)

    ####

    tkgrid(bstatus1, row = 0, column = 0, padx = 2, pady = 0, sticky = "we")
    tkgrid(bstatus2, row = 0, column = 1, padx = 1, pady = 0, sticky = "we")
    tkgrid(bstatus3, row = 0, column = 2, padx = 1, pady = 0, sticky = "we")
    tkgrid(bstatus4, row = 0, column = 3, sticky = "e")
    tkgrid.columnconfigure(bstatus1, 0, weight = 1)
    tkgrid.columnconfigure(bstatus2, 0, weight = 1)
    tkgrid.columnconfigure(bstatus2, 2, weight = 1)
    tkgrid.columnconfigure(bstatus3, 1, weight = 1)
    tkgrid.columnconfigure(bstatus4, 1, weight = 1)

    ####

    tkgrid(status.frame, row = 0, column = 0, sticky = "ew")
    tkgrid.columnconfigure(status.frame, 0, weight = 2)
    tkgrid.columnconfigure(status.frame, 1, weight = 2)
    tkgrid.columnconfigure(status.frame, 2, weight = 1)
    tkgrid.columnconfigure(status.frame, 3, weight = 1)

    return(frstatusbar)
}
