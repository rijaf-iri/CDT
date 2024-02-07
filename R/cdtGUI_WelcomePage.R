
cdtWelcomePage <- function(){
    frameAcc <- tkframe(.cdtEnv$tcl$main$tknotes)

    cdtfr.irilogo0 <- tkframe(frameAcc)
    cdtfr.cdtname <- tkframe(frameAcc)

    cdtfr.tmp2 <- tkframe(frameAcc)

    cdtfr.tmp3a <- tkframe(frameAcc)

    cdtfr.tmp4a <- tkframe(frameAcc)
    cdtfr.tmp4b <- tkframe(frameAcc)
    cdtfr.tmp4c <- tkframe(frameAcc)

    cdtfr.irilogo1 <- tkframe(frameAcc)

    #######
    cdtfont0 <- tkfont.create(family = "times", size = 48, weight = "bold")
    cdtfont1 <- tkfont.create(family = "times", size = 18, weight = "bold")
    cdtfont2 <- tkfont.create(family = "times", size = 12)

    imgAcc.cdt <- tkimage.create('photo', file = file.path(.cdtDir$Root, "images", "cdttext.gif"))
    imgAcc.iri0 <- resizeTclImage(file.path(.cdtDir$Root, "images", "iriLogo.gif"), factor = 3, zoom = FALSE)
    imgAcc.iri1 <- resizeTclImage(file.path(.cdtDir$Root, "images", "iri_logo_no_icon.gif"), factor = 4, zoom = FALSE)

    #######
    imglab.iri0 <- tklabel(cdtfr.irilogo0, image = imgAcc.iri0)
    tkgrid(imglab.iri0)

    #######
    imglab.cdt <- tklabel(cdtfr.cdtname, image = imgAcc.cdt)
    txtlab.cdt <- tklabel(cdtfr.cdtname, text = 'Climate Data Tools', font = cdtfont0, foreground = '#00227C')
    txtlab.ver <- tklabel(cdtfr.cdtname, text = paste('Version', .cdtEnv$pkg$version), font = cdtfont1)
    txtlab.auth <- tklabel(cdtfr.cdtname, text = 'Rija Faniriantsoa, Tufa Dinku', font = cdtfont2)

    tkgrid(imglab.cdt)
    tkgrid(txtlab.cdt)
    tkgrid(txtlab.ver)
    tkgrid(txtlab.auth)

    #######
    tcl.file.conf <- file.path(.cdtDir$dirLocal, "config", "Tcl_config.json")
    TclConfig <- jsonlite::fromJSON(tcl.file.conf)
    TclConfig <- rapply(TclConfig, trimws, classes = "character", how = "replace")

    sysNom <- tools::toTitleCase(Sys.info()["sysname"])
    acc_dim0 <- switch(sysNom,
                      "Windows" = c(9, 15.8),
                      "Darwin" = c(15, 18),
                      "Linux" = c(15, 18))
    if(sysNom == "Darwin") sysNom <- "MacOS"
    acc_dim <- c(TclConfig[[sysNom]]$height.panRight,
                 TclConfig[[sysNom]]$width.panRight)
    if((length(acc_dim) < 2) | any(is.na(acc_dim))) acc_dim <- acc_dim0
    hauteur_sep <- .cdtEnv$tcl$fun$h.widgets(acc_dim[1])
    largeur_sep <- .cdtEnv$tcl$fun$w.widgets(acc_dim[2])

    txtlab.tmp2 <- tklabel(cdtfr.tmp2, text = '', height = hauteur_sep)
    tkgrid(txtlab.tmp2)

    #######

    txtlab.tmp3a <- tklabel(cdtfr.tmp3a, text = '', width = largeur_sep)
    tkgrid(txtlab.tmp3a)

    #######

    txtlab.tmp4a <- tklabel(cdtfr.tmp4a, text = '', width = largeur_sep)
    txtlab.tmp4b <- tklabel(cdtfr.tmp4b, text = '', width = largeur_sep)
    txtlab.tmp4c <- tklabel(cdtfr.tmp4c, text = '', width = largeur_sep)
    tkgrid(txtlab.tmp4a)
    tkgrid(txtlab.tmp4b)
    tkgrid(txtlab.tmp4c)

    #######
    imglab.iri1 <- tklabel(cdtfr.irilogo1, image = imgAcc.iri1)
    tkgrid(imglab.iri1, row = 0, column = 0, sticky = 'se')

    #######
    tkgrid(cdtfr.irilogo0, row = 0, column = 0, sticky = 'nw', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
    tkgrid(cdtfr.cdtname, row = 0, column = 1, sticky = 'nwe', rowspan = 2, columnspan = 5, padx = 1, pady = 1, ipadx = 3, ipady = 1)

    tkgrid(cdtfr.tmp2, row = 2, column = 0, sticky = 'we', rowspan = 1, columnspan = 6, padx = 1, pady = 1, ipadx = 1, ipady = 1)

    tkgrid(cdtfr.tmp3a, row = 3, column = 0, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)

    tkgrid(cdtfr.tmp4a, row = 4, column = 0, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
    tkgrid(cdtfr.tmp4b, row = 4, column = 1, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
    tkgrid(cdtfr.tmp4c, row = 4, column = 2, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)

    tkgrid(cdtfr.irilogo1, row = 4, column = 3, sticky = 'se', rowspan = 1, columnspan = 3, padx = 1, pady = 1, ipadx = 1, ipady = 1)

    #######
    tkgrid(frameAcc, sticky = 'nsew', pady = 25, padx = 1, ipadx = 1, ipady = 1)

    invisible()
}