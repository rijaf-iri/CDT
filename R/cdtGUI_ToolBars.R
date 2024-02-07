
cdtDrawToolbars <- function(){
    tools.frame <- tkframe(.cdtEnv$tcl$main$win, bd = 2, relief = 'ridge')

    ############

    plot.scale <- if(WindowsOS()) 385 else 480
    horiz <- .cdtEnv$tcl$fun$w.scale(70) / plot.scale
    verti <- .cdtEnv$tcl$fun$h.scale(60) / plot.scale

    horizS <- round(horiz, 1)
    vertiS <- round(verti, 1)

    xml.toolbar <- file.path(.cdtDir$dirLocal, "languages", "cdt_toolbars_help.xml")
    lang.toolbar <- cdtLanguageParse(xml.toolbar, .cdtData$Config$lang.iso)

    ##################

    tb.open.file <- tkbutton.toolbar(tools.frame, "open_file24.gif", lang.toolbar[['tooltip']][['1']], lang.toolbar[['status']][['1']])
    tb.save.image <- tkbutton.toolbar(tools.frame, "save_img24.gif", lang.toolbar[['tooltip']][['2']], lang.toolbar[['status']][['2']])
    tb.open.table <- tkbutton.toolbar(tools.frame, "open_table24.gif", lang.toolbar[['tooltip']][['3']], lang.toolbar[['status']][['3']])
    tb.save.table <- tkbutton.toolbar(tools.frame, "save_table24.gif", lang.toolbar[['tooltip']][['4']], lang.toolbar[['status']][['4']])

    ###
    .cdtEnv$tcl$toolbar$run <- tkbutton.toolbar(tools.frame, "execute24.gif", lang.toolbar[['tooltip']][['5']], lang.toolbar[['status']][['5']])
    tkconfigure(.cdtEnv$tcl$toolbar$run, state = "disabled")

    ###
    lspinH <- tklabel(tools.frame, text = lang.toolbar[['label']][['1']])
    .cdtEnv$tcl$toolbar$spinH <- ttkspinbox(tools.frame, from = 0.5, to = 5.0, increment = 0.1, justify = 'center', width = 6, state = 'disabled')
    tkset(.cdtEnv$tcl$toolbar$spinH, horizS)

    helpWidget(.cdtEnv$tcl$toolbar$spinH, lang.toolbar[['tooltip']][['6']], lang.toolbar[['status']][['6']])

    ###
    lspinV <- tklabel(tools.frame, text = lang.toolbar[['label']][['2']])
    .cdtEnv$tcl$toolbar$spinV <- ttkspinbox(tools.frame, from = 0.5, to = 5.0, increment = 0.1, justify = 'center', width = 6, state = 'disabled')
    tkset(.cdtEnv$tcl$toolbar$spinV, vertiS)

    helpWidget(.cdtEnv$tcl$toolbar$spinV, lang.toolbar[['tooltip']][['7']], lang.toolbar[['status']][['7']])

    ###
    .cdtEnv$tcl$toolbar$plotRedraw <- tkbutton.toolbar(tools.frame, "RedrawButton24.gif", lang.toolbar[['tooltip']][['8']], lang.toolbar[['status']][['8']])
    helpWidget(.cdtEnv$tcl$toolbar$plotRedraw, lang.toolbar[['tooltip']][['11']], lang.toolbar[['status']][['11']])

    ###
    tb.close.tab <- tkbutton.toolbar(tools.frame, "close_tab24.gif", lang.toolbar[['tooltip']][['9']], lang.toolbar[['status']][['9']])
    tb.exit.win <- tkbutton.toolbar(tools.frame, "quit_cdt24.gif", lang.toolbar[['tooltip']][['10']], lang.toolbar[['status']][['10']])

    ###
    tb.separator0 <- ttkseparator(tools.frame, orient = 'vertical')
    tb.separator1 <- ttkseparator(tools.frame, orient = 'vertical')
    tb.separator2 <- ttkseparator(tools.frame, orient = 'vertical')
    tb.separator3 <- ttkseparator(tools.frame, orient = 'vertical')

    ##################

    tkgrid(
            tb.open.file, tb.save.image,
            tb.separator0,
            tb.open.table, tb.save.table,
            tb.separator1,
            .cdtEnv$tcl$toolbar$run,
            tb.separator2,
            lspinH, .cdtEnv$tcl$toolbar$spinH,
            lspinV, .cdtEnv$tcl$toolbar$spinV,
            .cdtEnv$tcl$toolbar$plotRedraw,
            tb.separator3,
            tb.close.tab, tb.exit.win
        )

    ###
    tkgrid.configure(tb.separator0, sticky = 'ns')
    tkgrid.configure(tb.separator1, sticky = 'ns')
    tkgrid.configure(tb.separator2, sticky = 'ns', padx = 20)
    tkgrid.configure(tb.separator3, sticky = 'ns', padx = 20)

    tkgrid.configure(tb.open.file, padx = 5)
    tkgrid.configure(tb.save.image, padx = 5)

    tkgrid.configure(tb.open.table, padx = 5)
    tkgrid.configure(tb.save.table, padx = 5)

    ###
    tkgrid.configure(.cdtEnv$tcl$toolbar$run, padx = 20, ipadx = 5)

    ###
    tkgrid.configure(.cdtEnv$tcl$toolbar$plotRedraw, padx = 5)

    ###
    tkgrid.configure(tb.close.tab, padx = 5)
    tkgrid.configure(tb.exit.win, padx = 30, sticky = 'e')

    ##################

    hRedraw <- tkimage.create('photo', '-file', file.path(.cdtDir$Root, "images", 'RedrawButton24.gif'))
    hRedraw1 <- tkimage.create('photo', '-file', file.path(.cdtDir$Root, "images", 'RedrawButton-Change24.gif'))

    tkbind(.cdtEnv$tcl$toolbar$plotRedraw, "<ButtonRelease>", function(){
        tkconfigure(.cdtEnv$tcl$toolbar$plotRedraw, image = hRedraw)
    })

    tkbind(.cdtEnv$tcl$toolbar$spinH, "<<Increment>>", function(){
        tkconfigure(.cdtEnv$tcl$toolbar$plotRedraw, image = hRedraw1)
    })
    tkbind(.cdtEnv$tcl$toolbar$spinH, "<<Decrement>>", function(){
        tkconfigure(.cdtEnv$tcl$toolbar$plotRedraw, image = hRedraw1)
    })
    tkbind(.cdtEnv$tcl$toolbar$spinV, "<<Increment>>", function(){
        tkconfigure(.cdtEnv$tcl$toolbar$plotRedraw, image = hRedraw1)
    })
    tkbind(.cdtEnv$tcl$toolbar$spinV, "<<Decrement>>", function(){
        tkconfigure(.cdtEnv$tcl$toolbar$plotRedraw, image = hRedraw1)
    })

    ##################

    tkconfigure(tb.exit.win, command = function(){
        on.exit({
            rm(list = ls(envir = .cdtData), envir = .cdtData)
            .cdtEnv$tcl <- NULL
            options(warn = 0)
        })
        refreshCDT()
        tkdestroy(.cdtEnv$tcl$main$win)
    })

    ##################

    tkconfigure(tb.close.tab, command = function(){
        tabid <- as.integer(tclvalue(tkindex(.cdtEnv$tcl$main$tknotes, 'current')))
        Close_Notebook_Tab(tabid)
    })

    ##################

    tkconfigure(tb.open.file, command = function(){
        on.exit({
            tkconfigure(.cdtEnv$tcl$main$win, cursor = '')
            tcl('update')
        })
        tkconfigure(.cdtEnv$tcl$main$win, cursor = 'watch')
        tcl('update')

        dat.opfiles <- getOpenFiles(.cdtEnv$tcl$main$win)
        if(!is.null(dat.opfiles)) update.OpenFiles('ascii', dat.opfiles)
    })

    ##################

    tkconfigure(tb.save.image, command = function(){
        if(length(.cdtData$OpenTab$Type)){
            tabid <- as.integer(tclvalue(tkindex(.cdtEnv$tcl$main$tknotes, 'current'))) + 1
            if(.cdtData$OpenTab$Type[[tabid]] == "img") SavePlot()
            tkfocus(.cdtEnv$tcl$main$win)
        }
    })

    ##################

    tkconfigure(tb.open.table, command = function() {
        tab.array <- Display_Array_Tab(.cdtEnv$tcl$main$win)
        if(!is.null(tab.array)){
            ntab <- update.OpenTabs('arr', tab.array)
            tkselect(.cdtEnv$tcl$main$tknotes, ntab)
        }
    })

    ##################

    tkconfigure(tb.save.table, command = function(){
        if(length(.cdtData$OpenTab$Type) == 0) return(NULL)
        tabid <- as.integer(tclvalue(tkindex(.cdtEnv$tcl$main$tknotes, 'current'))) + 1
        arrTypes <- c("arr", "chkcrds", "falsezero", "outqc", "outhom")
        if(!.cdtData$OpenTab$Type[[tabid]] %in% arrTypes) return(NULL)

        tab2sav <- try(Save_Notebook_Tab_Array(), silent = TRUE)
        if(inherits(tab2sav, "try-error")){
            Insert.Messages.Out(gsub('[\r\n]', '', tab2sav[1]), TRUE, 'e')
            Insert.Messages.Out(lang.toolbar[['message']][['2']], TRUE, 'e')
        }else{
            if(!is.null(tab2sav))
                Insert.Messages.Out(lang.toolbar[['message']][['1']], TRUE, "s")
        }
    })

    ##################

    tkconfigure(.cdtEnv$tcl$toolbar$run, command = function(){
        on.exit({
            tkconfigure(.cdtEnv$tcl$main$win, cursor = '')
            tcl('update')
        })
        tkconfigure(.cdtEnv$tcl$main$win, cursor = 'watch')
        tcl('update')

        if(!is.null(.cdtData$GalParams)) Execute_Function()
    })

    ##################

    tkconfigure(.cdtEnv$tcl$toolbar$plotRedraw, relief = 'raised', state = "disabled",
        command = function()
    {
        on.exit({
            tkconfigure(.cdtEnv$tcl$main$win, cursor = '')
            tcl('update')
        })
        tkconfigure(.cdtEnv$tcl$main$win, cursor = 'watch')
        tcl('update')

        tabid <- as.integer(tclvalue(tkindex(.cdtEnv$tcl$main$tknotes, 'current'))) + 1
        if(length(.cdtData$OpenTab$Type)){
            if(.cdtData$OpenTab$Type[[tabid]] == "img"){

                if(class(.cdtData$OpenTab$Data[[tabid]][[2]]) == "tkwin"){
                    W <- .cdtData$OpenTab$Data[[tabid]][[2]]
                    img <- .cdtData$OpenTab$Data[[tabid]][[2]]
                    refreshPlot(W = W, img = img,
                        hscale = as.numeric(tclvalue(tkget(.cdtEnv$tcl$toolbar$spinH))),
                        vscale = as.numeric(tclvalue(tkget(.cdtEnv$tcl$toolbar$spinV))))
                }
                if(class(.cdtData$OpenTab$Data[[tabid]][[2]]) == "list"){
                    W <- .cdtData$OpenTab$Data[[tabid]][[2]][[1]]
                    img <- .cdtData$OpenTab$Data[[tabid]][[2]][[2]]
                    refreshPlot(W = W, img = img,
                        hscale = as.numeric(tclvalue(tkget(.cdtEnv$tcl$toolbar$spinH))),
                        vscale = as.numeric(tclvalue(tkget(.cdtEnv$tcl$toolbar$spinV))))
                    win.child.class <- tclvalue(tkwinfo('class', tkwinfo('children', .cdtData$OpenTab$Data[[tabid]][[1]][[2]])))
                    if(win.child.class == "Frame"){
                        w <- as.double(tkwinfo("width", .cdtEnv$tcl$main$panel.right))
                        h <- as.double(tkwinfo("height", .cdtEnv$tcl$main$panel.right))
                        setScrollCanvas(W, w, h)
                    }
                }
                tkconfigure(.cdtEnv$tcl$toolbar$plotRedraw, image = hRedraw)
            }
        }
    })

    return(tools.frame)
}