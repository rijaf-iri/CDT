
extractTS.previewWin <- function(states, shpL, type){
    listOpenFiles <- openFile_ttkcomboList()
    if(WindowsOS()){
        largeur0 <- .cdtEnv$tcl$fun$w.widgets(35)
        largeur1 <- .cdtEnv$tcl$fun$w.widgets(40)
        wtext <- .cdtEnv$tcl$fun$w.widgets(29)
    }else{
        largeur0 <- .cdtEnv$tcl$fun$w.widgets(27)
        largeur1 <- .cdtEnv$tcl$fun$w.widgets(31)
        wtext <- .cdtEnv$tcl$fun$w.widgets(35)
    }

    xml.dlg <- file.path(.cdtDir$dirLocal, "languages", "cdtExtractData_dlgBox.xml")
    lang.dlg <- cdtLanguageParse(xml.dlg, .cdtData$Config$lang.iso)

    #############################

    tt <- tktoplevel()
    frA <- tkframe(tt, relief = "raised", borderwidth = 2)
    frB <- tkframe(tt)

    #############################

    if(type == 'mpoint'){
        titre <- lang.dlg[['label']][['1']]
        frameType <- ttklabelframe(frA, text = titre, relief = 'groove', borderwidth = 2)

        coordsfiles <- tclVar()
        coordsfrom <- tclVar('crd')

        crdfrm1 <- tkradiobutton(frameType, variable = coordsfrom, value = "crd", text = lang.dlg[['label']][['3']], anchor = 'w', justify = 'left', state = states[1])
        crdfrm2 <- tkradiobutton(frameType, variable = coordsfrom, value = "cdt", text = lang.dlg[['label']][['4']], anchor = 'w', justify = 'left', state = states[1])
        cbmltpts <- ttkcombobox(frameType, values = unlist(listOpenFiles), textvariable = coordsfiles, state = states[1], width = largeur0)
        btmltpts <- tkbutton(frameType, text = "...", state = states[1])

        #########

        tkgrid(crdfrm1, row = 0, column = 0, sticky = 'we', rowspan = 1, columnspan = 5, padx = 1, pady = 1, ipadx = 1, ipady = 1)
        tkgrid(crdfrm2, row = 1, column = 0, sticky = 'we', rowspan = 1, columnspan = 5, padx = 1, pady = 1, ipadx = 1, ipady = 1)
        tkgrid(cbmltpts, row = 2, column = 0, sticky = 'we', rowspan = 1, columnspan = 4, padx = 0, pady = 1, ipadx = 1, ipady = 1)
        tkgrid(btmltpts, row = 2, column = 4, sticky = 'w', rowspan = 1, columnspan = 1, padx = 0, pady = 1, ipadx = 1, ipady = 1)

        helpWidget(crdfrm1, lang.dlg[['tooltip']][['1']], lang.dlg[['status']][['1']])
        helpWidget(crdfrm2, lang.dlg[['tooltip']][['2']], lang.dlg[['status']][['2']])
        helpWidget(cbmltpts, lang.dlg[['tooltip']][['3']], lang.dlg[['status']][['3']])
        helpWidget(btmltpts, lang.dlg[['tooltip']][['4']], lang.dlg[['status']][['4']])

        #########
        tkconfigure(btmltpts, command = function(){
            tkdelete(textObj, "0.0", "end")
            dat.opfiles <- getOpenFiles(.cdtEnv$tcl$main$win)
            if(!is.null(dat.opfiles)){
                update.OpenFiles('ascii', dat.opfiles)
                listOpenFiles[[length(listOpenFiles) + 1]] <<- dat.opfiles[[1]]
                tclvalue(coordsfiles) <- dat.opfiles[[1]]
                tkconfigure(cbmltpts, values = unlist(listOpenFiles))
                
                crds <- dat.opfiles[[2]]
                if(tclvalue(coordsfrom) == 'crd') crds <- crds[, c(1, 3, 4), drop = FALSE]
                if(tclvalue(coordsfrom) == 'cdt') crds <- t(crds[1:3, -1, drop = FALSE])
                for(i in 1:nrow(crds)) tkinsert(textObj, "end", paste(crds[i, 1], crds[i, 2], crds[i, 3], "\n"))
            }
        })

        #########
        tkbind(cbmltpts, "<<ComboboxSelected>>", function(){
            tkdelete(textObj, "0.0", "end")
            crds <- getStnOpenData(coordsfiles)
            if(tclvalue(coordsfrom) == 'crd') crds <- crds[, c(1, 3, 4), drop = FALSE]
            if(tclvalue(coordsfrom) == 'cdt') crds <- t(crds[1:3, -1, drop = FALSE])
            for(i in 1:nrow(crds))
                tkinsert(textObj, "end", paste(crds[i, 1], crds[i, 2], crds[i, 3], "\n"))
        })
    }

    #############################

    if(type == 'mpoly'){ 
        titre <- lang.dlg[['label']][['2']]
        frameType <- ttklabelframe(frA, text = titre, relief = 'groove', borderwidth = 2)

        btmpoly <- ttkbutton(frameType, text = lang.dlg[['button']][['1']], state = states[2], width = largeur1)

        #########
        tkgrid(btmpoly, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 3, ipadx = 1, ipady = 1)

        helpWidget(btmpoly, lang.dlg[['tooltip']][['5']], lang.dlg[['status']][['5']])

        #########
        tkconfigure(btmpoly, command = function(){
            tkdelete(textObj, "0.0", "end")
            shpf <- getShpOpenData(shpL[[2]])
            if(!is.null(shpf)){
                dat <- shpf[[2]]@data
                idx <- as.integer(tclvalue(tcl(shpL[[1]], 'current'))) + 1
                adminN <- as.character(dat[, idx])
                shpAttr <- levels(as.factor(adminN))
                for(i in 1:length(shpAttr))
                    tkinsert(textObj, "end", paste0(shpAttr[i], "\n"))
            }
        })
    }

    ########################

    btClear <- ttkbutton(frA, text = lang.dlg[['button']][['2']])

    tkconfigure(btClear, command = function(){
        tkdelete(textObj, "0.0", "end")
        .cdtData$EnvData$multiptspoly <- NULL
    })

    ########################

    frameText <- tkframe(frA, relief = 'groove', borderwidth = 2)

    yscr <- tkscrollbar(frameText, repeatinterval = 4,
                        command = function(...) tkyview(textObj, ...))
    textObj <- tktext(frameText, bg = "white", wrap = "none", height = 5, width = wtext,
                        yscrollcommand = function(...) tkset(yscr, ...))

    tkgrid(textObj, yscr)
    tkgrid.configure(yscr, sticky = "ns")
    tkgrid.configure(textObj, sticky = 'nswe')

    #########

    if(!is.null(.cdtData$EnvData$multiptspoly))
        tkinsert(textObj, "end", .cdtData$EnvData$multiptspoly)

    #############################
    tkgrid(frameType, row = 0, column = 0, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
    tkgrid(btClear, row = 1, column = 0, sticky = '', rowspan = 1, columnspan = 1, padx = 1, pady = 5, ipadx = 1, ipady = 1)
    tkgrid(frameText, row = 2, column = 0, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)

    #############################

    btOK <- ttkbutton(frB, text = .cdtEnv$tcl$lang$global[['button']][['1']])
    btCA <- ttkbutton(frB, text = .cdtEnv$tcl$lang$global[['button']][['2']])

    ret.params <- list(win = tt, textObj = textObj)
    .cdtData$EnvData$dlgBoxOpen <- TRUE

    tkconfigure(btOK, command = function(){
        retvars <- tclvalue(tkget(textObj, "0.0", "end"))
        .cdtData$EnvData$multiptspoly <- retvars
        tkdestroy(tt)
        tkfocus(.cdtEnv$tcl$main$win)
        ret.params <<- NULL
    })

    tkconfigure(btCA, command = function(){
        tkdestroy(tt)
        tkfocus(.cdtEnv$tcl$main$win)
        ret.params <<- NULL
    })

    ########################
    tkgrid(btOK, row = 0, column = 0, sticky = 'w', padx = 5, pady = 1, ipadx = 1, ipady = 1)
    tkgrid(btCA, row = 0, column = 1, sticky = 'e', padx = 5, pady = 1, ipadx = 1, ipady = 1)

    ########################

    tkgrid(frA, row = 0, column = 0, sticky = 'nswe', rowspan = 1, columnspan = 2, padx = 1, pady = 1, ipadx = 1, ipady = 1)
    tkgrid(frB, row = 1, column = 1, sticky = 'se', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)

    #########################
    tkwm.withdraw(tt)
    tcl('update')
    tkwm.geometry(tt, '+5+15')
    tkwm.transient(tt, .cdtEnv$tcl$main$win)
    tkwm.title(tt, titre)
    tkwm.deiconify(tt)
    tcl('wm', 'attributes', tt, topmost = TRUE)

    tkfocus(.cdtEnv$tcl$main$win)
    tkbind(tt, "<Destroy>", function(){
        tkfocus(.cdtEnv$tcl$main$win)
        ret.params <<- NULL
        .cdtData$EnvData$dlgBoxOpen <- FALSE
    })
    return(ret.params)
}
