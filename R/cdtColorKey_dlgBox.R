
createColorkey <- function(parent.win, listCol){
    txtCol_width <- if(WindowsOS()) 48 else 44
    kcol <- if(WindowsOS()) 18 else 15

    ########################

    xml.dlg <- file.path(.cdtDir$dirLocal, "languages", "cdtColorKey_dlgBox.xml")
    lang.dlg <- cdtLanguageParse(xml.dlg, .cdtData$Config$lang.iso)

    ########################

    tt <- tktoplevel()
    tkgrab.set(tt)
    tkfocus(tt)

    frDialog <- tkframe(tt, relief = 'raised', borderwidth = 2)
    frButt <- tkframe(tt)

    ########################

    listCol <- listCol[!is.na(listCol)]
    if(length(listCol) > 0){
        rgbcol <- grDevices::col2rgb(listCol, alpha = TRUE)
        listCol <- grDevices::rgb(t(rgbcol), maxColorValue = 255)
        nlCol <- length(listCol)
        varCol <- tclVar(listCol[nlCol])
    }else varCol <- tclVar('red')

    listCanColor <- list()

    ########################

    parseKolor <- function(vcolor){
        vcolor <- gsub("[\t\r\n]", "", vcolor)
        vcolor <- gsub('\\s+', '', vcolor)
        vcolor <- strsplit(vcolor, ",")[[1]]
        vcolor <- vcolor[!is.na(vcolor) & vcolor != '']

        hexcol <- substr(vcolor, 1, 1) == "#"
        txtcol <- which(!hexcol)
        if(length(txtcol) > 0){
            vtxtcol <- vcolor[txtcol]
            existcol <- vtxtcol %in% grDevices::colors()
            if(any(!existcol)){
                msg <- paste(lang.dlg[['message']][['1']], paste0(vtxtcol[!existcol], collapse = ", "))
                Insert.Messages.Out(msg, format = TRUE)
                vcolor <- vcolor[-txtcol[!existcol]]
                if(length(vcolor) == 0){
                    Insert.Messages.Out(lang.dlg[['message']][['2']], format = TRUE)
                    return(NULL)
                }
            }
        }
        return(vcolor)
    }

    getColorCanvas <- function(W, x, y){
        rootx <- as.integer(tkwinfo("rootx", W))  
        rooty <- as.integer(tkwinfo("rooty", W))
        xpos <- as.integer(x) + rootx
        ypos <- as.integer(y) + rooty
        wdgt <- tclvalue(tkwinfo('containing', displayof = W, xpos, ypos))
        tclvalue(varRemColor) <<- wdgt
        .Tcl(paste("tk_popup", .Tcl.args(popMenuRemColor, xpos, ypos)))
    }

    ########################

    frCmd <- tkframe(frDialog)

    labColor <- tklabel(frCmd, text = lang.dlg[['label']][['1']])
    canColor <- tkcanvas(frCmd, width = 80, height = 25, bg = tclvalue(varCol))
    addColor <- ttkbutton(frCmd, text = lang.dlg[['button']][['1']])
    saveColor <- ttkbutton(frCmd, text = lang.dlg[['button']][['2']])
    loadColor <- ttkbutton(frCmd, text = lang.dlg[['button']][['3']])

    #########
    tkconfigure(addColor, command = function(){
        tcl('wm', 'attributes', tt, topmost = FALSE)
        color <- tclvalue(tcl("tk_chooseColor", initialcolor = tclvalue(varCol), title = lang.dlg[['title']]))
        tcl('wm', 'attributes', tt, topmost = TRUE)
        if(nchar(color) > 0){
            tkconfigure(canColor, bg = color)
            tclvalue(varCol) <- color
            listCol[iColor + 1] <<- color
            listCanColor[[iColor + 1]] <<- tkcanvas(frListColor, width = 20, height = 20, bg = listCol[iColor + 1])
            tkgrid(listCanColor[[iColor + 1]], row = floor(iColor / kcol), column = iColor %% kcol)
            tkinsert(textColor, "end", paste0(color, ', '))
            iColor <<- iColor + 1
        }
    })

    tkconfigure(saveColor, command = function(){
        if(tclvalue(editColor) == '1'){
            vcolor <- tclvalue(tkget(textColor, "0.0", "end"))
            vcolor <- parseKolor(vcolor)
            if(is.null(vcolor)) return(NULL)
            listCol <- vcolor
        }else{
            listCol <- if(length(iColor) == 1 & iColor == 0) NULL else listCol[!is.na(listCol)]
        }
        rgbcol <- grDevices::col2rgb(listCol, alpha = TRUE)
        listCol <- grDevices::rgb(t(rgbcol), maxColorValue = 255)

        filetypes <- "{{Color Files} {.clr .CLR}} {{All files} *}"
        tcl('wm', 'attributes', tt, topmost = FALSE)
        file2save <- tk_get_SaveFile(filetypes = filetypes)
        tcl('wm', 'attributes', tt, topmost = TRUE)
        write.table(listCol, file2save, row.names = FALSE, col.names = FALSE)
    })

    tkconfigure(loadColor, command = function(){
        filetypes <- "{{Color Files} {.clr .CLR}} {{All files} *}"
        tcl('wm', 'attributes', tt, topmost = FALSE)
        fileopen <- tclvalue(tkgetOpenFile(initialdir = getwd(), initialfile = "", filetypes = filetypes))
        tcl('wm', 'attributes', tt, topmost = TRUE)
        if(fileopen != "" | !is.na(fileopen)){
            datColor <- try(read.table(fileopen, strip.white = TRUE, colClasses = "character"), silent = TRUE)
            if(!inherits(datColor, "try-error")){
                listCol <<- as.character(datColor[, 1])
                if(length(listCanColor) > 0)
                    for(icvs in seq_along(listCanColor)) tkdestroy(listCanColor[[icvs]])
                tkdelete(textColor, "0.0", "end")
                listCanColor <<- list()
                for(icvs1 in seq_along(listCol)){
                    tkinsert(textColor, "end", paste0(listCol[icvs1], ', '))
                    listCanColor[[icvs1]] <<- tkcanvas(frListColor, width = 20, height = 20, bg = listCol[icvs1])
                    pj <- icvs1 - 1
                    tkgrid(listCanColor[[icvs1]], row = floor(pj / kcol), column = pj %% kcol)
                }
                nlCol <- length(listCol)
                if(nlCol > 0){
                    tkconfigure(canColor, bg = listCol[nlCol])
                    tclvalue(varCol) <- listCol[nlCol]
                    iColor <<- nlCol
                }else iColor <<- 0
            }else Insert.Messages.Out(gsub('[\r\n]', '', datColor[1]), format = TRUE)
        }
    })

    #########
    tkgrid(labColor, row = 0, column = 0, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
    tkgrid(canColor, row = 0, column = 1, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
    tkgrid(addColor, row = 0, column = 2, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
    tkgrid(saveColor, row = 0, column = 3, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
    tkgrid(loadColor, row = 0, column = 4, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)

    ########################

    frEditCol <- tkframe(frDialog)

    yscrColor <- tkscrollbar(frEditCol, repeatinterval = 4,
                        command = function(...) tkyview(textColor, ...))
    textColor <- tktext(frEditCol, bg = "white", wrap = "word", height = 5, width = txtCol_width,
                        yscrollcommand = function(...) tkset(yscrColor, ...))
    menuCopyPaste(textColor, scopy = 'normal', scut = 'normal', spaste = 'normal')

    tkgrid(textColor, yscrColor)
    tkgrid.configure(yscrColor, sticky = "ns")
    tkgrid.configure(textColor, sticky = 'nswe')

    ########################

    frListColor <- tkframe(frDialog, relief = 'sunken', bd = 2, highlightcolor = 'black')

    if(length(listCol) > 0){
        for(j in seq_along(listCol)){
            listCanColor[[j]] <- tkcanvas(frListColor, width = 20, height = 20, bg = listCol[j])
            pj <- j - 1
            tkgrid(listCanColor[[j]], row = floor(pj / kcol), column = pj %% kcol)
            tkinsert(textColor, "end", paste0(listCol[j], ', '))
        }
        iColor <- length(listCol)
    }else{
        iColor <- 0
        listCol[iColor + 1] <- tclvalue(varCol)
    }

    #########

    popMenuRemColor <- tkmenu(frListColor, tearoff = FALSE)

    varRemColor <- tclVar()

    tkadd(popMenuRemColor, "command", label = lang.dlg[['label']][['3']], command = function(){
        icanvas <- match(tclvalue(varRemColor), sapply(listCanColor, function(x) x$ID))
        color <- tclvalue(tcl("tk_chooseColor", initialcolor = tclvalue(varCol), title = lang.dlg[['title']]))
        if(nchar(color) > 0){
            tkconfigure(canColor, bg = color)
            tclvalue(varCol) <- color
            listCol[icanvas] <<- color
            tkconfigure(listCanColor[[icanvas]], bg = color)
            tkdelete(textColor, "0.0", "end")
            for(icvs in seq_along(listCol))
                tkinsert(textColor, "end", paste0(listCol[icvs], ', '))
        }
    })

    tkadd(popMenuRemColor, "separator")

    tkadd(popMenuRemColor, "command", label = lang.dlg[['label']][['4']], command = function(){
        icanvas <- match(tclvalue(varRemColor), sapply(listCanColor, function(x) x$ID))
        if(!is.na(icanvas)){
            for(icvs in seq_along(listCanColor)) tkdestroy(listCanColor[[icvs]])
            tkdelete(textColor, "0.0", "end")
            listCol <<- listCol[-icanvas]
            listCanColor <<- list()
            for(icvs1 in seq_along(listCol)){
                tkinsert(textColor, "end", paste0(listCol[icvs1], ', '))
                listCanColor[[icvs1]] <<- tkcanvas(frListColor, width = 20, height = 20, bg = listCol[icvs1])
                pj <- icvs1 - 1
                tkgrid(listCanColor[[icvs1]], row = floor(pj / kcol), column = pj %% kcol)
            }
            nlCol <- length(listCol)
            if(nlCol > 0){
                tkconfigure(canColor, bg = listCol[nlCol])
                tclvalue(varCol) <- listCol[nlCol]
                iColor <<- nlCol
            }else iColor <<- 0
        }
    })

    #########

    tkbind(frListColor, "<Enter>", function(){ 
        for(jj in seq_along(listCanColor)){
            tkbind(listCanColor[[jj]], "<Button-3>", function(W, x, y){
                getColorCanvas(W, x, y)
            })
        }
    })

    ########################

    frEditCmd <- tkframe(frDialog)

    editColor <- tclVar(0)

    chkButColor <- tkcheckbutton(frEditCmd, variable = editColor, text = lang.dlg[['label']][['2']], anchor = 'w', justify = 'left')
    dispButColor <- ttkbutton(frEditCmd, text = lang.dlg[['button']][['4']])

    tkconfigure(dispButColor, command = function(){
        if(tclvalue(editColor) == '1'){
            vcolor <- tclvalue(tkget(textColor, "0.0", "end"))
            vcolor <- parseKolor(vcolor)
            if(is.null(vcolor)) return(NULL)
            listCol <<- vcolor
            if(length(listCol) > 0){
                if(length(listCanColor) > 0)
                    for(icvs in seq_along(listCanColor)) tkdestroy(listCanColor[[icvs]])
                tkdelete(textColor, "0.0", "end")
                listCanColor <<- list()
                 for(icvs1 in seq_along(listCol)){
                     tkinsert(textColor, "end", paste0(listCol[icvs1] , ', '))
                     listCanColor[[icvs1]] <<- tkcanvas(frListColor, width = 20, height = 20, bg = listCol[icvs1])
                     pj <- icvs1 - 1
                     tkgrid(listCanColor[[icvs1]], row = floor(pj / kcol), column = pj %% kcol)
                 }
                nlCol <- length(listCol)
                if(nlCol > 0){
                    tkconfigure(canColor, bg = listCol[nlCol])
                    tclvalue(varCol) <- listCol[nlCol]
                    iColor <<- nlCol
                }else iColor <<- 0
            }
        }
    })

    #########

    tkgrid(chkButColor, row = 0, column = 0, sticky = 'w')
    tkgrid(dispButColor, row = 0, column = 1, sticky = 'e')

    ########################

    frPreviewCmd <- tkframe(frDialog)

    dispGradColor <- ttkbutton(frPreviewCmd, text = lang.dlg[['button']][['5']])

    #########
    tkconfigure(dispGradColor, command = function(){
        if(tclvalue(editColor) == '1'){
            vcolor <- tclvalue(tkget(textColor, "0.0", "end"))
            vcolor <- parseKolor(vcolor)
            if(is.null(vcolor)) return(NULL)
            listCol <- vcolor
        }else listCol <- listCol[!is.na(listCol)]

        kolor <- getGradientColor(listCol, 0:canWidth)
        tkdelete(canPreview, 'gradlines')
        for(i in 0:canWidth)
            tkcreate(canPreview, "line", i, 0, i, 20, fill = kolor[i], tags = 'gradlines')
        tcl('update')
    })

    #########

    tkgrid(dispGradColor, sticky = 'e')

    ########################

    frPreviewCanvas <- tkframe(frDialog)

    canWidth <- as.double(tkwinfo('reqwidth', textColor))
    canPreview <- tkcanvas(frPreviewCanvas, width = canWidth, height = 20, bg = 'white')

    tkgrid(canPreview, sticky = 'we')

    ###############################################################

    tkgrid(frCmd, row = 0, column = 0, sticky = 'we', rowspan = 1, columnspan = 2, padx = 1, pady = 1, ipadx = 1, ipady = 1)
    tkgrid(frListColor, row = 1, column = 0, sticky = 'we', rowspan = 1, columnspan = 2, padx = 1, pady = 1, ipadx = 1, ipady = 1)
    tkgrid(frEditCol, row = 2, column = 0, sticky = 'we', rowspan = 1, columnspan = 2, padx = 1, pady = 1, ipadx = 1, ipady = 1)
    tkgrid(frEditCmd, row = 3, column = 0, sticky = 'w', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
    tkgrid(frPreviewCmd, row = 3, column = 1, sticky = 'e', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
    tkgrid(frPreviewCanvas, row = 4, column = 0, sticky = '', rowspan = 1, columnspan = 2, padx = 1, pady = 1, ipadx = 1, ipady = 1)

    ###############################################################

    bt.opt.OK <- ttkbutton(frButt, text = .cdtEnv$tcl$lang$global[['button']][['1']])
    bt.opt.CA <- ttkbutton(frButt, text = .cdtEnv$tcl$lang$global[['button']][['2']])

    tkconfigure(bt.opt.OK, command = function(){
        if(tclvalue(editColor) == '1'){
            vcolor <- tclvalue(tkget(textColor, "0.0", "end"))
            vcolor <- parseKolor(vcolor)
            if(is.null(vcolor)) return(NULL)

            listCol <- vcolor
        }else{
            listCol <- if(length(iColor) == 1 & iColor == 0) NULL else listCol[!is.na(listCol)]
        }

        if(!is.null(listCol)){
            rgbcol <- grDevices::col2rgb(listCol, alpha = TRUE)
            listCol <<- grDevices::rgb(t(rgbcol), maxColorValue = 255)
        }else NULL

        tkgrab.release(tt)
        tkdestroy(tt)
        tkfocus(parent.win)
    })

    tkconfigure(bt.opt.CA, command = function(){
        listCol <<- if(length(iColor) == 1 & iColor == 0) NULL else listCol[!is.na(listCol)]
        tkgrab.release(tt)
        tkdestroy(tt)
        tkfocus(parent.win)
    })

    tkgrid(bt.opt.OK, row = 0, column = 0, padx = 5, pady = 1, ipadx = 1, sticky = 'w')
    tkgrid(bt.opt.CA, row = 0, column = 1, padx = 5, pady = 1, ipadx = 1, sticky = 'e')

    ###############################################################

    tkgrid(frDialog, row = 0, column = 0, sticky = 'nswe', rowspan = 1, columnspan = 2, padx = 1, pady = 1, ipadx = 1, ipady = 1)
    tkgrid(frButt, row = 1, column = 1, sticky = 'se', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)

    ###############################################################
    tkwm.withdraw(tt)
    tcl('update')
    tt.w <- as.integer(tkwinfo("reqwidth", tt))
    tt.h <- as.integer(tkwinfo("reqheight", tt))
    tt.x <- as.integer(.cdtEnv$tcl$data$width.scr*0.5 - tt.w*0.5)
    tt.y <- as.integer(.cdtEnv$tcl$data$height.scr*0.5 - tt.h*0.5)
    tkwm.geometry(tt, paste0('+', tt.x, '+', tt.y))
    tkwm.transient(tt)
    tkwm.title(tt, lang.dlg[['title']])
    tkwm.deiconify(tt)
    tcl('wm', 'attributes', tt, topmost = TRUE)

    ##################################################################
    tkfocus(tt)
    tkbind(tt, "<Destroy>", function(){
        tkgrab.release(tt)
        tkfocus(parent.win)
    })
    tkwait.window(tt)
    return(listCol)
}
