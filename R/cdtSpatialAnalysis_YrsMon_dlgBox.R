
spatialAnalysisEditYrsMon <- function(parent.win, vedit, is.year){
    largeur1 <- if(WindowsOS()) 30 else 36

    xml.dlg <- file.path(.cdtDir$dirLocal, "languages", "cdtSpatialAnalysis_YrsMon_dlgBox.xml")
    lang.dlg <- cdtLanguageParse(xml.dlg, .cdtData$Config$lang.iso)

    #########
    tt <- tktoplevel()
    tkgrab.set(tt)
    tkfocus(tt)

    frDialog <- tkframe(tt, relief = 'raised', borderwidth = 2)
    frButt <- tkframe(tt)

    ####
    frameEdit <- tkframe(frDialog)
    frameInfo <- tkframe(frDialog)

    #########
    yscr.Edit <- tkscrollbar(frameEdit, repeatinterval = 4,
                             command = function(...) tkyview(text.Edit, ...))
    text.Edit <- tktext(frameEdit, bg = "white", wrap = "word",
                        height = 4, width = largeur1,
                        yscrollcommand = function(...) tkset(yscr.Edit, ...))

    tkgrid(text.Edit, yscr.Edit)
    tkgrid.configure(yscr.Edit, sticky = "ns")
    tkgrid.configure(text.Edit, sticky = 'nswe')

    if(length(vedit) == 1){
        if(is.na(vedit)) vedit <- ''
    }
    if(length(vedit) == 0) vedit <- ''
    vedit <- paste0(vedit, collapse = ', ')

    tkinsert(text.Edit, "end", vedit)

    #########

    yscr.Info <- tkscrollbar(frameInfo, repeatinterval = 4, command = function(...) tkyview(txta.Info, ...))
    txta.Info <- tktext(frameInfo, cursor = "", wrap = "word", height = 4, width = largeur1,
                        yscrollcommand = function(...) tkset(yscr.Info, ...))

    tkgrid(txta.Info, yscr.Info)
    tkgrid.configure(yscr.Info, sticky = "ns")
    tkgrid.configure(txta.Info, sticky = 'nswe')

    ihlp <- if(is.year) '1' else '2'

    tkinsert(txta.Info, "1.0", lang.dlg[['message']][[ihlp]])
    tkconfigure(txta.Info, state = "disabled")

    #########
    tkgrid(frameEdit, row = 0, column = 0, sticky = 'we', rowspan = 1, columnspan = 1, padx = 2, pady = 2, ipadx = 1, ipady = 1)
    tkgrid(frameInfo, row = 1, column = 0, sticky = 'we', rowspan = 1, columnspan = 1, padx = 2, pady = 2, ipadx = 1, ipady = 1)

    #########

    if(is.year){
        xlo <- 1800
        xup <- 2200
    }else{
        xlo <- 1
        xup <- 12
    }

    #########
    bt.opt.OK <- ttkbutton(frButt, text = .cdtEnv$tcl$lang$global[['button']][['1']]) 
    bt.opt.CA <- ttkbutton(frButt, text = .cdtEnv$tcl$lang$global[['button']][['2']]) 

    tkconfigure(bt.opt.OK, command = function(){
        tmp <- tclvalue(tkget(text.Edit, "0.0", "end"))
        tmp <- gsub("[\r\n]", "", tmp)
        tmp <- trimws(strsplit(tmp, ",")[[1]])
        tmp <- tmp[tmp != ""]
        tmp <- as.numeric(tmp)
        if(length(tmp) == 0){
            ix <- if(is.year) '3' else '4'
            tkmessageBox(message = lang.dlg[['message']][[ix]], icon = "warning", type = "ok")
            tkwait.window(tt)
        }else if(length(tmp) > 0 & any(is.na(tmp))){
            ix <- if(is.year) '5' else '6'
            msg <- paste0(lang.dlg[['message']][[ix]], '\n', paste0(tmp, collapse = ', '))
            tkmessageBox(message = msg, icon = "warning", type = "ok")
            tkwait.window(tt)
        }else if(any(tmp > xup | tmp < xlo)){
            ix <- if(is.year) '7' else '8'
            msg <- paste0(lang.dlg[['message']][[ix]], "\n", paste0(tmp, collapse = ', '))
            tkmessageBox(message = msg, icon = "warning", type = "ok")
            tkwait.window(tt)
        }else{
            vedit <<- tmp
            tkgrab.release(tt)
            tkdestroy(tt)
            tkfocus(parent.win)
        }
    })

    tkconfigure(bt.opt.CA, command = function(){
        tkgrab.release(tt)
        tkdestroy(tt)
        tkfocus(parent.win)
    })

    tkgrid(bt.opt.OK, row = 0, column = 0, padx = 5, pady = 1, ipadx = 1, sticky = 'w')
    tkgrid(bt.opt.CA, row = 0, column = 1, padx = 5, pady = 1, ipadx = 1, sticky = 'e')

    ############################################################### 

    tkgrid(frDialog, row = 0, column = 0, sticky = 'nswe', rowspan = 1, columnspan = 2, padx = 1, pady = 1, ipadx = 1, ipady = 1)
    tkgrid(frButt, row = 1, column = 1, sticky = 'se', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)

    tkwm.withdraw(tt)
    tcl('update')
    tt.w <- as.integer(tkwinfo("reqwidth", tt))
    tt.h <- as.integer(tkwinfo("reqheight", tt))
    tt.x <- as.integer(.cdtEnv$tcl$data$width.scr*0.5 - tt.w*0.5)
    tt.y <- as.integer(.cdtEnv$tcl$data$height.scr*0.5 - tt.h*0.5)
    tkwm.geometry(tt, paste0('+', tt.x, '+', tt.y))
    tkwm.transient(tt)
    ix <- if(is.year) '1' else '2'
    tkwm.title(tt, lang.dlg[['tab_title']][[ix]])
    tkwm.deiconify(tt)
    tcl('wm', 'attributes', tt, topmost = TRUE)

    tkfocus(tt)
    tkbind(tt, "<Destroy>", function(){
        tkgrab.release(tt)
        tkfocus(parent.win)
    })
    tkwait.window(tt)
    return(vedit)
}
