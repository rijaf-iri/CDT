
cdtUserInfo <- function(){
    if(WindowsOS()){
        largeur0 <- 46
    }else{
        largeur0 <- 42
    }

    xml.dlg <- file.path(.cdtDir$dirLocal, "languages", "cdtUserInformation_dlgBox.xml")
    lang.dlg <- cdtLanguageParse(xml.dlg, .cdtData$Config$lang.iso)

    #####################

    tt <- tktoplevel()
    tkgrab.set(tt)
    tkfocus(tt)

    #####################

    frDialog <- tkframe(tt, relief = 'raised', borderwidth = 2)
    frButt <- tkframe(tt)

    userCountFile <- file.path(.cdtDir$dirLocal, 'config', '.user_count')
    countOpen <- 1
    if(file.exists(userCountFile)){
        tmp <- readRDS(userCountFile)
        countOpen <- tmp + 1
        if(countOpen > 3){
            tkwm.protocol(tt, "WM_DELETE_WINDOW", function(){return(NULL)})
        }
    }
    saveRDS(countOpen, file = userCountFile)

    #####################

    user_cntr_file <- file.path(.cdtDir$Root, "data", "UserInfo_Countries.rds")
    user_countries <- readRDS(user_cntr_file)
    placeholder <- lang.dlg[['label']][['1']]

    var.fullname <- tclVar()
    var.email <- tclVar()
    var.institution <- tclVar()
    var.position <- tclVar()
    var.country <- tclVar()

    txt.fullname <- tklabel(frDialog, text = lang.dlg[['label']][['2']], anchor = 'w', justify = 'left')
    en.fullname <- tkentry(frDialog, textvariable = var.fullname)
    txt.email <- tklabel(frDialog, text = lang.dlg[['label']][['3']], anchor = 'w', justify = 'left')
    en.email <- tkentry(frDialog, textvariable = var.email)
    txt.institut <- tklabel(frDialog, text = lang.dlg[['label']][['4']], anchor = 'w', justify = 'left')
    en.institut <- tkentry(frDialog, textvariable = var.institution)
    txt.position <- tklabel(frDialog, text = lang.dlg[['label']][['5']], anchor = 'w', justify = 'left')
    en.position <- tkentry(frDialog, textvariable = var.position)
    txt.country <- tklabel(frDialog, text = lang.dlg[['label']][['6']], anchor = 'w', justify = 'left')
    cb.country <- ttkcombobox_search(frDialog, values = user_countries, textvariable = var.country, placeholder = placeholder)
    frameDescrp <- tkframe(frDialog, relief = 'groove', borderwidth = 2)

    tkgrid(txt.fullname, row = 0, column = 0, sticky = 'we', padx = 1, pady = 1, ipadx = 1, ipady = 1)
    tkgrid(en.fullname, row = 1, column = 0, sticky = 'we', padx = 1, pady = 1, ipadx = 1, ipady = 1)
    tkgrid(txt.email, row = 2, column = 0, sticky = 'we', padx = 1, pady = 1, ipadx = 1, ipady = 1)
    tkgrid(en.email, row = 3, column = 0, sticky = 'we', padx = 1, pady = 1, ipadx = 1, ipady = 1)
    tkgrid(txt.institut, row = 4, column = 0, sticky = 'we', padx = 1, pady = 1, ipadx = 1, ipady = 1)
    tkgrid(en.institut, row = 5, column = 0, sticky = 'we', padx = 1, pady = 1, ipadx = 1, ipady = 1)
    tkgrid(txt.position, row = 6, column = 0, sticky = 'we', padx = 1, pady = 1, ipadx = 1, ipady = 1)
    tkgrid(en.position, row = 7, column = 0, sticky = 'we', padx = 1, pady = 1, ipadx = 1, ipady = 1)
    tkgrid(txt.country, row = 8, column = 0, sticky = 'we', padx = 1, pady = 1, ipadx = 1, ipady = 1)
    tkgrid(cb.country, row = 9, column = 0, sticky = 'we', padx = 1, pady = 1, ipadx = 1, ipady = 1)
    tkgrid(tklabel(frDialog, text = ""), row = 10)
    tkgrid(frameDescrp, row = 11, column = 0, sticky = 'we', padx = 1, pady = 1, ipadx = 1, ipady = 1)

    #####################

    # xscr <- tkscrollbar(frameDescrp, repeatinterval = 5, orient = "horizontal",
    #                     command = function(...) tkxview(txta, ...))
    yscr <- tkscrollbar(frameDescrp, repeatinterval = 5,
                        command = function(...) tkyview(txta, ...))
    txta <- tktext(frameDescrp, bg = "white", font = "courier", wrap = "word",
                   height = 4, width = largeur0,
                   # xscrollcommand = function(...) tkset(xscr, ...),
                   yscrollcommand = function(...) tkset(yscr, ...))

    ####
    tkgrid(txta, yscr)
    # tkgrid(xscr)
    tkgrid.configure(txta, sticky = "nsew")
    tkgrid.configure(yscr, sticky = "ns")
    # tkgrid.configure(xscr, sticky = "ew")
    tcl("update", "idletasks")

    ####
    font4 <- tkfont.create(family = "courier", size = 11)
    tktag.configure(txta, "font4f", font = font4)
    tkinsert(txta, "end", lang.dlg[['message']][['1']], "font4f")
    tkconfigure(txta, state = 'disabled')

    #####################

    bt.OK <- ttkbutton(frButt, text = lang.dlg[['button']][['1']])

    user_info <- NULL

    tkconfigure(bt.OK, command = function(){
        rep.fullname <- trimws(tclvalue(var.fullname))
        rep.email <- trimws(tclvalue(var.email))
        rep.institution <- trimws(tclvalue(var.institution))
        rep.position <- trimws(tclvalue(var.position))
        rep.country <- trimws(tclvalue(var.country))

        format_email <- "\\<[A-Z0-9._%+-]+@[A-Z0-9.-]+\\.[A-Z]{2,}\\>"
        isEmailOK <- grepl(format_email, rep.email, ignore.case = TRUE)

        if(rep.fullname == "" | nchar(rep.fullname) < 4){
            cdt.tkmessageBox(tt, message = lang.dlg[['message']][['2']], icon = "warning", type = "ok")
            tkwait.window(tt)
        }else if(rep.email == ""){
            cdt.tkmessageBox(tt, message = lang.dlg[['message']][['3']], icon = "warning", type = "ok")
            tkwait.window(tt)
        }else if(!isEmailOK){
            cdt.tkmessageBox(tt, message = lang.dlg[['message']][['4']], icon = "warning", type = "ok")
            tkwait.window(tt)
        }else if(rep.institution == "" | nchar(rep.institution) < 3){
            cdt.tkmessageBox(tt, message = lang.dlg[['message']][['5']], icon = "warning", type = "ok")
            tkwait.window(tt)
        }else if(rep.position == "" | nchar(rep.position) < 4){
            cdt.tkmessageBox(tt, message = lang.dlg[['message']][['6']], icon = "warning", type = "ok")
            tkwait.window(tt)
        }else if(!rep.country %in% user_countries){
            cdt.tkmessageBox(tt, message = lang.dlg[['message']][['7']], icon = "warning", type = "ok")
            tkwait.window(tt)
        }else{
            user_info$fullname <<- rep.fullname
            user_info$email <<- rep.email
            user_info$institution <<- rep.institution
            user_info$position <<- rep.position
            user_info$country <<- rep.country

            if(file.exists(userCountFile)) unlink(userCountFile)

            tkgrab.release(tt)
            tkdestroy(tt)
            tkfocus(.cdtEnv$tcl$main$win)
        }
    })

    tkgrid(bt.OK, row = 0, column = 0, padx = 5, pady = 1, ipadx = 1, sticky = 'e')

    #####################

    tkgrid(frDialog, row = 0, column = 0, sticky = 'nswe', rowspan = 1, columnspan = 2, padx = 4, pady = 1, ipadx = 2, ipady = 1)
    tkgrid(frButt, row = 1, column = 1, sticky = 'se', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)

    tkwm.withdraw(tt)
    tcl('update')
    tt.w <- as.integer(tkwinfo("reqwidth", tt))
    tt.h <- as.integer(tkwinfo("reqheight", tt))
    tt.x <- as.integer(.cdtEnv$tcl$data$width.scr * 0.5 - tt.w * 0.5)
    tt.y <- as.integer(.cdtEnv$tcl$data$height.scr * 0.5 - tt.h * 0.5)
    tkwm.geometry(tt, paste0('+', tt.x, '+', tt.y))
    tkwm.transient(tt)
    tkwm.title(tt, lang.dlg[['title']])
    tkwm.deiconify(tt)
    tcl('wm', 'attributes', tt, topmost = TRUE)

    #####################

    tkfocus(tt)
    tkbind(tt, "<Destroy>", function(){
        tkgrab.release(tt)
        tkfocus(.cdtEnv$tcl$main$win)
    })

    tkwait.window(tt)
    return(user_info)
}
