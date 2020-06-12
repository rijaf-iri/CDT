
download_CountryShapefile <- function(){
    if(WindowsOS()){
        largeur <- 56
        largeur1 <- 27
    }else{
        largeur <- 53
        largeur1 <- 27
    }

    xml.dlg <- file.path(.cdtDir$dirLocal, "languages", "cdtDownloadSHP_dlgBox.xml")
    lang.dlg <- cdtLanguageParse(xml.dlg, .cdtData$Config$lang.iso)
    cntr <- readRDS(file.path(.cdtDir$Root, 'data', 'Country.rds'))

    #####
    tt <- tktoplevel()
    tkgrab.set(tt)
    tkfocus(tt)

    frGrd0 <- tkframe(tt, relief = 'raised', borderwidth = 2)
    frGrd1 <- tkframe(tt)

    #####
    frA1 <- tkframe(frGrd0, relief = 'sunken', bd = 2)

    region <- tclVar()
    cbRegionTXT <- c("Africa", "South East Asia", "South-Central America & Caribbean")
    cbRegionVAL <- c("Africa", "Asia", "America")
    tclvalue(region) <- cbRegionTXT[cbRegionVAL %in% .cdtData$GalParams$region]

    cbCountry <- str_trim(cntr$NAME_0[cntr$REG %in% .cdtData$GalParams$region])
    country <- tclVar(.cdtData$GalParams$country)

    selCntr <- cntr[cntr$NAME_0 %in% str_trim(tclvalue(country)), , drop = FALSE]
    cbLevel <- 0:(selCntr$max_lev - 1)
    level_sub <- tclVar(.cdtData$GalParams$level)

    ilev <- as.numeric(str_trim(tclvalue(level_sub)))
    if(ilev > 0){
        type_sub <- as.character(selCntr[1, paste0("TYPE_", ilev)])
        entype_sub <- as.character(selCntr[1, paste0("ENGTYPE_", ilev)])
    }else{
        type_sub <- "Country"
        entype_sub <- "Country"
    }
    level_type <- tclVar(type_sub)
    level_entype <- tclVar(entype_sub)
 
    txt.reg <- tklabel(frA1, text = lang.dlg[['label']][['1']], anchor = 'w', justify = 'left')
    cb.reg <- ttkcombobox(frA1, values = cbRegionTXT, textvariable = region, width = largeur1)
    txt.cntr <- tklabel(frA1, text = lang.dlg[['label']][['2']], anchor = 'w', justify = 'left')
    cb.cntr <- ttkcombobox(frA1, values = cbCountry, textvariable = country, width = largeur1)
    txt.lev <- tklabel(frA1, text = lang.dlg[['label']][['3']], anchor = 'w', justify = 'left')
    cb.lev <- ttkcombobox(frA1, values = cbLevel, textvariable = level_sub, width = 2)
    txt.lev1 <- tklabel(frA1, text = tclvalue(level_type), textvariable = level_type, anchor = 'center', justify = 'center')
    txt.lev2 <- tklabel(frA1, text = tclvalue(level_entype), textvariable = level_entype, anchor = 'center', justify = 'center')

    tkgrid(txt.reg, row = 0, column = 0, sticky = 'we', rowspan = 1, columnspan = 2, padx = 1, pady = 1, ipadx = 1, ipady = 1)
    tkgrid(cb.reg, row = 1, column = 0, sticky = 'we', rowspan = 1, columnspan = 2, padx = 1, pady = 1, ipadx = 1, ipady = 1)
    tkgrid(txt.cntr, row = 0, column = 2, sticky = 'we', rowspan = 1, columnspan = 2, padx = 1, pady = 1, ipadx = 1, ipady = 1)
    tkgrid(cb.cntr, row = 1, column = 2, sticky = 'we', rowspan = 1, columnspan = 2, padx = 1, pady = 1, ipadx = 1, ipady = 1)
    tkgrid(txt.lev, row = 3, column = 0, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
    tkgrid(cb.lev, row = 3, column = 1, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
    tkgrid(txt.lev1, row = 2, column = 2, sticky = 'we', rowspan = 1, columnspan = 2, padx = 1, pady = 1, ipadx = 1, ipady = 1)
    tkgrid(txt.lev2, row = 3, column = 2, sticky = 'we', rowspan = 1, columnspan = 2, padx = 1, pady = 1, ipadx = 1, ipady = 1)

    helpWidget(cb.reg, lang.dlg[['tooltip']][['0']], lang.dlg[['status']][['0']])
    helpWidget(cb.cntr, lang.dlg[['tooltip']][['1']], lang.dlg[['status']][['1']])
    helpWidget(cb.lev, lang.dlg[['tooltip']][['2']], lang.dlg[['status']][['2']])

    ###
    tkbind(cb.reg, "<<ComboboxSelected>>", function(){
        reg <- cbRegionVAL[cbRegionTXT %in% str_trim(tclvalue(region))]
        cbCountry <- str_trim(cntr$NAME_0[cntr$REG %in% reg])

        tkconfigure(cb.cntr, values = cbCountry)
        if(!any(cbCountry %in% str_trim(tclvalue(country))))
            tclvalue(country) <- cbCountry[1]

        selLevel <- cntr[cntr$NAME_0 %in% str_trim(tclvalue(country)), "max_lev"]
        cbLevel <- 0:(selLevel - 1)
        tkconfigure(cb.lev, values = cbLevel)
        tclvalue(level_sub) <- 0
        tclvalue(level_type) <- "Country"
        tclvalue(level_entype) <- "Country"
    })

    tkbind(cb.cntr, "<<ComboboxSelected>>", function(){
        selLevel <- cntr[cntr$NAME_0 %in% str_trim(tclvalue(country)), "max_lev"]
        cbLevel <- 0:(selLevel - 1)
        tkconfigure(cb.lev, values = cbLevel)
        tclvalue(level_sub) <- 0
        tclvalue(level_type) <- "Country"
        tclvalue(level_entype) <- "Country"
    })

    tkbind(cb.lev, "<<ComboboxSelected>>", function(){
        selCntr <- cntr[cntr$NAME_0 %in% str_trim(tclvalue(country)), , drop = FALSE]
        ilev <- as.numeric(str_trim(tclvalue(level_sub)))
        if(ilev > 0){
            tclvalue(level_type) <- as.character(selCntr[1, paste0("TYPE_", ilev)])
            tclvalue(level_entype) <- as.character(selCntr[1, paste0("ENGTYPE_", ilev)])
        }else{
            tclvalue(level_type) <- "Country"
            tclvalue(level_entype) <- "Country"
        }
    })

    #####
    frA2 <- tkframe(frGrd0, relief = 'sunken', bd = 2)

    dir2save <- tclVar(.cdtData$GalParams$dir2save)

    txt.file.save <- tklabel(frA2, text = lang.dlg[['label']][['4']], anchor = 'w', justify = 'left')
    en.file.save <- tkentry(frA2, textvariable = dir2save, width = largeur)
    bt.file.save <- tkbutton(frA2, text = "...")

    ###
    tkconfigure(bt.file.save, command = function(){
        tcl('wm', 'attributes', tt, topmost = FALSE)
        dir2savepth <- tk_choose.dir(.cdtData$GalParams$dir2save, "")
        tcl('wm', 'attributes', tt, topmost = TRUE)
        if(is.na(dir2savepth)){
            tclvalue(dir2save) <- .cdtData$GalParams$dir2save
        }else{
            dir.create(dir2savepth, showWarnings = FALSE, recursive = TRUE)
            tclvalue(dir2save) <- dir2savepth
        }
    })

    ###
    tkgrid(txt.file.save, row = 0, column = 0, sticky = 'we', rowspan = 1, columnspan = 2, padx = 1, pady = 1, ipadx = 1, ipady = 1)
    tkgrid(en.file.save, row = 1, column = 0, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
    tkgrid(bt.file.save, row = 1, column = 1, sticky = 'w', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)

    helpWidget(en.file.save, lang.dlg[['tooltip']][['3']], lang.dlg[['status']][['3']])
    helpWidget(bt.file.save, lang.dlg[['tooltip']][['4']], lang.dlg[['status']][['4']])

    ###
    tkgrid(frA1, row = 0, column = 0, sticky = 'we', rowspan = 1, columnspan = 1, padx = 5, pady = 5, ipadx = 1, ipady = 1)
    tkgrid(frA2, row = 1, column = 0, sticky = 'we', rowspan = 1, columnspan = 1, padx = 5, pady = 5, ipadx = 1, ipady = 1)

    ###
    btOK <- ttkbutton(frGrd1, text = lang.dlg[['button']][['1']])
    btCA <- ttkbutton(frGrd1, text = lang.dlg[['button']][['2']])

    tkconfigure(btOK, command = function(){
        if(str_trim(tclvalue(dir2save)) %in% c("", "NA")){
            cdt.tkmessageBox(tt, message = lang.dlg[['message']][['1']], icon = "warning", type = "ok")
            tkwait.window(tt)
        }else{
            tkgrab.release(tt)
            tkdestroy(tt)
            tkfocus(.cdtEnv$tcl$main$win)
            tcl('update')

            .cdtData$GalParams$dir2save <- str_trim(tclvalue(dir2save))
            .cdtData$GalParams$region <- cbRegionVAL[cbRegionTXT %in% str_trim(tclvalue(region))]
            .cdtData$GalParams$country <- str_trim(tclvalue(country))
            .cdtData$GalParams$level <- as.numeric(str_trim(tclvalue(level_sub)))
            
            cntr_iso <- as.character(cntr[cntr$NAME_0 %in% str_trim(tclvalue(country)), "GID_0"])
            .cdtData$GalParams$cntr_iso3 <- cntr_iso
            .cdtData$GalParams$message <- lang.dlg[['message']]

            if(testConnection()){
                Insert.Messages.Out(lang.dlg[['message']][['3']], TRUE, "i")
                ExecDownload_GADM()
            }else{
                Insert.Messages.Out(lang.dlg[['message']][['2']], format = TRUE)
                return(NULL)
            }
        }
    })

    tkconfigure(btCA, command = function(){
        tkgrab.release(tt)
        tkdestroy(tt)
        tkfocus(.cdtEnv$tcl$main$win)
    })

    tkgrid(btCA, row = 0, column = 0, sticky = 'w', padx = 5, pady = 1, ipadx = 1, ipady = 1)
    tkgrid(btOK, row = 0, column = 1, sticky = 'e', padx = 5, pady = 1, ipadx = 1, ipady = 1)

    #####
    tkgrid(frGrd0, row = 0, column = 0, sticky = 'nswe', rowspan = 1, columnspan = 2, padx = 1, pady = 1, ipadx = 1, ipady = 1)
    tkgrid(frGrd1, row = 1, column = 1, sticky = 'se', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)

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

    tkfocus(tt)
    tkbind(tt, "<Destroy>", function(){
        tkgrab.release(tt)
        tkfocus(.cdtEnv$tcl$main$win)
    })
    tkwait.window(tt)
}
