
getInfo_volumetricValidNetCDF <- function(parent.win, Parameters){
    if(WindowsOS()){
        largeur0 <- 36
        largeur1 <- 18
    }else{
        largeur0 <- 34
        largeur1 <- 18
    }

    ###################

    xml.dlg <- file.path(.cdtDir$dirLocal, "languages", "cdtValidation_volumetricStatNetCDF_dlgBox.xml")
    lang.dlg <- cdtLanguageParse(xml.dlg, .cdtData$Config$lang.iso)

    ###################

    tt <- tktoplevel()
    tkgrab.set(tt)
    tkfocus(tt)

    frMRG0 <- tkframe(tt, relief = 'raised', borderwidth = 2)
    frMRG1 <- tkframe(tt)

    ################################

    userVal <- tclVar(Parameters$user)

    chk.user <- tkcheckbutton(frMRG0, variable = userVal, text = lang.dlg[['checkbutton']][['1']], anchor = 'w', justify = 'left')

    helpWidget(chk.user, lang.dlg[['tooltip']][['1']], lang.dlg[['status']][['1']])

    tkbind(chk.user, "<Button-1>", function(){
        stateUser <- if(tclvalue(userVal) == '1') 'disabled' else 'normal'
        tkconfigure(chk.onethres, state = stateUser)
        if(tclvalue(unique.thres) == '0'){
            tkconfigure(widgetIN$cb.ncfl, state = stateUser)
            tkconfigure(widgetIN$bt.ncfl, state = stateUser)
        }else{
            tkconfigure(widgetIN$en.thres, state = stateUser)
        }

        stateCalc <- if(tclvalue(userVal) == '1') 'normal' else 'disabled'
        tkconfigure(cb.percData, state = stateCalc)
        tkconfigure(en.percVal, state = stateCalc)

        stateBaseP <- if(tclvalue(userVal) == '1') 'normal' else 'disabled'
        tkconfigure(bt.BasePeriod, state = stateBaseP)
    })

    #############
    widgetIN <- new.env()
    widgetIN$en.thres <- NULL
    widgetIN$cb.ncfl <- NULL
    widgetIN$bt.ncfl <- NULL

    uniqueThresFun <- function(){
        txt.thres <- tklabel(frameThres, text = lang.dlg[['label']][['1']], anchor = 'e', justify = 'right')
        widgetIN$en.thres <- tkentry(frameThres, textvariable = user.thres, width = 4, state = stateUser)

        tkgrid(txt.thres, row = 0, column = 0, sticky = 'e', padx = 1, pady = 1, ipadx = 1, ipady = 1)
        tkgrid(widgetIN$en.thres, row = 0, column = 1, sticky = 'we', padx = 1, pady = 1, ipadx = 1, ipady = 1)

        helpWidget(widgetIN$en.thres, lang.dlg[['tooltip']][['2']], lang.dlg[['status']][['2']])
    }

    multipleThresFun <- function(){
        txt.ncfl <- tklabel(frameThres, text = lang.dlg[['label']][['2']], anchor = 'w', justify = 'left')
        widgetIN$cb.ncfl <- ttkcombobox(frameThres, values = unlist(openFile_ttkcomboList()), textvariable = file.ncdf, width = largeur0, state = stateUser)
        widgetIN$bt.ncfl <- tkbutton(frameThres, text = "...", state = stateUser)

        tkgrid(txt.ncfl, row = 0, column = 0, sticky = 'we', rowspan = 1, columnspan = 10, padx = 1, pady = 0, ipadx = 1, ipady = 1)
        tkgrid(widgetIN$cb.ncfl, row = 1, column = 0, sticky = 'we', rowspan = 1, columnspan = 9, padx = 0, pady = 0, ipadx = 1, ipady = 1)
        tkgrid(widgetIN$bt.ncfl, row = 1, column = 9, sticky = 'we', rowspan = 1, columnspan = 1, padx = 0, pady = 0, ipadx = 1, ipady = 1)

        helpWidget(widgetIN$cb.ncfl, lang.dlg[['tooltip']][['3']], lang.dlg[['status']][['3']])
        helpWidget(widgetIN$bt.ncfl, lang.dlg[['tooltip']][['4']], lang.dlg[['status']][['4']])

        #####
        tkconfigure(widgetIN$bt.ncfl, command = function(){
            tcl('wm', 'attributes', tt, topmost = FALSE)
            nc.opfiles <- getOpenNetcdf(tt, initialdir = getwd())
            tcl('wm', 'attributes', tt, topmost = TRUE)
            if(!is.null(nc.opfiles)){
                update.OpenFiles('netcdf', nc.opfiles)
                tclvalue(file.ncdf) <- nc.opfiles[[1]]
                tkconfigure(widgetIN$cb.ncfl, values = unlist(openFile_ttkcomboList()))
            }
        })
    }

    #############
    fr.user <- tkframe(frMRG0, relief = 'groove', borderwidth = 2)

    unique.thres <- tclVar(Parameters$one.thres)
    user.thres <- tclVar(Parameters$user.val)
    file.ncdf <- tclVar(Parameters$user.file)

    stateUser <- if(Parameters$user) 'normal' else 'disabled'

    chk.onethres <- tkcheckbutton(fr.user, variable = unique.thres, text = lang.dlg[['checkbutton']][['2']], anchor = 'w', justify = 'left', state = stateUser)
    frameThres <- tkframe(fr.user)

    if(Parameters$one.thres) uniqueThresFun() else multipleThresFun()

    tkgrid(chk.onethres, row = 0, column = 0, sticky = 'we', padx = 1, pady = 1, ipadx = 1, ipady = 1)
    tkgrid(frameThres, row = 1, column = 0, sticky = 'we', padx = 1, pady = 1, ipadx = 1, ipady = 1)

    ######
    tkbind(chk.onethres, "<Button-1>", function(){
        if(tclvalue(userVal) == '1'){
            tkdestroy(frameThres)
            frameThres <<- tkframe(fr.user)

            if(tclvalue(unique.thres) == '1') multipleThresFun() else uniqueThresFun()

            tkgrid(frameThres, row = 1, column = 0, sticky = 'we', padx = 1, pady = 1, ipadx = 1, ipady = 1)
        }
    })

    #############
    fr.calc <- tkframe(frMRG0, relief = 'groove', borderwidth = 2)

    stateCalc <- if(Parameters$user) 'disabled' else 'normal'

    #######
    frameP1 <- tkframe(fr.calc)

    CbpercDATA <- lang.dlg[['combobox']][['1']]
    percDATA <- c("obs", "est")

    perc.data <- tclVar()
    tclvalue(perc.data) <- CbpercDATA[percDATA %in% Parameters$from]

    txt.percData <- tklabel(frameP1, text = lang.dlg[['label']][['3']], anchor = "e", justify = "right")
    cb.percData <- ttkcombobox(frameP1, values = CbpercDATA, textvariable = perc.data, justify = 'center', width = largeur1, state = stateCalc)

    tkgrid(txt.percData, row = 0, column = 0, sticky = 'e', padx = 1, pady = 1, ipadx = 1, ipady = 1)
    tkgrid(cb.percData, row = 0, column = 1, sticky = 'we', padx = 1, pady = 1, ipadx = 1, ipady = 1)

    helpWidget(cb.percData, lang.dlg[['tooltip']][['5']], lang.dlg[['status']][['5']])

    #######
    frameP2 <- tkframe(fr.calc)

    percVal <- tclVar(Parameters$perc)

    txt.percVal1 <- tklabel(frameP2, text = lang.dlg[['label']][['4']], anchor = "e", justify = "right")
    en.percVal <- tkentry(frameP2, textvariable = percVal, width = 3, state = stateCalc, justify = "right")
    txt.percVal2 <- tklabel(frameP2, text = lang.dlg[['label']][['5']], anchor = "w", justify = "left")

    tkgrid(txt.percVal1, row = 0, column = 0, sticky = 'e', padx = 1, pady = 1, ipadx = 1, ipady = 1)
    tkgrid(en.percVal, row = 0, column = 1, sticky = 'we', padx = 1, pady = 1, ipadx = 1, ipady = 1)
    tkgrid(txt.percVal2, row = 0, column = 2, sticky = 'w', padx = 0, pady = 1, ipadx = 0, ipady = 1)

    helpWidget(en.percVal, lang.dlg[['tooltip']][['6']], lang.dlg[['status']][['6']])

    #######

    stateBaseP <- if(!Parameters$user) 'normal' else 'disabled'

    bt.BasePeriod <- ttkbutton(fr.calc, text = lang.dlg[['button']][['1']], state = stateBaseP)

    tkconfigure(bt.BasePeriod, command = function(){
        tcl('wm', 'attributes', tt, topmost = FALSE)
        Parameters$period <<- getInfoBasePeriod(tt, Parameters$period)
        tcl('wm', 'attributes', tt, topmost = TRUE)
    })

    helpWidget(bt.BasePeriod, lang.dlg[['tooltip']][['7']], lang.dlg[['status']][['7']])

    #############

    tkgrid(frameP1, row = 0, column = 0, sticky = 'we', columnspan = 2, padx = 1, pady = 1, ipadx = 1, ipady = 1)
    tkgrid(frameP2, row = 1, column = 0, sticky = 'we', columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
    tkgrid(bt.BasePeriod, row = 1, column = 1, sticky = 'we', columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)

    ################################

    tkgrid(chk.user, row = 0, column = 0, sticky = 'we', padx = 5, pady = 5, ipadx = 1, ipady = 1)
    tkgrid(fr.user, row = 1, column = 0, sticky = '', padx = 5, pady = 1, ipadx = 1, ipady = 1)
    tkgrid(fr.calc, row = 2, column = 0, sticky = 'we', padx = 5, pady = 1, ipadx = 1, ipady = 1)

    ################################

    bt.prm.OK <- ttkbutton(frMRG1, text = .cdtEnv$tcl$lang$global[['button']][['1']])
    bt.prm.CA <- ttkbutton(frMRG1, text = .cdtEnv$tcl$lang$global[['button']][['2']])

    tkconfigure(bt.prm.OK, command = function(){
        Parameters$user <<- switch(tclvalue(userVal), '0' = FALSE, '1' = TRUE)
        Parameters$one.thres <<- switch(tclvalue(unique.thres), '0' = FALSE, '1' = TRUE)
        Parameters$user.val <<- as.numeric(str_trim(tclvalue(user.thres)))
        Parameters$user.file <<- str_trim(tclvalue(file.ncdf))

        if(Parameters$user & !Parameters$one.thres){
            if(Parameters$user.file == ""){
                cdt.tkmessageBox(tt, message = lang.dlg[['message']][['1']], icon = "warning", type = "ok")
                tkwait.window(tt)
            }
        }

        Parameters$from <<- percDATA[CbpercDATA %in% str_trim(tclvalue(perc.data))]
        if(Parameters$from == "") Parameters$from <<- "obs"
        Parameters$perc <<- as.numeric(str_trim(tclvalue(percVal)))

        tkgrab.release(tt)
        tkdestroy(tt)
        tkfocus(parent.win)
    })

    tkconfigure(bt.prm.CA, command = function(){
        tkgrab.release(tt)
        tkdestroy(tt)
        tkfocus(parent.win)
    })

    tkgrid(bt.prm.OK, row = 0, column = 0, sticky = 'w', padx = 5, pady = 1, ipadx = 1, ipady = 1)
    tkgrid(bt.prm.CA, row = 0, column = 1, sticky = 'e', padx = 5, pady = 1, ipadx = 1, ipady = 1)

    ################################
    tkgrid(frMRG0, row = 0, column = 0, sticky = 'nswe', rowspan = 1, columnspan = 2, padx = 5, pady = 5, ipadx = 1, ipady = 1)
    tkgrid(frMRG1, row = 1, column = 1, sticky = 'se', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)

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
        tkfocus(parent.win)
    })
    tkwait.window(tt)
    return(Parameters)
}
