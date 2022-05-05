
format.input.InfoDate <- function(x, tstep){
    if(tstep == "monthly"){
        paste(c(unlist(x), 1), collapse = "-")
    }else{
        paste(unlist(x), collapse = "-")
    }
}

check.input.InfoDate <- function(x, tstep){
    x <- format.input.InfoDate(x, tstep)
    if(tstep %in% c("daily", "pentad", "dekadal", "monthly"))
        x <- try(as.Date(x, format = "%Y-%m-%d"), silent = TRUE)
    if(tstep == "hourly")
        x <- try(as.POSIXct(x, format = "%Y-%m-%d-%H"), silent = TRUE)
    if(tstep == "minute")
        x <- try(as.POSIXct(x, format = "%Y-%m-%d-%H-%M"), silent = TRUE)

    if(!inherits(x, "try-error")){
        ret <- if(is.na(x)) NULL else 0
    }else ret <- NULL

    return(ret)
}

check.start.end.InfoDate <- function(tstep, date.range){
    suffix <- switch(tstep,
                    "minute" = c('year', 'mon', 'day', 'hour', 'min'),
                    "hourly" = c('year', 'mon', 'day', 'hour'),
                    "daily" = c('year', 'mon', 'day'),
                    "pentad" = c('year', 'mon', 'pen'),
                    "dekadal" = c('year', 'mon', 'dek'),
                    "monthly" = c('year', 'mon'))
    start <- date.range[paste0("start.", suffix)]
    end <- date.range[paste0("end.", suffix)]
    start <- check.input.InfoDate(start, tstep)
    end <- check.input.InfoDate(end, tstep)
    list(start = start, end = end)
}

##############################

getInfoDateRange <- function(parent.win, Parameters, tstep)
{
    largeur0 <- 40

    ###################

    xml.dlg <- file.path(.cdtDir$dirLocal, "languages", "cdtInfoDateRange_dlgBox.xml")
    lang.dlg <- cdtLanguageParse(xml.dlg, .cdtData$Config$lang.iso)

    ###################

    tt <- tktoplevel()
    tkgrab.set(tt)
    tkfocus(tt)

    frMRG0 <- tkframe(tt, relief = 'raised', borderwidth = 2)
    frMRG1 <- tkframe(tt)

    ###################

    labelW1 <- tklabel(frMRG0, text = "", width = largeur0)
    frameDates <- tkframe(frMRG0)
    labelW2 <- tklabel(frMRG0, text = "", width = largeur0)

    ###################

    dtstart <- c("start.year", "start.mon", "start.dek", "start.pen", "start.day", "start.hour", "start.min")
    dtend <- c("end.year", "end.mon", "end.dek", "end.pen", "end.day", "end.hour", "end.min")
    datytime <- c(dtstart, dtend)
    state <- pars <- vector("list", length(datytime))
    names(state) <- names(pars) <- c(dtstart, dtend)
    for(v in datytime){
        pars[[v]] <- ""
        if(!is.null(Parameters[[v]])){
            pars[[v]] <- if(is.na(Parameters[[v]])) "" else Parameters[[v]]
        }
    }
    Parameters <- pars
    state[] <- "disabled"

    ix <- switch(tstep,
        "annual" = 1,
        "monthly" = 1:2,
        "dekadal" = 1:3,
        "pentad" = c(1:2, 4),
        "daily" = c(1:2, 5),
        "hourly" = c(1:2, 5:6),
        "minute" = c(1:2, 5:7),
        NA)

    if(all(!is.na(ix)))
        state[c(dtstart[ix], dtend[ix])] <- "normal"

    ###################

    frDatyR <- tkframe(frameDates)

    istart.yrs <- tclVar(Parameters$start.year)
    istart.mon <- tclVar(Parameters$start.mon)
    istart.dek <- tclVar(Parameters$start.dek)
    istart.pen <- tclVar(Parameters$start.pen)
    istart.day <- tclVar(Parameters$start.day)
    istart.hour <- tclVar(Parameters$start.hour)
    istart.min <- tclVar(Parameters$start.min)

    iend.yrs <- tclVar(Parameters$end.year)
    iend.mon <- tclVar(Parameters$end.mon)
    iend.dek <- tclVar(Parameters$end.dek)
    iend.pen <- tclVar(Parameters$end.pen)
    iend.day <- tclVar(Parameters$end.day)
    iend.hour <- tclVar(Parameters$end.hour)
    iend.min <- tclVar(Parameters$end.min)

    deb.txt <- tklabel(frDatyR, text = paste0(lang.dlg[['label']][['1']], ":"), anchor = 'e', justify = 'right')
    fin.txt <- tklabel(frDatyR, text = paste0(lang.dlg[['label']][['2']], ":"), anchor = 'e', justify = 'right')
    yrs.txt <- tklabel(frDatyR, text = lang.dlg[['label']][['3']])
    mon.txt <- tklabel(frDatyR, text = lang.dlg[['label']][['4']])
    dek.txt <- tklabel(frDatyR, text = lang.dlg[['label']][['5']])
    pen.txt <- tklabel(frDatyR, text = lang.dlg[['label']][['6']])
    day.txt <- tklabel(frDatyR, text = lang.dlg[['label']][['7']])
    hour.txt <- tklabel(frDatyR, text = lang.dlg[['label']][['8']])
    min.txt <- tklabel(frDatyR, text = lang.dlg[['label']][['9']])

    yrs1.v <- tkentry(frDatyR, width = 4, textvariable = istart.yrs, justify = "center", state = state$start.year)
    mon1.v <- tkentry(frDatyR, width = 4, textvariable = istart.mon, justify = "center", state = state$start.mon)
    dek1.v <- tkentry(frDatyR, width = 4, textvariable = istart.dek, justify = "center", state = state$start.dek)
    pen1.v <- tkentry(frDatyR, width = 4, textvariable = istart.pen, justify = "center", state = state$start.pen)
    day1.v <- tkentry(frDatyR, width = 4, textvariable = istart.day, justify = "center", state = state$start.day)
    hour1.v <- tkentry(frDatyR, width = 4, textvariable = istart.hour, justify = "center", state = state$start.hour)
    min1.v <- tkentry(frDatyR, width = 4, textvariable = istart.min, justify = "center", state = state$start.min)

    yrs2.v <- tkentry(frDatyR, width = 4, textvariable = iend.yrs, justify = "center", state = state$end.year)
    mon2.v <- tkentry(frDatyR, width = 4, textvariable = iend.mon, justify = "center", state = state$end.mon)
    dek2.v <- tkentry(frDatyR, width = 4, textvariable = iend.dek, justify = "center", state = state$end.dek)
    pen2.v <- tkentry(frDatyR, width = 4, textvariable = iend.pen, justify = "center", state = state$end.pen)
    day2.v <- tkentry(frDatyR, width = 4, textvariable = iend.day, justify = "center", state = state$end.day)
    hour2.v <- tkentry(frDatyR, width = 4, textvariable = iend.hour, justify = "center", state = state$end.hour)
    min2.v <- tkentry(frDatyR, width = 4, textvariable = iend.min, justify = "center", state = state$end.min)

    ###################
    tkgrid(deb.txt, row = 1, column = 0, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
    tkgrid(fin.txt, row = 2, column = 0, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)

    tkgrid(yrs.txt, row = 0, column = 1, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
    tkgrid(yrs1.v, row = 1, column = 1, sticky = '', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
    tkgrid(yrs2.v, row = 2, column = 1, sticky = '', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)

    tkgrid(mon.txt, row = 0, column = 2, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
    tkgrid(mon1.v, row = 1, column = 2, sticky = '', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
    tkgrid(mon2.v, row = 2, column = 2, sticky = '', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)

    if(tstep == "dekadal"){
        tkgrid(dek.txt, row = 0, column = 3, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
        tkgrid(dek1.v, row = 1, column = 3, sticky = '', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
        tkgrid(dek2.v, row = 2, column = 3, sticky = '', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
    }

    if(tstep == "pentad"){
        tkgrid(pen.txt, row = 0, column = 4, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
        tkgrid(pen1.v, row = 1, column = 4, sticky = '', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
        tkgrid(pen2.v, row = 2, column = 4, sticky = '', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
    }

    if(tstep %in% c("minute", "hourly", "daily", "monthly", "annual")){
        tkgrid(day.txt, row = 0, column = 5, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
        tkgrid(day1.v, row = 1, column = 5, sticky = '', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
        tkgrid(day2.v, row = 2, column = 5, sticky = '', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
    }

    if(tstep %in% c("minute", "hourly")){
        tkgrid(hour.txt, row = 0, column = 6, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
        tkgrid(hour1.v, row = 1, column = 6, sticky = '', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
        tkgrid(hour2.v, row = 2, column = 6, sticky = '', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
    }

    if(tstep == "minute"){
        tkgrid(min.txt, row = 0, column = 7, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
        tkgrid(min1.v, row = 1, column = 7, sticky = '', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
        tkgrid(min2.v, row = 2, column = 7, sticky = '', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
    }

    tkgrid(frDatyR, row = 0, column = 0, sticky = 'we', padx = 1, pady = 1, ipadx = 1, ipady = 1)

    ###################

    tkgrid(labelW1, row = 0, column = 0, sticky = 'we', padx = 1, pady = 1, ipadx = 1, ipady = 1)
    tkgrid(frameDates, row = 1, column = 0, sticky = 'w', padx = 1, pady = 1, ipadx = 1, ipady = 1)
    tkgrid(labelW2, row = 2, column = 0, sticky = 'we', padx = 1, pady = 1, ipadx = 1, ipady = 1)

    ################################

    bt.prm.OK <- ttkbutton(frMRG1, text = .cdtEnv$tcl$lang$global[['button']][['1']])
    bt.prm.CA <- ttkbutton(frMRG1, text = .cdtEnv$tcl$lang$global[['button']][['2']])

    tkconfigure(bt.prm.OK, command = function(){
        Parameters$start.year <<- as.numeric(str_trim(tclvalue(istart.yrs)))
        Parameters$start.mon <<- as.numeric(str_trim(tclvalue(istart.mon)))
        Parameters$start.dek <<- as.numeric(str_trim(tclvalue(istart.dek)))
        Parameters$start.pen <<- as.numeric(str_trim(tclvalue(istart.pen)))
        Parameters$start.day <<- as.numeric(str_trim(tclvalue(istart.day)))
        Parameters$start.hour <<- as.numeric(str_trim(tclvalue(istart.hour)))
        Parameters$start.min <<- as.numeric(str_trim(tclvalue(istart.min)))

        Parameters$end.year <<- as.numeric(str_trim(tclvalue(iend.yrs)))
        Parameters$end.mon <<- as.numeric(str_trim(tclvalue(iend.mon)))
        Parameters$end.dek <<- as.numeric(str_trim(tclvalue(iend.dek)))
        Parameters$end.pen <<- as.numeric(str_trim(tclvalue(iend.pen)))
        Parameters$end.day <<- as.numeric(str_trim(tclvalue(iend.day)))
        Parameters$end.hour <<- as.numeric(str_trim(tclvalue(iend.hour)))
        Parameters$end.min <<- as.numeric(str_trim(tclvalue(iend.min)))

        ina <- sapply(Parameters[c(dtstart[ix], dtend[ix])], is.na)
        if(any(ina)){
            cdt.tkmessageBox(tt, message = lang.dlg[['message']][['1']], icon = "warning", type = "ok")
            tkwait.window(tt)
        }else if(tstep != "annual" &
            (Parameters$start.mon > 12 | Parameters$end.mon > 12))
        {
            cdt.tkmessageBox(tt, message = lang.dlg[['message']][['2']], icon = "warning", type = "ok")
            tkwait.window(tt)
        }else if(tstep == "dekadal" &
            (Parameters$start.dek > 3 | Parameters$end.dek > 3))
        {
            cdt.tkmessageBox(tt, message = lang.dlg[['message']][['3']], icon = "warning", type = "ok")
            tkwait.window(tt)
        }else if(tstep == "pentad" &
            (Parameters$start.pen > 6 | Parameters$end.pen > 6))
        {
            cdt.tkmessageBox(tt, message = lang.dlg[['message']][['4']], icon = "warning", type = "ok")
            tkwait.window(tt)
        }else if(tstep %in% c("minute", "hourly", "daily") &
            (Parameters$start.day > 31 | Parameters$end.day > 31))
        {
            cdt.tkmessageBox(tt, message = lang.dlg[['message']][['5']], icon = "warning", type = "ok")
            tkwait.window(tt)
        }else if(tstep %in% c("minute", "hourly") &
            (Parameters$start.hour > 23 | Parameters$end.hour > 23))
        {
            cdt.tkmessageBox(tt, message = lang.dlg[['message']][['6']], icon = "warning", type = "ok")
            tkwait.window(tt)
        }else if(tstep == "minute" &
            (Parameters$start.min > 59 | Parameters$end.min > 59))
        {
            cdt.tkmessageBox(tt, message = lang.dlg[['message']][['7']], icon = "warning", type = "ok")
            tkwait.window(tt)
        }else{
            startend <- check.start.end.InfoDate(tstep, Parameters)

            if(is.null(startend$start)){
                cdt.tkmessageBox(tt, message = lang.dlg[['message']][['8']], icon = "warning", type = "ok")
                tkwait.window(tt)
            }

            if(is.null(startend$end)){
                cdt.tkmessageBox(tt, message = lang.dlg[['message']][['9']], icon = "warning", type = "ok")
                tkwait.window(tt)
            }

            tkgrab.release(tt)
            tkdestroy(tt)
            tkfocus(parent.win)
        }
    })

    tkconfigure(bt.prm.CA, command = function(){
        tkgrab.release(tt)
        tkdestroy(tt)
        tkfocus(parent.win)
    })

    tkgrid(bt.prm.OK, row = 0, column = 0, sticky = 'w', padx = 5, pady = 1, ipadx = 1, ipady = 1)
    tkgrid(bt.prm.CA, row = 0, column = 1, sticky = 'e', padx = 5, pady = 1, ipadx = 1, ipady = 1)

    ################################
    tkgrid(frMRG0, row = 0, column = 0, sticky = 'nswe', rowspan = 1, columnspan = 2, padx = 1, pady = 1, ipadx = 1, ipady = 1)
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
