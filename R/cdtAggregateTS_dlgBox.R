
AggregateTS_GetInfo <- function(){
    listOpenFiles <- openFile_ttkcomboList()
    if(WindowsOS()){
        largeur0 <- 47
        largeur1 <- 45
        largeur2 <- 33
        wtkcombo <- 19
    }else{
        largeur0 <- 40
        largeur1 <- 39
        largeur2 <- 30
        wtkcombo <- 18
    }

    xml.dlg <- file.path(.cdtDir$dirLocal, "languages", "cdtAggregateTS_dlgBox.xml")
    lang.dlg <- cdtLanguageParse(xml.dlg, .cdtData$Config$lang.iso)

    ###################

    tt <- tktoplevel()
    tkgrab.set(tt)
    tkfocus(tt)

    frMRG0 <- tkframe(tt, relief = 'raised', borderwidth = 2)
    frMRG1 <- tkframe(tt)
    frAGGRTS <- tkframe(frMRG0, relief = "sunken", borderwidth = 2)

    ############################################

    frConvTS <- ttklabelframe(frAGGRTS, text = lang.dlg[['label']][['1']], labelanchor = "nw", relief = "groove", borderwidth = 2)

    ####################

    frameTS <- tkframe(frConvTS)

    OriginData <- tclVar()
    Cbperiod0VAL <- .cdtEnv$tcl$lang$global[['combobox']][['1']][1:6]
    period0VAL <- c('minute', 'hourly', 'daily', 'pentad', 'dekadal', 'monthly')
    tclvalue(OriginData) <- Cbperiod0VAL[period0VAL %in% .cdtData$GalParams$in.tstep]

    CbperiodVAL <- .cdtEnv$tcl$lang$global[['combobox']][['1']][1:9]
    periodVAL <- c('minute', 'hourly', 'daily', 'pentad', 'dekadal', 'monthly', 'annual', 'seasonal', 'roll.seas')

    if(.cdtData$GalParams$in.tstep == 'minute'){
        Cbperiod1VAL <- CbperiodVAL[1:3]
        period1VAL <- periodVAL[1:3]
    }
    if(.cdtData$GalParams$in.tstep == 'hourly'){
        Cbperiod1VAL <- CbperiodVAL[2:3]
        period1VAL <- periodVAL[2:3]
    }
    if(.cdtData$GalParams$in.tstep == 'daily'){
        Cbperiod1VAL <- CbperiodVAL[4:9]
        period1VAL <- periodVAL[4:9]
    }
    if(.cdtData$GalParams$in.tstep == 'pentad'){
        Cbperiod1VAL <- CbperiodVAL[5:9]
        period1VAL <- periodVAL[5:9]
    }
    if(.cdtData$GalParams$in.tstep == 'dekadal'){
        Cbperiod1VAL <- CbperiodVAL[6:9]
        period1VAL <- periodVAL[6:9]
    }
    if(.cdtData$GalParams$in.tstep == 'monthly'){
        Cbperiod1VAL <- CbperiodVAL[7:9]
        period1VAL <- periodVAL[7:9]
    }

    ConvertData <- tclVar()
    tclvalue(ConvertData) <- Cbperiod1VAL[period1VAL %in% .cdtData$GalParams$out.tstep]

    cb.intstep <- ttkcombobox(frameTS, values = Cbperiod0VAL, textvariable = OriginData, width = wtkcombo)
    cb.outstep <- ttkcombobox(frameTS, values = Cbperiod1VAL, textvariable = ConvertData, width = wtkcombo)
    txt.convTs <- tklabel(frameTS, text = paste("-", lang.dlg[['label']][['2']], "-"))

    tkgrid(cb.intstep, row = 0, column = 0, sticky = 'we', rowspan = 1, columnspan = 8, padx = 1, pady = 1)
    tkgrid(txt.convTs, row = 0, column = 8, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1)
    tkgrid(cb.outstep, row = 0, column = 9, sticky = 'we', rowspan = 1, columnspan = 8, padx = 1, pady = 1)

    helpWidget(cb.intstep, lang.dlg[['tooltip']][['1']], lang.dlg[['status']][['1']])
    helpWidget(cb.outstep, lang.dlg[['tooltip']][['2']], lang.dlg[['status']][['2']])

    ####################

    frameMH <- tkframe(frConvTS, relief = "sunken", borderwidth = 2)

    mnhr1 <- .cdtData$GalParams$HourMin$int
    if(is.na(mnhr1)) mnhr1 <- ""
    mnhr2 <- .cdtData$GalParams$HourMin$out
    if(is.na(mnhr2)) mnhr2 <- ""

    minhour.in <- tclVar(mnhr1)
    minhour.out <- tclVar(mnhr2)

    CbminhourVAL0 <- ""
    CbminhourVAL1 <- ""

    minhour0.txtVar <- tclVar(lang.dlg[['label']][['12']])
    minhour1.txtVar <- tclVar(lang.dlg[['label']][['11']])

    if(str_trim(tclvalue(OriginData)) %in% Cbperiod0VAL[1:2]){
        state.mhI <- "normal"
        state.mhO <- if(str_trim(tclvalue(ConvertData)) %in% Cbperiod0VAL[1:2]) "normal" else "disabled"

        ## min
        if(str_trim(tclvalue(OriginData)) == Cbperiod0VAL[1]){
            CbminhourVAL0 <- c(5, 10, 15, 30)
            tclvalue(minhour0.txtVar) <- lang.dlg[['label']][['12']]

            if(!as.numeric(str_trim(tclvalue(minhour.in))) %in% CbminhourVAL0)
                tclvalue(minhour.in) <- CbminhourVAL0[1]

            ## min
            if(str_trim(tclvalue(ConvertData)) == Cbperiod0VAL[1]){
                if(as.numeric(str_trim(tclvalue(minhour.in))) == 5){
                    CbminhourVAL1 <- c(10, 15, 30)
                    tclvalue(minhour1.txtVar) <- lang.dlg[['label']][['12']]
                }

                if(as.numeric(str_trim(tclvalue(minhour.in))) == 10){
                    CbminhourVAL1 <- 30
                    tclvalue(minhour1.txtVar) <- lang.dlg[['label']][['12']]
                }

                if(as.numeric(str_trim(tclvalue(minhour.in))) == 15){
                    CbminhourVAL1 <- 30
                    tclvalue(minhour1.txtVar) <- lang.dlg[['label']][['12']]
                }

                if(as.numeric(str_trim(tclvalue(minhour.in))) == 30){
                    tclvalue(ConvertData) <- Cbperiod0VAL[2]
                    CbminhourVAL1 <- c(1, 3, 6, 12)
                    tclvalue(minhour1.txtVar) <- lang.dlg[['label']][['11']]
                }
            }

            ## hour
            if(str_trim(tclvalue(ConvertData)) == Cbperiod0VAL[2]){
                CbminhourVAL1 <- c(1, 3, 6, 12)
                tclvalue(minhour1.txtVar) <- lang.dlg[['label']][['11']]
            }
        }

        ## hour
        if(str_trim(tclvalue(OriginData)) == Cbperiod0VAL[2]){
            CbminhourVAL0 <- c(1, 3, 6, 12)
            tclvalue(minhour0.txtVar) <- lang.dlg[['label']][['11']]
            tclvalue(minhour1.txtVar) <- lang.dlg[['label']][['11']]

            if(!as.numeric(str_trim(tclvalue(minhour.in))) %in% CbminhourVAL0)
                tclvalue(minhour.in) <- CbminhourVAL0[1]

            ## hour
            if(str_trim(tclvalue(ConvertData)) == Cbperiod0VAL[2]){
                if(as.numeric(str_trim(tclvalue(minhour.in))) == 1){
                    CbminhourVAL1 <- c(3, 6, 12)
                }

                if(as.numeric(str_trim(tclvalue(minhour.in))) == 3){
                    CbminhourVAL1 <- c(6, 12)
                }

                if(as.numeric(str_trim(tclvalue(minhour.in))) == 6){
                    CbminhourVAL1 <- 12
                }

                if(as.numeric(str_trim(tclvalue(minhour.in))) == 12){
                    tclvalue(ConvertData) <- Cbperiod0VAL[3]
                    state.mhO <- "disabled"
                }
            }
        }

        if(!as.numeric(str_trim(tclvalue(minhour.out))) %in% CbminhourVAL1)
            tclvalue(minhour.out) <- CbminhourVAL1[1]
     }else{
        state.mhI <- "disabled"
        state.mhO <- "disabled"
    }

    cb.mhI <- ttkcombobox(frameMH, values = CbminhourVAL0, textvariable = minhour.in, width = 3, state = state.mhI)
    txt.mhI <- tklabel(frameMH, text = tclvalue(minhour0.txtVar), textvariable = minhour0.txtVar, anchor = 'w', justify = 'left')
    txt.mht <- tklabel(frameMH, text = paste("-", lang.dlg[['label']][['2']], "-"))
    cb.mhO <- ttkcombobox(frameMH, values = CbminhourVAL1, textvariable = minhour.out, width = 3, state = state.mhO)
    txt.mhO <- tklabel(frameMH, text = tclvalue(minhour1.txtVar), textvariable = minhour1.txtVar, anchor = 'w', justify = 'left')

    tkgrid(cb.mhI, row = 0, column = 0, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1)
    tkgrid(txt.mhI, row = 0, column = 1, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1)
    tkgrid(txt.mht, row = 0, column = 2, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1)
    tkgrid(cb.mhO, row = 0, column = 3, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1)
    tkgrid(txt.mhO, row = 0, column = 4, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1)

    ####################

    frameOH <- tkframe(frConvTS, relief = "sunken", borderwidth = 2)

    obs.hour <- tclVar(.cdtData$GalParams$HourMin$obs.hour)

    state.shour <- "disabled"
    if(str_trim(tclvalue(ConvertData)) == Cbperiod0VAL[3]){
        if(str_trim(tclvalue(OriginData)) %in% Cbperiod0VAL[1]) state.shour <- "normal"
        if(str_trim(tclvalue(OriginData)) %in% Cbperiod0VAL[2])
            state.shour <- if(as.numeric(str_trim(tclvalue(minhour.in))) %in% c(6:12)) "disabled" else "normal"
    }

    txt.shour <- tklabel(frameOH, text = lang.dlg[['label']][['13']], anchor = 'e', justify = 'right')
    en.shour <- tkentry(frameOH, textvariable = obs.hour, width = 2, state = state.shour)

    tkgrid(txt.shour, row = 0, column = 0, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1)
    tkgrid(en.shour, row = 0, column = 1, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1)

    helpWidget(en.shour, lang.dlg[['tooltip']][['12']], lang.dlg[['status']][['12']])

    ####################

    frameSeas <- tkframe(frConvTS, relief = "sunken", borderwidth = 2)

    MOIS <- format(ISOdate(2014, 1:12, 1), "%B")
    mois <- format(ISOdate(2014, 1:12, 1), "%b")
    mon <- as.numeric(str_trim(.cdtData$GalParams$Seasonal$start.mon))
    len <- as.numeric(str_trim(.cdtData$GalParams$Seasonal$length.mon))
    mon1 <- (mon + len - 1) %% 12
    mon1[mon1 == 0] <- 12
    seasdef <- paste(mois[mon], "->", mois[mon1])

    start.mon <- tclVar(MOIS[mon])
    length.mon <- tclVar(len)
 
    seasonalVAL <- .cdtEnv$tcl$lang$global[['combobox']][['1']][8:9]

    state.cbSeasS <- if(str_trim(tclvalue(ConvertData)) == seasonalVAL[1]) "normal" else "disabled"
    state.cbSeasL <- if(str_trim(tclvalue(ConvertData)) %in% seasonalVAL[1:2]) "normal" else "disabled"

    seasdef <- if(str_trim(tclvalue(ConvertData)) == seasonalVAL[1]) seasdef else ""
    season.def <- tclVar(seasdef)

    txt.seasS <- tklabel(frameSeas, text = lang.dlg[['label']][['3']], anchor = 'e', justify = 'right')
    cb.seasS <- ttkcombobox(frameSeas, values = MOIS, textvariable = start.mon, width = 10, state = state.cbSeasS)
    txt.seasL <- tklabel(frameSeas, text = lang.dlg[['label']][['4']])
    cb.seasL <- ttkcombobox(frameSeas, values = 2:12, textvariable = length.mon, width = 3, state = state.cbSeasL)
    txt.SeasD <- tklabel(frameSeas, text = tclvalue(season.def), textvariable = season.def)

    tkgrid(txt.seasS, row = 0, column = 0, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1)
    tkgrid(cb.seasS, row = 0, column = 1, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1)
    tkgrid(txt.seasL, row = 0, column = 2, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1)
    tkgrid(cb.seasL, row = 0, column = 3, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1)
    tkgrid(txt.SeasD, row = 0, column = 4, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1)

    helpWidget(cb.seasS, lang.dlg[['tooltip']][['3']], lang.dlg[['status']][['3']])
    helpWidget(cb.seasL, lang.dlg[['tooltip']][['4']], lang.dlg[['status']][['4']])

    #####################

    tkgrid(frameTS, row = 0, column = 0, sticky = 'we', rowspan = 1, columnspan = 2, padx = 1, pady = 2)
    tkgrid(frameMH, row = 1, column = 0, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 4, ipady = 3)
    tkgrid(frameOH, row = 1, column = 1, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 4, ipady = 3)
    tkgrid(frameSeas, row = 2, column = 0, sticky = '', rowspan = 1, columnspan = 2, padx = 1, pady = 2, ipady = 3)

    #####################

    tkbind(cb.intstep, "<<ComboboxSelected>>", function(){
        ## minute
        if(str_trim(tclvalue(OriginData)) == Cbperiod0VAL[1]){
            Cbperiod1VAL <- .cdtEnv$tcl$lang$global[['combobox']][['1']][1:3]
            period1VAL <- c('minute', 'hourly', 'daily')
            tkconfigure(cb.outstep, values = Cbperiod1VAL)
            if(str_trim(tclvalue(ConvertData)) %in% .cdtEnv$tcl$lang$global[['combobox']][['1']][4:9])
                tclvalue(ConvertData) <- Cbperiod1VAL[1]

            state.mhI <- "normal"
            state.mhO <- if(str_trim(tclvalue(ConvertData)) %in% Cbperiod0VAL[1:2]) "normal" else "disabled"
            state.shour <- if(str_trim(tclvalue(ConvertData)) == Cbperiod0VAL[3]) "normal" else "disabled"

            CbminhourVAL0 <- c(5, 10, 15, 30)
            tclvalue(minhour0.txtVar) <- lang.dlg[['label']][['12']]

            if(!as.numeric(str_trim(tclvalue(minhour.in))) %in% CbminhourVAL0)
                tclvalue(minhour.in) <- CbminhourVAL0[1]

            ## min
            if(str_trim(tclvalue(ConvertData)) == Cbperiod0VAL[1]){
                if(as.numeric(str_trim(tclvalue(minhour.in))) == 5){
                    CbminhourVAL1 <- c(10, 15, 30)
                    tclvalue(minhour1.txtVar) <- lang.dlg[['label']][['12']]
                }

                if(as.numeric(str_trim(tclvalue(minhour.in))) == 10){
                    CbminhourVAL1 <- 30
                    tclvalue(minhour1.txtVar) <- lang.dlg[['label']][['12']]
                }

                if(as.numeric(str_trim(tclvalue(minhour.in))) == 15){
                    CbminhourVAL1 <- 30
                    tclvalue(minhour1.txtVar) <- lang.dlg[['label']][['12']]
                }

                if(as.numeric(str_trim(tclvalue(minhour.in))) == 30){
                    tclvalue(ConvertData) <- Cbperiod0VAL[2]
                    CbminhourVAL1 <- c(1, 3, 6, 12)
                    tclvalue(minhour1.txtVar) <- lang.dlg[['label']][['11']]
                }
            }

            ## hour
            if(str_trim(tclvalue(ConvertData)) == Cbperiod0VAL[2]){
                CbminhourVAL1 <- c(1, 3, 6, 12)
                tclvalue(minhour1.txtVar) <- lang.dlg[['label']][['11']]
            }

            if(!as.numeric(str_trim(tclvalue(minhour.out))) %in% CbminhourVAL1)
                tclvalue(minhour.out) <- CbminhourVAL1[1]
        }

        ## hourly
        if(str_trim(tclvalue(OriginData)) == Cbperiod0VAL[2]){
            Cbperiod1VAL <- .cdtEnv$tcl$lang$global[['combobox']][['1']][2:3]
            period1VAL <- c('hourly', 'daily')
            tkconfigure(cb.outstep, values = Cbperiod1VAL)
            if(str_trim(tclvalue(ConvertData)) %in% .cdtEnv$tcl$lang$global[['combobox']][['1']][c(1, 4:9)])
                tclvalue(ConvertData) <- Cbperiod1VAL[1]

            state.mhI <- "normal"
            state.mhO <- if(str_trim(tclvalue(ConvertData)) == Cbperiod0VAL[2]) "normal" else "disabled"
            state.shour <- "disabled"
            if(str_trim(tclvalue(ConvertData)) == Cbperiod0VAL[3]){
                state.shour <- if(as.numeric(str_trim(tclvalue(minhour.in))) %in% c(6:12)) "disabled" else "normal"
            }

            CbminhourVAL0 <- c(1, 3, 6, 12)
            tclvalue(minhour0.txtVar) <- lang.dlg[['label']][['11']]
            tclvalue(minhour1.txtVar) <- lang.dlg[['label']][['11']]

            if(!as.numeric(str_trim(tclvalue(minhour.in))) %in% CbminhourVAL0)
                tclvalue(minhour.in) <- CbminhourVAL0[1]

            if(str_trim(tclvalue(ConvertData)) == Cbperiod0VAL[2]){
                if(as.numeric(str_trim(tclvalue(minhour.in))) == 1){
                    CbminhourVAL1 <- c(3, 6, 12)
                }

                if(as.numeric(str_trim(tclvalue(minhour.in))) == 3){
                    CbminhourVAL1 <- c(6, 12)
                }

                if(as.numeric(str_trim(tclvalue(minhour.in))) == 6){
                    CbminhourVAL1 <- 12
                }

                if(as.numeric(str_trim(tclvalue(minhour.in))) == 12){
                    tclvalue(ConvertData) <- Cbperiod0VAL[3]
                    state.mhO <- "disabled"
                }
            }

            if(!as.numeric(str_trim(tclvalue(minhour.out))) %in% CbminhourVAL1)
                tclvalue(minhour.out) <- CbminhourVAL1[1]
        }

        ## daily
        if(str_trim(tclvalue(OriginData)) == Cbperiod0VAL[3]){
            Cbperiod1VAL <- .cdtEnv$tcl$lang$global[['combobox']][['1']][4:9]
            period1VAL <- c('pentad', 'dekadal', 'monthly', 'annual', 'seasonal', 'roll.seas')
            tkconfigure(cb.outstep, values = Cbperiod1VAL)
            if(str_trim(tclvalue(ConvertData)) %in% Cbperiod0VAL[1:3])
                tclvalue(ConvertData) <- Cbperiod1VAL[1]

            state.mhI <- "disabled"
            state.mhO <- "disabled"
            state.shour <- "disabled"
        }

        ## pentad
        if(str_trim(tclvalue(OriginData)) == Cbperiod0VAL[4]){
            Cbperiod1VAL <- .cdtEnv$tcl$lang$global[['combobox']][['1']][5:9]
            period1VAL <- c('dekadal', 'monthly', 'annual', 'seasonal', 'roll.seas')
            tkconfigure(cb.outstep, values = Cbperiod1VAL)
            if(str_trim(tclvalue(ConvertData)) %in% Cbperiod0VAL[1:4])
                tclvalue(ConvertData) <- Cbperiod1VAL[1]

            state.mhI <- "disabled"
            state.mhO <- "disabled"
            state.shour <- "disabled"
        }

        ## dekadal
        if(str_trim(tclvalue(OriginData)) == Cbperiod0VAL[5]){
            Cbperiod1VAL <- .cdtEnv$tcl$lang$global[['combobox']][['1']][6:9]
            period1VAL <- c('monthly', 'annual', 'seasonal', 'roll.seas')
            tkconfigure(cb.outstep, values = Cbperiod1VAL)
            if(str_trim(tclvalue(ConvertData)) %in% Cbperiod0VAL[1:5])
                tclvalue(ConvertData) <- Cbperiod1VAL[1]

            state.mhI <- "disabled"
            state.mhO <- "disabled"
            state.shour <- "disabled"
        }

        ## monthly
        if(str_trim(tclvalue(OriginData)) == Cbperiod0VAL[6]){
            Cbperiod1VAL <- .cdtEnv$tcl$lang$global[['combobox']][['1']][7:9]
            period1VAL <- c('annual', 'seasonal', 'roll.seas')
            tkconfigure(cb.outstep, values = Cbperiod1VAL)
            if(str_trim(tclvalue(ConvertData)) %in% Cbperiod0VAL[1:6])
                tclvalue(ConvertData) <- Cbperiod1VAL[1]

            state.mhI <- "disabled"
            state.mhO <- "disabled"
            state.shour <- "disabled"
        }

        tkconfigure(cb.mhI, values = CbminhourVAL0, state = state.mhI)
        tkconfigure(cb.mhO, values = CbminhourVAL1, state = state.mhO)
        tkconfigure(en.shour, state = state.shour)

        state.cbSeasS <- if(str_trim(tclvalue(ConvertData)) == seasonalVAL[1]) "normal" else "disabled"
        state.cbSeasL <- if(str_trim(tclvalue(ConvertData)) %in% seasonalVAL[1:2]) "normal" else "disabled"
        tkconfigure(cb.seasS, state = state.cbSeasS)
        tkconfigure(cb.seasL, state = state.cbSeasL)
    })

    ##############

    tkbind(cb.outstep, "<<ComboboxSelected>>", function(){
        if(str_trim(tclvalue(ConvertData)) == Cbperiod0VAL[1]){
            state.mhI <- "normal"
            state.mhO <- "normal"
            state.shour <- "disabled"
            tclvalue(minhour1.txtVar) <- lang.dlg[['label']][['12']]
        }else if(str_trim(tclvalue(ConvertData)) == Cbperiod0VAL[2]){
            state.mhI <- "normal"
            state.mhO <- "normal"
            state.shour <- "disabled"

            tclvalue(minhour1.txtVar) <- lang.dlg[['label']][['11']]
        }else if(str_trim(tclvalue(ConvertData)) == Cbperiod0VAL[3]){
            state.mhI <- "normal"
            state.mhO <- "disabled"
            if(str_trim(tclvalue(OriginData)) == Cbperiod0VAL[1]) state.shour <- "normal"
            if(str_trim(tclvalue(OriginData)) == Cbperiod0VAL[2])
                state.shour <- if(as.numeric(str_trim(tclvalue(minhour.in))) %in% c(6:12)) "disabled" else "normal"
        }else{
            state.mhI <- "disabled"
            state.mhO <- "disabled"
            state.shour <- "disabled"
        }

        #######

        if(str_trim(tclvalue(OriginData)) == Cbperiod0VAL[1]){
            if(str_trim(tclvalue(ConvertData)) == Cbperiod0VAL[1]){
                if(as.numeric(str_trim(tclvalue(minhour.in))) == 5) CbminhourVAL1 <- c(10, 15, 30)
                if(as.numeric(str_trim(tclvalue(minhour.in))) == 10) CbminhourVAL1 <- 30
                if(as.numeric(str_trim(tclvalue(minhour.in))) == 15) CbminhourVAL1 <- 30
                if(as.numeric(str_trim(tclvalue(minhour.in))) == 30){
                    tclvalue(ConvertData) <- Cbperiod0VAL[2]
                    CbminhourVAL1 <- c(1, 3, 6, 12)
                    tclvalue(minhour1.txtVar) <- lang.dlg[['label']][['11']]
                }
            }

            if(str_trim(tclvalue(ConvertData)) == Cbperiod0VAL[2]){
                CbminhourVAL1 <- c(1, 3, 6, 12)
                tclvalue(minhour1.txtVar) <- lang.dlg[['label']][['11']]
            }

            if(!as.numeric(str_trim(tclvalue(minhour.out))) %in% CbminhourVAL1)
                tclvalue(minhour.out) <- CbminhourVAL1[1]
        }

        if(str_trim(tclvalue(OriginData)) == Cbperiod0VAL[2]){
            if(str_trim(tclvalue(ConvertData)) == Cbperiod0VAL[2]){
                if(as.numeric(str_trim(tclvalue(minhour.in))) == 1) CbminhourVAL1 <- c(3, 6, 12)
                if(as.numeric(str_trim(tclvalue(minhour.in))) == 3) CbminhourVAL1 <- c(6, 12)
                if(as.numeric(str_trim(tclvalue(minhour.in))) == 6) CbminhourVAL1 <- 12
                if(as.numeric(str_trim(tclvalue(minhour.in))) == 12){
                    tclvalue(ConvertData) <- Cbperiod0VAL[3]
                    state.mhO <- "disabled"
                }
            }

            if(!as.numeric(str_trim(tclvalue(minhour.out))) %in% CbminhourVAL1)
                tclvalue(minhour.out) <- CbminhourVAL1[1]
        }

        tkconfigure(cb.mhI, state = state.mhI)
        tkconfigure(cb.mhO, values = CbminhourVAL1, state = state.mhO)
        tkconfigure(en.shour, state = state.shour)

        state.cbSeasS <- if(str_trim(tclvalue(ConvertData)) == seasonalVAL[1]) "normal" else "disabled"
        state.cbSeasL <- if(str_trim(tclvalue(ConvertData)) %in% seasonalVAL[1:2]) "normal" else "disabled"
        tkconfigure(cb.seasS, state = state.cbSeasS)
        tkconfigure(cb.seasL, state = state.cbSeasL)

        ########

        seasdef <- ""
        if(str_trim(tclvalue(ConvertData)) == seasonalVAL[1]){
            mon <-  which(MOIS %in% str_trim(tclvalue(start.mon)))
            len <- as.numeric(str_trim(tclvalue(length.mon)))
            mon1 <- (mon + len - 1) %% 12
            mon1[mon1 == 0] <- 12
            seasdef <- paste(mois[mon], "->", mois[mon1])
        }
        tclvalue(season.def) <- seasdef
    })

    ##############

    tkbind(cb.mhI, "<<ComboboxSelected>>", function(){
        if(str_trim(tclvalue(OriginData)) == Cbperiod0VAL[1]){
            ## min
            if(str_trim(tclvalue(ConvertData)) == Cbperiod0VAL[1]){
                if(as.numeric(str_trim(tclvalue(minhour.in))) == 5){
                    CbminhourVAL1 <- c(10, 15, 30)
                    tclvalue(minhour1.txtVar) <- lang.dlg[['label']][['12']]
                }

                if(as.numeric(str_trim(tclvalue(minhour.in))) == 10){
                    CbminhourVAL1 <- 30
                    tclvalue(minhour1.txtVar) <- lang.dlg[['label']][['12']]
                }

                if(as.numeric(str_trim(tclvalue(minhour.in))) == 15){
                    CbminhourVAL1 <- 30
                    tclvalue(minhour1.txtVar) <- lang.dlg[['label']][['12']]
                }

                if(as.numeric(str_trim(tclvalue(minhour.in))) == 30){
                    tclvalue(ConvertData) <- Cbperiod0VAL[2]
                    CbminhourVAL1 <- c(1, 3, 6, 12)
                    tclvalue(minhour1.txtVar) <- lang.dlg[['label']][['11']]
                }

                state.shour <- "disabled"
                state.mhO <- "normal"
            }

            ## hour
            if(str_trim(tclvalue(ConvertData)) == Cbperiod0VAL[2]){
                CbminhourVAL1 <- c(1, 3, 6, 12)
                tclvalue(minhour1.txtVar) <- lang.dlg[['label']][['11']]
                state.shour <- "disabled"
                state.mhO <- "normal"
            }

            if(str_trim(tclvalue(ConvertData)) == Cbperiod0VAL[3]){
                state.shour <- "normal"
                state.mhO <- "disabled"
            }
        }

        if(str_trim(tclvalue(OriginData)) == Cbperiod0VAL[2]){
            if(str_trim(tclvalue(ConvertData)) == Cbperiod0VAL[2]){
                state.mhO <- "normal"

                if(as.numeric(str_trim(tclvalue(minhour.in))) == 1){
                    CbminhourVAL1 <- c(3, 6, 12)
                }

                if(as.numeric(str_trim(tclvalue(minhour.in))) == 3){
                    CbminhourVAL1 <- c(6, 12)
                }

                if(as.numeric(str_trim(tclvalue(minhour.in))) == 6){
                    CbminhourVAL1 <- 12
                }

                if(as.numeric(str_trim(tclvalue(minhour.in))) == 12){
                    tclvalue(ConvertData) <- Cbperiod0VAL[3]
                    state.mhO <- "disabled"
                }

                state.shour <- "disabled"
            }

            if(str_trim(tclvalue(ConvertData)) == Cbperiod0VAL[3]){
                state.shour <- if(as.numeric(str_trim(tclvalue(minhour.in))) %in% c(6:12)) "disabled" else "normal"
                state.mhO <- "disabled"
            }
        }

        tkconfigure(cb.mhO, values = CbminhourVAL1, state = state.mhO)
        tkconfigure(en.shour, state = state.shour)

        if(!as.numeric(str_trim(tclvalue(minhour.out))) %in% CbminhourVAL1)
            tclvalue(minhour.out) <- CbminhourVAL1[1]
    })

    ##############

    tkbind(cb.seasS, "<<ComboboxSelected>>", function(){
        seasdef <- ""
        if(str_trim(tclvalue(ConvertData)) == seasonalVAL[1]){
            mon <-  which(MOIS %in% str_trim(tclvalue(start.mon)))
            len <- as.numeric(str_trim(tclvalue(length.mon)))
            mon1 <- (mon + len - 1) %% 12
            mon1[mon1 == 0] <- 12
            seasdef <- paste(mois[mon], "->", mois[mon1])
        }
        tclvalue(season.def) <- seasdef
    })

    ##############

    tkbind(cb.seasL, "<<ComboboxSelected>>", function(){
        seasdef <- ""
        if(str_trim(tclvalue(ConvertData)) == seasonalVAL[1]){
            mon <-  which(MOIS %in% str_trim(tclvalue(start.mon)))
            len <- as.numeric(str_trim(tclvalue(length.mon)))
            mon1 <- (mon + len - 1) %% 12
            mon1[mon1 == 0] <- 12
            seasdef <- paste(mois[mon], "->", mois[mon1])
        }
        tclvalue(season.def) <- seasdef
    })

    ###########################################

    frDataType <- ttklabelframe(frAGGRTS, text = lang.dlg[['label']][['5']], labelanchor = "nw", relief = "groove", borderwidth = 2)

    DataType <- tclVar()
    CbdatatypeVAL <- .cdtEnv$tcl$lang$global[['combobox']][['2']][1:3]
    datatypeVAL <- c('cdtstation', 'cdtdataset', 'cdtnetcdf')
    tclvalue(DataType) <- CbdatatypeVAL[datatypeVAL %in% .cdtData$GalParams$data.type]

    if(.cdtData$GalParams$data.type == 'cdtstation'){
        file.stnfl <- tclVar(.cdtData$GalParams$cdtstation)
        txtFileDir <- lang.dlg[['label']][['6']]
        stateSetData <- "disabled"
    }else if(.cdtData$GalParams$data.type == 'cdtdataset'){
        file.stnfl <- tclVar(.cdtData$GalParams$cdtdataset)
        txtFileDir <- lang.dlg[['label']][['7']]
        stateSetData <- "disabled"
    }else{
        file.stnfl <- tclVar(.cdtData$GalParams$cdtnetcdf$dir)
        txtFileDir <- lang.dlg[['label']][['8']]
        stateSetData <- "normal"
    }
    fileINdir <- tclVar(txtFileDir)

    ##############
    cb.datatype <- ttkcombobox(frDataType, values = CbdatatypeVAL, textvariable = DataType, width = largeur2)
    set.datatype <- ttkbutton(frDataType, text = .cdtEnv$tcl$lang$global[['button']][['5']], state = stateSetData)

    txt.stnfl <- tklabel(frDataType, text = tclvalue(fileINdir), textvariable = fileINdir, anchor = 'w', justify = 'left')
    if(.cdtData$GalParams$data.type == 'cdtstation'){
        cb.stnfl <- ttkcombobox(frDataType, values = unlist(listOpenFiles), textvariable = file.stnfl, width = largeur1)
    }else{
        cb.stnfl <- tkentry(frDataType, textvariable = file.stnfl, width = largeur0)
    }
    bt.stnfl <- tkbutton(frDataType, text = "...")

    ##############

    tkgrid(cb.datatype, row = 0, column = 0, sticky = 'we', rowspan = 1, columnspan = 3, padx = 1, pady = 1, ipadx = 1, ipady = 1)
    tkgrid(set.datatype, row = 0, column = 3, sticky = 'we', rowspan = 1, columnspan = 2, padx = 1, pady = 1, ipadx = 1, ipady = 1)
    tkgrid(txt.stnfl, row = 1, column = 0, sticky = 'we', rowspan = 1, columnspan = 5, padx = 1, pady = 1, ipadx = 1, ipady = 1)
    tkgrid(cb.stnfl, row = 2, column = 0, sticky = 'we', rowspan = 1, columnspan = 4, padx = 0, pady = 1, ipadx = 1, ipady = 1)
    tkgrid(bt.stnfl, row = 2, column = 4, sticky = 'we', rowspan = 1, columnspan = 1, padx = 0, pady = 1, ipadx = 1, ipady = 1)

    if(.cdtData$GalParams$data.type == 'cdtstation'){
        helpWidget(cb.stnfl, lang.dlg[['tooltip']][['5']], lang.dlg[['status']][['5']])
        helpWidget(bt.stnfl, lang.dlg[['tooltip']][['8']], lang.dlg[['status']][['8']])
    }else if(.cdtData$GalParams$data.type == 'cdtdataset'){
        helpWidget(cb.stnfl, lang.dlg[['tooltip']][['6']], lang.dlg[['status']][['6']])
        helpWidget(bt.stnfl, lang.dlg[['tooltip']][['9']], lang.dlg[['status']][['9']])
    }else{
        helpWidget(cb.stnfl, lang.dlg[['tooltip']][['7']], lang.dlg[['status']][['7']])
        helpWidget(bt.stnfl, lang.dlg[['tooltip']][['9']], lang.dlg[['status']][['9']])
    }

    ##############

    settingdone <- .cdtData$GalParams$settingdone

    tkconfigure(set.datatype, command = function(){
        tcl('wm', 'attributes', tt, topmost = FALSE)
        AggregateTS_ncdfData(tt, str_trim(tclvalue(file.stnfl)), tclvalue(OriginData))
        tcl('wm', 'attributes', tt, topmost = TRUE)
        settingdone <<- 1
    })

    tkconfigure(bt.stnfl, command = function(){
        if(.cdtData$GalParams$data.type == 'cdtstation'){
            tcl('wm', 'attributes', tt, topmost = FALSE)
            dat.opfiles <- getOpenFiles(tt)
            tcl('wm', 'attributes', tt, topmost = TRUE)
            if(!is.null(dat.opfiles)){
                update.OpenFiles('ascii', dat.opfiles)
                listOpenFiles[[length(listOpenFiles) + 1]] <<- dat.opfiles[[1]]
                tclvalue(file.stnfl) <- dat.opfiles[[1]]
                tkconfigure(cb.stnfl, values = unlist(listOpenFiles))
            }
        }else if(.cdtData$GalParams$data.type == 'cdtdataset'){
            tcl('wm', 'attributes', tt, topmost = FALSE)
            path.rds <- tclvalue(tkgetOpenFile(filetypes = .cdtEnv$tcl$data$filetypes6))
            tcl('wm', 'attributes', tt, topmost = TRUE)
            tclvalue(file.stnfl) <- if(path.rds %in% c("", "NA") | is.na(path.rds)) "" else path.rds
        }else{
            tcl('wm', 'attributes', tt, topmost = FALSE)
            dirnc <- tk_choose.dir(getwd(), "")
            tcl('wm', 'attributes', tt, topmost = TRUE)
            tclvalue(file.stnfl) <- if(dirnc %in% c("", "NA") | is.na(dirnc)) "" else dirnc
        }
    })

    ##############

    tkbind(cb.datatype, "<<ComboboxSelected>>", function(){
        tkdestroy(cb.stnfl)
        tclvalue(file.stnfl) <- ''

        ####
        if(str_trim(tclvalue(DataType)) == CbdatatypeVAL[1]){
            tclvalue(fileINdir) <- lang.dlg[['label']][['6']]
            tclvalue(fileORdir) <- lang.dlg[['label']][['9']]

            cb.stnfl <- ttkcombobox(frDataType, values = unlist(listOpenFiles), textvariable = file.stnfl, width = largeur1)

            #######
            tkconfigure(bt.stnfl, command = function(){
                tcl('wm', 'attributes', tt, topmost = FALSE)
                dat.opfiles <- getOpenFiles(tt)
                tcl('wm', 'attributes', tt, topmost = TRUE)
                if(!is.null(dat.opfiles)){
                    update.OpenFiles('ascii', dat.opfiles)
                    listOpenFiles[[length(listOpenFiles) + 1]] <<- dat.opfiles[[1]]
                    tclvalue(file.stnfl) <- dat.opfiles[[1]]
                    tkconfigure(cb.stnfl, values = unlist(listOpenFiles))
                }
            })
            tkconfigure(bt.file.save, command = function(){
                tcl('wm', 'attributes', tt, topmost = FALSE)
                fileORdir2Save(file.save, isFile = TRUE)
                tcl('wm', 'attributes', tt, topmost = TRUE)
            })
            tkconfigure(set.datatype, state = 'disabled')

            #######
            helpWidget(cb.stnfl, lang.dlg[['tooltip']][['5']], lang.dlg[['status']][['5']])
            helpWidget(bt.stnfl, lang.dlg[['tooltip']][['8']], lang.dlg[['status']][['8']])
            helpWidget(en.file.save, lang.dlg[['tooltip']][['10']], lang.dlg[['status']][['10']])
        }

        ####
        if(str_trim(tclvalue(DataType)) == CbdatatypeVAL[2]){
            tclvalue(fileINdir) <- lang.dlg[['label']][['7']]
            tclvalue(fileORdir) <- lang.dlg[['label']][['10']]

            cb.stnfl <- tkentry(frDataType, textvariable = file.stnfl, width = largeur0)

            #######
            tkconfigure(bt.stnfl, command = function(){
                tcl('wm', 'attributes', tt, topmost = FALSE)
                path.rds <- tclvalue(tkgetOpenFile(filetypes = .cdtEnv$tcl$data$filetypes6))
                tcl('wm', 'attributes', tt, topmost = TRUE)
                tclvalue(file.stnfl) <- if(path.rds %in% c("", "NA") | is.na(path.rds)) "" else path.rds
            })

            tkconfigure(bt.file.save, command = function(){
                tcl('wm', 'attributes', tt, topmost = FALSE)
                fileORdir2Save(file.save, isFile = FALSE)
                tcl('wm', 'attributes', tt, topmost = TRUE)
            })
            tkconfigure(set.datatype, state = 'disabled')

            #######
            helpWidget(cb.stnfl, lang.dlg[['tooltip']][['6']], lang.dlg[['status']][['6']])
            helpWidget(bt.stnfl, lang.dlg[['tooltip']][['9']], lang.dlg[['status']][['9']])
            helpWidget(en.file.save, lang.dlg[['tooltip']][['11']], lang.dlg[['status']][['11']])
        }

        ####
        if(str_trim(tclvalue(DataType)) == CbdatatypeVAL[3]){
            tclvalue(fileINdir) <- lang.dlg[['label']][['8']]
            tclvalue(fileORdir) <- lang.dlg[['label']][['10']]

            cb.stnfl <- tkentry(frDataType, textvariable = file.stnfl, width = largeur0)

            #######
            tkconfigure(bt.stnfl, command = function(){
                tcl('wm', 'attributes', tt, topmost = FALSE)
                dirnc <- tk_choose.dir(getwd(), "")
                tcl('wm', 'attributes', tt, topmost = TRUE)
                tclvalue(file.stnfl) <- if(dirnc %in% c("", "NA") | is.na(dirnc)) "" else dirnc
            })

            tkconfigure(set.datatype, command = function(){
                tcl('wm', 'attributes', tt, topmost = FALSE)
                AggregateTS_ncdfData(tt, str_trim(tclvalue(file.stnfl)), tclvalue(OriginData))
                tcl('wm', 'attributes', tt, topmost = TRUE)
                settingdone <<- 1
            })

            tkconfigure(bt.file.save, command = function(){
                tcl('wm', 'attributes', tt, topmost = FALSE)
                fileORdir2Save(file.save, isFile = FALSE)
                tcl('wm', 'attributes', tt, topmost = TRUE)
            })
            tkconfigure(set.datatype, state = 'normal')

            #######
            helpWidget(cb.stnfl, lang.dlg[['tooltip']][['7']], lang.dlg[['status']][['7']])
            helpWidget(bt.stnfl, lang.dlg[['tooltip']][['9']], lang.dlg[['status']][['9']])
            helpWidget(en.file.save, lang.dlg[['tooltip']][['11']], lang.dlg[['status']][['11']])
        }

        #######
        tkgrid(cb.stnfl, row = 2, column = 0, sticky = 'we', rowspan = 1, columnspan = 4, padx = 0, pady = 1, ipadx = 1, ipady = 1)
        tkfocus(tt)
    })

    ############################################

    bt.aggrPars <- ttkbutton(frAGGRTS, text = lang.dlg[['button']][['1']])

    tkconfigure(bt.aggrPars, command = function(){
        tcl('wm', 'attributes', tt, topmost = FALSE)
        .cdtData$GalParams[['aggr.series']] <- getInfo_AggregateFun(tt, .cdtData$GalParams[['aggr.series']])
        tcl('wm', 'attributes', tt, topmost = TRUE)
    })

    ############################################

    frSave <- tkframe(frAGGRTS, relief = 'groove', borderwidth = 2)

    file.save <- tclVar(.cdtData$GalParams$output)

    if(.cdtData$GalParams$data.type == 'cdtstation'){
        txtSaveDir <- lang.dlg[['label']][['9']]
        isFile <- TRUE
    }else{
        txtSaveDir <- lang.dlg[['label']][['10']]
        isFile <- FALSE
    }
    fileORdir <- tclVar(txtSaveDir)

    txt.file.save <- tklabel(frSave, text = tclvalue(fileORdir), textvariable = fileORdir, anchor = 'w', justify = 'left')
    en.file.save <- tkentry(frSave, textvariable = file.save, width = largeur0)
    bt.file.save <- tkbutton(frSave, text = "...")

    #########

    tkgrid(txt.file.save, row = 0, column = 0, sticky = 'we', rowspan = 1, columnspan = 5, padx = 1, pady = 0, ipadx = 1, ipady = 1)
    tkgrid(en.file.save, row = 1, column = 0, sticky = 'we', rowspan = 1, columnspan = 4, padx = 0, pady = 0, ipadx = 1, ipady = 1)
    tkgrid(bt.file.save, row = 1, column = 4, sticky = 'we', rowspan = 1, columnspan = 1, padx = 0, pady = 0, ipadx = 1, ipady = 1)

    ihlpSav <- if(.cdtData$GalParams$data.type == 'cdtstation') '10' else '11'
    helpWidget(en.file.save, lang.dlg[['tooltip']][[ihlpSav]], lang.dlg[['status']][[ihlpSav]])
    helpWidget(bt.file.save, lang.dlg[['tooltip']][['9']], lang.dlg[['status']][['9']])

    #########

    tkconfigure(bt.file.save, command = function(){
        tcl('wm', 'attributes', tt, topmost = FALSE)
        fileORdir2Save(file.save, isFile = isFile)
        tcl('wm', 'attributes', tt, topmost = TRUE)
    })

    ############################################
    tkgrid(frConvTS, row = 0, column = 0, sticky = 'we', padx = 1, pady = 1, ipadx = 1, ipady = 1)
    tkgrid(frDataType, row = 1, column = 0, sticky = 'we', padx = 1, pady = 1, ipadx = 1, ipady = 1)
    tkgrid(bt.aggrPars, row = 3, column = 0, sticky = 'we', padx = 1, pady = 3, ipadx = 1, ipady = 1)
    tkgrid(frSave, row = 4, column = 0, sticky = 'we', padx = 1, pady = 1, ipadx = 1, ipady = 1)

    ############################################

    tkgrid(frAGGRTS, row = 0, column = 0, sticky = 'news', padx = 5, pady = 1, ipadx = 1, ipady = 1)

    ############################################

    bt.prm.OK <- ttkbutton(frMRG1, text = .cdtEnv$tcl$lang$global[['button']][['1']])
    bt.prm.CA <- ttkbutton(frMRG1, text = .cdtEnv$tcl$lang$global[['button']][['2']])

    ####
    tkconfigure(bt.prm.OK, command = function(){
        if(str_trim(tclvalue(file.stnfl)) == ""){
            cdt.tkmessageBox(tt, message = lang.dlg[['message']][['1']], icon = "warning", type = "ok")
            tkwait.window(tt)
        }else if(str_trim(tclvalue(file.save)) %in% c("", "NA")){
            cdt.tkmessageBox(tt, message = lang.dlg[['message']][['2']], icon = "warning", type = "ok")
            tkwait.window(tt)
        }else if(str_trim(tclvalue(DataType)) == CbdatatypeVAL[3] & is.null(settingdone)){
            cdt.tkmessageBox(tt, message = lang.dlg[['message']][['3']], icon = "warning", type = "ok")
            tkwait.window(tt)
        }else{
            .cdtData$GalParams$in.tstep <- period0VAL[Cbperiod0VAL %in% str_trim(tclvalue(OriginData))]
            .cdtData$GalParams$out.tstep <- periodVAL[CbperiodVAL %in% str_trim(tclvalue(ConvertData))]

            .cdtData$GalParams$HourMin$int <- as.numeric(str_trim(tclvalue(minhour.in)))
            .cdtData$GalParams$HourMin$out <- as.numeric(str_trim(tclvalue(minhour.out)))
            .cdtData$GalParams$HourMin$obs.hour <- as.numeric(str_trim(tclvalue(obs.hour)))

            .cdtData$GalParams$Seasonal$start.mon <- which(MOIS %in% str_trim(tclvalue(start.mon)))
            .cdtData$GalParams$Seasonal$length.mon <- as.numeric(str_trim(tclvalue(length.mon)))

            .cdtData$GalParams$data.type <- datatypeVAL[CbdatatypeVAL %in% str_trim(tclvalue(DataType))]

            if(str_trim(tclvalue(DataType)) == CbdatatypeVAL[1])
                .cdtData$GalParams$cdtstation <- str_trim(tclvalue(file.stnfl))
            if(str_trim(tclvalue(DataType)) == CbdatatypeVAL[2])
                .cdtData$GalParams$cdtdataset <- str_trim(tclvalue(file.stnfl))
            if(str_trim(tclvalue(DataType)) == CbdatatypeVAL[3])
                .cdtData$GalParams$cdtnetcdf$dir <- str_trim(tclvalue(file.stnfl))

            .cdtData$GalParams$output <- str_trim(tclvalue(file.save))

            .cdtData$GalParams$settingdone <- settingdone
            .cdtData$GalParams$message <- lang.dlg[['message']]

            tkgrab.release(tt)
            tkdestroy(tt)
            tkfocus(.cdtEnv$tcl$main$win)
        }
    })

    tkconfigure(bt.prm.CA, command = function(){
        tkgrab.release(tt)
        tkdestroy(tt)
        tkfocus(.cdtEnv$tcl$main$win)
    })

    ####
    tkgrid(bt.prm.OK, row = 0, column = 0, sticky = 'w', padx = 5, pady = 1, ipadx = 1, ipady = 1)
    tkgrid(bt.prm.CA, row = 0, column = 1, sticky = 'e', padx = 5, pady = 1, ipadx = 1, ipady = 1)

    ############################################

    tkgrid(frMRG0, row = 0, column = 0, sticky = 'nswe', rowspan = 1, columnspan = 2, padx = 1, pady = 1, ipadx = 1, ipady = 1)
    tkgrid(frMRG1, row = 1, column = 1, sticky = 'se', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)

    ###########################
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
    tkbind(tt, "<Destroy>", function() {
        tkgrab.release(tt)
        tkfocus(.cdtEnv$tcl$main$win)
    })
    tkwait.window(tt)
}
