
biasCoeffGetInfoWind <- function(){
    if(WindowsOS()){
        largeur0 <- 22
        largeur1 <- 43
        largeur2 <- 45
        largeur3 <- 35
        largeur4 <- 20
    }else{
        largeur0 <- 22
        largeur1 <- 41
        largeur2 <- 42
        largeur3 <- 35
        largeur4 <- 20
    }

    ####################################

    xml.dlg <- file.path(.cdtDir$dirLocal, "languages", "cdtBias_Coefficient_Wind_dlgBox.xml")
    lang.dlg <- cdtLanguageParse(xml.dlg, .cdtData$Config$lang.iso)

    ####################################

    tt <- tktoplevel()
    tkgrab.set(tt)
    tkfocus(tt)

    frMRG0 <- tkframe(tt, relief = 'raised', borderwidth = 2, padx = 3, pady = 3)
    frMRG1 <- tkframe(tt)

    ####################################

    region.box <- function(frBox, h){
        txt.boxrg <- tklabel(frBox, text = paste(lang.dlg[['label']][['1']], ":"), width = largeur4, anchor = 'e', justify = 'right')
        txt.boxlo <- tklabel(frBox, text = lang.dlg[['label']][['2']])
        en.boxlo <- tkentry(frBox, textvariable = box.lon, width = 4)
        txt.boxla <- tklabel(frBox, text = lang.dlg[['label']][['3']])
        en.boxla <- tkentry(frBox, textvariable = box.lat, width = 4)

        tkgrid(txt.boxrg, row = 0, column = 0, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
        tkgrid(txt.boxlo, row = 0, column = 1, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
        tkgrid(en.boxlo, row = 0, column = 2, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
        tkgrid(txt.boxla, row = 0, column = 3, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
        tkgrid(en.boxla, row = 0, column = 4, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)

        helpWidget(en.boxlo, lang.dlg[['tooltip']][[h]], lang.dlg[['status']][[h]])
        helpWidget(en.boxla, lang.dlg[['tooltip']][[h]], lang.dlg[['status']][[h]])
    }

    ncdata.chunks <- function(frBox){
        stateC <- if(tclvalue(chunks.exist) == "1") 'normal' else 'disabled'

        chk.ncdata <- tkcheckbutton(frBox, variable = chunks.exist, text = lang.dlg[['checkbutton']][['3']], anchor = 'w', justify = 'left')
        txt.ncdata <- tklabel(frBox, text = lang.dlg[['label']][['10']], anchor = 'e', justify = 'right')
        en.ncdata <- tkentry(frBox, textvariable = chunks.dir, width = largeur2, state = stateC)
        bt.ncdata <- tkbutton(frBox, text = "...", state = stateC)

        tkgrid(chk.ncdata, row = 0, column = 0, sticky = 'w', rowspan = 1, columnspan = 2, padx = 1, pady = 2, ipadx = 1, ipady = 1)
        tkgrid(txt.ncdata, row = 1, column = 0, sticky = 'we', rowspan = 1, columnspan = 2, padx = 1, pady = 2, ipadx = 1, ipady = 1)
        tkgrid(en.ncdata, row = 2, column = 0, sticky = 'we', rowspan = 1, columnspan = 1, pady = 1, ipady = 1)
        tkgrid(bt.ncdata, row = 2, column = 1, sticky = 'we', rowspan = 1, columnspan = 1, pady = 1, ipady = 1)

        helpWidget(chk.ncdata, lang.dlg[['tooltip']][['17']], lang.dlg[['status']][['17']])
        helpWidget(en.ncdata, lang.dlg[['tooltip']][['18']], lang.dlg[['status']][['18']])
        helpWidget(bt.ncdata, lang.dlg[['tooltip']][['19']], lang.dlg[['status']][['19']])

        tkconfigure(bt.ncdata, command = function(){
            tcl('wm', 'attributes', tt, topmost = FALSE)
            dirnc <- tk_choose.dir(getwd(), "")
            tcl('wm', 'attributes', tt, topmost = TRUE)
            tclvalue(chunks.dir) <- if(!is.na(dirnc)) dirnc else ""
        })

        tkbind(chk.ncdata, "<Button-1>", function(){
            stateC <- if(tclvalue(chunks.exist) == "0") 'normal' else 'disabled'
            tkconfigure(en.ncdata, state = stateC)
            tkconfigure(bt.ncdata, state = stateC)

            stateNC <- if(tclvalue(chunks.exist) == "1") 'normal' else 'disabled'
            ncdata.chunks_bind(stateNC)
        })
    }


    ncdata.chunks_bind <- function(stateNC){
        wvar <- valWindVar[cbWindVar %in% trimws(tclvalue(windVar))]
        widgets_class <- c('Button', 'TButton', 'Entry')

        fr_child <- tkwinfo("children", fr.windVar)
        fr_child <- as.character(fr_child)

        if(wvar == "speed"){
            fr_child <- fr_child[-(1:3)]
            for(id in fr_child){
                id_class <- as.character(tkwinfo('class', id))
                if(id_class %in% widgets_class) tkconfigure(id, state = stateNC)
            }
        }else{
            for(id in fr_child){
                id_class <- as.character(tkwinfo('class', id))
                if(id_class != "Frame") next
                fr_uv <- tkwinfo("children", id)
                fr_uv <- as.character(fr_uv)
                for(el in fr_uv){
                    el_class <- as.character(tkwinfo('class', el))
                    if(el_class %in% widgets_class) tkconfigure(el, state = stateNC)
                }
            }
        }
    }

    bias_method_inputs <- function(biasmthd){
        tkdestroy(fr.biasOpts)
        fr.biasOpts <<- tkframe(frameBias)

        if(biasmthd == "qmecdf"){
            region.box(fr.biasOpts, '1')
        }

        if(biasmthd == "qmdist"){
            txt.distr <- tklabel(fr.biasOpts, text = lang.dlg[['label']][['11']], anchor = 'e', justify = 'right')
            cb.distr <- ttkcombobox(fr.biasOpts, values = cb.distrName, textvariable = distr.name, width = largeur0, justify = 'center')
            txt.supp <- tklabel(fr.biasOpts, text = lang.dlg[['label']][['12']], anchor = 'e', justify = 'right')
            cb.supp <- ttkcombobox(fr.biasOpts, values = cb.tsSupport, textvariable = ts.support, width = largeur0, justify = 'center')
            fr.box <- tkframe(fr.biasOpts, relief = 'groove', borderwidth = 2)

            tsSupp <- val.tsSupport[cb.tsSupport %in% trimws(tclvalue(ts.support))]
            if(tsSupp == "rectbox") region.box(fr.box, '16') else ncdata.chunks(fr.box)

            tkgrid(txt.distr, row = 0, column = 0, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 2, ipadx = 1, ipady = 1)
            tkgrid(cb.distr, row = 0, column = 1, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 2, ipadx = 1, ipady = 1)
            tkgrid(txt.supp, row = 1, column = 0, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 2, ipadx = 1, ipady = 1)
            tkgrid(cb.supp, row = 1, column = 1, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 2, ipadx = 1, ipady = 1)
            tkgrid(fr.box, row = 2, column = 0, sticky = 'we', rowspan = 1, columnspan = 2, padx = 1, pady = 1, ipadx = 1, ipady = 1)

            helpWidget(cb.distr, lang.dlg[['tooltip']][['2']], lang.dlg[['status']][['2']])
            helpWidget(cb.supp, lang.dlg[['tooltip']][['15']], lang.dlg[['status']][['15']])

            tkbind(cb.supp, "<<ComboboxSelected>>", function(){
                tsSupp <- val.tsSupport[cb.tsSupport %in% trimws(tclvalue(ts.support))]
                tkdestroy(fr.box)

                fr.box <<- tkframe(fr.biasOpts, relief = 'groove', borderwidth = 2)
                if(tsSupp == "rectbox") region.box(fr.box, '16') else ncdata.chunks(fr.box)
                tkgrid(fr.box, row = 2, column = 0, sticky = 'we', rowspan = 1, columnspan = 2, padx = 1, pady = 1, ipadx = 1, ipady = 1)

                if(tsSupp == "points"){
                    stateNC <- if(tclvalue(chunks.exist) == "0") 'normal' else 'disabled'
                }else stateNC <- 'normal'
                ncdata.chunks_bind(stateNC)

                stateInterp <- if(tsSupp == 'rectbox') "disabled" else "normal"
                tkconfigure(bt.bias.interp, state = stateInterp)
            })
        }

        if(biasmthd %in% c("qmecdf", "qmdist"))
            tkgrid(fr.biasOpts, row = 2, column = 0, sticky = 'we', rowspan = 1, columnspan = 2, padx = 1, pady = 1, ipadx = 1, ipady = 1)
    }

    ####################################

    getwindSpeed <- function(){
        tkdestroy(fr.windVar)
        fr.windVar <<- tkframe(frInputData)

        bsmethod <- val.biasMthd[cb.biasMthd %in% trimws(tclvalue(bias.method))]
        tsSupp <- val.tsSupport[cb.tsSupport %in% trimws(tclvalue(ts.support))]
        chunksexist <- if(tclvalue(chunks.exist) == '1') TRUE else FALSE

        stateNC <- if(bsmethod == 'qmdist' && tsSupp == 'points' && chunksexist) 'disabled' else 'normal'

        txt.stnS <- tklabel(fr.windVar, text = lang.dlg[['label']][['4-1']], anchor = 'w', justify = 'left')
        cb.stnS <- ttkcombobox(fr.windVar, values = unlist(openFile_ttkcomboList()), textvariable = file.STN_S, width = largeur1)
        addTo_all_Combobox_List(cb.stnS)
        bt.stnS <- tkbutton(fr.windVar, text = "...")
        txt.ncS <- tklabel(fr.windVar, text = lang.dlg[['label']][['5-1']], anchor = 'w', justify = 'left')
        set.ncS <- ttkbutton(fr.windVar, text = .cdtEnv$tcl$lang$global[['button']][['5']], state = stateNC)
        en.ncS <- tkentry(fr.windVar, textvariable = dir.NC_S, width = largeur2, state = stateNC)
        bt.ncS <- tkbutton(fr.windVar, text = "...", state = stateNC)

        tkgrid(txt.stnS, row = 0, column = 0, sticky = 'we', rowspan = 1, columnspan = 5, padx = 1, pady = 1, ipadx = 1, ipady = 1)
        tkgrid(cb.stnS, row = 1, column = 0, sticky = 'we', rowspan = 1, columnspan = 4, padx = 0, pady = 1, ipadx = 1, ipady = 1)
        tkgrid(bt.stnS, row = 1, column = 4, sticky = 'we', rowspan = 1, columnspan = 1, padx = 0, pady = 1, ipadx = 1, ipady = 1)

        tkgrid(txt.ncS, row = 2, column = 0, sticky = 'we', rowspan = 1, columnspan = 3, padx = 1, pady = 1, ipadx = 1, ipady = 1)
        tkgrid(set.ncS, row = 2, column = 3, sticky = 'we', rowspan = 1, columnspan = 2, padx = 1, pady = 1, ipadx = 1, ipady = 1)
        tkgrid(en.ncS, row = 3, column = 0, sticky = 'we', rowspan = 1, columnspan = 4, padx = 0, pady = 1, ipadx = 1, ipady = 1)
        tkgrid(bt.ncS, row = 3, column = 4, sticky = 'we', rowspan = 1, columnspan = 1, padx = 0, pady = 1, ipadx = 1, ipady = 1)

        helpWidget(cb.stnS, lang.dlg[['tooltip']][['5-1']], lang.dlg[['status']][['5-1']])
        helpWidget(bt.stnS, lang.dlg[['tooltip']][['6']], lang.dlg[['status']][['6']])
        helpWidget(en.ncS, lang.dlg[['tooltip']][['7-1']], lang.dlg[['status']][['7-1']])
        helpWidget(bt.ncS, lang.dlg[['tooltip']][['8']], lang.dlg[['status']][['8']])
        helpWidget(set.ncS, lang.dlg[['tooltip']][['9']], lang.dlg[['status']][['9']])

        ######

        tkconfigure(bt.stnS, command = function(){
            tcl('wm', 'attributes', tt, topmost = FALSE)
            dat.opfiles <- getOpenFiles(tt)
            tcl('wm', 'attributes', tt, topmost = TRUE)
            if(!is.null(dat.opfiles)){
                update.OpenFiles('ascii', dat.opfiles)
                tclvalue(file.STN_S) <- dat.opfiles[[1]]
            }
        })

        tkconfigure(set.ncS, command = function(){
            tcl('wm', 'attributes', tt, topmost = FALSE)
            .cdtData$GalParams[["INPUT.S"]] <- getInfoNetCDFData(tt, .cdtData$GalParams[["INPUT.S"]],
                                                                 trimws(tclvalue(dir.NC_S)))
            tcl('wm', 'attributes', tt, topmost = TRUE)
        })

        tkconfigure(bt.ncS, command = function(){
            tcl('wm', 'attributes', tt, topmost = FALSE)
            dirnc <- tk_choose.dir(getwd(), "")
            tcl('wm', 'attributes', tt, topmost = TRUE)
            tclvalue(dir.NC_S) <- if(!is.na(dirnc)) dirnc else ""
        })

        tkgrid(fr.windVar, row = 1, column = 0, sticky = 'we', rowspan = 1, columnspan = 2)
    }

    getwindData <- function(){
        bsmethod <- val.biasMthd[cb.biasMthd %in% trimws(tclvalue(bias.method))]
        tsSupp <- val.tsSupport[cb.tsSupport %in% trimws(tclvalue(ts.support))]
        chunksexist <- if(tclvalue(chunks.exist) == '1') TRUE else FALSE

        stateNC <- if(bsmethod == 'qmdist' && tsSupp == 'points' && chunksexist) 'disabled' else 'normal'

        getwindUVone <- function(){
            tkdestroy(fr.windUV)
            fr.windUV <<- tkframe(fr.windVar)

            txt.ncUV <- tklabel(fr.windUV, text = lang.dlg[['label']][['5-4']], anchor = 'w', justify = 'left')
            set.ncUV <- ttkbutton(fr.windUV, text = .cdtEnv$tcl$lang$global[['button']][['5']], state = stateNC)
            en.ncUV <- tkentry(fr.windUV, textvariable = dir.NC_UV, width = largeur2, state = stateNC)
            bt.ncUV <- tkbutton(fr.windUV, text = "...", state = stateNC)

            tkgrid(txt.ncUV, row = 0, column = 0, sticky = 'we', rowspan = 1, columnspan = 3, padx = 1, pady = 1, ipadx = 1, ipady = 1)
            tkgrid(set.ncUV, row = 0, column = 3, sticky = 'we', rowspan = 1, columnspan = 2, padx = 1, pady = 1, ipadx = 1, ipady = 1)
            tkgrid(en.ncUV, row = 1, column = 0, sticky = 'we', rowspan = 1, columnspan = 4, padx = 0, pady = 1, ipadx = 1, ipady = 1)
            tkgrid(bt.ncUV, row = 1, column = 4, sticky = 'we', rowspan = 1, columnspan = 1, padx = 0, pady = 1, ipadx = 1, ipady = 1)

            helpWidget(en.ncUV, lang.dlg[['tooltip']][['7-4']], lang.dlg[['status']][['7-4']])
            helpWidget(bt.ncUV, lang.dlg[['tooltip']][['8']], lang.dlg[['status']][['8']])
            helpWidget(set.ncUV, lang.dlg[['tooltip']][['9']], lang.dlg[['status']][['9']])

            #####

            tkconfigure(set.ncUV, command = function(){
                tcl('wm', 'attributes', tt, topmost = FALSE)
                .cdtData$GalParams[["INPUT.UV"]] <- getInfoNetCDFDataWind(tt, .cdtData$GalParams[["INPUT.UV"]],
                                                                          trimws(tclvalue(dir.NC_UV)))
                tcl('wm', 'attributes', tt, topmost = TRUE)
            })

            tkconfigure(bt.ncUV, command = function(){
                tcl('wm', 'attributes', tt, topmost = FALSE)
                dirnc <- tk_choose.dir(getwd(), "")
                tcl('wm', 'attributes', tt, topmost = TRUE)
                tclvalue(dir.NC_UV) <- if(!is.na(dirnc)) dirnc else ""
            })

            tkgrid(fr.windUV, row = 5, column = 0, sticky = 'we', rowspan = 1, columnspan = 5)
            tcl('update')
        }

        getwindUVsep <- function(){
            tkdestroy(fr.windUV)
            fr.windUV <<- tkframe(fr.windVar)

            txt.ncU <- tklabel(fr.windUV, text = lang.dlg[['label']][['5-2']], anchor = 'w', justify = 'left')
            set.ncU <- ttkbutton(fr.windUV, text = .cdtEnv$tcl$lang$global[['button']][['5']], state = stateNC)
            en.ncU <- tkentry(fr.windUV, textvariable = dir.NC_U, width = largeur2, state = stateNC)
            bt.ncU <- tkbutton(fr.windUV, text = "...", state = stateNC)

            txt.ncV <- tklabel(fr.windUV, text = lang.dlg[['label']][['5-3']], anchor = 'w', justify = 'left')
            set.ncV <- ttkbutton(fr.windUV, text = .cdtEnv$tcl$lang$global[['button']][['5']], state = stateNC)
            en.ncV <- tkentry(fr.windUV, textvariable = dir.NC_V, width = largeur2, state = stateNC)
            bt.ncV <- tkbutton(fr.windUV, text = "...", state = stateNC)

            tkgrid(txt.ncU, row = 0, column = 0, sticky = 'we', rowspan = 1, columnspan = 3, padx = 1, pady = 1, ipadx = 1, ipady = 1)
            tkgrid(set.ncU, row = 0, column = 3, sticky = 'we', rowspan = 1, columnspan = 2, padx = 1, pady = 1, ipadx = 1, ipady = 1)
            tkgrid(en.ncU, row = 1, column = 0, sticky = 'we', rowspan = 1, columnspan = 4, padx = 0, pady = 1, ipadx = 1, ipady = 1)
            tkgrid(bt.ncU, row = 1, column = 4, sticky = 'we', rowspan = 1, columnspan = 1, padx = 0, pady = 1, ipadx = 1, ipady = 1)
            tkgrid(txt.ncV, row = 2, column = 0, sticky = 'we', rowspan = 1, columnspan = 3, padx = 1, pady = 1, ipadx = 1, ipady = 1)
            tkgrid(set.ncV, row = 2, column = 3, sticky = 'we', rowspan = 1, columnspan = 2, padx = 1, pady = 1, ipadx = 1, ipady = 1)
            tkgrid(en.ncV, row = 3, column = 0, sticky = 'we', rowspan = 1, columnspan = 4, padx = 0, pady = 1, ipadx = 1, ipady = 1)
            tkgrid(bt.ncV, row = 3, column = 4, sticky = 'we', rowspan = 1, columnspan = 1, padx = 0, pady = 1, ipadx = 1, ipady = 1)

            helpWidget(en.ncU, lang.dlg[['tooltip']][['7-2']], lang.dlg[['status']][['7-2']])
            helpWidget(bt.ncU, lang.dlg[['tooltip']][['8']], lang.dlg[['status']][['8']])
            helpWidget(set.ncU, lang.dlg[['tooltip']][['9']], lang.dlg[['status']][['9']])

            helpWidget(en.ncV, lang.dlg[['tooltip']][['7-3']], lang.dlg[['status']][['7-3']])
            helpWidget(bt.ncV, lang.dlg[['tooltip']][['8']], lang.dlg[['status']][['8']])
            helpWidget(set.ncV, lang.dlg[['tooltip']][['9']], lang.dlg[['status']][['9']])

            #####

            tkconfigure(set.ncU, command = function(){
                tcl('wm', 'attributes', tt, topmost = FALSE)
                .cdtData$GalParams[["INPUT.U"]] <- getInfoNetCDFData(tt, .cdtData$GalParams[["INPUT.U"]],
                                                                     trimws(tclvalue(dir.NC_U)))
                tcl('wm', 'attributes', tt, topmost = TRUE)
            })

            tkconfigure(bt.ncU, command = function(){
                tcl('wm', 'attributes', tt, topmost = FALSE)
                dirnc <- tk_choose.dir(getwd(), "")
                tcl('wm', 'attributes', tt, topmost = TRUE)
                tclvalue(dir.NC_U) <- if(!is.na(dirnc)) dirnc else ""
            })

            tkconfigure(set.ncV, command = function(){
                tcl('wm', 'attributes', tt, topmost = FALSE)
                .cdtData$GalParams[["INPUT.V"]] <- getInfoNetCDFData(tt, .cdtData$GalParams[["INPUT.V"]],
                                                                     trimws(tclvalue(dir.NC_V)))
                tcl('wm', 'attributes', tt, topmost = TRUE)
            })

            tkconfigure(bt.ncV, command = function(){
                tcl('wm', 'attributes', tt, topmost = FALSE)
                dirnc <- tk_choose.dir(getwd(), "")
                tcl('wm', 'attributes', tt, topmost = TRUE)
                tclvalue(dir.NC_V) <- if(!is.na(dirnc)) dirnc else ""
            })

            tkgrid(fr.windUV, row = 5, column = 0, sticky = 'we', rowspan = 1, columnspan = 5)
            tcl('update')
        }

        ##########

        tkdestroy(fr.windVar)
        fr.windVar <<- tkframe(frInputData)

        txt.stnU <- tklabel(fr.windVar, text = lang.dlg[['label']][['4-2']], anchor = 'w', justify = 'left')
        cb.stnU <- ttkcombobox(fr.windVar, values = unlist(openFile_ttkcomboList()), textvariable = file.STN_U, width = largeur1)
        addTo_all_Combobox_List(cb.stnU)
        bt.stnU <- tkbutton(fr.windVar, text = "...")

        txt.stnV <- tklabel(fr.windVar, text = lang.dlg[['label']][['4-3']], anchor = 'w', justify = 'left')
        cb.stnV <- ttkcombobox(fr.windVar, values = unlist(openFile_ttkcomboList()), textvariable = file.STN_V, width = largeur1)
        addTo_all_Combobox_List(cb.stnV)
        bt.stnV <- tkbutton(fr.windVar, text = "...")

        chk.windUV <- tkcheckbutton(fr.windVar, variable = windUV, text = lang.dlg[['checkbutton']][['2']], anchor = 'w', justify = 'left')
        fr.windUV <- tkframe(fr.windVar)

        #####
        tkgrid(txt.stnU, row = 0, column = 0, sticky = 'we', rowspan = 1, columnspan = 5, padx = 1, pady = 1, ipadx = 1, ipady = 1)
        tkgrid(cb.stnU, row = 1, column = 0, sticky = 'we', rowspan = 1, columnspan = 4, padx = 0, pady = 1, ipadx = 1, ipady = 1)
        tkgrid(bt.stnU, row = 1, column = 4, sticky = 'we', rowspan = 1, columnspan = 1, padx = 0, pady = 1, ipadx = 1, ipady = 1)
        tkgrid(txt.stnV, row = 2, column = 0, sticky = 'we', rowspan = 1, columnspan = 5, padx = 1, pady = 1, ipadx = 1, ipady = 1)
        tkgrid(cb.stnV, row = 3, column = 0, sticky = 'we', rowspan = 1, columnspan = 4, padx = 0, pady = 1, ipadx = 1, ipady = 1)
        tkgrid(bt.stnV, row = 3, column = 4, sticky = 'we', rowspan = 1, columnspan = 1, padx = 0, pady = 1, ipadx = 1, ipady = 1)
        tkgrid(chk.windUV, row = 4, column = 0, sticky = 'we', rowspan = 1, columnspan = 5, padx = 1, pady = 1, ipadx = 1, ipady = 1)

        helpWidget(cb.stnU, lang.dlg[['tooltip']][['5-2']], lang.dlg[['status']][['5-2']])
        helpWidget(bt.stnU, lang.dlg[['tooltip']][['6']], lang.dlg[['status']][['6']])
        helpWidget(cb.stnV, lang.dlg[['tooltip']][['5-3']], lang.dlg[['status']][['5-3']])
        helpWidget(bt.stnV, lang.dlg[['tooltip']][['6']], lang.dlg[['status']][['6']])
        helpWidget(chk.windUV, lang.dlg[['tooltip']][['7-0']], lang.dlg[['status']][['7-0']])

        #####

        if(tclvalue(windUV) == '1') getwindUVone() else getwindUVsep()

        tkconfigure(bt.stnU, command = function(){
            tcl('wm', 'attributes', tt, topmost = FALSE)
            dat.opfiles <- getOpenFiles(tt)
            tcl('wm', 'attributes', tt, topmost = TRUE)
            if(!is.null(dat.opfiles)){
                update.OpenFiles('ascii', dat.opfiles)
                tclvalue(file.STN_U) <- dat.opfiles[[1]]
            }
        })

        tkconfigure(bt.stnV, command = function(){
            tcl('wm', 'attributes', tt, topmost = FALSE)
            dat.opfiles <- getOpenFiles(tt)
            tcl('wm', 'attributes', tt, topmost = TRUE)
            if(!is.null(dat.opfiles)){
                update.OpenFiles('ascii', dat.opfiles)
                tclvalue(file.STN_V) <- dat.opfiles[[1]]
            }
        })

        tkbind(chk.windUV, "<Button-1>", function(){
            if(tclvalue(windUV) == '1') getwindUVsep() else getwindUVone()
        })

        tkgrid(fr.windVar, row = 1, column = 0, sticky = 'we', rowspan = 1, columnspan = 2)
    }

    ####################################

    cb.biasMthd <- lang.dlg[['combobox']][['1']]
    val.biasMthd <- c("mbvar", "mbmon", "qmdist", "qmecdf")
    bias.method <- tclVar()
    tclvalue(bias.method) <- cb.biasMthd[val.biasMthd %in% .cdtData$GalParams$BIAS$method]

    min.length <- tclVar(.cdtData$GalParams$BIAS$min.length)
    box.lon <- tclVar(.cdtData$GalParams$BIAS$blon)
    box.lat <- tclVar(.cdtData$GalParams$BIAS$blat)

    cb.distrName <- lang.dlg[['combobox']][['4']]
    val.distrName <- c('norm', 'lnorm', 'snorm', 'gamma', 'exp', 'weibull', 'gumbel',
                       'berngamma', 'bernexp', 'bernlnorm', 'bernweibull')
    distr.name <- tclVar()
    tclvalue(distr.name) <- cb.distrName[val.distrName %in% .cdtData$GalParams$BIAS$distr.name]

    cb.tsSupport <- lang.dlg[['combobox']][['3']]
    val.tsSupport <- c('rectbox', 'points')
    ts.support <- tclVar()
    tclvalue(ts.support) <- cb.tsSupport[val.tsSupport %in% .cdtData$GalParams$BIAS$ts.support]

    chunks.exist <- tclVar(.cdtData$GalParams$BIAS$chunks.exist)
    chunks.dir <- tclVar(.cdtData$GalParams$BIAS$chunks.dir)

    #############

    frameBias <- tkframe(frMRG0, relief = 'sunken', borderwidth = 2, pady = 3)

    txt.bias <- tklabel(frameBias, text = lang.dlg[['label']][['7']], anchor = 'e', justify = 'right')
    cb.bias <- ttkcombobox(frameBias, values = cb.biasMthd, textvariable = bias.method, width = largeur3)

    ####################################

    frtimestep <- tkframe(frMRG0, relief = 'sunken', borderwidth = 2, pady = 3)

    file.period <- tclVar()
    CbperiodVAL <- .cdtEnv$tcl$lang$global[['combobox']][['1']][3:6]
    periodVAL <- c('daily', 'pentad', 'dekadal', 'monthly')
    tclvalue(file.period) <- CbperiodVAL[periodVAL %in% .cdtData$GalParams$period]

    cb.period <- ttkcombobox(frtimestep, values = CbperiodVAL, textvariable = file.period, justify = 'center', width = largeur0)
    bt.baseBias <- ttkbutton(frtimestep, text = lang.dlg[['button']][['1']], width = largeur0)

    #######

    tkgrid(cb.period, row = 0, column = 0, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
    tkgrid(bt.baseBias, row = 0, column = 1, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)

    helpWidget(cb.period, lang.dlg[['tooltip']][['3']], lang.dlg[['status']][['3']])
    helpWidget(bt.baseBias, lang.dlg[['tooltip']][['4']], lang.dlg[['status']][['4']])

    #######

    tkconfigure(bt.baseBias, command = function(){
        tcl('wm', 'attributes', tt, topmost = FALSE)
        .cdtData$GalParams[["base.period"]] <- getInfoBasePeriod(tt, .cdtData$GalParams[["base.period"]])
        tcl('wm', 'attributes', tt, topmost = TRUE)
    })

    ####################################

    frInputData <- tkframe(frMRG0, relief = 'sunken', borderwidth = 2)

    windVar <- tclVar()
    cbWindVar <- lang.dlg[['combobox']][['2']]
    valWindVar <- c('speed', 'uv-comp')
    tclvalue(windVar) <- cbWindVar[valWindVar %in% .cdtData$GalParams$wvar]

    file.STN_S <- tclVar(.cdtData$GalParams$STN.S)
    dir.NC_S <- tclVar(.cdtData$GalParams$INPUT.S$dir)

    windUV <- tclVar(.cdtData$GalParams$one.ncdf)
    file.STN_U <- tclVar(.cdtData$GalParams$STN.U)
    file.STN_V <- tclVar(.cdtData$GalParams$STN.V)
    dir.NC_U <- tclVar(.cdtData$GalParams$INPUT.U$dir)
    dir.NC_V <- tclVar(.cdtData$GalParams$INPUT.V$dir)
    dir.NC_UV <- tclVar(.cdtData$GalParams$INPUT.UV$dir)

    txt.windVar <- tklabel(frInputData, text = lang.dlg[['label']][['9']], anchor = 'e', justify = 'right')
    cb.windVar <- ttkcombobox(frInputData, values = cbWindVar, textvariable = windVar, justify = 'center', width = largeur0)
    fr.windVar <- tkframe(frInputData)

    if(.cdtData$GalParams$wvar == "speed") getwindSpeed() else getwindData()

    tkgrid(txt.windVar, row = 0, column = 0, sticky = 'e', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
    tkgrid(cb.windVar, row = 0, column = 1, sticky = 'w', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)

    tkbind(cb.windVar, "<<ComboboxSelected>>", function(){
        wvar <- valWindVar[cbWindVar %in% trimws(tclvalue(windVar))]
        if(wvar == "speed") getwindSpeed() else getwindData()
    })

    ####################################

    frSave <- tkframe(frMRG0, relief = 'sunken', borderwidth = 2)

    dir2save <- tclVar(.cdtData$GalParams$output$dir)

    txt.dir2save <- tklabel(frSave, text = lang.dlg[['label']][['6']], anchor = 'w', justify = 'left')
    en.dir2save <- tkentry(frSave, textvariable = dir2save, width = largeur2)
    bt.dir2save <- tkbutton(frSave, text = "...")

    #####

    tkgrid(txt.dir2save, row = 0, column = 0, sticky = 'we', rowspan = 1, columnspan = 2, padx = 1, pady = 2, ipadx = 1, ipady = 1)
    tkgrid(en.dir2save, row = 1, column = 0, sticky = 'we', rowspan = 1, columnspan = 1, padx = 0, pady = 1, ipadx = 1, ipady = 1)
    tkgrid(bt.dir2save, row = 1, column = 1, sticky = 'we', rowspan = 1, columnspan = 1, padx = 0, pady = 1, ipadx = 1, ipady = 1)

    helpWidget(en.dir2save, lang.dlg[['tooltip']][['10']], lang.dlg[['status']][['10']])
    helpWidget(bt.dir2save, lang.dlg[['tooltip']][['8']], lang.dlg[['status']][['8']])

    #####

    tkconfigure(bt.dir2save, command = function(){
        tcl('wm', 'attributes', tt, topmost = FALSE)
        dir2savepth <- tk_choose.dir(.cdtData$GalParams$output$dir, "")
        tcl('wm', 'attributes', tt, topmost = TRUE)
        if(is.na(dir2savepth))
            tclvalue(dir2save) <- .cdtData$GalParams$output$dir
        else{
            dir.create(dir2savepth, showWarnings = FALSE, recursive = TRUE)
            tclvalue(dir2save) <- dir2savepth
        }
    })

    ####################################

    fr.minstn <- tkframe(frameBias)
    txt.minstn <- tklabel(fr.minstn, text = lang.dlg[['label']][['8']], anchor = 'e', justify = 'right')
    en.minstn <- tkentry(fr.minstn, textvariable = min.length, width = 6)

    ########

    fr.biasOpts <- tkframe(frameBias)

    bias_method_inputs(.cdtData$GalParams$BIAS$method)

    ########
    tkgrid(txt.minstn, row = 0, column = 0, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
    tkgrid(en.minstn, row = 0, column = 1, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)

    tkgrid(txt.bias, row = 0, column = 0, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
    tkgrid(cb.bias, row = 0, column = 1, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
    tkgrid(fr.minstn, row = 1, column = 0, sticky = 'e', rowspan = 1, columnspan = 2, padx = 1, pady = 1, ipadx = 1, ipady = 1)
    tkgrid(fr.biasOpts, row = 2, column = 0, sticky = 'we', rowspan = 1, columnspan = 2, padx = 1, pady = 1, ipadx = 1, ipady = 1)

    helpWidget(cb.bias, lang.dlg[['tooltip']][['11']], lang.dlg[['status']][['11']])
    helpWidget(en.minstn, lang.dlg[['tooltip']][['12']], lang.dlg[['status']][['12']])

    ########

    tkbind(cb.bias, "<<ComboboxSelected>>", function(){
        bsmethod <- val.biasMthd[cb.biasMthd %in% trimws(tclvalue(bias.method))]
        bias_method_inputs(bsmethod)

        tsSupp <- val.tsSupport[cb.tsSupport %in% trimws(tclvalue(ts.support))]
        if(bsmethod %in% c("qmdist", "qmecdf")){
            if(bsmethod == "qmecdf") stateInterp <- "disabled"
            if(bsmethod == "qmdist"){
                stateInterp <- if(tsSupp == 'rectbox') "disabled" else "normal"
            }
        }else{
            stateInterp <- "normal"
        }
        tkconfigure(bt.bias.interp, state = stateInterp)

        stateNC <- if(bsmethod == "qmdist" && tsSupp == 'points') 'disabled' else 'normal'
        ncdata.chunks_bind(stateNC)
    })

    ####################################

    stateInterp <- if(.cdtData$GalParams$BIAS$method == "qmecdf") "disabled" else "normal"

    bt.bias.interp <- ttkbutton(frMRG0, text = lang.dlg[['button']][['2']], state = stateInterp)
    bt.grid.interp <- ttkbutton(frMRG0, text = lang.dlg[['button']][['3']])

    helpWidget(bt.bias.interp, lang.dlg[['tooltip']][['13']], lang.dlg[['status']][['13']])
    helpWidget(bt.grid.interp, lang.dlg[['tooltip']][['14']], lang.dlg[['status']][['14']])

    tkconfigure(bt.bias.interp, command = function(){
        tcl('wm', 'attributes', tt, topmost = FALSE)
        .cdtData$GalParams[["interp"]] <- getInterpolationPars(tt, .cdtData$GalParams[["interp"]], group = 1)
        tcl('wm', 'attributes', tt, topmost = TRUE)
    })

    tkconfigure(bt.grid.interp, command = function(){
        tcl('wm', 'attributes', tt, topmost = FALSE)
        .cdtData$GalParams[["grid"]] <- createGridInterpolation(tt, .cdtData$GalParams[["grid"]])
        tcl('wm', 'attributes', tt, topmost = TRUE)
    })

    ####################################

    tkgrid(frtimestep, row = 0, column = 0, sticky = 'we', padx = 1, pady = 1, ipadx = 1, ipady = 1)
    tkgrid(frInputData, row = 1, column = 0, sticky = 'we', padx = 1, pady = 1, ipadx = 1, ipady = 1)
    tkgrid(frSave, row = 2, column = 0, sticky = 'we', padx = 1, pady = 1, ipadx = 1, ipady = 1)
    tkgrid(frameBias, row = 3, column = 0, sticky = 'we', padx = 1, pady = 1, ipadx = 1, ipady = 1)
    tkgrid(bt.bias.interp, row = 4, column = 0, sticky = 'we', padx = 1, pady = 1, ipadx = 1, ipady = 1)
    tkgrid(bt.grid.interp, row = 5, column = 0, sticky = 'we', padx = 1, pady = 1, ipadx = 1, ipady = 1)

    ############################################

    bt.prm.Opt <- ttkbutton(frMRG1, text = .cdtEnv$tcl$lang$global[['button']][['4']])
    bt.prm.OK <- ttkbutton(frMRG1, text = .cdtEnv$tcl$lang$global[['button']][['1']])
    bt.prm.CA <- ttkbutton(frMRG1, text = .cdtEnv$tcl$lang$global[['button']][['2']])

    #######

    tkconfigure(bt.prm.Opt, command = function(){
        tssupp <- val.tsSupport[cb.tsSupport %in% trimws(tclvalue(ts.support))]
        biasmth <- val.biasMthd[cb.biasMthd %in% trimws(tclvalue(bias.method))]
        distname <- val.distrName[cb.distrName %in% trimws(tclvalue(distr.name))]
        distList <- c('berngamma', 'bernexp', 'bernlnorm', 'bernweibull')
        bernoulli <- biasmth == "qmdist" && distname %in% distList
        interp <- (biasmth %in% c("mbvar", "mbmon") ||
                  (biasmth == "qmdist" && tssupp == "points")) &&
                  .cdtData$GalParams$interp$method %in% c('idw', 'okr')
        mbias <- biasmth %in% c("mbvar", "mbmon")
        rectbox <- biasmth == "qmecdf" || (biasmth == "qmdist" && tssupp == "rectbox")

        biasCoeff_Options(tt, mbias, bernoulli, interp, rectbox)
    })

    tkconfigure(bt.prm.OK, command = function(){
        .cdtData$GalParams$output$dir <- trimws(tclvalue(dir2save))

        if(.cdtData$GalParams$output$dir %in% c("", "NA")){
            cdt.tkmessageBox(tt, message = lang.dlg[['message']][['4']], icon = "warning", type = "ok")
            tkwait.window(tt)
        }

        .cdtData$GalParams$period <- periodVAL[CbperiodVAL %in% trimws(tclvalue(file.period))]

        .cdtData$GalParams$BIAS$method <- val.biasMthd[cb.biasMthd %in% trimws(tclvalue(bias.method))]
        .cdtData$GalParams$BIAS$min.length <- as.numeric(trimws(tclvalue(min.length)))
        .cdtData$GalParams$BIAS$blon <- as.numeric(trimws(tclvalue(box.lon)))
        .cdtData$GalParams$BIAS$blat <- as.numeric(trimws(tclvalue(box.lat)))
        .cdtData$GalParams$BIAS$distr.name <- val.distrName[cb.distrName %in% trimws(tclvalue(distr.name))]
        .cdtData$GalParams$BIAS$ts.support <- val.tsSupport[cb.tsSupport %in% trimws(tclvalue(ts.support))]

        .cdtData$GalParams$BIAS$chunks.exist <- if(tclvalue(chunks.exist) == '1') TRUE else FALSE
        .cdtData$GalParams$BIAS$chunks.dir <- trimws(tclvalue(chunks.dir))

        disableNC <- .cdtData$GalParams$BIAS$method == "qmdist" &&
                     .cdtData$GalParams$BIAS$ts.support == "points" &&
                     .cdtData$GalParams$BIAS$chunks.exist

        .cdtData$GalParams$wvar <- valWindVar[cbWindVar %in% trimws(tclvalue(windVar))]
        if(.cdtData$GalParams$wvar == "speed"){
            .cdtData$GalParams$STN.S <- trimws(tclvalue(file.STN_S))
            .cdtData$GalParams$INPUT.S$dir <- trimws(tclvalue(dir.NC_S))

            if(.cdtData$GalParams$STN.S == ""){
                cdt.tkmessageBox(tt, message = lang.dlg[['message']][['1-1']], icon = "warning", type = "ok")
                tkwait.window(tt)
            }
            if(.cdtData$GalParams$INPUT.S$dir %in% c("", "NA") && !disableNC)
            {
                cdt.tkmessageBox(tt, message = lang.dlg[['message']][['2-1']], icon = "warning", type = "ok")
                tkwait.window(tt)
            }
        }else{
            .cdtData$GalParams$one.ncdf <- switch(tclvalue(windUV), '0' = FALSE, '1' = TRUE)
            .cdtData$GalParams$STN.U <- trimws(tclvalue(file.STN_U))
            .cdtData$GalParams$STN.V <- trimws(tclvalue(file.STN_V))

            if(.cdtData$GalParams$STN.U == ""){
                cdt.tkmessageBox(tt, message = lang.dlg[['message']][['1-2']], icon = "warning", type = "ok")
                tkwait.window(tt)
            }
            if(.cdtData$GalParams$STN.V == ""){
                cdt.tkmessageBox(tt, message = lang.dlg[['message']][['1-3']], icon = "warning", type = "ok")
                tkwait.window(tt)
            }

            if(.cdtData$GalParams$one.ncdf){
                .cdtData$GalParams$INPUT.UV$dir <- trimws(tclvalue(dir.NC_UV))

                if(.cdtData$GalParams$INPUT.UV$dir %in% c("", "NA") &&  !disableNC)
                {
                    cdt.tkmessageBox(tt, message = lang.dlg[['message']][['2-2']], icon = "warning", type = "ok")
                    tkwait.window(tt)
                }
            }else{
                .cdtData$GalParams$INPUT.U$dir <- trimws(tclvalue(dir.NC_U))
                .cdtData$GalParams$INPUT.V$dir <- trimws(tclvalue(dir.NC_V))

                if(.cdtData$GalParams$INPUT.U$dir %in% c("", "NA") &&  !disableNC)
                {
                    cdt.tkmessageBox(tt, message = lang.dlg[['message']][['2-3']], icon = "warning", type = "ok")
                    tkwait.window(tt)
                }
                if(.cdtData$GalParams$INPUT.V$dir %in% c("", "NA") && !disableNC)
                {
                    cdt.tkmessageBox(tt, message = lang.dlg[['message']][['2-4']], icon = "warning", type = "ok")
                    tkwait.window(tt)
                }
            }
        }

        .cdtData$GalParams$message <- lang.dlg[['message']]

        tkgrab.release(tt)
        tkdestroy(tt)
        tkfocus(.cdtEnv$tcl$main$win)
    })

    tkconfigure(bt.prm.CA, command = function(){
        tkgrab.release(tt)
        tkdestroy(tt)
        tkfocus(.cdtEnv$tcl$main$win)
    })

    tkgrid(bt.prm.Opt, row = 0, column = 0, sticky = 'w', padx = 5, pady = 1, ipadx = 1, ipady = 1)
    tkgrid(bt.prm.OK, row = 0, column = 1, sticky = '', padx = 5, pady = 1, ipadx = 1, ipady = 1)
    tkgrid(bt.prm.CA, row = 0, column = 2, sticky = 'e', padx = 5, pady = 1, ipadx = 1, ipady = 1)

    ############################################
    
    tkgrid(frMRG0, row = 0, column = 0, sticky = 'nswe', rowspan = 1, columnspan = 2, padx = 1, pady = 1, ipadx = 1, ipady = 1)
    tkgrid(frMRG1, row = 1, column = 1, sticky = 'se', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)

    ############################################

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
    invisible()
}
