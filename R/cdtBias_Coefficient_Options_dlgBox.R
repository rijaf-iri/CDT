biasCoeff_Options <- function(parent.win, mbias, bernoulli, interp, rectbox){
    if(WindowsOS()){
        largeur0 <- 47
        largeur1 <- 25
        largeur2 <- 30
        largeur3 <- 22
        largeur4 <- 44
    }else{
        largeur0 <- 47
        largeur1 <- 25
        largeur2 <- 30
        largeur3 <- 22
        largeur4 <- 44
    }

    xml.dlg <- file.path(.cdtDir$dirLocal, "languages", "cdtBias_Coefficient_Options_dlgBox.xml")
    lang.dlg <- cdtLanguageParse(xml.dlg, .cdtData$Config$lang.iso)

    biasOpts <- biascoeff.options()

    #####################
    tt <- tktoplevel()
    tkgrab.set(tt)
    tkfocus(tt)

    #####################
    frDialog <- tkframe(tt, relief = 'raised', borderwidth = 2)
    frButt <- tkframe(tt)

    #####################

    frMBias <- tkframe(frDialog, relief = 'groove', borderwidth = 2)

    mbias_fun <- tclVar(biasOpts$mulBiasFunction)

    stateMB <- if(mbias) 'normal' else 'disabled'

    txt.mbias <- tklabel(frMBias, text = lang.dlg[['label']][['1']], anchor = 'e', justify = 'right', width = largeur0)
    cb.mbias <- ttkcombobox(frMBias, values = c("mean", "median"), textvariable = mbias_fun, width = 8, justify = 'center', state = stateMB)

    tkgrid(txt.mbias, row = 0, column = 0, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 2)
    tkgrid(cb.mbias, row = 0, column = 1, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 2)

    helpWidget(cb.mbias, lang.dlg[['tooltip']][['1']], lang.dlg[['status']][['1']])

    #####################

    frBox <- tkframe(frDialog, relief = 'groove', borderwidth = 2)

    cbBoxStn <- lang.dlg[['combobox']][['1']]
    valBoxStn <- c("idw", "mean")
    aggr_boxstn <- tclVar()
    tclvalue(aggr_boxstn) <- cbBoxStn[valBoxStn %in% biasOpts$aggrBoxMethodStation]

    cbBoxGrd <- lang.dlg[['combobox']][['2']]
    valBoxGrd <- c("bilinear", "weightedAverage")
    aggr_boxgrd <- tclVar()
    tclvalue(aggr_boxgrd) <- cbBoxGrd[valBoxGrd %in% biasOpts$aggrBoxMethodGrid]

    max_neighbor <- tclVar(biasOpts$maxBoxNeighbor)

    stateBox <- if(rectbox) 'normal' else 'disabled'

    txt.boxstn <- tklabel(frBox, text = lang.dlg[['label']][['2']], anchor = 'e', justify = 'right')
    cb.boxstn <- ttkcombobox(frBox, values = cbBoxStn, textvariable = aggr_boxstn, width = largeur1, justify = 'center', state = stateBox)
    txt.boxgrd <- tklabel(frBox, text = lang.dlg[['label']][['3']], anchor = 'e', justify = 'right')
    cb.boxgrd <- ttkcombobox(frBox, values = cbBoxGrd, textvariable = aggr_boxgrd, width = largeur1, justify = 'center', state = stateBox)
    txt.boxmax <- tklabel(frBox, text = lang.dlg[['label']][['10']], anchor = 'e', justify = 'right')
    en.boxmax <- tkentry(frBox, textvariable = max_neighbor, width = 5, justify = 'right', state = stateBox)

    tkgrid(txt.boxstn, row = 0, column = 0, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
    tkgrid(cb.boxstn, row = 0, column = 1, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
    tkgrid(txt.boxgrd, row = 1, column = 0, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
    tkgrid(cb.boxgrd, row = 1, column = 1, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
    tkgrid(txt.boxmax, row = 2, column = 0, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
    tkgrid(en.boxmax, row = 2, column = 1, sticky = 'w', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)

    helpWidget(cb.boxstn, lang.dlg[['tooltip']][['2']], lang.dlg[['status']][['2']])
    helpWidget(cb.boxgrd, lang.dlg[['tooltip']][['3']], lang.dlg[['status']][['3']])
    helpWidget(en.boxmax, lang.dlg[['tooltip']][['12']], lang.dlg[['status']][['12']])

    #####################

    frThres <- tkframe(frDialog, relief = 'groove', borderwidth = 2)

    rain_thres <- tclVar(biasOpts$rainyEventThres)

    stateThres <- if(bernoulli) 'normal' else 'disabled'

    txt.thres <- tklabel(frThres, text = lang.dlg[['label']][['4']], anchor = 'e', justify = 'right', width = largeur2)
    en.thres <- tkentry(frThres, textvariable = rain_thres, width = 5, justify = 'right', state = stateThres)

    tkgrid(txt.thres, row = 0, column = 0, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 2)
    tkgrid(en.thres, row = 0, column = 1, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 2)

    helpWidget(en.thres, lang.dlg[['tooltip']][['4']], lang.dlg[['status']][['4']])

    #####################

    frCoarse <- tkframe(frDialog, relief = 'groove', borderwidth = 2)

    add_coarse <- tclVar(biasOpts$addCoarseGrid)
    save_coarse <- tclVar(biasOpts$saveCoarseGrid)

    stateCoarse <- if(interp) 'normal' else 'disabled'

    chk.acoarse <- tkcheckbutton(frCoarse, variable = add_coarse, text = lang.dlg[['checkbutton']][['1']], anchor = 'w', justify = 'left', state = stateCoarse)
    chk.scoarse <- tkcheckbutton(frCoarse, variable = save_coarse, text = lang.dlg[['checkbutton']][['2']], anchor = 'w', justify = 'left', state = stateCoarse)

    tkgrid(chk.acoarse, row = 0, column = 0, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
    tkgrid(chk.scoarse, row = 1, column = 0, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)

    helpWidget(chk.acoarse, lang.dlg[['tooltip']][['5']], lang.dlg[['status']][['5']])
    helpWidget(chk.scoarse, lang.dlg[['tooltip']][['6']], lang.dlg[['status']][['6']])

    #####################

    frBlock <- tkframe(frDialog, relief = 'groove', borderwidth = 2)

    cbBlockM <- lang.dlg[['combobox']][['3']]
    valBlockM <- c("gaussian", "userdefined")
    block_M <- tclVar()
    tclvalue(block_M) <- cbBlockM[valBlockM %in% biasOpts$blockType]

    stateBlock <- if(interp) 'normal' else 'disabled'

    frBlock1 <- tkframe(frBlock)
    txt.block <- tklabel(frBlock1, text = lang.dlg[['label']][['5']], anchor = 'e', justify = 'right')
    cb.block <- ttkcombobox(frBlock1, values = cbBlockM, textvariable = block_M, width = largeur3, justify = 'center', state = stateBlock)

    tkgrid(txt.block, row = 0, column = 0, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
    tkgrid(cb.block, row = 0, column = 1, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)

    helpWidget(cb.block, lang.dlg[['tooltip']][['7']], lang.dlg[['status']][['7']])

    tkbind(cb.block, "<<ComboboxSelected>>", function(){
        blocktype <- valBlockM[cbBlockM %in% trimws(tclvalue(block_M))]
        tkdestroy(frBlock2)
        frBlock2 <<- tkframe(frBlock)
        set_blockSize(blocktype, frBlock2)
    })

    ########

    set_blockSize <- function(blocktype, frameB){
        tb_wx <- tklabel(frameB, text = lang.dlg[['label']][['6']], anchor = 'e', justify = 'right', width = largeur4)
        tb_wy <- tklabel(frameB, text = lang.dlg[['label']][['7']], anchor = 'e', justify = 'right')
        tb_width <- tklabel(frameB, text = lang.dlg[['label']][['8']])
        eb_wx <- tkentry(frameB, width = 6, justify = "right", textvariable = block_Wx, state = stateBlock)
        eb_wy <- tkentry(frameB, width = 6, justify = "right", textvariable = block_Wy, state = stateBlock)

        tkgrid(tb_width, row = 0, column = 1, sticky = "ew")
        tkgrid(tb_wx, row = 1, column = 0, sticky = "ew")
        tkgrid(eb_wx, row = 1, column = 1, sticky = "ew")
        tkgrid(tb_wy, row = 2, column = 0, sticky = "ew")
        tkgrid(eb_wy, row = 2, column = 1, sticky = "ew")

        helpWidget(eb_wx, lang.dlg[['tooltip']][['8']], lang.dlg[['status']][['8']])
        helpWidget(eb_wy, lang.dlg[['tooltip']][['9']], lang.dlg[['status']][['9']])

        if(blocktype == "userdefined"){
            tb_step <- tklabel(frameB, text = lang.dlg[['label']][['9']])
            eb_sx <- tkentry(frameB, width = 6, justify = "right", textvariable = block_Sx, state = stateBlock)
            eb_sy <- tkentry(frameB, width = 6, justify = "right", textvariable = block_Sy, state = stateBlock)

            tkgrid(tb_step, row = 0, column = 2, sticky = "ew")
            tkgrid(eb_sx, row = 1, column = 2, sticky = "ew")
            tkgrid(eb_sy, row = 2, column = 2, sticky = "ew")

            helpWidget(eb_sx, lang.dlg[['tooltip']][['10']], lang.dlg[['status']][['10']])
            helpWidget(eb_sy, lang.dlg[['tooltip']][['11']], lang.dlg[['status']][['11']])
        }

        tkgrid(frameB, row = 1, column = 0, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
    }

    ########

    frBlock2 <- tkframe(frBlock)

    if(biasOpts$blockType == "gaussian"){
        bwx <- biasOpts$blockSize[1]
        bwy <- biasOpts$blockSize[2]
        bsx <- 0.5
        bsy <- 0.5
    }else{
        bwx <- biasOpts$blockSize[1]
        bwy <- biasOpts$blockSize[3]
        bsx <- biasOpts$blockSize[2]
        bsy <- biasOpts$blockSize[4]
    }

    block_Wx <- tclVar(bwx)
    block_Wy <- tclVar(bwy)
    block_Sx <- tclVar(bsx)
    block_Sy <- tclVar(bsy)

    set_blockSize(biasOpts$blockType, frBlock2)

    ###
    tkgrid(frBlock1, row = 0, column = 0, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)

    #####################

    tkgrid(frMBias, row = 0, column = 0, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 2)
    tkgrid(frBox, row = 1, column = 0, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 5, ipadx = 1, ipady = 2)
    tkgrid(frThres, row = 2, column = 0, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 5, ipadx = 1, ipady = 2)
    tkgrid(frCoarse, row = 3, column = 0, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 5, ipadx = 1, ipady = 2)
    tkgrid(frBlock, row = 4, column = 0, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 2)

    #####################
    bt.opt.OK <- ttkbutton(frButt, text = .cdtEnv$tcl$lang$global[['button']][['1']])
    bt.opt.CA <- ttkbutton(frButt, text = .cdtEnv$tcl$lang$global[['button']][['2']])

    tkconfigure(bt.opt.OK, command = function(){
        mbiasf <- trimws(tclvalue(mbias_fun))
        boxstn <- valBoxStn[cbBoxStn %in% trimws(tclvalue(aggr_boxstn))]
        boxgrd <- valBoxGrd[cbBoxGrd %in% trimws(tclvalue(aggr_boxgrd))]
        maxngb <- as.numeric(trimws(tclvalue(max_neighbor)))
        rainyt <- as.numeric(trimws(tclvalue(rain_thres)))

        gridcoarse <- if(tclvalue(add_coarse) == '1') TRUE else FALSE
        savecoarse <- if(tclvalue(save_coarse) == '1') TRUE else FALSE

        blocktype <- valBlockM[cbBlockM %in% trimws(tclvalue(block_M))]
        width_x <- as.numeric(trimws(tclvalue(block_Wx)))
        width_y <- as.numeric(trimws(tclvalue(block_Wy)))
        by_x <- as.numeric(trimws(tclvalue(block_Sx)))
        by_y <- as.numeric(trimws(tclvalue(block_Sy)))

        if(blocktype == "userdefined"){
            if((width_x < by_x) || (width_y < by_y)){
                cdt.tkmessageBox(tt, message = lang.dlg[['message']][['1']], icon = "warning", type = "ok")
                tkwait.window(tt)
            }
        }

        if(blocktype == "gaussian"){
            blocksize <- c(width_x, width_y)
        }else{
            by_x <- as.numeric(trimws(tclvalue(block_Sx)))
            by_y <- as.numeric(trimws(tclvalue(block_Sy)))
            blocksize <- c(width_x, by_x, width_y, by_y)
        }

        biascoeff.options(mulBiasFunction = mbiasf, aggrBoxMethodStation = boxstn,
                          aggrBoxMethodGrid = boxgrd, maxBoxNeighbor = maxngb, rainyEventThres = rainyt,
                          addCoarseGrid = gridcoarse, saveCoarseGrid=savecoarse,
                          blockType = blocktype, blockSize = blocksize)

        tkgrab.release(tt)
        tkdestroy(tt)
        tkfocus(parent.win)
    })

    tkconfigure(bt.opt.CA, command = function(){
        tkgrab.release(tt)
        tkdestroy(tt)
        tkfocus(parent.win)
    })

    tkgrid(bt.opt.OK, row = 0, column = 0, padx = 5, pady = 1, ipadx = 1, sticky = 'w')
    tkgrid(bt.opt.CA, row = 0, column = 1, padx = 5, pady = 1, ipadx = 1, sticky = 'e')

    ##########################

    tkgrid(frDialog, row = 0, column = 0, sticky = 'nswe', rowspan = 1, columnspan = 2, padx = 1, pady = 1, ipadx = 1, ipady = 1)
    tkgrid(frButt, row = 1, column = 1, sticky = 'se', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)

    #####################
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
        tkfocus(parent.win)
    })
    tkwait.window(tt)
}
