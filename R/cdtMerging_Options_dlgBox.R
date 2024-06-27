
mergingData_Options <- function(parent.win, action, variable, wind = "speed"){
    if(WindowsOS()){
        largeur0 <- 20
        largeur1 <- 50
        largeur2 <- 40
        largeur3 <- 18
        largeur4 <- 49
        largeur5 <- 50
    }else{
        largeur0 <- 20
        largeur1 <- 49
        largeur2 <- 40
        largeur3 <- 18
        largeur4 <- 48
        largeur5 <- 50
    }

    xml.dlg <- file.path(.cdtDir$dirLocal, "languages", "cdtMerging_Options_dlgBox.xml")
    lang.dlg <- cdtLanguageParse(xml.dlg, .cdtData$Config$lang.iso)

    mrgOpts <- merging.options()

    #####################
    tt <- tktoplevel()
    tkgrab.set(tt)
    tkfocus(tt)

    #####################
    frDialog <- tkframe(tt, relief = 'raised', borderwidth = 2)
    frButt <- tkframe(tt)

    #####################

    frMinStn <- tkframe(frDialog, relief = 'groove', borderwidth = 2)

    minstn_mrg <- tclVar(mrgOpts$mrgMinNumberSTN)
    minstn_rk <- tclVar(mrgOpts$rkMinNumberSTN)
    minstn_vgm <- tclVar(mrgOpts$vgmMinNumberSTN)

    txt.mMrg <- tklabel(frMinStn, text = lang.dlg[['label']][['1']], anchor = 'e', justify = 'right')
    en.mMrg <- tkentry(frMinStn, textvariable = minstn_mrg, width = 4)
    txt.mRk <- tklabel(frMinStn, text = lang.dlg[['label']][['2']], anchor = 'e', justify = 'right')
    en.mRk <- tkentry(frMinStn, textvariable = minstn_rk, width = 4)
    txt.mVgm <- tklabel(frMinStn, text = lang.dlg[['label']][['3']], anchor = 'e', justify = 'right')
    en.mVgm <- tkentry(frMinStn, textvariable = minstn_vgm, width = 4)

    tkgrid(txt.mMrg, row = 0, column = 0, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
    tkgrid(en.mMrg, row = 0, column = 1, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
    tkgrid(txt.mRk, row = 1, column = 0, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
    tkgrid(en.mRk, row = 1, column = 1, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
    tkgrid(txt.mVgm, row = 2, column = 0, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
    tkgrid(en.mVgm, row = 2, column = 1, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)

    helpWidget(en.mMrg, lang.dlg[['tooltip']][['1']], lang.dlg[['status']][['1']])
    helpWidget(en.mRk, lang.dlg[['tooltip']][['2']], lang.dlg[['status']][['2']])
    helpWidget(en.mVgm, lang.dlg[['tooltip']][['3']], lang.dlg[['status']][['3']])

    #####################

    frInterp <- tkframe(frDialog, relief = 'groove', borderwidth = 2)

    local_interp <- tclVar(mrgOpts$useLocalInterpolation)
    power_idw <- tclVar(mrgOpts$powerWeightIDW)
    power_shepard <- tclVar(mrgOpts$powerWeightShepard)
    power_barnes <- tclVar(mrgOpts$powerWeightBarnes)

    chk.localI <- tkcheckbutton(frInterp, variable = local_interp, text = lang.dlg[['checkbutton']][['1']], anchor = 'w', justify = 'left')
    txt.pwidw <- tklabel(frInterp, text = lang.dlg[['label']][['4']], anchor = 'e', justify = 'right', width = largeur1)
    en.pwidw <- tkentry(frInterp, textvariable = power_idw, width = 4)
    txt.pwshp <- tklabel(frInterp, text = lang.dlg[['label']][['5']], anchor = 'e', justify = 'right')
    en.pwshp <- tkentry(frInterp, textvariable = power_shepard, width = 4)
    txt.pwbar <- tklabel(frInterp, text = lang.dlg[['label']][['6']], anchor = 'e', justify = 'right')
    en.pwbar <- tkentry(frInterp, textvariable = power_barnes, width = 4)

    tkgrid(chk.localI, row = 0, column = 0, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
    tkgrid(txt.pwidw, row = 1, column = 0, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
    tkgrid(en.pwidw, row = 1, column = 1, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
    tkgrid(txt.pwshp, row = 2, column = 0, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
    tkgrid(en.pwshp, row = 2, column = 1, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
    tkgrid(txt.pwbar, row = 3, column = 0, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
    tkgrid(en.pwbar, row = 3, column = 1, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)

    helpWidget(chk.localI, lang.dlg[['tooltip']][['4']], lang.dlg[['status']][['4']])

    #####################

    frCoarse <- tkframe(frDialog, relief = 'groove', borderwidth = 2)

    add_coarse <- tclVar(mrgOpts$addCoarseGrid)
    save_coarse <- tclVar(mrgOpts$saveGridBuffer)

    if(action == 'crossv'){
        stateSaveC <- 'disabled'
        tclvalue(save_coarse) <- FALSE
    }else{
        stateSaveC <- 'normal'
    }

    chk.acoarse <- tkcheckbutton(frCoarse, variable = add_coarse, text = lang.dlg[['checkbutton']][['2']], anchor = 'w', justify = 'left')
    chk.scoarse <- tkcheckbutton(frCoarse, variable = save_coarse, text = lang.dlg[['checkbutton']][['3']], anchor = 'w', justify = 'left', state = stateSaveC)

    tkgrid(chk.acoarse, row = 0, column = 0, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
    tkgrid(chk.scoarse, row = 1, column = 0, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)

    helpWidget(chk.acoarse, lang.dlg[['tooltip']][['5']], lang.dlg[['status']][['5']])
    helpWidget(chk.scoarse, lang.dlg[['tooltip']][['6']], lang.dlg[['status']][['6']])

    #####################

    frBlock <- tkframe(frDialog, relief = 'groove', borderwidth = 2)

    cbBlockM <- lang.dlg[['combobox']][['1']]
    valBlockM <- c("gaussian", "userdefined")
    block_M <- tclVar()
    tclvalue(block_M) <- cbBlockM[valBlockM %in% mrgOpts$blockType]

    frBlock1 <- tkframe(frBlock)
    txt.block <- tklabel(frBlock1, text = lang.dlg[['label']][['7']], anchor = 'e', justify = 'right')
    cb.block <- ttkcombobox(frBlock1, values = cbBlockM, textvariable = block_M, width = largeur0, justify = 'center')

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
        tb_wx <- tklabel(frameB, text = lang.dlg[['label']][['8']], anchor = 'e', justify = 'right', width = largeur2)
        tb_wy <- tklabel(frameB, text = lang.dlg[['label']][['9']], anchor = 'e', justify = 'right')
        tb_width <- tklabel(frameB, text = lang.dlg[['label']][['10']])
        eb_wx <- tkentry(frameB, width = 6, justify = "right", textvariable = block_Wx)
        eb_wy <- tkentry(frameB, width = 6, justify = "right", textvariable = block_Wy)

        tkgrid(tb_width, row = 0, column = 1, sticky = "ew")
        tkgrid(tb_wx, row = 1, column = 0, sticky = "ew")
        tkgrid(eb_wx, row = 1, column = 1, sticky = "ew")
        tkgrid(tb_wy, row = 2, column = 0, sticky = "ew")
        tkgrid(eb_wy, row = 2, column = 1, sticky = "ew")

        helpWidget(eb_wx, lang.dlg[['tooltip']][['8']], lang.dlg[['status']][['8']])
        helpWidget(eb_wy, lang.dlg[['tooltip']][['9']], lang.dlg[['status']][['9']])

        if(blocktype == "userdefined"){
            tb_step <- tklabel(frameB, text = lang.dlg[['label']][['11']])
            eb_sx <- tkentry(frameB, width = 6, justify = "right", textvariable = block_Sx)
            eb_sy <- tkentry(frameB, width = 6, justify = "right", textvariable = block_Sy)

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

    if(mrgOpts$blockType == "gaussian"){
        bwx <- mrgOpts$blockSize[1]
        bwy <- mrgOpts$blockSize[2]
        bsx <- 0.5
        bsy <- 0.5
    }else{
        bwx <- mrgOpts$blockSize[1]
        bwy <- mrgOpts$blockSize[3]
        bsx <- mrgOpts$blockSize[2]
        bsy <- mrgOpts$blockSize[4]
    }

    block_Wx <- tclVar(bwx)
    block_Wy <- tclVar(bwy)
    block_Sx <- tclVar(bsx)
    block_Sy <- tclVar(bsy)

    set_blockSize(mrgOpts$blockType, frBlock2)

    ###
    tkgrid(frBlock1, row = 0, column = 0, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)

    #####################

    if(action == 'merge'){
        frNCDF <- tkframe(frDialog)

        bt.ncdfvar <- ttkbutton(frNCDF, text = lang.dlg[['button']][['1']], width = largeur5)

        tkgrid(bt.ncdfvar, row = 0, column = 0, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)

        tkconfigure(bt.ncdfvar, command = function(){
            mergingNCDF_Options(tt, variable, wind)
        })
    }

    #####################

    if(variable == 'rain'){
        frRnoR <- tkframe(frDialog, relief = 'groove', borderwidth = 2)

        ###
        frRnoR1 <- tkframe(frRnoR)

        cbRnoRM <- lang.dlg[['combobox']][['2']]
        valRnoRM <- c("logit", "additive")
        rnor_Model <- tclVar()
        tclvalue(rnor_Model) <- cbRnoRM[valRnoRM %in% mrgOpts$RnoRModel]

        txt.rnrMod <- tklabel(frRnoR1, text = lang.dlg[['label']][['12']], anchor = 'e', justify = 'right')
        cb.rnrMod <- ttkcombobox(frRnoR1, values = cbRnoRM, textvariable = rnor_Model, width = largeur3, justify = 'center')

        tkgrid(txt.rnrMod, row = 0, column = 0, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
        tkgrid(cb.rnrMod, row = 0, column = 1, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)

        helpWidget(cb.rnrMod, lang.dlg[['tooltip']][['12']], lang.dlg[['status']][['12']])

        ###
        frRnoR2 <- tkframe(frRnoR)

        rnor_Cutoff <- tclVar(mrgOpts$RnoRCutOff)
        rnor_PxSmooth <- tclVar(mrgOpts$RnoRSmoothingPixels)

        txt.rnrCut <- tklabel(frRnoR2, text = lang.dlg[['label']][['13']], anchor = 'e', justify = 'right', width = largeur4)
        cb.rnrCut <- ttkcombobox(frRnoR2, values = 1:3, textvariable = rnor_Cutoff, width = 3, justify = 'center')
        txt.rnrPx <- tklabel(frRnoR2, text = lang.dlg[['label']][['14']], anchor = 'e', justify = 'right')
        en.rnrPx <- tkentry(frRnoR2, textvariable = rnor_PxSmooth, width = 3)

        tkgrid(txt.rnrCut, row = 0, column = 0, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
        tkgrid(cb.rnrCut, row = 0, column = 1, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
        tkgrid(txt.rnrPx, row = 1, column = 0, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
        tkgrid(en.rnrPx, row = 1, column = 1, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)

        helpWidget(cb.rnrCut, lang.dlg[['tooltip']][['13']], lang.dlg[['status']][['13']])
        helpWidget(en.rnrPx, lang.dlg[['tooltip']][['16']], lang.dlg[['status']][['16']])

        ###
        frRnoR3 <- tkframe(frRnoR)

        rnor_Coarse <- tclVar(mrgOpts$RnoRaddCoarse)
        rnor_Usemrg <- tclVar(mrgOpts$RnoRUseMerged)
        rnor_Save <- tclVar(mrgOpts$saveRnoR)

        if(action == 'crossv'){
            stateSaveR <- 'disabled'
            tclvalue(rnor_Save) <- FALSE
        }else{
            stateSaveR <- 'normal'
        }

        chk.rnrUsemrg <- tkcheckbutton(frRnoR3, variable = rnor_Usemrg, text = lang.dlg[['checkbutton']][['5']], anchor = 'w', justify = 'left')
        chk.rnrCroarse <- tkcheckbutton(frRnoR3, variable = rnor_Coarse, text = lang.dlg[['checkbutton']][['4']], anchor = 'w', justify = 'left')
        chk.rnrSave <- tkcheckbutton(frRnoR3, variable = rnor_Save, text = lang.dlg[['checkbutton']][['6']], anchor = 'w', justify = 'left', state = stateSaveR)

        tkgrid(chk.rnrUsemrg, row = 0, column = 0, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
        tkgrid(chk.rnrCroarse, row = 1, column = 0, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
        tkgrid(chk.rnrSave, row = 2, column = 0, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)

        helpWidget(chk.rnrUsemrg, lang.dlg[['tooltip']][['15']], lang.dlg[['status']][['15']])
        helpWidget(chk.rnrCroarse, lang.dlg[['tooltip']][['14']], lang.dlg[['status']][['14']])

        ###
        tkgrid(frRnoR1, row = 0, column = 0, sticky = '', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
        tkgrid(frRnoR2, row = 1, column = 0, sticky = 'e', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
        tkgrid(frRnoR3, row = 2, column = 0, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
    }

    #####################

    tkgrid(frMinStn, row = 0, column = 0, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
    tkgrid(frInterp, row = 1, column = 0, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
    tkgrid(frCoarse, row = 2, column = 0, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
    tkgrid(frBlock, row = 3, column = 0, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
    if(action == 'merge')
        tkgrid(frNCDF, row = 4, column = 0, sticky = '', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
    if(variable == 'rain')
        tkgrid(frRnoR, row = 5, column = 0, sticky = 'we', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)

    #####################
    bt.opt.OK <- ttkbutton(frButt, text = .cdtEnv$tcl$lang$global[['button']][['1']])
    bt.opt.CA <- ttkbutton(frButt, text = .cdtEnv$tcl$lang$global[['button']][['2']])

    tkconfigure(bt.opt.OK, command = function(){
        nsmrg <- as.numeric(trimws(tclvalue(minstn_mrg)))
        nsrk <- as.numeric(trimws(tclvalue(minstn_rk)))
        nsvgm <- as.numeric(trimws(tclvalue(minstn_vgm)))

        localInt <- if(tclvalue(local_interp) == '1') TRUE else FALSE
        pidw <- as.numeric(trimws(tclvalue(power_idw)))
        pshepard <- as.numeric(trimws(tclvalue(power_shepard)))
        pbarnes <- as.numeric(trimws(tclvalue(power_barnes)))

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

        merging.options(mrgMinNumberSTN = nsmrg, rkMinNumberSTN = nsrk, vgmMinNumberSTN = nsvgm,
                        useLocalInterpolation = localInt, powerWeightIDW = pidw,
                        powerWeightShepard = pshepard, powerWeightBarnes = pbarnes,
                        addCoarseGrid = gridcoarse, saveGridBuffer = savecoarse,
                        blockType = blocktype, blockSize = blocksize)

        if(variable == 'rain'){
            rnrmodel <- valRnoRM[cbRnoRM %in% trimws(tclvalue(rnor_Model))]
            rnrcutoff <- as.integer(trimws(tclvalue(rnor_Cutoff)))
            rnrsmthpx <- as.integer(trimws(tclvalue(rnor_PxSmooth)))

            rnrsave <- if(tclvalue(rnor_Save) == '1') TRUE else FALSE
            rnrusemrg <- if(tclvalue(rnor_Usemrg) == '1') TRUE else FALSE
            rnrcoarse <- if(tclvalue(rnor_Coarse) == '1') TRUE else FALSE

            if(rnrcoarse & !gridcoarse){
                cdt.tkmessageBox(tt, message = lang.dlg[['message']][['2']], icon = "warning", type = "ok")
                tkwait.window(tt)
            }

            merging.options(RnoRModel = rnrmodel, RnoRCutOff = rnrcutoff,
                            RnoRSmoothingPixels = rnrsmthpx, RnoRUseMerged = rnrusemrg,
                            RnoRaddCoarse = rnrcoarse, saveRnoR = rnrsave)
        }

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
        tkfocus(parent.win)
    })
    tkwait.window(tt)
}
