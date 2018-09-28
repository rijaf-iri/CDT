aboutCDT <- function(){
	tt <- tktoplevel()
	tkgrab.set(tt)
	tkfocus(tt)

	###########
	frameLogo <- tkframe(tt)

	irilogo.path <- file.path(.cdtDir$Root, "images", "iriLogo.gif")
	irilogo <- tkimage.create('photo',"::tcl::logo_origin", file = irilogo.path)
	irilogo1 <- tkimage.create("photo", "::tcl::logo_transfo")
	tcl(irilogo1, "copy", irilogo, subsample = 4)

	img.irilogo <- tklabel(frameLogo, image = irilogo1)
	tkgrid(img.irilogo)

	###########
	frameCDT <- tkframe(tt)

	font1 <- tkfont.create(family = "times", size = 20, weight = "bold")
	font2 <- tkfont.create(family = "times", size = 12, weight = "bold")
	cdtl <- tklabel(frameCDT, text = 'Climate Data Tools', font = font1)
	cdtv <- tklabel(frameCDT, text = paste("Version:", .cdtEnv$pkg$version), font = font2)

	tkgrid(cdtl, sticky = 'ew')
	tkgrid(cdtv, sticky = 'ew')

	###########
	frameDescrp <- tkframe(tt, relief = 'groove', borderwidth = 2)

	xscr <- tkscrollbar(frameDescrp, repeatinterval = 5, orient = "horizontal",
						command = function(...) tkxview(txta, ...))
	yscr <- tkscrollbar(frameDescrp, repeatinterval = 5,
						command = function(...) tkyview(txta, ...))
	txta <- tktext(frameDescrp, bg = "white", font = "courier",
					wrap = "none", height = 8, width = 50,
					xscrollcommand = function(...) tkset(xscr, ...),
					yscrollcommand = function(...) tkset(yscr, ...))

	####
	tkgrid(txta, yscr)
	tkgrid(xscr)
	tkgrid.configure(txta, sticky = "nsew")
	tkgrid.configure(yscr, sticky = "ns")
	tkgrid.configure(xscr, sticky = "ew")

	####
	descrp <- packageDescription("CDT")
	# descrp$Description <- gsub('\n', ' ', descrp$Description)
	descrp$Depends <- gsub('\n', ' ', descrp$Depends)
	descrp$Imports <- gsub('\n', ' ', descrp$Imports)
	descrp <- do.call(c, descrp)
	descrp <- paste(paste0(names(descrp), ":"), descrp)
	descrp <- paste0(descrp, collapse = "\n\n")

	font4 <- tkfont.create(family = "courier", size = 11)
	tktag.configure(txta, "font4f", font = font4)
	tkinsert(txta, "end", descrp, "font4f")

	###########
	frameClose <- tkframe(tt)

	bt.close <- ttkbutton(frameClose, text = "Close")
	tkconfigure(bt.close, command = function(){
		tkgrab.release(tt)
		tkdestroy(tt)
		tkfocus(.cdtEnv$tcl$main$win)
	})

	tkgrid(bt.close)

	###########

	tkgrid(frameLogo, row = 0, column = 0, sticky = 'nw', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
	tkgrid(frameCDT, row = 0, column = 1, sticky = 'ew', rowspan = 1, columnspan = 1, padx = 1, pady = 1, ipadx = 1, ipady = 1)
	tkgrid(frameDescrp, row = 1, column = 0, sticky = 'ew', rowspan = 1, columnspan = 2, padx = 1, pady = 1, ipadx = 1, ipady = 1)
	tkgrid(frameClose, row = 2, column = 0, sticky = '', rowspan = 1, columnspan = 2, padx = 1, pady = 1, ipadx = 1, ipady = 1)

	###########

	tkwm.withdraw(tt)
	tcl('update')
	tt.w <- as.integer(tkwinfo("reqwidth", tt))
	tt.h <- as.integer(tkwinfo("reqheight", tt))
	tt.x <- as.integer(.cdtEnv$tcl$data$width.scr*0.5 - tt.w*0.5)
	tt.y <- as.integer(.cdtEnv$tcl$data$height.scr*0.5 - tt.h*0.5)
	tkwm.geometry(tt, paste0('+', tt.x, '+', tt.y))
	tkwm.transient(tt)
	tkwm.title(tt, 'About CDT')
	tkwm.deiconify(tt)

	tkfocus(tt)
	tkbind(tt, "<Destroy>", function(){
		tkgrab.release(tt)
		tkfocus(.cdtEnv$tcl$main$win)
	})
	tkwait.window(tt)
}
