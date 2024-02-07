
cdtImagesZoom <- function(){
    .cdtEnv$tcl$zoom$img$plus <- resizeTclImage(file.path(.cdtDir$Root, "images", 'ZoomIn128.gif'), factor = 4, zoom = FALSE)
    .cdtEnv$tcl$zoom$img$moins <- resizeTclImage(file.path(.cdtDir$Root, "images", 'ZoomOut128.gif'), factor = 4, zoom = FALSE)
    .cdtEnv$tcl$zoom$img$rect <- resizeTclImage(file.path(.cdtDir$Root, "images", 'ZoomRect128.gif'), factor = 4, zoom = FALSE)
    .cdtEnv$tcl$zoom$img$centre <- tkimage.create('photo', file = file.path(.cdtDir$Root, "images", 'imgCentre24.gif'))
    .cdtEnv$tcl$zoom$img$redraw <- resizeTclImage(file.path(.cdtDir$Root, "images", 'redraw128.gif'), factor = 4, zoom = FALSE)
    .cdtEnv$tcl$zoom$img$pan <- tkimage.create('photo', file = file.path(.cdtDir$Root, "images", 'PanImage32.gif'))
    .cdtEnv$tcl$zoom$img$reset <- resizeTclImage(file.path(.cdtDir$Root, "images", 'reset128.gif'), factor = 4, zoom = FALSE)

    invisible()
}
