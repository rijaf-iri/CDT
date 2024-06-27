
PlotNetCDFdataMaps <- function(){
    don <- .cdtData$EnvData$ncData$map
    dataMapOp <- .cdtData$EnvData$ncMapOp
    varinfo <- .cdtData$EnvData$varinfo

    ## titre
    if(!dataMapOp$title$user){
        .titre <- paste(.cdtData$EnvData$ncData$file2plot, '-', varinfo$name)
    }else .titre <- dataMapOp$title$title

    if(!dataMapOp$colkeyLab$user){
        dataMapOp$colkeyLab$user <- TRUE
        dataMapOp$colkeyLab$label <- paste0(varinfo$longname, " (", varinfo$units, ")")
    }

    #################

    map.args <- cdt.plotmap.args(don, dataMapOp, .cdtData$EnvData$shapefile)
    opar <- graphics::par(mar = map.args$mar)
    map.args.add <- list(titre = .titre, data.type = "Grid")
    map.args <- map.args[!(names(map.args) %in% "mar")]
    map.args <- c(map.args, map.args.add)
    par.plot <- do.call(cdt.plotmap.fun, map.args)
    ## scale bar
    cdt.plotmap.scalebar(dataMapOp$scalebar)

    graphics::par(opar)
    return(0)
}
