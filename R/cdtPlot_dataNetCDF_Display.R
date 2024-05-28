
PlotNetCDFdataMaps <- function(){
    don <- .cdtData$EnvData$ncData$map
    dataMapOp <- .cdtData$EnvData$ncMapOp

    ## titre
    if(!dataMapOp$title$user){
        .titre <- .cdtData$EnvData$ncData$file2plot
    }else .titre <- dataMapOp$title$title

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
