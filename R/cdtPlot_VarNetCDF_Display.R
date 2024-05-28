
PlotNetCDFVarsMaps <- function(){
    don <- .cdtData$EnvData$ncData$map
    inull <- sapply(don, is.null)

    if(all(inull)){
        Insert.Messages.Out(.cdtData$EnvData$message[['9']], TRUE, "e")
        return(NULL)
    }

    mapOP <- .cdtData$EnvData$ncData$MapOp[!inull]
    don <- don[!inull]
    nbPlot <- length(don)

    mapOP <- lapply(mapOP, function(x){
        bbx <- .cdtData$EnvData$tmpMapOp$bbox
        c(x, list(bbox = bbx))
    })
    draw.box <- .cdtData$EnvData$tmpMapOp$draw.box

    #################

    mar.h <- if(nbPlot < 3) c(6.5, 4, 2.5, 1) else c(5.5, 4, 2.5, 1)
    map.args <- lapply(seq(nbPlot), function(j){
        cdt.plotmap.args.ncvar(don[[j]], mapOP[[j]], .cdtData$EnvData$shapefile, mar.h = mar.h)
    })
    mat.layout <- manageLayout(nbPlot)

    graphics::layout(matrix(mat.layout$order, mat.layout$dim[1], mat.layout$dim[2]))
    ret <- lapply(seq(nbPlot), function(j){
        opar <- graphics::par(mar = map.args[[j]]$mar)
        par.plot <- do.call(cdt.plotmap.fun, map.args[[j]]$map.args)
        graphics::par(opar)
        if(draw.box) graphics::box("figure")
        return(par.plot)
    })

    return(0)
}
