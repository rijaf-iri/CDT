
PlotNetCDFVarsMaps <- function(){
    don <- .cdtData$EnvData$ncData$map
    inull <- sapply(don, is.null)

    mapOP <- .cdtData$EnvData$ncData$MapOp[!inull]
    don <- don[!inull]

    nbPlot <- length(don)

    #################
    ## draw box
    draw.box <- if(tclvalue(.cdtData$EnvData$plot.maps$draw.box) == "0") FALSE else TRUE
    PlotType <- str_trim(tclvalue(.cdtData$EnvData$plot.maps$plot.type))
    shpf <- .cdtData$EnvData$shp
    SHPOp <- .cdtData$EnvData$SHPOp

    #################

    mar.h <- if(nbPlot < 3) c(6.5, 4, 2.5, 1) else c(5.5, 4, 2.5, 1)
    map.args <- lapply(seq(nbPlot), function(j){
        cdt.plotmap.args.ncvar(don[[j]], mapOP[[j]], PlotType,
                                shpf, SHPOp, mar.h = mar.h)
    })
    mat.layout <- manageLayout(nbPlot)

    layout(matrix(mat.layout$order, mat.layout$dim[1], mat.layout$dim[2]))
    ret <- lapply(seq(nbPlot), function(j){
        opar <- par(mar = map.args[[j]]$mar)
        par.plot <- do.call(cdt.plotmap.fun, map.args[[j]]$map.args)
        par(opar)
        if(draw.box) box("figure")
        return(par.plot)
    })

    return(0)
}
