
## Code adapted from http://www.sciviews.org/recipes/tcltk/TclTk-using-tk-table-widget/

tclArrayVar <- function(Rarray = NULL){
    if(!is.null(Rarray) && !is.vector(Rarray) && length(dim(Rarray)) != 2)
        stop("Array must be one-dimensional or two-dimensional.")

    n <- .TkRoot$env$TclVarCount <- .TkRoot$env$TclVarCount + 1L
    name <- paste0("::RTcl", n)
    arr.env <- list(env = new.env(), nrow = 0, ncol = 0, ndim = 0)
    assign(name, NULL, envir = arr.env$env)
    reg.finalizer(arr.env$env, function(env) tcl("unset", ls(env)))
    class(arr.env) <- "tclArrayVar"

    if(is.null(Rarray)){
        ndim <- 2
        nrowR <- 0
        ncolR <- 0
        rownameR <- NULL
        colnameR <- NULL
        .Tcl(paste0("set ", name, "(0,0) \"\""))
    }else{
        ndim <- if(is.vector(Rarray)) 1 else 2
        # Rarray <- as.data.frame(Rarray)
        nrowR <- nrow(Rarray)
        ncolR <- ncol(Rarray)
        seq_row <- 1:nrowR
        seq_col <- 1:ncolR
        colnameR <- colnames(Rarray)
        rownameR <- rownames(Rarray)

        vec <- do.call(c, lapply(seq_col, function(i) as.character(Rarray[, i])))
        vec[is.na(vec)] <- ''

        mat1 <- if(ndim == 2) cbind(as.matrix(expand.grid(seq_row, seq_col)), vec) else cbind(seq_row, 1, vec)
        mat2 <- if(!is.null(rownameR)) cbind(seq_row, 0, rownameR) else cbind(seq_row, 0, seq_row)
        mat3 <- if(!is.null(colnameR)) cbind(0, seq_col, colnameR) else cbind(0, seq_col, paste0('X', seq_col))
        mat <- rbind(mat2, mat3, mat1)
        tabs <- paste0('set ', name, '(', mat[, 1], ',', mat[, 2], ') "', mat[, 3], '"')
        for(i in seq_along(tabs)) .Tcl(tabs[i])
    }

    arr.env$nrow <- nrowR
    arr.env$ncol <- ncolR
    arr.env$ndim <- ndim
    arr.env$row.names <- if(is.null(rownameR)) FALSE else TRUE
    arr.env$col.names <- if(is.null(colnameR)) FALSE else TRUE

    return(arr.env)
}

#' @exportS3Method NULL
`[.tclArrayVar` <- function(object, i, j = NULL) {
    if(is.null(j) && object$ndim != 1)
        stop("Object is not a one-dimensional Tclarray")
    if(!is.null(j) && object$ndim != 2)
        stop("Object is not a two-dimensional Tclarray")
    if(object$ndim == 1) j <- 1
    tclArrayName <- ls(object$env)
    tclvalue(paste0(tclArrayName, "(", i, ",", j, ")"))
}

# assign("[.tclArrayVar", function(object, i, j = NULL) {
#     if(is.null(j) && object$ndim != 1)
#         stop("Object is not a one-dimensional Tclarray")
#     if(!is.null(j) && object$ndim != 2)
#         stop("Object is not a two-dimensional Tclarray")
#     if(object$ndim == 1) j <- 1
#     tclArrayName <- ls(object$env)
#     tclvalue(paste0(tclArrayName, "(", i, ",", j, ")"))
# })

#' @exportS3Method NULL
`[<-.tclArrayVar` <- function(object, i, j = NULL, value) {
    if(is.null(j) && object$ndim != 1)
        stop("Object is not a one-dimensional Tclarray")
    if(!is.null(j) && object$ndim != 2)
        stop("Object is not a two-dimensional Tclarray")
    if(object$ndim == 1) j <- 1
    tclArrayName <- ls(object$env)
    if(is.null(value) || is.na(value) || value == "")
        .Tcl(paste0("set ", tclArrayName, "(", i, ",", j, ") \"\""))
    else
        .Tcl(paste0("set ", tclArrayName, "(", i, ",", j, ") ", value))
    if(i > object$nrow) object$nrow <- i
    return(object)
}

# assign("[<-.tclArrayVar", function(object, i, j = NULL, value) {
#     if(is.null(j) && object$ndim != 1)
#         stop("Object is not a one-dimensional Tclarray")
#     if(!is.null(j) && object$ndim != 2)
#         stop("Object is not a two-dimensional Tclarray")
#     if(object$ndim == 1) j <- 1
#     tclArrayName <- ls(object$env)
#     if(is.null(value) || is.na(value) || value == "")
#         .Tcl(paste0("set ", tclArrayName, "(", i, ",", j, ") \"\""))
#     else
#         .Tcl(paste0("set ", tclArrayName, "(", i, ",", j, ") ", value))
#     if(i > object$nrow) object$nrow <- i
#     return(object)
# })

########################################################################

## display table
displayTable <- function(parent, tclArray = NULL, colwidth = 10){
    if(!is.null(tclArray)){
        for(j in (tclArray$ncol+1):(tclArray$ncol + 20)) tclArray[0, j] <- paste0('X', j)
        for(i in (tclArray$nrow+1):(tclArray$nrow + 50)) tclArray[i, 0] <- i
    }else{
        tclArray <- tclArrayVar()
        for(j in 1:40) tclArray[0, j] <- paste0('X', j)
        for(i in 1:100) tclArray[i, 0] <- i
    }

    xscr <- tkscrollbar(parent, orient = "horizontal", command = function(...) tkxview(table, ...))
    yscr <- tkscrollbar(parent, command = function(...) tkyview(table, ...))
    table <- tkwidget(parent, "table",
                    xscrollcommand = function(...) tkset(xscr, ...),
                    yscrollcommand = function(...) tkset(yscr, ...))

    tktag.configure(table, "active", foreground = 'red', background = 'white')
    tktag.configure(table, "sel", foreground = 'blue', background = 'yellow')
    tcl(table, 'width', '0', '5')
    tkconfigure(table, variable = ls(tclArray$env), foreground = "black", background = "white")
    tkconfigure(table, rows = tclArray$nrow, cols = tclArray$ncol + 20, colwidth = colwidth)
    tkconfigure(table, anchor = "nw", justify = "right")
    tkconfigure(table, titlerows = 1, titlecols = 1, selecttitle = 1)
    tkconfigure(table, colorigin = 0, roworigin = 0)
    tkconfigure(table, rowseparator = "\n", colseparator = "\t", selectmode = "extended")
    tkconfigure(table, resizeborders = "both", drawmode = "single")
    tkconfigure(table, highlightcolor = "blue", highlightthickness = 0)
    tkconfigure(table, colstretchmode = "all", rowstretchmode = "all")
    tkconfigure(table, cache = 1, validate = 1, wrap = 1)
    tkconfigure(table, ipadx = 1, ipady = 1)

    tkgrid(table, yscr)
    tkgrid.configure(yscr, sticky = "ns")
    tkgrid(xscr, sticky = "ew")

    tkgrid.configure(table, sticky = 'nswe')
    tkgrid.rowconfigure(table, 0, weight = 1)
    tkgrid.columnconfigure(table, 0, weight = 1)

    tclArray$ncol <- tclArray$ncol + 20

    return(list(table, tclArray))
}

########################################################################

## convert tclArray to data.frame
tclArray2dataframe <- function(object){
    ncol <- as.integer(tclvalue(tkindex(object[[1]], 'end', 'col'))) 
    nrow <- as.integer(tclvalue(tkindex(object[[1]],'end', 'row')))  
    Rarray <- ls(object[[2]]$env)
    rownom <- unlist(lapply(paste0(Rarray, "(", 1:nrow, ",", 0, ")"), function(x) tclvalue(x)))
    colnom <- unlist(lapply(paste0(Rarray, "(", 0, ",", 1:ncol, ")"), function(x) tclvalue(x)))

    grid.array <- expand.grid(1:nrow, 1:ncol)
    amat.array <- paste0(Rarray, "(", grid.array[, 1], ",", grid.array[, 2], ")")
    exist.array <- unlist(lapply(paste("info", "exists", amat.array, sep = " "),
                                function(x) ifelse(tclvalue(.Tcl(x)) == "1", TRUE, FALSE)))
    amat.array <- amat.array[exist.array]
    grid.array <- grid.array[exist.array, ]
    values.array <- unlist(lapply(amat.array, function(x) tclvalue(x)))
    data.array <- data.frame(grid.array, values.array, stringsAsFactors = FALSE)

    if(nrow(data.array) > 0){
        ret.array <- reshape.array(data.array)
        ret.array[ret.array == ""] <- NA
        colnames(ret.array) <- colnom[1:ncol(ret.array)]
        rownames(ret.array) <- rownom[1:nrow(ret.array)]
    }else ret.array <- NULL

    return(ret.array)
}
