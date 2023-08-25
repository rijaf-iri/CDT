
get.level.list <- function(obj.list){
    ifelse(is.list(obj.list), max(sapply(obj.list, get.level.list)) + 1, 0)
}

get.lang.value0 <- function(obj.list, lang){
    lapply(obj.list, function(child){
        intxt <- sapply(child, "[[", "text")
        inlang <- sapply(child, "[[", ".attrs")
        txt <- trimws(intxt[inlang == "en"])
        if(lang != "en"){
            txt.o <- trimws(intxt[inlang == lang])
            if(length(txt.o) == 1) txt <- txt.o
        }
        txt
    })
}

get.lang.value1 <- function(obj.list, obj.txt, lang){
    out <- lapply(obj.list,"[[", obj.txt)
    if(all(sapply(out, is.null))) return(NULL)
    sapply(seq_along(out), function(i) get.lang.value0(out[i], lang))
}

cdtLanguageParse <- function(xml.path, lang = "en"){
    xml.doc <- XML::xmlParse(xml.path)
    xml.lang <- XML::xmlRoot(xml.doc)
    obj.lang <- NULL
    obj.lang$type <- XML::xmlName(xml.lang)
    xml.lang <- XML::xmlToList(xml.lang)
    XML::free(xml.doc)

    out.lang <- lapply(seq_along(xml.lang), function(j){
        obj.list <- xml.lang[j]
        name.list <- names(obj.list)
        if(name.list == "title")
            ret <- get.lang.value0(obj.list, lang)
        if(name.list == "help"){
            attrs <- sapply(obj.list[[1]],"[[", ".attrs")
            status <- get.lang.value1(obj.list[[1]], "status", lang)
            if(!is.null(status)) names(status) <- attrs
            tooltip <- get.lang.value1(obj.list[[1]], "tooltip", lang)
            if(!is.null(tooltip)) names(tooltip) <- attrs
            ret <- list(status = status, tooltip = tooltip)
        }
        if(name.list == "widget"){
            name.wdg <- names(obj.list[[1]])
            type.wdg <- unique(name.wdg)
            ret <- lapply(type.wdg, function(x){
                obj.list1 <- obj.list[[1]][name.wdg == x]
                attrs <- sapply(obj.list1,"[[", ".attrs")
                obj.list1 <- lapply(obj.list1, function(n) n[!names(n) %in% ".attrs"])
                if(x %in% c("label", "button", "checkbutton", "tab"))
                    out <- sapply(seq_along(obj.list1), function(i) get.lang.value0(obj.list1[i], lang))
                if(x %in% c("combobox", "radiobutton")){
                    out <- lapply(obj.list1, function(xc){
                        xc <- lapply(xc, function(n) n[!names(n) %in% ".attrs"])
                        unname(unlist(sapply(seq_along(xc), function(i) get.lang.value0(xc[i], lang))))
                    })
                }
                names(out) <- attrs
                out
            })
            names(ret) <- type.wdg
        }
        if(name.list %in% c("tab_title", "message")){
            id.msg <- unname(sapply(obj.list[[1]], "[[", ".attrs"))
            obj.list <- lapply(obj.list[[1]], function(n) n[!names(n) %in% ".attrs"])
            ret <- lapply(seq_along(obj.list), function(i){
                out <- get.lang.value0(obj.list[i], lang)
                names(out) <- id.msg[i]
                out
            })
            ret <- list(do.call(c, ret))
            names(ret) <- name.list
        }

        return(ret)
    })
    out.lang <- do.call(c, out.lang)
    out.lang <- c(obj.lang, out.lang)
    out.lang0 <- out.lang
    out.lang <- unlist(out.lang)
    out.lang <- gsub("\\\\n", "\n", out.lang)
    utils::relist(out.lang, out.lang0)
}

cdtLanguageParse.menu <- function(xml.path, lang = "en"){
    xml.doc <- XML::xmlParse(xml.path)
    xml.lang <- XML::xmlRoot(xml.doc)
    obj.lang <- NULL
    obj.lang$type <- XML::xmlName(xml.lang)
    xml.lang <- XML::xmlToList(xml.lang)
    XML::free(xml.doc)

    id.menu <- unname(sapply(xml.lang, "[[", ".attrs"))
    xml.lang <- lapply(xml.lang, function(n) n[!names(n) %in% ".attrs"])
    ret <- lapply(xml.lang, function(obj.list){
        attrs <- sapply(obj.list,"[[", ".attrs")
        obj.list <- lapply(obj.list, function(n) n[!names(n) %in% ".attrs"])
        out <- sapply(seq_along(obj.list), function(i) get.lang.value0(obj.list[i], lang))
        names(out) <- attrs
        out
    })
    names(ret) <- id.menu

    c(obj.lang, ret)
}
