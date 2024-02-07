
cdtUserInformation <- function(){
    userInfoFile <- file.path(.cdtDir$dirLocal, 'config', 'user_infos')
    if(!file.exists(userInfoFile)){
        userForms <- cdtUserInfo()
        if(!is.null(userForms)){
            userForms$created <- format(as.POSIXct(Sys.time()), '%Y%m%d%H%M%S', tz = 'UTC')
            if(testConnection()){
                sendForms <- TRUE
            }else{
                userForms$sent <- FALSE
                saveRDS(userForms, userInfoFile)
                sendForms <- FALSE
            }
        }else{
            sendForms <- FALSE
        }
    }else{
        userForms <- readRDS(userInfoFile)
        if(userForms$sent){
            sendForms <- FALSE
        }else{
            if(testConnection()){
                sendForms <- TRUE
            }else{
                sendForms <- FALSE
            }
        }
    }

    if(sendForms){
        future::plan("multisession")
        on.exit(future::plan("sequential"))
        future::future({
            cdt_api <- readRDS(file.path(.cdtDir$Root, 'data', 'tmp_url'))
            res <- httr::POST(cdt_api$endpoint, body = userForms, encode = "json")
            if(httr::status_code(res) == 200){
                ret <- httr::content(res)
                if(ret$response == 'ok'){
                    userForms$sent <- TRUE
                }else{
                    userForms$sent <- FALSE
                }
            }else{
                userForms$sent <- FALSE
            }
            saveRDS(userForms, userInfoFile)
        })
    }

    invisible()
}
