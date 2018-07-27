

#' Function to get a valid token for the API.
#'
#' The token is used in Authorization header in each request to
#' the API.
#'
#' @param client_id A client id from MK Online.
#' @param client_secret A client secret from MK Online.
mk_token <- function(client_id = NULL, client_secret = NULL){

  if(is.null(client_id)){
    client_id <- Sys.getenv("MK_CLIENT_ID")
  }
  if(is.null(client_secret)){
    client_secret <- Sys.getenv("MK_CLIENT_SECRET")
  }

  if(client_id == "" | client_secret == ""){
    stop("The MK_CLIENT_ID or the MK_CLIENT_SECRET variables are not saved in the .Renviron file.")
  }

  auth <- httr::authenticate(user = client_id, password = client_secret)

  httr::set_config(auth)

  cnt <- httr::POST(url = "https://auth.wattsight.com/oauth2/token",
                    httr::user_agent("https://github.com/krose/mkonline2"),
                    body = list(grant_type = "client_credentials"),
                    encode = "form")

  if(httr::http_error(cnt)){
    stop(paste0("The token could not be retrieved. Error message: ",
                httr::http_status(cnt)$message))
  }

  cnt <- httr::content(cnt)

  cnt
}
