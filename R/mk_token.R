

#' Function to get a valid token for the API.
#'
#' The token is used in Authorization header in each request to
#' the API.
#'
#' @param client_id A client id from MK Online.
#' @param client_secret A client secret from MK Online.
mk_token <- function(client_id, client_secret){

  auth <- httr::authenticate(user = client_id, password = client_secret)

  httr::set_config(auth)

  cnt <- httr::POST(url = "https://auth.mkonline.com/oauth2/token",
                    httr::user_agent("https://github.com/krose/mkonline2"),
                    body = list(grant_type = "client_credentials"),
                    encode = "form",
                    verbose())

  cnt
}
