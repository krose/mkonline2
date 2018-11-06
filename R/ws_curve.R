

#' FUnction to get the curve attributes.
#'
#' @param query_list Named list with search params. If the get method is
#'     used the query_list must uniquely identify a curve.
#' @param method Method: get, search or access
#' @param token Token
#'
#' @export
#'
#' @examples
#' library(wattsight)
#'
#' ws_curve(query_list = list(query = "de"), method = "search")
#' ws_curve(query_list = list(id = 2), method = "get")
#' ws_curve(query_list = list(id = 2), method = "access")
#'
ws_curve <- function(query_list = NULL, method = "get", token = NULL){

  # If there is no token, get one
  if(is.null(token)){
    token <- ws_token()
  }

  #######################################
  # Get the data
  # - construct url
  # - make the get request
  # - Check for errors.
  # - extract the actual content
  ########################################

  # Construct the url
  url <- httr::parse_url("https://api.wattsight.com/api/curves")

  if(method == "get"){
    url$path <- paste0(url$path, "/get")
  } else if(method == "access"){
    url$path <- paste0(url$path, "/", query_list$id[1], "/access")
  }

  if(!is.null(query_list) & method != "access"){
    url$query <- query_list
  }

  url <- httr::build_url(url)

  cnt <- httr::GET(url,
                   httr::user_agent("https://github.com/krose/mkonline2"),
                   httr::add_headers('Authorization' = paste("Bearer", token$access_token)))

  # Return an error if is one.
  if(httr::http_error(cnt)){
    stop(paste0("Error message: ",
                httr::http_status(cnt)$message))
  }

  # Extract the content of the request.
  cnt <- httr::content(x = cnt, as = "text", encoding = "UTF-8")
  cnt <- jsonlite::fromJSON(cnt)

  cnt
}
