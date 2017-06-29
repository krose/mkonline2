

#' FUnction to get the curve attributes.
#'
#' @param token Token
#' @param query_list Named list with search params.
#'
#' @export
#'
#' @examples
#' library(mkonline2)
#'
#' mk_curve_search(query_list = list(query = "de"))
#'
mk_curve_search <- function(token = NULL, query_list = NULL){

  # If there is no token, get one
  if(is.null(token)){
    token <- mk_token()
  }

  #######################################
  # Get the data
  # - construct url
  # - make the get request
  # - Check for errors.
  # - extract the actual content
  ########################################

  # Construct the url
  url <- httr::parse_url("https://data.mkonline.io/api/curves")

  if(!is.null(query_list)){

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
