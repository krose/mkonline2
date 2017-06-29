

#' FUnction to get the curve attributes.
#'
#' @param curve_id Curve id. Integer.
#' @param token Token
#'
#' @export
#'
#' @examples
#' library(mkonline2)
#'
#' mk_series(curve_id = 2)
#'
mk_series <- function(curve_id, token = NULL){

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
  url <- httr::parse_url("https://data.mkonline.io/api/series")

  url$path <- paste0(url$path, "/:", curve_id)

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
