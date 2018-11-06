

#' FUnction to get the curve attributes.
#'
#' @param id Curve id. Integer.
#' @param from character.
#' @param to Character.
#' @param time_zone Character.
#' @param ws_filter Character.
#' @param ws_function Character.
#' @param frequency Character.
#' @param output_time_zone Character.
#' @param token Token
#'
#' @export
#'
#' @examples
#' library(wattsight2)
#'
#' ws_series(curve_id = 2)
#'
ws_series <- function(id, from = NULL, to = NULL, time_zone = NULL,
                      ws_filter = NULL, ws_function = NULL, frequency = NULL,
                      output_time_zone = NULL, token = NULL){

  params <- list(from = from,
                 to = to,
                 time_zone = time_zone,
                 ws_filter = ws_filter,
                 ws_function = ws_function,
                 frequency = frequency,
                 output_time_zone = output_time_zone)

  params[sapply(params, is.null)] <- NULL


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
  url <- httr::parse_url("https://api.wattsight.com/api/series")

  url$path <- paste0(url$path, "/", id)
  url$query <- params

  url <- httr::build_url(url)

  cnt <- httr::GET(url,
                   httr::user_agent("https://github.com/krose/wattsight2"),
                   httr::accept_json(),
                   httr::add_headers('Authorization' = paste("Bearer", token$access_token)))

  # Return an error if is one.
  if(httr::http_error(cnt)){
    stop(paste0("Error message: ",
                httr::http_status(cnt)$message))
  }

  # Extract the content of the request.
  cnt <- httr::content(x = cnt, as = "text", encoding = "UTF-8")
  cnt <- jsonlite::fromJSON(cnt)

  # convert to df and make output easier to work with.
  cnt$points <- as.data.frame(cnt$points)
  names(cnt$points) <- c("dt", "values")

  cnt$points$dt <- as.POSIXct.numeric(x = cnt$points$dt / 1000, tz = "GMT", origin = "1970-01-01")

  ws_s <- cnt$points

  attr(ws_s, "frequency") <-  cnt$frequency
  attr(ws_s, "time_zone") <-  cnt$time_zone
  attr(ws_s, "id") <-  cnt$id
  attr(ws_s, "name") <-  cnt$name

  ws_s
}
