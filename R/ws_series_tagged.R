#' FUnction to get the curve attributes.
#'
#' @param id Curve id. Integer.
#' @param token Token
#'
#' @export
#'
#' @examples
#' library(mkonline2)
#'
ws_tagged_tags <- function(id, token = NULL){

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
  url <- httr::parse_url("https://api.wattsight.com/api/series/tagged")

  url$path <- paste0(url$path, "/", id, "/tags")

  url <- httr::build_url(url)

  cnt <- httr::GET(url,
                   httr::user_agent("https://github.com/krose/mkonline2"),
                   httr::accept_json(),
                   httr::add_headers('Authorization' = paste("Bearer", token$access_token)))

  # Check for errors.
  if(httr::http_error(cnt)){
    stop(paste0("Error message: ",
                httr::http_status(cnt)$message))
  }

  # Extract the content of the request.
  cnt <- httr::content(x = cnt, as = "text", encoding = "UTF-8")
  cnt <- jsonlite::fromJSON(cnt)

  cnt
}


#' FUnction to get the curve attributes.
#'
#' @param id Curve id. Integer.
#' @param tag Time series tag. Array [string].
#' @param from Values from date (inclusive). Character.
#' @param to Values to date (exclusive). Character.
#' @param time_zone Change curve time_zone before filtering and frequency change. Character.
#' @param ws_filter Filter out parts of the time series. Array[string].
#' @param ws_function The aggregate/split function to use when changing frequency. Character.
#' @param frequency The required frequency of the output. Character.
#' @param output_time_zone Change curve time_zone after filtering and frequency change.
#' @param token Token
#'
#' @export
#'
#' @examples
#' library(mkonline2)
#'
ws_tagged <- function(id, tag = NULL, from = NULL, to = NULL,
                             time_zone = NULL, ws_filter = NULL, ws_function = NULL,
                             frequency = NULL, output_time_zone = NULL, token = NULL){

  params <- list("tag" = tag,
                 "from" = from,
                 "to" = to,
                 "time_zone" = time_zone,
                 "filter" = ws_filter,
                 "function" = ws_function,
                 "frequency" = frequency,
                 "output_time_zone" = output_time_zone)

  params[sapply(params, is.null)] <- NULL

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
  url <- httr::parse_url("https://api.wattsight.com/api/series/tagged")

  url$path <- paste0(url$path, "/", id)
  url$query <- params

  url <- httr::build_url(url)

  cnt <- httr::GET(url,
                   httr::user_agent("https://github.com/krose/mkonline2"),
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
  cnt$points <- lapply(seq_along(cnt$points),
                       function(x){

                         ws_s <- cnt$points[[x]]

                         ws_s <- as.data.frame(ws_s)
                         names(ws_s) <- c("dt", "values")

                         ws_s$dt <- as.POSIXct.numeric(x = ws_s$dt / 1000, tz = "GMT", origin = "1970-01-01")

                         attr(ws_s, "frequency") <-  cnt$frequency[x]
                         attr(ws_s, "time_zone") <-  cnt$time_zone[x]
                         attr(ws_s, "id") <-  cnt$id[x]
                         attr(ws_s, "name") <-  cnt$name[x]
                         attr(ws_s, "tag") <-  cnt$tag[x]

                         ws_s
                       })

  cnt
}
