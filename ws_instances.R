#' Function to Find instances for the specified curve
#'
#' @param id Curve id. Integer.
#' @param issue_date_from Limit search to one or more specific issue dates. Character.
#' @param issue_date_to Limit search to one or more specific issue dates. Character.
#' @param output_time_zone Change curve time_zone after filtering and frequency change. Character
#' @param with_data Include data for the returned instances. Boolean
#' @param only_accessible Only return instances you have access to. Boolean
#' @param token Token
#'
#' @export
#'
#' @examples
#'
#'

ws_instances <- function(id, issue_date_from = NULL, issue_date_to = NULL,
                         with_data = TRUE , output_time_zone = NULL, only_accessible = TRUE, token = NULL){

  params <- list("issue_date_from" = issue_date_from,
                 "issue_date_to" = issue_date_to,
                 "with_data" = with_data,
                 "output_time_zone" = output_time_zone,
                 "only_accessible" = only_accessible)

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
  url <- httr::parse_url("https://api.wattsight.com/api/instances")

  url$path <- paste0(url$path, "/", id)
  url$query <- params
  #' #https://api.wattsight.com/api/instances/1225?issue_date_from=2018-11-01T00%3A00&issue_date_to=2018-11-07T00%3A00&with_data=true&only_accessible=true

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
  if(with_data != TRUE){
    return(cnt)
  } else {
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

  return(cnt)
  }
}