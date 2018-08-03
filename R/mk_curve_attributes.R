#' FUnction to get the curve attributes.
#'
#' @param curve_attr The name of the curve attribute you want. The
#'     current possibilities are: areas, categories, commodities
#'
#' @export
#'
mk_curve_attributes <- function(curve_attr, token = NULL){

  ######################################
  # Checks
  # - Check possible curve attr
  # - check number of attr
  # - check if token is supplied.
  ######################################

  poss_curve_attr <- c("areas", "categories", "commodities",
                       "curve_types", "data_types", "frequencies",
                       "functions", "sources", "stations", "storage_types",
                       "time_zones", "units")

  if(!curve_attr %in% poss_curve_attr){
    stop(paste0("The curve_attr '", curve_attr, "' is not one of the possible attributes."))
  }

  if(length(curve_attr) > 1){
    stop("You can only supply one curve_attr to the mk_curve_attributes function.")
  }

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
  url <- paste0("https://api.wattsight.com/api/", curve_attr)

  cnt <- httr::GET(url,
                   httr::user_agent("https://github.com/krose/mkonline2"),
                   httr::accept_json(),
                   httr::add_headers('Authorization' = paste("Bearer", token$access_token)))

  # Return an error if is one.
  if(httr::http_error(cnt)){
    stop(paste0("The attributes for ",
                curve_attr,
                " could not be retrieved. Error message: ",
                httr::http_status(cnt)$message))
  }

  # Extract the content of the request.
  cnt <- httr::content(x = cnt, as = "text", encoding = "UTF-8")
  cnt <- jsonlite::fromJSON(cnt)

  cnt
}
