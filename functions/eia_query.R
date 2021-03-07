#' Pulling Data from EIA API
#' @description The function enables to pull data from EIA API based on series ID
#' @details The function send a GET request to the EIA API, parse the returned object from JSON to a clean data.frame object. More details:
#' 
#' - API documentation - https://www.eia.gov/opendata/commands.php
#' - The API required a key, register here https://www.eia.gov/opendata/register.php
#' - The function is using jq to parse the JSON - https://stedolan.github.io/jq/
#' 
#' Note: if pulling hourly data with UTC time zone setting, 
#' a zulu time format (ISO 8601) should be use for the start and end argument.
#' @param api_key EIA API key
#' @param series_id Series ID
#' @param start Optional, start time in a yyyymmddThhZ format (e.g., for 12 pm at January 1st, 2021 use 20210101T12Z). 
#' See note about the time zone on the function details
#' @param end Optional, end time in a yyyymmddThhZ format (e.g., for 12 pm at January 1st, 2021 use 20210101T12Z). 
#' See note about the time zone on the function details
#' @param tz The series index time zone, by default set to UTC
#' @return a data.frame object


eia_query <- function(api_key, 
                      series_id, 
                      start = NULL, 
                      end = NULL, 
                      tz = "UTC"){
  `%>%` <- magrittr::`%>%`
  
  url <- base::paste("http://api.eia.gov/series/?",
                     "api_key=", api_key,
                     "&series_id=", series_id,
                     "&out=json", sep = "")
  
  if(!is.null(start)){
    url <- base::paste(url, "&start=", start, sep = "")
  }
  
  if(!is.null(end)){
    url <- base::paste(url, "&end=", end, sep = "")
  }
  
  command <- base::paste("curl", " '",url, "' | jq -r '.series[].data[] | @tsv'", sep = "")
  
  output <- utils::read.table(text = system(command = command, intern = TRUE), sep = "\t") %>%
    stats::setNames(c("timestamp", "series")) %>%
    dplyr::mutate(date_time = lubridate::ymd_h(timestamp, tz = tz)) %>%
    dplyr::select(date_time, series) %>%
    dplyr::arrange(date_time)
  
  return(output)
}



#' Pulling Category Info from EIA API
#' @description The function enables to pull category data from EIA API based on category ID
#' @details The function send a GET request to the EIA API, parse the returned object from JSON to a clean data.frame object. More details:
#' 
#' - API documentation - https://www.eia.gov/opendata/commands.php
#' - The API required a key, register here https://www.eia.gov/opendata/register.php
#' - The function is using jq to parse the JSON - https://stedolan.github.io/jq/
#' 
#' Note: if pulling hourly data with UTC time zone setting, 
#' a zulu time format (ISO 8601) should be use for the start and end argument.
#' @param api_key EIA API key
#' @param category_id Category ID

eia_category <- function(api_key, category_id = NULL){
  url <- get <- output <- NULL
  url <- paste("http://api.eia.gov/category/?api_key=", api_key, sep = "")
  if(!base::is.null(category_id)){
    url <- base::paste(url, "&category_id=", category_id, "&", sep = "")
  }
  
  get <- httr::GET(url = url)
  output <- jsonlite::fromJSON(httr::content(get, as = "text"))
  return(output)
}