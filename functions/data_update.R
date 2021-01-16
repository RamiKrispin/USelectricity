#' Updating the Data
#' @description The function load the existing data and pull additional data, if available
update_data <- function(){
  source("./functions/eia_query.R")
  load("./data/us_elec.rda")
  `%>%` <- magrittr::`%>%`
  api_key <- Sys.getenv("eia_key")
  demand_df <- us_elec %>% dplyr::filter(type == "demand")
  refresh_flag <- FALSE
  time_str_d <- as.character(max(demand_df$date_time) + lubridate::hours(1))
  start_d <- paste(gsub("-", "",substr(time_str_d, 1, 10)), "T",
                   substr(time_str_d, 12, 13), "Z", sep = "")
  
  
  series_id_d  <- "EBA.US48-ALL.D.H"
  
  demand_new <- NULL
  tryCatch(
    demand_new <- eia_query(api_key = api_key, series_id = series_id_d, start = start_d),
    error = function(c){
      base::message(paste("Error,", c, sep = " "))
    }
  )
  
  if(is.null(demand_new)){
    cat("Demand - new data is not available...\n")
  } else if(max(demand_df$date_time) + lubridate::hours(1) != min(demand_new$date_time)){
    stop("The new data timestamp is missmatch the current data")
  } else{
    
    demand_new$type <- "demand"
    us_elec <- us_elec %>% 
      dplyr::bind_rows(demand_new) %>%
      dplyr::arrange(date_time)
    
    refresh_flag <- TRUE
  }
  
  # Updating the generation data
  generation_df <- us_elec %>% 
    dplyr::filter(type == "generation")
  
  time_str_g <- as.character(max(generation_df$date_time) + lubridate::hours(1))
  start_g <- paste(gsub("-", "",substr(time_str_g, 1, 10)), "T",
                   substr(time_str_g, 12, 13), "Z", sep = "")
  
  
  series_id_g  <- "EBA.US48-ALL.NG.H"
  
  generation_new <- NULL
  tryCatch(
    generation_new <- eia_query(api_key = api_key, series_id = series_id_g, start = start_g),
    error = function(c){
      base::message(paste("Error,", c, sep = " "))
    }
  )
  
  if(is.null(generation_new)){
    cat("Generation - new data is not available...\n")
  } else if(max(generation_df$date_time) + lubridate::hours(1) != min(generation_new$date_time)){
    stop("The new data timestamp is missmatch the current data")
  } else{
    
    generation_new$type <- "generation"
    us_elec <- us_elec %>% 
      dplyr::bind_rows(generation_new) %>%
      dplyr::arrange(date_time)
    
    refresh_flag <- TRUE
  }
  
  
  if(refresh_flag){
  save(us_elec, file = "./data/us_elec.rda")
    cat("Done...\n")
    return(TRUE)
  } else{
    cat("Updates are not available...\n")
    return(FALSE)
  }
}
