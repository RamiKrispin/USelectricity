#' Updating the Data
#' @description The function load the existing data and pull additional data, if available
update_data <- function(api_key = Sys.getenv("eia_key"),
                        demand_id = "EBA.US48-ALL.D.H", 
                        generation_id = "EBA.US48-ALL.NG.H"){
  
  `%>%` <- magrittr::`%>%`
  
  msg <- function(message, n = TRUE){
    if(n){
      cat(paste("\033[0;92m", message,"\033[0m\n", sep = ""))
    } else{
      cat(paste("\033[0;92m", message,"\033[0m", sep = ""))
    }
  }
  
  
  source("./functions/eia_query.R")
  
  
  msg("Checking for updates...")
  
  load("./data/elec_df.rda")
  
  
  
  demand_df <- elec_df %>% dplyr::filter(type == "demand")
  refresh_flag <- FALSE
  
  time_str_d <- as.character(max(demand_df$date_time) + lubridate::hours(1))
  
  if(lubridate::hour(max(demand_df$date_time)) != 23) {
    start_d <- paste(gsub("-", "",substr(time_str_d, 1, 10)), "T",
                     substr(time_str_d, 12, 13), "Z", sep = "")
  } else{
    start_d <- paste(gsub("-", "",substr(time_str_d, 1, 10)), "T",
                     "00", "Z", sep = "")
  }
  
  
  demand_new <- NULL
  
  tryCatch(
    demand_new <- eia_query(api_key = api_key, 
                            series_id = demand_id, 
                            start = start_d),
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
    elec_df <- elec_df %>% 
      dplyr::bind_rows(demand_new) %>%
      dplyr::arrange(date_time)
    
    refresh_flag <- TRUE
  }
  
  # Updating the generation data
  generation_df <- elec_df %>% 
    dplyr::filter(type == "generation")
  
  time_str_g <- as.character(max(generation_df$date_time) + lubridate::hours(1))
  
  if(lubridate::hour(max(generation_df$date_time)) != 23) {
    start_g <- paste(gsub("-", "",substr(time_str_g, 1, 10)), "T",
                     substr(time_str_g, 12, 13), "Z", sep = "")
  } else{
    start_g <- paste(gsub("-", "",substr(time_str_g, 1, 10)), "T",
                     "00", "Z", sep = "")
  }
  
  generation_new <- NULL
  tryCatch(
    generation_new <- eia_query(api_key = api_key, 
                                series_id = generation_id, 
                                start = start_g),
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
    elec_df <- elec_df %>% 
      dplyr::bind_rows(generation_new) %>%
      dplyr::arrange(date_time)
    
    refresh_flag <- TRUE
  }
  
  
  if(refresh_flag){
    save(elec_df, file = "./data/elec_df.rda")
    msg("Update the demand and generation dataset...")
    return(TRUE)
  } else{
    msg("Updates are not available for the demand and generation datasets...")
    return(FALSE)
  }
}

update_generation <- function(api_key = Sys.getenv("eia_key")){
  
  `%>%` <- magrittr::`%>%`
  
  msg <- function(message, n = TRUE){
    if(n){
      cat(paste("\033[0;92m", message,"\033[0m\n", sep = ""))
    } else{
      cat(paste("\033[0;92m", message,"\033[0m", sep = ""))
    }
  }
  
  
  source("./functions/eia_query.R")
  
  msg("Checking for updates...")
  
  load("./data/gen_cat.rda")
  load("./data/gen_df.rda")
  
  
  gen_cat <- gen_cat %>% 
    dplyr::left_join(
      gen_df %>% 
        dplyr::group_by(type) %>%
        dplyr::summarise(start = max(date_time) + lubridate::hours(1)),
      by = "type")
  
  update_flag <- FALSE
  
  for(i in 1:nrow(gen_cat)){
    gen_new <- start <- NULL
    
    msg(paste("Trying to pull", gen_cat$type[i], "data", sep = " "))
    
    if(lubridate::hour(gen_cat$start[i]) != 23) {
      start <- paste(gsub("-", "",substr(gen_cat$start[i], 1, 10)), "T",
                       substr(gen_cat$start[i], 12, 13), "Z", sep = "")
    } else{
      start <- paste(gsub("-", "",substr(gen_cat$start[i], 1, 10)), "T",
                       "00", "Z", sep = "")
    }
    
    
    tryCatch(
      
      gen_new <- eia_query(api_key = api_key, 
                           series_id = gen_cat$series_id[i], 
                           start = start),
      error = function(c){
        base::message(paste("Error,", c, sep = " "))
      }
    )
    
    if(!is.null(gen_new)){
      if(min(gen_new$date_time) !=  gen_cat$start[i]){
        stop("The timestamp of the new data is not aligned with existing one")
      } else if(nrow(gen_new) == 0){
        stop("The generation table is emptly")
      } else{
        
        gen_df <- gen_df %>% 
          dplyr::bind_rows(gen_new %>% 
                             dplyr::select(date_time, value = series) %>%
                             dplyr::mutate(type = gen_cat$type[i]))
        
        msg(paste("Updated the", 
                  gen_cat$type[i], 
                  "date", 
                  sep = " "))
        
        update_flag <- TRUE
      }
      
      
      
    }
    
  }
  
  if(update_flag){
    msg("Saving changes...")
    
    save(gen_df, file = "./data/gen_df.rda")
    return(TRUE)
  } else {
    msg("No updates avaiable....")
    return(FALSE)
  }
}
