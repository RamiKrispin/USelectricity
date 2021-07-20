init_pull <- function(demand_id = "EBA.US48-ALL.D.H", 
                      generation_id = "EBA.US48-ALL.NG.H", 
                      generation_cat = 3390105,
                      api_key = Sys.getenv("eia_key")){
  
  
  # Error handling ----
  if(!is.character(demand_id)){
    stop("The demand_id argument is not valid")
  } else if(!is.character(generation_id)){
    stop("The generation_id argument is not valid")
  } else if(!is.character(api_key)){
    stop("The api_key argument is not valid")
  } else if(!is.numeric(generation_cat)){
    stop("The generation_cat argument is not valid")
  } 
  
  # Load functions ---- 
  source("./functions/eia_query.R")
  
  `%>%` <- magrittr::`%>%`
  
  msg <- function(message, n = TRUE){
    if(n){
      cat(paste("\033[0;92m", message,"\033[0m\n", sep = ""))
    } else{
      cat(paste("\033[0;92m", message,"\033[0m", sep = ""))
    }
  }
  
  # Pull demand data - hourly, UTC time
  # Units: megawatthours
  # Series ID: the demand_id argument
  
  msg("Pulling the demand data...")
  
  demand1 <- demand <- NULL
  
  tryCatch(
    demand1<- eia_query(api_key = api_key, series_id  = demand_id) %>%
      dplyr::mutate(type = "demand") %>%
      dplyr::arrange(date_time),
    
    error = function(c){
      base::message(paste("Error,", c, sep = " "))
    }
    
  )
  
  if(is.null(demand1)){
    stop("Could non pull the demand data")
  }
  
  
  
  start_time <- end_time <-  NULL
  start_time <- min(demand1$date_time)
  end_time <- max(demand1$date_time)
  
  demand <- data.frame(date_time = seq.POSIXt(from = start_time, to = end_time, by = "hour")) %>%
    dplyr::left_join(demand1,  by = "date_time")
  
  # Pull Generation data - hourly, UTC time
  # Units: megawatthours
  # Series ID: the demand_id argument
  msg("Pulling the generation data...")
  
  tryCatch(
    generation1 <- eia_query(api_key = api_key, series_id  = generation_id) %>%
      dplyr::mutate(type = "generation") %>%
      dplyr::arrange(date_time),
    
    error = function(c){
      base::message(paste("Error,", c, sep = " "))
    }
  )
  
  if(is.null(generation1)){
    stop("Could non pull the generation data")
  }
  
  
  start_time <- end_time <-  NULL
  start_time <- min(generation1$date_time)
  end_time <- max(generation1$date_time)
  
  generation <- data.frame(date_time = seq.POSIXt(from = start_time, to = end_time, by = "hour")) %>%
    dplyr::left_join(generation1,  by = "date_time")
  
  msg("Merging the demand and generation datasets")
  
  
  # Impute missing value for June 27, 2021
  # using the generation values as the demand
  if(is.na(demand[which(demand$type == "demand" & 
             demand$date_time == as.POSIXct("2021-06-27 00:00:00", tz = "UTC")), "series"])){
    
  d  <- demand %>% as.data.frame() %>%
    dplyr::filter(!(type == "demand" & 
                      date_time == as.POSIXct("2021-06-27 00:00:00", tz = "UTC"))) %>%
    dplyr::bind_rows(demand %>%  as.data.frame() %>%
                       dplyr::filter(type == "generation", 
                                     date_time >= as.Date("2021-06-27") & 
                                       date_time < as.Date("2021-06-28")) %>%
                       dplyr::mutate(type = "demand"))
  
  if(any(is.na(d$series))){
    stop("Some missing values")
  } else if(nrow(demand) > nrow(d)){
    stop("The number of rows of the imputated table is invalid")
  } 
  
  }
  
  
  
  elec_df <- dplyr::bind_rows(demand, generation) %>%
    tsibble::as_tsibble(key = type, index = date_time)
  
  
  save(elec_df, file = "./data/elec_df.rda")
  
  
  
  # Pulling the generation by source
  
  msg("Pulling the generation by energy source data...")
  
  gen_cat <- gen_df <-  NULL
  
  tryCatch(
    gen_cat <- eia_category(api_key = api_key, 
                            category_id = generation_cat) %>%
      dplyr::filter(f == "H"),
    error = function(c){
      base::message(paste("Error,", c, sep = " "))
    }
  )
  
  
  if(is.null(gen_cat)){
    stop("Could non pull the gen_cat data")
  }
  
  
  for(i in 1:nrow(gen_cat)){
    
    x1 <- regexpr(pattern = "Net generation from ", text = gen_cat$name[i])
    x2 <- regexpr(pattern = "for", text = gen_cat$name[i])
    gen_cat$type[i] <- substr(gen_cat$name[i], start = x1 + attr(x1,"match.length"), stop = x2 - 2)
  }
  
  save(gen_cat, file = "./data/gen_cat.rda")
  
  
  
  
  tryCatch(
    gen_df <- lapply(1:nrow(gen_cat), function(i){
      
      x1 <- regexpr(pattern = "Net generation from ", text = gen_cat$name[i])
      x2 <- regexpr(pattern = "for", text = gen_cat$name[i])
      gen_type <- substr(gen_cat$name[i], start = x1 + attr(x1,"match.length"), stop = x2 - 2)
      df <- eia_query(api_key = api_key, 
                      series_id = gen_cat$series_id[i], 
                      start = NULL, 
                      end = NULL, 
                      tz = "UTC") %>%
        dplyr::mutate(type = gen_type) %>%
        dplyr::select(date_time, value = series, type)
    }) %>% dplyr::bind_rows(),
    error = function(c){
      base::message(paste("Error,", c, sep = " "))
    }
  )
  
  if(is.null(gen_df)){
    stop("Could non pull the gen_df data")
  }
  
  
  save(gen_df, file = "./data/gen_df.rda")
  
  
  return(TRUE)
}
