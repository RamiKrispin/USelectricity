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
  
  # Pull demand data - hourly, UTC time
  # Units: megawatthours
  # Series ID: the demand_id argument
  
  demand1<- eia_query(api_key = api_key, series_id  = demand_id) %>%
    dplyr::mutate(type = "demand") %>%
    dplyr::arrange(date_time)
  
  head(demand1)
  tail(demand1)
  
  
  start_time <- end_time <-  NULL
  start_time <- min(demand1$date_time)
  end_time <- max(demand1$date_time)
  
  demand <- data.frame(date_time = seq.POSIXt(from = start_time, to = end_time, by = "hour")) %>%
    dplyr::left_join(demand1,  by = "date_time")
  
  # Pull Generation data - hourly, UTC time
  # Units: megawatthours
  # Series ID: the demand_id argument
  
  generation1 <- eia_query(api_key = api_key, series_id  = generation_id) %>%
    dplyr::mutate(type = "generation") %>%
    dplyr::arrange(date_time)
  
  head(generation1)
  tail(generation1)
  
  table(is.na(generation1$series))
  
  start_time <- end_time <-  NULL
  start_time <- min(generation1$date_time)
  end_time <- max(generation1$date_time)
  
  generation <- data.frame(date_time = seq.POSIXt(from = start_time, to = end_time, by = "hour")) %>%
    dplyr::left_join(generation1,  by = "date_time")
  
  elec_df <- dplyr::bind_rows(demand, generation) %>%
    tsibble::as_tsibble(key = type, index = date_time)
  
  
  head(elec_df)
  tail(elec_df)
  
  save(elec_df, file = "./data/elec_df.rda")
  
  plotly::plot_ly(data = elec_df,
                  x = ~ date_time,
                  y = ~ series,
                  color = ~ type,
                  type = "scatter",
                  mode = "lines")
  
  
  # Pulling the generation by source
  gen_cat <- eia_category(api_key = api_key, 
                          category_id = generation_cat) %>%
    dplyr::filter(f == "H")
  
  
  gen_df <- lapply(1:nrow(gen_cat), function(i){
    
    x1 <- regexpr(pattern = "Net generation from ", text = gen_cat$name[i])
    x2 <- regexpr(pattern = "for", text = gen_cat$name[i])
    gen_type <- substr(gen_cat$name[i], start = x1 + attr(x1,"match.length"), stop = x2 - 2)
    df <- eia_query(api_key = api_key, 
                    series_id = gen_cat$series_id[i], 
                    start = NULL, 
                    end = NULL, 
                    tz = "UTC") %>%
      dplyr::mutate(type = gen_type)
  }) %>% dplyr::bind_rows()
  
  
  head(gen_df)
  
  plotly::plot_ly(data = gen_df,
                  x = ~ date_time, 
                  y = ~ series,
                  type = 'scatter', 
                  mode = 'none', 
                  stackgroup = 'one', 
                  fillcolor = ~ type) 
  
  
  save(gen_df, file = "./data/gen_df.rda")
  
  
  return(TRUE)
}