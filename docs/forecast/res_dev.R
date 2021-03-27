# Function for calculating the model residuals and coefficients distribution

load("./data/elec_df.rda")
source('./functions/forecast.R')
load("./forecast/model_setting.RData")


df <- elec_df %>%  
  dplyr::filter(type == "demand") %>%
  dplyr::select(date_time, y = series) %>%
  as.data.frame() %>%
  dplyr::mutate(time = lubridate::with_tz(time = date_time, tzone = "US/Eastern")) %>%
  dplyr::arrange(time) %>%
  dplyr::select(time, y) 


class(df)
head(df)
max(df$time)
input <- df

get_dist <- function(input, start, alpha, lambda, cores = parallel::detectCores()){
  
  # Functions
  `%>%` <- magrittr::`%>%`
  
  # Error handling
  if(!is.data.frame(input) || ncol(input) != 2 || 
     !all(names(input) %in% c("time", "y")) || nrow(input) == 0){
    stop("The input object is not valid")
  } else if(!all(class(input$time) %in% class(start))){
    stop("The class of the input timestamp is different form the one of the start argument")
  } else if(!is.numeric(alpha)){
    stop("The alpha argument is not numeric")
  } else if(!is.numeric(lambda)){
    stop("The lambda argument is not numeric")
  }
  
  fc_sim <- parallel::mclapply(seq_along(start), 
                               mc.cores = cores,
                               mc.preschedule = FALSE,
                               mc.cleanup = TRUE,
                               mc.silent = FALSE,
                               function(i){
                                 port <- 9000 + i * 3
                                 x <- system(command = paste("netstat -taln | grep", port, sep = " "), intern = TRUE)
                                 if(length(x) != 0){
                                   cat("\033[0;93mport", port, "is not available\033[0m\n")
                                   
                                   port <- 9000 + i * 3 + 100 
                                 }
                                 
                                 fc <- NULL
                                 c <- 3 
                                 while(c > 0){
                                   tryCatch(
                                     fc <- glm_fc(data = df %>%
                                                    dplyr::filter(time < start[i]), 
                                                  y = "y", 
                                                  date_time = "time", 
                                                  alpha = alpha, 
                                                  lambda = lambda, 
                                                  lags = lags,
                                                  trend = TRUE,
                                                  seasonal = list(hour = TRUE,
                                                                  yday = TRUE,
                                                                  wday = TRUE,
                                                                  month = TRUE,
                                                                  year = TRUE),
                                                  port = port,
                                                  max_mem_size = "1G",
                                                  h = 72),
                                     error = function(c){
                                       base::message(paste("Error,", c, sep = " "))
                                     }
                                   )
                                   
                                   if(class(fc) != "try-error"){
                                     c <- 0
                                   } else {
                                     c <- c - 1
                                   }
                                   
                                   
                                 }
                                 
                                 
                                 if(is.null(fc) || class(fc) != "list"){
                                   fc <- start[i]
                                 }
                                 return(fc)
                               })
  
  
  cat("\033[0;93mTrying to connect to postgres\033[0m\n")
  
  for(i in 1:length(fc_sim)){
    if(!is.list(fc_sim[[i]])){
      print(i)
    }
  }
  
  
  
  res_df <- lapply(1:length(fc_sim), function(i){
    print(i)
    fc <- fc_sim[[i]]$forecast %>% dplyr::select(time, yhat, index) %>%
      dplyr::mutate(index = index - min(index) + 1) %>% 
      dplyr::left_join(df, by = "time") %>%
      dplyr::mutate(res = y - yhat,
                    label = as.Date(min(time)))
    
    
    return(fc)
  }) %>% dplyr::bind_rows()
  
  
  coef_df <- lapply(fc_sim, function(i){
    
    label <- as.Date(min(i$forecast$time))
    
    coef <- i$coefficients %>% 
      as.data.frame() %>%
      dplyr::mutate(label = label)
    
    
    return(coef)
  }) %>% dplyr::bind_rows()
  
  output <- list(forecast = res_df, coefficients = coef_df)
  
  return(output)
  
}

start <- seq.POSIXt(from = as.POSIXct("2021-02-01 01:00:00", tz = "US/Eastern"), 
                    by = "24 hours", 
                    length.out = 45)
dist <- get_dist(input = input, start = start, alpha = alpha, lambda = lambda)
save(dist, file = "./data/dist.rda")



dates_list <- seq.Date(from = as.Date("2021-01-05"), 
                       length.out = 45, 
                       by = "day")




fc_sim <- parallel::mclapply(seq_along(dates_list), 
                             mc.cores = 12,
                             mc.preschedule = FALSE,
                             mc.cleanup = TRUE,
                             mc.silent = FALSE,
                             function(i){
                               
                               
                               start <- lubridate::ymd_hms(paste(dates_list[i],
                                                                 "01:00:00", 
                                                                 sep = " "), 
                                                           tz = "US/Eastern")
                               port <- 9000 + i * 3
                               
                               fc <- glm_fc(data = df %>%
                                              dplyr::filter(time < start), 
                                            y = "y", 
                                            date_time = "time", 
                                            alpha = alpha, 
                                            lambda = lambda, 
                                            lags = lags,
                                            trend = TRUE,
                                            seasonal = list(hour = TRUE,
                                                            yday = TRUE,
                                                            wday = TRUE,
                                                            month = TRUE,
                                                            year = TRUE),
                                            port = port,
                                            max_mem_size = "1G",
                                            h = 72)
                               return(fc)
                             })



res_df <- lapply(fc_sim, function(i){
  
  fc <- i$forecast %>% dplyr::select(time, yhat, index) %>%
    dplyr::mutate(index = index - min(index) + 1) %>% 
    dplyr::left_join(df, by = "time") %>%
    dplyr::mutate(res = y - yhat)
  
  
  return(fc)
}) %>% dplyr::bind_rows()


p <- lapply(c(1:12, 69:72), function(i){
  
  df1 <- res_df %>% dplyr::filter(index == i)
  d <- density(df1$res)
  p <- plotly::plot_ly(x = ~d$x, y = ~d$y, 
                       type = 'scatter', 
                       mode = 'lines', 
                       fill = 'tozeroy')
  return(p)
})


plotly::subplot(p, nrows = 4)

save(res_df, file = "./data/residuals.rda")


res_metric <- res_df %>% 
  dplyr::group_by(index) %>%
  dplyr::summarise(mean = mean(res),
                   sd = sd(res))

head(res_metric)  
tail(res_metric)