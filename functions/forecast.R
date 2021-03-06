
glm_fc <- function(data, 
                   y, 
                   date_time, 
                   alpha, lambda, 
                   lags,
                   trend = TRUE,
                   seasonal = list(hour = TRUE,
                                   yday = TRUE,
                                   wday = TRUE,
                                   month = TRUE,
                                   year = TRUE),
                   port = 9001,
                   nthreads = 1,
                   max_mem_size = NULL,
                   h){
  
  `%>%` <- magrittr::`%>%`
  labels <- NULL
  # Error handling
  if(!is.data.frame(data)){
    stop("The input object is not a data.frame")
  } else if(!is.numeric(lags)){
    stop("The trend argument is not numeric object")
  } else if(!is.character(y)){
    stop("The y argument is not character object")
  } else if(!is.numeric(alpha)){
    stop("The alpha argument is not numeric object")
  } else if(!is.numeric(lambda)){
    stop("The lambda argument is not numeric object")
  } else if(!is.logical(trend)){
    stop("The trend argument is not logical object")
  } else if(!is.numeric(h)){
    stop("The h argument is not numeric object")
  } 
  
  
  data <- data %>% dplyr::arrange(date_time)
  
  if(!is.null(seasonal)){
    if(!is.list(seasonal)){
      stop("The seasonal argument is not list object")
    }
  }
  
  start <- max(data[[date_time]]) + lubridate::hours(1)
  future_df <- data.frame(temp = seq.POSIXt(from = start, length.out = h, by = "hour"),
                          stringsAsFactors = FALSE)
  names(future_df) <- date_time
  
  
  if(trend){
    labels <- c(labels, "index")
    data$index <- 1:nrow(data)
    future_df$index <- (nrow(data) + 1):(nrow(data) + h)
  }
  
  if(!is.null(seasonal)){
    for(i in names(seasonal)){
      if(seasonal[[i]])
        labels <- c(labels, i)
      
      data[[i]] <- eval(parse(text = paste("lubridate::", i, "(data$",
                                           date_time,
                                           ")", sep = "")))
      
      future_df[[i]] <- eval(parse(text = paste("lubridate::", i, "(future_df$",
                                                date_time,
                                                ")", sep = "")))
    }
  }
  
  
  
  
  
  for(i in lags){
    labels <- c(labels, paste("lag", i, sep = "_"))
    data[[paste("lag", i, sep = "_")]] <- dplyr::lag(data[, which(names(data) == y)], i)
    if(i <= h){
      future_df[[paste("lag", i, sep = "_")]] <- c(tail(data[[y]], i), rep(NA, h - i))
    } else if(i > h){
      future_df[[paste("lag", i, sep = "_")]] <- head(tail(data[[y]], i), h)
    }
  }
  
  
  
  if(any(names(seasonal) %in% c("hour", "yday", "wday", "month", "day"))){
    future_df[[y]] <- NA
    future_df$label <- "future"
    data$label <- "actual"
    temp <- rbind(data, future_df)
    
    for(i in names(seasonal)){
      if(i  %in% c("hour", "yday", "wday", "month", "day")){
        temp[[i]] <- factor(temp[[i]], ordered = FALSE)
      }
    }
    
    data <- temp %>% 
      dplyr::filter(label == "actual") %>% 
      dplyr::select(-label)
    future_df <- temp %>% 
      dplyr::filter(label == "future") %>% 
      dplyr::select(-label)
  }
  
  h2o::h2o.init(port = port,
                nthreads = nthreads,
                max_mem_size = max_mem_size)
  
  data_h <- data[, -which(names(data) == date_time)] %>% 
    as.data.frame() %>%
    h2o::as.h2o()
  
  md <- h2o::h2o.glm(y = "y",
                     x = labels,
                     alpha = alpha,
                     lambda = lambda,
                     seed = 12345,
                     training_frame = data_h,
                     family = "gaussian",
                     standardize = TRUE,
                     nfolds = 5,
                     compute_p_values = FALSE,
                     lambda_search = FALSE)
  
  
  # Creating the forecast
  future_df$yhat <- 0
  
  fc_df <- future_df[, -which(names(future_df) == date_time)] %>% 
    as.data.frame() %>%
    h2o::as.h2o()
  
  yhat <- numeric(nrow(fc_df))
  
  if(!is.null(lags)){
    for(r in 1:nrow(fc_df)){
      
      for(l in lags){
        
        if(l < r){
          fc_df[r, paste("lag", l, sep = "_")] <-  yhat[r-l]
        }
      }
      
      yhat[r] <- (h2o::h2o.predict(md, fc_df[r,]))[1,1]
    }
  }
  
  
  
  future_df$yhat <- yhat
  
  
  h2o::h2o.shutdown(prompt = FALSE)
  output <- list(forecast = future_df,
                 coefficients = md@model$coefficients_table)
  return(output)
}




refresh_forecast <- function(){
  `%>%` <- magrittr::`%>%`
  load("./data/forecast.rda")
  load("./data/residuals.rda")
  load("./data/us_elec.rda")
  load("./forecast/model_setting.RData")
  
  df <- us_elec %>%  
    dplyr::filter(type == "demand") %>%
    dplyr::select(date_time, y = series) %>%
    as.data.frame() %>%
    dplyr::mutate(time = lubridate::with_tz(time = date_time, tzone = "US/Eastern")) %>%
    dplyr::arrange(time) %>%
    dplyr::select(time, y) 
  
  start <- max(fc_df$time) + lubridate::hours(1)
  
  if(max(df$time) > max(fc_df$time)){
    
    
    res_temp <- fc_df %>% 
      dplyr::left_join(df, by = "time")
    
    if(any(is.na(res_temp$y))){
      stop("Failed to merge the current forecast with actuals, some data points are missing...")
    } else {
      res_df <- res_df %>% dplyr::bind_rows(
      res_temp %>% 
        dplyr::mutate(res = y - yhat) %>%
        dplyr::select(time, yhat, index, y , res))
      
    }
    
    
    
    cat("Refresh the forecast...\n")
   
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
                 port = 9001,
                 max_mem_size = NULL,
                 h = 72)
    
    res_summary <- res_df %>%
      dplyr::group_by(index) %>%
      dplyr::summarise(mean = mean(res),
                       sd = sd(res), 
                       .groups = "drop") %>%
      dplyr::mutate(up = mean + qnorm(p = 0.975) * sd, 
                    low = mean - qnorm(p = 0.975) * sd)
    
    
    fc_df <- fc$forecast %>% 
      dplyr::select(time, yhat, index_temp = index) %>%
      dplyr::mutate(index = index_temp - min(index_temp) + 1,
                    label = as.Date(substr(as.character(min(time)), 
                                           start = 1, 
                                           stop = 10)),
                    type = "latest") %>%
      dplyr::select(-index_temp) %>%
      dplyr::left_join(res_summary, by = "index") %>%
      dplyr::mutate(upper = yhat + up,
                    lower = yhat + low)
    
    
    save(fc_df, file = "./data/forecast.rda")
    save(res_df, file = "./data/residuals.rda")
  }
  
  cat("Done...\n")
  return(TRUE)
  
}