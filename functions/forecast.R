
# Data ----
load("./data/us_elec.rda")

df <-us_elec %>%  dplyr::filter(type == "demand") %>%
  dplyr::select(date_time, y = series) %>%
  as.data.frame() %>%
  dplyr::arrange(date_time)

data <- df
y <- "y"
date_time <- "date_time"
lags <- c(1:24, 48, 72, 24 * 7)
h <- 72
glm_fc <- function(data, 
                   y, 
                   date_time, 
                   alpha, lambda, 
                   lags,
                   trend = TRUE,
                   seasonal = list(hour = TRUE,
                                   day = TRUE,
                                   wday = TRUE,
                                   month = TRUE,
                                   year = TRUE),
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
  
  
  h2o::h2o.init(port = 9001)
  
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
  
  
  
  
  
  
  
  
}
