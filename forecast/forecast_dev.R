# Forecasting model
# Functions ----
`%>%` <- magrittr::`%>%`
# Data ----
load("./data/us_elec.rda")

df <-us_elec %>%  dplyr::filter(type == "demand") %>%
  dplyr::select(date_time, y = series) %>%
  as.data.frame() %>%
  dplyr::arrange(date_time) %>%
  dplyr::mutate(index = 1:dplyr::n(),
                year = lubridate::year(date_time),
                month = factor(lubridate::month(date_time, label = TRUE), ordered = FALSE),
                day = lubridate::yday(date_time),
                wday = factor(lubridate::wday(date_time, label = TRUE), ordered = FALSE),
                hour = factor(lubridate::hour(date_time), order = FALSE))
for(i in 1:24){
  df[[paste("lag", i, sep = "_")]] <- dplyr::lag(df$y, i)
}

# Backtesting setting ----
h <- 24 * 3
p <- 10
space <- 24
sample_out <- h + space * (p-1)

train <- df %>% dplyr::slice_head(n = nrow(df) - sample_out)
test <- df %>% dplyr::slice_tail(n = sample_out)
# Training a model ----
h2o::h2o.init(max_mem_size = "24G",
              nthreads = 12)

train_h <- train %>% 
  dplyr::select(-date_time) %>%
  h2o::as.h2o()

alpha_opts = seq(0,1, 0.02)
lambda_opts = c(0.000005, 0.00001, 0.000015,0.00005, 0.0001, 0.0005, 0.001, 0.0011, 0.0012,0.01, 0.1, 0.5, 1, 2)

hyper_parameters = list(alpha = alpha_opts, 
                        lambda = lambda_opts)

grid <- h2o::h2o.grid("glm", hyper_params = hyper_parameters,
                      y = "y",
                      x = c("index", "year", "month", "day", 
                            "wday", "hour",
                            paste("lag", 1:24, sep = "_")),
                      training_frame = train_h,
                      family = "gaussian",
                      standardize= TRUE,
                      nfolds = 8,
                      cold_start = TRUE,
                      early_stopping = TRUE,
                      stopping_rounds = 5,
                      seed = 1234,
                      stopping_metric = "RMSE",
                      stopping_tolerance = 0.001,
                      compute_p_values = FALSE,
                      lambda_search = FALSE)

grid@summary_table
grid_models <- lapply(grid@model_ids, function(model_id){ 
  model = h2o::h2o.getModel(model_id) })

md <- grid_models[[1]]

# Testing with backtesting
fc <- lapply(1:p, function(i){
  
  start <- 1 + (i -1) * space  
  end <- start + h - 1
  
  test_df <- test[start:end,]
  
  
  
  for(r in 2:nrow(test_df)){
    
    if(r <= 24){
      for(c in 1:(r-1)){
        test_df[r, paste("lag", c, sep = "_")] <- NA
      }
    } else {
      for(c in 1:24){
        test_df[r, paste("lag", c, sep = "_")] <- NA
      }
    }
  }
  
  test_df$yhat <- 0
  
  test_dfh <- test_df %>% 
    dplyr::select(-date_time) %>%
    h2o::as.h2o()
  
  for(l in 1:nrow(test_df)){
    if(l > 1 && l <= 24){
      for(c in 1:(l-1)){
        test_dfh[l, paste("lag", c, sep = "_")] <-  test_dfh$yhat[l-c]
      }
    } else if(l > 24){
      for(c in 1:24){
        test_dfh[l, paste("lag", c, sep = "_")] <-  test_dfh$yhat[l-c]
      }
    }
    test_dfh[l, "yhat"] <- (h2o::h2o.predict(md, test_dfh[l,]))[1,1]
    
  }
  
  output <- as.data.frame(test_dfh)
  output$par <- i
  
  
  return(output)
}) %>% dplyr::bind_rows()


head(fc)




res_plot <- lapply(1:p, function(i){
  
  df1 <- fc %>% dplyr::filter(par == i) %>%
    dplyr::arrange(index) %>%
    dplyr::mutate(index1 = 1:dplyr::n()) %>%
    dplyr::arrange(index1)
  
  
  print(mean(abs(df1$y - df1$yhat) / df1$y))
  p <- plotly::plot_ly(x = df1$index1,
                       y = df1$y - df1$yhat,
                       type = "scatter",
                       mode = "marker",
                       name = paste("Partition", i, sep = " "))
  return(p)
  
  
  
})

plotly::subplot(res_plot, nrows = 5)
hist(fc$y - fc$yhat, breaks = 100)


