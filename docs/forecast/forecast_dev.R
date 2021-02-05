# Forecasting model

# Example for running in parallel h2o
# https://stackoverflow.com/questions/43444333/parallel-processing-in-r-with-h2o
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

# Analysis ----
lags_plot <- lapply(c(1:24, 48, 72, 24 * 7), function(i){
  
df1 <- df %>% 
  dplyr::filter(year >= 2020) %>%
  dplyr::select(date_time, y) %>%
  
  dplyr::mutate(y_l = dplyr::lag(y, n = i))

p <- plotly::plot_ly(data = df1,
                     x = ~ y_l,
                     y = ~y)

})

# plotly::subplot(lags_plot, nrows = 4)
# 
# ts.obj <- ts(df1$y, start = c(5), frequency = 24)
# 
# TSstudio::ts_cor(ts.obj = ts.obj, lag.max = 24 * 8, seasonal_lags = 7 * 24)
# 
# TSstudio::ts_decompose(ts.obj = ts.obj)




lags <- c(1:24, 48, 72, 24 * 7)
for(i in lags){
  df[[paste("lag", i, sep = "_")]] <- dplyr::lag(df$y, i)
}


# Backtesting setting ----
h <- 24 * 3
p <- 4
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

# Shallow search
alpha_opts = seq(0,1, 0.02)
lambda_opts = c(0.000005, 0.00001, 0.000015,0.00005, 0.0001, 0.0005, 0.001, 0.01, 0.1)

hyper_parameters = list(alpha = alpha_opts, 
                        lambda = lambda_opts)

grid <- h2o::h2o.grid("glm", hyper_params = hyper_parameters,
                      y = "y",
                      x = c("index", "year", "month", "day", 
                            "wday", "hour",
                            paste("lag", lags, sep = "_")),
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
                      lambda_search = FALSE,
                      parallelism = 1)

grid@summary_table


grid_models <- lapply(grid@model_ids, function(model_id){ 
  model = h2o::h2o.getModel(model_id) })

md <- grid_models[[1]]

md@parameters$solver
alpha <- md@parameters$alpha
lambda <- md@parameters$lambda




fc <- lapply(1:p, function(i){
  
  
  sample_out <- h + space * (i-1) 
  
  train <- df %>% dplyr::slice_head(n = nrow(df) - sample_out)
  test_df <- df %>% dplyr::slice_tail(n = sample_out) %>%
    dplyr::slice_head(n = h)
  
  train_h <- train %>% 
    dplyr::select(-date_time) %>%
    h2o::as.h2o()
  
  
  md <- h2o::h2o.glm(y = "y",
                     x = c("index", "year", "month", "day", "wday", "hour",
                           paste("lag", lags, sep = "_")),
                     alpha = alpha,
                     lambda = lambda,
                     seed = 12345,
                     training_frame = train_h,
                     family = "gaussian",
                     standardize = TRUE,
                     nfolds = 5,
                     compute_p_values = FALSE,
                     lambda_search = FALSE)
  
  
  
  
  for(r in 2:nrow(test_df)){
    for(l in lags){
      if(r > l){
        test_df[r, paste("lag", l, sep = "_")] <- NA
      }
    }
  }
  
  test_df$yhat <- 0
  
  test_dfh <- test_df %>% 
    dplyr::select(-date_time) %>%
    h2o::as.h2o()
  
  for(r in 1:nrow(test_dfh)){

    for(l in lags){

      if(l < r){
        test_dfh[r, paste("lag", l, sep = "_")] <-  test_dfh$yhat[r-l]
      }
    }
    
    test_dfh[r, "yhat"] <- (h2o::h2o.predict(md, test_dfh[r,]))[1,1]
  }
  
  output <- as.data.frame(test_dfh)
  output$par <- i
  print(paste("Partition", i, sep = " "))
  return(output)
  
}) %>% 
  dplyr::bind_rows()



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

plotly::subplot(res_plot, nrows = 4)
hist(fc$y - fc$yhat, breaks = 100)

alpha 
lambda
save(alpha, lambda, lags, file = "./forecast/model_setting.RData")



