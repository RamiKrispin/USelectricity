# Creating error dist
`%>%` <- magrittr::`%>%`
dates_list <- seq.Date(from = as.Date("2021-01-05"), 
                       length.out = 45, 
                       by = "day")

load("./data/us_elec.rda")
source('./functions/forecast.R')
load("./forecast/model_setting.RData")
df <-us_elec %>%  
  dplyr::filter(type == "demand") %>%
  dplyr::select(date_time, y = series) %>%
  as.data.frame() %>%
  dplyr::mutate(time = lubridate::with_tz(time = date_time, tzone = "US/Eastern")) %>%
  dplyr::arrange(time) %>%
  dplyr::select(time, y) 

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