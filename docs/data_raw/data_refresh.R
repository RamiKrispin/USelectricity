api_key <- commandArgs(trailingOnly = TRUE)
source('./functions/data_update.R')
source('./functions/forecast.R')
status <- FALSE
status <- update_data(api_key = api_key[1])

if(status){
  
  refresh_forecast()
  
  rmarkdown::render_site()
}