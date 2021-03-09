api_key <- commandArgs(trailingOnly = TRUE)
source('./functions/data_update.R')
source('./functions/forecast.R')
status <- FALSE
status_gen <- update_generation()
status_demand <- update_data(api_key = api_key[1])

if(status_gen | status_demand){
  
  refresh_forecast()
  
  rmarkdown::render_site()
}