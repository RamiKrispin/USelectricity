api_key <- commandArgs(trailingOnly = TRUE)
source('./functions/data_update.R')
status <- FALSE
status <- update_data(api_key = api_key[1])

if(status){
  rmarkdown::render_site()
}