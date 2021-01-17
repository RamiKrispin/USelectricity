api_key <- commandArgs(trailingOnly = TRUE)
source('./functions/data_update.R')
update_data(api_key = api_key)