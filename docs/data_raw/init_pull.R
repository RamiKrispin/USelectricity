#----------- Pulling the demand for electricity -----------
source("./functions/eia_query.R")
`%>%` <- magrittr::`%>%`
api_key <- Sys.getenv("eia_key")

# Demand for United States Lower 48 (region), hourly - UTC time
# Units: megawatthours
# Series ID: EBA.US48-ALL.D.H

us_demand1<- eia_query(api_key = api_key, series_id  = "EBA.US48-ALL.D.H") %>%
  dplyr::mutate(type = "demand") %>%
  dplyr::arrange(date_time)

head(us_demand1)
tail(us_demand1)


start_time <- end_time <-  NULL
start_time <- min(us_demand1$date_time)
end_time <- max(us_demand1$date_time)

us_demand <- data.frame(date_time = seq.POSIXt(from = start_time, to = end_time, by = "hour")) %>%
  dplyr::left_join(us_demand1,  by = "date_time")

# Net generation for United States Lower 48 (region), hourly - UTC time
# Units: megawatthours
# Series ID: EBA.US48-ALL.NG.H


us_gen1 <- eia_query(api_key = api_key, series_id  = "EBA.US48-ALL.NG.H") %>%
  dplyr::mutate(type = "generation") %>%
  dplyr::arrange(date_time)

head(us_gen1)
tail(us_gen1)

table(is.na(us_gen1$series))

start_time <- end_time <-  NULL
start_time <- min(us_gen1$date_time)
end_time <- max(us_gen1$date_time)

us_gen <- data.frame(date_time = seq.POSIXt(from = start_time, to = end_time, by = "hour")) %>%
  dplyr::left_join(us_gen1,  by = "date_time")

us_elec <- dplyr::bind_rows(us_demand, us_gen) %>%
  tsibble::as_tsibble(key = type, index = date_time)


head(us_elec)
tail(us_elec)

save(us_elec, file = "./data/us_elec.rda")

plotly::plot_ly(data = us_elec,
                x = ~ date_time,
                y = ~ series,
                color = ~ type,
                type = "scatter",
                mode = "lines")
