convert_units <- function(x, from, to) {
  drop_units(set_units(set_units(x, from, mode = "standard"), to, mode = "standard"))
}


wqp_fetch_data <- function (station_ids, batch_size = 50, ...) {
  if (length(station_ids) > batch_size) {
    current_stations <- station_ids[1:batch_size]
    next_stations <- station_ids[(batch_size + 1):length(station_ids)]
  } else {
    current_stations <- station_ids
    next_stations <- c()
  }
  log_info("fetch: {length(current_stations)} stations (remaining={length(next_stations)})")
  x <- dataRetrieval::readWQPdata(
    siteid = current_stations,
    sampleMedia = "Water",
    tz = "America/New_York",
    ...
  ) %>% 
    mutate(across(everything(), as.character))
  
  if (length(next_stations) > 0) {
    Sys.sleep(2)
    return(bind_rows(x, wqp_fetch_data(next_stations, batch_size = batch_size, ...)))
  }
  x
}
