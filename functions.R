#' Convert units
#'
#' @param x numeric vector of measurements (e.g., concentrations) in `from` units
#' @param from units to convert from
#' @param to units to convert to
#'
#' @return numeric vector of measurements (e.g., concentrations) in `to` units
#'
#' @examples
#' convert_units(c(150, 250), "ug/L", "mg/L") # returns c(0.15, 0.25)
convert_units <- function(x, from, to) {
  units::drop_units(units::set_units(units::set_units(x, from, mode = "standard"), to, mode = "standard"))
}

#' Fetch samples from Water Quality Portal
#' 
#' Recursively fetches samples from multiple stations in batches
#'
#' @param station_ids character vector of WQP station IDs
#' @param batch_size number of stations to fetch in each batch
#' @param ... additional parameters passed to dataRetrieval::readWQPdata() such
#'            as characteristicName, startDate, dataProfile
#'            (see ?dataRetrieval::readWQPdata)
#'
#' @return data frame of sample data returned from WQP 
#'
#' @examples
#' wqp_fetch_samples(
#'   station_ids = c("NARS_WQX-NCA_MA-10162", "11NPSWRD_WQX-CACO_ROUND_E"),
#'   characteristicName = c("Phosphorus", "Total Phosphorus, mixed forms"),
#'   startDate = "1990-01-01",
#'   dataProfile = "resultPhysChem"
#' )
wqp_fetch_samples <- function (station_ids, batch_size = 50, ...) {
  if (length(station_ids) > batch_size) {
    current_stations <- station_ids[1:batch_size]
    next_stations <- station_ids[(batch_size + 1):length(station_ids)]
  } else {
    current_stations <- station_ids
    next_stations <- c()
  }
  logger::log_info("wqp_fetch_samples: {length(current_stations)} stations ({length(next_stations)} remaining)")
  x <- dataRetrieval::readWQPdata(
    siteid = current_stations,
    sampleMedia = "Water",
    tz = "America/New_York",
    ...
  ) %>% 
    mutate(across(everything(), as.character))
  
  if (length(next_stations) > 0) {
    Sys.sleep(2)
    return(bind_rows(x, wqp_fetch_samples(next_stations, batch_size = batch_size, ...)))
  }
  x
}
