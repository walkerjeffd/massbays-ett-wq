# generate and export files for ETT

targets_ett <- list(
  tar_target(ett_data_tn_computed, {
    x_wqp_data <- wqp_data %>% 
      filter(wq_param %in% c("NO23N", "TKN")) %>% 
      # exclude duplicate results (cannot determine which values to add)
      add_count(monitoring_location_identifier, activity_identifier, activity_start_date, activity_start_time_time, wq_param) %>% 
      filter(n == 1) %>%
      select(-n)
    x_wqp_data %>% 
      select(monitoring_location_identifier, activity_identifier, activity_start_date, activity_start_time_time, wq_param, ett_value, ett_units) %>% 
      pivot_wider(names_from = "wq_param", values_from = "ett_value") %>% 
      filter(!is.na(NO23N), !is.na(TKN)) %>% 
      mutate(ett_value = NO23N + TKN, wq_param = "TN") %>% 
      select(-NO23N, -TKN)
  }),
  tar_target(ett_data, {
    wqp_data %>% 
      filter(!wq_param %in% c("NO23N", "TKN")) %>% 
      bind_rows(ett_data_tn_computed) %>% 
      mutate(
        datetime_str = str_c(activity_start_date, " ", coalesce(activity_start_time_time, "00:00:00")),
        utc_offset = case_when(
          activity_start_time_time_zone_code == "AST" ~ -4,
          activity_start_time_time_zone_code == "EST" ~ -5,
          activity_start_time_time_zone_code == "EDT" ~ -4,
          TRUE ~ NA_real_
        ),
        datetime_parse = ymd_hms(datetime_str, tz = "UTC"),
        datetime_utc = if_else(
          is.na(utc_offset),
          with_tz(force_tz(datetime_parse, "US/Eastern"), "UTC"),
          datetime_parse - hours(utc_offset)
        ),
        datetime = with_tz(datetime_utc, tzone = "US/Eastern")
      ) %>% 
      transmute(
        station_id = monitoring_location_identifier,
        param = wq_param,
        units = ett_units,
        datetime,
        value = ett_value
      )
  }),
  tar_target(ett_stations_counts, {
    ett_data %>% 
      count(station_id, param) %>% 
      pivot_wider(names_from = "param", values_from = "n", values_fill = 0) %>% 
      nest_by(station_id, .key = "wq_counts")
  }),
  
  tar_target(ett_stations_geojson, {
    filename <- "out/ett/stations.json"
    if (file.exists(filename)) {
      log_warn("deleting existing stations file ({filename})")
      unlink(filename)
    }
    wqp_stations %>% 
      select(
        provider_name = provider_name,
        organization_identifier = organization_identifier,
        station_id = monitoring_location_identifier,
        station_name = monitoring_location_name,
        type = monitoring_location_type_name,
        url = site_url,
        eda_unit_name = eda_unit_name
      ) %>% 
      inner_join(
        ett_stations_counts %>% 
          mutate(wq_counts = as.character(toJSON(as.list(wq_counts), auto_unbox = TRUE))),
        by = "station_id"
      ) %>%
      st_transform(crs = "EPSG:4326") %>% 
      st_write(filename, driver = "GeoJSON", layer_options = c("ID_FIELD=station_id"), delete_dsn = TRUE)
    filename
  }, format = "file"),
  tar_target(ett_data_json, {
    if (dir.exists("out/ett/wq")) {
      log_warn("deleting existing data files (out/ett/wq/*.json)")
      unlink("out/ett/wq", recursive = TRUE)
    }
    
    dir.create("out/ett/wq", recursive = TRUE, showWarnings = FALSE)
    
    x <- ett_data %>% 
      mutate(datetime = format_ISO8601(datetime, usetz = TRUE)) %>% 
      nest_by(station_id, param, units, .key = "values") %>% 
      nest_by(station_id)
    
    for (i in 1:nrow(x)) {
      filename <- glue("out/ett/wq/{x$station_id[[i]]}.json")
      log_info("saving: {filename}")
      write_json(x$data[[i]], path = filename, pretty = FALSE)
    }
    
    glue("out/ett/wq/{x$station_id}.json")
  }, format = "file")
)
