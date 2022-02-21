library(targets)

# packages
options(tidyverse.quiet = TRUE)
tar_option_set(packages = c("tidyverse", "lubridate", "janitor", "glue", "units", "patchwork", "sf", "logger"))

# load packages into session
if (interactive()) {
  sapply(tar_option_get("packages"), require, character.only = TRUE)
}

# load all functions
invisible(sapply(list.files("R", pattern = ".R", full.names = TRUE), source))

wqp_fetch_data <- function (characteristic_name, station_ids, batch_size = 50, ...) {
  if (length(station_ids) > batch_size) {
    current_stations <- station_ids[1:batch_size]
    next_stations <- station_ids[(batch_size + 1):length(station_ids)]
  } else {
    current_stations <- station_ids
    next_stations <- c()
  }
  log_info("fetch: {characteristic_name} (current={length(current_stations)}, remaining={length(next_stations)})")
  x <- dataRetrieval::readWQPdata(
      siteid = current_stations,
      characteristicName = characteristic_name,
      sampleMedia = "Water",
      ...
    ) %>% 
    mutate(across(everything(), as.character))

  if (length(next_stations) > 0) {
    Sys.sleep(2)
    return(bind_rows(x, wqp_fetch_data(characteristic_name, next_stations, batch_size = batch_size, ...)))
  }
  x
}

list(
  tar_target(embayments_file, "data/embayments_watersheds/embayment_watersheds.shp", format = "file"),
  tar_target(embayments, st_read(embayments_file) %>% st_transform(crs = "EPSG:2249")),
  tar_target(embayments_bbox, {
    embayments %>%
      st_transform(crs = 4326) %>%
      st_bbox()
  }),
  tar_target(embayments_map, {
    embayments %>% 
      ggplot() +
      geom_sf(aes(fill = Estuarine)) +
      scale_fill_brewer(type = "qual", palette = 6)
  }),
  
  tar_target(wqp_characteristic_names, {
    c(
      "Phosphorus",
      "Nitrogen",
      "Temperature, water",
      "Chlorophyll a",
      "Salinity",
      "pH",
      "Dissolved oxygen (DO)",
      "Enterococcus"
    )
  }),
  tar_target(wqp_stations_bbox_fetch_nitrogen, {
    tibble(
      characteristic_name = c("Kjeldahl nitrogen", "Ammonia and ammonium", "Inorganic nitrogen (nitrate and nitrite)", "Nitrate + Nitrite", "Nitrogen")
    ) %>% 
      mutate(
        data = map(characteristic_name, function (characteristic_name) {
          log_info("characteristic_name: {characteristic_name}")
          x <- dataRetrieval::whatWQPdata(
            bBox = embayments_bbox,
            characteristicName = characteristic_name,
            sampleMedia = "Water"
          )
          log_info("sleeping...")
          Sys.sleep(2)
          x
        })
      ) %>% 
      unnest(data)
  }),
  tar_target(wqp_stations_bbox_fetch_nitrogen_map, {
    wqp_stations_bbox_fetch_nitrogen %>% 
      mutate(characteristic_name = fct_inorder(characteristic_name)) %>% 
      st_as_sf(coords = c("lon", "lat"), crs = 4326) %>%
      st_transform(crs = "EPSG:2249") %>% 
      clean_names() %>% 
      st_filter(embayments) %>% 
      ggplot() +
      geom_sf(aes(color = organization_formal_name)) +
      geom_sf(data = embayments, fill = NA) +
      facet_wrap(vars(characteristic_name), nrow = 1)
  }),
  tar_target(wqp_stations_bbox_fetch, {
    tibble(
      characteristic_name = wqp_characteristic_names
    ) %>% 
      mutate(
        data = map(characteristic_name, function (characteristic_name) {
          log_info("characteristic_name: {characteristic_name}")
          x <- dataRetrieval::whatWQPdata(
            bBox = embayments_bbox,
            characteristicName = characteristic_name,
            sampleMedia = "Water"
          )
          log_info("sleeping...")
          Sys.sleep(2)
          x
        })
      ) %>% 
      unnest(data)
  }),
  tar_target(wqp_stations_bbox_sf, {
    wqp_stations_bbox_fetch %>% 
      st_as_sf(coords = c("lon", "lat"), crs = 4326) %>%
      st_transform(crs = "EPSG:2249") %>% 
      clean_names()
  }),
  tar_target(wqp_stations_bbox_map_provider, {
    wqp_stations_bbox_sf %>% 
      ggplot() +
      geom_sf(aes(color = provider_name)) +
      geom_sf(data = embayments, fill = NA)
  }),
  tar_target(wqp_stations_sf, {
    wqp_stations_bbox_sf %>% 
      st_filter(embayments)
  }),
  tar_target(wqp_stations_map_characteristic_type, {
    wqp_stations_sf %>% 
      ggplot() +
      geom_sf(aes(color = monitoring_location_type_name)) +
      geom_sf(data = embayments, fill = NA, alpha = 0.5) +
      facet_wrap(vars(characteristic_name), nrow = 2)
  }),
  tar_target(wqp_stations_table_characteristic, {
    wqp_stations_sf %>% 
      st_drop_geometry() %>% 
      tabyl(monitoring_location_type_name, characteristic_name)
  }),
  tar_target(wqp_stations_map_characteristic_organization, {
    wqp_stations_sf %>% 
      ggplot() +
      geom_sf(aes(color = organization_formal_name)) +
      geom_sf(data = embayments, fill = NA, alpha = 0.5) +
      facet_wrap(vars(characteristic_name), nrow = 2)
  }),
  tar_target(wqp_stations_map_type, {
    wqp_stations_sf %>% 
      select(-characteristic_name) %>% 
      distinct() %>% 
      ggplot() +
      geom_sf(aes(color = monitoring_location_type_name)) +
      geom_sf(data = embayments, fill = NA)
  }),
  tar_target(wqp_stations_hist_characteristic, {
    wqp_stations_sf %>% 
      st_drop_geometry() %>% 
      count(characteristic_name) %>% 
      ggplot(aes(fct_reorder(characteristic_name, n), n)) +
      geom_col() +
      coord_flip() +
      labs(x = "characteristic_name", y = "# stations")
  }),
  tar_target(wqp_stations_hist_characteristic_results, {
    wqp_stations_sf %>% 
      st_drop_geometry() %>% 
      group_by(characteristic_name) %>% 
      summarise(n = sum(result_count)) %>% 
      ggplot(aes(fct_reorder(characteristic_name, n), n)) +
      geom_col() +
      coord_flip() +
      labs(x = "characteristic_name", y = "# samples")
  }),
  
  tar_target(wqp_stations_tile_characteristic_results, {
    wqp_stations_sf %>% 
      st_drop_geometry() %>% 
      group_by(organization_formal_name, characteristic_name) %>% 
      summarise(n = sum(result_count), .groups = "drop") %>% 
      ggplot(aes(fct_reorder(characteristic_name, n), organization_formal_name)) +
      geom_tile(aes(fill = n)) +
      scale_fill_viridis_c(trans = "log10") +
      labs(x = "characteristic_name", y = "organization_formal_name", fill = "# samples") +
      theme(
        axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5)
      )
  }),
  
  tar_target(wqp_stations_hist_type, {
    wqp_stations_sf %>% 
      st_drop_geometry() %>% 
      count(monitoring_location_type_name) %>% 
      ggplot(aes(fct_reorder(monitoring_location_type_name, n), n)) +
      geom_col() +
      coord_flip() +
      labs(x = "monitoring_location_type_name")
  }),
  tar_target(wqp_stations_hist_organization, {
    wqp_stations_sf %>% 
      st_drop_geometry() %>% 
      count(organization_formal_name) %>% 
      ggplot(aes(fct_reorder(organization_formal_name, n), n)) +
      geom_col() +
      coord_flip() +
      labs(x = "organization_formal_name")
  }),
  tar_target(wqp_stations_geojson_file, {
    counts <- wqp_stations_sf %>% 
      st_drop_geometry() %>% 
      select(characteristic_name, monitoring_location_identifier, result_count) %>% 
      left_join(
        tribble(
          ~characteristic_name, ~characteristic_id,
          "Phosphorus", "TP",
          "Nitrogen", "TN",
          "Temperature, water", "TEMP",
          "Chlorophyll a", "CHLA",
          "Salinity", "SALINITY",
          "pH", "PH",
          "Dissolved oxygen (DO)", "DO",
          "Enterococcus", "ENT"
        ),
        by = "characteristic_name"
      ) %>% 
      select(-characteristic_name) %>% 
      pivot_wider(names_from = "characteristic_id", values_from = "result_count", values_fill = 0)
    stations <- wqp_stations_sf %>% 
      select(-characteristic_name) %>% 
      distinct() %>% 
      left_join(counts, by = "monitoring_location_identifier")
    filename <- "out/wqp-stations.geojson"
    if (file.exists(filename)) {
      log_info("deleting: {filename}")
      unlink(filename)
    }
    log_info("saving: {filename}")
    st_write(stations, filename)
    filename
  }),
  tar_target(wqp_stations_shp_file, {
    counts <- wqp_stations_sf %>% 
      st_drop_geometry() %>% 
      select(characteristic_name, monitoring_location_identifier, result_count) %>% 
      left_join(
        tribble(
          ~characteristic_name, ~characteristic_id,
          "Phosphorus", "TP",
          "Nitrogen", "TN",
          "Temperature, water", "TEMP",
          "Chlorophyll a", "CHLA",
          "Salinity", "SALINITY",
          "pH", "PH",
          "Dissolved oxygen (DO)", "DO",
          "Enterococcus", "ENT"
        ),
        by = "characteristic_name"
      ) %>% 
      select(-characteristic_name) %>% 
      pivot_wider(names_from = "characteristic_id", values_from = "result_count", values_fill = 0)
    stations <- wqp_stations_sf %>% 
      select(-characteristic_name) %>% 
      distinct() %>% 
      left_join(counts, by = "monitoring_location_identifier")
    filename <- "out/wqp-stations.shp"
    # if (file.exists(filename)) {
    #   log_info("deleting: {filename}")
    #   unlink(filename)
    # }
    log_info("saving: {filename}")
    stations %>% 
      select(
        PROVIDER = provider_name,
        ORG_ID   = organization_identifier,
        ORG_NAME = organization_formal_name,
        SITE_ID  = monitoring_location_identifier,
        SITE_NM  = monitoring_location_name,
        TYPE     = monitoring_location_type_name,
        URL      = site_url,
        TP:ENT
      ) %>% 
      st_write(filename, append = FALSE)
    filename
  }),
  
  tar_target(ett_stations_exclude_types, {
    c(
      "Atmosphere",
      "Facility",
      "Facility Industrial",
      "Facility Municipal Sewage (POTW)",
      "Facility: Outfall",
      "Facility: Wastewater land application",
      "Spring",
      "Well",
      "Well: Multiple wells",
      "Well: Test hole not completed as a well"
    )
  }),
  tar_target(ett_stations, {
    wqp_stations_sf %>%
      filter(!monitoring_location_type_name %in% ett_stations_exclude_types)
  }),
  tar_target(ett_wq_params, {
    tribble(
      ~wq_param, ~characteristic_name,
      "TP", "Phosphorus",
      "TN", "Nitrogen",
      "TEMP", "Temperature, water",
      "DO", "Dissolved oxygen (DO)",
      "PH", "pH"
    )
  }),
  tar_target(ett_wq_params_fraction_exclude, {
    tribble(
      ~characteristic_name, ~result_sample_fraction_text,
      "Phosphorus", "Dissolved",
      "Nitrogen", "Dissolved"
    )
  }),
  tar_target(ett_wq_fetch, {
    ett_stations %>%
      st_drop_geometry() %>%
      nest_by(characteristic_name, .key = "stations") %>% 
      semi_join(ett_wq_params, by = "characteristic_name") %>%
      rowwise() %>% 
      mutate(
        station_ids = list(unique(stations$monitoring_location_identifier)),
        wqp = list(wqp_fetch_data(characteristic_name, station_ids, batch_size = 50))
      )
  }),
  tar_target(ett_wq_data_summary, {
    x <- ett_wq_fetch %>% 
      select(wqp) %>% 
      unnest(wqp) %>% 
      clean_names()

    t_sample_fraction <- x %>% 
      count(characteristic_name, result_sample_fraction_text)
    t_characteristic_units <- x %>% 
      count(characteristic_name, result_measure_measure_unit_code)
    t_status_identifier <- x %>% 
      count(characteristic_name, result_status_identifier)
    t_value_type <- x %>% 
      count(characteristic_name, result_value_type_name)
    t_usgs_pcode <- x %>% 
      count(characteristic_name, usgsp_code)
    t_activity_media_subdivision <- x %>% 
      count(characteristic_name, activity_media_subdivision_name)
    t_value_nonnumeric <- x %>% 
      mutate(parsed_value = parse_number(result_measure_value)) %>% 
      filter(is.na(parsed_value), !is.na(result_measure_value)) %>% 
      count(characteristic_name, result_measure_value)
    
    p_hist <- x %>% 
      bind_rows(mutate(x, characteristic_name = "(ALL)")) %>% 
      count(monitoring_location_identifier, characteristic_name) %>% 
      ggplot(aes(n)) +
      geom_histogram() +
      scale_x_log10() +
      facet_wrap(vars(characteristic_name), scales = "free") +
      labs(x = "# samples per station")
    
    
    p_cdf <- x %>% 
      bind_rows(mutate(x, characteristic_name = "(ALL)")) %>% 
      count(monitoring_location_identifier, characteristic_name) %>% 
      ggplot(aes(n)) +
      stat_ecdf() +
      scale_x_log10() +
      scale_y_continuous(labels = scales::percent, breaks = scales::pretty_breaks()) +
      facet_wrap(vars(characteristic_name), scales = "free") +
      labs(x = "# samples per station", y = "non-exceedance freq")
    
    p_period <- x %>% 
      ggplot(aes(as_date(activity_start_date), characteristic_name)) +
      geom_jitter(size = 0.2) +
      labs(x = "activity_start_date")
    
    p_dup_date <- x %>% 
      nest_by(monitoring_location_identifier, characteristic_name, activity_start_date, activity_start_time_time) %>% 
      mutate(n = nrow(data)) %>% 
      arrange(desc(n)) %>%
      head() %>% 
      unnest(data) %>% 
      ggplot(aes(ymd(activity_start_date), parse_number(result_measure_value))) +
      geom_point(aes()) +
      labs(x = "activity_start_date", y = "result_measure_value") +
      facet_wrap(vars(characteristic_name, monitoring_location_identifier), labeller = label_both, scales = "free")
    
    p_depth <- x %>% 
      ggplot(aes(as_date(activity_start_date), parse_number(activity_depth_height_measure_measure_value))) +
      geom_point() +
      facet_wrap(vars(characteristic_name)) +
      labs(
        x = "activity_start_date",
        y = "activity_depth_height_measure_measure_value",
        title = "WQP Data | Sample Depths",
        caption = glue("updated: {now()}")
      )
    p_value <- x %>% 
      ggplot(aes(as_date(activity_start_date), parse_number(result_measure_value))) +
      geom_point() +
      facet_wrap(vars(characteristic_name, result_measure_measure_unit_code), scales = "free") +
      labs(
        x = "activity_start_date",
        y = "result_measure_value",
        title = "WQP Data | Sample Values",
        caption = glue("updated: {now()}")
      )
    
    list(
      data = x,
      plot = list(
        depth = p_depth,
        value = p_value,
        dup_date = p_dup_date,
        hist = p_hist,
        period = p_period
      ),
      table = list(
        sample_fraction = t_sample_fraction,
        characteristic_units = t_characteristic_units,
        status_identifier = t_status_identifier,
        value_type = t_value_type,
        usgs_pcode = t_usgs_pcode,
        activity_media_subdivision = t_activity_media_subdivision,
        value_nonnumeric = t_value_nonnumeric
      )
    )
  }),
  tar_target(ett_wq_meta, {
    ett_wq_fetch %>%
      tibble() %>% 
      clean_names() %>%
      inner_join(ett_wq_params, by = c("characteristic_name", "result_sample_fraction_text"))
  }),
  tar_target(ett_wq, {
    ett_wq_meta %>%
      transmute(
        station_id = monitoring_location_identifier,
        wq_param,
        date = activity_start_date,
        datetime = ymd_hms(str_c(as.character(date), activity_start_time_time, sep = " ")),
        value = result_measure_value,
        detection = result_detection_condition_text,
        units = result_measure_measure_unit_code
      )
  }),
  tar_target(ett_wq_boxplot, {
    ett_wq %>% 
      ggplot(aes(station_id, value)) +
      geom_boxplot() +
      coord_flip() +
      facet_wrap(vars(units), scales = "free_x")
  })
)
