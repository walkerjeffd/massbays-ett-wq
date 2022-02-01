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
  })
)

# 