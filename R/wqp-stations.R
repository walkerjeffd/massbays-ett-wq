# fetch data from WQP

targets_wqp_stations <- list(
  tar_target(wqp_stations_fetch, {
    dataRetrieval::whatWQPdata(
      bBox = eda_units_bbox,
      sampleMedia = "Water",
      characteristicName = wq_params$characteristic_name,
      startDate = start_date
    )
  }),
  tar_target(wqp_stations, {
    wqp_stations_fetch %>% 
      st_as_sf(coords = c("lon", "lat"), crs = 4326) %>%
      st_transform(crs = "EPSG:2249") %>% 
      clean_names() %>% 
      st_intersection(
        select(eda_units, eda_unit_name = NAME)
      ) %>%
      filter(!monitoring_location_type_name %in% wqp_filters$stations$monitoring_location_type_name)
  }),
  tar_target(wqp_stations_map_organization, {
    wqp_stations %>% 
      ggplot() +
      geom_sf(aes(color = fct_lump_n(organization_formal_name, n = 8))) +
      geom_sf(data = eda_units, fill = NA, alpha = 0.5) +
      scale_color_brewer("organization", palette = "Set1")
  }),
  tar_target(wqp_stations_map_type, {
    wqp_stations %>% 
      ggplot() +
      geom_sf(aes(color = fct_lump_n(monitoring_location_type_name, n = 8))) +
      geom_sf(data = eda_units, fill = NA, alpha = 0.5) +
      scale_color_brewer("location type", palette = "Set1")
  }),
  tar_target(wqp_stations_shp_file, {
    dir.create("out/wqp-stations", recursive = TRUE, showWarnings = FALSE)
    filename <- "out/wqp-stations/wqp-stations.shp"
    wqp_stations %>% 
      select(
        provider = provider_name,
        org_id = organization_identifier,
        stn_id = monitoring_location_identifier,
        name = monitoring_location_name,
        type = monitoring_location_type_name,
        url = site_url,
        eda_name = eda_unit_name
      ) %>% 
      st_write(filename, append = FALSE)
    filename
  }, format = "file")
)
