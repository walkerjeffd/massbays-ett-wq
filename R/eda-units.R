# EDA units (polygons)

targets_eda_units <- list(
  tar_target(eda_units_file, "data/EDA_UNITS_REVISED/EDA_UNITS_REVISED.shp", format = "file"),
  tar_target(eda_units, st_read(eda_units_file) %>% st_transform(crs = "EPSG:2249")),
  tar_target(eda_units_bbox, {
    eda_units %>%
      st_transform(crs = 4326) %>%
      st_bbox()
  }),
  tar_target(eda_units_map, {
    eda_units %>% 
      ggplot() +
      geom_sf(aes(fill = Estuarine)) +
      scale_fill_brewer(type = "qual", palette = 6)
  })
)