targets_wqp_data <- list(
  tar_target(wqp_data_fetch, {
    wqp_fetch_data(
      wqp_stations$monitoring_location_identifier, 
      batch_size = 50, 
      characteristicName = wq_params$characteristic_name, 
      startDate = start_date,
      dataProfile = "resultPhysChem" # needed to get ActivityRelativeDepthName
    )
  }),
  tar_target(wqp_data_all, {
    wqp_data_fetch %>% 
      as_tibble() %>% 
      clean_names() %>% 
      left_join(
        wq_params,
        by = "characteristic_name"
      ) %>% 
      remove_empty("cols") %>% 
      mutate(
        result_measure_nondetect = result_measure_value %in% c("Not Detected", "*Non-detect"),
        result_measure_censored = result_measure_value %in% c("## (Censored)"),
        result_measure_value = if_else(
          result_measure_nondetect | result_measure_censored,
          NA_character_, result_measure_value
        ),
        across(ends_with("_date"), ymd),
        across(ends_with("_value"), parse_number),
        across(ends_with("_code"), factor),
        across(
          c(
            provider_name, organization_identifier, organization_formal_name, 
            activity_relative_depth_name, 
            result_value_type_name, result_detection_condition_text, 
            detection_quantitation_limit_type_name,
            wq_param
          ),
          factor
        ),
        activity_depth_m = case_when(
          activity_depth_height_measure_measure_value < 0 ~ NA_real_,
          activity_depth_height_measure_measure_unit_code %in% c("ft", "feet") ~ convert_units(activity_depth_height_measure_measure_value, "ft", "m"),
          activity_depth_height_measure_measure_unit_code %in% c("m") ~ activity_depth_height_measure_measure_value,
          TRUE ~ NA_real_
        )
      ) %>% 
      left_join(
        wqp_stations %>% 
          st_drop_geometry() %>% 
          select(monitoring_location_identifier, monitoring_location_type_name),
        by = "monitoring_location_identifier"
      )%>%
      left_join(
        wqp_filters$samples$result_sample_fraction_text %>% 
          mutate(exclude_result_sample_fraction_text = TRUE),
        by = c("wq_param", "result_sample_fraction_text")
      ) %>% 
      mutate(
        # fill non-detects with quantitation limit
        wqp_value = coalesce(result_measure_value, detection_quantitation_limit_measure_measure_value),
        wqp_units = case_when(
          !is.na(result_measure_value) ~ as.character(result_measure_measure_unit_code),
          !is.na(wqp_value) ~ as.character(detection_quantitation_limit_measure_measure_unit_code),
          TRUE ~ NA_character_
        ),
      ) %>% 
      left_join(
        wqp_filters$samples$wqp_units %>% 
          mutate(wqp_units_exclude = TRUE),
        by = c("wq_param", "wqp_units")
      ) %>% 
      mutate(
        wqp_filters = case_when(
          is.na(wqp_value) ~ "missing_value",
          wq_param != "ENT" & is.na(result_measure_measure_unit_code) ~ "missing_units",
          wqp_units_exclude ~ "excluded_units",
          exclude_result_sample_fraction_text ~ "excluded_result_sample_fraction_text",
          result_status_identifier %in% wqp_filters$samples$result_status_identifier ~ "excluded_result_status_identifier",
          activity_type_code %in% wqp_filters$samples$activity_type_code ~ "excluded_activity_type_code",
          activity_relative_depth_name %in% wqp_filters$samples$activity_relative_depth_name ~ "excluded_activity_relative_depth_name",
          activity_depth_m > wqp_filters$samples$max_activity_depth_m ~ "exceeds_max_activity_depth_m",
          TRUE ~ NA_character_
        )
      ) %>% 
      select(-result_identifier) %>%
      distinct()  # remove duplicates
  }),
  tar_target(wqp_data_excluded, filter(wqp_data_all, !is.na(wqp_filters))),
  tar_target(wqp_data_excluded_table, tabyl(wqp_data_excluded, wqp_filters, wq_param)),
  tar_target(wqp_data, {
    x <- wqp_data_all %>% 
      filter(is.na(wqp_filters)) %>% 
      select(-wqp_filters) %>% 
      left_join(
        unit_conversion,
        by = c("wq_param", "wqp_units")
      ) %>%
      mutate(
        ett_value = convert_slope * (wqp_value + convert_base)
      )
    
    stopifnot(all(!is.na(x$ett_value)))
    
    x
  }),
  
  tar_target(wqp_data_plot_organization, {
    wqp_data %>% 
      ggplot(aes(organization_identifier, ett_value)) +
      geom_boxplot() +
      coord_flip() +
      facet_wrap(vars(wq_param, ett_units), scales = "free_x")
  })
)
