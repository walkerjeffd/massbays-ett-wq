# fetch, process, and export water quality stations and samples for ETT 

# install dependency packages
# only need to run the following line once 
# install.packages(c("tidyverse", "janitor", "logger", "units", "sf", "dataRetrieval"))

library(tidyverse)
library(lubridate)
library(janitor)
library(sf)

# load helper functions
source("functions.R")

# parameters and reference tables -----------------------------------------

# start date for fetching WQ data
start_date <- "1980-01-01"

# list of options for filtering WQP stations and samples
wqp_filters <- list(
  stations = list(
    monitoring_location_type_name = c(
      "Atmosphere",
      "Facility",
      "Facility Industrial",
      "Facility Municipal Sewage (POTW)",
      "Facility: Outfall",
      "Facility: Wastewater land application",
      "Land",
      "Pipe, Unspecified Source",
      "Spring",
      "Well",
      "Well: Multiple wells",
      "Well: Test hole not completed as a well"
    )
  ),
  samples = list(
    result_sample_fraction_text = tribble(
      ~wq_param, ~result_sample_fraction_text,
      "TP",      "Dissolved",
      "TKN",     "Dissolved",
      "TN",      "Dissolved"
    ),
    wqp_units = tribble(
      ~wq_param, ~wqp_units,
      "DO",      "%",
      "SAL",     "mg/mL @25C",
      "TURB",    "NTRU",
      "TURB",    "FNU",
      "TURB",    "FTU",
      "TURB",    "JTU"
    ),
    result_status_identifier = c(
      "Rejected"
    ),
    activity_type_code = c(
      "Quality Control Sample-Field Blank",
      "Quality Control Field Replicate Msr/Obs",
      "Quality Control Sample-Field Replicate"
    ),
    activity_relative_depth_name = c(
      "Near Bottom",
      "Bottom"
    ),
    max_activity_depth_m = 1
  )
)

# wq_param = parameter name used in ETT
# characteristic_name = name used in WQP (must match allowed values in WQP **exactly**)
# note: multiple rows have same wq_param (e.g. TP = "Phosphorus" and "Total Phosphorus, mixed forms")
wq_params <- read_csv("data/wq-params.csv", col_types = cols(.default = col_character()))

# conversion table for converting alternative units in WQP to standard ETT units
unit_conversion <- read_csv("data/unit-conversion.csv", col_types = cols(
  .default = col_character(), 
  convert_base = col_double(), 
  convert_slope = col_double())
)


# EDA units ---------------------------------------------------------------

# this repo contains the most recent version of the EDA_UNITS polygons
# if a newer version is available, change the path to the shapefile below
# the EDA_UNITS shapefile should have an attribute called 'NAME' with the unit name
# if this attribute has a different name (e.g., 'EDA_NAME'), then rename the column to 'NAME' after it is loaded
eda_units <- st_read("data/EDA_UNITS_simplified_Final_MarbleheadNeckCorrection/EDA_UNITS_simplified_Final.shp") %>%
  st_transform(crs = "EPSG:2249")

eda_units_bbox <- eda_units %>%
  st_transform(crs = 4326) %>%
  st_bbox()

eda_units %>% 
  ggplot() +
  geom_sf(aes(fill = Estuarine)) +
  scale_fill_brewer(type = "qual", palette = 6)


# WQP: stations ------------------------------------------------------------

# fetch stations within EDA units bbox having samples for given characteristic_names
# if this function returns "Request failed [400]" then there is likely an invalid characteristic_name
# try running it with each individual value of the wq_params$characteristic_name vector
#  e.g.  dataRetrieval::whatWQPdata(..., characteristicName = wq_params$characteristic_name[1], ...)
# to identify which characteristic_name is invalid
wqp_stations_raw <- dataRetrieval::whatWQPdata(
  bBox = eda_units_bbox,
  sampleMedia = "Water",
  characteristicName = wq_params$characteristic_name,
  startDate = start_date
)

# summary tables
tabyl(wqp_stations_raw, MonitoringLocationTypeName)
tabyl(wqp_stations_raw, OrganizationFormalName)

# convert to sf object, reproject, add EDA unit name, and filter by monitoring_location_type_name
wqp_stations <- wqp_stations_raw %>% 
  st_as_sf(coords = c("lon", "lat"), crs = 4326) %>%
  st_transform(crs = "EPSG:2249") %>% # project to NAD83 / Massachusetts Mainland (ftUS)
  clean_names() %>% # cleans up column names (e.g. MonitoringLocationTypeName -> monitoring_location_type_name)
  st_intersection(
    select(eda_units, eda_unit_name = NAME)
  ) %>%
  filter(!monitoring_location_type_name %in% wqp_filters$stations$monitoring_location_type_name)

# station maps
wqp_stations %>% 
  ggplot() +
  geom_sf(aes(color = fct_lump_n(organization_formal_name, n = 8))) +
  geom_sf(data = eda_units, fill = NA, alpha = 0.5) +
  scale_color_brewer("organization", palette = "Set1") +
  ggtitle("WQP Stations by Organization")

wqp_stations %>% 
  ggplot() +
  geom_sf(aes(color = fct_lump_n(monitoring_location_type_name, n = 8))) +
  geom_sf(data = eda_units, fill = NA, alpha = 0.5) +
  scale_color_brewer("location type", palette = "Set1") +
  ggtitle("WQP Stations by Location Type")

# save stations to shapefile: out/wqp-stations/wqp-stations.shp
# useful for reviewing all available WQP stations in GIS
# this shapefile is NOT used in the ETT website itself
dir.create("out/wqp-stations", recursive = TRUE, showWarnings = FALSE)
wqp_stations %>% 
  select(
    # shorten names (must be 8 characters or less for shapefile)
    provider = provider_name,
    org_id = organization_identifier,
    stn_id = monitoring_location_identifier,
    name = monitoring_location_name,
    type = monitoring_location_type_name,
    url = site_url,
    eda_name = eda_unit_name
  ) %>% 
  st_write("out/wqp-stations/wqp-stations.shp", append = FALSE)


# WQP: fetch samples --------------------------------------------------------

# warning: takes a couple minutes to run this
# if you ran this previously, and want to use the backed up dataset
# skip this step and load wqp_samples_raw from the rds file below
wqp_samples_raw <- wqp_fetch_samples(
  wqp_stations$monitoring_location_identifier, 
  batch_size = 50, 
  characteristicName = wq_params$characteristic_name, 
  startDate = start_date,
  dataProfile = "resultPhysChem" # needed to get ActivityRelativeDepthName
)
write_rds(wqp_samples_raw, "wqp_samples_raw.rds") # save raw sample data to RDS file for backup

# load the sample data from the backup RDS file to avoid re-fetching
# useful if you need to re-run this script or access the sample data, but
# do not need to update the wqp samples
wqp_samples_raw <- read_rds("wqp_samples_raw.rds")


# WQP: clean samples ------------------------------------------------------

# clean raw data by:
#   - cleaning names
#   - adding wq_param (ETT version of characteristic_name)
#   - parsing columns (dates, numeric, etc)
#   - replace non-detect with detection_quantitation_limit_measure_measure_value
#   - convert activity depths to meters
#   - add monitoring_location_type_name for sampling stations
wqp_samples_clean <- wqp_samples_raw %>% 
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
    across(ends_with("_value"), ~ if_else(.x == "FALSE", NA_character_, .x)), # some value columns contain "FALSE", convert to NA before parsing
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
    ),
    wqp_value = coalesce(result_measure_value, detection_quantitation_limit_measure_measure_value),
    wqp_units = case_when(
      !is.na(result_measure_value) ~ as.character(result_measure_measure_unit_code),
      !is.na(wqp_value) ~ as.character(detection_quantitation_limit_measure_measure_unit_code),
      TRUE ~ NA_character_
    )
  ) %>% 
  left_join(
    wqp_stations %>% 
      st_drop_geometry() %>% 
      select(monitoring_location_identifier, monitoring_location_type_name),
    by = "monitoring_location_identifier"
  ) 


# WQP: filter samples -----------------------------------------------------

# filter WQP samples by
#   - missing values
#   - missing/excluded units
#   - excluded result_sample_fraction
#   - excluded result_status_identifier
#   - excluded activity_type_code
#   - excluded activity_relative_depth_name
#   - max activity_depth
# samples that will be filtered will have the filter type in a column named wqp_filters
# wqp_filters will be NA for samples that will not be filtered, and exported to ETT
wqp_samples_filters <- wqp_samples_clean %>%
  left_join(
    wqp_filters$samples$result_sample_fraction_text %>% 
      mutate(exclude_result_sample_fraction_text = TRUE),
    by = c("wq_param", "result_sample_fraction_text")
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
  distinct() # remove duplicates

# extract samples that will be filtered out for review
wqp_samples_excluded <- filter(wqp_samples_filters, !is.na(wqp_filters))

# summary of the number of excluded samples by wq_param
tabyl(wqp_samples_excluded, wqp_filters, wq_param)

# exclude filtered samples and convert units using unit_conversion table
wqp_samples <- wqp_samples_filters %>% 
  filter(is.na(wqp_filters)) %>% 
  select(-wqp_filters) %>% 
  left_join(
    unit_conversion,
    by = c("wq_param", "wqp_units")
  ) %>%
  mutate(
    ett_value = convert_slope * (wqp_value + convert_base)
  )

# verify that no samples have missing value 
stopifnot(all(!is.na(wqp_samples$ett_value)))

# if previous line throws an error then there are samples with missing values
# in that case, review the result of the following command to determine the reason
# most likely, there are one or more types of wqp_units that are not included in the
# unit_conversion table (and thus convert_slope and convert_base will be NA).
# to fix this, add the new wqp_units value to data/unit-conversion.csv and
# re-run the relevant portions of this script above to reload the unit_conversion
# table and then convert the WQP values to ETT values (i.e., create wqp_samples from wqp_samples_filters)
wqp_samples %>% 
  filter(is.na(ett_value)) %>%
  select(monitoring_location_identifier, activity_start_date, wq_param, wqp_units, convert_slope, convert_base, wqp_value, ett_value)
# this command should return zero rows if there are no missing ETT values 

# summary of wq parameter distributions by organization based on standardized units
# useful to identify erroneous values
wqp_samples %>% 
  ggplot(aes(organization_identifier, ett_value)) +
  geom_boxplot() +
  coord_flip() +
  facet_wrap(vars(wq_param, ett_units), scales = "free_x")


# ETT: compute TN samples -----------------------------------------------------

# extract NO23N and TKN samples
# excludes duplicate samples having the same station, activity ID, and date/time
# since we cannot determine which TKN and NO23N values to add
samples_tkn_no23n <- wqp_samples %>% 
  filter(wq_param %in% c("NO23N", "TKN")) %>% 
  add_count(monitoring_location_identifier, activity_identifier, activity_start_date, activity_start_time_time, wq_param) %>% 
  filter(n == 1) %>%
  select(-n)

# compute TN = TKN + NO23N
samples_tn_computed <- samples_tkn_no23n %>% 
  select(monitoring_location_identifier, activity_identifier, activity_start_date, activity_start_time_time, wq_param, ett_value, ett_units) %>% 
  pivot_wider(names_from = "wq_param", values_from = "ett_value") %>% 
  filter(!is.na(NO23N), !is.na(TKN)) %>% 
  mutate(ett_value = NO23N + TKN, wq_param = "TN") %>% 
  select(-NO23N, -TKN)

# ETT: generate samples ------------------------------------------------------------

# generate ETT samples dataset
#   - exclude NO23N and TKN from wqp_samples
#   - append computed TN samples
#   - generate sample datetime in US/Eastern and adust for sample timezone
#   - rename final columns: {station_id, param, units, datetime, value}

ett_samples <- wqp_samples %>% 
  filter(!wq_param %in% c("NO23N", "TKN")) %>% 
  bind_rows(samples_tn_computed) %>% 
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

# ETT: export stations --------------------------------------------------

# create ETT output folder
dir.create("out/ett", showWarnings = FALSE, recursive = TRUE)

# create summary table of sample counts by parameter and station_id
ett_stations_counts <- ett_samples %>% 
  count(station_id, param) %>% 
  pivot_wider(names_from = "param", values_from = "n", values_fill = 0) %>% 
  nest_by(station_id, .key = "wq_counts")

# export stations to GeoJSON: out/ett/stations.json
#   - rename columns used in web app
#   - reproject to WGS84 (EPSG:4326)
#   - join sample counts
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
      mutate(wq_counts = as.character(jsonlite::toJSON(as.list(wq_counts), auto_unbox = TRUE))),
    by = "station_id"
  ) %>%
  st_transform(crs = "EPSG:4326") %>% 
  st_write("out/ett/stations.json", driver = "GeoJSON", layer_options = c("ID_FIELD=station_id"), delete_dsn = TRUE)


# ETT: export samples ---------------------------------------------------

# delete any existing files in out/ett/wq directory (best to start fresh with each export)
if (dir.exists("out/ett/wq")) {
  logger::log_warn("deleting existing data files (out/ett/wq/*.json)")
  unlink("out/ett/wq", recursive = TRUE)
}

# create directory: out/ett/wq
dir.create("out/ett/wq", recursive = TRUE, showWarnings = FALSE)

# convert rows to nested format for JSON export
ett_samples_json <- ett_samples %>% 
  mutate(datetime = format_ISO8601(datetime, usetz = TRUE)) %>% 
  nest_by(station_id, param, units, .key = "values") %>% 
  nest_by(station_id)

# export JSON file for each station_id: out/ett/wq/{ett_samples_json$station_id[[i]]}.json
for (i in 1:nrow(ett_samples_json)) {
  filename <- glue::glue("out/ett/wq/{ett_samples_json$station_id[[i]]}.json")
  logger::log_info("saving: {filename}")
  jsonlite::write_json(ett_samples_json$data[[i]], path = filename, pretty = FALSE)
}

# FINAL STEP: copy all files and folders from `out/ett/` to the `data/` directory on the ETT web server
