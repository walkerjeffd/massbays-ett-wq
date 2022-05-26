library(targets)

# packages
options(tidyverse.quiet = TRUE)
tar_option_set(packages = c("tidyverse", "lubridate", "janitor", "glue", "units", "patchwork", "sf", "logger", "skimr", "jsonlite"))

# load packages into interactive session
if (interactive()) {
  sapply(tar_option_get("packages"), require, character.only = TRUE)
}

# load all functions
invisible(sapply(list.files("R", pattern = ".R", full.names = TRUE), source))

list(
  tar_target(start_date, "1980-01-01"),
  tar_target(wqp_filters, {
    list(
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
  }),
  
  tar_target(wq_params_file, "data/wq-params.csv", format = "file"),
  tar_target(wq_params, read_csv(wq_params_file, col_types = cols(.default = col_character()))),
  
  tar_target(unit_conversion_file, "data/unit-conversion.csv", format = "file"),
  tar_target(unit_conversion, read_csv(unit_conversion_file, col_types = cols(.default = col_character(), convert_base = col_double(), convert_slope = col_double()))),

  targets_eda_units,
  targets_wqp_stations,
  targets_wqp_data,
  targets_ett
  # targets_wqp_summary,
)
