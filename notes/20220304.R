source("_targets.R")

# how to get activityRelativeDepthName?

tar_load(c(ett_stations, ett_wq_params, ett_start_date))

x <- dataRetrieval::readWQPdata(
  siteid = "11NPSWRD_WQX-CACO_BEECH_FOR",
  sampleMedia = "Water",
  tz = "America/New_York",
  characteristicName = ett_wq_params$characteristic_name,
  startDate = ett_start_date
)

attributes(x)
tabyl(x, CharacteristicName)

