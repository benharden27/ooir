library(ooir)
library(tidyverse)

api_user <- 'OOIAPI-GBZLL30PK7OU67'
api_token <- 'TEMP-TOKEN-NVSQ4OQM8VH7H7'

# full list of sites
sites <- get_sites(api_user, api_token)

# loop through these sites
# find the nodes, loop through these
# Find the sensors, loop through these and make a data table containing all that info
# Plus plain language interpretations of coded vars
for (i in 1:length(sites)) {
  print(i)
  print(sites[i])
  nodes <- get_nodes(sites[i], api_user, api_token)
  for (j in 1:length(nodes)) {
    sensors <- get_sensors(sites[i],nodes[j],api_user, api_token)
    for (k in 1:length(sensors)) {
      name <- get_sensor_name(sites[i],nodes[j],sensors[k],api_user, api_token)
      if(is.null(name)) {
        name <- NA
      }
      location <- get_sensor_location(sites[i],nodes[j],sensors[k],api_user, api_token)
      if(length(location)==0) {
        location <- NA
      }
      depth <- get_sensor_depth(sites[i],nodes[j],sensors[k],api_user, api_token)
      if(length(depth)==0) {
        depth <- NA
      }
      df <- tibble(site = sites[i],
                   node = nodes[j],
                   sensor = sensors[k],
                   name = name,
                   location = location,
                   min_depth = depth[1],
                   max_depth = depth[2])

      if(i == 1 & j ==1 & k == 1) {
        sensors_all <- df
      } else {
        sensors_all <- bind_rows(sensors_all, df)
      }
    }
  }
}

usethis::use_data(sensors_all,overwrite = TRUE)
