library(ooir)
library(tidync)
library(tidyverse)
library(ncdf4)

# Set up parameters -------------------------------------------------------

# which field do you want to download?
field <- "ctd" # "do"
csv_write <- TRUE # write the csv?
csv_file <- "~/Desktop/test.csv" # where to write the csv file?
stephr = 1 # time step in hours for extracting data

# insert your api login details (use yours not mine)
api_user <- 'OOIAPI-GBZLL30PK7OU67'
api_token <- 'TEMP-TOKEN-NVSQ4OQM8VH7H7'

# list sites and subset to just Global Irminger (GI) and the surface and two flanking moorings
sites <- unique(stringr::str_subset(sensors_all$site,"GI"))
df_sites <- filter(sensors_all, site %in% sites)

# what kind of data do you want?
sensor_type <- "CTD"
depth <- c(0,40)
param_names <- c("time",
                 "ctdmo_seawater_temperature",
                 "ctdbp_seawater_temperature",
                 "ctdmo_seawater_pressure",
                 "ctdbp_seawater_pressure",
                 "practical_salinity")

# filter for sensor type and depth range
df <- filter(df_sites,str_detect(name,sensor_type) & min_depth > depth[1] & max_depth < depth[2])

# create a data frame of all streams and parameters that match the ones above
for (i in 1:nrow(df)) {
  params <- get_sensor_meta(df$site[i], df$node[i], df$sensor[i], api_user, api_token)
  streams <- unique(params$stream)
  dfadd <- tibble::tibble(stream = streams, inst_id = vector("list", length(streams)), params = inst_id)
  for (j in 1:length(streams)) {
    dfadd$inst_id[[j]] <- unique(as.numeric(stringr::str_extract(filter(params, stream == streams[j] & particleKey %in% param_names)$pdId, "[0-9].*")))
    dfadd$params[[j]] <- as.character(unique(filter(params, stream == streams[j] & particleKey %in% param_names)$particleKey))
  }
  dfadd <- mutate(dfadd, site = df$site[i], node = df$node[i], sensor = df$sensor[i], name = df$name[i],
                  location = df$location[i], min_depth = df$min_depth[i], max_depth = df$max_depth[i])
  if(i == 1) {
    dfstreams <- dfadd
  } else {
    dfstreams <- bind_rows(dfstreams,dfadd)
  }
}

# set the method (recovered_host? telemetered?)
method <- "recovered_inst"
df <- mutate(df, method = method)

# Get the streams at each site, node, and sensor to make the request to the API
# Loop through each row of df to find the streams for each and put these into a new data frame
dfout <- get_streams_all(df,api_user, api_token)

# join with dfstreams to have a complete list of request info
dfout <- left_join(dfout, dfstreams)

# Set up a parameter enquirely (this will only request certain parameters)

# loop through the rows of dfout and make a request for each site, node, etc combo
# save the request details to list (data_request)
data_request <- vector(mode = "list", length = nrow(dfout))
for (i in 1:nrow(dfout)) {
  df2 <- filter(dfout, 1:nrow(dfout) %in% i)
  query <- paste0("parameters=",paste0(df2$inst_id[[1]],collapse = ","))
  data_request[[i]] <- request_data(df2$site,df2$node,df2$sensor,method,df2$stream, api_user, api_token, query = query)
}



#####
# PAUSE - the server needs time to process your request and create your data
# Give it 5 mins.
# Have a cup of tea.
#####



# Retrieve the data -------------------------------------------------------
#
# Loop through list of requests and extract the data for each request
for (i in 1:length(data_request)) {

  # If there's an error with a request, skip it
  if("message" %in% names(data_request[[i]])) {
    message("skipping: ", i)
    next
  }

  # get the filenames of all the files generated during the request
  filenames <- get_filenames(data_request[[i]]) # get all filenames

  # loop through filenames, read the data and combine
  for (file in filenames) {
    # get the number of data points in the file
    # This is a real hack - involves reading all the html from a page and then extacting 1 instance of the number of obs
    file_html <- RCurl::getURL(paste0(file,".html"))
    n <- as.numeric(stringr::str_extract(file_html,"(?<=time\\[obs \\= )[0-9]*"))-1

    # find the length of the timestep and hence the number of data points to skip to get ~1hr resolution
    # # this involves downloading a very small data chunk and searching the global attributes for the time step
    param_add <- "obs[1]"
    file2 <- file2 <- paste(file,param_add,sep="?")
    f <- nc_open(file2)
    timecov <- ncatt_get(f, varid = 0)$time_coverage_resolution
    dt <- as.numeric(stringr::str_extract(timecov,"(?<=[A-Z]).*(?=[A-Z])"))
    step <- ceiling(stephr * 3600 / dt)

    # create file request that only selects param_names from 0->n in steps of "step"
    param_add <- paste(paste0(dfout$params[[i]],"[0:", step, ":", n, "]"), collapse = ",")
    file2 <- paste(file,param_add,sep="?")

    # read data and combine
    f <- tidync(file2) %>%
      hyper_tibble()
    f <- mutate(f,
                site = dfout$site[i],
                node = dfout$node[i],
                sensor = dfout$sensor[i],
                method = method,
                stream = dfout$stream[i],
                file = file)
    if (i == 1 & file == filenames[1]) {
      df <- f
    } else {
      df <- bind_rows(df,f)
    }
  }



}

# convert time column to date time format (from seconds since 1900-01-01)
df <- mutate(df, time = as.POSIXct(time, origin = "1900-01-01"))

# ctd specific, coalesce data
df <- mutate(df, temp = coalesce(ctdmo_seawater_temperature,ctdbp_seawater_temperature),
             pres = coalesce(ctdmo_seawater_pressure,ctdbp_seawater_pressure))

# write to file
if(csv_write) {
  write_csv(df, csv_file)
}


# test plot
ggplot(dftest, aes(time, temp)) +
  geom_line() +
  facet_wrap(~paste(site,node),ncol=1, scales = "free_y")
