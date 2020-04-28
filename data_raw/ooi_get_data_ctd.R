library(ooir)
library(tidync)
library(tidyverse)
library(ncdf4)

# Set up parameters -------------------------------------------------------

# which field do you want to download?
field <- "ctd" # "do"
csv_write <- TRUE # write the csv?
csv_file <- "~/Desktop/text.csv" # where to write the csv file?
stephr = 1 # time step in hours for extracting data

# insert your api login details (use yours not mine)
api_user <- 'OOIAPI-GBZLL30PK7OU67'
api_token <- 'TEMP-TOKEN-NVSQ4OQM8VH7H7'

# list sites and subset to just Global Irminger (GI) and the surface and two flanking moorings
sites <- unique(stringr::str_subset(sensors_all$site,"GI02HYPM"))
df_sites <- filter(sensors_all, site %in% sites)

sensor_type <- "CTD"
depth <- c(200,4000)
param_names <- c("time",
                 "ctdpf_ckl_seawater_temperature",
                 "practical_salinity",
                 "ctdpf_ckl_seawater_pressure")


df <- filter(df_sites,str_detect(name,sensor_type) & min_depth > depth[1] & max_depth < depth[2])
params <- get_sensor_meta(df$site[1], df$node[1], df$sensor[1], api_user, api_token)
inst_id <- unique(as.numeric(stringr::str_extract(filter(params, particleKey %in% param_names)$pdId, "[0-9].*")))

# CHECK IF: all inst_ids exist in all files. Can we need to make inst_ids for each line?

# # NOTE!!! field dependent change --------------------------------------------------
#
# # things change here depending on which field you want to extract
# if (field == "chla") {
#   # filter df_sites for just the fluorometer sensors - name found in df_sites$name
#   df <- filter(df_sites,str_detect(name,"^3-Wavelength Fluorometer$"))
#   # get all the parameters exported in a sensor stream by just looking at the first one
#   params <- get_sensor_meta(df$site[1], df$node[1], df$sensor[1], api_user, api_token)
#   # parameters you want to download - see params$pdId column in params (7 = time, 22 = chla)
#   inst_id = c(7,22)
# }
#
# if (field == "do") {
#   df <- filter(df_sites,str_detect(name,"^Dissolved Oxygen$"))e
#   params <- get_sensor_meta(df$site[1], df$node[1], df$sensor[1], api_user, api_token)
#   inst_id = c(7,14)
# }
#
# if (field == "nit") {
#   df <- filter(df_sites,str_detect(name,"^Nitrate$"))
#   params <- get_sensor_meta(df$site[1], df$node[1], df$sensor[1], api_user, api_token)
#   inst_id = c(7,315)
# }
#
# if (field == "ctd") {
#   df <- filter(df_sites,str_detect(name,"^CTD$|^CTD \\(30 meters\\)$"))
#   params <- get_sensor_meta(df$site[2], df$node[2], df$sensor[2], api_user, api_token)
#   inst_id = c(7,13,193)
#   inst_id = c(7,13,2927)
# }
# continueing on ----------------------------------------------------------

# set the method (recovered_host? telemetered?)
method <- "recovered_wfp"
df <- mutate(df, method = method)

# Get the streams at each site, node, and sensor to make the request to the API
# Loop through each row of df to find the streams for each and put these into a new data frame
dfout <- get_streams_all(df,api_user, api_token)
dfout <- dfout[1,]

# Set up a parameter enquirely (this will only request certain parameters)
query <- paste0("parameters=",paste0(inst_id,collapse = ","))
query <- paste(query,
               'beginDT=2018-06-01T00:00:00.000Z',
               'endDT=2019-09-01T00:00:00.000Z',
               sep = "&")

# loop through the rows of dfout and make a request for each site, node, etc combo
# save the request details to list (data_request)
data_request <- vector(mode = "list", length = nrow(dfout))
for (i in 1:nrow(dfout)) {
  df2 <- filter(dfout, 1:nrow(dfout) %in% i)
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

  # remove some of the filenames that have been causing some trouble (not sure why right now)
  if (field != "ctd") {
    filenames <- filenames[!str_detect(filenames, "METBK|CTD")]
  }

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
    # step <- ceiling(stephr * 3600 / dt)
    step <- 100

    # create file request that only selects param_names from 0->n in steps of "step"
    param_add <- paste(paste0(param_names,"[0:", step, ":", n, "]"), collapse = ",")
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

if(csv_write) {
  write_csv(df, csv_file)
}

filter(df, 1:nrow(df) %in% seq(1,nrow(df),100)) %>%
  ggplot(aes(time, ctdpf_ckl_seawater_pressure, color = ctdpf_ckl_seawater_temperature)) +
  geom_point()

filter(df, 1:nrow(df) %in% seq(1,nrow(df),100)) %>%
  ggplot(aes(time, ctdpf_ckl_seawater_pressure)) +
  geom_point()

ii <- c(1, which(diff(df$time) > 100))
ctd <- vector(list,length(ii)-1)
for (i in 1:(length(ii)-1)) {
  vec <- ii[i]:ii[i+1]
  ctd_int <- oce::as.ctd(df$practical_salinity[vec],
                     df$ctdpf_ckl_seawater_temperature[vec],
                     df$ctdpf_ckl_seawater_pressure[vec],
                     time = df$time[vec],
                     station = i,
                     longitude = i,
                     latitude = 0,
                     startTime = df$time[ii[i]])
  ctd[[i]] <- oce::ctdDecimate(ctd_int, p = 5)
  ctd_add <- tibble::tibble(dep = oce::swDepth(ctd[[i]]@data$pressure),
                            pres = ctd[[i]]@data$pressure,
                            temp = ctd[[i]]@data$temperature,
                            theta = oce::swTheta(ctd[[i]]@data$salinity,
                                                 ctd[[i]]@data$temperature,
                                                 ctd[[i]]@data$pressure),
                            sigtheta = oce::swSigma0(ctd[[i]]@data$salinity,
                                                     ctd[[i]]@data$temperature,
                                                     ctd[[i]]@data$pressure),
                            sal = ctd[[i]]@data$salinity,
                            profile = i,
                            time = df$time[ii[i]])
  if (i == 1){
    ctdtib <- ctd_add
  } else {
    ctdtib <- dplyr::bind_rows(ctdtib,ctd_add)
  }
}

unprof <- unique(ctdtib$profile)
filter(ctdtib, profile %in% seq(1,max(unprof),3)) %>%
ggplot(aes(time, dep, color = sigtheta)) + geom_point()

write_csv(ctdtib,"~/Desktop/test.csv")
