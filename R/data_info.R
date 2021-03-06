#' Get list of sites
#'
#' @param api_user
#' @param api_token
#' @param base_url
#'
#' @return
#' @export
#'
#' @examples
get_sites <- function(api_user, api_token, base_url = 'https://ooinet.oceanobservatories.org/api/m2m/12576/sensor/inv') {
  get_api(base_url, api_user, api_token)
}

#' Get list of nodes from a site
#'
#' @param site
#' @param api_user
#' @param api_token
#' @param base_url
#'
#' @return
#' @export
#'
#' @examples
get_nodes <- function(site, api_user, api_token, base_url = 'https://ooinet.oceanobservatories.org/api/m2m/12576/sensor/inv') {
  url <- paste(base_url,site,sep = "/")
  get_api(url, api_user, api_token)
}

#' Get names of sensors at a node
#'
#' @param site
#' @param node
#' @param api_user
#' @param api_token
#' @param base_url
#'
#' @return
#' @export
#'
#' @examples
get_sensors <- function(site, node, api_user, api_token, base_url = 'https://ooinet.oceanobservatories.org/api/m2m/12576/sensor/inv') {
  url <- paste(base_url,site,node,sep = "/")
  get_api(url, api_user, api_token)
}

#' Get methods for a particular sensor
#'
#' @param site
#' @param node
#' @param sensor
#' @param api_user
#' @param api_token
#' @param base_url
#'
#' @return
#' @export
#'
#' @examples
get_methods <- function(site, node, sensor, api_user, api_token, base_url = 'https://ooinet.oceanobservatories.org/api/m2m/12576/sensor/inv') {
  url <- paste(base_url,site,node,sensor,sep = "/")
  get_api(url, api_user, api_token)
}

#' Get streams for a method
#'
#' @param site
#' @param node
#' @param sensor
#' @param method
#' @param api_user
#' @param api_token
#' @param base_url
#'
#' @return
#' @export
#'
#' @examples
get_streams <- function(site, node, sensor, method, api_user, api_token, base_url = 'https://ooinet.oceanobservatories.org/api/m2m/12576/sensor/inv') {
  url <- paste(base_url,site,node,sensor,method, sep = "/")
  get_api(url, api_user, api_token)
}

#' Get all streams from a data frame of sensor info
#'
#' @param site_df
#' @param api_user
#' @param api_token
#'
#' @return
#' @export
#'
#' @examples
get_streams_all <- function(site_df, api_user, api_token) {
  # loop through all the rows of the data frame and request the streams for that row
  for (i in 1:nrow(df)) {
    df2<- filter(df, 1:nrow(df) %in% i)
    streams <- get_streams(df2$site, df2$node, df2$sensor, method, api_user, api_token)
    for (j in 1:length(streams)) {
      dfadd <- mutate(df2, stream = streams[j])
      if (i == 1 & j == 1) {
        dfout <- dfadd
      } else {
        dfout <- bind_rows(dfout,dfadd)
      }
    }
  }
  return(dfout)
}

#' Get sensor metadata
#'
#' @param site
#' @param node
#' @param sensor
#' @param api_user
#' @param api_token
#' @param base_url
#'
#' @return
#' @export
#'
#' @examples
get_sensor_meta <- function(site, node, sensor, api_user, api_token,
                            base_url = 'https://ooinet.oceanobservatories.org/api/m2m/12576/sensor/inv',
                            unpack_request = TRUE) {
  url <- paste(base_url,site,node,sensor,"metadata", sep = "/")
  metadata <- get_api(url, api_user, api_token, simple = FALSE)
  if(unpack_request) {
    unpack_metadata(metadata)
  } else {
    return(metadata)
  }
}

#' Unpack the contents of a api request for metadata
#'
#' @param metadata metadata object returned by get_sensor_meta()
#'
#' @return
#' @export
#'
#' @examples
unpack_metadata <- function(metadata) {
  params <- metadata$parameters
  params <- as.data.frame(t(sapply(params, unlist)))
  return(params)
}

#' Get the location of the sensor
#'
#' @param ...
#'
#' @return
#' @export
#'
#' @examples
get_sensor_location <- function(...) {
  info <- get_sensor_info(...)
  location <- paste(info["tocL1"], info["tocL2"], info["tocL3"],sep = ", ")
  return(location)
}

#' Get a sensor name
#'
#' @param ...
#'
#' @return
#' @export
#'
#' @examples
get_sensor_name <- function(...) {
  info <- get_sensor_info(...)
  name <- unname(info["instrument"])
  return(name)

}

#' Get the information on a sensor
#'
#' @param site.
#' @param sensor
#' @param api_user
#' @param api_token
#' @param base_url
#'
#' @return
#' @export
#'
#' @examples
get_sensor_info <- function(site, node, sensor, api_user, api_token,
                            base_url = "https://ooinet.oceanobservatories.org/api/m2m/12586/vocab/inv") {
  url <- paste(base_url,site,node,sensor, sep = "/")
  # info <- unname(get_api(url, api_user, api_token)["instrument"])
  # return(info)
  get_api(url, api_user, api_token)

}

#' Get depth range of sensor
#'
#' @param ...
#'
#' @return
#' @export
#'
#' @examples
get_sensor_depth <- function(...) {
  info <- get_sensor_info(...)
  depth <- as.numeric(c(info["mindepth"], info["maxdepth"]))
  return(depth)
}


