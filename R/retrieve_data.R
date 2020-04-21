#' Generic get and parse api request
#'
#' @param url
#' @param api_user
#' @param api_token
#'
#' @return
#' @export
#'
#' @examples
get_api <- function(url, api_user, api_token, simple = TRUE, query = NULL) {

  if (!is.null(query)) {
    query_add <- paste(query,sep = "&")
    url <- paste(url,query_add,sep = "?")
  }

  response <- httr::GET(url, httr::authenticate(api_user, api_token))
  output <- httr::content(response)
  if(simple) {
    output <- unlist(output)
  }

  return(output)
}


#' Make a data request to the API Server
#'
#' @param site
#' @param node
#' @param sensor
#' @param method
#' @param stream
#' @param api_user
#' @param api_token
#' @param base_url
#'
#' @return
#' @export
#'
#' @examples
request_data <- function(site, node, sensor, method, stream, api_user, api_token,
                         query = NULL,
                         base_url = 'https://ooinet.oceanobservatories.org/api/m2m/12576/sensor/inv') {
  url <- paste(base_url,site,node,sensor,method,stream, sep = "/")
  get_api(url, api_user, api_token, simple = FALSE, query = query)
}

#' Gets (probable) list of nc files for download
#'
#' @param data_request
#' @param base_url
#'
#' @return
#' @export
#'
#' @examples
get_filenames <- function(data_request, base_url = "https://opendap.oceanobservatories.org/thredds/dodsC/") {
  page_contents <- RCurl::getURL(data_request$outputURL)
  files <- stringr::str_extract_all(page_contents,"(?<=dataset\\=).*?\\.nc(?!ml)")[[1]]
  if(length(files) == 0) {
    return(NULL)
  } else {
    files <- paste0(base_url,files)
    return(files)
  }
}


