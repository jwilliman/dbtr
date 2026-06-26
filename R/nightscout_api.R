#' Generate a single Nightscout API request, and perform request to get response
#'
#' @param hostname Base URL for request.
#' @param pathname Default api/v1
#' @param filename Data collection to retrieve. CGM data = `entries`, Care Portal Treatments = `treatments`, Treatment profiles = `profile`, Server status and settings = `status`
#' @param filetype Data to be return as `json` or `csv`
#' @param count Number of values to return. Default is 10, set to `Inf` to ensure all are returned when date range is specifed.
#' @param start_date First day of date range to return. As date field or character in form YYYY-MM-DD. **Note** The extract will always start at midnight UTC / GMT as that is how the data is saved within Nightscout.
#' @param end_date Last day of date range to return. As date field or character in form YYYY-MM-DD. **Note** The extract will always end at 23:59:59 UTC / GMT as that is how the data is saved within Nightscout.
#' @param api_secret API secret / user password.
#' @param api_hash Hashed version of API secret / user password. If supplied api_secret is ignored.
#' @param path Passed to httr2::req_perform. Path to save body of the response (data retrieved). This is useful for large responses since it avoids storing the response in memory. Optional, if not supplied data will be directly returned into R. If path ends with .Rds will save as an R data file with full httr2 repsonse information.
#' @param perform Whether to perform to request `TRUE` or just return the request itself `FALSE`
#'
#' @returns
#' Response from httr2::req_perform
#' 
#' If the HTTP request succeeds, and the status code is ok (e.g. 200), an HTTP response.
#'
#' If the HTTP request succeeds, but the status code is an error (e.g a 404), an error with class c("httr2_http_404", "httr2_http"). By default, all 400 and 500 status codes will be treated as an error, but you can customise this with req_error().

#' If the HTTP request fails (e.g. the connection is dropped or the server doesn't exist), an error with class "httr2_failure".
#' @import httr2
#' @importFrom openssl sha1
#' @export
#'
#' @examples
#' \dontrun{
#' ns_api_fetch(url = "http://localhost.1337", filename = "entries", count = 100)
#' }
#' 
#' 
ns_api_fetch <- function(
    hostname   = NULL,
    pathname   = "api/v1",
    filename   = c("entries","treatments", "devicestatus", "profile", "status"),
    filetype   = c("json", "csv"),
    count      = 10,
    start_date = NULL,
    end_date   = NULL,
    api_secret = NULL,  
    api_hash   = NULL,
    path       = NULL,
    perform    = TRUE,
    ...) {
  
  # Ensure base_url is correct
  if(grepl("^http", hostname))
    origin <- hostname
  else
    origin <- paste0("https://", hostname)
  
  filename <- match.arg(filename, choices = c("entries","treatments", "profile", "status"))
  
  if(filename == "entries") {
    
    # Building request components for CGM / entries data
    pathname    = file.path(pathname, "entries")
    filenametype = paste("sgv", match.arg(filetype, choices = c("json", "csv")), sep = ".")
    
    queries <- list(
      `find[dateString][$gte]` = start_date,
      `find[dateString][$lte]` = end_date,
      ...
    )
    
    
  } else if(filename == "treatments") {
    
    # Building request components for treatments data
    filenametype = paste(filename, match.arg(filetype, choices = c("json", "csv")), sep = ".")
    
    # Build list of queries
    queries <- list(
      `find[created_at][$gte]` = start_date,
      `find[created_at][$lte]` = end_date,
      ...
    )} else {
      if(!is.null(start_date) | !is.null(count))
        warning("Queries and 'count' not valid for current schema, and will be ignored")
      filenametype = paste(filename, match.arg(filetype, choices = c("json", "csv")), sep = ".")
      queries = list()
    }
  
  
  # Put it all together as an API request
  req <- httr2::request(origin) |>
    httr2::req_url_path_append(pathname) |>
    httr2::req_url_path_append(filenametype) |>
    ## Nightscout is a MongoDB. httr2 Encodes square brackets but doesn't seem to be a problem?
    httr2::req_url_query(!!!queries) |>  
    httr2::req_url_query(count = count)
  
  # Add API secret
  if(!is.null(api_hash)) {
    
    req <- httr2::req_headers(
      req,
      "api-secret" = api_hash,
      "Accept"     = "application/json")
    
  } else if (!is.null(api_secret)) {
    
    req <- httr2::req_headers(
      req,
      "api-secret" = openssl::sha1(api_secret),
      "Accept"     = "application/json")
    
  }
  
  if(perform) {
    
    # Perform API request    
    if(is.null(path)) {
      httr2::req_perform(req)
    } else if(tools::file_ext(path) == "Rds") {
      saveRDS(httr2::req_perform(req), file = path)  
    } else {
      httr2::req_perform(req, path = path)
    }
    
  } else
    return(req)
}



#' Read in json or Rds data extract from Nightscout previously and saved on a disk
#'
#' @param file A json or Rds file previous saved by function `ns_retrieve`, or other httr2::req_perform operation. 
#'
#' @returns Data table
#' @export
#'

ns_read_file <- function(file) {
  
  ## Read in treatments if saved
  if(tools::file_ext(file) == "json") {
    
    treatments <- jsonlite::fromJSON(txt = file) |> 
      data.table::as.data.table()  
    
  } else if(tools::file_ext(file) == "Rds") {
    
    rds <- readRDS(file = file)
    
    rds_class <- class(rds)
    
    if(rds_class == "httr2_response") {
   
      treatments <- rds |> 
        httr2::resp_body_json() |> 
        data.table::rbindlist(fill = TRUE)       
      
    } else {
      
    treatments <- rds |> 
      jsonlite::fromJSON() |> 
      data.table::rbindlist(fill = TRUE) 
    
    }
  }
  
  return(treatments)
  
}


#' Clean treatment data. Mostly creating local date variables.
#'
#' @param treatments `data.table` of raw Nightscout Care Portal Treatment information
#' @param tz If not provided will take the most common timezone in treatments.
#'
#' @returns The data.table is modified by reference (and returned invisibly). 
#' @export
#'

ns_clean_treatments <- function(treatments, tz = NULL) {
  
  ## Determine timezone if not provided
  if(is.null(tz)) {
    tz = na.omit(treatments$profileJson) |> 
      sapply(\(x) jsonlite::fromJSON(x)$timezone, USE.NAMES = FALSE) |> 
      table() |> 
      sort(decreasing = TRUE) |> 
      names()
    
    if(length(tz) > 1) {
      cat("Multiple timezones detected, using most common")
      tz <- head(tz, 1)
    }
    
    cat("Timezone recorded as", tz)
    
  }
  
  ## Edit treatment dates. Use unix epoch time (integer) to maximise speed
  treatments[, dttm_utc := fasttime::fastPOSIXct(created_at, tz = "UTC")]
  treatments[, epoch    := as.integer(dttm_utc)]
  data.table::setkey(treatments, epoch)
  
  ## Create grouping variable
  treatments[, dttm_local := fasttime::fastPOSIXct(created_at, tz = tz)]
  treatments[, c("idate_local", "itime_local") := data.table::IDateTime(dttm_local)]
  
  ## Combine rate and absolute in case one is missign
  treatments[, rate  := data.table::fcoalesce(rate, absolute)]
  
}


