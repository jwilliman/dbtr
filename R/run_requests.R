#' @title Wrapper to create and run Nightscout API requests and return data to global environment   
#'
#' @param type Type of data file to output. 'json' as extracted from Nightscout, or 'data' to convert to r `data.table` using `data.table::rbindlist`.
#' @param ... Additional arguments to pass to `make_requests`. Including base `url` and `api_secret`.
#'
#' @return
#' @export
#'
#' @examples
run_requests <- function(type = "json", ...) {
  
  reqs <- make_requests(...)
  
  dats <- lapply(reqs, httr2::req_performa)
  
  if(type == "data.table")
    dats <- lapply(dats, data.table::rbindlist, fill = TRUE)
  
  return(dats)
  
}


#' @title Create and run Nightscout API requests and save returned data to a specified external folder
#'
#' @param path A folder location where retrieved data is to be saved. 
#' @param suffix By default files are saved according to their schema (e.g. entries.Rds), a suffix may be applied to the filename.
#' @param type Type of data file to output. 'json' as extracted from Nightscout, or 'data' to convert to r `data.table` using `data.table::rbindlist`.
#' @param ... Additional arguments to pass to `make_requests`. Including base `url` and `api_secret`.
#'
#' @return
#' @export
#'
#' @examples
save_requests <- function(path = "", suffix = "", type = "json", ...) {
  
  reqs <- make_requests(...)
  
  dats <- lapply(reqs, httr2::req_performa)
  
  if(type == "data.table")
    dats <- lapply(dats, data.table::rbindlist, fill = TRUE)
  
  for(schema in names(dats))
    saveRDS(dats[[schema]], file = file.path(path, paste0(schema, suffix, ".Rds")))
  
}
