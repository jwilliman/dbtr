#' Create dataset from date range
#'
#' @param treatments `data.table` of cleaned Nightscout Care Portal Treatment information. 
#' @param date_var Name of column containing date variable.
#' @param date_range If date minimum and maximum are not provided, will obtain these from the treatments 
#' @param tz Expected timezone of users data. Defaults to system timezone. 
#'
#' @returns A data.table with columns by (day) and unix epoch for start of day.
#' @export
#'
ns_date_range <- function(treatments, date_var = "dttm_local", date_range = NULL, tz = Sys.timezone()) {
  
  if(is.null(date_range)) {
    
    ## First and last day a treatment was recorded
    date_range <- treatments[[date_var]] |> 
      as.Date(tz = tz) |> 
      range()
    
  }
  
  # Dataset with all dates and breakpoints
  dt_dates <- data.table::data.table(
    by = seq.Date(from = date_range[1], to = date_range[2], "day") |> 
      data.table::as.IDate(), key = "by")
  
  dt_dates[, epoch := paste(by, "00:00:00", sep = " ") |> 
             as.POSIXct(tz = tz) |> 
             as.integer()]
  
  return(dt_dates[])
  
}


#' Extract dates and times of profile switches, and scheduled basals, from Nightscout treatment information
#'
#' @param treatments `data.table` of cleaned Nightscout Care Portal Treatment information.
#'
#' @returns A `data.table` with dates and times of profile switches
#' @export
#'
ns_extract_profiles <- function(treatments) {
  
  ## Extract profile switches and scheduled basals from treatment information
  profiles_treat <- treatments[
    eventType == "Profile Switch" & !is.na(profileJson), 
    .(dttm_local, epoch, profile = trimws(profile), profileJson)] |> 
    unique()
  
  profiles_treat[!is.na(profileJson), basal := (lapply(profileJson, \(x) jsonlite::fromJSON(x)$basal))]
  profiles_treat[, profileJson := NULL]
  
  # Profile ID set as profile used on given day rather than name (in case of edit to profile)
  profiles_treat[, profile_id := .I]   
  
  return(profiles_treat[, .(profile_id, profile_name = profile, epoch, dttm_local, basal)])
  
}


#' Create data.frame of scheduled basal insulin rates by day and time 
#'
#' @param treatments `data.table` of cleaned Nightscout Care Portal Treatment information.
#'
#' @returns An R `data.table` with columns by (day), start_epoch, end_epoch, and rate (units per hour).
#' @export
#'
ns_basal_scheduled <- function(treatments) {
  
  profiles_treat <- ns_extract_profiles(treatments)  
  dt_dates       <- ns_date_range(treatments, tz = "Pacific/Auckland")
  
  dt_dates[, start_date := as.POSIXct(epoch, tz = "Pacific/Auckland")]
  
  ## Combine with all dates to fill in days where profiles don't change.
  profile_switches <- data.table::rbindlist(list(
    "day"     = dt_dates[, .(by, epoch)], 
    "profile" = profiles_treat[
      , .(profile_id, by = data.table::as.IDate(dttm_local), epoch)]
  ), fill = TRUE, idcol = "switch_type")
  
  data.table::setorder(profile_switches, epoch)  
  
  ## Need to use numeric column for setnafill - so using profile_id
  data.table::setnafill(profile_switches, "locf", cols = c("profile_id"))
  
  profile_switches[, epoch_end := data.table::shift(epoch, type = "lead", fill = (
    paste(max(by) + 1, "00:00:00", sep = " ") |> as.POSIXct() |> as.numeric()
  ))]
  
  ## Merge back in profile name and basal rate
  profile_days <- dt_dates[, .(by, day_epoch = epoch)] |> 
    merge(profile_switches, by = "by") |> 
    merge(profiles_treat[, .(profile_id, profile_name, basal)], by = "profile_id") |> 
    tidytable::unnest(basal) |> 
    data.table::as.data.table()
  
  ## Each new scheduled rate starts at the scheduled time after the profile was activated 
  profile_days[, start_epoch  := pmax(day_epoch + timeAsSeconds, epoch)]
  ## And finishes at the start of the next scheduled rate
  profile_days[, next_epoch   := data.table::shift(start_epoch, type = "lead")]
  profile_days[, end_epoch    := pmin(next_epoch, epoch_end)]
  
  ## Profies that start and finish at the same time can be dropped
  profile_days[, drop        := start_epoch >= end_epoch]
  
  
  dat_basal_sched <- profile_days[drop == FALSE, .(
    by, 
    # start_min = (start_epoch - day_epoch) / 60, 
    # end_min   = (end_epoch   - day_epoch) / 60,
    rate = value, start_epoch, end_epoch)]
  
  return(dat_basal_sched[, .(by, start_epoch, end_epoch, rate)])
  
}


#' Create data.frame of temporary basal insulin rates by day and time
#'
#' @param treatments `data.table` of cleaned Nightscout Care Portal Treatment information.
#'
#' @returns An R `data.table` with columns by (day), start_epoch, end_epoch, and rate (units per hour).
#' @export
#'
ns_basal_temporary <- function(treatments) {
  
  dat_basal_temps <- treatments[
    !is.na(durationInMilliseconds) & (!is.na(rate)), 
    .(by = idate_local, start_epoch = epoch, 
      seconds = as.integer(durationInMilliseconds/1000), rate)] |> 
    unique()
  
  dat_basal_temps[, next_epoch := data.table::shift(start_epoch, type = "lead", fill = max(treatments$epoch))]
  dat_basal_temps[, end_epoch  := pmin(next_epoch, start_epoch + seconds)]
  
  return(dat_basal_temps[, .(by, start_epoch, end_epoch, seconds, rate)])
  
}


#' Create data.frame of combined scheduled and temporary basal insulin units delivered by day.
#'
#' @param treatments `data.table` of cleaned Nightscout Care Portal Treatment information.
#'
#' @returns An R `data.table` with columns by (day), start_epoch, end_epoch, and units of insulin scheduled and delivered.
#' @export
#'
ns_basal_total <- function(treatments) {
  
  # Create datasets of scheduled and temporary basals
  dat_basal_sched <- ns_basal_scheduled(treatments)
  dat_basal_temps <- ns_basal_temporary(treatments)
  
  # Create a dataset with the timing of all the rate changes
  vct_basal_times <- sort(unique(c(
    dat_basal_sched$start_epoch, dat_basal_sched$end_epoch,
    dat_basal_temps$start_epoch, dat_basal_temps$end_epoch
  )))
  
  dat_basal_times <- data.table::data.table(
    start_epoch = head(vct_basal_times, -1),
    end_epoch   = tail(vct_basal_times, -1)
  )
  
  data.table::setkey(dat_basal_times, start_epoch, end_epoch)
  
  # Add the scheduled basals to the timings dataset
  data.table::setkey(dat_basal_sched, start_epoch, end_epoch)
  
  dat_basal_times_sched <- data.table::foverlaps(
    dat_basal_times,
    dat_basal_sched,
    type = "within",
    nomatch = 0L
  )[, .(
    by, 
    start_epoch    = i.start_epoch,
    end_epoch      = i.end_epoch,
    rate_scheduled = rate
  )] 
  
  # Add on temporary basals to the timings and scheduled basals
  data.table::setkey(dat_basal_temps, start_epoch, end_epoch)
  
  dat_basal_full <- data.table::foverlaps(
    dat_basal_times_sched,
    dat_basal_temps,
    type = "within",
    nomatch = NA
  )[, .(
    by = i.by,
    start_epoch = i.start_epoch,
    end_epoch   = i.end_epoch,
    rate_scheduled,
    rate_temporary = rate
  )]
  
  # Finally, add extra variables and check timings
  dat_basal_full[, duration_secs  := end_epoch - start_epoch]  
  
  dat_basal_full[, rate_delivered := data.table::fcoalesce(rate_temporary, rate_scheduled)]
  dat_basal_full[!is.na(rate_temporary), rate_difference := rate_temporary - rate_scheduled]
  
  dat_basal_full[, units_scheduled := rate_scheduled * duration_secs / (60*60)]
  dat_basal_full[, units_temporary := rate_temporary * duration_secs / (60*60)]
  dat_basal_full[, units_basal := data.table::fcoalesce(units_temporary, units_scheduled)]    
  
  dat_basal_full[rate_difference > 0, units_temp_positive := rate_difference * duration_secs / (60 * 60)]
  dat_basal_full[rate_difference < 0, units_temp_negative := rate_difference * duration_secs / (60 * 60)]
  
  dat_basal_full[, .(
    by, start_epoch, end_epoch, duration_secs, 
    units_scheduled, units_temp_positive, units_temp_negative, units_basal)]
  
  
}


#' Extract insulin bolus information from Nightscout treatment data
#'
#' @param treatments `data.table` of cleaned Nightscout Care Portal Treatment information.
#'
#' @returns An R `data.table` with columns by (day), time, bolus type (SMB or manual) and units of insulin delivered.
#' @export
#'
ns_bolus_total <- function(treatments) {
  
  unknown_as <- match.arg(unknown_as)
  
  dat_bolus <- treatments[!is.na(insulin) & insulin > 0]
  
  dat_bolus[
    , bolusType := data.table::fifelse(
      isSMB %in% TRUE, "SMB", "Manual"
    )]
  
  return(dat_bolus[, .(by, epoch, bolusType, insulin)])
  
}


#' Create data.frame of basal and bolus insulin scheduled and delivered by day
#'
#' @param treatments `data.table` of cleaned Nightscout Care Portal Treatment information.
#'
#' @returns An R `data.table` with columns by (day), basal and bolus insulin delivered to match Nightscout dashboard. Calculations aren't exactly the same, not sure why?
#' @export
#'
ns_insulin <- function(treatments) {
  
  # Calculate basal and summarise by day
  dat_basal <- ns_basal_total(treatments)
  dat_basal_daily <- dat_basal[
    , lapply(.SD, sum, na.rm = TRUE)
    , .SDcols = c("duration_secs", "units_scheduled", "units_temp_positive", 
                  "units_temp_negative", "units_basal")
    , by = by]
  
  
  # Calculate bolus and summarise by day
  dat_bolus <- ns_bolus(treatments)
  dat_bolus_daily <- dat_bolus[
    , .(n = .N, units = sum(insulin)), by = .(by, bolusType)] |> 
    data.table::dcast(by ~ bolusType, value.var = c("n", "units"), fill = 0)
  dat_bolus_daily[, units_bolus := rowSums(.SD), .SDcols = grep("units", names(dat_bolus_daily), value = TRUE)]
  
  dat_insulin_daily <- merge(
    dat_basal_daily,
    dat_bolus_daily,
    by = "by"
  )
  
  dat_insulin_daily[, units_total := units_basal + units_bolus]
  
  return(dat_insulin_daily[])
  
}
